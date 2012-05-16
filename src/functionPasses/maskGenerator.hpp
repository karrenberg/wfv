/**
 * @file   maskGenerator.hpp
 * @date   24.07.2009
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2008, 2009, 2010, 2011 Saarland University
 *
 * This pass generates mask information for all edges of the CFG.
 * Currently, the masks are inserted as bitcode operations on integers
 *
 */
#ifndef _MASKGENERATOR_HPP
#define	_MASKGENERATOR_HPP

#ifdef DEBUG_TYPE
#undef DEBUG_TYPE
#endif
#define DEBUG_TYPE "maskgenerator"

#include "llvm/Support/raw_ostream.h"

#include <llvm/Pass.h>
#include <llvm/Analysis/LoopInfo.h>

#include "vectorizationAnalysis.hpp"
#include "utils/analysisResults.hpp"
#include "utils/maskGraph.hpp"
#include "utils/wholeFunctionVectorizationAAW.hpp"

#include "llvm/ADT/Statistic.h" //STATISTIC

#include <sstream> // stringstream
#include <stdexcept>

// forward declaration of initializer
namespace llvm {
	void initializeMaskGeneratorPass(PassRegistry&);
}

using namespace llvm;

STATISTIC(MaskOperationCounter, "Counts number of mask operations that were generated");
STATISTIC(MaskPhiCounter, "Counts number of loop mask phis that were generated");

namespace {

class MaskGenerator : public FunctionPass {
public:
	static char ID; // Pass identification, replacement for typeid
	typedef std::map<BasicBlock*, std::pair<unsigned, Value**> > MaskMapType;

	MaskGenerator(bool* failed_flag=NULL, const bool verbose_flag=false)
			: FunctionPass(ID), failed(failed_flag), mVerbose(verbose_flag)
	{
		initializeMaskGeneratorPass(*PassRegistry::getPassRegistry());
	}

	~MaskGenerator() {}

	virtual void releaseMemory() {
		maskGraph->clear();
		delete maskGraph; // results in a "double free or corruption"
	}

	virtual void getAnalysisUsage(AnalysisUsage &AU) const {
		AU.addRequired<DominatorTree>();
		AU.addRequired<PostDominatorTree>();
		AU.addRequired<LoopInfo>();
		//AU.addRequired<BranchInfoAnalysis>();    // dummy
		//AU.addRequired<LoopLiveValueAnalysis>(); // dummy
		AU.addRequired<VectorizationAnalysis>();

		AU.addPreserved<DominatorTree>();
		AU.addPreserved<PostDominatorTree>();
		AU.addPreserved<LoopInfo>();
		AU.addPreserved<BranchInfoAnalysis>();
		AU.addPreserved<LoopLiveValueAnalysis>();
		AU.addPreserved<VectorizationAnalysis>();
		AU.setPreservesCFG(); // doesn't this include DT/PDT/LI?
	}

	virtual bool runOnFunction(Function& F) {
		MaskOperationCounter = 0;
		MaskPhiCounter = 0;

		if (failed && *failed) return true;

		// initialize constants
		boolOneConst = Constant::getAllOnesValue(Type::getInt1Ty(F.getContext()));
		boolZeroConst = Constant::getNullValue(Type::getInt1Ty(F.getContext()));

		domTree               = &getAnalysis<DominatorTree>();
		postDomTree           = &getAnalysis<PostDominatorTree>();
		loopInfo              = &getAnalysis<LoopInfo>();
		analysisResults       = getAnalysis<VectorizationAnalysis>().getAnalysisResults();

		maskGraph             = new MaskGraph(F, *loopInfo, F.getContext(), mVerbose);

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "generating mask information for blocks of function " << F.getName() << "...\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		try {
			// initialize graph with return node
			MaskGraphNode* returnNode = maskGraph->getSink();
			assert (returnNode);
			std::set<BasicBlock*> visitedBlocks;
			recGenerateMaskInfo(returnNode, visitedBlocks);
		}
		catch (std::logic_error& error) {
			errs() << "\nException occurred during mask generation: "
					<< error.what() << "\n";
			if (failed) *failed = true;
			return true;
		}
		catch (...) {
			errs() << "\nINTERNAL ERROR: Unexpected exception occurred during "
					<< "mask generation!\n";
			if (failed) *failed = true;
			return true;
		}

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "generating mask information for loops of function " << F.getName() << "...\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		// We have to be sure to create loop exit masks for every nested varying
		// loop. Therefore, we iterate over all those loops that are varying
		// and "top-level", meaning they are not nested or nested directly
		// inside a uniform loop.
		// EXAMPLE: Varying loop inside uniform loop inside varying loop.

		// Create loop exit masks for varying "top-level" loops.
		// Here, we can now stop recursion every time we see a uniform sub-loop
		// because possible varying loops nested deeper have been collected
		// before.
		for (AnalysisResults::varyingtoplevelloop_iterator
				L=analysisResults->varyingtoplevelloop_begin(),
				LE=analysisResults->varyingtoplevelloop_end(); L!=LE; ++L)
		{
			SmallVector<BasicBlock*, 4>* exitingBlocks = new SmallVector<BasicBlock*, 4>();
			(*L)->getExitingBlocks(*exitingBlocks);
			generateLoopExitMaskInfo(*L, exitingBlocks);
			delete exitingBlocks;
		}

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "finished generation of mask information for loops!\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		maskGraph->finalize();
		DEBUG_PKT( maskGraph->print(outs()); );

		assert (maskGraph->verify() && "verification of mask graph failed!");
		assert (maskGraph->verifyLoopMasks(*analysisResults) && "verification of loop mask graph failed!");

		//optimizeMasks(); //TODO: implement

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "mask-generation finished.\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "inserting mask operations...\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );
		// up to here, the CFG has not been altered
		// NOTE: vectorizationAnalysis is only updated in mask graph, not used!
		maskGraph->insertMasks(*analysisResults);
		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "insertion of mask operations finished!\n"; );
		DEBUG_PKT( this->print(outs()); );
		DEBUG_PKT( outs() << "\n"; );
		DEBUG_PKT( printMaskGraph(outs()); printLoopExitMasks(outs()); );

		assert(verifyMaskInsertion() && "verification of mask operation insertion failed!");

		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		assert (verify(F) && "verification of mask generation failed!");

		// check mask info
		assert (!maskGraph->hasCompoundMasks() && "mask operation insertion not complete!");

		return 0; // CFG is preserved
	}

	void print(raw_ostream &o, const Module* M=NULL) const {
		o << "  mask operations generated: " << MaskOperationCounter << "\n";
		o << "  mask phis generated: " << MaskPhiCounter << "\n";
	}

	void printMaskGraph(raw_ostream &o) const { maskGraph->print(o); }

	void printLoopExitMasks(raw_ostream &o) const { maskGraph->printLoopExitMap(o); }

	//returns true if all blocks of the function have non-NULL associated entry- and exit-masks
	//returns false otherwise and prints all 'bad' blocks
	// NOTE: We must not use llvm's verification because we generate values that
	//       break the SSA dominance property (masks that rely on CFG linearization).
	bool verify(Function& F) {
		DEBUG_PKT( outs() << "verifying generation of masks for function '" << F.getName() << "'... "; );
		bool verified = true;
		for (Function::iterator BB=F.begin(); BB!=F.end(); ++BB) {
			if (const MaskGraphNode* node = maskGraph->findMaskNode(BB)) {
				verified &= node->verify();
			} else {
				DEBUG_PKT( if (verified) outs() << "\n"; );
				errs() << "ERROR: no masks generated for block '" << BB->getName() << "'!\n";
				verified = false;
			}
		}
		if (verified) { DEBUG_PKT( outs() << "done.\n"; ); }
		else errs() << "verification of mask generation failed!\n";
		return verified;
	}

	MaskGraph* getMaskGraph() { return maskGraph; }

private:
	bool* failed;
	const bool mVerbose;
	MaskGraph* maskGraph;
	AnalysisResults* analysisResults;
	LoopInfo* loopInfo;
	DominatorTree* domTree;
	PostDominatorTree* postDomTree;

	Constant* boolOneConst;
	Constant* boolZeroConst;

	void recGenerateMaskInfo(MaskGraphNode* node,
							 std::set<BasicBlock*>& visitedBlocks)
	{
		assert (node && node->getBlock());
		BasicBlock* block = node->getBlock();

		// If we have seen this node already, ignore it.
		if (visitedBlocks.find(block) != visitedBlocks.end()) return;
		visitedBlocks.insert(block);

		// Generate mask information for all predecessors (bottom-up post reversed order).
		for (pred_iterator PI=pred_begin(block), PE=pred_end(block); PI!=PE; ++PI) {
			BasicBlock* predBB = *PI;
			assert (predBB);
			assert (maskGraph->findMaskNode(predBB));
			recGenerateMaskInfo(maskGraph->findMaskNode(predBB), visitedBlocks);
		}

		DEBUG_PKT( outs() << "\n  generating mask information for block '"
				<< block->getName() << "'... \n"; );

		//
		// Create entry mask.
		//

		MaskOperation* entryMask = NULL;

		const bool isLoopHeader = loopInfo->isLoopHeader(block);
		const Loop* loop = loopInfo->getLoopFor(block);
		assert (!isLoopHeader || loop);

		// NOTE: We have to be sure to test for a loop header first!
		//       Otherwise, we might do bad things, e.g. a FULLY_UNIFORM entry
		//       into a VARYING loop would prevent generation of a loop mask phi.
		if (isLoopHeader) {
			if (analysisResults->isUniform(loop)) {
				// This is the header of a UNIFORM loop, so we have to make sure
				// that the entry mask is the exit mask of the preheader in all
				// iterations (no loop mask phi).
				BasicBlock* preheaderBB = loop->getLoopPreheader();
				assert (maskGraph->findMaskNode(preheaderBB));
				MaskGraphNode* preheaderNode = maskGraph->findMaskNode(preheaderBB);
				assert (preheaderNode->getNumSuccessors() == 1);
				entryMask = new MaskNodeReference(preheaderNode,
					MaskNodeReference::EXITTRUE);
			} else {
				// This is the header of a VARYING loop, so we have to make sure
				// that there is a loop mask phi.
				// Current block is the header of a loop.
				// Build a phi for the loop-condition (incoming value from
				// preheader and latch).
				MaskPhiOperation* phiOp = new MaskPhiOperation();

				for (unsigned i=0, e=node->getNumPredecessors(); i<e; ++i) {
					MaskGraphNode* predNode = node->getPredecessor(i);
					MaskOperation* tmpMask =
							new MaskNodeReference(predNode, node);
					assert (tmpMask);
					phiOp->addIncoming(tmpMask, predNode->getBlock());
				}

				entryMask = phiOp;
			}
		} else if (analysisResults->hasFullyUniformEntry(block)) {
			// Block has fully uniform entry and is no loop header or a loop
			// header of a uniform loop.
			// Block is always executed by ALL instances.
			entryMask = new MaskValue(boolOneConst);
		} else {
			// TODO: update description to implementation!
			// "Normal" block is no loop header and either UNIFORM or VARYING.
			// 3 cases for the entry mask in order of decreasing strength:
			// 1) UNIFORM entry:
			//    a) exit mask of single, UNIFORM predecessor
			//    b) phi with incoming masks from all predecessors if all are UNIFORM
			//    c) entry mask of common dominator if any predecessor's exit in this block's direction is VARYING (block has > 1 incoming edges)
			//    deprecated:
			//    d) exit mask of first predecessor with VARYING conditional exit
			//    e) exit mask of first predecessor with VARYING entry (pred has > 1 incoming edges)
			//    f) entry mask of first predecessor with UNIFORM entry (recursive referencing)
			// 2) VARYING entry, common dominator of paths exists, current block post-dominates the common dominator:
			//    - entry mask of common dominator
			// 3) VARYING entry, no common dominator:
			//    - join over exit masks of all predecessors

			if (node->hasSinglePredecessor()) {

				// case 1a)
				// A block with a single entry simply uses the corresponding
				// exit mask of its predecessor as its entry mask, regardless
				// of UNIFORM or VARYING entry.
				MaskGraphNode* predNode = node->getSinglePredecessor();
				assert (predNode);
				entryMask = new MaskNodeReference(predNode, node);

			} else {

				// multiple predecessors

				if (analysisResults->hasUniformEntry(block)) {

					// UNIFORM entry

					// Blocks that are marked uniform can have uniform
					// incoming edges, but their predecessors can still be
					// VARYING, resulting in linearized control-flow, where
					// a phi would be wrong.
					bool hasAtLeastOneVaryingExitPredecessor = false;
					bool hasAtLeastOneVaryingEntryPredecessor = false;
					for (unsigned i=0, e=node->getNumPredecessors(); i<e; ++i) {
						MaskGraphNode* predNode = node->getPredecessor(i);

						const bool predHasVaryingExit =
							!analysisResults->hasUniformExit(predNode->getBlock());
						hasAtLeastOneVaryingExitPredecessor |= predHasVaryingExit;

						const bool predHasVaryingEntry =
							!analysisResults->hasUniformEntry(predNode->getBlock());
						hasAtLeastOneVaryingEntryPredecessor |= predHasVaryingEntry;

						if (hasAtLeastOneVaryingEntryPredecessor &&
								hasAtLeastOneVaryingExitPredecessor) break;
					}

					// for readability ;)
					const bool hasOnlyUniformExitPredecessors =
						!hasAtLeastOneVaryingExitPredecessor;
					const bool hasOnlyUniformEntryPredecessors =
						!hasAtLeastOneVaryingEntryPredecessor;

					if (hasOnlyUniformExitPredecessors &&
							hasOnlyUniformEntryPredecessors)
					{
						// case 1b)
						// Multiple uniform incoming edges from uniform blocks.
						// This indicates entirely uniform control-flow, which
						// will not be linearized.
						// -> create phi for mask.
						MaskPhiOperation* phiOp = new MaskPhiOperation();

						for (unsigned i=0, e=node->getNumPredecessors(); i<e; ++i) {
							MaskGraphNode* predNode = node->getPredecessor(i);
							MaskOperation* tmpMask =
								new MaskNodeReference(predNode, node);
							assert (tmpMask);
							phiOp->addIncoming(tmpMask, predNode->getBlock());
						}

						entryMask = phiOp;
					} else {
						// case 1c)
						// At least one predecessor with VARYING exit or entry,
						// which means the preceding control-flow will be
						// linearized.
						// -> Use the common dominator's entry mask.

						// If any predecessor has a VARYING entry, the predecessors have to have a
						// common dominator and the current block has to be the post-dominating
						// join block (the block could not have a UNIFORM entry otherwise).

						BasicBlock* commonDominator = node->getPredecessor(0)->getBlock();

						typedef GraphTraits<Inverse<BasicBlock*> > InvBlockTraits;
						for (InvBlockTraits::ChildIteratorType PI = InvBlockTraits::child_begin(block),
								PE = InvBlockTraits::child_end(block); PI != PE; ++PI)
						{
							BasicBlock* predBB = *PI;
							commonDominator = domTree->findNearestCommonDominator(commonDominator, predBB);
						}

						assert (analysisResults->hasUniformEntry(commonDominator) &&
								"common dominator of disjunct paths which end uniformly has to have uniform entry!");

						assert (postDomTree->dominates(block, commonDominator) && "UNIFORM block with predecessors with VARYING exits has to post dominate common dominator!");

						DEBUG_PKT ( outs() << "  predecessors of UNIFORM block with VARYING exits have common dominator ("
							<< commonDominator->getName()
							<< ") that is post dominated by current block - can optimize entry mask!\n"; );
						assert (commonDominator);
						assert (maskGraph->findMaskNode(commonDominator));
						MaskGraphNode* domNode = maskGraph->findMaskNode(commonDominator);

						entryMask = new MaskNodeReference(domNode, MaskNodeReference::ENTRY);
					}

					// end UNIFORM entry

				} else {

					// VARYING entry

					// Block with multiple incoming edges
					assert (node->getNumPredecessors() > 1 &&
							"graph node has wrong number of predecessors!");

					BasicBlock* commonDominator = node->getPredecessor(0)->getBlock();

					typedef GraphTraits<Inverse<BasicBlock*> > InvBlockTraits;
					for (InvBlockTraits::ChildIteratorType PI = InvBlockTraits::child_begin(block),
							PE = InvBlockTraits::child_end(block); PI != PE; ++PI)
					{
						BasicBlock* predBB = *PI;
						commonDominator = domTree->findNearestCommonDominator(commonDominator, predBB);
					}

					if (commonDominator && postDomTree->dominates(block, commonDominator)) {
						// case 2)
						DEBUG_PKT ( outs() << "  predecessors of VARYING block have common dominator ("
								<< commonDominator->getName()
								<< ") that is post dominated by current block - can optimize entry mask!\n"; );

						// It would not be wrong, but our analysis should find out
						// that a join block is uniform if its common dominator had
						// a uniform entry.
						// NOTE: This obviously fires if UNIFORM/VARYING analysis is disabled.
						assert (!analysisResults->hasUniformEntry(commonDominator) &&
								"common dominator of disjunct paths which end VARYING should have VARYING entry (check for error in vectorization analysis)!");

						assert (maskGraph->findMaskNode(commonDominator));
						MaskGraphNode* domNode = maskGraph->findMaskNode(commonDominator);

						entryMask = new MaskNodeReference(domNode, MaskNodeReference::ENTRY);
					} else {
						// case 3)
						// The varying entry mask is the join over the exit mask of all predecessors.

						// For joining paths, use the 'or' of the exitmasks of all
						// incoming paths.
						// NOTE: We might have more than two incoming edges if there
						//       are no phis in the block!
						MaskGraphNode* predNode = node->getPredecessor(0);
						entryMask = new MaskNodeReference(predNode, node);
						assert (entryMask);

						for (unsigned i=1, e=node->getNumPredecessors(); i<e; ++i) {
							predNode = node->getPredecessor(i);
							MaskOperation* tmpMask =
								new MaskNodeReference(predNode, node);
							assert (tmpMask);
							entryMask = new MaskDisjunction(entryMask, tmpMask);
						}
					}

					// end VARYING entry

				}

				// end multiple predecessors

			}


		}

		assert (entryMask);

		//
		// Create exit mask(s).
		//

		MaskOperation* exitMaskT = NULL;
		MaskOperation* exitMaskF = NULL;

		BranchInst* blockBr = dyn_cast<BranchInst>(block->getTerminator());
		const bool hasConditionalExit = (blockBr && blockBr->isConditional());

		if (analysisResults->hasFullyUniformExit(block)) {
			// Either successor is executed by ALL instances.
			exitMaskT = new MaskValue(boolOneConst);
			if (hasConditionalExit) exitMaskF = new MaskValue(boolOneConst);
		} else if (analysisResults->hasUniformExit(block)) {
			MaskOperation* selfRef = new MaskNodeReference(node, node);
			exitMaskT = selfRef;
			if (hasConditionalExit) exitMaskF = selfRef;
		} else {
			// Create uniform exitMask, possibilities:
			// - entrymask && condition of terminating conditional branch of block itself
			// - entrymask
			// A self reference implicitly points to the entry mask.
			MaskOperation* selfRef = new MaskNodeReference(node, node);

			// Set mask to condition of branch of current block if existing.
			// If the condition is uniform, set exit masks of both edges to the
			// entry mask of the current block (without negation!).
			if (hasConditionalExit) {
				MaskOperation* exitCondition = new MaskValue(blockBr->getCondition());
				exitMaskT = new MaskConjunction(selfRef, exitCondition);
				exitMaskF = new MaskConjunction(selfRef, new MaskNegation(exitCondition));
			} else {
				exitMaskT = selfRef;
			}
		}

		assert (entryMask && "entry mask has to be set to some value!");
		assert (exitMaskT && "exit mask has to be set to some value!");
		assert ((!hasConditionalExit || exitMaskF) && "exit mask of false-branch has to be set to some value!");

		// Finally, insert node into mask graph
		node->setEntryMask(entryMask);
		node->setExitMaskTrue(exitMaskT);
		if (hasConditionalExit) node->setExitMaskFalse(exitMaskF);

		DEBUG_PKT(
			node->verify();
			outs() << "  generated mask information for block '" << block->getName() << "':\n";
			node->print(outs());
		);
	}


	//REQUIRES: loopInfo
	//REQUIRES: loop simplification
	//REQUIRES: masks
	//TODO: make everything const
	void generateLoopExitMaskInfo(Loop* loop, SmallVector<BasicBlock*, 4>* exitingBlocks) {
		assert (loop);
		// Ignore uniform loops.
		if (analysisResults->isUniform(loop)) {
			DEBUG_PKT ( outs() << "uniform sub-loop ignored: " << *loop; );
			return;
		}

		//we need information about the parent loops' mask phis during recursion.
		//thus, generate them here already and insert them into graph
		for (SmallVector<BasicBlock*, 4>::iterator it=exitingBlocks->begin(); it!=exitingBlocks->end(); ++it) {
			MaskPhiOperation* exitMaskPhi = new MaskPhiOperation();
			//set loop mask phi of corresponding loop exit node
			maskGraph->setLoopExitMaskPhi(*it, loop, exitMaskPhi);
			++MaskPhiCounter;
		}

		//now, recurse into nested loops
		for (Loop::iterator SL=loop->begin(); SL != loop->end(); ++SL)  {
			//SmallVectorImpl<BasicBlock*> exitingBlocksSL((*SL)->getBlocks().size());
			SmallVector<BasicBlock*, 4>* exitingBlocksSL = new SmallVector<BasicBlock*, 4>();
			(*SL)->getExitingBlocks(*exitingBlocksSL);
			DEBUG_PKT(
				outs() << "\nExiting blocks of nested loop "; (*SL)->print(outs(), (*SL)->getLoopDepth());
				for (SmallVector<BasicBlock*, 4>::iterator it=exitingBlocksSL->begin(); it!=exitingBlocksSL->end(); ++it) {
					outs() << "  * " << (*it)->getName() << "\n";
				}
			);
			generateLoopExitMaskInfo(*SL, exitingBlocksSL);
			delete exitingBlocksSL;
		}

		DEBUG_PKT( outs() << "\ngenerating exit mask phis for all exiting blocks of loop: "; loop->print(outs(), loop->getLoopDepth()); );
		BasicBlock* preheaderBB = loop->getLoopPreheader();
		BasicBlock* latchBB = loop->getLoopLatch();
		const bool isTopLevelLoop = analysisResults->isVaryingTopLevelLoop(loop);
		DEBUG_PKT(
			if (isTopLevelLoop) outs() << "  loop is varying \"top-level\" loop!\n";
			else outs() << "  loop is nested!\n";
		);

		for (SmallVector<BasicBlock*, 4>::iterator it=exitingBlocks->begin(); it!=exitingBlocks->end(); ++it) {
			BasicBlock* curExitBB = *it;

			DEBUG_PKT( outs() << "  generating exit mask phi for exiting block '" << curExitBB->getName() << "'...\n"; );

			const bool exitsMultipleLoops = maskGraph->exitsMultipleLoops(curExitBB);
			DEBUG_PKT( if (exitsMultipleLoops) outs() << "    block has multi-loop exit!\n"; );
			//			DEBUG_MG( maskGraph->getLoopExitNode(curExitBB)->print(outs()); )

			//find exit direction and corresponding mask
			MaskGraphNode* exitNode = maskGraph->findMaskNode(curExitBB);
			assert (exitNode && "there has to be a mask graph node for each block!");
			assert (exitNode->hasConditionalExit());
			assert (isa<BranchInst>(curExitBB->getTerminator()) && cast<BranchInst>(curExitBB->getTerminator())->isConditional());
			const bool exitEdgeOnTruePath = loop->contains(cast<BranchInst>(curExitBB->getTerminator())->getSuccessor(1));
			MaskOperation* exitMask = exitEdgeOnTruePath ? exitNode->getExitMaskTrue() : exitNode->getExitMaskFalse();
			DEBUG_PKT( outs() << "    exit mask (" << (exitEdgeOnTruePath ? "true" : "false") << "-path): "; exitMask->print(outs()); outs() << "\n"; );


			//if the mask of this loop exit edge does not have any uses, don't generate a phi for it
			//(this would only result in a circular dependence between phi and mask operation
			//which cannot be detected by packetizer, resulting in type error during packetization of instructions)
			//NOTE: if we use the loop exit masks for left/right blending, we must not skip those without uses
			//			if (exitMask->getNumUses() < 1) {
			//				outs() << "    exit mask does not have any uses, skipping phi generation!\n";
			//				continue;
			//			}

			//get mask phi of current exit (already generated before recursion into child-loops)
			assert (maskGraph->getLoopExitMaskPhiOp(curExitBB, loop));
			assert (typeid(*maskGraph->getLoopExitMaskPhiOp(curExitBB, loop)) == typeid(MaskPhiOperation));
			MaskPhiOperation* exitMaskPhi = static_cast<MaskPhiOperation*>(maskGraph->getLoopExitMaskPhiOp(curExitBB, loop));

			//incoming value from preheader is:
			// - for single-loop exit (~break): zero-vector (no instance has left the loop through this exit yet)
			// - for multi-loop exit (~break n/return) if in parent: zero-vector (no instance has left the loop through this exit yet)
			// - for multi-loop exit (~break n/return) if in nested loop: corresponding exit mask phi of parent loop
			if (exitsMultipleLoops && !isTopLevelLoop) {
				exitMaskPhi->addIncoming(new MaskNodeReference(exitNode, MaskNodeReference::LOOPEXITPHI), preheaderBB);
			} else {
				exitMaskPhi->addIncoming(new MaskValue(boolZeroConst), preheaderBB);
			}

			//incoming value from latch is:
			// - for single-loop exit (~break): new 'or' of loop's mask phi and the block's old exit mask ((negated) branch condition)
			// - for multi-loop exit (~break n/return) if in parent: the innermost loop's 'or'-operation
			// - for multi-loop exit (~break n/return) if in innermost loop: new 'or' of loop's mask phi and the block's old exit mask ((negated) branch condition)
			if (exitsMultipleLoops && !maskGraph->isInnermostLoopOfExit(curExitBB, loop)) {
				DEBUG_PKT( outs() << "    loop is not innermost loop of current multi-loop exit - not generating mask update operation!\n"; );
				assert (maskGraph->getLoopExitMaskUpdateOp(curExitBB));
				//exitMaskPhi->addIncoming(maskGraph->getLoopExitMaskUpdateOp(curExitBB), latchBB);
				assert (maskGraph->findLoopExitNode(curExitBB));
				BasicBlock* targetBlock = maskGraph->findLoopExitNode(curExitBB)->getExitBlock();
				assert (targetBlock);
				assert (maskGraph->findMaskNode(targetBlock));
				exitMaskPhi->addIncoming(new MaskNodeReference(exitNode, maskGraph->findMaskNode(targetBlock)), latchBB);
			} else {
				MaskDisjunction* newExitMask = new MaskDisjunction(exitMask, new MaskNodeReference(exitNode, MaskNodeReference::LOOPEXITPHI));
				DEBUG_PKT( outs() << "    generated new loop exit mask update operation: "; newExitMask->print(outs()); outs() << "\n"; );

				//store the new mask update operation
				maskGraph->setLoopExitMaskUpdateOp(curExitBB, newExitMask);

				//add incoming value to phi
				exitMaskPhi->addIncoming(newExitMask, latchBB);

				//update all dependent masks
				if (exitEdgeOnTruePath) maskGraph->setExitMaskTrue(exitNode, newExitMask);
				else maskGraph->setExitMaskFalse(exitNode, newExitMask);
			}

			DEBUG_PKT( outs() << "    generated loop exit mask phi: "; exitMaskPhi->print(outs()); outs() << "\n"; );
		}
		DEBUG_PKT( outs() << "finished generation of exit mask phis for loop!\n\n"; );
		return;
	}

	inline bool verifyMaskInsertion() {
		bool verified = true;
		for (MaskGraph::const_iterator it=maskGraph->begin(), E=maskGraph->end(); it!=E; ++it) {
			assert (it->first && "block must not be NULL!");
			assert (it->second && "node must not be NULL!");
			assert (it->first == it->second->getBlock() && "associated blocks have to match!");

			const bool nodeVerified = it->second->verify();
			if (!nodeVerified)
				errs() << "ERROR: verification of node of block '" << it->first->getName() << "' failed!\n";

			//mask insertion was successfull if all masks are MaskValues with direct value
			const bool maskVerified = (!it->second->hasPredecessors() || it->second->getEntryMask()->isMaskValue()) &&
				(!it->second->hasExitEdge() || it->second->getExitMaskTrue()->isMaskValue()) &&
				(!it->second->hasConditionalExit() || it->second->getExitMaskFalse()->isMaskValue());
			if (!maskVerified)
				errs() << "ERROR: block '" << it->first->getName() << "' still has compound masks in graph!\n";

			verified &= nodeVerified && maskVerified;
		}
		return verified;
	}

	//TODO: this function should check if any masks are redundant
	//e.g. if all branches merge at a block
	void removeUnnecessaryMaskInstructions() {
		//		//std::map<BasicBlock*, std::pair<unsigned, Value**> >
		//		for (MaskMapType::iterator it=masks.begin(); it!=masks.end(); ++it) {
		//			Value** maskArr = (*it).second.second;
		//			for (unsigned i=0; i<(*it).second.first; ++i) {
		//				Value* mask = maskArr[i];
		//				if (!isa<Instruction>(mask)) continue;
		//				Instruction* maskInstr = cast<Instruction>(mask);
		//				if (maskInstr->use_empty()) maskInstr->eraseFromParent();
		//				maskArr[i] = NULL;
		//			}
		//		}
	}

};

}


char MaskGenerator::ID = 0;
INITIALIZE_PASS_BEGIN(MaskGenerator, "mask generator", "Mask Generator", false, false)
INITIALIZE_PASS_DEPENDENCY(DominatorTree)
INITIALIZE_PASS_DEPENDENCY(PostDominatorTree)
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_DEPENDENCY(VectorizationAnalysis)
INITIALIZE_PASS_END(MaskGenerator, "mask generator", "Mask Generator", false, false)

// Public interface to the MaskGenerator pass
namespace llvm {

FunctionPass* createMaskGeneratorPass(bool* failed, const bool verbose=false) {
	return new MaskGenerator(failed, verbose);
}

}


#endif	/* _MASKGENERATOR_HPP */
