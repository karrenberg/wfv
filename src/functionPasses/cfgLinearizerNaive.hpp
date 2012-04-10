/**
 * @file   cfgLinearizerNaive.hpp
 * @date   26.07.2009
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2008, 2009, 2010 Saarland University
 *
 * This pass linearizes the blocks of the control flow graph
 * s.t. only conditional branches for loop backedges remain.
 *
 */
#ifndef _CFGLINEARIZERNAIVE_HPP
#define	_CFGLINEARIZERNAIVE_HPP


#ifdef DEBUG_TYPE
#undef DEBUG_TYPE
#endif
#define DEBUG_TYPE "cfglinearizernaive"

#include <llvm/Support/raw_ostream.h>

#include <llvm/Pass.h>
#include <llvm/Transforms/Scalar.h> //BreakCriticalEdges
#include <llvm/Analysis/LoopInfo.h>

#include "maskGenerator.hpp"
#include "utils/maskGraph.hpp"
#include "vectorizationAnalysis.hpp"
#include "utils/analysisResults.hpp"
#include "utils/llvmTools.hpp" // isDominatedBy
#include "utils/wholeFunctionVectorizationAAW.hpp"

#include "llvm/ADT/Statistic.h" //STATISTIC

// the following includes are only required for single-file compilation
#include <list>

namespace {
class SelectGenerator; // fwd def for addPreserved
}

// forward declaration of initializer
namespace llvm {
	void initializeCFGLinearizerNaivePass(PassRegistry&);
}

using namespace llvm;

STATISTIC(ConditionalBranchesRemovedCounterNaive, "Counts number of conditional branches that were removed");
STATISTIC(LoopBranchesGeneratedCounterNaive, "Counts number of conditional branches that were generated for new loop backedges");

namespace {

class CFGLinearizerNaive : public FunctionPass {
private:
	enum BackedgeType {
		NO_BACKEDGE,
		BACKEDGE_TRUE,
		BACKEDGE_FALSE,
		UNCONDITIONAL_BACKEDGE
	};

	const std::string getBackedgeTypeString(const BackedgeType& bi) const {
		switch (bi) {
			case NO_BACKEDGE: return "NO_BACKEDGE";
			case BACKEDGE_TRUE: return "BACKEDGE_TRUE";
			case BACKEDGE_FALSE: return "BACKEDGE_FALSE";
			case UNCONDITIONAL_BACKEDGE: return "UNCONDITIONAL_BACKEDGE";
		}
		assert (false && "must never happen!");
		return "INVALID";
	}

public:
	static char ID; // Pass identification, replacement for typeid
	CFGLinearizerNaive(bool* failed_flag=NULL, const bool verbose_flag=false)
			: FunctionPass(ID), failed(failed_flag), verbose(verbose_flag)
	{
		initializeCFGLinearizerNaivePass(*PassRegistry::getPassRegistry());
	}

	~CFGLinearizerNaive() {}

	virtual void releaseMemory() {}

	virtual void getAnalysisUsage(AnalysisUsage &AU) const {
		AU.addRequired<LoopInfo>();
		//AU.addRequired<BranchInfoAnalysis>();     // dummy
		//AU.addRequired<LoopLiveValueAnalysis>();  // dummy
		AU.addRequired<VectorizationAnalysis>();
		AU.addRequired<MaskGenerator>();

		//AU.addPreserved<DominatorTree>();         // not preserved
		//AU.addPreserved<PostDominatorTree>();     // not preserved
		//AU.addPreserved<LoopInfo>();              // not preserved
		//AU.addPreserved<BranchInfoAnalysis>();    // not preserved
		AU.addPreserved<LoopLiveValueAnalysis>(); // not preserved
		AU.addPreserved<VectorizationAnalysis>();
		AU.addPreserved<MaskGenerator>();
		AU.addPreserved<SelectGenerator>();
	}

	//radically linearize all blocks of function (= remove all conditional branches except for backedges)
	//if loops are encountered on one path, the other path is moved after the branch first
	virtual bool runOnFunction(Function& f) {
		ConditionalBranchesRemovedCounterNaive = 0;
		LoopBranchesGeneratedCounterNaive = 0;

		if (failed && *failed) return true;

		analysisResults       = getAnalysis<VectorizationAnalysis>().getAnalysisResults();
		maskGraph             = getAnalysis<MaskGenerator>().getMaskGraph();
		loopInfo              = &getAnalysis<LoopInfo>();

		assert (!maskGraph->hasCompoundMasks() && "mask operation insertion not complete!");

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "linearizing CFG of function '" << f.getNameStr() << "'...\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		try {
			linearize(f);
		}
		catch (std::logic_error& error) {
			errs() << "\nException occurred during CFG linearization: "
					<< error.what() << "\n";
			if (failed) *failed = true;
			return true;
		}
		catch (...) {
			errs() << "\nINTERNAL ERROR: Unexpected exception occurred during "
					<< "CFG linearization!\n";
			if (failed) *failed = true;
			return true;
		}

		DEBUG_PKT( outs() << "done.\n"; );

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "CFG-linearization finished.\n"; );
		//DEBUG_PKT( this->printBlockList(outs(), blockList); );

		//verification was done on the fly
		//assert (verify(&f) && "verification of cfg linearization failed!");

		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		// CFG is only preserved if no branch was removed or created
		return ConditionalBranchesRemovedCounterNaive > 0 || LoopBranchesGeneratedCounterNaive > 0;
	}

	void printBlockList(raw_ostream& o, const std::list<BasicBlock*>& blockList) const {
		o << "  conditional branches removed: " << ConditionalBranchesRemovedCounterNaive << "\n";
		o << "  conditional branches generated for loop backedges: " << LoopBranchesGeneratedCounterNaive << "\n";
		printLinearizedBlockList(o, blockList);
	}

private:
	bool* failed;
	const bool verbose;
	MaskGraph* maskGraph;
	AnalysisResults* analysisResults;
	LoopInfo* loopInfo;

	void linearize(Function& f) {
		const std::list<BasicBlock*> blockList = findLinearizedBlockOrdering(&f);

		DEBUG_PKT( outs() << "\ntransforming CFG to determined order of blocks...\n\n"; );
		//"re-wire" blocks (if control-flow is not uniform)
		//-> replace all branches by unconditional branch to next block in list
		// except for branches with backedges (only replace 'forward'-edge)
		// and except for uniform branches (which requires remembering some blocks).
		std::set<BasicBlock*> uniformTargetBlocks;
		//std::list<BasicBlock*>::iterator it = blockList.begin();
		//while (it != blockList.end()) {
		for (std::list<BasicBlock*>::const_iterator it=blockList.begin(),
				E=blockList.end(); it!=E; )
		{
			BasicBlock* BB1 = *it++;
			//if (it == blockList.end()) break;

			DEBUG_PKT ( outs() << "  scheduling block '" << BB1->getName() << "'...\n"; );

			if (isa<ReturnInst>(BB1->getTerminator())) {
				assert (it == E);
				continue;
			}

			assert (isa<BranchInst>(BB1->getTerminator()) && "block does not terminate with a branch!");
			BranchInst* br = cast<BranchInst>(BB1->getTerminator());
			assert (br && "branch is NULL!");

			BasicBlock* BB2 = *it;

			const BackedgeType backedgeType = getBackedgeType(BB1);

			// If the following block in the list is a target block of a uniform
			// conditional branch that we have seen before, we must not create a
			// branch to it from here in order to retain control flow.
			// NOTE: If the current block's branch is conditional again,
			//       this does not have to be handled separately, as the
			//       branch has to be uniform anyway and thus executes the
			//       next case (nested ifs).

			// Don't change successors if the next block in the list
			// is the target of a uniform conditional branch.
			// In these cases, we do not have to add another target block
			// because the other branch is a back-edge.
			const bool nextBlockIsTarget =
				uniformTargetBlocks.find(BB2) != uniformTargetBlocks.end();
			assert (!nextBlockIsTarget || analysisResults->isUniform(br));

			// If this is a branch that exits a varying loop, linearize anyway.
			// This is required because we rely on the blending in the loop
			// latch to happen again right before exiting the loop.
			// TODO: It might be possible to optimize cases where some exits of
			//       a loop are uniform while others are not, but this would
			//       require handling such loops differently in mask/select
			//       generation. I think it should be a vary rare situation that
			//       such a case would have a large impact on performance.
			Loop* loop = loopInfo->getLoopFor(BB1);
			const bool branchExitsVaryingLoop = loop &&
				!analysisResults->isUniform(loop) &&
				(!loop->contains(br->getSuccessor(0)) ||
				br->isConditional() ? // otherwise we must not query getSucc(1)
					!loop->contains(br->getSuccessor(1)) : false);

			// If the branch is uniform, we leave the control flow untouched.
			// Therefore, do not remove the conditional branch if it is
			// uniform. Additionally, store the target block that is higher
			// in the list to know which other branch must not be replaced.
			// -> The higher successor is the one that got "pulled" upwards
			// and therefore it possibly has a branch to the lower successor
			// that does not directly succeed it in the list (which would be
			// removed by the linearization).
			// Thus, we store the LOWER successor to find these branches.
			if (backedgeType == NO_BACKEDGE &&
					br->isConditional() &&
					!branchExitsVaryingLoop &&
					analysisResults->isUniform(br))
			{
				DEBUG_PKT ( outs() << "    uniform conditional branch found\n"; );
				// Store the target block that is lower in the list
				// TODO: rewrite to only iterate list once
				const unsigned dist0 = getListDistance(BB1, br->getSuccessor(0), blockList);
				const unsigned dist1 = getListDistance(BB1, br->getSuccessor(1), blockList);
				assert (dist0 != 0 && "successor 0 not found in list!");
				assert (dist1 != 0 && "successor 1 not found in list!");
				assert (dist0 != dist1);
				BasicBlock* laterTargetBB = dist0 < dist1 ?
					br->getSuccessor(1) : br->getSuccessor(0);
				uniformTargetBlocks.insert(laterTargetBB);
				DEBUG_PKT ( outs() << "    added block to target list: '"
						<< laterTargetBB->getName() << "'\n"; );
				continue;
			}


			if (nextBlockIsTarget) {
				DEBUG_PKT ( outs() << "    next block is target block of "
						"uniform conditional branch!\n"; );
				uniformTargetBlocks.erase(BB2);

				// Here we know which block is the final join point of the
				// uniform conditional branch.
				BasicBlock* endBB;
				if (br->isConditional()) {
					// Store the target block that is higher in the list ("nearer")
					// TODO: rewrite to only iterate list once
					const unsigned dist0 = getListDistance(BB1, br->getSuccessor(0), blockList);
					const unsigned dist1 = getListDistance(BB1, br->getSuccessor(1), blockList);
					assert (dist0 != 0 && "successor 0 not found in list!");
					assert (dist1 != 0 && "successor 1 not found in list!");
					assert (dist0 != dist1);
					endBB = dist1 < dist0 ? br->getSuccessor(1) : br->getSuccessor(0);
				} else {
					endBB = br->getSuccessor(0);
				}
				//DEBUG_PKT( replaceSelectsByPhiNodes(endBB); );
				continue;
			}

			// Otherwise, we have varying control-flow.
			// -> Replace branches.

			DEBUG_PKT ( outs() << "    back-edge-type of block '"
					<< BB1->getNameStr() << "': "
					<< getBackedgeTypeString(backedgeType) << "\n"; );

			switch (backedgeType) {
				case NO_BACKEDGE:
				{
					// Replace branch (conditional or unconditional)
					// by unconditional branch to next block in list
					// NOTE: this implicitly performs if-conversion!
					analysisResults->removeValueInfo(br);
					br->eraseFromParent();
					BranchInst* newBr = BranchInst::Create(BB2, BB1);
					DEBUG_PKT ( outs() << "    re-wired block " << BB1->getName() << " to block " << BB2->getName() << "\n";  );

					analysisResults->addValueInfo(newBr,
													   AnalysisResults::UNIFORM,
													   AnalysisResults::INDEX_SAME,
													   AnalysisResults::ALIGN_FALSE,
													   AnalysisResults::SPLIT_NEVER);
					break;
				}
				case BACKEDGE_TRUE:
				{
					br->setSuccessor(1, BB2);
					DEBUG_PKT ( outs() << "    re-wired false-edge of block " << BB1->getName() << " to block " << BB2->getName() << "\n";  );
					break;
				}
				case BACKEDGE_FALSE:
				{
					br->setSuccessor(0, BB2);
					DEBUG_PKT ( outs() << "    re-wired true-edge of block " << BB1->getName() << " to block " << BB2->getName() << "\n";  );
					break;
				}
				case UNCONDITIONAL_BACKEDGE:
				{
					DEBUG_PKT ( outs() << "    unconditional back-edge found!\n"; );
					//found unconditional branch to header
					//replace by conditional branch to header/next block in list
					BasicBlock* header = br->getSuccessor(0);
					analysisResults->removeValueInfo(br);
					br->eraseFromParent();

					assert (maskGraph->findMaskNode(BB1) && "unconditional loop-latch has to have associated mask graph node!");
					assert (maskGraph->findMaskNode(BB1)->hasExitEdge() && "block has to have associated exit mask");
					assert (!maskGraph->findMaskNode(BB1)->hasConditionalExit() && "block must not have more than one exit mask if it terminates with an unconditional branch!");
					//use exit mask of block as condition for branch
					Value* cond = maskGraph->getExitMaskTrue(BB1);
					assert (cond && "condition must not be NULL!");
					BranchInst* newBr = BranchInst::Create(header, BB2, cond, BB1);
					DEBUG_PKT ( outs() << "    re-wired block " << BB1->getName() << " to blocks " << header->getName() << " and " << BB2->getName() << "\n"; );

					// store information about newly created branch
					const AnalysisResults::ValueInfo* info =
						analysisResults->getValueInfo(cond);
					assert (info);
					analysisResults->addValueInfo(newBr, info->uniformInfo, info->indexInfo, info->alignmentInfo, info->splitInfo);
					break;
				}
			}
		}
	}
	

	inline BackedgeType getBackedgeType(BasicBlock* block) {
		Loop* loop = loopInfo->getLoopFor(block);
		if (!loop) return NO_BACKEDGE;
		if (loop->getLoopLatch() != block) return NO_BACKEDGE;
		assert (isa<BranchInst>(block->getTerminator()) && "latch does not terminate with a branch!");
		BranchInst* br = cast<BranchInst>(block->getTerminator());
		if (br->isUnconditional()) return UNCONDITIONAL_BACKEDGE;

		if (loop->contains(br->getSuccessor(0))) return BACKEDGE_TRUE;
		else if (loop->contains(br->getSuccessor(1))) return BACKEDGE_FALSE;
		else return NO_BACKEDGE;
	}

	std::list<BasicBlock*> findLinearizedBlockOrdering(Function* f) {
		DEBUG_PKT( outs() << "\n-----------------------------------------------------\n"; );
		DEBUG_PKT( outs() << "determining linearization of function '" << f->getNameStr() << "'...\n"; );

		BasicBlock* returnBlock = findReturnBlock(f);
		assert (loopInfo->getLoopDepth(returnBlock) == 0 && "return-block of a function must not be inside a loop!");

		std::list<BasicBlock*> list;
		std::vector<std::list<BasicBlock*> > loopLists;

		//recursively walk BU through function and add all blocks that are not part of a loop
		std::set<BasicBlock*> tmpSet;
		recFindLinearizedBlockOrdering(returnBlock, list, tmpSet);

		//print list
		DEBUG_PKT(
			outs() << "\nprinting list of blocks outside of all loops:\n";
			for (std::list<BasicBlock*>::iterator it=list.begin(), E=list.end(); it!=E; ++it) {
				BasicBlock* BB = *it;
				outs() << "  * " << BB->getNameStr() << " (" << loopInfo->getLoopDepth(BB) << ")\n";
			}
			outs() << "\n";
		);

		//construct linearized block-lists for all top-level-loops
		for (LoopInfo::iterator L=loopInfo->begin(); L!=loopInfo->end(); ++L) {
			std::list<BasicBlock*> loopList = findLoopLinearization(*L);
			// TODO: DEBUG_PKT ?
			bool verified = verifyLinearizedLoopBlockList(loopList, *L);
			if (!verified) {
				//(*L)->print(outs());
				printLinearizedBlockList(outs(), loopList);
				outs() << "\n";
				f->viewCFGOnly();
			}
			assert (verified && "verification of linearized block ordering of loop failed!");
			loopLists.push_back(loopList);
		}


		//merge lists of all loops together with blocks outside loops
		//walk list of this loop, test for each block if it is the preheader of a sub-loop
		//if yes, merge the sub-loop-list behind that block
		//TODO: remove boolean & assertion and place a 'break' after insertion
		//TODO: possibly walk backwards in order to find the last possible entry to the loop
		unsigned i=0;
		for (LoopInfo::iterator L=loopInfo->begin(); L!=loopInfo->end(); ++L) {
			Loop* loop = *L;
			bool preheaderFound = false;
			for (std::list<BasicBlock*>::iterator it=list.begin(), E=list.end(); it!=E; ) {
				if (loop->getLoopPreheader() == *it) {
					assert (!preheaderFound && "loop cannot have more than one pre-header!");
					//DEBUG_PKT ( outs() << "  preheader found (1): " << (*it)->getNameStr() << "\n"; );
					list.insert(++it, loopLists[i].begin(), loopLists[i].end());
					preheaderFound = true;
					DEBUG_PKT( outs() << "inserted top-level-loop list " << i << "!\n"; );
				} else ++it;
			}
			++i;
			assert (preheaderFound && "could not find preheader for loop!");
		}

		//verify list
		bool verified = verifyLinearizedBlockList(list, f);
		if (!verified) {
			printLinearizedBlockList(outs(), list);
			DEBUG_PKT( f->viewCFGOnly(); );
		}

		assert (verified && "verification of linearized block ordering failed!");

		DEBUG_PKT( outs() << "linearization determined for function '" << f->getNameStr() << "'.\n"; );
		DEBUG_PKT( printLinearizedBlockList(outs(), list); );
		DEBUG_PKT( outs() << "-----------------------------------------------------\n"; );
		return list;
	}

	void recFindLinearizedBlockOrdering(BasicBlock* block, std::list<BasicBlock*> &list, std::set<BasicBlock*>& visitedBlocks) {
		if (visitedBlocks.find(block) != visitedBlocks.end()) return;
		visitedBlocks.insert(block);

		//recurse into predecessors
		typedef GraphTraits<Inverse<BasicBlock*> > InvBlockTraits;
		for (InvBlockTraits::ChildIteratorType PI = InvBlockTraits::child_begin(block),
				PE = InvBlockTraits::child_end(block); PI != PE; ++PI) {
			BasicBlock* pred = *PI;
			recFindLinearizedBlockOrdering(pred, list, visitedBlocks);
		}

		//after returning from recursion, add current block to list
		//IFF it does not belong to a loop
		if (loopInfo->getLoopDepth(block) == 0) {
			list.push_back(block);
			DEBUG_PKT( outs() << "      added block: " << block->getNameStr() << "\n"; );
		}
	}

	//recursively linearizes loop and its sub-loops and merges the lists
	//TODO: should use visited-blocks-list?
	std::list<BasicBlock*> findLoopLinearization(Loop* loop) {
		std::list<BasicBlock*> loopList;
		std::list<BasicBlock*>* subLoopLists = new std::list<BasicBlock*>[loop->getSubLoops().size()]();

		//recurse into sub-loops
		unsigned i=0;
		for (Loop::iterator SL=loop->begin(); SL!=loop->end(); ++SL) {
			std::list<BasicBlock*> curList = findLoopLinearization(*SL);
			const bool verified = verifyLinearizedLoopBlockList(curList, *SL);
			if (!verified) {
				printLinearizedBlockList(outs(), curList);
				DEBUG_PKT( loop->getHeader()->getParent()->viewCFGOnly(); );
			}
			assert (verified && "verification of linearized block ordering of sub-loop failed!");
			subLoopLists[i++] = curList;
		}
		//DEBUG_PKT ( outs() << "  findLoopLinearization(" << loop->getLoopDepth() << ")\n"; );

		//linearize all blocks that belong to 'loop'
		std::set<BasicBlock*> tmpSet;
		recFindLoopLinearization(loop, loop->getLoopLatch(), loop->getHeader(), loopList, tmpSet);

		//TODO: it is possibly not enough to just walk BU from latch... we might not cover all loop-blocks
		//maybe better walk TD from header to be sure to catch all exits
		//            for (Loop::block_iterator it=loop->block_begin(), E=loop->block_end(); it!=E; ++it) {
		//                BasicBlock* BB = *it;
		//                if (visitedBlocks->find(BB) != visitedBlocks->end()) continue; //block already in list
		//                std::vector<BasicBlock*> loopEntries;
		//                std::vector<BasicBlock*> loopExits;
		//                recFindLoopLinearization(loop, BB, loop->getHeader(), loopEntries, loopExits, list, visitedBlocks);
		//            }

		//print lists
		DEBUG_PKT(
			if (!loop->getSubLoops().empty()) outs() << "\nprinting lists of sub-loops:\n";
			for (unsigned i=0; i<loop->getSubLoops().size(); ++i) {
				outs() << "  sub-loop " << i << "\n";
				for (std::list<BasicBlock*>::iterator it=subLoopLists[i].begin(); it!=subLoopLists[i].end(); ++it) {
					outs() << "   * " << (*it)->getNameStr() << "\n";
				}
			}
		);

		//now merge list of current loop with lists of its sub-loops
		//walk list of this loop, test for each block if it is the preheader of a sub-loop
		//if yes, merge the sub-loop-list behind that block
		//TODO: remove boolean & assertion and place a 'break' after insertion
		//TODO: possibly walk backwards in order to find the last possible entry to the loop
		unsigned j=0;
		for (Loop::iterator SL=loop->begin(); SL!=loop->end(); ++SL) {
			Loop* subLoop = *SL;
			bool subLoopEntryFound = false;
			for (std::list<BasicBlock*>::iterator it=loopList.begin(), E=loopList.end(); it!=E; ) {
				if (subLoop->getLoopPreheader() == *it) {
					//DEBUG_PKT ( outs() << "  preheader found: " << (*it)->getNameStr() << "\n"; );
					assert (!subLoopEntryFound && "block cannot be preheader of several sub-loops!");
					loopList.insert(++it, subLoopLists[j].begin(), subLoopLists[j].end());
					subLoopEntryFound = true;
					DEBUG_PKT( outs() << "inserted sub-loop list " << j << "!\n"; );
				} else ++it;
			}
			++j;
			assert (subLoopEntryFound && "could not find preheader for sub-loop!");
		}

		DEBUG_PKT(
			if (!loopList.empty()) outs() << "\nprinting list of merged loop:\n";
			for (std::list<BasicBlock*>::iterator it=loopList.begin(); it!=loopList.end(); ++it) {
				outs() << "  * " << (*it)->getNameStr() << "\n";
			}
			outs() << "\n";
		);

		delete[] subLoopLists;

		return loopList;
	}

	//recursively walk all predecessors of 'block' until header is reached
	//then add blocks to 'list' when returning
	//NOTE: 'block' has to be initialized with loop-latch
	void recFindLoopLinearization(Loop* loop, BasicBlock* block, BasicBlock* headerBB, std::list<BasicBlock*> &list, std::set<BasicBlock*>& visitedBlocks) {
		//if (loopInfo->getLoopDepth(block) < loop->getLoopDepth()) return; //same as loop->contains(block)
		if (!loop->contains(block)) return;
		if (visitedBlocks.find(block) != visitedBlocks.end()) return;
		visitedBlocks.insert(block);

		//DEBUG_PKT ( outs() << "    recFindLoopLinearization(" << block->getNameStr() << ", " << loop->getLoopDepth() << ")\n"; );

		//recurse into all predecessors that belong to the same loop
		//and are not yet in the list
		typedef GraphTraits<Inverse<BasicBlock*> > InvBlockTraits;
		for (InvBlockTraits::ChildIteratorType PI = InvBlockTraits::child_begin(block),
				PE = InvBlockTraits::child_end(block); PI != PE; ++PI) {
			BasicBlock* pred = *PI;
			recFindLoopLinearization(loop, pred, headerBB, list, visitedBlocks);
		}

		//after returning from recursion, add current block to list
		//IFF it belongs to current loop (excluding sub-loops!)
		if (loopInfo->getLoopDepth(block) == loop->getLoopDepth()) {
			list.push_back(block);
			DEBUG_PKT( outs() << "      added loop-block: " << block->getNameStr() << "\n"; );
		}
	}

	inline bool verifyLinearizedBlockList(std::list<BasicBlock*>& list, Function* f) {
		bool verified = true;
		for (std::list<BasicBlock*>::iterator it=list.begin(), E=list.end(); it!=E; ++it) {
			BasicBlock* BB = *it;
			for (std::list<BasicBlock*>::iterator it2=list.begin(), E=list.end(); it2!=E; ++it2) {
				BasicBlock* BB2 = *it2;
				if (it2 == it) continue;
				if (BB == BB2) {
					errs() << "ERROR: block '" << BB->getNameStr() << "' appears in list more than once!\n";
					verified = false;
				}
			}
		}
		if (list.size() != f->getBasicBlockList().size()) {
			errs() << "ERROR: all blocks of the function have to be in the linearized list!\n";
			verified = false;
		}
		return verified;
	}
	inline bool verifyLinearizedLoopBlockList(std::list<BasicBlock*>& loopList, Loop* loop) {
		bool verified = true;
		for (std::list<BasicBlock*>::iterator it=loopList.begin(), E=loopList.end(); it!=E; ++it) {
			BasicBlock* BB = *it;
			for (std::list<BasicBlock*>::iterator it2=loopList.begin(), E=loopList.end(); it2!=E; ++it2) {
				BasicBlock* BB2 = *it2;
				if (it2 == it) continue;
				if (BB == BB2) {
					errs() << "ERROR: block '" << BB->getNameStr() << "' appears in loop-list more than once!\n";
					verified = false;
				}
			}
		}
		if (loopList.size() < loop->getBlocks().size()) {
			errs() << "ERROR: all blocks of the loop and its sub-loops have to be in linearized list!\n";
			DEBUG_PKT(
				outs() << "loopList.size(): " << loopList.size() << "\n";
				outs() << "loop->getBlocks().size(): " << loop->getBlocks().size() << "\n";
				outs() << "loop-blocks:\n";
				for (std::vector<BasicBlock*>::const_iterator it=loop->getBlocks().begin(); it!=loop->getBlocks().end(); ++it) {
					outs() << "  * " << (*it)->getNameStr() << "\n";
				}
			);
			verified = false;
		} else if (loopList.size() > loop->getBlocks().size()) {
			errs() << "WARNING: linearized list of loop has more blocks than loop itself!\n";
		}
		return verified;
	}

	inline void printLinearizedBlockList(raw_ostream& o,
										 const std::list<BasicBlock*>& list) const
	{
		o << "\nlinearized order of blocks:\n";
		for (std::list<BasicBlock*>::const_iterator it=list.begin(), E=list.end(); it!=E; ++it) {
			BasicBlock* BB = *it;
			o << "  * " << BB->getNameStr() << " (" << loopInfo->getLoopDepth(BB) << ")\n";
		}
		o << "\n";
	}


	// Returns the unique return block of function 'f'.
	// We rely on the ReturnUnifier pass and thus terminate as soon as we
	// have found a return.
	// TODO: move to tools or sth.
	BasicBlock* findReturnBlock(Function* f) const {
		for (Function::iterator BB=f->begin(), BBE=f->end(); BB!=BBE; ++BB) {
			assert (BB->getTerminator() && "each basic block has to have a terminator!");
			if (isa<ReturnInst>(BB->getTerminator())) return BB;
		}
		assert (false && "ERROR: Function does not contain a return statement!");
		throw std::logic_error("ERROR: Function does not contain a return statement!");
	}


	unsigned getListDistance(const BasicBlock* source,
							 const BasicBlock* target,
							 const std::list<BasicBlock*>& blockList) const
	{
		unsigned dist = 0;
		std::list<BasicBlock*>::const_iterator it = blockList.begin();
		while (it != blockList.end()) {
			BasicBlock* BB1 = *it++;
			if (BB1 == target) return dist;
			if (it == blockList.end()) break;
			++dist;
		}
		return 0;
	}

	// Replace all select instructions in 'block' by phi nodes again
	// Not required - only used in debug mode right now.
	void replaceSelectsByPhiNodes(BasicBlock* block) {
		assert (false && "SHOULD NEVER BE CALLED!");
		for (BasicBlock::iterator I=block->begin(), IE=block->end(); I!=IE; ) {
			if (!isa<SelectInst>(I)) break; // only transform those that are in the place of a phi

			assert (false && "there should not be any select left if mask/select generation work correctly!");

			SelectInst* sel = cast<SelectInst>(I++);
			// ignore result selects
			if (std::strstr(sel->getNameStr().c_str(), "result.vec") != 0) continue;


			Value* trueVal = sel->getTrueValue();
			Value* falseVal = sel->getFalseValue();
			// find predecessors for these values
			typedef GraphTraits<Inverse<BasicBlock*> > InvBlockTraits;
			InvBlockTraits::ChildIteratorType PI = InvBlockTraits::child_begin(block);
			BasicBlock* pred0BB = *PI++;
			assert (PI != InvBlockTraits::child_end(block) && "must not have less than 2 predecessors!");
			BasicBlock* pred1BB = *PI;
			assert (++PI == InvBlockTraits::child_end(block) && "must not have more than 2 predecessors!");

			// check which predecessor belongs to which value
			BasicBlock* trueValBB;
			BasicBlock* falseValBB;
			if (Instruction* trueValInst = dyn_cast<Instruction>(trueVal)) {
				const bool pred0IsTrueValBB =
					Packetizer::isDominatedBy(pred0BB, trueValInst->getParent(), *loopInfo);
				trueValBB = pred0IsTrueValBB ? pred0BB : pred1BB;
				falseValBB = pred0IsTrueValBB ? pred1BB : pred0BB;
			} else {
				assert (isa<Instruction>(falseVal) && "select must select from at least one instruction!");
				Instruction* falseValInst = cast<Instruction>(falseVal);
				const bool pred1IsFalseValBB =
					Packetizer::isDominatedBy(pred1BB, falseValInst->getParent(), *loopInfo);
				trueValBB = pred1IsFalseValBB ? pred0BB : pred1BB;
				falseValBB = pred1IsFalseValBB ? pred1BB : pred0BB;
			}

			PHINode* phi = PHINode::Create(sel->getType(), sel->getName(), block->getFirstNonPHI());
			phi->addIncoming(trueVal, trueValBB);
			phi->addIncoming(falseVal, falseValBB);

			AnalysisResults::ValueInfo* info =
					analysisResults->getValueInfo(sel);
			assert (info);
			analysisResults->addValueInfo(phi,
											   info->uniformInfo,
											   info->indexInfo,
											   info->alignmentInfo,
											   info->splitInfo,
											   info->isMask);
			analysisResults->removeValueInfo(sel);

			sel->replaceAllUsesWith(phi);
			sel->eraseFromParent();
		}
	}


};

} // unnamed namespace


char CFGLinearizerNaive::ID = 0;
INITIALIZE_PASS_BEGIN(CFGLinearizerNaive, "naive cfg linearizer", "Naive CFG Linearizer", false, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_DEPENDENCY(VectorizationAnalysis)
INITIALIZE_PASS_DEPENDENCY(MaskGenerator)
INITIALIZE_PASS_END(CFGLinearizerNaive, "naive cfg linearizer", "Naive CFG Linearizer", false, false)

// Public interface to the MaskGenerator pass
namespace llvm {

FunctionPass* createCFGLinearizerNaivePass(bool* failed, const bool verbose=false) {
	return new CFGLinearizerNaive(failed, verbose);
}

}


#endif	/* _CFGLINEARIZERNAIVE_HPP */

