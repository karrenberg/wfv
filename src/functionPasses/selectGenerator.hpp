/**
 * @file   selectGenerator.hpp
 * @date   25.07.2009
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2008, 2009, 2010 Saarland University
 *
 * This pass generates select operations everywhere different paths of the CFG
 * join for all values that are live on more than one incoming edge.
 * This is especially important for loops.
 *
 * @REQUIRES: loopInfo
 * @REQUIRES: maskInfo
 * @REQUIRES: canonicalized phis (less than 2 incoming edges per block)
 *
 */
#ifndef _SELECTGENERATOR_HPP
#define	_SELECTGENERATOR_HPP

#ifdef DEBUG_TYPE
#undef DEBUG_TYPE
#endif
#define DEBUG_TYPE "selectgenerator"

#include "llvm/Support/raw_ostream.h"

#include <llvm/Pass.h>
#include <llvm/Analysis/LoopInfo.h>

#include "maskGenerator.hpp"
#include "utils/maskGraph.hpp"
#include "loopLiveValueAnalysis.hpp"
#include "vectorizationAnalysis.hpp"
#include "utils/analysisResults.hpp"
#include "utils/wholeFunctionVectorizationAAW.hpp"

#include "llvm/ADT/Statistic.h" //STATISTIC

// the following includes are only required for single-file compilation
#include "utils/llvmTools.hpp" // isDominatedBy

// forward declaration of initializer
namespace llvm {
	void initializeSelectGeneratorPass(PassRegistry&);
}

using namespace llvm;

STATISTIC(SelectCounter, "Counts number of select operations that were generated");
STATISTIC(NewMaskCounter, "Counts number of additional mask operations that were generated");

namespace {

class SelectGenerator : public FunctionPass {
public:
	static char ID; // Pass identification, replacement for typeid
	SelectGenerator(bool* failed_flag=NULL, const bool verbose_flag=false)
			: FunctionPass(ID), failed(failed_flag), verbose(verbose_flag)
	{
		// initialize constants
		boolOneConst = Constant::getAllOnesValue(Type::getInt1Ty(getGlobalContext()));
		boolZeroConst = Constant::getNullValue(Type::getInt1Ty(getGlobalContext()));

		initializeSelectGeneratorPass(*PassRegistry::getPassRegistry());
	}

	~SelectGenerator() {}

	virtual void releaseMemory() {
		for (LoopResultMapType::iterator it=loopResultMap.begin(), E=loopResultMap.end(); it!=E; ++it) {
			delete it->second;
		}
		loopResultMap.clear();

		for (SelectInfoMapType::iterator it=selectInfoMap.begin(), E=selectInfoMap.end(); it!=E; ++it) {
			delete it->second;
		}
		selectInfoMap.clear();
	}

	virtual void getAnalysisUsage(AnalysisUsage &AU) const {
		AU.addRequired<LoopInfo>();
		//AU.addRequired<BranchInfoAnalysis>();  // dummy
		AU.addRequired<LoopLiveValueAnalysis>();
		AU.addRequired<VectorizationAnalysis>();
		AU.addRequired<MaskGenerator>();

		AU.addPreserved<DominatorTree>();
		AU.addPreserved<PostDominatorTree>();
		AU.addPreserved<LoopInfo>();
		AU.addPreserved<BranchInfoAnalysis>();
		AU.addPreserved<LoopLiveValueAnalysis>();
		AU.addPreserved<VectorizationAnalysis>();
		AU.addPreserved<MaskGenerator>();
		AU.setPreservesCFG(); // doesn't this include DT/PDT/LI?
	}

	virtual bool runOnFunction(Function& F) {
		SelectCounter = 0;
		NewMaskCounter = 0;

		if (failed && *failed) return true;

		loopInfo              = &getAnalysis<LoopInfo>();
		loopLiveValueAnalysis = &getAnalysis<LoopLiveValueAnalysis>();
		analysisResults       = getAnalysis<VectorizationAnalysis>().getAnalysisResults();
		maskGraph             = getAnalysis<MaskGenerator>().getMaskGraph();

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "generating selects for function '" << F.getNameStr() << "'...\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		try {
			bool changed = generatePhiSelects(F);

			changed |= generateLoopSelects(F); //'changed' is never used...
		}
		catch (std::logic_error& error) {
			errs() << "\nException occurred during select generation: "
					<< error.what() << "\n";
			if (failed) *failed = true;
			return true;
		}
		catch (...) {
			errs() << "\nINTERNAL ERROR: Unexpected exception occurred during "
					<< "select generation!\n";
			if (failed) *failed = true;
			return true;
		}

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "select-generation finished.\n"; );
		DEBUG_PKT( this->print(outs()); );

		assert (verify(F) && "verification of select generation failed!");

		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		return 0; // CFG is preserved
	}

	void print(raw_ostream &o, const Module* M=NULL) const {
		o << "  select operations generated: " << SelectCounter << "\n";
		o << "  additional mask operations generated: " << NewMaskCounter << "\n";
	}

	// TODO: implement
	// NOTE: We must not use llvm's verification because we generate values that
	//       break the SSA dominance property (masks/selects that rely on CFG
	//       linearization).
	inline bool verify(const Function& f) const {
		DEBUG_PKT( outs() << "verifying generation of selects for function '" << f.getNameStr() << "'... "; );
		bool verified = true;
		// TODO: implement
//		for (Function::const_iterator BB=f.begin(); BB!=f.end(); ++BB) {
//			bool nonPsiFound = false;
//			for (BasicBlock::const_iterator I=BB->begin(); I!=BB->end(); ++I) {
//				if (nonPsiFound && isa<SelectInst>(I)) {
//					//TODO: this gives a false error if the select belongs
//					//to some other computation :P)
//					errs() << "ERROR: psi has to dominate all other instructions in block except phis!\n";
//					verified = false;
//				}
//				if (isa<PHINode>(I)) continue;
//				if (!isa<SelectInst>(I)) nonPsiFound = true;
//			}
//		}
		if (verified) { DEBUG_PKT( outs() << "done.\n"; ); }
		else errs() << "verification of select generation failed!\n";
		return verified;
	}

	struct SelectInfo {
		SelectInfo(Value* trueVal, Value* falseVal, BasicBlock* cDir, BasicBlock* trueBB, BasicBlock* falseBB)
				: incomingValueTrue(trueVal), incomingValueFalse(falseVal), conditionDir(cDir), incomingBlockTrue(trueBB), incomingBlockFalse(falseBB)
		{}
		Value* incomingValueTrue;
		Value* incomingValueFalse;
		BasicBlock* conditionDir;
		BasicBlock* incomingBlockTrue;
		BasicBlock* incomingBlockFalse;
	};

	inline Value* getIncomingValueTrue(const SelectInst* select) const {
		assert (select);
		SelectInfoMapType::const_iterator it = selectInfoMap.find(select);
		assert (it != selectInfoMap.end());

		return it->second->incomingValueTrue;
	}
	inline Value* getIncomingValueFalse(const SelectInst* select) const {
		assert (select);
		SelectInfoMapType::const_iterator it = selectInfoMap.find(select);
		assert (it != selectInfoMap.end());

		return it->second->incomingValueFalse;
	}
	inline BasicBlock* getConditionDir(const SelectInst* select) const {
		assert (select);
		SelectInfoMapType::const_iterator it = selectInfoMap.find(select);
		assert (it != selectInfoMap.end());

		return it->second->conditionDir;
	}
	inline BasicBlock* getIncomingBlockTrue(const SelectInst* select) const {
		assert (select);
		SelectInfoMapType::const_iterator it = selectInfoMap.find(select);
		assert (it != selectInfoMap.end());

		return it->second->incomingBlockTrue;
	}
	inline BasicBlock* getIncomingBlockFalse(const SelectInst* select) const {
		assert (select);
		SelectInfoMapType::const_iterator it = selectInfoMap.find(select);
		assert (it != selectInfoMap.end());

		return it->second->incomingBlockFalse;
	}
	

private:
	bool* failed;
	const bool verbose; // required for DEBUG mode
	LoopInfo* loopInfo;
	LoopLiveValueAnalysis* loopLiveValueAnalysis;
	AnalysisResults* analysisResults;
	MaskGraph* maskGraph;

	Constant* boolOneConst;
	Constant* boolZeroConst;

	typedef std::map<const SelectInst*, SelectInfo*> SelectInfoMapType;
	SelectInfoMapType selectInfoMap;


	// Call 'generateSelectFromPhi()' for each phi of each block of 'f'
	// that is not the header of a loop and has not only predecessors with
	// uniform exits in this direction.
	//REQUIRES: loopInfo
	bool generatePhiSelects(Function& f) {
		//loop over all blocks that contain a phi
		bool changed = false;
		for (Function::iterator BB=f.begin(); BB != f.end(); ++BB) {
			if (BB->getFirstNonPHI() == &(BB->front())) continue; //no phi, do nothing
			//due to loop simplification there are no cases where header
			//has more than one incoming edge from outside, so we can
			//safely ignore this block
			if (Loop* loop = loopInfo->getLoopFor(BB))
				if (loop->getHeader() == BB) continue; //ignore loop-headers

			// If the "preceding" control-flow was uniform, but not guarded by
			// uniform masks (e.g. uniform branch inside varying loop), we still
			// have to use a phi instead of a select because we have disjunct
			// paths. The correct blending is performed automatically at the
			// next join.
			// TODO: I do not understand the part with the uniform masks anymore :D.
			// NOTE: If a block is entered uniformly, this is not sufficient
			//       because the preceding control-flow might have split into
			//       varying parts that only join in this block (which means we
			//       would have to blend). Thus, the predecessors also have to
			//       have uniform entries, by which we can deduce that these are
			//       really disjoint paths.
			// NOTE: Additionally, the block itself can even be VARYING and
			//       still have uniform, disjoint paths join, meaning its
			//       predecessors will not be linearized.
			// Thus, all we need is two predecessors which have uniform entries.
			// TODO: Is this really enough or do we have to make sure that the
			//       common dominator also has a uniform exit?
			// TODO: What about uniform predecessors with diverging, varying exits?
			const MaskGraphNode* node = maskGraph->findMaskNode(BB);
			assert (node);
			if (node->getNumPredecessors() > 1) {
				BasicBlock* pred0BB = node->getPredecessor(0)->getBlock();
				BasicBlock* pred1BB = node->getPredecessor(1)->getBlock();
				assert (pred0BB && pred1BB);

				if (analysisResults->hasUniformEntry(pred0BB) &&
						analysisResults->hasUniformEntry(pred1BB))
				{
					assert (node->getPredecessor(1)->getBlock());
					assert (analysisResults->hasUniformEntry(node->getPredecessor(1)->getBlock()));
					DEBUG_PKT( outs() << "      block with uniform predecessors"
							" found: " << BB->getName() << "\n"; );
					DEBUG_PKT( outs() << "      PHIs will not be replaced by selects!\n"; );
					continue;
				}
			}

			DEBUG_PKT( outs() << "\n  generating selects for block '" << BB->getNameStr() << "'...\n"; );
			for (BasicBlock::iterator I=BB->begin(); I!=BB->end(); ) {
				if (!isa<PHINode>(I)) break; //it is guaranteed that no phis can follow
				assert (!analysisResults->isMask(I) && "there is a mask phi where there should not be one");
				assert (node->getNumPredecessors() > 1 && "what is a phi doing in a block with only one predecessor?");

				changed |= generateSelectFromPhi(cast<PHINode>(I++));
			}
			DEBUG_PKT( outs() << "  select-generation for block '" << BB->getNameStr() << "' finished.\n"; );
		}
		return changed;
	}

	//replace 'phi' by select with entry-mask of parent-block as condition
	//REQUIRES: PhiCanonicalization
	//REQUIRES: masks
	inline bool generateSelectFromPhi(PHINode* phi) {
		assert (phi);
		DEBUG_PKT( outs() << "    generating selects for phi: " << *phi << "\n"; );

		//1st case: dummy phi needs no treatment
		if (phi->getNumIncomingValues() < 2) {
			DEBUG_PKT( outs() << "      phi has less than two incoming values, no select needed!\n"; );
			//TODO: possibly don't delete, instead generate select for incoming value?
			if (phi->getNumIncomingValues() == 1) phi->replaceAllUsesWith(phi->getIncomingValue(0));
			// remove from info map before erasing
			analysisResults->removeValueInfo(phi);
			loopLiveValueAnalysis->removeLiveValue(phi);
			phi->eraseFromParent();

			DEBUG_PKT( outs() << "    select-generation for phi finished.\n"; );
			return false;
		}

		BasicBlock* block = phi->getParent();
		assert (block);

		//2nd case: phi with 2 incoming values
		if (phi->getNumIncomingValues() == 2) {
			Value* valI = phi->getIncomingValue(0);
			Value* valJ = phi->getIncomingValue(1);
			BasicBlock* predI = phi->getIncomingBlock(0);
			BasicBlock* predJ = phi->getIncomingBlock(1);
			assert (valI && valJ && predI && predJ);

			//condition = exit mask of first incoming value (selected at '1')
			// NOTE: We have to make sure that the mask is really the one
			//       of the edge to the current block instead of blindly using
			//       getExitMaskTrue()!
			// TODO: Would it make sense to use either the left or the right
			//       mask for some reason? -> Would require some heuristic.
			Value* cond = maskGraph->getExitMaskInDir(predI, block);
			assert (cond && "condition must not be NULL!");

			// If the active mask in this block is uniform, the phi should not
			// be deleted.
			// NOTE: It is crucial for this to work that the CFG linearization pass
			//       also works accordingly and does not remove branches with
			//       uniform conditions.
			// NOTE: This behaviour currently implies that we ALWAYS retain
			//       uniform control flow. We expect that there are no cases
			//       where it would be beneficial to flatten it.
			//       If there are, LLVM optimizations should do the job as well ;).
			// NOTE: We have to test the exit masks of the predecessor block,
			//       not the entry mask of the current block: The block is
			//       allowed to be VARYING while the incoming masks can be UNIFORM.
			// NOTE: Using hasUniformExit() fails here due to blocks with
			//       unconditional branches always returning true.
			if (analysisResults->isUniform(cond)) {
				DEBUG_PKT( outs() << "      predecessor block has uniform exit mask: " << predI->getName() << "\n"; );
				DEBUG_PKT( outs() << "      corresponding exit mask: " << *cond << "\n"; );
				DEBUG_PKT( outs() << "      PHI will not be replaced by select!\n"; );
				assert (analysisResults->isUniform(maskGraph->getExitMaskInDir(predI, block)) &&
						"exit mask of block with UNIFORM exit has to be UNIFORM!");
				assert (analysisResults->hasUniformExit(phi->getIncomingBlock(1)) &&
						"exit masks of predecessors with differing UNIFORM/VARYING information found!");
				assert (analysisResults->isUniform(maskGraph->getExitMaskInDir(phi->getIncomingBlock(1), block)) &&
						"incoming masks with differing UNIFORM/VARYING information found!");
				return false;
			}

			DEBUG_PKT( outs() << "      mask: " << *cond << "\n"; );

			SelectInst* sel = NULL;
			//if the mask is an instruction in the same block, we have to insert the select BEHIND it!
			//TODO: is there a better (i.e. more efficient) way to do this?
			if (isa<Instruction>(cond) && cast<Instruction>(cond)->getParent() == block) {
				Instruction* condInstr = cast<Instruction>(cond);
				sel = SelectInst::Create(cond, valI, valJ, "psi", condInstr);
				condInstr->moveBefore(sel); //select was inserted *before* mask, so move mask back...
			} else {
				sel = SelectInst::Create(cond, valI, valJ, "psi", block->getFirstNonPHI());
			}

			assert (sel);

			selectInfoMap.insert(std::make_pair(sel, new SelectInfo(valI, valJ, predI, predI, predJ)));

			// store uniform info (should actually always be VARYING)
			std::set<Value*> tmp;
			analysisResults->deriveUniformInfo(sel, tmp);
			const AnalysisResults::ValueInfo* vi = analysisResults->getValueInfo(phi);
			assert (vi);
			analysisResults->setIndexInfo(sel, vi->indexInfo);
			analysisResults->setAlignmentInfo(sel, vi->alignmentInfo);
			analysisResults->setSplitInfo(sel, vi->splitInfo);
			DEBUG_PKT( outs() << "New select is marked as "
					<< AnalysisResults::getUniformInfoString(analysisResults->getValueInfo(sel)->uniformInfo)
					<< " / " << AnalysisResults::getIndexInfoString(vi->indexInfo)
					<< " / " << AnalysisResults::getAlignmentInfoString(vi->alignmentInfo)
					<< " / " << AnalysisResults::getSplitInfoString(vi->splitInfo)
					<< " (generateSelectFromPhi)\n"; );

			// If the value is a pointer, mark condition als SPLIT_RESULT
			// (varying mask). This is required because the select will be split
			// up later.
			if (sel->getType()->isPointerTy()) {
				analysisResults->setSplitInfo(cond, AnalysisResults::SPLIT_RESULT);
			}

			phi->replaceAllUsesWith(sel);
			// remove from info map before erasing
			analysisResults->removeValueInfo(phi);
			// remove from live value map and insert new select
			loopLiveValueAnalysis->updateLiveValue(phi, sel);
			phi->eraseFromParent();
			DEBUG_PKT( outs() << "    generated select: " << *sel << "\n"; );
			DEBUG_PKT( outs() << "    select-generation for phi finished.\n"; );
			++SelectCounter;
			return true;
		} else {
			//3rd case: phi with more than 2 incoming values
			assert (!"there should be no phis left with more than two incoming values! (phi canonicalization pass not executed?)");
		}

		DEBUG_PKT( outs() << "    select-generation for phi finished.\n"; );
		return false;
	}

	//if we are in a loop, make sure that we use a mask that tells us which instances came from which direction,
	//NOT which instances exitted the loop
	//to this end, walk up cfg in direction of the mask up to the definition of valI, and
	//check if there are exits on the path. for each found exit, replace the current mask by an "OR" with that exit mask
	// NOTE: currently not used
	inline void createLoopCFGDirectionMask(PHINode* phi, const unsigned dir, Value* mask) {
		Loop* loop = loopInfo->getLoopFor(phi->getParent());
		if (!loop) return;

		BasicBlock* predBB = phi->getIncomingBlock(dir);

		//SmallVectorImpl<BasicBlock*> exitingBlocks(loop->getBlocks().size()); //DON'T!
		SmallVector<BasicBlock*, 4>* exitingBlocks = new SmallVector<BasicBlock*, 4>();
		loop->getExitingBlocks(*exitingBlocks);

		for (SmallVector<BasicBlock*, 4>::iterator it=exitingBlocks->begin(); it!=exitingBlocks->end(); ++it) {
			BasicBlock* curExitBB = *it;
			if (curExitBB == phi->getParent()) continue; //ignore current block
			if (!Packetizer::isDominatedBy(predBB, curExitBB, *loopInfo)) continue; //ignore blocks that come after phi

			//find out exit edge
			assert (isa<BranchInst>(curExitBB->getTerminator()) && cast<BranchInst>(curExitBB->getTerminator())->isConditional());
			BranchInst* br = cast<BranchInst>(curExitBB->getTerminator());
			BasicBlock* exitEdge = loop->contains(br->getSuccessor(1)) ?
				br->getSuccessor(2) : br->getSuccessor(1);

			//get exit mask of this block
			Value* exitMask = maskGraph->getExitMaskInDir(curExitBB, exitEdge);
			assert (exitMask && "exit mask must not be NULL!");

			//get exit mask of this iteration, not the one that combines all exitted values
			assert (isa<Instruction>(exitMask));
			assert (cast<Instruction>(exitMask)->isBinaryOp(Instruction::Or));
			assert (isa<PHINode>(cast<Instruction>(exitMask)->getOperand(1)));
			assert (isExitMaskPhi(cast<PHINode>(cast<Instruction>(exitMask)->getOperand(1))));
			exitMask = cast<Instruction>(exitMask)->getOperand(0);
			assert (exitMask);

			//replace current mask by an or with the exit mask
			mask = createMaskOr(mask, exitMask, "", phi);
		}
		delete exitingBlocks;
	}


	//generate select statements at all loop-exits and backedges of 'f'
	//this function calls 'generateMultipleExitLoopSelects()' for each loop
	//REQUIRES: loopInfo
	bool generateLoopSelects(Function& f) {
		DEBUG_PKT( outs() << "\n  generating selects for loops of function " << f.getNameStr() << "... \n"; );

		if (loopInfo->empty()) {
			DEBUG_PKT( outs() << "    no loops found in function!\n"; );
			DEBUG_PKT( outs() << "  generation of loop-selects done!\n"; );
			return false;
		}

		bool changed = false;
		// Create loop selects for varying "top-level" loops.
		// Here, we can now stop recursion every time we see a uniform sub-loop
		// because possible varying loops nested deeper have been collected
		// before.
		for (AnalysisResults::varyingtoplevelloop_iterator
				L=analysisResults->varyingtoplevelloop_begin(),
				LE=analysisResults->varyingtoplevelloop_end(); L!=LE; ++L)
		{
			SmallVector<BasicBlock*, 4>* exitingBlocks = new SmallVector<BasicBlock*, 4>();
			(*L)->getExitingBlocks(*exitingBlocks);
			//print blocks
			DEBUG_PKT(
				outs() << "\nExiting blocks of top-level loop "; (*L)->print(outs(), (*L)->getLoopDepth());
				for (SmallVector<BasicBlock*, 4>::iterator it=exitingBlocks->begin(); it!=exitingBlocks->end(); ++it) {
					outs() << "  * " << (*it)->getNameStr() << "\n";
				}
			);
			changed |= generateMultipleExitLoopSelects(*L, exitingBlocks);
			delete exitingBlocks;
		}

		DEBUG_PKT( outs() << "\n  generation of loop-selects done!\n"; );
		return changed;
	}

	//version 6 - use nested result vectors, blend results of instances that left the loop in latch
	//REQUIRES: loopInfo
	//REQUIRES: loop simplification
	//REQUIRES: masks
	class LoopResults {
		private:
			const Loop* loop;
			std::map<const Instruction*, PHINode*> loopResultPhiMap;
			std::map<const Instruction*, SelectInst*> loopResultMap;
		public:
			LoopResults(const Loop* l) : loop(l) {}
			~LoopResults() {
				loopResultMap.clear();
				loopResultPhiMap.clear();
			}
			PHINode* getResultPhi(const Instruction* inst) const {
				assert (inst);
				std::map<const Instruction*, PHINode*>::const_iterator it = loopResultPhiMap.find(inst);
				if (it == loopResultPhiMap.end()) return NULL;
				return it->second;
			}
			SelectInst* getResult(const Instruction* inst) const {
				assert (inst);
				std::map<const Instruction*, SelectInst*>::const_iterator it = loopResultMap.find(inst);
				if (it == loopResultMap.end()) return NULL;
				return it->second;
			}
			void addResult(const Instruction* liveValue, PHINode* phi, SelectInst* update) {
				assert (liveValue && phi && update);
				assert (loop->contains(liveValue->getParent()));
				loopResultPhiMap.insert(std::make_pair(liveValue, phi));
				loopResultMap.insert(std::make_pair(liveValue, update));
			}
			void addResultPhi(const Instruction* liveValue, PHINode* phi) {
				assert (liveValue && phi);
				assert (loop->contains(liveValue->getParent()));
				loopResultPhiMap.insert(std::make_pair(liveValue, phi));
			}
			void addResultSelect(const Instruction* liveValue, SelectInst* update) {
				assert (liveValue && update);
				assert (loop->contains(liveValue->getParent()));
				loopResultMap.insert(std::make_pair(liveValue, update));
			}
			bool empty() const { return loopResultPhiMap.empty() && loopResultMap.empty(); }
			bool verify() const {
				const bool a = loop != NULL;
				const bool b = loopResultPhiMap.size() == loopResultMap.size();
				bool c = true;
				for (std::map<const Instruction*, PHINode*>::const_iterator it=loopResultPhiMap.begin(),
						E=loopResultPhiMap.end(); it!=E; ++it) {
					const Instruction* instA = it->first;
					c &= getResult(instA) != NULL;
				}
				bool d = true;
				for (std::map<const Instruction*, SelectInst*>::const_iterator it=loopResultMap.begin(),
						E=loopResultMap.end(); it!=E; ++it) {
					const Instruction* instA = it->first;
					d &= getResultPhi(instA) != NULL;
				}
				const bool res = a && b && c && d;
				assert(res && "could not verify LoopResult!");
				return res;
			}
	};

	typedef std::map<Loop*, LoopResults*> LoopResultMapType;
	LoopResultMapType loopResultMap;
	bool generateMultipleExitLoopSelects(Loop* loop, SmallVector<BasicBlock*, 4>* exitingBlocks) {
		assert (loop);
		// Ignore uniform loops.
		if (analysisResults->isUniform(loop)) {
			DEBUG_PKT ( outs() << "uniform loop ignored: " << *loop; );
			return false;
		}

		//first collect loop live values
		LoopLiveValueAnalysis::LiveValueSetType& liveValues =
				*loopLiveValueAnalysis->getLiveValueSet(loop);

		//if there are no live values (e.g. in a loop which only has side-effects), don't generate any selects
		if (liveValues.empty()) {
			DEBUG_PKT( errs() << "NOTE: loop has no result-values to generate selects for!\n"; );
			return false;
		}

		//we need information about the parent loops' result phis during recursion.
		//thus, generate them here already
		LoopResults* loopResults = new LoopResults(loop);
		Instruction* pos = loop->getHeader()->getFirstNonPHI();
		for (LoopLiveValueAnalysis::LiveValueSetType::const_iterator it=
				liveValues.begin(); it!=liveValues.end(); ++it)
		{
			PHINode* phi = PHINode::Create((*it)->getType(), 2U, "result.phi", pos);
			loopResults->addResultPhi(*it, phi);

			// We mark the phi as UNIFORM before completion:
			// After generating the operands, we derive the information again
			// and possibly overwrite it if VARYING is returned.
			// Other possibility: If it is actually used later-on, it has to be
			// VARYING.
//#define PKT_FIXME
#ifdef PKT_FIXME
			analysisResults->addValueInfo(phi,
				AnalysisResults::UNIFORM,
				AnalysisResults::INDEX_NOT_INITIALIZED,
				AnalysisResults::ALIGN_NOT_INITIALIZED,
				AnalysisResults::SPLIT_NEVER, // should not be split
				false); // mask = false
#else
			// If the loop is VARYING, the result phis also have to be VARYING.
			analysisResults->addValueInfo(phi,
				AnalysisResults::VARYING,
				AnalysisResults::INDEX_RANDOM,
				AnalysisResults::ALIGN_FALSE,
				AnalysisResults::SPLIT_NEVER, // should not be split
				false); // mask = false
#endif
		}
		loopResultMap.insert(std::make_pair(loop, loopResults));

		//now recursively generate selects for nested loops
		bool changed = false;
		for (Loop::iterator SL=loop->begin(), SLE=loop->end(); SL!=SLE; ++SL)  {
			SmallVector<BasicBlock*, 4>* exitingBlocksSL = new SmallVector<BasicBlock*, 4>();
			(*SL)->getExitingBlocks(*exitingBlocksSL);
			DEBUG_PKT(
				outs() << "\nExiting blocks of nested loop "; (*SL)->print(outs(), (*SL)->getLoopDepth());
				for (SmallVectorImpl<BasicBlock*>::iterator it=exitingBlocksSL->begin(); it!=exitingBlocksSL->end(); ++it) {
					outs() << "  * " << (*it)->getNameStr() << "\n";
				}
			);
			changed |= generateMultipleExitLoopSelects(*SL, exitingBlocksSL);
			delete exitingBlocksSL;
		}

		DEBUG_PKT( outs() << "\ngenerating selects for multiple-exit-loop: "; loop->print(outs(), loop->getLoopDepth()); );

		BasicBlock* latchBB = loop->getLoopLatch();
		assert (latchBB && "loop-latch has to exist! (loop simplification pass missing?)");


		// generate combined exit mask (required for blending in latch)
		// TODO: move to own function
		DEBUG_PKT( outs() << "  building combined exit mask...\n"; );
		Value* combinedExitMask = NULL;
		for (SmallVector<BasicBlock*, 4>::iterator it=exitingBlocks->begin(); it!=exitingBlocks->end(); ++it) {
			BasicBlock* curExitBB = *it;
			DEBUG_PKT( outs() << "  combining with exit mask of block '" << curExitBB->getNameStr() << "'\n"; );

			//find out mask of loop exit edge
			assert (isa<BranchInst>(curExitBB->getTerminator()) && cast<BranchInst>(curExitBB->getTerminator())->isConditional());
			BranchInst* br = cast<BranchInst>(curExitBB->getTerminator());
			BasicBlock* exitEdge = loop->contains(br->getSuccessor(1)) ?
				br->getSuccessor(2) : br->getSuccessor(1);

			Value* loopExitMask = maskGraph->getExitMaskInDir(curExitBB, exitEdge);
			assert (loopExitMask && "condition must not be NULL!");

			//now we have the blended loop exit mask
			//(where ALL instances that left the loop over this exit in any previous iteration are 'true')
			//This mask is only desired for blending if the exit leaves from a nested loop (return/break n)
			//where all iterations of this inner loop correspond to the current iteration of the parent loop.
			//For exits leaving the current loop we only want those instances that left the loop in the CURRENT iteration.
			//summarized:
			// - for exits leaving from a nested loop:
			//    - use accumulated loop exit mask
			// - for exits leaving from the current loop:
			//    - use corresponding exit condition
			//    - by construction, the required mask is the left hand operand of the 'or' (rho is the exit mask phi)
			//NOTE:
			//This is not entirely correct:
			//Now we use the complete loop exit mask of this exit which includes instances that left in other iterations
			//than the current one. However, this does not introduce wrong values because we only blend with the result
			//value of the next nested loop (which can only hold a valid result in this case).
			if (maskGraph->isInnermostLoopOfExit(curExitBB, loop)) {
				DEBUG_PKT( outs() << "  loop is innermost loop of current exit, using exit condition as input!\n"; );
				assert (isa<Instruction>(loopExitMask));
				assert (cast<Instruction>(loopExitMask)->isBinaryOp(Instruction::Or));
				assert (isa<PHINode>(cast<Instruction>(loopExitMask)->getOperand(1)));
				assert (isExitMaskPhi(cast<PHINode>(cast<Instruction>(loopExitMask)->getOperand(1))));
				loopExitMask = cast<Instruction>(loopExitMask)->getOperand(0);
				assert (loopExitMask);
			}
			DEBUG_PKT( outs() << "  input mask: " << *loopExitMask << "\n"; );

			//combine mask
			if (!combinedExitMask) combinedExitMask = loopExitMask;
			else combinedExitMask = createMaskOr(combinedExitMask, loopExitMask, "", latchBB->getTerminator());
		}
		assert (combinedExitMask);
		DEBUG_PKT( outs() << "  combined exit mask: " << *combinedExitMask << "\n"; );



		DEBUG_PKT( outs() << "generating selects...\n"; );
		for (LoopLiveValueAnalysis::LiveValueSetType::iterator it=liveValues.begin(); it!=liveValues.end(); ++it) {
			Instruction* liveValue = *it;
			DEBUG_PKT( outs() << "  generating select for live value: " << *liveValue << "\n"; );

			// Make sure we do not blend result vectors more than once.
			if (SelectInst* sel = dyn_cast<SelectInst>(liveValue)) {
				if (std::strstr(sel->getNameStr().c_str(), "result.vec") != 0) continue;
			}

#ifdef PKT_FIXME
			// Make sure we do not blend result vectors of uniform values
			assert (!analysisResults->isUniform(liveValue) &&
					"must not attempt to generate select for uniform live value!");
#endif

			//condition = combined exit mask ('true' iff an instance left over any exit in the current iteration)
			//trueval = keep old result (resultPhi)
			//falseval = blend new result (instance left loop -> store result) (liveValue)
			Value* mask = combinedExitMask;
			DEBUG_PKT( outs() << "    blend mask: " << *mask << "\n"; );

			//generate 'result-vector' for live value,
			//consisting of phi in loop-header and select in latch (to only blend once for all exits)

			//get result phi
			PHINode* resultPhi = loopResults->getResultPhi(liveValue);
			assert (resultPhi);

			assert (mask && resultPhi && liveValue && "required values for loop select generation must not be NULL!");

			//generate select
			SelectInst* resultSelect = SelectInst::Create(mask, liveValue, resultPhi, "result.vec", latchBB->getTerminator());
			DEBUG_PKT( outs() << "    select generated: " << *resultSelect << "\n"; );
			++SelectCounter;
			// NOTE: We do not add loop selects to the selectInfoMap.

			// store uniform info (should actually always be VARYING)
			// not necessarily - loopns7 had a case with a weird innermost loop
			// that reused the induction variable. This resulted in a select
			// being generated which could be left uniform and broadcasted before
			// each varying comparison operation.
			// This however leads to the conclusion that we should not create
			// select operations for uniform values in the first place.
			std::set<Value*> tmp;
			analysisResults->deriveUniformInfo(resultSelect, tmp);
			const AnalysisResults::ValueInfo* vi = analysisResults->getValueInfo(liveValue);
			assert (vi);
			analysisResults->setIndexInfo(resultSelect, vi->indexInfo);
			analysisResults->setAlignmentInfo(resultSelect, vi->alignmentInfo);
			analysisResults->setSplitInfo(resultSelect, vi->splitInfo);
			DEBUG_PKT( outs() << "New select is marked as "
					<< AnalysisResults::getUniformInfoString(vi->uniformInfo)
					<< " / " << AnalysisResults::getIndexInfoString(vi->indexInfo)
					<< " / " << AnalysisResults::getAlignmentInfoString(vi->alignmentInfo)
					<< " / " << AnalysisResults::getSplitInfoString(vi->splitInfo)
					<< " (generateMultipleExitLoopSelects)\n"; );

			// If the value is a pointer, mark condition als SPLIT_RESULT
			// (varying mask). This is required because the select will be split
			// up later.
			if (resultSelect->getType()->isPointerTy()) {
				analysisResults->setSplitInfo(mask, AnalysisResults::SPLIT_RESULT);
			}

			loopResults->addResultSelect(liveValue, resultSelect);

			if (analysisResults->isVaryingTopLevelLoop(loop)) {
				resultPhi->addIncoming(UndefValue::get(liveValue->getType()), loop->getLoopPreheader());
			} else {
				//use result phi of this live value from parent loop
				assert (loopResultMap.find(loop->getParentLoop()) != loopResultMap.end()); //TODO: could happen
				LoopResults* parentLoopResults = loopResultMap.find(loop->getParentLoop())->second;
				assert (!parentLoopResults->empty()); //TODO: could happen

				//if this live value is not live in the parent loop, use UndefValue
				PHINode* parentResultPhi = parentLoopResults->getResultPhi(liveValue);
				if (parentResultPhi) {
					resultPhi->addIncoming(parentResultPhi, loop->getLoopPreheader());
				} else {
					resultPhi->addIncoming(UndefValue::get(liveValue->getType()), loop->getLoopPreheader());
				}
			}
			resultPhi->addIncoming(resultSelect, latchBB);
			DEBUG_PKT( outs() << "    result phi generated: " << *resultPhi << "\n"; );

			// update uniform info if required
			analysisResults->setUniformInfo(resultPhi,
				analysisResults->joinUniformInfo(resultPhi->getIncomingValue(0), resultSelect));
			assert (analysisResults->getValueInfo(resultPhi));
			DEBUG_PKT( outs() << "    result phi uniform info is updated to "
					<< AnalysisResults::getUniformInfoString(analysisResults->getValueInfo(resultPhi)->uniformInfo)
					<< " (generateMultipleExitLoopSelects)\n"; );

			//replace live value operand of select with corresponding select of next nested loop
			//in "nesting direction" of the live value's loop (if existing)
			const bool liveValueDefinedInNestedLoop =
				loopInfo->getLoopFor(liveValue->getParent()) != loop;
			//const bool liveValueDefinedInVaryingNonTopLevelLoop =
				//liveValueDefinedInNestedLoop &&
				//!vectorizationAnalysis->isVaryingTopLevelLoop(loop);

			if (liveValueDefinedInNestedLoop) {
				Loop* nextNestedLoop = findNestedLoopOfInst(loop, liveValue);
				assert (!maskGraph->hasNestedLoop(loop) || nextNestedLoop);
				//assert (loopResultMap.find(nextNestedLoop) != loopResultMap.end()); //TODO: could happen
				Value* nestedVal = NULL;
				if (loopResultMap.find(nextNestedLoop) == loopResultMap.end()) {
					// If we do not have any info on this nested loop stored,
					// it must be a uniform one.
					assert (analysisResults->isUniform(nextNestedLoop));
					// Set live value operand to the live value itself (there
					// are no blending operations inside a uniform loop).
					nestedVal = liveValue;
				} else {
					// Otherwise, get the result select from the nested loop.
					LoopResults* nestedLoopResults = loopResultMap.find(nextNestedLoop)->second;
					assert (!nestedLoopResults->empty()); //TODO: could happen
					SelectInst* nestedSelect = nestedLoopResults->getResult(liveValue);
					nestedVal = nestedSelect;
				}
				assert (nestedVal);
				resultSelect->setOperand(1, nestedVal);
			}

			//replace all uses of live value that are on the next outer nesting level
			replaceDirectParentLoopUsesOfWith(liveValue, resultSelect, loop);

			//if varying "top level" loop, call replaceNonLoopUsesOfWith()
			if (analysisResults->isVaryingTopLevelLoop(loop)) {
				replaceNonLoopUsesOfWith(liveValue, resultSelect, loop);
			}
		}
		DEBUG_PKT( outs() << "generation of selects finished for multiple-exit loop: "; loop->print(outs(), loop->getLoopDepth()); );

		return true;
	}

	inline void replaceDirectParentLoopUsesOfWith(Value* oldValue, Value* newValue, Loop* loop) {
		Loop* parentLoop = loop->getParentLoop();
		if (!parentLoop) return;
		for (Value::use_iterator U=oldValue->use_begin(), UE=oldValue->use_end(); U!=UE; ) {
			Value* useVal = *U++;
			if (newValue == useVal) continue; //don't replace use in new value (also prevented by loop contains check)
			assert (isa<Instruction>(useVal) && "all uses have to be instructions (!?)");
			Instruction* useI = cast<Instruction>(useVal);
			if (loop->contains(useI->getParent())) continue;
			if (!parentLoop->contains(useI->getParent())) continue;
			useI->replaceUsesOfWith(oldValue, newValue);
		}
	}
	inline void replaceNonLoopUsesOfWith(Value* oldValue, Value* newValue, Loop* loop) {
		assert (oldValue && newValue && loop);
		Loop* parentLoop = loop->getParentLoop();
		for (Value::use_iterator U=oldValue->use_begin(), UE=oldValue->use_end(); U!=UE; ) {
			Value* useVal = *U++;
			if (newValue == useVal) continue; //don't replace use in new value (also prevented by loop contains check)
			assert (isa<Instruction>(useVal) && "all uses have to be instructions (!?)");
			Instruction* useI = cast<Instruction>(useVal);
			//if the use is inside loop itself or some parent loop, ignore it
			if (loop->contains(useI->getParent())) continue;
			if (parentLoop && isContainedInSomeParentLoop(useI->getParent(), parentLoop)) continue;
			useI->replaceUsesOfWith(oldValue, newValue);
		}
	}
	//not used, weird effect anyway
	inline void replaceOutsideLoopUsesOfWith(Value* oldValue, Value* newValue, Loop* loop) {
		for (Value::use_iterator U=oldValue->use_begin(), UE=oldValue->use_end(); U!=UE; ) {
			Value* useVal = *U++;
			assert (isa<Instruction>(useVal) && "all uses have to be instructions (!?)");
			if (newValue == useVal) continue; //don't replace use in new value (also prevented by loop contains check)
			Instruction* useI = cast<Instruction>(useVal);
			if (loop->contains(useI->getParent())) continue;
			//make sure only one result vector per live value is used outside loop:
			//the one of the loop with the lowest depth unequal to the uses loop depth
			//do not replace use if there is another loop around the current one
			//if (loop->getLoopDepth() - loopInfo->getLoopDepth(useI->getParent()) > 1) continue;
			useI->replaceUsesOfWith(oldValue, newValue);
		}
	}
	Loop* getTopLevelLoop(Loop* loop) {
		assert (loop);
		Loop* parentLoop = loop->getParentLoop();
		return parentLoop ? getTopLevelLoop(parentLoop) : loop;
	}
	bool isContainedInSomeParentLoop(BasicBlock* block, Loop* loop) {
		assert (block && loop);
		if (loop->contains(block)) return true;
		Loop* parentLoop = loop->getParentLoop();
		return parentLoop ? isContainedInSomeParentLoop(block, parentLoop) : false;
	}
	Loop* findNestedLoopOfInst(Loop* parentLoop, Instruction* inst) {
		assert (parentLoop && inst);
		BasicBlock* block = inst->getParent();
		//            Loop* loop = loopInfo->getLoopFor(block);
		//            assert (loop);
		//            Loop* nestedLoop = loop;
		//            while (loop = loop->getParentLoop()) {
		//                if (loop == parentLoop) return nestedLoop;
		//            }
		//            assert (!"instruction is not defined in nested loop!");
		//            return NULL;

		//alternative implementation:
		for (Loop::iterator SL=parentLoop->begin(), SLE=parentLoop->end(); SL!=SLE; ++SL) {
			if ((*SL)->contains(block)) return *SL;
		}
		//assert (!"instruction is not defined in nested loop!"); //has to be allowed
		return NULL;
	}


	//TODO: replace substring-matching...
	inline bool isExitMaskPhi(PHINode* phi) {
		return (std::strstr(phi->getNameStr().c_str(), "loop.exit.mask") != 0);
	}

	// TODO: This is not the right place for this!
	// TODO: move to utils?
	Value* createMaskOr(Value* mask1, Value* mask2, const std::string& name, Instruction* insertBefore) {
		assert (mask1->getType()->isIntegerTy(1) && "trying to create bit-operation on non-boolean type!");
		assert (mask2->getType()->isIntegerTy(1) && "trying to create bit-operation on non-boolean type!");
		if (mask1 == boolOneConst) return boolOneConst;
		if (mask2 == boolOneConst) return boolOneConst;
		if (mask1 == boolZeroConst && mask2 == boolZeroConst) return boolZeroConst;
		if (mask1 == boolZeroConst) return mask1;
		if (mask2 == boolZeroConst) return mask2;
		if (mask1 == mask2) return mask1;
		++NewMaskCounter;
		Value* mask = BinaryOperator::Create(Instruction::Or, mask1, mask2, name, insertBefore);

		std::set<Value*> tmp;
		const AnalysisResults::UniformInfo& ui = analysisResults->deriveUniformInfo(mask, tmp);
		const AnalysisResults::IndexInfo& ii =
			ui == AnalysisResults::UNIFORM ?
				AnalysisResults::INDEX_SAME :
				AnalysisResults::INDEX_RANDOM;
		analysisResults->setIsMask(mask, true);
		analysisResults->setIndexInfo(mask, ii);
		analysisResults->setAlignmentInfo(mask, AnalysisResults::ALIGN_FALSE);
		const AnalysisResults::SplitInfo& si = analysisResults->deriveMaskSplitInfo(mask);
		analysisResults->setSplitInfo(mask, si);
		DEBUG_PKT( outs() << "New mask is marked as "
				<< AnalysisResults::getUniformInfoString(ui)
				<< AnalysisResults::getIndexInfoString(ii)
				<< " / ALIGN_FALSE / "
				<< AnalysisResults::getSplitInfoString(si)
				<< " (createMaskOr)\n"; );

		return mask;
	}

};

} // unnamed namespace


char SelectGenerator::ID = 0;
INITIALIZE_PASS_BEGIN(SelectGenerator, "select generator", "Select Generator", false, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_DEPENDENCY(LoopLiveValueAnalysis)
INITIALIZE_PASS_DEPENDENCY(VectorizationAnalysis)
INITIALIZE_PASS_DEPENDENCY(MaskGenerator)
INITIALIZE_PASS_END(SelectGenerator, "select generator", "Select Generator", false, false)

// Public interface to the SelectGenerator pass
namespace llvm {

FunctionPass* createSelectGeneratorPass(bool* failed, const bool verbose=false) {
	return new SelectGenerator(failed, verbose);
}

}



#endif	/* _SELECTGENERATOR_HPP */

