/**
 * @file   coherentMaskBranchGenerator.hpp
 * @date   30.09.2009
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2008, 2009, 2010, 2011 Saarland University
 *
 * This pass inserts dynamic "branches-on-superword-condition-codes"
 * (BOSCCs) after the function has been linearized.
 *
 */
#ifndef _COHERENTMASKBRANCHGENERATOR_HPP
#define	_COHERENTMASKBRANCHGENERATOR_HPP

#ifdef DEBUG_TYPE
#undef DEBUG_TYPE
#endif
#define DEBUG_TYPE "coherentmaskbranchgenerator"

#include "llvm/Support/raw_ostream.h"

#include <llvm/Pass.h>
#include <llvm/Function.h>
#include <llvm/Module.h>

#include "llvm/ADT/Statistic.h" //STATISTIC

// the following includes are only required for single-file compilation
#include "branchInfoAnalysis.hpp"
#include "vectorizationAnalysis.hpp"
#include "utils/analysisResults.hpp"
#include "maskGenerator.hpp"
#include "selectGenerator.hpp"
#include "utils/maskGraph.hpp"
#include "packetizerConfig.hpp"

// forward declaration of initializer
namespace llvm {
	void initializeCoherentMaskBranchGeneratorPass(PassRegistry&);
}

using namespace llvm;

STATISTIC(EdgeCounter, "Counts number of CFG edges that were inserted");
STATISTIC(PhiCounter, "Counts number of phis that were created");


namespace {

class CoherentMaskBranchGenerator : public FunctionPass {
public:
	static char ID; // Pass identification, replacement for typeid
	CoherentMaskBranchGenerator(bool* failed_flag=NULL, const bool verbose_flag = false)
			: FunctionPass(ID), failed(failed_flag), verbose(verbose_flag)
	{
		initializeCoherentMaskBranchGeneratorPass(*PassRegistry::getPassRegistry());
	}

	~CoherentMaskBranchGenerator() {}

	virtual void releaseMemory() {}

	virtual void getAnalysisUsage(AnalysisUsage &AU) const {
		AU.addRequired<LoopInfo>();
		//AU.addRequired<BranchInfoAnalysis>();     // dummy
		//AU.addRequired<LoopLiveValueAnalysis>();  // dummy
		AU.addRequired<VectorizationAnalysis>();
		AU.addRequired<MaskGenerator>();
		AU.addRequired<SelectGenerator>();
		//AU.addRequired<CFGLinearizerNaive>();     // dummy

		//AU.addPreserved<DominatorTree>();         // not preserved
		//AU.addPreserved<PostDominatorTree>();     // not preserved
		//AU.addPreserved<LoopInfo>();              // not preserved
		//AU.addPreserved<BranchInfoAnalysis>();    // not preserved
		AU.addPreserved<LoopLiveValueAnalysis>(); // not preserved
		AU.addPreserved<VectorizationAnalysis>();
		AU.addPreserved<MaskGenerator>();
		AU.addPreserved<SelectGenerator>();
		AU.addPreserved<CFGLinearizerNaive>();
	}

	virtual bool runOnFunction(Function &f) {
		EdgeCounter = 0;
		PhiCounter = 0;

		if (failed && *failed) return true;

		loopInfo              = &getAnalysis<LoopInfo>(); // get recent info (from after linearization)
		analysisResults       = getAnalysis<VectorizationAnalysis>().getAnalysisResults();
		maskGraph             = getAnalysis<MaskGenerator>().getMaskGraph();
		selectGenerator       = &getAnalysis<SelectGenerator>();
		branchMap             = getAnalysis<VectorizationAnalysis>().getBranchMap(); // get original info (from before linearization)

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "generating branches around disjunct paths for coherent masks..."; );
		if (branchMap->empty()) {
			DEBUG_PKT( outs() << "done.\nno conditional branches found!\n"; );
			DEBUG_PKT( outs() << "coherent mask branch generation finished!\n"; );
			DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );
			return false;
		}
		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		try {
			generateMaskBranches();
		}
		catch (std::logic_error& error) {
			errs() << "\nException occurred during coherent mask branch generation: "
					<< error.what() << "\n";
			if (failed) *failed = true;
			return true;
		}
		catch (...) {
			errs() << "\nINTERNAL ERROR: Unexpected exception occurred during "
					<< "coherent mask branch generation!\n";
			if (failed) *failed = true;
			return true;
		}

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "coherent mask branch generation finished!\n"; );
		DEBUG_PKT( print(outs(), NULL); );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );
		return true;
	}

	void print(raw_ostream &o, const Module *M=NULL) const {
		o << "Edges inserted: " << EdgeCounter << "\n";
		o << "New phi-functions inserted: " << PhiCounter << "\n";
	}

	static const unsigned MAX_COST_DIFFERENCE_MULTIPLIER = 2;
	static const unsigned MAX_COST_THRESHOLD = 30;
	static const unsigned FIXED_LOOP_COST = 30;

private:
	bool* failed;
	const bool verbose;
	LoopInfo* loopInfo;
	AnalysisResults* analysisResults;
	MaskGraph* maskGraph;
	SelectGenerator* selectGenerator;
	BranchInfoAnalysis::BranchMapType* branchMap;

	typedef std::map<const BasicBlock*, const unsigned> CostMapType;
	CostMapType costMap;

	typedef std::vector<std::pair<SelectInst*, Instruction*> > SelectMoveVecType;
	SelectMoveVecType selectMoveVec;

	void generateMaskBranches() {

		for (BranchInfoAnalysis::BranchMapType::iterator it=branchMap->begin(), E=branchMap->end(); it!=E; ++it) {
			DEBUG_PKT( outs() << "  generating branches around disjunct paths for conditional: "; it->second->print(outs()); );
			BasicBlock* source = it->first;
			BasicBlock* joinBlock = it->second->joinBlock;
			BasicBlock* successorTrue = it->second->successorTrue;
			BasicBlock* successorFalse = it->second->successorFalse;
			BasicBlock* truePathEndBlock = it->second->truePathEndBlock;
			BasicBlock* falsePathEndBlock = it->second->falsePathEndBlock;

			assert (source);
			assert (successorTrue);
			assert (truePathEndBlock);
			assert (successorFalse);
			assert (falsePathEndBlock);
			assert (joinBlock);

			if (analysisResults->hasUniformExit(source)) {
				DEBUG_PKT( outs() << "    source has uniform exit - branch"
						<< " not required!\n"; );
				continue;
			}

			// Calculate costs and collect all blocks of the paths
			CostMapType accumulatedCostMapTrue;
			CostMapType accumulatedCostMapFalse;
			const unsigned costTrue = calculateCost(successorTrue, truePathEndBlock, accumulatedCostMapTrue);
			const unsigned costFalse = calculateCost(successorFalse, falsePathEndBlock, accumulatedCostMapFalse);

			const bool createBranches = evaluateCostFunction(costTrue, costFalse);

			if (!createBranches) {
				DEBUG_PKT( outs() << "    disjunct paths not costly enough"
						<< " to justify creation of mask branches!\n"; );
				continue;
			}

			//determine which path comes first
			unsigned firstPath = 0;
			bool found = false;
			succ_iterator S = succ_begin(source);
			while (!found) {
				assert (S != succ_end(source));
				BasicBlock* succ = *S;
				if (succ == successorTrue) {
					found = true;
					firstPath = 1;
				} else if (succ == successorFalse) {
					found = true;
					firstPath = 2;
				}
				++S;
			}
			assert (firstPath > 0);

			//get masks of source block
			assert (maskGraph->getExitMaskTrue(source));
			assert (maskGraph->getExitMaskFalse(source));
			Value* exitMaskTrue = maskGraph->getExitMaskTrue(source);
			Value* exitMaskFalse = maskGraph->getExitMaskFalse(source);
			assert (exitMaskTrue == maskGraph->getExitMaskInDir(source, successorTrue));
			assert (exitMaskFalse == maskGraph->getExitMaskInDir(source, successorFalse));

			//match exit masks to first and second path
			Value* firstCond = firstPath == 1 ? exitMaskTrue : exitMaskFalse;
			Value* secondCond = firstPath == 1 ? exitMaskFalse : exitMaskTrue;

			//match successor and end blocks to first and second path
			BasicBlock* firstBlock = firstPath == 1 ? successorTrue : successorFalse;
			BasicBlock* secondBlock = firstPath == 1 ? successorFalse : successorTrue;
			DEBUG_PKT( outs() << "    firstBlock: " << firstBlock->getNameStr() << "\n"; );
			DEBUG_PKT( outs() << "    secondBlock: " << secondBlock->getNameStr() << "\n"; );

			BasicBlock* firstPathEndBlock = firstPath == 1 ? truePathEndBlock : falsePathEndBlock;
			BasicBlock* secondPathEndBlock = firstPath == 1 ? falsePathEndBlock : truePathEndBlock;
			DEBUG_PKT( outs() << "    firstPathEndBlock: " << firstPathEndBlock->getNameStr() << "\n"; );
			DEBUG_PKT( outs() << "    secondPathEndBlock: " << secondPathEndBlock->getNameStr() << "\n"; );

			//delete old edges
			analysisResults->removeValueInfo(source->getTerminator());
			analysisResults->removeValueInfo(firstPathEndBlock->getTerminator());
			source->getTerminator()->eraseFromParent();
			firstPathEndBlock->getTerminator()->eraseFromParent();


			//now insert additional edges

			//1. branch over first path from source
			BranchInst* br = BranchInst::Create(firstBlock, secondBlock, firstCond, source);
			analysisResults->addValueInfo(br,
											 AnalysisResults::UNIFORM,
											 AnalysisResults::INDEX_SAME,
											 AnalysisResults::ALIGN_FALSE,
											 AnalysisResults::SPLIT_NEVER, // should not be split
											 false); // mask = false

			//2. branch over second path from end of first path
			BranchInst* br2 = BranchInst::Create(secondBlock, joinBlock, secondCond, firstPathEndBlock);
			analysisResults->addValueInfo(br2,
											 AnalysisResults::UNIFORM,
											 AnalysisResults::INDEX_SAME,
											 AnalysisResults::ALIGN_FALSE,
											 AnalysisResults::SPLIT_NEVER, // should not be split
											 false); // mask = false

			EdgeCounter += 2;


			// Insert phis for live values at beginning of second path and
			// in the join block.

			for (BasicBlock::iterator I=joinBlock->begin(),
					IE=joinBlock->end(); I!=IE; )
			{
				Instruction*  inst = I++;
				if (!isa<SelectInst>(inst)) continue;
				if (!inst->getName().startswith("psi")) continue;

				insertLiveValuePhis(cast<SelectInst>(inst),
									source,
									firstPathEndBlock,
									secondBlock,
									secondPathEndBlock,
									joinBlock);
			}

#ifdef PACKETIZER_DEBUG_BOSCC_RUNTIME
			insertDebugBlocksOnCoherentBranches(source,
												firstPathEndBlock,
												secondBlock,
												secondPathEndBlock,
												joinBlock);
#endif

#ifndef PACKETIZER_SILENT_MODE
			outs() << "COHERENT-MASK-BRANCH AROUND DISJUNCT PATHS CREATED!\n";
#endif
		}

		// Now move the selects to the end of the corresponding second path.
		for (SelectMoveVecType::iterator it=selectMoveVec.begin(),
				E=selectMoveVec.end(); it!=E; ++it)
		{
			SelectInst* select = it->first;
			Instruction* moveBefore = it->second;
			select->moveBefore(moveBefore);
		}
	}

	void insertLiveValuePhis(SelectInst* select,
							 BasicBlock* source,
							 BasicBlock* firstPathEndBlock,
							 BasicBlock* secondBlock,
							 BasicBlock* secondPathEndBlock,
							 BasicBlock* joinBlock)
	{
		assert (select);

		Value* trueVal = select->getTrueValue();
		Value* falseVal = select->getFalseValue();

		DEBUG_PKT( outs() << "    creating phis for live value given by select: " << *select << "\n"; );
		DEBUG_PKT( outs() << "      true value: " << *trueVal << "\n"; );
		DEBUG_PKT( outs() << "      false value: " << *falseVal << "\n"; );

		const bool trueValIsInst = isa<Instruction>(trueVal);
		const bool falseValIsInst = isa<Instruction>(falseVal);

		// If both values are no instructions, we do not need to
		// move or generate anything, blending will do the job.
		if (!trueValIsInst && !falseValIsInst) {
			DEBUG_PKT( outs() << "      incoming values are no instructions - ignored!\n"; );
			return;
		}

		// Otherwise, move select and create phis

		// We cannot move the selects to the right locations before all
		// branches are created.
		selectMoveVec.push_back(std::make_pair(select, secondPathEndBlock->getTerminator()));

		// Determine which value comes from which path.

		// Initialize as if first path == 'true' dir
		// NOTE: We must not use the original true/false values of
		//       the select (stored in getIncomingValueTrue/False)
		//       because they might have been replaced by phis in a
		//       previous iteration of this loop.
		Value* firstPathValue = trueVal;
		Value* secondPathValue = falseVal;
		BasicBlock* truePathBlock = selectGenerator->getIncomingBlockTrue(select);

		DEBUG_PKT( outs() << "      true path block: " << truePathBlock->getName() << "\n"; );
		DEBUG_PKT( outs() << "      false path block: " << selectGenerator->getIncomingBlockFalse(select)->getName() << "\n"; );

		assert (truePathBlock == firstPathEndBlock || selectGenerator->getIncomingBlockFalse(select) == firstPathEndBlock);
		assert (truePathBlock == secondPathEndBlock || selectGenerator->getIncomingBlockFalse(select) == secondPathEndBlock);

		// swap values if first path == false path
		if (truePathBlock == secondPathEndBlock) {
			std::swap(firstPathValue, secondPathValue);
			DEBUG_PKT( outs() << "        paths swapped!\n"; );
		}

		DEBUG_PKT( outs() << "      first path value: " << *firstPathValue << "\n"; );
		DEBUG_PKT( outs() << "      second path value: " << *secondPathValue << "\n"; );

		Instruction* firstPathValI = dyn_cast<Instruction>(firstPathValue);

		// If the value from the first path is an instruction,
		// create a phi operation in first block of second path.
		if (firstPathValI) {
			// firstPathValue == trueValI
			PHINode* phi = PHINode::Create(select->getType(), 2U, "", secondBlock->getFirstNonPHI());
			//firstPathValI->replaceAllUsesWith(phi); // before adding firstPathValue as incoming value ;)
			//replaceDominatedUsesOfWith(firstPathValI, phi, phi);
			replaceUsesOfWithBelow(firstPathValI, phi, phi);
			phi->addIncoming(firstPathValI, firstPathEndBlock);
			phi->addIncoming(UndefValue::get(select->getType()), source);
			analysisResults->addValueInfo(phi, select);
			DEBUG_PKT( outs() << "      new phi: " << *phi << "\n"; );
			++PhiCounter;
		}

		// Create phi operation in join block with the select and
		// the original first-path-value as incoming values.
		PHINode* phi2 = PHINode::Create(select->getType(), 2U, "", joinBlock->getFirstNonPHI());
		select->replaceAllUsesWith(phi2); // before adding select as incoming value ;)
		phi2->addIncoming(select, secondPathEndBlock);
		phi2->addIncoming(firstPathValue, firstPathEndBlock);

		analysisResults->addValueInfo(phi2, select);
		DEBUG_PKT( outs() << "      new end-phi: " << *phi2 << "\n"; );
		++PhiCounter;
	}


#if 0
	// Replaces all uses of 'old' that are dominated by 'dom' with 'newVal'.
	// Currently not used
	void replaceDominatedUsesOfWith(Instruction* old, Value* newVal, Instruction* dom) {
		for (Instruction::use_iterator U=old->use_begin(), UE=old->use_end(); U!=UE; ++U) {
			assert (isa<Instruction>(*U));
			Instruction* useI = cast<Instruction>(*U);

			if (!domTree.dominates(dom, useI)) continue;

			useI->replaceUsesOfWith(old, newVal);
		}
	}
#endif

	// Replaces all uses of 'old' that are reachable (without taking loop-
	// backedges) from 'replaceBelow' with 'newVal'.
	void replaceUsesOfWithBelow(Instruction* old, Value* newVal, Instruction* replaceBelow) {
		DEBUG_PKT( outs() << "        replacing 'below'-uses of instruction: " << *old << "\n"; );
		DEBUG_PKT( outs() << "        with value: " << *newVal << "\n"; );
		DEBUG_PKT( outs() << "        below: " << *replaceBelow << "\n"; );

		for (Instruction::use_iterator U=old->use_begin(), UE=old->use_end(); U!=UE; ++U) {
			assert (isa<Instruction>(*U));
			Instruction* useI = cast<Instruction>(*U);

			if (!isReachableWithoutLoop(replaceBelow, useI)) continue;

			DEBUG_PKT( outs() << "          use was reachable without loop: " << *useI << "\n"; );
			useI->replaceUsesOfWith(old, newVal);
		}
	}

	bool isReachableWithoutLoop(const Instruction* start, const Instruction* target) const {
		assert (start && target);
		return isReachableWithoutLoop(start->getParent(), target->getParent());
	}

	bool isReachableWithoutLoop(const BasicBlock* block, const BasicBlock* targetBB) const {
		assert (block && targetBB);
		if (block == targetBB) return true;

		typedef GraphTraits<const BasicBlock*> BlockTraits;
		for (BlockTraits::ChildIteratorType SI = BlockTraits::child_begin(block),
				SE = BlockTraits::child_end(block); SI != SE; ++SI)
		{
			const BasicBlock* succBB = *SI;

			// Make sure we do not take loop-backedges
			const Loop* loop = loopInfo->getLoopFor(block);
			if (loop &&
					loop->getLoopLatch() == block &&
					loop->getHeader() == succBB)
			{
				continue;
			}

			if (isReachableWithoutLoop(succBB, targetBB)) return true;
		}

		return false;
	}


	//
	// Cost heuristic
	//


	// TODO: Try to calculate cost of loops as accurately as possible
	unsigned calculateCost(const BasicBlock* block, const BasicBlock* endBB, CostMapType& accumulatedCostMap) {
		assert (block && endBB);

		// calculate cost of this block
		const unsigned cost = calculateCost(block);

		// If we have already seen the block, return its accumulated cost.
		CostMapType::const_iterator it = accumulatedCostMap.find(block);
		if (it != accumulatedCostMap.end()) return it->second;

		// If this is the end block of the path, store its cost and return it.
		if (block == endBB) {
			accumulatedCostMap.insert(std::make_pair(block, cost));
			return cost;
		}

		// insert "dummy" to prevent infinite loops
		accumulatedCostMap.insert(std::make_pair(block, cost));

		// Otherwise, recurse into successors and return the max cost
		unsigned maxSuccCost = 0;

		typedef GraphTraits<const BasicBlock*> BlockTraits;
		for (BlockTraits::ChildIteratorType SI = BlockTraits::child_begin(block),
				SE = BlockTraits::child_end(block); SI != SE; ++SI)
		{
			const BasicBlock* succBB = *SI;

			unsigned succCost = calculateCost(succBB, endBB, accumulatedCostMap);

			if (loopInfo->isLoopHeader(const_cast<BasicBlock*>(succBB))) {
				// We found a loop header... what should we do with the costs? :P
				// TODO: Calculate cost of entire loop, try to find out
				//       iteration count if possible, otherwise multiply
				//       with empirically determined constant ;).
				succCost += FIXED_LOOP_COST;
			}

			maxSuccCost = succCost > maxSuccCost ? succCost : maxSuccCost;
		}

		// if the block has no successors and was not endBB, something went wrong
		assert (BlockTraits::child_begin(block) != BlockTraits::child_end(block));

		// update "dummy"
		assert (accumulatedCostMap.find(block) != accumulatedCostMap.end());
		accumulatedCostMap.erase(block);
		accumulatedCostMap.insert(std::make_pair(block, cost+maxSuccCost));

		return cost+maxSuccCost;
	}


	// TODO: calculate vector operations separately!
	unsigned calculateCost(const BasicBlock* block) {
		assert (block);

		CostMapType::const_iterator it = costMap.find(block);
		if (it != costMap.end()) return it->second;

		unsigned cost = 0;

		for (BasicBlock::const_iterator I=block->begin(), IE=block->end();
				I!=IE; ++I)
		{
			switch (I->getOpcode()) {
				// Terminators
				case Instruction::Ret:         cost += 0; break;
				case Instruction::Br:          cost += 1; break; // weight conditional branches?
				case Instruction::Switch:      cost += 4; break; // no idea
				case Instruction::Invoke:      cost += 15; break; // no idea
				case Instruction::Unwind:      cost += 0; break; // no idea
				case Instruction::Unreachable: cost += 0; break; // no idea

				// Standard binary operators...
				case Instruction::Add : cost += 1; break;
				case Instruction::FAdd: cost += 3; break;
				case Instruction::Sub : cost += 0; break;
				case Instruction::FSub: cost += 1; break;
				case Instruction::Mul : cost += 4; break;
				case Instruction::FMul: cost += 5; break;
				case Instruction::UDiv: cost += 25; break;
				case Instruction::SDiv: cost += 25; break;
				case Instruction::FDiv: cost += 17; break;
				case Instruction::URem: cost += 10; break; // no idea
				case Instruction::SRem: cost += 10; break; // no idea
				case Instruction::FRem: cost += 21; break;

				// Logical operators...
				case Instruction::And: cost += 1; break;
				case Instruction::Or : cost += 1; break;
				case Instruction::Xor: cost += 1; break;

				// Memory instructions...
				case Instruction::Alloca: cost += 1; break; // no idea
				case Instruction::Load:   cost += 3; break;
				case Instruction::Store:  cost += 3; break;
				case Instruction::GetElementPtr: cost += 1; break;

				// Convert instructions...
				case Instruction::Trunc:    cost += 1; break;
				case Instruction::ZExt:     cost += 1; break;
				case Instruction::SExt:     cost += 1; break;
				case Instruction::FPTrunc:  cost += 1; break;
				case Instruction::FPExt:    cost += 1; break;
				case Instruction::FPToUI:   cost += 1; break;
				case Instruction::FPToSI:   cost += 1; break;
				case Instruction::UIToFP:   cost += 1; break;
				case Instruction::SIToFP:   cost += 1; break;
				case Instruction::IntToPtr: cost += 0; break;
				case Instruction::PtrToInt: cost += 0; break;
				case Instruction::BitCast:  cost += 0; break;

				// Other instructions...
				case Instruction::ICmp:   cost += 1; break;
				case Instruction::FCmp:   cost += 1; break;
				case Instruction::PHI:    cost += 0; break;
				case Instruction::Select: cost += 2; break;
				case Instruction::Call:   cost += 15; break; // no idea

				case Instruction::Shl:  cost += 1; break;
				case Instruction::LShr: cost += 1; break;
				case Instruction::AShr: cost += 1; break;
				case Instruction::VAArg:          cost += 1; break; // no idea
				case Instruction::ExtractElement: cost += 2; break;
				case Instruction::InsertElement:  cost += 2; break;
				case Instruction::ShuffleVector:  cost += 1; break;
				case Instruction::ExtractValue:   cost += 2; break;
				case Instruction::InsertValue:    cost += 2; break;

				default:
				{
					errs() << "UNKNOWN INSTRUCTION FOUND: " << *I << "\n";
					assert (false && "UNKNOWN INSTRUCTION FOUND!");
					return 0;
				}
			}
		}

		costMap.insert(std::make_pair(block, cost));

		return cost;
	}

	// Evaluate costs of two paths.
	// We consider them to require uniform mask branching if
	// - one path is significantly more costly than the other
	// - any path is more costly than a fixed threshold
	inline bool evaluateCostFunction(const unsigned costA, const unsigned costB) {
		const bool differenceTooLarge =
			costA > MAX_COST_DIFFERENCE_MULTIPLIER*costB ||
			costB > MAX_COST_DIFFERENCE_MULTIPLIER*costA;

		const bool anyPathTooCostly =
			costA > MAX_COST_THRESHOLD || costB > MAX_COST_THRESHOLD;

		return differenceTooLarge || anyPathTooCostly;
	}


#ifdef PACKETIZER_DEBUG_BOSCC_RUNTIME
	void insertDebugBlocksOnCoherentBranches(BasicBlock* source,
											 BasicBlock* firstPathEndBlock,
											 BasicBlock* secondBlock,
											 BasicBlock* secondPathEndBlock,
											 BasicBlock* joinBlock)
	{
		// Create block on branch around first path (between source and secondBlock)
		const bool fullyUniform = analysisResults->hasFullyUniformEntry(source);
		BasicBlock* bosccFBB = BasicBlock::Create(getGlobalContext(), "bosccF", secondBlock->getParent(), secondBlock);
		BranchInst* bosccFExitBr = BranchInst::Create(secondBlock, bosccFBB);
		Value* printfF = Packetizer::insertPrintf("BOSCC (all-false) executed!", ConstantInt::getAllOnesValue(Type::getInt32Ty(getGlobalContext())), true, bosccFExitBr);
		assert (maskGraph->find(bosccFBB) == maskGraph->end());
		maskGraph->insert(bosccFBB, maskGraph->getEntryMask(source), maskGraph->getEntryMask(source));
		maskGraph->addPredecessor(bosccFBB, source);
		maskGraph->setSuccessorTrue(bosccFBB, secondBlock);
		analysisResults->addBlockInfo(bosccFBB, true, fullyUniform, true, fullyUniform);
		analysisResults->addValueInfo(bosccFExitBr,
										 AnalysisResults::UNIFORM,
										 AnalysisResults::INDEX_SAME,
										 AnalysisResults::ALIGN_FALSE,
										 AnalysisResults::SPLIT_NEVER, // should not be split
										 false); // mask = false
		analysisResults->addValueInfo(printfF,
										 AnalysisResults::UNIFORM,
										 AnalysisResults::INDEX_SAME,
										 AnalysisResults::ALIGN_FALSE,
										 AnalysisResults::SPLIT_NEVER, // should not be split
										 false); // mask = false
		BranchInst* sourceBr = cast<BranchInst>(source->getTerminator());
		for (unsigned i=0, e=sourceBr->getNumSuccessors(); i<e; ++i) {
			if (sourceBr->getSuccessor(i) != secondBlock) continue;
			sourceBr->setSuccessor(i, bosccFBB);
			break;
		}
		for (BasicBlock::iterator I=secondBlock->begin(), IE=secondBlock->getFirstNonPHI(); I!=IE; ++I) {
			PHINode* phi = cast<PHINode>(I);
			const int idx = phi->getBasicBlockIndex(source);
			if (idx == -1) continue;
			phi->setIncomingBlock(idx, bosccFBB);
		}

		// Create block on branch around second path (between firstPathEndBlock and joinBlock)
		BasicBlock* bosccTBB = BasicBlock::Create(getGlobalContext(), "bosccT", joinBlock->getParent(), joinBlock);
		BranchInst* bosccTExitBr = BranchInst::Create(joinBlock, bosccTBB);
		Value* printfT = Packetizer::insertPrintf("BOSCC (all-true) executed!", ConstantInt::getAllOnesValue(Type::getInt32Ty(getGlobalContext())), true, bosccTExitBr);
		assert (maskGraph->find(bosccTBB) == maskGraph->end());
		maskGraph->insert(bosccTBB, maskGraph->getEntryMask(source), maskGraph->getEntryMask(source));
		maskGraph->addPredecessor(bosccTBB, firstPathEndBlock);
		maskGraph->setSuccessorTrue(bosccTBB, joinBlock);
		analysisResults->addBlockInfo(bosccTBB, true, fullyUniform, true, fullyUniform);
		analysisResults->addValueInfo(bosccTExitBr,
										 AnalysisResults::UNIFORM,
										 AnalysisResults::INDEX_SAME,
										 AnalysisResults::ALIGN_FALSE,
										 AnalysisResults::SPLIT_NEVER, // should not be split
										 false); // mask = false
		analysisResults->addValueInfo(printfT,
										 AnalysisResults::UNIFORM,
										 AnalysisResults::INDEX_SAME,
										 AnalysisResults::ALIGN_FALSE,
										 AnalysisResults::SPLIT_NEVER, // should not be split
										 false); // mask = false
		BranchInst* firstPathEndBlockBr = cast<BranchInst>(firstPathEndBlock->getTerminator());
		for (unsigned i=0, e=firstPathEndBlockBr->getNumSuccessors(); i<e; ++i) {
			if (firstPathEndBlockBr->getSuccessor(i) != joinBlock) continue;
			firstPathEndBlockBr->setSuccessor(i, bosccTBB);
			break;
		}
		for (BasicBlock::iterator I=joinBlock->begin(), IE=joinBlock->getFirstNonPHI(); I!=IE; ++I) {
			PHINode* phi = cast<PHINode>(I);
			const int idx = phi->getBasicBlockIndex(firstPathEndBlock);
			if (idx == -1) continue;
			phi->setIncomingBlock(idx, bosccTBB);
		}

		joinBlock->getParent()->viewCFG();
	}
#endif

};

} // namespace

char CoherentMaskBranchGenerator::ID = 0;
INITIALIZE_PASS_BEGIN(CoherentMaskBranchGenerator, "boscc", "Coherent Mask Branch Generator (BOSCC)", false, false)
INITIALIZE_PASS_DEPENDENCY(VectorizationAnalysis)
INITIALIZE_PASS_DEPENDENCY(MaskGenerator)
INITIALIZE_PASS_DEPENDENCY(SelectGenerator)
INITIALIZE_PASS_END(CoherentMaskBranchGenerator, "boscc", "Coherent Mask Branch Generator (BOSCC)", false, false)

// Public interface to the CoherentMaskBranchGenerator pass
namespace llvm {

FunctionPass* createCoherentMaskBranchGeneratorPass(bool* failed, const bool verbose=false) {
	return new CoherentMaskBranchGenerator(failed, verbose);
}

}



#endif	/* _COHERENTMASKBRANCHGENERATOR_HPP */
