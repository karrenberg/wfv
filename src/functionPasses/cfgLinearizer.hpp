/**
 * @file   cfgLinearizer.hpp
 * @date   26.10.2011
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2011 Saarland University
 *
 * This pass linearizes the blocks of the control flow graph
 * s.t. only conditional branches for loop backedges and non-divergent
 * control flow remain.
 *
 */
#ifndef _CFGLINEARIZER_HPP
#define	_CFGLINEARIZER_HPP


#ifdef DEBUG_TYPE
#undef DEBUG_TYPE
#endif
#define DEBUG_TYPE "cfglinearizer"

#include <llvm/Support/raw_ostream.h>

#include <llvm/Pass.h>
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
#include <set>
#include <map>

// forward declaration of initializer
namespace llvm {
	void initializeCFGLinearizerPass(PassRegistry&);
}

using namespace llvm;

STATISTIC(ConditionalBranchesRemovedCounter, "Counts number of conditional branches that were removed");
STATISTIC(LoopBranchesGeneratedCounter, "Counts number of conditional branches that were generated for new loop backedges");

namespace {

class CFGLinearizer : public FunctionPass {
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

public:
	static char ID; // Pass identification, replacement for typeid
	CFGLinearizer(bool* failed_flag=NULL, const bool verbose_flag=false)
			: FunctionPass(ID), failed(failed_flag), verbose(verbose_flag)
	{
		initializeCFGLinearizerPass(*PassRegistry::getPassRegistry());
	}

	~CFGLinearizer() {}

	virtual void releaseMemory() {}

	//radically linearize all blocks of function (= remove all conditional branches except for backedges)
	//if loops are encountered on one path, the other path is moved after the branch first
	virtual bool runOnFunction(Function& f) {
		ConditionalBranchesRemovedCounter = 0;
		LoopBranchesGeneratedCounter = 0;

		if (failed && *failed) return true;

		loopInfo              = &getAnalysis<LoopInfo>();
		analysisResults       = getAnalysis<VectorizationAnalysis>().getAnalysisResults();
		maskGraph             = getAnalysis<MaskGenerator>().getMaskGraph();

		assert (!maskGraph->hasCompoundMasks() && "mask operation insertion not complete!");

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "linearizing CFG of function '" << f.getNameStr() << "'...\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		try {
			// TODO: implement
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

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "CFG-linearization finished.\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		// CFG is only preserved if no branch was removed or created
		return ConditionalBranchesRemovedCounter > 0 || LoopBranchesGeneratedCounter > 0;
	}

private:
	bool* failed;
	const bool verbose;
	MaskGraph* maskGraph;
	AnalysisResults* analysisResults;
	LoopInfo* loopInfo;
	

	// TODO:
	// - Check if we can optimize if the same value is live-in in several blocks.
	// - Incorporate knowledge that a block has at least a register pressure equal
	//   to the number of its live-in registers
	// - Incorporate knowledge that incoming edges of a block have to have an equal
	//   number of live-in registers (= ignore phis with "undef" values)

	typedef std::list<const BasicBlock*> ScheduleType;

	struct Edge {
		const BasicBlock* startBB;
		const BasicBlock* endBB;
		int cost;
	};

	typedef std::set<const Edge*> EdgeSetType;
	typedef std::map<const BasicBlock*, EdgeSetType*> EdgeSetMapType;

	struct LiveValInfo {
		const BasicBlock* block;
		EdgeSetType* liveInSet;
		EdgeSetMapType* liveOutMap; // maps every successor to its liveOutSet
	};

	typedef std::map<const BasicBlock*, int> BlockCostMapType;
	typedef std::map<const BasicBlock*, LiveValInfo*> BlockLiveValInfoMapType;

	BlockCostMapType blockCostMap;
	BlockLiveValInfoMapType liveValInfoMap;

	typedef std::set<const BasicBlock*> BlockSetType;
	typedef std::map<const BasicBlock*, BlockSetType*> BlockSetMapType;

	BlockSetMapType successorSetMap;


	inline int maxLive(const BasicBlock* block) const {
		assert (block);
		BlockCostMapType::const_iterator it = blockCostMap.find(block);
		return it == blockCostMap.end() ? -1 : it->second;
	}

	inline int liveValCost(const EdgeSetType& liveValues) const {
		int cost = 0;
		for (EdgeSetType::const_iterator it=liveValues.begin(),
				E=liveValues.end(); it != E; ++it)
		{
			cost += (*it)->cost;
		}
		return cost;
	}

	EdgeSetType* getLiveInSet(const BasicBlock* block) const {
		assert (block);
		BlockLiveValInfoMapType::const_iterator it = liveValInfoMap.find(block);
		if (it == liveValInfoMap.end()) return NULL;
		return it->second->liveInSet;
	}

	EdgeSetType* getLiveOutSet(const BasicBlock* block,
							   const BasicBlock* target) const
	{
		assert (block && target);
		BlockLiveValInfoMapType::const_iterator it = liveValInfoMap.find(block);
		if (it == liveValInfoMap.end()) return NULL;

		EdgeSetMapType* liveOutMap = it->second->liveOutMap;
		EdgeSetMapType::const_iterator it2 = liveOutMap->find(target);
		if (it2 == liveOutMap->end()) return NULL;

		return it2->second;
	}



	inline void removeLiveIn(const BasicBlock* block,
							 EdgeSetType& liveValues) const
	{
		assert (block);
		EdgeSetType* liveInSet = getLiveInSet(block);
		liveValues.erase(liveInSet->begin(), liveInSet->end()); // K_block
	}

	inline void insertLiveOut(const BasicBlock* head,
							  const BasicBlock* target,
							  EdgeSetType& liveValues) const
	{
		assert (head && target);
		EdgeSetType* liveOutSet = getLiveOutSet(head, target);
		liveValues.insert(liveOutSet->begin(), liveOutSet->end()); //C_head(block)
	}

	inline void scheduleBlock(const BasicBlock* block,
							  ScheduleType& schedule,
							  BlockSetType& scheduledBlocks) const
	{
		assert (block);
		schedule.push_back(block);
		scheduledBlocks.insert(block);
	}



	// Add 'block' to the 'schedule'-list, updating the set of live values and
	// costs
	void addBlockToSchedule(const BasicBlock* block,
							int& maxCost,
							EdgeSetType& liveValues,
							ScheduleType& schedule,
							BlockSetType& scheduledBlocks) const
	{
		assert (block);

		if (schedule.empty()) {
			maxCost = maxLive(block);
			scheduleBlock(block, schedule, scheduledBlocks);
			return;
		}

		const BasicBlock* last = schedule.back();
		scheduleBlock(block, schedule, scheduledBlocks);
		removeLiveIn(block, liveValues);
		insertLiveOut(last, block, liveValues);

		const int cost = maxLive(block) + liveValCost(liveValues);
		if (cost > maxCost) maxCost = cost;
	}

	inline bool dependenciesNotMet(const BasicBlock* block,
								   const BlockSetType& scheduledBlocks) const
	{
		assert (block);
		for (const_pred_iterator P=pred_begin(block), PE=pred_end(block);
				P!=PE; ++P)
		{
			const BasicBlock* predBB = *P;
			if (scheduledBlocks.find(predBB) == scheduledBlocks.end())
				return true;
		}
		return false;
	}

	inline bool scheduleNotAllowed(const BasicBlock* next,
								   const BlockSetType& scheduledBlocks) const
	{
		assert (next);
		if (scheduledBlocks.find(next) != scheduledBlocks.end()) return true;
		if (dependenciesNotMet(next, scheduledBlocks)) return true;
		return false;
	}

	// Use a DFS to find the best schedule of the blocks schedulable from a given
	// start block.
	// This algorithm iterates over the entire search tree except for paths that
	// correspond to invalid schedules.
	// However, it stops if a schedule with globally minimal cost is found.
	// NOTE: This algorithm only treats fully divergent regions optimally.
	bool recSchedule(const BasicBlock* block,
					 int& maxCost,
					 EdgeSetType& liveValues,
					 ScheduleType& schedule,
					 BlockSetType& scheduledBlocks,
					 const int minCostForSchedule,
					 const int numBlocksInSchedule) const
	{
		assert (block);

		// Schedule current block.
		addBlockToSchedule(block, maxCost, liveValues, schedule, scheduledBlocks);

		// Initialize final values.
		int          maxCostFinal         = INT_MAX;
		EdgeSetType  liveValuesFinal      = liveValues;
		ScheduleType scheduleFinal        = schedule;
		BlockSetType scheduledBlocksFinal = scheduledBlocks;

		// Fetch all blocks that can succeed 'block' in the final linearized
		// schedule. This may include blocks that are no direct successors of
		// 'block'.
		assert (successorSetMap.find(block) != successorSetMap.end());
		BlockSetType* successorSet = successorSetMap.find(block)->second;

		// Loop over all possible successors of 'block' and find the best schedule.
		// This is a DFS which enumerates all possible schedules.
		for (BlockSetType::iterator it=successorSet->begin(),
				E=successorSet->end(); it!=E; ++it)
		{
			const BasicBlock* succBB = *it;

			// Make sure we do not attempt to schedule blocks that
			// - were already scheduled, or
			// - whose dependencies would be violated by the schedule.
			if (scheduleNotAllowed(succBB, scheduledBlocks)) continue;

			// Initialize values for this successor.
			int          maxCostNew         = 0;
			EdgeSetType  liveValuesNew      = liveValues;
			ScheduleType scheduleNew        = schedule;
			BlockSetType scheduledBlocksNew = scheduledBlocks;

			// Recurse into this successor.
			const bool globalMinFound = recSchedule(succBB,
													maxCostNew,
													liveValuesNew,
													scheduleNew,
												 	scheduledBlocksNew,
													minCostForSchedule,
													numBlocksInSchedule);
			assert (!globalMinFound || (maxCostNew < maxCostFinal));

			// If there is a schedule starting at this successor that is cheaper
			// than previously seen ones, save this one.
			if (maxCostNew < maxCostFinal) {
				maxCostFinal         = maxCostNew;
				liveValuesFinal      = liveValuesNew;
				scheduleFinal        = scheduleNew;
				scheduledBlocksFinal = scheduledBlocksNew;
			}

			// Stop recursion if we found a valid schedule with minimal cost.
			if (globalMinFound ||
				((int)scheduleFinal.size() == numBlocksInSchedule &&
				 maxCostFinal == minCostForSchedule))
			{
				maxCost         = maxCostFinal;
				liveValues      = liveValuesFinal;
				schedule        = scheduleFinal;
				scheduledBlocks = scheduledBlocksFinal;
				return true;
			}
		}

		assert ((int)schedule.size() == numBlocksInSchedule);

		maxCost         = maxCostFinal;
		liveValues      = liveValuesFinal;
		schedule        = scheduleFinal;
		scheduledBlocks = scheduledBlocksFinal;
		return false; // Cost of schedule is not the global minimum.
	}


};

} // unnamed namespace


char CFGLinearizer::ID = 0;
INITIALIZE_PASS_BEGIN(CFGLinearizer, "cfg linearizer", "CFG Linearizer", false, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_DEPENDENCY(VectorizationAnalysis)
INITIALIZE_PASS_DEPENDENCY(MaskGenerator)
INITIALIZE_PASS_END(CFGLinearizer, "cfg linearizer", "CFG Linearizer", false, false)

// Public interface to the CFGLinearizer pass
namespace llvm {

FunctionPass* createCFGLinearizerPass(bool* failed, const bool verbose=false) {
	return new CFGLinearizer(failed, verbose);
}

}


#endif	/* _CFGLINEARIZER_HPP */

