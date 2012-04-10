/**
 * @file   loopLiveValueAnalysis.hpp
 * @date   22.06.2011
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2011 Saarland University
 *
 */
#ifndef _LOOPLIVEVALUEANALYSIS_HPP
#define	_LOOPLIVEVALUEANALYSIS_HPP

#include "llvm/Support/raw_ostream.h"

#include <map>
#include <set>

#include <llvm/Analysis/LoopInfo.h>

#include "packetizerConfig.hpp" // DEBUG_PKT macro

// dummy
#include "branchInfoAnalysis.hpp"

// forward declaration of initializer
namespace llvm {
	void initializeLoopLiveValueAnalysisPass(PassRegistry&);
}

using namespace llvm;

namespace {

class LoopLiveValueAnalysis : public FunctionPass {
public:
	static char ID; // Pass identification, replacement for typeid
	typedef std::set<Instruction*> LiveValueSetType;

	LoopLiveValueAnalysis(bool* failed_flag=NULL, const bool verbose_flag = false)
			: FunctionPass(ID), failed(failed_flag), mVerbose(verbose_flag)
	{
		initializeLoopLiveValueAnalysisPass(*PassRegistry::getPassRegistry());
	}

	~LoopLiveValueAnalysis() {}

	virtual void releaseMemory() {
		for (std::map<const Loop*, LiveValueSetType*>::iterator
				it=liveValueSets.begin(), E=liveValueSets.end(); it!=E; ++it)
		{
			delete it->second;
		}
	}

	virtual void getAnalysisUsage(AnalysisUsage &AU) const {
		AU.addRequired<LoopInfo>();

		AU.setPreservesAll();
	}


	virtual bool runOnFunction(Function& F)
	{
		if (failed && *failed) return true;

		loopInfo = &getAnalysis<LoopInfo>();

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
				<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "analyzing function for values live across loop "
				<< "boundaries\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
				<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );

		try {
			for (LoopInfo::iterator L=loopInfo->begin(), LE=loopInfo->end(); L!=LE; ++L) {
				findAllLoopLiveValues(*L);
			}
		}
		catch (std::logic_error& error) {
			errs() << "\nException occurred during loop live value analysis: "
					<< error.what() << "\n";
			if (failed) *failed = true;
			return true;
		}
		catch (...) {
			errs() << "\nINTERNAL ERROR: Unexpected exception occurred during "
					<< "loop live value analysis!\n";
			if (failed) *failed = true;
			return true;
		}

		DEBUG_PKT( print(outs()); );
		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
				<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "loop live value analysis finished.\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
				<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( if (!verify()) assert (false && "VERIFICATION FAILED!"); );

		return false;
	}

	void print (raw_ostream& O) const
	{
		if (liveValueSets.empty()) O << "\nloop live value set is empty!\n";
		else O << "\nloop live value sets:\n";
		for (std::map<const Loop*, LiveValueSetType*>::const_iterator
				it=liveValueSets.begin(), E=liveValueSets.end(); it!=E; ++it)
		{
			O << "\nLoop: \n" << *it->first;
			printLoopLiveValues(*it->second);
		}
		O << "\n\n";
	}

	void printLoopLiveValues(const Loop* loop) const {
		assert (loop);
		assert (liveValueSets.find(loop) != liveValueSets.end());
		const LiveValueSetType* liveValues = liveValueSets.find(loop)->second;
		printLoopLiveValues(*liveValues);
	}
	void printLoopLiveValues(const LiveValueSetType& liveValues) const {
		outs() << "\nValues live across loop boundaries:\n";
		for (LiveValueSetType::const_iterator it=liveValues.begin(); it!=liveValues.end(); ++it) {
			outs() << "    val: " << **it << "\n";
		}
		outs() << "\n";
	}

	// TODO: implement
	bool verify() { return true; }

	LiveValueSetType* getLiveValueSet(const Loop* loop) const {
		assert (loop);

		std::map<const Loop*, LiveValueSetType*>::const_iterator it =
			liveValueSets.find(loop);

		assert (it != liveValueSets.end() && "live value set for loop not found!");
		if (it == liveValueSets.end()) return NULL;

		return it->second;
	}

	// remove value from ALL loops
	void removeLiveValue(Instruction* value) {
		assert (value);

		for (std::map<const Loop*, LiveValueSetType*>::iterator
				it=liveValueSets.begin(), E=liveValueSets.end(); it!=E; ++it)
		{
			it->second->erase(value); // if not in this set, nothing bad should happen :)
		}
	}

	void updateLiveValue(Instruction* value, Instruction* newValue) {
		assert (value && newValue);

		for (std::map<const Loop*, LiveValueSetType*>::iterator
				it=liveValueSets.begin(), E=liveValueSets.end(); it!=E; ++it)
		{
			if (it->second->find(value) == it->second->end()) continue;

			it->second->erase(value);
			it->second->insert(newValue);
		}
	}

private:
	bool* failed;
	const bool mVerbose; // required for DEBUG mode
	LoopInfo* loopInfo;

	std::map<const Loop*, LiveValueSetType*> liveValueSets;


	void findAllLoopLiveValues(Loop* loop) {
		assert (loop);

		LiveValueSetType* liveValueSet = new LiveValueSetType();
		findLoopLiveValues(loop, *liveValueSet);
		liveValueSets.insert(std::make_pair(loop, liveValueSet));

		for (Loop::iterator SL=loop->begin(), SLE=loop->end(); SL!=SLE; ++SL) {
			findAllLoopLiveValues(*SL);
		}
	}

	void findLoopLiveValues(Loop* loop, LiveValueSetType& liveValueSet) {
		assert (loop);
		DEBUG_PKT( outs() << "\ncollecting all values that are live across loop boundaries...\n"; );

		for (Loop::block_iterator BB=loop->block_begin(); BB!=loop->block_end(); ++BB) {
			BasicBlock* curBB = *BB;

			for (BasicBlock::iterator I=curBB->begin(); I!=curBB->end(); ++I) {
				Instruction* useI = findUseOutsideLoop(I, loop);
				if (!useI) continue;

				DEBUG_PKT( outs() << "  found live value: " << *I << "\n"; );
				DEBUG_PKT( outs() << "    with use outside loop-boundary: " << *useI << "\n"; );

				// TODO: This seems to be wrong, but not ignoring these breaks loopnsmx10.
				//       We should *have* to blend with induction variables:
				//       If indVar is also used in computation, the last iteration of the loop
				//       would not compute the same "freezed" result if indVar was increased
				//       instead of being freezed as well.
				PHINode* loopPhi = findLoopPhiForInstruction(I, loop);
				if (loopPhi && isPhiInductionVariable(loopPhi, loop)) {
					DEBUG_PKT( outs() << "    value is induction variable - ignored!\n"; );
					continue;
				}

				liveValueSet.insert(I);
			}
		}

		DEBUG_PKT( printLoopLiveValues(liveValueSet); );
	}


	// Returns instruction that uses 'inst' outside the loop,
	// where 'outside' means 'outside the current loop iteration'
	inline Instruction* findUseOutsideLoop(Instruction* inst, const Loop* loop) const {
		for (Instruction::use_iterator U=inst->use_begin(); U!=inst->use_end(); ++U) {
			assert (isa<Instruction>(*U) && "all uses have to be instructions (!?)");
			Instruction* useI = cast<Instruction>(*U);
			if (!loop->contains(useI->getParent())) return useI;
			if (isa<PHINode>(useI) && useI->getParent() == loop->getHeader()) return useI;
		}
		return NULL;
	}

	// This method looks for a phi in the current loop, which may be a parent loop of the loop where 'inst' is defined
	// Returns the loop-header-phi connected to 'inst' if it exists, otherwise NULL
	// phis of the same loop nesting as the live value are prioritized
	// (this prevents resetting of all computations of an inner loop if a phi of the loop header was used for blending)
	// TODO: could also stop if encountered a psi?!
	PHINode* findLoopPhiForInstruction(Instruction* inst, Loop* loop) {
		BasicBlock* parentBB = inst->getParent();
		if (!loop->contains(parentBB)) return NULL;

		BasicBlock* headerBB = loop->getHeader();
		BasicBlock* latchBB = loop->getLoopLatch();
		assert (headerBB && latchBB && "special loop-blocks have to exist! (loop simplification pass missing?)");

		for (BasicBlock::iterator I=headerBB->begin(); I!=headerBB->end(); ++I) {
			if (headerBB->getFirstNonPHI() == I) break;
			PHINode* phi = cast<PHINode>(I);
			assert (phi->getBasicBlockIndex(latchBB) != -1);
			if (phi->getIncomingValueForBlock(latchBB) == inst) return phi;
		}

		//recursively search for a phi
		//for (Instruction::op_iterator OP=inst->op_begin(); OP!=inst->op_end(); ++OP) {
		//    if (!isa<Instruction>(OP)) continue;
		//    Instruction*  opI = cast<Instruction>(OP);
		//    if (!loop->contains(opI->getParent())) continue;
		//
		//    if (PHINode* phi = findLoopPhiForInstruction(opI, loop)) return phi;
		//}

		return NULL;
	}

	// Simply checks if there is a constant incoming value from preheader.
	// TODO: Is this always safe?
	// TODO: What else could define the induction variable?
	inline bool isPhiInductionVariable(PHINode* phi, Loop* loop) {
		//if block has no incoming edge from preheader, phi cannot be an induction variable
		BasicBlock* preheader = loop->getLoopPreheader();
		if (phi->getBasicBlockIndex(preheader) == -1) return false;
		if (loop->getCanonicalInductionVariable() == phi) return true;
		return isa<Constant>(phi->getIncomingValueForBlock(preheader));
	}

};

} // unnamed namespace

char LoopLiveValueAnalysis::ID = 0;
INITIALIZE_PASS_BEGIN(LoopLiveValueAnalysis, "loop live value analysis", "loop live value analysis", false, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_END(LoopLiveValueAnalysis, "loop live value analysis", "loop live value analysis", false, false)

namespace llvm {
	FunctionPass* createLoopLiveValueAnalysisPass(bool* failed, const bool verbose=false)
	{
		return new LoopLiveValueAnalysis(failed, verbose);
	}
}

#endif	/* _LOOPLIVEVALUEANALYSIS_HPP */

