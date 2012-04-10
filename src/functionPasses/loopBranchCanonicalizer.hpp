/**
 * @file   loopBranchCanonicalizer.hpp
 * @date   24.07.2009
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2008, 2009, 2010 Saarland University
 *
 * This pass flips all outgoing edges of branches that exit a loop
 * if the branch condition is 'true' instead of 'false'.
 *
 */
#ifndef _LOOPBRANCHCANONICALIZER_HPP
#define	_LOOPBRANCHCANONICALIZER_HPP

#ifdef DEBUG_TYPE
#undef DEBUG_TYPE
#endif
#define DEBUG_TYPE "loopbranchcanonicalizer"

#include "llvm/Support/raw_ostream.h"

#include <llvm/Pass.h>
#include <llvm/Transforms/Scalar.h> //BreakCriticalEdges
#include <llvm/Analysis/LoopInfo.h>

#include "llvm/ADT/Statistic.h" //STATISTIC

// the following includes are only required for single-file compilation
#include "llvm/Instructions.h"
#include "llvm/LLVMContext.h"
#include "packetizerConfig.hpp"
#include "returnUnifier.hpp"
#include "phiCanonicalizer.hpp"

// forward declaration of initializer
namespace llvm {
	void initializeLoopBranchCanonicalizerPass(PassRegistry&);
}

using namespace llvm;

STATISTIC(LoopBranchCounter, "Counts number of loop branches that were checked");
STATISTIC(LoopBranchesFlipped, "Counts number of loop branches that were flipped");
STATISTIC(ComparisonsFlipped, "Counts number of comparison operations that were flipped");
STATISTIC(BitOperationsFlipped, "Counts number of bit operations that were flipped");

namespace {

class LoopBranchCanonicalizer : public FunctionPass {
public:
	static char ID; // Pass identification, replacement for typeid
	LoopBranchCanonicalizer(const bool verbose_flag = false)
			: FunctionPass(ID), mVerbose(verbose_flag)
	{
		initializeLoopBranchCanonicalizerPass(*PassRegistry::getPassRegistry());
	}

	~LoopBranchCanonicalizer() {}

	virtual void releaseMemory() {}

	virtual bool runOnFunction(Function& f) {
		LoopBranchCounter = 0;
		LoopBranchesFlipped = 0;
		ComparisonsFlipped = 0;
		BitOperationsFlipped = 0;

		// initialize constants
		boolOneConst = Constant::getAllOnesValue(Type::getInt1Ty(getGlobalContext()));
		boolZeroConst = Constant::getNullValue(Type::getInt1Ty(getGlobalContext()));

		// get loop info
		LoopInfo& LI = getAnalysis<LoopInfo>();
		loopInfo = &LI;

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "canonicalizing loop branches of function '" << f.getNameStr() << "'...\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		for (LoopInfo::iterator L=loopInfo->begin(); L!=loopInfo->end(); ++L) {
			canonicalizeLoopBranches(*L);
		}

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "loop-branch canonicalization done!\n"; );
		DEBUG_PKT( this->print(outs(), f.getParent()); );

		//assert (verify(&f) && "verification of loop-branch-canonicalization failed!");

		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		return LoopBranchesFlipped > 0; // CFG is only preserved if no branch was modified
	}

	void print(raw_ostream &o, const Module *M) const {
		o << "  loop-branches seen: " << LoopBranchCounter << "\n";
		o << "  loop-branches flipped: " << LoopBranchesFlipped << "\n";
		//o << "  comparisons flipped: " << ComparisonsFlipped << "\n";
		//o << "  bit operations flipped: " << BitOperationsFlipped << "\n\n";
	}

	virtual void getAnalysisUsage(AnalysisUsage &AU) const {
		AU.addRequired<LoopInfo>();
		AU.addRequired<ReturnUnifier>(); //not really required
		AU.addRequired<PhiCanonicalizer>(); //not really required

		AU.addPreservedID(BreakCriticalEdgesID); //really?
		AU.addPreservedID(LowerSwitchID);
		AU.addPreservedID(LoopSimplifyID);
		AU.addPreserved<LoopInfo>(); //?
		AU.addPreserved<ReturnUnifier>();
		AU.addPreserved<PhiCanonicalizer>();

		//AU.setPreservesCFG(); //not preserved, because we change terminator instructions
	}

	//returns true if all conditional branches in 'f' are canonicalized in terms of the masking:
	//all loop-exits go through false-branches, all backedges go through true-branches
	//returns false otherwise and prints all 'bad' branches
	//TODO: correct?
	bool verify(Function* f) {
		outs() << "verifying canonicalization of loop-branches for function '" << f->getNameStr() << "'... ";
		bool verified = true;
		//for (Function::iterator BB=f->begin(); BB!=f->end(); ++BB) {
		for (LoopInfo::iterator L=loopInfo->begin(); L!=loopInfo->end(); ++L) {
			verified &= recVerifyLoopBranchCanonicalization(*L);
		}

		if (verified) outs() << "done.\n";
		else outs() << "verification of loop branch canonicalization failed!\n";
		return verified;
	}

private:
	LoopInfo* loopInfo;

	bool hasLoopFlag;

	Constant* boolOneConst;
	Constant* boolZeroConst;

	bool mVerbose;

	void canonicalizeLoopBranches(Loop* loop) {
		//canonicalize sub-loops
		for (Loop::iterator L=loop->begin(); L!=loop->end(); ++L) {
			canonicalizeLoopBranches(*L);
		}

		DEBUG_PKT( outs() << "  canonicalizing branches of loop: "; loop->print(outs(), loop->getLoopDepth()); );

		for (Loop::block_iterator BB=loop->block_begin(); BB!=loop->block_end(); ++BB) {
			if (loopInfo->getLoopDepth(*BB) != loop->getLoopDepth()) continue; //only check blocks of current depth

			DEBUG_PKT( outs() << "    testing block '" << (*BB)->getNameStr() << "' for flipped branch...\n"; );
			if (!isa<BranchInst>((*BB)->getTerminator())) continue;
			BranchInst* br = cast<BranchInst>((*BB)->getTerminator());
			++LoopBranchCounter;
			if (br->isUnconditional()) continue;

			if (!isBranchFlipped(br, loop)) {
				//DEBUG_IFC( outs() << "      flipped branch found!\n"; )
				//flip branch-directions
				BasicBlock* tmp = br->getSuccessor(0);
				br->setSuccessor(0, br->getSuccessor(1));
				br->setSuccessor(1, tmp);
				DEBUG_PKT( outs() << "      flipped branch!\n"; );
				++LoopBranchesFlipped;

				//now recursively flip all comparisons the condition depends on
//                    if (Instruction* condInst = dyn_cast<Instruction>(br->getCondition()))
//                        if (condInst->getParent() == br->getParent())
//                            recFlipConditions(condInst);

				//much easier ;)
				Value* cond = br->getCondition();
				//BinaryOperator* notInst = BinaryOperator::CreateNot(cond, "", br);
				Value* notVal = createMaskNot(cond, "", br);

				br->setCondition(notVal);
				//possibly need to replace other uses of condition as well!
				//TODO: this might hurt somewhere sometime...
//                    Value* oldCond = cond;
//                    cond->replaceAllUsesWith(notVal);
//                    if (Instruction* notInst = dyn_cast<Instruction>(notVal)) {
//                        notInst->replaceUsesOfWith(notInst, oldCond);
//                    }

			}
		}
	}

	bool recVerifyLoopBranchCanonicalization(Loop* loop) {
		bool verified = true;
		for (Loop::iterator SL=loop->begin(); SL!=loop->end(); ++SL) {
			verified &= recVerifyLoopBranchCanonicalization(*SL);
		}

		for (Loop::block_iterator BB=loop->block_begin(); BB!=loop->block_end(); ++BB) {
			if (loopInfo->getLoopDepth(*BB) != loop->getLoopDepth()) continue; //only check blocks of current depth
			if (BranchInst* br = dyn_cast<BranchInst>((*BB)->getTerminator())) {
				if (br->isUnconditional()) continue;
				if (!isBranchFlipped(br, loop)) {
					if (verified) outs() << "\n";
					errs() << "ERROR: conditional branch in loop block '" << (*BB)->getNameStr() << "' remained flipped after loop-branch-canonicalization:\n";
					br->print(outs());
					verified = false;
				}
			}
		}

		return verified;
	}

	//returns true if the branch exits the loop if the condition is false
	//returns false otherwise
	//implies that the default is that a backedge of a loop is taken if the condition is 'false'
	bool isBranchFlipped(BranchInst* br, Loop* loop) {
		assert (loop->contains(br->getParent()) && "parent of branch has to be inside the loop!");
		assert (loopInfo->getLoopDepth(br->getParent()) == loop->getLoopDepth() && "parent of branch has to have same loop depth as loop!");
		//if exit-conditions test for 1 instead of 0,
		//we have to flip the all-false-cmp to an all-true-cmp
		//and flip the blend-values
		//if (br->isUnconditional()) return false;
		if (br->isUnconditional()) {
			assert (isa<BranchInst>(br->getSuccessor(0)->getTerminator()) && "block is not allowed to end with something other than a branch");
			return isBranchFlipped(cast<BranchInst>(br->getSuccessor(0)->getTerminator()), loop);
		}

		BasicBlock* latchBB = loop->getLoopLatch();

		if (latchBB == br->getSuccessor(0)) return true; //backedge on true-path -> branch is flipped!
		if (latchBB == br->getSuccessor(1)) return false;

		std::set<BasicBlock*>* tmpSet = new std::set<BasicBlock*>();
		bool backedgeOnTruePath = isBackedgeOnForwardPath(br->getSuccessor(0), loop, tmpSet);
		tmpSet->clear();
		bool backedgeOnFalsePath = isBackedgeOnForwardPath(br->getSuccessor(1), loop, tmpSet);
		delete tmpSet;

		//assert (!(backedgeOnTruePath && backedgeOnFalsePath) && "cannot have backedges on both paths of a branch!");
		if (backedgeOnTruePath && backedgeOnFalsePath) return false; //cannot say anything...

		if (backedgeOnTruePath) return true;
		if (backedgeOnFalsePath) return false;

		//this is REALLY bad :P
		errs() << "ERROR: neither edge from branch leads to backedge: "
		<< br->getSuccessor(0)->getNameStr() << " / " << br->getSuccessor(1)->getNameStr() << "\n";
		assert(!"CRITICAL ERROR!");
		return false;

	}

	bool isBackedgeOnForwardPath(BasicBlock* startBB, Loop* loop, std::set<BasicBlock*>* visitedBlocks) {
		//DEBUG_IFC( outs() << "  looking for BB '" << requestedBB->getNameStr() << "' on foward path from BB '" << startBB->getNameStr() << "'\n"; )
		if (!loop->contains(startBB)) return false; //we definitiely have an exit on this path
		if (visitedBlocks->find(startBB) != visitedBlocks->end()) return true; //current block already seen, so we took a backedge at some point :p
		visitedBlocks->insert(startBB);

		BasicBlock* latchBB = loop->getLoopLatch();

		//recurse into successors (walk graph forward)
		typedef GraphTraits<BasicBlock*> BlockTraits;
		for (BlockTraits::ChildIteratorType PI = BlockTraits::child_begin(startBB),
				PE = BlockTraits::child_end(startBB); PI != PE; ++PI) {
			BasicBlock* succBB = *PI;
			//DEBUG_IFC( outs() << "    checking predecessor '" << predBB->getNameStr() << "'\n"; )
			if (latchBB == succBB) return true;
			if (isBackedgeOnForwardPath(succBB, loop, visitedBlocks)) return true;
		}
		//DEBUG_IFC( outs() << "  nothing found on path through '" << startBB->getNameStr() << "'\n"; )
		return false;
	}

#if 0
	//if correct, this should implement DeMorgan's rules...
	//TODO: incomplete
	void recFlipConditions(Instruction* inst) {
		//if condition is a comparison-operation, flip it
		if (CmpInst* cmpInst = dyn_cast<CmpInst>(inst)) {
			cmpInst->setPredicate(cmpInst->getInversePredicate());
			DEBUG_PKT( outs() << "      flipped comparison!\n"; );
			++ComparisonsFlipped;
		} else if (isa<BinaryOperator>(inst)) {
			Instruction* newInst = NULL;
			switch (inst->getOpcode()) { //TODO: use createMaskOr etc.
				case Instruction::And : newInst = BinaryOperator::Create(Instruction::Or, inst->getOperand(0), inst->getOperand(1), inst->getNameStr(), inst); break;
				case Instruction::Or  : newInst = BinaryOperator::Create(Instruction::And, inst->getOperand(0), inst->getOperand(1), inst->getNameStr(), inst); break;
				case Instruction::Xor : {
					Value* xorOpVal = BinaryOperator::getNotArgument(inst);
					if (isa<Instruction>(xorOpVal)) newInst = cast<Instruction>(xorOpVal);
					else return; //argument is no instruction -> stop recursion
					break;
				}
				default : break;
			}
			if (newInst) {
				inst->replaceAllUsesWith(newInst);
				inst->eraseFromParent();
				++BitOperationsFlipped;
				inst = newInst;
			}
		} else {
			//if instruction is neither comparison nor bit operation, we have to stop recursion
			//(don't want to recurse through "normal" instructions
			return;
		}

		//recurse into operands
		for (Instruction::op_iterator OP=inst->op_begin(), E=inst->op_end(); OP!=E; ++OP) {
			if (!isa<Instruction>(OP)) continue; //nothing to flip
			if (isa<PHINode>(OP)) continue; //do not recurse through phi-nodes (prevents looping)
			//TODO: what if branch-condition itself was a phi-node directly? (not handled)
			Instruction* opInst = cast<Instruction>(OP);
			//only flip comparisons in the same block, rest is handled by masks
			if (opInst->getParent() != inst->getParent()) continue;

			recFlipConditions(opInst);
		}
	}
#endif

	Value* createMaskNot(Value* mask, const std::string& name, Instruction* insertBefore) {
		assert (mask->getType()->isIntegerTy(1) && "trying to create bit-operation on non-boolean type!");
		if (mask == boolOneConst) return boolZeroConst;
		if (mask == boolZeroConst) return boolOneConst;
		if (Instruction* maskInstr = dyn_cast<Instruction>(mask)) {
			if (maskInstr->isBinaryOp(Instruction::Xor)) {
				Value* op0 = maskInstr->getOperand(0);
				Value* op1 = maskInstr->getOperand(1);
				if (op0 == boolOneConst) {
					return op1; //found not, return op
				}
				if (op1 == boolOneConst) {
					return op0; //found not, return op
				}
				if (op0 == op1) {
					return boolOneConst; //found 'zero', return 'one'
				}
			}
		}
		return BinaryOperator::CreateNot(mask, name, insertBefore);
	}
};

} // namespace


char LoopBranchCanonicalizer::ID = 0;
INITIALIZE_PASS_BEGIN(LoopBranchCanonicalizer, "loop branch canonicalization", "loop branch canonicalization", false, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_DEPENDENCY(ReturnUnifier)    // not really
INITIALIZE_PASS_DEPENDENCY(PhiCanonicalizer) // not really
INITIALIZE_PASS_END(LoopBranchCanonicalizer, "loop branch canonicalization", "loop branch canonicalization", false, false)

// Public interface to the LoopBranchCanonicalization pass
namespace llvm {
    FunctionPass* createLoopBranchCanonicalizationPass(const bool verbose=false) {
        return new LoopBranchCanonicalizer(verbose);
    }
}

#endif	/* _LOOPBRANCHCANONICALIZER_HPP */

