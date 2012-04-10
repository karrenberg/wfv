/**
 * @file   returnUnifier.hpp
 * @date   23.01.2009
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2008, 2009, 2010 Saarland University
 *
 * @TODO: replace by llvm/Transforms/Utils/UnifyFunctionExitNodes.h
 */
#ifndef _RETURNUNIFIER_HPP
#define	_RETURNUNIFIER_HPP

#ifdef DEBUG_TYPE
#undef DEBUG_TYPE
#endif
#define DEBUG_TYPE "returnunifier"

#include "llvm/Support/raw_ostream.h"
#include <vector>

#include <llvm/Pass.h>
#include "llvm/ADT/Statistic.h" //STATISTIC

// the following includes are only required for single-file compilation
#include "llvm/Instructions.h"
#include "llvm/Function.h"
#include "llvm/LLVMContext.h"
#include "llvm/Transforms/Scalar.h"
#include "packetizerConfig.hpp"

// forward declaration of initializer
namespace llvm {
	void initializeReturnUnifierPass(PassRegistry&);
}

//TODO: use llvm/Transforms/Utils/UnifyFunctionExitNodes.h

using namespace llvm;

STATISTIC(UnifiedReturnsCounter, "Counts number of returns unified");

namespace {

class ReturnUnifier : public FunctionPass {
public:
	static char ID; // Pass identification, replacement for typeid
	ReturnUnifier(const bool verbose_flag = false)
			: FunctionPass(ID), verbose(verbose_flag)
	{
		initializeReturnUnifierPass(*PassRegistry::getPassRegistry());
	}

	~ReturnUnifier() {}

	virtual void releaseMemory() {}

	/// transform program to only have one return, i.e.
	/// if there is more than one return-statement in the function,
	/// replace them by additional exit-block with phi-function
	virtual bool runOnFunction(Function &f) {
		UnifiedReturnsCounter = 0;

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "unifying return-statements...\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		//find all return-statements
		std::vector<ReturnInst*> returns;
		for (Function::iterator BB=f.begin(); BB != f.end(); ++BB) {
			if (ReturnInst* ret = dyn_cast<ReturnInst>(BB->getTerminator()))
				returns.push_back(ret);
		}

		assert (returns.size() > 0 && "functions without return statement not allowed!");

		if (returns.size() == 1) {
			DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
			DEBUG_PKT( outs() << "only one return-statement found in function '" << f.getNameStr() << "'.\n"; );
			DEBUG_PKT( outs() << "return-unification finished!\n"; );

			assert (verify(f) && "verification of return unification failed!");

			DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );
			return false; //did not change anything
		}

		//start unification

		const Type* returnType = f.getReturnType();

		//construct exit-block with phi with 'returns.size()' incoming edges and one return
		BasicBlock* exitBlock = BasicBlock::Create(getGlobalContext(), "exit", &f, f.begin());
		exitBlock->moveAfter(&(f.back())); //move at the end of the function

		//if returnType is void, only replace returns by branches to exitBlock
		if (returnType == Type::getVoidTy(getGlobalContext())) {

			std::vector<ReturnInst*>::iterator it = returns.begin();
			while (it != returns.end()) {
				ReturnInst* I = *it++;
				//create branch to exit-block and insert before return
				BranchInst::Create(exitBlock, I);
				//I->uncheckedReplaceAllUsesWith(brInst); //returns should not have any uses
				//remove return-statement
				I->eraseFromParent();

				++UnifiedReturnsCounter;
			}

			//finally, but simple 'return' in exitblock
			ReturnInst::Create(getGlobalContext(), exitBlock);

		} else {

			//if returnType is non-void, we need a phi-function to compute the result

			//add a "final" return-instruction that returns the result of the phi
			PHINode* result = PHINode::Create(returnType, "result", exitBlock);
			ReturnInst::Create(getGlobalContext(), result, exitBlock);

			//create edges from all return-values to exit-Block
			std::vector<ReturnInst*>::iterator it = returns.begin();
			while (it != returns.end()) {
				ReturnInst* I = *it++;

				result->addIncoming(I->getReturnValue(), I->getParent());

				//create branch to exit-block and insert before return
				BranchInst::Create(exitBlock, I);
				//I->uncheckedReplaceAllUsesWith(brInst); //returns should not have any uses
				//remove return-statement
				I->eraseFromParent();

				++UnifiedReturnsCounter;
			}

		}

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "return-unification finished!\n"; );
		DEBUG_PKT( this->print(outs(), f.getParent()); );

		assert (verify(f) && "verification of return unification failed!");

		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

	return true;
	}

	void print(raw_ostream &o, const Module *M) const {
		o << "Returns unified: " << UnifiedReturnsCounter << "\n\n";
	}

	virtual void getAnalysisUsage(AnalysisUsage &AU) const {
		AU.addPreservedID(BreakCriticalEdgesID); //really?
		AU.addPreservedID(LowerSwitchID);
		AU.addPreservedID(LoopSimplifyID);
		//we do not preserve loop info without implementing update-method
	}

	bool verify(Function& f) {
		DEBUG_PKT( outs() << "verifying unification of return-statements... "; );
		bool verified = true;
		bool retFound = false;
		for (Function::const_iterator BB=f.begin(); BB!=f.end(); ++BB) {
			if (!BB->getTerminator()) {
				DEBUG_PKT( if (verified) outs() << "failed!\n"; );
				errs() << "ERROR: block '" << BB->getNameStr() << "' has no terminator!\n";
				verified = false;
				continue;
			}
			if (isa<ReturnInst>(BB->getTerminator())) {
				if (retFound) {
					DEBUG_PKT( if (verified) outs() << "failed!\n"; );
					errs() << "ERROR: more than one return instruction found in function!\n";
					verified = false;
				} else retFound = true;
			}
		}
		DEBUG_PKT( if (verified) outs() << "done.\n"; );
		return verified;
	}

private:
	const bool verbose;
};

} // namespace

char ReturnUnifier::ID = 0;
INITIALIZE_PASS_BEGIN(ReturnUnifier, "return-unifier", "Return Unification", false, false)
INITIALIZE_PASS_END(ReturnUnifier, "return-unifier", "Return Unification", false, false)

// Public interface to the ReturnUnification pass
namespace llvm {
    FunctionPass* createReturnUnifierPass(const bool verbose=false) {
        return new ReturnUnifier(verbose);
    }
}

#endif	/* _RETURNUNIFIER_HPP */
