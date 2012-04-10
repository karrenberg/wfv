/**
 * @file   memAccessCanonicalizer.hpp
 * @date   07.08.2011
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2011 Saarland University
 *
 */
#ifndef _MEMACCESSCANONICALIZER_HPP
#define	_MEMACCESSCANONICALIZER_HPP

#ifdef DEBUG_TYPE
#undef DEBUG_TYPE
#endif
#define DEBUG_TYPE "memaccesscanonicalizer"

#include "llvm/Support/raw_ostream.h"

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
	void initializeMemAccessCanonicalizerPass(PassRegistry&);
}

using namespace llvm;

STATISTIC(CanonicalizedMemAccessCounter, "Counts number of canonicalized load/store operations");

namespace {

class MemAccessCanonicalizer : public FunctionPass {
public:
	static char ID; // Pass identification, replacement for typeid
	MemAccessCanonicalizer(const bool verbose_flag = false)
			: FunctionPass(ID), verbose(verbose_flag)
	{
		initializeMemAccessCanonicalizerPass(*PassRegistry::getPassRegistry());
	}

	~MemAccessCanonicalizer() {}

	virtual void releaseMemory() {}

	/// Transform program to only have load/store operations that depend on
	/// a GetElementPtr instruction.
	/// This means we only touch those that do not (accesses to array- or
	/// struct-element 0) and insert a redundant GEP with index 0.
	virtual bool runOnFunction(Function &f) {
		CanonicalizedMemAccessCounter = 0;

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "canonicalizing memory access operations...\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		for (Function::iterator BB=f.begin(), BBE=f.end();
				BB != BBE; ++BB)
		{
			for (BasicBlock::iterator I=BB->begin(), IE=BB->end();
					I != IE; ++I)
			{
				if (!isa<LoadInst>(I) && !isa<StoreInst>(I)) continue;

				Value* pointer = isa<LoadInst>(I) ?
					cast<LoadInst>(I)->getPointerOperand() :
					cast<StoreInst>(I)->getPointerOperand();

				if (isa<GetElementPtrInst>(pointer)) continue;

				// This assert is not really important... just to know what cases can happen...
				DEBUG_PKT(
					if (!isa<Argument>(pointer) &&
						!isa<AllocaInst>(pointer) &&
						!isa<LoadInst>(pointer) &&
						!isa<PHINode>(pointer) &&
						!isa<CallInst>(pointer) &&
						!isa<BitCastInst>(pointer))
					{
						outs() << f << "\n";
						outs() << "oops!\n";
						outs() << "  " << *pointer << "\n";
					}
					assert (isa<Argument>(pointer) ||
						isa<AllocaInst>(pointer) ||
						isa<LoadInst>(pointer) ||
						isa<PHINode>(pointer) ||
						isa<CallInst>(pointer) ||
						isa<BitCastInst>(pointer));
				);

				GetElementPtrInst* gep = GetElementPtrInst::Create(pointer, ConstantInt::get(getGlobalContext(), APInt(32, 0)), "", I);

				if (LoadInst* load = dyn_cast<LoadInst>(I)) {
					load->setOperand(load->getPointerOperandIndex(), gep);
				} else {
					assert (isa<StoreInst>(I));
					StoreInst* store = cast<StoreInst>(I);
					store->setOperand(store->getPointerOperandIndex(), gep);
				}

				++CanonicalizedMemAccessCounter;
			}
		}

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "memory access canonicalization finished!\n"; );
		DEBUG_PKT( this->print(outs(), f.getParent()); );

		assert (verify(f) && "verification of memory access canonicalization failed!");

		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		return CanonicalizedMemAccessCounter > 0;
	}

	void print(raw_ostream &o, const Module *M=NULL) const {
		o << "Memory access operations canonicalized: "
				<< CanonicalizedMemAccessCounter << "\n\n";
	}

	virtual void getAnalysisUsage(AnalysisUsage &AU) const {
		AU.setPreservesCFG();
	}

	bool verify(Function& f) {
		DEBUG_PKT( outs() << "verifying canonicalization of memory access "
				<< "operations... "; );
		bool verified = true;
		for (Function::iterator BB=f.begin(), BBE=f.end();
				BB!=BBE; ++BB)
		{
			for (BasicBlock::iterator I=BB->begin(), IE=BB->end();
					I!=IE; ++I)
			{
				if (!isa<LoadInst>(I) && !isa<StoreInst>(I)) continue;

				Value* pointer = isa<LoadInst>(I) ?
					cast<LoadInst>(I)->getPointerOperand() :
					cast<StoreInst>(I)->getPointerOperand();

				if (!isa<GetElementPtrInst>(pointer)) {
					DEBUG_PKT( if (verified) outs() << "\n"; );
					DEBUG_PKT( outs() << "  ERROR: load/store still has "
							<< "pointer that is no GEP: " << *I << "\n"; );
					verified = false;
				}
			}
		}
		DEBUG_PKT( if (verified) outs() << "done.\n"; );
		return verified;
	}

private:
	const bool verbose;
};

} // namespace

char MemAccessCanonicalizer::ID = 0;
INITIALIZE_PASS_BEGIN(MemAccessCanonicalizer, "mem-access-canonicalizer", "Memory Access Canonicalization", false, false)
INITIALIZE_PASS_END(MemAccessCanonicalizer, "mem-access-canonicalizer", "Memory Access Canonicalization", false, false)


// Public interface to the MemAccessCanonicalization pass
namespace llvm {
    FunctionPass* createMemAccessCanonicalizationPass(const bool verbose=false) {
        return new MemAccessCanonicalizer(verbose);
    }
}

#endif	/* _MEMACCESSCANONICALIZER_HPP */

