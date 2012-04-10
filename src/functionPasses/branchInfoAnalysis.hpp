/**
 * @file   branchInfoAnalysis.hpp
 * @date   01.10.2009
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2008, 2009, 2010, 2011 Saarland University
 *
 */
#ifndef _BRANCHINFOANALYSIS_HPP
#define	_BRANCHINFOANALYSIS_HPP


#ifdef DEBUG_TYPE
#undef DEBUG_TYPE
#endif
#define DEBUG_TYPE "branchinfoanalysis"

#include <llvm/Support/raw_ostream.h>

#include <llvm/Pass.h>
#include <llvm/Function.h>
#include <llvm/Module.h>

#include <llvm/Analysis/PostDominators.h>

#include "llvm/ADT/Statistic.h" //STATISTIC

// the following includes are only required for single-file compilation
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Instructions.h>
#include "packetizerConfig.hpp"

// forward declaration of initializer
namespace llvm {
	void initializeBranchInfoAnalysisPass(PassRegistry&);
}

using namespace llvm;

STATISTIC(BranchCounter, "Counts number of CFG edges that were inserted");


namespace {

class BranchInfoAnalysis : public FunctionPass {
public:
	static char ID; // Pass identification, replacement for typeid
	BranchInfoAnalysis(bool* failed_flag=NULL, const bool verbose_flag = false)
			: FunctionPass(ID), failed(failed_flag), verbose(verbose_flag)
	{
		initializeBranchInfoAnalysisPass(*PassRegistry::getPassRegistry());
	}

	~BranchInfoAnalysis() {}

	virtual void releaseMemory() {
		for (BranchMapType::iterator it=branchMap.begin(), E=branchMap.end(); it!=E; ++it) {
			delete it->second;
		}
		branchMap.clear();
	}


	virtual bool runOnFunction(Function &f) {
		BranchCounter = 0;

		if (failed && *failed) return true;

		const DominatorTree&     DT       = getAnalysis<DominatorTree>();
		const PostDominatorTree& PDT      = getAnalysis<PostDominatorTree>();
		const LoopInfo&          loopInfo = getAnalysis<LoopInfo>();

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "analyzing branches...\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );

		try {
			analyzeBranches(f, DT, PDT, loopInfo);
		}
		catch (std::logic_error& error) {
			errs() << "\nException occurred during branch analysis: "
					<< error.what() << "\n";
			if (failed) *failed = true;
			return true;
		}
		catch (...) {
			errs() << "\nINTERNAL ERROR: Unexpected exception occurred during "
					<< "branch analysis!\n";
			if (failed) *failed = true;
			return true;
		}

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "branch analysis finished!\n"; );
		DEBUG_PKT( print(outs(), NULL); );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		return false;
	}

	void print(raw_ostream& o, const Module *M=NULL) const {
		o << "conditional branches with disjunct paths found: " << BranchCounter << "\n";
		for (BranchMapType::const_iterator it=branchMap.begin(), E=branchMap.end(); it!=E; ++it) {
			o << " *"; it->second->print(o);
		}
	}

	virtual void getAnalysisUsage(AnalysisUsage &AU) const {
		AU.addRequired<DominatorTree>();
		AU.addRequired<PostDominatorTree>();
		AU.addRequired<LoopInfo>();

		AU.setPreservesAll();
	}

	struct BranchMapNode {
		BranchMapNode(BasicBlock* sourceBB, BasicBlock* succTrue, BasicBlock* succFalse, BasicBlock* joinBB, BasicBlock* truePathEndBB, BasicBlock* falsePathEndBB)
			: source(sourceBB), successorTrue(succTrue), successorFalse(succFalse), joinBlock(joinBB), truePathEndBlock(truePathEndBB), falsePathEndBlock(falsePathEndBB) {}
		BasicBlock* source;
		BasicBlock* successorTrue;
		BasicBlock* successorFalse;
		BasicBlock* joinBlock;

		BasicBlock* truePathEndBlock;
		BasicBlock* falsePathEndBlock;

		void print(raw_ostream& o) {
			o << "  " << source->getNameStr() << " -> ("
				<< successorTrue->getNameStr() << ", " << successorFalse->getNameStr()
				<< ") -> " << joinBlock->getNameStr() << "\n";
		}
	};
	typedef std::map<BasicBlock*, BranchMapNode*> BranchMapType;
	BranchMapType& getBranchMap() { return branchMap; }

private:
	bool* failed;
	const bool verbose;

	BranchMapType branchMap;

	void analyzeBranches(Function& f,
						 const DominatorTree& DT,
						 const PostDominatorTree& PDT,
						 const LoopInfo& loopInfo)
	{
		//find all conditional branch-statements that are not related to a loop-backedge
		for (Function::iterator BB=f.begin(); BB != f.end(); ++BB) {
			BasicBlock* block = BB;
			TerminatorInst* tI = block->getTerminator();
			assert (tI);
			if (isa<ReturnInst>(tI)) continue;
			assert (isa<BranchInst>(tI));
			BranchInst* br = cast<BranchInst>(tI);
			if (br->isUnconditional()) continue;

			//get the post dominator of the current block (this is where the paths join again)
			assert (PDT[block] && PDT[block]->getIDom() && PDT[block]->getIDom()->getBlock());
			BasicBlock* joinBlock = PDT[block]->getIDom()->getBlock();

			BasicBlock* trueBlock = br->getSuccessor(0);
			BasicBlock* falseBlock = br->getSuccessor(1);

			//we want to ignore all branches that somehow leave a loop:
			//if either branch successor is outside the loop, or
			//if the post dominator of the branch is outside the loop
			//(e.g. if one of the two paths has an exit that is not the first block of the path
			if (Loop* loop = loopInfo.getLoopFor(BB)) {
				if (!loop->contains(trueBlock) ||
						!loop->contains(falseBlock) ||
						!loop->contains(joinBlock)) continue;
			}

			//we also have to ignore cases where paths can join before the post dominator
			//(e.g. when one exit of a multi-exit loop joins the other path earlier than an other exit)
			//-> we have to make sure that both paths are completely disjunct
			//-> path is okay if the end block is dominated by the start block
			BasicBlock* truePathEndBlock = NULL;
			BasicBlock* falsePathEndBlock = NULL;
			unsigned predNr = 0;
			for (pred_iterator P = pred_begin(joinBlock), PE=pred_end(joinBlock); P!=PE; ++P) {
				BasicBlock* predBB = *P;
				if (++predNr > 2) break; //can't handle more than two incoming edges

				assert (!(DT.dominates(trueBlock, predBB) && DT.dominates(falseBlock, predBB)) &&
						"predecessor can not be dominated by both path start blocks");

				if (DT.dominates(trueBlock, predBB)) {
					//assert (!truePathEndBlock && "true path start block dominates more than one predecessor!");
					truePathEndBlock = predBB;
				}
				if (DT.dominates(falseBlock, predBB)) {
					//assert (!falsePathEndBlock && "false path start block dominates more than one predecessor!");
					falsePathEndBlock = predBB;
				}

				//pred is not dominated by either path start block
				if (!truePathEndBlock && !falsePathEndBlock) break;
			}
			if (!truePathEndBlock || !falsePathEndBlock) continue;
			if (predNr > 2) continue; //can't handle more than two incoming edges

			BranchMapNode* node = new BranchMapNode(block, trueBlock, falseBlock, joinBlock, truePathEndBlock, falsePathEndBlock);
			branchMap.insert(std::make_pair(block, node));
			++BranchCounter;
		}
	}
};

}

char BranchInfoAnalysis::ID = 0;
INITIALIZE_PASS_BEGIN(BranchInfoAnalysis, "branch info analysis", "Branch Info Analysis", false, false)
INITIALIZE_PASS_DEPENDENCY(DominatorTree)
INITIALIZE_PASS_DEPENDENCY(PostDominatorTree)
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_END(BranchInfoAnalysis, "branch info analysis", "Branch Info Analysis", false, false)

// Public interface to the BranchInfoAnalysis pass
namespace llvm {
	FunctionPass* createBranchInfoAnalysisPass(bool* failed, const bool verbose=false) {
		return new BranchInfoAnalysis(failed, verbose);
	}
}

#endif	/* _BRANCHINFOANALYSIS_HPP */

