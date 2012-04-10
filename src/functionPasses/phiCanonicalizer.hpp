/**
 * @file   phiCanonicalizer.hpp
 * @date   20.03.2009
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2008, 2009, 2010 Saarland University
 *
 * This pass splits all blocks that have at least one phi with more than
 * one incoming edge.
 * Its result is a CFG that only has phi-functions with at most two incoming
 * edges. This implies that only those blocks can have more than two incoming
 * edges that do not hold a phi.
 *
 * @NOTE: this pass updates dominator info but invalidates post dominator info!
 *
 */
#ifndef _PHICANONICALIZER_HPP
#define	_PHICANONICALIZER_HPP

#ifdef DEBUG_TYPE
#undef DEBUG_TYPE
#endif
#define DEBUG_TYPE "phicanonicalizer"

#include "llvm/Support/raw_ostream.h"

#include <llvm/Pass.h>
#include <llvm/Transforms/Scalar.h> //BreakCriticalEdges
#include <llvm/Analysis/LoopInfo.h>

#include "llvm/ADT/Statistic.h" //STATISTIC

#include <llvm/Analysis/Dominators.h>

// the following includes are only required for single-file compilation
#include <list>
#include "llvm/Instructions.h"
#include "llvm/LLVMContext.h"
#include "packetizerConfig.hpp"
#include "returnUnifier.hpp"

// forward declaration of initializer
namespace llvm {
	void initializePhiCanonicalizerPass(PassRegistry&);
}

using namespace llvm;

STATISTIC(SplitPhisCounter, "Counts number of phis that were split");
STATISTIC(NewBlocksCounter, "Counts number of new blocks generated during conversion");

namespace {

class PhiCanonicalizer : public FunctionPass {
public:
	static char ID; // Pass identification, replacement for typeid
	PhiCanonicalizer(const bool verbose_flag = false)
			: FunctionPass(ID), verbose(verbose_flag)
	{
		initializePhiCanonicalizerPass(*PassRegistry::getPassRegistry());
	}

	~PhiCanonicalizer() {}

	virtual void releaseMemory() {}

	virtual bool runOnFunction(Function& f) {
		SplitPhisCounter = 0;
		NewBlocksCounter = 0;

		// get dominator info
		DominatorTree& DT = getAnalysis<DominatorTree>();
		idom = &DT;

		// get loop info
		LoopInfo& LI = getAnalysis<LoopInfo>();
		loopInfo = &LI;

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "canonicalizing non-loop-related phis of function '" << f.getNameStr() << "'...\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		canonicalizePhiNodes(&f);

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "phi-canonicalization finished.\n"; );
		DEBUG_PKT( this->print(outs(), f.getParent()); );

		assert (verify(&f) && "verification of phi-canonicalization failed!");

		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		return NewBlocksCounter > 0; //if no new blocks were created, CFG was preserved
	}

	void print(raw_ostream &o, const Module *M) const {
		o << "phis split: " << SplitPhisCounter << "\n";
		o << "phi-blocks created: " << NewBlocksCounter << "\n\n";
	}
	virtual void getAnalysisUsage(AnalysisUsage &AU) const {
		AU.addRequired<DominatorTree>();
		AU.addRequired<LoopInfo>();
		AU.addRequired<ReturnUnifier>();

		AU.addPreserved<DominatorTree>();
		AU.addPreservedID(BreakCriticalEdgesID);
		AU.addPreservedID(LowerSwitchID);
		AU.addPreservedID(LoopSimplifyID);
		AU.addPreserved<LoopInfo>();
		AU.addPreserved<ReturnUnifier>();

		//post dominator info is NOT preserved!
	}
	bool verify(Function* f) {
		DEBUG_PKT( outs() << "verifying phi canonicalization... "; );
		bool verified = true;
		for (Function::iterator BB=f->begin(); BB != f->end(); ++BB) {
			for (BasicBlock::iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {
				if (BB->getFirstNonPHI() == I) break;
				PHINode* phi = cast<PHINode>(I);
				if (phi->getNumIncomingValues() > 2) {
					if (verified) errs() << "\n";
					errs() << "ERROR: phi has more than 2 incoming values after canonicalization: " << *phi << "\n";
					verified = false;
				}
			}

			unsigned predNr = 0;
			for (pred_iterator PI=pred_begin(BB), PE=pred_end(BB); PI!=PE; ++PI) {
				++predNr;
			}
			if (predNr > 2) {
				if (verified) errs() << "\n";
				errs() << "ERROR: block has more than 2 incoming edges after canonicalization: " << BB->getNameStr() << "\n";
				verified = false;
			}

		}
		DEBUG_PKT(
			if (verified) outs() << "done.\n";
			else errs() << "FAILED!\n";
		);
		return verified;
	}

private:
	LoopInfo* loopInfo;
	DominatorTree* idom;

	const bool verbose;

#ifdef PACKETIZER_USE_OLD_PHI_CANONICALIZATION //default: deactivated
	//ensures that all phis inside a function that do not belong to a loop
	//only have two incoming values
	//for each additional incoming value, an additional block with a phi is
	//constructed as a predecessor to the original phi's node
	//this implies that after this conversion, each block can at most have two
	//incoming edges
	//TODO: check if loop-headers are correctly excluded (they should only have two predecessors)
	void canonicalizePhiNodes(Function* f) {
		bool fixPointNotReached = true;

		while (fixPointNotReached) {
			fixPointNotReached = false;

			for (Function::iterator BB=f->begin(), BBE=f->end(); BB!=BBE; ++BB) {
				if (BB->getFirstNonPHI() == BB->begin()) continue; //no phi, no splitting :p
				//if the block has less than 3 predecessors, we also do not need to do anything
				//DEBUG_PHIC( outs() << "  found phi(s) in block '" << BB->getNameStr() << "'... \n"; )
				unsigned predNr = 0;
				pred_iterator P = pred_begin(BB);
				while (P != pred_end(BB)) { ++predNr; ++P; }
				if (predNr <= 2) continue;

				DEBUG_PKT( outs() << "canonicalizing non-loop-related phis of block '" << BB->getNameStr() << "'... \n"; );

				//save loop of current block in order to avoid splitting the values from the latch
				Loop* loop = loopInfo->getLoopFor(BB);
				BasicBlock* latchBB = loop ? loop->getLoopLatch() : NULL;

				//for each predecessor-pair of BB, split all phis inside BB
				//that have incoming values from both predecessors.
				//those two values build a new phi in a new block that precedes BB
				typedef GraphTraits<Inverse<BasicBlock*> > InvBlockTraits;
				for (InvBlockTraits::ChildIteratorType PI = InvBlockTraits::child_begin(BB),
						PE = InvBlockTraits::child_end(BB); PI != PE; ++PI) {
					BasicBlock* predI = *PI;

					//the latch of a loop has to be the one block that directly goes back to its header
					//all other incoming edges to a header are joined before
					if (latchBB == predI) continue;

					InvBlockTraits::ChildIteratorType tmp = PI;
					for (InvBlockTraits::ChildIteratorType PJ = ++tmp; PJ != PE; ++PJ) {
						BasicBlock* predJ = *PJ;

						//the latch of a loop has to be the one block that directly goes back to its header
						//all other incoming edges to a header are joined before
						if (latchBB == predJ) continue;

						//BasicBlock* ccdBlock = findCommonControlDependence(BB, predI, predJ);
						//if (!ccdBlock) continue; //no common CD for these two incoming values
						////NOTE: those are cases where other conversions have to be performed first
						////TODO: actually, we do not necessarily need to incorporate CD at all
						//DEBUG_PHIC( outs() << "        common control dependence found: " << ccdBlock->getNameStr() << "\n"; )

						BasicBlock* phiBB = NULL;
						bool changed = false;

						//split all phis that have incoming values from both predecessors
						for (BasicBlock::iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {
							if (BB->getFirstNonPHI() == I) break;
							PHINode* phi = cast<PHINode>(I);
							DEBUG_PKT( outs() << "  splitting phi: " << *phi << "\n"; );

							changed |= splitPhi(phi, predI, predJ, phiBB);
						}

						if (!changed) continue; //if nothing changed, go on with next predJ
						fixPointNotReached = true;

						assert (phiBB && "new phi-block must have been created if any phi was split!");

						//"re-wire" branch of predI s.t. the edge to 'BB' is redirected to 'phiBB'
						TerminatorInst* tI = predI->getTerminator();
						bool found = false;
						for (unsigned k=0; k<tI->getNumSuccessors(); ++k) {
							if (tI->getSuccessor(k) != BB) continue;
							tI->setSuccessor(k, phiBB);
							found = true;
							break;
						}
						assert (found && "could not find correct edge from predecessor i to block");
						found = false;
						//"re-wire" branch of predJ s.t. the edge to 'BB' is redirected to 'phiBB'
						TerminatorInst* tJ = predJ->getTerminator();
						for (unsigned k=0; k<tJ->getNumSuccessors(); ++k) {
							if (tJ->getSuccessor(k) != BB) continue;
							tJ->setSuccessor(k, phiBB);
							found = true;
							break;
						}
						assert (found && "could not find correct edge from predecessor j to block");

						//create branch to 'BB'
						BranchInst::Create(BB, phiBB);

						DEBUG_PKT(
							outs() << "  created new block '" << phiBB->getNameStr()
								<< "' that joins edges from blocks '"
								<< predI->getNameStr() << "' and '" << predJ->getNameStr() << "'!\n";
						);
					}
				}

				DEBUG_PKT( outs() << "canonicalization of non-loop-related phis of block '" << BB->getNameStr() << "' finished.\n"; );
			}
		}
	}
#else
	//Ensures that each block can at most have two incoming edges by inserting new blocks
	//A block with N incoming edges is split into N-1 blocks with two incoming edges each.
	//All phis with more than two incoming values are split accordingly.
	//NOTE: this version determines which pair of predecessors to join by
	//      using the dominance relation of the nearest common dominators of all possible pairs
	void canonicalizePhiNodes(Function* f) {
		bool fixPointNotReached = true;

		while (fixPointNotReached) {
			fixPointNotReached = false;

			for (Function::iterator BB=f->begin(), BBE=f->end(); BB!=BBE; ++BB) {
				//if the block has less than 3 predecessors, we do not need to do anything
				unsigned predNr = 0;
				pred_iterator P = pred_begin(BB);
				while (P != pred_end(BB)) { ++predNr; ++P; }
				if (predNr <= 2) continue;

				fixPointNotReached = true;

				DEBUG_PKT( outs() << "canonicalizing incoming edges of block '" << BB->getNameStr() << "'... \n"; );

				ListNode* bestBlockPair = sortIncomingEdges(BB);

				BasicBlock* predI = bestBlockPair->predI;
				BasicBlock* predJ = bestBlockPair->predJ;
				BasicBlock* domBB = bestBlockPair->domBB;
				delete bestBlockPair;

				//create new block that joins the two incoming edges from predI and predJ
				BasicBlock* joinBB = BasicBlock::Create(getGlobalContext(), "bb_phi." + predI->getNameStr() + "." + predJ->getNameStr(), BB->getParent(), BB);
				DEBUG_PKT( outs() << "      generated new block: " << joinBB->getNameStr() << "\n"; );
				++NewBlocksCounter;
				//update dominator tree
				idom->addNewBlock(joinBB, domBB);
				//if necessary, update loop info (add new block to loop)
				//only if block was not the header (this means joinBB is new preheader and thus not inside loop)
				Loop* loop = loopInfo->getLoopFor(BB);
				assert ((!loop || loop->getHeader() != BB) && "loop header must not have more than one incoming edge after loop simplification!");
				if (loop) loop->addBasicBlockToLoop(joinBB, loopInfo->getBase());


				//split all phis that have incoming values from both predecessors
				for (BasicBlock::iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {
					if (BB->getFirstNonPHI() == I) break;
					splitPhi(cast<PHINode>(I), predI, predJ, joinBB);
				}

				//"re-wire" branch of predI s.t. the edge to 'BB' is redirected to 'joinBB'
				TerminatorInst* tI = predI->getTerminator();
				bool found = false;
				for (unsigned k=0; k<tI->getNumSuccessors(); ++k) {
					if (tI->getSuccessor(k) != BB) continue;
					tI->setSuccessor(k, joinBB);
					found = true;
					break;
				}
				assert (found && "could not find correct edge from predecessor i to block");
				found = false;
				//"re-wire" branch of predJ s.t. the edge to 'BB' is redirected to 'joinBB'
				TerminatorInst* tJ = predJ->getTerminator();
				for (unsigned k=0; k<tJ->getNumSuccessors(); ++k) {
					if (tJ->getSuccessor(k) != BB) continue;
					tJ->setSuccessor(k, joinBB);
					found = true;
					break;
				}
				assert (found && "could not find correct edge from predecessor j to block");

				//create branch to 'BB'
				BranchInst::Create(BB, joinBB);

				DEBUG_PKT(
					outs() << "  created new block '" << joinBB->getNameStr()
						<< "' that joins edges from blocks '"
						<< predI->getNameStr() << "' and '" << predJ->getNameStr() << "'!\n";
				);

				DEBUG_PKT( outs() << "canonicalization of non-loop-related phis of block '" << BB->getNameStr() << "' finished.\n"; );
			}
		}
	}
#endif


	struct ListNode {
		ListNode(const unsigned l, BasicBlock* i, BasicBlock* j, BasicBlock* d)
		 : maxPathLength(l), predI(i), predJ(j), domBB(d) {}
		const unsigned maxPathLength;
		BasicBlock* predI;
		BasicBlock* predJ;
		BasicBlock* domBB;
	};
	static bool compareListNodes(ListNode* first, ListNode* second) { return first->maxPathLength < second->maxPathLength; }
	inline ListNode* sortIncomingEdges(BasicBlock* block) const {
		assert (block);

		std::list<ListNode*> list;
		//compute maximal path lengths from each predecessor to common dominator block
		for (pred_iterator PI=pred_begin(block), PE=pred_end(block); PI!=PE; ++PI) {
			for (pred_iterator PJ=PI; PJ!=PE; ++PJ) {
				if (PI == PJ) continue;
				BasicBlock* domBB = idom->findNearestCommonDominator(*PI, *PJ);
				assert (domBB && "blocks must have a common dominator!");
				//outs() << "    common dominator: " << domBB->getNameStr() << "\n";
				unsigned lengthI = recGetMaxPathLength(*PI, domBB, 0);
				unsigned lengthJ = recGetMaxPathLength(*PJ, domBB, 0);
				list.push_back(new ListNode(lengthI > lengthJ ? lengthI : lengthJ, *PI, *PJ, domBB));
			}
		}

		list.sort(compareListNodes);
		ListNode* min = NULL;
		for (std::list<ListNode*>::iterator it=list.begin(), E=list.end(); it!=E; ++it) {
			if (!min) {
				min = *it;
				continue;
			}
			if (idom->dominates(min->domBB, (*it)->domBB)) min = *it;
		}

		DEBUG_PKT(
			outs() << "  sorted list of path lengths of incoming edges to common dominator:\n";
			for (std::list<ListNode*>::const_iterator it=list.begin(), E=list.end(); it!=E; ++it) {
				outs() << "   * " << (*it)->maxPathLength << " | "
					<< (*it)->predI->getNameStr() << " / " << (*it)->predJ->getNameStr() << " (" << (*it)->domBB->getNameStr() << ")\n";
			}
			outs() << "  minimal element: " << min->maxPathLength << " | "
				<< min->predI->getNameStr() << " / " << min->predJ->getNameStr() << " (" << min->domBB->getNameStr() << ")\n";
		);

		// cleanup
		for (std::list<ListNode*>::iterator it=list.begin(), E=list.end(); it!=E; ++it) {
			if (*it == min) continue; // this node is deleted outside
			delete *it;
		}

		return min;
	}
	unsigned recGetMaxPathLength(const BasicBlock* fromBB, const BasicBlock* toBB, unsigned currentPathLength) const {
		assert (fromBB && toBB);
		if (fromBB == toBB) return currentPathLength;
		unsigned maxPathLength = currentPathLength;

		const Loop* loop = loopInfo->getLoopFor(fromBB);
		const BasicBlock* latchBB = loop ? loop->getLoopLatch() : NULL;

		for (const_pred_iterator P=pred_begin(fromBB), PE=pred_end(fromBB); P!=PE; ++P) {
			if (latchBB && latchBB == *P) continue; //don't go back through loop-backedges
			unsigned pathLength = recGetMaxPathLength(*P, toBB, currentPathLength+1);
			if (pathLength > maxPathLength) maxPathLength = pathLength;
		}
		return maxPathLength;
	}

	inline bool splitPhi(PHINode* phi, BasicBlock* predI, BasicBlock* predJ, BasicBlock* joinBB) {
		assert (phi && predI && predJ && joinBB);
		DEBUG_PKT( outs() << "  splitting phi: " << *phi << "\n"; );

		//if phi does not have incoming values from both predI and predJ, don't split anything
		if (phi->getBasicBlockIndex(predI) == -1 ||
			phi->getBasicBlockIndex(predJ) == -1) return false;

		Value* valI = phi->getIncomingValueForBlock(predI);
		Value* valJ = phi->getIncomingValueForBlock(predJ);

		//create new phi with incoming values from predecessors i and j
		PHINode* newPhi = PHINode::Create(phi->getType(), 2U, "phi_c", joinBB);
		newPhi->addIncoming(valI, predI);
		newPhi->addIncoming(valJ, predJ);

		//add incoming value from new block to phi
		phi->addIncoming(newPhi, joinBB);

		//remove values from old phi
		//TODO: possibly change to 'true' and remove assertion below (see comment)
		phi->removeIncomingValue(predI, false); //do not delete empty phi
		phi->removeIncomingValue(predJ, false); //do not delete empty phi

		//this assertion is not necessarily correct, it could happen that a complete phi is moved to new block
		//if e.g. one phi has more than two incoming values and another one only has two...
		assert (phi->getNumIncomingValues() > 1 && "phi should never have less than two incoming values after phi-canonicalization!");

		DEBUG_PKT( outs() << "      generated new phi: " << *newPhi << "\n"; );
		DEBUG_PKT( outs() << "      modified old phi: " << *phi << "\n"; );
		++SplitPhisCounter;

		return true;
	}
};

} // namespace


char PhiCanonicalizer::ID = 0;
INITIALIZE_PASS_BEGIN(PhiCanonicalizer, "phi-canonicalizer", "PHI Canonicalization", false, false)
INITIALIZE_PASS_DEPENDENCY(DominatorTree)
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_DEPENDENCY(ReturnUnifier)
INITIALIZE_PASS_END(PhiCanonicalizer, "phi-canonicalizer", "PHI Canonicalization", false, false)

// Public interface to the PhiCanonicalization pass
namespace llvm {
    FunctionPass* createPhiCanonicalizationPass(const bool verbose=false) {
        return new PhiCanonicalizer(verbose);
    }
}

#endif	/* _PHICANONICALIZER_HPP */

