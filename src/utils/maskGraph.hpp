/**
 * @file   maskGraph.hpp
 * @date   24.08.2009
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2008, 2009, 2010, 2011 Saarland University
 *
 */
#ifndef _MASKGRAPH_HPP
#define	_MASKGRAPH_HPP

#include <typeinfo>
#include "llvm/Assembly/Writer.h" // WriteAsOperand required for loop->print()
#include "utils/analysisResults.hpp"

// the following includes are only required for single-file compilation
#include "llvm/LLVMContext.h"
#include "llvmTools.hpp"

using namespace llvm;

namespace {

class MaskOperation; //forward def
class MaskGraphNode; //forward def

static unsigned nextMaskOpID; // this being static is perfectly okay

class MaskOperation {
private:
	unsigned ID;
public:
	MaskOperation() : ID(++nextMaskOpID) {}
	unsigned getID() const { return ID; }

	virtual bool isMaskValue() const = 0;
	virtual bool isMaskNegation() const = 0;
	virtual bool isMaskConjunction() const = 0;
	virtual bool isMaskDisjunction() const = 0;
	virtual bool isMaskPhi() const = 0;
	virtual bool isMaskNodeRef() const = 0;
	virtual bool operator==(const MaskOperation& m) const = 0;
	virtual void print(raw_ostream& o) const = 0;
	virtual bool verify(std::set<const MaskOperation*>& visitedMaskOps) const = 0;
};

class MaskValue : public MaskOperation {
private:
	Value* mask;
public:
	MaskValue(Value* m) : mask(m) { /*assert(m);*/ }
	virtual bool isMaskValue() const { return true; }
	virtual bool isMaskNegation() const { return false; }
	virtual bool isMaskConjunction() const { return false; }
	virtual bool isMaskDisjunction() const { return false; }
	virtual bool isMaskPhi() const { return false; }
	virtual bool isMaskNodeRef() const { return false; }
	virtual bool operator==(const MaskOperation& m) const {
		if (!m.isMaskValue()) return false;
		const MaskValue& mv = static_cast<const MaskValue&>(m);
		return mask == mv.getMask();
	}
	virtual void print(raw_ostream& o) const { o << getID() << ": " << *mask; }
	virtual bool verify(std::set<const MaskOperation*>& visitedMaskOps) const {
		return mask != NULL;
	}

	inline Value* getMask() const { return mask; }
	inline void setMask(Value* m) { /*assert(m);*/ mask = m; }
};

class MaskNegation : public MaskOperation {
private:
	MaskOperation* mask;
public:
	MaskNegation(MaskOperation* m) : mask(m) { assert(m); }
	virtual bool isMaskValue() const { return false; }
	virtual bool isMaskNegation() const { return true; }
	virtual bool isMaskConjunction() const { return false; }
	virtual bool isMaskDisjunction() const { return false; }
	virtual bool isMaskPhi() const { return false; }
	virtual bool isMaskNodeRef() const { return false; }
	virtual bool operator==(const MaskOperation& m) const {
		if (!m.isMaskNegation()) return false;
		const MaskNegation& mn = static_cast<const MaskNegation&>(m);
		return mask == mn.getMask();
	}
	virtual void print(raw_ostream& o) const {
		o << getID() << ": NOT( "; getMask()->print(o); o << " )";
		//o << getID() << ": NOT( " << getMask()->getID() << " )";
	}
	virtual bool verify(std::set<const MaskOperation*>& visitedMaskOps) const {
		if (visitedMaskOps.find(this) != visitedMaskOps.end()) return true;
		visitedMaskOps.insert(this);
		return (mask && mask != this && mask->verify(visitedMaskOps));
	}

	inline MaskOperation* getMask() const { return mask; }
	inline void setMask(MaskOperation* m) {
		if (mask) delete mask;
		assert(m);
		mask = m;
	}
};

class MaskConjunction : public MaskOperation {
private:
	MaskOperation* maskLeft;
	MaskOperation* maskRight;
public:
	MaskConjunction(MaskOperation* mL, MaskOperation* mR) : maskLeft(mL), maskRight(mR) { assert(mL && mR); }
	virtual bool isMaskValue() const { return false; }
	virtual bool isMaskNegation() const { return false; }
	virtual bool isMaskConjunction() const { return true; }
	virtual bool isMaskDisjunction() const { return false; }
	virtual bool isMaskPhi() const { return false; }
	virtual bool isMaskNodeRef() const { return false; }
	virtual bool operator==(const MaskOperation& m) const {
		if (!m.isMaskConjunction()) return false;
		const MaskConjunction& mc = static_cast<const MaskConjunction&>(m);
		return maskLeft == mc.getMaskLeft() && maskRight == mc.getMaskRight();
	}
	virtual void print(raw_ostream& o) const {
		o << getID() << ": AND( ";
		maskLeft->print(o);
		o << ", ";
		maskRight->print(o);
		o <<  " )";
	}
	virtual bool verify(std::set<const MaskOperation*>& visitedMaskOps) const {
		if (visitedMaskOps.find(this) != visitedMaskOps.end()) return true;
		visitedMaskOps.insert(this);
		return (maskLeft && maskRight && maskLeft != this && maskRight != this && maskLeft->verify(visitedMaskOps) && maskRight->verify(visitedMaskOps));
	}

	inline MaskOperation* getMaskLeft() const { return maskLeft; }
	inline MaskOperation* getMaskRight() const { return maskRight; }
	inline void setMaskLeft(MaskOperation* m) {
		if (maskLeft) delete maskLeft;
		assert(m);
		maskLeft = m;
	}
	inline void setMaskRight(MaskOperation* m) {
		if (maskRight) delete maskRight;
		assert(m);
		maskRight = m;
	}
};

class MaskDisjunction : public MaskOperation {
private:
	MaskOperation* maskLeft;
	MaskOperation* maskRight;
public:
	MaskDisjunction(MaskOperation* mL, MaskOperation* mR) : maskLeft(mL), maskRight(mR) { assert(mL && mR); }
	virtual bool isMaskValue() const { return false; }
	virtual bool isMaskNegation() const { return false; }
	virtual bool isMaskConjunction() const { return false; }
	virtual bool isMaskDisjunction() const { return true; }
	virtual bool isMaskPhi() const { return false; }
	virtual bool isMaskNodeRef() const { return false; }
	virtual bool operator==(const MaskOperation& m) const {
		if (!m.isMaskDisjunction()) return false;
		const MaskDisjunction& md = static_cast<const MaskDisjunction&>(m);
		return maskLeft == md.getMaskLeft() && maskRight == md.getMaskRight();
	}
	virtual void print(raw_ostream& o) const {
		o << getID() << ": OR( ";
		maskLeft->print(o);
		o << ", ";
		maskRight->print(o);
		o <<  " )";
	}
	virtual bool verify(std::set<const MaskOperation*>& visitedMaskOps) const {
		if (visitedMaskOps.find(this) != visitedMaskOps.end()) return true;
		visitedMaskOps.insert(this);
		return (maskLeft && maskRight && maskLeft != this && maskRight != this && maskLeft->verify(visitedMaskOps) && maskRight->verify(visitedMaskOps));
	}

	inline MaskOperation* getMaskLeft() const { return maskLeft; }
	inline MaskOperation* getMaskRight() const { return maskRight; }
	inline void setMaskLeft(MaskOperation* m) {
		if (maskLeft) delete maskLeft;
		assert(m);
		maskLeft = m;
	}
	inline void setMaskRight(MaskOperation* m) {
		if (maskRight) delete maskRight;
		assert(m);
		maskRight = m;
	}
};

class MaskPhiOperation : public MaskOperation {
private:
	SmallVector<std::pair<MaskOperation*, BasicBlock*>, 2> incomingMasks;
public:
	MaskPhiOperation() : MaskOperation() {}
	virtual bool isMaskValue() const { return false; }
	virtual bool isMaskNegation() const { return false; }
	virtual bool isMaskConjunction() const { return false; }
	virtual bool isMaskDisjunction() const { return false; }
	virtual bool isMaskPhi() const { return true; }
	virtual bool isMaskNodeRef() const { return false; }
	virtual bool operator==(const MaskOperation& m) const {
		if (!m.isMaskPhi()) return false;
		const MaskPhiOperation& mpo = static_cast<const MaskPhiOperation&>(m);
		bool isEqual = true;
		for (unsigned i=0, e=incomingMasks.size(); i<e; ++i) {
			isEqual &= (mpo.getIncomingMask(i) == incomingMasks[i].first);
		}
		return isEqual;
	}
	virtual void print(raw_ostream& o) const {
		o << getID() << ": PHI( ";
		for (unsigned i=0, e=incomingMasks.size(); i<e; ++i) {
			o << "[ ";
			if (!incomingMasks[i].first) o << "NULL"; else incomingMasks[i].first->print(o); //o << incomingMasks[i].first->getID();
			if (!incomingMasks[i].second) o << ", NULL ]"; else o << ", " << incomingMasks[i].second->getNameStr() << " ]"; //o << ", " << incomingMasks[i].second->getNameStr() << " ]";
			if (i+1 < e) o << ", ";
		}
		o << " )";
	}
	virtual bool verify(std::set<const MaskOperation*>& visitedMaskOps) const {
		if (visitedMaskOps.find(this) != visitedMaskOps.end()) return true;
		visitedMaskOps.insert(this);
		for (SmallVector<std::pair<MaskOperation*, BasicBlock*>, 2>::const_iterator it=incomingMasks.begin(),
				E=incomingMasks.end(); it!=E; ++it) {
			assert (it->first);
			if (!it->first) return false;
			if (it->first == this) return false;
			if (!it->first->verify(visitedMaskOps)) return false;
		}
		return true;
	}

	inline void addIncoming(MaskOperation* m, BasicBlock* b) { assert(m && b); incomingMasks.push_back(std::make_pair(m, b)); }
	inline unsigned getNumIncomingMasks() const { return incomingMasks.size(); }
	inline MaskOperation* getIncomingMask(unsigned index) const {
		assert (index < incomingMasks.size());
		return incomingMasks[index].first;
	}
	inline BasicBlock* getIncomingBlock(unsigned index) const {
		assert (index < incomingMasks.size());
		return incomingMasks[index].second;
	}
};

class MaskNodeReference : public MaskOperation {
public:
	enum MaskRef { NONE, ENTRY, EXITTRUE, EXITFALSE, LOOPEXITPHI };
private:
	MaskGraphNode* node;
	MaskRef maskRef;
public:
	MaskNodeReference(MaskGraphNode* n, MaskRef mr) : MaskOperation(), node(n), maskRef(mr) { assert(n); }
	MaskNodeReference(MaskGraphNode* n, MaskGraphNode* dir);
	virtual bool isMaskValue() const { return false; }
	virtual bool isMaskNegation() const { return false; }
	virtual bool isMaskConjunction() const { return false; }
	virtual bool isMaskDisjunction() const { return false; }
	virtual bool isMaskPhi() const { return false; }
	virtual bool isMaskNodeRef() const { return true; }
	virtual bool operator==(const MaskOperation& m) const {
		if (!m.isMaskNodeRef()) return false;
		const MaskNodeReference& mnr = static_cast<const MaskNodeReference&>(m);
		return (node == mnr.getNode() && maskRef == mnr.getMaskRef());
	}
	virtual void print(raw_ostream& o) const;
	virtual bool verify(std::set<const MaskOperation*>& visitedMaskOps) const {
		if (visitedMaskOps.find(this) != visitedMaskOps.end()) return true;
		visitedMaskOps.insert(this);
		return node != NULL;
	}

	inline MaskGraphNode* getNode() const { return node; }
	inline void setNode(MaskGraphNode* n) { assert(n); node = n; }
	inline MaskRef getMaskRef() const { return maskRef; }
	inline void setMaskRef(MaskRef& mr) { maskRef = mr; }
	inline bool hasReference() const { return maskRef != NONE; }
	inline bool referencesEntryMask() const { return maskRef == ENTRY; }
	inline bool referencesExitMaskTrue() const { return maskRef == EXITTRUE; }
	inline bool referencesExitMaskFalse() const { return maskRef == EXITFALSE; }
	inline bool referencesLoopExitPhi() const { return maskRef == LOOPEXITPHI; }

};



class MaskGraphNode {
private:
	bool verbose;
	BasicBlock* block;
	MaskOperation* entryMask;
	MaskOperation* exitMaskTrue;
	MaskOperation* exitMaskFalse;

	typedef SmallVector<MaskGraphNode*, 2> PredVectorType;
	PredVectorType preds;

	MaskGraphNode* succTrue;
	MaskGraphNode* succFalse;

	Constant* boolOneConst;
	Constant* boolZeroConst;
	unsigned MaskOperationCounter; //TODO: use!

	inline void init() {
		MaskOperationCounter = 0;
		boolOneConst = Constant::getAllOnesValue(Type::getInt1Ty(getGlobalContext()));
		boolZeroConst = Constant::getNullValue(Type::getInt1Ty(getGlobalContext()));
	}

	Value* createMaskAnd(Value* mask1, Value* mask2, const std::string& name,
			Instruction* insertBefore, AnalysisResults& analysisResults)
	{
		assert (mask1->getType() == Type::getInt1Ty(getGlobalContext()) && "trying to create bit-operation on non-boolean type!");
		assert (mask2->getType() == Type::getInt1Ty(getGlobalContext()) && "trying to create bit-operation on non-boolean type!");
		if (mask1 == boolZeroConst) return boolZeroConst;
		if (mask2 == boolZeroConst) return boolZeroConst;
		if (mask1 == boolOneConst && mask2 == boolOneConst) return boolOneConst;
		if (mask1 == boolOneConst) return mask2;
		if (mask2 == boolOneConst) return mask1;
		if (mask1 == mask2) return mask1;

		++MaskOperationCounter;
		Value* mask = BinaryOperator::Create(Instruction::And, mask1, mask2, name, insertBefore);

		std::set<Value*> tmp;
		const AnalysisResults::UniformInfo& ui = analysisResults.deriveUniformInfo(mask, tmp);
		const AnalysisResults::IndexInfo& ii =
			ui == AnalysisResults::UNIFORM ?
				AnalysisResults::INDEX_SAME :
				AnalysisResults::INDEX_RANDOM;
		analysisResults.setIsMask(mask, true);
		analysisResults.setIndexInfo(mask, ii);
		analysisResults.setAlignmentInfo(mask, AnalysisResults::ALIGN_FALSE);
		const AnalysisResults::SplitInfo& si = analysisResults.deriveMaskSplitInfo(mask);
		analysisResults.setSplitInfo(mask, si);
		DEBUG_PKT( outs() << "New mask is marked as "
				<< AnalysisResults::getUniformInfoString(ui)
				<< AnalysisResults::getIndexInfoString(ii)
				<< " / ALIGN_FALSE / "
				<< AnalysisResults::getSplitInfoString(si)
				<< " (createMaskAnd)\n"; );

		return mask;
	}
	Value* createMaskOr(Value* mask1, Value* mask2, const std::string& name,
			Instruction* insertBefore, AnalysisResults& analysisResults)
	{
		assert (mask1->getType() == Type::getInt1Ty(getGlobalContext()) && "trying to create bit-operation on non-boolean type!");
		assert (mask2->getType() == Type::getInt1Ty(getGlobalContext()) && "trying to create bit-operation on non-boolean type!");
		if (mask1 == boolOneConst) return boolOneConst;
		if (mask2 == boolOneConst) return boolOneConst;
		if (mask1 == boolZeroConst && mask2 == boolZeroConst) return boolZeroConst;
		if (mask1 == boolZeroConst) return mask1;
		if (mask2 == boolZeroConst) return mask2;
		if (mask1 == mask2) return mask1;

		++MaskOperationCounter;
		Value* mask = BinaryOperator::Create(Instruction::Or, mask1, mask2, name, insertBefore);

		std::set<Value*> tmp;
		const AnalysisResults::UniformInfo& ui = analysisResults.deriveUniformInfo(mask, tmp);
		const AnalysisResults::IndexInfo& ii =
			ui == AnalysisResults::UNIFORM ?
				AnalysisResults::INDEX_SAME :
				AnalysisResults::INDEX_RANDOM;
		analysisResults.setIsMask(mask, true);
		analysisResults.setIndexInfo(mask, ii);
		analysisResults.setAlignmentInfo(mask, AnalysisResults::ALIGN_FALSE);
		const AnalysisResults::SplitInfo& si = analysisResults.deriveMaskSplitInfo(mask);
		analysisResults.setSplitInfo(mask, si);
		DEBUG_PKT( outs() << "New mask is marked as "
				<< AnalysisResults::getUniformInfoString(ui)
				<< AnalysisResults::getIndexInfoString(ii)
				<< " / ALIGN_FALSE / "
				<< AnalysisResults::getSplitInfoString(si)
				<< " (createMaskOr)\n"; );

		return mask;
	}
	Value* createMaskNot(Value* mask, const std::string& name,
			Instruction* insertBefore, AnalysisResults& analysisResults)
	{
		assert (mask->getType() == Type::getInt1Ty(getGlobalContext()) && "trying to create bit-operation on non-boolean type!");
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

		++MaskOperationCounter;
		Value* notInst = BinaryOperator::CreateNot(mask, name, insertBefore);

		// Storing the uniform info is a little more complicated here,
		// because CreateNot creates combinations of XOR and SUB
		std::set<Value*> tmp;
		const AnalysisResults::UniformInfo& ui = analysisResults.deriveUniformInfo(notInst, tmp);
		const AnalysisResults::IndexInfo& ii =
			ui == AnalysisResults::UNIFORM ?
				AnalysisResults::INDEX_SAME :
				AnalysisResults::INDEX_RANDOM;
		analysisResults.setIsMask(notInst, true);
		analysisResults.setIndexInfo(notInst, ii);
		analysisResults.setAlignmentInfo(notInst, AnalysisResults::ALIGN_FALSE);
		const AnalysisResults::SplitInfo& si = analysisResults.deriveMaskSplitInfo(notInst);
		analysisResults.setSplitInfo(notInst, si);
		DEBUG_PKT( outs() << "New mask is marked as "
				<< AnalysisResults::getUniformInfoString(ui)
				<< AnalysisResults::getIndexInfoString(ii)
				<< " / ALIGN_FALSE / "
				<< AnalysisResults::getSplitInfoString(si)
				<< " (createMaskNot)\n"; );

		return notInst;
	}

public:
	MaskGraphNode(BasicBlock* b, bool verbose_flag=false)
		: verbose(verbose_flag), block(b), entryMask(NULL), exitMaskTrue(NULL), exitMaskFalse(NULL),
		succTrue(NULL), succFalse(NULL)
	{ assert(b); init(); }
	MaskGraphNode(BasicBlock* b, MaskOperation* em, MaskOperation* exm, bool verbose_flag=false)
		: verbose(verbose_flag), block(b), entryMask(em), exitMaskTrue(exm), exitMaskFalse(NULL),
		succTrue(NULL), succFalse(NULL)
	{ assert(b && em && exm); init(); }
	MaskGraphNode(BasicBlock* b, MaskOperation* em, MaskOperation* exmT, MaskOperation* exmF, bool verbose_flag=false)
		: verbose(verbose_flag), block(b), entryMask(em), exitMaskTrue(exmT), exitMaskFalse(exmF),
		succTrue(NULL), succFalse(NULL)
	{ assert(b && em && exmT && exmF); init(); }
	MaskGraphNode(BasicBlock* b, Value* em, Value* exm, bool verbose_flag=false)
		: verbose(verbose_flag), block(b), entryMask(new MaskValue(em)), exitMaskTrue(new MaskValue(exm)),
		exitMaskFalse(NULL), succTrue(NULL), succFalse(NULL)
	{ assert(b && em && exm); init(); }
	MaskGraphNode(BasicBlock* b, Value* em, Value* exmT, Value* exmF, bool verbose_flag=false)
		: verbose(verbose_flag), block(b), entryMask(new MaskValue(em)), exitMaskTrue(new MaskValue(exmT)),
		exitMaskFalse(new MaskValue(exmF)), succTrue(NULL), succFalse(NULL)
	{ assert(b && em && exmT && exmF); init(); }

	Value* createMask(MaskOperation* mask, Instruction* pos, AnalysisResults& ar) {
		assert (!mask->isMaskPhi() && "phis not allowed in nested mask operation");

		if (mask->isMaskValue()) {
			assert (typeid(*mask) == typeid(MaskValue));
			return static_cast<MaskValue*>(mask)->getMask();
		}
		if (mask->isMaskNegation()) {
			assert (typeid(*mask) == typeid(MaskNegation));
			MaskNegation* md = static_cast<MaskNegation*>(mask);
			Value* maskV = createMask(md->getMask(), pos, ar);
			return createMaskNot(maskV, "", pos, ar);
		}
		if (mask->isMaskConjunction()) {
			assert (typeid(*mask) == typeid(MaskConjunction));
			MaskConjunction* md = static_cast<MaskConjunction*>(mask);
			Value* maskLeft = createMask(md->getMaskLeft(), pos, ar);
			Value* maskRight = createMask(md->getMaskRight(), pos, ar);
			return createMaskAnd(maskLeft, maskRight, "", pos, ar);
		}
		if (mask->isMaskDisjunction()) {
			assert (typeid(*mask) == typeid(MaskDisjunction));
			MaskDisjunction* md = static_cast<MaskDisjunction*>(mask);
			Value* maskLeft = createMask(md->getMaskLeft(), pos, ar);
			Value* maskRight = createMask(md->getMaskRight(), pos, ar);
			return createMaskOr(maskLeft, maskRight, "", pos, ar);
		}
		if (mask->isMaskNodeRef()) {
			assert (typeid(*mask) == typeid(MaskNodeReference));
			MaskNodeReference* mnr = static_cast<MaskNodeReference*>(mask);
			assert (mnr->getNode());
			assert (mnr->getMaskRef() != MaskNodeReference::NONE);

			if (mnr->referencesEntryMask()) {
				//found reference to entry mask
				assert (mnr->getNode() == this);
				assert (mnr->getNode()->getEntryMask() == entryMask);
				return createEntryMask(ar);
			} else if (mnr->referencesExitMaskTrue()) {
				return mnr->getNode()->createExitMaskTrue(ar);
			} else if (mnr->referencesExitMaskFalse()) {
				return mnr->getNode()->createExitMaskFalse(ar);
			} else if (mnr->referencesLoopExitPhi()) {
				assert (!"NOT IMPLEMENTED!");
			}
		}
		assert (!"bad mask operation found!");
		return NULL;
	}

	inline Value* createEntryMask(AnalysisResults& ar) {
		assert (entryMask);
		assert (typeid(*entryMask) == typeid(MaskValue) ||
				typeid(*entryMask) == typeid(MaskDisjunction) ||
				typeid(*entryMask) == typeid(MaskPhiOperation) ||
				typeid(*entryMask) == typeid(MaskNodeReference));

		DEBUG_PKT ( outs() << "createEntryMask() for node: "; print(outs()); );

		Instruction* pos = block->getFirstNonPHI();
		Value* mask = NULL;

		if (entryMask->isMaskValue()) {
			assert (typeid(*entryMask) == typeid(MaskValue));

			mask = static_cast<MaskValue*>(entryMask)->getMask();

//			//make sure we don't miss any blocks if mask is 'true'
//			//if we encounter a phi instruction, this means we followed a backedge
//			if (!isa<PHINode>(mask) && hasPredecessors()) {
//				for (unsigned i=0, e=getNumPredecessors(); i<e; ++i) {
//					MaskGraphNode* predNode = getPredecessor(i);
//					predNode->createExitMaskInDir(this, ar);
//				}
//			}
		}
		else if (entryMask->isMaskDisjunction()) {
			assert (preds[0] && preds[1]);
			assert (typeid(*entryMask) == typeid(MaskDisjunction));
			MaskDisjunction* md = static_cast<MaskDisjunction*>(entryMask);
			Value* maskLeft = createMask(md->getMaskLeft(), pos, ar);
			Value* maskRight = createMask(md->getMaskRight(), pos, ar);
			mask = createMaskOr(maskLeft, maskRight, "", pos, ar);
		}
		else if (entryMask->isMaskPhi()) {
			assert (preds[0] && preds[1]);
			assert (typeid(*entryMask) == typeid(MaskPhiOperation));
			MaskPhiOperation* mpo = static_cast<MaskPhiOperation*>(entryMask);
			assert (mpo->getNumIncomingMasks() == 2);

			PHINode* phi = PHINode::Create(
					Type::getInt1Ty(getGlobalContext()),
					"loop.mask.phi", pos);

			// We mark the phi as UNIFORM before completion:
			// After generating the operands, we derive the information again
			// and possibly overwrite it if VARYING is returned.
			// Other possibility: If it is actually used later-on, it has to be
			// VARYING (otherwise, no loop mask is required).
			// NOTE: This technique might make problems with masks that derived
			//       its value based on this phi before the phi was updated.
			//       However, initially setting loop exit mask phi information
			//       to VARYING in insertMasks() fixed the problem.
//#define PKT_FIXME
#ifdef PKT_FIXME
			ar.addValueInfo(phi,
				AnalysisResults::UNIFORM,
				AnalysisResults::INDEX_NOT_INITIALIZED,
				AnalysisResults::ALIGN_NOT_INITIALIZED,
				AnalysisResults::SPLIT_NEVER, // should never be split
				true); // mask = true
#else
			// NOTE: Setting to UNIFORM and overwriting it later did not work
			//       in all cases: operations using the phi were not updated.
			// If the loop is VARYING, the loop mask phi also has to be VARYING.
			ar.addValueInfo(phi,
				ar.isUniformLoopBlock(block) ?
					AnalysisResults::UNIFORM :
					AnalysisResults::VARYING,
				ar.isUniformLoopBlock(block) ?
					AnalysisResults::INDEX_SAME :
					AnalysisResults::INDEX_RANDOM,
				AnalysisResults::ALIGN_FALSE,
				AnalysisResults::SPLIT_NEVER, // should never be split
				true); // mask = true
#endif
			assert (ar.getValueInfo(phi));
			DEBUG_PKT( outs() << "New loop mask phi is marked as "
					<< AnalysisResults::getUniformInfoString(ar.getValueInfo(phi)->uniformInfo)
					<< AnalysisResults::getIndexInfoString(ar.getValueInfo(phi)->indexInfo)
					<< " / ALIGN_FALSE / SPLIT_NEVER (createEntryMask)\n"
					<< ": " << *phi << "\n"; );

			//update graph to break loops (replace mask by phi)
			//if (entryMask) delete entryMask;
			entryMask = new MaskValue(phi);

			Value* maskLeft = createMask(mpo->getIncomingMask(0), pos, ar);
			Value* maskRight = createMask(mpo->getIncomingMask(1), pos, ar);

			phi->addIncoming(maskLeft, mpo->getIncomingBlock(0));
			phi->addIncoming(maskRight, mpo->getIncomingBlock(1));

			ar.setUniformInfo(phi, ar.joinUniformInfo(maskLeft, maskRight));
			//DEBUG_PKT( outs() << "Loop mask phi is updated to "
					//<< AnalysisResults::getUniformInfoString(ar.getValueInfo(phi)->uniformInfo)
					//<< ": " << *phi << " (createEntryMask)\n"; );

			++MaskOperationCounter;
			mask = phi;
		}
		else if (entryMask->isMaskNodeRef()) {
			assert (preds[0]);
			assert (typeid(*entryMask) == typeid(MaskNodeReference));
			MaskNodeReference* mnr = static_cast<MaskNodeReference*>(entryMask);
			assert (mnr->getNode());
			assert (mnr->hasReference());
			// This assertion is not true anymore since masks can refer e.g. to entry masks
			// of common dominator blocks.
			//assert ((mnr->referencesExitMaskTrue() || mnr->referencesExitMaskFalse()) &&
					//"entry mask can only reference to exit mask!");

			if (mnr->referencesEntryMask()) {
				mask = mnr->getNode()->createEntryMask(ar);
			}
			if (mnr->referencesExitMaskTrue()) {
				mask = mnr->getNode()->createExitMaskTrue(ar);
			}
			if (mnr->referencesExitMaskFalse()) {
				mask = mnr->getNode()->createExitMaskFalse(ar);
			}
			if (mnr->referencesLoopExitPhi()) {
				assert (!"must never happen (loop exit mask phis are not connected to any entry/exit masks directly!");
			}
		}
		else assert (!"bad mask operation found!");

		assert (mask);

		//store generated value in map
		//if (entryMask) delete entryMask;
		entryMask = new MaskValue(mask);

		return mask;
	}

	inline Value* createExitMaskTrue(AnalysisResults& ar) {
		assert (exitMaskTrue);
		assert (typeid(*exitMaskTrue) != typeid(MaskPhiOperation));
		
		DEBUG_PKT ( outs() << "createExitMaskTrue() for node: "; print(outs()); );

		Instruction* pos = block->getTerminator();
		Value* mask = NULL;

		if (exitMaskTrue->isMaskValue()) {
			assert (typeid(*exitMaskTrue) == typeid(MaskValue));
			mask = static_cast<MaskValue*>(exitMaskTrue)->getMask();
		}
		else if (exitMaskTrue->isMaskNegation()) {
			assert (typeid(*exitMaskTrue) == typeid(MaskNegation));
			MaskNegation* mn = static_cast<MaskNegation*>(exitMaskTrue);
			mask = createMaskNot(createMask(mn->getMask(), pos, ar), "", pos, ar);
		}
		else if (exitMaskTrue->isMaskConjunction()) {
			assert (typeid(*exitMaskTrue) == typeid(MaskConjunction));
			MaskConjunction* mc = static_cast<MaskConjunction*>(exitMaskTrue);
			Value* maskLeft = createMask(mc->getMaskLeft(), pos, ar);
			Value* maskRight = createMask(mc->getMaskRight(), pos, ar);
			mask = createMaskAnd(maskLeft, maskRight, "", pos, ar);
		}
		else if (exitMaskTrue->isMaskDisjunction()) {
			assert (typeid(*exitMaskTrue) == typeid(MaskDisjunction));
			MaskDisjunction* md = static_cast<MaskDisjunction*>(exitMaskTrue);
			Value* maskLeft = createMask(md->getMaskLeft(), pos, ar);
			Value* maskRight = createMask(md->getMaskRight(), pos, ar);
			mask = createMaskOr(maskLeft, maskRight, "", pos, ar);
		}
		else if (exitMaskTrue->isMaskNodeRef()) {
			assert (typeid(*exitMaskTrue) == typeid(MaskNodeReference));
			assert (static_cast<MaskNodeReference*>(exitMaskTrue)->getNode());
			assert (static_cast<MaskNodeReference*>(exitMaskTrue)->referencesEntryMask() && "exit mask can only reference to entry mask of same block!");
			assert (static_cast<MaskNodeReference*>(exitMaskTrue)->getNode() == this); //exit mask can only reference to entry mask of same block
			assert (static_cast<MaskNodeReference*>(exitMaskTrue)->getNode()->getEntryMask() == entryMask);
			mask = createEntryMask(ar);
		}
		else assert (!"bad mask operation found!");

		assert (mask);

		//store generated value in map
		//if (exitMaskTrue) delete exitMaskTrue;
		exitMaskTrue = new MaskValue(mask);

		return mask;
	}

	inline Value* createExitMaskFalse(AnalysisResults& ar) {
		assert (exitMaskFalse);
		assert (typeid(*exitMaskFalse) != typeid(MaskPhiOperation));

		DEBUG_PKT ( outs() << "createExitMaskFalse() for node: "; print(outs()); );

		Instruction* pos = block->getTerminator();
		Value* mask = NULL;

		if (exitMaskFalse->isMaskValue()) {
			assert (typeid(*exitMaskFalse) == typeid(MaskValue));
			mask = static_cast<MaskValue*>(exitMaskFalse)->getMask();
		}
		else if (exitMaskFalse->isMaskNegation()) {
			assert (typeid(*exitMaskFalse) == typeid(MaskNegation));
			MaskNegation* mn = static_cast<MaskNegation*>(exitMaskFalse);
			mask = createMaskNot(createMask(mn->getMask(), pos, ar), "", pos, ar);
		}
		else if (exitMaskFalse->isMaskConjunction()) {
			assert (typeid(*exitMaskFalse) == typeid(MaskConjunction));
			MaskConjunction* mc = static_cast<MaskConjunction*>(exitMaskFalse);
			Value* maskLeft = createMask(mc->getMaskLeft(), pos, ar);
			Value* maskRight = createMask(mc->getMaskRight(), pos, ar);
			mask = createMaskAnd(maskLeft, maskRight, "", pos, ar);
		}
		else if (exitMaskFalse->isMaskDisjunction()) {
			assert (typeid(*exitMaskFalse) == typeid(MaskDisjunction));
			MaskDisjunction* md = static_cast<MaskDisjunction*>(exitMaskFalse);
			Value* maskLeft = createMask(md->getMaskLeft(), pos, ar);
			Value* maskRight = createMask(md->getMaskRight(), pos, ar);
			mask = createMaskOr(maskLeft, maskRight, "", pos, ar);
		}
		else if (exitMaskFalse->isMaskNodeRef()) {
			assert (typeid(*exitMaskFalse) == typeid(MaskNodeReference));
			assert (static_cast<MaskNodeReference*>(exitMaskFalse)->getNode());
			assert (static_cast<MaskNodeReference*>(exitMaskFalse)->referencesEntryMask() && "exit mask can only reference to entry mask of same block!");
			assert (static_cast<MaskNodeReference*>(exitMaskFalse)->getNode() == this); //exit mask can only reference to entry mask of same block
			assert (static_cast<MaskNodeReference*>(exitMaskFalse)->getNode()->getEntryMask() == entryMask);
			mask = createEntryMask(ar);
		}
		else assert (!"bad mask operation found!");

		assert (mask);

		//store generated value in map
		//if (exitMaskFalse) delete exitMaskFalse;
		exitMaskFalse = new MaskValue(mask);

		return mask;
	}

	inline Value* createExitMaskInDir(const MaskGraphNode* node, AnalysisResults& ar) {
		assert (node);
		if (succTrue && succTrue == node) return createExitMaskTrue(ar);
		if (succFalse && succFalse == node) return createExitMaskFalse(ar);
		assert (!"could not find successor in this direction!");
		return NULL;
	}

	typedef PredVectorType::iterator pred_iterator;
	typedef PredVectorType::const_iterator pred_const_iterator;
	pred_iterator pred_begin() { return preds.begin(); }
	pred_iterator pred_end() { return preds.end(); }
	pred_const_iterator pred_begin() const { return preds.begin(); }
	pred_const_iterator pred_end() const { return preds.end(); }

	inline void setPredecessor(MaskGraphNode* node, unsigned index) {
		assert (node);
		assert (index < preds.size() && "wrong index specified!");
		preds[index] = node;
	}
	inline bool updatePredecessor(MaskGraphNode* oldNode, MaskGraphNode* newNode) {
		assert (oldNode && newNode);
		assert (isPredecessor(oldNode));
		for (SmallVector<MaskGraphNode*, 2>::iterator it=preds.begin();
				it!=preds.end(); ++it) {
			if (oldNode == *it) {
				*it = newNode;
				return true;
			}
		}
		return false;
	}
	inline void addPredecessor(MaskGraphNode* node) {
		assert (node);
		for (PredVectorType::iterator it=preds.begin(), E=preds.end(); it!=E; ++it) {
			if (node == *it) return; //predecessor already exists
		}
		preds.push_back(node);
	}
	inline bool addSuccessor(MaskGraphNode* node) {
		//this method automatically derives which successor the node is
		assert (node);
		assert (node->getBlock());
		assert (isa<BranchInst>(block->getTerminator()));
		if (node == succTrue || node == succFalse) return false;

		if (isTrueSuccessor(node)) {
			assert (!succTrue);
			setSuccessorTrue(node);
			return true;
		}
		if (isFalseSuccessor(node)) {
			assert (!succFalse);
			setSuccessorFalse(node);
			return true;
		}
		return false;
	}
	inline void setSuccessorTrue(MaskGraphNode* node) {
		assert (node);
		succTrue = node;
	}
	inline void setSuccessorFalse(MaskGraphNode* node) {
		assert (node);
		succFalse = node;
	}

	inline void removePredecessor(unsigned index) {
		assert (index < preds.size() && "wrong index specified!");
		PredVectorType::iterator it=preds.begin()+index;
		assert (it != preds.end());
		assert (*it == preds[index]);
		preds.erase(it);
	}
	inline bool removePredecessor(MaskGraphNode* pred) {
		assert (pred);
		for (PredVectorType::iterator it=preds.begin(), E=preds.end(); it!=E; ++it) {
			if (pred == *it) {
				preds.erase(it);
				return true;
			}
		}
		return false;
	}
	inline void removeSuccessorTrue() {
		succTrue = NULL;
	}
	inline void removeSuccessorFalse() {
		succFalse = NULL;
	}

	inline bool isTrueSuccessor(MaskGraphNode* node) const {
		assert (node);
		assert (block);
		assert (isa<BranchInst>(block->getTerminator()));
		BranchInst* br = cast<BranchInst>(block->getTerminator());
		return br->getSuccessor(0) == node->getBlock();
	}
	inline bool isFalseSuccessor(MaskGraphNode* node) const {
		assert (node);
		assert (block);
		assert (isa<BranchInst>(block->getTerminator()));
		BranchInst* br = cast<BranchInst>(block->getTerminator());
		if (br->isUnconditional()) return false;
		return br->getSuccessor(1) == node->getBlock();
	}

	inline BasicBlock* getBlock() const { assert (block); return block; }

	inline MaskOperation* getEntryMask() const { assert (entryMask); return entryMask; }
	inline MaskOperation* getExitMaskTrue() const { assert (exitMaskTrue); return exitMaskTrue; }
	inline MaskOperation* getExitMaskFalse() const {
		assert (hasConditionalExit() && "getExitMaskFalse() called for mask-map-entry that only has one exit mask!");
		assert (exitMaskFalse);
		return exitMaskFalse;
	}

	inline Value* getEntryMaskVal() const {
		assert (entryMask);
		assert (entryMask->isMaskValue() && "must not ask for explicit mask of type Value* for compound mask!");
		MaskValue* maskOp = static_cast<MaskValue*>(entryMask);
		assert (maskOp->getMask());
		return maskOp->getMask();
	}
	inline Value* getExitMaskTrueVal() const {
		assert (exitMaskTrue);
		assert (exitMaskTrue->isMaskValue() && "must not ask for explicit mask of type Value* for compound mask!");
		MaskValue* maskOp = static_cast<MaskValue*>(exitMaskTrue);
		assert (maskOp->getMask());
		return maskOp->getMask();
	}
	inline Value* getExitMaskFalseVal() const {
		assert (exitMaskFalse);
		assert (exitMaskFalse->isMaskValue() && "must not ask for explicit mask of type Value* for compound mask!");
		MaskValue* maskOp = static_cast<MaskValue*>(exitMaskFalse);
		assert (maskOp->getMask());
		return maskOp->getMask();
	}

	inline MaskGraphNode* getSinglePredecessor() const {
		assert (hasSinglePredecessor() && "node has more than one predecessor!");
		assert (preds[0]);
		return preds[0];
	}
	inline unsigned getNumPredecessors() const { return preds.size(); }
	inline MaskGraphNode* getPredecessor(unsigned index) const {
		assert (index < preds.size() && "wrong index specified!");
		assert (preds[index]);
		return preds[index];
	}
	inline unsigned getNumSuccessors() const {
		if (!hasExitEdge()) return 0;
		if (!hasConditionalExit()) return 1;
		return 2;
	}
	inline MaskGraphNode* getSuccessorTrue() const {
		assert (hasExitEdge() && "node does not have any outgoing edges!");
		return succTrue;
	}
	inline MaskGraphNode* getSuccessorFalse() const {
		assert (hasConditionalExit() && "node only has a single exit edge!");
		return succFalse;
	}

	inline bool hasExitEdge() const { return succTrue != NULL; }
	inline bool hasConditionalExit() const { return succFalse != NULL; }
	inline bool hasPredecessors() const { return preds.size() > 0; }
	inline bool hasSinglePredecessor() const { return preds.size() == 1; }
	inline bool entryMaskIsSinglePredecessorExitMask() const {
		assert (entryMask);
		return hasSinglePredecessor() && entryMask == preds[0]->getExitMaskInDir(this);
	}

	inline bool isPredecessor(const MaskGraphNode* node) const {
		assert (node);
		for (SmallVector<MaskGraphNode*, 2>::const_iterator it=preds.begin();
				it!=preds.end(); ++it) {
			if (node == *it) return true;
		}
		return false;
	}
	inline bool isSuccessor(const MaskGraphNode* node) const {
		assert (node);
		return node == succTrue || node == succFalse;
	}

	inline MaskOperation* getExitMaskInDir(const MaskGraphNode* dir) {
		assert (dir);
		if (succTrue && succTrue == dir) return exitMaskTrue;
		else if (succFalse && succFalse == dir) return exitMaskFalse;
		else return NULL;
	}
	inline MaskOperation* getExitMaskInDir(const BasicBlock* dir) {
		assert (dir);
		if (succTrue && succTrue->getBlock() == dir) return exitMaskTrue;
		else if (succFalse && succFalse->getBlock() == dir) return exitMaskFalse;
		else return NULL;
	}
	inline Value* getExitMaskValInDir(const MaskGraphNode* dir) {
		assert (dir);
		if (succTrue && succTrue == dir) return getExitMaskTrueVal();
		else if (succFalse && succFalse == dir) return getExitMaskFalseVal();
		else return NULL;
	}

	//overwrite entry mask with 'maskOp' without updating connected masks
	inline void setEntryMask(Value* mask) {
		assert (mask);
		//if (entryMask) delete entryMask; // breaks something...
		entryMask = new MaskValue(mask);
	}
	inline void setExitMaskTrue(Value* mask) {
		assert (mask);
		//if (exitMaskTrue) delete exitMaskTrue;
		exitMaskTrue = new MaskValue(mask);
	}
	inline void setExitMaskFalse(Value* mask) {
		assert (mask);
		//if (exitMaskFalse) delete exitMaskFalse;
		exitMaskFalse = new MaskValue(mask);
	}
	inline void setEntryMask(MaskOperation* maskOp) {
		assert (maskOp);
		//if (entryMask) delete entryMask;
		entryMask = maskOp;
	}
	inline void setExitMaskTrue(MaskOperation* maskOp) {
		assert (maskOp);
		//if (exitMaskTrue) delete exitMaskTrue;
		exitMaskTrue = maskOp;
	}
	inline void setExitMaskFalse(MaskOperation* maskOp) {
		assert (maskOp);
		//if (exitMaskFalse) delete exitMaskFalse;
		exitMaskFalse = maskOp;
	}

	void print(raw_ostream& o) const {
		assert (block);
		o << "Node: " << block->getNameStr() << "\n";
		if (!entryMask) o << "  entry mask: NULL\n";
		else {
			o << "  entry mask: "; entryMask->print(o); o << "\n";
		}
		if (hasExitEdge()) {
			if (!exitMaskTrue) o << "  exit mask T: NULL\n";
			else {
				o << "  exit mask T: "; exitMaskTrue->print(o); o << "\n";
			}
		}
		if (hasConditionalExit()) {
			if (!exitMaskFalse) o << "  exit mask F: NULL\n";
			else {
				o << "  exit mask F: "; exitMaskFalse->print(o); o << "\n";
			}
		}

		if (preds.empty()) o << "  no predecessors!\n";
		else {
			for (unsigned i=0, e=preds.size(); i<e; ++i) {
				o << "  predecessor " << i << ": " << preds[i]->getBlock()->getNameStr() << "\n";
			}
		}

		if (hasExitEdge()) {
			if (!succTrue) o << "  successor T: NULL\n";
			else {
				o << "  successor T: " << succTrue->getBlock()->getNameStr() << "\n";
			}
		}
		if (hasConditionalExit()) {
			if (!succFalse) o << "  successor F: NULL\n";
			else {
				o << "  successor F: " << succFalse->getBlock()->getNameStr() << "\n";
			}
		}
	}
	bool verify() const {
		assert (block);
		assert (block->getParent());
		assert (!block->getParent()->getBasicBlockList().empty());
		const bool isEntryBlock = &block->getParent()->getEntryBlock() == block;
		//const bool isReturnBlock = isa<ReturnInst>(block->getTerminator());
		const bool isReturnBlock = !hasExitEdge();
		const bool x = block && entryMask;
		const bool a = isReturnBlock || exitMaskTrue;
		const bool b = !hasConditionalExit() || exitMaskFalse;
		const bool c = isEntryBlock || hasPredecessors();
		const bool d = isEntryBlock || (hasPredecessors() && preds[0]);
		const bool e = isEntryBlock || hasSinglePredecessor() || (getNumPredecessors() > 1 && preds[1]);
		const bool f = isReturnBlock || succTrue;
		const bool g = !hasConditionalExit() || succFalse;
		std::set<const MaskOperation*> visitedMaskOps;
		const bool h = entryMask && entryMask->verify(visitedMaskOps);
		visitedMaskOps.clear();
		const bool i = isReturnBlock || (exitMaskTrue && exitMaskTrue->verify(visitedMaskOps));
		visitedMaskOps.clear();
		const bool j = !hasConditionalExit() || (exitMaskFalse && exitMaskFalse->verify(visitedMaskOps));
		const bool res = x && a && b && c && d && e && f && g && h && i && j;
		if (!res) {
			errs() << "\nERROR: verification of mask graph node failed!\n";
			if (block) errs() << "block: " << block->getNameStr() << "\n";
			if (!entryMask) errs() << "  entry mask is NULL!\n";
			if (!a) errs() << "  exit mask true is NULL!\n";
			if (!b) errs() << "  exit mask false is NULL!\n";
			if (!c) errs() << "  non-entry block has no predecessors!\n";
			if (!d) errs() << "  first predecessor of non-entry block is NULL!\n";
			if (!e) errs() << "  second predecessor of non-entry block is NULL!\n";
			if (!f) errs() << "  true-successor of non-return block is NULL!\n";
			if (!g) errs() << "  false-successor of block with conditional exit is NULL!\n";
			if (!h) {
				errs() << "  verification of entry mask failed: ";
				if (entryMask) { entryMask->print(errs()); errs() << "\n"; }
			}
			if (!i) {
				errs() << "  verification of exit mask (true-edge) failed: ";
				if (exitMaskTrue) { exitMaskTrue->print(errs()); errs() << "\n"; }
			}
			if (!j) {
				errs() << "  verification of exit mask (false-edge) failed: ";
				if (exitMaskFalse) { exitMaskFalse->print(errs()); errs() << "\n"; }
			}

			errs() << "\n";
			assert (!"verification failed!");
		}
		return res;
	}
	bool complete() const {
		assert (block);
		assert (block->getParent());
		assert (!block->getParent()->getBasicBlockList().empty());
		const bool isEntryBlock = &block->getParent()->getEntryBlock() == block;
		const bool isReturnBlock = !hasExitEdge();
		const bool x = block && entryMask;
		const bool a = isReturnBlock || exitMaskTrue;
		const bool b = !hasConditionalExit() || exitMaskFalse;
		const bool c = isEntryBlock || !preds.empty();
		const bool d = isEntryBlock || (!preds.empty() && preds[0]);
		const bool e = isEntryBlock || hasSinglePredecessor() || (preds.size() > 1 && preds[1]);
		const bool f = isReturnBlock || succTrue;
		const bool g = !hasConditionalExit() || succFalse;
		const bool res = x && a && b && c && d && e && f && g;
		return res;
	}
};


class LoopExitNode {
private:
	BasicBlock* block;  //exiting block
	BasicBlock* target; //exit block
	const Loop* innermostLoop; //the innermost loop that is left (not necessarily the one this node is connected to!)

	MaskOperation* maskUpdateOp; //the 'or' that updates the mask before the exit

	//map of phi operations of each loop & related mask update operations
	//NOTE: this also includes the innermost loop
	typedef std::map<const Loop*, MaskOperation*> MaskPhiMapType;
	MaskPhiMapType maskPhiMap;
public:
	LoopExitNode(BasicBlock* exitingBlock, const LoopInfo& loopInfo)
	: block(exitingBlock), innermostLoop(loopInfo.getLoopFor(exitingBlock)), maskUpdateOp(NULL)
	{
		assert (block);
		assert (block->getTerminator());
		assert (isa<BranchInst>(block->getTerminator()));
		BranchInst* br = cast<BranchInst>(block->getTerminator());
		assert (br->isConditional());
		target = innermostLoop->contains(br->getSuccessor(1)) ?
			br->getSuccessor(2) : br->getSuccessor(1);

		//add innermost loop to map
		assert (maskPhiMap.find(innermostLoop) == maskPhiMap.end());
		maskPhiMap.insert(std::make_pair(innermostLoop, new MaskValue(NULL)));
	}

	typedef MaskPhiMapType::iterator iterator;
	typedef MaskPhiMapType::const_iterator const_iterator;
	inline iterator begin() { return maskPhiMap.begin(); }
	inline iterator end() { return maskPhiMap.end(); }
	inline const_iterator begin() const { return maskPhiMap.begin(); }
	inline const_iterator end() const { return maskPhiMap.end(); }

	inline bool isInnermostLoop(const Loop* loop) const {
		assert (loop);
		assert (innermostLoop);
		return loop == innermostLoop;
	}

	inline bool exitsMultipleLoops() const { return maskPhiMap.size() > 1; }

	inline BasicBlock* getExitingBlock() const { assert (block); return block; }
	inline BasicBlock* getExitBlock()  const{ assert (target); return target; }
	inline const Loop* getInnermostLoop() const { assert(innermostLoop); return innermostLoop; }
	inline unsigned getNumExitedLoops() const { return maskPhiMap.size(); }

	inline MaskOperation* getMaskUpdateOp() const {
		assert (maskUpdateOp);
		return maskUpdateOp;
	}
	inline MaskOperation* getMaskPhiOp(const Loop* l) const {
		MaskPhiMapType::const_iterator tmp = maskPhiMap.find(l);
		assert (tmp != maskPhiMap.end());
		assert (tmp->second);
		return tmp->second;
	}

	inline void setMaskPhiOp(const Loop* loop, MaskOperation* maskPhi) {
		assert (loop && maskPhi);
		MaskPhiMapType::iterator tmp = maskPhiMap.find(loop);
		assert (tmp != maskPhiMap.end());
		if (tmp->second) delete tmp->second;
		tmp->second = maskPhi;
	}
	inline void setMaskUpdateOp(MaskOperation* maskOp) {
		assert (maskOp);
		if (maskUpdateOp) delete maskUpdateOp;
		maskUpdateOp = maskOp;
	}

	inline void addLoop(const Loop* loop) {
		assert (loop);
		assert (maskPhiMap.find(loop) == maskPhiMap.end());
		maskPhiMap.insert(std::make_pair(loop, new MaskValue(NULL)));
	}
	inline void addLoop(const Loop* loop, MaskOperation* maskPhi) {
		assert (loop);
		assert (maskPhiMap.find(loop) == maskPhiMap.end());
		maskPhiMap.insert(std::make_pair(loop, maskPhi));
	}


	inline bool verify(AnalysisResults& ar) const {
		if (ar.isUniform(innermostLoop)) return true; // uniform loops have no mask phis etc.

		if (!block || !target || !innermostLoop || !maskUpdateOp) return false;

		for (MaskPhiMapType::const_iterator MO=maskPhiMap.begin(),
				MOE=maskPhiMap.end(); MO!=MOE; ++MO)
		{
			if (!MO->first || !MO->second) return false;
			if (MO->second->isMaskValue()) {
				if (!(static_cast<MaskValue*>(MO->second)->getMask())) return false;
			}
		}
		return true;
	}
	inline void print(raw_ostream& o) const {
		assert (block && target);
		o << "Loop Exit '" << block->getNameStr() << "' -> '" << target->getNameStr() << "':\n";
		o << "  innermost loop: "; if (!innermostLoop) o << "NULL!\n"; else innermostLoop->print(o);
		o << "  mask update operation: "; if (!maskUpdateOp) o << "NULL!\n"; else { maskUpdateOp->print(o); o << "\n"; }
		o << "  exited loops & corresponding loop mask phis:\n";
		for (MaskPhiMapType::const_iterator MO=maskPhiMap.begin(),
		MOE=maskPhiMap.end(); MO!=MOE; ++MO) {
			assert(MO->first && MO->second);
			o << "    loop mask phi: "; MO->second->print(o);
			o << "\n    "; MO->first->print(o, MO->first->getLoopDepth());
		}
		o << "\n";
	}

};

class LoopNode {
private:
	const Loop* loop;

	std::set<BasicBlock*> loopExits;
	typedef std::map<BasicBlock*, std::pair<BasicBlock*, BasicBlock*> > EntryMapType; //header -> (preheader, latch)
	EntryMapType loopEntries;

	LoopNode* parentLoopNode;
	SmallVector<LoopNode*, 2> nestedLoopNodes;
public:
	LoopNode(const Loop* l, LoopNode* parent) : loop(l), parentLoopNode(parent) {}

	inline bool isTopLevelLoop() const { return parentLoopNode == NULL; }
	inline bool hasParentLoop() const { return parentLoopNode != NULL; }
	inline bool hasNestedLoop() const { return nestedLoopNodes.size() > 0; }
	inline bool hasMultipleExits() const { return loopExits.size() > 1; }
	inline bool hasMultipleEntries() const { return loopEntries.size() > 1; }

	inline const Loop* getLoop() const { assert(loop); return loop; }
	//returns the number of *direct* child-loops (non-recursive)
	inline unsigned getNumNestedLoops() const { return nestedLoopNodes.size(); }
	inline LoopNode* getNestedLoopNode(unsigned index) const {
		assert (index < nestedLoopNodes.size() && "wrong index specified!");
		assert (nestedLoopNodes[index] && "nested loop node is NULL!");
		return nestedLoopNodes[index];
	}
	inline LoopNode* getParentLoopNode() const {
		assert (hasParentLoop() && "getParentLoop() called for top-level LoopNode!");
		return parentLoopNode;
	}

	inline unsigned getNumLoopExits() const { return loopExits.size(); }
	inline unsigned getNumLoopEntries() const { return loopEntries.size(); }

	inline void addNestedLoopNode(LoopNode* child) { nestedLoopNodes.push_back(child); }
	inline void addLoopExit(BasicBlock* exitingBlock) {
		assert (loopExits.find(exitingBlock) == loopExits.end());
		loopExits.insert(exitingBlock);
	}
	inline void addLoopEntry(BasicBlock* header, BasicBlock* preheader, BasicBlock* latch) {
		assert (loopEntries.find(header) == loopEntries.end());
		loopEntries.insert(std::make_pair(header, std::make_pair(preheader, latch)));
	}

	//	inline BasicBlock* getPreheader(BasicBlock* header) {}

	inline bool verify() const {
		const bool res = loop != NULL;
		//parentLoopNode is allowed to be null
		if (!res) {
			errs() << "\nERROR: verification of loop graph node failed!\n";
			errs() << "  Loop: "; loop->print(errs());
			errs() << "  loop is NULL!\n";

			errs() << "\n";
			assert (!"verification failed!");
		}
		return res;
	}
	inline void print(raw_ostream& o) const {
		assert (loop);
		o << "Loop Node for loop: "; loop->print(o, loop->getLoopDepth());

		assert (!loopExits.empty());
		o << "  loop exits:\n";
		for (std::set<BasicBlock*>::const_iterator BB=loopExits.begin(), BBE=loopExits.end(); BB!=BBE; ++BB) {
			o << "   * " << (*BB)->getNameStr() << "\n";
		}

		if (!nestedLoopNodes.empty()) o << "\n  nested loops:\n";
		for (SmallVector<LoopNode*, 2>::const_iterator LN=nestedLoopNodes.begin(), LNE=nestedLoopNodes.end(); LN!=LNE; ++LN) {
			assert (*LN);
			assert ((*LN)->getLoop());
			//o << "   * "; (*LN)->getLoop()->print(o, (*LN)->getLoop()->getLoopDepth());
			(*LN)->print(o);
		}
		o << "\n";
	}

};


//TODO: setExitMaskInDir() to replace exitEdgeOnTruePath .... stuff
class MaskGraph {
private:
	bool verbose;
	typedef std::map<BasicBlock*, MaskGraphNode*> MaskGraphType;

	MaskGraphType maskGraph;
	MaskGraphNode* root;
	MaskGraphNode* sink;

	Function& function;

	bool initialized;
	bool complete;

	//Loop Mask Information
	typedef std::map<const Loop*, LoopNode*> LoopMapType;
	typedef std::map<BasicBlock*, LoopExitNode*> LoopExitMaskMapType;
	LoopMapType loopMap;
	LoopExitMaskMapType loopExitMaskMap;
	const LoopInfo& loopInfo;

	Constant* boolOneConst;
	Constant* boolZeroConst;

	void buildGraph(MaskGraphNode* node) {
		BasicBlock* block = node->getBlock();
		unsigned predNr = 0;
		unsigned succNr = 0;
		//set predecessor nodes
		for (pred_iterator PI=pred_begin(block), PE=pred_end(block); PI!=PE; ++PI) {
			BasicBlock* predBB = *PI;
			MaskGraphNode* predNode = findMaskNode(predBB);
			if (!predNode) {
				predNode = new MaskGraphNode(predBB, verbose);
				node->addPredecessor(predNode);
				//predNode->addSuccessor(node);
				insert(predNode);
				buildGraph(predNode);
			} else {
				node->addPredecessor(predNode);
				//predNode->addSuccessor(node);
			}
			++predNr;
		}

		//set successor nodes
		for (succ_iterator SI=succ_begin(block), SE=succ_end(block); SI!=SE; ++SI) {
			BasicBlock* succBB = *SI;
			MaskGraphNode* succNode = findMaskNode(succBB);
			if (!succNode) {
				succNode = new MaskGraphNode(succBB, verbose);
				node->addSuccessor(succNode);
				//succNode->addPredecessor(node);
				insert(succNode);
				buildGraph(succNode);
			} else {
				node->addSuccessor(succNode);
				//succNode->addPredecessor(node);
			}
			++succNr;
		}

		assert (node->getNumPredecessors() == predNr);
		assert (node->getNumSuccessors() == succNr);
	}
	inline MaskGraphNode* insert(MaskGraphNode* mgn) {
		assert (mgn);
		assert (maskGraph.find(mgn->getBlock()) == maskGraph.end());
		std::pair<MaskGraphType::iterator, bool> resPair = maskGraph.insert(std::make_pair(mgn->getBlock(), mgn));
		assert (resPair.second && "insertion must not fail!");
		assert (resPair.first->second && "insertion must return valid object!");
		return resPair.first->second;
	}

	inline bool checkForCompletion() {
		assert (initialized);
		if (complete) return true;

		if (maskGraph.size() != function.getBasicBlockList().size()) {
			complete = false;
			DEBUG_PKT( errs() << "ERROR: mask graph has wrong size -> incomplete!\n"; );
			return false;
		}
		complete = true;
		for (MaskGraph::const_iterator it=maskGraph.begin(), E=maskGraph.end(); it!=E; ++it) {
			complete &= it->second->complete();
			if (!it->second->complete()) {
				DEBUG_PKT( errs() << "ERROR: node is not complete: "; );
				DEBUG_PKT( it->second->print(outs()); );
			}
		}
		return complete;
	}

	inline void buildLoopMaskGraph() {
		for (LoopInfo::iterator L=loopInfo.begin(), LE=loopInfo.end(); L!=LE; ++L) {
			LoopNode* loopNode = recBuildLoopMaskNode(*L, NULL);
			assert (loopNode);
			assert (loopMap.find(*L) == loopMap.end());
			loopMap.insert(std::make_pair(*L, loopNode));
		}
	}
	LoopNode* recBuildLoopMaskNode(const Loop* loop, LoopNode* parent) {
		LoopNode* loopNode = new LoopNode(loop, parent);
		SmallVector<LoopNode*, 4> subLoopVec;
		for (Loop::iterator SL=loop->begin(), LE=loop->end(); SL!=LE; ++SL) {
			LoopNode* subLoopNode = recBuildLoopMaskNode(*SL, loopNode);
			assert (loopMap.find(*SL) == loopMap.end());
			loopMap.insert(std::make_pair(*SL, subLoopNode));
			subLoopVec.push_back(subLoopNode);
		}
		//add child loops
		for (SmallVector<LoopNode*,4>::const_iterator LN=subLoopVec.begin(),
				LNE=subLoopVec.end(); LN!=LNE; ++LN) {
			loopNode->addNestedLoopNode(*LN);
		}
		//add exits
		SmallVector<BasicBlock*, 4>* exitingBlocks = new SmallVector<BasicBlock*, 4>();
		loop->getExitingBlocks(*exitingBlocks);
		for (SmallVector<BasicBlock*, 4>::const_iterator it=exitingBlocks->begin(), E=exitingBlocks->end(); it!=E;) {
			BasicBlock* exitingBlock = *it++;
			//check if there is already an associated loop exit node
			LoopExitMaskMapType::const_iterator tmp = loopExitMaskMap.find(exitingBlock);
			if (tmp != loopExitMaskMap.end()) {
				//update existing loop exit node
				tmp->second->addLoop(loop);
			} else {
				//create new loop exit node
				loopExitMaskMap.insert(std::make_pair(exitingBlock, new LoopExitNode(exitingBlock, loopInfo)));
			}
			//add info to loop node
			loopNode->addLoopExit(exitingBlock);
		}
		//add entry
		loopNode->addLoopEntry(loop->getHeader(), loop->getLoopPreheader(), loop->getLoopLatch());

		delete exitingBlocks;

		return loopNode;
	}

public:
	MaskGraph(Function& f, const LoopInfo& loopInfos, bool verbose_flag=false)
		: verbose(verbose_flag), function(f), complete(false), loopInfo(loopInfos)
	{
		root = new MaskGraphNode(&f.getEntryBlock(), verbose);
		insert(root);
		buildGraph(root);
		initialized = true;
		sink = findMaskNode(Packetizer::findReturnBlock(f));
		assert (sink);

		if (!loopInfo.empty()) {
			//build loop mask graph
			boolOneConst = Constant::getAllOnesValue(Type::getInt1Ty(getGlobalContext()));
			boolZeroConst = Constant::getNullValue(Type::getInt1Ty(getGlobalContext()));
			buildLoopMaskGraph();
		}
	}

	inline void clear() {
		std::set<MaskValue*> uniqueMaskValues;
		collectUniqueMaskValues(uniqueMaskValues);
		for (std::set<MaskValue*>::iterator it=uniqueMaskValues.begin(),
				E=uniqueMaskValues.end(); it!=E; ++it)
		{
			//outs() << "deleting mask value: "; (*it)->print(outs()); outs() << "...\n";
			delete *it;
		}

		for (MaskGraphType::iterator it=maskGraph.begin(), E=maskGraph.end(); it!=E; ++it) {
			//outs() << "deleting node of block '" << it->second->getBlock()->getName() << "'...\n";
			delete it->second;
		}
		for (LoopMapType::iterator it=loopMap.begin(), E=loopMap.end(); it!=E; ++it) {
			delete it->second;
		}
		for (LoopExitMaskMapType::iterator it=loopExitMaskMap.begin(), E=loopExitMaskMap.end(); it!=E; ++it) {
			delete it->second;
		}

		maskGraph.clear();

		root = NULL;
		sink = NULL;
		initialized = false;
		complete = false;
		nextMaskOpID = 0;

		loopMap.clear();
		loopExitMaskMap.clear();
	}


	void collectUniqueMaskValues(std::set<MaskValue*>& maskValues) {
		for (MaskGraphType::iterator it=maskGraph.begin(), E=maskGraph.end(); it!=E; ++it) {
			MaskGraphNode* node = it->second;
			MaskOperation* entry = node->hasPredecessors() ? node->getEntryMask() : NULL;
			MaskOperation* exitT = node->hasExitEdge() ? node->getExitMaskTrue() : NULL;
			MaskOperation* exitF = node->hasConditionalExit() ? node->getExitMaskFalse() : NULL;
			if (entry && entry->isMaskValue()) maskValues.insert((MaskValue*)entry);
			if (exitT && exitT->isMaskValue()) maskValues.insert((MaskValue*)exitT);
			if (exitF && exitF->isMaskValue()) maskValues.insert((MaskValue*)exitF);
		}
	}

	inline void finalize() {
		//assert (checkForCompletion() && "must not call finalize() on incomplete graph!"); // this is dangerous, checkForCompletion() modifies state of maskGraph!
		checkForCompletion();
		assert (initialized);
		assert (complete);
		assert (verify());
	}

	typedef MaskGraphType::iterator iterator;
	typedef MaskGraphType::const_iterator const_iterator;
	inline iterator begin() { assert (initialized); return maskGraph.begin(); }
	inline iterator end() { assert (initialized); return maskGraph.end(); }
	inline const_iterator begin() const { assert (initialized); return maskGraph.begin(); }
	inline const_iterator end() const { assert (initialized); return maskGraph.end(); }

	inline unsigned size() const { assert (initialized); return maskGraph.size(); }
	inline bool empty() const { assert (initialized); return maskGraph.empty(); }
	inline bool isInitialized() const { return initialized; }

	inline MaskGraphNode* getRoot() const { assert(initialized && root); return root; }
	inline MaskGraphNode* getSink() const { assert(initialized && sink); return sink; }

	inline MaskGraphType::const_iterator find(BasicBlock* block) const {
		assert (initialized && block);
		return maskGraph.find(block);
	}
	// can be called during initialization
	inline MaskGraphNode* findMaskNode(BasicBlock* block) const {
		assert (block);
		MaskGraphType::const_iterator tmp = maskGraph.find(block);
		if (tmp == maskGraph.end()) return NULL;
		return tmp->second;
	}

	inline MaskGraphNode* insert(BasicBlock* block, Value* entryMask) {
		assert (initialized && block);
		MaskOperation* entry = new MaskValue(entryMask);
		MaskGraphNode* mgn = new MaskGraphNode(block, verbose);
		mgn->setEntryMask(entry);
		assert (maskGraph.find(block) == maskGraph.end());
		std::pair<MaskGraphType::iterator, bool> resPair = maskGraph.insert(std::make_pair(block, mgn));
		assert (resPair.second && "insertion must not fail!");
		assert (resPair.first->second && "insertion must return valid object!");
		checkForCompletion();
		return resPair.first->second;
	}
	inline MaskGraphNode* insert(BasicBlock* block, Value* entryMask, Value* exitMask) {
		assert (initialized && block && entryMask && exitMask);
		MaskOperation* entry = new MaskValue(entryMask);
		MaskOperation* exit = new MaskValue(exitMask);
		MaskGraphNode* mgn = new MaskGraphNode(block, entry, exit, verbose);
		assert (maskGraph.find(block) == maskGraph.end());
		std::pair<MaskGraphType::iterator, bool> resPair = maskGraph.insert(std::make_pair(block, mgn));
		assert (resPair.second && "insertion must not fail!");
		assert (resPair.first->second && "insertion must return valid object!");
		checkForCompletion();
		return resPair.first->second;
	}
	inline MaskGraphNode* insert(BasicBlock* block, Value* entryMask, Value* exitMaskTrue, Value* exitMaskFalse) {
		assert (initialized && block && entryMask && exitMaskTrue && exitMaskFalse);
		MaskOperation* entry = new MaskValue(entryMask);
		MaskOperation* exitT = new MaskValue(exitMaskTrue);
		MaskOperation* exitF = new MaskValue(exitMaskFalse);
		MaskGraphNode* mgn = new MaskGraphNode(block, entry, exitT, exitF, verbose);
		assert (maskGraph.find(block) == maskGraph.end());
		std::pair<MaskGraphType::iterator, bool> resPair = maskGraph.insert(std::make_pair(block, mgn));
		assert (resPair.second && "insertion must not fail!");
		assert (resPair.first->second && "insertion must return valid object!");
		checkForCompletion();
		return resPair.first->second;
	}
	inline MaskGraphNode* insert(BasicBlock* block, MaskOperation* entryMask, MaskOperation* exitMask) {
		assert (initialized && block && entryMask && exitMask);
		MaskGraphNode* mgn = new MaskGraphNode(block, entryMask, exitMask, verbose);
		assert (maskGraph.find(block) == maskGraph.end());
		std::pair<MaskGraphType::iterator, bool> resPair = maskGraph.insert(std::make_pair(block, mgn));
		assert (resPair.second && "insertion must not fail!");
		assert (resPair.first->second && "insertion must return valid object!");
		checkForCompletion();
		return resPair.first->second;
	}
	inline MaskGraphNode* insert(BasicBlock* block, MaskOperation* entryMask, MaskOperation* exitMaskTrue, MaskOperation* exitMaskFalse) {
		assert (initialized && block && entryMask && exitMaskTrue && exitMaskFalse);
		MaskGraphNode* mgn = new MaskGraphNode(block, entryMask, exitMaskTrue, exitMaskFalse, verbose);
		assert (maskGraph.find(block) == maskGraph.end());
		std::pair<MaskGraphType::iterator, bool> resPair = maskGraph.insert(std::make_pair(block, mgn));
		assert (resPair.second && "insertion must not fail!");
		assert (resPair.first->second && "insertion must return valid object!");
		checkForCompletion();
		return resPair.first->second;
	}

	inline bool updatePredecessor(BasicBlock* block, BasicBlock* oldPredBB, BasicBlock* newPredBB) {
		assert (initialized && block && newPredBB && oldPredBB);
		MaskGraphType::const_iterator node = find(block);
		if (node == maskGraph.end()) return false;

		MaskGraphType::const_iterator oldPredNode = find(oldPredBB);
		if (oldPredNode == maskGraph.end()) return false;

		MaskGraphType::const_iterator newPredNode = find(newPredBB);
		if (newPredNode == maskGraph.end()) return false;

		return node->second->updatePredecessor(oldPredNode->second, newPredNode->second);
	}
	inline bool addPredecessor(BasicBlock* block, BasicBlock* newPredBB) {
		assert (initialized && block && newPredBB);
		MaskGraphType::const_iterator node = find(block);
		if (node == maskGraph.end()) return false;

		MaskGraphType::const_iterator predNode = find(newPredBB);
		if (predNode == maskGraph.end()) return false;

		node->second->addPredecessor(predNode->second);
		return true;
	}
	inline bool setSuccessorTrue(BasicBlock* block, BasicBlock* newSuccBB) {
		assert (initialized && block && newSuccBB);
		MaskGraphType::const_iterator node = find(block);
		if (node == maskGraph.end()) return false;

		MaskGraphType::const_iterator succNode = find(newSuccBB);
		if (succNode == maskGraph.end()) return false;

		//assert(node->second->hasExitEdge() && "trying to add successor to node that does not have any exit edge!");

		node->second->setSuccessorTrue(succNode->second);
		return true;
	}
	inline bool setSuccessorFalse(BasicBlock* block, BasicBlock* newSuccBB) {
		assert (initialized && block && newSuccBB);
		MaskGraphType::const_iterator node = find(block);
		if (node == maskGraph.end()) return false;

		MaskGraphType::const_iterator succNode = find(newSuccBB);
		if (succNode == maskGraph.end()) return false;

		assert(node->second->hasConditionalExit() && "trying to add false-successor to node that only has one mask associated!");

		node->second->setSuccessorFalse(succNode->second);
		return true;
	}
	inline bool removePredecessor(BasicBlock* block, BasicBlock* oldPredBB) {
		assert (initialized && block && oldPredBB);
		MaskGraphType::const_iterator node = find(block);
		if (node == maskGraph.end()) return false;

		MaskGraphType::const_iterator predNode = find(oldPredBB);
		if (predNode == maskGraph.end()) return false;

		return node->second->removePredecessor(predNode->second);
	}
	inline void setEntryMask(MaskGraphNode* node, MaskOperation* newMask) {
		assert (initialized && node && newMask);
		node->setEntryMask(newMask);
	}
	inline void setExitMaskTrue(MaskGraphNode* node, MaskOperation* newMask) {
		assert (initialized && node && newMask);
		node->setExitMaskTrue(newMask);
	}
	inline void setExitMaskFalse(MaskGraphNode* node, MaskOperation* newMask) {
		assert (initialized && node && newMask);
		node->setExitMaskFalse(newMask);
	}

	inline Value* getEntryMask(BasicBlock* block) const {
		assert (initialized && block);
		assert (!hasCompoundMasks() && "must not ask for explicit mask of type Value* if mask graph still holds compound masks!");
		MaskGraphNode* node = findMaskNode(block);
		assert (node); //if (!node) return NULL;
		assert (node->getEntryMask());
		assert (node->getEntryMask()->isMaskValue());
		assert (typeid(*node->getEntryMask()) == typeid(MaskValue));
		MaskValue* maskOp = static_cast<MaskValue*>(node->getEntryMask());
		assert (maskOp->getMask());
		return maskOp->getMask();
	}
	inline Value* getExitMaskTrue(BasicBlock* block) const {
		assert (initialized && block);
		assert (!hasCompoundMasks() && "must not ask for explicit mask of type Value* if mask graph still holds compound masks!");
		MaskGraphNode* node = findMaskNode(block);
		assert (node); //if (!node) return NULL;
		assert (node->hasExitEdge());
		assert (node->getExitMaskTrue());
		assert (node->getExitMaskTrue()->isMaskValue());
		assert (typeid(*node->getExitMaskTrue()) == typeid(MaskValue));
		MaskValue* maskOp = static_cast<MaskValue*>(node->getExitMaskTrue());
		assert (maskOp->getMask());
		return maskOp->getMask();
	}
	inline Value* getExitMaskFalse(BasicBlock* block) const {
		assert (initialized && block);
		assert (!hasCompoundMasks() && "must not ask for explicit mask of type Value* if mask graph still holds compound masks!");
		MaskGraphNode* node = findMaskNode(block);
		assert (node); //if (!node) return NULL;
		assert (node->hasConditionalExit());
		assert (node->getExitMaskFalse());
		assert (node->getExitMaskFalse()->isMaskValue());
		assert (typeid(*node->getExitMaskFalse()) == typeid(MaskValue));
		MaskValue* maskOp = static_cast<MaskValue*>(node->getExitMaskFalse());
		assert (maskOp->getMask());
		return maskOp->getMask();
	}
	inline Value* getExitMaskInDir(BasicBlock* source, BasicBlock* dir) const {
		assert (initialized && source && dir);
		assert (!hasCompoundMasks() && "must not ask for explicit mask of type Value* if mask graph still holds compound masks!");
		MaskOperation* maskOp = getExitMaskOperationInDir(source, dir);
		assert (maskOp); //if (!maskOp) return NULL;
		assert (maskOp->isMaskValue());
		MaskValue* singleMaskOp = static_cast<MaskValue*>(maskOp);
		assert (singleMaskOp->getMask());
		return singleMaskOp->getMask();
	}

	inline MaskOperation* getExitMaskTrue(MaskGraphNode* source) const {
		assert (initialized && source);
		return source->getExitMaskTrue();
	}
	inline MaskOperation* getExitMaskOperationInDir(MaskGraphNode* source, BasicBlock* dir) const {
		assert (initialized && source && dir);
		return source->getExitMaskInDir(dir);
	}
	inline MaskOperation* getExitMaskOperationInDir(BasicBlock* source, BasicBlock* dir) const {
		assert (initialized && source && dir);
		MaskGraphType::const_iterator node = find(source);
		if (node == maskGraph.end()) {
			//errs() << "ERROR: getExitMaskInDir(): source block '" << source->getNameStr() << "'\n";
			return NULL;
		}

		return node->second->getExitMaskInDir(dir);
	}

	inline void print(raw_ostream& o) const {
		assert (initialized);
		//	assert (initialized && checkForCompletion()); // this is dangerous, checkForCompletion() modifies state of maskGraph!
		for (MaskGraphType::const_iterator it=maskGraph.begin(), E=maskGraph.end(); it!=E; ++it) {
			it->second->print(o);
		}
	}
	inline bool verify() const {
		bool verified = true;

		if (!initialized) {
			errs() << "ERROR: mask graph was not initialized, can not verify!\n";
			return false;
		}

		//if (!complete) {
		//	verified = false;
		//	errs() << "ERROR: graph was not completed! (finalize() not called?)\n";
		//}

		if (maskGraph.size() != function.getBasicBlockList().size()) {
			verified = false;
			errs() << "ERROR: size of mask graph does not match block list size of function!\n";
		}

		for (MaskGraphType::const_iterator it=maskGraph.begin(), E=maskGraph.end(); it!=E; ++it) {
			assert (it->first && "block must not be NULL!");
			assert (it->second && "node must not be NULL!");
			assert (it->first == it->second->getBlock() && "associated blocks have to match!");

			const bool nodeVerified = it->second->verify();
			if (!nodeVerified)
				errs() << "ERROR: verification of node of block '" << it->first->getNameStr() << "' failed!\n";

			verified &= nodeVerified;
		}
		if (!root) {
			verified = false;
			errs() << "ERROR: verification of root of mask graph failed!\n";
		}
		return verified;
	}
	bool hasCompoundMasks() const {
		assert (initialized);
		//assert (checkForCompletion()); // this is dangerous, checkForCompletion() modifies state of maskGraph!
		for (MaskGraphType::const_iterator it=maskGraph.begin(), E=maskGraph.end(); it!=E; ++it) {
			MaskGraphNode* node = it->second;
			assert (node->complete());
			if (node->hasPredecessors()) {
				MaskOperation* entryMaskOp = node->getEntryMask();
				if (!entryMaskOp->isMaskValue()) return true;
			}
			if (node->hasExitEdge()) {
				MaskOperation* exitMaskTrueOp = node->getExitMaskTrue();
				if (!exitMaskTrueOp->isMaskValue()) return true;
			}
			if (node->hasConditionalExit()) {
				MaskOperation* exitMaskFalseOp = node->getExitMaskFalse();
				if (!exitMaskFalseOp->isMaskValue()) return true;
			}
		}
		return false;
	}

	inline void insertMasks(AnalysisResults& ar) {
		assert (initialized);

		// Create empty loop exit mask phis upfront.
		for (LoopExitMaskMapType::iterator LM=loopExitMaskMap.begin(), LME=loopExitMaskMap.end(); LM!=LME; ++LM) {
			LoopExitNode* loopExitNode = LM->second;

			// Ignore uniform loops (do not require exit masks etc.).
			// If the loop is uniform, all its exiting blocks are as well.
			// Therefore, the reverse also holds: if the exiting block is uniform,
			// so is the loop. This tells us that we can ignore it.
			assert (ar.isUniform(loopExitNode->getInnermostLoop()) ==
					ar.hasUniformExit(loopExitNode->getExitingBlock()));

			//if (analysisResults.hasUniformExit(loopExitNode->getExitingBlock()))
			if (ar.isUniform(loopExitNode->getInnermostLoop())) {
				assert (ar.isUniform(loopExitNode->getInnermostLoop()));
				assert (ar.isUniform(loopInfo.getLoopFor(loopExitNode->getExitingBlock())));
				DEBUG_PKT( outs() << "ignoring generation of loop exit mask "
						"phis for uniform loop: " << *loopInfo.getLoopFor(loopExitNode->getExitingBlock()); );
				continue;
			}

			//insert empty exit mask phis into headers of all loops exited by this exit
			for (LoopExitNode::iterator it=loopExitNode->begin(), E=loopExitNode->end(); it!=E; ++it) {
				const Loop* loop = it->first;

				PHINode* phi = PHINode::Create(
						Type::getInt1Ty(getGlobalContext()),
						"loop.exit.mask.phi",
						loop->getHeader()->getFirstNonPHI());

				// We mark the phi as UNIFORM before completion:
				// After generating the operands, we derive the information again
				// and possibly overwrite it if VARYING is returned.
				// Other possibility: If it is actually used later-on, it has to be
				// VARYING (otherwise, no loop mask is required).
				// NOTE: Use the latter - the first option made problems with
				//       derived values being marked uniform before the phi was
				//       updated to VARYING, resulting in wrong information.
				ar.addValueInfo(phi,
					AnalysisResults::VARYING,
					AnalysisResults::INDEX_RANDOM,
					AnalysisResults::ALIGN_FALSE,
					AnalysisResults::SPLIT_NEVER, // should never be split
					true); // mask = true
				DEBUG_PKT( outs() << "New loop exit mask phi is marked as VARYING (insertMasks)\n"; );

				//break circular dependencies of loop exit masks
				//update loop exit graph
				loopExitNode->setMaskPhiOp(loop, new MaskValue(phi));

				// If this is the innermost loop, set the phi as second operand in the exit mask update operation
				// NOTE: This innermost loop is always the correct one because
				//       it is relative to the exiting block, not to the entire
				//       loop nest. We do not need to distinguish the innermost
				//       varying loop here.
				if (loopExitNode->isInnermostLoop(loop)) {
					MaskOperation* exitMaskUpdateOp = loopExitNode->getMaskUpdateOp();
					assert (exitMaskUpdateOp->isMaskDisjunction());
					MaskDisjunction* exitMaskUpdateDisOp = static_cast<MaskDisjunction*>(exitMaskUpdateOp);
					exitMaskUpdateDisOp->setMaskRight(new MaskValue(phi)); //is right operand by construction
				}
			}
		}

		// recursively create "normal" mask operations
		assert (sink);
		//sink->createEntryMask(analysisResults); // should not be necessary in addition to recInsertMasks
		// Recursion over node graph only is not enough due to FULLY_UNIFORM blocks
		// (entry mask = true). We need to iterate over the blocks' predecessors.
		std::set<MaskGraphNode*> visitedSet;
		recInsertMasks(sink, ar, visitedSet);

		// set update operation of each exit mask to just generated exit mask value
		for (LoopExitMaskMapType::iterator LM=loopExitMaskMap.begin(), LME=loopExitMaskMap.end(); LM!=LME; ++LM) {
			LoopExitNode* loopExitNode = LM->second;
			BasicBlock* exitingBlock = LM->first;

			// Just like above, ignore uniform loops
			if (ar.isUniform(loopExitNode->getInnermostLoop())) {
				assert (ar.isUniform(loopExitNode->getInnermostLoop()));
				assert (ar.isUniform(loopInfo.getLoopFor(loopExitNode->getExitingBlock())));
				DEBUG_PKT( outs() << "ignoring update of loop exit mask "
						"phis for uniform loop: " << *loopInfo.getLoopFor(loopExitNode->getExitingBlock()); );
				continue; // TODO: HERE! This was missing before, but test suite etc. were working!
			}

			MaskGraphNode* node = findMaskNode(exitingBlock);
			assert (node);
			MaskOperation* exitMaskOp = node->getExitMaskInDir(loopExitNode->getExitBlock());
			assert (exitMaskOp);
			loopExitNode->setMaskUpdateOp(exitMaskOp);
		}

		// add incoming values to loop exit mask phis
		insertLoopExitMasks(ar);
	}

	void recInsertMasks(MaskGraphNode* node,
						AnalysisResults& analysisResults,
						std::set<MaskGraphNode*>& visitedSet)
	{
		if (visitedSet.find(node) != visitedSet.end()) return;
		visitedSet.insert(node);

		// first recurse into predecessors (if any)
		if (node->getNumPredecessors() > 0)
			recInsertMasks(node->getPredecessor(0), analysisResults, visitedSet);
		if (node->getNumPredecessors() > 1)
			recInsertMasks(node->getPredecessor(1), analysisResults, visitedSet);

		DEBUG_PKT ( outs() << "inserting entry mask for node: " << node->getBlock()->getName() << "\n"; );

		// all references should be computed by now
		node->createEntryMask(analysisResults);

		// Make sure we create ALL masks.
		// TODO: This is not really elegant and could have plenty of overhead.
		if (node->getNumSuccessors() > 0) node->createExitMaskTrue(analysisResults);
		if (node->getNumSuccessors() > 1) node->createExitMaskFalse(analysisResults);
	}

	inline void insertLoopExitMasks(AnalysisResults& ar) {
		assert (initialized);
		for (LoopExitMaskMapType::iterator LM=loopExitMaskMap.begin(), LME=loopExitMaskMap.end(); LM!=LME; ++LM) {
			LoopExitNode* loopExitNode = LM->second;

			// Just like in 'insertMasks()', ignore uniform loops
			if (ar.isUniform(loopExitNode->getInnermostLoop())) {
				assert (ar.isUniform(loopExitNode->getInnermostLoop()));
				assert (ar.isUniform(loopInfo.getLoopFor(loopExitNode->getExitingBlock())));
				DEBUG_PKT( outs() << "\nignoring update of loop exit mask "
						"phis for uniform loop: " << *loopInfo.getLoopFor(loopExitNode->getExitingBlock()); );
				continue;
			}


			DEBUG_PKT ( outs() << "\ninserting loop exit mask for node:\n"; loopExitNode->print(outs()); );

			//get generated mask update instruction
			MaskOperation* maskUpdateOp = loopExitNode->getMaskUpdateOp();
			assert (maskUpdateOp->isMaskValue());
			Value* maskUpdateInstr = static_cast<MaskValue*>(maskUpdateOp)->getMask();

			//wire all exit mask phis
			//incoming value from preheader is:
			// - for single-loop exit (~break): zero-vector (no instance has left the loop through this exit yet)
			// - for multi-loop exit (~break n/return) if in parent: zero-vector (no instance has left the loop through this exit yet)
			// - for multi-loop exit (~break n/return) if in nested loop: corresponding exit mask phi of parent loop
			//incoming value from latch is:
			// - for single-loop exit (~break): new 'or' of loop's mask phi and the block's old exit mask ((negated) branch condition)
			// - for multi-loop exit (~break n/return) if in parent: the innermost loop's 'or'-operation
			// - for multi-loop exit (~break n/return) if in innermost loop: new 'or' of loop's mask phi and the block's old exit mask ((negated) branch condition)
			for (LoopExitNode::iterator it=loopExitNode->begin(), E=loopExitNode->end(); it!=E; ++it) {
				const Loop* loop = it->first;
				MaskOperation* maskPhiOp = it->second;
				assert (maskPhiOp->isMaskValue());
				assert (isa<PHINode>(static_cast<MaskValue*>(maskPhiOp)->getMask()));
				PHINode* phi = cast<PHINode>(static_cast<MaskValue*>(maskPhiOp)->getMask());

				LoopNode* loopNode = findLoopNode(loop);

				const bool isTopLevelLoop = loopNode->isTopLevelLoop();
				const bool exitsMultipleLoops = loopExitNode->exitsMultipleLoops();

				BasicBlock* preheaderBB = loop->getLoopPreheader();
				BasicBlock* latchBB = loop->getLoopLatch();

				//wire preheader
				if (exitsMultipleLoops && !isTopLevelLoop) {
					const Loop* parentLoop = loopNode->getParentLoopNode()->getLoop();
					MaskOperation* parentMaskPhiOp = loopExitNode->getMaskPhiOp(parentLoop);
					assert (parentMaskPhiOp->isMaskValue());
					Value* parentMaskPhi = static_cast<MaskValue*>(parentMaskPhiOp)->getMask();
					phi->addIncoming(parentMaskPhi, preheaderBB);
				} else {
					phi->addIncoming(Constant::getNullValue(Type::getInt1Ty(getGlobalContext())), preheaderBB);
				}

				//wire latch
				phi->addIncoming(maskUpdateInstr, latchBB);

				// update uniform info if required
				AnalysisResults::UniformInfo ui =
						ar.joinUniformInfo(phi->getIncomingValue(0), maskUpdateInstr);
				ar.setUniformInfo(phi, ui);
				DEBUG_PKT( outs() << "Loop exit mask phi is updated to "
						<< AnalysisResults::getUniformInfoString(ui)
						<< ": " << *phi << " (insertLoopExitMasks)\n"; );
			}

			//be happy
		}
	}


	//------------------------------------------------------------------------//
	//Loop Info stuff
	//------------------------------------------------------------------------//
	inline LoopNode* findLoopNode(const Loop* loop) const {
		assert (initialized);
		assert (!loopMap.empty() && "loop exit mask map is empty!");
		LoopMapType::const_iterator tmp = loopMap.find(loop);
		if (tmp == loopMap.end()) return NULL;
		return tmp->second;
	}
	inline bool isTopLevelLoop(const Loop* loop) const {
		assert (initialized);
		LoopMapType::const_iterator tmp = loopMap.find(loop);
		assert (tmp != loopMap.end() && "isTopLevelLoop() requested for unknown loop!");
		const bool isNormalTopLevelLoop = tmp->second->isTopLevelLoop();
		return isNormalTopLevelLoop;
	}
	inline bool hasNestedLoop(const Loop* loop) const {
		assert (initialized);
		LoopMapType::const_iterator tmp = loopMap.find(loop);
		assert (tmp != loopMap.end() && "hasNestedLoop() requested for unknown loop!");
		return tmp->second->hasNestedLoop();
	}
	inline bool hasMultipleExits(const Loop* loop) const {
		assert (initialized);
		LoopMapType::const_iterator tmp = loopMap.find(loop);
		assert (tmp != loopMap.end() && "hasMultipleExits() requested for unknown loop!");
		return tmp->second->hasMultipleExits();
	}

	//Loop Exit Info stuff
	inline LoopExitNode* findLoopExitNode(BasicBlock* exitingBlock) const {
		assert (initialized);
		assert (!loopExitMaskMap.empty() && "loop exit mask map is empty!");
		LoopExitMaskMapType::const_iterator tmp = loopExitMaskMap.find(exitingBlock);
		if (tmp == loopExitMaskMap.end()) return NULL;
		return tmp->second;
	}
	inline bool isInnermostLoopOfExit(BasicBlock* exitingBlock, const Loop* loop) const {
		assert (initialized);
		LoopExitMaskMapType::const_iterator tmp = loopExitMaskMap.find(exitingBlock);
		assert (tmp != loopExitMaskMap.end());
		return tmp->second->isInnermostLoop(loop);
	}
	inline bool exitsMultipleLoops(BasicBlock* exitingBlock) const {
		assert (initialized);
		LoopExitMaskMapType::const_iterator tmp = loopExitMaskMap.find(exitingBlock);
		assert (tmp != loopExitMaskMap.end());
		return tmp->second->exitsMultipleLoops();
	}
	inline bool getInnermostLoopOfExit(BasicBlock* exitingBlock) const {
		assert (initialized);
		LoopExitMaskMapType::const_iterator tmp = loopExitMaskMap.find(exitingBlock);
		assert (tmp != loopExitMaskMap.end());
		return tmp->second->getInnermostLoop();
	}

	//returns the mask operations inside 'loop' that are related to the exit 'exitingBlock'
	//NOTE: loop can be any loop that is exited by this exit
	inline MaskOperation* getLoopExitMaskPhiOp(BasicBlock* exitingBlock, const Loop* loop) const {
		assert (initialized);
		assert (!loopExitMaskMap.empty() && "loop exit mask map is empty!");
		LoopExitMaskMapType::const_iterator tmp = loopExitMaskMap.find(exitingBlock);
		assert (tmp != loopExitMaskMap.end());
		return tmp->second->getMaskPhiOp(loop);
	}
	inline MaskOperation* getLoopExitMaskUpdateOp(BasicBlock* exitingBlock) const {
		assert (initialized);
		assert (!loopExitMaskMap.empty() && "loop exit mask map is empty!");
		LoopExitMaskMapType::const_iterator tmp = loopExitMaskMap.find(exitingBlock);
		assert (tmp != loopExitMaskMap.end());
		return tmp->second->getMaskUpdateOp();
	}

	inline void setLoopExitMaskPhi(BasicBlock* exitingBlock, const Loop* loop, MaskOperation* maskPhi) {
		assert (initialized);
		assert (exitingBlock && loop && maskPhi);
		assert (!loopExitMaskMap.empty() && "loop exit mask map is empty!");
		LoopExitMaskMapType::const_iterator tmp = loopExitMaskMap.find(exitingBlock);
		assert (tmp != loopExitMaskMap.end());
		tmp->second->setMaskPhiOp(loop, maskPhi);
	}
	inline void setLoopExitMaskUpdateOp(BasicBlock* exitingBlock, MaskOperation* maskUpdateOp) {
		assert (initialized);
		assert (!loopExitMaskMap.empty() && "loop exit mask map is empty!");
		LoopExitMaskMapType::const_iterator tmp = loopExitMaskMap.find(exitingBlock);
		assert (tmp != loopExitMaskMap.end());
		tmp->second->setMaskUpdateOp(maskUpdateOp);
	}

	inline bool verifyLoopMasks(AnalysisResults& ar) const {
		assert (initialized);
		for (LoopMapType::const_iterator LN=loopMap.begin(), LNE=loopMap.end(); LN!=LNE; ++LN) {
			const bool verified = LN->second->verify();
			if (!verified) {
				errs() << "ERROR: verification of loop node failed!\n";
				errs() << "  Loop: "; LN->first->print(errs());
				return false;
			}
		}
		for (LoopExitMaskMapType::const_iterator LN=loopExitMaskMap.begin(), LNE=loopExitMaskMap.end(); LN!=LNE; ++LN) {
			const bool verified = LN->second->verify(ar);
			if (!verified) {
				errs() << "ERROR: verification of loop exit node failed!\n";
				errs() << "  Loop: "; LN->first->print(errs());
				return false;
			}
		}
		return true;
	}
	inline void printLoopMap(raw_ostream& o) const {
		assert (initialized);
		o << "\nLoop Map:\n";
		//recursively print loop hierarchy
		for (LoopInfo::iterator it=loopInfo.begin(), E=loopInfo.end(); it!=E; ++it) {
			assert (findLoopNode(*it) && "loop node must not be NULL!");
			findLoopNode(*it)->print(o);
		}
	}
	inline void printLoopExitMap(raw_ostream& o) const {
		assert (initialized);
		o << "\nLoop Exit Mask Map:\n";
		for (LoopExitMaskMapType::const_iterator it=loopExitMaskMap.begin(), E=loopExitMaskMap.end(); it!=E; ++it) {
			it->second->print(o);
		}
	}

};


//this constructor requires full class definition of MaskGraphNode
//and thus needs to be extracted from rest of MaskNodeReference's class definition
MaskNodeReference::MaskNodeReference(MaskGraphNode* n, MaskGraphNode* dir) : MaskOperation(), node(n) {
	//determine reference direction
	assert (n);
	assert (dir);
	if (node == dir) maskRef = ENTRY;
	else if (node->hasExitEdge() && node->getSuccessorTrue() == dir) maskRef = EXITTRUE;
	else if (node->hasConditionalExit() && node->getSuccessorFalse() == dir) maskRef = EXITFALSE;
	else {
		assert (!"could not determine correct mask reference direction!");
		maskRef = NONE;
	}
}
void MaskNodeReference::print(raw_ostream& o) const {
	o << getID() << ": REF( ";
	if (referencesEntryMask()) o << "entry (" << node->getBlock()->getNameStr() << ")";
	else if (referencesExitMaskTrue()) o << "exit true (" << node->getBlock()->getNameStr() << ")";
	else if (referencesExitMaskFalse()) o << "exit false (" << node->getBlock()->getNameStr() << ")";
	else if (referencesLoopExitPhi()) o << "loop exit phi (" << node->getBlock()->getNameStr() << ")";
	else o << "none (" << node->getBlock()->getNameStr() << ")";
	o << " )";
}

}

#endif	/* _MASKGRAPH_HPP */

