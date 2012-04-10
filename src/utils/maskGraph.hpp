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
	bool mVerbose;
	BasicBlock* mBlock;
	MaskOperation* mEntryMask;
	MaskOperation* mExitMaskTrue;
	MaskOperation* mExitMaskFalse;

	typedef SmallVector<MaskGraphNode*, 2> PredVectorType;
	PredVectorType mPreds;

	MaskGraphNode* mSuccTrue;
	MaskGraphNode* mSuccFalse;

	Constant* mConstBoolTrue;
	Constant* mConstBoolFalse;
	unsigned mMaskOperationCounter; //TODO: use!

	inline void init(LLVMContext& context) {
		mMaskOperationCounter = 0;
		mConstBoolTrue = Constant::getAllOnesValue(Type::getInt1Ty(context));
		mConstBoolFalse = Constant::getNullValue(Type::getInt1Ty(context));
	}

	Value* createMaskAnd(Value* mask1, Value* mask2, const std::string& name,
			Instruction* insertBefore, AnalysisResults& analysisResults)
	{
		assert (mask1->getType() == Type::getInt1Ty(mask1->getContext()) && "trying to create bit-operation on non-boolean type!");
		assert (mask2->getType() == Type::getInt1Ty(mask1->getContext()) && "trying to create bit-operation on non-boolean type!");
		if (mask1 == mConstBoolFalse) return mConstBoolFalse;
		if (mask2 == mConstBoolFalse) return mConstBoolFalse;
		if (mask1 == mConstBoolTrue && mask2 == mConstBoolTrue) return mConstBoolTrue;
		if (mask1 == mConstBoolTrue) return mask2;
		if (mask2 == mConstBoolTrue) return mask1;
		if (mask1 == mask2) return mask1;

		++mMaskOperationCounter;
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
		assert (mask1->getType() == Type::getInt1Ty(mask1->getContext()) && "trying to create bit-operation on non-boolean type!");
		assert (mask2->getType() == Type::getInt1Ty(mask1->getContext()) && "trying to create bit-operation on non-boolean type!");
		if (mask1 == mConstBoolTrue) return mConstBoolTrue;
		if (mask2 == mConstBoolTrue) return mConstBoolTrue;
		if (mask1 == mConstBoolFalse && mask2 == mConstBoolFalse) return mConstBoolFalse;
		if (mask1 == mConstBoolFalse) return mask1;
		if (mask2 == mConstBoolFalse) return mask2;
		if (mask1 == mask2) return mask1;

		++mMaskOperationCounter;
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
		assert (mask->getType() == Type::getInt1Ty(mask->getContext()) && "trying to create bit-operation on non-boolean type!");
		if (mask == mConstBoolTrue) return mConstBoolFalse;
		if (mask == mConstBoolFalse) return mConstBoolTrue;
		if (Instruction* maskInstr = dyn_cast<Instruction>(mask)) {
			if (maskInstr->isBinaryOp(Instruction::Xor)) {
				Value* op0 = maskInstr->getOperand(0);
				Value* op1 = maskInstr->getOperand(1);
				if (op0 == mConstBoolTrue) {
					return op1; //found not, return op
				}
				if (op1 == mConstBoolTrue) {
					return op0; //found not, return op
				}
				if (op0 == op1) {
					return mConstBoolTrue; //found 'zero', return 'one'
				}
			}
		}

		++mMaskOperationCounter;
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
	MaskGraphNode(BasicBlock* b, LLVMContext& context, bool verbose_flag=false)
		: mVerbose(verbose_flag), mBlock(b), mEntryMask(NULL), mExitMaskTrue(NULL), mExitMaskFalse(NULL),
		mSuccTrue(NULL), mSuccFalse(NULL)
	{ assert(b); init(context); }
	MaskGraphNode(BasicBlock* b, MaskOperation* em, MaskOperation* exm, LLVMContext& context, bool verbose_flag=false)
		: mVerbose(verbose_flag), mBlock(b), mEntryMask(em), mExitMaskTrue(exm), mExitMaskFalse(NULL),
		mSuccTrue(NULL), mSuccFalse(NULL)
	{ assert(b && em && exm); init(context); }
	MaskGraphNode(BasicBlock* b, MaskOperation* em, MaskOperation* exmT, MaskOperation* exmF, LLVMContext& context, bool verbose_flag=false)
		: mVerbose(verbose_flag), mBlock(b), mEntryMask(em), mExitMaskTrue(exmT), mExitMaskFalse(exmF),
		mSuccTrue(NULL), mSuccFalse(NULL)
	{ assert(b && em && exmT && exmF); init(context); }
	MaskGraphNode(BasicBlock* b, Value* em, Value* exm, LLVMContext& context, bool verbose_flag=false)
		: mVerbose(verbose_flag), mBlock(b), mEntryMask(new MaskValue(em)), mExitMaskTrue(new MaskValue(exm)),
		mExitMaskFalse(NULL), mSuccTrue(NULL), mSuccFalse(NULL)
	{ assert(b && em && exm); init(context); }
	MaskGraphNode(BasicBlock* b, Value* em, Value* exmT, Value* exmF, LLVMContext& context, bool verbose_flag=false)
		: mVerbose(verbose_flag), mBlock(b), mEntryMask(new MaskValue(em)), mExitMaskTrue(new MaskValue(exmT)),
		mExitMaskFalse(new MaskValue(exmF)), mSuccTrue(NULL), mSuccFalse(NULL)
	{ assert(b && em && exmT && exmF); init(context); }

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
				assert (mnr->getNode()->getEntryMask() == mEntryMask);
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
		assert (mEntryMask);
		assert (typeid(*mEntryMask) == typeid(MaskValue) ||
				typeid(*mEntryMask) == typeid(MaskDisjunction) ||
				typeid(*mEntryMask) == typeid(MaskPhiOperation) ||
				typeid(*mEntryMask) == typeid(MaskNodeReference));

		DEBUG_PKT ( outs() << "createEntryMask() for node: "; print(outs()); );

		Instruction* pos = mBlock->getFirstNonPHI();
		Value* mask = NULL;

		if (mEntryMask->isMaskValue()) {
			assert (typeid(*mEntryMask) == typeid(MaskValue));

			mask = static_cast<MaskValue*>(mEntryMask)->getMask();

//			//make sure we don't miss any blocks if mask is 'true'
//			//if we encounter a phi instruction, this means we followed a backedge
//			if (!isa<PHINode>(mask) && hasPredecessors()) {
//				for (unsigned i=0, e=getNumPredecessors(); i<e; ++i) {
//					MaskGraphNode* predNode = getPredecessor(i);
//					predNode->createExitMaskInDir(this, ar);
//				}
//			}
		}
		else if (mEntryMask->isMaskDisjunction()) {
			assert (mPreds[0] && mPreds[1]);
			assert (typeid(*mEntryMask) == typeid(MaskDisjunction));
			MaskDisjunction* md = static_cast<MaskDisjunction*>(mEntryMask);
			Value* maskLeft = createMask(md->getMaskLeft(), pos, ar);
			Value* maskRight = createMask(md->getMaskRight(), pos, ar);
			mask = createMaskOr(maskLeft, maskRight, "", pos, ar);
		}
		else if (mEntryMask->isMaskPhi()) {
			assert (mPreds[0] && mPreds[1]);
			assert (typeid(*mEntryMask) == typeid(MaskPhiOperation));
			MaskPhiOperation* mpo = static_cast<MaskPhiOperation*>(mEntryMask);
			assert (mpo->getNumIncomingMasks() == 2);

			PHINode* phi = PHINode::Create(
					Type::getInt1Ty(pos->getContext()),
					2U,
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
				ar.isUniformLoopBlock(mBlock) ?
					AnalysisResults::UNIFORM :
					AnalysisResults::VARYING,
				ar.isUniformLoopBlock(mBlock) ?
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
			mEntryMask = new MaskValue(phi);

			Value* maskLeft = createMask(mpo->getIncomingMask(0), pos, ar);
			Value* maskRight = createMask(mpo->getIncomingMask(1), pos, ar);

			phi->addIncoming(maskLeft, mpo->getIncomingBlock(0));
			phi->addIncoming(maskRight, mpo->getIncomingBlock(1));

			ar.setUniformInfo(phi, ar.joinUniformInfo(maskLeft, maskRight));
			//DEBUG_PKT( outs() << "Loop mask phi is updated to "
					//<< AnalysisResults::getUniformInfoString(ar.getValueInfo(phi)->uniformInfo)
					//<< ": " << *phi << " (createEntryMask)\n"; );

			++mMaskOperationCounter;
			mask = phi;
		}
		else if (mEntryMask->isMaskNodeRef()) {
			assert (mPreds[0]);
			assert (typeid(*mEntryMask) == typeid(MaskNodeReference));
			MaskNodeReference* mnr = static_cast<MaskNodeReference*>(mEntryMask);
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
		mEntryMask = new MaskValue(mask);

		return mask;
	}

	inline Value* createExitMaskTrue(AnalysisResults& ar) {
		assert (mExitMaskTrue);
		assert (typeid(*mExitMaskTrue) != typeid(MaskPhiOperation));
		
		DEBUG_PKT ( outs() << "createExitMaskTrue() for node: "; print(outs()); );

		Instruction* pos = mBlock->getTerminator();
		Value* mask = NULL;

		if (mExitMaskTrue->isMaskValue()) {
			assert (typeid(*mExitMaskTrue) == typeid(MaskValue));
			mask = static_cast<MaskValue*>(mExitMaskTrue)->getMask();
		}
		else if (mExitMaskTrue->isMaskNegation()) {
			assert (typeid(*mExitMaskTrue) == typeid(MaskNegation));
			MaskNegation* mn = static_cast<MaskNegation*>(mExitMaskTrue);
			mask = createMaskNot(createMask(mn->getMask(), pos, ar), "", pos, ar);
		}
		else if (mExitMaskTrue->isMaskConjunction()) {
			assert (typeid(*mExitMaskTrue) == typeid(MaskConjunction));
			MaskConjunction* mc = static_cast<MaskConjunction*>(mExitMaskTrue);
			Value* maskLeft = createMask(mc->getMaskLeft(), pos, ar);
			Value* maskRight = createMask(mc->getMaskRight(), pos, ar);
			mask = createMaskAnd(maskLeft, maskRight, "", pos, ar);
		}
		else if (mExitMaskTrue->isMaskDisjunction()) {
			assert (typeid(*mExitMaskTrue) == typeid(MaskDisjunction));
			MaskDisjunction* md = static_cast<MaskDisjunction*>(mExitMaskTrue);
			Value* maskLeft = createMask(md->getMaskLeft(), pos, ar);
			Value* maskRight = createMask(md->getMaskRight(), pos, ar);
			mask = createMaskOr(maskLeft, maskRight, "", pos, ar);
		}
		else if (mExitMaskTrue->isMaskNodeRef()) {
			assert (typeid(*mExitMaskTrue) == typeid(MaskNodeReference));
			assert (static_cast<MaskNodeReference*>(mExitMaskTrue)->getNode());
			assert (static_cast<MaskNodeReference*>(mExitMaskTrue)->referencesEntryMask() && "exit mask can only reference to entry mask of same block!");
			assert (static_cast<MaskNodeReference*>(mExitMaskTrue)->getNode() == this); //exit mask can only reference to entry mask of same block
			assert (static_cast<MaskNodeReference*>(mExitMaskTrue)->getNode()->getEntryMask() == mEntryMask);
			mask = createEntryMask(ar);
		}
		else assert (!"bad mask operation found!");

		assert (mask);

		//store generated value in map
		//if (exitMaskTrue) delete exitMaskTrue;
		mExitMaskTrue = new MaskValue(mask);

		return mask;
	}

	inline Value* createExitMaskFalse(AnalysisResults& ar) {
		assert (mExitMaskFalse);
		assert (typeid(*mExitMaskFalse) != typeid(MaskPhiOperation));

		DEBUG_PKT ( outs() << "createExitMaskFalse() for node: "; print(outs()); );

		Instruction* pos = mBlock->getTerminator();
		Value* mask = NULL;

		if (mExitMaskFalse->isMaskValue()) {
			assert (typeid(*mExitMaskFalse) == typeid(MaskValue));
			mask = static_cast<MaskValue*>(mExitMaskFalse)->getMask();
		}
		else if (mExitMaskFalse->isMaskNegation()) {
			assert (typeid(*mExitMaskFalse) == typeid(MaskNegation));
			MaskNegation* mn = static_cast<MaskNegation*>(mExitMaskFalse);
			mask = createMaskNot(createMask(mn->getMask(), pos, ar), "", pos, ar);
		}
		else if (mExitMaskFalse->isMaskConjunction()) {
			assert (typeid(*mExitMaskFalse) == typeid(MaskConjunction));
			MaskConjunction* mc = static_cast<MaskConjunction*>(mExitMaskFalse);
			Value* maskLeft = createMask(mc->getMaskLeft(), pos, ar);
			Value* maskRight = createMask(mc->getMaskRight(), pos, ar);
			mask = createMaskAnd(maskLeft, maskRight, "", pos, ar);
		}
		else if (mExitMaskFalse->isMaskDisjunction()) {
			assert (typeid(*mExitMaskFalse) == typeid(MaskDisjunction));
			MaskDisjunction* md = static_cast<MaskDisjunction*>(mExitMaskFalse);
			Value* maskLeft = createMask(md->getMaskLeft(), pos, ar);
			Value* maskRight = createMask(md->getMaskRight(), pos, ar);
			mask = createMaskOr(maskLeft, maskRight, "", pos, ar);
		}
		else if (mExitMaskFalse->isMaskNodeRef()) {
			assert (typeid(*mExitMaskFalse) == typeid(MaskNodeReference));
			assert (static_cast<MaskNodeReference*>(mExitMaskFalse)->getNode());
			assert (static_cast<MaskNodeReference*>(mExitMaskFalse)->referencesEntryMask() && "exit mask can only reference to entry mask of same block!");
			assert (static_cast<MaskNodeReference*>(mExitMaskFalse)->getNode() == this); //exit mask can only reference to entry mask of same block
			assert (static_cast<MaskNodeReference*>(mExitMaskFalse)->getNode()->getEntryMask() == mEntryMask);
			mask = createEntryMask(ar);
		}
		else assert (!"bad mask operation found!");

		assert (mask);

		//store generated value in map
		//if (exitMaskFalse) delete exitMaskFalse;
		mExitMaskFalse = new MaskValue(mask);

		return mask;
	}

	inline Value* createExitMaskInDir(const MaskGraphNode* node, AnalysisResults& ar) {
		assert (node);
		if (mSuccTrue && mSuccTrue == node) return createExitMaskTrue(ar);
		if (mSuccFalse && mSuccFalse == node) return createExitMaskFalse(ar);
		assert (!"could not find successor in this direction!");
		return NULL;
	}

	typedef PredVectorType::iterator pred_iterator;
	typedef PredVectorType::const_iterator pred_const_iterator;
	pred_iterator pred_begin() { return mPreds.begin(); }
	pred_iterator pred_end() { return mPreds.end(); }
	pred_const_iterator pred_begin() const { return mPreds.begin(); }
	pred_const_iterator pred_end() const { return mPreds.end(); }

	inline void setPredecessor(MaskGraphNode* node, unsigned index) {
		assert (node);
		assert (index < mPreds.size() && "wrong index specified!");
		mPreds[index] = node;
	}
	inline bool updatePredecessor(MaskGraphNode* oldNode, MaskGraphNode* newNode) {
		assert (oldNode && newNode);
		assert (isPredecessor(oldNode));
		for (SmallVector<MaskGraphNode*, 2>::iterator it=mPreds.begin();
				it!=mPreds.end(); ++it) {
			if (oldNode == *it) {
				*it = newNode;
				return true;
			}
		}
		return false;
	}
	inline void addPredecessor(MaskGraphNode* node) {
		assert (node);
		for (PredVectorType::iterator it=mPreds.begin(), E=mPreds.end(); it!=E; ++it) {
			if (node == *it) return; //predecessor already exists
		}
		mPreds.push_back(node);
	}
	inline bool addSuccessor(MaskGraphNode* node) {
		//this method automatically derives which successor the node is
		assert (node);
		assert (node->getBlock());
		assert (isa<BranchInst>(mBlock->getTerminator()));
		if (node == mSuccTrue || node == mSuccFalse) return false;

		if (isTrueSuccessor(node)) {
			assert (!mSuccTrue);
			setSuccessorTrue(node);
			return true;
		}
		if (isFalseSuccessor(node)) {
			assert (!mSuccFalse);
			setSuccessorFalse(node);
			return true;
		}
		return false;
	}
	inline void setSuccessorTrue(MaskGraphNode* node) {
		assert (node);
		mSuccTrue = node;
	}
	inline void setSuccessorFalse(MaskGraphNode* node) {
		assert (node);
		mSuccFalse = node;
	}

	inline void removePredecessor(unsigned index) {
		assert (index < mPreds.size() && "wrong index specified!");
		PredVectorType::iterator it=mPreds.begin()+index;
		assert (it != mPreds.end());
		assert (*it == mPreds[index]);
		mPreds.erase(it);
	}
	inline bool removePredecessor(MaskGraphNode* pred) {
		assert (pred);
		for (PredVectorType::iterator it=mPreds.begin(), E=mPreds.end(); it!=E; ++it) {
			if (pred == *it) {
				mPreds.erase(it);
				return true;
			}
		}
		return false;
	}
	inline void removeSuccessorTrue() {
		mSuccTrue = NULL;
	}
	inline void removeSuccessorFalse() {
		mSuccFalse = NULL;
	}

	inline bool isTrueSuccessor(MaskGraphNode* node) const {
		assert (node);
		assert (mBlock);
		assert (isa<BranchInst>(mBlock->getTerminator()));
		BranchInst* br = cast<BranchInst>(mBlock->getTerminator());
		return br->getSuccessor(0) == node->getBlock();
	}
	inline bool isFalseSuccessor(MaskGraphNode* node) const {
		assert (node);
		assert (mBlock);
		assert (isa<BranchInst>(mBlock->getTerminator()));
		BranchInst* br = cast<BranchInst>(mBlock->getTerminator());
		if (br->isUnconditional()) return false;
		return br->getSuccessor(1) == node->getBlock();
	}

	inline BasicBlock* getBlock() const { assert (mBlock); return mBlock; }

	inline MaskOperation* getEntryMask() const { assert (mEntryMask); return mEntryMask; }
	inline MaskOperation* getExitMaskTrue() const { assert (mExitMaskTrue); return mExitMaskTrue; }
	inline MaskOperation* getExitMaskFalse() const {
		assert (hasConditionalExit() && "getExitMaskFalse() called for mask-map-entry that only has one exit mask!");
		assert (mExitMaskFalse);
		return mExitMaskFalse;
	}

	inline Value* getEntryMaskVal() const {
		assert (mEntryMask);
		assert (mEntryMask->isMaskValue() && "must not ask for explicit mask of type Value* for compound mask!");
		MaskValue* maskOp = static_cast<MaskValue*>(mEntryMask);
		assert (maskOp->getMask());
		return maskOp->getMask();
	}
	inline Value* getExitMaskTrueVal() const {
		assert (mExitMaskTrue);
		assert (mExitMaskTrue->isMaskValue() && "must not ask for explicit mask of type Value* for compound mask!");
		MaskValue* maskOp = static_cast<MaskValue*>(mExitMaskTrue);
		assert (maskOp->getMask());
		return maskOp->getMask();
	}
	inline Value* getExitMaskFalseVal() const {
		assert (mExitMaskFalse);
		assert (mExitMaskFalse->isMaskValue() && "must not ask for explicit mask of type Value* for compound mask!");
		MaskValue* maskOp = static_cast<MaskValue*>(mExitMaskFalse);
		assert (maskOp->getMask());
		return maskOp->getMask();
	}

	inline MaskGraphNode* getSinglePredecessor() const {
		assert (hasSinglePredecessor() && "node has more than one predecessor!");
		assert (mPreds[0]);
		return mPreds[0];
	}
	inline unsigned getNumPredecessors() const { return mPreds.size(); }
	inline MaskGraphNode* getPredecessor(unsigned index) const {
		assert (index < mPreds.size() && "wrong index specified!");
		assert (mPreds[index]);
		return mPreds[index];
	}
	inline unsigned getNumSuccessors() const {
		if (!hasExitEdge()) return 0;
		if (!hasConditionalExit()) return 1;
		return 2;
	}
	inline MaskGraphNode* getSuccessorTrue() const {
		assert (hasExitEdge() && "node does not have any outgoing edges!");
		return mSuccTrue;
	}
	inline MaskGraphNode* getSuccessorFalse() const {
		assert (hasConditionalExit() && "node only has a single exit edge!");
		return mSuccFalse;
	}

	inline bool hasExitEdge() const { return mSuccTrue != NULL; }
	inline bool hasConditionalExit() const { return mSuccFalse != NULL; }
	inline bool hasPredecessors() const { return mPreds.size() > 0; }
	inline bool hasSinglePredecessor() const { return mPreds.size() == 1; }
	inline bool entryMaskIsSinglePredecessorExitMask() const {
		assert (mEntryMask);
		return hasSinglePredecessor() && mEntryMask == mPreds[0]->getExitMaskInDir(this);
	}

	inline bool isPredecessor(const MaskGraphNode* node) const {
		assert (node);
		for (SmallVector<MaskGraphNode*, 2>::const_iterator it=mPreds.begin();
				it!=mPreds.end(); ++it) {
			if (node == *it) return true;
		}
		return false;
	}
	inline bool isSuccessor(const MaskGraphNode* node) const {
		assert (node);
		return node == mSuccTrue || node == mSuccFalse;
	}

	inline MaskOperation* getExitMaskInDir(const MaskGraphNode* dir) {
		assert (dir);
		if (mSuccTrue && mSuccTrue == dir) return mExitMaskTrue;
		else if (mSuccFalse && mSuccFalse == dir) return mExitMaskFalse;
		else return NULL;
	}
	inline MaskOperation* getExitMaskInDir(const BasicBlock* dir) {
		assert (dir);
		if (mSuccTrue && mSuccTrue->getBlock() == dir) return mExitMaskTrue;
		else if (mSuccFalse && mSuccFalse->getBlock() == dir) return mExitMaskFalse;
		else return NULL;
	}
	inline Value* getExitMaskValInDir(const MaskGraphNode* dir) {
		assert (dir);
		if (mSuccTrue && mSuccTrue == dir) return getExitMaskTrueVal();
		else if (mSuccFalse && mSuccFalse == dir) return getExitMaskFalseVal();
		else return NULL;
	}

	//overwrite entry mask with 'maskOp' without updating connected masks
	inline void setEntryMask(Value* mask) {
		assert (mask);
		//if (entryMask) delete entryMask; // breaks something...
		mEntryMask = new MaskValue(mask);
	}
	inline void setExitMaskTrue(Value* mask) {
		assert (mask);
		//if (exitMaskTrue) delete exitMaskTrue;
		mExitMaskTrue = new MaskValue(mask);
	}
	inline void setExitMaskFalse(Value* mask) {
		assert (mask);
		//if (exitMaskFalse) delete exitMaskFalse;
		mExitMaskFalse = new MaskValue(mask);
	}
	inline void setEntryMask(MaskOperation* maskOp) {
		assert (maskOp);
		//if (entryMask) delete entryMask;
		mEntryMask = maskOp;
	}
	inline void setExitMaskTrue(MaskOperation* maskOp) {
		assert (maskOp);
		//if (exitMaskTrue) delete exitMaskTrue;
		mExitMaskTrue = maskOp;
	}
	inline void setExitMaskFalse(MaskOperation* maskOp) {
		assert (maskOp);
		//if (exitMaskFalse) delete exitMaskFalse;
		mExitMaskFalse = maskOp;
	}

	void print(raw_ostream& o) const {
		assert (mBlock);
		o << "Node: " << mBlock->getNameStr() << "\n";
		if (!mEntryMask) o << "  entry mask: NULL\n";
		else {
			o << "  entry mask: "; mEntryMask->print(o); o << "\n";
		}
		if (hasExitEdge()) {
			if (!mExitMaskTrue) o << "  exit mask T: NULL\n";
			else {
				o << "  exit mask T: "; mExitMaskTrue->print(o); o << "\n";
			}
		}
		if (hasConditionalExit()) {
			if (!mExitMaskFalse) o << "  exit mask F: NULL\n";
			else {
				o << "  exit mask F: "; mExitMaskFalse->print(o); o << "\n";
			}
		}

		if (mPreds.empty()) o << "  no predecessors!\n";
		else {
			for (unsigned i=0, e=mPreds.size(); i<e; ++i) {
				o << "  predecessor " << i << ": " << mPreds[i]->getBlock()->getNameStr() << "\n";
			}
		}

		if (hasExitEdge()) {
			if (!mSuccTrue) o << "  successor T: NULL\n";
			else {
				o << "  successor T: " << mSuccTrue->getBlock()->getNameStr() << "\n";
			}
		}
		if (hasConditionalExit()) {
			if (!mSuccFalse) o << "  successor F: NULL\n";
			else {
				o << "  successor F: " << mSuccFalse->getBlock()->getNameStr() << "\n";
			}
		}
	}
	bool verify() const {
		assert (mBlock);
		assert (mBlock->getParent());
		assert (!mBlock->getParent()->getBasicBlockList().empty());
		const bool isEntryBlock = &mBlock->getParent()->getEntryBlock() == mBlock;
		//const bool isReturnBlock = isa<ReturnInst>(block->getTerminator());
		const bool isReturnBlock = !hasExitEdge();
		const bool x = mBlock && mEntryMask;
		const bool a = isReturnBlock || mExitMaskTrue;
		const bool b = !hasConditionalExit() || mExitMaskFalse;
		const bool c = isEntryBlock || hasPredecessors();
		const bool d = isEntryBlock || (hasPredecessors() && mPreds[0]);
		const bool e = isEntryBlock || hasSinglePredecessor() || (getNumPredecessors() > 1 && mPreds[1]);
		const bool f = isReturnBlock || mSuccTrue;
		const bool g = !hasConditionalExit() || mSuccFalse;
		std::set<const MaskOperation*> visitedMaskOps;
		const bool h = mEntryMask && mEntryMask->verify(visitedMaskOps);
		visitedMaskOps.clear();
		const bool i = isReturnBlock || (mExitMaskTrue && mExitMaskTrue->verify(visitedMaskOps));
		visitedMaskOps.clear();
		const bool j = !hasConditionalExit() || (mExitMaskFalse && mExitMaskFalse->verify(visitedMaskOps));
		const bool res = x && a && b && c && d && e && f && g && h && i && j;
		if (!res) {
			errs() << "\nERROR: verification of mask graph node failed!\n";
			if (mBlock) errs() << "block: " << mBlock->getNameStr() << "\n";
			if (!mEntryMask) errs() << "  entry mask is NULL!\n";
			if (!a) errs() << "  exit mask true is NULL!\n";
			if (!b) errs() << "  exit mask false is NULL!\n";
			if (!c) errs() << "  non-entry block has no predecessors!\n";
			if (!d) errs() << "  first predecessor of non-entry block is NULL!\n";
			if (!e) errs() << "  second predecessor of non-entry block is NULL!\n";
			if (!f) errs() << "  true-successor of non-return block is NULL!\n";
			if (!g) errs() << "  false-successor of block with conditional exit is NULL!\n";
			if (!h) {
				errs() << "  verification of entry mask failed: ";
				if (mEntryMask) { mEntryMask->print(errs()); errs() << "\n"; }
			}
			if (!i) {
				errs() << "  verification of exit mask (true-edge) failed: ";
				if (mExitMaskTrue) { mExitMaskTrue->print(errs()); errs() << "\n"; }
			}
			if (!j) {
				errs() << "  verification of exit mask (false-edge) failed: ";
				if (mExitMaskFalse) { mExitMaskFalse->print(errs()); errs() << "\n"; }
			}

			errs() << "\n";
			assert (!"verification failed!");
		}
		return res;
	}
	bool complete() const {
		assert (mBlock);
		assert (mBlock->getParent());
		assert (!mBlock->getParent()->getBasicBlockList().empty());
		const bool isEntryBlock = &mBlock->getParent()->getEntryBlock() == mBlock;
		const bool isReturnBlock = !hasExitEdge();
		const bool x = mBlock && mEntryMask;
		const bool a = isReturnBlock || mExitMaskTrue;
		const bool b = !hasConditionalExit() || mExitMaskFalse;
		const bool c = isEntryBlock || !mPreds.empty();
		const bool d = isEntryBlock || (!mPreds.empty() && mPreds[0]);
		const bool e = isEntryBlock || hasSinglePredecessor() || (mPreds.size() > 1 && mPreds[1]);
		const bool f = isReturnBlock || mSuccTrue;
		const bool g = !hasConditionalExit() || mSuccFalse;
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
	bool mVerbose;
	typedef std::map<BasicBlock*, MaskGraphNode*> MaskGraphType;

	MaskGraphType mMaskGraph;
	MaskGraphNode* mRoot;
	MaskGraphNode* mSink;

	Function& mFunction;

	bool mInitialized;
	bool mComplete;

	//Loop Mask Information
	typedef std::map<const Loop*, LoopNode*> LoopMapType;
	typedef std::map<BasicBlock*, LoopExitNode*> LoopExitMaskMapType;
	LoopMapType mLoopMap;
	LoopExitMaskMapType mLoopExitMaskMap;
	const LoopInfo& mLoopInfo;
    LLVMContext& mContext;

	Constant* mConstBoolTrue;
	Constant* mConstBoolFalse;

	void buildGraph(MaskGraphNode* node) {
		BasicBlock* block = node->getBlock();
		unsigned predNr = 0;
		unsigned succNr = 0;
		//set predecessor nodes
		for (pred_iterator PI=pred_begin(block), PE=pred_end(block); PI!=PE; ++PI) {
			BasicBlock* predBB = *PI;
			MaskGraphNode* predNode = findMaskNode(predBB);
			if (!predNode) {
				predNode = new MaskGraphNode(predBB, mContext, mVerbose);
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
				succNode = new MaskGraphNode(succBB, mContext, mVerbose);
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
		assert (mMaskGraph.find(mgn->getBlock()) == mMaskGraph.end());
		std::pair<MaskGraphType::iterator, bool> resPair = mMaskGraph.insert(std::make_pair(mgn->getBlock(), mgn));
		assert (resPair.second && "insertion must not fail!");
		assert (resPair.first->second && "insertion must return valid object!");
		return resPair.first->second;
	}

	inline bool checkForCompletion() {
		assert (mInitialized);
		if (mComplete) return true;

		if (mMaskGraph.size() != mFunction.getBasicBlockList().size()) {
			mComplete = false;
			DEBUG_PKT( errs() << "ERROR: mask graph has wrong size -> incomplete!\n"; );
			return false;
		}
		mComplete = true;
		for (MaskGraph::const_iterator it=mMaskGraph.begin(), E=mMaskGraph.end(); it!=E; ++it) {
			mComplete &= it->second->complete();
			if (!it->second->complete()) {
				DEBUG_PKT( errs() << "ERROR: node is not complete: "; );
				DEBUG_PKT( it->second->print(outs()); );
			}
		}
		return mComplete;
	}

	inline void buildLoopMaskGraph() {
		for (LoopInfo::iterator L=mLoopInfo.begin(), LE=mLoopInfo.end(); L!=LE; ++L) {
			LoopNode* loopNode = recBuildLoopMaskNode(*L, NULL);
			assert (loopNode);
			assert (mLoopMap.find(*L) == mLoopMap.end());
			mLoopMap.insert(std::make_pair(*L, loopNode));
		}
	}
	LoopNode* recBuildLoopMaskNode(const Loop* loop, LoopNode* parent) {
		LoopNode* loopNode = new LoopNode(loop, parent);
		SmallVector<LoopNode*, 4> subLoopVec;
		for (Loop::iterator SL=loop->begin(), LE=loop->end(); SL!=LE; ++SL) {
			LoopNode* subLoopNode = recBuildLoopMaskNode(*SL, loopNode);
			assert (mLoopMap.find(*SL) == mLoopMap.end());
			mLoopMap.insert(std::make_pair(*SL, subLoopNode));
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
			LoopExitMaskMapType::const_iterator tmp = mLoopExitMaskMap.find(exitingBlock);
			if (tmp != mLoopExitMaskMap.end()) {
				//update existing loop exit node
				tmp->second->addLoop(loop);
			} else {
				//create new loop exit node
				mLoopExitMaskMap.insert(std::make_pair(exitingBlock, new LoopExitNode(exitingBlock, mLoopInfo)));
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
	MaskGraph(Function& f, const LoopInfo& loopInfos, LLVMContext& context, bool verbose_flag=false)
		: mVerbose(verbose_flag), mFunction(f), mComplete(false), mLoopInfo(loopInfos), mContext(context)
	{
		mRoot = new MaskGraphNode(&f.getEntryBlock(), mContext, mVerbose);
		insert(mRoot);
		buildGraph(mRoot);
		mInitialized = true;
		mSink = findMaskNode(Packetizer::findReturnBlock(f));
		assert (mSink);

		if (!mLoopInfo.empty()) {
			//build loop mask graph
			mConstBoolTrue = Constant::getAllOnesValue(Type::getInt1Ty(mContext));
			mConstBoolFalse = Constant::getNullValue(Type::getInt1Ty(mContext));
			buildLoopMaskGraph();
		}
	}

    ~MaskGraph() {}

	inline void clear() {
		std::set<MaskValue*> uniqueMaskValues;
		collectUniqueMaskValues(uniqueMaskValues);
		for (std::set<MaskValue*>::iterator it=uniqueMaskValues.begin(),
				E=uniqueMaskValues.end(); it!=E; ++it)
		{
			//outs() << "deleting mask value: "; (*it)->print(outs()); outs() << "...\n";
			delete *it;
		}

		for (MaskGraphType::iterator it=mMaskGraph.begin(), E=mMaskGraph.end(); it!=E; ++it) {
			//outs() << "deleting node of block '" << it->second->getBlock()->getName() << "'...\n";
			delete it->second;
		}
		for (LoopMapType::iterator it=mLoopMap.begin(), E=mLoopMap.end(); it!=E; ++it) {
			delete it->second;
		}
		for (LoopExitMaskMapType::iterator it=mLoopExitMaskMap.begin(), E=mLoopExitMaskMap.end(); it!=E; ++it) {
			delete it->second;
		}

		mMaskGraph.clear();

		mRoot = NULL;
		mSink = NULL;
		mInitialized = false;
		mComplete = false;
		nextMaskOpID = 0;

		mLoopMap.clear();
		mLoopExitMaskMap.clear();
	}


	void collectUniqueMaskValues(std::set<MaskValue*>& maskValues) {
		for (MaskGraphType::iterator it=mMaskGraph.begin(), E=mMaskGraph.end(); it!=E; ++it) {
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
		assert (mInitialized);
		assert (mComplete);
		assert (verify());
	}

	typedef MaskGraphType::iterator iterator;
	typedef MaskGraphType::const_iterator const_iterator;
	inline iterator begin() { assert (mInitialized); return mMaskGraph.begin(); }
	inline iterator end() { assert (mInitialized); return mMaskGraph.end(); }
	inline const_iterator begin() const { assert (mInitialized); return mMaskGraph.begin(); }
	inline const_iterator end() const { assert (mInitialized); return mMaskGraph.end(); }

	inline unsigned size() const { assert (mInitialized); return mMaskGraph.size(); }
	inline bool empty() const { assert (mInitialized); return mMaskGraph.empty(); }
	inline bool isInitialized() const { return mInitialized; }

	inline MaskGraphNode* getRoot() const { assert(mInitialized && mRoot); return mRoot; }
	inline MaskGraphNode* getSink() const { assert(mInitialized && mSink); return mSink; }

	inline MaskGraphType::const_iterator find(BasicBlock* block) const {
		assert (mInitialized && block);
		return mMaskGraph.find(block);
	}
	// can be called during initialization
	inline MaskGraphNode* findMaskNode(BasicBlock* block) const {
		assert (block);
		MaskGraphType::const_iterator tmp = mMaskGraph.find(block);
		if (tmp == mMaskGraph.end()) return NULL;
		return tmp->second;
	}

	inline MaskGraphNode* insert(BasicBlock* block, Value* entryMask) {
		assert (mInitialized && block);
		MaskOperation* entry = new MaskValue(entryMask);
		MaskGraphNode* mgn = new MaskGraphNode(block, mContext, mVerbose);
		mgn->setEntryMask(entry);
		assert (mMaskGraph.find(block) == mMaskGraph.end());
		std::pair<MaskGraphType::iterator, bool> resPair = mMaskGraph.insert(std::make_pair(block, mgn));
		assert (resPair.second && "insertion must not fail!");
		assert (resPair.first->second && "insertion must return valid object!");
		checkForCompletion();
		return resPair.first->second;
	}
	inline MaskGraphNode* insert(BasicBlock* block, Value* entryMask, Value* exitMask) {
		assert (mInitialized && block && entryMask && exitMask);
		MaskOperation* entry = new MaskValue(entryMask);
		MaskOperation* exit = new MaskValue(exitMask);
		MaskGraphNode* mgn = new MaskGraphNode(block, entry, exit, mContext, mVerbose);
		assert (mMaskGraph.find(block) == mMaskGraph.end());
		std::pair<MaskGraphType::iterator, bool> resPair = mMaskGraph.insert(std::make_pair(block, mgn));
		assert (resPair.second && "insertion must not fail!");
		assert (resPair.first->second && "insertion must return valid object!");
		checkForCompletion();
		return resPair.first->second;
	}
	inline MaskGraphNode* insert(BasicBlock* block, Value* entryMask, Value* exitMaskTrue, Value* exitMaskFalse) {
		assert (mInitialized && block && entryMask && exitMaskTrue && exitMaskFalse);
		MaskOperation* entry = new MaskValue(entryMask);
		MaskOperation* exitT = new MaskValue(exitMaskTrue);
		MaskOperation* exitF = new MaskValue(exitMaskFalse);
		MaskGraphNode* mgn = new MaskGraphNode(block, entry, exitT, exitF, mContext, mVerbose);
		assert (mMaskGraph.find(block) == mMaskGraph.end());
		std::pair<MaskGraphType::iterator, bool> resPair = mMaskGraph.insert(std::make_pair(block, mgn));
		assert (resPair.second && "insertion must not fail!");
		assert (resPair.first->second && "insertion must return valid object!");
		checkForCompletion();
		return resPair.first->second;
	}
	inline MaskGraphNode* insert(BasicBlock* block, MaskOperation* entryMask, MaskOperation* exitMask) {
		assert (mInitialized && block && entryMask && exitMask);
		MaskGraphNode* mgn = new MaskGraphNode(block, entryMask, exitMask, mContext, mVerbose);
		assert (mMaskGraph.find(block) == mMaskGraph.end());
		std::pair<MaskGraphType::iterator, bool> resPair = mMaskGraph.insert(std::make_pair(block, mgn));
		assert (resPair.second && "insertion must not fail!");
		assert (resPair.first->second && "insertion must return valid object!");
		checkForCompletion();
		return resPair.first->second;
	}
	inline MaskGraphNode* insert(BasicBlock* block, MaskOperation* entryMask, MaskOperation* exitMaskTrue, MaskOperation* exitMaskFalse) {
		assert (mInitialized && block && entryMask && exitMaskTrue && exitMaskFalse);
		MaskGraphNode* mgn = new MaskGraphNode(block, entryMask, exitMaskTrue, exitMaskFalse, mContext, mVerbose);
		assert (mMaskGraph.find(block) == mMaskGraph.end());
		std::pair<MaskGraphType::iterator, bool> resPair = mMaskGraph.insert(std::make_pair(block, mgn));
		assert (resPair.second && "insertion must not fail!");
		assert (resPair.first->second && "insertion must return valid object!");
		checkForCompletion();
		return resPair.first->second;
	}

	inline bool updatePredecessor(BasicBlock* block, BasicBlock* oldPredBB, BasicBlock* newPredBB) {
		assert (mInitialized && block && newPredBB && oldPredBB);
		MaskGraphType::const_iterator node = find(block);
		if (node == mMaskGraph.end()) return false;

		MaskGraphType::const_iterator oldPredNode = find(oldPredBB);
		if (oldPredNode == mMaskGraph.end()) return false;

		MaskGraphType::const_iterator newPredNode = find(newPredBB);
		if (newPredNode == mMaskGraph.end()) return false;

		return node->second->updatePredecessor(oldPredNode->second, newPredNode->second);
	}
	inline bool addPredecessor(BasicBlock* block, BasicBlock* newPredBB) {
		assert (mInitialized && block && newPredBB);
		MaskGraphType::const_iterator node = find(block);
		if (node == mMaskGraph.end()) return false;

		MaskGraphType::const_iterator predNode = find(newPredBB);
		if (predNode == mMaskGraph.end()) return false;

		node->second->addPredecessor(predNode->second);
		return true;
	}
	inline bool setSuccessorTrue(BasicBlock* block, BasicBlock* newSuccBB) {
		assert (mInitialized && block && newSuccBB);
		MaskGraphType::const_iterator node = find(block);
		if (node == mMaskGraph.end()) return false;

		MaskGraphType::const_iterator succNode = find(newSuccBB);
		if (succNode == mMaskGraph.end()) return false;

		//assert(node->second->hasExitEdge() && "trying to add successor to node that does not have any exit edge!");

		node->second->setSuccessorTrue(succNode->second);
		return true;
	}
	inline bool setSuccessorFalse(BasicBlock* block, BasicBlock* newSuccBB) {
		assert (mInitialized && block && newSuccBB);
		MaskGraphType::const_iterator node = find(block);
		if (node == mMaskGraph.end()) return false;

		MaskGraphType::const_iterator succNode = find(newSuccBB);
		if (succNode == mMaskGraph.end()) return false;

		assert(node->second->hasConditionalExit() && "trying to add false-successor to node that only has one mask associated!");

		node->second->setSuccessorFalse(succNode->second);
		return true;
	}
	inline bool removePredecessor(BasicBlock* block, BasicBlock* oldPredBB) {
		assert (mInitialized && block && oldPredBB);
		MaskGraphType::const_iterator node = find(block);
		if (node == mMaskGraph.end()) return false;

		MaskGraphType::const_iterator predNode = find(oldPredBB);
		if (predNode == mMaskGraph.end()) return false;

		return node->second->removePredecessor(predNode->second);
	}
	inline void setEntryMask(MaskGraphNode* node, MaskOperation* newMask) {
		assert (mInitialized && node && newMask);
		node->setEntryMask(newMask);
	}
	inline void setExitMaskTrue(MaskGraphNode* node, MaskOperation* newMask) {
		assert (mInitialized && node && newMask);
		node->setExitMaskTrue(newMask);
	}
	inline void setExitMaskFalse(MaskGraphNode* node, MaskOperation* newMask) {
		assert (mInitialized && node && newMask);
		node->setExitMaskFalse(newMask);
	}

	inline Value* getEntryMask(BasicBlock* block) const {
		assert (mInitialized && block);
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
		assert (mInitialized && block);
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
		assert (mInitialized && block);
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
		assert (mInitialized && source && dir);
		assert (!hasCompoundMasks() && "must not ask for explicit mask of type Value* if mask graph still holds compound masks!");
		MaskOperation* maskOp = getExitMaskOperationInDir(source, dir);
		assert (maskOp); //if (!maskOp) return NULL;
		assert (maskOp->isMaskValue());
		MaskValue* singleMaskOp = static_cast<MaskValue*>(maskOp);
		assert (singleMaskOp->getMask());
		return singleMaskOp->getMask();
	}

	inline MaskOperation* getExitMaskTrue(MaskGraphNode* source) const {
		assert (mInitialized && source);
		return source->getExitMaskTrue();
	}
	inline MaskOperation* getExitMaskOperationInDir(MaskGraphNode* source, BasicBlock* dir) const {
		assert (mInitialized && source && dir);
		return source->getExitMaskInDir(dir);
	}
	inline MaskOperation* getExitMaskOperationInDir(BasicBlock* source, BasicBlock* dir) const {
		assert (mInitialized && source && dir);
		MaskGraphType::const_iterator node = find(source);
		if (node == mMaskGraph.end()) {
			//errs() << "ERROR: getExitMaskInDir(): source block '" << source->getNameStr() << "'\n";
			return NULL;
		}

		return node->second->getExitMaskInDir(dir);
	}

	inline void print(raw_ostream& o) const {
		assert (mInitialized);
		//	assert (initialized && checkForCompletion()); // this is dangerous, checkForCompletion() modifies state of maskGraph!
		for (MaskGraphType::const_iterator it=mMaskGraph.begin(), E=mMaskGraph.end(); it!=E; ++it) {
			it->second->print(o);
		}
	}
	inline bool verify() const {
		bool verified = true;

		if (!mInitialized) {
			errs() << "ERROR: mask graph was not initialized, can not verify!\n";
			return false;
		}

		//if (!complete) {
		//	verified = false;
		//	errs() << "ERROR: graph was not completed! (finalize() not called?)\n";
		//}

		if (mMaskGraph.size() != mFunction.getBasicBlockList().size()) {
			verified = false;
			errs() << "ERROR: size of mask graph does not match block list size of function!\n";
		}

		for (MaskGraphType::const_iterator it=mMaskGraph.begin(), E=mMaskGraph.end(); it!=E; ++it) {
			assert (it->first && "block must not be NULL!");
			assert (it->second && "node must not be NULL!");
			assert (it->first == it->second->getBlock() && "associated blocks have to match!");

			const bool nodeVerified = it->second->verify();
			if (!nodeVerified)
				errs() << "ERROR: verification of node of block '" << it->first->getNameStr() << "' failed!\n";

			verified &= nodeVerified;
		}
		if (!mRoot) {
			verified = false;
			errs() << "ERROR: verification of root of mask graph failed!\n";
		}
		return verified;
	}
	bool hasCompoundMasks() const {
		assert (mInitialized);
		//assert (checkForCompletion()); // this is dangerous, checkForCompletion() modifies state of maskGraph!
		for (MaskGraphType::const_iterator it=mMaskGraph.begin(), E=mMaskGraph.end(); it!=E; ++it) {
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
		assert (mInitialized);

		// Create empty loop exit mask phis upfront.
		for (LoopExitMaskMapType::iterator LM=mLoopExitMaskMap.begin(), LME=mLoopExitMaskMap.end(); LM!=LME; ++LM) {
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
				assert (ar.isUniform(mLoopInfo.getLoopFor(loopExitNode->getExitingBlock())));
				DEBUG_PKT( outs() << "ignoring generation of loop exit mask "
						"phis for uniform loop: " << *mLoopInfo.getLoopFor(loopExitNode->getExitingBlock()); );
				continue;
			}

			//insert empty exit mask phis into headers of all loops exited by this exit
			for (LoopExitNode::iterator it=loopExitNode->begin(), E=loopExitNode->end(); it!=E; ++it) {
				const Loop* loop = it->first;

				PHINode* phi = PHINode::Create(
						Type::getInt1Ty(mContext),
						2U,
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
		assert (mSink);
		//sink->createEntryMask(analysisResults); // should not be necessary in addition to recInsertMasks
		// Recursion over node graph only is not enough due to FULLY_UNIFORM blocks
		// (entry mask = true). We need to iterate over the blocks' predecessors.
		std::set<MaskGraphNode*> visitedSet;
		recInsertMasks(mSink, ar, visitedSet);

		// set update operation of each exit mask to just generated exit mask value
		for (LoopExitMaskMapType::iterator LM=mLoopExitMaskMap.begin(), LME=mLoopExitMaskMap.end(); LM!=LME; ++LM) {
			LoopExitNode* loopExitNode = LM->second;
			BasicBlock* exitingBlock = LM->first;

			// Just like above, ignore uniform loops
			if (ar.isUniform(loopExitNode->getInnermostLoop())) {
				assert (ar.isUniform(loopExitNode->getInnermostLoop()));
				assert (ar.isUniform(mLoopInfo.getLoopFor(loopExitNode->getExitingBlock())));
				DEBUG_PKT( outs() << "ignoring update of loop exit mask "
						"phis for uniform loop: " << *mLoopInfo.getLoopFor(loopExitNode->getExitingBlock()); );
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
		assert (mInitialized);
		for (LoopExitMaskMapType::iterator LM=mLoopExitMaskMap.begin(), LME=mLoopExitMaskMap.end(); LM!=LME; ++LM) {
			LoopExitNode* loopExitNode = LM->second;

			// Just like in 'insertMasks()', ignore uniform loops
			if (ar.isUniform(loopExitNode->getInnermostLoop())) {
				assert (ar.isUniform(loopExitNode->getInnermostLoop()));
				assert (ar.isUniform(mLoopInfo.getLoopFor(loopExitNode->getExitingBlock())));
				DEBUG_PKT( outs() << "\nignoring update of loop exit mask "
						"phis for uniform loop: " << *mLoopInfo.getLoopFor(loopExitNode->getExitingBlock()); );
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
					phi->addIncoming(Constant::getNullValue(Type::getInt1Ty(mContext)), preheaderBB);
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
		assert (mInitialized);
		assert (!mLoopMap.empty() && "loop exit mask map is empty!");
		LoopMapType::const_iterator tmp = mLoopMap.find(loop);
		if (tmp == mLoopMap.end()) return NULL;
		return tmp->second;
	}
	inline bool isTopLevelLoop(const Loop* loop) const {
		assert (mInitialized);
		LoopMapType::const_iterator tmp = mLoopMap.find(loop);
		assert (tmp != mLoopMap.end() && "isTopLevelLoop() requested for unknown loop!");
		const bool isNormalTopLevelLoop = tmp->second->isTopLevelLoop();
		return isNormalTopLevelLoop;
	}
	inline bool hasNestedLoop(const Loop* loop) const {
		assert (mInitialized);
		LoopMapType::const_iterator tmp = mLoopMap.find(loop);
		assert (tmp != mLoopMap.end() && "hasNestedLoop() requested for unknown loop!");
		return tmp->second->hasNestedLoop();
	}
	inline bool hasMultipleExits(const Loop* loop) const {
		assert (mInitialized);
		LoopMapType::const_iterator tmp = mLoopMap.find(loop);
		assert (tmp != mLoopMap.end() && "hasMultipleExits() requested for unknown loop!");
		return tmp->second->hasMultipleExits();
	}

	//Loop Exit Info stuff
	inline LoopExitNode* findLoopExitNode(BasicBlock* exitingBlock) const {
		assert (mInitialized);
		assert (!mLoopExitMaskMap.empty() && "loop exit mask map is empty!");
		LoopExitMaskMapType::const_iterator tmp = mLoopExitMaskMap.find(exitingBlock);
		if (tmp == mLoopExitMaskMap.end()) return NULL;
		return tmp->second;
	}
	inline bool isInnermostLoopOfExit(BasicBlock* exitingBlock, const Loop* loop) const {
		assert (mInitialized);
		LoopExitMaskMapType::const_iterator tmp = mLoopExitMaskMap.find(exitingBlock);
		assert (tmp != mLoopExitMaskMap.end());
		return tmp->second->isInnermostLoop(loop);
	}
	inline bool exitsMultipleLoops(BasicBlock* exitingBlock) const {
		assert (mInitialized);
		LoopExitMaskMapType::const_iterator tmp = mLoopExitMaskMap.find(exitingBlock);
		assert (tmp != mLoopExitMaskMap.end());
		return tmp->second->exitsMultipleLoops();
	}
	inline bool getInnermostLoopOfExit(BasicBlock* exitingBlock) const {
		assert (mInitialized);
		LoopExitMaskMapType::const_iterator tmp = mLoopExitMaskMap.find(exitingBlock);
		assert (tmp != mLoopExitMaskMap.end());
		return tmp->second->getInnermostLoop();
	}

	//returns the mask operations inside 'loop' that are related to the exit 'exitingBlock'
	//NOTE: loop can be any loop that is exited by this exit
	inline MaskOperation* getLoopExitMaskPhiOp(BasicBlock* exitingBlock, const Loop* loop) const {
		assert (mInitialized);
		assert (!mLoopExitMaskMap.empty() && "loop exit mask map is empty!");
		LoopExitMaskMapType::const_iterator tmp = mLoopExitMaskMap.find(exitingBlock);
		assert (tmp != mLoopExitMaskMap.end());
		return tmp->second->getMaskPhiOp(loop);
	}
	inline MaskOperation* getLoopExitMaskUpdateOp(BasicBlock* exitingBlock) const {
		assert (mInitialized);
		assert (!mLoopExitMaskMap.empty() && "loop exit mask map is empty!");
		LoopExitMaskMapType::const_iterator tmp = mLoopExitMaskMap.find(exitingBlock);
		assert (tmp != mLoopExitMaskMap.end());
		return tmp->second->getMaskUpdateOp();
	}

	inline void setLoopExitMaskPhi(BasicBlock* exitingBlock, const Loop* loop, MaskOperation* maskPhi) {
		assert (mInitialized);
		assert (exitingBlock && loop && maskPhi);
		assert (!mLoopExitMaskMap.empty() && "loop exit mask map is empty!");
		LoopExitMaskMapType::const_iterator tmp = mLoopExitMaskMap.find(exitingBlock);
		assert (tmp != mLoopExitMaskMap.end());
		tmp->second->setMaskPhiOp(loop, maskPhi);
	}
	inline void setLoopExitMaskUpdateOp(BasicBlock* exitingBlock, MaskOperation* maskUpdateOp) {
		assert (mInitialized);
		assert (!mLoopExitMaskMap.empty() && "loop exit mask map is empty!");
		LoopExitMaskMapType::const_iterator tmp = mLoopExitMaskMap.find(exitingBlock);
		assert (tmp != mLoopExitMaskMap.end());
		tmp->second->setMaskUpdateOp(maskUpdateOp);
	}

	inline bool verifyLoopMasks(AnalysisResults& ar) const {
		assert (mInitialized);
		for (LoopMapType::const_iterator LN=mLoopMap.begin(), LNE=mLoopMap.end(); LN!=LNE; ++LN) {
			const bool verified = LN->second->verify();
			if (!verified) {
				errs() << "ERROR: verification of loop node failed!\n";
				errs() << "  Loop: "; LN->first->print(errs());
				return false;
			}
		}
		for (LoopExitMaskMapType::const_iterator LN=mLoopExitMaskMap.begin(), LNE=mLoopExitMaskMap.end(); LN!=LNE; ++LN) {
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
		assert (mInitialized);
		o << "\nLoop Map:\n";
		//recursively print loop hierarchy
		for (LoopInfo::iterator it=mLoopInfo.begin(), E=mLoopInfo.end(); it!=E; ++it) {
			assert (findLoopNode(*it) && "loop node must not be NULL!");
			findLoopNode(*it)->print(o);
		}
	}
	inline void printLoopExitMap(raw_ostream& o) const {
		assert (mInitialized);
		o << "\nLoop Exit Mask Map:\n";
		for (LoopExitMaskMapType::const_iterator it=mLoopExitMaskMap.begin(), E=mLoopExitMaskMap.end(); it!=E; ++it) {
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

