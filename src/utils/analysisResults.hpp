/**
 * @file   analysisResults.hpp
 * @date   04.11.2011
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2011 Saarland University
 *
 */
#ifndef _ANALYSISRESULTS_HPP
#define	_ANALYSISRESULTS_HPP

#include "llvm/Support/raw_ostream.h"

#include "llvm/Analysis/LoopInfo.h"

#include <map>


using namespace llvm;

// TODO: This was quickly hacked to allow testing of results of
//       Vectorization Analysis. VA should write directly into this structure
//       which can be used in the other passes as well.
//       However, this currently does not affect performance because it is only
//       used by analyzeResults() which is independent of WFV.


class AnalysisResults {
public:
	enum UniformInfo {
		UNIFORM_NOT_INITIALIZED, // not initialized yet
		UNIFORM_UNKNOWN,         // unknown value, could not be determined
		UNIFORM,                 // value is equal for all instances
		FULLY_UNIFORM,
		VARYING                  // values can be different
	};
	enum IndexInfo {
		INDEX_NOT_INITIALIZED,
		INDEX_UNKNOWN,
		INDEX_SAME,
		INDEX_CONSECUTIVE,
		INDEX_SHUFFLE,
		INDEX_STRIDED,
		INDEX_RANDOM
	};
	enum AlignmentInfo {
		ALIGN_NOT_INITIALIZED,
		ALIGN_UNKNOWN,
		ALIGN_TRUE,
		ALIGN_FALSE
	};
	enum SplitInfo {
		SPLIT_NOT_INITIALIZED, // not initialized yet
		SPLIT_UNKNOWN,         // unknown value, could not be determined
		SPLIT_NEVER,           // does not require any splitting ever
		SPLIT_REPLICATE,       // does not require any splitting ever, but result is used in some split operation(s) (implies UNIFORM)
		SPLIT_RESULT,          // requires splitting of its result (= result can be used both in split and in vector operations)
		SPLIT_FULL,            // instruction must never remain vectorized
		SPLIT_FULL_GUARDED     // instruction must never remain vectorized and each scalar execution has to be guarded by the instances mask
	};

	static const std::string getUniformInfoString(const UniformInfo& ui) {
		switch (ui) {
			case UNIFORM_NOT_INITIALIZED: return "UNIFORM_NOT_INITIALIZED";
			case UNIFORM_UNKNOWN: return "UNIFORM_UNKNOWN";
			case UNIFORM: return "UNIFORM";
			case FULLY_UNIFORM: return "FULLY_UNIFORM";
			case VARYING: return "VARYING";
		}
		assert (false && "must never happen!");
		return "INVALID";
	}
	static const std::string getIndexInfoString(const IndexInfo& ii) {
		switch (ii) {
			case INDEX_NOT_INITIALIZED: return "INDEX_NOT_INITIALIZED";
			case INDEX_UNKNOWN: return "INDEX_UNKNOWN";
			case INDEX_SAME: return "INDEX_SAME";
			case INDEX_CONSECUTIVE: return "INDEX_CONSECUTIVE";
			case INDEX_SHUFFLE: return "INDEX_SHUFFLE";
			case INDEX_STRIDED: return "INDEX_STRIDED";
			case INDEX_RANDOM: return "INDEX_RANDOM";
		}
		assert (false && "must never happen!");
		return "INVALID";
	}
	static const std::string getAlignmentInfoString(const AlignmentInfo& ai) {
		switch (ai) {
			case ALIGN_NOT_INITIALIZED: return "ALIGN_NOT_INITIALIZED";
			case ALIGN_UNKNOWN: return "ALIGN_UNKNOWN";
			case ALIGN_TRUE: return "ALIGN_TRUE";
			case ALIGN_FALSE: return "ALIGN_FALSE";
		}
		assert (false && "must never happen!");
		return "INVALID";
	}
	static const std::string getSplitInfoString(const SplitInfo& si) {
		switch (si) {
			case SPLIT_NOT_INITIALIZED: return "SPLIT_NOT_INITIALIZED";
			case SPLIT_UNKNOWN: return "SPLIT_UNKNOWN";
			case SPLIT_NEVER: return "SPLIT_NEVER";
			case SPLIT_REPLICATE: return "SPLIT_REPLICATE";
			case SPLIT_RESULT: return "SPLIT_RESULT";
			case SPLIT_FULL: return "SPLIT_FULL";
			case SPLIT_FULL_GUARDED: return "SPLIT_FULL_GUARDED";
		}
		assert (false && "must never happen!");
		return "INVALID";
	}

	// TODO: create class with class methods "add", "remove", "get"
	struct ValueInfo {
		ValueInfo(Value* val)
				: value(val),
				uniformInfo(UNIFORM_NOT_INITIALIZED),
				indexInfo(INDEX_NOT_INITIALIZED),
				alignmentInfo(ALIGN_NOT_INITIALIZED),
				splitInfo(SPLIT_NOT_INITIALIZED),
				isMask(false)
		{}
		ValueInfo(const ValueInfo& vi)
				: value(vi.value),
				uniformInfo(vi.uniformInfo),
				indexInfo(vi.indexInfo),
				alignmentInfo(vi.alignmentInfo),
				splitInfo(vi.splitInfo),
				isMask(vi.isMask)
		{}

		Value* value;
		UniformInfo uniformInfo;
		IndexInfo indexInfo;
		AlignmentInfo alignmentInfo;
		SplitInfo splitInfo;
		bool isMask;

		void print(raw_ostream& O) const
		{
			O << "Value: " << *value << "\n";
			O << "  Uniform: " << getUniformInfoString(uniformInfo) << "\n";
			O << "  Index: " << getIndexInfoString(indexInfo) << "\n";
			O << "  Alignment: " << getAlignmentInfoString(alignmentInfo) << "\n";
			O << "  Split: " << getSplitInfoString(splitInfo) << "\n";
			O << "  IsMask: " << (isMask ? "true" : "false") << "\n";
		}
	};


	// TODO: create class with class methods "add", "remove", "get"
	struct BlockInfo {
		BlockInfo(BasicBlock* b,
				  bool nonDivergent,
				  bool fullyUniform,
				  bool uniformExit,
				  bool fullyUniformExit)
		: block(b),
				isNonDivergent(nonDivergent),
				isFullyNonDivergent(fullyUniform),
				hasUniformExit(uniformExit),
				hasFullyUniformExit(fullyUniformExit)
		{}
		BlockInfo(const BlockInfo& bi)
		: block(bi.block),
				isNonDivergent(bi.isNonDivergent),
				isFullyNonDivergent(bi.isFullyNonDivergent),
				hasUniformExit(bi.hasUniformExit),
				hasFullyUniformExit(bi.hasFullyUniformExit)
		{}

		BasicBlock* block;
		bool isNonDivergent; // all active instances relative to last uniform branch enter this block
		bool isFullyNonDivergent; // all instances enter this block
		bool hasUniformExit; // all active instances leave over the same edge
		bool hasFullyUniformExit; // all instances leave over the same edge

		void print(raw_ostream& O) const
		{
			O << "  * " << block->getName() << ": " << getBlockInfoString(this) << "\n";
		}
	};

	static const std::string getBlockInfoString(const BlockInfo* bi) {
		std::string s = "";
		if (bi->isFullyNonDivergent) s += "FULLY_NON_DIVERGENT, ";
		else if (bi->isNonDivergent) s += "NON_DIVERGENT, ";
		else s += "DIVERGENT, ";
		if (bi->hasFullyUniformExit) s += "FULLY_UNIFORM_EXIT";
		else if (bi->hasUniformExit) s += "UNIFORM_EXIT";
		else s += "VARYING_EXIT";
		return s;
	}


	// TODO: create class with class methods "add", "remove", "get"
	struct UniformLoopInfo {
		UniformLoopInfo(const Loop* l,
				  bool nonDivergent,
				  bool fullyNonDivergent)
		: loop(l),
				isNonDivergent(nonDivergent),
				isFullyNonDivergent(fullyNonDivergent),
				isVaryingTopLevel(false),
				isVaryingInnermost(false)
		{}
		UniformLoopInfo(const UniformLoopInfo& li)
		: loop(li.loop),
				isNonDivergent(li.isNonDivergent),
				isFullyNonDivergent(li.isFullyNonDivergent),
				isVaryingTopLevel(li.isVaryingTopLevel),
				isVaryingInnermost(li.isVaryingInnermost)
		{}

		const Loop* loop;
		bool isNonDivergent;
		bool isFullyNonDivergent;
		bool isVaryingTopLevel;
		bool isVaryingInnermost;

		void print(raw_ostream& O) const
		{
			O << "  loop of header '"
				<< loop->getHeader()->getName() << "' is "
				<< (isFullyNonDivergent ? "FULLY_UNIFORM" :
						(isNonDivergent ? "UNIFORM" : "VARYING"))
				<< (isVaryingTopLevel ? ", VARYING_TOP_LEVEL" : "")
				<< (isVaryingInnermost ? ", VARYING_INNERMOST" : "")
				<< "\n";
		}
	};

	static const std::string getUniformLoopInfoString(const UniformLoopInfo* uli) {
		if (uli->isFullyNonDivergent) return "FULLY_UNIFORM";
		if (uli->isNonDivergent) return "CF_UNIFORM";
		return "CF_VARYING";
	}

	typedef std::map<const Value*, ValueInfo*>      ValueInfoMapType;
	typedef std::map<const BasicBlock*, BlockInfo*> BlockInfoMapType;
	typedef std::map<const Loop*, UniformLoopInfo*> UniformLoopInfoMapType;
	typedef std::set<Loop*>                         VaryingTopLevelLoopSetType;
	typedef std::map<const Instruction*, bool>      InputIndependentInstructionMapType;


private:
	LoopInfo* mLoopInfo;
	const bool mVerbose;

	ValueInfoMapType mValueInfoMap;
	BlockInfoMapType mBlockInfoMap;
	UniformLoopInfoMapType mUniformLoopInfoMap;
	VaryingTopLevelLoopSetType mVaryingTopLevelLoops;
	InputIndependentInstructionMapType mInputIndependentInstructions;

	std::set<const Instruction*> mScalarSet;

public:
	AnalysisResults(const bool verbose_flag=false) : mVerbose(verbose_flag) {}
	void setLoopInfo(LoopInfo* li) { mLoopInfo = li; }

	AnalysisResults(LoopInfo* li, const bool verbose_flag=false)
			: mLoopInfo(li), mVerbose(verbose_flag)
	{
		// add constants: true, false, 0, 0xffffffff, 1, 2, 3, ... info.simdWidth, undef
		// NOTE: "SPLIT_NEVER" might be overwritten
		addValueInfo(Constant::getAllOnesValue(Type::getInt1Ty(getGlobalContext())), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		addValueInfo(Constant::getNullValue(Type::getInt1Ty(getGlobalContext())), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		addValueInfo(UndefValue::get(Type::getFloatTy(getGlobalContext())), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		addValueInfo(Constant::getAllOnesValue(Type::getFloatTy(getGlobalContext())), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		addValueInfo(Constant::getNullValue(Type::getFloatTy(getGlobalContext())), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		addValueInfo(UndefValue::get(Type::getInt32Ty(getGlobalContext())), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		addValueInfo(Constant::getAllOnesValue(Type::getInt32Ty(getGlobalContext())), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		addValueInfo(Constant::getNullValue(Type::getInt32Ty(getGlobalContext())), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		//for (unsigned i=0, e=info.simdWidth; i!=e; ++i) {
			//addValueInfo(ConstantInt::get(getGlobalContext(), APInt(32, i)), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		//}
		//addValueInfo(UndefValue::get(info.vectorTy_boolSIMD), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		//addValueInfo(UndefValue::get(info.vectorTy_intSIMD), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		//addValueInfo(UndefValue::get(info.vectorTy_floatSIMD), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		//addValueInfo(Constant::getAllOnesValue(info.vectorTy_boolSIMD), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		//addValueInfo(Constant::getAllOnesValue(info.vectorTy_intSIMD), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		//addValueInfo(Constant::getAllOnesValue(info.vectorTy_floatSIMD), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		//addValueInfo(Constant::getNullValue(info.vectorTy_boolSIMD), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		//addValueInfo(Constant::getNullValue(info.vectorTy_intSIMD), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		//addValueInfo(Constant::getNullValue(info.vectorTy_floatSIMD), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		//for (unsigned i=0, e=8; i!=e; ++i) {
			//addValueInfo(ConstantInt::get(getGlobalContext(), APInt(8, i)), UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
		//}
	}

	~AnalysisResults() {
//		DEBUG_PKT( outs() << "deleting " << valueInfoMap.size()
//				<< " elements from VectorizationAnalysis::valueInfoMap...\n"; );
//		for (ValueInfoMapType::const_iterator V=valueInfoMap.begin(),
//				VE=valueInfoMap.end(); V!=VE; ++V)
//		{
//			delete V->second;
//		}
//
//		for (BlockInfoMapType::const_iterator B=blockInfoMap.begin(),
//				BE=blockInfoMap.end(); B!=BE; ++B)
//		{
//			delete B->second;
//		}
//
//		for (UniformLoopInfoMapType::const_iterator L=uniformLoopInfoMap.begin(),
//				LE=uniformLoopInfoMap.end(); L!=LE; ++L)
//		{
//			delete L->second;
//		}
	}

	void print(raw_ostream &O, const Module* M=NULL) const
	{
		if (mValueInfoMap.empty()) O << "\nvalue info map is empty!\n";
		else O << "\nvalue info map:\n";
		for (ValueInfoMapType::const_iterator V=mValueInfoMap.begin(),
				VE=mValueInfoMap.end(); V!=VE; ++V)
		{
			V->second->print(O);
		}
		O << "\n";

		if (mBlockInfoMap.empty()) O << "\nblock info map is empty!\n";
		else O << "\nblock info map:\n";
		for (BlockInfoMapType::const_iterator B=mBlockInfoMap.begin(),
				BE=mBlockInfoMap.end(); B!=BE; ++B)
		{
			B->second->print(O);
		}
		O << "\n";

		if (mUniformLoopInfoMap.empty()) O << "\nloop info map is empty!\n";
		else O << "\nloop info map:\n";
		for (UniformLoopInfoMapType::const_iterator L=mUniformLoopInfoMap.begin(),
				LE=mUniformLoopInfoMap.end(); L!=LE; ++L)
		{
			L->second->print(O);
		}
		O << "\n\n";
	}


	bool verify(Function& source) const
	{
		bool successful = true;
		std::set<const Value*> visitedValues;
		for (Function::iterator BB=source.begin(), BBE=source.end(); BB!=BBE; ++BB) {
			// check instruction info
			for (BasicBlock::const_iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {
				successful &= verifyValue(I, visitedValues);
			}

			// check block info
			if (!hasBlockInfo(BB)) {
				errs() << "ERROR: block has not been marked properly: "
						<< BB->getName() << "\n";
				successful = false;
			}

			// check loop info
			// TODO: not possible if passmanagers etc. are destroyed already
			//       (which also destroys loopinfo). We need our own data
			//       structure for this.
			if (mLoopInfo) {
				if (mLoopInfo->isLoopHeader(BB)) {
					const Loop* loop = mLoopInfo->getLoopFor(BB);
					assert (loop);
					if (!hasUniformLoopInfo(loop)) {
						errs() << "ERROR: loop with header " << BB->getName() << " has not been marked properly!\n";
						successful = false;
					}
				}
			}
		}

		// Test all values that are in the map
		for (AnalysisResults::const_iterator V=begin(), VE=end(); V!=VE; ++V) {
			successful &= verifyValue(V->first, visitedValues);
		}

		return successful;
	}


	bool verifyValue(const Value* value, std::set<const Value*>& visitedValues) const
	{
		assert (value);
		if (visitedValues.find(value) != visitedValues.end()) return true;
		visitedValues.insert(value);

		if (!hasValueInfo(value)) {
			errs() << "ERROR: value has not been marked: " << *value << "\n";
			return false;
		}

		bool successful = true;

		const ValueInfo* vi = getValueInfo(value);
		if (vi->alignmentInfo == ALIGN_NOT_INITIALIZED ||
				vi->indexInfo == INDEX_NOT_INITIALIZED ||
				vi->uniformInfo == UNIFORM_NOT_INITIALIZED ||
				vi->splitInfo == SPLIT_NOT_INITIALIZED ||
				vi->alignmentInfo == ALIGN_UNKNOWN ||
				vi->indexInfo == INDEX_UNKNOWN ||
				vi->uniformInfo == UNIFORM_UNKNOWN ||
				vi->splitInfo == SPLIT_UNKNOWN)
		{
			errs() << "ERROR: value has incomplete information: "
				<< *value << "\n";
			vi->print(errs());
			successful = false;
		}

		if (!isUniform(value) && vi->indexInfo == INDEX_SAME) {
			errs() << "ERROR: VARYING value must not be INDEX_SAME: "
					<< *value << "\n";
			successful = false;
		}

		if (isUniform(value) && vi->indexInfo != INDEX_SAME) {
			errs() << "ERROR: UNIFORM value must not be "
					<< getIndexInfoString(vi->indexInfo) << ": "
					<< *value << "\n";
			successful = false;
		}

		if (!isa<Instruction>(value)) return successful;

		// if this is an instruction that takes a condition, check if the
		// condition is marked as MASK.
		if (const BranchInst* br = dyn_cast<BranchInst>(value)) {
			if (br->isConditional() && !isMask(br->getCondition())) {
				errs() << "ERROR: Branch condition must be marked MASK: "
						<< *br->getCondition() << "\n";
				successful = false;
			}
		} else if (const SelectInst* sel = dyn_cast<SelectInst>(value)) {
			if (!isMask(sel->getCondition())) {
				errs() << "ERROR: Select condition must be marked MASK: "
						<< *sel->getCondition() << "\n";
				successful = false;
			}
		}

		// if this is an instruction, check operands
		const Instruction* valI = cast<Instruction>(value);
		for (Instruction::const_op_iterator O=valI->op_begin(), OE=valI->op_end(); O!=OE; ++O) {
			if (isa<Function>(*O)) continue;
			if (isa<BasicBlock>(*O)) continue;
			assert (isa<Value>(*O));
			successful &= verifyValue(cast<Value>(*O), visitedValues);
		}

		return successful;
	}


	typedef ValueInfoMapType::iterator       iterator;
	typedef ValueInfoMapType::const_iterator const_iterator;
	iterator		begin()       { return mValueInfoMap.begin(); }
	const_iterator	begin() const { return mValueInfoMap.begin(); }
	iterator		end  ()       { return mValueInfoMap.end();   }
	const_iterator	end  () const { return mValueInfoMap.end();   }

	typedef BlockInfoMapType::iterator       block_iterator;
	typedef BlockInfoMapType::const_iterator block_const_iterator;
	block_iterator			block_begin()       { return mBlockInfoMap.begin(); }
	block_const_iterator	block_begin() const { return mBlockInfoMap.begin(); }
	block_iterator			block_end  ()       { return mBlockInfoMap.end();   }
	block_const_iterator	block_end  () const { return mBlockInfoMap.end();   }

	typedef UniformLoopInfoMapType::iterator       uniformloop_iterator;
	typedef UniformLoopInfoMapType::const_iterator uniformloop_const_iterator;
	uniformloop_iterator		uniformloop_begin()       { return mUniformLoopInfoMap.begin(); }
	uniformloop_const_iterator	uniformloop_begin() const { return mUniformLoopInfoMap.begin(); }
	uniformloop_iterator		uniformloop_end  ()       { return mUniformLoopInfoMap.end();   }
	uniformloop_const_iterator	uniformloop_end  () const { return mUniformLoopInfoMap.end();   }

	typedef VaryingTopLevelLoopSetType::iterator       varyingtoplevelloop_iterator;
	typedef VaryingTopLevelLoopSetType::const_iterator varyingtoplevelloop_const_iterator;
	varyingtoplevelloop_iterator		varyingtoplevelloop_begin()       { return mVaryingTopLevelLoops.begin(); }
	varyingtoplevelloop_const_iterator	varyingtoplevelloop_begin() const { return mVaryingTopLevelLoops.begin(); }
	varyingtoplevelloop_iterator		varyingtoplevelloop_end  ()       { return mVaryingTopLevelLoops.end();   }
	varyingtoplevelloop_const_iterator	varyingtoplevelloop_end  () const { return mVaryingTopLevelLoops.end();   }

	typedef InputIndependentInstructionMapType::iterator       inputindependentvalue_iterator;
	typedef InputIndependentInstructionMapType::const_iterator inputindependentvalue_const_iterator;
	inputindependentvalue_iterator			inputindependentvalue_begin()       { return mInputIndependentInstructions.begin(); }
	inputindependentvalue_const_iterator	inputindependentvalue_begin() const { return mInputIndependentInstructions.begin(); }
	inputindependentvalue_iterator			inputindependentvalue_end  ()       { return mInputIndependentInstructions.end();   }
	inputindependentvalue_const_iterator	inputindependentvalue_end  () const { return mInputIndependentInstructions.end();   }

	bool isUniform(const Value* value) const
	{
		assert (value);
		assert (!isa<BasicBlock>(value));
		assert (!isa<Function>(value));

		if (isa<Constant>(value)) return true; // do not have to search map ;)
		const ValueInfo* vi = getValueInfo(value);
		assert (vi->uniformInfo != UNIFORM_UNKNOWN &&
				vi->uniformInfo != UNIFORM_NOT_INITIALIZED);
		return vi->uniformInfo == UNIFORM;
	}

	bool isSame(const Value* value) const
	{
		assert (value);
		assert (!isa<BasicBlock>(value));
		assert (!isa<Function>(value));

		if (isa<Constant>(value)) return true; // do not have to search map ;)
		const ValueInfo* vi = getValueInfo(value);
		if (vi->indexInfo == INDEX_UNKNOWN ||
				vi->indexInfo == INDEX_NOT_INITIALIZED)
		{
			errs() << "WARNING: incomplete index information found"
				<< " for value: " << *value << "\n";
			assert (false && "incomplete index information found!");
		}

		return vi->indexInfo == INDEX_SAME;
	}

	bool isConsecutive(const Value* value) const
	{
		assert (value);
		assert (!isa<BasicBlock>(value));
		assert (!isa<Function>(value));

		if (isa<Constant>(value)) return false; // do not have to search map ;)
		const ValueInfo* vi = getValueInfo(value);
		if (vi->indexInfo == INDEX_UNKNOWN ||
				vi->indexInfo == INDEX_NOT_INITIALIZED)
		{
			errs() << "WARNING: incomplete index information found"
				<< " for value: " << *value << "\n";
			assert (false && "incomplete index information found!");
		}
		return vi->indexInfo == INDEX_CONSECUTIVE;
	}

	bool isRandom(const Value* value) const
	{
		assert (value);
		assert (!isa<BasicBlock>(value));
		assert (!isa<Function>(value));

		if (isa<Constant>(value)) return false; // do not have to search map ;)
		const ValueInfo* vi = getValueInfo(value);
		if (vi->indexInfo == INDEX_UNKNOWN ||
				vi->indexInfo == INDEX_NOT_INITIALIZED)
		{
			errs() << "WARNING: incomplete index information found"
				<< " for value: " << *value << "\n";
			assert (false && "incomplete index information found!");
		}
		return vi->indexInfo == INDEX_RANDOM;
	}

	bool isAligned(const Value* value) const
	{
		assert (value);
		assert (!isa<BasicBlock>(value));
		assert (!isa<Function>(value));

		if (isa<Constant>(value)) return true; // do not have to search map ;)
		const ValueInfo* vi = getValueInfo(value);
		if (vi->alignmentInfo == ALIGN_UNKNOWN ||
				vi->alignmentInfo == ALIGN_NOT_INITIALIZED)
		{
			errs() << "WARNING: incomplete alignment information found"
				<< " for value: " << *value << "\n";
			assert (false && "incomplete alignment information found!");
		}
		return vi->alignmentInfo == ALIGN_TRUE;
	}

	bool requiresReplication(const Value* value) const
	{
		assert (value);
		assert (!isa<BasicBlock>(value));
		assert (!isa<Function>(value));

		const ValueInfo* vi = getValueInfo(value);
		if (vi->splitInfo == SPLIT_UNKNOWN ||
				vi->splitInfo == SPLIT_NOT_INITIALIZED)
		{
			errs() << "WARNING: incomplete split information found"
				<< " for value: " << *value << "\n";
			assert (false && "incomplete split information found!");
		}
		return vi->splitInfo == SPLIT_REPLICATE;
	}

	bool requiresSplitResult(const Value* value) const
	{
		assert (value);
		assert (!isa<BasicBlock>(value));
		assert (!isa<Function>(value));

		const ValueInfo* vi = getValueInfo(value);
		if (vi->splitInfo == SPLIT_UNKNOWN ||
				vi->splitInfo == SPLIT_NOT_INITIALIZED)
		{
			errs() << "WARNING: incomplete split information found"
				<< " for value: " << *value << "\n";
			assert (false && "incomplete split information found!");
		}
		return vi->splitInfo == SPLIT_RESULT;
	}

	bool requiresSplitFull(const Value* value) const
	{
		assert (value);
		assert (!isa<BasicBlock>(value));
		assert (!isa<Function>(value));

		const ValueInfo* vi = getValueInfo(value);
		if (vi->splitInfo == SPLIT_UNKNOWN ||
				vi->splitInfo == SPLIT_NOT_INITIALIZED)
		{
			errs() << "WARNING: incomplete split information found"
				<< " for value: " << *value << "\n";
			assert (false && "incomplete split information found!");
		}
		return vi->splitInfo == SPLIT_FULL ||
				vi->splitInfo == SPLIT_FULL_GUARDED;
	}

	bool requiresSplitFullGuarded(const Value* value) const
	{
		assert (value);
		assert (!isa<BasicBlock>(value));
		assert (!isa<Function>(value));

		const ValueInfo* vi = getValueInfo(value);
		if (vi->splitInfo == SPLIT_UNKNOWN ||
				vi->splitInfo == SPLIT_NOT_INITIALIZED)
		{
			errs() << "WARNING: incomplete split information found"
				<< " for value: " << *value << "\n";
			assert (false && "incomplete split information found!");
		}
		return vi->splitInfo == SPLIT_FULL_GUARDED;
	}


	bool isMask(const Value* value) const
	{
		assert (value);
		assert (!isa<BasicBlock>(value));
		assert (!isa<Function>(value));

		assert (!isa<Constant>(value) && "NOT IMPLEMENTED FOR CONSTANTS!");
		const ValueInfo* vi = getValueInfo(value);
		return vi->isMask;
	}

	bool hasUniformEntry(const BasicBlock* block) const
	{
		assert (block);
		BlockInfo* bi = getBlockInfo(block);
		return bi->isNonDivergent;
	}
	bool hasFullyUniformEntry(const BasicBlock* block) const
	{
		assert (block);
		BlockInfo* bi = getBlockInfo(block);
		return bi->isFullyNonDivergent;
	}
	bool hasUniformExit(const BasicBlock* block) const
	{
		assert (block);
		BlockInfo* bi = getBlockInfo(block);
		return bi->hasUniformExit;
	}
	bool hasFullyUniformExit(const BasicBlock* block) const
	{
		assert (block);
		BlockInfo* bi = getBlockInfo(block);
		return bi->hasFullyUniformExit;
	}

	bool isUniform(const Loop* loop) const
	{
		assert (loop);
		UniformLoopInfo* uli = getUniformLoopInfo(loop);
		return uli->isNonDivergent;
	}
	bool isFullyUniform(const Loop* loop) const
	{
		assert (loop);
		UniformLoopInfo* uli = getUniformLoopInfo(loop);
		return uli->isFullyNonDivergent;
	}
	bool isVaryingTopLevelLoop(const Loop* loop) const
	{
		assert (loop);
		UniformLoopInfo* uli = getUniformLoopInfo(loop);
		return uli->isVaryingTopLevel;
	}
	bool isVaryingInnermostLoop(const Loop* loop) const
	{
		assert (loop);
		UniformLoopInfo* uli = getUniformLoopInfo(loop);
		return uli->isVaryingInnermost;
	}
	bool isUniformLoopBlock(const BasicBlock* block) const
	{
		assert (block);
		if (!mLoopInfo) {
			outs() << "ERROR: isUniformLoopBlock() should not be called "
					<< "after LoopInfo was destroyed!\n";
			return false;
		}
		const Loop* loop = mLoopInfo->getLoopFor(block);
		if (!loop) return false;
		UniformLoopInfo* uli = getUniformLoopInfo(loop);
		return uli->isNonDivergent;
	}
	bool isFullyUniformLoopBlock(const BasicBlock* block) const
	{
		assert (block);
		if (!mLoopInfo) {
			outs() << "ERROR: isUniformLoopBlock() should not be called "
					<< "after LoopInfo was destroyed!\n";
			return false;
		}
		const Loop* loop = mLoopInfo->getLoopFor(block);
		UniformLoopInfo* uli = getUniformLoopInfo(loop);
		return uli->isFullyNonDivergent;
	}


	void setUniformInfo(const Value* value, UniformInfo ui) {
		assert (value);
		ValueInfo* vi = getValueInfo(value);

		// prevent overwriting with weaker information
		if (ui == UNIFORM && vi->uniformInfo == VARYING) {
			assert (false && "attempting to overwrite VARYING with UNIFORM!");
			return;
		}
		if (ui == FULLY_UNIFORM && vi->uniformInfo == VARYING) {
			assert (false && "attempting to overwrite VARYING with FULLY_UNIFORM!");
			return;
		}
		vi->uniformInfo = ui;
	}
	void setIndexInfo(const Value* value, IndexInfo ii) {
		assert (value);
		ValueInfo* vi = getValueInfo(value);

		// prevent overwriting with weaker information
		IndexInfo oldii = vi->indexInfo;
		if (ii == INDEX_CONSECUTIVE &&
				(oldii == INDEX_SHUFFLE ||
				 oldii == INDEX_STRIDED ||
				 oldii == INDEX_RANDOM))
		{
			assert (false && "attempting to overwrite INDEX_SHUFFLE, INDEX_STRIDED, or INDEX_RANDOM with INDEX_CONSECUTIVE!");
			return;
		}

		if (ii == INDEX_SAME &&
				oldii != INDEX_NOT_INITIALIZED &&
				oldii != INDEX_UNKNOWN &&
				oldii != INDEX_SAME)
		{
			assert (false && "attempting to overwrite INDEX_SHUFFLE, INDEX_STRIDED, INDEX_RANDOM, or INDEX_CONSECUTIVE with INDEX_SAME!");
			return;
		}

		if (isUniform(value) && ii != INDEX_SAME) {
			errs() << "attempting to mark UNIFORM value as "
					<< getIndexInfoString(ii) << ": " << *value << "\n";
			assert (false && "attempting to mark UNIFORM value as something else than INDEX_SAME!");
			return;
		}

		if (!isUniform(value) && ii == INDEX_SAME) {
			DEBUG_PKT( errs() << "WARNING: attempted to mark VARYING value as "
					<< "INDEX_SAME: " << *value << " - marking as INDEX_CONSECUTIVE "
					<< "(possibly introducing some imprecision)\n"; );
			vi->indexInfo = INDEX_CONSECUTIVE;
		} else {
			vi->indexInfo = ii;
		}
	}
	void setAlignmentInfo(const Value* value, AlignmentInfo ai) {
		assert (value);
		ValueInfo* vi = getValueInfo(value);

		if (ai == ALIGN_TRUE && vi->alignmentInfo == ALIGN_FALSE) {
			assert (false && "attempting to overwrite ALIGN_FALSE with ALIGN_TRUE!");
			return;
		}
		vi->alignmentInfo = ai;
	}
	void setSplitInfo(const Value* value, SplitInfo si, bool forceUpdate=false) {
		assert (value);
		ValueInfo* vi = getValueInfo(value);
		SplitInfo oldsi = vi->splitInfo;

		if (!forceUpdate &&
				si == SPLIT_NEVER &&
				(oldsi == SPLIT_REPLICATE ||
				 oldsi == SPLIT_RESULT ||
				 oldsi == SPLIT_FULL ||
				 oldsi == SPLIT_FULL_GUARDED))
		{
			assert (false && "attempting to overwrite SPLIT_REPLICATE, SPLIT_RESULT, SPLIT_FULL, or SPLIT_FULL_GUARDED with SPLIT_NEVER!");
			return;
		}
		assert ((forceUpdate ||
				(!isUniform(value) ||
				(si != SPLIT_RESULT &&
				 si != SPLIT_FULL &&
				 si != SPLIT_FULL_GUARDED))) &&
				"attempting to mark UNIFORM value as SPLIT_RESULT, SPLIT_FULL, or SPLIT_FULL_GUARDED!");

		vi->splitInfo = si;
	}
	void setIsMask(const Value* value, const bool isMask) {
		assert (value);
		ValueInfo* vi = getValueInfo(value);
		vi->isMask = isMask;
	}


	inline bool hasValueInfo() const {
		return !mValueInfoMap.empty();
	}

	inline bool hasValueInfo(const Value* value) const {
		assert (value);
		return mValueInfoMap.find(value) != mValueInfoMap.end();
	}

	ValueInfo* getValueInfo(const Value* value) const {
		assert (value);
		ValueInfoMapType::const_iterator it = mValueInfoMap.find(value);
		if (it == mValueInfoMap.end()) return NULL;
		return it->second;
	}

	bool addValueInfo(Value* value,
			UniformInfo ui=UNIFORM_NOT_INITIALIZED,
			IndexInfo ii=INDEX_NOT_INITIALIZED,
			AlignmentInfo ai=ALIGN_NOT_INITIALIZED,
			SplitInfo si=SPLIT_NOT_INITIALIZED,
			bool isMask=false)
	{
		assert (value);
		if (hasValueInfo(value)) {
			assert (false && "value is already in valueInfo map!");
			return false;
		}
		ValueInfo* vi = new ValueInfo(value);
		vi->uniformInfo = ui;
		vi->indexInfo = ii;
		vi->alignmentInfo = ai;
		vi->splitInfo = si;
		vi->isMask = isMask;

		mValueInfoMap.insert(std::make_pair(value, vi));
		return true;
	}

	inline bool addValueInfo(Value* value, const Value* copyFrom) {
		assert (value && copyFrom);
		const ValueInfo* copyInfo = getValueInfo(copyFrom);
		assert (copyInfo);

		return addValueInfo(value,
							copyInfo->uniformInfo,
							copyInfo->indexInfo,
							copyInfo->alignmentInfo,
							copyInfo->splitInfo,
							copyInfo->isMask);
	}

	inline bool addValueInfoFalse(Value* value) {
		assert (value);
		return addValueInfo(value, VARYING, INDEX_RANDOM, ALIGN_FALSE, SPLIT_NEVER, false);
	}

	bool removeValueInfo(Value* value) {
		ValueInfoMapType::iterator it = mValueInfoMap.find(value);
		if (it == mValueInfoMap.end()) {
			assert (false && "value is not in valueInfo map!");
			return false;
		}
		delete it->second;
		mValueInfoMap.erase(it);
		return true;
	}

	

	inline bool hasBlockInfo(BasicBlock* block) const {
		assert (block);
		return mBlockInfoMap.find(block) != mBlockInfoMap.end();
	}

	inline BlockInfo* getBlockInfo(const BasicBlock* block) const {
		assert (block);
		BlockInfoMapType::const_iterator it = mBlockInfoMap.find(block);
		if (it == mBlockInfoMap.end()) return NULL;
		return it->second;
	}

	bool addBlockInfo(BasicBlock* block,
					  bool uniformEntry,
					  bool fullyUniformEntry,
					  bool uniformExit,
					  bool fullyUniformExit)
	{
		if (hasBlockInfo(block)) {
			assert (false && "block is already in blockInfo map!");
			return false;
		}
		BlockInfo* bi = new BlockInfo(block,
									  uniformEntry,
									  fullyUniformEntry,
									  uniformExit,
									  fullyUniformExit);

		std::pair<BlockInfoMapType::iterator, bool> res =
			mBlockInfoMap.insert(std::make_pair(block, bi));

		if (!res.second) {
			assert (false && "insertion of block info failed!");
			return false;
		}
		return true;
	}

	bool removeBlockInfo(BasicBlock* block) {
		BlockInfoMapType::iterator it = mBlockInfoMap.find(block);
		if (it == mBlockInfoMap.end()) {
			assert (false && "block is not in blockInfo map!");
			return false;
		}
		delete it->second;
		mBlockInfoMap.erase(it);
		return true;
	}



	inline bool hasUniformLoopInfo(const Loop* loop) const {
		assert (loop);
		return mUniformLoopInfoMap.find(loop) != mUniformLoopInfoMap.end();
	}

	inline UniformLoopInfo* getUniformLoopInfo(const Loop* loop) const {
		assert (loop);
		UniformLoopInfoMapType::const_iterator it = mUniformLoopInfoMap.find(loop);
		assert (it != mUniformLoopInfoMap.end());
		return it->second;
	}

	bool addUniformLoopInfo(const Loop* loop,
					  bool isUniform,
					  bool isFullyUniform)
	{
		assert (loop);
		if (hasUniformLoopInfo(loop)) {
			assert (false && "loop is already in loopInfo map!");
			return false;
		}
		UniformLoopInfo* uli = new UniformLoopInfo(loop,
									  isUniform,
									  isFullyUniform);
		mUniformLoopInfoMap.insert(std::make_pair(loop, uli));
		return true;
	}

	bool removeUniformLoopInfo(const Loop* loop) {
		assert (loop);
		UniformLoopInfoMapType::iterator it = mUniformLoopInfoMap.find(loop);
		if (it == mUniformLoopInfoMap.end()) {
			assert (false && "loop is not in loopInfo map!");
			return false;
		}
		delete it->second;
		mUniformLoopInfoMap.erase(it);
		return true;
	}




	inline UniformInfo joinUniformInfo(const Value* val0, const Value* val1) const
	{
		assert (val0 && val1);
		assert (!isa<BasicBlock>(val0) && !isa<BasicBlock>(val1));
		assert (!isa<Function>(val0) && !isa<Function>(val1));

		if (isa<Constant>(val0)) {
			if (isa<Constant>(val1)) return FULLY_UNIFORM;
			ValueInfo* vi = getValueInfo(val1);
			return vi->uniformInfo;
		}
		if (isa<Constant>(val1)) {
			ValueInfo* vi = getValueInfo(val0);
			return vi->uniformInfo;
		}

		ValueInfo* vi0 = getValueInfo(val0);
		ValueInfo* vi1 = getValueInfo(val1);
		return joinUniformInfo(vi0->uniformInfo, vi1->uniformInfo);
	}

	inline UniformInfo joinUniformInfo(const UniformInfo& a, const UniformInfo& b) const
	{
		// at least one info must be initialized
		assert (a != UNIFORM_NOT_INITIALIZED || b != UNIFORM_NOT_INITIALIZED);
		// If one value is not initialized yet, return the other
		if (a == UNIFORM_NOT_INITIALIZED) return b;
		if (b == UNIFORM_NOT_INITIALIZED) return a;
		// If any value is VARYING, the join is also VARYING.
		if (a == VARYING || b == VARYING) return VARYING;
		// If any value is not known yet while the other is
		// also unknown or uniform, the join is still UNKNOWN.
		if (a == UNIFORM_UNKNOWN || b == UNIFORM_UNKNOWN) return UNIFORM_UNKNOWN;
		// If any value is UNIFORM (not FULLY_UNIFORM, the join is also UNIFORM.
		if (a == UNIFORM || b == UNIFORM) return UNIFORM;
		// Otherwise, we can safely say the value is FULLY_UNIFORM :).
		return FULLY_UNIFORM;
	}

	/**
	 * Recursively test uniform info of operands of 'value' to deduce the
	 * corresponding information for 'value' itself.
	 * On the way back down, mark all values accordingly
	 *
     * @param value
     * @param visitedSet
     * @return
     */
	UniformInfo deriveUniformInfo(Value* value, std::set<Value*>& visitedSet)
	{
		if (isa<BasicBlock>(value)) return UNIFORM; // simply return the "weakest" information ;)
		assert (!isa<Function>(value));

		DEBUG_PKT( outs() << "deriving uniform info for value " << *value << "...\n"; );
		// Stop recursion if the value is already in the uniform or varying set
		// This counts especially for function arguments
		const ValueInfo* vi = hasValueInfo(value) ? getValueInfo(value) : NULL;
		if (vi &&
				vi->uniformInfo != UNIFORM_NOT_INITIALIZED &&
				vi->uniformInfo != UNIFORM_UNKNOWN)
		{
			DEBUG_PKT( outs() << "  value already marked as "
					<< getUniformInfoString(vi->uniformInfo) << "\n"; );
			return vi->uniformInfo;
		}

		// stop recursion if this is a constant (-> mark as uniform)
		if (isa<Constant>(value)) {
			addValueInfo(value, UNIFORM, INDEX_SAME, ALIGN_TRUE, SPLIT_NEVER);
			DEBUG_PKT( outs() << "  marked constant: " <<
					*value << " as UNIFORM / INDEX_SAME / ALIGN_TRUE / SPLIT_NEVER\n"; );
			return FULLY_UNIFORM;
		}

		// If neither of the above cases happened but we have already seen
		// the value, we don't know anything about it.
		if (visitedSet.find(value) != visitedSet.end()) {
			DEBUG_PKT( outs() << "  value already seen (unknown or not initialized): " << *value << "\n"; );
			return UNIFORM_UNKNOWN;
		}

		visitedSet.insert(value);

		// At this point, the value has to be an instruction.
		assert (isa<Instruction>(value));
		Instruction* valInst = cast<Instruction>(value);

		// Recursively test operands
		Instruction::op_iterator O = valInst->op_begin();
		int idx = 0;
		UniformInfo info = UNIFORM_NOT_INITIALIZED;
		for (Instruction::op_iterator OE=valInst->op_end(); O!=OE; ++O, ++idx) {
			if (!isa<Value>(O)) continue; // ignore blocks etc.

			// we have to ignore condition of selects
			// TODO: don't think so. although a select with uniform values
			//       and varying condition is... interesting
			//if (isa<SelectInst>(valInst) && idx == 0) continue;

			UniformInfo ui = deriveUniformInfo(cast<Value>(O), visitedSet);
			//DEBUG_PKT( outs() << "  operand " << idx << " is " << getUniformInfoString(ui) << "\n"; );
			info = joinUniformInfo(info, ui);
		}
		if (valInst->getNumOperands() == 0) info = UNIFORM_UNKNOWN;
		assert (info != UNIFORM_NOT_INITIALIZED);

		// mark current value accordingly
		addValueInfo(value, info);
		DEBUG_PKT( outs() << "  marked value: " << *value << " as "
				<< getUniformInfoString(info) << "\n"; );

		return info;
	}

	// Check uses of mask value for instructions marked SPLIT_FULL or
	// SPLIT_FULL_GUARDED.
	// If any was found, return SPLIT_REPLICATE if the mask is uniform, return
	// SPLIT_RESULT if the mask is varying.
	// Otherwise, return SPLIT_NEVER.
	// NOTE: It is never be required to fully split a mask value, so we never
	//       return SPLIT_FULL or SPLIT_FULL_GUARDED.
	SplitInfo deriveMaskSplitInfo(const Value* value) {
		assert (value);
		assert (!isa<BasicBlock>(value) && !isa<Function>(value));
		assert (isMask(value));

		DEBUG_PKT( outs() << "deriving split info for mask value " << *value << "...\n"; );
		// Stop recursion if the value already has valid split information.
		// This counts especially for function arguments
		const ValueInfo* vi = getValueInfo(value);

		if (vi->splitInfo != SPLIT_NOT_INITIALIZED &&
				vi->splitInfo != SPLIT_UNKNOWN)
		{
			DEBUG_PKT( outs() << "  value already marked as "
					<< getSplitInfoString(vi->splitInfo) << "\n"; );
			return vi->splitInfo;
		}

		const bool uniformMask = isUniform(value);
		DEBUG_PKT( if (uniformMask) outs() << "  mask is UNIFORM!\n"; );
		DEBUG_PKT( if (!uniformMask) outs() << "  mask is VARYING!\n"; );

		for (Value::const_use_iterator U=value->use_begin(), UE=value->use_end();
				U!=UE; ++U)
		{
			assert (isa<Value>(*U));
			const Value* useVal = cast<Value>(*U);

			const ValueInfo* vi2 = getValueInfo(useVal);

			if (vi2->splitInfo == SPLIT_FULL ||
					vi2->splitInfo == SPLIT_FULL_GUARDED)
			{
				DEBUG_PKT( outs() << "  found a use that has to be split - "
						<< "marking mask as "; );
				DEBUG_PKT( if (uniformMask) outs() << "SPLIT_REPLICATE!\n"; );
				DEBUG_PKT( if (!uniformMask) outs() << "SPLIT_RESULT!\n"; );
				return uniformMask ? SPLIT_REPLICATE : SPLIT_RESULT;
			}
		}

		DEBUG_PKT( outs() << "  no uses found that have to be split -"
				<< " marking mask as SPLIT_NEVER!\n"; );

		return SPLIT_NEVER;
	}



	// TODO: unify this with UniformLoopInfo ?!
	bool addVaryingTopLevelLoop(Loop* loop)
	{
		assert (loop);
		return mVaryingTopLevelLoops.insert(loop).second;
	}

	//bool isVaryingTopLevelLoop(const Loop* loop) const
	//{
		//assert (loop);
		//return varyingTopLevelLoops.find(loop) != varyingTopLevelLoops.end();
	//}

	bool removeVaryingTopLevelLoop(Loop* loop) {
		assert (loop);
		VaryingTopLevelLoopSetType::iterator it = mVaryingTopLevelLoops.find(loop);
		if (it == mVaryingTopLevelLoops.end()) {
			assert (false && "loop is not in loopInfo map!");
			return false;
		}
		mVaryingTopLevelLoops.erase(it);
		return true;
	}



	bool isInputIndependent(const Instruction* value) const {
		assert (value);
		InputIndependentInstructionMapType::const_iterator it = mInputIndependentInstructions.find(value);
		return it != mInputIndependentInstructions.end();
	}

	void addInputIndependentValue(const Instruction* value, const bool isVaryingDueToVaryingLoop) {
		assert (value);
		assert (mInputIndependentInstructions.find(value) == mInputIndependentInstructions.end());
		mInputIndependentInstructions.insert(std::make_pair(value, isVaryingDueToVaryingLoop));
	}

	void removeInputIndependentValue(const Instruction* value) {
		assert (value);
		assert (mInputIndependentInstructions.find(value) != mInputIndependentInstructions.end());
		mInputIndependentInstructions.erase(value);
	}

	void updateInputIndependentValue(const Instruction* oldI, const Instruction* newI) {
		assert (oldI && newI);
		const bool isVDTVL = isVaryingDueToVaryingLoop(oldI);
		removeInputIndependentValue(oldI);
		addInputIndependentValue(newI, isVDTVL);
	}

	bool isVaryingDueToVaryingLoop(const Instruction* value) const {
		assert (value);

		// This prevents calculating the information on updated loopInfo
		// when called after CFG linearization.
		InputIndependentInstructionMapType::const_iterator it = mInputIndependentInstructions.find(value);
		assert (it != mInputIndependentInstructions.end());

		return it->second;
	}

	void setIsVaryingDueToVaryingLoop(const Instruction* value, bool isVarying) {
		assert (value);
		assert (isInputIndependent(value));

		InputIndependentInstructionMapType::iterator it = mInputIndependentInstructions.find(value);
		it->second = isVarying;
	}



	void addScalarInstruction(const Instruction* I) {
		assert (I);
		mScalarSet.insert(I);
	}

	inline bool instCanRemainScalar(const Instruction* I) const {
		assert (I);
		return mScalarSet.find(I) != mScalarSet.end();
	}

	void updateRemainScalarInfo(const Instruction* oldI, const Instruction* newI) {
		assert (oldI && newI);
		if (!instCanRemainScalar(oldI)) return;
		mScalarSet.erase(oldI);
		mScalarSet.insert(newI);
	}


};


#endif	/* _ANALYSISRESULTS_HPP */

