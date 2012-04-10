/**
 * @file   packetizer.hpp
 * @date   22.01.2009
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2008, 2009, 2010, 2011 Saarland University
 *
 */
#ifndef _PACKETIZER_HPP
#define	_PACKETIZER_HPP

#ifdef DEBUG_TYPE
#undef DEBUG_TYPE
#endif
#define DEBUG_TYPE "packetizer"

#include "packetizerConfig.hpp"

#include <map>
#include <vector>
#include <set>

#include "llvm/LLVMContext.h"
#include <llvm/Module.h>
#include <llvm/Pass.h>

#include "llvm/ADT/Statistic.h" //STATISTIC

#include "utils/packetizerInfo.hpp"
#include "utils/analysisResults.hpp"

using namespace llvm;

STATISTIC(PacketizedFunctionsCounter, "Counts number of packetized functions");


// forward declaration of initializer
namespace llvm {
	void initializeWholeFunctionVectorizerPass(PassRegistry&);
}

// only required for stupid dummy constructor (requires initialization of all fields)
static Module dummyModule("dummy", getGlobalContext());

class WholeFunctionVectorizer : public ModulePass {
public:
	static char ID; // Pass identification, replacement for typeid

	// darn dummy constructor requires initialization of all fields :p
	// -> only required if we need pass registration
	// -> we need pass registration for LoopInfo...
	WholeFunctionVectorizer() 
			: ModulePass(ID), simdWidth(0), packetizationSize(0), use_sse41(false), use_avx(false), verbose(true),
			info(Packetizer::PacketizerInfo(dummyModule, 0, 0, false, false, true)), nativeMethods(NativeMethods(use_sse41, use_avx, verbose))
	{
		errs() << "ERROR: empty constructor of class WholeFunctionVectorizer should never be called!\n";
	}
	
	WholeFunctionVectorizer(Module& M, const unsigned simd_width, const unsigned packetization_size, const bool use_sse41_flag, const bool use_avx_flag, const bool verbose_flag = false);
	~WholeFunctionVectorizer();
	virtual void releaseMemory();
	virtual bool runOnModule(Module& M);
	inline bool addFunction(const std::string& scalarName, const std::string& targetName);
	inline bool addVaryingFunctionMapping(const std::string& scalarName, const int maskIndex, Function* nativeF);
	inline bool addValueInfo(Value* value, const bool uniform, const bool consecutive, const bool aligned);
	bool isPacketizable(const Function* f) const;
	bool isPacketizableType(const Type* type) const;
	virtual void print(raw_ostream& o, const Module *M) const;
	virtual void getAnalysisUsage(AnalysisUsage& AU) const;
	bool wfvSuccessful(const std::string& simdFunctionName, const Module& module) const;
	
	Module& getModule() const { return info.module; }

	bool analyzeFunction(const std::string& scalarName, const std::string& targetName);
	bool isUniform(const llvm::Value* value) const;
	bool isSame(const llvm::Value* value) const;
	bool isConsecutive(const llvm::Value* value) const;
	bool isRandom(const llvm::Value* value) const;
	bool isAligned(const llvm::Value* value) const;
	bool isMask(const llvm::Value* value) const;

	bool requiresReplication(const llvm::Value* value) const;
	bool requiresSplitResult(const llvm::Value* value) const;
	bool requiresSplitFull(const llvm::Value* value) const;
	bool requiresSplitFullGuarded(const llvm::Value* value) const;

	bool isNonDivergent(const llvm::BasicBlock* block) const;
	bool isFullyNonDivergent(const llvm::BasicBlock* block) const;
	bool hasUniformExit(const llvm::BasicBlock* block) const;
	bool hasFullyUniformExit(const llvm::BasicBlock* block) const;

	bool isInputIndependent(const llvm::Instruction* value) const;

	struct NativeFunctionInfo {
		NativeFunctionInfo(Function* scalarF,
						   Function* nativeF,
						   const int maskIdx,
						   const bool uniformFlag,
						   const bool consecutiveFlag,
						   const bool alignedFlag)
				: scalarFn(scalarF),
				nativeFn(nativeF),
				maskIndex(maskIdx),
				uniform(uniformFlag),
				consecutive(consecutiveFlag),
				aligned(alignedFlag)
		{}

		Function* scalarFn;
		Function* nativeFn;
		const int maskIndex;
		const bool uniform;
		const bool consecutive;
		const bool aligned;
	};

	struct ValueInfo {
		ValueInfo(Value* v, bool _uniform, bool _consecutive, bool _aligned)
				: value(v), uniform(_uniform), consecutive(_consecutive), aligned(_aligned)
		{}

		Value* value;
		const bool uniform;
		const bool consecutive;
		const bool aligned;
	};

	typedef std::map<const Function*, NativeFunctionInfo* > NativeFunctionMapType;
	typedef std::map<Value*, const ValueInfo*> ValueInfoMapType;

private:
	const unsigned simdWidth;
	const unsigned packetizationSize;
	const bool use_sse41;
	const bool use_avx;
	const bool verbose;

	const Packetizer::PacketizerInfo info;

	//stores old and new names of all functions to be packetized
	std::map<const std::string, const std::string> functions;

	// stores native method mappings
	NativeFunctionMapType nativeFunctionMap;

	// stores uniform, index, and alignment information for values of ALL
	// target functions
	ValueInfoMapType globalValueInfoMap;

	// stores uniform, index, and alignment information for values of the
	// current function
	ValueInfoMapType valueInfoMap;

	//natively available SSE function mappings
	NativeMethods nativeMethods;

	// only to be used by analysis API (analyzeFunction() etc.)
	AnalysisResults* analysisResults;

	// Should never be called directly but only through runOnModule (exception handling)
	inline bool packetizeAllFunctions();
	bool packetizeFunction(const std::string& scalarName, const std::string& targetName);

	void cleanup();

	//custom transformations
	bool transformFunction(Function* f);
	bool transformConstants(Instruction* I);
	void transformCalls(Function* f);

	bool verifyFunctionSignaturesMatch(const Function* f, const Function* f_SIMD);
	bool verifyPacketizedType(const Type* scalarType, const Type* vecType);
};

char WholeFunctionVectorizer::ID = 0;
INITIALIZE_PASS_BEGIN(WholeFunctionVectorizer, "whole-function vectorization", "Whole-Function Vectorization", false, false)
INITIALIZE_PASS_DEPENDENCY(DominatorTree)
INITIALIZE_PASS_DEPENDENCY(PostDominatorTree)
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_END(WholeFunctionVectorizer, "whole-function vectorization", "Whole-Function Vectorization", false, false)

namespace llvm {
	ModulePass* createWholeFunctionVectorizerPass(Module& M, const unsigned simdWidth, const unsigned packetizationSize, const bool use_sse41, const bool use_avx, const bool verbose=false) {
		return new WholeFunctionVectorizer(M, simdWidth, packetizationSize, use_sse41, use_avx, verbose);
	}
}

#endif	/* _PACKETIZER_HPP */

