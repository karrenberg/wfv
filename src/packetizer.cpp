/**
 * @file   packetizer.cpp
 * @date   14.12.2008
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2008, 2009, 2010, 2011 Saarland University
 *
 */

// own header
#include "packetizer.hpp"

// API headers
#include "packetizerAPI.hpp"
#include "packetizerAPI_C.h"

//
// LLVM stuff
//
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/Timer.h>

#include <llvm/ADT/SmallVector.h>

#include <llvm/Module.h>

// function creation
#include <llvm/DerivedTypes.h> //VectorType
#include <llvm/CallingConv.h> //CallingConv C
#include <llvm/BasicBlock.h>
#include <llvm/Instructions.h> //Instruction
#include <llvm/Transforms/Utils/Cloning.h> //ClonedCodeInfo, CloneAndPruneFunctionInto
#include <llvm/Intrinsics.h>

// various packetizer requirements
#include <llvm/Support/CallSite.h>
#include <llvm/Target/TargetData.h>
#include <llvm/ExecutionEngine/GenericValue.h>

// passes
#include <llvm/Pass.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Transforms/Scalar.h> //LowerSwitch, LoopSimplify, BreakCriticalEdges, LCSSA
//#include <llvm/Transforms/Utils/UnifyFunctionExitNodes.h>

// only for static packetizeFunction
#include <llvm/PassManager.h> //Bitwriter & ModulePassManager
#include <llvm/Target/TargetData.h> //for ModulePassManager

//
// internal packetizer stuff
//
// passes
#include "functionPasses/returnUnifier.hpp"
#include "functionPasses/phiCanonicalizer.hpp"
#include "functionPasses/loopBranchCanonicalizer.hpp"
#include "functionPasses/memAccessCanonicalizer.hpp"
#include "functionPasses/vectorizationAnalysis.hpp"
#include "functionPasses/maskGenerator.hpp"
#include "functionPasses/selectGenerator.hpp"
#include "functionPasses/branchInfoAnalysis.hpp"
#include "functionPasses/cfgLinearizer.hpp"
#include "functionPasses/cfgLinearizerNaive.hpp"
#include "functionPasses/coherentMaskBranchGenerator.hpp"
#include "functionPasses/functionPacketizer.hpp"

// utils
#include "utils/wholeFunctionVectorizationAAW.hpp"
#include "utils/llvmTools.hpp"

//
// stdlib stuff
//
#include <list>
#include <stdexcept>

using namespace llvm;



//////////////////////////////////////////////////////////////////////////////////////////////////////////
// wrapper functions to use whole function vectorization without linking LLVM (only link libPacketizer) //
// (= implementation of packetizerAPI.hpp)                                                                //
//////////////////////////////////////////////////////////////////////////////////////////////////////////

namespace Packetizer {

PACKETIZER_API
Packetizer::Packetizer(llvm::Module& module,
                       llvm::LLVMContext& context,
                       const unsigned simdWidth,
                       const unsigned packetizationSize,
                       const bool use_sse41,
                       const bool use_avx,
                       const bool verbose)
{
	wfv = new WholeFunctionVectorizer(module,
                                         context,
                                         simdWidth,
                                         packetizationSize,
                                         use_sse41,
                                         use_avx,
                                         verbose);
}

PACKETIZER_API
Packetizer::~Packetizer()
{
    // PassManager deletes wfv already if run() was called
    if (wfv) {
        wfv->releaseMemory();
        delete wfv;
    }
}

PACKETIZER_API bool
Packetizer::addFunction(const std::string& functionName, const std::string& newName)
{
	assert (wfv && "Packetizer is not initialized correctly");
	return wfv->addFunction(functionName, newName);
}

PACKETIZER_API bool
Packetizer::addVaryingFunctionMapping(const std::string& scalarFunctionName,
                                      const int maskPosition,
                                      llvm::Function* packetFunction)
{
	assert (wfv && "Packetizer is not initialized correctly");
	return wfv->addVaryingFunctionMapping(scalarFunctionName, maskPosition, packetFunction);
}

PACKETIZER_API bool
Packetizer::addValueInfo(llvm::Value* value,
                         const bool uniform,
                         const bool consecutive,
                         const bool aligned)
{
	assert (wfv && "Packetizer is not initialized correctly");
	return wfv->addValueInfo(value, uniform, consecutive, aligned);
}

PACKETIZER_API void
Packetizer::run()
{
	assert (wfv && "Packetizer is not initialized correctly");
	PassManager PM;
	PM.add(new TargetData(wfv->getModule()));
	PM.add(wfv);
	PM.run(*wfv->getModule());
	wfv = NULL; // make sure we do not delete twice
}

PACKETIZER_API bool
Packetizer::wfvSuccessful(const std::string& simdFunctionName, const llvm::Module& module) const
{
	Function* f = module.getFunction(simdFunctionName);
	return f && !f->empty();
}

} // namespace Packetizer


////////////////////////////////////////////////////////////////
// C Binding implementation for packetizerAPI_C.h             //
////////////////////////////////////////////////////////////////

PKTPacketizerRef
PKTCreatePacketizer(LLVMModuleRef module,
                    LLVMContextRef context,
                    const unsigned simdWidth,
                    const unsigned packetizationSize,
                    const bool use_sse41,
                    const bool use_avx,
                    const bool verbose)
{
	return wrap(new Packetizer::Packetizer(*unwrap(module),
                                           *unwrap(context),
                                           simdWidth,
										   packetizationSize,
                                           use_sse41,
										   use_avx, verbose));
}

bool
PKTAddFunction(PKTPacketizerRef packetizerRef,
               const char* functionName,
               const char* newName)
{
	return Packetizer::unwrap(packetizerRef)->addFunction(functionName, newName);
}

bool
PKTAddVaryingFunctionMapping(PKTPacketizerRef packetizerRef,
                             const char* scalarFunctionName,
                             const int maskPosition,
                             LLVMValueRef packetFunction)
{
	return Packetizer::unwrap(packetizerRef)->
		addVaryingFunctionMapping(scalarFunctionName, maskPosition,
						  cast<Function>(unwrap(packetFunction)));
}

bool
PKTAddUniformVaryingInfo(PKTPacketizerRef packetizerRef,
						 LLVMValueRef value,
                         const bool uniform,
						 const bool consecutive,
                         const bool aligned)
{
	return Packetizer::unwrap(packetizerRef)->
		addValueInfo(cast<Value>(unwrap(value)),
					 uniform,
					 consecutive,
					 aligned);
}

void
PKTRun(PKTPacketizerRef packetizerRef)
{
	Packetizer::unwrap(packetizerRef)->run();
}

bool
PKTSuccessful(PKTPacketizerRef packetizerRef,
			  const char* simdFunctionName,
			  LLVMModuleRef moduleRef)
{
	Function* f = unwrap(moduleRef)->getFunction(simdFunctionName);
	return f && !f->empty();
}

////////////////////////////////////////////////////////////////
// Now comes the actual implementation of the WFV module pass //
////////////////////////////////////////////////////////////////


WholeFunctionVectorizer::WholeFunctionVectorizer(Module& M,
                                                 LLVMContext& C,
                                                 const unsigned simd_width,
                                                 const unsigned packetization_size,
                                                 const bool use_sse41_flag,
                                                 const bool use_avx_flag,
                                                 const bool verbose_flag)
: ModulePass(ID),
        mSimdWidth(simd_width),
        mPacketizationSize(packetization_size),
        mUseSSE41(use_sse41_flag),
        mUseAVX(use_avx_flag),
        mVerbose(verbose_flag),
        mInfo(new Packetizer::PacketizerInfo(&M,
                                             &C,
                                             simd_width,
                                             packetization_size,
                                             mUseSSE41,
                                             mUseAVX,
                                             mVerbose)),
        mAnalysisResults(NULL)
{
	initializeWholeFunctionVectorizerPass(*PassRegistry::getPassRegistry());
}

WholeFunctionVectorizer::~WholeFunctionVectorizer() {}

void
WholeFunctionVectorizer::releaseMemory()
{
	for (NativeFunctionMapType::iterator it=mNativeFunctionMap.begin(),
			E=mNativeFunctionMap.end(); it!=E; ++it)
	{
		delete it->second;
	}

	for (ValueInfoMapType::iterator it=mGlobalValueInfoMap.begin(),
			E=mGlobalValueInfoMap.end(); it!=E; ++it)
	{
		delete it->second;
	}
    if (mInfo) delete mInfo;
	if (mAnalysisResults) delete mAnalysisResults;
}

/// This function should be called *before* run()
inline bool
WholeFunctionVectorizer::addFunction(const std::string& scalarName, const std::string& targetName)
{
	if (scalarName == "") {
		errs() << "ERROR: function to be added for packetization must have a name!\n";
		return false;
	}
	if (targetName == "") {
		errs() << "ERROR: packetization prototype function must have a name!\n";
		return false;
	}
	if (!mInfo->mModule->getFunction(scalarName)) {
		errs() << "ERROR: function to be added for packetization does not exist in module: '" << scalarName << "'\n";
		return false;
	}
	if (!mInfo->mModule->getFunction(targetName)) {
		errs() << "ERROR: packetization prototype function does not exist in module: '" << targetName << "'\n";
		return false;
	}
	if (mFunctionNames.find(scalarName) != mFunctionNames.end()) {
		errs() << "ERROR: the same scalar function must not be added more than once: '" << scalarName << "'\n";
		return false;
	}
	for (std::map<const std::string, const std::string>::iterator
			it=mFunctionNames.begin(), E=mFunctionNames.end(); it!=E; ++it)
	{
		if (it->second == targetName) {
			errs() << "ERROR: the same target function must not be the target "
					<< "of multiple scalar functions: '" << targetName << "' "
					<< "(source 1: '" << it->first << "', source 2: '"
					<< scalarName << "')\n";
			return false;
		}
	}

	mFunctionNames.insert(std::make_pair(scalarName, targetName));
	return true;
}

/// This function should be called *before* run()
inline bool
WholeFunctionVectorizer::addVaryingFunctionMapping(const std::string& scalarName,
                                                   const int maskIndex,
                                                   Function* nativeF)
{
	if (!nativeF) {
		errs() << "ERROR: native varying function to be added must not be NULL!\n";
		return false;
	}

	Function* scalarFn = mInfo->mModule->getFunction(scalarName);
	if (!scalarFn) {
		errs() << "ERROR: scalar native function '" << scalarName
				<< "' does not exist in module!\n";
		return false;
	}

	if (maskIndex < -1 || maskIndex > (int)mInfo->mModule->getFunction(scalarName)->getFunctionType()->getNumParams()) {
		errs() << "ERROR: bad mask index found (should be between -1 (no mask) and " << mInfo->mModule->getFunction(scalarName)->getFunctionType()->getNumParams() << ")!\n";
		return false;
	}

	if (mNativeFunctionMap.find(scalarFn) != mNativeFunctionMap.end()) {
		errs() << "ERROR: adding more than one mapping for scalar function '"
				<< scalarFn->getName() << "' is not allowed!\n";
		return false;
	}

	// store info
	const bool uniform = false;     // by definition of this function :)
	const bool consecutive = false; // TODO: really?
	const bool aligned = false;     // TODO: really?
	NativeFunctionInfo* nfi = new NativeFunctionInfo(scalarFn, nativeF, maskIndex, uniform, consecutive, aligned);
	mNativeFunctionMap.insert(std::make_pair(scalarFn, nfi));

	// and add to native functions
	mInfo->mNativeMethods->addNativeFunction(scalarFn, nativeF, maskIndex);

	// Functions are not inserted into globalValueInfoMap, but their uses are.
	for (Function::use_iterator U=scalarFn->use_begin(),
			UE=scalarFn->use_end(); U!=UE; ++U)
	{
		assert (isa<Instruction>(*U));
		Instruction* useI = cast<Instruction>(*U);
		assert (useI->getParent() && useI->getParent()->getParent());
		addValueInfo(useI, uniform, consecutive, aligned);
	}
	return true;
}

// This function has no means to supply a maskIndex or native function mapping.
inline bool
WholeFunctionVectorizer::addValueInfo(Value* value,
                                      const bool uniform,
                                      const bool consecutive,
                                      const bool aligned)
{
	if (!value) {
		errs() << "ERROR: " << (uniform ? "UNIFORM" : "VARYING")
				<< " value must not be NULL!\n";
		return false;
	}

	if (uniform && consecutive) {
		errs() << "ERROR: UNIFORM value can not be CONSECUTIVE: '" << *value << "'\n";
		return false;
	}

	if (isa<BasicBlock>(value)) {
		errs() << "ERROR: adding basic blocks as UNIFORM/VARYING is not supported!\n";
		return false;
	}

	if (Function* fn = dyn_cast<Function>(value)) {
		// Add to native function map
		NativeFunctionInfo* nfi = new NativeFunctionInfo(fn,
                                                         NULL,
                                                         -1,
                                                         uniform,
                                                         consecutive,
                                                         aligned);
		mNativeFunctionMap.insert(std::make_pair(fn, nfi));

		// Functions are not inserted into globalValueInfoMap, but their uses are.
		for (Function::use_iterator U=fn->use_begin(),
				UE=fn->use_end(); U!=UE; ++U)
		{
			assert (isa<Instruction>(*U));
			Instruction* useI = cast<Instruction>(*U);
			assert (useI->getParent() && useI->getParent()->getParent());
			bool success = addValueInfo(useI, uniform, consecutive, aligned);
			if (!success) return false;
		}

		return true;
	}

	// Functions count as constants and they are allowed to be VARYING,
	// so we must not check before we are sure the value is no function.
	if (isa<Constant>(value) && !uniform) {
		errs() << "ERROR: constant can not be supplied as VARYING: " << *value << "\n";
		return false;
	}

	// insert into global value info map
	ValueInfo* info = new ValueInfo(value, uniform, consecutive, aligned);
	mGlobalValueInfoMap.insert(std::make_pair(value, info));
	return true;
}

void
WholeFunctionVectorizer::print(raw_ostream& o, const Module *M=NULL) const
{
	o << "Functions successfully packetized: " << PacketizedFunctionsCounter << "\n";
}

void
WholeFunctionVectorizer::getAnalysisUsage(AnalysisUsage& AU) const
{
	// TODO: add all passes here and then use getAnalysis<FunctionPacketizer>() ?
}

bool WholeFunctionVectorizer::runOnModule(Module& M)
{
	assert (&M == mInfo->mModule);
	PacketizedFunctionsCounter = 0;
	return packetizeAllFunctions();
}

inline bool
WholeFunctionVectorizer::packetizeAllFunctions()
{
	DEBUG_PKT( outs() << "\n-----------------------------------------------------\n"; );
	DEBUG_PKT( outs() << "Running whole-function vectorization...\n"; );

	DEBUG_PKT(
		outs() << "globalValueInfoMap:\n";
		for (ValueInfoMapType::const_iterator it=mGlobalValueInfoMap.begin(),
				E=mGlobalValueInfoMap.end(); it!=E; ++it)
		{
			outs() << "  " << *it->first << " ["
					<< (it->second->mUniform ? "UNIFORM, " : "VARYING, ")
					<< (it->second->mConsecutive ? "CONSECUTIVE, " :
						(it->second->mUniform ? "SAME, " : "RANDOM, "))
					<< (it->second->mAligned ? "ALIGNED, " : "UNALIGNED, ")
					<< "]\n";
		}
	);

	DEBUG_PKT_VISIBLE( llvm::TimerGroup tg("Whole-Function Vectorization"); )
	DEBUG_PKT_VISIBLE( llvm::Timer t("Whole-Function Vectorization", tg); )

	DEBUG_PKT( t.startTimer(); );

	// Iterate over all added functions, check them and packetize them
	for (std::map<const std::string, const std::string>::iterator
			it=mFunctionNames.begin(), E=mFunctionNames.end(); it!=E; ++it)
	{
		const std::string& scalarName = it->first;
		const std::string& targetName = it->second;

		DEBUG_PKT( outs() << "\n#####################################################\n"; );
		DEBUG_PKT( outs() << "#####################################################\n"; );
		DEBUG_PKT( outs() << "#####################################################\n"; );
		DEBUG_PKT( outs() << "### Packetizing function: " << scalarName << "...\n"; );
		DEBUG_PKT( outs() << "#####################################################\n"; );
		DEBUG_PKT( outs() << "#####################################################\n"; );
		DEBUG_PKT( outs() << "#####################################################\n"; );

#ifndef PACKETIZER_SILENT_MODE
		outs() << "\n### Packetizing function: " << scalarName << "...\n";
#endif

		bool success = false;
		try {
			success = packetizeFunction(scalarName, targetName);
		}
		catch (std::logic_error& error) {
			errs() << "\nException occurred during vectorization of function '"
					<< scalarName << "':\n";
			errs() << error.what() << "\n";
		}
		catch (...) {
			errs() << "\nINTERNAL ERROR: Unexpected exception occurred during "
					<< "vectorization of function '" << scalarName << "'!\n";
		}

#ifndef PACKETIZER_SILENT_MODE
		outs() << "### Packetization of function '" << scalarName
			<< (success ? "' successful!\n" : "' failed!\n");
#endif

		cleanup();

		if (!success) {
			Function* f_SIMD = mInfo->mModule->getFunction(targetName);
			if (f_SIMD && !f_SIMD->empty()) {
				f_SIMD->deleteBody();
				assert (f_SIMD->empty());
			}
			continue;
		}

		// count number of successful vectorizations
		++PacketizedFunctionsCounter;
	}

	DEBUG_PKT( t.stopTimer(); );
#ifndef PACKETIZER_SILENT_MODE
	outs() << "\nWhole-function vectorization finished ("
			<< PacketizedFunctionsCounter << " successful, "
			<< mFunctionNames.size()-PacketizedFunctionsCounter << " failed)!\n";
#endif
	DEBUG_PKT( tg.print(outs()); );
	DEBUG_PKT( outs() << "-----------------------------------------------------\n\n"; );

	return mFunctionNames.size()-PacketizedFunctionsCounter > 0;;
}

bool
WholeFunctionVectorizer::packetizeFunction(const std::string& scalarName,
                                           const std::string& targetName)
{
	Function* f = mInfo->mModule->getFunction(scalarName);

	if (!f) {
		errs() << "ERROR while packetizing function in module '"
				<< mInfo->mModule->getModuleIdentifier() << "': function '"
				<< scalarName << "' not found!\n";
		return false;
	}

	if (f->isVarArg()) {
		errs() << "ERROR while packetizing function in module '"
				<< mInfo->mModule->getModuleIdentifier() << "': function '"
				<< scalarName << "' has a variable argument list (not supported)!\n";
		return false;
	}

	//check if newName is function declared as 'external'
	Function* extF = mInfo->mModule->getFunction(targetName);

	if (!extF) {
		errs() << "ERROR while packetizing function in module '"
				<< mInfo->mModule->getModuleIdentifier() << "': extern target function '"
				<< targetName << "' not declared!\n";
		return false;
	}

	if (!extF->isDeclaration()) {
		assert (!extF->getBasicBlockList().empty() &&
				"Function is no declaration but does not have basic blocks?!");
		errs() << "ERROR while packetizing function in module '"
				<< mInfo->mModule->getModuleIdentifier() << "': extern target function '"
				<< targetName << "' must not have a body!\n";
		return false;
	}

	DEBUG_PKT( f->print(outs()); );
	DEBUG_PKT( Packetizer::writeFunctionToFile(f, scalarName+".ll"); );
	DEBUG_PKT( Packetizer::writeModuleToFile(mInfo->mModule, scalarName+".mod.ll"); );
	DEBUG_PKT_NO_VERBOSE( verifyFunction(*f); );

	// Don't touch original function... clone it.
	// however, don't use CloneFunction, because the function
	// has no parent then. Thus, create new temporary function
	// and clone into it.

	// create temp function (target for cloning)
	Function* tempF = Function::Create(f->getFunctionType(), GlobalValue::ExternalLinkage, scalarName+".tmp", mInfo->mModule);
	tempF->setCallingConv(CallingConv::C);
	tempF->setAttributes(f->getAttributes());
	tempF->setAlignment(f->getAlignment());
	tempF->setLinkage(f->getLinkage());

	// clone function
	ValueToValueMapTy valueMap;
	Function::arg_iterator destI = tempF->arg_begin();
	for (Function::const_arg_iterator I = f->arg_begin(), E = f->arg_end(); I != E; ++I) {
		if (valueMap.count(I) == 0) {     // Is this argument preserved?
			destI->setName(I->getName()); // Copy the name over...
			valueMap[I] = destI++;        // Add mapping to ValueMap
		}
	}
	SmallVector<ReturnInst*, 10> returns;
	ClonedCodeInfo clonedFInfo;
	const char* nameSuffix = ".";
	CloneFunctionInto(tempF, f, valueMap, false, returns, nameSuffix, &clonedFInfo);

	// Map all user-defined uniform/consecutive/aligned values that "belong"
	// to the current function.
	for (ValueInfoMapType::const_iterator it=mGlobalValueInfoMap.begin(),
			E=mGlobalValueInfoMap.end(); it!=E; ++it)
	{
		const ValueInfo* origInfo = it->second;
		Value* origValue = origInfo->mValue;

		// ignore all instructions and arguments that do not belong to
		// the current function.
		if (isa<Instruction>(origValue) &&
				cast<Instruction>(origValue)->getParent()->getParent() != f) continue;
		if (isa<Argument>(origValue) &&
				cast<Argument>(origValue)->getParent() != f) continue;

		if (!isa<Instruction>(origValue) && !isa<Argument>(origValue)) {
			errs() << "WARNING: value found in global value info map that"
					<< " is neither instruction nor argument: "
					<< *origValue << " - ignored!\n";
			continue;
		}

		DEBUG_PKT( outs() << "mapped user-defined uniform/consecutive/aligned"
				<< " value to cloned function: " << *origValue << "\n"; );

		Value* clonedValue = valueMap[origValue];
		assert (clonedValue);

		ValueInfo* info = new ValueInfo(clonedValue,
										origInfo->mUniform,
										origInfo->mConsecutive,
										origInfo->mAligned);

		mValueInfoMap.insert(std::make_pair(clonedValue, info));
	}

	DEBUG_PKT(
		outs() << "valueInfoMap:\n";
		for (ValueInfoMapType::const_iterator it=mValueInfoMap.begin(), E=mValueInfoMap.end(); it!=E; ++it) {
			outs() << "  " << *it->first << " ["
				<< (it->second->mUniform ? "UNIFORM, " : "VARYING, ")
				<< (it->second->mConsecutive ? "CONSECUTIVE, " :
						(it->second->mUniform ? "SAME, " : "RANDOM, "))
				<< (it->second->mAligned ? "ALIGNED, " : "UNALIGNED, ")
				<< "]\n";
		}
	);

	// TODO: get rid of this, it should be done by host application!
	Packetizer::optimizeFunction(tempF);

	// Replace calls to fmodf by simple REM
	// TODO: Shouldn't this be part of "native functions" or so?
	if (Function* fmodf = mInfo->mModule->getFunction("fmodf")) transformCalls(fmodf);

	DEBUG_PKT( tempF->print(outs()); );
	DEBUG_PKT( extF->print(outs()); );

	if (!verifyFunctionSignaturesMatch(tempF, extF)) {
		errs() << "ERROR: Function signatures do not match!\n";
		errs() << "       scalar    : " << *tempF->getType() << "\n";
		errs() << "       packetized: " << *extF->getType() << "\n";
		return false;
	}

	// TODO: Move this to after vectorization analysis and exit with verified
	//       decision instead of giving some possibly meaningless warning.
	if (!isPacketizable(tempF)) {
		errs() << "WARNING: might not be able to packetize function '" << scalarName << "'!\n";
	}

	DEBUG_PKT_NO_VERBOSE( verifyFunction(*tempF); );

	/////////////////////////////
	//run packetization passes //
	/////////////////////////////

	{
		FunctionPassManager funPassManager(mInfo->mModule);

		// run preparatory transformation phases (LLVM passes)
		funPassManager.add(createLowerSwitchPass());
		funPassManager.add(createBreakCriticalEdgesPass());
		funPassManager.add(createLoopSimplifyPass());
		// run custom preparation phases
		funPassManager.add(createReturnUnifierPass(mVerbose));
		//funPassManager.add(createUnifyFunctionExitNodesPass()); // TODO: Use this instead of returnUnifier
		funPassManager.add(createPhiCanonicalizationPass(mVerbose));
		funPassManager.add(createLoopBranchCanonicalizationPass(mVerbose));
		funPassManager.add(createMemAccessCanonicalizationPass(mVerbose));
		DEBUG_PKT_NO_VERBOSE( funPassManager.add(createVerifierPass()); );

		funPassManager.doInitialization();
		funPassManager.run(*tempF);
		funPassManager.doFinalization();
	}

	// TODO: is there a better way to cast to a const value?
	const NativeFunctionMapType* constNativeFunctionMap =
	const_cast<const NativeFunctionMapType*>(&mNativeFunctionMap);
	const ValueInfoMapType* constValueInfoMap =
	const_cast<const ValueInfoMapType*>(&mValueInfoMap);

	bool failed = false;
	{
		FunctionPassManager PM(mInfo->mModule);
		PM.add(createBranchInfoAnalysisPass(&failed, mVerbose));
		PM.add(createLoopLiveValueAnalysisPass(&failed, mVerbose));
		PM.add(createVectorizationAnalysisPass(mInfo, tempF, extF, *constNativeFunctionMap, *constValueInfoMap, &failed, mVerbose));
		PM.add(createMaskGeneratorPass(&failed, mVerbose));
		PM.add(createSelectGeneratorPass(&failed, mVerbose));
		PM.add(createCFGLinearizerNaivePass(&failed, mVerbose));
#ifndef PACKETIZER_DO_NOT_USE_COHERENT_MASK_BRANCHING
		PM.add(createCoherentMaskBranchGeneratorPass(&failed, mVerbose));
#endif
		PM.add(createFunctionPacketizerPass(mInfo, tempF, extF, &failed, mVerbose));
		DEBUG_PKT_NO_VERBOSE( PM.add(createVerifierPass()); );

		PM.doInitialization();
		PM.run(*tempF);
		PM.doFinalization();
	}

	if (failed) return false;

	// NOTE: Loop live value information is invalidated after select generation!
	// NOTE: loopInfo, PDT, and DT do not match tempF anymore after cfgLinearization!

	// TODO: after each phase:
	//tempF->print(outs(), new WholeFunctionVectorizationAAW(vectorizationAnalysis));
	//assert (maskGraph.verify());
	//assert (vectorizationAnalysis.verify());
	//verifyFunction(*tempF);

	// NOTE: Passes must not be deleted manually, this is done by PM (including calls to releaseMemory()).

	// clean up temporary function
	tempF->eraseFromParent();

	return true;
}

void
WholeFunctionVectorizer::cleanup()
{
	// TODO: erase tempF from parent (requires tempF to be visible here...)

	// clean valueInfoMap of current function
	for (ValueInfoMapType::const_iterator it=mValueInfoMap.begin(),
			E=mValueInfoMap.end(); it!=E; ++it)
	{
		delete it->second;
	}
	mValueInfoMap.clear();
}

bool
WholeFunctionVectorizer::verifyFunctionSignaturesMatch(const Function* f,
                                                       const Function* f_SIMD)
{
	bool verified = true;

	if (f->arg_size() != f_SIMD->arg_size()) {
		errs() << "ERROR: number of function arguments does not match!\n";
		verified = false;
	}

	// check argument and return types
	Type* scalarReturnType = f->getReturnType();
	Type* foundPacketReturnType = f_SIMD->getReturnType();
    //Type* expectedPacketReturnType =
        //scalarReturnType->isVoidTy() ? scalarReturnType :
            //packetizeType(packetize4xType(scalarReturnType));

	if (!verifyPacketizedType(scalarReturnType, foundPacketReturnType)) {
		errs() << "ERROR: return type does not match!\n";
		errs() << "       scalar      : " << *scalarReturnType << "\n";
		//errs() << "       vec expected: " << *expectedPacketReturnType << "\n";
		errs() << "       vec found   : " << *foundPacketReturnType << "\n";
		verified = false;
	}

	for (Function::const_arg_iterator A=f->arg_begin(), extA=f_SIMD->arg_begin();
			A!=f->arg_end() && extA!=f_SIMD->arg_end(); ++A, ++extA) {
		Type* scalarType = A->getType();
		assert (!scalarType->isVoidTy() && "if this ever fires, modify creation of foundPacketType to handle void ;)");
		Type* foundPacketType = extA->getType();
		//Type* expectedPacketType = packetizeType(packetize4xType(A->getType()));
		if (!verifyPacketizedType(scalarType, foundPacketType)) {
			errs() << "ERROR: argument type does not match: " << *A << "\n";
			errs() << "       scalar      : " << *scalarType << "\n";
			//errs() << "       vec expected: " << *expectedPacketType << "\n";
			errs() << "       vec found   : " << *foundPacketType << "\n";
			verified = false;
		}
	}

	return verified;
}

bool
WholeFunctionVectorizer::verifyPacketizedType(Type* scalarType, Type* vecType)
{
	// Check for uniform equivalence.
	if (scalarType == vecType) return true;
    if (Packetizer::typesMatch(scalarType, vecType, *mInfo)) return true;

    // Check for varying equivalence.
    Type* vectorizedType =
        Packetizer::packetizeSIMDWrapperType(Packetizer::packetizeSIMDType(scalarType, *mInfo), *mInfo);
    if (Packetizer::typesMatch(vecType, vectorizedType, *mInfo)) return true;

    return false;
}

// TODO: this function should ignore uniform paths!
// Until this is implemented, this function only produces warnings instead of exiting.
// The return value thus only gives a hint if the function *might* not be packetizable.
bool
WholeFunctionVectorizer::isPacketizable(const Function* f) const
{
	assert (f);
	bool packetizable = true;

	for (Function::const_arg_iterator A=f->arg_begin(); A!=f->arg_end(); ++A) {
		if (!isPacketizableType(A->getType())) {
			errs() << "WARNING: Function '" << f->getName() << "' contains unsupported argument-type: \n  " << *A << " - will not be able to packetize unless argument is only used in uniform operations!\n";
			packetizable = false;
		}
	}

	for (Function::const_iterator BB=f->begin(), BBE=f->end(); BB!=BBE; ++BB) {
		for (BasicBlock::const_iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {

			// we do not allow instructions without uses except for these:
			if (I->use_empty() &&
					!isa<StoreInst>(I) &&
					!isa<ReturnInst>(I) &&
					!isa<SwitchInst>(I) &&
					!isa<BranchInst>(I) &&
					!isa<CallInst>(I))
			{
				errs() << "ERROR: Function '" << f->getName() << "' contains instruction without use: " << *I << "(run DCE before WFV!)\n";
				throw std::logic_error("ERROR: Function contains instruction without use! Run DCE before WFV!");
			}

			switch (I->getOpcode()) {
				case Instruction::FPExt:
				{
					errs() << "WARNING: Function '" << f->getName() << "' contains FPExt instruction - will not be able to packetize unless on uniform path!\n";
					packetizable = false;
					break;
				}
				case Instruction::Trunc:
				{
					errs() << "WARNING: Function '" << f->getName() << "' contains TruncInst instruction - will not be able to packetize unless on uniform path!\n";
					packetizable = false;
					break;
				}
				case Instruction::ZExt:
				{
					if (I->getType()->isIntegerTy(32)) continue; //allow ZExt to i32
					errs() << "WARNING: Function '" << f->getName() << "' contains ZExtInst instruction - will not be able to packetize unless on uniform path!\n";
					packetizable = false;
					break;
				}
				case Instruction::SExt:
				{
					errs() << "WARNING: Function '" << f->getName() << "' contains SExtInst instruction - will not be able to packetize unless on uniform path!\n";
					packetizable = false;
					break;
				}
				case Instruction::Switch:
				{
					DEBUG_PKT_NO_VERBOSE( errs() << "WARNING: Function '" << f->getName() << "' contains Switch instruction: lowering to branches!\n"; );
					break;
				}
				default:
				{
					//HACK: this requires special automatic treatment of i8* instructions!
					//      -> to be replaced by some generic handling of uniform/varying values
					//						const bool isVoidPointer = I->getType()->getTypeID() == Type::PointerTyID &&
					//								cast<PointerType>(I->getType())->getElementType() == Type::getInt8Ty(info.context);
					//						if (!isPacketizableType(I->getType()) && !isVoidPointer)
					if (!isPacketizableType(I->getType())) {
						errs() << "WARNING: Function '" << f->getName() << "' contains non-packetizable type - will not be able to packetize unless on uniform path!\n";
						errs() << "       Instruction: " << *I << "\n";
						packetizable = false;
					}
					break;
				}
			} //switch

		} //I
	} //BB
	return packetizable;
}

bool
WholeFunctionVectorizer::isPacketizableType(Type* type) const
{
	//first check most common types
	if (type->isFloatTy() ||
			type->isVoidTy() ||
			(type->isIntegerTy() && type->getPrimitiveSizeInBits() <= 32U)) return true;

	Type::TypeID typeID = type->getTypeID();
	//if none of these, test for primitive types
	if (typeID < Type::FirstDerivedTyID) return false;
	//if also none of these, test derived types
	switch (typeID) {
		case Type::VectorTyID: return false;
		case Type::PointerTyID:
		{
			const bool multipleIndirection = type->getContainedType(0)->isPointerTy();
			if (multipleIndirection) errs() << "ERROR: packetization can not handle multiple indirection!\n";
			return !multipleIndirection && isPacketizableType(type->getContainedType(0));
		}
		case Type::ArrayTyID: return isPacketizableType(cast<ArrayType>(type)->getElementType());
		case Type::StructTyID:
		{
			StructType* sType = cast<StructType>(type);
			for (unsigned i=0; i<sType->getNumContainedTypes(); ++i) {
				if (!isPacketizableType(sType->getElementType(i))) return false;
			}
			return true;
		}

		default: return false;
	}
}

//
// TODO: these functions should only transform paths that will be packetized!!
//
// NOTE: These functions are different kinds of hacks to get around specific
//       patterns in LLVM IR, e.g. due to front-end issues.
//       They should all be replaced by error messages + exceptions:
//       The host application that uses WFV should be responsible for providing
//       the right input. For this stuff below, we can not really guarantee
//       anything.
//

bool
WholeFunctionVectorizer::transformFunction(Function* f)
{
	DEBUG_PKT( outs() << "transforming function '" << f->getName() << "'... \n"; );

	DEBUG_PKT( outs() << "  replacing constants of types of precision > 32bit if possible... "; );
	for (Function::iterator BB=f->begin(), BE=f->end(); BB!=BE; ++BB) {
		for (BasicBlock::iterator I=BB->begin(), IE=BB->end(); I!=IE; ) {
			//DEBUG_PKT( outs() << "transforming instruction: " << *I << "\n"; );
			Instruction* inst = I++;

			if (isa<CallInst>(inst)) continue;

			// replace constants of type double by float and i64 by i32
			transformConstants(inst);
		}
	}
	DEBUG_PKT( outs() << "done.\n"; );
	return true;
}

//replaces constant operands of I by 'packetizable' type
// TODO: currently only handles i64 and double, nothing else implemented
bool
WholeFunctionVectorizer::transformConstants(Instruction* I)
{
	if (I->isShift()) return false;

	bool changed = false;
	for (unsigned i=0; i<I->getNumOperands(); ++i) {
		Value* OP = I->getOperand(i);
		if (!isa<Constant>(OP)) continue;

		Type* opType = OP->getType();

		Constant* newC = NULL;

		if (opType == Type::getInt64Ty(*mInfo->mContext)) {
			if (isa<UndefValue>(OP)) {
				newC = UndefValue::get(Type::getInt32Ty(*mInfo->mContext));
			} else {
				ConstantInt* opC = cast<ConstantInt>(OP);
				const uint64_t intValue = *opC->getValue().getRawData();
				if (!ConstantInt::isValueValidForType(Type::getInt32Ty(*mInfo->mContext), intValue)) {
					errs() << "WARNING: Integer constant is too large to fit into 32bit "
						<< "- cannot vectorize: " << intValue << "\n";
					break;
				}
				newC = ConstantInt::get(*mInfo->mContext, APInt(32, intValue));
			}
		} else if (opType == Type::getDoubleTy(*mInfo->mContext)) {
			if (isa<UndefValue>(OP)) {
				newC = UndefValue::get(Type::getFloatTy(*mInfo->mContext));
			} else {
				ConstantFP* opC = cast<ConstantFP>(OP);
				if (!ConstantFP::isValueValidForType(Type::getFloatTy(*mInfo->mContext), opC->getValueAPF())) {
					errs() << "WARNING: Floating point constant is too large to fit into 32bit "
						<< "- cannot vectorize: " << opC->getValueAPF().convertToDouble() << "\n";
					break;
				}
				newC = ConstantFP::get(Type::getFloatTy(*mInfo->mContext), opC->getValueAPF().convertToDouble());
			}
		}

		if (!newC) continue;
		I->setOperand(i, newC);
		DEBUG_PKT( outs() << "    replaced constant in instruction:" << *I << "\n"; );
		changed = true;
	}
	return changed;
}

void
WholeFunctionVectorizer::transformCalls(Function* f)
{
	assert (f);

	if (!f->getName().equals("fmodf")) return;

	for (Function::use_iterator U=f->use_begin(), UE=f->use_end(); U!=UE; ) {
		assert (isa<CallInst>(*U));
		CallInst* call = cast<CallInst>(*U++);
		Value* val0 = call->getArgOperand(0);
		Value* val1 = call->getArgOperand(1);
		BinaryOperator* subInst = BinaryOperator::Create(Instruction::FRem, val0, val1, "", call);
		call->replaceAllUsesWith(subInst);
		call->eraseFromParent();
	}
}


// only run analyses without modifying the code
// TODO: This is not exactly equivalent to what happens if packetization is performed:
//       There are preparatory transformations (returnUnification etc.) that are not performed by this function.
bool
WholeFunctionVectorizer::analyzeFunction(const std::string& scalarName,
                                         const std::string& targetName)
{
	Function* f = mInfo->mModule->getFunction(scalarName);

	if (!f) {
		errs() << "ERROR while analyzing function in module '"
				<< mInfo->mModule->getModuleIdentifier() << "': function '"
				<< scalarName << "' not found!\n";
		return false;
	}

	if (f->isVarArg()) {
		errs() << "ERROR while analyzing function in module '"
				<< mInfo->mModule->getModuleIdentifier() << "': function '"
				<< scalarName << "' has a variable argument list (not supported)!\n";
		return false;
	}

	//check if newName is function declared as 'external'
	Function* extF = mInfo->mModule->getFunction(targetName);

	if (!extF) {
		errs() << "ERROR while analyzing function in module '"
				<< mInfo->mModule->getModuleIdentifier() << "': extern target function '"
				<< targetName << "' not declared!\n";
		return false;
	}

	if (!extF->isDeclaration()) {
		assert (!extF->getBasicBlockList().empty() &&
				"Function is no declaration but does not have basic blocks?!");
		errs() << "ERROR while analyzing function in module '"
				<< mInfo->mModule->getModuleIdentifier() << "': extern target function '"
				<< targetName << "' must not have a body!\n";
		return false;
	}

	DEBUG_PKT_NO_VERBOSE( verifyFunction(*f); );

	// Map all user-defined uniform/consecutive/aligned values that "belong"
	// to the current function.
	for (ValueInfoMapType::const_iterator it=mGlobalValueInfoMap.begin(),
			E=mGlobalValueInfoMap.end(); it!=E; ++it)
	{
		const ValueInfo* origInfo = it->second;
		Value* origValue = origInfo->mValue;

		// ignore all instructions and arguments that do not belong to
		// the current function.
		if (isa<Instruction>(origValue) &&
				cast<Instruction>(origValue)->getParent()->getParent() != f) continue;
		if (isa<Argument>(origValue) &&
				cast<Argument>(origValue)->getParent() != f) continue;

		if (!isa<Instruction>(origValue) && !isa<Argument>(origValue)) {
			errs() << "WARNING: value found in global value info map that"
					<< " is neither instruction nor argument: "
					<< *origValue << " - ignored!\n";
			continue;
		}

		mValueInfoMap.insert(std::make_pair(origValue, origInfo));
	}

	if (!verifyFunctionSignaturesMatch(f, extF)) {
		errs() << "ERROR: Function signatures do not match!\n";
		errs() << "       scalar    : " << *f->getType() << "\n";
		errs() << "       packetized: " << *extF->getType() << "\n";
		return false;
	}

	// TODO: Move this to after vectorization analysis and exit with verified
	//       decision instead of giving some possibly meaningless warning.
	if (!isPacketizable(f)) {
		errs() << "WARNING: might not be able to packetize function '" << scalarName << "'!\n";
	}

	// TODO: is there a better way to cast to a const value?
	const NativeFunctionMapType* constNativeFunctionMap =
	const_cast<const NativeFunctionMapType*>(&mNativeFunctionMap);
	const ValueInfoMapType* constValueInfoMap =
	const_cast<const ValueInfoMapType*>(&mValueInfoMap);

	if (mAnalysisResults) delete mAnalysisResults;
	mAnalysisResults = new AnalysisResults(mVerbose);

	bool failed = false;
	FunctionPassManager PM(mInfo->mModule);
	PM.add(createBranchInfoAnalysisPass(&failed, mVerbose));
	PM.add(createLoopLiveValueAnalysisPass(&failed, mVerbose));
	PM.add(createVectorizationAnalysisPass(mInfo, f, extF, *constNativeFunctionMap, *constValueInfoMap, &failed, mVerbose, mAnalysisResults));
	DEBUG_PKT_NO_VERBOSE( PM.add(createVerifierPass()); );

	PM.run(*f);

	if (failed) {
		errs() << "ERROR: analyses failed!\n";
	}

	// clean valueInfoMap of current function
	for (ValueInfoMapType::const_iterator it=mValueInfoMap.begin(),
			E=mValueInfoMap.end(); it!=E; ++it)
	{
		delete it->second;
	}
	mValueInfoMap.clear();

	return mAnalysisResults->verify(*f);
}

bool
WholeFunctionVectorizer::isUniform(const Value* value) const
{
	return mAnalysisResults->isUniform(value);
}
bool
WholeFunctionVectorizer::isSame(const Value* value) const
{
	return mAnalysisResults->isSame(value);
}
bool
WholeFunctionVectorizer::isConsecutive(const Value* value) const
{
	return mAnalysisResults->isConsecutive(value);
}
bool
WholeFunctionVectorizer::isRandom(const Value* value) const
{
	return mAnalysisResults->isRandom(value);
}
bool
WholeFunctionVectorizer::isAligned(const Value* value) const
{
	return mAnalysisResults->isAligned(value);
}
bool
WholeFunctionVectorizer::isMask(const Value* value) const
{
	return mAnalysisResults->isMask(value);
}

bool
WholeFunctionVectorizer::requiresReplication(const Value* value) const
{
	return mAnalysisResults->requiresReplication(value);
}
bool
WholeFunctionVectorizer::requiresSplitResult(const Value* value) const
{
	return mAnalysisResults->requiresSplitResult(value);
}
bool
WholeFunctionVectorizer::requiresSplitFull(const Value* value) const
{
	return mAnalysisResults->requiresSplitFull(value);
}
bool
WholeFunctionVectorizer::requiresSplitFullGuarded(const Value* value) const
{
	return mAnalysisResults->requiresSplitFullGuarded(value);
}

bool
WholeFunctionVectorizer::isNonDivergent(const BasicBlock* block) const
{
	return mAnalysisResults->hasUniformEntry(block);
}
bool
WholeFunctionVectorizer::isFullyNonDivergent(const BasicBlock* block) const
{
	return mAnalysisResults->hasFullyUniformEntry(block);
}
bool
WholeFunctionVectorizer::hasUniformExit(const BasicBlock* block) const
{
	return mAnalysisResults->hasUniformExit(block);
}
bool
WholeFunctionVectorizer::hasFullyUniformExit(const BasicBlock* block) const
{
	return mAnalysisResults->hasFullyUniformExit(block);
}

bool
WholeFunctionVectorizer::isInputIndependent(const Instruction* value) const
{
	return mAnalysisResults->isInputIndependent(value);
}


namespace Packetizer {

PACKETIZER_API bool
Packetizer::analyzeFunction(const std::string& scalarName, const std::string& targetName)
{
	return wfv->analyzeFunction(scalarName, targetName);
}

PACKETIZER_API bool
Packetizer::isUniform(const Value* value) const
{
	return wfv->isUniform(value);
}
PACKETIZER_API bool
Packetizer::isSame(const Value* value) const
{
	return wfv->isSame(value);
}
PACKETIZER_API bool
Packetizer::isConsecutive(const Value* value) const
{
	return wfv->isConsecutive(value);
}
PACKETIZER_API bool
Packetizer::isRandom(const Value* value) const
{
	return wfv->isRandom(value);
}
PACKETIZER_API bool
Packetizer::isAligned(const Value* value) const
{
	return wfv->isAligned(value);
}
PACKETIZER_API bool
Packetizer::isMask(const Value* value) const
{
	return wfv->isMask(value);
}

PACKETIZER_API bool
Packetizer::requiresReplication(const Value* value) const
{
	return wfv->requiresReplication(value);
}
PACKETIZER_API bool
Packetizer::requiresSplitResult(const Value* value) const
{
	return wfv->requiresSplitResult(value);
}
PACKETIZER_API bool
Packetizer::requiresSplitFull(const Value* value) const
{
	return wfv->requiresSplitFull(value);
}
PACKETIZER_API bool
Packetizer::requiresSplitFullGuarded(const Value* value) const
{
	return wfv->requiresSplitFullGuarded(value);
}

PACKETIZER_API bool
Packetizer::isNonDivergent(const BasicBlock* block) const
{
	return wfv->isNonDivergent(block);
}
PACKETIZER_API bool
Packetizer::isFullyNonDivergent(const BasicBlock* block) const
{
	return wfv->isFullyNonDivergent(block);
}
PACKETIZER_API bool
Packetizer::hasUniformExit(const BasicBlock* block) const
{
	return wfv->hasUniformExit(block);
}
PACKETIZER_API bool
Packetizer::hasFullyUniformExit(const BasicBlock* block) const
{
	return wfv->hasFullyUniformExit(block);
}

PACKETIZER_API bool
Packetizer::isInputIndependent(const Instruction* value) const
{
	return wfv->isInputIndependent(value);
}

} // namespace Packetizer

