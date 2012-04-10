/**
 * @file   llvmTools.hpp
 * @date   10.12.2008
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2008, 2009, 2010, 2011 Saarland University
 *
 */
#ifndef _LLVMTOOLS_HPP
#define _LLVMTOOLS_HPP

#ifdef __INTEL_COMPILER
    #pragma warning( disable : 279 )
#endif

//#define DEBUG_LLVM_PASSES
#ifdef DEBUG_LLVM_PASSES
#include "llvm/Support/Debug.h"
#endif

#include "llvm/Support/raw_ostream.h"
#include "llvm/Module.h"

#include <sstream> // stringstream
#include <stdexcept> // logic_error

// optimization
#include "llvm/Instructions.h"
#include "llvm/LLVMContext.h"
#include "llvm/PassManager.h"
#include "llvm/Support/StandardPasses.h"
#include "llvm/Transforms/Utils/Cloning.h" //InlineFunction
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetLibraryInfo.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/Timer.h"

using namespace llvm;

namespace Packetizer {

/**
 * method for SIMD width packetization
 * -> only 32bit-float, integers <= 32bit, pointers, arrays and structs allowed
 * -> no scalar datatypes allowed
 * -> no pointers to pointers allowed
 **/
const Type* packetizeSIMDType(const Type* oldType, const PacketizerInfo& info) {
	Type::TypeID oldTypeID = oldType->getTypeID();
	switch (oldTypeID) {
		//case Type::getVoidTy(getGlobalContext())ID : return Type::getVoidTy(getGlobalContext()); //not allowed
		case Type::FloatTyID:
		case Type::DoubleTyID:
		{
			return info.vectorTy_floatSIMD;
		}
		case Type::IntegerTyID:
		{
			// TODO: Support arbitrary types < 32bit. #11
#if 1
			return info.vectorTy_intSIMD;
#else
			if (oldType->getPrimitiveSizeInBits() >= 32U) {
				return info.vectorTy_intSIMD;
			}
			return VectorType::get(oldType, info.simdWidth);
#endif
		}
		//case Type::VectorTyID: return NULL;  //not allowed!
		case Type::PointerTyID:
		{
			const PointerType* pType = cast<PointerType>(oldType);
			//const Type* elType = pType->getElementType();
			//if (elType->isPointerTy()) {
				//throw std::logic_error("INTERNAL ERROR: packetization can not handle multiple indirection!");
			//}
			return PointerType::get(packetizeSIMDType(pType->getElementType(), info), pType->getAddressSpace());
		}
		case Type::ArrayTyID:
		{
			const ArrayType* aType = cast<ArrayType>(oldType);
			return ArrayType::get(packetizeSIMDType(aType->getElementType(), info), aType->getNumElements());
		}
		case Type::StructTyID:
		{
			const StructType* sType = cast<StructType>(oldType);
			std::vector<const Type*> newParams;
			for (unsigned i=0; i<sType->getNumContainedTypes(); ++i) {
				newParams.push_back(packetizeSIMDType(sType->getElementType(i), info));
			}
			return StructType::get(getGlobalContext(), newParams, sType->isPacked());
		}

		default:
		{
			errs() << "\nERROR: only arguments of type float, int, pointer, array or struct can be packetized, not '" << *oldType << "'!\n";
			throw std::logic_error("INTERNAL ERROR: 'first' packetization (scalar -> internal simd) can only handle float, int, pointers, arrays and structs!");
		}
	}
}

/**
 * method for arbitrary packetization size
 * packetize types from simd-format to arbitrary size
 * -> only pointers, packets, arrays and structs allowed
 * -> no scalar datatypes allowed
 * -> no pointers to pointers allowed
 **/
const Type* packetizeSIMDWrapperType(const Type* oldType, const PacketizerInfo& info) {
	if (info.totalSIMDIterations == 1) return oldType;
	Type::TypeID oldTypeID = oldType->getTypeID();
	switch (oldTypeID) {
		//case Type::VoidTyID: return Type::getVoidTy(getGlobalContext());     //not allowed
		//case Type::FloatTyID: return info.vectorTy_float4; //not allowed
		//case Type::IntegerTyID: return info.vectorTy_int4; //not allowed
		case Type::VectorTyID:
		{
			//only put into array if 'inside' pointer
			return PointerType::getUnqual(oldType);
		}
		case Type::PointerTyID:
		{
			const PointerType* pType = cast<PointerType>(oldType);
			const Type* elType = pType->getElementType();
			//if (elType->isPointerTy()) {
				//throw std::logic_error("INTERNAL ERROR: packetization can not handle multiple indirection!");
			//}
			if (elType->isVectorTy()) {
				const Type* newElType = elType == info.vectorTy_floatSIMD ? ArrayType::get(info.vectorTy_floatSIMD, info.totalSIMDIterations)
					: elType == info.vectorTy_intSIMD ? ArrayType::get(info.vectorTy_intSIMD, info.totalSIMDIterations) : NULL;
				assert (newElType && "bad vector type found (should never fire)!");
				return PointerType::get(packetizeSIMDWrapperType(newElType, info), pType->getAddressSpace());
			} else {
				return PointerType::get(packetizeSIMDWrapperType(pType->getElementType(), info), pType->getAddressSpace());
			}
		}
		case Type::ArrayTyID:
		{
			return ArrayType::get(oldType, info.totalSIMDIterations);
		}
		case Type::StructTyID:
		{
			const StructType* sType = cast<StructType>(oldType);
			std::vector<const Type*> newParams;
			for (unsigned i=0; i<sType->getNumContainedTypes(); ++i) {
				newParams.push_back(packetizeSIMDWrapperType(sType->getElementType(i), info));
			}
			return StructType::get(getGlobalContext(), newParams, sType->isPacked());
		}

		default :
		{
			errs() << "\nERROR: only arguments of type pointer, packet, array or struct can be packetized, not '" << *oldType << "'!\n";
			throw std::logic_error("INTERNAL ERROR: 'second' packetization (internal simd -> wrapper) can only handle pointers, packets, arrays and structs!");
		}
	}
}

Constant* getMax32BitConstant(Constant* c) {
	assert (c);
	const Type* type = c->getType();

	if (type->getPrimitiveSizeInBits() <= 32U) {
		// TODO: Support arbitrary types < 32bit. #11
#if 1
		if (type->isFloatTy()) return c;
		if (type->isIntegerTy()) {
			ConstantInt* opC = cast<ConstantInt>(c);
			const uint64_t intValue = *opC->getValue().getRawData();
			assert (ConstantInt::isValueValidForType(Type::getInt32Ty(getGlobalContext()), intValue));
			return ConstantInt::get(getGlobalContext(), APInt(32, intValue));
		}
		assert (false && "NOT IMPLEMENTED");
#else
		return c;
#endif
	}

	Constant* newC = NULL;

	if (type == Type::getInt64Ty(getGlobalContext())) {
		if (isa<UndefValue>(c)) {
			newC = UndefValue::get(Type::getInt32Ty(getGlobalContext()));
		} else {
			ConstantInt* opC = cast<ConstantInt>(c);
			const uint64_t intValue = *opC->getValue().getRawData();
			if (!ConstantInt::isValueValidForType(Type::getInt32Ty(getGlobalContext()), intValue)) {
				errs() << "WARNING: Integer constant is too large to fit into 32bit "
					<< "- cannot vectorize: " << intValue << "\n";
				throw new std::logic_error("ERROR: Integer constant is too large to fit into 32bit!");
			}
			newC = ConstantInt::get(getGlobalContext(), APInt(32, intValue));
		}
	} else if (type == Type::getDoubleTy(getGlobalContext())) {
		if (isa<UndefValue>(c)) {
			newC = UndefValue::get(Type::getFloatTy(getGlobalContext()));
		} else {
			ConstantFP* opC = cast<ConstantFP>(c);
			if (!ConstantFP::isValueValidForType(Type::getFloatTy(getGlobalContext()), opC->getValueAPF())) {
				errs() << "WARNING: Floating point constant is too large to fit into 32bit "
					<< "- cannot vectorize: " << opC->getValueAPF().convertToDouble() << "\n";
				throw new std::logic_error("ERROR: Floating point constant is too large to fit into 32bit!");
			}
			newC = ConstantFP::get(Type::getFloatTy(getGlobalContext()), opC->getValueAPF().convertToDouble());
		}
	} else {
		errs() << "ERROR: bad type found for constant creation: " << *type << "\n";
		throw std::logic_error("INTERNAL ERROR: bad type found for constant creation!");
	}

	return newC;
}

Instruction* createDummy(const Type* type, Instruction* insertBefore) {
	Constant* c = Constant::getNullValue(type);
	if (type->isPointerTy()) {
		// we must not insert pointer selects ;)
		return new BitCastInst(c, type, "dummy", insertBefore);
	} else {
		return SelectInst::Create(Constant::getNullValue(Type::getInt1Ty(getGlobalContext())), c, c, "dummy", insertBefore);
	}
}



// This function only performs a check if the given type is a valid
// instruction return type for packetization.
// This includes void, arrays and structs but excludes pointers!
// TODO: Actually, arrays and structs are either also disallowed or
//       they need different handling because pointers inside might be okay.
bool isPacketizableInstructionType(const Type* type) {
	const Type::TypeID typeID = type->getTypeID();
	switch (typeID) {
		case Type::VoidTyID : return true;
		case Type::FloatTyID : return true;
		case Type::IntegerTyID : return type->getPrimitiveSizeInBits() <= 32U;
		case Type::VectorTyID : return false;
		case Type::PointerTyID : return false;
		case Type::ArrayTyID : return isPacketizableInstructionType(cast<ArrayType>(type)->getElementType());
		case Type::StructTyID :
		{
			const StructType* sType = cast<StructType>(type);
			for (unsigned i=0; i<sType->getNumContainedTypes(); ++i) {
				if (!isPacketizableInstructionType(sType->getElementType(i))) return false;
			}
			return true;
		}
		default : return false;
	}
}


//returns true if blockA 'follows' blockB or blockA == blockB
//REQUIRES LoopInfo
bool isDominatedBy(const BasicBlock* blockA, const BasicBlock* blockB, const LoopInfo& loopInfo) {
	if (blockA == blockB) return true;

	bool isDominated = false;
	for (succ_const_iterator it=succ_begin(blockB); it!=succ_end(blockB); ++it) {
		if (const Loop* L = loopInfo.getLoopFor(*it)) {
			if (L->getHeader() == *it) continue;
		}
		if (blockA == *it) return true;
		isDominated |= isDominatedBy(blockA, *it, loopInfo);
	}
	return isDominated;
}

//returns true if instA 'follows' instB or instA == instB
inline bool isDominatedBy(const Instruction* instA, const Instruction* instB, const LoopInfo& loopInfo) {
	if (instA == instB) return true;

	const BasicBlock* blockA = instA->getParent();
	const BasicBlock* blockB = instB->getParent();
	if (blockA != blockB) return isDominatedBy(blockA, blockB, loopInfo);

	for (BasicBlock::const_iterator I=blockA->begin(); I!=blockA->end(); ++I) {
		if (instA == I) return false;
		if (instB == I) return true;
	}
	assert (!"CRITICAL ERROR in 'isDominatedBy()'!");
	return false;
}

// Returns the unique return block of function 'f'.
// We rely on the ReturnUnifier pass and thus terminate as soon as we
// have found a return.
BasicBlock* findReturnBlock(Function& f) {
	for (Function::iterator BB=f.begin(), BBE=f.end(); BB!=BBE; ++BB) {
		assert (BB->getTerminator() && "each basic block has to have a terminator!");
		if (isa<ReturnInst>(BB->getTerminator())) return BB;
	}
	errs() << "ERROR: Function does not contain a return statement!\n";
	assert (false && "ERROR: Function does not contain a return statement!");
	return NULL;
}


void writeModuleToFile(Module* mod, const std::string& fileName) {
	assert (mod);
	std::string errorMessage = "";
	raw_fd_ostream file(fileName.c_str(), errorMessage);
	mod->print(file, NULL);
	file.close();
	if (errorMessage != "") {
		errs() << "ERROR: printing module to file failed: " << errorMessage << "\n";
	}
}
void writeFunctionToFile(Function* f, const std::string & fileName) {
	assert (f);
	std::string errorMessage = "";
	raw_fd_ostream file(fileName.c_str(), errorMessage);
	f->print(file, NULL);
	file.close();
	if (errorMessage != "") {
		errs() << "ERROR: printing function to file failed: " << errorMessage << "\n";
	}
}

// TODO: is that required?? -> use llvm::InlineFunctionInto()
inline void inlineFunctionCalls(Function* f) {
    bool functionChanged = true;
    while (functionChanged) {
        functionChanged = false;
        for (Function::iterator BB=f->begin(); BB!=f->end(); ++BB) {
            bool blockChanged = false;
            for (BasicBlock::iterator I=BB->begin(); !blockChanged && I!=BB->end();) {
                if (!isa<CallInst>(I)) { ++I; continue; }
                CallInst* call = cast<CallInst>(I++);
                Function* callee = call->getCalledFunction();
                if (!callee) {
                    //errs() << "ERROR: could not inline function call: " << *call;
                    continue;
                }
                if (callee->getAttributes().hasAttrSomewhere(Attribute::NoInline)) {
                    //DEBUG_OPT( outs() << "    function '" << callee->getNameStr() << "' has attribute 'no inline', ignored call!\n"; )
                    continue;
                }

                const std::string calleeName = callee->getNameStr(); //possibly deleted by InlineFunction()

				InlineFunctionInfo IFI(NULL, NULL); //new TargetData(mod));
                blockChanged = InlineFunction(call, IFI);
                functionChanged |= blockChanged;
            }
        }
    }
}

// adopted from: llvm-2.9/tools/opt
void optimizeModule(Module* mod) {
	assert (mod);

	// Initialize passes
	//PassRegistry &Registry = *PassRegistry::getPassRegistry();
	//initializeCore(Registry);
	//initializeScalarOpts(Registry);
	//initializeIPO(Registry);
	//initializeAnalysis(Registry);
	//initializeIPA(Registry);
	//initializeTransformUtils(Registry);
	//initializeInstCombine(Registry);
	//initializeInstrumentation(Registry);
	//initializeTarget(Registry);

	PassManager Passes;

	TargetLibraryInfo *TLI = new TargetLibraryInfo(Triple(mod->getTargetTriple()));
	Passes.add(TLI);

	TargetData *TD = 0;
	const std::string &ModuleDataLayout = mod->getDataLayout();
	if (!ModuleDataLayout.empty()) TD = new TargetData(ModuleDataLayout);
	//else TD = new TargetData(""); //provide default layout?

	if (TD) Passes.add(TD);

	PassManager* FPasses = new PassManager();
	if (TD) FPasses->add(new TargetData(*TD));

	Passes.add(createStripSymbolsPass(true));

	// AddStandardLinkPasses
	createStandardLTOPasses(&Passes,
						 /*Internalize=*/ true,
						 /*RunInliner=*/ true,
						 /*VerifyEach=*/ false);

	// AddOptimizationPasses
	const unsigned OptLevel = 3;
	createStandardFunctionPasses(FPasses, OptLevel);

	const unsigned Threshold = 275;
	createStandardModulePasses(&Passes,
							 OptLevel,
							 /*OptimizeSize=*/ false,
							 /*UnitAtATime=*/ false, // IPO -> same as llvm-gcc's -funit-at-a-time
							 /*UnrollLoops=*/ true,
							 /*SimplifyLibCalls=*/ true,
							 /*HaveExceptions=*/ true,
							 createFunctionInliningPass(Threshold));

	Passes.add(createVerifierPass());

	FPasses->run(*mod);
	Passes.run(*mod);
}

/// adopted from: llvm-2.9/include/llvm/Support/StandardPasses.h
void optimizeFunctionNew(Function* f, const bool disableLICM=false, const bool disableLoopRotate=false) {
	assert (f);
	assert (f->getParent());
	Module* mod = f->getParent();
	TargetData* targetData = new TargetData(mod);

	const unsigned OptimizationLevel = 3;
	//const bool OptimizeSize = false;
	//const bool UnitAtATime = true;
	const bool UnrollLoops = true;
	const bool SimplifyLibCalls = true;
	//const bool HaveExceptions = false;
	//Pass* InliningPass = createFunctionInliningPass(275);

	//PassManager Passes;
	FunctionPassManager Passes(mod);
	Passes.add(targetData);

	//
	// custom
	//
	Passes.add(createScalarReplAggregatesPass(-1, false));

	//
	// createStandardFunctionPasses
	//
	Passes.add(createCFGSimplificationPass());
	Passes.add(createPromoteMemoryToRegisterPass());
	Passes.add(createInstructionCombiningPass());

	// Add TypeBasedAliasAnalysis before BasicAliasAnalysis so that
	// BasicAliasAnalysis wins if they disagree. This is intended to help
	// support "obvious" type-punning idioms.
	Passes.add(createTypeBasedAliasAnalysisPass());
	Passes.add(createBasicAliasAnalysisPass());

	// Start of function pass.
	// Break up aggregate allocas, using SSAUpdater.
	Passes.add(createScalarReplAggregatesPass(-1, false));
	Passes.add(createEarlyCSEPass());              // Catch trivial redundancies
	if (SimplifyLibCalls)
		Passes.add(createSimplifyLibCallsPass());    // Library Call Optimizations
	Passes.add(createJumpThreadingPass());         // Thread jumps.
	Passes.add(createCorrelatedValuePropagationPass()); // Propagate conditionals
	Passes.add(createCFGSimplificationPass());     // Merge & remove BBs
	Passes.add(createInstructionCombiningPass());  // Combine silly seq's

	Passes.add(createTailCallEliminationPass());   // Eliminate tail calls
	Passes.add(createCFGSimplificationPass());     // Merge & remove BBs
	Passes.add(createReassociatePass());           // Reassociate expressions
	if (!disableLoopRotate) Passes.add(createLoopRotatePass());            // Rotate Loop // makes packetized Mandelbrot fail
	if (!disableLICM) Passes.add(createLICMPass());                  // Hoist loop invariants // makes scalar driver crash after optimization
	//Passes.add(createLoopUnswitchPass(OptimizeSize || OptimizationLevel < 3)); // breaks DCT with UNIFORM_ANALYSIS=0
	Passes.add(createInstructionCombiningPass());
	Passes.add(createIndVarSimplifyPass());        // Canonicalize indvars
	Passes.add(createLoopIdiomPass());             // Recognize idioms like memset.
	Passes.add(createLoopDeletionPass());          // Delete dead loops
	if (UnrollLoops)
		Passes.add(createLoopUnrollPass());          // Unroll small loops
	Passes.add(createInstructionCombiningPass());  // Clean up after the unroller
	if (OptimizationLevel > 1)
		Passes.add(createGVNPass());                 // Remove redundancies
	Passes.add(createMemCpyOptPass());             // Remove memcpy / form memset
	Passes.add(createSCCPPass());                  // Constant prop with SCCP

	// Run instcombine after redundancy elimination to exploit opportunities
	// opened up by them.
	Passes.add(createInstructionCombiningPass());
	Passes.add(createJumpThreadingPass());         // Thread jumps
	Passes.add(createCorrelatedValuePropagationPass());
	Passes.add(createDeadStoreEliminationPass());  // Delete dead stores
	Passes.add(createAggressiveDCEPass());         // Delete dead instructions
	Passes.add(createCFGSimplificationPass());     // Merge & remove BBs

#ifdef DEBUG_PACKETIZER
	Passes.add(createVerifierPass());
#endif

	Passes.doInitialization();
	Passes.run(*f);
	Passes.doFinalization();
}

/// adopted from: llvm-2.5/tools/llvm-ld
/// TODO: update!!!
void optimizeFunction(Function* f) {
    assert (f);
    assert (f->getParent());
    Module* mod = f->getParent();

    inlineFunctionCalls(f);


#ifdef DEBUG_LLVM_PASSES
	DebugFlag = true;
#endif

    //PassManager Passes;
    FunctionPassManager Passes(mod);
    Passes.add(new TargetData(mod));
    Passes.add(createScalarReplAggregatesPass(2048)); // Break up allocas, override default threshold of maximum struct size of 128 bytes
    Passes.add(createInstructionCombiningPass());
    Passes.add(createJumpThreadingPass());        // Thread jumps.
    Passes.add(createReassociatePass());
    Passes.add(createInstructionCombiningPass());
    Passes.add(createCFGSimplificationPass());
    Passes.add(createAggressiveDCEPass()); //fpm.add(createDeadCodeEliminationPass());
    Passes.add(createScalarReplAggregatesPass(2048)); // Break up allocas, override default threshold of maximum struct size of 128 bytes
    Passes.add(createLICMPass());                 // Hoist loop invariants   //Pass
    Passes.add(createGVNPass());                  // Remove redundancies
    Passes.add(createMemCpyOptPass());            // Remove dead memcpy's
    Passes.add(createDeadStoreEliminationPass()); // Nuke dead stores
    Passes.add(createInstructionCombiningPass());
    Passes.add(createJumpThreadingPass());        // Thread jumps.
    Passes.add(createPromoteMemoryToRegisterPass()); // Cleanup jumpthread.
    Passes.add(createCFGSimplificationPass());

    //custom
    Passes.add(createSCCPPass()); //fpm.add(createConstantPropagationPass());
    Passes.add(createTailCallEliminationPass());
    Passes.add(createAggressiveDCEPass());

	// custom, added after llvm 2.7
	// -> wrong results in OpenCL-Mandelbrot!!!
	// -> without: bad optimization of OpenCL-SimpleConvolution
	/*Passes.add(createLoopRotatePass());            // Rotate Loop
    Passes.add(createLICMPass());                  // Hoist loop invariants
    Passes.add(createLoopUnswitchPass(false));
    Passes.add(createInstructionCombiningPass());
    Passes.add(createIndVarSimplifyPass());        // Canonicalize indvars
    Passes.add(createLoopDeletionPass());          // Delete dead loops
    Passes.add(createLoopUnrollPass());          // Unroll small loops
	*/
    //clean up again
    Passes.add(createInstructionCombiningPass());
    Passes.add(createReassociatePass());
    Passes.add(createGVNPass());
    Passes.add(createCFGSimplificationPass());
    Passes.add(createAggressiveDCEPass());

#ifdef DEBUG_PACKETIZER
    Passes.add(createVerifierPass());
#endif

    //DEBUG_OPT( llvm::TimerGroup tg("llvmOptimization"); )
    //DEBUG_OPT( llvm::Timer t("llvmOptimization", tg); )
    //DEBUG_OPT( t.startTimer(); )

	Passes.doInitialization();
    Passes.run(*f);
	Passes.doFinalization();

#ifdef DEBUG_LLVM_PASSES
	DebugFlag = false;
#endif

    //DEBUG_OPT( t.stopTimer(); )
    //DEBUG_OPT( outs() << "done.\ndone.\n"; )
    //DEBUG_OPT( t.print(outs()); )
}


// insert print statement that prints 'value' preceeded by 'DEBUG: `message`'
// example what can be generated:
// declare i32 @printf(i8* noalias nocapture, ...) nounwind
// @.str1 = private constant [19 x i8] c"DEBUG: indexA: %d\0A\00", align 1 ; <[19 x i8]*> [#uses=1]
// %printf1 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([19 x i8]* @.str1, i64 0, i64 0), i32 %call) ; <i32> [#uses=0]
// Usage Example:
// insertPrintf("else-block executed! - pointerIdx: ", pointerIdx, true, (*elseBB)->getTerminator());
CallInst* insertPrintf(const std::string& message, Value* value, const bool endLine, Instruction* insertBefore) {
	assert (value && insertBefore);
	assert (insertBefore->getParent());
	Function* f = insertBefore->getParent()->getParent();
	assert (f);
	Module* mod = f->getParent();
	assert (mod);

	Function* func_printf =  mod->getFunction("printf");

	if (!func_printf) {
		PointerType* PointerTy_6 = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);

		std::vector<const Type*>FuncTy_10_args;
		FuncTy_10_args.push_back(PointerTy_6);
		FunctionType* FuncTy_10 = FunctionType::get(
				/*Result=*/IntegerType::get(mod->getContext(), 32),
				/*Params=*/FuncTy_10_args,
				/*isVarArg=*/true);

		func_printf = Function::Create(
				/*Type=*/FuncTy_10,
				/*Linkage=*/GlobalValue::ExternalLinkage,
				/*Name=*/"printf", mod); // (external, no body)
		func_printf->setCallingConv(CallingConv::C);
		AttrListPtr func_printf_PAL;
		{
			SmallVector<AttributeWithIndex, 4> Attrs;
			AttributeWithIndex PAWI;
			PAWI.Index = 1U; PAWI.Attrs = 0  | Attribute::NoAlias | Attribute::NoCapture;
			Attrs.push_back(PAWI);
			PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind;
			Attrs.push_back(PAWI);
			func_printf_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

		}
		func_printf->setAttributes(func_printf_PAL);
	}

	const bool valueIsVector = value->getType()->isVectorTy();
	const unsigned vectorSize = valueIsVector ? cast<VectorType>(value->getType())->getNumElements() : 0;
	const unsigned stringSize = message.length() +
		(valueIsVector ? vectorSize*2 : 2) +
		(valueIsVector ? vectorSize-1 : 0) +
		(endLine ? 2 : 1) +
		7;
	ArrayType* ArrayTy_0 = ArrayType::get(IntegerType::get(mod->getContext(), 8), stringSize);
	GlobalVariable* gvar_array__str = new GlobalVariable(/*Module=*/*mod,
			/*Type=*/ArrayTy_0,
			/*isConstant=*/true,
			/*Linkage=*/GlobalValue::PrivateLinkage,
			/*Initializer=*/0, // has initializer, specified below
			/*Name=*/".str");
	gvar_array__str->setAlignment(1);

	// Constant Definitions
	std::string str = "";
	switch (value->getType()->getTypeID()) {
		case Type::IntegerTyID : str = "%d"; break;
		case Type::FloatTyID   : str = "%f"; break;
		case Type::PointerTyID : str = "%x"; break;
		case Type::VectorTyID  : {
			std::string tmp;
			switch (value->getType()->getContainedType(0)->getTypeID()) {
				case Type::IntegerTyID : tmp = "%d"; break;
				case Type::FloatTyID   : tmp = "%f"; break;
				default                : tmp = "%x"; break;
			}
			for (unsigned i=0; i<vectorSize; ++i) {
				if (i != 0) str += " ";
				str += tmp;
			}
			break;
		}
		default                : str = "%x"; break;
	}

	std::stringstream sstr;
	sstr << "DEBUG: " << message << str << (endLine ? "\x0A" : "");
	Constant* const_array_11 = ConstantArray::get(mod->getContext(), sstr.str(), true);
	std::vector<Constant*> const_ptr_17_indices;
	ConstantInt* const_int64_18 = ConstantInt::get(mod->getContext(), APInt(64, StringRef("0"), 10));
	const_ptr_17_indices.push_back(const_int64_18);
	const_ptr_17_indices.push_back(const_int64_18);
	Constant* const_ptr_17 = ConstantExpr::getGetElementPtr(gvar_array__str, &const_ptr_17_indices[0], const_ptr_17_indices.size());

	// Global Variable Definitions
	gvar_array__str->setInitializer(const_array_11);


	std::vector<Value*> int32_51_params;
	int32_51_params.push_back(const_ptr_17);
	if (valueIsVector) {
		for (unsigned i=0; i<vectorSize; ++i) {
			ExtractElementInst* ei = ExtractElementInst::Create(value, ConstantInt::get(mod->getContext(), APInt(32, i)), "printfElem", insertBefore);
			int32_51_params.push_back(ei);
		}
	} else {
		int32_51_params.push_back(value);
	}
	CallInst* int32_51 = CallInst::Create(func_printf, int32_51_params.begin(), int32_51_params.end(), "", insertBefore);
	return int32_51;
}


} // namespace Packetizer


#endif /* _LLVMTOOLS_HPP */

