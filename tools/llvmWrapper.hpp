/**
 * @file   llvmWrapper.h
 * @date   10.05.2011
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2008, 2009, 2010, 2011, 2012 Saarland University
 *
 */

#ifndef _LLVMWRAPPER_H
#define _LLVMWRAPPER_H

#ifdef __INTEL_COMPILER
	#pragma warning( disable : 279 )
#endif

//#define DEBUG_LLVM_PASSES
#ifdef DEBUG_LLVM_PASSES
#include <llvm/Support/Debug.h>
#endif

#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/system_error.h>
#include <llvm/Support/Debug.h> //dbgs()

#include <vector>
#include <map>

//bitcode reading
#include <llvm/Module.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Analysis/Verifier.h>

// linking
#include <llvm/Linker.h>

// execution
#include <llvm/Module.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JITMemoryManager.h>
#include <llvm/ExecutionEngine/JIT.h> //required to prevent "JIT has not been linked in" errors
//#include <llvm/ExecutionEngine/MCJIT.h> // LLVMLinkInMCJIT()

#include <llvm/Target/TargetData.h>
#include <llvm/Support/TargetSelect.h>

// optimization
#include <llvm/PassManager.h>
#include <llvm/LinkAllPasses.h>
#include <llvm/CodeGen/Passes.h>
#include <llvm/Transforms/IPO.h> //FunctionInliningPass
#include <llvm/Transforms/Utils/Cloning.h> //InlineFunction
#include <llvm/Support/Timer.h>

#include <llvm/LLVMContext.h>

#include "llvm/Support/ManagedStatic.h" // llvm_shutdown()

#include <llvm/Support/InstIterator.h>

using namespace llvm;

namespace LLVMWrapper {

//
// create module, search function, etc.
//

Module*
createModuleFromFile(const std::string & fileName)
{
	std::string errorMessage;

	//create memory buffer for file
	OwningPtr<MemoryBuffer> fileBuffer;
	error_code e = MemoryBuffer::getFile(fileName.c_str(), fileBuffer);

	if (e)
	{
		errs() << "Error reading file '"
			<< fileName << "': " << e.message() << "\n";
		return NULL;
	}

	if (!fileBuffer)
	{
		errs() << "Error reading file '" << fileName << "'.\n";
		return NULL;
	}

	if (fileBuffer->getBufferSize() & 3)
	{
		errs() << "Error: Bitcode stream should be "
			<< "a multiple of 4 bytes in length\n";
		return NULL;
	}

	//parse file

	Module* mod = ParseBitcodeFile(fileBuffer.get(), getGlobalContext(), &errorMessage);

	if (errorMessage != "")
	{
		errs() << "Error reading bitcode file: " << errorMessage << "\n";
		return NULL;
	}

	if (!mod)
	{
		errs() << "Error reading bitcode file.\n";
		return NULL;
	}

	return mod;
}

void
verifyModule(Module* module)
{
	assert(module && "module must not be NULL");
	//use LLVMs own verifier pass
	std::string errorMessage;
	if (verifyModule(*module, AbortProcessAction, &errorMessage))
	{
		errs() << "\nverification of module failed:\n" << errorMessage << "\n";
	}
}

//@returns the function or null if it was not found in the specified module
Function*
getFunction(const std::string& name, Module* module)
{
	assert (module);
	return module->getFunction(name);
}

std::string
getFunctionName(const Function* f)
{
	assert (f);
	return f->getName();
}

Module*
getModule(Function* f)
{
	assert (f);
	if (!f->getParent())
	{
		errs() << "ERROR: function '" << f->getName()
			<< "' has no parent module!\n";
		return NULL;
	}
	return f->getParent();
}

//
// linking
//

// link source module into target module
Module*
linkInModule(Module* target, Module* source)
{
	assert (source && target);
	//outs() << "linking src module '" << source->getModuleIdentifier()
	//<< "' into dest module '" << target->getModuleIdentifier()
	//<< "'... ";
	std::auto_ptr<llvm::Linker> linker(new llvm::Linker("Packetizer Linker", target));
	assert (linker.get() != 0);
	std::string errorMessage;
	if (linker->LinkInModule(source, &errorMessage))
	{
		errs() << "ERROR: Could not link source module into cloned target: " << errorMessage << "\n";
		return NULL;
	}

	return linker->releaseModule();
}

//
// execution
//

//we must prevent creation of several JITs
static ExecutionEngine *globalExecEngine = 0;

ExecutionEngine*
getExecutionEngine()
{
	if (!globalExecEngine)
	{
		errs() << "ERROR: no execution engine available, create one first!\n";
		return NULL;
	}
	return globalExecEngine;
}

ExecutionEngine*
createExecutionEngine(Module* mod, const bool useAVX)
{
	assert (mod);

	if (!globalExecEngine)
	{
		//we first have to initialize the native target for code generation
		const bool initFailed = InitializeNativeTarget();
		//InitializeAllTargetMCs();
		//LLVMLinkInMCJIT();
		InitializeNativeTargetAsmPrinter(); // Required, otherwise "LLVM ERROR: Target does not support MC emission!" if setUseMCJIT() is set to 'true'

		if (initFailed)
		{
			errs() << "ERROR: could not initialize native target (required for "
				<< "LLVM execution engine)\n";
			return NULL;
		}

		std::string errorMessage = "";

		EngineBuilder eb = EngineBuilder(mod);
		eb.setEngineKind(EngineKind::JIT);
		eb.setErrorStr(&errorMessage);
		eb.setJITMemoryManager(JITMemoryManager::CreateDefaultMemManager());
		eb.setOptLevel(CodeGenOpt::Aggressive);
		eb.setAllocateGVsWithCode(false);
		eb.setRelocationModel(Reloc::Default);
		eb.setCodeModel(CodeModel::JITDefault); // Default crashes with "invalid rip-relative address"
		eb.setMArch("x86-64");
        eb.setMCPU("corei7-avx"); //eb.setMCPU("penryn");
        eb.setUseMCJIT(true);
		std::vector<std::string> attrs;
		if (useAVX) {
			outs() << "Creating ExecutionEngine with AVX support!\n";
			attrs.push_back("avx");
		} else {
			attrs.push_back("sse41");
		}
		eb.setMAttrs(attrs);

		globalExecEngine = eb.create();

		if (errorMessage != "")
		{
			errs() << "ERROR: could not create execution engine for module "
				<< mod->getModuleIdentifier() << ": " << errorMessage << "\n";
			return NULL;
		}

		if (!globalExecEngine)
		{
			errs() << "ERROR: could not create execution engine for module "
				<< mod->getModuleIdentifier() << "!\n";
			return NULL;
		}

	} else {
		if (globalExecEngine->removeModule(mod))
		{
			errs() << "WARNING: module '"
				<< mod->getModuleIdentifier()
				<< "' was already added to execution engine"
				<< " (removed and re-added)!\n";
		}
		globalExecEngine->addModule(mod);
	}

	globalExecEngine->runStaticConstructorsDestructors(false);

	return globalExecEngine;
}

bool
removeModule(Module* mod, ExecutionEngine* engine)
{
	assert (mod && engine);
	return engine->removeModule(mod);
}

void
deleteModule(llvm::Module * mod)
{
	llvm::ExecutionEngine *engine = getExecutionEngine();
	if (engine && mod)
	{
		engine->clearGlobalMappingsFromModule(mod);
		if (!engine->removeModule(mod))
		{
			errs() << "WARNING: could not remove module '" << mod->getModuleIdentifier() << "' from engine!\n";
		}
	}
	delete mod;
}

void*
getPointerToFunction(Function* f, ExecutionEngine* engine)
{
	assert (engine && f);
	return engine->getPointerToFunction(f);
}

int
executeMain(void* mainPtr, int argc, char **argv)
{
	outs() << "executing function 'main()'... ";
	//example: cast function-pointer to fn: int -> int
	//int (*add1)(int) = (int (*)(int))functions[0][0];
	//execute with arbitrary argument
	//int z = add1(12);

	TimerGroup tg("executeMain");
	llvm::Timer t("executeMain", tg);

	t.startTimer();
	//cast mainPtr to function with arguments int and char**,
	//then invoke it with argc and argv
	int res = ((int (*)(int, char**))mainPtr)(argc,argv);

	t.stopTimer();
	outs() << "done.\n";

	tg.print(dbgs());

	return res;
}

static void
shutdown()
{
	outs() << "shutting down... ";
	globalExecEngine->runStaticConstructorsDestructors(true);
	delete globalExecEngine;
	//llvm_shutdown(); // "pointer being freed was not allocated"
	outs() << "done.\n";
}

//
// optimization
//

void
inlineFunctionCalls(Function* f)
{
	bool functionChanged = true;
	while (functionChanged) {
		functionChanged = false;
		for (Function::iterator BB=f->begin(); BB!=f->end(); ++BB) {
			bool blockChanged = false;
			for (BasicBlock::iterator I=BB->begin(); !blockChanged && I!=BB->end();) {
				if (!isa<CallInst>(I)) { ++I; continue; }
				CallInst* call = cast<CallInst>(I++);
				Function* callee = call->getCalledFunction();
				if (!callee)
				{
					//errs() << "ERROR: could not inline function call: " << *call;
					continue;
				}
				if (callee->getAttributes().hasAttrSomewhere(Attribute::NoInline))
				{
					//DEBUG_OPT( outs() << "    function '" << callee->getName() << "' has attribute 'no inline', ignored call!\n"; )
					continue;
				}

				const std::string calleeName = callee->getName(); //possibly deleted by InlineFunction()

				InlineFunctionInfo IFI(NULL, NULL); //new TargetData(mod));
				blockChanged = InlineFunction(call, IFI);
				functionChanged |= blockChanged;
			}
		}
	}
}

/// adopted from: llvm-2.5/tools/llvm-ld
void
optimizeFunction(Function* f)
{
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

// Transform all functions in the module to only use registers if possible
// (= construct SSA from load-store-code that is often produced by front ends)
// Adding the following line would perform optimizations similar to GCC's -O3
// createStandardModulePasses(&Passes, 3, false, false, true, true, false, createFunctionInliningPass());
void
optimizeModuleSimple(Module* mod)
{
	assert (mod);

	PassManager Passes;

	// remove debug symbols etc. (packetizer can not handle it) (#17)
	Passes.add(createStripSymbolsPass());
	// transform to SSA
	Passes.add(createTypeBasedAliasAnalysisPass());
	Passes.add(createBasicAliasAnalysisPass());
	Passes.add(createScalarReplAggregatesPass(-1, true));
	Passes.add(createPromoteMemoryToRegisterPass());
	// remove dead code (packetizer can not handle it)
	Passes.add(createDeadCodeEliminationPass());

	Passes.run(*mod);
}

//
// output / debugging
//

void
writeModuleToFile(Module* mod, const std::string& fileName)
{
	assert (mod);
	std::string errorMessage = "";
	raw_fd_ostream file(fileName.c_str(), errorMessage);
	mod->print(file, NULL);
	file.close();
	if (errorMessage != "")
	{
		errs() << "ERROR: printing module to file failed: " << errorMessage << "\n";
	}
}

void
writeFunctionToFile(Function* f, const std::string & fileName)
{
	assert (f);
	std::string errorMessage = "";
	raw_fd_ostream file(fileName.c_str(), errorMessage);
	f->print(file, NULL);
	file.close();
	if (errorMessage != "")
	{
		errs() << "ERROR: printing function to file failed: " << errorMessage << "\n";
	}
}



} // namespace LLVMWrapper

#endif /* _LLVMWRAPPER_H */
