/**
 * @file   packetizeFunction.cpp
 * @date   07.12.2009
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2009, 2010, 2011 Saarland University
 *
 * Packetize given function "into" given prototype declared in input module.
 * The generated function is either printed to stdout or dumped to given file.
 *
 */
#include <string.h> //strcmp
#include <iostream>
#include <vector>

#include <cassert>

#ifdef USE_C_BINDINGS
	#include "packetizerAPI_C.h"
#else
	#include "packetizerAPI.hpp"
#endif

#include "llvmWrapper.hpp"

int main(int argc, char** argv) {

	atexit(LLVMWrapper::shutdown);

	bool verbose = false;
	bool use_sse41 = true;
	bool use_avx = false;
	bool do_not_optimize = false;
	bool verify = false;
	bool outputFunctionOnly = false;
	std::vector<std::string> linkModuleNames;
	std::string moduleName = "";
	std::string functionName = "";
	std::string targetFunctionName = "";
	std::string outputFileName = "";

	// print copyright information
	std::cout << "\n\
********************************************************************************\n\
*     Whole-Function Vectorization                                             *\n\
*                                                                              *\n\
*     Automatically transform a scalar function into its SIMD counterpart that *\n\
*     produces the same results as N parallel executions of the source         *\n\
*     function (where N is the SIMD width).                                    *\n\
*                                                                              *\n\
*     Copyright (C) 2008, 2009, 2010, 2011 Ralf Karrenberg                     *\n\
*                                                                              *\n\
*     This file is distributed under the University of Illinois Open Source    *\n\
*     License. See the COPYING file in the root directory for details.         *\n\
*                                                                              *\n\
********************************************************************************\n\
		\n";

	///////////////////////////////////////////////////////////////////////////
	//                           read input                                  //
	///////////////////////////////////////////////////////////////////////////

	bool displayUsage = false;

	for (int i=1; i<argc; ++i) {
		if (strcmp(argv[i], "-m") == 0) {
			if (i+1 > argc) {
				std::cerr << "ERROR: no bitcode module specified!\n";
				return -1;
			}
			moduleName = argv[++i];
			continue;
		}
		if (strcmp(argv[i], "-f") == 0) {
			if (i+1 > argc) {
				std::cerr << "ERROR: no function name specified!\n";
				return -1;
			}
			functionName = argv[++i];
			continue;
		}
		if (strcmp(argv[i], "-t") == 0) {
			if (i+1 > argc) {
				std::cerr << "ERROR: no target function name specified!\n";
				return -1;
			}
			targetFunctionName = argv[++i];
			continue;
		}
		if (strcmp(argv[i], "-of") == 0) {
			if (i+1 > argc) {
				std::cerr << "ERROR: output filename missing!\n";
				return -1;
			}
			outputFileName = argv[++i];
			outputFunctionOnly = true;
			continue;
		}
		if (strcmp(argv[i], "-o") == 0) {
			if (i+1 > argc) {
				std::cerr << "ERROR: output filename missing!\n";
				return -1;
			}
			outputFileName = argv[++i];
			continue;
		}
		if (strcmp(argv[i], "-l") == 0) {
			if (i+1 > argc) {
				std::cerr << "ERROR: link module filename missing!\n";
				return -1;
			}
			linkModuleNames.push_back(argv[++i]);
			continue;
		}

		if (strcmp(argv[i], "-msse41") == 0) {
			use_sse41 = true;
			continue;
		}
		if (strcmp(argv[i], "-no-sse41") == 0) {
			use_sse41 = false;
			continue;
		}
		if (strcmp(argv[i], "-mavx") == 0) {
			use_avx = true;
			continue;
		}
		if (strcmp(argv[i], "-no-avx") == 0) {
			use_avx = false;
			continue;
		}
		if (strcmp(argv[i], "-O0") == 0 || strcmp(argv[i], "-no-opt") == 0) {
			do_not_optimize = true;
			continue;
		}
		if (strcmp(argv[i], "-verify") == 0) {
			verify = true;
			continue;
		}
		if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "-verbose") == 0) {
			verbose = true;
			continue;
		}
		if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "-help") == 0) {
			displayUsage = true;
			continue;
		}
		std::cerr << "WARNING: Unknown input found: " << argv[i] << "\n";
	}

	if (!displayUsage) {
		// print error messages if mandatory input is missing
		if (argc < 1) {
			std::cerr << "ERROR: Wrong number of arguments!\n";
			displayUsage = true;
		}
		if (moduleName == "") {
			std::cerr << "ERROR: no source module specified!\n";
			displayUsage = true;
		}
		if (functionName == "") {
			std::cerr << "ERROR: no source function specified!\n";
			displayUsage = true;
		}
		if (targetFunctionName == "") {
			std::cerr << "ERROR: no target function specified!\n";
			displayUsage = true;
		}
	}

	//if necessary, print usage message
	if (displayUsage) {
		std::cerr << "\nUsage: " << argv[0] << " -m module.bc -f functionName -t targetFunctionName [options]\n";
		std::cerr << "Parameters:\n";
		std::cerr << "  -m FILE     : source bitcode module\n";
		std::cerr << "  -f NAME     : name of scalar source function in specified module\n";
		std::cerr << "  -t NAME     : name of target function prototype in specified module\n";
		std::cerr << "Options:\n";
		std::cerr << "  -o FILE     : output filename for human readable bitcode (.ll file)\n";
		std::cerr << "  -of FILE    : same as -o, but only output packetized code instead of entire module\n";
		std::cerr << "  -l FILE     : additional module that should be linked to source module\n";
		std::cerr << "  -no-sse41   : disable generation of SSE4.1 intrinsics\n";
		std::cerr << "  -O0 -no-opt : disable module optimization after packetization\n";
		std::cerr << "  -v -verbose : enable verbose output (only valid for debug builds)\n";
		std::cerr << "  -verify     : enable verification of module after packetization\n";
		std::cerr << "  -h -help    : display help message\n";
		return 0;
	}

	///////////////////////////////////////////////////////////////////////////
	//                       1. create source module                         //
	///////////////////////////////////////////////////////////////////////////

	llvm::Module* mod = LLVMWrapper::createModuleFromFile(moduleName);
	if (!mod) {
		std::cerr << "ERROR: could not create source module from file '" << moduleName << "'!\n";
		return -1;
	}

	///////////////////////////////////////////////////////////////////////////
	//                       2. create link module(s)                        //
	///////////////////////////////////////////////////////////////////////////

	const unsigned moduleNr = linkModuleNames.size();
	llvm::Module** linkModules = new llvm::Module*[moduleNr]();
	for (unsigned i=0; i<moduleNr; ++i) {
		linkModules[i] = LLVMWrapper::createModuleFromFile(linkModuleNames[i]);
		if (linkModules[i] == NULL) {
			std::cerr << "ERROR: could not create module from file '" << linkModuleNames[i] << "'!\n";
			return -1;
		}
	}

	///////////////////////////////////////////////////////////////////////////
	//                          3. link modules                              //
	///////////////////////////////////////////////////////////////////////////

	if (moduleNr > 1) {
		std::cout << "linking each module with each other... \n";

		mod = LLVMWrapper::linkInModule(mod, linkModules[moduleNr-1]);
		for (unsigned i=0; i<moduleNr; ++i) {
			llvm::Module* tmp = linkModules[i];
			for (unsigned j=0; j<moduleNr; ++j) {
				if (i == j) continue; //do not link module with itself
				LLVMWrapper::linkInModule(tmp, linkModules[j]);
			}
			if (i != moduleNr-1) LLVMWrapper::linkInModule(mod, tmp); //do not link main with main
		}

		std::cout << "linking done.\n";
	}

	if (!mod) {
		std::cerr << "ERROR: linking of modules failed!\n";
		return -1;
	}

	///////////////////////////////////////////////////////////////////////////
	//                     4. run optimization passes                        //
	///////////////////////////////////////////////////////////////////////////

	if (!do_not_optimize) LLVMWrapper::optimizeFunction(LLVMWrapper::getFunction(targetFunctionName, mod));

	///////////////////////////////////////////////////////////////////////////
	//                       5. packetize function                           //
	///////////////////////////////////////////////////////////////////////////

	if (!LLVMWrapper::getFunction(functionName, mod)) {
		std::cerr << "ERROR: source function '" << functionName << "' not found in module!\n";
		return -1;
	}
	if (!LLVMWrapper::getFunction(targetFunctionName, mod)) {
		std::cerr << "ERROR: target function '" << targetFunctionName  << "' not found in module!\n";
		return -1;
	}

	const unsigned simdWidth = 4;
	const unsigned packetizationSize = 4;

#ifdef USE_C_BINDINGS
	// This demonstrates the usage of the C bindings:
	LLVMModuleRef modRef = wrap(mod);
	PKTPacketizerRef packetizer = PKTCreatePacketizer(modRef, simdWidth, packetizationSize, use_sse41, use_avx, verbose);
	PKTAddFunction(packetizer, functionName.c_str(), targetFunctionName.c_str());
	PKTRun(packetizer);
#else
	Packetizer::Packetizer packetizer(*mod, mod->getContext(), simdWidth, packetizationSize, use_sse41, use_avx, verbose);
	packetizer.addFunction(functionName, targetFunctionName);
	packetizer.run();
#endif

	if (!LLVMWrapper::getFunction(targetFunctionName, mod)) {
		std::cerr << "ERROR: packetized target function not found in module!\n";
		return -1;
	}

	///////////////////////////////////////////////////////////////////////////
	//                     6. run optimization passes                        //
	///////////////////////////////////////////////////////////////////////////

	if (!do_not_optimize) LLVMWrapper::optimizeFunction(LLVMWrapper::getFunction(targetFunctionName, mod));

	///////////////////////////////////////////////////////////////////////////
	//                         7. verify module                              //
	///////////////////////////////////////////////////////////////////////////

	if (verify) LLVMWrapper::verifyModule(mod);

	///////////////////////////////////////////////////////////////////////////
	//                          8. write bytecode                            //
	///////////////////////////////////////////////////////////////////////////


	if (outputFileName != "") {
		if (outputFunctionOnly) {
			LLVMWrapper::writeFunctionToFile(LLVMWrapper::getFunction(targetFunctionName, mod), outputFileName);
		} else {
			LLVMWrapper::writeModuleToFile(mod, outputFileName);
		}
	} else {
		llvm::outs() << *LLVMWrapper::getFunction(targetFunctionName, mod) << "\n";
		//outputFileName = targetFunctionName + ".ll";
	}

	for (unsigned i=0; i<moduleNr; ++i) {
		if (linkModules[i]) delete linkModules[i];
	}

	delete [] linkModules;

	return 0;
}
