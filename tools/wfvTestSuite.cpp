/**
 * @file   wfvTestSuite.cpp
 * @date   14.12.2008
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2008, 2009, 2010, 2011 Saarland University
 *
 */
#include <string.h> //strcmp
#include <iostream>

#include "packetizerAPI.hpp"
#include "llvmWrapper.hpp"

int main(int argc, char** argv) {

	atexit(LLVMWrapper::shutdown);

	// print copyright information
	std::cout << "\n\
********************************************************************************\n\
*     Whole-Function Vectorization Test Suite                                  *\n\
*                                                                              *\n\
*     This program serves as a driver for test suites.                         *\n\
*                                                                              *\n\
*     Copyright (C) 2008, 2009, 2010, 2011 Saarland University                 *\n\
*                                                                              *\n\
*     This file is distributed under the University of Illinois Open Source    *\n\
*     License. See the COPYING file in the root directory for details.         *\n\
*                                                                              *\n\
********************************************************************************\n\
		\n";

	///////////////////////////////////////////////////////////////////////////
	//                           read arguments                              //
	///////////////////////////////////////////////////////////////////////////

	bool        verbose         = false;
	bool        use_sse41       = true;
	bool        use_avx         = false;
	bool        do_not_optimize = false;
	bool        verify          = true;
	bool        dump            = false;
	unsigned    testSuiteIdx    = 1;
	std::string filename        = "test/wfvTests.bc";

	bool displayUsage = false;
	bool error = false;

	for (int i=1; i<argc; ++i) {
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
		if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "-verbose") == 0) {
			verbose = true;
			continue;
		}
		if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "-dump") == 0) {
			dump = true;
			continue;
		}
		if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "-help") == 0) {
			displayUsage = true;
			continue;
		}
		if (strcmp(argv[i], "-O0") == 0 || strcmp(argv[i], "-no-opt") == 0) {
			do_not_optimize = false;
			continue;
		}
		if (strcmp(argv[i], "-no-verify") == 0) {
			verify = false;
			continue;
		}
		if (strcmp(argv[i], "-test1") == 0) {
			testSuiteIdx = 1;
			filename = "test/wfvTests.bc";
			continue;
		}
		if (strcmp(argv[i], "-test2") == 0) {
			testSuiteIdx = 2;
			filename = "test/wfvTests2.bc";
			continue;
		}
		if (strcmp(argv[i], "-test3") == 0) {
			testSuiteIdx = 3;
			filename = "test/wfvTests3.bc";
			continue;
		}

		std::cerr << "ERROR: Unknown argument found: " << argv[i] << "\n\n";
		displayUsage = true;
		error = true;
	}

	if (displayUsage) {
		std::cerr << "Usage: " << argv[0] << " -testX [options]\n";
		std::cerr << "Arguments:\n";
		std::cerr << "  -testX      : execute test suite X (X = { 1, 2, 3 })\n";
		std::cerr << "Options:\n";
		std::cerr << "  -no-sse41   : disable generation of SSE4.1 intrinsics\n";
		std::cerr << "  -mavx       : ensable generation of AVX intrinsics\n";
		std::cerr << "  -no-avx     : disable generation of AVX intrinsics\n";
		std::cerr << "  -v -verbose : enable verbose output (only valid for debug builds)\n";
		std::cerr << "  -O0 -no-opt : disable module optimization after WFV\n";
		std::cerr << "  -no-verify  : disable verification of module after WFV\n";
		std::cerr << "  -d -dump    : enable dumping of test module\n";
		std::cerr << "  -h -help    : display this help message\n\n";
		return error ? -1 : 0;
	}



	///////////////////////////////////////////////////////////////////////////
	//                         1. create module                              //
	///////////////////////////////////////////////////////////////////////////

	llvm::Module* module = LLVMWrapper::createModuleFromFile(filename);
	if (!module) return -1;

	///////////////////////////////////////////////////////////////////////////
	//                  2. run simple optimization passes                    //
	///////////////////////////////////////////////////////////////////////////

	if (!do_not_optimize) {
		std::cout << "\noptimizing test suite module... ";
		LLVMWrapper::optimizeModuleSimple(module);
		std::cout << "done.\n";
	}

	///////////////////////////////////////////////////////////////////////////
	//                       3. vectorize function(s)                        //
	///////////////////////////////////////////////////////////////////////////

	const unsigned simdWidth = use_avx ? 8 : 4;
	const unsigned packetizationSize = use_avx ? 8 : 4;

	Packetizer::Packetizer packetizer(*module,
                                         module->getContext(),
                                         simdWidth,
                                         packetizationSize,
                                         use_sse41,
                                         use_avx,
                                         verbose);

	if (testSuiteIdx == 1) {

		//
		// test suite 1
		//

		// arithmetic only
		packetizer.addFunction("test_001_simple_scalar", "test_001_simple_generated");
		// simple control flow
		packetizer.addFunction("test_002_if01_scalar", "test_002_if01_generated");
		packetizer.addFunction("test_003_if02_scalar", "test_003_if02_generated");
		packetizer.addFunction("test_004_if03_scalar", "test_004_if03_generated");
		packetizer.addFunction("test_005_if04_scalar", "test_005_if04_generated");
		packetizer.addFunction("test_006_if05_scalar", "test_006_if05_generated");
		packetizer.addFunction("test_007_if06_scalar", "test_007_if06_generated");
		packetizer.addFunction("test_008_if07_scalar", "test_008_if07_generated");
		packetizer.addFunction("test_009_if08_scalar", "test_009_if08_generated");
		packetizer.addFunction("test_010_if09_scalar", "test_010_if09_generated");
		packetizer.addFunction("test_011_if10_scalar", "test_011_if10_generated");
		packetizer.addFunction("test_012_if11_scalar", "test_012_if11_generated");
		packetizer.addFunction("test_013_if12_scalar", "test_013_if12_generated");
		// simple loops
		packetizer.addFunction("test_014_loop01_scalar", "test_014_loop01_generated");
		packetizer.addFunction("test_015_loop02_scalar", "test_015_loop02_generated");
		packetizer.addFunction("test_016_loop03_scalar", "test_016_loop03_generated");
		packetizer.addFunction("test_017_loop04_scalar", "test_017_loop04_generated");
		packetizer.addFunction("test_018_loop05_scalar", "test_018_loop05_generated");
		packetizer.addFunction("test_019_loop06_scalar", "test_019_loop06_generated");
		// more complex loops
		packetizer.addFunction("test_020_loopc01_scalar", "test_020_loopc01_generated");
		packetizer.addFunction("test_021_loopc02_scalar", "test_021_loopc02_generated");
		packetizer.addFunction("test_022_loopc03_scalar", "test_022_loopc03_generated");
		packetizer.addFunction("test_023_loopc04_scalar", "test_023_loopc04_generated");
		packetizer.addFunction("test_024_loopc05_scalar", "test_024_loopc05_generated");
		packetizer.addFunction("test_025_loopc06_scalar", "test_025_loopc06_generated");
		packetizer.addFunction("test_026_loopc07_scalar", "test_026_loopc07_generated");
		packetizer.addFunction("test_027_loopc08_scalar", "test_027_loopc08_generated");
		packetizer.addFunction("test_028_loopc09_scalar", "test_028_loopc09_generated");
		// loops with multiple exits
		packetizer.addFunction("test_029_loopmx01_scalar", "test_029_loopmx01_generated");
		packetizer.addFunction("test_030_loopmx02_scalar", "test_030_loopmx02_generated");
		packetizer.addFunction("test_031_loopmx03_scalar", "test_031_loopmx03_generated");
		packetizer.addFunction("test_032_loopmx04_scalar", "test_032_loopmx04_generated");
		packetizer.addFunction("test_033_loopmx05_scalar", "test_033_loopmx05_generated");
		packetizer.addFunction("test_034_loopmx06_scalar", "test_034_loopmx06_generated");
		packetizer.addFunction("test_035_loopmx07_scalar", "test_035_loopmx07_generated");
		packetizer.addFunction("test_036_loopmx08_scalar", "test_036_loopmx08_generated");
		packetizer.addFunction("test_037_loopmx09_scalar", "test_037_loopmx09_generated");
		packetizer.addFunction("test_038_loopmx10_scalar", "test_038_loopmx10_generated");
		packetizer.addFunction("test_039_loopmx11_scalar", "test_039_loopmx11_generated");
		packetizer.addFunction("test_040_loopmx12_scalar", "test_040_loopmx12_generated");
		packetizer.addFunction("test_041_loopmx13_scalar", "test_041_loopmx13_generated");
		// nested loops
		packetizer.addFunction("test_042_loopns01_scalar", "test_042_loopns01_generated");
		packetizer.addFunction("test_043_loopns02_scalar", "test_043_loopns02_generated");
		packetizer.addFunction("test_044_loopns03_scalar", "test_044_loopns03_generated");
		packetizer.addFunction("test_045_loopns04_scalar", "test_045_loopns04_generated");
		packetizer.addFunction("test_046_loopns05_scalar", "test_046_loopns05_generated");
		packetizer.addFunction("test_047_loopns06_scalar", "test_047_loopns06_generated");
		packetizer.addFunction("test_048_loopns07_scalar", "test_048_loopns07_generated");
		packetizer.addFunction("test_049_loopns08_scalar", "test_049_loopns08_generated");
		packetizer.addFunction("test_050_loopns09_scalar", "test_050_loopns09_generated");
		packetizer.addFunction("test_051_loopns10_scalar", "test_051_loopns10_generated");
		packetizer.addFunction("test_052_loopns11_scalar", "test_052_loopns11_generated");
		packetizer.addFunction("test_053_loopns12_scalar", "test_053_loopns12_generated");
		packetizer.addFunction("test_054_loopns13_scalar", "test_054_loopns13_generated");
		packetizer.addFunction("test_055_loopns14_scalar", "test_055_loopns14_generated");
		packetizer.addFunction("test_056_loopns15_scalar", "test_056_loopns15_generated");
		// nested loops with multiple exits
		packetizer.addFunction("test_057_loopnsmx01_scalar", "test_057_loopnsmx01_generated");
		packetizer.addFunction("test_058_loopnsmx02_scalar", "test_058_loopnsmx02_generated");
		packetizer.addFunction("test_059_loopnsmx03_scalar", "test_059_loopnsmx03_generated");
		packetizer.addFunction("test_060_loopnsmx04_scalar", "test_060_loopnsmx04_generated");
		packetizer.addFunction("test_061_loopnsmx05_scalar", "test_061_loopnsmx05_generated");
		packetizer.addFunction("test_062_loopnsmx06_scalar", "test_062_loopnsmx06_generated");
		packetizer.addFunction("test_063_loopnsmx07_scalar", "test_063_loopnsmx07_generated");
		packetizer.addFunction("test_064_loopnsmx08_scalar", "test_064_loopnsmx08_generated");
		packetizer.addFunction("test_065_loopnsmx09_scalar", "test_065_loopnsmx09_generated");
		packetizer.addFunction("test_066_loopnsmx10_scalar", "test_066_loopnsmx10_generated");
		packetizer.addFunction("test_067_loopnsmx11_scalar", "test_067_loopnsmx11_generated");
		packetizer.addFunction("test_068_loopnsmx12_scalar", "test_068_loopnsmx12_generated");
		packetizer.addFunction("test_069_loopnsmx13_scalar", "test_069_loopnsmx13_generated");
		packetizer.addFunction("test_070_loopnsmx14_scalar", "test_070_loopnsmx14_generated");
		packetizer.addFunction("test_071_loopnsmx15_scalar", "test_071_loopnsmx15_generated");

		// function calls
		packetizer.addFunction("test_072_call01_scalar", "test_072_call01_generated");
		packetizer.addFunction("test_073_call02_scalar", "test_073_call02_generated");
		packetizer.addFunction("test_074_call03_scalar", "test_074_call03_generated");
		packetizer.addFunction("test_075_call04_scalar", "test_075_call04_generated");
		packetizer.addFunction("test_076_call05_scalar", "test_076_call05_generated");
		packetizer.addFunction("test_077_call06_scalar", "test_077_call06_generated");
		packetizer.addFunction("test_078_call07_scalar", "test_078_call07_generated");
		packetizer.addFunction("test_079_call08_scalar", "test_079_call08_generated");
		packetizer.addFunction("test_080_call09_scalar", "test_080_call09_generated");
		packetizer.addFunction("test_081_call10_scalar", "test_081_call10_generated");
		// add "native" functions for call9/call10
		llvm::Function* noinlinecall2_4fn = LLVMWrapper::getFunction("noinlinecall2_4", module);
		llvm::Function* noinlinecall3_4fn = LLVMWrapper::getFunction("noinlinecall3_4", module);
		if (noinlinecall2_4fn) packetizer.addVaryingFunctionMapping("noinlinecall2", -1, noinlinecall2_4fn);
		else std::cout << "WARNING: native function 'noinlinecall2_4' not found!\n";
		if (noinlinecall3_4fn) packetizer.addVaryingFunctionMapping("noinlinecall3", 1, noinlinecall3_4fn);
		else std::cout << "WARNING: native function 'noinlinecall3_4' not found!\n";

		// misc tests
		packetizer.addFunction("test_082_misc_scalar", "test_082_misc_generated");
		packetizer.addFunction("test_083_ocl_mandelbrot_scalar", "test_083_ocl_mandelbrot_generated");
		packetizer.addFunction("test_084_noise_scalar", "test_084_noise_generated");
		packetizer.addFunction("test_085_ocl_aobench_scalar", "test_085_ocl_aobench_generated");

		// tests for irreducible control flow
		packetizer.addFunction("test_086_irreducible1_scalar", "test_086_irreducible1_generated");
		packetizer.addFunction("test_087_irreducible2_scalar", "test_087_irreducible2_generated");
		packetizer.addFunction("test_088_irreducible3_scalar", "test_088_irreducible3_generated");

	} else if (testSuiteIdx == 2) {

		//
		// test suite 2
		//
		packetizer.addFunction("test01", "test01_pkt");
		packetizer.addFunction("test02", "test02_pkt");
		packetizer.addFunction("test03", "test03_pkt");
		packetizer.addFunction("test04", "test04_pkt");
		packetizer.addFunction("test05", "test05_pkt");
		packetizer.addFunction("test06", "test06_pkt");
		packetizer.addFunction("test07", "test07_pkt");
		packetizer.addFunction("test08", "test08_pkt");

		packetizer.addFunction("test_array_load01", "test_array_load01_pkt");
		packetizer.addFunction("test_array_load02", "test_array_load02_pkt");
		packetizer.addFunction("test_array_load03", "test_array_load03_pkt");
		packetizer.addFunction("test_array_load04", "test_array_load04_pkt");
		packetizer.addFunction("test_array_load05", "test_array_load05_pkt");
		packetizer.addFunction("test_array_load06", "test_array_load06_pkt");
		packetizer.addFunction("test_array_load07", "test_array_load07_pkt");
		packetizer.addFunction("test_array_cload01", "test_array_cload01_pkt");
		packetizer.addFunction("test_array_cload02", "test_array_cload02_pkt");
		packetizer.addFunction("test_array_cload03", "test_array_cload03_pkt");
		packetizer.addFunction("test_array_cload04", "test_array_cload04_pkt");
		packetizer.addFunction("test_array_cload05", "test_array_cload05_pkt");
		packetizer.addFunction("test_array_cload06", "test_array_cload06_pkt");
		packetizer.addFunction("test_array_cload07", "test_array_cload07_pkt");
		packetizer.addFunction("test_array_cload08", "test_array_cload08_pkt");
		packetizer.addFunction("test_array_cload09", "test_array_cload09_pkt");
		packetizer.addFunction("test_array_cload10", "test_array_cload10_pkt");
		packetizer.addFunction("test_array_cload11", "test_array_cload11_pkt");
		packetizer.addFunction("test_array_cload12", "test_array_cload12_pkt");

		packetizer.addFunction("test_array_store01", "test_array_store01_pkt");
		packetizer.addFunction("test_array_store02", "test_array_store02_pkt");
		packetizer.addFunction("test_array_store03", "test_array_store03_pkt");
		packetizer.addFunction("test_array_store04", "test_array_store04_pkt");
		packetizer.addFunction("test_array_store05", "test_array_store05_pkt");
		packetizer.addFunction("test_array_store06", "test_array_store06_pkt");
		packetizer.addFunction("test_array_store07", "test_array_store07_pkt");
		packetizer.addFunction("test_array_store08", "test_array_store08_pkt");
		packetizer.addFunction("test_array_store09", "test_array_store09_pkt");
		packetizer.addFunction("test_array_store10", "test_array_store10_pkt");
		packetizer.addFunction("test_array_cstore01", "test_array_cstore01_pkt");
		packetizer.addFunction("test_array_cstore02", "test_array_cstore02_pkt");
		packetizer.addFunction("test_array_cstore03", "test_array_cstore03_pkt");
		packetizer.addFunction("test_array_cstore04", "test_array_cstore04_pkt");
		packetizer.addFunction("test_array_cstore05", "test_array_cstore05_pkt");
		packetizer.addFunction("test_array_cstore06", "test_array_cstore06_pkt");
		packetizer.addFunction("test_array_cstore07", "test_array_cstore07_pkt");
		packetizer.addFunction("test_array_cstore08", "test_array_cstore08_pkt");
		packetizer.addFunction("test_array_cstore09", "test_array_cstore09_pkt");
		packetizer.addFunction("test_array_cstore10", "test_array_cstore10_pkt");
		packetizer.addFunction("test_array_cstore11", "test_array_cstore11_pkt");
		packetizer.addFunction("test_array_cstore12", "test_array_cstore12_pkt");
		packetizer.addFunction("test_array_cstore13", "test_array_cstore13_pkt");
		packetizer.addFunction("test_array_cstore14", "test_array_cstore14_pkt");
		packetizer.addFunction("test_array_cstore15", "test_array_cstore15_pkt");
		packetizer.addFunction("test_array_cstore16", "test_array_cstore16_pkt");
		packetizer.addFunction("test_array_cstore17", "test_array_cstore17_pkt");
		packetizer.addFunction("test_array_cstore18", "test_array_cstore18_pkt");
		packetizer.addFunction("test_array_cstore19", "test_array_cstore19_pkt");
		packetizer.addFunction("test_array_cstore20", "test_array_cstore20_pkt");
		packetizer.addFunction("test_array_cstore21", "test_array_cstore21_pkt");
		packetizer.addFunction("test_array_cstore22", "test_array_cstore22_pkt");
		packetizer.addFunction("test_array_cstore23", "test_array_cstore23_pkt");
		packetizer.addFunction("test_array_cstore24", "test_array_cstore24_pkt");

		packetizer.addFunction("test_array_extra01", "test_array_extra01_pkt");
		packetizer.addFunction("test_array_extra02", "test_array_extra02_pkt");
		packetizer.addFunction("test_array_extra03", "test_array_extra03_pkt");
		packetizer.addFunction("test_array_extra04", "test_array_extra04_pkt");
		packetizer.addFunction("test_array_extra05", "test_array_extra05_pkt");
		packetizer.addFunction("test_array_extra06", "test_array_extra06_pkt");

		packetizer.addFunction("test_struct_load01", "test_struct_load01_pkt");
		packetizer.addFunction("test_struct_load02", "test_struct_load02_pkt");
		packetizer.addFunction("test_struct_load03", "test_struct_load03_pkt");
		packetizer.addFunction("test_struct_load04", "test_struct_load04_pkt");
		packetizer.addFunction("test_struct_load05", "test_struct_load05_pkt");
		packetizer.addFunction("test_struct_load06", "test_struct_load06_pkt");
		packetizer.addFunction("test_struct_cload01", "test_struct_cload01_pkt");
		packetizer.addFunction("test_struct_cload02", "test_struct_cload02_pkt");
		packetizer.addFunction("test_struct_cload03", "test_struct_cload03_pkt");
		packetizer.addFunction("test_struct_cload04", "test_struct_cload04_pkt");
		packetizer.addFunction("test_struct_cload05", "test_struct_cload05_pkt");
		packetizer.addFunction("test_struct_cload06", "test_struct_cload06_pkt");
		packetizer.addFunction("test_struct_cload07", "test_struct_cload07_pkt");
		packetizer.addFunction("test_struct_cload08", "test_struct_cload08_pkt");
		packetizer.addFunction("test_struct_cload09", "test_struct_cload09_pkt");
		packetizer.addFunction("test_struct_cload10", "test_struct_cload10_pkt");
		packetizer.addFunction("test_struct_cload11", "test_struct_cload11_pkt");
		packetizer.addFunction("test_struct_cload12", "test_struct_cload12_pkt");

		packetizer.addFunction("test_struct_store01", "test_struct_store01_pkt");
		packetizer.addFunction("test_struct_store02", "test_struct_store02_pkt");
		packetizer.addFunction("test_struct_store03", "test_struct_store03_pkt");
		packetizer.addFunction("test_struct_store04", "test_struct_store04_pkt");
		packetizer.addFunction("test_struct_store05", "test_struct_store05_pkt");
		packetizer.addFunction("test_struct_store06", "test_struct_store06_pkt");
		packetizer.addFunction("test_struct_store07", "test_struct_store07_pkt");
		packetizer.addFunction("test_struct_store08", "test_struct_store08_pkt");
		packetizer.addFunction("test_struct_store09", "test_struct_store09_pkt");
		packetizer.addFunction("test_struct_store10", "test_struct_store10_pkt");
		packetizer.addFunction("test_struct_store11", "test_struct_store11_pkt");
		packetizer.addFunction("test_struct_store12", "test_struct_store12_pkt");
		packetizer.addFunction("test_struct_cstore01", "test_struct_cstore01_pkt");
		packetizer.addFunction("test_struct_cstore02", "test_struct_cstore02_pkt");
		packetizer.addFunction("test_struct_cstore03", "test_struct_cstore03_pkt");
		packetizer.addFunction("test_struct_cstore04", "test_struct_cstore04_pkt");
		packetizer.addFunction("test_struct_cstore05", "test_struct_cstore05_pkt");
		packetizer.addFunction("test_struct_cstore06", "test_struct_cstore06_pkt");
		packetizer.addFunction("test_struct_cstore07", "test_struct_cstore07_pkt");
		packetizer.addFunction("test_struct_cstore08", "test_struct_cstore08_pkt");
		packetizer.addFunction("test_struct_cstore09", "test_struct_cstore09_pkt");
		packetizer.addFunction("test_struct_cstore10", "test_struct_cstore10_pkt");
		packetizer.addFunction("test_struct_cstore11", "test_struct_cstore11_pkt");
		packetizer.addFunction("test_struct_cstore12", "test_struct_cstore12_pkt");
		packetizer.addFunction("test_struct_cstore13", "test_struct_cstore13_pkt");
		packetizer.addFunction("test_struct_cstore14", "test_struct_cstore14_pkt");
		packetizer.addFunction("test_struct_cstore15", "test_struct_cstore15_pkt");
		packetizer.addFunction("test_struct_cstore16", "test_struct_cstore16_pkt");
		packetizer.addFunction("test_struct_cstore17", "test_struct_cstore17_pkt");
		packetizer.addFunction("test_struct_cstore18", "test_struct_cstore18_pkt");
		packetizer.addFunction("test_struct_cstore19", "test_struct_cstore19_pkt");
		packetizer.addFunction("test_struct_cstore20", "test_struct_cstore20_pkt");
		packetizer.addFunction("test_struct_cstore21", "test_struct_cstore21_pkt");
		packetizer.addFunction("test_struct_cstore22", "test_struct_cstore22_pkt");
		packetizer.addFunction("test_struct_cstore23", "test_struct_cstore23_pkt");
		packetizer.addFunction("test_struct_cstore24", "test_struct_cstore24_pkt");

		packetizer.addFunction("test_struct_extra01", "test_struct_extra01_pkt");
		packetizer.addFunction("test_struct_extra02", "test_struct_extra02_pkt");
		packetizer.addFunction("test_struct_extra03", "test_struct_extra03_pkt");
		packetizer.addFunction("test_struct_extra04", "test_struct_extra04_pkt");
		packetizer.addFunction("test_struct_extra05", "test_struct_extra05_pkt");
		packetizer.addFunction("test_struct_extra06", "test_struct_extra06_pkt");
		packetizer.addFunction("test_struct_extra07", "test_struct_extra07_pkt");
		packetizer.addFunction("test_struct_extra08", "test_struct_extra08_pkt");
		packetizer.addFunction("test_struct_extra09", "test_struct_extra09_pkt");
		packetizer.addFunction("test_struct_extra10", "test_struct_extra10_pkt");

	} else if (testSuiteIdx == 3) {

		//
		// test suite 3
		//
#define ADD_NATIVE_FN(name, nameStr, uniform, consecutive, aligned)                \
		llvm::Function* name = LLVMWrapper::getFunction(nameStr, module);      \
		if (name) packetizer.addValueInfo(name, uniform, consecutive, aligned);    \
		else std::cout << "WARNING: native function '" << nameStr << "' not found!\n"

		ADD_NATIVE_FN(get_global_id_0, "get_global_id_0", false, true, true);
		ADD_NATIVE_FN(get_global_id_1, "get_global_id_1", true, false, false);
		ADD_NATIVE_FN(get_local_id_0, "get_local_id_0", false, true, true);
		ADD_NATIVE_FN(get_local_id_1, "get_local_id_1", true, false, false);
		ADD_NATIVE_FN(get_group_id_0, "get_group_id_0", true, false, false);
		ADD_NATIVE_FN(get_group_id_1, "get_group_id_1", true, false, false);
		ADD_NATIVE_FN(get_global_size_0, "get_global_size_0", true, false, false);
		ADD_NATIVE_FN(get_global_size_1, "get_global_size_1", true, false, false);
		ADD_NATIVE_FN(get_local_size_0, "get_local_size_0", true, false, false);
		ADD_NATIVE_FN(get_local_size_1, "get_local_size_1", true, false, false);

		packetizer.addFunction("test_opencl1_scalar", "test_opencl1_generated");
		packetizer.addFunction("test_opencl2_scalar", "test_opencl2_generated");
		packetizer.addFunction("test_opencl3_scalar", "test_opencl3_generated");
		packetizer.addFunction("test_opencl4_scalar", "test_opencl4_generated");
		packetizer.addFunction("test_opencl5_scalar", "test_opencl5_generated");
		packetizer.addFunction("test_bitonicsort_scalar", "test_bitonicsort_generated");
		packetizer.addFunction("test_blackscholes_scalar", "test_blackscholes_generated");
		packetizer.addFunction("test_fastwalshtransform_scalar", "test_fastwalshtransform_generated");
		packetizer.addFunction("test_mandelbrot_scalar", "test_mandelbrot_generated");
		packetizer.addFunction("test_nbody_scalar", "test_nbody_generated");

	}

	packetizer.run();

	///////////////////////////////////////////////////////////////////////////
	//                    4. run optimization pass(es)                       //
	///////////////////////////////////////////////////////////////////////////

	llvm::Function* mainFn = LLVMWrapper::getFunction("main", module);

	if (!do_not_optimize) {
		// Doing this might not be a good idea... we want to focus on the
		// correctness of the vectorized stuff and should not care about the
		// runtime of the test suite.
		//std::cout << "\noptimizing test suite main() function... ";
		//LLVMWrapper::optimizeFunction(mainFn);
		//std::cout << "done.\n";
	}

	///////////////////////////////////////////////////////////////////////////
	//                         5. verify module                              //
	///////////////////////////////////////////////////////////////////////////

	if (verify) {
		std::cout << "verifying bitcode of new module... ";
		LLVMWrapper::verifyModule(module);
		std::cout << "done.\n";
	}

	///////////////////////////////////////////////////////////////////////////
	//                          6. dump bytecode                             //
	///////////////////////////////////////////////////////////////////////////

	if (dump) {
		std::cout << "writing module to file... ";
		LLVMWrapper::writeModuleToFile(module, "testModule.pkt.ll");
		std::cout << "done.\n";
	}

	///////////////////////////////////////////////////////////////////////////
	//                        7. compile main module                         //
	///////////////////////////////////////////////////////////////////////////

	// Compile and store function pointer to 'main()'.
	// This takes ownership of the module
	llvm::ExecutionEngine* engine = LLVMWrapper::createExecutionEngine(module, use_avx);
	std::cout << "JIT compiling test suite... ";
	void* mainPtr = LLVMWrapper::getPointerToFunction(mainFn, engine);
	if (!mainPtr) {
		std::cout << "FAILED!\n";
		return -1;
	} else {
		std::cout << "done.\n";
	}

	///////////////////////////////////////////////////////////////////////////
	//                         8. execute function                           //
	///////////////////////////////////////////////////////////////////////////

	LLVMWrapper::executeMain(mainPtr, argc, argv);

	return 0;
}

