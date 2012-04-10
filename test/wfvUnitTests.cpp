/**
 * @file   wfvUnitTests.cpp
 * @date   31.10.2011
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2011 Saarland University
 *
 */
#include <stdio.h>
//#include <ctime>    // For time()
//#include <cstdlib>  // For srand() and rand()

#include <gtest/gtest.h>

#include "packetizerAPI.hpp"
#include "../tools/llvmWrapper.hpp" // createModuleFromFile

// global configuration variables
static unsigned simdWidth;
static unsigned packetizationSize;
static bool use_sse41;
static bool use_avx;
static bool verbose;
static bool verify;

// Macro helper
#define WFV_SETUP_TEST(filename) \
	Module* module                   = LLVMWrapper::createModuleFromFile(filename); \
	EXPECT_FALSE(module == NULL); \
	if (!module) return; \
	Packetizer::Packetizer packetizer(*module, simdWidth, packetizationSize, use_sse41, use_avx, verbose)

using namespace llvm;

namespace {

// The fixture for testing class Foo.
class FooTest : public ::testing::Test {
protected:
	// You can remove any or all of the following functions if its body
	// is empty.

	FooTest() {
		// You can do set-up work for each test here.
	}

	virtual ~FooTest() {
		// You can do clean-up work that doesn't throw exceptions here.
	}

	// If the constructor and destructor are not enough for setting up
	// and cleaning up each test, you can define the following methods:

	virtual void SetUp() {
		// Code here will be called immediately after the constructor (right
		// before each test).
		
	}

	virtual void TearDown() {
		// Code here will be called immediately after each test (right
		// before the destructor).
	}

	// Objects declared here can be used by all tests in the test case for Foo.
};

///////////////////////////////////////////////////////////////////////////////
// Failure tests with bad signatures
///////////////////////////////////////////////////////////////////////////////

TEST_F(FooTest, TestVA01) {
	WFV_SETUP_TEST("test/unittests/va/test_va_01.bc");

	packetizer.analyzeFunction("test", "test_SIMD");

	Function* f = module->getFunction("test");
	EXPECT_EQ(packetizer.isFullyNonDivergent(&f->getEntryBlock()), true);
}



TEST_F(FooTest, TestAPI_signature_failure_01) {
	WFV_SETUP_TEST("test/unittests/api/test_api_signatures.bc");

	packetizer.addFunction("test1", "test1_SIMD");
	packetizer.run();

	EXPECT_FALSE(packetizer.wfvSuccessful("test1_SIMD", *module));
}

TEST_F(FooTest, TestAPI_signature_failure_02) {
	WFV_SETUP_TEST("test/unittests/api/test_api_signatures.bc");

	packetizer.addFunction("test2", "test2_SIMD");
	packetizer.run();

	EXPECT_FALSE(packetizer.wfvSuccessful("test2_SIMD", *module));
}

TEST_F(FooTest, TestAPI_signature_failure_03) {
	WFV_SETUP_TEST("test/unittests/api/test_api_signatures.bc");

	packetizer.addFunction("test3", "test3_SIMD");
	packetizer.run();

	EXPECT_FALSE(packetizer.wfvSuccessful("test3_SIMD", *module));
}

TEST_F(FooTest, TestAPI_signature_failure_04) {
	WFV_SETUP_TEST("test/unittests/api/test_api_signatures.bc");

	packetizer.addFunction("test4", "test4_SIMD");
	packetizer.run();

	EXPECT_FALSE(packetizer.wfvSuccessful("test4_SIMD", *module));
}

TEST_F(FooTest, TestAPI_signature_failure_05) {
	WFV_SETUP_TEST("test/unittests/api/test_api_signatures.bc");

	packetizer.addFunction("test5", "test5_SIMD");
	packetizer.run();

	EXPECT_FALSE(packetizer.wfvSuccessful("test5_SIMD", *module));
}

TEST_F(FooTest, TestAPI_signature_failure_06) {
	WFV_SETUP_TEST("test/unittests/api/test_api_signatures.bc");

	packetizer.addFunction("test6", "test6_SIMD");
	packetizer.run();

	EXPECT_FALSE(packetizer.wfvSuccessful("test6_SIMD", *module));
}

TEST_F(FooTest, TestAPI_signature_failure_07) {
	WFV_SETUP_TEST("test/unittests/api/test_api_signatures.bc");

	packetizer.addFunction("test7", "test7_SIMD");
	packetizer.run();

	EXPECT_FALSE(packetizer.wfvSuccessful("test7_SIMD", *module));
}

// We currently cannot test this, so this test fails expectedly
// (see test_api_signatures.ll).
TEST_F(FooTest, TestAPI_signature_failure_08) {
	//WFV_SETUP_TEST("test/unittests/api/test_api_signatures.bc");

	//packetizer.addFunction("test8", "test8_SIMD");
	//packetizer.run();

	//EXPECT_FALSE(packetizer.wfvSuccessful("test8_SIMD", *module));
	EXPECT_TRUE(false && "expected failure");
}

///////////////////////////////////////////////////////////////////////////////
// Failure tests with "bad" code
///////////////////////////////////////////////////////////////////////////////

// We currently still have optimizations enabled, so this test
// fails expectedly.
TEST_F(FooTest, TestAPI_deadcode_failure) {
	//WFV_SETUP_TEST("test/unittests/api/test_api_badinsts.bc");

	//packetizer.addFunction("test_deadcode", "test_deadcode_SIMD");
	//packetizer.run();

	//EXPECT_FALSE(packetizer.wfvSuccessful("test_deadcode_SIMD", *module));
	EXPECT_TRUE(false && "expected failure");
}

TEST_F(FooTest, TestAPI_fpext_failure) {
	WFV_SETUP_TEST("test/unittests/api/test_api_badinsts.bc");

	packetizer.addFunction("test_fpext", "test_fpext_SIMD");
	packetizer.run();

	EXPECT_FALSE(packetizer.wfvSuccessful("test_fpext_SIMD", *module));
}

TEST_F(FooTest, TestAPI_sext_failure) {
	WFV_SETUP_TEST("test/unittests/api/test_api_badinsts.bc");

	packetizer.addFunction("test_sext", "test_sext_SIMD");
	packetizer.run();

	EXPECT_FALSE(packetizer.wfvSuccessful("test_sext_SIMD", *module));
}

TEST_F(FooTest, TestAPI_zext_failure) {
	WFV_SETUP_TEST("test/unittests/api/test_api_badinsts.bc");

	packetizer.addFunction("test_zext", "test_zext_SIMD");
	packetizer.run();

	EXPECT_FALSE(packetizer.wfvSuccessful("test_zext_SIMD", *module));
}


///////////////////////////////////////////////////////////////////////////////
// Tests with valid code
///////////////////////////////////////////////////////////////////////////////


TEST_F(FooTest, TestAPI_simple) {
	WFV_SETUP_TEST("test/unittests/api/test_api_valid.bc");

	packetizer.addFunction("test_int", "test_int_SIMD");
	packetizer.run();

	EXPECT_TRUE(packetizer.wfvSuccessful("test_int_SIMD", *module));
}

TEST_F(FooTest, TestAPI_simple2) {
	WFV_SETUP_TEST("test/unittests/api/test_api_valid.bc");

	packetizer.addFunction("test_float", "test_float_SIMD");
	packetizer.run();

	EXPECT_TRUE(packetizer.wfvSuccessful("test_float_SIMD", *module));
}

TEST_F(FooTest, TestAPI_uniform) {
	WFV_SETUP_TEST("test/unittests/api/test_api_valid.bc");

	packetizer.addFunction("test_uniform", "test_uniform_SIMD");
	packetizer.run();

	EXPECT_TRUE(packetizer.wfvSuccessful("test_uniform_SIMD", *module));
}

TEST_F(FooTest, TestAPI_uniform2) {
	WFV_SETUP_TEST("test/unittests/api/test_api_valid.bc");

	packetizer.addFunction("test_uniform2", "test_uniform2_SIMD");
	packetizer.run();

	EXPECT_TRUE(packetizer.wfvSuccessful("test_uniform2_SIMD", *module));
}

TEST_F(FooTest, TestAPI_uniform3) {
	WFV_SETUP_TEST("test/unittests/api/test_api_valid.bc");

	packetizer.addFunction("test_uniform3", "test_uniform3_SIMD");
	packetizer.run();

	EXPECT_TRUE(packetizer.wfvSuccessful("test_uniform3_SIMD", *module));
}

TEST_F(FooTest, TestAPI_equivalent_vectype) {
	WFV_SETUP_TEST("test/unittests/api/test_api_valid.bc");

	packetizer.addFunction("test_equivalent_vectype", "test_equivalent_vectype_SIMD");
	packetizer.run();

	EXPECT_TRUE(packetizer.wfvSuccessful("test_equivalent_vectype_SIMD", *module));
}

TEST_F(FooTest, TestAPI_equivalent_vectype2) {
	WFV_SETUP_TEST("test/unittests/api/test_api_valid.bc");

	packetizer.addFunction("test_equivalent_vectype2", "test_equivalent_vectype2_SIMD");
	packetizer.run();

	EXPECT_TRUE(packetizer.wfvSuccessful("test_equivalent_vectype2_SIMD", *module));
}

}  // namespace



int main(int argc, char** argv) {

	// print copyright information
	std::cout << "\n\
********************************************************************************\n\
*     Whole-Function Vectorization Unit Tests                                  *\n\
*                                                                              *\n\
*     This program employs googletest to perform unit tests.                   *\n\
*                                                                              *\n\
*     Copyright (C) 2011 Saarland University                                   *\n\
*                                                                              *\n\
*     This file is distributed under the University of Illinois Open Source    *\n\
*     License. See the COPYING file in the root directory for details.         *\n\
*                                                                              *\n\
********************************************************************************\n\
		\n";

	///////////////////////////////////////////////////////////////////////////
	//                           read arguments                              //
	///////////////////////////////////////////////////////////////////////////

	simdWidth         = 4;
	packetizationSize = 4;
	verbose           = false;
	use_sse41         = true;
	use_avx           = false;
	verify            = true;

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
		if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "-help") == 0) {
			displayUsage = true;
			continue;
		}
		if (strcmp(argv[i], "-no-verify") == 0) {
			verify = false;
			continue;
		}

		std::cerr << "ERROR: Unknown argument found: " << argv[i] << "\n\n";
		displayUsage = true;
		error = true;
	}

	if (displayUsage) {
		std::cerr << "Usage: " << argv[0] << " [options]\n";
		std::cerr << "Options:\n";
		std::cerr << "  -no-sse41   : disable generation of SSE4.1 intrinsics\n";
		std::cerr << "  -mavx       : ensable generation of AVX intrinsics\n";
		std::cerr << "  -no-avx     : disable generation of AVX intrinsics\n";
		std::cerr << "  -v -verbose : enable verbose output (only valid for debug builds)\n";
		std::cerr << "  -h -help    : display this help message\n\n";
		return error ? -1 : 0;
	}

    printf("\n\n\n--------------------------------------------------------------------------------\n");
    printf("running test-suite for automatic packetization...\n\n");

	::testing::InitGoogleTest(&argc, argv);
	const int testResult = RUN_ALL_TESTS();

    printf("\n\ntest-suite run complete!\n");
    printf("--------------------------------------------------------------------------------\n\n");


    return testResult;
}

