/**
 * @file   wfvTests3.cpp
 * @date   08.07.2011
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2011 Saarland University
 *
 * This test suite resembles the usage of Whole-Function Vectorization in a
 * data-parallel language such as OpenCL.
 * All test cases have only uniform arguments, but access these with varying
 * indices from functions like "get_global_id()". These functions have to be
 * marked as "varying" by the driver (packetizerTestSuite.cpp) by calling
 * Packetizer::addUniformVaryingInfo().
 *
 */
#include <stdio.h>
#include <math.h>
#include <vector>
#include <ctime>
#include <cstdlib>  // For srand() and rand()

#define SIMD_WIDTH 4U

#define NUM_INPUTS_SQRT 16U
#define NUM_INPUTS (NUM_INPUTS_SQRT * NUM_INPUTS_SQRT)
#define GLOBAL_SIZE_0 NUM_INPUTS_SQRT
#define GLOBAL_SIZE_1 NUM_INPUTS_SQRT
#define LOCAL_SIZE_0 (NUM_INPUTS_SQRT / 4)
#define LOCAL_SIZE_1 (NUM_INPUTS_SQRT / 4)
//#define GLOBAL_SIZE_0_SIMD (GLOBAL_SIZE_0 / SIMD_WIDTH)
//#define LOCAL_SIZE_0_SIMD (LOCAL_SIZE_0 / SIMD_WIDTH)

// due to imprecisions etc. we apply a few rules
// when to treat results as equal.
// - float == float -> equality of first 5 decimal places
// - NaN == NaN -> true ;)
bool resultMatches(const float a, const float b) {
	return a == b ||
		(isnan(a) && isnan(b)) ||
		(abs(a-b) < 0.000001);
}
bool resultMatches(const int a, const int b) {
	return a == b ||
		(abs(a-b) < 0.000001);
}

//
// new tests based on addUniformVaryingInfo()
//

static unsigned global_id_0;
static unsigned global_id_1;
static unsigned local_id_0;
static unsigned local_id_1;
static unsigned group_id_0;
static unsigned group_id_1;

// to be marked VARYING
extern "C" __attribute__((noinline)) unsigned get_global_id_0() {
	return global_id_0;
}
// to be marked VARYING
extern "C" __attribute__((noinline)) unsigned get_local_id_0() {
	return local_id_0;
}
// to be marked UNIFORM
extern "C" __attribute__((noinline)) unsigned get_global_id_1() {
	return global_id_1;
}
// to be marked UNIFORM
extern "C" __attribute__((noinline)) unsigned get_local_id_1() {
	return local_id_1;
}
// to be marked UNIFORM
extern "C" __attribute__((noinline)) unsigned get_group_id_0() {
	return group_id_0;
}
// to be marked UNIFORM
extern "C" __attribute__((noinline)) unsigned get_group_id_1() {
	return group_id_1;
}
// to be marked UNIFORM
extern "C" __attribute__((noinline)) unsigned get_global_size_0() {
	return GLOBAL_SIZE_0;
}
// to be marked UNIFORM
extern "C" __attribute__((noinline)) unsigned get_global_size_1() {
	return GLOBAL_SIZE_1;
}
// to be marked UNIFORM
extern "C" __attribute__((noinline)) unsigned get_local_size_0() {
	return LOCAL_SIZE_0;
}
// to be marked UNIFORM
extern "C" __attribute__((noinline)) unsigned get_local_size_1() {
	return LOCAL_SIZE_1;
}

//----------------------------------------------------------------------------//
// Macro definitions for scalar/vectorized functions
//----------------------------------------------------------------------------//

#define SCALAR_FN(name)                                 \
extern "C" __attribute__((noinline))                    \
void name(const float input0[NUM_INPUTS],               \
						const float input1[NUM_INPUTS], \
						const int input2[NUM_INPUTS],   \
						float output0[NUM_INPUTS],      \
						float output1[NUM_INPUTS],      \
						int output2[NUM_INPUTS],        \
						const float input3,             \
						const int input4)

#define VECTOR_FN(name)                   \
extern "C"                                \
void name(const float input0[NUM_INPUTS], \
		  const float input1[NUM_INPUTS], \
		  const int input2[NUM_INPUTS],   \
		  float output0[NUM_INPUTS],      \
		  float output1[NUM_INPUTS],      \
		  int output2[NUM_INPUTS],        \
		  const float input3,             \
		  const int input4)

//----------------------------------------------------------------------------//
// declarations of empty prototypes of packetized functions
//----------------------------------------------------------------------------//

VECTOR_FN(test_opencl1_generated);
VECTOR_FN(test_opencl2_generated);
VECTOR_FN(test_opencl3_generated);
VECTOR_FN(test_opencl4_generated);
VECTOR_FN(test_opencl5_generated);
VECTOR_FN(test_bitonicsort_generated);
VECTOR_FN(test_blackscholes_generated);
VECTOR_FN(test_fastwalshtransform_generated);
VECTOR_FN(test_mandelbrot_generated);
VECTOR_FN(test_nbody_generated);

//----------------------------------------------------------------------------//
// implementations of corresponding scalar source functions
//----------------------------------------------------------------------------//

// write the same value to the output array at index gid0
// - store of SAME float value to CONSECTUIVE float pointer
SCALAR_FN(test_opencl1_scalar) {
	const unsigned idx = get_global_id_0();
	output0[idx] = input3;
}

// write the index to the output array at index gid0
// - store of CONSECUTIVE integer value to CONSECUTIVE float pointer
// TODO: we broadcast the value one instruction too "early" here.
SCALAR_FN(test_opencl2_scalar) {
	const unsigned idx = get_global_id_0();
	output0[idx] = idx;
}

// write the input array at index gid0 to the output array at index gid0
// - load from CONSECUTIVE float pointer
// - store of RANDOM float value to CONSECUTIVE float pointer
SCALAR_FN(test_opencl3_scalar) {
	const unsigned idx = get_global_id_0();
	output0[idx] = input0[idx];
}

// unused
SCALAR_FN(test_opencl4_scalar) {
	const unsigned idx = get_global_id_0();
	output0[idx] = idx;
}

// unused
SCALAR_FN(test_opencl5_scalar) {
	const unsigned idx = get_global_id_0();
	output0[idx] = idx;
}

SCALAR_FN(test_bitonicsort_scalar) {
	// arguments
	const int* theArray = input2;
	int* theArrayOut = output2; // original kernel reads/writes to same array
	const int direction = input4;
	const int stage = input4;
	const int passOfStage = input4;
	//const int width = input4; // unused

	// kernel
	unsigned sortIncreasing = direction;
    unsigned threadId = get_global_id_0();

    unsigned pairDistance = 1 << (stage - passOfStage);
    unsigned blockWidth   = 2 * pairDistance;

    unsigned leftId = (threadId % pairDistance)
                   + (threadId / pairDistance) * blockWidth;

    unsigned rightId = leftId + pairDistance;

    unsigned leftElement = theArray[leftId];
    unsigned rightElement = theArray[rightId];

    unsigned sameDirectionBlockWidth = 1 << stage;

    if((threadId/sameDirectionBlockWidth) % 2 == 1)
        sortIncreasing = 1 - sortIncreasing;

    unsigned greater;
    unsigned lesser;
    if(leftElement > rightElement)
    {
        greater = leftElement;
        lesser  = rightElement;
    }
    else
    {
        greater = rightElement;
        lesser  = leftElement;
    }

    if(sortIncreasing)
    {
        theArrayOut[leftId]  = lesser;
        theArrayOut[rightId] = greater;
    }
    else
    {
        theArrayOut[leftId]  = greater;
        theArrayOut[rightId] = lesser;
    }
}

// helper for blackscholes
void phi(float X, float* phi)
{
    float y;
    float absX;
    float t;
    float result;

    const float c1 = (float)0.319381530f;
    const float c2 = (float)-0.356563782f;
    const float c3 = (float)1.781477937f;
    const float c4 = (float)-1.821255978f;
    const float c5 = (float)1.330274429f;

    const float zero = (float)0.0f;
    const float one = (float)1.0f;
    const float two = (float)2.0f;
    const float temp4 = (float)0.2316419f;

    const float oneBySqrt2pi = (float)0.398942280f;

    absX = fabsf(X);
    t = one/(one + temp4 * absX);

    y = one - oneBySqrt2pi * expf(-X*X/two) * t
        * (c1 + t
              * (c2 + t
                    * (c3 + t
                          * (c4 + t * c5))));

    result = (X < zero)? (one - y) : y;

    *phi = result;
}

#define S_LOWER_LIMIT 10.0f
#define S_UPPER_LIMIT 100.0f
#define K_LOWER_LIMIT 10.0f
#define K_UPPER_LIMIT 100.0f
#define T_LOWER_LIMIT 1.0f
#define T_UPPER_LIMIT 10.0f
#define R_LOWER_LIMIT 0.01f
#define R_UPPER_LIMIT 0.05f
#define SIGMA_LOWER_LIMIT 0.01f
#define SIGMA_UPPER_LIMIT 0.10f

SCALAR_FN(test_blackscholes_scalar) {
	// arguments
	const float* randArray = input0;
	const int width = input4;
	float* call = output0;
	float* put = output1;

	// kernel
	float d1, d2;
    float phiD1, phiD2;
    float sigmaSqrtT;
    float KexpMinusRT;

    size_t xPos = get_global_id_0();
    size_t yPos = get_global_id_1();
    float two = (float)2.0f;
    //float inRand = randArray[xPos];
    float inRand = randArray[yPos * width + xPos];
    float S = S_LOWER_LIMIT * inRand + S_UPPER_LIMIT * (1.0f - inRand);
    float K = K_LOWER_LIMIT * inRand + K_UPPER_LIMIT * (1.0f - inRand);
    float T = T_LOWER_LIMIT * inRand + T_UPPER_LIMIT * (1.0f - inRand);
    float R = R_LOWER_LIMIT * inRand + R_UPPER_LIMIT * (1.0f - inRand);
    float sigmaVal = SIGMA_LOWER_LIMIT * inRand + SIGMA_UPPER_LIMIT * (1.0f - inRand);


    sigmaSqrtT = sigmaVal * sqrtf(T);

    d1 = (logf(S/K) + (R + sigmaVal * sigmaVal / two)* T)/ sigmaSqrtT;
    d2 = d1 - sigmaSqrtT;

    KexpMinusRT = K * expf(-R * T);
    phi(d1, &phiD1), phi(d2, &phiD2);
    //call[xPos] = S * phiD1 - KexpMinusRT * phiD2;
    call[yPos * width + xPos] = S * phiD1 - KexpMinusRT * phiD2;
    phi(-d1, &phiD1), phi(-d2, &phiD2);
    //put[xPos]  = KexpMinusRT * phiD2 - S * phiD1;
    put[yPos * width + xPos]  = KexpMinusRT * phiD2 - S * phiD1;
}

#undef S_LOWER_LIMIT
#undef S_UPPER_LIMIT
#undef K_LOWER_LIMIT
#undef K_UPPER_LIMIT
#undef T_LOWER_LIMIT
#undef T_UPPER_LIMIT
#undef R_LOWER_LIMIT
#undef R_UPPER_LIMIT
#undef SIGMA_LOWER_LIMIT
#undef SIGMA_UPPER_LIMIT

SCALAR_FN(test_fastwalshtransform_scalar) {
	// arguments
	const float* tArray = input0;
	float* tArrayOut = output0; // original kernel reads/writes to same array
	// step has to be non-negative and non-zero and smaller than some size to prevent writing to bad locations
	const int step = input4 <= 0 ? 4 : abs(input4) > GLOBAL_SIZE_0 ? GLOBAL_SIZE_0 : input4;

	// kernel
	unsigned int tid = get_global_id_0();

	const unsigned int group = tid%step;
	const unsigned int pair  = 2*step*(tid/step) + group;

	const unsigned int match = pair + step;

	float T1          = tArray[pair];
	float T2          = tArray[match];

	tArrayOut[pair]             = T1 + T2;
	tArrayOut[match]            = T1 - T2;
}

SCALAR_FN(test_mandelbrot_scalar) {
	// arguments
	int* mandelbrotImage = output2;
	const float scale = input3;
	// we have to restrict our random arguments a little...
	const unsigned maxIterations = input4 == 0 ? 20 : abs(input4) > 1024 ? 1024 : abs(input4); // should not be too large/small
	const int width = input4 == 0 ? 256 : input4; // has to be non-zero

	// kernel
	int tid = get_global_id_0();

	int i = tid%width;
	int j = tid/width;

	float x0 = ((i*scale) - ((scale/2)*width))/width;
	float y0 = ((j*scale) - ((scale/2)*width))/width;

	float x = x0;
	float y = y0;

	float x2 = x*x;
	float y2 = y*y;

	float scaleSquare = scale * scale;

	unsigned iter=0;
	for(iter=0; (x2+y2 <= scaleSquare) && (iter < maxIterations); ++iter)
	{
		y = 2 * x * y + y0;
		x = x2 - y2   + x0;

		x2 = x*x;
		y2 = y*y;
	}
	mandelbrotImage[tid] = 255*iter/maxIterations;
}

SCALAR_FN(test_nbody_scalar) {
	// arguments
	const float* posX = input0;
	const float* posY = input1;
	const float* posZ = input0;
	const float* posW = input1;
	const float* velX = input0;
	const float* velY = input1;
	const float* velZ = input1;
	const int numBodies = input4;
	const float deltaTime = input3;
	const float epsSqr = input3;
	float lpX[NUM_INPUTS];
	float lpY[NUM_INPUTS];
	float lpZ[NUM_INPUTS];
	float lpW[NUM_INPUTS];
	float* localPosX = lpX;
	float* localPosY = lpY;
	float* localPosZ = lpZ;
	float* localPosW = lpW;
	float* newPositionX = output0;
	float* newPositionY = output1;
	float* newPositionZ = output0;
	float* newPositionW = output1;
	float* newVelocityX = (float*)output2;
	float* newVelocityY = (float*)output2;
	float* newVelocityZ = (float*)output2;

	// kernel
	unsigned int tid = get_local_id_0();
    unsigned int gid = get_global_id_0();
    unsigned int localSize = get_local_size_0();

    // Number of tiles we need to iterate
    unsigned int numTiles = numBodies / localSize;

    // position of this work-item
    float myPosX = posX[gid];
    float myPosY = posY[gid];
    float myPosZ = posZ[gid];
    float myPosW = posW[gid];
    float accX = 0.0f;
    float accY = 0.0f;
    float accZ = 0.0f;

    for(int i = 0; i < numTiles; ++i)
    {
        // load one tile into local memory
        int idx = i * localSize + tid;
        localPosX[tid] = posX[idx];
        localPosY[tid] = posY[idx];
        localPosZ[tid] = posZ[idx];
        localPosW[tid] = posW[idx];

        // Synchronize to make sure data is available for processing
        //barrier(CLK_LOCAL_MEM_FENCE);

        // calculate acceleration effect due to each body
        // a[i->j] = m[j] * r[i->j] / (r^2 + epsSqr)^(3/2)
        for(int j = 0; j < localSize; ++j)
        {
            // Calculate acceleration caused by particle j on particle i
            float rX = localPosX[j] - myPosX;
            float rY = localPosY[j] - myPosY;
            float rZ = localPosZ[j] - myPosZ;
            float distSqr = rX * rX  +  rY * rY  +  rZ * rZ;
            float invDist = 1.0f / sqrtf(distSqr + epsSqr);
            float invDistCube = invDist * invDist * invDist;
            float s = localPosW[j] * invDistCube;

            // accumulate effect of all particles
            accX += s * rX;
            accY += s * rY;
            accZ += s * rZ;
        }

        // Synchronize so that next tile can be loaded
        //barrier(CLK_LOCAL_MEM_FENCE); // not required? (at least scalar, single-threaded mode passes)
    }

    float oldVelX = velX[gid];
    float oldVelY = velY[gid];
    float oldVelZ = velZ[gid];

    // updated position and velocity
    float newPosX = myPosX + oldVelX * deltaTime + accX * 0.5f * deltaTime * deltaTime;
    float newPosY = myPosY + oldVelY * deltaTime + accY * 0.5f * deltaTime * deltaTime;
    float newPosZ = myPosZ + oldVelZ * deltaTime + accZ * 0.5f * deltaTime * deltaTime;
    float newPosW = myPosW;

    float newVelX = oldVelX + accX * deltaTime;
    float newVelY = oldVelY + accY * deltaTime;
    float newVelZ = oldVelZ + accZ * deltaTime;

    // write to global memory
    newPositionX[gid] = newPosX;
    newPositionY[gid] = newPosY;
    newPositionZ[gid] = newPosZ;
    newPositionW[gid] = newPosW;
    newVelocityX[gid] = newVelX;
    newVelocityY[gid] = newVelY;
    newVelocityZ[gid] = newVelZ;
}


//----------------------------------------------------------------------------//
// add test cases to test suite (uncomment / comment blocks to enable/disable)
//----------------------------------------------------------------------------//
typedef void (*fnType)(const float[NUM_INPUTS], const float[NUM_INPUTS], const int[NUM_INPUTS], float[NUM_INPUTS], float[NUM_INPUTS], int[NUM_INPUTS], float, int);

struct TestCase {
	TestCase(const char* _name, fnType _scalarFn, fnType _vectorizedFn)
		: name(_name), scalarFn(_scalarFn), vectorizedFn(_vectorizedFn)
	{}

	const char* name;
	fnType scalarFn;
	fnType vectorizedFn;
};
void addTestCases(std::vector<TestCase*>& testCases) {
#if 0
#endif
	testCases.push_back(new TestCase("test_opencl1_scalar", test_opencl1_scalar, test_opencl1_generated));
	testCases.push_back(new TestCase("test_opencl2_scalar", test_opencl2_scalar, test_opencl2_generated));
	testCases.push_back(new TestCase("test_opencl3_scalar", test_opencl3_scalar, test_opencl3_generated));
	testCases.push_back(new TestCase("test_opencl4_scalar", test_opencl4_scalar, test_opencl4_generated));
	testCases.push_back(new TestCase("test_opencl5_scalar", test_opencl5_scalar, test_opencl5_generated));

//	testCases.push_back(new TestCase("test_bitonicsort_scalar", test_bitonicsort_scalar, test_bitonicsort_generated));
//	testCases.push_back(new TestCase("test_blackscholes_scalar", test_blackscholes_scalar, test_blackscholes_generated));
	testCases.push_back(new TestCase("test_fastwalshtransform_scalar", test_fastwalshtransform_scalar, test_fastwalshtransform_generated));
	testCases.push_back(new TestCase("test_mandelbrot_scalar", test_mandelbrot_scalar, test_mandelbrot_generated));
//	testCases.push_back(new TestCase("test_nbody_scalar", test_nbody_scalar, test_nbody_generated));
}


struct Result {
	Result(const char*  _name,
		   const float* _input0,
		   const float* _input1,
		   const int*   _input2,
		   const float  _input3,
		   const int    _input4,
		   float* _scalarOutput0,
		   float* _scalarOutput1,
		   int*   _scalarOutput2,
		   float* _vectorizedOutput0,
		   float* _vectorizedOutput1,
		   int*   _vectorizedOutput2)
	: name(_name),
	  input0(_input0),
	  input1(_input1),
	  input2(_input2),
	  input3(_input3),
	  input4(_input4),
	  scalarOutput0(_scalarOutput0),
	  scalarOutput1(_scalarOutput1),
	  scalarOutput2(_scalarOutput2),
	  vectorizedOutput0(_vectorizedOutput0),
	  vectorizedOutput1(_vectorizedOutput1),
	  vectorizedOutput2(_vectorizedOutput2)
	{}

	~Result() {
		delete [] scalarOutput0;
		delete [] scalarOutput1;
		delete [] scalarOutput2;
		delete [] vectorizedOutput0;
		delete [] vectorizedOutput1;
		delete [] vectorizedOutput2;
	}

	const char*  name;
	const float* input0;
	const float* input1;
	const int*   input2;
	const float  input3;
	const int    input4;
	float* scalarOutput0;
	float* scalarOutput1;
	int*   scalarOutput2;
	float* vectorizedOutput0;
	float* vectorizedOutput1;
	int*   vectorizedOutput2;
};

int main(int argc, char** argv) {

    printf("\n\n\n--------------------------------------------------------------------------------\n");
    printf("running test-suite for automatic packetization...\n\n");

	if (NUM_INPUTS_SQRT < 8U) {
		printf("ERROR: NUM_INPUTS_SQRT must be at least 8!\n");
		exit(1);
	}
	if (NUM_INPUTS_SQRT % 4U != 0) {
		printf("ERROR: NUM_INPUTS_SQRT must be defined as a multiple of 4!\n");
		exit(1);
	}

    //------------------------------------------------------------------------//
    // create function pointers for test cases and save test case names
    //------------------------------------------------------------------------//
    std::vector<TestCase*> testCases;

    //add scalar and generated functions
    addTestCases(testCases);

    const unsigned testCaseNr = testCases.size();

    //------------------------------------------------------------------------//
    // create input values
    //------------------------------------------------------------------------//
	const unsigned hardcodedInputNr = 16; // must not be changed
    const unsigned inputPermutations = 4; // must not be changed

	float scalarInputs0[NUM_INPUTS];
	float scalarInputs1[NUM_INPUTS];
	float scalarInputs2[NUM_INPUTS];
	float scalarInputs3[NUM_INPUTS];
	int scalarInputsInt0[NUM_INPUTS];
	int scalarInputsInt1[NUM_INPUTS];

	// 16 hardcoded input value sets
	scalarInputs0[0] = 0.f;
	scalarInputs0[1] = 3.f;
	scalarInputs0[2] = 2.f;
	scalarInputs0[3] = 8.f;
	scalarInputs0[4] = 10.2f;
	scalarInputs0[5] = -1.f;
	scalarInputs0[6] = 0.f;
	scalarInputs0[7] = 1000.23f;
	scalarInputs0[8] = 0.0002f;
	scalarInputs0[9] = -0.0002f;
	scalarInputs0[10] = -3.f;
	scalarInputs0[11] = -1.f;
	scalarInputs0[12] = 0.f;
	scalarInputs0[13] = 12.f;
	scalarInputs0[14] = -333.12f;
	scalarInputs0[15] = 0.003f;

    scalarInputs1[0] = 1.f;
    scalarInputs1[1] = 2.f;
    scalarInputs1[2] = 4.f;
    scalarInputs1[3] = 6.f;
    scalarInputs1[4] = -14.13f;
    scalarInputs1[5] = -13.f;
    scalarInputs1[6] = 0.f;
    scalarInputs1[7] = 0.0002f;
    scalarInputs1[8] = 420.001f;
    scalarInputs1[9] = -420.001f;
    scalarInputs1[10] = 3.f;
    scalarInputs1[11] = -1.f;
    scalarInputs1[12] = 0.f;
    scalarInputs1[13] = 12.f;
	scalarInputs1[14] = -33.0012f;
	scalarInputs1[15] = 1.0004f;

    scalarInputs2[0] = 2.f;
    scalarInputs2[1] = 1.f;
    scalarInputs2[2] = 6.f;
    scalarInputs2[3] = 4.f;
    scalarInputs2[4] = 999.f;
    scalarInputs2[5] = -5.f;
    scalarInputs2[6] = 0.f;
    scalarInputs2[7] = 420.001f;
    scalarInputs2[8] = 0.01f;
    scalarInputs2[9] = 0.01f;
    scalarInputs2[10] = 3.f;
    scalarInputs2[11] = 1.f;
    scalarInputs2[12] = 333.333f;
    scalarInputs2[13] = 4.f;
	scalarInputs2[14] = -20.f;
	scalarInputs2[15] = 20.1546f;

    scalarInputs3[0] = 3.f;
    scalarInputs3[1] = 0.f;
    scalarInputs3[2] = 8.f;
    scalarInputs3[3] = 2.f;
    scalarInputs3[4] = 0.f;
    scalarInputs3[5] = -420.001f;
    scalarInputs3[6] = 0.f;
    scalarInputs3[7] = 0.01f;
    scalarInputs3[8] = 1000.23f;
    scalarInputs3[9] = 0.01f;
    scalarInputs3[10] = -3.f;
    scalarInputs3[11] = 1.f;
    scalarInputs3[12] = -333.333f;
    scalarInputs3[13] = -4.f;
	scalarInputs3[14] = 777.01f;
	scalarInputs3[15] = -0.004f;

    scalarInputsInt0[0] = 0;
    scalarInputsInt0[1] = 1;
    scalarInputsInt0[2] = 2;
    scalarInputsInt0[3] = 3;
    scalarInputsInt0[4] = 8;
    scalarInputsInt0[5] = -13;
    scalarInputsInt0[6] = -222;
    scalarInputsInt0[7] = 99;
    scalarInputsInt0[8] = 0;
    scalarInputsInt0[9] = 111;
    scalarInputsInt0[10] = -32;
    scalarInputsInt0[11] = -1;
    scalarInputsInt0[12] = 99;
    scalarInputsInt0[13] = 1000;
	scalarInputsInt0[14] = -1000;
	scalarInputsInt0[15] = 71;

    scalarInputsInt1[0] = -3;
    scalarInputsInt1[1] = 5;
    scalarInputsInt1[2] = 100;
    scalarInputsInt1[3] = 22;
    scalarInputsInt1[4] = -56;
    scalarInputsInt1[5] = -2;
    scalarInputsInt1[6] = -1;
    scalarInputsInt1[7] = 1;
    scalarInputsInt1[8] = 2;
    scalarInputsInt1[9] = 15;
    scalarInputsInt1[10] = 1024;
    scalarInputsInt1[11] = -255;
    scalarInputsInt1[12] = -256;
    scalarInputsInt1[13] = 10;
	scalarInputsInt1[14] = 11;
	scalarInputsInt1[15] = -10;

	// now add as many random inputs as required to reach NUM_INPUTS
	#define CUSTOM_RAND_MAX 1000 //prevent too large inputs
	srand((unsigned)time(0));
	for (unsigned i=hardcodedInputNr; i<NUM_INPUTS; ++i) {
		float r = (float)rand()/(float)RAND_MAX;
		float neg = rand() > (RAND_MAX/2) ? 1.f : -1.f;
		scalarInputs0[i] = (rand() % CUSTOM_RAND_MAX) * r * neg;

		r = (float)rand()/(float)RAND_MAX;
		neg = rand() > (RAND_MAX/2) ? 1.f : -1.f;
		scalarInputs1[i] = (rand() % CUSTOM_RAND_MAX) * r * neg;

		r = (float)rand()/(float)RAND_MAX;
		neg = rand() > (RAND_MAX/2) ? 1.f : -1.f;
		scalarInputs2[i] = (rand() % CUSTOM_RAND_MAX) * r * neg;

		r = (float)rand()/(float)RAND_MAX;
		neg = rand() > (RAND_MAX/2) ? 1.f : -1.f;
		scalarInputs3[i] = (rand() % CUSTOM_RAND_MAX) * r * neg;

		int r2 = rand()/RAND_MAX;
		int neg2 = rand() > (RAND_MAX/2) ? 1 : -1;
		scalarInputsInt0[i] = (rand() % CUSTOM_RAND_MAX) * r2 * neg2;

		r2 = rand()/RAND_MAX;
		neg2 = rand() > (RAND_MAX/2) ? 1 : -1;
		scalarInputsInt1[i] = (rand() % CUSTOM_RAND_MAX) * r2 * neg2;
	}

	// store pointers to arrays for random access (input permutations)
	float* inputArrays[4];
	int* inputArraysInt[2];

	inputArrays[0] = scalarInputs0;
	inputArrays[1] = scalarInputs1;
	inputArrays[2] = scalarInputs2;
	inputArrays[3] = scalarInputs3;
	inputArraysInt[0] = scalarInputsInt0;
	inputArraysInt[1] = scalarInputsInt1;


    //------------------------------------------------------------------------//
    // create result array (each result holds the computed arrays, which
	// equals the results of NUM_INPUTS scalar calls)
    //------------------------------------------------------------------------//
    const unsigned resultSetNr = inputPermutations * inputPermutations * testCaseNr;
    Result** results = new Result*[resultSetNr]();
    
    //------------------------------------------------------------------------//
    // compute results of scalar and generated functions
    //------------------------------------------------------------------------//
    unsigned testsRun = 0;

	for (unsigned TC=0; TC<testCaseNr; ++TC) {
		unsigned inputPermsRun = 0;
		for (unsigned i=0; i<inputPermutations; ++i) {
			for (unsigned i2=0; i2<inputPermutations; ++i2) {

				// abort if we have already run too many test cases
				if (testsRun >= resultSetNr*NUM_INPUTS) {
					printf("\nERROR: not enough space allocated for results!\n");
					exit(-1);
				}

				// get function pointers of current test case
				const TestCase& testCase = *testCases[TC];

				// choose input arrays
				const float* input0 = inputArrays[i];
				const float* input1 = inputArrays[i2];
				const int*   input2 = inputArraysInt[(i+i2) % 2];
				// choose uniform inputs ( = the same for the entire run!)
				const float  input3 = scalarInputs0[i*inputPermutations + i2];
				const int    input4 = scalarInputsInt0[i*inputPermutations + i2];

				// create output arrays for scalar function
				float* scalarOutput0 = new float[NUM_INPUTS]();
				float* scalarOutput1 = new float[NUM_INPUTS]();
				int*   scalarOutput2 = new int[NUM_INPUTS]();
				for (unsigned idx=0; idx<NUM_INPUTS; ++idx) {
					scalarOutput0[idx] = 0.f;
					scalarOutput1[idx] = 0.f;
					scalarOutput2[idx] = 0;
				}

				//printf("\nexecuting scalar function...\n");

				// execute scalar function, setting the appropriate opencl state in each iteration
				for (unsigned x=0; x<GLOBAL_SIZE_0; ++x) {
					global_id_0 = x;
					local_id_0 = x % LOCAL_SIZE_0;
					group_id_0 = x / LOCAL_SIZE_0;

					for (unsigned y=0; y<GLOBAL_SIZE_1; ++y) {
						global_id_1 = y;
						local_id_1 = y % LOCAL_SIZE_1;
						group_id_1 = y / LOCAL_SIZE_1;
						//printf("iteration: %d/%d\n", x, y);
						//printf("get_global_id_0: %d\n", get_global_id_0());
						//printf("get_global_id_1: %d\n", get_global_id_1());
						//printf("get_local_id_0: %d\n", get_local_id_0());
						//printf("get_local_id_1: %d\n", get_local_id_1());
						testCase.scalarFn(input0, input1, input2, scalarOutput0, scalarOutput1, scalarOutput2, input3, input4);
						//printf("result: %f\n", scalarOutput0[get_global_id_0()]);
					}
				}


				// create output arrays for vector function
				float* vectorizedOutput0 = new float[NUM_INPUTS]();
				float* vectorizedOutput1 = new float[NUM_INPUTS]();
				int*   vectorizedOutput2 = new int[NUM_INPUTS]();
				for (unsigned idx=0; idx<NUM_INPUTS; ++idx) {
					vectorizedOutput0[idx] = 0.f;
					vectorizedOutput1[idx] = 0.f;
					vectorizedOutput2[idx] = 0;
				}

				//printf("\nexecuting packet function...\n");

				// execute vectorized function, setting the appropriate opencl state in each iteration
				for (unsigned x=0; x<GLOBAL_SIZE_0; x+=SIMD_WIDTH) {
					global_id_0 = x;
					local_id_0 = x % (LOCAL_SIZE_0 / SIMD_WIDTH);
					group_id_0 = x / (LOCAL_SIZE_0 / SIMD_WIDTH);

					for (unsigned y=0; y<GLOBAL_SIZE_1; ++y) {
						global_id_1 = y;
						local_id_1 = y % LOCAL_SIZE_1;
						group_id_1 = y / LOCAL_SIZE_1;
						//printf("iteration: %d/%d\n", x, y);
						//printf("get_global_id_0: %d\n", get_global_id_0());
						//printf("get_global_id_1: %d\n", get_global_id_1());
						//printf("get_local_id_0: %d\n", get_local_id_0());
						//printf("get_local_id_1: %d\n", get_local_id_1());
						testCase.vectorizedFn(input0, input1, input2, vectorizedOutput0, vectorizedOutput1, vectorizedOutput2, input3, input4);
						//printf("result: %f\n", vectorizedOutput0[get_global_id_0()]);
					}
				}

				// result index (between 0 and resultSetNr)
				const unsigned index = TC*inputPermutations*inputPermutations + i*inputPermutations + i2;

				// store results & information
				results[index] = new Result(testCase.name,
											input0,
											input1,
											input2,
											input3,
											input4,
											scalarOutput0,
											scalarOutput1,
											scalarOutput2,
											vectorizedOutput0,
											vectorizedOutput1,
											vectorizedOutput2);

				//Result* res = results[index];
				//for (unsigned idx=0; idx<NUM_INPUTS; ++idx) {
					//const float scalarRes0 = res->scalarOutput0[idx];
					//const float pktRes0 = res->vectorizedOutput0[idx];
					//const bool success = resultMatches(scalarRes0, pktRes0);
					//if (!success) {
						//printf("ERROR at index %d:\n  expected result: %f\n  computed result: %f\n", idx, scalarRes0, pktRes0);
					//}
				//}

				testsRun += NUM_INPUTS;
				++inputPermsRun;
				printf("Total progress: %.2f percent | test case %03d/%03d (inputs %03d/%03d) : %-30.30s\r", ((float)(testsRun / NUM_INPUTS) * 100.f) / (float)resultSetNr, TC+1, testCaseNr, inputPermsRun, inputPermutations*inputPermutations, testCase.name); fflush(stdout);
			}
        }
    }

    if (testsRun <= 0) {
        printf("\nERROR: need to compute at least one result! (forgot to activate test cases?)\n");
        exit(-1);
    }
    if (testsRun != resultSetNr*NUM_INPUTS) {
        printf("\nERROR: unexpected number of results  (testsRun != resultSetNr*NUM_INPUTS)!\n");
        exit(-1);
    }

    printf("\nverifying results:\n\n");

    bool allSuccessfull = true;
    unsigned failedTestsNr = 0;
    for (unsigned i=0; i<resultSetNr; ++i) {
		Result* res = results[i];
		for (unsigned j=0; j<NUM_INPUTS; ++j) {

			// The inputs actually don't help much, because each test case
			// can access arbitrary values from the arrays
			const float input0 = res->input0[j];
			const float input1 = res->input1[j];
			const int   input2 = res->input2[j];
			const float input3 = res->input3;
			const int   input4 = res->input4;

			const float scalarRes0 = res->scalarOutput0[j];
			const float scalarRes1 = res->scalarOutput1[j];
			const int   scalarRes2 = res->scalarOutput2[j];
			const float pktRes0 = res->vectorizedOutput0[j];
			const float pktRes1 = res->vectorizedOutput1[j];
			const int   pktRes2 = res->vectorizedOutput2[j];

			const bool success =
				resultMatches(scalarRes0, pktRes0) &&
				resultMatches(scalarRes1, pktRes1) &&
				resultMatches(scalarRes2, pktRes2);

			if (!success) {
				const unsigned gid0 = j % GLOBAL_SIZE_0;
				const unsigned gid1 = j / GLOBAL_SIZE_1;
				printf("%s FAILED at global id %d/%d! ", res->name, gid0, gid1);
				printf(" %f %f %d %f %d\n", input0, input1, input2, input3, input4);
				printf("  expected results: [ %f %f %d ]\n", scalarRes0, scalarRes1, scalarRes2);
				printf("  computed results: [ %f %f %d ]\n", pktRes0, pktRes1, pktRes2);
			}

			allSuccessfull &= success;
			if (!success) ++failedTestsNr;
		}
    }

    if (allSuccessfull) printf("ALL TESTS SUCCESSFULL! (%d)\n", testsRun);
	else printf("\n%d / %d TESTS FAILED!", failedTestsNr, testsRun);

    printf("\n\ntest-suite run complete!\n");
    printf("--------------------------------------------------------------------------------\n\n");


    for (unsigned i=0; i<resultSetNr; ++i) {
		delete results[i];
	}

    return 0;
}

