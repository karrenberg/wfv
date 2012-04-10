/**
 * @file   wfvTests2.cpp
 * @date   24.02.2010
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2010, 2011 Saarland University
 *
 */
#include <stdio.h>
#include <math.h>
#include <xmmintrin.h>

#include <vector>

#define USE_RANDOM_TESTS
#include <ctime>    // For time()
#include <cstdlib>  // For srand() and rand()
#define NUM_RANDOM_INPUT_VALUE_SETS 30

#define packetizationSize 4
#define totalSIMDIterations packetizationSize / 4

#if totalSIMDIterations == 1
    #define VEC __m128
    #define VECI __m128i
#else
    #define VEC __m128*
    #define VECI __m128i*
    #warning "test suite currently does not support packetizationSize > 4!"
#endif

#define ALIGN __attribute__ ((aligned (16)))
#define REALIGN __attribute__((force_align_arg_pointer))

// helper: SmallVector does not allow to use __m128 directly (no constructor?!)
struct V {
    V() {}
    V(VEC a) { data = a; }
    VEC data;
} ALIGN;

void *aligned_malloc(size_t size, size_t align_size) {

  char *ptr,*ptr2,*aligned_ptr;
  int align_mask = align_size - 1;

  ptr=(char *)malloc(size + align_size + sizeof(int));
  if(ptr==NULL) return(NULL);

  ptr2 = ptr + sizeof(int);
  aligned_ptr = ptr2 + (align_size - ((size_t)ptr2 & align_mask));


  ptr2 = aligned_ptr - sizeof(int);
  *((int *)ptr2)=(int)(aligned_ptr - ptr);

  return(aligned_ptr);
}

// helper: extract ith element of a __m128
inline float& get(const VEC& v, const unsigned idx) {
    return ((float*)&v)[idx];
}
inline int& get(const VECI& v, const unsigned idx) {
    return ((int*)&v)[idx];
}
inline float& get(const V& v, const unsigned idx) {
    return ((float*)&v)[idx];
}
inline float& get(const V* v, const unsigned idx) {
    return ((float*)&(v->data))[idx];
}

// helper: we treat NaN as an equal result ;)
inline bool resultMatches(const float a, const float b) {
	return a == b ||
		(isnan(a) && isnan(b)) ||
		(abs(a-b) < 0.000001);
}
inline bool resultMatches(const int a, const int b) {
	return a == b;
}


//----------------------------------------------------------------------------//
// type declarations for scalar functions
//----------------------------------------------------------------------------//
// TODO: test integer data
struct Vec3f {
	float x, y, z;
	Vec3f() {};
	Vec3f(float a, float b, float c) : x(a), y(b), z(c) {}
};
struct Vec3fA {
	float data[3];
	Vec3fA() {};
	Vec3fA(float a, float b, float c) { data[0] = a; data[1] = b; data[2] = c; }
};
struct Uniform {
	float x, y, z;
	float a;
};
struct Uniform2 {
	float x;
	float a;
	float y;
	float b;
	float z;
	float c;
};
struct Nested {
	Vec3f a, b, c;
};
struct Nested2 {
	float x;
	Vec3f a;
	float y;
	Uniform u;
	Vec3fA b;
};

//----------------------------------------------------------------------------//
// type declarations for packetized functions
//----------------------------------------------------------------------------//
typedef float Arr[3];
typedef __m128 Arr_pkt[3];
typedef float Arr8[8];
typedef __m128 Arr8_pkt[8];
typedef int ArrI8[8];
typedef __m128i ArrI8_pkt[8];
typedef int ArrI6[6];
typedef __m128i ArrI6_pkt[6];

struct StrI3 {
	int f;
	int* g;
	int h;
};
struct StrI3_pkt {
	VECI f;
	VECI* g;
	VECI h;
};

struct Vec3f_pkt {
	VEC x, y, z;
	Vec3f_pkt() {}
	Vec3f_pkt(VEC a, VEC b, VEC c) : x(a), y(b), z(c) {}
};
struct Vec3fA_pkt {
	VEC data[3];
	Vec3fA_pkt() {}
	Vec3fA_pkt(VEC a, VEC b, VEC c) { data[0] = a; data[1] = b; data[2] = c; }
};
struct Uniform_pkt {
	VEC x, y, z;
	float a;
};
struct Uniform2_pkt {
	VEC x;
	float a;
	VEC y;
	float b;
	VEC z;
	float c;
};
struct Nested_pkt {
	Vec3f_pkt a, b, c;
};
struct Nested2_pkt {
	VEC x;
	Vec3f_pkt a;
	float y;
	Uniform u;
	Vec3fA_pkt b;
};


//----------------------------------------------------------------------------//
// input data struct supplied to each test case execution function
//----------------------------------------------------------------------------//
#define INPUTDATA float f0, float f1, float f2, float f3, \
	float f4, float f5, float f6, float f7, \
	VEC v3210, VEC v7654, VECI vi3210, VECI vi7654

typedef bool (*executeFnType)(float, float, float, float,
					float, float, float, float,
					VEC, VEC, VECI, VECI);


//----------------------------------------------------------------------------//
// helper functions/macros
//----------------------------------------------------------------------------//
#define SETUPSCALARSTRUCTS \
	Vec3f a0(f0, f0, f0); \
	Vec3f a1(f1, f1, f1); \
	Vec3f a2(f2, f2, f2); \
	Vec3f a3(f3, f3, f3); \
	Vec3f b0(f4, f4, f4); \
	Vec3f b1(f5, f5, f5); \
	Vec3f b2(f6, f6, f6); \
	Vec3f b3(f7, f7, f7); \
	Vec3f r0(f0, f0, f0); \
	Vec3f r1(f1, f1, f1); \
	Vec3f r2(f2, f2, f2); \
	Vec3f r3(f3, f3, f3); \
	((void)0)

#define SETUPPACKETSTRUCTS \
	VEC a = _mm_set_ps(f3, f2, f1, f0); \
	VEC b = _mm_set_ps(f7, f6, f5, f4); \
	Vec3f_pkt av(a, a, a); \
	Vec3f_pkt bv(b, b, b); \
	Vec3f_pkt res(a, a, a); \
	((void)0)

#define SETUPSCALARARRAYS \
	Vec3fA a0(f0, f0, f0); \
	Vec3fA a1(f1, f1, f1); \
	Vec3fA a2(f2, f2, f2); \
	Vec3fA a3(f3, f3, f3); \
	Vec3fA b0(f4, f4, f4); \
	Vec3fA b1(f5, f5, f5); \
	Vec3fA b2(f6, f6, f6); \
	Vec3fA b3(f7, f7, f7); \
	Vec3fA r0(f0, f0, f0); \
	Vec3fA r1(f1, f1, f1); \
	Vec3fA r2(f2, f2, f2); \
	Vec3fA r3(f3, f3, f3); \
	((void)0)

#define SETUPPACKETARRAYS \
	VEC a = _mm_set_ps(f3, f2, f1, f0); \
	VEC b = _mm_set_ps(f7, f6, f5, f4); \
	Vec3fA_pkt av(a, a, a); \
	Vec3fA_pkt bv(b, b, b); \
	Vec3fA_pkt res(a, a, a); \
	((void)0)

#define SETUPFULLARRAYTEST(testfn_scalar, testfn_pkt, testfn_execute) \
	extern "C" void testfn_pkt(int* uniformOutArr, VECI* varyingOutArr, int* uniformInArr, VECI* varyingInArr, VECI varyingInX, int uniformInX) ALIGN; \
	bool testfn_execute(INPUTDATA) { \
		/* Uniform input array, same for scalar and packet function */ \
		ArrI8 in1 = { (int)f0, (int)f1, (int)f2, (int)f3, (int)f4, (int)f5, (int)f6, (int)f7 }; \
	 \
		/* Varying input arrays, different for each call */ \
		ArrI8 in2_0 = { (int)f4, (int)f5, (int)f6, (int)f7, (int)f0, (int)f1, (int)f1, (int)f4 }; \
		ArrI8 in2_1 = { (int)f1, (int)f5, (int)f4, (int)f2, (int)f1, (int)f1, (int)f1, (int)f5 }; \
		ArrI8 in2_2 = { (int)f3, (int)f2, (int)f3, (int)f7, (int)f7, (int)f4, (int)f6, (int)f6 }; \
		ArrI8 in2_3 = { (int)f4, (int)f5, (int)f6, (int)f2, (int)f3, (int)f4, (int)f3, (int)f7 }; \
		ArrI8 in2_4 = { (int)f6, (int)f1, (int)f2, (int)f4, (int)f3, (int)f5, (int)f2, (int)f2 }; \
		ArrI8 in2_5 = { (int)f6, (int)f3, (int)f0, (int)f3, (int)f3, (int)f6, (int)f4, (int)f4 }; \
		ArrI8 in2_6 = { (int)f2, (int)f7, (int)f0, (int)f1, (int)f0, (int)f5, (int)f5, (int)f1 }; \
		ArrI8 in2_7 = { (int)f7, (int)f0, (int)f7, (int)f3, (int)f2, (int)f4, (int)f5, (int)f3 }; \
	 \
		/* Vectorized arrays (equivalent to in2_0-in2_7) */ \
		ArrI8_pkt in2_pkt_0 = { _mm_set_epi32((int)f4, (int)f3, (int)f1, (int)f4), \
								_mm_set_epi32((int)f5, (int)f2, (int)f5, (int)f5), \
								_mm_set_epi32((int)f6, (int)f3, (int)f4, (int)f6), \
								_mm_set_epi32((int)f2, (int)f7, (int)f2, (int)f7), \
								_mm_set_epi32((int)f3, (int)f7, (int)f1, (int)f0), \
								_mm_set_epi32((int)f4, (int)f4, (int)f1, (int)f1), \
								_mm_set_epi32((int)f3, (int)f6, (int)f1, (int)f1), \
								_mm_set_epi32((int)f7, (int)f6, (int)f5, (int)f4) }; \
		ArrI8_pkt in2_pkt_1 = { _mm_set_epi32((int)f7, (int)f2, (int)f6, (int)f6), \
								_mm_set_epi32((int)f0, (int)f7, (int)f3, (int)f1), \
								_mm_set_epi32((int)f7, (int)f0, (int)f0, (int)f2), \
								_mm_set_epi32((int)f3, (int)f1, (int)f3, (int)f4), \
								_mm_set_epi32((int)f2, (int)f0, (int)f3, (int)f3), \
								_mm_set_epi32((int)f4, (int)f5, (int)f6, (int)f5), \
								_mm_set_epi32((int)f5, (int)f5, (int)f4, (int)f2), \
								_mm_set_epi32((int)f3, (int)f1, (int)f4, (int)f2) }; \
	 \
		ArrI8 out1 = { 13, 13, 13, 13, 13, 13, 13, 13 }; \
		ArrI8 out2_0 = { 13, 13, 13, 13, 13, 13, 13, 13 }; \
		ArrI8 out2_1 = { 13, 13, 13, 13, 13, 13, 13, 13 }; \
		ArrI8 out2_2 = { 13, 13, 13, 13, 13, 13, 13, 13 }; \
		ArrI8 out2_3 = { 13, 13, 13, 13, 13, 13, 13, 13 }; \
		ArrI8 out2_4 = { 13, 13, 13, 13, 13, 13, 13, 13 }; \
		ArrI8 out2_5 = { 13, 13, 13, 13, 13, 13, 13, 13 }; \
		ArrI8 out2_6 = { 13, 13, 13, 13, 13, 13, 13, 13 }; \
		ArrI8 out2_7 = { 13, 13, 13, 13, 13, 13, 13, 13 }; \
	 \
		testfn_scalar(out1, out2_0, in1, in2_0, 0, 1); \
		testfn_scalar(out1, out2_1, in1, in2_1, 1, 1); \
		testfn_scalar(out1, out2_2, in1, in2_2, 2, 1); \
		testfn_scalar(out1, out2_3, in1, in2_3, 3, 1); \
		testfn_scalar(out1, out2_4, in1, in2_4, 7, 1); \
		testfn_scalar(out1, out2_5, in1, in2_5, 4, 1); \
		testfn_scalar(out1, out2_6, in1, in2_6, 5, 1); \
		testfn_scalar(out1, out2_7, in1, in2_7, 6, 1); \
	 \
		/* Varying 'varyingInX' parameter, equivalent to 4th parameters of scalar calls */ \
		VECI varyingInX0 = _mm_set_epi32(3, 2, 1, 0); \
		VECI varyingInX1 = _mm_set_epi32(6, 5, 4, 7); \
	 \
		ArrI8 out1_pkt = { 13, 13, 13, 13, 13, 13, 13, 13 }; \
		ArrI8_pkt out2_pkt_0 = { _mm_set1_epi32(13), _mm_set1_epi32(13), _mm_set1_epi32(13), _mm_set1_epi32(13), _mm_set1_epi32(13), _mm_set1_epi32(13), _mm_set1_epi32(13), _mm_set1_epi32(13) }; \
		ArrI8_pkt out2_pkt_1 = { _mm_set1_epi32(13), _mm_set1_epi32(13), _mm_set1_epi32(13), _mm_set1_epi32(13), _mm_set1_epi32(13), _mm_set1_epi32(13), _mm_set1_epi32(13), _mm_set1_epi32(13) }; \
	 \
		testfn_pkt(out1_pkt, out2_pkt_0, in1, in2_pkt_0, varyingInX0, 1); \
		testfn_pkt(out1_pkt, out2_pkt_1, in1, in2_pkt_1, varyingInX1, 1); \
	 \
		const bool success = resultMatches(out1, out1_pkt) && \
				resultMatches(out2_0, out2_1, out2_2, out2_3, out2_pkt_0) && \
				resultMatches(out2_4, out2_5, out2_6, out2_7, out2_pkt_1); \
     \
	 	if (!success) { \
			printf("\nWrong result found!\n"); \
			printf("inputU: %d %d %d %d %d %d %d %d\n", in1[0], in1[1], in1[2], in1[3], in1[4], in1[5], in1[6], in1[7]); \
			for (unsigned i=0; i<8; ++i) { \
				printf("input[%d]: %d %d %d %d %d %d %d %d\n", i, in2_0[i], in2_1[i], in2_2[i], in2_3[i], in2_4[i], in2_5[i], in2_6[i], in2_7[i]); \
			} \
			printf("expectedU: %d %d %d %d %d %d %d %d\n", out1[0], out1[1], out1[2], out1[3], out1[4], out1[5], out1[6], out1[7]); \
			printf("returnedU: %d %d %d %d %d %d %d %d\n", out1_pkt[0], out1_pkt[1], out1_pkt[2], out1_pkt[3], out1_pkt[4], out1_pkt[5], out1_pkt[6], out1_pkt[7]); \
			for (unsigned i=0; i<8; ++i) { \
				printf("expected[%d]: %d %d %d %d %d %d %d %d\n", i, out2_0[i], out2_1[i], out2_2[i], out2_3[i], out2_4[i], out2_5[i], out2_6[i], out2_7[i]); \
				printf("returned[%d]: %d %d %d %d %d %d %d %d\n", i, get(out2_pkt_0[i], 0), get(out2_pkt_0[i], 1), get(out2_pkt_0[i], 2), get(out2_pkt_0[i], 3), get(out2_pkt_1[i], 0), get(out2_pkt_1[i], 1), get(out2_pkt_1[i], 2), get(out2_pkt_1[i], 3)); \
			} \
			printf("\n"); \
		} \
		return success; \
	}
		

#define SETUPFULLSTRUCTTEST(testfn_scalar, testfn_pkt, testfn_execute) \
	extern "C" void testfn_pkt(int* uniformOutArr, StrI3* uniformOutStr, StrI3_pkt* varyingOutStr, StrI3 uniformInStr, StrI3_pkt varyingInStr, VECI varyingInX, int uniformInX) ALIGN; \
	bool testfn_execute(INPUTDATA) { \
		/* Uniform input struct, same for scalar and packet function */ \
		StrI3 uniformInStr; \
		int guni = (int)f1; \
		uniformInStr.f = (int)f0; \
		uniformInStr.g = &guni; \
		uniformInStr.h = (int)f2; \
 \
		/* Varying input structs, different for each call, supplied via pointer */ \
		int gin0 = (int)f0, gin1 = (int)f1, gin2 = (int)f2, gin3 = (int)f3, gin4 = (int)f4, gin5 = (int)f5, gin6 = (int)f6, gin7 = (int)f7; \
		StrI3 varyingInStr0; varyingInStr0.f = (int)f0; varyingInStr0.g = (int*)&gin0; varyingInStr0.h = (int)f7; \
		StrI3 varyingInStr1; varyingInStr1.f = (int)f1; varyingInStr1.g = (int*)&gin1; varyingInStr1.h = (int)f6; \
		StrI3 varyingInStr2; varyingInStr2.f = (int)f3; varyingInStr2.g = (int*)&gin2; varyingInStr2.h = (int)f4; \
		StrI3 varyingInStr3; varyingInStr3.f = (int)f5; varyingInStr3.g = (int*)&gin3; varyingInStr3.h = (int)f1; \
		StrI3 varyingInStr4; varyingInStr4.f = (int)f6; varyingInStr4.g = (int*)&gin4; varyingInStr4.h = (int)f5; \
		StrI3 varyingInStr5; varyingInStr5.f = (int)f2; varyingInStr5.g = (int*)&gin5; varyingInStr5.h = (int)f0; \
		StrI3 varyingInStr6; varyingInStr6.f = (int)f7; varyingInStr6.g = (int*)&gin6; varyingInStr6.h = (int)f2; \
		StrI3 varyingInStr7; varyingInStr7.f = (int)f4; varyingInStr7.g = (int*)&gin7; varyingInStr7.h = (int)f3; \
 \
		/* Vectorized input structs (equivalent to varyingInStr0-varyingInStr7) */ \
		VECI gin_pkt_0 = _mm_set_epi32((int)f3, (int)f2, (int)f1, (int)f0); \
		VECI gin_pkt_1 = _mm_set_epi32((int)f7, (int)f6, (int)f5, (int)f4); \
		StrI3_pkt varyingInStr_pkt_0; varyingInStr_pkt_0.f = _mm_set_epi32((int)f5, (int)f3, (int)f1, (int)f0); varyingInStr_pkt_0.g = &gin_pkt_0; varyingInStr_pkt_0.h = _mm_set_epi32((int)f1, (int)f4, (int)f6, (int)f7); \
		StrI3_pkt varyingInStr_pkt_1; varyingInStr_pkt_1.f = _mm_set_epi32((int)f4, (int)f7, (int)f2, (int)f6); varyingInStr_pkt_1.g = &gin_pkt_1; varyingInStr_pkt_1.h = _mm_set_epi32((int)f3, (int)f2, (int)f0, (int)f5); \
 \
		/* Uniform output array for scalar calls */ \
		ArrI8 out = { 13, 13, 13, 13, 13, 13, 13, 13 }; \
 \
		/* Uniform output struct for scalar calls, same for scalar and packet function */ \
		StrI3 uniformOutStr; int g = 33; uniformOutStr.f = 5; uniformOutStr.g = &g; uniformOutStr.h = -1; \
		StrI3 uniformOutStr_pkt; int g_pkt = 33; uniformOutStr_pkt.f = 5; uniformOutStr_pkt.g = &g_pkt; uniformOutStr_pkt.h = -1; \
 \
		/* Varying output structs, different for each call, supplied via pointer */ \
		int gout0 = 33, gout1 = 33, gout2 = 33, gout3 = 33, gout4 = 33, gout5 = 33, gout6 = 33, gout7 = 33; \
		StrI3 varyingOutStr0; varyingOutStr0.f = 5; varyingOutStr0.g = &gout0; varyingOutStr0.h = -1; \
		StrI3 varyingOutStr1; varyingOutStr1.f = 5; varyingOutStr1.g = &gout1; varyingOutStr1.h = -1; \
		StrI3 varyingOutStr2; varyingOutStr2.f = 5; varyingOutStr2.g = &gout2; varyingOutStr2.h = -1; \
		StrI3 varyingOutStr3; varyingOutStr3.f = 5; varyingOutStr3.g = &gout3; varyingOutStr3.h = -1; \
		StrI3 varyingOutStr4; varyingOutStr4.f = 5; varyingOutStr4.g = &gout4; varyingOutStr4.h = -1; \
		StrI3 varyingOutStr5; varyingOutStr5.f = 5; varyingOutStr5.g = &gout5; varyingOutStr5.h = -1; \
		StrI3 varyingOutStr6; varyingOutStr6.f = 5; varyingOutStr6.g = &gout6; varyingOutStr6.h = -1; \
		StrI3 varyingOutStr7; varyingOutStr7.f = 5; varyingOutStr7.g = &gout7; varyingOutStr7.h = -1; \
 \
		/* Vectorized output structs (equivalent to varyingOutStr0-varyingOutStr7) */ \
		VECI gout_pkt_0 = _mm_set_epi32(33, 33, 33, 33); \
		VECI gout_pkt_1 = _mm_set_epi32(33, 33, 33, 33); \
		StrI3_pkt varyingOutStr_pkt_0; varyingOutStr_pkt_0.f = _mm_set_epi32(5, 5, 5, 5); varyingOutStr_pkt_0.g = &gout_pkt_0; varyingOutStr_pkt_0.h = _mm_set_epi32(-1, -1, -1, -1); \
		StrI3_pkt varyingOutStr_pkt_1; varyingOutStr_pkt_1.f = _mm_set_epi32(5, 5, 5, 5); varyingOutStr_pkt_1.g = &gout_pkt_1; varyingOutStr_pkt_1.h = _mm_set_epi32(-1, -1, -1, -1); \
 \
		testfn_scalar(out, &uniformOutStr, &varyingOutStr0, uniformInStr, varyingInStr0, 0, 1); \
		testfn_scalar(out, &uniformOutStr, &varyingOutStr1, uniformInStr, varyingInStr1, 1, 1); \
		testfn_scalar(out, &uniformOutStr, &varyingOutStr2, uniformInStr, varyingInStr2, 2, 1); \
		testfn_scalar(out, &uniformOutStr, &varyingOutStr3, uniformInStr, varyingInStr3, 3, 1); \
		testfn_scalar(out, &uniformOutStr, &varyingOutStr4, uniformInStr, varyingInStr4, 7, 1); \
		testfn_scalar(out, &uniformOutStr, &varyingOutStr5, uniformInStr, varyingInStr5, 4, 1); \
		testfn_scalar(out, &uniformOutStr, &varyingOutStr6, uniformInStr, varyingInStr6, 5, 1); \
		testfn_scalar(out, &uniformOutStr, &varyingOutStr7, uniformInStr, varyingInStr7, 6, 1); \
 \
		/* Varying 'varyingInX' parameter, equivalent to 4th parameters of scalar calls */ \
		VECI varyingInX0 = _mm_set_epi32(3, 2, 1, 0); \
		VECI varyingInX1 = _mm_set_epi32(6, 5, 4, 7); \
 \
		/* Uniform output array for vectorized calls */ \
		ArrI8 out_pkt = { 13, 13, 13, 13, 13, 13, 13, 13 }; \
 \
		testfn_pkt(out_pkt, &uniformOutStr_pkt, &varyingOutStr_pkt_0, uniformInStr, varyingInStr_pkt_0, varyingInX0, 1); \
		testfn_pkt(out_pkt, &uniformOutStr_pkt, &varyingOutStr_pkt_1, uniformInStr, varyingInStr_pkt_1, varyingInX1, 1); \
 \
		const bool success = resultMatches(out, out_pkt) && \
				resultMatches(uniformOutStr, uniformOutStr_pkt) && \
				resultMatches(varyingOutStr0, varyingOutStr1, varyingOutStr2, varyingOutStr3, varyingOutStr_pkt_0) && \
				resultMatches(varyingOutStr4, varyingOutStr5, varyingOutStr6, varyingOutStr7, varyingOutStr_pkt_1); \
 \
		if (!success) { \
			printf("\nWrong result found!\n"); \
			printf("uniform input: { %d %d %d }\n", uniformInStr.f, *(uniformInStr.g), uniformInStr.h); \
			\
			printf("input.f: %d %d %d %d %d %d %d %d\n", varyingInStr0.f, varyingInStr1.f, varyingInStr2.f, varyingInStr3.f, varyingInStr4.f, varyingInStr5.f, varyingInStr6.f, varyingInStr7.f); \
			printf("input.h: %d %d %d %d %d %d %d %d\n", varyingInStr0.h, varyingInStr1.h, varyingInStr2.h, varyingInStr3.h, varyingInStr4.h, varyingInStr5.h, varyingInStr6.h, varyingInStr7.h); \
			printf("input.g: %d %d %d %d %d %d %d %d\n", *(varyingInStr0.g), *(varyingInStr1.g), *(varyingInStr2.g), *(varyingInStr3.g), *(varyingInStr4.g), *(varyingInStr5.g), *(varyingInStr6.g), *(varyingInStr7.g)); \
			printf("expectedU: %d %d %d %d %d %d %d %d\n", out[0], out[1], out[2], out[3], out[4], out[5], out[6], out[7]); \
			printf("returnedU: %d %d %d %d %d %d %d %d\n", out_pkt[0], out_pkt[1], out_pkt[2], out_pkt[3], out_pkt[4], out_pkt[5], out_pkt[6], out_pkt[7]); \
			printf("uniform expected: { %d %d %d }\n", uniformOutStr.f, *(uniformOutStr.g), uniformOutStr.h); \
			printf("uniform returned: { %d %d %d }\n", uniformOutStr_pkt.f, *(uniformOutStr_pkt.g), uniformOutStr_pkt.h); \
			printf("expected.f: %d %d %d %d %d %d %d %d\n", varyingOutStr0.f, varyingOutStr1.f, varyingOutStr2.f, varyingOutStr3.f, varyingOutStr4.f, varyingOutStr5.f, varyingOutStr6.f, varyingOutStr7.f); \
			printf("returned.f: %d %d %d %d %d %d %d %d\n", get(varyingOutStr_pkt_0.f, 0), get(varyingOutStr_pkt_0.f, 1), get(varyingOutStr_pkt_0.f, 2), get(varyingOutStr_pkt_0.f, 3), get(varyingOutStr_pkt_1.f, 0), get(varyingOutStr_pkt_1.f, 1), get(varyingOutStr_pkt_1.f, 2), get(varyingOutStr_pkt_1.f, 3)); \
			printf("expected.h: %d %d %d %d %d %d %d %d\n", varyingOutStr0.h, varyingOutStr1.h, varyingOutStr2.h, varyingOutStr3.h, varyingOutStr4.h, varyingOutStr5.h, varyingOutStr6.h, varyingOutStr7.h); \
			printf("returned.h: %d %d %d %d %d %d %d %d\n", get(varyingOutStr_pkt_0.h, 0), get(varyingOutStr_pkt_0.h, 1), get(varyingOutStr_pkt_0.h, 2), get(varyingOutStr_pkt_0.h, 3), get(varyingOutStr_pkt_1.h, 0), get(varyingOutStr_pkt_1.h, 1), get(varyingOutStr_pkt_1.h, 2), get(varyingOutStr_pkt_1.h, 3)); \
			printf("expected.g: %d %d %d %d %d %d %d %d\n", *(varyingOutStr0.g), *(varyingOutStr1.g), *(varyingOutStr2.g), *(varyingOutStr3.g), *(varyingOutStr4.g), *(varyingOutStr5.g), *(varyingOutStr6.g), *(varyingOutStr7.g)); \
			printf("returned.g: %d %d %d %d %d %d %d %d\n", get(*(varyingOutStr_pkt_0.g), 0), get(*(varyingOutStr_pkt_0.g), 1), get(*(varyingOutStr_pkt_0.g), 2), get(*(varyingOutStr_pkt_0.g), 3), get(*(varyingOutStr_pkt_1.g), 0), get(*(varyingOutStr_pkt_1.g), 1), get(*(varyingOutStr_pkt_1.g), 2), get(*(varyingOutStr_pkt_1.g), 3)); \
			printf("\n"); \
		} \
		return success; \
	}
		
		


inline bool resultMatches(const int& r0, const int& r1, const int& r2, const int& r3, const VECI& res) {
	return resultMatches(r0, get(res, 0)) &&
		resultMatches(r1, get(res, 1)) &&
		resultMatches(r2, get(res, 2)) &&
		resultMatches(r3, get(res, 3));
}

inline bool resultMatches(const Vec3f& r0, const Vec3f& r1, const Vec3f& r2, const Vec3f& r3, const Vec3f_pkt& res) {
	return resultMatches(r0.x, get(res.x, 0)) &&
		resultMatches(r0.y, get(res.y, 0)) &&
		resultMatches(r0.z, get(res.z, 0)) &&
		resultMatches(r1.x, get(res.x, 1)) &&
		resultMatches(r1.y, get(res.y, 1)) &&
		resultMatches(r1.z, get(res.z, 1)) &&
		resultMatches(r2.x, get(res.x, 2)) &&
		resultMatches(r2.y, get(res.y, 2)) &&
		resultMatches(r2.z, get(res.z, 2)) &&
		resultMatches(r3.x, get(res.x, 3)) &&
		resultMatches(r3.y, get(res.y, 3)) &&
		resultMatches(r3.z, get(res.z, 3));
}
inline bool resultMatches(const Vec3fA& r0, const Vec3fA& r1, const Vec3fA& r2, const Vec3fA& r3, const Vec3fA_pkt& res) {
	return resultMatches(r0.data[0], get(res.data[0], 0)) &&
		resultMatches(r0.data[1], get(res.data[1], 0)) &&
		resultMatches(r0.data[2], get(res.data[2], 0)) &&
		resultMatches(r1.data[0], get(res.data[0], 1)) &&
		resultMatches(r1.data[1], get(res.data[1], 1)) &&
		resultMatches(r1.data[2], get(res.data[2], 1)) &&
		resultMatches(r2.data[0], get(res.data[0], 2)) &&
		resultMatches(r2.data[1], get(res.data[1], 2)) &&
		resultMatches(r2.data[2], get(res.data[2], 2)) &&
		resultMatches(r3.data[0], get(res.data[0], 3)) &&
		resultMatches(r3.data[1], get(res.data[1], 3)) &&
		resultMatches(r3.data[2], get(res.data[2], 3));
}
inline bool resultMatches(const Arr& r0, const Arr& r1, const Arr& r2, const Arr& r3, const Arr_pkt& res) {
	return resultMatches(r0[0], get(res[0], 0)) &&
		resultMatches(r0[1], get(res[1], 0)) &&
		resultMatches(r0[2], get(res[2], 0)) &&
		resultMatches(r1[0], get(res[0], 1)) &&
		resultMatches(r1[1], get(res[1], 1)) &&
		resultMatches(r1[2], get(res[2], 1)) &&
		resultMatches(r2[0], get(res[0], 2)) &&
		resultMatches(r2[1], get(res[1], 2)) &&
		resultMatches(r2[2], get(res[2], 2)) &&
		resultMatches(r3[0], get(res[0], 3)) &&
		resultMatches(r3[1], get(res[1], 3)) &&
		resultMatches(r3[2], get(res[2], 3));
}
inline bool resultMatches(ArrI8 a, ArrI8 b) {
	return resultMatches(a[0], b[0]) &&
		resultMatches(a[1], b[1]) &&
		resultMatches(a[2], b[2]) &&
		resultMatches(a[3], b[3]) &&
		resultMatches(a[4], b[4]) &&
		resultMatches(a[5], b[5]) &&
		resultMatches(a[6], b[6]) &&
		resultMatches(a[7], b[7]);
}
inline bool resultMatches(const ArrI8& r0, const ArrI8& r1, const ArrI8& r2, const ArrI8& r3, const ArrI8_pkt& res) {
	return resultMatches(r0[0], get(res[0], 0)) &&
		resultMatches(r0[1], get(res[1], 0)) &&
		resultMatches(r0[2], get(res[2], 0)) &&
		resultMatches(r0[3], get(res[3], 0)) &&
		resultMatches(r0[4], get(res[4], 0)) &&
		resultMatches(r0[5], get(res[5], 0)) &&
		resultMatches(r0[6], get(res[6], 0)) &&
		resultMatches(r0[7], get(res[7], 0)) &&

		resultMatches(r1[0], get(res[0], 1)) &&
		resultMatches(r1[1], get(res[1], 1)) &&
		resultMatches(r1[2], get(res[2], 1)) &&
		resultMatches(r1[3], get(res[3], 1)) &&
		resultMatches(r1[4], get(res[4], 1)) &&
		resultMatches(r1[5], get(res[5], 1)) &&
		resultMatches(r1[6], get(res[6], 1)) &&
		resultMatches(r1[7], get(res[7], 1)) &&

		resultMatches(r2[0], get(res[0], 2)) &&
		resultMatches(r2[1], get(res[1], 2)) &&
		resultMatches(r2[2], get(res[2], 2)) &&
		resultMatches(r2[3], get(res[3], 2)) &&
		resultMatches(r2[4], get(res[4], 2)) &&
		resultMatches(r2[5], get(res[5], 2)) &&
		resultMatches(r2[6], get(res[6], 2)) &&
		resultMatches(r2[7], get(res[7], 2)) &&

		resultMatches(r3[0], get(res[0], 3)) &&
		resultMatches(r3[1], get(res[1], 3)) &&
		resultMatches(r3[2], get(res[2], 3)) &&
		resultMatches(r3[3], get(res[3], 3)) &&
		resultMatches(r3[4], get(res[4], 3)) &&
		resultMatches(r3[5], get(res[5], 3)) &&
		resultMatches(r3[6], get(res[6], 3)) &&
		resultMatches(r3[7], get(res[7], 3));
}

inline bool resultMatches(const StrI3& a, const StrI3& b) {
	return resultMatches(a.f, b.f) &&
			resultMatches(*(a.g), *(b.g)) &&
			resultMatches(a.h, b.h);
}

inline bool resultMatches(const StrI3& r0, const StrI3& r1, const StrI3& r2, const StrI3& r3, const StrI3_pkt& res) {
	return resultMatches(r0.f, r1.f, r2.f, r3.f, res.f) &&
			resultMatches(*(r0.g), *(r1.g), *(r2.g), *(r3.g), *(res.g)) &&
			resultMatches(r0.h, r1.h, r2.h, r3.h, res.h);
}




//----------------------------------------------------------------------------//
// test cases
// 
// each case consists of the following:
// - scalar function implementation
// - declaration of packet prototype
// - execute() which sets up data and runs tests
//----------------------------------------------------------------------------//
#define TESTSOURCE extern "C" __attribute__((noinline))
#define TESTTARGET extern "C"

#if 1
TESTSOURCE void test01(float a, float b, Vec3f* r) {
	r->x += a+b;
    r->y = r->x * r->z;
}
TESTTARGET void test01_pkt(VEC a, VEC b, Vec3f_pkt* r) ALIGN;
bool execute01(INPUTDATA) {
	SETUPSCALARSTRUCTS;

	test01(f0, f4, &r0);
	test01(f1, f5, &r1);
	test01(f2, f6, &r2);
	test01(f3, f7, &r3);

	SETUPPACKETSTRUCTS;

	test01_pkt(a, b, &res);

	return resultMatches(r0, r1, r2, r3, res);
}

TESTSOURCE void test02(Vec3f* a, Vec3f* b, Vec3f* r) {
	float x = a->x + b->x;
	float y = a->y + b->y;
	float z = a->z + b->z;
	float w = x * y * z;
	r->x = w;
	r->y = w;
	r->z = w;
}
TESTTARGET void test02_pkt(Vec3f_pkt* a, Vec3f_pkt* b, Vec3f_pkt* r) ALIGN;
bool execute02(INPUTDATA) {
	SETUPSCALARSTRUCTS;
	
	test02(&a0, &b0, &r0);
	test02(&a1, &b1, &r1);
	test02(&a2, &b2, &r2);
	test02(&a3, &b3, &r3);

	SETUPPACKETSTRUCTS;

	test02_pkt(&av, &bv, &res);

	return resultMatches(r0, r1, r2, r3, res);
}

TESTSOURCE void test03(Vec3f* a, Vec3f* b, Vec3f* r) {
	float x = a->x + b->x;
	r->y += a->y + b->y;
	r->z -= a->z + b->z;

	r->x += x;
    for (unsigned i=0; i<100; ++i) {
        r->x += a->z * i + r->y;
    }
    float w = r->x * r->z;

	r->x += w;
	r->y *= w;
	r->z /= w;
}
TESTTARGET void test03_pkt(Vec3f_pkt* a, Vec3f_pkt* b, Vec3f_pkt* r) ALIGN;
bool execute03(INPUTDATA) {
	SETUPSCALARSTRUCTS;

	test03(&a0, &b0, &r0);
	test03(&a1, &b1, &r1);
	test03(&a2, &b2, &r2);
	test03(&a3, &b3, &r3);

	SETUPPACKETSTRUCTS;

	test03_pkt(&av, &bv, &res);

	return resultMatches(r0, r1, r2, r3, res);
}

TESTSOURCE void test04(Vec3f* a, Vec3f* b, Vec3f* r) {
	float x = a->x + b->x;
	r->y += a->y + b->y;
	r->z -= a->z + b->z;

	static int px[] = { 3,160,-15,91,131,13,201,95,96,-53,0,233,7,225,-140,36 };

    const int A = (int)floorf(x) & 16;
    const float U = x * (x * 6 - 15) + 10;
    const int FA = px[A  ]+A;
    const int FC = px[A+1]+A;
    r->x += FA-FC * U;
    float w = r->x * r->z;

	r->x += w;
	r->y *= w;
	r->z /= w;
}
TESTTARGET void test04_pkt(Vec3f_pkt* a, Vec3f_pkt* b, Vec3f_pkt* r) ALIGN;
bool execute04(INPUTDATA) {
	SETUPSCALARSTRUCTS;

	test04(&a0, &b0, &r0);
	test04(&a1, &b1, &r1);
	test04(&a2, &b2, &r2);
	test04(&a3, &b3, &r3);

	SETUPPACKETSTRUCTS;

	test04_pkt(&av, &bv, &res);

	return resultMatches(r0, r1, r2, r3, res);
} 

// array test
TESTSOURCE void test05(Vec3fA* a, Vec3fA* b, Vec3fA* r) {
	Vec3fA res;
	res.data[0] = a->data[0] + b->data[0];
	res.data[1] = a->data[1] + b->data[1];
	res.data[2] = a->data[2] + b->data[2];
	*r = res;
}
TESTTARGET void test05_pkt(Vec3fA_pkt* a, Vec3fA_pkt* b, Vec3fA_pkt* r) ALIGN;
bool execute05(INPUTDATA) {
	SETUPSCALARARRAYS;

	test05(&a0, &b0, &r0);
	test05(&a1, &b1, &r1);
	test05(&a2, &b2, &r2);
	test05(&a3, &b3, &r3);

	SETUPPACKETARRAYS;

	test05_pkt(&av, &bv, &res);

	return resultMatches(r0, r1, r2, r3, res);
}

TESTSOURCE void test06(Arr* a, Arr* b, Arr* r) {
	(*r)[0] = (*a)[0] + (*b)[0];
	(*r)[1] = (*a)[1] + (*b)[1];
	(*r)[2] = (*a)[2] + (*b)[2];
}
TESTTARGET void test06_pkt(Arr_pkt* a, Arr_pkt* b, Arr_pkt* r) ALIGN;
bool execute06(INPUTDATA) {
	Arr a0, a1, a2, a3, b0, b1, b2, b3, r0, r1, r2, r3;
	a0[0] = f0; a0[1] = f0; a0[2] = f0;
	a1[0] = f1; a1[1] = f1; a1[2] = f1;
	a2[0] = f2; a2[1] = f2; a2[2] = f2;
	a3[0] = f3; a3[1] = f3; a3[2] = f3;
	b0[0] = f4; b0[1] = f4; b0[2] = f4;
	b1[0] = f5; b1[1] = f5; b1[2] = f5;
	b2[0] = f6; b2[1] = f6; b2[2] = f6;
	b3[0] = f7; b3[1] = f7; b3[2] = f7;
	r0[0] = f0; r0[1] = f0; r0[2] = f0;
	r1[0] = f1; r1[1] = f1; r1[2] = f1;
	r2[0] = f2; r2[1] = f2; r2[2] = f2;
	r3[0] = f3; r3[1] = f3; r3[2] = f3;

	test06(&a0, &b0, &r0);
	test06(&a1, &b1, &r1);
	test06(&a2, &b2, &r2);
	test06(&a3, &b3, &r3);

	VEC a = _mm_set_ps(f3, f2, f1, f0);
	VEC b = _mm_set_ps(f7, f6, f5, f4);
	Arr_pkt av, bv, res;
	av[0] = a; av[1] = a; av[2] = a;
	bv[0] = b; bv[1] = b; bv[2] = b;
	res[0] = a; res[1] = a; res[2] = a;

	test06_pkt(&av, &bv, &res);

	return resultMatches(r0, r1, r2, r3, res);
}

// uniform second parameter -> uniform control flow
TESTSOURCE void test07(Arr* a, Arr* b, Arr* r) {
	float x = (*b)[0];
	float y = (*b)[1];
	if (x < y) {
		(*r)[0] = (*a)[0] + (*b)[0];
	} else {
		(*r)[0] = (*r)[2] / (*r)[1];
	}
	(*r)[1] = (*a)[1] + (*b)[1];
	(*r)[2] = (*a)[2] + (*b)[2];
}
TESTTARGET void test07_pkt(Arr_pkt* a, Arr* b, Arr_pkt* r) ALIGN;
bool execute07(INPUTDATA) {
	Arr a0, a1, a2, a3, b0, b1, b2, b3, r0, r1, r2, r3;
	a0[0] = f0; a0[1] = f0; a0[2] = f0;
	a1[0] = f1; a1[1] = f1; a1[2] = f1;
	a2[0] = f2; a2[1] = f2; a2[2] = f2;
	a3[0] = f3; a3[1] = f3; a3[2] = f3;
	b0[0] = f4; b0[1] = f4; b0[2] = f4;
	b1[0] = f5; b1[1] = f5; b1[2] = f5;
	b2[0] = f6; b2[1] = f6; b2[2] = f6;
	b3[0] = f7; b3[1] = f7; b3[2] = f7;
	r0[0] = f0; r0[1] = f0; r0[2] = f0;
	r1[0] = f1; r1[1] = f1; r1[2] = f1;
	r2[0] = f2; r2[1] = f2; r2[2] = f2;
	r3[0] = f3; r3[1] = f3; r3[2] = f3;

	test07(&a0, &b0, &r0);
	test07(&a1, &b0, &r1);
	test07(&a2, &b0, &r2);
	test07(&a3, &b0, &r3);

	VEC a = _mm_set_ps(f3, f2, f1, f0);
	VEC b = _mm_set_ps(f7, f6, f5, f4);
	Arr_pkt av, bv, res;
	av[0] = a; av[1] = a; av[2] = a;
	bv[0] = b; bv[1] = b; bv[2] = b;
	res[0] = a; res[1] = a; res[2] = a;

	test07_pkt(&av, &b0, &res);

	return resultMatches(r0, r1, r2, r3, res);
}

// use scalar arrays with varying index
// -> splits everything -> safe :)
TESTSOURCE void test08(int* output, int* input, int varyingInX, int uniformInX)
{
    int a = varyingInX%uniformInX;
    int b = varyingInX/uniformInX;
	int p = a*b > 7 ? 7 : a*b;

    output[varyingInX] = input[p];
}
TESTTARGET void test08_pkt(int* output, int* input, VECI varyingInX, int uniformInX) ALIGN;
bool execute08(INPUTDATA) {
	ArrI8 in = { (int)f0, (int)f1, (int)f2, (int)f3, (int)f4, (int)f5, (int)f6, (int)f7 };

	ArrI8 out;

	for (int i=0; i<8; ++i) {
		test08(out, in, i, 3);
	}

	ArrI8 out_pkt;
	VECI i0 = _mm_set_epi32(3, 2, 1, 0);
	VECI i1 = _mm_set_epi32(7, 6, 5, 4);

	test08_pkt(out_pkt, in, i0, 3);
	test08_pkt(out_pkt, in, i1, 3);

	const bool x = resultMatches(out, out_pkt);
	if (!x) {
		printf("\nbad result found:\n  input:");
		for (unsigned i=0; i<8; ++i) {
			printf(" %d", in[i]);
		}
		printf("\n  expected:");
		for (unsigned i=0; i<8; ++i) {
			printf(" %d", out[i]);
		}
		printf("\n  returned:");
		for (unsigned i=0; i<8; ++i) {
			printf(" %d", out_pkt[i]);
		}
		printf("\n");
	}
	return x;
}
#endif

//
// array loading tests
//

// 1-3: loading from uniform array
TESTSOURCE void test_array_load01(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	uniformOutArr[varyingInX] = *uniformInArr;     // single GEP index, 4 times the same value (UNIFORM, INDEX_SAME) -> equivalent to uniformInArr[0]
}

TESTSOURCE void test_array_load02(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	uniformOutArr[varyingInX] = uniformInArr[uniformInX];   // single GEP index, 4 times the same value (UNIFORM, INDEX_SAME)
}

TESTSOURCE void test_array_load03(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	uniformOutArr[varyingInX] = uniformInArr[varyingInX]; // possibly 4 different GEP indices, 4 different values (VARYING, INDEX_RANDOM)
}

// 4-6: loading from varying array
TESTSOURCE void test_array_load04(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	uniformOutArr[varyingInX] = *varyingInArr;     // single GEP index, but points to 4 consecutive values (1 vector) (VARYING, INDEX_CONSECUTIVE) -> think of this as an access to an SoA array of colors (x/y/z/x/y/z/...) transformed to vectorized colors (xxxx/yyyy/zzzz/xxxx/yyyy/zzzz/...) -> equivalent to varyingInArr[0]
}

TESTSOURCE void test_array_load05(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	uniformOutArr[varyingInX] = varyingInArr[uniformInX];   // single GEP index, but points to 4 consecutive values (1 vector) (VARYING, INDEX_CONSECUTIVE)
}

TESTSOURCE void test_array_load06(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	uniformOutArr[varyingInX] = varyingInArr[varyingInX/2]; // possibly 4 different GEP index-sets, 4 different values (VARYING, INDEX_RANDOM) -> /2 because the arrays only have length 6 and varyingInX goes up to 7
}

// 7: all in one
TESTSOURCE void test_array_load07(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	// Access to uniform array
	int x1 = *uniformInArr;       // single GEP index, 4 times the same value (UNIFORM, INDEX_SAME) -> equivalent to uniformInArr[0]
	int x2 = uniformInArr[uniformInX];     // single GEP index, 4 times the same value (UNIFORM, INDEX_SAME)
	int x3 = uniformInArr[varyingInX];   // possibly 4 different GEP indices, 4 different values (VARYING, INDEX_RANDOM)

	// Access to varying array (4 times the size, VARYING = a different array for each scalar execution!)
	int x4 = *varyingInArr;       // single GEP index, but points to 4 consecutive values (1 vector) (VARYING, INDEX_CONSECUTIVE) -> think of this as an access to an SoA array of colors (x/y/z/x/y/z/...) transformed to vectorized colors (xxxx/yyyy/zzzz/xxxx/yyyy/zzzz/...) -> equivalent to varyingInArr[0]
	int x5 = varyingInArr[uniformInX];     // single GEP index, but points to 4 consecutive values (1 vector) (VARYING, INDEX_CONSECUTIVE)
	int x6 = varyingInArr[varyingInX/2]; // possibly 4 different GEP index-sets, 4 different values (VARYING, INDEX_RANDOM) -> /2 because the arrays only have length 6 and varyingInX goes up to 7

	uniformOutArr[varyingInX] = (x1 - x2 + x3) - (x4 - x5 + x6);
}

// conditional loading

// NOTE: varyingInArr[0] is varying, therefore the last load and/or a
//       corresponding pointer phi have to be split (pointer phi is created if
//       the load is pulled down to only have one load directly before the
//       store).
//       -> Control-flow is uniform, but pointers are not.

// 1-3: loading from uniform array with uniform control-flow
TESTSOURCE void test_array_cload01(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		uniformOutArr[varyingInX] = *uniformInArr;
	}
}

TESTSOURCE void test_array_cload02(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		uniformOutArr[varyingInX] = uniformInArr[uniformInX];
	}
}

TESTSOURCE void test_array_cload03(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		uniformOutArr[varyingInX] = uniformInArr[varyingInX];
	}
}

// 4-6: loading from uniform array with varying control-flow
TESTSOURCE void test_array_cload04(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		uniformOutArr[varyingInX] = *uniformInArr;
	}
}

TESTSOURCE void test_array_cload05(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		uniformOutArr[varyingInX] = uniformInArr[uniformInX];
	}
}

TESTSOURCE void test_array_cload06(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		uniformOutArr[varyingInX] = uniformInArr[varyingInX];
	}
}

// 7-9: loading from varying array with uniform control-flow
TESTSOURCE void test_array_cload07(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		uniformOutArr[varyingInX] = *varyingInArr;
	}
}

TESTSOURCE void test_array_cload08(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		uniformOutArr[varyingInX] = varyingInArr[uniformInX];
	}
}

TESTSOURCE void test_array_cload09(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		uniformOutArr[varyingInX] = varyingInArr[varyingInX];
	}
}

// 10-12: loading from varying array with varying control-flow
TESTSOURCE void test_array_cload10(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		uniformOutArr[varyingInX] = *varyingInArr;
	}
}

TESTSOURCE void test_array_cload11(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		uniformOutArr[varyingInX] = varyingInArr[uniformInX];
	}
}

TESTSOURCE void test_array_cload12(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		uniformOutArr[varyingInX] = varyingInArr[varyingInX];
	}
}



//
// array storing tests
//

// 1-4: storing to UNIFORM array
// No tests for the following cases:
//*uniformOutArr = varyingInX; // INDEX_SAME pointer, INDEX_RANDOM value -> race condition! (4 different values, same array)
//uniformOutArr[uniformInX] = varyingInX; // INDEX_SAME pointer, INDEX_RANDOM value -> race condition! (4 different values, same array)
TESTSOURCE void test_array_store01(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	*uniformOutArr = uniformInX;     // INDEX_SAME pointer, INDEX_SAME value
}

TESTSOURCE void test_array_store02(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	uniformOutArr[uniformInX] = uniformInX;     // INDEX_SAME pointer, INDEX_SAME value
}

TESTSOURCE void test_array_store03(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	uniformOutArr[varyingInX] = uniformInX; // INDEX_RANDOM pointer, INDEX_SAME value
}

TESTSOURCE void test_array_store04(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	uniformOutArr[varyingInX] = varyingInX; // INDEX_RANDOM pointer, INDEX_RANDOM value
}

// 5-10: storing to VARYING array
TESTSOURCE void test_array_store05(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	*varyingOutArr = uniformInX;   // INDEX_RANDOM pointer, INDEX_SAME value
}

TESTSOURCE void test_array_store06(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	*varyingOutArr = varyingInX;   // INDEX_RANDOM pointer, INDEX_RANDOM value -> no race condition (4 different values, 4 different arrays)
}

TESTSOURCE void test_array_store07(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	varyingOutArr[uniformInX] = uniformInX;   // INDEX_RANDOM pointer, INDEX_SAME value
}

TESTSOURCE void test_array_store08(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	varyingOutArr[uniformInX] = varyingInX;   // INDEX_RANDOM pointer, INDEX_RANDOM value
}

TESTSOURCE void test_array_store09(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	varyingOutArr[varyingInX] = uniformInX;   // INDEX_RANDOM pointer, INDEX_SAME value
}

TESTSOURCE void test_array_store10(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	varyingOutArr[varyingInX] = varyingInX;   // INDEX_RANDOM pointer, INDEX_RANDOM value
}

// conditional stores

// 1-6: storing to uniform array with uniform control-flow
// NOTE: cases 2 and 4 are ALWAYS race conditions (correctly analyzed by packetizer)!
TESTSOURCE void test_array_cstore01(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		*uniformOutArr = uniformInX; // INDEX_SAME pointer, INDEX_SAME value
	}
}

TESTSOURCE void test_array_cstore02(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		*uniformOutArr = varyingInX; // INDEX_SAME pointer, INDEX_RANDOM value -> race condition!
	}
}

TESTSOURCE void test_array_cstore03(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		uniformOutArr[uniformInX] = uniformInX; // INDEX_SAME pointer, INDEX_SAME value
	}
}

TESTSOURCE void test_array_cstore04(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		uniformOutArr[uniformInX] = varyingInX; // INDEX_SAME pointer, INDEX_RANDOM value -> race condition!
	}
}

TESTSOURCE void test_array_cstore05(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		uniformOutArr[varyingInX] = uniformInX; // INDEX_RANDOM pointer, INDEX_SAME value
	}
}

TESTSOURCE void test_array_cstore06(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		uniformOutArr[varyingInX] = varyingInX; // INDEX_RANDOM pointer, INDEX_RANDOM value
	}
}

// 7-12: storing to uniform array with varying control-flow
// NOTE: Cases 8 and 10 are ALWAYS race conditions (correctly analyzed by packetizer)!
// NOTE: Cases 7 and 9 are also race conditions, but they do not have a visible effect as long as masking (if-cascade) is correct.
TESTSOURCE void test_array_cstore07(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		*uniformOutArr = uniformInX; // INDEX_SAME pointer, INDEX_SAME value, but VARYING control-flow -> race condition without effect!
	}
}

TESTSOURCE void test_array_cstore08(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		*uniformOutArr = varyingInX; // INDEX_SAME pointer, INDEX_RANDOM value -> race condition!
	}
}

TESTSOURCE void test_array_cstore09(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		uniformOutArr[uniformInX] = uniformInX; // INDEX_SAME pointer, INDEX_SAME value, but VARYING control-flow -> race condition without effect!
	}
}

TESTSOURCE void test_array_cstore10(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		uniformOutArr[uniformInX] = varyingInX; // INDEX_SAME pointer, INDEX_RANDOM value -> race condition!
	}
}

TESTSOURCE void test_array_cstore11(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		uniformOutArr[varyingInX] = uniformInX; // INDEX_RANDOM pointer, INDEX_SAME value
	}
}

TESTSOURCE void test_array_cstore12(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		uniformOutArr[varyingInX] = varyingInX; // INDEX_RANDOM pointer, INDEX_RANDOM value
	}
}

// 13-18: storing to varying array with uniform control-flow
TESTSOURCE void test_array_cstore13(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		*varyingOutArr = uniformInX; // INDEX_CONSECUTIVE pointer, INDEX_SAME value
	}
}

TESTSOURCE void test_array_cstore14(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		*varyingOutArr = varyingInX; // INDEX_CONSECUTIVE pointer, INDEX_RANDOM value
	}
}

TESTSOURCE void test_array_cstore15(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		varyingOutArr[uniformInX] = uniformInX; // INDEX_CONSECUTIVE pointer, INDEX_SAME value
	}
}

TESTSOURCE void test_array_cstore16(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		varyingOutArr[uniformInX] = varyingInX; // INDEX_CONSECUTIVE pointer, INDEX_RANDOM value
	}
}

TESTSOURCE void test_array_cstore17(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		varyingOutArr[varyingInX] = uniformInX; // INDEX_RANDOM pointer, INDEX_SAME value
	}
}

TESTSOURCE void test_array_cstore18(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (uniformInArr[0] > 0) {
		varyingOutArr[varyingInX] = varyingInX; // INDEX_RANDOM pointer, INDEX_RANDOM value
	}
}

// 19-24: storing to varying array with varying control-flow
TESTSOURCE void test_array_cstore19(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		*varyingOutArr = uniformInX; // INDEX_CONSECUTIVE pointer, INDEX_SAME value
	}
}

TESTSOURCE void test_array_cstore20(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		*varyingOutArr = varyingInX; // INDEX_CONSECUTIVE pointer, INDEX_RANDOM value
	}
}

TESTSOURCE void test_array_cstore21(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		varyingOutArr[uniformInX] = uniformInX; // INDEX_CONSECUTIVE pointer, INDEX_SAME value
	}
}

TESTSOURCE void test_array_cstore22(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		varyingOutArr[uniformInX] = varyingInX; // INDEX_CONSECUTIVE pointer, INDEX_RANDOM value
	}
}

TESTSOURCE void test_array_cstore23(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		varyingOutArr[varyingInX] = uniformInX; // INDEX_RANDOM pointer, INDEX_SAME value
	}
}

TESTSOURCE void test_array_cstore24(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 0) {
		varyingOutArr[varyingInX] = varyingInX; // INDEX_RANDOM pointer, INDEX_RANDOM value
	}
}

//
// Various array stuff, e.g. pointer selects
//

// 1: loading from uniform array with uniform control-flow
TESTSOURCE void test_array_extra01(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	int x1 = 3;
	if (uniformInArr[0] > 0) {
		x1 = *uniformInArr;
	} else if (uniformInArr[0] < 0) {
		x1 = uniformInArr[uniformInX];
	} else {
		x1 = uniformInArr[varyingInX];
	}
	uniformOutArr[varyingInX] = x1;
}

// 2: loading from uniform array with varying control-flow
TESTSOURCE void test_array_extra02(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	int x1 = 3;
	if (varyingInArr[0] > 0) {
		x1 = *uniformInArr;
	} else if (varyingInArr[0] < 0) {
		x1 = uniformInArr[uniformInX];
	} else {
		x1 = uniformInArr[varyingInX];
	}
	uniformOutArr[varyingInX] = x1;
}

// 3: loading from varying array with uniform control-flow
TESTSOURCE void test_array_extra03(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	int x1 = 3;
	if (uniformInArr[0] > 0) {
		x1 = *varyingInArr;
	} else if (uniformInArr[0] < 0) {
		x1 = varyingInArr[uniformInX];
	} else {
		x1 = varyingInArr[varyingInX];
	}
	uniformOutArr[varyingInX] = x1;
}

// 4: loading from varying array with varying control-flow
TESTSOURCE void test_array_extra04(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	int x1 = 3;
	if (varyingInArr[0] > 0) {
		x1 = *varyingInArr;
	} else if (varyingInArr[0] < 0) {
		x1 = varyingInArr[uniformInX];
	} else {
		x1 = varyingInArr[varyingInX];
	}
	uniformOutArr[varyingInX] = x1;
}

TESTSOURCE void test_array_extra05(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 10) {
		varyingOutArr[varyingInX] = uniformInArr[uniformInX];
	} else if (varyingInArr[0] < -10) {
		varyingOutArr[varyingInX] = uniformInArr[varyingInX];
	} else if (varyingInArr[0] > 0) {
		varyingOutArr[varyingInX] = varyingInArr[uniformInX];
	} else {
		varyingOutArr[varyingInX] = varyingInArr[varyingInX];
	}
}

TESTSOURCE void test_array_extra06(int* uniformOutArr, int* varyingOutArr, int* uniformInArr, int* varyingInArr, int varyingInX, int uniformInX)
{
	if (varyingInArr[0] > 10) {
		varyingOutArr[varyingInX] = uniformInArr[uniformInX];
	} else if (varyingInArr[0] < 0) {
		if (uniformInArr[0] > 0) {
			varyingOutArr[varyingInX] = uniformInArr[varyingInX];
		} else {
			varyingOutArr[uniformInX] = varyingInArr[uniformInX];
		}
	} else {
		if (uniformInArr[0] > 0) {
			varyingOutArr[uniformInX] = uniformInArr[varyingInX];
		} else {
			varyingOutArr[varyingInX] = varyingInArr[uniformInX];
		}
	}
}


//
// struct loading tests
//

// 1-3: loading from uniform struct
TESTSOURCE void test_struct_load01(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	uniformOutArr[varyingInX] = uniformInStr.f; // UNIFORM pointer/load -> scalar load + broadcast
}

TESTSOURCE void test_struct_load02(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	uniformOutArr[varyingInX] = uniformInStr.h; // UNIFORM pointer/load -> scalar load + broadcast
}

TESTSOURCE void test_struct_load03(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	uniformOutArr[varyingInX] = *(uniformInStr.g); // UNIFORM pointer-to-pointer/load/load -> 2x scalar load + broadcast
}

// 4-6: loading from varying struct
TESTSOURCE void test_struct_load04(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	uniformOutArr[varyingInX] = varyingInStr.f; // INDEX_CONSECUTIVE pointer -> vector load
}

TESTSOURCE void test_struct_load05(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	uniformOutArr[varyingInX] = varyingInStr.h; // INDEX_CONSECUTIVE pointer -> vector load
}

TESTSOURCE void test_struct_load06(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	uniformOutArr[varyingInX] = *(varyingInStr.g);
}

// conditional loading

// 1-3: loading from uniform struct with uniform control-flow
TESTSOURCE void test_struct_cload01(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		uniformOutArr[varyingInX] = uniformInStr.f;
	}
}

TESTSOURCE void test_struct_cload02(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		uniformOutArr[varyingInX] = uniformInStr.h;
	}
}

TESTSOURCE void test_struct_cload03(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		uniformOutArr[varyingInX] = *(uniformInStr.g);
	}
}

// 4-6: loading from uniform struct with varying control-flow
TESTSOURCE void test_struct_cload04(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		uniformOutArr[varyingInX] = uniformInStr.f;
	}
}

TESTSOURCE void test_struct_cload05(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		uniformOutArr[varyingInX] = uniformInStr.h;
	}
}

TESTSOURCE void test_struct_cload06(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		uniformOutArr[varyingInX] = *(uniformInStr.g);
	}
}

// 7-9: loading from varying struct with uniform control-flow
TESTSOURCE void test_struct_cload07(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		uniformOutArr[varyingInX] = varyingInStr.f;
	}
}

TESTSOURCE void test_struct_cload08(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		uniformOutArr[varyingInX] = varyingInStr.h;
	}
}

TESTSOURCE void test_struct_cload09(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		uniformOutArr[varyingInX] = *(varyingInStr.g);
	}
}

// 10-12: loading from varying struct with varying control-flow
TESTSOURCE void test_struct_cload10(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		uniformOutArr[varyingInX] = varyingInStr.f;
	}
}

TESTSOURCE void test_struct_cload11(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		uniformOutArr[varyingInX] = varyingInStr.h;
	}
}

TESTSOURCE void test_struct_cload12(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		uniformOutArr[varyingInX] = *(varyingInStr.g);
	}
}


//
// struct storing tests
//

// 1-4: storing to UNIFORM struct
// No tests for the following case:
// uniformOutStr->f/g/h = varyingInX; // INDEX_SAME pointer, INDEX_RANDOM value -> race condition! (4 different values, same struct)
// *uniformOutStr = varyingInStr; // INDEX_SAME pointer, INDEX_RANDOM value -> race condition! (4 different values, same struct)
TESTSOURCE void test_struct_store01(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	uniformOutStr->f = uniformInX;
}

TESTSOURCE void test_struct_store02(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	uniformOutStr->h = uniformInX;
}

TESTSOURCE void test_struct_store03(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	*(uniformOutStr->g) = uniformInX;
}

TESTSOURCE void test_struct_store04(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	*uniformOutStr = uniformInStr; // clang transforms this into a memcpy
}

// 5-12: storing to VARYING struct
TESTSOURCE void test_struct_store05(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	varyingOutStr->f = uniformInX;
}

TESTSOURCE void test_struct_store06(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	varyingOutStr->h = uniformInX;
}

TESTSOURCE void test_struct_store07(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	*(varyingOutStr->g) = uniformInX;
}

TESTSOURCE void test_struct_store08(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	*varyingOutStr = uniformInStr; // clang transforms this into a memcpy
}

TESTSOURCE void test_struct_store09(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	varyingOutStr->f = varyingInX;
}

TESTSOURCE void test_struct_store10(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	*(varyingOutStr->g) = varyingInX;
}

TESTSOURCE void test_struct_store11(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	varyingOutStr->h = varyingInX;
}

TESTSOURCE void test_struct_store12(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	*varyingOutStr = varyingInStr; // clang transforms this into a memcpy
}

// conditional storing

// 1-6: storing to uniform struct with uniform control-flow
TESTSOURCE void test_struct_cstore01(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		uniformOutStr->f = uniformInX;
	}
}

TESTSOURCE void test_struct_cstore02(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		uniformOutStr->h = uniformInX;
	}
}

TESTSOURCE void test_struct_cstore03(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		*(uniformOutStr->g) = uniformInX;
	}
}

TESTSOURCE void test_struct_cstore04(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		uniformOutStr->f = varyingInX; // race condition!
	}
}

TESTSOURCE void test_struct_cstore05(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		uniformOutStr->h = varyingInX; // race condition!
	}
}

TESTSOURCE void test_struct_cstore06(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		*(uniformOutStr->g) = varyingInX; // race condition!
	}
}

// 7-12: storing to varying struct with uniform control-flow
TESTSOURCE void test_struct_cstore07(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		varyingOutStr->f = uniformInX;
	}
}

TESTSOURCE void test_struct_cstore08(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		varyingOutStr->h = uniformInX;
	}
}

TESTSOURCE void test_struct_cstore09(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		*(varyingOutStr->g) = uniformInX;
	}
}

TESTSOURCE void test_struct_cstore10(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		varyingOutStr->f = varyingInX;
	}
}

TESTSOURCE void test_struct_cstore11(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		varyingOutStr->h = varyingInX;
	}
}

TESTSOURCE void test_struct_cstore12(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		*(varyingOutStr->g) = varyingInX;
	}
}

// 13-18: storing to uniform struct with varying control-flow
TESTSOURCE void test_struct_cstore13(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		uniformOutStr->f = uniformInX;
	}
}

TESTSOURCE void test_struct_cstore14(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		uniformOutStr->h = uniformInX;
	}
}

TESTSOURCE void test_struct_cstore15(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		*(uniformOutStr->g) = uniformInX;
	}
}

TESTSOURCE void test_struct_cstore16(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		uniformOutStr->f = varyingInX; // race condition!
	}
}

TESTSOURCE void test_struct_cstore17(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		uniformOutStr->h = varyingInX; // race condition!
	}
}

TESTSOURCE void test_struct_cstore18(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		*(uniformOutStr->g) = varyingInX; // race condition!
	}
}

// 19-24: storing to varying struct with varying control-flow
TESTSOURCE void test_struct_cstore19(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		varyingOutStr->f = uniformInX;
	}
}

TESTSOURCE void test_struct_cstore20(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		varyingOutStr->h = uniformInX;
	}
}

TESTSOURCE void test_struct_cstore21(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		*(varyingOutStr->g) = uniformInX;
	}
}

TESTSOURCE void test_struct_cstore22(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		varyingOutStr->f = varyingInX;
	}
}

TESTSOURCE void test_struct_cstore23(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		varyingOutStr->h = varyingInX;
	}
}

TESTSOURCE void test_struct_cstore24(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		*(varyingOutStr->g) = varyingInX;
	}
}

//
// Various struct stuff, e.g. pointer selects
//

// 1: loading from uniform struct with uniform control-flow
TESTSOURCE void test_struct_extra01(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	int x1 = 3;
	if (uniformInStr.f > 0) {
		x1 = uniformInStr.f;
	} else if (uniformInStr.f < 0) {
		x1 = *(uniformInStr.g);
	} else {
		x1 = uniformInStr.h;
	}
	uniformOutArr[varyingInX] = x1;
}

// 2: loading from uniform struct with varying control-flow
TESTSOURCE void test_struct_extra02(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	int x1 = 3;
	if (varyingInStr.f > 0) {
		x1 = uniformInStr.f;
	} else if (varyingInStr.f < 0) {
		x1 = *(uniformInStr.g);
	} else {
		x1 = uniformInStr.h;
	}
	uniformOutArr[varyingInX] = x1;
}

// 3: loading from varying struct with uniform control-flow
TESTSOURCE void test_struct_extra03(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	int x1 = 3;
	if (uniformInStr.f > 0) {
		x1 = varyingInStr.f;
	} else if (uniformInStr.f < 0) {
		x1 = *(varyingInStr.g);
	} else {
		x1 = varyingInStr.h;
	}
	uniformOutArr[varyingInX] = x1;
}

// 4: loading from varying struct with varying control-flow
TESTSOURCE void test_struct_extra04(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	int x1 = 3;
	if (varyingInStr.f > 0) {
		x1 = varyingInStr.f;
	} else if (varyingInStr.f < 0) {
		x1 = *(varyingInStr.g);
	} else {
		x1 = varyingInStr.h;
	}
	uniformOutArr[varyingInX] = x1;
}

TESTSOURCE void test_struct_extra05(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (uniformInStr.f > 0) {
		*uniformOutStr = uniformInStr;
	//} else if (uniformInStr.f < 0) {
		//*uniformOutStr = varyingInStr; // INDEX_SAME pointer, INDEX_RANDOM value -> race condition!
	} else {
		StrI3 t;
		t.f = 3;
		t.g = uniformInStr.g;
		t.h = 7;
		*uniformOutStr = t;
	}
}

TESTSOURCE void test_struct_extra06(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		*uniformOutStr = uniformInStr;
	//} else if (varyingInStr.f < 0) {
		//*uniformOutStr = varyingInStr; // INDEX_SAME pointer, INDEX_RANDOM value -> race condition!
	} else {
		StrI3 t;
		t.f = 3;
		t.g = uniformInStr.g;
		t.h = 7;
		*uniformOutStr = t;
	}
}

TESTSOURCE void test_struct_extra07(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{
	if (varyingInStr.f > 0) {
		*varyingOutStr = uniformInStr;
	} else if (varyingInStr.f < 0) {
		*varyingOutStr = varyingInStr;
	} else {
		StrI3 t;
		t.f = 3;
		t.g = uniformInStr.g;
		t.h = 7;
		*varyingOutStr = t;
	}
}

TESTSOURCE void test_struct_extra08(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{

}

TESTSOURCE void test_struct_extra09(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{

}

TESTSOURCE void test_struct_extra10(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3 uniformInStr, StrI3 varyingInStr, int varyingInX, int uniformInX)
{

}




#if 0
//
// array of struct loading tests
//

TESTSOURCE void test_aos_load01(int* uniformOutArr, StrI3* uniformOutStr, StrI3* varyingOutStr, StrI3* uniformIn, StrI3* varyingIn, int varyingInX, int uniformInX)
{
	// Access to uniform array of structs
	int x1 = (*uniformIn).f;      // UNIFORM, INDEX_SAME -> equivalent to tt[0].f
	int x2 = uniformIn[uniformInX].f;      // UNIFORM, INDEX_SAME
	int x3 = uniformIn[varyingInX].f;    // VARYING, INDEX_RANDOM
	int x4 = uniformIn[uniformInX].h;      // UNIFORM, INDEX_SAME -> access to third element
	//int z0 = *(uniformIn[varyingInX].g); // VARYING, INDEX_RANDOM -> access to pointer (multiple indirection)

	// Access to pointer to varying struct (4 times the size, VARYING = a different struct for each scalar execution!)
	int x5 = varyingIn->f;    // VARYING, INDEX_CONSECUTIVE
	int x6 = varyingIn->h;    // VARYING, INDEX_CONSECUTIVE -> access to third element
	//int z1 = *(varyingIn->g); // access to pointer (multiple indirection)

	uniformOutArr[varyingInX] = (x1 - x2 + x3 * x4) - (x5 + x6);
}
TESTTARGET void test_aos_load01_pkt(int* output, VECI varyingInX, int uniformInX, StrI3* tt, StrI3_pkt* tt2) ALIGN;
bool test_aos_load01_execute(INPUTDATA) {

	// Uniform array of structs, same for scalar and packet function
	StrI3 tts[8];
	tts[0].f = (int)f0; tts[0].g = (int*)&f0;
	tts[1].f = (int)f5; tts[1].g = (int*)&f7;
	tts[2].f = (int)f4; tts[2].g = (int*)&f2;
	tts[3].f = (int)f7; tts[3].g = (int*)&f3;
	tts[4].f = (int)f1; tts[4].g = (int*)&f6;
	tts[5].f = (int)f6; tts[5].g = (int*)&f1;
	tts[6].f = (int)f3; tts[6].g = (int*)&f4;
	tts[7].f = (int)f2; tts[7].g = (int*)&f5;

	// Varying structs, different for each call, supplied via pointer
	StrI3 tt0; tt0.f = (int)f0; tt0.g = (int*)&f0; tt0.h = (int)f7;
	StrI3 tt1; tt1.f = (int)f1; tt1.g = (int*)&f1; tt1.h = (int)f6;
	StrI3 tt2; tt2.f = (int)f3; tt2.g = (int*)&f2; tt2.h = (int)f4;
	StrI3 tt3; tt3.f = (int)f5; tt3.g = (int*)&f3; tt3.h = (int)f1;
	StrI3 tt4; tt4.f = (int)f6; tt4.g = (int*)&f4; tt4.h = (int)f5;
	StrI3 tt5; tt5.f = (int)f2; tt5.g = (int*)&f5; tt5.h = (int)f0;
	StrI3 tt6; tt6.f = (int)f7; tt6.g = (int*)&f6; tt6.h = (int)f2;
	StrI3 tt7; tt7.f = (int)f4; tt7.g = (int*)&f7; tt7.h = (int)f3;

	// Vectorized structs (equivalent to tt0-tt7)
	VECI gg4_0 = _mm_set_epi32(f3, f2, f1, f0);
	VECI gg4_1 = _mm_set_epi32(f7, f6, f5, f4);
	StrI3_pkt tt_4_0; tt_4_0.f = _mm_set_epi32(f5, f3, f1, f0); tt_4_0.g = &gg4_0; tt_4_0.h = _mm_set_epi32(f1, f4, f6, f7);
	StrI3_pkt tt_4_1; tt_4_1.f = _mm_set_epi32(f4, f7, f2, f6); tt_4_1.g = &gg4_1; tt_4_1.h = _mm_set_epi32(f3, f2, f0, f5);

	ArrI8 out = { 13, 13, 13, 13, 13, 13, 13, 13 };

	test_aos_load01(out, 0, 1, tts, &tt0);
	test_aos_load01(out, 1, 1, tts, &tt1);
	test_aos_load01(out, 2, 1, tts, &tt2);
	test_aos_load01(out, 3, 1, tts, &tt3);
	test_aos_load01(out, 7, 1, tts, &tt4);
	test_aos_load01(out, 4, 1, tts, &tt5);
	test_aos_load01(out, 5, 1, tts, &tt6);
	test_aos_load01(out, 6, 1, tts, &tt7);

	// Varying 'varyingInX' parameter, equivalent to 4th parameters of scalar calls
	VECI varyingInX0 = _mm_set_epi32(3, 2, 1, 0);
	VECI varyingInX1 = _mm_set_epi32(6, 5, 4, 7);

	ArrI8 out_pkt = { 13, 13, 13, 13, 13, 13, 13, 13 };

	test_aos_load01_pkt(out_pkt, varyingInX0, 1, tts, &tt_4_0);
	test_aos_load01_pkt(out_pkt, varyingInX1, 1, tts, &tt_4_1);

	return resultMatches(out, out_pkt);
}
#endif


#if 0
#endif
SETUPFULLARRAYTEST(test_array_load01, test_array_load01_pkt, test_array_load01_execute)
SETUPFULLARRAYTEST(test_array_load02, test_array_load02_pkt, test_array_load02_execute)
SETUPFULLARRAYTEST(test_array_load03, test_array_load03_pkt, test_array_load03_execute)
SETUPFULLARRAYTEST(test_array_load04, test_array_load04_pkt, test_array_load04_execute)
SETUPFULLARRAYTEST(test_array_load05, test_array_load05_pkt, test_array_load05_execute)
SETUPFULLARRAYTEST(test_array_load06, test_array_load06_pkt, test_array_load06_execute)
SETUPFULLARRAYTEST(test_array_load07, test_array_load07_pkt, test_array_load07_execute)

SETUPFULLARRAYTEST(test_array_cload01, test_array_cload01_pkt, test_array_cload01_execute)
SETUPFULLARRAYTEST(test_array_cload02, test_array_cload02_pkt, test_array_cload02_execute)
SETUPFULLARRAYTEST(test_array_cload03, test_array_cload03_pkt, test_array_cload03_execute)
SETUPFULLARRAYTEST(test_array_cload04, test_array_cload04_pkt, test_array_cload04_execute)
SETUPFULLARRAYTEST(test_array_cload05, test_array_cload05_pkt, test_array_cload05_execute)
SETUPFULLARRAYTEST(test_array_cload06, test_array_cload06_pkt, test_array_cload06_execute)
SETUPFULLARRAYTEST(test_array_cload07, test_array_cload07_pkt, test_array_cload07_execute)
SETUPFULLARRAYTEST(test_array_cload08, test_array_cload08_pkt, test_array_cload08_execute)
SETUPFULLARRAYTEST(test_array_cload09, test_array_cload09_pkt, test_array_cload09_execute)
SETUPFULLARRAYTEST(test_array_cload10, test_array_cload10_pkt, test_array_cload10_execute)
SETUPFULLARRAYTEST(test_array_cload11, test_array_cload11_pkt, test_array_cload11_execute)
SETUPFULLARRAYTEST(test_array_cload12, test_array_cload12_pkt, test_array_cload12_execute)

SETUPFULLARRAYTEST(test_array_store01, test_array_store01_pkt, test_array_store01_execute)
SETUPFULLARRAYTEST(test_array_store02, test_array_store02_pkt, test_array_store02_execute)
SETUPFULLARRAYTEST(test_array_store03, test_array_store03_pkt, test_array_store03_execute)
SETUPFULLARRAYTEST(test_array_store04, test_array_store04_pkt, test_array_store04_execute)
SETUPFULLARRAYTEST(test_array_store05, test_array_store05_pkt, test_array_store05_execute)
SETUPFULLARRAYTEST(test_array_store06, test_array_store06_pkt, test_array_store06_execute)
SETUPFULLARRAYTEST(test_array_store07, test_array_store07_pkt, test_array_store07_execute)
SETUPFULLARRAYTEST(test_array_store08, test_array_store08_pkt, test_array_store08_execute)
SETUPFULLARRAYTEST(test_array_store09, test_array_store09_pkt, test_array_store09_execute)
SETUPFULLARRAYTEST(test_array_store10, test_array_store10_pkt, test_array_store10_execute)

SETUPFULLARRAYTEST(test_array_cstore01, test_array_cstore01_pkt, test_array_cstore01_execute)
//SETUPFULLARRAYTEST(test_array_cstore02, test_array_cstore02_pkt, test_array_cstore02_execute) // race condition
SETUPFULLARRAYTEST(test_array_cstore03, test_array_cstore03_pkt, test_array_cstore03_execute)
//SETUPFULLARRAYTEST(test_array_cstore04, test_array_cstore04_pkt, test_array_cstore04_execute) // race condition
SETUPFULLARRAYTEST(test_array_cstore05, test_array_cstore05_pkt, test_array_cstore05_execute)
SETUPFULLARRAYTEST(test_array_cstore06, test_array_cstore06_pkt, test_array_cstore06_execute)
SETUPFULLARRAYTEST(test_array_cstore07, test_array_cstore07_pkt, test_array_cstore07_execute)
//SETUPFULLARRAYTEST(test_array_cstore08, test_array_cstore08_pkt, test_array_cstore08_execute) // race condition
SETUPFULLARRAYTEST(test_array_cstore09, test_array_cstore09_pkt, test_array_cstore09_execute)
//SETUPFULLARRAYTEST(test_array_cstore10, test_array_cstore10_pkt, test_array_cstore10_execute) // race condition
SETUPFULLARRAYTEST(test_array_cstore11, test_array_cstore11_pkt, test_array_cstore11_execute)
SETUPFULLARRAYTEST(test_array_cstore12, test_array_cstore12_pkt, test_array_cstore12_execute)
SETUPFULLARRAYTEST(test_array_cstore13, test_array_cstore13_pkt, test_array_cstore13_execute)
SETUPFULLARRAYTEST(test_array_cstore14, test_array_cstore14_pkt, test_array_cstore14_execute)
SETUPFULLARRAYTEST(test_array_cstore15, test_array_cstore15_pkt, test_array_cstore15_execute)
SETUPFULLARRAYTEST(test_array_cstore16, test_array_cstore16_pkt, test_array_cstore16_execute)
SETUPFULLARRAYTEST(test_array_cstore17, test_array_cstore17_pkt, test_array_cstore17_execute)
SETUPFULLARRAYTEST(test_array_cstore18, test_array_cstore18_pkt, test_array_cstore18_execute)
SETUPFULLARRAYTEST(test_array_cstore19, test_array_cstore19_pkt, test_array_cstore19_execute)
SETUPFULLARRAYTEST(test_array_cstore20, test_array_cstore20_pkt, test_array_cstore20_execute)
SETUPFULLARRAYTEST(test_array_cstore21, test_array_cstore21_pkt, test_array_cstore21_execute)
SETUPFULLARRAYTEST(test_array_cstore22, test_array_cstore22_pkt, test_array_cstore22_execute)
SETUPFULLARRAYTEST(test_array_cstore23, test_array_cstore23_pkt, test_array_cstore23_execute)
SETUPFULLARRAYTEST(test_array_cstore24, test_array_cstore24_pkt, test_array_cstore24_execute)

//SETUPFULLARRAYTEST(test_array_extra01, test_array_extra01_pkt, test_array_extra01_execute)
//SETUPFULLARRAYTEST(test_array_extra02, test_array_extra02_pkt, test_array_extra02_execute)
//SETUPFULLARRAYTEST(test_array_extra03, test_array_extra03_pkt, test_array_extra03_execute)
//SETUPFULLARRAYTEST(test_array_extra04, test_array_extra04_pkt, test_array_extra04_execute)
//SETUPFULLARRAYTEST(test_array_extra05, test_array_extra05_pkt, test_array_extra05_execute)
//SETUPFULLARRAYTEST(test_array_extra06, test_array_extra06_pkt, test_array_extra06_execute)

SETUPFULLSTRUCTTEST(test_struct_load01, test_struct_load01_pkt, test_struct_load01_execute)
SETUPFULLSTRUCTTEST(test_struct_load02, test_struct_load02_pkt, test_struct_load02_execute)
SETUPFULLSTRUCTTEST(test_struct_load03, test_struct_load03_pkt, test_struct_load03_execute)
SETUPFULLSTRUCTTEST(test_struct_load04, test_struct_load04_pkt, test_struct_load04_execute)
SETUPFULLSTRUCTTEST(test_struct_load05, test_struct_load05_pkt, test_struct_load05_execute)
SETUPFULLSTRUCTTEST(test_struct_load06, test_struct_load06_pkt, test_struct_load06_execute)

SETUPFULLSTRUCTTEST(test_struct_cload01, test_struct_cload01_pkt, test_struct_cload01_execute)
SETUPFULLSTRUCTTEST(test_struct_cload02, test_struct_cload02_pkt, test_struct_cload02_execute)
SETUPFULLSTRUCTTEST(test_struct_cload03, test_struct_cload03_pkt, test_struct_cload03_execute)
SETUPFULLSTRUCTTEST(test_struct_cload04, test_struct_cload04_pkt, test_struct_cload04_execute)
SETUPFULLSTRUCTTEST(test_struct_cload05, test_struct_cload05_pkt, test_struct_cload05_execute)
SETUPFULLSTRUCTTEST(test_struct_cload06, test_struct_cload06_pkt, test_struct_cload06_execute)
SETUPFULLSTRUCTTEST(test_struct_cload07, test_struct_cload07_pkt, test_struct_cload07_execute)
SETUPFULLSTRUCTTEST(test_struct_cload08, test_struct_cload08_pkt, test_struct_cload08_execute)
SETUPFULLSTRUCTTEST(test_struct_cload09, test_struct_cload09_pkt, test_struct_cload09_execute)
SETUPFULLSTRUCTTEST(test_struct_cload10, test_struct_cload10_pkt, test_struct_cload10_execute)
SETUPFULLSTRUCTTEST(test_struct_cload11, test_struct_cload11_pkt, test_struct_cload11_execute)
SETUPFULLSTRUCTTEST(test_struct_cload12, test_struct_cload12_pkt, test_struct_cload12_execute)

SETUPFULLSTRUCTTEST(test_struct_store01, test_struct_store01_pkt, test_struct_store01_execute)
SETUPFULLSTRUCTTEST(test_struct_store02, test_struct_store02_pkt, test_struct_store02_execute)
SETUPFULLSTRUCTTEST(test_struct_store03, test_struct_store03_pkt, test_struct_store03_execute)
SETUPFULLSTRUCTTEST(test_struct_store04, test_struct_store04_pkt, test_struct_store04_execute)
SETUPFULLSTRUCTTEST(test_struct_store05, test_struct_store05_pkt, test_struct_store05_execute)
SETUPFULLSTRUCTTEST(test_struct_store06, test_struct_store06_pkt, test_struct_store06_execute)
SETUPFULLSTRUCTTEST(test_struct_store07, test_struct_store07_pkt, test_struct_store07_execute)
//SETUPFULLSTRUCTTEST(test_struct_store08, test_struct_store08_pkt, test_struct_store08_execute) // VARYING memcpy
SETUPFULLSTRUCTTEST(test_struct_store09, test_struct_store09_pkt, test_struct_store09_execute)
SETUPFULLSTRUCTTEST(test_struct_store10, test_struct_store10_pkt, test_struct_store10_execute)
SETUPFULLSTRUCTTEST(test_struct_store11, test_struct_store11_pkt, test_struct_store11_execute)
//SETUPFULLSTRUCTTEST(test_struct_store12, test_struct_store12_pkt, test_struct_store12_execute) // VARYING memcpy

SETUPFULLSTRUCTTEST(test_struct_cstore01, test_struct_cstore01_pkt, test_struct_cstore01_execute)
SETUPFULLSTRUCTTEST(test_struct_cstore02, test_struct_cstore02_pkt, test_struct_cstore02_execute)
SETUPFULLSTRUCTTEST(test_struct_cstore03, test_struct_cstore03_pkt, test_struct_cstore03_execute)
//SETUPFULLSTRUCTTEST(test_struct_cstore04, test_struct_cstore04_pkt, test_struct_cstore04_execute) // race condition!
//SETUPFULLSTRUCTTEST(test_struct_cstore05, test_struct_cstore05_pkt, test_struct_cstore05_execute) // race condition!
//SETUPFULLSTRUCTTEST(test_struct_cstore06, test_struct_cstore06_pkt, test_struct_cstore06_execute) // race condition!
SETUPFULLSTRUCTTEST(test_struct_cstore07, test_struct_cstore07_pkt, test_struct_cstore07_execute)
SETUPFULLSTRUCTTEST(test_struct_cstore08, test_struct_cstore08_pkt, test_struct_cstore08_execute)
SETUPFULLSTRUCTTEST(test_struct_cstore09, test_struct_cstore09_pkt, test_struct_cstore09_execute)
SETUPFULLSTRUCTTEST(test_struct_cstore10, test_struct_cstore10_pkt, test_struct_cstore10_execute)
SETUPFULLSTRUCTTEST(test_struct_cstore11, test_struct_cstore11_pkt, test_struct_cstore11_execute)
SETUPFULLSTRUCTTEST(test_struct_cstore12, test_struct_cstore12_pkt, test_struct_cstore12_execute)
SETUPFULLSTRUCTTEST(test_struct_cstore13, test_struct_cstore13_pkt, test_struct_cstore13_execute)
SETUPFULLSTRUCTTEST(test_struct_cstore14, test_struct_cstore14_pkt, test_struct_cstore14_execute)
SETUPFULLSTRUCTTEST(test_struct_cstore15, test_struct_cstore15_pkt, test_struct_cstore15_execute)
//SETUPFULLSTRUCTTEST(test_struct_cstore16, test_struct_cstore16_pkt, test_struct_cstore16_execute) // race condition
//SETUPFULLSTRUCTTEST(test_struct_cstore17, test_struct_cstore17_pkt, test_struct_cstore17_execute) // race condition
//SETUPFULLSTRUCTTEST(test_struct_cstore18, test_struct_cstore18_pkt, test_struct_cstore18_execute) // race condition
SETUPFULLSTRUCTTEST(test_struct_cstore19, test_struct_cstore19_pkt, test_struct_cstore19_execute)
SETUPFULLSTRUCTTEST(test_struct_cstore20, test_struct_cstore20_pkt, test_struct_cstore20_execute)
SETUPFULLSTRUCTTEST(test_struct_cstore21, test_struct_cstore21_pkt, test_struct_cstore21_execute)
SETUPFULLSTRUCTTEST(test_struct_cstore22, test_struct_cstore22_pkt, test_struct_cstore22_execute)
SETUPFULLSTRUCTTEST(test_struct_cstore23, test_struct_cstore23_pkt, test_struct_cstore23_execute)
SETUPFULLSTRUCTTEST(test_struct_cstore24, test_struct_cstore24_pkt, test_struct_cstore24_execute)

//SETUPFULLSTRUCTTEST(test_struct_extra01, test_struct_extra01_pkt, test_struct_extra01_execute)
//SETUPFULLSTRUCTTEST(test_struct_extra02, test_struct_extra02_pkt, test_struct_extra02_execute)
//SETUPFULLSTRUCTTEST(test_struct_extra03, test_struct_extra03_pkt, test_struct_extra03_execute)
//SETUPFULLSTRUCTTEST(test_struct_extra04, test_struct_extra04_pkt, test_struct_extra04_execute)
//SETUPFULLSTRUCTTEST(test_struct_extra05, test_struct_extra05_pkt, test_struct_extra05_execute)
//SETUPFULLSTRUCTTEST(test_struct_extra06, test_struct_extra06_pkt, test_struct_extra06_execute)
//SETUPFULLSTRUCTTEST(test_struct_extra07, test_struct_extra07_pkt, test_struct_extra07_execute)
//SETUPFULLSTRUCTTEST(test_struct_extra08, test_struct_extra08_pkt, test_struct_extra08_execute)
//SETUPFULLSTRUCTTEST(test_struct_extra09, test_struct_extra09_pkt, test_struct_extra09_execute)
//SETUPFULLSTRUCTTEST(test_struct_extra10, test_struct_extra10_pkt, test_struct_extra10_execute)
#if 0
#endif

//----------------------------------------------------------------------------//
// add test cases to test suite (comment / uncomment blocks here and above (SETUPFULLTEST))
//----------------------------------------------------------------------------//
struct TestCase {
	const char* name;
	executeFnType executeFn;
	TestCase(const char* n, executeFnType e) : name(n), executeFn(e) {}
};

void addTestCases(std::vector<TestCase>& testCases) {
#if 0
#endif
    testCases.push_back(TestCase("test01", execute01));
    testCases.push_back(TestCase("test02", execute02));
    testCases.push_back(TestCase("test03", execute03));
    testCases.push_back(TestCase("test04", execute04));
    testCases.push_back(TestCase("test05", execute05));
    testCases.push_back(TestCase("test06", execute06));
    testCases.push_back(TestCase("test07", execute07));
    testCases.push_back(TestCase("test08", execute08));

    testCases.push_back(TestCase("test_array_load01", test_array_load01_execute));
    testCases.push_back(TestCase("test_array_load02", test_array_load02_execute));
    testCases.push_back(TestCase("test_array_load03", test_array_load03_execute));
    testCases.push_back(TestCase("test_array_load04", test_array_load04_execute));
    testCases.push_back(TestCase("test_array_load05", test_array_load05_execute));
    testCases.push_back(TestCase("test_array_load06", test_array_load06_execute));
    testCases.push_back(TestCase("test_array_load07", test_array_load07_execute));

    testCases.push_back(TestCase("test_array_cload01", test_array_cload01_execute));
    testCases.push_back(TestCase("test_array_cload02", test_array_cload02_execute));
    testCases.push_back(TestCase("test_array_cload03", test_array_cload03_execute));
    testCases.push_back(TestCase("test_array_cload04", test_array_cload04_execute));
    testCases.push_back(TestCase("test_array_cload05", test_array_cload05_execute));
    testCases.push_back(TestCase("test_array_cload06", test_array_cload06_execute));
    testCases.push_back(TestCase("test_array_cload07", test_array_cload07_execute));
    testCases.push_back(TestCase("test_array_cload08", test_array_cload08_execute));
    testCases.push_back(TestCase("test_array_cload09", test_array_cload09_execute));
    testCases.push_back(TestCase("test_array_cload10", test_array_cload10_execute));
    testCases.push_back(TestCase("test_array_cload11", test_array_cload11_execute));
    testCases.push_back(TestCase("test_array_cload12", test_array_cload12_execute));

    testCases.push_back(TestCase("test_array_store01", test_array_store01_execute));
    testCases.push_back(TestCase("test_array_store02", test_array_store02_execute));
    testCases.push_back(TestCase("test_array_store03", test_array_store03_execute));
    testCases.push_back(TestCase("test_array_store04", test_array_store04_execute));
    testCases.push_back(TestCase("test_array_store05", test_array_store05_execute));
    testCases.push_back(TestCase("test_array_store06", test_array_store06_execute));
    testCases.push_back(TestCase("test_array_store07", test_array_store07_execute));
    testCases.push_back(TestCase("test_array_store08", test_array_store08_execute));
    testCases.push_back(TestCase("test_array_store09", test_array_store09_execute));
    testCases.push_back(TestCase("test_array_store10", test_array_store10_execute));

    testCases.push_back(TestCase("test_array_cstore01", test_array_cstore01_execute));
    //testCases.push_back(TestCase("test_array_cstore02", test_array_cstore02_execute)); // race condition
    testCases.push_back(TestCase("test_array_cstore03", test_array_cstore03_execute));
    //testCases.push_back(TestCase("test_array_cstore04", test_array_cstore04_execute)); // race condition
    testCases.push_back(TestCase("test_array_cstore05", test_array_cstore05_execute));
    testCases.push_back(TestCase("test_array_cstore06", test_array_cstore06_execute));
    testCases.push_back(TestCase("test_array_cstore07", test_array_cstore07_execute));
    //testCases.push_back(TestCase("test_array_cstore08", test_array_cstore08_execute)); // race condition
    testCases.push_back(TestCase("test_array_cstore09", test_array_cstore09_execute));
    //testCases.push_back(TestCase("test_array_cstore10", test_array_cstore10_execute)); // race condition
    testCases.push_back(TestCase("test_array_cstore11", test_array_cstore11_execute));
    testCases.push_back(TestCase("test_array_cstore12", test_array_cstore12_execute));
    testCases.push_back(TestCase("test_array_cstore13", test_array_cstore13_execute));
    testCases.push_back(TestCase("test_array_cstore14", test_array_cstore14_execute));
    testCases.push_back(TestCase("test_array_cstore15", test_array_cstore15_execute));
    testCases.push_back(TestCase("test_array_cstore16", test_array_cstore16_execute));
    testCases.push_back(TestCase("test_array_cstore17", test_array_cstore17_execute));
    testCases.push_back(TestCase("test_array_cstore18", test_array_cstore18_execute));
    testCases.push_back(TestCase("test_array_cstore19", test_array_cstore19_execute));
    testCases.push_back(TestCase("test_array_cstore20", test_array_cstore20_execute));
    testCases.push_back(TestCase("test_array_cstore21", test_array_cstore21_execute));
    testCases.push_back(TestCase("test_array_cstore22", test_array_cstore22_execute));
    testCases.push_back(TestCase("test_array_cstore23", test_array_cstore23_execute));
    testCases.push_back(TestCase("test_array_cstore24", test_array_cstore24_execute));

//    testCases.push_back(TestCase("test_array_extra01", test_array_extra01_execute));
//    testCases.push_back(TestCase("test_array_extra02", test_array_extra02_execute));
//    testCases.push_back(TestCase("test_array_extra03", test_array_extra03_execute));
//    testCases.push_back(TestCase("test_array_extra04", test_array_extra04_execute));
//    testCases.push_back(TestCase("test_array_extra05", test_array_extra05_execute));
//    testCases.push_back(TestCase("test_array_extra06", test_array_extra06_execute));

    testCases.push_back(TestCase("test_struct_load01", test_struct_load01_execute));
    testCases.push_back(TestCase("test_struct_load02", test_struct_load02_execute));
    testCases.push_back(TestCase("test_struct_load03", test_struct_load03_execute));
    testCases.push_back(TestCase("test_struct_load04", test_struct_load04_execute));
    testCases.push_back(TestCase("test_struct_load05", test_struct_load05_execute));
    testCases.push_back(TestCase("test_struct_load06", test_struct_load06_execute));

    testCases.push_back(TestCase("test_struct_cload01", test_struct_cload01_execute));
    testCases.push_back(TestCase("test_struct_cload02", test_struct_cload02_execute));
    testCases.push_back(TestCase("test_struct_cload03", test_struct_cload03_execute));
    testCases.push_back(TestCase("test_struct_cload04", test_struct_cload04_execute));
    testCases.push_back(TestCase("test_struct_cload05", test_struct_cload05_execute));
    testCases.push_back(TestCase("test_struct_cload06", test_struct_cload06_execute));
    testCases.push_back(TestCase("test_struct_cload07", test_struct_cload07_execute));
    testCases.push_back(TestCase("test_struct_cload08", test_struct_cload08_execute));
    testCases.push_back(TestCase("test_struct_cload09", test_struct_cload09_execute));
    testCases.push_back(TestCase("test_struct_cload10", test_struct_cload10_execute));
    testCases.push_back(TestCase("test_struct_cload11", test_struct_cload11_execute));
    testCases.push_back(TestCase("test_struct_cload12", test_struct_cload12_execute));

    testCases.push_back(TestCase("test_struct_store01", test_struct_store01_execute));
    testCases.push_back(TestCase("test_struct_store02", test_struct_store02_execute));
    testCases.push_back(TestCase("test_struct_store03", test_struct_store03_execute));
    testCases.push_back(TestCase("test_struct_store04", test_struct_store04_execute));
    testCases.push_back(TestCase("test_struct_store05", test_struct_store05_execute));
    testCases.push_back(TestCase("test_struct_store06", test_struct_store06_execute));
    testCases.push_back(TestCase("test_struct_store07", test_struct_store07_execute));
    //testCases.push_back(TestCase("test_struct_store08", test_struct_store08_execute)); // VARYING memcpy
    testCases.push_back(TestCase("test_struct_store09", test_struct_store09_execute));
    testCases.push_back(TestCase("test_struct_store10", test_struct_store10_execute));
    testCases.push_back(TestCase("test_struct_store11", test_struct_store11_execute));
    //testCases.push_back(TestCase("test_struct_store12", test_struct_store12_execute)); // VARYING memcpy

    testCases.push_back(TestCase("test_struct_cstore01", test_struct_cstore01_execute));
    testCases.push_back(TestCase("test_struct_cstore02", test_struct_cstore02_execute));
    testCases.push_back(TestCase("test_struct_cstore03", test_struct_cstore03_execute));
    //testCases.push_back(TestCase("test_struct_cstore04", test_struct_cstore04_execute)); // race condition
    //testCases.push_back(TestCase("test_struct_cstore05", test_struct_cstore05_execute)); // race condition
    //testCases.push_back(TestCase("test_struct_cstore06", test_struct_cstore06_execute)); // race condition
    testCases.push_back(TestCase("test_struct_cstore07", test_struct_cstore07_execute));
    testCases.push_back(TestCase("test_struct_cstore08", test_struct_cstore08_execute));
    testCases.push_back(TestCase("test_struct_cstore09", test_struct_cstore09_execute));
    testCases.push_back(TestCase("test_struct_cstore10", test_struct_cstore10_execute));
    testCases.push_back(TestCase("test_struct_cstore11", test_struct_cstore11_execute));
    testCases.push_back(TestCase("test_struct_cstore12", test_struct_cstore12_execute));
    testCases.push_back(TestCase("test_struct_cstore13", test_struct_cstore13_execute));
    testCases.push_back(TestCase("test_struct_cstore14", test_struct_cstore14_execute));
    testCases.push_back(TestCase("test_struct_cstore15", test_struct_cstore15_execute));
    //testCases.push_back(TestCase("test_struct_cstore16", test_struct_cstore16_execute)); // race condition
    //testCases.push_back(TestCase("test_struct_cstore17", test_struct_cstore17_execute)); // race condition
    //testCases.push_back(TestCase("test_struct_cstore18", test_struct_cstore18_execute)); // race condition
    testCases.push_back(TestCase("test_struct_cstore19", test_struct_cstore19_execute));
    testCases.push_back(TestCase("test_struct_cstore20", test_struct_cstore20_execute));
    testCases.push_back(TestCase("test_struct_cstore21", test_struct_cstore21_execute));
    testCases.push_back(TestCase("test_struct_cstore22", test_struct_cstore22_execute));
    testCases.push_back(TestCase("test_struct_cstore23", test_struct_cstore23_execute));
    testCases.push_back(TestCase("test_struct_cstore24", test_struct_cstore24_execute));

//    testCases.push_back(TestCase("test_struct_extra01", test_struct_extra01_execute));
//    testCases.push_back(TestCase("test_struct_extra02", test_struct_extra02_execute));
//    testCases.push_back(TestCase("test_struct_extra03", test_struct_extra03_execute));
//    testCases.push_back(TestCase("test_struct_extra04", test_struct_extra04_execute));
//    testCases.push_back(TestCase("test_struct_extra05", test_struct_extra05_execute));
//    testCases.push_back(TestCase("test_struct_extra06", test_struct_extra06_execute));
//    testCases.push_back(TestCase("test_struct_extra07", test_struct_extra07_execute));
//    testCases.push_back(TestCase("test_struct_extra08", test_struct_extra08_execute));
//    testCases.push_back(TestCase("test_struct_extra09", test_struct_extra09_execute));
//    testCases.push_back(TestCase("test_struct_extra10", test_struct_extra10_execute));

    //testCases.push_back(TestCase("test_aos_load01", test_aos_load01_execute));
#if 0
#endif
}

int main(int argc, char** argv) {

    printf("\n\n\n--------------------------------------------------------------------------------\n");
    printf("running test-suite for automatic packetization...\n\n");

    //------------------------------------------------------------------------//
    // create function pointers for test cases and save test case names
    //------------------------------------------------------------------------//
    std::vector<TestCase> testCases;

    //add scalar and generated functions
    addTestCases(testCases);

    const unsigned testCaseNr = testCases.size();

    //------------------------------------------------------------------------//
    // create input values
    //------------------------------------------------------------------------//
#ifdef USE_RANDOM_TESTS
    const unsigned inputNr = 14 + NUM_RANDOM_INPUT_VALUE_SETS;
#else
    const unsigned inputNr = 14;
#endif
    const unsigned inputParamNr = 2;
    const unsigned inputPermutations = pow(inputNr, inputParamNr);

	float scalarInputs0[inputNr];
	float scalarInputs1[inputNr];
	float scalarInputs2[inputNr];
	float scalarInputs3[inputNr];

	// 14 hardcoded input value sets
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

#ifdef USE_RANDOM_TESTS
	// now add random inputs
	#define CUSTOM_RAND_MAX 1000 //prevent too large inputs
	srand((unsigned)time(0));
	for (unsigned i=0; i<inputNr-14; ++i) {
		float r = (float)rand()/(float)RAND_MAX;
		float neg = rand() > (RAND_MAX/2) ? 1.f : -1.f;
		scalarInputs0[i+14] = (rand() % CUSTOM_RAND_MAX) * r * neg;

		r = (float)rand()/(float)RAND_MAX;
		neg = rand() > (RAND_MAX/2) ? 1.f : -1.f;
		scalarInputs1[i+14] = (rand() % CUSTOM_RAND_MAX) * r * neg;

		r = (float)rand()/(float)RAND_MAX;
		neg = rand() > (RAND_MAX/2) ? 1.f : -1.f;
		scalarInputs2[i+14] = (rand() % CUSTOM_RAND_MAX) * r * neg;

		r = (float)rand()/(float)RAND_MAX;
		neg = rand() > (RAND_MAX/2) ? 1.f : -1.f;
		scalarInputs3[i+14] = (rand() % CUSTOM_RAND_MAX) * r * neg;
	}
#endif

    //------------------------------------------------------------------------//
    // create result arrays for generated functions
    //------------------------------------------------------------------------//
    const unsigned maxResultNr = inputPermutations * testCaseNr;

    //const bool printAllResults = false;
    //const bool printAllTimes = false;

    //------------------------------------------------------------------------//
    // create timer and data structures that hold execution times
    //------------------------------------------------------------------------//
    //Packetizer::Timer timer;
    //std::vector<double> executionTimesScalar;
    //std::vector<double> executionTimesPacketized;
    
    //------------------------------------------------------------------------//
    // compute results of scalar and generated functions
    //------------------------------------------------------------------------//
	std::vector<std::pair<const char*, bool> > results;
    unsigned testsRun = 0;

    for (unsigned TC=0; TC<testCaseNr; ++TC) {
        unsigned inputPermsRun = 0;
        for (unsigned i=0; i<inputNr; ++i) {
            for (unsigned j=0; j<inputNr; ++j) {
                // abort if we have already run too many test cases
                if (testsRun >= maxResultNr) {
                    printf("\nERROR: not enough space allocated for results!\n");
                    exit(-1);
                }

                // get function pointers of current test case
				TestCase testCase = testCases[TC];
                
                // get input values
                const float input0i = scalarInputs0[i];
                const float input1i = scalarInputs1[i];
                const float input2i = scalarInputs2[i];
                const float input3i = scalarInputs3[i];
                const float input0j = scalarInputs0[j];
                const float input1j = scalarInputs1[j];
                const float input2j = scalarInputs2[j];
                const float input3j = scalarInputs3[j];

				const ALIGN VEC v3210 = _mm_set_ps(input0i, input1i, input2i, input3i);
				const ALIGN VEC v7654 = _mm_set_ps(input0j, input1j, input2j, input3j);
				const ALIGN VECI vi3210 = _mm_set_epi32((int)input0i, (int)input1i, (int)input2i, (int)input3i);
				const ALIGN VECI vi7654 = _mm_set_epi32((int)input0j, (int)input1j, (int)input2j, (int)input3j);

				// execute
				const bool success = testCase.executeFn(input0i, input1i, input2i, input3i,
					input0j, input1j, input2j, input3j,
					v3210, v7654, vi3210, vi7654);

				// save result
				results.push_back(std::make_pair(testCase.name, success));

                // generate info-string for this execution
				char inputString[120];
				sprintf(inputString, "[ [ %f %f %f %f ] | [ %f %f %f %f ] ]",
					input0i, input1i, input2i, input3i, input0j, input1j, input2j, input3j);

                ++testsRun;
                printf("Total progress: %.2f percent | test case %03d/%03d (inputs %03d/%03d) : %-30.30s\r", ((float)testsRun * 100.f) / (float)maxResultNr, TC+1, testCaseNr, ++inputPermsRun, inputPermutations, testCase.name); fflush(stdout);
            }
        }
    }

    if (testsRun <= 0) {
        printf("ERROR: need to compute at least one result! (forgot to activate test cases?)\n");
        exit(-1);
    }
    if (testsRun != maxResultNr) {
        printf("ERROR: unexpected number of results  (testsRun != maxResultNr)!\n");
        exit(-1);
    }

/*
    if (printAllResults) {
        for (unsigned i=0; i<testsRun; ++i) {
            const char* testCaseName = scalarResults[i].second.first;
            printf("%s (scalar) = [ ", testCaseName);
            printf("%f %f %f %f ", (*scalarResults[i].second.second)[0], (*scalarResults[i].second.second)[1], (*scalarResults[i].second.second)[2], (*scalarResults[i].second.second)[3]);
            printf("]\n");

            printf("%s (packetized) = [ ", testCaseName);
            printf("%f %f %f %f ", get((*results)[i], 0), get((*results)[i], 1), get((*results)[i], 2), get((*results)[i], 3));
            printf("]\n");
        }
    }
    
    //TODO: print average statistics per test case
    printf("execution times:\n\n");
    double accumulatedSpeedup = 0.0;
    double minSpeedup = 999.0;
    double maxSpeedup = -1.0;
    unsigned inputPermutationNr = 1;
    double testCaseAccumulatedSpeedup = 0.0;
    double testCaseMinSpeedup = 999.0;
    double testCaseMaxSpeedup = -1.0;
    for (unsigned i=0; i<testsRun; ++i) {
        const char* testCaseName = scalarResults[i].second.first;
        const double executionTimeScalar = executionTimesScalar[i];
        const double executionTimePacketized = executionTimesPacketized[i];
        const double speedup = executionTimeScalar / (executionTimePacketized == 0.0f ? 0.00001f : executionTimePacketized);
        //global statistics
        accumulatedSpeedup += speedup;
        if (speedup < minSpeedup) minSpeedup = speedup;
        if (speedup > maxSpeedup) maxSpeedup = speedup;
        //test case statistics
        if (inputPermutationNr == inputNr * inputNr) {
            //print stats of current test case
            const double testCaseAverageSpeedup = testCaseAccumulatedSpeedup / (inputNr * inputNr);
            printf("Statistics for test case '%s':\n", testCaseName);
            printf("  min speedup: %.2f\n", testCaseMinSpeedup);
            printf("  max speedup: %.2f\n", testCaseMaxSpeedup);
            printf("  average speedup: %.2f\n\n", testCaseAverageSpeedup);
    
            //set statistics to first run of current test case
            testCaseAccumulatedSpeedup = speedup;
            testCaseMinSpeedup = speedup;
            testCaseMaxSpeedup = speedup;
            inputPermutationNr = 1;
        } else {
            testCaseAccumulatedSpeedup += speedup;
            if (speedup < testCaseMinSpeedup) testCaseMinSpeedup = speedup;
            if (speedup > testCaseMaxSpeedup) testCaseMaxSpeedup = speedup;
            ++inputPermutationNr;
        }
        
        if (printAllTimes) {
            printf("%s (scalar) = %f\n", testCaseName, executionTimeScalar);
            printf("%s (packetized) = %f", testCaseName, executionTimePacketized);
            if (speedup >= 1.0) printf(" (speedup: %.2f)\n\n", speedup);
            else printf(" (slowdown: %.2f)\n\n", 1.0/speedup);
        }
    }
            
    const double averageSpeedup = accumulatedSpeedup / (double)testsRun;
    printf("\nOverall statistics:\n");
    printf("  min speedup: %.2f\n", minSpeedup);
    printf("  max speedup: %.2f\n", maxSpeedup);
    printf("  average speedup: %.2f\n\n", averageSpeedup);
    

    printf("verifying results:\n\n");

    bool allSuccessfull = true;
    unsigned failedTestsNr = 0;
    for (unsigned i=0; i<testsRun; ++i) {
        bool success = true;
        success &= ((*scalarResults[i].second.second)[0] == get((*results)[i], 0)) &&
                   ((*scalarResults[i].second.second)[1] == get((*results)[i], 1)) &&
                   ((*scalarResults[i].second.second)[2] == get((*results)[i], 2)) &&
                   ((*scalarResults[i].second.second)[3] == get((*results)[i], 3));
        if (printAllResults) {
            printf("%s ", scalarResults[i].second.first);
            success ? printf(" SUCCESSFULL!") : printf(" FAILED!     ");
			printf(" %s\n", scalarResults[i].first);
        } else if (!success) {
            printf("%s FAILED! ", scalarResults[i].second.first);
			printf(" %s\n", scalarResults[i].first); //input-values
            printf("  expected result: [ %f %f %f %f ]\n",
				(*scalarResults[i].second.second)[0],
				(*scalarResults[i].second.second)[1],
				(*scalarResults[i].second.second)[2],
				(*scalarResults[i].second.second)[3]);
            printf("  computed result: [ %f %f %f %f ]\n",
				get((*results)[i], 0),
				get((*results)[i], 1),
				get((*results)[i], 2),
				get((*results)[i], 3));
        }

        allSuccessfull &= success;
        if (!success) ++failedTestsNr;
    }
	*/

	printf("\n\n");

	bool allSuccessfull = true;
	unsigned failedTestsNr = 0;
    for (unsigned i=0; i<testsRun; ++i) {
		const bool success = results[i].second;
		if (!success) {
			++failedTestsNr;
			allSuccessfull = false;
			printf("test %d failed: %s\n", i, results[i].first);
		}
	}

    if (allSuccessfull) printf("ALL TESTS SUCCESSFULL! (%d)\n", testsRun);
	else printf("\n%d / %d TESTS FAILED!", failedTestsNr, testsRun);

    printf("\n\ntest-suite run complete!\n");
    printf("--------------------------------------------------------------------------------\n\n");


    return 0;
}

