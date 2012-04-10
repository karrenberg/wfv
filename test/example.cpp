/**
 * @file   example.cpp
 * @date   19.07.2010
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2010 Saarland University
 *
 *
 * Compile with:
 * clang -emit-llvm -c -Wall example.cpp -o example.bc
 *
 * Test packetizer:
 * packetizeFunction -m example.bc -f test_scalar -t test_to_be_generated
 *
 */

#include <xmmintrin.h>
#include <stdio.h>

extern "C" __m128 test_to_be_generated(__m128 a, __m128 b);

// implementierung
extern "C" float test_scalar(float a, float b) {
	float x = a * b;
	return x;
}

int main(void) {

	float a0 = 1.01f;
	float a1 = 1.02f;
	float a2 = 1.03f;
	float a3 = 1.04f;
	float b0 = 13.33f;
	float b1 = 13.34f;
	float b2 = 13.35f;
	float b3 = 13.36f;

	float res0 = test_scalar(a0, b0);
	float res1 = test_scalar(a1, b1);
	float res2 = test_scalar(a2, b2);
	float res3 = test_scalar(a3, b3);

	__m128 av = _mm_set_ps(a0, a1, a2, a3);
	__m128 bv = _mm_set_ps(b0, b1, b2, b3);

	// fake use to prevent deletion of target function
	__m128 resv = test_to_be_generated(av, bv);

	printf("res (scalar): %f %f %f %f\n", res0, res1, res2, res3);
	printf("res (packetized): %f %f %f %f\n", ((float*)&resv)[0], ((float*)&resv)[1], ((float*)&resv)[2], ((float*)&resv)[3]);

	return 0;
}
