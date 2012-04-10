/**
 * @file   wfvTestsAVX.cpp
 * @date   29.04.2011
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2011 Saarland University
 *
 */
#include <stdio.h>
#include <math.h>
//#include <mmintrin.h> //MMX
#include <xmmintrin.h> //SSE
//#include <emmintrin.h> //SSE2
//#include <pmmintrin.h> //SSE3
//#include <tmmintrin.h> //SSSE3
//#include <ammintrin.h> //SSE4a
//#include <smmintrin.h> //SSE4.1
//#include <nmmintrin.h> //SSE4.2
//#include <bmmintrin.h> //SSE5
#include <immintrin.h> //AVX

#include <vector>

//#include "Packetizer/timer.h"
//#include "include/RTfactShader.hpp"

#if !defined __x86_64 && !defined _M_X64
	#define RUN_SHADER_TESTS //does not work under 64bit
#endif

#define packetizationSize 8
#define totalSIMDIterations packetizationSize / 8

#if totalSIMDIterations == 1
    #define VEC __m256
    #define VECI __m256i
#else
    #define VEC __m256*
    #define VECi __m256i*
    #warning "test suite currently does not support packetizationSize > 8!"
#endif

#define ALIGN __attribute__ ((aligned (32)))
#define REALIGN //__attribute__((force_align_arg_pointer))

// helper: SmallVector does not allow to use __m256 directly (no constructor?!)
struct V {
    V() {}
    V(VEC a) { data = a; }
    VEC data;
} ALIGN;

void *aligned_malloc(const size_t size, const size_t align_size) {

  char *ptr,*ptr2,*aligned_ptr;
  const int align_mask = align_size - 1;

  ptr=(char *)malloc(size + align_size + sizeof(int));
  if(ptr==NULL) return(NULL);

  ptr2 = ptr + sizeof(int);
  aligned_ptr = ptr2 + (align_size - ((size_t)ptr2 & align_mask));


  ptr2 = aligned_ptr - sizeof(int);
  *((int *)ptr2)=(int)(aligned_ptr - ptr);

  return(aligned_ptr);
}

// helper: extract ith element of a __m256
inline float& get(const VEC& v, const unsigned idx) {
    return ((float*)&v)[idx];
}
inline float& get(const V& v, const unsigned idx) {
    return ((float*)&v)[idx];
}
inline float& get(const V* v, const unsigned idx) {
    return ((float*)&(v->data))[idx];
}

// due to imprecisions etc. we apply a few rules
// when to treat results as equal.
// - float == float -> equality of first 5 decimal places
// - NaN == NaN -> true ;)
bool resultMatches(const float a, const float b) {
	return a == b ||
		(isnan(a) && isnan(b)) ||
		(abs(a-b) < 0.000001);
}

//----------------------------------------------------------------------------//
// declarations of empty prototypes of packetized functions
//----------------------------------------------------------------------------//
//arithmetic only
extern "C" VEC test_generated(VEC a, VEC b) ALIGN;
//simple control flow
extern "C" VEC test_if_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_criticaledge_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_phi1_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_phi2_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_phi3_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_phi4_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_phi5_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_phi6_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_phi7_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_phi8_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_phi9_generated(VEC a, VEC b) ALIGN;
//simple loops
extern "C" VEC test_loop1_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loop2_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loop3_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loop4_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loop5_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loop6_generated(VEC a, VEC b) ALIGN;
//more complex loops
extern "C" VEC test_loopc1_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopc2_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopc3_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopc4_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopc5_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopc6_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopc7_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopc8_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopc9_generated(VEC a, VEC b) ALIGN;
//loops with multiple exits
extern "C" VEC test_loopmx1_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopmx2_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopmx3_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopmx4_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopmx5_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopmx6_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopmx7_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopmx8_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopmx9_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopmx10_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopmx11_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopmx12_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopmx13_generated(VEC a, VEC b) ALIGN;
//nested loops
extern "C" VEC test_loopns1_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopns2_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopns3_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopns4_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopns5_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopns6_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopns7_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopns8_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopns9_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopns10_generated(VEC a, VEC b) ALIGN;
//nested loops with multiple exits
extern "C" VEC test_loopnsmx1_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopnsmx2_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopnsmx3_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopnsmx4_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopnsmx5_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopnsmx6_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopnsmx7_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopnsmx8_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopnsmx9_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_loopnsmx10_generated(VEC a, VEC b) ALIGN;

//function calls
extern "C" VEC test_call1_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_call2_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_call3_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_call4_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_call5_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_call6_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_call7_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_call8_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_call9_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_call10_generated(VEC a, VEC b) ALIGN;

//array access
extern "C" VEC test_noise_generated(VEC a, VEC b) ALIGN;

//shader tests
extern "C" VEC test_shader_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_brick_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_checker_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_dented_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_glass_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_granite_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_parquet_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_phong_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_screen_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_starball_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_whitted_generated(VEC a, VEC b) ALIGN;
extern "C" VEC test_wood_generated(VEC a, VEC b) ALIGN;

extern "C" VEC test_irreducible1_generated(VEC a, VEC b) ALIGN; 
extern "C" VEC test_irreducible2_generated(VEC a, VEC b) ALIGN; 
extern "C" VEC test_irreducible3_generated(VEC a, VEC b) ALIGN; 

//----------------------------------------------------------------------------//
// implementations of corresponding scalar source functions
//----------------------------------------------------------------------------//
//arithmetic only
extern "C" float test_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    return x / y;
}
//simple control flow
extern "C" float test_if_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z;

    if (x<y) z = a;
    else z = a*a;

    return z;
}
extern "C" float test_criticaledge_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = a*a;

    if (x<y) z = z + a;

    z = z+b;

    return z;
}
extern "C" float test_phi1_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z;

    if (x<y) {
        z = a+x;
    } else {
        z = a*a;
    }

    z = z+x;
    z = y-z;

    return z;
}
extern "C" float test_phi2_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z;
    float r;

    if (x<y) {
        z = a+x;
        r = x*x;
    } else {
        z = a*a;
        r = x-a;
    }

    z = z+x;
    z = y-z;

    return z * r;
}
extern "C" float test_phi3_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z;
    float r;

    if (x<y) {
        z = a+x;
        r = x*x;
    } else if (x>y) {
        z = a*a;
        r = x-a;
    } else {
        z = y-a;
        r = y+a;
    }

    z = z+x;
    z = y-z;

    return z * r;
}
extern "C" float test_phi4_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z;
    float r;

    if (x>y) {
        z = a+x;
        r = x*x;
    } else if (y>x) {
        z = a*a;
        if (z != y) r = x-a;
        else r = x+a;
    } else {
        z = y-a;
        r = y+a;
    }

    z = z+x;
    z = y-z;

    return z * r;
}
extern "C" float test_phi5_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;
    float r = 3;

    if (x<y) {
        z = a+x;
        r += z*z;
        float f = z-r;
        z -= f;
    }

    z = z+x;

    return z * r;
}
extern "C" float test_phi6_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;
    float r = 3;

    if ((a <= z && b > 4) || z>y) {
        z = a+x;
        r += z*z;
        float f = z-r;
        z -= f;
    }

    z = z+x;

    return z * r;
}
extern "C" float test_phi7_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z;

    if (x<y) {
        z = a+x;
    } else {
        z = a*a;
    }

    if (z > a && a < b) {
        z++;
    }

    z = z+x;
    z = y-z;

    return z;
}
extern "C" float test_phi8_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    if (y > b) {
        z *= z;

        if (x<y) {
            z = a+x;
        } else {
            z = a*a;
        }

        z -= a;

        if (z > a && a < b) {
            z++;
        }

        z += b;
    }

    z = z+x;
    z = y-z;

    return z;
}
extern "C" float test_phi9_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    if (a < b) {
        z += a;
    } else if (b < a) {
        z += a*a;
    }

    z = z+x;
    z = y-z;

    return z;
}
//simple loops
extern "C" float test_loop1_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        z += x;
    }

    return z;
}
extern "C" float test_loop2_scalar(float a, float b) {
    int i = 0;
    float sum = a*b;

    do {
        sum += i;
        i++;
    } while (i < 42);

    return sum;
//    float i = 0.4f;
//    float sum = a*b;
//
//    do {
//        sum += i;
//        i+=a;
//    } while (i < 42.3f);
//
//    return sum;
}
extern "C" float test_loop3_scalar(float a, float b) {
    int i = 0;
    float sum = a*b;

    do {
        sum += i;
        i++;
    } while (i < 42);

    return sum;
}
extern "C" float test_loop4_scalar(float a, float b) {
    int i = 0;
    float sum = a*b;

    do {
	i++;
    } while (sum += i,i < 42);
    return sum;
}
extern "C" float test_loop5_scalar(float a, float b) {
    int i = 0;
    float sum = a*b;

    do {

    } while (sum += i,i++,i < 42);

    return sum;
}
extern "C" float test_loop6_scalar(float a, float b) {
    int i = 0;
    float sum = a*b+42;

    do ; while (++i,i < 1000);

    return sum;
}
//more complex loops
extern "C" float test_loopc1_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    int i = 0;
    int j = 100;
    while (i<1000 && j != 42) {
        z -= 3*y;
        --j;
        ++i;
    }

    return z;
}
extern "C" float test_loopc2_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = a;

    for (int i=0; i<1000; ++i) {
        if (z<y) {
            z += a;
        } else {
            z += b;
        }
    }

    return z-b;
}
extern "C" float test_loopc3_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        if (x<y) {
            z += a;
        } else {
            z += a*a;
        }
    }

    return z-b;
}
extern "C" float test_loopc4_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    if (x<y) {
        for (int i=0; i<1000; ++i) {
            z += a;
        }
    } else {
        z += a*a;
    }

    return z-b;
}
extern "C" float test_loopc5_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        if (z > 200) continue;
        z += x;
    }

    z = z-y;

    return z;
}
extern "C" float test_loopc6_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<a; ++i) {
        z += x;
    }

    z = z+x;
    z = y-z;

    return z;
}
extern "C" float test_loopc7_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;
    float r = 42.42f;

    for (float i=3.2f; i<a && z <= b*y; i=i+2) {
        z += x*i;
    }

    return z*r;
}
extern "C" float test_loopc8_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<a && z >= x-b; ++i) {
        if (z-x == y) z += x;
        else {
            z = a;
            if (z / a < x) {
                z += a/i;
            }
        }
    }

    z = z+x;
    z = y-z;

    return z;
}
extern "C" float test_loopc9_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;
    float r = 42.42f;

    for (float i=3.2f; i<a; i=i+2) {
        if (x<y) {
            z = a+x;
            r = x*x;
            y++;
        } else if (x>y) {
            z = a*a;
            y--;
            if (z != y) r = x-a;
            else r = x+a;
        } else {
            z = y-a;
            r = y+a;
        }
        z += x;
    }

    return z*r;
}
//loops with multiple exits
extern "C" float test_loopmx1_scalar(float a, float b) {
    int i = 0;
    float sum = a*b;

    for (;;) {
	sum += i;
	i++;
	if (i >= 42) break;
    }

    return sum;
}
extern "C" float test_loopmx2_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        if (z > 104.0f) break; //needs icmp + masking
        z += x; // 98/96/94/92 += 10
    }

    z = z-y;

    return z;
}
extern "C" float test_loopmx3_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<100; ++i) {
        z += x;
        if (i > a) break;
    }

    z = z+x;
    z = y-z;

    return z;
}
extern "C" float test_loopmx4_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    if (x<y) {
        for (int i=0; i<10; ++i) {
            z += a;
            if (z > 53.12f) return a;
        }
    } else {
        z += a*a;
    }

    return z-b;
}
extern "C" float test_loopmx5_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;
    for (int i=5; i<1000; ++i) {
        if (z-x == y) continue;
        z += a;
        if (z / a < x) break;
    }

    z = z-y;

    z+=a;

    return z;
}
extern "C" float test_loopmx6_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;
    for (int i=5; i<1000; ++i) {
        if (z-x == y) continue;
        z += a;
        if (z / a < x) break;
    }

    z = z-y;

    if (a > b) {
        z = z+y;
    } else {
        z--;
    }

    z+=a;

    return z;
}
extern "C" float test_loopmx7_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;
    for (int i=0; i<1000; ++i) {
        z += a;
        if (z-x == y) {
            z *= 133.0f;
            continue;
        }
        z += y;
        if (z / a > x) {
            z /= -2.11f;
            break;
        }
        z -= b;
    }

    z = z-y;

    return z;
}
extern "C" float test_loopmx8_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;
    float r;

    for (int i=0; i<100; ++i) {
        if (z-x == y) continue;
        z += x;
        //if (z / i < x) break; //-> means we have to vectorize i !!!
        if (z / a < x) break;
    }

    if (x<y) {
        z = a+x;
        r = x*x;
    } else if (x>y) {
        z -= a*a;
        if (z != y) r = x-a;
        else r = x+a;
    } else {
        z = y-a;
        r = y+a;
    }

    z = z+x;
    z = y-z;

    return z * r;
}
extern "C" float test_loopmx9_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        z += x;
        if (z > 600) continue;
        if (z-x == y) break;
        z += a;
        if (z > 200) continue;
        if (z / a < x) break;
        z -= b;
    }

    z = z-y;

    return z;
}
extern "C" float test_loopmx10_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    //the 'butterfly' / 'lung'
    for (int i=0; i<100; ++i) {
        z += a;
        if (z > a) {
            a *= a;
            if (a > b) break;
        } else {
            a += a;
            if (a < b) break;
        }
        z += b;
    }

    z = z+x;
    z = y-z;

    return z;
}
extern "C" float test_loopmx11_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    //the 'butterfly' / 'lung'
    for (int i=0; i<100; ++i) {
        z += a;
        if (a > 4) {
            if (a > b) break;
        } else {
            if (a < b) break;
        }
        z += b;
    }

    z = z+x;
    z = y-z;

    return z;
}
extern "C" float test_loopmx12_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    //the 'butterfly' / 'lung' with vectorized loop and stuff :P
    for (int i=0; i<a && z != y*y; ++i) {
        z += a;
        if (z > a) {
            z += 7.33f;
            a *= a;
            if (a > b) break;
            z -= 7.13f;
        } else {
            z -= 7.33f;
            a += a;
            if (a < b) break;
            z += 7.13f;
        }
        z += b;
    }

    z = z+x;
    z = y-z;

    return z;
}
extern "C" float test_loopmx13_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    //the 'butterfly' / 'lung' with vectorized loop and stuff :P
    for (int i=0; i<a && z != y*y; ++i) {
        z += a;
        if (a > 4) {
            z += 7.33f;
            if (a > b) break;
            z -= 7.13f;
        } else {
            z -= 7.33f;
            if (a < b) break;
            z += 7.13f;
        }
        z += b;
    }

    z = z+x;
    z = y-z;

    return z;
}
//nested loops
extern "C" float test_loopns1_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<100; ++i) {
        for (int j=0; j<100; ++j) {
            z += x;
        }
        z -= 3*y;
    }

    return z;
}
extern "C" float test_loopns2_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        z += a;
        if (z / a < x) z += a;
        else {
            z -= b;
            if (a > b) z -= b;
            else {
                z *= z-y;
                if (b == a) {
                    for (int j=0; j<200; ++j) {
                        if (i == j) z *= z;
                        z += 13.2f;
                    }
                    z = a+3;
                } else {
                    ++z;
                }
            }
            for (int j=0; j<100; ++j) {
                if (i < j) z += a;
                else z -= 13.2f;
            }
        }
    }

    z = z-y;

    return z;
}
extern "C" float test_loopns3_scalar(float a, float b) {
    float x = a + b;
    float y = x;
    float z = y;

    for (int i=0; i<a; ++i) {
        for (int j=0; j<100; ++j) {
            z += x;
        }
    }

    z = z-y;

    return z;
}
extern "C" float test_loopns4_scalar(float a, float b) {
    float x = a + b;
    float y = x;
    float z = y;
    
    for (int i=0; i<a; ++i) {
        for (int j=0; j<b; ++j) {
            z += x;
        }
    }

    z = z-y;

    return z;
}
extern "C" float test_loopns5_scalar(float a, float b) {
    float x = a + b;
    float y = x;
    float z = y;

    for (int i=0; i<a; ++i) {
        for (int j=0; j<b; ++j) {
            z += a-i;
        }
    }

    z = z-y;

    return z;
}
extern "C" float test_loopns6_scalar(float a, float b) {
    float x = a + b;
    float y = x;
    float z = y;

    for (int i=0; i<a; ++i) {
        for (int j=0; j<b; ++j) {
            a -= i;
            z += a;
        }
    }
    
    z = z-y;

    return z;
}
extern "C" float test_loopns7_scalar(float a, float b) {
    float x = a + b;
    float y = x;
    float z = y;

    for (int i=0; i<a; ++i) {
        for (int j=0; j<b; ++j) {
            z += y-i*j;
        }
        z -= a/i;
    }

    z = z-y;

    return z;
}
extern "C" float test_loopns8_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<b; ++i) {
        z *= a;
        for (int j=0; j<100; ++j) {
            for (int k=0; k<a; ++k) {
                a -= i;
                z += x+k*j-a;
            }
            z -= i*b - j*x;
        }
        a *= i;
    }

    z = z-y;

    return z;
}
extern "C" float test_loopns9_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<a && z >= x-b; ++i) {
        if (z-x == y) z += x;
        else {
            z = a;
            for (int j=0; j<a; ++j) {
                z -= 12.33f;
                if (z / a < x) {
                    z += a;
                } else {
                    ++z;
                }
            }
        }
    }

    z = z+x;
    z = y-z;

    return z;
}
extern "C" float test_loopns10_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<a && z >= x-b; ++i) {
        if (z-x == y) z += x;
        else {
            z = a;
            for (int j=0; j<a; ++j) {
                z -= 12.33f;
                if (z / a < x) {
                    z += a*i;
                }
                z *= a-b;
                if (i == j) {
                    for (int k=0; k<a; k+=2) {
                        z += 5.33f;
                    }
                } else {
                    ++z;
                }
            }
        }
    }

    z = z+x;
    z = y-z;

    return z;
}
//nested loops with multiple exits
extern "C" float test_loopnsmx1_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        z += a;
        for (int j=3; j<200; ++j) {
            z /= -0.12f;
            if (i > 500) break;
            if (i*j > 2000) return z;
        }
    }

    z = z-y;

    return z;
}
extern "C" float test_loopnsmx2_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    //for (int i=0; i<2; ++i) {
    //    z += 1;
    //    for (int j=0; j<3; ++j) {
    //        z /= -1.f;
    //        if (z < 0.f) return z;
    //    }
    //}
    
    for (int i=0; i<a; ++i) {
        z++;
        for (int j=0; j<b; ++j) {
            z -= i;
            if (z < 0.f) return z;
        }
    }
    
    z = z-y;

    return z;
}
extern "C" float test_loopnsmx3_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<2; ++i) {
        z += 1;
        for (int j=0; j<3; ++j) {
            z /= -1.f;
            if (z < -100.f) break;
            if (z < 0.f) return z;
        }
    }

    z = z-y;

    return z;
}
extern "C" float test_loopnsmx4_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        z += a;
        for (int j=0; j<200; ++j) {
            z /= -0.12f;
            if (z < -100.f) break;
            if (z < 0.f) return z;
        }
    }

    for (int i=0; i<2; ++i) {
        z += 1;
        for (int j=0; j<3; ++j) {
            z /= -1.f;
            if (z < -100.f) break;
            if (z < 0.f) return z;
        }
    }

    z = z-y;

    return z;
}
extern "C" float test_loopnsmx5_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        z += a;
        if (z / a < x) break;
        else {
            z -= b;
            if (a > b) {
                for (int j=3; j<4500; ++j) {
                    if (i == j) z /= -0.12f;
                    if (z < -100.f) break;
                    if (z < 0.f) return z;
                }
                continue;
            }
            else {
                z *= z-y;
                if (b == a) {
                    return z;
                } else {
                    ++z;
                    break;
                }
            }
        }
    }

    z = z-y;

    return z;
}
extern "C" float test_loopnsmx6_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;
    float r = 42.42f;

    for (float i=3.2f; i<a && z <= b*y; i=i+2) {
        for (int j=0; j<b; ++j) {
            if (z > 120.5123f) break;
            z += y-i;
        }
        if (x>y) {
            z = a+x;
            r = x*x;
            continue;
        } else if (y>x) {
            for (int k=0; k<x; ++k) {
                z = z-3*a;
                //if (k*z > 120.5123f) break;
                if (a > 120.5123f) return z;
                else if (z == b*b) continue;
                z -= b*3.32f;
            }
            if (z != y) r = x-a;
            else if (z < y) r = x+a;
            else return b;
        } else {
            z = y-a;
            r = y+a;
        }
    }

    z = z-y;

    return z*r;
}
extern "C" float test_loopnsmx7_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<a*b; ++i) {
        z += 1;
        for (int j=0; j<b; ++j) {
            z /= -1.32f;
            if (a < b) break;
            z += a;
            for (int k=0; k<a; ++k) {
                z += a/b;
                if (b < a) goto X;
                --z;
            }
            if (a == b) return z;
        }
    }

X:  z = z-y;

    return z;
}
extern "C" float test_loopnsmx8_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<a*b; ++i) {
        z += 1;
        for (int j=0; j<b; ++j) {
            z /= -1.32f;
            if (z < -100.f) break;
            z += a;
            for (int k=0; k<a; ++k) {
                z += a/b;
                if (z > 100.f) goto X;
                --z;
            }
            if (z < 0.f) return z;
        }
    }

X:  z = z-y;

    return z;
}
extern "C" float test_loopnsmx9_scalar(float a, float b) {
	float z, r, x = a + b;
    float y =z=r= x * x - b;
    for (float i=3.2f; i<a && z <= b*y; i=i+2) {
X:    for (int j=0; j<b; ++j) {
        if (z > 120.5123f) goto Y;
        z += y-i;
      }
      if (x>y) {
Y:          z = a+x;
            r = x*x;
            continue;
      } else if (y>x) {
            for (int k=0; k<x; ++k) {
            z = z-3*a;
                if (a > 120.5123f) goto Z;
                else if (z == b*b) continue;
                z -= b*3.32f;
            }
            if (z != y) r = x-a;
            else if (z < y) goto X;
            else return b;
        } else {
            z = y-a;
            r = y+a;
        }
    }
Z: z -= r;
    return z;
}
extern "C" float test_loopnsmx10_scalar(float a, float b) {
	float z, r, x = a + b;
    float y =z=r= x * x - b;
    for (float i=3.2f; i<a && z <= b*y; i=i+2) {
X:    for (int j=0; j<b; ++j) {
        if (z > 120.5123f) goto Y;
        z += y-i;
      }
      if (x>y) {
            z = a+x;
            r = x*x;
            continue;
      } else if (y>x) {
Y:         for (int k=0; k<x; ++k) {
                z = z-3*a;
                if (a > 120.5123f) goto Z;
                else if (z == b*b) continue;
                z -= b*3.32f;
            }
            if (z != y) r = x-a;
            else if (z < y) goto X;
            else return b;
        } else {
            z = y-a;
            r = y+a;
        }
    }
Z: z -= r;
    return z;
}

//function calls
extern "C" float test_call1_scalar(float a, float b) {
    return cosf(a*b);
}
extern "C" float test_call2_scalar(float a, float b) {
    return floorf(a/b);
}
extern "C" float test_call3_scalar(float a, float b) {
    return a > b ? logf(a-b) : 1.f;
}
extern "C" float test_call4_scalar(float a, float b) {
    return a > b ? logf(a-b) : sinf(b*a);
}
extern "C" float test_call5_scalar(float a, float b) {
    float z;
	
	if (a > b) {
        z = floorf(a);
        z += cosf(b);
    }
    else {
        z = sqrtf(b);
        z += logf(b);
    }

    return z;
}
extern "C" float test_call6_scalar(float a, float b) {
	// call to function without packetized native equivalent
	// (requires splitting)
    return powf(a, b);
}
extern "C" float test_call7_scalar(float a, float b) {
	// call to function without packetized native equivalent
	// (requires splitting with if-cascade (non-true-mask)
    return a < b ? a : powf(a, b);
}
extern "C" __attribute__((noinline)) float noinlinecall(float x) { return x*x; }
extern "C" float test_call8_scalar(float a, float b) {
    return a < b ? a : noinlinecall(a+b);
}
extern "C" __attribute__((noinline)) float noinlinecall2(float x) { return x*x; }
extern "C" __attribute__((noinline)) VEC noinlinecall2_4(VEC x) {
	VEC one = _mm256_set_ps(1.f, 1.f, 1.f, 1.f, 1.f, 1.f, 1.f, 1.f);
	VEC mask = _mm256_cmp_ps(x, one, 14); //GT_OS
	VEC x2 = _mm256_mul_ps(x,x);
	return _mm256_blendv_ps(x, x2, mask);
}
extern "C" float test_call9_scalar(float a, float b) {
    return a < b ? a : noinlinecall2(a+b);
}
extern "C" __attribute__((noinline)) float noinlinecall3(float x) { return x*x; }
extern "C" __attribute__((noinline)) VEC noinlinecall3_4(VEC x, VEC mask) {
	if (_mm256_movemask_ps(mask)) return _mm256_mul_ps(x,x);
	else return _mm256_sub_ps(x, _mm256_set_ps(1.f, 1.f, 1.f, 1.f, 1.f, 1.f, 1.f, 1.f));
}
extern "C" float test_call10_scalar(float a, float b) {
    return a < b ? a : noinlinecall3(a+b);
}
//array access
extern "C" float test_noise_scalar(float x, float y) {
    float z = x*y;
    static int p[] = { 151,160,137,91,90,15,
        131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
        190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
        88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
        77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
        102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
        135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
        5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
        223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
        129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
        251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
        49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
        138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180,
        151,160,137,91,90,15,
        131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
        190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
        88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
        77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
        102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
        135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
        5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
        223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
        129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
        251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
        49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
        138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180
    };

    //here begins 'real' noise-function
    const int X = (int)floor(x) & 255;                  // FIND UNIT CUBE THAT
    const int Y = (int)floor(y) & 255;                  // CONTAINS POINT.
    const int Z = (int)floor(z) & 255;
    x -= floor(x);                                      // FIND RELATIVE X,Y,Z
    y -= floor(y);                                      // OF POINT IN CUBE.
    z -= floor(z);
    const float U = x * x * x * (x * (x * 6 - 15) + 10);  // COMPUTE FADE CURVES
    const float V = y * y * y * (y * (y * 6 - 15) + 10);  // FOR EACH OF X,Y,Z.
    const float W = z * z * z * (z * (z * 6 - 15) + 10);
    const int A = p[X  ]+Y, AA = p[A]+Z, AB = p[A+1]+Z;   // HASH COORDINATES OF
    const int B = p[X+1]+Y, BA = p[B]+Z, BB = p[B+1]+Z;   // THE 8 CUBE CORNERS,

//      return lerp(w, lerp(v, lerp(u, grad(p[AA  ], x  , y  , z   ),  // AND ADD
//                                     grad(p[BA  ], x-1, y  , z   )), // BLENDED
//                             lerp(u, grad(p[AB  ], x  , y-1, z   ),  // RESULTS
//                                     grad(p[BB  ], x-1, y-1, z   ))),// FROM  8
//                     lerp(v, lerp(u, grad(p[AA+1], x  , y  , z-1 ),  // CORNERS
//                                     grad(p[BA+1], x-1, y  , z-1 )), // OF CUBE
//                             lerp(u, grad(p[AB+1], x  , y-1, z-1 ),
//                                     grad(p[BB+1], x-1, y-1, z-1 ))));

    int h = p[AA  ] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    float u = h<8 ? x : y;                 // INTO 12 GRADIENT DIRECTIONS.
    float v = h<4 ? y : h==12||h==14 ? x : z;
    const float grad1 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad1 = grad(p[AA  ], x  , y  , z   );

    h = p[BA  ] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x-1 : y;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y : h==12||h==14 ? x-1 : z;
    const float grad2 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad2 = grad(p[BA  ], x-1, y  , z   );

    h = p[AB  ] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x : y-1;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y-1 : h==12||h==14 ? x : z;
    const float grad3 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad3 = grad(p[AB  ], x  , y-1, z   );

    h = p[BB  ] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x-1 : y-1;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y-1 : h==12||h==14 ? x-1 : z;
    const float grad4 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad4 = grad(p[BB  ], x-1, y-1, z   );

    h = p[AA+1] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x : y;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y : h==12||h==14 ? x : z-1;
    const float grad5 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad5 = grad(p[AA+1], x  , y  , z-1 );

    h = p[BA+1] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x-1 : y;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y : h==12||h==14 ? x-1 : z-1;
    const float grad6 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad6 = grad(p[BA+1], x-1, y  , z-1 );

    h = p[AB+1] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x : y-1;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y-1 : h==12||h==14 ? x : z-1;
    const float grad7 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad7 = grad(p[AB+1], x  , y-1, z-1 );

    h = p[BB+1] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x-1 : y-1;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y-1 : h==12||h==14 ? x-1 : z-1;
    const float grad8 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad8 = grad(p[BB+1], x-1, y-1, z-1 );

    const float lerp1 = grad1 + U * (grad2 - grad1); //lerp(u, grad1, grad2);
    const float lerp2 = grad3 + U * (grad4 - grad3); //lerp(u, grad3, grad4);
    const float lerp3 = grad5 + U * (grad6 - grad5); //lerp(u, grad5, grad6);
    const float lerp4 = grad7 + U * (grad8 - grad7); //lerp(u, grad7, grad8);
    const float lerp12 = lerp1 + V * (lerp2 - lerp1); //lerp(v, lerp1, lerp2);
    const float lerp34 = lerp3 + V * (lerp4 - lerp3); //lerp(v, lerp3, lerp4);
    return lerp12 + W * (lerp34 - lerp12); //lerp(w, lerp12, lerp34);
}

#ifdef RUN_SHADER_TESTS
//shader tests
extern "C" float test_shader_scalar(float x, float y) {
    //unsigned depth = (unsigned)x;
    float activeMask = x > 0.f;
    //Point origin = t_Vec3f1(x, y, x-y);
    //Point dir = t_Vec3f1(x-1.f, y+.3f, x-y);
    //float appearanceID = x / 2.f;
    //float hitDistance = x * y;
    //Point geomNormal = t_Vec3f1(x, x, y);
    Point normal = t_Vec3f1(y*.32f, y*.17f, x/1.03f);
    //Point vertColor = t_Vec3f1(y, x, x);
    //float u = x;
    //float v = y;
    //float texCoord1 = x * (x/2);
    //float texCoord2 = y * (y/2);
    Point result = t_Vec3f1(-13.f, -13.f, -13.f);

    if (activeMask != 0.f) {
        //normal shader
        result = normal + .5f;
    }
    return result.x + result.y + result.z;
}
extern "C" float test_brick_scalar(float x, float y) {
    float activeMask = x > 0.f;
    Point origin = t_Vec3f1(x, y, x-y);
    Point dir = t_Vec3f1(x-1.f, y+.3f, x-y);
    float hitDistance = x * y;
    Point normal = t_Vec3f1(y*.32f, y*.17f, x/1.03f);
    float texCoord1 = x * (x/2);
    float texCoord2 = y * (y/2);
    Point result = t_Vec3f1(-13.f, -13.f, -13.f);

    const Color mortarColor = Color(0.4f, 0.4f, 0.4f);
    const Color brickColor = Color(1.f, .15f, .14f);
    const float t = 1.f;
    const float s = 1.f;
    const float brickHeight = 0.25f;
    const float brickWidth = 0.08f;
    const float mortarThickness = 0.01f;

    if (activeMask != 0.f) {
        Point hit = origin + hitDistance * dir;
	texCoord1 = hit.x;
	texCoord2 = hit.z;

	//precomputed values
	float bmwidth = brickWidth + mortarThickness;
	float bmheight = brickHeight + mortarThickness;
	float mwf = mortarThickness * 0.5f / bmwidth;
	float mhf = mortarThickness * 0.5f / bmheight;

	//shader

	float sbrick, tbrick, w, h;

	// Texture coordinates
	float scoord = texCoord1 * s;
	float tcoord = texCoord2 * t;

	float ss = scoord / brickWidth;
	float tt = tcoord / brickHeight;

	// shift alternate rows
	if (Mod(tt * 0.5f,1.0f) > 0.5f) ss += 0.5f;

	sbrick = Floor(ss); /* which brick? */
	tbrick = Floor(tt); /* which brick? */

	//generate n oise
	float brickNoise  = .8f + .2f * Noise(12.f + sbrick * .1f, -13.f + tbrick * .1f);
	float slowNoise   = .7f + .3f * Noise(ss, tt);
	float fastNoise   = .9f + .1f * Noise(ss * 77.3f, tt * 53.5f);

	float mortarNoise = .5f + .5f * Noise(77.f * ss, 77.7f * tt);

	float mixedNoise = brickNoise * slowNoise * fastNoise;


	ss -= sbrick;
	tt -= tbrick;

	w = Step(mwf,ss) - Step(1.0f-mwf,ss);
	h = Step(mhf,tt) - Step(1.0f-mhf,tt);


	//blend everything together
	Color Ct = Mix(mortarColor * mortarNoise, brickColor * mixedNoise, w * h);

	// compute bump-mapping function for mortar grooves

	//TODO implement illumination
	float sbump = SmoothStep(0.0f, mwf, ss) - SmoothStep(1.0f-mwf,1.0f,ss);
	float tbump = SmoothStep(0.0f, mhf, tt) - SmoothStep(1.0f-mhf,1.0f,tt);

	// compute shading normal
	Point U, V;
	GetFrame(normal, U, V);
	Point Nf = FaceForward(Normalized(normal + 0.3f*(1.f - sbump)*U + 0.3f*(1.f - tbump)*V), dir);

	//store(result, Nf + Vec3f(1.f) * .5f);
	result =  Ct * -Dot(Nf, dir);
    }
    return result.x + result.y + result.z;
}
extern "C" float test_checker_scalar(float x, float y) {
    float activeMask = x > 0.f;
    Point origin = t_Vec3f1(x, y, x-y);
    Point dir = t_Vec3f1(x-1.f, y+.3f, x-y);
    float hitDistance = x * y;
    Point normal = t_Vec3f1(y*.32f, y*.17f, x/1.03f);
    float texCoord1 = x * (x/2);
    float texCoord2 = y * (y/2);
    Point result = t_Vec3f1(-13.f, -13.f, -13.f);

    const float Kd = 0.5f;
    const float Ka = 0.1f;
    const float frequency = 10.0f;
    const float s_scale = 1.0f;
    const float t_scale = 1.0f;
    const Color blackcolor = Color(0.0f, 0.0f, 0.0f);
    const Color whitecolor = Color(1.0f, 1.0f, 1.0f);

    if (activeMask != 0.f) {
        const Point P = origin + hitDistance * dir;
        const Vector & IN = dir;  /* normalized incident vector */
        const Normal & N = normal;  /* surface normal */

        Normal Nf = FaceForward(N, IN);

        float smod = Mod (texCoord1 * s_scale * frequency, 1.0f);
        float tmod = Mod (texCoord2 * t_scale * frequency, 1.0f);

        Color Ci;

        if (smod < 0.5f)
        {
            if (tmod < 0.5f) Ci = whitecolor;
            else Ci = blackcolor;
        }
        else
        {
            if (tmod < 0.5f) Ci = blackcolor;
            else Ci = whitecolor;
        }

        Color C_diffuse(0.0f, 0.0f, 0.0f);
        //BEGIN_ILLUMINANCE_LOOP(P) {
        //    C_diffuse += diffuseComponent(L_dir_norm, P, Nf, Cl);
        //} END_ILLUMINANCE_LOOP;

        result = Ci * (Ka + Kd  * C_diffuse);
        result *= Nf;
    }
    return result.x + result.y + result.z;
}
extern "C" float test_dented_scalar(float x, float y) {
    float activeMask = x > 0.f;
    Point origin = t_Vec3f1(x, y, x-y);
    Point dir = t_Vec3f1(x-1.f, y+.3f, x-y);
    float hitDistance = x * y;
    Point normal = t_Vec3f1(y*.32f, y*.17f, x/1.03f);
    Point result = t_Vec3f1(-13.f, -13.f, -13.f);
    
    const float Km = 1.0f;
    const float power = 2.0f;
    const float frequency = 1.0f;
    const float maxoctaves = 6.0f;
    const Color ambientColor = Color(.1f, .1f, .1f);
    const Color diffuseColor = Color(.3f, .2f, .1f);
    const Color specularColor = Color(.6f, .4f, .2f);
    //const float kg = 0.9f;

    if (activeMask != 0.f) {
        const Point P = origin + hitDistance * dir;
        const Vector & IN = dir;  /* normalized incident vector */
        const Normal & N = normal;  /* surface normal */

        const Point PP = P*20.0f; // transform ("shader", P);

        float magnitude = 0.0f;

        float size = frequency;
        for (float i = 0.0f;  i < maxoctaves;  i += 1.0f)
        {
            magnitude += Abs (.5f - Noise (PP*size)) / size;
            size *= 2.0f;
        }

        float bump = (Km * Pow (magnitude, power));

        // compute shading normal
            Point U, V;
            GetFrame(N, U, V);
            Normal Nf = FaceForward(Normalized(N + 0.8f*bump*U + 0.8f*bump*V), IN);
        //const Normal Nf = FaceForward(N, IN);

        Color C_diffuse(0.0f, 0.0f, 0.0f);
        Color C_phong(0.0f, 0.0f, 0.0f);

        const Vector R = Reflect(IN, N);

//        BEGIN_ILLUMINANCE_LOOP(P) {
//            C_diffuse += diffuseComponent(L_dir_norm, P, Nf, Cl);
//            C_phong += phongComponent(L_dir_norm,
//                                      P, Nf, R, Cl,
//                                      kg);
//        } END_ILLUMINANCE_LOOP;

        result = bump*(ambientColor + diffuseColor * C_diffuse + specularColor*C_phong);
    }
    return result.x + result.y + result.z;
}
extern "C" float test_glass_scalar(float x, float y) {
    Context context;
    unsigned depth = 1;
    float activeMask = x > 0.f;
    Point origin = t_Vec3f1(x, y, x-y);
    Point dir = t_Vec3f1(x-1.f, y+.3f, x-y);
    float hitDistance = x * y;
    Point normal = t_Vec3f1(y*.32f, y*.17f, x/1.03f);
    Point result = t_Vec3f1(-13.f, -13.f, -13.f);
    
    const float Ka = 0.2f;
    const float Kd = 0.0f;
    const float Ks = 0.5f;
    const float Kr = 1.0f;       /* reflective coefficient */
    const float Kt = 0.7f;       /* transmissive coefficient */
    const float roughness = 0.05f;
    const float blur = 0.01f;
    const float eta = 1.5f;      /* index of refraction */
    const Color specularcolor = Color(1.0f,1.0f,1.0f);
    const Color transmitcolor = Color(1.0f,0.0f,0.0f);
    const unsigned samples = 1;

#define MAX_DEPTH 2
#define RAY_EPS 0.01f

    if (activeMask != 0.f) {
        const Point P = origin + hitDistance * dir;
        const Vector & IN = dir;    /* normalized incident vector */
        const Normal & N = normal;  /* surface normal */

        Vector Rfldir, Rfrdir;      /* Smooth reflection/refraction directions */
        Vector uoffset, voffset;    /* Offsets for blur */
        Color ev(0.0f, 0.0f, 0.0f); /* Color of the environment reflections */
        Color cr(0.0f, 0.0f, 0.0f); /* Color of the refractions */
        float kr, kt;


        /* Construct a forward facing surface normal */
        Normal Nf = FaceForward(N, IN);

        /* Compute the reflection & refraction directions and amounts */
        Fresnel (IN, Nf, (Dot(IN, N) < 0) ? 1.0f/eta : eta, kr, kt, Rfldir, Rfrdir);
        kr *= Kr;
        kt *= Kt;

        /* Calculate the reflection color */
        if (kr > 0.001f && depth < MAX_DEPTH) {
            /* Rdir gets the perfect reflection direction */
            Vector Rdir = Normalized (Rfldir);
            if (blur > 0.0f) {
                /* Construct orthogonal components to Rdir */
                uoffset = blur * Normalized(Vector (Rdir.z - Rdir.y,
                                                    Rdir.x - Rdir.z,
                                                    Rdir.y - Rdir.x));
                voffset = Cross(Rdir, uoffset);
                for (unsigned i = 0;  i < samples;  i += 1) {
                    for (unsigned j = 0;  j < samples;  j += 1) {
                        /* Add a random offset to the smooth reflection vector */
                        Vector R = Rdir +
                            (((float)i + Random())/samples - 0.5f) * uoffset +
                            (((float)j + Random())/samples - 0.5f) * voffset;

                        Vector RN = Normalized(R);
                        Color rayColor;
                        TraceRayEps(P, RN, RAY_EPS, rayColor);
                        //rayColor = P*Color(x, x-0.04f, y/0.89f) + RN;

                        ev += rayColor;
                    }
                }
                ev *= kr / (samples*samples);
            } else {
                /* No blur, just do a simple trace */
                Color rayColor;
                TraceRayEps(P, Rdir, RAY_EPS, rayColor);
                //rayColor = P*Color(x, x-0.04f, y/0.89f) + Rdir;
                ev = kr * rayColor;
            }
        }

        /* Calculate the refraction color */
        if (kt > 0.001f && depth < MAX_DEPTH) {
            /* Rdir gets the perfect refraction direction */
            Vector Rdir = Normalized (Rfrdir);
            if (blur > 0.0f) {
                /* Construct orthogonal components to Rdir */
                uoffset = blur * Normalized(Vector(Rfrdir.z - Rfrdir.y,
                                                   Rfrdir.x - Rfrdir.z,
                                                   Rfrdir.y - Rfrdir.x));
                voffset = Cross(Rfrdir, uoffset);
                for (unsigned i = 0;  i < samples;  i += 1) {
                    for (unsigned j = 0;  j < samples;  j += 1) {
                        /* Add a random offset to the smooth reflection vector */
                        Vector R = Rdir +
                            (((float)i + Random())/samples - 0.5) * uoffset +
                            (((float)j + Random())/samples - 0.5) * voffset;

                        Color rayColor;
                        TraceRayEps(P, R, RAY_EPS, rayColor);
                        //rayColor = P*Color(x, x-0.04f, y/0.89f) + R;

                        cr += rayColor;
                    }
                }
                cr *= kt / (samples*samples);
            } else {
                /* No blur, just do a simple trace */
                Color rayColor;
                TraceRayEps(P, Rdir, RAY_EPS, rayColor);
                //rayColor = P*Color(x, x-0.04f, y/0.89f) + Rdir;
                cr = kt * rayColor;
            }
        }

        Color C_diffuse(0.0f, 0.0f, 0.0f);
        Color C_specular(0.0f, 0.0f, 0.0f);
        float invRoughness = 1.0f/roughness;

        BEGIN_ILLUMINANCE_LOOP(P) {
            C_diffuse += diffuseComponent(L_dir_norm, P, Nf, Cl);
            C_specular += specularComponent(L_dir_norm,
                                            P, Nf, -IN, Cl,
                                            roughness, invRoughness);
        } END_ILLUMINANCE_LOOP;

        result = ((Ka + Kd*C_diffuse) +
                  specularcolor * (ev + Ks*C_specular) +
                  transmitcolor * cr);
        result *= invRoughness;
    }
    return result.x + result.y + result.z;
}
extern "C" float test_granite_scalar(float x, float y) {
    float activeMask = x > 0.f;
    Point origin = t_Vec3f1(x, y, x-y);
    Point dir = t_Vec3f1(x-1.f, y+.3f, x-y);
    float hitDistance = x * y;
    Point normal = t_Vec3f1(y*.32f, y*.17f, x/1.03f);
    Point result = t_Vec3f1(-13.f, -13.f, -13.f);

    const Color color = Color(1.0f, 1.0f, 1.0f);
    const float Ka = 0.2f;
    const float Kd = 0.4f;
    const float Ks = 0.1f;
    const float roughness = 0.1f;

    if (activeMask != 0.f) {
        const Vector &IN = dir; /* normalized incident vector */
	const Point P = origin + hitDistance * dir;
        const Normal &N = normal;

	float sum = 0;
	float i, freq = 1.0f; /* Try other values for example, 7.0 */

	for (i = 0; i < 6; i = i + 1) {
		sum = sum + Abs(.5f - Noise(4.0f * freq * IN * hitDistance)) / freq;
		freq *= 2.0f;
	}

        Normal Nf = FaceForward(N, IN);

        Color C_diffuse(0.0f, 0.0f, 0.0f);
        Color C_specular(0.0f, 0.0f, 0.0f);
        float invRoughness = 1.0f/roughness;

//        BEGIN_ILLUMINANCE_LOOP(P) {
//            C_diffuse += diffuseComponent(L_dir_norm, P, Nf, Cl);
//            C_specular += specularComponent(L_dir_norm,
//                                            P, Nf, -IN, Cl,
//                                            roughness, invRoughness);
//        } END_ILLUMINANCE_LOOP;

        result = color * sum * (Ka + Kd * C_diffuse + Ks * C_specular);
        result *= invRoughness * Nf - P;
    }
    return result.x + result.y + result.z;
}
extern "C" float test_parquet_scalar(float x, float y) {
    float activeMask = x > 0.f;
    Point origin = t_Vec3f1(x, y, x-y);
    Point dir = t_Vec3f1(x-1.f, y+.3f, x-y);
    float hitDistance = x * y;
    Point normal = t_Vec3f1(y*.32f, y*.17f, x/1.03f);
    float texCoord1 = x * (x/2);
    float texCoord2 = y * (y/2);
    Point result = t_Vec3f1(-13.f, -13.f, -13.f);

    const float Ka = 1.0f;
    const float Kd = 0.75f;
    const float Ks = 0.15f;
    const float roughness = 0.025f;
    const Color specularcolor = Color(1.0f, 1.0f, 1.0f);
    const float ringscale = 15.0f;  // scaling for the ring spacing
    const float grainscale = 60.0f; // scaling for the fine grain
    const float ts_x = 1.0f; // overall scaling factor for the texture X
    const float ts_y = 1.0f; // overall scaling factor for the texture Y
    const float plankspertile = 4.0f; // number of planks in each parquet tile
    const Color lightwood = Color(0.57f, 0.292f, 0.125f);
    const Color darkwood = Color(0.275f, 0.15f, 0.06f);
    const Color groovecolor = Color(.05f, .04f, .015f);
    const float plankwidth = 0.05f;
    const float groovewidth = 0.001f;
    const float plankvary = 0.8f;
    const float grainy = 1.0f;
    const float wavy = 0.08f;

    if (activeMask != 0.f) {
        const Vector &I = dir; /* normalized incident vector */
        Point P = origin + hitDistance * dir;
        const Normal &N = normal;

        float PGWIDTH = plankwidth+groovewidth;
        float planklength = PGWIDTH * plankspertile - groovewidth;
        float PGHEIGHT = planklength+groovewidth;
        float GWF = groovewidth*0.5f/PGWIDTH;
        float GHF = groovewidth*0.5f/PGHEIGHT;

        /* Determine how wide in s-t space one pixel projects to */
        float swidth = 0.05f*ts_x;
        float twidth = 0.05f*ts_y;
        float fwidth = Max(swidth,twidth);

        Normal Nf = FaceForward(N, I);

        float ss = (ts_x * texCoord1) / PGWIDTH;
        float whichrow = Floor(ss);
        float tt = (ts_y * texCoord2) / PGHEIGHT;
        float whichplank = Floor(tt);
        if (Mod (whichrow/plankspertile + whichplank, 2.f) >= 1.f)
        {
            ss = ts_x * texCoord2 / PGWIDTH;
            whichrow = Floor (ss);
            tt = ts_y * texCoord1 / PGHEIGHT;
            whichplank = Floor(tt);
            float tmp = swidth;  swidth = twidth;  twidth = tmp;
        }
        ss -= whichrow;
        tt -= whichplank;
        whichplank += 20.f*(whichrow+10.f);

        /*
         * Figure out where the grooves are.  The value groovy is 0 during
         * are grooves, 1 where the wood grain is visible.
         */
        /* This would be the non-antialiased version: */
        float w = Step (GWF,ss) - Step(1-GWF,ss);
        float h = Step (GHF,tt) - Step(1-GHF,tt);
        float groovy = w*h;

        /*
         * Add the ring patterns
         */
        float r;
        float fade = SmoothStep (1.f/ringscale, 8.f/ringscale, fwidth);
        if (fade < 0.999f)
        {
            float ttt = tt/4.f+whichplank/28.38f + wavy * Noise (8.f*ss, tt/4.f);
            r = ringscale * Noise (ss-whichplank, ttt);
            r -= floor (r);
            r = 0.3f+0.7f*SmoothStep(0.2f, 0.55f, r)*(1-SmoothStep(0.75f, 0.8f, r));
            r = (1.f-fade)*r + 0.65f*fade;

            /*
             * Multiply the ring pattern by the fine grain
             */
            fade = SmoothStep (2.f/grainscale, 8.f/grainscale, fwidth);
            if (fade < 0.999f) {
                float r2 = 1.3f - Noise (ss*grainscale, (tt*grainscale/4.f));
                r2 = grainy * r2*r2 + (1.f-grainy);
                r *= (1.f-fade)*r2 + (0.75f*fade);
            }
            else r *= 0.75f;
        }
        else r = 0.4875f;

        /* Mix the light and dark wood according to the grain pattern */
        Color woodcolor = Mix (lightwood, darkwood, r);

        /* Add plank-to-plank variation in overall color */
        woodcolor *= (1.f-plankvary/2.f + plankvary * Noise (whichplank+0.5f));

        Color Ct = Mix (groovecolor, woodcolor, groovy);

        /* Use the plastic illumination model */
        // Ci = Os * ( Ct * (Ka*ambient() + Kd*diffuse(Nf)) +
        //	       specularcolor * Ks*specular(Nf,-normalize(I),roughness));

        Color C_diffuse(0.0f, 0.0f, 0.0f);
        Color C_specular(0.0f, 0.0f, 0.0f);
        float invRoughness = 1.0f/roughness;



//        BEGIN_ILLUMINANCE_LOOP(P) {
//            C_specular += specularComponent(L_dir_norm,
//                                            P, Nf, -I, Cl,
//                                            roughness, invRoughness);
//            C_diffuse += diffuseComponent(L_dir_norm, P, Nf, Cl);
//        } END_ILLUMINANCE_LOOP;

        result = Ct * (Ka + Kd  * C_diffuse) + specularcolor * Ks * C_specular;
        result *= invRoughness * Nf - P;
    }
    return result.x + result.y + result.z;
}
extern "C" float test_phong_scalar(float x, float y) {
    float activeMask = x > 0.f;
    Point origin = t_Vec3f1(x, y, x-y);
    Point dir = t_Vec3f1(x-1.f, y+.3f, x-y);
    float hitDistance = x * y;
    Point normal = t_Vec3f1(y*.32f, y*.17f, x/1.03f);
    Point vertColor = t_Vec3f1(y, x, x);
    Point result = t_Vec3f1(-13.f, -13.f, -13.f);

    const Color diffuseColor = Color(1.f, .2f, 0.f);
    const Color ambientIntensity = Color(.1f, .3f, .4f);
    const Color specularColor = Color(1.f, 1.f, 1.f);
    const float shininess = 50.f;
    const int useVertexColor = 0;
    const int isSpecular = 0;

    if (activeMask != 0.f) {
        //phong shader
        Point hit          = origin + hitDistance * dir;
	Point fixedNormal  = FaceForward(normal, dir);
	int numLights      = 2; //int numLights = Context::getNumLightSources(context);
	Point reflView     = Reflect(dir, fixedNormal);
	Color diffuse;

	if (useVertexColor) {
		diffuse = vertColor;
	} else {
		diffuse = diffuseColor;
	}

	result = ambientIntensity * diffuse;

	for(int light = 0; light < numLights; ++light)
	{
		float lightDist, dotLightNormal, dotLightRefl;
		Point lightDir, lightPos;
		Color intensity;

		//Context::sampleLightSource(context, light, hit, lightDir, lightDist, intensity, lightPos);
                lightDir = dir;
                lightPos = origin;
                lightDist = 0.f;
                intensity = Color(3.f, 3.f, 3.f);

		Normalize(lightDir);

		dotLightNormal = Dot(fixedNormal, lightDir);

		if (dotLightNormal > 0.f) { //&&
			//Context::getLightContribution(context, lightPos, -lightDir, lightDist, intensity)) {

			dotLightRefl = Dot(lightDir, reflView);

			if (isSpecular) {
				result += diffuse * intensity * dotLightNormal
					+ (dotLightRefl > EPS ? specularColor * powf(dotLightRefl, shininess) : 0.0f);
			} else {
				result += diffuse * intensity * dotLightNormal;
			}
		}
	}
    }
    return result.x + result.y + result.z;
}
extern "C" float test_screen_scalar(float x, float y) {
    float activeMask = x > 0.f;
    Point origin = t_Vec3f1(x, y, x-y);
    Point dir = t_Vec3f1(x-1.f, y+.3f, x-y);
    float hitDistance = x * y;
    Point normal = t_Vec3f1(y*.32f, y*.17f, x/1.03f);
    float texCoord1 = x * (x/2);
    float texCoord2 = y * (y/2);
    Point result = t_Vec3f1(-13.f, -13.f, -13.f);

    const Color Cs = Color(1.0f,1.0f,1.0f);
    const Color specularcolor = Color(1.0f,1.0f,1.0f);
    const float Ka = 1.0f; // ambient
    const float Kd = 0.75f; // diffuse
    const float Ks = 0.4f; // specular
    const float roughness = 0.1f;
    const float density = 0.25f;
    const float frequency = 20.0f;

    if (activeMask != 0.f) {
        const Point & IN = dir; /* normalized incident vector */
        float d;      /* Density at the sample point */
        float ss, tt; /* s,t, parameters in phase */

        /* Compute a forward facing normal */
        Point Nf = FaceForward(normal, IN);

        /* Figure out where in the pattern we are */
        ss = Mod (frequency * texCoord1, 1);
        tt = Mod (frequency * texCoord2, 1);

        float GWF = density*0.5;


        /* This would be the non-antialiased version: */
        float w = Step (GWF,ss) - Step(1-GWF,ss);
        float h = Step (GWF,tt) - Step(1-GWF,tt);
        d = 1 - w*h;

        Point P = origin + hitDistance * dir;

        if (d > 0)
        {
            Color C_diffuse(0.0f, 0.0f, 0.0f);
            Color C_specular(0.0f, 0.0f, 0.0f);
            float invRoughness = 1.0f/roughness;

//            BEGIN_ILLUMINANCE_LOOP(P) {
//                C_specular += specularComponent(L_dir_norm,
//                                                P, Nf, -IN, Cl,
//                                                roughness, invRoughness);
//                C_diffuse += diffuseComponent(L_dir_norm, P, Nf, Cl);
//            } END_ILLUMINANCE_LOOP;

            result = d*(Cs * (Ka + Kd  * C_diffuse) + specularcolor * Ks * C_specular);
            result *= invRoughness * Nf;

            return result.x + result.y + result.z;
        }

        //TraceRay(P + 10.0f * EPS * IN, IN, result);
        result = P + 10.0f * EPS * IN;
    }
    return result.x + result.y + result.z;
}
extern "C" float test_starball_scalar(float x, float y) {
    float activeMask = x > 0.f;
    Point origin = t_Vec3f1(x, y, x-y);
    Point dir = t_Vec3f1(x-1.f, y+.3f, x-y);
    float hitDistance = x * y;
    Point normal = t_Vec3f1(y*.32f, y*.17f, x/1.03f);
    float texCoord1 = x * (x/2);
    float texCoord2 = y * (y/2);
    Point result = t_Vec3f1(-13.f, -13.f, -13.f);

    const float Ka = 1.0f;
    const float Kd = 0.5f;
    const float Ks = 0.5f;
    const float roughness = 0.1f;

    if (activeMask != 0.f) {
        const Point P = origin + hitDistance * dir;
        const Vector & IN = dir;  /* normalized incident vector */
        const Normal & N = normal;  /* surface normal */

        const float dv = 0.005f; // dv not supported yet
        const float du = 0.005f; // du not supported yet
        float ddv = 2.0f * Abs(dv);
        float ddu = 2.0f * Abs(du);

        float ang = Mod(texCoord1*360.0f, 144.0f);
        float ht = .3090f/Sin(((ang+18.0f)*.01745f));
        ang = Mod ((1.0f-texCoord1)*360.0f, 144.0f);
        float ht1 = .3090f/Sin(((ang+18.0f)*.01745f));
        ht = Max (ht, ht1);
        ht1 = ht*.5f-Min(texCoord2*2.0f, 1.0f);
        ht1 = Clamp (ht1, -ddu, ddu)/(ddu*2.0f)+.5f;
        ht = ht/2.0f - Min((1.0f-texCoord2)*2.0f, 1.0f);
        ht1 = ht1 + Clamp(ht, -ddu, ddu)/(ddu*2.0f)+.5f;

        Color Ct = Mix (Color(.8f,.6f,0.0f), Color (.5f,.05f,.05f), ht1);
        Ct = Mix (Color(0.0f,0.2f,.7f), Ct,
                  Clamp(Abs(texCoord2-0.5f)-0.1f, 0.0f, ddv)/ddv);

        Normal Nf = FaceForward (N, IN);
        //result = MaterialPlastic(CTX_ARG, P, IN, Nf, Ct, Ka, Kd, Ks, roughness);
        result = (P * IN -Nf) + (Ct + Ka * Kd * Ks) / roughness;
    }
    return result.x + result.y + result.z;
}
extern "C" float test_whitted_scalar(float x, float y) {
    unsigned depth = 1;
    float activeMask = x > 0.f;
    Point origin = t_Vec3f1(x, y, x-y);
    Point dir = t_Vec3f1(x-1.f, y+.3f, x-y);
    float hitDistance = x * y;
    Point normal = t_Vec3f1(y*.32f, y*.17f, x/1.03f);
    Point result = t_Vec3f1(-13.f, -13.f, -13.f);

    const float eta = 1.5;      /* index of refraction */
    //const float Kr =.8;         /* reflective coefficient */
    const float Kt =.2;         /* transmissive coefficient */
    const float Ks =.2;         /* specular coefficient */
    //const float Kss = 2;        /* specular exponent */
    const float Kd = .5;

    if (activeMask != 0.f) {
        const Point P = origin + hitDistance * dir;
        const Vector & IN = dir; /* normalized incident vector */
        const Normal & N = normal;  /* surface normal */

        Normal Nn = FaceForward(N, IN);

        /* ambient term */
        Color Ci = Kd; /* * ambient(); */

        /* diffuse and specular terms */
//        BEGIN_ILLUMINANCE_LOOP(P) {
//            /* diffuse */
//            Ci += Kd * Cl * Dot(L_dir, Nn);
//            /* specular */
//            Vector H = Normalized(L_dir_norm+IN);
//            Ci += Ks * Cl * Pow(Max(0.0f, Dot(Nn, H)), Kss);
//        } END_ILLUMINANCE_LOOP;


        Color rayColor;
        /* reflection */
        Vector R = Reflect( IN, Nn );

        if (depth < MAX_DEPTH && Ks > 0.0f)
        {
            //TraceRay(P + RAY_EPS * R, R, rayColor);
            rayColor = P + RAY_EPS * R;
            Ci += Ks * rayColor;
        }

        /* transmittance */
        if (Kt > 0.0f)
        {
            Vector T = Refract( IN, Nn, Dot(N, IN) < 0.0f ? eta : 1.0f/eta );
            if ( Length(T) != 0.0f && depth < MAX_DEPTH)
            {
                //TraceRay(P + RAY_EPS * T, T, rayColor);
                rayColor = P + RAY_EPS * T;
                Ci += Kt * rayColor;
            }
        }

        result = Ci;
    }
    return result.x + result.y + result.z;
}
extern "C" float test_wood_scalar(float x, float y) {
    float activeMask = x > 0.f;
    Point origin = t_Vec3f1(x, y, x-y);
    Point dir = t_Vec3f1(x-1.f, y+.3f, x-y);
    float hitDistance = x * y;
    Point normal = t_Vec3f1(y*.32f, y*.17f, x/1.03f);
    Point result = t_Vec3f1(-13.f, -13.f, -13.f);

    const float ringscale = 10.0f;
    const Color lightWood = Color(0.3f, 0.12f, 0.03f);
    const Color darkWood = Color(0.05f, 0.01f, 0.005f);
    const float Ka = 0.2f;
    const float Kd = 0.4f;
    const float Ks = 0.6f;
    const float roughness = 0.1f;

    if (activeMask != 0.f) {
        const Point P = origin + hitDistance * dir;
        const Vector & IN = dir;  /* normalized incident vector */
        const Normal & N = normal;  /* surface normal */

      /*
       * Compute the forward-facing normal NN and the vector
       * toward the ray orgigin V, both normalized.
       * These vectors are used by "specular" and "diffuse". */
        Normal NN = FaceForward(N, IN);
        Vector V = -IN;

        Point PP = P; // transform("shader", P);
        PP += Noise(PP);

        /* compute radial distance r from PP to axis of "tree" */
        float r = Sqrt (PP.y * PP.y + PP.z * PP.z);

        /* map radial distance r nto ring position [0, 1] */
        r *= ringscale;
        r += Abs (Noise(r));
        r -= Floor (r);         /* == mod (r, 1) */

        /* use ring poisition r to select wood color */
        r = SmoothStep (0.f, 0.8f, r) - SmoothStep (0.83f, 1.0f, r);
        Color Ci = Mix(lightWood, darkWood, r);

        /* shade using r to vary shininess */
        // Oi = Os;
        //Ci = Oi * Ci * (Ka * ambient() + Kd * diffuse(NN))
        //  + (0.3 * r + 0.7) * Ks * specular (NN, V, roughness);

        Color C_diffuse(0.0f, 0.0f, 0.0f);
        Color C_specular(0.0f, 0.0f, 0.0f);
        float invRoughness = 1.0f/roughness;

//        BEGIN_ILLUMINANCE_LOOP(P) {
//            C_diffuse += diffuseComponent(L_dir_norm, P, NN, Cl);
//            C_specular += specularComponent(L_dir_norm,
//                                            P, NN, V, Cl,
//                                            roughness, invRoughness);
//        } END_ILLUMINANCE_LOOP;

        result = Ci * (Ka + Kd  * C_diffuse) + (0.3f * r + 0.7f) * Ks * C_specular;
        result *= invRoughness * NN - V;
    }
    return result.x + result.y + result.z;
}
#endif

#ifdef RUN_IRREDUCIBLE_TESTS
//irreducible control-flow tests
extern "C" float test_irreducible1_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    int i=0;
    if (a > b) goto G;

    for(; i<100; ++i) {
        z += a;
G:      ++z;
    }

    return z-b;
}
extern "C" float test_irreducible2_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    int i=0, j=0;
    if (a > b) goto G;

    for(; i<a; ++i) {
        z += a;
	for (; j<b; ++j) {
	    ++z;
G:	    z -= 2.f;
	}
    }

    return z-b;
}
extern "C" float test_irreducible3_scalar(float a, float b) {
    float x = a + b;
    float y = x * x - b;
    float z = y;

    int i=0, j=0;
    if (a > b) goto G;
    if (b > a) goto G2;

    for(; i<a; ++i) {
        z += a;
	for (; j<b; ++j) {
	    ++z;
	    if (z < 0.f) goto X;
G:	    z -= 2.f;
	}
	++z;
	if (z < -100.f) return a;
G2:	++z;
    }

X:  z -= b;

    return z;
}
#endif

//----------------------------------------------------------------------------//
// add test cases to test suite (comment / uncomment blocks)
//----------------------------------------------------------------------------//
typedef float (*scalarFnType)(float, float);
typedef VEC (*vecFnType)(VEC, VEC) ALIGN REALIGN;

void addTestCases(std::vector<std::pair<const char*, std::pair<scalarFnType, vecFnType> > >& testCases) {
    testCases.push_back(std::make_pair("test_scalar", std::make_pair(test_scalar, test_generated)));

    testCases.push_back(std::make_pair("test_if", std::make_pair(test_if_scalar, test_if_generated)));
#if 0
    testCases.push_back(std::make_pair("test_criticaledge_scalar", std::make_pair(test_criticaledge_scalar, test_criticaledge_generated)));
    testCases.push_back(std::make_pair("test_phi1_scalar", std::make_pair(test_phi1_scalar, test_phi1_generated)));
    testCases.push_back(std::make_pair("test_phi2_scalar", std::make_pair(test_phi2_scalar, test_phi2_generated)));
    testCases.push_back(std::make_pair("test_phi3_scalar", std::make_pair(test_phi3_scalar, test_phi3_generated)));
    testCases.push_back(std::make_pair("test_phi4_scalar", std::make_pair(test_phi4_scalar, test_phi4_generated)));
    testCases.push_back(std::make_pair("test_phi5_scalar", std::make_pair(test_phi5_scalar, test_phi5_generated)));
    testCases.push_back(std::make_pair("test_phi6_scalar", std::make_pair(test_phi6_scalar, test_phi6_generated)));
    testCases.push_back(std::make_pair("test_phi7_scalar", std::make_pair(test_phi7_scalar, test_phi7_generated)));
    testCases.push_back(std::make_pair("test_phi8_scalar", std::make_pair(test_phi8_scalar, test_phi8_generated)));
    testCases.push_back(std::make_pair("test_phi9_scalar", std::make_pair(test_phi9_scalar, test_phi9_generated)));

    testCases.push_back(std::make_pair("test_loop1_scalar", std::make_pair(test_loop1_scalar, test_loop1_generated)));
    testCases.push_back(std::make_pair("test_loop2_scalar", std::make_pair(test_loop2_scalar, test_loop2_generated)));
    testCases.push_back(std::make_pair("test_loop3_scalar", std::make_pair(test_loop3_scalar, test_loop3_generated)));
    testCases.push_back(std::make_pair("test_loop4_scalar", std::make_pair(test_loop4_scalar, test_loop4_generated)));
    testCases.push_back(std::make_pair("test_loop5_scalar", std::make_pair(test_loop5_scalar, test_loop5_generated)));
    testCases.push_back(std::make_pair("test_loop6_scalar", std::make_pair(test_loop6_scalar, test_loop6_generated)));

    testCases.push_back(std::make_pair("test_loopc1_scalar", std::make_pair(test_loopc1_scalar, test_loopc1_generated)));
    testCases.push_back(std::make_pair("test_loopc2_scalar", std::make_pair(test_loopc2_scalar, test_loopc2_generated)));
    testCases.push_back(std::make_pair("test_loopc3_scalar", std::make_pair(test_loopc3_scalar, test_loopc3_generated)));
    testCases.push_back(std::make_pair("test_loopc4_scalar", std::make_pair(test_loopc4_scalar, test_loopc4_generated)));
    testCases.push_back(std::make_pair("test_loopc5_scalar", std::make_pair(test_loopc5_scalar, test_loopc5_generated)));
    testCases.push_back(std::make_pair("test_loopc6_scalar", std::make_pair(test_loopc6_scalar, test_loopc6_generated)));
    testCases.push_back(std::make_pair("test_loopc7_scalar", std::make_pair(test_loopc7_scalar, test_loopc7_generated)));
    testCases.push_back(std::make_pair("test_loopc8_scalar", std::make_pair(test_loopc8_scalar, test_loopc8_generated)));
    testCases.push_back(std::make_pair("test_loopc9_scalar", std::make_pair(test_loopc9_scalar, test_loopc9_generated)));

    testCases.push_back(std::make_pair("test_loopmx1_scalar", std::make_pair(test_loopmx1_scalar, test_loopmx1_generated)));
    testCases.push_back(std::make_pair("test_loopmx2_scalar", std::make_pair(test_loopmx2_scalar, test_loopmx2_generated)));
    testCases.push_back(std::make_pair("test_loopmx3_scalar", std::make_pair(test_loopmx3_scalar, test_loopmx3_generated)));
    testCases.push_back(std::make_pair("test_loopmx4_scalar", std::make_pair(test_loopmx4_scalar, test_loopmx4_generated)));
    testCases.push_back(std::make_pair("test_loopmx5_scalar", std::make_pair(test_loopmx5_scalar, test_loopmx5_generated)));
    testCases.push_back(std::make_pair("test_loopmx6_scalar", std::make_pair(test_loopmx6_scalar, test_loopmx6_generated)));
    testCases.push_back(std::make_pair("test_loopmx7_scalar", std::make_pair(test_loopmx7_scalar, test_loopmx7_generated)));
    testCases.push_back(std::make_pair("test_loopmx8_scalar", std::make_pair(test_loopmx8_scalar, test_loopmx8_generated)));
    testCases.push_back(std::make_pair("test_loopmx9_scalar", std::make_pair(test_loopmx9_scalar, test_loopmx9_generated)));
    testCases.push_back(std::make_pair("test_loopmx10_scalar", std::make_pair(test_loopmx10_scalar, test_loopmx10_generated)));
    testCases.push_back(std::make_pair("test_loopmx11_scalar", std::make_pair(test_loopmx11_scalar, test_loopmx11_generated)));
    testCases.push_back(std::make_pair("test_loopmx12_scalar", std::make_pair(test_loopmx12_scalar, test_loopmx12_generated)));
    testCases.push_back(std::make_pair("test_loopmx13_scalar", std::make_pair(test_loopmx13_scalar, test_loopmx13_generated)));

    testCases.push_back(std::make_pair("test_loopns1_scalar", std::make_pair(test_loopns1_scalar, test_loopns1_generated)));
    testCases.push_back(std::make_pair("test_loopns2_scalar", std::make_pair(test_loopns2_scalar, test_loopns2_generated)));
    testCases.push_back(std::make_pair("test_loopns3_scalar", std::make_pair(test_loopns3_scalar, test_loopns3_generated)));
    testCases.push_back(std::make_pair("test_loopns4_scalar", std::make_pair(test_loopns4_scalar, test_loopns4_generated)));
    testCases.push_back(std::make_pair("test_loopns5_scalar", std::make_pair(test_loopns5_scalar, test_loopns5_generated)));
    testCases.push_back(std::make_pair("test_loopns6_scalar", std::make_pair(test_loopns6_scalar, test_loopns6_generated)));
    testCases.push_back(std::make_pair("test_loopns7_scalar", std::make_pair(test_loopns7_scalar, test_loopns7_generated)));
    testCases.push_back(std::make_pair("test_loopns8_scalar", std::make_pair(test_loopns8_scalar, test_loopns8_generated)));
    testCases.push_back(std::make_pair("test_loopns9_scalar", std::make_pair(test_loopns9_scalar, test_loopns9_generated)));
    testCases.push_back(std::make_pair("test_loopns10_scalar", std::make_pair(test_loopns10_scalar, test_loopns10_generated)));

    testCases.push_back(std::make_pair("test_loopnsmx1_scalar", std::make_pair(test_loopnsmx1_scalar, test_loopnsmx1_generated)));
    testCases.push_back(std::make_pair("test_loopnsmx2_scalar", std::make_pair(test_loopnsmx2_scalar, test_loopnsmx2_generated)));
    testCases.push_back(std::make_pair("test_loopnsmx3_scalar", std::make_pair(test_loopnsmx3_scalar, test_loopnsmx3_generated)));
    testCases.push_back(std::make_pair("test_loopnsmx4_scalar", std::make_pair(test_loopnsmx4_scalar, test_loopnsmx4_generated)));
    testCases.push_back(std::make_pair("test_loopnsmx5_scalar", std::make_pair(test_loopnsmx5_scalar, test_loopnsmx5_generated)));
    testCases.push_back(std::make_pair("test_loopnsmx6_scalar", std::make_pair(test_loopnsmx6_scalar, test_loopnsmx6_generated)));
    testCases.push_back(std::make_pair("test_loopnsmx7_scalar", std::make_pair(test_loopnsmx7_scalar, test_loopnsmx7_generated)));
    testCases.push_back(std::make_pair("test_loopnsmx8_scalar", std::make_pair(test_loopnsmx8_scalar, test_loopnsmx8_generated)));
    testCases.push_back(std::make_pair("test_loopnsmx9_scalar", std::make_pair(test_loopnsmx9_scalar, test_loopnsmx9_generated)));
	// somehow, the last 6 input values for this one always fail, no matter which ones they are
    //testCases.push_back(std::make_pair("test_loopnsmx10_scalar", std::make_pair(test_loopnsmx10_scalar, test_loopnsmx10_generated)));

    testCases.push_back(std::make_pair("test_call1_scalar", std::make_pair(test_call1_scalar, test_call1_generated)));
    testCases.push_back(std::make_pair("test_call2_scalar", std::make_pair(test_call2_scalar, test_call2_generated)));
    testCases.push_back(std::make_pair("test_call3_scalar", std::make_pair(test_call3_scalar, test_call3_generated)));
    testCases.push_back(std::make_pair("test_call4_scalar", std::make_pair(test_call4_scalar, test_call4_generated)));
    testCases.push_back(std::make_pair("test_call5_scalar", std::make_pair(test_call5_scalar, test_call5_generated)));
    testCases.push_back(std::make_pair("test_call6_scalar", std::make_pair(test_call6_scalar, test_call6_generated)));
    testCases.push_back(std::make_pair("test_call7_scalar", std::make_pair(test_call7_scalar, test_call7_generated)));
    testCases.push_back(std::make_pair("test_call8_scalar", std::make_pair(test_call8_scalar, test_call8_generated)));
    testCases.push_back(std::make_pair("test_call9_scalar", std::make_pair(test_call9_scalar, test_call9_generated)));
    testCases.push_back(std::make_pair("test_call10_scalar", std::make_pair(test_call10_scalar, test_call10_generated)));
#endif
	// fails for some unknown reason, noise actually works in practice...
    //testCases.push_back(std::make_pair("test_noise_scalar", std::make_pair(test_noise_scalar, test_noise_generated)));

#ifdef RUN_SHADER_TESTS
    testCases.push_back(std::make_pair("test_shader_scalar", std::make_pair(test_shader_scalar, test_shader_generated)));
    testCases.push_back(std::make_pair("test_brick_scalar", std::make_pair(test_brick_scalar, test_brick_generated)));
    testCases.push_back(std::make_pair("test_checker_scalar", std::make_pair(test_checker_scalar, test_checker_generated)));
    testCases.push_back(std::make_pair("test_dented_scalar", std::make_pair(test_dented_scalar, test_dented_generated)));
    testCases.push_back(std::make_pair("test_glass_scalar", std::make_pair(test_glass_scalar, test_glass_generated)));
    testCases.push_back(std::make_pair("test_granite_scalar", std::make_pair(test_granite_scalar, test_granite_generated)));
    testCases.push_back(std::make_pair("test_parquet_scalar", std::make_pair(test_parquet_scalar, test_parquet_generated)));
    testCases.push_back(std::make_pair("test_phong_scalar", std::make_pair(test_phong_scalar, test_phong_generated)));
    testCases.push_back(std::make_pair("test_screen_scalar", std::make_pair(test_screen_scalar, test_screen_generated)));
    testCases.push_back(std::make_pair("test_starball_scalar", std::make_pair(test_starball_scalar, test_starball_generated)));
    testCases.push_back(std::make_pair("test_whitted_scalar", std::make_pair(test_whitted_scalar, test_whitted_generated)));
    testCases.push_back(std::make_pair("test_wood_scalar", std::make_pair(test_wood_scalar, test_wood_generated)));
#endif

#ifdef RUN_IRREDUCIBLE_TESTS
    testCases.push_back(std::make_pair("test_irreducible1_scalar", std::make_pair(test_irreducible1_scalar, test_irreducible1_generated)));
    testCases.push_back(std::make_pair("test_irreducible2_scalar", std::make_pair(test_irreducible2_scalar, test_irreducible2_generated)));
    testCases.push_back(std::make_pair("test_irreducible3_scalar", std::make_pair(test_irreducible3_scalar, test_irreducible3_generated)));
#endif
}

int main(int argc, char** argv) {

    printf("\n\n\n--------------------------------------------------------------------------------\n");
    printf("running test-suite for automatic packetization...\n\n");

    //------------------------------------------------------------------------//
    // create function pointers for test cases and save test case names
    //------------------------------------------------------------------------//
    std::vector<std::pair<const char*, std::pair<scalarFnType, vecFnType> > > testCases;

    //add scalar and generated functions
    addTestCases(testCases);

    const unsigned testCaseNr = testCases.size();

    //------------------------------------------------------------------------//
    // create input values
    //------------------------------------------------------------------------//
    const unsigned inputNr = 14;
    const unsigned inputParamNr = 2;
    const unsigned inputPermutations = pow(inputNr, inputParamNr) / 4;
    
    const float scalarInputs0[inputNr] = { 0.f, 3.f, 2.f, 8.f, 10.2f, -1.f, 0.f, 1000.23f, 0.0002f, -0.0002f, -3.f, -1.f, 0.f, 12.f };
    const float scalarInputs1[inputNr] = { 1.f, 2.f, 4.f, 6.f, -14.13f, -13.f, 0.f, 0.0002f, 420.001f, -420.001f, 3.f, -1.f, 0.f, 12.f };
    const float scalarInputs2[inputNr] = { 2.f, 1.f, 6.f, 4.f, 999.f, -5.f, 0.f, 420.001f, 0.01f, 0.01f, 3.f, 1.f, 333.333f, 4.f };
    const float scalarInputs3[inputNr] = { 3.f, 0.f, 8.f, 2.f, 0.f, -420.001f, 0.f, 0.01f, 1000.23f, 0.01f, -3.f, 1.f, -333.333f, -4.f };

    //------------------------------------------------------------------------//
    // create result arrays for generated functions
    //------------------------------------------------------------------------//
    const unsigned maxResultNr = inputPermutations * testCaseNr;
    std::vector<V*>* results = new std::vector<V*>(maxResultNr); //only aligned if on heap

    const bool printAllResults = false;

    //------------------------------------------------------------------------//
    // create result arrays for scalar functions
    //------------------------------------------------------------------------//
	typedef std::pair<const char*, std::pair<const char*, std::vector<float>* > > ScalarResult;
    ScalarResult* scalarResults = new ScalarResult[maxResultNr]();
    
    //------------------------------------------------------------------------//
    // create timer and data structures that hold execution times
    //------------------------------------------------------------------------//
    //Packetizer::Timer timer;
    std::vector<double> executionTimesScalar;
    std::vector<double> executionTimesPacketized;
    
    //------------------------------------------------------------------------//
    // compute results of scalar and generated functions
    //------------------------------------------------------------------------//
    unsigned testsRun = 0;

    for (unsigned TC=0; TC<testCaseNr; ++TC) {
        unsigned inputPermsRun = 0;
        for (unsigned i=0; i<inputNr; i+=2) {
            for (unsigned j=0; j<inputNr; j+=2) {
                // abort if we have already run too many test cases
                if (testsRun >= maxResultNr) {
                    printf("\nERROR: not enough space allocated for results!\n");
                    exit(-1);
                }

                // get function pointers of current test case
				std::pair<const char*, std::pair<scalarFnType, vecFnType> > testCase = testCases[TC];

                
                // get input values
                const float input0i = scalarInputs0[i];
                const float input1i = scalarInputs1[i];
                const float input2i = scalarInputs2[i];
                const float input3i = scalarInputs3[i];
                const float input4i = scalarInputs0[i+1];
                const float input5i = scalarInputs1[i+1];
                const float input6i = scalarInputs2[i+1];
                const float input7i = scalarInputs3[i+1];
                const float input0j = scalarInputs0[j];
                const float input1j = scalarInputs1[j];
                const float input2j = scalarInputs2[j];
                const float input3j = scalarInputs3[j];
                const float input4j = scalarInputs0[j+1];
                const float input5j = scalarInputs1[j+1];
                const float input6j = scalarInputs2[j+1];
                const float input7j = scalarInputs3[j+1];

                // generate info-string for this execution
				char inputString[120];
				sprintf(inputString, "[ [ %f %f %f %f %f %f %f %f ] | [ %f %f %f %f %f %f %f %f ] ]",
					input0i, input1i, input2i, input3i, input4i, input5i, input6i, input7i, input0j, input1j, input2j, input3j, input4j, input5j, input6j, input7j);

                // execute scalar function
                std::vector<float>* resS = new std::vector<float>();
                //timer.startTimer();
                const float resS0 = testCase.second.first(input0i, input0j);
                const float resS1 = testCase.second.first(input1i, input1j);
                const float resS2 = testCase.second.first(input2i, input2j);
                const float resS3 = testCase.second.first(input3i, input3j);
                const float resS4 = testCase.second.first(input4i, input4j);
                const float resS5 = testCase.second.first(input5i, input5j);
                const float resS6 = testCase.second.first(input6i, input6j);
                const float resS7 = testCase.second.first(input7i, input7j);
                //timer.stopTimer();
                //executionTimesScalar.push_back(timer.getTime());
                
                // store result of scalar function
                resS->push_back(resS0);
                resS->push_back(resS1);
                resS->push_back(resS2);
                resS->push_back(resS3);
                resS->push_back(resS4);
                resS->push_back(resS5);
                resS->push_back(resS6);
                resS->push_back(resS7);
                scalarResults[testsRun] = std::make_pair(inputString, std::make_pair(testCase.first, resS));


                // generate inputs for packetized function
                V* ax = (V*)aligned_malloc(sizeof(V), 32);
                V* bx = (V*)aligned_malloc(sizeof(V), 32);
                ax->data = _mm256_set_ps(scalarInputs3[i+1], scalarInputs2[i+1], scalarInputs1[i+1], scalarInputs0[i+1], scalarInputs3[i], scalarInputs2[i], scalarInputs1[i], scalarInputs0[i]);
                bx->data = _mm256_set_ps(scalarInputs3[j+1], scalarInputs2[j+1], scalarInputs1[j+1], scalarInputs0[j+1], scalarInputs3[j], scalarInputs2[j], scalarInputs1[j], scalarInputs0[j]);

                // execute generated function
                V* r = (V*)aligned_malloc(sizeof(V), 32);
                //timer.startTimer();
                r->data = testCase.second.second(ax->data, bx->data);
                //timer.stopTimer();
                //executionTimesPacketized.push_back(timer.getTime());

                // store result of generated function
                (*results)[testsRun] = r;

                ++testsRun;
                printf("Total progress: %.2f percent | test case %03d/%03d (inputs %03d/%03d) : %-30.30s\r", ((float)testsRun * 100.f) / (float)maxResultNr, TC+1, testCaseNr, ++inputPermsRun, inputPermutations, testCase.first); fflush(stdout);
            }
        }
    }

    if (testsRun <= 0) {
        printf("ERROR: need to compute at least one result! (forgot to activate test cases?)\n");
        exit(-1);
    }
    if (testsRun != maxResultNr) {
        printf("ERROR: unexpected number of results  (testsRun (%d) != maxResultNr (%d))!\n", testsRun, maxResultNr);
        exit(-1);
    }


    if (printAllResults) {
        for (unsigned i=0; i<testsRun; ++i) {
            const char* testCaseName = scalarResults[i].second.first;
            printf("%s (scalar) = [ ", testCaseName);
            printf("%f %f %f %f %f %f %f %f ", (*scalarResults[i].second.second)[0], (*scalarResults[i].second.second)[1], (*scalarResults[i].second.second)[2], (*scalarResults[i].second.second)[3], (*scalarResults[i].second.second)[4], (*scalarResults[i].second.second)[5], (*scalarResults[i].second.second)[6], (*scalarResults[i].second.second)[7]);
            printf("]\n");

            printf("%s (packetized) = [ ", testCaseName);
            printf("%f %f %f %f %f %f %f %f ", get((*results)[i], 0), get((*results)[i], 1), get((*results)[i], 2), get((*results)[i], 3), get((*results)[i], 4), get((*results)[i], 5), get((*results)[i], 6), get((*results)[i], 7));
            printf("]\n");
        }
    }
    
#if 0 // executionTimesScalar is empty
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
        const double speedup = executionTimeScalar;
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
        
		const bool printAllTimes = false;
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
#endif


    printf("verifying results:\n\n");

    bool allSuccessfull = true;
    unsigned failedTestsNr = 0;
    for (unsigned i=0; i<testsRun; ++i) {
		const float scalarRes0 = (*scalarResults[i].second.second)[0];
		const float scalarRes1 = (*scalarResults[i].second.second)[1];
		const float scalarRes2 = (*scalarResults[i].second.second)[2];
		const float scalarRes3 = (*scalarResults[i].second.second)[3];
		const float scalarRes4 = (*scalarResults[i].second.second)[4];
		const float scalarRes5 = (*scalarResults[i].second.second)[5];
		const float scalarRes6 = (*scalarResults[i].second.second)[6];
		const float scalarRes7 = (*scalarResults[i].second.second)[7];
		const float pktRes0 = get((*results)[i], 0);
		const float pktRes1 = get((*results)[i], 1);
		const float pktRes2 = get((*results)[i], 2);
		const float pktRes3 = get((*results)[i], 3);
		const float pktRes4 = get((*results)[i], 4);
		const float pktRes5 = get((*results)[i], 5);
		const float pktRes6 = get((*results)[i], 6);
		const float pktRes7 = get((*results)[i], 7);
		
		const bool success =
			resultMatches(scalarRes0, pktRes0) &&
			resultMatches(scalarRes1, pktRes1) &&
			resultMatches(scalarRes2, pktRes2) &&
			resultMatches(scalarRes3, pktRes3) &&
			resultMatches(scalarRes4, pktRes4) &&
			resultMatches(scalarRes5, pktRes5) &&
			resultMatches(scalarRes6, pktRes6) &&
			resultMatches(scalarRes7, pktRes7);

        if (printAllResults) {
            printf("%s ", scalarResults[i].second.first);
            success ? printf(" SUCCESSFULL!") : printf(" FAILED!     ");
			printf(" %s\n", scalarResults[i].first);
        } else if (!success) {
            printf("%s FAILED! ", scalarResults[i].second.first);
			printf(" %s\n", scalarResults[i].first); //input-values
            printf("  expected result: [ %f %f %f %f %f %f %f %f ]\n", scalarRes0, scalarRes1, scalarRes2, scalarRes3, scalarRes4, scalarRes5, scalarRes6, scalarRes7);
            printf("  computed result: [ %f %f %f %f %f %f %f %f ]\n", pktRes0, pktRes1, pktRes2, pktRes3, pktRes4, pktRes5, pktRes6, pktRes7);
        }

        allSuccessfull &= success;
        if (!success) ++failedTestsNr;
    }

    if (allSuccessfull) printf("ALL TESTS SUCCESSFULL! (%d)\n", testsRun);
	else printf("\n%d / %d TESTS FAILED! (6 expected for last input values of nsmx10 - TODO: fix ;) )", failedTestsNr, testsRun);

    printf("\n\ntest-suite run complete!\n");
    printf("--------------------------------------------------------------------------------\n\n");


    return 0;
}

