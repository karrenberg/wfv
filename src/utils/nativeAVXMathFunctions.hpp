/**
 * @file   nativeAVXMathFunctions.hpp
 * @date   29.04.2011
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2011 Saarland University
 *
 * This file generates llvm bitcode resembling Julien Pommier's SSE functions
 * (sse_mathfun.h).
 * It has some additional handwritten AVX implementations (e.g. pow_ps),
 * some parts are inspired by the 'Universal SIMD Mathlibrary'.
 *
 */
#ifndef _NATIVEAVXMATHFUNCTIONS_HPP
#define	_NATIVEAVXMATHFUNCTIONS_HPP

/* Copyright (C) 2007  Julien Pommier

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  (this is the zlib license)
*/

#include "llvm/Support/raw_ostream.h"

#include <llvm/Module.h>
#include <llvm/DerivedTypes.h>
#include <llvm/Constants.h>
#include <llvm/GlobalVariable.h>
#include <llvm/Function.h>
#include <llvm/CallingConv.h>
#include <llvm/BasicBlock.h>
#include <llvm/Instructions.h>
#include <llvm/InlineAsm.h>
#include <llvm/Support/MathExtras.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Pass.h>
#include <llvm/PassManager.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Assembly/PrintModulePass.h>
#include <algorithm>

#include <llvm/Intrinsics.h>

#include <llvm/LLVMContext.h>

using namespace llvm;

class NativeAVXMathFunctions {
public:
    inline Function* getSinPS(Module* mod, const unsigned simdWidth) const { return NULL; }
    inline Function* getCosPS(Module* mod, const unsigned simdWidth) const { return NULL; }
    inline Function* getSinCosPS(Module* mod, const unsigned simdWidth) const { return NULL; }
    inline Function* getLogPS(Module* mod, const unsigned simdWidth) const { return NULL; }
    inline Function* getExpPS(Module* mod, const unsigned simdWidth) const { return NULL; }
    inline Function* getLog2PS(Module* mod, const unsigned simdWidth) const { return NULL; }
    inline Function* getExp2PS(Module* mod, const unsigned simdWidth) const { return NULL; }
    inline Function* getPowPS(Module* mod, const unsigned simdWidth) const { return NULL; }
	inline Function* getAbsPS(Module* mod, const unsigned simdWidth) const { return NULL; }

    inline Function* getRoundPS(Module* mod) const { return generateRoundPS(mod); }

    inline Function* getRsqrtPS(Module* mod) const { return generateRsqrtPS(mod); }
    inline Function* getSqrtPS(Module* mod) const { return generateSqrtPS(mod); }
    inline Function* getRcpPS(Module* mod) const { return generateRcpPS(mod); }
    inline Function* getMinPS(Module* mod) const { return generateMinPS(mod); }
    inline Function* getMaxPS(Module* mod) const { return generateMaxPS(mod); }
    inline Function* getCmpPS(Module* mod) const { return generateCmpPS(mod); }
    inline Function* getAddSubPS(Module* mod) const { return generateAddSubPS(mod); }

private:
    /////////////////////////////
    // AVX function generation //
    /////////////////////////////
    //intrinsics have to be generated/declared BEFORE math functions!
    inline Function* generateRsqrtPS(Module* mod) const {
		return Intrinsic::getDeclaration(mod, Intrinsic::x86_avx_rsqrt_ps_256);
    }
    inline Function* generateSqrtPS(Module* mod) const {
		return Intrinsic::getDeclaration(mod, Intrinsic::x86_avx_sqrt_ps_256);
    }
    inline Function* generateRcpPS(Module* mod) const {
		return Intrinsic::getDeclaration(mod, Intrinsic::x86_avx_rcp_ps_256);
    }
    inline Function* generateMaxPS(Module* mod) const {
		return Intrinsic::getDeclaration(mod, Intrinsic::x86_avx_max_ps_256);
    }
    inline Function* generateMinPS(Module* mod) const {
		return Intrinsic::getDeclaration(mod, Intrinsic::x86_avx_min_ps_256);
    }
    inline Function* generateCmpPS(Module* mod) const {
		return Intrinsic::getDeclaration(mod, Intrinsic::x86_avx_cmp_ps_256);
    }
    inline Function* generateAddSubPS(Module* mod) const {
		return Intrinsic::getDeclaration(mod, Intrinsic::x86_avx_addsub_ps_256);
    }
    inline Function* generateRoundPS(Module* mod) const {
		return Intrinsic::getDeclaration(mod, Intrinsic::x86_avx_round_ps_256);
    }

	// TODO: write these in C and generate LLVM IR with clang... should be a lot easier ;)
    /*
    Function* generateSinPS(Module* mod, const unsigned simdWidth)
    Function* generateCosPS(Module* mod, const unsigned simdWidth)
    Function* generateSinCosPS(Module* mod, const unsigned simdWidth)
    Function* generateLogPS(Module* mod, const unsigned simdWidth)
    Function* generateExpPS(Module* mod, const unsigned simdWidth)

    Function* generateExp2PS(Module* mod, const unsigned simdWidth)
	Function* generateLog2PS(Module* mod, const unsigned simdWidth)
	Function* generatePowPS(Module* mod, const unsigned simdWidth)
	Function* generateAbsPS(Module* mod, const unsigned simdWidth)
	*/
};

#endif	/* _NATIVEAVXMATHFUNCTIONS_HPP */

