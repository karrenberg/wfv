/**
 * @file   nativeSSEMathFunctions.hpp
 * @date   07.04.2009
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2009, 2010 Saarland University
 *
 * This file generates llvm bitcode resembling Julien Pommier's SSE functions
 * (sse_mathfun.h).
 * It has some additional handwritten SSE implementations (e.g. pow_ps),
 * some parts inspired by the 'Universal SIMD Mathlibrary'.
 *
 */
#ifndef _NATIVESSEMATHFUNCTIONS_HPP
#define	_NATIVESSEMATHFUNCTIONS_HPP

/* SIMD (SSE1+MMX or SSE2) implementation of sin, cos, exp and log

   Inspired by Intel Approximate Math library, and based on the
   corresponding algorithms of the cephes math library

   The default is to use the SSE1 version. If you define USE_SSE2 the
   the SSE2 intrinsics will be used in place of the MMX intrinsics. Do
   not expect any significant performance improvement with SSE2.
*/

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

class NativeSSEMathFunctions {
public:
    inline Function* getSinPS(Module& mod, const unsigned simdWidth) const { return generateSinPS(mod, simdWidth); }
    inline Function* getCosPS(Module& mod, const unsigned simdWidth) const { return generateCosPS(mod, simdWidth); }
    inline Function* getSinCosPS(Module& mod, const unsigned simdWidth) const { return generateSinCosPS(mod, simdWidth); }
    inline Function* getLogPS(Module& mod, const unsigned simdWidth) const { return generateLogPS(mod, simdWidth); }
    inline Function* getExpPS(Module& mod, const unsigned simdWidth) const { return generateExpPS(mod, simdWidth); }
    inline Function* getRoundPS(Module& mod) const { return generateRoundPS(mod); }

    inline Function* getLog2PS(Module& mod, const unsigned simdWidth) const { return generateLog2PS(mod, simdWidth); }
    inline Function* getExp2PS(Module& mod, const unsigned simdWidth) const { return generateExp2PS(mod, simdWidth); }
    inline Function* getPowPS(Module& mod, const unsigned simdWidth) const { return generatePowPS(mod, simdWidth); }

	inline Function* getAbsPS(Module& mod, const unsigned simdWidth) const { return generateAbsPS(mod, simdWidth); }

    inline Function* getRsqrtPS(Module& mod) const { return generateRsqrtPS(mod); }
    inline Function* getSqrtPS(Module& mod) const { return generateSqrtPS(mod); }
    inline Function* getRcpPS(Module& mod) const { return generateRcpPS(mod); }
    inline Function* getMinPS(Module& mod) const { return generateMinPS(mod); }
    inline Function* getMaxPS(Module& mod) const { return generateMaxPS(mod); }
    inline Function* getCmpPS(Module& mod) const { return generateCmpPS(mod); }
    inline Function* getAddSubPS(Module& mod) const { return generateAddSubPS(mod); }

private:
    /////////////////////////////
    // SSE function generation //
    /////////////////////////////
    //intrinsics have to be generated/declared BEFORE math functions!
    inline Function* generateCvttps2dq(Module& mod) const {
		return Intrinsic::getDeclaration(&mod, Intrinsic::x86_sse2_cvttps2dq);
    }
    inline Function* generateCvtdq2ps(Module& mod) const {
		return Intrinsic::getDeclaration(&mod, Intrinsic::x86_sse2_cvtdq2ps);
    }
    inline Function* generatePcmpeq(Module& mod) const {
		return Intrinsic::getDeclaration(&mod, Intrinsic::x86_sse2_pcmpeq_d);
    }
    inline Function* generatePsllid(Module& mod) const {
		return Intrinsic::getDeclaration(&mod, Intrinsic::x86_sse2_pslli_d);
    }
    inline Function* generatePsrlid(Module& mod) const {
		return Intrinsic::getDeclaration(&mod, Intrinsic::x86_sse2_psrli_d);
    }

    inline Function* generateRsqrtPS(Module& mod) const {
		return Intrinsic::getDeclaration(&mod, Intrinsic::x86_sse_rsqrt_ps);
    }
    inline Function* generateSqrtPS(Module& mod) const {
		return Intrinsic::getDeclaration(&mod, Intrinsic::x86_sse_sqrt_ps);
    }
    inline Function* generateRcpPS(Module& mod) const {
		return Intrinsic::getDeclaration(&mod, Intrinsic::x86_sse_rcp_ps);
    }
    inline Function* generateMaxPS(Module& mod) const {
		return Intrinsic::getDeclaration(&mod, Intrinsic::x86_sse_max_ps);
    }
    inline Function* generateMinPS(Module& mod) const {
		return Intrinsic::getDeclaration(&mod, Intrinsic::x86_sse_min_ps);
    }
    inline Function* generateCmpPS(Module& mod) const {
		return Intrinsic::getDeclaration(&mod, Intrinsic::x86_sse_cmp_ps);
    }
    inline Function* generateAddSubPS(Module& mod) const {
		return Intrinsic::getDeclaration(&mod, Intrinsic::x86_sse3_addsub_ps);
    }
    inline Function* generateRoundPS(Module& mod) const {
		return Intrinsic::getDeclaration(&mod, Intrinsic::x86_sse41_round_ps);
    }

    Function* generateSinPS(Module& mod, const unsigned simdWidth) const {
        if (Function* tmpF = mod.getFunction("sin_ps")) {
            return tmpF;
        }

        assert (!(mod.getFunction("_ZL17_ps_inv_sign_mask") ||
                mod.getFunction("_ZL13_ps_sign_mask") ||
                mod.getFunction("_ZL15_ps_cephes_FOPI") ||
                mod.getFunction("_ZL7_pi32_1") ||
                mod.getFunction("_ZL10_pi32_inv1") ||
                mod.getFunction("_ZL7_pi32_4") ||
                mod.getFunction("_ZL7_pi32_2") ||
                mod.getFunction("_ZL20_ps_minus_cephes_DP1") ||
                mod.getFunction("_ZL20_ps_minus_cephes_DP2") ||
                mod.getFunction("_ZL20_ps_minus_cephes_DP3") ||
                mod.getFunction("_ZL13_ps_coscof_p0") ||
                mod.getFunction("_ZL13_ps_coscof_p1") ||
                mod.getFunction("_ZL13_ps_coscof_p2") ||
                mod.getFunction("_ZL7_ps_0p5") ||
                mod.getFunction("_ZL5_ps_1") ||
                mod.getFunction("_ZL13_ps_sincof_p0") ||
                mod.getFunction("_ZL13_ps_sincof_p1") ||
                mod.getFunction("_ZL13_ps_sincof_p2")) && "function to be generated is already declared in module!");

		LLVMContext& context = mod.getContext();

		VectorType* vectorTy_float_SIMD = VectorType::get(Type::getFloatTy(context), simdWidth);
		VectorType* vectorTy_int_SIMD = VectorType::get(Type::getInt32Ty(context), simdWidth);

      // Type Definitions
      ArrayType* ArrayTy_0 = ArrayType::get(IntegerType::get(context, 32), 4);
      ArrayType* ArrayTy_2 = ArrayType::get(Type::getFloatTy(context), 4);
      VectorType* VectorTy_10 = VectorType::get(IntegerType::get(context, 64), 2);
      PointerType* PointerTy_6 = PointerType::get(vectorTy_float_SIMD, 0);
      PointerType* PointerTy_11 = PointerType::get(VectorTy_10, 0);

      std::vector<Type*>FuncTy_5_args;
      FuncTy_5_args.push_back(vectorTy_float_SIMD);
      FunctionType* FuncTy_5 = FunctionType::get(
        /*Result=*/vectorTy_float_SIMD,
        /*Params=*/FuncTy_5_args,
        /*isVarArg=*/false);

      // Function Declarations

      Function* sin_ps = Function::Create(
        /*Type=*/FuncTy_5,
        /*Linkage=*/GlobalValue::ExternalLinkage,
        /*Name=*/"sin_ps", &mod);
      sin_ps->setCallingConv(CallingConv::C);
      AttrListPtr sin_ps_PAL;
      {
        SmallVector<AttributeWithIndex, 4> Attrs;
        AttributeWithIndex PAWI;
        PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
        Attrs.push_back(PAWI);
        sin_ps_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

      }
      sin_ps->setAttributes(sin_ps_PAL);

      // Global Variable Declarations


      GlobalVariable* gvar_array__ZL17_ps_inv_sign_mask = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_inv_sign_mask");
      gvar_array__ZL17_ps_inv_sign_mask->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_sign_mask = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_sign_mask");
      gvar_array__ZL13_ps_sign_mask->setAlignment(16);

      GlobalVariable* gvar_array__ZL15_ps_cephes_FOPI = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL15_ps_cephes_FOPI");
      gvar_array__ZL15_ps_cephes_FOPI->setAlignment(16);

      GlobalVariable* gvar_array__ZL7_pi32_1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL7_pi32_1");
      gvar_array__ZL7_pi32_1->setAlignment(16);

      GlobalVariable* gvar_array__ZL10_pi32_inv1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL10_pi32_inv1");
      gvar_array__ZL10_pi32_inv1->setAlignment(16);

      GlobalVariable* gvar_array__ZL7_pi32_4 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL7_pi32_4");
      gvar_array__ZL7_pi32_4->setAlignment(16);

      GlobalVariable* gvar_array__ZL7_pi32_2 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL7_pi32_2");
      gvar_array__ZL7_pi32_2->setAlignment(16);

      GlobalVariable* gvar_array__ZL20_ps_minus_cephes_DP1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL20_ps_minus_cephes_DP1");
      gvar_array__ZL20_ps_minus_cephes_DP1->setAlignment(16);

      GlobalVariable* gvar_array__ZL20_ps_minus_cephes_DP2 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL20_ps_minus_cephes_DP2");
      gvar_array__ZL20_ps_minus_cephes_DP2->setAlignment(16);

      GlobalVariable* gvar_array__ZL20_ps_minus_cephes_DP3 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL20_ps_minus_cephes_DP3");
      gvar_array__ZL20_ps_minus_cephes_DP3->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_coscof_p0 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_coscof_p0");
      gvar_array__ZL13_ps_coscof_p0->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_coscof_p1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_coscof_p1");
      gvar_array__ZL13_ps_coscof_p1->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_coscof_p2 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_coscof_p2");
      gvar_array__ZL13_ps_coscof_p2->setAlignment(16);

      GlobalVariable* gvar_array__ZL7_ps_0p5 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL7_ps_0p5");
      gvar_array__ZL7_ps_0p5->setAlignment(16);

      GlobalVariable* gvar_array__ZL5_ps_1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL5_ps_1");
      gvar_array__ZL5_ps_1->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_sincof_p0 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_sincof_p0");
      gvar_array__ZL13_ps_sincof_p0->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_sincof_p1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_sincof_p1");
      gvar_array__ZL13_ps_sincof_p1->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_sincof_p2 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_sincof_p2");
      gvar_array__ZL13_ps_sincof_p2->setAlignment(16);

      // Constant Definitions
      std::vector<Constant*> const_array_18_elems;
      ConstantInt* const_int32_19 = ConstantInt::get(context, APInt(32,  "2147483647", 10));
      const_array_18_elems.push_back(const_int32_19);
      const_array_18_elems.push_back(const_int32_19);
      const_array_18_elems.push_back(const_int32_19);
      const_array_18_elems.push_back(const_int32_19);
      Constant* const_array_18 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_18_elems));
      std::vector<Constant*> const_array_20_elems;
      ConstantInt* const_int32_21 = ConstantInt::get(context, APInt(32,  "-2147483648", 10));
      const_array_20_elems.push_back(const_int32_21);
      const_array_20_elems.push_back(const_int32_21);
      const_array_20_elems.push_back(const_int32_21);
      const_array_20_elems.push_back(const_int32_21);
      Constant* const_array_20 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_20_elems));
      std::vector<Constant*> const_array_22_elems;
      ConstantFP* const_float_23 = ConstantFP::get(context, APFloat(BitsToFloat(0x3FA2F983U) /* 1.273239e+00 */));
      const_array_22_elems.push_back(const_float_23);
      const_array_22_elems.push_back(const_float_23);
      const_array_22_elems.push_back(const_float_23);
      const_array_22_elems.push_back(const_float_23);
      Constant* const_array_22 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_22_elems));
      std::vector<Constant*> const_array_24_elems;
      ConstantInt* const_int32_25 = ConstantInt::get(context, APInt(32,  "1", 10));
      const_array_24_elems.push_back(const_int32_25);
      const_array_24_elems.push_back(const_int32_25);
      const_array_24_elems.push_back(const_int32_25);
      const_array_24_elems.push_back(const_int32_25);
      Constant* const_array_24 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_24_elems));
      std::vector<Constant*> const_array_26_elems;
      ConstantInt* const_int32_27 = ConstantInt::get(context, APInt(32,  "-2", 10));
      const_array_26_elems.push_back(const_int32_27);
      const_array_26_elems.push_back(const_int32_27);
      const_array_26_elems.push_back(const_int32_27);
      const_array_26_elems.push_back(const_int32_27);
      Constant* const_array_26 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_26_elems));
      std::vector<Constant*> const_array_28_elems;
      ConstantInt* const_int32_29 = ConstantInt::get(context, APInt(32,  "4", 10));
      const_array_28_elems.push_back(const_int32_29);
      const_array_28_elems.push_back(const_int32_29);
      const_array_28_elems.push_back(const_int32_29);
      const_array_28_elems.push_back(const_int32_29);
      Constant* const_array_28 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_28_elems));
      std::vector<Constant*> const_array_30_elems;
      ConstantInt* const_int32_31 = ConstantInt::get(context, APInt(32,  "2", 10));
      const_array_30_elems.push_back(const_int32_31);
      const_array_30_elems.push_back(const_int32_31);
      const_array_30_elems.push_back(const_int32_31);
      const_array_30_elems.push_back(const_int32_31);
      Constant* const_array_30 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_30_elems));
      std::vector<Constant*> const_array_32_elems;
      ConstantFP* const_float_33 = ConstantFP::get(context, APFloat(BitsToFloat(0xBF490000U) /* -7.851562e-01 */));
      const_array_32_elems.push_back(const_float_33);
      const_array_32_elems.push_back(const_float_33);
      const_array_32_elems.push_back(const_float_33);
      const_array_32_elems.push_back(const_float_33);
      Constant* const_array_32 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_32_elems));
      std::vector<Constant*> const_array_34_elems;
      ConstantFP* const_float_35 = ConstantFP::get(context, APFloat(BitsToFloat(0xB97DA000U) /* -2.418756e-04 */));
      const_array_34_elems.push_back(const_float_35);
      const_array_34_elems.push_back(const_float_35);
      const_array_34_elems.push_back(const_float_35);
      const_array_34_elems.push_back(const_float_35);
      Constant* const_array_34 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_34_elems));
      std::vector<Constant*> const_array_36_elems;
      ConstantFP* const_float_37 = ConstantFP::get(context, APFloat(-3.774895e-08f));
      const_array_36_elems.push_back(const_float_37);
      const_array_36_elems.push_back(const_float_37);
      const_array_36_elems.push_back(const_float_37);
      const_array_36_elems.push_back(const_float_37);
      Constant* const_array_36 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_36_elems));
      std::vector<Constant*> const_array_38_elems;
      ConstantFP* const_float_39 = ConstantFP::get(context, APFloat(BitsToFloat(0x37CCF5CEU) /* 2.443316e-05 */));
      const_array_38_elems.push_back(const_float_39);
      const_array_38_elems.push_back(const_float_39);
      const_array_38_elems.push_back(const_float_39);
      const_array_38_elems.push_back(const_float_39);
      Constant* const_array_38 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_38_elems));
      std::vector<Constant*> const_array_40_elems;
      ConstantFP* const_float_41 = ConstantFP::get(context, APFloat(BitsToFloat(0xBAB6061AU) /* -1.388732e-03 */));
      const_array_40_elems.push_back(const_float_41);
      const_array_40_elems.push_back(const_float_41);
      const_array_40_elems.push_back(const_float_41);
      const_array_40_elems.push_back(const_float_41);
      Constant* const_array_40 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_40_elems));
      std::vector<Constant*> const_array_42_elems;
      ConstantFP* const_float_43 = ConstantFP::get(context, APFloat(BitsToFloat(0x3D2AAAA5U) /* 4.166665e-02 */));
      const_array_42_elems.push_back(const_float_43);
      const_array_42_elems.push_back(const_float_43);
      const_array_42_elems.push_back(const_float_43);
      const_array_42_elems.push_back(const_float_43);
      Constant* const_array_42 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_42_elems));
      std::vector<Constant*> const_array_44_elems;
      ConstantFP* const_float_45 = ConstantFP::get(context, APFloat(5.000000e-01f));
      const_array_44_elems.push_back(const_float_45);
      const_array_44_elems.push_back(const_float_45);
      const_array_44_elems.push_back(const_float_45);
      const_array_44_elems.push_back(const_float_45);
      Constant* const_array_44 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_44_elems));
      std::vector<Constant*> const_array_46_elems;
      ConstantFP* const_float_47 = ConstantFP::get(context, APFloat(1.000000e+00f));
      const_array_46_elems.push_back(const_float_47);
      const_array_46_elems.push_back(const_float_47);
      const_array_46_elems.push_back(const_float_47);
      const_array_46_elems.push_back(const_float_47);
      Constant* const_array_46 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_46_elems));
      std::vector<Constant*> const_array_48_elems;
      ConstantFP* const_float_49 = ConstantFP::get(context, APFloat(BitsToFloat(0xB94CA1F9U) /* -1.951530e-04 */));
      const_array_48_elems.push_back(const_float_49);
      const_array_48_elems.push_back(const_float_49);
      const_array_48_elems.push_back(const_float_49);
      const_array_48_elems.push_back(const_float_49);
      Constant* const_array_48 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_48_elems));
      std::vector<Constant*> const_array_50_elems;
      ConstantFP* const_float_51 = ConstantFP::get(context, APFloat(8.332161e-03f));
      const_array_50_elems.push_back(const_float_51);
      const_array_50_elems.push_back(const_float_51);
      const_array_50_elems.push_back(const_float_51);
      const_array_50_elems.push_back(const_float_51);
      Constant* const_array_50 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_50_elems));
      std::vector<Constant*> const_array_52_elems;
      ConstantFP* const_float_53 = ConstantFP::get(context, APFloat(BitsToFloat(0xBE2AAAA3U) /* -1.666666e-01 */));
      const_array_52_elems.push_back(const_float_53);
      const_array_52_elems.push_back(const_float_53);
      const_array_52_elems.push_back(const_float_53);
      const_array_52_elems.push_back(const_float_53);
      Constant* const_array_52 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_52_elems));
      Constant* const_ptr_54 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_inv_sign_mask, PointerTy_6);
      Constant* const_ptr_55 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_sign_mask, PointerTy_6);
      Constant* const_ptr_56 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL15_ps_cephes_FOPI, PointerTy_6);
      Constant* const_ptr_57 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL7_pi32_1, PointerTy_11);
      Constant* const_ptr_58 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL10_pi32_inv1, PointerTy_11);
      Constant* const_ptr_59 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL7_pi32_4, PointerTy_11);
      ConstantInt* const_int32_60 = ConstantInt::get(context, APInt(32,  "29", 10));
      Constant* const_ptr_61 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL7_pi32_2, PointerTy_11);
      ConstantAggregateZero* const_packed_62 = ConstantAggregateZero::get(vectorTy_int_SIMD); //llvm 2.5
      Constant* const_ptr_63 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL20_ps_minus_cephes_DP1, PointerTy_6);
      Constant* const_ptr_64 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL20_ps_minus_cephes_DP2, PointerTy_6);
      Constant* const_ptr_65 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL20_ps_minus_cephes_DP3, PointerTy_6);
      Constant* const_ptr_66 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_coscof_p0, PointerTy_6);
      Constant* const_ptr_67 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_coscof_p1, PointerTy_6);
      Constant* const_ptr_68 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_coscof_p2, PointerTy_6);
      Constant* const_ptr_69 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL7_ps_0p5, PointerTy_6);
      Constant* const_ptr_70 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL5_ps_1, PointerTy_6);
      Constant* const_ptr_71 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_sincof_p0, PointerTy_6);
      Constant* const_ptr_72 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_sincof_p1, PointerTy_6);
      Constant* const_ptr_73 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_sincof_p2, PointerTy_6);
      std::vector<Constant*> const_packed_74_elems;
      ConstantInt* const_int32_75 = ConstantInt::get(context, APInt(32,  "-1", 10));
      const_packed_74_elems.push_back(const_int32_75);
      const_packed_74_elems.push_back(const_int32_75);
      const_packed_74_elems.push_back(const_int32_75);
      const_packed_74_elems.push_back(const_int32_75);
      Constant* const_packed_74 = ConstantVector::get(ArrayRef<Constant*>(const_packed_74_elems));

      // Global Variable Definitions
      gvar_array__ZL17_ps_inv_sign_mask->setInitializer(const_array_18);
      gvar_array__ZL13_ps_sign_mask->setInitializer(const_array_20);
      gvar_array__ZL15_ps_cephes_FOPI->setInitializer(const_array_22);
      gvar_array__ZL7_pi32_1->setInitializer(const_array_24);
      gvar_array__ZL10_pi32_inv1->setInitializer(const_array_26);
      gvar_array__ZL7_pi32_4->setInitializer(const_array_28);
      gvar_array__ZL7_pi32_2->setInitializer(const_array_30);
      gvar_array__ZL20_ps_minus_cephes_DP1->setInitializer(const_array_32);
      gvar_array__ZL20_ps_minus_cephes_DP2->setInitializer(const_array_34);
      gvar_array__ZL20_ps_minus_cephes_DP3->setInitializer(const_array_36);
      gvar_array__ZL13_ps_coscof_p0->setInitializer(const_array_38);
      gvar_array__ZL13_ps_coscof_p1->setInitializer(const_array_40);
      gvar_array__ZL13_ps_coscof_p2->setInitializer(const_array_42);
      gvar_array__ZL7_ps_0p5->setInitializer(const_array_44);
      gvar_array__ZL5_ps_1->setInitializer(const_array_46);
      gvar_array__ZL13_ps_sincof_p0->setInitializer(const_array_48);
      gvar_array__ZL13_ps_sincof_p1->setInitializer(const_array_50);
      gvar_array__ZL13_ps_sincof_p2->setInitializer(const_array_52);

      // Function Definitions

      // Function: sin_ps (sin_ps)
      {
        Function::arg_iterator args = sin_ps->arg_begin();
        Value* packed_x = args++;
        packed_x->setName("x");

        BasicBlock* label_entry = BasicBlock::Create(context, "entry",sin_ps,0);

        // Block entry (label_entry)
        LoadInst* packed_76 = new LoadInst(const_ptr_54, "", false, label_entry);
        CastInst* packed_tmp_i47 = new BitCastInst(packed_x, vectorTy_int_SIMD, "tmp.i47", label_entry);
        CastInst* packed_tmp1_i48 = new BitCastInst(packed_76, vectorTy_int_SIMD, "tmp1.i48", label_entry);
        BinaryOperator* packed_tmp2_i49 = BinaryOperator::Create(Instruction::And, packed_tmp_i47, packed_tmp1_i48, "tmp2.i49", label_entry);
        CastInst* packed_tmp3_i50 = new BitCastInst(packed_tmp2_i49, vectorTy_float_SIMD, "tmp3.i50", label_entry);
        LoadInst* packed_77 = new LoadInst(const_ptr_55, "", false, label_entry);
        CastInst* packed_tmp1_i44 = new BitCastInst(packed_77, vectorTy_int_SIMD, "tmp1.i44", label_entry);
        BinaryOperator* packed_tmp2_i45 = BinaryOperator::Create(Instruction::And, packed_tmp_i47, packed_tmp1_i44, "tmp2.i45", label_entry);
        LoadInst* packed_78 = new LoadInst(const_ptr_56, "", false, label_entry);
        BinaryOperator* packed_tmp_i42 = BinaryOperator::Create(Instruction::FMul, packed_tmp3_i50, packed_78, "tmp.i42", label_entry);
        CallInst* packed_79 = CallInst::Create(generateCvttps2dq(mod), ArrayRef<Value*>(packed_tmp_i42), "", label_entry);
        packed_79->setCallingConv(CallingConv::C);
        packed_79->setTailCall(true);AttrListPtr packed_79_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_79_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());
        }
        packed_79->setAttributes(packed_79_PAL);

        LoadInst* packed_80 = new LoadInst(const_ptr_57, "", false, label_entry);
        CastInst* packed_81 = new BitCastInst(packed_80, vectorTy_int_SIMD, "", label_entry);
        BinaryOperator* packed_tmp_i41 = BinaryOperator::Create(Instruction::Add, packed_79, packed_81, "tmp.i41", label_entry);
        CastInst* packed_82 = new BitCastInst(packed_tmp_i41, VectorTy_10, "", label_entry);
        LoadInst* packed_83 = new LoadInst(const_ptr_58, "", false, label_entry);
        BinaryOperator* packed_tmp_i40 = BinaryOperator::Create(Instruction::And, packed_82, packed_83, "tmp.i40", label_entry);
        CastInst* packed_84 = new BitCastInst(packed_tmp_i40, vectorTy_int_SIMD, "", label_entry);
        CallInst* packed_85 = CallInst::Create(generateCvtdq2ps(mod), ArrayRef<Value*>(packed_84), "", label_entry);
        packed_85->setCallingConv(CallingConv::C);
        packed_85->setTailCall(true);AttrListPtr packed_85_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_85_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_85->setAttributes(packed_85_PAL);

        LoadInst* packed_86 = new LoadInst(const_ptr_59, "", false, label_entry);
        BinaryOperator* packed_tmp_i39 = BinaryOperator::Create(Instruction::And, packed_tmp_i40, packed_86, "tmp.i39", label_entry);
        CastInst* packed_87 = new BitCastInst(packed_tmp_i39, vectorTy_int_SIMD, "", label_entry);
        std::vector<Value*> packed_88_params;
        packed_88_params.push_back(packed_87);
        packed_88_params.push_back(const_int32_60);
        CallInst* packed_88 = CallInst::Create(generatePsllid(mod), ArrayRef<Value*>(packed_88_params), "", label_entry);
        packed_88->setCallingConv(CallingConv::C);
        packed_88->setTailCall(true);AttrListPtr packed_88_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_88_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_88->setAttributes(packed_88_PAL);

        LoadInst* packed_89 = new LoadInst(const_ptr_61, "", false, label_entry);
        BinaryOperator* packed_tmp_i38 = BinaryOperator::Create(Instruction::And, packed_tmp_i40, packed_89, "tmp.i38", label_entry);
        CastInst* packed_90 = new BitCastInst(packed_tmp_i38, vectorTy_int_SIMD, "", label_entry);
        std::vector<Value*> packed_91_params;
        packed_91_params.push_back(packed_90);
        packed_91_params.push_back(const_packed_62);
        CallInst* packed_91 = CallInst::Create(generatePcmpeq(mod), ArrayRef<Value*>(packed_91_params), "", label_entry);
        packed_91->setCallingConv(CallingConv::C);
        packed_91->setTailCall(true);AttrListPtr packed_91_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_91_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_91->setAttributes(packed_91_PAL);

        BinaryOperator* packed_tmp2_i36 = BinaryOperator::Create(Instruction::Xor, packed_tmp2_i45, packed_88, "tmp2.i36", label_entry);
        LoadInst* packed_92 = new LoadInst(const_ptr_63, "", false, label_entry);
        LoadInst* packed_93 = new LoadInst(const_ptr_64, "", false, label_entry);
        LoadInst* packed_94 = new LoadInst(const_ptr_65, "", false, label_entry);
        BinaryOperator* packed_tmp_i33 = BinaryOperator::Create(Instruction::FMul, packed_85, packed_92, "tmp.i33", label_entry);
        BinaryOperator* packed_tmp_i32 = BinaryOperator::Create(Instruction::FMul, packed_85, packed_93, "tmp.i32", label_entry);
        BinaryOperator* packed_tmp_i31 = BinaryOperator::Create(Instruction::FMul, packed_85, packed_94, "tmp.i31", label_entry);
        BinaryOperator* packed_tmp_i30 = BinaryOperator::Create(Instruction::FAdd, packed_tmp3_i50, packed_tmp_i33, "tmp.i30", label_entry);
        BinaryOperator* packed_tmp_i29 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i30, packed_tmp_i32, "tmp.i29", label_entry);
        BinaryOperator* packed_tmp_i28 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i29, packed_tmp_i31, "tmp.i28", label_entry);
        LoadInst* packed_95 = new LoadInst(const_ptr_66, "", false, label_entry);
        BinaryOperator* packed_tmp_i27 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i28, packed_tmp_i28, "tmp.i27", label_entry);
        BinaryOperator* packed_tmp_i26 = BinaryOperator::Create(Instruction::FMul, packed_95, packed_tmp_i27, "tmp.i26", label_entry);
        LoadInst* packed_96 = new LoadInst(const_ptr_67, "", false, label_entry);
        BinaryOperator* packed_tmp_i25 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i26, packed_96, "tmp.i25", label_entry);
        BinaryOperator* packed_tmp_i24 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i25, packed_tmp_i27, "tmp.i24", label_entry);
        LoadInst* packed_97 = new LoadInst(const_ptr_68, "", false, label_entry);
        BinaryOperator* packed_tmp_i23 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i24, packed_97, "tmp.i23", label_entry);
        BinaryOperator* packed_tmp_i22 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i23, packed_tmp_i27, "tmp.i22", label_entry);
        BinaryOperator* packed_tmp_i21 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i22, packed_tmp_i27, "tmp.i21", label_entry);
        LoadInst* packed_98 = new LoadInst(const_ptr_69, "", false, label_entry);
        BinaryOperator* packed_tmp_i20 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i27, packed_98, "tmp.i20", label_entry);
        BinaryOperator* packed_tmp_i19 = BinaryOperator::Create(Instruction::FSub, packed_tmp_i21, packed_tmp_i20, "tmp.i19", label_entry);
        LoadInst* packed_99 = new LoadInst(const_ptr_70, "", false, label_entry);
        BinaryOperator* packed_tmp_i18 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i19, packed_99, "tmp.i18", label_entry);
        LoadInst* packed_100 = new LoadInst(const_ptr_71, "", false, label_entry);
        BinaryOperator* packed_tmp_i17 = BinaryOperator::Create(Instruction::FMul, packed_100, packed_tmp_i27, "tmp.i17", label_entry);
        LoadInst* packed_101 = new LoadInst(const_ptr_72, "", false, label_entry);
        BinaryOperator* packed_tmp_i16 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i17, packed_101, "tmp.i16", label_entry);
        BinaryOperator* packed_tmp_i15 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i16, packed_tmp_i27, "tmp.i15", label_entry);
        LoadInst* packed_102 = new LoadInst(const_ptr_73, "", false, label_entry);
        BinaryOperator* packed_tmp_i14 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i15, packed_102, "tmp.i14", label_entry);
        BinaryOperator* packed_tmp_i13 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i14, packed_tmp_i27, "tmp.i13", label_entry);
        BinaryOperator* packed_tmp_i12 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i13, packed_tmp_i28, "tmp.i12", label_entry);
        BinaryOperator* packed_tmp_i11 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i12, packed_tmp_i28, "tmp.i11", label_entry);
        CastInst* packed_tmp1_i8 = new BitCastInst(packed_tmp_i11, vectorTy_int_SIMD, "tmp1.i8", label_entry);
        BinaryOperator* packed_tmp2_i9 = BinaryOperator::Create(Instruction::And, packed_91, packed_tmp1_i8, "tmp2.i9", label_entry);
        CastInst* packed_tmp3_i10 = new BitCastInst(packed_tmp2_i9, vectorTy_float_SIMD, "tmp3.i10", label_entry);
        CastInst* packed_tmp1_i4 = new BitCastInst(packed_tmp_i18, vectorTy_int_SIMD, "tmp1.i4", label_entry);
        BinaryOperator* packed_tmp2_i5 = BinaryOperator::Create(Instruction::Xor, packed_91, const_packed_74, "tmp2.i5", label_entry);
        BinaryOperator* packed_tmp3_i6 = BinaryOperator::Create(Instruction::And, packed_tmp1_i4, packed_tmp2_i5, "tmp3.i6", label_entry);
        CastInst* packed_tmp4_i = new BitCastInst(packed_tmp3_i6, vectorTy_float_SIMD, "tmp4.i", label_entry);
        BinaryOperator* packed_tmp_i2 = BinaryOperator::Create(Instruction::FAdd, packed_tmp4_i, packed_tmp3_i10, "tmp.i2", label_entry);
        CastInst* packed_tmp_i = new BitCastInst(packed_tmp_i2, vectorTy_int_SIMD, "tmp.i", label_entry);
        BinaryOperator* packed_tmp2_i = BinaryOperator::Create(Instruction::Xor, packed_tmp_i, packed_tmp2_i36, "tmp2.i", label_entry);
        CastInst* packed_tmp3_i = new BitCastInst(packed_tmp2_i, vectorTy_float_SIMD, "tmp3.i", label_entry);
        ReturnInst::Create(context, packed_tmp3_i, label_entry);

      }

      return sin_ps;
    }
    Function* generateCosPS(Module& mod, const unsigned simdWidth) const {
        if (Function* tmpF = mod.getFunction("cos_ps")) {
            return tmpF;
        }

        assert (!(mod.getFunction("_ZL17_ps_inv_sign_mask") ||
                mod.getFunction("_ZL15_ps_cephes_FOPI") ||
                mod.getFunction("_ZL7_pi32_1") ||
                mod.getFunction("_ZL10_pi32_inv1") ||
                mod.getFunction("_ZL7_pi32_2") ||
                mod.getFunction("_ZL7_pi32_4") ||
                mod.getFunction("_ZL20_ps_minus_cephes_DP1") ||
                mod.getFunction("_ZL20_ps_minus_cephes_DP2") ||
                mod.getFunction("_ZL20_ps_minus_cephes_DP3") ||
                mod.getFunction("_ZL13_ps_coscof_p0") ||
                mod.getFunction("_ZL13_ps_coscof_p1") ||
                mod.getFunction("_ZL13_ps_coscof_p2") ||
                mod.getFunction("_ZL7_ps_0p5") ||
                mod.getFunction("_ZL5_ps_1") ||
                mod.getFunction("_ZL13_ps_sincof_p0") ||
                mod.getFunction("_ZL13_ps_sincof_p1") ||
                mod.getFunction("_ZL13_ps_sincof_p2")) && "function to be generated is already declared in module!");

		LLVMContext& context = mod.getContext();

		VectorType* vectorTy_float_SIMD = VectorType::get(Type::getFloatTy(context), simdWidth);
		VectorType* vectorTy_int_SIMD = VectorType::get(Type::getInt32Ty(context), simdWidth);
        
      // Type Definitions
      ArrayType* ArrayTy_0 = ArrayType::get(IntegerType::get(context, 32), 4);

      ArrayType* ArrayTy_2 = ArrayType::get(Type::getFloatTy(context), 4);

      std::vector<Type*>FuncTy_5_args;
      FuncTy_5_args.push_back(vectorTy_float_SIMD);
      FunctionType* FuncTy_5 = FunctionType::get(
        /*Result=*/vectorTy_float_SIMD,
        /*Params=*/FuncTy_5_args,
        /*isVarArg=*/false);

      PointerType* PointerTy_6 = PointerType::get(vectorTy_float_SIMD, 0);

      VectorType* VectorTy_10 = VectorType::get(IntegerType::get(context, 64), 2);

      PointerType* PointerTy_11 = PointerType::get(VectorTy_10, 0);
      
      // Function Declarations

      Function* cos_ps = Function::Create(
        /*Type=*/FuncTy_5,
        /*Linkage=*/GlobalValue::ExternalLinkage,
        /*Name=*/"cos_ps", &mod);
      cos_ps->setCallingConv(CallingConv::C);
      AttrListPtr cos_ps_PAL;
      {
        SmallVector<AttributeWithIndex, 4> Attrs;
        AttributeWithIndex PAWI;
        PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
        Attrs.push_back(PAWI);
        cos_ps_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

      }
      cos_ps->setAttributes(cos_ps_PAL);


      // Global Variable Declarations


      GlobalVariable* gvar_array__ZL17_ps_inv_sign_mask = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_inv_sign_mask");
      gvar_array__ZL17_ps_inv_sign_mask->setAlignment(16);

      GlobalVariable* gvar_array__ZL15_ps_cephes_FOPI = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL15_ps_cephes_FOPI");
      gvar_array__ZL15_ps_cephes_FOPI->setAlignment(16);

      GlobalVariable* gvar_array__ZL7_pi32_1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL7_pi32_1");
      gvar_array__ZL7_pi32_1->setAlignment(16);

      GlobalVariable* gvar_array__ZL10_pi32_inv1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL10_pi32_inv1");
      gvar_array__ZL10_pi32_inv1->setAlignment(16);

      GlobalVariable* gvar_array__ZL7_pi32_2 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL7_pi32_2");
      gvar_array__ZL7_pi32_2->setAlignment(16);

      GlobalVariable* gvar_array__ZL7_pi32_4 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL7_pi32_4");
      gvar_array__ZL7_pi32_4->setAlignment(16);

      GlobalVariable* gvar_array__ZL20_ps_minus_cephes_DP1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL20_ps_minus_cephes_DP1");
      gvar_array__ZL20_ps_minus_cephes_DP1->setAlignment(16);

      GlobalVariable* gvar_array__ZL20_ps_minus_cephes_DP2 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL20_ps_minus_cephes_DP2");
      gvar_array__ZL20_ps_minus_cephes_DP2->setAlignment(16);

      GlobalVariable* gvar_array__ZL20_ps_minus_cephes_DP3 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL20_ps_minus_cephes_DP3");
      gvar_array__ZL20_ps_minus_cephes_DP3->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_coscof_p0 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_coscof_p0");
      gvar_array__ZL13_ps_coscof_p0->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_coscof_p1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_coscof_p1");
      gvar_array__ZL13_ps_coscof_p1->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_coscof_p2 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_coscof_p2");
      gvar_array__ZL13_ps_coscof_p2->setAlignment(16);

      GlobalVariable* gvar_array__ZL7_ps_0p5 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL7_ps_0p5");

      GlobalVariable* gvar_array__ZL5_ps_1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL5_ps_1");
      gvar_array__ZL5_ps_1->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_sincof_p0 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_sincof_p0");
      gvar_array__ZL13_ps_sincof_p0->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_sincof_p1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_sincof_p1");
      gvar_array__ZL13_ps_sincof_p1->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_sincof_p2 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_sincof_p2");
      gvar_array__ZL13_ps_sincof_p2->setAlignment(16);

      // Constant Definitions
      std::vector<Constant*> const_array_18_elems;
      ConstantInt* const_int32_19 = ConstantInt::get(context, APInt(32,  "2147483647", 10));
      const_array_18_elems.push_back(const_int32_19);
      const_array_18_elems.push_back(const_int32_19);
      const_array_18_elems.push_back(const_int32_19);
      const_array_18_elems.push_back(const_int32_19);
      Constant* const_array_18 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_18_elems));
      std::vector<Constant*> const_array_20_elems;
      ConstantFP* const_float_21 = ConstantFP::get(context, APFloat(BitsToFloat(0x3FA2F983U) /* 1.273239e+00 */));
      const_array_20_elems.push_back(const_float_21);
      const_array_20_elems.push_back(const_float_21);
      const_array_20_elems.push_back(const_float_21);
      const_array_20_elems.push_back(const_float_21);
      Constant* const_array_20 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_20_elems));
      std::vector<Constant*> const_array_22_elems;
      ConstantInt* const_int32_23 = ConstantInt::get(context, APInt(32,  "1", 10));
      const_array_22_elems.push_back(const_int32_23);
      const_array_22_elems.push_back(const_int32_23);
      const_array_22_elems.push_back(const_int32_23);
      const_array_22_elems.push_back(const_int32_23);
      Constant* const_array_22 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_22_elems));
      std::vector<Constant*> const_array_24_elems;
      ConstantInt* const_int32_25 = ConstantInt::get(context, APInt(32,  "-2", 10));
      const_array_24_elems.push_back(const_int32_25);
      const_array_24_elems.push_back(const_int32_25);
      const_array_24_elems.push_back(const_int32_25);
      const_array_24_elems.push_back(const_int32_25);
      Constant* const_array_24 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_24_elems));
      std::vector<Constant*> const_array_26_elems;
      ConstantInt* const_int32_27 = ConstantInt::get(context, APInt(32,  "2", 10));
      const_array_26_elems.push_back(const_int32_27);
      const_array_26_elems.push_back(const_int32_27);
      const_array_26_elems.push_back(const_int32_27);
      const_array_26_elems.push_back(const_int32_27);
      Constant* const_array_26 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_26_elems));
      std::vector<Constant*> const_array_28_elems;
      ConstantInt* const_int32_29 = ConstantInt::get(context, APInt(32,  "4", 10));
      const_array_28_elems.push_back(const_int32_29);
      const_array_28_elems.push_back(const_int32_29);
      const_array_28_elems.push_back(const_int32_29);
      const_array_28_elems.push_back(const_int32_29);
      Constant* const_array_28 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_28_elems));
      std::vector<Constant*> const_array_30_elems;
      ConstantFP* const_float_31 = ConstantFP::get(context, APFloat(BitsToFloat(0xBF490000U) /* -7.851562e-01 */));
      const_array_30_elems.push_back(const_float_31);
      const_array_30_elems.push_back(const_float_31);
      const_array_30_elems.push_back(const_float_31);
      const_array_30_elems.push_back(const_float_31);
      Constant* const_array_30 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_30_elems));
      std::vector<Constant*> const_array_32_elems;
      ConstantFP* const_float_33 = ConstantFP::get(context, APFloat(BitsToFloat(0xB97DA000U) /* -2.418756e-04 */));
      const_array_32_elems.push_back(const_float_33);
      const_array_32_elems.push_back(const_float_33);
      const_array_32_elems.push_back(const_float_33);
      const_array_32_elems.push_back(const_float_33);
      Constant* const_array_32 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_32_elems));
      std::vector<Constant*> const_array_34_elems;
      ConstantFP* const_float_35 = ConstantFP::get(context, APFloat(-3.774895e-08f));
      const_array_34_elems.push_back(const_float_35);
      const_array_34_elems.push_back(const_float_35);
      const_array_34_elems.push_back(const_float_35);
      const_array_34_elems.push_back(const_float_35);
      Constant* const_array_34 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_34_elems));
      std::vector<Constant*> const_array_36_elems;
      ConstantFP* const_float_37 = ConstantFP::get(context, APFloat(BitsToFloat(0x37CCF5CEU) /* 2.443316e-05 */));
      const_array_36_elems.push_back(const_float_37);
      const_array_36_elems.push_back(const_float_37);
      const_array_36_elems.push_back(const_float_37);
      const_array_36_elems.push_back(const_float_37);
      Constant* const_array_36 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_36_elems));
      std::vector<Constant*> const_array_38_elems;
      ConstantFP* const_float_39 = ConstantFP::get(context, APFloat(BitsToFloat(0xBAB6061AU) /* -1.388732e-03 */));
      const_array_38_elems.push_back(const_float_39);
      const_array_38_elems.push_back(const_float_39);
      const_array_38_elems.push_back(const_float_39);
      const_array_38_elems.push_back(const_float_39);
      Constant* const_array_38 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_38_elems));
      std::vector<Constant*> const_array_40_elems;
      ConstantFP* const_float_41 = ConstantFP::get(context, APFloat(BitsToFloat(0x3D2AAAA5U) /* 4.166665e-02 */));
      const_array_40_elems.push_back(const_float_41);
      const_array_40_elems.push_back(const_float_41);
      const_array_40_elems.push_back(const_float_41);
      const_array_40_elems.push_back(const_float_41);
      Constant* const_array_40 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_40_elems));
      std::vector<Constant*> const_array_42_elems;
      ConstantFP* const_float_43 = ConstantFP::get(context, APFloat(5.000000e-01f));
      const_array_42_elems.push_back(const_float_43);
      const_array_42_elems.push_back(const_float_43);
      const_array_42_elems.push_back(const_float_43);
      const_array_42_elems.push_back(const_float_43);
      Constant* const_array_42 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_42_elems));
      std::vector<Constant*> const_array_44_elems;
      ConstantFP* const_float_45 = ConstantFP::get(context, APFloat(1.000000e+00f));
      const_array_44_elems.push_back(const_float_45);
      const_array_44_elems.push_back(const_float_45);
      const_array_44_elems.push_back(const_float_45);
      const_array_44_elems.push_back(const_float_45);
      Constant* const_array_44 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_44_elems));
      std::vector<Constant*> const_array_46_elems;
      ConstantFP* const_float_47 = ConstantFP::get(context, APFloat(BitsToFloat(0xB94CA1F9U) /* -1.951530e-04 */));
      const_array_46_elems.push_back(const_float_47);
      const_array_46_elems.push_back(const_float_47);
      const_array_46_elems.push_back(const_float_47);
      const_array_46_elems.push_back(const_float_47);
      Constant* const_array_46 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_46_elems));
      std::vector<Constant*> const_array_48_elems;
      ConstantFP* const_float_49 = ConstantFP::get(context, APFloat(8.332161e-03f));
      const_array_48_elems.push_back(const_float_49);
      const_array_48_elems.push_back(const_float_49);
      const_array_48_elems.push_back(const_float_49);
      const_array_48_elems.push_back(const_float_49);
      Constant* const_array_48 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_48_elems));
      std::vector<Constant*> const_array_50_elems;
      ConstantFP* const_float_51 = ConstantFP::get(context, APFloat(BitsToFloat(0xBE2AAAA3U) /* -1.666666e-01 */));
      const_array_50_elems.push_back(const_float_51);
      const_array_50_elems.push_back(const_float_51);
      const_array_50_elems.push_back(const_float_51);
      const_array_50_elems.push_back(const_float_51);
      Constant* const_array_50 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_50_elems));
      Constant* const_ptr_52 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_inv_sign_mask, PointerTy_6);
      Constant* const_ptr_53 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL15_ps_cephes_FOPI, PointerTy_6);
      Constant* const_ptr_54 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL7_pi32_1, PointerTy_11);
      Constant* const_ptr_55 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL10_pi32_inv1, PointerTy_11);
      Constant* const_ptr_56 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL7_pi32_2, PointerTy_11);
      Constant* const_ptr_57 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL7_pi32_4, PointerTy_11);
      std::vector<Constant*> const_packed_58_elems;
      ConstantInt* const_int64_59 = ConstantInt::get(context, APInt(64,  "-1", 10));
      const_packed_58_elems.push_back(const_int64_59);
      const_packed_58_elems.push_back(const_int64_59);
      Constant* const_packed_58 = ConstantVector::get(ArrayRef<Constant*>(const_packed_58_elems));
      ConstantInt* const_int32_60 = ConstantInt::get(context, APInt(32,  "29", 10));
      ConstantAggregateZero* const_packed_61 = ConstantAggregateZero::get(vectorTy_int_SIMD); //llvm 2.5
      Constant* const_ptr_62 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL20_ps_minus_cephes_DP1, PointerTy_6);
      Constant* const_ptr_63 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL20_ps_minus_cephes_DP2, PointerTy_6);
      Constant* const_ptr_64 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL20_ps_minus_cephes_DP3, PointerTy_6);
      Constant* const_ptr_65 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_coscof_p0, PointerTy_6);
      Constant* const_ptr_66 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_coscof_p1, PointerTy_6);
      Constant* const_ptr_67 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_coscof_p2, PointerTy_6);
      Constant* const_ptr_68 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL7_ps_0p5, PointerTy_6);
      Constant* const_ptr_69 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL5_ps_1, PointerTy_6);
      Constant* const_ptr_70 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_sincof_p0, PointerTy_6);
      Constant* const_ptr_71 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_sincof_p1, PointerTy_6);
      Constant* const_ptr_72 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_sincof_p2, PointerTy_6);
      std::vector<Constant*> const_packed_73_elems;
      ConstantInt* const_int32_74 = ConstantInt::get(context, APInt(32,  "-1", 10));
      const_packed_73_elems.push_back(const_int32_74);
      const_packed_73_elems.push_back(const_int32_74);
      const_packed_73_elems.push_back(const_int32_74);
      const_packed_73_elems.push_back(const_int32_74);
      Constant* const_packed_73 = ConstantVector::get(ArrayRef<Constant*>(const_packed_73_elems));

      // Global Variable Definitions
      gvar_array__ZL17_ps_inv_sign_mask->setInitializer(const_array_18);
      gvar_array__ZL15_ps_cephes_FOPI->setInitializer(const_array_20);
      gvar_array__ZL7_pi32_1->setInitializer(const_array_22);
      gvar_array__ZL10_pi32_inv1->setInitializer(const_array_24);
      gvar_array__ZL7_pi32_2->setInitializer(const_array_26);
      gvar_array__ZL7_pi32_4->setInitializer(const_array_28);
      gvar_array__ZL20_ps_minus_cephes_DP1->setInitializer(const_array_30);
      gvar_array__ZL20_ps_minus_cephes_DP2->setInitializer(const_array_32);
      gvar_array__ZL20_ps_minus_cephes_DP3->setInitializer(const_array_34);
      gvar_array__ZL13_ps_coscof_p0->setInitializer(const_array_36);
      gvar_array__ZL13_ps_coscof_p1->setInitializer(const_array_38);
      gvar_array__ZL13_ps_coscof_p2->setInitializer(const_array_40);
      gvar_array__ZL7_ps_0p5->setInitializer(const_array_42);
      gvar_array__ZL5_ps_1->setInitializer(const_array_44);
      gvar_array__ZL13_ps_sincof_p0->setInitializer(const_array_46);
      gvar_array__ZL13_ps_sincof_p1->setInitializer(const_array_48);
      gvar_array__ZL13_ps_sincof_p2->setInitializer(const_array_50);

      // Function Definitions

      // Function: cos_ps (cos_ps)
      {
        Function::arg_iterator args = cos_ps->arg_begin();
        Value* packed_x = args++;
        packed_x->setName("x");

        BasicBlock* label_entry = BasicBlock::Create(context, "entry",cos_ps,0);

        // Block entry (label_entry)
        LoadInst* packed_75 = new LoadInst(const_ptr_52, "", false, label_entry);
        CastInst* packed_tmp_i41 = new BitCastInst(packed_x, vectorTy_int_SIMD, "tmp.i41", label_entry);
        CastInst* packed_tmp1_i42 = new BitCastInst(packed_75, vectorTy_int_SIMD, "tmp1.i42", label_entry);
        BinaryOperator* packed_tmp2_i43 = BinaryOperator::Create(Instruction::And, packed_tmp_i41, packed_tmp1_i42, "tmp2.i43", label_entry);
        CastInst* packed_tmp3_i44 = new BitCastInst(packed_tmp2_i43, vectorTy_float_SIMD, "tmp3.i44", label_entry);
        LoadInst* packed_76 = new LoadInst(const_ptr_53, "", false, label_entry);
        BinaryOperator* packed_tmp_i40 = BinaryOperator::Create(Instruction::FMul, packed_tmp3_i44, packed_76, "tmp.i40", label_entry);

        CallInst* packed_77 = CallInst::Create(generateCvttps2dq(mod), ArrayRef<Value*>(packed_tmp_i40), "", label_entry);
        packed_77->setCallingConv(CallingConv::C);
        packed_77->setTailCall(true);AttrListPtr packed_77_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_77_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_77->setAttributes(packed_77_PAL);

        LoadInst* packed_78 = new LoadInst(const_ptr_54, "", false, label_entry);
        CastInst* packed_79 = new BitCastInst(packed_78, vectorTy_int_SIMD, "", label_entry);
        BinaryOperator* packed_tmp_i39 = BinaryOperator::Create(Instruction::Add, packed_77, packed_79, "tmp.i39", label_entry);
        CastInst* packed_80 = new BitCastInst(packed_tmp_i39, VectorTy_10, "", label_entry);
        LoadInst* packed_81 = new LoadInst(const_ptr_55, "", false, label_entry);
        BinaryOperator* packed_tmp_i38 = BinaryOperator::Create(Instruction::And, packed_80, packed_81, "tmp.i38", label_entry);
        CastInst* packed_82 = new BitCastInst(packed_tmp_i38, vectorTy_int_SIMD, "", label_entry);
        CallInst* packed_83 = CallInst::Create(generateCvtdq2ps(mod), ArrayRef<Value*>(packed_82), "", label_entry);
        packed_83->setCallingConv(CallingConv::C);
        packed_83->setTailCall(true);AttrListPtr packed_83_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_83_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_83->setAttributes(packed_83_PAL);

        LoadInst* packed_84 = new LoadInst(const_ptr_56, "", false, label_entry);
        CastInst* packed_85 = new BitCastInst(packed_84, vectorTy_int_SIMD, "", label_entry);
        BinaryOperator* packed_tmp_i37 = BinaryOperator::Create(Instruction::Sub, packed_82, packed_85, "tmp.i37", label_entry);
        CastInst* packed_86 = new BitCastInst(packed_tmp_i37, VectorTy_10, "", label_entry);
        LoadInst* packed_87 = new LoadInst(const_ptr_57, "", false, label_entry);
        BinaryOperator* packed_tmp_i35 = BinaryOperator::Create(Instruction::Xor, packed_86, const_packed_58, "tmp.i35", label_entry);
        BinaryOperator* packed_tmp1_i36 = BinaryOperator::Create(Instruction::And, packed_87, packed_tmp_i35, "tmp1.i36", label_entry);
        CastInst* packed_88 = new BitCastInst(packed_tmp1_i36, vectorTy_int_SIMD, "", label_entry);
        std::vector<Value*> packed_89_params;
        packed_89_params.push_back(packed_88);
        packed_89_params.push_back(const_int32_60);
        CallInst* packed_89 = CallInst::Create(generatePsllid(mod), ArrayRef<Value*>(packed_89_params), "", label_entry);
        packed_89->setCallingConv(CallingConv::C);
        packed_89->setTailCall(true);AttrListPtr packed_89_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_89_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_89->setAttributes(packed_89_PAL);

        BinaryOperator* packed_tmp_i34 = BinaryOperator::Create(Instruction::And, packed_86, packed_84, "tmp.i34", label_entry);
        CastInst* packed_90 = new BitCastInst(packed_tmp_i34, vectorTy_int_SIMD, "", label_entry);
        std::vector<Value*> packed_91_params;
        packed_91_params.push_back(packed_90);
        packed_91_params.push_back(const_packed_61);
        CallInst* packed_91 = CallInst::Create(generatePcmpeq(mod), ArrayRef<Value*>(packed_91_params), "", label_entry);
        packed_91->setCallingConv(CallingConv::C);
        packed_91->setTailCall(true);AttrListPtr packed_91_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_91_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_91->setAttributes(packed_91_PAL);

        LoadInst* packed_92 = new LoadInst(const_ptr_62, "", false, label_entry);
        LoadInst* packed_93 = new LoadInst(const_ptr_63, "", false, label_entry);
        LoadInst* packed_94 = new LoadInst(const_ptr_64, "", false, label_entry);
        BinaryOperator* packed_tmp_i33 = BinaryOperator::Create(Instruction::FMul, packed_83, packed_92, "tmp.i33", label_entry);
        BinaryOperator* packed_tmp_i32 = BinaryOperator::Create(Instruction::FMul, packed_83, packed_93, "tmp.i32", label_entry);
        BinaryOperator* packed_tmp_i31 = BinaryOperator::Create(Instruction::FMul, packed_83, packed_94, "tmp.i31", label_entry);
        BinaryOperator* packed_tmp_i30 = BinaryOperator::Create(Instruction::FAdd, packed_tmp3_i44, packed_tmp_i33, "tmp.i30", label_entry);
        BinaryOperator* packed_tmp_i29 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i30, packed_tmp_i32, "tmp.i29", label_entry);
        BinaryOperator* packed_tmp_i28 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i29, packed_tmp_i31, "tmp.i28", label_entry);
        LoadInst* packed_95 = new LoadInst(const_ptr_65, "", false, label_entry);
        BinaryOperator* packed_tmp_i27 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i28, packed_tmp_i28, "tmp.i27", label_entry);
        BinaryOperator* packed_tmp_i26 = BinaryOperator::Create(Instruction::FMul, packed_95, packed_tmp_i27, "tmp.i26", label_entry);
        LoadInst* packed_96 = new LoadInst(const_ptr_66, "", false, label_entry);
        BinaryOperator* packed_tmp_i25 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i26, packed_96, "tmp.i25", label_entry);
        BinaryOperator* packed_tmp_i24 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i25, packed_tmp_i27, "tmp.i24", label_entry);
        LoadInst* packed_97 = new LoadInst(const_ptr_67, "", false, label_entry);
        BinaryOperator* packed_tmp_i23 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i24, packed_97, "tmp.i23", label_entry);
        BinaryOperator* packed_tmp_i22 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i23, packed_tmp_i27, "tmp.i22", label_entry);
        BinaryOperator* packed_tmp_i21 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i22, packed_tmp_i27, "tmp.i21", label_entry);
        LoadInst* packed_98 = new LoadInst(const_ptr_68, "", false, label_entry);
        BinaryOperator* packed_tmp_i20 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i27, packed_98, "tmp.i20", label_entry);
        BinaryOperator* packed_tmp_i19 = BinaryOperator::Create(Instruction::FSub, packed_tmp_i21, packed_tmp_i20, "tmp.i19", label_entry);
        LoadInst* packed_99 = new LoadInst(const_ptr_69, "", false, label_entry);
        BinaryOperator* packed_tmp_i18 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i19, packed_99, "tmp.i18", label_entry);
        LoadInst* packed_100 = new LoadInst(const_ptr_70, "", false, label_entry);
        BinaryOperator* packed_tmp_i17 = BinaryOperator::Create(Instruction::FMul, packed_100, packed_tmp_i27, "tmp.i17", label_entry);
        LoadInst* packed_101 = new LoadInst(const_ptr_71, "", false, label_entry);
        BinaryOperator* packed_tmp_i16 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i17, packed_101, "tmp.i16", label_entry);
        BinaryOperator* packed_tmp_i15 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i16, packed_tmp_i27, "tmp.i15", label_entry);
        LoadInst* packed_102 = new LoadInst(const_ptr_72, "", false, label_entry);
        BinaryOperator* packed_tmp_i14 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i15, packed_102, "tmp.i14", label_entry);
        BinaryOperator* packed_tmp_i13 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i14, packed_tmp_i27, "tmp.i13", label_entry);
        BinaryOperator* packed_tmp_i12 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i13, packed_tmp_i28, "tmp.i12", label_entry);
        BinaryOperator* packed_tmp_i11 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i12, packed_tmp_i28, "tmp.i11", label_entry);
        CastInst* packed_tmp1_i8 = new BitCastInst(packed_tmp_i11, vectorTy_int_SIMD, "tmp1.i8", label_entry);
        BinaryOperator* packed_tmp2_i9 = BinaryOperator::Create(Instruction::And, packed_91, packed_tmp1_i8, "tmp2.i9", label_entry);
        CastInst* packed_tmp3_i10 = new BitCastInst(packed_tmp2_i9, vectorTy_float_SIMD, "tmp3.i10", label_entry);
        CastInst* packed_tmp1_i4 = new BitCastInst(packed_tmp_i18, vectorTy_int_SIMD, "tmp1.i4", label_entry);
        BinaryOperator* packed_tmp2_i5 = BinaryOperator::Create(Instruction::Xor, packed_91, const_packed_73, "tmp2.i5", label_entry);
        BinaryOperator* packed_tmp3_i6 = BinaryOperator::Create(Instruction::And, packed_tmp1_i4, packed_tmp2_i5, "tmp3.i6", label_entry);
        CastInst* packed_tmp4_i = new BitCastInst(packed_tmp3_i6, vectorTy_float_SIMD, "tmp4.i", label_entry);
        BinaryOperator* packed_tmp_i2 = BinaryOperator::Create(Instruction::FAdd, packed_tmp4_i, packed_tmp3_i10, "tmp.i2", label_entry);
        CastInst* packed_tmp_i = new BitCastInst(packed_tmp_i2, vectorTy_int_SIMD, "tmp.i", label_entry);
        BinaryOperator* packed_tmp2_i = BinaryOperator::Create(Instruction::Xor, packed_tmp_i, packed_89, "tmp2.i", label_entry);
        CastInst* packed_tmp3_i = new BitCastInst(packed_tmp2_i, vectorTy_float_SIMD, "tmp3.i", label_entry);
        ReturnInst::Create(context, packed_tmp3_i, label_entry);

      }

      return cos_ps;
    }
    Function* generateSinCosPS(Module& mod, const unsigned simdWidth) const {
        if (Function* tmpF = mod.getFunction("sincos_ps")) {
            return tmpF;
        }

        assert (!(mod.getFunction("_ZL17_ps_inv_sign_mask") ||
                mod.getFunction("_ZL13_ps_sign_mask") ||
                mod.getFunction("_ZL15_ps_cephes_FOPI") ||
                mod.getFunction("_ZL7_pi32_1") ||
                mod.getFunction("_ZL10_pi32_inv1") ||
                mod.getFunction("_ZL7_pi32_2") ||
                mod.getFunction("_ZL7_pi32_4") ||
                mod.getFunction("_ZL20_ps_minus_cephes_DP1") ||
                mod.getFunction("_ZL20_ps_minus_cephes_DP2") ||
                mod.getFunction("_ZL20_ps_minus_cephes_DP3") ||
                mod.getFunction("_ZL13_ps_coscof_p0") ||
                mod.getFunction("_ZL13_ps_coscof_p1") ||
                mod.getFunction("_ZL13_ps_coscof_p2") ||
                mod.getFunction("_ZL7_ps_0p5") ||
                mod.getFunction("_ZL5_ps_1") ||
                mod.getFunction("_ZL13_ps_sincof_p0") ||
                mod.getFunction("_ZL13_ps_sincof_p1") ||
                mod.getFunction("_ZL13_ps_sincof_p2")) && "function to be generated is already declared in module!");

		LLVMContext& context = mod.getContext();

		VectorType* vectorTy_float_SIMD = VectorType::get(Type::getFloatTy(context), simdWidth);
		VectorType* vectorTy_int_SIMD = VectorType::get(Type::getInt32Ty(context), simdWidth);

      // Type Definitions
      ArrayType* ArrayTy_0 = ArrayType::get(IntegerType::get(context, 32), 4);

      ArrayType* ArrayTy_2 = ArrayType::get(Type::getFloatTy(context), 4);

      std::vector<Type*>FuncTy_4_args;

      FuncTy_4_args.push_back(vectorTy_float_SIMD);
      PointerType* PointerTy_6 = PointerType::get(vectorTy_float_SIMD, 0);

      FuncTy_4_args.push_back(PointerTy_6);
      FuncTy_4_args.push_back(PointerTy_6);
      FunctionType* FuncTy_4 = FunctionType::get(
        /*Result=*/Type::getVoidTy(context),
        /*Params=*/FuncTy_4_args,
        /*isVarArg=*/false);

      VectorType* VectorTy_10 = VectorType::get(IntegerType::get(context, 64), 2);

      PointerType* PointerTy_11 = PointerType::get(VectorTy_10, 0);

      // Function Declarations

      Function* sincos_ps = Function::Create(
        /*Type=*/FuncTy_4,
        /*Linkage=*/GlobalValue::ExternalLinkage,
        /*Name=*/"sincos_ps", &mod);
      sincos_ps->setCallingConv(CallingConv::C);
      AttrListPtr sincos_ps_PAL;
      {
        SmallVector<AttributeWithIndex, 4> Attrs;
        AttributeWithIndex PAWI;
        PAWI.Index = 2U; PAWI.Attrs = 0  | Attribute::NoCapture;
        Attrs.push_back(PAWI);
        PAWI.Index = 3U; PAWI.Attrs = 0  | Attribute::NoCapture;
        Attrs.push_back(PAWI);
        PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind;
        Attrs.push_back(PAWI);
        sincos_ps_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

      }
      sincos_ps->setAttributes(sincos_ps_PAL);


      // Global Variable Declarations


      GlobalVariable* gvar_array__ZL17_ps_inv_sign_mask = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_inv_sign_mask");
      gvar_array__ZL17_ps_inv_sign_mask->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_sign_mask = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_sign_mask");
      gvar_array__ZL13_ps_sign_mask->setAlignment(16);

      GlobalVariable* gvar_array__ZL15_ps_cephes_FOPI = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL15_ps_cephes_FOPI");
      gvar_array__ZL15_ps_cephes_FOPI->setAlignment(16);

      GlobalVariable* gvar_array__ZL7_pi32_1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL7_pi32_1");
      gvar_array__ZL7_pi32_1->setAlignment(16);

      GlobalVariable* gvar_array__ZL10_pi32_inv1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL10_pi32_inv1");
      gvar_array__ZL10_pi32_inv1->setAlignment(16);

      GlobalVariable* gvar_array__ZL7_pi32_4 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL7_pi32_4");
      gvar_array__ZL7_pi32_4->setAlignment(16);

      GlobalVariable* gvar_array__ZL7_pi32_2 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL7_pi32_2");
      gvar_array__ZL7_pi32_2->setAlignment(16);

      GlobalVariable* gvar_array__ZL20_ps_minus_cephes_DP1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL20_ps_minus_cephes_DP1");
      gvar_array__ZL20_ps_minus_cephes_DP1->setAlignment(16);

      GlobalVariable* gvar_array__ZL20_ps_minus_cephes_DP2 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL20_ps_minus_cephes_DP2");
      gvar_array__ZL20_ps_minus_cephes_DP2->setAlignment(16);

      GlobalVariable* gvar_array__ZL20_ps_minus_cephes_DP3 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL20_ps_minus_cephes_DP3");
      gvar_array__ZL20_ps_minus_cephes_DP3->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_coscof_p0 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_coscof_p0");
      gvar_array__ZL13_ps_coscof_p0->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_coscof_p1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_coscof_p1");
      gvar_array__ZL13_ps_coscof_p1->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_coscof_p2 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_coscof_p2");
      gvar_array__ZL13_ps_coscof_p2->setAlignment(16);

      GlobalVariable* gvar_array__ZL7_ps_0p5 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL7_ps_0p5");
      gvar_array__ZL7_ps_0p5->setAlignment(16);

      GlobalVariable* gvar_array__ZL5_ps_1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL5_ps_1");
      gvar_array__ZL5_ps_1->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_sincof_p0 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_sincof_p0");
      gvar_array__ZL13_ps_sincof_p0->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_sincof_p1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_sincof_p1");
      gvar_array__ZL13_ps_sincof_p1->setAlignment(16);

      GlobalVariable* gvar_array__ZL13_ps_sincof_p2 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL13_ps_sincof_p2");
      gvar_array__ZL13_ps_sincof_p2->setAlignment(16);

      // Constant Definitions
      std::vector<Constant*> const_array_18_elems;
      ConstantInt* const_int32_19 = ConstantInt::get(context, APInt(32,  "2147483647", 10));
      const_array_18_elems.push_back(const_int32_19);
      const_array_18_elems.push_back(const_int32_19);
      const_array_18_elems.push_back(const_int32_19);
      const_array_18_elems.push_back(const_int32_19);
      Constant* const_array_18 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_18_elems));
      std::vector<Constant*> const_array_20_elems;
      ConstantInt* const_int32_21 = ConstantInt::get(context, APInt(32,  "-2147483648", 10));
      const_array_20_elems.push_back(const_int32_21);
      const_array_20_elems.push_back(const_int32_21);
      const_array_20_elems.push_back(const_int32_21);
      const_array_20_elems.push_back(const_int32_21);
      Constant* const_array_20 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_20_elems));
      std::vector<Constant*> const_array_22_elems;
      ConstantFP* const_float_23 = ConstantFP::get(context, APFloat(BitsToFloat(0x3FA2F983U) /* 1.273239e+00 */));
      const_array_22_elems.push_back(const_float_23);
      const_array_22_elems.push_back(const_float_23);
      const_array_22_elems.push_back(const_float_23);
      const_array_22_elems.push_back(const_float_23);
      Constant* const_array_22 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_22_elems));
      std::vector<Constant*> const_array_24_elems;
      ConstantInt* const_int32_25 = ConstantInt::get(context, APInt(32,  "1", 10));
      const_array_24_elems.push_back(const_int32_25);
      const_array_24_elems.push_back(const_int32_25);
      const_array_24_elems.push_back(const_int32_25);
      const_array_24_elems.push_back(const_int32_25);
      Constant* const_array_24 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_24_elems));
      std::vector<Constant*> const_array_26_elems;
      ConstantInt* const_int32_27 = ConstantInt::get(context, APInt(32,  "-2", 10));
      const_array_26_elems.push_back(const_int32_27);
      const_array_26_elems.push_back(const_int32_27);
      const_array_26_elems.push_back(const_int32_27);
      const_array_26_elems.push_back(const_int32_27);
      Constant* const_array_26 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_26_elems));
      std::vector<Constant*> const_array_28_elems;
      ConstantInt* const_int32_29 = ConstantInt::get(context, APInt(32,  "4", 10));
      const_array_28_elems.push_back(const_int32_29);
      const_array_28_elems.push_back(const_int32_29);
      const_array_28_elems.push_back(const_int32_29);
      const_array_28_elems.push_back(const_int32_29);
      Constant* const_array_28 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_28_elems));
      std::vector<Constant*> const_array_30_elems;
      ConstantInt* const_int32_31 = ConstantInt::get(context, APInt(32,  "2", 10));
      const_array_30_elems.push_back(const_int32_31);
      const_array_30_elems.push_back(const_int32_31);
      const_array_30_elems.push_back(const_int32_31);
      const_array_30_elems.push_back(const_int32_31);
      Constant* const_array_30 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_30_elems));
      std::vector<Constant*> const_array_32_elems;
      ConstantFP* const_float_33 = ConstantFP::get(context, APFloat(BitsToFloat(0xBF490000U) /* -7.851562e-01 */));
      const_array_32_elems.push_back(const_float_33);
      const_array_32_elems.push_back(const_float_33);
      const_array_32_elems.push_back(const_float_33);
      const_array_32_elems.push_back(const_float_33);
      Constant* const_array_32 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_32_elems));
      std::vector<Constant*> const_array_34_elems;
      ConstantFP* const_float_35 = ConstantFP::get(context, APFloat(BitsToFloat(0xB97DA000U) /* -2.418756e-04 */));
      const_array_34_elems.push_back(const_float_35);
      const_array_34_elems.push_back(const_float_35);
      const_array_34_elems.push_back(const_float_35);
      const_array_34_elems.push_back(const_float_35);
      Constant* const_array_34 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_34_elems));
      std::vector<Constant*> const_array_36_elems;
      ConstantFP* const_float_37 = ConstantFP::get(context, APFloat(-3.774895e-08f));
      const_array_36_elems.push_back(const_float_37);
      const_array_36_elems.push_back(const_float_37);
      const_array_36_elems.push_back(const_float_37);
      const_array_36_elems.push_back(const_float_37);
      Constant* const_array_36 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_36_elems));
      std::vector<Constant*> const_array_38_elems;
      ConstantFP* const_float_39 = ConstantFP::get(context, APFloat(BitsToFloat(0x37CCF5CEU) /* 2.443316e-05 */));
      const_array_38_elems.push_back(const_float_39);
      const_array_38_elems.push_back(const_float_39);
      const_array_38_elems.push_back(const_float_39);
      const_array_38_elems.push_back(const_float_39);
      Constant* const_array_38 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_38_elems));
      std::vector<Constant*> const_array_40_elems;
      ConstantFP* const_float_41 = ConstantFP::get(context, APFloat(BitsToFloat(0xBAB6061AU) /* -1.388732e-03 */));
      const_array_40_elems.push_back(const_float_41);
      const_array_40_elems.push_back(const_float_41);
      const_array_40_elems.push_back(const_float_41);
      const_array_40_elems.push_back(const_float_41);
      Constant* const_array_40 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_40_elems));
      std::vector<Constant*> const_array_42_elems;
      ConstantFP* const_float_43 = ConstantFP::get(context, APFloat(BitsToFloat(0x3D2AAAA5U) /* 4.166665e-02 */));
      const_array_42_elems.push_back(const_float_43);
      const_array_42_elems.push_back(const_float_43);
      const_array_42_elems.push_back(const_float_43);
      const_array_42_elems.push_back(const_float_43);
      Constant* const_array_42 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_42_elems));
      std::vector<Constant*> const_array_44_elems;
      ConstantFP* const_float_45 = ConstantFP::get(context, APFloat(5.000000e-01f));
      const_array_44_elems.push_back(const_float_45);
      const_array_44_elems.push_back(const_float_45);
      const_array_44_elems.push_back(const_float_45);
      const_array_44_elems.push_back(const_float_45);
      Constant* const_array_44 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_44_elems));
      std::vector<Constant*> const_array_46_elems;
      ConstantFP* const_float_47 = ConstantFP::get(context, APFloat(1.000000e+00f));
      const_array_46_elems.push_back(const_float_47);
      const_array_46_elems.push_back(const_float_47);
      const_array_46_elems.push_back(const_float_47);
      const_array_46_elems.push_back(const_float_47);
      Constant* const_array_46 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_46_elems));
      std::vector<Constant*> const_array_48_elems;
      ConstantFP* const_float_49 = ConstantFP::get(context, APFloat(BitsToFloat(0xB94CA1F9U) /* -1.951530e-04 */));
      const_array_48_elems.push_back(const_float_49);
      const_array_48_elems.push_back(const_float_49);
      const_array_48_elems.push_back(const_float_49);
      const_array_48_elems.push_back(const_float_49);
      Constant* const_array_48 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_48_elems));
      std::vector<Constant*> const_array_50_elems;
      ConstantFP* const_float_51 = ConstantFP::get(context, APFloat(8.332161e-03f));
      const_array_50_elems.push_back(const_float_51);
      const_array_50_elems.push_back(const_float_51);
      const_array_50_elems.push_back(const_float_51);
      const_array_50_elems.push_back(const_float_51);
      Constant* const_array_50 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_50_elems));
      std::vector<Constant*> const_array_52_elems;
      ConstantFP* const_float_53 = ConstantFP::get(context, APFloat(BitsToFloat(0xBE2AAAA3U) /* -1.666666e-01 */));
      const_array_52_elems.push_back(const_float_53);
      const_array_52_elems.push_back(const_float_53);
      const_array_52_elems.push_back(const_float_53);
      const_array_52_elems.push_back(const_float_53);
      Constant* const_array_52 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_52_elems));
      Constant* const_ptr_54 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_inv_sign_mask, PointerTy_6);
      Constant* const_ptr_55 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_sign_mask, PointerTy_6);
      Constant* const_ptr_56 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL15_ps_cephes_FOPI, PointerTy_6);
      Constant* const_ptr_57 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL7_pi32_1, PointerTy_11);
      Constant* const_ptr_58 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL10_pi32_inv1, PointerTy_11);
      Constant* const_ptr_59 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL7_pi32_4, PointerTy_11);
      ConstantInt* const_int32_60 = ConstantInt::get(context, APInt(32,  "29", 10));
      Constant* const_ptr_61 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL7_pi32_2, PointerTy_11);
      ConstantAggregateZero* const_packed_62 = ConstantAggregateZero::get(vectorTy_int_SIMD); //llvm 2.5
      Constant* const_ptr_63 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL20_ps_minus_cephes_DP1, PointerTy_6);
      Constant* const_ptr_64 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL20_ps_minus_cephes_DP2, PointerTy_6);
      Constant* const_ptr_65 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL20_ps_minus_cephes_DP3, PointerTy_6);
      std::vector<Constant*> const_packed_66_elems;
      ConstantInt* const_int64_67 = ConstantInt::get(context, APInt(64,  "-1", 10));
      const_packed_66_elems.push_back(const_int64_67);
      const_packed_66_elems.push_back(const_int64_67);
      Constant* const_packed_66 = ConstantVector::get(ArrayRef<Constant*>(const_packed_66_elems));
      Constant* const_ptr_68 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_coscof_p0, PointerTy_6);
      Constant* const_ptr_69 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_coscof_p1, PointerTy_6);
      Constant* const_ptr_70 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_coscof_p2, PointerTy_6);
      Constant* const_ptr_71 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL7_ps_0p5, PointerTy_6);
      Constant* const_ptr_72 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL5_ps_1, PointerTy_6);
      Constant* const_ptr_73 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_sincof_p0, PointerTy_6);
      Constant* const_ptr_74 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_sincof_p1, PointerTy_6);
      Constant* const_ptr_75 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL13_ps_sincof_p2, PointerTy_6);
      std::vector<Constant*> const_packed_76_elems;
      ConstantInt* const_int32_77 = ConstantInt::get(context, APInt(32,  "-1", 10));
      const_packed_76_elems.push_back(const_int32_77);
      const_packed_76_elems.push_back(const_int32_77);
      const_packed_76_elems.push_back(const_int32_77);
      const_packed_76_elems.push_back(const_int32_77);
      Constant* const_packed_76 = ConstantVector::get(ArrayRef<Constant*>(const_packed_76_elems));

      // Global Variable Definitions
      gvar_array__ZL17_ps_inv_sign_mask->setInitializer(const_array_18);
      gvar_array__ZL13_ps_sign_mask->setInitializer(const_array_20);
      gvar_array__ZL15_ps_cephes_FOPI->setInitializer(const_array_22);
      gvar_array__ZL7_pi32_1->setInitializer(const_array_24);
      gvar_array__ZL10_pi32_inv1->setInitializer(const_array_26);
      gvar_array__ZL7_pi32_4->setInitializer(const_array_28);
      gvar_array__ZL7_pi32_2->setInitializer(const_array_30);
      gvar_array__ZL20_ps_minus_cephes_DP1->setInitializer(const_array_32);
      gvar_array__ZL20_ps_minus_cephes_DP2->setInitializer(const_array_34);
      gvar_array__ZL20_ps_minus_cephes_DP3->setInitializer(const_array_36);
      gvar_array__ZL13_ps_coscof_p0->setInitializer(const_array_38);
      gvar_array__ZL13_ps_coscof_p1->setInitializer(const_array_40);
      gvar_array__ZL13_ps_coscof_p2->setInitializer(const_array_42);
      gvar_array__ZL7_ps_0p5->setInitializer(const_array_44);
      gvar_array__ZL5_ps_1->setInitializer(const_array_46);
      gvar_array__ZL13_ps_sincof_p0->setInitializer(const_array_48);
      gvar_array__ZL13_ps_sincof_p1->setInitializer(const_array_50);
      gvar_array__ZL13_ps_sincof_p2->setInitializer(const_array_52);

      // Function Definitions

      // Function: sincos_ps (sincos_ps)
      {
        Function::arg_iterator args = sincos_ps->arg_begin();
        Value* packed_x = args++;
        packed_x->setName("x");
        Value* ptr_s = args++;
        ptr_s->setName("s");
        Value* ptr_c = args++;
        ptr_c->setName("c");

        BasicBlock* label_entry = BasicBlock::Create(context, "entry",sincos_ps,0);

        // Block entry (label_entry)
        LoadInst* packed_78 = new LoadInst(const_ptr_54, "", false, label_entry);
        CastInst* packed_tmp_i56 = new BitCastInst(packed_x, vectorTy_int_SIMD, "tmp.i56", label_entry);
        CastInst* packed_tmp1_i57 = new BitCastInst(packed_78, vectorTy_int_SIMD, "tmp1.i57", label_entry);
        BinaryOperator* packed_tmp2_i58 = BinaryOperator::Create(Instruction::And, packed_tmp_i56, packed_tmp1_i57, "tmp2.i58", label_entry);
        CastInst* packed_tmp3_i59 = new BitCastInst(packed_tmp2_i58, vectorTy_float_SIMD, "tmp3.i59", label_entry);
        LoadInst* packed_79 = new LoadInst(const_ptr_55, "", false, label_entry);
        CastInst* packed_tmp1_i53 = new BitCastInst(packed_79, vectorTy_int_SIMD, "tmp1.i53", label_entry);
        BinaryOperator* packed_tmp2_i54 = BinaryOperator::Create(Instruction::And, packed_tmp_i56, packed_tmp1_i53, "tmp2.i54", label_entry);
        LoadInst* packed_80 = new LoadInst(const_ptr_56, "", false, label_entry);
        BinaryOperator* packed_tmp_i51 = BinaryOperator::Create(Instruction::FMul, packed_tmp3_i59, packed_80, "tmp.i51", label_entry);
        CallInst* packed_81 = CallInst::Create(generateCvttps2dq(mod), ArrayRef<Value*>(packed_tmp_i51), "", label_entry);
        packed_81->setCallingConv(CallingConv::C);
        packed_81->setTailCall(true);AttrListPtr packed_81_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_81_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_81->setAttributes(packed_81_PAL);

        LoadInst* packed_82 = new LoadInst(const_ptr_57, "", false, label_entry);
        CastInst* packed_83 = new BitCastInst(packed_82, vectorTy_int_SIMD, "", label_entry);
        BinaryOperator* packed_tmp_i50 = BinaryOperator::Create(Instruction::Add, packed_81, packed_83, "tmp.i50", label_entry);
        CastInst* packed_84 = new BitCastInst(packed_tmp_i50, VectorTy_10, "", label_entry);
        LoadInst* packed_85 = new LoadInst(const_ptr_58, "", false, label_entry);
        BinaryOperator* packed_tmp_i49 = BinaryOperator::Create(Instruction::And, packed_84, packed_85, "tmp.i49", label_entry);
        CastInst* packed_86 = new BitCastInst(packed_tmp_i49, vectorTy_int_SIMD, "", label_entry);
        CallInst* packed_87 = CallInst::Create(generateCvtdq2ps(mod), ArrayRef<Value*>(packed_86), "", label_entry);
        packed_87->setCallingConv(CallingConv::C);
        packed_87->setTailCall(true);AttrListPtr packed_87_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_87_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_87->setAttributes(packed_87_PAL);

        LoadInst* packed_88 = new LoadInst(const_ptr_59, "", false, label_entry);
        BinaryOperator* packed_tmp_i48 = BinaryOperator::Create(Instruction::And, packed_tmp_i49, packed_88, "tmp.i48", label_entry);
        CastInst* packed_89 = new BitCastInst(packed_tmp_i48, vectorTy_int_SIMD, "", label_entry);
        std::vector<Value*> packed_90_params;
        packed_90_params.push_back(packed_89);
        packed_90_params.push_back(const_int32_60);
        CallInst* packed_90 = CallInst::Create(generatePsllid(mod), ArrayRef<Value*>(packed_90_params), "", label_entry);
        packed_90->setCallingConv(CallingConv::C);
        packed_90->setTailCall(true);AttrListPtr packed_90_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_90_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_90->setAttributes(packed_90_PAL);

        LoadInst* packed_91 = new LoadInst(const_ptr_61, "", false, label_entry);
        BinaryOperator* packed_tmp_i47 = BinaryOperator::Create(Instruction::And, packed_tmp_i49, packed_91, "tmp.i47", label_entry);
        CastInst* packed_92 = new BitCastInst(packed_tmp_i47, vectorTy_int_SIMD, "", label_entry);
        std::vector<Value*> packed_93_params;
        packed_93_params.push_back(packed_92);
        packed_93_params.push_back(const_packed_62);
        CallInst* packed_93 = CallInst::Create(generatePcmpeq(mod), ArrayRef<Value*>(packed_93_params), "", label_entry);
        packed_93->setCallingConv(CallingConv::C);
        packed_93->setTailCall(true);AttrListPtr packed_93_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_93_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_93->setAttributes(packed_93_PAL);

        LoadInst* packed_94 = new LoadInst(const_ptr_63, "", false, label_entry);
        LoadInst* packed_95 = new LoadInst(const_ptr_64, "", false, label_entry);
        LoadInst* packed_96 = new LoadInst(const_ptr_65, "", false, label_entry);
        BinaryOperator* packed_tmp_i46 = BinaryOperator::Create(Instruction::FMul, packed_87, packed_94, "tmp.i46", label_entry);
        BinaryOperator* packed_tmp_i45 = BinaryOperator::Create(Instruction::FMul, packed_87, packed_95, "tmp.i45", label_entry);
        BinaryOperator* packed_tmp_i44 = BinaryOperator::Create(Instruction::FMul, packed_87, packed_96, "tmp.i44", label_entry);
        BinaryOperator* packed_tmp_i43 = BinaryOperator::Create(Instruction::FAdd, packed_tmp3_i59, packed_tmp_i46, "tmp.i43", label_entry);
        BinaryOperator* packed_tmp_i42 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i43, packed_tmp_i45, "tmp.i42", label_entry);
        BinaryOperator* packed_tmp_i41 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i42, packed_tmp_i44, "tmp.i41", label_entry);
        CastInst* packed_97 = new BitCastInst(packed_91, vectorTy_int_SIMD, "", label_entry);
        BinaryOperator* packed_tmp_i40 = BinaryOperator::Create(Instruction::Sub, packed_86, packed_97, "tmp.i40", label_entry);
        CastInst* packed_98 = new BitCastInst(packed_tmp_i40, VectorTy_10, "", label_entry);
        BinaryOperator* packed_tmp_i38 = BinaryOperator::Create(Instruction::Xor, packed_98, const_packed_66, "tmp.i38", label_entry);
        BinaryOperator* packed_tmp1_i39 = BinaryOperator::Create(Instruction::And, packed_88, packed_tmp_i38, "tmp1.i39", label_entry);
        CastInst* packed_99 = new BitCastInst(packed_tmp1_i39, vectorTy_int_SIMD, "", label_entry);
        std::vector<Value*> packed_100_params;
        packed_100_params.push_back(packed_99);
        packed_100_params.push_back(const_int32_60);
        CallInst* packed_100 = CallInst::Create(generatePsllid(mod), ArrayRef<Value*>(packed_100_params), "", label_entry);
        packed_100->setCallingConv(CallingConv::C);
        packed_100->setTailCall(true);AttrListPtr packed_100_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_100_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_100->setAttributes(packed_100_PAL);

        BinaryOperator* packed_tmp2_i36 = BinaryOperator::Create(Instruction::Xor, packed_tmp2_i54, packed_90, "tmp2.i36", label_entry);
        BinaryOperator* packed_tmp_i33 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i41, packed_tmp_i41, "tmp.i33", label_entry);
        LoadInst* packed_101 = new LoadInst(const_ptr_68, "", false, label_entry);
        BinaryOperator* packed_tmp_i32 = BinaryOperator::Create(Instruction::FMul, packed_101, packed_tmp_i33, "tmp.i32", label_entry);
        LoadInst* packed_102 = new LoadInst(const_ptr_69, "", false, label_entry);
        BinaryOperator* packed_tmp_i31 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i32, packed_102, "tmp.i31", label_entry);
        BinaryOperator* packed_tmp_i30 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i31, packed_tmp_i33, "tmp.i30", label_entry);
        LoadInst* packed_103 = new LoadInst(const_ptr_70, "", false, label_entry);
        BinaryOperator* packed_tmp_i29 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i30, packed_103, "tmp.i29", label_entry);
        BinaryOperator* packed_tmp_i28 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i29, packed_tmp_i33, "tmp.i28", label_entry);
        BinaryOperator* packed_tmp_i27 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i28, packed_tmp_i33, "tmp.i27", label_entry);
        LoadInst* packed_104 = new LoadInst(const_ptr_71, "", false, label_entry);
        BinaryOperator* packed_tmp_i26 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i33, packed_104, "tmp.i26", label_entry);
        BinaryOperator* packed_tmp_i25 = BinaryOperator::Create(Instruction::FSub, packed_tmp_i27, packed_tmp_i26, "tmp.i25", label_entry);
        LoadInst* packed_105 = new LoadInst(const_ptr_72, "", false, label_entry);
        BinaryOperator* packed_tmp_i24 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i25, packed_105, "tmp.i24", label_entry);
        LoadInst* packed_106 = new LoadInst(const_ptr_73, "", false, label_entry);
        BinaryOperator* packed_tmp_i23 = BinaryOperator::Create(Instruction::FMul, packed_106, packed_tmp_i33, "tmp.i23", label_entry);
        LoadInst* packed_107 = new LoadInst(const_ptr_74, "", false, label_entry);
        BinaryOperator* packed_tmp_i22 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i23, packed_107, "tmp.i22", label_entry);
        BinaryOperator* packed_tmp_i21 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i22, packed_tmp_i33, "tmp.i21", label_entry);
        LoadInst* packed_108 = new LoadInst(const_ptr_75, "", false, label_entry);
        BinaryOperator* packed_tmp_i20 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i21, packed_108, "tmp.i20", label_entry);
        BinaryOperator* packed_tmp_i19 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i20, packed_tmp_i33, "tmp.i19", label_entry);
        BinaryOperator* packed_tmp_i18 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i19, packed_tmp_i41, "tmp.i18", label_entry);
        BinaryOperator* packed_tmp_i17 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i18, packed_tmp_i41, "tmp.i17", label_entry);
        CastInst* packed_tmp1_i14 = new BitCastInst(packed_tmp_i17, vectorTy_int_SIMD, "tmp1.i14", label_entry);
        BinaryOperator* packed_tmp2_i15 = BinaryOperator::Create(Instruction::And, packed_93, packed_tmp1_i14, "tmp2.i15", label_entry);
        CastInst* packed_tmp3_i16 = new BitCastInst(packed_tmp2_i15, vectorTy_float_SIMD, "tmp3.i16", label_entry);
        CastInst* packed_tmp1_i10 = new BitCastInst(packed_tmp_i24, vectorTy_int_SIMD, "tmp1.i10", label_entry);
        BinaryOperator* packed_tmp2_i11 = BinaryOperator::Create(Instruction::Xor, packed_93, const_packed_76, "tmp2.i11", label_entry);
        BinaryOperator* packed_tmp3_i12 = BinaryOperator::Create(Instruction::And, packed_tmp1_i10, packed_tmp2_i11, "tmp3.i12", label_entry);
        CastInst* packed_tmp4_i = new BitCastInst(packed_tmp3_i12, vectorTy_float_SIMD, "tmp4.i", label_entry);
        BinaryOperator* packed_tmp_i8 = BinaryOperator::Create(Instruction::FSub, packed_tmp_i17, packed_tmp3_i16, "tmp.i8", label_entry);
        BinaryOperator* packed_tmp_i7 = BinaryOperator::Create(Instruction::FSub, packed_tmp_i24, packed_tmp4_i, "tmp.i7", label_entry);
        BinaryOperator* packed_tmp_i6 = BinaryOperator::Create(Instruction::FAdd, packed_tmp4_i, packed_tmp3_i16, "tmp.i6", label_entry);
        BinaryOperator* packed_tmp_i5 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i7, packed_tmp_i8, "tmp.i5", label_entry);
        CastInst* packed_tmp_i1 = new BitCastInst(packed_tmp_i6, vectorTy_int_SIMD, "tmp.i1", label_entry);
        BinaryOperator* packed_tmp2_i3 = BinaryOperator::Create(Instruction::Xor, packed_tmp_i1, packed_tmp2_i36, "tmp2.i3", label_entry);
        CastInst* packed_tmp3_i4 = new BitCastInst(packed_tmp2_i3, vectorTy_float_SIMD, "tmp3.i4", label_entry);
         new StoreInst(packed_tmp3_i4, ptr_s, false, label_entry);
        CastInst* packed_tmp_i = new BitCastInst(packed_tmp_i5, vectorTy_int_SIMD, "tmp.i", label_entry);
        BinaryOperator* packed_tmp2_i = BinaryOperator::Create(Instruction::Xor, packed_tmp_i, packed_100, "tmp2.i", label_entry);
        CastInst* packed_tmp3_i = new BitCastInst(packed_tmp2_i, vectorTy_float_SIMD, "tmp3.i", label_entry);
         new StoreInst(packed_tmp3_i, ptr_c, false, label_entry);
        ReturnInst::Create(context, label_entry);

      }

      return sincos_ps;
    }
    Function* generateLogPS(Module& mod, const unsigned simdWidth) const {
        if (Function* tmpF = mod.getFunction("log_ps")) {
            return tmpF;
        }

        assert (!(mod.getFunction("_ZL5_ps_1") ||
                mod.getFunction("_ZL16_ps_min_norm_pos") ||
                mod.getFunction("_ZL17_ps_inv_mant_mask") ||
                mod.getFunction("_ZL7_ps_0p5") ||
                mod.getFunction("_ZL10_pi32_0x7f") ||
                mod.getFunction("_ZL17_ps_cephes_SQRTHF") ||
                mod.getFunction("_ZL17_ps_cephes_log_p0") ||
                mod.getFunction("_ZL17_ps_cephes_log_p1") ||
                mod.getFunction("_ZL17_ps_cephes_log_p2") ||
                mod.getFunction("_ZL17_ps_cephes_log_p3") ||
                mod.getFunction("_ZL17_ps_cephes_log_p4") ||
                mod.getFunction("_ZL17_ps_cephes_log_p5") ||
                mod.getFunction("_ZL17_ps_cephes_log_p6") ||
                mod.getFunction("_ZL17_ps_cephes_log_p7") ||
                mod.getFunction("_ZL17_ps_cephes_log_p8") ||
                mod.getFunction("_ZL17_ps_cephes_log_q1") ||
                mod.getFunction("_ZL17_ps_cephes_log_q2")) && "function to be generated is already declared in module!");

		LLVMContext& context = mod.getContext();

		VectorType* vectorTy_float_SIMD = VectorType::get(Type::getFloatTy(context), simdWidth);
		VectorType* vectorTy_int_SIMD = VectorType::get(Type::getInt32Ty(context), simdWidth);

      // Type Definitions
      ArrayType* ArrayTy_0 = ArrayType::get(Type::getFloatTy(context), 4);
      ArrayType* ArrayTy_2 = ArrayType::get(IntegerType::get(context, 32), 4);

      std::vector<Type*>FuncTy_5_args;
      FuncTy_5_args.push_back(vectorTy_float_SIMD);
      FunctionType* FuncTy_5 = FunctionType::get(
        /*Result=*/vectorTy_float_SIMD,
        /*Params=*/FuncTy_5_args,
        /*isVarArg=*/false);

      PointerType* PointerTy_6 = PointerType::get(vectorTy_float_SIMD, 0);

      VectorType* VectorTy_14 = VectorType::get(IntegerType::get(context, 64), 2);

      PointerType* PointerTy_15 = PointerType::get(VectorTy_14, 0);

      // Function Declarations

      Function* log_ps = Function::Create(
        /*Type=*/FuncTy_5,
        /*Linkage=*/GlobalValue::ExternalLinkage,
        /*Name=*/"log_ps", &mod);
      log_ps->setCallingConv(CallingConv::C);
      AttrListPtr log_ps_PAL;
      {
        SmallVector<AttributeWithIndex, 4> Attrs;
        AttributeWithIndex PAWI;
        PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
        Attrs.push_back(PAWI);
        log_ps_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

      }
      log_ps->setAttributes(log_ps_PAL);


      // Global Variable Declarations


      GlobalVariable* gvar_array__ZL5_ps_1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL5_ps_1");
      gvar_array__ZL5_ps_1->setAlignment(16);

      GlobalVariable* gvar_array__ZL16_ps_min_norm_pos = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL16_ps_min_norm_pos");
      gvar_array__ZL16_ps_min_norm_pos->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_inv_mant_mask = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_inv_mant_mask");
      gvar_array__ZL17_ps_inv_mant_mask->setAlignment(16);

      GlobalVariable* gvar_array__ZL7_ps_0p5 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL7_ps_0p5");
      gvar_array__ZL7_ps_0p5->setAlignment(16);

      GlobalVariable* gvar_array__ZL10_pi32_0x7f = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL10_pi32_0x7f");
      gvar_array__ZL10_pi32_0x7f->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_SQRTHF = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_SQRTHF");
      gvar_array__ZL17_ps_cephes_SQRTHF->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_log_p0 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_log_p0");
      gvar_array__ZL17_ps_cephes_log_p0->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_log_p1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_log_p1");
      gvar_array__ZL17_ps_cephes_log_p1->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_log_p2 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_log_p2");
      gvar_array__ZL17_ps_cephes_log_p2->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_log_p3 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_log_p3");
      gvar_array__ZL17_ps_cephes_log_p3->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_log_p4 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_log_p4");
      gvar_array__ZL17_ps_cephes_log_p4->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_log_p5 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_log_p5");
      gvar_array__ZL17_ps_cephes_log_p5->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_log_p6 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_log_p6");
      gvar_array__ZL17_ps_cephes_log_p6->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_log_p7 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_log_p7");
      gvar_array__ZL17_ps_cephes_log_p7->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_log_p8 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_log_p8");
      gvar_array__ZL17_ps_cephes_log_p8->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_log_q1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_log_q1");
      gvar_array__ZL17_ps_cephes_log_q1->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_log_q2 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_log_q2");
      gvar_array__ZL17_ps_cephes_log_q2->setAlignment(16);

      // Constant Definitions
      std::vector<Constant*> const_array_18_elems;
      ConstantFP* const_float_19 = ConstantFP::get(context, APFloat(1.000000e+00f));
      const_array_18_elems.push_back(const_float_19);
      const_array_18_elems.push_back(const_float_19);
      const_array_18_elems.push_back(const_float_19);
      const_array_18_elems.push_back(const_float_19);
      Constant* const_array_18 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_18_elems));
      std::vector<Constant*> const_array_20_elems;
      ConstantInt* const_int32_21 = ConstantInt::get(context, APInt(32,  "8388608", 10));
      const_array_20_elems.push_back(const_int32_21);
      const_array_20_elems.push_back(const_int32_21);
      const_array_20_elems.push_back(const_int32_21);
      const_array_20_elems.push_back(const_int32_21);
      Constant* const_array_20 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_20_elems));
      std::vector<Constant*> const_array_22_elems;
      ConstantInt* const_int32_23 = ConstantInt::get(context, APInt(32,  "-2139095041", 10));
      const_array_22_elems.push_back(const_int32_23);
      const_array_22_elems.push_back(const_int32_23);
      const_array_22_elems.push_back(const_int32_23);
      const_array_22_elems.push_back(const_int32_23);
      Constant* const_array_22 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_22_elems));
      std::vector<Constant*> const_array_24_elems;
      ConstantFP* const_float_25 = ConstantFP::get(context, APFloat(5.000000e-01f));
      const_array_24_elems.push_back(const_float_25);
      const_array_24_elems.push_back(const_float_25);
      const_array_24_elems.push_back(const_float_25);
      const_array_24_elems.push_back(const_float_25);
      Constant* const_array_24 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_24_elems));
      std::vector<Constant*> const_array_26_elems;
      ConstantInt* const_int32_27 = ConstantInt::get(context, APInt(32,  "127", 10));
      const_array_26_elems.push_back(const_int32_27);
      const_array_26_elems.push_back(const_int32_27);
      const_array_26_elems.push_back(const_int32_27);
      const_array_26_elems.push_back(const_int32_27);
      Constant* const_array_26 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_26_elems));
      std::vector<Constant*> const_array_28_elems;
      ConstantFP* const_float_29 = ConstantFP::get(context, APFloat(BitsToFloat(0x3F3504F3U) /* 7.071068e-01 */));
      const_array_28_elems.push_back(const_float_29);
      const_array_28_elems.push_back(const_float_29);
      const_array_28_elems.push_back(const_float_29);
      const_array_28_elems.push_back(const_float_29);
      Constant* const_array_28 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_28_elems));
      std::vector<Constant*> const_array_30_elems;
      ConstantFP* const_float_31 = ConstantFP::get(context, APFloat(BitsToFloat(0x3D9021BBU) /* 7.037684e-02 */));
      const_array_30_elems.push_back(const_float_31);
      const_array_30_elems.push_back(const_float_31);
      const_array_30_elems.push_back(const_float_31);
      const_array_30_elems.push_back(const_float_31);
      Constant* const_array_30 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_30_elems));
      std::vector<Constant*> const_array_32_elems;
      ConstantFP* const_float_33 = ConstantFP::get(context, APFloat(-1.151461e-01f));
      const_array_32_elems.push_back(const_float_33);
      const_array_32_elems.push_back(const_float_33);
      const_array_32_elems.push_back(const_float_33);
      const_array_32_elems.push_back(const_float_33);
      Constant* const_array_32 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_32_elems));
      std::vector<Constant*> const_array_34_elems;
      ConstantFP* const_float_35 = ConstantFP::get(context, APFloat(BitsToFloat(0x3DEF251AU) /* 1.167700e-01 */));
      const_array_34_elems.push_back(const_float_35);
      const_array_34_elems.push_back(const_float_35);
      const_array_34_elems.push_back(const_float_35);
      const_array_34_elems.push_back(const_float_35);
      Constant* const_array_34 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_34_elems));
      std::vector<Constant*> const_array_36_elems;
      ConstantFP* const_float_37 = ConstantFP::get(context, APFloat(BitsToFloat(0xBDFE5D4FU) /* -1.242014e-01 */));
      const_array_36_elems.push_back(const_float_37);
      const_array_36_elems.push_back(const_float_37);
      const_array_36_elems.push_back(const_float_37);
      const_array_36_elems.push_back(const_float_37);
      Constant* const_array_36 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_36_elems));
      std::vector<Constant*> const_array_38_elems;
      ConstantFP* const_float_39 = ConstantFP::get(context, APFloat(BitsToFloat(0x3E11E9BFU) /* 1.424932e-01 */));
      const_array_38_elems.push_back(const_float_39);
      const_array_38_elems.push_back(const_float_39);
      const_array_38_elems.push_back(const_float_39);
      const_array_38_elems.push_back(const_float_39);
      Constant* const_array_38 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_38_elems));
      std::vector<Constant*> const_array_40_elems;
      ConstantFP* const_float_41 = ConstantFP::get(context, APFloat(BitsToFloat(0xBE2AAE50U) /* -1.666806e-01 */));
      const_array_40_elems.push_back(const_float_41);
      const_array_40_elems.push_back(const_float_41);
      const_array_40_elems.push_back(const_float_41);
      const_array_40_elems.push_back(const_float_41);
      Constant* const_array_40 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_40_elems));
      std::vector<Constant*> const_array_42_elems;
      ConstantFP* const_float_43 = ConstantFP::get(context, APFloat(BitsToFloat(0x3E4CCEACU) /* 2.000071e-01 */));
      const_array_42_elems.push_back(const_float_43);
      const_array_42_elems.push_back(const_float_43);
      const_array_42_elems.push_back(const_float_43);
      const_array_42_elems.push_back(const_float_43);
      Constant* const_array_42 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_42_elems));
      std::vector<Constant*> const_array_44_elems;
      ConstantFP* const_float_45 = ConstantFP::get(context, APFloat(BitsToFloat(0xBE7FFFFCU) /* -2.499999e-01 */));
      const_array_44_elems.push_back(const_float_45);
      const_array_44_elems.push_back(const_float_45);
      const_array_44_elems.push_back(const_float_45);
      const_array_44_elems.push_back(const_float_45);
      Constant* const_array_44 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_44_elems));
      std::vector<Constant*> const_array_46_elems;
      ConstantFP* const_float_47 = ConstantFP::get(context, APFloat(3.333333e-01f));
      const_array_46_elems.push_back(const_float_47);
      const_array_46_elems.push_back(const_float_47);
      const_array_46_elems.push_back(const_float_47);
      const_array_46_elems.push_back(const_float_47);
      Constant* const_array_46 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_46_elems));
      std::vector<Constant*> const_array_48_elems;
      ConstantFP* const_float_49 = ConstantFP::get(context, APFloat(BitsToFloat(0xB95E8083U) /* -2.121944e-04 */));
      const_array_48_elems.push_back(const_float_49);
      const_array_48_elems.push_back(const_float_49);
      const_array_48_elems.push_back(const_float_49);
      const_array_48_elems.push_back(const_float_49);
      Constant* const_array_48 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_48_elems));
      std::vector<Constant*> const_array_50_elems;
      ConstantFP* const_float_51 = ConstantFP::get(context, APFloat(6.933594e-01f));
      const_array_50_elems.push_back(const_float_51);
      const_array_50_elems.push_back(const_float_51);
      const_array_50_elems.push_back(const_float_51);
      const_array_50_elems.push_back(const_float_51);
      Constant* const_array_50 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_50_elems));
      Constant* const_ptr_52 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL5_ps_1, PointerTy_6);
      ConstantAggregateZero* const_packed_53 = ConstantAggregateZero::get(vectorTy_float_SIMD); //llvm 2.5
      ConstantInt* const_int8_54 = ConstantInt::get(context, APInt(8,  "2", 10));
      Constant* const_ptr_55 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL16_ps_min_norm_pos, PointerTy_6);
      ConstantInt* const_int32_56 = ConstantInt::get(context, APInt(32,  "23", 10));
      Constant* const_ptr_57 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_inv_mant_mask, PointerTy_6);
      Constant* const_ptr_58 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL7_ps_0p5, PointerTy_6);
      Constant* const_ptr_59 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL10_pi32_0x7f, PointerTy_15);
      Constant* const_ptr_60 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_SQRTHF, PointerTy_6);
      ConstantInt* const_int8_61 = ConstantInt::get(context, APInt(8,  "1", 10));
      Constant* const_ptr_62 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_log_p0, PointerTy_6);
      Constant* const_ptr_63 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_log_p1, PointerTy_6);
      Constant* const_ptr_64 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_log_p2, PointerTy_6);
      Constant* const_ptr_65 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_log_p3, PointerTy_6);
      Constant* const_ptr_66 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_log_p4, PointerTy_6);
      Constant* const_ptr_67 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_log_p5, PointerTy_6);
      Constant* const_ptr_68 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_log_p6, PointerTy_6);
      Constant* const_ptr_69 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_log_p7, PointerTy_6);
      Constant* const_ptr_70 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_log_p8, PointerTy_6);
      Constant* const_ptr_71 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_log_q1, PointerTy_6);
      Constant* const_ptr_72 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_log_q2, PointerTy_6);

      // Global Variable Definitions
      gvar_array__ZL5_ps_1->setInitializer(const_array_18);
      gvar_array__ZL16_ps_min_norm_pos->setInitializer(const_array_20);
      gvar_array__ZL17_ps_inv_mant_mask->setInitializer(const_array_22);
      gvar_array__ZL7_ps_0p5->setInitializer(const_array_24);
      gvar_array__ZL10_pi32_0x7f->setInitializer(const_array_26);
      gvar_array__ZL17_ps_cephes_SQRTHF->setInitializer(const_array_28);
      gvar_array__ZL17_ps_cephes_log_p0->setInitializer(const_array_30);
      gvar_array__ZL17_ps_cephes_log_p1->setInitializer(const_array_32);
      gvar_array__ZL17_ps_cephes_log_p2->setInitializer(const_array_34);
      gvar_array__ZL17_ps_cephes_log_p3->setInitializer(const_array_36);
      gvar_array__ZL17_ps_cephes_log_p4->setInitializer(const_array_38);
      gvar_array__ZL17_ps_cephes_log_p5->setInitializer(const_array_40);
      gvar_array__ZL17_ps_cephes_log_p6->setInitializer(const_array_42);
      gvar_array__ZL17_ps_cephes_log_p7->setInitializer(const_array_44);
      gvar_array__ZL17_ps_cephes_log_p8->setInitializer(const_array_46);
      gvar_array__ZL17_ps_cephes_log_q1->setInitializer(const_array_48);
      gvar_array__ZL17_ps_cephes_log_q2->setInitializer(const_array_50);

      // Function Definitions

      // Function: log_ps (log_ps)
      {
        Function::arg_iterator args = log_ps->arg_begin();
        Value* packed_x = args++;
        packed_x->setName("x");

        BasicBlock* label_entry = BasicBlock::Create(context, "entry",log_ps,0);

        // Block entry (label_entry)
        LoadInst* packed_73 = new LoadInst(const_ptr_52, "", false, label_entry);
        std::vector<Value*> packed_tmp_i50_params;
        packed_tmp_i50_params.push_back(packed_x);
        packed_tmp_i50_params.push_back(const_packed_53);
        packed_tmp_i50_params.push_back(const_int8_54);
        CallInst* packed_tmp_i50 = CallInst::Create(generateCmpPS(mod), ArrayRef<Value*>(packed_tmp_i50_params), "tmp.i50", label_entry);
        packed_tmp_i50->setCallingConv(CallingConv::C);
        packed_tmp_i50->setTailCall(true);AttrListPtr packed_tmp_i50_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind;
          Attrs.push_back(PAWI);
          packed_tmp_i50_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_tmp_i50->setAttributes(packed_tmp_i50_PAL);

        LoadInst* packed_74 = new LoadInst(const_ptr_55, "", false, label_entry);
        std::vector<Value*> packed_75_params;
        packed_75_params.push_back(packed_x);
        packed_75_params.push_back(packed_74);
        CallInst* packed_75 = CallInst::Create(generateMaxPS(mod), ArrayRef<Value*>(packed_75_params), "", label_entry);
        packed_75->setCallingConv(CallingConv::C);
        packed_75->setTailCall(true);AttrListPtr packed_75_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_75_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_75->setAttributes(packed_75_PAL);

        CastInst* packed_76 = new BitCastInst(packed_75, vectorTy_int_SIMD, "", label_entry);
        std::vector<Value*> packed_77_params;
        packed_77_params.push_back(packed_76);
        packed_77_params.push_back(const_int32_56);
        CallInst* packed_77 = CallInst::Create(generatePsrlid(mod), ArrayRef<Value*>(packed_77_params), "", label_entry);
        packed_77->setCallingConv(CallingConv::C);
        packed_77->setTailCall(true);AttrListPtr packed_77_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_77_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_77->setAttributes(packed_77_PAL);

        LoadInst* packed_78 = new LoadInst(const_ptr_57, "", false, label_entry);
        CastInst* packed_tmp1_i47 = new BitCastInst(packed_78, vectorTy_int_SIMD, "tmp1.i47", label_entry);
        BinaryOperator* packed_tmp2_i48 = BinaryOperator::Create(Instruction::And, packed_76, packed_tmp1_i47, "tmp2.i48", label_entry);
        LoadInst* packed_79 = new LoadInst(const_ptr_58, "", false, label_entry);
        CastInst* packed_tmp1_i43 = new BitCastInst(packed_79, vectorTy_int_SIMD, "tmp1.i43", label_entry);
        BinaryOperator* packed_tmp2_i44 = BinaryOperator::Create(Instruction::Or, packed_tmp2_i48, packed_tmp1_i43, "tmp2.i44", label_entry);
        CastInst* packed_tmp3_i45 = new BitCastInst(packed_tmp2_i44, vectorTy_float_SIMD, "tmp3.i45", label_entry);
        LoadInst* packed_80 = new LoadInst(const_ptr_59, "", false, label_entry);
        CastInst* packed_81 = new BitCastInst(packed_80, vectorTy_int_SIMD, "", label_entry);
        BinaryOperator* packed_tmp_i41 = BinaryOperator::Create(Instruction::Sub, packed_77, packed_81, "tmp.i41", label_entry);
        CallInst* packed_82 = CallInst::Create(generateCvtdq2ps(mod), ArrayRef<Value*>(packed_tmp_i41), "", label_entry);
        packed_82->setCallingConv(CallingConv::C);
        packed_82->setTailCall(true);AttrListPtr packed_82_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_82_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_82->setAttributes(packed_82_PAL);

        BinaryOperator* packed_tmp_i40 = BinaryOperator::Create(Instruction::FAdd, packed_82, packed_73, "tmp.i40", label_entry);
        LoadInst* packed_83 = new LoadInst(const_ptr_60, "", false, label_entry);
        std::vector<Value*> packed_tmp_i39_params;
        packed_tmp_i39_params.push_back(packed_tmp3_i45);
        packed_tmp_i39_params.push_back(packed_83);
        packed_tmp_i39_params.push_back(const_int8_61);
        CallInst* packed_tmp_i39 = CallInst::Create(generateCmpPS(mod), ArrayRef<Value*>(packed_tmp_i39_params), "tmp.i39", label_entry);
        packed_tmp_i39->setCallingConv(CallingConv::C);
        packed_tmp_i39->setTailCall(true);AttrListPtr packed_tmp_i39_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind;
          Attrs.push_back(PAWI);
          packed_tmp_i39_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_tmp_i39->setAttributes(packed_tmp_i39_PAL);

        CastInst* packed_tmp1_i36 = new BitCastInst(packed_tmp_i39, vectorTy_int_SIMD, "tmp1.i36", label_entry);
        BinaryOperator* packed_tmp2_i37 = BinaryOperator::Create(Instruction::And, packed_tmp2_i44, packed_tmp1_i36, "tmp2.i37", label_entry);
        CastInst* packed_tmp3_i38 = new BitCastInst(packed_tmp2_i37, vectorTy_float_SIMD, "tmp3.i38", label_entry);
        BinaryOperator* packed_tmp_i34 = BinaryOperator::Create(Instruction::FSub, packed_tmp3_i45, packed_73, "tmp.i34", label_entry);
        CastInst* packed_tmp_i30 = new BitCastInst(packed_73, vectorTy_int_SIMD, "tmp.i30", label_entry);
        BinaryOperator* packed_tmp2_i32 = BinaryOperator::Create(Instruction::And, packed_tmp_i30, packed_tmp1_i36, "tmp2.i32", label_entry);
        CastInst* packed_tmp3_i33 = new BitCastInst(packed_tmp2_i32, vectorTy_float_SIMD, "tmp3.i33", label_entry);
        BinaryOperator* packed_tmp_i29 = BinaryOperator::Create(Instruction::FSub, packed_tmp_i40, packed_tmp3_i33, "tmp.i29", label_entry);
        BinaryOperator* packed_tmp_i28 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i34, packed_tmp3_i38, "tmp.i28", label_entry);
        BinaryOperator* packed_tmp_i27 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i28, packed_tmp_i28, "tmp.i27", label_entry);
        LoadInst* packed_84 = new LoadInst(const_ptr_62, "", false, label_entry);
        BinaryOperator* packed_tmp_i26 = BinaryOperator::Create(Instruction::FMul, packed_84, packed_tmp_i28, "tmp.i26", label_entry);
        LoadInst* packed_85 = new LoadInst(const_ptr_63, "", false, label_entry);
        BinaryOperator* packed_tmp_i25 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i26, packed_85, "tmp.i25", label_entry);
        BinaryOperator* packed_tmp_i24 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i25, packed_tmp_i28, "tmp.i24", label_entry);
        LoadInst* packed_86 = new LoadInst(const_ptr_64, "", false, label_entry);
        BinaryOperator* packed_tmp_i23 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i24, packed_86, "tmp.i23", label_entry);
        BinaryOperator* packed_tmp_i22 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i23, packed_tmp_i28, "tmp.i22", label_entry);
        LoadInst* packed_87 = new LoadInst(const_ptr_65, "", false, label_entry);
        BinaryOperator* packed_tmp_i21 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i22, packed_87, "tmp.i21", label_entry);
        BinaryOperator* packed_tmp_i20 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i21, packed_tmp_i28, "tmp.i20", label_entry);
        LoadInst* packed_88 = new LoadInst(const_ptr_66, "", false, label_entry);
        BinaryOperator* packed_tmp_i19 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i20, packed_88, "tmp.i19", label_entry);
        BinaryOperator* packed_tmp_i18 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i19, packed_tmp_i28, "tmp.i18", label_entry);
        LoadInst* packed_89 = new LoadInst(const_ptr_67, "", false, label_entry);
        BinaryOperator* packed_tmp_i17 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i18, packed_89, "tmp.i17", label_entry);
        BinaryOperator* packed_tmp_i16 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i17, packed_tmp_i28, "tmp.i16", label_entry);
        LoadInst* packed_90 = new LoadInst(const_ptr_68, "", false, label_entry);
        BinaryOperator* packed_tmp_i15 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i16, packed_90, "tmp.i15", label_entry);
        BinaryOperator* packed_tmp_i14 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i15, packed_tmp_i28, "tmp.i14", label_entry);
        LoadInst* packed_91 = new LoadInst(const_ptr_69, "", false, label_entry);
        BinaryOperator* packed_tmp_i13 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i14, packed_91, "tmp.i13", label_entry);
        BinaryOperator* packed_tmp_i12 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i13, packed_tmp_i28, "tmp.i12", label_entry);
        LoadInst* packed_92 = new LoadInst(const_ptr_70, "", false, label_entry);
        BinaryOperator* packed_tmp_i11 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i12, packed_92, "tmp.i11", label_entry);
        BinaryOperator* packed_tmp_i10 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i11, packed_tmp_i28, "tmp.i10", label_entry);
        BinaryOperator* packed_tmp_i9 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i10, packed_tmp_i27, "tmp.i9", label_entry);
        LoadInst* packed_93 = new LoadInst(const_ptr_71, "", false, label_entry);
        BinaryOperator* packed_tmp_i8 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i29, packed_93, "tmp.i8", label_entry);
        BinaryOperator* packed_tmp_i7 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i9, packed_tmp_i8, "tmp.i7", label_entry);
        BinaryOperator* packed_tmp_i6 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i27, packed_79, "tmp.i6", label_entry);
        BinaryOperator* packed_tmp_i5 = BinaryOperator::Create(Instruction::FSub, packed_tmp_i7, packed_tmp_i6, "tmp.i5", label_entry);
        LoadInst* packed_94 = new LoadInst(const_ptr_72, "", false, label_entry);
        BinaryOperator* packed_tmp_i4 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i29, packed_94, "tmp.i4", label_entry);
        BinaryOperator* packed_tmp_i3 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i28, packed_tmp_i5, "tmp.i3", label_entry);
        BinaryOperator* packed_tmp_i2 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i3, packed_tmp_i4, "tmp.i2", label_entry);
        CastInst* packed_tmp_i = new BitCastInst(packed_tmp_i2, vectorTy_int_SIMD, "tmp.i", label_entry);
        CastInst* packed_tmp1_i = new BitCastInst(packed_tmp_i50, vectorTy_int_SIMD, "tmp1.i", label_entry);
        BinaryOperator* packed_tmp2_i = BinaryOperator::Create(Instruction::Or, packed_tmp_i, packed_tmp1_i, "tmp2.i", label_entry);
        CastInst* packed_tmp3_i = new BitCastInst(packed_tmp2_i, vectorTy_float_SIMD, "tmp3.i", label_entry);
        ReturnInst::Create(context, packed_tmp3_i, label_entry);

      }

      return log_ps;
    }
    Function* generateExpPS(Module& mod, const unsigned simdWidth) const {
        if (Function* tmpF = mod.getFunction("exp_ps")) {
            return tmpF;
        }

		assert (!(mod.getFunction("_ZL5_ps_1") ||
			mod.getFunction("_ZL10_ps_exp_hi") ||
			mod.getFunction("_ZL10_ps_exp_lo") ||
			mod.getFunction("_ZL17_ps_cephes_LOG2EF") ||
			mod.getFunction("_ZL7_ps_0p5") ||
			mod.getFunction("_ZL17_ps_cephes_exp_C1") ||
			mod.getFunction("_ZL17_ps_cephes_exp_C2") ||
			mod.getFunction("_ZL17_ps_cephes_exp_p0") ||
			mod.getFunction("_ZL17_ps_cephes_exp_p1") ||
			mod.getFunction("_ZL17_ps_cephes_exp_p2") ||
			mod.getFunction("_ZL17_ps_cephes_exp_p3") ||
			mod.getFunction("_ZL17_ps_cephes_exp_p4") ||
			mod.getFunction("_ZL10_pi32_0x7f")) && "function to be generated is already declared in module!");

		LLVMContext& context = mod.getContext();

		VectorType* vectorTy_float_SIMD = VectorType::get(Type::getFloatTy(context), simdWidth);
		VectorType* vectorTy_int_SIMD = VectorType::get(Type::getInt32Ty(context), simdWidth);
        
      // Type Definitions
      ArrayType* ArrayTy_0 = ArrayType::get(Type::getFloatTy(context), 4);
      ArrayType* ArrayTy_2 = ArrayType::get(IntegerType::get(context, 32), 4);

      std::vector<Type*>FuncTy_5_args;
      FuncTy_5_args.push_back(vectorTy_float_SIMD);
      FunctionType* FuncTy_5 = FunctionType::get(
        /*Result=*/vectorTy_float_SIMD,
        /*Params=*/FuncTy_5_args,
        /*isVarArg=*/false);

      PointerType* PointerTy_6 = PointerType::get(vectorTy_float_SIMD, 0);

      VectorType* VectorTy_16 = VectorType::get(IntegerType::get(context, 64), 2);

      PointerType* PointerTy_17 = PointerType::get(VectorTy_16, 0);

      // Function Declarations

      Function* exp_ps = Function::Create(
        /*Type=*/FuncTy_5,
        /*Linkage=*/GlobalValue::ExternalLinkage,
        /*Name=*/"exp_ps", &mod);
      exp_ps->setCallingConv(CallingConv::C);
      AttrListPtr exp_ps_PAL;
      {
        SmallVector<AttributeWithIndex, 4> Attrs;
        AttributeWithIndex PAWI;
        PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
        Attrs.push_back(PAWI);
        exp_ps_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

      }
      exp_ps->setAttributes(exp_ps_PAL);


      // Global Variable Declarations


      GlobalVariable* gvar_array__ZL5_ps_1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL5_ps_1");
      gvar_array__ZL5_ps_1->setAlignment(16);

      GlobalVariable* gvar_array__ZL10_ps_exp_hi = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL10_ps_exp_hi");
      gvar_array__ZL10_ps_exp_hi->setAlignment(16);

      GlobalVariable* gvar_array__ZL10_ps_exp_lo = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL10_ps_exp_lo");
      gvar_array__ZL10_ps_exp_lo->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_LOG2EF = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_LOG2EF");
      gvar_array__ZL17_ps_cephes_LOG2EF->setAlignment(16);

      GlobalVariable* gvar_array__ZL7_ps_0p5 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL7_ps_0p5");
      gvar_array__ZL7_ps_0p5->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_exp_C1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_exp_C1");
      gvar_array__ZL17_ps_cephes_exp_C1->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_exp_C2 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_exp_C2");
      gvar_array__ZL17_ps_cephes_exp_C2->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_exp_p0 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_exp_p0");
      gvar_array__ZL17_ps_cephes_exp_p0->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_exp_p1 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_exp_p1");
      gvar_array__ZL17_ps_cephes_exp_p1->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_exp_p2 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_exp_p2");
      gvar_array__ZL17_ps_cephes_exp_p2->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_exp_p3 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_exp_p3");
      gvar_array__ZL17_ps_cephes_exp_p3->setAlignment(16);

      GlobalVariable* gvar_array__ZL17_ps_cephes_exp_p4 = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_0,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL17_ps_cephes_exp_p4");
      gvar_array__ZL17_ps_cephes_exp_p4->setAlignment(16);

      GlobalVariable* gvar_array__ZL10_pi32_0x7f = new GlobalVariable(
		mod,
      /*Type=*/ArrayTy_2,
      /*isConstant=*/true,
      /*Linkage=*/GlobalValue::InternalLinkage,
      /*Initializer=*/0, // has initializer, specified below
      /*Name=*/"_ZL10_pi32_0x7f");
      gvar_array__ZL10_pi32_0x7f->setAlignment(16);

      // Constant Definitions
      std::vector<Constant*> const_array_20_elems;
      ConstantFP* const_float_21 = ConstantFP::get(context, APFloat(1.000000e+00f));
      const_array_20_elems.push_back(const_float_21);
      const_array_20_elems.push_back(const_float_21);
      const_array_20_elems.push_back(const_float_21);
      const_array_20_elems.push_back(const_float_21);
      Constant* const_array_20 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_20_elems));
      std::vector<Constant*> const_array_22_elems;
      ConstantFP* const_float_23 = ConstantFP::get(context, APFloat(8.837626e+01f));
      const_array_22_elems.push_back(const_float_23);
      const_array_22_elems.push_back(const_float_23);
      const_array_22_elems.push_back(const_float_23);
      const_array_22_elems.push_back(const_float_23);
      Constant* const_array_22 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_22_elems));
      std::vector<Constant*> const_array_24_elems;
      ConstantFP* const_float_25 = ConstantFP::get(context, APFloat(-8.837626e+01f));
      const_array_24_elems.push_back(const_float_25);
      const_array_24_elems.push_back(const_float_25);
      const_array_24_elems.push_back(const_float_25);
      const_array_24_elems.push_back(const_float_25);
      Constant* const_array_24 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_24_elems));
      std::vector<Constant*> const_array_26_elems;
      ConstantFP* const_float_27 = ConstantFP::get(context, APFloat(1.442695e+00f));
      const_array_26_elems.push_back(const_float_27);
      const_array_26_elems.push_back(const_float_27);
      const_array_26_elems.push_back(const_float_27);
      const_array_26_elems.push_back(const_float_27);
      Constant* const_array_26 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_26_elems));
      std::vector<Constant*> const_array_28_elems;
      ConstantFP* const_float_29 = ConstantFP::get(context, APFloat(5.000000e-01f));
      const_array_28_elems.push_back(const_float_29);
      const_array_28_elems.push_back(const_float_29);
      const_array_28_elems.push_back(const_float_29);
      const_array_28_elems.push_back(const_float_29);
      Constant* const_array_28 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_28_elems));
      std::vector<Constant*> const_array_30_elems;
      ConstantFP* const_float_31 = ConstantFP::get(context, APFloat(6.933594e-01f));
      const_array_30_elems.push_back(const_float_31);
      const_array_30_elems.push_back(const_float_31);
      const_array_30_elems.push_back(const_float_31);
      const_array_30_elems.push_back(const_float_31);
      Constant* const_array_30 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_30_elems));
      std::vector<Constant*> const_array_32_elems;
      ConstantFP* const_float_33 = ConstantFP::get(context, APFloat(BitsToFloat(0xB95E8083U) /* -2.121944e-04 */));
      const_array_32_elems.push_back(const_float_33);
      const_array_32_elems.push_back(const_float_33);
      const_array_32_elems.push_back(const_float_33);
      const_array_32_elems.push_back(const_float_33);
      Constant* const_array_32 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_32_elems));
      std::vector<Constant*> const_array_34_elems;
      ConstantFP* const_float_35 = ConstantFP::get(context, APFloat(BitsToFloat(0x39506967U) /* 1.987569e-04 */));
      const_array_34_elems.push_back(const_float_35);
      const_array_34_elems.push_back(const_float_35);
      const_array_34_elems.push_back(const_float_35);
      const_array_34_elems.push_back(const_float_35);
      Constant* const_array_34 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_34_elems));
      std::vector<Constant*> const_array_36_elems;
      ConstantFP* const_float_37 = ConstantFP::get(context, APFloat(BitsToFloat(0x3AB743CEU) /* 1.398200e-03 */));
      const_array_36_elems.push_back(const_float_37);
      const_array_36_elems.push_back(const_float_37);
      const_array_36_elems.push_back(const_float_37);
      const_array_36_elems.push_back(const_float_37);
      Constant* const_array_36 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_36_elems));
      std::vector<Constant*> const_array_38_elems;
      ConstantFP* const_float_39 = ConstantFP::get(context, APFloat(8.333452e-03f));
      const_array_38_elems.push_back(const_float_39);
      const_array_38_elems.push_back(const_float_39);
      const_array_38_elems.push_back(const_float_39);
      const_array_38_elems.push_back(const_float_39);
      Constant* const_array_38 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_38_elems));
      std::vector<Constant*> const_array_40_elems;
      ConstantFP* const_float_41 = ConstantFP::get(context, APFloat(BitsToFloat(0x3D2AA9C1U) /* 4.166580e-02 */));
      const_array_40_elems.push_back(const_float_41);
      const_array_40_elems.push_back(const_float_41);
      const_array_40_elems.push_back(const_float_41);
      const_array_40_elems.push_back(const_float_41);
      Constant* const_array_40 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_40_elems));
      std::vector<Constant*> const_array_42_elems;
      ConstantFP* const_float_43 = ConstantFP::get(context, APFloat(BitsToFloat(0x3E2AAAAAU) /* 1.666667e-01 */));
      const_array_42_elems.push_back(const_float_43);
      const_array_42_elems.push_back(const_float_43);
      const_array_42_elems.push_back(const_float_43);
      const_array_42_elems.push_back(const_float_43);
      Constant* const_array_42 = ConstantArray::get(ArrayTy_0, ArrayRef<Constant*>(const_array_42_elems));
      std::vector<Constant*> const_array_44_elems;
      ConstantInt* const_int32_45 = ConstantInt::get(context, APInt(32,  "127", 10));
      const_array_44_elems.push_back(const_int32_45);
      const_array_44_elems.push_back(const_int32_45);
      const_array_44_elems.push_back(const_int32_45);
      const_array_44_elems.push_back(const_int32_45);
      Constant* const_array_44 = ConstantArray::get(ArrayTy_2, ArrayRef<Constant*>(const_array_44_elems));
      Constant* const_ptr_46 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL5_ps_1, PointerTy_6);
      Constant* const_ptr_47 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL10_ps_exp_hi, PointerTy_6);
      Constant* const_ptr_48 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL10_ps_exp_lo, PointerTy_6);
      Constant* const_ptr_49 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_LOG2EF, PointerTy_6);
      Constant* const_ptr_50 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL7_ps_0p5, PointerTy_6);
      ConstantInt* const_int8_51 = ConstantInt::get(context, APInt(8,  "1", 10));
      Constant* const_ptr_52 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_exp_C1, PointerTy_6);
      Constant* const_ptr_53 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_exp_C2, PointerTy_6);
      Constant* const_ptr_54 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_exp_p0, PointerTy_6);
      Constant* const_ptr_55 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_exp_p1, PointerTy_6);
      Constant* const_ptr_56 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_exp_p2, PointerTy_6);
      Constant* const_ptr_57 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_exp_p3, PointerTy_6);
      Constant* const_ptr_58 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL17_ps_cephes_exp_p4, PointerTy_6);
      Constant* const_ptr_59 = ConstantExpr::getCast(Instruction::BitCast, gvar_array__ZL10_pi32_0x7f, PointerTy_17);
      ConstantInt* const_int32_60 = ConstantInt::get(context, APInt(32,  "23", 10));

      // Global Variable Definitions
      gvar_array__ZL5_ps_1->setInitializer(const_array_20);
      gvar_array__ZL10_ps_exp_hi->setInitializer(const_array_22);
      gvar_array__ZL10_ps_exp_lo->setInitializer(const_array_24);
      gvar_array__ZL17_ps_cephes_LOG2EF->setInitializer(const_array_26);
      gvar_array__ZL7_ps_0p5->setInitializer(const_array_28);
      gvar_array__ZL17_ps_cephes_exp_C1->setInitializer(const_array_30);
      gvar_array__ZL17_ps_cephes_exp_C2->setInitializer(const_array_32);
      gvar_array__ZL17_ps_cephes_exp_p0->setInitializer(const_array_34);
      gvar_array__ZL17_ps_cephes_exp_p1->setInitializer(const_array_36);
      gvar_array__ZL17_ps_cephes_exp_p2->setInitializer(const_array_38);
      gvar_array__ZL17_ps_cephes_exp_p3->setInitializer(const_array_40);
      gvar_array__ZL17_ps_cephes_exp_p4->setInitializer(const_array_42);
      gvar_array__ZL10_pi32_0x7f->setInitializer(const_array_44);

      // Function Definitions

      // Function: exp_ps (exp_ps)
      {
        Function::arg_iterator args = exp_ps->arg_begin();
        Value* packed_x = args++;
        packed_x->setName("x");

        BasicBlock* label_entry = BasicBlock::Create(context, "entry",exp_ps,0);

        // Block entry (label_entry)
        LoadInst* packed_61 = new LoadInst(const_ptr_46, "", false, label_entry);
        LoadInst* packed_62 = new LoadInst(const_ptr_47, "", false, label_entry);
        std::vector<Value*> packed_63_params;
        packed_63_params.push_back(packed_x);
        packed_63_params.push_back(packed_62);
        CallInst* packed_63 = CallInst::Create(generateMinPS(mod), ArrayRef<Value*>(packed_63_params), "", label_entry);
        packed_63->setCallingConv(CallingConv::C);
        packed_63->setTailCall(true);AttrListPtr packed_63_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_63_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_63->setAttributes(packed_63_PAL);

        LoadInst* packed_64 = new LoadInst(const_ptr_48, "", false, label_entry);
        std::vector<Value*> packed_65_params;
        packed_65_params.push_back(packed_63);
        packed_65_params.push_back(packed_64);
        CallInst* packed_65 = CallInst::Create(generateMaxPS(mod), ArrayRef<Value*>(packed_65_params), "", label_entry);
        packed_65->setCallingConv(CallingConv::C);
        packed_65->setTailCall(true);AttrListPtr packed_65_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_65_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_65->setAttributes(packed_65_PAL);

        LoadInst* packed_66 = new LoadInst(const_ptr_49, "", false, label_entry);
        BinaryOperator* packed_tmp_i25 = BinaryOperator::Create(Instruction::FMul, packed_65, packed_66, "tmp.i25", label_entry);
        LoadInst* packed_67 = new LoadInst(const_ptr_50, "", false, label_entry);
        BinaryOperator* packed_tmp_i24 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i25, packed_67, "tmp.i24", label_entry);
        CallInst* packed_68 = CallInst::Create(generateCvttps2dq(mod), ArrayRef<Value*>(packed_tmp_i24), "", label_entry);
        packed_68->setCallingConv(CallingConv::C);
        packed_68->setTailCall(true);AttrListPtr packed_68_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_68_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_68->setAttributes(packed_68_PAL);

        CallInst* packed_69 = CallInst::Create(generateCvtdq2ps(mod), ArrayRef<Value*>(packed_68), "", label_entry);
        packed_69->setCallingConv(CallingConv::C);
        packed_69->setTailCall(true);AttrListPtr packed_69_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_69_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_69->setAttributes(packed_69_PAL);

        std::vector<Value*> packed_tmp_i23_params;
        packed_tmp_i23_params.push_back(packed_tmp_i24);
        packed_tmp_i23_params.push_back(packed_69);
        packed_tmp_i23_params.push_back(const_int8_51);
        CallInst* packed_tmp_i23 = CallInst::Create(generateCmpPS(mod), ArrayRef<Value*>(packed_tmp_i23_params), "tmp.i23", label_entry);
        packed_tmp_i23->setCallingConv(CallingConv::C);
        packed_tmp_i23->setTailCall(true);AttrListPtr packed_tmp_i23_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind;
          Attrs.push_back(PAWI);
          packed_tmp_i23_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_tmp_i23->setAttributes(packed_tmp_i23_PAL);

        CastInst* packed_tmp_i22 = new BitCastInst(packed_tmp_i23, vectorTy_int_SIMD, "tmp.i22", label_entry);
        CastInst* packed_tmp1_i = new BitCastInst(packed_61, vectorTy_int_SIMD, "tmp1.i", label_entry);
        BinaryOperator* packed_tmp2_i = BinaryOperator::Create(Instruction::And, packed_tmp_i22, packed_tmp1_i, "tmp2.i", label_entry);
        CastInst* packed_tmp3_i = new BitCastInst(packed_tmp2_i, vectorTy_float_SIMD, "tmp3.i", label_entry);
        BinaryOperator* packed_tmp_i21 = BinaryOperator::Create(Instruction::FSub, packed_69, packed_tmp3_i, "tmp.i21", label_entry);
        LoadInst* packed_70 = new LoadInst(const_ptr_52, "", false, label_entry);
        BinaryOperator* packed_tmp_i20 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i21, packed_70, "tmp.i20", label_entry);
        LoadInst* packed_71 = new LoadInst(const_ptr_53, "", false, label_entry);
        BinaryOperator* packed_tmp_i19 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i21, packed_71, "tmp.i19", label_entry);
        BinaryOperator* packed_tmp_i18 = BinaryOperator::Create(Instruction::FSub, packed_65, packed_tmp_i20, "tmp.i18", label_entry);
        BinaryOperator* packed_tmp_i17 = BinaryOperator::Create(Instruction::FSub, packed_tmp_i18, packed_tmp_i19, "tmp.i17", label_entry);
        BinaryOperator* packed_tmp_i16 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i17, packed_tmp_i17, "tmp.i16", label_entry);
        LoadInst* packed_72 = new LoadInst(const_ptr_54, "", false, label_entry);
        BinaryOperator* packed_tmp_i15 = BinaryOperator::Create(Instruction::FMul, packed_72, packed_tmp_i17, "tmp.i15", label_entry);
        LoadInst* packed_73 = new LoadInst(const_ptr_55, "", false, label_entry);
        BinaryOperator* packed_tmp_i14 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i15, packed_73, "tmp.i14", label_entry);
        BinaryOperator* packed_tmp_i13 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i14, packed_tmp_i17, "tmp.i13", label_entry);
        LoadInst* packed_74 = new LoadInst(const_ptr_56, "", false, label_entry);
        BinaryOperator* packed_tmp_i12 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i13, packed_74, "tmp.i12", label_entry);
        BinaryOperator* packed_tmp_i11 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i12, packed_tmp_i17, "tmp.i11", label_entry);
        LoadInst* packed_75 = new LoadInst(const_ptr_57, "", false, label_entry);
        BinaryOperator* packed_tmp_i10 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i11, packed_75, "tmp.i10", label_entry);
        BinaryOperator* packed_tmp_i9 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i10, packed_tmp_i17, "tmp.i9", label_entry);
        LoadInst* packed_76 = new LoadInst(const_ptr_58, "", false, label_entry);
        BinaryOperator* packed_tmp_i8 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i9, packed_76, "tmp.i8", label_entry);
        BinaryOperator* packed_tmp_i7 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i8, packed_tmp_i17, "tmp.i7", label_entry);
        BinaryOperator* packed_tmp_i6 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i7, packed_67, "tmp.i6", label_entry);
        BinaryOperator* packed_tmp_i5 = BinaryOperator::Create(Instruction::FMul, packed_tmp_i6, packed_tmp_i16, "tmp.i5", label_entry);
        BinaryOperator* packed_tmp_i4 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i5, packed_tmp_i17, "tmp.i4", label_entry);
        BinaryOperator* packed_tmp_i3 = BinaryOperator::Create(Instruction::FAdd, packed_tmp_i4, packed_61, "tmp.i3", label_entry);
        CallInst* packed_77 = CallInst::Create(generateCvttps2dq(mod), ArrayRef<Value*>(packed_tmp_i21), "", label_entry);
        packed_77->setCallingConv(CallingConv::C);
        packed_77->setTailCall(true);AttrListPtr packed_77_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_77_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_77->setAttributes(packed_77_PAL);

        LoadInst* packed_78 = new LoadInst(const_ptr_59, "", false, label_entry);
        CastInst* packed_79 = new BitCastInst(packed_78, vectorTy_int_SIMD, "", label_entry);
        BinaryOperator* packed_tmp_i2 = BinaryOperator::Create(Instruction::Add, packed_77, packed_79, "tmp.i2", label_entry);
        std::vector<Value*> packed_80_params;
        packed_80_params.push_back(packed_tmp_i2);
        packed_80_params.push_back(const_int32_60);
        CallInst* packed_80 = CallInst::Create(generatePsllid(mod), ArrayRef<Value*>(packed_80_params), "", label_entry);
        packed_80->setCallingConv(CallingConv::C);
        packed_80->setTailCall(true);AttrListPtr packed_80_PAL;
        {
          SmallVector<AttributeWithIndex, 4> Attrs;
          AttributeWithIndex PAWI;
          PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
          Attrs.push_back(PAWI);
          packed_80_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());

        }
        packed_80->setAttributes(packed_80_PAL);

        CastInst* packed_81 = new BitCastInst(packed_80, vectorTy_float_SIMD, "", label_entry);
        BinaryOperator* packed_tmp_i = BinaryOperator::Create(Instruction::FMul, packed_tmp_i3, packed_81, "tmp.i", label_entry);
        ReturnInst::Create(context, packed_tmp_i, label_entry);

      }

      return exp_ps;
    }

    Function* generateExp2PS(Module& mod, const unsigned simdWidth) const {
        if (Function* tmpF = mod.getFunction("exp2f_ps")) {
            return tmpF;
        }

		LLVMContext& context = mod.getContext();

		VectorType* vectorTy_float_SIMD = VectorType::get(Type::getFloatTy(context), simdWidth);
		VectorType* vectorTy_int_SIMD = VectorType::get(Type::getInt32Ty(context), simdWidth);
		
        // Type Definitions
		std::vector<Type*>FuncTy_1_args;
		FuncTy_1_args.push_back(vectorTy_float_SIMD);
		FunctionType* FuncTy_1 = FunctionType::get(
				/*Result=*/vectorTy_float_SIMD,
				/*Params=*/FuncTy_1_args,
				/*isVarArg=*/false);

		// Function Declarations
		Function* exp2_ps = Function::Create(
			/*Type=*/FuncTy_1,
			/*Linkage=*/GlobalValue::ExternalLinkage,
			/*Name=*/"exp2f_ps", &mod);
		exp2_ps->setCallingConv(CallingConv::C);
		AttrListPtr exp2_ps_PAL;
		{
			SmallVector<AttributeWithIndex, 4> Attrs;
			AttributeWithIndex PAWI;
			PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
			Attrs.push_back(PAWI);
			exp2_ps_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());
		}
		exp2_ps->setAttributes(exp2_ps_PAL);


		// Constant Definitions
		std::vector<Constant*> const_packed_3_elems;
		ConstantFP* const_float_4 = ConstantFP::get(context, APFloat(5.000000e-01f));
		const_packed_3_elems.push_back(const_float_4);
		const_packed_3_elems.push_back(const_float_4);
		const_packed_3_elems.push_back(const_float_4);
		const_packed_3_elems.push_back(const_float_4);
		Constant* const_packed_3 = ConstantVector::get(ArrayRef<Constant*>(const_packed_3_elems));
		std::vector<Constant*> const_packed_5_elems;
		ConstantInt* const_int32_6 = ConstantInt::get(context, APInt(32,  "23", 10));
		const_packed_5_elems.push_back(const_int32_6);
		const_packed_5_elems.push_back(const_int32_6);
		const_packed_5_elems.push_back(const_int32_6);
		const_packed_5_elems.push_back(const_int32_6);
		Constant* const_packed_5 = ConstantVector::get(ArrayRef<Constant*>(const_packed_5_elems));
		std::vector<Constant*> const_packed_7_elems;
		ConstantInt* const_int32_8 = ConstantInt::get(context, APInt(32,  "1065353216", 10));
		const_packed_7_elems.push_back(const_int32_8);
		const_packed_7_elems.push_back(const_int32_8);
		const_packed_7_elems.push_back(const_int32_8);
		const_packed_7_elems.push_back(const_int32_8);
		Constant* const_packed_7 = ConstantVector::get(ArrayRef<Constant*>(const_packed_7_elems));
		std::vector<Constant*> const_packed_9_elems;
		ConstantFP* const_float_10 = ConstantFP::get(context, APFloat(BitsToFloat(0x3AF61905U) /* 1.877577e-03 */));
		const_packed_9_elems.push_back(const_float_10);
		const_packed_9_elems.push_back(const_float_10);
		const_packed_9_elems.push_back(const_float_10);
		const_packed_9_elems.push_back(const_float_10);
		Constant* const_packed_9 = ConstantVector::get(ArrayRef<Constant*>(const_packed_9_elems));
		std::vector<Constant*> const_packed_11_elems;
		ConstantFP* const_float_12 = ConstantFP::get(context, APFloat(8.989340e-03f));
		const_packed_11_elems.push_back(const_float_12);
		const_packed_11_elems.push_back(const_float_12);
		const_packed_11_elems.push_back(const_float_12);
		const_packed_11_elems.push_back(const_float_12);
		Constant* const_packed_11 = ConstantVector::get(ArrayRef<Constant*>(const_packed_11_elems));
		std::vector<Constant*> const_packed_13_elems;
		ConstantFP* const_float_14 = ConstantFP::get(context, APFloat(BitsToFloat(0x3D64AA23U) /* 5.582632e-02 */));
		const_packed_13_elems.push_back(const_float_14);
		const_packed_13_elems.push_back(const_float_14);
		const_packed_13_elems.push_back(const_float_14);
		const_packed_13_elems.push_back(const_float_14);
		Constant* const_packed_13 = ConstantVector::get(ArrayRef<Constant*>(const_packed_13_elems));
		std::vector<Constant*> const_packed_15_elems;
		ConstantFP* const_float_16 = ConstantFP::get(context, APFloat(BitsToFloat(0x3E75EAD4U) /* 2.401536e-01 */));
		const_packed_15_elems.push_back(const_float_16);
		const_packed_15_elems.push_back(const_float_16);
		const_packed_15_elems.push_back(const_float_16);
		const_packed_15_elems.push_back(const_float_16);
		Constant* const_packed_15 = ConstantVector::get(ArrayRef<Constant*>(const_packed_15_elems));
		std::vector<Constant*> const_packed_17_elems;
		ConstantFP* const_float_18 = ConstantFP::get(context, APFloat(6.931531e-01f));
		const_packed_17_elems.push_back(const_float_18);
		const_packed_17_elems.push_back(const_float_18);
		const_packed_17_elems.push_back(const_float_18);
		const_packed_17_elems.push_back(const_float_18);
		Constant* const_packed_17 = ConstantVector::get(ArrayRef<Constant*>(const_packed_17_elems));
		std::vector<Constant*> const_packed_19_elems;
		ConstantFP* const_float_20 = ConstantFP::get(context, APFloat(BitsToFloat(0x3F7FFFFFU) /* 9.999999e-01 */));
		const_packed_19_elems.push_back(const_float_20);
		const_packed_19_elems.push_back(const_float_20);
		const_packed_19_elems.push_back(const_float_20);
		const_packed_19_elems.push_back(const_float_20);
		Constant* const_packed_19 = ConstantVector::get(ArrayRef<Constant*>(const_packed_19_elems));

		// Function Definitions

		// Function: exp2f_ps (func_exp2f_ps)
		{
			Function::arg_iterator args = exp2_ps->arg_begin();
			Value* packed_x = args++;
			packed_x->setName("x");

			BasicBlock* label_entry = BasicBlock::Create(context, "entry",exp2_ps,0);

			// Block entry (label_entry)
			BinaryOperator* packed_21 = BinaryOperator::Create(Instruction::FSub, packed_x, const_packed_3, "", label_entry);
			CastInst* packed_22 = new FPToSIInst(packed_21, vectorTy_int_SIMD, "", label_entry);
			CastInst* packed_23 = new SIToFPInst(packed_22, vectorTy_float_SIMD, "", label_entry);
			BinaryOperator* packed_24 = BinaryOperator::Create(Instruction::FSub, packed_x, packed_23, "", label_entry);
			BinaryOperator* packed_25 = BinaryOperator::Create(Instruction::Shl, packed_22, const_packed_5, "", label_entry);
			BinaryOperator* packed_26 = BinaryOperator::Create(Instruction::Add, packed_25, const_packed_7, "", label_entry);
			CastInst* packed_27 = new BitCastInst(packed_26, vectorTy_float_SIMD, "", label_entry);
			BinaryOperator* packed_28 = BinaryOperator::Create(Instruction::FMul, packed_24, const_packed_9, "", label_entry);
			BinaryOperator* packed_29 = BinaryOperator::Create(Instruction::FAdd, packed_28, const_packed_11, "", label_entry);
			BinaryOperator* packed_30 = BinaryOperator::Create(Instruction::FMul, packed_29, packed_24, "", label_entry);
			BinaryOperator* packed_31 = BinaryOperator::Create(Instruction::FAdd, packed_30, const_packed_13, "", label_entry);
			BinaryOperator* packed_32 = BinaryOperator::Create(Instruction::FMul, packed_31, packed_24, "", label_entry);
			BinaryOperator* packed_33 = BinaryOperator::Create(Instruction::FAdd, packed_32, const_packed_15, "", label_entry);
			BinaryOperator* packed_34 = BinaryOperator::Create(Instruction::FMul, packed_33, packed_24, "", label_entry);
			BinaryOperator* packed_35 = BinaryOperator::Create(Instruction::FAdd, packed_34, const_packed_17, "", label_entry);
			BinaryOperator* packed_36 = BinaryOperator::Create(Instruction::FMul, packed_35, packed_24, "", label_entry);
			BinaryOperator* packed_37 = BinaryOperator::Create(Instruction::FAdd, packed_36, const_packed_19, "", label_entry);
			BinaryOperator* packed_38 = BinaryOperator::Create(Instruction::FMul, packed_27, packed_37, "", label_entry);
			ReturnInst::Create(context, packed_38, label_entry);
		}

      return exp2_ps;
    }
	Function* generateLog2PS(Module& mod, const unsigned simdWidth) const {
        if (Function* tmpF = mod.getFunction("log2f_ps")) {
            return tmpF;
        }

		LLVMContext& context = mod.getContext();

		VectorType* vectorTy_float_SIMD = VectorType::get(Type::getFloatTy(context), simdWidth);
		VectorType* vectorTy_int_SIMD = VectorType::get(Type::getInt32Ty(context), simdWidth);

		// Type Definitions
		std::vector<Type*>FuncTy_1_args;
		FuncTy_1_args.push_back(vectorTy_float_SIMD);
		FunctionType* FuncTy_1 = FunctionType::get(
			/*Result=*/vectorTy_float_SIMD,
			/*Params=*/FuncTy_1_args,
			/*isVarArg=*/false);

		// Function Declarations
		Function* log2_ps = Function::Create(
			/*Type=*/FuncTy_1,
			/*Linkage=*/GlobalValue::ExternalLinkage,
			/*Name=*/"log2f_ps", &mod);
		log2_ps->setCallingConv(CallingConv::C);
		AttrListPtr log2_ps_PAL;
		{
			SmallVector<AttributeWithIndex, 4> Attrs;
			AttributeWithIndex PAWI;
			PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadNone;
			Attrs.push_back(PAWI);
			log2_ps_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());
		}
		log2_ps->setAttributes(log2_ps_PAL);

		// Global Variable Declarations


		// Constant Definitions
		std::vector<Constant*> const_packed_3_elems;
		ConstantInt* const_int32_4 = ConstantInt::get(context, APInt(32,  "23", 10));
		const_packed_3_elems.push_back(const_int32_4);
		const_packed_3_elems.push_back(const_int32_4);
		const_packed_3_elems.push_back(const_int32_4);
		const_packed_3_elems.push_back(const_int32_4);
		Constant* const_packed_3 = ConstantVector::get(ArrayRef<Constant*>(const_packed_3_elems));
		std::vector<Constant*> const_packed_5_elems;
		ConstantInt* const_int32_6 = ConstantInt::get(context, APInt(32,  "255", 10));
		const_packed_5_elems.push_back(const_int32_6);
		const_packed_5_elems.push_back(const_int32_6);
		const_packed_5_elems.push_back(const_int32_6);
		const_packed_5_elems.push_back(const_int32_6);
		Constant* const_packed_5 = ConstantVector::get(ArrayRef<Constant*>(const_packed_5_elems));
		std::vector<Constant*> const_packed_7_elems;
		ConstantInt* const_int32_8 = ConstantInt::get(context, APInt(32,  "-127", 10));
		const_packed_7_elems.push_back(const_int32_8);
		const_packed_7_elems.push_back(const_int32_8);
		const_packed_7_elems.push_back(const_int32_8);
		const_packed_7_elems.push_back(const_int32_8);
		Constant* const_packed_7 = ConstantVector::get(ArrayRef<Constant*>(const_packed_7_elems));
		std::vector<Constant*> const_packed_9_elems;
		ConstantInt* const_int32_10 = ConstantInt::get(context, APInt(32,  "1065353216", 10));
		const_packed_9_elems.push_back(const_int32_10);
		const_packed_9_elems.push_back(const_int32_10);
		const_packed_9_elems.push_back(const_int32_10);
		const_packed_9_elems.push_back(const_int32_10);
		Constant* const_packed_9 = ConstantVector::get(ArrayRef<Constant*>(const_packed_9_elems));
		std::vector<Constant*> const_packed_11_elems;
		ConstantInt* const_int32_12 = ConstantInt::get(context, APInt(32,  "1073741823", 10));
		const_packed_11_elems.push_back(const_int32_12);
		const_packed_11_elems.push_back(const_int32_12);
		const_packed_11_elems.push_back(const_int32_12);
		const_packed_11_elems.push_back(const_int32_12);
		Constant* const_packed_11 = ConstantVector::get(ArrayRef<Constant*>(const_packed_11_elems));
		std::vector<Constant*> const_packed_13_elems;
		ConstantFP* const_float_14 = ConstantFP::get(context, APFloat(5.965155e-02f));
		const_packed_13_elems.push_back(const_float_14);
		const_packed_13_elems.push_back(const_float_14);
		const_packed_13_elems.push_back(const_float_14);
		const_packed_13_elems.push_back(const_float_14);
		Constant* const_packed_13 = ConstantVector::get(ArrayRef<Constant*>(const_packed_13_elems));
		std::vector<Constant*> const_packed_15_elems;
		ConstantFP* const_float_16 = ConstantFP::get(context, APFloat(BitsToFloat(0x3EEE7397U) /* 4.657256e-01 */));
		const_packed_15_elems.push_back(const_float_16);
		const_packed_15_elems.push_back(const_float_16);
		const_packed_15_elems.push_back(const_float_16);
		const_packed_15_elems.push_back(const_float_16);
		Constant* const_packed_15 = ConstantVector::get(ArrayRef<Constant*>(const_packed_15_elems));
		std::vector<Constant*> const_packed_17_elems;
		ConstantFP* const_float_18 = ConstantFP::get(context, APFloat(BitsToFloat(0x3FBD96DDU) /* 1.481166e+00 */));
		const_packed_17_elems.push_back(const_float_18);
		const_packed_17_elems.push_back(const_float_18);
		const_packed_17_elems.push_back(const_float_18);
		const_packed_17_elems.push_back(const_float_18);
		Constant* const_packed_17 = ConstantVector::get(ArrayRef<Constant*>(const_packed_17_elems));
		std::vector<Constant*> const_packed_19_elems;
		ConstantFP* const_float_20 = ConstantFP::get(context, APFloat(BitsToFloat(0x402153F6U) /* 2.520750e+00 */));
		const_packed_19_elems.push_back(const_float_20);
		const_packed_19_elems.push_back(const_float_20);
		const_packed_19_elems.push_back(const_float_20);
		const_packed_19_elems.push_back(const_float_20);
		Constant* const_packed_19 = ConstantVector::get(ArrayRef<Constant*>(const_packed_19_elems));
		std::vector<Constant*> const_packed_21_elems;
		ConstantFP* const_float_22 = ConstantFP::get(context, APFloat(BitsToFloat(0x4038D96CU) /* 2.888270e+00 */));
		const_packed_21_elems.push_back(const_float_22);
		const_packed_21_elems.push_back(const_float_22);
		const_packed_21_elems.push_back(const_float_22);
		const_packed_21_elems.push_back(const_float_22);
		Constant* const_packed_21 = ConstantVector::get(ArrayRef<Constant*>(const_packed_21_elems));
		std::vector<Constant*> const_packed_23_elems;
		ConstantFP* const_float_24 = ConstantFP::get(context, APFloat(1.000000e+00f));
		const_packed_23_elems.push_back(const_float_24);
		const_packed_23_elems.push_back(const_float_24);
		const_packed_23_elems.push_back(const_float_24);
		const_packed_23_elems.push_back(const_float_24);
		Constant* const_packed_23 = ConstantVector::get(ArrayRef<Constant*>(const_packed_23_elems));

		// Global Variable Definitions

		// Function Definitions

		// Function: log2f_ps (func_log2f_ps)
		{
			Function::arg_iterator args = log2_ps->arg_begin();
			Value* packed_x = args++;
			packed_x->setName("x");

			BasicBlock* label_entry = BasicBlock::Create(context, "entry",log2_ps,0);

			// Block entry (label_entry)
			CastInst* packed_x6 = new BitCastInst(packed_x, vectorTy_int_SIMD, "x6", label_entry);
			BinaryOperator* packed_25 = BinaryOperator::Create(Instruction::LShr, packed_x6, const_packed_3, "", label_entry);
			BinaryOperator* packed_26 = BinaryOperator::Create(Instruction::And, packed_25, const_packed_5, "", label_entry);
			BinaryOperator* packed_27 = BinaryOperator::Create(Instruction::Add, packed_26, const_packed_7, "", label_entry);
			CastInst* packed_28 = new SIToFPInst(packed_27, vectorTy_float_SIMD, "", label_entry);
			BinaryOperator* packed_29 = BinaryOperator::Create(Instruction::Or, packed_x6, const_packed_9, "", label_entry);
			BinaryOperator* packed_30 = BinaryOperator::Create(Instruction::And, packed_29, const_packed_11, "", label_entry);
			CastInst* packed_31 = new BitCastInst(packed_30, vectorTy_float_SIMD, "", label_entry);
			BinaryOperator* packed_32 = BinaryOperator::Create(Instruction::FMul, packed_31, const_packed_13, "", label_entry);
			BinaryOperator* packed_33 = BinaryOperator::Create(Instruction::FSub, packed_32, const_packed_15, "", label_entry);
			BinaryOperator* packed_34 = BinaryOperator::Create(Instruction::FMul, packed_33, packed_31, "", label_entry);
			BinaryOperator* packed_35 = BinaryOperator::Create(Instruction::FAdd, packed_34, const_packed_17, "", label_entry);
			BinaryOperator* packed_36 = BinaryOperator::Create(Instruction::FMul, packed_35, packed_31, "", label_entry);
			BinaryOperator* packed_37 = BinaryOperator::Create(Instruction::FSub, packed_36, const_packed_19, "", label_entry);
			BinaryOperator* packed_38 = BinaryOperator::Create(Instruction::FMul, packed_37, packed_31, "", label_entry);
			BinaryOperator* packed_39 = BinaryOperator::Create(Instruction::FAdd, packed_38, const_packed_21, "", label_entry);
			BinaryOperator* packed_40 = BinaryOperator::Create(Instruction::FSub, packed_31, const_packed_23, "", label_entry);
			BinaryOperator* packed_41 = BinaryOperator::Create(Instruction::FMul, packed_40, packed_39, "", label_entry);
			BinaryOperator* packed_42 = BinaryOperator::Create(Instruction::FAdd, packed_41, packed_28, "", label_entry);
			ReturnInst::Create(context, packed_42, label_entry);

		}

		return log2_ps;
	}
	// this function has issues for certain input values (see e.g. phong) :(
	Function* generatePowPS(Module& mod, const unsigned simdWidth) const {
        if (Function* tmpF = mod.getFunction("pow_ps")) {
            return tmpF;
        }

		LLVMContext& context = mod.getContext();

		VectorType* vectorTy_float_SIMD = VectorType::get(Type::getFloatTy(context), simdWidth);

		// Type Definitions
		std::vector<Type*>FuncTy_2_args;
		FuncTy_2_args.push_back(vectorTy_float_SIMD);
		FuncTy_2_args.push_back(vectorTy_float_SIMD);
		FunctionType* FuncTy_2 = FunctionType::get(
			/*Result=*/vectorTy_float_SIMD,
			/*Params=*/FuncTy_2_args,
			/*isVarArg=*/false);

		// Function Declarations
		Function* pow_ps = Function::Create(
		/*Type=*/FuncTy_2,
		/*Linkage=*/GlobalValue::ExternalLinkage,
		/*Name=*/"pow_ps", &mod);
		pow_ps->setCallingConv(CallingConv::C);
		AttrListPtr pow_ps_PAL;
		{
			SmallVector<AttributeWithIndex, 4> Attrs;
			AttributeWithIndex PAWI;
			PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadOnly;
			Attrs.push_back(PAWI);
			pow_ps_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());
		}
		pow_ps->setAttributes(pow_ps_PAL);

		// Function Definitions

		// Function: pow_ps (pow_ps)
		{
			Function::arg_iterator args = pow_ps->arg_begin();
			Value* packed_x = args++;
			packed_x->setName("x");
			Value* packed_y = args++;
			packed_y->setName("y");

			BasicBlock* label_entry = BasicBlock::Create(context, "entry",pow_ps,0);

			// Block entry (label_entry)
			CallInst* packed_4 = CallInst::Create(generateLog2PS(mod, simdWidth), ArrayRef<Value*>(packed_x), "", label_entry);
			packed_4->setCallingConv(CallingConv::C);
			packed_4->setTailCall(true);AttrListPtr packed_4_PAL;
			packed_4->setAttributes(packed_4_PAL);

			BinaryOperator* packed_5 = BinaryOperator::Create(Instruction::FMul, packed_4, packed_y, "", label_entry);
			CallInst* packed_6 = CallInst::Create(generateExp2PS(mod, simdWidth), ArrayRef<Value*>(packed_5), "", label_entry);
			packed_6->setCallingConv(CallingConv::C);
			packed_6->setTailCall(true);AttrListPtr packed_6_PAL;
			packed_6->setAttributes(packed_6_PAL);

			ReturnInst::Create(context, packed_6, label_entry);
		}

		return pow_ps;
	}
	/*
	define <4 x float> @fabs_ps(<4 x float> %x) nounwind readnone {
	entry:
		%tmp.i = bitcast <4 x float> %x to <4 x i32>    ; <<4 x i32>> [#uses=1]
		%0 = and <4 x i32> %tmp.i, <i32 1325400064, i32 1325400064, i32 1325400064, i32 1325400064> ; <<4 x i32>> [#uses=1]
		%1 = bitcast <4 x i32> %0 to <4 x float>        ; <<4 x float>> [#uses=1]
		ret <4 x float> %1
	}
	*/
	Function* generateAbsPS(Module& mod, const unsigned simdWidth) const {
        if (Function* tmpF = mod.getFunction("abs_ps")) {
            return tmpF;
        }

		LLVMContext& context = mod.getContext();

		VectorType* vectorTy_float_SIMD = VectorType::get(Type::getFloatTy(context), simdWidth);
		VectorType* vectorTy_int_SIMD = VectorType::get(Type::getInt32Ty(context), simdWidth);

		// Type Definitions
		std::vector<Type*>FuncTy_2_args;
		FuncTy_2_args.push_back(vectorTy_float_SIMD);
		FunctionType* FuncTy_2 = FunctionType::get(
			/*Result=*/vectorTy_float_SIMD,
			/*Params=*/FuncTy_2_args,
			/*isVarArg=*/false);

		// Function Declarations
		Function* abs_ps = Function::Create(
		/*Type=*/FuncTy_2,
		/*Linkage=*/GlobalValue::ExternalLinkage,
		/*Name=*/"abs_ps", &mod);
		abs_ps->setCallingConv(CallingConv::C);
		AttrListPtr abs_ps_PAL;
		{
			SmallVector<AttributeWithIndex, 4> Attrs;
			AttributeWithIndex PAWI;
			PAWI.Index = 4294967295U; PAWI.Attrs = 0  | Attribute::NoUnwind | Attribute::ReadOnly;
			Attrs.push_back(PAWI);
			abs_ps_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());
		}
		abs_ps->setAttributes(abs_ps_PAL);

		// Function Definitions

		// Function: abs_ps (abs_ps)
		{
			Function::arg_iterator args = abs_ps->arg_begin();
			Value* packed_x = args++;
			packed_x->setName("x");

			BasicBlock* label_entry = BasicBlock::Create(context, "entry",abs_ps,0);

			// Block entry (label_entry)
			BitCastInst* bc = new BitCastInst(packed_x, vectorTy_int_SIMD, "", label_entry);

			std::vector<Constant*> elems;
			// 4 x 0x7FFFFFFF
			//ConstantInt* scalarConst = ConstantInt::get(mod.getContext(), APInt(32, StringRef("1325400064"), 10));
			//ConstantInt* scalarConst = ConstantInt::get(mod.getContext(), APInt(32, StringRef("2.147483647e9"), 10));
			ConstantInt* scalarConst = ConstantInt::get(mod.getContext(), APInt(32, StringRef("2147483647"), 10));
			elems.push_back(scalarConst);
			elems.push_back(scalarConst);
			elems.push_back(scalarConst);
			elems.push_back(scalarConst);
			Constant* packedConst = ConstantVector::get(ArrayRef<Constant*>(elems));

			BinaryOperator* andInst = BinaryOperator::Create(Instruction::And, bc, packedConst, "", label_entry);
			BitCastInst* bc2 = new BitCastInst(andInst, vectorTy_float_SIMD, "", label_entry);

			ReturnInst::Create(context, bc2, label_entry);
		}

		return abs_ps;
	}
};

#endif	/* _NATIVESSEMATHFUNCTIONS_HPP */

