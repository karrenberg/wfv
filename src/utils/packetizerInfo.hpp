/**
 * @file   packetizerInfo.hpp
 * @date   13.10.2009
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2009, 2010, 2011 Saarland University
 *
 */
#ifndef _PACKETIZERINFO_HPP
#define	_PACKETIZERINFO_HPP

#include <llvm/Target/TargetData.h>

#include "nativeMethods.hpp"


using namespace llvm;

namespace Packetizer {

class PacketizerInfo {
public:
    PacketizerInfo(Module* M,
                   LLVMContext* C,
                   const unsigned simd_width,
				   const unsigned packetization_size,
                   const bool use_sse41_flag=false,
                   const bool use_avx_flag=false,
                   const bool verbose_flag=false)
            : mModule(M),
                    mTargetData(new TargetData(M)),
                    mContext(C),
                    mNativeMethods(new NativeMethods(use_sse41_flag, use_avx_flag, verbose_flag)),
                    mSimdWidth(simd_width),
                    mPacketizationSize(packetization_size),
                    mTotalSIMDIterations(packetization_size/simd_width),
                    mUseSSE41(use_sse41_flag),
                    mUseAVX(use_avx_flag),
                    mVerbose(verbose_flag)
    {
		// initialize all constants (values that do not depend on module or target data)

		assert (mPacketizationSize > 0);
		if (mPacketizationSize % mSimdWidth != 0)
        {
			errs() << "ERROR: Packetization size (" << mPacketizationSize << ") is no multiple of 'simdWidth' (" << mSimdWidth << ")!\n";
			assert (false && "packetization size is no multiple of SIMD width!");
		}

		DEBUG_PKT( outs() << "\nuse SSE4.1 = " << (mUseSSE41 ? "true" : "false") << "\n"; );
		DEBUG_PKT( outs() << "\nuse AVX = " << (mUseAVX ? "true" : "false") << "\n"; );

		//create packet-datatypes
		mVectorTyFloatSIMD = VectorType::get(Type::getFloatTy(*mContext), mSimdWidth);
		mVectorTyIntSIMD = VectorType::get(Type::getInt32Ty(*mContext), mSimdWidth);
		mVectorTyBoolSIMD = VectorType::get(Type::getInt1Ty(*mContext), mSimdWidth);

		//generate constants
		mConstVecSIMDInt32MinusOne = createPacketConstantInt(-1);
		mConstVecSIMDF32One = createPacketConstantFloat(1.000000e+00f);
		mConstInt32Zero = ConstantInt::get(*mContext, APInt(32,  "0", 10));
		mConstInt32One = ConstantInt::get(*mContext, APInt(32,  "1", 10));
		mConstInt32Two = ConstantInt::get(*mContext, APInt(32,  "2", 10));
		mConstInt32Three = ConstantInt::get(*mContext, APInt(32,  "3", 10));
		mConstBoolTrue = Constant::getAllOnesValue(Type::getInt1Ty(*mContext));
		mConstBoolFalse = Constant::getNullValue(Type::getInt1Ty(*mContext));
		
		
		// initialize "dynamic" stuff

		mAlignmentScalar = mTargetData->getABITypeAlignment(Type::getFloatTy(*mContext));
		mAlignmentPtr = mTargetData->getABITypeAlignment(PointerType::getUnqual(Type::getFloatTy(*mContext)));
		mAlignmentSIMDPtr = mTargetData->getABITypeAlignment(PointerType::getUnqual(mVectorTyFloatSIMD));
		mAlignmentSIMD = mTargetData->getABITypeAlignment(mVectorTyFloatSIMD); //simdWidth * 4;
		DEBUG_PKT( outs() << "\nalignment = " << mAlignmentScalar << "\n"; );
		DEBUG_PKT( outs() << "alignmentPtr = " << mAlignmentPtr << "\n"; );
		DEBUG_PKT( outs() << "alignmentSIMDPtr = " << mAlignmentSIMDPtr << "\n"; );
		DEBUG_PKT( outs() << "alignmentSIMD = " << mAlignmentSIMD << "\n\n"; );
		
		mConstAlignmentSIMD = ConstantInt::get(*mContext, APInt(32, mAlignmentSIMD));
	}

	~PacketizerInfo()
    {
		delete mTargetData;
        if (mNativeMethods) delete mNativeMethods;
	}

	Module*        mModule;
	TargetData*    mTargetData;
    LLVMContext*   mContext;
	NativeMethods* mNativeMethods; // natively available SSE function mappings

	//packetization information
	const unsigned mSimdWidth; //TODO: get from host machine (target data?)
	const unsigned mPacketizationSize;
	const unsigned mTotalSIMDIterations;

	const bool mUseSSE41;
	const bool mUseAVX;
	const bool mVerbose;

	//packetized datatypes
	VectorType* mVectorTyFloatSIMD;
	VectorType* mVectorTyIntSIMD;
	VectorType* mVectorTyBoolSIMD;

	//llvm constants
	Constant*    mConstVecSIMDInt32MinusOne;
	Constant*    mConstVecSIMDF32One;
	ConstantInt* mConstInt32Zero;
	ConstantInt* mConstInt32One;
	ConstantInt* mConstInt32Two;
	ConstantInt* mConstInt32Three;
	Constant*    mConstBoolTrue;
	Constant*    mConstBoolFalse;
	Constant*    mConstAlignmentSIMD;
	
	//alignment information
	unsigned mAlignmentScalar;
	unsigned mAlignmentPtr;
	unsigned mAlignmentSIMDPtr;
	unsigned mAlignmentSIMD;

	// NOTE: we must not pre-generate this due to possibly different address spaces
	const PointerType* getPointerVectorType(const PointerType* oldType) const
    {
		return PointerType::get(VectorType::get(oldType->getElementType(), mSimdWidth),
                                oldType->getAddressSpace());
	}
	
private:

	inline Constant* createPacketConstantInt(const int c) const
    {
		std::vector<Constant*> cVec; //packet of 'simdWidth' int32
		ConstantInt* const_int32 = ConstantInt::get(*mContext, APInt(32, c));
		for (unsigned i=0; i<mSimdWidth; ++i)
        {
			cVec.push_back(const_int32);
		}
		return ConstantVector::get(ArrayRef<Constant*>(cVec));
	}

	inline Constant* createPacketConstantFloat(const float c) const
    {
		std::vector<Constant*> fVec; //packet of 'simdWidth' f32
		ConstantFP* const_f32 = ConstantFP::get(*mContext, APFloat(c));
		for (unsigned i=0; i<mSimdWidth; ++i)
        {
			fVec.push_back(const_f32);
		}
		return ConstantVector::get(ArrayRef<Constant*>(fVec));
	}

};

}

#endif	/* _PACKETIZERINFO_HPP */

