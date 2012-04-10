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
	PacketizerInfo(Module& M,
				   const unsigned simd_width,
				   const unsigned packetization_size,
				   const bool use_sse41_flag=false,
				   const bool use_avx_flag=false,
				   const bool verbose_flag=false)
			: module(M),
					targetData(new TargetData(&M)),
					simdWidth(simd_width),
					packetizationSize(packetization_size),
					totalSIMDIterations(packetization_size/simd_width),
					use_sse41(use_sse41_flag),
					use_avx(use_avx_flag),
					verbose(verbose_flag)
	{
		// initialize all constants (values that do not depend on module or target data)

		assert (packetizationSize > 0);
		if (packetizationSize % simdWidth != 0) {
			errs() << "ERROR: Packetization size (" << packetizationSize << ") is no multiple of 'simdWidth' (" << simdWidth << ")!\n";
			assert (false && "packetization size is no multiple of SIMD width!");
		}

		DEBUG_PKT( outs() << "\nuse SSE4.1 = " << (use_sse41 ? "true" : "false") << "\n"; );
		DEBUG_PKT( outs() << "\nuse AVX = " << (use_avx ? "true" : "false") << "\n"; );

		//create packet-datatypes
		vectorTy_floatSIMD = VectorType::get(Type::getFloatTy(getGlobalContext()), simdWidth);
		vectorTy_intSIMD = VectorType::get(Type::getInt32Ty(getGlobalContext()), simdWidth);
		vectorTy_boolSIMD = VectorType::get(Type::getInt1Ty(getGlobalContext()), simdWidth);

		//generate constants
		const_vec_SIMD_int32_neg1 = createPacketConstantInt(-1);
		const_vec_SIMD_f32_1 = createPacketConstantFloat(1.000000e+00f);
		const_int32_0 = ConstantInt::get(getGlobalContext(), APInt(32,  "0", 10));
		const_int32_1 = ConstantInt::get(getGlobalContext(), APInt(32,  "1", 10));
		const_int32_2 = ConstantInt::get(getGlobalContext(), APInt(32,  "2", 10));
		const_int32_3 = ConstantInt::get(getGlobalContext(), APInt(32,  "3", 10));
		const_bool_1 = Constant::getAllOnesValue(Type::getInt1Ty(getGlobalContext()));
		const_bool_0 = Constant::getNullValue(Type::getInt1Ty(getGlobalContext()));
		
		
		// initialize "dynamic" stuff

		alignmentScalar = targetData->getABITypeAlignment(Type::getFloatTy(getGlobalContext()));
		alignmentPtr = targetData->getABITypeAlignment(PointerType::getUnqual(Type::getFloatTy(getGlobalContext())));
		alignmentSIMDPtr = targetData->getABITypeAlignment(PointerType::getUnqual(vectorTy_floatSIMD));
		alignmentSIMD = targetData->getABITypeAlignment(vectorTy_floatSIMD); //simdWidth * 4;
		DEBUG_PKT( outs() << "\nalignment = " << alignmentScalar << "\n"; );
		DEBUG_PKT( outs() << "alignmentPtr = " << alignmentPtr << "\n"; );
		DEBUG_PKT( outs() << "alignmentSIMDPtr = " << alignmentSIMDPtr << "\n"; );
		DEBUG_PKT( outs() << "alignmentSIMD = " << alignmentSIMD << "\n\n"; );
		
		const_alignment_SIMD = ConstantInt::get(getGlobalContext(), APInt(32, alignmentSIMD));
	}

	~PacketizerInfo() {
		//delete targetData; // memory leak, but deleting introduces a segfault...
	}

	Module& module;
	TargetData* targetData;

	//packetization information
	const unsigned simdWidth; //TODO: get from host machine (target data?)
	const unsigned packetizationSize;
	const unsigned totalSIMDIterations;

	const bool use_sse41;
	const bool use_avx;
	const bool verbose;

	//packetized datatypes
	VectorType* vectorTy_floatSIMD;
	VectorType* vectorTy_intSIMD;
	VectorType* vectorTy_boolSIMD;

	//llvm constants
	Constant* const_vec_SIMD_int32_neg1;
	Constant* const_vec_SIMD_f32_1;
	ConstantInt* const_int32_0;
	ConstantInt* const_int32_1;
	ConstantInt* const_int32_2;
	ConstantInt* const_int32_3;
	Constant* const_bool_1;
	Constant* const_bool_0;
	Constant* const_alignment_SIMD;
	
	//alignment information
	unsigned alignmentScalar;
	unsigned alignmentPtr;
	unsigned alignmentSIMDPtr;
	unsigned alignmentSIMD;

	// NOTE: we must not pre-generate this due to possibly different address spaces
	const PointerType* getPointerVectorType(const PointerType* oldType) const {
		return PointerType::get(VectorType::get(oldType->getElementType(), simdWidth), oldType->getAddressSpace());
	}
	
private:

	inline Constant* createPacketConstantInt(const int c) const {
		std::vector<Constant*> cVec; //packet of 'simdWidth' int32
		ConstantInt* const_int32 = ConstantInt::get(getGlobalContext(), APInt(32, c)); //APInt(32,  "c", 2, 10)
		for (unsigned i=0; i<simdWidth; ++i) {
			cVec.push_back(const_int32);
		}
		return ConstantVector::get(ArrayRef<Constant*>(cVec));
	}

	inline Constant* createPacketConstantFloat(const float c) const {
		std::vector<Constant*> fVec; //packet of 'simdWidth' f32
		ConstantFP* const_f32 = ConstantFP::get(getGlobalContext(), APFloat(c));
		for (unsigned i=0; i<simdWidth; ++i) {
			fVec.push_back(const_f32);
		}
		return ConstantVector::get(ArrayRef<Constant*>(fVec));
	}

};

}

#endif	/* _PACKETIZERINFO_HPP */

