/**
 * @file   packetizerAPI_C.hpp
 * @date   14.05.2011
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2011 Saarland University
 *
 */
#ifndef _PACKETIZERAPI_C_H_
#define _PACKETIZERAPI_C_H_
// C bindings for libPacketizer


#include "llvm-c/Core.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct OpaquePacketizer *PKTPacketizerRef;

PKTPacketizerRef 
PKTCreatePacketizer(LLVMModuleRef module, 
                    const unsigned simdWidth, const unsigned packetizationSize,
                    const bool use_sse41, const bool use_avx, 
                    const bool verbose);

bool
PKTAddFunction(PKTPacketizerRef packetizerRef, 
               const char* functionName, const char* simdFunctionName);

bool
PKTAddVaryingFunctionMapping(PKTPacketizerRef packetizerRef,
                     const char* scalarFunctionName, const int maskPosition, 
                     LLVMValueRef packetFunction);

bool
PKTAddUniformVaryingInfo(PKTPacketizerRef packetizerRef,
						 LLVMValueRef value, const bool uniform,
						 const bool consecutive, const bool aligned);

void 
PKTRun(PKTPacketizerRef packetizerRef);

bool
PKTSuccessful(PKTPacketizerRef packetizerRef,
			  const char* simdFunctionName,
			  LLVMModuleRef module);


#ifdef __cplusplus
}

namespace Packetizer {
  class Packetizer;
  
  #define DEFINE_SIMPLE_CONVERSION_FUNCTIONS(ty, ref)   \
    inline ty *unwrap(ref P) {                          \
      return reinterpret_cast<ty*>(P);                  \
    }                                                   \
                                                        \
    inline ref wrap(const ty *P) {                      \
      return reinterpret_cast<ref>(const_cast<ty*>(P)); \
    }
  
  DEFINE_SIMPLE_CONVERSION_FUNCTIONS(Packetizer, PKTPacketizerRef)
  
  #undef DEFINE_SIMPLE_CONVERSION_FUNCTIONS
}
  
#endif /* defined(__cplusplus) */

#endif /* _PACKETIZERAPI_C_H_ */
