/**
 * @file   nativeMethods.hpp
 * @date   07.04.2009
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2009, 2010 Saarland University
 *
 */
#ifndef _NATIVEMETHODS_HPP
#define _NATIVEMETHODS_HPP

#define USE_NATIVE_FUNCTIONS

#include "nativeSSEMathFunctions.hpp"
#include "nativeAVXMathFunctions.hpp"


// the following includes are only required for single-file compilation
#include <map>
#include "packetizerConfig.hpp"


using namespace llvm;

class NativeMethods {

private:
    const bool use_sse41;
    const bool use_avx;
	const bool verbose;
    NativeSSEMathFunctions ssemf;
    NativeAVXMathFunctions avxmf;

    typedef std::map<const std::string, const std::string> NatFunctionMapType;
    typedef std::map<const std::string, const int> NatFunctionMaskIndexMapType;
    NatFunctionMapType nativeFunctions;
    NatFunctionMaskIndexMapType nativeFunctionMaskIndices;

    //this method holds all valid aliases for builtin functions (could also be added to map...)
    inline Function* findNativeFunction(const std::string& scalarName, Module& mod, const unsigned simdWidth) const {
#ifndef USE_NATIVE_FUNCTIONS
        return NULL;
#endif
        if (scalarName == "") return NULL;
		else if (scalarName == "sinf" || scalarName == "llvm.sin.f32") return use_avx ? avxmf.getSinPS(mod, simdWidth) : ssemf.getSinPS(mod, simdWidth);
		else if (scalarName == "cosf" || scalarName == "llvm.cos.f32") return use_avx ? avxmf.getCosPS(mod, simdWidth) : ssemf.getCosPS(mod, simdWidth);
		else if (scalarName == "sincosf") return use_avx ? avxmf.getSinCosPS(mod, simdWidth) : ssemf.getSinCosPS(mod, simdWidth);
		else if (scalarName == "logf" || scalarName == "llvm.log.f32") return use_avx ? avxmf.getLogPS(mod, simdWidth) : ssemf.getLogPS(mod, simdWidth);
		else if (scalarName == "expf" || scalarName == "llvm.exp.f32") return use_avx ? avxmf.getExpPS(mod, simdWidth) : ssemf.getExpPS(mod, simdWidth);
		else if (scalarName == "log2f" || scalarName == "llvm.log2.f32") return use_avx ? avxmf.getLog2PS(mod, simdWidth) : ssemf.getLog2PS(mod, simdWidth);
		else if (scalarName == "exp2f" || scalarName == "llvm.exp2.f32") return use_avx ? avxmf.getExp2PS(mod, simdWidth) : ssemf.getExp2PS(mod, simdWidth);
#ifdef PACKETIZER_USE_NATIVE_POW_PS
		else if (scalarName == "powf" || scalarName == "llvm.pow.f32") return use_avx ? avxmf.getPowPS(mod, simdWidth) : ssemf.getPowPS(mod, simdWidth);
#endif
		else if (scalarName == "fabsf" || scalarName == "fabs" || scalarName == "llvm.abs.f32") return use_avx ? avxmf.getAbsPS(mod, simdWidth) : ssemf.getAbsPS(mod, simdWidth);
		else if (scalarName == "rsqrt" || scalarName == "rsqrtf") return use_avx ? avxmf.getRsqrtPS(mod) : ssemf.getRsqrtPS(mod);
		else if (scalarName == "sqrtf" || scalarName == "llvm.sqrt.f32") return use_avx ? avxmf.getSqrtPS(mod) : ssemf.getSqrtPS(mod);
		else if ((use_sse41 || use_avx) && (scalarName == "roundf" || scalarName == "floorf" || scalarName == "ceilf")) return use_avx ? avxmf.getRoundPS(mod) : ssemf.getRoundPS(mod);
		else if (scalarName == "max" || scalarName == "maxf") return use_avx ? avxmf.getMaxPS(mod) : ssemf.getMaxPS(mod);
		else if (scalarName == "min" || scalarName == "minf") return use_avx ? avxmf.getMinPS(mod) : ssemf.getMinPS(mod);
		else if (scalarName == "addsub") return use_avx ? avxmf.getAddSubPS(mod) : ssemf.getAddSubPS(mod);
		else if (scalarName == "fmodf") {
			assert (false && "call to 'fmodf()' should have been replaced before!");
		}

        NatFunctionMapType::const_iterator tmp = nativeFunctions.find(scalarName);
        if (tmp == nativeFunctions.end()) return NULL;

		Function* nativeFn = mod.getFunction(tmp->second);
		if (!nativeFn) {
			errs() << "ERROR: native function '" << tmp->second << "' not found in module '" << mod.getModuleIdentifier() << "'!\n";
			return NULL;
		}

		return nativeFn;
    }
    inline int findNativeFunctionMaskIndex(const std::string& scalarName) const {
#ifndef USE_NATIVE_FUNCTIONS
        return -1;
#endif
        NatFunctionMaskIndexMapType::const_iterator tmp = nativeFunctionMaskIndices.find(scalarName);
        return tmp == nativeFunctionMaskIndices.end() ? -1 : tmp->second;
    }

public:
    NativeMethods(const bool use_sse41_flag, const bool use_avx_flag, const bool verbose_flag=false)
			: use_sse41(use_sse41_flag), use_avx(use_avx_flag), verbose(verbose_flag) {}
	
    ~NativeMethods() {
        nativeFunctions.clear();
        nativeFunctionMaskIndices.clear();
    }

    inline void addNativeFunction(int maskIndex, Function* nativeF) {
		assert (nativeF);
		addNativeFunction(maskIndex, nativeF->getNameStr());
	}
    void addNativeFunction(int maskIndex, const std::string& packetName) {
        //only search exactly matching names to determine if function is already inserted!
        NatFunctionMapType::const_iterator tmp = nativeFunctions.find(packetName);
        if (tmp != nativeFunctions.end()) {
            DEBUG_PKT( errs() << "WARNING: Native function '" << packetName << "' already defined!\n"; );
            return;
        }

        nativeFunctions.insert(std::make_pair(packetName, packetName));
        nativeFunctionMaskIndices.insert(std::make_pair(packetName, maskIndex));
        DEBUG_PKT( outs() << "Added native function '" << packetName << "'!\n"; );
    }
    inline void addNativeFunction(const std::string& scalarName, const int maskIndex, Function* nativeF) {
		assert (nativeF);
		addNativeFunction(scalarName, maskIndex, nativeF->getNameStr());
	}
    void addNativeFunction(const std::string& scalarName, const int maskIndex, const std::string& packetName) {
		//only search exactly matching names to determine if function is already inserted!
        NatFunctionMapType::const_iterator tmp = nativeFunctions.find(packetName);
        if (tmp != nativeFunctions.end()) {
            DEBUG_PKT( outs() << "Native function '" << packetName << "' already defined!\n"; );
            return;
        }
        //additionally, do not allow scalarName to be inserted twice
        tmp = nativeFunctions.find(scalarName);
        if (tmp != nativeFunctions.end()) {
            DEBUG_PKT( outs() << "Already have a function to replace scalar function '" << scalarName << "'!\n"; );
            return;
        }

        assert (nativeFunctionMaskIndices.find(packetName) == nativeFunctionMaskIndices.end());

        nativeFunctions.insert(std::make_pair(scalarName, packetName));
        //nativeFunctionMaskIndices.insert(std::make_pair(fname, maskIndex)); //maybe add both :P
        nativeFunctionMaskIndices.insert(std::make_pair(scalarName, maskIndex));
        DEBUG_PKT( outs() << "Added native function '" << packetName << "' (mask index " << maskIndex << ", maps to scalar function '" << scalarName << "')!\n"; );
    }

    inline Function* getNativeFunction(const std::string& scalarName, Module& mod, const unsigned simdWidth) const {
        //try to match name
        Function* f = findNativeFunction(scalarName, mod, simdWidth);

        DEBUG_PKT( if (!f) errs() << "No native function found in module '" << mod.getModuleIdentifier() << "' that replaces '" << scalarName << "'!\n"; );

        return f;
    }
    inline int getNativeFunctionMaskIndex(const std::string& scalarName) const {
        return findNativeFunctionMaskIndex(scalarName);
    }

    inline bool isSideEffectFree(Function* f) const {
		assert (!"not implemented!");
        //this can have really bad effects on performance!
//        const std::string& scalarName = f->getNameStr();
//        if (scalarName == "") return false;
//        if (scalarName == "tanf" ||
//                scalarName == "atanf" ||
//                scalarName == "asinf" ||
//                scalarName == "acosf" ||
//                scalarName == "powf" ||
//                scalarName == "llvm.pow.f32")
//            return true;

        return false;
    }
};


#endif  /* _NATIVEMETHODS_HPP */

