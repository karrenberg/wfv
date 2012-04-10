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

// forward declarations
namespace Packetizer {
    class PacketizerInfo;
    static bool typesMatch(Type* t1, Type* t2, const PacketizerInfo& info);
}

using namespace llvm;

class NativeMethods {

private:
    const bool mUseSSE41;
    const bool mUseAVX;
	const bool mVerbose;
    NativeSSEMathFunctions mSSEFuns;
    NativeAVXMathFunctions mAVXFuns;

    typedef std::map<const std::string, const std::string> NatFunctionMapType;
    typedef std::map<const std::string, const int> NatFunctionMaskIndexMapType;
    NatFunctionMapType mNativeFunctions;
    NatFunctionMaskIndexMapType mNativeFunctionMaskIndices;

    //this method holds all valid aliases for builtin functions (could also be added to map...)
    inline Function*
    findNativeFunction(const std::string& scalarName,
                       Module* mod,
                       const unsigned simdWidth) const
    {
#ifndef USE_NATIVE_FUNCTIONS
        return NULL;
#endif
        if (scalarName == "") return NULL;
		else if (scalarName == "sinf" || scalarName == "llvm.sin.f32")
            return mUseAVX ? mAVXFuns.getSinPS(mod, simdWidth) : mSSEFuns.getSinPS(mod, simdWidth);
		else if (scalarName == "cosf" || scalarName == "llvm.cos.f32")
            return mUseAVX ? mAVXFuns.getCosPS(mod, simdWidth) : mSSEFuns.getCosPS(mod, simdWidth);
		else if (scalarName == "sincosf")
            return mUseAVX ? mAVXFuns.getSinCosPS(mod, simdWidth) : mSSEFuns.getSinCosPS(mod, simdWidth);
		else if (scalarName == "logf" || scalarName == "llvm.log.f32")
            return mUseAVX ? mAVXFuns.getLogPS(mod, simdWidth) : mSSEFuns.getLogPS(mod, simdWidth);
		else if (scalarName == "expf" || scalarName == "llvm.exp.f32")
            return mUseAVX ? mAVXFuns.getExpPS(mod, simdWidth) : mSSEFuns.getExpPS(mod, simdWidth);
		else if (scalarName == "log2f" || scalarName == "llvm.log2.f32")
            return mUseAVX ? mAVXFuns.getLog2PS(mod, simdWidth) : mSSEFuns.getLog2PS(mod, simdWidth);
		else if (scalarName == "exp2f" || scalarName == "llvm.exp2.f32")
            return mUseAVX ? mAVXFuns.getExp2PS(mod, simdWidth) : mSSEFuns.getExp2PS(mod, simdWidth);
#ifdef PACKETIZER_USE_NATIVE_POW_PS
		else if (scalarName == "powf" || scalarName == "llvm.pow.f32")
            return mUseAVX ? mAVXFuns.getPowPS(mod, simdWidth) : mSSEFuns.getPowPS(mod, simdWidth);
#endif
		else if (scalarName == "fabsf" || scalarName == "fabs" || scalarName == "llvm.abs.f32")
            return mUseAVX ? mAVXFuns.getAbsPS(mod, simdWidth) : mSSEFuns.getAbsPS(mod, simdWidth);
		else if (scalarName == "rsqrt" || scalarName == "rsqrtf")
            return mUseAVX ? mAVXFuns.getRsqrtPS(mod) : mSSEFuns.getRsqrtPS(mod);
		else if (scalarName == "sqrtf" || scalarName == "llvm.sqrt.f32")
            return mUseAVX ? mAVXFuns.getSqrtPS(mod) : mSSEFuns.getSqrtPS(mod);
		else if ((mUseSSE41 || mUseAVX) &&
                (scalarName == "roundf" || scalarName == "floorf" || scalarName == "ceilf"))
            return mUseAVX ? mAVXFuns.getRoundPS(mod) : mSSEFuns.getRoundPS(mod);
		else if (scalarName == "max" || scalarName == "maxf")
            return mUseAVX ? mAVXFuns.getMaxPS(mod) : mSSEFuns.getMaxPS(mod);
		else if (scalarName == "min" || scalarName == "minf")
            return mUseAVX ? mAVXFuns.getMinPS(mod) : mSSEFuns.getMinPS(mod);
		else if (scalarName == "addsub")
            return mUseAVX ? mAVXFuns.getAddSubPS(mod) : mSSEFuns.getAddSubPS(mod);
		else if (scalarName == "fmodf")
			assert (false && "call to 'fmodf()' should have been replaced before!");

        NatFunctionMapType::const_iterator tmp = mNativeFunctions.find(scalarName);
        if (tmp == mNativeFunctions.end()) return NULL;

		Function* nativeFn = mod->getFunction(tmp->second);
		if (!nativeFn) {
			errs() << "ERROR: native function '" << tmp->second
                << "' not found in module '" << mod->getModuleIdentifier() << "'!\n";
			return NULL;
		}

		return nativeFn;
    }
    inline int findNativeFunctionMaskIndex(const std::string& scalarName) const
    {
#ifndef USE_NATIVE_FUNCTIONS
        return -1;
#endif
        NatFunctionMaskIndexMapType::const_iterator tmp =
            mNativeFunctionMaskIndices.find(scalarName);
        return tmp == mNativeFunctionMaskIndices.end() ? -1 : tmp->second;
    }

    // TODO: Remove, this was just inserted temporarily.
    inline Function*
    getNativeFunction(const std::string& scalarName, Module* mod) const
    {
        return getNativeFunction(scalarName, mod, 4);
    }

public:
    NativeMethods(const bool use_sse41_flag,
                  const bool use_avx_flag,
                  const bool verbose_flag=false)
			: mUseSSE41(use_sse41_flag), mUseAVX(use_avx_flag), mVerbose(verbose_flag)
    {}
	
    ~NativeMethods()
    {
        mNativeFunctions.clear();
        mNativeFunctionMaskIndices.clear();
    }

    inline void
    addNativeFunction(Function* scalarF,
                      Function* nativeF,
                      const int maskIndex)
    {
		assert (nativeF);
		addNativeFunction(scalarF->getName(), nativeF->getNameStr(), maskIndex);
	}


    void
    addNativeFunction(const std::string& scalarName,
                      const std::string& packetName,
                      const int maskIndex)
    {
		//only search exactly matching names to determine if function is already inserted!
        NatFunctionMapType::const_iterator tmp = mNativeFunctions.find(packetName);
        if (tmp != mNativeFunctions.end()) {
            DEBUG_PKT( outs() << "Native function '" << packetName << "' already defined!\n"; );
            return;
        }

        //additionally, do not allow scalarName to be inserted twice
        tmp = mNativeFunctions.find(scalarName);
        if (tmp != mNativeFunctions.end()) {
            DEBUG_PKT( outs() << "Already have a function to replace scalar function '"
                    << scalarName << "'!\n"; );
            return;
        }

        assert (mNativeFunctionMaskIndices.find(packetName) == mNativeFunctionMaskIndices.end());

        mNativeFunctions.insert(std::make_pair(scalarName, packetName));
        //nativeFunctionMaskIndices.insert(std::make_pair(fname, maskIndex)); //maybe add both :P
        mNativeFunctionMaskIndices.insert(std::make_pair(scalarName, maskIndex));
        DEBUG_PKT( outs() << "Added native function '" << packetName
                << "' (mask index " << maskIndex << ", maps to scalar function '"
                << scalarName << "')!\n"; );
    }

    inline Function*
    getNativeFunction(const std::string& scalarName,
                      Module* mod,
                      const unsigned simdWidth) const
    {
        //try to match name
        Function* f = findNativeFunction(scalarName, mod, simdWidth);

        DEBUG_PKT( if (!f) errs() << "No native function found in module '" << mod->getModuleIdentifier() << "' that replaces '" << scalarName << "'!\n"; );

        return f;
    }

    inline int
    getNativeFunctionMaskIndex(const std::string& scalarName) const
    {
        return findNativeFunctionMaskIndex(scalarName);
    }

    bool
    isUniformArg(Function* scalarFn,
                 const unsigned index,
                 const Packetizer::PacketizerInfo& info) const
    {
        assert (index < scalarFn->getArgumentList().size());
        const Function* nativeFn = getNativeFunction(scalarFn->getNameStr(),
                                                     scalarFn->getParent());

        const int maskIndex = getNativeFunctionMaskIndex(scalarFn->getNameStr());

        Function::const_arg_iterator A  = scalarFn->arg_begin();
        Function::const_arg_iterator NA = nativeFn->arg_begin();

        std::advance(A, index);
        std::advance(NA, index);
        if ((int)index >= maskIndex) ++NA;

        return Packetizer::typesMatch(A->getType(), NA->getType(), info);
    }

    inline bool
    isSideEffectFree(Function* f) const
    {
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

