/**
 * @file   packetizerConfig.hpp
 * @date   13.10.2009
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2008, 2009, 2010 Saarland University
 *
 */
#ifndef _PACKETIZERCONFIG_HPP
#define	_PACKETIZERCONFIG_HPP

// variables defined by corresponding compiler
// gcc     = __GNUC__
// icc     = __INTEL_COMPILER
// msvc    = _MSC_VER
// llvm    = __llvm__
// borland = __BORLANDC__

// variables defined by corresponding operating system
// windows (general) = _WIN32
// windows 64bit     = _WIN64
// linux             = __linux
// mac os            = __APPLE__ & __MACH__ (icc & gcc)

// http://predef.sourceforge.net/


//----------------------------------------------------------------------------//
// Packetizer configuration defines
// These should be set by the build script
//----------------------------------------------------------------------------//

// use LLVM 2.4
// (default: deactivated)

//#define LLVM_2_4


// Enable silent mode (no output at all)
// (default: deactivated)

//#define PACKETIZER_SILENT_MODE


// prevent replacing of unsupported constructs (e.g. types i64, double,
// instructions sext/zext/...)
// (default: deactivated)

//#define PACKETIZER_DO_NOT_REPLACE_UNSUPPORTED_CONSTRUCTS //executes transformFunction() which removes all zext/sext/i64/double
#ifdef PACKETIZER_DO_NOT_REPLACE_UNSUPPORTED_CONSTRUCTS
    #ifndef _MSC_VER
        #warning "WARNING: packetizer will not attempt to replace unsupported constructs (e.g. sext/zext/fpext, values of types i64/double, etc.)!"
    #else
        //#pragma WARNING ( packetizer will not attempt to replace unsupported constructs (e.g. sext/zext/fpext, values of types i64/double, etc.)! )
    #endif
#endif


// always execute calls (sin, log, floor, trace, etc.)
// NOTE: long time not tested ;)
// (default: deactivated)

//#define PACKETIZER_NO_PACKETIZED_CALLS
#ifdef PACKETIZER_NO_PACKETIZED_CALLS
    #ifndef _MSC_VER
        #warning "WARNING: *all* calls are always executed 4x scalar instead of packetized!"
    #else
        //#pragma WARNING ( *all* calls are always executed 4x scalar instead of packetized! )
    #endif
#endif


// use coherent mask branching
// NOTE: This can result both in speedups and slow-downs, it is very much depending on the function's CFG.
// (default: activated)

//#define PACKETIZER_DO_NOT_USE_COHERENT_MASK_BRANCHING
#ifdef PACKETIZER_DO_NOT_USE_COHERENT_MASK_BRANCHING
	#ifndef _MSC_VER
        #warning "WARNING: disabled creation of coherent mask branches (BOSCCs)!"
    #else
        //#pragma WARNING ( disabled creation of coherent mask branches (BOSCCs)! )
    #endif
#endif


// use 'old' phi canonicalization
// randomly joins incoming edges
// deactivated joins edge pairs depending on the dominator relation of the
// nearest common dominators of all possible pairs
// (default: deactivated)

//#define PACKETIZER_USE_OLD_PHI_CANONICALIZATION
#if defined PACKETIZER_USE_COHERENT_MASK_BRANCHING && defined PACKETIZER_USE_OLD_PHI_CANONICALIZATION
    #undef PACKETIZER_USE_OLD_PHI_CANONICALIZATION
    #ifndef _MSC_VER
        #warning "WARNING: disabled usage of old phi canonicalization (not supported in combination with uniform mask branching)!"
    #else
        //#pragma WARNING ( disabled usage of old phi canonicalization (not supported in combination with uniform mask branching)! )
    #endif
#endif


// make use of custom powf SSE implementation
// NOTE: some shaders (e.g. c++ phong and whitted) produce artifacts
// (default: deactivated)

//#define PACKETIZER_USE_NATIVE_POW_PS
#if defined PACKETIZER_USE_NATIVE_POW_PS
	#ifndef _MSC_VER
        #warning "WARNING: using native SSE pow implementation, might produce artifacts!"
    #else
        //#pragma WARNING ( using native SSE pow implementation, might produce artifacts! )
    #endif
#endif


// Use scalar instead of SIMD registers for masks.
// This means movmsk-instructions are generated after each comparison and mask
// operations remain scalar i32 instead of being transformed to <W x float>
// EXPERIMENTAL IMPLEMENTATION FOR AVX, NOT FULLY FINISHED!
// (default: deactivated)

//#define PACKETIZER_USE_SCALAR_MASKS
#if defined PACKETIZER_USE_SCALAR_MASKS
	#ifndef _MSC_VER
        #warning "WARNING: using experimental implementation of scalar masks!"
    #else
        //#pragma WARNING ( using experimental implementation of scalar masks! )
    #endif
#endif


// Create dynamic checks for indices of memory operations.
// If an index can not be proven to be consecutive statically, this can help
// us gain some performance by executing an optimized path if the indices are
// consecutive at runtime.
// (default: deactivated)

//#define PACKETIZER_USE_DYNAMIC_CONSECUTIVENESS_CHECKS
#ifdef PACKETIZER_USE_DYNAMIC_CONSECUTIVENESS_CHECKS
	#ifndef _MSC_VER
        #warning "WARNING: enabled creation of dynamic consecutiveness checks!"
    #else
        //#pragma WARNING ( enabled creation of dynamic consecutiveness checks! )
    #endif
#endif


//#define PACKETIZER_FORCE_ALIGNED_MEMOPS
#ifdef PACKETIZER_FORCE_ALIGNED_MEMOPS
	#ifndef _MSC_VER
        #warning "WARNING: forcing aligned memory operations - this is not safe!"
    #else
        //#pragma WARNING ( forcing aligned memory operations - this is not safe! )
    #endif
#endif


//#define PACKETIZER_DISABLE_MEMOP_VECTORIZATION
#ifdef PACKETIZER_DISABLE_MEMOP_VECTORIZATION
	#ifndef _MSC_VER
        #warning "WARNING: disabled vectorization of memory operations!"
        #warning "WARNING: This feature is not fully implemented!"
    #else
        //#pragma WARNING ( disabled vectorization of memory operations! )
        //#pragma WARNING ( This feature is not fully implemented! )
    #endif
#endif


//#define PACKETIZER_DO_NOT_USE_UNIFORM_ANALYSIS
#ifdef PACKETIZER_DO_NOT_USE_UNIFORM_ANALYSIS
	#ifndef _MSC_VER
        #warning "WARNING: disabled optimization of UNIFORM paths!"
    #else
        //#pragma WARNING ( disabled optimization of UNIFORM paths! )
    #endif
#endif


//#define PACKETIZER_DO_NOT_USE_SPLIT_ANALYSIS
#ifdef PACKETIZER_DO_NOT_USE_SPLIT_ANALYSIS
	#ifndef _MSC_VER
        #warning "WARNING: disabled optimization of operations with side-effects!"
        #warning "WARNING: This feature is not fully implemented!"
    #else
        //#pragma WARNING ( disabled optimization of operations with side-effects! )
        //#pragma WARNING ( This feature is not fully implemented! )
    #endif
#endif


//#define PACKETIZER_ERROR_ON_RACE_CONDITION
#ifdef PACKETIZER_ERROR_ON_RACE_CONDITION
	#ifndef _MSC_VER
        #warning "WARNING: race conditions will be treated as errors (experimental)!"
    #else
        //#pragma WARNING ( race conditions will be treated as errors (experimental)! )
    #endif
#endif


//----------------------------------------------------------------------------//
// debug flags
//----------------------------------------------------------------------------//

// DEBUG flag has to be set by compiler
#if defined(DEBUG) || defined(_DEBUG)
    #define DEBUG_PACKETIZER
#endif

//if NDEBUG is defined, always ignore debug information
#ifdef NDEBUG
    #undef DEBUG_PACKETIZER
#endif


// debug macros
// do while and ((void)0) are used to enforce semicolon
// DEBUG_PKT_VISIBLE allows arbitrary code that does not have its own scope
// NOTE: a boolean 'mVerbose' has to be in scope in order to use this ;)
#ifdef DEBUG_PACKETIZER
	#define DEBUG_PKT_NO_VERBOSE(x) do { x } while (0)
	#define DEBUG_PKT(x) do if (mVerbose) { x } while (0)
	#define DEBUG_PKT_VISIBLE(x) x
#else
	#define DEBUG_PKT_NO_VERBOSE(x) ((void)0)
	#define DEBUG_PKT(x) ((void)0)
	#define DEBUG_PKT_VISIBLE(x)
#endif

//----------------------------------------------------------------------------//
// misc settings
//----------------------------------------------------------------------------//

// suppress "controlling expression is constant" warning of icc
#ifdef __INTEL_COMPILER
    #pragma warning( disable : 279 )
#endif



#endif // _PACKETIZERCONFIG_HPP

