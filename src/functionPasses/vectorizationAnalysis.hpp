/**
 * @file   vectorizationAnalysis.hpp
 * @date   13.05.2011
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2011 Saarland University
 *
 */
#ifndef _VECTORIZATIONANALYSIS_HPP
#define	_VECTORIZATIONANALYSIS_HPP

#ifdef DEBUG_TYPE
#undef DEBUG_TYPE
#endif
#define DEBUG_TYPE "vectorizationanalysis"

#include "llvm/Support/raw_ostream.h"

#include "llvm/Pass.h"
#include "llvm/Analysis/LoopInfo.h"

#include "utils/analysisResults.hpp"
#include "utils/llvmTools.hpp"
#include "utils/packetizerInfo.hpp"
#include "utils/nativeMethods.hpp"
#include "loopLiveValueAnalysis.hpp"
#include "branchInfoAnalysis.hpp"
#include "utils/wholeFunctionVectorizationAAW.hpp"

#include <map>
#include <stdexcept>

// the following includes are only required for single-file compilation
#include "packetizerConfig.hpp"

// forward declaration of initializer
namespace llvm {
	void initializeVectorizationAnalysisPass(PassRegistry&);
}

using namespace llvm;

// only required for stupid dummy constructor (requires initialization of all fields)
//static Module dummyModule("dummy", getGlobalContext());

namespace { // unnamed namespace prevents correct linking against packetizerAPI

class VectorizationAnalysis : public FunctionPass {
public:
	static char ID; // Pass identification, replacement for typeid

	VectorizationAnalysis()
	: FunctionPass(ID),
			info(Packetizer::PacketizerInfo(dummyModule, 0, 0, false, false, true)),
			nativeMethods(NativeMethods(false, false, true)),
			source(*Function::Create(NULL, GlobalValue::ExternalLinkage, "", &dummyModule)),
			target(*Function::Create(NULL, GlobalValue::ExternalLinkage, "", &dummyModule)),
			nativeFunctions(WholeFunctionVectorizer::NativeFunctionMapType()),
			userValueInfoMap(WholeFunctionVectorizer::ValueInfoMapType()),
			failed(NULL),
			verbose(true),
			deleteResults(true),
			analysisResults(NULL)
	{
		errs() << "ERROR: empty constructor of class VectorizationAnalysis should never be called!\n";
	}

	VectorizationAnalysis(const Packetizer::PacketizerInfo& _info,
						  NativeMethods& _nativeMethods,
						  Function& sourceFunction,
						  Function& targetFunction,
						  const WholeFunctionVectorizer::NativeFunctionMapType& _nativeFunctions,
						  const WholeFunctionVectorizer::ValueInfoMapType& _userValueInfoMap,
						  bool* failedFlag,
						  const bool verbose_flag=false,
						  AnalysisResults* res=NULL) // if NULL, result is destroyed when analysis object is destroyed
						  
	: FunctionPass(ID),
			info(_info),
			nativeMethods(_nativeMethods),
			source(sourceFunction),
			target(targetFunction),
			nativeFunctions(_nativeFunctions),
			userValueInfoMap(_userValueInfoMap),
			failed(failedFlag),
			verbose(verbose_flag),
			deleteResults(res == NULL),
			analysisResults(res)
	{
		initializeVectorizationAnalysisPass(*PassRegistry::getPassRegistry());
	}

	~VectorizationAnalysis() {}

	virtual void releaseMemory() {
		// If the results object was supplied from outside (= results are still
		// used after passes finished and PassManager is destroyed), we only
		// need to set loop info to NULL (loop info is not available anymore
		// after PassManager is destroyed -> all public code that uses loopInfo
		// has to take this into account)
		// Otherwise, we delete the results object.
		analysisResults->setLoopInfo(NULL);
		if (deleteResults) delete analysisResults;
	}

	virtual void getAnalysisUsage(AnalysisUsage &AU) const {
		AU.addRequired<PostDominatorTree>();
		AU.addRequired<LoopInfo>();
		AU.addRequired<BranchInfoAnalysis>();
		AU.addRequired<LoopLiveValueAnalysis>();

		AU.setPreservesAll();
	}

	AnalysisResults* getAnalysisResults() { return analysisResults; }
	BranchInfoAnalysis::BranchMapType* getBranchMap() { return branchMap; }

	void print(raw_ostream &O, const Module* M=NULL) const
	{
		analysisResults->print(O, M);
	}


	// recursively collect uses of uniform parameters
	// NOTE: We assume that it has been checked whether the function signatures
	//       match and fulfill vectorization requirements
	// TODO: Throw ERROR when uniform return is marked as varying
	// TODO: Uniform loop induction variable that is used in VARYING instruction:
	//       Better use vector register instead of broadcasting every iteration?
	// TODO: Do we actually require BranchInfo or would PDT do the job here?
	virtual bool runOnFunction(Function& F) {
		assert (&F == &source);

		if (failed && *failed) return true;

		loopInfo = &getAnalysis<LoopInfo>();
		loopLiveValueAnalysis = &getAnalysis<LoopLiveValueAnalysis>();
		postDomTree = &getAnalysis<PostDominatorTree>();
		branchMap = &getAnalysis<BranchInfoAnalysis>().getBranchMap();

		// If the results object was supplied from outside (= results are still
		// used after passes finished and PassManager is destroyed), we only
		// need to set the loop info.
		// Otherwise, we have to create a new results object.
		if (!analysisResults) analysisResults = new AnalysisResults(loopInfo, verbose);
		else analysisResults->setLoopInfo(loopInfo);

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
				<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "analyzing function '" << source.getNameStr() <<
				"' for uniform, consecutive, and aligned values\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
				<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );

		try {
			// find out which arguments are uniform and which are varying
			findUniformArguments(source, target, uniformArgVec);

			analyzeUniformInfo();

			analyzeConsecutiveAlignedInfo();

			analyzeSplitInfo(nativeMethods, info);

			if (source.getName().equals("__OpenCL_mandelbrot_kernel.tmp")) {
				DEBUG_PKT( outs() << "mandelbrot - attempting to find varying instructions that can remain scalar...\n"; );
				findVaryingInstructionsThatCanRemainScalar(); // execute BEFORE marking of masks!!
			}

			analyzeMaskInfo();

			if (detectRaceConditions(info)) {
#ifdef PACKETIZER_ERROR_ON_RACE_CONDITION
				throw std::logic_error("ERROR: RACE CONDITION DETECTED!\nStore operation attempts to write different values to the same location!");
#endif
			}
		}
		catch (std::logic_error& error) {
			errs() << "\nException occurred during vectorization analysis: "
					<< error.what() << "\n";
			if (failed) *failed = true;
			return true;
		}
		catch (...) {
			errs() << "\nINTERNAL ERROR: Unexpected exception occurred during "
					<< "vectorization analysis!\n";
			if (failed) *failed = true;
			return true;
		}


		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
				<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "vectorization analysis finished.\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
				<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );

		DEBUG_PKT( source.print(outs(), new WholeFunctionVectorizationAAW(*analysisResults)); );
		assert (analysisResults->verify(source) && "verification of vectorization analysis failed!");

		return false; // function was not changed
	}

	bool hasNonUniformIndex(GetElementPtrInst* gep) {
		assert (gep);

		for (GetElementPtrInst::op_iterator IDX=gep->idx_begin(),
				IDXE=gep->idx_end(); IDX!=IDXE; ++IDX)
		{
			assert (isa<Value>(IDX));
			if (!isa<Constant>(IDX) && !analysisResults->isUniform(cast<Value>(IDX))) return true;
		}

		return true;
	}

	// This function defines what we consider matching types
	// in terms of uniform/varying.
	inline bool typesMatch(const Type* t1, const Type* t2)
	{
		assert (t1 && t2);
		if (t1 == t2) return true;
		// TODO: add special cases
		// e.g. <2 x i64> matches <4 x i32>

		return false;
	}

	// find out which parameters of scalarFun remain scalar in vectorFun
	void findUniformArguments(
			const Function& scalarFun,
			const Function& vectorFun,
			std::vector<bool>& uniformArgs)
	{
		uniformArgs.resize(scalarFun.getArgumentList().size());
		unsigned i = 0;
		Function::const_arg_iterator A_SCALAR = scalarFun.arg_begin();
		for (Function::const_arg_iterator A = vectorFun.arg_begin(),
				AE = vectorFun.arg_end(); A != AE; ++A, ++A_SCALAR, ++i) {
			uniformArgs[i] = typesMatch(A_SCALAR->getType(), A->getType());
		}
	}

	
private:
	const Packetizer::PacketizerInfo& info;
	NativeMethods nativeMethods;
	Function& source;
	Function& target;
	const WholeFunctionVectorizer::NativeFunctionMapType& nativeFunctions;
	const WholeFunctionVectorizer::ValueInfoMapType& userValueInfoMap;
	LoopInfo* loopInfo;
	LoopLiveValueAnalysis* loopLiveValueAnalysis;
	PostDominatorTree* postDomTree;
	BranchInfoAnalysis::BranchMapType* branchMap;

	bool* failed;
	const bool verbose;
	const bool deleteResults;         // set to "false" if results are still required after this object is destroyed
	AnalysisResults* analysisResults; // stores all relevant results

	std::vector<bool> uniformArgVec;

	std::set<const Instruction*> scalarSet; // TODO: is that still used? :p


	typedef AnalysisResults::UniformInfo   UniformInfo;
	typedef AnalysisResults::IndexInfo     IndexInfo;
	typedef AnalysisResults::AlignmentInfo AlignmentInfo;
	typedef AnalysisResults::SplitInfo     SplitInfo;

	// TODO: create class with classes with methods "add", "remove", "get"
	typedef AnalysisResults::ValueInfo ValueInfo;
	typedef AnalysisResults::BlockInfo BlockInfo;
	typedef AnalysisResults::UniformLoopInfo UniformLoopInfo;


	////////////////////////////////////////////////////////////////////////////
	//                  UNIFORM/VARYING INFORMATION ANALYSIS                  //
	////////////////////////////////////////////////////////////////////////////

	void analyzeUniformInfo() {

		// Each time a value, block, or loop is marked VARYING, it is removed from the corresponding set.
		std::set<Value*> uniformValues;
		std::set<BasicBlock*> uniformBlocks;
		std::set<const Loop*> uniformLoops;

		// initialize sets
		for (Function::iterator BB=source.begin(), BBE=source.end(); BB!=BBE; ++BB) {
			for (BasicBlock::iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {
				uniformValues.insert(I);
			}
			uniformBlocks.insert(BB);
			const Loop* loop = loopInfo->getLoopFor(BB);
			if (loop && loop->getHeader() == BB) {
				uniformLoops.insert(loop);
			}
		}
		// also add arguments
		for (Function::arg_iterator A = source.arg_begin(),
				AE = source.arg_end(); A != AE; ++A)
		{
			uniformValues.insert(A);
		}


		// Mark instructions that depend on varying arguments.
		// This does not have to be repeated every iteration.
		unsigned i=0;
		for (Function::arg_iterator A=source.arg_begin(),
				AE=source.arg_end(); A!=AE; ++A, ++i)
		{
			Value* argVal = cast<Value>(A);
			if (uniformArgVec[i]) continue;
			DEBUG_PKT( outs() << "\nmarking argument: " << *A << " as VARYING...\n"; );
			markVaryingValue(argVal, uniformValues);
		}

		// Mark instructions that are defined as VARYING by the user via
		// addValueInfo() as well as their uses.
		for (WholeFunctionVectorizer::ValueInfoMapType::const_iterator
				it=userValueInfoMap.begin(),
				E=userValueInfoMap.end(); it!=E; ++it)
		{
			if (it->second->uniform) continue; // ignore uniform values

			// ignore instructions of other functions
			if (const Instruction* inst = dyn_cast<Instruction>(it->first)) {
				if (inst->getParent()->getParent() != &source) continue;
			}
			// ignore arguments of other functions
			if (const Argument* arg = dyn_cast<Argument>(it->first)) {
				if (arg->getParent() != &source) continue;
			}

			DEBUG_PKT( outs() << "\nmarking user-defined varying value: "
					<< *it->first << " as VARYING...\n"; );
			markVaryingValue(it->first, uniformValues);
		}

#ifdef PACKETIZER_DO_NOT_USE_UNIFORM_ANALYSIS
//		// mark all values as VARYING
//		for (Function::iterator BB=source.begin(), BBE=source.end(); BB!=BBE; ++BB) {
//			for (BasicBlock::iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {
//				// ignore values marked as UNIFORM by user
//				WholeFunctionVectorizer::ValueInfoMapType::const_iterator
//						tmp = userValueInfoMap.find(I);
//				if (tmp != userValueInfoMap.end() && tmp->second->uniform) continue;
//
//				markVaryingValue(I, uniformValues);
//			}
//		}

		// mark all branches as VARYING (= force linearization of all control flow)
		for (Function::iterator BB=source.begin(), BBE=source.end(); BB!=BBE; ++BB) {
			if (!isa<BranchInst>(BB->getTerminator())) continue;
			if (cast<BranchInst>(BB->getTerminator())->isUnconditional()) continue;
			markVaryingValue(BB->getTerminator(), uniformValues);
		}
#endif

		// Collect all instructions that do not depend on any argument.
		// That should only be:
		// - calls
		// - loads
		// - induction variables
		// - return (without value or returning an induction variable)
		// - branches (unconditional or dependent on induction variable)
		std::set<Instruction*> argDependentInsts;
		for (Function::arg_iterator A=source.arg_begin(),
				AE=source.arg_end(); A!=AE; ++A, ++i)
		{
			for (Argument::use_iterator U=A->use_begin(), UE=A->use_end(); U!=UE; ++U) {
				assert (isa<Instruction>(*U));
				markArgDependent(cast<Instruction>(*U), argDependentInsts);
			}
		}

		// Mark input-independent loads and all calls that are not defined as
		// UNIFORM by the user via addValueInfo().
		// This does not have to be repeated every iteration.
		for (Function::iterator BB=source.begin(), BBE=source.end(); BB!=BBE; ++BB) {
			for (BasicBlock::iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {
				// ignore this instruction if it depends on any argument
				if (argDependentInsts.find(I) != argDependentInsts.end()) continue;

				if (isa<TerminatorInst>(I)) continue;

				// ignore values marked as UNIFORM by user
				WholeFunctionVectorizer::ValueInfoMapType::const_iterator
						tmp = userValueInfoMap.find(I);
				if (tmp != userValueInfoMap.end() && tmp->second->uniform) continue;

				// If it is no call or load, it is an induction variable, which
				// is UNIFORM by definition if independent of arguments, unless it
				// lives out of a VARYING loop.
				if (!isa<CallInst>(I) && !isa<LoadInst>(I)) {
					DEBUG_PKT( errs() << "\nWARNING: Input-independent value found"
							<< " that is neither load nor call: " << *I << "\n"; );
					DEBUG_PKT( errs() << "         Make sure this value is "
							<< "related to an induction variable!\n"; );
					analysisResults->addInputIndependentValue(I, false);
					continue;
				}

				// If this is a call to a native function marked UNIFORM,
				// do not mark it as VARYING.
				if (CallInst* call = dyn_cast<CallInst>(I)) {
					Function* callee = call->getCalledFunction();
					WholeFunctionVectorizer::NativeFunctionMapType::const_iterator
						it = nativeFunctions.find(callee);
					if (it != nativeFunctions.end() &&
							it->second->uniform)
					{
						DEBUG_PKT( outs() << "\ntarget function (" << callee->getName()
								<< ") of input-independend call: " << *call
								<< " is marked as UNIFORM by user - ignored!\n"; );
						continue;
					}
				}

				// If this is a load that was defined as UNIFORM, ignore it
				if (LoadInst* load = dyn_cast<LoadInst>(I)) {
					WholeFunctionVectorizer::ValueInfoMapType::const_iterator
						it = userValueInfoMap.find(load);
					if (it != userValueInfoMap.end() &&
							it->second->uniform)
					{
						DEBUG_PKT( outs() << "\ninput-independend load: " << *load
								<< " is marked as UNIFORM by user - ignored!\n"; );
						continue;
					}
				}

				DEBUG_PKT( outs() << "\nmarking uses of input-independent "
						<< (isa<CallInst>(I) ? "call" : "load") << ": "
						<< *I << " as VARYING...\n"; );

				markVaryingValue(I, uniformValues);
			}
		}

		//
		// Fixpoint iteration
		//

		DEBUG_PKT_VISIBLE( unsigned iteration = 0; );

		while (true) {

			bool changed = false;

			// mark loops
			DEBUG_PKT ( outs() << "\nmarking uniform/varying loops...\n"; );
			for (LoopInfo::iterator L=loopInfo->begin(), LE=loopInfo->end(); L!=LE; ++L) {
				changed |= markLoop(*L, uniformValues, uniformLoops);
			}
			DEBUG_PKT( outs() << "finished marking of uniform/varying loops!\n"; );

			// After having information about all loops and values,
			// make sure to mark all values live across loop boundaries of varying
			// loops as VARYING.
			DEBUG_PKT ( outs() << "\nmarking values live across loop boundaries "
					<< "of VARYING loops...\n"; );
			for (LoopInfo::iterator L=loopInfo->begin(), LE=loopInfo->end(); L!=LE; ++L) {
				changed |= markLiveAcrossLoopBoundaryValues(*L, uniformValues, uniformLoops);
			}
			DEBUG_PKT( outs() << "finished marking of values live across loop "
					<< "boundaries of VARYING loops!\n"; );

			// mark blocks
			DEBUG_PKT ( outs() << "\nmarking uniform/varying blocks...\n\n"; );
			BasicBlock* entryBB = &source.getEntryBlock();
			std::set<BasicBlock*> finishedSet;
			const bool uniform = true;
			changed |= markBlock(entryBB, uniform, true, uniformValues, uniformLoops, uniformBlocks, finishedSet);
			DEBUG_PKT ( outs() << "\nfinished marking of uniform/varying blocks!\n"; );

			// mark phi-instructions in varying non-loop-header-blocks as VARYING
			for (Function::iterator BB=source.begin(), BBE=source.end(); BB!=BBE; ++BB) {
				if (loopInfo->isLoopHeader(BB)) continue;
				if (uniformBlocks.find(BB) != uniformBlocks.end()) continue;

				for (BasicBlock::iterator I=BB->begin(), IE=BB->getFirstNonPHI(); I!=IE; ++I) {
					assert (isa<PHINode>(I));
					DEBUG_PKT( outs() << "\nupdating phi in VARYING "
							<< "non-loop-header block '" << BB->getName()
							<< "': " << *I << " as VARYING...\n"; );
					changed |= markVaryingValue(I, uniformValues);
				}
			}

			// After having complete information on uniform/varying control-
			// flow, there is one more special case to cover:
			// A phi node must not be marked UNIFORM if it has incoming values
			// that depend on VARYING control-flow. These are phi nodes that
			// will later be transformed into vector-selects due to the VARYING
			// mask, but we have to make sure that their uses are also correctly
			// marked as VARYING.
			// This case occurrs if any predecessor block of the phi's parent
			// block has a VARYING entry, which means the select that the phi
			// will be transformed to (because control-flow will be linearized)
			// will have a VARYING condition.
			for (Function::iterator BB=source.begin(), BBE=source.end(); BB!=BBE; ++BB) {
				if (loopInfo->isLoopHeader(BB)) continue;
				if (BB->getUniquePredecessor()) continue;
				if (&source.getEntryBlock() == BB) continue;

				typedef GraphTraits<Inverse<BasicBlock*> > InvBlockTraits;
				InvBlockTraits::ChildIteratorType PI = InvBlockTraits::child_begin(BB);
				BasicBlock* pred0BB = *PI++;
				assert (PI != InvBlockTraits::child_end(BB) && "block must not have less than 2 predecessors!");
				BasicBlock* pred1BB = *PI;
				assert (++PI == InvBlockTraits::child_end(BB) && "block must not have more than 2 predecessors!");

				if (uniformBlocks.find(pred0BB) != uniformBlocks.end() &&
						uniformBlocks.find(pred1BB) != uniformBlocks.end()) continue;

				for (BasicBlock::iterator I=BB->begin(), IE=BB->getFirstNonPHI(); I!=IE; ++I) {
					assert (isa<PHINode>(I));
					DEBUG_PKT( outs() << "\nupdating phi in non-loop-header "
							<< "block '" << BB->getName()
							<< "': " << *I << " as VARYING (predecessors have "
							<< "VARYING entries)...\n"; );
					changed |= markVaryingValue(I, uniformValues);
				}
			}


			DEBUG_PKT ( outs() << "\nFixpoint iteration #" << iteration++ << " finished!\n"; );

			// if no block or loop was marked, stop fixpoint iteration
			if (!changed) break;

		}

		DEBUG_PKT(
			outs() << "\nUniform Values:\n";
			for (std::set<Value*>::iterator it=uniformValues.begin(), E=uniformValues.end(); it!=E; ++it) {
				outs() << "  " << **it << "\n";
			}
			outs() << "\nUniform Blocks:\n";
			for (std::set<BasicBlock*>::iterator it=uniformBlocks.begin(), E=uniformBlocks.end(); it!=E; ++it) {
				outs() << "  " << (*it)->getName() << "\n";
			}
			outs() << "\nUniform Loops:\n";
			for (std::set<const Loop*>::iterator it=uniformLoops.begin(), E=uniformLoops.end(); it!=E; ++it) {
				outs() << "  " << (*it)->getHeader()->getName() << "\n";
			}
			outs() << "\n\n";
		);

		//
		// Now store the actual information in the various maps
		//

		// add argument info
		unsigned j=0;
		for (Function::arg_iterator A=source.arg_begin(),
				AE=source.arg_end(); A!=AE; ++A, ++j)
		{
			const bool isUniform = uniformArgVec[j];
			analysisResults->addValueInfo(A, isUniform ? AnalysisResults::UNIFORM : AnalysisResults::VARYING);
		}

		for (Function::iterator BB=source.begin(), BBE=source.end(); BB!=BBE; ++BB) {

			// add instruction info
			for (BasicBlock::iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {
				const bool isUniform = uniformValues.find(I) != uniformValues.end();
				analysisResults->addValueInfo(I, isUniform ? AnalysisResults::UNIFORM : AnalysisResults::VARYING);

				// add possible constant operands
				for (Instruction::op_iterator O=I->op_begin(), OE=I->op_end(); O!=OE; ++O) {
					if (!isa<Constant>(O)) continue;
					if (isa<BasicBlock>(O)) continue;
					if (isa<Function>(O)) continue; // apparently a function-operand in a call is a constant, which can screw up things badly :D
					// Make sure we do not add the same constant more than once
					if (analysisResults->hasValueInfo(cast<Value>(O))) continue;
					analysisResults->addValueInfo(cast<Value>(O), AnalysisResults::UNIFORM, AnalysisResults::INDEX_SAME, AnalysisResults::ALIGN_TRUE, AnalysisResults::SPLIT_NEVER);
					DEBUG_PKT( outs() << "marked constant: " << *cast<Value>(O)
							<< " as UNIFORM / INDEX_SAME / ALIGN_TRUE / SPLIT_NEVER!\n"; );
				}
			}

			// add block info
			const bool blockIsUniform    = uniformBlocks.find(BB) != uniformBlocks.end();
			const bool uniformEntry      = blockIsUniform;
			const bool uniformExit       = uniformValues.find(BB->getTerminator()) != uniformValues.end();
			const bool fullyUniformEntry = false; // derived later
			const bool fullyUniformExit  = false; // derived later
			analysisResults->addBlockInfo(BB,
										 uniformEntry,
										 fullyUniformEntry,
										 uniformExit,
										 fullyUniformExit);

			// add loop info
			const Loop* loop = loopInfo->getLoopFor(BB);
			if (loop && loop->getHeader() == BB) {
				const bool isUniform = uniformLoops.find(loop) != uniformLoops.end();
				const bool isFullyUniform = false; // derived later
				analysisResults->addUniformLoopInfo(loop, isUniform, isFullyUniform);
			}

		}

		// mark FULLY_UNIFORM blocks
		DEBUG_PKT ( outs() << "\nmarking FULLY_UNIFORM blocks and loops...\n"; );
		BasicBlock* entryBB = &source.getEntryBlock();
		std::set<BasicBlock*> visitedSet;
		markFullyUniform(entryBB, visitedSet);
		DEBUG_PKT ( outs() << "\nfinished marking of FULLY_UNIFORM blocks and loops!\n"; );


		// Derive information about nested "top level" varying loops.
		// A nested "top level" varying loop is a varying loop either at the
		// "real" top level or inside a uniform loop.
		DEBUG_PKT ( outs() << "\nmarking nested varying \"top level\" loops..."; );
		collectAllNestedVaryingTopLevelLoops();
		DEBUG_PKT ( outs() << "done.\n\n"; );
	}

	void markInputIndependentUsesVaryingDueToVaryingLoop(const Instruction* value) {
		assert (value);

		for (Instruction::const_use_iterator U=value->use_begin(), UE=value->use_end(); U!=UE; ++U) {
			assert (isa<Instruction>(*U));
			const Instruction* useI = cast<Instruction>(*U);

			if (!analysisResults->isInputIndependent(useI)) continue;
			if (analysisResults->isVaryingDueToVaryingLoop(useI)) continue; // already marked

			outs() << "      input-independent use of value marked VARYING_DUE_TO_VARYING_LOOP: " << *useI << " - can not be kept scalar (marked as VARYING_DUE_TO_VARYING_LOOP)!\n";
			analysisResults->setIsVaryingDueToVaryingLoop(useI, true);

			markInputIndependentUsesVaryingDueToVaryingLoop(useI);
		}
	}

	inline bool operandUsesValue(const Instruction* inst, const Value* value) {
		assert (inst && value);
		for (Instruction::const_op_iterator O=inst->op_begin(), OE=inst->op_end(); O!=OE; ++O) {
			const Value* opVal = cast<Value>(*O);
			if (value == opVal) return true;
		}
		return false;
	}


	void markArgDependent(Instruction* inst, std::set<Instruction*>& argDependentInsts) {
		assert (inst);
		if (argDependentInsts.find(inst) != argDependentInsts.end()) return;
		argDependentInsts.insert(inst);

		for (Instruction::use_iterator U = inst->use_begin(), UE = inst->use_end(); U != UE; ++U) {
			assert (isa<Instruction>(*U));
			Instruction* useI = cast<Instruction>(*U);
			markArgDependent(useI, argDependentInsts);
		}
	}

	// Returns true if some mark was set, false otherwise.
	// This is guaranteed to terminate, as each value is removed from the
	// set before recursing.
	bool markVaryingValue(Value* value, std::set<Value*>& uniformValues) {
		assert (value);

		DEBUG_PKT( outs() << "  marking value: " << *value << " as VARYING...\n"; );

		std::set<Value*>::iterator it = uniformValues.find(value);
		if (it == uniformValues.end()) {
			DEBUG_PKT( outs() << "    previously marked as VARYING - ignored!\n"; );
			return false; // is already marked as VARYING
		}

		uniformValues.erase(it); // mark as VARYING

		// recurse into uses (DFS)
		for (Value::use_iterator U = value->use_begin(), UE = value->use_end(); U != UE; ++U) {
			Value* useVal = cast<Value>(*U);
			markVaryingValue(useVal, uniformValues);
		}

		return true;
	}

	// Returns true if some mark was set, false otherwise.
	bool markLoop(const Loop* loop, std::set<Value*>& uniformValues, std::set<const Loop*>& uniformLoops) {
		assert (loop);

		for (Loop::iterator SL=loop->begin(); SL != loop->end(); ++SL) {
			markLoop(*SL, uniformValues, uniformLoops);
		}

		std::set<const Loop*>::iterator it = uniformLoops.find(loop);
		if (it == uniformLoops.end()) return false; // is already marked as VARYING

		SmallVector<BasicBlock*, 4> exitingBlocks;
		loop->getExitingBlocks(exitingBlocks);

		bool onlyUniformExitingBranches = true;
		for (SmallVector<BasicBlock*, 4>::const_iterator it=exitingBlocks.begin();
				it!=exitingBlocks.end(); ++it)
		{
			BasicBlock* curExitBB = *it;
			TerminatorInst* term = curExitBB->getTerminator();
			const bool isUniform = uniformValues.find(term) != uniformValues.end();
			onlyUniformExitingBranches &= isUniform;
			if (!onlyUniformExitingBranches) break;
		}

		// mark as VARYING if any exit is not UNIFORM
		if (!onlyUniformExitingBranches) uniformLoops.erase(it);

		return !onlyUniformExitingBranches;
	}

	bool markBlock(BasicBlock* block,
				   const bool uniform,
				   const bool forceMark,
				   std::set<Value*>& uniformValues,
				   std::set<const Loop*>& uniformLoops,
				   std::set<BasicBlock*>& uniformBlocks,
				   std::set<BasicBlock*>& finishedSet)
	{
		assert (block);

		if (finishedSet.find(block) != finishedSet.end()) {
			DEBUG_PKT( outs() << "block '" << block->getName() << "' already finished - ignored!\n"; );
			return false; // already seen, nothing changed
		}

		bool changed = false;

		//
		// mark/update this block
		//

		DEBUG_PKT( outs() << "marking block '" << block->getName() << "' as "
				<< (uniform ? "UNIFORM_ENTRY, " : "VARYING_ENTRY, ")
				<< ((uniformValues.find(block->getTerminator()) != uniformValues.end()) ?
					"UNIFORM_EXIT\n" : "VARYING_EXIT\n"); );

		const Loop* loop = loopInfo->getLoopFor(block);
		const bool isLoopHeader = loop && (loopInfo->isLoopHeader(block));
		const bool loopIsUniform = loop && uniformLoops.find(loop) != uniformLoops.end();

		std::set<BasicBlock*>::iterator it = uniformBlocks.find(block);
		if (it != uniformBlocks.end()) {
			// Block is still marked UNIFORM, see if that is okay.
			const bool blockExitsVaryingLoop = loop && !loopIsUniform && loop->isLoopExiting(block);

			// If this block exits a VARYING loop, make sure the conditional
			// branch is also marked VARYING.
			// NOTE: This might later lead to more updates.
			if (blockExitsVaryingLoop) {
				DEBUG_PKT( outs() << "  is exiting block of VARYING loop - marking terminator as VARYING...\n"; );
				changed |= markVaryingValue(block->getTerminator(), uniformValues);
			}

			// If this block is the header of a loop, it is marked the same as
			// its loop (i.e. we discard the value of the parameter 'uniform').
			if (isLoopHeader) {

				if (!loopIsUniform) {
					uniformBlocks.erase(it);
					changed = true;
					DEBUG_PKT( outs() << "  is loop header of VARYING loop - marked VARYING!\n"; );
				} else {
					DEBUG_PKT( outs() << "  is loop header of UNIFORM loop!\n"; );
				}

			} else  {

				// Block is no loop header and still marked UNIFORM,
				// so we update the mark according to the 'uniform' parameter.
				if (!uniform) {
					uniformBlocks.erase(it);
					changed = true;
					DEBUG_PKT( outs() << "  has VARYING entry - marked VARYING!\n"; );
				}

			}
		}

		//
		// Wait untill all predecessors of this block are marked.
		// This ensures that we do not have to visit a block multiple times
		// if it has predecessors with different entry marks.
		// Exceptions are loop headers (only one "true" predecessor), and if
		// 'forceMark' is set, which means we are currently marking a post-
		//  dominator where we are sure this is the correct mark.
		//

		if (!isLoopHeader && !forceMark) {
			typedef GraphTraits<Inverse<BasicBlock*> > InvBlockTraits;
			for (InvBlockTraits::ChildIteratorType PI = InvBlockTraits::child_begin(block),
					PE = InvBlockTraits::child_end(block); PI != PE; ++PI)
			{
				BasicBlock* predBB = *PI;
				if (finishedSet.find(predBB) == finishedSet.end()) {
					DEBUG_PKT( outs() << "  predecessor '"
							<< predBB->getName() << "' not visited - marking of "
							<< "successors delayed!\n"; );
					return changed;
				}
			}
		}

		DEBUG_PKT( outs() << "marking of predecessors of block '"
				<< block->getName() << "' finished, final mark: "
				<< (uniform ? "UNIFORM_ENTRY, " : "VARYING_ENTRY, ")
				<< ((uniformValues.find(block->getTerminator()) != uniformValues.end()) ?
					"UNIFORM_EXIT\n" : "VARYING_EXIT\n"); );

		// Mark block as finished.
		finishedSet.insert(block);

		// Before going on, recompute the most recent information for this block.
		const bool stillUniform = uniformBlocks.find(block) != uniformBlocks.end();

		//
		// Block and all predecessors are marked (except if loop header),
		// now mark successors, post dominators, etc.
		//

		//
		// If loop header: mark common post dominator of all exit blocks and exit blocks of loop
		// NOTE: This has to be done before marking the direct exits of conditional exits.
		//       Otherwise, a unique, UNIFORM exit block of a VARYING loop is marked VARYING
		//       because of the VARYING exit condition of the block.
		//

		if (isLoopHeader) {
			// First, we set the entry mark of the common post
			// dominator of all exits to the current mark.
			// If we don't, we can lose information, e.g. marking all exit
			// blocks and successors as VARYING if the loop is VARYING
			// although the control-flow was FULLY_UNIFORM when entering
			// the loop.
			SmallVector<BasicBlock*, 4> exitBlocks;
			loop->getUniqueExitBlocks(exitBlocks);
			// The post dominator of A and B is A if A post-dominates B,
			// so this here computes the common post dominator of all exits.
			BasicBlock* postDomBlock = *exitBlocks.begin();
			for (SmallVector<BasicBlock*, 4>::iterator it2=exitBlocks.begin()+1,
					E=exitBlocks.end(); it2!=E; ++it2)
			{
				postDomBlock = postDomTree->findNearestCommonDominator(postDomBlock, *it2);
			}
			DEBUG_PKT ( outs() << "  recursing into common post dominator of"
					<< " loop exits: " << postDomBlock->getName()
					<< " - marking as " << (stillUniform ? "UNIFORM" : "VARYING") << "...\n"; );

			// The post dominator has to be marked with 'uniform' instead of
			//'stillUniform' to mark it according to the incoming mark of the
			// loop instead of the "loop exit mark".
			changed |= markBlock(postDomBlock, uniform, true, uniformValues, uniformLoops, uniformBlocks, finishedSet);

			// Now mark all exiting blocks depending on the loop's uniform info.
			// NOTE: This means that we possibly mark exit edges as VARYING
			//       although the corresponding branch is actually UNIFORM.
			//       It might be possible to even exploit such cases (multiple
			//       exits of which some are uniform and some varying), but this
			//       looks too complicated right now.
			// The last conditional branch block for the exit blocks is the header
			// (= the current block) if the loop is uniform, the current last
			// conditional branch block otherwise.
			// NOTE: This does not influence cases of VARYING loops with only
			//       one exit. In this case, the above call to markBlock already
			//       sets a permanent mark with the correct label from "above".
			DEBUG_PKT ( outs() << "  recursing into exit blocks of "
				<< (loopIsUniform ? "UNIFORM" : "VARYING") << " loop - marking as "
				<< (loopIsUniform ? "UNIFORM" : "VARYING") << "...\n"; );

			for (SmallVector<BasicBlock*, 4>::iterator it2=exitBlocks.begin();
					it2!=exitBlocks.end(); ++it2)
			{
				BasicBlock* curExitBB = *it2;
				assert (!loopInfo->getLoopFor(curExitBB) || loopInfo->getLoopFor(curExitBB) != loop);
				changed |= markBlock(curExitBB, loopIsUniform, true, uniformValues, uniformLoops, uniformBlocks, finishedSet);
			}

		}

		//
		// if conditional exit: mark post dominator block
		//

		TerminatorInst* term = block->getTerminator();
		if (isa<ReturnInst>(term)) return changed;
		assert (isa<BranchInst>(term));
		BranchInst* branch = cast<BranchInst>(term);

		const bool hasConditionalExit = branch->isConditional();


		// If this block exits with a conditional branch,
		// make sure the post dominator (where both paths join again)
		// is marked the same as this block.
		if (hasConditionalExit) {

			BranchInfoAnalysis::BranchMapType::iterator BI = branchMap->find(block);
			if (BI != branchMap->end()) {
				DEBUG_PKT ( outs() << "  has conditional exit with disjunct paths - marking join block as " << (stillUniform ? "UNIFORM" : "VARYING") << "...\n"; );
				changed |= markBlock(BI->second->joinBlock, stillUniform, true, uniformValues, uniformLoops, uniformBlocks, finishedSet);
			} else {
				// The conditional branch has no common join point or no disjunct paths.
				// Conservatively recurse into the post-dominator.
				DEBUG_PKT ( outs() << "  has conditional exit - marking post dominator as " << (stillUniform ? "UNIFORM" : "VARYING") << "...\n"; );
				BasicBlock* postDomBlock = (*postDomTree)[block]->getIDom()->getBlock();
				assert (postDomBlock);
				changed |= markBlock(postDomBlock, stillUniform, true, uniformValues, uniformLoops, uniformBlocks, finishedSet);
			}

		}


		//
		// Special case if unconditional exit: mark successor with the same mark as this block's entry.
		// Otherwise, all blocks will just be marked with the mark of their predecessor's exit.
		//
		if (!hasConditionalExit) {
			DEBUG_PKT( outs() << "  branch is unconditional - marking as " << (stillUniform ? "UNIFORM" : "VARYING") << "...\n"; );
			changed |= markBlock(branch->getSuccessor(0), stillUniform, false, uniformValues, uniformLoops, uniformBlocks, finishedSet);
			return changed;
		}

		//
		// Conditional exit: mark successor blocks according to the terminator's mark.
		// This allows to nest UNIFORM control-flow inside VARYING control-flow (UNIFORM "relative to parent").
		//

		// Otherwise, recurse into all successors and mark according to this block's terminator.
		const bool hasUniformExit = uniformValues.find(branch) != uniformValues.end();

		DEBUG_PKT ( outs() << "  recursing into successors of block '" << block->getName() << "' - marking as " << (hasUniformExit ? "UNIFORM" : "VARYING") << "...\n"; );

		// Unconditional branches have to be UNIFORM.
		assert (hasConditionalExit || hasUniformExit);

		typedef GraphTraits<BasicBlock*> BlockTraits;
		for (BlockTraits::ChildIteratorType PI = BlockTraits::child_begin(block),
				PE = BlockTraits::child_end(block); PI != PE; ++PI)
		{
			BasicBlock* succBB = *PI;
			changed |= markBlock(succBB, hasUniformExit, false, uniformValues, uniformLoops, uniformBlocks, finishedSet);
		}

		return changed;
	}

	// Mark all values that are live across the boundaries of a varying loop
	// as VARYING.
	bool markLiveAcrossLoopBoundaryValues(const Loop* loop, std::set<Value*>& uniformValues, std::set<const Loop*>& uniformLoops) {
		assert (loop);

		bool changed = false;

		// ignore uniform loop...
		if (uniformLoops.find(loop) == uniformLoops.end()) {

			LoopLiveValueAnalysis::LiveValueSetType* liveValueMap =
					loopLiveValueAnalysis->getLiveValueSet(loop);

			for (LoopLiveValueAnalysis::LiveValueSetType::iterator it=
					liveValueMap->begin(), E=liveValueMap->end(); it!=E; ++it)
			{
				Instruction* liveVal = *it;
				changed |= markVaryingValue(liveVal, uniformValues);
			}

		}

		// ... but always recurse into all subloops
		for (Loop::iterator SL=loop->begin(), SLE=loop->end(); SL!=SLE; ++SL) {
			changed |= markLiveAcrossLoopBoundaryValues(*SL, uniformValues, uniformLoops);
		}

		return changed;
	}

	// requires blockInfoMap to be filled (after fixpoint iteration)
	void markFullyUniform(BasicBlock* block, std::set<BasicBlock*>& visitedSet) {
		assert (block);

		if (visitedSet.find(block) != visitedSet.end()) return;
		visitedSet.insert(block);

		BlockInfo* bi = analysisResults->getBlockInfo(block);
		assert (bi);

		// If block is a loop header, it is allowed to be VARYING and
		// we still have to recurse into common post dominator of exits.
		const Loop* loop = loopInfo->getLoopFor(block);
		const bool isLoopHeader = loopInfo->isLoopHeader(block);
		if (!bi->isNonDivergent && isLoopHeader) {
			SmallVector<BasicBlock*, 4> exitBlocks;
			loop->getUniqueExitBlocks(exitBlocks);
			BasicBlock* postDomBlock = *exitBlocks.begin();
			for (SmallVector<BasicBlock*, 4>::iterator it2=exitBlocks.begin()+1,
					E=exitBlocks.end(); it2!=E; ++it2)
			{
				postDomBlock = postDomTree->findNearestCommonDominator(postDomBlock, *it2);
			}
			DEBUG_PKT ( outs() << "  marking common post dominator of exits of VARYING loop '" << postDomBlock->getName() << "' as FULLY_UNIFORM...\n"; );
			markFullyUniform(postDomBlock, visitedSet);
			return;
		}

		// all other cases: exit if not UNIFORM
		if (!bi->isNonDivergent) return;

		// Mark block as FULLY_UNIFORM.
		bi->isFullyNonDivergent = true;
		DEBUG_PKT ( outs() << "marked block '" << block->getName() << "' as FULLY_UNIFORM!\n"; );

		TerminatorInst* term = block->getTerminator();
		if (isa<ReturnInst>(term)) return;

		// Recurse into post dominator
		BasicBlock* postDomBlock = (*postDomTree)[block]->getIDom()->getBlock();
		DEBUG_PKT ( outs() << "  marking post dominator '" << postDomBlock->getName() << "' as FULLY_UNIFORM...\n"; );
		markFullyUniform(postDomBlock, visitedSet);

		// If this is a loop header, we found a FULLY_UNIFORM loop.
		// Thus, in addition to successors, recurse into
		// a) common post dominator of all exits
		// b) all exits
		if (loop && isLoopHeader) {

			UniformLoopInfo* uli = analysisResults->getUniformLoopInfo(loop);
			assert (uli);
			if (uli->isNonDivergent) {
				uli->isFullyNonDivergent = true;
				DEBUG_PKT ( outs() << "  marked UNIFORM loop with header '" << block->getName() << "' as FULLY_UNIFORM...\n"; );
			}

			// recurse into common post dominator of exits
			SmallVector<BasicBlock*, 4> exitBlocks;
			loop->getUniqueExitBlocks(exitBlocks);
			postDomBlock = *exitBlocks.begin();
			for (SmallVector<BasicBlock*, 4>::iterator it2=exitBlocks.begin()+1,
					E=exitBlocks.end(); it2!=E; ++it2)
			{
				postDomBlock = postDomTree->findNearestCommonDominator(postDomBlock, *it2);
			}
			DEBUG_PKT ( outs() << "  marking common post dominator of loop exits '" << postDomBlock->getName() << "' as FULLY_UNIFORM...\n"; );
			markFullyUniform(postDomBlock, visitedSet);

			// if loop is UNIFORM (now FULLY_UNIFORM), every exit is also FULLY_UNIFORM
			if (uli->isNonDivergent) {
				for (SmallVector<BasicBlock*, 4>::iterator it2=exitBlocks.begin();
						it2!=exitBlocks.end(); ++it2)
				{
					BasicBlock* curExitBB = *it2;
					assert (!loopInfo->getLoopFor(curExitBB) || loopInfo->getLoopFor(curExitBB) != loop);
					DEBUG_PKT ( outs() << "  marking loop exit block '" << curExitBB->getName() << "' as FULLY_UNIFORM...\n"; );
					markFullyUniform(curExitBB, visitedSet);
				}
			}
		}

		// If the block's exit is UNIFORM, mark as FULLY_UNIFORM
		// and recurse into successors.
		if (bi->hasUniformExit) {
			bi->hasFullyUniformExit = true;

			typedef GraphTraits<BasicBlock*> BlockTraits;
			for (BlockTraits::ChildIteratorType PI = BlockTraits::child_begin(block),
					PE = BlockTraits::child_end(block); PI != PE; ++PI)
			{
				BasicBlock* succBB = *PI;
				DEBUG_PKT ( outs() << "  marking successor of block with FULLY_UNIFORM exit '" << succBB->getName() << "' as FULLY_UNIFORM...\n"; );
				markFullyUniform(succBB, visitedSet);
			}
		}
	}

	// We have to be sure to create loop exit masks for every nested varying
	// loop. Therefore, we first collect all those loops that are varying
	// and "top-level", meaning they are not nested or nested directly
	// inside a uniform loop.
	// EXAMPLE: Varying loop inside uniform loop inside varying loop.
	void collectAllNestedVaryingTopLevelLoops()
	{
		for (LoopInfo::iterator L=loopInfo->begin(), LE=loopInfo->end(); L!=LE; ++L) {
			const bool loopIsUniform = analysisResults->isUniform(*L);
			if (!loopIsUniform) {
				UniformLoopInfo* uli = analysisResults->getUniformLoopInfo(*L);
				uli->isVaryingTopLevel = true;
				analysisResults->addVaryingTopLevelLoop(*L);
				if ((*L)->getSubLoops().empty())
					uli->isVaryingInnermost = true; // standard case
			}
			collectNestedVaryingTopLevelLoops(*L);
		}
	}

	// We must not generate mask operations for subloops before their parents,
	// so we only collect the "top-level" varying loops (= at uniform loops, add
	// all varying subloops, at varying loops, do not add any direct subloop but
	// continue recursion into them).
	// NOTE: Should never be called directly!
	void collectNestedVaryingTopLevelLoops(Loop* loop) {
		const bool loopIsUniform = analysisResults->isUniform(loop);

		bool allSubLoopsUniform = true;
		for (Loop::iterator SL=loop->begin(); SL != loop->end(); ++SL) {
			const bool subLoopIsUniform = analysisResults->isUniform(*SL);
			if (loopIsUniform && !subLoopIsUniform) {
				// VARYING inner loop inside UNIFORM outer loop
				// -> mark inner loop as "top level"
				UniformLoopInfo* uli = analysisResults->getUniformLoopInfo(*SL);
				uli->isVaryingTopLevel = true;
				analysisResults->addVaryingTopLevelLoop(*SL);
				allSubLoopsUniform = false;
			}
			collectNestedVaryingTopLevelLoops(*SL);
		}

		if (!loopIsUniform && allSubLoopsUniform) {
			// Only UNIFORM inner loops inside VARYING outer loop
			// -> mark outer loop as "innermost"
			// TODO: This is conservative, but can we gain something from
			//       correctly handling mixed uniform/varying inner loops
			//       (same nesting level)?
			UniformLoopInfo* uli = analysisResults->getUniformLoopInfo(loop);
			uli->isVaryingInnermost = true;
		}
	}



	////////////////////////////////////////////////////////////////////////////
	//             CONSECUTIVE AND ALIGNMENT INFORMATION ANALYSIS             //
	////////////////////////////////////////////////////////////////////////////

	// IMPORTANT: Index and alignment info always refer to the result of the
	//            operation! This means that if we want to know whether we can
	//            use a vector load instead of a gather, we have to look at the
	//            indices of the GEP instruction if there is one.

	// Marking of same/consecutive/random and aligned/unaligned works different
	// than uniform/varying: We cannot simply mark everything and then remove
	// marks but rather have to only mark what we can prove.
	// Therefore, we start by marking uniform values as SAME and user-defined
	// values according to the input.
	// Only then we start recursively marking the rest of the function.
	void analyzeConsecutiveAlignedInfo() {
		assert (analysisResults->hasValueInfo() && "forgot to run analyzeUniformInfo()?");

		std::set<Value*> markedValues;

		DEBUG_PKT( outs() << source; );

//#ifdef PACKETIZER_DO_NOT_USE_SPLIT_ANALYSIS
//		for (AnalysisResults::ValueInfoMapType::const_iterator it=analysisResults->begin(),
//				E=analysisResults->end(); it!=E; ++it)
//		{
//			Value* value = it->second->value;
//			if (analysisResults->isUniform(value)) {
//				analysisResults->setIndexInfo(value, AnalysisResults::INDEX_SAME);
//			} else {
//				analysisResults->setIndexInfo(value, AnalysisResults::INDEX_RANDOM);
//			}
//			analysisResults->setAlignmentInfo(value, AnalysisResults::ALIGN_FALSE);
//		}
//		return;
//#endif

		// Mark instructions that are defined by the user via addValueInfo()
		// as SAME, CONSECUTIVE, or RANDOM and ALIGN_TRUE or ALIGN_FALSE.
		for (WholeFunctionVectorizer::ValueInfoMapType::const_iterator
				it=userValueInfoMap.begin(),
				E=userValueInfoMap.end(); it!=E; ++it)
		{
			// ignore instructions of other functions
			if (const Instruction* inst = dyn_cast<Instruction>(it->first)) {
				if (inst->getParent()->getParent() != &source) {
					DEBUG_PKT( outs() << "ignored user-defined instruction while marking: "
							<< *inst << "\n  source: " << source.getName()
							<< "\n  parent: " << inst->getParent()->getParent()->getName() << "\n"; );
					continue;
				}
			}
			// ignore arguments of other functions
			if (const Argument* arg = dyn_cast<Argument>(it->first)) {
				if (arg->getParent() != &source) {
					DEBUG_PKT( outs() << "ignored user-defined argument while marking: "
							<< *arg << "\n  source: " << source.getName()
							<< "\n  parent: " << arg->getParent()->getName() << "\n"; );
					continue;
				}
			}

			IndexInfo ii = it->second->uniform ? AnalysisResults::INDEX_SAME :
				it->second->consecutive ? AnalysisResults::INDEX_CONSECUTIVE : AnalysisResults::INDEX_RANDOM;
			AlignmentInfo ai = it->second->aligned ? AnalysisResults::ALIGN_TRUE : AnalysisResults::ALIGN_FALSE;
			analysisResults->setIndexInfo(it->first, ii);
			analysisResults->setAlignmentInfo(it->first, ai);

			assert (!(ii == AnalysisResults::INDEX_SAME && !analysisResults->isUniform(it->first)) && "value must not be VARYING / INDEX_SAME!");

			DEBUG_PKT( outs() << "marked user-defined value: " << *it->first << " as "
					<< AnalysisResults::getIndexInfoString(ii) << " / "
					<< AnalysisResults::getAlignmentInfoString(ai) << "!\n"; );

			markedValues.insert(it->first);
		}

		// Mark all arguments.
		// Mark uniform arguments that are pointers as INDEX_SAME / ALIGN_TRUE.
		// Mark varying pointer arguments as INDEX_CONSECUTIVE / ALIGN_TRUE.
		// These are requirements for the caller of the vectorized function.
		DEBUG_PKT( outs() << "\nmarking index/alignment info of arguments...\n"; );
		for (AnalysisResults::ValueInfoMapType::const_iterator it=analysisResults->begin(),
				E=analysisResults->end(); it!=E; ++it)
		{
			Value* value = it->second->value;

			// only do something for arguments, ignore all other values.
			if (!isa<Argument>(value)) continue;

			// ignore values that are already marked
			if (markedValues.find(value) != markedValues.end()) {
				assert (analysisResults->getValueInfo(value));
				DEBUG_PKT( outs() << "  value already marked as "
					<< AnalysisResults::getIndexInfoString(analysisResults->getValueInfo(value)->indexInfo) << " / "
					<< AnalysisResults::getAlignmentInfoString(analysisResults->getValueInfo(value)->alignmentInfo) << " - ignored!\n"; );
				continue;
			}

			Argument* arg = cast<Argument>(value);

			if (analysisResults->isUniform(arg)) {
				AlignmentInfo ai =
					value->getType()->isPointerTy() ? AnalysisResults::ALIGN_TRUE : AnalysisResults::ALIGN_FALSE;
				analysisResults->setIndexInfo(value, AnalysisResults::INDEX_SAME);
				analysisResults->setAlignmentInfo(value, ai);
				DEBUG_PKT( outs() << "marked UNIFORM argument: " << *value << " as "
						<< "INDEX_SAME / " << AnalysisResults::getAlignmentInfoString(ai) << "!\n"; );
			} else {
				IndexInfo ii =
					value->getType()->isPointerTy() ? AnalysisResults::INDEX_CONSECUTIVE : AnalysisResults::INDEX_RANDOM;
				analysisResults->setIndexInfo(arg, ii);
				analysisResults->setAlignmentInfo(arg, AnalysisResults::ALIGN_TRUE);
				DEBUG_PKT( outs() << "marked VARYING argument: " << *value
					<< " as " << AnalysisResults::getIndexInfoString(ii) << " / ALIGN_TRUE!\n"; );
			}

			markedValues.insert(value);
		}

		//
		// Collect all "outputs" of the function and conditional branches
		// as starting points for a post-reversed DFS.
		//

		std::set<Value*> workSet;

		// If the function returns something, this is an output :).
		// Otherwise, ignore but mark as INDEX_SAME / ALIGN_FALSE.
		BasicBlock* returnBlock = Packetizer::findReturnBlock(source);
		assert (returnBlock);
		if (!source.getFunctionType()->getReturnType()->isVoidTy()) {
			workSet.insert(returnBlock->getTerminator());
		} else {
			analysisResults->setIndexInfo(returnBlock->getTerminator(), AnalysisResults::INDEX_SAME);
			analysisResults->setAlignmentInfo(returnBlock->getTerminator(), AnalysisResults::ALIGN_FALSE);
		}

		// All calls and stores have to count as outputs.
		// All conditional branches also have to be added to workSet because
		// they depend on other values that have to be marked.
		// NOTE: Adding calls is not enough: they might have been added by the
		//       user and thus are marked already. We have to add their operands.
		for (Function::iterator BB=source.begin(), BBE=source.end(); BB!=BBE; ++BB) {
			for (BasicBlock::iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {
				if (!isa<CallInst>(I) &&
						!isa<StoreInst>(I) &&
						!isa<BranchInst>(I)) continue;

				if (BranchInst* br = dyn_cast <BranchInst>(I)) {
					if (br->isUnconditional()) {
						// ignore, but mark as INDEX_SAME / ALIGN_FALSE
						analysisResults->setIndexInfo(br, AnalysisResults::INDEX_SAME);
						analysisResults->setAlignmentInfo(br, AnalysisResults::ALIGN_FALSE);
						markedValues.insert(br);
						continue;
					}
				}

				if (markedValues.find(I) != markedValues.end()) {
					// Value is already marked, add operands to be sure that we
					// mark everything.
					for (Instruction::op_iterator O=I->op_begin(), OE=I->op_end(); O!=OE; ++O) {
						if (isa<BasicBlock>(O)) continue;
						if (isa<Function>(O)) continue;
						assert (isa<Value>(O));
						workSet.insert(cast<Value>(O));
					}
					continue;
				}

				workSet.insert(I);
			}
		}



		//
		// Now mark all other instructions according to their dependencies.
		//

		DEBUG_PKT( outs() << "\nmarking instructions...\n"; );
		for (std::set<Value*>::iterator it=workSet.begin(), E=workSet.end(); it!=E; ++it) {
			Value* value = *it;
			markConsecutiveAlignedValueAndOperands(value, markedValues);
			assert (analysisResults->getValueInfo(value));
			assert (analysisResults->getValueInfo(value)->alignmentInfo != AnalysisResults::ALIGN_NOT_INITIALIZED);
			assert (analysisResults->getValueInfo(value)->alignmentInfo != AnalysisResults::ALIGN_UNKNOWN);
			assert (analysisResults->getValueInfo(value)->indexInfo != AnalysisResults::INDEX_NOT_INITIALIZED);
			assert (analysisResults->getValueInfo(value)->indexInfo != AnalysisResults::INDEX_UNKNOWN);
			assert (!(analysisResults->getValueInfo(value)->indexInfo == AnalysisResults::INDEX_SAME && !analysisResults->isUniform(value)) && "value must not be VARYING / INDEX_SAME!");
		}

	}

	void markConsecutiveAlignedValueAndOperands(Value* value, std::set<Value*>& markedValues) {
		assert (value);
		assert (!isa<BasicBlock>(value));
		assert (!isa<Function>(value));

		DEBUG_PKT( outs() << "markConsecutiveAlignedValueAndOperands(): " << *value << "\n"; );

		// handle values that were already marked
		if (markedValues.find(value) != markedValues.end()) {
			assert (analysisResults->getValueInfo(value));
			DEBUG_PKT( outs() << "  already marked as "
				<< AnalysisResults::getIndexInfoString(analysisResults->getValueInfo(value)->indexInfo) << " / "
				<< AnalysisResults::getAlignmentInfoString(analysisResults->getValueInfo(value)->alignmentInfo) << " - ignored!\n"; );
			return;
		}

		if (Constant* c = dyn_cast<Constant>(value)) {
			AlignmentInfo ai = deriveAlignedInformation(c);
			IndexInfo ii = deriveIndexInfo(c);
			analysisResults->setIndexInfo(value, ii);
			analysisResults->setAlignmentInfo(value, ai);
			DEBUG_PKT( outs() << "marked constant: " << *c << " as "
					<< AnalysisResults::getIndexInfoString(ii) << " / "
					<< AnalysisResults::getAlignmentInfoString(ai) << "!\n"; );
			return;
		}


		// If this is a loop PHI, make sure to recurse into the predecessor
		// outside of the loop first to break cycles. The phi is then marked
		// according to this predecessor.
		if (isa<PHINode>(value) &&
				loopInfo->isLoopHeader(cast<PHINode>(value)->getParent()))
		{
			PHINode* phi = cast<PHINode>(value);
			assert (phi->getNumIncomingValues() == 2);
			Loop* loop = loopInfo->getLoopFor(phi->getParent());

			// Mark predecessor of incoming value from outside loop
			BasicBlock* preheaderBB = loop->getLoopPreheader();
			Value* preheaderVal = phi->getIncomingValueForBlock(preheaderBB);
			markConsecutiveAlignedValueAndOperands(preheaderVal, markedValues);

			ValueInfo* info = analysisResults->getValueInfo(preheaderVal);
			assert (info);
			assert (info->indexInfo != AnalysisResults::INDEX_NOT_INITIALIZED);
			assert (info->alignmentInfo != AnalysisResults::ALIGN_NOT_INITIALIZED);
			assert (info->indexInfo != AnalysisResults::INDEX_UNKNOWN);
			assert (info->alignmentInfo != AnalysisResults::ALIGN_UNKNOWN);

			// Mark the phi according to this predecessor, unless the phi is
			// known to be VARYING and the predecessor is INDEX_SAME - this
			// violates an important assumption, so we do not set a wrong mark
			// here to prevent this in the first place. Unfortunately, this
			// NOTE: Unfortunately, marking the phi as INDEX_RANDOM  might
			//       introduce some imprecision in cases where the other
			//       incoming value is INDEX_CONSECUTIVE.
			analysisResults->setIndexInfo(phi, info->indexInfo);
			analysisResults->setAlignmentInfo(phi, info->alignmentInfo);
			DEBUG_PKT( outs() << "marked loop phi: " << *phi << " as "
				<< AnalysisResults::getIndexInfoString(info->indexInfo) << " / "
				<< AnalysisResults::getAlignmentInfoString(info->alignmentInfo) << "!\n"; );

			markedValues.insert(phi);

			// now recurse into other operand (back edge)
			const int preheaderIdx = phi->getBasicBlockIndex(preheaderBB);
			const int backedgeIdx = preheaderIdx == 0 ? 1 : 0;
			Value* backedgeVal = phi->getIncomingValue(backedgeIdx);
			markConsecutiveAlignedValueAndOperands(backedgeVal, markedValues);

			assert (analysisResults->getValueInfo(backedgeVal));
			assert (analysisResults->getValueInfo(backedgeVal)->alignmentInfo != AnalysisResults::ALIGN_NOT_INITIALIZED);
			assert (analysisResults->getValueInfo(backedgeVal)->indexInfo != AnalysisResults::INDEX_NOT_INITIALIZED);
			assert (analysisResults->getValueInfo(backedgeVal)->alignmentInfo != AnalysisResults::ALIGN_UNKNOWN);
			assert (analysisResults->getValueInfo(backedgeVal)->indexInfo != AnalysisResults::INDEX_UNKNOWN);

			// If necessary, update marks of phi (if backedge-marks differ from
			// preheader-marks).
			std::vector<AlignmentInfo> aiVec;
			std::vector<IndexInfo> iiVec;
			iiVec.push_back(info->indexInfo);
			iiVec.push_back(analysisResults->getValueInfo(backedgeVal)->indexInfo);
			aiVec.push_back(info->alignmentInfo);
			aiVec.push_back(analysisResults->getValueInfo(backedgeVal)->alignmentInfo);

			IndexInfo ii = deriveIndexInfo(phi, iiVec);
			AlignmentInfo ai = deriveAlignmentInfo(phi, aiVec);
			analysisResults->setIndexInfo(phi, ii);
			analysisResults->setAlignmentInfo(phi, ai);
			DEBUG_PKT( outs() << "updated loop phi: " << *phi << " as "
				<< AnalysisResults::getIndexInfoString(ii) << " / "
				<< AnalysisResults::getAlignmentInfoString(ai) << "!\n"; );

			return;
		}

		// arguments have to be marked already, same for constants now.
		assert (isa<Instruction>(value));
		Instruction* valI = cast<Instruction>(value);

		// collect info of operands
		std::vector<AlignmentInfo> aiVec;
		std::vector<IndexInfo> iiVec;

		for (Instruction::op_iterator O=valI->op_begin(), OE=valI->op_end(); O!=OE; ++O) {
			Value* opVal = *O;
			if (isa<BasicBlock>(opVal)) continue; // handle phis correctly
			if (isa<Function>(opVal)) continue; // handle calls correctly

			markConsecutiveAlignedValueAndOperands(opVal, markedValues);

			ValueInfo* info = analysisResults->getValueInfo(opVal);
			assert (info);
			assert (info->indexInfo != AnalysisResults::INDEX_NOT_INITIALIZED);
			assert (info->indexInfo != AnalysisResults::INDEX_UNKNOWN);
			assert (info->alignmentInfo != AnalysisResults::ALIGN_NOT_INITIALIZED);
			assert (info->alignmentInfo != AnalysisResults::ALIGN_UNKNOWN);

			aiVec.push_back(info->alignmentInfo);
			iiVec.push_back(info->indexInfo);
		}

		// Derive alignment and index info depending on instruction and marks of
		// operands.
		IndexInfo ii = deriveIndexInfo(valI, iiVec);
		AlignmentInfo ai = deriveAlignmentInfo(valI, aiVec);
		analysisResults->setIndexInfo(valI, ii);
		analysisResults->setAlignmentInfo(valI, ai);

		DEBUG_PKT( outs() << "marked value: " << *valI << " as "
				<< AnalysisResults::getIndexInfoString(ii) << " / "
				<< AnalysisResults::getAlignmentInfoString(ai) << "!\n"; );

		markedValues.insert(valI);

		assert (analysisResults->getValueInfo(valI));
		assert (analysisResults->getValueInfo(valI)->indexInfo != AnalysisResults::INDEX_NOT_INITIALIZED);
		assert (analysisResults->getValueInfo(valI)->indexInfo != AnalysisResults::INDEX_UNKNOWN);
		assert (analysisResults->getValueInfo(valI)->alignmentInfo != AnalysisResults::ALIGN_NOT_INITIALIZED);
		assert (analysisResults->getValueInfo(valI)->alignmentInfo != AnalysisResults::ALIGN_UNKNOWN);
	}

	// Derive alignment info depending on the instruction and operands marks
	// TODO: div/rem/others?
	inline AlignmentInfo deriveAlignmentInfo(Instruction* inst, const std::vector<AlignmentInfo>& aiVec) const
	{
		assert (inst);

		switch (inst->getOpcode()) {

			// These instructions retain alignment if all operands are aligned
			case Instruction::Add:
			case Instruction::Sub:
			case Instruction::FAdd:
			case Instruction::FSub:
			{
				assert (aiVec.size() == 2);
				if (aiVec[0] != AnalysisResults::ALIGN_TRUE || aiVec[1] != AnalysisResults::ALIGN_TRUE) return AnalysisResults::ALIGN_FALSE;
				return AnalysisResults::ALIGN_TRUE;
			}

			// These instructions retain alignment if one operand is aligned
			case Instruction::Mul:
			case Instruction::FMul:
			{
				assert (aiVec.size() == 2);
				if (aiVec[0] == AnalysisResults::ALIGN_TRUE || aiVec[1] == AnalysisResults::ALIGN_TRUE) return AnalysisResults::ALIGN_TRUE;
				return AnalysisResults::ALIGN_FALSE;
			}

			// GEP retains alignment if all indices are aligned
			// TODO: really? :P
			case Instruction::GetElementPtr:
			{
				GetElementPtrInst* gep = cast<GetElementPtrInst>(inst);
				const unsigned numIndices = gep->getNumIndices();
				const unsigned idxBegin = gep->getNumOperands() - numIndices;
				for (unsigned i=idxBegin, e=gep->getNumOperands(); i!=e; ++i) {
					if (aiVec[i] != AnalysisResults::ALIGN_TRUE) return AnalysisResults::ALIGN_FALSE;
				}
				return AnalysisResults::ALIGN_TRUE;
			}

			// Phi is aligned if all incoming values are aligned
			case Instruction::PHI:
			{
				PHINode* phi = cast<PHINode>(inst);
				for (unsigned i=0, e=phi->getNumIncomingValues(); i!=e; ++i) {
					// We ignored basic blocks while collecting indexInfo,
					// so now we can directly index iiVec per incoming value.
					if (aiVec[i] != AnalysisResults::ALIGN_TRUE) return AnalysisResults::ALIGN_FALSE;
				}

				return AnalysisResults::ALIGN_TRUE;
			}

			// All other instructions (conservatively) produce non-aligned values
			default:
			{
				return AnalysisResults::ALIGN_FALSE; // weakest information
			}
		}
	}

	// Derive index info depending on the instruction and operands marks
	inline IndexInfo deriveIndexInfo(Instruction* inst, const std::vector<IndexInfo>& iiVec) const
	{
		assert (inst);

		// A VARYING instruction can never be INDEX_SAME, but it can be
		// INDEX_CONSECUTIVE, so if we return INDEX_RANDOM here we lose precision.
		//if (!analysisResults->isUniform(inst)) return INDEX_RANDOM;

		switch (inst->getOpcode()) {

			case Instruction::Add:
			case Instruction::FAdd:
			case Instruction::Sub:
			case Instruction::FSub:
			{
				assert (iiVec.size() == 2);
				// Adding/subtracting two "same" indices yields a "same" index.
				if (iiVec[0] == AnalysisResults::INDEX_SAME && iiVec[1] == AnalysisResults::INDEX_SAME) {
					return AnalysisResults::INDEX_SAME;
				}
				if (iiVec[0] == AnalysisResults::INDEX_CONSECUTIVE && iiVec[1] == AnalysisResults::INDEX_CONSECUTIVE) {
					if (inst->getOpcode() == Instruction::Sub ||
							inst->getOpcode() == Instruction::FSub)
					{
						// Subtracting two consecutive indices yields a "same" index.
						return AnalysisResults::INDEX_SAME;
					}
					else {
						// Adding two consecutive indices yields a strided index.
						// TODO: do something useful with this info :)
						//return INDEX_STRIDED;
						return AnalysisResults::INDEX_RANDOM;
					}
				}
				// Adding/subtracting a consecutive and a "same" index yields a consecutive index.
				if ((iiVec[0] == AnalysisResults::INDEX_CONSECUTIVE && iiVec[1] == AnalysisResults::INDEX_SAME) ||
						(iiVec[1] == AnalysisResults::INDEX_CONSECUTIVE && iiVec[0] == AnalysisResults::INDEX_SAME))
				{
					return AnalysisResults::INDEX_CONSECUTIVE;
				}
				return AnalysisResults::INDEX_RANDOM;
			}

			case Instruction::GetElementPtr:
			{
				// If all operands are "same", return "same".
				// If one index is consecutive while all others are "same" and
				// the pointer operand is "same" as well, return "consecutive".
				// Otherwise, return INDEX_RANDOM.
				GetElementPtrInst* gep = cast<GetElementPtrInst>(inst);
				bool allSame = true;
				unsigned consecFound = 0;
				for (unsigned i=0, e=gep->getNumOperands(); i!=e; ++i) {
					if (iiVec[i] == AnalysisResults::INDEX_CONSECUTIVE) {
						++consecFound;
						allSame = false;
					} else if (iiVec[i] != AnalysisResults::INDEX_SAME) {
						return AnalysisResults::INDEX_RANDOM;
					}
				}
				if (allSame) return AnalysisResults::INDEX_SAME;
				if (consecFound == 1) return AnalysisResults::INDEX_CONSECUTIVE;
				return AnalysisResults::INDEX_RANDOM;
			}

			case Instruction::Mul:
			case Instruction::FMul:
			{
				assert (iiVec.size() == 2);
				// Multiplying two "same" indices yields a "same" index.
				if (iiVec[0] == AnalysisResults::INDEX_SAME && iiVec[1] == AnalysisResults::INDEX_SAME) {
					return AnalysisResults::INDEX_SAME;
				}
				// Multiplying a "same" index with a consecutive one yields a strided index.
				if ((iiVec[0] == AnalysisResults::INDEX_SAME && iiVec[1] == AnalysisResults::INDEX_CONSECUTIVE) ||
						(iiVec[1] == AnalysisResults::INDEX_SAME && iiVec[0] == AnalysisResults::INDEX_CONSECUTIVE))
				{
					// TODO: do something useful with this info :)
					//return INDEX_STRIDED;
					return AnalysisResults::INDEX_RANDOM;
				}
				return AnalysisResults::INDEX_RANDOM;
			}

			// These instructions retain "same" if operands are all "same",
			// nothing else.
			case Instruction::UDiv:
			case Instruction::SDiv:
			case Instruction::URem:
			case Instruction::SRem:
			case Instruction::And:
			case Instruction::Or:
			case Instruction::Xor:
			case Instruction::Shl:
			case Instruction::LShr:
			case Instruction::AShr:
			case Instruction::Call:
			case Instruction::ICmp:
			case Instruction::FCmp:
			{
				for (std::vector<IndexInfo>::const_iterator it=iiVec.begin(), E=iiVec.end(); it!=E; ++it) {
					if (*it != AnalysisResults::INDEX_SAME) return AnalysisResults::INDEX_RANDOM;
				}
				return AnalysisResults::INDEX_SAME;
			}

			// These instructions retain any information if all operands have
			// the same mark.
			case Instruction::Trunc:
			case Instruction::SExt:
			case Instruction::FPTrunc:
			case Instruction::FPExt:
			case Instruction::ZExt:
			case Instruction::FPToUI:
			case Instruction::FPToSI:
			case Instruction::UIToFP:
			case Instruction::SIToFP:
			case Instruction::IntToPtr:
			case Instruction::PtrToInt:
			case Instruction::BitCast:
			{
				bool allSame = true;
				bool allConsecutive = true;
				bool allStrided = true;
				for (unsigned i=0, e=iiVec.size(); i!=e; ++i) {
					allSame &= iiVec[i] == AnalysisResults::INDEX_SAME;
					allConsecutive &= iiVec[i] == AnalysisResults::INDEX_CONSECUTIVE;
					allStrided &= iiVec[i] == AnalysisResults::INDEX_STRIDED;
				}
				assert (!(allSame && allConsecutive));
				assert (!(allSame && allStrided));
				assert (!(allStrided && allConsecutive));

				if (allSame) return AnalysisResults::INDEX_SAME;
				if (allConsecutive) return AnalysisResults::INDEX_CONSECUTIVE;
				if (allStrided) return AnalysisResults::INDEX_STRIDED;
				return AnalysisResults::INDEX_RANDOM;
			}

			case Instruction::Store:
			{
				for (std::vector<IndexInfo>::const_iterator it=iiVec.begin(), E=iiVec.end(); it!=E; ++it) {
					if (*it != AnalysisResults::INDEX_SAME) return AnalysisResults::INDEX_RANDOM;
				}
				return AnalysisResults::INDEX_SAME;
			}

			case Instruction::Load:
			{
				LoadInst* load = cast<LoadInst>(inst);

				// Special case:
				// If we load from a pointer-to-pointer that is marked
				// INDEX_CONSECUTIVE, the result is still INDEX_CONSECUTIVE.
				// TODO: Unsure if this is safe :)
				Value* pointer = load->getPointerOperand();
				if (pointer->getType()->getContainedType(0)->isPointerTy()) {
					if (analysisResults->isConsecutive(pointer)) return AnalysisResults::INDEX_CONSECUTIVE;
				}

				const unsigned ptrIdx = load->getPointerOperandIndex();
				if (iiVec[ptrIdx] == AnalysisResults::INDEX_SAME) return AnalysisResults::INDEX_SAME;
				return AnalysisResults::INDEX_RANDOM;
			}

			case Instruction::PHI:
			{
				PHINode* phi = cast<PHINode>(inst);
				bool allSame = true;
				bool allConsecutive = true;
				bool allStrided = true;
				for (unsigned i=0, e=phi->getNumIncomingValues(); i!=e; ++i) {
					// We ignored basic blocks while collecting indexInfo,
					// so now we can directly index iiVec per incoming value.
					allSame &= iiVec[i] == AnalysisResults::INDEX_SAME;
					allConsecutive &= iiVec[i] == AnalysisResults::INDEX_CONSECUTIVE;
					allStrided &= iiVec[i] == AnalysisResults::INDEX_STRIDED;
				}
				assert (!(allSame && allConsecutive));
				assert (!(allSame && allStrided));
				assert (!(allStrided && allConsecutive));

				if (allSame) return AnalysisResults::INDEX_SAME;
				if (allConsecutive) return AnalysisResults::INDEX_CONSECUTIVE;
				if (allStrided) return AnalysisResults::INDEX_STRIDED;
				return AnalysisResults::INDEX_RANDOM;
			}

			case Instruction::Select:
			{
				assert (iiVec.size() == 3);

				// If the condition is not UNIFORM, we cannot say anything
				// about the result.
				if (iiVec[0] != AnalysisResults::INDEX_SAME) {
					assert (!analysisResults->isUniform(cast<SelectInst>(inst)->getCondition()));
					return AnalysisResults::INDEX_RANDOM;
				}

				assert (analysisResults->isUniform(cast<SelectInst>(inst)->getCondition()));

				// If the condition is INDEX_SAME, we know that the result
				// has a certain property if both incoming values have it.
				bool allSame = iiVec[1] == AnalysisResults::INDEX_SAME && iiVec[2] == AnalysisResults::INDEX_SAME;
				bool allConsecutive = iiVec[1] == AnalysisResults::INDEX_CONSECUTIVE && iiVec[2] == AnalysisResults::INDEX_CONSECUTIVE;
				bool allStrided = iiVec[1] == AnalysisResults::INDEX_STRIDED && iiVec[2] == AnalysisResults::INDEX_STRIDED;

				if (allSame) return AnalysisResults::INDEX_SAME;
				if (allConsecutive) return AnalysisResults::INDEX_CONSECUTIVE;
				if (allStrided) return AnalysisResults::INDEX_STRIDED;
				return AnalysisResults::INDEX_RANDOM;
			}

			default:
			{
				// Retain any information if all operands have the same mark.
				if (iiVec.size() == 0) return AnalysisResults::INDEX_SAME;
				bool allSame = true;
				bool allConsecutive = true;
				bool allStrided = true;
				for (unsigned i=0, e=iiVec.size(); i!=e; ++i) {
					allSame &= iiVec[i] == AnalysisResults::INDEX_SAME;
					allConsecutive &= iiVec[i] == AnalysisResults::INDEX_CONSECUTIVE;
					allStrided &= iiVec[i] == AnalysisResults::INDEX_STRIDED;
				}
				assert (!(allSame && allConsecutive));
				assert (!(allSame && allStrided));
				assert (!(allStrided && allConsecutive));

				if (allSame) return AnalysisResults::INDEX_SAME;
				if (allConsecutive) return AnalysisResults::INDEX_CONSECUTIVE;
				if (allStrided) return AnalysisResults::INDEX_STRIDED;
				return AnalysisResults::INDEX_RANDOM; // weakest information
			}
		}
	}


	IndexInfo deriveIndexInfo(const Constant* c) const
	{
		assert (c);
		assert (!isa<BasicBlock>(c));
		assert (!isa<Function>(c));

		return AnalysisResults::INDEX_SAME;

		// This is all bullshit. A constant is AnalysisResults::INDEX_SAME, full stop.
#if 0
		// Constants of "simple" type are SAME by definition.
		// TODO: What about pointers?
		if (c->getType()->isIntegerTy() ||
				c->getType()->isFloatTy() ||
				c->getType()->isDoubleTy()) return AnalysisResults::INDEX_SAME;

		// Only vector constants can be same or consecutive, anything else is RANDOM.
		if (!c->getType()->isVectorTy()) return AnalysisResults::INDEX_RANDOM;


		assert (isa<ConstantVector>(c));
		const ConstantVector* cv = cast<ConstantVector>(c);

		// constant vectors are SAME if their elements are the same.
		bool same = true;
		const Constant* celem = cv->getOperand(0);
		for (unsigned i=1, e=cv->getNumOperands(); i!=e; ++i) {
			same &= cv->getOperand(i) != celem;
			if (!same) break;
		}
		if (same) return AnalysisResults::INDEX_SAME;

		// Only integer vector constants can be consecutive, anything else is RANDOM.
		if (!cv->getType()->getElementType()->isIntegerTy()) return AnalysisResults::INDEX_RANDOM;

		assert (isa<ConstantInt>(cv->getOperand(0)));
		const ConstantInt* cielem = cast<ConstantInt>(cv->getOperand(0));
		uint64_t intValue1 = *cielem->getValue().getRawData();

		// The vector is consecutive, if each element is exactly the increment
		// of its predecessor-element.
		for (unsigned i=1, e=cv->getNumOperands(); i!=e; ++i) {
			const uint64_t& intValue2 = *cast<ConstantInt>(cv->getOperand(i))->getValue().getRawData();
			if (intValue2 != ++intValue1) return AnalysisResults::INDEX_RANDOM;
		}

		return AnalysisResults::INDEX_CONSECUTIVE;
#endif
	}

	// TODO: implement support for natural numbers stored as floats?
	AlignmentInfo deriveAlignedInformation(const Constant* c) const
	{
		assert (c);
		assert (!isa<BasicBlock>(c));
		assert (!isa<Function>(c));

		// Integer that are divisible by the simd width are ALIGN_TRUE.
		if (c->getType()->isIntegerTy()) {
			const ConstantInt* cint = cast<ConstantInt>(c);
			const uint64_t& intValue = *cint->getValue().getRawData();
			if (intValue % info.simdWidth == 0) return AnalysisResults::ALIGN_TRUE;
			else return AnalysisResults::ALIGN_FALSE;
		}

		// Other than that, only integer vector constants can be aligned.
		if (!c->getType()->isVectorTy()) return AnalysisResults::ALIGN_FALSE;

		assert (isa<ConstantVector>(c));
		const ConstantVector* cv = cast<ConstantVector>(c);

		if (!cv->getType()->getElementType()->isIntegerTy()) return AnalysisResults::ALIGN_FALSE;

		assert (isa<ConstantInt>(cv->getOperand(0)));
		const ConstantInt* celem = cast<ConstantInt>(cv->getOperand(0));
		const uint64_t& intValue = *celem->getValue().getRawData();

		// The vector is aligned if its first element is aligned and the
		// constant is either SAME or CONSECUTIVE
		if (intValue % info.simdWidth != 0) return AnalysisResults::ALIGN_FALSE;

		// TODO: There might be other cases (e.g. STRIDED) where we want to
		//       return true...
		if (analysisResults->isSame(c)) return AnalysisResults::ALIGN_TRUE;
		if (analysisResults->isConsecutive(c)) return AnalysisResults::ALIGN_TRUE;
		return AnalysisResults::ALIGN_FALSE;
	}



	////////////////////////////////////////////////////////////////////////////
	//                       SPLIT INFORMATION ANALYSIS                       //
	////////////////////////////////////////////////////////////////////////////

	typedef std::map<const Value*, SplitInfo> SplitInfoMapType;

	void analyzeSplitInfo(NativeMethods& nativeMethods, const Packetizer::PacketizerInfo& info) {
		assert (analysisResults->hasValueInfo() && "forgot to run analyzeUniformInfo()?");

		DEBUG_PKT( outs() << "\nTesting for instructions that have to be split"
			<< " and executed scalar instead of vectorized...\n"; );

		// 1) Analyze primary split targets (instructions that have to be split)

		SplitInfoMapType primarySplitValues;

		for (Function::const_iterator BB=source.begin(), BBE=source.end(); BB!=BBE; ++BB) {
			for (BasicBlock::const_iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {

				// Only these kinds of instructions are targets for "full" splitting.
				if (!isa<LoadInst>(I) &&
						!isa<StoreInst>(I) &&
						!isa<GetElementPtrInst>(I) &&
						!isa<PHINode>(I) &&
						!isa<SelectInst>(I) &&
						!isa<CallInst>(I)) continue;

				// UNIFORM instructions never have to be split, at most
				// replicated (secondary targets).
				// Store and call instructions that depend on VARYING control-
				// flow have to be split and guarded even if they are UNIFORM
				// (the dynamic mask might require no call or store to happen).
				// NOTE: UNIFORM is not enough, FULLY_UNIFORM is required!
				if (analysisResults->isUniform(I)) {
					if (!isa<StoreInst>(I) && !isa<CallInst>(I)) continue;
					if (analysisResults->hasFullyUniformEntry(BB)) continue;
				}

				DEBUG_PKT( outs() << "testing if VARYING instruction has to be split:"
						<< *I << "...\n"; );

				// Do not perform a full split if the value produces a result
				// that is INDEX_CONSECUTIVE.
#ifdef PACKETIZER_DISABLE_MEMOP_VECTORIZATION
				if (analysisResults->isConsecutive(I) && !isa<GetElementPtrInst>(I)) {
					DEBUG_PKT( outs() << "  is INDEX_CONSECUTIVE - ignored!\n"; );
					continue;
				}
#else
				if (analysisResults->isConsecutive(I)) {
					DEBUG_PKT( outs() << "  is INDEX_CONSECUTIVE - ignored!\n"; );
					continue;
				}
#endif


				SplitInfo si = AnalysisResults::SPLIT_NOT_INITIALIZED;
				switch (I->getOpcode()) {
					case Instruction::Load:
					{
						si = analyzeLoadForSplitting(cast<LoadInst>(I));
						break;
					}
					case Instruction::Store:
					{
						si = analyzeStoreForSplitting(cast<StoreInst>(I));
						break;
					}
					case Instruction::GetElementPtr:{
						si = analyzeGEPForSplitting(cast<GetElementPtrInst>(I));
						break;
					}
					case Instruction::PHI:
					{
						si = analyzePhiForSplitting(cast<PHINode>(I));
						break;
					}
					case Instruction::Select:
					{
						si = analyzeSelectForSplitting(cast<SelectInst>(I));
						break;
					}
					case Instruction::Call:
					{
						si = analyzeCallForSplitting(cast<CallInst>(I), nativeMethods, info);
						break;
					}
					default:
					{
						// Any other instruction can never be marked as SPLIT_FULL or
						// SPLIT_FULL_GUARDED.
						// They can be marked SPLIT_RESULT if their result is used by another
						// instruction which has to be split.
						// They can be marked SPLIT_REPLICATE if they are uniform, so their result
						// is simply used in all of the split instructions (no splitting).
						// -> This is done in a second step.
						assert (false && "wtf?!");
						throw std::logic_error("INTERNAL ERROR: should never happen!");
						break;
					}
				}

#ifdef PACKETIZER_DO_NOT_USE_SPLIT_ANALYSIS
				if (isa<LoadInst>(I) || isa<StoreInst>(I) || isa<CallInst>(I)) {
					if (si == AnalysisResults::SPLIT_NEVER) si = AnalysisResults::SPLIT_FULL;
				}
#endif

				if (si != AnalysisResults::SPLIT_NEVER) primarySplitValues.insert(std::make_pair(I, si));
			}
		}

		DEBUG_PKT(
			outs() << "\nPrimary targets for splitting:\n";
			for (SplitInfoMapType::const_iterator i = primarySplitValues.begin(),
					e = primarySplitValues.end(); i != e; ++i)
			{
				outs() << " * " << *(i->first) << " (" << AnalysisResults::getSplitInfoString(i->second) << ")\n";
			}
			outs() << "\n";
		);

		// 2) analyze uses/operands of primary split targets

		// At this point, primarySplitValues holds all those instructions that
		// were marked initially as "to be split" (primary targets).
		// Now, all other values that require splitting because they are used by
		// one of the primary targets or because they depend on the primary
		// targets.
		// NOTE: There is no particular reason here to use a second map instead
		//       of directly using valueInfoMap other than that this code was
		//       migrated from somewhere else and I did not want to change this.
		SplitInfoMapType secondarySplitValues;

		// Insert primary split values in order to prevent them from being added twice.
		secondarySplitValues.insert(primarySplitValues.begin(), primarySplitValues.end());

		for (SplitInfoMapType::iterator it=primarySplitValues.begin(),
				E=primarySplitValues.end(); it!=E; ++it)
		{
			// iterate only over split values
			assert (it->second != AnalysisResults::SPLIT_NOT_INITIALIZED && it->second != AnalysisResults::SPLIT_UNKNOWN);
			if (it->second != AnalysisResults::SPLIT_FULL && it->second != AnalysisResults::SPLIT_FULL_GUARDED) continue;

			assert (isa<Instruction>(it->first));
			const Instruction* splitInst = cast<Instruction>(it->first);

			DEBUG_PKT( outs() << "\nMarking uses and operands of primary split target: "
				<< *splitInst << "...\n"; );

			// splitValue is only allowed to be UNIFORM if it is a call or store
			// that depends on VARYING control-flow and thus has to be "split".
			assert (!analysisResults->isUniform(splitInst) ||
					((isa<StoreInst>(splitInst) || isa<CallInst>(splitInst)) &&
					 it->second == AnalysisResults::SPLIT_FULL_GUARDED));

			switch (splitInst->getOpcode()) {

				case Instruction::Load:
				{
					const LoadInst* oldLoad = cast<LoadInst>(splitInst);

					// Add non-constant, non-uniform pointer operand to split map.
					markSplitOperand(oldLoad->getPointerOperand(), secondarySplitValues);

					// Recursively add all uses to split map until "mergeable"
					// instructions are found.
					markSplitInstUses(oldLoad, secondarySplitValues);

					break;
				}
				case Instruction::Store:
				{
					const StoreInst* oldStore = cast<StoreInst>(splitInst);

					// Add non-constant, non-uniform operands to split map.
					markSplitOperand(oldStore->getValueOperand(), secondarySplitValues);
					markSplitOperand(oldStore->getPointerOperand(), secondarySplitValues);

					// There are no uses that we could add to the split map ;).

					break;
				}
				case Instruction::GetElementPtr:
				{
					const GetElementPtrInst* oldGEP = cast<GetElementPtrInst>(splitInst);

					// Do not add pointer operand to split map.

					// Add non-constant, non-uniform index operands to split map.
					for (GetElementPtrInst::const_op_iterator IDX=oldGEP->idx_begin(),
							IDXE=oldGEP->idx_end(); IDX!=IDXE; ++IDX)
					{
						assert (isa<Value>(IDX));
						markSplitOperand(cast<Value>(IDX), secondarySplitValues);
					}

					// Recursively add all uses to split map until "mergeable"
					// instructions are found.
					markSplitInstUses(oldGEP, secondarySplitValues);

					break;
				}
				case Instruction::PHI:
				{
					// NOTE: This is a future pointer-select
					const PHINode* oldPhi = cast<PHINode>(splitInst);

					// Add non-constant, non-uniform incoming values to split map.
					for (unsigned i=0, e=oldPhi->getNumIncomingValues(); i!=e; ++i) {
						const Value* incVal = oldPhi->getIncomingValue(i);

						markSplitOperand(incVal, secondarySplitValues);
					}

					// Recursively add all uses to split map until "mergeable"
					// instructions are found.
					markSplitInstUses(oldPhi, secondarySplitValues);

					break;
				}
				case Instruction::Select:
				{
					const SelectInst* oldSelect = cast<SelectInst>(splitInst);

					// Add non-constant, non-uniform operands to split map
					markSplitOperand(oldSelect->getCondition(), secondarySplitValues);
					markSplitOperand(oldSelect->getTrueValue(), secondarySplitValues);
					markSplitOperand(oldSelect->getFalseValue(), secondarySplitValues);

					// Recursively add all uses to split map until "mergeable"
					// instructions are found.
					markSplitInstUses(oldSelect, secondarySplitValues);

					break;
				}
				case Instruction::Call:
				{
					const CallInst* oldCall = cast<CallInst>(splitInst);

					// Add non-constant, non-uniform parameters to split map.
					for (unsigned i=0, e=oldCall->getNumArgOperands(); i!=e; ++i) {
						markSplitOperand(oldCall->getArgOperand(i), secondarySplitValues);
					}

					// Recursively add all uses to split map until "mergeable"
					// instructions are found.
					markSplitInstUses(oldCall, secondarySplitValues);

					break;
				}
				default:
				{
					assert (false && "bad primary splitting target found!");
					throw std::logic_error("INTERNAL ERROR: bad primary splitting target found!");
				}
			}
		}

		// All marked values are in secondarySplitValues map.

		DEBUG_PKT(
			outs() << "\nValues to be split up or replicated:\n";
			for (SplitInfoMapType::const_iterator i = secondarySplitValues.begin(),
					e = secondarySplitValues.end(); i != e; ++i)
			{
				outs() << " * " << *(i->first) << " (" << AnalysisResults::getSplitInfoString(i->second) << ")\n";
				assert ((!(isa<Argument>(i->first) && i->first->getType()->isPointerTy()) ||
						(i->second != AnalysisResults::SPLIT_RESULT &&
						 i->second != AnalysisResults::SPLIT_FULL &&
						 i->second != AnalysisResults::SPLIT_FULL_GUARDED)) &&
						"pointer argument must not be marked anything else but SPLIT_NEVER or SPLIT_REPLICATE!");
			}
			outs() << "\n";
		);


		// Now update information in valueInfoMap and mark all
		// values that are still without mark as SPLIT_NEVER.
		for (Function::const_iterator BB=source.begin(), BBE=source.end(); BB!=BBE; ++BB) {
			for (BasicBlock::const_iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {

				ValueInfo* info = analysisResults->getValueInfo(I);
				if (info->splitInfo != AnalysisResults::SPLIT_NOT_INITIALIZED) continue; // do not overwrite anything

				SplitInfoMapType::const_iterator it = secondarySplitValues.find(I);
				info->splitInfo = it == secondarySplitValues.end() ? AnalysisResults::SPLIT_NEVER : it->second;

				// Be sure that we have all operators marked as well.
				for (Instruction::const_op_iterator O=I->op_begin(), OE=I->op_end(); O!=OE; ++O) {
					if (isa<BasicBlock>(*O) || isa<Function>(*O)) continue;
					Value* opVal = cast<Value>(*O);

					ValueInfo* opInfo = analysisResults->getValueInfo(opVal);
					if (opInfo->splitInfo != AnalysisResults::SPLIT_NOT_INITIALIZED) continue; // do not overwrite anything

					SplitInfoMapType::const_iterator it2 = secondarySplitValues.find(opVal);
					opInfo->splitInfo = it2 == secondarySplitValues.end() ? AnalysisResults::SPLIT_NEVER : it2->second;
				}

			}
		}

		// Also do not forget to mark all other arguments.
		// This is only for completeness, it actually means that the argument
		// is unused.
		for (Function::const_arg_iterator A=source.arg_begin(),
				AE=source.arg_end(); A!=AE; ++A)
		{
				ValueInfo* info = analysisResults->getValueInfo(A);
				if (info->splitInfo != AnalysisResults::SPLIT_NOT_INITIALIZED) continue; // do not overwrite anything

				SplitInfoMapType::const_iterator it = secondarySplitValues.find(A);
				info->splitInfo = it == secondarySplitValues.end() ? AnalysisResults::SPLIT_NEVER : it->second;
		}

	}


public:
	// The following options exist for a load:
	// a) The pointer is INDEX_SAME -> create scalar load (automatically broadcasted later)
	// b) The pointer is INDEX_CONSECUTIVE / ALIGN_TRUE -> create vector load
	// c) The pointer is INDEX_CONSECUTIVE / ALIGN_FALSE -> create unaligned vector load
	// d) The pointer is INDEX_RANDOM -> create "gather" (split into W scalar loads + merge)
	// e) The pointer has a non-simple type -> create "gather" (split into W scalar loads + merge)
	SplitInfo analyzeLoadForSplitting(const LoadInst* load) {
		assert (load);

		const Value* pointer = load->getPointerOperand();

		const bool hasRandomPtr = analysisResults->isRandom(pointer);
		const bool hasNonSimpleNonPointerTargetType =
			!isa<PointerType>(load->getType()) &&
			!Packetizer::isPacketizableInstructionType(load->getType());

#ifndef PACKETIZER_DISABLE_MEMOP_VECTORIZATION
		if (!hasRandomPtr && !hasNonSimpleNonPointerTargetType) {
			DEBUG_PKT( outs() << "    load can be VECTORIZED!\n"; );
			return AnalysisResults::SPLIT_NEVER;
		}
#endif

		DEBUG_PKT( if (hasRandomPtr) outs() << "  has INDEX_RANDOM pointer operand!\n"; );
		DEBUG_PKT( if (hasNonSimpleNonPointerTargetType) outs() << "    has pointer operand with non-simple/non-pointer type!\n"; );

		DEBUG_PKT( outs() << "    requires splitting ('gather')!\n"; );

		// TODO: Unsure whether guarding scalar loads with non-uniform mask
		//       could be faster than executing all info.simdWidth loads.
		const bool requiresIfCascade = false /* || !analysisResults->hasFullyUniformEntry(load->getParent())*/;

		return requiresIfCascade ? AnalysisResults::SPLIT_FULL_GUARDED : AnalysisResults::SPLIT_FULL;
	}

	// A store has to be split if
	// - its pointer operand is INDEX_RANDOM
	// - the block's entry mask is not FULLY_UNIFORM
	// - the store is of non-simple type (neither int nor float)
	SplitInfo analyzeStoreForSplitting(const StoreInst* store) {
		assert (store);

		const Value* value = store->getValueOperand();

		const bool hasRandomPtr = analysisResults->isRandom(store->getPointerOperand());
		const bool hasNonFullyUniformMask =
			!analysisResults->hasFullyUniformEntry(store->getParent());
		const bool hasNonSimpleNonPointerTargetType =
			!isa<PointerType>(value->getType()) &&
			!Packetizer::isPacketizableInstructionType(value->getType());

#ifndef PACKETIZER_DISABLE_MEMOP_VECTORIZATION
		if (!hasRandomPtr && !hasNonFullyUniformMask && !hasNonSimpleNonPointerTargetType) {
			DEBUG_PKT( outs() << "    store can be VECTORIZED!\n"; );
			return AnalysisResults::SPLIT_NEVER;
		}
#endif

		DEBUG_PKT( if (hasRandomPtr)outs() << "  has INDEX_RANDOM pointer operand!\n"; );
		DEBUG_PKT( if (hasNonFullyUniformMask) outs() << "  has mask that is not FULLY_UNIFORM!\n"; );
		DEBUG_PKT( if (hasNonSimpleNonPointerTargetType) outs() << "    has pointer operand with non-simple/non-pointer type!\n"; );

		DEBUG_PKT( outs() << "    requires splitting ('scatter')!\n"; );
		const bool requiresIfCascade = hasNonFullyUniformMask;

		return requiresIfCascade ? AnalysisResults::SPLIT_FULL_GUARDED : AnalysisResults::SPLIT_FULL;
	}

	// A GEP has to be split if
	// - it has any VARYING indices (uniform or varying does not matter)
	SplitInfo analyzeGEPForSplitting(const GetElementPtrInst* gep) {
		assert (gep);

#ifndef PACKETIZER_DISABLE_MEMOP_VECTORIZATION
		if (!hasVaryingIndex(gep)) return AnalysisResults::SPLIT_NEVER;
#endif

		DEBUG_PKT( outs() << "  requires splitting (has varying index)!\n"; );

		// GEPs never require guards
		return AnalysisResults::SPLIT_FULL;
	}

	// A phi has to be split if
	// - it has pointer-type and the predecessors do not have uniform entries
	// - it has pointer-type and the incoming values have to be split
	//   -> secondary target in this case!
	// NOTE: A pointer-phi does not have to be split in all cases, only if it
	//       will really hold W different pointers at runtime - then, it does
	//       not matter whether it will be a pointer-select or still be a
	//       pointer-phi (in case of uniform control-flow), we have to split in
	//       both cases.
	SplitInfo analyzePhiForSplitting(const PHINode* phi) {
		assert (phi);

		if (!phi->getType()->isPointerTy()) return AnalysisResults::SPLIT_NEVER;

		const BasicBlock* parentBB = phi->getParent();
		assert (!parentBB->getUniquePredecessor() && "what is a phi doing in a block with only one predecessor?");

		//if (Loop* loop = loopInfo->getLoopFor(parentBB)) {
			//if (loop->getHeader() == parentBB) return AnalysisResults::SPLIT_NEVER;
		//}

		typedef GraphTraits<Inverse<const BasicBlock*> > InvBlockTraits;
		InvBlockTraits::ChildIteratorType PI = InvBlockTraits::child_begin(parentBB);
		const BasicBlock* pred0BB = *PI++;
		assert (PI != InvBlockTraits::child_end(parentBB) && "block must not have less than 2 predecessors!");
		const BasicBlock* pred1BB = *PI;
		assert (++PI == InvBlockTraits::child_end(parentBB) && "block must not have more than 2 predecessors!");

		if (!analysisResults->hasUniformEntry(pred0BB) || !analysisResults->hasUniformEntry(pred1BB)) {
			DEBUG_PKT( outs() << "  requires splitting (pointer-phi with "
					<< "predecessors with VARYING entry!\n"; );
			// phis never require guards
			return AnalysisResults::SPLIT_FULL;
		}

		return AnalysisResults::SPLIT_NEVER;
	}

	// A select has to be split if
	// - it is a pointer-select and not uniform
	SplitInfo analyzeSelectForSplitting(const SelectInst* select) {
		assert (select);

		if (analysisResults->isUniform(select) || !select->getType()->isPointerTy()) {
			return AnalysisResults::SPLIT_NEVER;
		}

		DEBUG_PKT( outs() << "  requires splitting (has pointer values)!\n"; );

		// selects never require guards
		return AnalysisResults::SPLIT_FULL;
	}

	// A call has to be split if there is no vectorized function mapping
	// available.
	SplitInfo analyzeCallForSplitting(const CallInst* call,
									  NativeMethods& nativeMethods,
									  const Packetizer::PacketizerInfo& info)
	{
		assert (call);

		const Function* callee = call->getCalledFunction();
		if (!callee) return AnalysisResults::SPLIT_FULL_GUARDED;

		Function* vecF = nativeMethods.getNativeFunction(callee->getName(),
														 info.module,
														 info.simdWidth);

		if (vecF) return AnalysisResults::SPLIT_NEVER;

		DEBUG_PKT( outs() << "  requires splitting (no native function mapping"
				<< "found)!\n"; );

		// NOTE: For calls, an if-cascade is often less costly than executing
		//       info.simdWidth calls. Therefore, we currently ignore the fact
		//       that some calls might be free from side effects and could be
		//       executed without guards even if the mask is varying.
		const bool requiresIfCascade =
			!analysisResults->hasFullyUniformEntry(call->getParent()) /*&& !nativeMethods.isSideEffectFree(f)*/;

		return requiresIfCascade ? AnalysisResults::SPLIT_FULL_GUARDED : AnalysisResults::SPLIT_FULL;
	}


	// Returns true if the GEP does not require splitting, false otherwise.
	inline bool hasVaryingIndex(const GetElementPtrInst* gep) const {
		assert (gep);
		for (GetElementPtrInst::const_op_iterator IDX=gep->idx_begin(), IDXE=gep->idx_end(); IDX!=IDXE; ++IDX) {
			assert (isa<Value>(IDX));
			const Value* idxV = cast<Value>(IDX);
			if (!analysisResults->isUniform(idxV)) return true;
			//if (!analysisResults->isSame(idxV) && !analysisResults->isConsecutive(idxV)) return true; // TODO: is that better?
		}
		return false;
	}

private:
	//
	// second part: analyze uses/operands of primary split targets
	//

	// Mark values that are only used as operands in instructions that will be
	// split up. These do not have to be split themselves, but only their result
	// has to be split (-> SPLIT_RESULT).
	// Thus, we do not have to care about the mask in this case.
	// Uniform and constant operands are not split but replicated, but this
	// is done automatically during splitting, so we ignore them here.
	void markSplitOperand(const Value* value, SplitInfoMapType& secondarySplitMap) {
		assert (value);

		if (secondarySplitMap.find(value) != secondarySplitMap.end()) {
			DEBUG_PKT( outs() << "  operand already in secondarySplitMap: "
					<< *value << " - ignored!\n"; );
			return;
		}

		// Constants, pointer arguments, and uniform values must never be split.
		if (isa<Constant>(value) || analysisResults->isUniform(value) ||
				(isa<Argument>(value) && value->getType()->isPointerTy()))
		{
			DEBUG_PKT( outs() << "  adding constant, pointer argument, or "
					<< "uniform operand to secondarySplitMap: " << *value
					<< " as SPLIT_REPLICATE!\n"; );

			secondarySplitMap.insert(std::make_pair(value, AnalysisResults::SPLIT_REPLICATE));
			return;
		}

		assert (isa<Instruction>(value) || isa<Argument>(value));

		DEBUG_PKT( outs() << "  adding non-constant, non-uniform operand to "
				<< "secondarySplitMap: " << *value << " as SPLIT_RESULT!\n"; );

		secondarySplitMap.insert(std::make_pair(value, AnalysisResults::SPLIT_RESULT));
	}

	// Mark uses of instructions that will be split up. These also have to be
	// split up until a result can be merged to a vector again.
	void markSplitInstUses(const Instruction* inst, SplitInfoMapType& secondarySplitMap) {
		assert (inst);
		assert (!analysisResults->isUniform(inst) &&
					"must not attempt to split uniform instruction!");

		// If this is a "mergeable" instruction, we can stop splitting.
		if (instructionIsMergeable(inst)) {
			DEBUG_PKT( outs() << "  instruction is mergeable, uses do not have"
					<< " to be split: " << *inst << "\n"; );
			return;
		}

		for (Instruction::const_op_iterator O=inst->op_begin(),
				OE=inst->op_end(); O!=OE; ++O)
		{
			assert (!isa<Function>(*O) &&
					"call was not properly added to split map");
			if (isa<BasicBlock>(*O)) continue; // inst is a phi

			markSplitOperand(cast<Value>(*O), secondarySplitMap);
		}

		for (Instruction::const_use_iterator U=inst->use_begin(),
				UE=inst->use_end(); U!=UE; ++U)
		{
			assert (isa<Instruction>(*U));
			const Instruction* useI = cast<Instruction>(*U);

			assert (!analysisResults->isUniform(useI) &&
					"must not attempt to split uniform use!");

			SplitInfoMapType::iterator it = secondarySplitMap.find(useI);
			if (it != secondarySplitMap.end()) {
				DEBUG_PKT( outs() << "  use already in secondarySplitMap: "
						<< *useI << " marked as "
						<< AnalysisResults::getSplitInfoString(it->second); );
				if (it->second == AnalysisResults::SPLIT_FULL ||
						it->second == AnalysisResults::SPLIT_FULL_GUARDED)
				{
					DEBUG_PKT( outs() << " - ignored!\n"; );
				} else {
					DEBUG_PKT( outs() << " - overwriting as SPLIT_FULL!\n"; );
					it->second = AnalysisResults::SPLIT_FULL;
					markSplitInstUses(useI, secondarySplitMap);
				}

				continue;
			}

			// We only mark primary targets as SPLIT_FULL_GUARDED, therefore we
			// can ignore whether the parent block is uniform or varying here.
			DEBUG_PKT( outs() << "  adding use to secondarySplitMap: "
					<< *useI << " as SPLIT_FULL!\n"; );

			// NOTE: This should never be a store. These always have to be
			//       discovered as primary targets.
			assert (!isa<StoreInst>(useI) &&
					"should have been marked already!\n");

			secondarySplitMap.insert(std::make_pair(useI, AnalysisResults::SPLIT_FULL));

			markSplitInstUses(useI, secondarySplitMap);
		}
	}

	inline bool instructionIsMergeable(const Instruction* inst) const {
		assert (inst);

		Type* type = inst->getType();

		// Pointers can not be merged to a vector.
		if (type->isPointerTy()) return false;

		// Every other value type that is packetizable can be merged.
		return Packetizer::isPacketizableInstructionType(type);
	}



	////////////////////////////////////////////////////////////////////////////
	//                  INPUT-INDEPENDENT VALUE OPTIMIZATION                  //
	////////////////////////////////////////////////////////////////////////////


	void findVaryingInstructionsThatCanRemainScalar() {

		// Store information about input-independent VARYING values.
		DEBUG_PKT( outs() << "testing if input-independent values depend on VARYING loops...\n"; );
		for (AnalysisResults::inputindependentvalue_iterator
				it=analysisResults->inputindependentvalue_begin(),
				E=analysisResults->inputindependentvalue_end(); it!=E; ++it)
		{
			const Instruction* value = it->first;
			DEBUG_PKT( outs() << "  input-independent value: " << *value << "\n"; );
			const Loop* loop = loopInfo->getLoopFor(value->getParent());

			if (it->second) {
				DEBUG_PKT( outs() << "    already marked as VARYING_DUE_TO_VARYING_LOOP!\n"; );
				continue;
			}

			for (Instruction::const_op_iterator O=value->op_begin(), OE=value->op_end(); O!=OE; ++O) {
				if (!isa<Instruction>(*O)) continue;
				Instruction* opI = cast<Instruction>(*O);

				// 'value' is input-independent, so its operands also have to be.
				assert (analysisResults->isInputIndependent(opI));

				const Loop* opLoop = loopInfo->getLoopFor(opI->getParent());
				if (!opLoop) continue;
				if (analysisResults->isUniform(opLoop)) continue;

				//if (!opLoop->contains(value))
				if (opLoop != loop) {
					// If operand and value are not in the same VARYING loop, the value must not remain scalar.
					// Subloops also have to count as different loops. // TODO: the else-case should cover this (dependence across loop iterations)!
					DEBUG_PKT( outs() << "    operand is in VARYING loop: " << *opI << " - value can not be kept scalar (marked as VARYING_DUE_TO_VARYING_LOOP)!\n"; );
					it->second = true;

					// If the value has input-independent uses, these have to be marked as well.
					markInputIndependentUsesVaryingDueToVaryingLoop(value);
				} else {
					// TODO: this breaks mandelbrot performance again but fixes mx04...
					// If operand and value are in the same VARYING loop but depend on each other across loop iterations, they also must not remain scalar.
					if (isa<PHINode>(value) && loop->getHeader() == value->getParent() && !operandUsesValue(opI, value)) {
						DEBUG_PKT( outs() << "    operand is live across loop boundary of VARYING loop: " << *opI << " - value can not be kept scalar (marked as VARYING_DUE_TO_VARYING_LOOP)!\n"; );
						it->second = true;
						// If the value has input-independent uses, these have to be marked as well.
						markInputIndependentUsesVaryingDueToVaryingLoop(value);
					}
				}
			}
		}

		// Collect input-independent values that are allowed to remain scalar
		std::set<const Instruction*> inputIndepVecSet;
		for (Function::const_iterator BB=source.begin(), BBE=source.end(); BB!=BBE; ++BB) {
			for (BasicBlock::const_iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {

				std::set<const Instruction*> visitedSet;
				if (analyzeVaryingInstToRemainScalar(I, inputIndepVecSet, scalarSet, visitedSet)) {
					if (!analysisResults->isUniform(I)) markInstToRemainScalar(I, scalarSet);
					continue;
				}

				// Make sure all uses of instructions that can not remain scalar
				// are also in the set (this implicitly adds I).
				markInstForVectorization(I, inputIndepVecSet);
			}
		}

		// Remove all instructions from scalarSet that are in vecSet
		for (std::set<const Instruction*>::const_iterator it=inputIndepVecSet.begin(), E=inputIndepVecSet.end(); it!=E; ++it) {
			std::set<const Instruction*>::iterator scIt = scalarSet.find(*it);
			if (scIt == scalarSet.end()) continue;
			DEBUG_PKT( outs() << "  removing instruction from scalarSet: " << **scIt << "\n"; );
			scalarSet.erase(scIt);
		}

		// store results

		for (std::set<const Instruction*>::const_iterator it=scalarSet.begin(), E=scalarSet.end(); it!=E; ++it) {
			analysisResults->addScalarInstruction(*it);
		}

	}

	// Check if this instruction can remain UNIFORM although it was marked
	// VARYING by the vectorization analysis.
	// This is only true for instructions that do not depend on any input-
	// parameter and are not used in instructions that require vectorization
	// in any case (e.g. Select due to its mask usage or load/store/call).
	// The reason behind this is that we introduce a result-vector for these if
	// they are computed inside a loop and used outside. This result vector is
	// always blended correctly, so we can perform all operations inside the
	// same loop iteration in a scalar register.
	bool analyzeVaryingInstToRemainScalar(const Instruction* I,
										  std::set<const Instruction*>& inputIndepVecSet,
										  std::set<const Instruction*>& inputIndepScalarSet,
										  std::set<const Instruction*>& visitedSet)
	{
		assert (I);

		if (analysisResults->isUniform(I)) return true; // UNIFORM values can always remain scalar
		if (analysisResults->requiresSplitFull(I) || analysisResults->requiresSplitFullGuarded(I)) return false; // SPLIT values must never remain scalar
		if (inputIndepVecSet.find(I) != inputIndepVecSet.end()) return false; // already marked -> must not remain scalar

		if (visitedSet.find(I) != visitedSet.end()) return true; // break loops
		visitedSet.insert(I);

		DEBUG_PKT( outs() << "  testing if VARYING instruction can remain scalar: " << *I << "\n"; );

		// always vectorize VARYING masks
		// NOTE: Bad for comparisons, e.g. iter < maxIterations in Mandelbrot is also a mask.
		//if (analysisResults->isMask(I)) {
			//DEBUG_PKT( outs() << "    is VARYING mask - must be vectorized!\n"; );
			//return false;
		//}

		// always vectorize instructions that are marked VARYING_DUE_TO_VARYING_LOOP
		if (analysisResults->isInputIndependent(I) && analysisResults->isVaryingDueToVaryingLoop(I)) {
			DEBUG_PKT( outs() << "    marked as VARYING_DUE_TO_VARYING_LOOP - must be vectorized!\n"; );
			return false;
		}

		// never vectorize input-independent values if in FULLY_UNIFORM block.
		// TODO: unsure if this does not break something
		//if (analysisResults->hasFullyUniformEntry(I->getParent())) {
			//DEBUG_PKT( outs() << "    parent block is FULLY_UNIFORM - can remain scalar!\n"; );
			//return true;
		//}

		// always vectorize instructions that depend on a non-FULLY_UNIFORM mask.
		if (!analysisResults->hasFullyUniformEntry(I->getParent()) &&
				(isa<LoadInst>(I) ||
				 isa<StoreInst>(I) ||
				 isa<CallInst>(I) ||
				 isa<SelectInst>(I)))
		{
			DEBUG_PKT( outs() << "    instruction type depends on non-FULLY_UNIFORM mask - must be vectorized!\n"; );
			return false;
		}

		// always vectorize instructions that have VARYING operands
		for (Instruction::const_op_iterator O=I->op_begin(), OE=I->op_end(); O!=OE; ++O) {
			if (isa<BasicBlock>(*O)) continue;
			if (isa<Function>(*O)) continue;
			if (analysisResults->isUniform(*O)) continue;

			// If the operand is not UNIFORM, our only "chance" is that this is an input-independent instruction.
			if (!isa<Instruction>(*O)) {
				DEBUG_PKT( outs() << "    has non-UNIFORM, non-instruction operand: " << **O << " - must be vectorized\n"; );
				return false;
			}
			const Instruction* opI = cast<Instruction>(*O);
			if (analysisResults->isInputIndependent(opI) && !analysisResults->isVaryingDueToVaryingLoop(opI)) {
				continue;
			}

			DEBUG_PKT( outs() << "    has non-UNIFORM, non-input-independent operand: " << *opI << " - must be vectorized\n"; );
			return false;
		}

#if 0
		// always vectorize VARYING instructions that are used by other VARYING instructions
		// (except for result-blends, which explicitly allow broadcasting *before* blending).
		for (Instruction::const_use_iterator U=I->use_begin(), UE=I->use_end(); U!=UE; ++U) {
			assert (isa<Instruction>(*U));
			const Instruction* useI = cast<Instruction>(*U);

			// TODO: HERE!
			// There is no such thing as result vectors yet.
			//if (useI->getName().startswith("result.vec")) continue;

			// If both values are in the same loop (or in no loop), test normally (skip this part).
			// Otherwise, the use can allow I to remain scalar even if it is VARYING etc., because
			// it is outside a loop and therefore will be replaced by a result-blend during select generation.
			if (Loop* loop = loopInfo->getLoopFor(I->getParent())) {
				Loop* useLoop = loopInfo->getLoopFor(useI->getParent());
				if (loop != useLoop) continue;
			}

			// The same goes for simple phi operations that will be transformed to selects.
			if (isa<PHINode>(useI) && !loopInfo->isLoopHeader(const_cast<BasicBlock*>(useI->getParent()))) continue;

			if (analyzeVaryingInstToRemainScalar(useI, inputIndepVecSet, inputIndepScalarSet, visitedSet)) {
				markInstToRemainScalar(useI, inputIndepScalarSet);
				continue;
			}
			DEBUG_PKT( outs() << "    has non-result-blend-use that must be vectorized: " << *useI << " - must be vectorized\n"; );
			markInstForVectorization(useI, inputIndepVecSet);
			return false;
		}
#endif

		DEBUG_PKT( outs() << "  instruction can remain scalar: " << *I << "\n"; );
		return true;
	}

	void markInstForVectorization(const Instruction* I, std::set<const Instruction*>& inputIndepVecSet) {
		assert (I);
		assert (!analysisResults->isUniform(I));

		if (inputIndepVecSet.find(I) != inputIndepVecSet.end()) return;
		inputIndepVecSet.insert(I);
		DEBUG_PKT( outs() << "    instruction marked for vectorization: " << *I << "\n"; );

		// We know that we have to vectorize uses of this instruction as well.
		// In order to do prevent "full" checking of the uses later-on,
		// recurse and mark all uses that are also input-independent.
		for (Instruction::const_use_iterator U=I->use_begin(), UE=I->use_end(); U!=UE; ++U) {
			assert (isa<Instruction>(*U));
			const Instruction* useI = cast<Instruction>(*U);

			if (!analysisResults->isInputIndependent(useI)) continue;

			markInstForVectorization(useI, inputIndepVecSet);
		}
	}

	void markInstToRemainScalar(const Instruction* I, std::set<const Instruction*>& inputIndepScalarSet) {
		assert (I);
		if (inputIndepScalarSet.find(I) != inputIndepScalarSet.end()) return;
		inputIndepScalarSet.insert(I);
		DEBUG_PKT( outs() << "    instruction marked to prevent vectorization: " << *I << "\n"; );
	}


	////////////////////////////////////////////////////////////////////////////
	//                           MASK INFO ANALYSIS                           //
	////////////////////////////////////////////////////////////////////////////


	inline void analyzeMaskInfo() {
		for (Function::const_iterator BB=source.begin(), BBE=source.end(); BB!=BBE; ++BB) {
			for (BasicBlock::const_iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {
				if (const BranchInst* br = dyn_cast<BranchInst>(I)) {
					if (br->isConditional()) {
						markAsMask(br->getCondition());
					}
				} else if (const SelectInst* sel = dyn_cast<SelectInst>(I)) {
					markAsMask(sel->getCondition());
				}
			}
		}
	}

	void markAsMask(const Value* val) {
		assert (val);

		if (analysisResults->isMask(val)) return;
		if (!val->getType()->isIntegerTy(1)) return;

		analysisResults->setIsMask(val, true);

		// Stop at compare instructions
		if (isa<CmpInst>(val)) return;

		if (!isa<Instruction>(val)) return;
		const Instruction* valI = cast<Instruction>(val);

		// If this is no compare instruction, go backwards and mark values as MASK.
		for (Instruction::const_op_iterator O=valI->op_begin(), OE=valI->op_end(); O!=OE; ++O) {
			if (isa<BasicBlock>(*O)) continue;
			if (isa<Function>(*O)) continue;
			if (isa<Constant>(*O)) continue;

			const Value* opVal = cast<Value>(*O);
			markAsMask(opVal);
		}
	}


	////////////////////////////////////////////////////////////////////////////
	//                        RACE CONDITION DETECTION                        //
	////////////////////////////////////////////////////////////////////////////


	bool detectRaceConditions(const Packetizer::PacketizerInfo& info) const {

		bool raceConditionFound = false;

		for (Function::const_iterator BB=source.begin(), BBE=source.end();
				BB!=BBE; ++BB)
		{
			for (BasicBlock::const_iterator I=BB->begin(), IE=BB->end();
					I!=IE; ++I)
			{
				if (!isa<StoreInst>(I)) continue;

				const StoreInst* store = cast<StoreInst>(I);
				const Value* value = store->getValueOperand();
				const Value* pointer = store->getPointerOperand();

				if (!analysisResults->isSame(pointer)) continue;

				if (analysisResults->isUniform(value) || analysisResults->isSame(value)) {
					if (analysisResults->hasFullyUniformEntry(BB)) continue;

					// Both pointer and value are the same for all
					// instances, but the control-flow is varying. This
					// is a race condition that will have no visible effect
					// unless there is another, disjunct branch that writes
					// a different value to the same location.

					errs() << "\nWARNING: POSSIBLE RACE CONDITION DETECTED!\n"
						<< "         Store attempts to write UNIFORM value to "
						<< "UNIFORM pointer under VARYING control-flow. "
						<< "This may result in a 'visible' race condition if a "
						<< "store on a disjunct control-flow path attempts to "
						<< "write a different value to the same location!\n"
						<< "         Store  : " << *store << "\n"
						<< "         Value  : " << *value << "\n"
						<< "         Pointer: " << *pointer << "\n";

					continue;
				}

				// The pointer is the same for all instances, but a VARYING
				// value should be stored, this means that there is a race
				// condition.

#ifdef PACKETIZER_ERROR_ON_RACE_CONDITION
				errs() << "\nERROR: RACE CONDITION DETECTED!\n"
#else
				errs() << "\nWARNING: RACE CONDITION DETECTED!\n"
#endif
					<< "       Store attempts to write " << info.simdWidth
					<< " values to the same address!\n"
					<< "       Store  : " << *store << "\n"
					<< "       Value  : " << *value << "\n"
					<< "       Pointer: " << *pointer << "\n";

				raceConditionFound = true;

			}
		}

		return raceConditionFound;
	}


};

} // unnamed namespace

char VectorizationAnalysis::ID = 0;
INITIALIZE_PASS_BEGIN(VectorizationAnalysis, "vectorization analysis", "Vectorization Analysis", false, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_DEPENDENCY(LoopLiveValueAnalysis)
INITIALIZE_PASS_DEPENDENCY(PostDominatorTree)
INITIALIZE_PASS_DEPENDENCY(BranchInfoAnalysis)
INITIALIZE_PASS_END(VectorizationAnalysis, "vectorization analysis", "Vectorization Analysis", false, false)

namespace llvm {
	FunctionPass* createVectorizationAnalysisPass(const Packetizer::PacketizerInfo& info,
												 NativeMethods& nativeMethods,
												 Function& source,
												 Function& target,
												 const WholeFunctionVectorizer::NativeFunctionMapType& nativeFunctions,
												 const WholeFunctionVectorizer::ValueInfoMapType& userValueInfoMap,
												 bool* failed,
												 const bool verbose=false,
												 AnalysisResults* analysisResults=NULL)
	{
		return new VectorizationAnalysis(info, nativeMethods, source, target, nativeFunctions, userValueInfoMap, failed, verbose, analysisResults);
	}
}


#endif	/* _VECTORIZATIONANALYSIS_HPP */

