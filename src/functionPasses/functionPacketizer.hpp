/**
 * @file   functionPacketizer.hpp
 * @date   13.10.2009
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2008, 2009, 2010, 2011 Saarland University
 *
 */
#ifndef _FUNCTIONPACKETIZER_HPP
#define	_FUNCTIONPACKETIZER_HPP

#ifdef DEBUG_TYPE
#undef DEBUG_TYPE
#endif
#define DEBUG_TYPE "functionpacketizer"

#include "llvm/Support/raw_ostream.h"
#include <sstream> // stringstream
#include <stdexcept>

#include <llvm/Attributes.h>

#include "llvm/ADT/Statistic.h" //STATISTIC

#include "utils/packetizerInfo.hpp"
#include "vectorizationAnalysis.hpp"
#include "utils/analysisResults.hpp"
#include "utils/wholeFunctionVectorizationAAW.hpp"

// the following includes are only required for single-file compilation
#include <list>
#include "llvm/Transforms/Utils/Cloning.h" // ValueToValueMapTy
#include "utils/maskGraph.hpp"
#include "utils/llvmTools.hpp"
#include "utils/metadata.hpp"

// forward declaration of initializer
namespace llvm {
	void initializeFunctionPacketizerPass(PassRegistry&);
}

using namespace llvm;

STATISTIC(PacketizedInstructionCounter, "Counts number of packetized instructions");

namespace {

class FunctionPacketizer : public FunctionPass {
public:
	static char ID; // Pass identification, replacement for typeid
	FunctionPacketizer()
	: FunctionPass(ID),
			mInfo(NULL),
			source(NULL),
			target(NULL),
			failed(NULL),
			mVerbose(true)
	{}
	FunctionPacketizer(const Packetizer::PacketizerInfo* info,
					 Function* sourceFn,
					 Function* targetFn,
					 bool* failedFlag,
					 const bool verbose_flag=false)
	: FunctionPass(ID),
			mInfo(info),
			source(sourceFn),
			target(targetFn),
			failed(failedFlag),
			mVerbose(verbose_flag)
	{
		initializeFunctionPacketizerPass(*PassRegistry::getPassRegistry());
	}

	~FunctionPacketizer() {}

	virtual void releaseMemory() {
		for (std::map<Value*, std::set<BasicBlock*>* >::iterator it=entryMasks.begin(), E=entryMasks.end(); it!=E; ++it) {
			it->second->clear();
			delete it->second;
		}
		entryMasks.clear();
		constantArgs.clear();
        delete maskGraph;
	}

	virtual void getAnalysisUsage(AnalysisUsage &AU) const {
		//AU.addRequired<BranchInfoAnalysis>();    // dummy
		AU.addRequired<LoopLiveValueAnalysis>(); // dummy
		AU.addRequired<VectorizationAnalysis>();
		AU.addRequired<MaskGenerator>();
		//AU.addRequired<SelectGenerator>();       // dummy
		//AU.addRequired<CFGLinearizerNaive>();    // dummy

		AU.addPreserved<DominatorTree>();
		AU.addPreserved<PostDominatorTree>();
		AU.addPreserved<LoopInfo>();
		AU.addPreserved<BranchInfoAnalysis>();
		AU.addPreserved<LoopLiveValueAnalysis>();
		AU.addPreserved<VectorizationAnalysis>();
		AU.addPreserved<MaskGenerator>();
		AU.addPreserved<SelectGenerator>();
		AU.addPreserved<CFGLinearizerNaive>();
		AU.setPreservesCFG(); // doesn't this include DT/PDT/LI?
	}

	virtual bool runOnFunction(Function &f) {
		assert (source == &f);
		PacketizedInstructionCounter = 0;

		if (failed && *failed) return true;

		analysisResults               = getAnalysis<VectorizationAnalysis>().getAnalysisResults();
		const MaskGraph& oldMaskGraph = *getAnalysis<MaskGenerator>().getMaskGraph();

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "packetizing function '" << source->getNameStr() << "'...\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		try {
			packetizeFunction(source, target, oldMaskGraph);
		}
		catch (std::logic_error& error) {
			errs() << "\nException occurred during packetization of function: "
					<< error.what() << "\n";
			if (failed) *failed = true;
			return true;
		}
		catch (...) {
			errs() << "\nINTERNAL ERROR: Unexpected exception occurred during "
					<< "packetization of function!\n";
			if (failed) *failed = true;
			return true;
		}

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "packetization of function '" << source->getNameStr() << "' finished!\n"; );
		DEBUG_PKT( print(outs()); );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		assert (verify() && "verification of packetization failed!");
		DEBUG_PKT_NO_VERBOSE( verifyFunction(*target); );

		// We can only print the final packetized function here, because outside
		// the pass we don't know the name of the function without allowing to
		// query it.
		DEBUG_PKT( target->print(outs()); );

		return true;
	}

	void print(raw_ostream& o, const Module *M=NULL) const {
		o << "  instructions packetized: " << PacketizedInstructionCounter << "\n";
		// TODO:
		//control flow paths followed
		//loops packetized
		//ifs converted
		//...
	}

	bool verify() {
		DEBUG_PKT( outs() << "verifying packetization of function... "; );
		bool verified = true;

		// TODO: do sth :p
		verified = !*failed;

		if (verified) { DEBUG_PKT( outs() << "done.\n"; ); }
		else errs() << "verification of packetization failed!\n";
		return verified;
	}

private:
	const Packetizer::PacketizerInfo* mInfo;
	Function* source;
	Function* target;
	AnalysisResults* analysisResults;
	bool* failed;
	const bool mVerbose; // required for DEBUG makros, could be changed to use info.verbose

	MaskGraph* maskGraph;

	std::map<Value*, std::set<BasicBlock*>* > entryMasks;

	std::vector<bool> constantArgs; // only used in createSIMDWrapperFunction()

	struct SplitInfo {
		SplitInfo(Value* val, const bool replicate, const bool onlySplitResult, const bool requiresIfCascade)
				: oldValue(val), requiresReplication(replicate), requiresResultSplit(onlySplitResult), requiresGuards(requiresIfCascade), mergedResult(NULL)
		{
			assert (isa<Constant>(val) || isa<Instruction>(val) || isa<Argument>(val));
		}

		Value* oldValue;
		const bool requiresReplication;
		const bool requiresResultSplit;
		const bool requiresGuards; // "if cascade"
		std::vector<Value*> splitValues;
		std::vector<Instruction*> dummies;
		Instruction* mergedResult;
	};

	typedef std::map<Value*, SplitInfo*> SplitInfoMapType;


	/**
	 * creates 2 functions:
	 * - f_SIMD which handles most of the packetization and only works on the SIMD
	 *   width of the target architecture.
	 * - f_arr which works on arbitrary multiples of 'info.simdWidth' and loops over f_SIMD,
	 *   allocating temporary memory for each call if necessary (e.g. for structs)
	 *   and storing back all pointers after each call
	 *   (NOTE: f_arr is not created if info.simdWidth == info.packetizationSize)
	 * @param info.packetizationSize the overall packet size, independent of the target SIMD width
	 * @param f the scalar function that is to be packetized (will be destroyed!)
	 * @param extF the externally declared function that is replaced by the generated one
	 **/
	bool packetizeFunction(const Function* f, Function* f_SIMD, const MaskGraph& oldMaskGraph) {
		DEBUG_PKT( outs() << "\npacketizing function '" << f->getNameStr()
				<< "' in module '" << mInfo->mModule->getModuleIdentifier() << "'... \n\n"; );

		const std::string functionName = f->getNameStr();

		ValueToValueMapTy valueMap;
		Function::arg_iterator destI = f_SIMD->arg_begin();
		for (Function::const_arg_iterator I = f->arg_begin(), E = f->arg_end(); I != E; ++I) {
			if (valueMap.count(I) == 0) {     // Is this argument preserved?
				destI->setName(I->getNameStr()); // Copy the name over...
				valueMap[I] = destI++;        // Add mapping to ValueMap
			}
		}

		SmallVector<ReturnInst*, 10> returns;
		ClonedCodeInfo newFInfo;
		const char* nameSuffix = ".";

		DEBUG_PKT( verifyFunction(*f); );

		//clone without optimizing (need exactly the same function for mask-mapping)
		CloneFunctionInto(f_SIMD, f, valueMap, false, returns, nameSuffix, &newFInfo);

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "mapping masks...\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		LoopInfo li; // "useful" loop info not required here
		maskGraph = new MaskGraph(*f_SIMD, li, f_SIMD->getContext(), mVerbose);
		mapMaskGraph(oldMaskGraph, *maskGraph, valueMap);
		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "successfully mapped masks!\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );

		DEBUG_PKT( f->print(outs(), new WholeFunctionVectorizationAAW(*analysisResults)); );

		//--------------------------------------------------------------------------
		// Map vectorization analysis info (-> allows to query if an
		// instruction is uniform where required).
		//--------------------------------------------------------------------------
		// Add argument info
		for (Function::const_arg_iterator A=f->arg_begin(), AE=f->arg_end(); A!=AE; ++A) {
			assert (isa<Argument>(A));
			Argument* newArg = cast<Argument>(valueMap[A]);
			addValueInfo(newArg, cast<Argument>(A));
		}

		{
			// make sure input vector types are of 32bit types
			BasicBlock* entryBB = &f_SIMD->getEntryBlock();
			Instruction* insertBefore = entryBB->getFirstNonPHI();
			for (Function::arg_iterator A=f_SIMD->arg_begin(), AE=f_SIMD->arg_end(); A!=AE; ++A) {
				BitCastInst* bc = createBitCastToEquivalentPacketType(A, insertBefore);
				if (!bc) continue;
				ArrayRef<Value*> arg(A);
				MDNode* mtd = MDNode::get(*mInfo->mContext, arg);
				bc->setMetadata(Packetizer::WFV_META_ARG_CAST, mtd);

				Packetizer::uncheckedReplaceAllUsesWith(A, bc);
				bc->replaceUsesOfWith(bc, A);

				// Update splitting info of argument (does not
				// require any splitting anymore).
				setSplitInfo(A, AnalysisResults::SPLIT_NEVER);
			}
		}

		for (Function::const_iterator BB=f->begin(), BBE=f->end(); BB!=BBE; ++BB) {
			// Add instruction info.
			for (BasicBlock::const_iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {
				assert (isa<Instruction>(valueMap[I]));
				Instruction* newI = cast<Instruction>(valueMap[I]);
				addValueInfo(newI, I);

				// Add possible constant operands
				for (Instruction::const_op_iterator O=I->op_begin(), OE=I->op_end();
						O!=OE; ++O)
				{
					const Value* opVal = cast<Value>(*O);
					if (isa<BasicBlock>(opVal) || isa<Function>(opVal)) continue;

					Value* newOpVal = valueMap[opVal];
					assert (newOpVal);

					if (isa<Instruction>(newOpVal)) continue; // will be added later
					if (analysisResults->getValueInfo(newOpVal)) continue;

					DEBUG_PKT( outs() << "Value not in valueInfoMap yet: " << *newOpVal << "\n"; );
					assert (isa<Constant>(newOpVal));

					addValueInfo(newOpVal, true, true, false, true, false, false, false, false);
				}

				// Update inputIndependent-info
				if (f->getName().equals("__OpenCL_mandelbrot_kernel.tmp") &&
						analysisResults->isInputIndependent(I))
				{
					DEBUG_PKT( outs() << "Updating info of input-independent instruction: " << *I << "\n"; );
					analysisResults->updateInputIndependentValue(I, cast<Instruction>(valueMap[I]));
				}

				if (analysisResults->instCanRemainScalar(I))
					analysisResults->updateRemainScalarInfo(I, cast<Instruction>(valueMap[I]));
			}

			// Add block info.
			const AnalysisResults::BlockInfo* bi =
				analysisResults->getBlockInfo(BB);
			assert (isa<BasicBlock>(valueMap[BB]));
			BasicBlock* newBlock = cast<BasicBlock>(valueMap[BB]);
			analysisResults->addBlockInfo(newBlock,
											   bi->isNonDivergent,
											   bi->isFullyNonDivergent,
											   bi->hasUniformExit,
											   bi->hasFullyUniformExit);
		}

		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "collecting instructions to be packetized...\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		std::list<Instruction*> workList;

		// Collect instructions that have to be vectorized:
		// - exclude those marked VARYING / INDEX_CONSECUTIVE
		// - exclude those marked UNIFORM and/or INDEX_SAME
		// - exclude those marked SPLIT_FULL or SPLIT_FULL_GUARDED
		// - exclude those independent of input values except if they are used outside a VARYING loop
		// - include all others that are marked VARYING
		for (Function::iterator BB=f_SIMD->begin(), BBE=f_SIMD->end(); BB!=BBE; ++BB) {
			for (BasicBlock::iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {

				DEBUG_PKT( outs() << "instruction: " << *I << "\n"; );

				if (analysisResults->instCanRemainScalar(I)) {
					DEBUG_PKT( outs() << "  is independent of VARYING input and can remain scalar - ignored!\n"; );
					continue;
				}

				if (analysisResults->isUniform(I)) {
					DEBUG_PKT( outs() << "  is marked UNIFORM - ignored!\n"; );
					continue;
				}

				if (!I->getType()->isPointerTy() &&
						analysisResults->isConsecutive(I))
				{
					DEBUG_PKT( outs() << "  non-pointer-type instruction is marked CONSECUTIVE - ignored!\n"; );
					continue;
				}

				// ignore bitcasts of <2 x i64> etc. function arguments
				BitCastInst* bc = dyn_cast<BitCastInst>(I);
				if (isa<BitCastInst>(I) &&
						isa<Argument>(bc->getOperand(0)) &&
						isPacketizedType(bc->getType()) &&
						isPacketizedType(bc->getOperand(0)->getType()))
				{
					DEBUG_PKT( outs() << "  instruction is argument-bitcast - ignored!\n"; );
					continue;
				}

				assert (!analysisResults->isSame(I) && "INDEX_SAME but not UNIFORM or CONSECUTIVE?!?!");
				assert ((I->getType()->isPointerTy() || analysisResults->isRandom(I)) && "Not UNIFORM, INDEX_SAME, or INDEX_CONSECUTIVE, but also not INDEX_RANDOM?!?!?");
				assert (!analysisResults->requiresReplication(I) && "REPLICATE but not UNIFORM?!?!");

				if (analysisResults->requiresSplitFull(I) ||
					analysisResults->requiresSplitFullGuarded(I))
				{
					DEBUG_PKT( outs() << "  is marked SPLIT_FULL - ignored!\n"; );
					continue;
				}

				workList.push_back(I);
			}
		}

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "successfully collected instructions to be packetized!\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );


		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "packetizing instructions...\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );


		// collect split values
		// TODO: remove splitInfoMap stuff (#16)!
		SplitInfoMapType splitInfoMap;

		for (AnalysisResults::iterator V=analysisResults->begin(),
				VE=analysisResults->end(); V!=VE; ++V)
		{
			AnalysisResults::ValueInfo* info = V->second;
			Value* value = info->value;

			// We must only iterate over values of the new function!
			if (Instruction* valI = dyn_cast<Instruction>(value)) {
				if (valI->getParent()->getParent() != f_SIMD) continue;
			}
			if (Argument* valA = dyn_cast<Argument>(value)) {
				if (valA->getParent() != f_SIMD) continue;
			}

			SplitInfo* splitInfo = NULL;

			switch (info->splitInfo) {
				case AnalysisResults::SPLIT_NEVER: continue;

				case AnalysisResults::SPLIT_REPLICATE:
				{
					splitInfo = new SplitInfo(value, true, false, false);
					break;
				}

				case AnalysisResults::SPLIT_RESULT:
				{
					splitInfo = new SplitInfo(value, false, true, false);
					break;
				}

				case AnalysisResults::SPLIT_FULL:
				{
					splitInfo = new SplitInfo(value, false, false, false);
					break;
				}

				case AnalysisResults::SPLIT_FULL_GUARDED:
				{
					splitInfo = new SplitInfo(value, false, false, true);
					break;
				}

				default:
				{
					errs() << "bad split-info case found ("
							<< analysisResults->getSplitInfoString(info->splitInfo)
							<< ") for value: " << *value << "\n";
					assert (false && "should never happen!");
					throw std::logic_error("INTERNAL ERROR: should never happen!");
				}
			}

			splitInfoMap.insert(std::make_pair(value, splitInfo));
		}



		//--------------------------------------------------------------------------
		// packetize instructions in worklist
		//--------------------------------------------------------------------------
		DEBUG_PKT( outs() << "packetizing instructions in worklist... \n"; );

		for (std::list<Instruction* >::iterator i = workList.begin(),
				e = workList.end(); i != e; ++i)
		{
			Instruction* I = cast<Instruction>(*i);

			if (!packetizeInstruction(I, splitInfoMap)) {
				DEBUG_PKT( errs() << "INTERNAL ERROR: Packetization of "
						<< "instruction failed: " << *I << "\n"; );
				throw std::logic_error("INTERNAL ERROR: Packetization of instruction failed!");
			}
		}

		DEBUG_PKT( outs() << "packetization of worklist finished.\n"; );
		DEBUG_PKT( outs() << "-----------------------------------------------------\n\n"; );


		//--------------------------------------------------------------------------
		// Perform splitting of values.
		//--------------------------------------------------------------------------
		DEBUG_PKT( outs() << "\n-----------------------------------------------------\n"; );
		DEBUG_PKT( outs() << "Splitting values... \n"; );

		if (!splitAllInstructions(splitInfoMap)) {
			assert (false && "splitting of instructions failed!");
			throw std::logic_error("INTERNAL ERROR: splitting of instructions failed!");
		}

		DEBUG_PKT( outs() << "Splitting of instructions finished.\n"; );
		DEBUG_PKT( outs() << "-----------------------------------------------------\n\n"; );

		//--------------------------------------------------------------------------
		// broadcast non-packetized operands of packetized instructions
		//--------------------------------------------------------------------------
		broadcastNonPacketizedValues(f_SIMD);

#ifdef PACKETIZER_USE_SCALAR_MASKS
		//--------------------------------------------------------------------------
		// adjust mask constants (i1 -> i32) (masks are not vectorized)
		//--------------------------------------------------------------------------
		// recurse from branches and check all uses for i1 constants
		std::set<Instruction*> maskVec;
		for (Function::iterator BB=f_SIMD->begin(), BBE=f_SIMD->end(); BB!=BBE; ++BB) {
			assert (BB->getTerminator());
			if (!isa<BranchInst>(BB->getTerminator())) continue;
			BranchInst* brInst = cast<BranchInst>(BB->getTerminator());
			DEBUG_PKT( outs() << "  adjusting block '" << BB->getNameStr() << "' with branch: " << *brInst << "\n"; );

			//ignore this block if its exit-branch is either unconditional or its condition is of scalar type
			if (brInst->isUnconditional()) {
				DEBUG_PKT( outs() << "    needs no adjustment (unconditional branch)\n"; );
				continue;
			}

			Value* cond = brInst->getCondition();
			Type* int32Ty = Type::getInt32Ty(*mInfo->mContext);
			if (cond->getType() == int32Ty) continue;

			// add cond to set and collect use-paths
			// TODO: HERE!

			assert (false && "NOT IMPLEMENTED!");
			throw std::logic_error("INTERNAL ERROR: NOT IMPLEMENTED!");
		}
#endif

		//--------------------------------------------------------------------------
		// packetize branches
		//--------------------------------------------------------------------------
		packetizeBranches(f_SIMD);

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "packetization of instructions finished!\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );


		//--------------------------------------------------------------------------
		// clean up
		//--------------------------------------------------------------------------
		{
			// Insert bitcasts and modify the return instructions
			// if the function returns a different vector type.
			for (Function::iterator BB=f_SIMD->begin(), BBE=f_SIMD->end(); BB!=BBE; ++BB) {
				TerminatorInst* term = BB->getTerminator();
				if (!isa<ReturnInst>(term)) continue;
				ReturnInst* ret = cast<ReturnInst>(term);
				assert (ret->getNumOperands() <= 1 && "returns with more than one operand not supported yet!");
				Value* retVal = ret->getReturnValue();

				Type* type = f_SIMD->getReturnType();
				if (type == VectorType::get(Type::getInt64Ty(*mInfo->mContext), 2)) {
					BitCastInst* bc = new BitCastInst(retVal, type, "", ret);
					addValueInfo(bc, ret);
					ret->mutateType(type);
				}
				if (type == VectorType::get(Type::getDoubleTy(*mInfo->mContext), 2)) {
					BitCastInst* bc = new BitCastInst(retVal, type, "", ret);
					addValueInfo(bc, ret);
					ret->mutateType(type);
				}
			}
		}
		removeUnnecessaryCasts(f_SIMD);

		DEBUG_PKT( f_SIMD->print(outs(), new WholeFunctionVectorizationAAW(*analysisResults)); );

		// Verify.
		// Do not verify vectorization analysis -> we don't care anymore here,
		// plus it is only constants that are missing a mark.
		DEBUG_PKT( Packetizer::writeFunctionToFile(f_SIMD, f_SIMD->getNameStr()+".pkt.unopt.ll"); );
		DEBUG_PKT( verifyFunction(*f_SIMD); );

		DEBUG_PKT( Packetizer::writeFunctionToFile(f_SIMD, f_SIMD->getNameStr()+".pkt.ll"); );
		DEBUG_PKT( Packetizer::writeModuleToFile(f_SIMD->getParent(), f_SIMD->getNameStr()+".mod.pkt.ll"); );

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "generation of SIMD-function finished!\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		//--------------------------------------------------------------------------
		// create wrapper for arbitrary packetization sizes
		//--------------------------------------------------------------------------
		Function* f_arr = createSIMDWrapperFunction(f_SIMD, functionName + "_WRAPPER");
		if (!f_arr) {
			std::stringstream sstr;
			sstr << "INTERNAL ERROR: Could not create wrapper for packetization"
					<< " size " << mInfo->mPacketizationSize << "! (SIMD width: "
					<< mInfo->mSimdWidth;
			throw std::logic_error(sstr.str());
		}

		//save name
		std::string extName = f_SIMD->getNameStr();

		//replace extern call with new function
		//replaceExternalFunctionUses(f_SIMD, f_arr);

		//set new function to saved name of external function
		f_arr->setName(extName);

		// final verification
		DEBUG_PKT( verifyFunction(*f_arr); );


		DEBUG_PKT( outs() << "\npacketization of function " << functionName << " finished successfully.\n\n"; );

		return true;
	}


	BitCastInst*
	createBitCastToEquivalentPacketType(Value* oldVal, Instruction* insertBefore)
	{
		assert (oldVal && insertBefore);
		assert (!isa<BasicBlock>(oldVal) && !isa<Function>(oldVal));

		DEBUG_PKT( outs() << "creating bitcast to equivalent packet type for value: " << *oldVal << "\n"; );

		Type* oldType = oldVal->getType();

		if (!isPacketizedType(oldType)) {
			DEBUG_PKT( outs() << "  is no packet type - ignored!\n"; );
			return NULL;
		}

		if (oldType == mInfo->mVectorTyIntSIMD ||
				oldType == mInfo->mVectorTyFloatSIMD)
		{
			DEBUG_PKT( outs() << "  is of suitable packet type - ignored!\n"; );
			return NULL;
		}

		Type* newType = createEquivalentPacketType(oldType);
		if (!newType) {
			errs() << "ERROR: Could not create suitable packet type for bitcasting of"
					"value: " << *oldVal << "!\n";
			throw std::logic_error("INTERNAL ERROR: Could not create suitable packetized type for bitcast!");
		}

		BitCastInst* bc = new BitCastInst(oldVal, newType, "", insertBefore);
		addValueInfo(bc, oldVal);
		return bc;
	}

	Type*
	createEquivalentPacketType(Type* oldType)
	{
		assert (oldType);
		assert (isPacketizedType(oldType));

		if (oldType == mInfo->mVectorTyIntSIMD ||
				oldType == mInfo->mVectorTyFloatSIMD)
		{
			return oldType;
		}

		Type::TypeID oldTypeID = oldType->getTypeID();
		switch (oldTypeID) {

			case Type::VectorTyID:
			{
				VectorType* vType = cast<VectorType>(oldType);
				if (vType == VectorType::get(Type::getInt64Ty(*mInfo->mContext), 2) ||
						vType == VectorType::get(Type::getInt16Ty(*mInfo->mContext), 8) ||
						vType == VectorType::get(Type::getInt8Ty(*mInfo->mContext), 16))
				{
					return mInfo->mVectorTyIntSIMD;
				}
				if (vType == VectorType::get(Type::getDoubleTy(*mInfo->mContext), 2))
				{
					return mInfo->mVectorTyFloatSIMD;
				}
				errs() << "ERROR: bad vector type found: " << *vType << "\n";
				return NULL;
			}

			case Type::ArrayTyID:
			{
				ArrayType* aType = cast<ArrayType>(oldType);
				Type* elemType = aType->getElementType();
				if (!elemType) return NULL;
				Type* newElemType = createEquivalentPacketType(elemType);
				return ArrayType::get(newElemType, aType->getNumElements());
			}

			case Type::StructTyID:
			{
				StructType* sType = cast<StructType>(oldType);

				std::vector<Type*> elems;
				for (unsigned i=0; i<sType->getNumElements(); ++i) {
					Type* elemType = sType->getElementType(i);
					if (!elemType) return NULL;
					Type* newElemType = createEquivalentPacketType(elemType);
					elems.push_back(newElemType);
				}
				return StructType::get(*mInfo->mContext, elems, sType->isPacked());
			}
			case Type::PointerTyID:
			{
				PointerType* pType = cast<PointerType>(oldType);
				Type* elemType = pType->getElementType();
				if (!elemType) return NULL;
				Type* newElemType = createEquivalentPacketType(elemType);
				return PointerType::get(newElemType, pType->getAddressSpace());
			}

			default :
			{
				errs() << "ERROR: bad packet type found: " << *oldType << "\n";
				return NULL;
			}
		}
	}


	Instruction* createDummy(Type* type, Value* replacedVal, Instruction* insertBefore) {
		Constant* c = Constant::getNullValue(type);
		// we must not insert pointer selects ;)
		Instruction* dummy = NULL;
		if (type->isPointerTy()) {
			dummy = new BitCastInst(c, type, "dummy", insertBefore);
		} else {
			dummy = SelectInst::Create(Constant::getNullValue(Type::getInt1Ty(*mInfo->mContext)), c, c, "dummy", insertBefore);
		}
		// not adding these results in not being able to print temporary states
		addValueInfo(dummy, replacedVal);
		return dummy;
	}
	Instruction* createDummy(Type* type, Value* replacedVal, BasicBlock* insertAtEnd) {
		Constant* c = Constant::getNullValue(type);
		// we must not insert pointer selects ;)
		Instruction* dummy = NULL;
		if (type->isPointerTy()) {
			dummy = new BitCastInst(c, type, "dummy", insertAtEnd);
		} else {
			dummy = SelectInst::Create(Constant::getNullValue(Type::getInt1Ty(*mInfo->mContext)), c, c, "dummy", insertAtEnd);
		}
		// not adding these results in not being able to print temporary states
		addValueInfo(dummy, replacedVal);
		return dummy;
	}

	Constant* createPacketConstantIntFromBool(bool b) {
		std::vector<Constant*> cVec; //vector of 'info.simdWidth' int32
		Constant* const_int32 = b ? Constant::getAllOnesValue(Type::getInt32Ty(*mInfo->mContext)) : Constant::getNullValue(Type::getInt32Ty(*mInfo->mContext));
		for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
			cVec.push_back(const_int32);
		}
		return ConstantVector::get(ArrayRef<Constant*>(cVec));
	}

	Constant* createPacketConstant(Constant* oldC) {
		Constant *c = NULL;
		std::vector<Constant* > vecvec;

		if (isa<UndefValue>(oldC)) {
			c = UndefValue::get(Packetizer::packetizeSIMDType(oldC->getType(), *mInfo));
			DEBUG_PKT( outs() << "created new undefined packet constant: " << *c << "\n"; );
			// if not inside map already, add info
			if (c && !analysisResults->getValueInfo(c))
				addValueInfo(c, true, true, false, false, false, false, false, false);
			return c;
		}

		if (oldC->getType()->isIntegerTy(1)) {
			bool b = *cast<ConstantInt>(oldC)->getValue().getRawData();
			c = createPacketConstantIntFromBool(b);
			DEBUG_PKT( outs() << "created new packet constant: " << *c << "\n"; );
			// if not inside map already, add info
			if (c && !analysisResults->getValueInfo(c))
				addValueInfo(c, true, true, false, false, false, false, false, false);
			return c;
		}

		if (oldC->isNullValue()) {
			Constant* c = Constant::getNullValue(Packetizer::packetizeSIMDType(oldC->getType(), *mInfo));
			// if not inside map already, add info
			if (c && !analysisResults->getValueInfo(c))
				addValueInfo(c, true, true, false, true, false, false, false, false);
			return c;
		}

		// TODO: unsure whether this test makes sense
		if (oldC == Constant::getAllOnesValue(oldC->getType())) {
			Constant* c = Constant::getAllOnesValue(Packetizer::packetizeSIMDType(oldC->getType(), *mInfo));
			// if not inside map already, add info
			if (c && !analysisResults->getValueInfo(c))
				addValueInfo(c, true, true, false, false, false, false, false, false);
			return c;
		}

		for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
			vecvec.push_back(Packetizer::getMax32BitConstant(oldC, *mInfo->mContext));
		}

		Type::TypeID oldTypeID = oldC->getType()->getTypeID();
		switch (oldTypeID) {
			case Type::FloatTyID :
			{
				Type* simdType = Packetizer::packetizeSIMDType(oldC->getType(), *mInfo);
				assert (isa<VectorType>(simdType));
				c = ConstantVector::get(ArrayRef<Constant*>(vecvec));
				break;
			}
			case Type::IntegerTyID :
			{
				Type* simdType = Packetizer::packetizeSIMDType(oldC->getType(), *mInfo);
				assert (isa<VectorType>(simdType));
				c = ConstantVector::get(ArrayRef<Constant*>(vecvec));
				break;
			}
			case Type::PointerTyID :
			{
				errs() << "WARNING: called createPacketConstant() on pointer!\n";
				c = oldC;
				break;
				////http://www.nabble.com/Creating-Pointer-Constants-td22401381.html
				//ConstantInt* constInt = ConstantInt::get(info.context, Type::getInt64Ty(info.context), (uintptr_t)thePointer);
				//Value* constPtr = ConstantExpr::getIntToPtr(constInt, llvm::PointerType::getUnqual(Type::getInt32Ty(info.context)));
			}
			case Type::VectorTyID :
			{
				errs() << "WARNING: called createPacketConstant() on constant that is already packetized!\n";
				c = oldC;
				break;
			}
			case Type::ArrayTyID :
			{
				ConstantArray* oldArrC = cast<ConstantArray>(oldC);
				ArrayType* arrType = oldArrC->getType();

				assert (Packetizer::packetizeSIMDType(arrType, *mInfo)->isArrayTy());
				ArrayType* newArrType = cast<ArrayType>(Packetizer::packetizeSIMDType(arrType, *mInfo));
				assert (Packetizer::packetizeSIMDType(arrType->getElementType(), *mInfo) == newArrType->getElementType());
				const unsigned numVals = newArrType->getNumElements();
				// recursively create packet constants
				Constant** vals = new Constant*[numVals]();
				for (unsigned i=0; i<numVals; ++i) {
					vals[i] = createPacketConstant(oldArrC->getOperand(i));
					assert (vals[i]->getType() == newArrType->getElementType() && "types of array elements have to match");
					// if not inside map already, add info
					if (vals[i] && !analysisResults->getValueInfo(vals[i])) addValueInfo(vals[i], oldC);
				}
				c = ConstantArray::get(newArrType, ArrayRef<Constant*>(vals, numVals));

				delete [] vals;
				break;
			}
			case Type::StructTyID :
			{
				errs() << "ERROR: packetization of struct constants not implemented yet: " << *oldC << "\n";
				assert (false && "packetization of struct constants not implemented yet!");
				throw std::logic_error("INTERNAL ERROR: NOT IMPLEMENTED!");
			}

			default : errs() << "ERROR: can only packetize constants of types float, int and arrays!\n";
		}

		if (c) { DEBUG_PKT( outs() << "created new packet constant: " << *c << "\n"; ); }
		else {
			errs() << "ERROR: could not packetize constant: " << *oldC << "\n";
			assert (false && "should never happen!");
			throw std::logic_error("INTERNAL ERROR: Could not packetize constant - should never happen!");
		}

		// if not inside map already, add info
		if (c && !analysisResults->getValueInfo(c)) addValueInfo(c, oldC);

		return c;
	}

	// This function only performs a check if the given type is a valid
	// packet type, applying the type rules (e.g. SoA layout)
	bool isPacketizedType(Type* type) const {
		//first check most common types
		if (type == mInfo->mVectorTyFloatSIMD || type == mInfo->mVectorTyIntSIMD) return true;

		Type::TypeID oldTypeID = type->getTypeID();
		//if none of these, test for primitive types
		if (oldTypeID < Type::FirstDerivedTyID) return false;
		//if also none of these, test derived types
		switch (oldTypeID) {
			case Type::VoidTyID: errs() << "WARNING: void type checked!\n"; return false;
			//case Type::FloatTyID: return false;
			//case Type::IntegerTyID  return false;
			case Type::VectorTyID: return true;
			case Type::PointerTyID: return isPacketizedType(cast<PointerType>(type)->getElementType());
			case Type::ArrayTyID: return isPacketizedType(cast<ArrayType>(type)->getElementType());
			case Type::StructTyID:
			{
				StructType* sType = cast<StructType>(type);
				for (unsigned i=0; i<sType->getNumContainedTypes(); ++i) {
					if (!isPacketizedType(sType->getElementType(i))) return false;
				}
				return true;
			}
			default: return false;
		}
	}

	inline bool registerTypeSizeMatches(Type* typeA, Type* typeB) {
		if (typeA == typeB) return true;

		const uint64_t sizeA = mInfo->mTargetData->getTypeSizeInBits(typeA);
		const uint64_t sizeB = mInfo->mTargetData->getTypeSizeInBits(typeB);

		return sizeA == sizeB;
	}


	Value* mapValue(Value* oldVal, ValueToValueMapTy& valueMap) {
		assert (oldVal);
		assert (!valueMap.empty());
		Value* newVal = NULL;
		if (isa<Argument>(oldVal)) {
			ValueToValueMapTy::iterator tmp = valueMap.find(oldVal);
			if (tmp == valueMap.end()) errs() << "ERROR: argument value has no mapping: " << *oldVal << "\n";
			assert (tmp != valueMap.end() && "argument-value has to have a mapping!");
			newVal = tmp->second;
		} else if (isa<Constant>(oldVal)) {
			newVal = oldVal;
		} else if (isa<Instruction>(oldVal)) {
			ValueToValueMapTy::iterator tmp = valueMap.find(oldVal);
			if (tmp == valueMap.end()) errs() << "ERROR: instruction value has no mapping: " << *oldVal;
			assert (tmp != valueMap.end() && "instruction-value has to have a mapping!");
			newVal = tmp->second;
		} else {
			assert (false && "NOT IMPLEMENTED!");
			throw std::logic_error("INTERNAL ERROR: NOT IMPLEMENTED!");
		}
		assert (newVal && "new value must not be NULL!");
		DEBUG_PKT( outs() << "  value mapped:\n  " << *oldVal << "\n"; );
		DEBUG_PKT( outs() << "  -> " << *newVal << "\n"; );
		return newVal;
	}


	void mapMaskGraph(const MaskGraph& oldMaskGraph, MaskGraph& newMaskGraph, ValueToValueMapTy& valueMap) {
		assert (oldMaskGraph.verify() && "verification of mask graph of scalar function failed!");
		assert (newMaskGraph.isInitialized());

		for (MaskGraph::const_iterator it=oldMaskGraph.begin(), e=oldMaskGraph.end(); it!=e; ++it) {
			BasicBlock* oldBB = it->first;
			MaskGraphNode* oldNode = it->second;
			assert (oldNode);

			//find corresponding block in new function
			ValueToValueMapTy::iterator tmp = valueMap.find(oldBB);
			assert (tmp != valueMap.end() && "block has to have a mapping!");
			assert (isa<BasicBlock>(tmp->second));
			BasicBlock* newBB = cast<BasicBlock>(tmp->second);

			DEBUG_PKT( outs() << "  block mapped: '" << oldBB->getNameStr() << "' -> '" << newBB->getNameStr() << "'\n"; );

			//find corresponding values of mask in new function
			MaskGraphNode* newNode = newMaskGraph.findMaskNode(newBB);
			assert (newNode && "corresponding node for new block in mask graph of new function not found!");

			//set entry mask to mapped value
			Value* oldEntryMask = oldNode->getEntryMaskVal();
			assert (oldEntryMask);
			Value* newEntryMask = mapValue(oldEntryMask, valueMap);
			newNode->setEntryMask(newEntryMask);

			if (oldNode->hasExitEdge()) {
				//set mask of true-edge to mapped value
				Value* oldExitMaskT = oldNode->getExitMaskTrueVal();
				assert (oldExitMaskT);
				Value* newExitMaskT = mapValue(oldExitMaskT, valueMap);
				newNode->setExitMaskTrue(newExitMaskT);

				//update successor information (invalidated by linearization)
				ValueToValueMapTy::iterator tmp = valueMap.find(oldNode->getSuccessorTrue()->getBlock());
				assert (tmp != valueMap.end() && "block has to have a mapping!");
				assert (isa<BasicBlock>(tmp->second));
				MaskGraphNode* newSuccBB = newMaskGraph.findMaskNode(cast<BasicBlock>(tmp->second));
				assert (newSuccBB);
				newNode->setSuccessorTrue(newSuccBB);
			} else {
				newNode->removeSuccessorTrue();
			}
			if (oldNode->hasConditionalExit()) {
				//set mask of false-edge to mapped value
				Value* oldExitMaskF = oldNode->getExitMaskFalseVal();
				assert (oldExitMaskF);
				Value* newExitMaskF = mapValue(oldExitMaskF, valueMap);
				newNode->setExitMaskFalse(newExitMaskF);

				//update successor information (invalidated by linearization)
				ValueToValueMapTy::iterator tmp = valueMap.find(oldNode->getSuccessorFalse()->getBlock());
				assert (tmp != valueMap.end() && "block has to have a mapping!");
				assert (isa<BasicBlock>(tmp->second));
				MaskGraphNode* newSuccBB = newMaskGraph.findMaskNode(cast<BasicBlock>(tmp->second));
				assert (newSuccBB);
				newNode->setSuccessorTrue(newSuccBB);
			} else {
				newNode->removeSuccessorFalse();
			}

			//update predecessor information (invalidated by linearization)
			unsigned num = 0;
			for (MaskGraphNode::pred_iterator P=oldNode->pred_begin(), PE=oldNode->pred_end(); P!=PE; ++P, ++num) {
				ValueToValueMapTy::iterator tmp = valueMap.find((*P)->getBlock());
				assert (tmp != valueMap.end() && "pred block has to have a mapping!");
				assert (isa<BasicBlock>(tmp->second));
				MaskGraphNode* predNode = newMaskGraph.findMaskNode(cast<BasicBlock>(tmp->second));
				assert (predNode);
				if (newNode->getNumPredecessors() <= num) {
					newNode->setPredecessor(predNode, num);
				} else {
					newNode->addPredecessor(predNode);
				}
			}
			//if we still have more predecessors set, remove them
			if (newNode->getNumPredecessors() <= num) {
				for (unsigned i=num, e=newNode->getNumPredecessors(); i<e; ++i)
					newNode->removePredecessor(i);
			}

			//fill entryMask map
			std::map<Value*, std::set<BasicBlock*>* >::iterator tmp2 = entryMasks.find(newEntryMask);
			if (tmp2 == entryMasks.end()) {
				std::set<BasicBlock*>* blockSet = new std::set<BasicBlock*>();
				blockSet->insert(newBB);
				entryMasks.insert(std::make_pair(newEntryMask, blockSet));
			} else {
				std::set<BasicBlock*>* blockSet = tmp2->second;
				blockSet->insert(newBB);
			}
		}
		assert (newMaskGraph.size() == oldMaskGraph.size() && "new masks have to match old masks!");

		newMaskGraph.finalize();
		assert (newMaskGraph.verify() && "mapped mask graph could not be verified!");
		assert (!newMaskGraph.empty());
		assert (!entryMasks.empty());
	}

	/**
	 * checks instructions that have packet returntype for operands that should be
	 * but are not packetized and broadcasts them
	 * TODO: Have to handle return? -> has type void, regardless of value!
	 **/
	void broadcastNonPacketizedValues(Function* f_SIMD) {
		DEBUG_PKT( outs() << "\nreplicating non-packetized values...\n"; );

        outs() << *f_SIMD;

		// loop over all instructions, if they are packetized, check their operands
		for (Function::iterator BB=f_SIMD->begin(), BBE=f_SIMD->end(); BB!=BBE; ++BB) {
			DEBUG_PKT( outs() << "\n  testing block '" << BB->getNameStr() << "'...\n"; );
			for (BasicBlock::iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I) {

				// TODO: are we sure extract/insert can not happen in scalar code???
				if (isa<IntToPtrInst>(I)) continue; //ignore int2ptr (generated code from aligned alloc)
				if (isa<PtrToIntInst>(I)) continue; //ignore ptr2int (generated code from aligned alloc)
#ifdef PACKETIZER_DISABLE_MEMOP_VECTORIZATION
				if (isa<ExtractElementInst>(I)) {
					Value* vecOp = cast<ExtractElementInst>(I)->getVectorOperand();
					if (isPacketizedType(vecOp->getType())) continue;
					Value* newVecOp = broadcastValue(vecOp, I);
					I->replaceUsesOfWith(vecOp, newVecOp);
					continue;
				}
#else
                // Ignore code generated during splitting.
				if (isa<ExtractElementInst>(I)) continue; 
#endif
                // Ignore code generated during splitting.
				if (isa<InsertElementInst>(I)) continue;
				if (isa<ExtractValueInst>(I)) continue;
				if (isa<InsertValueInst>(I)) continue;

				// ignore our own introduced pointer casts
				if (isScalarToVectorPtrCast(I)) {
					DEBUG_PKT( outs() << "    found pointer cast - ignored (introduced during vectorization)!\n"; );
					assert (std::strstr(I->getNameStr().c_str(), "pktPtrCast") != 0 &&
							"bitcast has unexpected name - this means it was not introduced by ouselves!");
					continue;
				}

				//handle calls differently (match against signature)
				if (CallInst* call = dyn_cast<CallInst>(I)) {
					//if (std::strstr(call->getCalledFunction()->getNameStr().c_str(), "llvm.dbg") != 0) continue;
					DEBUG_PKT( outs() << "    checking arguments of call: " << *call << "\n"; );
					Function* callee = call->getCalledFunction();
					if (!callee) {
						// TODO: Should  this not have been split?
						DEBUG_PKT( outs() << "      has no known callee, skipped!\n"; );
						continue; // important e.g. for 'call i32 inttoptr (...)(...)'
					}
					//map arguments to supplied values and match their types
					CallInst::op_iterator OP = call->op_begin(); //set iterator to first supplied argument
					for (Function::arg_iterator A=callee->arg_begin(), AE=callee->arg_end(); A!=AE; ++A, ++OP) {
						Value* opV = cast<Value>(OP);
						DEBUG_PKT( outs() << "      testing operand: " << *opV << "\n"; );
						DEBUG_PKT( outs() << "      desired argument: " << *A << "\n"; );
						assert (isPacketizedType(A->getType()) || (!isPacketizedType(A->getType()) && !isPacketizedType(opV->getType())));
						if (isPacketizedType(A->getType()) && !isPacketizedType(opV->getType())) {
							DEBUG_PKT( outs() << "      is scalar, replicating...\n"; );
							//broadcast operand and insert before call
							Value* newOp = broadcastValue(opV, call);
							call->replaceUsesOfWith(opV, newOp);
						}
					}
					continue;
				}

				if (StoreInst* store = dyn_cast<StoreInst>(I)) {
					// if pointer type is a packet, the stored data has to be, too
					Value* data = store->getValueOperand();
					const bool pointerIsPacket = isPacketizedType(store->getPointerOperand()->getType());
					const bool dataIsPacket = isPacketizedType(data->getType());
					if (pointerIsPacket && dataIsPacket) continue;
					if (!pointerIsPacket && !dataIsPacket) continue;
					if (dataIsPacket) {
						store->getParent()->getParent()->print(outs(), new WholeFunctionVectorizationAAW(*analysisResults));
						errs() << "    store attempts to write packet to scalar pointer: " << *I << "\n";
						assert (false && "store must not attempt to write packet to scalar pointer!");
						throw std::logic_error("INTERNAL ERROR: store must not attempt to write packet to scalar pointer!");
					}
					// only pointer is packet -> broadcast data
					DEBUG_PKT( outs() << "    found scalar data in packet store: " << *I << "\n"; );
					Value* newOp = broadcastValue(data, I);
					I->replaceUsesOfWith(data, newOp);

					continue;
				}


				//only handle other instructions that have packetized returntype
				if (!isPacketizedType(I->getType())) continue;

				//loop over operands
				for (Instruction::op_iterator OP=I->op_begin(), OPE=I->op_end(); OP!=OPE; ++OP) {
					Value* opV = cast<Value>(OP);
					if (opV->getType()->isLabelTy()) continue;
					if (isPacketizedType(opV->getType())) continue;
					if (isa<Constant>(opV)) continue; //TODO: handle separately
					if (isa<SelectInst>(I) && (opV == cast<SelectInst>(I)->getCondition())) continue; //scalar condition is allowed
					// indices to GEP are allowed to be scalar
					// -> packet GEP is okay as long as pointer is a packet
					if (isa<GetElementPtrInst>(I) &&
							(isPacketizedType(cast<GetElementPtrInst>(I)->getPointerOperand()->getType()))) continue;
					//operand is not packetized -> broadcast
					DEBUG_PKT( outs() << "    found scalar operand in instruction: " << *I << "\n"; );

					// If this is a phi, we must not insert the new instructions
					// before it, but at the end of the incoming block.
					Instruction* insertBefore = I;
					if (PHINode* phi = dyn_cast<PHINode>(I)) {
						for (unsigned i=0, e=phi->getNumIncomingValues(); i!=e; ++i) {
							Value* incVal = phi->getIncomingValue(i);
							if (opV != incVal) continue;

							BasicBlock* incBB = phi->getIncomingBlock(i);
							insertBefore = incBB->getTerminator();
							break;
						}
						assert (insertBefore != I);
					}

					Value* newOp = broadcastValue(opV, insertBefore);
					I->replaceUsesOfWith(opV, newOp);
				}
			}
		}

		DEBUG_PKT( outs() << "value replication finished.\n\n"; );
	}

	// NOTE: This function creates W insert-element operations instead of an
	//       insert and a shuffle. LLVM is able to optimize both to the same
	//       code (pshufd).
	Value* broadcastValue(Value* oldVal, Instruction* insertBefore) {
		assert (oldVal && insertBefore);
		assert (!isa<BasicBlock>(oldVal) && !isa<Function>(oldVal));

		DEBUG_PKT( outs() << "      replicating value: " << *oldVal << "\n"; );

		Type* oldType = oldVal->getType();
		assert (oldType != Type::getVoidTy(*mInfo->mContext) && "must never call broadcastValue() on values of type void!");
		assert (oldType != Type::getLabelTy(*mInfo->mContext) && "must never call broadcastValue() on values of type label!");
		Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);

		const bool isConsecutive = analysisResults->isConsecutive(oldVal);

#ifdef PACKETIZER_USE_DYNAMIC_CONSECUTIVENESS_CHECKS
		DEBUG_PKT(
			if (analysisResults->isRandom(oldVal)) {
				errs() << "\nWARNING: broadcastValue must never be called on "
						<< "values marked INDEX_RANDOM, except for vectorized "
						<< "load/store operations in dynamic consecutiveness "
						<< "checks or input-independent operations that can "
						<< "be left scalar. Make sure this is such an operation: "
						<< *oldVal << "\n";
			}
		);
#else
		assert ((insertBefore->getParent()->getParent()->getName().equals("mandelbrot_SIMD") ||
				!analysisResults->isRandom(oldVal)) && "must never call broadcastValue() on values marked INDEX_RANDOM!");
#endif

		Type::TypeID oldTypeID = oldType->getTypeID();
		switch (oldTypeID) {

			//case Type::VoidTyID : assert (false && "not allowed"); //not allowed!

			case Type::FloatTyID :
			{
				Value* newVal = UndefValue::get(newType);
//#define PACKETIZER_USE_SHUFFLE_BROADCAST
#ifdef PACKETIZER_USE_SHUFFLE_BROADCAST
				// NOTE:
				newVal = InsertElementInst::Create(newVal, oldVal, mInfo->mConstInt32Zero, "", insertBefore);
				std::vector<Constant*> maskElems;
				for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
					maskElems.push_back(mInfo->mConstInt32Zero);
				}
				Value* mask = ConstantVector::get(ArrayRef<Constant*>(maskElems));
				newVal = new ShuffleVectorInst(newVal, UndefValue::get(newType), mask, "", insertBefore);
#else
				for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
					newVal = InsertElementInst::Create(newVal, oldVal, ConstantInt::get(*mInfo->mContext, APInt(32, i)), "", insertBefore);
					addValueInfo(newVal, false, true, false, false, false, false, false, false); // INDEX_SAME
					if (isConsecutive) {
						oldVal = BinaryOperator::Create(Instruction::FAdd, oldVal, ConstantFP::get(Type::getFloatTy(*mInfo->mContext), 1.0), "consec.inc", insertBefore);
						addValueInfo(oldVal, false, true, false, false, false, false, false, false); // INDEX_SAME
					}
				}
#endif
				DEBUG_PKT( outs() << "      new value: " << *newVal << "\n"; );
				return newVal;
			}

			case Type::IntegerTyID :
			{
				Value* newVal = UndefValue::get(newType);
				// if oldVal is a boolean, we have to generate a vector with elements -1 or 0
				if (oldVal->getType()->isIntegerTy(1)) {
					assert (newType == mInfo->mVectorTyIntSIMD);
					assert (!isConsecutive && "broadcast of consecutive boolean value requested - what should that mean? :P");
					newVal = SelectInst::Create(oldVal, mInfo->mConstVecSIMDInt32MinusOne, Constant::getNullValue(newType), "", insertBefore);
					addValueInfo(newVal, false, true, false, false, false, false, false, false); // INDEX_SAME
				} else {
					for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
						newVal = InsertElementInst::Create(newVal, oldVal, ConstantInt::get(*mInfo->mContext, APInt(32, i)), "", insertBefore);
						addValueInfo(newVal, false, true, false, false, false, false, false, false); // INDEX_SAME
						if (isConsecutive) {
							oldVal = BinaryOperator::Create(Instruction::Add, oldVal, mInfo->mConstInt32One, "consec.inc", insertBefore);
							addValueInfo(oldVal, false, true, false, false, false, false, false, false); // INDEX_SAME
						}
					}
				}
				DEBUG_PKT( outs() << "      new value: " << *newVal << "\n"; );
				return newVal;
			}

			case Type::ArrayTyID:
			{
				// create new array of packet-type 'newType' and same size
				ArrayType* aType = cast<ArrayType>(oldType);

				// broadcast each element of the array
				Value* result = UndefValue::get(newType); // the new 'element'-array
				for (unsigned i=0, ie=aType->getNumElements(); i<ie; ++i) {
					Instruction* scalarElem = ExtractValueInst::Create(oldVal, ArrayRef<unsigned>(i), "", insertBefore);
					addValueInfo(scalarElem, false, true, false, false, false, false, false, false); // INDEX_SAME
					// recurse in order to broadcast sub-element
					Value* pktElem = broadcastValue(scalarElem, insertBefore);
					result = InsertValueInst::Create(result, pktElem, ArrayRef<unsigned>(i), "", insertBefore);
					addFalseValueInfo(result); // TODO: really varying?
				}
				DEBUG_PKT( outs() << "    new array: " << *result << "\n"; );
				assert (result->getType() == newType);
				assert (isa<Instruction>(result));
				return cast<Instruction>(result);
			}

			case Type::StructTyID:
			{
				// create new struct of packet-type 'newType'
				StructType* sType = cast<StructType>(oldType);

				// broadcast each element of the struct
				Value* result = UndefValue::get(newType);
				for (unsigned i=0; i<sType->getNumElements(); ++i) {
					Value* scalarElem = ExtractValueInst::Create(oldVal, ArrayRef<unsigned>(i), "", insertBefore);
					addValueInfo(scalarElem, false, true, false, false, false, false, false, false); // INDEX_SAME
					// recurse in order to broadcast sub-element
					Value* pktElem = broadcastValue(scalarElem, insertBefore);
					result = InsertValueInst::Create(result, pktElem, ArrayRef<unsigned>(i), "", insertBefore);
					addFalseValueInfo(result); // TODO: really varying?
				}
				DEBUG_PKT( outs() << "    new struct: " << *result << "\n"; );
				assert (result->getType() == newType);
				assert (isa<Instruction>(result));
				return cast<Instruction>(result);
			}
			case Type::PointerTyID:
			{
				// create new pointer of packet-type 'newType'
				// This means we have to allocate a new object of type 'newType',
				// load the value from the supplied pointer ('oldVal'),
				// broadcast that value (=recurse),
				// store the broadcasted value into the newly allocated object
				// and finally return the new pointer (the alloca)
				LoadInst* loadedScalarVal = new LoadInst(oldVal, "", false, mInfo->mAlignmentScalar, insertBefore);
                addValueInfo(loadedScalarVal, false, true, false, false, false, false, false, false); // INDEX_SAME
				Value* newVal = broadcastValue(loadedScalarVal, insertBefore);

				AllocaInst* newPtr = new AllocaInst(newType->getContainedType(0), mInfo->mConstInt32One, mInfo->mAlignmentSIMD, "", insertBefore);
				StoreInst* sti = new StoreInst(newVal, newPtr, false, mInfo->mAlignmentSIMD, insertBefore);
				DEBUG_PKT( outs() << "    new pointer: " << *newPtr << "\n"; );
				assert (newPtr->getType() == newType);
				addFalseValueInfo(newPtr); // TODO: really varying?
				addFalseValueInfo(sti); // TODO: really varying?
				return newPtr;
			}

			default :
			{
				errs() << "\nERROR: only values of type float, int, array, struct, and pointer can be broadcasted, not '" << *oldType << "'!\n";
				assert (false && "broadcastValue() can only handle float, int, array, struct, and pointer!");
				throw std::logic_error("INTERNAL ERROR: Can only broadcast values of types float, int, array, struct, and pointer!");
			}
		}
	}


	// NOTE: long time not updated :p
	// TODO: use generateHorizontalExtract()/Merge()
	Function* createSIMDWrapperFunction(Function* f_SIMD, const std::string& name) {
		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "creating wrapper function for info.packetizationSize " << mInfo->mPacketizationSize << "... "; );
		if (mInfo->mPacketizationSize == mInfo->mSimdWidth) {
			DEBUG_PKT( outs() << "skipped (info.packetizationSize == SIMD width).\n"; );
			DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );
			return f_SIMD;
		}
		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		assert (mInfo->mPacketizationSize >= mInfo->mSimdWidth && mInfo->mPacketizationSize % mInfo->mSimdWidth == 0);

		//--------------------------------------------------------------------------
		// create new argument-types
		//--------------------------------------------------------------------------
		std::vector<Type*> argTypes;
		DEBUG_PKT( outs() << "  creating argument types...\n"; );
		unsigned argIndex = 0;
		for (Function::arg_iterator A = f_SIMD->arg_begin(); A != f_SIMD->arg_end(); ++A, ++argIndex) {
			//DEBUG_PKT( outs() << "    type of f_SIMD: "; A->getType()->print(outs()); outs() << "\n"; );
			//TODO: possibly incorporate per-packet-constant arguments here, too
			if (constantArgs[argIndex]) {
				//not packetized before -> store old type
				argTypes.push_back(A->getType());
				DEBUG_PKT( outs() << "    type not packetized, ignored!\n"; );
			} else {
				argTypes.push_back(Packetizer::packetizeSIMDWrapperType(A->getType(), *mInfo));
			}
		}

		//--------------------------------------------------------------------------
		// create new return-type
		//--------------------------------------------------------------------------
		const bool hasReturnValue = f_SIMD->getReturnType() != Type::getVoidTy(*mInfo->mContext);
		const bool hasPacketizedReturnValue = hasReturnValue && isPacketizedType(f_SIMD->getReturnType());
		Type* returnType = hasReturnValue ? Packetizer::packetizeSIMDWrapperType(f_SIMD->getReturnType(), *mInfo) : Type::getVoidTy(*mInfo->mContext);


		//--------------------------------------------------------------------------
		// create new function
		//--------------------------------------------------------------------------
		//create a new function type...
		FunctionType* fTy = FunctionType::get(returnType, argTypes, false); //isVarArg = false

		//create the new function...
		//std::string name = f_SIMD->getNameStr();
		//f_SIMD->setName(name + "_SIMD");
		//Function* f_arr = Function::Create(fTy, f_SIMD->getLinkage(), name, info.module);
		Function* f_arr = Function::Create(fTy, f_SIMD->getLinkage(), name, mInfo->mModule);

		f_arr->setCallingConv(CallingConv::C);

		//set attributes
		f_arr->setAttributes(f_SIMD->getAttributes());

		//set names of arguments
		Function::arg_iterator destI = f_arr->arg_begin();
		for (Function::const_arg_iterator I = f_SIMD->arg_begin(), E = f_SIMD->arg_end(); I != E; ++I) {
			destI->setName(I->getNameStr());
			++destI;
		}


		//--------------------------------------------------------------------------
		// insert content
		//--------------------------------------------------------------------------
		//insert 'info.packetizationSize / info.simdWidth' calls to f_SIMD
		//DEBUG_PKT( outs() << "creating loop with " << loopIterations << " calls to " << f_SIMD->getNameStr() << "...\n"; );

		BasicBlock* entryBB = BasicBlock::Create(*mInfo->mContext, "entry", f_arr, 0);
		BasicBlock* loopBB = BasicBlock::Create(*mInfo->mContext, "loop", f_arr, 0);
		BasicBlock* exitBB = BasicBlock::Create(*mInfo->mContext, "exit", f_arr, 0);


		//fill entry-block

		Constant* iterationNrConst = ConstantInt::get(*mInfo->mContext, APInt(32, mInfo->mTotalSIMDIterations));
		Constant* arraySizeConst = ConstantInt::get(*mInfo->mContext, APInt(32, 16 * mInfo->mTotalSIMDIterations)); //sizeof(__m128) = 16
		//malloc does not work, ignores alignment...
		//MallocInst* result = new MallocInst(info.vectorTy_float4, arraySize, info.alignmentSIMD, "res", entry);

		//Value* resultMemAlignPtr = NULL;
		Instruction* result = NULL;
		if (hasReturnValue && hasPacketizedReturnValue) {
			//create alloc, bitcast, call to posix_memalign and load
			AllocaInst* resultArrayPtr = new AllocaInst(returnType, mInfo->mConstInt32One, mInfo->mAlignmentSIMD, "res.ptr", entryBB);
			resultArrayPtr->setAlignment(mInfo->mAlignmentPtr);
			//CastInst* ra_ptr_cast_i8 = new BitCastInst(resultArrayPtr, PointerType::get(PointerType::get(IntegerType::get(info.context, 8), 0), 0), "", entryBB);
			//createPosixMemalignCall(ra_ptr_cast_i8, arraySizeConst, "", entryBB);
			//resultMemAlignPtr = ra_ptr_cast_i8; //don't free return value!
			result = new LoadInst(resultArrayPtr, "res", false, entryBB);

		} else if (hasReturnValue) {
			//result = new MallocInst(returnType, arraySizeConst, info.alignmentScalar, "res", entryBB);
			result = new AllocaInst(returnType, arraySizeConst, mInfo->mAlignmentScalar, "res", entryBB);
		}


		BranchInst::Create(loopBB, entryBB); //branch to loop

		DEBUG_PKT( outs() << "entry block finished!\n"; );


		//fill bb (loop)

		//create phi-function for loop-variable (use dummy-forward-reference)
		Argument* fwdref = new Argument(IntegerType::get(*mInfo->mContext, 32));
		PHINode* int32_i_0_reg2mem_0 = PHINode::Create(IntegerType::get(*mInfo->mContext, 32), 2U, "i.0.reg2mem.0", loopBB);
		int32_i_0_reg2mem_0->addIncoming(mInfo->mConstInt32Zero, entryBB);
		int32_i_0_reg2mem_0->addIncoming(fwdref, loopBB);

		DEBUG_PKT( outs() << "created phi function for loop-variable.\n"; );

		//load f_SIMD_call-arguments from input values
		std::vector<Value*> packed_params;
		Function::arg_iterator oldA = f_SIMD->arg_begin();
		argIndex = 0;
		for (Function::arg_iterator A = f_arr->arg_begin(); A != f_arr->arg_end(); ++A, ++oldA, ++argIndex) {
			//ignore non-packetized arguments
			if (constantArgs[argIndex]) packed_params.push_back(A);
			else packed_params.push_back(generateExtractForWrapper(A, int32_i_0_reg2mem_0, oldA->getType(), "", entryBB->getTerminator(), loopBB));
		}
		DEBUG_PKT( outs() << "created load-instructions for parameters of call\n"; );


		//if called function has return type void, do not attempt to assign a name
		const std::string nameTmp = f_SIMD->getReturnType()->isVoidTy() ? "" : "f_SIMD_call";
		CallInst* f_SIMD_call = CallInst::Create(f_SIMD, ArrayRef<Value*>(packed_params), nameTmp, loopBB);
		f_SIMD_call->setCallingConv(CallingConv::C);
		f_SIMD_call->setTailCall(true);
		//f_SIMD_call->setAttributes(f_SIMD->getAttributes()); //?

		DEBUG_PKT( outs() << "created call: " << *f_SIMD_call << "\n"; );

		//store result of loop-iteration into result-array
		//TODO: does it have to be an array? -> non-packetized return value? -> not allowed!
		if (hasReturnValue) {
			//std::vector<Value*> indices;
			//indices.push_back(Constant::getNullValue(Type::getInt32Ty(info.context)));
			//indices.push_back(int32_i_0_reg2mem_0);
			GetElementPtrInst* resultArrayIndex = GetElementPtrInst::Create(result, ArrayRef<Value*>(int32_i_0_reg2mem_0), "", loopBB);
			//GetElementPtrInst* resultIndex = GetElementPtrInst::Create(result, ArrayRef<Value*>(indices), "", loopBB);
			//LoadInst* loadI = new LoadInst(resultIndex, "", false, info.alignmentSIMD, loopBB); //volatile=false
			DEBUG_PKT( outs() << "    f_SIMD_call: " << *f_SIMD_call << "\n"; );
			DEBUG_PKT( outs() << "    resultArrayIndex: " << *resultArrayIndex << "\n"; );
			if (hasPacketizedReturnValue) new StoreInst(f_SIMD_call, resultArrayIndex, false, mInfo->mAlignmentSIMD, loopBB);
			else new StoreInst(f_SIMD_call, resultArrayIndex, false, mInfo->mAlignmentScalar, loopBB);

			DEBUG_PKT( outs() << "created store-instruction for return value of call\n"; );
		}

		//store back any values of pointers
		generateStoreBackForWrapper(f_SIMD_call, int32_i_0_reg2mem_0, loopBB);

		//increment loop-variable
		BinaryOperator* int32_indvar_next = BinaryOperator::Create(Instruction::Add, int32_i_0_reg2mem_0, mInfo->mConstInt32One, "indvar.next", loopBB);
		//test exit condition
		CmpInst* int1_exitcond = CmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_EQ, int32_indvar_next, iterationNrConst, "exitcond", loopBB);
		//conditionally branch out of loop or to begin of loop
		BranchInst::Create(exitBB, loopBB, int1_exitcond, loopBB);

		DEBUG_PKT( outs() << "loop block finished!\n"; );

		if (hasReturnValue) ReturnInst::Create(*mInfo->mContext, result, exitBB);
		else ReturnInst::Create(*mInfo->mContext, exitBB);

		// Resolve Forward Reference of loop-block
		fwdref->replaceAllUsesWith(int32_indvar_next); delete fwdref;

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "finished creation of wrapper function '" << name << "'!\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );
		DEBUG_PKT( f_arr->print(outs()); );

		//TODO: inline call to f_SIMD
		//llvm does this automatically when optimizing (at least up to a threshold)

		return f_arr;
	}

	// TODO: use uniform/varying info!?
	Instruction* generateExtractForWrapper(Value* V, Value* index, Type* targetType, std::string name, Instruction* allocPos, BasicBlock* insertAtEnd) {
		Instruction* insertBefore = createDummy(Type::getInt32Ty(*mInfo->mContext), V, insertAtEnd);
		Instruction* extractInst = generateExtractForWrapper(V, index, targetType, name, allocPos, insertBefore);
		removeFromInstructionInfo(insertBefore);
		insertBefore->eraseFromParent();
		return extractInst;
	}
	Instruction* generateExtractForWrapper(Value* V, Value* index, Type* targetType, std::string name, Instruction* allocPos, Instruction* insertBefore) {
		DEBUG_PKT( outs() << "  extracting argument of wrapper-call at index " << *index << " of value: " << *V << "\n"; );
		DEBUG_PKT( outs() << "    target type: " << *targetType << "\n"; );
		Type* valType = V->getType();
		const unsigned valTypeID = valType->getTypeID();

		assert (valType == Packetizer::packetizeSIMDWrapperType(targetType, *mInfo) && "bad target type found for extraction of value (for wrapper-call to SIMD function)");

		switch (valTypeID) {
			case Type::VectorTyID :
			{
				//if creating the wrapper, this case should never be executed (should be handled by typecheckers before packetization)!
				assert (false && "PACKET TYPE NOT ALLOWED FOR EXTRACTION IN WRAPPER!");
				throw std::logic_error("INTERNAL ERROR: Packet type not allowed for extraction in wrapper!");
			}
			case Type::StructTyID :
			{
				errs() << "struct type: " << *valType << "\n";
				assert (false && "CALL-BY-VALUE NOT IMPLEMENTED FOR STRUCTS!");
				throw std::logic_error("INTERNAL ERROR: Support for call-by-value not implemented for structs!");
			}
			case Type::ArrayTyID :
			{
				errs() << "array type: " << *valType << "\n";
				assert (false && "CALL-BY-VALUE NOT IMPLEMENTED FOR ARRAYS!");
				throw std::logic_error("INTERNAL ERROR: Support for call-by-value not implemented for arrays!");
			}
			case Type::PointerTyID :
			{
				Type* containedType = valType->getContainedType(0);

				//pointer to struct of arrays of 'primitive' types
				//e.g. { <4 x i32> *, <4 x float> *, <4 x float> * } *
				if (containedType->isStructTy()) {
					DEBUG_PKT( outs() << "    generating code for indexing argument of pointer to struct type...\n"; );
					assert (targetType->isPointerTy() && "if value type is a pointer, target type has to be a pointer as well!");
					assert (targetType->getContainedType(0)->isStructTy() && "contained value type and contained target type have to be of the same type class!");
					//if we have a pointer to a struct, step through the pointer,
					//extract the ith element of each contained value
					//and create a pointer to a struct of the 'lower' struct-type with them
					StructType* sType = cast<StructType>(valType->getContainedType(0));
					//allocate struct of 'lowered' type (= type of argument of called function)
					//AllocaInst* structPtr = new AllocaInst(targetType, info.const_int32_1, info.alignmentSIMD, "", allocPos->getParent()->getParent()->getEntryBlock().getTerminator());//allocPos); //allocate struct for call
					Instruction* structVal = generateAlignedAlloc(targetType, allocPos);

					//for each element of the struct, get the ith subelement
					//TODO: in order to allow structs of structs of ... we would have to
					//index with info.packetizationSize instead of num elements and recurse
					//through all subtypes until a primitive type is found
					for (unsigned i=0; i<sType->getNumElements(); ++i) {
						//first load ith struct-element through pointer
						std::vector<Value*> indices;
						indices.push_back(mInfo->mConstInt32Zero); //step through pointer //TODO: can pull that out of the loop
						indices.push_back(ConstantInt::get(*mInfo->mContext, APInt(32, i))); //ith struct-element
						if (mInfo->mTotalSIMDIterations > 1) indices.push_back(index); //loop-index for array-access
						GetElementPtrInst* gepI = GetElementPtrInst::Create(V, ArrayRef<Value*>(indices), "", insertBefore);
						LoadInst* loadI = new LoadInst(gepI, "", false, mInfo->mAlignmentSIMD, insertBefore); //volatile=false
						//DEBUG_NPKT( outs() << "    GEP: " << *gepI << "\n"; );
						DEBUG_PKT( outs() << "    load: " << *loadI << "\n"; );

						addFalseValueInfo(gepI); // TODO: really varying?
						addFalseValueInfo(loadI); // TODO: really varying?

						//finally store loaded value in newly allocated struct (call-argument)
						indices.clear();
						indices.push_back(mInfo->mConstInt32Zero); //step through pointer
						indices.push_back(ConstantInt::get(*mInfo->mContext, APInt(32, i))); //index of struct-element
						gepI = GetElementPtrInst::Create(structVal, ArrayRef<Value*>(indices), "", insertBefore);
						StoreInst* sti = new StoreInst(loadI, gepI, false, mInfo->mAlignmentSIMD, insertBefore);
						//DEBUG_NPKT( outs() << "    GEP: " << *gepI << "\n"; );
						DEBUG_PKT( outs() << "    store: " << *sti << "\n"; );

						addFalseValueInfo(gepI); // TODO: really varying?
						addFalseValueInfo(sti); // TODO: really varying?
					}
					DEBUG_PKT( outs() << "    extract instruction: " << *structVal << "\n"; );
					return structVal;
				} // Pointer to Struct

				if (containedType->isVectorTy()) {
					if (!targetType->isPointerTy()) {
						//pointer to packet type (= treated as array of packets)
						//e.g. <4 x float> *, scalar type = float
						DEBUG_PKT( outs() << "    generating code for indexing argument of array of packet type...\n"; );
						//if we have a standard array of values, simply take the indexed array-element
						GetElementPtrInst* gepI = GetElementPtrInst::Create(V, ArrayRef<Value*>(index), "", insertBefore);
						LoadInst* loadI = new LoadInst(gepI, name, false, mInfo->mAlignmentSIMD, insertBefore); //volatile=false
						DEBUG_PKT( outs() << "    extract instruction: " << *loadI << "\n"; );
						addFalseValueInfo(gepI); // TODO: really varying?
						addFalseValueInfo(loadI); // TODO: really varying?
						return loadI;
					} else {
						//pointer to packet type (= treated as pointer to array of packets)
						//e.g. <4 x float> *, scalar type = float *
						DEBUG_PKT( outs() << "    generating code for indexing argument of (pointer to first element of) array of packet type...\n"; );
						std::vector<Value*> indices;
						indices.push_back(mInfo->mConstInt32Zero); //step through pointer
						indices.push_back(index); //loop-index for array-access
						GetElementPtrInst* gepI = GetElementPtrInst::Create(V, ArrayRef<Value*>(indices), "", insertBefore);
						DEBUG_PKT( outs() << "    extract instruction: " << *gepI << "\n"; );
						addFalseValueInfo(gepI); // TODO: really varying?
						return gepI;
					}
				} // Pointer to Packet

				if (containedType->isArrayTy()) {
					//TODO: arrays could also be nested!
					if (containedType->getContainedType(0)->isVectorTy()) {
						if (!targetType->isPointerTy()) {
							//pointer to array of packet type
							//e.g. [ info.totalSIMDIterations x <4 x i32> ] *, 'smaller' type = <4 x float>
							DEBUG_PKT( outs() << "    generating code for indexing argument of pointer to array of primitive type...\n"; );
							//if we have a standard array of values, simply take the indexed array-element
							GetElementPtrInst* gepI = GetElementPtrInst::Create(V, ArrayRef<Value*>(index), "", insertBefore);
							LoadInst* loadI = new LoadInst(gepI, name, false, mInfo->mAlignmentSIMD, insertBefore); //volatile=false
							DEBUG_PKT( outs() << "    extract instruction: " << *loadI << "\n"; );
							addFalseValueInfo(gepI); // TODO: really varying?
							addFalseValueInfo(loadI); // TODO: really varying?
							return loadI;
						} else {
							//pointer to array of packet type
							//e.g. [ info.totalSIMDIterations x <4 x i32> ] *, 'smaller' type = <4 x float> *
							DEBUG_PKT( outs() << "    generating code for indexing argument of (pointer to first element of) array of packet type...\n"; );
							std::vector<Value*> indices;
							indices.push_back(mInfo->mConstInt32Zero); //step through pointer
							indices.push_back(index); //loop-index for array-access
							GetElementPtrInst* gepI = GetElementPtrInst::Create(V, ArrayRef<Value*>(indices), "", insertBefore);
							//LoadInst* loadI = new LoadInst(gepI, name, false, info.alignmentSIMD, insertBefore); //volatile=false
							//DEBUG_PKT( outs() << "    extract instruction: " << *loadI << "\n"; );
							//return loadI;
							DEBUG_PKT( outs() << "    extract instruction: " << *gepI << "\n"; );
							addFalseValueInfo(gepI); // TODO: really varying?
							return gepI;
						}
					}

				} // Pointer to Array

				//all other cases: fall through to default

			} // Pointer

			default :
			{
				errs() << "ERROR: unsupported argument type: " << *valType << "\n";
				assert (false && "ARGUMENT TYPE NOT SUPPORTED!");
				throw std::logic_error("INTERNAL ERROR: Unsupported argument type found!");
			}
		}
	}


	// Returns the element type for a splitting operation.
	// The input type is required to be packetized, meaning that e.g. on
	// leaf level of a struct, the underlying type is a packet (vector)
	// and never a scalar.
	// NOTE: This basically applies the packetization rules in reverse.
	// EXAMPLE: { <4 x float>, [ <4 x float>, <4 x float> ], <4 x int> }
	//          -> { float, [ float, float ], int }
	// TODO: move to tools or something...
	Type* getScalarFromPacketizedType(Type* type) {
		assert (isPacketizedType(type) && "can not extract element type from non-packetized type!");

		switch(type->getTypeID()) {
			case Type::VectorTyID:
			{
				VectorType* vType = cast<VectorType>(type);
				assert (type == Packetizer::packetizeSIMDType(vType->getScalarType(), *mInfo));
				return vType->getScalarType();
			}
			case Type::ArrayTyID:
			{
				ArrayType* aType = cast<ArrayType>(type);
				// create array-type with newElementType
				Type* newElementType = getScalarFromPacketizedType(aType->getElementType());
				assert (type == Packetizer::packetizeSIMDType(ArrayType::get(newElementType, aType->getNumElements()), *mInfo));
				return ArrayType::get(newElementType, aType->getNumElements());
			}
			case Type::StructTyID:
			{
				StructType* sType = cast<StructType>(type);
				// create struct-type with extracted elements
				std::vector<Type*> params;
				for (StructType::subtype_iterator ST=sType->subtype_begin(), STE=sType->subtype_end(); ST!=STE; ++ST) {
					Type* subType = getScalarFromPacketizedType(*ST);
					assert (subType && "can not extract element type: struct field is not packetized!");
					params.push_back(subType);
				}
				assert (type == Packetizer::packetizeSIMDType(StructType::get(*mInfo->mContext, params, sType->isPacked()), *mInfo));
				return StructType::get(*mInfo->mContext, params, sType->isPacked());
			}
			case Type::PointerTyID:
			{
				PointerType* pType = cast<PointerType>(type);
				Type* subType = getScalarFromPacketizedType(pType->getElementType());
				assert (subType && "can not extract element type: struct field is not packetized!");
				assert (type == Packetizer::packetizeSIMDType(PointerType::get(subType, pType->getAddressSpace()), *mInfo));
				return PointerType::get(subType, pType->getAddressSpace());
			}
			default:
			{
				DEBUG_PKT( errs() << "ERROR: bad type for element extraction found: " << *type << "\n"; );
				assert (false && "bad type for element extraction found!");
				throw std::logic_error("INTERNAL ERROR: bad type for element extraction found!");
			}
		}
	}

	// Generates code that extracts the ith sub-element from a packetized value.
	// This means that the generated value holds only the values of the
	// ith scalar instance of that type.
	// Thus, the code does not extract e.g. the ith value of a struct
	// but generates a new struct where each element is the ith scalar
	// element of the original packet value.
	// EXAMPLE: todo ;)
	inline Instruction* generateHorizontalExtract(Value* V, Value* indexVal, const std::string& name, Instruction* allocPos, BasicBlock* insertAtEnd) {
		Instruction* insertBefore = createDummy(Type::getInt32Ty(*mInfo->mContext), V, insertAtEnd);
		Instruction* extractInst = generateHorizontalExtract(V, indexVal, name, allocPos, insertBefore);
		removeFromInstructionInfo(insertBefore);
		insertBefore->eraseFromParent();
		return extractInst;
	}
	Instruction* generateHorizontalExtract(Value* V, Value* indexVal, const std::string& name, Instruction* allocPos, Instruction* insertBefore) {
		DEBUG_PKT( outs() << "  extracting horizontal elements at index " << *indexVal << " of value: " << *V << "\n"; );
		Type* sourceType = V->getType();
		Type* elementType = getScalarFromPacketizedType(sourceType);
		assert (sourceType && elementType);
		DEBUG_PKT( outs() << "    source type: " << *sourceType << "\n"; );
		DEBUG_PKT( outs() << "    element type: " << *elementType << "\n"; );

		switch (sourceType->getTypeID()) {
			case Type::VectorTyID:
			{
				// simply extract the element at index 'index'
				ExtractElementInst* eeInst = ExtractElementInst::Create(V, indexVal, name, insertBefore);
				DEBUG_PKT( outs() << "    extracted element (new vector): " << *eeInst << "\n"; );
				assert (!isPacketizedType(eeInst->getType()));
				assert (eeInst->getType() == elementType);
				addFalseValueInfo(eeInst); // TODO: really varying?
				return eeInst;
			}
			case Type::ArrayTyID:
			{
				// create new array of type 'elementType' and same size
				ArrayType* aType = cast<ArrayType>(sourceType);

				//for each element of the array, get the ith subelement
				Value* result = UndefValue::get(elementType); // the new 'element'-array
				for (unsigned j=0, je=aType->getNumElements(); j<je; ++j) {
					Instruction* pktElem = ExtractValueInst::Create(V, ArrayRef<unsigned>(j), name, insertBefore);
					// recurse in order to extract correct sub-element
					Value* scalarElem = generateHorizontalExtract(pktElem, indexVal, name, allocPos, insertBefore);
					result = InsertValueInst::Create(result, scalarElem, ArrayRef<unsigned>(j), name, insertBefore);
					addFalseValueInfo(pktElem); // TODO: really varying?
					addFalseValueInfo(result); // TODO: really varying?
				}
				DEBUG_PKT( outs() << "    extracted element (new array): " << *result << "\n"; );
				assert (result->getType() == elementType);
				assert (isa<Instruction>(result));
				return cast<Instruction>(result);
			}
			case Type::StructTyID:
			{
				// create new struct of "sub"-type 'elementType'
				StructType* sType = cast<StructType>(sourceType);

				//for each element of the struct, get the ith subelement
				Value* result = UndefValue::get(elementType);
				for (unsigned j=0; j<sType->getNumElements(); ++j) {
					Value* pktElem = ExtractValueInst::Create(V, ArrayRef<unsigned>(j), name, insertBefore);
					// recurse in order to extract correct sub-element
					Value* scalarElem = generateHorizontalExtract(pktElem, indexVal, name, allocPos, insertBefore);
					result = InsertValueInst::Create(result, scalarElem, ArrayRef<unsigned>(j), name, insertBefore);
					addFalseValueInfo(pktElem); // TODO: really varying?
					addFalseValueInfo(result); // TODO: really varying?
				}
				DEBUG_PKT( outs() << "    extracted element (new struct): " << *result << "\n"; );
				assert (result->getType() == elementType);
				assert (isa<Instruction>(result));
				return cast<Instruction>(result);
			}
			case Type::PointerTyID:
			{
				// create new pointer of "sub"-type 'elementType'
				// This means we have to allocate a new object of type 'elementType',
				// load the value from the supplied pointer ('V'),
				// extract from each element of that value the ith sub-element (=recurse),
				// store the extracted value into the newly allocated object
				// and finally return the new pointer (the alloca)
				PointerType* pType = cast<PointerType>(pType);
				LoadInst* loadedVal = new LoadInst(V, name, false, mInfo->mAlignmentSIMD, insertBefore);
				Value* newVal = generateHorizontalExtract(loadedVal, indexVal, name, allocPos, insertBefore);

				AllocaInst* newPtr = new AllocaInst(elementType->getContainedType(0), mInfo->mConstInt32One, mInfo->mAlignmentScalar, name, allocPos);
				StoreInst* sti = new StoreInst(newVal, newPtr, false, mInfo->mAlignmentScalar, insertBefore);
				DEBUG_PKT( outs() << "    extracted element (new pointer): " << *newPtr << "\n"; );
				assert (newPtr->getType() == elementType);
				addFalseValueInfo(loadedVal); // TODO: really varying?
				addFalseValueInfo(newPtr); // TODO: really varying?
				addFalseValueInfo(sti); // TODO: really varying?
				return newPtr;
			}
			default:
			{
				DEBUG_PKT( errs() << "ERROR: bad type for element extraction found: " << *sourceType << "\n"; );
				assert (false && "bad type for element extraction found!");
				throw std::logic_error("INTERNAL ERROR: bad type for element extraction found!");
			}
		}
	}

	// Recursively merges 'info.simdWidth' values together to form a
	// value of type 'targetType'.
	// This function merges horizontally, meaning it has to reorder
	// values of structs and arrays because each scalarVal represents
	// the composite value of one instance (instead of all values of one
	// element of the structure).
	// NOTE: targetType is allowed to be scalar. In this case, it is
	//       assumed that splitVals only holds one element which is uniform.
	Instruction* generateHorizontalMerge(Value** splitVals, Type* targetType, const std::string& name, Instruction* insertBefore) {
		assert (targetType);
		DEBUG_PKT( outs() << "  merging values...\n"; );
		assert (!targetType->isPrimitiveType() && !targetType->isIntegerTy() && "merging into scalar value does not make sense!");
		//assert (targetType->getNumContainedTypes() >= index && "type of target value does not have "+index+" elements!");
		//Type* targetType = targetType->getContainedType(index);
		Type* elementType = splitVals[0]->getType();
		DEBUG_PKT( outs() << "    target type: " << *targetType << "\n"; );
		DEBUG_PKT( outs() << "    element type: " << *elementType << "\n"; );
		assert ((targetType == elementType || targetType == Packetizer::packetizeSIMDType(elementType, *mInfo)) && "element target type has to be packetized element type or uniform!");

		DEBUG_PKT( outs() << "    values:\n"; );
		for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
			assert (splitVals[i]);
			DEBUG_PKT( outs() << "     *" << *splitVals[i] << "\n"; );
			assert (splitVals[i]->getType() == elementType);
			if (elementType == targetType) break; // uniform value
		}

		Value* result = UndefValue::get(targetType);

		switch (targetType->getTypeID()) {
			case Type::FloatTyID:
			{
				// uniform float, no merging necessary
				result = splitVals[0];
				break;
			}
			case Type::IntegerTyID:
			{
				// uniform int, no merging necessary
				result = splitVals[0];
				break;
			}
			case Type::VectorTyID:
			{
				// simply merge info.simdWidth scalars into a vector
				assert (elementType->isFloatTy() || elementType->isIntegerTy(32) || elementType->isIntegerTy(1));
				for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
					result = InsertElementInst::Create(result, splitVals[i], ConstantInt::get(*mInfo->mContext, APInt(32, i)), "", insertBefore);
					addFalseValueInfo(result); // TODO: really varying?
				}
				break;
			}
			case Type::ArrayTyID:
			{
				// loop over elements of array and call merge on
				// 'info.simdWidth' values of split input arrays
				ArrayType* aType = cast<ArrayType>(targetType);
				assert (elementType->isArrayTy() && "elements have to be arrays!");
				//assert (elemType->isArrayTy()); //TODO: activate

				for (unsigned j=0, je=aType->getNumElements(); j<je; ++j) {
					Value** values = new Value*[mInfo->mSimdWidth]();
					// extract jth value from each of the input arrays
					for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
						values[i] = ExtractValueInst::Create(splitVals[i], ArrayRef<unsigned>(j), "", insertBefore);
						addFalseValueInfo(values[i]); // TODO: really varying?
					}
					// merge the extracted values
					Instruction* mergedArrayElem = generateHorizontalMerge(values, aType->getElementType(), "", insertBefore);
					// insert the merged element into the new array
					result = InsertValueInst::Create(result, mergedArrayElem, ArrayRef<unsigned>(j), "", insertBefore);
					addFalseValueInfo(result); // TODO: really varying?
					delete [] values;
				}
				break;
			}
			case Type::StructTyID:
			{
				// loop over elements of struct and call merge on
				// 'info.simdWidth' values of split input structs
				StructType* sType = cast<StructType>(targetType);
				assert (elementType->isStructTy() && "elements have to be structs!");
				//assert (elemType->isStructTy()); //TODO: activate

				for (unsigned j=0, je=sType->getNumElements(); j<je; ++j) {
					Value** values = new Value*[mInfo->mSimdWidth]();
					// extract jth value from each of the input structs
					for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
						values[i] = ExtractValueInst::Create(splitVals[i], ArrayRef<unsigned>(j), "", insertBefore);
						addFalseValueInfo(values[i]); // TODO: really varying?
					}
					// merge the extracted values
					Instruction* mergedStructElem = generateHorizontalMerge(values, sType->getElementType(j), "", insertBefore);
					// insert the merged element into the new struct
					result = InsertValueInst::Create(result, mergedStructElem, ArrayRef<unsigned>(j), "", insertBefore);
					addFalseValueInfo(result); // TODO: really varying?
					delete [] values;
				}
				break;
			}
			case Type::PointerTyID:
			{

				PointerType* pType = cast<PointerType>(targetType);
				assert (elementType->isPointerTy() && "elements have to be pointers!");
				//assert (elemType->isPointerTy()); //TODO: activate

				Value** values = new Value*[mInfo->mSimdWidth]();
				// load from each of the input pointers
				for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
					values[i] = new LoadInst(splitVals[i], "", false, mInfo->mAlignmentSIMD, insertBefore);
					addFalseValueInfo(values[i]); // TODO: really varying?
				}
				// merge the loaded values
				Instruction* mergedStructElem = generateHorizontalMerge(values, pType->getElementType(), "", insertBefore);
				// allocate space for the new type
				result = new AllocaInst(pType->getElementType(), mInfo->mConstInt32Zero, mInfo->mAlignmentSIMD, "", insertBefore);
				// store the merged element into the newly allocated space
				StoreInst* storeInst = new StoreInst(mergedStructElem, result, false, mInfo->mAlignmentSIMD, insertBefore);
				addFalseValueInfo(result); // TODO: really varying?
				addFalseValueInfo(storeInst); // TODO: really varying?

				delete [] values;
				break;
			}
			default:
			{
				DEBUG_PKT( errs() << "ERROR: cannot merge elements into value of type: " << *targetType << "\n"; );
				assert (false && "bad type for element merging found!");
				throw std::logic_error("INTERNAL ERROR: bad type for element merging found!");
			}
		}

		DEBUG_PKT( outs() << "    merged value: " << *result << "\n"; );
		assert (result->getType() == targetType);

		result->setName(name);
		assert (isa<Instruction>(result));
		return cast<Instruction>(result);
	}

	// Recursively merges N values into one value of type 'targetType'.
	// This function merges vertically, meaning each scalarVal represents
	// all instances of one element of the target value.
	// Thus, one merge operation is required for each "low-level"-element
	// of the structure, plus extract/insert operations for nested arrays/structs.
	// The algorithm then recursively reconstructs the structure of the
	// data type.
	// NOTE: We assume that all values are supplied in "breadth-first"
	//       layout, meaning that e.g. all low-level elements of a compound
	//       value's first member are layed out sequentially before those
	//       of the second member and so on.
	Instruction* generateMerge(Value** splitVals, Type* targetType, const std::string& name, Instruction* insertBefore) {
		assert (false && "not implemented!");
		throw std::logic_error("INTERNAL ERROR: NOT IMPLEMENTED!");
	}

	void generateStoreBackForSplitting(CallInst* call, Value* index, std::vector<Value*>& oldCallArgs, BasicBlock* insertAtEnd) {
		Instruction* insertBefore = createDummy(Type::getInt32Ty(*mInfo->mContext), call, insertAtEnd);
		generateStoreBackForSplitting(call, index, oldCallArgs, insertBefore);
		removeFromInstructionInfo(insertBefore);
		insertBefore->eraseFromParent();
	}
	void generateStoreBackForSplitting(CallInst* call, Value* index, std::vector<Value*>& oldCallArgs, Instruction* insertBefore) {
		//store back any values of pointers
		CallInst::op_iterator simdOp = call->op_begin(); //value supplied to current argument-index of call (= pointer to value to store back in case of a pointer-arg)

		for (std::vector<Value*>::iterator A=oldCallArgs.begin(); A!=oldCallArgs.end(); ++A, ++simdOp) {
			Value* argVal = *A; //the value where we have to store back
			Type* argType = argVal->getType();
			if (!argType->isPointerTy()) continue;
			if (isa<Constant>(argVal)) continue;
			DEBUG_PKT( outs() << "  argument has to be stored back: " << *argVal << "\n"; );

			Value* simdOpVal = cast<Value>(simdOp);

			assert (simdOpVal->getType()->isPointerTy() && "supplied value to f_SIMD has to be a pointer!");
			Type* contType = argType->getContainedType(0);
			if (!isPacketizedType(contType)) continue; //do not need to store back non-packetized arguments!

			DEBUG_PKT( outs() << "    corresponding value: " << *simdOpVal << "\n"; );
			DEBUG_PKT( outs() << "    corresponding value where to store: " << *argVal << "\n"; );

			//contType has to be packet, array or struct
			switch (contType->getTypeID()) {
				case Type::VectorTyID :
				{
					//get index where to store
					std::vector<Value*> indices;
					indices.push_back(mInfo->mConstInt32Zero); //step through pointer
					indices.push_back(index); //index of simd iteration
					GetElementPtrInst* argArrayIndex = GetElementPtrInst::Create(argVal, ArrayRef<Value*>(indices), "", insertBefore);
					//get value what to store out of pointer which was supplied to f_SIMD
					LoadInst* packetVal = new LoadInst(simdOpVal, "", false, mInfo->mAlignmentSIMD, insertBefore);
					addFalseValueInfo(argArrayIndex); // TODO: really varying?
					addFalseValueInfo(packetVal); // TODO: really varying?

					//store
					StoreInst* storeInst = new StoreInst(packetVal, argArrayIndex, false, mInfo->mAlignmentSIMD, insertBefore);
					DEBUG_PKT( outs() << "    argArrayIndex: " << *argArrayIndex << "\n"; );
					DEBUG_PKT( outs() << "    new store: " << *storeInst << "\n"; );
					addFalseValueInfo(storeInst); // TODO: really varying?
					break;
				}
				case Type::ArrayTyID :
				{
					assert (false && "generateStoreBackForSplitting for type 'array' NOT IMPLEMENTED!");
					throw std::logic_error("INTERNAL ERROR: NOT IMPLEMENTED!");
				}
				case Type::StructTyID :
				{
					StructType* sType = cast<StructType>(contType);
					//store back all values of the struct
					for (unsigned i=0; i<sType->getNumElements(); ++i) {
						//get index where to store
						std::vector<Value*> indices;
						indices.push_back(mInfo->mConstInt32Zero); //step through pointer
						indices.push_back(ConstantInt::get(*mInfo->mContext, APInt(32, i))); //index of struct element
						indices.push_back(index); //index of simd iteration
						GetElementPtrInst* argArrayIndex = GetElementPtrInst::Create(argVal, ArrayRef<Value*>(indices), "", insertBefore);
						DEBUG_PKT( outs() << "    argArrayIndex: " << *argArrayIndex << "\n"; );
						addFalseValueInfo(argArrayIndex); // TODO: really varying?
						//get struct which was supplied to f_SIMD out of pointer
						//LoadInst* structVal = new LoadInst(simdOpVal, "", false, info.alignmentSIMD, loopBB); //is a struct
						//DEBUG_PKT( outs() << "    new load: " << *structVal << "\n"; );
						//get index to correct value out of the supplied struct
						indices.clear();
						indices.push_back(mInfo->mConstInt32Zero); //step through pointer
						indices.push_back(ConstantInt::get(*mInfo->mContext, APInt(32, i))); //index of simd iteration
						GetElementPtrInst* storeStructIndex = GetElementPtrInst::Create(simdOpVal, ArrayRef<Value*>(indices), "", insertBefore);
						DEBUG_PKT( outs() << "    storeStructIndex: " << *storeStructIndex << "\n"; );
						addFalseValueInfo(storeStructIndex); // TODO: really varying?
						//load value of this index (= value to store back to wrapper-arg)
						LoadInst* structElemVal = new LoadInst(storeStructIndex, "", false, mInfo->mAlignmentSIMD, insertBefore); //is correct struct-element
						DEBUG_PKT( outs() << "    structElemVal: " << *structElemVal << "\n"; );
						addFalseValueInfo(structElemVal); // TODO: really varying?
						//store
						StoreInst* storeInst = new StoreInst(structElemVal, argArrayIndex, false, mInfo->mAlignmentSIMD, insertBefore);
						DEBUG_PKT( outs() << "    new store: " << *storeInst << "\n"; );
						addFalseValueInfo(storeInst); // TODO: really varying?
					}
					break;
				}
				default :
				{
					assert (false && "UNSUPPORTED TYPE FOUND!");
					throw std::logic_error("INTERNAL ERROR: Unsupported type found!");
				}
			} //switch

			DEBUG_PKT( outs() << "created store-instruction for pointer argument of call\n"; );
		}
	}

	void generateStoreBackForWrapper(CallInst* call, Value* index, BasicBlock* insertAtEnd) {
		CallInst::op_iterator simdOp = call->op_begin(); //value supplied to current argument-index of call (= pointer to value to store back in case of a pointer-arg)
		Function* f_SIMD = call->getCalledFunction(); //called function
		Function* f_arr = insertAtEnd->getParent(); //calling function (is guaranteed to be the wrapper)
		Function::arg_iterator wrapperArg = f_arr->arg_begin(); //corresponding argument of wrapper
		for (Function::arg_iterator A=f_SIMD->arg_begin(); A!=f_SIMD->arg_end(); ++A, ++wrapperArg, ++simdOp) {
			Type* argType = A->getType();
			if (!argType->isPointerTy()) continue;
			DEBUG_PKT( outs() << "  argument has to be stored back: " << *A << "\n"; );
			Value* simdOpVal = cast<Value>(simdOp);

			assert (simdOpVal->getType()->isPointerTy() && "supplied value to f_SIMD has to be a pointer!");
			Type* contType = argType->getContainedType(0);
			if (!isPacketizedType(contType)) continue; //do not need to store back non-packetized arguments!

			DEBUG_PKT( outs() << "    corresponding value: " << *simdOpVal << "\n"; );
			DEBUG_PKT( outs() << "    corresponding wrapperArg: " << *wrapperArg << "\n"; );

			//contType has to be packet, array or struct
			switch (contType->getTypeID()) {
				case Type::VectorTyID :
				{
					//get index where to store
					std::vector<Value*> indices;
					indices.push_back(mInfo->mConstInt32Zero); //step through pointer
					indices.push_back(index); //index of simd iteration
					GetElementPtrInst* argArrayIndex = GetElementPtrInst::Create(wrapperArg, ArrayRef<Value*>(indices), "", insertAtEnd);
					//get value what to store out of pointer which was supplied to f_SIMD
					LoadInst* packetVal = new LoadInst(simdOpVal, "", false, mInfo->mAlignmentSIMD, insertAtEnd);
					addFalseValueInfo(argArrayIndex); // TODO: really varying?
					addFalseValueInfo(packetVal); // TODO: really varying?
					//store
					StoreInst* storeInst = new StoreInst(packetVal, argArrayIndex, false, mInfo->mAlignmentSIMD, insertAtEnd);
					DEBUG_PKT( outs() << "    argArrayIndex: " << *argArrayIndex << "\n"; );
					DEBUG_PKT( outs() << "    new store: " << *storeInst << "\n"; );
					addFalseValueInfo(storeInst); // TODO: really varying?
					break;
					}
				case Type::ArrayTyID :
				{
					assert (false && "generateStoreBackForWrapper for type 'array' NOT IMPLEMENTED!");
					throw std::logic_error("INTERNAL ERROR: NOT IMPLEMENTED!");
				}
				case Type::StructTyID :
				{
					StructType* sType = cast<StructType>(contType);
					//store back all values of the struct
					for (unsigned i=0; i<sType->getNumElements(); ++i) {
						//get index where to store
						std::vector<Value*> indices;
						indices.push_back(mInfo->mConstInt32Zero); //step through pointer
						indices.push_back(ConstantInt::get(*mInfo->mContext, APInt(32, i))); //index of struct element
						indices.push_back(index); //index of simd iteration
						GetElementPtrInst* argArrayIndex = GetElementPtrInst::Create(wrapperArg, ArrayRef<Value*>(indices), "", insertAtEnd);
						DEBUG_PKT( outs() << "    argArrayIndex: " << *argArrayIndex << "\n"; );
						addFalseValueInfo(argArrayIndex); // TODO: really varying?
						//get struct which was supplied to f_SIMD out of pointer
						//LoadInst* structVal = new LoadInst(simdOpVal, "", false, info.alignmentSIMD, loopBB); //is a struct
						//DEBUG_PKT( outs() << "    new load: " << *structVal << "\n"; );
						//get index to correct value out of the supplied struct
						indices.clear();
						indices.push_back(mInfo->mConstInt32Zero); //step through pointer
						indices.push_back(ConstantInt::get(*mInfo->mContext, APInt(32, i))); //index of simd iteration
						GetElementPtrInst* storeStructIndex = GetElementPtrInst::Create(simdOpVal, ArrayRef<Value*>(indices), "", insertAtEnd);
						DEBUG_PKT( outs() << "    storeStructIndex: " << *storeStructIndex << "\n"; );
						addFalseValueInfo(storeStructIndex); // TODO: really varying?
						//load value of this index (= value to store back to wrapper-arg)
						LoadInst* structElemVal = new LoadInst(storeStructIndex, "", false, mInfo->mAlignmentSIMD, insertAtEnd); //is correct struct-element
						DEBUG_PKT( outs() << "    structElemVal: " << *structElemVal << "\n"; );
						addFalseValueInfo(structElemVal); // TODO: really varying?
						//store
						StoreInst* storeInst = new StoreInst(structElemVal, argArrayIndex, false, mInfo->mAlignmentSIMD, insertAtEnd);
						DEBUG_PKT( outs() << "    new store: " << *storeInst << "\n"; );
						addFalseValueInfo(storeInst); // TODO: really varying?
					}
					break;
				}
				default : assert (false && "UNSUPPORTED TYPE FOUND!");
				throw std::logic_error("INTERNAL ERROR: Unsupported type found!");
			}

			DEBUG_PKT( outs() << "created store-instruction for pointer argument of call\n"; );
		}
	}

	Instruction* generateAlignedAlloc(Type* targetType, Instruction* insertBefore) {
		//always generate allocs in entry block of function
		insertBefore = insertBefore->getParent()->getParent()->getEntryBlock().getTerminator();

		AllocaInst* structPtr = new AllocaInst(targetType, mInfo->mConstInt32Two, mInfo->mAlignmentSIMD, "", insertBefore);
		PtrToIntInst* ptr2IntInst = new PtrToIntInst(structPtr, mInfo->mTargetData->getIntPtrType(*mInfo->mContext), "", insertBefore);
		BinaryOperator* andInst = BinaryOperator::Create(Instruction::And, ptr2IntInst, ConstantInt::get(mInfo->mTargetData->getIntPtrType(*mInfo->mContext), -16, true), "", insertBefore);
		BinaryOperator* addInst = BinaryOperator::Create(Instruction::Add, andInst, ConstantInt::get(mInfo->mTargetData->getIntPtrType(*mInfo->mContext), 16, true), "", insertBefore);
		IntToPtrInst* int2PtrInst = new IntToPtrInst(addInst, structPtr->getType(), "", insertBefore);
		addFalseValueInfo(structPtr); // TODO: really varying?
		addFalseValueInfo(ptr2IntInst); // TODO: really varying?
		addFalseValueInfo(andInst); // TODO: really varying?
		addFalseValueInfo(addInst); // TODO: really varying?
		addFalseValueInfo(int2PtrInst); // TODO: really varying?
		return int2PtrInst;
	}

	// This function vectorizes a single instruction by replacing the old,
	// scalar instruction by its corresponding vector counterpart.
	// It ignores mask information, so the decision for which instructions this
	// function should be called has to be made beforehand.
	// In general, all operands will be replaced by temporary vector operands
	// in order to allow creation of vector instructions without requiring a
	// fixed order of vectorization to resolve dependencies.
	// Excluded from this are all instructions that might receive mixed scalar
	// and vectorized operands and/or constants, e.g. call, gep, alloca, ...
	// These cases handle everything on their own.
	bool packetizeInstruction(Instruction* oldI, SplitInfoMapType& splitInfoMap) {

		DEBUG_PKT( outs() << "\npacketizing instruction: "
				<< *oldI << " (type: " << *oldI->getType() << ")\n"; );

		assert (!isPacketizedType(oldI->getType()) &&
			"must not request packetization of already packetized instruction!");

		// The instruction is not allowed to be marked for full splitting or
		// replication (in these cases the scalar value must not be altered).
		// It is only allowed to be marked as SPLIT_RESULT, which means we
		// vectorize normally but afterwards also need the scalar results.
		assert (!analysisResults->requiresReplication(oldI) &&
				"value marked for replication should never be vectorized!");
		assert (!analysisResults->requiresSplitFull(oldI) &&
				"value marked for full splitting should never be vectorized!");
		assert (!analysisResults->requiresSplitFullGuarded(oldI) &&
				"value marked for full splitting should never be vectorized!");

		// TODO: remove splitInfoMap stuff!
		{
			// If the value is marked for splitting, ignore it unless it is marked
			// as SPLIT_RESULT - which means we vectorize normally but afterwards
			// also need the scalar results.
			SplitInfoMapType::iterator tmp = splitInfoMap.find(oldI);
			if (tmp != splitInfoMap.end() && !tmp->second->requiresResultSplit) {
				assert (!tmp->second->requiresReplication &&
						"value marked for replication should never be vectorized!");
				DEBUG_PKT( outs() << "  is marked for splitting - ignored!\n"; );
				return true;
			}
		}

		// NOTE: The instruction will never be marked as SPLIT_REPLICATE:
		//       These values are defined to be uniform and thus this function
		//       will never be called with them as arguments.
		assert (!analysisResults->requiresReplication(oldI));


		const bool requiresCustomHandling = isa<AllocaInst>(oldI) ||
											isa<CallInst>(oldI) ||
											isa<LoadInst>(oldI) ||
											isa<StoreInst>(oldI) ||
											isa<GetElementPtrInst>(oldI) ||
											//isa<CmpInst>(oldI) || // TODO: remove all special cases etc. and implement in handler directly
											//isa<FCmpInst>(oldI) || // TODO: remove all special cases etc. and implement in handler directly
											isa<SelectInst>(oldI);

		// If any of the instruction's operands is a constant, packetize it.
		// NOTE: Only do this if its no instruction that allows indexing
		//       (gep, alloca, ...?).
		// NOTE: Handling some instructions differently solves an additional
		//       problem: xor etc. need i1 -> 4xi32, cmps need i1 -> 4xf32
		if (!requiresCustomHandling) {
			if (!packetizeConstantOperands(oldI)) return false;
		}

		//This vector holds dummies for operands which are not packetized yet.
		//It is required because LLVM does not allow to change the type of an instruction
		//and it does not allow to create an instruction with operands of the wrong type.
		//What it does allow is to 'uncheckedReplaceUsesWith' which can create
		//temporarily wrong instructions.
		// NOTE: Be sure to treat instructions with different returntypes correctly!
		// NOTE: We don't care about float/int/whatever here, each instruction has to
		//       care about creating bitcasts to ensure correct types itself!
		// NOTE: Selects are treated separately because they are allowed to
		//       receive uniform (scalar) parameters.
		// NOTE: Load/store are responsible for creating their own dummies.
		// NOTE: The dummy-vector is also used by the customly handled instructions.
		std::vector<std::pair<Instruction*, Value*> > dummies;
		Type* oldType = oldI->getType();

		// Replace operands by vector dummies if not of vector type already due
		// to finished vectorization.
		if (!requiresCustomHandling) {
			for (Instruction::op_iterator OP=oldI->op_begin(); OP!=oldI->op_end(); ++OP) {
				// Constants are packetized above. Uniform arguments need to be
				// broadcasted, so we only do something for instructions and
				// arguments (ignore BasicBlocks, Functions, etc.).
				if (!isa<Instruction>(OP) && !isArgOrArgCast(cast<Value>(OP))) continue;
				Value* opV = *OP;

				if (opV->getType()->isVoidTy() || isPacketizedType(opV->getType())) continue;

				Instruction* dummyI = createDummy(Packetizer::packetizeSIMDType(opV->getType(), *mInfo), opV, oldI);
				dummies.push_back(std::make_pair(dummyI, opV));

				Packetizer::uncheckedReplaceAllUsesWith(opV, dummyI);
			}
		}


		const std::string name = oldI->getNameStr();
		Instruction* newI = NULL;

		//the big nasty switch :P
		switch (oldI->getOpcode()) {
			// Terminators
			case Instruction::Ret:
			{
				if (oldI->getNumOperands() < 1) newI = oldI;
				else {
					newI = ReturnInst::Create(*mInfo->mContext, oldI->getOperand(0), oldI);
					addValueInfo(newI, oldI);
				}
				break;
			}
			case Instruction::Br:
			{
				newI = oldI; // ignore
				break;
			}
			case Instruction::Switch: // fallthrough
			case Instruction::Invoke: // fallthrough
			case Instruction::Unwind: // fallthrough
			case Instruction::Unreachable:
			{
				newI = NULL;
				break;
			}

			// Standard binary operators
			case Instruction::Add:
			{
				newI = BinaryOperator::Create(Instruction::Add, oldI->getOperand(0), oldI->getOperand(1), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::FAdd:
			{
				newI = BinaryOperator::Create(Instruction::FAdd, oldI->getOperand(0), oldI->getOperand(1), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::Sub:
			{
				newI = BinaryOperator::Create(Instruction::Sub, oldI->getOperand(0), oldI->getOperand(1), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::FSub:
			{
				newI = BinaryOperator::Create(Instruction::FSub, oldI->getOperand(0), oldI->getOperand(1), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::Mul:
			{
				newI = BinaryOperator::Create(Instruction::Mul, oldI->getOperand(0), oldI->getOperand(1), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::FMul:
			{
				newI = BinaryOperator::Create(Instruction::FMul, oldI->getOperand(0), oldI->getOperand(1), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::UDiv:
			{
				newI = BinaryOperator::Create(Instruction::UDiv, oldI->getOperand(0), oldI->getOperand(1), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::SDiv:
			{
				newI = BinaryOperator::Create(Instruction::SDiv, oldI->getOperand(0), oldI->getOperand(1), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::FDiv:
			{
				newI = BinaryOperator::Create(Instruction::FDiv, oldI->getOperand(0), oldI->getOperand(1), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::URem:
			{
				newI = BinaryOperator::Create(Instruction::URem, oldI->getOperand(0), oldI->getOperand(1), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::SRem:
			{
				newI = BinaryOperator::Create(Instruction::SRem, oldI->getOperand(0), oldI->getOperand(1), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::FRem:
			{
				newI = BinaryOperator::Create(Instruction::FRem, oldI->getOperand(0), oldI->getOperand(1), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}

			// Logical operators
			case Instruction::And:
			{
				Instruction* bcInst1 = new BitCastInst(oldI->getOperand(0), mInfo->mVectorTyIntSIMD, "", oldI);
				Instruction* bcInst2 = new BitCastInst(oldI->getOperand(1), mInfo->mVectorTyIntSIMD, "", oldI);
				BinaryOperator* andInst = BinaryOperator::Create(Instruction::And, bcInst1, bcInst2, name, oldI);
				addValueInfo(bcInst1, oldI);
				addValueInfo(bcInst2, oldI);
				addValueInfo(andInst, oldI);
				Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);
				if (newType == mInfo->mVectorTyFloatSIMD) {
					newI = new BitCastInst(andInst, mInfo->mVectorTyFloatSIMD, "", oldI);
					addValueInfo(newI, oldI);
				}
				else newI = andInst;
				break;
			}
			case Instruction::Or:
			{
				Instruction* bcInst1 = new BitCastInst(oldI->getOperand(0), mInfo->mVectorTyIntSIMD, "", oldI);
				Instruction* bcInst2 = new BitCastInst(oldI->getOperand(1), mInfo->mVectorTyIntSIMD, "", oldI);
				BinaryOperator* orInst = BinaryOperator::Create(Instruction::Or, bcInst1, bcInst2, name, oldI);
				addValueInfo(bcInst1, oldI);
				addValueInfo(bcInst2, oldI);
				addValueInfo(orInst, oldI);
				Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);
				if (newType == mInfo->mVectorTyFloatSIMD) {
					newI = new BitCastInst(orInst, mInfo->mVectorTyFloatSIMD, "", oldI);
					addValueInfo(newI, oldI);
				}
				else newI = orInst;
				break;
			}
			case Instruction::Xor:
			{
				Instruction* bcInst1 = new BitCastInst(oldI->getOperand(0), mInfo->mVectorTyIntSIMD, "", oldI);
				Instruction* bcInst2 = new BitCastInst(oldI->getOperand(1), mInfo->mVectorTyIntSIMD, "", oldI);
				BinaryOperator* xorInst = BinaryOperator::Create(Instruction::Xor, bcInst1, bcInst2, name, oldI);
				addValueInfo(bcInst1, oldI);
				addValueInfo(bcInst2, oldI);
				addValueInfo(xorInst, oldI);
				Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);
				if (newType == mInfo->mVectorTyFloatSIMD) {
					newI = new BitCastInst(xorInst, mInfo->mVectorTyFloatSIMD, "", oldI);
					addValueInfo(newI, oldI);
				}
				else newI = xorInst;
				break;
			}

			// Memory instructions
			case Instruction::Alloca:
			{
				AllocaInst* oldAlloca = cast<AllocaInst>(oldI);
				// TODO: does simply taking the contained type work in all cases?
				Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);
				newI = new AllocaInst(newType->getContainedType(0), oldAlloca->getArraySize(), mInfo->mAlignmentSIMD, name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::Load:
			{
				LoadInst* oldLoad = cast<LoadInst>(oldI);
				newI = packetizeLoad(oldLoad);
				DEBUG_PKT( if (!newI) errs() << "ERROR: packetization of load failed!\n"; );
				break;
			}
			case Instruction::Store:
			{
				StoreInst* oldStore = cast<StoreInst>(oldI);
				newI = packetizeStore(oldStore);
				DEBUG_PKT( if (!newI) errs() << "ERROR: packetization of store failed!\n"; );
				break;
			}
			case Instruction::GetElementPtr:
			{
				GetElementPtrInst* oldGEP = cast<GetElementPtrInst>(oldI);
				newI = packetizeGEP(oldGEP);
				DEBUG_PKT( if (!newI) errs() << "ERROR: packetization of GEP failed!\n"; );
				break;
			}

			// Convert instructions
			case Instruction::Trunc:   // fallthrough
			case Instruction::SExt:    // fallthrough
			case Instruction::FPTrunc: // fallthrough
			case Instruction::FPExt:   // fallthrough
			case Instruction::ZExt:
			{
				// If the target type still fits into a SIMD register after packetization,
				// go ahead. Otherwise, ZExt to largest possible type and issue warning.
				if (oldType->getPrimitiveSizeInBits() <= 32U) {
					Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);
					if (isa<TruncInst>(oldI))
						newI = new TruncInst(oldI->getOperand(0), newType, name, oldI);
					else if (isa<SExtInst>(oldI))
						newI = new SExtInst(oldI->getOperand(0), newType, name, oldI);
					else if (isa<ZExtInst>(oldI))
						newI = new ZExtInst(oldI->getOperand(0), newType, name, oldI);
					else if (isa<FPExtInst>(oldI))
						newI = new FPExtInst(oldI->getOperand(0), newType, name, oldI);
					else if (isa<FPTruncInst>(oldI))
						newI = new FPTruncInst(oldI->getOperand(0), newType, name, oldI);
					else
						throw std::logic_error("INTERNAL ERROR: bad inst found!");
					addValueInfo(newI, oldI);
					break;
				}

				Value* operand = oldI->getOperand(0);

				Constant* oldC = dyn_cast<Constant>(operand);
				if (oldC) {
					// Create new constant that fits into SIMD register
					// or throw exception if constant is too large for this.
					operand = Packetizer::getMax32BitConstant(oldC, *mInfo->mContext);
					assert (operand);
				}

				if (!oldC) {
					errs() << "WARNING: ZExt with target type size > 32bit found,"
							<< " might lose precision!\n";
				}

				Type* newType = oldType->isFloatTy() ?
					mInfo->mVectorTyFloatSIMD : mInfo->mVectorTyIntSIMD;

				// We replace the sext/fpext/etc. by its operand, so we have to
				// merge the split info of both values into the operand.
				// TODO: Update entire value info.
				if (newType == operand->getType()) {
					assert (isa<Instruction>(operand));
					newI = cast<Instruction>(operand);
					mergeSplitInfo(newI, operand);
					break;
				}

				if (isa<TruncInst>(oldI))
					newI = new TruncInst(operand, newType, name, oldI);
				else if (isa<SExtInst>(oldI))
					newI = new SExtInst(operand, newType, name, oldI);
				else if (isa<ZExtInst>(oldI))
					newI = new ZExtInst(operand, newType, name, oldI);
				else if (isa<FPExtInst>(oldI))
					newI = new FPExtInst(operand, newType, name, oldI);
				else if (isa<FPTruncInst>(oldI))
					newI = new FPTruncInst(operand, newType, name, oldI);
				else
					throw std::logic_error("INTERNAL ERROR: bad inst found!");
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::FPToUI:
			{
				Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);
			 	newI = new FPToUIInst(oldI->getOperand(0), newType, name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::FPToSI:
			{
				Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);
			 	newI = new FPToSIInst(oldI->getOperand(0), newType, name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::UIToFP:
			{
				Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);
			 	newI = new UIToFPInst(oldI->getOperand(0), newType, name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::SIToFP:
			{
				Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);
			 	newI = new SIToFPInst(oldI->getOperand(0), newType, name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::IntToPtr:
			{
				Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);
			 	newI = new IntToPtrInst(oldI->getOperand(0), newType, name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::PtrToInt:
			{
				Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);
			 	newI = new PtrToIntInst(oldI->getOperand(0), newType, name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::BitCast:
			{
				Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);
			 	newI = new BitCastInst(oldI->getOperand(0), newType, name, oldI);
				addValueInfo(newI, oldI);
				break;
			}

			// Other instructions...
			case Instruction::ICmp:
			{
				ICmpInst* icmp = cast<ICmpInst>(oldI);

				if (!mInfo->mUseAVX) {
					// There is no pcmp for AVX yet, so we only create this for
					// SSE.
					BitCastInst* bcInst1 = new BitCastInst(icmp->getOperand(0), mInfo->mVectorTyIntSIMD, "icmp_cast1", icmp);
					BitCastInst* bcInst2 = new BitCastInst(icmp->getOperand(1), mInfo->mVectorTyIntSIMD, "icmp_cast2", icmp);
					addFalseValueInfo(bcInst1);
					addFalseValueInfo(bcInst2);
					icmp->replaceUsesOfWith(icmp->getOperand(0), bcInst1);
					icmp->replaceUsesOfWith(icmp->getOperand(1), bcInst2);
					newI = packetizeICmpInst(icmp);
					break;
				}

				//workaround: cast, fcmp, cast
				//create cast-instructions that convert input-values to float4 (have to be inserted before fcmp)
				BitCastInst* bcInst1 = new BitCastInst(oldI->getOperand(0), mInfo->mVectorTyFloatSIMD, "icmp_cast1", oldI);
				BitCastInst* bcInst2 = new BitCastInst(oldI->getOperand(1), mInfo->mVectorTyFloatSIMD, "icmp_cast2", oldI);
				addFalseValueInfo(bcInst1);
				addFalseValueInfo(bcInst2);
				//construct fcmp with dummies and converted predicate
				Instruction* dummy = createDummy(Type::getFloatTy(*mInfo->mContext), oldI, oldI);
				CmpInst* fcmp = CmpInst::Create(Instruction::FCmp, convertICmpToFCmpPredicate(icmp->getPredicate()), dummy, dummy, "icmp_fcmp", oldI);
				addValueInfo(fcmp, oldI);
				//replace dummies of fcmp with bitcasts to correct type for packetization
				fcmp->setOperand(0, bcInst1);
				fcmp->setOperand(1, bcInst2);
				removeFromInstructionInfo(dummy);
				dummy->eraseFromParent();
				//packetize fcmp
				Instruction* fcmp4 = packetizeFCmpInst(cast<FCmpInst>(fcmp));
				removeFromInstructionInfo(fcmp);
				fcmp->eraseFromParent(); //oldI is removed automatically, dummy-fcmp is not
#ifdef PACKETIZER_USE_SCALAR_MASKS
				newI = fcmp4;
#else
				//cast back to packetized old type
				Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);
				newI = new BitCastInst(fcmp4, newType, "icmp_res", oldI);
				addValueInfo(newI, oldI);
#endif
				break;
			}

			case Instruction::FCmp:
			{
				newI = packetizeFCmpInst(cast<FCmpInst>(oldI));
#ifndef PACKETIZER_USE_SCALAR_MASKS
				//cast back to packetized old type
				// TODO: activate this again when we got rid of the SSE intrinsics. #10
				//Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);
				Type* newType = mInfo->mVectorTyFloatSIMD;
				newI = new BitCastInst(newI, newType, "icmp_res", oldI);
				addValueInfo(newI, oldI);
#endif
				break;
			}

			case Instruction::PHI:
			{
				PHINode* oldPhi = cast<PHINode>(oldI);

				Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);
				if (newType != mInfo->mVectorTyFloatSIMD &&
						newType != mInfo->mVectorTyIntSIMD &&
						(isa<PointerType>(newType) && newType->getContainedType(0)->isPrimitiveType() && newType != mInfo->getPointerVectorType(cast<PointerType>(newType)))
				   ) {
					errs() << "ERROR: only int or float allowed as operand types for packet phi!\n";
					return false;
				}

				//stores dummies and the values that are temporarily replaced
				std::vector<std::pair<Instruction*, Value*> > replacedValues;

				for (unsigned i=0; i<oldPhi->getNumIncomingValues(); ++i) {
					Value* incV = oldPhi->getIncomingValue(i);

					//if it is already packetized, ignore this value
					if (isPacketizedType(incV->getType())) continue;

					// use type of operand for dummy to be able to create correct bitcasts after creation of new phi
					Type* newIncVType = Packetizer::packetizeSIMDType(incV->getType(), *mInfo);

					// Replace all incoming values that are not of correct type by dummies.
					// Store the dummy-instruction pairs to be able to undo this after creation of new phi.
					// Constant incoming values have already been packetized.
					if (isa<Instruction>(incV)) {
						Instruction* incI = cast<Instruction>(incV);
						Instruction* dummyV = createDummy(newIncVType, incI, incI);
						Packetizer::uncheckedReplaceAllUsesWith(incI, dummyV);
						replacedValues.push_back(std::make_pair(dummyV, incI));
					} else if (isa<UndefValue>(incV)) {
						oldPhi->replaceUsesOfWith(incV, UndefValue::get(newIncVType));
					} else {
						errs() << "ERROR: found non-instruction incoming value "
							<< "to phi with scalar type: " << *incV << "\n";
						return false;
					}
				}

				//now create new phi
				PHINode* newPhi = PHINode::Create(newType, oldPhi->getNumIncomingValues(), oldPhi->getNameStr(), oldPhi);
				addValueInfo(newPhi, oldI);

				for (unsigned i=0; i<oldPhi->getNumIncomingValues(); ++i) {
					Value* incV = oldPhi->getIncomingValue(i);
					if (incV->getType() != newType) {
						if (Instruction* incInstr = dyn_cast<Instruction>(incV)) {
							BitCastInst* bcInst = new BitCastInst(incV, newType, "phi_cast", incInstr);
							addValueInfo(bcInst, oldI);
							incInstr->moveBefore(bcInst);
							newPhi->addIncoming(bcInst, oldPhi->getIncomingBlock(i));
						} else {
							BitCastInst* bcInst = new BitCastInst(incV, newType, "phi_cast", oldPhi->getIncomingBlock(i)->getTerminator());
							addValueInfo(bcInst, oldI);
							newPhi->addIncoming(bcInst, oldPhi->getIncomingBlock(i));
						}
					} else {
						newPhi->addIncoming(oldPhi->getIncomingValue(i), oldPhi->getIncomingBlock(i));
					}
				}

				//remove dummy values and replace their uses with the corresponding instructions again
				for (unsigned i=0; i<replacedValues.size(); ++i) {
					std::pair<Instruction*, Value*> viPair = replacedValues[i];
					removeFromInstructionInfo(viPair.first);
					Packetizer::uncheckedReplaceAllUsesWith(viPair.first, viPair.second);
					viPair.first->eraseFromParent();
				}

				newI = newPhi;
				break;
			}

			case Instruction::Select:
			{
				SelectInst* oldSelect = cast<SelectInst>(oldI);
				newI = packetizeSelect(oldSelect);
				DEBUG_PKT( if (!newI) errs() << "ERROR: packetization of select failed!\n"; );
				break;
			}

			case Instruction::Call:
			{
				CallInst* oldCall = cast<CallInst>(oldI);
				newI = packetizeCall(oldCall);
				DEBUG_PKT( if (!newI) errs() << "ERROR: packetization of call failed!\n"; );
				break;
			}

			case Instruction::Shl:
			{
				newI = BinaryOperator::Create(Instruction::Shl, oldI->getOperand(0), oldI->getOperand(1), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::LShr:
			{
				newI = BinaryOperator::Create(Instruction::LShr, oldI->getOperand(0), oldI->getOperand(1), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::AShr:
			{
				newI = BinaryOperator::Create(Instruction::AShr, oldI->getOperand(0), oldI->getOperand(1), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::VAArg: // fallthrough
			case Instruction::ExtractElement: // fallthrough
			case Instruction::InsertElement: // fallthrough
			case Instruction::ShuffleVector:
			{
				newI = NULL; break;
			}
			case Instruction::ExtractValue:
			{
				ExtractValueInst* ex = cast<ExtractValueInst>(oldI);
				newI = ExtractValueInst::Create(ex->getAggregateOperand(), ArrayRef<unsigned>(ex->idx_begin(), ex->idx_end()), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}
			case Instruction::InsertValue:
			{
				InsertValueInst* in = cast<InsertValueInst>(oldI);
				newI = InsertValueInst::Create(in->getAggregateOperand(), in->getInsertedValueOperand(), ArrayRef<unsigned>(in->idx_begin(), in->idx_end()), name, oldI);
				addValueInfo(newI, oldI);
				break;
			}

			default:
			{
				newI = NULL;
				break;
			}
		}

		// If we inserted any dummies, be sure to remove them again.
		for (unsigned i=0; i<dummies.size(); ++i) {
			removeFromInstructionInfo(dummies[i].first);
			Packetizer::uncheckedReplaceAllUsesWith(dummies[i].first, dummies[i].second);
			dummies[i].first->eraseFromParent();
		}

		if (!newI) {
			errs() << "ERROR: packetization of instruction '"
					<< oldI->getOpcodeName() << "' not supported!\n";
			return false;
		} else if (newI == oldI) {
			DEBUG_PKT( outs() << "left old instruction untouched: "
					<< *newI << "\n"; );
			return true; // must not update any masks/uses
		} else {
			DEBUG_PKT( outs() << "inserted new instruction: "
					<< *newI << "\n"; );
		}

		// If this was a mask operation, update mask graph.
		//if (analysisResults->isMask(oldI)) { // TODO: does not work yet :p
			updateMasks(oldI, newI);
		//}

		// We expect the created return type to match the expected one.
		// This has to be ensured by all handlers.
		DEBUG_PKT(
			if (!newI->getType()->isVoidTy() && !isPacketizedType(newI->getType())) {
				errs() << "ERROR: type of new instruction (" << *newI->getType()
					<< ") is no packetized type!\n";
				assert (false && "type of packetized instruction has to be packetized!");
			}
		);

		// TODO: remove splitInfoMap stuff, it is fully redundant by now!
		{
			// If the old instruction is in splitInfoMap, the new one also must
			// be in the map, marked as SPLIT_RESULT.
			// Thus, we have to update splitInfoMap.
			// TODO: This here really has to be removed, it could cause bad things, e.g. if
			//       the old instruction was marked as SPLIT_FULL.
			SplitInfoMapType::iterator tmp2 = splitInfoMap.find(oldI);
			if (tmp2 != splitInfoMap.end()) {
				assert (tmp2->second->requiresResultSplit);
				SplitInfo* info = new SplitInfo(newI, false, true, false);
				splitInfoMap.insert(std::make_pair(newI, info));
				delete tmp2->second;
				splitInfoMap.erase(tmp2);
				analysisResults->setSplitInfo(newI, AnalysisResults::SPLIT_RESULT); // keep analysis results up to date
			}
		}

		// Remove old instruction and replace its uses.
		removeFromInstructionInfo(oldI);
		Packetizer::uncheckedReplaceAllUsesWith(oldI, newI);
		oldI->eraseFromParent();

		return true;
	}

	bool packetizeConstantOperands(Instruction* oldI) {
		assert (oldI);
		for (unsigned i=0; i<oldI->getNumOperands(); ++i) {
			Value* opV = oldI->getOperand(i);
			//DEBUG_PKT( outs() << "    operand(" << i << "): " << *opV << "\n"; );
			if (isa<Function>(opV)) continue; //TODO: should be handled outside (e.g. CallInst)
			//if (isa<GlobalValue>(opV)) continue; //have to somehow ignore global arrays...
			if (isa<Constant>(opV)) {
				Constant* newC = createPacketConstant(cast<Constant>(opV));
				if (!newC) return false;

				oldI->setOperand(i, newC);
			}
		}
		return true;
	}

	// TODO: use analysisResults->isMask()
	void updateMasks(Instruction* oldI, Instruction* newI) {
		assert (oldI && newI);
		assert (!entryMasks.empty() && "entry mask mapping must not be empty!");
		assert (!maskGraph->empty() && "mask graph must not be empty!");
		std::map<Value*, std::set<BasicBlock*>* >::iterator tmp = entryMasks.find(oldI);

		if (tmp == entryMasks.end()) return;

		DEBUG_PKT( outs() << "instruction is a mask, updating mask-map...\n"; );

		//update maskMap of blocks using oldI as entry mask
		std::set<BasicBlock*>* blocks = tmp->second;
		for (std::set<BasicBlock*>::iterator it=blocks->begin(); it!=blocks->end(); ++it) {
			BasicBlock* block = *it;
			MaskGraphNode* node = maskGraph->findMaskNode(block);
			assert (node);
			if (node->hasExitEdge()) {
				Value* entryMask = node->getEntryMaskVal();
				Value* exitMaskT = node->hasExitEdge() ? node->getExitMaskTrueVal() : NULL;
				assert (entryMask);
				if (exitMaskT == entryMask) node->setExitMaskTrue(newI);
				if (node->hasConditionalExit() && entryMask == node->getExitMaskFalseVal()) node->setExitMaskFalse(newI);
			}
			node->setEntryMask(newI);
		}

		//update entryMaskMap
		entryMasks.erase(oldI);
		assert (entryMasks.find(newI) == entryMasks.end());
		entryMasks.insert(std::make_pair(newI, blocks));

		//update maskMap of parent of oldI (oldI is an exit mask of its parent-block)
		assert (oldI->getParent());
		MaskGraphNode* parentNode = maskGraph->findMaskNode(oldI->getParent());
		assert (parentNode);
		if (parentNode->getExitMaskTrueVal() == oldI) parentNode->setExitMaskTrue(newI);
		if (parentNode->hasConditionalExit() && parentNode->getExitMaskFalseVal() == oldI) parentNode->setExitMaskFalse(newI);
	}


	void updateValueInfo(Instruction* oldI, Instruction* newI, bool isUniform) {
		assert (oldI && newI);
		DEBUG_PKT( outs() << "updating instruction in instructionInfo-map...\n"; );
		AnalysisResults::ValueInfo* vi = analysisResults->getValueInfo(oldI);
		assert (vi);

		analysisResults->addValueInfo(newI,
										   isUniform ? AnalysisResults::UNIFORM : AnalysisResults::VARYING,
										   vi->indexInfo,
										   vi->alignmentInfo,
										   vi->splitInfo,
										   vi->isMask);

		assert (analysisResults->getValueInfo(newI));

		analysisResults->removeValueInfo(oldI);
	}
	// This function sets the valueInfo of newI to the values of oldI
	void updateValueInfo(Instruction* oldI, Instruction* newI) {
		assert (oldI && newI);
		DEBUG_PKT( outs() << "updating instruction in instructionInfo-map...\n"; );
		AnalysisResults::ValueInfo* vi = analysisResults->getValueInfo(oldI);
		assert (vi);

		analysisResults->addValueInfo(newI,
										   vi->uniformInfo,
										   vi->indexInfo,
										   vi->alignmentInfo,
										   vi->splitInfo,
										   vi->isMask);

		assert (analysisResults->getValueInfo(newI));

		analysisResults->removeValueInfo(oldI);
	}
	void setSplitInfo(Value* value, const AnalysisResults::SplitInfo& si) {
		assert (value);
		DEBUG_PKT( outs() << "updating split info of instruction...\n"; );
		analysisResults->setSplitInfo(value, si, true /* forceUpdate */);
	}

	// Set SplitInfo of 'value' to the union of value and copyFrom.
	void mergeSplitInfo(Value* value, const Value* copyFrom) {
		assert (value && copyFrom);
		DEBUG_PKT( outs() << "merging split info of two values...\n"; );
		AnalysisResults::ValueInfo* vi1 = analysisResults->getValueInfo(value);
		AnalysisResults::ValueInfo* vi2 = analysisResults->getValueInfo(copyFrom);
		assert (vi2 && vi2);

		AnalysisResults::SplitInfo& si1 = vi1->splitInfo;
		AnalysisResults::SplitInfo& si2 = vi2->splitInfo;
		if (si1 == AnalysisResults::SPLIT_FULL_GUARDED ||
				si2 == AnalysisResults::SPLIT_FULL_GUARDED)
		{
			analysisResults->setSplitInfo(value, AnalysisResults::SPLIT_FULL_GUARDED);
			return;
		}
		if (si1 == AnalysisResults::SPLIT_FULL ||
				si2 == AnalysisResults::SPLIT_FULL)
		{
			analysisResults->setSplitInfo(value, AnalysisResults::SPLIT_FULL);
			return;
		}
		if (si1 == AnalysisResults::SPLIT_RESULT ||
				si2 == AnalysisResults::SPLIT_RESULT)
		{
			analysisResults->setSplitInfo(value, AnalysisResults::SPLIT_RESULT);
			return;
		}
		if (si1 == AnalysisResults::SPLIT_REPLICATE ||
				si2 == AnalysisResults::SPLIT_REPLICATE)
		{
			analysisResults->setSplitInfo(value, AnalysisResults::SPLIT_REPLICATE);
			return;
		}
		analysisResults->setSplitInfo(value, AnalysisResults::SPLIT_NEVER);
	}

	inline void addValueInfo(Value* value, const Value* copyFrom) {
		assert (value && copyFrom);
		analysisResults->addValueInfo(value, copyFrom);
	}

	inline void addFalseValueInfo(Value* value) {
		analysisResults->addValueInfoFalse(value);
	}

	inline void addValueInfo(Value* value,
							 const bool isUniform,
							 const bool isSame,
							 const bool isConsecutive,
							 const bool isAligned,
							 const bool requiresReplication,
							 const bool requiresResultSplit,
							 const bool requiresFullSplit,
							 const bool requiresFullSplitGuarded)
	{
		assert (value);
		assert (!(isSame && isConsecutive));

		assert (!analysisResults->getValueInfo(value));

		const AnalysisResults::UniformInfo ui = isUniform ?
			AnalysisResults::UNIFORM :
			AnalysisResults::VARYING;

		const AnalysisResults::IndexInfo ii = isSame ?
			AnalysisResults::INDEX_SAME :
				isConsecutive ?
				AnalysisResults::INDEX_CONSECUTIVE :
				AnalysisResults::INDEX_RANDOM;

		const AnalysisResults::AlignmentInfo ai = isAligned ?
			AnalysisResults::ALIGN_TRUE :
			AnalysisResults::ALIGN_FALSE;

		const AnalysisResults::SplitInfo si = requiresReplication ?
			AnalysisResults::SPLIT_REPLICATE : requiresResultSplit ?
			AnalysisResults::SPLIT_RESULT : requiresFullSplitGuarded ?
			AnalysisResults::SPLIT_FULL_GUARDED : requiresFullSplit ?
			AnalysisResults::SPLIT_FULL :
			AnalysisResults::SPLIT_NEVER;

		analysisResults->addValueInfo(value, ui, ii, ai, si); // memory leak here
	}

	void removeFromInstructionInfo(Value* value) {
		assert (value);
		analysisResults->removeValueInfo(value);
	}


	Instruction* packetizeLoad(LoadInst* oldLoad) {
		assert (oldLoad);
		DEBUG_PKT( outs() << "packetizing load: " << *oldLoad << "\n"; );
		Value* pointer = oldLoad->getPointerOperand();
		DEBUG_PKT( outs() << "  pointer: " << *pointer << "\n"; );

		// values to be split should be caught before calling this function
		assert (!analysisResults->requiresSplitFull(oldLoad));

		if (analysisResults->isSame(pointer)) {
			// This actually would be a bit weird to happen, VARYING / INDEX_SAME
			// should be UNIFORM / INDEX_SAME I think.
			DEBUG_PKT( errs() << "WARNING: VARYING load has INDEX_SAME mark -> "
					<< "not transformed to vector load but left scalar!\n"; );
			return oldLoad;
		}

		assert (analysisResults->isConsecutive(pointer));

		DEBUG_PKT( outs() << "    pointer operand is INDEX_CONSECUTIVE, "
			<< "creating vector load!\n"; );

#ifdef PACKETIZER_FORCE_ALIGNED_MEMOPS
		const bool isAligned = true;
#else
		const bool isAligned = analysisResults->isAligned(pointer);
#endif
		const unsigned alignment = isAligned ? mInfo->mAlignmentSIMD : 1;

		if (!isAligned) {
			DEBUG_PKT( outs() << "  unaligned vector load required!\n"; );
		}

		// Create pointer cast (redundant if pointer is a pointer to a
		// vector later, but required if e.g. the target array is uniform and
		// the pointer is INDEX_CONSECUTIVE).
		// In any case, the bitcast is required as a dummy to create a vector load.
		Value* pktPtrCast = createPointerCast(pointer, oldLoad);

		Instruction* newLoad = new LoadInst(pktPtrCast,
											oldLoad->getName(),
											oldLoad->isVolatile(),
											alignment,
											oldLoad);
		addValueInfo(newLoad, oldLoad);

#ifndef PACKETIZER_SILENT_MODE
		outs() << "LOAD WAS VECTORIZED" << (isAligned ? "!\n" : " (UNALIGNED)!\n");
#endif

		return newLoad;
	}

	Instruction* packetizeStore(StoreInst* oldStore) {
		assert (oldStore);
		DEBUG_PKT( outs() << "packetizing store: " << *oldStore << "\n"; );
		DEBUG_PKT( outs() << "  pointer: " << *oldStore->getPointerOperand() << "\n"; );
		DEBUG_PKT( outs() << "  value: " << *oldStore->getValueOperand() << "\n"; );

		Value* pointer = oldStore->getPointerOperand();
		Value* value = oldStore->getValueOperand();

		// values to be split should be caught before calling this function
		assert (!analysisResults->requiresSplitFull(oldStore));

		DEBUG_PKT( outs() << "  block entry mask is FULLY_UNIFORM and pointer is not INDEX_RANDOM -> can use vector store!\n"; );

		if (analysisResults->isSame(pointer)) {

			// If the pointer is the same for all instances, but a VARYING value
			// should be stored, this means that there is a race condition.
			// NOTE: This should have been detected during vectorization
			//       analysis already, but if desired we just do "something" ;).
			if (!analysisResults->isUniform(value) &&
					!analysisResults->isSame(value))
			{
				errs() << "WARNING: Creating scalar store that writes last "
						<< "value of vector to UNIFORM pointer (fixed behavior "
						<< "for race conditions)!\n";

			} else {
				// Otherwise, all instances store the same value to the same
				// location, so we can simply do a single scalar store.
				DEBUG_PKT( outs() << "    pointer operand is INDEX_SAME, "
						<< "have to store W times to the same location "
						<< "(can optimize: single scalar store)!\n"; );
			}

			Value* lastElemIdx = ConstantInt::get(*mInfo->mContext,
												  APInt(32, mInfo->mSimdWidth-1));
			Value* lastElem = generateHorizontalExtract(value,
														lastElemIdx,
														"",
														oldStore,
														oldStore);
			oldStore->replaceUsesOfWith(value, lastElem);

			return oldStore;
		}

		assert (analysisResults->isConsecutive(pointer));
		DEBUG_PKT( outs() << "    pointer operand is INDEX_CONSECUTIVE!\n"; );

		// If we attempt to store to a UNIFORM / INDEX_CONSECUTIVE pointer,
		// we can optimize store operations that store vectors (no compound
		// data types) by creating a vector store via a pointer cast.
		// Otherwise (e.g. to store to a uniform array of structs), we have
		// to do a "scatter" (= split).
		// This is caught above (hasNonSimpleType).

#ifdef PACKETIZER_FORCE_ALIGNED_MEMOPS
		const bool isAligned = true;
#else
		const bool isAligned = analysisResults->isAligned(pointer);
#endif
		const unsigned alignment = isAligned ? mInfo->mAlignmentSIMD : 1;

		if (!isAligned) {
			DEBUG_PKT( outs() << "  unaligned vector store required!\n"; );
		}

		// Create pointer cast (redundant if depending on GEP that also creates
		// this bitcast, but required if e.g. directly loading from the pointer
		// (element 0).
		pointer = createPointerCast(pointer, oldStore);

		// create vectorized dummy-value
		Type* oldValueType = value->getType();
		Instruction* dummy = isPacketizedType(oldValueType) ? NULL :
			createDummy(Packetizer::packetizeSIMDType(oldValueType, *mInfo), value, oldStore);

		StoreInst* vectorStore = new StoreInst(dummy ? dummy : value,
												   pointer,
												   oldStore->isVolatile(),
												   alignment,
												   oldStore);
		addValueInfo(vectorStore, oldStore);

		if (dummy) {
			removeFromInstructionInfo(dummy);
			Packetizer::uncheckedReplaceAllUsesWith(dummy, value);
			dummy->eraseFromParent();
		}

		DEBUG_PKT( outs() << "  inserted new store: " << *vectorStore << "\n"; );

#ifndef PACKETIZER_SILENT_MODE
		outs() << "STORE WAS VECTORIZED" << (isAligned ? "!\n" : " (UNALIGNED)!\n");
#endif

		return vectorStore;
	}

	Instruction* packetizeGEP(GetElementPtrInst* oldGEP) {
		assert (oldGEP);
		DEBUG_PKT( outs() << "\npacketizing GEP: " << *oldGEP << "\n"; );

		// values to be split should be caught before calling this function
		assert (!analysisResults->requiresSplitFull(oldGEP));

		// We only have uniform indices, so we can simply create a packet GEP.
		DEBUG_PKT( outs() << "  GEP can be vectorized!\n"; );

		// create vectorized dummy-pointer
		Value* pointer = oldGEP->getPointerOperand();
		Type* oldPointerType = pointer->getType();
		const bool requiresPacketPointer =
			!analysisResults->isUniform(pointer);
		const bool pointerIsPacketized = isPacketizedType(oldPointerType);
		assert (!pointerIsPacketized || requiresPacketPointer);
		Type* vecPointerType = pointerIsPacketized ?
			oldPointerType : Packetizer::packetizeSIMDType(oldPointerType, *mInfo);

		Instruction* dummy = requiresPacketPointer ?
			createDummy(vecPointerType, pointer, oldGEP) : NULL;

		// Indices remain untouched.
		// Stupid constructor does neither allow supplying idx_begin/end
		// directly nor does GEP provide anything to retrieve whole operand-list.
		std::vector<Value*> indices;
		for (GetElementPtrInst::op_iterator IDX=oldGEP->idx_begin();
				IDX != oldGEP->idx_end(); ++IDX)
		{
			indices.push_back(*IDX);
		}

		// Create new GEP either with vectorized or with scalar pointer
		// depending on vectorization analysis.
		Instruction* newGEP = GetElementPtrInst::Create(dummy ? dummy : pointer,
														ArrayRef<Value*>(indices),
														"",
														oldGEP);
		addValueInfo(newGEP, oldGEP);

		if (dummy) {
			removeFromInstructionInfo(dummy);
			newGEP->replaceUsesOfWith(dummy, pointer);
			dummy->eraseFromParent();
		}

		DEBUG_PKT( outs() << "  inserted new GEP: " << *newGEP << "\n"; );

		// If the pointer is UNIFORM, we now have to create a pointer
		// cast of the result of the GEP (the correct address has to
		// be computed using the UNIFORM type, which is then bitcasted
		// to vector type).
		// NOTE: This is guaranteed to return an instruction if an instruction
		//       is given as first argument.
		newGEP = cast<Instruction>(createPointerCast(newGEP, oldGEP));

		assert (isPacketizedType(newGEP->getType()));

		return newGEP;
	}

	Instruction* packetizeSelect(SelectInst* oldSelect) {
		assert (oldSelect);

		Value* condition = oldSelect->getCondition();
		Value* trueValue = oldSelect->getTrueValue();
		Value* falseValue = oldSelect->getFalseValue();
		Type* newType = Packetizer::packetizeSIMDType(oldSelect->getType(), *mInfo);

		// values to be split should be caught before calling this function
		assert (!analysisResults->requiresSplitFull(oldSelect));

		if (Constant* c = dyn_cast<Constant>(oldSelect->getCondition())) {
			condition = createPacketConstant(c);
			if (!condition) return NULL;
		}
		if (Constant* c = dyn_cast<Constant>(oldSelect->getTrueValue())) {
			trueValue = createPacketConstant(c);
			if (!trueValue) return NULL;
		}
		if (Constant* c = dyn_cast<Constant>(oldSelect->getFalseValue())) {
			falseValue = createPacketConstant(c);
			if (!falseValue) return NULL;
		}

		DEBUG_PKT( outs() << "creating vector blend operation from original values: \n"; );
		DEBUG_PKT( outs() << "  cond    : " << *condition << "\n"; );
		DEBUG_PKT( outs() << "  true    : " << *trueValue << "\n"; );
		DEBUG_PKT( outs() << "  false   : " << *falseValue << "\n"; );
		DEBUG_PKT( outs() << "  old type: " << *oldSelect->getType() << "\n"; );

		Instruction* dummies[3] = { NULL, NULL, NULL };
		Value* origs[3] = { condition, trueValue, falseValue };
		// The condition of a select is allowed to remain uniform if we
		// can prove that it really is (and not just not yet packetized).
		if (!isPacketizedType(condition->getType()) &&
				!analysisResults->isUniform(condition))
		{
			dummies[0] = createDummy(Packetizer::packetizeSIMDType(condition->getType(), *mInfo), condition, oldSelect);
			condition = dummies[0];
		}

		if (!isPacketizedType(trueValue->getType())) {
			if (isa<Constant>(trueValue)) {
				trueValue = createPacketConstant(cast<Constant>(trueValue));
			} else {
				dummies[1] = createDummy(Packetizer::packetizeSIMDType(trueValue->getType(), *mInfo), trueValue, oldSelect);
				trueValue = dummies[1];
			}
		}

		if (!isPacketizedType(falseValue->getType())) {
			if (isa<Constant>(falseValue)) {
				falseValue = createPacketConstant(cast<Constant>(falseValue));
			} else {
				dummies[2] = createDummy(Packetizer::packetizeSIMDType(falseValue->getType(), *mInfo), falseValue, oldSelect);
				falseValue = dummies[2];
			}
		}

		DEBUG_PKT( outs() << "  dummies to ensure correct types: \n"; );
		DEBUG_PKT( outs() << "  cond    : " << *condition << "\n"; );
		DEBUG_PKT( outs() << "  true    : " << *trueValue << "\n"; );
		DEBUG_PKT( outs() << "  false   : " << *falseValue << "\n"; );
		DEBUG_PKT( outs() << "  new type: " << *newType << "\n"; );
		assert(trueValue->getType() == newType);
		assert(falseValue->getType() == newType);

#ifdef PACKETIZER_USE_SCALAR_MASKS
		assert (false && "NOT IMPLEMENTED!");
		// In this case, we ALWAYS have a scalar condition
		// -> be sure that all operands were vectorized beforehand
		// -> make the decision based on operand types instead of condition type

		assert (!isPacketizedType(condition->getType()) && "Must not have packetized mask if USE_SCALAR_MASKS is defined!");

		// TODO: If we have scalar masks, we cannot distinguish between uniform and vectorized,
		//       so this here will not do the right job
		if (condition->getType() != Type::getInt32Ty(*mInfo->mContext)) {
			assert (condition->getType()->isIntegerTy(1));
			DEBUG_PKT( outs() << "packetizing uniform select (scalar condition)...\n"; );
			SelectInst* sel =  SelectInst::Create(condition, trueValue, falseValue, oldSelect->getName(), oldSelect);
			addValueInfo(sel, oldSelect);
			for (unsigned i=0; i<3; ++i) {
				if (dummies[i]) {
					Packetizer::uncheckedReplaceAllUsesWith(dummies[i], origs[i]);
					removeFromInstructionInfo(dummies[i]);
					dummies[i]->eraseFromParent();
				}
			}
			return sel;
		}

		assert (condition->getType()->isIntegerTy(32));

		// NOTE: We basically have to copy the code below.
		//       However, we have to manually extract i1 values from the i32 mask
		//       in order to generate scalar selects.
		if (!trueValue->getType()->isVectorTy()) {
			// TODO: implement
			assert (false && "NOT IMPLEMENTED!");
			throw std::logic_error("INTERNAL ERROR: NOT IMPLEMENTED!");
		}

		if (mInfo->mUseAVX) {
			//version 1 - use AVX blend
			std::vector<Value* > params;
			params.clear();
			if (trueValue->getType() != mInfo->mVectorTyFloatSIMD) {
				trueValue = new BitCastInst(trueValue, mInfo->mVectorTyFloatSIMD, "blend_true", oldSelect);
				falseValue = new BitCastInst(falseValue, mInfo->mVectorTyFloatSIMD, "blend_false", oldSelect);
				addFalseValueInfo(trueValue); // TODO: really varying?
				addFalseValueInfo(falseValue); // TODO: really varying?
			}
			params.push_back(falseValue);
			params.push_back(trueValue);
			params.push_back(condition);
			Function *blendps = Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_avx_blend_ps_256);
			CallInst* call = createExternalIntrinsicCall(blendps, params, oldSelect->getName(), oldSelect);
			addValueInfo(call, oldSelect);
			Value* result = call;
			if (newType != mInfo->mVectorTyFloatSIMD) {
				result = new BitCastInst(call, newType, "blend_result", oldSelect);
				addValueInfo(result, oldSelect);
			}
			for (unsigned i=0; i<3; ++i) {
				if (dummies[i]) {
					Packetizer::uncheckedReplaceAllUsesWith(dummies[i], origs[i]);
					removeFromInstructionInfo(dummies[i]);
					dummies[i]->eraseFromParent();
				}
			}
			return result;
		} else if (mInfo->mUseSSE41) {
			//version 2 - use SSE4.1 blend
			std::vector<Value* > params;
			params.clear();
			if (trueValue->getType() != mInfo->mVectorTyFloatSIMD) {
				trueValue = new BitCastInst(trueValue, mInfo->mVectorTyFloatSIMD, "blend_true", oldSelect);
				falseValue = new BitCastInst(falseValue, mInfo->mVectorTyFloatSIMD, "blend_false", oldSelect);
				addFalseValueInfo(trueValue); // TODO: really varying?
				addFalseValueInfo(falseValue); // TODO: really varying?
			}
			params.push_back(falseValue);
			params.push_back(trueValue);
			params.push_back(condition);
			Function *blendps = Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_sse41_blendps);
			CallInst* call = createExternalIntrinsicCall(blendps, params, oldSelect->getName(), oldSelect);
			addValueInfo(call, oldSelect);
			Value* result = call;
			if (newType != mInfo->mVectorTyFloatSIMD) {
				result = new BitCastInst(call, newType, "blend_result", oldSelect);
				addValueInfo(result, oldSelect);
			}
			for (unsigned i=0; i<3; ++i) {
				if (dummies[i]) {
					Packetizer::uncheckedReplaceAllUsesWith(dummies[i], origs[i]);
					removeFromInstructionInfo(dummies[i]);
					dummies[i]->eraseFromParent();
				}
			}
			return result;
		} else {
			//version 3 - use SSE2 bit-level instructions
			// TODO: how to do this with i32-masks? :)
			assert (false && "NOT IMPLEMENTED!");
			throw std::logic_error("INTERNAL ERROR: NOT IMPLEMENTED!");
		}
#endif

		// If the condition is uniform, we do not have to create a (possibly
		// complicated) blend operation, even though the operands are packetized.
		if (analysisResults->isUniform(condition)) {
			DEBUG_PKT( outs() << "packetizing select with uniform condition...\n"; );
			SelectInst* sel = SelectInst::Create(condition, trueValue, falseValue, oldSelect->getName(), oldSelect);
			addValueInfo(sel, oldSelect);
			for (unsigned i=0; i<3; ++i) {
				if (dummies[i]) {
					Packetizer::uncheckedReplaceAllUsesWith(dummies[i], origs[i]);
					removeFromInstructionInfo(dummies[i]);
					dummies[i]->eraseFromParent();
				}
			}
			return sel;
		}

		assert(condition->getType()->isVectorTy() && "can not packetize select with condition type other than scalar or vector!");

		// If we don't have a scalar type, we have to split up the value
		// and generate 'info.simdWidth' scalar selects which are then
		// merged back to the result.
		// NOTE: This is only required because we generate blend-intrinsics.
		//       Thus, we can only handle scalars or vectors, no other types
		//       (e.g. structs of vectors). Thus, we only test with isVectorTy()
		//       instead of isPacketizedType().
		// NOTE: We do not need an if-cascade here, because the selects
		//       implicitly perform the masking.
		// TODO: We don't necessarily have to extract "down" to scalar values,
		//       generating vector blends where possible would be a lot better.
		if (!trueValue->getType()->isVectorTy()) {
			DEBUG_PKT( outs() << "\npacketizing select of complex value type...\n"; );
			assert (isa<CompositeType>(newType));
			//CompositeType* compType = cast<CompositeType>(trueValue->getType());
			// loop over each element of the type and generate select
			Value** selects = new Value*[mInfo->mSimdWidth]();

			// be sure that condition has correct type before extracting
			if (condition->getType() != mInfo->mVectorTyIntSIMD) {
				condition = new BitCastInst(condition, mInfo->mVectorTyIntSIMD, "", oldSelect);
				addFalseValueInfo(condition); // TODO: really varying?
			}
			// extract scalar values from condition
			Value** conditions = new Value*[mInfo->mSimdWidth]();
			for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
				conditions[i] = ExtractElementInst::Create(condition, ConstantInt::get(*mInfo->mContext, APInt(32, i)), "", oldSelect);
				addFalseValueInfo(conditions[i]); // TODO: really varying?
			}
			// generate check if condition is true/false (select requires i1, not i32)
			for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
				conditions[i] = CmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_NE, conditions[i], mInfo->mConstInt32Zero, "", oldSelect);
				addFalseValueInfo(conditions[i]); // TODO: really varying?
			}
			// extract true/false values
			Value** scalarTrueVals = new Value*[mInfo->mSimdWidth]();
			Value** scalarFalseVals = new Value*[mInfo->mSimdWidth]();
			DEBUG_PKT( outs() << "\ngenerating extracts for true values...\n"; );
			for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
				scalarTrueVals[i] = generateHorizontalExtract(trueValue, ConstantInt::get(*mInfo->mContext, APInt(32, i)), "", oldSelect, oldSelect);
				DEBUG_PKT( outs() << "\n  scalar true value " << i << ": " << *scalarTrueVals[i] << "\n\n"; );
			}
			DEBUG_PKT( outs() << "\ngenerating extracts for false values...\n"; );
			for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
				scalarFalseVals[i] = generateHorizontalExtract(falseValue, ConstantInt::get(*mInfo->mContext, APInt(32, i)), "", oldSelect, oldSelect);
				DEBUG_PKT( outs() << "\n  scalar false value " << i << ": " << *scalarFalseVals[i] << "\n\n"; );
			}

			DEBUG_PKT( outs() << "\ngenerating selects...\n"; );
			for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
				DEBUG_PKT( outs() << "  condition: " << *conditions[i] << "\n"; );
				DEBUG_PKT( outs() << "  true val : " << *scalarTrueVals[i] << "\n"; );
				DEBUG_PKT( outs() << "  false val: " << *scalarFalseVals[i] << "\n"; );
				selects[i] = SelectInst::Create(conditions[i], scalarTrueVals[i], scalarFalseVals[i], oldSelect->getName()+".split", oldSelect);
				DEBUG_PKT( outs() << "  new scalar select: " << *selects[i] << "\n"; );
				addFalseValueInfo(selects[i]); // TODO: really varying?
			}

			// merge results back together
			// NOTE: this again requires copying all values due to the
			//       different data layouts
			Value* result = generateHorizontalMerge(selects, newType, "", oldSelect);
			result->setName(oldSelect->getName());
			DEBUG_PKT( outs() << "  merged select result: " << *result << "\n"; );

			delete [] conditions;
			delete [] scalarTrueVals;
			delete [] scalarFalseVals;
			delete [] selects;

			for (unsigned i=0; i<3; ++i) {
				if (dummies[i]) {
					Packetizer::uncheckedReplaceAllUsesWith(dummies[i], origs[i]);
					removeFromInstructionInfo(dummies[i]);
					dummies[i]->eraseFromParent();
				}
			}

			assert (isa<Instruction>(result));
			return cast<Instruction>(result);
		}

		// Otherwise, produce a "simple" blend operation by either
		// using the SSE or AVX BLENDVPS intrinsic or by doing bit operations.

		Instruction* result = NULL;

		if (mInfo->mUseAVX) {
			//version 1 - use AVX blend
			std::vector<Value* > params;
			if (condition->getType() != mInfo->mVectorTyFloatSIMD) {
				BitCastInst* bc = new BitCastInst(condition, mInfo->mVectorTyFloatSIMD, "blend_cond", oldSelect);
				addValueInfo(bc, condition);
				condition = bc;
			}

			params.clear();
			if (trueValue->getType() != mInfo->mVectorTyFloatSIMD) {
				trueValue = new BitCastInst(trueValue, mInfo->mVectorTyFloatSIMD, "blend_true", oldSelect);
				falseValue = new BitCastInst(falseValue, mInfo->mVectorTyFloatSIMD, "blend_false", oldSelect);
				addFalseValueInfo(trueValue);
				addFalseValueInfo(falseValue);
			}
			params.push_back(falseValue);
			params.push_back(trueValue);
			params.push_back(condition);
			Function *blendvps = Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_avx_blendv_ps_256);
			CallInst* call = createExternalIntrinsicCall(blendvps, params, oldSelect->getName(), oldSelect);
			addValueInfo(call, oldSelect);
			result = call;
			if (newType != mInfo->mVectorTyFloatSIMD) {
				result = new BitCastInst(call, newType, "blend_result", oldSelect);
				addValueInfo(result, oldSelect);
			}
		} else if (mInfo->mUseSSE41) {
			//version 2 - use SSE4.1 blend
			std::vector<Value* > params;
			if (condition->getType() != mInfo->mVectorTyFloatSIMD) {
				condition = new BitCastInst(condition, mInfo->mVectorTyFloatSIMD, "blend_cond", oldSelect);
				addFalseValueInfo(condition);
			}

			params.clear();
			if (trueValue->getType() != mInfo->mVectorTyFloatSIMD) {
				trueValue = new BitCastInst(trueValue, mInfo->mVectorTyFloatSIMD, "blend_true", oldSelect);
				falseValue = new BitCastInst(falseValue, mInfo->mVectorTyFloatSIMD, "blend_false", oldSelect);
				addFalseValueInfo(trueValue);
				addFalseValueInfo(falseValue);
			}
			params.push_back(falseValue);
			params.push_back(trueValue);
			params.push_back(condition);
			Function *blendvps = Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_sse41_blendvps);
			CallInst* call = createExternalIntrinsicCall(blendvps, params, oldSelect->getName(), oldSelect);
			addValueInfo(call, oldSelect);
			result = call;
			if (newType != mInfo->mVectorTyFloatSIMD) {
				result = new BitCastInst(call, newType, "blend_result", oldSelect);
				addValueInfo(result, oldSelect);
			}
		} else {
			//version 3 - use SSE2 bit-level instructions
			CastInst* cond_i32 = new BitCastInst(condition, mInfo->mVectorTyIntSIMD, "blend_cond_i32", oldSelect);
			CastInst* res1_i32 = new BitCastInst(trueValue, mInfo->mVectorTyIntSIMD, "blend_true_i32", oldSelect);
			CastInst* res2_i32 = new BitCastInst(falseValue, mInfo->mVectorTyIntSIMD, "blend_false_i32", oldSelect);
			Instruction* xorInst = BinaryOperator::Create(Instruction::Xor, cond_i32, mInfo->mConstVecSIMDInt32MinusOne, "blend_xor", oldSelect);
			BinaryOperator* and1Inst = BinaryOperator::Create(Instruction::And, res2_i32, xorInst, "blend_and1", oldSelect);
			BinaryOperator* and2Inst = BinaryOperator::Create(Instruction::And, cond_i32, res1_i32, "blend_and2", oldSelect);
			BinaryOperator* orInst = BinaryOperator::Create(Instruction::Or, and2Inst, and1Inst, "blend_or", oldSelect);
			Instruction* result = new BitCastInst(orInst, newType, "blend_result", oldSelect);
			addFalseValueInfo(cond_i32);
			addFalseValueInfo(res1_i32);
			addFalseValueInfo(res2_i32);
			addFalseValueInfo(xorInst);
			addFalseValueInfo(and1Inst);
			addFalseValueInfo(and2Inst);
			addFalseValueInfo(orInst);
			addValueInfo(result, oldSelect);
		}

		for (unsigned i=0; i<3; ++i) {
			if (dummies[i]) {
				Packetizer::uncheckedReplaceAllUsesWith(dummies[i], origs[i]);
				removeFromInstructionInfo(dummies[i]);
				dummies[i]->eraseFromParent();
			}
		}

		return result;
	}

	Instruction* packetizeCall(CallInst* oldCall) {
		assert (oldCall);

		DEBUG_PKT( outs() << "\npacketizing call: " << *oldCall << "\n"; );

		assert (!isPacketizedType(oldCall->getType()) && "call is already packetized!");

		// values to be split should be caught before calling this function
		assert (!analysisResults->requiresSplitFull(oldCall));


		Function* f = oldCall->getCalledFunction();
		assert (f);

		DEBUG_PKT(
			outs() << "  arguments of scalar function:\n";
			for (Function::arg_iterator A=f->arg_begin(); A!=f->arg_end(); ++A) {
				outs() << "  - " << *A << "\n";
			}
		);

		//ignore calls to debug functions?
		//if (std::strstr(f->getNameStr().c_str(), "llvm.dbg") != 0) return oldCall;

		BasicBlock* startBB = oldCall->getParent();

		//find out mask
		MaskGraphNode* startBBNode = maskGraph->findMaskNode(startBB);
		assert (startBBNode && "call-block has to have associated masks!");

		Value* mask = startBBNode->getEntryMaskVal();
		assert (mask && "the mask associated to block with call is NULL!");
		DEBUG_PKT( outs() << "    mask: " << *mask << "\n"; );

		const std::string& fname = f->getNameStr();
		DEBUG_PKT( outs() << "  name of called function: " << fname << "\n"; );


#ifndef PACKETIZER_NO_PACKETIZED_CALLS
		// if we have a 'native' packetized function in 'nativeMethods'
		// which corresponds to 'f', generate a call to this function
        // TODO: This is not conservative enough: We can only employ the
        //       native method, if the parameters match. It is possible
        //       that a native method with a uniform parameter is called
        //       with a varying value. In this case, the call can not
        //       be replaced by the native method but has to be split
        //       into W calls to the scalar function (#20).
		Function* vecF = mInfo->mNativeMethods->getNativeFunction(fname,
                                                                  mInfo->mModule,
                                                                  mInfo->mSimdWidth);

		if (vecF) {
			Instruction* newCall = generateNativePacketFunctionCall(oldCall, vecF, mask);
			assert (newCall != oldCall);
			DEBUG_PKT( outs() << "inserted new call: " << *newCall << "\n"; );

#	ifndef PACKETIZER_SILENT_MODE
			outs() << "CALL TO FUNCTION '" << fname << "' WAS VECTORIZED!\n";
#	endif

			return newCall;
		}
#endif

		assert (false && "should never happen!");
		throw std::logic_error("INTERNAL ERROR: native function not found!\n");
	}


	// TODO: We might have to call packetizeConstantOperands() or so...
	CallInst* generateNativePacketFunctionCall(CallInst* oldCall, Function* vecF, Value* mask) {
		assert (oldCall && vecF);
		Function* scalarF = oldCall->getCalledFunction();
		const std::string& fname = scalarF->getNameStr();
		DEBUG_PKT( outs() << "  found packetized function: '" << vecF->getNameStr() << "' (replacing call to '" << fname << "')\n"; );
		const int natFnMaskIndex = mInfo->mNativeMethods->getNativeFunctionMaskIndex(fname);
		DEBUG_PKT( outs() << "  mask-argument index: " << natFnMaskIndex << "\n"; );

		DEBUG_PKT(
			outs() << "  arguments of packetized function:\n";
			for (Function::arg_iterator A=vecF->arg_begin(); A!=vecF->arg_end(); ++A) {
				outs() << "  - " << *A << "\n";
			}
		);

		std::vector<std::pair<Instruction*, Value*> > dummies;

		//attributes cannot directly be copied due to the additional mask parameter
		const AttrListPtr& oldAttrList = oldCall->getAttributes();
		std::vector<Attributes> attrs;
		unsigned attrIdx = 0;
		attrs.push_back(oldAttrList.getRetAttributes());

		//map arguments to packet function,
		//generate dummies with correct types for operands if necessary on the fly
		//NOTE: Here we must not generate dummies for those values that are uniform
		//      parameters to the function!
		std::vector<Value*> args;
		Function::arg_iterator vecA=vecF->arg_begin();
		CallInst::op_iterator oldArg=oldCall->op_begin();

		const int numScalarArgs = oldCall->getNumArgOperands();
		const int numVecArgs = natFnMaskIndex == -1 ? numScalarArgs : numScalarArgs+1;

		for (int argIndex=0; argIndex < numVecArgs; ++argIndex, ++vecA) {
			DEBUG_PKT( outs() << "  arg-index:  " << argIndex << "\n"; );
			DEBUG_PKT( outs() << "  target:  " << *vecA << "\n"; );
			Type* paramType = vecA->getType();

			// if this is the mask parameter index, add the mask to the arguments
			if (argIndex == natFnMaskIndex) {

                // If the mask is UNIFORM, broadcast it, because native functions
                // always expect a vector.
                if (analysisResults->isUniform(mask)) {
                    mask = broadcastValue(mask, oldCall);
                } else {
                    // If the mask is VARYING but not yet packetized, create a dummy.
                    if (!isPacketizedType(mask->getType())) {
                        Instruction* dummy =
                            createDummy(Packetizer::packetizeSIMDType(mask->getType(), *mInfo),
                                        mask,
                                        oldCall);
                        dummies.push_back(std::make_pair(dummy, mask));
                        mask = dummy;
                    }
                }

				DEBUG_PKT( outs() << "    adding mask-argument: " << *mask << "\n"; );
				Value* maskRef = mask;
				Type* maskRefType = maskRef->getType();

				if (maskRefType != paramType) {
					// Type of mask differs from mask parameter of function.
					// This is likely to be unwanted, so we output a warning if
					// if the type sizes do not match
					// (e.g. we allow <2 x i64> == <4 x i32>).
					if (!registerTypeSizeMatches(paramType, maskRefType)) {
						errs() << "WARNING: native function '" << fname
								<< "' requires a mask of type " << *paramType
								<< " at parameter index " << argIndex << ", but"
								<< " available mask has type: " << *maskRefType
								<< "\n";
					}

					if (paramType->isPointerTy() && !maskRefType->isPointerTy()) {
						// The mask is supplied via pointer but is currently a
						// scalar value: allocate memory for the reference.
						maskRef = generateAlignedAlloc(maskRefType, oldCall);
						DEBUG_PKT( outs() << "    generated alloc: " << *maskRef << "\n"; );
						//set pointer to point to correct value
						StoreInst* store = new StoreInst(mask, maskRef, false, mInfo->mAlignmentSIMD, oldCall);
						DEBUG_PKT( outs() << "    generated store: " << *store << "\n"; );
						addFalseValueInfo(store); // TODO: really varying?
					}

					DEBUG_PKT( outs() << "    bitcast from " << *maskRefType
                            << " to " << *paramType << " requested!\n"; );
					BitCastInst* bc = new BitCastInst(maskRef, paramType, "", oldCall);
					args.push_back(bc);
					addFalseValueInfo(bc); // TODO: really varying?
				} else {
					// Types match -> supply mask argument directly.
					args.push_back(maskRef);
				}

				//don't increment 'oldArg' in this case, we must not miss an argument

				attrs.push_back(0); //save empty attribute
				continue;
			}

			// Otherwise, add the correct argument (broadcast/bitcast if necessary).
			DEBUG_PKT( outs() << "    adding argument: " << **oldArg << "\n"; );

			assert (isa<Value>(oldArg));
			Value* argVal = cast<Value>(oldArg);
			Type* argValType = argVal->getType();

            // If the argument is uniform but the native function
            // expects a vector, broadcast it.
            const bool uniformExpected = mInfo->mNativeMethods->isUniformArg(scalarF, argIndex, *mInfo);
            const bool uniformValue    = analysisResults->isUniform(argVal);

            if (uniformExpected) {
                if (uniformValue) {
                    // UNIFORM/UNIFORM -> Do nothing.
                } else {
                    // UNIFORM/VARYING -> Ouch.
                    // The target type is a scalar, but we have a vector,
                    // we assume the vector to be uniform and simply extract the first element
                    // NOTE: this is likely to produce wrong results!
                    errs() << "WARNING: native function '" << vecF->getName()
                            << "' expects UNIFORM argument, but a VARYING value is passed!\n"
                            << "  Assuming implicitly uniform vector, attempting to extract"
                            << " first element of value: " << *argVal << "\n";
                    if (Packetizer::isPacketizableInstructionType(paramType) &&
                            Packetizer::packetizeSIMDType(paramType, *mInfo) == argValType)
                    {
                        // TODO: set alloc position to somewhere else?
                        argVal = generateHorizontalExtract(argVal,
                                                           mInfo->mConstInt32Zero,
                                                           "", oldCall, oldCall);
                    } else {
                        errs() << "ERROR: argument and parameter type of call to native function"
                                << " '" << vecF->getName() << "' do not match: "
                                << *paramType << " != " << *argValType << "\n";
                    }
                }
            } else {
                if (uniformValue) {
                    // VARYING/UNIFORM -> Broadcast.
                    argVal = broadcastValue(argVal, oldCall);
                } else {
                    // VARYING/VARYING -> Do nothing unless the value is
                    // not yet packetized. In this case, create a dummy.
                    if (!isPacketizedType(argValType)) {
                        Instruction* dummy =
                            createDummy(Packetizer::packetizeSIMDType(argValType, *mInfo),
                                        argVal,
                                        oldCall);
                        dummies.push_back(std::make_pair(dummy, argVal));
                        argVal = dummy;
                    }
                }
            }

            // From here on, use the most recent type (possibly changed by dummy)!
            Type* newArgValType = argVal->getType();

            // In case the function requires a pointer, we have to allocate memory,
			// store the value to this pointer and supply it to the function.
			if (paramType->isPointerTy() && !newArgValType->isPointerTy()) {
                assert (Packetizer::typesMatch(paramType, newArgValType->getContainedType(0), *mInfo) &&
                        "parameter type does not match underlying argument pointer type");

				//allocate memory for the reference
				argVal = generateAlignedAlloc(newArgValType, oldCall);
				DEBUG_PKT( outs() << "    generated alloc: " << *argVal << "\n"; );

				//set pointer to point to correct value
				StoreInst* store = new StoreInst(cast<Value>(oldArg),
                                                 argVal,
                                                 false,
                                                 mInfo->mAlignmentSIMD,
                                                 oldCall);

				DEBUG_PKT( outs() << "    generated store: " << *store << "\n"; );
				addFalseValueInfo(store); // TODO: really varying?
			}

//            // If types still do not match, generate correct argument by a bitcast.
//            // TODO: This is really dangerous and should be removed!
//            if (!Packetizer::typesMatch(newArgValType, paramType, *mInfo)) {
//
//                // First, generate dummy of correct type.
//                if (argValType != Type::getVoidTy(*mInfo->mContext) &&
//                        !isa<Function>(argVal) &&
//                        !isPacketizedType(argValType))
//                {
//                    Instruction* dummyI =
//                        createDummy(Packetizer::packetizeSIMDType(argValType, *mInfo), argVal, oldCall);
//                    dummies.push_back(std::make_pair(dummyI, argVal));
//                    Packetizer::uncheckedReplaceAllUsesWith(argVal, dummyI);
//                    argVal = dummyI;
//                }
//
//                DEBUG_PKT( outs() << "    bitcast from " << *newArgRefType
//                        << " to " << *paramType << " requested!\n"; );
//
//                BitCastInst* bcInst = new BitCastInst(argVal, paramType, "", oldCall);
//                args.push_back(bcInst);
//                addFalseValueInfo(bcInst); // TODO: really varying?
//            }

			// Types of parameter and value should match now.
            if (!Packetizer::typesMatch(newArgValType, paramType, *mInfo)) {
                errs() << "ERROR: native function '" << vecF->getName()
                        << "' expects UNIFORM argument, but a VARYING value is passed!\n"
                        << "  Assuming implicitly uniform vector, attempting to extract"
                        << " first element of value: " << *argVal << "\n";
                throw new std::logic_error("Type of argument passed to native function does not match");
            }

            // Store value as argument.
            args.push_back(argVal);
            ++oldArg;

            // save attribute and increment read index
            // (start reading at 1, 0 is return attribute)
            attrs.push_back(oldAttrList.getParamAttributes(++attrIdx));
		}

		// SSE round requires additional unsigned to determine rounding mode
		if (fname == "roundf") args.push_back(mInfo->mConstInt32Zero);
		else if (fname == "floorf") args.push_back(mInfo->mConstInt32One);
        else if (fname == "ceilf") args.push_back(mInfo->mConstInt32Two);

		DEBUG_PKT(
			outs() << "  arguments:\n";
			for (std::vector<Value*>::iterator it=args.begin(); it!=args.end(); ++it) {
				outs() << " * " << **it << "\n";
			}
		);

		std::string fixedName = vecF->getReturnType()->isVoidTy() ? "" : fname;
		CallInst* newCall = CallInst::Create(vecF, ArrayRef<Value*>(args), fixedName, oldCall);
		newCall->setCallingConv(oldCall->getCallingConv());
		//set attributes
		for (unsigned i=0; i<attrs.size(); ++i) {
			newCall->addAttribute(i, attrs[i]);
		}
		// we do not have to store back any values of pointer-arguments
		// because no extraction was performed
		//generateStoreBackForNativeFunction(newCall, oldCall);

		//if we inserted any dummies, be sure to remove them again :P
		for (unsigned i=0; i<dummies.size(); ++i) {
			removeFromInstructionInfo(dummies[i].first);
			Packetizer::uncheckedReplaceAllUsesWith(dummies[i].first, dummies[i].second);
			dummies[i].first->eraseFromParent();
		}

		addValueInfo(newCall, oldCall);

		return newCall;
	}

	// generate an if-cascade at instruction 'I':
	// 1) split parent block of 'I' (I remains in "upper" block = parentBB)
	// 2) generate 'info.simdWidth' blocks that extract the ith mask value,
	//    and jump to an 'execution'-block if the value is true, or
	//    to the next if-block.
	// 3) generate 'info.simdWidth' empty blocks where scalar operations
	//    can be inserted afterwards. These blocks are executed depending on
	//    the guarding ifs.
	//
	// This basically does the following:
	// for (unsigned i=0, e=info.simdWidth; i<e; ++i) {
	//     result[i] = mask[i] ? execute[i] : do_nothing()
	// }
	//
	// do_nothing() results in an undef value, which is blended out due to the masks.
	//
	// 'ifBlocks' is assumed to be an (empty) array of size 'info.simdWidth'+1.
	// 'targetBlocks' is assumed to be an (empty) array of size 'info.simdWidth'.
	//
	// State after execution of this function:
	// - parent block of I is split at the position of I
	// - first if-block is former parent block of 'I' ("upper part")
	// - last if-block is new block containing "lower part" of former parent block of 'I'
	// - each if-block holds mask extraction and scalar comparison if mask-instance is true
	// - each target-block only holds an unconditional branch to the next if-block
	//
	//TODO: add assertions or checks, e.g. to prevent generation for all-true-masks etc.
	void generateIfCascade(Instruction* I, Value* mask, BasicBlock** ifBlocks, BasicBlock** targetBlocks) {
		assert (I && mask && ifBlocks && targetBlocks && I->getParent());
		assert (!analysisResults->isUniform(mask) && "must not attempt to generate if cascade for UNIFORM mask!");
		assert (isPacketizedType(mask->getType()) && "can not generate if cascade for scalar mask!");

		DEBUG_PKT( outs() << "  generating if-cascade...\n"; );

		DEBUG_PKT( outs() << "    creating new blocks... "; );

		// split parent block and move all instructions after I into endBB
		BasicBlock* parentBB = I->getParent();
		BasicBlock* endBB = parentBB->splitBasicBlock(I, parentBB->getNameStr()+".casc.end");
		parentBB->getTerminator()->eraseFromParent(); // newly generated branch is not needed
		Function* parentF = parentBB->getParent();

		// add info on newly created block
		analysisResults->addBlockInfo(endBB,
										   analysisResults->hasUniformEntry(parentBB),
										   analysisResults->hasFullyUniformEntry(parentBB),
										   analysisResults->hasUniformExit(parentBB),
										   analysisResults->hasFullyUniformExit(parentBB));

		// create blocks
		for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
			if (i>0) {
				std::stringstream sstr;
				sstr << "casc.if" << i;
				ifBlocks[i] = BasicBlock::Create(*mInfo->mContext, sstr.str(), parentF, endBB);
				analysisResults->addBlockInfo(ifBlocks[i], true, true, true, true);
			}
			std::stringstream sstr;
			sstr << "casc.exec" << i;
			targetBlocks[i] = BasicBlock::Create(*mInfo->mContext, sstr.str(), parentF, endBB);
			analysisResults->addBlockInfo(targetBlocks[i], true, true, true, true);
		}
		// those are not really if-blocks but this simplifies iteration
		// - iterate until i<simdWidth and use i -> first 4 blocks (includes parent)
		// - iterate until i<simdWidth and use i+1 -> last 4 blocks (includes end)
		ifBlocks[0] = parentBB;
		ifBlocks[mInfo->mSimdWidth] = endBB;

		DEBUG_PKT( outs() << "done.\n    generating unconditional branch statements... "; );

		//generate unconditional jump from each exec-block to next if-block
		for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
			BranchInst* br = BranchInst::Create(ifBlocks[i+1], targetBlocks[i]);
			addFalseValueInfo(br); // TODO: really varying?
		}

		DEBUG_PKT( outs() << "done.\n    generating extract statements of mask values... "; );

		// be sure that mask has correct type before extracting
		if (mask->getType() != mInfo->mVectorTyIntSIMD) {
			DEBUG_PKT( outs() << "done.\n      generating bitcast for mask... "; );
			mask = new BitCastInst(mask, mInfo->mVectorTyIntSIMD, "", parentBB);
			addFalseValueInfo(mask);
		}

		//extract scalar values from entry-mask of exec-block
		Value** masks = new Value*[mInfo->mSimdWidth]();
		for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
			masks[i] = ExtractElementInst::Create(mask, ConstantInt::get(*mInfo->mContext, APInt(32, i)), "", ifBlocks[i]);
			addFalseValueInfo(masks[i]);
		}
		DEBUG_PKT( outs() << "done.\n    generating NEQ 0 comparisons... "; );

		//bitcasting i32 -> i1 is illegal :(
		for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
			masks[i] = CmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_NE, masks[i], mInfo->mConstInt32Zero, "", ifBlocks[i]);
			addFalseValueInfo(masks[i]);
		}
		DEBUG_PKT( outs() << "done.\n    generating conditional branch statements... "; );

		//generate conditional jump from each if-block to next exec-block/next if-block
		for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
			BranchInst* br = BranchInst::Create(targetBlocks[i], ifBlocks[i+1], masks[i], ifBlocks[i]);
			addFalseValueInfo(br);
		}

		DEBUG_PKT( outs() << "done.\n    saving masks of end block... "; );

		// new end block has no mask yet -> assign mask of parentBB (all paths join again)
		MaskGraphNode* parentBBNode = maskGraph->findMaskNode(parentBB);
		assert (parentBBNode && "exec-block has to have associated masks!");

		assert (maskGraph->find(endBB) == maskGraph->end());
		MaskGraphNode* newNode = maskGraph->insert(endBB, mask);
		if (parentBBNode->hasExitEdge()) newNode->setExitMaskTrue(parentBBNode->getExitMaskTrueVal());
		if (parentBBNode->hasConditionalExit()) newNode->setExitMaskFalse(parentBBNode->getExitMaskFalseVal());

		// If this mask already has an associated block, add endBB to the set.
		// Otherwise, create new set.
		std::map<Value*, std::set<BasicBlock*>* >::iterator tmp = entryMasks.find(mask);
		if (tmp == entryMasks.end()) {
			std::set<BasicBlock*>* blockSet = new std::set<BasicBlock*>();
			blockSet->insert(endBB);
			entryMasks.insert(std::make_pair(mask, blockSet));
		} else {
			tmp->second->insert(endBB);
		}

		// Add mask information to mask graph to retain a complete graph.
		// Ignore adding entry mask information for now.
		for (unsigned i=1; i<mInfo->mSimdWidth; ++i) {
			BasicBlock* ifBB = ifBlocks[i];
			assert (maskGraph->find(ifBB) == maskGraph->end());
			// Exit masks are wrong: should be negation of masks[i-1] and masks[i-1]
			maskGraph->insert(ifBB, parentBBNode->getEntryMaskVal(), masks[i-1], masks[i-1]);
			maskGraph->addPredecessor(ifBB, ifBlocks[i-1]);
			maskGraph->addPredecessor(ifBB, targetBlocks[i-1]);
			maskGraph->setSuccessorTrue(ifBB, ifBlocks[i+1]);
			maskGraph->setSuccessorFalse(ifBB, targetBlocks[i]);
		}
		for (unsigned i=0; i<mInfo->mSimdWidth; ++i) {
			BasicBlock* targetBB = targetBlocks[i];
			assert (maskGraph->find(targetBB) == maskGraph->end());
			maskGraph->insert(targetBB, masks[i], masks[i]);
			maskGraph->addPredecessor(targetBB, ifBlocks[i]);
			maskGraph->setSuccessorTrue(targetBB, ifBlocks[i+1]);
		}
		newNode->addPredecessor(maskGraph->findMaskNode(ifBlocks[mInfo->mSimdWidth-1]));
		newNode->addPredecessor(maskGraph->findMaskNode(targetBlocks[mInfo->mSimdWidth-1]));
		if (parentBBNode->hasExitEdge()) newNode->setSuccessorTrue(parentBBNode->getSuccessorTrue());
		if (parentBBNode->hasConditionalExit()) newNode->setSuccessorFalse(parentBBNode->getSuccessorFalse());
		parentBBNode->setExitMaskTrue(masks[0]); // wrong, see above
		parentBBNode->setExitMaskFalse(masks[0]); // wrong, see above
		parentBBNode->setSuccessorTrue(maskGraph->findMaskNode(ifBlocks[1]));
		parentBBNode->setSuccessorFalse(maskGraph->findMaskNode(targetBlocks[0]));

		DEBUG_PKT( outs() << "done.\n  successfully generated if-cascade!\n"; );

		delete [] masks;
	}

	// generate a simple if-statement without else-part at instruction 'I':
	// 1) split parent block of 'I' (I remains in "upper" block = parentBB)
	// 2) generate target block and corresponding branches
	//    the target block is executed if the mask is true at runtime
	//
	// NOTE: If the mask is a vector mask, an "any-mask-index-true"-comparison
	//       is generated. This means that the target block is executed if any
	//       instance would execute it.
	// NOTE: ifBB and targetBB are returned (have to be supplied uninitialized).
	void generateIf(Instruction* I, Value* mask, BasicBlock** ifBB, BasicBlock** targetBB) {
		assert (I && mask);

		DEBUG_PKT( outs() << "  generating simple if-statement...\n"; );
		DEBUG_PKT( outs() << "    creating new blocks... "; );

		// only generate a target-block (if mask is false, nothing is executed)

		// split parent block and move all instructions after I into endBB
		BasicBlock* parentBB = I->getParent();
		BasicBlock* endBB = parentBB->splitBasicBlock(I, parentBB->getNameStr()+".if.merge");
		parentBB->getTerminator()->eraseFromParent(); // newly generated branch is not needed
		Function* parentF = parentBB->getParent();

		// add info on newly created block
		analysisResults->addBlockInfo(endBB,
										   analysisResults->hasUniformEntry(parentBB),
										   analysisResults->hasFullyUniformEntry(parentBB),
										   analysisResults->hasUniformExit(parentBB),
										   analysisResults->hasFullyUniformExit(parentBB));

		// create target-block
		BasicBlock* targetBlock = BasicBlock::Create(*mInfo->mContext, parentBB->getNameStr()+".if.exec", parentF, endBB);
		analysisResults->addBlockInfo(targetBlock, true, true, true, true);

		DEBUG_PKT( outs() << "done.\n    generating unconditional branch statement... "; );
		BranchInst* br = BranchInst::Create(endBB, targetBlock);
		addFalseValueInfo(br);

		if (!analysisResults->isUniform(mask)) {
			DEBUG_PKT( outs() << "done.\n    VARYING mask found - generating "
					<< "'any-mask-index-true'-comparison for conditional branch... "; );

			Instruction* insertBefore = createDummy(mask->getType(), mask, parentBB);

			//insert sse-movemask (could probably be changed to bitcast+x)
			Function* movmskps = mInfo->mUseAVX ? Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_avx_movmsk_ps_256)
				: Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_sse_movmsk_ps);
			std::vector<Value* > params;
			Value* movmskVal = mask;
			if (mask->getType() != mInfo->mVectorTyFloatSIMD) {
				movmskVal = new BitCastInst(mask, mInfo->mVectorTyFloatSIMD, "", insertBefore);
				addFalseValueInfo(movmskVal);
			}
			params.push_back(movmskVal);
			CallInst* movmskI = createExternalIntrinsicCall(movmskps, params, "", insertBefore);
			addFalseValueInfo(movmskI);

			//insert 'allfalse'-comparison (jump to targetBB if movmsk is not 0)
			Instruction* cmp = CmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_NE, movmskI, mInfo->mConstInt32Zero, "cmp0", insertBefore);
			addFalseValueInfo(cmp); // TODO: really varying?

			// remove dummy (only required for placement of instructions)
			removeFromInstructionInfo(insertBefore);
			insertBefore->eraseFromParent();

			mask = cmp;
		}

		DEBUG_PKT( outs() << "done.\n    generating final conditional branch statement... "; );

		//generate conditional jump from parent-block to exec-block/end-block
		BranchInst* br2 = BranchInst::Create(targetBlock, endBB, mask, parentBB);
		addFalseValueInfo(br2);

		DEBUG_PKT( outs() << "done.\n    saving masks of end block... "; );

		// new end block has no mask yet -> assign mask of parentBB (all paths join again)
		MaskGraphNode* parentBBNode = maskGraph->findMaskNode(parentBB);
		assert (parentBBNode && "exec-block has to have associated masks!");

		assert (maskGraph->find(endBB) == maskGraph->end());
		MaskGraphNode* newNode = maskGraph->insert(endBB, mask);
		if (parentBBNode->hasExitEdge()) newNode->setExitMaskTrue(parentBBNode->getExitMaskTrueVal());
		if (parentBBNode->hasConditionalExit()) newNode->setExitMaskFalse(parentBBNode->getExitMaskFalseVal());

		// If this mask already has an associated block, add endBB to the set.
		// Otherwise, create new set.
		std::map<Value*, std::set<BasicBlock*>* >::iterator tmp = entryMasks.find(mask);
		if (tmp == entryMasks.end()) {
			std::set<BasicBlock*>* blockSet = new std::set<BasicBlock*>();
			blockSet->insert(endBB);
			entryMasks.insert(std::make_pair(mask, blockSet));
		} else {
			tmp->second->insert(endBB);
		}

		MaskGraphNode* targetBBNode = maskGraph->insert(targetBlock, mask, mask);
		targetBBNode->addPredecessor(parentBBNode);
		targetBBNode->setSuccessorTrue(newNode);

		newNode->addPredecessor(parentBBNode);
		newNode->addPredecessor(targetBBNode);
		if (parentBBNode->hasExitEdge()) newNode->setSuccessorTrue(parentBBNode->getSuccessorTrue());
		if (parentBBNode->hasConditionalExit()) newNode->setSuccessorFalse(parentBBNode->getSuccessorFalse());
		parentBBNode->setExitMaskTrue(mask);
		parentBBNode->setExitMaskFalse(mask);
		parentBBNode->setSuccessorTrue(newNode);
		parentBBNode->setSuccessorFalse(targetBBNode);

		DEBUG_PKT( outs() << "done.\n  successfully generated simple if!\n"; );

		// store back blocks
		*ifBB = parentBB;
		*targetBB = targetBlock;

		return;
	}

	// generate an if-then-else-statement at instruction 'I':
	// 1) split parent block of 'I' (I remains in "upper" block = parentBB)
	// 2) generate then/else blocks and corresponding branches
	//
	// NOTE: This implementation currently requires a UNIFORM mask for simplicity.
	// NOTE: ifBB, thenBB, and elseBB are returned (have to be supplied uninitialized).
	void generateIfThenElse(Instruction* I,
							Value* condition,
							BasicBlock*& ifBB,
							BasicBlock*& thenBB,
							BasicBlock*& elseBB,
							BasicBlock*& endBB)
	{
		assert (I && condition);

		DEBUG_PKT( outs() << "  generating if-then-else-statement...\n"; );
		DEBUG_PKT( outs() << "    creating new blocks... "; );

		// entry/exit masks do not necessarily depend on condition, so we have
		// to use the one of the parent block (which we determine before splitting).
		// UNIFORM exit -> both exits have the same mask as the entry
		Value* mask = maskGraph->getEntryMask(I->getParent());
		assert (mask);

		// split parent block and move all instructions after I into endBB
		BasicBlock* parentBB = I->getParent();
		BasicBlock* endBlock = parentBB->splitBasicBlock(I, parentBB->getNameStr()+".if.merge");
		parentBB->getTerminator()->eraseFromParent(); // newly generated branch is not needed
		Function* parentF = parentBB->getParent();
		assert (parentF);

		// add info on newly created block
		analysisResults->addBlockInfo(endBlock,
										   analysisResults->hasUniformEntry(parentBB),
										   analysisResults->hasFullyUniformEntry(parentBB),
										   analysisResults->hasUniformExit(parentBB),
										   analysisResults->hasFullyUniformExit(parentBB));

		// create then-block
		BasicBlock* thenBlock = BasicBlock::Create(*mInfo->mContext, parentBB->getNameStr()+".then", parentF, endBlock);
		analysisResults->addBlockInfo(thenBlock, true, true, true, true);

		// create else-block
		BasicBlock* elseBlock = BasicBlock::Create(*mInfo->mContext, parentBB->getNameStr()+".else", parentF, endBlock);
		analysisResults->addBlockInfo(elseBlock, true, true, true, true);

		DEBUG_PKT( outs() << "done.\n    generating conditional branch statement... "; );

		//generate conditional jump from parent-block to then-block/else-block
		BranchInst* br3 = BranchInst::Create(thenBlock, elseBlock, condition, parentBB);
		addFalseValueInfo(br3);

		DEBUG_PKT( outs() << "done.\n    generating unconditional branch statements... "; );
		BranchInst* br = BranchInst::Create(endBlock, thenBlock);
		addFalseValueInfo(br);
		BranchInst* br2 = BranchInst::Create(endBlock, elseBlock);
		addFalseValueInfo(br2);

		DEBUG_PKT( outs() << "done.\n    saving masks of end block... "; );

		// new end block has no mask yet -> assign mask of parentBB (all paths join again)
		MaskGraphNode* parentBBNode = maskGraph->findMaskNode(parentBB);
		assert (parentBBNode && "exec-block has to have associated masks!");

		assert (maskGraph->find(endBlock) == maskGraph->end());
		MaskGraphNode* newNode = maskGraph->insert(endBlock, mask);
		if (parentBBNode->hasExitEdge()) newNode->setExitMaskTrue(parentBBNode->getExitMaskTrueVal());
		if (parentBBNode->hasConditionalExit()) newNode->setExitMaskFalse(parentBBNode->getExitMaskFalseVal());

		// If this mask already has an associated block, add endBB to the set.
		// Otherwise, create new set.
		std::map<Value*, std::set<BasicBlock*>* >::iterator tmp = entryMasks.find(mask);
		if (tmp == entryMasks.end()) {
			std::set<BasicBlock*>* blockSet = new std::set<BasicBlock*>();
			blockSet->insert(endBlock);
			entryMasks.insert(std::make_pair(mask, blockSet));
		} else {
			tmp->second->insert(endBlock);
		}

		MaskGraphNode* thenBBNode = maskGraph->insert(thenBlock, mask, mask);
		thenBBNode->addPredecessor(parentBBNode);
		thenBBNode->setSuccessorTrue(newNode);
		MaskGraphNode* elseBBNode = maskGraph->insert(elseBlock, mask, mask);
		elseBBNode->addPredecessor(parentBBNode);
		elseBBNode->setSuccessorTrue(newNode);

		newNode->addPredecessor(thenBBNode);
		newNode->addPredecessor(elseBBNode);
		if (parentBBNode->hasExitEdge()) newNode->setSuccessorTrue(parentBBNode->getSuccessorTrue());
		if (parentBBNode->hasConditionalExit()) newNode->setSuccessorFalse(parentBBNode->getSuccessorFalse());
		parentBBNode->setExitMaskTrue(mask);
		parentBBNode->setExitMaskFalse(mask);
		parentBBNode->setSuccessorTrue(thenBBNode);
		parentBBNode->setSuccessorFalse(elseBBNode);

		DEBUG_PKT( outs() << "done.\n  successfully generated if-then-else statement!\n"; );

		// store back blocks
		ifBB = parentBB;
		thenBB = thenBlock;
		elseBB = elseBlock;
		endBB = endBlock;

		return;
	}


	Instruction* packetizeICmpInst(ICmpInst* icmp) {
		assert (icmp->getOperand(0)->getType() == mInfo->mVectorTyIntSIMD);
		assert (icmp->getOperand(1)->getType() == mInfo->mVectorTyIntSIMD);

		Value* op0 = icmp->getOperand(0);
		Value* op1 = icmp->getOperand(1);

		Function* cmpps = NULL;
		bool flip = false;
		bool neg = false;
		int predCode = -1;

		switch (icmp->getPredicate()) {
			case ICmpInst::ICMP_EQ:
			{
				cmpps = Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_sse2_pcmpeq_d);
				//predCode = 0;
				//cmpps = Intrinsic::getDeclaration(&info.module, Intrinsic::x86_sse_cmp_ps);
				break;
			}
			case ICmpInst::ICMP_NE:
			{
				neg = true;
				cmpps = Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_sse2_pcmpeq_d);
				//predCode = 4;
				//cmpps = Intrinsic::getDeclaration(&info.module, Intrinsic::x86_sse_cmp_ps);
				break;
			}
			case ICmpInst::ICMP_UGT:
			case ICmpInst::ICMP_SGT:
			{
				cmpps = Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_sse2_pcmpgt_d);
				//predCode = 1;
				//flip = true;
				//cmpps = Intrinsic::getDeclaration(&info.module, Intrinsic::x86_sse_cmp_ps);
				break;
			}
			case ICmpInst::ICMP_UGE:
			case ICmpInst::ICMP_SGE:
			{
				predCode = 2;
				flip = true;
				cmpps = Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_sse_cmp_ps);
				break;
			}
			case ICmpInst::ICMP_ULT:
			case ICmpInst::ICMP_SLT:
			{
				predCode = 1;
				cmpps = Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_sse_cmp_ps);
				break;
			}
			case ICmpInst::ICMP_ULE:
			case ICmpInst::ICMP_SLE:
			{
				flip = true;
				cmpps = Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_sse2_pcmpgt_d);
				//predCode = 2;
				//cmpps = Intrinsic::getDeclaration(&info.module, Intrinsic::x86_sse_cmp_ps);
				break;
			}
			default:
			{
				errs() << "ERROR: Invalid ICmp Predicate found: " << icmp->getPredicate() << "\n";
				return NULL;
			}
		}

		assert (cmpps);

		if (predCode >= 0) {
			// This means we require an fcmp -> also fp values
			BitCastInst* bc0 = new BitCastInst(op0, mInfo->mVectorTyFloatSIMD, "", icmp);
			BitCastInst* bc1 = new BitCastInst(op1, mInfo->mVectorTyFloatSIMD, "", icmp);
			addValueInfo(bc0, op0);
			addValueInfo(bc1, op1);
			op0 = bc0;
			op1 = bc1;
		}

		std::vector<Value* > params;
		if (flip) {
			params.push_back(op1);
			params.push_back(op0);
		} else {
			params.push_back(op0);
			params.push_back(op1);
		}

		if (predCode >= 0) {
			Value* pred = ConstantInt::get(Type::getInt8Ty(*mInfo->mContext), predCode);
			params.push_back(pred);
		}

		Instruction* cmpCall = createExternalIntrinsicCall(cmpps, params, icmp->getNameStr(), icmp);
		addValueInfo(cmpCall, icmp);

		if (predCode >= 0) {
			cmpCall = new BitCastInst(cmpCall, mInfo->mVectorTyIntSIMD, "", icmp);
			addValueInfo(cmpCall, icmp);
		}

		if (neg) {
			cmpCall = BinaryOperator::CreateNot(cmpCall, "", icmp);
			addValueInfo(cmpCall, icmp);
		}

		return cmpCall;
	}


	Instruction* packetizeFCmpInst(FCmpInst* fcmp) {
		assert (fcmp->getOperand(0)->getType() == mInfo->mVectorTyFloatSIMD);
		assert (fcmp->getOperand(1)->getType() == mInfo->mVectorTyFloatSIMD);

		Function *cmpps = mInfo->mUseAVX ? Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_avx_cmp_ps_256)
			: Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_sse_cmp_ps);
		bool flip = false;
		unsigned predCode = 0;

		// TODO: adjust this for AVX (unordered/ordered seem to exist)
		switch (fcmp->getPredicate()) {
			case FCmpInst::FCMP_FALSE:
			{
				//not available
				errs() << "ERROR: FCMP_FALSE not available yet!\n";
				return NULL;
			}
			case FCmpInst::FCMP_ORD:   predCode = 7; break; //IX86_BUILTIN_CMPORDPS
			case FCmpInst::FCMP_UNO:   predCode = 3; break; //IX86_BUILTIN_CMPUNORDPS
			case FCmpInst::FCMP_OEQ:   predCode = 0; break; //IX86_BUILTIN_CMPEQPS
			case FCmpInst::FCMP_UEQ:   predCode = 0; break; //IX86_BUILTIN_CMPEQPS
			case FCmpInst::FCMP_ONE:   predCode = 4; break; //IX86_BUILTIN_CMPNEQPS
			case FCmpInst::FCMP_UNE:   predCode = 4; break; //IX86_BUILTIN_CMPNEQPS
			case FCmpInst::FCMP_OLT:   predCode = 1; break; //IX86_BUILTIN_CMPLTPS
			case FCmpInst::FCMP_ULT:   predCode = 1; break; //IX86_BUILTIN_CMPLTPS
			case FCmpInst::FCMP_OGT:   predCode = 1; flip = true; break; //IX86_BUILTIN_CMPGTPS
			case FCmpInst::FCMP_UGT:   predCode = 1; flip = true; break; //IX86_BUILTIN_CMPGTPS
			case FCmpInst::FCMP_OLE:   predCode = 2; break; //IX86_BUILTIN_CMPLEPS
			case FCmpInst::FCMP_ULE:   predCode = 2; break; //IX86_BUILTIN_CMPLEPS
			case FCmpInst::FCMP_OGE:   predCode = 2; flip = true; break; //IX86_BUILTIN_CMPGEPS
			case FCmpInst::FCMP_UGE:   predCode = 2; flip = true; break; //IX86_BUILTIN_CMPGEPS
			case FCmpInst::FCMP_TRUE:
			{
				//not available
				errs() << "ERROR: FCMP_TRUE not available yet!\n";
				return NULL;
			}
			default:
			{
				errs() << "ERROR: Invalid FCmp Predicate found: " << fcmp->getPredicate() << "\n";
				return NULL;
			}
		}
		Value* pred = ConstantInt::get(Type::getInt8Ty(*mInfo->mContext), predCode);

		std::vector<Value* > params;
		if (flip) {
			params.push_back(fcmp->getOperand(1));
			params.push_back(fcmp->getOperand(0));
		} else {
			params.push_back(fcmp->getOperand(0));
			params.push_back(fcmp->getOperand(1));
		}
		params.push_back(pred);

#ifdef PACKETIZER_USE_SCALAR_MASKS
		// create comparison + movmask
		CallInst* newCmp = createExternalIntrinsicCall(cmpps, params, fcmp->getNameStr(), fcmp);
		addFalseValueInfo(newCmp);
		assert (newCmp->getType() == mInfo->mVectorTyFloatSIMD);
		Function* movmskps = mInfo->mUseAVX ? Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_avx_movmsk_ps_256)
			: Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_sse_movmsk_ps);
		params.clear();
		params.push_back(newCmp);
		CallInst* movmskI = createExternalIntrinsicCall(movmskps, params, "", fcmp);
		addValueInfo(movmskI, fcmp);
		return movmskI;
#else
		CallInst* cmpCall = createExternalIntrinsicCall(cmpps, params, fcmp->getNameStr(), fcmp);
		addValueInfo(cmpCall, fcmp);
		return cmpCall;
#endif
	}

	CmpInst::Predicate convertICmpToFCmpPredicate(CmpInst::Predicate predicate) {
		switch (predicate) {
			case ICmpInst::ICMP_EQ: return FCmpInst::FCMP_OEQ;  /// equal
			case ICmpInst::ICMP_NE: return FCmpInst::FCMP_ONE;  /// not equal
			case ICmpInst::ICMP_UGT: return FCmpInst::FCMP_OGT;  /// unsigned greater than
			case ICmpInst::ICMP_UGE: return FCmpInst::FCMP_OGE;  /// unsigned greater or equal
			case ICmpInst::ICMP_ULT: return FCmpInst::FCMP_OLT;  /// unsigned less than
			case ICmpInst::ICMP_ULE: return FCmpInst::FCMP_OLE;  /// unsigned less or equal
			case ICmpInst::ICMP_SGT: return FCmpInst::FCMP_OGT;  /// signed greater than
			case ICmpInst::ICMP_SGE: return FCmpInst::FCMP_OGE;  /// signed greater or equal
			case ICmpInst::ICMP_SLT: return FCmpInst::FCMP_OLT;  /// signed less than
			case ICmpInst::ICMP_SLE: return FCmpInst::FCMP_OLE;  /// signed less or equal

			default:
			{
				DEBUG_PKT( errs() << "INTERNAL ERROR: Invalid ICmp predicate "
						<< "found: " << predicate << "\n"; );
				assert (false && "ERROR: Invalid ICmp predicate found!");
				throw std::logic_error("INTERNAL ERROR: Invalid ICmp predicate found!");
			}
		}
	}

	//get integer constant of cmp predicate for intrinsic-generation
	//returns an integer constant that corresponds to the correct predicate
	Constant* getPredicateConstant(CmpInst::Predicate p) {
		Constant* c = ConstantInt::get(*mInfo->mContext, APInt(8, p));
		DEBUG_PKT( outs() << "created constant " << *c << " from predicate: " << p << "\n"; );
		return c;
	}

	//creates a call to an externally declared intrinsic f with parameters 'params'
	CallInst* createExternalIntrinsicCall(Function* f, std::vector<Value*> params, std::string name, Instruction* insertBefore) {
		CallInst* callInst = CallInst::Create(f, ArrayRef<Value*>(params), name, insertBefore);
		callInst->setCallingConv(CallingConv::C);
		callInst->setTailCall(true); AttrListPtr ret_PAL;
		{
			SmallVector<AttributeWithIndex, 4> Attrs;
			AttributeWithIndex PAWI;
			//PAWI.Index = 65535; PAWI.Attrs = 0 | Attribute::NoUnwind | Attribute::ReadNone;
			PAWI.Index = 4294967295U; PAWI.Attrs = 0 | Attribute::NoUnwind | Attribute::ReadNone;
			Attrs.push_back(PAWI);
			ret_PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());
		}
		callInst->setAttributes(ret_PAL);
		return callInst;
	}

	bool packetizeBranches(Function* f) {
		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "packetizing conditional branches of function '" << f->getNameStr() << "'...\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );

		bool changed = false;

		for (Function::iterator BB=f->begin(), BBE=f->end(); BB!=BBE; ++BB) {
			assert (BB->getTerminator());
			if (!isa<BranchInst>(BB->getTerminator())) continue;
			BranchInst* brInst = cast<BranchInst>(BB->getTerminator());
			DEBUG_PKT( outs() << "  adjusting block '" << BB->getNameStr() << "' with branch: " << *brInst << "\n"; );

			//ignore this block if its exit-branch is either unconditional or its condition is of scalar type
			if (brInst->isUnconditional()) {
				DEBUG_PKT( outs() << "    needs no adjustment (unconditional branch)\n"; );
				continue;
			}

			Value* cond = brInst->getCondition();

			if (!isPacketizedType(cond->getType())) {
				DEBUG_PKT( outs() << "    needs no adjustment (condition remains scalar)\n"; );
				continue;
			}

			//insert sse-movemask (could probably be changed to bitcast+x)
			Function* movmskps = mInfo->mUseAVX ? Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_avx_movmsk_ps_256)
				: Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_sse_movmsk_ps);
			std::vector<Value* > params;
			Value* movmskVal = cond;
			if (cond->getType() != mInfo->mVectorTyFloatSIMD) {
				movmskVal = new BitCastInst(cond, mInfo->mVectorTyFloatSIMD, "", brInst);
				addFalseValueInfo(movmskVal);
			}
			params.push_back(movmskVal);
			CallInst* movmskI = createExternalIntrinsicCall(movmskps, params, "", brInst);
			addFalseValueInfo(movmskI);

			//insert 'allfalse'-comparison (jump back to header if movmsk is not 0)
			Instruction* exitCmp = CmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_NE, movmskI, mInfo->mConstInt32Zero, "cmp0", brInst);
			addFalseValueInfo(exitCmp); // TODO: really varying?

			brInst->setCondition(exitCmp);
			DEBUG_PKT( outs() << "    generated movmask/allFalse-cmp!\n"; );
			changed = true;
		}

		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "packetization of conditional loop branches finished.\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );
		return changed;
	}


	//removes unnecessary bitcasts
	void removeUnnecessaryCasts(Function* f) {
		DEBUG_PKT( outs() << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; );
		DEBUG_PKT( outs() << "removing unnecessary bitcast instructions... "; );
		unsigned bitcastsRemoved = 0;
		for (Function::iterator BB=f->begin(), BBE=f->end(); BB!=BBE; ++BB) {
			for (BasicBlock::iterator I=BB->begin(), IE=BB->end(); I!=IE; ) {
				if (!isa<BitCastInst>(I)) { ++I; continue; }
				BitCastInst* bcInst = cast<BitCastInst>(I++);
				if (bcInst->getDestTy() == bcInst->getSrcTy()) {
					++bitcastsRemoved;
					removeFromInstructionInfo(bcInst);
					bcInst->replaceAllUsesWith(bcInst->getOperand(0));
					bcInst->eraseFromParent();
				}
			}
		}
		DEBUG_PKT( outs() << "done.\n"; );
		DEBUG_PKT( outs() << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"; );
	}


	//
	// Instruction splitting
	//

	inline bool splitAllInstructions(SplitInfoMapType& splitInfoMap) {
		bool success = true;

		DEBUG_PKT(
			outs() << "\nsplitInfoMap:\n";
			for (SplitInfoMapType::iterator it=splitInfoMap.begin(),
					E=splitInfoMap.end(); it!=E; ++it)
			{
				outs() << " * " << *it->first << "\n";
			}
			outs() << "\n";
		);

#ifdef PACKETIZER_USE_DYNAMIC_CONSECUTIVENESS_CHECKS
		// 4) Insert dynamic consecutiveness checks where possible
		// NOTE: This loop targets only values marked as SPLIT_FULL
		// NOTE: In certain cases, we have to insert a phi for the merged result
		//       of a load, in which case the phi not only has to be marked
		//       like the load, but also has to be inserted into the split-
		//       infoMap. As this cannot be done directly, we create a new map
		//       and merge it into splitInfoMap after this step.
		std::map<Instruction*, Instruction*> newSplitInsts;
		for (SplitInfoMapType::iterator it=splitInfoMap.begin(),
				E=splitInfoMap.end(); it!=E; ++it)
		{
			SplitInfo* splitInfo = it->second;

			if (splitInfo->requiresGuards) continue;
			if (splitInfo->requiresReplication) continue;
			if (splitInfo->requiresResultSplit) continue;

			Value* oldValue = splitInfo->oldValue;
			if (!isa<LoadInst>(oldValue) && !isa<StoreInst>(oldValue)) continue;

			assert (analysisResults->requiresSplitFull(oldValue));
			assert (!analysisResults->requiresSplitFullGuarded(oldValue));

			DEBUG_PKT( outs() << "\n  checking if fully split memory "
					<< "operation can benefit from dynamic check: "
					<< *oldValue << "\n"; );

			assert (isa<Instruction>(oldValue));
			Instruction* valI = cast<Instruction>(oldValue);

			//outs() << "\nbefore dyncheck-split:\n";
			//outs() << *valI->getParent()->getParent();

			BasicBlock* ifBB = NULL;
			BasicBlock* thenBB = NULL;
			BasicBlock* elseBB = NULL;
			BasicBlock* endBB = NULL;
			const bool dynCheckCreated =
				createDynamicAlternativeVectorMemOp(valI, ifBB, thenBB, elseBB, endBB, newSplitInsts);

			//outs() << "\nafter dyncheck-insertion:\n";
			//outs() << *valI->getParent()->getParent();

			if (!dynCheckCreated) continue;

			//Packetizer::insertPrintf("thenBB executed!", ConstantInt::getAllOnesValue(Type::getInt32Ty(info.context)), true, thenBB->getTerminator());
			//Packetizer::insertPrintf("elseBB executed!", ConstantInt::getAllOnesValue(Type::getInt32Ty(info.context)), true, elseBB->getTerminator());

			// now move original split instruction into else-block so that this
			// block is split in the next steps.
			Instruction* splitPos = elseBB->getTerminator();
			valI->moveBefore(splitPos);

#ifndef PACKETIZER_SILENT_MODE
			outs() << "DYNAMIC CONSECUTIVENESS CHECK CREATED!\n";
#endif
		}

		// check possible new split phi instructions and insert into
		// splitInfoMap (mark as "SPLIT_RESULT")
		for (std::map<Instruction*, Instruction*>::iterator it=
				newSplitInsts.begin(), E=newSplitInsts.end(); it!=E; ++it)
		{
			Instruction* oldInst = it->second;
			SplitInfoMapType::iterator tmp = splitInfoMap.find(oldInst);
			assert (tmp != splitInfoMap.end());
			assert (!tmp->second->requiresReplication);
			assert (!tmp->second->requiresResultSplit);
			Instruction* newPhi = it->first;
			SplitInfo* splitInfo = new SplitInfo(newPhi, false, true, false);
			splitInfoMap.insert(std::make_pair(newPhi, splitInfo));
		}
#endif

		// 1) Create 'info.simdWidth' scalar instructions using dummy operands
		//    - do not erase anything, do not replace anything.
		for (SplitInfoMapType::iterator it=splitInfoMap.begin(),
				E=splitInfoMap.end(); it!=E; ++it)
		{
			success &= splitValue(*(it->second), splitInfoMap);
		}

		// 2) Replace dummies with correct (split) operands
		//    - do not yet erase old instructions (might still be in use in
		//      other instructions that are in splitInfoMap
		//    - if result is not used in other split instruction, replace uses
		//      by mergedResult
		for (SplitInfoMapType::iterator it=splitInfoMap.begin(),
				E=splitInfoMap.end(); it!=E; ++it)
		{
			success &= finishValueSplitting(*(it->second), splitInfoMap);
		}

		// 3) If required, create if-cascades and sort newly created, scalar
		//    instructions to the right places.
		// 3b) If the mask is scalar, create simple if?!?!
		// TODO: move before splitting and put all splitting code into branches!
		for (SplitInfoMapType::iterator it=splitInfoMap.begin(),
				E=splitInfoMap.end(); it!=E; ++it)
		{
			SplitInfo* splitInfo = it->second;
			if (!splitInfo->requiresGuards) continue;

#ifdef PACKETIZER_NEVER_CREATE_IF_CASCADES
			continue;
#endif

			Value* oldValue = splitInfo->oldValue;
			DEBUG_PKT( outs() << "\n  instruction requires guards: "
					<< *oldValue << "\n"; );

			assert (isa<Instruction>(oldValue));
			Instruction* valI = cast<Instruction>(oldValue);

			BasicBlock* parentBB = valI->getParent();
			Value* mask = maskGraph->getEntryMask(parentBB);

			// Before block creation, move old value directly behind the split
			// values. This temporarily breaks domination relations, but
			// ultimately puts all instructions to the right places because
			// splitting then ensures that possible merge-operations are in
			// the block *behind* the if.
			Value* lastSplitVal = splitInfo->splitValues[mInfo->mSimdWidth-1];
			assert (isa<Instruction>(lastSplitVal));
			Instruction* lastSplitValI = cast<Instruction>(lastSplitVal);

			valI->moveBefore(lastSplitValI); // move split position above merge code
			lastSplitValI->moveBefore(valI); // swap again

			// Special case if the value is a UNIFORM store or call (which can
			// only happen here if it depends on VARYING control-flow):
			// Splitting means doing an "any-mask-index-true"-comparison
			// followed by a branch to a scalar store.
			// NOTE: !UNIFORM is not enough, !FULLY_UNIFORM is required!
			if (analysisResults->isUniform(valI) || analysisResults->isUniform(mask)) {
				assert ((isa<StoreInst>(valI) || isa<CallInst>(valI)) &&
						!analysisResults->hasFullyUniformEntry(parentBB));

				BasicBlock* ifBB = NULL;
				BasicBlock* targetBB = NULL;
				generateIf(valI, mask, &ifBB, &targetBB);
				DEBUG_PKT( maskGraph->verify(); );

				// Move split values into the target block.
				for (unsigned i=0; i<mInfo->mSimdWidth; ++i) {
					Value* splitVal = splitInfo->splitValues[i];
					assert (isa<Instruction>(splitVal));
					Instruction* splitValI = cast<Instruction>(splitVal);
					splitValI->moveBefore(targetBB->getTerminator());

					// If the the split value has uses, create a phi for it.
					if (!splitValI->use_empty()) {
						Type* scalarType = splitValI->getType();
						assert (!isPacketizedType(scalarType) && "type of split value must be scalar!");

						assert (!valI->getParent()->empty());
						PHINode* phi = PHINode::Create(scalarType, 2U, "", valI->getParent()->getFirstNonPHI());
						addFalseValueInfo(phi);

						splitValI->replaceAllUsesWith(phi); // before adding the use of the phi ;)

						phi->addIncoming(splitValI, targetBB);
						//insert dummy-values where the mask is 0
						phi->addIncoming(UndefValue::get(scalarType), ifBB);
					}
				}

				continue;
			}

#ifdef PACKETIZER_USE_DYNAMIC_CONSECUTIVENESS_CHECKS
			// This would never be executed:
			// a) Loads are never marked as SPLIT_FULL_GUARDED
			// b) Stores are only marked as SPLIT_FULL_GUARDED if control-flow
			//    is VARYING - which also prevents the dynamic optimization
			//createDynamicAlternativeVectorMemOp(valI);
#endif

			// create if-cascade:
			// each if-block holds mask extraction and scalar comparison if mask-instance is true
			// each use-block holds scalar use (either load or store with operand extraction code)
			BasicBlock** ifBlocks = new BasicBlock*[mInfo->mSimdWidth+1]();
			BasicBlock** targetBlocks = new BasicBlock*[mInfo->mSimdWidth]();
			generateIfCascade(valI, mask, ifBlocks, targetBlocks);
			DEBUG_PKT( maskGraph->verify(); );

			// Move split values into the correct target blocks
			std::vector<Value*>& splitValues = splitInfo->splitValues;
			for (unsigned i=0; i<mInfo->mSimdWidth; ++i) {
				Value* splitValue = splitValues[i];
				assert (isa<Instruction>(splitValue));
				Instruction* splitValI = cast<Instruction>(splitValue);
				splitValI->moveBefore(targetBlocks[i]->getTerminator());

				// If the the split value has uses, create a phi for it.
				if (!splitValI->use_empty()) {
					Type* scalarType = splitValI->getType();
					assert (!isPacketizedType(scalarType) && "type of split value must be scalar!");

					assert (!ifBlocks[i+1]->empty());
					PHINode* phi = PHINode::Create(scalarType, 2U, "", ifBlocks[i+1]->getFirstNonPHI());
					addFalseValueInfo(phi);

					splitValI->replaceAllUsesWith(phi); // before adding the use of the phi ;)

					phi->addIncoming(splitValI, targetBlocks[i]);
					//insert dummy-values where the mask is 0
					phi->addIncoming(UndefValue::get(scalarType), ifBlocks[i]);
				}
			}

			DEBUG_PKT( outs() << "done.\n    finished generation of "
					<< "'extract-execute-insert' scheme for split value!\n"; );

			delete [] targetBlocks;
			delete [] ifBlocks;
		}


		// 5) Clean up
		//    - erase old instructions (ignore arguments and constants)
		//    - erase dummy values
		//    - erase data structure
		unsigned lastIterMapSize = splitInfoMap.size()+1;
		while (splitInfoMap.size() < lastIterMapSize) {
			lastIterMapSize = splitInfoMap.size();

			for (SplitInfoMapType::iterator it=splitInfoMap.begin(),
					E=splitInfoMap.end(); it!=E; )
			{
				SplitInfo* splitInfo = it->second;

				Value* oldValue = splitInfo->oldValue;

				if (Instruction* inst = dyn_cast<Instruction>(oldValue)) {
					if (!inst->use_empty()) {
						// Never mind if there are still some instructions with
						// e.g. circular uses - optimizations should get rid
						// of them later anyway.
						++it;
						continue; // do not erase anything
					}

					removeFromInstructionInfo(inst);
					inst->eraseFromParent();
					oldValue = NULL;
				}

				for (std::vector<Instruction*>::iterator it2=splitInfo->dummies.begin(),
						E=splitInfo->dummies.end(); it2!=E; ++it2)
				{
					removeFromInstructionInfo(*it2);
					(*it2)->eraseFromParent();
				}

				delete splitInfo;
				splitInfoMap.erase(it++);
			}

		}


		DEBUG_PKT(
			outs() << "\nValues that were split but still have packet uses (except the split values):\n";
			for (SplitInfoMapType::const_iterator i = splitInfoMap.begin(),
					e = splitInfoMap.end(); i != e; ++i)
			{
				outs() << " * " << *(i->first) << "\n";
				Value* val = i->first;
				unsigned usenr = 0;
				for (Value::use_iterator U=val->use_begin(), UE=val->use_end(); U!=UE; ++U, ++usenr) {
					if (isa<ExtractElementInst>(*U)) continue;
					outs() << "   use " << usenr << ": " << **U << "\n";
				}
			}
			outs() << "\n";
		);

		return success;
	}


	// TODO: cluster directly subsequent uses and generate one cascade for them
	bool splitValue(SplitInfo& splitInfo, SplitInfoMapType& splitInfoMap) {

		Value* oldValue = splitInfo.oldValue;
		Type* scalarType = oldValue->getType();

		DEBUG_PKT( outs() << "\nSplitting value ("
				<< (splitInfo.requiresReplication ? "REPLICATE" :
					splitInfo.requiresResultSplit ? "SPLIT_RESULT" :
						splitInfo.requiresGuards ? "SPLIT_FULL_GUARDED" :
							"SPLIT_FULL") << "): " << *oldValue << "\n"; );

		if (splitInfo.requiresReplication) {
			DEBUG_PKT( outs() << "  only requires replication (is UNIFORM or pointer-argument)!\n"; );
			// NOTE: Arguments can also be bitcasts (=bitcasted arguments)
			//       which are marked by special metadata.
			assert (analysisResults->requiresReplication(oldValue));
			assert (analysisResults->isUniform(oldValue) ||
					(isArgOrArgCast(oldValue) && oldValue->getType()->isPointerTy()));
			assert (!isPacketizedType(scalarType) ||
					(isArgOrArgCast(oldValue) && oldValue->getType()->isPointerTy()));

			for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
				splitInfo.splitValues.push_back(oldValue);
			}

			return true;
		}

		if (isa<Constant>(oldValue)) {
			DEBUG_PKT( outs() << "  is VARYING constant!\n"; );
			assert (analysisResults->requiresSplitResult(oldValue));
			assert (!analysisResults->requiresSplitFull(oldValue));
			return splitPacketConstant(splitInfo, splitInfoMap);
		}

		if (isArgOrArgCast(oldValue)) {
			DEBUG_PKT( outs() << "  is VARYING argument!\n"; );
			assert (analysisResults->requiresSplitResult(oldValue));
			assert (!analysisResults->requiresReplication(oldValue));
			assert (!analysisResults->requiresSplitFull(oldValue));
			assert (!oldValue->getType()->isPointerTy() &&
					"must not attempt to split pointer argument!");
			return splitVaryingArgument(splitInfo, splitInfoMap);
		}

#ifdef PACKETIZER_DISABLE_MEMOP_VECTORIZATION
		if (analysisResults->isConsecutive(oldValue) &&
				isa<Instruction>(oldValue) &&
				!isa<GetElementPtrInst>(oldValue) &&
				splitInfo.requiresResultSplit)
		{
			Instruction* insertBefore = isa<PHINode>(oldValue) ?
				cast<Instruction>(oldValue)->getParent()->getFirstNonPHI() :
				getNextInstructionInList(cast<Instruction>(oldValue));
			assert (insertBefore && "no following instruction found?!");
			Value** splitInsts = new Value*[mInfo->mSimdWidth]();
			for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
				Value* add = NULL;
				if (oldValue->getType()->isIntegerTy()) {
					add = BinaryOperator::Create(Instruction::Add, oldValue, ConstantInt::get(*mInfo->mContext, APInt(32, i)), "", insertBefore);
				} else {
					add = BinaryOperator::Create(Instruction::FAdd, oldValue, ConstantFP::get(Type::getFloatTy(*mInfo->mContext), (double)i), "", insertBefore);
				}
				splitInfo.splitValues.push_back(add);
				splitInsts[i] = add;
				addFalseValueInfo(add);
			}
			//Instruction* mergedRes = generateHorizontalMerge(splitInsts, Packetizer::packetizeSIMDType(scalarType), "mergedConsecSplit", insertBefore);
			//splitInfo.mergedResult = mergedRes;
			delete [] splitInsts;
			return true;
		}
#endif

		assert (isa<Instruction>(oldValue));
		Instruction* oldInst = cast<Instruction>(oldValue);

		if (splitInfo.requiresResultSplit) {
			DEBUG_PKT( outs() << "  only requires splitting of result!\n"; );
			assert (analysisResults->requiresSplitResult(oldInst));
			assert (!analysisResults->requiresReplication(oldInst));
			assert (!analysisResults->requiresSplitFull(oldInst));
			assert (isPacketizedType(scalarType));

			// Insert the scalar instructions directly behind oldInst.
			// This prevents uses from not being dominated if we e.g.
			// insert at the end of the block with the use being in the
			// same block.
			Instruction* insertBefore = isa<PHINode>(oldInst) ?
				oldInst->getParent()->getFirstNonPHI() :
				getNextInstructionInList(oldInst);
			assert (insertBefore && "no following instruction found?!");

			// If the result is a pointer type, split the value as if it was
			// marked SPLIT_FULL, but do not delete the original instruction
			// (which might still have uses that remain vectorized).
			// Splitting of the pointer value is equal to creating one GEP per
			// scalar value which has a vectorized pointer and one index
			// (0 to info.simdWidth-1).
			if (oldInst->getType()->isPointerTy()) {
				Value* pointer = createPointerCast(oldInst, insertBefore);
				assert (isPacketizedType(pointer->getType()));

				std::vector<Value*> indices;
				indices.push_back(mInfo->mConstInt32Zero);

				for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
					indices.push_back(
							ConstantInt::get(*mInfo->mContext, APInt(32, i)));
					GetElementPtrInst* gep =
							GetElementPtrInst::Create(pointer,
													  ArrayRef<Value*>(indices),
													  "",
													  insertBefore);
					addFalseValueInfo(gep);
					splitInfo.splitValues.push_back(gep);
					indices.pop_back();
				}

				return true;
			}

			for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
				Instruction* scalarInst = generateHorizontalExtract(oldInst, ConstantInt::get(*mInfo->mContext, APInt(32, i)), "", insertBefore, insertBefore);
				splitInfo.splitValues.push_back(scalarInst);
			}

			return true;
		}

		assert (analysisResults->requiresSplitFull(oldValue) ||
				analysisResults->requiresSplitFullGuarded(oldValue));
		assert (!analysisResults->requiresReplication(oldValue));
		assert (!analysisResults->requiresSplitResult(oldValue));


		switch (oldInst->getOpcode()) {

			case Instruction::Load:
			{
				LoadInst* oldLoad = cast<LoadInst>(oldInst);
				Value* pointer = oldLoad->getPointerOperand();

#ifndef PACKETIZER_SILENT_MODE
				outs() << "LOAD COULD NOT BE VECTORIZED!\n";
#endif

				// Make sure the pointer operand is also in split set.
				// (Otherwise, we would not have to split the load.)
				assert (splitInfoMap.find(pointer) != splitInfoMap.end() &&
						"load pointer operand has to be in split set!");

				// Create appropriate dummy for pointer operand.
				// This dummy is used in all generated loads and is only replaced
				// after the correct (split) operands are created.
				const unsigned addrSpace =
					cast<PointerType>(pointer->getType())->getAddressSpace();
				Instruction* dummy = createDummy(
						PointerType::get(scalarType, addrSpace),
						pointer,
						oldLoad);
				splitInfo.dummies.push_back(dummy);

				// Create scalar loads
				Value** splitInsts = new Value*[mInfo->mSimdWidth]();
				for (unsigned i=0, e=mInfo->mSimdWidth; i!=e; ++i) {
					LoadInst* newLoad = new LoadInst(dummy,
													 "",
													 oldLoad->isVolatile(),
													 mInfo->mAlignmentScalar,
													 oldLoad);
					splitInfo.splitValues.push_back(newLoad);
					splitInsts[i] = newLoad;
					addFalseValueInfo(newLoad);

					DEBUG_PKT( outs() << "  new load: " << *newLoad << "\n"; );
				}

				// If the loaded value is no pointer, create a merged result value.
				if (!scalarType->isPointerTy()) {
					Instruction* mergedRes = generateHorizontalMerge(splitInsts, Packetizer::packetizeSIMDType(scalarType, *mInfo), "mergedLoad", oldLoad);
					splitInfo.mergedResult = mergedRes;
				}

				delete [] splitInsts;
				break;
			}

			case Instruction::Store:
			{
				StoreInst* oldStore = cast<StoreInst>(oldInst);
				Value* value = oldStore->getValueOperand();
				Value* pointer = oldStore->getPointerOperand();

#ifndef PACKETIZER_SILENT_MODE
				outs() << "STORE COULD NOT BE VECTORIZED!\n";
#endif

				// Here, we ignore the special case of a UNIFORM store in
				// VARYING control-flow. This means we create too many scalar
				// stores (we only need one), but they are all the same, so
				// the final optimizations should get rid of them :).

				// Make sure the pointer operand is also in split set.
				// (Otherwise, we would not have to split the store.)
				assert (splitInfoMap.find(pointer) != splitInfoMap.end() &&
						"store pointer operand has to be in split set!");

				// Find out the correct value type (return type of store is
				// void). This means we have to test whether the type at hand
				// is packetized or not (depends on the operand also being part
				// of the split set (then it is scalar) or not (then it is
				// already packetized).
				Type* valueType = value->getType();
				Type* scalarValueType = isPacketizedType(valueType) ?
					getScalarFromPacketizedType(valueType) : valueType;

				assert ((isPacketizedType(valueType) ||
						analysisResults->isUniform(value) ||
						(splitInfoMap.find(value) != splitInfoMap.end())) &&
						"expecting non-packetized, non-uniform values to be in split info set!");

				// Create appropriate dummies for pointer and value operand.
				// These dummies is used in all generated stores and are only
				// replaced after the correct (split) operands are created.
				const unsigned addrSpace =
					cast<PointerType>(pointer->getType())->getAddressSpace();
				Instruction* dummyV = createDummy(
						scalarValueType,
						value,
						oldStore);
				Instruction* dummyP = createDummy(
						PointerType::get(scalarValueType, addrSpace),
						pointer,
						oldStore);
				splitInfo.dummies.push_back(dummyV);
				splitInfo.dummies.push_back(dummyP);

				// Create scalar stores
				for (unsigned i=0, e=mInfo->mSimdWidth; i!=e; ++i) {
					StoreInst* newStore = new StoreInst(dummyV,
														dummyP,
														oldStore->isVolatile(),
														mInfo->mAlignmentScalar,
														oldStore);
					splitInfo.splitValues.push_back(newStore);
					addFalseValueInfo(newStore);

					DEBUG_PKT( outs() << "  new store: " << *newStore << "\n"; );
				}

				break;
			}

			case Instruction::GetElementPtr:
			{
				GetElementPtrInst* oldGEP = cast<GetElementPtrInst>(oldInst);
				Value* pointer = oldGEP->getPointerOperand();

				// Find out the correct pointer type (return type of gep can be
				// different). This means we have to test whether the type at
				// hand is packetized or not (depends on the operand also being
				// part of the split set (then it is scalar) or not (then it is
				// already packetized).
				// NOTE: The pointer's type should be fixed and not modified -
				//       Only the indices are split and - if necessary - an
				//       additional one is appended, which is the correct way of
				//       "scalarizing" the GEP.

				DEBUG_PKT( outs() << "  pointer: " << *pointer << "\n"; );
				DEBUG_PKT( outs() << "  pointerType: " << *pointer->getType() << "\n"; );

				// Create appropriate dummies for index operands.
				// These dummies are used in all generated geps and are only
				// replaced after the correct (split) operands are created.
				std::vector<Value*> indexDummies;
				for (GetElementPtrInst::op_iterator IDX=oldGEP->idx_begin(),
						IDXE = oldGEP->idx_end(); IDX!=IDXE; ++IDX)
				{
					Value* indexVal = *IDX;

					// Find out the correct index type. This means we have to
					// test whether the type at hand is packetized or not
					// (depends on the operand also being part of the split set
					// (then it is scalar) or not (then it is already packetized).
					Type* valueType = indexVal->getType();
					Type* scalarValueType = isPacketizedType(valueType) ?
						getScalarFromPacketizedType(valueType) : valueType;

					assert ((isPacketizedType(valueType) ||
							analysisResults->isUniform(indexVal) ||
							(splitInfoMap.find(indexVal) != splitInfoMap.end())) &&
							"expecting non-packetized, non-uniform values to be in split info set!");

					Instruction* dummyI = createDummy(scalarValueType,
													  indexVal,
													  oldGEP);
					splitInfo.dummies.push_back(dummyI);
					indexDummies.push_back(dummyI);
				}

				// Create scalar GEPs
				// TODO: VARYING is not enough to determine that this is a
				//       pointer to a packet instead of a vector of pointers.
				//       Testing against SPLIT_FULL also does not help, as a
				//       packet pointer can just as well be split.
				//       Solution?
				const bool isPointerToPacket = !analysisResults->isUniform(pointer);

				//if (isPointerToPacket) {
				//	Type* pointerType = pointer->getType();
				//	Type* pktPointerType = isPacketizedType(pointerType) ?
				//		pointerType : Packetizer::packetizeSIMDType(pointerType);
				//	Instruction* dummyP = createDummy(pktPointerType,
				//									 pointer,
				//									 oldGEP);
				//	pointer = dummyP;
				//}

				GetElementPtrInst** geps = new GetElementPtrInst*[mInfo->mSimdWidth]();
				for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
					// If the GEP points to a packet, we have to add one index
					// to access the correct scalar value of the ith instance.
					if (isPointerToPacket) {
						indexDummies.push_back(ConstantInt::get(*mInfo->mContext, APInt(32, i)));
					}

					//create GEP with scalar values
					geps[i] = GetElementPtrInst::Create(pointer, ArrayRef<Value*>(indexDummies), "", oldGEP);
					splitInfo.splitValues.push_back(geps[i]);
					addFalseValueInfo(geps[i]);

					DEBUG_PKT( outs() << "  new GEP: " << *geps[i] << "\n"; );

					// remove last index dummy again if added (different for each i)
					if (isPointerToPacket) indexDummies.pop_back();
					//if (isPointerToPacket) geps[i]->replaceUsesOfWith(pointer, oldGEP->getPointerOperand());
				}

				delete [] geps;
				break;
			}

			case Instruction::PHI:
			{
				PHINode* oldPhi = cast<PHINode>(oldInst);

				// Create dummies for incoming values.
				// The same dummies are used in all generated phis and are only
				// replaced after the correct (split) values are created.
				std::vector<Instruction*> incValDummies;
				for (unsigned i=0, ei=oldPhi->getNumIncomingValues(); i!=ei; ++i) {
					Value* incVal = oldPhi->getIncomingValue(i);
					BasicBlock* incBB = oldPhi->getIncomingBlock(i);

					assert ((isPacketizedType(scalarType) ||
							analysisResults->isUniform(incVal) ||
							(splitInfoMap.find(incVal) != splitInfoMap.end())) &&
							"expecting non-packetized, non-uniform incoming phi values to be in split info set!");

					Instruction* incValInsertBefore = incBB->getTerminator();
					Instruction* dummyV = createDummy(scalarType,
													  incVal,
													  incValInsertBefore);
					splitInfo.dummies.push_back(dummyV);
					incValDummies.push_back(dummyV);
				}

				// Create scalar phis
				Value** splitInsts = new Value*[mInfo->mSimdWidth]();
				for (unsigned i=0, e=mInfo->mSimdWidth; i!=e; ++i) {
					PHINode* newPhi = PHINode::Create(scalarType,
													  2U,
													  "",
													  oldPhi);

					// add operand dummies
					for (unsigned j=0, ej=oldPhi->getNumIncomingValues(); j!=ej; ++j) {
						BasicBlock* incBB = oldPhi->getIncomingBlock(j);
						newPhi->addIncoming(incValDummies[j], incBB);
					}

					splitInfo.splitValues.push_back(newPhi);
					splitInsts[i] = newPhi;
					addFalseValueInfo(newPhi);

					DEBUG_PKT( outs() << "  new PHI: " << *newPhi << "\n"; );
				}

				// If this is no pointer phi, create a merged result value.
				if (!scalarType->isPointerTy()) {
					Type* packetType = isPacketizedType(scalarType) ?
						scalarType : Packetizer::packetizeSIMDType(scalarType, *mInfo);
					Instruction* mergedRes =
							generateHorizontalMerge(splitInsts,
													packetType,
													"mergedPhi",
													oldPhi->getParent()->getFirstNonPHI());
					splitInfo.mergedResult = mergedRes;
				}

				delete [] splitInsts;
				break;
			}

			case Instruction::Select:
			{
				SelectInst* oldSelect = cast<SelectInst>(oldInst);
				Value* condition = oldSelect->getCondition();
				Value* valueT = oldSelect->getTrueValue();
				Value* valueF = oldSelect->getFalseValue();

				// Make sure the operands are also in split set
				assert (splitInfoMap.find(condition) != splitInfoMap.end() &&
						"select condition has to be in split set!");
				assert (splitInfoMap.find(valueT) != splitInfoMap.end() &&
						"select true value has to be in split set!");
				assert (splitInfoMap.find(valueF) != splitInfoMap.end() &&
						"select false value has to be in split set!");

				assert ((isPacketizedType(condition->getType()) ||
						analysisResults->isUniform(condition) ||
						(splitInfoMap.find(condition) != splitInfoMap.end())) &&
						"expecting non-packetized, non-uniform condition to be in split info set!");

				// Create appropriate dummies for operands.
				// These dummies are used in all generated selects and are only
				// replaced after the correct (split) operands are created.
				// The condition-dummy is slightly more complex: the select
				// requires an i1 instead of an i32, so we insert an i1 dummy
				// (which will later be replaced by an NE-0-comparison of the
				// extracted element of the mask).
				assert (condition->getType()->isIntegerTy() ||
						(condition->getType()->isVectorTy() &&
						 cast<VectorType>(condition->getType())->getContainedType(0)->isIntegerTy()));
				Instruction* dummyC = createDummy(Type::getInt1Ty(*mInfo->mContext), condition, oldSelect);
				Instruction* dummyT = createDummy(scalarType, valueT, oldSelect);
				Instruction* dummyF = createDummy(scalarType, valueF, oldSelect);
				splitInfo.dummies.push_back(dummyC);
				splitInfo.dummies.push_back(dummyT);
				splitInfo.dummies.push_back(dummyF);

				// Create scalar selects
				Value** splitInsts = new Value*[mInfo->mSimdWidth]();
				for (unsigned i=0, e=mInfo->mSimdWidth; i!=e; ++i) {
					SelectInst* newSelect = SelectInst::Create(dummyC,
															   dummyT,
															   dummyF,
															   "",
															   oldSelect);
					splitInfo.splitValues.push_back(newSelect);
					splitInsts[i] = newSelect;
					addFalseValueInfo(newSelect);

					DEBUG_PKT( outs() << "  new select: " << *newSelect << "\n"; );
				}

				// If this is no pointer select, create a merged result value.
				if (!scalarType->isPointerTy()) {
					Instruction* mergedRes =
							generateHorizontalMerge(splitInsts,
													Packetizer::packetizeSIMDType(scalarType, *mInfo),
													"mergedSelect",
													oldSelect);
					splitInfo.mergedResult = mergedRes;
				}

				delete [] splitInsts;
				break;
			}

			case Instruction::Call:
			{
				CallInst* oldCall = cast<CallInst>(oldInst);
				Function* callee = oldCall->getCalledFunction();

#ifndef PACKETIZER_SILENT_MODE
				outs() << "CALL TO FUNCTION '" << callee->getName()
						<< "' COULD NOT BE VECTORIZED!\n";
#endif

				// Create appropriate dummies for arguments.
				// These dummies are used in all generated calls and are only
				// replaced after the correct (split) arguments are created.
				// At the same time, collect old arguments for storeback below.
				std::vector<Value*> args;
				std::vector<Value*> oldCallArgs;
				for (unsigned j=0, je=callee->getFunctionType()->getNumParams();
						j<je; ++j)
				{
					Type* paramType = callee->getFunctionType()->getParamType(j);
					Value* A = oldCall->getArgOperand(j);
					oldCallArgs.push_back(A);

					assert ((isPacketizedType(paramType) ||
							analysisResults->isUniform(A) ||
							(splitInfoMap.find(A) != splitInfoMap.end())) &&
							"expecting non-packetized, non-uniform argument to be in split info set!");

					Instruction* dummyA = createDummy(paramType, A, oldCall);
					args.push_back(dummyA);
					splitInfo.dummies.push_back(dummyA);
				}

				// Create scalar calls
				Value** splitInsts = new Value*[mInfo->mSimdWidth]();
				for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
					//call function with scalar values
					CallInst* newCall = CallInst::Create(callee, ArrayRef<Value*>(args), "", oldCall);
					newCall->setAttributes(oldCall->getAttributes());
					newCall->setCallingConv(oldCall->getCallingConv());
					splitInfo.splitValues.push_back(newCall);
					splitInsts[i] = newCall;
					addFalseValueInfo(newCall);

					DEBUG_PKT( outs() << "  new call: " << *newCall << "\n"; );
				}

				// If the return type is non-void and not a pointer type, create
				// a merged result value.
				Value* result = splitInsts[mInfo->mSimdWidth-1];
				const bool returnsNonVoid = !callee->getReturnType()->isVoidTy();
				if (returnsNonVoid && !scalarType->isPointerTy()) {
					result = generateHorizontalMerge(splitInsts, Packetizer::packetizeSIMDType(scalarType, *mInfo), "", oldCall);
					assert (isa<Instruction>(result));
					splitInfo.mergedResult = cast<Instruction>(result);
				}

				// if the function received any pointer arguments
				// we have to store back the results of the "subtype"-pointers.
				// (the value to be stored in the pointer was also "split" by extraction)
				for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
					generateStoreBackForSplitting(cast<CallInst>(splitInsts[i]),
												  ConstantInt::get(*mInfo->mContext, APInt(32, i)),
												  oldCallArgs,
												  oldCall);
				}

				delete [] splitInsts;
				break;
			}

			default:
			{
				// All other instructions:
				// Split only the result (replication is handled above).
				errs() << "ERROR: Full splitting of instruction type not implemented:" << *oldInst << "\n";
				assert (false && "FULL SPLITTING OF INSTRUCTION TYPE NOT IMPLEMENTED!");
				throw std::logic_error("INTERNAL ERROR: Full splitting of instruction type not implemented!");
			}
		}

		return true;
	}

	inline Instruction* getNextInstructionInList(Instruction* inst) {
		BasicBlock* parentBB = inst->getParent();

		for (BasicBlock::iterator I = parentBB->begin(),
				IE=parentBB->end(); I!=IE; ++I)
		{
			if (inst == I) return ++I;
		}
		return NULL;
	}


	// This is a packetized constant. -> Extract scalar constants.
	bool splitPacketConstant(SplitInfo& splitInfo, SplitInfoMapType& splitInfoMap) {
		//Value* oldValue = splitInfo.oldValue;
		//assert (isa<Constant>(oldValue));
		//assert (!analysisResults->isUniform(oldValue));
		//assert (isPacketizedType(oldValue->getType()));

		assert (false && "NOT IMPLEMENTED");
		return false;
	}

	// This is a VARYING argument. -> Extract scalar values.
	bool splitVaryingArgument(SplitInfo& splitInfo, SplitInfoMapType& splitInfoMap) {
		Value* oldValue = splitInfo.oldValue;
		assert (isArgOrArgCast(oldValue));
		assert (!analysisResults->isUniform(oldValue));

		Instruction* insertBefore;
		if (Argument* arg = dyn_cast<Argument>(oldValue)) {
			insertBefore = arg->getParent()->getEntryBlock().getFirstNonPHI();
		} else {
			assert (isa<BitCastInst>(oldValue));
			assert (cast<Instruction>(oldValue)->getParent() ==
					&cast<Instruction>(oldValue)->getParent()->getParent()->getEntryBlock());
			insertBefore = cast<Instruction>(oldValue);
		}

		for (unsigned i=0, e=mInfo->mSimdWidth; i<e; ++i) {
			Instruction* scalarInst = generateHorizontalExtract(oldValue, ConstantInt::get(*mInfo->mContext, APInt(32, i)), "", insertBefore, insertBefore);
			splitInfo.splitValues.push_back(scalarInst);
		}

		if (BitCastInst* bc = dyn_cast<BitCastInst>(oldValue)) {
			// move bitcast back up above the extracts
			bc->moveBefore(bc->getParent()->getFirstNonPHI());
		}

		return true;
	}


	// NOTE: We assume that every operand that is not itself part of the
	//       splitInfoMap is a uniform value and therefore does not require any
	//       splitting and can be used in all of the newly split, scalar
	//       instructions directly.
	bool finishValueSplitting(SplitInfo& splitInfo, SplitInfoMapType& splitInfoMap) {
		Value* oldValue = splitInfo.oldValue;

		DEBUG_PKT( outs() << "\nFinishing splitting of value ("
				<< (splitInfo.requiresReplication ? "REPLICATE" :
					splitInfo.requiresResultSplit ? "SPLIT_RESULT" :
						splitInfo.requiresGuards ? "SPLIT_FULL_GUARDED" :
							"SPLIT_FULL") << "): " << *oldValue << "\n"; );

		// It is required for all splitting values to provide info.simdWidth
		// scalar values, no matter what kind of splitting is performed.
		assert (splitInfo.splitValues.size() == mInfo->mSimdWidth);

		if (isa<Constant>(oldValue) || isArgOrArgCast(oldValue)) {
			DEBUG_PKT( outs() << "  is constant or argument - nothing to do!\n"; );
			assert (!analysisResults->requiresSplitFull(oldValue));
			return true;
		}

		// This catches e.g. GEPs or loads that are uniform but whose result is
		// used in a split operation. They must not be changed.
		if (splitInfo.requiresReplication) {
			DEBUG_PKT( outs() << "  only required replication - nothing to do!\n"; );
			assert (analysisResults->requiresReplication(oldValue));
			assert (!analysisResults->requiresSplitResult(oldValue));
			assert (!analysisResults->requiresSplitFull(oldValue));
			return true;
		}

		DEBUG_PKT(
			for (unsigned i=0, e=splitInfo.splitValues.size(); i!=e; ++i) {
				outs() << "  split value " << i << ": " << *splitInfo.splitValues[i] << "\n";
				for (Value::use_iterator U=splitInfo.splitValues[i]->use_begin(),
						UE=splitInfo.splitValues[i]->use_end(); U!=UE; ++U)
				{
					assert (isa<Instruction>(*U));
					Instruction* useI = cast<Instruction>(*U);
					outs() << "    use: " << *useI << "\n";
				}
			}
		);

		assert (isa<Instruction>(oldValue));
		Instruction* oldInst = cast<Instruction>(oldValue);

		if (splitInfo.requiresResultSplit) {
			DEBUG_PKT( outs() << "  only required splitting of result - nothing to do!\n"; );
			assert (analysisResults->requiresSplitResult(oldInst));
			assert (!analysisResults->requiresReplication(oldInst));
			assert (!analysisResults->requiresSplitFull(oldInst));
			return true;
		}


		assert (analysisResults->requiresSplitFull(oldValue) ||
				analysisResults->requiresSplitFullGuarded(oldValue));
		assert (!analysisResults->requiresReplication(oldValue));
		assert (!analysisResults->requiresSplitResult(oldValue));



		switch (oldInst->getOpcode()) {

			case Instruction::Load:
			{
				LoadInst* oldLoad = cast<LoadInst>(oldInst);
				Value* pointer = oldLoad->getPointerOperand();

				// pointer has to be in splitInfoMap
				assert (splitInfoMap.find(pointer) != splitInfoMap.end());
				SplitInfo* ptrSplitInfo = splitInfoMap.find(pointer)->second;
				assert (ptrSplitInfo);
				assert (ptrSplitInfo->splitValues.size() == mInfo->mSimdWidth);

				// replace dummy pointer with scalar pointers
				assert (splitInfo.dummies.size() == 1);
				Instruction* dummyPointer = splitInfo.dummies[0];
				for (unsigned i=0, e=mInfo->mSimdWidth; i!=e; ++i) {
					LoadInst* scalarLoad = cast<LoadInst>(splitInfo.splitValues[i]);

					Value* scalarPointer = ptrSplitInfo->splitValues[i];
					scalarLoad->replaceUsesOfWith(dummyPointer, scalarPointer);

					DEBUG_PKT( outs() << "  final split load: " << *scalarLoad << "\n"; );
				}

				assert (oldInst->getType()->isPointerTy() || splitInfo.mergedResult);
				break;
			}

			case Instruction::Store:
			{
				StoreInst* oldStore = cast<StoreInst>(oldInst);
				Value* value = oldStore->getValueOperand();
				Value* pointer = oldStore->getPointerOperand();

				// pointer has to be in splitInfoMap, value not necessarily
				SplitInfoMapType::iterator it = splitInfoMap.find(value);
				SplitInfo* valueSplitInfo = it == splitInfoMap.end() ?
					NULL : it->second;
				assert (!valueSplitInfo || valueSplitInfo->splitValues.size() == mInfo->mSimdWidth);

				assert (splitInfoMap.find(pointer) != splitInfoMap.end());
				SplitInfo* ptrSplitInfo = splitInfoMap.find(pointer)->second;
				assert (ptrSplitInfo);
				assert (ptrSplitInfo->splitValues.size() == mInfo->mSimdWidth);

				// Replace dummy pointer and value with scalar pointers and
				// either values that were also split or W times the same value.
				assert (splitInfo.dummies.size() == 2);
				Instruction* dummyValue = splitInfo.dummies[0];
				Instruction* dummyPointer = splitInfo.dummies[1];

				for (unsigned i=0, e=mInfo->mSimdWidth; i!=e; ++i) {
					StoreInst* scalarStore = cast<StoreInst>(splitInfo.splitValues[i]);

					Value* scalarValue = valueSplitInfo ?
						valueSplitInfo->splitValues[i] : value;
					scalarStore->replaceUsesOfWith(dummyValue, scalarValue);

					Value* scalarPointer = ptrSplitInfo->splitValues[i];
					scalarStore->replaceUsesOfWith(dummyPointer, scalarPointer);

					DEBUG_PKT( outs() << "  final split store: " << *scalarStore << "\n"; );
				}

				// There should be nothing to merge ;)
				assert (!splitInfo.mergedResult);
				break;
			}

			case Instruction::GetElementPtr:
			{
				GetElementPtrInst* oldGEP = cast<GetElementPtrInst>(oldInst);

				// pointer not necessarily has to be in splitInfoMap
				DEBUG_PKT(
					Value* pointer = oldGEP->getPointerOperand();
					SplitInfoMapType::iterator it = splitInfoMap.find(pointer);
					SplitInfo* ptrSplitInfo = it == splitInfoMap.end() ? NULL : it->second;
					assert (!ptrSplitInfo || ptrSplitInfo->splitValues.size() == mInfo->mSimdWidth);
				);

				// Replace dummy indices with scalar indices and either pointers
				// that were also split or W times the same pointer.
				assert (splitInfo.dummies.size() == oldGEP->getNumIndices());

				for (unsigned i=0, e=mInfo->mSimdWidth; i!=e; ++i) {
					GetElementPtrInst* scalarGEP = cast<GetElementPtrInst>(splitInfo.splitValues[i]);

					// replace indices either by scalar indices given by other
					// split instructions or by original index value.
					// NOTE: We ignore the additional last index added to the
					//       split GEP instructions (0/1/2/.../simdWidth-1).
					unsigned j=0;
					for (GetElementPtrInst::op_iterator IDX=oldGEP->idx_begin(),
							IDXE = oldGEP->idx_end(); IDX!=IDXE; ++IDX)
					{
						Value* origIndexVal = *IDX;
						Instruction* dummyIndex = splitInfo.dummies[j++];

						// index not necessarily has to be in splitInfoMap
						SplitInfoMapType::iterator it2 = splitInfoMap.find(origIndexVal);
						SplitInfo* indexSplitInfo = it2 == splitInfoMap.end() ?
							NULL : it2->second;
						assert (!indexSplitInfo || indexSplitInfo->splitValues.size() == mInfo->mSimdWidth);

						Value* scalarIndex = indexSplitInfo ?
							indexSplitInfo->splitValues[i] : origIndexVal;

						scalarGEP->replaceUsesOfWith(dummyIndex, scalarIndex);
					}

					DEBUG_PKT( outs() << "  final split GEP: " << *scalarGEP << "\n"; );
				}

				assert (oldInst->getType()->isPointerTy() || splitInfo.mergedResult);
				break;
			}

			case Instruction::PHI:
			{
				PHINode* oldPhi = cast<PHINode>(oldInst);

				assert (splitInfo.dummies.size() == oldPhi->getNumIncomingValues());

				for (unsigned i=0, e=mInfo->mSimdWidth; i!=e; ++i) {
					PHINode* scalarPhi = cast<PHINode>(splitInfo.splitValues[i]);

					for (unsigned j=0, ej=oldPhi->getNumIncomingValues(); j!=ej; ++j) {
						Value* origIncVal = oldPhi->getIncomingValue(j);
						Value* dummyIncVal = splitInfo.dummies[j];

						// incoming value not necessarily has to be in splitInfoMap
						SplitInfoMapType::iterator it = splitInfoMap.find(origIncVal);
						SplitInfo* incValSplitInfo = it == splitInfoMap.end() ?
							NULL : it->second;
						assert (!incValSplitInfo || incValSplitInfo->splitValues.size() == mInfo->mSimdWidth);

						Value* scalarIncVal = incValSplitInfo ?
							incValSplitInfo->splitValues[i] : origIncVal;

						scalarPhi->replaceUsesOfWith(dummyIncVal, scalarIncVal);
					}

					DEBUG_PKT( outs() << "  final split phi: " << *scalarPhi << "\n"; );
				}

				assert (oldInst->getType()->isPointerTy() || splitInfo.mergedResult);
				break;
			}

			case Instruction::Select:
			{
				SelectInst* oldSelect = cast<SelectInst>(oldInst);
				Value* condition = oldSelect->getCondition();
				Value* valueT = oldSelect->getTrueValue();
				Value* valueF = oldSelect->getFalseValue();

				// None of the operands necessarily has to be in splitInfoMap.
				SplitInfoMapType::iterator it = splitInfoMap.find(condition);
				SplitInfo* conditionSplitInfo = it == splitInfoMap.end() ?
					NULL : it->second;
				it = splitInfoMap.find(valueT);
				SplitInfo* valueTSplitInfo = it == splitInfoMap.end() ?
					NULL : it->second;
				it = splitInfoMap.find(valueF);
				SplitInfo* valueFSplitInfo = it == splitInfoMap.end() ?
					NULL : it->second;
				assert (!conditionSplitInfo || conditionSplitInfo->splitValues.size() == mInfo->mSimdWidth);
				assert (!valueTSplitInfo || valueTSplitInfo->splitValues.size() == mInfo->mSimdWidth);
				assert (!valueFSplitInfo || valueFSplitInfo->splitValues.size() == mInfo->mSimdWidth);

				// Replace dummy values with values that were also split or W
				// times the same value.
				assert (splitInfo.dummies.size() == 3);
				Instruction* dummyCondition = splitInfo.dummies[0];
				Instruction* dummyValueT = splitInfo.dummies[1];
				Instruction* dummyValueF = splitInfo.dummies[2];

				for (unsigned i=0, e=mInfo->mSimdWidth; i!=e; ++i) {
					Value* scalarCondition = conditionSplitInfo ?
						conditionSplitInfo->splitValues[i] : condition;
					Value* scalarValueT = valueTSplitInfo ?
						valueTSplitInfo->splitValues[i] : valueT;
					Value* scalarValueF = valueFSplitInfo ?
						valueFSplitInfo->splitValues[i] : valueF;

					SelectInst* scalarSelect = cast<SelectInst>(splitInfo.splitValues[i]);

					// The condition-dummy is slightly more complex: the select
					// requires an i1 instead of an i32, so we have to replace the
					// dummy by a NE-0-comparison of the extracted element of the
					// condition.
					scalarCondition = CmpInst::Create(Instruction::ICmp,
													  ICmpInst::ICMP_NE,
													  scalarCondition,
													  mInfo->mConstInt32Zero,
													  "",
													  scalarSelect);
					addFalseValueInfo(scalarCondition); // TODO: really varying?

					scalarSelect->replaceUsesOfWith(dummyCondition, scalarCondition);
					scalarSelect->replaceUsesOfWith(dummyValueT, scalarValueT);
					scalarSelect->replaceUsesOfWith(dummyValueF, scalarValueF);

					DEBUG_PKT( outs() << "  final split select: " << *scalarSelect << "\n"; );
				}

				assert (oldInst->getType()->isPointerTy() || splitInfo.mergedResult);
				break;
			}

			case Instruction::Call:
			{
				CallInst* oldCall = cast<CallInst>(oldInst);
				Function* callee = oldCall->getCalledFunction();

				for (unsigned i=0, e=mInfo->mSimdWidth; i!=e; ++i) {
					CallInst* scalarCall = cast<CallInst>(splitInfo.splitValues[i]);

					// replace parameters
					for (unsigned j=0, je=callee->getFunctionType()->getNumParams();
							j<je; ++j)
					{
						Value* A = oldCall->getArgOperand(j);
						Instruction* dummyArg = splitInfo.dummies[j];

						// None of the parameters necessarily has to be in splitInfoMap.
						SplitInfoMapType::iterator it = splitInfoMap.find(A);
						SplitInfo* argSplitInfo = it == splitInfoMap.end() ?
							NULL : it->second;
						assert (!argSplitInfo || argSplitInfo->splitValues.size() == mInfo->mSimdWidth);

						Value* scalarArg = argSplitInfo ?
							argSplitInfo->splitValues[i] : A;

						scalarCall->replaceUsesOfWith(dummyArg, scalarArg);

						DEBUG_PKT( outs() << "  final split call: " << *scalarCall << "\n"; );
					}
				}

				assert (callee->getReturnType()->isVoidTy() || oldInst->getType()->isPointerTy() || splitInfo.mergedResult);
				break;
			}

			default:
			{
				// All other instructions:
				// Result has been split or replicated, depending on uniform
				// info. We do not have to replace any uses explicitly, that is
				// done by other calls to this function.

				//errs() << "ERROR: Splitting of instruction type not implemented:" << *oldInst << "\n";
				//assert (false && "SPLITTING OF INSTRUCTION TYPE NOT IMPLEMENTED!");
				//throw std::logic_error("INTERNAL ERROR: Splitting of instruction type not implemented!");
				//return false;
			}
		}

		// Replace uses of old instruction by merged result if existant.
		// This does only apply to instructions that were really split up, not
		// just replicated or result-split.
		// NOTE: We must not replace uses in instructions that also have to be
		//       fully split. Otherwise the merged result will be incorrectly
		//       used in all the scalar instructions instead of the scalar split
		//       values.
		//       E.g. a split load could be used by a store which also has to
		//       be split. The load's use in the store must not be replaced by
		//       the merged value but by the scalar split values.
		//       At the same time, we must not replace uses that are not marked
		//       as SPLIT_FULL(_GUARDED), because e.g. values whose result is
		//       split still have to operate on the vectorized result instead of
		//       a scalar one.
		//       E.g. an add could use the merged result of a split load while
		//       only its result will be split up and used in a split
		//       instruction.
		if (splitInfo.mergedResult) {
			DEBUG_PKT( outs() << "Replacing VARYING non-SPLIT_FULL uses of old"
					<< "instruction by merged result: "
					<< *splitInfo.mergedResult << "\n"; );

			for (Instruction::use_iterator U=oldInst->use_begin(),
					UE=oldInst->use_end(); U!=UE; )
			{
				assert (isa<Instruction>(*U));
				Instruction* useI = cast<Instruction>(*U++);
				DEBUG_PKT( outs() << "  use before replacing: " << *useI << "\n"; );

				// If use is marked as SPLIT_FULL or SPLIT_FULL_GUARDED,
				// operands are replaced in other call of
				// finishInstructionSplitting.
				SplitInfoMapType::iterator tmp = splitInfoMap.find(useI);
				if (tmp != splitInfoMap.end() &&
						!tmp->second->requiresReplication &&
						!tmp->second->requiresResultSplit)
				{
					DEBUG_PKT( outs() << "    use requires full split - ignored!\n"; );
					continue;
				}

				assert (!useI->getType()->isPointerTy() && splitInfo.mergedResult);

				useI->replaceUsesOfWith(oldInst, splitInfo.mergedResult);
				DEBUG_PKT( outs() << "  use after  replacing: " << *useI << "\n"; );
			}
		}

		return true;
	}


	inline Value* createPointerCast(Value* pointer, Instruction* insertBefore) {
		assert (pointer);

		Type* oldType = pointer->getType();
		if (isPacketizedType(oldType)) return pointer;

		Type* newType = Packetizer::packetizeSIMDType(oldType, *mInfo);

		BitCastInst* pktPtrCast = new BitCastInst(pointer,
												  newType,
												  "pktPtrCast",
												  insertBefore);
		addValueInfo(pktPtrCast, pointer);

		DEBUG_PKT( outs() << "  inserted new pointer cast: "
				<< *pktPtrCast << "\n"; );

		return pktPtrCast;
	}

	inline bool isScalarToVectorPtrCast(const Value* value) {
		if (!isa<BitCastInst>(value)) return false;

		const BitCastInst* bc = cast<BitCastInst>(value);

		if (!value->getType()->isPointerTy()) return false;

		return bc->getDestTy()->getContainedType(0)->isVectorTy() &&
				!bc->getSrcTy()->getContainedType(0)->isVectorTy();
	}

	//
	// dynamic checking stuff
	//

	bool createDynamicAlternativeVectorMemOp(Instruction* oldInst,
											 BasicBlock*& ifBB,
											 BasicBlock*& thenBB,
											 BasicBlock*& elseBB,
											 BasicBlock*& endBB,
											 std::map<Instruction*, Instruction*>& newSplitInsts)
	{
		assert (oldInst);

		Value* pointer = isa<LoadInst>(oldInst) ?
			cast<LoadInst>(oldInst)->getPointerOperand() :
			isa<StoreInst>(oldInst) ?
				cast<StoreInst>(oldInst)->getPointerOperand() :
				NULL;

		// We must *never* create a vector store if there is VARYING
		// control-flow!
		const bool dynamicCheckPrereq = !pointer ? false :
			!isa<StoreInst>(oldInst) ? true :
				analysisResults->hasFullyUniformEntry(oldInst->getParent());

		// Find the index operation that is used to get this pointer
		// (should be a GEP-index)
		Value* pointerIdx = NULL;
		Value* baseAddr = NULL;
		if (dynamicCheckPrereq) {
			bool success = findIndexOperation(pointer, &pointerIdx, &baseAddr);
			if (!success) {
				DEBUG_PKT( outs() << "    could not find index operation!"; );
			}
		}

		if (!pointerIdx) {
			DEBUG_PKT( outs() << "    prerequisites for dynamic consecutiveness"
					<< " check not fulfilled!\n"; );
			return false;
		}

		DEBUG_PKT( outs() << "    inserting dynamic "
				<< "consecutiveness check for load/store... \n"; );

		DEBUG_PKT( outs() << "      pointer: " << *pointer << "\n"; );
		DEBUG_PKT( outs() << "      pointer index: " << *pointerIdx << "\n"; );

		Instruction* insertBefore = oldInst; //inst->getParent()->getTerminator();

		// extract first index from index-vector
		assert (!analysisResults->isUniform(pointerIdx));
#ifndef PACKETIZER_DISABLE_MEMOP_VECTORIZATION
		assert (analysisResults->isRandom(pointerIdx));
#endif
		Instruction* dummy = createDummy(mInfo->mVectorTyIntSIMD, pointerIdx, insertBefore);
		Value* firstIdx = ExtractElementInst::Create(dummy, Constant::getNullValue(Type::getInt32Ty(*mInfo->mContext)), "", insertBefore);
		addFalseValueInfo(firstIdx); // TODO: really varying?

		removeFromInstructionInfo(dummy);
		Packetizer::uncheckedReplaceAllUsesWith(dummy, pointerIdx);
		dummy->eraseFromParent();

		// create new GEP with first index
		Value* newPointer = GetElementPtrInst::Create(baseAddr, ArrayRef<Value*>(firstIdx), "", insertBefore);
		addFalseValueInfo(newPointer);

		Value* isConsec = createDynamicConsecutivenessCheck(pointerIdx, insertBefore);

		generateIfThenElse(oldInst, isConsec, ifBB, thenBB, elseBB, endBB);
		DEBUG_PKT( maskGraph->verify(); );

		// move inst to else-block for splitting
		oldInst->moveBefore(elseBB->getTerminator());

		// create vector load/store in then-block
		Instruction* dummyP = NULL;
		if (StoreInst* tmpStore = dyn_cast<StoreInst>(oldInst)) {
			Value* val = tmpStore->getValueOperand();
			assert (isa<PointerType>(pointer->getType()));
			PointerType* pType = cast<PointerType>(pointer->getType());
			dummyP = createDummy(PointerType::get(val->getType(), pType->getAddressSpace()), pointer, oldInst);
			tmpStore->replaceUsesOfWith(pointer, dummyP);
		}
		Instruction* oldInstClone = oldInst->clone();
		addFalseValueInfo(oldInstClone);
		oldInstClone->insertBefore(thenBB->getTerminator());

		if (dummyP) {
			removeFromInstructionInfo(dummyP);
			oldInst->replaceUsesOfWith(dummyP, pointer);
			oldInstClone->replaceUsesOfWith(dummyP, pointer);
			dummyP->eraseFromParent();
		}

		if (LoadInst* clonedLoad = dyn_cast<LoadInst>(oldInstClone)) {
			// Create pointer cast (redundant if pointer is a pointer to a
			// vector later, but required if e.g. the target array is uniform and
			// the pointer is INDEX_CONSECUTIVE).
			// In any case, the bitcast is required as a dummy to create a vector load.
			Value* pktPtrCast = createPointerCast(newPointer, clonedLoad);

			Instruction* newLoad = new LoadInst(pktPtrCast,
												clonedLoad->getName(),
												clonedLoad->isVolatile(),
												mInfo->mAlignmentSIMD,
												clonedLoad);
			addValueInfo(newLoad, clonedLoad);

			// create phi in merge block
			PHINode* phi = PHINode::Create(newLoad->getType(), 2U, "", endBB->getFirstNonPHI());

			// add new phi to value info map, mark as SPLIT_RESULT
			addValueInfo(phi, oldInst);
			analysisResults->setSplitInfo(phi, AnalysisResults::SPLIT_RESULT);


			// add new phi to map of values that possibly have to be split
			newSplitInsts.insert(std::make_pair(phi, oldInst));

			phi->addIncoming(newLoad, thenBB);
			// We have to use a dummy for the other value (which will be
			// replaced by the merged value later).
			Instruction* dummy2 = createDummy(newLoad->getType(), oldInst, insertBefore);
			phi->addIncoming(dummy2, elseBB);

			// This phi is the future use of the merged loaded value. Thus, we
			// replace all uses of oldInst by the phi. oldInst then only has one
			// use remaining (the phi), which is later replaced by the merged
			// result value of the split-path.
			Packetizer::uncheckedReplaceAllUsesWith(oldInst, phi);

			// Now we can replace & remove the dummy
			removeFromInstructionInfo(dummy2);
			Packetizer::uncheckedReplaceAllUsesWith(dummy2, oldInst);
			dummy2->eraseFromParent();

		} else if (StoreInst* clonedStore = dyn_cast<StoreInst>(oldInstClone)) {
			Value* value = clonedStore->getValueOperand();
			// Create pointer cast (redundant if depending on GEP that also creates
			// this bitcast, but required if e.g. directly loading from the pointer
			// (element 0).
			newPointer = createPointerCast(newPointer, clonedStore);

			// create vectorized dummy-value
			Type* oldValueType = value->getType();
			Instruction* dummy = isPacketizedType(oldValueType) ? NULL :
				createDummy(Packetizer::packetizeSIMDType(oldValueType, *mInfo), value, clonedStore);

			StoreInst* newStore = new StoreInst(dummy ? dummy : value,
												newPointer,
												clonedStore->isVolatile(),
												mInfo->mAlignmentSIMD,
												clonedStore);
			addValueInfo(newStore, clonedStore);

			if (dummy) {
				removeFromInstructionInfo(dummy);
				Packetizer::uncheckedReplaceAllUsesWith(dummy, value);
				dummy->eraseFromParent();
			}
		}

		// remove cloned value again
		removeFromInstructionInfo(oldInstClone);
		oldInstClone->eraseFromParent();

		DEBUG_PKT( outs() << "done.\n    finished generation of "
			<< "dynamic consecutiveness check and branch for load/store!\n"; );

		return true;
	}

	// NOTE: This function currently just stops if the pointer is given by a
	//       phi. Handling this case is much more pain.
	bool findIndexOperation(Value* pointer, Value** pointerIndex, Value** baseAddr) {
		assert (pointer);
		assert (pointer->getType()->isPointerTy());

		DEBUG_PKT( outs() << "    findIndexOperation(" << *pointer << " )\n"; );

		if (!isa<GetElementPtrInst>(pointer)) {
			DEBUG_PKT(outs() << "      is no GEP - ignored!\n"; );
			return false;
		}

		GetElementPtrInst* gep = cast<GetElementPtrInst>(pointer);

		if (gep->getNumIndices() > 1) {
			DEBUG_PKT(outs() << "      GEP has more than one index - ignored!\n"; );
			return false;
		}

		Value* index = cast<Value>(*gep->idx_begin());

#if defined PACKETIZER_DO_NOT_USE_SPLIT_ANALYSIS
		if (isa<Constant>(index)) {
			// If splitting is forced, we can find GEPs with constant indices.
			// Simply ignore them.
			DEBUG_PKT(outs() << "      GEP has constant index - ignored!\n"; );
			return false;
		}
#else
		// Otherwise, this GEP should not appear here - the corresponding
		// load/store should not have been split in the first place.
		assert (!isa<Constant>(index) && "how can that be?");
#endif
		assert (!index->getType()->isPointerTy());
		assert (index->getType()->isIntegerTy() ||
				(index->getType()->isVectorTy() &&
				 index->getType()->getContainedType(0)->isIntegerTy()));

		if (analysisResults->isUniform(index)) {
			DEBUG_PKT(outs() << "      GEP index is UNIFORM - ignored!\n"; );
			return false;
		}

		// If the GEP has both a VARYING pointer and a VARYING index, there is
		// no way we can prevent scalar loads/stores because the access is
		// (provably) fully random.
		if (!analysisResults->isUniform(gep->getPointerOperand())) {
			DEBUG_PKT(outs() << "      GEP has VARYING pointer *and* VARYING "
					<< " index - ignored!\n"; );
			return false;
		}

#ifndef PACKETIZER_DISABLE_MEMOP_VECTORIZATION
		assert (analysisResults->isRandom(index));
#endif

		*pointerIndex = index;
		*baseAddr = gep->getPointerOperand();

		return true;
	}

	// ptest((value << 32) - value, vec(-1, -1, -1, -1) << 32);
	// TODO: at least in AVX, there are several more instructions... VPTEST, VTESTPS, ...
	//       these cover both m256 and m256i.
	Value* createDynamicConsecutivenessCheck(Value* value, Instruction* insertBefore) {
		assert (value && insertBefore);
		assert (mInfo->mUseSSE41 || mInfo->mUseAVX);
		assert (!analysisResults->isUniform(value));

		DEBUG_PKT( outs() << "    creating dynamic consecutiveness check "
				<< "of value: " << *value << "\n"; );

		assert (!mInfo->mUseAVX && "NOT IMPLEMENTED ON AVX!");

		VectorType* vectorTy_longSIMD = VectorType::get(Type::getInt64Ty(*mInfo->mContext), 2);
		Instruction* dummy = createDummy(vectorTy_longSIMD, value, insertBefore);

		Value* bc = new BitCastInst(dummy, vectorTy_longSIMD, "", insertBefore); // PSLLDQ requires <2 x i64>
		addFalseValueInfo(bc);

		removeFromInstructionInfo(dummy);
		Packetizer::uncheckedReplaceAllUsesWith(dummy, value);
		dummy->eraseFromParent();

		ConstantInt* int32Const32 = ConstantInt::get(*mInfo->mContext, APInt(32, 32)); // PSLLDQ takes bytes, but LLVM intrinsic wants bits

		// create shift
		//Value* shift = BinaryOperator::Create(Instruction::Shl, bc, intConst32, "", insertBefore);
		Function* shiftFn = mInfo->mUseAVX ? NULL // there is no byte shift left on ymm-registers in AVX
				: Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_sse2_psll_dq);
		std::vector<Value* > params;
		params.push_back(bc);
		params.push_back(int32Const32);
		Value* shift = createExternalIntrinsicCall(shiftFn, params, "", insertBefore);
		addFalseValueInfo(shift);

		//Packetizer::insertPrintf("\ndynamic check:", value, true, insertBefore);
		//Packetizer::insertPrintf("valuebc:", bc, true, insertBefore);
		//Packetizer::insertPrintf("shift:", shift, true, insertBefore);

#if 1
		// create sub -> also works, but no idea which version is faster
		shift = new BitCastInst(shift, mInfo->mVectorTyIntSIMD, "", insertBefore);
		addFalseValueInfo(shift);
		bc = new BitCastInst(bc, mInfo->mVectorTyIntSIMD, "", insertBefore);
		addFalseValueInfo(bc);
		Value* sub = BinaryOperator::Create(Instruction::Sub, shift, bc, "", insertBefore);
		addFalseValueInfo(sub);
		//Packetizer::insertPrintf("shiftbc:", shift, true, insertBefore);
		//Packetizer::insertPrintf("valuebc:", bc, true, insertBefore);
		//Packetizer::insertPrintf("sub:", sub, true, insertBefore);
#else
		// This is slower (uses floating point subtraction)!
		shift = new BitCastInst(shift, mInfo->mVectorTyIntSIMD, "", insertBefore);
		addFalseValueInfo(shift);
		//Packetizer::insertPrintf("shiftbc:", shift, true, insertBefore);
		bc = new BitCastInst(bc, mInfo->mVectorTyIntSIMD, "", insertBefore);
		addFalseValueInfo(bc);
		Function* cvtdq2psFn = mInfo->mUseAVX ? Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_avx_cvtdq2_ps_256)
				: Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_sse2_cvtdq2ps);
		params.clear();
		params.push_back(shift);
		shift = createExternalIntrinsicCall(cvtdq2psFn, params, "", insertBefore);
		addFalseValueInfo(shift);
		params.clear();
		params.push_back(bc);
		bc = createExternalIntrinsicCall(cvtdq2psFn, params, "", insertBefore);
		addFalseValueInfo(bc);

		// create fsub
		Value* sub = BinaryOperator::Create(Instruction::FSub, shift, bc, "", insertBefore);
		addFalseValueInfo(sub);

		//Packetizer::insertPrintf("shiftcast:", shift, true, insertBefore);
		//Packetizer::insertPrintf("valuecast:", bc, true, insertBefore);
		//Packetizer::insertPrintf("sub:", sub, true, insertBefore);
#endif

		// create constant shift (first operand of ptest)
		//Value* shift2 = BinaryOperator::Create(Instruction::Shl, info.const_vec_SIMD_int32_neg1, intConst32, "", insertBefore);
		std::vector<Constant*> cVec;
		ConstantInt* int64ConstNeg1 = ConstantInt::get(*mInfo->mContext, APInt(64, -1));
		for (unsigned i=0; i<mInfo->mSimdWidth/2; ++i) {
			cVec.push_back(int64ConstNeg1);
		}
		params.clear();
		params.push_back(ConstantVector::get(ArrayRef<Constant*>(cVec)));
		params.push_back(int32Const32);
		CallInst* shift2 = createExternalIntrinsicCall(shiftFn, params, "", insertBefore);
		addFalseValueInfo(shift2);

		// create ptest
		Function* ptestFn = mInfo->mUseAVX ? Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_avx_ptestc_256)
				: Intrinsic::getDeclaration(mInfo->mModule, Intrinsic::x86_sse41_ptestc);
		BitCastInst* bcSub = new BitCastInst(sub, mInfo->mVectorTyFloatSIMD, "", insertBefore); // cast back to <W x float> ...
		BitCastInst* bcShift2 = new BitCastInst(shift2, mInfo->mVectorTyFloatSIMD, "", insertBefore); // cast back to <W x float> ...
		params.clear();
		params.push_back(bcSub);
		params.push_back(bcShift2);
		CallInst* ptest = createExternalIntrinsicCall(ptestFn, params, "test.consec", insertBefore);
		addFalseValueInfo(bcSub);
		addFalseValueInfo(bcShift2);
		addFalseValueInfo(ptest);

		//insert 'allfalse'-comparison (jump to vector load/store if ptest is not 0)
		Instruction* consecCmp = CmpInst::Create(Instruction::ICmp, ICmpInst::ICMP_NE, ptest, mInfo->mConstInt32Zero, "", insertBefore);
		addValueInfo(consecCmp, true, true, false, false, false, false, false, false); // UNIFORM required for generateIfThenElse()

		//Packetizer::insertPrintf("subbc:", bcSub, true, insertBefore);
		//Packetizer::insertPrintf("shift2:", shift2, true, insertBefore);
		//Packetizer::insertPrintf("shift2bc:", bcShift2, true, insertBefore);
		//Packetizer::insertPrintf("ptest:", ptest, true, insertBefore);
		//Packetizer::insertPrintf("cmp:", consecCmp, true, insertBefore);

		return consecCmp;
	}

	inline bool isArgOrArgCast(const Value* value) const {
		assert (value);
		if (isa<Argument>(value)) return true;
		return isa<BitCastInst>(value) &&
				cast<BitCastInst>(value)->getMetadata(Packetizer::WFV_META_ARG_CAST) != NULL;
	}


};

}

char FunctionPacketizer::ID = 0;
INITIALIZE_PASS_BEGIN(FunctionPacketizer, "function packetizer", "function packetizer", false, false)
INITIALIZE_PASS_DEPENDENCY(VectorizationAnalysis)
INITIALIZE_PASS_DEPENDENCY(MaskGenerator)
INITIALIZE_PASS_END(FunctionPacketizer, "function packetizer", "function packetizer", false, false)

// Public interface to the FunctionPacketizer pass
namespace llvm {
	FunctionPass* createFunctionPacketizerPass(const Packetizer::PacketizerInfo* info,
											 Function* sourceFn,
											 Function* targetFn,
											 bool* failed,
											 const bool verbose)
	{
		return new FunctionPacketizer(info, sourceFn, targetFn, failed, verbose);
	}
}

#endif	/* _FUNCTIONPACKETIZER_HPP */

