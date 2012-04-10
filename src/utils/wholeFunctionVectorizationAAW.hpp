/* 
 * @file   wholeFunctionVectorizationAAW.hpp
 * @date   17.05.2011
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2011 Saarland University
 *
 * This custom assembly annotation writer allows us to print analysis results.
 *
 */

#ifndef WHOLEFUNCTIONVECTORIZATIONAAW_HPP
#define	WHOLEFUNCTIONVECTORIZATIONAAW_HPP

// the following include is only required for single-file compilation
#include "llvm/Value.h" // has to come before AAW.h

#include "llvm/Assembly/AssemblyAnnotationWriter.h"
#include "llvm/Support/FormattedStream.h"

#include "utils/analysisResults.hpp"

namespace {

class WholeFunctionVectorizationAAW : public AssemblyAnnotationWriter {
public:
	WholeFunctionVectorizationAAW(const AnalysisResults& va)
		: analysisResults(va)
	{}

	/// emitFunctionAnnot - This may be implemented to emit a string right before
	/// the start of a function.
	//virtual void emitFunctionAnnot(const Function *F,
								   //formatted_raw_ostream &OS)
	//{}

	/// emitBasicBlockStartAnnot - This may be implemented to emit a string right
	/// after the basic block label, but before the first instruction in the
	/// block.
	virtual void emitBasicBlockStartAnnot(const BasicBlock *BB,
										  formatted_raw_ostream &OS)
	{
		const AnalysisResults::BlockInfo* bi =
			analysisResults.getBlockInfo(BB);
		OS << analysisResults.getBlockInfoString(bi) << "\n";
	}

	/// emitBasicBlockEndAnnot - This may be implemented to emit a string right
	/// after the basic block.
	//virtual void emitBasicBlockEndAnnot(const BasicBlock *BB,
										//formatted_raw_ostream &OS)
	//{}

	/// emitInstructionAnnot - This may be implemented to emit a string right
	/// before an instruction is emitted.
	//virtual void emitInstructionAnnot(const Instruction *I,
									  //formatted_raw_ostream &OS)
	//{}

	/// printInfoComment - This may be implemented to emit a comment to the
	/// right of an instruction or global value.
	virtual void printInfoComment(const llvm::Value &V, formatted_raw_ostream &OS)
	{
		if (isa<GlobalValue>(V)) return;

		const AnalysisResults::ValueInfo* info =
			analysisResults.getValueInfo(&V);

		// print value information
		if (info) {
			// print uniform info
			const AnalysisResults::UniformInfo& ui = info->uniformInfo;
			const std::string& uis = analysisResults.getUniformInfoString(ui);
			OS << "     ; " << uis;

			// print index info
			const AnalysisResults::IndexInfo& ii = info->indexInfo;
			const std::string& iis = analysisResults.getIndexInfoString(ii);
			OS << ", " << iis;

			// print alignment info
			const AnalysisResults::AlignmentInfo& ai = info->alignmentInfo;
			const std::string& ais = analysisResults.getAlignmentInfoString(ai);
			OS << ", " << ais;

			// print split info
			const AnalysisResults::SplitInfo& si = info->splitInfo;
			const std::string& sis = analysisResults.getSplitInfoString(si);
			OS << ", " << sis;

			// print mask info
			if (info->isMask) OS << ", MASK";
		}

		// print type and #uses
		OS << " " << *V.getType() << " [" << V.getNumUses() << "]";
	}

private:
	const AnalysisResults& analysisResults;
};

} // unnamed namespace

#endif	/* WHOLEFUNCTIONVECTORIZATIONAAW_HPP */

