###
### WFV MAKEFILE ###
###

# NOTE: This makefile assumes $LLVM_INSTALL_DIR to be set in the environment!

# NOTE: If clang++ fails with "fatal error: 'bits/c++config.h' file not found",
#       try including /usr/include/c++/4.5/x86_64-linux-gnu/ or sth similar.
#       This happened on Ubuntu 11.04 64bit (gcc 4.5) as well as Windows 7 32bit (cygwin).
#       At least for Ubuntu this include once solved the issue :).

# COMMAND LINE VARIABLES WITH DEFAULT VALUES

# DEBUG				# enable debug mode (default: 0)
# LLVM_NO_DEBUG		# (windows) compiled with 0 = /MDd, 1 = /MD (default: 0)
# SILENT			# disable all output (default: 0)
# UNIFORM_ANALYSIS	# analyze and optimize UNIFORM paths (only vectorize if required) (default: 1, deactivation experimental)
# BOSCC				# create "branch-on-superword-condition-code" (BOSCC) schemes (default: 1)
# DYNCHECK			# create dynamic consecutiveness checks with optimized load/store operations (default: 0)
# DISABLE_MEM_VEC	# if set, all loads and stores will be split up (scatter/gather) (default: 0, activation experimental)
# FORCE_ALIGNED		# if set, forces vector loads and stores to be aligned (unsafe, will crash if loading/storing from/to unaligned adresses) (default: 0)
# SPLIT_ANALYSIS	# analyze and optimize operations with side-effects (default: 1, deactivation experimental)
# ERROR_ON_RC		# treat detected race conditions as errors (default: 0, activation experimental)

DEBUG=0
LLVM_NO_DEBUG=0
SILENT=0
UNIFORM_ANALYSIS=1
BOSCC=1
DYNCHECK=0
DISABLE_MEM_VEC=0
FORCE_ALIGNED=0
SPLIT_ANALYSIS=1
ERROR_ON_RC=0

# DETERMINE PLATFORM

uname_S := $(shell uname -s 2>/dev/null)
uname_O := $(shell uname -o 2>/dev/null)

ifeq ($(uname_S),Linux)
	TARGET=lib/libWFV.a
	CYGPATH=echo
	LLVMINCLUDEDIR=$(LLVM_INSTALL_DIR)/include
	LLVMLIBDIR=$(LLVM_INSTALL_DIR)/lib
endif
ifeq ($(uname_S),Darwin)
	TARGET=lib/libWFV.a
	CYGPATH=echo
	LLVMINCLUDEDIR=$(LLVM_INSTALL_DIR)/include
	LLVMLIBDIR=$(LLVM_INSTALL_DIR)/lib
endif
ifeq ($(uname_O),Cygwin)
	TARGET=lib/WFV.lib
	CYGPATH=cygpath -m
	LLVMINCLUDEDIR=$(shell $(CYGPATH) $(LLVM_INSTALL_DIR)/include)
	LLVMLIBDIR=$(shell $(CYGPATH) $(LLVM_INSTALL_DIR)/lib)
	LLVMLIBS=LLVMObject.lib LLVMMCJIT.lib LLVMMCDisassembler.lib LLVMLinker.lib LLVMipo.lib LLVMInterpreter.lib LLVMInstrumentation.lib LLVMJIT.lib LLVMExecutionEngine.lib LLVMBitWriter.lib LLVMX86Disassembler.lib LLVMX86AsmParser.lib LLVMX86CodeGen.lib LLVMSelectionDAG.lib LLVMX86AsmPrinter.lib LLVMX86Utils.lib LLVMX86Info.lib LLVMAsmPrinter.lib LLVMMCParser.lib LLVMCodeGen.lib LLVMScalarOpts.lib LLVMInstCombine.lib LLVMTransformUtils.lib LLVMipa.lib LLVMAsmParser.lib LLVMArchive.lib LLVMBitReader.lib LLVMAnalysis.lib LLVMTarget.lib LLVMMC.lib LLVMCore.lib LLVMSupport.lib
endif
ifeq ($(uname_S),MINGW32_NT-6.1)
	TARGET=lib/WFV.lib
	CYGPATH=echo
	LLVMINCLUDEDIR=$(LLVM_INSTALL_DIR)/include
	LLVMLIBDIR=$(LLVM_INSTALL_DIR)/lib
	LLVMLIBS=LLVMObject.lib LLVMMCJIT.lib LLVMMCDisassembler.lib LLVMLinker.lib LLVMipo.lib LLVMInterpreter.lib LLVMInstrumentation.lib LLVMJIT.lib LLVMExecutionEngine.lib LLVMBitWriter.lib LLVMX86Disassembler.lib LLVMX86AsmParser.lib LLVMX86CodeGen.lib LLVMSelectionDAG.lib LLVMX86AsmPrinter.lib LLVMX86Utils.lib LLVMX86Info.lib LLVMAsmPrinter.lib LLVMMCParser.lib LLVMCodeGen.lib LLVMScalarOpts.lib LLVMInstCombine.lib LLVMTransformUtils.lib LLVMipa.lib LLVMAsmParser.lib LLVMArchive.lib LLVMBitReader.lib LLVMAnalysis.lib LLVMTarget.lib LLVMMC.lib LLVMCore.lib LLVMSupport.lib
endif

#
# PLATFORM-INDEPENDENT FLAGS
#

FLAGS=

ifeq ($(SILENT), 1)
	FLAGS+=-DPACKETIZER_SILENT_MODE
endif

ifeq ($(DYNCHECK), 1)
	FLAGS+=-DPACKETIZER_USE_DYNAMIC_CONSECUTIVENESS_CHECKS
endif

ifeq ($(BOSCC), 0)
	FLAGS+=-DPACKETIZER_DO_NOT_USE_COHERENT_MASK_BRANCHING
endif

ifeq ($(FORCE_ALIGNED), 1)
	FLAGS+=-DPACKETIZER_FORCE_ALIGNED_MEMOPS
endif

ifeq ($(DISABLE_MEM_VEC), 1)
	FLAGS+=-DPACKETIZER_DISABLE_MEMOP_VECTORIZATION
endif

ifeq ($(UNIFORM_ANALYSIS), 0)
	FLAGS+=-DPACKETIZER_DO_NOT_USE_UNIFORM_ANALYSIS
endif

ifeq ($(SPLIT_ANALYSIS), 0)
	FLAGS+=-DPACKETIZER_DO_NOT_USE_SPLIT_ANALYSIS
endif

ifeq ($(ERROR_ON_RC), 1)
	FLAGS+=-DPACKETIZER_ERROR_ON_RACE_CONDITION
endif

#
# LIBRARIES
#

all: $(TARGET) lib/wfv/wfv.bc
linux: all wfvTestSuite wfvUnitTests packetizeFunction
win: all wfvTestSuite.exe wfvUnitTests.exe packetizeFunction.exe

lib/wfv/wfv.bc: wfvlibsrc/dummy.ll
	mkdir -p lib/wfv
	llvm-link -o $@ $^

###############
### WINDOWS ###
###############

# Zi = debug
# MDd = link multithreaded,  dynamic, debug runtime library (LLVM debug setting)
# MD  = link multithreaded,  dynamic, runtime library (LLVM release setting)
# ML  = link singlethreaded, static, runtime library (compiler default)
# MT  = link multithreaded,  static, runtime library

WINFLAGS=$(FLAGS)
WINFLAGS+=/EHsc
WINLINKFLAGS =

ifeq ($(DEBUG), 1)
	WINFLAGS+=/Zi /D_DEBUG
	#WINLINKFLAGS+=/DEBUG # results in millions of warnings that no debug info will be included, and it is not valid for static linking ;)
else
	WINFLAGS+=/Ox /Ob2 /Oi /GL /DNDEBUG
	WINLINKFLAGS+=/LTCG
endif

ifeq ($(LLVM_NO_DEBUG), 1)
	WINFLAGS+=/MD
else
	WINFLAGS+=/MDd
endif

# STATIC LIBRARY #

obj/WFV.obj: src/packetizer.cpp
	mkdir -p obj
	cl /nologo /Fo$@ /c $(WINFLAGS) $< /I include /I $(LLVMINCLUDEDIR) /DPACKETIZER_LIB /DPACKETIZER_STATIC_LIBS

lib/WFV.lib: obj/WFV.obj
	mkdir -p lib
	link /lib /nologo $(WINLINKFLAGS) /OUT:$@ $<

# DYNAMIC LIBRARY #

obj/wfv_dynamic.obj: src/packetizer.cpp
	mkdir -p obj
	cl /nologo /Fo$@ /c $(WINFLAGS) $< /I include /I $(LLVMINCLUDEDIR) /DPACKETIZER_LIB

lib/WFV.dll: obj/wfv_dynamic.obj
	echo "WARNING: Compiling packetizer as a dynamic library on Windows might lead to problems when used with another library that links LLVM (no shared LLVM libraries on Windows)."
	mkdir -p lib
	link /nologo /dll $(WINLINKFLAGS) /OUT:$@ /implib:lib/WFV.lib $< /LIBPATH:$(LLVMLIBDIR) $(LLVMLIBS) shell32.lib advapi32.lib


############
### UNIX ###
############

UNIXFLAGS=$(FLAGS)
UNIXFLAGS+=-Wall

ifeq ($(DEBUG), 1)
	UNIXFLAGS+=-g -D_DEBUG
else
	UNIXFLAGS+=-O3 -DNDEBUG
endif

# STATIC LIBRARY #

obj/wfv.o: src/packetizer.cpp
	mkdir -p obj
	$(CXX) -o $@ -c $< -I src -fPIC $(UNIXFLAGS) -I include -I $(LLVMINCLUDEDIR) -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -DPACKETIZER_STATIC_LIBS -DPACKETIZER_LIB

lib/libWFV.a: obj/wfv.o
	mkdir -p lib
	ar rcs $@ $<

# DYNAMIC LIBRARY #

obj/wfv_dynamic.o: src/packetizer.cpp
	mkdir -p obj
	$(CXX) -o $@ -c $< -I src -fPIC $(UNIXFLAGS) -I include -I $(LLVMINCLUDEDIR) -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -DPACKETIZER_LIB

lib/libWFV.so: obj/wfv_dynamic.o
	mkdir -p lib
	ld -shared -o $@ -lc $<


#
# TEST SUITE (requires clang)
# TODO: adjust linking of dynamic/shared lib, PACKETIZER_STATIC_LIBS etc.
#

# WINDOWS
wfvTestSuite.exe: lib/WFV.lib
	cl /nologo tools/wfvTestSuite.cpp $(WINFLAGS) /Isrc /Iinclude /I$(LLVMINCLUDEDIR) /DPACKETIZER_STATIC_LIBS /DPACKETIZER_LIB /link /LIBPATH:$(LLVMLIBDIR) $(LLVMLIBS) shell32.lib advapi32.lib lib/WFV.lib

# UNIX
wfvTestSuite: lib/libWFV.a tools/wfvTestSuite.cpp test/wfvTests.bc test/wfvTests2.bc test/wfvTests3.bc test/wfvTestsAVX.bc
	$(CXX) -o $@ tools/wfvTestSuite.cpp $(UNIXFLAGS) -I src -I include -I $(LLVMINCLUDEDIR) -L$(LLVMLIBDIR) -Llib -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS `$(LLVM_INSTALL_DIR)/bin/llvm-config --ldflags` -lLLVM-3.2svn lib/libWFV.a

test/wfvTests.bc: test/wfvTests.cpp
	$(LLVM_INSTALL_DIR)/bin/clang++ -emit-llvm -c -Wall -msse4.1 -o $@ $<
test/wfvTests2.bc: test/wfvTests2.cpp
	$(LLVM_INSTALL_DIR)/bin/clang++ -emit-llvm -c -Wall -msse4.1 -o $@ $<
test/wfvTests3.bc: test/wfvTests3.cpp
	$(LLVM_INSTALL_DIR)/bin/clang++ -emit-llvm -c -Wall -msse4.1 -o $@ $<
test/wfvTestsAVX.bc: test/wfvTestsAVX.cpp
	$(LLVM_INSTALL_DIR)/bin/clang++ -emit-llvm -c -Wall -mavx -o $@ $<


#
# GOOGLE TEST SUITE
#

# UNIX
lib/libgtest: test/gtest/src/gtest-all.cc
	$(MAKE) -C test/gtest/make
	mkdir -p lib
	ar -rv lib/libgtest.a test/gtest/make/gtest-all.o

unittests:
	$(LLVM_INSTALL_DIR)/bin/llvm-as -o test/unittests/va/test_va_01.bc test/unittests/va/test_va_01.ll
	$(LLVM_INSTALL_DIR)/bin/llvm-as -o test/unittests/api/test_api_signatures.bc test/unittests/api/test_api_signatures.ll
	$(LLVM_INSTALL_DIR)/bin/llvm-as -o test/unittests/api/test_api_badinsts.bc test/unittests/api/test_api_badinsts.ll
	$(LLVM_INSTALL_DIR)/bin/llvm-as -o test/unittests/api/test_api_valid.bc test/unittests/api/test_api_valid.ll

wfvUnitTests: lib/libWFV.a lib/libgtest test/wfvUnitTests.cpp unittests
	$(CXX) -o $@ test/wfvUnitTests.cpp $(UNIXFLAGS) -I src -I include -I $(LLVMINCLUDEDIR) -I test/gtest/include -L$(LLVMLIBDIR) -Llib -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS `$(LLVM_INSTALL_DIR)/bin/llvm-config --ldflags` -lLLVM-3.2svn -lgtest lib/libWFV.a -lpthread

#
# COMMAND-LINE TOOL
# TODO: adjust linking of dynamic/shared lib, PACKETIZER_STATIC_LIBS etc.
#

# WINDOWS
packetizeFunction.exe: lib/WFV.lib tools/packetizeFunction.cpp
	cl /nologo tools/packetizeFunction.cpp $(WINFLAGS) /Iinclude /Isrc /I$(LLVMINCLUDEDIR) /DPACKETIZER_STATIC_LIBS /DPACKETIZER_LIB /link /LIBPATH:$(LLVMLIBDIR) $(LLVMLIBS) shell32.lib advapi32.lib lib/WFV.lib

# UNIX
packetizeFunction: lib/libWFV.a tools/packetizeFunction.cpp
	$(CXX) -o $@ tools/packetizeFunction.cpp $(UNIXFLAGS) -I include -I src -I $(LLVMINCLUDEDIR) -L$(LLVMLIBDIR) -Llib -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS `$(LLVM_INSTALL_DIR)/bin/llvm-config --ldflags` -lLLVM-3.2svn lib/libWFV.a


clean:
	rm -rf obj/
	rm -rf lib/
	rm -rf wfvTestSuite* test/wfvTests*.bc
	rm -rf packetizeFunction*
	rm -rf wfvUnitTests*
	$(MAKE) -C test/gtest/make clean
