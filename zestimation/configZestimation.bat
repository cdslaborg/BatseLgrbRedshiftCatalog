
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::
::   ParaMonte: plain powerful parallel Monte Carlo library.
::
::   Copyright (C) 2012-present, The Computational Data Science Lab
::
::   This file is part of the ParaMonte library.
::
::   ParaMonte is free software: you can redistribute it and/or modify it
::   under the terms of the GNU Lesser General Public License as published
::   by the Free Software Foundation, version 3 of the License.
::
::   ParaMonte is distributed in the hope that it will be useful,
::   but WITHOUT ANY WARRANTY; without even the implied warranty of
::   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
::   GNU Lesser General Public License for more details.
::
::   You should have received a copy of the GNU Lesser General Public License
::   along with the ParaMonte library. If not, see,
::
::       https://github.com/cdslaborg/paramonte/blob/master/LICENSE
::
::   ACKNOWLEDGMENT
::
::   As per the ParaMonte library license agreement terms,
::   if you use any parts of this library for any purposes,
::   we ask you to acknowledge the ParaMonte library's usage
::   in your work (education/research/industry/development/...)
::   by citing the ParaMonte library as described on this page:
::
::       https://github.com/cdslaborg/paramonte/blob/master/ACKNOWLEDGMENT.md
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:: This batch file configures the flags required for building the LGRB world model with the ParaMonte library on Windows-Operating Systems.
:: Prerequisites: Intel Parallel Studio >2018 already installed on the system (which is installed on top of Microsoft Visual Studio >2017).

:: silence cmd output
@echo off
cd %~dp0
set ERRORLEVEL=0

:: LGRB world model's SFR model: H06, L08, B10
if not defined LGRB_RATE_MODEL set LGRB_RATE_MODEL=H06

:: set Kfactor correction: OneThird, None
if not defined KFAC_CORRECTION set KFAC_CORRECTION=OneThird

:: set the relative path to the root directory of the ParaMonte library
set ParaMonte_ROOT_DIR=..\..\..\20180101_ParaMonte\git

:: Flags controlling which LGRB world model builds to perform (all should be set to either true or false values. Anything other than true is considered false.)
set         ZESTIMATION_OBJ_BUILD_ENABLED=true
set         ZESTIMATION_EXE_BUILD_ENABLED=true
set               ZESTIMATION_RUN_ENABLED=true

:: define the ParaMonte interface language (all must be lower-case. Also make sure CFI_ENABLED is set perperly):
::                         c:   Full-blown highly optimized production-level library build
::                   fortran:   Fast compilation, used only for testing purposes during the development. No optimizations performed.
::                    matlab:   Used only for development step. No optimizations performed.
::                    python:   Used only for development step. No optimizations performed.
if not defined                          INTERFACE_LANGUAGE set       INTERFACE_LANGUAGE=fortran

:: define build type:
::                   release:   Full-blown highly optimized production-level library build
::                   testing:   Fast compilation, used only for testing purposes during the development. No optimizations performed.
::                     debug:   Used only for development step. No optimizations performed.
if not defined                              BTYPE set                             BTYPE=release

:: define linking type:
::                   dynamic:   Use this flag when you have R/Python/MATLAB/Julia code to which you need to link the ParaMonte library dynamically, using DLL files. 
::                    static:   Use this flag when you have Fortran/C/C++ code to which you want to link the ParaMonte library statically.
::                              You can also link dynamically your Fortran/C/C++ codes using DLL files by specifying LTYPE=dynamic flag instead.
if not defined                              LTYPE set                             LTYPE=static

:: set Fortran/C array allocation resource.
::                      true:   All automatic and stack arrays will be allocated on the heap. Use this when calling ParaMonte from other languages.
::                     false:   All automatic and stack arrays will be allocated on the stack. This could lead to stack overflow when calling DLL from other languages.
if not defined                 HEAP_ARRAY_ENABLED set                HEAP_ARRAY_ENABLED=true

:: set interoperability mode:
::                      true:   When you are calling ParaMonte library from any language other than Fortran.
::                     false:   When your objective function to be sampled by ParaMonte library is written in Fortran (as opposed to C).
::                              It is also fine to set this flag to true for Fortran application. However, the syntax of the Fortran objective function that
::                              is passed to ParaMonte library will have to conform to the rules of C-Fortran interoperability standard,
::                              as given in the abstract interface in the ParaMonte source file: ParaMonteLogFunc_mod.f90
if not defined                        CFI_ENABLED set                       CFI_ENABLED=false

:: set Coarray Fortran (CAF) parallelization model:
::                      none:   No Coarray Parallelization will be performed. Serial library will be built.   
::                    single:   Coarray Parallelism will be invoked. However, only one image will perform the tasks, and there is no parallel communications.
::                    shared:   Coarray Parallelism will be invoked. It causes the underlying Intel® Message Passing Interface (MPI) parallelization
::                              to run on one node with multiple cores or processors with shared memory.
::               distributed:   This option requires a special license to be installed for Intel compiler suite, and is only available on Linux systems, although you can specify it here.
::                              On the Linux systems, it causes the underlying Intel® MPI Library parallelization to run in a multi-node environment (multiple CPUs with distributed memory).
if not defined                            CAFTYPE set                           CAFTYPE=none

:: set number of Fortran Coarray images (that will run in parallel, if Coarray parallel programming is requested by user)
if not defined             FOR_COARRAY_NUM_IMAGES set            FOR_COARRAY_NUM_IMAGES=3

:: set other parallelization model flags:
if not defined                        MPI_ENABLED set                       MPI_ENABLED=false
if not defined                        OMP_ENABLED set                       OMP_ENABLED=false

:: set Fortran/C compiler suite: currently only intel is supported on Windows systems.
if not defined                     COMPILER_SUITE set                    COMPILER_SUITE=intel

:: set Fortran/C compiler version: compiler version matters when optimizations are enabled as object files will be different across different versions.
:: version is automatically determined at build time. If the build fails due to version detection failure, manually set the compiler version below.
REM if not defined                   COMPILER_VERSION set                  COMPILER_VERSION=19.0.4.245

:: Intel Fortran compiler/linker debug build flags. Will be used only when compiler is intel and build mode BTYPE is set to debug.
if not defined          INTEL_FORTRAN_DEBUG_FLAGS set         INTEL_FORTRAN_DEBUG_FLAGS=/debug:full /Zi /CB /Od /Qinit:snan,arrays /warn:all /gen-interfaces /traceback /check:all /check:bounds /fpe-all:0 /Qdiag-error-limit:10 /Qdiag-disable:5268 /Qdiag-disable:7025 /Qtrapuv

:: Intel Fortran compiler/linker release build flags. Will be used only when compiler is intel and build mode BTYPE is set to release. /Qipo-c generates a single object file, containing all object files.
REM if not defined        INTEL_FORTRAN_RELEASE_FLAGS set       INTEL_FORTRAN_RELEASE_FLAGS=/fast /O3 /Qip /Qipo /Qunroll /Qunroll-aggressive /inline:all /Ob2 /Qparallel /Qinline-dllimport
if not defined        INTEL_FORTRAN_RELEASE_FLAGS set       INTEL_FORTRAN_RELEASE_FLAGS=/O3 /Qip /Qipo /Qunroll /Qunroll-aggressive
REM if not defined        INTEL_FORTRAN_RELEASE_FLAGS set       INTEL_FORTRAN_RELEASE_FLAGS=/Od

:: Intel Fortran compiler/linker testing build flags. Will be used only when compiler is intel and build mode BTYPE is set to testing.
if not defined        INTEL_FORTRAN_TESTING_FLAGS set       INTEL_FORTRAN_TESTING_FLAGS=/Od

:: Intel Fortran compiler preprocessor definitions. Will be used to pass any user-defined macro definition to the intel compiler for preprocessing the files.
:: A macro is denoted by /define:MACRO_NAME=VALUE, the value of which can be dropped, and if so, it will be given a default value of 1.
:: Example: /define:INTEL, will create a macro INTEL with a default value of 1.
if not defined  INTEL_FORTRAN_PREPROCESSOR_MACROS set INTEL_FORTRAN_PREPROCESSOR_MACROS=

:: Intel C++ compiler/linker debug build flags. Will be used only when compiler is intel and build mode BTYPE is set to debug.
if not defined              INTEL_CPP_DEBUG_FLAGS set             INTEL_CPP_DEBUG_FLAGS=/debug:full /Zi /Od /Wall /traceback /Qcheck-pointers:rw /Qcheck-pointers-undimensioned /Qdiag-error-limit:10 /Qtrapuv

:: Intel Fortran compiler/linker release build flags. Will be used only when compiler is intel and build mode BTYPE is set to release.
if not defined            INTEL_CPP_RELEASE_FLAGS set           INTEL_CPP_RELEASE_FLAGS=/O3 /Qip /Qipo /Qunroll /Qunroll-aggressive /Ob2 /Qparallel /Qinline-dllimport

:: Intel C++ compiler/linker testing build flags. Will be used only when compiler is intel and build mode BTYPE is set to testing.
if not defined            INTEL_CPP_TESTING_FLAGS set           INTEL_CPP_TESTING_FLAGS=/Od

:: set path to the Python interpreter. Will be used only when attempting to run Python tests and examples on windows.
if not defined                        Python_PATH set                       Python_PATH=C:\ProgramData\Anaconda3\python.exe

exit /B 0
