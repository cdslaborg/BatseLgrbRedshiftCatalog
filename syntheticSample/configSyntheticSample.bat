:: This batch file configures the flags required for building the LGRB world model and the ParaMonte library on Windows-Operating Systems.
:: Prerequisites: Intel Parallel Studio >2018 already installed on the system (which is installed on top of Microsoft Visual Studio >2017).

echo off

cd %~dp0

set ERRORLEVEL=0

:: LGRB world model's SFR model:
set           SYNTHETIC_SAMPLE_PROG=main

:: set Kfactor correction: OneThird, None
set           KFAC_CORRECTION=OneThird

:: Flags controlling which LGRB world model builds to perform (all should be set to either TRUE or FALSE values. Anything other than TRUE is considered FALSE.)
set               ASTRO_OBJ_BUILD_ENABLED=FALSE
set          ASTRO_TEST_OBJ_BUILD_ENABLED=FALSE
set          ASTRO_TEST_EXE_BUILD_ENABLED=FALSE
set                ASTRO_TEST_RUN_ENABLED=FALSE
set    SYNTHETIC_SAMPLE_OBJ_BUILD_ENABLED=TRUE
set    SYNTHETIC_SAMPLE_EXE_BUILD_ENABLED=TRUE
set          SYNTHETIC_SAMPLE_RUN_ENABLED=TRUE



:: Flags controlling which builds to perform (all should be set to either TRUE or FALSE values. Anything other than TRUE is considered FALSE.)
if not defined        PARAMONTE_OBJ_BUILD_ENABLED set       PARAMONTE_OBJ_BUILD_ENABLED=FALSE
if not defined             TEST_OBJ_BUILD_ENABLED set            TEST_OBJ_BUILD_ENABLED=FALSE
if not defined             TEST_EXE_BUILD_ENABLED set            TEST_EXE_BUILD_ENABLED=FALSE
if not defined                   TEST_RUN_ENABLED set                  TEST_RUN_ENABLED=FALSE
if not defined                  DLL_BUILD_ENABLED set                 DLL_BUILD_ENABLED=FALSE
if not defined     GAUSSIAN_EXAMPLE_BUILD_ENABLED set    GAUSSIAN_EXAMPLE_BUILD_ENABLED=FALSE
if not defined       GAUSSIAN_EXAMPLE_RUN_ENABLED set      GAUSSIAN_EXAMPLE_RUN_ENABLED=FALSE
if not defined               FLAG_CLEANUP_ENABLED set              FLAG_CLEANUP_ENABLED=FALSE

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

:: set interoperability mode:
::                      TRUE:   When you are calling ParaMonte library from any language other than Fortran.
::                     FLASE:   When your objective function to be sampled by ParaMonte library is written in Fortran (as opposed to C).
::                              It is also fine to set this flag to TRUE for Fortran application. However, the syntax of the Fortran objective function that
::                              is passed to ParaMonte library will have to conform to the rules of C-Fortran interoperability standard,
::                              as given in the abstract interface in the ParaMonte source file: ParaMonteLogFunc_mod.f90
if not defined         C_INTEROPERABILITY_ENABLED set        C_INTEROPERABILITY_ENABLED=FALSE

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

:: set other parallelization model flags: Currently no other parallelization modes are available.
if not defined                        MPI_ENABLED set                       MPI_ENABLED=FALSE
if not defined                        OMP_ENABLED set                       OMP_ENABLED=FALSE

:: set Fortran/C compiler suite: currently only intel is supported on Windows systems.
if not defined                     COMPILER_SUITE set                    COMPILER_SUITE=intel

:: Intel Fortran compiler/linker debug build flags. Will be used only when compiler is intel and build mode BTYPE is set to debug.
if not defined          INTEL_FORTRAN_DEBUG_FLAGS set         INTEL_FORTRAN_DEBUG_FLAGS=/debug:full /Zi /CB /Od /Qinit:snan /warn:all /gen-interfaces /traceback /check:all /check:bounds /fpe-all:0 /Qdiag-error-limit:10 /Qtrapuv

:: Intel Fortran compiler/linker release build flags. Will be used only when compiler is intel and build mode BTYPE is set to release.
if not defined        INTEL_FORTRAN_RELEASE_FLAGS set       INTEL_FORTRAN_RELEASE_FLAGS=/fast /O3 /Qip /Qipo /Qunroll /Qunroll-aggressive /inline:all /Ob2 /Qparallel /Qinline-dllimport

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

exit /B 0
