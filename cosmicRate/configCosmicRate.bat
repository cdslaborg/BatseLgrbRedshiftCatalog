:: This batch file configures the flags required for building the LGRB world model and the ParaMonte library on Windows-Operating Systems.
:: Prerequisites: Intel Parallel Studio >2018 already installed on the system (which is installed on top of Microsoft Visual Studio >2017).

echo off
cd %~dp0
set ERRORLEVEL=0

:: change the following hard-coded ParaMonte library names and root path to the desired library names and bin folder of ParaMonte
:: based on the specified value for ParaMonte_LIB_NAME:
::      - the build (release/debug)
::      - the parallelism (mpi/caf/serial)
::      - the library (dynamic/static)
:: types will automatically inferred.

set ParaMonte_LIB_ROOT=D:\Dropbox\Projects\20180101_ParaMonte\git\build\winx64\intel\19.0.4.245\release\static\stack\mpi\Fortran
set ParaMonte_LIB_NAME=libparamonte_static_stack_release_intel_fortran_mpi_windows_x64_mt.lib
REM set ParaMonte_LIB_ROOT=D:\Dropbox\Projects\20180101_ParaMonte\git\build\winx64\intel\19.0.4.245\release\static\stack\serial\Fortran
REM set ParaMonte_LIB_NAME=libparamonte_static_stack_release_intel_fortran_windows_x64_mt.lib

:: input BATSE data file
set BATSE_DATA_FILE_PATH=BATSE_1366_LGRB_P1024ph_Epk_Sch23ph.txt

:: LGRB world model's SFR model: H06, L08, B10
set           RATE_DENSITY_MODEL=B10

:: set Kfactor correction: kfacOneThird, kfacNone
set           KFAC_CORRECTION=kfacOneThird

:: set integration methodology: quadpackDPR, quadpackSPR, romberg
set           INTEGRATION_METHOD=romberg

:: Flags controlling which LGRB world model builds to perform (all should be set to either true or false values. Anything other than true is considered false.)
set             CosmicRate_OBJ_ENABLED=true
set             CosmicRate_EXE_ENABLED=true
set             CosmicRate_RUN_ENABLED=true

if not defined             FOR_COARRAY_NUM_IMAGES set            FOR_COARRAY_NUM_IMAGES=3

:: Intel Fortran compiler/linker debug build flags. Will be used only when compiler is intel and build mode BTYPE is set to debug.
if not defined          INTEL_FORTRAN_DEBUG_FLAGS set         INTEL_FORTRAN_DEBUG_FLAGS=/debug:full /CB /Od /Qinit:snan,arrays /warn:all /gen-interfaces /traceback /check:all /check:bounds /fpe-all:0 /Qdiag-error-limit:10 /Qdiag-disable:5268 /Qdiag-disable:7025 /Qtrapuv

:: Intel Fortran compiler/linker release build flags. Will be used only when compiler is intel and build mode BTYPE is set to release. /Qipo 
REM if not defined        INTEL_FORTRAN_RELEASE_FLAGS set       INTEL_FORTRAN_RELEASE_FLAGS=/fast /O3 /Qip /Qunroll /Qunroll-aggressive /inline:all /Ob2 /Qparallel /Qinline-dllimport
if not defined        INTEL_FORTRAN_RELEASE_FLAGS set       INTEL_FORTRAN_RELEASE_FLAGS=/O3 /Qip /Qipo /Qunroll /Qunroll-aggressive /Qinline-dllimport

:: Intel Fortran compiler/linker testing build flags. Will be used only when compiler is intel and build mode BTYPE is set to testing.
if not defined        INTEL_FORTRAN_TESTING_FLAGS set       INTEL_FORTRAN_TESTING_FLAGS=/Od

exit /B 0
