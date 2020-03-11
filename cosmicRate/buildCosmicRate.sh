#!/bin/sh
#**********************************************************************************************************************************
#**********************************************************************************************************************************
#
#  ParaMonte: plain powerful parallel Monte Carlo library.
#
#  Copyright (C) 2012-present, The Computational Data Science Lab
#
#  This file is part of ParaMonte library. 
#
#  ParaMonte is free software: you can redistribute it and/or modify
#  it under the terms of the GNU Lesser General Public License as published by
#  the Free Software Foundation, version 3 of the License.
#
#  ParaMonte is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public License
#  along with ParaMonte.  If not, see <https://www.gnu.org/licenses/>.
#
#**********************************************************************************************************************************
#**********************************************************************************************************************************

# example usage:
#   ./buildCosmicRate.sh -r H06 -p /work/01902/ashahmor/stampede2/git/paramonte/build/linuxx64/intel/18.0.2.199/release/static/stack/serial/Fortran/
# This bash script file configures the flags required for building the LGRB world model on non-Windows Operating-Systems.
# Prerequisites: Intel Parallel Studio >2018 already installed on the system or GNU >7.0.0.

BUILD_NAME="CosmicRate"; export BUILD_NAME

FILE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

if [[ ! -f "$(pwd)/build${BUILD_NAME}.sh" ]]; then
  echo >&2
  echo >&2 "-- ${BUILD_NAME} - FATAL: build failed."
  echo >&2 "-- ${BUILD_NAME} - FATAL: Please run this script inside the top-level ${BUILD_NAME} root directory."
  echo >&2
  exit 1
fi

####################################################################################################################################
# get platform / architecture
####################################################################################################################################

UNAME_PLATFORM="$(uname -s)"
case "${UNAME_PLATFORM}" in
    Linux*)     PLATFORM=linux;;
    Darwin*)    PLATFORM=mac;;
    CYGWIN*)    PLATFORM=cygwin;;
    MINGW*)     PLATFORM=mingw;;
    *)          PLATFORM="unknown:${UNAME_PLATFORM}"
esac
if [[ "$PLATFORM" =~ .*"unknown".* ]]; then
    echo >&2
    echo >&2 "-- ${BUILD_NAME} - FATAL: build failed. unrecognized platform - ${PLATFORM}"
    echo >&2 "-- ${BUILD_NAME} - supported platforms include: Linux, Darwin, CYGWIN, MINGW"
    echo >&2 "-- ${BUILD_NAME} - This build script has been only tested on Linux and Darwin platforms."
    echo >&2
    echo >&2 "-- ${BUILD_NAME} - gracefully exiting."
    echo >&2
else
    export PLATFORM
fi

ARCHITECTURE=$(uname -p)
if [[ "$ARCHITECTURE" =~ .*"64".* ]]; then
    ARCHITECTURE="x64"
else
    ARCHITECTURE=$(uname -m)
    if [[ "$ARCHITECTURE" =~ .*"64".* ]]; then ARCHITECTURE="x64"; fi
fi
export ARCHITECTURE

echo >&2 
echo >&2 "::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::"
echo >&2 "::::                                                                                                                            ::::"
echo >&2 "                                                 ${BUILD_NAME} build on ${PLATFORM}                                                 "
echo >&2 "::::                                                                                                                            ::::"
echo >&2 "::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::"
echo >&2

echo >&2
echo >&2 "-- ${BUILD_NAME} - current directory: ${FILE_DIR}"
echo >&2 "-- ${BUILD_NAME} - current system's platform: ${PLATFORM}"
echo >&2 "-- ${BUILD_NAME} - current system's architecture: ${ARCHITECTURE}"
echo >&2

####################################################################################################################################
# parse arguments
####################################################################################################################################

ParaMonte_LIB_ROOT="/work/01902/ashahmor/stampede2/git/paramonte/build/linuxx64/intel/18.0.2.199/release/static/stack/mpi/Fortran"
KFACTOR_CORRECTION="onethird"
INTEGRATION_METHOD="romberg"
RATE_DENSITY_MODEL="B10"
FOR_COARRAY_NUM_IMAGES=3

usage()
{
cat << EndOfMessage

This is a simple standalone Bash script for building CosmicRate Fortran application based on the ParaMonte library on Unix-like OS.
The compiler name and flags are automatically inferred from the ParaMonte library name. Note this requires the existence of the 
required compiler and libraries on the system (presumably TACC). These requirements should be automatically installed on your 
system when ParaMonte is built via its provided build scripts at the root of its project's directory. 

    usage:

        buildCosmicRate.sh \
        --ratedensity <The Rate Density Model: H06/L08/B10/M14> \
        --kfac <kfactor correction model for durations: none,onethird> \
        --pmlib <path to the root of the ParaMonte library build> \
        --quad <quadrature method: romberg/quadpackSPR/quadpackDPR> \
        --num_images <number of processors: 3>

    example:

        build.sh -r B10 -k onethird -n 3 -p 

    flag definitions:

        -r | --ratedensity      : The GRB Rate Density model to be used
                                : possible values: H06/L08/B10/M14
        -k | --kfac             : The kfactor corrections to the observed durations.
                                : If not provided, the default value will be set to onethird.
        -q | --quad             : The quadrature method. possible values: romberg/quadpackSPR/quadpackDPR
                                : If not provided, the default value will be set to romberg.
        -n | --num_images       : the default number of processes (coarray images) on which the application 
                                : will be run: positive integer. If not provided, the default is 3.
        -p | --pmlib            : The path to the root of the specific ParaMonte library build.
                                : This root folder must contain the three lib,mod,obj folders.
        -h | --help             : help with the sctipt usage

NOTE: ALL FLAGS ARE OPTIONAL. If not provided, appropriate values will be set for each missing flag.
NOTE: Upon finishing the build, the script will generate another Bash script named run.sh in 
NOTE: the same bin directory, which can be used to run the executable. Usage:
NOTE: 
NOTE:     source ./run.sh

EndOfMessage
}

while [ "$1" != "" ]; do
    case $1 in
        -r | --ratedensity )    shift
                                RATE_DENSITY_MODEL=$1
                                ;;
        -k | --kfac )           shift
                                KFACTOR_CORRECTION=$1
                                ;;
        -q | --quad )           shift
                                INTEGRATION_METHOD=$1
                                ;;
        -p | --pmlib )          shift
                                ParaMonte_LIB_ROOT=$1
                                ;;
        -n | --num_images )     shift
                                FOR_COARRAY_NUM_IMAGES=$1
                                ;;
        -h | --help )           usage
                                exit
                                ;;
        * )                     echo >&2 "-- ${BUILD_NAME}BuildScript - FATAL: the input flag is not recognized: $1"
                                usage
                                exit 1
    esac
    shift
done

####################################################################################################################################
# ParaMonte library setup
####################################################################################################################################

export ParaMonte_LIB_ROOT
if ! [ -d "${ParaMonte_LIB_ROOT}" ]; then
    echo >&2 "-- ${BUILD_NAME} - FATAL: the input ParaMonte library build directory does not exist."
    echo >&2 "-- ${BUILD_NAME} - FATAL: ParaMonte_LIB_ROOT=${ParaMonte_LIB_ROOT}"
    exit 1
fi

ParaMonte_LIB_PATH="$(ls -d ${ParaMonte_LIB_ROOT}/lib/*libparamonte_* | sort -V | tail -n1)"; export ParaMonte_LIB_PATH
if ! [ -f "${ParaMonte_LIB_PATH}" ]; then
    echo >&2 "-- ${BUILD_NAME} - FATAL: the input ParaMonte library file does not exist."
    echo >&2 "-- ${BUILD_NAME} - FATAL: ParaMonte_LIB_PATH=${ParaMonte_LIB_PATH}"
    exit 1
fi

ParaMonte_LIB_NAME=${ParaMonte_LIB_PATH##*/}; export ParaMonte_LIB_NAME
#ParaMonte_LIB_NAME="libparamonte_static_stack_release_intel_fortran_mpi_linux_x64_mt.a"
ParaMonte_MOD_DIR="${ParaMonte_LIB_ROOT}/mod"; export ParaMonte_MOD_DIR
ParaMonte_LIB_DIR="${ParaMonte_LIB_ROOT}/obj"; export ParaMonte_LIB_DIR

# based on the specified value for ParaMonte_LIB_NAME:
#      - the build (release/debug)
#      - the parallelism (mpi/caf/serial)
#      - the library (dynamic/static)
# types will automatically inferred.

echo >&2 "-- ${BUILD_NAME} - ParaMonte_LIB_PATH: ${ParaMonte_LIB_PATH}"
echo >&2 "-- ${BUILD_NAME} - ParaMonte_LIB_ROOT: ${ParaMonte_LIB_ROOT}"
echo >&2 "-- ${BUILD_NAME} - ParaMonte_LIB_NAME: ${ParaMonte_LIB_NAME}"
echo >&2 "-- ${BUILD_NAME} - ParaMonte_MOD_DIR: ${ParaMonte_MOD_DIR}"
echo >&2 "-- ${BUILD_NAME} - ParaMonte_LIB_DIR: ${ParaMonte_LIB_DIR}"

####################################################################################################################################
# Configure CosmicRate
####################################################################################################################################

# input BATSE data file

BATSE_DATA_FILE_NAME="batse_1366_lgrb_p1024ph_epk_sch23ph.txt"; export BATSE_DATA_FILE_NAME
echo >&2 "-- ${BUILD_NAME} - BATSE_DATA_FILE_NAME: ${BATSE_DATA_FILE_NAME}"

# LGRB world model's SFR model: H06, L08, B10

#RATE_DENSITY_MODEL="B10"; 
export RATE_DENSITY_MODEL
echo >&2 "-- ${BUILD_NAME} - RATE_DENSITY_MODEL: ${RATE_DENSITY_MODEL}"

# set Kfactor correction: kfacOneThird, kfacNone

if [ "${KFACTOR_CORRECTION}" = "onethird" ]; then
    KFACTOR_CORRECTION="kfacOneThird";
else
    if [ "${KFACTOR_CORRECTION}" = "onethird" ]; then
        KFACTOR_CORRECTION="kfacNone";
    else
        echo >&2 "-- ${BUILD_NAME} - FATAL: unsupported input value for KFACTOR_CORRECTION=${KFACTOR_CORRECTION}"
        exit 1
    fi
fi
export KFACTOR_CORRECTION
echo >&2 "-- ${BUILD_NAME} - KFACTOR_CORRECTION: ${KFACTOR_CORRECTION}"

# set integration methodology: quadpackDPR, quadpackSPR, romberg

export INTEGRATION_METHOD
echo >&2 "-- ${BUILD_NAME} - INTEGRATION_METHOD: ${INTEGRATION_METHOD}"

# Flags controlling which actions to perform. all values should be set to either true or false values. Anything other than true is considered false.

CosmicRate_OBJ_ENABLED=true; export CosmicRate_OBJ_ENABLED
CosmicRate_EXE_ENABLED=true; export CosmicRate_EXE_ENABLED
CosmicRate_RUN_ENABLED=true; export CosmicRate_RUN_ENABLED
echo >&2 "-- ${BUILD_NAME} - CosmicRate_OBJ_ENABLED: ${CosmicRate_OBJ_ENABLED}"
echo >&2 "-- ${BUILD_NAME} - CosmicRate_EXE_ENABLED: ${CosmicRate_EXE_ENABLED}"
echo >&2 "-- ${BUILD_NAME} - CosmicRate_RUN_ENABLED: ${CosmicRate_RUN_ENABLED}"

if [ -z ${FOR_COARRAY_NUM_IMAGES+x} ]; then
    FOR_COARRAY_NUM_IMAGES=3; export FOR_COARRAY_NUM_IMAGES
fi
echo >&2 "-- ${BUILD_NAME} - default number of cores: $FOR_COARRAY_NUM_IMAGES"
echo >&2

####################################################################################################################################
# compiler setup
####################################################################################################################################

EXE_NAME="main.exe"; export EXE_NAME

unset FILE_EXT
unset COMPILER_NAME
unset LANG
unset COMPILER_SUITE
if [[ "${ParaMonte_LIB_NAME}" =~ .*"_fortran_".* ]]; then
    FILE_EXT=f90
    LANG=Fortran
    if [[ "$ParaMonte_LIB_NAME" =~ .*"_intel_".* ]]; then
        COMPILER_SUITE=intel
        COMPILER_NAME=ifort
    fi
    if [[ "$ParaMonte_LIB_NAME" =~ .*"_gnu_".* ]]; then
        COMPILER_SUITE=gnu
        COMPILER_NAME=gfortran
    fi
else
    CFI_ENABLED=true
    echo >&2
    echo >&2 "-- ${BUILD_NAME} - FATAL: unsupported ParaMonte library language: ${ParaMonte_LIB_NAME}"
    echo >&2 "-- ${BUILD_NAME} - gracefully exiting build."
    echo >&2
    exit 1
fi

# get compiler version

cd ../lib/
if ${COMPILER_NAME} getCompilerVersion.f90 -o getCompilerVersion.exe; then
    chmod +x getCompilerVersion.exe
    ./getCompilerVersion.exe && {
        COMPILER_VERSION="$(head -n 1 getCompilerVersion.tmp)"
        echo >&2 "-- ${BUILD_NAME} - ${COMPILER_SUITE} ${LANG} compiler version: COMPILER_VERSION=${COMPILER_VERSION}"
        rm *.tmp getCompilerVersion.exe
    } || {
        echo >&2 "-- ${BUILD_NAME} - failed to detect the ${COMPILER_SUITE} ${LANG} compiler version...skipping"
        unset ${COMPILER_VERSION}
    }

else

    echo >&2 "-- ${BUILD_NAME} - failed to detect the ${COMPILER_SUITE} ${LANG} compiler version...skipping"
    unset ${COMPILER_VERSION}

fi
cd "${FILE_DIR}"

####################################################################################################################################
# build setup
####################################################################################################################################

unset BTYPE
unset GNU_C_COMPILER_FLAGS
unset GNU_Fortran_COMPILER_FLAGS
unset INTEL_C_COMPILER_FLAGS
INTEL_Fortran_COMPILER_FLAGS="-standard-semantics"

if [[ "$ParaMonte_LIB_NAME" =~ .*"release".* ]]; then
    BTYPE=release
    GNU_C_COMPILER_FLAGS="${GNU_C_COMPILER_FLAGS} -O3"
    GNU_Fortran_COMPILER_FLAGS="${GNU_Fortran_COMPILER_FLAGS} -O3 -funroll-loops -finline-functions -ftree-vectorize"
    INTEL_C_COMPILER_FLAGS="${INTEL_C_COMPILER_FLAGS} -O3"
    INTEL_Fortran_COMPILER_FLAGS="${INTEL_Fortran_COMPILER_FLAGS} -O3 -ip -ipo -unroll -unroll-aggressive -finline-functions"
fi
if [[ "$ParaMonte_LIB_NAME" =~ .*"testing".* ]]; then
    BTYPE=testing
    GNU_C_COMPILER_FLAGS="${GNU_C_COMPILER_FLAGS} -O2"
    GNU_Fortran_COMPILER_FLAGS="${GNU_Fortran_COMPILER_FLAGS} -O2"
    INTEL_C_COMPILER_FLAGS="${INTEL_C_COMPILER_FLAGS} -O2"
    INTEL_Fortran_COMPILER_FLAGS="${INTEL_Fortran_COMPILER_FLAGS} -O2"
fi
if [[ "$ParaMonte_LIB_NAME" =~ .*"debug".* ]]; then
    BTYPE=debug
    GNU_C_COMPILER_FLAGS="${GNU_C_COMPILER_FLAGS} -O0 -g"
    GNU_Fortran_COMPILER_FLAGS="${GNU_Fortran_COMPILER_FLAGS} -O0 -g"
    INTEL_C_COMPILER_FLAGS="${INTEL_C_COMPILER_FLAGS} -O0 -debug full"
    INTEL_Fortran_COMPILER_FLAGS="${INTEL_Fortran_COMPILER_FLAGS} -O0 -debug full"
fi
echo >&2 "-- ${BUILD_NAME} - ParaMonte build type: ${BTYPE}"

####################################################################################################################################
# library type
####################################################################################################################################

if [[ "${ParaMonte_LIB_NAME}" =~ .*"dynamic".* ]]; then
    LTYPE=dynamic
else
    LTYPE=static
fi
echo >&2 "-- ${BUILD_NAME} - ParaMonte library type: ${LTYPE}"

####################################################################################################################################
# MPI
####################################################################################################################################

PTYPE=serial
MPI_ENABLED=false
if [[ "$ParaMonte_LIB_NAME" =~ .*"mpi".* ]]; then
    MPI_ENABLED=true
    PTYPE=mpi
fi
echo >&2 "-- ${BUILD_NAME} - MPI_ENABLED: ${MPI_ENABLED}"

if [ "${MPI_ENABLED}" = "true" ]; then
    if [ "${LANG}" = "Fortran" ]; then
        if [ "${COMPILER_SUITE}" = "intel" ]; then
            COMPILER_NAME="mpiifort"
        fi
        if [ "${COMPILER_SUITE}" = "gnu" ]; then
            COMPILER_NAME="mpifort"
        fi
    fi
fi

####################################################################################################################################
# CAF
####################################################################################################################################

CAFTYPE=none
CAF_ENABLED=false
if [[ "$ParaMonte_LIB_NAME" =~ .*"cafsingle".* ]]; then
    #-fcoarray=single
    GNU_Fortran_COMPILER_FLAGS="${GNU_Fortran_COMPILER_FLAGS}"
    INTEL_Fortran_COMPILER_FLAGS="${INTEL_Fortran_COMPILER_FLAGS} -coarray=single"
    CAF_ENABLED=true
    PTYPE=cafsingle
fi
if [[ "$ParaMonte_LIB_NAME" =~ .*"cafshared".* ]]; then
    #-fcoarray=shared
    GNU_Fortran_COMPILER_FLAGS="${GNU_Fortran_COMPILER_FLAGS}"
    INTEL_Fortran_COMPILER_FLAGS="${INTEL_Fortran_COMPILER_FLAGS} -coarray=shared"
    CAF_ENABLED=true
    PTYPE=cafshared
fi
if [[ "$ParaMonte_LIB_NAME" =~ .*"cafdistributed".* ]]; then
    #-fcoarray=distributed
    GNU_Fortran_COMPILER_FLAGS="${GNU_Fortran_COMPILER_FLAGS}"
    INTEL_Fortran_COMPILER_FLAGS="${INTEL_Fortran_COMPILER_FLAGS} -coarray=distributed"
    CAF_ENABLED=true
    PTYPE=cafdistributed
fi
echo >&2 "-- ${BUILD_NAME} - CAFTYPE: ${CAFTYPE}"

if [ "${COMPILER_SUITE}" = "gnu" ] && [ "${CAF_ENABLED}" = "true" ]; then
    COMPILER_NAME=caf
fi

####################################################################################################################################
# report compiler choice
####################################################################################################################################

echo >&2 "-- ${BUILD_NAME} - ParaMonte library's compiler suite: ${COMPILER_SUITE}"
echo >&2 "-- ${BUILD_NAME} - inferred compiler choice(s): ${COMPILER_NAME}"

if [ -z ${USER_SELECTED_COMPILER+x} ]; then
    echo >&2 "-- ${BUILD_NAME} - user-selected compiler/linker: none"
else
    COMPILER_NAME="${USER_SELECTED_COMPILER}"
    echo >&2 "-- ${BUILD_NAME} - user-selected compiler/linker: ${USER_SELECTED_COMPILER}"
fi

if [ -z ${USER_SELECTED_COMPILER_FLAGS+x} ]; then
    if [ "${COMPILER_SUITE}" = "intel" ]; then
        #if [ "${LANG}" = "C" ]; then COMPILER_FLAGS=${INTEL_C_COMPILER_FLAGS}; fi
        if [ "${LANG}" = "Fortran" ]; then COMPILER_FLAGS="${INTEL_Fortran_COMPILER_FLAGS}"; fi
    fi
    if [ "${COMPILER_SUITE}" = "gnu" ]; then
        #if [ "${LANG}" = "C" ]; then COMPILER_FLAGS=${GNU_C_COMPILER_FLAGS}; fi
        if [ "${LANG}" = "Fortran" ]; then COMPILER_FLAGS="${GNU_Fortran_COMPILER_FLAGS}"; fi
    fi
    echo >&2 "-- ${BUILD_NAME} - inferred compiler/linker flags(s): ${COMPILER_FLAGS}"
else
    COMPILER_FLAGS="${USER_SELECTED_COMPILER_FLAGS}"
    echo >&2 "-- ${BUILD_NAME} - user-selected compiler/linker flags: ${USER_SELECTED_COMPILER_FLAGS}"
fi

####################################################################################################################################
# preprocessor flags
####################################################################################################################################

if [ "${COMPILER_SUITE}" = "gnu" ]; then FPP_FLAGS="-cpp"; fi
if [ "${COMPILER_SUITE}" = "intel" ]; then FPP_FLAGS="-fpp"; fi

if [ "${CFI_ENABLED}" = "true" ]; then FPP_FLAGS="${FPP_FLAGS} -DCFI_ENABLED"; fi
if [ "${CAF_ENABLED}" = "true" ]; then FPP_FLAGS="${FPP_FLAGS} -DCAF_ENABLED"; fi
if [ "${MPI_ENABLED}" = "true" ]; then FPP_FLAGS="${FPP_FLAGS} -DMPI_ENABLED"; fi

# add Kfactor correction and SFR macros

FPP_FLAGS="${FPP_FLAGS} -D${KFACTOR_CORRECTION}"
FPP_FLAGS="${FPP_FLAGS} -D${RATE_DENSITY_MODEL}"
FPP_FLAGS="${FPP_FLAGS} -D${INTEGRATION_METHOD}"
export FPP_FLAGS

MTYPE="${KFACTOR_CORRECTION}${RATE_DENSITY_MODEL}"

echo >&2
echo >&2 "-- ${BUILD_NAME} - preprocessor macros: ${FPP_FLAGS}"
echo >&2 "-- ${BUILD_NAME} - chosen model: ${MTYPE}"
echo >&2

####################################################################################################################################
# set up directories
####################################################################################################################################

BUILD_SUCCEEDED=false
RUN_FILE_NAME="run.sh"

CosmicRate_ROOT_PATH="${FILE_DIR}"
CosmicRate_BLD_DIR="${CosmicRate_ROOT_PATH}/build/${PLATFORM}${ARCHITECTURE}/${COMPILER_SUITE}/${COMPILER_VERSION}/${BTYPE}/${LTYPE}/${PTYPE}/${MTYPE}/${INTEGRATION_METHOD}"
CosmicRate_SRC_DIR="${CosmicRate_ROOT_PATH}/src"
CosmicRate_MOD_DIR="${CosmicRate_BLD_DIR}/mod"
CosmicRate_OBJ_DIR="${CosmicRate_BLD_DIR}/obj"
CosmicRate_BIN_DIR="${CosmicRate_BLD_DIR}/bin"

echo >&2
echo >&2 "-- ${BUILD_NAME} - CosmicRate_BLD_DIR: ${CosmicRate_BLD_DIR}"
echo >&2 "-- ${BUILD_NAME} - CosmicRate_SRC_DIR: ${CosmicRate_SRC_DIR}"
echo >&2 "-- ${BUILD_NAME} - CosmicRate_MOD_DIR: ${CosmicRate_MOD_DIR}"
echo >&2 "-- ${BUILD_NAME} - CosmicRate_OBJ_DIR: ${CosmicRate_OBJ_DIR}"
echo >&2 "-- ${BUILD_NAME} - CosmicRate_BIN_DIR: ${CosmicRate_BIN_DIR}"
echo >&2

if [ -d "${CosmicRate_BLD_DIR}" ]; then
    echo >&2 "-- ${BUILD_NAME} - build directory already exists. skipping..."
else
    echo >&2 "-- ${BUILD_NAME} - generating build directory..."
    mkdir -p "${CosmicRate_BLD_DIR}"
fi
echo >&2 "-- ${BUILD_NAME} - all generated build files will be stored at: ${CosmicRate_BLD_DIR}"

if ! [ -d "${CosmicRate_OBJ_DIR}" ]; then mkdir -p "${CosmicRate_OBJ_DIR}"; fi
if ! [ -d "${CosmicRate_MOD_DIR}" ]; then mkdir -p "${CosmicRate_MOD_DIR}"; fi
if ! [ -d "${CosmicRate_BIN_DIR}" ]; then mkdir -p "${CosmicRate_BIN_DIR}"; fi

####################################################################################################################################
# get source file list
####################################################################################################################################

SRC_FILE_LIST_PATH="${CosmicRate_SRC_DIR}/filelist.txt"
unset SRC_FILE_LIST
while read -r line; do
    [[ $line = *\!* ]] && continue
    SRC_FILE_LIST="${SRC_FILE_LIST} ${line}"
done < "$SRC_FILE_LIST_PATH"

####################################################################################################################################
# build CosmicRate
####################################################################################################################################

echo >&2
echo >&2 "-- ${BUILD_NAME} - compiling ${BUILD_NAME} with ${COMPILER_NAME}"

cd "${CosmicRate_OBJ_DIR}"
for SRC_FILE in ${SRC_FILE_LIST}
do
    echo >&2 "-- ${BUILD_NAME} - ${COMPILER_NAME} ${COMPILER_FLAGS} ${CosmicRate_SRC_DIR}/${SRC_FILE} -c"
    ${COMPILER_NAME} ${COMPILER_FLAGS} ${FPP_FLAGS} \
    -module "${CosmicRate_MOD_DIR}" \
    -I"${CosmicRate_MOD_DIR}" \
    -I"${ParaMonte_MOD_DIR}" \
    -c "${CosmicRate_SRC_DIR}/${SRC_FILE}"
done
echo >&2

LINKER=${COMPILER_NAME}
LINKER_FLAGS=
if [ "${LANG}" = "C" ] && [ "${LTYPE}" = "static" ]; then
    if [ "${COMPILER_SUITE}" = "intel" ]; then
        LINKER_FLAGS="-nofor_main"
        if [ "${MPI_ENABLED}" = "true" ]; then
            LINKER="mpiifort" # xxx point of weakness: assumes intel mpi to have been installed
        else
            LINKER="ifort"
        fi
    fi
    if [ "${COMPILER_SUITE}" = "gnu" ]; then
        if [ "${MPI_ENABLED}" = "true" ]; then
            LINKER="mpifort"
        else
            LINKER="gfortran"
        fi
    fi
fi

echo >&2
echo >&2 "-- ${BUILD_NAME} - linking ${BUILD_NAME} with ${LINKER}"
echo >&2 "-- ${BUILD_NAME} - ${LINKER} ${COMPILER_FLAGS} ${LINKER_FLAGS} .o ${ParaMonte_LIB_PATH} -o ${EXE_NAME}"

cd "${CosmicRate_BIN_DIR}"
${LINKER} ${COMPILER_FLAGS} ${LINKER_FLAGS} \
-module "${CosmicRate_MOD_DIR}" \
-I"${CosmicRate_MOD_DIR}" \
-I"${ParaMonte_MOD_DIR}" \
"${CosmicRate_OBJ_DIR}"/*.o \
${ParaMonte_LIB_PATH} \
-o ${EXE_NAME}

if [ $? -eq 0 ]; then

    echo >&2 "-- ${BUILD_NAME} - build appears to have succeeded."
    BUILD_SUCCEEDED=true

    # copy necessary input files in the executable's directory

    mkdir -p "${CosmicRate_BIN_DIR}/in/"
    INPUT_SPEC_FILE_NAME=${MTYPE}.nml
    echo >&2 "-- ${BUILD_NAME} - copying input files to the executable's directory"
    echo >&2 "-- ${BUILD_NAME} - from: ${CosmicRate_ROOT_PATH}/in/${INPUT_SPEC_FILE_NAME}"
    echo >&2 "-- ${BUILD_NAME} -   to: ${CosmicRate_BIN_DIR}/in/"
    cp "${CosmicRate_ROOT_PATH}/in/${INPUT_SPEC_FILE_NAME}" "${CosmicRate_BIN_DIR}/in/"
    echo >&2
    echo >&2 "-- ${BUILD_NAME} - copying BATSE data to the executable's directory"
    echo >&2 "-- ${BUILD_NAME} - from: ${CosmicRate_ROOT_PATH}/in/BATSE_1366_LGRB_P1024ph_Epk_Sch23ph.txt"
    echo >&2 "-- ${BUILD_NAME} -   to: ${CosmicRate_BIN_DIR}/in/"
    cp "${CosmicRate_ROOT_PATH}/in/${BATSE_DATA_FILE_NAME}" "${CosmicRate_BIN_DIR}/in/"
    echo >&2

    EXE_NAME_WITH_OPTIONS="./${EXE_NAME} ./in/ ${INPUT_SPEC_FILE_NAME}"

    {
    echo "# ${BUILD_NAME} runtime setup script."
    echo "# "
    } > ${RUN_FILE_NAME}
    if [ "${MPI_ENABLED}" = "true" ] || [ "${CAF_ENABLED}" = "true" ]; then
        {
        echo "# usage:"
        echo "# "
        echo "#     source ./${RUN_FILE_NAME} -n number_of_processors"
        echo "# "
        echo "# where number_of_processors is an integer representing the number"
        echo "# of physical proceesors on which the application will be run."
        echo ""
        echo "while [ \"\$1\" != \"\" ]; do"
        echo "    case \$1 in"
        echo "        -n | --num_images )   shift"
        echo "                              FOR_COARRAY_NUM_IMAGES=\$1"
        echo "                              ;;"
        echo "        * )                   echo >\&2 \"-- ${BUILD_NAME}RunScript - FATAL: the input flag is not recognized: \$1\""
        echo "                              exit 1"
        echo "    esac"
        echo "    shift"
        echo "done"
        echo ""
        } >> ${RUN_FILE_NAME}
    else
        {
        echo "# usage:"
        echo "#     source ./${RUN_FILE_NAME}"
        echo ""
        } >> ${RUN_FILE_NAME}
    fi
    {
    echo ""
    echo "FILE_DIR=\"\$( cd \"\$( dirname \"\${BASH_SOURCE[0]}\" )\" >/dev/null 2>&1 && pwd )\""
    echo "if [ -z \${PATH+x} ]; then"
    echo "    PATH=."
    echo "else"
    echo "    if [[ \":\$PATH:\" != *\":${FILE_DIR}:\"* ]]; then"
    echo "        PATH=\"${FILE_DIR}:\${PATH}\""
    echo "    fi"
    echo "fi"
    echo "export LD_LIBRARY_PATH"
    echo "if [ -z \${LD_LIBRARY_PATH+x} ]; then"
    echo "    LD_LIBRARY_PATH=."
    echo "else"
    echo "    if [[ \":\$LD_LIBRARY_PATH:\" != *\":${FILE_DIR}:\"* ]]; then"
    echo "        LD_LIBRARY_PATH=\"${FILE_DIR}:\${LD_LIBRARY_PATH}\""
    echo "    fi"
    echo "fi"
    echo "export LD_LIBRARY_PATH"
    echo "export PATH"
    echo ""
    echo ""
    echo "if [ -z \${FOR_COARRAY_NUM_IMAGES+x} ]; then"
    echo "    FOR_COARRAY_NUM_IMAGES=${FOR_COARRAY_NUM_IMAGES}"
    echo "fi"
    echo ""
    } >> ${RUN_FILE_NAME}

    if [ -f "./setup.sh" ]; then
        echo "source ./setup.sh" >> ${RUN_FILE_NAME}
        echo "" >> ${RUN_FILE_NAME}
    fi

    echo "# run ${BUILD_NAME} executable" >> ${RUN_FILE_NAME}
    echo "" >> ${RUN_FILE_NAME}
    echo "chmod +x ${EXE_NAME}" >> ${RUN_FILE_NAME}
    if [ "${MPI_ENABLED}" = "true" ]; then
        echo "mpiexec -np \${FOR_COARRAY_NUM_IMAGES} ${EXE_NAME_WITH_OPTIONS}" >> ${RUN_FILE_NAME}
        echo "" >> ${RUN_FILE_NAME}
    else
        if [ "${CAF_ENABLED}" = "true" ]; then
            if [ "${COMPILER_SUITE}" = "intel" ]; then
                echo "export FOR_COARRAY_NUM_IMAGES && ${EXE_NAME_WITH_OPTIONS}" >> ${RUN_FILE_NAME}
            else
                echo "cafrun -np \${FOR_COARRAY_NUM_IMAGES} ${EXE_NAME_WITH_OPTIONS}" >> ${RUN_FILE_NAME}
            fi
        else
            echo "${EXE_NAME_WITH_OPTIONS}" >> ${RUN_FILE_NAME}
        fi
    fi

    chmod +x ${RUN_FILE_NAME}

    break

else

    echo >&2
    echo >&2 "-- ${BUILD_NAME} - build appears to have failed. skipping..."

fi

##########################################

echo >&2

if [ "${BUILD_SUCCEEDED}" = "true" ]; then

    echo >&2 "-- ${BUILD_NAME} - To run the executable with the propoer environmental setup, try:"
    echo >&2 ""
    echo >&2 "    cd ${CosmicRate_BIN_DIR}"
    echo >&2 "    source ./${RUN_FILE_NAME}"
    echo >&2 ""
    echo >&2 "-- ${BUILD_NAME} - Look at the contents of ${RUN_FILE_NAME} to change the runtime settings as you wish."
    echo >&2

else

    echo >&2 "-- ${BUILD_NAME} - exhausted all possible compiler names to build and run ${BUILD_NAME} but failed."
    echo >&2 "-- ${BUILD_NAME} - Please consider reporting the circumstances surrounding this issue at"
    echo >&2 "-- ${BUILD_NAME} - "
    echo >&2 "-- ${BUILD_NAME} -    https://github.com/cdslaborg/paramonte/issues"
    echo >&2 "-- ${BUILD_NAME} - "
    echo >&2 "-- ${BUILD_NAME} - or directly to ParaMonte authors (e.g., shahmoradi@utexas.edu)"
    echo >&2 "-- ${BUILD_NAME} - "
    echo >&2 "-- ${BUILD_NAME} - gracefully exiting ParaMonte"
    echo >&2

    exit 1

fi

exit 0
