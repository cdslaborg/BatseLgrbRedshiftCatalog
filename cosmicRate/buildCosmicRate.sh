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
# Configure CosmicRate
####################################################################################################################################

# ParaMonte library setup

# change the following hard-coded ParaMonte library names and root path to the desired library names and bin folder of ParaMonte
# based on the specified value for ParaMonte_LIB_NAME:
#      - the build (release/debug)
#      - the parallelism (mpi/caf/serial)
#      - the library (dynamic/static)
# types will automatically inferred.

ParaMonte_LIB_ROOT="/work/01902/ashahmor/stampede2/git/paramonte/build/linuxx64/intel/18.0.2.199/release/static/stack/mpi/Fortran"; export ParaMonte_LIB_ROOT
echo >&2 "-- ${BUILD_NAME} - ParaMonte_LIB_ROOT: ${ParaMonte_LIB_ROOT}"

ParaMonte_LIB_NAME="libparamonte_static_stack_release_intel_fortran_mpi_linux_x64_mt.a"; export ParaMonte_LIB_NAME
echo >&2 "-- ${BUILD_NAME} - ParaMonte_LIB_NAME: ${ParaMonte_LIB_NAME}"

ParaMonte_MOD_DIR="${ParaMonte_LIB_ROOT}/mod"; export ParaMonte_MOD_DIR
ParaMonte_LIB_DIR="${ParaMonte_LIB_ROOT}/obj"; export ParaMonte_LIB_DIR
ParaMonte_LIB_PATH="${ParaMonte_LIB_DIR}/ParaMonte_LIB_NAME"; export ParaMonte_LIB_PATH
echo >&2 "-- ${BUILD_NAME} - ParaMonte_MOD_DIR: ${ParaMonte_MOD_DIR}"
echo >&2 "-- ${BUILD_NAME} - ParaMonte_LIB_DIR: ${ParaMonte_LIB_DIR}"
echo >&2 "-- ${BUILD_NAME} - ParaMonte_LIB_PATH: ${ParaMonte_LIB_PATH}"


# input BATSE data file

BATSE_DATA_FILE_PATH="BATSE_1366_LGRB_P1024ph_Epk_Sch23ph.txt"; export BATSE_DATA_FILE_PATH
echo >&2 "-- ${BUILD_NAME} - BATSE_DATA_FILE_PATH: ${BATSE_DATA_FILE_PATH}"

# LGRB world model's SFR model: H06, L08, B10

RATE_DENSITY_MODEL="B10"; export RATE_DENSITY_MODEL
echo >&2 "-- ${BUILD_NAME} - RATE_DENSITY_MODEL: ${RATE_DENSITY_MODEL}"

# set Kfactor correction: kfacOneThird, kfacNone

KFAC_CORRECTION="kfacOneThird"; export KFAC_CORRECTION
echo >&2 "-- ${BUILD_NAME} - KFAC_CORRECTION: ${KFAC_CORRECTION}"

# set integration methodology: quadpackDPR, quadpackSPR, romberg

INTEGRATION_METHOD="romberg"; export INTEGRATION_METHOD
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
unset COMPILER_LIST
unset EXAMPLE_LANGUAGE
unset PM_COMPILER_SUITE
if [[ "$ParaMonte_LIB_NAME" =~ .*"_fortran_".* ]]; then
    FILE_EXT=f90
    EXAMPLE_LANGUAGE=Fortran
    if [[ "$ParaMonte_LIB_NAME" =~ .*"_intel_".* ]]; then
        PM_COMPILER_SUITE=intel
        COMPILER_LIST=ifort
    fi
    if [[ "$ParaMonte_LIB_NAME" =~ .*"_gnu_".* ]]; then
        PM_COMPILER_SUITE=gnu
        COMPILER_LIST=gfortran
    fi
else
    echo >&2
    echo >&2 "-- ${BUILD_NAME} - Fatal: unsupported ParaMonte library language: ${ParaMonte_LIB_NAME}"
    echo >&2 "-- ${BUILD_NAME} - gracefully exiting build."
    echo >&2
    exit 1
fi

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

MPI_ENABLED=false
if [[ "$ParaMonte_LIB_NAME" =~ .*"mpi".* ]]; then
    MPI_ENABLED=true
fi
echo >&2 "-- ${BUILD_NAME} - MPI_ENABLED: ${MPI_ENABLED}"

if [ "${MPI_ENABLED}" = "true" ]; then
    if [ "${EXAMPLE_LANGUAGE}" = "Fortran" ]; then
        COMPILER_LIST="mpiifort mpifort"
    fi
    if [ "${EXAMPLE_LANGUAGE}" = "C" ]; then
        COMPILER_LIST="mpiicc mpicc"
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
fi
if [[ "$ParaMonte_LIB_NAME" =~ .*"cafshared".* ]]; then
    #-fcoarray=shared
    GNU_Fortran_COMPILER_FLAGS="${GNU_Fortran_COMPILER_FLAGS}"
    INTEL_Fortran_COMPILER_FLAGS="${INTEL_Fortran_COMPILER_FLAGS} -coarray=shared"
    CAF_ENABLED=true
fi
if [[ "$ParaMonte_LIB_NAME" =~ .*"cafdistributed".* ]]; then
    #-fcoarray=distributed
    GNU_Fortran_COMPILER_FLAGS="${GNU_Fortran_COMPILER_FLAGS}"
    INTEL_Fortran_COMPILER_FLAGS="${INTEL_Fortran_COMPILER_FLAGS} -coarray=distributed"
    CAF_ENABLED=true
fi
echo >&2 "-- ${BUILD_NAME} - CAFTYPE: ${CAFTYPE}"

if [ "${PM_COMPILER_SUITE}" = "gnu" ] && [ "${CAF_ENABLED}" = "true" ]; then
    COMPILER_LIST=caf
fi

####################################################################################################################################
# report compiler choice
####################################################################################################################################

echo >&2 "-- ${BUILD_NAME} - ParaMonte library's compiler suite: ${PM_COMPILER_SUITE}"
echo >&2 "-- ${BUILD_NAME} - inferred compiler choice(s): ${COMPILER_LIST}"

if [ -z ${USER_SELECTED_COMPILER+x} ]; then
    echo >&2 "-- ${BUILD_NAME} - user-selected compiler/linker: none"
else
    COMPILER_LIST="${USER_SELECTED_COMPILER}"
    echo >&2 "-- ${BUILD_NAME} - user-selected compiler/linker: ${USER_SELECTED_COMPILER}"
fi

if [ -z ${USER_SELECTED_COMPILER_FLAGS+x} ]; then
    if [ "${PM_COMPILER_SUITE}" = "intel" ]; then
        if [ "${EXAMPLE_LANGUAGE}" = "C" ]; then COMPILER_FLAGS=${INTEL_C_COMPILER_FLAGS}; fi
        if [ "${EXAMPLE_LANGUAGE}" = "Fortran" ]; then COMPILER_FLAGS="${INTEL_Fortran_COMPILER_FLAGS} -fpp -DIS_COMPATIBLE_COMPILER"; fi
    fi
    if [ "${PM_COMPILER_SUITE}" = "gnu" ]; then
        if [ "${EXAMPLE_LANGUAGE}" = "C" ]; then COMPILER_FLAGS=${GNU_C_COMPILER_FLAGS}; fi
        if [ "${EXAMPLE_LANGUAGE}" = "Fortran" ]; then COMPILER_FLAGS="${GNU_Fortran_COMPILER_FLAGS} -cpp -DIS_COMPATIBLE_COMPILER"; fi
    fi
    echo >&2 "-- ${BUILD_NAME} - inferred compiler/linker flags(s): ${COMPILER_FLAGS}"
else
    COMPILER_FLAGS="${USER_SELECTED_COMPILER_FLAGS}"
    echo >&2 "-- ${BUILD_NAME} - user-selected compiler/linker flags: ${USER_SELECTED_COMPILER_FLAGS}"
fi

####################################################################################################################################
# build example
####################################################################################################################################

BUILD_SUCCEEDED=false
RUN_FILE_NAME="run.sh"

for COMPILER in ${COMPILER_LIST}
do

    echo >&2
    echo >&2 "-- ${BUILD_NAME} - compiling ParaMonte example with ${COMPILER}"
    echo >&2 "-- ${BUILD_NAME} - ${COMPILER} ${COMPILER_FLAGS} BatseLgrbWorldModel_mod.${FILE_EXT} main.${FILE_EXT} ${ParaMonte_LIB_PATH} -o ${EXE_NAME}"

    ${COMPILER} ${COMPILER_FLAGS} BatseLgrbWorldModel_mod.${FILE_EXT} main.${FILE_EXT} -c

    LINKER=${COMPILER}
    LINKER_FLAGS=
    if [ "${EXAMPLE_LANGUAGE}" = "C" ] && [ "${LTYPE}" = "static" ]; then
        if [ "${PM_COMPILER_SUITE}" = "intel" ]; then
            LINKER_FLAGS="-nofor_main"
            if [ "${MPI_ENABLED}" = "true" ]; then
                LINKER="mpiifort" # xxx point of weakness: assumes intel mpi to have been installed
            else
                LINKER="ifort"
            fi
        fi
        if [ "${PM_COMPILER_SUITE}" = "gnu" ]; then
            if [ "${MPI_ENABLED}" = "true" ]; then
                LINKER="mpifort"
            else
                LINKER="gfortran"
            fi
        fi
    fi

    echo >&2
    echo >&2 "-- ${BUILD_NAME} - linking ParaMonte example with ${LINKER}"
    echo >&2 "-- ${BUILD_NAME} - ${LINKER} ${COMPILER_FLAGS} ${LINKER_FLAGS} BatseLgrbWorldModel_mod.o main.o ${ParaMonte_LIB_PATH} -o ${EXE_NAME}"

    ${LINKER} ${COMPILER_FLAGS} ${LINKER_FLAGS} BatseLgrbWorldModel_mod.o main.o ${ParaMonte_LIB_NAME} -o ${EXE_NAME}

    if [ $? -eq 0 ]; then

        BUILD_SUCCEEDED=true

        echo >&2 "-- ${BUILD_NAME} - example build appears to have succeeded."

        {
        echo "# ParaMonte example runtime setup script."
        echo "# "
        } > ${RUN_FILE_NAME}
        if [ "${MPI_ENABLED}" = "true" ] || [ "${CAF_ENABLED}" = "true" ]; then
            {
            echo "# usage:"
            echo "# "
            echo "#     source ./${RUN_FILE_NAME} -n number_of_processors"
            echo "# "
            echo "# where number_of_processors is an integer representing the number"
            echo "# of physical proceesors on which the example will be run."
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

        echo "# run ParaMonte example executable" >> ${RUN_FILE_NAME}
        echo "" >> ${RUN_FILE_NAME}
        echo "chmod +x ${EXE_NAME}" >> ${RUN_FILE_NAME}
        if [ "${MPI_ENABLED}" = "true" ]; then
            echo "mpiexec -np \${FOR_COARRAY_NUM_IMAGES} ./${EXE_NAME}" >> ${RUN_FILE_NAME}
            echo "" >> ${RUN_FILE_NAME}
        else
            if [ "${CAF_ENABLED}" = "true" ]; then
                if [ "${PM_COMPILER_SUITE}" = "intel" ]; then
                    echo "export FOR_COARRAY_NUM_IMAGES && ./${EXE_NAME}" >> ${RUN_FILE_NAME}
                else
                    echo "cafrun -np \${FOR_COARRAY_NUM_IMAGES} ./${EXE_NAME}" >> ${RUN_FILE_NAME}
                fi
            else
                echo "./${EXE_NAME}" >> ${RUN_FILE_NAME}
            fi
        fi

        chmod +x ${RUN_FILE_NAME}

        break

    else

        echo >&2
        echo >&2 "-- ${BUILD_NAME} - example build appears to have failed. skipping..."

    fi

done

echo >&2

if [ "${BUILD_SUCCEEDED}" = "true" ]; then

    echo >&2 "-- ${BUILD_NAME} - To run the example's executable with the propoer environmental setup, try:"
    echo >&2 "-- ${BUILD_NAME} - "
    echo >&2 "-- ${BUILD_NAME} -     source ./${RUN_FILE_NAME}"
    echo >&2 "-- ${BUILD_NAME} - "
    echo >&2 "-- ${BUILD_NAME} - Look at the contents of ${RUN_FILE_NAME} to change the runtime settings as you wish."
    echo >&2

else

    echo >&2 "-- ${BUILD_NAME} - exhausted all possible compiler names to build and run ParaMonte example but failed."
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
