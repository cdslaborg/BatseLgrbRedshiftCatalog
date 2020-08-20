program SynSam_prog

use, intrinsic :: iso_fortran_env, only: output_unit
use System_mod, only: CmdArg_type
use Constants_mod, only: IK, RK
use ParaPost_mod, only: ParaPost_type
use SynRed_mod, only: SynRed_type
use SynSam_mod, only: generateSynSam

implicit none

character(*), parameter             :: GRB_RATE_MODEL(*) = [ "H06" , "L08" , "B10" , "M14" , "M17" , "F18" , "P15" ]
integer(IK) , parameter             :: N_GRB_RATE_MODEL = size(GRB_RATE_MODEL)

character(3)                        :: GrbFormationModel(N_GRB_RATE_MODEL)  ! list of GRB formation models
character(2047)                     :: SynRedFilePath(N_GRB_RATE_MODEL)     ! list of synthetic redshift file paths
character(2047)                     :: ParaPostFilePath(N_GRB_RATE_MODEL)
character(2047)                     :: outPathBase
character(:), allocatable           :: outPath

type(SynRed_type)                   :: SynRed
type(ParaPost_type)                 :: ParaPost
type(CmdArg_type)                   :: CmdArg

integer                             :: inputFileUnit, imodel
integer(IK)                         :: sampleSize = 200000

namelist /SynSam/ GrbFormationModel
namelist /SynSam/ ParaPostFilePath
namelist /SynSam/ SynRedFilePath
namelist /SynSam/ outPathBase
namelist /SynSam/ sampleSize

! query input data file name from the command line

call CmdArg%query()
if (CmdArg%count/=1) then
    write(output_unit,"(*(g0))")
    write(output_unit,"(*(g0))") "FATAL: Invalid number of command-line arguments: ", CmdArg%count
    write(output_unit,"(*(g0))") "       Use the following example syntax to invoke the program: "
    write(output_unit,"(*(g0))") "       a.exe <input file path: ../in/SyntheticSample.nml>"
    error stop
end if

! read simulation input data

open( newunit = inputFileUnit, file = CmdArg%Arg(1)%record, status="old" )
    GrbFormationModel = "NUL"
    read(inputFileUnit,nml=SynSam)
close(inputFileUnit)

! generate synthetic sample

do imodel = 1, N_GRB_RATE_MODEL

    if ( any(GrbFormationModel(imodel) == GRB_RATE_MODEL) ) then

        write(output_unit,"(*(g0))"); write(output_unit,"(*(g0))")
        write(output_unit,"(*(g0))") "Generating GRB synthetic sample based on the GRB rate model of ", GrbFormationModel(imodel)
        write(output_unit,"(*(g0))"); write(output_unit,"(*(g0))")

        ! generate the synthetic sample

        call execute_command_line("mkdir "//trim(adjustl(outPathBase)))
        outPath = trim(adjustl(outPathBase)) // "syntheticSample" // trim(adjustl(GrbFormationModel(imodel))) // ".csv"

        ! Read the LGRB world model parameters

        ParaPost = ParaPost_type( sampleFilePath = trim(adjustl( ParaPostFilePath(imodel) )) )

        ! Read the synthetic redshifts

        SynRed = SynRed_type( sampleFilePath = trim(adjustl(SynRedFilePath(imodel))) )

        ! generate LGRB sample

        call generateSynSam ( ParaPost      = ParaPost      &
                            , SynRed        = SynRed        &
                            , outFilePath   = outPath       &
                            , sampleSize    = sampleSize    &
                            )

    elseif (GrbFormationModel(imodel)/="NUL") then 

        write(output_unit,"(*(g0))")
        write(output_unit,"(*(g0))") "FATAL: Invalid star formation model on input: ", trim(adjustl(GrbFormationModel(imodel)))
        write(output_unit,"(*(g0))") "       Use the one of following supported models: "
        write(output_unit,"(*(g0))")
        write(output_unit,"(*(g0,:, ' '))")  "        ", GRB_RATE_MODEL
        write(output_unit,"(*(g0))")

    end if

end do

end program SynSam_prog
