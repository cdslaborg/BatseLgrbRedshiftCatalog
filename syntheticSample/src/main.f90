program SampleSynthesis_prog

use, intrinsic :: iso_fortran_env, only: output_unit
use System_mod, only: CmdArg_type
use Constants_mod, only: IK, RK
use ParaPost_mod, only: ParaPost_type
use SyntheticRedshift_mod, only: SyntheticRedshift_type
use SyntheticSample_mod, only: generateSyntheticSample
!use JaggedArray_mod, only: CharVec_type

implicit none

integer(IK) , parameter         :: N_SFR_MODEL = 3
character(*), parameter         :: SFR_MODEL(N_SFR_MODEL) = [ "H06" , "L08" , "B10" ]
character(:), allocatable       :: StarFormationModel(:)
!type(CharVec_type), allocatable :: SynRedFilePath(:), ParaPostFilePath(:)
character(:), allocatable       :: SynRedFilePath(:), ParaPostFilePath(:), ChainFilePath(:)
character(:), allocatable       :: outPathBase, outPath
type(SyntheticRedshift_type)    :: SynRed
type(ParaPost_type)             :: ParaPost
type(CmdArg_type)               :: CmdArg
integer(IK)                     :: inFileUnit, imodel, sampleCount

namelist /InputData/ StarFormationModel, ChainFilePath, SynRedFilePath, ParaPostFilePath, outPathBase, sampleCount

! query input data file name from the command line
call CmdArg%query()
if (CmdArg%count/=1) then
    write(output_unit,"(*(g0))")
    write(output_unit,"(*(g0))") "FATAL: Invalid number of command-line arguments: ", CmdArg%count
    write(output_unit,"(*(g0))") "       Use the following example syntax to invoke the program: "
    write(output_unit,"(*(g0))") "       a.exe <input file path: ../in/SyntheticSample.nml>"
end if

! read simulation input data
open( newunit = inFileUnit, file = CmdArg%Arg(1)%record, status="old" )
    allocate(character(1000)    :: outPathBase)
    allocate(character(4)       :: StarFormationModel(N_SFR_MODEL))
    allocate( character(2047)   :: SynRedFilePath(N_SFR_MODEL), ChainFilePath(N_SFR_MODEL), ParaPostFilePath(N_SFR_MODEL) )
   !allocate(SynRedFilePath(N_SFR_MODEL), ParaPostFilePath(N_SFR_MODEL) )
   !do imodel = 1, N_SFR_MODEL
   !    allocate( character(2047) :: SynRedFilePath(imodel)%record, ParaPostFilePath(imodel)%record )
   !end do
    StarFormationModel = "NULL"
    read(inFileUnit,nml=InputData)
    outPathBase = trim(adjustl(outPathBase))
    !do imodel = 1, N_SFR_MODEL
    !    SynRedFilePath(imodel)%record    = trim(adjustl(SynRedFilePath(imodel)%record  ))
    !    ParaPostFilePath(imodel)%record  = trim(adjustl(ParaPostFilePath(imodel)%record))
    !end do
close(inFileUnit)

do imodel = 1, N_SFR_MODEL

    write(output_unit,"(*(g0))"); write(output_unit,"(*(g0))")
    write(output_unit,"(*(g0))") "Generating GRB redshift sample based on the SFR model of ", StarFormationModel(imodel)
    write(output_unit,"(*(g0))"); write(output_unit,"(*(g0))")

    ! estimate redshifts
    call execute_command_line("mkdir "//trim(adjustl(outPathBase)))
    outPath = outPathBase // "syntheticSample" // trim(adjustl(StarFormationModel(imodel))) // ".csv"
    if (trim(adjustl(StarFormationModel(imodel)))=="H06" .or. &
        trim(adjustl(StarFormationModel(imodel)))=="L08" .or. &
        trim(adjustl(StarFormationModel(imodel)))=="B10") then

        ! Read the LGRB world model parameters
        ParaPost = ParaPost_type( chainFilePath = trim(adjustl(ChainFilePath(imodel))) &
                                , paraPostFilePath = trim(adjustl(ParaPostFilePath(imodel))) )

        ! Read the synthetic redshifts
        SynRed = SyntheticRedshift_type( redshiftChainFilePath = trim(adjustl(SynRedFilePath(imodel))) )

        ! generate LGRB sample
        call generateSyntheticSample( ParaPost          = ParaPost          &
                                    , SynRed            = SynRed            &
                                    , outFilePath       = outPath           &
                                    , sampleCount       = sampleCount       &
                                    )

    else
        write(output_unit,"(*(g0))")
        write(output_unit,"(*(g0))") "FATAL: Invalid star formation model on input: ", trim(adjustl(StarFormationModel(imodel)))
        write(output_unit,"(*(g0))") "       Use the one of following three supported models: "
        write(output_unit,"(*(g0))") "       H06   L08   B10"
        write(output_unit,"(*(g0))")
    end if

end do

end program SampleSynthesis_prog
