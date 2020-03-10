program batseWorldModelSimualtion

use, intrinsic :: iso_fortran_env, only: output_unit
use System_mod, only: CmdArg_type
use Batse_mod, only: readDataGRB
use BatseLgrbWorldModel_mod, only: NPAR, getLogPostProb
use BatseLgrbWorldModel_mod, only: zoneMin, zoneMax
use BatseLgrbWorldModel_mod, only: zoneTol, lisoTol, epkzTol
use BatseLgrbWorldModel_mod, only: zoneRef, lisoRef, epkzRef
use Constants_mod, only: IK, RK

implicit none

integer(IK)                 :: i, j, inFileUnit, ngrb
character(:), allocatable   :: inputBatseDataFile, outputBatseDataFile
type(CmdArg_type)           :: CmdArg

namelist /InputData/ ngrb, inputBatseDataFile, outputBatseDataFile
namelist /InputData/ zoneMin, zoneMax
namelist /InputData/ zoneTol, lisoTol, epkzTol
namelist /InputData/ zoneRef, lisoRef, epkzRef

! query input data file name from the command line

call CmdArg%query()
if (CmdArg%count/=2) then
    write(output_unit,"(*(g0))")
    write(output_unit,"(*(g0))") "FATAL: Invalid number of command-line arguments: ", CmdArg%count
    write(output_unit,"(*(g0))") "       Use the following example syntax to invoke the program: "
    write(output_unit,"(*(g0))") "       a.exe <input file path: ../in/> <input file name: WorldModelSimualtionLGRB.nml>"
end if

! read simulation input data

open( newunit = inFileUnit, file = CmdArg%Arg(1)%record//CmdArg%Arg(2)%record, status="old" )
    allocate(character(1000) :: inputBatseDataFile, outputBatseDataFile)
    read(inFileUnit,nml=InputData)
    inputBatseDataFile  = CmdArg%Arg(1)%record//trim(adjustl(inputBatseDataFile))
    outputBatseDataFile = CmdArg%Arg(1)%record//trim(adjustl(outputBatseDataFile))
    write(*,*) "zoneTol: ", zoneTol
    write(*,*) "lisoTol: ", lisoTol
    write(*,*) "epkzTol: ", epkzTol
close(inFileUnit)

!***********************************************************************************************************************************
!***********************************************************************************************************************************

!blockRefinedSample: block
!
!    use ParaDRAMRefinedSample_mod, only: RefinedSample_type
!    use ParaDRAMChainFileContents_mod, only: ChainFileContents_type
!
!    type(ChainFileContents_type)    :: ChainFileContents
!    type(RefinedSample_type)        :: RefinedSample
!    integer(IK)                     :: fileUnit, irow
!    character(:), allocatable       :: chainFilePath
!   !integer(IK)                     :: refinedSampleSize, iweight
!
!    ! reread the generated chain file for further analysis
!    chainFilePath = "../out/withKfac/HB/ParaDRAM_run_20181231_010635.719_image_1_chain.txt"
!    call ChainFileContents%get  ( chainFilePath = chainFilePath &
!                                , chainSize     = 1000_IK &
!                                , ndim          = 16_IK &
!                                )
!    if (ChainFileContents%Err%occurred) then
!        write(*,*) ChainFileContents%Err%msg
!        error stop
!    end if
!
!    write(*,*) "chainSize: ", ChainFileContents%chainSize
!    open(newunit=fileUnit, file = "testChainContents.txt", status="replace")
!    do irow = 1,ChainFileContents%chainSize
!        write(fileUnit,"(I25,2E25.8,2I25,*(E25.8))" ) ChainFileContents%ProcessorID(irow)          &
!                                                    , ChainFileContents%MeanAcceptanceRate(irow)   &
!                                                    , ChainFileContents%AdaptationMeasure(irow)    &
!                                                    , ChainFileContents%BurninLocation(irow)       &
!                                                    , ChainFileContents%SampleWeight(irow)         &
!                                                    , ChainFileContents%SampleLogFunc(irow)        &
!                                                    , ChainFileContents%SampleVariable(:,irow)
!    end do
!
!    call RefinedSample%get  ( chainFilePath = chainFilePath &
!                            , chainSize     = 1000_IK &
!                            , ndim          = 16_IK &
!                            , burninLoc     = 7_IK &
!                            )
!    if (RefinedSample%Err%occurred) then
!        write(*,*) RefinedSample%Err%msg
!        error stop
!    end if
!
!end block blockRefinedSample

!***********************************************************************************************************************************
!***********************************************************************************************************************************

! read observed grb input data

call readDataGRB( inputBatseDataFile    &
                , outputBatseDataFile   &
                , isLgrb = .true.       &
                )

! sample the WoldModel's parameters

blockSampling: block
    use ParaMonte, only: ParaDRAM
    type(ParaDRAM) :: pd
    call pd%runSampler  ( ndim = NPAR &
                        , getLogFunc = getLogPostProb &
                        , inputFile = CmdArg%Arg(1)%record//CmdArg%Arg(2)%record &
                        )
end block blockSampling

end program batseWorldModelSimualtion
