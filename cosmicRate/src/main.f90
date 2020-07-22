program batseWorldModelSimualtion

use, intrinsic :: iso_fortran_env, only: output_unit
use Batse_mod, only: readDataGRB
use System_mod, only: CmdArg_type
use Constants_mod, only: IK, RK
use BatseLgrbWorldModel_mod, only: NPAR, getLogPostProb
use BatseLgrbWorldModel_mod, only: zoneMin, zoneMax
use BatseLgrbWorldModel_mod, only: zoneTol, lisoTol, epkzTol
use BatseLgrbWorldModel_mod, only: zoneRef, lisoRef, epkzRef

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
    write(output_unit,"(*(g0))") "       a.exe <input file path: ./in/> <input file name: WorldModelSimualtionLGRB.nml>"
end if

! read simulation input data

open( newunit = inFileUnit, file = CmdArg%Arg(1)%record//CmdArg%Arg(2)%record, status="old" )
    allocate(character(1000) :: inputBatseDataFile, outputBatseDataFile)
    read(inFileUnit,nml=InputData)
    inputBatseDataFile  = CmdArg%Arg(1)%record//trim(adjustl(inputBatseDataFile))
    outputBatseDataFile = CmdArg%Arg(1)%record//trim(adjustl(outputBatseDataFile))
    !write(*,*) "zoneTol: ", zoneTol
    !write(*,*) "lisoTol: ", lisoTol
    !write(*,*) "epkzTol: ", epkzTol
close(inFileUnit)

! report kfac correction type

#ifdef kfacOneThird
write(*,"(*(g0,:,','))") "ATTN: assuming kfacOneThird k-correction..."
#else
write(*,"(*(g0,:,','))") "ATTN: assuming NO k-correction..."
#endif

! read observed grb input data

call readDataGRB( inputBatseDataFile    &
                , outputBatseDataFile   &
                , isLgrb = .true.       &
                )

#ifdef ERR_ESTIMATION_ENABLED

blockErr: block
    use Path_mod, only: Path_type
    use Timer_mod, only: Timer_type
    use String_mod, only: num2str, String_type
    use BatseLgrbWorldModel_mod, only: NPAR, getLogPostProb
    use BatseLgrbWorldModel_mod, only: zone_relerr, zgrb_relerr, liso_relerr, epkz_relerr
    use BatseLgrbWorldModel_mod, only: zone_neval, zgrb_neval, liso_neval, epkz_neval
    !use BatseLgrbWorldModel_mod, only: zgrb_count, liso_count, epkz_count
    type(Path_type) :: Path
    type(Timer_type) :: Timer
    type(String_type) :: String
    integer(IK) :: errFileUnit, paramsFileUnit, isample, ipart
    integer(IK), parameter :: NSAMPLE = 1000_IK
    real(RK) :: logPostProb
    real(RK) :: Params(NPAR)
    character(:), allocatable :: errEstimationFileName
    call Timer%tic()
    Path = Path_type("./out/")
    Path%Err = Path%mkdir(Path%modified)
    errEstimationFileName = Path%original // "errEst" // &
                            "_zoneTol_" // num2str(zoneRef) // "_" // num2str(zoneTol,"(g0.1)") // &
                            "_lisoTol_" // num2str(lisoRef) // "_" // num2str(lisoTol,"(g0.1)") // &
                            "_epkzTol_" // num2str(epkzRef) // "_" // num2str(epkzTol,"(g0.1)") // &
                            ".txt"
    write(*,"(*(g0,:,' '))")
    write(*,"(*(g0,:,' '))") "errEstimationFileName:", errEstimationFileName
    write(*,"(*(g0,:,' '))")
    open( newunit = errFileUnit, file = errEstimationFileName, status="replace" )
    open( newunit = paramsFileUnit, file = CmdArg%Arg(1)%record//"errEsimationParams.txt", status="old" )
    ! read the test params
    read(paramsFileUnit,*)
    allocate(character(512) :: String%value)
    write(errFileUnit,"(*(g0.4,:,','))" ) "time", "logPostProb" &
                                        , "zgrb_neval", "zgrb_relerr" &
                                        , "zone_neval", "zone_relerr" &
                                        , "liso_neval", "liso_relerr" &
                                        , "epkz_neval", "epkz_relerr"
    do isample = 1, NSAMPLE
        read(paramsFileUnit,"(A)") String%value
        String%Parts = String%splitStr(String%value,",",String%nPart)
        do ipart = 2, String%nPart
            Params(ipart-1) = String%str2real(String%Parts(ipart)%record)
            !write(*,"(*(g0,:,','))") String%Parts(ipart)%record, String%str2real(String%Parts(ipart)%record)
        end do
        if (mod(isample,100)==0) write(*,"(*(g0,:,','))") isample!,logPostProb,Params(1:NPAR)
        call Timer%toc()
        logPostProb = getLogPostProb(NPAR,Params(1:NPAR))
        call Timer%toc()
        write(errFileUnit,"(*(g0.4,:,','))" ) Timer%Time%delta, logPostProb &
                                            , nint(zgrb_neval), zgrb_relerr &
                                            , nint(zone_neval), zone_relerr &
                                            , nint(liso_neval), liso_relerr &
                                            , nint(epkz_neval), epkz_relerr
    end do
    write(*,"(*(g0,:,','))")
    close(paramsFileUnit)
    close(errFileUnit)
end block blockErr

#else

! sample the WoldModel's parameters

blockSampling: block
    use ParaMonte, only: ParaDRAM
    type(ParaDRAM) :: PD
    call PD%runSampler  ( ndim = NPAR &
                        , getLogFunc = getLogPostProb &
                        , inputFile = CmdArg%Arg(1)%record//CmdArg%Arg(2)%record &
                        )
end block blockSampling

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

#endif

end program batseWorldModelSimualtion
