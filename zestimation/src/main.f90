program Zestimate_prog

use, intrinsic :: iso_fortran_env, only: output_unit
use StarFormation_mod, only: getLogRateH06, getLogRateL08, getLogRateB10, getLogRateM14, getLogRateM17, getLogRateF18
use Zestimate_mod, only: zestimate
use Constants_mod, only: IK, RK
use ParaPost_mod, only: getParaPost
use System_mod, only: CmdArg_type
use Batse_mod, only: readDataGRB

implicit none

character(:), allocatable   :: inputBatseDataFile, outputBatseDataFile, sampleFilePath
character(:), allocatable   :: outDir, rateModel
type(CmdArg_type)           :: CmdArg
integer(IK)                 :: inFileUnit, ngrb

namelist /InputData/ ngrb, inputBatseDataFile, outputBatseDataFile, outDir

! query input data file name from the command line

call CmdArg%query()
if (CmdArg%count/=3) then
    write(output_unit,"(*(g0))")
    write(output_unit,"(*(g0))") "FATAL: Invalid number of command-line arguments: ", CmdArg%count
    write(output_unit,"(*(g0))") "       Use the following example syntax to invoke the program: "
    write(output_unit,"(*(g0))") "       a.exe <input file path: ../in/WorldModelSimualtionLGRB.nml> <sample file path: path to the cosmic rate posterior PDF>"
end if

sampleFilePath = CmdArg%Arg(3)%record

! read the simulation input data

open( newunit = inFileUnit, file = CmdArg%Arg(1)%record//CmdArg%Arg(2)%record, status="old" )
    allocate(character(1000) :: inputBatseDataFile, outputBatseDataFile, outDir)
    read(inFileUnit,nml=InputData)
    outDir = trim(adjustl(outDir))
close(inFileUnit)

call execute_command_line("mkdir "//outDir)

! read observed grb input data

call readDataGRB( inFilePath    = inputBatseDataFile    &
                , outFilePath   = outputBatseDataFile   &
                , isLgrb        = .true.                &
                )

#ifdef H06
    rateModel = "H06"
#elif defined L08
    rateModel = "L08"
#elif defined B10
    rateModel = "B10"
#elif defined M14
    rateModel = "M14"
#elif defined M17
    rateModel = "M17"
#elif defined F18
    rateModel = "F18"
#elif defined P15
    rateModel = "P15"
#else
#error "Unknown SFR model in WorldModelForBatseLGRB_mod.f90"
#endif

write(output_unit,"(*(g0))"); write(output_unit,"(*(g0))"); write(output_unit,"(*(g0))")
write(output_unit,"(*(g0))") "Generating GRB redshift distributions based on the SFR model of "//rateModel
write(output_unit,"(*(g0))")

! read the posterior distribution of the parameters of the model

call getParaPost( sampleFilePath = sampleFilePath )

! estimate redshifts:
! I noticed that passing a procedure pointer instead of hardcoding the procedure in Zestimate_mod can change the code runtime
! from taking 49 sec, to 75 sec (second time only 58 sec) with O3 optimization.
!outPath = outDir !// rateModel // "\"
call zestimate(outDir)

end program Zestimate_prog
