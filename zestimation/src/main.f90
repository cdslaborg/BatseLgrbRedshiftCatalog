program Zestimate_prog

use, intrinsic :: iso_fortran_env, only: output_unit
use System_mod, only: CmdArg_type
use Astro_mod, only: getLogSFRH06, getLogSFRL08, getLogSFRB10
use Batse_mod, only: readDataGRB
use Zestimate_mod, only: zestimate
use Constants_mod, only: IK, RK
use ParaPost_mod, only: getParaPost

implicit none

character(:), allocatable   :: inputBatseDataFile, outputBatseDataFile, chainFilePath
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
    write(output_unit,"(*(g0))") "       a.exe <input file path: ../in/WorldModelSimualtionLGRB.nml> <chain file path: path to the cosmic rate posterior PDF>"
end if

chainFilePath = CmdArg%Arg(3)%record

! read simulation input data
open( newunit = inFileUnit, file = CmdArg%Arg(1)%record//CmdArg%Arg(2)%record, status="old" )
    allocate(character(1000) :: inputBatseDataFile, outputBatseDataFile, outDir)
    read(inFileUnit,nml=InputData)
    outDir = trim(adjustl(outDir))
!#ifdef KFAC_ONETHIRD_ENABLED
!    outDir = trim(adjustl(outDir))//"kfacOneThird\"
!#else
!    outDir = trim(adjustl(outDir))//"kfacNone\"
!#endif
close(inFileUnit)

call execute_command_line("mkdir "//outDir)

! read observed grb input data
call readDataGRB(ngrb,inputBatseDataFile,outputBatseDataFile)

#ifdef H06
    rateModel = "H06"
#elif defined L08
    rateModel = "L08"
#elif defined B10
    rateModel = "B10"
#else
#error "Unknown SFR model in WorldModelForBatseLGRB_mod.f90"
#endif

write(output_unit,"(*(g0))"); write(output_unit,"(*(g0))"); write(output_unit,"(*(g0))")
write(output_unit,"(*(g0))") "Generating GRB redshift distributions based on the SFR model of "//rateModel
write(output_unit,"(*(g0))")

! read the posterior distribution of the parameters of the model
call getParaPost( chainFilePath = chainFilePath , paraPostFilePath = outDir//"paraPost"//rateModel//".csv" )

! estimate redshifts:
! I noticed that passing a procedure pointer instead of hardcoding the procedure in Zestimate_mod can change the code runtime
! from taking 49 sec, to 75 sec (second time only 58 sec) with O3 optimization.
!outPath = outDir !// rateModel // "\"
call zestimate(outDir)

end program Zestimate_prog
