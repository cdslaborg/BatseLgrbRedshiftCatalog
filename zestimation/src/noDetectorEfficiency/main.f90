program Zestimate_prog

use, intrinsic :: iso_fortran_env, only: output_unit
use System_mod, only: CmdArg_type
use Batse_mod, only: readDataGRB
use Zestimate_mod, only: zestimate
use Constants_mod, only: IK, RK
use ParaPost_mod, only: readParaPost

implicit none

integer(IK)                 :: i, j, inFileUnit, ngrb
character(:), allocatable   :: inputBatseDataFile, outputBatseDataFile, paraPostFilePath
type(CmdArg_type)           :: CmdArg

namelist /InputData/ ngrb, inputBatseDataFile, outputBatseDataFile, paraPostFilePath

! query input data file name from the command line
call CmdArg%query()
if (CmdArg%count/=2) then
    write(output_unit,"(*(g0))")
    write(output_unit,"(*(g0))") "FATAL: Invalid number of command-line arguments: ", CmdArg%count
    write(output_unit,"(*(g0))") "       Use the following example syntax to invoke the program: "
    write(output_unit,"(*(g0))") "       a.exe <input file path: ../in/WorldModelSimualtionLGRB.nml>"
end if

! read simulation input data
open( newunit = inFileUnit, file = CmdArg%Arg(1)%record//CmdArg%Arg(2)%record, status="old" )
    allocate(character(1000) :: inputBatseDataFile, outputBatseDataFile, paraPostFilePath)
    read(inFileUnit,nml=InputData)
close(inFileUnit)

! read observed grb input data
call readDataGRB(ngrb,inputBatseDataFile,outputBatseDataFile)

! read the posterior distribution of the parameters of the model
call readParaPost(paraPostFilePath)

! read the posterior distribution of the parameters of the model
call zestimate()

end program Zestimate_prog
