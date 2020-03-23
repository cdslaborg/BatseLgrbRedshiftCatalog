program Zestimate_prog

use, intrinsic :: iso_fortran_env, only: output_unit
use System_mod, only: CmdArg_type
use Astro_mod, only: getLogSFRH06, getLogSFRL08, getLogSFRB10
use Batse_mod, only: readDataGRB
use Zestimate_mod, only: zestimate
use Constants_mod, only: IK, RK
use ParaPost_mod, only: readParaPost

implicit none

integer(IK) , parameter     :: N_SFR_MODEL = 3
character(*), parameter     :: SFR_MODEL(N_SFR_MODEL) = [ "H06" , "L08" , "B10" ]
character(:), allocatable   :: inputBatseDataFile, outputBatseDataFile, paraPostFileBase, StarFormationModel(:)
character(:), allocatable   :: outPathBase, outPath
type(CmdArg_type)           :: CmdArg
integer(IK)                 :: i, j, inFileUnit, ngrb, imodel

namelist /InputData/ ngrb, inputBatseDataFile, outputBatseDataFile, paraPostFileBase, StarFormationModel, outPathBase

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
    allocate(character(1000) :: inputBatseDataFile, outputBatseDataFile, paraPostFileBase, outPathBase)
    allocate(character(4) :: StarFormationModel(N_SFR_MODEL))
    StarFormationModel = "NULL"
    read(inFileUnit,nml=InputData)
    outPathBase = trim(adjustl(outPathBase))
    paraPostFileBase = trim(adjustl(paraPostFileBase))
close(inFileUnit)

! read observed grb input data
call readDataGRB(ngrb,inputBatseDataFile,outputBatseDataFile)

do imodel = 1, N_SFR_MODEL

    write(output_unit,"(*(g0))"); write(output_unit,"(*(g0))"); write(output_unit,"(*(g0))")
    write(output_unit,"(*(g0))") "Generating GRB redshift distributions based on the SFR model of ", StarFormationModel(imodel)
    write(output_unit,"(*(g0))")

    ! read the posterior distribution of the parameters of the model
    call readParaPost( paraPostFilePath = paraPostFileBase//trim(adjustl(StarFormationModel(imodel)))//".csv" )

    ! estimate redshifts
    ! I noticed that passing a procedure pointer instead of hardcoding the procedure in Zestimate_mod can change the code runtime
    ! from taking 49 sec, to 75 sec (second time only 58 sec) with O3 optimization.
    call execute_command_line("mkdir "//outPathBase)
    outPath = outPathBase // StarFormationModel(imodel) // "/"
    if (StarFormationModel(imodel)=="H06") then
        call zestimate(getLogSFRH06,outPath)
    elseif (StarFormationModel(imodel)=="L08") then
        call zestimate(getLogSFRL08,outPath)
    elseif (StarFormationModel(imodel)=="B10") then
        call zestimate(getLogSFRB10,outPath)
    else
        write(output_unit,"(*(g0))")
        write(output_unit,"(*(g0))") "FATAL: Invalid star formation model on input: ", trim(adjustl(StarFormationModel(imodel)))
        write(output_unit,"(*(g0))") "       Use the one of following three supported models: "
        write(output_unit,"(*(g0))") "       H06   L08   B10"
        write(output_unit,"(*(g0))")
    end if

end do

end program Zestimate_prog
