program SampleSynthesis_prog

use, intrinsic :: iso_fortran_env, only: output_unit
use System_mod, only: CmdArg_type
use Constants_mod, only: IK, RK
use ParaDRAM_mod, only: runParaDRAM

implicit none

integer(IK) , parameter     :: N_SFR_MODEL = 4
character(*), parameter     :: SFR_MODEL(N_SFR_MODEL) = [ "H06" , "L08" , "B10", "M14" ]
character(:), allocatable   :: StarFormationModel(:)
character(:), allocatable   :: outPathBase  !, outPath
type(CmdArg_type)           :: CmdArg
integer(IK)                 :: i, j, inFileUnit, imodel

namelist /InputData/ StarFormationModel, outPathBase

! query input data file name from the command line
call CmdArg%query()
if (CmdArg%count/=2) then
    write(output_unit,"(*(g0))")
    write(output_unit,"(*(g0))") "FATAL: Invalid number of command-line arguments: ", CmdArg%count
    write(output_unit,"(*(g0))") "       Use the following example syntax to invoke the program: "
    write(output_unit,"(*(g0))") "       a.exe <input file path: ../in/> <input file name: SynthesizeRedshift.nml>"
end if

! read simulation input data
open( newunit = inFileUnit, file = CmdArg%Arg(1)%record//CmdArg%Arg(2)%record, status="old" )
    allocate(character(1000) :: outPathBase)
    allocate(character(4) :: StarFormationModel(N_SFR_MODEL))
    StarFormationModel = "NUL"
    read(inFileUnit,nml=InputData)
#ifdef SFR_DENSITY_ENABLED
    outPathBase = trim(adjustl(outPathBase)) // "StarFormationRateDensity"
#else
    outPathBase = trim(adjustl(outPathBase)) // "StarFormationRate"
#endif
close(inFileUnit)

do imodel = 1, N_SFR_MODEL

    write(output_unit,"(*(g0))"); write(output_unit,"(*(g0))")
    write(output_unit,"(*(g0))") "Generating GRB redshift sample based on the SFR model of ", StarFormationModel(imodel)
    write(output_unit,"(*(g0))"); write(output_unit,"(*(g0))")

    ! estimate redshifts
    !outPath = outPathBase // StarFormationModel(imodel) // "/"
    if (StarFormationModel(imodel)=="H06") then

        call runParaDRAM( ndim          = 1_IK &
                        , getLogFunc    = wrapLogRateH06 &
                        , inputFile     = CmdArg%Arg(1)%record//CmdArg%Arg(2)%record &
                        , fileBase      = outPathBase // StarFormationModel(imodel) &
                        )

    elseif (StarFormationModel(imodel)=="L08") then

        call runParaDRAM( ndim          = 1_IK &
                        , getLogFunc    = wrapLogRateL08 &
                        , inputFile     = CmdArg%Arg(1)%record//CmdArg%Arg(2)%record &
                        , fileBase      = outPathBase // StarFormationModel(imodel) &
                        )

    elseif (StarFormationModel(imodel)=="B10") then

        call runParaDRAM( ndim          = 1_IK &
                        , getLogFunc    = wrapLogRateB10 &
                        , inputFile     = CmdArg%Arg(1)%record//CmdArg%Arg(2)%record &
                        , fileBase      = outPathBase // StarFormationModel(imodel) &
                        )

    elseif (StarFormationModel(imodel)=="M14") then

        call runParaDRAM( ndim          = 1_IK &
                        , getLogFunc    = wrapLogRateM14 &
                        , inputFile     = CmdArg%Arg(1)%record//CmdArg%Arg(2)%record &
                        , fileBase      = outPathBase // StarFormationModel(imodel) &
                        )

    else
        write(output_unit,"(*(g0))")
        write(output_unit,"(*(g0))") "FATAL: Invalid star formation model on input: ", trim(adjustl(StarFormationModel(imodel)))
        write(output_unit,"(*(g0))") "       Use the one of following three supported models: "
        write(output_unit,"(*(g0))") "       H06   L08   B10 M14"
        write(output_unit,"(*(g0))")
    end if

end do

!***********************************************************************************************************************************
!***********************************************************************************************************************************

contains

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    pure function wrapLogRateH06(nd,Redshift) result(logRate)
        use Cosmology_mod, only: getLogLumDisWicMpc
#ifdef SFR_DENSITY_ENABLED
        use StarFormation_mod, only: getLogRate => getLogDensitySFRH06
#else
        use StarFormation_mod, only: getLogRate => getLogSFRH06
#endif
        implicit none
        integer(IK), intent(in) :: nd
        real(RK), intent(in)    :: Redshift(nd)
        real(RK)                :: logRate, zplus1
        zplus1 = Redshift(1) + 1._RK
        logRate = getLogRate( logzplus1         = log(zplus1)   &
#ifndef SFR_DENSITY_ENABLED
                            , zplus1            = zplus1        &
                            , twiceLogLumDisMpc = 2 * getLogLumDisWicMpc(zplus1) &
#endif
                            )
    end function wrapLogRateH06

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    pure function wrapLogRateL08(nd,Redshift) result(logRate)
        use Cosmology_mod, only: getLogLumDisWicMpc
#ifdef SFR_DENSITY_ENABLED
        use StarFormation_mod, only: getLogRate => getLogRateDensityL08
#else
        use StarFormation_mod, only: getLogRate => getLogRateL08
#endif
        implicit none
        integer(IK), intent(in) :: nd
        real(RK), intent(in)    :: Redshift(nd)
        real(RK)                :: logRate, zplus1
        zplus1 = Redshift(1) + 1._RK
        logRate = getLogRate( logzplus1         = log(zplus1)   &
#ifndef SFR_DENSITY_ENABLED
                            , zplus1            = zplus1        &
                            , twiceLogLumDisMpc = 2 * getLogLumDisWicMpc(zplus1) &
#endif
                            )
    end function wrapLogRateL08

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    pure function wrapLogRateB10(nd,Redshift) result(logRate)
        use Cosmology_mod, only: getLogLumDisWicMpc
#ifdef SFR_DENSITY_ENABLED
        use StarFormation_mod, only: getLogRate => getLogDensitySFRB10
#else
        use StarFormation_mod, only: getLogRate => getLogSFRB10
#endif
        implicit none
        integer(IK), intent(in) :: nd
        real(RK), intent(in)    :: Redshift(nd)
        real(RK)                :: logRate, zplus1
        zplus1 = Redshift(1) + 1._RK
        logRate = getLogRate( logzplus1         = log(zplus1)   &
#ifndef SFR_DENSITY_ENABLED
                            , zplus1            = zplus1        &
                            , twiceLogLumDisMpc = 2 * getLogLumDisWicMpc(zplus1) &
#endif
                            )
    end function wrapLogRateB10

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    pure function wrapLogRateM14(nd,Redshift) result(logRate)
        use Cosmology_mod, only: getLogLumDisWicMpc
#ifdef SFR_DENSITY_ENABLED
        use StarFormation_mod, only: getLogRate => getLogDensitySFRM14
#else
        use StarFormation_mod, only: getLogRate => getLogSFRM14
#endif
        implicit none
        integer(IK), intent(in) :: nd
        real(RK), intent(in)    :: Redshift(nd)
        real(RK)                :: logRate, zplus1
        zplus1 = Redshift(1) + 1._RK
        logRate = getLogRate( logzplus1         = log(zplus1)   &
                            , zplus1            = zplus1        &
#ifndef SFR_DENSITY_ENABLED
                            , twiceLogLumDisMpc = 2 * getLogLumDisWicMpc(zplus1) &
#endif
                            )
    end function wrapLogRateM14

!***********************************************************************************************************************************
!***********************************************************************************************************************************

end program SampleSynthesis_prog
