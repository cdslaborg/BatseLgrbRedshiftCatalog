module SyntheticRedshift_mod

    use Err_mod, only: Err_type
    use Constants_mod, only: IK, RK

    implicit none

    character(*), parameter :: MODULE_NAME = "@SyntheticRedshift_mod"

    type :: RedshiftSample_type
        real(RK) :: z, logzplus1, logLisoLogPbolDiff, logEisoLogSbolDiff
    end type RedshiftSample_type

    type :: SyntheticRedshift_type
        integer(IK) :: count
        character(:), allocatable :: filePath
        type(Err_type) :: Err
        type(RedshiftSample_type), allocatable :: Sample(:)
    end type SyntheticRedshift_type

    interface SyntheticRedshift_type
        module procedure :: constructSyntheticRedshift
    end interface

!***********************************************************************************************************************************
!***********************************************************************************************************************************

contains

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    ! All parameters are assumed to be in log Neper (not log10) wherever needed.
    function constructSyntheticRedshift(redshiftChainFilePath) result(SynRed)

       !use, intrinsic :: iso_fortran_env, only: output_unit
        use ParaDRAMChainFileContents_mod, only: ChainFileContents_type
        use Astro_mod, only: LOGMPC2CMSQ4PI, getLogLumDisWicMpc
        use Constants_mod, only: IK, RK
        implicit none

        character(*), intent(in)                        :: redshiftChainFilePath
        type(SyntheticRedshift_type)                    :: SynRed

        character(*), parameter                         :: PROCEDURE_NAME = MODULE_NAME//"@getModelIntegral()"
        integer(IK)                                     :: isample
        real(RK)                                        :: zplus1, twiceLogLumDisMpc
        type(ChainFileContents_type)                    :: ChainFileContents

        SynRed%filePath = trim(adjustl(redshiftChainFilePath))
        call ChainFileContents%get(SynRed%filePath)
        if (ChainFileContents%Err%occurred) then
            ChainFileContents%Err%msg = PROCEDURE_NAME//ChainFileContents%Err%msg
            return
        end if

        SynRed%count = ChainFileContents%chainSize
        if (allocated(SynRed%Sample)) deallocate(SynRed%Sample)
        allocate(SynRed%Sample(SynRed%count))
        do isample = 1,SynRed%count
            SynRed%Sample(isample)%z = ChainFileContents%SampleVariable(1,isample)
            zplus1 = SynRed%Sample(isample)%z + 1._RK
            twiceLogLumDisMpc = 2 * getLogLumDisWicMpc(zplus1)
            SynRed%Sample(isample)%logzplus1           = log(zplus1)
            SynRed%Sample(isample)%logLisoLogPbolDiff  = LOGMPC2CMSQ4PI + twiceLogLumDisMpc
            SynRed%Sample(isample)%logEisoLogSbolDiff  = LOGMPC2CMSQ4PI + twiceLogLumDisMpc - SynRed%Sample(isample)%logzplus1
        end do

    end function constructSyntheticRedshift

!***********************************************************************************************************************************
!***********************************************************************************************************************************

end module SyntheticRedshift_mod