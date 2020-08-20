module SynRed_mod

    use Constants_mod, only: IK, RK

    implicit none

    character(*), parameter :: MODULE_NAME = "@SynRed_mod"

    type :: SynRedSample_type
        real(RK) :: z, logzplus1, logLisoLogPbolDiff, logEisoLogSbolDiff
    end type SynRedSample_type

    type :: SynRed_type
        integer(IK) :: count
        character(:), allocatable :: filePath
        type(SynRedSample_type), allocatable :: Sample(:)
    end type SynRed_type

    interface SynRed_type
        module procedure :: constructSynRed
    end interface

!***********************************************************************************************************************************
!***********************************************************************************************************************************

contains

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    function constructSynRed(sampleFilePath,sampleSize) result(SynRed)
        ! Parse the contents of the input ParaMonte::ParaDRAM sampleFile into the array of derived types SynRed, 
        ! each of which represents a redshift corresponding to one possible LGRB in the LGRB world model.
        !
        ! Input
        ! =====
        !
        !   sampleFilePath
        !       The path to the "*_sample.txt" file from the Synthetic Redshift simulations via the ParaMonte library's ParaDRAM() sampler.
        !
        ! Output
        ! ======
        !
        !   SynRed
        !       An array of SynRed_type derived types
        !
        ! All parameters are assumed to be read in log Neper (not log10) from the input file, wherever needed.
        use, intrinsic :: iso_fortran_env, only: output_unit
        use Cosmology_mod, only: LOGMPC2CMSQ4PI, getLogLumDisWicMpc
        use Constants_mod, only: IK, RK
        use String_mod, only: String_type
        implicit none

        character(*), intent(in)            :: sampleFilePath
        integer(IK), intent(in), optional   :: sampleSize
        type(SynRed_type)                   :: SynRed

        character(*), parameter             :: PROCEDURE_NAME = MODULE_NAME//"@getModelIntegral()"
        integer(IK)                         :: isample, sampleFileUnit
        real(RK)                            :: zplus1, twiceLogLumDisMpc
        type(String_type)                   :: Record

        SynRed%count = 10000; if (present(sampleSize)) SynRed%count = sampleSize
        SynRed%filePath = trim(adjustl(sampleFilePath))
        allocate(SynRed%Sample(SynRed%count))

        ! read the file contents

        open( newunit = sampleFileUnit &
            , file = SynRed%filePath &
            , status = "old" &
            )

        read(sampleFileUnit,*) ! header
        allocate(character(127) :: Record%value)
        write(*,"(*(g0,' '))", advance = "no") " reading file:", SynRed%filePath, "..."
        write(*,"(*(g0,:,' '))", advance = "yes") "mission accomplished."
        do isample = 1, SynRed%count
            read(sampleFileUnit,"(A)") Record%value
            Record%Parts = Record%splitStr  ( string = Record%value &
                                            , delimiter = "," &
                                            , nPart = Record%nPart &
                                            )
            SynRed%Sample(isample)%z = Record%str2real64(Record%Parts(2)%record)
            zplus1 = SynRed%Sample(isample)%z + 1._RK
            twiceLogLumDisMpc = 2 * getLogLumDisWicMpc(zplus1)
            SynRed%Sample(isample)%logzplus1           = log(zplus1)
            SynRed%Sample(isample)%logLisoLogPbolDiff  = LOGMPC2CMSQ4PI + twiceLogLumDisMpc
            SynRed%Sample(isample)%logEisoLogSbolDiff  = LOGMPC2CMSQ4PI + twiceLogLumDisMpc - SynRed%Sample(isample)%logzplus1
        end do

    end function constructSynRed

!***********************************************************************************************************************************
!***********************************************************************************************************************************

end module SynRed_mod