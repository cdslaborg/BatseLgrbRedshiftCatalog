module ParaPost_mod

    use Constants_mod, only: IK, RK, PI

    implicit none

    character(*), parameter :: MODULE_NAME = "@ParaPost_mod"

    integer(IK) , parameter :: NSAMPLE = 2500_IK
    integer(IK) , parameter :: NVAR = 4     ! number of GRB attributes used in the world model
    integer(IK) , parameter :: NPAR = 16    ! number of world model's parameters

    ! BATSE threshold parameters
    type :: Thresh_type
        real(RK) :: avg, invStdSqrt2
    end type Thresh_type

    ! Posterior sample
    type :: ParaPostSample_type
        ! order of variables: logLiso, logEpkz, logEiso, logT90z
        type(Thresh_type) :: Thresh
        real(RK), allocatable :: Avg(:), CholFacLower(:,:), CholFacDiag(:)
    end type ParaPostSample_type

    type :: ParaPost_type
        integer(IK) :: count, ndim
        character(:), allocatable :: filePath
        type(ParaPostSample_type), allocatable :: Sample(:)
    end type ParaPost_type

    interface ParaPost_type
        module procedure :: constructParaPost
    end interface

!***********************************************************************************************************************************
!***********************************************************************************************************************************

contains

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    ! All parameters are assumed to be read in log Neper (not log10) from the input file, wherever needed.
    function constructParaPost(chainFilePath,paraPostFilePath) result(ParaPost)

        use, intrinsic :: iso_fortran_env, only: output_unit
        use Constants_mod, only: IK, RK, SQRT2, SQRT2PI, CARRIAGE_RETURN, CLOCK_TICK
        use ParaDRAMRefinedSample_mod, only: RefinedSample_type
        use Matrix_mod, only: getCholeskyFactor
        use Timer_mod, only: Timer_type
        implicit none

        character(*), intent(in)            :: chainFilePath, paraPostFilePath
        type(ParaPost_type)                 :: ParaPost

        character(*), parameter             :: PROCEDURE_NAME = MODULE_NAME//"@getModelIntegral()"

        integer(IK)                         :: i, j, paraPostFileUnit, isample, itick   !, ierr
        integer(IK)                         :: iweight, irefined, logFuncOffset
        real(RK), parameter                 :: INV_SQRT2 = 1._RK / SQRT2
        type(Timer_type)                    :: Timer
        type(RefinedSample_type)            :: Refined

        ParaPost%ndim = NVAR
        ParaPost%count = NSAMPLE
        ParaPost%filePath = trim(adjustl(paraPostFilePath))
        if (allocated(ParaPost%Sample)) deallocate(ParaPost%Sample)
        allocate(ParaPost%Sample(ParaPost%count))

        ! get chain file contents
        call Refined%get( chainFilePath = chainFilePath &
                        , refinedSampleSize = ParaPost%count &
                        )

        write(output_unit,"(*(g0))")

        itick = 0
        call Timer%tic()

        open(newunit=paraPostFileUnit,file=ParaPost%filePath,status="replace")

        isample = 0_IK
        do irefined = 1, Refined%count
            do iweight = 1, Refined%Weight(irefined)

                isample = isample + 1

                if (isample>ParaPost%count) then
                    write(*,*) "Something fishy going on here. program aborted..."
                    error stop
                end if

                write(paraPostFileUnit,"(*(g0.8,:,','))") Refined%State(:,irefined)
                !write(paraPostFileUnit,"(*(g0.8,:,','))")  ParaPost%Sample(isample)%Avg(1:ParaPost%ndim) &
                !                                        , (ParaPost%Sample(isample)%CholFacLower(i,i),i=1,ParaPost%ndim) &  ! log of standard deviations
                !                                        ,  ParaPost%Sample(isample)%CholFacLower(2:4,1) &                   ! Rho: LisoEpkz, LisoEiso, LisoT90z
                !                                        ,  ParaPost%Sample(isample)%CholFacLower(3:4,2) &                   ! Rho: EpkzEiso, EpkzT90z
                !                                        ,  ParaPost%Sample(isample)%CholFacLower(4:4,3) &                   ! Rho: EisoT90z
                !                                        ,  ParaPost%Sample(isample)%Thresh%avg &                            ! log of threshold mean
                !                                        ,  ParaPost%Sample(isample)%Thresh%invStdSqrt2                      ! log of threshold standard deviation

                !*******************************************************************************************************************
                !****                                         begin reading sample data                                         ****
                !*******************************************************************************************************************

                ! now read ParaPost sample properties
                allocate(ParaPost%Sample(isample)%Avg(ParaPost%ndim))
                allocate(ParaPost%Sample(isample)%CholFacLower(ParaPost%ndim,ParaPost%ndim))
                allocate(ParaPost%Sample(isample)%CholFacDiag(ParaPost%ndim))

                ! NOTE: the extra one in all vector indices of Refined%State compensates for the logFunc column
                ! mean vector of the LGRB variables' distribution
                logFuncOffset = 1_IK
                ParaPost%Sample(isample)%Avg(1:ParaPost%ndim) = Refined%State(logFuncOffset+1:logFuncOffset+ParaPost%ndim,irefined)

                ! log of standard deviations
                do i = 1,ParaPost%ndim
                    ParaPost%Sample(isample)%CholFacLower(i,i) = Refined%State(logFuncOffset+i+ParaPost%ndim,irefined)
                end do

                ! Rho: LisoEpkz, LisoEiso, LisoT90z
                ParaPost%Sample(isample)%CholFacLower(2:4,1) = Refined%State(logFuncOffset+1+2*ParaPost%ndim:logFuncOffset+3+2*ParaPost%ndim,irefined)

                ! Rho: EpkzEiso, EpkzT90z
                ParaPost%Sample(isample)%CholFacLower(3:4,2) = Refined%State(logFuncOffset+4+2*ParaPost%ndim:logFuncOffset+5+2*ParaPost%ndim,irefined)

                ! Rho: EisoT90z
                ParaPost%Sample(isample)%CholFacLower(4:4,3) = Refined%State(logFuncOffset+6+2*ParaPost%ndim:logFuncOffset+6+2*ParaPost%ndim,irefined)

                ! log of threshold mean
                ParaPost%Sample(isample)%Thresh%avg = Refined%State(logFuncOffset+15,irefined)

                ! log of threshold standard deviation
                ParaPost%Sample(isample)%Thresh%invStdSqrt2 = Refined%State(logFuncOffset+16,irefined)

                !*******************************************************************************************************************
                !****                             begin transform of the sample to the original scales                          ****
                !*******************************************************************************************************************

                ! threshold parameters
                ParaPost%Sample(isample)%Thresh%invStdSqrt2 = INV_SQRT2 / exp(ParaPost%Sample(isample)%Thresh%invStdSqrt2)

                ! compute the standard deviations
                do i = 1, ParaPost%ndim
                    ParaPost%Sample(isample)%CholFacLower(i,i) = exp(ParaPost%Sample(isample)%CholFacLower(i,i))
                end do

                ! compute the covariance matrix's upper-triangle elements (excluding CholFacDiag variance elements)
                do i = 1, ParaPost%ndim
                    do j = i+1,ParaPost%ndim

                        ! convert the lower-triangle elements to correlation coefficients
                        ParaPost%Sample(isample)%CholFacLower(j,i) = tanh(ParaPost%Sample(isample)%CholFacLower(j,i))

                        ! compute the upper-triangle elements of the covariance matrix
                        ParaPost%Sample(isample)%CholFacLower(i,j)  = ParaPost%Sample(isample)%CholFacLower(j,i) &
                                                                    * ParaPost%Sample(isample)%CholFacLower(i,i) &
                                                                    * ParaPost%Sample(isample)%CholFacLower(j,j)

                    end do
                end do

                ! now convert the standard deviations to variances
                do i = 1, ParaPost%ndim
                    ParaPost%Sample(isample)%CholFacLower(i,i) = ParaPost%Sample(isample)%CholFacLower(i,i)**2  ! This is now squared standard deviation
                end do

                ! get the inverse covariance matrix and determinant
                call getCholeskyFactor  ( nd = ParaPost%ndim &
                                        , PosDefMat = ParaPost%Sample(isample)%CholFacLower &
                                        , Diagonal  = ParaPost%Sample(isample)%CholFacDiag &
                                        )
                if (ParaPost%Sample(isample)%CholFacDiag(1)<0._RK) then
                    write(*,*) "FATAL: Covariance Matrix to corresponding to the parameter sample ", isample, "not positive-definite."
                    error stop
                end if

                !*******************************************************************************************************************
                !****                             end transform of the sample to the original scales                            ****
                !*******************************************************************************************************************

                call Timer%toc()
                itick = itick + 1
                write(output_unit,"(*(' ',g0))", advance="no"   ) CARRIAGE_RETURN, CLOCK_TICK(itick) &
                                                                , isample, " out of  ", ParaPost%count &
                                                                , "parameter posterior samples processed in" &
                                                                , Timer%Time%total, "seconds."
                flush(output_unit)
                if (itick==4) itick = 0

            end do
        end do
        write(output_unit,"(*(g0))")
        close(paraPostFileUnit)

    end function constructParaPost


!***********************************************************************************************************************************
!***********************************************************************************************************************************

end module ParaPost_mod