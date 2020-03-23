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
    function constructParaPost(paraPostFilePath) result(ParaPost)

        use, intrinsic :: iso_fortran_env, only: output_unit
        use Timer_mod, only: Timer_type
        use Constants_mod, only: IK, RK, SQRT2, SQRT2PI, CARRIAGE_RETURN, CLOCK_TICK
        use Matrix_mod, only: getCholeskyFactor
        implicit none

        character(*), intent(in)            :: paraPostFilePath
        type(ParaPost_type)                 :: ParaPost

        character(*), parameter             :: PROCEDURE_NAME = MODULE_NAME//"@getModelIntegral()"

        integer(IK)                         :: i, j, paraPostFileUnit, isample, ierr, itick
        real(RK), parameter                 :: INV_SQRT2 = 1._RK / SQRT2
        real(RK)                            :: logFunc
        type(Timer_type)                    :: Timer

        ParaPost%ndim = NVAR
        ParaPost%count = NSAMPLE
        ParaPost%filePath = trim(adjustl(paraPostFilePath))

        if (allocated(ParaPost%Sample)) deallocate(ParaPost%Sample)
        allocate(ParaPost%Sample(ParaPost%count))
        !if (allocated(ParaPost%Sample%Avg)) deallocate(ParaPost%Sample%Avg)
        !if (allocated(ParaPost%Sample%CholFacLower)) deallocate(ParaPost%Sample%CholFacLower)
        !if (allocated(ParaPost%Sample%CholFacDiag)) deallocate(ParaPost%Sample%CholFacDiag)

        open(newunit=paraPostFileUnit,file=ParaPost%filePath,status="old")
        read(paraPostFileUnit,*)

        write(output_unit,"(*(g0))")

        itick = 0
        call Timer%tic()
        do isample = 1, ParaPost%count

            allocate(ParaPost%Sample(isample)%Avg(ParaPost%ndim))
            allocate(ParaPost%Sample(isample)%CholFacLower(ParaPost%ndim,ParaPost%ndim))
            allocate(ParaPost%Sample(isample)%CholFacDiag(ParaPost%ndim))
            read(paraPostFileUnit,* )  logFunc &
                                    ,  ParaPost%Sample(isample)%Avg(1:ParaPost%ndim) &
                                    , (ParaPost%Sample(isample)%CholFacLower(i,i),i=1,ParaPost%ndim) &  ! log of standard deviations
                                    ,  ParaPost%Sample(isample)%CholFacLower(2:4,1) &                   ! Rho: LisoEpkz, LisoEiso, LisoT90z
                                    ,  ParaPost%Sample(isample)%CholFacLower(3:4,2) &                   ! Rho: EpkzEiso, EpkzT90z
                                    ,  ParaPost%Sample(isample)%CholFacLower(4:4,3) &                   ! Rho: EisoT90z
                                    ,  ParaPost%Sample(isample)%Thresh%avg &                            ! log of threshold mean
                                    ,  ParaPost%Sample(isample)%Thresh%invStdSqrt2                      ! log of threshold standard deviation

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

            !***********************************************************************************************************************
            ! set the global variables from module WorldModelForBatseLGRB_mod to compute model integral
            !***********************************************************************************************************************

            ! first assign the module variables from WorldModelForBatseLGRB_mod
!            mv_Thresh%avg           = ParaPost%Sample(isample)%Thresh%avg
!            mv_Thresh%invStdSqrt2   = ParaPost%Sample(isample)%Thresh%invStdSqrt2
!            mv_Thresh%logPbolMin    = mv_Thresh%avg - THRESH_HALF_WIDTH*mv_Thresh%invStdSqrt2 - MAX_LOGPH53_4_LOGPBOLZERO
!            mv_Thresh%logPbolMax    = mv_Thresh%avg + THRESH_HALF_WIDTH*mv_Thresh%invStdSqrt2 - MIN_LOGPH53_4_LOGPBOLZERO
!
!            ! mean and standard deviations
!            mv_Avg%logLiso  = ParaPost%Sample(isample)%Avg(1)
!            mv_Avg%logEpkz  = ParaPost%Sample(isample)%Avg(2)
!            mv_Avg%logEiso  = ParaPost%Sample(isample)%Avg(3)
!            mv_Avg%logT90z  = ParaPost%Sample(isample)%Avg(4)
!            mv_Std%logLiso  = ParaPost%Sample(isample)%CholFacLower(1,1)
!            mv_Std%logEpkz  = ParaPost%Sample(isample)%CholFacLower(2,2)
!            mv_Std%logEiso  = ParaPost%Sample(isample)%CholFacLower(3,3)
!            mv_Std%logT90z  = ParaPost%Sample(isample)%CholFacLower(4,4)
!
!            ! do inverse Fisher-transform to get the correlation coefficients
!            rhoLisoEpkz = ParaPost%Sample(isample)%CholFacLower(2,1)
!
!            ! terms used in the conditional distribution of logEpkz given logLiso.
!            mv_LogEpkzGivenLogLiso%tilt            = rhoLisoEpkz    * mv_Std%logEpkz / mv_Std%logLiso
!            mv_LogEpkzGivenLogLiso%bias            = mv_Avg%logEpkz - mv_Avg%logLiso * mv_LogEpkzGivenLogLiso%tilt
!            mv_LogEpkzGivenLogLiso%std             = sqrt( 1._RK - rhoLisoEpkz**2 ) * mv_Std%logEpkz
!            mv_LogEpkzGivenLogLiso%invStdSqrt2     = 1._RK / (mv_LogEpkzGivenLogLiso%std * SQRT2)
!            mv_LogEpkzGivenLogLiso%invStdSqrt2pi   = 1._RK / (mv_LogEpkzGivenLogLiso%std * SQRT2PI)
!
!            ! terms used in the marginal distribution of logLiso given logLiso.
!            mv_logLisoInvStdSqrt2   = 1._RK / (mv_Std%logLiso * SQRT2)   ! coefficient of the exponent of Gaussian distribution.
!            mv_logLisoInvStdSqrt2pi = 1._RK / (mv_Std%logLiso * SQRT2PI) ! coefficient of the Gaussian function.
!
!            ! compute the normalization factor of the world model by integrating over all GRB attributes subject to BATSE threshold
!            call doQuadRombOpen ( getModelIntOverLogLisoGivenZ  &
!                                , midexp                        &
!                                , IntegrationLimit%zplus1min    &
!                                , IntegrationLimit%zplus1max    &
!                                , modelIntegral                 &
!                                , ierr )
!            if (ierr/=0) then
!                write(output_unit,"(*(g0))") PROCEDURE_NAME//ErrorMessage(ierr)
!                error stop
!            end if
!            if (modelIntegral<=0.0_RK) then
!                write(output_unit,"(*(g0))") PROCEDURE_NAME//"modelIntegral is non-positive: ", modelIntegral
!                error stop
!            end if

            !***********************************************************************************************************************
            ! End of model integral computation
            !***********************************************************************************************************************

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

            call Timer%toc()
            itick = itick + 1
            write(output_unit,"(*(' ',g0))", advance="no"   ) CARRIAGE_RETURN, CLOCK_TICK(itick) &
                                                            , isample, " out of  ", ParaPost%count &
                                                            , "parameter posterior samples processed in" &
                                                            , Timer%Time%total, "seconds."
            flush(output_unit)
            if (itick==4) itick = 0

        end do
        write(output_unit,"(*(g0))")
        close(paraPostFileUnit)

    end function constructParaPost


!***********************************************************************************************************************************
!***********************************************************************************************************************************

end module ParaPost_mod