module ParaPost_mod

    use Constants_mod, only: IK, RK, PI

    implicit none

    character(*), parameter :: MODULE_NAME = "@ParaPost_mod"

    integer(IK) , parameter :: NSAMPLE = 2000_IK
    integer(IK) , parameter :: NVAR = 4     ! number of GRB attributes used in the world model
    integer(IK) , parameter :: NPAR = 16    ! number of world model's parameters

    ! the normalization factor of the multivariate log-normal distribution
    real(RK)    , parameter :: INV_TWOPI_POW_HALFNVAR = 1._RK / sqrt( (2._RK*PI)**NVAR )

    ! BATSE threshold parameters
    type :: Thresh_type
        real(RK) :: avg, invStdSqrt2
    end type Thresh_type

    ! Posterior sample
    type :: ParaPost_type
        ! order of variables: logLiso, logEpkz, logEiso, logT90z
        real(RK) :: coef, Avg(NVAR), InvCovMat(NVAR,NVAR)
        type(Thresh_type) :: Thresh
    end type ParaPost_type
    type (ParaPost_type) :: ParaPost(NSAMPLE)

!***********************************************************************************************************************************
!***********************************************************************************************************************************

contains

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    ! All parameters are assumed to be read in log Neper (not log10) from the input file, wherever needed.
    subroutine getParaPost(chainFilePath,paraPostFilePath)

        use, intrinsic :: iso_fortran_env, only: output_unit
        use Timer_mod, only: Timer_type
        use Constants_mod, only: IK, RK, SQRT2, SQRT2PI, CARRIAGE_RETURN, CLOCK_TICK
        use ParaDRAMRefinedSample_mod, only: RefinedSample_type
        use Matrix_mod, only: getInvPosDefMatSqrtDet

        ! modules needed for computing the model integral
        use IntegrationOverZ_mod, only: doQuadRombOpen, ErrorMessage, midexp
        use Batse_mod, only: MIN_LOGPH53_4_LOGPBOLZERO, MAX_LOGPH53_4_LOGPBOLZERO
        use WorldModelForBatseLGRB_mod, only: getModelIntOverLogLisoGivenZ, IntegrationLimit
        use WorldModelForBatseLGRB_mod, only: mv_Thresh, THRESH_HALF_WIDTH
        use WorldModelForBatseLGRB_mod, only: mv_Avg, mv_Std
        use WorldModelForBatseLGRB_mod, only: mv_logLisoInvStdSqrt2, mv_logLisoInvStdSqrt2pi
        use WorldModelForBatseLGRB_mod, only: mv_LogEpkzGivenLogLiso

        implicit none

        character(*), parameter             :: PROCEDURE_NAME = MODULE_NAME//"@getModelIntegral()"

        character(*), intent(in)            :: chainFilePath, paraPostFilePath
        integer(IK)                         :: i, j, ParaPostFileUnit, isample, ierr, itick, irefined, iweight, logFuncOffset
        real(RK), parameter                 :: INV_SQRT2 = 1._RK / SQRT2
        real(RK)                            :: rhoLisoEpkz, modelIntegral   !, logFunc
        type(RefinedSample_type)            :: Refined

        type(Timer_type)                    :: Timer

        ! get chain file contents
        call Refined%get( chainFilePath = chainFilePath , refinedSampleSize = NSAMPLE )
        if (Refined%Err%occurred) then
            write(*,*) Refined%Err%msg
            error stop
        end if

        write(output_unit,"(*(g0))")

        itick = 0
        call Timer%tic()

        open(newunit=paraPostFileUnit,file=paraPostFilePath,status="replace")

        ! write column headers to paraPost file
        write(ParaPostFileUnit,"(*(g0.8,:,','))") (Refined%ColHeader(i)%record,i=1,Refined%ndim+1)

        isample = 0_IK
        do irefined = 1, Refined%count
            do iweight = 1, Refined%Weight(irefined)

                isample = isample + 1

                if (isample>NSAMPLE) then
                    write(*,*) "Something fishy going on here. program aborted..."
                    error stop
                end if

                !*******************************************************************************************************************
                !****                                         begin reading sample data                                         ****
                !*******************************************************************************************************************

                ! NOTE: the extra one in all vector indices of Refined%State compensates for the logFunc column
                ! mean vector of the LGRB variables' distribution
                logFuncOffset = 1_IK
                ParaPost(isample)%Avg(1:NVAR) = Refined%State(logFuncOffset+1:logFuncOffset+NVAR,irefined)

                ! log of standard deviations
                do i = 1,NVAR
                    ParaPost(isample)%InvCovMat(i,i) = Refined%State(logFuncOffset+i+NVAR,irefined)
                end do

                ! Rho: LisoEpkz, LisoEiso, LisoT90z
                ParaPost(isample)%InvCovMat(2:4,1) = Refined%State(logFuncOffset+1+2*NVAR:logFuncOffset+3+2*NVAR,irefined)

                ! Rho: EpkzEiso, EpkzT90z
                ParaPost(isample)%InvCovMat(3:4,2) = Refined%State(logFuncOffset+4+2*NVAR:logFuncOffset+5+2*NVAR,irefined)

                ! Rho: EisoT90z
                ParaPost(isample)%InvCovMat(4:4,3) = Refined%State(logFuncOffset+6+2*NVAR:logFuncOffset+6+2*NVAR,irefined)

                ! log of threshold mean
                ParaPost(isample)%Thresh%avg = Refined%State(logFuncOffset+15,irefined)

                ! log of threshold standard deviation
                ParaPost(isample)%Thresh%invStdSqrt2 = Refined%State(logFuncOffset+16,irefined)

                !*******************************************************************************************************************
                !****                                     write parapost to output file                                         ****
                !*******************************************************************************************************************

               !write(paraPostFileUnit,"(*(g0.8,:,','))") Refined%State(:,irefined)
!write(*,*) ParaPost(isample)%Avg(1:NVAR)
!write(*,*) (ParaPost(isample)%InvCovMat(i,i),i=1,NVAR)
!write(*,*) ParaPost(isample)%InvCovMat(2:4,1)
!write(*,*) ParaPost(isample)%InvCovMat(3:4,2)
!write(*,*) ParaPost(isample)%InvCovMat(4:4,3)
!write(*,*) ParaPost(isample)%Thresh%avg
!write(*,*) ParaPost(isample)%Thresh%invStdSqrt2
!read(*,*)
                write(ParaPostFileUnit,"(*(g0.8,:,','))")  Refined%State(1,irefined) &                  ! logFunc
                                                        ,  ParaPost(isample)%Avg(1:NVAR) &              ! mean vector
                                                        , (ParaPost(isample)%InvCovMat(i,i),i=1,NVAR) & ! log of standard deviations
                                                        ,  ParaPost(isample)%InvCovMat(2:4,1) &         ! Rho: LisoEpkz, LisoEiso, LisoT90z
                                                        ,  ParaPost(isample)%InvCovMat(3:4,2) &         ! Rho: EpkzEiso, EpkzT90z
                                                        ,  ParaPost(isample)%InvCovMat(4:4,3) &         ! Rho: EisoT90z
                                                        ,  ParaPost(isample)%Thresh%avg &               ! log of threshold mean
                                                        ,  ParaPost(isample)%Thresh%invStdSqrt2         ! log of threshold standard deviation

                !*******************************************************************************************************************
                !****                                      begin transformation of parapost                                     ****
                !*******************************************************************************************************************

                ! threshold parameters
                ParaPost(isample)%Thresh%invStdSqrt2 = INV_SQRT2 / exp(ParaPost(isample)%Thresh%invStdSqrt2)

                ! compute the standard deviations
                do i = 1, NVAR
                    ParaPost(isample)%InvCovMat(i,i) = exp(ParaPost(isample)%InvCovMat(i,i))
                end do

                ! compute the covariance matrix's upper-triangle elements (excluding diagonal variance elements)
                do i = 1, NVAR
                    do j = i+1,NVAR

                        ! convert the lower-triangle elements to correlation coefficients
                        ParaPost(isample)%InvCovMat(j,i) = tanh(ParaPost(isample)%InvCovMat(j,i))

                        ! compute the upper-triangle elements of the covariance matrix
                        ParaPost(isample)%InvCovMat(i,j)    = ParaPost(isample)%InvCovMat(j,i) &
                                                            * ParaPost(isample)%InvCovMat(i,i) &
                                                            * ParaPost(isample)%InvCovMat(j,j)

                    end do
                end do

                !***********************************************************************************************************************
                ! set the global variables from module WorldModelForBatseLGRB_mod to compute model integral
                !***********************************************************************************************************************

                ! first assign the module variables from WorldModelForBatseLGRB_mod
                mv_Thresh%avg           = ParaPost(isample)%Thresh%avg
                mv_Thresh%invStdSqrt2   = ParaPost(isample)%Thresh%invStdSqrt2
                mv_Thresh%logPbolMin    = mv_Thresh%avg - THRESH_HALF_WIDTH*mv_Thresh%invStdSqrt2 - MAX_LOGPH53_4_LOGPBOLZERO
                mv_Thresh%logPbolMax    = mv_Thresh%avg + THRESH_HALF_WIDTH*mv_Thresh%invStdSqrt2 - MIN_LOGPH53_4_LOGPBOLZERO

                ! mean and standard deviations
                mv_Avg%logLiso  = ParaPost(isample)%Avg(1)
                mv_Avg%logEpkz  = ParaPost(isample)%Avg(2)
                mv_Avg%logEiso  = ParaPost(isample)%Avg(3)
                mv_Avg%logT90z  = ParaPost(isample)%Avg(4)
                mv_Std%logLiso  = ParaPost(isample)%InvCovMat(1,1)
                mv_Std%logEpkz  = ParaPost(isample)%InvCovMat(2,2)
                mv_Std%logEiso  = ParaPost(isample)%InvCovMat(3,3)
                mv_Std%logT90z  = ParaPost(isample)%InvCovMat(4,4)

                ! do inverse Fisher-transform to get the correlation coefficients
                rhoLisoEpkz = ParaPost(isample)%InvCovMat(2,1)

                ! terms used in the conditional distribution of logEpkz given logLiso.
                mv_LogEpkzGivenLogLiso%tilt            = rhoLisoEpkz    * mv_Std%logEpkz / mv_Std%logLiso
                mv_LogEpkzGivenLogLiso%bias            = mv_Avg%logEpkz - mv_Avg%logLiso * mv_LogEpkzGivenLogLiso%tilt
                mv_LogEpkzGivenLogLiso%std             = sqrt( 1._RK - rhoLisoEpkz**2 ) * mv_Std%logEpkz
                mv_LogEpkzGivenLogLiso%invStdSqrt2     = 1._RK / (mv_LogEpkzGivenLogLiso%std * SQRT2)
                mv_LogEpkzGivenLogLiso%invStdSqrt2pi   = 1._RK / (mv_LogEpkzGivenLogLiso%std * SQRT2PI)

                ! terms used in the marginal distribution of logLiso given logLiso.
                mv_logLisoInvStdSqrt2   = 1._RK / (mv_Std%logLiso * SQRT2)   ! coefficient of the exponent of Gaussian distribution.
                mv_logLisoInvStdSqrt2pi = 1._RK / (mv_Std%logLiso * SQRT2PI) ! coefficient of the Gaussian function.

                ! compute the normalization factor of the world model by integrating over all GRB attributes subject to BATSE threshold
                call doQuadRombOpen ( getModelIntOverLogLisoGivenZ  &
                                    , midexp                        &
                                    , IntegrationLimit%zplus1min    &
                                    , IntegrationLimit%zplus1max    &
                                    , modelIntegral                 &
                                    , ierr )
                if (ierr/=0) then
                    write(output_unit,"(*(g0))") PROCEDURE_NAME//ErrorMessage(ierr)
                    error stop
                end if
                if (modelIntegral<=0.0_RK) then
                    write(output_unit,"(*(g0))") PROCEDURE_NAME//"modelIntegral is non-positive: ", modelIntegral
                    error stop
                end if

                !***********************************************************************************************************************
                ! End of model integral computation
                !***********************************************************************************************************************

                ! now convert the standard deviations to variances
                do i = 1, NVAR
                    ParaPost(isample)%InvCovMat(i,i) = ParaPost(isample)%InvCovMat(i,i)**2  ! This is now squared standard deviation
                end do

                ! get the inverse covariance matrix and determinant
                call getInvPosDefMatSqrtDet ( nd = NVAR &
                                            , MatInvMat = ParaPost(isample)%InvCovMat &
                                            , sqrtDetInvPosDefMat = ParaPost(isample)%coef )
                if (ParaPost(isample)%coef<0._RK) then
                    write(*,*) "FATAL: Covariance Matrix to corresponding to the parameter sample ", isample, "not positive-definite."
                    error stop
                end if
                ParaPost(isample)%coef = INV_TWOPI_POW_HALFNVAR * ParaPost(isample)%coef / modelIntegral

                call Timer%toc()
                itick = itick + 1
                write(output_unit,"(*(' ',g0))", advance="no"   ) CARRIAGE_RETURN, CLOCK_TICK(itick) &
                                                            , isample, " out of  ", NSAMPLE &
                                                            , "parameter posterior samples processed in" &
                                                            , Timer%Time%total, "seconds."
                flush(output_unit)
                if (itick==4) itick = 0

            end do
        end do
        write(output_unit,"(*(g0))")

    end subroutine getParaPost


!***********************************************************************************************************************************
!***********************************************************************************************************************************

end module ParaPost_mod