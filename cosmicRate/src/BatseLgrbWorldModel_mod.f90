module BatseLgrbWorldModel_mod

#ifdef H06
    use StarFormation_mod, only: getLogSFR => getLogSFRH06
#elif defined L08
    use StarFormation_mod, only: getLogSFR => getLogSFRL08
#elif defined B10
    use StarFormation_mod, only: getLogSFR => getLogSFRB10
#elif defined M14
    use StarFormation_mod, only: getLogSFR => getLogSFRM14
#else
#error "Unknown SFR model in BatseLgrbWorldModel_mod.f90"
#endif
    use Constants_mod, only: IK, RK, PI, NEGINF_RK
    use Batse_mod, only: GRB

    implicit none

    character(*), parameter :: MODULE_NAME = "@BatseLgrbWorldModel_mod"

    ! *********************************************
    ! world model parameters
    ! *********************************************

    integer(IK), parameter :: NVAR = 4     ! number of GRB attributes used in the world model
    integer(IK), parameter :: NPAR = 16    ! number of world model's parameters

    ! the normalization factor of the multivariate log-normal distribution

    real(RK), parameter :: SQRT_TWOPI_POW_NVAR = sqrt((2._RK*PI)**NVAR)

    ! the exponent of zone in time-dilation translation of T90 to T90z

#ifdef kfacOneThird
    real(RK), parameter :: TIME_DILATION_EXPO = 0.666666666666667_RK
#endif

    ! the half-width of efficiency curve from 0 to 1, in units of threshold standard deviation
    real(RK), parameter :: THRESH_HALF_WIDTH = 4._RK

    real(RK), parameter :: INTEGRATION_LIMIT_LOGEPK_MIN = -6.712165960423344_RK
    real(RK), parameter :: INTEGRATION_LIMIT_LOGEPK_MAX = 12.455573549219071_RK

    ! *********************************************
    ! variables to be read from the input file
    ! *********************************************

    !type :: IntegrationLimit_type
    !    real(RK) :: zonemin = 1.1_RK, zonemax = 1.01e2_RK
    !    real(RK) :: logepkmin = -6.712165960423344_RK, logepkmax = 12.455573549219071_RK
    !end type IntegrationLimit_type
    !type(IntegrationLimit_type) :: IntegrationLimit

    ! integration specifications

    real(RK)    :: zoneMin = 1.1e0_RK
    real(RK)    :: zoneMax = 2.1e1_RK
    real(RK)    :: zoneTol = 5.e-5_RK
    real(RK)    :: lisoTol = 1.e-5_RK
    real(RK)    :: epkzTol = 5.e-6_RK
    integer(IK) :: zoneRef = 4_IK
    integer(IK) :: lisoRef = 5_RK
    integer(IK) :: epkzRef = 5_RK

    ! *********************************************
    ! other shared variables used in this module
    ! *********************************************

    type :: Attribute_type
        real(RK) :: logLiso, logEpkz, logEiso, logT90z
    end type Attribute_type
    type(Attribute_type) :: mv_Avg, mv_Std

    type :: Threshold_type
        real(RK) :: avg, invStdSqrt2, logPbolMin, logPbolMax
    end type Threshold_type
    type(Threshold_type) :: mv_Thresh

    type :: LogEpkzGivenLogLiso_type
        real(RK):: tilt, bias, avg, std, invStdSqrt2, invStdSqrt2pi
    end type LogEpkzGivenLogLiso_type
    type(LogEpkzGivenLogLiso_type) :: mv_LogEpkzGivenLogLiso

    ! The covariance matrix of the world model

    real(RK)        :: mv_CholeskyLowerLogNormModel(NVAR,NVAR), mv_DiagonalLogNormModel(NVAR)
    real(RK)        :: mv_InvCovMatLogNormModel(NVAR,NVAR)
   
    real(RK)        :: mv_logLisoLogPbolDiff ! this is log10(4*pi*dl^2) where dl is luminosity distance in units of mpc
    real(RK)        :: mv_logZone, mv_logEpkzMin, mv_logEpkzMax, mv_logPbol
    real(RK)        :: mv_logLisoInvStdSqrt2, mv_logLisoInvStdSqrt2pi
    integer(IK)     :: mv_igrb ! index for referencing GRBs in the computation of logPostProb

!***********************************************************************************************************************************
!***********************************************************************************************************************************

contains

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    ! Amir Shahmoradi, Sunday 12:12 AM, Dec 17, 2018, SEIR, UTA, Arlington, TX.
    function getLogPostProb(npar,Param) result(logPostProb)

        use, intrinsic :: iso_fortran_env, only: output_unit
        use Constants_mod, only: IK, RK, SQRT2, SQRT2PI
       !use Integration_mod, only: doQuadRombOpen, ErrorMessage, midexp
#if defined quadpackDPR
        use QuadPackDPR_mod, only: dqag
#elif defined quadpackSPR
        use QuadPackSPR_mod, only: qag
#else
       !use IntegrationOverZ_mod, only: doQuadRombOpen, ErrorMessage, midexp
        use Integration_mod, only: doQuadRombOpen, ErrorMessage, midexp
#endif
        use Matrix_mod, only: getCholeskyFactor, getInvMatFromCholFac
        use Batse_mod, only: MIN_LOGPH53_4_LOGPBOLZERO, MAX_LOGPH53_4_LOGPBOLZERO
        implicit none
        integer(IK)             :: i, j, ierr
        integer(IK), intent(in) :: npar
        real(RK), intent(in)    :: Param(npar)
        real(RK)                :: logPostProb
        real(RK)                :: rhoLisoEpkz, normFac, probGRB
        real(RK)                :: modelint ! integral of the model over the redshift range given by variable zone.
        !real(RK)               :: lumsigma,epkzsigma,conepkzsigma,normFac
        !real(RK), external     :: getModelIntOverLogLisoGivenZ,midexp,getProbGRB

        ! integration variables

        real(RK)                :: relerr
        integer(IK)             :: neval

#if defined quadpackDPR
        integer(IK), parameter  :: limit = 1000_IK
        integer(IK), parameter  :: lenw = 4_IK * limit
        integer(IK)             :: last
        integer(IK)             :: iwork(limit)
        real(RK)                :: work(lenw)
#endif

        ! mean and standard deviations

        mv_Avg%logLiso  = Param(1)
        mv_Avg%logEpkz  = Param(2)
        mv_Avg%logEiso  = Param(3)
        mv_Avg%logT90z  = Param(4)
        mv_Std%logLiso  = exp(Param(5))
        mv_Std%logEpkz  = exp(Param(6))
        mv_Std%logEiso  = exp(Param(7))
        mv_Std%logT90z  = exp(Param(8))

        ! do inverse Fisher-transform to get the correlation coefficients

        rhoLisoEpkz = tanh(Param(9))

        ! covariance matrix of the LogNormal GRB world model

        mv_CholeskyLowerLogNormModel(1,1) = mv_Std%logLiso * mv_Std%logLiso
        mv_CholeskyLowerLogNormModel(2,2) = mv_Std%logEpkz * mv_Std%logEpkz
        mv_CholeskyLowerLogNormModel(3,3) = mv_Std%logEiso * mv_Std%logEiso
        mv_CholeskyLowerLogNormModel(4,4) = mv_Std%logT90z * mv_Std%logT90z
        mv_CholeskyLowerLogNormModel(1,2) = mv_Std%logLiso * mv_Std%logEpkz * rhoLisoEpkz
        mv_CholeskyLowerLogNormModel(1,3) = mv_Std%logLiso * mv_Std%logEiso * tanh(Param(10))
        mv_CholeskyLowerLogNormModel(1,4) = mv_Std%logLiso * mv_Std%logT90z * tanh(Param(11))
        mv_CholeskyLowerLogNormModel(2,3) = mv_Std%logEpkz * mv_Std%logEiso * tanh(Param(12))
        mv_CholeskyLowerLogNormModel(2,4) = mv_Std%logEpkz * mv_Std%logT90z * tanh(Param(13))
        mv_CholeskyLowerLogNormModel(3,4) = mv_Std%logEiso * mv_Std%logT90z * tanh(Param(14))

        ! terms used in the conditional distribution of logEpkz given logLiso.

        mv_LogEpkzGivenLogLiso%tilt            = rhoLisoEpkz    * mv_Std%logEpkz / mv_Std%logLiso
        mv_LogEpkzGivenLogLiso%bias            = mv_Avg%logEpkz - mv_Avg%logLiso * mv_LogEpkzGivenLogLiso%tilt
        mv_LogEpkzGivenLogLiso%std             = sqrt( 1._RK - rhoLisoEpkz**2 ) * mv_Std%logEpkz
        mv_LogEpkzGivenLogLiso%invStdSqrt2     = 1._RK / (mv_LogEpkzGivenLogLiso%std * SQRT2)
        mv_LogEpkzGivenLogLiso%invStdSqrt2pi   = 1._RK / (mv_LogEpkzGivenLogLiso%std * SQRT2PI)

        ! terms used in the marginal distribution of logLiso given logLiso.

        mv_logLisoInvStdSqrt2   = 1._RK / (mv_Std%logLiso * SQRT2)   ! scale factor in the exponent of Gaussian distribution.
        mv_logLisoInvStdSqrt2pi = 1._RK / (mv_Std%logLiso * SQRT2PI) ! normalization constant of the univariate Gaussian function.

        ! BATSE detection threshold

        mv_Thresh%avg           = Param(15)
        mv_Thresh%invStdSqrt2   = exp(Param(16))    ! momentarily is the threshold's standard deviation for the needs below.

        ! logPbol below which no trigger happens, and above which trigger efficiency is 100%.

        mv_Thresh%logPbolMin    = mv_Thresh%avg - THRESH_HALF_WIDTH*mv_Thresh%invStdSqrt2 - MAX_LOGPH53_4_LOGPBOLZERO
        mv_Thresh%logPbolMax    = mv_Thresh%avg + THRESH_HALF_WIDTH*mv_Thresh%invStdSqrt2 - MIN_LOGPH53_4_LOGPBOLZERO
        mv_Thresh%invStdSqrt2   = 1._RK / (mv_Thresh%invStdSqrt2*SQRT2)

        !write(output_unit,"(*(g20.13))") ((mv_CholeskyLowerLogNormModel(i,j),j=1,NVAR),new_line("A"),i=1,NVAR)
        call getCholeskyFactor(NVAR,mv_CholeskyLowerLogNormModel,mv_DiagonalLogNormModel)
        if (mv_DiagonalLogNormModel(1)<0._RK) then
            !write(output_unit,"(*(g0))") "covariance matrix not positive definite..cycling.."
            !write(output_unit,"(*(g20.13))") ((mv_CholeskyLowerLogNormModel(i,j),j=1,NVAR),new_line("A"),i=1,NVAR)
            logPostProb = -huge(logPostProb)
            return
        end if
 
        ! (2*pi)^(NVAR/2)(=39.478417604357434)*sqrt(determinant)
        normFac = SQRT_TWOPI_POW_NVAR * product(mv_DiagonalLogNormModel)
        if (normFac<=0) then
            write(output_unit,"(*(g0))") "sqrt of covariance determinant is <=0: ", normFac
            write(output_unit,"(*(g0))") "Cholesky mv_DiagonalLogNormModel: "
            write(output_unit,"(*(g0))") mv_DiagonalLogNormModel
            write(output_unit,"(*(g0))") "mv_CholeskyLowerLogNormModel/CovarianceMatrix: "
            write(output_unit,"(*(g20.13))") ((mv_CholeskyLowerLogNormModel(i,j),j=1,NVAR),new_line("A"),i=1,NVAR)
            stop
        end if

        ! get the full Inverse covariance matrix
        mv_InvCovMatLogNormModel = getInvMatFromCholFac(NVAR,mv_CholeskyLowerLogNormModel,mv_DiagonalLogNormModel)

        ! compute the normalization factor of the world model by integrating over all GRB attributes, subject to BATSE threshold
#if defined quadpackDPR
        call    dqag( f             = getModelIntOverLogLisoGivenZ  &
                    , a             = zoneMin                       &
                    , b             = zoneMax                       &
                    , epsabs        = 0._RK                         &
                    , epsrel        = zoneTol                       &
                    , key           = 1_IK                          &
                    , result        = modelint                      &
                    , abserr        = relerr                        &
                    , neval         = neval                         &
                    , ier           = ierr                          &
                    , limit         = limit                         &
                    , lenw          = lenw                          &
                    , last          = last                          &
                    , iwork         = iwork                         &
                    , work          = work                          &
                    )
        if (ierr/=0) then
            write(output_unit,"(*(g0))") "FATAL: @qag(): error occurred while computing model integral over redshift. ierr, neval = ", ierr, neval
            error stop
        end if
#elif defined quadpackSPR
        call     qag( f             = getModelIntOverLogLisoGivenZ  &
                    , a             = zoneMin                       &
                    , b             = zoneMax                       &
                    , epsabs        = 0._RK                         &
                    , epsrel        = zoneTol                       &
                    , key           = 1_IK                          &
                    , result        = modelint                      &
                    , abserr        = relerr                        &
                    , neval         = neval                         &
                    , ier           = ierr                          &
                    )
        if (ierr/=0) then
            write(output_unit,"(*(g0))") "FATAL: @qag(): error occurred while computing model integral over redshift. ierr=",ierr
            error stop
        end if
#else
        call doQuadRombOpen ( getFunc           = getModelIntOverLogLisoGivenZ  &
                            , integrate         = midexp                        &
                            , lowerLim          = zoneMin                       &
                            , upperLim          = zoneMax                       &
                            , maxRelativeError  = zoneTol                       &
                            , nRefinement       = zoneRef                       &
                            , integral          = modelint                      &
                            , relativeError     = relerr                        &
                            , numFuncEval       = neval                         &
                            , ierr              = ierr                          &
                            )
        !write(*,*) "Zone: ", neval, relerr / modelint
        if (ierr/=0) then
            write(output_unit,"(*(g0))") ErrorMessage(ierr)
            error stop
        end if
#endif
        if (modelint<=0.0_RK) then
            write(output_unit,"(*(g0))") "model_integral (variable modelint in getLogPostProb.f90) is non-positive: ", modelint
            error stop
        end if

        ! marginalize over all possible redshifts

        logPostProb = 0._RK
        loopLogPostProb: do mv_igrb = 1, GRB%count
            !probGRB = getProbGRB(2.5_RK)
#if defined quadpackDPR
            call    dqag( f             = getProbGRB    &
                        , a             = zoneMin       &
                        , b             = zoneMax       &
                        , epsabs        = 0._RK         &
                        , epsrel        = zoneTol       &
                        , key           = 1_IK          &
                        , result        = probGRB       &
                        , abserr        = relerr        &
                        , neval         = neval         &
                        , ier           = ierr          &
                        , limit         = limit         &
                        , lenw          = lenw          &
                        , last          = last          &
                        , iwork         = iwork         &
                        , work          = work          &
                        )
            if (ierr/=0) then
                write(output_unit,"(*(g0))") "FATAL: @qag(): error occurred while computing probGRB. ierr=", ierr
                error stop
            end if
#elif defined quadpackSPR
            call     qag( f             = getProbGRB    &
                        , a             = zoneMin       &
                        , b             = zoneMax       &
                        , epsabs        = 0._RK         &
                        , epsrel        = zoneTol       &
                        , key           = 1_IK          &
                        , result        = probGRB       &
                        , abserr        = relerr        &
                        , neval         = neval         &
                        , ier           = ierr          &
                        )
            if (ierr/=0) then
                write(output_unit,"(*(g0))") "FATAL: @qag(): error occurred while computing probGRB. ierr=", ierr
                error stop
            end if
#else
            call doQuadRombOpen ( getFunc           = getProbGRB    &
                                , integrate         = midexp        &
                                , lowerLim          = zoneMin       &
                                , upperLim          = zoneMax       &
                                , maxRelativeError  = zoneTol       &
                                , nRefinement       = zoneRef       &
                                , integral          = probGRB       &
                                , relativeError     = relerr        &
                                , numFuncEval       = neval         &
                                , ierr              = ierr          &
                                )
            !write(*,*) "Zone, ith GRB: ", mv_igrb, neval, relerr / probGRB
            if (ierr/=0) then
                write(output_unit,"(*(g0))") ErrorMessage(ierr)
                error stop
            end if
#endif
            if (probGRB<=0.0_RK) then
                write(output_unit,"(*(g0))") "WARNING: probGRB <= 0.0_RK: ", probGRB, ". Setting logPostProb = NEGINF_RK ..."
                logPostProb = NEGINF_RK
                exit loopLogPostProb
            end if
            logPostProb = logPostProb + log( probGRB / (modelint*normFac) )
        end do loopLogPostProb

    end function getLogPostProb

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    ! integral of grb world model at given redshift z, detemined by zone.
    function getModelIntOverLogLisoGivenZ(zone) result(modelIntOverLogLisoGivenZ)

        use, intrinsic :: iso_fortran_env, only: output_unit
        use Cosmology_mod, only: LOGMPC2CMSQ4PI, getLogLumDisWicMpc
       !use IntegrationOverLiso_mod, only: doQuadRombClosed, ErrorMessage
        use Integration_mod, only: doQuadRombClosed, ErrorMessage
        use Constants_mod, only: RK, SPR
        implicit none

        character(*), parameter :: PROCEDURE_NAME = "@getModelIntOverLogLisoGivenZ()"
        real(RK), intent(in)    :: zone
        real(RK)                :: relerr
        real(RK)                :: modelIntOverLogLisoGivenZ
        real(RK)                :: twiceLogLumDisMpc, logLisoAtFullEfficiency
        integer(IK)             :: ierr, neval

        mv_logZone = log(zone)
        twiceLogLumDisMpc = 2 * getLogLumDisWicMpc(zone)
        mv_logLisoLogPbolDiff = LOGMPC2CMSQ4PI + twiceLogLumDisMpc ! This is later used in 

        ! These are used in getModelIntOverLogEpkzGivenLogLisoZ()
        mv_logEpkzMin = INTEGRATION_LIMIT_LOGEPK_MIN + mv_logZone
        mv_logEpkzMax = INTEGRATION_LIMIT_LOGEPK_MAX + mv_logZone

        ! compute world model integral over logLiso in the varying BATSE efficiency range, for the given z.
        logLisoAtFullEfficiency = mv_Thresh%logPbolMax + mv_logLisoLogPbolDiff
        call doQuadRombClosed   ( getFunc           = getModelIntOverLogEpkzGivenLogLisoZ           &
                                , lowerLim          = mv_Thresh%logPbolMin + mv_logLisoLogPbolDiff  &
                                , upperLim          = logLisoAtFullEfficiency                       &
                                , maxRelativeError  = lisoTol                                       &
                                , nRefinement       = lisoRef                                       &
                                , integral          = modelIntOverLogLisoGivenZ                     &
                                , relativeError     = relerr                                        &
                                , numFuncEval       = neval                                         &
                                , ierr              = ierr                                          &
                                )
!write(*,*) "Liso: ", neval, relerr / modelIntOverLogLisoGivenZ
        if (ierr/=0) then
            write(output_unit,"(*(g0))") PROCEDURE_NAME // ErrorMessage(ierr)
            error stop
        end if

        ! add the analytical integral of the logLiso range within which BATSE efficiency is 100%
        modelIntOverLogLisoGivenZ   = modelIntOverLogLisoGivenZ + 0.5_RK &
                                    * erfc( real( (logLisoAtFullEfficiency-mv_Avg%logLiso)*mv_logLisoInvStdSqrt2 , kind=SPR ) )

        ! multiply the integral result by the GRB rate density at the given redshift
        modelIntOverLogLisoGivenZ = modelIntOverLogLisoGivenZ * exp(getLogSFR(zone,mv_logZone,twiceLogLumDisMpc))

    end function getModelIntOverLogLisoGivenZ

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    function getModelIntOverLogEpkzGivenLogLisoZ(logLiso) result(modelIntOverLogEpkzGivenLogLisoZ)

        use, intrinsic :: iso_fortran_env, only: output_unit
        use Batse_mod, only: MIN_LOGPH53_4_LOGPBOLZERO
        use Integration_mod, only: doQuadRombClosed, ErrorMessage
       !use IntegrationOverEpkz_mod, only: doQuadRombClosed, ErrorMessage
        use Constants_mod, only: RK, SPR

        implicit none
        character(*), parameter :: PROCEDURE_NAME = "@getModelIntOverLogEpkzGivenLogLisoZ()"
        real(RK), intent(in)    :: logLiso
        real(RK)                :: modelIntOverLogEpkzGivenLogLisoZ
        real(RK)                :: relerr
        integer(IK)             :: ierr, neval

        mv_LogEpkzGivenLogLiso%avg = mv_LogEpkzGivenLogLiso%bias + mv_LogEpkzGivenLogLiso%tilt*logLiso
        mv_logPbol = logLiso - mv_logLisoLogPbolDiff
        call doQuadRombClosed   ( getFunc           =  getProbEpkzGivenLiso             &
                                , lowerLim          =  mv_logEpkzMin                    &
                                , upperLim          =  mv_logEpkzMax                    &
                                , maxRelativeError  =  epkzTol                          &
                                , nRefinement       =  epkzRef                          &
                                , integral          =  modelIntOverLogEpkzGivenLogLisoZ &
                                , relativeError     =  relerr                           &
                                , numFuncEval       =  neval                            &
                                , ierr              =  ierr                             &
                                )
!write(*,*) "Epkz: ", neval, relerr / modelIntOverLogEpkzGivenLogLisoZ
        if (ierr/=0) then
            write(output_unit,"(*(g0))") PROCEDURE_NAME // ErrorMessage(ierr)
            error stop
        end if

        ! add integral of the tails of the conditional logEpkz distribution given mv_logLiso
        modelIntOverLogEpkzGivenLogLisoZ = modelIntOverLogEpkzGivenLogLisoZ + &
        ! efficiency is fixed for the tails of logEpkz distribution beyond the logEpkz limits
        ( 0.5_RK + 0.5_RK*erf(real((MIN_LOGPH53_4_LOGPBOLZERO+mv_logPbol-mv_Thresh%avg)*mv_Thresh%invStdSqrt2,kind=SPR)) ) * &
        ! sum of integrals of the tails of the conditional logEpkz distribution given mv_logLiso
        ( 1.0_RK + 0.5_RK * ( &
        erf( real( (mv_logEpkzMin-mv_LogEpkzGivenLogLiso%avg)*mv_LogEpkzGivenLogLiso%invStdSqrt2 , kind=SPR ) ) - &
        erf( real( (mv_logEpkzMax-mv_LogEpkzGivenLogLiso%avg)*mv_LogEpkzGivenLogLiso%invStdSqrt2 , kind=SPR ) ) ) )

        modelIntOverLogEpkzGivenLogLisoZ = modelIntOverLogEpkzGivenLogLisoZ &
                                         * mv_logLisoInvStdSqrt2pi &
                                         * exp( -( (logLiso-mv_Avg%logLiso)*mv_logLisoInvStdSqrt2 )**2 )

    end function getModelIntOverLogEpkzGivenLogLisoZ

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    pure function getProbEpkzGivenLiso(logEpkz) result(probEpkzGivenLiso)
        use Constants_mod, only: RK, SPR
        use Batse_mod, only: getLogPF53
        implicit none
        real(RK), intent(in)    :: logEpkz
        real(RK)                :: probEpkzGivenLiso, normedLogPF53, efficiency
        normedLogPF53 = (getLogPF53(logEpkz-mv_logZone,mv_logPbol)-mv_Thresh%avg)*mv_Thresh%invStdSqrt2
        efficiency = 0.5_RK + 0.5_RK * erf(real(normedLogPF53,kind=SPR))
        probEpkzGivenLiso   = efficiency * mv_LogEpkzGivenLogLiso%invStdSqrt2pi &
                            * exp( -( (logEpkz-mv_LogEpkzGivenLogLiso%avg)*mv_LogEpkzGivenLogLiso%invStdSqrt2)**2 )
    end function getProbEpkzGivenLiso

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    function getProbGRB(zone) result(probGRB)

        use Cosmology_mod, only: LOGMPC2CMSQ4PI, getLogLumDisWicMpc
        use Batse_mod, only: getLogPF53
        use Constants_mod, only: RK
        implicit none
        real(RK), intent(in)    :: zone
        real(RK)                :: probGRB
        real(RK)                :: MeanSubtractedVar(NVAR)
        real(RK)                :: twiceLogLumDisMpc, logZone
        real(RK)                :: logLisoLogPbolDiff

        logZone = log(zone)
        twiceLogLumDisMpc = 2 * getLogLumDisWicMpc(zone)

        ! this is log10(4*pi*dl^2) where dl is luminosity distance in units of mpc
        logLisoLogPbolDiff = LOGMPC2CMSQ4PI + twiceLogLumDisMpc

        ! observed data probability
        MeanSubtractedVar(1) = GRB%Event(mv_igrb)%logPbol - mv_Avg%logLiso + logLisoLogPbolDiff
        MeanSubtractedVar(2) = GRB%Event(mv_igrb)%logEpk  - mv_Avg%logEpkz + logZone
        MeanSubtractedVar(3) = GRB%Event(mv_igrb)%logsbol - mv_Avg%logEiso - logZone + logLisoLogPbolDiff
#ifdef kfacOneThird
        MeanSubtractedVar(4) = GRB%Event(mv_igrb)%logt90  - mv_Avg%logT90z - logZone * TIME_DILATION_EXPO
#else
        MeanSubtractedVar(4) = GRB%Event(mv_igrb)%logt90  - mv_Avg%logT90z - logZone
#endif

        probGRB = getBatseEfficiency( mv_Thresh%invStdSqrt2 * ( GRB%Event(mv_igrb)%logPF53 - mv_Thresh%avg ) ) &
                * exp( getLogSFR(zone,logZone,twiceLogLumDisMpc) &
                - 0.5_RK * dot_product( MeanSubtractedVar , matmul(mv_InvCovMatLogNormModel,MeanSubtractedVar) ) )

    end function getProbGRB

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    pure function getBatseEfficiency(normedLogPF53) result(batseEfficiency)
        use Constants_mod, only: RK, SPR
        implicit none
        real(RK), intent(in) :: normedLogPF53
        real(RK)             :: batseEfficiency
        batseEfficiency = 0.5_RK + 0.5_RK * erf( real( normedLogPF53 , kind=SPR ) )
    end function getBatseEfficiency

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    pure function getBatseEfficiencyApprox(logEpk,logPbol) result(batseEfficiency)
        use Constants_mod, only: RK, SPR
        use Batse_mod, only: getLogPF53
        implicit none
        real(RK), intent(in) :: logEpk,logPbol
        real(RK)             :: batseEfficiency
        real(RK)             :: normedLogPF53
        if ( logPbol < mv_Thresh%logPbolMin ) then
            batseEfficiency = 0._RK
        elseif ( logPbol < mv_Thresh%logPbolMax ) then
            normedLogPF53 = ( getLogPF53(logEpk,logPbol) - mv_Thresh%avg ) * mv_Thresh%invStdSqrt2
            batseEfficiency = 0.5_RK + 0.5_RK * erf( real( normedLogPF53 , kind=SPR ) )
        elseif ( logPbol >= mv_Thresh%logPbolMax ) then
            batseEfficiency = 1._RK
        end if
    end function getBatseEfficiencyApprox

!***********************************************************************************************************************************
!***********************************************************************************************************************************

end module BatseLgrbWorldModel_mod