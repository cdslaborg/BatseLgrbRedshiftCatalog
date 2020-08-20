module SynSam_mod

    use Constants_mod, only: IK, RK, LN10, SQRT2

    implicit none

    character(*), parameter :: MODULE_NAME = "@SynSam_mod"

    ! the exponent of zplus1 in time-dilation translation of T90 to T90z
#ifdef kfacOneThird
    real(RK)   , parameter :: TIME_DILATION_EXPO = 0.666666666666667_RK
#else
#error "kfacOneThird is not enabled in SynSam_mod.f90"
#endif

    ! Swift detection parameters
    real(RK)   , parameter :: SWIFT_INV_STD_LOG_THRESH_SQRT2 = 1._RK / (0.1_RK * LN10 * SQRT2)
    real(RK)   , parameter :: SWIFT_AVG_LOG_THRESH_B07 = log(3._RK)
    real(RK)   , parameter :: SWIFT_AVG_LOG_THRESH_B10 = log(0.15_RK) ! = log( butlerCeffmin / sqrt(avgSwiftPartialCoding*medSwiftT90OverTr45) )


!***********************************************************************************************************************************
!***********************************************************************************************************************************

contains

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    subroutine generateSynSam(ParaPost, SynRed, outFilePath, sampleSize)

        use, intrinsic :: iso_fortran_env, only: output_unit
        use Constants_mod, only: IK, RK, SPR, CARRIAGE_RETURN, CLOCK_TICK, LOG_ERG2KEV
        use BandSpectrum_mod, only: getPhotonFluenceFromEnergyFluence
        use Statistics_mod, only: getRandInt, getRandMVN
        use ParaPost_mod, only: ParaPost_type
        use SynRed_mod, only: SynRed_type
        use Timer_mod, only: Timer_type
        use Batse_mod, only: getLogPF53
        use Err_mod, only: Err_type

        implicit none

        
        type(ParaPost_type), intent(in)     :: ParaPost
        type(SynRed_type), intent(in)       :: SynRed
        character(*), intent(in)            :: outFilePath
        integer(IK) , intent(in), optional  :: sampleSize

        type(Err_type)          :: Err
        real(RK)                :: logPF53, normedLogPF53, photonFluence, photonFluence15150, invSqrtT90Obs
        real(RK)                :: probDetection, probDetectionSwiftB07, probDetectionSwiftB10
        real(RK)                :: normedEffectivePhotonFluenceB07, normedEffectivePhotonFluenceB10
        real(RK), allocatable   :: GrbInt(:) ! GRB intrinsic properties
        real(RK), allocatable   :: GrbObs(:) ! GRB observed properties
        integer(IK)             :: isample, iParaPost, iRedshift, fileUnit, itick, sampleCount, i
        type(Timer_type)        :: Timer

        sampleCount = 200000
        if (present(sampleSize)) sampleCount = sampleSize

        itick = 0

        call Timer%tic()
        open(newunit=fileUnit,file=outFilePath,status="replace")
        write(fileUnit,"(*(g0))")   "logLiso,logEpkz,logEiso,logDurz,logPbol,logEpk,logSbol,logDur,z,probDetection,&
                                    &photonFluence,photonFluence15150,probDetectionSwiftB07,probDetectionSwiftB10"

        allocate(GrbInt(ParaPost%ndim), GrbObs(ParaPost%ndim))

        do isample = 1, sampleCount
            iParaPost = getRandInt(1,ParaPost%count)
            iRedshift = getRandInt(1,SynRed%count)
            GrbInt = getRandMVN ( nd = ParaPost%ndim &
                                , MeanVec = ParaPost%Sample(iParaPost)%Avg &
                                , CholeskyLower = ParaPost%Sample(iParaPost)%CholeskyLower &
                                , Diagonal = ParaPost%Sample(iParaPost)%CholeskyDiag &
                                )
            GrbObs(1) = GrbInt(1) - SynRed%Sample(iRedshift)%logLisoLogPbolDiff
            GrbObs(2) = GrbInt(2) - SynRed%Sample(iRedshift)%logzplus1
            GrbObs(3) = GrbInt(3) - SynRed%Sample(iRedshift)%logEisoLogSbolDiff
#ifdef kfacOneThird
            GrbObs(4) = GrbInt(4) + SynRed%Sample(iRedshift)%logzplus1 * TIME_DILATION_EXPO
#else
            GrbObs(4) = GrbInt(4) + SynRed%Sample(iRedshift)%logzplus1
#endif

            !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
            !:::: get the bolometric photon fluence
            !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

            call getPhotonFluenceFromEnergyFluence  ( energyFluence = exp(GrbObs(3)+LOG_ERG2KEV)    &
                                                    , lowerLim      = 1.e-1_RK                      &
                                                    , upperLim      = 2.e+4_RK                      &
                                                    , epk           = exp(GrbObs(2))                &
                                                    , alpha         = -1.1_RK                       &
                                                    , beta          = -2.3_RK                       &
                                                    , tolerance     = 1.e-6_RK                      &
                                                    , photonFluence = photonFluence                 &
                                                    , Err           = Err                           &
                                                    )
            if (Err%occurred) then
                write(*,"(*(g0,:,' '))") Err%msg
                write(*,"(*(g0,:,' '))") "Err%stat: ", Err%stat
                error stop
            end if

            !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
            !:::: get the bolometric photon fluence
            !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

            call getPhotonFluenceFromEnergyFluence  ( energyFluence = exp(GrbObs(3)+LOG_ERG2KEV)    &
                                                    , lowerLim      = 1.e-1_RK                      &
                                                    , upperLim      = 2.e+4_RK                      &
                                                    , epk           = exp(GrbObs(2))                &
                                                    , alpha         = -1.1_RK                       &
                                                    , beta          = -2.3_RK                       &
                                                    , tolerance     = 1.e-6_RK                      &
                                                    , photonFluence = photonFluence15150            &
                                                    , Err           = Err                           &
                                                    , lowerLimNew   = 15._RK                        &
                                                    , upperLimNew   = 150._RK                       &
                                                    )
            if (Err%occurred) then
                write(*,"(*(g0,:,' '))") Err%msg
                write(*,"(*(g0,:,' '))") "Err%stat: ", Err%stat
                error stop
            end if

            !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
            !:::: get probability of detection by Swift
            !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

            invSqrtT90Obs = 1._RK / sqrt(exp(GrbObs(4)))
            normedEffectivePhotonFluenceB07 = SWIFT_INV_STD_LOG_THRESH_SQRT2 * ( log(photonFluence*invSqrtT90Obs) - SWIFT_AVG_LOG_THRESH_B07 ) ! Nbol / sqrt(T90)
            normedEffectivePhotonFluenceB10 = SWIFT_INV_STD_LOG_THRESH_SQRT2 * ( log(photonFluence15150*invSqrtT90Obs) - SWIFT_AVG_LOG_THRESH_B10 ) ! Nbol / sqrt(T90)
            probDetectionSwiftB07 = 0.5_RK + 0.5_RK * erf( real( normedEffectivePhotonFluenceB07 , kind=SPR ) )
            probDetectionSwiftB10 = 0.5_RK + 0.5_RK * erf( real( normedEffectivePhotonFluenceB10 , kind=SPR ) )

            !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
            !:::: get probability of detection by Batse
            !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

            logPF53 = getLogPF53(logEpk=GrbObs(2),logPbol=GrbObs(1))
            normedLogPF53 = ParaPost%Sample(iParaPost)%Thresh%invStdSqrt2 * ( logPF53 - ParaPost%Sample(iParaPost)%Thresh%avg )
            probDetection = 0.5_RK + 0.5_RK * erf( real( normedLogPF53 , kind=SPR ) )

            !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
            !:::: write out the quantiles
            !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

            write(fileUnit,"(*(g0.8,:,','))") GrbInt, GrbObs, SynRed%Sample(iRedshift)%z, probDetection &
                                            , photonFluence, photonFluence15150, probDetectionSwiftB07, probDetectionSwiftB10

            if (mod(sampleCount,1000)==0) then
                call Timer%toc()
                itick = itick + 1
                write(output_unit,"(*(' ',g0))", advance="no"   ) CARRIAGE_RETURN, CLOCK_TICK(itick) &
                                                                , isample, " out of  ", sampleCount &
                                                                , "GRB samples generated in" &
                                                                , Timer%Time%total, "seconds."
                flush(output_unit)
                if (itick==4) itick = 0
            end if

        end do
        close(fileUnit)

        !call Timer%toc()
        !write(output_unit,"(*(g0))")
        !write(output_unit,"(*(g0))")
        !write(output_unit,"(*(g0))") "Total Time: ", Timer%Time%total, " seconds."
        !write(output_unit,"(*(g0))")

    end subroutine generateSynSam

!***********************************************************************************************************************************
!***********************************************************************************************************************************

end module SynSam_mod