module Zestimate_mod

    use Batse_mod, only: NVAR, GRB, Trigger, NGRB => NLGRB
    use ParaPost_mod, only: NSAMPLE, ParaPost
    use Constants_mod, only: IK, RK, PI, NEG_INFINITY

    implicit none

    character(*), parameter :: MODULE_NAME = "@Zestimate_mod"

    integer(IK) , parameter :: NZ = 1000_IK
    real(RK)    , parameter :: LOGZPLUS1_MIN = log(1.1_RK), LOGZPLUS1Z_MAX = log(1.01e2_RK)
    real(RK)    , parameter :: DELTA_LOGZPLUS1 = (LOGZPLUS1Z_MAX-LOGZPLUS1_MIN) / (NZ-1)

    type :: ZInfo_type
        real(RK) :: logzplus1, logLisoLogPbolDiff, logEisoLogSbolDiff, logSFR
    end type ZInfo_type
    type(ZInfo_type) :: Zinfo(NZ)

    real(RK)                :: DetectionEfficiency(NSAMPLE)


!***********************************************************************************************************************************
!***********************************************************************************************************************************

contains

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    subroutine zestimate()

        use, intrinsic :: iso_fortran_env, only: output_unit
        use Astro_mod, only: LOGMPC2CMSQ4PI, getLogLumDisWicMpc, getLogSFRHB
       !use Astro_mod, only: grbrateHB06, ldiswickram
        use Constants_mod, only: IK, RK, SPR
        use String_mod, only: num2str
        use Timer_mod, only: Timer_type
        implicit none
        integer(IK)                 :: isample, igrb, iz, fileUnit
        real(RK)                    :: ProbZ(2,NZ) % without efficiency, with efficiency
        real(RK)                    :: zplus1, twiceLogLumDisMpc
        character(:), allocatable   :: fileName
        type(Timer_type)            :: Timer

        ! first generate the redshift grid:

        write(output_unit,"(*(g0))")
        write(output_unit,"(*(g0))") "Generating redshift grid..."
        call Timer%tic()

        do iz = 1, NZ

            Zinfo(iz)%logzplus1             = LOGZPLUS1_MIN + real(iz-1,kind=RK) * DELTA_LOGZPLUS1
            zplus1                          = exp(Zinfo(iz)%logzplus1)
            twiceLogLumDisMpc               = 2 * getLogLumDisWicMpc(zplus1)

            ! this is log10(4*pi*dl^2) where dl is luminosity distance in units of mpc
            Zinfo(iz)%logLisoLogPbolDiff    = LOGMPC2CMSQ4PI + twiceLogLumDisMpc
            Zinfo(iz)%logEisoLogSbolDiff    = LOGMPC2CMSQ4PI + twiceLogLumDisMpc - Zinfo(iz)%logzplus1

            Zinfo(iz)%logSFR                = getLogSFRHB   ( zplus1    = zplus1 &
                                                            , logzplus1 = Zinfo(iz)%logzplus1 &
                                                            , twiceLogLumDisMpc = twiceLogLumDisMpc )

        end do

        call Timer%toc()
        write(output_unit,"(*(g0))") "Total Time: ", Timer%Time%delta, " seconds."
        write(output_unit,"(*(g0))")
        call Timer%tic()

        ! compute the probability of each redshift for each GRB
        ProbZ = 0._RK
        do igrb = 1, NGRB

            call Timer%toc()
            write(output_unit,"(*(g0))")
            write(output_unit,"(*(g0))" ) num2str(igrb) &
                                        , " GRBs processed. Generating redshift probability distribution for GRB file: " &
                                        , fileName

            ! compute the detection efficiency for each GRB for each ParaPost sample
            do isample = 1, NSAMPLE
                DetectionEfficiency(isample)    = ParaPost(isample)%Thresh%invSqrt2Std &
                                                * ( GRB%Event(igrb)%logPF53 - ParaPost(isample)%Thresh%avg )
                DetectionEfficiency(isample)    = 0.5_RK + 0.5_RK * erf( real( DetectionEfficiency(isample) , kind=SPR ) )
            end do

            fileName = "../out/zdist_"//num2str(Trigger(igrb),"(I4.4)")//".txt"
            open(newunit=fileUnit,file=fileName,status="replace")
            do iz = 1, NZ
                ProbZ(1:2,iz) = getProbGRB(iz,igrb)
                write(fileUnit,"(3E25.15)") exp(Zinfo(iz)%logzplus1)-1._RK, ProbZ(iz)
            end do

            call Timer%toc()
            write(output_unit,"(*(g0))") "Total Time: ", Timer%Time%delta, " seconds."

        end do
        
        call Timer%toc()
        write(output_unit,"(*(g0))")
        write(output_unit,"(*(g0))") "Total Time for all ", num2str(NGRB), " GRBs: ", Timer%Time%total, " seconds."

    end subroutine zestimate

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    pure function getProbGRB(iz,igrb) result(ProbGRB)

        use Constants_mod, only: IK, RK
        implicit none
        integer(IK) , intent(in)    :: iz,igrb
        real(RK)    , parameter     :: INV_NSAMPLE_RK = 1._RK / real(NSAMPLE,kind=RK)
        real(RK)                    :: probGRB(2)
        real(RK)                    :: dummyTerm, MeanSubtractedVar(NVAR)
        integer(IK)                 :: isample

        probGRB = 0._RK
        do isample = 1,NSAMPLE

            ! observed data probability
            MeanSubtractedVar(1) = GRB%Event(igrb)%logPbol - ParaPost(isample)%Avg(1) + Zinfo(iz)%logLisoLogPbolDiff
            MeanSubtractedVar(2) = GRB%Event(igrb)%logEpk  - ParaPost(isample)%Avg(2) + Zinfo(iz)%logzplus1
            MeanSubtractedVar(3) = GRB%Event(igrb)%logsbol - ParaPost(isample)%Avg(3) + Zinfo(iz)%logEisoLogSbolDiff
            MeanSubtractedVar(4) = GRB%Event(igrb)%logt90  - ParaPost(isample)%Avg(4) - Zinfo(iz)%logzplus1 !* timeDilationExponent

            dummyTerm = 
           !probGRB = probGRB + batseEfficiency * exp( Zinfo(iz)%logSFR - 0.5_RK &
            probGRB = probGRB + ParaPost(isample)%coef * exp( Zinfo(iz)%logSFR - 0.5_RK &
                    * dot_product( MeanSubtractedVar , matmul(ParaPost(isample)%InvCovMat,MeanSubtractedVar) ) )

        end do
        probGRB = probGRB * INV_NSAMPLE_RK

    end function getProbGRB

!***********************************************************************************************************************************
!***********************************************************************************************************************************

end module Zestimate_mod