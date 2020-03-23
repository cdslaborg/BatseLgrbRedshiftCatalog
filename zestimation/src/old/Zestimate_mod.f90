module Zestimate_mod

    use Batse_mod, only: NVAR, GRB, Trigger, NGRB => NLGRB
    use ParaPost_mod, only: NSAMPLE, ParaPost
    use Constants_mod, only: IK, RK, PI

    implicit none

    character(*), parameter :: MODULE_NAME = "@Zestimate_mod"

    integer(IK) , parameter :: NZ = 1000_IK
    real(RK)    , parameter :: ZMIN = 0.1_RK, ZMAX = 1.01e2_RK
    real(RK)    , parameter :: LOGZPLUS1_MIN = log(ZMIN+1._RK), LOGZPLUS1Z_MAX = log(ZMAX+1._RK)
    real(RK)    , parameter :: DELTA_LOGZPLUS1 = (LOGZPLUS1Z_MAX-LOGZPLUS1_MIN) / (NZ-1)

    type :: ZInfo_type
        real(RK) :: z, logzplus1, logLisoLogPbolDiff, logEisoLogSbolDiff, logSFR
    end type ZInfo_type
    type(ZInfo_type) :: mv_Zinfo(NZ)

    real(RK)                :: mv_DetectionEfficiency(NSAMPLE)

!***********************************************************************************************************************************
!***********************************************************************************************************************************

contains

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    subroutine zestimate(getLogSFR, outPath)

        use, intrinsic :: iso_fortran_env, only: output_unit
        use Astro_mod, only: getLogSFR_proc
        !use Astro_mod, only: getLogSFR => getLogSFRHB
        use Astro_mod, only: LOGMPC2CMSQ4PI, getLogLumDisWicMpc !, getlogdvdz
        use Constants_mod, only: IK, RK, SPR, CARRIAGE_RETURN, CLOCK_TICK
        use String_mod, only: num2str
        use Timer_mod, only: Timer_type
        use System_mod, only: OS_type
        use Path_mod, only: winifyPath, linifyPath
        !use Math_mod, only: getCumSum

        implicit none

        character(*), intent(in)    :: outPath
        procedure(getLogSFR_proc)   :: getLogSFR

        integer(IK)                 :: isample, igrb, iz, fileUnit, fileUnitStatZ, itick
        real(RK)                    :: probZ, CumSumProbZ(0:NZ)
        real(RK)                    :: BatseRate(NZ), CumSumBatseRate(0:NZ), CumSumSFR(0:NZ)
        real(RK)                    :: zplus1, logLumDisWicMpc, twiceLogLumDisMpc
        character(:), allocatable   :: fileName, fileNameStatZ, modifiedPath
        type(Timer_type)            :: Timer

        ! quantiles
        integer                     :: iq
        integer(IK), parameter      :: NQ = 5
        real(RK), parameter         :: QPROB(NQ) = [0.05_RK, 0.25_RK, 0.5_RK, 0.75_RK, 0.95_RK]
        real(RK)                    :: normCoef, dumvar
        type :: StatZ_type
            real(RK)                :: mean, mode, Quantile(NQ)
        end type StatZ_type
        type(StatZ_type)            :: StatZ

        type(OS_type)               :: OS

        call OS%query()
        if (OS%Err%occurred) then
            write(output_unit,"(*(g0))")
            write(output_unit,"(*(g0))") "FATAL: "//OS%Err%msg
            write(output_unit,"(*(g0))")
            error stop
        end if
        if (OS%isWindows) then
            call winifyPath(outPath,modifiedPath,OS%Err)
        else
            call linifyPath(outPath,modifiedPath)
        end if
        if (OS%Err%occurred) then
            write(output_unit,"(*(g0))")
            write(output_unit,"(*(g0))") "FATAL: "//OS%Err%msg
            write(output_unit,"(*(g0))")
            error stop
        end if
        call execute_command_line("mkdir "//modifiedPath)

        !***************************************************************************************************************************
        ! first generate the redshift grid:
        !***************************************************************************************************************************

        write(output_unit,"(*(g0))")
        write(output_unit,"(*(g0))")
        write(output_unit,"(*(g0))") "Generating redshift grid..."
        call Timer%tic()

        fileName = outPath//"zgrid.txt"
        open(newunit=fileUnit,file=fileName,status="replace")
        write(fileUnit,"(*(g0))") "z,logLuminosityDistance,logLgrbRate"
        do iz = 1, NZ

            mv_Zinfo(iz)%logzplus1  = LOGZPLUS1_MIN + real(iz-1,kind=RK) * DELTA_LOGZPLUS1
            zplus1                  = exp(mv_Zinfo(iz)%logzplus1)
            mv_Zinfo(iz)%z          = zplus1 - 1._RK
            logLumDisWicMpc         = getLogLumDisWicMpc(zplus1)
            twiceLogLumDisMpc       = 2 * logLumDisWicMpc

            ! this is log10(4*pi*dl^2) where dl is luminosity distance in units of mpc
            mv_Zinfo(iz)%logLisoLogPbolDiff    = LOGMPC2CMSQ4PI + twiceLogLumDisMpc
            mv_Zinfo(iz)%logEisoLogSbolDiff    = LOGMPC2CMSQ4PI + twiceLogLumDisMpc - mv_Zinfo(iz)%logzplus1

           !mv_Zinfo(iz)%logSFR                = getlogdvdz (zplus1 = zplus1 &
            mv_Zinfo(iz)%logSFR                = getLogSFR  ( zplus1    = zplus1 &
                                                            , logzplus1 = mv_Zinfo(iz)%logzplus1 &
                                                            , twiceLogLumDisMpc = twiceLogLumDisMpc )

            write(fileUnit,"(*(g0.8,:,','))") mv_Zinfo(iz)%z, logLumDisWicMpc, mv_Zinfo(iz)%logSFR

        end do
        close(fileUnit)

        call Timer%toc()
        write(output_unit,"(*(g0))") "Total Time: ", Timer%Time%delta, " seconds."
        write(output_unit,"(*(g0))")
        call Timer%tic()

        !***************************************************************************************************************************
        ! compute the probability of each redshift for each GRB
        !***************************************************************************************************************************

        fileNameStatZ = outPath//"batse_zstat.txt"
        open(newunit=fileUnitStatZ,file=fileNameStatZ,status="replace")
        write(fileUnitStatZ,"(*(g0))") "trigger,meanZ,modeZ,q05,q25,q50,q75,q95"

        itick = 0
        BatseRate = 0._RK
        do igrb = 1, NGRB

            ! compute the detection efficiency for each GRB for each ParaPost sample
            do isample = 1, NSAMPLE
                mv_DetectionEfficiency(isample) = ParaPost(isample)%Thresh%invStdSqrt2 &
                                                * ( GRB%Event(igrb)%logPF53 - ParaPost(isample)%Thresh%avg )
                mv_DetectionEfficiency(isample) = 0.5_RK + 0.5_RK * erf( real( mv_DetectionEfficiency(isample) , kind=SPR ) )
            end do

            fileName = outPath//"zprob_"//num2str(Trigger(igrb),"(I4.4)")//".txt"
            open(newunit=fileUnit,file=fileName,status="replace")
            write(fileUnit,"(*(g0))") "zlikelihood"

            CumSumProbZ(0) = 0._RK
            StatZ%mean = 0._RK
            dumvar = -1._RK
            do iz = 1, NZ
                probZ = getProbZ(iz,igrb)
                StatZ%mean = StatZ%mean + probZ * mv_Zinfo(iz)%z
                if (probZ>dumvar) then
                    StatZ%mode = mv_Zinfo(iz)%z
                    dumvar = probZ
                end if
                CumSumProbZ(iz) = CumSumProbZ(iz-1) + probZ
                BatseRate(iz) = BatseRate(iz) + probZ
                !write(fileUnit,"(*(g0.8,:,','))") mv_Zinfo(iz)%z, ",", probZ
                write(fileUnit,"(*(g0.8))") probZ
            end do
            close(fileUnit)

            normCoef = 1._RK / CumSumProbZ(NZ)
            StatZ%mean = StatZ%mean * normCoef

            ! compute the quantiles
            iq = 1
            StatZ%Quantile = -999._RK
            loopZ: do iz = 1, NZ
                CumSumProbZ(iz) = CumSumProbZ(iz) * normCoef
                if (CumSumProbZ(iz)<QPROB(iq)) cycle loopZ
                StatZ%Quantile(iq) = mv_Zinfo(iz)%z
                iq = iq + 1
                if (iq>NQ) exit loopZ
            end do loopZ

            ! write out the quantiles
            write(fileUnitStatZ,"(*(g0.8,:,','))") Trigger(igrb), StatZ%mean, StatZ%mode, StatZ%Quantile

            call Timer%toc()
            itick = itick + 1
            write(output_unit,"(*(' ',g0))", advance="no"   ) CARRIAGE_RETURN, CLOCK_TICK(itick) &
                                                            , igrb, " out of  ", NGRB &
                                                            , "GRB redshift distributions generated in" &
                                                            , Timer%Time%total, "seconds."
            flush(output_unit)
            if (itick==4) itick = 0

        end do
        write(output_unit,"(*(g0))")
        close(fileUnitStatZ)

        !***************************************************************************************************************************
        ! generate BATSE redshift distribution file:
        !***************************************************************************************************************************

        write(output_unit,"(*(g0))")
        write(output_unit,"(*(g0))")
        write(output_unit,"(*(g0))") "Generating BATSE redshift distribution file..."
        call Timer%tic()

        fileName = outPath//"batse_zdist.txt"
        open(newunit=fileUnit,file=fileName,status="replace")
        write(fileUnit,"(*(g0))") "z,normedBatseRate,normedSFR,cumSumNormedBatseRate,cumSumNormedSFR"

        ! get the cumulative distributions
        iz = 1
        mv_Zinfo(iz)%logSFR = exp(mv_Zinfo(iz)%logSFR)
        CumSumBatseRate(iz) = BatseRate(iz)
        CumSumSFR(iz) = mv_Zinfo(iz)%logSFR
        do iz = 2, NZ
            mv_Zinfo(iz)%logSFR = exp(mv_Zinfo(iz)%logSFR)
            CumSumBatseRate(iz) = CumSumBatseRate(iz-1) + BatseRate(iz)
            CumSumSFR(iz) = CumSumSFR(iz-1) + mv_Zinfo(iz)%logSFR
        end do

        normCoef = 1._RK / CumSumSFR(NZ)
        do iz = 1, NZ
            write(fileUnit,"(*(g0.8,:,','))") mv_Zinfo(iz)%z &
                                            , normCoef * BatseRate(iz) &
                                            , normCoef * mv_Zinfo(iz)%logSFR &
                                            , normCoef * CumSumBatseRate(iz) &
                                            , normCoef * CumSumSFR(iz)
        end do
        close(fileUnit)

        call Timer%toc()
        write(output_unit,"(*(g0))") "Total Time: ", Timer%Time%delta, " seconds."
        write(output_unit,"(*(g0))")

    end subroutine zestimate

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    pure function getProbZ(iz,igrb) result(probZ)

        use Constants_mod, only: IK, RK
       !use WorldModelForBatseLGRB_mod, only: TIME_DILATION_EXPO
        implicit none
        integer(IK) , intent(in)    :: iz,igrb
        real(RK)    , parameter     :: INV_NSAMPLE_RK = 1._RK / real(NSAMPLE,kind=RK)
        real(RK)                    :: probZ
        real(RK)                    :: MeanSubtractedVar(NVAR)
        integer(IK)                 :: isample

        probZ = 0._RK
        do isample = 1,NSAMPLE

            ! observed data probability
            MeanSubtractedVar(1) = GRB%Event(igrb)%logPbol - ParaPost(isample)%Avg(1) + mv_Zinfo(iz)%logLisoLogPbolDiff
            MeanSubtractedVar(2) = GRB%Event(igrb)%logEpk  - ParaPost(isample)%Avg(2) + mv_Zinfo(iz)%logzplus1
            MeanSubtractedVar(3) = GRB%Event(igrb)%logsbol - ParaPost(isample)%Avg(3) + mv_Zinfo(iz)%logEisoLogSbolDiff
            MeanSubtractedVar(4) = GRB%Event(igrb)%logt90  - ParaPost(isample)%Avg(4) - mv_Zinfo(iz)%logzplus1!*TIME_DILATION_EXPO

            probZ   = probZ &
                    + mv_DetectionEfficiency(isample) * ParaPost(isample)%coef * exp( mv_Zinfo(iz)%logSFR - 0.5_RK &
                    * dot_product( MeanSubtractedVar , matmul(ParaPost(isample)%InvCovMat,MeanSubtractedVar) ) )

        end do
        probZ = probZ * INV_NSAMPLE_RK

    end function getProbZ

!***********************************************************************************************************************************
!***********************************************************************************************************************************

end module Zestimate_mod