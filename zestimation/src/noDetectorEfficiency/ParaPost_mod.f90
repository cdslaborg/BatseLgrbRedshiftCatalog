module ParaPost_mod

    use Constants_mod, only: IK, RK, PI

    implicit none

    character(*), parameter :: MODULE_NAME = "@ParaPost_mod"

    integer(IK) , parameter :: NSAMPLE = 1000_IK
    integer(IK) , parameter :: NVAR = 4     ! number of GRB attributes used in the world model
    integer(IK) , parameter :: NPAR = 16    ! number of world model's parameters

    ! the normalization factor of the multivariate log-normal distribution
    real(RK)    , parameter :: INV_TWOPI_POW_HALFNVAR = 1._RK / sqrt( (2._RK*PI)**NVAR )

    ! BATSE threshold parameters
    type :: Thresh_type
        real(RK) :: avg, invSqrt2Std
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

    subroutine readParaPostOld(ParaPostFilePath)
        use Constants_mod, only: IK, RK, SQRT2, LN10
        use Matrix_mod, only: getInvPosDefMatSqrtDet
        implicit none
        character(*), intent(in)            :: ParaPostFilePath
        integer(IK)                         :: i, j, ParaPostFileUnit, isample, logLike
        real(RK), parameter                 :: INV_SQRT2 = 1._RK / SQRT2

        open(newunit=ParaPostFileUnit,file=ParaPostFilePath,status="old")
        read(ParaPostFileUnit,*)
        do isample = 1,NSAMPLE

            read(ParaPostFileUnit,*)  logLike &
                                    ,  ParaPost(isample)%Avg(1:NVAR) &
                                    , (ParaPost(isample)%InvCovMat(i,i),i=1,NVAR) & ! log10 of standard deviations
                                    ,  ParaPost(isample)%InvCovMat(2:4,1) &         ! Rho: LisoEpkz, LisoEiso, LisoT90z
                                    ,  ParaPost(isample)%InvCovMat(3:4,2) &         ! Rho: EpkzEiso, EpkzT90z
                                    ,  ParaPost(isample)%InvCovMat(4:4,3) &         ! Rho: EisoT90z
                                    ,  ParaPost(isample)%Thresh%avg &               ! log10 of threshold mean
                                    ,  ParaPost(isample)%Thresh%invSqrt2Std         ! log10 of threshold standard deviation

            !write(*,"(*(g15.8))")
            !write(*,"(*(g15.8))") "mean: ", ParaPost(isample)%Avg
            !do i = 1,NVAR
            !    write(*,"(*(g15.8))") "CovMat: ", ParaPost(isample)%InvCovMat(i,1:NVAR)
            !end do
            !write(*,"(*(g15.8))") "Thresh: ", Thresh(isample)
            !write(*,"(*(g15.8))")
            !read(*,*)

            ! convert to log Neper base
            ParaPost(isample)%Avg = LN10 * ParaPost(isample)%Avg

            do i = 1, NVAR
                ParaPost(isample)%InvCovMat(i,i) = LN10 * 10._RK**ParaPost(isample)%InvCovMat(i,i)   ! This is now standard deviation
            end do
            do i = 1, NVAR
                do j = i+1,NVAR
                    ParaPost(isample)%InvCovMat(i,j)    = ParaPost(isample)%InvCovMat(j,i) &
                                                        * ParaPost(isample)%InvCovMat(i,i) &
                                                        * ParaPost(isample)%InvCovMat(j,j)
                end do
            end do
            do i = 1, NVAR
                ParaPost(isample)%InvCovMat(i,i) = ParaPost(isample)%InvCovMat(i,i)**2    ! This is now squared standard deviation
               !ParaPost(isample)%InvCovMat(i+1:NVAR,i) = ParaPost(isample)%InvCovMat(i,i+1:NVAR)
            end do

            ! get the inverse covariance matrix and determinant
           !write(*,*); write(*,*)
           !do i = 1, NVAR
           !    write(*,"(*(g20.13))") (ParaPost(isample)%InvCovMat(i,j),j=1,NVAR)
           !end do
            call getInvPosDefMatSqrtDet ( nd = NVAR &
                                        , MatInvMat = ParaPost(isample)%InvCovMat &
                                        , sqrtDetInvPosDefMat = ParaPost(isample)%coef )
           !write(*,"(*(g20.13))") ParaPost(isample)%coef
           !write(*,*) 
           !do i = 1, NVAR
           !    write(*,"(*(g20.13))") (ParaPost(isample)%InvCovMat(i,j),j=1,NVAR)
           !end do
           !write(*,*); write(*,*)
           !read(*,*);

            ParaPost(isample)%coef = INV_TWOPI_POW_HALFNVAR * ParaPost(isample)%coef
            !write(*,"(*(g15.8))")
            !write(*,"(*(g15.8))") "mean: ", ParaPost(isample)%Avg
            !do i = 1,NVAR
            !    write(*,"(*(g15.8))") "CovMat: ", ParaPost(isample)%InvCovMat(i,1:NVAR)
            !end do
            !write(*,"(*(g15.8))")
            !read(*,*)

            ! threshold parameters
            ParaPost(isample)%Thresh%avg = LN10 * ParaPost(isample)%Thresh%avg ! convert to log Neper base
            ParaPost(isample)%Thresh%invSqrt2Std = INV_SQRT2 / (LN10*10._RK**ParaPost(isample)%Thresh%invSqrt2Std)

        end do

    end subroutine readParaPostOld

!***********************************************************************************************************************************
!***********************************************************************************************************************************

    subroutine readParaPost(ParaPostFilePath)
        use Constants_mod, only: IK, RK, SQRT2
        use Matrix_mod, only: getInvPosDefMatSqrtDet
        implicit none
        character(*), intent(in)            :: ParaPostFilePath
        integer(IK)                         :: i, j, ParaPostFileUnit, isample, logLike
        real(RK), parameter                 :: INV_SQRT2 = 1._RK / SQRT2

        open(newunit=ParaPostFileUnit,file=ParaPostFilePath,status="old")
        read(ParaPostFileUnit,*)
        do isample = 1,NSAMPLE

            read(ParaPostFileUnit,*)  logLike &
                                    ,  ParaPost(isample)%Avg(1:NVAR) &
                                    , (ParaPost(isample)%InvCovMat(i,i),i=1,NVAR) & ! log10 of standard deviations
                                    ,  ParaPost(isample)%InvCovMat(2:4,1) &         ! Rho: LisoEpkz, LisoEiso, LisoT90z
                                    ,  ParaPost(isample)%InvCovMat(3:4,2) &         ! Rho: EpkzEiso, EpkzT90z
                                    ,  ParaPost(isample)%InvCovMat(4:4,3) &         ! Rho: EisoT90z
                                    ,  ParaPost(isample)%Thresh%avg &               ! log10 of threshold mean
                                    ,  ParaPost(isample)%Thresh%invSqrt2Std         ! log10 of threshold standard deviation

            do i = 1, NVAR
                ParaPost(isample)%InvCovMat(i,i) = exp(ParaPost(isample)%InvCovMat(i,i))   ! This is now standard deviation
            end do
            do i = 1, NVAR
                do j = i+1,NVAR
                    ParaPost(isample)%InvCovMat(i,j)    = tanh(ParaPost(isample)%InvCovMat(j,i)) &
                                                        * ParaPost(isample)%InvCovMat(i,i) &
                                                        * ParaPost(isample)%InvCovMat(j,j)
                end do
            end do
            do i = 1, NVAR
                ParaPost(isample)%InvCovMat(i,i) = ParaPost(isample)%InvCovMat(i,i)**2    ! This is now squared standard deviation
               !ParaPost(isample)%InvCovMat(i+1:NVAR,i) = ParaPost(isample)%InvCovMat(i,i+1:NVAR)
            end do

            ! get the inverse covariance matrix and determinant
           !write(*,*); write(*,*)
           !do i = 1, NVAR
           !    write(*,"(*(g20.13))") (ParaPost(isample)%InvCovMat(i,j),j=1,NVAR)
           !end do
            call getInvPosDefMatSqrtDet ( nd = NVAR &
                                        , MatInvMat = ParaPost(isample)%InvCovMat &
                                        , sqrtDetInvPosDefMat = ParaPost(isample)%coef )
           !write(*,"(*(g20.13))") ParaPost(isample)%coef
           !write(*,*) 
           !do i = 1, NVAR
           !    write(*,"(*(g20.13))") (ParaPost(isample)%InvCovMat(i,j),j=1,NVAR)
           !end do
           !write(*,*); write(*,*)
           !read(*,*);

            ParaPost(isample)%coef = INV_TWOPI_POW_HALFNVAR * ParaPost(isample)%coef
            !write(*,"(*(g15.8))")
            !write(*,"(*(g15.8))") "mean: ", ParaPost(isample)%Avg
            !do i = 1,NVAR
            !    write(*,"(*(g15.8))") "CovMat: ", ParaPost(isample)%InvCovMat(i,1:NVAR)
            !end do
            !write(*,"(*(g15.8))")
            !read(*,*)

            ! threshold parameters
            ParaPost(isample)%Thresh%avg = LN10 * ParaPost(isample)%Thresh%avg ! convert to log Neper base
            ParaPost(isample)%Thresh%invSqrt2Std = INV_SQRT2 / (LN10*10._RK**ParaPost(isample)%Thresh%invSqrt2Std)

        end do

    end subroutine readParaPost

!***********************************************************************************************************************************
!***********************************************************************************************************************************

end module ParaPost_mod