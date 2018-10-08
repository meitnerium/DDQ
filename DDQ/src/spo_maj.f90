
SUBROUTINE simpson (N,H,FI,S)
    !
    ! Subroutine for integration over f(x) with the Simpson rule.  FI:
    ! integrand f(x); H: interval; S: integral.  Copyright (c) Tao Pang 1997.
    !
    IMPLICIT NONE
    INTEGER, INTENT (IN) :: N
    INTEGER :: I
    real(8) , INTENT (IN) :: H
    real(8) :: S0,S1,S2
    real(8), INTENT (OUT) :: S
    real(8), INTENT (IN), DIMENSION (N) :: FI
    !
    S  = 0.0
    S0 = 0.0
    S1 = 0.0
    S2 = 0.0
    DO I = 2, N-1, 2
        S1 = S1+FI(I-1)
        S0 = S0+FI(I)
        S2 = S2+FI(I+1)
    END DO
    S = H*(S1+4.0*S0+S2)/3.0
    !
    ! If N is even, add the last slice separately
    !
    IF (MOD(N,2).EQ.0) S = S &
    +H*(5.0*FI(N)+8.0*FI(N-1)-FI(N-2))/12.0
END SUBROUTINE simpson

!!!!!!!!!!!!!!!!!!!!!!!!!!!!! gerer field
!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!
!!!!!!!!!!!
!!!!!! modifier pour gerer champ1 et champ2 au lie de calculer field
!!! TODO
!!!!!!  ajouter
subroutine airesint (int1, int2, int3, delt,champ1,champ2)
    real(8) :: int1, int2, int3, champ1, champ2
    real(8) :: delt
    real(8) :: tmp , tmp2

    tmp = int1
    int1 = int1 - &
    (champ1 + champ2 ) * 0.25d0 * delt

    int2 = int2 + (tmp + int1) * 0.5d0 * delt
    int3 = int3 + (tmp**2 + int1**2) * 0.5d0 * delt
    return
end subroutine airesint

subroutine calc_champ(champ,wir,t,phase,nc,pi,E0)
    real(8) :: champ, wir, t, beginpulse, phase,pi,e,g,dere,derg,E0,beta
    integer :: nc
    !write(*,*) nc
    if (dabs(t).le.(dfloat(nc)*pi/wir)) then
        e=dsin(wir*t+phase)
        !dere=wir*dcos(wir*t+phase*pi)
        dere=dcos(wir*t+phase)
        g=dcos(wir*t/(2*nc))**2
        !derg=-wir/nc*dsin(wir*t/(2*nc))*dcos(wir*t/(2*nc))
        derg=dsin(wir*t/(2*nc))*dcos(wir*t/(2*nc))
        champ=E0*(e*derg+g*dere)
    else
        champ=0.d0
    end if
    return
end subroutine calc_champ



subroutine calczcut(zcutA, zcutI, rdeb, delr, npbig, r0cut, scut)
    integer :: npbig
    complex(8) :: zcutA(npbig), zcutI(npbig)
    real(8) :: rdeb, delr, r0cut, scut, pi
    real(8) :: r, r1, tmp
    integer :: n
    r1 = r0cut + scut
    pi = dacos(-1.d0)
    do n = 1, npbig
        r = rdeb + (n-1)*delr
        if(r.le.r0cut) then
            tmp =0.d0
        else if((r.gt.r0cut).and.(r.le.r1)) then
            tmp = (dsin(0.5d0*(r-r0cut)*pi/scut))**2
        else
            tmp = 1.d0
        end if
        !         write(*,*) r,tmp
        !c     **************************************************************
        !c         tmp = 1.d0 + dexp((r-r0cut)/scut)
        !c         tmp = 1.d0/tmp
        !c     **************************************************************
        zcutI(n) = dcmplx(1.d0 - tmp, 0.d0)
        zcutA(n) = dcmplx(tmp, 0.d0)
    !c     **************************************************************
    !c         zcutA(n) = dcmplx(1.d0 - tmp, 0.d0)
    !c         zcutI(n) = dcmplx(tmp, 0.d0)
    !c     **************************************************************
    !c      write(15,1003) r, dreal(zcutI(n)), dreal(zcutA(n))
    end do
1003 format(3(e16.8e3,2x))
    return
end subroutine calczcut




subroutine splitop (cw1, cw2, zetdt, pot, xmu12, npos,champ, delr, xmu, delt)

    !     *******************************************************************
    !
    !   Calcul du propagateur d'évolution temporelle
    !
    !     *******************************************************************




    integer npos

    real(8) delr,delk,kmax,kmin, champ, xmu, delt,nposreel,x(npos)
    complex(8) cw1(npos),cw2(npos), zetdt(npos)
    real(8) pot(2,npos),xmu12(npos)

    complex(8) cun, cim, cnul, cwtemp, cphase1, cphase2
    complex(8) ctemp1, ctemp2
    real(8) xmue, vp1, vp2, delta
    real(8) thet,norme,normedeb,k(npos),pi
    integer ll,i


    pi=MATH_PI
    delk=2d0*pi/(npos*delr)
    do ll = 1, npos
        if(ll.le.npos/2)then
            k(ll) = (ll-1) * delk
        else
            k(ll) = -(npos-ll+1) * delk
        endif
    enddo
    nposreel=npos*1.d0
    cun=dcmplx(1.0d0,0.d0)
    cim=dcmplx(0.d0,1.0d0)
    cnul=dcmplx(0.d0,0.d0)
    do ll = 1, npos
        xmue = xmu12(ll) * champ
        delta = (pot(2,ll) - pot(1,ll))**2 + (2.d0*xmue)**2
        delta = dsqrt(delta)
        thet = 0.5d0*datan((2.d0*xmue)/(pot(2,ll)-pot(1,ll)))

        vp1 = (pot(2,ll) + pot(1,ll) - delta)*0.5d0
        vp2 = (pot(2,ll) + pot(1,ll) + delta)*0.5d0
        cwtemp  = dcos(thet)*cw1(ll)-dsin(thet)*cw2(ll)
        cw2(ll) = dsin(thet)*cw1(ll)+dcos(thet)*cw2(ll)
        cw1(ll) = cwtemp
        cphase1 = cdexp(-cim*vp1*delt/2.d0)
        cphase2 = cdexp(-cim*vp2*delt/2.d0)
        cw1(ll) = cw1(ll)*cphase1
        cw2(ll) = cw2(ll)*cphase2
        cwtemp  = dcos(thet)*cw1(ll)+dsin(thet)*cw2(ll)
        cw2(ll) =-dsin(thet)*cw1(ll)+dcos(thet)*cw2(ll)
        cw1(ll) = cwtemp
    end do

    call zexptdt(zetdt, npos, delk, xmu, delt)

    !call COMPLEX_1D_DOUBLE_EX1(cw1,-1,npos)
    ! call COMPLEX_1D_DOUBLE_EX1(cw2,-1,npos)


    call fourier(cw1,npos,-1)
    call fourier(cw2,npos,-1)



    !     *******************************************************************
    !   multiplication vecteur par matrice (z pour complexe)
    !     *******************************************************************

    call ZVEM(npos, zetdt(1), 1, cw1(1), 1, cw1(1), 1)
    call ZVEM(npos, zetdt(1), 1, cw2(1), 1, cw2(1), 1)

    !call COMPLEX_1D_DOUBLE_EX1(cw1,1,npos)
    !call COMPLEX_1D_DOUBLE_EX1(cw2,1,npos)
    call fourier(cw1,npos,1)
    call fourier(cw2,npos,1)
    cw1=cw1/npos
    cw2=cw2/npos

    do ll = 1, npos
        xmue = xmu12(ll) * champ
        delta = (pot(2,ll) - pot(1,ll))**2 + (2.d0*xmue)**2
        delta = dsqrt(delta)
        if(dabs(xmue).gt.1.d-16)then
            thet = 0.5d0*datan((2.d0*xmue)/(pot(2,ll)-pot(1,ll)))
        else
            thet = 0.d0
        end if
        vp1 = (pot(2,ll) + pot(1,ll) - delta)*0.5d0
        vp2 = (pot(2,ll) + pot(1,ll) + delta)*0.5d0
        cwtemp  = dcos(thet)*cw1(ll)-dsin(thet)*cw2(ll)
        cw2(ll) = dsin(thet)*cw1(ll)+dcos(thet)*cw2(ll)
        cw1(ll) = cwtemp
        cphase1 = cdexp(-cim*vp1*delt/2.d0)
        cphase2 = cdexp(-cim*vp2*delt/2.d0)
        cw1(ll) = cw1(ll)*cphase1
        cw2(ll) = cw2(ll)*cphase2
        cwtemp  = dcos(thet)*cw1(ll)+dsin(thet)*cw2(ll)
        cw2(ll) =-dsin(thet)*cw1(ll)+dcos(thet)*cw2(ll)
        cw1(ll) = cwtemp
    end do
    return
end subroutine





!     *******************************************************************
!
!   Calcul du vecteur d'energie cinetique en representation impulsion
!
!     *******************************************************************

subroutine zexptdt(etdt, npos, xk1, xmu, delt)
    integer npos
    double complex etdt(npos)
    double precision xmu, delt
    double complex  cim
    double precision pi, xk1, xk(npos), arg
    integer ll

    pi = 3.141592654d0
    cim = dcmplx(0.d0, 1.d0)
    do ll = 1, npos
        if(ll.le.npos/2)then
            xk(ll) = (ll-1) * xk1
        else
            xk(ll) = -(npos-ll+1) * xk1
        endif
        arg = ((xk(ll)*xk(ll))/(2.d0*xmu)) * delt
        etdt(ll) = cdexp(-cim*arg)
    end do



    return
end subroutine

!******************************************************
!   Transformee de Fourier
!******************************************************
subroutine fourier(F,N,isig)
    implicit none
    integer :: N, i, j,isig,m,mmax,istep
    double precision :: wr,wi,wpr,wpi,wtemp,theta,tempr,tempi
    double precision :: G(2*N),normei,normef
    double complex :: F(N)
    do i=1,N
        G(2*i-1)=dreal(F(i))
        G(2*i)=dimag(F(i))
    enddo
    !select case (isig)
    !   case (1)
    !        call simpson(N,dx,cdabs(F)**2, normei)
    !   case (-1)
    !        call simpson(N,dk,cdabs(F)**2, normei)
    !end select
    j=1
    do i=1,2*N,2
        if (j.gt.i) then
            tempr=(G(j))
            tempi=(G(j+1))
            G(j)=G(i)
            G(j+1)=G(i+1)
            G(i)=tempr
            G(i+1)=tempi
        endif
        m=N
        do while ((M.ge.2).and.(j.gt.m))
            j=j-m
            m=m/2
        enddo
        j=j+m
    enddo
    mmax=2
    do while (2*N.gt.mmax)
        istep=2*mmax
        theta=6.28318530717959d0/(isig*mmax)
        wpr=-2d0*dsin(0.5d0*theta)**2
        wpi=dsin(theta)
        wr=1d0
        wi=0d0
        do m=1,mmax,2
            do i=m,2*N,istep
                j=i+mmax
                tempr=sngl(wr)*G(j)-sngl(wi)*G(j+1)
                tempi=sngl(wr)*G(j+1)+sngl(wi)*G(j)
                G(j)=G(i)-tempr
                G(j+1)=G(i+1)-tempi
                G(i)=G(i)+tempr
                G(i+1)=G(i+1)+tempi
            enddo
            wtemp=wr
            wr=wr*wpr-wi*wpi+wr
            wi=wi*wpr+wtemp*wpi+wi
        enddo
        mmax=istep
    enddo
    do i=1,N
        tempr=G(2*i-1)
        tempi=G(2*i)
        F(i)=dcmplx(tempr,tempi)

    enddo

    return
end subroutine




