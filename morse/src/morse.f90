program morse
    implicit none

    !##################################################################
    !### input variables ##############################################
    integer :: npos,v,nchannel
    real(8) :: xmin,xmax,rc0,p0,alpha,req,diss,mass

    !##################################################################
    !### derivated from the input variables ###########################
    real(8) :: delr

    !##################################################################
    !### vector variables #############################################
    complex(16), allocatable :: chi(:,:)
    real(8), allocatable :: pot(:,:), xmu12(:), x(:)
    real(8), allocatable :: ep(:,:)

    !##################################################################
    !### temp vector variables ########################################
    real(8), allocatable :: work(:)

    !##################################################################
    !### Integer variable used for loop ###############################
    integer :: n,i

    !##################################################################
    !### logical variable for file ####################################
    logical :: file_exists

    !##################################################################
    !### declaration of function morsefunc ############################
    real(8) :: morsefunc

    !##################################################################
    !### normalisation variable #######################################
    real(8) :: norme


    !##################################################################
    !### Used for read format of proper states ########################
    CHARACTER(2) :: FMT

    !##################################################################
    !### read the input ###############################################
    INQUIRE(FILE="input", EXIST=file_exists)
    if (file_exists) then
        open(5,file="input",status='old')
    end if
    namelist /iofile/ npos,v,xmin,xmax,rc0,p0,alpha,req,diss,mass,nchannel
    read(5,iofile)
    if (file_exists) then
        close(5)
    end if

    !##################################################################
    !### allocate vector variables ####################################
    allocate(chi(nchannel,npos),pot(nchannel,npos),x(npos),xmu12(npos))
    allocate(ep(v,npos),work(npos))

    !##################################################################
    !### definition of internuclear distance grid #####################
    delr=(xmax-xmin)/(npos-1)
    do i=1,npos
        x(i)=xmin+(i-1)*delr
        !write(*,*) i, x(i), delr
        !read(*,*) FMT
    enddo

    !##################################################################
    !### Potential energy surface creation #####################
    call pot_spec(pot(1,:),pot(2,:),xmu12, npos,delr,xmin)



    do n=1,v
        do i=1,npos
            ep(n,i)=morsefunc(diss,0.72d0,mass,req,x(i),n-1) !construction des n etats vibrationnels sur la grille
            work(i)=(dabs(ep(n,i)))**2
        enddo

        call simpson(npos,delr,work,norme)
        ep(n,:)=ep(n,:)/sqrt(norme) !normalisation des etats vibrationnels

    enddo
    open(10,file='properstate.dat')
    !write(10)npos
    !write(10)v
    WRITE(FMT,'(I2)') v+1
    do i=1,npos
        WRITE(10,"(" // ADJUSTL(FMT) // "(E28.20E3,4x))") x(i),(ep(n,i),n=1,v)
    enddo

    call eval(chi, delr, xmin, p0, rc0, alpha, npos)
    !##################################################################
    !### allocate vector variables ####################################
    IF (ALLOCATED (chi)) DEALLOCATE (chi)
    IF (ALLOCATED (pot)) DEALLOCATE (pot)
    IF (ALLOCATED (x)) DEALLOCATE (x)
    IF (ALLOCATED (xmu12)) DEALLOCATE (xmu12)
    IF (ALLOCATED (ep)) DEALLOCATE (ep)
    IF (ALLOCATED (work)) DEALLOCATE (work)



end program morse


subroutine pot_spec(v1, v2, xmu12, npos, delr, rdeb)
    !
    !     *******************************************************************
    !
    !       Calcul des potentiels et moments dipolaires
    !       doit avoir les expressions analytiques
    !       (si numeriques : doit avoir un 'read')
    !
    !     *******************************************************************
    !
    !
    integer npos
    double precision v1(npos),v2(npos),xmu12(npos)
    double precision delr, rdeb
    !
    double precision z0, s, req, x1, x2, y, r
    integer l
    !##################################################################
    !### Used for read format of proper states ########################
    CHARACTER(2) :: FMT
    !
    z0=.1026277d0
    s=.72d0
    req=2.d0
    x1=1.d0
    x2=-1.11d0
    y=-.055d0
    open(10,file='pot.dat')
    WRITE(FMT,'(I2)') 4
    do l = 1, npos
        r = rdeb+(l-1)*delr
        v1(l) = z0*(dexp(-2.d0*s*(r-req)) - 2.d0*x1*dexp(-s*(r-req)))
        v2(l) = z0*(dexp(-2.d0*s*(r-req)) - 2.d0*x2*dexp(-s*(r-req)))
        xmu12(l) = 1.07d0 + (.396d0/(s*y))*(1.d0-dexp(-s*y*(r-req)))
        if((r.gt.12.d0).and.(xmu12(l).gt.0.5d0*r))then
            xmu12(l)=0.5d0*r
        endif
        WRITE(10,"(" // ADJUSTL(FMT) // "(E28.20E3),4x)") r,v1(l),v2(l),xmu12(l)
    end do
    close(10)
    return
end subroutine pot_spec

function laguerrel(a,n,x)
    real(8) :: laguerrel,factrl
    integer a,n,j
    real(8) :: x,lagtmp
    lagtmp=0.d0
    do 22 j=0,n
        lagtmp=lagtmp+(1.d0/factrl(j))*((-x)**j)*factrl(n+a)/(factrl(n-j)*factrl(j+a))
22  continue
    laguerrel=lagtmp
    return
end function laguerrel



function morsefunc(diss,smalla,xmu,requ,r,nu)
    real(8) :: morsefunc,laguerrel
    real(8) :: diss,smalla,xmu
    real(8) :: biga,bigc,enu,alpha
    integer :: nu,m,factrl
    real(8) :: x,arg,r,requ
    real(8) :: norm
    if(r.lt.64.d0)then
        biga = (dsqrt(2.d0*xmu))/smalla
        bigc = biga*dsqrt(diss)
        enu = -((bigc-nu-.5d0)**2)/biga**2
        alpha = bigc-nu-0.5d0
        arg=dexp(-smalla*(r-requ))
        x=2.d0*bigc*arg
        m = 2*(idint(bigc)-nu)-1
        morsefunc = laguerrel(m,nu,x)
        morsefunc = morsefunc * (x**(idint(bigc)))
        morsefunc = morsefunc / (x**nu)
        morsefunc = morsefunc / dsqrt(x)
        morsefunc = morsefunc * dexp(-x/2.d0)

        norm = smalla * factrl(nu) * (2.d0*bigc - 2.d0*nu - 1.d0)
        norm = norm / factrl(m+nu)
        norm = dsqrt(norm)

        morsefunc = morsefunc*norm
    else
        morsefunc = 0.d0
    endif
    !write(*,*) r,nu,morsefunc

    return
end function morsefunc


!*******************************************************************
!
!       Calcul du paquet d'onde initial
!
!*******************************************************************


subroutine eval(cw, delr, rdeb,p0, rc0, alpha, npos)

    integer npos
    double complex cw(2,npos)
    double precision delr, rdeb, p0, rc0, alpha
    double precision pi, r
    double complex cnul, cim, cpoi, cval, arg
    integer  l
    !##################################################################
    !### Used for read format of proper states ########################
    CHARACTER(2) :: FMT
    !

    cnul = dcmplx(0.d0,0.d0)
    cim = dcmplx(0.d0,1.d0)
    pi = 3.141592654d0
    cpoi = cdsqrt(cdsqrt(dcmplx(2.d0*alpha/pi,0.d0)))
    r = rdeb-delr
    open(10,file='init.dat')
    WRITE(FMT,'(I2)') 3
    do l = 1, npos
        r = r + delr
        arg = dcmplx(-alpha*(r-rc0)**2, p0*(r-rc0))
        cval = cpoi*cdexp(arg)
        cw(1,l) = cval
        cw(2,l) = cnul
        WRITE(10,"(3(E28.20E3),4x)") r,real(cw(1,l)),real(cw(2,l))
    end do
    close(10)

    return
end subroutine eval


double precision function factrl(n)

    !real(8) factrl
    integer n,j,ntop
    double precision a(100)
    data ntop,a(1)/0,1./
    if(n.lt.0) then
        pause 'negative factorial'
    else if(n.le.ntop) then
        factrl=a(n+1)
    else if(n.le.100) then
        do 11 j=ntop+1,n
            a(j+1)=j*a(j)
11      continue
        ntop=n
        factrl=a(n+1)
    else
        factrl = dsqrt(6.283185307d0*n)*(n*dexp(-1.d0))**n
    endif
    return
end function factrl


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
