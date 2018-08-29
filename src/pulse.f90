module pulse
contains

subroutine pulsechoice(pulsetype,champ,t,nt,dt,logfile,E0,w,phase,le0wattcm2,tc,te,tf)
integer :: pulsetype,nt,le0wattcm2
real(8) :: champ(nt),t(nt),dt,E0,w,phase,tc,te,tf
if (pulsetype.eq.1) then
    write(*,*) "this is a test !!!"
    call pulsepseudocarre(champ,t,nt,dt,logfile,E0,w,phase,le0wattcm2,tc,te,tf)
    write(*,*) "this is a test !!!2"
else if (pulsetype.eq.2) then
    call sqsinpulse(champ,t,nt,dt,logfile,E0,w,phase,le0wattcm2,tc,te,tf)
end if 
end subroutine pulsechoice

subroutine sqsinpulse(champ,t,nt,dt,logfile,E0,w,phase,le0wattcm2,tc,te,tf)
    use gen
    include 'mkl_dfti_examples.fi'
    integer nt,le0wattcm2,logfile
    real(8) pulse(nt),champ(nt),t(nt),period,I,wcm1,phase
    real(8) w,tdeb,te,tf,E0,alpha,dt,t0,E0wattcm2,tc
    write(logfile,*) "w in atomic unit = " , w

    wcm1=au2cm1(w)
    phase=phase*MATH_PI
    write(logfile,*) "w in cm-1 = " , wcm1
    write(*,*) "* E0 :  ", E0
    period=MATH_PI*2.d0/w


    alpha=1/200.d0
    tc=tc/2.418884326505d-2
    te=te/2.418884326505d-2
    tf=tf/2.418884326505d-2
    !tc=9.5d0/2.418884326505d-2-period/2.d0   ! piegeant
    !tc=12.6d0/2.418884326505d0
    !tc=11.2d0/2.418884326505d-2

    !/2.418884326505d-2-period/2.d0   ! non-piegeant
    te=tc+6.d0*period
    tf=te+5.d0*period
    t0=0.d0
    dt=(tf-t0)/(nt-1)
    write(*,*) "this is a test !!!3"
    write(logfile,'(" period = ", E16.8 , " u.a. , ",  E16.8 , " fs" )')    period,    period*tau2fs
    if (le0wattcm2.eq.1) then
        E0=wattcm22au(E0)
    end if


    E0wattcm2=au2wattcm2(E0)
    write(logfile,'(" E0 = ", E16.8 , " u.a. , ",  E16.8 , "  W/cm2" )') E0 , E0wattcm2
    write(logfile,*) "phase = ", phase/MATH_PI , " Pi"
    write(logfile,'(" tc = ", E16.8 , " a.u. ," , E16.8, " fs" )') tc , tc*tau2fs
    write(logfile,'(" te = " , E16.8 , " a.u. ," , E16.8, " fs" )') te , te*tau2fs
    write(logfile,*) "tf = ", tf, "a.u." , tf*tau2fs ," fs"
    write(logfile,*) "nt = ", nt
    write(logfile,*) "dt = ", dt, "a.u." , dt*tau2fs ," fs"

        write(*,*) delta
    !    pause
    write(*,*) "nt in pulse" , nt
    do j=1,nt
        t(j) = t0 + (j-1)*dt
        if (t(j).lt.tc) then
            pulse(j)=0.d0
            !E0*dexp(-alpha*(t(j)-tc)**2)
        else if (t(j).lt.te) then
            !TODO, ici je dois multiplier par la pulse
            pulse(j)=E0*dsin(w*(t(j)-tc))**2
        else
            pulse(j)=0.d0
            !E0*dexp(-alpha*(t(j)-te)**2)
        end if
        champ(j)=-dcos(w*(t(j)-tc)+phase)*pulse(j)
        if (abs(pulse(j)).lt.1.d-100) then
            pulse(j)=0.d0
        end if
        if (abs(champ(j)).lt.1.d-100) then
            champ(j)=0.d0
        end if
        !write(56788+ph,'(5E16.8)')        t(j),pulse(j),champ(j)
    end do

end subroutine sqsinpulse


subroutine pulsepseudocarre(champ,t,nt,dt,logfile,E0,w,phase,le0wattcm2,tc,te,tf)
    use gen
    include 'mkl_dfti_examples.fi'
    integer nt,le0wattcm2
    real(8) pulse(nt),champ(nt),t(nt),period,I,wcm1,phase
    real(8) w,tdeb,te,tf,E0,alpha,dt,t0,E0wattcm2,tc
    write(logfile,*) "w in atomic unit = " , w 
    
    wcm1=au2cm1(w)
    phase=phase*MATH_PI
    write(logfile,*) "w in cm-1 = " , wcm1 
    write(*,*) "* E0 :  ", E0 
    period=MATH_PI*2.d0/w
    alpha=1/200.d0
    tc=tc/2.418884326505d-2
    te=te/2.418884326505d-2
    tf=tf/2.418884326505d-2
    !tc=9.5d0/2.418884326505d-2-period/2.d0   ! piegeant
    !tc=12.6d0/2.418884326505d0
    !tc=11.2d0/2.418884326505d-2

    !/2.418884326505d-2-period/2.d0   ! non-piegeant
    te=tc+6.d0*period
    tf=te+5.d0*period
    t0=0.d0
    dt=(tf-t0)/(nt-1)
    write(*,*) "this is a test !!!3"
    write(logfile,'(" period = ", E16.8 , " u.a. , ",  E16.8 , " fs" )')    period,    period*tau2fs
    if (le0wattcm2.eq.1) then
        E0=wattcm22au(E0)
    end if


    E0wattcm2=au2wattcm2(E0)
    write(logfile,'(" E0 = ", E16.8 , " u.a. , ",  E16.8 , "  W/cm2" )') E0 , E0wattcm2 
    write(logfile,*) "phase = ", phase/MATH_PI , " Pi"
    write(logfile,'(" tc = ", E16.8 , " a.u. ," , E16.8, " fs" )') tc , tc*tau2fs 
    write(logfile,'(" te = " , E16.8 , " a.u. ," , E16.8, " fs" )') te , te*tau2fs 
    write(logfile,*) "tf = ", tf, "a.u." , tf*tau2fs ," fs"
    write(logfile,*) "nt = ", nt
    write(logfile,*) "dt = ", dt, "a.u." , dt*tau2fs ," fs"

        write(*,*) delta
    !    pause
    write(*,*) "nt in pulse" , nt
    do j=1,nt
        t(j) = t0 + (j-1)*dt
        if (t(j).lt.tc) then
            pulse(j)=0.d0
            !E0*dexp(-alpha*(t(j)-tc)**2)
        else if (t(j).lt.te) then
            pulse(j)=E0
        else 
            pulse(j)=0.d0
            !E0*dexp(-alpha*(t(j)-te)**2)
        end if
        champ(j)=-dcos(w*(t(j)-tc)+phase)*pulse(j)
        if (abs(pulse(j)).lt.1.d-100) then
            pulse(j)=0.d0
        end if
        if (abs(champ(j)).lt.1.d-100) then
            champ(j)=0.d0
        end if
        !write(56788+ph,'(5E16.8)')        t(j),pulse(j),champ(j)
    end do

end subroutine pulsepseudocarre

subroutine pulsesinc(pulset,t,nt,f0,champ,tc,dw,ph)
    use gen
    include 'mkl_dfti_examples.fi'
    integer :: nt,n,ph
    real(8) :: t(nt),wcm1,wop,wg,wmin,wmax,dw,dt,tc,f0
    real(8) :: pulset(nt),champ(nt)
    character(40) :: numchr
!    dw=dw/(2.d0*MATH_PI) ! cm-1
    dw=cm12au(dw) ! a.u.
    write(*,*) 'dw = ', dw
    wcm1=2200.d0
    write(*,*) "Tc = ", tc
!    pause 701
    write(*,*) "dt    = ", t(2)-t(1)
    
    write(numchr,*) ph
    open(unit=5555,name="pulse_"//trim(adjustl(numchr))//".dat",status='replace')
    do n=1,nt
        pulset(n)=f0*dsin(dw*(t(n)-tc))/(dw*(t(n)-tc))
 ! (2.d0*MATH_PI*c*)d      f0
!        wau(n)=nm2au(w(n))
 !       if(cdabs(pulsew(n))**2.lt.(1.d-100))then
!            pulsew(n)=(0.d0,0.d0)
!        end if
        write(5555,'(I4,6E16.8)')n,t(n),t(n)*2.418884326505E-2,pulset(n),pulset(n)**2,pulset(n)**2*champ(n),champ(n)
    end do
    close(5555)
end subroutine pulsesinc   

subroutine pulsefreqgauss(w,nw)
    use gen
    use spo
    include 'mkl_dfti_examples.fi'
    integer :: nw,gaussp,wp,j,fft,n
    double precision :: w(nw),wau(nw),wcm1,wop,wg,wmin,wmax,dw,dt,t
    double complex :: pulsew(nw)
    fft=2
    wmin=100.d0
    wmax=50000.d0
    t=0.d0
    

    wcm1=2200.d0
    wop=wcm1!/219474.63068d0
    wg=350.d0!*2.d0*3.141592654d0/wop
    gaussp=10
    wp=2200.d0

    j=0
    dw=(wmax-wmin)/(nw-1)
    dt=(2.d0*MATH_PI)/(wmax-wmin)
    write(*,*) "dt    = ", dt
    do n=1,nw
        w(n)=wmin+(wmax-wmin)*(n-1)/(nw-1)
        pulsew(n)=dexp(-2.d0*dabs((w(n) - wp)/wg)**gaussp)
!        wau(n)=nm2au(w(n))
        if(cdabs(pulsew(n))**2.lt.(1.d-100))then
            pulsew(n)=(0.d0,0.d0)
        end if
        write(5555,'(I4,4E16.8)')n,wau(n)*1.d-7,dreal(pulsew(n)),dimag(pulsew(n)),cdabs(pulsew(n))**2
    end do
    if (fft.eq.1)then
      call fourier(pulsew,nw,1)
    elseif (fft.eq.2.)then
!      call COMPLEX_1D_DOUBLE_EX1(pulsew,1,nw)
    else
        write(*,*) "Problem here !!! fft can be 1 (local fft) or 2 (mkl fft)"
    end if
    do n=nw/2+1,nw
        j=j+1
        t=j*dt
        w(n)=wmin+(wmax-wmin)*(n-1)/(nw-1)
        !write(5679,*)w(n),dreal(pulsew(n)),dimag(pulsew(n)),cdabs(pulsew(n))**2
        write(5678,'(I4,4E16.8)')j,t,dreal(pulsew(n)),dimag(pulsew(n)),cdabs(pulsew(n))**2
    end do
    do n=1,nw/2
        j=j+1
        t=j*dt
        w(n)=wmin+(wmax-wmin)*(n-1)/(nw-1)
        !write(5679,*)w(n),dreal(pulsew(n)),dimag(pulsew(n)),cdabs(pulsew(n))**2
        write(5678,'(I4,4E16.8)')j,t,dreal(pulsew(n)),dimag(pulsew(n)),cdabs(pulsew(n))**2
    end do
end subroutine pulsefreqgauss


subroutine testgauss(w,nw,namefilebef,namefileaf,fnamel,fnamel2,fft)
    use gen
    use spo
    Use MKL_DFTI
    include 'mkl_dfti_examples.fi'



    integer :: nw,gaussp,wp,j,n,fft,fnamel,fnamel2

    real(8) :: w(nw),wcm1,wop,wg,wmin,wmax,dw,dt
    complex(8) :: pulsew(nw)
    character(len=fnamel) :: namefilebef
    character(len=fnamel2) :: namefileaf
    wmin=1600.d0
    wmax=3000.d0


    wcm1=2200.d0
    wop=wcm1!/219474.63068d0
    wg=35.d0!*2.d0*3.141592654d0/wop
    gaussp=10
    wp=2200.d0

    j=0
    dw=(wmax-wmin)/(nw-1)
    open(unit=5578,name=namefilebef,status='replace')
    do n=1,nw
        w(n)=wmin+(wmax-wmin)*(n-1)/(nw-1)
        pulsew(n)=dexp(-2.d0*dabs((w(n) - wp)/wg)**2)
        write(5578,'(3E16.8)')w(n),dreal(pulsew(n)),dimag(pulsew(n))
    end do
    close(5578)
    if (fft.eq.1)then
       call fourier(pulsew,nw,1)
    elseif (fft.eq.2.)then
!      call COMPLEX_1D_DOUBLE_EX1(pulsew,1,nw)
    else
      write(*,*) "Problem here !!! fft can be 1 (local fft) or 2 (mkl fft)"
      stop 101
    end if

!    call fourier(pulsew,nw,-1)
!    call COMPLEX_1D_DOUBLE_EX1(pulsew,1,nw)
    open(unit=5579,name=namefileaf,status='replace')
    do n=nw/2+1,nw
        j=j+1
        w(n)=wmin+(wmax-wmin)*(n-1)/(nw-1)
        !write(5679,*)w(n),dreal(pulsew(n)),dimag(pulsew(n)),cdabs(pulsew(n))**2
        write(5579,*)j,cdabs(pulsew(n))**2
    end do
    do n=1,nw/2
        j=j+1
        w(n)=wmin+(wmax-wmin)*(n-1)/(nw-1)
        !write(5679,*)w(n),dreal(pulsew(n)),dimag(pulsew(n)),cdabs(pulsew(n))**2
        write(5579,*)j,cdabs(pulsew(n))**2
    end do
    close(5579)
end subroutine testgauss





subroutine champir(champ,w,d,t,nt,typen)
    use gen
    include 'mkl_dfti_examples.fi'

    integer :: nt,n,typen
    real(8) :: champ(nt),t(nt),w,d,c,freq
    write(*,*) "w in champir", w
    freq = 4.3d-3 !fréquence du champ
    w=freq*5.d0

! typen=0 pour frequence (pas utiliser pour le moment
!typen=1 pour longueur d onde
!    c=127.d0
!    w=c/w
    open(unit=5555,name="champ.dat",status='replace')
    do n=1,nt
        champ(n)=dcos((w*t(n))+d)
        write(5555,'(I4,2E16.8)')n,t(n),champ(n)
    end do
    close(5555)
end subroutine champir



! 1 ev equal
! 2.417 989 40(21) × 1014 Hz
! 8 065.544 45(69) cm-1
! 1 239.841 91(11) nm 
! 11 604.505(20) K (kelvin) 
! 1.602 176 53(14) × 10-19 J (joule) 
!
!
!
! 1 Ry ( atomic unit) is 13.605 692 3(12) eV
! http://physics.nist.gov/Pubs/AtSpec/node01.html


! 1 u.a. temps = 2,418 884 326 505(16)×10-17 s

end module pulse


