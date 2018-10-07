program ddq
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calcul de propagations de paquets d'ondes par méthode de Split-Opérateur
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
use spo
use morse1
use gen
use pulse
use asympto
use simps0n

implicit none
integer :: npos,pulsetype,logfile,le0wattcm2,ntps,nt,v,id,nc
character(LEN=50) :: title
real(8) ::  wir,tc,te
!real(8) ::  x(npos),ep(v,npos),xmu12(npos)
real(8),allocatable ::  x(:),ep(:,:),xmu12(:)
!complex(8) :: chi1in(npos), chi2in(npos)
real(8),allocatable :: chi1in(:), chi2in(:)
real(8) :: pbfin
!real(8) :: pot(2,npos)
real(8),allocatable :: pot(:,:)

real(8) :: r0cut, scut
!npos pour la grille de position et d'impulsion, ntps pour la grille de temps
!real(8) :: w1(4*npos), w2(4*npos)
real(8),allocatable :: w1(:), w2(:)
character(LEN=50) :: fichier1,fichier2,fichier3,fichier4,fichier5
!real(8) :: wspos(4*npos+15),wsbig(16*npos+15)
real(8),allocatable :: wspos(:),wsbig(:)
real(8) :: timeper,dtper,pulsetemp,champ1,champ2
!complex(8) :: zwork1(4*npos), zwork2(4*npos), zwork3(4*npos), zwork4(4*npos)
complex(8),allocatable :: zwork1(:), zwork2(:), zwork3(:), zwork4(:)
real(8) :: int1tf, int2tf, int3tf
real(8) ::  int1t0, int2t0, int3t0
integer :: n,i,j,l,m,maxpot1,minpot2,ideb,ninput,npbfin,nchannel
parameter (ideb=85)
!complex(8) :: zcutA(4*npos),zcutI(4*npos)
complex(8),allocatable :: zcutA(:),zcutI(:)
real(8) :: evbyau,spk
!real(8) :: work2(4*npos),work3(4*npos),work4(4*npos), work5(4*npos)
real(8),allocatable :: work2(:),work3(:),work4(:), work5(:)
real(8) :: diss, lieprobv,E0wattcm2,dk,cte,xk
real(8) :: normedeb, rc0
real(8),allocatable :: work1(:),table1(:), champ(:)
real(8) :: projreal, projimag, lieprob
real(8),allocatable :: tablea(:),worka(:),workb(:)
complex(8) :: cun,cim,cnul
complex(8),allocatable :: chi1(:),chi2(:),zetdt(:),ctemp(:),chilie(:),chi1init(:)
complex(8),allocatable :: psik1(:), psik2(:)

real(8) :: alpha,p0,rdeb,xmue
real(8),allocatable :: proj(:) ,proji(:),auto_correl(:)
real(8) :: delt,pi,omega,  norme,norme1,norme2,periode,delta,sigma,tmax,f0
real(8),allocatable :: t(:),vp1(:),vp2(:),kmoyen(:),rmoyen(:),rmoyenlie(:),rclapet1(:),rclapet2(:)
real(8) :: dissprob
real(8),allocatable :: dispers(:)
real(8) :: periodir,dw
real(8),allocatable :: pulset(:),champir1(:)
real(8),allocatable :: vp1reel(:),vp2reel(:)
real(8) :: time1,time3,ftime,beta
real(8) xnorm1, xnorm2, xnormk1, xnormk2
character(LEN=120) :: pbname
character(LEN=50) :: nomfichier
character(LEN=50) :: charnum(10)
character(LEN=5) :: test
character(LEN=2500) :: string
integer :: k
CHARACTER(2) :: FMT

    !### Used for the initial write line ##############################
    character*8 :: DATE
    character*10 :: TIME
    character*32 :: hostname
    integer(4) :: istat,hostnm

    !##################################################################
    !### timing variables #############################################
    real(8) :: btime, time2

    !##################################################################
    !### input variables ##############################################
    real(8) :: t0,E0,w,dt,xmin,xmax,mass
    real(8) :: phase ! will be multiplied by pi
    integer :: pulsetype,nc,npos,nchannel,ncfin,v
    real(8),  parameter :: PI_8  = 4 * atan (1.0_8)

    !##################################################################
    !### derivated value from input variables #########################
    real(8) :: tf
    integer :: nt

    !##################################################################
    !### allocatable vector for the proper states #####################
    real(8),allocatable :: ep(:,:)

    !##################################################################
    !### Used for read format of proper states ########################
    CHARACTER(2) :: FMT

    !##################################################################
    !### Vector for time grid #########################################
    real(8),allocatable :: t(:), champ(:)

    !##################################################################
    !### Vector for wavefunction ######################################
    real(8),allocatable :: chiin(:,:),xmu12(:),pot(:,:),work(:)
    complex(16),allocatable :: chi(:,:),zetdt(:)


    !##################################################################
    !### Vector for internuclear distance #############################
    real(8),allocatable :: x(:)

    !##################################################################
    !### Variable for pbound calculation ##############################
    real(8) :: lieprob,lieprobv,projreal,projimag
    real(8), allocatable :: proj(:), proji(:)

    !##################################################################
    !### Integer variable used for loop ###############################
    integer :: n,i,j,l

    !##################################################################
    !### logical variable for file ####################################
    logical :: file_exists

    !real(8) :: CDABS,normedeb
    real(8) :: normedeb

    !##################################################################
    !### Time at the start ############################################
    call cpu_time ( btime )


    ISTAT = HOSTNM(hostname)
    CALL DATE_AND_TIME ( DATE , TIME )

    write(*,*) "DDQ Program start at ",DATE," ",TIME," on ",hostname
    write(*,*) "Reading the input"


    INQUIRE(FILE="input", EXIST=file_exists)
    if (file_exists) then
        open(5,file="input",status='old')
    end if
    namelist /iofile/ t0, pulsetype,E0,phase,w,dt,nc,ncfin,npos,xmax,nchannel,v,w,xmin,mass
    read(5,iofile)
    if (file_exists) then
        close(5)
    end if





    write(*,*) "Initial time (t0) = ",t0
    write(*,*) "Phase = ",phase,"*Pi"
    phase=phase*PI_8
    write(*,*) "E0 = ",E0
    write(*,*) "Frequency = ",w
    write(*,*) ncfin," optical cycles after t=0"
    tf=ncfin*(2.d0*PI_8/w)
    write(*,*) "tf = ",tf
    nt=int((tf-t0)/dt)

    !##################################################################
    !### Reading the v proper states ##################################
    open(10,file='properstate.dat')
    open(11,file='pot.dat')
    open(12,file='init.dat')
    allocate(ep(v,npos),chiin(nchannel,npos),chi(nchannel,npos),xmu12(npos))
    allocate(x(npos),pot(nchannel,npos),work(npos),zetdt(npos))
    allocate(proj(npos), proji(npos))
    WRITE(FMT,'(I2)') v+1
    do i=1,npos
        READ(10,"(" // ADJUSTL(FMT) // "(E28.20E3,4x))") x(i),(ep(n,i),n=1,v)
        READ(12,"(3(E28.20E3),4x)") x(i),chiin(1,i),chiin(2,i)
        READ(11,"(4(E28.20E3),4x)") x(i),pot(1,i),pot(2,i),xmu12(i)
    enddo
    close(10)
    close(11)
    close(12)


    ! *******************************************************************
    !           Construction de la grille temporelle
    ! *******************************************************************

    allocate(t(nt),champ(nt))
    do i=1,nt
        t(i)=t0+(i-1)*dt
    end do


    !***********************************************************************
    !            Calcul de la fonction d'onde initiale
    !***********************************************************************

    do j=1,npos
        chi(1,j)=cmplx(chiin(1,j),0.d0)
    enddo

    !***********************************************************************
    !           Normalisation de la fonction d'onde initiale
    !***********************************************************************
    do l=1,npos
        work(l)=(abs(chi(1,l)))**2
    enddo
    call simpson( npos,x(2)-x(1),work, normedeb)
    write(*,*) "NORME De Depart : ", normedeb
    do l = 1, npos
        chi(1,l) = chi(1,l)/sqrt(normedeb)
        chi(2,l) = cmplx(chiin(2,l),0.d0)
        !write(*,*) qchi(1,l)
    enddo
    !***********************************************************************

    !***********************************************************************
    !                           Main loop on the time
    !***********************************************************************



iE0=0
!WRITE(test,'(I5.5)') iE0
!write(*,*) "case_"//ADJUSTL(test)//"/input"
!open(5,name="case_"//ADJUSTL(test)//"/input",status="old")
namelist /iofile/ t0, pulsetype,E0,phase,w,dt,nc,npos,xmax,nchannel
read(5,iofile)
phase=phase*MATH_PI

!open(6,name="input",status="old")
!namelist /iofile/ npos,xmax,nE0
!read(6,iofile)

xmin=2.d-3

!v=19
!TODO : change number of optical cycle (nc) in the input
delt=dt
wir=w
write(*,*) "MATH_PI = ",MATH_PI
tf=6.d0*(2.d0*MATH_PI/w)
nt=int((tf-t0)/dt)
allocate(t(nt))
write(*,*) 'THIS IS A TEST 2! ', iE0
do i=1,nt
  t(i)=t0+(i-1)*dt
end do
!,t2,t3,clock_rate, clock_max
!***********************************************************************
!         Valeurs des paramètres, allocation des variables
!***********************************************************************
! call system_clock ( t1, clock_rate, clock_max )
call cpu_time ( btime )
!write(*,*) "* Begining of the program ", btime
logfile=876325
write(logfile,*) "Begining of the program ", btime
!call readinputsub(tc,dw,tf)

 cun = dcmplx(1.0d0,0.d0)
 cim = dcmplx(0.d0,1.0d0)
 cnul = dcmplx(0.d0,0.d0)
 pi = MATH_PI
!t0=0d0

 dissprob=0.d0
  r0cut = 34.026d0
  scut = 4.72d-1
    evbyau = 27.212d0

! Output File list
! 101 : pbound and field

open(101,file='pbound.dat')


!open(10,file='init.dat')
!read(10,*)npos
!real(8) ::  x(npos),ep(v,npos),xmu12(npos)
allocate(x(npos),xmu12(npos))
!complex(8) :: chi1in(npos), chi2in(npos)
allocate(chi1in(npos), chi2in(npos))
!real(8) :: pot(2,npos))
allocate(pot(nchannel,npos))
!real(8) :: w1(4*npos), w2(4*npos)
allocate(w1(4*npos), w2(4*npos))
!real(8) :: wspos(4*npos+15),wsbig(16*npos+15)
allocate(wspos(4*npos+15),wsbig(16*npos+15))
!complex(8) :: zwork1(4*npos), zwork2(4*npos), zwork3(4*npos), zwork4(4*npos)
allocate(zwork1(4*npos), zwork2(4*npos), zwork3(4*npos), zwork4(4*npos))
!complex(8) :: zcutA(4*npos),zcutI(4*npos)
allocate(zcutA(4*npos),zcutI(4*npos))
!real(8) :: work2(4*npos),work3(4*npos),work4(4*npos), work5(4*npos)
allocate(work2(4*npos),work3(4*npos),work4(4*npos), work5(4*npos))
!real(8) :: work1(npos),table1(npos), champ(nt)
allocate(work1(npos),table1(npos), champ(nt))
!real(8),allocatable :: tablea(npos),worka(npos),workb(npos)
allocate(tablea(npos),worka(npos),workb(npos))
!complex(8) :: chi1(npos),chi2(npos),zetdt(npos),ctemp(npos),chilie(npos),chi1init(npos)
allocate(chi1(npos),chi2(npos),zetdt(npos),ctemp(npos),chilie(npos),chi1init(npos))
!complex(8) :: psik1(npos*4), psik2(npos*4)
allocate(psik1(npos*4), psik2(npos*4))
!real(8) :: rdeb,proj(npos) ,proji(npos),auto_correl(nt)
allocate(proj(npos) ,proji(npos),auto_correl(nt))
!real(8) :: t(nt),vp1(npos),vp2(npos),kmoyen(nt),rmoyen(nt),rmoyenlie(nt),rclapet1(nt),rclapet2(nt)
allocate(vp1(npos),vp2(npos),kmoyen(nt),rmoyen(nt),rmoyenlie(nt),rclapet1(nt),rclapet2(nt))
!real(8) :: dispers(nt)
allocate(dispers(nt))
!real(8) :: pulset(nt),champir1(nt)
allocate(pulset(nt),champir1(nt))
!real(8),dimension(npos-ideb) :: vp1reel,vp2reel
allocate(vp1reel(npos-ideb),vp2reel(npos-ideb))
    !allocate(work1(npos))
    !allocate(chi1(npos),tmprchi1(npos),tmpichi1(npos))
    !allocate(chi2(npos),tmprchi2(npos),tmpichi2(npos))
    !allocate(pot(2,npos))
    !allocate(xmu12(npos))
    !allocate(x(npos))
!read(10,*)v

!WRITE(*,*) v+6
!WRITE(FMT,'(I2)') v+6
!do i=1,npos
  !read(10,"(" // ADJUSTL(FMT) // "(E28.20E3,4x))") x(i),(ep(n,i),n=1,v),pot(1,i),pot(2,i),xmu12(i),chi1in(i),chi2in(i)
  !read(10) x(i),(ep(n,i),n=1,v),pot(1,i),pot(2,i),xmu12(i),chi1in(i),chi2in(i)
!write(10,101)x(i),(ep(n,i),n=1,v),pot(1,i),pot(2,i),xmu12(i),real(chi1(i)),aimag(chi1(i)),real(chi2(i)),aimag(chi2(i))
!enddo
delr=x(2)-x(1)

rc0 = 1.3989d0 !position du paquet d'onde à t0
p0 = 0.0d0 !impulsion du paquet d'onde à t0
alpha = 13.019d0 !paramètre pour chi1 et chi2

    requ=2.d0 !valeur de r à l'equilibre = 2*a0 (a0=1 en u.a)
    diss=2.7925d0/27.2d0 !potentiel de dissociation de H2+
    massreduite=918.0762887608628d0 !=masse proton/2 en u.a
    write(*,*)"This is a test 2.1"
    write(*,*) xmax,xmin,npos

delr=(xmax-xmin)/(npos-1)
!read(*,*) test
    !write(*,*)"This is a test 2.53"
        do i=1,npos
    !write(*,*)"This is a test 2.53"
         x(i)=xmin+(i-1)*delr !création de l'axe des positions
        enddo

    !write(*,*)"This is a test 2.53"
 !do n=1,v
    !write(*,*)"This is a test 2.53"
  !      do i=1,npos
    !write(*,*)"This is a test 2.53"
   !       ep(n,i)=morse(diss,0.72d0,massreduite,requ,x(i),n-1) !construction des n etats vibrationnels sur la grille
   !       work1(i)=(dabs(ep(n,i)))**2
   ! enddo
    !write(*,*)"This is a test 2.53"

   !       call simpson(npos,delr,work1,norme)
   !       ep(n,:)=ep(n,:)/sqrt(norme) !normalisation des etats vibrationnels

 !enddo
    !write(*,*)"This is a test 3"

open(10,file='properstate.dat')
!write(10)npos
!write(10)v
READ(10,*) v
allocate(ep(v,npos))
write(*,*) v+1
write(*,'(I2)') 19
WRITE(FMT,'(I2)') v+1
do i=1,npos
    READ(10,"(" // ADJUSTL(FMT) // "(E28.20E3),4x)") x(i),(ep(n,i),n=1,v)
enddo
close(10)
open(10,file='pot.dat')
WRITE(FMT,'(I2)') 4
do i=1,npos
    READ(10,"(" // ADJUSTL(FMT) // "(E28.20E3),4x)") x(i),pot(1,i),pot(2,i),xmu12(i)
enddo
close(10)
open(10,file='init.dat')
WRITE(FMT,'(I2)') 3
do i=1,npos
    READ(10,"(" // ADJUSTL(FMT) // "(E28.20E3),4x)") x(i),chi1in(i),chi2in(i)
enddo
close(10)

!***********************************************************************
! Mise en place de la grille des positions, potentiels de Morse et
! etats propres associés
!***********************************************************************

! call pot_spec(pot(1,:),pot(2,:),xmu12, npos,delr,xmin) !construction des 2 potentiels de H2+ et du moment dipolaire
 ! AJOUTER DANS LE SCAN_E.f90
 ! pour ne pas ouvrir plusieur potentiel.dat
 !open(unit=1,name="potentiel.dat",status='replace')
!   do i=1,npos
!     write(1,*)x(i),pot(1,i),pot(2,i)
!   enddo
! close(1)



! *******************************************************************
!           Construction de la grille temporelle
! *******************************************************************


!***********************************************************************
!            Calcul de la fonction d'onde initiale
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!$OMP PARALLEL
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!$OMP DO
!call cpu_time ( btime )
!write(*,*) "Begining of time loop : ", time1
!do ph=0,1
! call eval(chi1, chi2, delr, rdeb, p0, rc0, alpha, npos)
 !do l=1,npos
!   chi1(l)=dcmplx(ep(0,l),0d0)
!   chi2(l)=dcmplx(0d0,0d0)
! enddo
 do j=1,npos
    chi1init(j)=cmplx(chi1in(j),0.d0)
   write(123556,*) x(j), chi1init(j)
 enddo



!***********************************************************************
!           Normalisation de la fonction d'onde initiale
!***********************************************************************
    do l=1,npos
    worka(l)=(cdabs(chi1init(l)))**2
    enddo
   call simpson( npos,delr,worka, normedeb)
   write(*,*) "NORME De Depart : ", normedeb
 !open(unit=1234567+iE0,name='chi1init.dat',status='replace')
 !open(unit=234579+iE0,name='chi2init.dat',status='replace')
   do l = 1, npos
         chi1(l) = chi1init(l)/dsqrt(normedeb)
         chi2(l) = cmplx(chi2in(l),0.d0)
!********************************************************************
    !write(1234567+iE0,*)x(l),dreal(chi1(l)),dimag(chi1(l))
    !write(234579+iE0,*)x(l),dreal(chi2(l)),dimag(chi2(l))
   enddo
 !close(1234567+iE0)
 !close(234579+iE0)




!********************************************************************
!           Construction du champ
!********************************************************************
! sigma=periode/(2d0*2.3548d0)
! tmax=2d0*periode
!tc=200.d0*10.d0
!open(unit=2000+iE0,status='replace')
!call champir(champir1,wir,phase,t,ntps,1)
!call pulsechoice(pulsetype,champ,t,ntps,delt,logfile,E0,wir,phase,le0wattcm2,tc,te,tf)
!pseudocarre(champ,t,ntps,delt,ph)
write(*,*) "Temps final : ", tf
    E0=wattcm22au(E0)
    E0wattcm2=au2wattcm2(E0)
    !write(logfile, *) " iE0 = ",  iE0
    write(*,'(" E0 = ", E16.8 , " u.a. , ",  E16.8 , "  W/cm2" )') E0 , E0wattcm2
    !write(logfile,*) "phase = ", phase/MATH_PI , " Pi"
    !write(logfile,'(" tc = ", E16.8 , " a.u. ," , E16.8, " fs" )') tc , tc*tau2fs
    !write(logfile,'(" te = " , E16.8 , " a.u. ," , E16.8, " fs" )') te , te*tau2fs
    !write(logfile,*) "tf = ", tf, "a.u." , tf*tau2fs ," fs"
    !write(logfile,*) "nt = ", nt
    !write(logfile,*) "dt = ", dt, "a.u." , dt*tau2fs ," fs"
    !write(logfile,*) "wir in travail = " , wir , " TL = " ,  2.d0*MATH_PI/(wir*TVIB) ," TVIB"

      pbfin=0.d0
      npbfin=0

      call calczcut(zcutA, zcutI, x(1), delr, 4*npos, r0cut, scut)

dtper=dt/1024.d0
      int1tf=0.d0
      int2tf=0.d0
      int3tf=0.d0
!TODO : calculate tf
      timeper = t0
      do while(timeper.lt.tf)
         timeper = timeper + dtper
         call calc_champ(champ2,wir,timeper,phase,nc,MATH_PI,E0)
         call calc_champ(champ1,wir,timeper-dtper,phase,nc,MATH_PI,E0)
         call airesint (int1tf, int2tf, int3tf, dtper, champ1, champ2)
      end do
      call calc_champ(champ1,wir,timeper,phase,nc,MATH_PI,E0)
      call calc_champ(champ2,wir,tf,phase,nc,MATH_PI,E0)
      call airesint(int1tf, int2tf, int3tf, tf-timeper, champ1, champ2)
!********************************************************************
!   Application du split operator-Ouverture de la boucle temps
!********************************************************************

write(*,*) "int1tf", int1tf
      int1t0=0.d0
      int2t0=0.d0
      int3t0=0.d0
      timeper = t0
 do i=1,nt
!********************************************************************
    if (mod(i,100).eq.0) then
      write(*,*) i, "/" , nt , "t = ",t(i)
    end if
      do while(timeper.lt.t(i))
         timeper = timeper + dtper
         call calc_champ(champ2,wir,timeper,phase,nc,MATH_PI,E0)
         call calc_champ(champ1,wir,timeper-dtper,phase,nc,MATH_PI,E0)
         call airesint (int1t0, int2t0, int3t0, dtper, champ1, champ2)
      end do
      call calc_champ(champ1,wir,timeper,phase,nc,MATH_PI,E0)
      call calc_champ(champ2,wir,t(i),phase,nc,MATH_PI,E0)
      call airesint(int1tf, int2tf, int3tf, t(i)-timeper, champ1, champ2)
!********************************************************************
!   Calcul des aires temporelles du champ
!********************************************************************



!call cpu_time ( time1 )
!write(*,*) "Time 1 (beg of time loop) : ", time1

!    if (mod(i,20).eq.0) then
!        write(*,'(I5," , t = ",F16.8," u.a, PB : ",F6.5,I5,F8.4)')        i,t(i),lieprob,iE0,t(i)-t(i-1)
!        do j=1,npos
            !write((10000*+i,'(I4,7E16.8)')i,t(i),t(i)*2.418884326505E-2,x(j),dabs(chi1(j))**2,dabs(chi2(j))**2,vp1(j),vp2(j)
!            write(90000+i,'(I4,7E16.8)')i,t(i),t(i)*2.418884326505E-2,x(j),abs(chi1(j))**2,abs(chi2(j))**2,vp1(j),vp2(j)

!       end do
!    end if
    call calc_champ(champ(i),wir,t(i),phase,nc,MATH_PI,E0)


    do j=1,npos
        worka(j)=x(j)*(cdabs(chi1(j)))**2 !position moyenne du paquet d'onde de l'état fondamental
        workb(j)=(x(j)**2*(cdabs(chi1(j)))**2) !position moyenne du paquet d'onde de l'état fondamental
        tablea(j)=(cdabs(chi1(j)))**2 !densité de probabilité du paquet d'onde de l'état fondamental
    enddo
    call simpson(npos,delr,worka,norme1)
    call simpson(npos,delr,workb,norme2)
    call simpson(npos,delr,tablea,norme)
    rmoyen(i)=norme1/norme
    dispers(i)=norme2/norme
    dispers(i)=dispers(i)-rmoyen(i)**2
    !kmoyen(i)=normedeb/norme
    write(70000+iE0,*)t(i),rmoyen(i)!r moyen du paquet d'onde total
    write(30000+iE0,*)t(i),dispers(i)!r moyen du paquet d'onde total


!write(logfile,*) "Rmoyen du iE0 : ", iE0 , " : fort." , 70000+iE0

call cpu_time ( time2 )
!write(*,*) "Time2 : ", time2
! PARTIE TRES LENTE DU PROGRAMME : CE CALCUL PREND PRESQUE 1 SEC PAR APPLICATION
!   do j=1,npos
!       chilie(j)=dcmplx(0d0,0d0)
!       do n=0,v
!           ctemp(j)=ep(n,j)*chi1(j)
!           call simpson(npos,delr,dreal(ctemp),normedeb)
!           call simpson(npos,delr,dimag(ctemp),norme)
!           chilie(j)=chilie(j)+ep(n,j)*(normedeb+cim*norme)
!       enddo
!       worka(j)=x(j)*(cdabs(chilie(j)))**2
!       tablea(j)=(cdabs(chilie(j)))**2
!   enddo
!   call simpson(npos,delr,worka,normedeb)
!   call simpson(npos,delr,tablea,norme)
!   rmoyenlie(i)=normedeb/norme
!   write(30+ph,*)t(i),rmoyenlie(i)!rmoyen de la partie liee du paquet d'onde

             !if (((t(i).gt.(0.d0)).and.(t(i).le.(0.d0+delt))).or.((t(i).gt.(1.d0*periode/4.d0)).and.(t(i).le.(1.d0*periode/4.d0+delt))).or.((t(i).gt.(2.d0*periode/4.d0)).and.(t(i).le.(2.d0*periode/4.d0+delt))).or.((t(i).gt.(3.d0*periode/4.d0)).and.(t(i).le.(3.d0*periode/4.d0+delt))).or.((t(i).gt.(4.d0*periode/4.d0)).and.(t(i).le.(4.d0*periode/4.d0+delt))).or.((t(i).gt.(12.d0*periode/4.d0)).and.(t(i).le.(12.d0*periode/4.d0+delt))).or.((t(i).gt.(13.d0*periode/4.d0)).and.(t(i).le.(13.d0*periode/4.d0+delt))).or.((t(i).gt.(14.d0*periode/4.d0)).and.(t(i).le.(14.d0*periode/4.d0+delt))).or.((t(i).gt.(15.d0*periode/4.d0)).and.(t(i).le.(15.d0*periode/4.d0+delt))).or.((t(i).gt.(16.d0*periode/4.d0)).and.(t(i).le.(16.d0*periode/4.d0+delt)))) then

!   open(unit=2000+i+(ntps+2000)*ph)
!   open(unit=2*ntps+5000+i+(ntps+2000)*ph)
    do j=1,npos
        xmue=xmu12(j)*champ(i)
        delta=(pot(2,j)-pot(1,j))**2+(2d0*xmue)**2
        delta=dsqrt(delta)
        vp1(j)=(pot(2,j)+pot(1,j)-delta)*0.5d0
        vp2(j)=(pot(2,j)+pot(1,j)+delta)*0.5d0
      !write(5433634,*) x(j), pot(1,j), pot(2,j), vp1(j), vp2(j)
        if (j.gt.ideb) then
            vp1reel(j-ideb)=vp1(j)
            vp2reel(j-ideb)=vp2(j)
        endif
!       write(2000+i+(ntps+2000)*ph,*) x(j), dsqrt((dreal(chi1(j))**2+dimag(chi1(j))**2)),dsqrt((dreal(chi2(j))**2+ dimag(chi2(j))**2))
!construction des potentiels E- et E+
!       write(2*ntps+5000+i+(ntps+2000)*ph,*)x(j), vp1(j), vp2(j)
    enddo
!   close(2000+i+(ntps+2000)*ph)
!   close(2*ntps+5000+i+(ntps+2000)*ph)
            !endif

! if ((t(i).gt.(11071.75d0)).and.(t(i).le.(11072.5d0)))then
!   do j=1,npos
!       write(51,*)x(j),dreal(chi1(j)),dimag(chi1(j))
!   enddo

! endif
! if ((t(i).gt.(22143.75d0)).and.(t(i).le.(22144.5d0))) then
!   do j=1,npos
!       write(50,*)x(j),dreal(chi1(j)),dimag(chi1(j))
!   enddo
! endif

! do j=1,npos
!   ctemp=chi1init(j)*chi1(j)
!   worka(j)=(cdabs(ctemp(j)))**2 !a revoir (calcul d'autocorrelation)
! enddo
! call simpson (npos,delr,worka,auto_correl(i))
! write(52,*)t(i),auto_correl(i)

! maxpot1=maxloc(vp1reel,1)
 minpot2=minloc(vp2,1)
! rclapet1(i)=x(ideb+maxpot1)
 rclapet2(i)=x(minpot2)
 write(80000+iE0,*) t(i),rclapet2(i)
 !write(200000+ph,*)t(i),rclapet1(i),rclapet2(i)

!********************************************************************
    call splitop(chi1, chi2, zetdt,pot,xmu12, npos, champ(i), delr, massreduite, delt)
! ajouter le calcul de int[1-2-3]t[0-f] ! FAIT
!
    call asympt(t(i), tf, psik1, psik2, chi1, chi2, &
                       zcutA, 4*npos, npos, massreduite,&
                       int1tf, int2tf, int3tf,&
                       int1t0, int2t0, int3t0, delr,&
                       zwork1, zwork2, zwork3,&
                       zwork4, wsbig,dissprob)
!!!!!!!!!!!!!!
!!!!!!!! Verifier le calcul du cut
!!!!!!!!!!!
!********************************************************************
            call ZVEM(npos,chi1(1),1,zcutI(1),1,chi1(1),1)
            call ZVEM(npos,chi2(1),1,zcutI(1),1,chi2(1),1)
!********************************************************************
!       Calcul de probabilité de dissociation
!********************************************************************
     lieprob = 0.d0
 !   pause 10
     !write(123453245,*) "t = ",t(i)
     do n = 1, v
           do j = 1, npos
                  proj(j) = ep(n,j)*dreal(chi1(j))
                  proji(j) = ep(n,j)*dimag(chi1(j))


           end do
       call simpson(npos,delr,proj,projreal)
       call simpson(npos,delr,proji,projimag)
       lieprobv=(projreal**2 + projimag**2)
            lieprob = lieprob + lieprobv
!       if (i.eq.1) then
!           write(49,*)n,lieprobv
!       endif
            end do
            write(101,'( 3F18.10,4X )') t(i), lieprob,champ(i)

  !open((iE0+1)*1000000+i)
  !do j=1,npos
  !  write((iE0+1)*1000000+i,*)x(j),cdabs(chi1(j))**2,cdabs(chi2(j))**2
  !enddo
  !close((iE0+1)*1000000+i)




  ! end of the loop on time
 enddo
 pbfin=pbfin/100.d0



      call asympt(tf, tf, psik1, psik2, chi1, chi2,&
                 zcutA, 4*npos, npos, massreduite,&
                 int1tf, int2tf, int3tf,&
                 int1t0, int2t0, int3t0, delr,&
                 zwork1, zwork2, zwork3,&
                 zwork4, wsbig,dissprob)
      call ZVEM(npos,chi1,1,zcutI,1,chi1,1)
      call ZVEM(npos,chi2,1,zcutI,1,chi2,1)
      write(fichier1,10000)10000+iE0
      write(fichier2,10000)20000+iE0
10000 FORMAT ("fichier",I5,".dat")
      !fichier1="fichier1.dat"
      !fichier2="fichier2.dat"
      write(*,*) "fichier1 = ", fichier1
      write(*,*) "fichier1 = ", fichier2
      open(1,file=fichier1,status='unknown')
      open(2,file=fichier2,status='unknown')
      do l = 1, npos
     w1(l) = cdabs(chi1(l))**2
     w2(l) = cdabs(chi2(l))**2
     write(1,1004) dreal(chi1(l)),dimag(chi1(l)),  dreal(chi2(l)),dimag(chi2(l))
     write(2,1005) x(l), chi1(l), chi2(l)
      end do
      close(1,status='keep')
      close(2,status='keep')
      write(fichier3,10000)30000+iE0
      write(fichier4,10000)40000+iE0
      write(fichier5,10000)50000+iE0
      open(1,file=fichier3,status='unknown')
      open(2,file=fichier4,status='unknown')
      open(3,file=fichier5,status='unknown')
      dk = 2.d0*pi/(4*npos*delr)
      cte = 0.5d0/massreduite
      do i = 4*npos/2 +2, 4*npos
         xk = dk*(i-1 - (4*npos))
         work5(i) = cdabs(psik1(i))**2
         work2(i) = cdabs(psik2(i))**2
         work3(i) = work5(i)
         work4(i) = work2(i)
         work5(i) = work5(i)*(-massreduite/(xk*evbyau))
         work2(i) = work2(i)*(-massreduite/(xk*evbyau))
         spk = work5(i) + work2(i)
         xk = -xk*xk*0.5/massreduite
!         write(*,*) "test :", massreduite, xk, cdabs(psik1(i))
         write(1,1002) xk*evbyau, work5(i)
         write(2,1002) xk*evbyau, work2(i)
         write(3,1002) xk*evbyau, spk
      end do
      do i = 1,4*npos/2 +1
         xk = dk*(i-1)
         work5(i) = cdabs(psik1(i))**2
         work2(i) = cdabs(psik2(i))**2
         work3(i) = work5(i)
         work4(i) = work2(i)
         if (xk.eq.0.d0) then
            work5(i) = 0.d0
            work2(i) = 0.d0
         else
            work5(i) = work5(i)*(massreduite/(xk*evbyau))
            work2(i) = work2(i)*(massreduite/(xk*evbyau))
         end if
         spk = work5(i) + work2(i)
         xk = xk*xk*0.5/massreduite
         write(1,1002) xk*evbyau, work5(i)
         write(2,1002) xk*evbyau, work2(i)
         write(3,1002) xk*evbyau, spk
      end do
      close(1)
      close(2)
      close(3)
   write(*,*) 'test1'
      call simpson(npos, delr, w1, xnorm1)
      call simpson(npos, delr, w2, xnorm2)
   write(*,*) 'test2'
      write(*,*)
     ! write(*,*) 'Niveau vibrationnel de depart'
     ! write(*,*) nu
      write(*,*)
      write(*,*) 'NORME de depart'
      write(*,*) normedeb
      write(*,*)
      write(*,*) 'NORMES population restante'
      write(*,*) '    SIGMA g           SIGMA u           TOTAL'
      write(*,1003) xnorm1, xnorm2, xnorm1 + xnorm2
      call simpson(4*npos, dk, work3, xnormk1)
      call simpson(4*npos, dk, work4, xnormk2)
      write(*,*)
      write(*,*) 'NORMES population dissociee'
      write(*,*) '    SIGMA g           SIGMA u           TOTAL'
      write(*,1003) xnormk1, xnormk2, xnormk1 + xnormk2
      write(*,*)
      write(*,*)
      write(*,*) '1 - NORMES population restante:', 1 - xnorm1 - xnorm2
      write(*,*)
      write(*,*) 'Erreur sur normes:',           1 - xnormk1 - xnormk2 - xnorm1 - xnorm2



!enddo
!call fourier(kmoyen,ntps,1,delt,delt)
    !write(*,*) iE0
   !write(*,*) "E0 : ", E0, "id : ", id




  close(10000+iE0)
   close(30000+iE0)
    close(70000+iE0)
  close((iE0+1)*10000000)

call cpu_time ( ftime )
write(logfile,*) "End of the program ", ftime
if ( ftime-btime > 60*60 )  then
    write(string,*) (ftime-btime)/60*60 , "heures," ,  ((ftime-btime)-(ftime-btime)/60*60)/60 , " min et ",    ftime-btime-(((ftime-btime)-(ftime-btime)/60*60)/60) , " sec "
else if ( ftime-btime > 60)  then
    write(string,*)  (ftime-btime)/60 , " min et ",    ftime-btime-((ftime-btime)/60) , " sec "
else
     write(string,*) ftime-btime , " sec "
end if
write(logfile,'(A120)') "Time of calculation  ", string

1002  format(2(e16.8e3,2x))
1003  format(3(e16.8e3,2x))
1004  format(4(e16.8e3,2x))
1005  format(5(e16.8e3,2x))
1006  format(6(e16.8e3,2x))
1007  format(7(e16.8e3,2x))
end program ddq

