program ddq
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calcul de propagations de paquets d'ondes par méthode de Split-Opérateur
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
use spo
!use morse1
!use gen
use asympto ! TODO: Make it work
!Use MKL_DFTI
use simps0n !Trapezoidal int
!include 'mkl_dfti_examples.fi'
use basics
use math
use morse1
  implicit none

!implicit none
integer :: iE0,nE0
integer :: npos,pulsetype,logfile,le0wattcm2,ntps,nt,v,id,nc
character(LEN=50) :: title
real(8) ::  wir,phase,tc,te,tf,delr,massreduite,dt,t0
!real(8) ::  x(npos),ep(v,npos),xmu12(npos)
real(8),allocatable ::  x(:),ep(:,:),xmu12(:)
real(8) :: E0,TVIB,w
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
integer :: n,i,j,l,m,maxpot1,minpot2,ideb,ninput,npbfin
parameter (ideb=85)
!complex(8) :: zcutA(4*npos),zcutI(4*npos)
complex(8),allocatable :: zcutA(:),zcutI(:)
real(8) :: evbyau,spk
!real(8) :: work2(4*npos),work3(4*npos),work4(4*npos), work5(4*npos)
real(8),allocatable :: work2(:),work3(:),work4(:), work5(:)
real(8) :: xmin, xmax, requ, diss,E0wattcm2,dk,cte,xk
real(8),dimension(:),allocatable :: lieprobv
real(8) :: normedeb, rc0
!real(8) :: work1(npos),table1(npos), champ(nt)
real(8),allocatable :: work1(:),table1(:), champ(:)
real(8) :: projreal, projimag, lieprob 
!real(8),allocatable :: tablea(npos),worka(npos),workb(npos)
real(8),allocatable :: tablea(:),worka(:),workb(:)
complex(8) :: cun,cim,cnul
!complex(8) :: chi1(npos),chi2(npos),zetdt(npos),ctemp(npos),chilie(npos),chi1init(npos)
complex(8),allocatable :: chi1(:),chi2(:),zetdt(:),ctemp(:),chilie(:),chi1init(:)
!complex(8) :: psik1(npos*4), psik2(npos*4)
complex(8),allocatable :: psik1(:), psik2(:)

real(8) :: alpha,p0,rdeb,xmue
!real(8) :: proj(npos) ,proji(npos),auto_correl(nt) 
real(8),allocatable :: proj(:) ,proji(:),auto_correl(:) 
real(8) :: delt,pi,omega,  norme,norme1,norme2,periode,delta,sigma,tmax,f0
!real(8) :: t(nt),vp1(npos),vp2(npos),kmoyen(nt),rmoyen(nt),rmoyenlie(nt),rclapet1(nt),rclapet2(nt)
real(8),allocatable :: t(:),vp1(:),vp2(:),kmoyen(:),rmoyen(:),rmoyenlie(:),rclapet1(:),rclapet2(:)
real(8) :: dissprob
!real(8) :: dispers(nt)
real(8),allocatable :: dispers(:)
real(8) :: periodir,dw
!real(8) :: pulset(nt),champir1(nt)
real(8),allocatable :: pulset(:),champir1(:)
!real(8),dimension(npos-ideb) :: vp1reel,vp2reel
real(8),allocatable :: vp1reel(:),vp2reel(:)
real(8) :: time1,time2,time3,btime,ftime,beta
real(8) xnorm1, xnorm2, xnormk1, xnormk2
character(LEN=120) :: pbname
character(LEN=50) :: nomfichier
character(LEN=50) :: charnum(10)
character(LEN=5) :: test
!character(LEN=2500) :: string(n
integer :: k
CHARACTER(2) FMT
character*18 :: lieprobnam
character*17 :: chinam
namelist /iofile/ t0, pulsetype,E0,phase,w,dt,tf,npos,v

iE0=0
!WRITE(test,'(I5.5)') iE0
!write(*,*) "case_"//ADJUSTL(test)//"/input"
!open(5,name="case_"//ADJUSTL(test)//"/input",status="old")
open(5,file='input',status='old')
read(5,nml=iofile)

 close(5)
 pi = 3.14159265358979323846264338327950288419716939937510d0!MATH_PI
phase=phase*pi

!open(6,name="input",status="old")(n
!namelist /iofile/ npos,xmax,nE0
!read(6,iofile)

!xmin=2.d-3

!v=19
!TODO : change number of optical cycle (nc) in the input
delt=dt
wir=w
!write(*,*) "MATH_PI = ",MATH_PI
!tf=6.d0*(2.d0*MATH_PI/w)
nt=int((tf-t0)/dt)
allocate(t(nt))
!write(*,*) 'THIS IS A TEST 2! ', iE0
do i=1,nt
  t(i)=t0+(i)*dt
end do 
!,t2,t3,clock_rate, clock_max
!***********************************************************************
!         Valeurs des paramètres, allocation des variables
!***********************************************************************
! call system_clock ( t1, clock_rate, clock_max )
!call cpu_time ( btime )
!write(*,*) "* Begining of the program ", btime
!logfile=876325
!write(logfile,*) "Begining of the program ", btime
!call readinputsub(tc,dw,tf)

 cun = dcmplx(1.0d0,0.d0)
 cim = dcmplx(0.d0,1.0d0)
 cnul = dcmplx(0.d0,0.d0)
 pi = 3.14159265358979323846264338327950288419716939937510d0!MATH_PI
!t0=0d0

 dissprob=0.d0
  r0cut = 34.026d0
  scut = 4.72d-1
    evbyau = 27.212d0

! Output File list
! 101 : pbound and field
 
open(101,file='out/pbound.dat')
write(lieprobnam,"('(',i3,'(es30.15e3))')") v+3

!write(charnum(1),'(I5)')10000+iE0
!write(*,*)"Charnum(1) = ", charnum(1)
!write((iE0+1)*10000000,"(A70)") pbname
 ! 70000+iE0 rmoyen
    ! 30000+iE0 dispersion
!write(charnum(1),'(I5)')10000+iE0
!write(*,*)"Charnum(1) = ", charnum(1)
!open(10000+iE0,name='pbound.'//charnum(1))

!write(charnum(1),'(I5)')10000+iE0
!write(*,*)"Charnum(1) = ", charnum(1)
!call readinputsub(tc,dw,tf)

! cun = dcmplx(1.0d0,0.d0)
! cim = dcmplx(0.d0,1.0d0)
! cnul = dcmplx(0.d0,0.d0)
! pi = MATH_PI
!t0=0d0

! dissprob=0.d0
!  r0cut = 34.026d0
!  scut = 4.72d-1
!    evbyau = 27.212d0


!open((iE0+1)*10000000)
!pbname = "set terminal png"
!write((iE0+1)*10000000,"(A70)") pbname
 ! 70000+iE0 rmoyen
    ! 30000+iE0 dispersion

!write(charnum(1),'(I5)')10000+iE0
!open(30000+iE0,name='dispers.'//charnum(1))

!write(charnum(1),'(I5)')10000+iE0
!open(70000+iE0,name='rmoyen.'//charnum(1))






allocate(lieprobv(v))
!open(10,file='init.dat')
!read(10,*)npos
!real(8) ::  x(npos),ep(v,npos),xmu12(npos)
allocate(x(npos),xmu12(npos))
!complex(8) :: chi1in(npos), chi2in(npos)
allocate(chi1in(npos), chi2in(npos))
!real(8) :: pot(2,npos))
allocate(pot(2,npos))
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
allocate(ep(v,npos))
!WRITE(*,*) v+6
!WRITE(FMT,'(I2)') v+6
!do i=1,npos
  !read(10,"(" // ADJUSTL(FMT) // "(E28.20E3,4x))") x(i),(ep(n,i),n=1,v),pot(1,i),pot(2,i),xmu12(i),chi1in(i),chi2in(i)
  !read(10) x(i),(ep(n,i),n=1,v),pot(1,i),pot(2,i),xmu12(i),chi1in(i),chi2in(i)
!write(10,101)x(i),(ep(n,i),n=1,v),pot(1,i),pot(2,i),xmu12(i),real(chi1(i)),aimag(chi1(i)),real(chi2(i)),aimag(chi2(i))
!enddo



!!!!!!!!!!!!!!!!!!LECTURE DES FICHIERS

    open(10,file='leivec.dat')
    open(11,file='lcis_mus.dat')
    open(12,file='lpsi00000.dat')
    do i=1,npos
        READ(10,'(17(es30.15e3))') x(i),(ep(n,i),n=1,v)
        READ(12,'(3(es30.15e3))') x(i),chi1in(i),chi2in(i)
        READ(11,'(4(es30.15e3))') x(i),pot(1,i),pot(2,i),xmu12(i)
    enddo
    close(10)
    close(11)
    close(12)




    !***********************************************************************
    !           Normalisation de la fonction d'onde initiale			
    !***********************************************************************
    do l=1,npos
        worka(l)=(abs(chi1in(l)))**2
    enddo
    call simpson( npos,x(2)-x(1),worka, normedeb)
	normedeb=dsqrt(normedeb)
    do l = 1, npos
        chi1(l) = chi1in(l)/normedeb
        chi2(l) = cmplx(chi2in(l),0.d0)
    !********************************************************************
    enddo

    !***********************************************************************
    !           Normalisation des états propres
    !***********************************************************************
    do i=1,v
    do l=1,npos
        worka(l)=(abs(ep(i,l)))**2
    enddo
    call simpson( npos,x(2)-x(1),worka, normedeb)
	normedeb=dsqrt(normedeb)
    do l = 1, npos
        ep(i,l) = ep(i,l)/normedeb
        worka(l)=(abs(ep(i,l)))**2
    !********************************************************************
    enddo
    enddo
    call simpson( npos,x(2)-x(1),worka, normedeb)

delr=x(2)-x(1)

rc0 = 1.3989d0 !position du paquet d'onde à t0
p0 = 0.0d0 !impulsion du paquet d'onde à t0
alpha = 13.019d0 !paramètre pour chi1 et chi2

    requ=2.d0 !valeur de r à l'equilibre = 2*a0 (a0=1 en u.a)
    diss=2.7925d0/27.2d0 !potentiel de dissociation de H2+
    massreduite=1.00794d0*1.6605d-27/(2.d0*9.10938356d-31)!918.0762887608628d0 !=masse proton/2 en u.a
!    write(*,*)"This is a test 2.1"
!    write(*,*) xmax,xmin,npos

!delr=(xmax-xmin)/(npos-1)
!read(*,*) test
    !write(*,*)"This is a test 2.53"
!        do i=1,npos
    !write(*,*)"This is a test 2.53"
!         x(i)=xmin+(i-1)*delr !création de l'axe des positions
!        enddo

    !write(*,*)"This is a test 2.53"
! do n=1,v
    !write(*,*)"This is a test 2.53"
!        do i=1,npos
    !write(*,*)"This is a test 2.53"
!          ep(n,i)=morse(diss,0.72d0,massreduite,requ,x(i),n-1) !construction des n etats vibrationnels sur la grille
!          work1(i)=(dabs(ep(n,i)))**2
!    enddo
    !write(*,*)"This is a test 2.53"

!          call simpson(npos,delr,work1,norme)
!          ep(n,:)=ep(n,:)/sqrt(norme) !normalisation des etats vibrationnels

! enddo
    !write(*,*)"This is a test 3"
! call eval(chi1, chi2, delr, xmin, p0, rc0, alpha, npos)
    !write(*,*)"This is a test 4"
! call pot_spec(pot(1,:),pot(2,:),xmu12, npos,delr,xmin) !construction des 2 potentiels de H2+ et du moment dipolaire
    !write(*,*)"This is a test 5"







!***********************************************************************
! Mise en place de la grille des positions, potentiels de Morse et
! etats propres associés
!***********************************************************************
 
! call pot_spec(pot(1,:),pot(2,:),xmu12, npos,delr,xmin) !construction des 2 potentiels de H2+ et du moment dipolaire
 ! AJOUTER DANS LE SCAN_E.f90
 ! pour ne pas ouvrir plusieur potentiel.dat
 !open(unit=1,name="potentiel.dat",status='replace') 
!	do i=1,npos
!	  write(1,*)x(i),pot(1,i),pot(2,i) 
!	enddo
! close(1)



! *******************************************************************  	
!    		Construction de la grille temporelle	
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
!	chi1(l)=dcmplx(ep(0,l),0d0)
!	chi2(l)=dcmplx(0d0,0d0)
! enddo
! do j=1,npos
!	chi1init(j)=chi1(j)
!   write(123556,*) x(j), chi1init(j)
! enddo



!***********************************************************************
! 	      	Normalisation de la fonction d'onde initiale
!***********************************************************************
!	do l=1,npos
!	worka(l)=(cdabs(chi1init(l)))**2
!	enddo
!   call simpson( npos,delr,worka, normedeb)
!   write(*,*) "NORME De Depart : ", normedeb
 !open(unit=1234567+iE0,name='chi1init.dat',status='replace')
 !open(unit=234579+iE0,name='chi2init.dat',status='replace')
!   do l = 1, npos
!         chi1(l) = chi1init(l)/dsqrt(normedeb)
!         chi2(l) = cmplx(chi2in(l),0.d0)
!********************************************************************
	!write(1234567+iE0,*)x(l),dreal(chi1(l)),dimag(chi1(l))
	!write(234579+iE0,*)x(l),dreal(chi2(l)),dimag(chi2(l))
!   enddo
 !close(1234567+iE0)
 !close(234579+iE0)




!********************************************************************
! 	   		Construction du champ
!********************************************************************
! sigma=periode/(2d0*2.3548d0)
! tmax=2d0*periode
!tc=200.d0*10.d0
!open(unit=2000+iE0,status='replace')
!call champir(champir1,wir,phase,t,ntps,1)
!call pulsechoice(pulsetype,champ,t,ntps,delt,logfile,E0,wir,phase,le0wattcm2,tc,te,tf)
!pseudocarre(champ,t,ntps,delt,ph)
write(*,*) "Temps final : ", tf
    E0wattcm2=E0
!    E0=wattcm22au(E0) ! TODO: IMP, E0 en u.a.
    !write(logfile, *) " iE0 = ",  iE0  
!    write(*,'(" E0 = ", E16.8 , " u.a. , ",  E16.8 , "  W/cm2" )') E0 , E0wattcm2 
    write(*,*) 'Intensite:',E0,'ua' 
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
         call calc_champ(champ2,wir,timeper,phase,nc,pi,E0)
         call calc_champ(champ1,wir,timeper-dtper,phase,nc,pi,E0)
         call airesint (int1tf, int2tf, int3tf, dtper, champ1, champ2)
      end do
      call calc_champ(champ1,wir,timeper,phase,nc,pi,E0)
      call calc_champ(champ2,wir,tf,phase,nc,pi,E0)
!    B. Friedrich and D. Herschbach, Alignment and trapping of molecules in intense laser fields, Phys. Rev. Lett. 74, 4623 (1995).
      call airesint(int1tf, int2tf, int3tf, tf-timeper, champ1, champ2)





!********************************************************************
!	Application du split operator-Ouverture de la boucle temps 
!********************************************************************






open(102,file='out/rmoy.dat')
open(105,file='see.py')

write(105,'(a)') 'import numpy as np'
write(105,'(a)') 'import matplotlib.pyplot as plt'
write(105,'(a)') ''
write(105,'(a)') 'x=np.loadtxt("out/rmoy.dat",usecols=[0])'
write(105,'(a)') 'y=np.loadtxt("out/rmoy.dat",usecols=[1])'
write(105,'(a)') 'plt.plot(x,y)'
write(105,'(a)') 'plt.title("R moyen")'
write(105,'(a)') 'plt.xlabel("Temps (ua)")'
write(105,'(a)') 'plt.ylabel("Distance (ua)")'
write(105,'(a)') 'plt.tight_layout()'
write(105,'(a)') 'plt.savefig("out/rmoy.png")'
write(105,'(a)') 'plt.close()'
write(105,'(a)') ''
write(105,'(a)') 'x=np.loadtxt("out/pbound.dat",usecols=[0])'
write(105,'(a)') 'y=np.loadtxt("out/pbound.dat",usecols=[1])'
write(105,'(a)') 'plt.plot(x,y)'
write(105,'(a)') 'plt.title("Champ")'
write(105,'(a)') 'plt.xlabel("Temps (ua)")'
write(105,'(a)') 'plt.ylabel("Intensite (ua)")'
write(105,'(a)') 'plt.tight_layout()'
write(105,'(a)') 'plt.savefig("out/champ.png")'
write(105,'(a)') 'plt.close()'
write(105,'(a)') ''
write(105,'(a)') 'y=np.loadtxt("out/pbound.dat",usecols=[2])'
write(105,'(a)') 'plt.plot(x,y)'
write(105,'(a)') 'plt.title("Probabilite liaison")'
write(105,'(a)') 'plt.xlabel("Temps (ua)")'
write(105,'(a)') 'plt.ylabel("Probabilite")'
write(105,'(a)') 'plt.tight_layout()'
write(105,'(a)') 'plt.savefig("out/pbound.png")'
write(105,'(a)') 'plt.close()'
write(105,'(a)') ''
write(105,'(a,i5,a)') 'for i in range(',v,'):'
write(105,'(a)') '  y=np.loadtxt("out/pbound.dat",usecols=[i+3])'
write(105,'(a)') '  plt.plot(x,y)'
write(105,'(a)') '  plt.title("Probabilite liaison etat %d"%(i))'
write(105,'(a)') '  plt.xlabel("Temps (ua)")'
write(105,'(a)') '  plt.ylabel("Probabilite")'
write(105,'(a)') '  plt.tight_layout()'
write(105,'(a)') '  plt.savefig("out/pbound%03d.png"%(i))'
write(105,'(a)') '  plt.close()'

write(*,*) "int1tf", int1tf
      int1t0=0.d0  
      int2t0=0.d0  
      int3t0=0.d0  
      timeper = t0
 do i=1,nt
!********************************************************************

      do while(timeper.lt.t(i))
         timeper = timeper + dtper
         call calc_champ(champ2,wir,timeper,phase,nc,pi,E0)
         call calc_champ(champ1,wir,timeper-dtper,phase,nc,pi,E0)
         call airesint (int1t0, int2t0, int3t0, dtper, champ1, champ2)
      end do
      call calc_champ(champ1,wir,timeper,phase,nc,pi,E0)
      call calc_champ(champ2,wir,t(i),phase,nc,pi,E0)
      call airesint(int1tf, int2tf, int3tf, t(i)-timeper, champ1, champ2)
!********************************************************************
!	Calcul des aires temporelles du champ 
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
    call calc_champ(champ(i),wir,t(i),phase,nc,pi,E0)
   
    
   
!write(logfile,*) "Rmoyen du iE0 : ", iE0 , " : fort." , 70000+iE0

!call cpu_time ( time2 )
!write(*,*) "Time2 : ", time2
! PARTIE TRES LENTE DU PROGRAMME : CE CALCUL PREND PRESQUE 1 SEC PAR APPLICATION
!	do j=1,npos
!		chilie(j)=dcmplx(0d0,0d0)
!		do n=0,v
!			ctemp(j)=ep(n,j)*chi1(j)
!			call simpson(npos,delr,dreal(ctemp),normedeb)
!			call simpson(npos,delr,dimag(ctemp),norme)
!			chilie(j)=chilie(j)+ep(n,j)*(normedeb+cim*norme)
!		enddo
!		worka(j)=x(j)*(cdabs(chilie(j)))**2
!		tablea(j)=(cdabs(chilie(j)))**2
!	enddo
!	call simpson(npos,delr,worka,normedeb)
!	call simpson(npos,delr,tablea,norme)
!	rmoyenlie(i)=normedeb/norme
!	write(30+ph,*)t(i),rmoyenlie(i)!rmoyen de la partie liee du paquet d'onde

	         !if (((t(i).gt.(0.d0)).and.(t(i).le.(0.d0+delt))).or.((t(i).gt.(1.d0*periode/4.d0)).and.(t(i).le.(1.d0*periode/4.d0+delt))).or.((t(i).gt.(2.d0*periode/4.d0)).and.(t(i).le.(2.d0*periode/4.d0+delt))).or.((t(i).gt.(3.d0*periode/4.d0)).and.(t(i).le.(3.d0*periode/4.d0+delt))).or.((t(i).gt.(4.d0*periode/4.d0)).and.(t(i).le.(4.d0*periode/4.d0+delt))).or.((t(i).gt.(12.d0*periode/4.d0)).and.(t(i).le.(12.d0*periode/4.d0+delt))).or.((t(i).gt.(13.d0*periode/4.d0)).and.(t(i).le.(13.d0*periode/4.d0+delt))).or.((t(i).gt.(14.d0*periode/4.d0)).and.(t(i).le.(14.d0*periode/4.d0+delt))).or.((t(i).gt.(15.d0*periode/4.d0)).and.(t(i).le.(15.d0*periode/4.d0+delt))).or.((t(i).gt.(16.d0*periode/4.d0)).and.(t(i).le.(16.d0*periode/4.d0+delt)))) then
	
!	open(unit=2000+i+(ntps+2000)*ph)
!	open(unit=2*ntps+5000+i+(ntps+2000)*ph)
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
!		write(2000+i+(ntps+2000)*ph,*) x(j), dsqrt((dreal(chi1(j))**2+dimag(chi1(j))**2)),dsqrt((dreal(chi2(j))**2+ dimag(chi2(j))**2))
!construction des potentiels E- et E+
!		write(2*ntps+5000+i+(ntps+2000)*ph,*)x(j), vp1(j), vp2(j)
	enddo
!	close(2000+i+(ntps+2000)*ph)
!	close(2*ntps+5000+i+(ntps+2000)*ph)
			!endif

! if ((t(i).gt.(11071.75d0)).and.(t(i).le.(11072.5d0)))then
!	do j=1,npos
!		write(51,*)x(j),dreal(chi1(j)),dimag(chi1(j))
!	enddo

! endif
! if ((t(i).gt.(22143.75d0)).and.(t(i).le.(22144.5d0))) then
!	do j=1,npos
!		write(50,*)x(j),dreal(chi1(j)),dimag(chi1(j))
!	enddo
! endif

! do j=1,npos
!	ctemp=chi1init(j)*chi1(j)
!	worka(j)=(cdabs(ctemp(j)))**2 !a revoir (calcul d'autocorrelation)
! enddo
! call simpson (npos,delr,worka,auto_correl(i))
! write(52,*)t(i),auto_correl(i)

! maxpot1=maxloc(vp1reel,1)
 minpot2=minloc(vp2,1)
! rclapet1(i)=x(ideb+maxpot1)
 rclapet2(i)=x(minpot2)
! write(80000+iE0,*) t(i),rclapet2(i)
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
!	    Calcul de probabilité de dissociation
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
	   lieprobv(n)=(projreal**2 + projimag**2)
	        lieprob = lieprob + lieprobv(n)
!		if (i.eq.1) then		
!			write(49,*)n,lieprobv
!		endif
            end do
            write(101,lieprobnam) t(i), champ(i),lieprob,(lieprobv(n),n=1,v)

  !open((iE0+1)*1000000+i)
  !do j=1,npos
  !  write((iE0+1)*1000000+i,*)x(j),cdabs(chi1(j))**2,cdabs(chi2(j))**2
  !enddo
  !close((iE0+1)*1000000+i)
if (mod(i,100).eq.(0)) then
write(105,'(a)') ''
write(105,'(a,i5.5,a)') 'x=np.loadtxt("out/chip',i,'.dat",usecols=[0])'
write(105,'(a,i5.5,a)') 'y1r=np.loadtxt("out/chip',i,'.dat",usecols=[1])'
write(105,'(a,i5.5,a)') 'y1i=np.loadtxt("out/chip',i,'.dat",usecols=[2])'
write(105,'(a,i5.5,a)') 'y2r=np.loadtxt("out/chip',i,'.dat",usecols=[3])'
write(105,'(a,i5.5,a)') 'y2i=np.loadtxt("out/chip',i,'.dat",usecols=[4])'
write(105,'(a)') 'plt.plot(x,y1r,label="reel")'
write(105,'(a)') 'plt.plot(x,y1i,label="imag")'
write(105,'(a,i5,a)') 'plt.title("Chig t ',i,'")'
write(105,'(a)') 'plt.xlabel("Distance (ua)")'
write(105,'(a)') 'plt.ylabel("Intensite relative (ua)")'
write(105,'(a)') 'plt.legend()'
write(105,'(a)') 'plt.tight_layout()'
write(105,'(a,i5.5,a)') 'plt.savefig("out/chig',i,'.png")'
write(105,'(a)') 'plt.close()'
write(105,'(a)') 'plt.plot(x,y2r,label="reel")'
write(105,'(a)') 'plt.plot(x,y2i,label="imag")'
write(105,'(a,i5,a)') 'plt.title("Chiu t ',i,'")'
write(105,'(a)') 'plt.xlabel("Distance (ua)")'
write(105,'(a)') 'plt.ylabel("Intensite relative (ua)")'
write(105,'(a)') 'plt.legend()'
write(105,'(a)') 'plt.tight_layout()'
write(105,'(a,i5.5,a)') 'plt.savefig("out/chiu',i,'.png")'
write(105,'(a)') 'plt.close()'
	write(chinam,'("out/chip",i5.5,".dat")') i
	open(104,file=chinam)
      do l = 1, npos
	 w1(l) = cdabs(chi1(l))**2
	 w2(l) = cdabs(chi2(l))**2
!	 write(1,1004) dreal(chi1(l)),dimag(chi1(l)),  dreal(chi2(l)),dimag(chi2(l))
	 write(104,'(5(es30.15e3))') x(l), chi1(l), chi2(l)
      end do
	close(104)
end if


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
	do j=1,npos
		worka(j)=(cdabs(chi1(j)))**2 !position moyenne du paquet d'onde de l'état fondamental
		workb(j)=(cdabs(chi2(j)))**2 !position moyenne du paquet d'onde de l'état fondamental
	enddo
	call simpson(npos,delr,worka,norme1)
	call simpson(npos,delr,workb,norme2)
	!kmoyen(i)=normedeb/norme
	write(102,'(5(es30.15e3))')t(i),rmoyen(i),dispers(i),norme1,norme2!r moyen du paquet d'onde total
!	write(30000+iE0,*)t(i),dispers(i)!r moyen du paquet d'onde total


!    if (mod(i,100).eq.0) then
      write(*,*) i, "/" , nt , "t = ",t(i),int(i*100/nt),'%'
!    end if
  ! end of the loop on time
 enddo 

 close(101)
 close(102)
 close(105)








 pbfin=pbfin/100.d0



      call asympt(tf, tf, psik1, psik2, chi1, chi2,&
                 zcutA, 4*npos, npos, massreduite,&
                 int1tf, int2tf, int3tf,&
                 int1t0, int2t0, int3t0, delr,&
                 zwork1, zwork2, zwork3,&
                 zwork4, wsbig,dissprob)
      call ZVEM(npos,chi1,1,zcutI,1,chi1,1)
      call ZVEM(npos,chi2,1,zcutI,1,chi2,1)
!      write(fichier1,10000)10000+iE0
!      write(fichier2,10000)20000+iE0
10000 FORMAT ("fichier",I5,".dat")
      !fichier1="fichier1.dat"
      !fichier2="fichier2.dat"
!      write(*,*) "fichier1 = ", fichier1
!      write(*,*) "fichier1 = ", fichier2
!      open(1,file=fichier1,status='unknown')
!      open(2,file=fichier2,status='unknown')

!      do l = 1, npos
!	 w1(l) = cdabs(chi1(l))**2
!	 w2(l) = cdabs(chi2(l))**2
!	 write(1,1004) dreal(chi1(l)),dimag(chi1(l)),  dreal(chi2(l)),dimag(chi2(l))
!	 write(2,1005) x(l), chi1(l), chi2(l)
!      end do
!      close(1,status='keep')
!      close(2,status='keep')
!      write(fichier3,10000)30000+iE0
!      write(fichier4,10000)40000+iE0
!      write(fichier5,10000)50000+iE0
!      open(1,file=fichier3,status='unknown')
!      open(2,file=fichier4,status='unknown')
!      open(3,file=fichier5,status='unknown')
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
!         write(1,1002) xk*evbyau, work5(i)
!         write(2,1002) xk*evbyau, work2(i)
!         write(3,1002) xk*evbyau, spk
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
!         write(1,1002) xk*evbyau, work5(i)
!         write(2,1002) xk*evbyau, work2(i)
!         write(3,1002) xk*evbyau, spk
      end do
!      close(1)
!      close(2)
!      close(3)
!   write(*,*) 'test1'
      call simpson(npos, delr, w1, xnorm1)
      call simpson(npos, delr, w2, xnorm2)
!   write(*,*) 'test2'
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



   !!!! WRITE TITLE IN GNUPLOT
!   write(charnum(1),'(E16.8)')E0
!   write(charnum(2),'(E16.8)')E0wattcm2

!   pbname="set title 'Pbound for E="//trim(charnum(1))//" u.a. , "//trim(charnum(2))//" w/cm2'"
!   write(123456,"(A50)") pbname

   !!! SET TERMINAL AND OUTPUT FILE
!   write(123456,*) "set terminal png"
!   write(charnum(3),"(I5)")10000+iE0
!   pbname="set output 'pbound_"//trim(charnum(3))//".png'"
!   write(123456,*) pbname
   
   !!! SET PLOTTING DATA
!   pbname="plot [0:35] [0:2] 'fort."//trim(charnum(3))//"' u 1:2 w l"
!   write(123456,*) pbname

   !!! PLOT THE FIELD
!   pbname="set title 'Laser field for E="//trim(charnum(1))//" u.a. , "//trim(charnum(2))//" w/cm2'"
!   write(123456,"(A50)") pbname
!   write(123456,*) "set terminal png"
!   pbname="set output 'champ_"//trim(charnum(3))//".png'"
!   write(123456,*) pbname
!   pbname="plot 'fort."//trim(charnum(3))//"' u 1:3 w l"
!   write(123456,*) pbname
   

   !!! PLOT RMOYEN
!   write(charnum(4),"(I5)")70000+iE0
!   pbname="set title 'rmoyen field for E="//trim(charnum(1))//" u.a. , "//trim(charnum(2))//" w/cm2'"
!   write(123456,"(A50)") pbname
!   write(123456,*) "set terminal png"
!   pbname="set output 'rmoyen_"//trim(charnum(4))//".png'"
!   write(123456,*) pbname
!   pbname="plot 'fort."//trim(charnum(4))//"' u 1:2 w l"
!   write(123456,*) pbname
!   pbfin=lieprob

!  close(10000+iE0)
!   close(30000+iE0)
!    close(70000+iE0)
!  close((iE0+1)*10000000)

!call cpu_time ( ftime )
!write(logfile,*) "End of the program ", ftime
!if ( ftime-btime > 60*60 )  then
!    write(string,*) (ftime-btime)/60*60 , "heures," ,  ((ftime-btime)-(ftime-btime)/60*60)/60 , " min et ",    ftime-btime-(((ftime-btime)-(ftime-btime)/60*60)/60) , " sec "
!else if ( ftime-btime > 60)  then
!    write(string,*)  (ftime-btime)/60 , " min et ",    ftime-btime-((ftime-btime)/60) , " sec "
!else
!     write(string,*) ftime-btime , " sec "
!end if
!write(logfile,'(A120)') "Time of calculation  ", string

1002  format(2(e16.8e3,2x))
1003  format(3(e16.8e3,2x))
1004  format(4(e16.8e3,2x))
1005  format(5(e16.8e3,2x))
1006  format(6(e16.8e3,2x))
1007  format(7(e16.8e3,2x))
end program ddq

!filename='obs_'//job
!open(60,file=filename,status='unknown',form='formatted')
!write (chain(6:8),'(i3.3)') int
!*******************************************************************  
!
!	Calcul du paquet d'onde initial
!
!*******************************************************************


     subroutine eval(cw1, cw2, delr, rdeb,p0, rc0, alpha, npos)

      integer npos
      double complex cw1(npos), cw2(npos)
      double precision delr, rdeb, p0, rc0, alpha
      double precision pi, r
      double complex cnul, cim, cpoi, cval, arg
      integer l

      cnul = dcmplx(0.d0,0.d0)
      cim = dcmplx(0.d0,1.d0)
      pi = 3.141592654d0
      cpoi = cdsqrt(cdsqrt(dcmplx(2.d0*alpha/pi,0.d0)))
      r = rdeb-delr
      do l = 1, npos
         r = r + delr
         arg = dcmplx(-alpha*(r-rc0)**2, p0*(r-rc0))
	if (dble(arg).lt.(-7.d2)) then
		cval=dcmplx(0.d0,0.d0)
	else
         cval = cpoi*cdexp(arg)
	end if
         cw1(l) = cval
         cw2(l) = cnul

      end do
 
      return
      end

!***************************************************************
! 		Calcul norme d'une fonction complexe
!***************************************************************
	subroutine simps(func, vint, delti, npl)

      integer j, npl
      double complex func(npl)
      double precision  vint, delti
      
      vint=0d0
      do j = 1, npl-1
         vint=vint+delti*sqrt(cdabs(func(j))**2) 
      end do
      return
      end

!***************************************************************
! 		Integration Simpson
!***************************************************************


