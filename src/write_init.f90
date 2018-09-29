program write_init
    use morse1
    use gen
    use simps0n
    integer :: nE0,pulsetype,iE0,id,le0wattcm2,logfile,nt,i,v,npos,n,cas,nc
    real(8), allocatable :: ep(:,:)
    real(8), allocatable :: pot(:,:)
    real(8) :: phase,wir,requ,diss,massreduite,TVIB,tsync
    real(8) :: tc,dw,te,tf,t0,period,dt,delr!,MATH_PI
    real(8) :: E0min,E0max,dE0,E0,norme,alpha,rc0,p0
    real(8) :: xmin,xmax,ntl,NP,NPE,beta
    real(8), allocatable :: t(:),x(:),work1(:),xmu12(:)
    real(8), allocatable :: pbfin(:)
    complex(8), allocatable :: chi1(:), chi2(:)
    character(len=50) :: test
    CHARACTER(LEN=20) FMT
    open(5,name="input",status="old")
    namelist /iofile/ npos,xmax,nE0
    read(5,iofile)




    allocate(work1(npos))
    allocate(chi1(npos))
    allocate(chi2(npos))
    allocate(pot(2,npos))
    allocate(xmu12(npos))
    
    write(*,*)"This is a test"
    
!    xmax=238.d0
    xmin=2.d-3
    wir=2.d0*MATH_PI/(ntl*Tvib)
    
rc0 = 1.3989d0 !position du paquet d'onde à t0
p0 = 0.0d0 !impulsion du paquet d'onde à t0
alpha = 13.019d0 !paramètre pour chi1 et chi2

    
    
    
    logfile=987654
open(987654,name="log.dat",status="replace")
v=19
    allocate(ep(v,npos))
    allocate(x(npos))
    requ=2.d0 !valeur de r à l'equilibre = 2*a0 (a0=1 en u.a) 
    diss=2.7925d0/27.2d0 !potentiel de dissociation de H2+
    massreduite=918.0762887608628d0 !=masse proton/2 en u.a
    write(*,*)"This is a test 2"

delr=(xmax-xmin)/(npos-1)
!read(*,*) test
	do i=1,npos
	 x(i)=xmin+(i-1)*delr !création de l'axe des positions
	enddo

 do n=1,v
	do i=1,npos
 	  ep(n,i)=morse(diss,0.72d0,massreduite,requ,x(i),n-1) !construction des n etats vibrationnels sur la grille
	  work1(i)=(dabs(ep(n,i)))**2
    enddo

	  call simpson(npos,delr,work1,norme)
	  ep(n,:)=ep(n,:)/sqrt(norme) !normalisation des etats vibrationnels

 enddo
    write(*,*)"This is a test 3"
 call eval(chi1, chi2, delr, xmin, p0, rc0, alpha, npos)
    write(*,*)"This is a test 4"
 call pot_spec(pot(1,:),pot(2,:),xmu12, npos,delr,xmin) !construction des 2 potentiels de H2+ et du moment dipolaire
    write(*,*)"This is a test 5"
!open(10,file='init.fmt',access='DIRECT', recl=2+npos*(v+6))
open(10,file='init.dat')
!write(10)npos
!write(10)v
write(10,*)npos
write(10,*)v
WRITE(FMT,*) v+6
do i=1,npos
  write(*,*) x(i),pot(1,i),pot(2,i)
  WRITE(10,"(" // ADJUSTL(FMT) // "(E28.20E3),4x)") x(i),(ep(n,i),n=1,v),pot(1,i),pot(2,i),xmu12(i),real(chi1(i)),real(chi2(i))
  !WRITE(10) x(i),(ep(n,i),n=1,v),pot(1,i),pot(2,i),xmu12(i),real(chi1(i)),real(chi2(i))
!write(10,101)x(i),(ep(n,i),n=1,v),pot(1,i),pot(2,i),xmu12(i),real(chi1(i)),aimag(chi1(i)),real(chi2(i)),aimag(chi2(i))
enddo




end program write_init
