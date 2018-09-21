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
    real(8), allocatable :: t(:),x(:),work1(:),xmu12(:),tmprchi1(:),tmpichi1(:),tmprchi2(:),tmpichi2(:)
    real(8), allocatable :: pbfin(:)
    complex(8), allocatable :: chi1(:), chi2(:)
    character(len=50) :: test
    CHARACTER(LEN=20) FMT


    
    
!    xmax=238.d0
    xmin=2.d-3
    wir=2.d0*MATH_PI/(ntl*Tvib)
    
rc0 = 1.3989d0 !position du paquet d'onde à t0
p0 = 0.0d0 !impulsion du paquet d'onde à t0
alpha = 13.019d0 !paramètre pour chi1 et chi2

    
    
    
    logfile=987654
open(987654,name="log.dat",status="replace")
v=19
open(10,file='init.dat')
read(10,*)npos
    allocate(work1(npos))
    allocate(chi1(npos),tmprchi1(npos),tmpichi1(npos))
    allocate(chi2(npos),tmprchi2(npos),tmpichi2(npos))
    allocate(pot(2,npos))
    allocate(xmu12(npos))
    allocate(x(npos))
read(10,*)v
    allocate(ep(v,npos))
WRITE(FMT,*) v+6
do i=1,npos
  read(10,"(" // ADJUSTL(FMT) // "E18.6E3)") x(i),(ep(n,i),n=1,v),pot(1,i),pot(2,i),xmu12(i),chi1(i),chi2(i)
  write(*,*) x(i),pot(1,i),pot(2,i)
!write(10,101)x(i),(ep(n,i),n=1,v),pot(1,i),pot(2,i),xmu12(i),real(chi1(i)),aimag(chi1(i)),real(chi2(i)),aimag(chi2(i))
enddo




end program write_init
