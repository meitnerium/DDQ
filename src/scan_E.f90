program scanE
    use omp_lib
    Use MKL_DFTI
    use morse1
    use gen
    use simps0n
    !use mpi
    !include 'mpif.h'
    include 'mkl_dfti_examples.fi'
    integer :: rank, size, tag, ierror ! status(MPI_STATUS_SIZE),    ierror
    integer :: nE0,pulsetype,iE0,id,le0wattcm2,logfile,nt,i,v,npos,n,cas,nc
    character(LEN=50) :: title
    real(8), allocatable :: ep(:,:)
    real(8), allocatable :: pot(:,:)
    real(8) :: phase,wir,requ,diss,massreduite,TVIB,tsync
    real(8) :: tc,dw,te,tf,t0,period,dt,delr
    real(8) :: E0min,E0max,dE0,E0,norme,alpha,rc0,p0
    real(8) :: xmin,xmax,ntl,NP,NPE
    real(8), allocatable :: t(:),x(:),work1(:),xmu12(:)
    real(8), allocatable :: pbfin(:)
    complex(8), allocatable :: chi1(:), chi2(:)
    character(len=50) :: test
    !Call MPI_Init( ierror )
    !write(*,*) "ERROR AFTER MPI_INIT = ",ierror
    !call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
    !write(*,*) "ERROR AFTER MPI_COMM_SIZE = ",ierror
    !call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
    !write(*,*) "ERROR AFTER MPI_COMM_RANK = ",ierror
    open(5,name="input",status="old")
    namelist /iofile/ t0,title, pulsetype,E0min,E0max,dE0,ntl,phase,le0wattcm2,dt,npos,TVIB,cas,tsync,xmax,nc
    read(5,iofile)




    allocate(work1(npos))
    allocate(chi1(npos))
    allocate(chi2(npos))
    allocate(pot(2,npos))
    allocate(xmu12(npos))
    
    
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

!write(*,*) "******************************************************************"
!write(*,*) "* Quantum Wave Packet Dynamics of H2+ by split operator technics *"
!write(*,*) "******************************************************************"
!write(*,*) "* Title : " , title 
write(logfile,*) "Title : " , title
!if (pulsetype.eq.1) then
!    write(*,*) "* Pulsetype = Squarepulse "  
!    write(logfile,*) "Pulsetype = Squarepulse" 
!end if
period=2*MATH_PI/wir
phase=phase*MATH_PI
!t0=0.d0
if (cas.eq.1) then 
    tc=tsync-period/2.d0
else if (cas.eq.2) then
    tc=tsync-period/4.d0
else
    write(*,*) "cas must be 1 (trapping) or 2(dissociative)"
    stop 10
end if
!write(*,*) "tc = " , tc , " en fs : " , tc*tau2fs 
!write(*,*) "period = " , period , " en fs : " , period*tau2fs 
!write(*,*) "tvib = " , TVIB , " en fs : " , TVIB*tau2fs 
write(logfile,*) "tc = " , tc , " en fs : " , tc*tau2fs 
write(logfile,*) "period = " , period , " en fs : " , period*tau2fs 
write(logfile,*) "tvib = " , TVIB , " en fs : " , TVIB*tau2fs 
write(logfile,*) "wir in scan_E = " , wir , " TL = " , 2.d0*MATH_PI/(wir*TVIB) ," TVIB" 
!pause 10
!pause 10
te=tc+2.d0*Tvib
tf=6.d0*Tvib
nt=(tf-t0)/dt+1
!101 dt=(tf-t0)/(nt-1)
!if (dt.gt.2.d0) then
!    nt=nt*2
!    go to 101
!end if
write(logfile,*) "nt = ", nt 
write(*,*) "nt = " , nt, dt, tf, tc, period
allocate(t(nt))
do i=1,nt
    t(i)=t0+(i-1)*dt
end do 
delr=(xmax-xmin)/(npos-1)

write(*,*)delr
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

      ! ajouter dans le scan_E.f90
	open(unit=(10+n),status='replace')
	do i=1,npos
		write(10+n,*)x(i),ep(n,i)!ecriture des etats propres d'indice n dans fort.10+n
	enddo	
	close(10+n)
 enddo
 call eval(chi1, chi2, delr, xmin, p0, rc0, alpha, npos)
 call pot_spec(pot(1,:),pot(2,:),xmu12, npos,delr,xmin) !construction des 2 potentiels de H2+ et du moment dipolaire

    nE0=(E0max-E0min)/dE0+1
    allocate(pbfin(0:nE0))
    iE0=0
    rank=0
    size=1
	id=0
    open(6554346,name='E0value.dat',status="replace")
    do while ((iE0+(rank)).le.nE0)  
        !id = omp_get_thread_num ( )
        write(*,*) "id = ",id
        E0=E0min+(iE0+rank)*dE0
        E0=wattcm22au(E0)
	    write(*,*) "iE0+(rank) " , iE0+(rank), "E0", E0
	    write(6554346,*) "iE0+(rank) " , iE0+(rank), "E0", E0
        call travail(t0,E0,title,pulsetype,wir,phase,le0wattcm2,tc,te,tf,iE0+rank,logfile,t,nt,ep,npos,v,x,id,dt,nt,xmu12,pot,delr,chi1,chi2,massreduite,pbfin(iE0+rank),TVIB,nc)
        write(999654,*)iE0,E0,pbfin(iE0)
         iE0=iE0+size
    end do 
    write(*,*) "test fin de programme"
    close(6554346)
    close(logfile)
   !call MPI_FINALIZE(ierror)

end program scanE
