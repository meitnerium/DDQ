program scanE
    use morse1
    use gen
    use simps0n
    use mpi
    include 'mkl_dfti_examples.fi'
    integer :: rank, size, tag, ierror , status(MPI_STATUS_SIZE)
    integer :: nE0,pulsetype,iE0,id,le0wattcm2,logfile,nt,i,v,npos,n,cas,nc
    character(LEN=50) :: title
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

    call MPI_Init( ierror ) 
    write(*,*) "ERROR AFTER MPI_INIT = ",ierror
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
    write(*,*) "ERROR AFTER MPI_COMM_SIZE = ",ierror
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
    write(*,*) "ERROR AFTER MPI_COMM_RANK = ",ierror
    open(5,name="input",status="old")
    namelist /iofile/ nE0,npos,xmax
    read(5,iofile)




    allocate(work1(npos))
    allocate(chi1(npos))
    allocate(chi2(npos))
    allocate(pot(2,npos))
    allocate(xmu12(npos))
    
    
    !nE0=(E0max-E0min)/dE0+1
    allocate(pbfin(0:nE0))
    iE0=0

    open(6554346,name='E0value.dat',status="replace")

    do while ((iE0+(rank)).le.nE0)  
        !id = omp_get_thread_num ( )
        if (iE0+(rank).le.nE0) then
          write(*,*) "rank = ",rank
          write(*,*) "iE0+(rank) " , iE0+(rank), "E0", E0
	  write(6554346,*) "iE0+(rank) " , iE0+(rank), "E0", E0
          call travail(iE0+rank)
          write(999654,*)(iE0+(rank))
          end if
        iE0=iE0+size
    end do 

    write(*,*) "test fin de programme"
    close(6554346)
    call MPI_FINALIZE(ierror)
    write(*,*) "ERROR AFTER MPI_FINALIZE = ",ierror
end program scanE
