program eigenvals
  use num_mod
  implicit none

!Variables
integer :: n,i,npts,spt,cut,lvl,ios
double precision :: m,hbar,Emin,Emax,dE,Emint,Emaxt,Eroll,Etemp,slopemin,slopemax,sloper
double precision,dimension(:),allocatable :: x,V,E,slope,negslope,y
character*12 :: filename
logical :: notfound

!Read potential file
  open(1,file='+pot.dat')
  npts=0
  do								!Read # of points
    read(1,*,iostat=ios)
    if (ios.ne.0) then
      exit
    end if
    npts=npts+1
  end do
  rewind 1
  allocate(x(npts),V(npts),y(npts))
  do i=1,npts							!Read x and potential
    read(1,*) x(i),V(i)
  end do

!Find minimum for slope comparison point
  do i=1,npts
    if (V(i).eq.minval(V)) then
      spt=i
    end if
  end do
  if ((spt.eq.1).or.(spt.eq.npts)) then
    spt=npts/2
  end if

!Starting conditions
  m=1.00794d0*1.6605d-27/(2.d0*9.10938356d-31)
  hbar=1.d0
  cut=100000		!# of slices of energy for scan
  Emin=minval(V)
  Emax=V(npts)
  dE=(Emax-Emin)/dble(cut)
  allocate(E(cut),slope(cut),negslope(cut))

!Scan with Numerov method
  open(2,file='scan.dat')
  open(3,file='eigenvalues.dat')
  lvl=0
  do n=1,cut
    E(n)=Emin+(dble(n)*dE)
    call numerov(npts,spt,m,hbar,E(n),x,V,slope(n),y,1.d0,.false.)
    call numerov(npts,spt,m,hbar,E(n),x,V,negslope(n),y,-1.d0,.false.)
    write(2,'(3es28.15e3)') E(n),slope(n),negslope(n)

!Find eigenvalues
!Starting positive from the left
    if (n.ge.3) then
    if((slope(n-1).lt.slope(n-2)).and.(slope(n-1).lt.slope(n))) then
      if (slope(n-2).gt.slope(n)) then
        Emint=E(n-1)
        slopemin=slope(n-1)
        Emaxt=E(n)
        slopemax=slope(n)
      else
        Emint=E(n-2)
        slopemin=slope(n-2)
        Emaxt=E(n-1)
        slopemax=slope(n-1)
      end if
      Etemp=Emint
      notfound=.true.
      Eroll=(Emint+Emaxt)/2.d0
      do while (notfound)
        call numerov(npts,spt,m,hbar,Eroll,x,V,sloper,y,1.d0,.false.)
        if (slopemin.gt.slopemax) then
          Emint=Eroll
          slopemin=sloper
        else
          Emaxt=Eroll
          slopemax=sloper
        end if
        Etemp=Eroll
        Eroll=(Emint+Emaxt)/2.d0
        if (Eroll.eq.Etemp) then
          call numerov(npts,spt,m,hbar,Eroll,x,V,sloper,y,1.d0,.true.)
          if (sloper.gt.(1.d-10)) then
            go to 100
          end if
          notfound=.false.
          !write(3,'(i5.5,es28.15e3)') lvl,Eroll
          write(3,'(i5.5,2es28.15e3)') lvl,Eroll,sloper
          write(filename,'(a,i5.5,a)') 'psi',lvl,'.dat'
          open(4,file=filename)
            do i=1,npts
              write(4,'(2es28.15e3)') x(i),y(i)
            end do
          close(4)
        end if
      end do
      lvl=lvl+1
!Starting negative from the left
    else  if ((negslope(n-1).lt.negslope(n-2)).and.(negslope(n-1).lt.negslope(n))) then
      if (negslope(n-2).gt.negslope(n)) then
        Emint=E(n-1)
        slopemin=negslope(n-1)
        Emaxt=E(n)
        slopemax=negslope(n)
      else
        Emint=E(n-2)
        slopemin=negslope(n-2)
        Emaxt=E(n-1)
        slopemax=negslope(n-1)
      end if
      Etemp=Emint
      notfound=.true.
      Eroll=(Emint+Emaxt)/2.d0
      do while (notfound)
        call numerov(npts,spt,m,hbar,Eroll,x,V,sloper,y,-1.d0,.false.)
        if (slopemin.gt.slopemax) then
          Emint=Eroll
          slopemin=sloper
        else
          Emaxt=Eroll
          slopemax=sloper
        end if
        Etemp=Eroll
        Eroll=(Emint+Emaxt)/2.d0
        if (Eroll.eq.Etemp) then
          call numerov(npts,spt,m,hbar,Eroll,x,V,sloper,y,-1.d0,.true.)
          if (sloper.gt.(1.d-10)) then
            go to 100
          end if
          notfound=.false.
          !write(3,'(i5.5,es28.15e3)') lvl,Eroll
          write(3,'(i5.5,2es28.15e3)') lvl,Eroll,sloper
          write(filename,'(a,i5.5,a)') 'psi',lvl,'.dat'
          open(4,file=filename)
            do i=1,npts
              write(4,'(2es28.15e3)') x(i),y(i)
            end do
          close(4)
        end if
      end do
      lvl=lvl+1
    end if
    end if
100 continue
    write(*,'(a,i3,a)') 'Scanning possible eigenvalues: ',int(1.d2*(dble(n)/dble(cut))),'%'
  end do
  close(2)
  close(3)

end program eigenvals
