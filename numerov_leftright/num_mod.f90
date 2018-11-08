module num_mod
  implicit none

contains

!************************************************************************************
!************************************************************************************
subroutine numerov(npts,spt,m,hbar,E,x,V,slope,y,sig,yes)
!************************************************************************************
!************************************************************************************

!Variables
integer :: i,j,slopts
double precision :: dx
double precision,dimension(:),allocatable :: yl,yr
integer,intent(in) :: npts
double precision,intent(in) :: m,hbar,E,sig
double precision,dimension(npts),intent(in) :: x,V
logical,intent(in) :: yes
integer,intent(inout) :: spt
double precision,intent(out) :: slope
double precision,dimension(npts) :: g
double precision,dimension(npts),intent(out) :: y

!Starting conditions
  slopts=100					!How many points to compare?
  allocate(yl(slopts+1),yr(slopts+1))
  dx=x(2)-x(1)
  y(1)=0.d0
  y(npts)=0.d0
  y(2)=1.d-20*sig
  y(npts-1)=1.d-20
100 continue

!Left-right Numerov method
  do i=1,spt+slopts						!From the left
    g(i)=2.d0*m*(E-V(i))/(hbar**2.d0)
    if (i.gt.2) then
      y(i)=((2.d0*(1.d0-((5.d0/1.2d1)*(dx**2.d0)*g(i-1)))*y(i-1))-((1.d0+&
((1.d0/1.2d1)*(dx**2.d0)*g(i-2)))*y(i-2)))/(1.d0+((1.d0/1.2d1)*(dx**2.d0)*g(i)))
    end if
  end do
  do i=1,slopts+1						!Attribute comparison points
    yl(i)=y(spt-1+i)
    if (yl(i).eq.(0.d0)) then
      spt=spt+1+i
      go to 100
    end if
  end do
  do i=1,npts-spt+1						!From the right
    j=npts-i+1
    g(j)=2.d0*m*(E-V(j))/(hbar**2.d0)
    if (i.gt.2) then
      y(j)=((2.d0*(1.d0-((5.d0/1.2d1)*(dx**2.d0)*g(j+1)))*y(j+1))-((1.d0+&
((1.d0/1.2d1)*(dx**2.d0)*g(j+2)))*y(j+2)))/(1.d0+((1.d0/1.2d1)*(dx**2.d0)*g(j)))
    end if
  end do
  do i=1,slopts+1						!Attribute comparison points
    yr(i)=y(spt-1+i)
    if (yr(i).eq.(0.d0)) then
      spt=spt+1+i
      go to 100
    end if
  end do
  slope=0.d0
  do i=2,slopts							!Comparison of the slopes
    yr(i)=yr(i)*(abs(yl(1))/abs(yr(1)))
    slope=slope+abs(yl(i)-yr(i))
  end do
  if (yes) then							!Normalize function if correct E
    do i=1,npts-spt+1
      j=npts-i+1
      y(j)=y(j)*(abs(yl(1))/abs(yr(1)))
    end do
  end if

end subroutine numerov
!************************************************************************************

end module num_mod
