Module math
! Contains common mathematical functions and operations used in molecular dynamics programs
!use Basics
!use blas95
!use lapack95
!use f95_precision
!use blas95
!use lapack95
!use f95_precision


contains



!****************************************************************************************
!****************************************************************************************
Subroutine Diagonalize(a,lda,evec,eval)
! Computes the eigenvalues and eigenvectors of a hermitian complex matrix
!****************************************************************************************
!****************************************************************************************

use basics

IMPLICIT NONE
!variables for zheev routine in LAPACK
Integer(kind=int_4), intent(in) :: lda !dimensionality of the problem
complex(kind=comp_16), intent(inout), dimension(lda,lda) :: a !input matrix to diagonalize, gets replaced with matrix of eigenvectors
complex(kind=comp_16), allocatable, dimension(:) :: work !working array for zheev
real(kind=real_8), allocatable, dimension(:) :: rwork !working array for zheev
real(kind=real_8), allocatable, dimension(:) :: w !array containing the eigenvalues
Integer(kind=int_4) :: info !dimension of the allocatable working array and information output parameter
Integer :: lwork

!variables introduced to interface hgeev with the current program
complex(kind=comp_16), allocatable, dimension(:,:) :: aaa !the working matrix, this avoids the input matrix a to be overwritten
complex(kind=comp_16), intent(out), dimension(lda) :: eval !eigenvalues will be copied in this array
complex(kind=comp_16), intent(out), dimension(lda,lda) :: evec !column-eigenvectors will be copied in this array
! other variables
Integer(kind=int_4) :: i,j
!memory allocation
allocate(aaa(lda,lda))
allocate(w(lda))
allocate(rwork(lda*3-2))

!make a copy of the input matrix into an internal working matrix
w=0.d0
rwork=0.d0
info=0
!do i=1,lda
!do j=1,lda
!aaa(i,j)=a(i,j)
!enddo
!enddo
aaa=a

!first run with a negative value of lwork to determine the size of working arrays

allocate(work(1))
work=dcmplx(0.d0,0.d0)
lwork=-1
! call zgeev('V','N',lda,aaa,lda,w,vl,lda,vr,lda,work,lwork,rwork,info)
! call zheev('N','U',lda,aaa,lda,w,work,lwork,rwork,info)
!write(*,*) "TEST",lda,aaa,lda,w,work,lwork,rwork,info
 call zheev('V','U',lda,aaa,lda,w,work,lwork,rwork,info)

! call heev(aaa,w,'V','U',info)
!write(*,*)'lda',lda
!write(*,*)'w',w
!write(*,*)'work',work
!write(*,*)'lwork',lwork
!write(*,*)'rwork',rwork
!write(*,*)'info',info
!STOP'prezheev'

if(info.ne.0)then
write(*,*)'Error initializing length of work arrays for matrix diagonalization. info=',info
write(*,*)'Calculation aborted',lwork
goto 22
endif

aaa=a
lwork=idint(dreal(work(1)))
!write(*,*)'lwork',lwork
!write(*,*)'w',w
!write(*,*)'info',info
deallocate(work)
!second run for the true calculation, lwork being optimally determined
allocate(work(lwork))
work=dcmplx(0.d0,0.d0)
! call zgeev('V','N',lda,aaa,lda,w,vl,lda,vr,lda,work,lwork,rwork,info)
! call zheev('N','U',lda,aaa,lda,w,work,lwork,rwork,info)
info=0
!write(*,*)lda
do i=1,lda
!  write(*,*)(aaa(i,j),j=1,lda)
end do

 !write(*,*) 'test2',lda,aaa,lda,w,work,lwork,rwork,info
 call zheev('V','U',lda,aaa,lda,w,work,lwork,rwork,info)

! call heev(aaa,w,'V','U',info)
deallocate(work)

if(info.ne.0)then
write(*,*)'Error during matrix diagonalization. info=',info
write(*,*)'Calculation aborted'
goto 22
endif

 !call cchop(aaa,lda,lda)  !suppression des chop
! call cchop(w,lda,1)

do i=1,lda
!eval(i)=dcmplx(chop(w(i)),0.d0)
	eval(i)=dcmplx(w(i),0.d0)
	do j=1,lda
		evec(i,j)=aaa(i,j)
	enddo
enddo

goto 22
22 continue
end subroutine Diagonalize


end Module math
