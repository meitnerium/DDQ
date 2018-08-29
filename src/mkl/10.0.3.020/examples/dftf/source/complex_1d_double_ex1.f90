!*******************************************************************************
!                              INTEL CONFIDENTIAL
!   Copyright(C) 2003-2008 Intel Corporation. All Rights Reserved.
!   The source code contained  or  described herein and all documents related to
!   the source code ("Material") are owned by Intel Corporation or its suppliers
!   or licensors.  Title to the  Material remains with  Intel Corporation or its
!   suppliers and licensors. The Material contains trade secrets and proprietary
!   and  confidential  information of  Intel or its suppliers and licensors. The
!   Material  is  protected  by  worldwide  copyright  and trade secret laws and
!   treaty  provisions. No part of the Material may be used, copied, reproduced,
!   modified, published, uploaded, posted, transmitted, distributed or disclosed
!   in any way without Intel's prior express written permission.
!   No license  under any  patent, copyright, trade secret or other intellectual
!   property right is granted to or conferred upon you by disclosure or delivery
!   of the Materials,  either expressly, by implication, inducement, estoppel or
!   otherwise.  Any  license  under  such  intellectual property  rights must be
!   express and approved by Intel in writing.
!
!*******************************************************************************
!   Content:
!       MKL DFTI interface example program (Fortran-interface)
!       Forward-Backward 1D complex transform for double precision data
!
!   Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_COMPLEX          (obligatory)
!           DFTI_PRECISION      = DFTI_DOUBLE           (obligatory)
!           DFTI_DIMENSION      = 1                     (obligatory)
!           DFTI_LENGTHS        = n                     (obligatory)
!           DFTI_PLACEMENT      = DFTI_INPLACE          (default)
!           DFTI_FORWARD_SCALE  = 1.0                   (default)
!           DFTI_BACKWARD_SCALE = 1.0/real(n, KIND=8)   (default=1.0)
!
!  Other default configuration parameters are in the mkl_dfti.f90 interface file
!*******************************************************************************
module fft
    contains
      subroutine COMPLEX_1D_DOUBLE_EX1(X_IN,DIR,N)

      Use MKL_DFTI
      include 'mkl_dfti_examples.fi'

      integer    N,DIR

      complex(8) :: X_IN (N)
      complex(8) :: X_EXP(N)

      type(DFTI_DESCRIPTOR), POINTER :: Desc_Handle
      integer   status
      real(8)   Scale

      real(8)    maxerr
      real(8)    eps
      parameter (eps=DOUBLE_EPS)
!      integer :: nw,gaussp,wp,j
!      real(8), allocatable :: w(:)
!      real(8) :: wcm1,wop,wg,wmin,wmax,dw,dt
!      complex(8), allocatable :: pulsew(:)
!
!     Read input n - size of transform from input file complex_1d_double_ex1.d
!
!      nw=1024
!      allocate(w(nw))
!      allocate(pulsew(nw))

!    wmin=1600.d0
!    wmax=3000.d0


!    wcm1=2200.d0
!    wop=wcm1!/219474.63068d0
!    wg=35.d0!*2.d0*3.141592654d0/wop
!    gaussp=10
!    wp=2200.d0

!    j=0
!    dw=(wmax-wmin)/(nw-1)

!    call pulsefreq(pulsew,nw)
!    do n=1,nw
!        w(n)=wmin+(wmax-wmin)*(n-1)/(nw-1)
      !  pulsew(n)=dexp(-2.d0*dabs((w(n) - wp)/wg)**2)
!        if ((cdabs(pulsew(n))**2).lt.1.d-100)then
!            pulsew(n)=(0.d0,0.d0)
!        end if

!        write(5578,'(4E16.8)')w(n),dreal(pulsew(n)),dimag(pulsew(n)),cdabs(pulsew(n))**2
!    end do
 











 


!
!     Create Dfti descriptor for 1D double precision  transform
!

      Status = DftiCreateDescriptor( Desc_Handle, DFTI_DOUBLE, &
      DFTI_COMPLEX, 1, N )

      if (.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then
!          call dfti_example_status_print(Status)
          goto 101
      end if
!     Commit Dfti descriptor
!
!
      Status = DftiCommitDescriptor( Desc_Handle )
      if (.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then
!          call dfti_example_status_print(Status)
          goto 100
      end if


!     Commit Dfti descriptor
!
!
      Status = DftiCommitDescriptor( Desc_Handle )
      if (.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then
!          call dfti_example_status_print(Status)
          goto 100
      end if

!
!     Compute Forward transform
!
    if (DIR.eq.1) then
      !print *
      !print*,'Compute DftiComputeForward'
      Status = DftiComputeForward( Desc_Handle, X_IN)
      if (.not. DftiErrorClass(Status, DFTI_NO_ERROR) ) then
!          call dfti_example_status_print(Status)
          goto 100
      end if

!      if(ADVANCED_DATA_PRINT) then
!        print *
!        print*,'Forward OUTPUT vector X '
!        call PRINT_VECTOR_D(X_IN, n)
!      endif
!        print *
!        print*,'INPUT vector X '
!        call PRINT_VECTOR_Z(X_IN, n)
!      endif

!
    elseif (DIR.eq.-1) then
!   call fourier(pulsew,nw,-1)
!    do n=nw/2+1,nw
!        j=j+1
!        w(n)=wmin+(wmax-wmin)*(n-1)/(nw-1)
        !write(5679,*)w(n),dreal(pulsew(n)),dimag(pulsew(n)),cdabs(pulsew(n))**2
!        write(5679,*)j,cdabs(pulsew(n))**2
!    end do
!    do n=1,nw/2
!        j=j+1
!        w(n)=wmin+(wmax-wmin)*(n-1)/(nw-1)
        !write(5679,*)w(n),dreal(pulsew(n)),dimag(pulsew(n)),cdabs(pulsew(n))**2
!        write(5679,*)j,cdabs(pulsew(n))**2
!    end do


!
!     Set Scale number for Backward transform
!
      Scale = 1.0/real(nw, KIND=8)

      Status = DftiSetValue(Desc_Handle, DFTI_BACKWARD_SCALE, Scale)
      if (.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then
!          call dfti_example_status_print(Status)
          goto 100
      end if

!
!     Commit Dfti descriptor
!
      Status = DftiCommitDescriptor( Desc_Handle )
      if (.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then
!          call dfti_example_status_print(Status)
          goto 100
      end if

!
!     Compute Backward transform
!
      !print *
      !print*,'Compute DftiComputeBackward'
      Status = DftiComputeBackward( Desc_Handle, X_IN)
      if (.not. DftiErrorClass(Status, DFTI_NO_ERROR) ) then
!          call dfti_example_status_print(Status)
          goto 100
      end if

!
!     Check result
!
!    do n=1,nw
!        w(n)=wmin+(wmax-wmin)*(n-1)/(nw-1)
!        pulsew(n)=dexp(-2.d0*dabs((w(n) - wp)/wg)**2)
!        if ((cdabs(pulsew(n))**2).lt.1.d-100)then
!            pulsew(n)=(0.d0,0.d0)
!        end if

!        write(5558,'(4E16.8)')w(n),dreal(pulsew(n)),dimag(pulsew(n)),cdabs(pulsew(n))**2
!    end do
     end if
 100  continue
      Status = DftiFreeDescriptor(Desc_Handle)
      if (.not. DftiErrorClass(Status, DFTI_NO_ERROR) ) then
!          call dfti_example_status_print(Status)
      end if

 903  format(' DFTI_LENGTHS        =', I4)
 904  format(' ACCURACY = ', G15.6)
 101  continue
      !print *

      end subroutine COMPLEX_1D_DOUBLE_EX1
      end module fft
!subroutine pulsefreq(pulsew,nw)
!    use spo
!    integer :: nw,gaussp,wp,j
!    double precision :: w(nw),wcm1,wop,wg,wmin,wmax,dw,dt
!    double complex :: pulsew(nw)
!    wmin=1600.d0
!    wmax=3000.d0
    

!    wcm1=2200.d0
!    wop=wcm1!/219474.63068d0
!    wg=350.d0!*2.d0*3.141592654d0/wop
!    gaussp=10
!    wp=2200.d0
!
!    j=0
!    dw=(wmax-wmin)/(nw-1)
!    do n=1,nw
!        w(n)=wmin+(wmax-wmin)*(n-1)/(nw-1)
!        pulsew(n)=dexp(-2.d0*dabs((w(n) - wp)/wg)**gaussp)
!        write(5678,*)w(n),real(pulsew(n))
!    end do
!    call fourier(pulsew,nw,-1)
!    do n=nw/2+1,nw
!        j=j+1
!        w(n)=wmin+(wmax-wmin)*(n-1)/(nw-1)
        !write(5679,*)w(n),dreal(pulsew(n)),dimag(pulsew(n)),cdabs(pulsew(n))**2
!        write(5679,*)j,cdabs(pulsew(n))**2
!    end do
!    do n=1,nw/2
!        j=j+1
!        w(n)=wmin+(wmax-wmin)*(n-1)/(nw-1)
        !write(5679,*)w(n),dreal(pulsew(n)),dimag(pulsew(n)),cdabs(pulsew(n))**2
!        write(5679,*)j,cdabs(pulsew(n))**2
!    end do
!end subroutine pulsefreq
