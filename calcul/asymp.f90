      module asympto
          contains
      subroutine asympt(t, tf, zpsik1, zpsik2, zfi1, zfi2, &
                       zcutA, npbig, npun, massreduite, &
                       int1tf, int2tf, int3tf, &
                       int1t0, int2t0, int3t0, dr, &
                       zwork, zwork1, zwork2, zwork3, work,dissprob)
                   use spo
                   use simps0n
      integer :: npbig, npun
      complex(8) :: zpsik1(npbig), zpsik2(npbig), zcutA(npbig)
      complex(8) :: zfi1(npun), zfi2(npun), zwork3(npbig)
      real(8) :: int1tf, int2tf, int3tf,dissprob
      real(8) :: int1t0, int2t0, int3t0, dr
      complex(8) :: zwork1(npbig), zwork2(npbig), zwork(npbig)
      real(8) :: rwork1(npbig), rwork2(npbig), rwork(npbig)
      real(8) :: work2(4*4096), xnorm1, xnorm2
      real(8) :: work(4*npbig+15), massreduite
      real(8) :: cte, phase, phase1, phase2, phase3
      real(8) :: dk, A, pi, rk, t, ctet, tf, scale
      real(8) :: norme, norme2
      integer :: iA, ll, l, i, isign
      complex(8)  :: zexpph, zexph1, zexph2, zexph3, ztmp1, ztmp2
      complex(8) :: zsqr2
      character(len=50) :: test
!      write(*,*) "t = ", t ,"int1tf", int1tf 
!      write(*,*) "t = ", t , "int2tf", int2tf 
!      write(*,*) "t = ", t , "int3tf", int3tf 
!      write(*,*) "t = ", t , "int1t0", int1t0 
!      write(*,*) "t = ", t , "int2t0", int2t0 
!      write(*,*) "t = ", t , "int3t0", int3t0 
      pi = 3.141592654d0
      cte = 0.5d0/massreduite

      phase1 = int1tf - int1t0
      phase2 = int1tf*(tf-t) - int2tf + int2t0
      phase3 = (tf-t)*int1tf**2 - 2.d0*int1tf*(int2tf-int2t0) + int3tf -int3t0
      zexph3 = cdexp(dcmplx(0.d0,-cte*phase3))
      dk = 2.d0*pi/(npbig*dr)
      A = phase1/dk
      iA = idnint(A)
!      write(*,*) "test ",phase1,phase2,dk,A

!      write(*,*) t, phase1, phase1/dk, dk,npbig,dr, iA
!      read(*,*) test
!      write(*,*) cdabs(zfi1(npun)), cdabs(zfi2(npun))
      ! 1st, copy zfi1 into zwork1
!      call zaxpy(npun,(0.d0,0.d0),zwork1,1,zwork1,1)
!      call zaxpy(npun,(1.d0,0.d0),zwork1,1,zfi1,1)
!      call zaxpy(npun,(1.d0,0.d0),zwork1,1,zfi2,1)
      ! 2nd copy zfi1 into zwork2
!      call zaxpy(npun,(0.d0,0.d0),zwork2,1,zwork2,1)
!      call zaxpy(npun,(1.d0,0.d0),zwork2,1,zfi1,1)
!      call zaxpy(npun,(-1.d0,0.d0),zwork2,1,zfi2,1)
      call ZVEA(npun,zfi1(1),1,zfi2(1),1,zwork1(1),1)
      call ZVES(npun,zfi1(1),1,zfi2(1),1,zwork2(1),1)
      do i = npun+1, npbig
         zwork1(i) = dcmplx(0.d0,0.d0)
         zwork2(i) = dcmplx(0.d0,0.d0)
      end do

      ! change from ZVEM to mkl zaxpy
      call ZVEM(npbig,zcutA(1),1,zwork1(1),1,zwork1(1),1)
      call ZVEM(npbig,zcutA(1),1,zwork2(1),1,zwork2(1),1)
     ! do i=1,npbig
     !   write(*,*) i,zwork1,zwork2
     ! end  do
     ! read(*,*) test
      !call zaxpy(npbig,(1.d0,0.d0),zwork1,1,zcutA,1)
      !call zaxpy(npbig,(1.d0,0.d0),zwork2,1,zcutA,1)
      
      scale = dr/dsqrt(2.d0*pi)
      isign = -1
      rwork1=cdabs(zwork1)**2
      rwork2=cdabs(zwork2)**2
      call simpson(npbig,dr,rwork1, norme)
      call simpson(npbig,dr,rwork2, norme2)
      !call simpson(npbig,dr,cdabs(zwork1)**2, norme)
      !call simpson(npbig,dr,cdabs(zwork2)**2, norme2)
      dissprob=dissprob+norme+norme2
      write(6562131,1004)t,norme,norme2,dissprob

      call fourier(zwork1,npbig,-1)
      call fourier(zwork2,npbig,-1)
      rwork1=cdabs(zwork1)**2
      rwork2=cdabs(zwork2)**2
      call simpson(npbig,dk,rwork1, norme)
      call simpson(npbig,dk,rwork2, norme2)
      write(6562132,*)t,norme,norme2
!      WRITE(*,*) "*****************************************************************************"
!      WRITE(*,*) "*****************************************************************************"
!      WRITE(*,*) "************************        NORME        ********************************"
!      WRITE(*,*) "*****************************************************************************"
!      WRITE(*,*) "*****************************************************************************"
!      write(*,*) 'norme : ' , norme, "norme2 : " , norme2
      !do i = npbig/2, npbig-1
      !   rk = (i-npbig)*dk
      !   write(6562131,*)rk,abs(zwork1(i))**2
      !   write(6562132,*)rk,abs(zwork2(i))**2
      !end do 
      !do i = 1, npbig/2-1
      !   rk = i*dk
      !   write(6562131,*)rk,abs(zwork1(i))**2
      !   write(6562132,*)rk,abs(zwork2(i))**2
      !end do 
      !do i=1,npbig
        !zwork1(i)=zwork1(i)
        !zwork2(i)=zwork2(i)
      !end do
      !write(*,*) "Press any Key"
      !read(*,*)test
      !call dcfft(zwork1(1), npbig, isign, scale, work(1))
      !call dcfft(zwork2(1), npbig, isign, scale, work(1))
      if(iA.ne.0) then
         call decalk(zwork1(1),zwork(1),iA,npbig)
         call decalk(zwork2(1),zwork1(1),-iA,npbig)
      else
         call ZCOPY(npbig,zwork1(1),1,zwork(1),1)
         call ZCOPY(npbig,zwork2(1),1,zwork1(1),1)
      end if
      do i = 0, npbig/2-1
         rk = i*dk
         phase = -2.d0*cte*rk*phase2
         zexph1 =  cdexp(dcmplx(0.d0,phase))
         zexph2 =  dconjg(zexph1)
         ztmp1 = zexph1*zwork(i+1)
         ztmp2 = zexph2*zwork1(i+1)
         zwork1(i+1) = ztmp2
         zwork(i+1) = ztmp1
      end do
      do i = npbig/2, npbig-1
         rk = (i-npbig)*dk
         phase = -2.d0*cte*rk*phase2
         zexph1 =  cdexp(dcmplx(0.d0,phase))
         zexph2 =  dconjg(zexph1)
         ztmp1 = zexph1*zwork(i+1)
         ztmp2 = zexph2*zwork1(i+1)
         zwork1(i+1) = ztmp2
         zwork(i+1) = ztmp1
      end do
      call ZVES(npbig,zwork(1),1,zwork1(1),1,zwork2(1),1)
      call ZVEA(npbig,zwork(1),1,zwork1(1),1,zwork1(1),1)
      ctet = cte*(t-tf)
      do i = 0, npbig/2-1
         rk = i*dk
         phase = ctet*(rk**2)
         zexpph =  cdexp(dcmplx(0.d0,phase))
         ztmp1 = zexpph*zwork1(i+1)
         ztmp2 = zexpph*zwork2(i+1)
         zwork1(i+1) = ztmp1
         zwork2(i+1) = ztmp2
      end do
      do i = npbig/2, npbig-1
         rk = (i-npbig)*dk
         phase = ctet*(rk**2)
         zexpph =  cdexp(dcmplx(0.d0,phase))
         ztmp1 = zexpph*zwork1(i+1)
         ztmp2 = zexpph*zwork2(i+1)
         zwork1(i+1) = ztmp1
         zwork2(i+1) = ztmp2
      end do
      zsqr2 = 0.5d0 * zexph3
      call ZAXPY(npbig,zsqr2,zwork1(1),1,zpsik1(1),1)
      call ZAXPY(npbig,zsqr2,zwork2(1),1,zpsik2(1),1)
1004  format(4(e16.8e3,2x))
      return
      end subroutine asympt
      subroutine decalk(ztab1,ztab2,id,n)
      integer id, n, idd
      complex*16 ztab1(0:n-1),ztab2(0:n-1)
      if(id.lt.0) then
            idd = -id
            call ZCOPY(n-idd,ztab1(0),1,ztab2(idd),1)
            call ZCOPY(idd,ztab1(n-idd),1,ztab2(0),1)
      else
            call ZCOPY(n-id,ztab1(id),1,ztab2(0),1)
            call ZCOPY(id,ztab1(0),1,ztab2(n-id),1)
      end if
      return
      end subroutine decalk
      end module asympto
