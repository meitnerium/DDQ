      module fact
          contains
      function factrl(n)
      
          real(8) factrl
          integer n,j
      double precision a(100)
      data ntop,a(1)/0,1./
      if(n.lt.0) then
         pause 'negative factorial'
      else if(n.le.ntop) then
         factrl=a(n+1)
      else if(n.le.100) then
         do 11 j=ntop+1,n
            a(j+1)=j*a(j)
   11    continue
         ntop=n
         factrl=a(n+1)
      else
         factrl = dsqrt(6.283185307d0*n)*(n*dexp(-1.d0))**n
      endif
      return
      end function factrl


end module fact
 


module gen
          contains

!function m2au(w)
    !  http://en.wikipedia.org/wiki/Atomic_units
    ! 1 au = 5.2917720859(36)×10−11 m
!    include 'mkl_dfti_examples.fi'
!    real(8) , INTENT(IN) :: w
!    real(8) :: m2au,temp
!    write(*,*) "w in m2au : ", w
    !m2au=w*1.d9/(1239.84191d0*13.6056923d0)
!    au2m=5.2917720859d-11
!    m2au=1/(127.d0*w)
    !m2au=1/temp
!    write(*,*) "w in a.u. : ", m2au
!end function m2au


function cm12au(w)
    !  http://en.wikipedia.org/wiki/Atomic_units
    ! 1 au = 5.2917720859(36)×10−11 m
    ! 8 065.544 45(69) cm-1 
    include 'mkl_dfti_examples.fi'
    real(8), INTENT(IN) :: w
    real(8)  :: cm12au
    write(*,*) "w in function ", w 
    !cm12au=w*5.2917720859d-9
    cm12au=w/(8065.54445d0*13.6056923d0*2.d0)
    !(27.212*8065)
    write(*,*) "cm12au in function ",  cm12au
end function cm12au
function au2cm1(w)
    !  http://en.wikipedia.org/wiki/Atomic_units
    ! 1 au = 5.2917720859(36)×10−11 m
    ! 8 065.544 45(69) cm-1 
    include 'mkl_dfti_examples.fi'
    real(8), INTENT(IN) :: w
    real(8)  :: au2cm1
    write(*,*) "w in function ", w 
    !cm12au=w*5.2917720859d-9
    au2cm1=w*(8065.54445d0*13.6056923d0*2.d0)
    !(27.212*8065)
    write(*,*) "au2cm1 in function ",  au2cm1
end function au2cm1

function wattcm22au(E0)
    real(8) wattcm22au,E0
    wattcm22au=dsqrt(E0/3.5094475d16)
    ! see http://onlinelibrary.wiley.com/store/10.1002/3527605606.app9/asset/app9.pdf?v=1&t=h84kttop&s=c3af7779f76bc990859e2987dd68709c22341b54
    !write(*,*) "Intens in au", wattcm22au
end function wattcm22au

function au2wattcm2(intens)
    real(8) au2wattcm2,intens
    write(*,*) "Intens in wattcm22au", intens
    au2wattcm2=(intens**2)*3.5094475d16
    ! see http://onlinelibrary.wiley.com/store/10.1002/3527605606.app9/asset/app9.pdf?v=1&t=h84kttop&s=c3af7779f76bc990859e2987dd68709c22341b54
end function au2wattcm2

!function tau2fs(t)
!    real(8) tau2fs,t
!    tau2fs=t*2.418884326505E-2
!end function tau2fs
function micron2au(l)
    real(8) :: micron2au,l
    include 'mkl_dfti_examples.fi'
    l=l*1.d-6/5.291772108d-11
    micron2au=2.d0*MATH_PI*MATH_C/l
end function micron2au


function laguerrel(a,n,x)
          use fact
          double precision laguerrel
      integer a,n,j
      double precision x,lagtmp
      lagtmp=0.d0 
      do 22 j=0,n
      lagtmp=lagtmp+(1.d0/factrl(j))*((-x)**j)*factrl(n+a)/(factrl(n-j)*factrl(j+a))
   22 continue
      laguerrel=lagtmp
      return
end function laguerrel
end module gen


module morse1
    contains
        function morse(diss,smalla,xmu,requ,r,nu)
!   
    use gen
    use fact
    real(8) :: morse
    real(8) :: diss,smalla,xmu
    real(8) :: biga,bigc,enu,alpha
    integer :: nu,m
    real(8) :: x,arg,r,requ
    real(8) :: norm
         if(r.lt.64.d0)then
         biga = (dsqrt(2.d0*xmu))/smalla 
	 bigc = biga*dsqrt(diss) 
         enu = -((bigc-nu-.5d0)**2)/biga**2 
         alpha = bigc-nu-0.5d0   
	 arg=dexp(-smalla*(r-requ)) 
         x=2.d0*bigc*arg 
         m = 2*(idint(bigc)-nu)-1 
	 morse = laguerrel(m,nu,x)
	 morse = morse * (x**(idint(bigc)))
         morse = morse / (x**nu)
         morse = morse / dsqrt(x)
         morse = morse * dexp(-x/2.d0)
	
         norm = smalla * factrl(nu) * (2.d0*bigc - 2.d0*nu - 1.d0)
         norm = norm / factrl(m+nu)
         norm = dsqrt(norm)
         morse = morse*norm
      else
         morse = 0.d0
      endif
      return
      end function morse
      end module morse1

!*******************************************************************
!
!       Calcul du paquet d'onde initial
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
         cval = cpoi*cdexp(arg)
         cw1(l) = cval
         cw2(l) = cnul

      end do

      return
      end

!***************************************************************
!               Calcul norme d'une fonction complexe
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

