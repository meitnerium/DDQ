program ddq
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Calcul de propagations de paquets d'ondes par méthode de Split-Opérateur
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    implicit none

    !### Used for the initial write line ##############################
    character*8 :: DATE
    character*10 :: TIME
    character*32 :: hostname
    integer(4) :: istat,hostnm

    !##################################################################
    !### logical variable for file ####################################
    logical :: file_exists

    !##################################################################
    !### timing variables #############################################
    real(8) :: btime, time2

    !##################################################################
    !### input variables ##############################################
    real(8) :: t0,E0,w,dt,xmin,xmax,mass
    real(8) :: phase ! will be multiplied by pi
    integer :: pulsetype,nc,npos,nchannel,ncfin,v
    real(8),  parameter :: PI_8  = 4 * atan (1.0_8)

    !##################################################################
    !### derivated value from input variables #########################
    real(8) :: tf
    integer :: nt

    !##################################################################
    !### allocatable vector for the proper states #####################
    real(8),allocatable :: ep(:,:)

    !##################################################################
    !### Used for read format of proper states ########################
    CHARACTER(2) :: FMT

    !##################################################################
    !### Vector for time grid #########################################
    real(8),allocatable :: t(:), champ(:)

    !##################################################################
    !### Vector for wavefunction ######################################
    real(8),allocatable :: chiin(:,:),xmu12(:),pot(:,:),work(:)
    complex(8),allocatable ::  chi(:,:),zetdt(:)

    !##################################################################
    !### Vector for internuclear distance #############################
    real(8),allocatable :: x(:)

    !##################################################################
    !### Variable for pbound calculation ##############################
    real(8) :: lieprob,lieprobv,projreal,projimag
    real(8), allocatable :: proj(:), proji(:)

    !##################################################################
    !### Integer variable used for loop ###############################
    integer :: n,i,j,l

    !real(8) :: CDABS,normedeb
    real(8) :: normedeb




    real(8) :: r0cut, scut
    !npos pour la grille de position et d'impulsion, ntps pour la grille de temps
    !real(8) :: w1(4*npos), w2(4*npos)
    real(8),allocatable :: w1(:), w2(:)
    character(LEN=50) :: fichier1,fichier2,fichier3,fichier4,fichier5
    !real(8) :: wspos(4*npos+15),wsbig(16*npos+15)
    real(8),allocatable :: wspos(:),wsbig(:)
    real(8) :: timeper,dtper,pulsetemp,champ1,champ2
    !complex(8) :: zwork1(4*npos), zwork2(4*npos), zwork3(4*npos), zwork4(4*npos)
    complex(8),allocatable :: zwork1(:), zwork2(:), zwork3(:), zwork4(:)
    real(8) :: int1tf, int2tf, int3tf
    real(8) ::  int1t0, int2t0, int3t0
    integer :: m,maxpot1,minpot2,ideb,ninput,npbfin
    parameter (ideb=85)
    !complex(8) :: zcutA(4*npos),zcutI(4*npos)
    complex(8),allocatable :: zcutA(:),zcutI(:)
    real(8) :: evbyau,spk
    !real(8) :: work2(4*npos),work3(4*npos),work4(4*npos), work5(4*npos)
    real(8),allocatable :: work2(:),work3(:),work4(:), work5(:)
    real(8) :: requ, diss, E0wattcm2,dk,cte,xk
    real(8) :: rc0
    !real(8) :: work1(npos),table1(npos), champ(nt)
    real(8),allocatable :: work1(:),table1(:)
    !real(8),allocatable :: tablea(npos),worka(npos),workb(npos)
    real(8),allocatable :: tablea(:),worka(:),workb(:)
    complex(8) :: cun,cim,cnul
    !complex(8) :: chi1(npos),chi2(npos),zetdt(npos),ctemp(npos),chilie(npos),chi1init(npos)
    complex(8),allocatable :: ctemp(:),chilie(:),chi1init(:)
    !complex(8) :: psik1(npos*4), psik2(npos*4)
    complex(8),allocatable :: psik1(:), psik2(:)

    real(8) :: alpha,p0,rdeb,xmue
    !real(8) :: proj(npos) ,proji(npos),auto_correl(nt)
    real(8),allocatable :: auto_correl(:)
    real(8) :: delt,omega,  norme,norme1,norme2,periode,delta,sigma,tmax,f0
    !real(8) :: t(nt),vp1(npos),vp2(npos),kmoyen(nt),rmoyen(nt),rmoyenlie(nt),rclapet1(nt),rclapet2(nt)
    real(8),allocatable :: vp1(:),vp2(:),kmoyen(:),rmoyenlie(:),rclapet1(:),rclapet2(:)
    real(8) :: dissprob,rmoyen,dispers
    real(8) :: periodir,dw
    !real(8) :: pulset(nt),champir1(nt)
    real(8),allocatable :: pulset(:),champir1(:)
    !real(8),dimension(npos-ideb) :: vp1reel,vp2reel
    real(8),allocatable :: vp1reel(:),vp2reel(:)
    real(8) :: time1,time3,ftime,beta
    real(8) xnorm1, xnorm2, xnormk1, xnormk2
    character(LEN=120) :: pbname
    character(LEN=50) :: nomfichier
    character(LEN=50) :: charnum(10)
    character(LEN=5) :: test
    character(LEN=2500) :: string
    integer :: k

    namelist /iofile/ t0, pulsetype,E0,phase,w,dt,nc,ncfin,npos,xmax,&
    nchannel,v,w,xmin,mass


    !##################################################################
    !### Time at the start ############################################
    call cpu_time ( btime )


    ISTAT = HOSTNM(hostname)
    CALL DATE_AND_TIME ( DATE , TIME )

    write(*,*) "DDQ Program start at ",DATE," ",TIME," on ",hostname
    write(*,*) "Reading the input"


    INQUIRE(FILE="input", EXIST=file_exists)
    if (file_exists) then
        open(5,file="input",status='old')
    end if
    read(5,iofile)
    if (file_exists) then
        close(5)
    end if





    !namelist /iofile/ t0, pulsetype,E0,phase,w,dt,nc,npos,xmax
    !WRITE(test,'(I5.5)') iE0
    !write(*,*) "case_"//ADJUSTL(test)//"/input"
    !open(5,name="case_"//ADJUSTL(test)//"/input",status="old")
    !read(5,iofile)
    phase=phase*PI_8

    !open(6,name="input",status="old")
    !namelist /iofile/ npos,xmax,nE0
    !read(6,iofile)

    xmin=2.d-3

    v=19
    !TODO : change number of optical cycle (nc) in the input
    delt=dt
    tf=6.d0*(2.d0*PI_8/w)
    nt=int((tf-t0)/dt)
    allocate(t(nt))
    do i=1,nt
        t(i)=t0+(i-1)*dt
    end do
    !,t2,t3,clock_rate, clock_max
    !***********************************************************************
    !         Valeurs des paramètres, allocation des variables
    !***********************************************************************
    ! call system_clock ( t1, clock_rate, clock_max )
    call cpu_time ( btime )
    write(*,*) "Begining of the program ", btime


    write(*,*) "Initial time (t0) = ",t0
    write(*,*) "Phase = ",phase,"*Pi"
    phase=phase*PI_8
    write(*,*) "E0 = ",E0
    write(*,*) "Frequency = ",w
    write(*,*) ncfin," optical cycles after t=0"
    tf=ncfin*(2.d0*PI_8/w)
    write(*,*) "tf = ",tf
    nt=int((tf-t0)/dt)
    write(*,*) "nt = ",nt,PI_8

    !##################################################################
    !### Reading the v proper states ##################################
    open(10,file='properstate.dat')
    open(11,file='pot.dat')
    open(12,file='init.dat')
    allocate(ep(v,npos),chiin(nchannel,npos),chi(nchannel,npos),xmu12(npos))
    allocate(x(npos),pot(nchannel,npos),work(npos),zetdt(npos))
    allocate(proj(npos), proji(npos))
    WRITE(FMT,'(I2)') v+1
    do i=1,npos
        READ(10,"(" // ADJUSTL(FMT) // "(E28.20E3,4x))") x(i),(ep(n,i),n=1,v)
        READ(12,"(3(E28.20E3),4x)") x(i),chiin(1,i),chiin(2,i)
        READ(11,"(4(E28.20E3),4x)") x(i),pot(1,i),pot(2,i),xmu12(i)
    enddo
    close(10)
    close(11)
    close(12)

    cun = dcmplx(1.0d0,0.d0)
    cim = dcmplx(0.d0,1.0d0)
    cnul = dcmplx(0.d0,0.d0)
    !t0=0d0

    dissprob=0.d0
    r0cut = 34.026d0
    scut = 4.72d-1
    evbyau = 27.212d0

    ! Output File list
    ! 101 : pbound and field

    open(101,file='pbound.dat')





    !allocate(x(npos),xmu12(npos))
    !allocate(chi1in(npos), chi2in(npos))
    !allocate(pot(2,npos))
    allocate(w1(4*npos), w2(4*npos))
    allocate(wspos(4*npos+15),wsbig(16*npos+15))
    allocate(zwork1(4*npos), zwork2(4*npos), zwork3(4*npos), zwork4(4*npos))
    allocate(zcutA(4*npos),zcutI(4*npos))
    allocate(work2(4*npos),work3(4*npos),work4(4*npos), work5(4*npos))
    allocate(work1(npos),table1(npos), champ(nt))
    allocate(tablea(npos),worka(npos),workb(npos))
    allocate(ctemp(npos),chilie(npos),chi1init(npos))
    allocate(psik1(npos*4), psik2(npos*4))
    allocate(auto_correl(nt))
    allocate(vp1(npos),vp2(npos),kmoyen(nt),rmoyenlie(nt),rclapet1(nt),rclapet2(nt))

    allocate(pulset(nt),champir1(nt))
    allocate(vp1reel(npos-ideb),vp2reel(npos-ideb))


    rc0 = 1.3989d0 !position du paquet d'onde à t0
    p0 = 0.0d0 !impulsion du paquet d'onde à t0
    alpha = 13.019d0 !paramètre pour chi1 et chi2

    requ=2.d0 !valeur de r à l'equilibre = 2*a0 (a0=1 en u.a)
    diss=2.7925d0/27.2d0 !potentiel de dissociation de H2+
    write(*,*) xmax,xmin,npos


    call cpu_time ( btime )
    write(*,*) "Begining of time loop : ", time1
    do j=1,npos
        chi1init(j)=chi(1,j)
        write(123556,*) x(j), chi1init(j)
    enddo



    !***********************************************************************
    !           Normalisation de la fonction d'onde initiale
    !***********************************************************************
    do l=1,npos
        worka(l)=(abs(chiin(1,l)))**2
    enddo
    call simpson( npos,x(2)-x(1),worka, normedeb)
    write(*,*) "NORME De Depart : ", normedeb
    do l = 1, npos
        chi(1,l) = chiin(1,l)/dsqrt(normedeb)
        chi(2,l) = cmplx(chiin(2,l),0.d0)
    !********************************************************************
    enddo




    !E0=wattcm22au(E0)
    !E0wattcm2=au2wattcm2(E0)
    !write(logfile, *) " iE0 = ",  iE0
    !write(*,'(" E0 = ", E16.8 , " u.a. , ",  E16.8 , "  W/cm2" )') E0 , E0wattcm2
    !write(logfile,*) "phase = ", phase/MATH_PI , " Pi"
    !write(logfile,'(" tc = ", E16.8 , " a.u. ," , E16.8, " fs" )') tc , tc*tau2fs
    !write(logfile,'(" te = " , E16.8 , " a.u. ," , E16.8, " fs" )') te , te*tau2fs
    !write(logfile,*) "tf = ", tf, "a.u." , tf*tau2fs ," fs"
    !write(logfile,*) "nt = ", nt
    !write(logfile,*) "dt = ", dt, "a.u." , dt*tau2fs ," fs"
    !write(logfile,*) "wir in travail = " , wir , " TL = " ,  2.d0*MATH_PI/(wir*TVIB) ," TVIB"

    call calczcut(zcutA, zcutI, x(1), x(2)-x(1), 4*npos, r0cut, scut)

    !********************************************************************
    !   Application du split operator-Ouverture de la boucle temps
    !********************************************************************

    do i=1,nt
        !********************************************************************
        if (mod(i,100).eq.0) then
            write(*,*) i, "/" , nt , "t = ",t(i)
        end if
        call calc_champ(champ(i),w,t(i),phase,nc,PI_8,E0)


        do j=1,npos
            worka(j)=x(j)*(abs(chi(1,j)))**2 !position moyenne du paquet d'onde de l'état fondamental
            workb(j)=(x(j)**2*(abs(chi(1,j)))**2) !position moyenne du paquet d'onde de l'état fondamental
            tablea(j)=(abs(chi(1,j)))**2 !densité de probabilité du paquet d'onde de l'état fondamental
        enddo
        call simpson(npos,x(2)-x(1),worka,norme1)
        call simpson(npos,x(2)-x(1),workb,norme2)
        call simpson(npos,x(2)-x(1),tablea,norme)
        rmoyen=norme1/norme
        dispers=norme2/norme-rmoyen**2



        !write(logfile,*) "Rmoyen du iE0 : ", iE0 , " : fort." , 70000+iE0

        call cpu_time ( time2 )
        do j=1,npos
            xmue=xmu12(j)*champ(i)
            delta=(pot(2,j)-pot(1,j))**2+(2d0*xmue)**2
            delta=dsqrt(delta)
            vp1(j)=(pot(2,j)+pot(1,j)-delta)*0.5d0
            vp2(j)=(pot(2,j)+pot(1,j)+delta)*0.5d0
                  !write(5433634,*) x(j), pot(1,j), pot(2,j), vp1(j), vp2(j)
            if (j.gt.ideb) then
                vp1reel(j-ideb)=vp1(j)
                vp2reel(j-ideb)=vp2(j)
            endif
        enddo
        minpot2=minloc(vp2,1)
        rclapet2(i)=x(minpot2)

        !********************************************************************
        call splitop(chi(1,:), chi(2,:), zetdt,pot,xmu12, npos, champ(i), x(2)-x(1), mass, delt)
        ! ajouter le calcul de int[1-2-3]t[0-f] ! FAIT
        !
        call asympt(t(i), tf, psik1, psik2, chi(1,:), chi(2,:), &
        zcutA, 4*npos, npos, mass,&
        int1tf, int2tf, int3tf,&
        int1t0, int2t0, int3t0, x(2)-x(1),&
        zwork1, zwork2, zwork3,&
        zwork4, wsbig,dissprob)
        !!!!!!!!!!!!!!
        !!!!!!!! Verifier le calcul du cut
        !!!!!!!!!!!
        !********************************************************************
        call ZVEM(npos,chi(1,1),1,zcutI(1),1,chi(1,1),1)
        call ZVEM(npos,chi(2,1),1,zcutI(1),1,chi(2,1),1)
        !********************************************************************
        !       Calcul de probabilité de dissociation
        !********************************************************************
        lieprob = 0.d0
        !   pause 10
        !write(123453245,*) "t = ",t(i)
        do n = 1, v
            do j = 1, npos
                proj(j) = ep(n,j)*dreal(chi(1,j))
                proji(j) = ep(n,j)*dimag(chi(1,j))


            end do
            call simpson(npos,x(2)-x(1),proj,projreal)
            call simpson(npos,x(2)-x(1),proji,projimag)
            lieprobv=(projreal**2 + projimag**2)
            lieprob = lieprob + lieprobv
        !       if (i.eq.1) then
        !           write(49,*)n,lieprobv
        !       endif
        end do
        write(101,'( 3F18.10,4X )') t(i), lieprob,champ(i)

     ! end of the loop on time
    enddo




    call asympt(tf, tf, psik1, psik2, chi(1,:), chi(2,:),&
    zcutA, 4*npos, npos, mass,&
    int1tf, int2tf, int3tf,&
    int1t0, int2t0, int3t0, x(2)-x(1),&
    zwork1, zwork2, zwork3,&
    zwork4, wsbig,dissprob)
    call ZVEM(npos,chi(1,:),1,zcutI,1,chi(1,:),1)
    call ZVEM(npos,chi(2,:),1,zcutI,1,chi(2,:),1)
    call simpson(npos, x(2)-x(1), w1, xnorm1)
    call simpson(npos, x(2)-x(1), w2, xnorm2)
    write(*,*)
    ! write(*,*) 'Niveau vibrationnel de depart'
    ! write(*,*) nu
    write(*,*)
    write(*,*) 'NORME de depart'
    write(*,*) normedeb
    write(*,*)
    write(*,*) 'NORMES population restante'
    write(*,*) '    SIGMA g           SIGMA u           TOTAL'
    write(*,1003) xnorm1, xnorm2, xnorm1 + xnorm2
    call simpson(4*npos, dk, work3, xnormk1)
    call simpson(4*npos, dk, work4, xnormk2)
    write(*,*)
    write(*,*) 'NORMES population dissociee'
    write(*,*) '    SIGMA g           SIGMA u           TOTAL'
    write(*,1003) xnormk1, xnormk2, xnormk1 + xnormk2
    write(*,*)
    write(*,*)
    write(*,*) '1 - NORMES population restante:', 1 - xnorm1 - xnorm2
    write(*,*)
    write(*,*) 'Erreur sur normes:',           1 - xnormk1 - xnormk2 - xnorm1 - xnorm2




    call cpu_time ( ftime )
    write(*,*) "End of the program ", ftime
    if ( ftime-btime > 60*60 )  then
        write(string,*) (ftime-btime)/60*60 , "heures," ,  ((ftime-btime)-&
        (ftime-btime)/60*60)/60 , " min et ",    ftime-btime-(((ftime-btime)-&
        (ftime-btime)/60*60)/60) , " sec "
    else if ( ftime-btime > 60)  then
        write(string,*)  (ftime-btime)/60 , " min et ",    ftime-btime-&
        ((ftime-btime)/60) , " sec "
    else
        write(string,*) ftime-btime , " sec "
    end if


1002 format(2(e16.8e3,2x))
1003 format(3(e16.8e3,2x))
1004 format(4(e16.8e3,2x))
1005 format(5(e16.8e3,2x))
1006 format(6(e16.8e3,2x))
1007 format(7(e16.8e3,2x))
end program ddq

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
!function micron2au(l)
!    real(8) :: micron2au,l
!    l=l*1.d-6/5.291772108d-11
!    micron2au=2.d0*MATH_PI*MATH_C/l
!end function micron2au





