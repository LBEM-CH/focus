       PROGRAM CALCPHA
C
C      Written by Henning Stahlberg on August 29, 2012
C
C************************************************************************
C                                                                       *
C                          SPACE GROUP INFO                             *
C                                                                       *
C   Number      Spacegroup   Asymmetric Unit    Real        Imaginary   *
C                                                                       *
C       1       P1              H>= 0                                   *
C                                                                       *
C       2       P21             H,Z>=0          Z=0                     *
C                                                                       *
C       3       P12             H,K>=0          K=0                     *
C                                                                       *
C       4       P121            H,K>=0          K=0                     *
C                                                                       *
C       5       C12             H,K>=0          K=0                     *
C                                                                       *
C       6       P222            H,K,Z>=0        H=0;K=0;Z=0             *
C                                                                       *
C       7       P2221           H,K,Z>=0        (0,2N,Z)     (0,2N+1,Z) *
C                                               (H,K,0)                 *
C                                               (H,0,Z)                 *
C                                                                       *
C       8       P22121          H,K,Z>=0        (H,K,0)                 *
C                                               (2N,0,Z)     (2N+1,0,Z) *
C                                               (0,2N,Z)     (0,2N+1,Z) *
C                                                                       *
C       9       C222            H,K,Z>=0        (H,K,0)                 *
C                                               (H,0,Z)                 *
C                                               (0,K,Z)                 *
C                                                                       *
C      10       P4              H,K,Z>=0        (H,K,0)                 *
C                                                                       *
C      11       P422            H,K,Z>=0        (H,K,0)                 *
C                               K>=H            (H,0,Z)                 *
C                                               (0,K,Z)                 *
C                                               (H,H,Z)                 *
C                                                                       *
C      12       P4212           H,K,Z>=0        (H,K,0)                 *
C                               K>=H            (H,H,Z)                 *
C                                               (2N,0,Z)     (2N+1,0,Z) *
C                                               (0,2N,Z)     (0,2N+1,Z) *
C                                                                       *
C      13       P3              H,K>=0                                  *
C                                                                       *
C      14       P312            H,K>=0          (H,H,Z)                 *
C                               K>=H                                    *
C                                                                       *
C      15       P321            H,K>=0          (H,0,Z)                 *
C                               K>H             (0,K,Z)                 *
C                                                                       *
C      16       P6              H,K,Z>=0        (H,K,0)                 *
C                                                                       *
C      17       P622            H,K,Z>=0        (H,K,0)                 *
C                               K>=H            (H,H,Z)                 *
C                                                                       *
C************************************************************************
C
      PARAMETER (IMAXREFL=100)
C
      INTEGER*8 ISPOTS
C
      real reala(3),realb(3),realc(3)
      real reciu(3),reciv(3),reciw(3)
C
      pi=3.141592654
C
      write(6,'(''2dx_calc_num_phases program'')')
C
      READ(5,*)IPG
      write(6,'(''Plane group number: '',I3)')IPG
      READ(5,*)RA,RB,RGAMMA
      write(6,'(''Real unit cell A,B: '',2F12.3)')RA,RB
      write(6,'(''Real unit cell GAMMA: '',F12.3)')RGAMMA
      READ(5,*)ALAT
      write(6,'(''Real unit cell C: '',F12.3)')ALAT
      READ(5,*)RANGMAX
      write(6,'(''Maximally included tilt angle: '',F8.3)')RANGMAX
      READ(5,*)RESMAX
      write(6,'(''Radial resolution limit [Angstroems]: '',F8.3)')RESMAX
      READ(5,*)RZRESMAX
      write(6,'(''Vertical resolution limit [reci Angstroems]: '',F8.3)')RZRESMAX
C
C-----Create the non-tilted theoretical real-space lattice
C
C-----We use a real-space lattice with b in Y-axis, and a somewhere right.
C-----This then gives a reciprocal lattice with reciproc. u in X-axis direction.
      reala(1) = RA*sin(RGAMMA*pi/180.0)
      reala(2) = RA*cos(RGAMMA*pi/180.0)
      reala(3) = 0.0
C
      realb(1) = 0.0
      realb(2) = RB
      realb(3) = 0.0
C
      realc(1) = 0.0
      realc(2) = 0.0
      realc(3) = ALAT
C
      write(*,'('' '')')
      write(*,'(''real lattice: '')')
      write(*,'(''A = '',3F12.3)')(reala(i),i=1,3)
      write(*,'(''B = '',3F12.3)')(realb(i),i=1,3)
      write(*,'(''C = '',3F12.3)')(realc(i),i=1,3)
C
C-----transform real-space lattice into reciprocal space
C
      call reciproc(reala,realb,realc,reciu,reciv,reciw)
C
      write(*,'('' '')')
      write(*,'(''reciprocal lattice: '')')
      write(*,'(''U = '',3F12.3)')(reciu(i),i=1,3)
      write(*,'(''V = '',3F12.3)')(reciv(i),i=1,3)
      write(*,'(''W = '',3F12.3)')(reciw(i),i=1,3)
C
      imax=IMAXREFL
      imin=-imax
      ISPOTS=0
C 
      RECRESMAX = 1.0 / RESMAX
      RECZRESMAX = RZRESMAX
C
      do i=0,imax
        do j=imin,imax
          do k=imin,imax
            RRESH2=(i*reciu(1))**2+(i*reciu(2))**2+(j*reciv(1))**2+(j*reciv(2))**2
            RRESH=SQRT(RRESH2)
            RRESV=k*reciw(3)
            RRES=sqrt(RRESH2+(RRESV**2))
            RANG=ATAN2(RRESV,RRESH)*180.0/pi
            if(RRES.le.RECRESMAX .and. 
     .         RRESV.le.RECZRESMAX .and.
     .         RANG.le.RANGMAX ) then
C
              IDO=0
              if ( IPG.eq. 1                                        )IDO=1
              if ( IPG.eq. 2              .and. k.ge.0              )IDO=1
              if ( IPG.eq. 3 .and. j.ge.0                           )IDO=1
              if ( IPG.eq. 4 .and. j.ge.0                           )IDO=1
              if ( IPG.eq. 5 .and. j.ge.0                           )IDO=1
              if ( IPG.eq. 6 .and. j.ge.0 .and. k.ge.0              )IDO=1
              if ( IPG.eq. 7 .and. j.ge.0 .and. k.ge.0              )IDO=1
              if ( IPG.eq. 8 .and. j.ge.0 .and. k.ge.0              )IDO=1
              if ( IPG.eq. 9 .and. j.ge.0 .and. k.ge.0              )IDO=1
              if ( IPG.eq.10 .and. j.ge.0 .and. k.ge.0              )IDO=1
              if ( IPG.eq.11 .and. j.ge.0 .and. k.ge.0 .and. j.ge.i )IDO=1
              if ( IPG.eq.12 .and. j.ge.0 .and. k.ge.0 .and. j.ge.i )IDO=1
              if ( IPG.eq.13 .and. j.ge.0                           )IDO=1
              if ( IPG.eq.14 .and. j.ge.0              .and. j.ge.i )IDO=1
              if ( IPG.eq.15 .and. j.ge.0              .and. j.ge.i )IDO=1
              if ( IPG.eq.16 .and. j.ge.0 .and. k.ge.0              )IDO=1
              if ( IPG.eq.17 .and. j.ge.0 .and. k.ge.0 .and. j.ge.i )IDO=1
C
              if (IDO.eq.1) ISPOTS=ISPOTS+1
C
C              write(*,'(''i,j,k,RRESH2,RRESH,RRESV,RES,RESMAX,RANG,RANGMAX = '',
C     .           3I4,7F12.3)')i,j,k,RRESH2,RRESH,RRESV,RES,RESMAX,RANG,RANGMAX
C           else
C             write(*,'(''no: i,j,k,RRESH2,RRESH,RRESV,RES,RESMAX,RANG,RANGMAX = '',
C     .          3I4,7F12.3)')i,j,k,RRESH2,RRESH,RRESV,RES,RESMAX,RANG,RANGMAX
            endif
          enddo
        enddo
      enddo
C
      write(*,'('' '')')
      write(*,'(''Total number of possible reflections = '',I16)')ISPOTS
C
      call system("\rm -f 2dx_calc_num_phases.out")
      open(UNIT=27,FILE="2dx_calc_num_phases.out",STATUS="NEW",ERR=901)
      write(27,'(I16)')ISPOTS
      close(27)
      goto 902
C
901   continue
      write(*,'('':: ERROR on file open in 2dx_calc_num_phases'')')
902   continue
C
      STOP
      END
C
C===========================================================================
C
      SUBROUTINE reciproc(reala,realb,realc,reciu,reciv,reciw)
C
C-----calculates the reciprocal lattice of a given real-space lattice
C-----assumption: raelc is exactly vertical, i.e., c(1)=c(2)=0
C
C     Real space:
C     A=(a1, a2,0);  B=(b1,b2,0); C=(0,0,c3)
C     
C     Reciprocal space:
C     U*=(b2,-b1) / mod(AxB)
C     V*=(-a2,a1) / mod(AxB)
C     W*=1/c3
C     
C     with AxB = a1*b2 - b1*a2
C     
      DIMENSION reala(3),realb(3),realc(3)
      DIMENSION reciu(3),reciv(3),reciw(3)
C
      rfact = 1.0/abs(reala(1)*realb(2)-reala(2)*realb(1))
      reciu(1) =  realb(2) * rfact
      reciu(2) = -realb(1) * rfact
      reciu(3) = 0.0
      reciv(1) = -reala(2) * rfact
      reciv(2) =  reala(1) * rfact
      reciv(3) = 0.0
      reciw(1) = 0.0
      reciw(2) = 0.0
      reciw(3) = 1.0 / realc(3)
C
      RETURN
      END
C

