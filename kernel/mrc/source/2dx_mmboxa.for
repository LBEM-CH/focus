C  MMBOXA  derived from Judy Smith's NNBOX program but with more statistics
C         suitable for more high resoution crystallographic analysis.
C
C      VX 1.00  pre-1985        RH      from Judy's program NNBOX
C      VX 1.01  23-05-85        JMB     outer radius can be .GT. NXM1
C      VX 1.02  24-11-86        RH      last version before sincfit
C      VX 1.03  07-12-86        RH      sinc func fit over 4x4 box (trial)
C      VX 1.04  21-04-87        RH      sinc func fit over 2X2 box (better)
C      VX 1.05  10-06-87        RH      radius limits now in Angstroms.
C      VX 1.06  09-09-87        RH      more spots, 2000 now PARAMETER.
C      VX 1.07  18-06-92        RH      health warning if origin is given twice
C      VX 2.00  09-01-92        RH      brought to UNIX on Alliant - no changes.
C      VX 2.01  06-10-92        RH      extra dummy output column.
C      VX 2.02  17-04-95        RH      extra precision for scale factor output.
C      VX 2.03  19-07-95        RH      output opened UNKNOWN to allow overwrite
C      VX 2.04  22-04-96        RH      debug IPERIM -> perim
C      VX 3.00  29-11-97        RH      changed background definition -> MMBOXA
C      VX 3.01  29-03-00       JMS      irtorg mod to include zorigin
C      VX 3.02  29-10-01        RH      change filenames to CHARACTER*80
C   !!!  remember to put date and version # in title output record below.
C
C###############################################################################
C      NNBOX prints out amplitudes & phases in N * N boxes from a 
C      Fourier transform.
C
C  The change to MMBOXA on 29.11.97 was made to eliminate the effect that has 
C  been observed by those who have been applying a rather tight box to the 
C  image area (to select the best regions), which is that the number of "good" 
C  spots tends to increase as the area boxed decreases, but in a way which does 
C  not indicate better data (e.g. high IQ spots occur at ridiculously high 
C  resolution).  This was due to a different algorithm being used for peak and 
C  background in the calculation of the IQ value of the spots, such that when 
C  adjacent pixels in the transform are correlated, as occurs with images 
C  containing only a small boxed area, the ratio of peak to background increases
C  slightly giving an appearance of improvement where none actually occurs.  
C  This has been corrected from version 3.00 onwards by changing the background
C  calculation so that it uses the same algorithm as the peak.  The change has 
C  a very small effect on unboxed transforms but effectively eliminates the 
C  spurious spots on heavily boxed images with IQ values of 3 or even 2 where 
C  there is in reality only noise.  An additional fudge-factor of 1.10 has been
C  applied in the subroutine GET_RMSBK to make the signal-to noise ratio of 
C  spots with the same IQ-value identical to that used in earlier versions of
C  MMBOX on full-size unboxed images.
C
C  DATA:
C
C       FILIN
C
C       ISER,TITLE (I10,15A4)
C
C       GU     (A)
C
C       GENGRID(A)
C
C       GENPTS (A)
C
C       IPIXEL, IOUT,NUMSPOT, NOH, NOK, NHOR, NVERT  (*)
C
CHENN>
CCC      FILOUT only if IOUT.NE.0
C       FILOUT only if IOUT.NE.0 .and. IOUT.NE.3
C
C       FILOUT2 only if IOUT.EQ.2 -> Statistics output file
C
C       FILOUT2 only if IOUT.EQ.3 -> Spotlist file
C
C       IQMAX only if IOUT.EQ.3 -> max value of IQ for inclusion in Spotlist
C
CHENN<
C
C       XORIG, YORIG  (*)
C
C       RINNER, ROUTER, IRAD, ACELL, BCELL, WIDTH, ABANG  (*)
C
C  if GENGRID :
C
C       AX, AY, BX, BY (*)
C
C  if .not. GENGRID :
C
C       IH(I), IK(I), X(I), Y(I)  (*)
C
C
C     ISER        serial number for run to be printed & output on IOUT.
C     TITLE       title to be printed & output on unit IOUT.
C     GU          if YES work in grid units, otherwise in mm.
C     GENGRID     if YES generate grid from lattice points (1,0) & (0,1)
C     GENPTS      if YES individual spots requested & generated from grid
C     IPIXEL      pixel size only used if .not.GU 
C     IOUT        output unit number for serial number and title, then
C                  IH,IK,A,P,IQ terminated with IH=100.
C     NUMSPOT     number of spots to be printed, if 0 defaults to 20.
C     NOH, NOK    number of orders of spots in H & K direcions
C     NHOR, NVERT box size in mm or grid units in horizontal &
C                 vertical directions,
C                 i.e. X & Y resp. ( up to 20 grid units in each
C                 direction).
C     XORIG, YORIG X & Y phase origin shifts to be added to those
C                  added to those read in on the transform
C     RINNER, ROUTER inner & outer radius in mm, grid units or Angstroms,
C                 within which spots (centre of box) must fall.
C     IRAD        if IRAD = 0 radii are specified in mm or grid units.
C                    IRAD = 1 radii are in Angstroms using a, b, gamma
C     ACELL
C     BCELL       cell dimensions, thickness (Angstroms)and cell angle gamma.
C     WIDTH             (used to calculate radii when IRAD = 1)
C     ABANG
C     AX,AY,BX,BY coordinates in mm or grid units of 1,0 & 0,1 spots
C                 respectively
C     IH, IK      indices of individual spot
C     X, Y        coordinates of individual spot.
C
C****************************************************************************
C
CHENN>
C      PARAMETER (NMAX=2000)
      PARAMETER (NMAX=21000)
C
C      DIMENSION TITLE(15),NXYZ(3),MXYZ(3),
C     1          XA(NMAX),YA(NMAX),IXC(NMAX),IYC(NMAX),IH(NMAX),IK(NMAX),
C     2          RAMP(21,21),RPHI(21,21),IXGU(21),IYGU(21),PHANG(4),
C     3          WTS(4),ISUM(21,21),ISUMI(21,21),DELX(2),DELY(2),NIQ(9)
C      REAL*8 SUM(21,21)
C
      DIMENSION TITLE(15),NXYZ(3),MXYZ(3),
     1          XA(NMAX),YA(NMAX),IXC(NMAX),IYC(NMAX),IH(NMAX),IK(NMAX),
     2          RAMP(21,21),RPHI(21,21),IXGU(21),IYGU(21),PHANG(4),
     3          WTS(4),ISUM(21,21),ISUMI(21,21),RSUMI(21,21),DELX(2),
     4          DELY(2),NIQ(9)
      REAL*8 SUM(21,21),NEWMAX
      INTEGER*8 ISER
CHENN<
C
      CHARACTER GU,GENGRID,GENPTS,YES
      LOGICAL TURN,FIRST
CHENN>
C      CHARACTER*80 FILIN,FILOUT
      CHARACTER*80 FILIN,FILOUT,FILOUT2,CZEIL,CNAMPAT
CHENN<
      COMMON NOH,NOK,NSPOT,IX1,IX2,IY1,IY2,IHOR,IVERT,IXOUT1,IXOUT2,
     1       IXL,IYL,IX,IY,
     2       IH,IK,RAMP,RPHI,IXGU,IYGU,IXC,IYC,
     3       XA,YA,AX,AY,BX,BY,RINNER,ROUTER,SCALE,SCAMP,
     4       APART,BPART,AMP,PHASE,DELPX,DELPY,
     5       TURN,FIRST,IRAD,ACELL,BCELL,WIDTH,ABANG,ihand
      COMMON//NX,NY,NZ
      EQUIVALENCE (NXYZ,NX)
      DATA YES/'Y'/,DUMMY/0/
      DATA NIQ/9*0/,ISINCBOX/2/
      REAL RA(3),RB(3),RC(3)
C
      WRITE(6,1000)
 1000 FORMAT(//' MMBOXA VX3.02 (29.10.2001) : prints N * N boxes of',
     1       ' amplitudes & phases from spots on transform')
      WRITE(6,*) ' '
      WRITE(6,*) ' # type in input image file name'
      WRITE(6,*) ' '
      READ(5,1005) FILIN
 1005 FORMAT(A)
      CALL  IMOPEN(1,FILIN,'RO')
      CALL  IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      CALL  IRTORG(1,XOR,YOR,ZOR)
      WRITE(6,1010) XOR,YOR
 1010 FORMAT(/' X & Y phase origin shift read from transform',
     1           2F10.2)
      NUMOUT = 0
      NGOOD=0
      NBAD=0
      NPHI = 4
      NY2 = NY / 2
      NY2M1 = NY2 - 1
      NY2M2 = NY2 - 2
      NXP2 = NX * 2
      NXM1 = NX - 1
      NXM2 = NX - 2
      WRITE(6,*) ' '
      WRITE(6,*) ' # type in serial no. and TITLE (I10,15A4)'
      WRITE(6,*) ' '
      READ(5,1020)ISER,TITLE
 1020 FORMAT(I10,15A4)
      WRITE(6,1025) ISER,TITLE
 1025 FORMAT(/' Serial number :',I10/' Title :',15A4)
      WRITE(6,*) ' '
      WRITE(6,*) ' # type in Y for grid units'
      READ(5,1028) GU
      WRITE(6,1029) GU
 1028 FORMAT(A)
1029    FORMAT(1X,20A1)
      WRITE(6,*) ' '
      WRITE(6,*) ' # type in Y for grid generation'
      WRITE(6,*) ' '
      READ(5,1028) GENGRID
      WRITE(6,1029) GENGRID
      WRITE(6,*) ' '
      WRITE(6,*) ' # type in Y for point generation'
      WRITE(6,*) ' '
      READ(5,1028) GENPTS
      WRITE(6,1029) GENPTS
      WRITE(6,*) ' '
      WRITE(6,*) ' # type IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT'
      WRITE(6,*) ' '
      READ(5,*) IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
CHENN>
      WRITE(6,*) IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
CHENN<
C
CHENN>
C      IF(IOUT.NE.0) THEN
      IF(IOUT.NE.0 .and. IOUT.NE.3) THEN
CHENN<
      WRITE(6,*) ' '
      WRITE(6,*) ' # type in output file name'
      WRITE(6,*) ' '
      READ(5,1005) FILOUT
CHENN>
      WRITE(6,'(A)') FILOUT
CHENN<
      OPEN(UNIT=IOUT,FILE=FILOUT,STATUS='UNKNOWN')
      END IF
C
CHENN>
      IOUT2=0
      IF(IOUT.EQ.2) THEN
        IOUT2=11
        WRITE(*,'('' '')')
        WRITE(*,'('' # type in second output file name'')')
        WRITE(*,'('' '')')
        READ(5,1005) FILOUT2
        WRITE(6,'(A)') FILOUT2
        WRITE(*,'('' '')')
        WRITE(*,'('' # type in name pattern'')')
        WRITE(*,'('' '')')
        READ(5,1005) CNAMPAT
        WRITE(6,'(A)') CNAMPAT
        OPEN(UNIT=IOUT2,FILE=FILOUT2,STATUS='NEW')
      END IF
C
      IF(IOUT.EQ.3) THEN
        IOUT2=11
        WRITE(*,'('' '')')
        WRITE(*,'('' # type in spotlist output file name'')')
        WRITE(*,'('' '')')
        READ(5,1005) FILOUT2
        WRITE(6,'(A)') FILOUT2
        OPEN(UNIT=IOUT2,FILE=FILOUT2,STATUS='NEW')
        WRITE(*,'('' '')')
        WRITE(*,'('' # type IQ max value'')')
        WRITE(*,'('' '')')
        READ(5,*) IQMAX
        WRITE(6,*) IQMAX
      END IF
CHENN<
C
      WRITE(6,*) ' '
      WRITE(6,*) ' # type XORIG,YORIG'
      WRITE(6,*) ' '
      READ(5,*) XORIG,YORIG
CHENN>
      WRITE(6,*) XORIG,YORIG
CHENN<
      WRITE(6,*) ' '
      WRITE(6,*) ' # type RINNER,ROUTER,IRAD,ACELL,BCELL,WIDTH,ABANG'
      WRITE(6,*) ' '
      READ(5,*) RINNER,ROUTER,IRAD,ACELL,BCELL,WIDTH,ABANG
CHENN>
      WRITE(6,*) RINNER,ROUTER,IRAD,ACELL,BCELL,WIDTH,ABANG
CHENN<
      WRITE(6,1030) IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT,
     1              XORIG,YORIG,RINNER,ROUTER,
     2              IRAD,ACELL,BCELL,WIDTH,ABANG
1030  FORMAT(/' Pixel size ==================================',I5/
     1        ' output unit number =========================='
     *        ,I5/
     7        ' number of spots printed ====================='
     *        ,I5/
     2        ' No. of orders in h & k=======================',2I5/
     3        ' No. of points in box horiz & vert direction =',
     4        2I5/
     5        ' X & Y phase origin shifts ===================',2F8.1/
     6        ' Inner & outer radii =========================',2F8.1/
     7        ' IRAD ========================================',I5/
     8        ' ACELL, BCELL, WIDTH, ABANG ==================',4F7.2)
      PI = 3.141592654
      TWOPI = 2. * PI
      IF((XOR.NE.0.0.AND.XORIG.NE.0.0).OR.
     . (YOR.NE.0.0.AND.YORIG.NE.0.0)) THEN
        WRITE(6,145)XOR,YOR,XORIG,YORIG
145     FORMAT(//' ***** WARNING - THIS IS OFTEN A CAUSE OF ERROR'/
     . '       ORIGIN SHIFTS HAVE BEEN GIVEN EXPLICITLY AND ',
     . 'IMPLICITLY IN THE IMAGE HEADER'/
     . '       THIS USUALLY HAPPENS WITH AUTOMATIC BOXING ',
     . 'PROGRAMMES ----  BEWARE'/
     . '       XOR,YOR, XORIG,YORIG were',4F8.1)
      ENDIF
      DELPX = -TWOPI * (XOR + XORIG) / (2. * (NXM1))
      DELPY = -TWOPI * (YOR + YORIG) / NY
C
      IF(GU.EQ.YES) THEN
      SCALE = 1.0
      ELSE
      SCALE = 1/(.254 * IPIXEL)
      END IF
C
      IF(IRAD.EQ.0) THEN
        ROUTER = ROUTER * SCALE
        ROUTER = MIN(ROUTER,NXM1*SQRT(2.0))
        RINNER = RINNER * SCALE
      ENDIF
C
C USED TO BE SCAMP = 999./DMAX
      SCAMP = 1.0
      WRITE(6,1040) SCALE, SCAMP
 1040 FORMAT(/' Input coords scaled by',F10.5//
     1 ' Amplitudes scaled by ',F15.5)
      NHOR = NHOR * SCALE + 0.5
      NVERT = NVERT * SCALE + 0.5
      IF(NHOR.GT.21) NHOR = 21
      IF(NVERT.GT.21) NVERT = 21  
      TURN = .FALSE.
      FIRST = .TRUE.
      I = 0
C
      IF(GENGRID.EQ.YES.OR.GENPTS.EQ.YES) THEN
        WRITE(6,*) ' '
        WRITE(6,*) ' # type in AX,AY,BX,BY'
        WRITE(6,*) ' '
        READ(5,*) AX,AY,BX,BY
        WRITE(6,1050) AX,AY,BX,BY
 1050   FORMAT(/' coordinates of 1,0 & 0,1 ',4F10.3)
C
CHEN>
C
        RA(1)=AX
        RB(1)=AY
        RA(2)=BX
        RB(2)=BY
        RA(3)=0.0
        RB(3)=0.0
C
C-------Calculate the Cross-product RC=RAxRB
        RC(1) = RA(2)*RB(3) - RA(3)*RB(2)
        RC(2) = RA(3)*RB(1) - RA(1)*RB(3)
        RC(3) = RA(1)*RB(2) - RA(2)*RB(1)
C
        if(RC(3).ge.0)then
          WRITE(*,'('' Right-handed lattice'')')
          ihand=1
        else
          WRITE(*,'('' Left-handed lattice'')')
          ihand=-1
        endif
C
CHEN<
C
C       point generation required
C 
        IF(GENPTS.EQ.YES) THEN
          CALL SPOTS
        ELSE
C
C       grid generation required
C
          CALL GRID
        END IF
C
C       read in individual points
C
      ELSE
        WRITE(6,1060)
 1060   FORMAT(/' Coordinates read in    H    K       X         Y'
     1        '    grid units:  X      Y'/'0')  
  110   I = I + 1
        IF(NSPOT.GT.NMAX) GO TO 4550
        IF(I.GT.100) GO TO 120
        WRITE(6,*) ' '
        WRITE(6,*) ' # type in IH,IK,XA,YA values'
        WRITE(6,*) ' '
  115   READ(5,*,END=120) IH(I),IK(I),XA(I),YA(I)
        IF(IH(I).EQ.100) GO TO 120
        IXC(I) = XA(I) * SCALE + SIGN(0.5,XA(I))
        IYC(I) = YA(I) * SCALE + SIGN(0.5,YA(I))
C     
C       reject spot if outside boundary
C
        IF(IXC(I).GT.NXM2) GO TO 115
        IF(IXC(I).LT.-NXM2) GO TO 115
        IF(IYC(I).GT.NY2M2) GO TO 115
        IF(IYC(I).LT.-NY2M2) GO TO 115
        WRITE(6,1070) IH(I),IK(I),XA(I),YA(I),IXC(I),IYC(I)
 1070   FORMAT(20X,2I5,2F10.1,12X,I5,2X,I5)
        NSPOT = I
        GO TO 110
      END IF
C
C     data read in proceed
C
  120 IHOR2 = NHOR / 2
      IVERT2 = NVERT / 2
      ISINCBOX2 = ISINCBOX/2    ! See DATA statement above.
      ISINCBOX = 2*ISINCBOX2    ! size of box to do sinc fit.

C
C     make sure odd number of elements in box
C
      NHOR = IHOR2 * 2 + 1
      NVERT = IVERT2 * 2 + 1
      WRITE(6,1080) NHOR,NVERT,ISINCBOX
 1080 FORMAT(/' Box size in transform grid units :',I3,' *',I3/
     . ' Output amplitude is sinc fit over square box size :',I3)
CHENN>
C      IF(IOUT.NE.0) WRITE(IOUT,1081)ISER,TITLE
C1081  FORMAT(I10,15A4)
C
      IF(IOUT.NE.0 .AND. IOUT.NE.3) WRITE(IOUT,1081)ISER,TITLE
1081  FORMAT(I10,15A4)
CHENN<
      DO 130 K=1,21
      DO 130 J=1,21
      SUM(J,K) = 0.
  130 CONTINUE
        SUMRMSBKOLD=0.0
        SUMRMSBKNEW=0.0
C
      DO 500 I=1,NSPOT
      IHOR = NHOR
      IVERT = NVERT
C
C     initialize arrays
C
      DO 150 K=1,21
      DO 150 J=1,21
      RAMP(J,K) = 0.0
      RPHI(J,K) = 0.0
  150 CONTINUE
      IXL = IXC(I) - IHOR2 
      IXR = IXC(I) + IHOR2 
      IYL = IYC(I) - IVERT2 
      IYU = IYC(I) + IVERT2 
C
C     check edge spots
C
  160 IF(IXL.LT.-NXM1) THEN
      IXL = IXL + 1
      IHOR = IHOR - 1
      GO TO 160
      END IF
  170 IF(IXR.GT.NXM1) THEN
      IXR = IXR - 1
      IHOR = IHOR - 1
      GO TO 170
      END IF
  180 IF(IYL.LT.-NY2M1) THEN
      IYL = IYL + 1
      IVERT = IVERT - 1
      GO TO 180
      END IF
  190 IF(IYU.GT.NY2M1) THEN
      IYU = IYU - 1
      IVERT = IVERT - 1
      GO TO 190
      END IF
C
C     set up box edge coordinates
C
C     simple case +ve quadrant
C
      IXOUT1 = 1
      IXOUT2 = IHOR
      IF(IXL.GE.0) THEN
      IX1 = IXL 
      IX2 = IX1 + IHOR - 1
      IY1 = NY2 + IYL 
      IY2 = IY1 + IVERT - 1
      IX = IXL - 1
      IY = IYL - 1
      CALL  RDSECT
      GO TO 280
C
C     simple case -ve quadrant
C
      ELSE IF(IXR.LT.0) THEN
      IX1 = -IXR  
      IX2 = IX1 + IHOR - 1
      IY1 = NY2 - IYU 
      IY2 = IY1 + IVERT - 1
      IX = IXR + 1
      IY = IYU + 1
      TURN = .TRUE.
      CALL  RDSECT
      GO TO 280
      END IF
C
C     complicated cases : spots split about Y axis
C
C     set up LHS of box
C
      IX1 = 1
      IX2 = -IXL 
      IY1 = NY2 - IYU 
      IY2 = IY1 + IVERT - 1
      IXOUT2 = -IXL
      IX = 0
      IY = IYU + 1
      TURN = .TRUE.
      CALL  RDSECT
C
C     set up RHS of box
C
      IXOUT1 = IXOUT2 + 1
      IXOUT2 = IHOR
      IX1 = 0
      IX2 = IXR  
      IY1 = NY2 + IYL 
      IY2 = IY1 + IVERT - 1
      IX = -1
      IY = IYL - 1
      FIRST = .FALSE.
      CALL  RDSECT
c
C     Arrays RAMP, RPHI now filled with correct numbers.
C     Move calculation of  RMS background below sinc calculation
C
  280 CONTINUE
C
      AMPSQ = 0.
      KV = IVERT - 1
      KH = IHOR - 1
      DO 290 K=1,IVERT,KV
      DO 290 J=1,IHOR
      F = RAMP(J,K)
      AMPSQ = AMPSQ + F * F
  290 CONTINUE
      DO 300 K=2,KV
      DO 300 J=1,IHOR,KH
      F = RAMP(J,K)
      AMPSQ = AMPSQ + F * F
  300 CONTINUE
      if(IHOR+IVERT-2.ne.0)then
        AMPTOT = AMPSQ / (2*(IHOR + IVERT - 2))
      else
        AMPTOT = AMPSQ 
      endif
      RMSBK = SQRT(AMPTOT)      ! this was old calculation of RMSBK
      RMSBKOLD=RMSBK            ! see below GET_RMSBK subroutine for new  RMSBK
C
C     Calculate integrated amplitude
C
      J1 = IHOR / 2
      J2 = J1 + 2
      K1 = IVERT / 2
      K2 = K1 + 2
      AMPSQ = 0.
      DO 310 K=K1,K2
      DO 310 J=J1,J2
      F = RAMP(J,K)
      AMPSQ = AMPSQ + F * F
  310 CONTINUE
      AMPTOT = AMPTOT * 9.
      IF(AMPSQ.GE.AMPTOT) THEN
      AMPINT = SQRT(AMPSQ - AMPTOT)
      ELSE
      AMPINT = 0.
      END IF
C
C     Calculate phase from vector sum of phase
C     First find requested centre of output array
C
      XSCALE = XA(I) * SCALE
      INTXA = XSCALE
      IF(XSCALE.LT.0.) INTXA = INTXA - 1
      J1 = INTXA - IXL + 1
      DELX(1) = XSCALE - INTXA
      DELX(2) = 1. - DELX(1)
      YSCALE = YA(I) * SCALE
      INTYA = YSCALE
      IF(YSCALE.LT.0.) INTYA = INTYA - 1
      DELY(1) = YSCALE - INTYA
      DELY(2) = 1. - DELY(1)
      K1 = INTYA - IYL + 1
      J2 = J1 + 1
      K2 = K1 + 1
      ASUM1 = 0.
      BSUM1 = 0.
      ASUM2 = 0.
      BSUM2 = 0.
      DENOM=0.
C
C  Calculate phases from vector sum over 2x2 points only.
C
      DO 320 L2 = 1,2   ! Vector phases over 2x2 points.
        K = K1 + L2 - 1
      DO 320 L1 = 1,2   !
        J = J1 + L1 - 1
        AMP = RAMP(J,K)
        PHASE = RPHI(J,K) / 57.2958
        ASUM1 = ASUM1 + AMP * COS(PHASE)
        BSUM1 = BSUM1 + AMP * SIN(PHASE)
  320 CONTINUE
C
C  Calculated sinc function weighted phase
C
      DO 330 L1=1,ISINCBOX      ! X
        J=J1+L1-ISINCBOX2
      DO 330 L2=1,ISINCBOX      ! Y
        K=K1+L2-ISINCBOX2
        AMP = RAMP(J,K)
        PHASE = RPHI(J,K) / 57.2958
        IF (L1.LE.ISINCBOX2) THEN
                DELTAX = DELX(1)+ISINCBOX2-L1
        ELSE
                DELTAX = DELX(2)+L1-ISINCBOX2-1
        ENDIF
        IF (L2.LE.ISINCBOX2) THEN
                DELTAY = DELY(1)+ISINCBOX2-L2
        ELSE
                DELTAY = DELY(2)+L2-ISINCBOX2-1
        ENDIF
        IF(DELTAX.EQ.0) GO TO 325
        IF(DELTAY.EQ.0) THEN
                SINC = (SIN(PI * DELTAX)) / (PI * DELTAX)
        ELSE
                SINC = (SIN(PI * DELTAX) * SIN(PI * DELTAY)) /
     . (PI**2 * DELTAX * DELTAY)
        END IF
        GO TO 328
C
325     IF(DELTAY.EQ.0) THEN
                SINC = 1.
        ELSE
                SINC = (SIN(PI * DELTAY)) / (PI * DELTAY)
        END IF
C
328     ASUM2 = ASUM2 + AMP * COS(PHASE) * SINC
        BSUM2 = BSUM2 + AMP * SIN(PHASE) * SINC
        DENOM = DENOM + SINC**2
330   CONTINUE
C
C changes to calculate better background using same algorithm as peak
      DX=DELX(1)
      DY=DELY(1)
      CALL GET_RMSBK(RAMP,RPHI,DX,DY,IVERT,IHOR,ISINCBOX,RMSBK)
      SUMRMSBKOLD=SUMRMSBKOLD+RMSBKOLD
      SUMRMSBKNEW=SUMRMSBKNEW+RMSBK
C
C Now some final tidy up of amp and phase calculation.
      IF(ASUM1.NE.0..OR.BSUM1.NE.0.) THEN 
        VECPHA1 = ATAN2(BSUM1,ASUM1) * 57.2958
        ELSE
        VECPHA1 = 0.
      END IF
      IF(VECPHA1.LT.0.) VECPHA1 = VECPHA1 + 360.
C
      IF(ASUM2.NE.0..OR.BSUM2.NE.0.) THEN 
        VECPHA2 = ATAN2(BSUM2,ASUM2) * 57.2958
        if(abs(DENOM).gt.0.0001)then
          AMPSINC = SQRT(ASUM2**2 + BSUM2**2)/DENOM
        else  
          AMPSINC = 0.0
        endif
        IF(AMPSINC.LE.RMSBK) THEN
                AMPOUT = 0.00001
              ELSE
                AMPOUT = SQRT(AMPSINC**2 - RMSBK**2)
        ENDIF
      ELSE
        VECPHA2 = 0.
        AMPSINC = 0.
        AMPOUT = 0.00001
      END IF
      IF(VECPHA2.LT.0.) VECPHA2 = VECPHA2 + 360.
      if(abs(AMPOUT).gt.                        0.0001)then
        PHSOUT=VECPHA2
        PHSERR = (180.0/PI)*RMSBK/AMPOUT
        IQ = 1 + (PHSERR/7.0)           ! THIS MEANS IQ=1 HAS AMP= 8x RMSBK
        IQ = MIN(IQ,8)                  !            IQ=7     AMP= 1x RMSBK
      else
        PHSOUT=VECPHA2
        PHSERR = 360.0
        IQ = 9
      endif
C
C     sum squared amplitudes
C
      DO 350 K=1,IVERT
      DO 350 J=1,IHOR
      SUM(J,K) = SUM(J,K) + RAMP(J,K)**2
  350 CONTINUE
C
C     calculate phase & amplitude @ requested point by linear
C     interpolation
C     calculate weights for interpolated phase angle
C
      PHANG(1) = RPHI(J1,K1)
      PHANG(2) = RPHI(J2,K1)
      PHANG(3) = RPHI(J1,K2)
      PHANG(4) = RPHI(J2,K2)
      WTS(1) = DELX(2) * DELY(2) * RAMP(J1,K1)
      WTS(2) = DELX(1) * DELY(2) * RAMP(J2,K1)
      WTS(3) = DELX(2) * DELY(1) * RAMP(J1,K2)
      WTS(4) = DELX(1) * DELY(1) * RAMP(J2,K2)
C
      CALL  ANGAVE(NPHI,PHANG,WTS,PINTP,GDMEAN) 
C
C     write output to unit IOUT
C
      IF(IOUT.NE.0)
     . WRITE(IOUT,1101)IH(I),IK(I),AMPOUT,PHSOUT,IQ,RMSBK,DUMMY
1101    FORMAT(I12,I12,G15.5,G15.5,I12,G15.5,F14.1)
C 1101    FORMAT(I12,I12,F14.1,F11.1,I12,F14.1,F14.1)
C
CHENN>
CHEN--write out spotlist
C
      IF(IOUT.EQ.3) then
        IF(IQ.LE.IQMAX) then
                WRITE(IOUT2,'(I8,'','',I8)')IH(I),IK(I)
        ENDIF
      ENDIF
CHENN<
C
        IF(IQ.LE.7) NGOOD=NGOOD+1
        IF(IQ.GT.7) NBAD=NBAD+1
        NIQ(IQ) = NIQ(IQ)+1
C
C     set up pagination
C
      NUMOUT = NUMOUT + 1
          IF(NUMOUT.EQ.NUMSPOT+1) WRITE(6,1102)
CHENN>
C 1102    FORMAT(/' OTHER SPOTS NOT PRINTED OUT WITH FULL DIAGNOSTICS'/
C      .           '   H   K  AMPOUT  PHSOUT IQ   RMSBK')
C                 IF(NUMOUT.GT.NUMSPOT)
C      .           WRITE(6,1101)IH(I),IK(I),AMPOUT,PHSOUT,IQ,RMSBK
C       IF(NUMOUT.GT.NUMSPOT) GO TO 500
C
1102      FORMAT(/' OTHER SPOTS NOT PRINTED OUT WITH FULL DIAGNOSTICS',/,
     .    '   H   K   AMPOUT       PHSOUT     IQ   RMSBK',
     .    '         SQRT(H2+K2) IQ')
          IF(NUMOUT.GT.NUMSPOT)THEN
C1103    FORMAT(2I4,2F12.1,I4,1F12.1,1F12.1,'  ',A24)
1103    FORMAT(2I4,2G12.3,I4,G12.3,F12.1,'  ',A24)
           CZEIL(1:24)='8877665544332211'
           ITEMP=19-IQ-IQ
           CZEIL(ITEMP:24) = ' '
           ITEMP=19-IQ-IQ-2
           IF(ITEMP.GE.1)CZEIL(1:ITEMP) = '------------------'
           RRES=SQRT(IH(I)*IH(I)+IK(I)*IK(I)+0.0)
C           WRITE(6,1103)IH(I),IK(I),AMPOUT,PHSOUT,IQ,RMSBK,RRES,CZEIL
          ENDIF
      IF(NUMOUT.GT.NUMSPOT) GO TO 500
CHENN<
C
C     write up to NUMSPOT spots
C
      IF(GU.EQ.YES) THEN
      WRITE(6,1105) IH(I),IK(I),XA(I),YA(I)
 1105 FORMAT(///132('*')///' Reflection  H',I3,'  K',I3,10X,
     1       'Lattice coordinates in grid units ',2F8.2)
C
      ELSE
      WRITE(6,1100) IH(I),IK(I),XA(I),YA(I),XSCALE,YSCALE
 1100 FORMAT(///132('*')///' Reflection  H',I3,'  K',I3,10X,
     1       'Lattice coordinates in mm ',2F8.2,
     2       ' in grid units ',2F8.2)
      END IF
C
      WRITE(6,1110) RMSBK, AMPINT, VECPHA1, VECPHA2, PINTP, GDMEAN
 1110 FORMAT(/' RMS backgd =',F6.1,
     . ' Integrated bgd-corr amp over 3x3 box =',F6.1/
     . '  Ampl-weighted vec. sum of phase =',F6.1, 
     . '  sinc func-weighted vec. sum of phase =',F6.1, 
     . ' Interp. phase/goodness=',2F6.1) 
      WRITE(6,1121) AMPOUT,PHSOUT,IQ
1121  FORMAT(' Amplitude, phase and IQ to be output =',2F8.1,I3)
      IF(IHOR.GT.10) GO TO 400
C  
C     amps & phases side by side
C
      WRITE(6,1120)
 1120 FORMAT(//19X,'Amplitudes',65X,'Phases')
      WRITE(6,1140) (IXGU(J),J=1,IHOR)
 1140 FORMAT(/'  X(grid units)',21I5)
      WRITE(6,1150) (IXGU(J),J=1,IHOR)
 1150 FORMAT('+',67X,'X(grid units)',10I5)
      WRITE(6,1160)
 1160 FORMAT(/'  Y(grid units)',53X,'Y(grid units)')
      L = IVERT
      DO 390 K=1,IVERT
      WRITE(6,1170) IYGU(L),(INT(RAMP(J,L)),J=1,IHOR)
 1170 FORMAT(/6X,I5,4X,21I5)
      WRITE(6,1180) IYGU(L),(INT(RPHI(J,L)),J=1,IHOR)
 1180 FORMAT('+',72X,I5,4X,10I5)
      L = L - 1
  390 CONTINUE
      GO TO 500
C
C     write amps first then phases
C
  400 WRITE(6,1200)
 1200 FORMAT(//19X,'Amplitudes')
      WRITE(6,1140) (IXGU(J),J=1,IHOR)
      WRITE(6,1210)
 1210 FORMAT(/'  Y(grid units)')
      L = IVERT
      DO 420 K=1,IVERT
      WRITE(6,1170) IYGU(L),(INT(RAMP(J,L)),J=1,IHOR)
      L = L - 1
  420 CONTINUE
      WRITE(6,1230)
 1230 FORMAT(/19X,'Phases')
      WRITE(6,1140) (IXGU(J),J=1,IHOR)
      WRITE(6,1210)
      L = IVERT
      DO 440 K=1,IVERT
      WRITE(6,1170) IYGU(L),(INT(RPHI(J,L)),J=1,IHOR)
      L = L - 1
  440 CONTINUE
  500 CONTINUE
      IF(SUMRMSBKOLD.GT.0.) WRITE(6,501) SUMRMSBKNEW/SUMRMSBKOLD
501   FORMAT(/' Ratio (overall average) of new to old way of ',
     . 'calculating the RMS background for denominator of IQ value ',
     . 'calculation was',F10.4/' It varies with inter-pixel ',
     . 'correlation which in turn depends on boxing of image')
C
C     write summed,squared amplitudes
C
      PERIM = 0.
      DO 530 K=1,NVERT
530   PERIM=PERIM+SUM(1,K)+SUM(NHOR,K)
      DO 535 J=2,NHOR-1
535   PERIM=PERIM+SUM(J,1)+SUM(J,NVERT)
      if(abs(NVERT+NHOR-2).gt.0)then
        PERIM=PERIM/(2.0*(NVERT+NHOR-2))
      else
        PERIM=0.0
      endif
C
      DO 520 K=1,NVERT
      DO 520 J=1,NHOR
      ISUMI(J,K)= SUM(J,K)*7.0/PERIM + 0.5
CHENN>
      RSUMI(J,K)= SUM(J,K)*7.0/PERIM + 0.5
CHENN<
      if(NSPOT.gt.0)then
        ISUM(J,K) = SQRT(SUM(J,K)/NSPOT) + 0.5
      else
        ISUM(J,K) = 0
      endif
  520 CONTINUE
C
      WRITE(6,1250)     ! Amplitude output
 1250 FORMAT(//132('*')//19X,'SQRT of summed,squared amplitudes',
     . /19X,33('-')/)
      L = NVERT
      DO 540 K=1,NVERT
      WRITE(6,1270) (ISUM(J,L),J=1,NHOR)
 1270 FORMAT(/15X,21I5)
      L = L - 1
  540 CONTINUE
      WRITE(6,1252)
C
      SCALEFAC = 7.0/PERIM
      WRITE(6,1251) SCALEFAC            ! Intensity output
 1251 FORMAT(//132('*')//19X,'scaled intensities (perimeter',
     . ' averaged to 7.0)',
     . '   scale factor = ',F15.9/19X,40('-'))
C
CHENN>
C
C  Here calculate how close the intensity average approaches theoretical.
C
      ICENTRE=ISUMI(IHOR2+1,IVERT2+1)
      IF(ICENTRE.lt.0)ICENTRE=0
      INEAR=ISUMI(IHOR2+1,IVERT2) + ISUMI(IHOR2+1,IVERT2+2) +
     . ISUMI(IHOR2,IVERT2+1) + ISUMI(IHOR2+2,IVERT2+1)
      IF(INEAR.LE.28) THEN
        IF(ICENTRE.GT.7)PERCENT=100.0
        IF(ICENTRE.LE.7)PERCENT=0.0
      ELSE
        PERCENT=((ICENTRE-7)*100.0)/((INEAR-28)*2.5)
C
CHEN--- same is :
C       PERCENT=((ICENTRE-7)*100.0)/(((INEAR/4.0)-7)*10.0)
CHEN---
C
      ENDIF
C
      ISCAMAX=-9999
      RSCAMAX=-9999.9
CHENN<
C
      L = NVERT
      DO 545 K=1,NVERT
      WRITE(6,1270) (ISUMI(J,L),J=1,NHOR)
CHENN>
         DO 544 ITEMP1=1,NHOR
            IF(ISUMI(ITEMP1,L).GT.ISCAMAX)ISCAMAX=ISUMI(ITEMP1,L)
            IF(RSUMI(ITEMP1,L).GT.RSCAMAX)RSCAMAX=RSUMI(ITEMP1,L)
  544    CONTINUE
CHENN<
      L = L - 1
  545 CONTINUE
      WRITE(6,1252)
1252  FORMAT(//)
      WRITE(6,1253) NUMOUT,NGOOD,NBAD,(J,NIQ(J),J=1,9)
1253  FORMAT(I10,'  Total spots found'/
     . I10,'  Good spots for output'/I10,'  Bad spots not used'/
     . '    IQ    NUMBER',9(/I6,I10),' (negatives)')
CHENN>
      IF(IOUT.EQ.2)THEN
C
        ILSUM=NIQ(1)+NIQ(2)+NIQ(3)+NIQ(4)+NIQ(5)+NIQ(6)+NIQ(7)
C        NEWMAX=(NIQ(1)*7+NIQ(2)*6+NIQ(3)*5+NIQ(4)*4+NIQ(5)*3
C     1  +NIQ(6)*2+NIQ(7))*RSCAMAX/200
        NEWMAX = QVAL(NIQ,RSCAMAX)
        RLMAX=ILSUM+(RSCAMAX/4.0)
CHEN
C        WRITE(IOUT2,1254)RLMAX,(NIQ(J),J=1,9),ILSUM,RSCAMAX
C1254    FORMAT('VAL=',F10.3,3X,'IQ=',9(I4),' GOOD=',I4,
C     1       ' MAX=',F7.1)
C
          write(IOUT2,'(''set QVAL_local = '',F16.1)')NEWMAX
C
          write(IOUT2,'(''set PSMAX = '',I12)')ICENTRE
C
          call shorten(CNAMPAT,k)
          if(k.gt.3)k=3
          do J = 1,9
            write(IOUT2,'(''set '',A,''_IQ'',I1,'' = '',I4)')
     1        CNAMPAT(1:k),J,NIQ(J)
          enddo
C
C        WRITE(IOUT2,1254)NEWMAX,(NIQ(J),J=1,9),ILSUM,RSCAMAX,RLMAX
C1254    FORMAT('VAL=',F10.3,3X,'IQ=',9(I4),' GOOD=',I4,
C     1       ' MAX=',F7.1,' OLDVAL=',F10.3,3X)
C        IF(PERCENT.GE.85.0)WRITE(IOUT2,1260)PERCENT
C        IF(PERCENT.GE.50.0.AND.PERCENT.LT.85.0)THEN
C          WRITE(IOUT2,1261)PERCENT
C        ENDIF
C        IF(PERCENT.LT.50.0)WRITE(IOUT2,1262)PERCENT
C 1260    FORMAT(F6.1,'% perfect peak-shapes. Good !')
C 1261    FORMAT(F6.1,'% perfect peak-shapes. ',
C      1              'Not bad, but could be better.')
C 1262    FORMAT(F6.1,'% perfect peak-shapes. Not good enough.')
1260    FORMAT(F8.3,'% pks')
1261    FORMAT(F8.3,'% pks')
1262    FORMAT(F8.3,'% pks')
        CLOSE(IOUT2)
      ENDIF
C
      IF(IOUT.EQ.3) CLOSE(IOUT2)
C     
CHENN<
C
      STOP
4550    WRITE(6,4551)NMAX
4551    FORMAT(' Too many spots for current prog dimensions',I5)
      END
C
C****************************************************************
C
      SUBROUTINE GRID
C
C     subroutine to generate a lattice from 1,0 & 0,1 coordinates
C
CHENN>
C      PARAMETER (NMAX=2000)
      PARAMETER (NMAX=21000)
CHENN<
      DIMENSION IH(NMAX),IK(NMAX),IXC(NMAX),IYC(NMAX),
     1          IXGU(21),IYGU(21),NXYZ(3),
     2          RAMP(21,21),RPHI(21,21),
     3          XA(NMAX),YA(NMAX)
      LOGICAL TURN,FIRST 
      COMMON NOH,NOK,NSPOT,IX1,IX2,IY1,IY2,IHOR,IVERT,IXOUT1,IXOUT2,
     1       IXL,IYL,IX,IY,
     2       IH,IK,RAMP,RPHI,IXGU,IYGU,IXC,IYC,
     3       XA,YA,AX,AY,BX,BY,RINNER,ROUTER,SCALE,SCAMP,
     4       APART,BPART,AMP,PHASE,DELPX,DELPY,
     5       TURN,FIRST,IRAD,ACELL,BCELL,WIDTH,ABANG,ihand
      COMMON//NX,NY,NZ
      EQUIVALENCE (NXYZ,NX)
        DATA DRAD/0.0174532/
C
      IF(IRAD.EQ.1) WRITE(6,9165) ROUTER,RINNER
9165  FORMAT(/' CALCULATIONS FOR THIS FILM WILL USE SPOTS IN ',
     . 'RESOLUTION RANGE ',F10.3,' TO ',F10.3,'A'/)
CHEN>
      if(ihand.eq.1)then
CHEN<
        if((ACELL*SIN(DRAD*ABANG)).gt.0.0001)then
          ASTAR=1.0/(ACELL*SIN(DRAD*ABANG))
        else
          ASTAR=1.0/0.001
        endif
        if((BCELL*SIN(DRAD*ABANG)).gt.0.0001)then
          BSTAR=1.0/(BCELL*SIN(DRAD*ABANG))
        else
          BSTAR=1.0/0.001
        endif
CHEN>
        write(*,'('' Right-handed lattice'')')
      else
C-------With a left-handed lattice of non-identical vector length, 
C-------the resolution determination needs inverted reciprocal vectors.
        if((BCELL*SIN(DRAD*ABANG)).gt.0.0001)then
          ASTAR=1.0/(BCELL*SIN(DRAD*ABANG))
        else
          ASTAR=1.0/0.001
        endif
        if((ACELL*SIN(DRAD*ABANG)).gt.0.0001)then
          BSTAR=1.0/(ACELL*SIN(DRAD*ABANG))
        else
          BSTAR=1.0/0.001
        endif
        write(*,'('' Left-handed lattice'')')
      endif
C
      write(*,'('' ACELL,BCELL,ABANG = '',3F12.3)')
     .  ACELL,BCELL,ABANG
C
      GAMMA = 180.0 - ABANG
C
      write(*,'('' ASTAR,BSTAR,GAMMA = '',3F15.6)')ASTAR,BSTAR,GAMMA
C
CHEN<
      AX = AX * SCALE
      AY = AY * SCALE
      BX = BX * SCALE
      BY = BY * SCALE
C      WRITE(6,10)
C   10 FORMAT(/' Lattice generated coordinates'/8X,'H',9X,'K',
C     1       7X,'X',9X,'Y'/'0')
      NOHD = 2 * NOH + 1
      NOKD = 2 * NOK + 1
      NSPOT = 0
      DO 100 NH=1,NOHD
        DO 100 NK=1,NOKD
          JH = NH - NOH - 1
          JK = NK - NOK - 1
          X = JH * AX + JK * BX
          Y = JH * AY + JK * BY
C         write(6,'(''Hier1: H,K,X,Y='',2I5,2F12.3)')JH,JK,X,Y
C
          IF(Y.LT.0.) GO TO 100
C
CHEN>
C--------The problem here is that AX,AY are the measured lattice.
C
C--------Selection is made by AX,AY,BX,BY, and also by ASTAR,BSTAR,GAMMA. However, the angle between 
C--------AX,AY and BX,BY may not be GAMMA. This is the case when the lattice is wrongly indexed,
C--------or when the sample is tilted.
C
CHEN<
C
C  Resolution limits here, skip to end if not within limits.
          IF(IRAD.EQ.1) THEN
C-----------Resolution check in Angstroms (using H,K and cell dimensions)
            DSTARSQ=(JH*ASTAR)**2+2*JH*JK*ASTAR*BSTAR*COS(DRAD*GAMMA)
     .        +(JK*BSTAR)**2
            IF(DSTARSQ.NE.0.0) THEN
              DRES=1.0/SQRT(DSTARSQ)
            ELSE
              GO TO 100          ! FOR (0,0) spot
            ENDIF
C           write(6,'(''Hier2: H,K,DRES='',2I5,F12.3)')JH,JK,DRES
            IF(DRES.GT.RINNER.OR.DRES.LT.ROUTER)then
C              write(*,'(''Skipping '',2I4,'', RES='',F12.3,
C     .          '' Lat='',4F9.3,'' AB*='',2F12.4)')
C     .          JH,JK,DRES,AX,AY,BX,BY,ASTAR,BSTAR
              goto 100
C            else
C              write(*,'(''Using    '',2I4,'', RES='',F12.3,
C     .          '' Lat='',4F9.3,'' AB*='',2F12.4)')
C     .          JH,JK,DRES,AX,AY,BX,BY,ASTAR,BSTAR
            endif
          ELSE
C-----------Resolution limits in transform units 
            DSTAR = SQRT(X**2 + Y**2)
C            write(*,'('':: X,Y,DSTAR,ROUTER,RINNER='',5F12.3)')
C     .        X,Y,DSTAR,ROUTER,RINNER
            IF((DSTAR.GT.ROUTER).OR.(DSTAR.LT.RINNER)) GO TO 100
          ENDIF
C
C     spot within radius, may still be outside box, see below.
C
          NSPOT = NSPOT + 1
          IF (NSPOT.GT.NMAX) GO TO 4550
          IXC(NSPOT) = X + SIGN(0.5,X)
          IYC(NSPOT) = Y + SIGN(0.5,Y)
C
C     XA & YA in mm if 1,0 & 0,1 were in mm
C
          if(SCALE.eq.0.0)then
            SCALE=1.0
            write(6,'(''::ERROR: SCALE=0'')')
          endif
          XA(NSPOT) = X / SCALE
          YA(NSPOT) = Y / SCALE
          IH(NSPOT) = JH
          IK(NSPOT) = JK
C         Reject if outside box
          IF(IXC(NSPOT).GT.NX-2) GO TO 115
          IF(IXC(NSPOT).LT.-NX+2) GO TO 115
          IF(IYC(NSPOT).GT.NY/2-2) GO TO 115
          IF(IYC(NSPOT).LT.-NY/2+2) GO TO 115
          GO TO 116
115         NSPOT=NSPOT-1
C           WRITE(6,117)JH,JK,X,Y
117         FORMAT(' SPOT OUTSIDE BOX NOT USED',2I10,2F10.1)
            GO TO 100
116       CONTINUE
C
C          WRITE(6,20) JH,JK,X,Y
C   20     FORMAT(2I10,2F10.1)
  100 CONTINUE
      WRITE(6,4552)NSPOT
4552  FORMAT('  THERE WERE A TOTAL OF',I5,'  SPOTS GENEREATED')
      RETURN
4550    WRITE(6,4551) NMAX
4551    FORMAT(' TOO MANY SPOTS FOR CURRENT PROG DIMENSION',I5)
        STOP
      END
C
C**************************************************************************
C
      SUBROUTINE AMPHA
C
C     subroutine to translate to polar, returns amplitude & phase in 
C     radians 
C
CHENN>
C      PARAMETER (NMAX=2000)
      PARAMETER (NMAX=21000)
CHENN<
      DIMENSION IH(NMAX),IK(NMAX),IXC(NMAX),IYC(NMAX),
     1          IXGU(21),IYGU(21),NXYZ(3),
     2          RAMP(21,21),RPHI(21,21),
     3          XA(NMAX),YA(NMAX)
      LOGICAL TURN,FIRST
      COMMON NOH,NOK,NSPOT,IX1,IX2,IY1,IY2,IHOR,IVERT,IXOUT1,IXOUT2,
     1       IXL,IYL,IX,IY,
     2       IH,IK,RAMP,RPHI,IXGU,IYGU,IXC,IYC,
     3       XA,YA,AX,AY,BX,BY,RINNER,ROUTER,SCALE,SCAMP,
     4       APART,BPART,AMP,PHASE,DELPX,DELPY,
     5       TURN,FIRST,IRAD,ACELL,BCELL,WIDTH,ABANG,ihand
      COMMON//NX,NY,NZ
      EQUIVALENCE(NXYZ,NX)
      PSHIFT = IX * DELPX + IY * DELPY
      IF(IX.LT.0) PSHIFT = - PSHIFT
      C = COS(PSHIFT)
      S = SIN(PSHIFT)
      A = APART * C - BPART * S
      B = APART * S + BPART * C
      IF(IX.LT.0) B = - B
      AMP = SQRT(A * A + B * B)
      IF(AMP.EQ.0.) THEN
      PHASE = 0.
      ELSE 
      PHASE = ATAN2(B,A) * 57.2958
      END IF
      IF(PHASE.LT.0.) PHASE = PHASE + 360.      ! Phase bet 0 and 360 degs.
  999 RETURN
      END
C***************************************************************************
C
      SUBROUTINE SPOTS
C
C     subroutine to generate required spots from 1,0 & 0,1 coordinates
C
CHENN>
C      PARAMETER (NMAX=2000)
      PARAMETER (NMAX=21000)
CHENN<
      DIMENSION IH(NMAX),IK(NMAX),IXC(NMAX),IYC(NMAX),
     1          IXGU(21),IYGU(21),NXYZ(3),
     2          RAMP(21,21),RPHI(21,21),
     3          XA(NMAX),YA(NMAX)
      LOGICAL TURN,FIRST 
      COMMON NOH,NOK,NSPOT,IX1,IX2,IY1,IY2,IHOR,IVERT,IXOUT1,IXOUT2,
     1       IXL,IYL,IX,IY,
     2       IH,IK,RAMP,RPHI,IXGU,IYGU,IXC,IYC,
     3       XA,YA,AX,AY,BX,BY,RINNER,ROUTER,SCALE,SCAMP,
     4       APART,BPART,AMP,PHASE,DELPX,DELPY,
     5       TURN,FIRST,IRAD,ACELL,BCELL,WIDTH,ABANG,ihand
      COMMON//NX,NY,NZ
      EQUIVALENCE (NXYZ,NX)
      AX = AX * SCALE
      AY = AY * SCALE
      BX = BX * SCALE
      BY = BY * SCALE
      WRITE(6,10)
   10 FORMAT(/' Requested spot coordinates'/8X,'H',9X,'K',
     1       7X,'X',9X,'Y'/'0')
      NSPOT = 0
      WRITE(6,*) ' '
      WRITE(6,*) ' # type in JH,JK'
      WRITE(6,*) ' '
      DO 100 NREQ=1,NMAX
        READ(5,*,END=200) JH,JK
        IF(JH.EQ.100) GO TO 200 
      X = JH * AX + JK * BX
      Y = JH * AY + JK * BY
      NSPOT = NSPOT + 1
      IXC(NSPOT) = X + SIGN(0.5,X)
      IYC(NSPOT) = Y + SIGN(0.5,Y)
C
C     XA & YA in mm if 1,0 & 0,1 were in mm
C
      XA(NSPOT) = X / SCALE
      YA(NSPOT) = Y / SCALE
      IH(NSPOT) = JH
      IK(NSPOT) = JK
C         Reject if outside box
      IF(IXC(NSPOT).GT.NX-2) GO TO 115
      IF(IXC(NSPOT).LT.-NX+2) GO TO 115
      IF(IYC(NSPOT).GT.NY/2-2) GO TO 115
      IF(IYC(NSPOT).LT.-NY/2+2) GO TO 115
      GO TO 116
115     NSPOT=NSPOT-1
C       WRITE(6,117)JH,JK,X,Y
117     FORMAT(' SPOT OUTSIDE BOX NOT USED',2I10,2F10.1)
116   CONTINUE
C
      WRITE(6,20) JH,JK,X,Y
   20 FORMAT(2I10,2F10.1)
  100 CONTINUE
      WRITE(6,21)NSPOT
21      FORMAT(' THERE WERE A TOTAL OF',I5,'  REQUESTED SPOTS') 
200     CONTINUE
      RETURN
      END
C
C**************************************************************************
C
      SUBROUTINE RDSECT
C
C     subroutine to read part of section separate amplitude & phase
C     then store in array
C
CHENN>
C      PARAMETER (NMAX=2000)
      PARAMETER (NMAX=21000)
CHENN<
      DIMENSION IH(NMAX),IK(NMAX),IXC(NMAX),IYC(NMAX),
     1          IXGU(21),IYGU(21),NXYZ(3),
     2          RAMP(21,21),RPHI(21,21),
     3          XA(NMAX),YA(NMAX),
     4          ARRAY(42,21)
      LOGICAL TURN,FIRST
      COMMON NOH,NOK,NSPOT,IX1,IX2,IY1,IY2,IHOR,IVERT,IXOUT1,IXOUT2,
     1       IXL,IYL,IX,IY,
     2       IH,IK,RAMP,RPHI,IXGU,IYGU,IXC,IYC,
     3       XA,YA,AX,AY,BX,BY,RINNER,ROUTER,SCALE,SCAMP,
     4       APART,BPART,AMP,PHASE,DELPX,DELPY,
     5       TURN,FIRST,IRAD,ACELL,BCELL,WIDTH,ABANG,ihand
      COMMON//NX,NY,NZ
      EQUIVALENCE(NXYZ,NX)
      CALL  IRDPAS(1,ARRAY,42,21,IX1,IX2,IY1,IY2,*900)
      CALL  IMPOSN(1,0,0)
      KX = IX
      IF(.NOT.TURN) THEN
C
C     straightforward case, no turning
C
      DO 100 K=1,IVERT
      L = 1
      IY = IY + 1
      IX = KX
      DO 100 J=IXOUT1,IXOUT2
      APART = ARRAY(L,K)
      BPART = ARRAY(L+1,K)
      IX = IX + 1
      CALL  AMPHA
      RAMP(J,K) = AMP * SCAMP + 0.5
      RPHI(J,K) = PHASE + 0.5
      L = L + 2
  100 CONTINUE
      ELSE
C
C     turn upside down & back to front
C
      KK = IVERT + 1
      DO 200 K=1,IVERT
      KK = KK - 1
      JJ = IXOUT2 + 1
      L = 1
      IY = IY - 1
      IX = KX
      DO 200 J=IXOUT1,IXOUT2
      JJ = JJ - 1
      APART = ARRAY(L,K)
      BPART = ARRAY(L+1,K)
      IX = IX - 1
      CALL  AMPHA
      RAMP(JJ,KK) = AMP * SCAMP + 0.5
      RPHI(JJ,KK) = PHASE + 0.5
      L = L + 2
  200 CONTINUE
      END IF
C
C     set up array for X coordinates
C
      IF(FIRST) THEN
      KXL = IXL
      DO 300 J=1,IHOR
      IXGU(J) = KXL
      KXL = KXL + 1
  300 CONTINUE
      KYL = IYL
      DO 400 J=1,IVERT
      IYGU(J) = KYL
      KYL = KYL + 1
  400 CONTINUE
      END IF
      FIRST = .TRUE.
      TURN = .FALSE.
      RETURN
C
C     diagnostics
C
  900 WRITE(6,10)
   10 FORMAT(/' error on reading transform')
      STOP
      END
C
C***************************************************************************
C
        SUBROUTINE ANGAVE(N, THETAS, WEIGHTS, THMEAN, THGOOD)
C       Function: to average a set of angles in degrees
C       Created: 27/7/84 by D.J.Thomas
C       Modified:  by
        REAL            COMEAN          !mean value of cosines
        INTEGER         I               !loop counter
        INTEGER*4       N               !number of input angles
        REAL*4          THETAS(1)       !array of input angles
        REAL*4          THGOOD          !goodness of average (0 to 1)
        REAL*4          THMEAN          !weighted average of input angles
        REAL            SIMEAN          !mean value of sines
        REAL            WEIGHT          !total input weight
        REAL*4          WEIGHTS(1)      !weights on input angles
C
        IF (N .LE. 0) GO TO 20
        WEIGHT = 0.0
        COMEAN = 0.0
        SIMEAN = 0.0
        DO 10 I=1,N
        WEIGHT = WEIGHT + WEIGHTS(I)
        COMEAN = COMEAN + (COS(THETAS(I)*0.01745329252)*WEIGHTS(I))
        SIMEAN = SIMEAN + (SIN(THETAS(I)*0.01745329252)*WEIGHTS(I))
10      CONTINUE
        IF ((SIMEAN .EQ. 0.0) .AND. (COMEAN .EQ. 0.0)) GO TO 20
        THMEAN = 57.295779513*ATAN2(SIMEAN,COMEAN)
        IF(THMEAN.LT.0.0) THMEAN=THMEAN+360.0
        IF (WEIGHT .EQ. 0.0) GO TO 20
        THGOOD = SQRT((SIMEAN*SIMEAN) + (COMEAN*COMEAN))/WEIGHT
        RETURN
20      THGOOD = 0.0                    !average is undefined
        RETURN
        END
C
C***************************************************************************
C
      SUBROUTINE GET_RMSBK(RAMP,RPHI,DX,DY,IVERT,IHOR,ISINCBOX,RMSBK)
      DIMENSION RAMP(21,21),RPHI(21,21)
C this calculation to do same sinc function treatment of background as is used
C for the peak in AMPOUT in main program
      PI = 3.141592654
      IF(ISINCBOX.NE.2) THEN
        WRITE(6,18)ISINCBOX
18      FORMAT(/' WARNING !!!!!!!!!!!!!!!!!!!!!!!!!!!, ISINCBOX not 2',I5/
     . ' background calculation algorithm based a value on 2'/)
        IS=2
      ELSE
        IS=ISINCBOX
      ENDIF
C
      NBK=0
      SUMBKSQ=0.0
C first do sums along left and right edges of box
      DO 30 NV=1,IVERT-1
      DO 30 ILEFT=1,2
        IH = 1 + (ILEFT-1)*(IHOR-2)
        IV = NV
        ASUM = 0.
        BSUM = 0.
        DENOM = 0.
        DO 20 I=1,IS
        DO 20 J=1,IS
                JBOX=IH+I-1
                KBOX=IV+J-1
                DXC=DX-I+1
                DYC=DY-J+1
                IF(DXC.EQ.0.0.AND.DYC.EQ.0.0) THEN
                        SINC = 1.
                ELSEIF(DYC.EQ.0.0) THEN
                        SINC = (SIN(PI * DXC)) / (PI * DXC)
                ELSEIF(DXC.EQ.0.0) THEN
                        SINC = (SIN(PI * DYC)) / (PI * DYC)
                ELSE
                        SINC = (SIN(PI * DXC) * SIN(PI * DYC)) /
     . (PI**2 * DXC * DYC)
                ENDIF
                AMP   = RAMP(JBOX,KBOX)
                PHASE = RPHI(JBOX,KBOX) / 57.2958
                ASUM = ASUM + AMP * COS(PHASE) * SINC
                BSUM = BSUM + AMP * SIN(PHASE) * SINC
                DENOM = DENOM + SINC**2
20      CONTINUE
        if(abs(DENOM).gt.0.0001)then
          AMPSINC = SQRT(ASUM**2 + BSUM**2)/DENOM
        else
          AMPSINC = 0.0
        endif
        NBK=NBK+1
        SUMBKSQ=SUMBKSQ+AMPSINC**2
30    CONTINUE
C second do sums along bottom and top edges of box
      DO 50 NH=2,IHOR-2
      DO 50 IBOTT=1,2
        IH = NH
        IV = 1 + (IBOTT-1)*(IVERT-2)
        ASUM = 0.
        BSUM = 0.
        DENOM = 0.
        DO 40 I=1,IS
        DO 40 J=1,IS
                JBOX=IH+I-1
                KBOX=IV+J-1
                DXC=DX-I+1
                DYC=DY-J+1
                IF(DXC.EQ.0.0.AND.DYC.EQ.0.0) THEN
                        SINC = 1.
                ELSEIF(DYC.EQ.0.0) THEN
                        SINC = (SIN(PI * DXC)) / (PI * DXC)
                ELSEIF(DXC.EQ.0.0) THEN
                        SINC = (SIN(PI * DYC)) / (PI * DYC)
                ELSE
                        SINC = (SIN(PI * DXC) * SIN(PI * DYC)) /
     .                         (PI**2 * DXC * DYC)
                ENDIF
                AMP   = RAMP(JBOX,KBOX)
                PHASE = RPHI(JBOX,KBOX) / 57.2958
                ASUM = ASUM + AMP * COS(PHASE) * SINC
                BSUM = BSUM + AMP * SIN(PHASE) * SINC
                DENOM = DENOM + SINC**2
40      CONTINUE
        if(abs(DENOM).gt.0.0001)then
          AMPSINC = SQRT(ASUM**2 + BSUM**2)/DENOM
        else
          AMPSINC = 0.0
        endif
        NBK=NBK+1
        SUMBKSQ=SUMBKSQ+AMPSINC**2
50    CONTINUE
C third calculate the overall rms background
      IF(NBK.NE.(2*(IVERT-1)+2*(IHOR-3))) STOP ' Error in GET_RMSBK'
      if(NBK.ne.0)then
        RMSBK=SQRT(SUMBKSQ/NBK)
        RMSBK=RMSBK/1.10          ! fudge factor to restore earlier scale of A/B
      else
        RMSBK=999999.9
      endif
      RETURN
      END
C
c==========================================================
c
      SUBROUTINE shorten(czeile,k)
C
C counts the number of actual characters not ' ' in czeile
C and gives the result out in k.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1
      CHARACTER * 1 CTMP2
      CTMP2=' '
C
      ilen=len(czeile)
      DO 100 I=1,ilen
         k=ilen+1-I
         READ(CZEILE(k:k),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)GOTO 300
  100 CONTINUE
  300 CONTINUE
      IF(k.LT.1)k=1
C
      RETURN
      END


