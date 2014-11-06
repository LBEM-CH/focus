C   MMLATREF : Refinement of lattice parameters for maximum amplitude.
C    First created from MMBOX -- 16.12.86  RH
C    Last modified            -- 24.12.86  RH
C    Last modified            -- 21.04.01  RH, change to IRTORG(1,XOR,YOR,ZOR)
C
C    Incorporates several aspects in the refinement
C       1. Maximises sum of sinc-fitted intensities.
C       2. Uses weighted coefficients -- 1/(rmsbk**2+ampsinc**2)
C               this should maximise the number of spots with IQ>=7
C       3. Matrix diagonal has constant added to it to stabilise refinement when
C               transform is very strong and clean --- this may mean that shifts
C               greater than 1.0 will be needed if the data is very noisy.
C                                                 (eg. at high resolution)
C       4. Summary table printed out at end to judge progress.
C       5. May be mainly good for getting just a little extra after UNBENDING.
C       6. Present sinc fit is just over 2x2 box around spot centre ... this is
C               thought to be insignificantly different from any bigger box.
C
C      NNBOX prints out amplitudes & phases in N * N boxes from a 
C      Fourier transform
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
C       FILOUT only if IOUT.NE.0
C
C       RINNER, ROUTER, XORIG, YORIG  (*)
C
C       NCYCLES, FSHIFT, ILIST (*)
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
C     RINNER, ROUTER inner & outer radius in mm or grid units within
C                    which spots (centre of box) must fall
C     XORIG, YORIG X & Y phase origin shifts to be added to those
C                  added to those read in on the transform
C     NCYCLES     number cycles of lattice parameter refinement to do.
C     FSHIFT      fractional shifts to be applied in lat. param. refn.
C     ILIST       if 0, abbreviated listing, if 1 full listing printed.
C     AX,AY,BX,BY coordinates in mm or grid units of 1,0 & 0,1 spots
C                 respectively
C     IH, IK      indices of individual spot
C     X, Y        coordinates of individual spot.
C
C****************************************************************************
C
      PROGRAM MMLATREF
      PARAMETER (MAXCYC=200)
      DIMENSION TITLE(15),NXYZ(3),MXYZ(3)
      DIMENSION XA(2000),YA(2000),IXC(2000),IYC(2000),IH(2000),IK(2000)
      DIMENSION IAMP(21,21),IPHI(21,21),IXGU(21),IYGU(21),PHANG(4)
      DIMENSION WTS(4),ISUM(21,21),ISUMI(21,21),DELX(2),DELY(2),NIQ(9)
      DIMENSION GRAD(4),BST(4,MAXCYC),SUMASQST(MAXCYC),NIQST(9,MAXCYC)
      DIMENSION RLATTICE(4,MAXCYC)
      REAL*8 A(4,4),B(4),E,W(80)
      INTEGER*8 IQMAX, IQVAL
      CHARACTER GU,GENGRID,GENPTS,YES
      LOGICAL TURN,FIRST
      CHARACTER*80 FILIN,FILOUT
      COMMON NOH,NOK,NSPOT,IX1,IX2,IY1,IY2,IHOR,IVERT,IXOUT1,IXOUT2,
     1       IXL,IYL,IX,IY,
     2       IH,IK,IAMP,IPHI,IXGU,IYGU,IXC,IYC,
     3       XA,YA,AX,AY,BX,BY,RINNER,ROUTER,SCALE,SCAMP,
     4       APART,BPART,AMP,PHASE,DELPX,DELPY,
     5       TURN,FIRST
      COMMON//NX,NY,NZ
      EQUIVALENCE (NXYZ,NX)
      DATA NMAX/2000/,YES/'Y'/
      DATA NIQ/9*0/,SUMASQST/MAXCYC*0.0/,ISINCBOX/2/
      INTEGER * 8 ISER
C
      WRITE(6,1000)
 1000 FORMAT('0',' MMLATREF : prints M * M boxes of amps & phases',
     . ' from spots on transform',/,'  and maximises average intensity',
     . ' by refinement of lattice params')
      WRITE(6,*)' '
      WRITE(6,*)' # type in input image file name'
      WRITE(6,*)' '
      READ(5,1005) FILIN
 1005 FORMAT(A)
      CALL  IMOPEN(1,FILIN,'RO')
      CALL  IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      CALL  IRTORG(1,XOR,YOR,ZOR)
      WRITE(6,1010) XOR,YOR
 1010 FORMAT(/,' X & Y phase origin shift read from transform',
     1           2F10.2)
      NPHI = 4
      NY2 = NY / 2
      NY2M1 = NY2 - 1
      NY2M2 = NY2 - 2
      NXP2 = NX * 2
      NXM1 = NX - 1
      NXM2 = NX - 2
      WRITE(6,*)' '
      WRITE(6,*)' # type in serial no. and TITLE (I10,15A4)'
      WRITE(6,*)' '
      READ(5,1020)ISER,TITLE
 1020 FORMAT(I10,15A4)
      WRITE(6,1025) ISER,TITLE
 1025 FORMAT(/,' Serial number :',I10,/,' Title :',15A4)
      WRITE(6,*)' '
      WRITE(6,*)' # type in Y for grid units'
      READ(5,1028) GU
      WRITE(6,1029) GU
 1028 FORMAT(A)
1029    FORMAT(1X,20A1)
      WRITE(6,*)' '
      WRITE(6,*)' # type in Y for grid generation'
      WRITE(6,*)' '
      READ(5,1028) GENGRID
      WRITE(6,1029) GENGRID
      WRITE(6,*)' '
      WRITE(6,*)' # type in Y for point generation'
      WRITE(6,*)' '
      READ(5,1028) GENPTS
      WRITE(6,1029) GENPTS
      WRITE(6,*)' '
      WRITE(6,*)' # type IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT'
      WRITE(6,*)' '
      READ(5,*) IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
C
      IF(IOUT.NE.0) THEN
      WRITE(6,*)' '
      WRITE(6,*)' # type in output file name'
      WRITE(6,*)' '
      READ(5,1005) FILOUT
      OPEN(UNIT=IOUT,FILE=FILOUT,STATUS='NEW')
      END IF
C
      WRITE(6,*)' '
      WRITE(6,*)' # type RINNER,ROUTER,XORIG,YORIG'
      WRITE(6,*)' '
      READ(5,*) RINNER,ROUTER,XORIG,YORIG
      READ(5,*) NCYCLES, FSHIFT, ILIST
      IF(NCYCLES.GT.MAXCYC) NCYCLES=MAXCYC
      WRITE(6,1030) IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT,
     1              RINNER,ROUTER,XORIG,YORIG,NCYCLES,FSHIFT,ILIST
1030  FORMAT(/,' Pixel size ==================================',I5,/,
     1        ' output unit number =========================='
     *        ,I5,/,
     7        ' number of spots printed ====================='
     *        ,I5,/,
     2        ' No. of orders in h & k=======================',2I5,/,
     3        ' No. of points in box horiz & vert direction =',
     4        2I5,/,
     5        ' Inner & outer radii =========================',2F8.1,/,
     6        ' X & Y phase origin shifts ===================',2F8.1,/,
     7        ' No. cycles lattice parameter refinement =====',I5,/,
     8        ' Fractional shift applied ====================',F8.3,/,
     9        ' ILIST (controls extent of printout) =========',I5)
      PI = 3.141592654
      TWOPI = 2. * PI
      DELPX = -TWOPI * (XOR + XORIG) / (2. * (NXM1))
      DELPY = -TWOPI * (YOR + YORIG) / NY
C
      IF(GU.EQ.YES) THEN
      SCALE = 1.0
      ELSE
      SCALE = 1/(.254 * IPIXEL)
      END IF
C
      ROUTER = ROUTER * SCALE
      RINNER = RINNER * SCALE
C
C     square inner & outer radii to improve efficiency in GRID
C
      ROUTER = MIN(ROUTER,NXM1*SQRT(2.0))
      ROUTER = ROUTER * ROUTER
      RINNER = RINNER * RINNER
C USED TO BE SCAMP = 999./DMAX
      SCAMP = 1.0
      WRITE(6,1040) SCALE, SCAMP
 1040 FORMAT(/,' Input coords scaled by',F10.5,/,/,
     1 ' Amplitudes scaled by ',F15.5)
      NHOR = NHOR * SCALE + 0.5
      NVERT = NVERT * SCALE + 0.5
      IF(NHOR.GT.21) NHOR = 21
      IF(NVERT.GT.21) NVERT = 21  
c!!! following statement removed as variable never used and fails
c!!! sun compiler since degree not declared logical
c      DEGREE = .TRUE.
      TURN = .FALSE.
      FIRST = .TRUE.
      I = 0
C
      IF(GENGRID.EQ.YES.OR.GENPTS.EQ.YES) THEN
      IPASS=0
      WRITE(6,*)' '
      WRITE(6,*)' # type in AX,AY,BX,BY'
      WRITE(6,*)' '
      READ(5,*) AX,AY,BX,BY
      WRITE(6,1050) AX,AY,BX,BY
 1050 FORMAT(/,' coordinates of 1,0 & 0,1 ',4F10.3)
      ENDIF
C
C   Outermost loop refinement cycle beginning
C
      DO 9000 ICYCL=1,NCYCLES
C
C     point generation required
C 
      IF(GENGRID.EQ.YES.OR.GENPTS.EQ.YES) THEN
      IF(GENPTS.EQ.YES) THEN
      CALL  SPOTS(IPASS)
      ELSE
C
C     grid generation required
C
      CALL  GRID(ILIST)
      END IF
C
C     read in individual points
C
      ELSE
      WRITE(6,1060)
 1060 FORMAT(/,' Coordinates read in    H    K       X         Y'
     1        '    grid units:  X      Y',/,'0')  
  110 I = I + 1
      IF(NSPOT.GT.NMAX) GO TO 4550
      IF(I.GT.100) GO TO 120
      WRITE(6,*)' '
      WRITE(6,*)' # type in IH,IK,XA,YA values'
      WRITE(6,*)' '
  115 READ(5,*,END=120) IH(I),IK(I),XA(I),YA(I)
      IF(IH(I).EQ.100) GO TO 120
      IXC(I) = XA(I) * SCALE + SIGN(0.5,XA(I))
      IYC(I) = YA(I) * SCALE + SIGN(0.5,YA(I))
C     
C     reject spot if outside boundary
C
      IF(IXC(I).GT.NXM2) GO TO 115
      IF(IXC(I).LT.-NXM2) GO TO 115
      IF(IYC(I).GT.NY2M2) GO TO 115
      IF(IYC(I).LT.-NY2M2) GO TO 115
      IF(ILIST.EQ.1)WRITE(6,1070) IH(I),IK(I),XA(I),YA(I),IXC(I),IYC(I)
 1070 FORMAT(20X,2I5,2F10.1,12X,I5,2X,I5)
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
                NUMOUT = 0
                NGOOD=0
                NBAD=0
                DO 124 N=1,9
124             NIQ(N)=0
                DO 125 IM=1,4
                B(IM)=0.0
                DO 125 JM=1,4
                A(IM,JM)=0.0
125             CONTINUE
C
C     make sure odd number of elements in box
C
      NHOR = IHOR2 * 2 + 1
      NVERT = IVERT2 * 2 + 1
      WRITE(6,1080) NHOR,NVERT,ISINCBOX
 1080 FORMAT(/,' Box size in transform grid units :',I3,' *',I3,/,
     .  ' Output amplitude is sinc fit over square box size :',I3)
      IF(IOUT.NE.0) WRITE(IOUT,1081)ISER,TITLE
1081  FORMAT(I10,15A4)
      DO 130 K=1,21
      DO 130 J=1,21
      ISUM(J,K) = 0
      ISUMI(J,K) = 0
  130 CONTINUE
      DO 500 I=1,NSPOT
      IHOR = NHOR
      IVERT = NVERT
C
C     initialize arrays
C
      DO 150 K=1,21
      DO 150 J=1,21
      IAMP(J,K) = 0
      IPHI(J,K) = 0
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
C     Arrays IAMP, IPHI now filled with correct numbers.
C     Now calculate RMS background
C
  280 AMPSQ = 0.
      KV = IVERT - 1
      KH = IHOR - 1
      DO 290 K=1,IVERT,KV
      DO 290 J=1,IHOR
      F = IAMP(J,K)
      AMPSQ = AMPSQ + F * F
  290 CONTINUE
      DO 300 K=2,KV
      DO 300 J=1,IHOR,KH
      F = IAMP(J,K)
      AMPSQ = AMPSQ + F * F
  300 CONTINUE
      AMPTOT = AMPSQ / (2*(IHOR + IVERT - 2))
      RMSBK = SQRT(AMPTOT)
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
      F = IAMP(J,K)
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
      DELX(2) = DELX(1) - 1.    ! note this gives negative values for DELX(2)
      YSCALE = YA(I) * SCALE
      INTYA = YSCALE
      IF(YSCALE.LT.0.) INTYA = INTYA - 1
      DELY(1) = YSCALE - INTYA
      DELY(2) = DELY(1) - 1.    ! note this gives negative values for DELY(2)
C                               ! which invalidates the ANGAVE function
      K1 = INTYA - IYL + 1
      J2 = J1 + 1
      K2 = K1 + 1
      ASUM1 = 0.
      BSUM1 = 0.
      ASUM2 = 0.
      BSUM2 = 0.
      DENOM=0.
      DADX=0.
      DADY=0.
      DBDX=0.
      DBDY=0.
C
C  Calculate phases from vector sum over 2x2 points only.
C
      DO 320 L2 = 1,2   ! Vector phases over 2x2 points.
        K = K1 + L2 - 1
      DO 320 L1 = 1,2   !
        J = J1 + L1 - 1
        AMP = IAMP(J,K)
        PHASE = IPHI(J,K) / 57.2958
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
        AMP = IAMP(J,K)
        PHASE = IPHI(J,K) / 57.2958
                DELTAX = DELX(1)+ISINCBOX2-L1
                DELTAY = DELY(1)+ISINCBOX2-L2
        IF(DELTAX.EQ.0) GO TO 325
        IF(DELTAY.EQ.0) THEN
           SINC   = (SIN(PI * DELTAX)) / (PI * DELTAX)
           GSINCX = COS(PI*DELTAX)/DELTAX-SIN(PI*DELTAX)/(PI*DELTAX**2)
           GSINCY = 0.0
        ELSE
           SINC   = (SIN(PI * DELTAX) * SIN(PI * DELTAY)) /
     .                          (PI**2 * DELTAX * DELTAY)
           GSINCX = (SIN(PI*DELTAY)/(PI*DELTAY)) * 
     .          (COS(PI*DELTAX)/DELTAX-SIN(PI*DELTAX)/(PI*DELTAX**2))
           GSINCY = (SIN(PI*DELTAX)/(PI*DELTAX)) * 
     .          (COS(PI*DELTAY)/DELTAY-SIN(PI*DELTAY)/(PI*DELTAY**2))
        END IF
        GO TO 328
C
325     IF(DELTAY.EQ.0) THEN
           SINC   = 1.
           GSINCX = 0.
           GSINCY = 0.
        ELSE
           SINC   = (SIN(PI * DELTAY)) / (PI * DELTAY)
           GSINCX = 0.
           GSINCY = COS(PI*DELTAY)/DELTAY-SIN(PI*DELTAY)/(PI*DELTAY**2)
        END IF
C
328     ASUM2 = ASUM2 + AMP * COS(PHASE) * SINC
        BSUM2 = BSUM2 + AMP * SIN(PHASE) * SINC
        DENOM = DENOM + SINC**2
        DADX  = DADX  + AMP * COS(PHASE) * GSINCX
        DADY  = DADY  + AMP * COS(PHASE) * GSINCY
        DBDX  = DBDX  + AMP * SIN(PHASE) * GSINCX
        DBDY  = DBDY  + AMP * SIN(PHASE) * GSINCY
330   CONTINUE
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
        AMPSINC = SQRT(ASUM2**2 + BSUM2**2)/DENOM
                SUMASQST(ICYCL) = SUMASQST(ICYCL) + AMPSINC**2
                DAMPDX=(ASUM2*DADX+BSUM2*DBDX)/AMPSINC
                DAMPDY=(ASUM2*DADY+BSUM2*DBDY)/AMPSINC
                GRAD(1)=IH(I)*DAMPDX
                GRAD(2)=IH(I)*DAMPDY
                GRAD(3)=IK(I)*DAMPDX
                GRAD(4)=IK(I)*DAMPDY
                WGT=1.0/(RMSBK**2+AMPSINC**2)   ! EQUAL WGT ABOVE IQ=7
                DO 340 IM=1,4
                  B(IM)=B(IM)+AMPSINC*GRAD(IM)*WGT
                DO 340 JM=1,4
                  A(IM,JM)=A(IM,JM)+GRAD(IM)*GRAD(JM)*WGT
340             CONTINUE
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
      PHSOUT=VECPHA2
        PHSERR = (180.0/PI)*RMSBK/AMPOUT
        IQ = 1 + (PHSERR/7.0)           ! THIS MEANS IQ=1 HAS AMP= 8x RMSBK
        IQ = MIN(IQ,8)                  !            IQ=7     AMP= 1x RMSBK
        IF (AMPOUT.EQ.0.00001) IQ=9     ! THUS IQ=8 HAS AMPSINC .GE. RMSBK
C
C     sum squared amplitudes
C
      DO 350 K=1,IVERT
      DO 350 J=1,IHOR
      ISUM(J,K) = ISUM(J,K) + IAMP(J,K) * IAMP(J,K)
  350 CONTINUE
C
C     calculate phase & amplitude @ requested point by linear
C     interpolation
C     calculate weights for interpolated phase angle
C
      PHANG(1) = IPHI(J1,K1)
      PHANG(2) = IPHI(J2,K1)
      PHANG(3) = IPHI(J1,K2)
      PHANG(4) = IPHI(J2,K2)
      WTS(1) = DELX(2) * DELY(2) * IAMP(J1,K1)
      WTS(2) = DELX(1) * DELY(2) * IAMP(J2,K1)
      WTS(3) = DELX(2) * DELY(1) * IAMP(J1,K2)
      WTS(4) = DELX(1) * DELY(1) * IAMP(J2,K2)
C
C      Note that in these refinement programs WTS can be negative which can
C      invalidate the ANGAVE function, but it is only cosmetic.
      CALL  ANGAVE(NPHI,PHANG,WTS,PINTP,GDMEAN) 
C
C     write output to unit IOUT
C
      IF(IOUT.NE.0)
     .   WRITE(IOUT,1101)IH(I),IK(I),AMPOUT,PHSOUT,IQ,RMSBK
1101    FORMAT(2I4,2F8.1,I3,F8.1)
        IF(IQ.LE.7) NGOOD=NGOOD+1
        IF(IQ.GT.7) NBAD=NBAD+1
        NIQ(IQ) = NIQ(IQ)+1
C
C     set up pagination
C
      NUMOUT = NUMOUT + 1
          IF(NUMOUT.EQ.NUMSPOT+1.AND.ILIST.EQ.1) WRITE(6,1102)
1102      FORMAT(/,' OTHER SPOTS NOT PRINTED OUT WITH FULL DIAGNOSTICS',/,
     .     '   H   K  AMPOUT  PHSOUT IQ   RMSBK')
          IF(NUMOUT.GT.NUMSPOT.AND.ILIST.EQ.1)
     .     WRITE(6,1101)IH(I),IK(I),AMPOUT,PHSOUT,IQ,RMSBK
      IF(NUMOUT.GT.NUMSPOT) GO TO 500
C
C     write up to NUMSPOT spots
C
      IF(GU.EQ.YES) THEN
      WRITE(6,1105) IH(I),IK(I),XA(I),YA(I)
 1105 FORMAT(/,/,/,132('*'),/,/,/,' Reflection  H',I3,'  K',I3,10X,
     1       'Lattice coordinates in grid units ',2F8.2)
C
      ELSE
      WRITE(6,1100) IH(I),IK(I),XA(I),YA(I),XSCALE,YSCALE
 1100 FORMAT(/,/,/,132('*'),/,/,/,' Reflection  H',I3,'  K',I3,10X,
     1       'Lattice coordinates in mm ',2F8.2,
     2       ' in grid units ',2F8.2)
      END IF
C
      WRITE(6,1110) RMSBK, AMPINT, VECPHA1, VECPHA2, PINTP, GDMEAN
 1110 FORMAT(/,' RMS backgd =',F6.1,
     .  ' Integrated bgd-corr amp over 3x3 box =',F6.1,/,
     . '  Ampl-weighted vec. sum of phase =',F6.1, 
     . '  sinc func-weighted vec. sum of phase =',F6.1, 
     .       ' Interp. phase/goodness=',2F6.1) 
      WRITE(6,1121) AMPOUT,PHSOUT,IQ
1121  FORMAT(' Amplitude, phase and IQ to be output =',2F8.1,I3)
      IF(IHOR.GT.10) GO TO 400
C  
C     amps & phases side by side
C
      WRITE(6,1120)
 1120 FORMAT(/,/,19X,'Amplitudes',65X,'Phases')
      WRITE(6,1140) (IXGU(J),J=1,IHOR)
 1140 FORMAT(/,'  X(grid units)',21I5)
      WRITE(6,1150) (IXGU(J),J=1,IHOR)
 1150 FORMAT('+',67X,'X(grid units)',10I5)
      WRITE(6,1160)
 1160 FORMAT(/,'  Y(grid units)',53X,'Y(grid units)')
      L = IVERT
      DO 390 K=1,IVERT
      WRITE(6,1170) IYGU(L),(IAMP(J,L),J=1,IHOR)
 1170 FORMAT(/,6X,I5,4X,21I5)
      WRITE(6,1180) IYGU(L),(IPHI(J,L),J=1,IHOR)
 1180 FORMAT('+',72X,I5,4X,10I5)
      L = L - 1
  390 CONTINUE
      GO TO 500
C
C     write amps first then phases
C
  400 WRITE(6,1200)
 1200 FORMAT(/,/,19X,'Amplitudes')
      WRITE(6,1140) (IXGU(J),J=1,IHOR)
      WRITE(6,1210)
 1210 FORMAT(/,'  Y(grid units)')
      L = IVERT
      DO 420 K=1,IVERT
      WRITE(6,1170) IYGU(L),(IAMP(J,L),J=1,IHOR)
      L = L - 1
  420 CONTINUE
      WRITE(6,1230)
 1230 FORMAT(/,19X,'Phases')
      WRITE(6,1140) (IXGU(J),J=1,IHOR)
      WRITE(6,1210)
      L = IVERT
      DO 440 K=1,IVERT
      WRITE(6,1170) IYGU(L),(IPHI(J,L),J=1,IHOR)
      L = L - 1
  440 CONTINUE
  500 CONTINUE
C
C     write summed,squared amplitudes
C
      IPERIM=0
      DO 530 K=1,NVERT
530     IPERIM=IPERIM+ISUM(1,K)+ISUM(NHOR,K)
      DO 535 J=2,NHOR-1
535     IPERIM=IPERIM+ISUM(J,1)+ISUM(J,NVERT)
      PERIM=FLOAT(IPERIM)/(2.0*(NVERT+NHOR-2))
C
      DO 520 K=1,NVERT
        DO 520 J=1,NHOR
          ISUMI(J,K)=ISUM(J,K)*7.0/PERIM + 0.5
          ISUM(J,K) = SQRT(FLOAT(ISUM(J,K)) /(NSPOT)) + 0.5
  520 CONTINUE
C
      WRITE(6,1250)     ! Amplitude output
 1250 FORMAT(/,/,132('*'),/,/,19X,'SQRT of summed,squared amplitudes',
     .  /,19X,33('-'),/)
      L = NVERT
      DO 540 K=1,NVERT
      WRITE(6,1270) (ISUM(J,L),J=1,NHOR)
 1270 FORMAT(/,15X,21I5)
      L = L - 1
  540 CONTINUE
      WRITE(6,1252)
C
      SCALEFAC = 7.0/PERIM
      WRITE(6,1251) SCALEFAC            ! Intensity output
 1251 FORMAT(/,/,132('*'),/,/,19X,'scaled intensities (perimeter',
     .  ' averaged to 7.0)',
     .  '    scale factor = ',F10.7,/,19X,40('-'))
      L = NVERT
      DO 545 K=1,NVERT
      WRITE(6,1270) (ISUMI(J,L),J=1,NHOR)
      L = L - 1
  545 CONTINUE
      WRITE(6,1252)
1252  FORMAT(/,/)
      WRITE(6,1253) NUMOUT,NGOOD,NBAD,(J,NIQ(J),J=1,9)
1253  FORMAT(I10,'  Total spots found',/,
     .  I10,'  Good spots for output',/,I10,'  Bad spots not used',/,
     .  '    IQ    NUMBER',9(/,I6,I10),' (negatives)')
      IA=4
      N=4
      E=-1.0
      DIAGSUM=0.0
      DO 1500 JA=1,4
1500  DIAGSUM=DIAGSUM+A(JA,JA)
      DIAGADD=(DIAGSUM/4.0) * 50.0
      DO 1510 JA=1,4
1510  A(JA,JA)=A(JA,JA) + DIAGADD
      WRITE(6,1501) DIAGSUM/4.0, DIAGADD
1501  FORMAT(' MEAN MATRIX DIAGONAL   =',F10.0,/,
     .   ' MEAN ADDED TO DIAGONAL =',F10.0)
      CALL MA21AD(A,IA,N,B,W,E)
      IF(E.NE.0.0) THEN
        WRITE(6,8901)E
8901    FORMAT(' MA21AD FAILED, E=',F10.5)
        STOP
      ENDIF
      AX = AX + B(1)*FSHIFT
      AY = AY + B(2)*FSHIFT
      BX = BX + B(3)*FSHIFT
      BY = BY + B(4)*FSHIFT
C
      RLATTICE(1,ICYCL)=AX
      RLATTICE(2,ICYCL)=AY
      RLATTICE(3,ICYCL)=BX
      RLATTICE(4,ICYCL)=BY
C
      WRITE(6,8910) ICYCL,(B(I),I=1,4),AX,AY,BX,BY
8910  FORMAT(': CYCLE             SHIFTS                      ',
     .       '   LATTICE PARAMS         ',
     .       /,':',I6,4F8.4,4F9.3)
        DO 8950 J=1,4
8950    BST(J,ICYCL)=B(J)
        DO 8960 J=1,9
8960    NIQST(J,ICYCL)=NIQ(J)
9000  CONTINUE
C
      IQMAX = 0
      IBESTCYCL = 1
      WRITE(6,9502)
      DO 9500 ICYCL=1,NCYCLES
        WRITE(6,9501)ICYCL,(BST(I,ICYCL),I=1,4),SUMASQST(ICYCL),
     .                   (NIQST(I,ICYCL),I=1,9)
        IQVAL = 0
        do IIQN=1,9
          IQVAL = IQVAL + NIQST(IIQN,ICYCL)*((10-IIQN)**2)
        enddo
        if(IQMAX.LT.IQVAL) then
          IQMAX = IQVAL
          IBESTCYCL=ICYCL
        endif
9500  continue
C
      WRITE(6,9504)
      ICYCL=IBESTCYCL
      WRITE(6,9503)ICYCL,(BST(I,ICYCL),I=1,4),SUMASQST(ICYCL),
     .                   (NIQST(I,ICYCL),I=1,9)
C
CHEN
C
      write(6,'(''::Best lattice: '',4F12.3)')(RLATTICE(I,ICYCL),I=1,4)
C
      OPEN(UNIT=11,FILE='TMP456111.tmp',STATUS='UNKNOWN')
      WRITE(11,'(4F12.3)')(RLATTICE(I,ICYCL),I=1,4)
      CLOSE(11)
C
CHEN
C
9501  FORMAT(':',I3,4F12.8,F15.0,9I4)
9502  FORMAT(': Summary of refinement',/,
     .': CYC              SHIFTS                                SUMINT ',
     .'  IQ 1   2   3   4   5   6   7   8   9',/)
9503  FORMAT('::',I3,4F12.8,F15.0,9I4)
9504  FORMAT('::',/,':: Best corrected lattice:',/,
     .':: CYC              SHIFTS                                SUMINT ',
     .'  IQ 1   2   3   4   5   6   7   8   9',/)
      STOP
4550    WRITE(6,4551)NMAX
4551    FORMAT(' Too many spots for current prog dimensions',I5)
      END
C
C****************************************************************
C
      SUBROUTINE GRID(ILIST)
C
C     subroutine to generate a lattice from 1,0 & 0,1 coordinates
C
      DIMENSION IH(2000),IK(2000),IXC(2000),IYC(2000),
     1          IXGU(21),IYGU(21),NXYZ(3),
     2          IAMP(21,21),IPHI(21,21),
     3          XA(2000),YA(2000)
      LOGICAL TURN,FIRST 
      COMMON NOH,NOK,NSPOT,IX1,IX2,IY1,IY2,IHOR,IVERT,IXOUT1,IXOUT2,
     1       IXL,IYL,IX,IY,
     2       IH,IK,IAMP,IPHI,IXGU,IYGU,IXC,IYC,
     3       XA,YA,AX,AY,BX,BY,RINNER,ROUTER,SCALE,SCAMP,
     4       APART,BPART,AMP,PHASE,DELPX,DELPY,
     5       TURN,FIRST
      COMMON//NX,NY,NZ
      EQUIVALENCE (NXYZ,NX)
        DATA NMAX/2000/
      AX = AX * SCALE
      AY = AY * SCALE
      BX = BX * SCALE
      BY = BY * SCALE
      IF(ILIST.EQ.1)WRITE(6,10)
   10 FORMAT(/,' Lattice generated coordinates',/,8X,'H',9X,'K',
     1       7X,'X',9X,'Y',/,'0')
      NOHD = 2 * NOH + 1
      NOKD = 2 * NOK + 1
      NSPOT = 0
      DO 100 NH=1,NOHD
      DO 100 NK=1,NOKD
      JH = NH - NOH - 1
      JK = NK - NOK - 1
      X = JH * AX + JK * BX
      Y = JH * AY + JK * BY
      IF(Y.LT.0.) GO TO 100
C
C     no need to SQRT DSTAR as RINNER & ROUTER have been squared
C
      DSTAR = X * X + Y * Y
      IF(DSTAR.GT.ROUTER) GO TO 100
      IF(DSTAR.LT.RINNER) GO TO 100
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
        IF(ILIST.EQ.1)WRITE(6,117)JH,JK,X,Y
117     FORMAT(' SPOT OUTSIDE BOX NOT USED',2I10,2F10.1)
        GO TO 100
116   CONTINUE
C
      IF(ILIST.EQ.1)WRITE(6,20) JH,JK,X,Y
   20 FORMAT(2I10,2F10.1)
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
      DIMENSION IH(2000),IK(2000),IXC(2000),IYC(2000),
     1          IXGU(21),IYGU(21),NXYZ(3),
     2          IAMP(21,21),IPHI(21,21),
     3          XA(2000),YA(2000)
      LOGICAL TURN,FIRST
      COMMON NOH,NOK,NSPOT,IX1,IX2,IY1,IY2,IHOR,IVERT,IXOUT1,IXOUT2,
     1       IXL,IYL,IX,IY,
     2       IH,IK,IAMP,IPHI,IXGU,IYGU,IXC,IYC,
     3       XA,YA,AX,AY,BX,BY,RINNER,ROUTER,SCALE,SCAMP,
     4       APART,BPART,AMP,PHASE,DELPX,DELPY,
     5       TURN,FIRST
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
      SUBROUTINE SPOTS(IPASS)
C
C     subroutine to generate required spots from 1,0 & 0,1 coordinates
C
      DIMENSION IH(2000),IK(2000),IXC(2000),IYC(2000),
     1          IXGU(21),IYGU(21),NXYZ(3),
     2          IAMP(21,21),IPHI(21,21),
     3          XA(2000),YA(2000)
      LOGICAL TURN,FIRST 
      COMMON NOH,NOK,NSPOT,IX1,IX2,IY1,IY2,IHOR,IVERT,IXOUT1,IXOUT2,
     1       IXL,IYL,IX,IY,
     2       IH,IK,IAMP,IPHI,IXGU,IYGU,IXC,IYC,
     3       XA,YA,AX,AY,BX,BY,RINNER,ROUTER,SCALE,SCAMP,
     4       APART,BPART,AMP,PHASE,DELPX,DELPY,
     5       TURN,FIRST
      COMMON//NX,NY,NZ
      EQUIVALENCE (NXYZ,NX)
        DATA NMAX/2000/
      AX = AX * SCALE
      AY = AY * SCALE
      BX = BX * SCALE
      BY = BY * SCALE
      WRITE(6,10)
   10 FORMAT(/,' Requested spot coordinates',/,8X,'H',9X,'K',
     1       7X,'X',9X,'Y',/,'0')
      NSPOTSTOR=NSPOT
      NSPOT = 0
      WRITE(6,*)' '
      WRITE(6,*)' # type in JH,JK'
      WRITE(6,*)' '
      DO 100 NREQ=1,NMAX
      IF(IPASS.EQ.0) THEN
        READ(5,*,END=200) JH,JK
        IF(JH.EQ.100) GO TO 200
      ELSE
        IF(NREQ.GT.NSPOTSTOR) GO TO 200
        JH=IH(NREQ)
        JK=IK(NREQ)
      ENDIF 
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
115   NSPOT=NSPOT-1
      WRITE(6,117)JH,JK,X,Y
117   FORMAT(' SPOT OUTSIDE BOX NOT USED',2I10,2F10.1)
116   CONTINUE
C
      WRITE(6,20) JH,JK,X,Y
   20 FORMAT(2I10,2F10.1)
  100 CONTINUE
      WRITE(6,21)NSPOT
21      FORMAT(' THERE WERE A TOTAL OF',I5,'  REQUESTED SPOTS') 
200     CONTINUE
      IPASS=1
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
      DIMENSION IH(2000),IK(2000),IXC(2000),IYC(2000),
     1          IXGU(21),IYGU(21),NXYZ(3),
     2          IAMP(21,21),IPHI(21,21),
     3          XA(2000),YA(2000),
     4          ARRAY(42,21)
      LOGICAL TURN,FIRST
      COMMON NOH,NOK,NSPOT,IX1,IX2,IY1,IY2,IHOR,IVERT,IXOUT1,IXOUT2,
     1       IXL,IYL,IX,IY,
     2       IH,IK,IAMP,IPHI,IXGU,IYGU,IXC,IYC,
     3       XA,YA,AX,AY,BX,BY,RINNER,ROUTER,SCALE,SCAMP,
     4       APART,BPART,AMP,PHASE,DELPX,DELPY,
     5       TURN,FIRST
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
      IAMP(J,K) = AMP * SCAMP + 0.5
      IPHI(J,K) = PHASE + 0.5
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
      IAMP(JJ,KK) = AMP * SCAMP + 0.5
      IPHI(JJ,KK) = PHASE + 0.5
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
   10 FORMAT(/,' error on reading transform')
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
