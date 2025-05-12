C*******************************************************************************
C
C TTBOXREF : Refines lattice parameters in image transform of tilted specimen.
C                          first created from TTBOX - 15 November 1990
C                          last modified - see versions below.
C
C        Version   2.00   15 Nov 1990  RH   Original changes from TTBOX/MMLATREF
C        Version   2.01   17 Nov 1990  RH   Extra format changes, annotation.
C
C    Incorporates several aspects in the refinement - similar to MMLATREF
C	1. Maximises sum of sinc-fitted intensities.
C	2. Uses weighted coefficients -- 1/(rmsbk**2+ampsinc**2)
C		this should maximise the number of spots with IQ>=7
C	3. Matrix diagonal has constant added to it to stabilise refinement when
C		transform is very strong and clean --- this may mean that shifts
C		greater than 1.0 will be needed if the data is very noisy.
C						  (eg. at high resolution)
C	4. Summary table printed out at end to judge progress.
C	5. May be mainly good for getting just a little extra after UNBENDING.
C	6. Present sinc fit is just over 2x2 box around spot centre ... this is
C		thought to be insignificantly different from any bigger box.
C
C  Previous TTBOX versions  1.01    9 Dec 1985  RH
C  from which this program  1.02   14 Feb 1986  RH  SEGMNT option.
C  was created.             1.03   18 Nov 1987  JMB Rectangular images
C                           1.04   12 Jul 1988  RH  
C                           1.05   10 Sep 1988  RH  Increase TITLE to 68 bytes.
C                           1.06    2 Jan 1989  RH  ICTFBXMAX upped to 120, etc
C                           1.07   24 Jan 1989  RH  IQ=8 plotted as small dot.
C
C     DATACARDS ----------------------------------------------
C
C  0.   NCYCLES,FSHIFT   (*)
C
C  1.	FILIN -- full name of input file (.FFT)
C
C  2.	ISER,TITLE (I10,17A4)
C
C  3.	GENGRID   (A)
C
C  4.	GENPTS    (A)
C
C  5.	ISIZEX,ISIZEY,DSTEP,XMAG,CS,KVOLT   (*)
C
C  6.	DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL   (*)
C
C  7.	NUMSPOT, NOH, NOK, NHOR, NVERT  (*)
C
C  9	RESMIN, RESMAX,  XORIG, YORIG, SEGMNT  (*)
C
C  10.	if GENGRID or GENPTS :		AX, AY, BX, BY    (*)
C
C  11.	if GENPTS 		        : IH(I), IK(I)              (*)
C	if .not.GENGRID and .not.GENPTS : IH(I), IK(I), X(I), Y(I)  (*)
C
C
C     NCYCLES     number of cycles of lattice parameter refinement.
C     FSHIFT      fractional shift to be applied at end of each cycle.
C     ISER        serial number for run
C     TITLE       title to be printed
C     GENGRID     if YES generate grid from lattice points (1,0) & (0,1).
C     GENPTS      if YES individual spots requested & generated from grid
C		  if NO all spots within RESMIN to RESMAX generated.
C     ISIZEX,Y	  size of image in x and y, checked against file-header.
C     DSTEP	  densitometer stepsize in microns.
C     XMAG	  magnification of micrograph.
C     CS	  spherical aberration coefficient in mm.
C     KVOLT	  microscope voltage in KV, used to calculate wavelength. 
C     DFMID1	  defocus in one direction (underfocus +ve) 
C     DFMID2	  defocus at 90-degs to above
C     ANGAST	  direction for DFMID1 in degrees relative to x,y in transform.
C     TLTAXIS	  direction of tiltaxis in degrees relative to x,y in transform,
C		    should be between -90 and +90 degrees.
C     TLTANGL	  magnitude of tiltangle.
C			(+ve for less underfocus at start of scan(y=0)).
C			if tiltaxis is precisely parallel to y, then TLTANGL
C			should be positive for less underfocus at x=0.
C     NUMSPOT     number of spots to be printed.
C     NOH, NOK    number of orders of spots in H & K directions to be generated.
C     NHOR, NVERT box size in grid units in horizontal & vertical directions,
C                 i.e. X & Y resp. ( up to 20 grid units in each
C                 direction).
C     RESMIN, RESMAX inner & outer resolution limits in Angstroms within which
C			 spots(centre of box) must fall.
C     XORIG, YORIG X & Y phase origin shifts to be added to those
C                  added to those read in on the transform.
C     SEGMNT      segment of reciprocal space within which spots
C                 must fall to be include in this run. +/- 90
C                 includes all spots, +45 is half data near tiltaxis
C                 -45 is half perpendicular to tilt axis.
C     AX,AY,BX,BY coordinates in grid units of 1,0 & 0,1 spots
C                 respectively.
C     IH, IK      indices of individual spots required.
C     X, Y        coordinates of individual spots required.
C
C*******************************************************************************
C
      PARAMETER (MAXCYC=50)
      PARAMETER (NMAX=2000,IBOXMAX=41,ICTFBXMAX=401,INBOXMAX=361)
      REAL KVOLT
      REAL*8 NEWMAX,AMPTOTAL,AMPTOT,RMSBK,RMSBKOLD,AMPSQ
      INTEGER*8 ISUM(IBOXMAX,IBOXMAX)
      INTEGER*8 ISUMI(IBOXMAX,IBOXMAX)
      INTEGER*8 IAMP(IBOXMAX,IBOXMAX)
      INTEGER*8 IPERIM,ICENTRE,INEAR
      INTEGER*8 ISER
      DIMENSION XA(NMAX),YA(NMAX),IXC(NMAX),IYC(NMAX),IH(NMAX),IK(NMAX)
      DIMENSION AAMP(IBOXMAX,IBOXMAX),PPHI(IBOXMAX,IBOXMAX),
     .		IXGU(IBOXMAX),IYGU(IBOXMAX),IPHI(IBOXMAX,IBOXMAX)
      DIMENSION AP(INBOXMAX,INBOXMAX),BP(INBOXMAX,INBOXMAX)
      DIMENSION ACTF(ICTFBXMAX,ICTFBXMAX),BCTF(ICTFBXMAX,ICTFBXMAX)
      DIMENSION TITLE(17),NXYZ(3),MXYZ(3),PHANG(4),WTS(4),
     .		DELX(2),DELY(2),NIQ(9)
      DIMENSION GRAD(4),BST(4,MAXCYC),SUMASQST(MAXCYC),NIQST(9,MAXCYC)
      REAL*8 A(4,4),B(4),E,W(80)
      CHARACTER GENGRID,GENPTS,YES
      LOGICAL TURN,ILIST,LIST
c      CHARACTER*40 FILIN,FILOUT
      CHARACTER*40 FILIN
      COMMON NX,NY,NZ
      EQUIVALENCE (NXYZ,NX)
      DATA YES/'Y'/,NIQ/9*0/,TWOPI/6.2831853/
      DATA ILIST/.FALSE./,LIST/.FALSE./
C
      WRITE(6,1000)
 1000 FORMAT('0',' TTBOXREF V2.01(17-Nov-1990) : refines lattice',
     1' parameters in tilted images')
      READ(5,*) NCYCLES,FSHIFT
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
      AMPTOTAL=0.0
      NPHI = 4
      NY2 = NY / 2
      NY2M1 = NY2 - 1
      NY2M2 = NY2 - 2
      NXP2 = NX * 2
      NXM1 = NX - 1
      NXM2 = NX - 2
      READ(5,1020)ISER,TITLE
 1020 FORMAT(I10,17A4)
      WRITE(6,1025) ISER,TITLE
 1025 FORMAT(/' Serial number :',I10/' Title :',17A4)
 1028 FORMAT(A)
1029	FORMAT(1X,20A1)
      READ(5,1028) GENGRID
      WRITE(6,1029) GENGRID
      READ(5,1028) GENPTS
      WRITE(6,1029) GENPTS
C
C  Input of all c.t.f. and tilt data for image, calculation of all the needed
C      preliminaries except for calculation of c.t.f. itself.
C
      READ(5,*) ISIZEX,ISIZEY,DSTEP,XMAG,CS,KVOLT
      	IF(ISIZEY.NE.NY)     GO TO 6001
      	IF(ISIZEX.NE.NXP2-2) GO TO 6001
      READ(5,*) DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
98     	IF(TLTAXIS.LE.-90.0) THEN
      		TLTAXIS=TLTAXIS+180.0
      		GO TO 98
      	ENDIF
99     	IF(TLTAXIS.GT.90.0) THEN
      		TLTAXIS=TLTAXIS-180.0
      		GO TO 99
      	ENDIF
C
      WRITE(6,101) ISIZEX,ISIZEY,DSTEP,XMAG,CS,KVOLT
      WRITE(6,102) DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
101   FORMAT(/' SIZE OF DENSITOMETERED ARRAY; X,Y...',2I7/
     .       ' DENSITOMETERED STEPSIZE(MICRONS)....',F10.2/
     .       ' MAGNIFICATION OF MICROGRAPH.........',F8.0/
     .       ' SPHERICAL ABERRATION (MM) ..........',F10.2/
     .       ' ACCELERATING VOLTAGE (KV) ..........',F8.0)
102   FORMAT(' UNDERFOCUS 1 .......................',F8.0/
     .       ' UNDERFOCUS 2 .......................',F8.0/
     .       ' DIRECTION FOR UNDERFOCUS 1 .........',F9.1/
     .       ' TILT AXIS DIRECTION ................',F9.1/
     .	     ' TILT ANGLE .........................',F9.1,
     .	' +VE FOR LESS UNDERFOCUS AT START OF SCAN(Y=0)'/)
      ANGAST=ANGAST*TWOPI/360.0
      TLTAXIS=TLTAXIS*TWOPI/360.0
      TLTANGL=TLTANGL*TWOPI/360.0
C
      TANTILT=TAN(TLTANGL)
      COST=COS(TLTAXIS)
      SINT=SIN(TLTAXIS)
C
      CS=CS*(10.0**7.0)
      KVOLT=KVOLT*1000.0
      WL=12.3/SQRT(KVOLT+KVOLT**2/(10.0**6.0))
      WRITE(6,103)WL
103   FORMAT(' WAVELENGTH (ANGSTROMS)',F10.4)
      STEPR=DSTEP*(10.0**4.0)/XMAG
C
      TRNSTEPX=1.0/(STEPR*ISIZEX)
C  TRNSTEPX IS SIZE OF TRANSFORM GRID STEPS IN X DIRECTION
      THETATR=WL/(STEPR*ISIZEX)
C  THETATR IS DIFFRACTION ANGLE OF FIRST GRID POINT IN X DIRECTION
C   OF TRANSFORM.
C
C
C	Calculate height difference across image
C
        PERP=ISIZEY*COST+ISIZEX*(ABS(SINT))
C      WRITE(6,91701)STEPR,TANTILT,COST,SINT,ISIZEX,ISIZEY
91701	FORMAT(4F10.5,2I10)
	DELHEIGHT=ABS(STEPR*PERP*TANTILT)
      WRITE(6,91700)PERP,DELHEIGHT
91700	FORMAT(' PERPENDICULAR DISTANCE FROM CORNER OF FILM TO TILT',
     .' AXIS',F10.3/' DIFFERENCE IN HEIGHT OF FAR CORNERS OF IMAGE',
     .F10.3/)
C
      READ(5,*) NUMSPOT,NOH,NOK,NHOR,NVERT
      IF(NUMSPOT.EQ.0) ILIST=.FALSE.
C
C
      READ(5,*) RESMIN,RESMAX,XORIG,YORIG,SEGMNT
      IF(RESMIN.LT.RESMAX) THEN
      	R=RESMIN	! Reverse if input in wrong order !
      	RESMIN=RESMAX
      	RESMAX=R
      ENDIF
      RESMINSQ=RESMIN**2
      RESMAXSQ=RESMAX**2
      WRITE(6,1030) NCYCLES,FSHIFT,NUMSPOT,NOH,NOK,NHOR,NVERT,
     1              RESMIN,RESMAX,
     .              XORIG,YORIG,SEGMNT
1030  FORMAT(/' number of cycles ============================',I5/
     1        ' fractional shift to be applied ==============',F7.2/
     2        ' number of spots printed =====================',I5/
     3        ' No. of orders in h & k=======================',2I5/
     4        ' No. of points in box horiz & vert direction =',2I5/
     5        ' Inner & outer resolution limits (Angstroms)==',2F8.1/
     6        ' X & Y phase origin shifts ===================',2F8.1/
     7        ' SEGMNT for o/p of spots within segment ======',F8.1)
      PI = 3.141592654
      TWOPI = 2. * PI
      DELPX = -TWOPI * (XOR + XORIG) / (2. * (NXM1))
      DELPY = -TWOPI * (YOR + YORIG) / NY
C
C
      DRAD=PI/180.0
      IF(NHOR.GT.IBOXMAX) NHOR = IBOXMAX
      IF(NVERT.GT.IBOXMAX) NVERT = IBOXMAX  
      TURN = .FALSE.	! Specifies whether desired spot comes from negative X.
      I = 0
C
      IF(GENGRID.EQ.YES.OR.GENPTS.EQ.YES) THEN
      	  READ(5,*) AX,AY,BX,BY
      	  WRITE(6,1050) AX,AY,BX,BY
 1050 	  FORMAT(/' coordinates of 1,0 & 0,1 ',4F10.3)
      ENDIF
C
C
C
C   Outermost loop refinement cycle beginning
C
      DO 9000 ICYCL=1,NCYCLES
C
      IF(GENGRID.EQ.YES.OR.GENPTS.EQ.YES) THEN
C     	  point generation required
      SIZEX=ISIZEX
      RATIOXY=SIZEX/ISIZEY     
      ACY=AY*RATIOXY
      BCY=BY*RATIOXY
      WRITE(6,91050)AX,ACY,BX,BCY
91050	FORMAT(' coordinates of 1,0 & 0,1 with y components scaled',
     .' by factor ISIZEX/ISIZEY'/26X,4F10.3/)
C 
      	  IF(GENPTS.EQ.YES) THEN
      	    CALL SPOTS(AX,AY,BX,BY,ACY,BCY,IXC,IYC,IH,IK,XA,YA,
     .			NXM2,NY2M2,NSPOT,LIST)
      	  ELSE
C
C     	    grid generation required
C
      	    CALL GRID(AX,AY,BX,BY,ACY,BCY,IXC,IYC,
     .                   IH,IK,XA,YA,NOH,NOK,
     .			 RESMINSQ,RESMAXSQ,
     .                   NXM2,NY2M2,NSPOT,LIST,
     .                   TLTAXIS,SEGMNT,TRNSTEPX)
      	  END IF
C
C     	  read in individual points
C
      ELSE
      	  WRITE(6,1060)
 1060 	  FORMAT(/' Coordinates read in    H    K       X         Y'
     1    	    ,' nearest grid pt X      Y'/'0')  
  110 	  I = I + 1
      	  IF(NSPOT.GE.NMAX) GO TO 4550
  115 	  READ(5,*,END=120) IH(I),IK(I),XA(I),YA(I)
      	  IF(IH(I).EQ.100) GO TO 120
      	  IXC(I) = XA(I) + SIGN(0.5,XA(I))
      	  IYC(I) = YA(I) + SIGN(0.5,YA(I))
C     
C     	  reject spot if outside boundary
C
      	  IF(IXC(I).GT.NXM2) GO TO 115
      	  IF(IXC(I).LT.-NXM2) GO TO 115
      	  IF(IYC(I).GT.NY2M2) GO TO 115
      	  IF(IYC(I).LT.-NY2M2) GO TO 115
      	  WRITE(6,1070) IH(I),IK(I),XA(I),YA(I),IXC(I),IYC(I)
 1070 	  FORMAT(20X,2I5,2F10.1,12X,I5,2X,I5)
      	  NSPOT = I
      	  GO TO 110
      END IF
C
C     data read in proceed
C
  120 IHOR2 = NHOR / 2
      IVERT2 = NVERT / 2
      		NUMOUT = 0
      		NGOOD=0
      		NBAD=0
      		DO 124 N=1,9
124		NIQ(N)=0
      		DO 125 IM=1,4
      		B(IM)=0.0
      		DO 125 JM=1,4
      		A(IM,JM)=0.0
125		CONTINUE
C
C     make sure odd number of elements in box
C
      NHOR = IHOR2 * 2 + 1
      NVERT = IVERT2 * 2 + 1
      WRITE(6,1080) NHOR,NVERT
 1080 FORMAT(/' Box size in transform grid units :',I3,' *',I3)
1081  FORMAT(I10,17A4)
      DO 130 K=1,IBOXMAX
      DO 130 J=1,IBOXMAX
      ISUM(J,K) = 0
      ISUMI(J,K) = 0
  130 CONTINUE
      WRITE(6,1103)		!  Asterisks to mark beginning of output.
C
C  Beginning of Do-loop over all required spots.
C
      DO 500 I=1,NSPOT
      IHOR = NHOR
      IVERT = NVERT
C
C  IHOR,IVERT is required output box size.
C  ICTFHOR,ICTFVERT is necessary CTF box size for convolution -- this will
C   get bigger with increasing resolution, becoming its maximum size for big,
C   highly tilted images at high resolution. For example, a 5000x5000 area
C   which is tilted to 60-degrees will require ICTFHOR,ICTFVERT = 100 , if
C   a resolution of 3 Angstroms is desired.  Both of these parameters are 
C   calculated inside subroutine CTFGEN.
C  INHOR,INVERT is then the necessary input box size from the transform needed
C   to carry out the convolution multiplication successfully.
c   It is always bigger than either ctf box or the output box.
C		INHOR  = IHOR  + ICTFHOR  (- 1)
C		INVERT = IVERT + ICTFVERT (- 1)
C
      CALL CTFGEN(IH(I),IK(I),XA(I),YA(I),RATIOXY,
     .    THETATR,DFMID1,DFMID2,ANGAST,
     .	  CS,WL,STEPR,ISIZEX,ISIZEY,DELHEIGHT,
     .    TLTAXIS,TLTANGL,TANTILT,COST,SINT,
     .    ICTFHOR,ICTFVERT,ACTF,BCTF,
     .	  ILIST,DFMID,DELCHI,CTFMID,FACTOR,ISENS)
C
      INHOR  = IHOR  + ICTFHOR 		! ODD = ODD +EVEN
      INVERT = IVERT + ICTFVERT 	! ODD = ODD +EVEN
      INHOR2 = INHOR / 2
      INVERT2= INVERT/ 2
C
      IXL = IXC(I) - INHOR2 
      IXR = IXC(I) + INHOR2 
      IYL = IYC(I) - INVERT2 
      IYU = IYC(I) + INVERT2 
C
C     check edge spots
C
      IF(IXL.LT.-NXM1) GO TO 160
      IF(IXR.GT.NXM1) GO TO 160
      IF(IYL.LT.-NY2M1) GO TO 160
      IF(IYU.GT.NY2M1) GO TO 160
      GO TO 180
160	WRITE(6,161) IH(I),IK(I)
161	FORMAT(' SPOT TOO NEAR EDGE FOR CTF TILT CORRECTION',2I8)
      	GO TO 500
180   CONTINUE
C
C     set up box edge coordinates
C
C     simple case +ve quadrant
C
      IXOUT1 = 1
      IXOUT2 = INHOR
      IF(IXL.GE.0) THEN
      IX1 = IXL 
      IX2 = IX1 + INHOR - 1
      IY1 = NY2 + IYL 
      IY2 = IY1 + INVERT - 1
      IX = IXL - 1
      IY = IYL - 1
      CALL  RDSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT,
     .		IX,IY,AP,BP,DELPX,DELPY,TURN)
      GO TO 280
C
C     simple case -ve quadrant
C
      ELSE IF(IXR.LT.0) THEN
      IX1 = -IXR  
      IX2 = IX1 + INHOR - 1
      IY1 = NY2 - IYU 
      IY2 = IY1 + INVERT - 1
      IX = IXR + 1
      IY = IYU + 1
      TURN = .TRUE.
      CALL  RDSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT,
     .		IX,IY,AP,BP,DELPX,DELPY,TURN)
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
      IY2 = IY1 + INVERT - 1
      IXOUT2 = -IXL
      IX = 0
      IY = IYU + 1
      TURN = .TRUE.
      CALL  RDSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT,
     .		IX,IY,AP,BP,DELPX,DELPY,TURN)
C
C     set up RHS of box
C
      IXOUT1 = IXOUT2 + 1
      IXOUT2 = INHOR
      IX1 = 0
      IX2 = IXR  
      IY1 = NY2 + IYL 
      IY2 = IY1 + INVERT - 1
      IX = -1
      IY = IYL - 1
      CALL  RDSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT,
     .		IX,IY,AP,BP,DELPX,DELPY,TURN)
C
280   CONTINUE
C
C     set up array for X and Y axis description coordinates.
C
      KXL = IXL + ICTFHOR/2
      DO 305 J=1,IHOR
      IXGU(J) = KXL
      KXL = KXL + 1
  305 CONTINUE
      KYL = IYL + ICTFVERT/2
      DO 306 J=1,IVERT
      IYGU(J) = KYL
      KYL = KYL + 1
  306 CONTINUE
C
C new convolution routine using scsl
C      CALL SCSL_CONVOLUTE(AP,BP,ACTF,BCTF,AAMP,PPHI,IHOR,IVERT,
C     .          ICTFHOR,ICTFVERT)
C
      CALL CONVOLUTE(AP,BP,ACTF,BCTF,AAMP,PPHI,IHOR,IVERT,
     .		ICTFHOR,ICTFVERT)
C
C     Arrays AAMP, PPHI now filled with correct numbers.
C     Now calculate RMS background -- here onwards as in MMBOX.
C       First redo IXL,IXR,IYL,IYR for the output box.
C
      IXL = IXC(I) - IHOR2 
      IXR = IXC(I) + IHOR2 
      IYL = IYC(I) - IVERT2 
      IYU = IYC(I) + IVERT2 
      AMPSQ = 0.
      KV = IVERT - 1
      KH = IHOR - 1
      DO 290 K=1,IVERT,KV
      DO 290 J=1,IHOR
      F = AAMP(J,K)
      AMPSQ = AMPSQ + F * F
  290 CONTINUE
      DO 300 K=2,KV
      DO 300 J=1,IHOR,KH
      F = AAMP(J,K)
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
      F = AAMP(J,K)
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
      XSCALE = XA(I)
      INTXA = XSCALE
      IF(XSCALE.LT.0.) INTXA = INTXA - 1
      J1 = INTXA - IXL + 1
      DELX(1) = XSCALE - INTXA
C
      DELX(2) = DELX(1) - 1.    ! Changed from 1-DELX(1) to get signed gradient
C
      YSCALE = YA(I)
      INTYA = YSCALE
      IF(YSCALE.LT.0.) INTYA = INTYA - 1
      DELY(1) = YSCALE - INTYA
C
      DELY(2) = DELY(1) - 1.    ! Changed from 1-DELY(1) to get signed gradient
C                               ! Note that result from ANGAVE will be wrong
C
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
      DO 320 L2 = 1,2	! Vector phases over 2x2 points only.
      K = K1 + L2 - 1
      DO 320 L1 = 1,2	!
      J = J1 + L1 - 1
      AMP = AAMP(J,K)
      PHASE = PPHI(J,K) / 57.2958
      ASUM1 = ASUM1 + AMP * COS(PHASE)
      BSUM1 = BSUM1 + AMP * SIN(PHASE)
C
C     calculated sinc function weighted phase
C
      IF(DELX(L1).EQ.0) GO TO 315
      IF(DELY(L2).EQ.0) THEN
      	SINC   = (SIN(PI * DELX(L1))) / (PI * DELX(L1))
      	GSINCX = COS(PI*DELX(L1))/DELX(L1) - 
     .			SIN(PI*DELX(L1))/(PI*DELX(L1)**2)
      	GSINCY = 0.0
      ELSE
      	SINC   = (SIN(PI * DELX(L1)) * SIN(PI * DELY(L2))) /
     .                          (PI**2 * DELX(L1) * DELY(L2))
      	GSINCX = (SIN(PI * DELY(L2))/(PI*DELY(L2))) * 
     .	 (COS(PI * DELX(L1))/DELX(L1)-SIN(PI*DELX(L1))/(PI*DELX(L1)**2))
      	GSINCY = (SIN(PI * DELX(L1))/(PI*DELX(L1))) * 
     .	 (COS(PI * DELY(L2))/DELY(L2)-SIN(PI*DELY(L2))/(PI*DELY(L2)**2))
      END IF
      GO TO 318
C
  315 IF(DELY(L2).EQ.0) THEN
      	SINC   = 1.
      	GSINCX = 0.
      	GSINCY = 0.
      ELSE
      	SINC   = (SIN(PI * DELY(L2))) / (PI * DELY(L2))
      	GSINCX = 0.
      	GSINCY = COS(PI*DELY(L2))/DELY(L2) -
     .			SIN(PI*DELY(L2))/(PI*DELY(L2)**2)
      END IF
C
  318 ASUM2 = ASUM2 + AMP * COS(PHASE) * SINC
      BSUM2 = BSUM2 + AMP * SIN(PHASE) * SINC
      DENOM = DENOM + SINC**2
      DADX  = DADX  + AMP * COS(PHASE) * GSINCX
      DADY  = DADY  + AMP * COS(PHASE) * GSINCY
      DBDX  = DBDX  + AMP * SIN(PHASE) * GSINCX
      DBDY  = DBDY  + AMP * SIN(PHASE) * GSINCY
  320 CONTINUE
C
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
      		WGT=1.0/(RMSBK**2+AMPSINC**2)	! EQUAL WGT ABOVE IQ=7
      		DO 340 IM=1,4
      		  B(IM)=B(IM)+AMPSINC*GRAD(IM)*WGT
      		DO 340 JM=1,4
   		  A(IM,JM)=A(IM,JM)+GRAD(IM)*GRAD(JM)*WGT
340		CONTINUE
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
      	IQ = 1 + (PHSERR/7.0)		! THIS MEANS IQ=1 HAS AMP= 8x RMSBK
      	IQ = MIN(IQ,8)			!            IQ=7     AMP= 1x RMSBK
      	IF(AMPOUT.EQ.0.00001)IQ=9
      	AMPTOTAL = AMPTOTAL + AMPOUT
C
C     sum squared amplitudes
C
      DO 350 K=1,IVERT
      DO 350 J=1,IHOR
      IAMP(J,K) = AAMP(J,K) + 0.5
      IPHI(J,K) = PPHI(J,K) + 0.5
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
      CALL  ANGAVE(NPHI,PHANG,WTS,PINTP,GDMEAN) 
C
      	IF(IQ.LE.7) NGOOD=NGOOD+1
      	IF(IQ.GT.7) NBAD=NBAD+1
      	NIQ(IQ) = NIQ(IQ)+1
C
C     set up pagination
C
      NUMOUT = NUMOUT + 1
      	  IF(NUMOUT.GE.NUMSPOT)	ILIST=.FALSE.
C      	  IF(NUMOUT.EQ.NUMSPOT+1) THEN
C      		WRITE(6,1103)
C      		WRITE(6,1102)
C      	  ENDIF
1102	  FORMAT(/' OTHER SPOTS NOT PRINTED OUT WITH FULL DIAGNOSTICS')
1108	  FORMAT('   H   K  AMPOUT  PHSOUT IQ   RMSBK     DFMID    ',
     .	   'NCTFSAMPLES    CTFINMIDDLE   RESCALING BY')
C    	  IF(NUMOUT.GT.NUMSPOT) THEN
C      	   NUMAFTER=NUMOUT-NUMSPOT-1	! Test for table heading output.
C      	   IF(60*((NUMAFTER)/60).EQ.NUMAFTER) WRITE(6,1108)
C      	   WRITE(6,1101)IH(I),IK(I),ISENS,AMPOUT,PHSOUT,IQ,RMSBK,
C     .	   DFMID,DELCHI,ICTFHOR,CTFMID,FACTOR
C      	  ENDIF
1101	FORMAT(2I4,A1,F7.1,F8.1,I3,F8.1,F10.1,F11.2,
     .	'(',I3,')',F11.4,F12.3)
      IF(NUMOUT.GT.NUMSPOT) GO TO 500
C
C     write in detail up to NUMSPOT spots
C
      WRITE(6,1105) IH(I),IK(I),XA(I),YA(I)
1103  FORMAT(//132('*')/)
1105  FORMAT(/' Reflection  H',I3,'  K',I3,10X,
     1       'Lattice coordinates in grid units ',2F8.2)
C
      WRITE(6,1110) RMSBK, AMPINT, VECPHA1, VECPHA2, PINTP, GDMEAN
1110  FORMAT(/' RMS backgd =',F6.1,
     .	' Integrated bgd-corr amp over 3x3 box =',F6.1/
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
 1120 FORMAT(//19X,'Amplitudes',65X,'Phases')
      WRITE(6,1140) (IXGU(J),J=1,IHOR)
 1140 FORMAT(/'  X(grid units)',21I5)
      WRITE(6,1150) (IXGU(J),J=1,IHOR)
 1150 FORMAT('+',67X,'X(grid units)',10I5)
      WRITE(6,1160)
 1160 FORMAT(/'  Y(grid units)',53X,'Y(grid units)')
      L = IVERT
      DO 390 K=1,IVERT
      WRITE(6,1170) IYGU(L),(IAMP(J,L),J=1,IHOR)
 1170 FORMAT(/6X,I5,4X,21I5)
      WRITE(6,1180) IYGU(L),(IPHI(J,L),J=1,IHOR)
 1180 FORMAT('+',72X,I5,4X,10I5)
      L = L - 1
  390 CONTINUE
      IF(ILIST)WRITE(6,1103)	! Asterisks.
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
      WRITE(6,1170) IYGU(L),(IAMP(J,L),J=1,IHOR)
      L = L - 1
  420 CONTINUE
      WRITE(6,1230)
 1230 FORMAT(/19X,'Phases')
      WRITE(6,1140) (IXGU(J),J=1,IHOR)
      WRITE(6,1210)
      L = IVERT
      DO 440 K=1,IVERT
      WRITE(6,1170) IYGU(L),(IPHI(J,L),J=1,IHOR)
      L = L - 1
  440 CONTINUE
      IF(ILIST)WRITE(6,1103)	!   Asterisks
  500 CONTINUE
C
C     write summed,squared amplitudes
C
      IPERIM=0
      DO 530 K=1,IVERT
530   IPERIM=IPERIM+ISUM(1,K)+ISUM(IHOR,K)
      DO 535 J=2,IHOR-1
535   IPERIM=IPERIM+ISUM(J,1)+ISUM(J,IVERT)
      PERIM=FLOAT(IPERIM)/(2.0*(IVERT+IHOR-2))
C
      DO 520 K=1,IVERT
      DO 520 J=1,IHOR
      ISUMI(J,K)=ISUM(J,K)*7.0/PERIM + 0.5
      ISUM(J,K) = SQRT(FLOAT(ISUM(J,K)) /(NSPOT)) + 0.5
  520 CONTINUE
C
      WRITE(6,1250)	! Amplitude output
 1250 FORMAT(//132('*')//1X,'SQRT of summed,squared amplitudes',
     .	/1X,33('-')/)
      L = IVERT
      DO 540 K=1,IVERT
      WRITE(6,1270) (ISUM(J,L),J=1,IHOR)
 1270 FORMAT(/1X,21I6)
      L = L - 1
  540 CONTINUE
      WRITE(6,1280) AMPTOTAL/NSPOT
1280  FORMAT(/19X,'overall average amplitude =',F10.3)
      WRITE(6,1252)
C
      SCALEFAC = 7.0/PERIM
      WRITE(6,1251) SCALEFAC		! Intensity output
 1251 FORMAT(//132('*')//1X,'scaled intensities (perimeter',
     .	' averaged to 7.0)',
     .	'   scale factor = ',F12.7/1X,40('-'))
C
C  Here calculate how close the intensity average approaches theoretical.
C
      ICENTRE=ISUMI(IHOR2+1,IVERT2+1)
      INEAR=ISUMI(IHOR2+1,IVERT2) + ISUMI(IHOR2+1,IVERT2+2) +
     .		ISUMI(IHOR2,IVERT2+1) + ISUMI(IHOR2+2,IVERT2+1)
      IF(INEAR.LE.28) THEN
      	IF(ICENTRE.GT.7)PERCENT=100.0
      	IF(ICENTRE.LE.7)PERCENT=0.0
      ELSE
      	PERCENT=((ICENTRE-7)*100.0)/((INEAR-28)*2.5)
      ENDIF
C
      L = IVERT
      DO 545 K=1,IVERT
      WRITE(6,1270) (ISUMI(J,L),J=1,IHOR)
      L = L - 1
  545 CONTINUE
      WRITE(6,1254)PERCENT
1254  FORMAT(/19X,'above indicates shape that is',F7.1,' % perfect')
      IF(PERCENT.GE.85.0) WRITE(6,1255)
      IF(PERCENT.GE.50.0.AND.PERCENT.LT.85.) WRITE(6,1256)
      IF(PERCENT.LT.50.0) WRITE(6,1257)
1255  FORMAT(19X,'this is quite satisfactory, well done!!')
1256  FORMAT(19X,'this is not bad, but could be improved!')
1257  FORMAT(19x,'this is not good enough, you must try harder!')
      WRITE(6,1252)
1252  FORMAT(//)
      WRITE(6,1253) NUMOUT,NGOOD,NBAD,(J,NIQ(J),J=1,9)
1253  FORMAT(I10,'  Total spots found'/
     .	I10,'  Good spots for output'/I10,'  Bad spots not used'/
     .	'    IQ    NUMBER'/9(I6,I10/))
      		IA=4
      		N=4
      		E=-1.0
      			DIAGSUM=0.0
      			DO 1500 JA=1,4
1500  			DIAGSUM=DIAGSUM+A(JA,JA)
      			DIAGADD=(DIAGSUM/4.0) * 50.0
      			DO 1510 JA=1,4
1510			A(JA,JA)=A(JA,JA) + DIAGADD
      			WRITE(6,1501) DIAGSUM/4.0, DIAGADD
1501			FORMAT(' MEAN MATRIX DIAGONAL   =',F10.0/
     .			       ' MEAN ADDED TO DIAGONAL =',F10.0)
      CALL MA21AD(A,IA,N,B,W,E)
      		IF(E.NE.0.0) THEN
      			WRITE(6,8901)E
8901			FORMAT(' MA21AD FAILED, E=',F10.5)
      			STOP
      		ENDIF
      AX = AX + B(1)*FSHIFT
      AY = AY + B(2)*FSHIFT
      BX = BX + B(3)*FSHIFT
      BY = BY + B(4)*FSHIFT
      WRITE(6,8910) ICYCL,(B(I),I=1,4),AX,AY,BX,BY
8910  FORMAT(' CYCLE             SHIFTS                      ',
     .	     '   LATTICE PARAMS         '/I6,4F8.4,4F9.3)
      	DO 8950 J=1,4
8950	BST(J,ICYCL)=B(J)
      	DO 8960 J=1,9
8960	NIQST(J,ICYCL)=NIQ(J)
9000  CONTINUE
      WRITE(6,9502)
      DO 9500 ICYCL=1,NCYCLES
9500  WRITE(6,9501)ICYCL,(BST(I,ICYCL),I=1,4),SUMASQST(ICYCL),
     .			 (NIQST(I,ICYCL),I=1,9)
9501  FORMAT(I3,4F8.4,F9.0,9I4)
9502  FORMAT(' Summary of refinement'/
     .' CYC              SHIFTS             SUMINT IQ 1   2   3',
     .'   4   5   6   7   8   9'/)
C
CHEN
      OPEN(11,FILE='TMP234567.tmp',STATUS='NEW',ERR=9509)
      write(11,'(F12.4,'','',F12.4,'','',F12.4,'','',F12.4)')
     1      AX,AY,BX,BY
      close(11)
      goto 9510
9509  continue
      write(*,'(''ERROR: Could not open TMP234567.tmp.'')')
      STOP
9510  continue
CHEN
C
      STOP
4550	WRITE(6,4551)NMAX
4551	FORMAT(' Too many spots for current prog dimensions',I5)
      STOP
6001	WRITE(6,6002)ISIZEX,ISIZEY,NX,NY
6002	FORMAT(' Image is not of size ISIZEX by ISIZEY',4I8)
      STOP 
      END
C
C*******************************************************************************
C
      SUBROUTINE GRID(AX,AY,BX,BY,ACY,BCY,IXC,IYC,
     .                   IH,IK,XA,YA,NOH,NOK,
     .			 RESMINSQ,RESMAXSQ,
     .                   NXM2,NY2M2,NSPOT,LIST,
     .                   TLTAXIS,SEGMNT,TRNSTEPX)
C
C     subroutine to generate a lattice from 1,0 & 0,1 coordinates
C
      PARAMETER (NMAX=2000)
      DIMENSION IH(NMAX),IK(NMAX),IXC(NMAX),IYC(NMAX),
     1          XA(NMAX),YA(NMAX)
      LOGICAL LIST
      IF(LIST)WRITE(6,10)
   10 FORMAT(/' Lattice generated coordinates'/8X,'H',9X,'K',
     1       7X,'X',9X,'Y'/'0')
      WRITE(6,9001)TRNSTEPX
9001	FORMAT(' STEP SIZE IN TRANSFORM IN X DIRECTION IN A-1',F12.8/)
      TRNSTEPXSQ=TRNSTEPX**2
      NOHD = 2 * NOH + 1
      NOKD = 2 * NOK + 1
      NSPOT = 0
      NOUTSIDE=0
      DO 100 NH=1,NOHD
      DO 100 NK=1,NOKD
      	JH = NH - NOH - 1
      	JK = NK - NOK - 1
      IF(JH.EQ.0.AND.JK.EQ.0)GO TO 100
      	X = JH * AX + JK * BX
      	Y = JH * AY + JK * BY
C
C   YC is Y coord on same scale as X, ie in undistorted transform space 
        YC = JH * ACY + JK * BCY
        ANGLE=ATAN2(YC,X)
      	IF(Y.LT.0.) GO TO 100
C
C	Resolution calculated from X and YC
      DSTARSQ=(X**2+YC**2)*TRNSTEPXSQ
      IF(DSTARSQ.EQ.0.0)GO TO 100
      DSQ=1.0/DSTARSQ
      IF(DSQ.LT.RESMAXSQ.OR.DSQ.GT.RESMINSQ)GO TO 100
C
C	
C	Need angle that is in undistorted transform space
      ANGDIF=ABS(ANGLE-TLTAXIS)*57.29577
50    IF(ANGDIF.GT.180.0) THEN
        ANGDIF=ANGDIF-180.0
        GO TO 50
      ELSE
        IF(ANGDIF.GT.90.0)ANGDIF=180.0-ANGDIF
      ENDIF
      IF(SEGMNT.GE.0.0) THEN
        IF(ANGDIF.GT.SEGMNT) GO TO 100
      ELSE
        IF(90.0-ANGDIF.GT.-SEGMNT) GO TO 100
      ENDIF
C
      	IF(ABS(NINT(X)).GT.NXM2.OR.ABS(NINT(Y)).GT.NY2M2)THEN
      NOUTSIDE=NOUTSIDE+1
       GO TO 100
      END IF
C
C     spot within radius criterion  and within box, and within required segment.
C
      	NSPOT = NSPOT + 1
	IF (NSPOT.GT.NMAX) GO TO 4550
      	IXC(NSPOT) = X + SIGN(0.5,X)
      	IYC(NSPOT) = Y + SIGN(0.5,Y)
      	XA(NSPOT) = X
      	YA(NSPOT) = Y
C
      	IH(NSPOT) = JH
      	IK(NSPOT) = JK
      	IF(LIST)WRITE(6,20) JH,JK,X,Y
C      WRITE(6,*)X,YC
   20 	FORMAT(2I10,2F10.1)
  100 CONTINUE
      WRITE(6,4552)NSPOT
4552  FORMAT('  THERE WERE A TOTAL OF',I5,'  SPOTS GENERATED')
      WRITE(6,4553)NOUTSIDE
4553	FORMAT(' Number in requested resolution range but outside',
     .' area of transform',I5/)
      RETURN
4550	WRITE(6,4551) NMAX
4551	FORMAT(' TOO MANY SPOTS FOR CURRENT PROG DIMENSION',I5)
	STOP
      END
C
C*******************************************************************************
C
      SUBROUTINE AMPHA(IX,IY,APART,BPART,AMP,PHASE,DELPX,DELPY)
C
C     subroutine to translate APART,BPART into amplitude & phase in 
C     degrees and apply origin phase shift to APART,BPART,AMP and PHASE.
C
      PSHIFT = IX * DELPX + IY * DELPY
      IF(IX.LT.0) PSHIFT = - PSHIFT
      C = COS(PSHIFT)
      S = SIN(PSHIFT)
      A = APART * C - BPART * S
      B = APART * S + BPART * C
      IF(IX.LT.0) B = - B
      APART = A
      BPART = B
      AMP = SQRT(A * A + B * B)
      IF(AMP.EQ.0.) THEN
      PHASE = 0.
      ELSE 
      PHASE = ATAN2(B,A) * 57.2958
      END IF
      IF(PHASE.LT.0.) PHASE = PHASE + 360.	! Phase bet 0 and 360 degs.
      RETURN
      END
C*******************************************************************************
C
      SUBROUTINE SPOTS(AX,AY,BX,BY,ACY,BCY,IXC,IYC,IH,IK,XA,YA,
     .			NXM2,NY2M2,NSPOT,LIST)
C
C     subroutine to generate required spots from 1,0 & 0,1 coordinates
C
      PARAMETER (NMAX=2000)
      DIMENSION IH(NMAX),IK(NMAX),IXC(NMAX),IYC(NMAX),
     .		XA(NMAX),YA(NMAX)
      LOGICAL LIST
      IF(LIST)WRITE(6,10)
   10 FORMAT(/' Requested spot coordinates'/8X,'H',9X,'K',
     1       7X,'X',9X,'Y'/'0')
      NSPOT = 0
      DO 100 I=1,NMAX
      	READ(5,*,END=200) JH,JK
      	IF(JH.EQ.100) GO TO 200 
      	X = JH * AX + JK * BX
      	Y = JH * AY + JK * BY
C
C  YC is Y coord on same scale as X; ie in undistorted transform space
C
        YC = JH * ACY + JK * BCY
      	IF(ABS(NINT(X)).GT.NXM2.OR.ABS(NINT(Y)).GT.NY2M2) THEN
      		WRITE(6,23)JH,JK,X,Y,NXM2,NY2M2
23		FORMAT(' Requested spot not inside box,',
     .		' JH,JK,X,Y,NXM2,NY2M2=',2I5,2F10.1,2I6)
      		GO TO 100
      	ENDIF
      	NSPOT = NSPOT + 1
      	IXC(NSPOT) = X + SIGN(0.5,X)
      	IYC(NSPOT) = Y + SIGN(0.5,Y)
      	XA(NSPOT) = X
      	YA(NSPOT) = Y
C
      	IH(NSPOT) = JH
      	IK(NSPOT) = JK
      	IF(LIST)WRITE(6,20) JH,JK,X,Y
   20 	FORMAT(2I10,2F10.1)
  100 CONTINUE
      WRITE(6,22) NMAX
22    FORMAT('  TOO MANY SPOTS FOR PROGRAM DIMENSIONS',I6)
200   CONTINUE
      WRITE(6,21)NSPOT
21    FORMAT(' THERE WERE A TOTAL OF',I5,'  REQUESTED SPOTS IN BOX')
      RETURN
      END
C
C*******************************************************************************
C
      SUBROUTINE RDSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT,
     .		IX,IY,AP,BP,DELPX,DELPY,TURN)
C
C     subroutine to read part of section required plus the extra area needed
C     for the ctf-dependent convolution, then store in array AP,BP for return
C     to main program ---- phases are corrected to desired phase origin.
C
      PARAMETER (INBOXMAX=361)
      DIMENSION ARRAY(2*INBOXMAX,INBOXMAX)	! Square array of complex no's.
      DIMENSION AP(INBOXMAX,INBOXMAX),BP(INBOXMAX,INBOXMAX)
      LOGICAL TURN
      CALL  IRDPAS(1,ARRAY,2*INBOXMAX,INBOXMAX,IX1,IX2,IY1,IY2,*900)
      CALL  IMPOSN(1,0,0)
      KX = IX
      IF(.NOT.TURN) THEN
C
C     straightforward case, no turning
C
      DO 100 K=1,INVERT
      L = 1
      IY = IY + 1
      IX = KX
      DO 100 J=IXOUT1,IXOUT2
      APART = ARRAY(L,K)
      BPART = ARRAY(L+1,K)
      IX = IX + 1
      CALL  AMPHA(IX,IY,APART,BPART,AMP,PHASE,DELPX,DELPY)
      AP(J,K) = APART
      BP(J,K) = BPART
      L = L + 2
  100 CONTINUE
      ELSE
C
C     turn upside down & back to front
C
      KK = INVERT + 1
      DO 200 K=1,INVERT
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
      CALL  AMPHA(IX,IY,APART,BPART,AMP,PHASE,DELPX,DELPY)
      AP(JJ,KK) = APART
      BP(JJ,KK) = BPART
      L = L + 2
  200 CONTINUE
      END IF
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
C*******************************************************************************
C
	SUBROUTINE ANGAVE(N, THETAS, WEIGHTS, THMEAN, THGOOD)
C	Function: to average a set of angles in degrees
C	Created: 27/7/84 by D.J.Thomas
C	Modified:  by R.HENDERSON 20.5.85
	INTEGER*4	N		!number of input angles
	REAL*4   	THETAS(1)	!array of input angles
	REAL*4   	THGOOD		!goodness of average (0 to 1)
	REAL*4   	THMEAN		!weighted average of input angles
	REAL     	COMEAN		!mean value of cosines
	REAL     	SIMEAN		!mean value of sines
	REAL		WEIGHT		!total input weight
	REAL*4		WEIGHTS(1)	!weights on input angles
C
	IF (N .LE. 0) GO TO 20
	WEIGHT = 0.0
	COMEAN = 0.0
	SIMEAN = 0.0
	DO 10 I=1,N
	WEIGHT = WEIGHT + WEIGHTS(I)
	COMEAN = COMEAN + (COS(THETAS(I)*0.01745329252)*WEIGHTS(I))
	SIMEAN = SIMEAN + (SIN(THETAS(I)*0.01745329252)*WEIGHTS(I))
10	CONTINUE
	IF ((SIMEAN .EQ. 0.0) .AND. (COMEAN .EQ. 0.0)) GO TO 20
	THMEAN = 57.295779513*ATAN2(SIMEAN,COMEAN)
      	IF(THMEAN.LT.0.0) THMEAN=THMEAN+360.0
	IF (WEIGHT .EQ. 0.0) GO TO 20
	THGOOD = SQRT((SIMEAN*SIMEAN) + (COMEAN*COMEAN))/WEIGHT
	RETURN
20	THGOOD = 0.0			!average is undefined
	RETURN
	END
C
C*******************************************************************************
C
      SUBROUTINE CONVOLUTE(AP,BP,ACTF,BCTF,AAMP,PPHI,IHOR,IVERT,
     .		ICTFHOR,ICTFVERT)
C
C  Subroutine to perform convolution of raw transform with transform
C   of ctf for tilted image correction.
C
      PARAMETER (IBOXMAX=41,ICTFBXMAX=401,ICTFHALF=200,INBOXMAX=361)
      DIMENSION AAMP(IBOXMAX,IBOXMAX),PPHI(IBOXMAX,IBOXMAX),
     .		ACTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .		BCTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .		AP(INBOXMAX,INBOXMAX),BP(INBOXMAX,INBOXMAX)
c      DIMENSION AT(5,5),PT(5,5)		! TEST DIAGNOSTIC O/P
C
      ICTFHOR2 = ICTFHOR/2
      ICTFVERT2 = ICTFVERT/2
      DO 100 I=1,IHOR
      DO 100 J=1,IVERT
      	A=0.
      	B=0.
      	DO 50 ICTF=-ICTFHOR2,ICTFHOR2
      	DO 50 JCTF=-ICTFVERT2,ICTFVERT2
      	   K=I-ICTF+ICTFHOR2
      	   L=J-JCTF+ICTFVERT2
      	   A = A + AP(K,L)*ACTF(ICTF,JCTF) - BP(K,L)*BCTF(ICTF,JCTF)
	   B = B + AP(K,L)*BCTF(ICTF,JCTF) + BP(K,L)*ACTF(ICTF,JCTF)
50	CONTINUE
      	AMP = SQRT(A*A + B*B)
      	IF(AMP.EQ.0.0) THEN
      		PHASE = 0.
      	ELSE
      		PHASE = ATAN2(B,A) * 57.2958
      	ENDIF
      	IF(PHASE.LT.0.0) PHASE = PHASE + 360.0	! Phase bet 0 and 360 deg.
      	AAMP(I,J) = AMP
      	PPHI(I,J) = PHASE
100   CONTINUE
      RETURN
      END
C
CC*******************************************************************************
CC
C      SUBROUTINE SCSL_CONVOLUTE(AP,BP,ACTF,BCTF,AAMP,PPHI,IHOR,IVERT,
C     .          ICTFHOR,ICTFVERT)
CC
CC  Subroutine to perform convolution of raw transform with transform
CC   of ctf for tilted image correction. (using SCSL)
CC
CC--------------------------ICTFBXMAX has to be 2*ICTFHALF+1 
C      PARAMETER (IBOXMAX=41,ICTFBXMAX=401,ICTFHALF=200,INBOXMAX=361)
C      DIMENSION AAMP(IBOXMAX,IBOXMAX),PPHI(IBOXMAX,IBOXMAX),
C     .          ACTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
C     .          BCTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
C     .          AP(INBOXMAX,INBOXMAX),BP(INBOXMAX,INBOXMAX)
CC
C      COMPLEX P      (IHOR,IVERT)
C      COMPLEX CTF    (ICTFHOR,ICTFVERT)
C      COMPLEX OUTPUT (ICTFHOR+IHOR-1,ICTFVERT+IVERT-1)
C      COMPLEX APLHA,BETA
CC
CC-----Fill complex array p(1:IHOR+ICTFHOR,1:IVERT+ICTFVERT)
CC
C      DO I=1,IHOR
C        DO J=1,IVERT
C          p(I,J)=CMPLX(AP(I,J),BP(I,J))
C        enddo
C      enddo
CC
C      ICTFHOR2 = ICTFHOR/2
C      ICTFVERT2 = ICTFVERT/2
CC
CC-----Fill complex array ctf centered arround ICTFHOR2 from
CC-----arrays that were centered arround origin (-ICTFHOR2:ICTFHOR2,-ICTFVERT2:ICTFVERT2)
CC
C      DO ICTF=-ICTFHOR2,ICTFHOR2
C        DO JCTF=-ICTFVERT2,ICTFVERT2
C          ctf(ICTF+ICTFHOR2+1,JCTF+ICTFVERT2+1)=
C     .       CMPLX(ACTF(ICTF,JCTF),BCTF(ICTF,JCTF))
C        ENDDO
C      ENDDO
CC
C      ALPHA=CMPLX(1,0)
C      BETA=CMPLX(0,0)
CC
CC      CALL TDXCONV(P(1,1),IHOR,IVERT,
C      CALL CONVOLUTE(P(1,1),IHOR,IVERT,
C     .  CTF(1,1), ICTFHOR, ICTFVERT,
C     .  OUTPUT(1,1))
CC
CC      CALL CFIR2D(
CC      CALL TDXCONV(
CC     .       P(1,1),                    1,INBOXMAX,  1,        ICTFHOR+IHOR, 1,         ICTFVERT+IVERT,
CC     .       CTF(-ICTFHOR2,-ICTFVERT2), 1,ICTFBXMAX, 1,        ICTFHOR+1,    1,         ICTFVERT+1,
CC     .       OUTPUT(1,1),               1,INBOXMAX,  2+ICTFHOR,        IHOR, 2+ICTFVERT,         IVERT,
CC     .       ALPHA,BETA)
CC
CC     CALL CFIR2D (x, incx, ldx, i1x0, nx1, i2x0, nx2,
CC                  h, inch, ldh, i1h0, nh1, i2h0, nh2,
CC                  y, incy, ldy, i1y0, ny1, i2y0, ny2,
CC                  alpha, beta)
CC
CC
C      DO I=1,IHOR
C        DO J=1,IVERT
C          K = MOD(I,IHOR-ICTFHOR)+ICTFHOR
C          L = MOD(J,IVERT-ICTFVERT)+ICTFVERT
C          AAMP(I,J) = CABS(OUTPUT(K,L))
C          IF(AAMP(I,J).EQ.0.0) THEN
C            PHASE = 0.
C          ELSE
C            RIMA = AIMAG(OUTPUT(K,L))
C            RREA = REAL (OUTPUT(K,L))
C            PHASE = ATAN2(RIMA,RREA) * 57.2958
C          ENDIF
C          IF(PHASE.LT.0.0) PHASE = PHASE + 360.0        ! Phase bet 0 and 360 deg.
C          PPHI(I,J) = PHASE
C        enddo
C      enddo
CC
C      RETURN
C      END
CC
CC*******************************************************************************
C

      SUBROUTINE CTFGEN(IH,IK,X,Y,RATIOXY,
     .  THETATR,DFMID1,DFMID2,ANGAST,
     .  CS,WL,STEPR,ISIZEX,ISIZEY,DELHEIGHT,
     .  TLTAXIS,TLTANGL,TANTILT,COST,SINT,
     .  ICTFHOR,ICTFVERT,ACTF,BCTF,
     .	ILIST,DFMID,DELCHI,CTFMID,FACTOR,ISENS)
      PARAMETER (ICTFBXMAX=401,ICTFHALF=200)
      DIMENSION ACTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .		BCTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .		 CTF(ICTFBXMAX*ICTFBXMAX)
      LOGICAL ILIST,LIST
      TWOPI = 2.0 * 3.1415926
      YC=Y*RATIOXY
      RAD = SQRT(X**2+YC**2)
      ANGLE=RAD*THETATR
      ANGSPT=ATAN2(YC,X)
      C1=TWOPI*ANGLE*ANGLE/(2.0*WL)
      DELCHI=C1*DELHEIGHT
      SINEWAVES=DELCHI/TWOPI
      	  ICTFHOR=MAX(10,INT(DELCHI))
      	  ICTFHOR=(ICTFHOR/2)*2			    ! ensure ICTFHOR is even.
      	  IF(ICTFHOR.GT.38) ICTFHOR=(ICTFHOR/8)*8   ! ensures prime factor < 19
      	  IF(ICTFHOR.GT.ICTFBXMAX-1) THEN	    ! ensures storage ok.
      		WRITE(6,101)ICTFHOR
101		FORMAT(' Subroutine CTFGEN dimensions too small,',
     .			'  ICTFHOR needs',I8)
      		STOP
      	  ENDIF
      	  ICTFVERT=ICTFHOR
      	  ICTFHOR2=ICTFHOR/2
      	  ICTFVERT2=ICTFVERT/2
      C2=-C1*CS*ANGLE*ANGLE/2.0
      ANGDIF=ANGSPT-ANGAST
      	CCOS=COS(2.0*ANGDIF)
      	CSIN=SIN(2.0*ANGDIF)
      	DFMID=0.5*(DFMID1+DFMID2+CCOS*(DFMID1-DFMID2))
      	CTFMID=-SIN(C1*DFMID+C2)
      	SUMC=0.0
      DO 100 I=1,ICTFHOR
      DO 100 J=1,ICTFVERT
      	ISTORE=I+(ICTFHOR+2)*(J-1)	! indexing for array CTF.
C	  Calculate height of this element of image.
      	XP=((I-0.5-ICTFHOR2)/(ICTFHOR))*ISIZEX*STEPR! 0.5 rounding error
      	YP=((J-0.5-ICTFVERT2)/(ICTFVERT))*ISIZEY*STEPR! 0.5 rounding error
      	DF = DFMID + TANTILT*(-XP*SINT+YP*COST)
      	CHI=C1*DF+C2
      	CNTRST=-SIN(CHI)
      	SUMC=SUMC+ABS(CNTRST)
100   CTF(ISTORE)=CNTRST
C
C  Now rescale so that the same power is present in the spot after convolution.
      RESCALE=ICTFHOR/SUMC
      FACTOR =ICTFHOR*ICTFVERT/SUMC
      DO 120 I=1,ICTFHOR
      DO 120 J=1,ICTFVERT
      	ISTORE=I+(ICTFHOR+2)*(J-1)	! indexing for array CTF.
120   CTF(ISTORE)=RESCALE*CTF(ISTORE)
C
CHEN
      CALL TDXFFT(CTF,ICTFHOR,ICTFVERT,0)
C-----CALL TODFFT(CTF,ICTFHOR,ICTFVERT,0)
CHEN
C
C  Here to transfer the transform of ctf to ACTF,BCTF. At the same time,
C     move phase origin for CTF to middle of image, as is presumed to be
C     the case for the requested phase origin in input XORIG, YORIG.
C
C  Now calculate correct phase shift for slightly offset origin for the
C     contrast distribution function.
      PHSHFT = -TWOPI*(ICTFHOR2-0.5)/ICTFHOR
C
      	DO 150 I=1,ICTFHOR2+1
      	DO 150 J=1,ICTFVERT
      	ISTORE = (2*I-1)+(ICTFHOR+2)*(J-1)
      	IF(J.LE.1+ICTFVERT2) THEN
      		IX=I-1
      		IY=J-1
      		C=COS((IX+IY)*PHSHFT)	! assumes ICTFHOR=ICTFVERT
      		S=SIN((IX+IY)*PHSHFT)	! assumes ICTFHOR=ICTFVERT
      		A = CTF(ISTORE)
      		B = CTF(ISTORE+1)
      		ACTF(IX,IY) = A*C-B*S
      		BCTF(IX,IY) = A*S+B*C
      	ENDIF
      	IF(J.GE.1+ICTFVERT2) THEN
      		IX=I-1
      		IY=J-1-ICTFVERT
      		C=COS((IX+IY)*PHSHFT)	! assumes ICTFHOR=ICTFVERT
      		S=SIN((IX+IY)*PHSHFT)	! assumes ICTFHOR=ICTFVERT
      		A = CTF(ISTORE)
      		B = CTF(ISTORE+1)
      		ACTF(IX,IY) = A*C-B*S
      		BCTF(IX,IY) = A*S+B*C
      	ENDIF
150	CONTINUE
      	DO 160 I= 1,ICTFHOR2
      	DO 160 J=-ICTFVERT2,ICTFVERT2
      		ACTF(-I,-J) =  ACTF(I,J)
      		BCTF(-I,-J) = -BCTF(I,J)
160	CONTINUE
C
      IF(ILIST) WRITE(6,103)DELHEIGHT,DFMID,SINEWAVES,
     .		DELCHI,ICTFHOR,CTFMID,FACTOR
103   FORMAT(' Contrast transfer function description -----',
     .	'-------- Height difference ===================',F10.1/
     .	54X,'Midpoint defocus ====================',F10.1/
     .	54X,'Number ctf cycles ===================',F10.3/
     .	54X,'Number ctf samples needed (used)=====',F10.1,'(',I2,')'/
     .	54X,'Midpoint C.T.F. =====================',F10.4/
     .	54X,'Rescaling factor (keeps noise const)=',F10.4)
      RETURN
      END
C
