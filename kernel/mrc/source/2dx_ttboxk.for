C*******************************************************************************
C
C TTBOXK : prints out amplitudes & phases in N * N boxes from a 
C          Fourier transform, fully corrected for contrast transfer function
C          in tilted image.
C
C          Version  1.01    9 Dec 1985  RH
C                   1.02   14 Feb 1986  RH  SEGMNT option.
C                   1.03   18 Nov 1987  JMB Rectangular images
C                   1.04   12 Jul 1988  RH  Newstyle plot output & IQ=9 added.
C                   1.05   10 Sep 1988  RH  Increase TITLE to 68 bytes.
C                   1.06    2 Jan 1989  RH  ICTFBXMAX upped to 120,NMAX to 2000.
C                   1.07   24 Jan 1989  RH  IQ=8 now plotted as a small dot.
C                   2.00    3 Jan 1992  RH  Convert to UNIX for Alliant
C                   2.01    6 Oct 1992  RH  Extra column on output - dummy
C                   2.02   21 Mar 1993  RH  minor bug pi=3.1415926
C                   3.00   24 Sep 1999  RH  changed background definition -> TTBOXA
C                   4.00   28 Aug 2000  RH  convert to plot2000 (postscript O/P)
C                   4.01   13 Jun 2001  TSH P2K_FONT needed string terminator
C                   4.02   29 Oct 2001  RH  change filenames to CHARACTER*80
C                   5.00   30 Oct 2005  HS  2dx
C
C  The change to TTBOXA on 24.9.99 was made to eliminate the effect that has
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
C  TTBOX on full-size unboxed images.
C
C
C     DATACARDS ----------------------------------------------
C
C  1.  FILIN -- full name of input file (.FFT)
C
C  2.  ISER,TITLE (I10,17A4)
C
C  3.  GENGRID   (A)
C
C  4.  GENPTS    (A)
C
C  4a.  LISTPTS   (A)
C  4b.  PLOTPTS   (A)
C
C  5.  ISIZEX,ISIZEY,DSTEP,XMAG,CS,KVOLT   (*)
C
C  6.  DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL   (*)
C
C  7.  IOUT,NUMSPOT, NOH, NOK, NHOR, NVERT  (*)
C
C  8.  FILOUT only if IOUT.NE.0 -- full name of output file, formatted data.
C  8b. FILOU2 only if IOUT.EQ.2 .or. IOUT.EQ.3 -- full name of second output file.
C  8c. RESORMIN only if IOUT.EQ.3 -- Minimial resolution perpendicular to tilt axis.
C      (This is not yet implemented.)
C
C  9    RESMIN, RESMAX,  XORIG, YORIG, SEGMNT  (*)
C
C  10.  if GENGRID or GENPTS :          AX, AY, BX, BY    (*)
C
C  11.  if GENPTS                       : IH(I), IK(I)              (*)
C       if .not.GENGRID and .not.GENPTS : IH(I), IK(I), X(I), Y(I)  (*)
C
C
C     ISER        serial number for run to be printed & output on IOUT.
C     TITLE       title to be printed & output on unit IOUT.
C     GENGRID     if YES generate grid from lattice points (1,0) & (0,1).
C     GENPTS      if YES individual spots requested & generated from grid
C                 if NO all spots within RESMIN to RESMAX generated.
C     LISTPTS     if YES list spots whose amps and phases will be output.
C     PLOTPTS     if YES plot spots with IQ<8 using symbol size propnl to 8-IQ
C     ISIZEX,Y    size of image in x and y, checked against file-header.
C     DSTEP       densitometer stepsize in microns.
C     XMAG        magnification of micrograph.
C     CS          spherical aberration coefficient in mm.
C     KVOLT       microscope voltage in KV, used to calculate wavelength. 
C     DFMID1      defocus in one direction (underfocus +ve) 
C     DFMID2      defocus at 90-degs to above
C     ANGAST      direction for DFMID1 in degrees relative to x,y in transform.
C     TLTAXIS     direction of tiltaxis in degrees relative to x,y in transform,
C                   should be between -90 and +90 degrees.
C     TLTANGL     magnitude of tiltangle.
C                       (+ve for less underfocus at start of scan(y=0)).
C                       if tiltaxis is precisely parallel to y, then TLTANGL
C                       should be positive for less underfocus at x=0.
C     IOUT        output unit number for serial number and title, followed by
C                  IH,IK,A,P,IQ terminated with IH=100.
C     NUMSPOT     number of spots to be printed.
C     NOH, NOK    number of orders of spots in H & K directions to be generated.
C     NHOR, NVERT box size in grid units in horizontal & vertical directions,
C                 i.e. X & Y resp. ( up to 20 grid units in each
C                 direction).
C     RESMIN, RESMAX inner & outer resolution limits in Angstroms within which
C                        spots(centre of box) must fall.
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
C remember to change IBOXMAX in subroutine 
      PARAMETER (NMAX=20100)
      PARAMETER (IBOXMAX=41)
      PARAMETER (ICTFBXMAX=401)
      PARAMETER (INBOXMAX=361)
      REAL KVOLT
CHEN>
      REAL*8 NEWMAX,AMPTOTAL,AMPTOT,RMSBK,RMSBKOLD,AMPSQ,AMPINT
      INTEGER*8 ISUM(IBOXMAX,IBOXMAX)
      REAL*8 RSUM(IBOXMAX,IBOXMAX)
      INTEGER*8 ISUMI(IBOXMAX,IBOXMAX)
      INTEGER*8 IAMP(IBOXMAX,IBOXMAX)
      INTEGER*8 IPERIM,ICENTRE,INEAR
      INTEGER*8 ISER
CHEN<
      DIMENSION XA(NMAX),YA(NMAX),IXC(NMAX),IYC(NMAX),IH(NMAX),IK(NMAX)
      DIMENSION IPHI(IBOXMAX,IBOXMAX),
     .  AAMP(IBOXMAX,IBOXMAX),PPHI(IBOXMAX,IBOXMAX),
     .  IXGU(IBOXMAX),IYGU(IBOXMAX),
     .  RSUMI(IBOXMAX,IBOXMAX)
      DIMENSION AP(INBOXMAX,INBOXMAX),BP(INBOXMAX,INBOXMAX)
      DIMENSION ACTF(ICTFBXMAX,ICTFBXMAX),BCTF(ICTFBXMAX,ICTFBXMAX)
      DIMENSION TITLE(17),NXYZ(3),MXYZ(3),PHANG(4),WTS(4),
     .  DELX(2),DELY(2),NIQ(9)
C      BYTE GENGRID,GENPTS,LISTPTS,PLOTPTS,YES
      CHARACTER GENGRID,GENPTS,LISTPTS,PLOTPTS,YES
      LOGICAL TURN,ILIST,LIST
      CHARACTER*80 FILIN,FILOUT,FILOU2,CZEILE,CNAMPAT
      INTEGER IOUT
      COMMON NX,NY,NZ
      EQUIVALENCE (NXYZ,NX)
      DATA YES/'Y'/,NIQ/9*0/,TWOPI/6.2831853/,DUMMY/0.0/
      DATA ILIST/.TRUE./,LIST/.FALSE./
C
      NSPOTS = 0
C
      WRITE(6,1000)
 1000 FORMAT('0',' TTBOX V5.00(30-Oct-2005) : prints amplitudes',
     1' & phases from spots on transform corrected for c.t.f.')
      READ(5,1005) FILIN
 1005 FORMAT(A)
      CALL  IMOPEN(1,FILIN,'RO')
      CALL  IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      CALL  IRTORG(1,XOR,YOR,ZOR)
      WRITE(6,1010) XOR,YOR,ZOR
 1010 FORMAT(/,' X & Y phase origin shift read from transform',
     1           3F10.2)
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
 1025 FORMAT(/,' Serial number :',I10,/,' Title :',17A4)
 1028 FORMAT(A)
1029    FORMAT(1X,20A1)
      READ(5,1028) GENGRID
      WRITE(6,1029) GENGRID
      READ(5,1028) GENPTS
      WRITE(6,1029) GENPTS
      READ(5,1028) LISTPTS
      WRITE(6,1029) LISTPTS
      READ(5,1028) PLOTPTS
      WRITE(6,1029) PLOTPTS
      IF(LISTPTS.EQ.YES) LIST=.TRUE.
C
C  Input of all c.t.f. and tilt data for image, calculation of all the needed
C      preliminaries except for calculation of c.t.f. itself.
C
      READ(5,*) ISIZEX,ISIZEY,DSTEP,XMAG,CS,KVOLT
        IF(ISIZEY.NE.NY)     GO TO 6001
        IF(ISIZEX.NE.NXP2-2) GO TO 6001
      READ(5,*) DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
98      IF(TLTAXIS.LE.-90.0) THEN
                TLTAXIS=TLTAXIS+180.0
                GO TO 98
        ENDIF
99      IF(TLTAXIS.GT.90.0) THEN
                TLTAXIS=TLTAXIS-180.0
                GO TO 99
        ENDIF

      WRITE(6,101) ISIZEX,ISIZEY,DSTEP,XMAG,CS,KVOLT
      WRITE(6,102) DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
101   FORMAT(/,' SIZE OF DENSITOMETERED ARRAY; X,Y...',2I7,/,
     .       ' DENSITOMETERED STEPSIZE(MICRONS)....',F10.2,/,
     .       ' MAGNIFICATION OF MICROGRAPH.........',F8.0,/,
     .       ' SPHERICAL ABERRATION (MM) ..........',F10.2,/,
     .       ' ACCELERATING VOLTAGE (KV) ..........',F8.0)
102   FORMAT(' UNDERFOCUS 1 .......................',F8.0,/,
     .       ' UNDERFOCUS 2 .......................',F8.0,/,
     .       ' DIRECTION FOR UNDERFOCUS 1 .........',F9.1,/,
     .       ' TILT AXIS DIRECTION ................',F9.1,/,
     .       ' TILT ANGLE .........................',F9.1,
     .  ' +VE FOR LESS UNDERFOCUS AT START OF SCAN(Y=0)',/)
CHEN
      ORIANG=ANGAST
      ORITAX=TLTAXIS
      ORITAN=TLTANGL
CHEN
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
C       Calculate height difference across image
C
        PERP=ISIZEY*COST+ISIZEX*(ABS(SINT))
C      WRITE(6,91701)STEPR,TANTILT,COST,SINT,ISIZEX,ISIZEY
91701   FORMAT(4F10.5,2I10)
        DELHEIGHT=ABS(STEPR*PERP*TANTILT)
      WRITE(6,91700)PERP,DELHEIGHT
91700   FORMAT(' PERPENDICULAR DISTANCE FROM CORNER OF FILM TO TILT',
     .' AXIS',F10.3,/,' DIFFERENCE IN HEIGHT OF FAR CORNERS OF IMAGE',
     .F10.3,/)
C
      READ(5,*) IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
      IF(NUMSPOT.EQ.0) ILIST=.FALSE.
C
      IF(IOUT.NE.0) THEN
        READ(5,1005) FILOUT
        OPEN(UNIT=2,FILE=FILOUT,STATUS='NEW')
      END IF
C
CHEN
      IF(IOUT.EQ.2 .or. IOUT.EQ.3) THEN
        IOU2=11
        READ(5,1005) FILOU2
        WRITE(*,'('' '')')
        WRITE(*,'('' # type in name pattern'')')
        WRITE(*,'('' '')')
        READ(5,1005) CNAMPAT
        WRITE(6,'(A)') CNAMPAT
        OPEN(UNIT=IOU2,FILE=FILOU2,STATUS='NEW')
      END IF
C
      if(IOUT.eq.3)then
        write(6,'('' Input RESORMIN (minimal resolution '',
     .    ''orthogonal to tilt axis)'')')
        read(5,*) RESORMIN
        write(6,'('' Read: '',F12.3)')RESORMIN
      endif
CHEN
C
      READ(5,*) RESMIN,RESMAX,XORIG,YORIG,SEGMNT
      IF(RESMIN.LT.RESMAX) THEN
        R=RESMIN        ! Reverse if input in wrong order !
        RESMIN=RESMAX
        RESMAX=R
      ENDIF
      RESMINSQ=RESMIN**2
      RESMAXSQ=RESMAX**2
      WRITE(6,1030) IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT,
     1              RESMIN,RESMAX,
     .              XORIG,YORIG,SEGMNT
1030  FORMAT(/,' output unit number ==========================',I5,/,
     1        ' number of spots printed =====================',I5,/,
     2        ' No. of orders in h & k=======================',2I5,/,
     3        ' No. of points in box horiz & vert direction =',2I5,/,
     4        ' Inner & outer resolution limits (Angstroms)==',2F8.1,/,
     5        ' X & Y phase origin shifts ===================',2F8.1,/,
     6        ' SEGMNT for o/p of spots within segment ======',F8.1)
      PI = 3.141592654
      TWOPI = 2. * PI
      DELPX = -TWOPI * (XOR + XORIG) / (2. * (NXM1))
      DELPY = -TWOPI * (YOR + YORIG) / NY
C
C
      DRAD=PI/180.0
      IF(NHOR.GT.IBOXMAX) NHOR = IBOXMAX
      IF(NVERT.GT.IBOXMAX) NVERT = IBOXMAX  
      TURN = .FALSE.  ! Specifies whether desired spot comes from negative X.
      I = 0
C
      IF(GENGRID.EQ.YES.OR.GENPTS.EQ.YES) THEN
          READ(5,*) AX,AY,BX,BY
          WRITE(6,1050) AX,AY,BX,BY
 1050     FORMAT(/,' coordinates of 1,0 & 0,1 ',4F10.3)
C
C         point generation required
C
      SIZEX=ISIZEX
      RATIOXY=SIZEX/ISIZEY     
      ACY=AY*RATIOXY
      BCY=BY*RATIOXY
      WRITE(6,91050)AX,ACY,BX,BCY
91050   FORMAT(' coordinates of 1,0 & 0,1 with y components scaled',
     .' by factor ISIZEX/ISIZEY',/,26X,4F10.3,/)
C 
          IF(GENPTS.EQ.YES) THEN
            CALL SPOTS(AX,AY,BX,BY,ACY,BCY,IXC,IYC,IH,IK,XA,YA,
     .  NXM2,NY2M2,NSPOT,LIST)
          ELSE
C
C           grid generation required
C
            CALL GRID(AX,AY,BX,BY,ACY,BCY,IXC,IYC,
     .                   IH,IK,XA,YA,NOH,NOK,
     .                   RESMINSQ,RESMAXSQ,
     .                   NXM2,NY2M2,NSPOT,LIST,
     .                   TLTAXIS,SEGMNT,TRNSTEPX)
          END IF
C
C         read in individual points
C
      ELSE
          WRITE(6,1060)
 1060     FORMAT(/,' Coordinates read in    H    K       X         Y'
     1              ,' nearest grid pt X      Y',/,'0')  
  110     I = I + 1
          IF(NSPOT.GE.NMAX) GO TO 4550
  115     READ(5,*,END=120) IH(I),IK(I),XA(I),YA(I)
          IF(IH(I).EQ.100) GO TO 120
          IXC(I) = XA(I) + SIGN(0.5,XA(I))
          IYC(I) = YA(I) + SIGN(0.5,YA(I))
C     
C         reject spot if outside boundary
C
          IF(IXC(I).GT.NXM2) GO TO 115
          IF(IXC(I).LT.-NXM2) GO TO 115
          IF(IYC(I).GT.NY2M2) GO TO 115
          IF(IYC(I).LT.-NY2M2) GO TO 115
          WRITE(6,1070) IH(I),IK(I),XA(I),YA(I),IXC(I),IYC(I)
 1070     FORMAT(20X,2I5,2F10.1,12X,I5,2X,I5)
          NSPOT = I
          GO TO 110
      END IF
C
C     data read in proceed
C
  120 IHOR2 = NHOR / 2
      IVERT2 = NVERT / 2
C
C     make sure odd number of elements in box
C
      NHOR = IHOR2 * 2 + 1
      NVERT = IVERT2 * 2 + 1
      WRITE(6,1080) NHOR,NVERT
 1080 FORMAT(/,' Box size in transform grid units :',I3,' *',I3)
      IF(IOUT.NE.0) WRITE(2,1081)ISER,TITLE
1081  FORMAT(I10,17A4)
      DO 130 K=1,IBOXMAX
        DO 130 J=1,IBOXMAX
          ISUM(J,K) = 0
          RSUM(J,K) = 0.0
          ISUMI(J,K) = 0
CHEN
          RSUMI(J,K) = 0.0
CHEN
  130 CONTINUE
      SUMRMSBKOLD=0.0
      SUMRMSBKNEW=0.0
C
      IF(PLOTPTS.EQ.YES) THEN           ! Plot initialisation.
C        write(6,'('' Hier 1'')')
        CALL TTPLOT(0,0,IQ,AX,BX,AY,BY,
     .    TLTAXIS,TRNSTEPX,RATIOXY,TITLE,RESMIN,RESMAX,
     .    NSPOTS,ORIANG,ORITAX,ORITAN,
     .    DSTEP,DFMID1,DFMID2,ANGAST,XMAG,NHOR,NVERT)
C        write(6,'('' Hier 2'')')
      ENDIF
C      write(6,'('' Hier 3'')')
      WRITE(6,1103)             !  Asterisks to mark beginning of output.
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
C               INHOR  = IHOR  + ICTFHOR  (- 1)
C               INVERT = IVERT + ICTFVERT (- 1)
C
      CALL CTFGEN(IH(I),IK(I),XA(I),YA(I),RATIOXY,
     .    THETATR,DFMID1,DFMID2,ANGAST,
     .    CS,WL,STEPR,ISIZEX,ISIZEY,DELHEIGHT,
     .    TLTAXIS,TLTANGL,TANTILT,COST,SINT,
     .    ICTFHOR,ICTFVERT,ACTF,BCTF,
     .    ILIST,DFMID,DELCHI,CTFMID,FACTOR,ISENS)
C
      INHOR  = IHOR  + ICTFHOR          ! ODD = ODD +EVEN
      INVERT = IVERT + ICTFVERT         ! ODD = ODD +EVEN
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
160     if(ILIST)WRITE(6,161) IH(I),IK(I)
161     FORMAT(' SPOT TOO NEAR EDGE FOR CTF TILT CORRECTION',2I8)
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
C        write(6,'(''+ve IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT:'',7I10)')IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT
        CALL  RDSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT,
     .  IX,IY,AP,BP,DELPX,DELPY,TURN)
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
C        write(6,'(''-ve IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT:'',7I10)')IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT
        CALL  RDSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT,
     .          IX,IY,AP,BP,DELPX,DELPY,TURN)
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
C        write(6,'(''LHS: IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT:'',7I10)'),IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT
        CALL  RDSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT,
     .  IX,IY,AP,BP,DELPX,DELPY,TURN)
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
C        write(6,'(''RHS: IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT:'',7I10)'),IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT
        CALL  RDSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT,
     .  IX,IY,AP,BP,DELPX,DELPY,TURN)
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
CHENN>
C new convolution routine using scsl
C      CALL SCSL_CONVOLUTE(AP,BP,ACTF,BCTF,AAMP,PPHI,IHOR,IVERT,
C     .  ICTFHOR,ICTFVERT)
C------Calling FFTW here not yet debugged.
C------We have to fix this later.
CHENN<
C
      CALL CONVOLUTE(AP,BP,ACTF,BCTF,AAMP,PPHI,IHOR,IVERT,
     .  ICTFHOR,ICTFVERT)
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
      DO K=1,IVERT,KV
        DO J=1,IHOR
          F = AAMP(J,K)
          AMPSQ = AMPSQ + F * F
        enddo
      enddo
      DO K=2,KV
        DO J=1,IHOR,KH
          F = AAMP(J,K)
          AMPSQ = AMPSQ + F * F
        enddo
      enddo
      AMPTOT = AMPSQ / (2*(IHOR + IVERT - 2))
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
      DO K=K1,K2
        DO J=J1,J2
          F = AAMP(J,K)
          AMPSQ = AMPSQ + F * F
        enddo
      enddo
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
      DELX(2) = 1. - DELX(1)
      YSCALE = YA(I)
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
C      write(6,'('' INTYA,IYL,I,YA(I)='',3I12.F12.3)')INTYA,IYL,I,YA(I)
      DO L2 = 1,2       ! Vector phases over 2x2 points only.
        K = K1 + L2 - 1
        DO L1 = 1,2     !
          J = J1 + L1 - 1
C         write(6,'('' j,k,INTYA,IYL='',4I12)')j,k,INTYA,IYL
          AMP = AAMP(J,K)
          PHASE = PPHI(J,K) / 57.2958
          ASUM1 = ASUM1 + AMP * COS(PHASE)
          BSUM1 = BSUM1 + AMP * SIN(PHASE)
C
C         calculated sinc function weighted phase
C
          IF(DELX(L1).EQ.0) GO TO 315
            IF(DELY(L2).EQ.0) THEN
              SINC = (SIN(PI * DELX(L1))) / (PI * DELX(L1))
            ELSE
              SINC = (SIN(PI * DELX(L1)) * SIN(PI * DELY(L2))) /
     .                          (PI**2 * DELX(L1) * DELY(L2))
            END IF
            GOTO 318
C
  315     IF(DELY(L2).EQ.0) THEN
            SINC = 1.
          ELSE
            SINC = (SIN(PI * DELY(L2))) / (PI * DELY(L2))
          END IF
C
  318     ASUM2 = ASUM2 + AMP * COS(PHASE) * SINC
          BSUM2 = BSUM2 + AMP * SIN(PHASE) * SINC
          DENOM = DENOM + SINC**2
        enddo
      enddo
C
C changes to calculate better background using same algorithm as peak
      DX=DELX(1)
      DY=DELY(1)
      CALL GET_RMSBK(AAMP,PPHI,DX,DY,IVERT,IHOR,2,RMSBK)
      SUMRMSBKOLD=SUMRMSBKOLD+RMSBKOLD
      SUMRMSBKNEW=SUMRMSBKNEW+RMSBK
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
        IF(AMPOUT.EQ.0.00001)IQ=9
        AMPTOTAL = AMPTOTAL + AMPOUT
C
C     sum squared amplitudes
C
      DO K=1,IVERT
        DO J=1,IHOR
          IAMP(J,K) = AAMP(J,K) + 0.5
          IPHI(J,K) = PPHI(J,K) + 0.5
          ISUM(J,K) = ISUM(J,K) + IAMP(J,K) * IAMP(J,K)
CMAR>
C like in mmbox 
          RSUM(J,K) = RSUM(J,K) + FLOAT(IAMP(J,K))**2
CMAR<
        enddo
      enddo 
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
C     write output to unit IOUT
C
      IF(IOUT.NE.0) THEN
        WRITE(2,1107)IH(I),IK(I),AMPOUT,PHSOUT,
     .    IQ,RMSBK,DUMMY
CHEN> 
C 1107    FORMAT(2I5,2F12.1,I5,2F12.1)
 1107    FORMAT(2I5,2G16.6,I5,2G16.6)
CHEN<
      ENDIF
      IF(PLOTPTS.EQ.YES) THEN           ! Plot spot
        CALL TTPLOT(IH(I),IK(I),IQ,AX,BX,AY,BY,
     .    TLTAXIS,TRNSTEPX,RATIOXY,TITLE,RESMIN,RESMAX,
     .    NSPOTS,ORIANG,ORITAX,ORITAN,
     .    DSTEP,DFMID1,DFMID2,ANGAST,XMAG,NHOR,NVERT)
      ENDIF
      IF(IQ.LE.7) NGOOD=NGOOD+1
      IF(IQ.GT.7) NBAD=NBAD+1
      NIQ(IQ) = NIQ(IQ)+1
C
C     set up pagination
C
      NUMOUT = NUMOUT + 1
          IF(NUMOUT.GE.NUMSPOT) ILIST=.FALSE.
          IF(NUMOUT.EQ.NUMSPOT+1) THEN
                if(ILIST)WRITE(6,1103)
                if(ILIST)WRITE(6,1102)
          ENDIF
1102      FORMAT(/,' OTHER SPOTS NOT PRINTED OUT WITH FULL ',
     .       'DIAGNOSTICS')
1108      FORMAT('    H    K       AMPOUT  PHSOUT IQ       ',
     .       'RMSBK     DFMID    ',
     .       'NCTFSAMPLES    CTFINMIDDLE   RESCALING BY')
          IF(NUMOUT.GT.NUMSPOT) THEN
           NUMAFTER=NUMOUT-NUMSPOT-1    ! Test for table heading output.
           IF(60*((NUMAFTER)/60).EQ.NUMAFTER) then
             if(ILIST)WRITE(6,1108)
           endif
C
CHEN
           CZEILE(1:24)='8877665544332211'
           ITEMP=19-IQ-IQ
           CZEILE(ITEMP:24) = ' '
           ITEMP=19-IQ-IQ-2
           IF(ITEMP.GE.1)CZEILE(1:ITEMP) = '------------------'
C
           if(ILIST)WRITE(6,1101)IH(I),IK(I),ISENS,AMPOUT,PHSOUT,IQ,RMSBK,
     .     DFMID,DELCHI,ICTFHOR,CTFMID,FACTOR,CZEILE(1:17)
CHEN
          ENDIF
1101    FORMAT(2I5,A1,F12.1,F8.1,I3,F12.1,F10.1,F11.2,
     .  '(',I3,')',F11.4,F9.3,'  ',A17)
      IF(NUMOUT.GT.NUMSPOT) GO TO 500
C
C     write up to NUMSPOT spots
C
      if(ILIST)WRITE(6,1105) IH(I),IK(I),XA(I),YA(I)
1103  FORMAT(/,/,132('*'),/)
1105  FORMAT(/,' Reflection  H',I3,'  K',I3,10X,
     1       'Lattice coordinates in grid units ',2F8.2)
C
      WRITE(6,1110) RMSBK, AMPINT, VECPHA1, VECPHA2, PINTP, GDMEAN
1110  FORMAT(/,' RMS backgd =',F6.1,
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
 1140 FORMAT(/,'  X(grid units)',41I5)
      WRITE(6,1150) (IXGU(J),J=1,IHOR)
 1150 FORMAT('+',67X,'X(grid units)',10I5)
      WRITE(6,1160)
 1160 FORMAT(/,'  Y(grid units)',53X,'Y(grid units)')
      L = IVERT
      DO 390 K=1,IVERT
      WRITE(6,1170) IYGU(L),(IAMP(J,L),J=1,IHOR)
 1170 FORMAT(/,6X,I5,4X,41I5)
      WRITE(6,1180) IYGU(L),(IPHI(J,L),J=1,IHOR)
 1180 FORMAT('+',72X,I5,4X,10I5)
      L = L - 1
  390 CONTINUE
      IF(ILIST)WRITE(6,1103)    ! Asterisks.
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
      IF(ILIST)WRITE(6,1103)    !   Asterisks
  500 CONTINUE
C
      IF(SUMRMSBKOLD.GT.0.) WRITE(6,501) SUMRMSBKNEW/SUMRMSBKOLD
501   FORMAT(/,' Ratio (overall average) of new to old way of ',
     .  'calculating the RMS background for denominator of IQ value ',
     .  'calculation was',F10.4,/,' It varies with inter-pixel ',
     .  'correlation which in turn depends on boxing of image')
C
        IF(PLOTPTS.EQ.YES) THEN         ! terminate plot
        CALL TTPLOT(999,999,IQ,AX,BX,AY,BY,
     .    TLTAXIS,TRNSTEPX,RATIOXY,TITLE,RESMIN,RESMAX,
     .    NSPOTS,ORIANG,ORITAX,ORITAN,
     .    DSTEP,DFMID1,DFMID2,ANGAST,XMAG,NHOR,NVERT)
        ENDIF
C
C     write summed,squared amplitudes
C
      PERIM = 0.0
      DO 530 K=1,IVERT
530   PERIM=PERIM+RSUM(1,K)+RSUM(IHOR,K)
      DO 535 J=2,IHOR-1
535   PERIM=PERIM+RSUM(J,1)+RSUM(J,IVERT)
      PERIM=PERIM/(2.0*(IVERT+IHOR-2))
C
      DO 520 K=1,IVERT
        DO 520 J=1,IHOR
          ISUMI(J,K)=ISUM(J,K)*7.0/PERIM + 0.5
CHEN
          RSUMI(J,K)=RSUM(J,K)*7.0/PERIM + 0.5
CHEN
          if(NSPOT.ne.0)then
            ISUM(J,K) = SQRT(FLOAT(ISUM(J,K)) /(NSPOT)) + 0.5
          else
            ISUM(J,K) = 0
          endif
  520 CONTINUE
C
      WRITE(6,1250)     ! Amplitude output
 1250 FORMAT(/,/,132('*'),/,/,1X,'SQRT of summed,squared amplitudes',
     .  /,1X,33('-'),/)
C
CHEN>
      if(NSPOT.ne.0)then
        RAVER = AMPTOTAL/NSPOT
      else
        RAVER = 0.0
      endif
      IDIVISOR=1
      if(RAVER.gt.    200)IDIVISOR=10
      if(RAVER.gt.   2000)IDIVISOR=100
      if(RAVER.gt.  20000)IDIVISOR=1000
      if(RAVER.gt. 200000)IDIVISOR=10000
      if(RAVER.gt.2000000)IDIVISOR=100000
      if(IDIVISOR.gt.1)then
        write(6,'('' Values to be multiplied by '',I6,/)')IDIVISOR
      endif
C
      L = IVERT
      DO 540 K=1,IVERT
        WRITE(6,1270) (ISUM(J,L)/IDIVISOR,J=1,IHOR)
 1270   FORMAT(/,1X,41I6)
        L = L - 1
  540 CONTINUE
      WRITE(6,1280) RAVER
CHEN<
1280  FORMAT(/,19X,'overall average amplitude =',F10.3)
      WRITE(6,1252)
C
      SCALEFAC = 7.0/PERIM
      WRITE(6,1251) SCALEFAC            ! Intensity output
 1251 FORMAT(/,/,132('*'),/,/,1X,'scaled intensities (perimeter',
     .  ' averaged to 7.0)',
     .  '   scale factor = ',F12.7,/,1X,40('-'))
C
C  Here calculate how close the intensity average approaches theoretical.
C
      ICENTRE=ISUMI(IHOR2+1,IVERT2+1)
      INEAR=ISUMI(IHOR2+1,IVERT2) + ISUMI(IHOR2+1,IVERT2+2) +
     .  ISUMI(IHOR2,IVERT2+1) + ISUMI(IHOR2+2,IVERT2+1)
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
CHEN----
C
C  Here calculate the maximum of the RSUMI array in REAL
C
      RSCAMAX=0.0
      L = IVERT
      DO 545 K=1,IVERT
        WRITE(6,1270) (ISUMI(J,L),J=1,IHOR)
        DO 544 ITEMP1=1,NHOR
           IF(RSUMI(ITEMP1,L).GT.RSCAMAX)RSCAMAX=RSUMI(ITEMP1,L)
  544   CONTINUE
        L = L - 1
  545 CONTINUE
      WRITE(6,1254)PERCENT
1254  FORMAT(/,19X,'above indicates shape that is',F7.1,' % perfect')
      IF(PERCENT.GE.85.0) WRITE(6,1255)
      IF(PERCENT.GE.50.0.AND.PERCENT.LT.85.) WRITE(6,1256)
      IF(PERCENT.LT.50.0) WRITE(6,1257)
1255  FORMAT(19X,'this is quite satisfactory, well done!!')
1256  FORMAT(19X,'this is not bad, but could be improved!')
1257  FORMAT(19x,'this is not good enough, you must try harder!')
      WRITE(6,1252)
1252  FORMAT(/,/)
      WRITE(6,1253) NUMOUT,NGOOD,NBAD,(J,NIQ(J),J=1,9)
1253  FORMAT(I10,'  Total spots found',/,
     .  I10,'  Good spots for output',/,I10,'  Bad spots not used',/,
     .  '    IQ    NUMBER',/,9(I6,I10,/))
      IF(IOUT.NE.0)WRITE(6,1258)NGOOD,FILOUT,NBAD
1258  FORMAT(' List of',I6,'  good spots written to file   ',A40,/,
     .' with   ',I6,'  bad(IQ=9) spots also included',/)
C
CHEN
      IF(IOU2.NE.0)THEN
C NEWMAX angepasst von Paul und Andreas am 21.12.2000
        ILSUM=NIQ(1)+NIQ(2)+NIQ(3)+NIQ(4)+NIQ(5)+NIQ(6)+NIQ(7)
        NEWMAX = QVAL(NIQ,RSCAMAX)
C        NEWMAX=(NIQ(1)*7+NIQ(2)*6+NIQ(3)*5+NIQ(4)*4+NIQ(5)*3
C     1  +NIQ(6)*2+NIQ(7))*RSCAMAX/200
        RLMAX=ILSUM+(RSCAMAX/4.0)
CHEN>
          write(IOU2,'(''set QVAL_local = '',F16.1)')NEWMAX
C
          write(IOU2,'(''set PSMAX = '',I12)')ICENTRE
C
          call shorten(CNAMPAT,k)
          if(k.gt.3)k=3
          do J = 1,9
            write(IOU2,'(''set '',A,''_IQ'',I1,'' = '',I4)')
     1        CNAMPAT(1:k),J,NIQ(J)
          enddo

C        WRITE(IOU2,1259)NEWMAX,(NIQ(J),J=1,9),ILSUM,RSCAMAX,RLMAX
C1259  FORMAT('VAL=',F10.3,3X,'IQ=',9(I4),' GOOD=',I4,
C     1       ' MAX=',F7.1,' OLDVAL=',F10.3,3X)
C        IF(PERCENT.GE.85.0)WRITE(IOU2,1260)PERCENT
C        IF(PERCENT.GE.50.0.AND.PERCENT.LT.85.0)THEN
C          WRITE(IOU2,1261)PERCENT
C        ENDIF
C        IF(PERCENT.LT.50.0)WRITE(IOU2,1262)PERCENT
CHEN<
1260    FORMAT(F6.1,'% peak.')
1261    FORMAT(F6.1,'% peak.')
1262    FORMAT(F6.1,'% peak.')
C
        CLOSE(IOU2)
      ENDIF
CHEN
C
      STOP
4550    WRITE(6,4551)NMAX
4551    FORMAT(' Too many spots for current prog dimensions',I5)
      STOP
6001    WRITE(6,6002)ISIZEX,ISIZEY,NX,NY
6002    FORMAT(' Image is not of size ISIZEX by ISIZEY',4I8)
      STOP 
      END
C
C*******************************************************************************
C
      SUBROUTINE GRID(AX,AY,BX,BY,ACY,BCY,IXC,IYC,
     .                   IH,IK,XA,YA,NOH,NOK,
     .                   RESMINSQ,RESMAXSQ,
     .                   NXM2,NY2M2,NSPOT,LIST,
     .                   TLTAXIS,SEGMNT,TRNSTEPX)
C
C     subroutine to generate a lattice from 1,0 & 0,1 coordinates
C
      PARAMETER (NMAX=20100)
      DIMENSION IH(NMAX),IK(NMAX),IXC(NMAX),IYC(NMAX),
     1          XA(NMAX),YA(NMAX)
      LOGICAL LIST
      IF(LIST)WRITE(6,10)
   10 FORMAT(/,' Lattice generated coordinates',/,8X,'H',9X,'K',
     1       7X,'X',9X,'Y',/,'0')
      WRITE(6,9001)TRNSTEPX
9001    FORMAT(' STEP SIZE IN TRANSFORM IN X DIRECTION IN A-1',F12.8,/)
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
C         YC is Y coord on same scale as X, ie in undistorted transform space 
          YC = JH * ACY + JK * BCY
          ANGLE=ATAN2(YC,X)
          IF(Y.LT.0.) GO TO 100
C
C         Resolution calculated from X and YC
          DSTARSQ=(X**2+YC**2)*TRNSTEPXSQ
          IF(DSTARSQ.EQ.0.0)GO TO 100
          DSQ=1.0/DSTARSQ
          IF(DSQ.LT.RESMAXSQ.OR.DSQ.GT.RESMINSQ)GO TO 100
C
C       
C         Need angle that is in undistorted transform space
          ANGDIF=ABS(ANGLE-TLTAXIS)*57.29577
50        IF(ANGDIF.GT.180.0) THEN
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
          IF(LIST)WRITE(6,20) JH,JK,X,Y,NSPOT
C        WRITE(6,*)X,YC
   20     FORMAT(2I10,2F10.1,I12)
  100 CONTINUE
      WRITE(6,4552)NSPOT
4552  FORMAT('  THERE WERE A TOTAL OF',I5,'  SPOTS GENERATED')
      WRITE(6,4553)NOUTSIDE
4553    FORMAT(' Number in requested resolution range but outside',
     .' area of transform',I5,/)
      RETURN
4550    WRITE(6,4551) NMAX
4551    FORMAT(' TOO MANY SPOTS FOR CURRENT PROG DIMENSION',I5)
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
      IF(PHASE.LT.0.) PHASE = PHASE + 360.  ! Phase bet 0 and 360 degs.
      RETURN
      END
C*******************************************************************************
C
      SUBROUTINE SPOTS(AX,AY,BX,BY,ACY,BCY,IXC,IYC,IH,IK,XA,YA,
     .  NXM2,NY2M2,NSPOT,LIST)
C
C     subroutine to generate required spots from 1,0 & 0,1 coordinates
C
      PARAMETER (NMAX=20100)
      LOGICAL LIST
      DIMENSION IH(NMAX),IK(NMAX),IXC(NMAX),IYC(NMAX),
     .  XA(NMAX),YA(NMAX)
      IF(LIST)WRITE(6,10)
   10 FORMAT(/,' Requested spot coordinates',/,8X,'H',9X,'K',
     1       7X,'X',9X,'Y',/,'0')
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
23              FORMAT(' Requested spot not inside box,',
     .  ' JH,JK,X,Y,NXM2,NY2M2=',2I5,2F10.1,2I6)
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
   20   FORMAT(2I10,2F10.1)
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
     .  IX,IY,AP,BP,DELPX,DELPY,TURN)
C
C     subroutine to read part of section required plus the extra area needed
C     for the ctf-dependent convolution, then store in array AP,BP for return
C     to main program ---- phases are corrected to desired phase origin.
C
      PARAMETER (INBOXMAX=361)
      DIMENSION ARRAY(2*INBOXMAX,INBOXMAX)      ! Square array of complex no's.
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
   10 FORMAT(/,' error on reading transform')
      STOP
      END
C
C*******************************************************************************
C
        SUBROUTINE ANGAVE(N, THETAS, WEIGHTS, THMEAN, THGOOD)
C       Function: to average a set of angles in degrees
C       Created: 27/7/84 by D.J.Thomas
C       Modified:  by R.HENDERSON 20.5.85
        INTEGER*4       N               !number of input angles
        REAL*4          THETAS(1)       !array of input angles
        REAL*4          THGOOD          !goodness of average (0 to 1)
        REAL*4          THMEAN          !weighted average of input angles
        REAL            COMEAN          !mean value of cosines
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
C*******************************************************************************
C
      SUBROUTINE CONVOLUTE(AP,BP,ACTF,BCTF,AAMP,PPHI,IHOR,IVERT,
     .  ICTFHOR,ICTFVERT)
C
C  Subroutine to perform convolution of raw transform with transform
C   of ctf for tilted image correction.
C
C--------------------------ICTFBXMAX must be 2*ICTFHALF+1 
      PARAMETER (IBOXMAX=41)
      PARAMETER (ICTFBXMAX=401)
      PARAMETER (ICTFHALF=200)
      PARAMETER (INBOXMAX=361)
      DIMENSION AAMP(IBOXMAX,IBOXMAX),PPHI(IBOXMAX,IBOXMAX),
     .  ACTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .  BCTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .  AP(INBOXMAX,INBOXMAX),BP(INBOXMAX,INBOXMAX)
c      DIMENSION AT(5,5),PT(5,5)                ! TEST DIAGNOSTIC O/P
C
C-----AP and BP are input transforms (cos and sin fractions)
C-----ACTF and BCTF are FFT of CTF pattern (cos and sin fraction)
C
      ICTFHOR2 = ICTFHOR/2
      ICTFVERT2 = ICTFVERT/2
      DO I=1,IHOR
        DO J=1,IVERT
          A=0.
          B=0.
          itmp1=I+ICTFHOR2
          itmp2=J+ICTFVERT2
          DO ICTF=-ICTFHOR2,ICTFHOR2
            DO JCTF=-ICTFVERT2,ICTFVERT2
               K=itmp1-ICTF
               L=itmp2-JCTF
C
C--------------A is real part: RE(pic1)*RE(pic2) + i*IM(pic1)*i*IM(pic2)
C--------------               =RE(pic1)*RE(pic2) - IM(pic1)*IM(pic2)
C
C--------------B is imag part: RE(pic1)*IM(pic2) + IM(pic1)*RE(pic2)
C
C--------------K = I + ICTFHOR2 - ICTF
C--------------K e {1...IHOR+2*ICTFHOR2} = { 1 ... IHOR+ICTFHOR }
C--------------K is centered around (IHOR/2 + ICTFHOR2)
C
               A = A + AP(K,L)*ACTF(ICTF,JCTF) - BP(K,L)*BCTF(ICTF,JCTF)
               B = B + AP(K,L)*BCTF(ICTF,JCTF) + BP(K,L)*ACTF(ICTF,JCTF)
             enddo
           enddo
C
C---------extract (RE,IM)=(cos,sin) into (AMP,PHASE)
C
          AMP = SQRT(A*A + B*B)
C---------write(6,'(''AMP: '',F12.5)')AMP
          IF(AMP.EQ.0.0) THEN
            PHASE = 0.
          ELSE
            PHASE = ATAN2(B,A) * 57.2958
          ENDIF
          IF(PHASE.LT.0.0) PHASE = PHASE + 360.0        ! Phase bet 0 and 360 deg.
          AAMP(I,J) = AMP
          PPHI(I,J) = PHASE
        enddo
      enddo
      RETURN
      END
C
CC*******************************************************************************
CC
C      SUBROUTINE SCSL_CONVOLUTE(AP,BP,ACTF,BCTF,AAMP,PPHI,IHOR,IVERT,
C     .  ICTFHOR,ICTFVERT)
CC
CC  Subroutine to perform convolution of raw transform with transform
CC   of ctf for tilted image correction. (using SCSL)
CC
CC--------------------------ICTFBXMAX must be 2*ICTFHALF+1 
C      PARAMETER (IBOXMAX=41)
C      PARAMETER (ICTFBXMAX=401)
C      PARAMETER (ICTFHALF=200)
C      PARAMETER (INBOXMAX=361)
C      DIMENSION AAMP(IHOR,IVERT),PPHI(IHOR,IVERT),
C     .          ACTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
C     .          BCTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
C     .          AP(IHOR,IVERT),BP(IHOR,IVERT)
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
CC      CALL CFIR2D(
CC      CALL TDXCONV(
CC     .       P(1,1),                    1,INBOXMAX,  1,
CC     .        ICTFHOR+IHOR, 1,         ICTFVERT+IVERT,
CC     .       CTF(-ICTFHOR2,-ICTFVERT2), 1,ICTFBXMAX, 1,
CC     .        ICTFHOR+1,    1,         ICTFVERT+1,
CC     .       OUTPUT(1,1),               1,INBOXMAX,  2+ICTFHOR,
CC     .        IHOR, 2+ICTFVERT,         IVERT,
CC     .       ALPHA,BETA)
CC
C      CALL CONVOLUTE(P(1,1),IHOR,IVERT,
C     .  CTF(1,1), ICTFHOR, ICTFVERT,
C     .  OUTPUT(1,1))
CC
CC      CALL TDXCONV(P(1,1),IHOR,IVERT,
CC     .  CTF(1,1), ICTFHOR, ICTFVERT,
CC     .  OUTPUT(1,1))
CC
CC     CALL CFIR2D (x, incx, ldx, i1x0, nx1, i2x0, nx2, 
CC                  h, inch, ldh, i1h0, nh1, i2h0, nh2, 
CC                  y, incy, ldy, i1y0, ny1, i2y0, ny2,
CC                  alpha, beta)
CC
C      DO I=1,IHOR
C        DO J=1,IVERT
C          K = MOD(I,IHOR-ICTFHOR)+ICTFHOR
C          L = MOD(J,IVERT-ICTFVERT)+ICTFVERT
C          AAMP(I,J) = CABS(OUTPUT(K,L))
C          IF(AAMP(I,J).EQ.0.0) THEN
C            PHASE = 0.
C          ELSE
C                  RIMA = AIMAG(OUTPUT(K,L))
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
C
      SUBROUTINE CTFGEN(IH,IK,X,Y,RATIOXY,
     .  THETATR,DFMID1,DFMID2,ANGAST,
     .  CS,WL,STEPR,ISIZEX,ISIZEY,DELHEIGHT,
     .  TLTAXIS,TLTANGL,TANTILT,COST,SINT,
     .  ICTFHOR,ICTFVERT,ACTF,BCTF,
     .  ILIST,DFMID,DELCHI,CTFMID,FACTOR,ISENS)
      PARAMETER (ICTFBXMAX=401)
      PARAMETER (ICTFHALF=200)
      DIMENSION ACTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .  BCTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .   CTF(ICTFBXMAX*ICTFBXMAX)
      LOGICAL ILIST
      ISENS=ICHAR(' ')
      TWOPI = 2.0 * 3.1415926
      YC=Y*RATIOXY
      RAD = SQRT(X**2+YC**2)
      ANGLE=RAD*THETATR
      ANGSPT=ATAN2(YC,X)
      C1=TWOPI*ANGLE*ANGLE/(2.0*WL)
      DELCHI=C1*DELHEIGHT
      SINEWAVES=DELCHI/TWOPI
          ICTFHOR=MAX(10,INT(DELCHI))
          ICTFHOR=(ICTFHOR/2)*2                     ! ensure ICTFHOR is even.
          IF(ICTFHOR.GT.38) ICTFHOR=(ICTFHOR/8)*8   ! ensures prime factor < 19
          IF(ICTFHOR.GT.ICTFBXMAX-1) THEN           ! ensures storage ok.
                WRITE(6,101)ICTFHOR
101             FORMAT(' Subroutine CTFGEN dimensions too small,',
     .  '  ICTFHOR needs',I8)
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
        IF(DELCHI/2.GT.ASIN(ABS(CTFMID))) ISENS=ICHAR('*')  !Spot has a zero in ctf
        SUMC=0.0
      rtmp1=(1.0/(ICTFHOR))*ISIZEX*STEPR
      rtmp2=(1.0/(ICTFVERT))*ISIZEY*STEPR
      DO 100 I=1,ICTFHOR
      DO 100 J=1,ICTFVERT
        ISTORE=I+(ICTFHOR+2)*(J-1)      ! indexing for array CTF.
C         Calculate height of this element of image.
        XP=(I-0.5-ICTFHOR2)*rtmp1               ! 0.5 rounding error
        YP=(J-0.5-ICTFVERT2)*rtmp2              ! 0.5 rounding error
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
        ISTORE=I+(ICTFHOR+2)*(J-1)      ! indexing for array CTF.
120   CTF(ISTORE)=RESCALE*CTF(ISTORE)
C
C-----CALL TODFFT(CTF,ICTFHOR,ICTFVERT,0)
      CALL TDXFFT(CTF,ICTFHOR,ICTFVERT,0)
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
                C=COS((IX+IY)*PHSHFT)   ! assumes ICTFHOR=ICTFVERT
                S=SIN((IX+IY)*PHSHFT)   ! assumes ICTFHOR=ICTFVERT
                A = CTF(ISTORE)
                B = CTF(ISTORE+1)
                ACTF(IX,IY) = A*C-B*S
                BCTF(IX,IY) = A*S+B*C
        ENDIF
        IF(J.GE.1+ICTFVERT2) THEN
                IX=I-1
                IY=J-1-ICTFVERT
                C=COS((IX+IY)*PHSHFT)   ! assumes ICTFHOR=ICTFVERT
                S=SIN((IX+IY)*PHSHFT)   ! assumes ICTFHOR=ICTFVERT
                A = CTF(ISTORE)
                B = CTF(ISTORE+1)
                ACTF(IX,IY) = A*C-B*S
                BCTF(IX,IY) = A*S+B*C
        ENDIF
150     CONTINUE
        DO 160 I= 1,ICTFHOR2
        DO 160 J=-ICTFVERT2,ICTFVERT2
                ACTF(-I,-J) =  ACTF(I,J)
                BCTF(-I,-J) = -BCTF(I,J)
160     CONTINUE
C
      IF(ILIST) WRITE(6,103)DELHEIGHT,DFMID,SINEWAVES,
     .  DELCHI,ICTFHOR,CTFMID,FACTOR
103   FORMAT(' Contrast transfer function description -----',
     .  '-------- Height difference ===================',F10.1,/,
     .  54X,'Midpoint defocus ====================',F10.1,/,
     .  54X,'Number ctf cycles ===================',F10.3,/,
     .  54X,'Number ctf samples needed (used)=====',F10.1,'(',I2,')',/,
     .  54X,'Midpoint C.T.F. =====================',F10.4,/,
     .  54X,'Rescaling factor (keeps noise const)=',F10.4)
      RETURN
      END
C
C*******************************************************************************
      SUBROUTINE TTPLOT(IH,IK,IQ,AX,BX,AY,BY,
     .  TLTAXIS,TRNSTEPX,RATIOXY,TITLE,RESMIN,RESMA2,
     .  NSPOTS,ORIANG,ORITAX,ORITAN,
     .  DSTEP,DFMID1,DFMID2,ANGAST,XMAG,NHOR,NVERT)
      PARAMETER (PLTSIZ=300.0)
      PARAMETER (RESMAX=0.3)
      PARAMETER (CHRSIZ=0.3)
      PARAMETER (FONTSIZE=3.5)
C
      CHARACTER*60 CZEILE(60)
      DIMENSION TITLE(17),RESTORE(3),TZEILE(15)
      CHARACTER*8 TEXT
C
      DATA RESTORE/7.0,5.5,3.5/
      SCALE=PLTSIZ/(2.0*RESMAX)         !MAXIMUM RESOLUTION, 0.3=3.33 ANGSTROMS
C       If IH=IK=0    the plot is initialised
C       If IH=IK=999  the plot is terminated
C       For other values of IH and IK, a symbol of size increasing with 
C               decreasing IQ is plotted in the appropriate position.
C
C  Here for initialisation
C
      IF(IH.EQ.0.AND.IK.EQ.0) THEN      ! Initialisation of plot
            WRITE(6,20)
20          FORMAT(' ENTERING TTPLOT INITIALISATION')
            NSPOTS = 0

            CALL P2K_OUTFILE('TTPLOT.PS',9)
            CALL P2K_HOME
            CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
            CALL P2K_GRID(0.5*PLTSIZ,0.5*PLTSIZ,1.0)
            CALL P2K_ORIGIN(-0.5*PLTSIZ,-0.7*PLTSIZ,0.)
            CALL P2K_COLOUR(0)
CHENN>
C
            YPOSSTE=9.0
            YPOSN=PLTSIZ+5.0+6.0*YPOSSTE
            XPOSN=10.0
            CALL P2K_MOVE(XPOSN,YPOSN,0.)
            CALL P2K_STRING(TITLE,68,0.)        ! PLOT TITLE ACCROSS TOP OF IMAGE
C
            YPOSN=YPOSN-YPOSSTE
            CALL P2K_MOVE(XPOSN,YPOSN,0.)
            WRITE (CZEILE,1237) RESMIN,RESMA2
 1237       FORMAT('Res from ',F6.1,' Ang. to ',F6.1,' Ang.')
            READ(CZEILE,'(15A4)') TZEILE
            CALL P2K_STRING(TZEILE,60,0.)
C
            YPOSN=YPOSN-YPOSSTE
            CALL P2K_MOVE(XPOSN,YPOSN,0.)
            WRITE (CZEILE,1233) DSTEP
 1233       FORMAT('Dens.Step = ',F6.1,' Micrometer')
            READ(CZEILE,'(15A4)') TZEILE
            CALL P2K_STRING(TZEILE,60,0.)
C
            YPOSN=YPOSN-YPOSSTE
            CALL P2K_MOVE(XPOSN,YPOSN,0.)
            WRITE (CZEILE,'(''Magnification = '',F8.0)') XMAG
            READ(CZEILE,'(15A4)') TZEILE
            CALL P2K_STRING(TZEILE,60,0.)
C
            YPOSN=YPOSN-YPOSSTE
            CALL P2K_MOVE(XPOSN,YPOSN,0.)
            WRITE (CZEILE,1234)DFMID1,DFMID2,ORIANG
 1234       FORMAT('Underfocus = ',F8.0,' Ang., ',F8.0,
     .             ' Ang., Angle=',F5.1)
            READ(CZEILE,'(15A4)') TZEILE
            CALL P2K_STRING(TZEILE,60,0.)
C
            YPOSN=YPOSN-YPOSSTE
            CALL P2K_MOVE(XPOSN,YPOSN,0.)
            WRITE (CZEILE,1236)ORITAX,ORITAN
 1236       FORMAT('Tiltaxis = ',F8.3,', Tiltangle = ',F8.3)
            READ(CZEILE,'(15A4)') TZEILE
            CALL P2K_STRING(TZEILE,60,0.)
C
            YPOSN=YPOSN-YPOSSTE
            CALL P2K_MOVE(XPOSN,YPOSN,0.)
            WRITE (CZEILE,1235) RESTORE(1),RESTORE(2),RESTORE(3)
 1235       FORMAT('Resolution rings are at ',F3.1,', ',F3.1,
     .             ' and ',F3.1,' A.')
            READ(CZEILE,'(15A4)') TZEILE
            CALL P2K_STRING(TZEILE,60,0.)
C
CHENN<
            CALL P2K_MOVE(0.,0.,0.)
            CALL P2K_DRAW(PLTSIZ,0.,0.)
            CALL P2K_DRAW(PLTSIZ,PLTSIZ,0.)
            CALL P2K_DRAW(0.,PLTSIZ,0.)
            CALL P2K_DRAW(0.,0.,0.)
            CENTRE=PLTSIZ/2.0
            CALL P2K_ORIGIN(CENTRE,CENTRE,0.)
            CALL P2K_MOVE(-CHRSIZ,-CHRSIZ,0.)
            CALL P2K_DRAW(CHRSIZ,CHRSIZ,0.)
            CALL P2K_MOVE(CHRSIZ,-CHRSIZ,0.)
            CALL P2K_DRAW(-CHRSIZ,CHRSIZ,0.)    ! CENTRAL CROSS AT ORIGIN.
                ALENGTH=SQRT(AX**2+AY**2)
                X=(AX/ALENGTH)*(PLTSIZ/2.0)
                Y=(AY/ALENGTH)*(PLTSIZ/2.0)
                CALL P2K_MOVE(0.,0.,0.)
                CALL P2K_DRAW(X,Y,0.)           ! PLOT ASTAR VECTOR
                X=X+8.
                Y=Y-1.5
                CALL P2K_MOVE(X,Y,0.)
                WRITE(TEXT,151)
151             FORMAT('H')
152             FORMAT('K')
                CALL P2K_CSTRING(TEXT,1,0.)
                BLENGTH=SQRT(BX**2+BY**2)
                X=(BX/BLENGTH)*(PLTSIZ/2.0)
                Y=(BY/BLENGTH)*(PLTSIZ/2.0)
                CALL P2K_MOVE(0.,0.,0.)
                CALL P2K_DRAW(X,Y,0.)           ! PLOT BSTAR VECTOR

                X=X+8.
                Y=Y-1.5
                CALL P2K_MOVE(X,Y,0.)
                WRITE(TEXT,152)
                CALL P2K_CSTRING(TEXT,1,0.)
                CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*0.6)
                X=COS(TLTAXIS)*PLTSIZ/2.1
                Y=SIN(TLTAXIS)*PLTSIZ/2.1
                CALL P2K_MOVE(-X,-Y,0.)
                CALL P2K_DRAW(X,Y,0.)           ! PLOT TILTAXIS
                X=X-15.0
                Y=Y-6.0
                CALL P2K_MOVE(X,Y,0.)
                WRITE(TEXT,153)
153             FORMAT('TILTAXIS')
                CALL P2K_CSTRING(TEXT,8,0.)
                DO 370 N=1,3                    ! Three resoultion ranges
                  RES=1.0/RESTORE(N)
                  RAD=SCALE*RES
                  CALL P2K_MOVE(RAD,0.,0.)
                  DO 360 I=1,360                        ! Resolution circles
                    ANG=I*3.1415926/180.0
                    X=RAD*COS(ANG)
                    Y=RAD*SIN(ANG)
                    CALL P2K_DRAW(X,Y,0.)
360               CONTINUE
370             CONTINUE
                RETURN
      ENDIF
C
C  Here for termination
C
      IF(IH.EQ.999) THEN
        WRITE(6,110) NSPOTS
110     FORMAT(' TOTAL SPOTS PLOTTED IN TTPLOT FILE =',I8,
     .    ' and plot file closed')
        CALL P2K_PAGE
        RETURN
      ENDIF
C
C      write(6,'('' Hier 2c'')')
C  Here for spot plots
C
      IF(IQ.GT.8) RETURN                ! PLOTS SPOTS WITH IQ =8 OR LESS.
C       WRITE(6,21)IH,IK
21      FORMAT(' SPOT PLOTTED & FRIEDEL PAIR ',2I5)
      DO 100 J=-1,1,2
        IHP=J*IH
        IKP=J*IK
        X=IHP*AX+IKP*BX
        Y=IHP*AY+IKP*BY
        X=X*TRNSTEPX            ! X-coordinate of spot in reciprocal angstroms
        Y=Y*RATIOXY*TRNSTEPX    ! Y-coordinate of spot in reciprocal angstroms
        IF(ABS(X).GE.RESMAX)GO TO 100
        IF(ABS(Y).GE.RESMAX)GO TO 100
        X=X*SCALE
        Y=Y*SCALE
C        WRITE(6,104)X,Y,SCALE
104     FORMAT(3F10.1)
        XN=X-CHRSIZ*(8.1-IQ)
        XP=X+CHRSIZ*(8.1-IQ)
        YN=Y-CHRSIZ*(8.1-IQ)
        YP=Y+CHRSIZ*(8.1-IQ)
        NSPOTS=NSPOTS+1
        CALL P2K_MOVE(XN,YN,0.)
        CALL P2K_DRAW(XP,YN,0.)
        CALL P2K_DRAW(XP,YP,0.)
        CALL P2K_DRAW(XN,YP,0.)
        CALL P2K_DRAW(XN,YN,0.)           ! SQUARE ROUND EACH SPOT.
        X=X-0.0                           ! ADJUST CHARACTER TO BE CENTRAL IN X.
        Y=Y-1.0                           ! ADJUST CHARACTER TO BE CENTRAL IN Y.
        CALL P2K_MOVE(X,Y,0.)
        WRITE(TEXT,160)
        IF(IQ.EQ.1) WRITE(TEXT,161)     ! IQ=1 include number
        IF(IQ.EQ.2) WRITE(TEXT,162)     ! IQ=2 include number
        IF(IQ.EQ.3) WRITE(TEXT,163)     ! IQ=3 include number
        IF(IQ.EQ.4) WRITE(TEXT,164)     ! IQ=4 include number
C
160     FORMAT(' ')
161     FORMAT('1')
162     FORMAT('2')
163     FORMAT('3')
164     FORMAT('4')
        CALL P2K_CSTRING(TEXT,1,0.)
100   CONTINUE
C
      RETURN
      END
C
C***************************************************************************
C
      SUBROUTINE GET_RMSBK(AAMP,PPHI,DX,DY,IVERT,IHOR,ISINCBOX,RMSBK)
      PARAMETER (IBOXMAX=41)
      REAL*8 RMSBK
      DIMENSION AAMP(IBOXMAX,IBOXMAX),PPHI(IBOXMAX,IBOXMAX)
C this calculation to do same sinc function treatment of background as is used
C for the peak in AMPOUT in main program
      PI = 3.141592654
      IF(ISINCBOX.NE.2) THEN
        WRITE(6,18)ISINCBOX
18      FORMAT(/,' WARNING !!!!!!!!!!!!!!!!!!!!!, ISINCBOX not 2',I5,/,
     .    ' background calculation algorithm based a value on 2',/)
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
     .  (PI**2 * DXC * DYC)
                ENDIF
                AMP   = AAMP(JBOX,KBOX)
                PHASE = PPHI(JBOX,KBOX) / 57.2958
                ASUM = ASUM + AMP * COS(PHASE) * SINC
                BSUM = BSUM + AMP * SIN(PHASE) * SINC
                DENOM = DENOM + SINC**2
20      CONTINUE
        AMPSINC = SQRT(ASUM**2 + BSUM**2)/DENOM
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
     .  (PI**2 * DXC * DYC)
                ENDIF
                AMP   = AAMP(JBOX,KBOX)
                PHASE = PPHI(JBOX,KBOX) / 57.2958
                ASUM = ASUM + AMP * COS(PHASE) * SINC
                BSUM = BSUM + AMP * SIN(PHASE) * SINC
                DENOM = DENOM + SINC**2
40      CONTINUE
        AMPSINC = SQRT(ASUM**2 + BSUM**2)/DENOM
        NBK=NBK+1
        SUMBKSQ=SUMBKSQ+AMPSINC**2
50    CONTINUE
C third calculate the overall rms background
      IF(NBK.NE.(2*(IVERT-1)+2*(IHOR-3))) STOP ' Error in GET_RMSBK'
      RMSBK=SQRT(SUMBKSQ/NBK)
      RMSBK=RMSBK/1.10          ! fudge factor to restore earlier scale of A/B
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

