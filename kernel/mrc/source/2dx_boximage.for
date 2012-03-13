C*BOXIMAGE.FOR**************************************************************
C     Simple boxing program to create circular or polygonal box with no
C       change in image size.
C     This makes it quite suitable for the FFT cross-correlation method.
C         Input original image on stream 1 (IN)
C         Output boxed image on stream 2 (OUT)
C
C     derived from BOXIM (27-AUG-82) program by JMS, version 2.0
C     Version  2.1      7-JUL-84    RH Simpler boxing and explicit origin
C                                      uses grid units to specify area and
C                                      does not move boxed off area to corner.
C     VERSION  2.2      29-SEP-85   RH Does not float the densities so that
C				       program will work for integer*1 images.
C     VERSION  2.3      28-May-87   RH Real*8 DTOT accumulation.
C     VERSION  2.4      23-Mar-88   RH vertices of box made non-integer
C     VERSION  2.5      18-Jul-91   RH double circular torus-shaped box option
C     VERSION  3.0      01-Jan-92   RH convert to UNIX for Alliant
C     VERSION  3.1      21-Sep-95   RH moved to Dec Alpha, debug RAD3,RAD4
C     VERSION  3.2      25-Nov-95   RH check number of vertices on input
C     VERSION  3.3      22-Apr-96   RH change JINT INT (JMS compatibility)
C     VERSION  3.4      24-Jul-96  JMS write centre of gravity to file 
C                                      boximage.tmp
C     VERSION  3.41	29-Mar-00  JMS zorigin inserted in ialorg for compatibility 
C					with imsubs2000C
C	card input
C
CHEN>
C       1.      NOVERT,IOUTZERO
C                                 -number of vertices (or 0 for circcular or 
C                                  1 for square box), flag for setting outside
C                                  of box to zero or float to average 
C                                  IOUTZERO: What should be outside of mask:
C                                  (0=average,1=zero)
CCCCCCCCCCCCC   NOVERT            -number of vertices, 0 for circular box (*)
CHEN<
C	2. OX,OY		  -phase origin position                  (*)
C	3a. CX,CY,RAD1,2,3,4  -centre coords and radii if circle      (*)
C				   :note four radii are for a tapered toroidal
C				   :mask. A simple circle would have RAD1,2=0
C				   :and rad3,4 equal to the required radius.
C   or	3b. PX(I),PY(I)	  -coords of vertices if polygon          (*)
C				   :note origin is (0,0) at lower left corner.
CHEN>
C   or  3c      RCX,RCY,RLENBO    -centre coords and side-lenght for square
C
C---------No: 4 IOUTZERO          -setting outside of box to zero? 1=y,0=average.
CHEN<
C
C   NOTE : Specify points in image wrt origin (0,0) in bottom left corner.
C          Therefore the middle of a 1024x1024 image is (512,512).
C
C   IMPORTANT NOTE : This program does not float the image, but replaces
C			densities outside box by the average along the
C			inside perimeter.
C
C****************************************************************************
C
CHEN>
C      PARAMETER (NMAX=10000)
C      PARAMETER (NVERTMAX=21)
      PARAMETER (NMAX=20000)
      PARAMETER (NVERTMAX=101)
CHEN<
      COMMON//NX,NY,NZ
      DIMENSION PX(NVERTMAX),PY(NVERTMAX),SLOPE(NVERTMAX),ALINE(NMAX),
     1          NXYZR(3),MXYZR(3),IXYZ(3),MXYZ(3),TITLE(20)
      REAL*8 DTOT
CHEN>
C      LOGICAL INCX(20),ADDXY(20)
      LOGICAL INCX(30),ADDXY(30)
CHEN<
      CHARACTER DAT*24
      EQUIVALENCE (NX,NXYZR)
CTSH++
      CHARACTER*80 TMPTITLE
      EQUIVALENCE (TMPTITLE,TITLE)
CTSH--

C
      PRINT *,' BOXIMAGE V3.41  : Simple boxing program 29.03.2000'
C
C     open files
C
      CALL  IMOPEN(1,'IN','RO')
      CALL  IMOPEN(2,'OUT','NEW')
      CALL  IRDHDR(1,NXYZR,MXYZR,MODE,DMIN,DMAX,DMEAN)
C
      IF(NXYZR(1).GT.NMAX.OR.NXYZR(2).GT.NMAX) THEN
      	WRITE(6,999) NMAX
 999    FORMAT(' ARRAY ALINE PROG DIMS TOO SMALL',I10)
      	STOP
      ENDIF
C
C     read in parameters
C
CHEN>
C      READ(5,*) NOVERT
C
      READ(5,*) NOVERT,IOUTZERO
C
      if(IOUTZERO.eq.0)then
        write(6,'('' setting outside of box to average.'')')
      endif
      if(IOUTZERO.eq.1)then
        write(6,'('' setting outside of box to zero.'')')
        DMEAN=0.0
C------ DMIN=0.0
      endif
C
CHEN<
      WRITE(6,10) NX,NY,NOVERT
   10 FORMAT(/,/,/,'  Boxed image size remains',I10,' by',I10,
     1 ' points',/,/,'  No of vertices',I10,/)
      BIGNUM = 10.0E+10
      IXYZ(1) = NX
      IXYZ(2) = NY
      IXYZ(3) = 1
      MXYZ(1) = 0
      MXYZ(2) = 0
      MXYZ(3) = 0
      NLINES = 0
      CALL  ICRHDR(2,IXYZ,IXYZ,MODE,TITLE,0)
      CALL  ITRLAB(2,1)
C
C     Scale now set to be in pixels as on al image.
C
      SCALE = 1.
C   
C     read in phase origin position
C
      READ(5,*) OX,OY
      OX = OX * SCALE
      OY = OY * SCALE
      IF(NOVERT.NE.0) GO TO 2000
C
C     circular box  ###########################################################
C
      READ(5,*) CX,CY,RAD1,RAD2,RAD3,RAD4
      WRITE(6,30) CX,CY,RAD1,RAD2,RAD3,RAD4
      IF(RAD1.GT.RAD2) STOP 'RAD1 must be less than RAD2'
      IF(RAD2.GT.RAD3) STOP 'RAD2 must be less than RAD3'
      IF(RAD3.GT.RAD4) STOP 'RAD3 must be less than RAD4'
   30 FORMAT(/,/, ' centre of circular area =',2F10.1,/,
     . ' toroid inner radii      =',2F10.1,/,
     . ' toroid outer radii      =',2F10.1)
      CX = CX * SCALE
      CY = CY * SCALE
      RAD1 = RAD1 * SCALE
      RADSQ1 = RAD1*RAD1
      JRAD1 = INT(RAD1)
      RAD2 = RAD2 * SCALE
      RADSQ2 = RAD2*RAD2
      JRAD2 = INT(RAD2)
      RAD3 = RAD3 * SCALE
      RADSQ3 = RAD3*RAD3
      JRAD3 = INT(RAD3)
      RAD4 = RAD4 * SCALE
      RADSQ4 = RAD4*RAD4
      JRAD4 = INT(RAD4)
C
C     apply phase origin corrections
C     non zero origin,calculate shift in phase origin
C
      XORIGIN = OX
      YORIGIN = OY
 1110 CALL IALORG(2,XORIGIN,YORIGIN,ZORIGIN)
      CALL  IALSIZ(2,IXYZ,MXYZ)
      CALL  FDATE(DAT)
CTSH      ENCODE(80,50,TITLE) DAT(5:24)
CTSH++
      WRITE(TMPTITLE,50) DAT(5:24)
CTSH--
   50 FORMAT('  BOXIMAGE : box off circular area without floating',
     . 5X,A20)
      CALL  IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
C     find mean density round inner and outer edge of boxed area
C
C  INNER 
      NCALC = INT(RAD1 + 0.5) * 2 + 1 
      DTOT = 0.
      NTOT = 0
      IX = CX - RAD1 + 0.5
      COUNT = 0.
      DO 1120 I=1,NCALC
      RADI = ABS(RAD1 - COUNT)
      IF(INT(RADI).GE.JRAD1) THEN
      SQRTRD = 0.
      ELSE
      SQRTRD = SQRT(RADSQ1 - RADI*RADI)
      END IF
      COUNT = COUNT + 1.0
      IX = IX + 1
      IY1 = CY + SQRTRD + 0.5
      IY2 = CY - SQRTRD + 0.5
      CALL  IMPOSN(1,0,IY1)
      CALL  IRDPAL(1,ALINE,IX,IX,*9000)
      DTOT = DTOT + ALINE(1)
      NTOT = NTOT + 1
      IF(IY1.NE.IY2) THEN
      	CALL  IMPOSN(1,0,IY2)
      	CALL  IRDPAL(1,ALINE,IX,IX,*9000)
      	DTOT = DTOT + ALINE(1)
      	NTOT = NTOT + 1
      ENDIF
 1120 CONTINUE
      DMEANINNER = DTOT / NTOT
      WRITE(6,1121)DMEANINNER
1121  FORMAT(' Mean density at inner radius =',F12.3)
C
C  OUTER
      NCALC = INT(RAD4 + 0.5) * 2 + 1 
      DTOT = 0.
      NTOT = 0
      IX = CX - RAD4 + 0.5
      COUNT = 0.
      DO 1122 I=1,NCALC
      RADI = ABS(RAD4 - COUNT)
      IF(INT(RADI).GE.JRAD4) THEN
      SQRTRD = 0.
      ELSE
      SQRTRD = SQRT(RADSQ4 - RADI*RADI)
      END IF
      COUNT = COUNT + 1.0
      IX = IX + 1
      IY1 = CY + SQRTRD + 0.5
      IY2 = CY - SQRTRD + 0.5
      CALL  IMPOSN(1,0,IY1)
      CALL  IRDPAL(1,ALINE,IX,IX,*9000)
      DTOT = DTOT + ALINE(1)
      NTOT = NTOT + 1
      IF(IY1.EQ.IY2) THEN
      	CALL  IMPOSN(1,0,IY2)
      	CALL  IRDPAL(1,ALINE,IX,IX,*9000)
      	DTOT = DTOT + ALINE(1)
      	NTOT = NTOT + 1
      ENDIF
 1122 CONTINUE
      DMEANOUTER = DTOT / NTOT
      DMEAN = DMEANOUTER
      WRITE(6,1123)DMEANOUTER
1123  FORMAT(' Mean density at outer radius =',F12.3)
C
C     Do not float by subtracting mean around edge from all points
c     inside box, but replace points outside box by average perimeter
C     density. Replace points inside toroid by average internal perimeter.
C     Do not place image in bottom left corner.
C
      COUNT = CY - RAD4 
      	NY1=NY-1
      KY1 = COUNT + 0.5
      COUNT = ABS(COUNT - KY1)
      KY2 = CY + RAD4 + 0.5
      KY3 = KY2 + 1
      DMIN = 1.E7
      DMAX = 0.
      DTOT = 0.
      NPTS = 0
      DO 1148 L=1,NX
1148  ALINE(L)=DMEANOUTER
      	DO 1149 KY=0,KY1-1		!PUTS KY1 BLANK LINES OUTSIDE RAD4.
      	NLINES=NLINES+1
1149  	CALL IWRLIN(2,ALINE)
      CALL  IMPOSN(1,0,KY1)
      DO 1160 KY=KY1,KY2
      RADI = ABS(RAD4 - COUNT)
      IF(INT(RADI).GE.JRAD4) THEN
      	SQRTRD = 0.
      	KX1 = CX + 1.5
      	KX2 = KX1
      ELSE
      	SQRTRD = SQRT(RADSQ4 - RADI*RADI)
C  ADD 1.5 because of array subscripts starting @ 1
        KX1 = CX - SQRTRD + 1.5
      	KX2 = CX + SQRTRD + 1.5
      END IF
      CALL  IRDLIN(1,ALINE,*9000)
      DO 1140 J=1,KX1-1
      ALINE(J) = DMEANOUTER
 1140 CONTINUE
      DO 1141 J=KX2+1,NX
      ALINE(J) = DMEANOUTER
 1141 CONTINUE
      DO 1130 K=KX1,KX2
      	RADIUS = SQRT((K-CX-1.0)**2+(KY-CY-1.0)**2)
      	IF(RADIUS.LE.RAD1) THEN
      		AL=DMEANINNER		! central area set to mean inner density
      	ELSE
      		IF(RADIUS.LE.RAD2) THEN
      			F1=(RAD2-RADIUS)/(RAD2-RAD1)
      			F2=(RADIUS-RAD1)/(RAD2-RAD1)
      			AL=DMEANINNER*F1+ALINE(K)*F2  ! inner taper interpolated
      		ELSE
      			IF (RADIUS.LE.RAD3) THEN
      				AL=ALINE(K)	! original density retained
      			ELSE
      			   IF (RAD3.NE.RAD4) THEN
      				F1=(RAD4-RADIUS)/(RAD4-RAD3)
      				F2=(RADIUS-RAD3)/(RAD4-RAD3)
      				AL=ALINE(K)*F1+DMEANOUTER*F2  ! outer taper
      			   ELSE
      				AL=ALINE(K)
      			   ENDIF
      			ENDIF
      		ENDIF
      	ENDIF      				
      	ALINE(K) = AL
      	IF(AL.GT.DMAX) DMAX = AL
      	IF(AL.LT.DMIN) DMIN = AL
      	DTOT = DTOT + AL
        NPTS = NPTS + 1
 1130 CONTINUE
C
C     position data correctly
C
      CALL  IWRLIN(2,ALINE)
      COUNT = COUNT + 1.0 
      NLINES = NLINES + 1
 1160 CONTINUE
C
C     pad lines in y direction
C
      DO 1180 K=1,NX
      ALINE(K) = DMEANOUTER
 1180 CONTINUE
      DO 1200 I=KY3,NY1
      CALL  IWRLIN(2,ALINE)
      NLINES = NLINES + 1
 1200 CONTINUE
      DBMEAN = DTOT / NPTS
      CALL  IWRHDR(2,TITLE,-1,DMIN,DMAX,DBMEAN)
      GO TO 4000
C
CHEN>
C
 2000 continue
CMAR>
C      IF(NOVERT.GE.NVERTMAX) THEN
C        PRINT *,' too many vertices for box'
C        WRITE(6,'('' max vertices = '',I3)') NVERTMAX
C        STOP
C      ENDIF
CMAR<
C
      IF(NOVERT.LT.0) THEN
C
C        SQUARE box #####################################################
C
         PRINT *,' Center of box, origin (0,0) at bottom left'
         PX0=0.0
         PY0=0.0
         READ(5,*) RCX,RCY
         WRITE(6,'('' SQUARE BOX, Middle = '',2F9.2)') RCX,RCY
         PRINT *,' Side lenght of box'
         READ(5,*) RLENBO
         WRITE(6,'('' LENGTH of side = '',F9.2)') RLENBO
         RLENBO=RLENBO/2.0
         RWIDTH=REAL(NX)
         RHEIGH=REAL(NY)
         if ( RCX .lt. RLENBO ) RCX = RLENBO
         if ( RCX .gt. RWIDTH-RLENBO-1.0 ) RCX = RWIDTH-RLENBO-1.0
         if ( RCY .lt. RLENBO ) RCY = RLENBO
         if ( RCY .gt. RHEIGH-RLENBO-1.0 ) RCY = RHEIGH-RLENBO-1.0
         PX(1)=RCX-RLENBO
         PY(1)=RCY-RLENBO
         PX(2)=RCX-RLENBO
         PY(2)=RCY+RLENBO
         PX(3)=RCX+RLENBO
         PY(3)=RCY+RLENBO
         PX(4)=RCX+RLENBO
         PY(4)=RCY-RLENBO
         DO 61 I=1,4
            WRITE(6,'(20X,2F10.2)') PX(I),PY(I)
            PX0=PX0+PX(I)
            PY0=PY0+PY(I)
   61    CONTINUE
         NOVERT=4
         PX0=PX0/NOVERT
         PY0=PY0/NOVERT
C
      ELSE
C
CHEN<
C
C     polygonal box  ##########################################################
C
CHEN>
C2000 IF(NOVERT.GE.NVERTMAX) THEN
      IF(NOVERT.GE.NVERTMAX) THEN
CHEN<
      	PRINT *,' too many vertices for box'
      	STOP
      ENDIF
C
C     read in vertices
C
 2040 PRINT *,' Vertices for box, origin (0,0) at bottom left'
      PX0=0.0
      PY0=0.0
      DO 2060 I=1,NOVERT
        READ(5,*) PX(I),PY(I)
        WRITE(6,60) I,PX(I),PY(I)
   60   FORMAT(20X,I8,' = ',2F10.2)
        PX0=PX0+PX(I)
        PY0=PY0+PY(I)
 2060 CONTINUE
      PX0=PX0/NOVERT
      PY0=PY0/NOVERT		! finds c of g. of box
C
CHEN>
       ENDIF
CHEN<
C
      WRITE(6,
     *'(''Writing Centre of gravity to file : boximage.tmp'')')
      OPEN(UNIT=3,FILE='boximage.tmp',STATUS='UNKNOWN')
      WRITE(3,*) PX0, PY0
      CLOSE(UNIT=3)
      DO 275 I=1,NOVERT
      PX(I)=1.00001*(PX(I)-PX0)+PX0
275   PY(I)=1.00001*(PY(I)-PY0)+PY0	! makes box bigger by 0.001% wrt c of g.
      DO 350 I=1,NOVERT
      IF(IFIX(PX(I))-PX(I)) 310,300,310
300   PX(I)=PX(I)+0.005			! makes vertex non-integer
310   IF(IFIX(PY(I))-PY(I)) 350,320,350
320   PY(I)=PY(I)+0.005			! makes vertex non-integer
350   CONTINUE
      PX(NOVERT+1) = PX(1)
      PY(NOVERT+1) = PY(1)
C
C     calculate max & min values of vertices, slopes & directions
C
      XMAX = PX(1)
      XMIN = XMAX
      YMAX = PY(1)
      YMIN = YMAX
      DO 2100 I=1,NOVERT
        X1 = PX(I)
        X2 = PX(I+1)
        Y1 = PY(I)
        Y2 = PY(I+1)
        IF(X1.GT.XMAX) XMAX = X1
        IF(X1.LT.XMIN) XMIN = X1
        IF(Y1.GT.YMAX) YMAX = Y1
        IF(Y1.LT.YMIN) YMIN = Y1
        IF(X1.EQ.X2) THEN
        SLOPE(I) = BIGNUM
        ELSE 
          SLOPE(I) = (Y1 - Y2)/(X1 - X2)
        END IF
        INCX(I) = .FALSE.
        ADDXY(I) = .FALSE.
        IF(ABS(SLOPE(I)).LE.1.0) THEN
C         increment in X
          INCX(I) = .TRUE.
          IF(X2.GT.X1) ADDXY(I) = .TRUE.
        ELSE
C         increment in Y
          IF(Y2.GT.Y1) ADDXY(I) = .TRUE.
        ENDIF
 2100 CONTINUE
C
C     set phase origin shifts to values as input.
C
      XORIGIN = OX
      YORIGIN = OY
 2140 CALL  IALORG(2,XORIGIN,YORIGIN,ZORIGIN)
      CALL  IALSIZ(2,IXYZ,MXYZ)
      CALL  FDATE(DAT)
CTSH      ENCODE(80,51,TITLE) DAT(5:24)
CTSH++
      WRITE(TMPTITLE,51) DAT(5:24)
CTSH--
   51 FORMAT('  BOXIMAGE : box off polygonal area without floating',
     . 5X,A20)
      CALL  IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
C     find mean density around edge of box (As in original JMS-VAX program)
C
      NTOT = 0
      DTOT = 0.
      DO 2200 I=1,NOVERT
        X1 = PX(I)
        IX1 = X1 + 0.5
        Y1 = PY(I)
        IY1 = Y1 + 0.5
        IX2 = PX(I+1) + 0.5
        IY2 = PY(I+1) + 0.5
        PITCH = SLOPE(I)
        IF(INCX(I)) GO TO 2180
C
C       increment in y
C
        IF(ADDXY(I)) GO TO 2170
C
C       subtract in Y
C
 2160   CALL  IMPOSN(1,0,IY1)
        CALL  IRDPAL(1,ALINE,IX1,IX1,*9000)
        NTOT = NTOT + 1
        DTOT = DTOT + ALINE(1)
        IY1 = IY1 - 1
        IF(IY1.LE.IY2) GO TO 2200
        X1 = X1 - 1./PITCH
        IX1 = X1 + 0.5
        GO TO 2160
C
C       add in Y
C
 2170   CALL  IMPOSN(1,0,IY1)
        CALL  IRDPAL(1,ALINE,IX1,IX1,*9000)
        NTOT = NTOT + 1
        DTOT = DTOT + ALINE(1)
        IY1 = IY1 + 1
        IF(IY1.GE.IY2) GO TO 2200
        X1 = X1 + 1./PITCH
        IX1 = X1 + 0.5
        GO TO 2170
C
C       increment in x
C
 2180   IF(ADDXY(I)) GO TO 2190
C
C       subtract in X
C
 2185   CALL  IMPOSN(1,0,IY1)
        CALL  IRDPAL(1,ALINE,IX1,IX1,*9000)
        NTOT = NTOT + 1
        DTOT = DTOT + ALINE(1)
        IX1 = IX1 - 1
        IF(IX1.LE.IX2) GO TO 2200
        Y1 = Y1 - PITCH
        IY1 = Y1 + 0.5
        GO TO 2185
C
C       add in X
C
 2190   CALL  IMPOSN(1,0,IY1)
        CALL  IRDPAL(1,ALINE,IX1,IX1,*9000)
        NTOT = NTOT + 1
        DTOT = DTOT + ALINE(1)
        IX1 = IX1 + 1
        IF(IX1.GE.IX2) GO TO 2200
        Y1 = Y1 + PITCH
        IY1 = Y1 + 0.5
        GO TO 2190
 2200 CONTINUE
      DMEAN = DTOT / NTOT
C
CHEN>
      if(IOUTZERO.eq.1)then
        DMEAN=0.0
      endif
CHEN<
C
C     test each individual point to see if in box
C
C     Do not float by subtracting mean around edge from all points inside
C     box, do not place image in bottom left corner. Instead replace all
C     points outside box by mean of values around edge.
C
      Y = YMIN 
      NX1=NX-1
      KX1 = XMIN + 0.5
      KX2 = XMAX + 0.5
      KX3 = KX2 + 1
      NY1=NY-1
      KY1 = YMIN + 0.5
      KY2 = YMAX + 0.5
      KY3 = KY2 + 1
      DMIN = 1.E7
      DMAX = 0.
      DTOT = 0.
      NPTS = 0
      DO 3248 L=1,NX
3248  ALINE(L)=DMEAN
      DO 3249 KY=0,KY1-1		!PUTS KY1 BLANK LINES OUT.
      NLINES=NLINES+1
3249  CALL IWRLIN(2,ALINE)
      CALL IMPOSN(1,0,KY1)
      DO 3250 KY=KY1,KY2
        CALL  IRDLIN(1,ALINE,*9000)
        L = 0
        X = 0.0
        DO 3200 KX=0,KX2
          L = L + 1
          LINE = 0
          DO 3100 J=1,NOVERT
            X1 = PX(J)
            X2 = PX(J+1)
            Y1 = PY(J)
            Y2 = PY(J+1)
C
C           test to see if point between x coords of line
C
            IF(X-X1) 3020,3050,3000
 3000       IF(X-X2) 3050,3100,3100
 3020       IF(X-X2) 3100,3100,3050
C
C           point lies between x coords
C
 3050       IF(Y.LT.Y1) GO TO 3060
            IF(Y.LT.Y2) GO TO 3070
            GO TO 3100
 3060       IF(Y.LT.Y2) GO TO 3090
C
C           edge point, test slope
C
 3070       IF(X1.EQ.X) THEN
              PSLOPE = BIGNUM
            ELSE
              PSLOPE = (Y1-Y) / (X1-X)
            END IF
            IF(X1.GT.X2) GO TO 3080
            IF(PSLOPE.GT.SLOPE(J)) GO TO 3100
            GO TO 3090
 3080       IF(PSLOPE.LT.SLOPE(J)) GO TO 3100
C
C           crossed a line increment
C
 3090       LINE = LINE + 1
 3100     CONTINUE
C
C         finished for that point, decide if inside or outside
C
          ILINE = LINE / 2
          ILINE = ILINE * 2
          IF(ILINE.EQ.LINE) THEN
C    
C           even no. crossed so outside
C
            ALINE(L) = DMEAN
          ELSE 
            NPTS = NPTS + 1
            AL = ALINE(L)		! used to be floated - DMEAN
            IF(AL.GT.DMAX) DMAX = AL
            IF(AL.LT.DMIN) DMIN = AL
            DTOT = DTOT + AL
C            ALINE(L) = AL		! used to be floated
          END IF
          X = X + 1.0 
 3200   CONTINUE
        DO 3201 IL=KX3,NX1
          L=L+1
3201    ALINE(L)=DMEAN
        IF(L.NE.NX) WRITE(6,3202)L,NX
3202    FORMAT(' **********NUMBER POINTS L, PUT IN LINE .NE. NX',2I10)
        CALL  IWRLIN(2,ALINE)
        NLINES = NLINES + 1
        Y = Y + 1.0
 3250 CONTINUE
C
C     pad in y dirn
C
      DO 3270 K=1,NX
        ALINE(K) = DMEAN
 3270 CONTINUE
      DO 3290 I=KY3,NY1
        CALL  IWRLIN(2,ALINE)
        NLINES = NLINES + 1
 3290 CONTINUE
      DBMEAN = DTOT / NPTS
C
CHEN>
      if(IOUTZERO.eq.1)then
        DBMEAN=0.0
C-------DMIN=0.0
      endif
CHEN<
C
      CALL  IWRHDR(2,TITLE,-1,DMIN,DMAX,DBMEAN)
C
C     o/p density params & close files
C
 4000 WRITE(6,80) NTOT,DMEAN
   80 FORMAT(/,/,' no of points around edge of box =',I10,
     1' average density =',F12.3)
      WRITE(6,90) NPTS,DMIN,DMAX,DBMEAN
   90 FORMAT(/,/,' number of points inside box =',I10,/,/,
     1 ' min,max,mean density inside box as written out =',3F12.3)
      WRITE(6,95) XORIGIN,YORIGIN
   95 FORMAT(/,' Phase origin stored with transform ',2F10.2,/,/,/,/)
      CALL IMCLOSE(1)
      CALL  IMCLOSE(2)
      IF(NLINES.NE.NY) WRITE(6,9001)NLINES,NY
9001  FORMAT(' ******* NLINES OUTPUT .NE. NY OF IMAGE',2I10)
      STOP
C
C     diagnostics
C
 9000 PRINT *,' end of data on reading image'
      STOP
      END
   
 

