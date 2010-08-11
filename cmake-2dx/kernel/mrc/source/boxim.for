C*BOXIM.FOR****************************************************************
C     Program to box off circular or polygonal area for Fourier transform
c         Input original densitometered image on stream 1 (IN)
C         Output boxed image on stream 2 (OUT)
C
C     version 2.0     27-AUG-82    JMS for VAX
C
C     version 2.1     JULY 84 mods to include input as grid units
C                     & check boundary values
C                     not checked out properly need to try a 512*512
C                     with box 261 & 260( only takes 259 to date)
C
C     version 3.0     FEB 85 rewritten method for polygonal boxes
C                     now add angles subtended by point with 2 vertices
C                     include point if they add to 360.This method only
C                     works for convex polygons.
C     version 4.0     FEB 85 algorithm rewritten for testing whether 
C                     point inside/outside box (Burton et al Computer
C                     Journal vol 27,no 4,1984 p 375) works for concave
C                     polygons. Further amendment expands the polygon
C                     by an infinitesimal quantity in the direction
C                     drawn between each vertex & the centroid.
C     version 4.05    04-04-00 zorigin inserted in ialorg for compatibility 
C                     with imsubs2000
C     version 4.06    28-01-02 correction to function inside - dimensions
C		      increased to novert+1 jms
C****************************************************************************
C
C       INPUT DATA:
C
C       FILIN  input image file name
C
C       FILOUT output image file name
C   
C       NXPAD,NYPAD,IPIXEL,NOVERT
C
C       NXPAD,NYPAD dimensions of output file in X & Y
C
C       IPIXEL  pixel size of input (output from TONE)
C
C       NOVERT number of vertices, 0 for circular box
C
C       OX,OY  phase origin position
C
C       if NOVERT = 0 then
C     
C       CX,CY,RAD  x,y coordinates of centre of circle & radius
C
C       else
C
C       IPX,IPY  x,y coordinates of vertices NOVERT of these
C
C**********************************************************************
C
      COMMON//NX,NY,NZ
      DIMENSION PX(21),PY(21),SLOPE(20),ALINE(10000),STORE(10000),
c     1          NXYZR(3),MXYZR(3),IXYZ(3),MXYZ(3),TITLE(20),
c     2          DISTSQ(20)
     1          NXYZR(3),MXYZR(3),IXYZ(3),MXYZ(3),TITLE(20)
      LOGICAL INCX(20),ADDXY(20)
      CHARACTER DAT*24
      CHARACTER*40 FILIN,FILOUT
CTSH++
      CHARACTER*80 TMPTITLE
      EQUIVALENCE (TMPTITLE, TITLE)
      LOGICAL INSIDE
CTSH--

C
      EQUIVALENCE (NX,NXYZR)
C
      RADIUS(X,Y) = SQRT(X*X + Y*Y)
C
      WRITE(6,*) ' BOXIM V4.06 Jan 28 2002 : '//
     . 'Box area for Fourier transform'
C
C     open files
C
      WRITE(6,*) ' # type in input file name'
      READ(5,5) FILIN
    5 FORMAT(A)
C
      WRITE(6,*) ' # type in output file name'
      READ(5,5) FILOUT
      CALL  IMOPEN(1,FILIN,'RO')
      CALL  IMOPEN(2,FILOUT,'NEW')
      CALL  IRDHDR(1,NXYZR,MXYZR,MODE,DMIN,DMAX,DMEAN)
C
C     read in parameters
C
      WRITE(6,*) ' # type in NXPAD,NYPAD,IPIXEL,NOVERT'
      READ(5,*) NXPAD,NYPAD,IPIXEL,NOVERT
      WRITE(6,10) NXPAD,NYPAD,IPIXEL,NOVERT
   10 FORMAT(/'  Boxed image padded to',I10,' by',I10,
     1 ' points'/'  Pixel size',I10,'  No of vertices',I10)
C
      BIGNUM = 10.0E+10
      IXYZ(1) = NXPAD
      IXYZ(2) = NYPAD
      IXYZ(3) = 1
      MXYZ(1) = 0
      MXYZ(2) = 0
      MXYZ(3) = 0
      NLINES = 0
      NXM1 = NX - 1
      NYM1 = NY - 1
      CALL  ICRHDR(2,IXYZ,IXYZ,MODE,TITLE,0)
      CALL  ITRLAB(2,1)
C
C     set scale from mm on TONE o/p to image samples
C
      IF(IPIXEL.NE.0) THEN
      SCALE = 1./(IPIXEL * .254)
      ELSE
      SCALE = 1.0
      END IF
C   
C     read in phase origin position
C
      WRITE(6,*) ' # type in OX,OY'
      READ(5,*) OX,OY
      OX = OX * SCALE
      OY = OY * SCALE
      IF(NOVERT.NE.0) GO TO 2000
C
C     circular box
C
      WRITE(6,*) ' # type in CX,CY,RAD'
      READ(5,*) CX,CY,RAD
      WRITE(6,30) CX,CY,RAD
   30 FORMAT(//'  Box centre =',2F10.1,' box radius =',F10.1)
      CX = CX * SCALE
      CY = CY * SCALE
      RAD = RAD * SCALE
C
C     check to see if circle in bounds
C
      ICHECK = CX + RAD + 0.5
      IF(ICHECK.GT.NXM1) GO TO 1080
      ICHECK = CX - RAD + 0.5
      IF(ICHECK.LT.0) GO TO 1080
      ICHECK = CY + RAD + 0.5
      IF(ICHECK.GT.NYM1) GO TO 1080
      ICHECK = CY - RAD + 0.5
      IF(ICHECK.LT.0) GO TO 1080
      GO TO 1090
C
 1080 WRITE(6,40) ICHECK,NX,NY
   40 FORMAT(' circle out of bounds, subscript value = ',
     . I5,' bounds = 0,',I5,' & 0,',I5)
      STOP
C
C     inside bounds, continue
C
 1090 RADSQ = RAD*RAD
c      JRAD = JINT(RAD)
      JRAD = INT(RAD)
C
C     apply phase origin corrections
C     non zero origin,calculate shift in phase origin
C
      IF(OX.EQ.0.AND.OY.EQ.0) GO TO 1100
      XORIGIN = OX - CX + RAD
      YORIGIN = OY - CY + RAD
      GO TO 1110
C
 1100 XORIGIN = RAD
      YORIGIN = RAD
C
 1110 CALL IALORG(2,XORIGIN,YORIGIN,ZORIGIN)
      CALL  IALSIZ(2,IXYZ,MXYZ)
      CALL  FDATE(DAT)
CTSH      WRITE(TITLE) DAT(5:24,50)
CTSH++
      WRITE(TMPTITLE,50) DAT(5:24)
CTSH--
   50 FORMAT('  BOXIM : box off selected area',20X,A20)
      CALL  IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
C     find mean density round edge of boxed area
C
c      NCALC = JINT(RAD + 0.5) * 2 + 1 
      NCALC = INT(RAD + 0.5) * 2 + 1 
      DTOT = 0.
      NTOT = 0
      IX = CX - RAD - 0.5
      COUNT = 0.
C
      DO 1120 I=1,NCALC
      RADI = ABS(RAD - COUNT)
c      IF(JINT(RADI).GE.JRAD) THEN
      IF(INT(RADI).GE.JRAD) THEN
      SQRTRD = 0.
C
      ELSE
      SQRTRD = SQRT(RADSQ - RADI*RADI)
      END IF
C
      COUNT = COUNT + 1.0
      IX = IX + 1
      IY1 = CY + SQRTRD + 0.5
      IY2 = CY - SQRTRD + 0.5
C
      CALL  IMPOSN(1,0,IY1)
      CALL  IRDPAL(1,ALINE,IX,IX,*9000)
C
      DTOT = DTOT + ALINE(1)
      NTOT = NTOT + 1
      IF(IY1.EQ.IY2) GO TO 1120
C
      CALL  IMPOSN(1,0,IY2)
      CALL  IRDPAL(1,ALINE,IX,IX,*9000)
C
      DTOT = DTOT + ALINE(1)
      NTOT = NTOT + 1
 1120 CONTINUE
      DMEAN = DTOT / NTOT
C
C     float by subtracting mean around edge from all points inside
C     box,place image bottom left corner & pad if necessary
C
      COUNT = CY - RAD 
      KY1 = COUNT + 0.5
      COUNT = ABS(COUNT - KY1)
      KY2 = CY + RAD + 0.5
      KY3 = KY2 -KY1 + 2
      DMIN = 1.E7
      DMAX = 0.
      DTOT = 0.
      NPTS = 0
C
      CALL  IMPOSN(1,0,KY1)
      DO 1160 KY=KY1,KY2
      RADI = ABS(RAD - COUNT)
c      IF(JINT(RADI).GE.JRAD) THEN
      IF(INT(RADI).GE.JRAD) THEN
      SQRTRD = 0.
      KX1 = CX + 1.5
      KX2 = KX1
C
      ELSE
      SQRTRD = SQRT(RADSQ - RADI*RADI)
C     ADD 1.5 because of array subscripts starting @ 1
      KX1 = CX - SQRTRD + 1.5
      KX2 = CX + SQRTRD + 1.5
      END IF
C
      KX3 = RAD - SQRTRD + 1.5
      CALL  IRDLIN(1,ALINE,*9000)
C
      DO 1130 K=KX1,KX2
      AL = ALINE(K) - DMEAN
      IF(AL.GT.DMAX) DMAX = AL
      IF(AL.LT.DMIN) DMIN = AL
      DTOT = DTOT + AL
      STORE(K) = AL
      NPTS = NPTS + 1
 1130 CONTINUE
C
C     position data correctly
C
      DO 1140 J=1,NXPAD
      ALINE(J) = 0.
 1140 CONTINUE
C
      DO 1150 K=KX1,KX2
      ALINE(KX3) = STORE(K)
      KX3 = KX3 + 1
 1150 CONTINUE
C
      CALL  IWRLIN(2,ALINE)
      COUNT = COUNT + 1.0 
      NLINES = NLINES + 1
 1160 CONTINUE
C
C     pad lines in y direction
C
      DO 1180 K=1,NXPAD
      ALINE(K) = 0.
 1180 CONTINUE
C
      DO 1200 I=KY3,NYPAD
      CALL  IWRLIN(2,ALINE)
      NLINES = NLINES + 1
 1200 CONTINUE
C
      DBMEAN = DTOT / NPTS
      CALL  IWRHDR(2,TITLE,-1,DMIN,DMAX,DBMEAN)
      GO TO 4000
C
C     polygonal box
C
 2000 IF(NOVERT.LE.20) GO TO 2040
      WRITE(6,*) ' too many vertices for box'
      STOP
C
C     read in vertices
C
 2040 WRITE(6,*) ' # type in vertices for box'
      DO 2060 I=1,NOVERT
      READ(5,*) IPX,IPY
      PX(I) = FLOAT(IPX) * SCALE
      PY(I) = FLOAT(IPY) * SCALE
C
C     check vertices inside bounds
C
      ICHECK = PX(I) + 0.5
      IF(ICHECK.LE.NXM1) GO TO 2050
      WRITE(6,62) I,ICHECK,NX
   62 FORMAT(' X vertex',I2,' out of bounds =',I8,' reset to',I8)
      PX(I) = NXM1
C
 2050 IF(ICHECK.GE.0) GO TO 2052
      WRITE(6,64) I,ICHECK
   64 FORMAT(' X vertex',I2,' out of bounds =',I8,' reset to 0')
      PX(I) = 0
C
 2052 ICHECK = PY(I) + 0.5
      IF(ICHECK.LE.NYM1) GO TO 2054
      WRITE(6,66) I,ICHECK,NY
   66 FORMAT(' Y vertex',I2,' out of bounds =',I8,' reset to',I8)
      PY(I) = NYM1
C
 2054 IF(ICHECK.GE.0) GO TO 2060
      WRITE(6,68) I,ICHECK,NY
   68 FORMAT(' Y vertex',I2,' out of bounds =',I8,' reset to 0')
      PY(I) = 0
 2060 CONTINUE
C
      PX(NOVERT+1) = PX(1)
      PY(NOVERT+1) = PY(1)
C
C     calculate max & min values of vertices, slopes & directions
C
      XMAX = PX(1)
      XMIN = XMAX
      YMAX = PY(1)
      YMIN = YMAX
C
      DO 2100 I=1,NOVERT
      X1 = PX(I)
      X2 = PX(I+1)
      Y1 = PY(I)
      Y2 = PY(I+1)
      IF(X1.GT.XMAX) XMAX = X1
      IF(X1.LT.XMIN) XMIN = X1
      IF(Y1.GT.YMAX) YMAX = Y1
      IF(Y1.LT.YMIN) YMIN = Y1
C
      IF(X1.EQ.X2) THEN
      SLOPE(I) = BIGNUM
C
      ELSE 
      SLOPE(I) = (Y1 - Y2)/(X1 - X2)
      END IF
C
      INCX(I) = .FALSE.
      ADDXY(I) = .FALSE.
      IF(ABS(SLOPE(I)).GT.1.0) GO TO 2080
C
C     increment in X
C
      INCX(I) = .TRUE.
      IF(X2.GT.X1) ADDXY(I) = .TRUE.
      GO TO 2100
C
C     increment in Y
C
 2080 IF(Y2.GT.Y1) ADDXY(I) = .TRUE.
 2100 CONTINUE
C
C     correct phase origin shifts
C     non zero origin, calculate shift
C
      IF(OX.EQ.0.AND.OY.EQ.0) GO TO 2120
      XORIGIN = OX - XMIN
      YORIGIN = OY - YMIN
      GO TO 2140
C
 2120 XORIGIN = (XMAX - XMIN)/2.
      YORIGIN = (YMAX - YMIN)/2.
C
 2140 CALL  IALORG(2,XORIGIN,YORIGIN,ZORIGIN)
      CALL  IALSIZ(2,IXYZ,MXYZ)
      CALL  FDATE(DAT)
CTSH      WRITE(TITLE) DAT(5:24,50)
CTSH++
      WRITE(TMPTITLE,50) DAT(5:24)
CTSH--
      CALL  IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
C     find mean density around edge of box
C
      NTOT = 0
      DTOT = 0.
C
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
C     increment in y
C
      IF(ADDXY(I)) GO TO 2170
C
C     subtract in Y
C
 2160 CALL  IMPOSN(1,0,IY1)
      CALL  IRDPAL(1,ALINE,IX1,IX1,*9000)
      NTOT = NTOT + 1
      DTOT = DTOT + ALINE(1)
      IY1 = IY1 - 1
      IF(IY1.LE.IY2) GO TO 2200
C
      X1 = X1 - 1./PITCH
      IX1 = X1 + 0.5
      GO TO 2160
C
C     add in Y
C
 2170 CALL  IMPOSN(1,0,IY1)
      CALL  IRDPAL(1,ALINE,IX1,IX1,*9000)
      NTOT = NTOT + 1
      DTOT = DTOT + ALINE(1)
      IY1 = IY1 + 1
      IF(IY1.GE.IY2) GO TO 2200
C
      X1 = X1 + 1./PITCH
      IX1 = X1 + 0.5
      GO TO 2170
C
C     increment in x
C
 2180 IF(ADDXY(I)) GO TO 2190
C
C     subtract in X
C
 2185 CALL  IMPOSN(1,0,IY1)
      CALL  IRDPAL(1,ALINE,IX1,IX1,*9000)
      NTOT = NTOT + 1
      DTOT = DTOT + ALINE(1)
      IX1 = IX1 - 1
      IF(IX1.LE.IX2) GO TO 2200
C
      Y1 = Y1 - PITCH
      IY1 = Y1 + 0.5
      GO TO 2185
C
C     add in X
C
 2190 CALL  IMPOSN(1,0,IY1)
      CALL  IRDPAL(1,ALINE,IX1,IX1,*9000)
      NTOT = NTOT + 1
      DTOT = DTOT + ALINE(1)
      IX1 = IX1 + 1
      IF(IX1.GE.IX2) GO TO 2200
C
      Y1 = Y1 + PITCH
      IY1 = Y1 + 0.5
      GO TO 2190
 2200 CONTINUE
      DMEAN = DTOT / NTOT
C
C     test each individual point to see if in box
C
C
C     float by subtracting mean around edge from all points inside
C     box,place image bottom left corner & pad if necessary
C
C     calculate centroid
C
      PXSUM = 0.
      PYSUM = 0.
      DO I=1,NOVERT
      PXSUM = PX(I) + PXSUM
      PYSUM = PY(I) + PYSUM
      END DO
C
      CENX = PXSUM / FLOAT(NOVERT)
      CENY = PYSUM / FLOAT(NOVERT)
C
C     expand polygon
C
      WRITE(6,*) ' Expanded polygon coordinates'
      DO I=1,NOVERT
      SCALE = RADIUS((PX(I) - CENX), (PY(I) - CENY))
      IF(SCALE.NE.0.) SCALE = 1. / (8192. * SCALE)
      PX(I) = PX(I) + SCALE * (PX(I) - CENX)
      PY(I) = PY(I) + SCALE * (PY(I) - CENY)
      WRITE(6,60)PX(I),PY(I)
   60 FORMAT(20X,2F15.8)
      END DO
      PX(NOVERT+1) = PX(1)
      PY(NOVERT+1) = PY(1)
C
      Y = YMIN 
      KX1 = XMIN + 0.5
      KX2 = XMAX + 0.5
      KX3 = KX2 + 1
      KY1 = YMIN + 0.5
      KY2 = YMAX + 0.5
      KY3 = KY2 - KY1 + 1
      KY4 = NYPAD - 1
      DMIN = 1.E7
      DMAX = 0.
      DTOT = 0.
      NPTS = 0
C
      CALL IMPOSN(1,0,KY1)
      DO 3250 KY=KY1,KY2
      CALL  IRDPAL(1,ALINE,KX1,KX2,*9000)
      L = 0
      X = XMIN 
C
      DO 3200 KX=KX1,KX2
      L = L + 1
      IF(INSIDE(X,Y,NOVERT,PX,PY)) THEN
      NPTS = NPTS + 1
      AL = ALINE(L) - DMEAN
      IF(AL.GT.DMAX) DMAX = AL
      IF(AL.LT.DMIN) DMIN = AL
      DTOT = DTOT + AL
      ALINE(L) = AL
C
      ELSE 
      ALINE(L) = 0.
      NPTS = NPTS + 1
      END IF
C
      X = X + 1.0 
 3200 CONTINUE
C
      CALL  IWRLIN(2,ALINE)
      NLINES = NLINES + 1
      Y = Y + 1.0
 3250 CONTINUE
C
C     pad in y dirn
C
      DO 3270 K=1,NXPAD
      ALINE(K) = 0.
 3270 CONTINUE
C
      DO 3290 I=KY3,KY4
      CALL  IWRLIN(2,ALINE)
      NLINES = NLINES + 1
 3290 CONTINUE
C
      DBMEAN = DTOT / NPTS
      CALL  IWRHDR(2,TITLE,-1,DMIN,DMAX,DBMEAN)
C
C     o/p density params & close files
C
 4000 WRITE(6,80) NTOT,DMEAN
   80 FORMAT(/' no of points around edge of box =',I10,
     1' average density =',F10.1)
      WRITE(6,90) NPTS,DMIN,DMAX,DBMEAN
   90 FORMAT(/' number of points inside box =',I10/
     1 ' min,max,mean density inside box after floating =',3F10.1)
      WRITE(6,95) XORIGIN,YORIGIN
   95 FORMAT(/' Phase origin stored with transform ',2F10.2/)
C
      CALL IMCLOSE(1)
      CALL  IMCLOSE(2)
      STOP
C
C     diagnostics
C
 9000 WRITE(6,*) ' end of data on reading image'
      STOP
      END
C
C*************************
C 
      LOGICAL FUNCTION INSIDE(X,Y,NOVERT,PX,PY)
      DIMENSION PX(NOVERT+1),PY(NOVERT+1)
      LOGICAL CROSS
      INSIDE = .FALSE.
      DO 10 I=1,NOVERT
      IF(CROSS(X,Y,PX(I),PY(I),PX(I+1),PY(I+1))) 
     1    INSIDE = .NOT.INSIDE
   10 CONTINUE
      RETURN
      END
C
C****************************
C
      LOGICAL FUNCTION CROSS(X,Y,X1,Y1,X2,Y2)
      IF(((Y.LT.Y1).EQV.(Y.LT.Y2)).OR.(X.GE.X1.AND.X.GE.X2)) THEN
      CROSS = .FALSE.
      ELSE IF(X.LT.X1.AND.X.LT.X2)THEN
      CROSS = .TRUE.
      ELSE IF(X1.LT.X2) THEN
      CROSS = X.LT.(X1 + (Y-Y1) * (X2-X1) / (Y2-Y1))
      ELSE
      CROSS = X.LT.(X2 + (Y-Y2) * (X1-X2) / (Y1-Y2))
      END IF
      RETURN
      END
