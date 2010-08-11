C*TRMASK.FOR******************************************************************
C     Program for masking a Fourier transform                                *
C     Input transform in standard format on stream 1 (IN)                    *
C     Output masked transform in standard format on stream 2 (OUT)           *
C     Control file contains list of holes in mask on stream 3 (MASK)         *
C                                                                            *
C     Mask file in free format  -  first record  ISHAPE,IPIXEL               *
C     ISHAPE=1 Hard edge circular holes                                      *
C          2 Soft edge circular holes (Gaussian weighted to EXP(-2) at edge) *
C          3 Hard edge rectangular holes                                     *
C     IPIXEL specifies pixel size in TONE display of transform in 1/100 inch *
C          If IPIXEL=0 then XC etc taken to be in transform steps.
C                                                                            *
C     Followed by record for each spot (IH,IK) of form                       *
C         IH,IK,XC,YC,RAD         (ISHAPE = 1 or 2)                          *
C         IH,IK,XC,YC,DELX,DELY   (ISHAPE = 3)                               *
C       Hole centre (XC,YC), radius RAD (circular holes), half edge lengths  *
C       (DELX,DELY) (rectangular holes), all in mm.  XC,YC measured from     *
C       F(0,0), WITH X pos across page to right, Y pos up page, from pixel   *
C       centre to pixel centre.                                              *
C                                                                            *
C                                                                            *
C     Version 1.03   2-APR-82        RAC        FOR VAX                      *
C     Version 1.04   22-SEP-82    To do stack of sections                    *
C     Version 1.05   14-AUG_85    Allow overlapping holes                    *
C     Version 2.0    11-NOV-00    RAC  Link with imsubs2000                  *
C                                                                            *
C*****************************************************************************
C
      COMMON//NX,NY,NZ
      DIMENSION ARRAY(1050624),TITLE(20),NXYZR(3),MXYZR(3),XC(500),
     1YC(500),RAD(500),DELX(500),DELY(500)
CTSH++
      CHARACTER*80 TMPTITLE
      EQUIVALENCE (TMPTITLE,TITLE)
CTSH--
      CHARACTER DAT*24
      EQUIVALENCE (NX,NXYZR)
      LOGICAL OVERLAP
      DATA ZERO/0.0/
C
      WRITE(6,1000)
1000  FORMAT(//'   TRMASK: Transform masking program V1.03',//)
C
      CALL IMOPEN(1,'IN','RO')
      CALL IMOPEN(2,'OUT','NEW')
      CALL CCPDPN(3,'MASK','READONLY','F',0,0)
      CALL FDATE(DAT)
      CALL IRDHDR(1,NXYZR,MXYZR,MODE,DMIN,DMAX,DMEAN)
      CALL ICRHDR(2,NXYZR,MXYZR,4,TITLE,0)
      CALL ITRLAB(2,1)
CTSH      WRITE(TITLE) DAT(5:24,1500)
CTSH++
      WRITE(TMPTITLE,1500) DAT(5:24)
CTSH--
1500  FORMAT('TRMASK: Transform masking program',18X,A20)
      CALL IWRHDR(2,TITLE,1,ZERO,ZERO,ZERO)
C
C     Read in mask params on input file 3.    
      WRITE(6,1600)
1600  FORMAT(///,'   Parameters of mask')
      READ(3,*) ISHAPE,IPIXEL
      WRITE(6,1700) ISHAPE,IPIXEL
1700  FORMAT(/,'   Hole shape',I10,'  Pixel size',I10,
     1/'   Hole parameters')
      NHOLE=0
C     Set scale factor to convert mm. on TONE to transform samples
C     1/100 inch = 0.254 mm.
C     If IPIXEL=0 use transform steps by setting SCALE=1
      IF(IPIXEL.EQ.0) THEN
           SCALE=1.
        ELSE
           SCALE=0.254*IPIXEL
      ENDIF
      IF(ISHAPE.EQ.3) GO TO 120
110   NHOLE=NHOLE+1
C
      IF (NHOLE.GT.500) GOTO 2001
C
      READ(3,*,END=100) IH,IK,XC(NHOLE),YC(NHOLE),RAD(NHOLE)
      WRITE(6,1800) IH,IK,XC(NHOLE),YC(NHOLE),RAD(NHOLE)
1800  FORMAT(5X,2I5,3F10.4)
      GO TO 110
C
120   NHOLE=NHOLE+1
C
      IF (NHOLE.GT.500) GOTO 2001
C
      READ(3,*,END=100) IH,IK,XC(NHOLE),YC(NHOLE),DELX(NHOLE),
     1DELY(NHOLE)
      WRITE(6,1810) IH,IK,XC(NHOLE),YC(NHOLE),DELX(NHOLE),
     1DELY(NHOLE)
1810  FORMAT(5X,2I5,4F10.4)
      GO TO 120
C
100   NHOLE=NHOLE-1
      NX1=NX-1
      NXP2=2*NX
      NY2=NY/2
      NY21=NY2+1
      NZ1=NZ-1
      TMIN=1.E10
      TMAX=-1.E10
      TMEAN=0.
      OVERLAP=.FALSE.
C     Sections loop
      DO 500 ISEC=0,NZ1
C     Read in section of transform
      CALL IRDSEC(1,ARRAY,*99)
      DO 200 NH=1,NHOLE
C
C     Set hole centre
      XXC=XC(NH)/SCALE
      YYC=NY2+YC(NH)/SCALE
      IXC=XXC+0.5
      IYC=YYC+0.5
      IF(ISHAPE.EQ.3) GO TO 150
C
C     Set hole limits
      RA=RAD(NH)/SCALE
      RADSQ=RA*RA
      IRAD=RA+0.5
      IX1=IXC-IRAD
      IX2=IXC+IRAD
      IY1=IYC-IRAD
      IY2=IYC+IRAD
      GO TO 160
C
150   XSIDE2=DELX(NH)/SCALE
      YSIDE2=DELY(NH)/SCALE
      IXS=XSIDE2+0.5
      IYS=YSIDE2+0.5
      IX1=IXC-IXS
      IX2=IXC+IXS
      IY1=IYC-IYS
      IY2=IYC+IYS
C
C     Scan over hole
160   DO 300 IY=IY1,IY2
      YSQ=(IY-YYC)**2
C     IX can be negative
      DO 310 IX=IX1,IX2
      IF(ISHAPE.EQ.3) GO TO 290
      RSQ=(IX-XXC)**2+YSQ
      IF(RSQ.GT.RADSQ) GO TO 310
      IF(ISHAPE.EQ.2) GWT=EXP(-(2.*RSQ)/RADSQ)
C     Check if point in neg X half transform - use Friedel mate
290   IF(IX.GE.0) THEN
        INDEX=IY*NXP2+2*IX+1
      ELSE
        INDEX=(NY-IY)*NXP2-2*IX+1
      END IF
C     Check not negative half of F(0,0) centre hole
      IF((IX.LT.0).AND.(XC(NH).EQ.0.).AND.(YC(NH).EQ.0.)) GO TO 310
C
C     Mark points within mask by scaling
      ARRAY(INDEX)=ARRAY(INDEX)*1.E10
      ARRAY(INDEX+1)=ARRAY(INDEX+1)*1.E10
      IF(ISHAPE.NE.2) GO TO 320
C     Gaussian weight for soft holes
      ARRAY(INDEX)=ARRAY(INDEX)*GWT
      ARRAY(INDEX+1)=ARRAY(INDEX+1)*GWT
C
C     On IX=0 need another segment
320   IF(IX.NE.0) GO TO 310
C     But not for centre hole
      IF((XC(NH).EQ.0.).AND.(YC(NH).EQ.0.)) GO TO 310
      INDEX=(NY-IY)*NXP2+1
      ARRAY(INDEX)=ARRAY(INDEX)*1.E10
      ARRAY(INDEX+1)=ARRAY(INDEX+1)*1.E10
      IF(ISHAPE.NE.2) GO TO 310
      ARRAY(INDEX)=ARRAY(INDEX)*GWT
      ARRAY(INDEX+1)=ARRAY(INDEX+1)*GWT
C
310   CONTINUE
C
300   CONTINUE
C
200   CONTINUE
C
C     Scan transform and set everything outside mask to zero
      NTOT=2*NX*NY
      DO 400 N=1,NTOT
      IF(ABS(ARRAY(N)).LT.1.E10) THEN
        ARRAY(N)=0.
      ELSE
        ARRAY(N)=ARRAY(N)*1.E-10
390     IF(ARRAY(N).GT.1.E10) THEN
           ARRAY(N)=ARRAY(N)*1.E-10
           OVERLAP=.TRUE.
           GO TO 390
        END IF
      END IF
400   CONTINUE
C
C     Write masked transform back to disc
      CALL IWRSEC(2,ARRAY)
      CALL ICLCDN(ARRAY,NX,NY,1,NX,1,NY,SMIN,SMAX,SMEAN)
      IF(NZ.GT.1) WRITE(6,1910) ISEC,SMIN,SMAX,SMEAN
1910  FORMAT(//'  Min max and mean values on section',I5,
     1'     are',3G13.5)
      IF(SMAX.GT.TMAX) TMAX=SMAX
      IF(SMIN.LT.TMIN) TMIN=SMIN
      TMEAN=TMEAN+SMEAN
500   CONTINUE
C
      TMEAN=TMEAN/NZ
      CALL IWRHDR(2,TITLE,-1,TMIN,TMAX,TMEAN)
      IF(OVERLAP) WRITE(6,2010)
2010  FORMAT(///'  N.B.  MASK HAS OVERLAPPING HOLES'///)
      WRITE(6,1900) TMIN,TMAX,TMEAN
1900  FORMAT(//,'   Overall min, max and mean masked transform 
     1 values are:   ',3G13.5)
      CALL IMCLOSE(1)
      CALL IMCLOSE(2)
      CALL EXIT
C
2001	WRITE (6,2002)
2002	FORMAT(///' MORE THAN 500 HOLES.PROGRAM STOPS.')
	STOP
C
99    WRITE(6,2000)
2000  FORMAT(///'  END OF FILE ON INPUT STREAM 1')
      CALL EXIT
      END
