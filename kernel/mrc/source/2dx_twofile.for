C*TWOFILE.FOR**************************************************************
C     Program to perform simple additions or multiplications on 2 images of
C      same dimensions and type (i.e. both must be real or both complex).
C
C     Data   ICOMB
C            WT1 , WT2   (Only needed if ICOMB=0)
C            IORIGIN,ORIGXA,ORIGYA,ORIGXB,ORIGYB  (Only needed for complex)
C
C       ICOMB=-1 Divide two image densities pointwise real or complex.
C       ICOMB=0  Linear combination (Addition) with weights WT1, WT2
C       ICOMB=1  Multiply two image densities pointwise, real or complex.
C                 and calculate correlation coefficient
C       ICOMB=2  Multiply first image point by complex conjugate of second
C                 image point (only useful in complex image(Transform)).
C       ICOMB=3  Minimum of the two input images
C       ICOMB=4  Maximum of the two input images
C       ICOMB=5  Amplitude-limit the first image with the info of the second image (0...1)
C
C       WT1,WT2  Weights for addition (ICOMB=0)   Result = (A*WT1 + B*WT2)
C
C       IORIGIN  If 0, do not apply any extra origin shift (to complex images)
C                If 1, apply origin shift ORIGXA,ORIGYA to image A(Stream IN1)
C                If 2, apply origin shift ORIGXB,ORIGYB to image B(Stream IN2)
C                If 3, apply origin shifts to images A and B
C
C        First image on stream 1 (IN1)
C        Second image on stream 2 (IN2)
C        Added or Multiplied image output on stream 3 (OUT)
C
C      VERSION 1.01     25-MAY-82   RAC     FOR VAX
C              1.02      4-JUL-84   RH      generalised for real and complex.
C              1.03     28-MAY-87   RH      Real*8 for DMEAN
C              2.00     01-Jan-92   RH      convert to UNIX for Alliant
C              2.01     10-Mar-95   RH      debug DMEAN double precision
C              2.02     13-Feb-96   RH      increase dimension to 16000
C              2.03     20-May-96   RH      calculate correlation coefficient
C              2.04     24-Feb-98   RH      add MIN and MAX functions
C              3.00      2-Jul-00   RH      generalise to 3D maps 
C              3.01     14-Aug-00   RH      debug - transforms have NZ=1
C
C              Dimensions set for max line length 20000
      PARAMETER (MAXDIM=20000)
      COMMON//NX,NY,NZ
      DIMENSION ARR1(MAXDIM),ARR2(MAXDIM),ARR3(MAXDIM),NXYZ1(3),
     . MXYZ1(3),NXYZ2(3),MXYZ2(3),TITLE(20),LABEL(20,20)
      REAL*8 DMEAN, CORTOP,CORBOT1,CORBOT2
      CHARACTER DAT*24
      EQUIVALENCE (NX,NXYZ1)
CTSH++
      CHARACTER*80 TMPTITLE
      EQUIVALENCE (TMPTITLE,TITLE)
CTSH--
      DATA ZERO/0./
C
      WRITE(6,1000)
1000  FORMAT(////' TWOFILE VX3.01(14-Aug-00) (2dx version):',
     . ' Image or Transform combining program')
      CALL IMOPEN(1,'IN1','RO')
      CALL IMOPEN(2,'IN2','RO')
      CALL IMOPEN(3,'OUT','NEW')
      CALL IRDHDR(1,NXYZ1,MXYZ1,MODE1,DMIN1,DMAX1,DMEAN1)
      CALL IRDHDR(2,NXYZ2,MXYZ2,MODE2,DMIN2,DMAX2,DMEAN2)
      CALL ITRHDR(3,1)
      IF(NXYZ1(1).NE.NXYZ2(1).OR.NXYZ1(2).NE.NXYZ2(2).OR.
     . NXYZ1(3).NE.NXYZ2(3)) GO TO 100
      MOD1=MODE1/3              ! 0,Real and 1,Complex 
      MOD2=MODE2/3              ! 0,Real and 1,Complex
      IF(MOD1.EQ.MOD2) GO TO 140
100   WRITE(6,1010)
1010  FORMAT(///' Terminate - Images not of same size or type')
      STOP
C
140   READ(5,*) ICOMB
      IF(ICOMB.EQ.0) READ(5,*) WT1,WT2
      IF(MOD1.EQ.1) THEN
        READ(5,*)IORIGIN,ORIGXA,ORIGYA,ORIGXB,ORIGYB
          NX1 = NX-1
          TWOPI = 6.283185
          DELPX1 = -TWOPI * ORIGXA / (2.0 * NX1)
          DELPY1 = -TWOPI * ORIGYA / NY
          DELPX2 = -TWOPI * ORIGXB / (2.0 * NX1)
          DELPY2 = -TWOPI * ORIGYB / NY
        IF(IORIGIN.EQ.0)WRITE(6,1012)
        IF(IORIGIN.GE.1)WRITE(6,1013)IORIGIN,
     . ORIGXA,ORIGYA,ORIGXB,ORIGYB
1012    FORMAT(' No origin shifts applied to Transforms before',
     . ' combining')
1013    FORMAT(' Origin shifts applied to Transforms'/' IORIGIN =',
     . I5/' ORIGXA  =',F8.1/' ORIGYA  =',F8.1/
     . ' ORIGXB  =',F8.1/' ORIGYB  =',F8.1)
      ENDIF
C
C
C  checks for sensible requests
      IF(MOD1.EQ.0.AND.ICOMB.EQ.2) THEN
        WRITE(6,1011)
1011    FORMAT(///' Terminate - ICOMB=2 and Real image files')
        STOP
      ENDIF
      IF(MOD1.EQ.1) THEN
        IF(ICOMB.EQ.3.OR.ICOMB.EQ.4) THEN
          WRITE(6,1014)
1014      FORMAT(/,/,/,' Terminate - ICOMB=3 or 4 must ',
     .     'have Real image files')
          STOP
        ENDIF
        IF(NXYZ1(3).NE.1) THEN
                WRITE(6,1015)
1015            FORMAT(///' Terminate - 3D maps must be real at present')
                STOP
        ENDIF
      ENDIF
C
C
C     Combine label lists from 2 images, keeping first 9 only
      CALL IRTLAB(1,LABEL(1,1),NL1)
      CALL IRTLAB(2,LABEL(1,NL1+1),NL2)
      CALL IALLAB(3,LABEL,MIN(9,NL1+NL2))
      CALL FDATE(DAT)
CTSH      IF(ICOMB.EQ.0) WRITE(TITLE) WT1,WT2,DAT(5:24,1500)
CTSH      IF(ICOMB.EQ.-1) WRITE(TITLE) DAT(5:24,1530)
CTSH      IF(ICOMB.EQ.1) WRITE(TITLE) DAT(5:24,1510)
CTSH      IF(ICOMB.EQ.2) WRITE(TITLE) DAT(5:24,1520)
CTSH      IF(ICOMB.EQ.3) WRITE(TITLE) DAT(5:24,1540)
CTSH      IF(ICOMB.EQ.4) WRITE(TITLE) DAT(5:24,1550)
CTSH++
      IF(ICOMB.EQ.0) WRITE(TMPTITLE,1500) WT1,WT2,DAT(5:24)
      IF(ICOMB.EQ.-1) WRITE(TMPTITLE,1530) DAT(5:24)
      IF(ICOMB.EQ.1) WRITE(TMPTITLE,1510) DAT(5:24)
      IF(ICOMB.EQ.2) WRITE(TMPTITLE,1520) DAT(5:24)
      IF(ICOMB.EQ.3) WRITE(TMPTITLE,1540) DAT(5:24)
      IF(ICOMB.EQ.4) WRITE(TMPTITLE,1550) DAT(5:24)
      IF(ICOMB.EQ.5) WRITE(TMPTITLE,1560) DAT(5:24)
CTSH--
1500  FORMAT(' TWOFILE : Images added with weights   ',2F6.1,9X,A20)
1510  FORMAT(' TWOFILE : Files simply multiplied AxB  ',20X,A20)
1520  FORMAT(' TWOFILE : Transforms multiplied AxB*   ',20X,A20)
1530  FORMAT(' TWOFILE : Files divided A/B,0 if B=0   ',20X,A20)
1540  FORMAT(' TWOFILE : Minimum of numbers substituted    ',15X,A20)
1550  FORMAT(' TWOFILE : Maximum of numbers substituted    ',15X,A20)
1560  FORMAT(' TWOFILE : Amplitude-limit A with B     ',20X,A20)
      CALL IWRHDR(3,TITLE,1,ZERO,ZERO,ZERO)
C
      DMIN=1.E10
      DMAX=-1.E10
      DMEAN=0.
      CORTOP=0.
      CORBOT1=0.
      CORBOT2=0.
      CALL IMPOSN(1,0,0)
      DO 199 IZ=1,NZ
        DO 200 IY=1,NY
          CALL IRDLIN(1,ARR1,*99)
          IF(MOD1.EQ.1.AND.(IORIGIN/2)*2.NE.IORIGIN) THEN
            IF(IY.EQ.1)WRITE(6,201)ORIGXA,ORIGYA
201         FORMAT('  PHASE ORIGIN FOR (IN1)IMAGE-A MOVED TO',2F10.1,
     .        '  BEFORE COMBINATION')
            CALL PHSHFT(ARR1,DELPX1,DELPY1,IY)
          ENDIF
          CALL IRDLIN(2,ARR2,*99)
          IF(MOD2.EQ.1.AND.IORIGIN.GE.2) THEN
            IF(IY.EQ.1)WRITE(6,202)ORIGXB,ORIGYB
202         FORMAT('  PHASE ORIGIN FOR (IN2)IMAGE-B MOVED TO',2F10.1,
     .        '  BEFORE COMBINATION')
            CALL PHSHFT(ARR2,DELPX2,DELPY2,IY)
          ENDIF
          IF(MOD1.EQ.0) THEN
C-----------Real image
            DO 210 IX=1,NX
              IF(ICOMB.EQ.-1) THEN
                IF(ARR2(IX).EQ.0.0) THEN
                  ARR3(IX)=0.0
                ELSE
                  ARR3(IX)=ARR1(IX)/ARR2(IX)
                ENDIF
              ENDIF
              IF(ICOMB.EQ.0) ARR3(IX)=WT1*ARR1(IX)+WT2*ARR2(IX)
              IF(ICOMB.EQ.1) THEN
                ARR3(IX)=ARR1(IX)*ARR2(IX)
                CORTOP=CORTOP+ARR3(IX)
                CORBOT1=CORBOT1+ARR1(IX)**2
                CORBOT2=CORBOT2+ARR2(IX)**2
              ENDIF
              IF(ICOMB.EQ.5) THEN
                ARR3(IX)=(ARR1(IX)-DMEAN1)*ARR2(IX)+DMEAN1
              ENDIF
              IF(ICOMB.EQ.3) ARR3(IX)=AMIN1(ARR1(IX),ARR2(IX))
              IF(ICOMB.EQ.4) ARR3(IX)=AMAX1(ARR1(IX),ARR2(IX))
              IF(ARR3(IX).LT.DMIN) DMIN=ARR3(IX)
              IF(ARR3(IX).GT.DMAX) DMAX=ARR3(IX)
210         DMEAN=DMEAN+ARR3(IX)
          ENDIF
          IF(MOD1.EQ.1) THEN
            if(ICOMB.eq.5) THEN
              write(*,'('':: ERROR: 2dx_twofile: ICOMB=5 only'',
     .          '' for real images.'')')
              write(*,'('':: ERROR: 2dx_twofile: ICOMB=5 only'',
     .          '' for real images.'')')
              write(*,'('':: ERROR: 2dx_twofile: ICOMB=5 only'',
     .          '' for real images.'')')
              STOP
            endif
C-----------Complex image (FFT)
            DO 310 IX=1,NX
              JX=2*IX-1
              IF(ICOMB.EQ.0)ARR3(JX)=WT1*ARR1(JX)+WT2*ARR2(JX)
              IF(ICOMB.EQ.0)ARR3(JX+1)=WT1*ARR1(JX+1)+WT2*ARR2(JX+1)
              IF(ICOMB.EQ.1) THEN
                ARR3(JX)=ARR1(JX)*ARR2(JX)-ARR1(JX+1)*ARR2(JX+1)
                ARR3(JX+1)=ARR1(JX+1)*ARR2(JX)+ARR1(JX)*ARR2(JX+1)
              ENDIF
              IF(ICOMB.EQ.2)THEN
                ARR3(JX)=ARR1(JX)*ARR2(JX)+ARR1(JX+1)*ARR2(JX+1)
              ENDIF
              IF(ICOMB.EQ.2)THEN
                ARR3(JX+1)=ARR1(JX+1)*ARR2(JX)-ARR1(JX)*ARR2(JX+1)
              ENDIF
              IF(ICOMB.EQ.-1) THEN
                DENOM=ARR2(JX)**2+ARR2(JX+1)**2
                IF(DENOM.EQ.0.0) THEN
                  ARR3(JX)=0.0
                  ARR3(JX+1)=0.0
                ELSE
C                 (a+ib)/(c+id)=(ac+bd)/(c**2+d**2) + i(bc-ad)/(c**2+d**2)
                  ARR3(JX)=(ARR1(JX)*ARR2(JX)+ARR1(JX+1)*ARR2(JX+1))/
     .              DENOM
                  ARR3(JX+1)=(ARR1(JX+1)*ARR2(JX)-ARR1(JX)*ARR2(JX+1))/
     .             DENOM
                ENDIF
              ENDIF
              VECT=SQRT(ARR3(JX)**2+ARR3(JX+1)**2)
              IF(VECT.LT.DMIN) DMIN=VECT
              IF(VECT.GT.DMAX) DMAX=VECT
310         DMEAN=DMEAN+VECT
          ENDIF
C
          CALL IWRLIN(3,ARR3)
200     CONTINUE
199   CONTINUE
C
      DMEAN=DMEAN/(NX*NY)
      WRITE(6,1600) DMIN,DMAX,DMEAN
1600  FORMAT(///'  Min max and mean density in combined image',
     1 3F10.1)
      CORBOT1=CORBOT1*CORBOT2
      IF(ICOMB.EQ.1.AND.CORBOT1.GT.0.0) THEN
        CORTOP=CORTOP/SQRT(CORBOT1)
        WRITE(6,1601) CORTOP
1601    FORMAT('   and correlation coefficient =',F8.4)
      ENDIF
      DMEAN3=DMEAN
      CALL IWRHDR(3,TITLE,-1,DMIN,DMAX,DMEAN3)
      CALL IMCLOSE(1)
      CALL IMCLOSE(2)
      CALL IMCLOSE(3)
      STOP
C
99    WRITE(6,2000)
2000  FORMAT(///' End of image when reading')
      STOP
      END
C****************TO APPLY PHASE SHIFT TO COMPLEX IMAGES (TRANSFORMS)***********
C  DX IS PHASE SHIFT FOR 1,0 TRANSFORM POINT
C  DY IS PHASE SHIFT FOR 0,1 TRANSFORM POINT
      SUBROUTINE PHSHFT(A,DX,DY,IY)
      COMMON//NX,NY,NZ
      DIMENSION A(1)
      DO 100 I=1,NX
      JX =2*I-1
      ITX=I - 1
      ITY=IY - NY/2 - 1
      PSHIFT = ITX*DX + ITY*DY
      C = COS(PSHIFT)
      S = SIN(PSHIFT)
      APART = A(JX) * C - A(JX+1) * S
      BPART = A(JX) * S + A(JX+1) * C
      A(JX)   = APART
100   A(JX+1) = BPART
      END
