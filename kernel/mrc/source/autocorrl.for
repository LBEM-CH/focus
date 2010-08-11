C*AUTOCORRL.FOR************************************************************
C									*
C	This program was adapted from FFTRANS to include                *
C	1.  Forward transform from image size NX,NY                     *
C	2.  Squaring of transform and placing in larger area            *
C	3.  Back transform to produce autocorrelation size NX3,NY3      *
C                                                                       *
C	Version	2.00	1.1.92	RH	Convert for Alliant, date only  *
C	Version	2.01	3.1.92	RH	Debug and tidy-up               *
C                                                                       *
C                                                                       *
C	 The real-space origin is at (1,1) and		                *
C	the origin of reciprocal space is at (1,NY/2+1).		*
C	The FT of an image NX,NY is NX/2+1,NY complex value.		*
C									*
C	  All transforms are done using Lynn ten Eyck's subroutines.	*
C	These allow arbitrary-sized images having a LARGEST PRIME	*
C	factor of 19!!.							*
C									*
C									*
C									*
C	Only input parameter determines change in size between input    *
C	and output image boxes.                                         *
C									*
C	Logical I/O assignments are:					*
C									*
C	IN			input image      			*
C	OUT			output autocorrelation			*
C									*
C									*
C	Adapted from the following versions of FFTRANS                  *
C	Version  1.00	18.10.81	DAA		FOR VAX		*
C	Version  1.01	27.MAY.82	DAA		FOR VAX		*
C	Version  1.02	10.JUNE.82	DAA		FOR VAX		*
C	Version  1.03	23.JULY.82	DAA		FOR VAX		*
C	Version  1.04	01.OCTOBER.82	DAA		FOR VAX		*
C	Version  1.05	08.November.82	DAA		FOR VAX		*
C	Update   1.05	18.November.82	DAA		FOR VAX		*
C	Revision 1.06	27.November.84	RH		FOR VAX		*
C									*
C	Dimensions set for up to 26*26 input image, and 520*520 output. *
C	NB Array sizes needed  are (NX+2)*NY and (20*NX+2)*(20*NY)      *
C                                                                       *
C************************************************************************
C
	COMMON//NX,NY,NZ,NX3,NY3,NZ3
	COMMON/FTBUF/ARRAY(750),ARRAYSQ(600000)
	DIMENSION TITLE(20),NXYZR(3),MXYZR(3),NXYZF(3),NXYZST(3)
CTSH++
	CHARACTER TMPTITLE*80
	EQUIVALENCE (TMPTITLE,TITLE)
CTSH--
	CHARACTER DAT*24
	EQUIVALENCE (NX,NXYZR)
	DATA IFOR/0/,IBAK/1/,ZERO/0.0/, NXYZST/3*0/
C
	WRITE(6,1000)
1000	FORMAT(//' FFTRANS: Autocorrelation Program V2.01(3.1.92) ',//)
C
	CALL IMOPEN(1,'IN','RO')
	CALL IMOPEN(2,'OUT','NEW')
	CALL FDATE(DAT)
C
C   Read input header 
C
	CALL IRDHDR(1,NXYZR,MXYZR,MODE,DMIN,DMAX,DMEAN)
	CALL ITRHDR(2,1)
	TMIN =  1.E10
	TMAX = -1.E10
	TMEAN = 0.0
	IF (MODE .GE. 3) GOTO 950
C
C   Here for forward transform
C
	NXM1 = NX - 1
	NYM1 = NY - 1
	NX21 = NX/2 + 1
	NXP2 = NX + 2
C
C  Loop over all sections 
C
	    CALL IRDPAS(1,ARRAY,NXP2,NY,0,NXM1,0,NYM1,*99)
CHENN>
C	    CALL TODFFT(ARRAY,NX,NY,IFOR)
	    CALL TDXFFT(ARRAY,NX,NY,IFOR)
CHENN<
	    CALL ICLCDN(ARRAY,NX21,NY,1,NX21,1,NY,DMIN,DMAX,DMEAN)
	  IF (DMIN .LT. TMIN) TMIN = DMIN
	  IF (DMAX .GT. TMAX) TMAX = DMAX
	  TMEAN =  DMEAN
1600	FORMAT(/,'Min,Max,Mean values for section ',I4,' are: ',3G13.5)
	WRITE(6,1800) TMIN,TMAX,TMEAN
1800	FORMAT(/,' Overall Min,Max,Mean values are: ',3G13.5)
C
C
C	Go to subroutine to square the transform and place in larger area
C
	CALL SQRTRAN
C
	WRITE(6,500)NX,NY,NZ,NX3,NY3,NZ3
500	FORMAT(' RETURN FROM SQRTRAN',6I10/)
C   Here for inverse transform
	TMIN =  1.E10
	TMAX = -1.E10
	TMEAN = 0.0
C
50	NXR = NX3
	NXP2 = NXR + 2
	NXYZF(1) = NXR
	NXYZF(2) = NY3
	NXYZF(3) = NZ3
	CALL IALSIZ(2,NXYZF,NXYZST)
CTSH	WRITE(TITLE) DAT(5:24,1700)
CTSH++
	WRITE(TMPTITLE,1700) DAT(5:24)
CTSH--
1700	FORMAT(' FFTRANS: Autocorrelation Calculated',6X,A20)
	WRITE(6,1700) DAT(5:24)
	CALL IWRHDR(2,TITLE,1,ZERO,ZERO,ZERO)
C
C    Loop over all sections 
C
CHENN>
C	    CALL TODFFT(ARRAYSQ,NXR,NY3,IBAK)
	    print *,'Entering FFT'
	    CALL TDXFFT(ARRAYSQ,NX3,NY3,IBAK)
	    print *,'Returned from FFT'
CHENN>
	    CALL IWRPAS(2,ARRAYSQ,NXP2,NY3,0,NXR-1,0,NY3-1)
	    CALL ICLDEN(ARRAYSQ,NXP2,NY3,1,NXR,1,NY3,DMIN,DMAX,DMEAN)
	  IF (DMIN .LT. TMIN) TMIN = DMIN
	  IF (DMAX .GT. TMAX) TMAX = DMAX
	  TMEAN =  DMEAN
C
	WRITE(6,1800) TMIN,TMAX,TMEAN
	CALL IWRHDR(2,TITLE,-1,TMIN,TMAX,TMEAN)
	CALL IMCLOSE(1)
	CALL IMCLOSE(2)
	CALL EXIT
C
99      STOP 'END-OF-FILE ERROR ON READ'
950     STOP 'WRONG TYPE OF INPUT FILE'
	END
C
	SUBROUTINE SQRTRAN
C
	COMMON//NX,NY,NZ,NX3,NY3,NZ3
	COMMON/FTBUF/ARRAY(750),ARRAYSQ(600000)
C
C*SQUARTRN.FOR************************************************************
C     Program to square a transform and place in larger area
C
c                Multiply transform point by complex conjugate of same
C                transform point.
C                Apply origin shift ORIGXB,ORIGYB to second
C                copy of transform (to get real space origin in centre 
C                of picture after squared transfrom is transformed back.
C		ORIGXA=0.0,ORIGYA=0.0,ORIGXB=NX-1,ORIGYB=NY/2 set in program.
C
C	NX,NY are dimensions of starting image box
C	NX3,NY3 are dimensions of expanded image box
C
C
      DIMENSION ARR1(28),ARR2(28),ARR3(28)
      DO 3000 I=1,600000
	ARRAYSQ(I)=0.0
3000	CONTINUE
C
      WRITE(6,1000)
1000  FORMAT(////' SQUARTRN :  Transform squaring program')
C
C	READ EXPANSION FACTOR
	READ (5,*)NEXPAND
	WRITE(6,3001)NEXPAND
3001	FORMAT(' SAMPLING IN OUTPUT AUTOCORRELATION MAP WILL BE',
     . I5,' TIMES AS FREQUENT AS IN INPUT IMAGE'/)
	NX3=NX*NEXPAND
	NY3=NY*NEXPAND
	NZ3=1
	WRITE(6,1011)NX,NY,NX3,NY3
1011	FORMAT(' NX, NY',2I10,10X,' NX3, NY3',2I10/)
C   FINAL SIZE OF TRANSFORM NX3,NY3
C
C
140     ORIGXA=0.0
	ORIGYA=0.0
	ORIGXB=NX/2
	ORIGYB=NY/2
      	  TWOPI = 6.283185
      	  DELPX1 = -TWOPI * ORIGXA / NX
      	  DELPY1 = -TWOPI * ORIGYA / NY
      	  DELPX2 = -TWOPI * ORIGXB / NX
      	  DELPY2 = -TWOPI * ORIGYB / NY
     	WRITE(6,1013)ORIGXA,ORIGYA,ORIGXB,ORIGYB
1013	FORMAT(' Origin shifts applied to Transforms'/
     . ' ORIGXA  =',F8.1/' ORIGYA  =',F8.1/
     . ' ORIGXB  =',F8.1/' ORIGYB  =',F8.1)
C
C
	NY2=NY/2
        NY21=NY2+1
C
        NX21=NX/2+1
	NXT=NX+2
	NX3T=NX3+2
C
	IBLOCK=NX3T*(NY3-NY)
	WRITE(6,9004)NX,NY,NX3,NY3
9004	FORMAT(' NX,NNY,NX3,NY3',4I10)
	WRITE(6,9005)NXT,NX3T,IBLOCK
9005	FORMAT(' NXT,NX3T,IBLOCK',3I10)
C
      DMIN=1.E10
      DMAX=-1.E10
      DMEAN=0.
C
C
	WRITE(6,9003)NY2
9003	FORMAT(' NY2',I10)
      DO 200 IY=1,NY2
	ILOCY=(IY-1)*NXT
	WRITE(6,9000)ILOCY
9000	FORMAT(' ILOCY',I10)
        DO 2001 IX=1,NX21
        JX=2*IX-1
C
	ILOC=ILOCY+JX
C	Get line of transform out of ARRAY into ARR1
        ARR1(JX)=ARRAY(ILOC)
        ARR1(JX+1)=ARRAY(ILOC+1)
C	Copy line of transfrom from ARR1 to ARR2
        ARR2(JX+1)=ARR1(JX+1)
2001	ARR2(JX)=ARR1(JX)
C
      	   IF(IY.EQ.1)WRITE(6,202)ORIGXB,ORIGYB
202	   FORMAT('  PHASE ORIGIN FOR 2ND COPY OF TRANSFORM MOVED TO',
     . 2F10.1,'  BEFORE COMBINATION')
      	   CALL PHSHFT(ARR2,DELPX2,DELPY2,IY,NX21)
C
	ILOC3Y=(IY-1)*NX3T
	WRITE(6,9001)ILOC3Y
9001	FORMAT(' ILOC3Y',I10)
C
        DO 310 IX=1,NX21
        JX=2*IX-1
        ARR3(JX)=ARR1(JX)*ARR2(JX)+ARR1(JX+1)*ARR2(JX+1)
        ARR3(JX+1)=ARR1(JX+1)*ARR2(JX)-ARR1(JX)*ARR2(JX+1)
C***
      	VECT=SQRT(ARR3(JX)**2+ARR3(JX+1)**2)
        IF(VECT.LT.DMIN) DMIN=VECT
        IF(VECT.GT.DMAX) DMAX=VECT
C
	ILOC=ILOC3Y+JX
C
	ARRAYSQ(ILOC)=ARR3(JX)
	ARRAYSQ(ILOC+1)=ARR3(JX+1)
310	DMEAN=DMEAN+VECT
C
200   CONTINUE
C
	WRITE(6,9002)NY21,NY
9002	FORMAT(' NY21,NY',2I10)
      DO 250 IY=NY21,NY
	ILOCY=(IY-1)*NXT
	WRITE(6,9000)ILOCY
        DO 2501 IX=1,NX21
        JX=2*IX-1
	ILOC=ILOCY+JX
C	Get line of transform out of ARRAY into ARR1
        ARR1(JX)=ARRAY(ILOC)
        ARR1(JX+1)=ARRAY(ILOC+1)
C	Copy line of transfrom from ARR1 to ARR2
        ARR2(JX+1)=ARR1(JX+1)
2501    ARR2(JX)=ARR1(JX)
C
         CALL PHSHFT(ARR2,DELPX2,DELPY2,IY,NX21)
C
	ILOC3Y=IBLOCK+(IY-NY21)*NX3T+NY2*NX3T
	WRITE(6,9001)ILOC3Y
        DO 3510 IX=1,NX21
        JX=2*IX-1
        ARR3(JX)=ARR1(JX)*ARR2(JX)+ARR1(JX+1)*ARR2(JX+1)
        ARR3(JX+1)=ARR1(JX+1)*ARR2(JX)-ARR1(JX)*ARR2(JX+1)
        VECT=SQRT(ARR3(JX)**2+ARR3(JX+1)**2)
        IF(VECT.LT.DMIN) DMIN=VECT
        IF(VECT.GT.DMAX) DMAX=VECT
C
	ILOC=ILOC3Y+JX
	ARRAYSQ(ILOC)=ARR3(JX)
	ARRAYSQ(ILOC+1)=ARR3(JX+1)
3510	DMEAN=DMEAN+VECT
C
250   CONTINUE
C
C
      DMEAN=DMEAN/(NX*NY)
C      WRITE(6,1600) DMIN,DMAX,DMEAN
C1600  FORMAT(///'  Min max and mean density in non-zero part of ',
C     1'squared transform', 3F10.1)
      print *,'Min max and mean density in ',
     .  'non-zero part of squared transform ',DMIN,DMAX,DMEAN
C
	RETURN
      END
C****************TO APPLY PHASE SHIFT TO COMPLEX IMAGES (TRANSFORMS)***
C******NOTE; THIS SUBROUTINE IS VALID FOR USE WITH TODFFT CONVENTION OF
C*****HAVING ORIGIN OF TRANSFORM AT 1,1
C  DX IS PHASE SHIFT FOR 1,0 TRANSFORM POINT
C  DY IS PHASE SHIFT FOR 0,1 TRANSFORM POINT
      SUBROUTINE PHSHFT(A,DX,DY,IY,NX21)
      COMMON//NX,NY,NZ,NX3,NY3,NZ3
      DIMENSION A(28)
C
      DO 100 I=1,NX21
      JX =2*I-1
      ITX=I - 1
      ITY=IY - 1
      PSHIFT = ITX*DX + ITY*DY
      C = COS(PSHIFT)
      S = SIN(PSHIFT)
      APART = A(JX) * C - A(JX+1) * S
      BPART = A(JX) * S + A(JX+1) * C
      A(JX)   = APART
100   A(JX+1) = BPART
      END
