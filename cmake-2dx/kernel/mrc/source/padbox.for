C**PADBOX.FOR************************************************************
C                                                                       *
C   Version	1.00	28.12.98   RH	Original program                *
C   Version	1.01	  4.1.99   RH	Add floating options 2 and 4    *
C									*
C   Card input:								*
C   1.  IMODE type of padding						*
C	      0 - add zeroes around the edge				*
C	      1 - add a single constant of perimeter average		*
C	      2 - subtract a single constant of perimeter average and   *
C		   pad with zeroes around edge				*
C	      3 - add a constant for each Z-section perimeter average	*
C	      4 - subtract a constant of each Z-section perimeter	*
C		   average and pad with zeroes around egde		*
C   2.	NXS,NXF,NYS,NYF,NZS,NZF number of pixels to add at each edge    *
C									*
C	Logical I/O assignments are:					*
C	IN			input file      			*
C	OUT			output file				*
C									*
C************************************************************************
        INTEGER ARRMXSIZ
      	PARAMETER (ARRMXSIZ=1000000)
	COMMON//NX,NY,NZ,NX2,NY2,NZ2
	DIMENSION ARRAYIN(ARRMXSIZ),ARRAYOUT(ARRMXSIZ),DENPAD(16384)
	DIMENSION TITLE(20),NXYZ(3),MXYZ(3),NXYZ2(3),NXYZST(3)
      	DOUBLE PRECISION TMEAN,DPERIM
	CHARACTER DAT*24
	EQUIVALENCE (NX,NXYZ),(NX2,NXYZ2)
CTSH++
	CHARACTER TMPTITLE*80
	EQUIVALENCE (TMPTITLE,TITLE)
CTSH--
	DATA ZERO/0.0/, NXYZST/3*0/
C
	WRITE(6,1000)
1000	FORMAT(/' PADBOX V1.01(4-Jan-1999): pad out to any size ',/)
C
C   Read input file and header 
C
	CALL IMOPEN(1,'IN','RO')
	CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
	CALL IMOPEN(2,'OUT','NEW')
	CALL ITRHDR(2,1)
      	CALL IALMOD(2,2)
      		DO 1010 J=1,3
1010		MXYZ(J)=NXYZ(J)
      	CALL IALSAM(2,MXYZ)
	CALL FDATE(DAT)
	IF (MODE .GE. 3) GOTO 950
C
C   Read input control cards
C
      	WRITE(6,1001)
1001	FORMAT(' IMODE type of padding??')
      	READ(5,*) IMODE
      	WRITE(6,1002)
1002	FORMAT(' NXS,NXF,NYS,NYF,NZS,NZF??',
     .		' number of pixels to add at each edge')
      	READ(5,*) NXS,NXF,NYS,NYF,NZS,NZF
      	WRITE(6,1003) IMODE,NXS,NXF,NYS,NYF,NZS,NZF
1003	FORMAT( '  IMODE........',I5/
     . 		'    NXS........',I5/
     .		'    NXF........',I5/
     .		'    NYS........',I5/
     .		'    NYF........',I5/
     .		'    NZS........',I5/
     . 		'    NZF........',I5)
      	NX2=NX+NXS+NXF
      	NY2=NY+NYS+NYF
      	NZ2=NZ+NZS+NZF
      	NXYZST(1)=NXS
      	NXYZST(2)=NYS
      	NXYZST(3)=NZS
     	WRITE(6,1004) NX2,NY2,NZ2
1004	FORMAT(' Padded out to size  ',I4,' x',I4,' x',I4)
        CALL IALSIZ(2,NXYZ2,NXYZST)
CTSH	WRITE(TITLE) NXS,NXF,NYS,NYF,NZS,NZF,DAT(5:24,1100)
CTSH++
	WRITE(TMPTITLE,1100) NXS,NXF,NYS,NYF,NZS,NZF,DAT(5:24)
CTSH--
1100	FORMAT(' PADBOX: file round edge padded by',6I4,2X,A20)
	WRITE(6,1100) NXS,NXF,NYS,NYF,NZS,NZF,DAT(5:24)
	CALL IWRHDR(2,TITLE,1,ZERO,ZERO,ZERO)
C
C  Calculate padding density
      	CALL IMPOSN(1,0,0)
C  Zero the padding numbers, needed for IMODE=0 all sections and for
C			     IMODE=3,4 for start and finish sections
      	IF(IMODE.EQ.0) THEN
      		DO 1200 IZ=1,NZ2
1200		DENPAD(IZ)=0.0
      	ELSEIF(IMODE.EQ.1.OR.IMODE.EQ.2) THEN
      		DPERIM=0.0
      		NPERIM=0
     		CALL IRDSEC(1,ARRAYIN,*999)
            		CALL ICLDEN(ARRAYIN,NX,NY,1,NX,1,NY,DMIN,DMAX,DMEAN)
      			DPERIM=DPERIM+DMEAN*NX*NY
      			NPERIM=NPERIM+NX*NY
      		DO 1300 JZ=2,NZ-1
      			CALL IRDSEC(1,ARRAYIN,*999)
      			DO 1310 I=1,NX
      				INDEX1=I
      				INDEX2=I+NX*(NY-1)
      				DPERIM=DPERIM+ARRAYIN(INDEX1)
      				DPERIM=DPERIM+ARRAYIN(INDEX2)
      				NPERIM=NPERIM+2
1310			CONTINUE
      			DO 1320 I=2,NY-1
      				INDEX1=1+(I-1)*NX
      				INDEX2=NX+(I-1)*NX
      				DPERIM=DPERIM+ARRAYIN(INDEX1)
      				DPERIM=DPERIM+ARRAYIN(INDEX2)
      				NPERIM=NPERIM+2
1320			CONTINUE
1300		CONTINUE      		
      		CALL IRDSEC(1,ARRAYIN,*999)
        		CALL ICLDEN(ARRAYIN,NX,NY,1,NX,1,NY,DMIN,DMAX,DMEAN)
      			DPERIM=DPERIM+DMEAN*NX*NY
      			NPERIM=NPERIM+NX*NY
      		DPERIM=DPERIM/NPERIM
      		DO 1350	IZ=1,NZ2
			DENPAD(IZ)=DPERIM
1350		CONTINUE
      		WRITE(6,1351) DPERIM
1351		FORMAT(' OVERALL PADDED DENSITY FROM PERIMETER',F12.4)
      	ELSEIF(IMODE.EQ.3.OR.IMODE.EQ.4) THEN
      		DO 1400 JZ=1,NZ
                	DPERIM=0.0
                	NPERIM=0
      			CALL IRDSEC(1,ARRAYIN,*999)
      			DO 1410 I=1,NX
      				INDEX1=I
      				INDEX2=I+NX*(NY-1)
      				DPERIM=DPERIM+ARRAYIN(INDEX1)
      				DPERIM=DPERIM+ARRAYIN(INDEX2)
      				NPERIM=NPERIM+2
1410			CONTINUE
      			DO 1420 I=2,NY-1
      				INDEX1=1+(I-1)*NX
      				INDEX2=NX+(I-1)*NX
      				DPERIM=DPERIM+ARRAYIN(INDEX1)
      				DPERIM=DPERIM+ARRAYIN(INDEX2)
      				NPERIM=NPERIM+2
1420			CONTINUE
      		DPERIM=DPERIM/NPERIM
      		ILOC=JZ+NZS
      		DENPAD(ILOC)=DPERIM
      		WRITE(6,1401) ILOC,DPERIM
1401		FORMAT(' Section, perimeter density ',I7,F12.4)
1400		CONTINUE      		
      	ELSE
      		STOP ' invalid IMODE requested'
      	ENDIF
      	WRITE(6,1499)
1499	FORMAT(' Padding densities successfully calculated')
C
C  Loop over all sections 
C
      	CALL IMPOSN(1,0,0)
	TMIN =  1.E10
	TMAX = -1.E10
	TMEAN = 0.0
      	NTOTAL = 0
      	DO 1500 IZ=1,NZ2
      	    IF(IZ.GT.NZS.AND.IZ.LE.NZ+NZS) 
     .		CALL IRDSEC(1,ARRAYIN,*999)
      	    CALL PAD(ARRAYIN,ARRAYOUT,NXS,NYS,NZS,IZ,DENPAD(IZ),IMODE)
            CALL IWRSEC(2,ARRAYOUT)
            CALL ICLDEN(ARRAYOUT,NX2,NY2,1,NX2,1,NY2,DMIN,DMAX,DMEAN)
C      write(6,*) ' Section,DMIN,DMAX,DPAD',IZ,DMIN,DMAX,DENPAD(IZ)
	    IF (DMIN .LT. TMIN) TMIN = DMIN
	    IF (DMAX .GT. TMAX) TMAX = DMAX
	    TMEAN  = TMEAN + DMEAN*NX2*NY2
      	    NTOTAL = NTOTAL + NX2*NY2
1500	CONTINUE
C
      	IF(NTOTAL.NE.NX2*NY2*NZ2) STOP ' Error'
      	DMEAN=TMEAN/NTOTAL
	WRITE(6,1800) TMIN,TMAX,DMEAN
1800	FORMAT(/,' Overall Min,Max,Mean values are: ',3G13.5)
	CALL IWRHDR(2,TITLE,-1,TMIN,TMAX,DMEAN)
	CALL IMCLOSE(1)
	CALL IMCLOSE(2)
	CALL EXIT
C
999	STOP 'END-OF-FILE ERROR ON READ'
950     STOP 'INPUT FILE MUST BE REAL DATA'
	END
C
C******************************************************************************
      SUBROUTINE PAD(ARRAYIN,ARRAYOUT,NXS,NYS,NZS,IZ,DPAD,IMODE)
      COMMON//NX,NY,NZ,NX2,NY2,NZ2
      DIMENSION ARRAYIN(1),ARRAYOUT(1)
      LOGICAL LPAD
C First NZS Z-sections
      IF(IZ.LE.NZS.AND.(IMODE.EQ.1.OR.IMODE.EQ.3)) THEN
      	DO 150 I=1,NX2
      	DO 150 J=1,NY2
      		INDEX=I+(J-1)*NY2
      		ARRAYOUT(INDEX)=DPAD
150	CONTINUE
C Middle Z-sections
      ELSEIF(IZ.GT.NZS.AND.IZ.LE.NZ+NZS) THEN
        DO 250 I=1,NX2
        DO 250 J=1,NY2
      		IF(I.LE.NXS.OR.I.GT.NX+NXS.OR.J.LE.NYS.OR.J.GT.NY+NYS) THEN
      			LPAD=.TRUE.
      		ELSE
      			LPAD=.FALSE.
      			INDEXIN=(I-NXS)+(J-NYS-1)*NY
      		ENDIF
                INDEX=I+(J-1)*NY2
      		IF(IMODE.EQ.1.OR.IMODE.EQ.3) THEN
                	IF(LPAD) THEN
      			  ARRAYOUT(INDEX)=DPAD
      			ELSE
      			  ARRAYOUT(INDEX)=ARRAYIN(INDEXIN)
      			ENDIF
      		ELSE
                	IF(LPAD) THEN
      			  ARRAYOUT(INDEX)=0.0
      			ELSE
      			  ARRAYOUT(INDEX)=ARRAYIN(INDEXIN)-DPAD
      			ENDIF
      		ENDIF
250     CONTINUE
C Last NZF Z-sections
      ELSEIF(IZ.GT.NZ+NZS.AND.(IMODE.EQ.1.OR.IMODE.EQ.3)) THEN
      	DO 350 I=1,NX2
      	DO 350 J=1,NY2
      		INDEX=I+(J-1)*NY2
      		ARRAYOUT(INDEX)=DPAD
350	CONTINUE      
      ENDIF
      RETURN
      END
