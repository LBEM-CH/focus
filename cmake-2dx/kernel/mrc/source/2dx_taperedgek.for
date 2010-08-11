C*TAPEREDGE.FOR**************************************************************
C     Simple program to taper edges of a rectangular image so that there are
C     no sharp discontinuities which make the Fourier tranform contain spikes.
C         Input original image on stream 1 (IN)
C         Output featheredged image on stream 2 (OUT)
C
C     Version  1.0      11-SEP-87    RH
C     Version  1.1      16-SEP-87    PAB
C     Version  1.2      25-JUN-88    RH    bigger value of IDEEP (taper)
C     Version  1.3       5-JUL-88    RH    check that dens<255 if MODE=0
C     Version  1.4       2-OCT-88    RH    prints out Y-dependence as plot
C     Version  1.5      14-MAR-92    RH    port to Alliant - UNIX
C     Version  1.6      14-FEB-96    JMS   bug fix - arrays went to 0
C     Version  1.7      27-FEB-96    JMS   remove redundant lines
C     Version  2.0      20-AUG-00    RH    convert to plot2000
C     Version  2.0      13-JUN-01    TSH   P2K_FONT needed string terminator
C     Version  3.0      30-OCT-05    HS    2dx
C
C---------------------------------INPUT-------------------------------------
C
C	CARD 1:IAVER,ISMOOTH,ITAPER,IDIST
C
C       IAVER:depths of strips parallel to x & y over which averaging
C      		    takes place.
C	ISMOOT:for each pixel running average calculated over area
C		        defined by (-ISMOOTH to ISMOOTH) x (IAVER).
C       ITAPER:depth over which tapering takes place.
C       IDIST:depth of constant exclusion border 
C
C****************************************************************************
C
      PARAMETER (NMAX=21000)
      PARAMETER (NDEEP=1000)
      COMMON//NX,NY,NZ
      DIMENSION ALINE(NMAX),NXYZR(3),MXYZR(3),IXYZ(3),MXYZ(3),TITLE(60)
      DIMENSION ARRAY(NMAX,NDEEP),START(NMAX),FINISH(NMAX),AVEDGE(NMAX)
      DIMENSION ARUNAV(NMAX)
      DIMENSION BRRAY(NDEEP,NMAX)
C     DIMENSION STEST(NMAX),FTEST(NMAX)
      DIMENSION AVOD(NMAX)
      REAL*8 DTOT
      CHARACTER DAT*24
      EQUIVALENCE (ARRAY,BRRAY)
      EQUIVALENCE (NX,NXYZR)
CTSH++
        CHARACTER*240 TMPTITLE
        EQUIVALENCE (TMPTITLE,TITLE) 
CTSH--
C
      PRINT *,
     * ' TAPEREDGEH V3.0(30.10.05) : applies smooth edge to images'
C
C     open files
C
      CALL  IMOPEN(1,'IN','RO')
      CALL  IMOPEN(2,'OUT','NEW')
      CALL  IRDHDR(1,NXYZR,MXYZR,MODE,DMIN,DMAX,DMEAN)
      IF(MODE.LT.0.OR.MODE.GT.2) THEN
      	WRITE(6,998)
998	FORMAT('  INPUT FILE IS NOT AN IMAGE, MODE.NE.0,1, or 2')
      ENDIF
      DMINO=DMIN
      DMAXO=DMAX
      DMEANO=DMEAN
      IF(NXYZR(1).GT.NMAX.OR.NXYZR(2).GT.NMAX) THEN
      	WRITE(6,999) NMAX
999	FORMAT(' ARRAY ALINE PROG DIMS TOO SMALL',I10)
      	STOP
      ENDIF
C
C     read density values on old image
C
C     	
C     read in parameters
C
      READ(5,*) IAVER,ISMOOTH,ITAPER,IDIST
C
CHEN> IDIST is the distance from the edge, which is completely tapered.
C
    
      IF(IAVER+IDIST+1.GT.NDEEP) GO TO 9100
      WRITE(6,10)IAVER,ISMOOTH,ITAPER,IDIST
   10 FORMAT(///'    IAVER=',I5/
     .'  ISMOOTH=',I5/
     .'   ITAPER=',I5/
     .'    IDIST=',I5)
      IXYZ(1) = NX
      IXYZ(2) = NY
      IXYZ(3) = 1
C      NLINES = 0
      CALL  ICRHDR(2,IXYZ,IXYZ,MODE,TITLE,0)
      CALL  ITRLAB(2,1)
C
C     CALL  IALSIZ(2,IXYZ,MXYZ)
      CALL  FDATE(DAT)
C
CTSH  WRITE(TITLE) DAT(5:24,50)
CTSH++
      WRITE(TMPTITLE,50) DAT(5:24)
CTSH--
   50 FORMAT(' TAPEREDGE  : apply a smooth tapering to edge of image',
     .5X,A20)
      CALL  IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
C####################################################################
C-----First read the edge strips parallel to X (i.e. vertical axis)
C####################################################################
C
C-----bottom edge (X=0)
C
      CALL IMPOSN(1,0,0)
      CALL IRDPAS(1,ARRAY,NMAX,NDEEP,0,NX-1,IDIST,IDIST+IAVER-1,*9000)
C
      DO 200 I=1,NX
      	START(I)=0.0
      	DO 210 J=1,IAVER
      	  START(I)=START(I)+ARRAY(I,J)
210   	continue
        START(I)=START(I)/IAVER
200   continue
C
C-----top edge (X=NX)
C
      CALL IMPOSN(1,0,0)
      CALL IRDPAS(1,ARRAY,NMAX,NDEEP,
     1            0,NX-1,NY-IAVER-IDIST,NY-IDIST-1,*9000)
C
      DO 300 I=1,NX
      	FINISH(I)=0.0
      	DO 310 J=1,IAVER
          FINISH(I)=FINISH(I)+ARRAY(I,J)
 310    continue
        FINISH(I)=FINISH(I)/IAVER
 300  continue
C
      do I=1,NX
        AVEDGE(I)=(START(I)+FINISH(I))/2.0
      enddo
C
C-----Appply smoothing parallel to edge in the form of a running average.
C
      DO 350 I=1,NX
      	ARUNAV(I)=0.0
      	IPTS=0
      	DO J=-ISMOOTH,ISMOOTH
      	  IN=I+J
      	  IF(IN.GT.0 .and. IN.le.NX)then
	    ARUNAV(I)=ARUNAV(I)+AVEDGE(IN)
      	    IPTS=IPTS+1
          endif
        enddo
      	ARUNAV(I)=ARUNAV(I)/IPTS
 350  continue
C
C####################################################################
C-----Read in whole image and write out new intermediate image with
C####################################################################
C
C-----the top and bottom edges tapered.
C
      CALL IMPOSN(1,0,0)
      do IY=1,IDIST
        CALL IRDLIN(1,ALINE,*9000)
        CALL CALCAVOD(ALINE,NX,IY,AVOD)
      enddo
      do IX=1,NX
	ALINE(IX)=ARUNAV(IX)
	IF(MODE.EQ.0) THEN
      	  IF(ALINE(IX).GT.255)ALINE(IX)=255.
      	  IF(ALINE(IX).LT.0)  ALINE(IX)=0.
      	ENDIF
      enddo
      do IY=1,IDIST
        CALL IWRLIN(2,ALINE)
      enddo
C
      IPOS=0
      DO IY=IDIST+1,IDIST+1+ITAPER
        CALL IRDLIN(1,ALINE,*9000)
        CALL CALCAVOD(ALINE,NX,IY,AVOD)
        rfrac=REAL(IPOS)/ITAPER
        DO IX=1,NX
   	  ALINE(IX)=ARUNAV(IX)*(1.0-rfrac) + ALINE(IX)*rfrac
 	  IF(MODE.EQ.0) THEN
      	    IF(ALINE(IX).GT.255)ALINE(IX)=255.
            IF(ALINE(IX).LT.0)  ALINE(IX)=0.
          ENDIF
        enddo
        CALL IWRLIN(2,ALINE)
        IPOS=IPOS+1
      enddo
C
      DO IY=IDIST+ITAPER+2,NY-(IDIST+ITAPER+2)
        CALL IRDLIN(1,ALINE,*9000)
        CALL CALCAVOD(ALINE,NX,IY,AVOD)
        CALL IWRLIN(2,ALINE)
      enddo
C
      IPOS=0
      do IY=NY-(IDIST+ITAPER+1),NY-(IDIST+1)
        CALL IRDLIN(1,ALINE,*9000)
        CALL CALCAVOD(ALINE,NX,IY,AVOD)
        rfrac=REAL(IPOS)/ITAPER
      	DO IX=1,NX
   	  ALINE(IX)=ALINE(IX)*(1.0-rfrac) + ARUNAV(IX)*rfrac
	  IF(MODE.EQ.0) THEN
      	    IF(ALINE(IX).GT.255)ALINE(IX)=255.
      	    IF(ALINE(IX).LT.0)  ALINE(IX)=0.
      	  ENDIF
        enddo
        CALL IWRLIN(2,ALINE)
        IPOS=IPOS+1
      enddo
C
      do IY=NY-IDIST,NY
        CALL IWRLIN(2,ALINE)
      enddo
C
      CALL IMCLOSE(1)	! Don't need input file again.
C
C#################################################################
C-----Second read the strips parallel to Y.
C#################################################################
C
C-----left edge (Y=0)
C
      CALL IMPOSN(2,0,0)
      CALL IRDPAS(2,BRRAY,NDEEP,NMAX,0,IAVER-1,0,NY-1,*9000)
C
      DO I=1,NY
      	START(I)=0.0
      	DO J=1,IAVER
       	  START(I)=START(I)+BRRAY(J,I)
        enddo
        START(I)=START(I)/IAVER
      enddo
C
C-----right edge (Y=NY)
C
      CALL IMPOSN(2,0,0)
      CALL IRDPAS(2,BRRAY,NDEEP,NMAX,NX-IAVER,NX-1,0,NY-1,*9000)
C
      DO I=1,NY
      	FINISH(I)=0.0
      	DO J=1,IAVER
       	  FINISH(I)=FINISH(I)+BRRAY(J,I)
        enddo
        FINISH(I)=FINISH(I)/IAVER
      enddo
C
      do I=1,NY
        AVEDGE(I)=(START(I)+FINISH(I))/2.0
      enddo
C
C-----Appply smoothing parallel to edge in the form of a running average.
C
      DO I=1,NY
      	ARUNAV(I)=0.0
      	IPTS=0
      	DO J=-ISMOOTH,ISMOOTH
      	  IN=I+J
      	  IF(IN.GT.0 .and. IN.le.NY)then
      	    ARUNAV(I)=ARUNAV(I)+AVEDGE(IN)
      	    IPTS=IPTS+1
          endif
       	enddo
       	ARUNAV(I)=ARUNAV(I)/IPTS
      enddo
C
C#################################################################
C-----Read in intermediate image and overwrite with final image which should now
C-----have the left and right edges also tapered.
C#################################################################
C
      DMIN=1.0E10
      DMAX=-1.0E10
      DTOT=0.0
C
      CALL IMPOSN(2,0,0)
C
      DO IY=1,NY
C
        CALL IRDLIN(2,ALINE,*9000)
C
 	do IX=1,IDIST
    	  ALINE(IX)=ARUNAV(IY)
        enddo
C
	IPOS=0
      	DO IX=IDIST+1,ITAPER+IDIST+1
          rfrac=REAL(IPOS)/ITAPER
          ALINE(IX)=ALINE(IX)*rfrac+ARUNAV(IY)*(1.0-rfrac)
          IPOS=IPOS+1
        enddo
C
	IPOS=0
      	DO IX=NX-ITAPER-IDIST-2,NX-IDIST-1
          rfrac=REAL(IPOS)/ITAPER
       	  ALINE(IX)=ALINE(IX)*(1.0-rfrac)+ARUNAV(IY)*rfrac
	  IPOS=IPOS+1
        enddo
C
      	DO IX=NX-IDIST,NX
       	  ALINE(IX)=ARUNAV(IY)
        enddo
C
      	DO IX=1,NX
      	  AL = ALINE(IX)
	  IF(MODE.EQ.0) THEN
      	    IF(AL.GT.255) AL = 255.
      	    IF(AL.LT.0)   AL = 0.
      	    ALINE(IX) = AL
      	  ENDIF
      	  IF(AL.GT.DMAX) DMAX = AL
      	  IF(AL.LT.DMIN) DMIN = AL
          DTOT = DTOT + AL
        enddo
C
        CALL IMPOSN(2,0,IY-1)
        CALL IWRLIN(2,ALINE)
C
      enddo
C
      NPTS=NX*NY
      DBMEAN = DTOT / NPTS
      CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DBMEAN)
C
C-----o/p density params & close files
C
      WRITE(6,90) NPTS,DMINO,DMAXO,DMEANO,DMIN,DMAX,DBMEAN
   90 FORMAT(//' number of image points =',I10//
     1 ' old min,max,mean density =',3F10.1//
     1 ' new min,max,mean density =',3F10.1)
      CALL  IMCLOSE(2)
      STOP
C
 9000 PRINT *,' end of data on reading image'
      STOP
 9100 WRITE(6,9101)IAVER,NDEEP
 9101 FORMAT(' IAVER TOO BIG, IAVER, NDEEP=',2I5)
      STOP
      END
C
C#################################################################
C#################################################################
C#################################################################
C******************************************************************************
      SUBROUTINE CALCAVOD(ALINE,NX,IY,AVOD)
      DIMENSION ALINE(1),AVOD(1)
      SUMOD=0.0
      DO 10 I=1,NX
10    SUMOD=SUMOD+ALINE(I)
      AVOD(IY)=SUMOD/NX
      RETURN
      END
C******************************************************************************
      SUBROUTINE YPLOT(AVOD,NY)
      DIMENSION AVOD(1),YA(10000),YB(10000)
      CHARACTER*80 TEXT
      YMXDEV=-1000000
      YMNDEV=100000
      DO 10 I=1,NY
        IF(AVOD(I).LT.YMNDEV)YMNDEV=AVOD(I)
        IF(AVOD(I).GT.YMXDEV)YMXDEV=AVOD(I)
10    CONTINUE
C
      write(*,*)' YMNDEV=',YMNDEV
      PLTSIZ=200
      FONTSIZE=3.5
      CALL P2K_OUTFILE('YPLOT.PS',8)
      CALL P2K_HOME
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
      CALL P2K_GRID(0.5*PLTSIZ,0.5*PLTSIZ,1.0)
      CALL P2K_ORIGIN(-0.5*PLTSIZ,-0.6*PLTSIZ,0.)
      CALL P2K_COLOUR(0)
      YPOSN=PLTSIZ+5.
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(200.,0.,0.)
      CALL P2K_DRAW(200.,150.,0.)
      CALL P2K_DRAW(0.,150.,0.)
      CALL P2K_DRAW(0.,0.,0.)
C
      YF1=200./(NY-1)
      RANGE=YMXDEV-YMNDEV
      YF2=150./RANGE
      NOUT=0
      DO 985 I=1,NY
        NOUT=NOUT+1
        YA(I)=(I-1)*YF1
        YB(I)=(AVOD(I)-YMNDEV)*YF2
985   CONTINUE
      CALL P2K_MOVE(YA(1),YB(1),0.)
      DO 990 I=2,NOUT
        CALL P2K_DRAW(YA(I),YB(I),0.)
990   CONTINUE
      YZERO=-YMNDEV*YF2
      CALL P2K_MOVE(10.,-10.,0.)
      WRITE(TEXT,508)
508   FORMAT(' PLOT OF AVERAGE OD VARIATION WITH Y-COORDINATE OF SCAN')
C
      CALL P2K_STRING(TEXT,55,0.)
      CALL P2K_MOVE(10.,-20.,0.)
      WRITE(TEXT,899)YMXDEV,YMNDEV
899   FORMAT(' Y VARIATION, AVMAX ',F6.2,' AVMIN ',F6.2)
      CALL P2K_STRING(TEXT,39,0.)
      CALL P2K_PAGE
C
      RETURN
      END
C******************************************************************************
