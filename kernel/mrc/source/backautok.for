C     ****** BACKAUTO ******
C
C     PROGRAM TO CALCULATE THE MEAN RADIAL DENSITY (BACKGROUND)
C     OF AN ELECTRON DIFFRACTION PATTERN - any size rectangular area.
C	The new version has options to carry out automatic centre
C	determination, and update of the header into the invisible IEXTRA 
C	records used by the subsequent program PICKYCOR.
C
C     VERSION 	FOR VAX	   MAY 1982
C     VX1.0	RH,JMB	 5-Feb-1992	cosmetic comments and title line added
C     VX2.0	RH	22-Mar-1992	UNIX, automatic centring and header update
C     VX2.1	RH	11-May-1992	3 passes, and only data within 2*STNDEV
C     VX2.2	RH	21-Mar-1993	default value for NSTEP=4
C     VX2.3	RH	16-Aug-1993	change highest density from 1024 to 32000
C     VX2.4	RH	19-Nov-1994	automatic centring with pointer removal
C     VX2.5	RH	 2-Dec-1994	limit instability in centre refinement
C     VX2.6	RH	 3-Dec-1994	centre refine uses gradient weighting. 
C     VX2.7	RH	19-Jan-1995	debug SQRT(GXY) in GETPERP
C     VX2.8	RH	 2-Mar-1995	pointer - test proportion <0.5*stndev
C     VX2.9	RH	25-Jul-1995	replaced CALL PAPER by BRKPLT(1)
C     VX2.10	RH	21-Sep-1995	annotation of plot output
C     VX2.11	RH	 7-OCT-1995	test for MODE=0
C     VX2.12	RH	 8-OCT-1995	move first CRTPLT inside PLOTRAST
C     VX2.13	RH	22-Apr-1996	delete all refs to PDPTIT
C     VX2.14	RH	21-Jul-1996	debug radial background -- ABS(XCOORD)
C					plus improved XYmax/min output format
C     VX2.15	RH	 6-May-1997	remove PEN(2) and PEN(3)
C     VX2.16	RH	15-Nov-1997	estimate variance in empty radius bins
C     VX2.17	RH	 3-Jan-1998	print out number of pixels per radius bin
C     VX2.18	RH	24-Feb-1998	test sqrt for zero argument at TEMP3
C     VX3.00	RH	15-Aug-2000	convert to plot2000 subroutines
C     VX3.01    TSH     13-Jun-2001     P2K_FONT needed string terminator
C     VX3.02    JMS     25-Feb-2005     IDIM, IOVERLOAD increased
C
C     Plots three possible background raster choices for PICKYCOR
C	- only if IEXTRA entries in header are set for DX1,DY1,DX2,DY2
C     Plots radial background curve
C     Plots X-average, Y-average, and perimeter Y-average (YCOR)
C
C
C	Data input cards :
C
C		0.	IREF,IPLOT	if IREF.eq.'F' no automatic centring
C					if IREF.eq.'T' centre is determined
C					if IPLOT.eq.F' no plots done.
C		1.	CX,CY		Centre coords relative to centre of film
C		2.	NPLATE		Film number used to identify outputs
C		3.	TITLE		Title for same purpose
C		4.	NPNTS		smoothing over 2*NPNTS+1 in radial curve
C		5.	IRMAX,IRMIN	max and min limits for X,Y-average plots
C		6.	IRMAXC,IRMINC	max and min limits for YCORRECTION
C
C
C	Input files required and output files produced :
C
C		IN        files of the raw data
C		OUT       data corrected for radial background and y-correction
C		FORT2     radial background to be used in PICKPROFA
C		FORT4     y-correction to be used in PICKPROFA
C		PLOTBACK  plot output data, radial curve and X and Y variations
C_______________________________________________________________________________
C  Notes:  STNDEV is raw standard deviation.
C          STND1V is smoothed standard deviation over adjacent radii.
C
C
CHENN>
C      PARAMETER (ISIZE=4096)
C      PARAMETER (IDIM=3000)
      PARAMETER (ISIZE=15000)
      PARAMETER (IDIM=6000)
CHENN<
      PARAMETER (IOVERLOAD=32000)
      PARAMETER (IUNDER=1)
      REAL*8 SINT(IDIM),SINTSQ(IDIM),PROP(IDIM),PROP1(IDIM)
      REAL*8 TEMP1,TEMP2
      REAL*8 DTOT
      CHARACTER DAT*24
      LOGICAL IREF,IPLOT
CHENN>
C     DIMENSION TEXT(20),TITLE(20),NINT(IDIM),NTINT(IDIM),AVINT(IDIM)
      DIMENSION TEXT(30),TITLE(20),NINT(IDIM),NTINT(IDIM),AVINT(IDIM)
CHENN<
CTSH++
      CHARACTER*80 TMPTEXT
      EQUIVALENCE (TMPTEXT,TEXT)
      CHARACTER*4 TMPFLAG
      EQUIVALENCE (TMPFLAG,FLAG)
CTSH--
      DIMENSION BXLMSQ(IDIM)
      DIMENSION XDEV(ISIZE),YDEV(ISIZE),YDEVC(ISIZE)
      INTEGER*2 NXDEV(ISIZE),NYDEV(ISIZE),NYDEVC(ISIZE)
      DIMENSION SMOOTH(IDIM),A(IDIM),B(IDIM)
      DIMENSION XA(ISIZE),XB(ISIZE),YA(ISIZE),YB(ISIZE)
      DIMENSION STNDEV(IDIM),STND1V(IDIM),AVINT1(IDIM),NINT1(IDIM)
      DIMENSION CRCTD(ISIZE)
C
      DIMENSION ARRAYSQ(ISIZE*ISIZE)
CHENN>
      DIMENSION ARRAYBS(ISIZE*ISIZE)
CHENN<
      DIMENSION IEXT(7),NXYZ(3),MXYZ(3)
      IBAD=0
      SIGMULT=3.5	! 3.5 standard deviations for dirt, scratches, etc

      WRITE(6,1000)
1000  FORMAT(/,/,' BACKAUTO - Calculates radial background',
     .' and plots rasters --  Version 3.02 (25-Feb-2005)',/)
      CALL IMOPEN(1,'IN','OLD')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      CALL IMOPEN(3,'OUT','NEW')
      DAMIN=0.
      DAMAX=0.
      DAMEAN=0.
      CALL FDATE(DAT)
      WRITE(TMPTEXT,9997) DAT(5:24)
 9997 FORMAT('RADIALLY AVERAGED BACKGROUND DENSITY SUBTRACTED ',A20,6X)
      CALL ITRHDR(3,1)
      CALL IWRHDR(3,TEXT,1,DAMIN,DAMAX,DAMEAN)
C
      NX=NXYZ(1)
      NY=NXYZ(2)
C	read in 7 entries in IEXT for NSTEP, OX,OY,DX1,DY1,DX2,DY2
      CALL IRTEXT(1,IEXT,1,7)
      CALL IRDSEC(1,ARRAYSQ,*9999)
      NSTEP=IEXT(1)/25.0
      WRITE(6,9400)(IEXT(I),I=1,7),NSTEP
9400  FORMAT(' IEXTRA(7) on BACKAUTO input    ',7I6,',  NSTEP=',I5)
      IF(NSTEP.EQ.0) NSTEP=4	! default, if empty header to prevent crash
      STEP10=NSTEP*10
      DX1=IEXT(4)/STEP10
      DY1=IEXT(5)/STEP10
      DX2=IEXT(6)/STEP10
      DY2=IEXT(7)/STEP10
C

      READ(5,*) IREF,IPLOT
      READ(5,*) CX,CY
      WRITE(6,503) CX,CY
      PI=3.14159
      ANGA=-89.0
      ANGB=89.0
      WRITE(6,504) ANGA,ANGB
      READ(5,*)NPLATE
C
C
      READ(5,513)TITLE
      WRITE(6,511)NPLATE,(TITLE(J),J=1,18)
C
      READ(5,*)NPNTS
      READ(5,*)IRMAX,IRMIN
      READ(5,*)IRMAXC,IRMINC
C
C	All input control cards now read
C
C	****************************** * * * * * * *
C
	IF(IREF)  THEN
		CALL AUTOCENTA(ARRAYSQ,DMIN,DMAX,NX,NY,CX,CY)
	ENDIF
		IEXT(2)=CX*STEP10  ! put centre in header 
		IEXT(3)=CY*STEP10  ! even if from card input
		CALL IALEXT(1,IEXT,1,7)
		CALL IALEXT(3,IEXT,1,7)
      		WRITE(6,9500) (IEXT(I),I=1,7),NSTEP
9500		FORMAT(' IEXTRA(7) in BACKAUTO output   ',7I6,',  NSTEP=',I5)
		CALL IWRHDR(1,TITLE,-1,DMIN,DMAX,DMEAN)
C   not usually needed if lattice parameters are not yet determined.
      	IF(IPLOT) THEN
      	      	FONTSIZE=4.5  	! 4.5 mm fontsize
      		CALL P2K_OUTFILE('PLOTBACK',8)
      		CALL P2K_HOME
      		CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
      		PLTSIZ = 300
      		CALL P2K_GRID(0.5*PLTSIZ,0.5*PLTSIZ,1.0)
      		CALL P2K_ORIGIN(0.,-0.125*PLTSIZ,0.)
      	      CALL PLOTRAST(NPLATE,TITLE,DX1,DY1,DX2,DY2)
      		CALL P2K_ROR
      	ENDIF
C
      WRITE(6,517)NPNTS
      XHALF=(NX+1.0)/2.0
      YHALF=(NY+1.0)/2.0
C    INTEGRATE OUT TO CORNERS OF FILM
      NMAX=SQRT(XHALF**2+YHALF**2)+1
      IF(NMAX.GT.IDIM)GO TO 5
      GO TO 9
5     WRITE(6,512)
      STOP
9     CX=CX+XHALF
      CY=CY+YHALF
C
C    PREPARE TABLE OF SQUARES
      DO 800 I=1,IDIM
      BOXLIM=I-1.5
      IF(I.EQ.1)BOXLIM=0
      BXLMSQ(I)=BOXLIM*BOXLIM
800   CONTINUE
C
C
      ANGA=ANGA*PI/180.
      ANGB=ANGB*PI/180.
      TANA=TAN(ANGA)
      TANB=TAN(ANGB)
C
C **************** HERE TO START MAIN LOOP **********************************
C                    NOW  WITH 4 PASSES
C 
      NPASS=1
9010  IF(NPASS.EQ.1)GO TO 9011	! skip  on first pass
      DO 9012 I=1,NMAX
      AVINT1(I)=AVINT(I)
9012  NINT1(I)=NINT(I)
C9012  STND1V(I)=STNDEV(I)	! now done at end of previous pass
C
9011  DO 10 I=1,NMAX
      PROP(I)=0.
      SINTSQ(I)=0.
      SINT(I)=0.
      NTINT(I)=0
10    NINT(I)=0
      YCOORD=1-CY
      XCOORD=1-CX
      RADSQ=XCOORD*XCOORD+YCOORD*YCOORD
      RAD=SQRT(RADSQ)
      JC=RAD+1.5
C
C In following do-loops, sum all pixels to calculate radial sums - on later 
C  cycles, ignore values deviating by more than SIGMULT(=3.5) sigma from mean; 
C  also ignore negative pixels in radius bins where distribution at 
C  a particular is bimodel (includes pointer shadow.
C
      DO 50 I=1,NY
	      YCOORD=I-CY
	      YSQ=YCOORD*YCOORD
	   DO 20 J=1,NX
		INDEX=J+NX*(I-1)
		VAL=ARRAYSQ(INDEX)
CHENN>
                ARRAYBS(INDEX)=0
CHENN<
	      XCOORD=J-CX
	      IF(ABS(XCOORD).LE.0.0001)XCOORD=0.0001
801	      TANTST=YCOORD/XCOORD
C	      IF((TANTST.LT.TANA).OR.(TANTST.GT.TANB)) GO TO 20
	      RADSQ=XCOORD*XCOORD+YSQ
	      IF(RADSQ.GE.BXLMSQ(JC)) GO TO 805
C
802	      IF(RADSQ.GE.BXLMSQ(JC-1))GO TO 803
	      JC=JC-1
	      GO TO 802
803	      JC=JC-1
	      GO TO 810
C
805	      IF(RADSQ.LT.BXLMSQ(JC+1))GO TO 810
	      JC=JC+1
	      GO TO 805
C
810	      IRAD=JC
      	      NTINT(IRAD)=NTINT(IRAD)+1
	      IF(IRAD.GT.NMAX) GO TO 20
		IF(VAL.LT.IOVERLOAD.AND.VAL.GT.IUNDER)GO TO 812
      		IBAD=IBAD+1
		IF(IBAD.LT.20)WRITE(6,9000)I,J,VAL
	      GO TO 20
812	      IF(NPASS.EQ.1)GO TO 811
		DEV=VAL-AVINT1(IRAD)
C  On third pass try to drop out residual pointer shadow
      		IF(NPASS.EQ.3.AND.PROP1(IRAD).LT.0.2.AND.DEV.LT.0.)GO TO 20 ! bimodal-pointer
	      IF(DEV.LT.0.0)DEV=-DEV
C
CHENN>
              IF(NPASS.EQ.4.AND.VAL.le.150.0)GO TO 21
CHENN<
	      IF(DEV.GT.(SIGMULT*STND1V(IRAD)))GO TO 20
      		IF(DEV.LE.0.25*STND1V(IRAD))PROP(IRAD)=PROP(IRAD)+1 ! < 0.25sig
811		SINT(IRAD)=SINT(IRAD)+VAL
		SINTSQ(IRAD)=SINTSQ(IRAD)+VAL*VAL
	      NINT(IRAD)=NINT(IRAD)+1
           goto 20
21         ARRAYBS(INDEX)=1
20	   CONTINUE
50    CONTINUE
C
      AVMAX=0.
      AVMIN=IOVERLOAD
      DO 70 I=1,NMAX
      IF(NINT(I).EQ.0) THEN
      	PROP1(I)=0.0
      	AVINT(I)=0.0
      	STNDEV(I)=1000.
      ELSE
      	PROP1(I)=PROP(I)/NINT(I)
      	AVINT(I)=SINT(I)/NINT(I)
      	  TEMP1=NINT(I)*SINTSQ(I)
      	  TEMP2=TEMP1-SINT(I)**2
      	  TEMP3=TEMP2
      	IF(TEMP3.GT.0.0) THEN
      		STNDEV(I)=SQRT(TEMP3)/NINT(I)
      	ELSE
      		STNDEV(I)=0.0
      	ENDIF
      	  AVMAX=AMAX1(AVMAX,AVINT(I))
      	  AVMIN=AMIN1(AVMIN,AVINT(I))
      ENDIF
C      write(6,69) NPASS,I,NINT(I),SINT(I),PROP1(I)	! comment out
69    format(' NPASS,radius,Npoints,sumint,propn =',I2,2I5,F10.1,F10.4)
70    CONTINUE
C
C APPLY SMOOTHING TO STANDARD DEVIATION CURVE
      DO 71 I=1,NMAX
      IBEGIN=I-NPNTS
      IFINSH=I+NPNTS
      IF(IBEGIN.LT.1) IBEGIN=1
      IF(IFINSH.GT.NMAX) IFINSH=NMAX
      ISUM=0
      SIGDEN=0.
      DO 72 J=IBEGIN,IFINSH
      IF(NINT(J).EQ.0) GO TO 72
      ISUM=ISUM+1
      SIGDEN=SIGDEN+STNDEV(J)
72    CONTINUE
      IF(ISUM.EQ.0) THEN
      	WRITE(6,73) I
73	FORMAT(' NO NON-ZERO PIXELS IN RADIUS BIN',I5)
      	write(6,74)
74	format(' probable film gradient or bad CCD top/bottom',
     .	' dark current correction, which invalidates the radial',
     .	' sigma determination')
C      	stop ' resulting zero variance not allowed'
      ELSE
      	STND1V(I)=SIGDEN/ISUM
      ENDIF
71    CONTINUE
C
C
      IF(NPASS.EQ.4)GO TO 9850	! now do 4 passes, to remove dirt and pointer
      NPASS=NPASS+1
      GO TO 9010
C
C **************** HERE IS END OF MAIN LOOP **********************************
C
C
C    NOW PLOT OUTPUT ON PLOTTER
9850  IF(IPLOT) THEN
	      RANGE=AVMAX-AVMIN
	      NOUT=0
	      F1=200./(NMAX-1)
	      F2=150./RANGE
	    DO 75 I=1,NMAX
	    IF(NINT(I).EQ.0) GO TO 75
	    NOUT=NOUT+1
	    A(NOUT)=(I-1)*F1
	    B(NOUT)=(AVINT(I)-AVMIN)*F2
75	    CONTINUE
      	      PLTSIZ=220 	! 200 mm originally
      	      CALL P2K_HOME
      	      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
      	      CALL P2K_GRID(0.5*PLTSIZ,0.5*PLTSIZ,1.0)
      	      CALL P2K_ORIGIN(-0.5*PLTSIZ,-0.6*PLTSIZ,0.)
      	      CALL P2K_COLOUR(0)

	      CALL P2K_MOVE(1.0,-10.0,0.)
CTSH	      WRITE(TEXT)(TITLE(J),J=1,20,508)
CTSH++
	      WRITE(TMPTEXT,508)(TITLE(J),J=1,20)
CTSH--
	      CALL P2K_STRING(TEXT,80,0.)
508	FORMAT(20A4)
	      CALL P2K_MOVE(1.0,-20.0,0.)
CTSH	      WRITE(TEXT,509)
CTSH++
	      WRITE(TMPTEXT,509)
CTSH--
      	      CALL P2K_STRING(TEXT,54,0.)
509	FORMAT(' BACKGROUND DENSITY CORRECTION, RED=RAW, GREEN=SMOOTH')
C
	      CALL P2K_MOVE(0.,0.,0.)
	      CALL P2K_DRAW(200.,0.,0.)
	      CALL P2K_DRAW(200.,150.,0.)
	      CALL P2K_DRAW(0.,150.,0.)
	      CALL P2K_DRAW(0.,0.,0.)
C	      CALL P2K_COLOUR(0)
	      CALL P2K_MOVE(A(1),B(1),0.)
	    DO 80 I=2,NOUT
      		CALL P2K_DRAW(A(I),B(I),0.)
80	    CONTINUE
      ENDIF
      WRITE(6,507) AVMAX,AVMIN
C
C    NOW APPLY A SMOOTHING FUNCTION BY AVERAGING OVER 2*NPNTS+1 ADJACENT
C    POINTS IN THE RADIAL DENSITY DISTRIBUTION.
      DO 90 I=1,NMAX
      IBEGIN=I-NPNTS
      IFINSH=I+NPNTS
      IF(IBEGIN.LT.1) IBEGIN=1
      IF(IFINSH.GT.NMAX) IFINSH=NMAX
      ISUM=0
      DEN=0.
      DO 92 J=IBEGIN,IFINSH
      IF(NINT(J).EQ.0) GO TO 92
      ISUM=ISUM+1
      DEN=DEN+AVINT(J)
92    CONTINUE
      IF(ISUM.EQ.0) THEN
      	SMOOTH(I)=0.0
      ELSE
      	SMOOTH(I)=DEN/ISUM
      ENDIF
90    STNDEV(I)=STND1V(I)	! replace with smoothed standard deviation
      NOUT=0
      DO 95 I=1,NMAX
      IF(NINT(I).EQ.0) GO TO 95
      NOUT=NOUT+1
      A(NOUT)=(I-1)*F1
      B(NOUT)=(SMOOTH(I)-AVMIN)*F2
95    CONTINUE
	IF(IPLOT) THEN
C      	      CALL P2K_COLOUR(0)
	      CALL P2K_MOVE(A(1),B(1),0.)
	    DO 100 I=2,NOUT
100	    CALL P2K_DRAW(A(I),B(I),0.)
	ENDIF
	IF(IPLOT) THEN
	      CALL P2K_MOVE(1.0,-30.0,0.)
CTSH	      WRITE(TEXT,9508)AVMAX,AVMIN
CTSH++
	      WRITE(TMPTEXT,9508)AVMAX,AVMIN
CTSH--
      	      CALL P2K_STRING(TEXT,43,0.)
9508  	    FORMAT(' MAXIMUM AND MINIMUM OD',2F10.2)
      	      WRITE(6,9509)
9509	    FORMAT('  Radial plot written out',/)
	ENDIF
C
C  NOW OUTPUT INTEGRATED DATA
C
C  For film with 10 micron pixels and an optical density of 1.0, there should 
C    be about 100 electrons per pixel, and therefore the standard deviation 
C    should be about 10%.  Scaled so that this corresponds to a density of 
C    about 200, this would give a standard deviation of about 20.  For the CCD,
C    with the raw data scaled down by 40x, a density of 200 should correspond
C    to about 140 electrons so the standard deviation should then be about 15.
C    Therefore the output standard deviation is checked for consistency within
C    a factor of two of this guidline.
C
C
      WRITE(2,511) NPLATE,(TITLE(J),J=1,18)
      DO 110 I=1,NMAX
      STNOLD=STNDEV(I)
      IF(SMOOTH(I).GT.0.0) THEN
      	STNGUIDE=SQRT(SMOOTH(I))*1.4
      	RATIO=STNDEV(I)/STNGUIDE
      	IF(RATIO.GT.2.0) STNDEV(I)=2.0*STNGUIDE	! keep near theoretical
      	IF(RATIO.LT.0.5) STNDEV(I)=0.5*STNGUIDE	!
      ENDIF
      IF(STNDEV(I).LT.0.5) STNDEV(I)=0.5
      IF(STNOLD.NE.STNDEV(I)) THEN
CTSH      	FLAG='   *'
CTSH++
      	TMPFLAG='   *'
CTSH--
      ELSE
CTSH      	FLAG='    '
CTSH++
      	TMPFLAG='    '
CTSH--
      ENDIF
      write(6,111)I,SMOOTH(I),STNOLD,STNDEV(I),FLAG,
     .		NINT(I),(NTINT(I)-NINT(I))	! comment out later
111   format(' rad,dens,stnold,stnnew,ngood,nbad =',I5,3F9.2,A4,2I6)
110   WRITE(2,514) SMOOTH(I),STNDEV(I)
      WRITE(6,516)
130   CONTINUE
C
      WRITE(6,10508)IRMAX,IRMIN
10508 FORMAT(' MAXIMUM AND MINIMUM RADIUS FOR X,Y PLOTS',2I8,/)
C
C     DEVIATIONS FROM RADIAL AVERAGE AS FUNCTION OF X AND Y POSITIONS
C     FOR DIAGNOSTIC PLOTS
C
C
      DO 930 I=1,NY
      YDEV(I)=0.
      YDEVC(I)=0.
      NYDEVC(I)=0
      NYDEV(I)=0
930   CONTINUE
      DO 931 J=1,NX
      XDEV(J)=0.
      NXDEV(J)=0
931   CONTINUE
C
      XMXDEV=0.
      XMNDEV=0.
      YMNDEV=0.
      YMXDEV=0.
      NLIMD=100
      NXSQD=0
      NYSQD=0
      AVRGXD=0.
      AVRGYD=0.
      XSQDEV=0.
      YSQDEV=0.
      NYC=0
      AVRGYC=0.
      YMXDVC=0.
      YMNDVC=0.
C
      YCOORD=1-CY
      XCOORD=1-CX
      RADSQ=XCOORD*XCOORD+YCOORD*YCOORD
      RAD=SQRT(RADSQ)
      JC=RAD+0.5
C
      DAMIN=1.0E10
      DAMAX=1.0E-10
      DTOT=0.
      DO 950 I=1,NY
        YCOORD=I-CY
        YSQ=YCOORD*YCOORD
901     DO 920 J=1,NX
		INDEX=J+NX*(I-1)
		VAL=ARRAYSQ(INDEX)
	      XCOORD=J-CX
	      RADSQ=XCOORD*XCOORD+YSQ
C
	      IF(RADSQ.GE.BXLMSQ(JC))GO TO 905
902	      IF(RADSQ.GE.BXLMSQ(JC-1))GO TO 903
	      JC=JC-1
	      GO TO 902
903	      JC=JC-1
	      GO TO 910
905   	      IF(RADSQ.LT.BXLMSQ(JC+1))GO TO 910
	      JC=JC+1
	      GO TO 905
910	      IRAD=JC
	      IF(IRAD.GT.NMAX)IRAD=NMAX
C
CHENN>
            IF(ARRAYBS(INDEX).eq.1) then
              DEV=VAL
            ELSE
              DEV=VAL-SMOOTH(IRAD)
            ENDIF
C
C             DEV=VAL-SMOOTH(IRAD)
CHENN<
      	    IF(MODE.EQ.0.AND.DEV.LT.0.0) DEV=0.0
	    CRCTD(J)=DEV
		IF(DEV.LT.DAMIN) DAMIN=DEV
		IF(DEV.GT.DAMAX) DAMAX=DEV
		DTOT=DTOT+DEV
C
		IF(VAL.LT.IOVERLOAD.AND.VAL.GT.IUNDER)GO TO 9001
      		IBAD=IBAD+1
		IF(IBAD.LT.20)WRITE(6,9000) J,I,VAL
	      GO TO 920
9000  	      FORMAT(' LARGE DEVIATION - greater than IOVER or ',
     .        'less than IUNDER (IX,IY,VAL)',2I5,F12.1)
9002  	      FORMAT(/,' NUMBER OF OVER AND UNDERLOADS ALTOGETHER',I10,/)
9001	      IF(IRAD.GT.IRMAX)GO TO 10920
	      IF(IRAD.LT.IRMIN)GO TO 10920
	      YDEV(I)=YDEV(I)+DEV
	      NYDEV(I)=NYDEV(I)+1
	      XDEV(J)=XDEV(J)+DEV
	      NXDEV(J)=NXDEV(J)+1
10920 	      IF(IRAD.GT.IRMAXC)GO TO 920
	      IF(IRAD.LT.IRMINC)GO TO 920
	      DEVM=ABS(DEV)
	      IF(DEVM.GT.(2.0*STNDEV(IRAD)))GO TO 920
	      YDEVC(I)=YDEVC(I)+DEV
	      NYDEVC(I)=NYDEVC(I)+1
920     CONTINUE
        IF(NYDEV(I).LT.NLIMD)GO TO 949
        YDEV(I)=YDEV(I)/NYDEV(I)
        YSQDEV=YSQDEV+YDEV(I)*YDEV(I)
        AVRGYD=AVRGYD+YDEV(I)
        NYSQD=NYSQD+1
        IF(YDEV(I).LT.YMXDEV)GO TO 948
        YMXDEV=YDEV(I)
        IYMX=I
948     IF(YDEV(I).GT.YMNDEV)GO TO 951
        YMNDEV=YDEV(I)
        IYMN=I
        GO TO 951
949     YDEV(I)=0.
951     CONTINUE
        IF(NYDEVC(I).EQ.0)GO TO 10949
        YDEVC(I)=YDEVC(I)/NYDEVC(I)
        AVRGYC=AVRGYC+YDEVC(I)
        NYC=NYC+1
        IF(YDEVC(I).LT.YMXDVC)GO TO 10948
        YMXDVC=YDEVC(I)
        IYMXC=I
10948   IF(YDEVC(I).GT.YMNDVC)GO TO 10951
        YMNDVC=YDEVC(I)
        IYMNC=I
        GO TO 10951
10949   YDEVC(I)=0.

10951   CALL IWRLIN(3,CRCTD)
950   CONTINUE
C
      DAMEAN=DTOT/(NX*NY)
      CALL IWRHDR(3,TITLE,-1,DAMIN,DAMAX,DAMEAN)
      DO 970 J=1,NX
      	IF(NXDEV(J).LT.NLIMD)GO TO 969
      	XDEV(J)=XDEV(J)/NXDEV(J)
      	XSQDEV=XSQDEV+XDEV(J)*XDEV(J)
      	AVRGXD=AVRGXD+XDEV(J)
      	NXSQD=NXSQD+1
      	IF(XDEV(J).LT.XMXDEV)GO TO 968
      	XMXDEV=XDEV(J)
      	JXMX=J
968   	IF(XDEV(J).GT.XMNDEV)GO TO 970
      	XMNDEV=XDEV(J)
      	JXMN=J
      	GO TO 970
969   	XDEV(J)=0.
970   CONTINUE
C
      WRITE(6,971) NX,NY,JXMN,XMNDEV,JXMX,XMXDEV,
     .		IYMN,YMNDEV,IYMX,YMXDEV
971   FORMAT(' MIN AND MAX DEVIATIONS FROM RADIAL DISTRIBUTION',
     .	' where 1<=NX<=',I5,'  1<=NY<=',I5,/,
     .	18X,'Xpos      Xmin      Xpos      Xmax',18X,
     .	'Ypos      Ymin      Ypos      Ymax',/,
     . ' X DIRECTION',2(I10,F10.2),' Y DIRECTION',2(I10,F10.2),/)
      AVRGXD=AVRGXD/NXSQD
      AVRGYD=AVRGYD/NYSQD
      XSQDEV=XSQDEV/NXSQD
      YSQDEV=YSQDEV/NYSQD
      RMSXD=SQRT(XSQDEV)
      RMSYD=SQRT(YSQDEV)
      WRITE(6,965)AVRGXD,RMSXD,AVRGYD,RMSYD
965   FORMAT(' MEAN AND RMS DEVIATION IN X DIRECTION',2F10.2,/,
     1' MEAN AND RMS DEVIATION IN Y DIRECTION',2F10.2,/,/)
C
C      WRITE(6,973)
C973   FORMAT(' VARIATION IN X DIRECTION',/,/)
C      DO 972 I=1,NX,10
C      X=I-CX
C      WRITE(6,974)X,XDEV(I),NXDEV(I)
C972   CONTINUE
C
974   FORMAT(5X,F10.2,F10.2,I10)
C
C      WRITE(6,976)
C976   FORMAT(' VARIATION IN Y DIRECTION',/,/)
C      DO 977 I=1,NY,10
C      Y=I-CY
C      WRITE(6,974)Y,YDEV(I),NYDEV(I)
C977   CONTINUE
C
	IF(IPLOT) THEN
C	      PLOT THESE TRENDS
C	      X FIRST
      	      CALL P2K_PAGE
      	      CALL P2K_HOME
      	      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
      	      CALL P2K_GRID(0.5*PLTSIZ,0.5*PLTSIZ,1.0)
      	      CALL P2K_ORIGIN(-0.5*PLTSIZ,-0.6*PLTSIZ,0.)
C      	      CALL P2K_COLOUR(0)
      	      CALL P2K_MOVE(0.,0.,0.)
      	      CALL P2K_DRAW(200.,0.,0.)
      	      CALL P2K_DRAW(200.,150.,0.)
      	      CALL P2K_DRAW(0.,150.,0.)
      	      CALL P2K_DRAW(0.,0.,0.)
C
C
	      XF1=200./(NX-1)
	      RANGE=XMXDEV-XMNDEV
	      XF2=150./RANGE
	      NOUT=0
	    DO 975 I=1,NX
	      NOUT=NOUT+1
	      XA(NOUT)=(I-1)*XF1
	      XB(NOUT)=(XDEV(I)-XMNDEV)*XF2
975	    CONTINUE
      	      CALL P2K_MOVE(XA(1),XB(1),0.)
C      	      CALL P2K_COLOUR(0)
	    DO 980 I=2,NOUT
      	      CALL P2K_DRAW(XA(I),XB(I),0.)
980	    CONTINUE
	      XZERO=-XMNDEV*XF2
      	      CALL P2K_MOVE(0.,XZERO,0.)
      	      CALL P2K_DRAW(200.,XZERO,0.)
	      CALL P2K_MOVE(1.0,-10.0,0.)
CTSH	      WRITE(TEXT)(TITLE(J),J=1,20,508)
CTSH++
	      WRITE(TMPTEXT,508)(TITLE(J),J=1,20)
CTSH--
      	      CALL P2K_STRING(TEXT,80,0.)
	      CALL P2K_MOVE(1.0,-20.0,0.)
CTSH	      WRITE(TEXT,898)XMXDEV,XMNDEV,IRMAX,IRMIN
CTSH++
	      WRITE(TMPTEXT(1:63),898)XMXDEV,XMNDEV,IRMAX,IRMIN
CTSH--
898	      FORMAT(' X DEVIATION, MAX ',F6.2,' MIN ',F6.2,
     .         ' (RADIUS LIMITS',2I6,')')
      	      CALL P2K_STRING(TEXT,63,0.)
      	      WRITE(6,897)
897	    FORMAT('  X-deviation plot written out')
C
C	     Y NEXT
      	      CALL P2K_PAGE
      	      CALL P2K_HOME
      	      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
      	      CALL P2K_GRID(0.5*PLTSIZ,0.5*PLTSIZ,1.0)
      	      CALL P2K_ORIGIN(-0.5*PLTSIZ,-0.6*PLTSIZ,0.)
C
C      	      CALL P2K_COLOUR(0)
C
      	      CALL P2K_MOVE(0.,0.,0.)
      	      CALL P2K_DRAW(200.,0.,0.)
      	      CALL P2K_DRAW(200.,150.,0.)
      	      CALL P2K_DRAW(0.,150.,0.)
      	      CALL P2K_DRAW(0.,0.,0.)
C
C
C
	      YF1=200./(NY-1)
	      RANGE=YMXDEV-YMNDEV
	      YF2=150./RANGE
	      NOUT=0
	    DO 985 I=1,NY
	      NOUT=NOUT+1
	      YA(I)=(I-1)*YF1
	      YB(I)=(YDEV(I)-YMNDEV)*YF2
985	    CONTINUE
      	      CALL P2K_MOVE(YA(1),YB(1),0.)
C      	      CALL P2K_COLOUR(0)
	    DO 990 I=2,NOUT
      	      CALL P2K_DRAW(YA(I),YB(I),0.)
990	    CONTINUE
	      YZERO=-YMNDEV*YF2
      	      CALL P2K_MOVE(0.,YZERO,0.)
      	      CALL P2K_DRAW(200.,YZERO,0.)
C
	      CALL P2K_MOVE(1.0,-10.0,0.)
CTSH	      WRITE(TEXT)(TITLE(J),J=1,20,508)
CTSH++
	      WRITE(TMPTEXT,508)(TITLE(J),J=1,20)
CTSH--
      	      CALL P2K_STRING(TEXT,80,0.)
	      CALL P2K_MOVE(1.0,-20.0,0.)
CTSH	      WRITE(TEXT,899)YMXDEV,YMNDEV,IRMAX,IRMIN
CTSH++
	      WRITE(TMPTEXT(1:63),899)YMXDEV,YMNDEV,IRMAX,IRMIN
CTSH--
899	       FORMAT(' Y DEVIATION, MAX ',F6.2,' MIN ',F6.2,
     .        ' (RADIUS LIMITS',2I6,')')
      	      CALL P2K_STRING(TEXT,63,0.)
      	      WRITE(6,896)
896	    FORMAT('  Y-deviation plot written out')
C
C	     NOW PLOT Y CORRECTION
      	      CALL P2K_PAGE
      	      CALL P2K_HOME
      	      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
      	      CALL P2K_GRID(0.5*PLTSIZ,0.5*PLTSIZ,1.0)
      	      CALL P2K_ORIGIN(-0.5*PLTSIZ,-0.6*PLTSIZ,0.)
C
C      	      CALL P2K_COLOUR(0)
C
      	      CALL P2K_MOVE(0.,0.,0.)
      	      CALL P2K_DRAW(200.,0.,0.)
      	      CALL P2K_DRAW(200.,150.,0.)
      	      CALL P2K_DRAW(0.,150.,0.)
      	      CALL P2K_DRAW(0.,0.,0.)
C
	      YF1=200./(NY-1)
	      RANGE=YMXDVC-YMNDVC
	      YF2=150./RANGE
	      NOUT=0
	    DO 10985 I=1,NY
	      NOUT=NOUT+1
	      YA(I)=(I-1)*YF1
	      YB(I)=(YDEVC(I)-YMNDVC)*YF2
10985	    CONTINUE
      	      CALL P2K_MOVE(YA(1),YB(1),0.)
C      	      CALL P2K_COLOUR(0)
	    DO 10990 I=2,NOUT
      	      CALL P2K_DRAW(YA(I),YB(I),0.)
10990	    CONTINUE
	      YZERO=-YMNDVC*YF2
      	      CALL P2K_MOVE(0.,YZERO,0.)
      	      CALL P2K_DRAW(200.,YZERO,0.)
C
	      CALL P2K_MOVE(1.0,-10.0,0.)
CTSH	      WRITE(TEXT)(TITLE(J),J=1,20,508)
CTSH++
	      WRITE(TMPTEXT,508)(TITLE(J),J=1,20)
CTSH--
      	      CALL P2K_STRING(TEXT,80,0.)
	      CALL P2K_MOVE(1.0,-20.0,0.)
CTSH	      WRITE(TEXT,10899)YMXDVC,YMNDVC,IRMAXC,IRMINC
CTSH++
	      WRITE(TMPTEXT(1:64),10899)YMXDVC,YMNDVC,IRMAXC,IRMINC
CTSH--
C
10899		FORMAT(' Y CORRECTION, MAX ',F6.2,' MIN ',F6.2,
     .          ' (RADIUS LIMITS',2I6,')')
      	      CALL P2K_STRING(TEXT,64,0.)
      	      WRITE(6,10897)
10897	    FORMAT('  Y-correction plot written out')
C
	ENDIF
      	      CALL P2K_PAGE
C
C     WRITE Y CORRECTION LIST TO OUTPUT DATA SET 4
      WRITE(6,3054)IRMAXC,IRMINC
3054  FORMAT(' MAXIMUM AND MINIMUM RADIUS FOR Y CORRECTION CALCULATION',
     .2I8)
C

      WRITE(4,3051)NPLATE
3051  FORMAT(I10) 
      DO 3050 I=1,NY
      WRITE(4,3052)I,YDEVC(I)
3050  CONTINUE
3052  FORMAT(I10,F10.5)
      WRITE(6,3053)
3053  FORMAT(' Y CORRECTION LIST WRITTEN TO DATA SET 4')
      GO TO 1500
C
501   FORMAT(' TITLE ',20A4,/,' NX=',I5,'  NY=',I5)
502   FORMAT(2F10.2)
503   FORMAT(' CENTRE OF PATTERN READ IN',2F9.2,
     . ' RELATIVE TO SCAN CENTRE')
504   FORMAT(' ARCS INTEG BETWEEN',F10.2,' AND',F10.2,' DEG')
505   FORMAT(I8,2F10.2,I8,F10.3,5X,F10.2,I8,F10.3)
506   FORMAT(/,'  RADIUS  SMOOTHED   DENSITY     NAV    STNDEV     ',
     1 'DENSITY(1)  NAV(1) STNDEV(1)',/)
507   FORMAT(/,/,' MAX AND MIN DENSITY IN PLOT=',2F10.2)
510   FORMAT(3I5)
CHENN>
C511   FORMAT(I5,18A4)
511   FORMAT(I10,18A4)
CHENN<
512   FORMAT(' AREA TOO LARGE FOR STORE')
513   FORMAT(20A4)
514   FORMAT(2F11.5)
515   FORMAT('  RADIAL DENSITY FOR THIS FILM ALREADY',
     1' ON DISC, NOT REDETERMINED')
516   FORMAT(/,'  RADIAL DENSITY NOW SMOOTHED AND PUT ON DISC')
517   FORMAT(' SMOOTHING OVER 2*N + 1 POINTS, WHERE N=',I5,/)
9999  WRITE(6,9998)
9998  FORMAT(' ERROR ON INPUT FILE')
      STOP
C
1500  CALL IMCLOSE(1)
      CALL IMCLOSE(3)
      WRITE(6,9002) IBAD
CHENN>
      goto 999
 911  continue
      write(*,'('' backauto: ERROR on file open'')')
      STOP 'ERROR occured'
C
 999  continue
      STOP
CHENN<
C
      END
C###############################################################################
C
	SUBROUTINE PLOTRAST(NPLATE,TITLE,DX1,DY1,DX2,DY2)
	DIMENSION TITLE(1)
CTSH++
        CHARACTER*80 TEXT
CTSH--
C
      IF((ABS(DX1).LT.0.00001.AND.ABS(DY1).LT.0.00001).OR.
     . (ABS(DX2).LT.0.00001.AND.ABS(DY2).LT.0.00001)) THEN
      		WRITE(6,9985)
9985		FORMAT(' No PLOTRAST, since no proper lattice yet')
      		WRITE(6,9984)DX1,DY1,DX2,DY2
9984		FORMAT(' DX1,DY1,DX2,DY2',4F9.3)
      		RETURN
      ENDIF
      WRITE(6,5000)DX1,DY1,DX2,DY2
5000  FORMAT(' plotting peak and background rasters',/,
     .   '  DX1,DY1,DX2,DY2',4F9.3)
      SCALE=1.0
C
9993  XH=DX1*SCALE
      XK=DX2*SCALE
      YH=DY1*SCALE
      YK=DY2*SCALE
      XR1=XH+XK
      YR1=YH+YK
      XR2=-XH+XK
      YR2=-YH+YK
      IF(XR1.GT.150.0.OR.XR1.LT.-150.0)GO TO 9995
      IF(XR2.GT.150.0.OR.XR2.LT.-150.0)GO TO 9995
      IF(YR1.GT.150.0.OR.YR1.LT.-150.0)GO TO 9995
      IF(YR2.GT.150.0.OR.YR2.LT.-150.0)GO TO 9995
      GO TO 9994
9995  SCALE=SCALE/2.0
      GO TO 9993
C
C     TYPE OF BACKGROUNDS NTYPE=0
9994  NTYPE=0
      XB1=(XH+XK)/3
      YB1=(YH+YK)/3
      XB2=(2*XH-XK)/3
      YB2=(2*YH-YK)/3
      XB3=(XH-2*XK)/3
      YB3=(YH-2*YK)/3
C
9990  CONTINUE
C      CALL P2K_COLOUR(0)
      CALL P2K_MOVE(-150.0,-160.0,0.)
CTSH      WRITE(TEXT)NPLATE,(TITLE(J),J=1,18,511)
CTSH++
      WRITE(TEXT(1:77),511)NPLATE,(TITLE(J),J=1,18)
CTSH--
CHENN>
C511   FORMAT(I5,18A4)
511   FORMAT(I10,18A4)
CHENN<
      CALL P2K_STRING(TEXT,77,0.)
C
      CALL P2K_MOVE(-150.0,-168.,0.)
CTSH      WRITE(TEXT,9996) NSTEP,DX1,DY1,DX2,DY2,SCALE
CTSH++
      WRITE(TEXT(1:66),9996) NSTEP,DX1,DY1,DX2,DY2,SCALE
CTSH--
9996  FORMAT(' LATTICE VECTORS',I3,4F8.2,' SCALE'
     .,F4.2,'MM= 1')       
      CALL P2K_STRING(TEXT,66,0.)
C
      CALL P2K_MOVE(-150.,-176.,0.)
CTSH      WRITE(TEXT,9988)NTYPE
CTSH++
      WRITE(TEXT(1:40),9988)NTYPE
CTSH--
9988  FORMAT(' TYPE OF BACKGROUND POSITIONS; NTYPE =',I2)
      CALL P2K_STRING(TEXT,40,0.)
      CALL P2K_MOVE(-150.,0.,0.)
      CALL P2K_DRAW(150.,0.,0.)
      CALL P2K_MOVE(0.,-150.,0.)
      CALL P2K_DRAW(0.,150.,0.)
      CALL P2K_MOVE(0.,0.,0.)
C
      CALL P2K_DRAW(-XH,-YH,0.)
      CALL P2K_MOVE(-XH,-YH,0.)
CTSH      WRITE(TEXT,9991)
CTSH++
      WRITE(TEXT(1:1),9991)
CTSH--
9991  FORMAT('H')
      CALL P2K_CSTRING(TEXT,1,0.)
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(XH,YH,0.)
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(-XH,-YH,0.)
      CALL P2K_MOVE(-XK,-YK,0.)
CTSH      WRITE(TEXT,9992)
CTSH++
      WRITE(TEXT(1:1),9992)
CTSH--
9992  FORMAT('K')
      CALL P2K_CSTRING(TEXT,1,0.)
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(XK,YK,0.)
      CALL P2K_MOVE(XR1,YR1,0.)
      CALL P2K_DRAW(XR2,YR2,0.)
      CALL P2K_DRAW(-XR1,-YR1,0.)
      CALL P2K_DRAW(-XR2,-YR2,0.)
      CALL P2K_DRAW(XR1,YR1,0.)
C
      XP1=XB1-5.
      XQ1=XB1+5.
      YP1=YB1-5.
      YQ1=YB1+5.
      CALL P2K_MOVE(XP1,YB1,0.)
      CALL P2K_DRAW(XQ1,YB1,0.)
      CALL P2K_MOVE(XB1,YP1,0.)
      CALL P2K_DRAW(XB1,YQ1,0.)
      CALL P2K_MOVE(-XP1,-YB1,0.)
      CALL P2K_DRAW(-XQ1,-YB1,0.)
      CALL P2K_MOVE(-XB1,-YP1,0.)
      CALL P2K_DRAW(-XB1,-YQ1,0.)
C
      XP1=XP1/2
      XQ1=XQ1/2
      YP1=YP1/2
      YQ1=YQ1/2
      XB1=XB1/2
      YB1=YB1/2
      CALL P2K_MOVE(XP1,YB1,0.)
      CALL P2K_DRAW(XQ1,YB1,0.)
      CALL P2K_MOVE(XB1,YP1,0.)
      CALL P2K_DRAW(XB1,YQ1,0.)
      CALL P2K_MOVE(-XP1,-YB1,0.)
      CALL P2K_DRAW(-XQ1,-YB1,0.)
      CALL P2K_MOVE(-XB1,-YP1,0.)
      CALL P2K_DRAW(-XB1,-YQ1,0.)
C
      XP2=XB2-5.
      XQ2=XB2+5.
      YP2=YB2-5.
      YQ2=YB2+5.
      CALL P2K_MOVE(XB2,YP2,0.)
      CALL P2K_DRAW(XB2,YQ2,0.)
      CALL P2K_MOVE(XP2,YB2,0.)
      CALL P2K_DRAW(XQ2,YB2,0.)
      CALL P2K_MOVE(-XB2,-YP2,0.)
      CALL P2K_DRAW(-XB2,-YQ2,0.)
      CALL P2K_MOVE(-XP2,-YB2,0.)
      CALL P2K_DRAW(-XQ2,-YB2,0.)
C
      XP2=XP2/2
      XQ2=XQ2/2
      YP2=YP2/2
      YQ2=YQ2/2
      XB2=XB2/2
      YB2=YB2/2
      CALL P2K_MOVE(XB2,YP2,0.)
      CALL P2K_DRAW(XB2,YQ2,0.)
      CALL P2K_MOVE(XP2,YB2,0.)
      CALL P2K_DRAW(XQ2,YB2,0.)
      CALL P2K_MOVE(-XB2,-YP2,0.)
      CALL P2K_DRAW(-XB2,-YQ2,0.)
      CALL P2K_MOVE(-XP2,-YB2,0.)
      CALL P2K_DRAW(-XQ2,-YB2,0.)
C
      IF(XB3.EQ.0.0)GO TO 10000
      XP3=XB3-5.
      XQ3=XB3+5.
      YP3=YB3-5.
      YQ3=YB3+5.
      CALL P2K_MOVE(XB3,YP3,0.)
      CALL P2K_DRAW(XB3,YQ3,0.)
      CALL P2K_MOVE(XP3,YB3,0.)
      CALL P2K_DRAW(XQ3,YB3,0.)
      CALL P2K_MOVE(-XB3,-YP3,0.)
      CALL P2K_DRAW(-XB3,-YQ3,0.)
      CALL P2K_MOVE(-XP3,-YB3,0.)
      CALL P2K_DRAW(-XQ3,-YB3,0.)
C
      XP3=XP3/2
      XQ3=XQ3/2
      YP3=YP3/2
      YQ3=YQ3/2
      XB3=XB3/2
      YB3=YB3/2
      CALL P2K_MOVE(XB3,YP3,0.)
      CALL P2K_DRAW(XB3,YQ3,0.)
      CALL P2K_MOVE(XP3,YB3,0.)
      CALL P2K_DRAW(XQ3,YB3,0.)
      CALL P2K_MOVE(-XB3,-YP3,0.)
      CALL P2K_DRAW(-XB3,-YQ3,0.)
      CALL P2K_MOVE(-XP3,-YB3,0.)
      CALL P2K_DRAW(-XQ3,-YB3,0.)
C
      IF(NTYPE.EQ.2) THEN
      	GO TO 10000
      ENDIF
      IF(NTYPE.EQ.1)GO TO 9987
C     BACKGROUND POSITIONS NTYPE=1
      CALL P2K_PAGE
      CALL P2K_LWIDTH(0.0)
      XB1=(2*XH+XK)/3
      YB1=(2*YH+YK)/3
      XB2=(XH+2*XK)/3
      YB2=(YH+2*YK)/3
      XB3=(XH-XK)/3
      YB3=(YH-YK)/3
      NTYPE=1
      GO TO 9990
C
9987  CONTINUE
C     BACKGROUND POSITIONS NTYPE=2
      CALL P2K_PAGE
      CALL P2K_LWIDTH(0.0)
      XB1=(XH+XK)/2
      YB1=(YH+YK)/2
      XB2=(XH-XK)/2
      YB2=(YH-YK)/2
      XB3=0.0
      YB3=0.0
      NTYPE=2
      GO TO 9990
10000 CALL P2K_PAGE
      RETURN
	END
C###############################################################################
	SUBROUTINE AUTOCENTA(ARRAYSQ,DMIN,DMAX,NX,NY,CX,CY)
      	PARAMETER (IDIAM=50)
        PARAMETER (NP=5000)
	DIMENSION ARRAYSQ(1), A(IDIAM*IDIAM)
      	DIMENSION PX(NP),PY(NP),GX(NP),GY(NP),PERP(NP)
      	DATA PROPMIN/0.20/,PROPMAX/0.995/
C
      	WRITE(6,101)
101    FORMAT(' Entering AUTOCENTA - ',
     .   'determination of position of central blackening')
C
C  to calculate the centre of inelastic blackening automatically.
C  First find gradients of density in local areas all over the picture
C  using only regions of the picture where the density includes regions 
C  above PROPMIN - i.e. near the central blackening, not including pointer.
C
99   	DCUTMIN = (1.0-PROPMIN)*DMIN + PROPMIN*DMAX
      	DCUTMAX = (1.0-PROPMAX)*DMIN + PROPMAX*DMAX
C
C  Use coordinates relative to origin at corner of pattern at (1,1)
C  as used in PICKAUTO to calculate where spots are.
C  First, determine local density gradients all over pattern.
C
    	NXAREAS=NX/IDIAM
      	NYAREAS=NY/IDIAM
      	N = 0
      	NOT = 0
      	DO 130 I=1,NXAREAS
      	  DO 135 J=1,NYAREAS
      		IXB = 1+(I-1)*IDIAM
      		IYB = 1+(J-1)*IDIAM
      		DO 140 LX=1,IDIAM
      		DO 140 LY=1,IDIAM
			INDEX = IXB-1+LX + NX*(IYB-1+LY-1)
      			VAL = ARRAYSQ(INDEX)
      			IF(VAL.GE.DCUTMIN.AND.VAL.LE.DCUTMAX) THEN
      				INDA = LX+(LY-1)*IDIAM
      				A(INDA) = VAL
      			ELSE
C      				WRITE(6,137) I,J,LX,LY,VAL
137				FORMAT(' Discarded area',2I5,' point',2I5,' density',F10.2)
      				NOT=NOT+1
      				GO TO 135  	! discard all useless areas.
      			ENDIF
140		CONTINUE
      		N = N+1
      		IF(N.GT.NP) GO TO 131
      		CALL GETGRAD(A,IDIAM,GX(N),GY(N))
      		PX(N) = IXB+(IDIAM-1)/2.0
      		PY(N) = IYB+(IDIAM-1)/2.0
135	  CONTINUE
130	CONTINUE
131	WRITE(6,132) N,NOT,IDIAM,IDIAM
132	FORMAT(/,I7,' separate regions have significant density gradients'
     .   ,/,I7,' areas not used','  : size of each area =',I3,' x',I3)
      	IF(N.LT.7) THEN		! must have several gradients
      		PROPMIN=PROPMIN*0.70
      		WRITE(6,133) PROPMIN
133		FORMAT(' not enough gradients to determine centre, PROPMIN',F5.2,/)
      		GO TO 99	! restart with less stringent density cut-off
      	ENDIF
C
C  Second, search for centre which has least distance to gradient vectors.
C   apply weights proportional to the gradient determined.
C
      	IRANGE = 200
      	CXBEST = CX+NX/2.0
      	CYBEST = CY+NY/2.0
      	SMIN = IRANGE*10
      	DO 150 I=-IRANGE,IRANGE,10
      	DO 150 J=-IRANGE,IRANGE,10
      		SUMNUM=0.0
      		SUMDEN=0.0
      		XT=I+CX+NX/2.0
      		YT=J+CY+NY/2.0
      		DO 155 K=1,N
      			CALL GETPERP(XT,YT,PX(K),PY(K),GX(K),GY(K),PERP(K))
      			WEIGHT=SQRT(GX(K)**2+GY(K)**2)
      			SUMNUM=SUMNUM+PERP(K)*WEIGHT
      			SUMDEN=SUMDEN+1.0*WEIGHT
155		CONTINUE
      		IF(SUMNUM/SUMDEN.LT.SMIN) THEN
      			SMIN=SUMNUM/SUMDEN
      			CXBEST=XT
      			CYBEST=YT
      		ENDIF
150	CONTINUE
      	WRITE(6,156) CXBEST,CYBEST,SMIN
156	FORMAT(' Best centre from very rough search',2F10.2,
     .  '  mean weighted distance to gradient vector lines',F8.2)
C
C  Thirdly and finally, determine precise centre with removal of outliers
C  distant by more than 1 (FMULT) standard deviations, down to a minimum of
C  10 gradient observations.
C  Use gradient/curvature measured at +/-0.5 pixels from current centre.
C
      	WRITE(6,189)
189	FORMAT(/,' Precise refinement of centre of inelastic scattering',/,
     .   ' Cycle   XSHIFT   YSHIFT   CXBEST   CYBEST',
     .   '     xgrad     xcurv     ygrad     ycurv fshift  dev (N)')
      	FSHIFT=1.0
      	AVDEV=100000.0
CHENN>
        iconverge = 1
C      	DO 190 I=1,100
      	DO 190 I=1,500
CHENN<
      	  FMULT=1.0
191   	  SPXY0=0.0
      	  SPXM=0.0
      	  SPXP=0.0
      	  SPYM=0.0
      	  SPYP=0.0
      	  NUSED=0
      	  SUMDEV=0.0
      	  DO 192 K=1,N
      		CALL GETPERP(CXBEST,CYBEST,PX(K),PY(K),GX(K),GY(K),PERP(K))
C				get rid of outliers > 1 pixel and > 1 stddev
      		IF(I.LT.8.OR.PERP(K).LT.FMULT*AVDEV.OR.PERP(K).LT.1.0) THEN
      				SUMDEV=SUMDEV+ABS(PERP(K))
      				NUSED=NUSED+1
      				WEIGHT=SQRT(GX(K)**2+GY(K)**2)
      			SPXY0=SPXY0+PERP(K)*WEIGHT
      			CALL GETPERP(CXBEST-0.5,CYBEST,PX(K),PY(K),GX(K),GY(K),PERP(K))
      			SPXM=SPXM+PERP(K)*WEIGHT
      			CALL GETPERP(CXBEST+0.5,CYBEST,PX(K),PY(K),GX(K),GY(K),PERP(K))
      			SPXP=SPXP+PERP(K)*WEIGHT
      			CALL GETPERP(CXBEST,CYBEST-0.5,PX(K),PY(K),GX(K),GY(K),PERP(K))
      			SPYM=SPYM+PERP(K)*WEIGHT
      			CALL GETPERP(CXBEST,CYBEST+0.5,PX(K),PY(K),GX(K),GY(K),PERP(K))
      			SPYP=SPYP+PERP(K)*WEIGHT
      		ENDIF
192	  CONTINUE
      	  IF((N.GT.10.AND.NUSED.LT.10).OR.
     .	      (NUSED.NE.N.AND.NUSED.LT.4)) THEN
      		FMULT=FMULT*1.5
      		GO TO 191	! repeat the calculation of perps with more data
      	  ENDIF
      	  AVDEV=SUMDEV/NUSED
      		XGRAD = (SPXP-SPXM)/WEIGHT
      		XCURV = 4.0*(SPXP+SPXM-2.0*SPXY0)/WEIGHT
      		IF(XCURV.LT.0.5) XCURV=0.5	! enforce positive curvature
      		XSHIFT = -FSHIFT*XGRAD/XCURV		! minimise perpendiculars
      		XMOD = SIGN(XSHIFT,1.0)
      		XSHIFT = SIGN(AMIN1(XMOD,2.0),XSHIFT)	! limit shift to two pixels
      		 YGRAD = (SPYP-SPYM)/WEIGHT
      		 YCURV = 4.0*(SPYP+SPYM-2.0*SPXY0)/WEIGHT
      		 IF(YCURV.LT.0.5) YCURV=0.5	! enforce positive curvature
      		 YSHIFT = -FSHIFT*YGRAD/YCURV		! minimise perpendiculars
      		 YMOD = SIGN(YSHIFT,1.0)
      		 YSHIFT = SIGN(AMIN1(YMOD,2.0),YSHIFT)	! limit shift to two pixels
      		CXBEST=CXBEST+XSHIFT
      		CYBEST=CYBEST+YSHIFT
      		WRITE(6,197) I,XSHIFT,YSHIFT,CXBEST,CYBEST,
     .    		XGRAD,XCURV,YGRAD,YCURV,FSHIFT,AVDEV,NUSED
197		FORMAT(I6,4F9.3,4F10.5,2F6.2,I4)
      		IF(ABS(XSHIFT).LT.0.001.AND.ABS(YSHIFT).LT.0.001) GO TO 230
      		IF(I.GT.10) THEN
      			IF(SIGN(1.0,XOLD).NE.SIGN(1.0,XSHIFT).AND.
     .      	   SIGN(1.0,YOLD).NE.SIGN(1.0,YSHIFT)) FSHIFT=FSHIFT/2.0
      		ENDIF
      		XOLD=XSHIFT
      		YOLD=YSHIFT
190	CONTINUE
        iconverge=0
	WRITE(6,201) 
201	FORMAT(' Refinement of centre of blackening did not converge')
230	WRITE(6,231) CXBEST,CYBEST
231	FORMAT(' New centre after refinement',2F10.3)
	CX=CXBEST-NX/2.0
	CY=CYBEST-NY/2.0
	WRITE(6,232) CX,CY
232	FORMAT(' New values of CX,CY after refinement',2F10.3)
C
CHENN>
C
        if(iconverge.eq.1)then
          open(18,FILE='TMP224433.tmp',STATUS='NEW',ERR=911)
          write(18,'(2F12.3)')CXBEST,CYBEST
          close(18)
          write(*,
     .    '(/,/,''file TMP224433.tmp written with new origin '',
     .     2F12.3,/,/)')CXBEST,CYBEST
        else
          write(*,'(/,/,/,'' ============================'')')
          write(*,'('' ============================ '')')
          write(*,'('' did not converge in backauto '')')
          write(*,'('' ============================ '')')
          write(*,'('' ============================ '',/,/,/)')
        endif
C
CHENN<
C
	RETURN
C
CHENN>
C
 911    continue
        write(*,'('' backauto: ERROR on file open'')')
        STOP 'ERROR occured'
C
CHENN<
C
	END
C##############################################################################
      	SUBROUTINE GETPERP(XT,YT,PX,PY,GX,GY,PERP)
      	IF(GX.NE.0.0.OR.GY.NE.0.0) THEN
      		GXY = GX**2+GY**2
      		GXY = SQRT(GXY)
      		IF(GX.NE.0.0) THEN
      			YL=PY-(PX-XT)*GY/GX
      			DY=YT-YL
      			PERP=ABS(DY*GX/GXY)
      		ELSE
      			XL=PX+(YT-PY)*GX/GY
      			DX=XT-XL
      			PERP=ABS(DX*GY/GXY)
      		ENDIF
      	ENDIF
      	RETURN
      	END
C##############################################################################
      	SUBROUTINE GETGRAD(A,IDIAM,GX,GY)
      	DIMENSION A(1)
      	REAL*8 AM(3,3),BM(3),W(100),E
C
C  Least squares fitting to find gradient with elimination of outliers, 
C   probably mostly spots and dirt.
      	SIGMA=1000000.0
      	D0=A(1)
      	GX=0.0
      	GY=0.0
      	DO 300 NC=1,25
      		DO 200 I=1,3
      		  BM(I)=0.0
      		DO 200 J=1,3
200		AM(I,J)=0.0
      		S=0.0
      		SX=0.0
      		SY=0.0
      		SXY=0.0
      		SXX=0.0
      		SYY=0.0
      		SD=0.0
      		SXD=0.0
      		SYD=0.0
      		DO 250 I=1,IDIAM
      		DO 250 J=1,IDIAM
      			INDEX=I+(J-1)*IDIAM
      			D=A(INDEX)
      			DD=ABS(D-D0-GX*I-GY*J)
      			IF(DD.LT.0.75*SIGMA) THEN
      				S=S+1.0
      				SX=SX+I
      				SY=SY+J
      				SXY=SXY+I*J
      				SXX=SXX+I*I
      				SYY=SYY+J*J
      				SD=SD+D
      				SXD=SXD+I*D
      				SYD=SYD+J*D
      			ENDIF
250		CONTINUE
      		AM(1,1)=S
      		AM(1,2)=SX
      		AM(1,3)=SY
      		AM(2,1)=SX
      		AM(2,2)=SXX
      		AM(2,3)=SXY
      		AM(3,1)=SY
      		AM(3,2)=SXY
      		AM(3,3)=SYY
      		BM(1)=SD
      		BM(2)=SXD
      		BM(3)=SYD
                IA=3
                NA=3
                E=-1.0
      		CALL MA21AD(AM,IA,NA,BM,W,E)
      		D0=BM(1)
      		GX=BM(2)
      		GY=BM(3)
C  Calculate SIGMA standard deviation of densities from fitted plane.
      		S=0.0
      		SDD=0.0
      		DO 270 K=1,IDIAM
      		DO 270 L=1,IDIAM
      			INDEX=K+(L-1)*IDIAM
      			D=A(INDEX)
      			S=S+1.0
270		SDD=SDD+(D-D0-GX*K-GY*L)**2
      		SIGMA=SQRT(SDD/S)
300	CONTINUE
      	RETURN
      	END
C##############################################################################
