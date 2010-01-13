C TWOLATT  - plots two lattices to see if there is overlap
C
C  INPUT PARAMETERS
C      CARD 1    A1X A1Y B1X B1Y	LATTICE 1 in pixels (*)
C      CARD 2    A2X A2Y B2X B2Y	LATTICE 2 in pixels (*)
C      CARD 3    RADLIM			LIMIT FOR PLOT in pixels (*)
C      CARD 4    TITLE			          (20A4)
C      CARD 5    DISTEST	threshold for interference (*)
C
C	v1.00	1.6.96	RH	Original program
C	v1.01	18.8.00	RH	converted to plot2000
C         "     13.6.01 TSH     P2K_FONT needed string terminator
C
      PARAMETER (PLTSIZ=300.09)
      PARAMETER (CHRSIZ=0.6)
      PARAMETER (FONTSIZE=3.5)
CTSH      DIMENSION TITLEIN(20),TEXT(20)
CTSH++
      DIMENSION TITLEIN(20)
      CHARACTER*80 TEXT
CTSH--
      WRITE(6,1)
1     FORMAT(/'  TWOLATT v1.01: 18.8.00'//)
      READ(5,*) A1X,A1Y,B1X,B1Y
      READ(5,*) A2X,A2Y,B2X,B2Y
      WRITE(6,101) A1X,A1Y,B1X,B1Y,A2X,A2Y,B2X,B2Y
101   FORMAT(' LATTICE PARAMETERS A1X,A1Y............',2F10.2/
     .       '                    B1X,B1Y............',2F10.2/
     .       '                    A2X,A2Y............',2F10.2/
     .       '                    B2X,B2Y............',2F10.2)
      READ(5,*) RADLIM
      WRITE(6,102) RADLIM
102   FORMAT(' RADIUS LIMIT.......................',F8.0)
      READ(5,2) TITLEIN
2     FORMAT(20A4)
      WRITE(6,98) TITLEIN
98    FORMAT(' TITLE FOR PLOT AND OUTPUT FILE :',20A4)
      READ(5,*) DISTEST
C
C
      CALL P2K_OUTFILE('PLOT.PS',7)
      CALL P2K_HOME
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
      CALL P2K_GRID(0.5*PLTSIZ,0.5*PLTSIZ,1.0)
      CALL P2K_ORIGIN(-0.5*PLTSIZ,-0.7*PLTSIZ,0.)
      CALL P2K_COLOUR(0)
      YPOSN=PLTSIZ+2.
      CALL P2K_MOVE(5.0,YPOSN,0.)
      CALL P2K_STRING(TITLEIN,80,0.)
      SCALE=0.95*PLTSIZ/(2.0*RADLIM)
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(PLTSIZ,0.,0.)
      CALL P2K_DRAW(PLTSIZ,PLTSIZ,0.)
      CALL P2K_DRAW(0.,PLTSIZ,0.)
      CALL P2K_DRAW(0.,0.,0.)
      CENTRE=PLTSIZ/2.0
      CALL P2K_ORIGIN(CENTRE,CENTRE,0.)
C
      CALL P2K_MOVE(-CHRSIZ,-CHRSIZ,0.)
      CALL P2K_DRAW(CHRSIZ,CHRSIZ,0.)
      CALL P2K_MOVE(CHRSIZ,-CHRSIZ,0.)
      CALL P2K_DRAW(-CHRSIZ,CHRSIZ,0.)	! CENTRAL CROSS AT ORIGIN.
C
C LATTICE 1
      	ALENGTH=SQRT(A1X**2+A1Y**2)
      	X=(A1X/ALENGTH)*(PLTSIZ/2.0)
      	Y=(A1Y/ALENGTH)*(PLTSIZ/2.0)
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(X,Y,0.)
      	  X=X+3.
      	  Y=Y-3.
      CALL P2K_MOVE(X,Y,0.)
CTSH      	  WRITE(TEXT,151)
CTSH++
      	  WRITE(TEXT,151)
CTSH--
      CALL P2K_CSTRING(TEXT,1,0.)
      	BLENGTH=SQRT(B1X**2+B1Y**2)
      	X=(B1X/BLENGTH)*(PLTSIZ/2.0)
      	Y=(B1Y/BLENGTH)*(PLTSIZ/2.0)
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(X,Y,0.)
      	  X=X+3.
      	  Y=Y-3.
      CALL P2K_MOVE(X,Y,0.)
CTSH      	  WRITE(TEXT,152)
CTSH++
      	  WRITE(TEXT,152)
CTSH--
      CALL P2K_CSTRING(TEXT,1,0.)
C
C  LATTICE 2
      	ALENGTH=SQRT(A2X**2+A2Y**2)
      	X=(A2X/ALENGTH)*(PLTSIZ/2.0)
      	Y=(A2Y/ALENGTH)*(PLTSIZ/2.0)
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(X,Y,0.)
      	  X=X+3.
      	  Y=Y-3.
      CALL P2K_MOVE(X,Y,0.)
CTSH      	  WRITE(TEXT,151)
CTSH++
      	  WRITE(TEXT,151)
CTSH--
151		FORMAT('H')
152		FORMAT('K')
      CALL P2K_CSTRING(TEXT,1,0.)
      	BLENGTH=SQRT(B2X**2+B2Y**2)
      	X=(B2X/BLENGTH)*(PLTSIZ/2.0)
      	Y=(B2Y/BLENGTH)*(PLTSIZ/2.0)
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(X,Y,0.)
      	  X=X+3.
      	  Y=Y-3.
      CALL P2K_MOVE(X,Y,0.)
CTSH      	  WRITE(TEXT,152)
CTSH++
      	  WRITE(TEXT,152)
CTSH--
      CALL P2K_CSTRING(TEXT,1,0.)
C
C
      CALL PLOTLATT(A1X,A1Y,B1X,B1Y,RADLIM,1,SCALE)
      CALL PLOTLATT(A2X,A2Y,B2X,B2Y,RADLIM,2,SCALE)
      CALL P2K_PAGE
      END
C********************************************************************
C
      SUBROUTINE PLOTLATT(AX,AY,BX,BY,RADLIM,NTYPE,SCALE)
      WRITE(6,10)
10    FORMAT(' Entering PLOTLATT')
      NSPOTS=0
      CHRSIZA=0.9
      IF(NTYPE.GE.2) CHRSIZA=CHRSIZA*1.414
      DO 109 IH = -80,80
      DO 109 IK = -80,80
C      WRITE(6,15) IH,IK,AX,AY,BX,BY
15    FORMAT(2I5,4F10.2)
      	X=IH*AX+IK*BX
      	Y=IH*AY+IK*BY
      	R=SQRT(X**2+Y**2)
     	 IF(ABS(R).GE.RADLIM)GO TO 100
      	NSPOTS=NSPOTS+1
      	X=X*SCALE
      	Y=Y*SCALE
C     	 WRITE(6,104)X,Y
104   	FORMAT(2F10.1)
      IF(NTYPE.LE.1) THEN
      	XN=X-CHRSIZA
      	XP=X+CHRSIZA
      	YN=Y-CHRSIZA
      	YP=Y+CHRSIZA
      	CALL P2K_MOVE(XN,YN,0.)
      	CALL P2K_DRAW(XP,YN,0.)
      	CALL P2K_DRAW(XP,YP,0.)
      	CALL P2K_DRAW(XN,YP,0.)
      	CALL P2K_DRAW(XN,YN,0.)
      ELSE
      	XN=X-CHRSIZA
      	XP=X+CHRSIZA
      	YN=Y-CHRSIZA
      	YP=Y+CHRSIZA
      	CALL P2K_MOVE(XP,Y,0.)
      	CALL P2K_DRAW(X,YP,0.)
      	CALL P2K_DRAW(XN,Y,0.)
      	CALL P2K_DRAW(X,YN,0.)
      	CALL P2K_DRAW(XP,Y,0.)
      ENDIF
C
C      	X=X-0.3				! ADJUST CHARACTER TO BE CENTRAL IN X.
C      	Y=Y+0.5				! ADJUST CHARACTER TO BE CENTRAL IN Y.
C      	CALL LOCCHR(X,Y,0)
C      	WRITE(TEXT,160)
C      	IF(IQIN.EQ.1) WRITE(TEXT,161)	! IQIN=1 include number
C      	IF(IQIN.EQ.2) WRITE(TEXT,162)	! IQIN=2 include number
C      	IF(IQIN.EQ.3) WRITE(TEXT,163)	! IQIN=3 include number
C      	IF(IQIN.EQ.4) WRITE(TEXT,164)	! IQIN=4 include number
C160	FORMAT(' ')
C161	FORMAT('1')
C162	FORMAT('2')
C163	FORMAT('3')
C164	FORMAT('4')
C      	CALL CSTRING(TEXT,1)
C
100   	CONTINUE
109   CONTINUE
      WRITE(6,20) NSPOTS,NTYPE
20    FORMAT(//I10,' spots plotted for lattice',I5)
      RETURN
      END
