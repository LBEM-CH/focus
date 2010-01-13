C     ****** AUTOINDEX ******
C
C  This program reads in a digitized electron diffraction pattern, to which 
C    background correction has already been applied, searches for peaks, 
C    averages the image areas centred on each peak, then performs a radial 
C    spiral search until the two shortest base vectors are found. 
C    These are the A- and B-axes. 
C        In subroutine REINDEX, which can be a dummy subroutine, the base vector
C    indexing can be rearranged to fit with a crystal-specific scheme defined
C    by the user. For example, purple membrane hk requires I(4,3) >> I(3,4). 
C        The lattice parameters together with suggested integration and 
C    background raster sizes are put into the header records of the input file 
C    and the output average picture file. 
C
C     VX1.0	RH	22-Mar-1992	original program
C     VX1.1	RH	 8-Apr-1992	various debugs
C     VX1.2	RH	13-Apr-1992	more debugs
C     VX1.3	RH	19-May-1992	CX,CY debug
C     VX1.4	RH	21-Mar-1993	default NSTEP=4
C     VX1.5	RH	16-Aug-1993	always return from SEARCH on failure
C     VX1.6	RH	 1-Nov-1994	fine tuning FPBOX -> 0.25, NRMAX -> 15
C     VX1.7	RH	12-Nov-1994	more tuning FPBOX -> 0.19, FRACT -> 0.20
C     VX1.8	RH	 6-Mar-1995	decrease FPBOX, FRACT till peaks found
C     VX1.9	RH	21-Sep-1995	changed over to  Alpha, plot on PLOTOUT
C     VX2.0	RH	17-Mar-1996	increase dimensions to 3000
C     VX2.1	RH	22-Mar-1996	ignore very edge of pattern (3 pixels)
C     VX2.2	RH	12-Jul-1996	checks radius near edge of average box
C     VX2.3	RH	14-Jul-1996	subroutine EXTEND, increased accuracy
C     VX2.4	RH	21-Jul-1996	improved lattice parameter refinement
C     VX2.5	RH	27-Jan-1997	increase search radius in 824 doloop
C     VX2.6	RH	29-Dec-1997	detect weaker spots on lower background
C     VX2.7	RH	 4-Oct-1998	increase sigma from 2.8 to 7.8 spot det
C     VX3.0	RH	15-Aug-2000	convert to plot2000 direct to postscript
C       "       TSH     13-Jun-2001     P2K_FONT needed string terminator
C
C     Output, for cosmetic examination only, is average picture near an average 
C     spot, together with the assigned indexing, on OUT
C     There is also a plot file showing the selected rasters on PLOTOUT
C
C     For input, requires only the file name of the background-corrected 
C     pattern, on IN
C
	PARAMETER (ISIZE=8000)
CHENN>
C	PARAMETER (IAVER=100)
C	PARAMETER (NMAX=100)
	PARAMETER (IAVER=300)
	PARAMETER (NMAX=500)
CHENN<
	DIMENSION TEXT(20),TITLE(20)
CTSH++
	CHARACTER TMPTEXT*80
	EQUIVALENCE (TMPTEXT,TEXT)
CTSH--
	DIMENSION ARRAY(ISIZE*ISIZE)
	DIMENSION ALINE(2*IAVER+1),AVER(-IAVER:IAVER,-IAVER:IAVER)
	DIMENSION IEXT(12),NXYZ(3),MXYZ(3),NXYZST(3)
	DIMENSION XC(NMAX),YC(NMAX)
	DIMENSION DT(NMAX),XT(NMAX),YT(NMAX),NT(NMAX)
	DATA NXYZST/3*0/,NL/1/
	DO 50 I=-IAVER,IAVER
	DO 50 J=-IAVER,IAVER
50	AVER(I,J)=0.0
C
C  Parameters which affect the autoindexing performance
	THRESH=40.0	! GETSPOTS uses this pixel density threshold
	DIST=10		! GETSPOTS uses this to define min spot separation
	FPBOX=0.19	! SEARCH uses this to find peak raster box size
	FRACT=0.20	! SEARCH uses this in spiral search from origin
	NMIN=20		! minimum number of spots from GETSPOTS to proceed 
C	NMAX=100	! (see PARAMETER) maximum number allowed - as NMIN
C	NEARCEN=(NX+NY)/12  	! (see below) omit spots away from centre 
C	IAVER=100	! (see PARAMETER) radius near central spot searched 
C
	WRITE(6,1000)
1000	FORMAT(/,/,' AUTOINDEX - autoindexing electron diffraction',
     . ' pattern --  Version 3.0(15-Aug-2000)',/)
      CALL IMOPEN(1,'IN','OLD')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      CALL IRTLAB(1,TITLE,NL)
      CALL IMOPEN(3,'OUT','NEW')
      NX=NXYZ(1)
      NY=NXYZ(2)
      NEARCEN=(NX+NY)/12  	! keep spots away from centre
CHENN>
      IFARCEN=(NX+NY)/5
      READ(5,*) THRESH,DIST,FPBOX,FRACT,NMIN,IMAXCYC
C
C	WRITE(6,51)THRESH,DIST,FPBOX*100. ,FRACT*100. ,
C     . NMAX,NMIN,NEARCEN,IAVER
      WRITE(6,51)THRESH,DIST,FPBOX*100. ,FRACT*100. ,
     . NMAX,NMIN,NEARCEN,IFARCEN,IAVER
C 51	FORMAT(' Parameters set in programme which may affect success',/
C      . '    Initial density threshold for spot detection  =',F8.0,/
C      . '    Minimum spot separation to define separate    =',F8.0,
C      . '  pixels',/
C      . '    % of peak used for peak raster size selection =',F8.0,/
C      . '    % of central peak used to find lattice peaks  =',F8.0,/
C      . '    Maximum and minimum number of spots required  =',2I4,/
C      . '    Radius of circle near direct beam omitted     =',I6,/
C      . '    Radius around central spot searched           =',I6,/)
C
51	FORMAT(' Parameters set in programme which may affect success',/,
     . '    Initial density threshold for spot detection  =',F8.0,/,
     . '    Minimum spot separation to define separate    =',F8.0,
     . '  pixels',/,
     . '    % of peak used for peak raster size selection =',F8.0,/,
     . '    % of central peak used to find lattice peaks  =',F8.0,/,
     . '    Maximum and minimum number of spots required  =',2I4,/,
     . '    Radius of circle near direct beam omitted     =',I10,/,
     . '    Radius of circle far from direct beam ommitted=',I10,/,
     . '    Radius around central spot searched           =',I10,/)
C
CHENN<
C
CTSH	WRITE(TEXT,9997)
CTSH++
C	WRITE(TMPTEXT(1:48),9997)
        WRITE(6,*) ' Averaged area around averaged spot ', TMPTEXT(1:48)
CTSH--
C9997	FORMAT(' Averaged area around averaged spot')
	CALL ITRHDR(3,1)
	  JAVER=2*IAVER+1
	  NXYZ(1)=JAVER
	  NXYZ(2)=JAVER
	  NXYZ(3)=1
	CALL IALSIZ(3,NXYZ,NXYZST)
	CALL IWRHDR(3,TEXT,1,DMIN,DMAX,DMEAN)
	CALL IRTEXT(1,IEXT,1,7)
C             read in 7 entries in IEXT for NSTEP, OX,OY,DX1,DY1,DX2,DY2
	CALL IRDSEC(1,ARRAY,*9999)
	GO TO 9990
9999		WRITE(6,9998)
9998		FORMAT(' Disk input error on IN -- IRDSEC')
		STOP
9990		CONTINUE
	NSTEP=IEXT(1)/25.0
      	WRITE(6,9400) (IEXT(I),I=1,7),NSTEP
9400	FORMAT(' IEXTRA(7) on autoindex input   ',7I6,',  NSTEP=',I5)
      	IF(NSTEP. EQ. 0) NSTEP=4	! default, if header empty to prevent crash
	STEP10=NSTEP*10
	IF(STEP10. GE. 10) THEN 
	  DX1=IEXT(4)/STEP10
	  DY1=IEXT(5)/STEP10
	  DX2=IEXT(6)/STEP10
	  DY2=IEXT(7)/STEP10
C
	  CX=IEXT(2)/STEP10
	  CY=IEXT(3)/STEP10
	ELSE
	  CX=0.0
	  CY=0.0
	ENDIF
	WRITE(6,90) CX,CY,DX1,DY1,DX2,DY2
90	FORMAT(' Coordinates of centre from input file',2F8.2,/,
     . '    lattice parameters from input file',4F6.1)
C
CHENN>
C	DO 100 J=1,5
C	  CALL GETSPOTS(XC,YC,NTOT,DT,XT,YT,NT,
C     . NEARCEN,CX,CY,ARRAY,NX,NY,THRESH,DIST,NMAX)
	DO 100 J=1,20
	  CALL GETSPOTS(XC,YC,NTOT,DT,XT,YT,NT,
     . NEARCEN,CX,CY,ARRAY,NX,NY,THRESH,DIST,NMAX,IFARCEN)
CHENN<
		WRITE(6,106)NTOT
106		FORMAT(' returned from GETSPOTS with',I5,'  spots')
	  IF (NTOT. GT. 30) THEN
		GO TO 105
	  ELSE
		THRESH=THRESH-5.0
		WRITE(6,101) THRESH
101		FORMAT(' THRESH reduced to',F10.1)
	  ENDIF
100	CONTINUE
105	CONTINUE
C
C   Now average together all the image areas centred around each spot
C
	
	DO 200 J=1,NTOT
		IXC=XC(J)+0.5
		IYC=YC(J)+0.5
		DO 160 IX=-IAVER,IAVER
		DO 160 IY=-IAVER,IAVER
		  IXA=IXC+IX
		  IYA=IYC+IY
		  IF(IXA. GE. 1. AND. IXA. LE. NX) THEN
		     IF(IYA. GE. 1. AND. IYA. LE. NY) THEN
			INDEX=IXA+(IYA-1)*NX
			AVER(IX,IY)=AVER(IX,IY)+ARRAY(INDEX)
		     ENDIF
		  ENDIF
160		CONTINUE
200	CONTINUE
	DO 220 IX=-IAVER,IAVER
	DO 220 IY=-IAVER,IAVER
220	AVER(IX,IY)=AVER(IX,IY)/NTOT
	WRITE(6,221)
221	FORMAT(' area round all spots now averaged')
C
C  Now average by two-fold rotation about (1/2,1/2)
C   replace only x. ge. 0 by average, leaving left side unaveraged
C
	DO 250 IX=1,IAVER
	DO 250 IY=-IAVER,IAVER
250	AVER(IX,IY)=0.5*(AVER(IX,IY)+AVER(-IX,-IY))
	DO 260 IY=1,IAVER
	AVER(0,IY)=0.5*(AVER(0,IY)+AVER(0,-IY))
260	AVER(0,-IY)=AVER(0,IY)
C
	CALL ICLDEN(AVER,JAVER,JAVER,1,JAVER,1,JAVER,D1,D2,D3)
	CALL IWRHDR(3,TEXT,-1,D1,D2,D3)
C
C  Output of averaged area around averaged spot
C
	DO 300 JY=-IAVER,IAVER
		DO 310 JX=-IAVER,IAVER
310		ALINE(JX+IAVER+1)=AVER(JX,JY)		
300		CALL IWRLIN(3,ALINE)
C
C  Now search for two short vectors, which should be the axes
C
CHENN>
C	CALL SEARCH(AVER,FRACT,FPBOX,DX1,DY1,DX2,DY2,
C     . DMAX,NXP,NYP,NXB,NYB,NTYPE)
	CALL SEARCH(AVER,FRACT,FPBOX,DX1,DY1,DX2,DY2,
     . DMAX,NXP,NYP,NXB,NYB,NTYPE,IMAXCYC)
CHENN<
	IF(DX1. NE. 0. OR. DY1. NE. 0) THEN	! i. e. successful indexing
     	  CALL REINDEX(CX,CY,DX1,DY1,DX2,DY2,ARRAY,NX,NY)
C Put back lattice parameters into old and new picture files
	  IEXT(4)=DX1*STEP10
	  IEXT(5)=DY1*STEP10
	  IEXT(6)=DX2*STEP10
	  IEXT(7)=DY2*STEP10
	  IEXT(8)=NXP
	  IEXT(9)=NYP
	  IEXT(10)=NXB
	  IEXT(11)=NYB
	  IEXT(12)=NTYPE
      	  WRITE(6,9500)(IEXT(I),I=1,12),NSTEP
9500	  FORMAT(' IEXTRA(12) on autoindex output ',12I6,',  NSTEP=',I5)
		CALL IALEXT(1,IEXT,1,12)
		CALL IWRHDR(1,TITLE,-1,DMIN,DMAX,DMEAN)
		CALL IALEXT(3,IEXT,1,12)
		CALL IWRHDR(3,TEXT,-1,D1,D2,D3)
      	ENDIF
C
      	CALL IMCLOSE(1)
      	CALL IMCLOSE(3)
C
CHENN>
C	IF(ABS(DX1+DX2+DY1+DY2). GT. 0.00001)
C     . CALL PLOTRAST(TITLE,DX1,DY1,DX2,DY2,NXP,NYP,NXB,NYB,NTYPE)
C
        IF(ABS(DX1+DX2+DY1+DY2) .GT. 0.00001)then
          CALL PLOTRAST(TITLE,DX1,DY1,DX2,DY2,NXP,NYP,NXB,NYB,NTYPE)
          open(9,FILE='TMP123774.tmp',STATUS='NEW',ERR=990)
          write(9,'(4F12.4)')DX1,DY1,DX2,DY2
          close(9)
        endif
        goto 999
C
 990    continue
        write(*,'('' ERROR on file open'')')
        stop 'ERROR occured'
C
 999    continue
        write(*,'('' autoindex finished normally. '')')
        stop
CHENN<
C
	END
C*******************************************************************************
CHENN>
C	SUBROUTINE GETSPOTS(XC,YC,NTOT,DT,XT,YT,NT,
C     . NEARCEN,CX,CY,ARRAY,NX,NY,THRESH,DIST,NMAX)
	SUBROUTINE GETSPOTS(XC,YC,NTOT,DT,XT,YT,NT,
     . NEARCEN,CX,CY,ARRAY,NX,NY,THRESH,DIST,NMAX,IFARCEN)
CHENN<
C  Searches ARRAY for independent spots greater than THRESH and separated
C  by more than DIST
C  Exclude spots too near the edge of the whole pattern
	DIMENSION ARRAY(1),XC(1),YC(1),DT(1),XT(1),YT(1),NT(1)
	WRITE(6,402)
402	FORMAT(/,/,' Entering GETSPOTS, random searching')
401	NTOT=0
	DO 405 J=1,NMAX
	XC(J)=0.0
	YC(J)=0.0
	XT(J)=0.0
	YT(J)=0.0
	DT(J)=0.0
	NT(J)=0
	NPIXELFOUND=0
	NPIXELDENS=0
405	CONTINUE
	DO 400 IX=4,NX-3
	DO 410 IY=4,NY-3
	 INDEX=IX+(IY-1)*NX
	 DENS=ARRAY(INDEX)
	 IF(DENS .GE. THRESH) THEN
	  NPIXELDENS=NPIXELDENS+1
	  DISTCEN=(FLOAT(NX)/2.0+CX-FLOAT(IX))**2+
     . (FLOAT(NY)/2.0+CY-FLOAT(IY))**2
	  DISTCEN=SQRT(DISTCEN)
CHENN>
C	  IF(DISTCEN. GT. NEARCEN) THEN
	  IF(DISTCEN .GT. NEARCEN .and. DISTCEN .LT. IFARCEN) THEN
CHENN<
	    NPIXELFOUND=NPIXELFOUND+1
	    IF(NTOT.EQ.0) THEN
C			this is first spot
	      XT(1)=XT(1)+IX
	      YT(1)=YT(1)+IY
	      NT(1)=NT(1)+1
	      XC(1)=XT(1)
	      YC(1)=YT(1)
	      DT(1)=DT(1)+DENS
	      NTOT=1
	    ELSE
C			search for existing spot for this pixel
	      DO 420 J=1,NTOT
		TESTDIST=SQRT((XC(J)-FLOAT(IX))**2+(YC(J)-FLOAT(IY))**2)
		IF(TESTDIST.LT. DIST/2.0)  THEN
C				add and escape
			XT(J)=XT(J)+IX
			YT(J)=YT(J)+IY
			NT(J)=NT(J)+1
			XC(J)=(XC(J)*DT(J)+IX*DENS)/(DT(J)+DENS)
			YC(J)=(YC(J)*DT(J)+IY*DENS)/(DT(J)+DENS)
			DT(J)=DT(J)+DENS
			GO TO 430
		ENDIF
420	      CONTINUE
C			no nearby spot in list
	      NTOT=NTOT+1
	      IF(NTOT .GT. NMAX) THEN
CHENN>
C		  THRESH=THRESH+20
		  THRESH=THRESH+12
CHENN<
		  WRITE(6,422)NMAX,THRESH
422		  FORMAT(' too many spots for store, THRESH increased',/,
     .'   NMAX,THRESH =',I5,F10.2)
		  GO TO 401
	      ENDIF
	      XT(NTOT)=XT(NTOT)+IX
	      YT(NTOT)=YT(NTOT)+IY
	      NT(NTOT)=NT(NTOT)+1
	      XC(NTOT)=XT(NTOT)
	      YC(NTOT)=YT(NTOT)
	      DT(NTOT)=DT(NTOT)+DENS
430	      CONTINUE
	    ENDIF
	  ENDIF
	 ENDIF
410	CONTINUE
400	CONTINUE
	WRITE(6,450)NPIXELDENS,NPIXELFOUND
450	FORMAT(' total number of pixels found .gt. THRESH, ',
     . 'total not near centre',2I10)
	DO 460 J=1,NTOT
C	WRITE(6,461)J,XC(J),YC(J),NT(J)
461	FORMAT(' spot, coordinates, and total pixels per spot',
     . I5,2F8.1,I5)
460	CONTINUE
	RETURN
	END
C*******************************************************************************
CHENN>
C	SUBROUTINE SEARCH(AVER,FRACT,FPBOX,DX1,DY1,DX2,DY2,
C     . DMAX,NXP,NYP,NXB,NYB,NTYPE)
	SUBROUTINE SEARCH(AVER,FRACT,FPBOX,DX1,DY1,DX2,DY2,
     . DMAX,NXP,NYP,NXB,NYB,NTYPE,IMAXCYC)
CHENN<
C  searhes averaged array of spots for two independent vectors which are the
C  lattice parameters, then decides on the rasters sizes and positions to be 
C  for the spot integration in the next program PICKYCOR
CHENN>
C	PARAMETER  (IAVER=100)
C	PARAMETER  (NRMAX=60)
	PARAMETER  (IAVER=300)
	PARAMETER  (NRMAX=200)
CHENN<
	DIMENSION AVER(-IAVER:IAVER,-IAVER:IAVER)
	DIMENSION DRAD(NRMAX), NRAD(NRMAX), DSQRAD(NRMAX)
C*** mod by jms to zero on certain very rarely encountered computers
c	DATA DRAD/NRMAX*0.0/,NRAD/NRMAX*0/
	do i=1,nrmax
	 drad(i) = 0.0
      	 dsqrad(i) = 0.0
	 nrad(i) = 0
	end do
	WRITE(6,601)
601	FORMAT(/,/' Entering SEARCH - spiral search for short',
     . ' independent vectors')
	DX1=0.0
	DY1=0.0
	DX2=0.0
	DY2=0.0
      	NRMAXCENT=15		! peak profile maximum search radius
C
C   first find width of central spot and minimum average radial density
	DO 600 IX=-NRMAX,NRMAX
	DO 600 IY=-NRMAX,NRMAX
      		IRAD=1+IX**2+IY**2
		IF(IRAD.NE. 1)IRAD=SQRT(FLOAT(IRAD))+1.5
		IF(IRAD.GT. NRMAX) GO TO 600
		DRAD(IRAD)=DRAD(IRAD)+AVER(IX,IY)
		DSQRAD(IRAD)=DSQRAD(IRAD)+AVER(IX,IY)**2
		NRAD(IRAD)=NRAD(IRAD)+1
600	CONTINUE
C
      	write(6,*) ' Radius, number,  mean density, standard deviation'
	DO 610 IRAD=1,NRMAX
	   IF(NRAD(IRAD).GT. 0) THEN 
      		DRAD(IRAD)=DRAD(IRAD)/NRAD(IRAD)	! mean radial density
      		DSQRAD(IRAD) = (DSQRAD(IRAD) - 
     . (2.0-1.0/FLOAT(NRAD(IRAD)))*DRAD(IRAD)**2)/NRAD(IRAD) ! variance
      		DSQRAD(IRAD)=SQRT(DSQRAD(IRAD))		! standard deviation
CHENN>
             write(6,'(2I7,2F14.2)') 
     . IRAD,NRAD(IRAD),DRAD(IRAD),DSQRAD(IRAD)
           else
             write(6,'(''NRAD(IRAD)<=0 for '',2I7,2F14.2)') 
     . IRAD,NRAD(IRAD),DRAD(IRAD),DSQRAD(IRAD)
      	   ENDIF
C      	write(6,609) IRAD,NRAD(IRAD),DRAD(IRAD),DSQRAD(IRAD)
CHENN<
609	format(2I7,2F14.2)
610	CONTINUE
	WRITE(6,611) DRAD(1)/DMAX,DRAD(1),DMAX
611	FORMAT(' central peak height is',F10.3,
     . '  compared with DMAX: numbers are',2F10.1)
605	DO 620 IRAD=1,NRMAXCENT
		IF(DRAD(IRAD).GT. 0.5*DRAD(1)) IRHALF=2*IRAD
		IF(DRAD(IRAD).LT. FPBOX*DRAD(1)) THEN
			IRTAIL=2*(IRAD-1)
			GO TO 630
		ENDIF
620	CONTINUE
	  WRITE(6,621)IRHALF
621	  FORMAT(' Central peak too diffuse, halfwidth',2I6)
      	  DX1=0.0
          DY1=0.0
	  RETURN
630	IFPBOX=FPBOX*100.0	! percentage
      	WRITE(6,631) IFPBOX,IRHALF,IRTAIL
631	FORMAT(' peak full width, at half height and',I4,'% height',2I6)
C
CHENN>
        write(*,'(/,/,'' IRAD = '',I8,/,/)')IRAD
CHENN<
C
C   check for elongation of peak in any direction
	DO 640 I=1,NRMAX
	DO 640 J=-I,+I
		IF(AVER(I,J).GT. FPBOX*DRAD(1)) NXALONE=2*I
		IF(AVER(J,I).GT. FPBOX*DRAD(1)) NYALONE=2*I
		IF(DRAD(I).LT. FPBOX*DRAD(1)*0.5) GO TO 645
640	CONTINUE
645	CONTINUE
C
	NXP=MAX(IRTAIL,NXALONE)
	NYP=MAX(IRTAIL,NYALONE)
	IF(NXALONE.GT. IRTAIL.OR. NYALONE.GT. IRTAIL) THEN
		WRITE(6,646) NXP,NYP,NXALONE,NYALONE
646		FORMAT(' peaks elongated : raster used ',2I3,5X,2I3)
	ENDIF		
C
C  Now move out radially, starting at radius IRAD [at density FPBOX*DRAD(1)]
C  until the first two independent spots with angular separations between 
C  45 and 135 degrees are found. 
C
	ISPOT=0
	DO 700 IR=IRAD,IAVER
      		ANGSTEP=50.0/IR		  ! search spirally in angular steps
      		IANGSTEP=ANGSTEP/0.5
      		IANGSTEP=MAX0(1,IANGSTEP)
	   DO 720 IANG=-180,180,IANGSTEP  ! minimum step 0.5 degrees
		ANGRAD=0.5*IANG*3.1415926/180.0
		IX=IR*COS(ANGRAD)
		IY=IR*SIN(ANGRAD)
      		IRTEST=MIN0(IR,NRMAX)
      		TESTDENS=DRAD(IRTEST)+7.8*DSQRAD(IRTEST)   ! more than 7.8*sigma
		IF(AVER(IX,IY).GT. FRACT*DRAD(1).OR. AVER(IX,IY).GT. TESTDENS) THEN
C		     pixel found
		  IF(ISPOT.EQ. 0) THEN
C		  		find centre
			WRITE(6,721)IX,IY,IR
721			FORMAT('  first spot found at x,y,rad',3I5)
C				first find peak of density
			IXOLD=IX
			IYOLD=IY
			IXNEW=IX
			IYNEW=IY
			DO 724 IS=1,IRTAIL+IRTAIL/2+3	! may need diagonal plus
				DO 725 INX=-1,1
				DO 725 INY=-1,1
				JTX=IXOLD+INX
				JTY=IYOLD+INY
				IF(JTX.GE. IAVER.OR. IABS(JTY).GE. IAVER) THEN
					WRITE(6,723)ISPOT+1,JTX,JTY
723					FORMAT(/,/,/' peak of spot',I3,' too near edge !!',2I5,/,
     . ' perhaps pattern contains some salt diffraction?',/,
     . '   this should be masked off before AUTOINDEX',/,
     . ' or lattice parameters are too big - could try',
     . ' adjacent pixel averaging ??',/,/,/,/)
					STOP 'statement 723'
				ENDIF
				IF(AVER(JTX,JTY).GT. AVER(IXOLD,IYOLD)) THEN
				  IXNEW=JTX
				  IYNEW=JTY
				ENDIF
725				CONTINUE
				IF(IXNEW.EQ. IXOLD.AND. IYNEW.EQ. IYOLD) THEN
C				   calculate centre of gravity
				  DENSTOT=0.0
      				  IRANGE=IRTAIL/2
      				  IF(IRANGE.GT.2*IRHALF) IRANGE=2*IRHALF
				  DO 727 INX=-IRANGE,IRANGE
				  DO 727 INY=-IRANGE,IRANGE
				  KX=IXNEW+INX
				  KY=IYNEW+INY
				  IF(KX.GE. IAVER.OR. IABS(KY).GE. IAVER) THEN
					WRITE(6,723)ISPOT+1,KX,KY
					STOP 'statement 723'
				  ENDIF
      				  VAL=AMAX1(0.0,AVER(KX,KY))
				  DX1=DX1+VAL*KX
				  DY1=DY1+VAL*KY
727				  DENSTOT=DENSTOT+VAL
      				  IF(DENSTOT.GT. 0.0) THEN
				  	DX1=DX1/DENSTOT
				  	DY1=DY1/DENSTOT
				  	IF(SQRT(DX1**2+DY1**2).LE. FLOAT(IRTAIL)) THEN
						WRITE(6,729)DX1,DY1
729						FORMAT('  Too near centre, DX1,DY1 =',2F10.2)
						GO TO 720
				  	ENDIF
      				  ELSE
      					STOP ' negative density for first lattice peak '
      				  ENDIF
				  ANGLE1=ATAN2(DY1,DX1)
				  WRITE(6,728) DX1,DY1
728				  FORMAT('  first lattice direction - ',
     . 'shortest vector, accurate DX1,DY1',2F10.2)
				  ISPOT=1
				  GO TO 720
				ELSE
				  IXOLD=IXNEW
				  IYOLD=IYNEW
				ENDIF
724			CONTINUE
			WRITE(6,726)
726			FORMAT(/,/,/' Peak of spot 1 not found ',
     . 'within IRTAIL of starting position',/,
     . '   density dribbles away from apparent peak',/,/,
     . ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',/)
			RETURN
		  ELSE
		    IF(ISPOT.EQ. 1) THEN
C		       test to find if too near previous spot
			DX=IX
			DY=IY
			ANGLE2=ATAN2(DY,DX)
			ANGDIFF1=ABS(ANGLE1-ANGLE2)
			ANGDIFF2=3.1415926-ANGDIFF1
			ANGDIFF =AMIN1(ANGDIFF1,ANGDIFF2)
			IF(ANGDIFF.LT. 0.3) GO TO 720	! too near
C			   calculate centre of gravity
			WRITE(6,821)IX,IY,IR
821			FORMAT(' second spot found at x,y,rad',3I5)
C				first find peak of density
			IXOLD=IX
			IYOLD=IY
			IXNEW=IX
			IYNEW=IY
			DO 824 IS=1,IRTAIL+IRTAIL/2+3	! may need diagonal plus
				DO 825 INX=-1,1
				DO 825 INY=-1,1
				JTX=IXOLD+INX
				JTY=IYOLD+INY
				IF(JTX.GE. IAVER.OR. IABS(JTY).GE. IAVER) THEN
					WRITE(6,723)ISPOT+1,JTX,JTY
					STOP 'statement 723'
				ENDIF
				IF(AVER(JTX,JTY).GT. AVER(IXOLD,IYOLD)) THEN
				  IXNEW=JTX
				  IYNEW=JTY
				ENDIF
825				CONTINUE
				IF(IXNEW.EQ. IXOLD.AND. IYNEW.EQ. IYOLD) THEN
C				   calculate centre of gravity
				  DENSTOT=0.0
				  DO 827 INX=-IRANGE,IRANGE
				  DO 827 INY=-IRANGE,IRANGE
				  KX=IXNEW+INX
				  KY=IYNEW+INY
				  IF(KX.GE. IAVER.OR. IABS(KY).GE. IAVER) THEN
					WRITE(6,723)ISPOT+1,KX,KY
					STOP 'statement 723'
				  ENDIF
C	write(6,'(''kx,ky,ixnew,iynew,inx,iny,irtail'',7i5)')
C     *  kx,ky,ixnew,iynew,inx,iny,irtail
      				  VAL=AMAX1(0.0,AVER(KX,KY))
				  DX2=DX2+VAL*KX
				  DY2=DY2+VAL*KY
827				  DENSTOT=DENSTOT+VAL

      				  IF(DENSTOT.GT. 0.0) THEN
				  	DX2=DX2/DENSTOT
				  	DY2=DY2/DENSTOT
				  	WRITE(6,828) DX2,DY2
828				  	FORMAT(' second lattice direction - ',
     . 'shortest vector, accurate DX2,DY2',2F10.2)
				  	GO TO 820
      				  ELSE
      					STOP ' negative density for second lattice peak '
      				  ENDIF
				ELSE
				  IXOLD=IXNEW
				  IYOLD=IYNEW
				ENDIF
824			CONTINUE
			WRITE(6,826)
826			FORMAT(/,/,/,' Peak of candidate spot 2 not found ',
     . 'within IRTAIL of starting position',/,/,
     . ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',/,/,/,/)
			RETURN
820			ANGLE2=ATAN2(DY2,DX2)
			ANGDIFF1= ABS(ANGLE1-ANGLE2)
			ANGDIFF2= 3.1415926-ANGDIFF1
			ANGDIFF = AMIN1(ANGDIFF1,ANGDIFF2)
			ANGTEST=1.0	! radians
			RTG=180.0/3.1415926
			IF(ANGDIFF.LT. ANGTEST) THEN
				WRITE(6,822) ANGTEST*RTG,ANGLE1*RTG,
     . ANGLE2*RTG,ANGDIFF1*RTG,ANGDIFF2*RTG
822				FORMAT('  This vector rejected - angle between',
     . ' two directions smaller than',F10.2,/
     . '  Angle1, Angle2, Deltas',4F10.2)
				DX2=0.0
				DY2=0.0
				GO TO 720
			ENDIF
			IF(ANGDIFF2.LT. ANGDIFF1) THEN
				DX2=-DX2
				DY2=-DY2
				ANGLE2=ATAN2(DY2,DX2)
			ENDIF
			GAMMAST=ANGDIFF*180.0/3.1415926
			WRITE(6,840) DX2,DY2,GAMMAST
840			FORMAT(' second lattice direction',
     . ' found, DX2,DY2,GAMMAST   ',3F10.2)
			ISPOT=2
			GO TO 730
		    ENDIF
		  ENDIF
		ENDIF
720	   CONTINUE
700	CONTINUE
      	FPBOX=FPBOX-0.02
      	FRACT=FRACT-0.02
      	WRITE(6,702) FPBOX,FRACT
702	FORMAT(' Two independent spots not found,',
     . ' FPBOX, FRACT reduced to',2F8.3)
      	IF(FPBOX. GT. 0.05) GO TO 605
	WRITE(6,701) ISPOT
701	FORMAT(' Two independent spots not found,',
     . ' number found was',I5,/,
     . ' Try examining the averaged map!')
      	DX1=0.0
      	DY1=0.0
	RETURN
730	CONTINUE
C
C  try for improved accuracy in the case of closely spaced spots
CHENN>
C      	IF(DX1**2+DY1**2. LT. (0.45*FLOAT(IAVER))**2. AND. 
C     . DX2**2+DY2**2. LT. (0.45*FLOAT(IAVER))**2) 
C     . CALL EXTEND(AVER,IRANGE,DX1,DY1,DX2,DY2)
      	IF(DX1**2+DY1**2. LT. (0.45*FLOAT(IAVER))**2. AND. 
     . DX2**2+DY2**2. LT. (0.45*FLOAT(IAVER))**2) 
     . CALL EXTEND(AVER,IRANGE,DX1,DY1,DX2,DY2,IMAXCYC)
CHENN<
C
C  Now work out largest possible background rasters for PICKYCOR
	NTYPE=2
	IF(GAMMAST. LT. 75.0) NTYPE=0
	IF(GAMMAST. GT. 135.0) NTYPE=1
C  Note that NTYPE will never be 1 with this indexing scheme
	X1=ABS(DX1)
	Y1=ABS(DY1)
	X2=ABS(DX2)
	Y2=ABS(DY2)
	X3=0
	Y3=0
	NXB=AMAX1(X1,X2)-NXP-1
	NYB=AMAX1(Y1,Y2)-NYP-1
	IF(NTYPE. EQ. 0) THEN
		X3=ABS(DX1-DX2)
		Y3=ABS(DY1-DY2)
		NXB=AMAX1(X1,X2,X3)-NXP-1
		NYB=AMAX1(Y1,Y2,Y3)-NYP-1
	ENDIF
	IF(NTYPE. EQ. 1) THEN
		X3=ABS(DX1+DX2)
		Y3=ABS(DY1+DY2)
		NXB=AMAX1(X1,X2,X3)-NXP-1
		NYB=AMAX1(Y1,Y2,Y3)-NYP-1
	ENDIF
	WRITE(6,751) NXP,NYP,NXB,NYB,NTYPE
751	FORMAT(' Peak and background rasters determined as',4I5,I8,/,/)
	RETURN
	END
C###############################################################################
C
	SUBROUTINE PLOTRAST(TITLE,DX1,DY1,DX2,DY2,NXP,NYP,NXB,NYB,NTYPE)
	DIMENSION TITLE(1),TEXT(20)
CTSH++
	CHARACTER TMPTEXT*80
	EQUIVALENCE (TMPTEXT,TEXT)
CTSH--
C
	WRITE(6,8000)
	IF(ABS(DX1+DY1+DX2+DY2). LT. 0.00001) RETURN
      SCALE=1.000
C
9993  WRITE(6,8002) SCALE
8002  FORMAT(' scale for plot',F10.1)
      XH=DX1*SCALE
      XK=DX2*SCALE
      YH=DY1*SCALE
      YK=DY2*SCALE
	XPBOX=NXP/2.0*SCALE
	YPBOX=NYP/2.0*SCALE
	XBBOX=NXB/2.0*SCALE
	YBBOX=NYB/2.0*SCALE
      XR1=XH+XK
      YR1=YH+YK
      XR2=-XH+XK
      YR2=-YH+YK
      IF(XR1. GT. 150.0. OR. XR1. LT. -150.0)GO TO 9995
      IF(XR2. GT. 150.0. OR. XR2. LT. -150.0)GO TO 9995
      IF(YR1. GT. 150.0. OR. YR1. LT. -150.0)GO TO 9995
      IF(YR2. GT. 150.0. OR. YR2. LT. -150.0)GO TO 9995
      GO TO 9994
9995  SCALE=SCALE/2.0
      GO TO 9993
C
C     TYPE OF BACKGROUNDS NTYPE=0
9994  IF(NTYPE. EQ. 0) THEN
      	XB1=(XH+XK)/3
      	YB1=(YH+YK)/3
      	XB2=(2*XH-XK)/3
      	YB2=(2*YH-YK)/3
      	XB3=(XH-2*XK)/3
      	YB3=(YH-2*YK)/3
      ENDIF
C     BACKGROUND POSITIONS NTYPE=1
      IF(NTYPE. EQ. 1) THEN
      	XB1=(2*XH+XK)/3
      	YB1=(2*YH+YK)/3
      	XB2=(XH+2*XK)/3
      	YB2=(YH+2*YK)/3
      	XB3=(XH-XK)/3
      	YB3=(YH-YK)/3
      ENDIF
C     BACKGROUND POSITIONS NTYPE=2
      IF(NTYPE. EQ. 2) THEN
      	XB1=(XH+XK)/2
      	YB1=(YH+YK)/2
      	XB2=(XH-XK)/2
      	YB2=(YH-YK)/2
      	XB3=0.0
      	YB3=0.0
      ENDIF
C
      PLTSIZ = 300  	! old plots were nominally 300 mm
      FONTSIZE = 4  	! set fontsize to 4mm
      CALL P2K_OUTFILE('PLOTOUT.PS',10)
      CALL P2K_HOME
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
      CALL P2K_GRID(0.5*PLTSIZ,0.5*PLTSIZ,1.0)
      CALL P2K_ORIGIN(0. ,-0.125*PLTSIZ,0. )
      CALL P2K_COLOUR(0)
      CALL P2K_MOVE(XPBOX,YPBOX,0. )
      CALL P2K_DRAW(-XPBOX,YPBOX,0. )
      CALL P2K_DRAW(-XPBOX,-YPBOX,0. )
      CALL P2K_DRAW(XPBOX,-YPBOX,0. )
      CALL P2K_DRAW(XPBOX,YPBOX,0. )
C
      CALL P2K_MOVE(-125. ,-150. ,0. )
CTSH      WRITE(TEXT) (TITLE(J),J=1,20,511)
CTSH++
      WRITE(TMPTEXT,511) (TITLE(J),J=1,20)
CTSH--
511   FORMAT(20A4)
      CALL P2K_STRING(TEXT,80,0. )
C
      CALL P2K_MOVE(-125. ,-160. ,0. )
CTSH      WRITE(TEXT,9996)NSTEP,DX1,DY1,DX2,DY2
CTSH++
      WRITE(TMPTEXT(1:55),9996)NSTEP,DX1,DY1,DX2,DY2
CTSH--
9996  FORMAT(' LATTICE VECTOR DATA',I3,4F8.2)
      CALL P2K_STRING(TEXT,55,0. )
C
      CALL P2K_MOVE(-125. ,-170. ,0. )
CTSH      WRITE(TEXT,9988)NTYPE
CTSH++
      WRITE(TMPTEXT(1:40),9988)NTYPE
CTSH--
9988  FORMAT(' TYPE OF BACKGROUND POSITIONS; NTYPE =',I2)
      CALL P2K_STRING(TEXT,40,0. )
C
      CALL P2K_MOVE(-125. ,-180. ,0. )
CTSH      WRITE(TEXT,9989) NXP,NYP,NXB,NYB
CTSH++
      WRITE(TMPTEXT(1:46),9989) NXP,NYP,NXB,NYB
CTSH--
9989  FORMAT(' PEAK AND BACKGROUND RASTER SIZES ',4I3)
      CALL P2K_STRING(TEXT,46,0. )
C
      CALL P2K_MOVE(-150. ,0. ,0. )
      CALL P2K_DRAW(150. ,0. ,0. )
      CALL P2K_MOVE(0. ,-150. ,0. )
      CALL P2K_DRAW(0. ,150. ,0. )
      CALL P2K_MOVE(0. ,0. ,0. )
C
      CALL P2K_DRAW(XH,YH,0. )
      CALL P2K_MOVE(XH,YH,0. )
CTSH      WRITE(TEXT,9991)
CTSH++
      WRITE(TMPTEXT(1:1),9991)
CTSH--
9991  FORMAT('H')
      CALL P2K_CSTRING(TEXT,1,0. )
      CALL P2K_MOVE(0. ,0. ,0. )
      CALL P2K_DRAW(-XH,-YH,0. )
      CALL P2K_MOVE(0. ,0. ,0. )
      CALL P2K_DRAW(XK,YK,0. )
      CALL P2K_MOVE(XK,YK,0. )
CTSH      WRITE(TEXT,9992)
CTSH++
      WRITE(TMPTEXT(1:1),9992)
CTSH--
9992  FORMAT('K')
      CALL P2K_CSTRING(TEXT,1,0. )
      CALL P2K_MOVE(0. ,0. ,0. )
      CALL P2K_DRAW(-XK,-YK,0. )
      CALL P2K_MOVE(XR1,YR1,0. )
      CALL P2K_DRAW(XR2,YR2,0. )
      CALL P2K_DRAW(-XR1,-YR1,0. )
      CALL P2K_DRAW(-XR2,-YR2,0. )
      CALL P2K_DRAW(XR1,YR1,0. )
C
      XP1=XB1-XBBOX
      XQ1=XB1+XBBOX
      YP1=YB1-YBBOX
      YQ1=YB1+YBBOX
      CALL P2K_MOVE(XP1,YP1,0. )
      CALL P2K_DRAW(XQ1,YP1,0. )
      CALL P2K_DRAW(XQ1,YQ1,0. )
      CALL P2K_DRAW(XP1,YQ1,0. )
      CALL P2K_DRAW(XP1,YP1,0. )
      CALL P2K_MOVE(-XP1,-YP1,0. )
      CALL P2K_DRAW(-XQ1,-YP1,0. )
      CALL P2K_DRAW(-XQ1,-YQ1,0. )
      CALL P2K_DRAW(-XP1,-YQ1,0. )
      CALL P2K_DRAW(-XP1,-YP1,0. )
C
      XP2=XB2-XBBOX
      XQ2=XB2+XBBOX
      YP2=YB2-YBBOX
      YQ2=YB2+YBBOX
      CALL P2K_MOVE(XP2,YP2,0. )
      CALL P2K_DRAW(XQ2,YP2,0. )
      CALL P2K_DRAW(XQ2,YQ2,0. )
      CALL P2K_DRAW(XP2,YQ2,0. )
      CALL P2K_DRAW(XP2,YP2,0. )
      CALL P2K_MOVE(-XP2,-YP2,0. )
      CALL P2K_DRAW(-XQ2,-YP2,0. )
      CALL P2K_DRAW(-XQ2,-YQ2,0. )
      CALL P2K_DRAW(-XP2,-YQ2,0. )
      CALL P2K_DRAW(-XP2,-YP2,0. )
C
      IF(XB3. EQ. 0.0)GO TO 10000
      XP3=XB3-XBBOX
      XQ3=XB3+XBBOX
      YP3=YB3-YBBOX
      YQ3=YB3+YBBOX
      CALL P2K_MOVE(XP3,YP3,0. )
      CALL P2K_DRAW(XQ3,YP3,0. )
      CALL P2K_DRAW(XQ3,YQ3,0. )
      CALL P2K_DRAW(XP3,YQ3,0. )
      CALL P2K_DRAW(XP3,YP3,0. )
      CALL P2K_MOVE(-XP3,-YP3,0. )
      CALL P2K_DRAW(-XQ3,-YP3,0. )
      CALL P2K_DRAW(-XQ3,-YQ3,0. )
      CALL P2K_DRAW(-XP3,-YQ3,0. )
      CALL P2K_DRAW(-XP3,-YP3,0. )
C
10000 WRITE(6,8001)
      CALL P2K_PAGE
8000	FORMAT(' Entering PLOTRAST')
8001	FORMAT('  exiting PLOTRAST')
	RETURN
	END
C*******************************************************************************
	SUBROUTINE REINDEX(CX,CY,DX1,DY1,DX2,DY2,ARRAY,NX,NY)
	PARAMETER (NTEST=60)
	PARAMETER (IDUMMY=2*NTEST-12)
	DIMENSION ARRAY(1), IREF(2,NTEST)
C  indices of spots used in indexing test are given here. 
	DATA IREF/1,2,2,4,4,3,1,5,5,2,6,1,IDUMMY*0/
C
C  This routine is meant to carry out very simple reindexing to simulate what
C  can also be done interactively at an early stage in indexing using a 
C  graphics system. It cannot replace the index checking at the later merging
C  step which requires accurately integrated intensities. 
C
C  This routine is specific for purple membrane and tests the low resolution
C  spots for the standard indexing convention (e. g. 4,3>3,4) These are the least 
C  sensitive to tilt. 
C
	WRITE(6,10)
10	FORMAT(/,' ENTERING REINDEX')
	DHK=0. 
	DKH=0. 
	NHK=0
	NKH=0
	DO 100 I=1,NTEST
	  JH=IREF(1,I)
	  JK=IREF(2,I)
	  DO 80 J=-1,1,2
		JHF=JH*J		! Friedel relation
		JKF=JK*J
		DO 70 K=1,3
		  IF(K. EQ. 1) THEN	! p3 relationship
			IH=JHF		! H,K
			IK=JKF
		  ENDIF
		  IF(K. EQ. 2) THEN
			IH=JKF		! K,-(H+K)
			IK=-(JHF+JKF)
		  ENDIF
		  IF(K. EQ. 3) THEN
			IH=-(JHF+JKF)	! -(H+K),H
			IK=JHF
		  ENDIF
		  IX1=CX+DX1*IH+DX2*IK+NX/2.0+0.5
		  IY1=CY+DY1*IH+DY2*IK+NY/2.0+0.5
		  IX2=CX+DX1*IK+DX2*IH+NX/2.0+0.5
		  IY2=CY+DY1*IK+DY2*IH+NY/2.0+0.5
		  INDEX1=IX1+NX*(IY1-1)
		  INDEX2=IX2+NX*(IY2-1)
		AINTHK=0. 
		AINTKH=0. 
			DO 60 IXN=-2,2	! Integrate 5x5 raster
			DO 60 IYN=-2,2
			INDEXN1=INDEX1+IXN+IYN*NX
			INDEXN2=INDEX2+IXN+IYN*NX
			AINTHK=AINTHK+ARRAY(INDEXN1)
60			AINTKH=AINTKH+ARRAY(INDEXN2)
		  DHK=DHK+AINTHK
		  DKH=DKH+AINTKH
		  WRITE(6,*)IH,IK,AINTHK,AINTKH
		  IF(AINTHK. GT. AINTKH) THEN
			NHK=NHK+1
		  ELSE
			NKH=NKH+1
		  ENDIF
70		CONTINUE
80	  CONTINUE
100	CONTINUE
	WRITE(6,101) DHK,DKH,NHK,NKH
101	FORMAT(' 5x5 raster intensity comparison',2F10.0,/,
     . '               number comparison',I9,1X,I9)
	IF(DHK. GT. DKH. AND. NHK. GT. NKH) THEN
		WRITE(6,102)
102		FORMAT(' indexing correct on both tests',/)
		RETURN
	ELSE
		IF(NHK. GT. NKH) THEN
			WRITE(6,103)
103			FORMAT(' indexing not changed,',
     . ' most HK tests correct',/)
			RETURN
		ENDIF
		WRITE(6,104)
104		FORMAT(' indexing reversed, HK->KH',/)
		DXS=DX1
		DYS=DY1
		DX1=DX2
		DY1=DY2
		DX2=DXS
		DY2=DYS
	ENDIF
	RETURN
	END
C*******************************************************************************
CHENN>
C	SUBROUTINE EXTEND(AVER,IRANGE,DX1,DY1,DX2,DY2)
	SUBROUTINE EXTEND(AVER,IRANGE,DX1,DY1,DX2,DY2,IMAXCYC)
C	PARAMETER (IAVER=100)
C	DIMENSION AVER(-IAVER:IAVER,-IAVER:IAVER), AP(-10:10,-10:10)
	PARAMETER (IAVER=300)
	PARAMETER (IAPMAX=30)
	DIMENSION AVER(-IAVER:IAVER,-IAVER:IAVER)
        DIMENSION AP(-IAPMAX:IAPMAX,-IAPMAX:IAPMAX)
CHENN<
C
C  refines lattice parameters more accurately using higher resolution spots
C  
CHENN>
C      do i = -40,40
C        write(*,'(40F6.1)')(AVER(i,j),j=-20,19)
C      enddo
C      DO 300 ICYC=2,6
      DO 300 ICYC=2,IMAXCYC
CHENN<
      	IMULT1=(IAVER-IRANGE)/SQRT(DX1**2+DY1**2)
      	IMULT1=MIN0(ICYC,IMULT1)
      	IMULT2=(IAVER-IRANGE)/SQRT(DX2**2+DY2**2)
      	IMULT2=MIN0(ICYC,IMULT2)
      	DXM1=DX1*IMULT1
      	DYM1=DY1*IMULT1
      	DXM2=DX2*IMULT2
      	DYM2=DY2*IMULT2
CHENN>
        write(*,'('' DX1,DY1   = '',2F12.3)')DX1,DY1
        write(*,'('' DX2,DY2   = '',2F12.3)')DX2,DY2
        write(*,'('' IMULT1,IMULT2 = '',2I12)')IMULT1,IMULT2
        write(*,'('' DXM1,DYM1 = '',2F12.3)')DXM1,DYM1
        write(*,'('' DXM2,DYM2 = '',2F12.3)')DXM2,DYM2
CHENN<
C
C  for first axis
      	DXT=0.0
      	DYT=0.0
      	DENSTOT=0.0
      	DO 120 IX=-IRANGE,IRANGE
      	DO 120 IY=-IRANGE,IRANGE
      		KX=DXM1+SIGN(0.5,DXM1)+IX
      		KY=DYM1+SIGN(0.5,DYM1)+IY
      			IF(IABS(KX). GE. IAVER. OR. IABS(KY). GE. IAVER) THEN
			WRITE(6,130) KX,KY,IX,IY
130			FORMAT(' attempt to look beyond averaged area',4I5)
			STOP ' subroutine EXTEND'
			ENDIF
      		VAL=AMAX1(0.0,AVER(KX,KY))
      		DXT=DXT+FLOAT(KX)*VAL
      		DYT=DYT+FLOAT(KY)*VAL
      		AP(IX,IY)=VAL
CHENN>
           write(*,'(2I10,F12.3)')KX,KY,AVER(KX,KY)
CHENN<
120   	DENSTOT=DENSTOT+VAL
      	IF(DENSTOT. GT. 0.0) THEN
      		DX1=(DXT/DENSTOT)/FLOAT(IMULT1)
      		DY1=(DYT/DENSTOT)/FLOAT(IMULT1)
      		WRITE(6,121)
121		FORMAT(' raster for first axis')
      		DO 125 J=-IRANGE,IRANGE
125		WRITE(6,128) (AP(I,J),I=-IRANGE,IRANGE)
CHENN>
C128		FORMAT(10F5.1)
128		FORMAT(10F6.1)
CHENN<
      	ENDIF
C
C  for second axis
      	DXT=0.0
      	DYT=0.0
      	DENSTOT=0.0
      	DO 220 IX=-IRANGE,IRANGE
      	DO 220 IY=-IRANGE,IRANGE
      		KX=DXM2+SIGN(0.5,DXM2)+IX
      		KY=DYM2+SIGN(0.5,DYM2)+IY
      			IF(IABS(KX). GE. IAVER. OR. IABS(KY). GE. IAVER) THEN
			WRITE(6,130) KX,KY,IX,IY
			STOP ' subroutine EXTEND'
			ENDIF
      		VAL=AMAX1(0.0,AVER(KX,KY))
      		DXT=DXT+FLOAT(KX)*VAL
      		DYT=DYT+FLOAT(KY)*VAL
      		AP(IX,IY)=VAL
220   	DENSTOT=DENSTOT+VAL
      	IF(DENSTOT. GT. 0.0) THEN
      		DX2=(DXT/DENSTOT)/FLOAT(IMULT2)
      		DY2=(DYT/DENSTOT)/FLOAT(IMULT2)
      		WRITE(6,221) 
221   		FORMAT(' raster for second axis')
      		DO 225 J=-IRANGE,IRANGE
225		WRITE(6,128) (AP(I,J),I=-IRANGE,IRANGE)
      	ENDIF
C
CHENN>
C      	WRITE(6,60) IMULT1,IMULT2,J,(2*IRANGE+1),DX1,DY1,DX2,DY2
        WRITE(6,60) IMULT1,IMULT2,ICYC,(2*IRANGE+1),DX1,DY1,DX2,DY2
CHENN<
60    	FORMAT(' More accurate lattice parameters refined using ',
     . I3,' order on first axis and ',I3,' order on second axis',/,
     . ' CYCLE',I2,' with a box size of',I3,
     . '       new parameters are',4F8.2,/)
300   CONTINUE
      RETURN
      END
