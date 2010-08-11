C*CTFFIND.FOR************************************************************
C									*
C	This program will attempt to determine the defocus (not yet	*
C	astigmatism) of any general image whether or not it contains	*
C	crystalline areas.  The program will also apply a CTF 		*
C	correction either by multiplication or division (accompanied 	*
C	by a Wiener filter).  Both amplitude and phase contrast with	*
C	astigmatism can be applied.					*
C									*
C	  The input and output are Fourier transforms			*
C									*
C	Card input - four cards only					*
C									*
C	1. IFIND,IAPPLY,IMULT,CTFMINMOD,WAMP - function control		*
C						(e.g.  F T T 0.2 0.07)	*
C	2. DFMID1 DFMID2 ANGAST	- defocus estimate			*
C	3. DSTEP XMAG CS KV	- image parameters			*
C	4. RESMIN RESMAX	- resolution limits to explore		*
C									*
C	Logical I/O assignments are:					*
C									*
C	IN			input image transform			*
C	OUT			output of ctf-corrected			*
C									*
C	Version   1.00	9.7.94		RH	original program	*
C	Version   1.01	5.3.95		RH	debug O/P title		*
C	Version   1.02 12.7.96		RH	use RESMIN		*
C	Version   2.00 28.5.99		RH	add IMULT,CTFMINMOD,WAMP*
C		       08.6.04		JMS     increased ARRMXSIZ	*
C									*
C-----------------------------------------------------------------------*
C									*
C IFIND     - CARRY OUT CTF DETERMINATION AND REFINEMENT, USING 	*
C	       STARTING VALUES IF GIVEN - tries from half to double the *
C	       given defocus using the given astigmatism as a guide to 	*
C	       the possible range and therefore rapidity of angular 	*
C	       variation						*
C IAPPLY    - APPLY CTF, USING INPUT OR REFINED PARAMETERS		*
C IMULT     - IF 'T' multiply by CTF, if 'F' divide by CTF		*
C CTFMINMOD - Minimum modulus of CTF to be used in denominator if 	*
C	       IMULT='F'.  This is similar to the normal Wiener filter	*
C WAMP      - Amplitude Contrast: e.g. 0.07(120kV), 0.04(200kV), etc	*
C ISIZE     - SIZE OF DENSITOMETERED ARRAY (E.G. 2000, 6000)		*
C DFMID1    - DEFOCUS LEVEL (UNDERFOCUS +VE). IF DFMID2=DFMID1, IMAGE	*
C DFMID2    -  IS NON-ASTIGMATIC. OTHERWISE AMOUNT OF DEFOCUS IN TWO	*
C              ORTHOGONAL DIRECTIONS, DFMID1 BEING DEFOCUS IN DIRECTION	*
C              ANGAST (DEG) RELATIVE TO X AND Y OF THE FOURIER TRANSFORM*
C DSTEP     - DENSITOMETER STEPSIZE IN MICRONS				*
C XMAG      - PRECISE MAGNIFICATION					*
C CS        - SPHERICAL ABERRATION IN MM				*
C KV        - E.M. ACCELERATING VOLTAGE					*
C RESMIN    - RESOLUTION LIMITS TO BE USED IN ANGSTROMS			*
C RESMAX    -    "  "							*
C									*
C************************************************************************
C
C      	PARAMETER (ARRMXSIZ=256000000)
      	PARAMETER (IARRMXSIZ=410000000)
      	COMMON//NX,NY,NZ
        COMMON/FTBUF/MAXSIZ,ARRAY(IARRMXSIZ)
	DIMENSION TITLE(20),NXYZR(3),MXYZR(3)
      	DIMENSION RSQINT(10000,30),NRSQ(10000,30)
	CHARACTER DAT*24
      	REAL KV
	EQUIVALENCE (NX,NXYZR)
      	LOGICAL IFIND,IAPPLY,IMULT,LARGE
CTSH++
	CHARACTER*80 TMPTITLE
	EQUIVALENCE (TMPTITLE,TITLE)
CTSH--
C      	 DATA RSQINT/300000*0./,NRSQ/300000*0/
      	TWOPI = 2.0 * 3.1415926
      	RDEG = 360.0/TWOPI
      	DO 5 I=1,10000
      	DO 5 J=1,30
	NRSQ(I,J)=0
5	RSQINT(I,J)=0.
C
	WRITE(6,10)
10	FORMAT(//' CTFFINDA: CTF determination',
     .		 ' - V2.00 (28.5.99)',//)
C
      READ(5,*) IFIND,IAPPLY,IMULT,CTFMINMOD,WAMP
      	IF(CTFMINMOD.LT.0.0) CTFMINMOD=-CTFMINMOD
      READ(5,*) DFMID1,DFMID2,ANGAST
      IF(DFMID1.EQ.DFMID2) DFMID2=DFMID2+1.0
      READ(5,*) DSTEP,XMAG,CS,KV
      READ(5,*) RESMIN,RESMAX
      IF(RESMAX.GT.RESMIN) THEN
      	REST=RESMIN
      	RESMIN=RESMAX
      	RESMAX=REST
      ENDIF
      WRITE(6,11) IFIND,IAPPLY,IMULT,CTFMINMOD,WAMP,
     .		DFMID1,DFMID2,ANGAST,DSTEP,XMAG,CS,KV,RESMIN,RESMAX
11    FORMAT(   ' IFIND, APPLY, IMULT ..... =',L1,5X,L1,5X,L1/
     .		' CTFMINMOD,WAMP .......... =',F7.3,F8.3/
     .		' DFMID1,DFMID2,ANGAST .... =',2F8.0,F7.1/
     .		' DSTEP ................... =',F8.1/
     .		' XMAG .................... =',F8.0/
     .		' CS, KV .................. =',F8.2,F7.0/
     .		' RESMIN,RESMAX ........... =',F8.2,F7.2/)
C
	CALL IMOPEN(1,'IN','RO')
	IF(IAPPLY) CALL IMOPEN(2,'OUT','NEW')
	CALL FDATE(DAT)
C
C   Read input header
C
	CALL IRDHDR(1,NXYZR,MXYZR,MODE,DMIN,DMAX,DMEAN)
	IF (MODE .LT. 3) STOP ' Input must be a transform'
	IF(IAPPLY) CALL ITRHDR(2,1)
C
C   Here for transform parameters plus new title line
	MAXSIZ = ARRMXSIZ
      	NYM1 =NY-1
	NY2 = NY/2
	NY21 = NY2 + 1
	NXR = (NX - 1)*2
      	NX2 = NXR/2
      	NX21 = NXR/2 + 1
      	ISIZE = NXR
	NXP2 = NXR + 2
	IF (NXP2*NY .GT. MAXSIZ) LARGE = .TRUE.
	IF (LARGE) STOP '*** TRANSFORM TOO BIG FOR STORAGE ***'
      ANGAST=ANGAST*TWOPI/360.0
      CS=CS*(10.0**7.0)				! Angstroms
      KV=KV*1000.0				! Volts
      WL=12.3/SQRT(KV+KV**2/(10.0**6.0))	! Angstroms
      WRITE(6,18)WL
18    FORMAT(' WAVELENGTH (ANGSTROMS)',F10.4)
      STEPR=DSTEP*(10.0**4.0)/XMAG
      THETATR=WL/(STEPR*ISIZE)
C  THETATR IS DIFFRACTION ANGLE OF POINT (0,1) IN TRANSFORM (IN RADIANS)
C
C
C  Input of transform
      	INDEX = NXP2*NY2 + 1
      	DO 20 IY = 1,NY
      		IF (IY .EQ. NY21) INDEX = 1   ! array origin at 1,1 
      		CALL IRDLIN(1,ARRAY(INDEX),*999)
      		INDEX = INDEX + NXP2
20	CONTINUE
      	WRITE(6,21)
21	FORMAT(' TRANSFORM READ IN - OK')
      	CALL IMCLOSE(1)
C
C##############################################################################
C
      IF(IFIND) THEN
C
C      Try to find the CTF by a right-every-time method
C      Create a table of mean intensity versus RSQ, ANGSPT
C
      	IRADMAX = WL/(RESMAX*THETATR)	! upper resolution limit
      	IRADMIN = WL/(RESMIN*THETATR)	! lower resolution limit
      	IDELRAD = 0.5 * (IRADMAX**2 - (IRADMAX-1)**2)
      	IDELPHI = 6.0*4000.0/ABS(DFMID1-DFMID2)
C          4000 Angstrom astigmatism => 6 deg angular steps and pro-rata
      	IF(IDELPHI.GT.180.0) IDELPHI=180.0
      	IF(IDELPHI.LT.6.0) IDELPHI=6.0
      	NSLOTS = IRADMAX**2/IDELRAD
      	NSLOTSPHI = (180.0/IDELPHI) + 0.5
      	WRITE(6,199) IRADMAX,IDELRAD,NSLOTS, IDELPHI,NSLOTSPHI
199	FORMAT(' IRADMAX,IDELRAD,NSLOTS, IDELPHI,NSLOTSPHI ',5I6)
      	DO 200 IY = 1,NY
      		IF(IY.LT.NY21) THEN
      			TY = IY-1
      		ELSE
      			TY = IY-1-NY
      		ENDIF
      		INDY = (IY-1)*NXP2
      	  DO 200 IX = 1,NX
      	   	TX = IX-1
      	   	INDEX = 1 + INDY + (IX-1)*2
      	   	RADSQ = TX**2+TY**2
      		IF(RADSQ.LT.FLOAT(IRADMAX**2).AND.RADSQ.GT.FLOAT(IRADMIN**2))
     .									 THEN
      			PHI=ATAN2(TY,TX)
      			PHI=PHI*RDEG
      			IF(PHI.LT.0.) PHI=PHI+180.0
      			IPHISLOT = 1 + PHI/IDELPHI
C      			IF(IPHISLOT.LT.1.OR.
C     .			 IPHISLOT.GT.NSLOTSPHI) WRITE(6,198)PHI
C198			FORMAT(' PHI WAS',F10.1)
      			ISLOT = 1 + RADSQ/IDELRAD
      			AINT = SQRT(ARRAY(INDEX)**2+ARRAY(INDEX+1)**2)
      			RSQINT(ISLOT,IPHISLOT) = RSQINT(ISLOT,IPHISLOT) + AINT
      			NRSQ(ISLOT,IPHISLOT) = NRSQ(ISLOT,IPHISLOT) + 1
      		ENDIF
200	CONTINUE
      	WRITE(6,252)
252	FORMAT(' ISLOT RESOLUTION ISLOTPHI ANGSPT AVINT NUMBER')
      	DO 250 J=1,10000      	
      	   DO 250 K=1,NSLOTSPHI
      	   IF(NRSQ(J,K).NE.0) THEN
      		RSQINT(J,K) = RSQINT(J,K)/NRSQ(J,K)
      		RES = WL/(THETATR*SQRT((J-0.5)*IDELRAD))
      		ANGSPT = (K-0.5)*IDELPHI
      		WRITE(6,251) J,RES,K,ANGSPT,RSQINT(J,K),NRSQ(J,K)
251		FORMAT(I5,F8.1,I7,F6.1,F14.1,I8)
      	   ENDIF
250   	CONTINUE
C  Evaluate the average intensity difference between the top half of the 
C   maxima and the bottom half of the minima of the CTF as a function of 
C   defocus and astigmatism and select the position for the biggest difference
C
      BIGDIFF = 0.0
      BESTFOCUS = -99999.0
      DSTART = 0.25*(DFMID1+DFMID2)
      DFINSH = (DFMID1+DFMID2)
      DFSTEP=50.0		! Try first in small steps 
      WRITE(6,255)DSTART,DFINSH,DFSTEP
255   FORMAT(/' Defocus range tested from',F8.0,' to',F8.0,
     .		' in steps of',F5.0)
      DRANGE = DFINSH-DSTART
      IDRANGE=DRANGE/50.0
      DO 270 I=1,IDRANGE+1
      	DEFOCUS = DSTART+(I-1)*DFSTEP
      	AVMAX=0.
      	NMAX=0
      	AVMIN=0.
      	NMIN=0
      	DO 265 J=1,NSLOTS
      	   RAD = SQRT((J-0.5)*IDELRAD)
      	   ANGLE=RAD*THETATR
      	   C1OLD=C1
      	   C2OLD=C2
      	   C1=TWOPI*ANGLE*ANGLE/(2.0*WL)
      	   C2=-C1*CS*ANGLE*ANGLE/2.0
      	   DO 265 K=1,NSLOTSPHI
      	    IF(NRSQ(J,K).NE.0) THEN
      		 ANGSPT = (K-0.5)*IDELPHI
      		 ANGDIF=ANGSPT-ANGAST
      		 CCOS=COS(2.0*ANGDIF)
C  WHEN ASTIGMATISM IS INCLUDED 0.5*(DFMID1+DFMID2+CCOS*(DFMID1-DFMID2))
      		DF=DEFOCUS
      		CHI=C1*DF+C2
      		CHIDELTA=(C1-C1OLD)*DF+(C2-C2OLD)
      		IF(J.GT.1.AND.ABS(CHIDELTA).GT.(130.0/RDEG)) THEN
      			WRITE(6,264)DEFOCUS
264			FORMAT(' CTF gradient too steep for sampling at',
     .				F8.0,' defocus')
      			GO TO 271
      		ENDIF
      		CNTRST=(SIN(CHI))**2
      		IF(CNTRST.GT.0.8) THEN
      			AVMAX=AVMAX+RSQINT(J,K)
      			NMAX=NMAX+1
      		ENDIF
      		IF(CNTRST.LT.0.2) THEN
      			AVMIN=AVMIN+RSQINT(J,K)
      			NMIN=NMIN+1
      		ENDIF
      	    ENDIF
265   	CONTINUE
      	AVMAX=AVMAX/NMAX
      	AVMIN=AVMIN/NMIN
      	IF((AVMAX-AVMIN).GT.BIGDIFF.AND.
     .		(IABS(NMAX-NMIN).LT.(NMAX+NMIN)/5)) THEN
      		BIGDIFF = AVMAX-AVMIN
      		BESTFOCUS = DEFOCUS
      		WRITE(6,266) BIGDIFF,DEFOCUS,AVMAX,NMAX,AVMIN,NMIN
266		FORMAT(' Better fit',F6.1,' at DF,AVMAX(N),AVMIN(N)',
     .			F8.0,F6.1,I6,F6.1,I6)
      	ELSE
      		WRITE(6,267) AVMAX-AVMIN,DEFOCUS,AVMAX,NMAX,AVMIN,NMIN
267		FORMAT(11X,F6.1,24X,F8.0,F6.1,I6,F6.1,I6)
      	ENDIF
270   CONTINUE
271   CONTINUE		! Jump here when defocus fringes get too close
      WRITE(6,275) BIGDIFF,BESTFOCUS
275   FORMAT(' Biggest average intensity difference',F10.2,
     .		' at',F8.0,' Angstroms defocus')
      DFMID1=BESTFOCUS
      DFMID2=BESTFOCUS	! no astigmatism for the moment
      ENDIF
C##############################################################################
C
      IF(IAPPLY) THEN
C      Calculate and apply CTF-correction to whole transform
C
       WRITE(6,295) DFMID1,DFMID2,ANGAST,WAMP
295    FORMAT(/' Beginning application of CTF-correction,',
     .		' using defocus parameters',2F9.0,F8.1/
     .		'   and relative amplitude contrast of',F8.4)
       ANGTIT=RDEG*ANGAST
CTSH       WRITE(TITLE) DFMID1,DFMID2,ANGTIT,DAT(5:24,17)
CTSH++
       WRITE(TMPTITLE,17) DFMID1,DFMID2,ANGTIT,DAT(5:24)
CTSH--
17     FORMAT(' CTFFIND: CTF applied - defocus',2F8.0,F6.1,4X,A20)
       CALL IWRHDR(2,TITLE,1,ZERO,ZERO,ZERO)
       DO 300 IY = 1,NY
      	IF(IY.LT.NY21) THEN
      		TY = IY-1
      	ELSE
      		TY = IY-1-NY
      	ENDIF
      	INDY = (IY-1)*NXP2
      	DO 300 IX = 1,NX
      	   TX = IX-1
      	   INDEX = 1 + INDY + (IX-1)*2
      	   RAD = TX**2+TY**2
      	   IF(RAD.NE.0) THEN
      		RAD = SQRT(RAD)
      		ANGLE=RAD*THETATR
      		ANGSPT=ATAN2(TY,TX)
      		C1=TWOPI*ANGLE*ANGLE/(2.0*WL)
      		C2=-C1*CS*ANGLE*ANGLE/2.0
      		ANGDIF=ANGSPT-ANGAST
      		CCOS=COS(2.0*ANGDIF)
      		DF=0.5*(DFMID1+DFMID2+CCOS*(DFMID1-DFMID2))
      		CHI=C1*DF+C2
      		CNTRST=-SIN(CHI)-WAMP*COS(CHI)
      	   ELSE
      		CNTRST=0.0
      	   ENDIF
      	IF(IMULT) THEN
      		ARRAY(INDEX)   = ARRAY(INDEX) * CNTRST
		ARRAY(INDEX+1) = ARRAY(INDEX+1) * CNTRST
      	ELSE
      		IF(CNTRST.LT.0.0)CNTRST=AMIN1(-CTFMINMOD,CNTRST)
      		IF(CNTRST.GE.0.0)CNTRST=AMAX1(CTFMINMOD,CNTRST)
      		ARRAY(INDEX)   = ARRAY(INDEX) / CNTRST
		ARRAY(INDEX+1) = ARRAY(INDEX+1) / CNTRST
      	ENDIF
300    CONTINUE
C
C      Output of transform
      	 CALL IWRPAS(2,ARRAY,NXP2,NY,0,NX2,NY2,NYM1)
      	 ISEC=0
      	 CALL IMPOSN(2,ISEC,NY2)
      	 CALL IWRPAS(2,ARRAY,NXP2,NY,0,NX2,0,NY2-1)
      	 CALL ICLCDN(ARRAY,NX21,NY,1,NX21,1,NY,DMIN,DMAX,DMEAN)
      	CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
      	CALL IMCLOSE(2)
      	WRITE(6,305)
305   	FORMAT(' Corrected transform written out')
      ENDIF
C
      CALL EXIT
999   STOP 'END-OF-FILE ERROR ON READ'
      END
C
