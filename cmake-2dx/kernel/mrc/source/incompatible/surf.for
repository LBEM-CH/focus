*************SURF******************************************************
*
*	ROTATES A MAP THROUGH SOME THETA AND PHI ANGLE, AND DISPLAYS
*	THE FRONT SURFACE AT SOME CONTOUR LEVEL WITH DEPTH CUEING.
*	FOR MORE DETAILS, SEE SEPARATE WRITE UP.
*
*	INPUTS: THETA,PHI,CLEVEL,FMAGF
*		THETA and PHI are the equatorial and azimuthal viewing
*		angles. If THETA,PHI = 0,0 the map is viewed along the 
*		X axis, looking towards the origin. If 90,0 map is viewed 
*		along the Y axis, etc.
*
*		CLEVEL = contour level of map.
*
*		FMAGF = Magnification of output map.
*
*		INPUT AND OUTPUT MAPS ARE IN STANDARD MRC MAP/IMAGE FORMAT.
*
*	The outut of SURF should be run through LIGHT to give a shaded 
*	surface representation.
*
*	Version 1.0	Written by GV for VAX, 28/5/86
*       Version 1.1     RAC Version for AL  7Jan93
*       Version 1.2     RH change output ICRHDR to NXYZ,MXYZ 04.04.2000
************************************************************************
*
	COMMON    //NXYZ,MXYZ,CELL,ARRAY,BRRAY,CRRAY,LABELS,TITLE,RM1,RM2,RM3,P,PP
C
	PARAMETER (IDIM=1024)
C
C       Note ARRAY dimensioned for a 201 cube map maximum
	DIMENSION NXYZ(3),MXYZ(3),CELL(6),ARRAY(8120601),BRRAY(IDIM,IDIM),CRRAY(IDIM,IDIM),LABELS(20,20),TITLE(20),RM1(3,3),RM2(3,3),RM3(3,3),P(3),PP(3)
CTSH++
	CHARACTER*80 TMPTITLE
	EQUIVALENCE (TMPTITLE,TITLE)
	REAL*4 PI/3.141592654/
CTSH--
C
C	Open image files.
C
      	WRITE(6,100)
100	FORMAT(/' SURF vx 1.2 (4.6.99) program to get surface of a 3D map'/)
	CALL IMOPEN(1,'IN','RO')
	CALL IMOPEN(2,'OUT','NEW')
	CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C
	NX=NXYZ(1)
	NY=NXYZ(2)
	NZ=NXYZ(3)
	NMAX=NX*NY*NZ
C
C	Check if map will fit into array.
C
	IF (NMAX.GT.8120601) THEN
	WRITE(6,91)
91	FORMAT(' INPUT IMAGE TOO LARGE. PROGRAM STOPS.')
	STOP
	ENDIF
C
C	Read in control data.
C
	READ(5,*)THETA,PHI,CLEVEL,FMAGF
	WRITE(6,295)THETA,PHI,CLEVEL,FMAGF
295	FORMAT(' Viewing angles Theta, Phi =',2F7.2,/,' Contour level of surface  =',F7.2,/,' Linear magnification of image =',F7.2)
C
CTSH	WRITE(TITLE,301)THETA,PHI,CLEVEL,FMAGF
CTSH++
	WRITE(TMPTITLE,301)THETA,PHI,CLEVEL,FMAGF
CTSH--
301	FORMAT('SURF:Theta,phi,clevel,mag =',4F7.2)
C
C	Read map into 1-D array.
C
	DO NSEC=0,NZ-1
          DO NLINE=0,NY-1
            ISTART=(NSEC*NX*NY)+NLINE*NX+1
            CALL IRDPAL(1,ARRAY(ISTART),0,NX-1,*99)
          ENDDO
        ENDDO
C
C	Set up rotation matrix for viewing.
C
C	1) Rotate by Theta around Z axis.
C
CTSH	RM1(1,1)=COSD(THETA)
CTSH	RM1(2,1)=SIND(THETA)
CTSH	RM1(1,2)=-SIND(THETA)
CTSH	RM1(2,2)=COSD(THETA)
CTSH++
	RM1(1,1)=COS(THETA*PI/180.)
	RM1(2,1)=SIN(THETA*PI/180.)
	RM1(1,2)=-SIN(THETA*PI/180.)
	RM1(2,2)=COS(THETA*PI/180.)
CTSH--
	RM1(3,3)=1.
C
C	2) Rotate by Phi around Y axis.
C
CTSH	RM2(1,1)=COSD(PHI)
CTSH	RM2(3,1)=SIND(PHI)
CTSH	RM2(1,3)=-SIND(PHI)
CTSH	RM2(3,3)=COSD(PHI)
CTSH++
	RM2(1,1)=COS(PHI*PI/180.)
	RM2(3,1)=SIN(PHI*PI/180.)
	RM2(1,3)=-SIN(PHI*PI/180.)
	RM2(3,3)=COS(PHI*PI/180.)
CTSH--
	RM2(2,2)=1.
C
C	Set up total rotation matrix.
C
	DO 150 J=1,3
	DO 150 I=1,3
	DO 150 K=1,3
C
150	RM3(I,J)=RM3(I,J)+RM1(I,K)*RM2(K,J)
C
C	----------------------------------------------------------
C
C	Make first pass at the magnification of the map file.
C	Search a sphere with radius of the largest diameter in map (NMAX).
C	Store the position of the surface in BRRAY square.
C
C	BRRAY is specified by coordinates I,J,K in matrix, or X,Y,Z relative
C	to the search center CEN.
C
C	Look up I axis for each J,K until a point is found higher than
C	the set contour level. List I coordinate.
C
C	Find diameter of sphere to be searched.
C
	FMAX=FLOAT(NX*NX+NY*NY+NZ*NZ)
	FMAX=SQRT(FMAX)
	NMAX=NINT(FMAX)
	FMAX=FLOAT(NMAX)
C
C	Find the centre of the sphere to be searched.
C
	CEN=(FMAX+1.)*0.5
	RSQ=FMAX*FMAX/4.
C
C	Search sphere.
C 
	DO 300 K=1,NMAX
	FK=FLOAT(K)
	Z=FK-CEN
	DO 300 J=1,NMAX
	FJ=FLOAT(J)
	Y=FJ-CEN
C
C	Find extent of search in X.
C
	RANGE=RSQ-Z*Z-Y*Y
	IF (RANGE.LT.0.) GOTO 300
	RANGE=SQRT(RANGE)
	IMIN=NINT(CEN-RANGE)
	IMAX=NINT(CEN+RANGE)
C
	DEN2=DMIN
C
	DO 310 I=IMAX,IMIN,-1
	FI=FLOAT(I)
	X=FI-CEN
C
	CALL DENTERP(DEN,X,Y,Z,IERR)
C
C	Check to see if point (X,Y,Z) was outside the map.
C
	IF (IERR.EQ.0) GOTO 310
C
	IF (DEN.LT.CLEVEL) THEN
	DEN2=DEN
	GOTO 310
	ENDIF
C
C	Point found above contour level.
C	Interpolate x-coordinate.
C
	XX=(CLEVEL-DEN)/(DEN2-DEN)
C
	BRRAY(J,K)=FLOAT(I)+XX
C
	GOTO 300
C
310	CONTINUE
C
C	If point not found, value set to zero.
C
	BRRAY(J,K)=0.
C
300	CONTINUE
C
C	Go through the map again at a double sampling, using the original
C	surface to set search limits.
C
	FMAG=1.
	FMAG2=2.
	NMAX2=NINT(NMAX*FMAG2)
	IF (NMAX2.GT.IDIM) THEN
	WRITE(6,461)IDIM
461	FORMAT(' Search sphere for second pass too large. Set to ',I5)
	NMAX2=IDIM
	ENDIF
C
	CALL SURFIT(NMAX2,FMAG2,FMAG,CEN,RSQ,CLEVEL,DMIN)
C
	IF (FMAGF.LE.2.) GOTO 470
C
C	Copy CRRAY into BRRAY for next pass.
C
C
	DO 450 J=1,NMAX2
	DO 450 I=1,NMAX2
C
450	BRRAY(I,J)=CRRAY(I,J)
C
C
C	Go through the map again at final sampling, using the coarsely
C	sampled surface to set search limits.
C
	FMAG=FMAG2
	CEN=FLOAT(NMAX2+1)*0.5
	FMAG2=FMAGF
	NMAX=NMAX2
	NMAX2=NINT(NMAX*FMAGF/FMAG)
	IF (NMAX2.GT.IDIM) THEN
	WRITE(6,462)IDIM
462	FORMAT(' Search sphere for third pass too large. Set to ',I5)
	NMAX2=IDIM
	ENDIF
C
	CALL SURFIT(NMAX2,FMAG2,FMAG,CEN,RSQ,CLEVEL,DMIN)
C
C
470	CONTINUE
C
C	Write out the map.
C
	NXYZ(1)=NMAX2
	NXYZ(2)=NMAX2
	NXYZ(3)=1
C
	MODE=2
C
	CALL IRTLAB(1,LABELS,NL)
	CALL ICRHDR(2,NXYZ,MXYZ,MODE,LABELS,NL)
C
	CALL IWRPAS(2,CRRAY,IDIM,IDIM,0,NMAX2-1,0,NMAX2-1)
	CALL ICLDEN(CRRAY,IDIM,IDIM,1,NMAX2,1,NMAX2,DMIN,DMAX,DMEAN)
C
	CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
	CALL IMCLOSE(2)
	CALL IMCLOSE(1)
C
	WRITE(6,93)
93	FORMAT(' PROGRAM EXECUTED TO END.')
	STOP
99	WRITE(6,92)
92	FORMAT(' END OF IMAGE WHILE READING')
	STOP
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	Subroutine to find new surface at higher mag than old surface.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE SURFIT(NMAX2,FMAG2,FMAG,CEN,RSQ,CLEVEL,DMIN)
C
	COMMON    //NXYZ,MXYZ,CELL,ARRAY,BRRAY,CRRAY,LABELS,TITLE,RM1,RM2,RM3,P,PP
C
	PARAMETER (IDIM=1024)
C
	DIMENSION NXYZ(3),MXYZ(3),CELL(6),ARRAY(8120601),BRRAY(IDIM,IDIM),CRRAY(IDIM,IDIM),LABELS(20,20),TITLE(20),RM1(3,3),RM2(3,3),RM3(3,3),P(3),PP(3)
C
	CEN2=(FLOAT(NMAX2)+1.)*0.5
C*** Next 2 lines inserted by jms 07.03.96
	NFIT = 0
	NSEARCH = 0
	DO 400 K2=1,NMAX2
C	Find coordinates in original sampling.
	FK2=FLOAT(K2)
	Z=(FK2-CEN2)/FMAG2
	FK=NINT(Z*FMAG+CEN)
	FK1=FK-1
	FK2=FK+1
	FK1=MAX(FK1,1.)
	FK2=MIN(FK2,FLOAT(IDIM))
C
	DO 410 J2=1,NMAX2
	FJ2=FLOAT(J2)
	Y=(FJ2-CEN2)/FMAG2
	FJ=NINT(Y*FMAG+CEN)
C
C	Find limits of search required in X.
C
	FJ1=FJ-1
	FJ2=FJ+1
	FJ1=MAX(FJ1,1.)
	FJ2=MIN(FJ2,FLOAT(IDIM))
C!!! insert by JMS. Sun compiler cannot handle real array subscripts
C
C!!!	X1=MIN(BRRAY(FJ1,FK1),BRRAY(FJ,FK1),BRRAY(FJ2,FK1),
C!!!	1      BRRAY(FJ1,FK),BRRAY(FJ,FK),BRRAY(FJ2,FK),
C!!!	1      BRRAY(FJ1,FK2),BRRAY(FJ,FK2),BRRAY(FJ2,FK2))
C
C!!!	X2=MAX(BRRAY(FJ1,FK1),BRRAY(FJ,FK1),BRRAY(FJ2,FK1),
C!!!	1      BRRAY(FJ1,FK),BRRAY(FJ,FK),BRRAY(FJ2,FK),
C!!!	1      BRRAY(FJ1,FK2),BRRAY(FJ,FK2),BRRAY(FJ2,FK2))
	ij = fj
	ij1 = fj1
	ij2 = fj2
	ik = fk
	ik1 = fk1
	ik2 = fk2
	x1=min(brray(ij1,ik1),brray(ij,ik1),brray(ij2,ik1),brray(ij1,ik),brray(ij,ik),brray(ij2,ik),brray(ij1,ik2),brray(ij,ik2),brray(ij2,ik2))
C
	x2=max(brray(ij1,ik1),brray(ij,ik1),brray(ij2,ik1),brray(ij1,ik),brray(ij,ik),brray(ij2,ik),brray(ij1,ik2),brray(ij,ik2),brray(ij2,ik2))
C
C	Check if point on back wall.
C
	IF ((X1.NE.0.).OR.(X2.NE.0.)) GOTO 412
C
	CRRAY(J2,K2)=0.
	GOTO 410
C
412	CONTINUE
C
C	Find limits of search in old sampling.
C
	X1=(X1-CEN-1.)/FMAG
	X2=(X2-CEN+1.)/FMAG
C
C	Find limits in new sampling.
C
	IMIN=NINT((X1*FMAG2)+CEN2)
	IMAX=NINT((X2*FMAG2)+CEN2)
C
C	Search, with trilinear interpolation.
C
	DEN2=DMIN
	DO 420 I2=IMAX,IMIN,-1
	FI2=FLOAT(I2)
	X=(FI2-CEN2)/FMAG2
C
	CALL DENTERP(DEN,X,Y,Z,IERR)
C
	IF (IERR.EQ.0) GOTO 420
C
	IF (DEN.LT.CLEVEL) THEN
	DEN2=DEN
	GOTO 420
	ENDIF
C
	IF (DEN2.EQ.DMIN) GOTO 440
C
C	Interpolate x-coordinate.
C
	XX=(CLEVEL-DEN)/(DEN2-DEN)
C
	CRRAY(J2,K2)=FLOAT(I2)+XX
C
	NFIT=NFIT+1
C
	GOTO 410
C
420	CONTINUE
C
C	If program reaches this point, no value was found in search.
C	Therefore:
C
440	CONTINUE
C
	RANGE=RSQ-(Y*Y+Z*Z)
	IF (RANGE.LT.0.) GOTO 431
	RANGE=SQRT(RANGE)
	IMIN2=NINT(CEN2-RANGE)-1
	IMAX2=NINT(CEN2+RANGE)+1
C
	NSEARCH=NSEARCH+1
	DEN2=DMIN
	DO 430 I2=IMAX2,IMIN2,-1
	X=(FLOAT(I2)-CEN2)/FMAG2
C
	CALL DENTERP(DEN,X,Y,Z,IERR)
C
	IF (IERR.EQ.0) GOTO 430
C
	IF (DEN.LT.CLEVEL) THEN
	DEN2=DEN
	GOTO 430
	ENDIF
C
C	Interpolate x-coordinate.
C
	XX=(CLEVEL-DEN)/(DEN2-DEN)
C
	CRRAY(J2,K2)=FLOAT(I2)+XX
C
	GOTO 410
C
430	CONTINUE
431     continue
C
	CRRAY(J2,K2)=0.
C
410	CONTINUE
C
400	CONTINUE
C
490	CONTINUE
C
C	-------------------------------------------------------------
C
C	Write out the number of points which had to be searched from scratch,
C	and no. of points which came out OK.
C
	WRITE(6,499)NSEARCH,NFIT
499	FORMAT(' No. of points searched from scratch =',I8,/,' No. of points that fitted O.K. =',I8)
C
	RETURN
C
	END
C
C********************************************************************
C
C	Subroutine to perform interpolation and rotation of image.
C	Checks bounds of array and to see whether in allowed segment.
C
C*******************************************************************
C
	SUBROUTINE DENTERP(DEN,FI,FJ,FK,IERR)
C
	COMMON    //NXYZ,MXYZ,CELL,ARRAY,BRRAY,CRRAY,LABELS,TITLE,RM1,RM2,RM3,P,PP
C
	PARAMETER (IDIM=1024)
C
	DIMENSION NXYZ(3),MXYZ(3),CELL(6),ARRAY(8120601),BRRAY(IDIM,IDIM),CRRAY(IDIM,IDIM),LABELS(20,20),TITLE(20),RM1(3,3),RM2(3,3),RM3(3,3),P(3),PP(3)
C
	NX=NXYZ(1)
	NY=NXYZ(2)
	NZ=NXYZ(3)
	IERR=1
C
	XCEN=(FLOAT(NX)+1.)*0.5
	YCEN=(FLOAT(NY)+1.)*0.5
	ZCEN=(FLOAT(NZ)+1.)*0.5
C
C	Transform input coordinates to map coordinate frame.
C
	P(3)=FK
	P(2)=FJ
	P(1)=FI
C
	DO 250 II=1,3
	PP(II)=0.
	DO 260 JJ=1,3
	PP(II)=PP(II)+RM3(II,JJ)*P(JJ)
260	CONTINUE
250	CONTINUE
C
	X=PP(1)+XCEN	
	Y=PP(2)+YCEN	
	Z=PP(3)+ZCEN	
C
	IX=X
	IY=Y
	IZ=Z
C
C	Check if outside array.
C
	IF ((IX.LT.1).OR.(IY.LT.1).OR.(IZ.LT.1).OR.(IX.GT.NX-1).OR.(IY.GT.NY-1).OR.(IZ.GT.NZ-1)) THEN
C
	IERR=0
	GOTO 200
	ENDIF
C
	FX=1.+IX-X
	FY=1.+IY-Y
	FZ=1.+IZ-Z
C
	FX1=1.-FX
	FY1=1.-FY
	FZ1=1.-FZ
C
	IX1=IX+1
	IY1=IY+1
	IZ1=IZ+1
C
	N1=((IZ-1)*NX*NY)+((IY-1)*NX)+IX
	N2=((IZ-1)*NX*NY)+((IY-1)*NX)+IX1
	N3=((IZ-1)*NX*NY)+((IY1-1)*NX)+IX
	N4=((IZ1-1)*NX*NY)+((IY-1)*NX)+IX
	N5=((IZ-1)*NX*NY)+((IY1-1)*NX)+IX1
	N6=((IZ1-1)*NX*NY)+((IY-1)*NX)+IX1
	N7=((IZ1-1)*NX*NY)+((IY1-1)*NX)+IX
	N8=((IZ1-1)*NX*NY)+((IY1-1)*NX)+IX1
C
	DEN=ARRAY(N1)*FX*FY*FZ+ARRAY(N2)*FX1*FY*FZ+ARRAY(N3)*FX*FY1*FZ+ARRAY(N4)*FX*FY*FZ1+ARRAY(N5)*FX1*FY1*FZ+ARRAY(N6)*FX1*FY*FZ1+ARRAY(N7)*FX*FY1*FZ1+ARRAY(N8)*FX1*FY1*FZ1
C
200	CONTINUE
C
	RETURN
C
	END
