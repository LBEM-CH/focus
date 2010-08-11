C*IMSUBS2.FOR************************************************************
C									*
C		ADDITIONAL IMAGE PROCESSING SUBROUTINES			*
C									*
C	Last Update:	02.4.82		DAA	for VAX			*
C									*
C************************************************************************
C*FILTER.FOR****************************************************
C
C	THIS SUBROUTINE DOES 2-D CONVOLUTION FILTERING IN FOURIER SPACE.
C	THE 2-D IMAGE STORED IN ARRAY IS FOURIER TRANSFORMED.
C	A RADIALLY-SYMMETRIC CONTRAST-TRANSFER FUNCTION IS APPLIED
C	IN FOURIER SPACE AND AN INVERSE TRANSFORM PERFORMED.
C	ALL OPERATIONS ARE PERFORMED IN-PLACE.
C
C	NOTE: ARRAY MUST BE DIMENSIONED AT LEAST (NX+2,NY) TO MAKE ROOM FOR
C	      FORURIER TERMS!!!!!! ALL INDEXING OF ELEMENTS IN
C	      ARRAY IS CARRIED OUT EXPLICITLY = THUS ARRAY CAN BE A
C	      1-D VECTOR.
C
C
C	CALL FILTER(ARRAY,NX,NY,CTF,DELTA)
C
C	NX IS FAST-DIMENSION
C	DELTA IS THE RECIPROCAL-SPACE INTERVAL FOR ELEMENTS OF THE CONTRAST-
C	TRANSFER FUNCTION (CTF).  FIRST ELEMENT IS AT S=0
C
C	VERSION 1.00		JULY 23 1981		DAA
C
	SUBROUTINE FILTER(ARRAY,NX,NY,CTF,DELTA)
	DIMENSION ARRAY(*),CTF(1)
	COMPLEX ARRAY
C
	NXO2 = NX/2
	NX21 = NXO2 + 1
	NYM1 = NY - 1
	DELX = 1.0/NX
	DELY = 1.0/NY
C
	CALL TODFFT(ARRAY,NX,NY,0)
C
C   APPLY FILTER FUNCTION
C
	INDEX = 1
	DO 200 IY = 0,NYM1
	  Y = IY*DELY
	  IF (Y .GT. 0.5) Y = 1.0 - Y
	  Y2 = Y*Y
	  X = 0.0
	  DO 100 IX = 0,NXO2
	    IND = INDEX + IX
	    S = SQRT(X*X + Y2)
	    INDF = S/DELTA + 1.5
	    ARRAY(IND) = ARRAY(IND)*CTF(INDF)
	    X = X + DELX
100	  CONTINUE
	  INDEX = INDEX + NX21
200	CONTINUE
C
	CALL TODFFT(ARRAY,NX,NY,1)
C
	RETURN
	END
C*BIGFILT.FOR************************************************************
C									*
C	  This subroutine does 2-d convolution filtering in fourier 	*
C	space, on very LARGE images. The Input and Output images are	*
C	 stored on disk.						*
C	  The input images is fourier transformed. A radially-		*
C	symmetric contrast-transfer function is applied in fourier 	*
C	space and an inverse transform performed.			*
C	  This subroutine does the foward and inverse 2-D FFT's using	*
C	the Lynn Ten Eyck FFT subroutines - on very large images.	*
C	Arbitray sized transforms having prime factors not larger	*
C	than 19 can be accomidated.					*
C	  								*
C	  All necessary file set up for In/Out units must be		*
C	done by the calling program!!!					*
C	  								*
C	A scratch file is created to handle the required 		*
C	transpositions.							*
C									*
C									*
C	CALL BIGFILT(INUNIT,IOUTUNIT,NX,NY,CTF,DELTA,DMIN,DMAX,DMEAN)	*
C									*
C	NX is fast-dimension						*
C	DELTA is the reciprocal-space interval for elements of the	*
C	      contrast-transfer function (ctf). The  first element	*
C	      is at s=0							*
C									*
C	  The buffer space is available thru the common block /FTBUF/	*
C									*
C	Last Update:	23.July.1982		DAA      for VAX	*
C									*
C									*
C************************************************************************
C
	SUBROUTINE BIGFILT(INUNIT,IOUTUNIT,NX,NY,CTF,DELTA,DMIN,
     .	DMAX,DMEAN)
	COMMON/FTBUF/ NBUFSIZ,ARRAY(500000)
	COMPLEX CRAY(250000),CLINE(4096)
	DIMENSION CTF(1)
	EQUIVALENCE (ARRAY,CRAY)
C
	CALL QOPEN(ISCR,'SCRATCH','SCRATCH')
	NXP2 = NX + 2
	NXP24 = NXP2*4
	NX21 = NXP2/2
	NXM1 = NX - 1
	NY2 = NY*2
	NY21 = NY/2 + 1
	DELX = 1.0/NX
	DELY = 1.0/NY
	DMIN =  1.E20
	DMAX = -1.E20
	DMEAN = 0.0
C
C  Calculate buffer loads
C
	NLINES = MIN(NBUFSIZ/NXP2,NY)
	JLINES = NLINES
	NSEC = NLINES*NXP24
	NLOADS = NY/NLINES
	NLAST = NY - NLINES*NLOADS
	IF (NLAST .EQ. 0) THEN
	  NLAST = NLINES
	ELSE
	  NLOADS = NLOADS + 1
	END IF
C
	NUSE = MIN(NBUFSIZ/NY2,NX21)
	NUSE8 = NUSE*8
	MLOADS = NX21/NUSE
	MLAST = NX21 - NUSE*MLOADS
	IF (MLAST .EQ. 0) THEN
	  MLAST = NUSE
	ELSE
	  MLOADS = MLOADS + 1
	END IF
C
C  Do first part of foward transform - writeout with no transposition
C
	WRITE(6,1000) NLINES,NLOADS,NUSE,MLOADS
1000	FORMAT(/,' BIGFILT: # Lines, Loads for passes 1,2: ',4I8,/)
	DO 150 LOADS = 1,NLOADS
	  IF (LOADS .EQ. NLOADS) THEN
	    NLINES = NLAST
	    NSEC = NLINES*NXP24
	  END IF
	  INDEX = 1
	  DO 100 LINES = 1,NLINES
	    CALL IRDLIN(INUNIT,ARRAY(INDEX),*90)
	    INDEX = INDEX + NXP2
100	  CONTINUE
C
	  CALL ODFFT(ARRAY,NX,NLINES,0)
	  CALL QWRITE(ISCR,ARRAY,NSEC)
150	CONTINUE
C
C  Do second half of foward transform, transpose on reading in 
C
	AX = 0.0
	INDEX = 1
	DO 500 LOADS = 1,MLOADS
	  IF (LOADS .EQ. MLOADS) THEN
	    NUSE = MLAST
	    NUSE8 = NUSE*8
	  END IF
	  DO 250 IY = 1,NY
	    CALL QSEEK(ISCR,IY,INDEX,NXP24)
	    CALL QREAD(ISCR,CLINE,NUSE8,IER)
	    IF (IER .NE. 0) GOTO 90
	    IND = IY
	    DO 200 J = 1,NUSE
	      CRAY(IND) = CLINE(J)
	      IND = IND + NY
200	    CONTINUE
250	  CONTINUE
C
	  CALL ODFFT(CRAY,NY,NUSE,-1)
C
C  Apply filter function
C
	  IND = 0
	  DO 350 J = 1,NUSE
	    AX2 = AX*AX
	    AY = 0.0
	    DO 300 IY = 1,NY
	      IND = IND + 1
	      IF (IY .LE. NY21) THEN
		S = SQRT(AX2 + AY*AY)
	      ELSE
		S = SQRT(AX2 + (1.0 - AY)**2)
	      END IF
	      INDF = S/DELTA + 1.5
	      CRAY(IND) = CRAY(IND)*CTF(INDF)
	      AY = AY + DELY
300	    CONTINUE
	    AX = AX + DELX
350	  CONTINUE
C
C  Do first part of inverse transform, transpose on writing out
C
	  CALL ODFFT(CRAY,NY,NUSE,-2)
C
	  DO 450 IY = 1,NY
	    IND = IY
	    DO 400 J = 1,NUSE
	      CLINE(J) = CRAY(IND)
	      IND = IND + NY
400	    CONTINUE
	    CALL QSEEK(ISCR,IY,INDEX,NXP24)
	    CALL QWRITE(ISCR,CLINE,NUSE8)
450	  CONTINUE
	  INDEX = INDEX + NUSE8
500	CONTINUE
C
C   Do second half of inverse transform, no transposition required
C
	NLINES = JLINES
	CALL QSEEK(ISCR,1,1,1)
	DO 700 LOADS = 1,NLOADS
	  IF (LOADS .EQ. NLOADS) NLINES = NLAST
	  NSEC = NLINES*NXP24
	  CALL QREAD(ISCR,CRAY,NSEC,IER)
	  IF (IER .NE. 0) GOTO 90
C
	  CALL ODFFT(CRAY,NX,NLINES,1)
C
	  IND = 1
	  DO 650 LINES=1,NLINES
	    CALL IWRLIN(IOUTUNIT,ARRAY(IND))
	    DO 600 J = 0,NXM1
	      VAL = ARRAY(IND + J)
	      IF (VAL .LT. DMIN) DMIN = VAL
	      IF (VAL .GT. DMAX) DMAX = VAL
	      DMEAN = DMEAN + VAL
600	    CONTINUE
	    IND = IND + NXP2
650	  CONTINUE
	  INDEX = INDEX + NLINE8
700	CONTINUE
	DMEAN = DMEAN/(NX*NY)
C
	GOTO 99
C
C  Finish up
C
90	WRITE(6,9000)
9000	FORMAT(//,' ******* BIGFILT:   ERROR ON READ  ********',//)
	CALL IMCLOSE(INUNIT)
	CALL IMCLOSE(IOUTUNIT)
	CALL QCLOSE(ISCR)
	STOP '*** ERROR ****'
99	CALL QCLOSE(ISCR)
	RETURN
	END	
C*INTERP.FOR***************************************************
C
C	This subroutine will perform coordinate transformations
C	(rotations,translations, etc.) by quadratic interpolation.
C
C	BRAY = T[ ARRAY ]*scale
C
C	ARRAY	- The input image array
C	BRAY	- The output image array
C	NXA,NYA	- The dimensions of ARRAY
C	NXB,NYB	- The dimensions of BRAY
C	AMAT	- A 2x2 matrix to specify rotation,scaling,skewing
C	XC,YC	- The cooridinates of the Center of ARRAY
C	XT,YT	- The translation to add to the final image. The
C		  center of the output array is normally taken as
C		  NXB/2, NYB/2
C	SCALE	- A multiplicative scale actor for the intensities
C	
C	Xo = a11(Xi - Xc) + a12(Yi - Yc) + NXB/2. + XT
C	Yo = a21(Xi - Xc) + a22(Yi - Yc) + NYB/2. + YT
C
	SUBROUTINE INTERP(ARRAY,BRAY,NXA,NYA,NXB,NYB,AMAT,
     .	XC,YC,XT,YT,SCALE)
	DIMENSION ARRAY(NXA,NYA),BRAY(NXB,NYB),AMAT(2,2)
C
C   Calc inverse transformation
C
	XCEN = NXB/2. + XT + 1
	YCEN = NYB/2. + YT + 1
	XCO = XC + 1
	YCO = YC + 1
	DENOM = AMAT(1,1)*AMAT(2,2) - AMAT(1,2)*AMAT(2,1)
	A11 =  AMAT(2,2)/DENOM
	A12 = -AMAT(1,2)/DENOM
	A21 = -AMAT(2,1)/DENOM
	A22 =  AMAT(1,1)/DENOM
C
C Loop over output image
C
	DO 200 IY = 1,NYB
	  DYO = IY - YCEN
	  DO 100 IX = 1,NXB
	    DXO = IX - XCEN
	    XP = A11*DXO + A12*DYO + XCO
	    YP = A21*DXO + A22*DYO + YCO
	    IXP = NINT(XP)
	    IYP = NINT(YP)
	    BRAY(IX,IY) = 0.0
	    IF (IXP .LT. 1 .OR. IXP .GT. NXA) GOTO 100
	    IF (IYP .LT. 1 .OR. IYP .GT. NYA) GOTO 100
C
C   Do quadratic interpolation
C
	    DX = XP - IXP
	    DY = YP - IYP
	    IXPP1 = IXP + 1
	    IXPM1 = IXP - 1
	    IYPP1 = IYP + 1
	    IYPM1 = IYP - 1
	    IF (IXPM1 .LT. 1) IXPM1 = 1
	    IF (IYPM1 .LT. 1) IYPM1 = 1
	    IF (IXPP1 .GT. NXA) IXPP1 = NXA
	    IF (IYPP1 .GT. NYA) IYPP1 = NYA
C
C	Set up terms for quadratic interpolation
C
	    V2 = ARRAY(IXP, IYPM1)
	    V4 = ARRAY(IXPM1, IYP)
	    V5 = ARRAY(IXP, IYP)
	    V6 = ARRAY(IXPP1, IYP)
	    V8 = ARRAY(IXP, IYPP1)
C
	    A = (V6 + V4)*.5 - V5
	    B = (V8 + V2)*.5 - V5
	    C = (V6 - V4)*.5
	    D = (V8 - V2)*.5
C
	    BRAY(IX,IY) = SCALE*(A*DX*DX + B*DY*DY + C*DX + D*DY + V5)
C
100	  CONTINUE
200	CONTINUE
C
	RETURN
	END
C
C*CINTERP
C
C	As above but for COMPLEX data
C
C
	SUBROUTINE CINTERP(ARRAY,BRAY,NXA,NYA,NXB,NYB,AMAT,
     .	XC,YC,XT,YT,SCALE)
	COMPLEX ARRAY(NXA,NYA),BRAY(NXB,NYB),A,B,C,D,V2,V4,V5,V6,V8
	DIMENSION AMAT(2,2)
C
C   Calc inverse transformation
C
	XCEN = NXB/2. + XT + 1
	YCEN = NYB/2. + YT + 1
	XCO = XC + 1
	YCO = YC + 1
	DENOM = AMAT(1,1)*AMAT(2,2) - AMAT(1,2)*AMAT(2,1)
	A11 =  AMAT(2,2)/DENOM
	A12 = -AMAT(1,2)/DENOM
	A21 = -AMAT(2,1)/DENOM
	A22 =  AMAT(1,1)/DENOM
C
C Loop over output image
C
	DO 200 IY = 1,NYB
	  DYO = IY - YCEN
	  DO 100 IX = 1,NXB
	    DXO = IX - XCEN
	    XP = A11*DXO + A12*DYO + XCO
	    YP = A21*DXO + A22*DYO + YCO
	    IXP = NINT(XP)
	    IYP = NINT(YP)
	    BRAY(IX,IY) = 0.0
	    IF (IXP .LT. 1 .OR. IXP .GT. NXA) GOTO 100
	    IF (IYP .LT. 1 .OR. IYP .GT. NYA) GOTO 100
C
C   Do quadratic interpolation
C
	    DX = XP - IXP
	    DY = YP - IYP
	    IXPP1 = IXP + 1
	    IXPM1 = IXP - 1
	    IYPP1 = IYP + 1
	    IYPM1 = IYP - 1
	    IF (IXPM1 .LT. 1) IXPM1 = 1
	    IF (IYPM1 .LT. 1) IYPM1 = 1
	    IF (IXPP1 .GT. NXA) IXPP1 = NXA
	    IF (IYPP1 .GT. NYA) IYPP1 = NYA
C
C	Set up terms for quadratic interpolation
C
	    V2 = ARRAY(IXP, IYPM1)
	    V4 = ARRAY(IXPM1, IYP)
	    V5 = ARRAY(IXP, IYP)
	    V6 = ARRAY(IXPP1, IYP)
	    V8 = ARRAY(IXP, IYPP1)
C
	    A = (V6 + V4)*.5 - V5
	    B = (V8 + V2)*.5 - V5
	    C = (V6 - V4)*.5
	    D = (V8 - V2)*.5
C
	    BRAY(IX,IY) = SCALE*(A*DX*DX + B*DY*DY + C*DX + D*DY + V5)
C
100	  CONTINUE
200	CONTINUE
C
	RETURN
	END
