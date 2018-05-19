C*2DX_FFTlib.FOR********************************************************
C
C	TWO-DIMENSIONAL FOURIER TRANSFORM SUBROUTINE FOR IMAGE
C	PROCESSING. DOES BOTH FOWARD & INVERSE TRANSFORMS
C	USES LYNN TENEYCK'S MIXED-RADIX ROUTINES
C	THUS THE ONLY RESTRICTION IS THAT THE IMAGE SIZE BE
C	AN EVEN NUMBER AND HAVE NO PRIME FACTORS LARGER THAN 19!!
C
C	IDIR =  0	FOWARD  TRANSFORM  :  exp(+2PIirs)
C	IDIR =  1	INVERSE TRANSFORM  :  exp(-2PIirs)
C	IDIR = -1	INVERSE TRANSFORM BUT NO COMPLEX CONJUGATE
C
C	DATA SET UP AS NY ROWS OF NX NUMBERS
C	NOTE NX,NY ALWAYS REFER TO THE REAL-SPACE IMAGE SIZE
C
C	NX,NY IMAGE IS TRANSFORMED IN-PLACE INTO NY STRIPS OF
C	NX/2 + 1 COMPLEX FOURIER COEFFICIENTS
C	THE ORIGIN IS LOCATED AT THE FIRST POINT!!!
C
C	ARRAY MUST BE DIMENSIONED TO ALLOW FOR EXTRA STRIP OF COMPLEX
C	NUMBERS ON OUTPUT.
C	THUS FOR A 300X400 TRANSFORM, ARRAY MUST BE DIMENSIONED:
C	REAL ARRAY(302,400)
C
C	A NORMALIZATION FACTOR IS APPLIED DURING BOTH  TRANSFORMATIONS
C
C	VERSION 1.00	OCT 11 1981		DAA
C	VERSION 1.02	APR 19 1982		DAA
C	VERSION 1.03	NOV 23 1982		DAA
C	Version 1.04    Mar 17 1998             JMS
C       Version 2.00    Oct 29 1999             HST
C
      SUBROUTINE TDXFFT(ARRAY,NX,NY,IDIR)
      DIMENSION ARRAY(*),IDIM(5)
C
      parameter (MAXSIZE = 15000)
C
      real*4 coeff(MAXSIZE+16+2*(MAXSIZE+16))
      real*4 WRK(MAXSIZE+1+4*(MAXSIZE+1))
C
      write(6,'(''In TDXFFT in 2dx_fftlib.for'')')
      if(nx.gt.MAXSIZE .or. ny.gt.MAXSIZE)then
        write(*,'(''TDXFFT: ERROR: maxsize='',I8)')MAXSIZE
        STOP
      endif
C
      NXO2 = NX/2
      IF (2*NXO2 .NE. NX) THEN
        WRITE(6,'(''TDXFFT: NX= '',I7,'' MUST BE EVEN!!!'')') NX
        STOP
      endif
C
      IF (IDIR .NE. 0) GOTO 50
C
C********      FOWARD TRANSFORM COMES HERE       ******************
C
c-----initialize the table
C-----CALL SCFFT2D (isign,n1,n2,scale,x,ldx,y,ldy,table,work,isys)
C
      ldx = nx+2
      ldy = ldx/2
C
      call scfft2d(0,nx,ny,0.0,ARRAY,ldx,ARRAY,ldy,coeff,WRK,0)
C
c-----perform the fft
C
      ONEVOL = SQRT(1.0/(NX*NY))
      call scfft2d(1,nx,ny,ONEVOL,ARRAY,ldx,ARRAY,ldy,coeff,WRK,0)
C
      RETURN
C
 50   continue
C
C**********        INVERSE TRANSFORM     *******************
C
c-----initialize the table
C-----CALL SCFFT2D (isign,n1,n2,scale,x,ldx,y,ldy,table,work,isys)
C
      ldx = nx/2+1
      ldy = nx+2
C
      call csfft2d(0,nx,ny,0.0,ARRAY,ldx,ARRAY,ldy,coeff,WRK,0)
C
c-----perform the fft
C
      ONEVOL = SQRT(1.0/(NX*NY))
      call csfft2d(-1,nx,ny,ONEVOL,ARRAY,ldx,ARRAY,ldy,coeff,WRK,0)
C
      RETURN
      END
