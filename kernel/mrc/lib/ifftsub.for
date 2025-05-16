C*ODFFT.FOR*******************************************************
C
C       Performs NY 1-D FFT'S in place. Uses Lynn Ten Eyck's FFT routines.
C       Thus only restrictions are NX MUST be EVEN & max prime factor =19.
C
C       Origin is at first point in each strip
C       A Normalization factor is applied during ALL transformations
C
C       For Real/complex or Complex/real:
C               Only the unique portion of the Transform is written
C               DIMENSION ARRAY(NX+2,NY)
C
C       For Complex/complex
C               The entire Transform is written
C               COMPLEX ARRAY(NX,NY)
C
C
C       IDIR =  0 Foward   (Real --> Complex)     exp(+2PIirs)
C       IDIR =  1 Reverse  (Complex --> Real)     exp(-2PIirs)
C       IDIR = -1 Foward   (Complex --> Complex)  exp(+2PIirs)
C       IDIR = -2 Reverse  (Complex --> Complex)  exp(-2PIirs)
C
C
C       Version 1.00    Apr 19 1982             DAA
C       Version 1.01    Nov 23 1982             DAA
C       Version 1.02    Mar 17 1998             JMS
C
C HERE, ARRAY IS REAL
C
        SUBROUTINE ODFFT(ARRAY,NX,NY,IDIR)
        DIMENSION ARRAY(1),IDIM(5)
C
        NXO2 = NX/2
        IF (2*NXO2 .EQ. NX .OR. IDIR .LT. 0) GOTO 1
        WRITE(6,1000) NX
1000    FORMAT(' ODFFT: NX= ',I7,' MUST BE EVEN!!!')
        STOP
1       ONEVOL = SQRT(1.0/NX)
        GOTO (10,10,30,40) IDIR + 3
C
C********      COMPLEX TRANSFORMS COME HERE       ******************
C
C
C
C  SET UP FOR FIRST DIMENSION OF TRANSFORM
C
10      NX2 = NX*2
        NXT = NX2*NY
        NXT1 = NXT - 1
        IDIM(1) = NXT
        IDIM(2) = 2
        IDIM(3) = IDIM(1)
        IDIM(4) = IDIM(1)
        IDIM(5) = NX2
C
        CALL CMPLFT(ARRAY(1),ARRAY(2),NX,IDIM)
C
        IF (IDIR .EQ. -1) GOTO 15
C
C   SCALE BY 1/VOLUME
C
        DO 100 J = 1,NXT1,2
          ARRAY(J) = ARRAY(J)*ONEVOL
          ARRAY(J+1) = ARRAY(J+1)*ONEVOL
100     CONTINUE
        RETURN
C
C   TAKE COMPLEX CONJUGATE TO DO FOWARD & SCALE BY 1/VOLUME
C
15      DO 150 J = 1,NXT1,2
          ARRAY(J) = ARRAY(J)*ONEVOL
          ARRAY(J+1) = -ARRAY(J+1)*ONEVOL
150     CONTINUE
        RETURN
C
C
C********      FOWARD REAL TRANSFORM COMES HERE       ******************
C
C
C  SET UP FOR TRANSFORM
C
30      NXP2 = NX + 2
        NXT = NXP2*NY
        IDIM(1) = NXT
        IDIM(2) = 2
        IDIM(3) = IDIM(1)
        IDIM(4) = IDIM(1)
        IDIM(5) = NXP2
C
        CALL REALFT(ARRAY(1),ARRAY(2),NXO2,IDIM)
C
C    NORMALIZE DATA 
C
        DO 200 J = 1,NXT
          ARRAY(J) = ARRAY(J)*ONEVOL
200     CONTINUE
C
        RETURN

C
C********      INVERSE HERMITE TRANSFORM COMES HERE       ******************
C
C
C  SET UP FOR TRANSFORM
C
40      NXP2 = NX + 2
        NXT = NXP2*NY
        NXM1 = NX - 1
        IDIM(1) = NXT
        IDIM(2) = 2
        IDIM(3) = IDIM(1)
        IDIM(4) = IDIM(1)
        IDIM(5) = NXP2
C
C   NORMALIZE DATA
C
        DO 300 J = 1,NXT
          ARRAY(J) = ONEVOL*ARRAY(J)
300     CONTINUE
C
C  CHANGE DATA STORAGE MODE
C
        INDEX = 2
        DO 350 IY = 1,NY
          ARRAY(INDEX) = ARRAY(NXM1 + INDEX)
          INDEX = INDEX + NXP2
350     CONTINUE
C
        CALL HERMFT(ARRAY(1),ARRAY(2),NXO2,IDIM)
C
        RETURN
        END

C*CODFFT.FOR*******************************************************
C
C       Performs NY 1-D FFT'S in place. Uses Lynn Ten Eyck's FFT routines.
C       Thus only restrictions are NX MUST be EVEN & max prime factor =19.
C
C       Origin is at first point in each strip
C       A Normalization factor is applied during ALL transformations
C
C       For Real/complex or Complex/real:
C               Only the unique portion of the Transform is written
C               DIMENSION ARRAY(NX+2,NY)
C
C       For Complex/complex
C               The entire Transform is written
C               COMPLEX ARRAY(NX,NY)
C
C
C       IDIR =  0 Foward   (Real --> Complex)     exp(+2PIirs)
C       IDIR =  1 Reverse  (Complex --> Real)     exp(-2PIirs)
C       IDIR = -1 Foward   (Complex --> Complex)  exp(+2PIirs)
C       IDIR = -2 Reverse  (Complex --> Complex)  exp(-2PIirs)
C
C
C       Version 1.00    Apr 19 1982             DAA
C       Version 1.01    Nov 23 1982             DAA
C       Version 1.02    Mar 17 1998             JMS
C
C HERE, ARRAY IS COMPLEX
C
        SUBROUTINE CODFFT(CRAY,NX,NY,IDIR)
        DIMENSION IDIM(5)
        REAL ARRAY(1)
        COMPLEX CRAY(1)
        EQUIVALENCE ARRAY, CRAY
C
        NXO2 = NX/2
        IF (2*NXO2 .EQ. NX .OR. IDIR .LT. 0) GOTO 1
        WRITE(6,1000) NX
1000    FORMAT(' ODFFT: NX= ',I7,' MUST BE EVEN!!!')
        STOP
1       ONEVOL = SQRT(1.0/NX)
        GOTO (10,10,30,40) IDIR + 3
C
C********      COMPLEX TRANSFORMS COME HERE       ******************
C
C
C
C  SET UP FOR FIRST DIMENSION OF TRANSFORM
C
10      NX2 = NX*2
        NXT = NX2*NY
        NXT1 = NXT - 1
        IDIM(1) = NXT
        IDIM(2) = 2
        IDIM(3) = IDIM(1)
        IDIM(4) = IDIM(1)
        IDIM(5) = NX2
C
        CALL CMPLFT(ARRAY(1),ARRAY(2),NX,IDIM)
C
        IF (IDIR .EQ. -1) GOTO 15
C
C   SCALE BY 1/VOLUME
C
        DO 100 J = 1,NXT1,2
          ARRAY(J) = ARRAY(J)*ONEVOL
          ARRAY(J+1) = ARRAY(J+1)*ONEVOL
100     CONTINUE
        RETURN
C
C   TAKE COMPLEX CONJUGATE TO DO FOWARD & SCALE BY 1/VOLUME
C
15      DO 150 J = 1,NXT1,2
          ARRAY(J) = ARRAY(J)*ONEVOL
          ARRAY(J+1) = -ARRAY(J+1)*ONEVOL
150     CONTINUE
        RETURN
C
C
C********      FOWARD REAL TRANSFORM COMES HERE       ******************
C
C
C  SET UP FOR TRANSFORM
C
30      NXP2 = NX + 2
        NXT = NXP2*NY
        IDIM(1) = NXT
        IDIM(2) = 2
        IDIM(3) = IDIM(1)
        IDIM(4) = IDIM(1)
        IDIM(5) = NXP2
C
        CALL REALFT(ARRAY(1),ARRAY(2),NXO2,IDIM)
C
C    NORMALIZE DATA
C
        DO 200 J = 1,NXT
          ARRAY(J) = ARRAY(J)*ONEVOL
200     CONTINUE
C
        RETURN

C
C********      INVERSE HERMITE TRANSFORM COMES HERE       ******************
C
C
C  SET UP FOR TRANSFORM
C
40      NXP2 = NX + 2
        NXT = NXP2*NY
        NXM1 = NX - 1
        IDIM(1) = NXT
        IDIM(2) = 2
        IDIM(3) = IDIM(1)
        IDIM(4) = IDIM(1)
        IDIM(5) = NXP2
C
C   NORMALIZE DATA
C
        DO 300 J = 1,NXT
          ARRAY(J) = ONEVOL*ARRAY(J)
300     CONTINUE
C
C  CHANGE DATA STORAGE MODE
C
        INDEX = 2
        DO 350 IY = 1,NY
          ARRAY(INDEX) = ARRAY(NXM1 + INDEX)
          INDEX = INDEX + NXP2
350     CONTINUE
C
        CALL HERMFT(ARRAY(1),ARRAY(2),NXO2,IDIM)
C
        RETURN
        END
C
C*TODFFT.FOR********************************************************
C
C       TWO-DIMENSIONAL FOURIER TRANSFORM SUBROUTINE FOR IMAGE
C       PROCESSING. DOES BOTH FOWARD & INVERSE TRANSFORMS
C       USES LYNN TENEYCK'S MIXED-RADIX ROUTINES
C       THUS THE ONLY RESTRICTION IS THAT THE IMAGE SIZE BE
C       AN EVEN NUMBER AND HAVE NO PRIME FACTORS LARGER THAN 19!!
C
C       IDIR =  0       FOWARD  TRANSFORM  :  exp(+2PIirs)
C       IDIR =  1       INVERSE TRANSFORM  :  exp(-2PIirs)
C       IDIR = -1       INVERSE TRANSFORM BUT NO COMPLEX CONJUGATE
C
C       DATA SET UP AS NY ROWS OF NX NUMBERS
C       NOTE NX,NY ALWAYS REFER TO THE REAL-SPACE IMAGE SIZE
C
C       NX,NY IMAGE IS TRANSFORMED IN-PLACE INTO NY STRIPS OF
C       NX/2 + 1 COMPLEX FOURIER COEFFICIENTS
C       THE ORIGIN IS LOCATED AT THE FIRST POINT!!!
C
C       ARRAY MUST BE DIMENSIONED TO ALLOW FOR EXTRA STRIP OF COMPLEX
C       NUMBERS ON OUTPUT.
C       THUS FOR A 300X400 TRANSFORM, ARRAY MUST BE DIMENSIONED:
C       REAL ARRAY(302,400)
C
C       A NORMALIZATION FACTOR IS APPLIED DURING BOTH  TRANSFORMATIONS
C
C       VERSION 1.00    OCT 11 1981             DAA
C       VERSION 1.02    APR 19 1982             DAA
C       VERSION 1.03    NOV 23 1982             DAA
C       Version 1.04    Mar 17 1998             JMS
C
        SUBROUTINE TDXFFT(ARRAY,NX,NY,IDIR)
        DIMENSION ARRAY(*),IDIM(5)
C
        write(6,'(''In TDXFFT from ifftlib.for'')')
        NXO2 = NX/2
        IF (2*NXO2 .EQ. NX) GOTO 1
        WRITE(6,1000) NX
1000    FORMAT(' TDXFFT: NX= ',I7,' MUST BE EVEN!!!')
        STOP
1       NXP2 = NX + 2
        NXT = NXP2*NY
        NXT1 = NXT - 1
        ONEVOL = SQRT(1.0/(NX*NY))
        IF (IDIR .NE. 0) GOTO 50
C
C********      FOWARD TRANSFORMS COME HERE       ******************
C
C
C  SET UP FOR FIRST DIMENSION OF TRANSFORM
C
        IDIM(1) = NXP2*NY
        IDIM(2) = 2
        IDIM(3) = IDIM(1)
        IDIM(4) = IDIM(1)
        IDIM(5) = NXP2
C
        CALL REALFT(ARRAY(1),ARRAY(2),NXO2,IDIM)
C
C  SET UP FOR SECOND DIMENSION OF TRANSFORM
C
        IDIM(2) = NXP2
        IDIM(4) = IDIM(2)
        IDIM(5) = 2
C
        CALL CMPLFT(ARRAY(1),ARRAY(2),NY,IDIM)
C
C   TAKE COMPLEX CONJUGATE (TO MAKE PROPER FOWARD TRANSFORM)& SCALE BY 1/VOLUME
C
        DO 100 J = 1,NXT1,2
          ARRAY(J) = ARRAY(J)*ONEVOL
          ARRAY(J+1) = -ARRAY(J+1)*ONEVOL
100     CONTINUE
C
        RETURN
C
C**********        INVERSE TRANSFORM     *******************
C
C
C  SET UP FOR FIRST DIMENSION OF TRANSFORM
C
50      NXM1 = NX - 1
        IDIM(1) = NXP2*NY
        IDIM(2) = NXP2
        IDIM(3) = IDIM(1)
        IDIM(4) = IDIM(2)
        IDIM(5) = 2
C
C   TAKE COMPLEX CONJUGATE TO DO INVERSE & SCALE BY 1/VOLUME
C
        IF (IDIR .EQ. 1) GOTO 60
        DO 300 J = 1,NXT1,2
          ARRAY(J) = ARRAY(J)*ONEVOL
          ARRAY(J+1) = -ARRAY(J+1)*ONEVOL
300     CONTINUE
        GOTO 70
C
C   IDIR = 1 JUST SCALE BY 1/VOLUME (FOR STANDARD INVERSE TRANSFORM)
C
60      DO 400 J = 1,NXT1,2
          ARRAY(J) = ARRAY(J)*ONEVOL
          ARRAY(J+1) = ARRAY(J+1)*ONEVOL
400     CONTINUE
C
70      CALL CMPLFT(ARRAY(1),ARRAY(2),NY,IDIM)
C
C  SET UP FOR SECOND DIMENSION OF TRANSFORM
C
        IDIM(2) = 2
        IDIM(4) = IDIM(1)
        IDIM(5) = NXP2
C
C    CHANGE DATA STORAGE MODE COMPLEX CONJUGATE DONE BY HERMFT
C
        INDEX = 2
        DO 500 IY = 1,NY
          ARRAY(INDEX) = ARRAY(NXM1 + INDEX)
          INDEX = INDEX + NXP2
500     CONTINUE
C
        CALL HERMFT(ARRAY(1),ARRAY(2),NXO2,IDIM)
C
        RETURN
        END

C*BIGFFT.FOR*************************************************************
C                                                                       *
C         This subroutine will do foward and inverse 2-D FFT's using    *
C       the Lynn Ten Eyck FFT subroutines - on very large images.       *
C       Arbitray sized transforms having prime factors not larger       *
C       than 19 can be accomidated.                                     *
C                                                                       *
C         All necessary file set uo for In/Out units must be            *
C       done by the calling program!!!                                  *
C                                                                       *
C       A scratch file is created to handle the required                *
C       transpositions. Two transpositions are done so that the final   *
C       transform is directly compatible with the in-core routines.     *
C                                                                       *
C                                                                       *
C         The buffer space is available thru the common block /FTBUF/   *
C                                                                       *
C       Fourier Transform MUST ALWAYS be complex reals!!                *
C                                                                       *
C                                                                       *
C       Last Update:    23.July.1982            DAA      for VAX        *
C       Last Update:    17.Mar.1998             JMS      array subscripts
C                                                                       *
C                                                                       *
C************************************************************************
C
C*GIANTFFT.FOR***********************************************************
C                                                                       *
C         This subroutine will do forward and inverse 2-D FFT's using   *
C       the Lynn Ten Eyck FFT subroutines - on very large images.       *
C       Arbitrary sized transforms having prime factors not larger      *
C       than 19 can be accommodated.                                    *
C                                                                       *
C       All necessary file set up for In/Out units must be              *
C       done by the calling program!!!                                  *
C                                                                       *
C       A scratch file is not required. Two transpositions are done     *
C       so that the final transform is directly compatible with the     *
C       in-core routines. All transpositions and temporary outputs      *
C       are done using the output file as the intermediate scratch      *
C       area.                                                           *
C                                                                       *
C                                                                       *
C       The buffer size and space is in the common block /FTBUF/        *
C                                                                       *
C       Fourier Transform MUST ALWAYS be complex reals!!                *
C                                                                       *
C                                                                       *
C       Last Update:    23.July.1982            DAA     for VAX         *
C                       15.December.1984        RH      for VAX         *
C                       23.March.1985           RH      for VAX         *
C                       28.May.87               RH      Real*8 DMEAN    *
C                       04.June.87              RH      parameter=16984 *
C                       25.January.95           RH      remove COMMON   *
C               VX1.7   09.March.95             RH      debug DDMEAN    *
C               VX1.8   25.August.95            RH    tiny memory debug *
C               VX1.9   17.March.98             JMS    array subscripts *
C                                                                       *
C************************************************************************
C
C*FFT1D.FR***********************************************
C
C     SIMPLE (NON-OPTIMIZED) SUBROUTINE FOR DOING
C     1-DIMENSIONAL FFT'S
C     IDIR = 0   FOWARD  TRANSFORM
C     IDIR = 1   INVERSE TRANSFORM
C
      SUBROUTINE FFT1D(A,N,IDIR)
      COMPLEX A(1),U,W,T
      DATA ALOG2/0.69314718/,PI/3.141592653/
C
      NV2 = N/2
      NM1 = N - 1
      J = 1
      LOG2N = ALOG(FLOAT(N))/ALOG2 + .1
      DO 100 I = 1,NM1
        IF (I .GE. J) GOTO 5
        T = A(J)
        A(J) = A(I)
        A(I) = T
5       K = NV2
6       IF (K .GE. J) GOTO 7
        J = J - K
        K = K/2
        GOTO 6
7       J = J + K
100   CONTINUE
C
      API = PI
      IF (IDIR .EQ. 1) API = -PI
      LE = 1
      DO 400 L = 1,LOG2N
        LE1 = LE
        LE = LE*2
        U = (1.0,0.)
        PL1 = API/LE1
        W = CMPLX(COS(PL1),SIN(PL1))
        DO 300 J = 1,LE1
          DO 200 I = J,N,LE
            IP = I + LE1
            T = A(IP)*U
            A(IP) = A(I) - T
            A(I) = A(I) + T
200       CONTINUE
          U = U*W
300     CONTINUE
400   CONTINUE
C
      RETURN
      END

      SUBROUTINE HERM FT(X, Y, N, DIM)
      INTEGER N
      INTEGER DIM(1)
      REAL X(1), Y(1)
C
C     HERMITIAN SYMMETRIC FOURIER TRANSFORM
C
C     GIVEN THE UNIQUE TERMS OF A HERMITIAN SYMMETRIC SEQUENCE OF LENGTH
C     2N THIS SUBROUTINE CALCULATES THE 2N REAL NUMBERS WHICH ARE ITS
C     FOURIER TRANSFORM.  THE EVEN NUMBERED ELEMENTS OF THE TRANSFORM
C     (0, 2, 4, . . ., 2N-2) ARE RETURNED IN X AND THE ODD NUMBERED
C     ELEMENTS (1, 3, 5, . . ., 2N-1) IN Y.
C
C     A FINITE HERMITIAN SEQUENCE OF LENGTH 2N CONTAINS N + 1 UNIQUE
C     REAL NUMBERS AND N - 1 UNIQUE IMAGINARY NUMBERS.  FOR CONVENIENCE
C     THE REAL VALUE FOR X(N) IS STORED AT Y(0).
C
      REAL A, B, C, D, E, F, ANGLE, CO, SI, TWO N, TWO PI
      INTEGER NT, D2, D3, D4, D5
      INTEGER I, J, K, N OVER 2, I0, I1, I2
      INTEGER K1
C
      TWO PI = 6.283185
      TWO N = FLOAT(2*N)
C
      NT = DIM(1)
      D2 = DIM(2)
      D3 = DIM(3)
      D4 = DIM(4) - 1
      D5 = DIM(5)
C
      DO 100 I0 = 1, NT, D3
      I1 = I0 + D4
      DO 100 I = I0, I1, D5
      A = X(I)
      B = Y(I)
      X(I) = A + B
      Y(I) = A - B
  100 CONTINUE
C
      N OVER 2 = N/2 + 1
      IF (N OVER 2 .LT. 2) GO TO 500
      DO 400 I0 = 2, N OVER 2
      ANGLE = TWO PI*FLOAT(I0-1)/TWO N
      CO = COS(ANGLE)
      SI = SIN(ANGLE)
      K = (N + 2 - 2*I0)*D2
      K1 = (I0 - 1)*D2 + 1
      DO 300 I1 = K1, NT, D3
      I2 = I1 + D4
      DO 200 I = I1, I2, D5
      J = I + K
      A = X(I) + X(J)
      B = X(I) - X(J)
      C = Y(I) + Y(J)
      D = Y(I) - Y(J)
      E = B*CO + C*SI
      F = B*SI - C*CO
      X(I) = A + F
      X(J) = A - F
      Y(I) = E + D
      Y(J) = E - D
  200 CONTINUE
  300 CONTINUE
  400 CONTINUE
C
      CALL CMPL FT (X, Y, N, DIM)
C
  500 CONTINUE
      RETURN
C
      END
      SUBROUTINE REAL FT (EVEN, ODD, N, DIM)
      INTEGER N
      INTEGER DIM(1)
      REAL EVEN(1), ODD(1)
C
C     REAL FOURIER TRANSFORM
C
C     GIVEN A REAL SEQUENCE OF LENGTH 2N THIS SUBROUTINE CALCULATES THE
C     UNIQUE PART OF THE FOURIER TRANSFORM.  THE FOURIER TRANSFORM HAS
C     N + 1 UNIQUE REAL PARTS AND N - 1 UNIQUE IMAGINARY PARTS.  SINCE
C     THE REAL PART AT X(N) IS FREQUENTLY OF INTEREST, THIS SUBROUTINE
C     STORES IT AT X(N) RATHER THAN IN Y(0).  THEREFORE X AND Y MUST BE
C     OF LENGTH N + 1 INSTEAD OF N.  NOTE THAT THIS STORAGE ARRANGEMENT
C     IS DIFFERENT FROM THAT EMPLOYED BY THE HERMITIAN FOURIER TRANSFORM
C     SUBROUTINE.
C
C     FOR CONVENIENCE THE DATA IS PRESENTED IN TWO PARTS, THE FIRST
C     CONTAINING THE EVEN NUMBERED REAL TERMS AND THE SECOND CONTAINING
C     THE ODD NUMBERED TERMS (NUMBERING STARTING AT 0).  ON RETURN THE
C     REAL PART OF THE TRANSFORM REPLACES THE EVEN TERMS AND THE
C     IMAGINARY PART OF THE TRANSFORM REPLACES THE ODD TERMS.
C
      REAL A, B, C, D, E, F, ANGLE, CO, SI, TWO PI, TWO N
      INTEGER NT, D2, D3, D4, D5
      INTEGER I, J, K, L, N OVER 2, I0, I1, I2
C
      TWO PI = 6.283185
      TWO N = FLOAT(2*N)
C
      CALL CMPL FT (EVEN, ODD, N, DIM)
C
      NT = DIM(1)
      D2 = DIM(2)
      D3 = DIM(3)
      D4 = DIM(4) - 1
      D5 = DIM(5)
      N OVER 2 = N/2 + 1
C
      IF (N OVER 2 .LT. 2) GO TO 400
      DO 300 I = 2, N OVER 2
      ANGLE = TWO PI*FLOAT(I-1)/TWO N
      CO = COS(ANGLE)
      SI = SIN(ANGLE)
      I0 = (I - 1)*D2 + 1
      J = (N + 2 - 2*I)*D2
      DO 200 I1 = I0, NT, D3
      I2 = I1 + D4
      DO 100 K = I1, I2, D5
      L = K + J
      A = (EVEN(L) + EVEN(K))/2.0
      C = (EVEN(L) - EVEN(K))/2.0
      B = (ODD(L) + ODD(K))/2.0
      D = (ODD(L) - ODD(K))/2.0
      E = C*SI + B*CO
      F = C*CO - B*SI
      EVEN(K) = A + E
      EVEN(L) = A - E
      ODD(K) = F - D
      ODD(L) = F + D
  100 CONTINUE
  200 CONTINUE
  300 CONTINUE
C
  400 CONTINUE
      IF (N .LT. 1) GO TO 600
      J = N*D2
      DO 500 I1 = 1, NT, D3
      I2 = I1 + D4
      DO 500 K = I1, I2, D5
      L = K + J
      EVEN(L) = EVEN(K) - ODD(K)
      ODD(L) = 0.0
      EVEN(K) = EVEN(K) + ODD(K)
      ODD(K) = 0.0
  500 CONTINUE
C
  600 CONTINUE
      RETURN
C
      END
      SUBROUTINE R SYM FT (X, N, DIM)
      INTEGER N
      INTEGER DIM(1)
      REAL X(1)
C
C     REAL SYMMETRIC MULTIDIMENSIONAL FOURIER TRANSFORM
C     N MUST BE A MULTIPLE OF 4.  THE TWO UNIQUE ELEMENTS ARE STORED AT
C     X(1) AND X(N+1).
C
C     DECIMATION IN FREQUENCY APPLIED TO A REAL SYMMETRIC SEQUENCE OF
C     LENGTH 2N GIVES A REAL SYMMETRIC SEQUENCE OF LENGTH N, THE
C     TRANSFORM OF WHICH GIVES THE EVEN NUMBERED FOURIER COEFFICIENTS,
C     AND A HERMITIAN SYMMETRIC SEQUENCE OF LENGTH N, THE TRANSFORM OF
C     WHICH GIVES THE ODD NUMBERED FOURIER COEFFICIENTS.  THE SUM OF
C     THE TWO SEQUENCES IS A HERMITIAN SYMMETRIC SEQUENCE OF LENGTH N,
C     WHICH MAY BE STORED IN N/2 COMPLEX LOCATIONS.  THE TRANSFORM OF
C     THIS SEQUENCE IS N REAL NUMBERS REPRESENTING THE TERM BY TERM SUM
C     OF THE EVEN AND ODD NUMBERED FOURIER COEFFICIENTS.  THIS SYMMETRIC
C     SEQUENCE MAY BE SOLVED IF ANY OF THE FOURIER COEFFICIENTS ARE
C     KNOWN.  FOR THIS PURPOSE X0, WHICH IS SIMPLY THE SUM OF THE
C     ORIGINAL SEQUENCE, IS COMPUTED AND SAVED IN X(N+1).
C
      REAL A, B, ANGLE, CO, SI, TWO N, TWO PI
      INTEGER I, J0, J1, J2, K, K0, K1, K2, L, MOD, NN, N2, TEST
      INTEGER N OVER 2, N OVER 4
      INTEGER D1, D2, D3, D4, D5
      INTEGER II, I0, I1, I2, J, M, MJ, MK, ML, MM, TWOD2
C
      IF (N .EQ. 1) GO TO 1300
      N OVER 2 = N/2
      N OVER 4 = N/4
      IF (4*N OVER 4 .NE. N) GO TO 1400
      D1 = DIM(1)
      D2 = DIM(2)
      D3 = DIM(3)
      D4 = DIM(4) - 1
      D5 = DIM(5)
      TWO PI = 6.283185
      TWO N = FLOAT(2*N)
      TWOD2 = 2*D2
C
      K0 = N*D2 + 1
      DO 100 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 100 K = K1, K2, D5
      X(K) = X(K)/2.0
  100 CONTINUE
C
      DO 300 I = 2, N OVER 2
      ANGLE = TWO PI*FLOAT(I-1)/TWO N
      CO = COS(ANGLE)
      SI = SIN(ANGLE)
      K0 = (I - 1)*D2 + 1
      J0 = (N + 2 - 2*I)*D2
      J1 = (N + 1 - I)*D2
      DO 200 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 200 K = K1, K2, D5
      L = K + J0
      NN = K + J1
      A = X(L) + X(K)
      B = X(L) - X(K)
      X(K) = A - B*CO
      X(L) = B*SI
      X(NN) = X(NN) + A
  200 CONTINUE
  300 CONTINUE
C
      IF (N OVER 4 .EQ. 1) GO TO 600
      J0 = N OVER 4 - 1
      DO 500 I = 1, J0
      K0 = (N OVER 2 + I)*D2 + 1
      J1 = (N OVER 2 - 2*I)*D2
      DO 400 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 400 K = K1, K2, D5
      L = K + J1
      A = X(K)
      X(K) = X(L)
      X(L) = A
  400 CONTINUE
  500 CONTINUE
C
  600 CONTINUE
      J0 = N OVER 2*D2
      J1 = N*D2
      DO 700 K1 = 1, D1, D3
      K2 = K1 + D4
      DO 700 K = K1, K2, D5
      I = K + J0
      L = K + J1
      X(I) = 2.0*X(I)
      X(L) = X(K) + X(I) + 2.0*X(L)
      X(K) = 2.0*X(K)
  700 CONTINUE
C
      K = N OVER 2*D2 + 1
      CALL HERM FT (X(1), X(K), N OVER 2, DIM)
C
C     SOLVE THE EQUATIONS FOR ALL OF THE SEQUENCES
C
      I0 = 1 - D2
      MK = N OVER 2*D2
      MJ = MK + D2
      ML = N*D2 + D2
      MM = ML
      DO 800 II = 1, N OVER 4
      I0 = I0 + D2
      MJ = MJ - TWOD2
      ML = ML - TWOD2
      MM = MM - D2
      DO 800 I1 = I0, D1, D3
      I2 = I1 + D4
      DO 800 I = I1, I2, D5
      J = I + MJ
      K = I + MK
      L = I + ML
      M = I + MM
      A = X(I) - X(M)
      B = X(L) - A
      C = X(K) - B
      D = X(J) - C
      X(I) = X(M)
      X(J) = A
      X(K) = B
      X(L) = C
      X(M) = D
  800 CONTINUE
C
C     THE RESULTS ARE NOW IN A SCRAMBLED DIGIT REVERSED ORDER, I.E.
C     X(1), X(5), X(9), ..., X(10), X(6), X(2), ..., X(3), X(7), X(11),
C     ..., X(12), X(8), X(4).  THE FOLLOWING SECTION OF PROGRAM FOLLOWS
C     THE PERMUTATION CYCLES AND DOES THE NECESSARY INTERCHANGES.
C
      IF (N OVER 4 .EQ. 1) GO TO 1300
      NN = N - 2
      DO 1200 I = 1, NN
      K = I
C
 1000 CONTINUE
      K0 = K/4
      L = K - K0*4
      IF (L .NE. (L/2)*2) K0 = N OVER 4 - 1 - K0
      K = K0 + L*N OVER 4
      IF (K .LT. I) GO TO 1000
      IF (K .EQ. I) GO TO 1200
C
      K0 = I*D2 + 1
      J0 = (K - I)*D2
      DO 1100 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 1100 K = K1, K2, D5
      L = K + J0
      A = X(K)
      X(K) = X(L)
      X(L) = A
 1100 CONTINUE
 1200 CONTINUE
C
 1300 CONTINUE
      RETURN
C
 1400 CONTINUE
      WRITE (6, 1500) N
      STOP
C
 1500 FORMAT (40H0N NOT A MULTIPLE OF 4 IN R SYM FT.  N =,I10//)
C
      END
      SUBROUTINE SDIAD (X, Y, N, DIM)
      INTEGER N
      INTEGER DIM(1)
      REAL X(1), Y(1)
C
C     THIS SUBROUTINE COMPUTES HALF THE FOURIER SYNTHESIS ALONG A SCREW
C     DIAD LYING ALONG A CRYSTALLOGRAPHIC AXIS GIVEN HALF THE FOURIER
C     COEFFICIENTS.  THAT IS, IT ASSUMES THAT F(T) = CONJG(F(-T)) FOR T
C     EVEN AND F(T) = -CONJG(F(-T)) FOR T ODD.  N IS THE LENGTH OF THE
C     DESIRED HALF OF THE TRANSFORM.  THE LOCATION X(N+1) IS REQUIRED AS
C     A SCRATCH LOCATION AND THEREFORE A VALUE IS ALSO RETURNED IN
C     X(N+1) AND Y(N+1).  THE VALUE OF THE SECOND HALF OF THE TRANSFORM
C     MAY BE GENERATED FROM THE FIRST HALF BY THE FORMULA X(N+T) = X(T),
C     Y(N+T) = -Y(T).  IN OTHER WORDS, THE LAST HALF OF THE TRANSFORM IS
C     THE COMPLEX CONJUGATE OF THE FIRST HALF.
C
C     THE TRANSFORM IS CALCULATED BY FORMING THE SUM OF THE EVEN TERMS
C     AND THE ODD TERMS IN PLACE, USING THE SYMMETRY RELATIONS TO
C     OBTAIN THE VALUES FOR NEGATIVE SUBSCRIPTS.  THE TRANSFORM OF THE
C     RESULTING SEQUENCE MAY BE SEPARATED BY USING THE FACT THAT THE
C     TRANSFORM OF THE EVEN TERMS IS REAL, WHILE THE PRODCT OF THE
C     TRANSFORM OF THE ODD TERMS AND (COS(PI*T/N) - I*SIN(PI*T/N)) IS
C     IMAGINARY.  THE SCRATCH LOCATION IS REQUIRED BECAUSE THE FORMULA
C     FOR SEPARATING THE TWO TRANSFORMS BREAKS DOWN WHEN T = N/2.
C
      LOGICAL FOLD
      REAL A, ANGLE, C, S, TWO N, TWO PI
      INTEGER D1, D2, D3, D4, D5
      INTEGER I, J, K, K0, K1, K2, L, M, MM, NN, N OVER 2
C
      N OVER 2 = N/2
      IF (2*N OVER 2 .NE. N) GO TO 700
      TWO N = FLOAT(2*N)
      TWO PI = 6.2831852
      D1 = DIM(1)
      D2 = DIM(2)
      D3 = DIM(3)
      D4 = DIM(4) - 1
      D5 = DIM(5)
C
      K0 = (N - 1)*D2 + 1
      DO 100 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 100 K = K1, K2, D5
      L = K + D2
      X(L) = X(K)
  100 CONTINUE
      S = 1.0
      NN = N - 2
      DO 200 I = 1, NN, 2
      S = -S
      MN = (N + 1 - I)*D2
      K0 = (I - 1)*D2 + 1
      DO 150 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 150 K = K1, K2, D5
      J = K + D2
      L = K + 2*D2
      M = K + MN
      X(M) = X(M) + S*X(J)
      X(K) = X(K) + X(J)
      X(J) = X(L) - X(J)
      Y(K) = Y(K) + Y(J)
      Y(J) = Y(J) - Y(L)
  150 CONTINUE
  200 CONTINUE
      K0 = (N - 2)*D2 + 1
      DO 250 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 250 K = K1, K2, D5
      L = K + D2
      X(K) = X(K) + X(L)
      Y(K) = Y(K) + Y(L)
      X(L) = -X(L)
  250 CONTINUE
C
C     REORDER SCRAMBLED FOURIER COEFFICIENTS
C
      DO 400 I = 1, NN
      K = I
  300 CONTINUE
      K = 2*K
      IF (K .GT. N - 1) K = 2*N - 1 - K
      IF (K .LT. I) GO TO 300
      IF (K .EQ. I) GO TO 400
      J = (K - I)*D2
      K0 = I*D2 + 1
      DO 350 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 350 K = K1, K2, D5
      L = K + J
      A = X(K)
      X(K) = X(L)
      X(L) = A
      A = Y(K)
      Y(K) = Y(L)
      Y(L) = A
  350 CONTINUE
  400 CONTINUE
C
      CALL CMPL FT (X, Y, N, DIM)
C
      M = N OVER 2 - 1
      DO 600 I = 1, M
      ANGLE = TWO PI*FLOAT(I)/TWO N
      C = COS(ANGLE)
      S = SIN(ANGLE)
      K0 = I*D2 + 1
      FOLD = .TRUE.
      GO TO 500
C
  450 CONTINUE
      C = -C
      K0 = (N - I)*D2 + 1
      FOLD = .FALSE.
C
  500 CONTINUE
      DO 550 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 550 K = K1, K2, D5
      A = Y(K)/C
      X(K) = X(K) + S*A
      Y(K) = A
  550 CONTINUE
      IF (FOLD) GO TO 450
  600 CONTINUE
C
      M = N OVER 2*D2
      K0 = M + 1
      DO 650 K1 = K0, D1, D3
      K2 = K1 + D4
      DO 650 K = K1, K2, D5
      J = K - M
      L = K + M
      A = 2.0*X(L)
      X(K) = X(K) + A
      Y(K) = A
      X(L) = X(J)
      Y(L) = -Y(J)
  650 CONTINUE
C
      RETURN
C
  700 CONTINUE
      WRITE (6, 1000) N
      STOP
C
 1000 FORMAT (18H0SDIAD N ODD.  N =, I10)
C
      END
      SUBROUTINE INV21(X,Y,N,D)
      INTEGER N,D(1)
      REAL X(1),Y(1)
C
C     INVERTS FOURIER TRANSFORM ALONG A SCREW
C     DIAD. THE RESULT IS SCALED BY N.
C
      INTEGER D1,D2,D3,D4,D5
      REAL A,B,C,C1,R,S,S1,PI
C
      PI = 3.141593
C
      D1 = D(1)
      D2 = D(2)
      D3 = D(3)
      D4 = D(4)-1
      D5 = D(5)
C
      NOVER2 = N/2
      LL = N*D2
      KK = NOVER2*D2
      DO 100 J1 = 1,D1,D3
      J2 = J1+D4
      DO 100 J = J1,J2,D5
      L = LL+J
      K = KK+J
      X(L) = X(J)+X(K)
      X(K) = X(K)+Y(K)
      Y(L) = 0.0
100   Y(K) = 0.0
C
      C1 = COS(PI/FLOAT(N))
      S1 = SIN(PI/FLOAT(N))
      C = 1.0
      S = 0.0
      DO 200 I = 2, NOVER2
      KK = (N+2-2*I)*D2
      LL = (N+1-I)*D2
      R = C*C1-S*S1
      S = C*S1+S*C1
      C = R
      J1 = (I-1)*D2+1
      DO 150 J2 = J1,D1,D3
      J3 = J2+D4
      DO 150 J = J2,J3,D5
      L = J+LL
      K = J+KK
      X(L) = X(L)+X(J)+X(K)
      X(J) = X(J)+S*Y(J)
      X(K) = X(K)+S*Y(K)
      Y(J) = C*Y(J)
150   Y(K) = -C*Y(K)
200   CONTINUE
C
      CALL CMPL FT(X,Y,N,D)
C
      DO 300 I = 1,NOVER2
      KK = (N+1-2*I)*D2
      LL = KK+I*D2
      J1 = (I-1)*D2+1
      DO 250 J2 = J1,D1,D3
      J3 = J2+D4
      DO 250 J = J2,J3,D5
      K = J+KK
      L = J+LL
      A = X(J)-X(L)
      B = Y(J)+Y(L)
      X(J) = X(L)
      Y(J) = -Y(L)
      X(L) = X(K)+A
      Y(L) = Y(K)-B
      X(K) = A
250   Y(K) = B
300   CONTINUE
C
      M = N-2
      DO 400 I = 1,M
      K = I
320   J = K
      K = J/2
      IF(2*K.NE.J) K = N-1-K
      IF(K-I) 320,400,340
340   KK = (K-I)*D2
      J1 = I*D2+1
      DO 360 J2 = J1,D1,D3
      J3 = J2+D4
      DO 360 J = J2,J3,D5
      K = J+KK
      A = X(K)
      B = Y(K)
      X(K) =X(J)
      Y(K) = Y(J)
      X(J) = A
360   Y(J) = B
400   CONTINUE
C
      RETURN
C
      END
      SUBROUTINE CMPL FT (X, Y, N, D)
      INTEGER N
      INTEGER D(1)
      REAL X(1), Y(1)
C
C     COMPLEX FINITE DISCRETE FOURIER TRANSFORM
C     TRANSFORMS ONE DIMENSION OF MULTI-DIMENSIONAL DATA
C     MODIFIED BY L. F. TEN EYCK FROM A ONE-DIMENSIONAL VERSION WRITTEN
C     BY G. T. SANDE, 1969.
C
C     THIS PROGRAM CALCULATES THE TRANSFORM
C               (X(T) + I*Y(T))*(COS(2*PI*T/N) - I*SIN(2*PI*T/N))
C
C     INDEXING -- THE ARRANGEMENT OF THE MULTI-DIMENSIONAL DATA IS
C     SPECIFIED BY THE INTEGER ARRAY D, THE VALUES OF WHICH ARE USED AS
C     CONTROL PARAMETERS IN DO LOOPS.  WHEN IT IS DESIRED TO COVER ALL
C     ELEMENTS OF THE DATA FOR WHICH THE SUBSCRIPT BEING TRANSFORMED HAS
C     THE VALUE I0, THE FOLLOWING IS USED.
C
C               I1 = (I0 - 1)*D(2) + 1
C               DO 100 I2 = I1, D(1), D(3)
C               I3 = I2 + D(4) - 1
C               DO 100 I = I2, I3, D(5)
C                  .
C                  .
C           100 CONTINUE
C
C     WITH THIS INDEXING IT IS POSSIBLE TO USE A NUMBER OF ARRANGEMENTS
C     OF THE DATA, INCLUDING NORMAL FORTRAN COMPLEX NUMBERS (D(5) = 2)
C     OR SEPARATE STORAGE OF REAL AND IMAGINARY PARTS.
C
      LOGICAL ERROR
      INTEGER P MAX, P SYM, TWO GRP
      INTEGER FACTOR(15), SYM(15), UN SYM(15)
C
C     P MAX IS THE LARGEST PRIME FACTOR THAT WILL BE TOLERATED BY THIS
C     PROGRAM.
C     TWO GRP IS THE LARGEST POWER OF TWO THAT IS TREATED AS A SPECIAL
C     CASE.
C
      P MAX = 19
      TWO GRP = 8
C
      IF (N .LE. 1) GO TO 100
      CALL S R FP (N, P MAX, TWO GRP, FACTOR, SYM, P SYM, UN SYM, ERROR)
      IF (ERROR) GO TO 200
C
      CALL MD FT KD (N, FACTOR, D, X, Y)
      CALL D IP RP (N, SYM, P SYM, UN SYM, D, X, Y)
C
  100 CONTINUE
      RETURN
C
  200 CONTINUE
      WRITE (6, 300) N
      STOP
C
  300 FORMAT ('INVALID NUMBER OF POINTS FOR CMPL FT.  N =',I10,/,/)
C
      END
      SUBROUTINE S R FP (PTS,PMAX,TWO GRP,FACTOR,SYM,P SYM,UN SYM,ERROR)
C     SYMMETRIZED REORDERING FACTORING PROGRAMME
C
      LOGICAL ERROR
      INTEGER PTS,PMAX,TWO GRP,P SYM
      INTEGER FACTOR (1), SYM (1), UN SYM (1)
C
      INTEGER PP(14), QQ (7)
      INTEGER F,J,JJ,N,NEST,P,P TWO,Q,R
C
      NEST=14
C
      N=PTS
      P SYM=1
      F=2
      P=0
      Q=0
  100 CONTINUE
      IF (N.LE.1) GO TO 500
      DO 200 J=F,PMAX
      IF (N.EQ.(N/J)*J) GO TO 300
  200 CONTINUE
      GO TO 1400
  300 CONTINUE
      IF (2*P+Q.GE.NEST) GO TO 1600
      F=J
      N=N/F
      IF (N.EQ.(N/F)*F) GO TO 400
      Q=Q+1
      QQ(Q)=F
      GO TO 100
  400 CONTINUE
      N=N/F
      P=P+1
      PP(P)=F
      P SYM=P SYM*F
      GO TO 100
C
  500 CONTINUE
      R=1
      IF (Q.EQ.0) R=0
      IF (P.LT.1) GO TO 700
      DO 600 J=1,P
      JJ=P+1-J
      SYM(J)=PP(JJ)
      FACTOR(J)=PP(JJ)
      JJ=P+Q+J
      FACTOR(JJ)=PP(J)
      JJ=P+R+J
      SYM(JJ)=PP(J)
  600 CONTINUE
  700 CONTINUE
      IF (Q.LT.1) GO TO 900
      DO 800 J=1,Q
      JJ=P+J
      UN SYM(J)=QQ(J)
      FACTOR(JJ)=QQ(J)
  800 CONTINUE
      SYM(P+1)=PTS/P SYM**2
  900 CONTINUE
      JJ=2*P+Q
      FACTOR(JJ+1)=0
      P TWO=1
      J=0
 1000 CONTINUE
      J=J+1
      IF (FACTOR(J).EQ.0) GO TO 1200
      IF (FACTOR(J).NE.2) GO TO 1000
      P TWO=P TWO*2
      FACTOR(J)=1
      IF (P TWO.GE.TWO GRP) GO TO 1100
      IF (FACTOR(J+1).EQ.2) GO TO 1000
 1100 CONTINUE
      FACTOR(J)=P TWO
      P TWO=1
      GO TO 1000
 1200 CONTINUE
      IF (P.EQ.0) R=0
      JJ=2*P+R
      SYM(JJ+1)=0
      IF (Q.LE.1) Q=0
      UN SYM(Q+1)=0
      ERROR=.FALSE.
C
 1300 CONTINUE
      RETURN
C
 1400 CONTINUE
      WRITE (6,1500) PMAX,PTS
      ERROR=.TRUE.
      GO TO 1300
 1500 FORMAT (24H0LARGEST FACTOR EXCEEDS ,I3,7H.  N = ,I6,1H.)
C
 1600 CONTINUE
      WRITE (6,1700) NEST,PTS
      ERROR=.TRUE.
      GO TO 1300
 1700 FORMAT (22H0FACTOR COUNT EXCEEDS ,I3,7H.  N = ,I6,1H.)
C
      END
      SUBROUTINE D IP RP (PTS,SYM,P SYM,UN SYM,DIM,X,Y)
C     DOUBLE IN PLACE REORDERING PROGRAMME
C
      INTEGER PTS,P SYM
      REAL X (1), Y (1)
      INTEGER SYM(1), UN SYM(1), DIM(1)
C
      REAL T
      LOGICAL ONE MOD
      INTEGER MODULO (14)
      INTEGER DK,JJ,KK,LK,MODS,MULT,NEST,P UN SYM,TEST
      INTEGER NT, SEP, DELTA, P, P0, P1, P2, P3, P4, P5, SIZE
C
      INTEGER S (14), U (14)
      INTEGER A,B,C,D,E,F,G,H,I,J,K,L,M,N
      INTEGER BS,CS,DS,ES,FS,GS,HS,IS,JS,KS,LS,MS,NS
      INTEGER AL,BL,CL,DL,EL,FL,GL,HL,IL,JL,KL,LL,ML,NL
      EQUIVALENCE           (AL,U(1)),(BS,S(2)),(BL,U(2))
      EQUIVALENCE (CS,S(3)),(CL,U(3)),(DS,S(4)),(DL,U(4))
      EQUIVALENCE (ES,S(5)),(EL,U(5)),(FS,S(6)),(FL,U(6))
      EQUIVALENCE (GS,S(7)),(GL,U(7)),(HS,S(8)),(HL,U(8))
      EQUIVALENCE (IS,S(9)),(IL,U(9)),(JS,S(10)),(JL,U(10))
      EQUIVALENCE (KS,S(11)),(KL,U(11)),(LS,S(12)),(LL,U(12))
      EQUIVALENCE (MS,S(13)),(ML,U(13)),(NS,S(14)),(NL,U(14))
C
      NEST=14
C
      NT = DIM(1)
      SEP = DIM(2)
      P2 = DIM(3)
      SIZE = DIM(4) - 1
      P4 = DIM(5)
      IF (SYM(1).EQ.0) GO TO 500
      DO 100 J=1,NEST
      U(J)=1
      S(J)=1
  100 CONTINUE
      N=PTS
      DO 200 J=1,NEST
      IF (SYM(J).EQ.0) GO TO 300
      JJ=NEST+1-J
      U(JJ)=N
      S(JJ)=N/SYM(J)
      N=N/SYM(J)
  200 CONTINUE
  300 CONTINUE
C
      JJ=0
      DO 400 A=1,AL
      DO 400 B=A,BL,BS
      DO 400 C=B,CL,CS
      DO 400 D=C,DL,DS
      DO 400 E=D,EL,ES
      DO 400 F=E,FL,FS
      DO 400 G=F,GL,GS
      DO 400 H=G,HL,HS
      DO 400 I=H,IL,IS
      DO 400 J=I,JL,JS
      DO 400 K=J,KL,KS
      DO 400 L=K,LL,LS
      DO 400 M=L,ML,MS
      DO 400 N=M,NL,NS
      JJ=JJ+1
      IF (JJ.GE.N) GO TO 400
      DELTA = (N-JJ)*SEP
      P1 = (JJ-1)*SEP + 1
      DO 350 P0 = P1, NT, P2
      P3 = P0 + SIZE
      DO 350 P = P0, P3, P4
      P5 = P + DELTA
      T = X(P)
      X(P) = X(P5)
      X(P5) = T
      T = Y(P)
      Y(P) = Y(P5)
      Y(P5) = T
  350 CONTINUE
  400 CONTINUE
C
  500 CONTINUE
      IF (UN SYM(1).EQ.0) GO TO 1900
      P UN SYM=PTS/P SYM**2
      MULT=P UN SYM/UN SYM(1)
      TEST=(UN SYM(1)*UN SYM(2)-1)*MULT*P SYM
      LK=MULT
      DK=MULT
      DO 600 K=2,NEST
      IF (UN SYM(K).EQ.0) GO TO 700
      LK=LK*UN SYM(K-1)
      DK=DK/UN SYM(K)
      U(K)=(LK-DK)*P SYM
      MODS=K
  600 CONTINUE
  700 CONTINUE
      ONE MOD=MODS.LT.3
      IF (ONE MOD) GO TO 900
      DO 800 J=3,MODS
      JJ=MODS+3-J
      MODULO(JJ)=U(J)
  800 CONTINUE
  900 CONTINUE
      MODULO(2)=U(2)
      JL=(P UN SYM-3)*P SYM
      MS=P UN SYM*P SYM
C
      DO 1800 J=P SYM,JL,P SYM
      K=J
C
 1000 CONTINUE
      K=K*MULT
      IF (ONE MOD) GO TO 1200
      DO 1100 I=3,MODS
      K=K-(K/MODULO(I))*MODULO(I)
 1100 CONTINUE
 1200 CONTINUE
      IF (K.GE.TEST) GO TO 1300
      K=K-(K/MODULO(2))*MODULO(2)
      GO TO 1400
 1300 CONTINUE
      K=K-(K/MODULO(2))*MODULO(2)+MODULO(2)
 1400 CONTINUE
      IF (K.LT.J) GO TO 1000
C
      IF (K.EQ.J) GO TO 1700
      DELTA = (K-J)*SEP
      DO 1600 L=1,P SYM
      DO 1500 M=L,PTS,MS
      P1 = (M+J-1)*SEP + 1
      DO 1500 P0 = P1, NT, P2
      P3 = P0 + SIZE
      DO 1500 JJ = P0, P3, P4
      KK = JJ + DELTA
      T=X(JJ)
      X(JJ)=X(KK)
      X(KK)=T
      T=Y(JJ)
      Y(JJ)=Y(KK)
      Y(KK)=T
 1500 CONTINUE
 1600 CONTINUE
 1700 CONTINUE
 1800 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE MD FT KD (N, FACTOR, DIM, X, Y)
C     MULTI-DIMENSIONAL COMPLEX FOURIER TRANSFORM KERNEL DRIVER
C
      INTEGER N
      INTEGER FACTOR (1), DIM(1)
      REAL X(1), Y(1)
C
      INTEGER F, M, P, R, S
C
      S = DIM(2)
      F = 0
      M = N
  100 CONTINUE
      F = F + 1
      P = FACTOR(F)
      IF (P.EQ.0) RETURN
      M = M/P
      R = M*S
      IF (P.GT.8) GO TO 700
      GO TO (100, 200, 300, 400, 500, 800, 700, 600), P
      GO TO 800
C
  200 CONTINUE
      CALL R2 CFTK (N, M, X(1), Y(1), X(R+1), Y(R+1), DIM)
      GO TO 100
C
  300 CONTINUE
      CALL R3 CFTK (N, M, X(1), Y(1), X(R+1), Y(R+1), X(2*R+1), Y(2*R+1)
     ., DIM)
      GO TO 100
C
  400 CONTINUE
      CALL R4 CFTK (N, M, X(1), Y(1), X(R+1), Y(R+1), X(2*R+1), Y(2*R+1)
     ., X(3*R+1), Y(3*R+1), DIM)
      GO TO 100
C
  500 CONTINUE
      CALL R5 CFTK (N, M, X(1), Y(1), X(R+1), Y(R+1), X(2*R+1), Y(2*R+1)
     ., X(3*R+1), Y(3*R+1), X(4*R+1), Y(4*R+1), DIM)
      GO TO 100
C
  600 CONTINUE
      CALL R8 CFTK (N, M, X(1), Y(1), X(R+1), Y(R+1), X(2*R+1), Y(2*R+1)
     ., X(3*R+1), Y(3*R+1), X(4*R+1), Y(4*R+1), X(5*R+1), Y(5*R+1),
     .X(6*R+1), Y(6*R+1), X(7*R+1), Y(7*R+1), DIM)
      GO TO 100
C
  700 CONTINUE
      CALL RP CFTK (N, M, P, R, X, Y, DIM)
      GO TO 100
C
  800 CONTINUE
      WRITE (6, 900)
      RETURN
  900 FORMAT (34H0TRANSFER ERROR DETECTED IN MDFTKD,//)
      END
      SUBROUTINE R2 CFTK (N, M, X0, Y0, X1, Y1, DIM)
C     RADIX 2 MULTI-DIMENSIONAL COMPLEX FOURIER TRANSFORM KERNEL
C
      INTEGER N, M, DIM(1)
      REAL X0 (1), Y0 (1), X1 (1), Y1 (1)
C
      LOGICAL FOLD,ZERO
      INTEGER J,K,K0,M2,M OVER 2
      INTEGER K1, K2, KK, L, L1, MM2, NT, SIZE, SEP
      INTEGER NS
      REAL ANGLE,C,IS,IU,RS,RU,S,TWOPI
      REAL FJM1, FM2
      DATA TWO PI/6.283185/
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M2=M*2
      FM2 = FLOAT(M2)
      M OVER 2=M/2+1
      MM2 = SEP*M2
C
      FJM1 = -1.0
      DO 600 J=1,M OVER 2
      FOLD=J.GT.1 .AND. 2*J.LT.M+2
      K0 = (J-1)*SEP + 1
      FJM1 = FJM1 + 1.0
      ANGLE = TWO PI*FJM1/FM2
      ZERO=ANGLE.EQ.0.0
      IF (ZERO) GO TO 200
      C=COS(ANGLE)
      S=SIN(ANGLE)
      GO TO 200
  100 CONTINUE
      FOLD=.FALSE.
      K0 = (M+1-J)*SEP + 1
      C=-C
  200 CONTINUE
C
      DO 500 KK = K0, NS, MM2
      DO 440 L = KK, NT, L1
      K1 = L + SIZE
      DO 420 K = L, K1, K2
      RS=X0(K)+X1(K)
      IS=Y0(K)+Y1(K)
      RU=X0(K)-X1(K)
      IU=Y0(K)-Y1(K)
      X0(K)=RS
      Y0(K)=IS
      IF (ZERO) GO TO 300
      X1(K)=RU*C+IU*S
      Y1(K)=IU*C-RU*S
      GO TO 400
  300 CONTINUE
      X1(K)=RU
      Y1(K)=IU
  400 CONTINUE
  420 CONTINUE
  440 CONTINUE
  500 CONTINUE
      IF (FOLD) GO TO 100
  600 CONTINUE
C
      RETURN
      END
      SUBROUTINE R3 CFTK (N, M, X0, Y0, X1, Y1, X2, Y2, DIM)
C     RADIX 3 MULTI-DIMENSIONAL COMPLEX FOURIER TRANSFORM KERNEL
C
      INTEGER N, M, DIM(1)
      REAL X0 (1), Y0 (1), X1 (1), Y1 (1), X2 (1), Y2 (1)
C
      LOGICAL FOLD,ZERO
      INTEGER J,K,K0,M3,M OVER 2
      INTEGER K1, K2, KK, L, L1, MM3, NT, SIZE, SEP
      INTEGER NS
      REAL ANGLE,A,B,C1,C2,S1,S2,T,TWOPI
      REAL I0,I1,I2,IA,IB,IS,R0,R1,R2,RA,RB,RS
      REAL FJM1, FM3
      DATA TWO PI/6.283185/, A/-0.5/, B/0.86602540/
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M3=M*3
      FM3 = FLOAT(M3)
      MM3 = SEP*M3
      M OVER 2=M/2+1
C
      FJM1 = -1.0
      DO 600 J=1,M OVER 2
      FOLD=J.GT.1 .AND. 2*J.LT.M+2
      K0 = (J-1)*SEP + 1
      FJM1 = FJM1 + 1.0
      ANGLE = TWO PI*FJM1/FM3
      ZERO=ANGLE.EQ.0.0
      IF (ZERO) GO TO 200
      C1=COS(ANGLE)
      S1=SIN(ANGLE)
      C2=C1*C1-S1*S1
      S2=S1*C1+C1*S1
      GO TO 200
  100 CONTINUE
      FOLD=.FALSE.
      K0 = (M+1-J)*SEP + 1
      T=C1*A+S1*B
      S1=C1*B-S1*A
      C1=T
      T=C2*A-S2*B
      S2=-C2*B-S2*A
      C2=T
  200 CONTINUE
C
      DO 500 KK = K0, NS, MM3
      DO 440 L = KK, NT, L1
      K1 = L + SIZE
      DO 420 K = L, K1, K2
      R0=X0(K)
      I0=Y0(K)
      RS=X1(K)+X2(K)
      IS=Y1(K)+Y2(K)
      X0(K)=R0+RS
      Y0(K)=I0+IS
      RA=R0+RS*A
      IA=I0+IS*A
      RB=(X1(K)-X2(K))*B
      IB=(Y1(K)-Y2(K))*B
      IF (ZERO) GO TO 300
      R1=RA+IB
      I1=IA-RB
      R2=RA-IB
      I2=IA+RB
      X1(K)=R1*C1+I1*S1
      Y1(K)=I1*C1-R1*S1
      X2(K)=R2*C2+I2*S2
      Y2(K)=I2*C2-R2*S2
      GO TO 400
  300 CONTINUE
      X1(K)=RA+IB
      Y1(K)=IA-RB
      X2(K)=RA-IB
      Y2(K)=IA+RB
  400 CONTINUE
  420 CONTINUE
  440 CONTINUE
  500 CONTINUE
      IF (FOLD) GO TO 100
  600 CONTINUE
C
      RETURN
      END
      SUBROUTINE R4 CFTK (N, M, X0, Y0, X1, Y1, X2, Y2, X3, Y3, DIM)
C     RADIX 4 MULTI-DIMENSIONAL COMPLEX FOURIER TRANSFORM KERNEL
C
      INTEGER N, M, DIM(1)
      REAL X0 (1), Y0 (1), X1 (1), Y1 (1)
      REAL X2 (1), Y2 (1), X3 (1), Y3 (1)
C
      LOGICAL FOLD,ZERO
      INTEGER J,K,K0,M4,M OVER 2
      INTEGER K1, K2, KK, L, L1, MM4, NT, SIZE, SEP
      INTEGER NS
      REAL ANGLE,C1,C2,C3,S1,S2,S3,T,TWOPI
      REAL I1,I2,I3,IS0,IS1,IU0,IU1,R1,R2,R3,RS0,RS1,RU0,RU1
      REAL FJM1, FM4
      DATA TWO PI/6.283185/
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M4=M*4
      FM4 = FLOAT(M4)
      MM4 = SEP*M4
      M OVER 2=M/2+1
C
      FJM1 = -1.0
      DO 600 J=1,M OVER 2
      FOLD=J.GT.1 .AND. 2*J.LT.M+2
      K0 = (J-1)*SEP + 1
      FJM1 = FJM1 + 1.0
      ANGLE = TWO PI*FJM1/FM4
      ZERO=ANGLE.EQ.0.0
      IF (ZERO) GO TO 200
      C1=COS(ANGLE)
      S1=SIN(ANGLE)
      C2=C1*C1-S1*S1
      S2=S1*C1+C1*S1
      C3=C2*C1-S2*S1
      S3=S2*C1+C2*S1
      GO TO 200
  100 CONTINUE
      FOLD=.FALSE.
      K0 = (M+1-J)*SEP + 1
      T=C1
      C1=S1
      S1=T
      C2=-C2
      T=C3
      C3=-S3
      S3=-T
  200 CONTINUE
C
      DO 500 KK = K0, NS, MM4
      DO 440 L = KK, NT, L1
      K1 = L + SIZE
      DO 420 K = L, K1, K2
      RS0=X0(K)+X2(K)
      IS0=Y0(K)+Y2(K)
      RU0=X0(K)-X2(K)
      IU0=Y0(K)-Y2(K)
      RS1=X1(K)+X3(K)
      IS1=Y1(K)+Y3(K)
      RU1=X1(K)-X3(K)
      IU1=Y1(K)-Y3(K)
      X0(K)=RS0+RS1
      Y0(K)=IS0+IS1
      IF (ZERO) GO TO 300
      R1=RU0+IU1
      I1=IU0-RU1
      R2=RS0-RS1
      I2=IS0-IS1
      R3=RU0-IU1
      I3=IU0+RU1
      X2(K)=R1*C1+I1*S1
      Y2(K)=I1*C1-R1*S1
      X1(K)=R2*C2+I2*S2
      Y1(K)=I2*C2-R2*S2
      X3(K)=R3*C3+I3*S3
      Y3(K)=I3*C3-R3*S3
      GO TO 400
  300 CONTINUE
      X2(K)=RU0+IU1
      Y2(K)=IU0-RU1
      X1(K)=RS0-RS1
      Y1(K)=IS0-IS1
      X3(K)=RU0-IU1
      Y3(K)=IU0+RU1
  400 CONTINUE
  420 CONTINUE
  440 CONTINUE
  500 CONTINUE
      IF (FOLD) GO TO 100
  600 CONTINUE
C
      RETURN
      END
      SUBROUTINE R5 CFTK (N, M, X0, Y0, X1, Y1, X2, Y2, X3, Y3, X4, Y4,
     . DIM)
C     RADIX 5 MULTI-DIMENSIONAL COMPLEX FOURIER TRANSFORM KERNEL
C
      INTEGER N, M, DIM(1)
      REAL X0 (1), Y0 (1), X1 (1), Y1 (1), X2 (1), Y2 (1)
      REAL X3 (1), Y3 (1), X4 (1), Y4 (1)
C
      LOGICAL FOLD,ZERO
      INTEGER J,K,K0,M5,M OVER 2
      INTEGER K1, K2, KK, L, L1, MM5, NT, SIZE, SEP
      INTEGER NS
      REAL ANGLE,A1,A2,B1,B2,C1,C2,C3,C4,S1,S2,S3,S4,T,TWOPI
      REAL R0,R1,R2,R3,R4,RA1,RA2,RB1,RB2,RS1,RS2,RU1,RU2
      REAL I0,I1,I2,I3,I4,IA1,IA2,IB1,IB2,IS1,IS2,IU1,IU2
      REAL FJM1, FM5
      DATA TWO PI/6.283185/, A1/0.30901699/, B1/0.95105652/,
     .      A2/-0.80901699/, B2/0.58778525/
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M5=M*5
      FM5 = FLOAT(M5)
      MM5 = SEP*M5
      M OVER 2=M/2+1
C
      FJM1 = -1.0
      DO 600 J=1,M OVER 2
      FOLD=J.GT.1 .AND. 2*J.LT.M+2
      K0 = (J-1)*SEP + 1
      FJM1 = FJM1 + 1.0
      ANGLE = TWO PI*FJM1/FM5
      ZERO=ANGLE.EQ.0.0
      IF (ZERO) GO TO 200
      C1=COS(ANGLE)
      S1=SIN(ANGLE)
      C2=C1*C1-S1*S1
      S2=S1*C1+C1*S1
      C3=C2*C1-S2*S1
      S3=S2*C1+C2*S1
      C4=C2*C2-S2*S2
      S4=S2*C2+C2*S2
      GO TO 200
  100 CONTINUE
      FOLD=.FALSE.
      K0 = (M+1-J)*SEP + 1
      T=C1*A1+S1*B1
      S1=C1*B1-S1*A1
      C1=T
      T=C2*A2+S2*B2
      S2=C2*B2-S2*A2
      C2=T
      T=C3*A2-S3*B2
      S3=-C3*B2-S3*A2
      C3=T
      T=C4*A1-S4*B1
      S4=-C4*B1-S4*A1
      C4=T
  200 CONTINUE
C
      DO 500 KK = K0, NS, MM5
      DO 440 L = KK, NT, L1
      K1 = L + SIZE
      DO 420 K = L, K1, K2
      R0=X0(K)
      I0=Y0(K)
      RS1=X1(K)+X4(K)
      IS1=Y1(K)+Y4(K)
      RU1=X1(K)-X4(K)
      IU1=Y1(K)-Y4(K)
      RS2=X2(K)+X3(K)
      IS2=Y2(K)+Y3(K)
      RU2=X2(K)-X3(K)
      IU2=Y2(K)-Y3(K)
      X0(K)=R0+RS1+RS2
      Y0(K)=I0+IS1+IS2
      RA1=R0+RS1*A1+RS2*A2
      IA1=I0+IS1*A1+IS2*A2
      RA2=R0+RS1*A2+RS2*A1
      IA2=I0+IS1*A2+IS2*A1
      RB1=RU1*B1+RU2*B2
      IB1=IU1*B1+IU2*B2
      RB2=RU1*B2-RU2*B1
      IB2=IU1*B2-IU2*B1
      IF (ZERO) GO TO 300
      R1=RA1+IB1
      I1=IA1-RB1
      R2=RA2+IB2
      I2=IA2-RB2
      R3=RA2-IB2
      I3=IA2+RB2
      R4=RA1-IB1
      I4=IA1+RB1
      X1(K)=R1*C1+I1*S1
      Y1(K)=I1*C1-R1*S1
      X2(K)=R2*C2+I2*S2
      Y2(K)=I2*C2-R2*S2
      X3(K)=R3*C3+I3*S3
      Y3(K)=I3*C3-R3*S3
      X4(K)=R4*C4+I4*S4
      Y4(K)=I4*C4-R4*S4
      GO TO 400
  300 CONTINUE
      X1(K)=RA1+IB1
      Y1(K)=IA1-RB1
      X2(K)=RA2+IB2
      Y2(K)=IA2-RB2
      X3(K)=RA2-IB2
      Y3(K)=IA2+RB2
      X4(K)=RA1-IB1
      Y4(K)=IA1+RB1
  400 CONTINUE
  420 CONTINUE
  440 CONTINUE
  500 CONTINUE
      IF (FOLD) GO TO 100
  600 CONTINUE
C
      RETURN
      END
      SUBROUTINE R8 CFTK (N, M, X0, Y0, X1, Y1, X2, Y2, X3, Y3, X4, Y4,
     . X5, Y5, X6, Y6, X7, Y7, DIM)
C     RADIX 8 MULTI-DIMENSIONAL COMPLEX FOURIER TRANSFORM KERNEL
C
      INTEGER N, M, DIM(1)
      REAL X0 (1), Y0 (1), X1 (1), Y1 (1), X2 (1), Y2 (1)
      REAL X3 (1), Y3 (1), X4 (1), Y4 (1), X5 (1), Y5 (1)
      REAL X6 (1), Y6 (1), X7 (1), Y7 (1)
C
      LOGICAL FOLD,ZERO
      INTEGER J,K,K0,M8,M OVER 2
      INTEGER K1, K2, KK, L, L1, MM8, NT, SIZE, SEP
      INTEGER NS
      REAL ANGLE,C1,C2,C3,C4,C5,C6,C7,E,S1,S2,S3,S4,S5,S6,S7,T,TWOPI
      REAL R1,R2,R3,R4,R5,R6,R7,RS0,RS1,RS2,RS3,RU0,RU1,RU2,RU3
      REAL I1,I2,I3,I4,I5,I6,I7,IS0,IS1,IS2,IS3,IU0,IU1,IU2,IU3
      REAL RSS0,RSS1,RSU0,RSU1,RUS0,RUS1,RUU0,RUU1
      REAL ISS0,ISS1,ISU0,ISU1,IUS0,IUS1,IUU0,IUU1
      REAL FJM1, FM8
      DATA TWO PI/6.283185/, E/0.70710678/
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M8=M*8
      FM8 = FLOAT(M8)
      MM8 = SEP*M8
      M OVER 2=M/2+1
C
      FJM1 = -1.0
      DO 600 J=1,M OVER 2
      FOLD=J.GT.1 .AND. 2*J.LT.M+2
      K0 = (J-1)*SEP + 1
      FJM1 = FJM1 + 1.0
      ANGLE = TWO PI*FJM1/FM8
      ZERO=ANGLE.EQ.0.0
      IF (ZERO) GO TO 200
      C1=COS(ANGLE)
      S1=SIN(ANGLE)
      C2=C1*C1-S1*S1
      S2=S1*C1+C1*S1
      C3=C2*C1-S2*S1
      S3=S2*C1+C2*S1
      C4=C2*C2-S2*S2
      S4=S2*C2+C2*S2
      C5=C4*C1-S4*S1
      S5=S4*C1+C4*S1
      C6=C4*C2-S4*S2
      S6=S4*C2+C4*S2
      C7=C4*C3-S4*S3
      S7=S4*C3+C4*S3
      GO TO 200
  100 CONTINUE
      FOLD=.FALSE.
      K0 = (M+1-J)*SEP + 1
      T=(C1+S1)*E
      S1=(C1-S1)*E
      C1=T
      T=S2
      S2=C2
      C2=T
      T=(-C3+S3)*E
      S3=(C3+S3)*E
      C3=T
      C4=-C4
      T=-(C5+S5)*E
      S5=(-C5+S5)*E
      C5=T
      T=-S6
      S6=-C6
      C6=T
      T=(C7-S7)*E
      S7=-(C7+S7)*E
      C7=T
  200 CONTINUE
C
      DO 500 KK = K0, NS, MM8
      DO 440 L = KK, NT, L1
      K1 = L + SIZE
      DO 420 K = L, K1, K2
      RS0=X0(K)+X4(K)
      IS0=Y0(K)+Y4(K)
      RU0=X0(K)-X4(K)
      IU0=Y0(K)-Y4(K)
      RS1=X1(K)+X5(K)
      IS1=Y1(K)+Y5(K)
      RU1=X1(K)-X5(K)
      IU1=Y1(K)-Y5(K)
      RS2=X2(K)+X6(K)
      IS2=Y2(K)+Y6(K)
      RU2=X2(K)-X6(K)
      IU2=Y2(K)-Y6(K)
      RS3=X3(K)+X7(K)
      IS3=Y3(K)+Y7(K)
      RU3=X3(K)-X7(K)
      IU3=Y3(K)-Y7(K)
      RSS0=RS0+RS2
      ISS0=IS0+IS2
      RSU0=RS0-RS2
      ISU0=IS0-IS2
      RSS1=RS1+RS3
      ISS1=IS1+IS3
      RSU1=RS1-RS3
      ISU1=IS1-IS3
      RUS0=RU0-IU2
      IUS0=IU0+RU2
      RUU0=RU0+IU2
      IUU0=IU0-RU2
      RUS1=RU1-IU3
      IUS1=IU1+RU3
      RUU1=RU1+IU3
      IUU1=IU1-RU3
      T=(RUS1+IUS1)*E
      IUS1=(IUS1-RUS1)*E
      RUS1=T
      T=(RUU1+IUU1)*E
      IUU1=(IUU1-RUU1)*E
      RUU1=T
      X0(K)=RSS0+RSS1
      Y0(K)=ISS0+ISS1
      IF (ZERO) GO TO 300
      R1=RUU0+RUU1
      I1=IUU0+IUU1
      R2=RSU0+ISU1
      I2=ISU0-RSU1
      R3=RUS0+IUS1
      I3=IUS0-RUS1
      R4=RSS0-RSS1
      I4=ISS0-ISS1
      R5=RUU0-RUU1
      I5=IUU0-IUU1
      R6=RSU0-ISU1
      I6=ISU0+RSU1
      R7=RUS0-IUS1
      I7=IUS0+RUS1
      X4(K)=R1*C1+I1*S1
      Y4(K)=I1*C1-R1*S1
      X2(K)=R2*C2+I2*S2
      Y2(K)=I2*C2-R2*S2
      X6(K)=R3*C3+I3*S3
      Y6(K)=I3*C3-R3*S3
      X1(K)=R4*C4+I4*S4
      Y1(K)=I4*C4-R4*S4
      X5(K)=R5*C5+I5*S5
      Y5(K)=I5*C5-R5*S5
      X3(K)=R6*C6+I6*S6
      Y3(K)=I6*C6-R6*S6
      X7(K)=R7*C7+I7*S7
      Y7(K)=I7*C7-R7*S7
      GO TO 400
  300 CONTINUE
      X4(K)=RUU0+RUU1
      Y4(K)=IUU0+IUU1
      X2(K)=RSU0+ISU1
      Y2(K)=ISU0-RSU1
      X6(K)=RUS0+IUS1
      Y6(K)=IUS0-RUS1
      X1(K)=RSS0-RSS1
      Y1(K)=ISS0-ISS1
      X5(K)=RUU0-RUU1
      Y5(K)=IUU0-IUU1
      X3(K)=RSU0-ISU1
      Y3(K)=ISU0+RSU1
      X7(K)=RUS0-IUS1
      Y7(K)=IUS0+RUS1
  400 CONTINUE
  420 CONTINUE
  440 CONTINUE
  500 CONTINUE
      IF (FOLD) GO TO 100
  600 CONTINUE
C
      RETURN
      END
      SUBROUTINE RP CFTK (N, M, P, R, X, Y, DIM)
C     RADIX PRIME MULTI-DIMENSIONAL COMPLEX FOURIER TRANSFORM KERNEL
C
      INTEGER N, M, P, R, DIM(1)
      REAL X(R,P), Y(R,P)
C
      LOGICAL FOLD,ZERO
      REAL ANGLE,IS,IU,RS,RU,T,TWOPI,XT,YT
      REAL FU, FP, FJM1, FMP
      INTEGER J,JJ,K0,K,M OVER 2,MP,PM,PP,U,V
      INTEGER K1, K2, KK, L, L1, MMP, NT, SIZE, SEP
      INTEGER NS
C
      REAL AA (9,9), BB (9,9)
      REAL A (18), B (18), C (18), S (18)
      REAL IA (9), IB (9), RA (9), RB (9)
C
      DATA TWO PI/6.283185/
C
      NT = DIM(1)
      SEP = DIM(2)
      L1 = DIM(3)
      SIZE = DIM(4) - 1
      K2 = DIM(5)
      NS = N*SEP
      M OVER 2=M/2+1
      MP=M*P
      FMP = FLOAT(MP)
      MMP = SEP*MP
      PP=P/2
      PM=P-1
      FP = FLOAT(P)
      FU = 0.0
      DO 100 U=1,PP
      FU = FU + 1.0
      ANGLE = TWO PI*FU/FP
      JJ=P-U
      A(U)=COS(ANGLE)
      B(U)=SIN(ANGLE)
      A(JJ)=A(U)
      B(JJ)=-B(U)
  100 CONTINUE
      DO 300 U=1,PP
      DO 200 V=1,PP
      JJ=U*V-U*V/P*P
      AA(V,U)=A(JJ)
      BB(V,U)=B(JJ)
  200 CONTINUE
  300 CONTINUE
C
      FJM1 = -1.0
      DO 1500 J=1,M OVER 2
      FOLD=J.GT.1 .AND. 2*J.LT.M+2
      K0 = (J-1)*SEP + 1
      FJM1 = FJM1 + 1.0
      ANGLE = TWO PI*FJM1/FMP
      ZERO=ANGLE.EQ.0.0
      IF (ZERO) GO TO 700
      C(1)=COS(ANGLE)
      S(1)=SIN(ANGLE)
      DO 400 U=2,PM
      C(U)=C(U-1)*C(1)-S(U-1)*S(1)
      S(U)=S(U-1)*C(1)+C(U-1)*S(1)
  400 CONTINUE
      GO TO 700
  500 CONTINUE
      FOLD=.FALSE.
      K0 = (M+1-J)*SEP + 1
      DO 600 U=1,PM
      T=C(U)*A(U)+S(U)*B(U)
      S(U)=-S(U)*A(U)+C(U)*B(U)
      C(U)=T
  600 CONTINUE
  700 CONTINUE
C
      DO 1400 KK = K0, NS, MMP
      DO 1340 L = KK, NT, L1
      K1 = L + SIZE
      DO 1320 K = L, K1, K2
      XT=X(K,1)
      YT=Y(K,1)
      RS=X(K,2)+X(K,P)
      IS=Y(K,2)+Y(K,P)
      RU=X(K,2)-X(K,P)
      IU=Y(K,2)-Y(K,P)
      DO 800 U=1,PP
      RA(U)=XT+RS*AA(U,1)
      IA(U)=YT+IS*AA(U,1)
      RB(U)=RU*BB(U,1)
      IB(U)=IU*BB(U,1)
  800 CONTINUE
      XT=XT+RS
      YT=YT+IS
      DO 1000 U=2,PP
      JJ=P-U
      RS=X(K,U+1)+X(K,JJ+1)
      IS=Y(K,U+1)+Y(K,JJ+1)
      RU=X(K,U+1)-X(K,JJ+1)
      IU=Y(K,U+1)-Y(K,JJ+1)
      XT=XT+RS
      YT=YT+IS
      DO 900 V=1,PP
      RA(V)=RA(V)+RS*AA(V,U)
      IA(V)=IA(V)+IS*AA(V,U)
      RB(V)=RB(V)+RU*BB(V,U)
      IB(V)=IB(V)+IU*BB(V,U)
  900 CONTINUE
 1000 CONTINUE
      X(K,1)=XT
      Y(K,1)=YT
      DO 1300 U=1,PP
      JJ=P-U
      IF (ZERO) GO TO 1100
      XT=RA(U)+IB(U)
      YT=IA(U)-RB(U)
      X(K,U+1)=XT*C(U)+YT*S(U)
      Y(K,U+1)=YT*C(U)-XT*S(U)
      XT=RA(U)-IB(U)
      YT=IA(U)+RB(U)
      X(K,JJ+1)=XT*C(JJ)+YT*S(JJ)
      Y(K,JJ+1)=YT*C(JJ)-XT*S(JJ)
      GO TO 1200
 1100 CONTINUE
      X(K,U+1)=RA(U)+IB(U)
      Y(K,U+1)=IA(U)-RB(U)
      X(K,JJ+1)=RA(U)-IB(U)
      Y(K,JJ+1)=IA(U)+RB(U)
 1200 CONTINUE
 1300 CONTINUE
 1320 CONTINUE
 1340 CONTINUE
 1400 CONTINUE
      IF (FOLD) GO TO 500
 1500 CONTINUE
C
      RETURN
      END
