C*2DX_FFTlib.FOR********************************************************
C
C       TWO-DIMENSIONAL FOURIER TRANSFORM SUBROUTINE FOR IMAGE
C       PROCESSING. DOES BOTH FOWARD & INVERSE TRANSFORMS
C       USES LYNN TENEYCK'S MIXED-RADIX ROUTINES
C       THUS THE ONLY RESTRICTION IS THAT THE IMAGE SIZE BE
C       AN EVEN NUMBER AND HAVE NO PRIME FACTORS LARGER THAN 19!!
C
C       IDIR =  0 Foward   (Real --> Complex)     exp(+2PIirs)
C       IDIR =  1 Reverse  (Complex --> Real)     exp(-2PIirs)
C       IDIR = -1 Foward   (Complex --> Complex)  exp(+2PIirs)
C       IDIR = -2 Reverse  (Complex --> Complex)  exp(-2PIirs)
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
C       Version 2.00    Oct 29 1999             HST
C
      SUBROUTINE TDXFFT(ARRAY,NX,NY,IDIR)
      DIMENSION ARRAY(1),IDIM(5)
C
      
      integer*8 plan
      CALL fftwf_plan_dft_r2c_2d(plan,NX,NY,ARRAY,ARRAY,FFTW_ESTIMATE)
      
      call fftwf_execute(plan)
      call fftwf_destroy_plan(plan)
C
C      
C
C
      RETURN
      END


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
        SUBROUTINE GIANTFFT(INUNIT,IOUTUNIT,NX,NY,DMIN,DMAX,DMEAN,IDIR,
     .                  NBUFSIZ,ARRAY,CRAY)
        include 'imsubs_common.for'
C       PARAMETER INOUTMAX=16384
        PARAMETER (INOUTMAX=1000000)
        DIMENSION ARRAY(1)
        DIMENSION BRRAY(2*INOUTMAX)
        REAL*8 DDMEAN
        COMPLEX CRAY(1),CLINE(INOUTMAX),CRRAY(INOUTMAX)
        EQUIVALENCE (CLINE,CRRAY,BRRAY)
C
        IN = lstream(INUNIT)
        IOUT = lstream(IOUTUNIT)
        NX4=NX*4
        NXP2 = NX + 2
        NXP24 = NXP2*4
        NX21 = NXP2/2
        NXM1 = NX - 1
        NY2 = NY*2
        NYO2 = NY/2
        WRITE(6,11)
11      FORMAT(/' GIANTFFT, disk-based FFTsubroutine, VX_1.8(25-Aug-1995)')
        WRITE(6,10)
10      FORMAT(' No scratch file is used, but four complete',
     .  ' disc read/writes are required for forward transform,',
     .  ' three for inverse')
        DMIN =  1.E20
        DMAX = -1.E20
        DDMEAN = 0.0
        INOUTSQRT=SQRT(2.0*INOUTMAX)
C
C  Calculate buffer loads
C
        NLINES = MIN(NBUFSIZ/NXP2,NY,INOUTSQRT)
        NLOADS = NY/NLINES
        NLAST = NY - NLINES*NLOADS
        IF (NLAST .EQ. 0) THEN
          NLAST = NLINES
        ELSE
          NLOADS = NLOADS + 1
        END IF
C
        NUSE = MIN(NBUFSIZ/NY2,NX21,INOUTSQRT/2)
        MLOADS = NX21/NUSE
        MLAST = NX21 - NUSE*MLOADS
        IF (MLAST .EQ. 0) THEN
          MLAST = NUSE
        ELSE
          MLOADS = MLOADS + 1
        END IF
C==============================================================================
C
C   HERE FOR FORWARD TRANSFORM
C
        IF (IDIR .NE. 0) GOTO 50
C
C  Do first part of transform - writeout with partial transposition
C  Scratch file storage has local y-coord fastest in strips parallel to x,
C   then x-coord, then slowest variable is the y-index of the strip.
C
        WRITE(6,1000) NLINES,NLOADS,NUSE,MLOADS
1000    FORMAT(/,' GIANTFFT : # Lines, Loads for passes 1,2: ',
     .          4I8,/)
C
        CALL QSEEK(IOUT,1,1025,8)
        DO 150 LOADS = 1,NLOADS
          IF (LOADS .EQ. NLOADS) THEN
            NL = NLAST
          ELSE
            NL=NLINES
          END IF
          INDEX = 1
          DO 100 LINES = 1,NL
            CALL IRDLIN(INUNIT,ARRAY(INDEX),*90)
            INDEX = INDEX + NXP2
100       CONTINUE
C
          CALL TDXFFT(ARRAY,NX,NL,0)
C
C    This section partially transposes the array before output in strips.
          NTURN= MIN(INOUTMAX/NL,NX21)
          NOUT=NTURN*NL*8
          NTIMES=NX21/NTURN
          NTURNLAST=NX21-NTIMES*NTURN
          IF(NTURNLAST.EQ.0) THEN
                NTURNLAST=NTURN
          ELSE
                NTIMES=NTIMES+1
          ENDIF
          INDEX=0
          DO 120 IDO=1,NTIMES
                  INDEXB=1
                IF(IDO.EQ.NTIMES) THEN
                  NTURN=NTURNLAST
                  NOUT=NTURN*NL*8
                ENDIF
                DO 110 ITURN=1,NTURN
                  INDEX=INDEX+1
                  INDEXA=INDEX*2-1
                DO 110 LINES=1,NL
                  BRRAY(INDEXB)=ARRAY(INDEXA)
                  BRRAY(INDEXB+1)=ARRAY(INDEXA+1)
                  INDEXB=INDEXB+2
                  INDEXA=INDEXA+NXP2
110             CONTINUE
                CALL QWRITE(IOUT,BRRAY,NOUT)
120        CONTINUE
           WRITE(6,151) LOADS
151        FORMAT(' First pass, Load #',I5,'  completed and output')
150     CONTINUE
C
C Do second half of transform, build up strip parallel to y by picking up
C  bands on reading in. Write out in same way - initially to same scratch area.
C
        NUSES=NUSE
        DO 400 LOADS = 1,MLOADS
          IF (LOADS .EQ. MLOADS) THEN
            NUSE = MLAST
          END IF
          DO 250 IY = 1,NLOADS
                IF(IY.EQ.NLOADS) THEN
                  NL=NLAST
                ELSE
                  NL=NLINES
                ENDIF
                NBLOCK=NUSE*NL
                NBLOCK8=NBLOCK*8
                INDEX = (IY-1)*NX21*NLINES + (LOADS-1)*NUSES*NL + 1
                CALL QSEEK(IOUT,INDEX,1025,8)
                CALL QREAD(IOUT,CRRAY,NBLOCK8,IER)
                IF (IER .NE. 0) IPROG=250
                IF (IER .NE. 0) GOTO 90
                IND=1+(IY-1)*NLINES-NY
                INDB=1
                DO 200 J=1,NUSE
                  IND=IND+NY
                  INDA=IND
                DO 200 I=1,NL
                  CRAY(INDA)=CRRAY(INDB)
                  INDA=INDA+1
                  INDB=INDB+1
200             CONTINUE
250       CONTINUE
C
          CALL TDXFFT(CRAY,NY,NUSE,-1)
C
C  Write back to same area of scratch file now.
          DO 350 IY = 1,NLOADS
                IF(IY.EQ.NLOADS) THEN
                  NL=NLAST
                ELSE
                  NL=NLINES
                ENDIF
                NBLOCK=NUSE*NL
                NBLOCK8=NBLOCK*8
                IND=1+(IY-1)*NLINES-NY
                INDB=1
                DO 300 J=1,NUSE
                  IND=IND+NY
                  INDA=IND
                DO 300 I=1,NL
                  CRRAY(INDB)=CRAY(INDA)
                  INDA=INDA+1
                  INDB=INDB+1
300             CONTINUE
                INDEX = (IY-1)*NX21*NLINES + (LOADS-1)*NUSES*NL + 1
                CALL QSEEK(IOUT,INDEX,1025,8)
                CALL QWRITE(IOUT,CRRAY,NBLOCK8)
350       CONTINUE
           WRITE(6,401) LOADS
401        FORMAT(' Transform turned around , Load #',I5)
400     CONTINUE
C
C  Read from scratch area, transpose and output fully transformed file.
        CALL QSEEK(IOUT,1,1025,8)
           WRITE(6,448)
448        FORMAT(' Second pass starting')
        DO 450 LOADS = 1,NLOADS
                IF (LOADS .EQ. NLOADS) THEN
                  NL = NLAST
                ELSE
                  NL=NLINES
                END IF
                NSEC=NL*NXP24
                CALL QREAD(IOUT,CRAY,NSEC,IER)
                WRITE(6,449) LOADS
449             FORMAT(' Second pass data read in, Load #',I5)
                IF(IER.NE.0) IPROG=450
                IF(IER.NE.0) GOTO 90
                IY=1+(LOADS-1)*NLINES
                CALL QSEEK(IOUT,IY,1025,NXP24)
                DO 420  LINES=1,NL
                        INDEXB=LINES
                        INDEXA=1
                        DO 410 IX=1,NX21
                           CLINE(INDEXA)=CRAY(INDEXB)
                           VAL = CABS(CRAY(INDEXB))
                           IF (VAL .LT. DMIN) DMIN = VAL
                           IF (VAL .GT. DMAX) DMAX = VAL
                           DDMEAN = DDMEAN + VAL
                           INDEXA=INDEXA+1
                           INDEXB=INDEXB+NL
410                     CONTINUE
                        CALL QWRITE(IOUT,CLINE,NXP24)
420             CONTINUE
           WRITE(6,451) LOADS
451        FORMAT(' Second pass, Load #',I5,'  completed and output')
450     CONTINUE
        DMEAN = DDMEAN/(NX21*NY)
C
C  Finally reorder output file NSWAP lines at a time, so that it is in the
C  standard MAPFORMAT.  (i.e. Origin is at NY/2 + 1)
        NSWAP = MIN((NBUFSIZ/2/NX21),INOUTMAX/NX21,NYO2)
        NOUT  = NSWAP*NXP24
        NTIMES = NYO2/NSWAP
        NSWAPLAST = NYO2 - NTIMES*NSWAP
        IF(NSWAPLAST.EQ.0) THEN
                NSWAPLAST = NSWAP
        ELSE
                NTIMES = NTIMES + 1
        ENDIF
           WRITE(6,489) NTIMES
489        FORMAT(' Number of swaps to be done =',I5)
        DO 490 IDO=1,NTIMES
                IY = 1 + (IDO-1)*NSWAP
                JY = IY + NYO2
              IF(IDO.EQ.NTIMES) NOUT = NSWAPLAST*NXP24
                CALL QSEEK(IOUT,IY,1025,NXP24)
                CALL QREAD(IOUT,CRAY,NOUT,IER)
                IF(IER.NE.0) GO TO 90
                CALL QSEEK(IOUT,JY,1025,NXP24)
                CALL QREAD(IOUT,CLINE,NOUT,IER)
                IF(IER.NE.0) GO TO 90
                CALL QSEEK(IOUT,IY,1025,NXP24)
                CALL QWRITE(IOUT,CLINE,NOUT)
                CALL QSEEK(IOUT,JY,1025,NXP24)
                CALL QWRITE(IOUT,CRAY,NOUT)
           WRITE(6,491) IDO
491        FORMAT(' Final swapping  #',I5,' output')
490     CONTINUE
        GOTO 99
C
C==============================================================================
C
C   HERE FOR INVERSE TRANSFORM
C
C
C  Read in, transpose into strips, and write out to scratch area.
C
50      WRITE(6,1000) NUSE,MLOADS,NLINES,NLOADS
        CALL QSEEK(IOUT,1,1025,8)
        DO 550 LOADS = 1,NLOADS
                IF (LOADS .EQ. NLOADS) THEN
                  NL = NLAST
                ELSE
                  NL=NLINES
                END IF
                DO 520  LINES=1,NL
                        INDEXB=LINES
                        INDEXA=1
                        IY=LINES + (LOADS-1)*NLINES
                        IF (IY .LE. NYO2) THEN
                          JY = IY + NYO2
                        ELSE
                          JY = IY - NYO2
                        END IF
                        CALL QSEEK(IN,JY,1025,NXP24)
                        CALL QREAD(IN,CLINE,NXP24,IER)
                        IF(IER.NE.0) GOTO 90
                        DO 510 IX=1,NX21
                           CRAY(INDEXB)=CLINE(INDEXA)
                           INDEXA=INDEXA+1
                           INDEXB=INDEXB+NL
510                     CONTINUE
520             CONTINUE
                NSEC=NL*NXP24
                CALL QWRITE(IOUT,CRAY,NSEC)
                WRITE(6,551) LOADS
551             FORMAT(' Initial transpose, Load #',I5,'  done')
550     CONTINUE
C
C   Do first part of transform : build up strip parallel to y by picking up
C   bands on reading in.   Write out in same way to same scratch area.
C
        NUSES=NUSE
        DO 800 LOADS = 1,MLOADS
          IF (LOADS .EQ. MLOADS) THEN
            NUSE = MLAST
          END IF
          DO 650 IY = 1,NLOADS
                IF(IY.EQ.NLOADS) THEN
                  NL=NLAST
                ELSE
                  NL=NLINES
                ENDIF
                NBLOCK=NUSE*NL
                NBLOCK8=NBLOCK*8
                INDEX = (IY-1)*NX21*NLINES + (LOADS-1)*NUSES*NL + 1
                CALL QSEEK(IOUT,INDEX,1025,8)
                CALL QREAD(IOUT,CRRAY,NBLOCK8,IER)
                IF (IER .NE. 0) GOTO 90
                IND=1+(IY-1)*NLINES-NY
                INDB=1
                DO 600 J=1,NUSE
                  IND=IND+NY
                  INDA=IND
                DO 600 I=1,NL
                  CRAY(INDA)=CRRAY(INDB)
                  INDA=INDA+1
                  INDB=INDB+1
600             CONTINUE
650       CONTINUE
C
          CALL TDXFFT(CRAY,NY,NUSE,-2)
C
C  Write back to same area of scratch file now.
          DO 750 IY = 1,NLOADS
                IF(IY.EQ.NLOADS) THEN
                  NL=NLAST
                ELSE
                  NL=NLINES
                ENDIF
                NBLOCK=NUSE*NL
                NBLOCK8=NBLOCK*8
                IND=1+(IY-1)*NLINES-NY
                INDB=1
                DO 700 J=1,NUSE
                  IND=IND+NY
                  INDA=IND
                DO 700 I=1,NL
                  CRRAY(INDB)=CRAY(INDA)
                  INDA=INDA+1
                  INDB=INDB+1
700             CONTINUE
                INDEX = (IY-1)*NX21*NLINES + (LOADS-1)*NUSES*NL + 1
                CALL QSEEK(IOUT,INDEX,1025,8)
                CALL QWRITE(IOUT,CRRAY,NBLOCK8)
750       CONTINUE
          WRITE(6,801) LOADS
801       FORMAT(' First dimension, Load #',I5,'  done')
800     CONTINUE
C
C  Read from scratch area, transpose, do second part of transform and
C                                       output fully transformed file.
        DO 950 LOADS = 1,NLOADS
          IF (LOADS .EQ. NLOADS) THEN
            NL = NLAST
          ELSE
            NL=NLINES
          END IF
C    This section does partial transpose of the array on input.
          NTURN= MIN(INOUTMAX/NL,NX21)
          NOUT=NTURN*NL*8
          NTIMES=NX21/NTURN
          NTURNLAST=NX21-NTIMES*NTURN
          IF(NTURNLAST.EQ.0) THEN
                NTURNLAST=NTURN
          ELSE
                NTIMES=NTIMES+1
          ENDIF
          INDEX=0
          IND=NLINES*NX21*(LOADS-1) + 1
          CALL QSEEK(IOUT,IND,1025,8)
          DO 920 IDO=1,NTIMES
                INDEXB=1
                IF(IDO.EQ.NTIMES) THEN
                  NTURN=NTURNLAST
                  NOUT=NTURN*NL*8
                ENDIF
                CALL QREAD(IOUT,BRRAY,NOUT,IER)
                IF(IER.NE.0) GOTO 90
                DO 910 ITURN=1,NTURN
                  INDEX=INDEX+1
                  INDEXA=INDEX*2-1
                DO 910 LINES=1,NL
                  ARRAY(INDEXA)=BRRAY(INDEXB)
                  ARRAY(INDEXA+1)=BRRAY(INDEXB+1)
                  INDEXB=INDEXB+2
                  INDEXA=INDEXA+NXP2
910             CONTINUE
920       CONTINUE
C
          CALL TDXFFT(ARRAY,NX,NL,1)
C
          INDEX = 1
          IND = NLINES*(LOADS-1) + 1
          CALL QSEEK(IOUT,IND,1025,NX4)
          DO 900 LINES = 1,NL
            CALL IWRLIN(IOUTUNIT,ARRAY(INDEX))
            DO 940 J = 0,NXM1
              VAL = ARRAY(INDEX + J)
              IF (VAL .LT. DMIN) DMIN = VAL
              IF (VAL .GT. DMAX) DMAX = VAL
              DDMEAN = DDMEAN + VAL
940         CONTINUE
            INDEX = INDEX + NXP2
900       CONTINUE
          WRITE(6,951) LOADS
951       FORMAT(' Second dimension, Load #',I5,'  completed')
950     CONTINUE
        DMEAN = DDMEAN/(NX*NY)
C
C  Finish up
C
99      RETURN
C
90      WRITE(6,9000) IPROG
9000    FORMAT(//,' *** GIANTFFT: ERROR ON DISCREAD  *** NEAR',I5//)
        CALL IMCLOSE(INUNIT)
        CALL IMCLOSE(IOUTUNIT)
        STOP '*** ERROR ****'
        END

