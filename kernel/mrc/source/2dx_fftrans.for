C*FFTRANS.FOR************************************************************
C                                                                       *
C         This program will do 2-dimensional FFTs in either             *
C       direction. The real-space origin is at (1,1) and                *
C       the origin of reciprocal space is at (1,NY/2+1).                *
C       The FT of an image NX,NY is NX/2+1,NY complex value.            *
C                                                                       *
C         All transforms are done using Lynn ten Eycks subroutines.     *
C       These allow arbitrary-sized images having a LARGEST PRIME       *
C       factor of 19!!.                                                 *
C                                                                       *
C       Very large images are transformed using the disk-based routine  *
C       GIANTFFT (current dimensions up to 16384 x 16384). The switch   *
C       point is now determined in UNIX by the dimension of the         *
C       parameter IARRMXSIZ.                                             *
C       Smaller images, currently up to 6000x6000 are done in core      *
C       using the in-core routine TDXFFT.                               *
C                                                                       *
C       NOTE:  3-D images are treated as image stacks.                  *
C         Each section is transformed independently!!                   *
C         HOWEVER, only single sections of LARGE images may be used!    *
C                                                                       *
C       No input parametrs are required.                                *
C                                                                       *
C       Logical I/O assignments are:                                    *
C                                                                       *
C       IN                      input image or FT                       *
C       OUT                     output FT or image                      *
C                                                                       *
C                                                                       *
C       Version   1.00  18.10.81        DAA             FOR VAX         *
C       Version   1.01  27.MAY.82       DAA             FOR VAX         *
C       Version   1.02  10.JUNE.82      DAA             FOR VAX         *
C       Version   1.03  23.JULY.82      DAA             FOR VAX         *
C       Version   1.04  01.OCTOBER.82   DAA             FOR VAX         *
C       Version   1.05  08.November.82  DAA             FOR VAX         *
C       Update    1.05  18.November.82  DAA             FOR VAX         *
C       Revision  1.06  27.November.84  RH              FOR VAX         *
C       Noscratch 1.07  23.March.85     RH              FOR VAX         *
C       Parameter 1.08  13.February.87  RH              FOR VAX         *
C       Cosmetic  1.09  04.June.87      RH              FOR VAX         *
C       Convert   2.00  13.July.90      RH              FOR 5400        *
C       Convert         11.September.91 RB              for Alliant     *
C       Dimension 2.01  31.December.91  RB and RH       for Alliant     *
C       Dimension 2.02  25.January.95   RH              remove COMMON   *
C       Dimension 2.03  22.April.96     RH              added comment   *
C       Version   2.04  29.August.00    RH              extra IMPOSN    *
C       Version   2.05  09.July.04      JMS             increase size   *
C                                                                       *
C************************************************************************
C
C*** Note : if IARRMXSIZ is increased, arrays must be declared in COMMON
C***        in order for the SGI to compile this program  22.04.96 JMS
C***        this should not now be necessary as I have put it in common anyway
C***        jms 25.04.96
        PARAMETER (IARRMXSIZ=256000000)
CHEN    PARAMETER (IARRMXSIZ=410000000)
CHEN    PARAMETER (IARRMXSIZ=810000000)
        COMMON//NX,NY,NZ
        DIMENSION ARRAY(IARRMXSIZ)
        COMPLEX CRAY(IARRMXSIZ/2)
C       COMMON/FTBUF/MAXSIZ,ARRAY(IARRMXSIZ)    ! left over from pre-1995
        DIMENSION TITLE(20),NXYZR(3),MXYZR(3),NXYZF(3),NXYZST(3)
        CHARACTER DAT*24
        LOGICAL LARGE
CTSH++
        CHARACTER*80 TMPTITLE
        EQUIVALENCE (TMPTITLE,TITLE)
CTSH--

C*** next statement added to force program to use data rather than
C*** stack (use system binary limit for stacksize and datasize) jms 25.04.96
        common/big/array
        EQUIVALENCE (NX,NXYZR),(ARRAY,CRAY)
        DATA IFOR/0/,IBAK/1/,ZERO/0.0/, LARGE/.FALSE./, NXYZST/3*0/
C
        WRITE(6,1000)
1000    FORMAT(/,/,' FFTRANS: Fourier Transform Program',
     .     '  V2.04 (29.8.00)',/,/)
C
        CALL IMOPEN(1,'IN','RO')
        CALL IMOPEN(2,'OUT','NEW')
        CALL FDATE(DAT)
C
C   Read input header & decide which direction to go
C
        CALL IRDHDR(1,NXYZR,MXYZR,MODE,DMIN,DMAX,DMEAN)
        CALL ITRHDR(2,1)
C   The following line should be replaced by a more sophisticated
C   calculation, once the mechanism of memory allocation is worked out.
        MAXSIZ = IARRMXSIZ
        NY2 = NY/2
        NY21 = NY2 + 1
        NZM1 = NZ - 1
        TMIN =  1.E10
        TMAX = -1.E10
        TMEAN = 0.0
        IF (NZ .GT. 1) WRITE(6,1200) NZ
1200    FORMAT(/,/,' Each of the ',I4,' sections are SEPARATELY ',
     . 'transformed!!!!',/,/)
        IF (MODE .GE. 3) GOTO 50
C
C   Here for foward transform
C
        NXM1 = NX - 1
        NYM1 = NY -1
        NX2 = NX/2
        NX21 = NX2 + 1
        NXP2 = NX + 2
        NXYZF(1) = NX21
        NXYZF(2) = NY
        NXYZF(3) = NZ
        IF (NXP2*NY .GT. MAXSIZ) LARGE = .TRUE.
        IF (LARGE .AND. NZ .GT. 1) 
     . STOP '*** ONLY SINGLE SECTIONS FOR LARGE IMAGES ****'
        CALL IALMOD(2,4)
        CALL IALSIZ(2,NXYZF,NXYZST)
CTSH    WRITE(TITLE) DAT(5:24,1500)
CTSH++
        WRITE(TMPTITLE,1500) DAT(5:24)
CTSH--
1500    FORMAT(' FFTRANS: Foward Fourier Transform Calculated',6X,A20)
        CALL IWRHDR(2,TITLE,1,ZERO,ZERO,ZERO)
C
C  Loop over all sections & write out with shifted origin
C
        DO 100 ISEC = 0,NZM1
C         IF (LARGE) THEN
C           CALL GIANTFFT(1,2,NX,NY,DMIN,DMAX,DMEAN,IFOR,MAXSIZ,ARRAY,CRAY)
C         ELSE
            CALL IMPOSN(1,ISEC,0)
            CALL IRDPAS(1,ARRAY,NXP2,NY,0,NXM1,0,NYM1,*99)
            CALL TDXFFT(ARRAY,NX,NY,IFOR)
            CALL IWRPAS(2,ARRAY,NXP2,NY,0,NX2,NY2,NYM1)
            CALL IMPOSN(2,ISEC,NY2)
            CALL IWRPAS(2,ARRAY,NXP2,NY,0,NX2,0,NY2-1)
            CALL ICLCDN(ARRAY,NX21,NY,1,NX21,1,NY,DMIN,DMAX,DMEAN)
C         END IF
          IF (DMIN .LT. TMIN) TMIN = DMIN
          IF (DMAX .GT. TMAX) TMAX = DMAX
          TMEAN = TMEAN + DMEAN
          IF (NZ .GT. 1) WRITE(6,1600) ISEC,DMIN,DMAX,DMEAN
100     CONTINUE
1600    FORMAT(/,'Min,Max,Mean values for section ',I4,' are: ',3G13.5)
        GOTO 90
C
C   Here for inverse transform
C
50      NXR = (NX - 1)*2
        NXP2 = NXR + 2
        NXYZF(1) = NXR
        NXYZF(2) = NY
        NXYZF(3) = NZ
        IF (NXP2*NY .GT. MAXSIZ) LARGE = .TRUE.
        IF (LARGE .AND. NZ .GT. 1) 
     . STOP '*** ONLY SINGLE SECTIONS FOR LARGE IMAGES ****'
        INDEX = NXP2*NY2 + 1
        CALL IALMOD(2,2)
        CALL IALSIZ(2,NXYZF,NXYZST)
CTSH    WRITE(TITLE) DAT(5:24,1700)
CTSH++
        WRITE(TMPTITLE,1700) DAT(5:24)
CTSH--
1700    FORMAT(' FFTRANS: Inverse Fourier Transform Calculated',6X,A20)
        CALL IWRHDR(2,TITLE,1,ZERO,ZERO,ZERO)
C
C  Loop over all sections , shift origin on reading in
C
        DO 300 ISEC = 0,NZM1
C         IF (LARGE) THEN
C           CALL GIANTFFT(1,2,NXR,NY,DMIN,DMAX,DMEAN,IBAK,MAXSIZ,ARRAY,CRAY)
C         ELSE
            DO 200 IY = 1,NY
              IF (IY .EQ. NY21) INDEX = 1
              CALL IRDLIN(1,ARRAY(INDEX),*99)
              INDEX = INDEX + NXP2
200         CONTINUE
            CALL TDXFFT(ARRAY,NXR,NY,IBAK)
            CALL IWRPAS(2,ARRAY,NXP2,NY,0,NXR-1,0,NY-1)
            CALL ICLDEN(ARRAY,NXP2,NY,1,NXR,1,NY,DMIN,DMAX,DMEAN)
C         END IF
          IF (DMIN .LT. TMIN) TMIN = DMIN
          IF (DMAX .GT. TMAX) TMAX = DMAX
          TMEAN = TMEAN + DMEAN
          IF (NZ .GT. 1) WRITE(6,1600) ISEC,DMIN,DMAX,DMEAN
300     CONTINUE
C
90      TMEAN = TMEAN/NZ
        WRITE(6,1800) TMIN,TMAX,TMEAN
1800    FORMAT(/,' Overall Min,Max,Mean values are: ',3G13.5)
        CALL IWRHDR(2,TITLE,-1,TMIN,TMAX,TMEAN)
        CALL IMCLOSE(1)
        CALL IMCLOSE(2)
        CALL EXIT
C
99      STOP 'END-OF-FILE ERROR ON READ'
        END

