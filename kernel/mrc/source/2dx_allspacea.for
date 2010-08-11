C  ALLSPACE - umbrella program to calculate internal phase residual in all
C            17 space groups from data on a single film.  Can do the same 
C            thing within one film as ORIGTILT does between different films.
C
C       VX      1.6     RH      29.09.99        FRACT=0.2, convergence changed
C       VX      1.5     RH       9.05.96        remove BACKSPACE in SPEC option
C       VX      1.4     RH      22.12.95        clean up to standard f77
C       VX      1.3     RH      1.12.93         debug p4212 LOOK table
C       VX      1.2     RH      4.11.93         c12b printout corrected
C       VX      1.1     RH      13.10.93        bug removed p312/p321 names.
C       VX      1.0     RH      11.10.93        Original version
C
C#######################################################################
C
C   CARD INPUT
C
C   ON UNIT 'IN'
C          ANY NUMBER OF SPOTS WITH ANY INDEX (PRECEDED BY TITLE LINE)
C          H, K, AMPLITUDE, PHASE
C
C   ON UNIT 5
C   1.  SYMM, ISPG(17) symmetry required, followed (if SYMM=SPEC) by up to 17 
C                       spacegroup numbers - end list of space groups with ##. 
C
C               This first control card controls the space groups which are to
C               be considered.  For example, if axes are the same length and
C               angle is near 90 degrees, then p4, p422, p4212 are sensible, as
C               well as all lower symmetry.  The permitted options are
C
C               ALL     - all spacegroups       
C               HEXA    - all hexagonal + p2, c2
C               SQUA    - all non-hexagonal
C               RECT    - all rectangular, but not square nor hexagonal
C               OBLI    - p2 
C               TWO     - p2
C               THRE    - p3
C               SIX     - p6
C               FOUR    - p4
C               SPEC    - specify spacegroups by a list of numbers.
C                       - p1 is always considered to give statistical comparison
C   1a. List of spacegroup numbers specified separately (I3)
C
C   2.  SEARCH,REFINE,TILT,NYC      3*(LOGICAL*1), I5 (* FORMAT)
C               IF TRUE(T) THEN APPROPRIATE OPERATION IS PERFORMED.
C               THE SEQUENCE OF OPERATIONS IS PERFORMED SEPARATELY, FOR
C               EACH SPACE GROUP CONSIDERED, IN THE ORDER SHOWN.
C
C   3.  ORIGH,ORIGK,TILTH,TILTK
C         STARTING ORIGIN PHASE SHIFT AND STARTING BEAM TILT.
C
C   4.  STEP,ISIZE
C       PHASE-SHIFT STEP AND SIZE OF SEARCH AREA FOR PHASE ORIGIN GRID SEARCH.
C       IF GRID IS 121 X 121, A STEPSIZE OF 3 DEGREES WILL COVER WHOLE CELL.
C
C   5.  A,B,GAMMA,RIN,ROUT,CS,REALKV
C         CELL DIMENSIONS AND RESOLUTION RANGE IN ANGSTROMS, SPHERICAL 
C         ABERRATION(in mm) AND MICROSCOPE OPERATING VOLTAGE (in KV, used to
C         calcuate the wavelength).
C
C   6.  ILIST,ROT180,IQMAX,REVHK
C         ILIST=T  GIVES MORE DETAILED OUTPUT
C         ROT180=1 ENABLES SAME CONVENTION AS OTHER PROGRAMS TO BE USED FOR
C           P3 INDEXING CONVENTION - e.g. so that origin is same as in ORIGTILT.
C         IQMAX is used to restrict the comparisons to spots with IQ<=IQMAX
C         REVHK=1 enables switching H and K values
C
C   7.  SPCGRPFILENAM
C         suggested best space group
C
C   8.  OUTFILENAM
C         Filename for the final output
C
C
C
C####################################################################
C
C   Table of phase comparisons to be made
C       -  not comparable       
C       1  directly identical
C       H  differ by 180 * H            JSIMPL  = number to compare directly
C       K  differ by 180 * K            JSCREW   = number to compare + 180 * M
C       HK differ by 180 * (H+K)         where M = H*JH180 + K*JK180
C                                        
C
C   SPACEGROUP  H=-h +h -h +k +k -k -k +h -h +k -k -h +h -h +h  JSIMPL
C               H=                                 -k +k -k +k     JSCREW
C ref in
C  prog # symb  K=+k -k -k +h -h +h -h -h +h -h +h +h -h +k -k         JH180
C               K=                     -k +k -k +k                         JK180
C
C  1    1   p1     -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  0   -   -
C  2    2   p2     -  -  1  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  3    3b  p12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  4    "a   "     -  1  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  5    4b  p121   K  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  1   -  180
C  6    "a   "     -  H  -  -  -  -  -  -  -  -  -  -  -  -  -   0  1  180  -
C  7    5b  c12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  8    "a   "     -  1  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  9    6   p222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -
C 10    7b  p2221  H  H  1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180  -
C 11    "a    "    K  K  1  -  -  -  -  -  -  -  -  -  -  -  -   1  2   -  180
C 12    8   p22121 HK HK 1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180 180
C 13    9   c222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -
C 14    10  p4     -  -  1  -  1  1  -  -  -  -  -  -  -  -  -   3  0   -   -
C 15    11  p422   1  1  1  1  1  1  1  -  -  -  -  -  -  -  -   7  0   -   -
C 16    12  p4212  HK HK 1  1  HK HK 1  -  -  -  -  -  -  -  -   3  4  180 180
C 17    13  p3     -  -  -  -  -  -  -  -  -  1  -  1  -  -  -   2  0   -   -
C 18    14  p312   -  -  -  -  -  -  1  -  1  1  -  1  -  -  1   5  0   -   -
C 19    15  p321   -  -  -  1  -  -  -  1  -  1  -  1  -  1  -   5  0   -   -
C 20    16  p6     -  -  1  -  -  -  -  -  -  1  1  1  1  -  -   5  0   -   -
C 21    17  p622   -  -  1  1  -  -  1  1  1  1  1  1  1  1  1   11 0   -   -
C
C   Notes:-
C       1.  Compare all possible pairs of phases each with error E.
C       2.  Error comparing 2 different reflections is   1.414 * E.
C       3.  Error comparing reflections toits Friedel is   2.0 * E.
C       4.  So Friedel comparisons should have less weight ????
C
C   REFINEMENT OF PHASE ORIGIN AND BEAM TILT IN PROJECTION IMAGES.
C    CONTAINS OPTIONS TO CARRY OUT :-
C   (A) PHASE ORIGIN SEARCH, ENABLES BEST PHASE ORIGIN TO BE FOUND REGARDLESS
C       OF HOW FAR FROM IT THE INITIAL ESTIMATE IS.
C   (B) PHASE ORIGIN REFINEMENT. THIS CARRIES OUT LEAST SQUARES REFINEMENT
C       TO MINIMISE PHASE RESIDUAL, ONCE THE PHASE ORIGIN MINIMUM IS NEAR.
C       THE STARTING POINT CAN BE EITHER THE INPUT ORIGIN OR THE VALUES 
C       DETERMINED IN (A).
C   (C) PHASE ORIGIN AND BEAM TILT REFINEMENT. FOUR PARAMETERS ARE REFINED 
C       HERE TO COMPENSATE FOR ALL IMAGE PARAMETERS. BEAM TILT CAUSES THE
C       APPARENT ORIGIN TO MOVE ALONG A LINE WHEN SPOTS OF
C       DIFFERENT RESOLUTION ARE CONSIDERED. THE STARTING POINT FOR PHASE
C       ORIGIN IS THE OUTPUT FROM (A) OR (B), OR THE INPUT VALUES. THE 
C       STARTING POINT FOR THE BEAM TILT IS EITHER (0,0) OR THE INPUT VALUES.
C 
C       PERFORMS MINIMISATION (NOT LEAST SQUARES) OF THE RECIPROCAL
C               SPACE DISTANCE ERROR
C               L = SUM OF 2 * SIN(ABS(PHASEDIFF/2))
C
C########################################################################
CHENN>
C      PARAMETER (NMAX=3000,ISMAX=121,NSLOTS=32)
      PARAMETER (NMAX=6000)
      PARAMETER (ISMAX=361)
      PARAMETER (NSLOTS=64)
CHENN<
      DOUBLE PRECISION SYMBOL(21)
CTSH++
      CHARACTER*8 TMPSYMBOL(21)
      EQUIVALENCE (TMPSYMBOL, SYMBOL)
CTSH--
      DIMENSION IHIN(NMAX),IKIN(NMAX),IQIN(NMAX),PHSIN(NMAX),
     .  AMP(NMAX),TITLE(20),BSH(NMAX),BSK(NMAX),
     .  JPOINT(11,NMAX,21),JSIMPL(21),JSCREW(21),JH180(21),JK180(21)
      DIMENSION SYMPOSS(10),ISPGIN(17)
CTSH++
      CHARACTER*4 TMPSYMPOSS(10)
      EQUIVALENCE (TMPSYMPOSS,SYMPOSS)
CTSH--
      LOGICAL ILIST,SEARCH,REFINE,TILT,ISPG(17,10),IDO(17),IOUT(21)
      DIMENSION SPGID(21),SYMRS(21),THEOR(21),NSYMRS(21),
     .          NTHEOR(21),SPGORG(4,21)
      CHARACTER*1 IFLAG
      DATA DRAD/0.0174533/
C
CHENN>
      CHARACTER*80 OUTFILENAM,SPCGRPFILENAM
CHENN<
C
CTSH++
      CHARACTER*4 TMPSPGID(21)
      EQUIVALENCE (TMPSPGID,SPGID)
      DATA TMPSPGID/'  1 ','  2 ','  3b','  3a','  4b','  4a',
     .  '  5b','  5a','  6 ','  7b','  7a','  8 ','  9 ',' 10 ',
     .  ' 11 ',' 12 ',' 13 ',' 14 ',' 15 ',' 16 ',' 17 '/
CTSH--
CTSH      DATA SPGID/'  1 ','  2 ','  3b','  3a','  4b','  4a',
CTSH     .      '  5b','  5a','  6 ','  7b','  7a','  8 ','  9 ',' 10 ',
CTSH     .      ' 11 ',' 12 ',' 13 ',' 14 ',' 15 ',' 16 ',' 17 '/
CTSH      DATA SYMBOL/'p1    ','p2    ','p12_b ','p12_a ','p121_b',
CTSH     .      'p121_a','c12_b ','c12_a ','p222  ','p2221b',
CTSH     .      'p2221a','p22121','c222  ','p4    ','p422  ','p4212 ',
CTSH     .      'p3    ','p312  ','p321  ','p6    ','p622  '/
CTSH ++
      DATA TMPSYMBOL/'p1    ','p2    ','p12_b ','p12_a ','p121_b',
     .  'p121_a','c12_b ','c12_a ','p222  ','p2221b',
     .  'p2221a','p22121','c222  ','p4    ','p422  ','p4212 ',
     .  'p3    ','p312  ','p321  ','p6    ','p622  '/
CTSH--
      DATA JSIMPL/0,1,1,1,0,0,1,1,3,1,1,1,3,3,7,3,2,5,5,5,11/
      DATA JSCREW/0,0,0,0,1,1,0,0,0,2,2,2,0,0,0,4,0,0,0,0,0/
      DATA JH180/0,0,0,0,0,180,0,0,0,180,0,180,0,0,0,180,0,0,0,0,0/
      DATA JK180/0,0,0,0,180,0,0,0,0,0,180,180,0,0,0,180,0,0,0,0,0/
      DATA SYMRS/21*0.0/,THEOR/21*0.0/,NSYMRS/21*0/
      DATA NTHEOR/21*0/,SPGORG/84*0.0/
CTSH      DATA SYMPOSS/'ALL ','HEXA','SQUA','RECT','OBLI','TWO ','THRE',
CTSH     .      'SIX ','FOUR','SPEC'/
CTSH++
      DATA TMPSYMPOSS/'ALL ','HEXA','SQUA','RECT','OBLI','TWO ','THRE',
     .  'SIX ','FOUR','SPEC'/
CTSH--
      DATA IDO / 17*.FALSE./
      DATA ISPG/ 17*.TRUE.,
     A           2*.TRUE.,2*.FALSE.,.TRUE.,7*.FALSE.,5*.TRUE.,
     B           12*.TRUE.,5*.FALSE.,
     C           9*.TRUE.,8*.FALSE.,
     D           2*.TRUE.,15*.FALSE.,
     E           2*.TRUE.,15*.FALSE.,
     F           .TRUE.,11*.FALSE.,.TRUE.,4*.FALSE.,
     G           .TRUE.,13*.FALSE.,.TRUE.,2*.FALSE.,
     H           .TRUE.,8*.FALSE.,.TRUE.,7*.FALSE.,
     I           .TRUE.,16*.FALSE./
C
C*** initialization added by jms 06.03.96
       do k=1,21
        do j=1,nmax
          do i=1,11
           jpoint(i,j,k) = 0.
          end do
         end do
        end do
C
C  INPUT CONTROL DATA FROM UNIT 5 *********************************************
C
      WRITE(6,1000)
1000  FORMAT(/,/,' ALLSPACE, origin + beamtilt refinement any',
     .  ', symmetry, Version 1.6 (29.09.99)')
C     overall control of which space groups to use.
      READ(5,999) SYMM
999   FORMAT(A4)
      DO 998 J=1,10
        IF(SYMM.EQ.SYMPOSS(J)) THEN
          DO 995 I=1,17
995       IDO(I)=ISPG(I,J)
          IF(J.EQ.10) THEN
            READ(5,994,ERR=993) (ISPGIN(K),K=1,17)
994         FORMAT(17I3)
993         continue
            DO 992 K=1,17
              IF(ISPGIN(K).GT.0. AND.ISPGIN(K).LE.17) THEN
                IDO(ISPGIN(K))=.TRUE.
              ENDIF
992         CONTINUE
          ENDIF
          GO TO 996
        ENDIF
998   CONTINUE
      WRITE(6,997)
997   FORMAT(' First control card must be ',
     .          'one of 10 possible symmetries')
      STOP
996     CONTINUE
      WRITE(6,991)SYMM,(IDO(J),J=1,17)
991   FORMAT(/,' Symmetry and space groups to use',/,2X,A4,17(2X,L1))
C                       ! 3a
        IOUT(4)=IDO(3)
C                       ! 4b
        IOUT(5)=IDO(4)
C                       ! 4a
        IOUT(6)=IDO(4)
C                       ! 5b
        IOUT(7)=IDO(5)
      DO 980 J=1,17
        IF(J.LE.3) THEN
C                       ! 1,2,3b
                IOUT(J)=IDO(J)
        ENDIF
        IF(J.GE.5.AND.J.LE.7) THEN
C                       ! 5a,6,7
                IOUT(J+3)=IDO(J)
        ENDIF
        IF(J.GE.7) THEN
C                       ! 7a and up to 17
                IOUT(J+4)=IDO(J)
        ENDIF
980   CONTINUE
C
      READ(5,*) SEARCH,REFINE,TILT,NCYC
      READ(5,*) ORIGH,ORIGK,TILTH,TILTK
      READ(5,*) STEP, ISIZE
      READ(5,*) ACELL,BCELL,GAMMA,RIN,ROUT,CS,REALKV
      READ(5,*) ILIST,ROT180,IQMAX,REVHK
CHENN>
      read(5,*) SPCGRPFILENAM
      read(5,*) OUTFILENAM
CHENN<
      WRITE(6,'(/,/,17X,''*******INPUT CONTROL PARAMETERS********'',
     .   /,/)')
      WRITE(6,'(17X,''SEARCH ====================  '',L1,/)')  SEARCH
      WRITE(6,'(17X,''REFINE ====================  '',L1,/)')  REFINE
      WRITE(6,'(17X,''TILT ======================  '',L1,/)')  TILT
      WRITE(6,'(17X,''NCYC ======================'',I6,/)')    NCYC
      WRITE(6,'(17X,''ORIGH ====================='',F10.3,/)') ORIGH
      WRITE(6,'(17X,''ORIGK ====================='',F10.3,/)') ORIGK
      WRITE(6,'(17X,''TILTH ====================='',F10.3,/)') TILTH
      WRITE(6,'(17X,''TILTK ====================='',F10.3,/)') TILTK
      WRITE(6,'(17X,''STEP (Origin search)======='',F10.3,/)') STEP
      WRITE(6,'(17X,''ISIZE (Origin matrix)======'',I6,/)')    ISIZE
      WRITE(6,'(17X,''CELL (A-axis)=============='',F10.3,/)') ACELL
      WRITE(6,'(17X,''CELL (B-axis)=============='',F10.3,/)') BCELL
      WRITE(6,'(17X,''CELL (Gamma)==============='',F10.3,/)') GAMMA
      WRITE(6,'(17X,''RIN  ======================'',F10.3,/)') RIN
      WRITE(6,'(17X,''ROUT ======================'',F10.3,/)') ROUT
      WRITE(6,'(17X,''CS (Spherical aberration)=='',F10.3,/)') CS
      WRITE(6,'(17X,''KV (Accelerating voltage)=='',F10.3,/)') REALKV
      WRITE(6,'(17X,''ILIST ===================== '',L1,/)')   ILIST
      WRITE(6,'(17X,''ROT180 ===================='',F7.0,/)')  ROT180
CHENN>
      WRITE(6,'(17X,''IQMAX ====================='',I6,/)')    IQMAX
      WRITE(6,'(17X,''REVHK ====================='',F7.0)')    REVHK
      WRITE(6,'(17X,''SpaceGroupFileName ========'',A40)') 
     .   SPCGRPFILENAM(1:40)
      WRITE(6,'(17X,''OutputFilename ============'',A40)') 
     .   OUTFILENAM(1:40)
CHENN<
      IF(GAMMA.GT.90.0) GAMMA=180.0-GAMMA
      GAMMA=GAMMA*DRAD
C***      ANGAST=ANGAST*DRAD
      ASTAR=1.0/ACELL
      BSTAR=1.0/BCELL
      ASTAR=ASTAR/SIN(GAMMA)
      BSTAR=BSTAR/SIN(GAMMA)
      REALKV=REALKV*1000.0
      WL=12.3/SQRT(REALKV+REALKV**2/(10.0**6.0))
      WRITE(6,103)WL
103   FORMAT(' WAVELENGTH (ANGSTROMS)',F10.4)
      TXIN=TILTH
      TYIN=TILTK
      IF(ISIZE.GT.ISMAX) STOP ' SEARCH ARRAY TOO SMALL'
      RIN=1.0/RIN
      ROUT=1.0/ROUT
      IF(ROUT.LT.RIN) THEN
        RTEMP=RIN
        RIN=ROUT
        ROUT=RTEMP
      ENDIF
C
C  INPUT OF SPOTDATA ON UNIT 'IN'  ********************************************
C
        IGNORE=0
      CALL CCPDPN(1,'IN','READONLY','F',0,0)
        READ(1,1) TITLE
1       FORMAT(20A4)
        WRITE(6,2)TITLE
2       FORMAT(' HEADER RECORD ON SPOTDATA FILE',20A4)
C                    space for Friedel storage too
      DO 20 J=1,NMAX,2  
3     READ(1,*,END=25) IH,IK,AIN,P,IQ
      if(REVHK.eq.1.0)then
        ITMP=IH
        IH=IK
        IK=ITMP
      endif
      IF(IH.GE.100) GO TO 25
      RAD=SQRT(ASTAR**2*IH**2+BSTAR**2*IK**2 +
     .          2.0*ASTAR*BSTAR*IH*IK*COS(GAMMA))
      IF(RAD.LT.RIN.OR.RAD.GT.ROUT.OR.IQ.GT.IQMAX) THEN
        IGNORE=IGNORE + 1
        GO TO 3
      ENDIF
      IF(ROT180.EQ.1.0) THEN
        IHIN(J) = -IH
        IKIN(J) = -IK
      ELSE
        IHIN(J)=IH
        IKIN(J)=IK
      ENDIF
      IQIN(J)=IQ
      AMP(J)=AIN
      PHSIN(J)=P
C       ! generate and store all Friedel reflections
      IHIN(J+1) = -IHIN(J)
      IKIN(J+1) = -IKIN(J)
      IQIN(J+1) =  IQIN(J)
      AMP(J+1)=AMP(J)
C       ! ALPHA -> -ALPHA
      PHSIN(J+1)=-PHSIN(J)
20    CONTINUE
      WRITE(6,21)NMAX
21    FORMAT(' TOO MANY SPOTS FOR PROGRAM DIMENSION',I6)
      STOP
25    NREF=J-1
      WRITE(6,12) IGNORE,NREF/2
12    FORMAT(/,/,' TOTAL REFLECTIONS EXCLUDED (IQ, RESOL)......',I5,/,
     .         ' TOTAL REFLECTIONS ACCEPTED (IQ, RESOL)......',I5)
C
C  CALCULATION OF BEAMSHIFT PARAMETER ONCE ONLY AT START.
      DO 28 J=1,NREF
      RADSQ = IHIN(J)**2*ASTAR**2 + IKIN(J)**2*BSTAR**2 + 
     .  2.0*IHIN(J)*IKIN(J)*ASTAR*BSTAR*COS(GAMMA)
      BSH(J) = ASTAR*0.360*(CS*10.0**7*WL**2*RADSQ)
      BSK(J) = BSTAR*0.360*(CS*10.0**7*WL**2*RADSQ)
28    CONTINUE
C
C                    -------------------------------------
C                    |     CALLS TO EACH SPACE GROUP     |
C                    =====================================
C
      CALL GETSYM(NREF,IHIN,IKIN,JPOINT,IOUT)
      CALL P1STAT(NREF,IHIN,IKIN,IQIN,AMP,PHSIN,IQMAX,
     .  SYMRS(1),NSYMRS(1),THEOR(1),NTHEOR(1))
      DO 500 J=2,21
        IF(IOUT(J)) THEN
                CALL OTREFN(J,NREF,IHIN,IKIN,IQIN,
     .          JPOINT(1,1,J),JSIMPL(J),JSCREW(J),JH180(J),JK180(J),
     .          SYMBOL(J),AMP,PHSIN,BSH,BSK,
     .          SEARCH,REFINE,TILT,NCYC,ILIST,
     .          ORIGH,ORIGK,TXIN,TYIN,STEP,ISIZE,CS,WL,ASTAR,BSTAR,GAMMA,
     .          SYMRS(J),NSYMRS(J),THEOR(J),NTHEOR(J),
     .          SPGORG(1,J))
        ENDIF
500   CONTINUE
C
C
C  Output of final table comparing all space group residuals.
CHENN>
      open(11,FILE=OUTFILENAM,STATUS='NEW')
CHENN<
C
      WRITE(6,2000)
CHENN>
      write(11,2110)
CHENN<
2000  FORMAT(/,' SPACEGROUP       Phase resid(No)  Phase resid(No) ',
     .  '    OX     OY      TX     TY     Target residual based on',/,
     .  '                  v.other spots    v.theoretical',
     .  36X,'statistics taking ',/,
     .  '                   (90 random)       (45 random)',
     .  36X,'Friedel weight into account',/)
C
2110  FORMAT('::',/,
     .  ':: SPACEGROUP   Phs.Res. (#)    Phs.Res. (#)     ',
     .                    'OX     OY      TX     TY   Target',/,
     .  '::              v.other spots   v.theoretical',/,
     .  '::               (90 random)     (45 random)',/,
     .   '::',80('-'))
C
C  SPACEGROUP   Phs.Res. (#)    Phs.Res. (#)     OX     OY      TX     TY   Target
C               v.other spots   v.theoretical
C                (90 random)     (45 random)
C --------------------------------------------------------------------------------
C   1  p1         28.4    58     20.9    58
C   2  p2         35.0*   29     17.5    58   -131.3  115.3    0.00   0.00   41.9
C   3b p12_b      30.8*   17     13.7     6    -37.7 -180.0    0.00   0.00   30.8
C   3a p12_a      40.2    16      1.2     4    -30.0 -156.4    0.00   0.00   30.1
C   4b p121_b     50.4    17     13.4     6   -128.0 -114.0    0.00   0.00   30.8
C   4a p121_a     39.8`   16      2.6     4     45.0  114.7    0.00   0.00   30.1
C   5b c12_b      30.8*   17     13.7     6    -37.7 -180.0    0.00   0.00   30.8
C   5a c12_a      40.2    16      1.2     4    -30.0 -156.4    0.00   0.00   30.1
C   6  p222       36.2!   62     18.6    58    -38.0 -156.5    0.00   0.00   34.7
C   7b p2221b     35.3*   62     17.7    58     51.9  -65.5    0.00   0.00   34.7
C   7a p2221a     40.8`   62     17.7    58     51.9  -65.7    0.00   0.00   34.7
C   8  p22121     41.9`   62     18.6    58    -38.1 -156.4    0.00   0.00   34.7
C   9  c222       36.2!   62     18.6    58    -38.0 -156.5    0.00   0.00   34.7
C  10  p4         23.2*   65     18.4    58   -127.0  113.6    0.00   0.00   34.4
C  11  p422       44.3   132     18.2    58   -127.1  -66.0    0.00   0.00   31.4
C  12  p4212      46.8   132     18.3    58   -127.1  113.6    0.00   0.00   31.4
C  13  p3         41.2    18      --     --    -39.8 -153.3    0.00   0.00   28.4
C  14  p312       52.9    47      4.6    10    -39.8 -153.3    0.00   0.00   29.8
C  15  p321       54.4    50     14.1    16    -40.4   26.0    0.00   0.00   30.6
C  16  p6         43.7`   65     18.9    58    141.2 -156.1    0.00   0.00   34.4
C  17  p622       52.9   126     21.1    58    140.2 -154.0    0.00   0.00   31.5
C --------------------------------------------------------------------------------
C                     * = acceptable
C                     ! = should be considered
C                     ` = possibility
C 
C  OX,OY = best phase origin for this symmetry
C  TX,TY = best beam tilt for this symmetry
C  Target = target resid. based on statistics, taking Friedel weight into account
C 
      WRITE(6,2004) SPGID(1),SYMBOL(1),SYMRS(1),NSYMRS(1),
     .  THEOR(1),NTHEOR(1)
CHENN>
      WRITE(11,2104) SPGID(1),SYMBOL(1),SYMRS(1),NSYMRS(1),
     .  THEOR(1),NTHEOR(1)
      rdiffmin=999999999.9
CHENN<
      DO 2010 J=2,21
        IFLAG=' '
        IF(IOUT(J)) THEN
          TARGET=(SYMRS(1)*(NSYMRS(J)-NTHEOR(J)/2.0) + 
     .      THEOR(1)*NTHEOR(J))/NSYMRS(J)
CHENN>
         rdiff=SYMRS(J)-TARGET
         if(rdiff.lt.rdiffmin)then
           rdiffmin=rdiff
           jbest=J
         endif
CHENN<
C                                       ! within 10 degrees
          IF(SYMRS(J).LE.TARGET + 10.) IFLAG='`'
C                                       ! within  5 degrees
          IF(SYMRS(J).LE.TARGET + 5.0) IFLAG='!'
C                                       ! within  1 degrees
          IF(SYMRS(J).LE.TARGET + 1.0) IFLAG='*'
          IF(J.EQ.17) THEN
            WRITE(6,2003) SPGID(J),SYMBOL(J),SYMRS(J),
     .        IFLAG,NSYMRS(J),(SPGORG(K,J),K=1,4),TARGET
CHENN>
            WRITE(11,2103) SPGID(J),SYMBOL(J),SYMRS(J),
     .        IFLAG,NSYMRS(J),(SPGORG(K,J),K=1,4),TARGET
CHENN<
          ELSE
            WRITE(6,2001) SPGID(J),SYMBOL(J),SYMRS(J),IFLAG,
     .        NSYMRS(J),THEOR(J),NTHEOR(J),
     .        (SPGORG(K,J),K=1,4),TARGET
CHENN>
            WRITE(11,2101) SPGID(J),SYMBOL(J),SYMRS(J),IFLAG,
     .        NSYMRS(J),THEOR(J),NTHEOR(J),
     .        (SPGORG(K,J),K=1,4),TARGET
CHENN<
          ENDIF
        ENDIF
2001    FORMAT(3X,A4,1X,A6,F11.1,A1,I5,F11.1,I6,2X,2F7.1,1X,2F7.2,F11.1)
C                                               ! and for p3 alone
2003    FORMAT(3X,A4,1X,A6,F11.1,A1,I5,8X,'--',5X,'--',
     .    2X,2F7.1,1X,2F7.2,F11.1)
2004    FORMAT(3X,A4,1X,A6,F11.1,I6,F11.1,I6)
2010  CONTINUE
      WRITE(6,2100)
CHENN>
      WRITE(11,2200)
CHENN<
2100  FORMAT(/,15X,'* = acceptable',/,15X,'! = should be considered',/,
     .  15X,'` = possibility',/,/,/,/,/)
CHENN>
2200  FORMAT('::',80('-'),/,
     .  '::                    * = acceptable',/,
     .  '::                    ! = should be considered',/,
     .  '::                    ` = possibility',/,
     .  '::',/,
     .  ':: OX,OY = best phase origin for this symmetry',/,
     .  ':: TX,TY = best beam tilt for this symmetry',/,
     .  '::Target = target resid. based on statistics, taking ',
     .     'Friedel weight into account',/,
     .  '::',/,/)
      close(11)
      open(12,FILE=SPCGRPFILENAM,STATUS='NEW')
      J=jbest
      IF(J.EQ.17) THEN
        WRITE(12,2203) SPGID(J),SYMBOL(J),SYMRS(J),
     .    NSYMRS(J),(SPGORG(K,J),K=1,4),TARGET
      ELSE
        WRITE(12,2201) SPGID(J),SYMBOL(J),SYMRS(J),
     .    NSYMRS(J),THEOR(J),NTHEOR(J),
     .    (SPGORG(K,J),K=1,4),TARGET
      ENDIF
      close(12)
2101    FORMAT('::',A4,1X,A6,F9.1,A1,I5,F9.1,I6,2X,2F7.1,1X,2F7.2,F7.1)
C                                               ! and for p3 alone
2103    FORMAT('::',A4,1X,A6,F9.1,A1,I5,6X,'--',5X,'--',
     .    2X,2F7.1,1X,2F7.2,F7.1)
2201    FORMAT(3X,A4,1X,A6,F11.1,I5,F11.1,I6,2X,2F7.1,1X,2F7.2,F11.1)
C                                               ! and for p3 alone
2203    FORMAT(3X,A4,1X,A6,F11.1,I5,8X,'--',5X,'--',
     .    2X,2F7.1,1X,2F7.2,F11.1)
2104    FORMAT('::',A4,1X,A6,F9.1,I6,F9.1,I6)
CHENN<
      END
C###############################################################################
      SUBROUTINE GETSYM(NREF,IH,IK,JPOINT,IOUT)
C
C       The function of this subroutine is to produce a list of pointers in
C       array JPOINT, giving the reflections which should have phases related
C       by symmetry to each of the NREF reflections in the entire dataset.  The
C       reflections with simple phase identity are stored in the lowest 
C       locations and those which involve an origin shift and therefore possible
C       phase relationships of 180*(H and/or K) at the higher locations.  Only
C       relationships with reflections later in the list are used, to avoid 
C       counting each relationship twice.
C
      PARAMETER (NMAX=6000)
      LOGICAL IOUT(21)
      DIMENSION IH(NMAX),IK(NMAX),JPOINT(11,NMAX,21),IPOINT(15,NMAX),
     .          ISYM(2,2,15),LOOK(15,21)

      DATA ISYM/-1, 0, 0, 1,
     A           1, 0, 0,-1,
     B          -1, 0, 0,-1,
     C           0, 1, 1, 0,
     D           0, 1,-1, 0,
     E           0,-1, 1, 0,
     F           0,-1,-1, 0,
     G           1, 0,-1,-1,
     H          -1, 0, 1, 1,
     I           0, 1,-1,-1,
     J           0,-1, 1, 1,
     K          -1,-1, 1, 0,
     L           1, 1,-1, 0,
     M          -1,-1, 0, 1,
     N           1, 1, 0,-1 /

      DATA LOOK / 15*0,0,0,1,12*0,1,14*0,0,1,13*0,2,14*0,0,2,13*0,
     .          1,14*0,0,1,13*0,1,1,1,12*0,2,2,1,12*0,2,2,1,12*0,
     .          2,2,1,12*0,1,1,1,12*0,0,0,1,0,1,1,9*0,7*1,8*0,
     .          2,2,1,1,2,2,1,8*0,9*0,1,0,1,0,0,0,
     .          6*0,1,0,1,1,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1,0,1,0,1,0,
     .          0,0,1,6*0,4*1,0,0,0,0,1,1,0,0,9*1      /
C!!!   DATA LOOK / 15*0,
C     A         0,0,1,12*0,
C     B         1,14*0,
C     C         0,1,13*0,
C     D         2,14*0,
C     E         0,2,13*0,
C     F         1,14*0,
C     G         0,1,13*0,
C     H         1,1,1,12*0,
C     I         2,2,1,12*0,
C     J         2,2,1,12*0,
C     K         2,2,1,12*0,
C     L         1,1,1,12*0,
C     M         0,0,1,0,1,1,9*0,
C     N         7*1,8*0,
C     O         2,2,1,1,2,2,1,8*0,
C     P         9*0,1,0,1,0,0,0,
C     Q         6*0,1,0,1,1,0,1,0,0,1,
C     R         0,0,0,1,0,0,0,1,0,1,0,1,0,1,0,
C     S         0,0,1,6*0,4*1,0,0,
C     T         0,0,1,1,0,0,9*1      /
C*** initialization added by jms 06.03.96
       do j=1,nmax
         do i=1,15
          ipoint(i,j) = 0.
         end do
        end do
C
C  First set up the array IPOINT to find all symmetry-related reflections
C
      DO 100 I=1,NREF-1
        DO 90 JSYM=1,15
                JH = IH(I)*ISYM(1,1,JSYM) + IK(I)*ISYM(2,1,JSYM)
                JK = IH(I)*ISYM(1,2,JSYM) + IK(I)*ISYM(2,2,JSYM)
C                       ! look only at reflections later in list
                DO 80 J=I+1,NREF
                        IF(IH(J).EQ.JH.AND.IK(J).EQ.JK) THEN
                                IPOINT(JSYM,I) = J
                        ENDIF
80              CONTINUE
90      CONTINUE
100   CONTINUE
C
C  Next transfer symmetry point into spacegroup-specific array JPOINT
C
      WRITE(6,201)
      DO 200 ISPG=1,21
        IF(IOUT(ISPG)) THEN
                ICOUNT=0
                NSIMPL=0
                NSCREW=0
C                       ! first simple relations
                DO 160 J=1,15
                        IF(LOOK(J,ISPG).EQ.1) THEN
                                ICOUNT=ICOUNT+1
                                DO 150 K=1,NREF
                                        IP=IPOINT(J,K)
                                        IF(IP.NE.0) THEN
                                                JPOINT(ICOUNT,K,ISPG)=IP
                                                NSIMPL=NSIMPL+1
                                        ENDIF
150                             CONTINUE
                        ENDIF
160             CONTINUE
C                       ! second screw relations
                DO 180 J=1,15
                        IF(LOOK(J,ISPG).EQ.2) THEN
                                ICOUNT=ICOUNT+1
                                DO 170 K=1,NREF
                                        IP=IPOINT(J,K)
                                        IF(IP.NE.0) THEN
                                                JPOINT(ICOUNT,K,ISPG)=IP
                                                NSCREW=NSCREW+1
                                        ENDIF
170                             CONTINUE
                        ENDIF
180             CONTINUE
                WRITE(6,202) ISPG,NSIMPL,NSCREW,(NSIMPL+NSCREW)
        ENDIF
200   CONTINUE
201   FORMAT(' Number of comparisons possible',/,
     .  '  Symmetry #   No.simple    No.screw   Total No.comparisons')
202   FORMAT(4I12)
      RETURN
      END
C###############################################################################
      SUBROUTINE P1STAT(NREF,IHIN,IKIN,IQIN,AMP,PHSIN,IQMAX,
     .  SYMRS,NSYMRS,THEOR,NTHEOR)
      PARAMETER (NMAX=6000)
      PARAMETER (NSLOTS=64)
      DIMENSION IHIN(NMAX),IKIN(NMAX),IQIN(NMAX),PHSIN(NMAX),
     .          AMP(NMAX),QIQERR(9)
C       DIMENSION STATEMENTS FOR RESOLUTION DEPENDENT PHASE RESIDUAL HISTOGRAM
C!!!
C       DIMENSION NRESO(NSLOTS),SERRES(NSLOTS),ERRES(NSLOTS),
C     .         SRESW(NSLOTS),WGTRES(NSLOTS),AVIQ(NSLOTS)
        DATA IRESTP/50/
      DATA DRAD/0.0174533/
      DATA QIQERR/5.0,12.3,20.5,28.6,36.8,45.0,53.2,71.6,90.0/
C
      WRITE(6,1000)
1000  FORMAT(/,' *** ENTERING P1STAT SUBROUTINE *****',/)
        THEOR=0.0
        NTHEOR=0
        SYMRS=0.0
        NSYMRS=0
      DO 50 J=1,NREF
        IF(IQIN(J).LE.IQMAX) THEN
                THEOR=THEOR + QIQERR(IQIN(J))
                NTHEOR=NTHEOR + 1
        ENDIF
50    CONTINUE
      IF(NTHEOR.NE.0) THEOR=THEOR/NTHEOR
      SYMRS=ATAN(SQRT(2.0)*TAN(DRAD*THEOR))/DRAD
      NSYMRS=NTHEOR
      RETURN
      END
C###############################################################################
      SUBROUTINE OTREFN(NSPG,NREF,IH,IK,IQ,
     .  JPOINT,JSIMPL,JSCREW,JH180,JK180,
     .  SYMBOL,AMP,PHASE,BSH,BSK,
     .  SEARCH,REFINE,TILT,NCYC,ILIST,
     .  ORIGH,ORIGK,TXIN,TYIN,STEP,ISIZE,CS,WL,ASTAR,BSTAR,GAMMA,
     .  SYMRS,NSYMRS,THEOR,NTHEOR,SPGORG)
C
C   Original subroutine 10.10.93
C
      PARAMETER (NMAX=6000)
      PARAMETER (ISMAX=361)
      PARAMETER (NSLOTS=64)
      DOUBLE PRECISION A(4,4),B(4),W(100),E,SYMBOL
      DIMENSION IH(NMAX),IK(NMAX),IQ(NMAX),PHASE(NMAX),PHSCOR(NMAX),
     .          AMP(NMAX),JPOINT(11,NMAX),BSH(NMAX),BSK(NMAX),
     .          DIFF(ISMAX,ISMAX),IDIFF(ISMAX,ISMAX)
      DIMENSION PARAMS(4),FUNCDF(4,2)
      LOGICAL ILIST,SEARCH,REFINE,TILT
C       DIMENSION STATEMENTS FOR RESOLUTION DEPENDENT PHASE RESIDUAL HISTOGRAM
        DIMENSION NRESO(NSLOTS),SERRES(NSLOTS),ERRES(NSLOTS),
     .          SRESW(NSLOTS),WGTRES(NSLOTS),AVIQ(NSLOTS)
      DIMENSION SPGORG(4)
      EQUIVALENCE (PARAMS(1),OXNEW),(PARAMS(2),OYNEW),(PARAMS(3),TX),
     .                  (PARAMS(4),TY)
      DATA IRESTP/50/
      DATA DORIGN/0.02/,DTILT/0.001/,DRAD/0.0174533/,FRACT/0.2/
C*** initialization installed by jms 11.03.96
      do i=1,nslots
          aviq(i) = 0.
        end do
      WRITE(6,1000) NSPG,SYMBOL
1000  FORMAT('0*** Entering OrigTiltREFiNe SYMMETRY #',
     .   I3,4X,A6,6('*'),/)
      TX=TXIN
      TY=TYIN
      ISIZE=(ISIZE/2)*2 +1
      IHALF=(ISIZE/2)+1
C
C  NOW PHASE ORIGIN SEARCH ***********************************************
      IF(SEARCH) THEN
        WRITE(6,1001)
1001    FORMAT(/,' ******* PHASE ORIGIN SEARCH BEGINNING *************')
        DO 100 J=1,ISIZE
        DO 100 K=1,ISIZE
                SH=ORIGH + STEP*(J-IHALF)
                SK=ORIGK + STEP*(K-IHALF)
                SUMDIF=0.0
                NCOMP=0
                DO 50 M=1,NREF
                  P1=PHASE(M)+PHSHFT(IH(M),IK(M),SH,SK,TX,TY,BSH(M),BSH(M))
                  DO 45 M1=1,JSIMPL
                        N=JPOINT(M1,M)
                        IF(N.EQ.0) GO TO 45
                        P2=PHASE(N)+PHSHFT(IH(N),IK(N),SH,SK,TX,TY,BSH(N),BSH(N))
                        NCOMP=NCOMP+1
                        SUMDIF=SUMDIF+PHSDIF(P1,P2)
45                CONTINUE
                  DO 49 M1=1+JSIMPL,JSIMPL+JSCREW
                        N=JPOINT(M1,M)
                        IF(N.EQ.0) GO TO 49
                        P2=PHASE(N)+PHSHFT(IH(N),IK(N),SH,SK,TX,TY,BSH(N),BSH(N))
     .                          + IH(N)*JH180 + IK(N)*JK180
                        NCOMP=NCOMP+1
                        SUMDIF=SUMDIF+PHSDIF(P1,P2)
49                CONTINUE
50              CONTINUE
100     DIFF(J,K)=SUMDIF/NCOMP
C               ! DUMMY MAXIMUM AT START OF SEARCH
        PHSERR=180.0
        DO 200 J=1,ISIZE
        DO 200 K=1,ISIZE
          IF(DIFF(J,K).LT.PHSERR) THEN
                PHSERR=DIFF(J,K)
                IHPMIN=J
                IKPMIN=K
          ENDIF
          DIFF(J,K)=DIFF(J,K)/10.0
          DIFF(J,K)=MIN(DIFF(J,K),9.0)
200     IDIFF(J,K)=(9.0-DIFF(J,K))
        WRITE(6,13) ORIGH,ORIGK,STEP
13    FORMAT(/,' THE MATRIX BELOW IS CENTRED ABOUT AN '
     .   'ORIGIN WITH A PHASE
     1   SHIFT OF ',/,F10.2,'  FOR THE (1,0) REFLECTION',/,F10.2,
     2'  FOR THE (0,1) REFLECTION',/,F10.2,'  STEP SIZE')
14      FORMAT(1X,121I1)
        DO 300 K=1,ISIZE
300     WRITE(6,14)(IDIFF(J,K),J=1,ISIZE)
        OXNEW=ORIGH+STEP*(IHPMIN-IHALF)
        OYNEW=ORIGK+STEP*(IKPMIN-IHALF)
        WRITE(6,15) OXNEW,OYNEW
15    FORMAT(/,' BEST PHASE SHIFTS ARE ',/,F10.2,
     1 '  FOR THE (1,0) REFLECTION',/,F10.2,
     .  '  FOR THE (0,1) REFLECTION',/,
     2 ' NOTE THAT THESE SHIFTS INCLUDE THE INITIAL SHIFTS AS WELL',
     3 ' AS THE ADDITIONAL REFINED SHIFTS')
        WRITE(6,17) PHSERR,SYMBOL
17      FORMAT(/,'  PHASE ERROR AT MINIMUM IS',F10.1,' DEGREES in ',A6)
      ELSE
        OXNEW=ORIGH
        OYNEW=ORIGK
      ENDIF
C
C  NOW PHASE ORIGIN DISTANCE FUNCTION REFINEMENT ******************************
      IF(REFINE) THEN
        WRITE(6,1002)
1002    FORMAT(/,/,' ******* ORIGIN REFINEMENT BEGINNING *******',/,
     .  ' ICYC        NEW ORIGIN       ---SHIFTS---   RESIDUAL',
     .  '    FUNCMIN     NCOMP   N.GT.90')
        ICYC=0
311     ICYC=ICYC+1
        IF(ICYC.GT.NCYC) GO TO 1005
        DO 310 I=1,2
        B(I)=0.0
        DO 310 J=1,2
310     A(I,J)=0.0
        NCOMP=0
        NFAR=0
        RESID=0.0
        FUNCMN=0.0
        DO 320 M=1,NREF
        PM=PHASE(M)+PHSHFT(IH(M),IK(M),OXNEW,OYNEW,TX,TY,BSH(M),BSH(M))
          DO 315 M1=1,JSIMPL
            N=JPOINT(M1,M)
            IF(N.EQ.0) GO TO 315
        PN=PHASE(N)+PHSHFT(IH(N),IK(N),OXNEW,OYNEW,TX,TY,BSH(N),BSH(N))
            CALL CALCFN(PM,PN,NFAR,NCOMP,RESID,FUNCMN)
315       CONTINUE
          DO 319 M1=1+JSIMPL,JSIMPL+JSCREW
            N=JPOINT(M1,M)
            IF(N.EQ.0) GO TO 319
        PN=PHASE(N)+PHSHFT(IH(N),IK(N),OXNEW,OYNEW,TX,TY,BSH(N),BSH(N))
     .                          + IH(N)*JH180 + IK(N)*JK180
            CALL CALCFN(PM,PN,NFAR,NCOMP,RESID,FUNCMN)
319       CONTINUE
320     CONTINUE
          DO 326 I=1,2
          DO 326 J=1,2
                FUNCDF(I,J)=0.0
                OX=OXNEW+(2-I)*(2*J-3)*DORIGN
                OY=OYNEW+(I-1)*(2*J-3)*DORIGN
          DO 324 M=1,NREF
                PM=PHASE(M)+PHSHFT(IH(M),IK(M),OX,OY,TX,TY,BSH(M),BSH(M))
                DO 325 M1=1,JSIMPL
                N=JPOINT(M1,M)
                IF(N.EQ.0) GO TO 325
                PN=PHASE(N)+PHSHFT(IH(N),IK(N),OX,OY,TX,TY,BSH(N),BSH(N))
                PDIFF=PHSDIF(PM,PN)
                FUNCDF(I,J)=FUNCDF(I,J)+2.0*SIN(PDIFF*DRAD/2.0)
325             CONTINUE
                DO 323 M1=1+JSIMPL,JSIMPL+JSCREW
                N=JPOINT(M1,M)
                IF(N.EQ.0) GO TO 323
                PN=PHASE(N)+PHSHFT(IH(N),IK(N),OX,OY,TX,TY,BSH(N),BSH(N))
     .                          + IH(N)*JH180 + IK(N)*JK180
                PDIFF=PHSDIF(PM,PN)
                FUNCDF(I,J)=FUNCDF(I,J)+2.0*SIN(PDIFF*DRAD/2.0)
323             CONTINUE
324     CONTINUE
326     CONTINUE
        DO 328 I=1,2
        B(I)=-(FUNCDF(I,2)-FUNCDF(I,1))/(2.0*DORIGN*NCOMP)
        A(I,I)=2.0*DRAD
328     CONTINUE
C      WRITE(6,*) FUNCMN,((FUNCDF(I,J),J=1,2),I=1,2)
        RESID=RESID/NCOMP
        FUNCMN=90.0*FUNCMN/NCOMP
                IA=4
                N=2
                E=-1.0
                CALL MA21AD(A,IA,N,B,W,E)
                IF(E.EQ.0.0) GO TO 860
                WRITE(6,861)E
861             FORMAT('  MA21AD FAILED',F10.5)
860     CONTINUE
      DO 865 I=1,2
        SHIFT=B(I)
        IF(ABS(SHIFT).GT.2.0) SHIFT=SIGN(2.0,SHIFT)
865     PARAMS(I)=PARAMS(I)+SHIFT*FRACT
        IF(ICYC.LE.10.OR.ICYC/11*11.EQ.ICYC) THEN
        WRITE(6,1003)ICYC,OXNEW,OYNEW,B(1),B(2),RESID,FUNCMN,
     .          NCOMP,NFAR
        ENDIF
1003    FORMAT(I5,3F10.2,F7.2,2F10.2,2I10)
        IF(ABS(B(1)).GT.0.02.OR.ABS(B(2)).GT.0.02) GO TO 311
1005    WRITE(6,1004) SYMBOL,OXNEW,OYNEW,RESID,NCOMP
1004    FORMAT(/,'  REFINEMENT OF PHASE ORIGIN COMPLETED in ',A6,/,
     .  '   BEST ORIGIN =',2F10.2,'  DEGREES',/,
     .  '   RESIDUAL PHASE DIFF =',F10.2,'  DEG FOR',I5,' COMPARISONS')
      ENDIF
C
C  NOW PHASE ORIGIN AND BEAM TILT REFINEMENT TOGETHER *************************
C
      IF(TILT) THEN
        WRITE(6,1010)
1010    FORMAT(/,/,' ******* BEAM TILT REFINEMENT BEGINNING *********',/,
     .  ' ICYC      OX        OY        TX        TY   ------------',
     .  ' SHIFTS -----------  RESIDUAL    FUNCMIN     NCOMP ',
     .  ' N.GT.90')
        ICYC=0
331     ICYC=ICYC+1
        IF(ICYC.GT.NCYC) GO TO 1013
        DO 330 I=1,4
        B(I)=0.0
        DO 330 J=1,4
330     A(I,J)=0.0
        NCOMP=0
        NFAR=0
        RESID=0.0
        FUNCMN=0.0
        DO 340 M=1,NREF
        PM=PHASE(M)+PHSHFT(IH(M),IK(M),OXNEW,OYNEW,TX,TY,BSH(M),BSH(M))
          DO 335 M1=1,JSIMPL
                N=JPOINT(M1,M)
                IF(N.EQ.0) GO TO 335
        PN=PHASE(N)+PHSHFT(IH(N),IK(N),OXNEW,OYNEW,TX,TY,BSH(N),BSH(N))
                CALL CALCFN(PM,PN,NFAR,NCOMP,RESID,FUNCMN)
335       CONTINUE
          DO 339 M1=1+JSIMPL,JSIMPL+JSCREW
                N=JPOINT(M1,M)
                IF(N.EQ.0) GO TO 339
        PN=PHASE(N)+PHSHFT(IH(N),IK(N),OXNEW,OYNEW,TX,TY,BSH(N),BSH(N))
     .                          + IH(N)*JH180 + IK(N)*JK180
                CALL CALCFN(PM,PN,NFAR,NCOMP,RESID,FUNCMN)
339       CONTINUE
340     CONTINUE
C***
        dum = 0.
          DO 346 I=1,4
          DO 346 J=1,2
                FUNCDF(I,J)=0.0
                OX=OXNEW
                OY=OYNEW
                TXX=TX
                TYY=TY
                IF(I.EQ.1)OX=OXNEW+(2*J-3)*DORIGN
                IF(I.EQ.2)OY=OYNEW+(2*J-3)*DORIGN
                IF(I.EQ.3)TXX=TX+(2*J-3)*DTILT
                IF(I.EQ.4)TYY=TY+(2*J-3)*DTILT
          DO 344 M=1,NREF
        PM=PHASE(M)+PHSHFT(IH(M),IK(M),OX,OY,TXX,TYY,BSH(M),BSH(M))
                DO 342 M1=1,JSIMPL
                        N=JPOINT(M1,M)
                        IF(N.EQ.0) GO TO 342
                PN=PHASE(N)+PHSHFT(IH(N),IK(N),OX,OY,TXX,TYY,BSH(N),BSH(N))
                        CALL CALCFN(PM,PN,IDUM,IDUM,DUM,FUNCDF(I,J))
342             CONTINUE
                DO 343 M1=1+JSIMPL,JSIMPL+JSCREW
                        N=JPOINT(M1,M)
                        IF(N.EQ.0) GO TO 343
                PN=PHASE(N)+PHSHFT(IH(N),IK(N),OX,OY,TXX,TYY,BSH(N),BSH(N))
     .                          + IH(N)*JH180 + IK(N)*JK180
                        CALL CALCFN(PM,PN,IDUM,IDUM,DUM,FUNCDF(I,J))
343             CONTINUE
344     CONTINUE
346     CONTINUE
C      WRITE(6,*)FUNCMN,((FUNCDF(I,J),J=1,2),I=1,4)
        DO 345 I=1,4
        DSTEP=DORIGN
        IF(I.GE.3)DSTEP=DTILT
        B(I)=-(FUNCDF(I,2)-FUNCDF(I,1))/(2.0*DSTEP*NCOMP)
        A(I,I)=2.0*DRAD
        IF(I.GE.3)A(I,I)=A(I,I)*70.0
345     CONTINUE
                IA=4
                N=4
                E=-1.0
                CALL MA21AD(A,IA,N,B,W,E)
                IF(E.EQ.0.0) GO TO 1860
                WRITE(6,861)E
1860    CONTINUE
        DO 1865 I=1,4
        SHIFT=B(I)
        IF(ABS(SHIFT).GT.0.5) SHIFT=SIGN(0.5,SHIFT)
1865    PARAMS(I)=PARAMS(I)+SHIFT*FRACT
        RESID=RESID/NCOMP
        FUNCMN=90.0*FUNCMN/NCOMP
        IF(ICYC.LE.10.OR.ICYC/11*11.EQ.ICYC) THEN
        WRITE(6,1011)ICYC,OXNEW,OYNEW,TX,TY,(B(I),I=1,4),RESID,
     .                  FUNCMN,NCOMP,NFAR
        ENDIF
1011    FORMAT(I5,2F10.2,2F10.3,F10.2,F7.2,2F7.3,2F10.2,I10,I6)
        IF(ABS(B(1)).GT.0.02.OR.ABS(B(2)).GT.0.02) GO TO 331
        IF(ABS(B(3)).GT.0.0005.OR.ABS(B(4)).GT.0.0005) GO TO 331
1013    BEAMSH = ASTAR*0.360*CS*10.0**7*WL**2*ASTAR**2*225.0
        BEAMSK = BSTAR*0.360*CS*10.0**7*WL**2*BSTAR**2*225.0
        PS1=ABS(PHSHFT(15,0,0.,0.,TX,TY,BEAMSH,BEAMSK))
        PS2=ABS(PHSHFT(0,15,0.,0.,TX,TY,BEAMSH,BEAMSK))
        PMAX15=AMAX1(PS1,PS2)
        WRITE(6,1012) SYMBOL,OXNEW,OYNEW,TX,TY,RESID,NCOMP,PMAX15
1012    FORMAT(/,' FINAL REFINEMENT OF ORIGIN AND BEAMTILT in ',A6,/,/,
     .  '                      ORIGH ====',F10.2,'   DEG',/,
     .  '                      ORIGK ====',F10.2,'   DEG',/,
     .  '                      TILTH ====',F10.3,'   MILLIRADIANS',/,
     .  '                      TILTK ====',F10.3,'   MILLIRADIANS',/,
     .  '                      RESIDUAL =',F10.2,'  DEG FOR',I5,
     .  '  COMPARISONS',/,/,
     .  ' THIS AMOUNT OF BEAMTILT CAUSES THE MAX CORRECTION OF PHASE',
     .  ' TO A REFLECTION AT RADIUS OF (15,0) OF',F10.3,' DEGREES')
      ENDIF
C
C  STATISTICAL HISTOGRAM ANALYSIS OF BEST SOLUTION AND
C  OUTPUT OF PHASES AFTER BEST ORIGIN SHIFT ***********************************
C**
C       CALCULATION OF RESIDUAL AS FUNCTION OF RESOLUTION.
C
C       CLEAR ARRAYS FOR HISTOGRAM
      WRITE(6,18011) OXNEW,OYNEW,TX,TY
18011 FORMAT('  Final values OX,OY,TX,TY =',2F10.2,2F10.3)
C               ! next 4 lines stored for output by main program
        SPGORG(1)=OXNEW
        SPGORG(2)=OYNEW
        SPGORG(3)=TX
        SPGORG(4)=TY
      DO 18010 J=1,NSLOTS
C               ! ZERO HIST0GRAM
        NRESO(J)=0
        WGTRES(J)=0.0
        SRESW(J)=0.0
        ERRES(J)=0.0
18010 SERRES(J)=0.0
        THEOR=0.0
        NTHEOR=0
C
      DO 1340 M=1,NREF
        LH=IH(M)
        LK=IK(M)
C               CALCULATE RESOLUTION OF SPOT
        RAD=SQRT(LH**2*ASTAR**2+LK**2*BSTAR**2 +
     .          2.0*LH*LK*ASTAR*BSTAR*COS(GAMMA))
        DSTAR2 = RAD**2
        IRES=DSTAR2*10000.
        ISLOT= 1 + (IRES-1)/IRESTP
        IF(ISLOT.LT.1.OR.ISLOT.GE.NSLOTS) THEN
                WRITE(6,20000)ISLOT
20000           FORMAT(':: ERROR, ISLOT=',I10)
                STOP
        END IF
        PM=PHASE(M)+PHSHFT(LH,LK,OXNEW,OYNEW,TX,TY,BSH(M),BSH(M))
C
C  test for 0/180 for statistics only
        IF((NSPG.EQ.2.OR.NSPG.GE.20.OR.(NSPG.GE.9.AND.NSPG.LE.16)).OR.
     .    (NSPG.EQ.19.AND.(LK.EQ.0.OR.LH.EQ.0.OR.LH.EQ.-LK)).OR.
     .    (NSPG.EQ.18.AND.(LH.EQ.LK.OR.LK.EQ.-2*LH.OR.LH.EQ.-2*LK)).OR.
     .    (LK.EQ.0.AND.(NSPG.EQ.3.OR.NSPG.EQ.5.OR.NSPG.EQ.7)).OR.
     .    (LH.EQ.0.AND.(NSPG.EQ.4.OR.NSPG.EQ.6.OR.NSPG.EQ.8))) THEN
C                       ! calculation for closeness to 0/180
                PDIFF=AMOD(PM,180.)
                IF(ABS(PDIFF).GT.90.) PDIFF=PDIFF-SIGN(180.,PDIFF)
                DELTA=ABS(PDIFF)
                THEOR=THEOR+DELTA
                NTHEOR=NTHEOR+1
        ENDIF
        SIGMAM=AMAX1(FLOAT(IQ(M)),2.5)
        DO 1335 M1=1,JSIMPL+JSCREW
                N=JPOINT(M1,M)
                IF(N.EQ.0) GO TO 1335
                PN=PHASE(N)+PHSHFT(IH(N),IK(N),OXNEW,OYNEW,TX,TY,BSH(N),BSH(N))
                IF(M1.GT.JSIMPL) PN = PN + IH(N)*JH180 + IK(N)*JK180
                SIGMAN=AMAX1(FLOAT(IQ(N)),2.5)
                WEIGHT=1.0/(SIGMAM*SIGMAN)
                DELTA=PHSDIF(PM,PN)
C***            SERR=DELTA+SERR
                SERRES(ISLOT)=SERRES(ISLOT)+DELTA
                SRESW(ISLOT)=SRESW(ISLOT)+DELTA*WEIGHT
                AVIQ(ISLOT) = AVIQ(ISLOT) + (IQ(M)+IQ(N))*0.5
                NRESO(ISLOT) = NRESO(ISLOT) + 1
                WGTRES(ISLOT)=WGTRES(ISLOT) + WEIGHT
1335    CONTINUE
1340   CONTINUE
C
C WRITE TABLE OF RESIDUAL AS FUNCTION OF RESOLUTION
C
        WRITE(6,10173)
10173   FORMAT(/,40X,'    PHASE RESIDUAL IN RESOLUTION RANGES',
     .    ' (random=90deg)',/)
        WRITE(6,10171)
10171  FORMAT(40X,' RANGE','     DMIN ','     DMAX ','   RESIDUAL',
     .'  NUMBER   RESIDWGT   AVIQ',/)
        NRESA=0
        SRESA=0.0
        DO 10175 I=1,NSLOTS
        IF(NRESO(I).EQ.0)GO TO 10175
        NRESA=NRESA+NRESO(I)
        SRESA=SERRES(I)+SRESA
        ERRES(I)=SERRES(I)/NRESO(I)
        SRESW(I)=SRESW(I)/WGTRES(I)
        AVIQ(I)=AVIQ(I)/NRESO(I)
        DMIN=SQRT(10000.0/((I-1)*IRESTP + 1))
        DMAX=SQRT(10000.0/(I*IRESTP))
        WRITE(6,10172)I,DMIN,DMAX,ERRES(I),NRESO(I),SRESW(I),AVIQ(I)
10175   CONTINUE
10172   FORMAT(40X,I6,3F10.3,I7,F12.3,F8.2)
        ERRESA=SRESA/NRESA
        IF(NTHEOR.NE.0) THEOR=THEOR/NTHEOR
        WRITE(6,10174) SYMBOL,ERRESA,NRESA,THEOR,NTHEOR
10174  FORMAT(/,/,40X,'OVERALL residual in ',A6,F10.3,I7,/,
     .    40X,'AND VERSUS 0/180',F22.3,I7,/,/)
C                       ! store for output to main program
         SYMRS = ERRESA
        NSYMRS = NRESA
C**
C
      IF(.NOT.ILIST) RETURN
      DO 400 J=1,NREF
      PHSCOR(J)=PHASE(J) +
     .  PHSHFT(IH(J),IK(J),OXNEW,OYNEW,TX,TY,BSH(J),BSH(J))
380   IF(PHSCOR(J).LT.0.0) GO TO 390
      IF(PHSCOR(J).GT.360.0) GO TO 395
      GO TO 400
390   PHSCOR(J)=PHSCOR(J)+360.0
      GO TO 380
395   PHSCOR(J)=PHSCOR(J)-360.0
      GO TO 380
400   CONTINUE
      WRITE(6,402)
      DO 450 J=1,NREF
      JWRITE=0
      DO 450 M1=1,JSIMPL+JSCREW
      K=JPOINT(M1,J)
C                         ! write at least one line per reflection
      IF(K.EQ.0.AND.JWRITE.EQ.0) THEN
        JWRITE=1
        WRITE(6,401)IH(J),IK(J),PHSCOR(J)
        GO TO 450
      ENDIF
      DELTA=PHSCOR(K)-PHSCOR(J)
      IF(M1.GT.JSIMPL) DELTA=DELTA + IH(J)*JH180 + IK(J)*JK180
      IF(DELTA.GT.180.0) DELTA=DELTA-360.0
      IF(DELTA.LT.-180.0) DELTA=DELTA+360.0
      WRITE(6,401)IH(J),IK(J),PHSCOR(J),
     .  IH(K),IK(K),PHSCOR(K),DELTA
401   FORMAT(2I5,F10.2,5X,2I5,2F10.2)
402   FORMAT(/,3X,'IH   IK     PHASE     IHCMP IKCMP  PHASECMP',
     .  ' PHASEDIFF')
450   CONTINUE
      RETURN
      END
C###############################################################################
      SUBROUTINE CALCFN(PM,PN,NFAR,NCOMP,RESID,FUNCMN)
        DATA DRAD/0.0174533/
        PDIFF=PHSDIF(PM,PN)
        IF(PDIFF.GT.90.0) NFAR=NFAR+1
        NCOMP=NCOMP+1
        RESID=RESID+PDIFF
        FUNCMN=FUNCMN+2.0*SIN(PDIFF*DRAD/2.0)
      RETURN
      END
C###############################################################################
      FUNCTION PHSHFT(IH,IK,OX,OY,TX,TY,BSH,BSK)
C       this function calculates phase-shift due to origin and beamtilt.
C       it assumes origin shift is in degrees, and beamtilt in milliradians.
C       BSH = ASTAR*0.360*(CS*10.0**7*WL**2*ASTAR**2*RADSQ-DEFOCUS)
      PHSHFT = IH*(OX+BSH*TX) + IK*(OY+BSK*TY)
      RETURN
      END
C###############################################################################
      FUNCTION PHSDIF(P1,P2)
C     finds absolute difference between any angles P1 and P2, measured in 
C     degrees, by shortest circular route.
        AP=AMOD((P1-P2),360.0)
48      IF(AP.GT.180.0) THEN
                AP=AP-360.0
                GO TO 48
        ENDIF
49      IF(AP.LT.-180.0) THEN
                AP=AP+360.0
                GO TO 49
        ENDIF
        PHSDIF=ABS(AP)
      RETURN
      END
C###############################################################################
