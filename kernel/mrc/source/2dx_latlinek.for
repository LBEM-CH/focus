C*LATLINEK.FOR ******************************************************************
C                                                                               *
C       This is a program to do fitting of curves to either                     *
C       Intensity or F,Phi data for individual layer lines.                     *
C       Individual weights are used for F's and phases (or I's)                 *
C       Normally a trial set of A,B for each lattice line are                   *
C       input (optionally this can be calculated internally).                   *
C       This trial set is then refined to minimize the weighted                 *
C       least-squares error between Fs and Phases.                              *
C                                                                               *
C       Version  1.01 LATLINE   18.01.82        DAA     VAX                     *
C       Version  1.03           17.02.82        DAA     VAX                     *
C       Version  1.10           25.02.82        DAA     VAX                     *
C       Version  1.11           09.03.82        DAA     VAX                     *
C       Version  1.20           23.10.82        DAA     VAX                     *
C       Version  1.21           07.11.82        DAA     VAX                     *
C       Version  1.22           10.12.82        DAA     VAX                     *
C       Version  1.23 LATLINEA  24.12.82        DAA     VAX                     *
C Version  1.24; 07.12.88; JMB; Option IWP=-2: sigphi input in degrees          *
C                               Guessed phase is weighted                       *
C                               AK adjusted by factor nfobs/nphases             *
C                               Symbols on phase plot show input sigphi         *
C Version 1.25 LATLINEB 15.3.91         RH   removed determinant test           *
C Version 1.26          20.9.93         RH   allow NPARM=1, for NOBS>0          *
C Version 1.27 LATLINEC 21.9.93         RH   min phase sigma 1.0 degree         *
C Version 1.28 LATLINED  5.8.94         RH   overall statistics added           *
C Version 1.29          30.12.94        RH   debug CLNFIT - WPHI                *
C Version 1.30          31.12.94        RH   debug phase residual (NP)          *
C Version 1.31           2.1.95         RH   debug above CLNFIT debug           *
C Version 1.32          22.1.95         RH   to UNIX, NCALC=2 in CLNFIT         *
C Version 1.33          19.1.98         RH   prevent VMETRIC looping            *
C Version 1.34          17.9.99         RH   parameter statements for NOBSMX    *
C                                            NPLTMX,NPROMX,NCALMX,NPRMX,NOUTMX  *
C Version 2.00          30.8.00         RH   plot2000 plots postscript directly *
C Version               21.6.00         TSH  fix wrong NOBSMX value in CLNFIT   *
C Version 2.01          03.7.01         TSH  set XPLTSIZ and YPLTSIZ            *
C                                                                               *
C       Remember to change version number in WRITE statement                    *
C                                                                               *
C********************************************************************************
C                                                                       *
C       Input parameters (unit 5):                                      *
C                                                                       *
C       TITLE           80 Character title for this data set            *
C                                                                       *
C       IPG             Plane group number (1-17)                       *
C                                                                       *
C       IPAT            0 for F & Phase     1 for Intensity data        *
C                                                                       *
C       AK,IWF,IWP      AK = relative weights for phases with           *
C                       respect to the F's                              *
C                       IWF,IWP are flags to use individual weights     *
C                       for F's and Phases in input file:               *
C                       -1 : use individual sigmas   (1/sigma**2)       *
C                        0 : set all weights = 1.0                      *
C                        1 : use individual weights                     *
C                 IWP=  -2 : use individual sigmas; (wt=1/sigma**2);    *
C                          : phase and sigma both input in degrees      *
C       ALAT,ZMIN,ZMAX,DELPLT                                           *
C                       ALAT = lattice size in angstroms                *
C                       ZMIN = minimum z* value expected for data set   *
C                       ZMAX = maximum z* value expected for data set   *
C                       DELPLT = z* interval for plotting               *
C                       if DELPLT = 0.0, then no plotting               *
C                                                                       *
C       DELPRO,RMIN,RMAX,RCUT,PFACT                                     *
C                       DELPRO = real-space  or patterson-space         *
C                       sampling interval for profile function          *
C                       (in angstroms)                                  *
C                       RMIN,RMAX lower,upper boundaries for            *
C                       profile function (angstroms)                    *
C                       in general RMIN = -RMAX                         *
C                       note, overall boundary width must be 2x         *
C                       as wide for Intensities compared to F,Phase     *
C                       RCUT = distance from either boundary to         *
C                       point where tapering of profile is to start     *
C                       (in angstroms) see also PFACT                   *
C                       PFACT controls mode of tapering, starting       *
C                       RCUT inside of RMIN/RMAX and going towards      *
C                       RMIN/RMAX.                                      *
C                       PFACT <= 0  then use linear drop off to zero    *
C                       at RMIN/RMAX                                    *
C                       PFACT > 0  then use gaussian roll-off where     *
C                       PFACT = value of gaussian at RMIN/RMAX          *
C                       (a value of .1 is reasonable)                   *
C                                                                       *
C       IGUESS,BINSIZ                                                   *
C                       IGUESS = 0 to refine input set of lattice data  *
C                              = 1 to generate a new set                *
C                       BINSIZ = delta z* for binning observations      *
C                       This is only used to generate the initial       *
C                       guess, which is then refined against the        *
C                       actual obsered data points.                     *
C                       (a value of .005 to .002 is reasonable)         *
C                                                                       *
C       NCYCLS,MPRINT   Number of refinement cycles to perform.         *
C                       If  <0  then output initial guess.              *
C                       If  =0  then minimizer goes till end            *
C                            else (25-50 is quite reasonable)           *
C                            19.1.98 impose a limit of 2000             *
C                       If MPRINT >0, print var/covar matrix            *
C                                 =0  do not                            *
C                                                                       *
C                                                                       *
C                                                                       *
C       Data formats (all are free-format):                             *
C                                                                       *
C       Observed data  [logical unit = OBS] :                           *
C       H, K, Zstar, Fobs, Phiobs, Weight on Fobs, Weight on Phiobs     *
C                                                                       *
C       If no Fobs for this Zstar you must set Fobs=-999.               *
C       If no Phiobs for this Zstar you must set Phiobs=-999.           *
C                                                                       *
C       For Intensity data:                                             *
C       Fobs    actually = Iobs                                         *
C       Phiobs  should be 0                                             *
C                                                                       *
C       It is assumed that the H,K are in the correct asymmetric        *
C       unit for the symmetry operators to be valid.                    *
C       The data can be un-sorted on Zstar.                             *
C                                                                       *
C                                                                       *
C       Initial guess for the lattice data  [logical unit = GUESS] :    *
C       H, K, Zstar, AMP, PHASE                                         *
C                                                                       *
C       H,K must correspond to the H,K that is on the observed          *
C       data file. Data must be sorted in order of increasing Zstar     *
C       and MUST be in equal intervals of Zstar. The actual sampling    *
C       interval is not critical, as long as it is the same or finer    *
C       than the critical sampling interval of 1./(RMAX - RMIN)         *
C       If the plane group has an inversion-center along Zstar          *
C       then only Zstar >= 0 are allowed; the first value must          *
C       correspond to Zstar=0.                                          *
C                                                                       *
C                                                                       *
C       Output data  [logical unit = OBS] :                             *
C                                                                       *
C       H, K, Zstar, Fcalc, Phicalc, SigmaF, SigmaPhi, Figmerit         *
C               the figure of merit = cos(Sigmaphi)                     *
C                                                                       *
C       Plots are generated on   [logical unit = PLOT]                  *
C                                                                       *
C************************************************************************
C                                                                       *
C                          SPACE GROUP INFO                             *
C                                                                       *
C   Number      Spacegroup   Asymmetric Unit    Real        Imaginary   *
C                                                                       *
C       1       P1              H>= 0                                   *
C                                                                       *
C       2       P21             H,Z>=0          Z=0                     *
C                                                                       *
C       3       P12             H,K>=0          K=0                     *
C                                                                       *
C       4       P121            H,K>=0          K=0                     *
C                                                                       *
C       5       C12             H,K>=0          K=0                     *
C                                                                       *
C       6       P222            H,K,Z>=0        H=0;K=0;Z=0             *
C                                                                       *
C       7       P2221           H,K,Z>=0        (0,2N,Z)     (0,2N+1,Z) *
C                                               (H,K,0)                 *
C                                               (H,0,Z)                 *
C                                                                       *
C       8       P22121          H,K,Z>=0        (H,K,0)                 *
C                                               (2N,0,Z)     (2N+1,0,Z) *
C                                               (0,2N,Z)     (0,2N+1,Z) *
C                                                                       *
C       9       C222            H,K,Z>=0        (H,K,0)                 *
C                                               (H,0,Z)                 *
C                                               (0,K,Z)                 *
C                                                                       *
C      10       P4              H,K,Z>=0        (H,K,0)                 *
C                                                                       *
C      11       P422            H,K,Z>=0        (H,K,0)                 *
C                               K>=H            (H,0,Z)                 *
C                                               (0,K,Z)                 *
C                                               (H,H,Z)                 *
C                                                                       *
C      12       P4212           H,K,Z>=0        (H,K,0)                 *
C                               K>=H            (H,H,Z)                 *
C                                               (2N,0,Z)     (2N+1,0,Z) *
C                                               (0,2N,Z)     (0,2N+1,Z) *
C                                                                       *
C      13       P3              H,K>=0                                  *
C                                                                       *
C      14       P312            H,K>=0          (H,H,Z)                 *
C                               K>=H                                    *
C                                                                       *
C      15       P321            H,K>=0          (H,0,Z)                 *
C                               K>H             (0,K,Z)                 *
C                                                                       *
C      16       P6              H,K,Z>=0        (H,K,0)                 *
C                                                                       *
C      17       P622            H,K,Z>=0        (H,K,0)                 *
C                               K>=H            (H,H,Z)                 *
C                                                                       *
C************************************************************************
C                                                                       *
C               NON-STANDARD ROUTINES USED:                             *
C                                                                       *
C       1)      Plotting routines in subroutine GRAPH                   *
C                                                                       *
C       2)      MOVE(B,A,NBYTES)  transfers NBYTES from A to B          *
C                                                                       *
C************************************************************************
C
CHENN>
C       PARAMETER (NOBSMX=1500)
C       PARAMETER (NCALMX=50) 
C       PARAMETER (NPROMX=251)
C
        INTEGER PPFPDIM
        PARAMETER (NOBSMX=3000)
        PARAMETER (NCALMX=100)
        PARAMETER (NPROMX=1001)
        PARAMETER (IPPFPDIM=500)
CHENN<
        PARAMETER (NPLTMX=2000)
        PARAMETER (NPRMX=401)
        PARAMETER (NOUTMX=300)
C remember to change any parameter values in all subroutines too
C
CHENN>
        COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
     .  WTFOB(NOBSMX),WTPHI(NOBSMX),
     .  ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
     .  ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
     .  NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN,IQOBS(NOBSMX)
C
C       COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
C     . WTFOB(NOBSMX),WTPHI(NOBSMX),
C     . ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
C     . ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
C     . NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN
CHENN<
C
        COMMON/STATS/NF,NP,RF,RMSP,WRF,WRMSP,WSUM,WNP
        COMMON/SYM/IPG,ISYMP,ISYML,ISYMZ,ISYMLZ
CHENN>
C       COMMON/FUN/ NCOUNT,NPRNT,THRESH,PFFP(100),PPFP(100)
        COMMON/FUN/ NCOUNT,NPRNT,THRESH,PFFP(IPPFPDIM),PPFP(IPPFPDIM)
CHENN<
        DIMENSION PROF(NPROMX),RP(NPROMX)
        DIMENSION PARMS(100),GRADS(100),SPACE(10500)
        DIMENSION SIGF(NOUTMX),SIGPHI(NOUTMX)
        DIMENSION ZOUT(NOUTMX),FOUT(NOUTMX),PHIOUT(NOUTMX)
        DIMENSION TITLE(20),SCALE(NCALMX*2)
C
        LOGICAL PLOT,INTEN,EOF
        EQUIVALENCE (ZOUT,GRADS), (FOUT,SCALE), (PHIOUT,PARMS)
        DATA TOPI/6.2831853/, PLOT/.FALSE./
        DATA CNV/57.2957795/,  EOF/.FALSE./
C       DATA NOBSMX/500/, NCALMX/50/, NPLTMX/500/, NPROMX/251/
        DATA WSUMSUM/0/, WNPSUM/0/, WRFSUM/0.0/, WRMSPSUM/0.0/
        DATA NFSUM/0/, NPSUM/0/, RFSUM/0.0/, RMSPSUM/0.0/
        DATA EFSUM/0.0/, EPSUM/0.0/, FCOMMONSUM/0.0/
        INTEN = .FALSE.
        NPRNT = 5
        istilt = 0
        ZSTG = 0.0
        ZSTO = 0.0
C
C   READ INPUT DATA
C
      write(6,'(''Latlinek Program'')')
C
1000  FORMAT(20A4)
      READ(5,1000) TITLE
      READ(5,*) IPG
      READ(5,*) IPAT
      READ(5,*) AK,IWF,IWP
      READ(5,*) ALAT,ZMIN,ZMAX,DELPLT
      READ(5,*) DELPRO,RMIN,RMAX,RCUT,PFACT
      READ(5,*) IGUESS,BINSIZ
      READ(5,*) NCYCLS,MPRINT
CHENN>
      READ(5,*) IDOH,IDOK
      READ(5,*) IAQP2,iplterr,imaxIQplot
CHENN<
      IF(NCYCLS.EQ.0) NCYCLS=2000
C
C  HANDLE I/O FILES
C
        IF (DELPLT .GT. 1.E-6) PLOT = .TRUE.
        CALL CCPDPN(1,'OBS','READONLY','F',0,0)
        IF (IGUESS .NE. 1) CALL CCPDPN(2,'GUESS','UNKNOWN','F',0,0)
        CALL CCPDPN(3,'OUT','UNKNOWN','F',0,0)
C
C   ECHO INPUT DATA
C
        IF (IPAT .NE. 0) INTEN = .TRUE.
        WRITE(6,1050) TITLE,IPG,AK,IWF,IWP,ALAT,ZMIN,
     .  ZMAX,DELPLT,DELPRO,RMIN,
     .  RMAX,RCUT,PFACT,IGUESS,BINSIZ,NCYCLS
1050    FORMAT(/,' LATLINE: CURVE FITTING PROGRAM  V2.01(3.7.01)',/,/,
     .  ' Title:  ',20A4,/,
     .  ' Plane group number ....................... ',I5,/,
     .  ' Weight for phases & flags for F,Phi ...... ',F10.4,2I4,/,
     .  ' Lattice size ........... ................. ',F10.2,/, 
     .  ' Min and max zstar ........................ ',2F10.5,/, 
     .  ' Plot & profile intervals ................. ',F10.4,F10.5,/,
     .  ' Min, max, cut, factor for profile ........ ',3F8.1,F10.4,/,
     .  ' Guess & binsize for guess ................ ',I4,F10.5,/,
     .  ' Number of refinement cycles .............. ',I5,/,/)
C
        IF (INTEN) WRITE(6,1100)
1100    FORMAT(/,/,10X,'Intensity data only will be fit',/,/)
C
      AKEEP=AK
C   SET UP PROFILE
C
        DELAT = 1.0/(RMAX - RMIN)
        ALPHA = 0.0
        IF (PFACT.GT.0. .AND. RCUT.GT.0.) ALPHA = -ALOG(PFACT)/RCUT**2
        NPROF = (RMAX - RMIN)/DELPRO + 1.5
        IF (NPROF .GT. NPROMX) GOTO 95
        R = RMIN
        RCUTMN = RMIN + RCUT
        RCUTMX = RMAX - RCUT
        DO 100 J = 1,NPROF
          RP(J) = R
          PROF(J) = 1.0
          RDIF = RP(J) - RCUTMX
          IF (RDIF .LT. 0.0) RDIF = RCUTMN - RP(J)
          IF (RDIF .LE. 0.0) GOTO 7
          IF (PFACT .GT. 0.0) GOTO 5
          PROF(J) = 1.0 - RDIF/RCUT
          GOTO 6
5         PROF(J) = EXP(-ALPHA*RDIF*RDIF)
6         IF (PROF(J) .LT. 0.0) PROF(J) = 0.0
          IF (PROF(J) .GT. 1.0) PROF(J) = 1.0
7         R = R + DELPRO
100     CONTINUE
C
C  SET UP RECIPROCAL SPACE VECTOR FOR PROFILE AND TRANSFORM
C       VECTOR MUST BE TWICE MAX Z RANGE!!
C
        NPR = NPRMX
C       ZMIN = -ZMAX
        ZMINO = ZMIN
        DELPRZ = 2.0*(ZMAX - ZMIN)/(NPR - 1)
        ZPROF(1) = 2.0*ZMIN
        DO 150 J = 2,NPR
          ZPROF(J) = ZPROF(J - 1) + DELPRZ
150     CONTINUE
C
        DO 300 J = 1,NPR
          TOPIS = TOPI*ZPROF(J)
          A= 0.0
          B = 0.0
          DO 200 K = 1,NPROF
            ANG = TOPIS*RP(K)
            A = A + PROF(K)*COS(ANG)
            B = B + PROF(K)*SIN(ANG)
200       CONTINUE
          APROF(J) = A*DELPRO
          BPROF(J) = B*DELPRO
300     CONTINUE
C
C   NOW READ IN A LAYER LINE OF DATA (OBSERVED)
C
8       READ(1,*,END=99) IH,IK,ZSTO,FO,PHIO,WTFO,WTPO
        if (ZSTO.ne.0.0)then
          istilt=1
        endif
        IF (ZSTO.LT.ZMIN.OR.ZSTO.GT.ZMAX) GOTO 8
9       IF (IGUESS .EQ. 0) READ(2,*,END=99) JH,JK,ZSTG,FG,PHIG
        IF (ZSTG.LT.ZMIN.OR.ZSTG.GT.ZMAX) GOTO 9
10      IHIN = IH
        IKIN = IK
        NOBS = 0
        NPTOT=0
        NFTOT=0
        FMAX = 0.0
CHENN>
C       CALL SETSYM(IHIN,IKIN)
C
        if(IAQP2.eq.1)then
          CALL SETSA2(IHIN,IKIN)
        else
          CALL SETSYM(IHIN,IKIN)
        endif
CHENN<
        ZMIN = ZMINO
        IF (ISYMP .NE. 0) ZMIN = 0.0
        GOTO 12
CHENN>
C11     READ(1,*,END=19) IH,IK,ZSTO,FO,PHIO,WTFO,WTPO
C
11      READ(1,*,END=19) IH,IK,ZSTO,FO,PHIO,WTFO,WTPO,IQQ
        if (ZSTO.ne.0.0)then
          istilt=1
        endif
CHENN<
          IF (ZSTO.LT.ZMIN.OR.ZSTO.GT.ZMAX) GOTO 11
          IF (IH .GT. IHIN .OR. IK .GT. IKIN) GOTO 25
          IF (IH .NE. IHIN .OR. IK .NE. IKIN) GOTO 11
12        IF (FO .LT. 0.0 .AND. INTEN) GOTO 11
          NOBS = NOBS + 1
          IF (IPAT .EQ. -1) FO = FO*FO
          IF (NOBS .GT. NOBSMX) GOTO 96
          ZSTAR(NOBS) = ZSTO
          FOBS(NOBS) = FO
CHENN>
          IQOBS(NOBS) = IQQ
CHENN<
          IF (FO .GT. FMAX) FMAX = FO
          IF (PHIO .LE. -900.) GOTO 13
          IF (PHIO .GT.  180.) PHIO = PHIO - 360.
          IF (PHIO .LT. -180.) PHIO = PHIO + 360.
          IF (IWP .EQ.-1 .AND. WTPO .GT. 1.E-4) WTPO = 1./WTPO**2
          IF (IWP .EQ. 0) WTPO = 1.0
      IF(IWP.EQ.-2.AND. WTPO .GT. 1.E-4)THEN
      SIGPHS=WTPO*TOPI/360.0
      WTPO=1.0/SIGPHS**2
      END IF
13        PHIOBS(NOBS) = PHIO
          IF (IWF .LT. 0 .AND. WTFO .GT. 1.E-4) WTFO = 1./WTFO**2
          IF (IWF .EQ. 0) WTFO = 1.0
          WTFOB(NOBS) = WTFO
          WTPHI(NOBS) = WTPO
      IF(FO.GT.-900)NFTOT=NFTOT+1
      IF(PHIO.GT.-900)NPTOT=NPTOT+1
        GOTO 11
C
19      EOF = .TRUE.
C
C----Test if the observed data are tilted
C
       if (istilt.eq.0) then
         write(6,'(''::'',78(''#''))')
         write(6,'(''::Only non-tilted data read.'')')
         write(6,'(''::ERROR: Lattice Lines can only be calculated'',
     .   '' with tilted data.'')')
         write(6,'(''::'',78(''#''))')
         stop 
       else
         write(6,'(''::'',78(''#''))')
         write(6,'(''::Tilted 3D dataset'')')
         write(6,'(''::'',78(''#''))')
       endif
C
C  HERE TO GENERATE GUESS VIA CLINFIT
C
25      IF (NOBS .EQ. 0) GOTO 10
      IF(NFTOT.EQ.0.OR.NPTOT.EQ.0)GO TO 10
      AK=(AKEEP*NFTOT)/NPTOT
        NCALC = 0
        IF (IGUESS .NE. 1) GOTO 31
        CALL CLNFIT(BINSIZ,ZMIN)
        GOTO 50
C
C  HERE TO READ IN CURRENT GUESS
C
30       READ(2,*,END=39) JH,JK,ZSTG,FG,PHIG
          IF (ZSTG.LT.ZMIN.OR.ZSTG.GT.ZMAX) GOTO 30
          IF (JH .GT. IHIN .OR. JK .GT. IKIN) GOTO 40
          IF (JH .NE. IHIN .OR. JK .NE. IKIN) GOTO 30
31        NCALC = NCALC + 1
          IF (NCALC .GT. NCALMX) GOTO 97
          ZCALC(NCALC) = ZSTG
          PHIG = PHIG/CNV
          ACALC(NCALC) = FG*COS(PHIG)
          BCALC(NCALC) = FG*SIN(PHIG)
        GOTO 30
C
39      EOF = .TRUE.
C
C
C   INTERPOLATE GUESS ONTO MINIMAL SAMPLING INTERVAL IF REQUIRED
C
40      DLAT = ZCALC(2) - ZCALC(1)
        IF (ABS(DLAT - DELAT) .LE. 1.E-4) GOTO 50
        WRITE(6,1300)
1300    FORMAT(/,' Guess data must be interpolated onto new lattice',/)
        DELTAZ = DELAT
        DELAT = DLAT
        Z = DELTAZ*NINT(ZCALC(1)/DELTAZ)
        ZEND = DELTAZ*NINT(ZCALC(NCALC)/DELTAZ)
        NLAT = (ZEND - Z)/DELTAZ + 1.5
        DO 400 J = 1,NLAT
          ZOUT(J) = Z
          CALL ABCALC(A,B,Z)
          SIGF(J) = A
          SIGPHI(J) = B
          Z = Z + DELTAZ
400     CONTINUE
        DELAT = DELTAZ
        NCALC = NLAT
        NMOVE = NLAT*4
        CALL MOVE(ZCALC,ZOUT,NMOVE)
        CALL MOVE(ACALC,SIGF,NMOVE)
        CALL MOVE(BCALC,SIGPHI,NMOVE)
C
C   SET UP CORRECT # OF PARAMETERS AND THEN CONVERT TO AMP,PHASE
C
50      THRESH = .005*FMAX
        NPARM = NCALC - ISYMLZ
C
C  force one parameter and one calculated point if there is at least one 
C    observation. (20.9.93 RH)
        IF(NOBS.GE.1.AND.NPARM.EQ.0) NPARM=1
        IF(NOBS.GE.1.AND.NCALC.EQ.0) NCALC=1
C
        IF (ISYML .EQ. 0 .AND. .NOT.INTEN) NPARM = NCALC*2 - ISYMZ
C
          WRITE(6,1500) IHIN,IKIN,ISYMP,ISYML,ISYMZ,ISYMLZ,
     .      NOBS,NCALC,NPARM
1500      FORMAT(/,' LINE = (',2I4,' )',5X,'SYMMETRY INFO: ',4I4,
     .      5X,/,'# OBSERVED REFLNS= ',I4,5X,'# LATTICE PTS = ',I4,
     .      5X,'# PARMS= ',I4)
          WRITE(6,1501)NFTOT,NPTOT,AK
1501      FORMAT(' # FS, # PHASES, ADJUSTED VALUE OF AK',2I5,F10.5)
C
        IF (NPARM .GE. 1) GOTO 52       ! allow even one parameter
C       IF (NPARM .GE. 2) GOTO 52       ! bypass 
          WRITE(6,1600)
1600      FORMAT(/,' ****** Not Enough Parameters on this line *****',/)
        GOTO 90
52      CALL LOAD(PARMS,NPARM)
C
C    CALCULATE NEW GUESS BY CONJUGATE GRADIENTS
C
        NCOUNT = 0
        EST = 1.E-4
        EPS = 1.E-4
        SHIFT = 1.0
        DO 450 J = 1,NCALC
          SCALE(J) = 0.15*AMAX1(ABS(PARMS(J)),THRESH)
          IF (ISYML .EQ. 0) SCALE(J+NCALC) = 0.2
450     CONTINUE
C
        IF (NCYCLS .LT. 0) GOTO 55
        CALL VMETRIC(NPARM,PARMS,F,GRADS,SCALE,EST,NCYCLS,SPACE,KOUNT)
C
C   GET FINAL INFO & CONVERT FINAL PARAMETERS BACK TO A,B
C
        NCOUNT = 0
        AK=AKEEP
55      CALL FUNCT(NPARM,PARMS,F,GRADS)
C
          WRITE(6,2000) NCYCLS,KOUNT,F
2000    FORMAT(' MAX #,COUNT, FINAL RESIDUAL = ',2I5,E15.5)
C         ACCUMULATE THE OVERALL RESIDUAL SUMS
        NFSUM = NFSUM + NF
        NPSUM = NPSUM + NP
        RFSUM = RFSUM + RF*NF
        RMSPSUM = RMSPSUM + RMSP*NP
        WSUMSUM = WSUMSUM + WSUM
        WRFSUM = WRFSUM + WRF
        WRMSPSUM = WRMSPSUM + WRMSP
        WNPSUM = WNPSUM + WNP
C
C   CALCULATE OUTPUT INTERVALS
C
        DELTAZ = 1./ALAT
C       Z = DELTAZ*NINT(ZCALC(1)/DELTAZ)
C       ZEND = DELTAZ*NINT(ZCALC(NCALC)/DELTAZ)
C       NOUT1 = (ZEND - Z)/DELTAZ + 1.5
C       ZOUT1 = Z
      ZLOW=ZCALC(1)
      IF(ZCALC(1).LT.ZSTAR(1))ZLOW=ZSTAR(1)
      ISZ=NINT(ZLOW/DELTAZ)
C      WRITE(6,*)ISZ,ZLOW,ZCALC(1),ZSTAR(1)
      ZHIGH=ZCALC(NCALC)
      IF(ZCALC(NCALC).GT.ZSTAR(NOBS))ZHIGH=ZSTAR(NOBS)
      IFZ=NINT(ZHIGH/DELTAZ)      
      NOUT=IFZ-ISZ+1
      ZOUT(1)=DELTAZ*ISZ
C      WRITE(6,*)NOUT,ZOUT(1),IFZ,ZHIGH,ZCALC(NCALC),ZSTAR(NOBS)
C      WRITE(6,*)NOUT1,ZOUT1,ZEND
        DO 500 J = 2,NOUT
          ZOUT(J) = ZOUT(J - 1) + DELTAZ
500     CONTINUE
C
C  CALCULATE STANDARD DEVIATIONS
C
        CALL MATRIX(MPRINT,NPARM,SPACE,PARMS,F,NOUT,ZOUT,SIGF,SIGPHI)
C
C  CALCULATE DATA AT OUTPUT LATTICE INVERVAL & WRITE OUT
C
        DO 600 J = 1,NOUT
          Z = ZOUT(J)
          CALL ABCALC(A,B,Z)
          F = SQRT(A*A + B*B)
          PHI = 0.0
          IF (F .GT. 1.E-4) PHI = ATAN2(B,A)*CNV
          FOUT(J) = F
          PHIOUT(J) = PHI
          SIGOUT=ABS(SIGPHI(J))
          IF(SIGOUT.LT.1.0) SIGOUT=1.0
CHENN>
          if(SIGOUT.gt.100.0) SIGOUT=100.0
C
          IF(SIGF(J).LT.-99999.0) SIGF(J)=-99999.0
          IF(SIGF(J).GT. 99999.0) SIGF(J)= 99999.0
          FMERIT = ABS(COS(AMIN1(ABS(SIGPHI(J)),90.0)/CNV))
          if(FMERIT.lt.0.00001)FMERIT=0.0
C
CHEN------WRITE(3,3000) IHIN,IKIN,Z,F,PHI,SIGF(J),SIGOUT,FMERIT
          if(NOBS.gt.8)then
            WRITE(3,3000) IHIN,IKIN,Z,F,PHI,SIGF(J),SIGOUT,FMERIT
          else
            WRITE(6,'(''Deleting lattice line data for '',2I6)') IHIN,IKIN
          endif
CHENN<
600     CONTINUE
CHENN>
C3000   FORMAT(2I5,F10.5,4F13.4,F7.3)
C
3000    FORMAT(2I5,F10.5,4G20.8,G20.8)
CHENN<
C
        NPLT = (ZCALC(NCALC) - ZCALC(1))/DELPLT + 1.5
        IF (NPLT .GT. NPLTMX) GOTO 98
CHENN>
C       IF (PLOT) CALL GRAPH(ZMIN,ZMAX,FMAX,IHIN,IKIN,NPLT,DELPLT,
C     . TITLE,SPACE,EOF,NOUT,ZOUT,FOUT,PHIOUT,SIGF,SIGPHI)
C
        if((IDOH.EQ.0 .AND. IDOK.EQ.0) .OR.
     .     (IHIN.EQ.IDOH .AND. IKIN.EQ.IDOK))then
          IF (PLOT) CALL GRAPH(ZMIN,ZMAX,FMAX,IHIN,IKIN,NPLT,DELPLT,
     .    TITLE,SPACE,EOF,NOUT,ZOUT,FOUT,PHIOUT,SIGF,SIGPHI,
     .    iplterr,imaxIQplot)
        endif
CHENN<
C
90      IF (.NOT. EOF) GOTO 10
        IF (PLOT) CALL P2K_PAGE
        GOTO 99
C
C  ERRORS HERE
C
CHENN>
C95     WRITE(6,4000) NPRO,NPROMX
95      WRITE(6,4000) NPROF,NPROMX
CHENN<
        GOTO 99
96      WRITE(6,4100) NOBS,NOBSMX
        GOTO 99
97      WRITE(6,4200) NCALC,NCALMX
        GOTO 99
98      WRITE(6,4300) NPLT,NPLTMX
4000    FORMAT(/,/,' TOO MANY POINTS IN PROFILE!!    FOUND= ',I8,
     .  ' MAX= ',I8)
4100    FORMAT(/,/,' TOO MANY OBSERVED DATA POINTS!! FOUND= ',I8,
     .  ' MAX= ',I8)
4200    FORMAT(/,/,' TOO MANY LATTICE POINTS!!       FOUND= ',I8,
     .  ' MAX= ',I8)
4300    FORMAT(/,/,' TOO MANY POINTS TO PLOT!!       FOUND= ',I8,
     .  ' MAX= ',I8)
C               Output of overall statistics
99      WRITE(6,4400) NFSUM,NPSUM,RFSUM/NFSUM,RMSPSUM/NPSUM,
     .          WRFSUM/WSUMSUM,SQRT(WRMSPSUM/WNPSUM)
CHENN>
        OPEN(UNIT=17,FILE='latline.statistics',STATUS='NEW')
        WRITE(17,4400) NFSUM,NPSUM,RFSUM/NFSUM,RMSPSUM/NPSUM,
     .          WRFSUM/WSUMSUM,SQRT(WRMSPSUM/WNPSUM)
        CLOSE(17)
CHENN<
4400    FORMAT(/,' Overall statistics :',/,
     .  '               Number of amplitudes observed==========',I8,/,
     .  '               Number of phases observed =============',I8,/,
     .  '               Overall R-factor (%)===================',F12.3,/,
     .  '               Overall phase residual (deg)===========',F10.1,/,
     .  '               Overall weighted R-factor (%)==========',F12.3,/,
     .  '               Overall weighted phase residual (deg)==',F10.1,/)
        STOP
        END
C
C*DERIV******************************************************************
C
C       SUBROUTINE TO CALCULATE DERIVATIVE OF FUNCTION AT ANY GIVEN POINT
C
        SUBROUTINE DERIV(A,B,FC,ZST,PARMS,GRADS,WDELF,WDELP)
CHENN>
C       PARAMETER (NOBSMX=1500)
C       PARAMETER (NCALMX=50) 
C       PARAMETER (NPROMX=251)
C
        INTEGER PPFPDIM
        PARAMETER (NOBSMX=3000)
        PARAMETER (NCALMX=100)
        PARAMETER (NPROMX=1001)
        PARAMETER (IPPFPDIM=500)
CHENN<
        PARAMETER (NPLTMX=2000)
        PARAMETER (NPRMX=401)
        PARAMETER (NOUTMX=300)
C
        COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
     .  WTFOB(NOBSMX),WTPHI(NOBSMX),
     .  ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
     .  ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
     .  NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN,IQOBS(NOBSMX)
        COMMON/SYM/IPG,ISYMP,ISYML,ISYMZ,ISYMLZ
CHENN>
C       COMMON/FUN/ NCOUNT,NPRNT,THRESH,PFFP(100),PPFP(100)
        COMMON/FUN/ NCOUNT,NPRNT,THRESH,PFFP(IPPFPDIM),PPFP(IPPFPDIM)
CHENN<
        DIMENSION GRADS(1),PARMS(1)
        LOGICAL INTEN
C
        DFC = DELAT/FC
        IF (ISYMP .NE. 0 .AND. ZST .GT. 1.E-4) DFC = DFC*2.0
        DFC2 = DFC/FC
C
        JST = ISYMLZ + 1
        DO 100 J = JST,NCALC
          K = J - ISYMLZ
          DS = ZST - ZCALC(J)
          CALL CDFIND(C,D,DS)
          ACBD = ACALC(J)*C - BCALC(J)*D
          ADBC = ACALC(J)*D + BCALC(J)*C
          TERM1 = A*ACBD + B*ADBC
          TERM2 = B*ACBD - A*ADBC
          AMP = PARMS(K)
C**       IF (ABS(AMP) .LT. THRESH) AMP = SIGN(THRESH,AMP)
          PFFP(K) = TERM1*DFC/AMP
          PPFP(K) = -TERM2*DFC2/AMP
          GRADS(K) = GRADS(K) + WDELF*PFFP(K) + WDELP*PPFP(K)
          IF (ISYML .NE. 0) GOTO 100
          IF (K .EQ. ISYMZ) GOTO 100
          L = K + NCALC - ISYMZ
          PFFP(L) = TERM2*DFC
          PPFP(L) = TERM1*DFC2
          GRADS(L) = GRADS(L) + WDELF*PFFP(L) + WDELP*PPFP(L)
100     CONTINUE
        IF (ISYMP .EQ. 0) RETURN
C
C  HANDLE EXTRA INVERSION SYMMETRY FOR GRADIENTS HERE
C
        DO 200 J = 2,NCALC
          K = J - ISYMLZ
          DS = ZST + ZCALC(J)
          CALL CDFIND(C,D,DS)
          ACBD = ACALC(J)*C + BCALC(J)*D
          ADBC = ACALC(J)*D - BCALC(J)*C
          TERM1 = A*ACBD + B*ADBC
          TERM2 = A*ADBC - B*ACBD
          AMP = PARMS(K)
C**       IF (ABS(AMP) .LT. THRESH) AMP = SIGN(THRESH,AMP)
          PFFP(K) = PFFP(K) + TERM1*DFC/AMP
          PPFP(K) = PPFP(K) - TERM2*DFC2/AMP
          GRADS(K) = GRADS(K) + WDELF*PFFP(K) + WDELP*PPFP(K)
          IF (ISYML .NE. 0 ) GOTO 200
          L = K + NCALC - ISYMZ
          PFFP(L) = PFFP(L) + TERM2*DFC
          PPFP(L) = PPFP(L) + TERM1*DFC2
          GRADS(L) = GRADS(L) + WDELF*PFFP(L) + WDELP*PPFP(L)
200     CONTINUE
C
        RETURN
        END
C
C*FUNCT*******************************************************************
C
C       SUBROUTINE TO CALCULATE STATS AND DERIVATIVES FOR LATTICE LINE
C       FITTING   CALLED BY FMCG
C
        SUBROUTINE FUNCT(NPARM,PARMS,F,GRADS)
CHENN>
C       PARAMETER (NOBSMX=1500)
C       PARAMETER (NCALMX=50) 
C       PARAMETER (NPRMX=401)
C       PARAMETER (NOUTMX=300)
C
        INTEGER PPFPDIM
        PARAMETER (NOBSMX=3000)
        PARAMETER (NPLTMX=2000)
        PARAMETER (NCALMX=100)
        PARAMETER (NPRMX=401)
        PARAMETER (NOUTMX=300)
        PARAMETER (NPROMX=1001)
        PARAMETER (IPPFPDIM=500)
C
        COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),                   
     .  WTFOB(NOBSMX),WTPHI(NOBSMX),
     .  ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
     .  ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
     .  NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN,IQOBS(NOBSMX)
C
C       COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
C     . WTFOB(NOBSMX),WTPHI(NOBSMX),
C     . ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
C     . ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
C     . NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN
CHENN<
        COMMON/STATS/NF,NP,RF,RMSP,WRF,WRMSP,WSUM,WNP
        COMMON/SYM/IPG,ISYMP,ISYML,ISYMZ,ISYMLZ
CHENN>
C       COMMON/FUN/ NCOUNT,NPRNT,THRESH,PFFP(100),PPFP(100)
        COMMON/FUN/ NCOUNT,NPRNT,THRESH,PFFP(IPPFPDIM),PPFP(IPPFPDIM)
CHENN<
        DIMENSION PARMS(1),GRADS(1)
        LOGICAL INTEN
        DATA CNV/57.2957795/
C
        CALL ZERO(GRADS,NPARM*4)
C
        CALL UNLOAD(PARMS,NPARM)
C
        RF = 0.0
        SUM = 0.0
        RMSP = 0.0
        EF = 0.0
        EP = 0.0
        F = 0.0
        NF = 0
        NP = 0
        WRF = 0.0               ! for weighted R-factors and residuals
        WRMSP = 0.0
        WNP = 0.0
        WSUM = 0.0
C
        DO 100 J = 1,NOBS
          CALL ABCALC(A,B,ZSTAR(J))
          FC2 = A*A + B*B
          IF (FC2 .LT. 1.E-5) GOTO 100
          FC = SQRT(FC2)
          FO = FOBS(J)
          PHIO = PHIOBS(J)
          ERF = 0.0
          ERP = 0.0
          WFOB = 0.0
          WDELF = 0.0
          WDELP = 0.0
          AMULT = 1.0
          IF (ISYMP.NE.0 .AND. ZSTAR(J).GT.1.E-4) AMULT=2.0
C
          IF (FO .LE. -900.0) GOTO 10
          NF = NF + 1
          WFOB = WTFOB(J)
          DELF = FO - FC
          SUM = SUM + FO
          RF = RF + ABS(DELF)
                WSUM = WSUM + WFOB*FO
                WRF = WRF + WFOB*ABS(DELF)
          ERF = AMULT*WFOB*DELF*DELF
          EF = EF + ERF
          F = F + ERF
          WDELF = -2.0*WFOB*DELF
C
10        IF (PHIO .LE. -900.0 .OR. INTEN) GOTO 20
          NP = NP + 1
          DELP = PHIO - DAATAN(B,A)*CNV
          DELP = AMOD(DELP,360.)
          IF (DELP .LT. -180.) DELP = DELP + 360.
          IF (DELP .GT.  180.) DELP = DELP - 360.
          DELPC = DELP/CNV
          RMSP = RMSP + DELP*DELP
          WPHI = WTPHI(J)*AK
                WNP = WNP + WPHI
                WRMSP = WRMSP + WPHI*DELP*DELP
          ERP = AMULT*WPHI*DELPC*DELPC
          EP = EP + ERP
          F = F + ERP
          WDELP = -2.0*WPHI*DELPC
C
20        CALL DERIV(A,B,FC,ZSTAR(J),PARMS,GRADS,WDELF,WDELP)
C
100     CONTINUE
C
        IF(SUM.NE.0.0) RF = RF/SUM
        IF (.NOT. INTEN.AND.NP.NE.0) RMSP = SQRT(RMSP/NP)
        IF (MOD(NCOUNT,NPRNT) .NE. 0) GOTO 90
        WRITE(6,1000) NF,NP,RF,RMSP,EF,EP,F
1000    FORMAT(' #FP,RF,RMS,FPT ',2I4,F9.4,F9.3,4E12.4)
        FCOMMON=F
90      NCOUNT = NCOUNT + 1
C
        RETURN
        END
C*MATRIX*******************************************************************
C
C       SUBROUTINE TO CALCULATE NORMAL MATRIX FROM FINAL SOLN 
C       INVERT TO GET SIGMAS OF PARAMETERS
C       AND THEN TO CALCULATE NEW SIGMAS AT OUTPUT LATTICE POINTS
C
C
        SUBROUTINE MATRIX(MPRINT,NPARM,ARRAY,PARMS,F,NOUT,ZOUT,
     .  SIGF,SIGPHI)
C
CHENN>
C       PARAMETER (NOBSMX=1500)
C       PARAMETER (NCALMX=50)
C       PARAMETER (NPRMX=401)
C       PARAMETER (NOUTMX=300)
C
        INTEGER PPFPDIM
        PARAMETER (NOBSMX=3000)
        PARAMETER (NPLTMX=2000)
        PARAMETER (NCALMX=100)
        PARAMETER (NPRMX=401)
        PARAMETER (NOUTMX=300)
        PARAMETER (NPROMX=1001)
        PARAMETER (IPPFPDIM=500)
C
C       COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
C     . WTFOB(NOBSMX),WTPHI(NOBSMX),
C     . ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
C     . ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
C     . NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN
C
        COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
     .  WTFOB(NOBSMX),WTPHI(NOBSMX),
     .  ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
     .  ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
     .  NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN,IQOBS(NOBSMX)
C
C       COMMON/FUN/ NCOUNT,NPRNT,THRESH,PFFP(100),PPFP(100)
        COMMON/FUN/ NCOUNT,NPRNT,THRESH,PFFP(IPPFPDIM),PPFP(IPPFPDIM)
CHENN<
C
        COMMON/SYM/IPG,ISYMP,ISYML,ISYMZ,ISYMLZ
        DIMENSION ARRAY(NPARM,NPARM)
        DIMENSION SIGF(1),SIGPHI(1),DIAG(100),PARMS(1),ZOUT(1)
        DIMENSION CARRAY(100)
        LOGICAL INTEN
        DATA CNV/57.2957795/
C
        CALL ZERO(ARRAY,NPARM*NPARM*4)
C
        NF = 0
        NP = 0
        DO 200 J = 1,NOBS
          CALL ABCALC(A,B,ZSTAR(J))
          FC2 = A*A + B*B
          IF (FC2 .LT. 1.E-5) GOTO 200
          FC = SQRT(FC2)
          WFOB = 0.0
          WPHI = 0.0
C
          IF (FOBS(J) .LE. -900.0) GOTO 10
          NF = NF + 1
          WFOB = WTFOB(J)
C
10        IF (PHIOBS(J) .LE. -900.0 .OR. INTEN) GOTO 20
          NP = NP + 1
          WPHI = WTPHI(J)*AK
C
20        CALL DERIV(A,B,FC,ZSTAR(J),PARMS,DIAG,0.,0.)
C
C   NOW BUILD NORMAL MATRIX
C
          DO 100 L = 1,NPARM
            DO 100 K = L,NPARM
              ARRAY(K,L) = ARRAY(K,L) + WFOB*PFFP(K)*PFFP(L)
     .        + WPHI*PPFP(K)*PPFP(L)
100       CONTINUE
200     CONTINUE
C
C    INVERT NORMAL MATRIX TO GET STANDARD DEVS
C
        DO 300 L = 1,NPARM
          DO 300 K = L,NPARM
            ARRAY(L,K) = ARRAY(K,L)
300     CONTINUE
C
        CALL MATINV(ARRAY,NPARM,D,IFLAG)
C
        WRITE(6,1000)D,NF,NP,NPARM
1000    FORMAT(/,' Matrix Determinant = ',G14.6,/,' # of Fs, Phases,'
     .  ' Parameters = ',3I6)
      NDAT = NF + NP
      IF ( NDAT.LE.NPARM ) GO TO 91
        ESCALE = F/(NF + NP - NPARM)
        WRITE(6,1001)ESCALE
1001    FORMAT(' Scale factor= ',G14.5,/)
C
        IF (IFLAG.EQ.0) GOTO 90 
        IF (D .LT. 1.E-30) WRITE(6,2002) 
2002    FORMAT(/,' ********** WARNING DETERMINANT NEAR ZERO : ',
     .  ' SIGMAS CALCULATED FOR THIS LINE, BUT SUSPECT !!! *****')
C
        DO 501 K = 1,NPARM
          DO 500 J = K,NPARM
            ARRAY(J,K) = ESCALE*ARRAY(J,K)
            ARRAY(K,J) = ARRAY(J,K)
500     CONTINUE
501     CONTINUE
C
      IF(MPRINT.EQ.0)GO TO 9006
C
      DO 9001 J=1,NPARM
      IF(NPARM.LE.14)THEN
        WRITE(6,9000)(ARRAY(J,K),K=1,NPARM)
      ELSE
        WRITE(6,9000)(ARRAY(J,K),K=1,14)
      END IF      
9001    CONTINUE
C
      IF(NPARM.LE.14)GO TO 9003
      WRITE(6,9004)
9004    FORMAT(//)
      DO 9002 J=1,NPARM
      IF(NPARM.LE.28)THEN
        WRITE(6,9000)(ARRAY(J,K),K=15,NPARM)
      ELSE
        WRITE(6,9000)(ARRAY(J,K),K=15,28)
      END IF      
9002    CONTINUE
C
      IF(NPARM.LE.28)GO TO 9003
      WRITE(6,9004)
      DO 9005 J=1,NPARM
      IF(NPARM.LE.42)THEN
        WRITE(6,9000)(ARRAY(J,K),K=29,NPARM)
      ELSE
        WRITE(6,9000)(ARRAY(J,K),K=29,42)
      END IF      
9005    CONTINUE
C
9003    CONTINUE
9000    FORMAT(/,/,14E9.1)
C
      IF(NPARM.LE.22)THEN
      NLOOP=NPARM
      ELSE
      NLOOP=22
      ENDIF
      DO 9007 J=1,NPARM
      XX=SQRT(ARRAY(J,J))
      DO 9011 K=1,NLOOP
      CARRAY(K)=ARRAY(J,K)/(XX*SQRT(ARRAY(K,K)))
9011    CONTINUE
        WRITE(6,9008)(CARRAY(K),K=1,NLOOP)
9007    CONTINUE
9008    FORMAT(//22F6.2)
C
      IF(NPARM.LE.22)GO TO 9010
      WRITE(6,9004)
      IF(NPARM.LE.44)THEN
      NLOOP=NPARM
      ELSE
      NLOOP=44
      END IF
      DO 9009 J=1,NPARM
      XX=SQRT(ARRAY(J,J))
      DO 9012 K=23,NLOOP
      CARRAY(K)=ARRAY(J,K)/(XX*SQRT(ARRAY(K,K)))
9012    CONTINUE
        WRITE(6,9008)(CARRAY(K),K=23,NLOOP)
9009    CONTINUE
C
9010    CONTINUE
C
9006    CONTINUE
C   CALCULATE NEW SIGMAS AT LATTICE POINTS
C
        DO 800 J = 1,NOUT
          Z = ZOUT(J)
          SIGF(J) = 0.0
          SIGPHI(J) = 0.0
          IF (ISYMLZ .EQ. 1 .AND. J .EQ. 1) GOTO 800
          CALL ABCALC(A,B,Z)
          FC = SQRT(A*A + B*B)
C
          CALL DERIV(A,B,FC,Z,PARMS,DIAG,0.,0.)
      IF(MPRINT.NE.0)WRITE(6,9004)
      IF(MPRINT.NE.0)      WRITE(6,*)J
      IF(MPRINT.NE.0)WRITE (6,9000)(PPFP(K),K=1,NPARM)
      IF(MPRINT.NE.0)WRITE (6,9000)(PFFP(K),K=1,NPARM)
C
          SUMF2 = 0.0
          SUMPHI2 = 0.0
          DO 700 K = 1,NPARM
            SUMF = 0.0
            SUMPHI = 0.0
            DO 600 L = 1,NPARM
              SUMF = SUMF + ARRAY(L,K)*PFFP(L)
              SUMPHI = SUMPHI + ARRAY(L,K)*PPFP(L)
600         CONTINUE
            SUMF2 = SUMF2 + SUMF*PFFP(K)
            SUMPHI2 = SUMPHI2 + SUMPHI*PPFP(K)
700       CONTINUE
          SIGF(J) = SIGN(SQRT(ABS(SUMF2)),SUMF2)
          SIGPHI(J) = SIGN(SQRT(ABS(SUMPHI2)),SUMPHI2)*CNV
800     CONTINUE
C
        RETURN
C
90      WRITE(6,2000) 
2000    FORMAT(/,' ********** WARNING DETERMINANT NEAR ZERO : ',
     .  'NO SIGMAS CALCULATED FOR THIS LINE *********')
        CALL ZERO(SIGF,NOUT*4)
        CALL ZERO(SIGPHI,NOUT*4)
C
        RETURN
91      WRITE(6,2001)
2001    FORMAT(/,' **** NOT ENOUGH DATA POINTS FOR # OF PARAMS :',
     .' NO SIGMAS FOR THIS LINE ****')
        RETURN
        END
C*ABCALC**************************************************************
C
C       SUBROUTINE TO CALCULATE A,B FROM CURRENT SET OF ACALC
C       & BCALC FOR A GENERAL POINT (BY CONVOLUTION)
C
        SUBROUTINE ABCALC(A,B,ZSTR)
CHENN>
C       PARAMETER (NOBSMX=1500)
C       PARAMETER (NCALMX=50) 
C       PARAMETER (NPRMX=401)
C       PARAMETER (NOUTMX=300)
C
        INTEGER PPFPDIM
        PARAMETER (NOBSMX=3000)
        PARAMETER (NPLTMX=2000)
        PARAMETER (NCALMX=100)
        PARAMETER (NPRMX=401)
        PARAMETER (NOUTMX=300)
        PARAMETER (NPROMX=1001)
        PARAMETER (IPPFPDIM=500)
C
C       COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
C     . WTFOB(NOBSMX),WTPHI(NOBSMX),
C     . ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
C     . ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
C     . NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN
C
        COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
     .  WTFOB(NOBSMX),WTPHI(NOBSMX),
     .  ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
     .  ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
     .  NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN,IQOBS(NOBSMX)
C
CHENN<
C
        COMMON/SYM/IPG,ISYMP,ISYML,ISYMZ,ISYMLZ
        LOGICAL INTEN
C
        A = 0.0
        B = 0.0
        DO 100 J = 1,NCALC
          DS = ZSTR - ZCALC(J)
          CALL CDFIND(C,D,DS)
          A = A + ACALC(J)*C - BCALC(J)*D
          B = B + ACALC(J)*D + BCALC(J)*C
100     CONTINUE
        IF (ISYMP .EQ. 0) GOTO 5
        DO 150 J = 2,NCALC
          DS = ZSTR + ZCALC(J)
          CALL CDFIND(C,D,DS)
          A = A + ACALC(J)*C + BCALC(J)*D
          B = B + ACALC(J)*D - BCALC(J)*C
150     CONTINUE
5       A = A*DELAT
        B = B*DELAT
        IF (ABS(ZSTR) .LT. 1.E-4 .AND. ISYMZ .EQ. 1) B = 0.0
        RETURN
C
        END
C*CDFIND************************************************************
C
C       SUBROUTINE TO FIND THE VALUES FOR THE TRANSFORM OF THE
C       PROFILE AT THE GENERAL OFFSET POSITION DS
C       DOES LINEAR INTERPOLATION
C
        SUBROUTINE CDFIND(C,D,DS)
CHENN>
C       PARAMETER (NOBSMX=1500)
C       PARAMETER (NCALMX=50) 
C       PARAMETER (NPRMX=401)
C       PARAMETER (NOUTMX=300)
C
        INTEGER PPFPDIM
        PARAMETER (NOBSMX=3000)
        PARAMETER (NPLTMX=2000)
        PARAMETER (NCALMX=100)
        PARAMETER (NPRMX=401)
        PARAMETER (NOUTMX=300)
        PARAMETER (NPROMX=1001)
        PARAMETER (IPPFPDIM=500)
C
C       COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
C     . WTFOB(NOBSMX),WTPHI(NOBSMX),
C     . ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
C     . ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
C     . NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN
C
        COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
     .  WTFOB(NOBSMX),WTPHI(NOBSMX),
     .  ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
     .  ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
     .  NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN,IQOBS(NOBSMX)
CHENN<
        COMMON/SYM/IPG,ISYMP,ISYML,ISYMZ,ISYMLZ
        LOGICAL INTEN
C
        C = 0.0
        D = 0.0
        IF (DS .LT. ZPROF(1) .OR. DS .GT. ZPROF(NPR)) RETURN
        AIND = (DS - ZPROF(1))/DELPRZ + 1.0
        IND = AIND
        X = AIND - IND
        IND1 = IND + 1
        IF (IND1 .GT. NPR) IND1 = NPR
        C = APROF(IND) + X*(APROF(IND1) - APROF(IND))
        IF (INTEN) RETURN
        D = BPROF(IND) + X*(BPROF(IND1) - BPROF(IND))
C
        RETURN
        END
C*LOAD********************************************************
C
C       SUBROUTINES TO LOAD UP PARAMETERS FROM ACALC & BCALC
C       OR THE REVERSE
C       THESE ROUTINES TAKE ALL SPECIAL SYMMETRY EFFECTS INTO
C       ACCOUNT (IE SHUFFLES DATA TO PACK UNIQUE PARAMETERS)
C
        SUBROUTINE LOAD(PARMS,NPARM)
CHENN>
C       PARAMETER (NOBSMX=1500)
C       PARAMETER (NCALMX=50)
C       PARAMETER (NPRMX=401)
C       PARAMETER (NOUTMX=300)
C
        INTEGER PPFPDIM
        PARAMETER (NOBSMX=3000)
        PARAMETER (NPLTMX=2000)
        PARAMETER (NCALMX=100)
        PARAMETER (NPRMX=401)
        PARAMETER (NOUTMX=300)
        PARAMETER (NPROMX=1001)
        PARAMETER (IPPFPDIM=500)
C
C       COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
C     . WTFOB(NOBSMX),WTPHI(NOBSMX),
C     . ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
C     . ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
C     . NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN
C
        COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
     .  WTFOB(NOBSMX),WTPHI(NOBSMX),
     .  ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
     .  ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
     .  NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN,IQOBS(NOBSMX)
C
C       COMMON/FUN/NCOUNT,NPRNT,THRESH,PFFP(100),PPFP(100)
C
        COMMON/FUN/ NCOUNT,NPRNT,THRESH,PFFP(IPPFPDIM),PPFP(IPPFPDIM)
CHENN<
        COMMON/SYM/IPG,ISYMP,ISYML,ISYMZ,ISYMLZ
        DIMENSION PARMS(1)
        LOGICAL INTEN
C
C  LOAD ACALC & BCALC INTO PARMS
C
        K = 1
        DO 100 J = 1,NCALC
          IF (ISYML) 1,3,2
C
1         ACALC(J) = 0.0
          J1 = J - ISYMZ
          IF (J1 .LT. 1) GOTO 100
          IF (ABS(BCALC(J)) .LT. THRESH)BCALC(J)=SIGN(THRESH,BCALC(J))
          PARMS(J1) = BCALC(J)
          GOTO 100
C
2         IF (ABS(ACALC(J)) .LT. THRESH)ACALC(J)=SIGN(THRESH,ACALC(J))
          PARMS(J) = ACALC(J)
          BCALC(J) = 0.0
          GOTO 100
C
3         IF (ISYMZ .NE. 0 .AND. J .EQ. 1) GOTO 2
          K = J + NCALC - ISYMZ
          PARMS(J) = SQRT(ACALC(J)**2 + BCALC(J)**2)
          PARMS(K) = 0.0
          IF (PARMS(J) .GT. 1.E-4) PARMS(K) = ATAN2(BCALC(J),ACALC(J))
          IF (PARMS(J) .LT. THRESH) PARMS(J) = THRESH
C
100     CONTINUE
        RETURN
C
C===========================================================================
C
C  OR LOAD ACALC & BCALC FROM PARMS
C
        ENTRY UNLOAD(PARMS,NPARM)
C
C  FIRST FIX UP ABS VALUE OF Fs
C
        KOUNT = NCALC - ISYMLZ
        DO 300 J = 1,KOUNT
          IF (ABS(PARMS(J)) .LT. THRESH)PARMS(J)=SIGN(THRESH,PARMS(J))
300     CONTINUE
        K = 1
        DO 400 J = 1,NCALC
          IF (ISYML) 10,14,13
C
10        ACALC(J) = 0.0
          J1 = J - ISYMZ
          IF (J1) 11,11,12
11        BCALC(J) = 0.0
          GOTO 400
12        BCALC(J) = PARMS(J1)
          GOTO 400
C
13        ACALC(J) = PARMS(J)
          BCALC(J) = 0.0
          GOTO 400
C
14        IF (ISYMZ .NE. 0 .AND. J .EQ. 1) GOTO 13
          K = J + NCALC - ISYMZ
          ACALC(J) = PARMS(J)*COS(PARMS(K))
          BCALC(J) = PARMS(J)*SIN(PARMS(K))
C
400     CONTINUE
        RETURN
        END
C*CLNFIT*******************************************************
C
C       SUBROUTINE TO GENERATE INITIAL GUESS BY PLACING
C       OBSERVED DATA POINTS INTO BINS (SPECIFIED BY BINSIZ)
C       LINEARLY INTERPOLATING TO FILL MISSING BINS
C       VALUES ON THE LATTICE POINTS ARE THEN FOUND BY CONVOLUTION
C       WITH THE TRANSFORM OF THE PROFILE FUNCTION
C
        SUBROUTINE CLNFIT(BINSIZ,ZMIN)
CHENN>
C       PARAMETER (NOBSMX=1500)
C       PARAMETER (NCALMX=50) 
C       PARAMETER (NPRMX=401)
C       PARAMETER (NOUTMX=300)
C
        INTEGER PPFPDIM
        PARAMETER (NOBSMX=3000)
        PARAMETER (NPLTMX=2000)
        PARAMETER (NCALMX=100)
        PARAMETER (NPRMX=401)
        PARAMETER (NOUTMX=300)
        PARAMETER (NPROMX=1001)
        PARAMETER (IPPFPDIM=500)
C
C       COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
C     . WTFOB(NOBSMX),WTPHI(NOBSMX),
C     . ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
C     . ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
C     . NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN
C
        COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
     .  WTFOB(NOBSMX),WTPHI(NOBSMX),
     .  ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
     .  ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
     .  NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN,IQOBS(NOBSMX)
CHENN<
        COMMON/SYM/IPG,ISYMP,ISYML,ISYMZ,ISYMLZ
        DIMENSION ABIN(301),BBIN(301),CBIN(301),NBINF(301),NBINP(301)
      DIMENSION SUMWT(301)
        LOGICAL INTEN
        DATA CNV/0.017453292/, TOPI/6.2831853/, NBINMX/301/
C       DATA NCALMX/50/
C
        NCLEAR = NBINMX*4
        CALL ZERO(ABIN,NCLEAR)
        CALL ZERO(BBIN,NCLEAR)
        CALL ZERO(NBINF,NCLEAR)
        CALL ZERO(CBIN,NCLEAR)
        CALL ZERO(SUMWT,NCLEAR)
        CALL ZERO(NBINP,NCLEAR)
C
C  LOAD OBS DATA INTO BINS
C
        INDLO = 1000
        INDHI = 0
        DO 200 J = 1,NOBS
          IND = (ZSTAR(J) - ZMIN)/BINSIZ + 1.5
          IF (IND .GT. NBINMX) GOTO 98
          IF (IND .LT. INDLO) INDLO = IND
          IF (IND .GT. INDHI) INDHI = IND
          IF (FOBS(J) .LE. -900.) GOTO 5
          NBINF(IND) = NBINF(IND) + 1
          ABIN(IND) = ABIN(IND) + FOBS(J)
5         IF (PHIOBS(J) .LE. -900. .OR. INTEN) GOTO 200
          NBINP(IND) = NBINP(IND) + 1
          ANG = PHIOBS(J)*CNV
          BBIN(IND) = BBIN(IND) + WTPHI(J)*COS(ANG)
          CBIN(IND) = CBIN(IND) + WTPHI(J)*SIN(ANG)
          SUMWT(IND)= SUMWT(IND) + WTPHI(J)
200     CONTINUE
C
      DO 900 J=INDLO,INDHI
      IF(NBINP(J).LE.0)GO TO 900
      BBIN(J)=BBIN(J)/SUMWT(J)
      CBIN(J)=CBIN(J)/SUMWT(J)
900     CONTINUE
C
C  NOW LINEARLY INTERPOLATE TO FILL ANY HOLES (FIRST F, THEN PHI)
C
        DO 250 J = INDLO,INDHI
          IF (NBINF(J) .LE. 0 ) GOTO 250
          ABIN(J) = ABIN(J)/NBINF(J)
250     CONTINUE
C
        J = INDLO
10      J = J + 1
          IF (J .GE. INDHI) GOTO 19
          IF (NBINF(J) .GT. 0) GOTO 10
          J1 = J
          JLO = J - 1
15        J = J + 1
          IF (J .GT. INDHI) GOTO 19
          IF (NBINF(J) .LE. 0) GOTO 15
          J2 = J - 1
          GAP = J - JLO
          XA = (ABIN(J) - ABIN(JLO))/GAP
          DO 300 K = J1,J2
            ABIN(K) = ABIN(K - 1) + XA
300       CONTINUE
        GOTO 10
C
C  NOW DO PHASES
C
19      IF (INTEN) GOTO 40
        J = INDLO
20      J = J + 1
          IF (J .GE. INDHI) GOTO 30
          IF (NBINP(J) .GT. 0)GO TO 20
          J1 = J
          JLO = J - 1
25        J = J + 1
          IF (J .GT. INDHI) GOTO 30
          IF (NBINP(J) .LE. 0) GOTO 25
          J2 = J - 1
          GAP = J - JLO
          XB = (BBIN(J) - BBIN(JLO))/GAP
          XC = (CBIN(J) - CBIN(JLO))/GAP
          DO 350 K = J1,J2
            BBIN(K) = BBIN(K - 1) + XB
            CBIN(K) = CBIN(K - 1) + XC
350       CONTINUE
        GOTO 20
C
C  CALC A,B PARTS
C
30      DO 400 J = INDLO,INDHI
          F = ABIN(J)
          IF (ISYML) 31,33,32
31        BBIN(J) = 0.0
          GOTO 33
32        CBIN(J) = 0.0
33        ANG = DAATAN(CBIN(J),BBIN(J))
          ABIN(J) = F*COS(ANG)
          BBIN(J) = F*SIN(ANG)
400     CONTINUE
C
C CALC START,END POINTS FOR LATTICE LINE
C
40      ZOBS = (INDLO - 1)*BINSIZ + ZMIN
        Z2 = (INDHI - 1)*BINSIZ + ZMIN
        ZSTART = NINT(ZOBS/DELAT)*DELAT
        ZEND = NINT(Z2/DELAT)*DELAT
        NCALC = NINT((ZEND - ZSTART)/DELAT) + 1
      IF(ISYML.EQ.-1.AND.NCALC.EQ.1.AND.INDHI.NE.INDLO) NCALC=2
C above line added to force a fit on low index imaginary lattice lines with
C enough data to be able to fit at a single non-zero zstar value of the
C requested sampling lattice (22.1.95-R.Henderson).
        IF (NCALC .GT. NCALMX) GOTO 99
        ZOBS = ZOBS - BINSIZ
C
C  NOW CALCULATE LATTICE VALUES BY CONVOLUTION
C
        ZC = ZSTART - DELAT
        INDLO1 = INDLO + 1
        DO 600 J = 1,NCALC
          ZC = ZC + DELAT
          ZCALC(J) = ZC
          A = 0.0
          B = 0.0
          ZBIN = ZOBS
          DO 500 K = INDLO,INDHI
            ZBIN = ZBIN + BINSIZ
            DS = ZC - ZBIN
            CALL CDFIND(C,D,DS)
            A = A + ABIN(K)*C - BBIN(K)*D
            B = B + ABIN(K)*D + BBIN(K)*C
500       CONTINUE
          IF (ISYMP .EQ. 0) GOTO 50
          ZBIN = ZOBS + BINSIZ
          DO 550 K = INDLO1,INDHI
            ZBIN = ZBIN + BINSIZ
            DS = ZC + ZBIN
            CALL CDFIND(C,D,DS)
            A = A + ABIN(K)*C + BBIN(K)*D
            B = B + ABIN(K)*D - BBIN(K)*C
550       CONTINUE
50        ACALC(J) = A*BINSIZ
          BCALC(J) = B*BINSIZ
C
C   ENSURE PROPER SYMMETRY
C
          IF (ISYML) 60,600,62
60        IF (INTEN) BCALC(J) = ACALC(J)
          ACALC(J) = 0.0
          GOTO 600
62        BCALC(J) = 0.0
600     CONTINUE          
        IF (ISYMLZ .EQ. 1) BCALC(1) = 0.0
C
        RETURN
C
C HERE FOR ERRORS
C
98      WRITE(6,1000) IND,NBINMX,ZSTAR(J)
1000    FORMAT(///,' CLNFIT: TOO MANY BINS REQUIRED!! INDEX= ',I6,
     .  ' MAX= ',I6,' ZSTAR= ',F9.4)
        STOP
99      WRITE(6,1200) NCALC,NCALMX
1200    FORMAT(///,' CLINFIT: TOO MANY LATTICE POINTS!!  CALC= ',I6,
     .  ' MAX ALLOWED= ',I6)
        STOP
        END
C
CHENN>
C*SETSA2*************************************************************
C
C       Subroutine to set up the appropriate symmetry flags
C       For each of the 17 plane groups the 4 flags are
C       defined as follows:
C
C       ISYMP   =  0    no inversion center about z=0
C               =  1    inversion about z=0, only z>=0 in data set
C                       phase(-z) = -phase(z)
C
C       ISYML   =  0    no restrictions for this line
C               =  1    this line is all real
C               = -1    this line is all imaginary
C
C       ISYMZ   =  0    no restrictions for z=0
C               =  1    real at z=0
C                       ( if ISYML = -1, then 0 at z=0 )
C       ISYMLZ  =  0    normal # of parameters
C               =  1    if ISYML = -1 AND ISYMZ = 1
C
C       Note:   For Intensity data ALL lines are either
C               entirely REAL or IMAGINARY (ISYMZ = -1 or 1)
C
C       The lookup table has 7 columns for each plane group
C       that together with H,K can specify the above flags.
C
C       ITABL(1)  =     same as ISYMP, above
C
C       ITABL(2)  =     real line for conditions on H
C                 =  0  no special cases on H
C                 =  1  yes, if H = 0
C                 =  2  yes, if H even & K = 0
C
C       ITABL(3)  =     real line for conditions on K
C                 =  0  no special cases on K
C                 =  1  yes, if K = 0
C                 =  2  yes, if K even & H = 0
C------------->   =  3  yes, if Keven&H=0 or K=0 (1 & 2)
C
C       ITABL(4)  =     real line for conditions on H & K
C                 =  0  no special cases on H & K
C                 =  1  yes, if H = K
C------------->   =  2  yes, if H = K and H even
C
C       ITABL(5)  =     imaginary line for conditions on H
C                 =  0  no special cases on H
C                 =  1  yes, if H odd & K = 0
C------------->   =  2  yes, if H = K and H odd
C
C       ITABL(6)  =     imaginary line for conditions on K
C                 =  0  no special cases on K
C                 =  1  yes, if K odd & H = 0
C
C       ITABL(7)  =     conditions for real at z=0
C                 =  0  no
C                 =  1  yes, for all H,K
C
        SUBROUTINE SETSA2(IH,IK)
C
        INTEGER PPFPDIM
        PARAMETER (NOBSMX=3000)
        PARAMETER (NPLTMX=2000)
        PARAMETER (NCALMX=100)
        PARAMETER (NPRMX=401)
        PARAMETER (NOUTMX=300)
        PARAMETER (NPROMX=1001)
        PARAMETER (IPPFPDIM=500)
C remember to change any parameter values in all subroutines too
C
        COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
     .  WTFOB(NOBSMX),WTPHI(NOBSMX),
     .  ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
     .  ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
     .  NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN,IQOBS(NOBSMX)
C
C       COMMON/FIT/FOBS(500),PHIOBS(500),ZSTAR(500),WTFOB(500),
C     . WTPHI(500),ACALC(100),BCALC(100),ZCALC(100),ZPROF(401),
C     . APROF(401),BPROF(401),NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN
C
        COMMON/SYM/IPG,ISYMP,ISYML,ISYMZ,ISYMLZ
        DIMENSION ITABL(7,17),IA2TABL(7,17)
        LOGICAL INTEN
        DATA ITABL/0,0,0,0,0,0,0,
     2  1,0,0,0,0,0,1,
     3  0,0,1,0,0,0,0,
     4  0,0,1,0,0,0,0,
     5  0,0,1,0,0,0,0,
     6  1,1,1,0,0,0,1,
     7  1,0,3,2,2,1,1,
     8  1,2,2,0,1,1,1,
     9  1,1,1,0,0,0,1,
     O  1,0,0,0,0,0,1,
     1  1,1,1,1,0,0,1,
     2  1,2,2,1,1,1,1,
     3  0,0,0,0,0,0,0,
     4  0,0,0,1,0,0,0,
     5  0,1,1,0,0,0,0,
     6  1,0,0,0,0,0,1,
     7  1,0,0,1,0,0,1/
C
        ISYMP = ITABL(1,IPG)
        ISYMZ = ITABL(7,IPG)
        ISYML = 0
        IF (INTEN) ISYML = 1
        ISYMLZ = 0
C
C  JH,JK = +IABS IF EVEN -IABS IF ODD
C
        JH = IABS(IH)
        JK = IABS(IK)
        IF (MOD(JH,2) .NE. 0) JH = -JH
        IF (MOD(JK,2) .NE. 0) JK = -JK
C
C  FIRST CHECK FOR IMAGINARY LINE
C
C       ITABL(5)  =     imaginary line for conditions on H
C                 =  0  no special cases on H
C                 =  1  yes, if H odd & K = 0
C                 =  2  yes, if H = K and H odd
C
C       ITABL(6)  =     imaginary line for conditions on K
C                 =  0  no special cases on K
C                 =  1  yes, if K odd & H = 0
C
        if(ITABL(5,IPG).eq.2 .and. JH.eq.JK
     1     .and. JH.lt.0) GOTO 5
        IF (JH .GE. 0 .AND. JK .GE. 0) GOTO 10
        J1 = JH*ITABL(5,IPG)
        J2 = JK*ITABL(6,IPG)
        IF (J1 .LT. 0 .AND. JK .EQ. 0
     1     .and. ITABL(5,IPG).ne.2 ) GOTO 5
        IF (J2 .GE. 0 .OR. JH .NE. 0) GOTO 10
5       ISYML = -1
        IF (ISYMZ .EQ. 1) ISYMLZ = 1
        RETURN
C
C  CHECK FOR REAL LINE
C
C       ITABL(2)  =     real line for conditions on H
C                 =  0  no special cases on H
C                 =  1  yes, if H = 0
C                 =  2  yes, if H even & K = 0
C
10      IF (ITABL(2,IPG) - 1) 20,11,15
11      IF (JH .EQ. 0) GOTO 40
        GOTO 20
15      IF (JH.gt.0 .AND. JK .EQ. 0) GOTO 40
C
C       ITABL(3)  =     real line for conditions on K
C                 =  0  no special cases on K
C                 =  1  yes, if K = 0
C                 =  2  yes, if K even & H = 0
C                 =  3  yes, if Keven&H=0 or K=0 (1 & 2)
C
20      IF (ITABL(3,IPG).eq.3) GOTO 27
        IF (ITABL(3,IPG) - 1) 30,21,25
21      IF (JK .EQ. 0) GOTO 40
        GOTO 30
25      IF (JK.gt.0 .AND. JH .EQ. 0) GOTO 40
        GOTO 30
27      IF (JK.gt.0 .AND. JH .EQ. 0) GOTO 40
        IF (JK .EQ. 0) GOTO 40
C
C       ITABL(4)  =     real line for conditions on H & K
C                 =  0  no special cases on H & K
C                 =  1  yes, if H = K
C                 =  2  yes, if H = K and H even
C
30      IF (JH.NE.JK .OR. ITABL(4,IPG).EQ.0) GOTO 45
        IF (ITABL(4,IPG).ne.2) GOTO 45
C
40      ISYML = 1
45      RETURN
        END
C
CHENN<
C*SETSYM*************************************************************
C
C       Subroutine to set up the appropriate symmetry flags
C       For each of the 17 plane groups the 4 flags are 
C       defined as follows:
C
C       ISYMP   =  0    no inversion center about z=0
C               =  1    inversion about z=0, only z>=0 in data set
C                       phase(-z) = -phase(z)
C
C       ISYML   =  0    no restrictions for this line
C               =  1    this line is all real
C               = -1    this line is all imaginary
C
C       ISYMZ   =  0    no restrictions for z=0
C               =  1    real at z=0
C                       ( if ISYML = -1, then 0 at z=0 )
C       ISYMLZ  =  0    normal # of parameters
C               =  1    if ISYML = -1 AND ISYMZ = 1
C
C       Note:   For Intensity data ALL lines are either
C               entirely REAL or IMAGINARY (ISYMZ = -1 or 1)
C
C       The lookup table has 7 columns for each plane group
C       that together with H,K can specify the above flags.
C       
C       ITABL(1)  =     same as ISYMP, above
C       ITABL(2)  =     real line for conditions on H
C                 =  0  no special cases on H 
C                 =  1  yes, if H = 0
C                 =  2  yes, if H even & K = 0
C       ITABL(3)  =     real line for conditions on K
C                 =  0  no special cases on K
C                 =  1  yes, if K = 0
C                 =  2  yes, if K even & H = 0
C       ITABL(4)  =     real line for conditions on H & K
C                 =  0  no special cases on H & K
C                 =  1  yes, if H = K
C       ITABL(5)  =     imaginary line for conditions on H
C                 =  0  no special cases on H 
C                 =  1  yes, if H odd & K = 0
C       ITABL(6)  =     imaginary line for conditions on K
C                 =  0  no special cases on K
C                 =  1  yes, if K odd & H = 0
C       ITABL(7)  =     conditions for real at z=0
C                 =  0  no
C                 =  1  yes, for all H,K
C
        SUBROUTINE SETSYM(IH,IK)
CHENN>
C       PARAMETER (NOBSMX=1500)
C       PARAMETER (NCALMX=50) 
C       PARAMETER (NPRMX=401)
C       PARAMETER (NOUTMX=300)
C
        INTEGER PPFPDIM
        PARAMETER (NOBSMX=3000)
        PARAMETER (NPLTMX=2000)
        PARAMETER (NCALMX=100)
        PARAMETER (NPRMX=401)
        PARAMETER (NOUTMX=300)
        PARAMETER (NPROMX=1001)
        PARAMETER (IPPFPDIM=500)
C
C       COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
C     . WTFOB(NOBSMX),WTPHI(NOBSMX),
C     . ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
C     . ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
C     . NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN
C
        COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
     .  WTFOB(NOBSMX),WTPHI(NOBSMX),
     .  ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
     .  ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
     .  NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN,IQOBS(NOBSMX)
C
CHENN<
        COMMON/SYM/IPG,ISYMP,ISYML,ISYMZ,ISYMLZ
        DIMENSION ITABL(7,17)
        LOGICAL INTEN
        DATA ITABL/0,0,0,0,0,0,0,
     2  1,0,0,0,0,0,1,
     3  0,0,1,0,0,0,0,
     4  0,0,1,0,0,0,0,
     5  0,0,1,0,0,0,0,
     6  1,1,1,0,0,0,1,
     7  1,2,1,0,0,1,1,
     8  1,2,2,0,1,1,1,
     9  1,1,1,0,0,0,1,
     O  1,0,0,0,0,0,1,
     1  1,1,1,1,0,0,1,
     2  1,2,2,1,1,1,1,
     3  0,0,0,0,0,0,0,
     4  0,0,0,1,0,0,0,
     5  0,1,1,0,0,0,0,
     6  1,0,0,0,0,0,1,
     7  1,0,0,1,0,0,1/
C
        ISYMP = ITABL(1,IPG)
        ISYMZ = ITABL(7,IPG)
        ISYML = 0
        IF (INTEN) ISYML = 1
        ISYMLZ = 0
C
C  JH,JK = +IABS IF EVEN -IABS IF ODD
C
        JH = IABS(IH)
        JK = IABS(IK)
        IF (MOD(JH,2) .NE. 0) JH = -JH
        IF (MOD(JK,2) .NE. 0) JK = -JK
C
C  FIRST CHECK FOR IMAGINARY LINE
C
        IF (JH .GE. 0 .AND. JK .GE. 0) GOTO 10
        J1 = JH*ITABL(5,IPG)
        J2 = JK*ITABL(6,IPG)
        IF (J1 .LT. 0 .AND. JK .EQ. 0) GOTO 5
        IF (J2 .GE. 0 .OR. JH .NE. 0) GOTO 10
5       ISYML = -1
        IF (ISYMZ .EQ. 1) ISYMLZ = 1
        RETURN
C
C  CHECK FOR REAL LINE
C
10      IF (ITABL(2,IPG) - 1) 20,11,15
11      IF (JH .EQ. 0) GOTO 40
        GOTO 20
15      IF (JH .GT. 0 .AND. JK .EQ. 0) GOTO 40
C
20      IF (ITABL(3,IPG) - 1) 30,21,25
21      IF (JK .EQ. 0) GOTO 40
        GOTO 30
25      IF (JK .GT. 0 .AND. JH .EQ. 0) GOTO 40
C
30      IF (JH.NE.JK .OR. ITABL(4,IPG).EQ.0) GOTO 45
C
40      ISYML = 1
45      RETURN
        END
C*GRAPH**********************************************************************
C
C  PLOT AMPLITUDES AND PHASES ALONG EACH (H,K) LINE ON PLOTTER.
C  PART OF LATLINE LATTICE-LINE FITTING PACKAGE
C
C  INTEN   - LOGICAL FLAG (TRUE FOR INTENSITY DATA)
C  NOBS    - NO. OF OBSERVATIONS OF AMP AND PHASE.
C  FOBS    - ARRAY OF OBSERVED AMPS.
C  PHIOBS  - ARRAY OF OBSERVED PHASES.
C  ZSTAR   - ARRAY OF OBSERVED ZSTARS.
C  ZMIN    - MINIMUM Z IN PLOT.
C  ZMAX    - MAXIMUM Z IN PLOT.
C  FMAX    - MAXIMUM AMP IN PLOT.
C  IHIN    - H INDEX OF LATTICE LINE.
C  IKIN    - K INDEX OF LATTIEC LINE.
C  ZCALC   - ARRAY OF ZSTAR AT WHICH ACALC, BCALC ARE DETERMINED.
C  NPLT    - NUMBER OF POINTS IN CURVE PLOT.
C  DELPLT  - PLOT INTERVAL.
C  LAST    - .TRUE. IF THIS IS LAST PLOT (ELSE= .FALSE.)
C  NOUT    - # OF LATTICE POINTS
C  ZOUT    - ZSTAR FOR LATTICE POINTS
C  FOUT    - Fs FOR LATTICE
C  PHIOUT  - PHSES FOR LATTICE
C  SIGF    - SIGMA ON F
C  SIGPHI  - SIGMA ON PHASE
C
C  ZSCALE IS 6MM=0.01 A-1,FSCALE HEIGHT=100MM,PSCALE HEIGHT=60MM.
C  FSCALE = 150MM FOR INTENSITY PLOTS
C       ZSCALE IS NOW AUTOMATICALLY DOUBLED UNTIL GRAPH IS AT LEAST 75 MM WIDE!
C
C
CHENN>
C       SUBROUTINE GRAPH(ZMIN,ZMAX,FMAX,IHIN,IKIN,NPLT,DELPLT,TITLE,
C     . PHASE,LAST,NOUT,ZOUT,FOUT,PHIOUT,SIGF,SIGPHI)
C
        SUBROUTINE GRAPH(ZMIN,ZMAX,FMAX,IHIN,IKIN,NPLT,DELPLT,TITLE,
     .  PHASE,LAST,NOUT,ZOUT,FOUT,PHIOUT,SIGF,SIGPHI,iplterr,imaxIQplot)
C
C       PARAMETER (NOBSMX=1500)
C       PARAMETER (NCALMX=50) 
C       PARAMETER (NPRMX=401)
C       PARAMETER (NOUTMX=300)
C
        INTEGER PPFPDIM
        PARAMETER (NOBSMX=3000)
        PARAMETER (NPLTMX=2000)
        PARAMETER (NCALMX=100)
        PARAMETER (NPRMX=401)
        PARAMETER (NOUTMX=300)
        PARAMETER (NPROMX=1001)
        PARAMETER (IPPFPDIM=500)
C
C       COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
C     . WTFOB(NOBSMX),WTPHI(NOBSMX),
C     . ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
C     . ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
C     . NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN
C
        COMMON/FIT/FOBS(NOBSMX),PHIOBS(NOBSMX),ZSTAR(NOBSMX),
     .  WTFOB(NOBSMX),WTPHI(NOBSMX),
     .  ACALC(NCALMX),BCALC(NCALMX),ZCALC(NCALMX),
     .  ZPROF(NPRMX),APROF(NPRMX),BPROF(NPRMX),
     .  NOBS,NCALC,NPR,DELAT,DELPRZ,AK,INTEN,IQOBS(NOBSMX)
CHENN<
        COMMON/SYM/IPG,ISYMP,ISYML,ISYMZ,ISYMLZ
        DATA CNV/57.2957795/, INIT/0/
CTSH    DIMENSION TITLE(1),LINE(20),PHASE(1),SIGF(1),SIGPHI(1)
CTSH++
        SAVE INIT,CNV
        DIMENSION TITLE(1),PHASE(1),SIGF(1),SIGPHI(1)
        CHARACTER*80 LINE
CTSH--
        DIMENSION ZOUT(1),FOUT(1),PHIOUT(1)
        LOGICAL INTEN,LAST
C
      IF(NOBS.LE.8) THEN
        WRITE(6,104) IHIN,IKIN
104     FORMAT('    TOO FEW SPOTS ON LINE FOR PLOT',2I5)
        RETURN
      ENDIF
C
      ZMAG=600.
      FMAG=100.
      IF (INTEN) FMAG = 150.
      PMAG=60.
      GAP=8.
      DELZ = .05
      IF(INIT.EQ.1) THEN
        CALL P2K_PAGE
        GO TO 5
      ENDIF
C  next line is to defeat what looks like an optimiser bug in Linux g77
ccc   IF (0.EQ.1) WRITE(6,*) 'xpltsix,ypltsiz=',xpltsiz,ypltsiz
      CALL P2K_OUTFILE('PLOT.PS',7)
5     CALL P2K_HOME
C  Scale the plotsize. (ZMIN=-0.1, ZMAX=0.1 uses PLTSIZ=136)
      YPLTSIZ=136.0
      XPLTSIZ=YPLTSIZ*ABS(ZMAX-ZMIN)/(0.2)
      FONTSIZE=6.5
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
      CALL P2K_GRID(0.5*XPLTSIZ,0.5*YPLTSIZ,1.0)
      CALL P2K_ORIGIN(-0.5*XPLTSIZ,-0.8*YPLTSIZ,0.)
      CALL P2K_COLOUR(0)
      CALL P2K_LWIDTH(0.3)
      INIT=1
      ZRANG=ZMAX-ZMIN
CHEN>
      ZMM=ZRANG*ZMAG
      ZMAGlocal=ZMAG
6     continue
      ZMMlocal=ZRANG*ZMAGlocal
      IF (ZMMlocal .GT. 75.0) GOTO 7
        ZMAGlocal = ZMAGlocal*2.0
        DELZ = DELZ*0.5
        GOTO 6
CHEN<
7     continue
      ZERO=-ZMIN*ZMAG
C
C  DRAW AXES FOR AMPLITUDE BOX
C
        CALL P2K_MOVE(0.,0.,0.)
        CALL P2K_ORIGIN(5.0,30.0,0.)
C       CALL P2K_MOVE(10.,-15.,0.)
        CALL P2K_MOVE(-5.,-12.,0.)
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*0.8)
        CALL P2K_STRING(TITLE,80,0.)
        CALL P2K_MOVE(0.,0.,0.)
        CALL P2K_DRAW(0.,FMAG,0.)
        CALL P2K_DRAW(ZMM,FMAG,0.)
        CALL P2K_DRAW(ZMM,0.,0.)
        CALL P2K_DRAW(0.,0.,0.)
        CALL P2K_MOVE(ZERO,0.,0.)
        CALL P2K_DRAW(ZERO,FMAG,0.)
CHEN>
        POSN=ZRANG*ZMAG*0.85
        CALL P2K_MOVE(POSN,103.0,0.)
        WRITE(LINE(1:11),103) IHIN,IKIN
103     FORMAT('(',I3,',',I3,')')
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*0.6)
        CALL P2K_STRING(LINE,9,0.)
        IZ=ZRANG/DELZ
C
C-------Plot units on horizontal axis:
C
        DO 25 J=1,100
          ZPOS=-0.5+J*DELZ
          IF((ZPOS.LT.ZMIN).OR.(ZPOS.GE.ZMAX))GO TO 25
          XPOS=ZERO+ZPOS*ZMAG
          CALL P2K_MOVE(XPOS,0.,0.)
          CALL P2K_DRAW(XPOS,2.0,0.)
          XPOS=XPOS-3.0
          CALL P2K_MOVE(XPOS,-4.5,0.)
          WRITE(LINE(1:6),26) ZPOS
          CALL P2K_STRING(LINE,6,0.)
25      CONTINUE
26      FORMAT(F6.3)
        POSN=ZRANG*ZMAG*0.6
        CALL P2K_MOVE(POSN,103.0,0.)
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*0.6)
        CALL P2K_STRING('LATTICE LINE',12,0.)
        POSN=ZRANG*ZMAG-2.5
        CALL P2K_MOVE(POSN,-4.5,0.)
        CALL P2K_STRING('RECIPROCAL',10,0.)
        CALL P2K_MOVE(POSN,-7.5,0.)
        CALL P2K_STRING('ANGSTROMS',9,0.)
        POSX=-5.0
        POSY=FMAG+GAP+PMAG - 3.0
        CALL P2K_MOVE(POSX,POSY,0.)
        CALL P2K_STRING('PHS',3,0.)

        POSY=FMAG - 1.0
        CALL P2K_MOVE(POSX,POSY,0.)
        CALL P2K_STRING('AMP',3,0.)
        CALL P2K_MOVE(0.,0.,0.)
        CALL P2K_ORIGIN(ZERO,0.0,0.)
        SCALE=FMAG/(1.05*FMAX)
        IA=ALOG10(1.05*FMAX)
        B=10.0**IA
        IC=FMAX*1.05/B
C
C-------Plot units on vertical axis:
C
        DO 200 J=1,IC
          F=J*B
          YPOS=F*SCALE-0.2
          ZA=ZMIN*ZMAG
          ZB=ZMAX*ZMAG
          CALL P2K_MOVE(ZA,YPOS,0.)
          ZD=ZA+1.0
          CALL P2K_DRAW(ZD,YPOS,0.)
          ZD=ZB-1.0
          CALL P2K_MOVE(ZB,YPOS,0.)
          CALL P2K_DRAW(ZD,YPOS,0.)
          XPOS=-10.0
          CALL P2K_MOVE(XPOS,YPOS,0.)
          if(F.gt.10000000)then
            WRITE(LINE(1:11),'(G11.3)') F
          else
            WRITE(LINE(1:11),'(F11.1)') F
          endif
          CALL P2K_STRING(LINE,11,0.)
200     CONTINUE
        CALL P2K_MOVE(XPOS,-0.2,0.)
        CALL P2K_STRING('        0.0',11,0.)
201     FORMAT(G11.2)
C
C-------PLOT OBSERVED AMPLITUDES FIRST
C
        CALL P2K_FONT('Helvetica'//CHAR(0),FONTSIZE*0.48)
        DO 50 J=1,NOBS
          IF(FOBS(J).EQ.-999.) GO TO 50
          XP=ZSTAR(J)*ZMAG
          YP=FOBS(J)*SCALE - 0.22*FONTSIZE*0.48
          CALL P2K_MOVE(XP,YP,0.)
          IF(IQOBS(J).le.imaxIQplot)then
            CALL P2K_CSTRING('X',1,0.)
          else
            CALL P2K_CSTRING('+',1,0.)
          endif
CHENN<
50      CONTINUE
C
C  CALCULATE AND DRAW FITTED AMPL CURVE.
C
        Z=ZCALC(1)-DELPLT
        DO 600 J=1,NPLT
          Z=Z+DELPLT
          CALL ABCALC(A,B,Z)
          IF (ISYML .EQ. -1) A = 0.0
          IF (ISYML .EQ.  1) B = 0.0
          F=SQRT(A*A+B*B)
          PHASE(J) = 0.0
          IF (F .GE. 1.E-5 .AND. .NOT. INTEN) PHASE(J)=ATAN2(B,A)*CNV
          XP=Z*ZMAG
          YP=F*SCALE
          IF(J.EQ.1) CALL P2K_MOVE(XP,YP,0.)
          CALL P2K_DRAW(XP,YP,0.)
600     CONTINUE
C
C   PLOT SIGMA F
C
        DO 610 J = 1,NOUT
          XP = ZOUT(J)*ZMAG
          YP = FOUT(J)*SCALE
          YL = AMAX1(0., FOUT(J) - SIGF(J))*SCALE
          YU = AMIN1(FMAX, FOUT(J) + SIGF(J))*SCALE
          CALL P2K_MOVE(XP - 0.8, YL,0.)
          CALL P2K_DRAW(XP + 0.8, YL,0.)
          CALL P2K_MOVE(XP, YL,0.)
          CALL P2K_DRAW(XP, YU,0.)
          CALL P2K_MOVE(XP - 0.8, YU,0.)
          CALL P2K_DRAW(XP + 0.8, YU,0.)
610     CONTINUE
        IF (INTEN) RETURN
C
C  DRAW AXES FOR PHASE BOX.
C
        PMAG2 = PMAG/360.
        YPOS=FMAG+GAP+PMAG/2.0
        CALL P2K_MOVE(0.,0.,0.)
        CALL P2K_ORIGIN(0.0,YPOS,0.)
        ZA=ZMIN*ZMAG
        ZB=ZMAX*ZMAG
        YAXIS=180.0*PMAG2
        CALL P2K_MOVE(ZA,-YAXIS,0.)
        CALL P2K_DRAW(ZA,+YAXIS,0.)
        CALL P2K_DRAW(ZB,+YAXIS,0.)
        CALL P2K_DRAW(ZB,-YAXIS,0.)
        CALL P2K_DRAW(ZA,-YAXIS,0.)
        CALL P2K_MOVE(0.0,-YAXIS,0.)
        CALL P2K_DRAW(0.0,+YAXIS,0.)
        DO 620 J=1,7
          YPOS=(PMAG/8)*J -YAXIS
CHEN>
          CALL P2K_MOVE(ZA,YPOS,0.)
          ZD=ZA+2.0
          CALL P2K_DRAW(ZD,YPOS,0.)
          ZD=ZB-2.0
          CALL P2K_MOVE(ZB,YPOS,0.)
          CALL P2K_DRAW(ZD,YPOS,0.)
620     CONTINUE
C
C-------Write PHASE Y-axis labels
C
        DO 630 J=1,5
          IANG=-180+(J-1)*90
C---------XPOS=ZA-3.5
          XPOS=-4.0
          YPOS=IANG*PMAG2-0.3
          CALL P2K_MOVE(XPOS,YPOS,0.)
          WRITE(LINE(1:4),631) IANG
          CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*0.6)
          CALL P2K_STRING(LINE,4,0.)
CHEN<
630     CONTINUE
631     FORMAT(I4)
C
C  PLOT OBS PHASE POINTS
C
        DO 500 J=1,NOBS
          IF(PHIOBS(J).EQ.-999.) GO TO 500
C BEST PHASES (1,2) ARE PLOTTED AS LARGE X; 
C GOOD ONES (3,4)  AS SMALLER x;
C LESS GOOD ONES (5,6) AS +; GRADE 7,8 AS SMALL +.
C LIMITS SET ARE OKAY FOR WEIGHTS AS SET UP BY READBOTH.FOR and
C input to latline using option 2.
        IF(WTPHI(J) .GT. 16.414)THEN
                CALL P2K_FONT('Helvetica'//CHAR(0),FONTSIZE*0.75)
                XP=ZSTAR(J)*ZMAG
                YP=PHIOBS(J)*PMAG2 - 0.22*FONTSIZE*0.75
                CALL P2K_MOVE(XP,YP,0.)
                CALL P2K_CSTRING('X',1,0.)
        END IF
        IF(WTPHI(J).LE. 16.414.AND.WTPHI(J).GT. 3.2828)THEN
                CALL P2K_FONT('Helvetica'//CHAR(0),FONTSIZE*0.48)
                XP=ZSTAR(J)*ZMAG
                YP=PHIOBS(J)*PMAG2 - 0.22*FONTSIZE*0.48
                CALL P2K_MOVE(XP,YP,0.)
                CALL P2K_CSTRING('X',1,0.)
        END IF
        IF(WTPHI(J).LE. 3.2828.AND.WTPHI(J).GT. 1.3131)THEN
                CALL P2K_FONT('Helvetica'//CHAR(0),FONTSIZE*0.6)
                XP=ZSTAR(J)*ZMAG
                YP=PHIOBS(J)*PMAG2 - 0.22*FONTSIZE*0.6
                CALL P2K_MOVE(XP,YP,0.)
                CALL P2K_CSTRING('+',1,0.)
        END IF
        IF(WTPHI(J).LE. 1.3131)THEN
                CALL P2K_FONT('Helvetica'//CHAR(0),FONTSIZE*0.4)
                XP=ZSTAR(J)*ZMAG
                YP=PHIOBS(J)*PMAG2 - 0.22*FONTSIZE*0.4
                CALL P2K_MOVE(XP,YP,0.)
                CALL P2K_CSTRING('+',1,0.)
        END IF
500     CONTINUE
C
C  DRAW  CALCULATED PHASE CURVE
C
        Z=ZCALC(1)-DELPLT
        DO 660 J=1,NPLT
          Z=Z+DELPLT
          P = PHASE(J)
          XP=Z*ZMAG
          YP=P*PMAG2
          IF(J.EQ.1) CALL P2K_MOVE(XP,YP,0.)
          CALL P2K_DRAW(XP,YP,0.)
660     CONTINUE
C
C  PLOT SIGMA PHASE
C
CHENN>
        if(iplterr.eq.1)then
CHENN<
C
        DO 700 J = 1,NOUT
          XP = ZOUT(J)*ZMAG
          YP = PHIOUT(J)*PMAG2
          YL = AMAX1(-180., PHIOUT(J) - SIGPHI(J))*PMAG2
          YU = AMIN1( 180., PHIOUT(J) + SIGPHI(J))*PMAG2
          CALL P2K_MOVE(XP - 0.8, YL,0.)
          CALL P2K_DRAW(XP + 0.8, YL,0.)
          CALL P2K_MOVE(XP, YL,0.)
          CALL P2K_DRAW(XP, YU,0.)
          CALL P2K_MOVE(XP - 0.8, YU,0.)
          CALL P2K_DRAW(XP + 0.8, YU,0.)
C         CALL MOVETO(XP - 0.8, YP)
C         CALL DRAWTO(XP + 0.8, YP)
700     CONTINUE
CHENN>
        endif
CHENN<
C
        RETURN
        END
C*MATINV.FOR*********************************************************
C
C     VERSION 2.0 WITH SMALL CHANGES RH 15.3.91
C       PURPOSE
C         INVERT A SYMMETRIC MATRIX AND CALCULATE ITS DETERMINANT
c         daa - matrix is pre-scaled and un-scaled at end
C
C       USAGW
C         CALL MATINV (ARRAY,NORDER,DET,IFLAG)
C
C       DESCRIPTION OF PARAMMETERS
C         ARRAY  -  INPUT MATRIX WHICH IS REPLACED BY ITS INVERSE
C         NORDER -  DEGREE OF MATRIX (ORDER OF DETERMINANT)
C         DET    -  DETERMINANT OF INPUT MATRIX
C
C       SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C         NONE
C
C       COMMENTS
C         DIMENSIONED FOR MATRICIES UP TO ORDER 200
C
C
        SUBROUTINE MATINV (SARRAY,NORDER,DET,IFLAG)
        DOUBLE PRECISION ARRAY,AMAX,SAVE
        DIMENSION ARRAY(200,200),IK(200),JK(200),DIAG(200)
        DIMENSION SARRAY(NORDER,NORDER)
C
C  CONVERT TO DOUBLE PRECISION
C
            DO 1 I=1,NORDER
            DO 1 J=1,NORDER
1           ARRAY(I,J)=SARRAY(I,J)
C
C  PRE-SCALE MATRIX BY DIAGS
C
        DET = 0.0
        DO 2 J = 1,NORDER
          DIAG(J) = ARRAY(J,J)
          IF (DIAG(J) .EQ. 0.0) GOTO 300
2       CONTINUE
        DO 3 K = 1,NORDER
          DO 3 J = 1,NORDER
            ARRAY(J,K) = ARRAY(J,K)/SQRT(DIAG(J)*DIAG(K))
3       CONTINUE
C       
        DET = 1.0
        DO 100 K=1,NORDER
C
C       FIND LARGEST ELEMENT ARRAY(I,J) IN REST OF MATRIX
        AMAX = 0.
21      DO 30 I=K,NORDER
        DO 30 J=K,NORDER
        IF (ABS(AMAX)- ABS(ARRAY(I,J))) 24,24,30
24      AMAX = ARRAY(I,J)
        IK(K) = I
        JK(K) = J
30      CONTINUE
C
C       INTERCHANGE ROWS AND COLUMNS-TO PUT AMAX IN ARRAY(K,K)
C
        IF(AMAX) 41,32,41
32      DET = 0.
        GO TO 300
41      I = IK(K)
        IF(I-K) 21,51,43
43      DO 50 J=1,NORDER
        SAVE = ARRAY(K,J)
        ARRAY(K,J) = ARRAY(I,J)
50      ARRAY(I,J) = -SAVE
51      J=JK(K)
        IF(J-K) 21,61,53
53      DO 60 I=1,NORDER
        SAVE = ARRAY(I,K)
        ARRAY(I,K)= ARRAY(I,J)
60      ARRAY(I,J) = -SAVE
C
C       ACCUMULATE ELEMENTS OF INVERSE MATRIX
C
C
61      DO 70 I=1,NORDER
        IF(I-K) 63,70,63
63      ARRAY(I,K) = -ARRAY(I,K)/AMAX
70      CONTINUE
        DO 80 I=1,NORDER
        DO 80 J=1,NORDER
        IF(I-K)74,80,74
74      IF(J-K)75,80,75
C
75      ARRAY(I,J) = ARRAY(I,J) + ARRAY(I,K)*ARRAY(K,J)
80      CONTINUE
        DO 90 J=1,NORDER
        IF(J-K)83,90,83
83      ARRAY(K,J) = ARRAY(K,J)/AMAX
90      CONTINUE
        ARRAY(K,K) = 1./AMAX
100     DET = DET*AMAX
C
C       RESTORE ORDERING OF MATRIX
C
        DO 130 L=1,NORDER
        K= NORDER -L + 1
        J = IK(K)
        IF(J-K) 111,111,105
105     DO 110 I=1,NORDER
        SAVE = ARRAY(I,K)
        ARRAY(I,K) = -ARRAY(I,J)
110     ARRAY(I,J) = SAVE
111     I=JK(K)
        IF(I-K) 130,130,113
113     DO 120 J=1,NORDER
        SAVE = ARRAY(K,J)
        ARRAY(K,J) = -ARRAY(I,J)
120     ARRAY(I,J) = SAVE
130     CONTINUE
C
C  UNSCALE MATRIX
C
        DO 200 K = 1,NORDER
          DO 200 J = 1,NORDER
            ARRAY(J,K) = ARRAY(J,K)/SQRT(DIAG(J)*DIAG(K))
200     CONTINUE
C  RESTORE TO SINGLE PRECISION MATRIX
            DO 250 I=1,NORDER
            DO 250 J=1,NORDER
250         SARRAY(I,J)=ARRAY(I,J)
        IFLAG=1
        RETURN
C
300     IFLAG=0
        RETURN
        END

      SUBROUTINE VMETRIC(N,X,F,G,SCALE,ACC,LIMIT,W,KOUNT)
      COMMON /VA13B/ IPRINT,LP,MAXFUN,MODE,NFUN
      DIMENSION X(1),G(1),SCALE(1),W(1)
      MAXFUN = LIMIT
      ND=1+(N*(N+1))/2
      NW=ND+N
      NXA=NW+N
      NGA=NXA+N
      NXB=NGA+N
      NGB=NXB+N
      CALL VA13C (N,X,F,G,SCALE,ACC,W,W(ND),W(NW),
     1W(NXA),W(NGA),W(NXB),W(NGB))
      KOUNT = NFUN
      RETURN
      END
      BLOCK DATA
      COMMON /VA13B/ IPRINT,LP,MAXFUN,MODE,NFUN
      INTEGER*4 IPRINT/0/,LP/6/,MAXFUN/0/,MODE/1/
      END
      SUBROUTINE VA13C (N,X,F,G,SCALE,ACC,H,D,W,XA,GA,XB,GB)
      COMMON /VA13B/ IPRINT,LP,MAXFUN,MODE,NFUN
      DIMENSION X(1),G(1),SCALE(1),H(1),D(1),W(1),
     1XA(1),GA(1),XB(1),GB(1)
C     BEGIN THE PRINTING FROM THE SUBROUTINE
      IF (IPRINT.EQ.0) GO TO 10
      WRITE (LP,1000)
 1000 FORMAT ('1ENTRY TO VA13A')
C     CALCULATE THE INITIAL FUNCTION VALUE
   10 CALL FUNCT (N,X,F,G)
      NFUN=1
      ITR=0
      NP=N+1
C     SET THE HESSIAN TO A DIAGONAL MATRIX DEPENDING ON SCALE(.)
      IF (MODE.GE.2) GO TO 60
   20 C=0.
      DO 30 I=1,N
   30 C=AMAX1(C,ABS(G(I)*SCALE(I)))
      IF (C.LE.0.) C=1.
      K=(N*NP)/2
      DO 40 I=1,K
   40 H(I)=0.
      K=1
      DO 50 I=1,N
      H(K)=0.01*C/SCALE(I)**2
   50 K=K+NP-I
      GO TO 100
C     FACTORIZE THE GIVEN HESSIAN MATRIX
   60 IF (MODE.GE.3) GO TO 80
      CALL MC11B (H,N,K)
      IF (K.GE.N) GO TO 100
   70 WRITE (LP,1010)
 1010 FORMAT (/5X,'BECAUSE THE HESSIAN GIVEN TO VA13A IS NOT POS DEF,'/
     15X,'IT HAS BEEN REPLACED BY A POSITIVE DIAGONAL MATRIX')
      GO TO 20
C     CHECK THAT THE GIVEN DIAGONAL ELEMENTS ARE POSITIVE
   80 K=1
      DO 90 I=1,N
      IF (H(K).LE.0.) GO TO 70
   90 K=K+NP-I
C     SET SOME VARIABLES FOR THE FIRST ITERATION
  100 DFF=0.
      IPRA=IABS(IPRINT)
      IP=IABS(IPRA-1)
  110 FA=F
      ISFV=1
      DO 120 I=1,N
      XA(I)=X(I)
  120 GA(I)=G(I)
C     BEGIN THE ITERATION BY GIVING THE REQUIRED PRINTING
  130 IP=IP+1
      IF (IP.NE.IPRA) GO TO 140
      IP=0
      WRITE (LP,1020) ITR,NFUN
 1020 FORMAT (/5X,'ITERATION =',I5,5X,'FUNCTIONS =',I5)
      WRITE (LP,1030) FA
 1030 FORMAT (5X,'F =',E15.7)
      IF (IPRINT.LE.0) GO TO 140
      WRITE (LP,1040) (XA(I),I=1,N)
 1040 FORMAT (5X,'X(.) =',(7E15.7))
      WRITE (LP,1050) (GA(I),I=1,N)
 1050 FORMAT (5X,'G(.) =',(7E15.7))
  140 ITR=ITR+1
C     CALCULATE THE SEARCH DIRECTION OF THE ITERATION
      DO 150 I=1,N
  150 D(I)=-GA(I)
      CALL MC11E (H,N,D,W,N)
C     CALCULATE A LOWER BOUND ON THE STEP-LENGTH
C     AND THE INITIAL DIRECTIONAL DERIVATIVE
      C=0.
      DGA=0.
      DO 160 I=1,N
      C=AMAX1(C,ABS(D(I)/SCALE(I)))
  160 DGA=DGA+GA(I)*D(I)
C     TEST IF THE SEARCH DIRECTION IS DOWNHILL
      IF (DGA.GE.0.) GO TO 240
C     SET THE INITIAL STEP-LENGTH OF THE LINE SEARCH
      STMIN=0.
      STEPBD=0.
      STEPLB=ACC/C
      FMIN=FA
      GMIN=DGA
      STEP=1.
      IF (DFF.LE.0.) STEP=AMIN1(STEP,1./C)
      IF (DFF.GT.0.) STEP=AMIN1(STEP,(DFF+DFF)/(-DGA))
  170 C=STMIN+STEP
C     TEST WHETHER FUNC HAS BEEN CALLED MAXFUN TIMES
      IF (NFUN.EQ.MAXFUN) GO TO 250
      NFUN=NFUN+1
C     CALCULATE ANOTHER FUNCTION VALUE AND GRADIENT
      DO 180 I=1,N
  180 XB(I)=XA(I)+C*D(I)
      CALL FUNCT (N,XB,FB,GB)
C     STORE THIS FUNCTION VALUE IF IT IS THE SMALLEST SO FAR
      ISFV=MIN0(2,ISFV)
      IF (FB.GT.F) GO TO 220
      IF (FB.LT.F) GO TO 200
      GL1=0.
      GL2=0.
      DO 190 I=1,N
      GL1=GL1+(SCALE(I)*G(I))**2
  190 GL2=GL2+(SCALE(I)*GB(I))**2
      IF (GL2.GE.GL1) GO TO 220
  200 ISFV=3
      F=FB
      DO 210 I=1,N
      X(I)=XB(I)
  210 G(I)=GB(I)
C     CALCULATE THE DIRECTIONAL DERIVATIVE AT THE NEW POINT
  220 DGB=0.
      DO 230 I=1,N
  230 DGB=DGB+GB(I)*D(I)
C     BRANCH IF WE HAVE FOUND A NEW LOWER BOUND ON THE STEP-LENGTH
      IF (FB-FA.LE.0.1*C*DGA) GO TO 280
C     FINISH THE ITERATION IF THE CURRENT STEP IS STEPLB
      IF (STEP.GT.STEPLB) GO TO 270
  240 IF (ISFV.GE.2) GO TO 110
C     AT THIS STAGE THE WHOLE CALCULATION IS COMPLETE
  250 IF (IPRINT.EQ.0) GO TO 260
      WRITE (LP,1070)
 1070 FORMAT (/5X,'THE RESULTS FROM VA13A ARE AS FOLLOWS')
      WRITE (LP,1020) ITR,NFUN
      WRITE (LP,1030) F
      WRITE (LP,1040) (X(I),I=1,N)
      WRITE (LP,1050) (G(I),I=1,N)
  260 RETURN
C     CALCULATE A NEW STEP-LENGTH BY CUBIC INTERPOLATION
  270 STEPBD=STEP
      C=GMIN+DGB-3.*(FB-FMIN)/STEP
      CC=SQRT(C*C-GMIN*DGB)
      C=(C-GMIN+CC)/(DGB-GMIN+CC+CC)
      STEP=STEP*AMAX1(0.1,C)
      GO TO 170
C     SET THE NEW BOUNDS ON THE STEP-LENGTH
  280 STEPBD=STEPBD-STEP
      STMIN=C
      FMIN=FB
      GMIN=DGB
C     CALCULATE A NEW STEP-LENGTH BY EXTRAPOLATION
      STEP=9.*STMIN
      IF (STEPBD.GT.0.) STEP=0.5*STEPBD
      C=DGA+3.*DGB-4.*(FB-FA)/STMIN
      IF (C.GT.0.) STEP=AMIN1(STEP,STMIN*AMAX1(1.,-DGB/C))
      IF (DGB.LT.0.7*DGA) GO TO 170
C     TEST FOR CONVERGENCE OF THE ITERATIONS
      ISFV=4-ISFV
      IF (STMIN+STEP.LE.STEPLB) GO TO 240
C     REVISE THE SECOND DERIVATIVE MATRIX
      IR=-N
      DO 290 I=1,N
      XA(I)=XB(I)
      XB(I)=GA(I)
      D(I)=GB(I)-GA(I)
  290 GA(I)=GB(I)
      CALL MC11A (H,N,XB,1./DGA,W,IR,1,0.)
      IR=-IR
      CALL MC11A (H,N,D,1./(STMIN*(DGB-DGA)),D,IR,0,0.)
C     BRANCH IF THE RANK OF THE NEW MATRIX IS DEFICIENT
      IF (IR.LT.N) GO TO 250
C     BEGIN ANOTHER ITERATION
      DFF=FA-FB
      FA=FB
      GO TO 130
      END
      SUBROUTINE MC11A(A,N,Z,SIG,W,IR,MK,EPS)
C  STANDARD FORTRAN 66 (A VERIFIED PFORT SUBROUTINE)
      DIMENSION A(1),Z(1),W(1)
C   UPDATE FACTORS GIVEN IN A BY   SIG*Z*ZTRANSPOSE
      IF(N.GT.1)GOTO1
      A(1)=A(1)+SIG *Z(1)**2
      IR=1
      IF(A(1).GT.0.)RETURN
      A(1)=0.
      IR=0
      RETURN
    1 CONTINUE
      NP=N+1
      IF(SIG.GT.0.)GOTO40
      IF(SIG.EQ.0..OR.IR.EQ.0)RETURN
      TI=1./SIG
      IJ=1
      IF(MK.EQ.0)GOTO10
      DO 7 I=1,N
      IF(A(IJ).NE.0.)TI=TI+W(I)**2/A(IJ)
    7 IJ=IJ+NP-I
      GOTO20
   10 CONTINUE
      DO 11 I=1,N
   11 W(I)=Z(I)
      DO 15 I=1,N
      IP=I+1
      V=W(I)
      IF(A(IJ).GT.0.)GOTO12
      W(I)=0.
      IJ=IJ+NP-I
      GOTO15
   12 CONTINUE
      TI=TI+V**2/A(IJ)
      IF(I.EQ.N)GOTO14
      DO 13 J=IP,N
      IJ=IJ+1
   13 W(J)=W(J)-V*A(IJ)
   14 IJ=IJ+1
   15 CONTINUE
   20 CONTINUE
      IF(IR.LE.0 )GOTO21
      IF(TI.GT.0.)GOTO22
      IF(MK-1)40,40,23
   21 TI=0.
      IR=-IR-1
      GOTO23
   22 TI=EPS/SIG
      IF(EPS.EQ.0.)IR=IR-1
   23 CONTINUE
      MM=1
      TIM=TI
      DO 30 I=1,N
      J=NP-I
      IJ=IJ-I
      IF(A(IJ).NE.0.)TIM=TI-W(J)**2/A(IJ)
      W(J)=TI
   30 TI=TIM
      GOTO41
   40 CONTINUE
      MM=0
      TIM=1./SIG
   41 CONTINUE
      IJ=1
      DO 66 I=1,N
      IP=I+1
      V=Z(I)
      IF(A(IJ).GT.0.)GOTO53
      IF(IR.GT.0 .OR.SIG.LT.0..OR.V.EQ.0.)GOTO52
      IR=1-IR
      A(IJ)=V**2/TIM
      IF(I.EQ.N)RETURN
      DO 51 J=IP,N
      IJ=IJ+1
   51 A(IJ)=Z(J)/V
      RETURN
   52 CONTINUE
      TI=TIM
      IJ=IJ+NP-I
      GOTO66
   53 CONTINUE
      AL=V/A(IJ)
      IF(MM)54,54,55
   54 TI=TIM+V*AL
      GOTO56
   55 TI=W(I)
   56 CONTINUE
      R=TI/TIM
      A(IJ)=A(IJ)*R
      IF(R.EQ.0.)GOTO70
      IF(I.EQ.N)GOTO70
      B=AL/TI
      IF(R.GT.4.)GOTO62
      DO 61 J=IP,N
      IJ=IJ+1
      Z(J)=Z(J)-V*A(IJ)
   61 A(IJ)=A(IJ)+B*Z(J)
      GOTO64
   62 GM=TIM/TI
      DO 63 J=IP,N
      IJ=IJ+1
      Y=A(IJ)
      A(IJ)=B*Z(J)+Y*GM
   63 Z(J)=Z(J)-V*Y
   64 CONTINUE
      TIM=TI
      IJ=IJ+1
   66 CONTINUE
   70 CONTINUE
      IF(IR.LT.0)IR=-IR
      RETURN
      END
      SUBROUTINE MC11B(A,N,IR)
C   FACTORIZE A MATRIX GIVEN IN A
      DIMENSION A(1)
      IR=N
      IF(N.GT.1)GOTO100
      IF(A(1).GT.0.)RETURN
      A(1)=0.
      IR=0
      RETURN
  100 CONTINUE
      NP=N+1
      II=1
      DO 104 I=2,N
      AA=A(II)
      NI=II+NP-I
      IF(AA.GT.0.)GOTO101
      A(II)=0.
      IR=IR-1
      II=NI+1
      GOTO104
  101 CONTINUE
      IP=II+1
      II=NI+1
      JK=II
      DO 103 IJ=IP,NI
      V=A(IJ)/AA
      DO 102 IK=IJ,NI
      A(JK)=A(JK)-A(IK)*V
  102 JK=JK+1
  103 A(IJ)=V
  104 CONTINUE
      IF(A(II).GT.0.)RETURN
      A(II)=0.
      IR=IR-1
      RETURN
      END
      SUBROUTINE MC11C(A,N)
C   MULTIPLY OUT THE FACTORS GIVEN IN A
      DIMENSION A(1)
      IF(N.EQ.1)RETURN
      NP=N+1
      II=N*NP/2
      DO 202 NIP=2,N
      JK=II
      NI=II-1
      II=II-NIP
      AA=A(II)
      IP=II+1
      IF(AA.GT.0.)GOTO203
      DO 204 IJ=IP,NI
  204 A(IJ)=0.
      GOTO202
  203 CONTINUE
      DO 201 IJ=IP,NI
      V=A(IJ)*AA
      DO 200 IK=IJ,NI
      A(JK)=A(JK)+A(IK)*V
  200 JK=JK+1
  201 A(IJ)=V
  202 CONTINUE
      RETURN
      END
      SUBROUTINE MC11D(A,N,Z,W)
C   MULTIPLY A VECTOR Z BY THE FACTORS GIVEN IN A
      DIMENSION A(1),Z(1),W(1)
      IF(N.GT.1)GOTO300
      Z(1)=Z(1)*A(1)
      W(1)=Z(1)
      RETURN
  300 CONTINUE
      NP=N+1
      II=1
      N1=N-1
      DO 303 I=1,N1
      Y=Z(I)
      IF(A(II).EQ.0.)GOTO302
      IJ=II
      IP=I+1
      DO 301 J=IP,N
      IJ=IJ+1
  301 Y=Y+Z(J)*A(IJ)
  302 Z(I)=Y*A(II)
      W(I)=Z(I)
  303 II=II+NP-I
      Z(N)=Z(N)*A(II)
      W(N)=Z(N)
      DO 311 K=1,N1
      I=N-K
      II=II-NP+I
      IF(Z(I).EQ.0.)GOTO311
      IP=I+1
      IJ=II
      Y=Z(I)
      DO 310 J=IP,N
      IJ=IJ+1
  310 Z(J)=Z(J)+A(IJ)*Z(I)
  311 CONTINUE
      RETURN
      END
      SUBROUTINE MC11E(A,N,Z,W,IR)
C   MULTIPLY A VECTOR Z BY THE INVERSE OF THE FACTORS GIVEN IN A
      DIMENSION A(1),Z(1),W(1)
      IF(IR.LT.N)RETURN
      W(1)=Z(1)
      IF(N.GT.1)GOTO400
      Z(1)=Z(1)/A(1)
      RETURN
  400 CONTINUE
      DO 402 I=2,N
      IJ=I
      I1=I-1
      V=Z(I)
      DO 401 J=1,I1
      V=V-A(IJ)*Z(J)
  401 IJ=IJ+N-J
      W(I)=V
  402 Z(I)=V
      Z(N)=Z(N)/A(IJ)
      NP=N+1
      DO 411 NIP=2,N
      I=NP-NIP
      II=IJ-NIP
      V=Z(I)/A(II)
      IP=I+1
      IJ=II
      DO 410 J=IP,N
      II=II+1
  410 V=V-A(II)*Z(J)
  411 Z(I)=V
      RETURN
      END
      SUBROUTINE MC11F(A,N,IR)
C   COMPUTE THE INVERSE MATRIX FROM FACTORS GIVEN IN A
      DIMENSION A(1)
      IF(IR.LT.N)RETURN
      A(1)=1./A(1)
      IF(N.EQ.1)RETURN
      NP=N+1
      N1=N-1
      II=2
      DO 511 I=2,N
      A(II)=-A(II)
      IJ=II+1
      IF(I.EQ.N)GOTO502
      DO 501 J=I,N1
      IK=II
      JK=IJ
      V=A(IJ)
      DO 500 K=I,J
      JK=JK+NP-K
      V=V+A(IK)*A(JK)
  500 IK=IK+1
      A(IJ)=-V
  501 IJ=IJ+1
  502 CONTINUE
      A(IJ)=1./A(IJ)
      II=IJ+1
      AA=A(IJ)
      IJ=I
      IP=I+1
      NI=N-I
      DO 511 J=2,I
      V=A(IJ)*AA
      IK=IJ
      K=IJ-IP+J
      I1=IJ-1
      NIP=NI+IJ
      DO 510 JK=K,I1
      A(JK)=A(JK)+V*A(IK)
  510 IK=IK+NIP-JK
      A(IJ)=V
  511 IJ=IJ+NP-J
      RETURN
      END
C*DAATAN*********************************************
C
C       ATAN2 ROUTINE THAT WILL DO 0/0 CORRECTLY
C
        FUNCTION DAATAN(B,A)
C
        DAATAN = 0.0
        IF (A .EQ. 0.0 .AND. B .EQ. 0.0) RETURN
        DAATAN = ATAN2(B,A)
C
        RETURN
        END
C*ZERO*********************************************
C
        SUBROUTINE ZERO(A,N)
C       ====================
C
C ZERO N BYTES OF A
C
        BYTE A(N)
        DO 1 I=1,N
1       A(I)=0
        RETURN
        END
C*MOVE*********************************************
C
        SUBROUTINE MOVE(B,A,N)
C       ====================
C
C MOVE N BYTES FROM A TO B
C
        BYTE A(N),B(N)
        DO 1 I=1,N
1       B(I)=A(I)
        RETURN
        END
