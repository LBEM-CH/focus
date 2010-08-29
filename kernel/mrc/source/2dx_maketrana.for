C   MAKETRAN:  remember also to change version number in first write statement.
C
C   This program reads in an MTZ file of diffraction amplitudes and phases from
C       the experimentally determined and merged 3-D data from a 2-dimensional 
C       crystal, and writes out the transform of the projection of the 
C       structure in the direction, at the magnification, and modified 
C       by the requested CTF, for later use as a reference in cross-correlation
C       with the digitised experimental image.  The code is derived from a
C       synthesis of bits of ORIGTILT, MASKTRAN, CTFAPPLY & EDMTZ.
C
C       Version 1.00    1.9.94          RH      Original version
C       Version 1.01    9.9.94          RH      Temperature factor added
C       Version 1.02    4.12.94         RH      no STANG in p2
C       Version 1.03    6.5.95          RH      slight extrapolation of lattice line
C       Version 1.04    25.7.95         RH      set ASTAR, BSTAR in subroutines
C       Version 1.05    22.4.96         RH      add RH ASTAR, BSTAR changes
C       Version 1.06    4.7.99          RH      IBEGIN(-MAXINDEX:MAXINDEX,etc), etc
C       Version 1.07    29.03.00        JMS     zorigin inserted in ialorg for 
C                                                 compatibility with imsubs2000
C       Version 1.08    29.10.01        RH      change NAME to CHARACTER*80
C       Version 1.09    4.8.02          RH      check array bounds IBEGIN,IFINISH
C
C   Input cards: 
C
C   CARD 1 : NPROG,ISHAPE,IAMPLIM,RAD
C   CARD 2 : NX NY DSTEP XMAG
CHEN>
CC   CARD 3 : AX,AY,BX,BY,  REVHK,SGNXCH,ROT180
C   CARD 3 : AX,AY,BX,BY,  REVHK,SGNXCH,ROT180,ROT90,REVHND
CHEN<
C   CARD 4 : OX OY TX TY   TAXA TANGL A B GAMMA
C   CARD 5 : RESMIN RESMAX     resolution limits to be used in Angstroms
C   CARD 6 : DFMID1 DFMID2 ANGAST CS KV
C   CARD 7 : Output transform file name - mapformat, MODE=4 (complex reals)
C   CARD 8 : Output transform title to be put in image header.
C
C        Parameters describing reference data - MTZ format - SUBROUTINE GETSFS.
C   CARD 9 : ISPGRP,LFPZERO,SFACTOR,BFACTOR
C   CARD 10: LABIN F=AMP PHS=PHASE FOM=FOM ##   - 
C
C         NPROG   - 0 for 2-D crystals
C                 - 1 possible extensions to helical, icosahedral & single part.
C         ISHAPE  -    1 hard edge circular holes
C                      2 soft edge circular (Gaussian weight exp(-2) at edge)
C                      3 hard edge square holes 
C         IAMPLIM -  T, limit spot amplitude to 2x average spot amplitude
C                    F, no amplitude reduction
C         RAD     - radius of circular hole or half-dege length of square hole
C         NX      - Size of densitometered array (e.g. 2048 x 2048)
C         NY      -
C         DSTEP   - Densitometer step-size in microns.
C         XMAG    - Precise magnification normally worked out from lattice
C                    parameters and known cell dimensions.
C         AX,AY   - Lattice parameters (from MMBOX) of (1,0) and (0,1)
C         BX,BY   -                                     in grid units.
C         SGNXCH  - IF NOT EQUAL TO 0, FLIP AROUND A AXIS, USEFUL IN P121
C         ROT180  - IF NOT=0, ROTATE 180 DEG ABOUT Z-AXIS, USEFUL IN P1,P3
CHEN>
C         ROT90  - IF NOT=0, ROTATE 90  DEG ABOUT Z-AXIS, USEFUL IN P2221
C                  THIS IS A COSMETIC FEATURE TO FACILITATE INDEXING
C                  DIFFICULT HIGHLY TILTED FILMS. NOTE THAT ALL OTHER
C                  PARAMETERS, SUCH AS TAXA,TANGL MUST REMAIN CORRECT
C                  W.R.T. THE ORIGINAL DIRECTIONS FOR H AND K.
C         REVHND - IF NOT = 0, sign of Z is inverted.
C                  THIS IS A COSMETIC FEATURE TO FACILITATE debugging the
C                  damned handedness.
C                  NOTE THAT ALL OTHER
C                  PARAMETERS, SUCH AS TAXA,TANGL MUST REMAIN CORRECT
C                  W.R.T. THE ORIGINAL DIRECTIONS FOR H AND K.
CHEN<
C         REVHK   - IF NOT = 0, H AND K ARE INTERCHANGED ON INPUT.
C                         THIS IS A COSMETIC FEATURE TO FACILITATE INDEXING
C                         DIFFICULT HIGHLY TILTED FILMS. NOTE THAT ALL OTHER
C                         PARAMETERS, SUCH AS TAXA,TANGL MUST REMAIN CORRECT
C                         W.R.T. THE ORIGINAL DIRECTIONS FOR H AND K.
C         OX,OY   - origin in degrees for (1,0) & (0,1)
C         TX,TY   - beamtilt in milliradians for (1,0) & (0,1)
C         TAXA    - Angle measured from the tilt axis to the A-axis,
C                   measured in direction of A to B being positive.
C         TANGL   - tilt angle in degrees
C         A       - cell dimensions and space group angle.
C         B       -   "
C         GAMMA   -   "
C         RESMIN,RESMAX  -   resolution limits to be used in Angstroms
C         DFMID1  - Defocus level (underfocus +ve). If DFMID2=DFMID1, image
C         DFMID2  -  is non-astigmatic. Otherwise, amount of defocus in two
C                    orthogonal directions, DFMID1 being defocus in direction
C                    ANGAST (degs) relative to X and Y of the Fourier transform.
C         CS      - Sperical aberration in mm.
C         KV      - Accelerating voltage in kilovolts
C
C*******************************************************************************
C
CHEN>
C       PARAMETER (IARRMXSIZ=37000000)
C       PARAMETER (NMAX=500)
C
        PARAMETER (IARRMXSIZ=410000000)
        PARAMETER (NMAX=20100)
CHEN<
        COMMON//NX,NY,NZ
        DIMENSION ARRAY(IARRMXSIZ)
        DIMENSION TITLE(20),TITLEIN(12),NXYZ(3),NXYZT(3),CELL(6)
        DIMENSION BEAMSHFT(4)
        DIMENSION IHS(NMAX),IKS(NMAX),ZS(NMAX),XC(NMAX),YC(NMAX),
     .          AMP(NMAX),PHASE(NMAX),APART(NMAX),BPART(NMAX),BSH(NMAX)
        REAL KV
        REAL*8 DOUBLMEAN
        CHARACTER*80 NAME
        CHARACTER DAT*24
        LOGICAL IAMPLIM
        EQUIVALENCE (NX,NXYZ)
        EQUIVALENCE (BEAMSHFT(2),ASTAR),(BEAMSHFT(3),BSTAR),
     .            (BEAMSHFT(4),ABANG)
CTSH++
        CHARACTER TMPTITLE*80
        EQUIVALENCE (TMPTITLE,TITLE)
CTSH--
        DATA CELL/100.,100.,100.,90.,90.,90./
CHEN>
        DATA ARRAY/IARRMXSIZ*0.0/
CHEN<
        DATA DOUBLMEAN/0.0/
        TWOPI = 2.0 * 3.14159265
        MAXSIZ = IARRMXSIZ
        DRAD = TWOPI/360.0
C
      WRITE(6,1000)
1000  FORMAT(/,/,' MAKETRAN VX 1.09 (4.8.02): ',
     .  'Create transform of reference image de novo',/,/)
      READ(5,*) NPROG,ISHAPE,IAMPLIM,RAD
      READ(5,*) NX,NY,DSTEP,XMAG
CHEN>
C      READ(5,*) AX,AY,BX,BY,REVHK,SGNXCH,ROT180
C
      READ(5,*) AX,AY,BX,BY,REVHK,SGNXCH,ROT180,ROT90,REVHND
CHEN<
      READ(5,*) OX,OY,TX,TY,TAXA,TANGL,A,B,GAMMA
      ABANG = GAMMA
      IF(ABANG.GT.90.0) ABANG = 180.0 - ABANG   ! ABANG in reciprocal space.
      ASTAR=1.0/(A*SIN(DRAD*ABANG))
      BSTAR=1.0/(B*SIN(DRAD*ABANG))
      TAXB  = TAXA + ABANG
      STAXA = ASTAR*SIN(DRAD*TAXA)
      STAXB = BSTAR*SIN(DRAD*TAXB)
      TTANGL= TAN(TANGL*DRAD)
      READ(5,*) RESMIN,RESMAX
      IF(RESMIN.EQ.0.0) RESMIN=1000.0    ! Angstroms
      IF(RESMAX.EQ.0.0) RESMAX=2.0       ! Angstroms
      IF(RESMAX.GT.RESMIN) THEN ! flip
        TEMP=RESMIN
        RESMIN=RESMAX
        RESMAX=TEMP
      ENDIF
      READ(5,*) DFMID1,DFMID2,ANGAST,CS,KV
      WRITE(6,1001) AX,AY,BX,BY,NX,NY,DSTEP,XMAG,DFMID1,DFMID2,
     .  ANGAST,CS,KV,OX,OY,TX,TY,TAXA,TANGL,RESMIN,RESMAX,
     .  A,B,GAMMA,REVHK,SGNXCH,ROT180,ISHAPE,IAMPLIM,RAD
1001  FORMAT( ' AX,AY,BX,BY ............=',4F9.2,/,
     .  ' NX,NY ..................=',2I8,/,
     .  ' DSTEP ..................=',F8.1,/,
     .  ' XMAG ...................=',F8.0,/,
     .  ' DFMID1,DFMID2,ANGAST ...=',2F8.0,F7.1,/,
     .  ' CS, KV .................=',F8.2,F7.0,/,
     .  ' OX,OY ..................=',2F8.1,/,
     .  ' TX,TY ..................=',2F8.2,/,
     .  ' TAXA,TANGL .............=',2F8.2,/,
     .  ' RESMIN,RESMAX ..........=',F8.2,F7.2,/,
     .  ' A,B,GAMMA ..............=',3F6.1,/,
     .  ' REVHK,SGNXCH,ROT180.....=',3F6.0,/,
     .  ' ISHAPE,IAMPLIM,RAD .....=',I4,3X,L1,F7.1,/)
      KV=KV*1000.0
      WL=12.3/SQRT(KV+KV**2/(10.0**6.0))
      WRITE(6,103)WL
103   FORMAT(' WAVELENGTH(ANGSTROMS)',F10.4)
      STEPR=DSTEP*(10.0**4.0)/XMAG
      THETATR=WL/(STEPR*NX)
C                 THETATR IS DIFFRACTION ANGLE OF (1,0) IN TRANSFORM (RADIANS)
      BEAMSHFT(1)=0.360*CS*10.0**7*WL**2
C
      CALL FDATE(DAT)
      WRITE(6,1002) DAT(5:24)
1002  FORMAT('  Date from fdate ----  ',A20)
C
C   Here for transform parameters plus new title line
C
      READ (5,1150) NAME   ! output file name
1150  FORMAT(A)
      CALL IMOPEN(1,NAME,'NEW')
      NXM1 =NX-1
      NYM1 =NY-1
      NX2 = NX/2
      NY2 = NY/2
      NX21 = NX/2 + 1
      NXP2 = NX + 2
      NY21 = NY/2 + 1
      TMIN =  1.E10
      TMAX = -1.E10
      TMEAN = 0.0
      DOUBLMEAN = 0.0
      NLABL = 1
      MODE = 4                ! defaults to real*4
      DO 6 J=1,NY*NXP2
6     ARRAY(J)=0.0
      READ(5,1500) TITLEIN    ! title for output file
1500  FORMAT(12A4)
CTSH            WRITE(TITLE) TITLEIN,DAT(5:24,1501)
CTSH++
      WRITE(TMPTITLE,1501) TITLEIN,DAT(5:24)
CTSH--
1501  FORMAT(' MAKETRAN : ',12A4,A20)
C
C  NOW GENERATE HEADER
C
      NXYZT(1) = NX21
      NXYZT(2) = NY
      NXYZT(3) = 1
      CELL(1) = NX
      CELL(2) = NY
      CALL ICRHDR(1,NXYZT,NXYZT,MODE,TITLE,NLABL)
      CALL IALCEL(1,CELL)
C     CALL IALORG(1,XORIGIN,YORIGIN,ZORIGIN)
      CALL IWRHDR(1,TITLE,-1,TMIN,TMAX,TMEAN)
C
C  Calculate the spots to be included in the transform to be created.
C
      RSQMAX = (NX*DSTEP*10000/XMAG/RESMAX)**2
      RSQMIN = (NX*DSTEP*10000/XMAG/RESMIN)**2
      IIN = SQRT(RSQMIN)
      IOUT= SQRT(RSQMAX)
      IF(IOUT.GT.NX2) THEN
        IOUT = NX2      ! maximum at edge of transform
        RSQMAX = IOUT**2
        RESLIM = NX*DSTEP*10000/XMAG/IOUT
        WRITE(6,1804) IOUT, RESLIM
1804    FORMAT(' Outer resolution request incompatible with size of',
     .    ' transform, reduced to',I5,/,'  equivalent to',F6.1,' A')
      ENDIF
      WRITE(6,1801) IIN,IOUT
1801  FORMAT(' Spots to be included in transform (between radii)',2I6,/)
      NHOLE = 0
      DO 120 I=-100,100
        DO 120 J=-100,100
C          write(*,'('':: I,J = '',2I6)')I,J
          IH=I
          IK=J
          XPOS=IH*AX+IK*BX
          YPOS=IH*AY+IK*BY
C
          IF(XPOS.LT.0.) GO TO 119                ! Use only positive X,
          IF(XPOS.EQ.0.0.AND.YPOS.LE.0.) GO TO 119  ! positive Y when X=0.
          RSQ=XPOS**2 + YPOS**2
          IF(RSQ.GT.RSQMAX) GO TO 119
          IF(RSQ.LT.RSQMIN) GO TO 119
C
          DPERP=IH*STAXA+IK*STAXB
          Z=DPERP*TTANGL
CHEN>
C         CALL FIDDLE(IH,IK,Z,REVHK,SGNXCH,ROT180)
C
          CALL FIDDLE(IH,IK,Z,REVHK,SGNXCH,ROT180,ROT90,REVHND)
CHEN<
          RADSQ=IH**2*ASTAR**2 + IK**2*BSTAR**2 +
     .      2.0*IH*IK*COS(DRAD*ABANG)*ASTAR*BSTAR+Z**2
          IF(RADSQ.GT.1.0/RESMAX**2.OR.RADSQ.LT.1.0/RESMIN**2)
     .      WRITE(6,1802) IH,IK
1802      FORMAT(' Possible resolution/magnification conflict',2I5)
          NHOLE=NHOLE+1
          IF(NHOLE.GT.NMAX) THEN
            WRITE(6,1803)
1803        FORMAT(' Too many spots for program dimension NMAX')
            stop
          ENDIF
          IHS(NHOLE)=IH
          IKS(NHOLE)=IK
          ZS(NHOLE) =Z
          XC(NHOLE)=XPOS
          YC(NHOLE)=YPOS
          BSH(NHOLE)=BEAMSHFT(1)*RADSQ          ! HERE BSH IS STORED
          WRITE(6,1800) IH,IK,XC(NHOLE),YC(NHOLE)
1800      FORMAT(5X,2I5,3F12.3)
119     CONTINUE                ! used for debugging
120   CONTINUE
      WRITE(6,121) NHOLE
121   FORMAT(/,' Number of holes within resolution ranges',I5,/)
C
C
C       Main guts of the program :- 
C               get the structure factors, put them in transform and apply CTF
C
      CALL GETSFS(NHOLE,AMP,PHASE,IHS,IKS,ZS,A,B,ABANG)
      DO 130 J=1,NHOLE            ! apply beamtilt phaseshift
        P=PHASE(J)
        P=P+PHSHFT(IHS(J),IKS(J),OX,OY,TX,TY,BEAMSHFT,BSH(J))
        PHASE(J)=AMOD(P,360.)     ! here P is stored.
        APART(J)=AMP(J)*COS(DRAD*PHASE(J))
        BPART(J)=AMP(J)*SIN(DRAD*PHASE(J))
C        WRITE(6,131) AMP(J),PHASE(J),APART(J),BPART(J),IHS(J),IKS(J)
131     FORMAT(' After GETSFS, AMP,PHS,a,b',4F10.1,2I8)
130   CONTINUE
      CALL INSERT(ARRAY,NHOLE,ISHAPE,RAD,NX,NY,APART,BPART,XC,YC)
      CALL CTF(ARRAY,NX,NY,THETATR,WL,CS,DFMID1,DFMID2,ANGAST)
C
C  Output of transform
      CALL IWRPAS(1,ARRAY,NXP2,NY,0,NX2,0,NYM1) ! top/bottom done in INSERT
C     CALL IWRPAS(1,ARRAY,NXP2,NY,0,NX2,NY2,NYM1)
C     ISEC = 0
C     CALL IMPOSN(1,ISEC,NY2)
C     CALL IWRPAS(1,ARRAY,NXP2,NY,0,NX2,0,NY2-1)
C
C CLOSE UP OPERATIONS!
C
      TMIN = SQRT(ARRAY(1)**2+ARRAY(2)**2)
      TMAX = TMIN
      DO 100 K=1,NY
        DO 100 J = 1,NX21
          INDEX = (NX21*(K-1)+J)*2-1
          VAL =  SQRT(ARRAY(INDEX)**2+ARRAY(INDEX+1)**2)
          IF (VAL .LT. TMIN) TMIN = VAL
          IF (VAL .GT. TMAX) TMAX = VAL
          DOUBLMEAN = DOUBLMEAN + VAL
100   CONTINUE
      TMEAN = DOUBLMEAN/(NX21*NY)
      CALL IWRHDR(1,TITLE,-1,TMIN,TMAX,TMEAN)
      CALL IMCLOSE(1)
      WRITE (6,2000) TMIN,TMAX,TMEAN
2000  FORMAT(' Transform written out with TMIN,TMAX,TMEAN =',3F11.2,/,/)
      write(6,'('' Program normal end.'')')
      CALL EXIT
      END
C
C************************end of main program************************************
C
      SUBROUTINE GETSFS(NHOLE,AMP,PHASE,IHS,IKS,ZS,A,B,ABANG)
      DIMENSION AMP(1),PHASE(1),IHS(1),IKS(1),ZS(1)
C
C
C  15.3.84 INITIAL PROGRAM WRITTEN USING ASYM SUBROUTINE BUT INCORPORATING
C                   A NUMBER OF P3 SPACE GROUP SPECIFIC ASPECTS, NAMELY :
C       1. THE LATTICE LINE POINTER FOR THE REFERENCE PHASE STORAGE LOCATION 
C            HAS INDICES H+1,K+1 - SO THAT SPACE GROUPS WITH ASYMMETRIC UNITS
C            HAVING H OR K NEGATIVE WILL FAIL - this should have been fixed
C            in the 4.7.99 version, where the pointer array is changed to allow 
C            negative and positive indices and the +1 is dropped.
C       2. THE FORMULA FOR CALCULATION OF PHASE AT AN ARBITRARY ZSTAR POSITION
C            DOES NOT TREAT THE SYMMETRY OF SPACE GROUPS WITH LATTICE LINES
C            FOR WHICH ZSTAR IS ONLY POSITIVE PROPERLY.
C       3. #####  IMPORTANT CHANGE #####################################
C            THE MATRICES IMAT, MAT, IGO HAVE BEEN CHANGED, TOGETHER WITH THE 
C            LREV TEST IN ASYM SO THAT THE CONVENTION IN P4, P3, AND P6 IS
C            FOR THE AXIAL INDICES TO BE H,0 RATHER THAN 0,K.
C
C
C###############################################################################
C
C          DATA CARDS :-
C
C                   1.  ISPGRP,           SPACE GROUP NO.(1 TO 17)
C                        LFPZERO,         diagnostic O/P if no MTZ data
C                        SFACTOR,         scale factor to increase F's on O/P
C                        BFACTOR          temperature factor for F's on O/P
C
C
C###############################################################################
C
C      ANGLE BETWEEN A AND B FOR ALL SPACEGROUPS BUT P1 IS FIXED 
C              (BUT IS READ IN LATER AS CELL(6) ANYWAY)
C
      DIMENSION STANG(17)
      DATA STANG/2*0.0,10*90.0,5*120.0/
C
        PARAMETER (BTEMP=80.0)
        PARAMETER (DRAD=0.0174532)
        PARAMETER (RDEG=57.295779)
        PARAMETER (PI=3.14159265)
CHEN>
C       PARAMETER (MAXPTS=6000)
C       PARAMETER (MAXINDEX=40)
C
        PARAMETER (MAXPTS=80000)
        PARAMETER (MAXINDEX=60)
CHEN<
        PARAMETER (IRNGESQ=(2*MAXINDEX+1)**2)
C
C
C     .. parameters for mtz aspects
        PARAMETER (NLOC=40)
        PARAMETER (MCOLS=200)
        PARAMETER (NPAR=200)
C
      DIMENSION CELL(6),RSYMX(4,4,96)
      LOGICAL EOF
C
C     .. Local Arrays ..
c      REAL ADATAIN(MCOLS),ADATAOUT(MCOLS),DUM(2,MCOLS)
      REAL ADATAIN(MCOLS),DUM(2,MCOLS)
c      INTEGER IH(3),JPOINT(NLOC),LOOKUP(NLOC)
      INTEGER JPOINT(NLOC),LOOKUP(NLOC)
c      CHARACTER OUTTYP(NLOC)*1,LSPRGI(NLOC)*30,LSPRGO(NLOC)*30,
c     + TITNEW*70,HISNEW(20)*80,CTPRGI(NLOC)*1,DUMMY*10
      CHARACTER LSPRGI(NLOC)*30,CTPRGI(NLOC)*1,DUMMY*10
C
C     .. Scalars for Parser ..
c      INTEGER NTOK
c      LOGICAL LEND
c      CHARACTER KEY*4,LINE*400
C     ..
C     .. Arrays for Parser ..
c      REAL FVALUE(NPAR)
c      INTEGER IBEG(NPAR),IDEC(NPAR),IEND(NPAR),ITYP(NPAR)
c      CHARACTER CVALUE(NPAR)*4
C
C
      INTEGER IHI(MAXPTS),IKI(MAXPTS),ILI(MAXPTS)
      REAL FPIN(MAXPTS),PHIN(MAXPTS),FOMIN(MAXPTS)
C
CHEN>
C      INTEGER*2 IBEGIN(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),
C     . IFINISH(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),IP1,IP2
C
      INTEGER*4 IBEGIN(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),
     .  IFINISH(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),IP1,IP2
CHEN<
C
C---- NLPRGI  =  number of input labels
      DATA NLPRGI,LSPRGI/7,'H','K','L','AMP','PHASE',
     +     'FOM',34*' '/
C---- .. This code signs which input columns are essential (LOOKUP)
      DATA CTPRGI/'H','H','H','F','P','W',34*' '/
      DATA LOOKUP/-1,-1,-1,-1,-1,-1,34*0/
      DATA JPOINT/40*0/
C
C
      DATA IBEGIN /IRNGESQ*-999/, IFINISH /IRNGESQ*-999/
      DATA NBEGIN /-999/,NFINISH/-999/
      LOGICAL LFPZERO
        ASTAR=1.0/(A*SIN(DRAD*ABANG))
        BSTAR=1.0/(B*SIN(DRAD*ABANG))
C
C     READ SPACE GROUP NUMBER, AND FLAG FOR OUTPUT OF ALL FP'S
C
      READ(5,*) ISPGRP,LFPZERO,SFACTOR,BFACTOR
      IF(SFACTOR.EQ.0.0) SFACTOR=1.0
      WRITE(6,9) ISPGRP,LFPZERO,SFACTOR,BFACTOR
9     FORMAT(' Space group number..........',I3,/,
     $       ' Write Fp if no phase........',L3,/,
     $       ' Scale factor for Fs.........',F8.2,/,
     $       ' Temperature factor for Fs...',F8.2)
C
C  INPUT OF PHASES OF NATIVE DATA 
C       ALL PHASES ARE STORED AS VALUES BETWEEN -180.0 AND 180.0 DEGREES
C
      CALL CCPFYP
      CALL MTZINI
      CALL LROPEN(1,'HKLIN',3,IERR)
      CALL LRCELL(1,CELL)
      CALL LRSYMM(1,NSYMX,RSYMX)
      IF(IERR.NE.0) THEN
        WRITE (6,8)IERR
8       FORMAT(':: ERROR ON INPUT OF MTZ FILE, IERR=',I5)
        GOTO 980
        STOP
      ENDIF
C---- Find out how many columns and reflections in input file
      CALL LRINFO(1,DUMMY,NCOL,NREF,DUM)
      CALL LKYASN(1,NLPRGI,LSPRGI,CTPRGI,LOOKUP)
C
CHEN>
C-----IF(A.NE.CELL(1).OR.B.NE.CELL(2)) THEN
      IF(ABS(A-CELL(1)).gt.0.2 .OR. ABS(B-CELL(2)).gt.0.2) THEN
CHEN<
        WRITE(6,1110) A,B,CELL(1),CELL(2)
1110    FORMAT(':: Conflict between A,B values input and MTZ values',
     .    2F8.2,/,'::',49X,2F8.2)
        GOTO 980
        STOP
      ENDIF
CHEN>
C-----IF(ISPGRP.GE.3.AND.STANG(ISPGRP).NE.CELL(6)) THEN
      IF(ISPGRP.GE.3.AND.ABS(STANG(ISPGRP)-CELL(6)).gt.0.2) THEN
CHEN<
        WRITE(6,1109) STANG(ISPGRP),CELL(6)
1109    FORMAT(':: Conflict between cell angles from space group',
     $  ' and mtz input file, STANG, CELL=',2F8.3)
        GOTO 980
        STOP
      ENDIF
C
CHEN>
C      IF(ABANG.NE.180.0 - CELL(6)) 
C     .         STOP' Conflict between GAMMA and MTZ cell angle'
C
      IF(ABANG.NE.180.0 - CELL(6)) THEN
        write(*,'('':: CELL(6) ='',F9.2,'' ABANG='',F9.2)')CELL(6),ABANG
        write(*,'('':: Conflict between GAMMA and MTZ cell angle'')')
        GOTO 980
        STOP
      endif
CHEN<
      N=0
      NREC=0
C
1     CALL LRREFF(1,RESOL,ADATAIN,EOF)
      IF(EOF) GO TO 10
      N=N+1
C
      IF(N.GT.MAXPTS) THEN
        WRITE(6,1106) MAXPTS
1106    FORMAT(':: Reference phase list too big for program dimension.
     $            MAXPTS= ',I6)
        GOTO 980
        STOP
      ENDIF
C
      NREC=NREC+1
      IHI(N)=ADATAIN(1)
      IKI(N)=ADATAIN(2)
      IF(IABS(IHI(N)).GT.MAXINDEX.OR.IABS(IKI(N)).GT.MAXINDEX) THEN
        WRITE(6,1101) MAXINDEX
1101    FORMAT(':: Array dimensions for IBEGIN, ',
     $ 'IFINISH too small for spot',
     $ 'indices       MAXINDEX= ',I6)
        GOTO 980
        STOP
      ENDIF
      ILI(N)=ADATAIN(3)
      FPIN(N)=ADATAIN(4)
      PHIN(N)=ADATAIN(5)
      FOMIN(N)=ADATAIN(6)
C
CHEN>
C        write(*,'(3I5,3F15.3)') IHI(N),IKI(N),ILI(N),FPIN(N),
C     .    PHIN(N),FOMIN(N)
CHEN<
C
      IF(N.EQ.1) THEN
        IHOLD=IHI(N)
        IKOLD=IKI(N)
        IBEGIN(IHOLD,IKOLD)=1
      ENDIF
      IF(.NOT.((IHI(N).EQ.IHOLD).AND.(IKI(N).EQ.IKOLD))) THEN
        IFINISH(IHOLD,IKOLD)=N-1
        IHOLD=IHI(N)
        IKOLD=IKI(N)
C
CHEN>
        IF((IHOLD.GT.MAXINDEX).OR.(IKOLD.GT.MAXINDEX))THEN
          WRITE(6,1209) MAXINDEX
1209      FORMAT(':: Maximum index too high for program dimension.
     .              MAXINDEX= ',I6)
          GOTO 980
          STOP
        ENDIF
CHEN<
C
        IBEGIN(IHOLD,IKOLD)=N
      ENDIF
      GO TO 1
C
10    WRITE(6,101)
101   FORMAT(' end of mtz input')
      IFINISH(IHOLD,IKOLD)=N
C
      WRITE(6,1100)N,NREC
1100  FORMAT(I10,' Phases input on stream 1',/,
     .  I10,' total records on stream 1.')
      CALL LRCLOS(1)
C
      NOTFOUND = 0
      NOUTPUT  = 0
C
        WRITE(6,1201)
1201    FORMAT(/,' IHIN IKIN  ZIN   NH  NK   ZSTAR    FREF ',
     .  '   PHS  PHIN-  PHIN+   FOM IHI IKI ILI')
      DO 1200 J=1,NHOLE
        NH=IHS(J)
        NK=IKS(J)
        ZSTAR=ZS(J)
        IP1=1
        IP2=0
C
C  NOW CALL ASYM TO GET CORRECT INDICES IN STANDARD ASYMMETRIC UNIT.
C  THE NH,NK,ZSTAR RETURNED BY ASYM ARE IN THE STANDARD ASYMMETRIC UNIT.
C      PHASES FROM THE REFERENCE CURVES ASYMMETRIC UNIT ARE TRANSFORMED
C          TO BE COMPARABLE BY TRANSFORMING THE REFERENCE DATASET PHASES
C          WITH IP1 AND IP2 TO CORRESPOND TO REFLECTIONS WITH THE SAME
C          INDICES AS THE INPUT REFLECTIONS. THE PHASES ARE THEN AVAILABLE
C          FOR A P1 SYNTHESIS.
C
        CALL ALASYM(NH,NK,ZSTAR,IP1,IP2,LSPEC,IPTEST,WSTAR,ISPGRP)
C
        NBEGIN=IBEGIN(NH,NK)
        NFINISH=IFINISH(NH,NK)
C
CHEN>
C       WRITE(6,7707) NBEGIN,NFINISH
7707   FORMAT(2I9)
CHEN<
C       
      IF((NBEGIN.EQ.-999).OR.(NFINISH.EQ.-999)) THEN
          NOTFOUND=NOTFOUND+1
        IF(LFPZERO) THEN
          WRITE(6,1107) IHS(J),IKS(J),NH,NK
1107      FORMAT(2I4,' has no structure factor in MTZ file.',
     .       '           Reflection ',2I4)
        ENDIF
        AMP(J)=0.0
        PHASE(J)=0.0
C
      ELSE              !CALCPHS
C
        ZBEGIN=ILI(NBEGIN)/CELL(3)
        ZFINISH=ILI(NFINISH)/CELL(3)
        ZALLOW = 1/(2.0*CELL(3))  ! allow limited extrapolation beyond data.
        IF(.NOT.((ZSTAR.GE.ZBEGIN-ZALLOW).AND.
     .                  (ZSTAR.LE.ZFINISH+ZALLOW))) THEN
          NOTFOUND=NOTFOUND+1
          WRITE(6,1108) NH,NK,IHS(J),IKS(J),ZSTAR,
     $                  ILI(NBEGIN),ILI(NFINISH)
1108      FORMAT(' ZSTAR outside range on line',2I5,'   spot',2I5,
     .  '        ZSTAR=',F8.5,' range=',2I5)
          GOTO 1200
        ENDIF
        CPART=0.0
        SPART=0.0
        SUMFP=0.0
        FOMWT=0.0
CHEN>
        FOMAVE=0.0
        ZI=0
CHEN<
        DO 85 I=NBEGIN,NFINISH
          ZI=ILI(I)/CELL(3)
          ZDIFF=ZSTAR-ZI
          IF(ZDIFF.NE.0) THEN
            ARGEXP=-0.25*BTEMP*ZDIFF**2
            ARGSINC=0.5*PI*ZDIFF*CELL(3)
            SINCF=SIN(ARGSINC)/ARGSINC
            SINCDAMP=SINCF*EXP(ARGEXP)
          ELSE
            SINCDAMP=1.0
          ENDIF
          PHAS=PHIN(I)*DRAD
          CPART=CPART+SINCDAMP*COS(PHAS)*FPIN(I)
          SPART=SPART+SINCDAMP*SIN(PHAS)*FPIN(I)
          SUMFP=SUMFP+SINCDAMP*FPIN(I)
          FOMWT=FOMWT+SINCDAMP*FPIN(I)*FOMIN(I)
CHEN>
          FOMAVE=FOMAVE+FOMIN(I)
CHEN<
85      CONTINUE
C
        FREF=0.5*SQRT(SPART**2+CPART**2)                ! Here for divide by two
        PPHS=RDEG*ATAN2(SPART,CPART)
CHEN>
C       PHS=PPHS*IP1-IP2
        PHS=AMOD(PPHS*IP1-IP2,360.0)
        if(SUMFP.eq.0.0)then
          FOM=FOMAVE/(1+NFINISH-NBEGIN)
        else
          FOM=FOMWT/SUMFP
        endif
CHEN<
        RADSQ=NH**2*ASTAR**2 + NK**2*BSTAR**2 +
     .          2.0*NH*NK*COS(DRAD*ABANG)*ASTAR*BSTAR+ZI**2
        BSCALE=EXP(-0.25*RADSQ*BFACTOR)
      PHASE(J)=PHS
      AMP(J)=FREF*FOM*SFACTOR*BSCALE
C
C--------------------ABOVE IS CALCULATION OF PHASE AT EXACT VALUE OF ZSTAR.
C                    IT IS BASED ON THE SUM OF DAMPED SINC FUNCTIONS, WITH
C                    DAMPING SET TO BTEMP=80, AND A SINC FUNCTION WHICH FALLS
C                    TO ZERO AT TWO REFLECTIONS AWAY FROM THE POINT BEING
C                    CALCULATED. THUS THE CALCULATION GIVES DOUBLE THE VALUE
C                    OF F WHICH WOULD BE OBTAINED BY SIMPLE INTERPOLATION.
C                    AFTER DIVIDING THE RESULT BY TWO,THE OUTPUT COLUMN 
C                    AMPREF IS THEREFORE DIRECTLY COMPARABLE WITH THE INPUT
C                    AMPLITUDES.
C
        IZLESS=ZSTAR*CELL(3)
        IZLESS=NBEGIN+(IZLESS-ILI(NBEGIN))
        IZMORE=IZLESS+1
        WRITE(6,1111) IHS(J),IKS(J),ZS(J),NH,NK,ZSTAR,
     $          FREF,PHS,PHIN(IZLESS),PHIN(IZMORE),FOM,
     $          IHI(IZLESS),IKI(IZLESS),ILI(IZLESS)
1111    FORMAT(2I4,F7.3,2I4,F7.3,G9.1,F7.1,2F7.1,F7.2,3I4)
C
        NOUTPUT=NOUTPUT+1
      ENDIF
1200  CONTINUE
C
C  NOW CLOSE OUTPUT FILE
C
100   WRITE(6,1105) NHOLE,NOUTPUT,NOTFOUND
1105  FORMAT(I10,' STRUCTURE FACTORS REQUESTED',/,I10,
     .  ' STRUCT FACTORS OUTPUT',/,/,I10,' NOT FOUND')
C
      RETURN
C
 980  CONTINUE
        write(*,'('':: '')')
        write(*,'(''::'',78(''=''))')
        write(*,'(''::'',78(''=''))')
        write(*,'(''::'',78(''=''))')
        write(*,'(''::'',78(''=''))')
        write(*,'(''::'',78(''=''))')
        write(*,'(''::'',78(''=''))')
        write(*,'(''::'',78(''=''))')
        write(*,'(''::  '')')
        write(*,'(''::'',30X,''ERROR in maketran.'')')
        write(*,'(''::  '')')
        write(*,'(''::'',78(''=''))')
        write(*,'(''::  '')')
        write(*,'(''::  '')')
        write(*,'(''::'',78(''You need to correct the error above,'',
     .                      '' otherwise this will not work.''))')
        write(*,'(''::  '')')
        write(*,'(''::  '')')
        write(*,'(''::'',78(''=''))')
        write(*,'(''::'',78(''=''))')
        write(*,'(''::'',78(''=''))')
        write(*,'(''::'',78(''=''))')
        write(*,'(''::'',78(''=''))')
        write(*,'(''::'',78(''=''))')
        write(*,'(''::'',78(''=''))')
        write(*,'(''::'',78(''=''))')
        write(*,'('':: '')')
      STOP
C
      END
C
*********************************************************************
      SUBROUTINE ALASYM(IH,IK,Z,IP1,IP2,LSPEC,IPTEST,WSTAR,ISPGRP)
C
C  Arms length routine to call ASYM. All matrices for manipulating
C   H,K's are in this subroutine instead of .MAIN (or other)
C
      LOGICAL LSPEC
      INTEGER*4 IP1,IP2
C
      INTEGER*2 ISPEC(5,17)
      DATA ISPEC/7*0,1,3*0,1,4*0,1,4*0,1,3*0,3*1,2*0,3*1,0,-1,3*1,0,1,
     A 3*1,4*0,1,0,0,4*1,0,5*1,8*0,1,0,1,1,5*0,1,4*0,1,1,0/
      INTEGER*2 IGO(8,17)
      DATA IGO/8*5,2*4,2*5,2*4,2*5,
     A 4,5,4,5,4,5,4,5,  4,5,4,5,4,5,4,5,  4,5,4,5,4,5,4,5,
     B 2,4,2,5,2,4,2,5,  2,4,2,5,2,4,2,5,  2,4,2,5,2,4,2,5,
     C 2,4,2,5,2,4,2,5,  3,4,3,5,3,4,3,5,  1,2,1,4,1,2,1,5,
     D 1,2,1,4,1,2,1,5,  4,5,4,5,3,5,3,5,  2,4,2,4,1,5,1,5,
     E 2,4,2,4,1,5,1,5,  3,4,3,5,1,4,1,5,  2,3,2,4,1,3,1,5/
C
C     IMAT SHOWS WHICH MATRICES WILL BE USED FROM MAT FOR EACH SPACE GROUP
C       THE FIRST ELEMENT OF EACH IS PASSED TO SET,ASYM FOR LATER USE.
C       THE SAME IS DONE FOR IGO WHICH CONTROLS PROGRAM FLOW IN SET,ASYM
C       AND FOR ISPEC WHICH INDICATES SPECIAL REFLECTIONS.
C
      INTEGER*2 IMAT(5,17)
      DATA IMAT/ 1,1,1,1,1,    1,2,1,1,1,    1,3,1,1,1,
     A           1,4,1,1,1,    1,3,1,1,1,    1,2,1,3,1,
     B           1,2,1,4,1,    1,2,1,6,1,    1,2,1,3,1,
     C           1,2,7,5,1,    1,8,1,2,3,    1,8,1,9,6,
     D           1,10,11,12,1, 1,8,1,10,11,  1,9,1,10,11,
     E           1,2,10,5,11,  1,8,9,10,11/
      INTEGER*2 MAT(8,12)
      DATA MAT/   -1,0,0,-1,-1,0,0,-1,      1,0,0,1,-1,0,0,-1,
     A            1,0,0,-1,1,0,0,-1,        1,0,0,-1,1,0,180,-1,
     B            0,1,-1,0,1,0,0,1,        1,0,0,-1,1,180,180,-1,
     C            0,-1,1,0,1,0,0,1,         0,1,1,0,1,0,0,-1,
     D            0,1,1,0,-1,0,0,1,         0,-1,1,1,-1,0,0,-1,
     E            -1,-1,1,0,1,0,0,1,        1,1,-1,0,-1,0,0,-1/
      LOGICAL LREV(17)
      DATA LREV/9*.FALSE.,.TRUE.,2*.FALSE.,.TRUE.,2*.FALSE.,.TRUE.,
     1.FALSE./
C        THESE MATRICES ARE USED BY ASYM TO TRANSFORM ALL REFLECTIONS
C           TO THE STANDARD ASYMMETRIC UNIT AND TO PICK OUT THE SPECIAL
C           REFLECTIONS.
C#####################################################################
C
C     NUMBER   SPACEGROUP    ASYMMETRIC UNIT        REAL   IMAGINARY
C
C          1          P1         H>=0
C
C          2         P21         H,Z>=0              Z=0
C
C          3         P12         H,K>=0              K=0
C
C          4        P121         H,K>=0              K=0
C
C          5         C12         H,K>=0              K=0
C
C          6        P222         H,K,Z>=0            H=0
C                                                    K=0
C                                                    Z=0
C
C          7       P2221         H,K,Z>=0          (0,2N,Z)  (0,2N+1,Z)
C                                                    (H,K,0)
C                                                    (H,0,Z)
C
C          8      P22121         H,K,Z>=0            (H,K,0)
C                                                   (2N,0,Z)  (2N+1,0,Z)
C                                                   (0,2N,Z)  (0,2N+1,Z)
C
C          9        C222         H,K,Z>=0            (H,K,0)
C                                                    (H,0,Z)
C                                                    (0,K,Z)
C
C         10          P4         H,K,Z>=0            (H,K,0)
C
C         11        P422         H,K,Z>=0            (H,K,0)
C                                K>=H                (H,0,Z)
C                                                    (0,K,Z)
C                                                    (H,H,Z)
C
C         12       P4212         H,K,Z>=0            (H,K,0)
C                                K>=H                (H,H,Z)
C                                                   (2N,0,Z)   (2N+1,0,Z)
C                                                   (0,2N,Z)   (0,2N+1,Z)
C
C         13          P3         H,K>=0
C
C         14        P312         H,K>=0              (H,H,Z)
C                                K>=H
C
C         15        P321         H,K>=0              (H,0,Z)
C                                 K>H                (0,K,Z)
C
C         16          P6       H,K,Z>=0             (H,K,0)
C
C         17        P622         H,K,Z>=0            (H,K,0)
C                                K>=H                (H,H,Z)
C
C#################################################################
C
C         ALTHOUGH THE PHASE CHANGE PART OF ASYM IS NOT USED IN THIS INTENSITY
C         MERGING PROGRAM, THE SAME ASYM SUBROUTINE AS USED BY S.D.FULLER'S
C         ORIGMERG PROGRAM IS USED HERE FOR THE SAKE OF CONSISTENCY.
C         THUS IP1 AND IP2 ARE NOT USED BY THIS PROGRAM.
C       IP1 AND IP2 GENERATE THE RELATIONSHIP BETWEEN PHASES OF REFLECTIONS
C         IN THE UNIQUE ASYMMETRIC UNIT AND THE INPUT REFLECTIONS. THE
C         REFLECTIONS FROM PREVIOUS FILMS WILL BE TRANSFORMED TO LIE IN THE
C         SAME POSITIONS AS THE INPUT REFLECTIONS AND ORIGIN REFINEMENT
C         WILL BE PERFORMED IN P1.
C       LSPEC IS TRUE A REFLECTION IS SPECIAL, HAS ITS PHASE RESTRICTED BY
C         SYMMETRY. IPTEST IS 0 IF THE REFLECTION SHOULD BE REAL AND 90
C         IF IT SHOULD BE IMAGINARY
C
      CALL ASYM(IH,IK,Z,IP1,IP2,LSPEC,IPTEST,WSTAR,
     1  MAT(1,IMAT(1,ISPGRP)),MAT(1,IMAT(2,ISPGRP)),
     2  MAT(1,IMAT(3,ISPGRP)),MAT(1,IMAT(4,ISPGRP)),
     3  MAT(1,IMAT(5,ISPGRP)),
     4  IGO(1,ISPGRP),ISPEC(1,ISPGRP),LREV(ISPGRP))
      RETURN
      END
C*************************************************************************
      SUBROUTINE ASYM(IH,IK,Z,IP1,IP2,SPEC,IPTEST,WSTAR,
     1  A1,A2,A3,A4,A5,IGO,ISPEC,LREV)
      INTEGER*2 A1(8),A2(8),A3(8),A4(8),A5(8),IGO(8),ISPEC(5)
      INTEGER*4 IP1,IP2
      LOGICAL SPEC,LREV
C
C      WRITE(6,904)A1,A2,A3,A4,A5,IH,IK,Z,IP1,IP2,SPEC,IPTEST,WSTAR
      IF(IH.LT.0) CALL MULT(A1,IH,IK,Z,IP1,IP2)
50    INDEX=1
      IF(IK.GE.0) INDEX=INDEX+1
      IF(Z.GE.0.0) INDEX=INDEX+2
      IF(IH.LT.IABS(IK)) INDEX=INDEX+4
      INDEX=IGO(INDEX)
C      WRITE(6,902) INDEX
902   FORMAT (I10)
C      WRITE(6,901) IH,IK,Z,IP1,IP2
901   FORMAT(2I5,F10.5,2I5)
      GO TO (100,150,200,250,500), INDEX
C
C     INDEX CLASSIFIES THE REFLECTION BY ITS INDICES
C     IGO INDICATES WHICH MATRIX WILL BRING THE REFLECTION
C        INTO THE UNIQUE ASYMMETRIC UNIT FOR A GIVEN INDEX
C
C    INDEX    K>=0     Z>=0   /K/>=/H/
C      1       NO       NO      NO
C      2       YES      NO      NO
C      3       NO       YES     NO
C      4       YES      YES     NO
C      5       NO       NO      YES
C      6       YES      NO      YES
C      7       NO       YES     YES
C      8       YES      YES     YES
C
C      P622 IS THE HIGHEST SYMMETRY AND ITS ASYMMETRIC UNIT IS ONLY
C         INDEX = 8
C
100    CALL MULT(A5,IH,IK,Z,IP1,IP2)
C       WRITE(6,900) A5,IH,IK,Z,IP1,IP2
900    FORMAT(8I5,5X,2I5,F10.5,2I10)
       GO TO 50
150    CALL MULT(A4,IH,IK,Z,IP1,IP2)
C       WRITE(6,900)A4,IH,IK,Z,IP1,IP2
       GO TO 50
200    CALL MULT(A3,IH,IK,Z,IP1,IP2)
C       WRITE(6,900)A3,IH,IK,Z,IP1,IP2
       GO TO 50
250    CALL MULT(A2,IH,IK,Z,IP1,IP2)
C       WRITE(6,900)A2,IH,IK,Z,IP1,IP2
C
C      AFTER REFLECTIONS HAVE BEEN PLACED INTO THE ASYMMETRIC UNIT
C       THEY ARE EXAMINED TO SEE IF THEY ARE SPECIAL REFLECTIONS,
C       ONES WHOSE PHASE MUST BE EITHER REAL (0 OR PI) OR IMAGINARY
C       (PI/2 OR 3*PI/2)
C
500    CONTINUE
       IF(IH.EQ.0 .AND. IK.LT.0) CALL MULT(A1,IH,IK,Z,IP1,IP2)
       IF(LREV .AND. IH.EQ.0) CALL MULT(A4,IH,IK,Z,IP1,IP2)
       SPEC=.FALSE.
       IPTEST=0
C      SPEC WILL BE TRUE IF THE REFLECTION IS SPECIAL .
C      IPTEST WILL BE 0 IF REAL AND 90 IF IMAGINARY
C      ISPEC INDICATES THE CONDITIONS FOR THE REFLECTIONS
C          ISPEC(1)=1  H=0 SPECIAL
C          ISPEC(2)=1  K=0 SPECIAL
C          ISPEC(3)=1  Z=0 SPECIAL
C          ISPEC(4)=1  H=K SPECIAL
C          ISPEC(5)=1  IF FOR H=0 OR K=0 K+H ODD INDICATES AN
C                       IMAGINARY VALUE FOR THE REFLECTION
C                      ALL OTHER SPECIAL REFLECTIONS ARE REAL
C
      IF(ISPEC(1).LT.1) GO TO 510
      IF(IH.EQ.0) GO TO 560
510   CONTINUE
      IF(ISPEC(2).LT.1) GO TO 520
      IF(IK.EQ.0) GO TO 560
520   CONTINUE
      IF(ISPEC(3).LT.1) GO TO 530
      IF(ABS(Z).LT.WSTAR) GO TO 570
530   CONTINUE
      IF(ISPEC(4).LT.1) GO TO 600
      IF(IH.EQ.IK) GO TO 570
      GO TO 600
560   CONTINUE
      IF(ISPEC(5).EQ.0) GO TO 570
      IF(ISPEC(5).EQ.-1) GO TO 563
      I=IH+IK
      GO TO 565
563   I=IK
565   I2=2*(I/2)
      IF(I.GT.I2) IPTEST=90
570   SPEC=.TRUE.
600    CONTINUE
C      WRITE(6,903)IH,IK,Z,IP1,IP2,SPEC,IPTEST,WSTAR
903   FORMAT(2I5,F10.5,2I10,L4,I5,F10.5)
      RETURN
      END
C
C*******************************************************************************
      SUBROUTINE MULT(IA,IH,IK,Z,IP1,IP2)
C
C     DOES MATRIX MULTIPLICATION TO BRING REFLECTIONS INTO THE
C       ASYMMETRIC UNIT.
C
C     (H' K' Z' AMP' PHS')=(H K Z AMP PHI) <A>
C
C
C        <A> HAS FORM     IA(1)  IA(3)     0      0  IA(6)
C                         IA(2)  IA(4)     0      0  IA(7)
C                             0      0 IA(5)      0      0
C                             0      0     0      1      0
C                             0      0     0      0  IA(8)
C           FOR ALL CASES.
C
C
      INTEGER*2 IA(8)
      INTEGER*4 IP1,IP2
C      WRITE(6,900)IA,IH,IK,Z,IP1,IP2
      IH1=IA(1)*IH+IA(2)*IK
      IK=IA(3)*IH+IA(4)*IK
      IH=IH1
      Z=IA(5)*Z
      IP1=IA(8)*IP1
      IP2=IP2+IA(6)*IH+IA(7)*IK
C      WRITE(6,900)IA,IH,IK,Z,IP1,IP2
C900   FORMAT(' IA,IH,IK,Z,IP1,IP2 ',8G5.1,10X,5G10.5)
      RETURN
      END
C
C*******************************************************************************
C
      SUBROUTINE INSERT(ARRAY,NHOLE,ISHAPE,RAD,NX,NY,
     .                  APART,BPART,XC,YC)
      DIMENSION ARRAY(1),APART(1),BPART(1),XC(1),YC(1)
C
c     LOGICAL IAMPLIM
C
      NX2  = NX/2
      NX21 = NX/2 + 1
      NXP2 = NX + 2
      NY2  = NY/2
      IATOT= 2*NX21*NY
      NINSERT=0
C
      DO 200 NH=1,NHOLE
C
C       Set hole centre
        X=XC(NH)
        Y=NY2+YC(NH)            ! shifts to transform coordinates
        IXC=X+0.5               ! nearest pixel to centre of spot
        IYC=Y+0.5               !   "      "
C
C       Set hole limits
        RA=RAD
        RADSQ=RA*RA
        IRAD=RA+0.5             ! rounded to nearest integer
        IX1=IXC-IRAD
        IX2=IXC+IRAD
        IY1=IYC-IRAD
        IY2=IYC+IRAD
C       WRITE(6,199) NH,IX1,IX2,IY1,IY2,APART(NH),BPART(NH)
C199    FORMAT(' subr INSERT, spot',5I5,2F8.0)
        IF(IX2.GT.NX2.OR.IY1.LT.0.OR.IY2.GT.NY) THEN
          WRITE(6,198) NH,XC(NH),YC(NH)
198       FORMAT(' Hole',I5,' outside edge of transform box',2F8.1)
          GO TO 200
        ENDIF
C
C       Scan over hole -- holes can overlap
        DO 300 IY=IY1,IY2
          YSQ=(IY-Y)**2
          IYTRUE=IY-NY2
C         IX can be negative
          DO 310 IX=IX1,IX2
            GWT = 1.0
            RCONJUG = 1.0
            PHSORG = (-1.0)**(IX+IYTRUE)
            IF(ISHAPE.LE.2) THEN
              RSQ=(IX-X)**2+YSQ
              IF(RSQ.GT.RADSQ) GO TO 310      ! from exact centre
C                  Gaussian weight from exact centre for soft holes
              IF(ISHAPE.EQ.2) GWT=EXP(-2.0*RSQ/RADSQ)
            ENDIF
C           Check if point in neg X half transform - use Friedel mate
            IF(IX.GE.0) THEN
              INDEX = (NX21*IY+IX)*2+1
            ELSE
              INDEX = (NX21*(NY-IY)-IX)*2+1
CHEN>
C-------------Friedel mate is complex conjugated
              RCONJUG = -1.0
CHEN<
            END IF
C           Check not negative half of F(0,0) centre hole
            IF((IX.LT.0).AND.(X.EQ.0.).AND.(Y.EQ.0.)) GO TO 310
C
            IF((INDEX.LT.1).OR.(INDEX.GT.IATOT))GOTO 310 !outside transform
C
            NINSERT=NINSERT+1
            ARRAY(INDEX)   = ARRAY(INDEX) + 
     .                       PHSORG*APART(NH)*GWT
            ARRAY(INDEX+1) = ARRAY(INDEX+1) + 
     .                       RCONJUG*PHSORG*BPART(NH)*GWT
C
C           On IX=0 need another segment
C               But not for centre hole
            IF(IX.EQ.0.AND.((X.NE.0.).OR.(Y.NE.0.))) THEN
CHEN>
C-------------Friedel mate is complex conjugated
              if(IYTRUE.LT.0)then
                RCONJUG = -1.0
              endif
CHEN<
              INDEX = NX21*(NY-IY)*2+1
              IF((INDEX.LT.1).OR.(INDEX.GT.IATOT)) GO TO 310
              NINSERT=NINSERT+1
              ARRAY(INDEX)   = ARRAY(INDEX) + 
     .                         PHSORG*APART(NH)*GWT
              ARRAY(INDEX+1) = ARRAY(INDEX+1) +
     .                         RCONJUG*PHSORG*BPART(NH)*GWT
            ENDIF
C
310       CONTINUE
300     CONTINUE
200   CONTINUE
      NEXPECT = 4.0*RAD**2
      IF(ISHAPE.LE.2) NEXPECT = 3.14159*RAD**2
        if(NHOLE.ne.0)then
          ITMP = NINSERT/NHOLE
        else
          write(*,'(''ERROR: NHOLE = 0'')')
          stop
        endif
        WRITE(6,201) NINSERT, ITMP, NEXPECT
201     FORMAT(' Number of transform points inserted (pix/spot)',
     .    I12,' (',I6,')',/,
     .    '  compared with expected number of pix/spot which is',I6)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE CTF(ARRAY,NX,NY,THETATR,WL,CS,DFMID1,DFMID2,ANGAST)
      DIMENSION ARRAY(1)
      IF(DFMID1.EQ.0.0.AND.DFMID2.EQ.0.0) THEN
        WRITE(6,10)
10      FORMAT(/,' No CTF correction applied')
        RETURN
      ELSE
        WRITE(6,20) DFMID1,DFMID2,ANGAST
20      FORMAT(/,' Appying CTF simulation using DFMID1,DFMID2,ANGAST =',
     .    2F9.1,F8.1)
      ENDIF
      TWOPI = 2*3.14159265
      ANGAST=ANGAST*TWOPI/360.0
      NX2  = NX/2
      NX21 = NX/2+1
      NXP2 = NX + 2
      NY2  = NY/2
      NY21 = NY/2 + 1
C
      DO 300 IY = 1,NY
        TY = IY-1-NY2
        INDY = (IY-1)*NXP2
      DO 300 IX = 1,NX21
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
                CNTRST=-SIN(CHI)
           ELSE
                CNTRST=0.0
           ENDIF
        ARRAY(INDEX)   = ARRAY(INDEX) * CNTRST
        ARRAY(INDEX+1) = ARRAY(INDEX+1) * CNTRST
300   CONTINUE
      RETURN
      END
C*******************************************************************************
C  FIDDLING WITH THE INDEXING TO GET CORRECT MATCH TO INDEXING CONVENTION
C  USEFUL IN A NUMBER OF SPACE GROUPS -- SEE WRITE-UP AT TOP OF PROGRAM.
CHEN>
C      SUBROUTINE FIDDLE(IH,IK,Z,REVHK,SGNXCH,ROT180)
C
      SUBROUTINE FIDDLE(IH,IK,Z,REVHK,SGNXCH,ROT180,ROT90,REVHND)
CHEN<
      IF(REVHK.EQ.0.0) GO TO 225
      I=IH
      IH=IK
      IK=I
      Z=-Z
225   CONTINUE 
      IF(SGNXCH.EQ.0.0) GO TO 230
      IK=-IK
      Z=-Z
  230 IF(ROT180.EQ.0) GO TO 231
      IH=-IH
      IK=-IK
231   CONTINUE
CHEN>
      if(ROT90.ne.0)then
C-------This here is a ROT90,
C        I=IH
C        IH=-IK
C        IK=I
C-------This here is a ROT90,
C-------rotating in the other direction than in all other programs.
        I=IH
        IH=IK
        IK=-I
      endif
      if(REVHND.ne.0)then
        Z=-Z
      endif
CHEN<
      RETURN
      END
C******************************************************************************
C              TO APPLY ORIGIN AND BEAMTILT PHASE-SHIFT.
      FUNCTION PHSHFT(IH,IK,OX,OY,TX,TY,BEAMSHFT,B)
      DIMENSION BEAMSHFT(4)
C       ! attempt to not make it p3 specific.
      ASTAR=BEAMSHFT(2)
      BSTAR=BEAMSHFT(3)
      PHSHFT=IH*OX + IK*OY + B*(IH*TX*ASTAR+IK*TY*BSTAR)
      RETURN
      END
C******************************************************************************
