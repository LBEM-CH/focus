C  EDMTZ --- PROGRAM TO : READ IN H,K,Z,DELF FROM TOM'S PROGRAM (MERGE)
C                       : READ IN STANDARD MTZ FILE CONTAINING PHASES IN 3-D
C                       : WRITE OUT MTZ FILE SUITABLE FOR FOURIER CALCULATION
C
C       VX 1.0  RH      21.5.94
C
C  DIFFERENCE MAP WILL BE DONE IN SPACE GROUP P1 -- NO SYMMETRY APPLIED.
C
C  15.3.84 INITIAL PROGRAM WRITTEN USING ASYM SUBROUTINE BUT INCORPORATING
C                   A NUMBER OF P3 SPACE GROUP SPECIFIC ASPECTS, NAMELY :
C       1. THE LATTICE LINE POINTER FOR THE REFERENCE PHASE STORAGE LOCATION 
C            HAS INDICES H+1,K+1 - SO THAT SPACE GROUPS WITH ASYMMETRIC UNITS
C            HAVING H OR K NEGATIVE WILL FAIL.
C       2. THE FORMULA FOR CALCULATION OF PHASE AT AN ARBITRARY ZSTAR POSITION
C            DOES NOT TREAT THE SYMMERY OF SPACE GROUPS WITH LATTICE LINES
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
C                        LFPZERO,         diagnostic O/P if no LCF data
C                        SFACTOR,         scale factor to increase F's on O/P
C                        LSIGN            change sign of FANOM
C
C                   2.  HISNEW      (A80)   NEW HISTORY LABEL TO DESCRIBE MAP
C
C###############################################################################
C
C      ANGLE BETWEEN A AND B FOR ALL SPACEGROUPS BUT P1 IS FIXED 
C              (BUT IS READ IN LATER AS CELL(6) ANYWAY)
C
C
      PARAMETER (BTEMP=80.0)
      PARAMETER (DRAD=0.0174532)
      PARAMETER (RDEG=57.295779)
      PARAMETER (PI=3.14159)
      PARAMETER (MAXPTS=6000)
      PARAMETER (IRNGE=20)
C     PARAMETER (IRNGESQ=IRNGE*IRNGE)
      PARAMETER (IRNGESQ=400)
C
C     .. parameters for mtz aspects
      PARAMETER (NLOC=40)
      PARAMETER (MCOLS=200)
      PARAMETER (NPAR=200)
C
      DIMENSION STANG(17)
      DATA STANG/2*0.0,10*90.0,5*120.0/
C
      DIMENSION CELL(6)
      DIMENSION RSYMX(4,4,96)
C
      LOGICAL EOF
C
C     .. Local Arrays ..
      REAL ADATAIN(MCOLS),ADATAOUT(MCOLS),DUM(2,MCOLS)
      INTEGER IH(3),JPOINT(NLOC),LOOKUP(NLOC)
      CHARACTER OUTTYP(NLOC)*1,LSPRGI(NLOC)*30,LSPRGO(NLOC)*30,
     +  TITNEW*70,HISNEW(20)*80,CTPRGI(NLOC)*1,DUMMY*10
C
C     .. Scalars for Parser ..
      INTEGER NTOK
      LOGICAL LEND
      CHARACTER KEY*4,LINE*400
C     ..
C     .. Arrays for Parser ..
      REAL FVALUE(NPAR)
      INTEGER IBEG(NPAR),IDEC(NPAR),IEND(NPAR),ITYP(NPAR)
      CHARACTER CVALUE(NPAR)*4

C
      INTEGER IHI(MAXPTS),IKI(MAXPTS),ILI(MAXPTS)
      REAL FPIN(MAXPTS),PHIN(MAXPTS),SIGFPIN(MAXPTS),FOMIN(MAXPTS)
C
      INTEGER*2 IBEGIN(IRNGE,IRNGE),IFINISH(IRNGE,IRNGE),IP1,IP2
C
C---- NLPRGI  =  number of input labels
      DATA NLPRGI,LSPRGI/7,'H','K','L','FP','SIGFP','PHS',
     +     'FOM',33*' '/
C---- NLPRGO  =  number of output labels
      DATA NLPRGO,LSPRGO/12,'H','K','L','DELF','PHS','FP','SIGFP',
     +     'FD','SIGFD','FOM','AFD','APH',28*' '/
C---- .. This code signs which input columns are essential (LOOKUP)
      DATA CTPRGI/'H','H','H','F','Q','P','W',33*' '/
      DATA OUTTYP/'H','H','H','F','P','F','Q','F','Q','W',
     +     'F','F',28*' '/
      DATA LOOKUP/-1,-1,-1,-1,-1,-1,-1,33*0/
      DATA TITNEW/' '/, HISNEW/20*' '/, JPOINT/40*0/
C
C
      DATA IBEGIN /IRNGESQ*-999/, IFINISH /IRNGESQ*-999/
      DATA NBEGIN /-999/,NFINISH/-999/
      LOGICAL LSTRUCT0,LFPZERO,LSIGN
C
      DELFSUM=0.0
      AMPDERSUM=0.0
      SMIN=40000.
      SMAX=0.
C
C     READ SPACE GROUP NUMBER, AND FLAG FOR OUTPUT OF ALL FP'S
C
      READ(5,*) ISPGRP,LFPZERO,SFACTOR,LSIGN
      WRITE(6,9) ISPGRP,LFPZERO,SFACTOR,LSIGN
9     FORMAT(' Space group number..........',I3/
     $       ' Write Fp if no phase........',L3/
     $       ' Scale factor for Fs.........',F8.2/
     $       ' Change sign of Fanom........',L3)
C
      IF(LSIGN) THEN
        SIGN=-1.0
      ELSE
        SIGN=1.0
      ENDIF
C
C  INPUT OF PHASES OF NATIVE DATA 
C       ALL PHASES ARE STORED AS VALUES BETWEEN -180.0 AND 180.0 DEGREES
C       THE SIN AND COS OF EACH PHASE IS ALSO STORED FOR USE IN AVERAGES
C        (TITLE WITH THE PHASES IS CARRIED INVISIBLY IN /LCF/ COMMON BLOCK)
C
      CALL CCPFYP
      CALL MTZINI
      CALL LROPEN(1,'HKLIN',3,IERR)
      CALL LRCELL(1,CELL)
      CALL LRSYMM(1,NSYMX,RSYMX)
      IF(IERR.NE.0) THEN
        WRITE (6,8)IERR
8       FORMAT(' ERROR ON INPUT OF MTZ FILE, IERR=',I5)
        STOP
      ENDIF
C---- Find out how many columns and reflections in input file
      CALL LRINFO(1,DUMMY,NCOL,NREF,DUM)
      CALL LKYASN(1,NLPRGI,LSPRGI,CTPRGI,LOOKUP)
C
      IF(STANG(ISPGRP).NE.CELL(6)) THEN
        WRITE(6,1109) STANG(ISPGRP),CELL(6)
1109    FORMAT(' Conflict between cell angles from space group',
     $  ' and lcf input file, STANG, CELL=',2F8.3)
        STOP
      ENDIF
C
      ABANG = 180.0 - CELL(6)
      ASTAR=1.0/(CELL(1)*SIN(DRAD*ABANG))
      BSTAR=1.0/(CELL(2)*SIN(DRAD*ABANG))
      CSTAR=1.0/ CELL(3)
      WSTAR=CSTAR/3.0
      FOMLIMIT=0.01             !THIS CORRESPONDS TO FOM OF 0.01
      N=0
      NREC=0
C
1     CALL LRREFF(1,RESOL,ADATAIN,EOF)
      IF(EOF) GO TO 10
      N=N+1
C
      IF(N.GT.MAXPTS) THEN
        WRITE(6,1106) MAXPTS
1106    FORMAT(' Reference phase list too big for program dimension.
     $            MAXPTS= ',I6)
        STOP
      ENDIF
C
      NREC=NREC+1
      IHI(N)=ADATAIN(1)
      IKI(N)=ADATAIN(2)
      ILI(N)=ADATAIN(3)
      IF(RESOL.LT.SMIN) SMIN=RESOL
      IF(RESOL.GT.SMAX) SMAX=RESOL
      IF(ADATAIN(7).LT.FOMLIMIT) THEN
        N=N-1
        GOTO 1
      ENDIF
      FPIN(N)=ADATAIN(4)
      PHIN(N)=ADATAIN(6)
      SIGFPIN(N)=ADATAIN(5)
      FOMIN(N)=ADATAIN(7)
      IF(N.EQ.1) THEN
        IHOLD=IHI(N)
        IKOLD=IKI(N)
        IBEGIN(IHOLD+1,IKOLD+1)=1
      ENDIF
      IF(.NOT.((IHI(N).EQ.IHOLD).AND.(IKI(N).EQ.IKOLD))) THEN
        IFINISH(IHOLD+1,IKOLD+1)=N-1
        IHOLD=IHI(N)
        IKOLD=IKI(N)
        IBEGIN(IHOLD+1,IKOLD+1)=N
      ENDIF
      GO TO 1
C
10    WRITE(6,101)
101   FORMAT(' end of mtz input')
      IFINISH(IHOLD+1,IKOLD+1)=N
C
      WRITE(6,1100)N,FOMLIMIT,NREC
1100  FORMAT(I10,' Phases with FOM > ',F5.2,' input on stream 1'/
     $  I10,' total records on stream 1.')
      CALL LRCLOS(1)
C
C  NOW READ THE TITLE PLUS OTHER ODDS AND ENDS DATA FROM MERGE PROGRAM
C               (THIS IS THE TITLE WITH THE DELTA F'S)
C
      CALL CCPDPN(4,'DELFDATA','READONLY','F',0,0)
      READ(4,1101) HISNEW(2)
      WRITE(6,1103) HISNEW(2)
1103  FORMAT(' Delf from film ----',A80)
      READ(4,*)
      WRITE(6,1104)
1104  FORMAT(' input format assumed to be:',
     $' H,K,Z,DELF,FD,SIGFD,FP,SIGFP,ANOM'/)
C
C  OPEN THE LCF FILE FOR OUTPUT OF DIFF MAP COEFFS :- H K L DELF AND PHASES.
C
      CALL LWOPEN(1,'HKLOUT')
      CALL PARSER(KEY,LINE,IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,NTOK,LEND,
     +            .FALSE.)
      CALL CCPUPC(KEY)
      IF(KEY.NE.'LABO') THEN
        STOP ' LABOUT card not present'
      ENDIF
      CALL LKYOUT(1,LSPRGO,NLPRGO,NTOK,LINE,IBEG,IEND)
      CALL LWASSN(1,LSPRGO,NLPRGO,OUTTYP,0)
      CALL LWSYMM(1,3,3,RSYMX,'P',143,'P3','PG3')
      READ(5,1101) HISNEW(1)
1101  FORMAT(A80)
      WRITE(6,1102) HISNEW(1)
1102  FORMAT(' TITLE FOR OUTPUT LCF FILE',A80)
      CALL LWTITL(1,TITNEW,0)
C----    Some history to append to output file
C        Only last 30 lines will be kept
      CALL LWHIST(1,HISNEW,2)
      CALL LWCELL(1,CELL)
C      
C  NOW INPUT THE DIFFERENCE AMPLITUDES FROM MERGE PROGRAM.
C       I.E.(H K ZSTAR DELF FD)
C
       NINPUT=0
       NOUTPUT=0
       NOTFOUND=0
C
C  HEADER TITLE FOR DIAGNOSTIC DATA OUTPUT
      WRITE(6,1112)
1112  FORMAT(' NKIN NKIN ZSTARIN  DELF   FD   SIGFD NH',
     $'  NK  ZSTAR     FP     SIGFP  PHASE  IPH- IPH+ FOM    ',
     $'IH-  IK-  IL-')
C
50    READ(4,*,END=100) NHIN,NKIN,ZSTARIN,DELF,
     $FD,SIGFD,FP,SIGFP,ANOM
      NINPUT=NINPUT+1
      NH=NHIN
      NK=NKIN
      ZSTAR=ZSTARIN
      IP1=1
      IP2=0
C
C  NOW CALL ASYM TO GET CORRECT INDICES IN STANDARD ASYMMETRIC UNIT.
C  THE NH,NK,ZSTAR RETURNED BY ASYM ARE IN THE STANDARD ASYMMETRIC UNIT.
C      PHASES FROM THE REFERENCE CURVES' ASYMMETRIC UNIT ARE TRANSFORMED
C          TO BE COMPARABLE BY TRANSFORMING THE REFERENCE DATASET PHASES
C          WITH IP1 AND IP2 TO CORRESPOND TO REFLECTIONS WITH THE SAME
C          INDICES AS THE INPUT REFLECTIONS. THE PHASES ARE THEN AVAILABLE
C          FOR A P1 SYNTHESIS.
C
      CALL ALASYM(NH,NK,ZSTAR,IP1,IP2,LSPEC,IPTEST,WSTAR,ISPGRP)
C
      NBEGIN=IBEGIN(NH+1,NK+1)
      NFINISH=IFINISH(NH+1,NK+1)
C
      IF((NBEGIN.EQ.-999).OR.(NFINISH.EQ.-999)) THEN
        IF(LFPZERO) THEN
          WRITE(6,1107)NH,NK,NHIN,NKIN
1107      FORMAT(' No structure factor from LCF file',
     $       2I5,', input reflection ',2I5)
          WRITE(6,1111)NHIN,NKIN,ZSTARIN,DELF,FD,SIGFD,NH,NK,ZSTAR,
     $                  FP,SIGFP
        ENDIF
        PHS=0.0
        FOM=0.0
        LSTRUCT0=.FALSE.
C
      ELSE              !CALCPHS
C
        LSTRUCT0=.TRUE.
        ZBEGIN=ILI(NBEGIN)/CELL(3)
        ZFINISH=ILI(NFINISH)/CELL(3)
        IF(.NOT.((ZSTAR.GT.ZBEGIN).AND.(ZSTAR.LT.ZFINISH))) THEN
          NOTFOUND=NOTFOUND+1
          WRITE(6,1108) NH,NK,NHIN,NKIN,ZSTAR,ZMULT,
     $                  ILI(NBEGIN),ILI(NFINISH)
1108      FORMAT(' ZSTAR outside range on line',2I5,'   spot',2I5,
     .  '        ZSTAR=',F8.5,F8.3,' range=',2I5)
          GOTO 50
        ENDIF
C
        CPART=0.0
        SPART=0.0
        SUMFP=0.0
        FOMWT=0.0
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
85      CONTINUE
C
        PPHS=RDEG*ATAN2(SPART,CPART)
        PHS=PPHS*IP1-IP2
        FOM=FOMWT/SUMFP
        APHS=(PHS+90.0)*IP1-IP2
C
C********************ABOVE IS CALCULATION OF PHASE AT EXACT VALUE OF ZSTAR.
C                    IT IS BASED ON THE SUM OF DAMPED SINC FUNCTIONS, WITH
C                    DAMPING SET TO BTEMP=80, AND A SINC FUNCTION WHICH FALLS
C                    TO ZERO AT TWO REFLECTIONS AWAY FROM THE POINT BEING
C                    CALCULATED. THUS THE CALCULATION GIVES DOUBLE THE VALUE
C                    OF F WHICH WOULD BE OBTAINED BY SIMPLE INTERPOLATION.
C                    AFTER DIVIDING THE RESULT BY TWO,THE OUTPUT COLUMN 
C                    AMPREF IS THEREFORE DIRECTLY COMPARABLE WITH THE INPUT
C                    AMPLITUDES.
C      WRITE(6,*)PHS,RDEG,SPART,CPART,ATAN2(SPART,CPART),IP1,IP2
C
        IZLESS=ZSTAR*CELL(3)
        IZLESS=NBEGIN+(IZLESS-ILI(NBEGIN))
        IZMORE=IZLESS+1
        WRITE(6,1111)NHIN,NKIN,ZSTARIN,DELF,FD,SIGFD,NH,NK,ZSTAR,
     $          FP,SIGFP,PHS,PHIN(IZLESS),PHIN(IZMORE),FOM,
     $          IHI(IZLESS),IKI(IZLESS),ILI(IZLESS)
1111    FORMAT(2I5,F8.4,3F6.1,2I4,F8.4,2F8.1,F7.1,2F7.1,F6.3,3I5)
C
      ENDIF             !CALCPHS
C
      IF(LFPZERO.OR.LSTRUCT0) THEN
C
C       CHANGE CONVENTION OF OUTPUT REFLECTIONS TO HAVE K POSITIVE.
        IF(NKIN.LT.0) THEN
          NHIN=-NHIN
          NKIN=-NKIN
          PHS=-PHS
          APHS=-APHS
        ENDIF
C
        ADATAOUT(1)=NHIN
        ADATAOUT(2)=NKIN
        ADATAOUT(3)=0        
        ADATAOUT(4)=DELF*SFACTOR
        ADATAOUT(5)=PHS
        ADATAOUT(6)=FP*SFACTOR
        ADATAOUT(7)=SIGFP*SFACTOR
        ADATAOUT(8)=FD*SFACTOR
        ADATAOUT(9)=SIGFD*SFACTOR
        ADATAOUT(10)=FOM
        ADATAOUT(11)=SIGN*ANOM*SFACTOR
        IF(ADATAOUT(10).EQ.0) THEN
          ADATAOUT(12)=0
        ELSE
          ADATAOUT(12)=APHS
        ENDIF
        CALL LWREFL(1,ADATAOUT)
        NOUTPUT=NOUTPUT+1
        DELFSUM=DELFSUM+ABS(DELF)
        AMPDERSUM=AMPDERSUM+FD
      ENDIF
      GO TO 50                  !NEXT REFLECTION
C
C  NOW CLOSE OUTPUT FILE
C
100   WRITE(6,1105)NINPUT,NOUTPUT
1105  FORMAT(I10,' DIFF COEFFS INPUT'/I10,' STRUCT FACTORS OUTPUT')
      CALL LWCLOS(1,3)
      IF(AMPDERSUM.NE.0.0) THEN
        DELDN=DELFSUM/NOUTPUT
        AMPDN=AMPDERSUM/NOUTPUT
        WRITE(6,1110) DELDN,AMPDN,DELDN/AMPDN
1110    FORMAT(' Average delta F is ',F10.1/
     $         ' Average F is       ',F10.1/
     $         ' Average (delta F/F)',F10.3)
      ENDIF
      STOP
      END
C*********************************************************************
      SUBROUTINE ALASYM(IH,IK,Z,IP1,IP2,LSPEC,IPTEST,WSTAR,ISPGRP)
C
C  Arms length routine to call ASYM. All matrices for manipulating
C   H,K's are in this subroutine instead of .MAIN (or other)
C
      LOGICAL LSPEC
      INTEGER*2 IP1,IP2
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
      INTEGER*2 IP1,IP2
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
      INTEGER*2 IA(8),IP1,IP2
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
