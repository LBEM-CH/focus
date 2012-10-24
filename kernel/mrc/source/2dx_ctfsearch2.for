C CTFSEARCH : remember also to change version number in first write statement.
C             derived from CTFREFINE originally, name changed 4.2.95
C
C       Version 2.04    14.1.98         RH  cosmetic MA21 E=-1.0 output statement
C       Version 2.03    22.12.97        RH  increase MAXINDEX to 30
C       Version 2.02    19.3.95         RH  writes out summary file
C       Version 2.01    1.3.95          RH  option IREF=2 for mtz input
C       Version 2.00    4.2.95          RH  CTFSEARCH phases only option (IPHASE=2)
C       Version 1.07    1.9.92          RH  option to use phase data (IPHASE=1)
C       Version 1.06    1.3.91          RH  bigger lcf curve storage NMAXC
C       Version 1.05    10.4.88         RH  option to use no ref data (IREF=-1)
C       Version 1.04    11.9.87         RH  cosmetic changes to this description
C       Version 1.03    30.6.87         JMB allows more spots (up to 800) -
C                                           also updated for new LCF routines.
C       Version 1.02    7. 6.85         RH  with improved definition of SIGMA.
C       Version 1.01    28.2.85         JMB modified
C       Version 1.00    1.10.84         RH
C
C       Rectangular images possible; ISIZEX given in data; Y components
C       of lattice parameters must be scaled by ISIZEX/ISIZEY before input.
C
C  PROGRAM TO REFINE C.T.F. OF AN IMAGE OF A TWO-DIMENSIONAL CRYSTAL BY
C   ALTERING THE DEFOCUS AND ASTIGMATISM.
C
C  INPUT :-
C         PARAMETERS DESCRIBING IMAGE   (AS IN CTFPLOT,CTFAPPLY,ORIGTILT)
C   CARD 0 : LIST LISTW QMAX ---
C                        IF LIST=.TRUE. (T) GIVES EXTENSIVE OUTPUT OF NUMBERS
C                        IF LISTW=.TRUE. GIVES INFORMATION FOR 'WILSON' PLOT
C                                       AS FOR LIST=.TRUE. BUT FOR Q <= QMAX
C   CARD 1 : AX AY BX BY ISIZEX DSTEP XMAG
C   CARD 2 : DFMID1 DFMID2 ANGAST CS KV
C   CARD 3 : TITLE FOR PLOT OF FINAL BEST FIT.
C   CARD 4 : ORIGH ORIGK TAXA TANGL REVHK ROT180 SGNXCH ROT90 ICTFREV IAQP2
C               (ICTFREV=1 inverts the contrast of the observed data, by )
C               (adding to their phase 180.0 degrees.)
C               (IAQP2 was a special symmetry case for a past project. Not used any more.)
C
C   CARD 5 : RESMIN RESMAX     RESOLUTION LIMITS TO BE USED IN ANGSTROMS.
C         PARAMETERS DESCRIBING REFERENCE DATA
C   CARD 6 : ISPGRP,IREF,NCYC,IPHASE,DFSTEP,DFRANGE
C                       (IREF=-1 if no reference data available)
C                       (IREF=0 FOR PROJECTION DATA)
C                       (IREF=1 FOR 3D LCF FILE DATA)
C                       (IREF=2 FOR 3D MTZ FILE DATA)
C               NCYC IS MAX NUMBER OF CYCLES OF CTF REFINEMENT.
C                       (IPHASE=0 uses only intensities for ctf-refine)
C                       (IPHASE=1 uses amps and phases for complex refinement)
C                       (IPHASE=2 uses phases alone for minimum residual)
C               DFSTEP,DFRANGE - stepsize and range for IPHASE=2 refinement
C   CARD 7 : FC=F SIGFC=SIGF PHCAL=PHS FOM=FOM ## (IF IREF=1 LCF CONTROL)
C   CARD 7 : LABIN AMP=F SIG=SIGF PHASE=PHS FOM=FOM ## (IF IREF=2 MTZ CONTROL)
C                       PROGRAM USES SIGFC AS TEST FOR PRESENCE OF F
C                       AND FOM AS TEST FOR PRESENCE OF MEASURED PHASE.
C
C         AX,AY   - LATTICE PARAMETERS (FROM NNBOX) OF (1,0) AND (0,1)
C         BX,BY   -                                     IN GRID UNITS.
C         ISIZEX   - SIZE OF DENSITOMETERED ARRAY IN X DIRECTION (E.G. 2048)
C         DSTEP   - DENSITOMETER STEPSIZE IN MICRONS
C         XMAG    - PRECISE MAGNIFICATION NORMALLY WORKED OUT FROM LATTICE
C                    PARAMETERS AND KNOWN CELL DIMENSIONS.
C         DFMID1  - DEFOCUS LEVEL (UNDERFOCUS +VE). IF DFMID2=DFMID1, IMAGE
C         DFMID2  -  IS NON-ASTIGMATIC. OTHERWISE AMOUNT OF DEFOCUS IN TWO
C                    ORTHOGONAL DIRECTIONS, DFMID! BEING DEFOCUS IN DIRECTION
C                    ANGAST (DEGS) RELATIVE TO X AND Y OF THE FOURIER TRANSFORM
C         CS      - SPHERICAL ABERRATION IN MM.
C         KV      - E.M. ACCELERATING VOLTAGE
C
C  DATASTREAMS
C   'INIMAGE'   - INPUT OF IMAGE DATA                    H K AMP PHS IQ RMSBK
C   'INREF'     - INPUT OF REFERENCE PROJECTION DATA     H K FOBS(I) PHASE
C                       ( IF FOBS BUT NO PHASE, SET PHASE = -999.0 )
C   'HKLIN'     - INPUT OF REFERENCE 3D LCF OR MTZ DATA
C   'WILSONCTF' - Output of data for Wilson plot using CURVY
C   'SUMMARY'   - OUTPUT OF RESULTS, for updating of image_info
C
C  PARAMETERS TO BE CALCULATED
C        (A)...PHASE AGREEMENT OVERALL, IF REFERENCE PHASE DATA EXISTS.
C        (B)...CORRELATION COEFF BETWEEN IMAGE AND REFERENCE AMPLITUDES.
C
C  THE FUNCTION MINIMISED WILL BE :-
C         L =  Sum of [(1/SIGMA**2) * (FIM**2-FREF**2*CTF**2)**2]    (IPHASE=0)
C         L =  Sum of [(1/SIGMA**2) * (FIM*COS(DP)-FREF*CTF*)**2]    (IPHASE=1)
C         L =  Sum of [DP]                                           (IPHASE=2)
C                  where DP is the phase difference between obs and ref data,
C                  and SIGMA is the error on FIM**2 and FIM respectively.
C  THIS IS THE SAME AS MAXIMISING      (1/SIGMA**2) * FIM**2 * FREF**2 * CTF**2
C
C               where SIGMA = standard deviation of FIM**2
C
C*******************************************************************************
C
      PARAMETER (NIM=2000)
      PARAMETER (ID=150)
      LOGICAL LIST,LISTW
      CHARACTER DAT*24
      REAL KV
      REAL*8 A(3,3),B(3),W(100),E
      INTEGER*8 NSER
      DIMENSION TITLE(20),TITLEIN(20),GRAD(3),PARAMS(3),PARAMSLIM(3)
      DIMENSION IHIN(NIM),IKIN(NIM),IHTRUE(NIM),IKTRUE(NIM),
     .  AIM(NIM),PIM(NIM),BIM(NIM),CONT(NIM),
     .  AREF(NIM),PREF(NIM),AREFSCALE(NIM),WP(NIM)
      EQUIVALENCE (PARAMS(1),DFMID1),(PARAMS(2),DFMID2),
     .          (PARAMS(3),ANGAST)
CTSH++
      CHARACTER*80 TMPTITLE,TMPTITIN
      EQUIVALENCE (TMPTITLE,TITLE)
      EQUIVALENCE (TMPTITIN,TITLEIN)
CTSH--
      CHARACTER*200 cline
C
      DATA PARAMSLIM/2*500.,0.174533/
      TWOPI = 2.0 * 3.1415926
      FSHIFT = 0.6
C     PHSCNST SET FOR CALCULATION OF Q FACTOR
      PHSCNST=180.0/(3.1415926*7.0)
C
C READ IN PARAMETERS DESCRIBING IMAGE.
C
      WRITE(6,50)
50    FORMAT('0',' CTFSEARCH Version 2.02(19.3.95): Searches the three',
     .' CTF parameters and origin by comparison with reference phases'/)
C
      READ(5,*) LIST,LISTW,QMAX
      READ(5,*) AX,AY,BX,BY,ISIZEX,DSTEP,XMAG
      READ(5,*) DFMID1,DFMID2,ANGAST,CS,KV,PHACON
C
      DFORI1 = DFMID1
      DFORI2 = DFMID2
      ANGORI = ANGAST
      IF(DFMID1.EQ.DFMID2) DFMID2 = DFMID1 + 1.0
C
      READ(5,99)TITLE
      WRITE(6,98)TITLE
99    FORMAT(20A4)
98    FORMAT(///' TITLE FOR PLOT :',20A4)
C
      WRITE(6,101) LIST,LISTW,QMAX,AX,AY,BX,BY,ISIZEX,DSTEP,XMAG
      WRITE(6,102)DFMID1,DFMID2,ANGAST,CS,KV,PHACON
101   FORMAT(/' LIST................................   ',L1/
     .       ' LISTW,QMAX..........................   ',L1,F7.2/
     .       ' LATTICE PARAMETERS AX,AY............',2F10.2/
     .       '                    BX,BY............',2F10.2/
     .       ' SIZE (X) OF DENSITOMETERED ARRAY ....',I7/
     .       ' DENSITOMETERED STEPSIZE(MICRONS)....',F10.2/
     .       ' MAGNIFICATION OF MICROGRAPH.........',F8.0)
102   FORMAT(' UNDERFOCUS 1 .......................',F8.0/
     .       ' UNDERFOCUS 2 .......................',F8.0/
     .       ' DIRECTION FOR UNDERFOCUS 1 .........',F9.1/
     .       ' SPHERICAL ABERRATION (MM) ..........',F10.2/
     .       ' ACCELERATING VOLTAGE (KV) ..........',F8.0/
     .       ' Phase contrast fraction ............',F8.4/)
C
      ANGAST=ANGAST*TWOPI/360.0
      CS=CS*(10.0**7.0)
      KV=KV*1000.0
      WL=12.3/SQRT(KV+KV**2/(10.0**6.0))
C
      WRITE(6,103)WL
103   FORMAT(' WAVELENGTH (ANGSTROMS)',F10.4)
C
      STEPR=DSTEP*(10.0**4.0)/XMAG
C
      THETATR=WL/(STEPR*ISIZEX)
C  THETATR IS DIFFRACTION ANGLE OF POINT (0,1) IN TRANSFORM (IN GRID UNITS)
C
      READ(5,*) ORIGH, ORIGK, TAXA, TANGL, REVHK, ROT180, SGNXCH, ROT90, ICTFREV, IAQP2
      WRITE(6,1005) ORIGH, ORIGK, TAXA, TANGL, REVHK, ROT180, SGNXCH, ROT90, ICTFREV, IAQP2
1005    FORMAT(/,/,' ********** OTHER IMAGE PARAMETERS',/,
     .  '           ORIGH..........',F9.1,/,
     .  '           ORIGK..........',F9.1,/,
     .  '           TAXA...........',F9.2,/,
     .  '           TANGL..........',F9.2,/,
     .  '           REVHK..........',F8.1,/,
     .  '           ROT180.........',F8.1,/,
     .  '           SGNXCH.........',F8.1,/,
     .  '           ROT90..........',F8.1,/,
     .  '           ICTFREV........',I2,/,
     .  '           IAQP2..........',I2)
C
      READ(5,*) RESMIN, RESMAX
      WRITE(6,1007) RESMIN,RESMAX
1007  FORMAT(60('*'),' RESOLUTION LIMITS.....',2F10.2)
C
C
C  READ IN IMAGE DATA 'INIMAGE' -----   H K AMP PHS IQ RMSBK
C
      CALL CCPDPN(1,'INIMAGE','READONLY','F',0,0)
C
      READ(1,96)  NSER,TMPTITIN
96    FORMAT(I10,20A4)
      WRITE(6,97) NSER,TITLEIN
97    FORMAT(' Serial no and title on input file of uncorrected data'/
     .  40X,I10,20A4)
C
      NSPOTS=0
      NREAD=0
C
      IF(LIST)WRITE(6,1002)
C
1002  FORMAT(/' LIST OF SPOTS READ IN FROM RAW IMAGE DATA FILE'/
     .  '   IH  IK     A       P   IQIN  RMSBK')
109   READ(1,*,END=110) IH,IK,AIN,PIN,IQ,RMSBK
CHEN>
        if(ICTFREV.eq.1)PIN=PIN-180.0
        if(PIN.lt.-180.0)PIN=PIN+360.0
CHEN<
        IF(LIST)WRITE(6,1003) IH,IK,AIN,PIN,IQ,RMSBK
1003    FORMAT(1X,2I4,2F8.1,I4,F8.1)
C
        NREAD=NREAD+1
        RESOL=(IH*AX+IK*BX)**2 + (IH*AY+IK*BY)**2
        RESOL=(STEPR*ISIZEX)/SQRT(RESOL)
        IF(RESOL.GT.RESMIN.OR.RESOL.LT.RESMAX) GO TO 109
C
        NSPOTS=NSPOTS+1
        IF(NSPOTS.GT.NIM) GO TO 5000
C
        IHIN(NSPOTS)=IH
        IKIN(NSPOTS)=IK
        IHTRUE(NSPOTS)=IH
        IKTRUE(NSPOTS)=IK
C
        CALL FIDDLE(IHTRUE(NSPOTS),IKTRUE(NSPOTS),Z,REVHK,SGNXCH,ROT180,ROT90)
C
        AIM(NSPOTS)=AIN
        PHSHFT = ORIGH*IHTRUE(NSPOTS)+ORIGK*IKTRUE(NSPOTS)
        PIN=PIN+PHSHFT          ! APPLY INPUT PHASE ORIGIN SHIFT.
        PIM(NSPOTS)=AMOD(PIN,360.0)
        BIM(NSPOTS)=RMSBK
C       WRITE(6,1009)IH,IK,IHTRUE(NSPOTS),IKTRUE(NSPOTS),PIM(NSPOTS)
1009    FORMAT('HK-RAW,HK-TRUE,PHS',4I5,F10.1)
      GO TO 109
110   WRITE(6,1004) NSPOTS,NREAD
1004  FORMAT(//I6,'  RAW IMAGE SPOTS WITHIN RESOLUTION RANGE',
     .  ' -- FROM INIMAGE WHICH CONTAINS A TOTAL OF',I6)
C
C
C-----NOW GET REFERENCE DATA AT SAME POSITIONS AS IMAGE DATA
C
      READ(5,*) ISPGRP,IREF,NCYC,IPHASE,DFSTEP,DFRANGE
      WRITE(6,1006) ISPGRP,IREF,NCYC,IPHASE,DFSTEP,DFRANGE
1006  FORMAT(//120('*')/'  REFERENCE DATA INPUT'/
     .  '                        ISPGRP......',I5/
     .  '                        IREF........',I5/
     .  '                        NCYC........',I5/
     .  '                        IPHASE......',I5/
     .  '                        DFSTEP......',F10.0/
     .  '                        DFRANGE.....',F10.0)
C
C
      CALL GETREFDAT(NSPOTS,IHIN,IKIN,AREF,PREF,TAXA,TANGL,
     .  REVHK,SGNXCH,ROT180,ROT90,ISPGRP,IREF,LIST,IAQP2)
C
C
      ICYC=0
      WRITE(6,749)ICYC,DFMID1,DFMID2,(ANGAST*360.0/TWOPI)
750   ICYC=ICYC+1
      IF (ICYC.GT.NCYC) GO TO 2000
C
      CALL SCALENEW(NSPOTS,IHIN,IKIN,AIM,BIM,AREF,AREFSCALE,
     .  AX,AY,BX,BY,THETATR,
     .  DFMID1,DFMID2,ANGAST,CS,WL,CNTRST,GRAD,PHACON)
C
C-----NOW L.S. REFINEMENT OF DFMID1,DFMID2,ANGAST
C
      DO I=1,3
        B(I)=0.0
        DO J=1,3
          A(I,J)=0.0
        enddo
      enddo
C
      PHSRES=0.0
      NPHS=0
      RFACNUMER=0.0
      RFACDENOM=0.0
      RMSMIN=0.0
      SUMWEIGHT=0.0
      CORNUMER=0.0
      CORDENOM1=0.0
      CORDENOM2=0.0
      NAMP=0
C
      DO 850 IN=1,NSPOTS
        IF(AREF(IN).EQ.-999.) GO TO 850
        SIGI=0.6*AIM(IN)**2+2.0*AIM(IN)*BIM(IN)+BIM(IN)**2
        SIGF=SQRT(0.4*AIM(IN)**2 + BIM(IN)**2)
        IF(AIM(IN).GT.0.0) THEN
          SIGP=(360.0/TWOPI)*SQRT(BIM(IN)**2+0.4*AIM(IN)**2)/AIM(IN)
        ELSE
          SIGP=10000.0
        ENDIF
C
        IF(IPHASE.EQ.0) WGT=1.0/SIGI**2         ! intensity residuals
        IF(IPHASE.EQ.1) WGT=1.0/SIGF**2         ! amplitude residuals
        IF(IPHASE.EQ.2) WGT=1.0/SIGP**2         ! phase weighting
        WP(IN)=WGT
C
        CALL CTFCALC(IHIN(IN),IKIN(IN),AX,AY,BX,BY,THETATR,
     .    DFMID1,DFMID2,ANGAST,CS,WL,CNTRST,GRAD,PHACON)
C
        CONT(IN)=CNTRST
        IF(PREF(IN).NE.-999.) THEN
          DP=PIM(IN)-PREF(IN)
          DELTAP=DP
          IF(CNTRST.LT.0.0) DP=DP+180.0
          DP=AMOD(DP,360.0)
          IF(DP.GT.180.0)DP=DP-360.0
          IF(DP.LT.-180.0)DP=DP+360.0
C
          picphs = PIM(IN)
 794      continue
          if(picphs.gt.180.0)then
            picphs=picphs-360.0
            goto 794
          endif
          if(picphs.lt.-180.0)then
            picphs=picphs+360.0
            goto 794
          endif
          refphs = PREF(IN)
 795      continue
          if(refphs.gt.180.0)then
            refphs=refphs-360.0
            goto 795
          endif
          if(refphs.lt.-180.0)then
            refphs=refphs+360.0
            goto 795
          endif
C
          IF(LIST)WRITE(6,801)IHIN(IN),IKIN(IN),picphs,refphs,CNTRST,DP
801       FORMAT(' H,K=',2I4,', PHSimage,PHSref=',2F9.1,', CNTR,DP =',F9.3,F9.1)
C
          PHSRES=PHSRES+ABS(DP)
          NPHS=NPHS+1
        ENDIF
C
        CORNUMER=CORNUMER+AIM(IN)*AREFSCALE(IN) *
     .    ABS(CNTRST)/BIM(IN)
        CORDENOM1=CORDENOM1+(AIM(IN)/BIM(IN))**2
        CORDENOM2=CORDENOM2+(AREFSCALE(IN)*CNTRST)**2
        RFACNUMER=RFACNUMER+ABS(AIM(IN)-AREFSCALE(IN)*ABS(CNTRST))/BIM(IN)
        RFACDENOM=RFACDENOM+AIM(IN)/BIM(IN)
C
C  Here the function minimised is either the intensity or amplitude residual.
C
        IF(IPHASE.EQ.0)
     .    RMSMIN=RMSMIN+WGT*(AIM(IN)**2-(AREFSCALE(IN)*CNTRST)**2)**2
        IF(IPHASE.EQ.1.AND.PREF(IN).NE.-999.) THEN
          RMSMINHK=WGT*(AIM(IN)*COS(DELTAP)-AREFSCALE(IN)*CNTRST)**2
          RMSMIN=RMSMIN+RMSMINHK
        ENDIF
        IF(IPHASE.EQ.2.AND.PREF(IN).NE.-999.) 
     .    RMSMIN=RMSMIN+WGT*ABS(DP)   ! simple average not rms for phases
        SUMWEIGHT=SUMWEIGHT+WGT
        NAMP=NAMP+1
C
        IF(IPHASE.EQ.0) THEN            ! for intensity only ctf-refinement
          DO I=1,3
            B(I)= B(I) + WGT*AREFSCALE(IN)**2*
     .        (AIM(IN)**2-(AREFSCALE(IN)*CNTRST)**2)*2.0*CNTRST*GRAD(I)
            DO J=1,3
              A(I,J)=A(I,J)+WGT*AREFSCALE(IN)**4*4.0*CNTRST**2*GRAD(I)*GRAD(J)
            enddo
          enddo
        ENDIF
        IF(IPHASE.EQ.1.AND.PREF(IN).NE.-999.) THEN ! to refine with ampl and phase
          DO I=1,3
            B(I)= B(I) + WGT*AREFSCALE(IN)*
     .        (AIM(IN)*COS(DELTAP)-AREFSCALE(IN)*CNTRST)*GRAD(I)
            DO J=1,3
              A(I,J)=A(I,J)+WGT*AREFSCALE(IN)**2*GRAD(I)*GRAD(J)
            enddo
          enddo
        ENDIF
C
850   CONTINUE
C
C--------------------------!!! Here actual CTF-refinement !!!!!!!!!!!!!!!
C
      IF(IPHASE.LE.1) THEN
        IA=3
        N=3
        E=-1.0
C
        CALL MA21AD(A,IA,N,B,W,E)
C
        IF(E.EQ.0.0) GO TO 860
          WRITE(6,861)E
861       FORMAT('  MA21AD FAILED',F10.5)
      ELSE      ! IPHASE.eq.2 here
        FSHIFT=1.0
C
        CALL CTFPHASE(NSPOTS,IHIN,IKIN,WP,AX,AY,BX,BY,THETATR,
     .    DFMID1,DFMID2,ANGAST,CS,WL,PIM,AIM,BIM,PREF,AREFSCALE,B,
     .    DFSTEP,DFRANGE,PHACON)
C
        DFSTEP=DFSTEP*0.8
        DFRANGE=DFRANGE*0.8
      ENDIF
C
860   continue
C
      DO 870 J=1,3
        IF(FSHIFT*ABS(B(J)).GT.PARAMSLIM(J).AND.IPHASE.LE.1) THEN
          IF(B(J).GT.0.0) PARAMS(J)=PARAMS(J)+PARAMSLIM(J)
          IF(B(J).LT.0.0) PARAMS(J)=PARAMS(J)-PARAMSLIM(J)
        ELSE
          PARAMS(J) = FSHIFT*B(J) + PARAMS(J)
        ENDIF
870   CONTINUE
      ANGOUT=(ANGAST*360.0/TWOPI)
      BOUT=B(3)*360.0/TWOPI
      IF(NPHS.NE.0)PHSRES = PHSRES/NPHS
      CORREL=CORNUMER/SQRT(CORDENOM1*CORDENOM2)
      RFAC = RFACNUMER/RFACDENOM
      IF(IPHASE.LE.1) RMSMIN = SQRT(RMSMIN/NAMP)
      IF(IPHASE.EQ.2) RMSMIN = RMSMIN/SUMWEIGHT ! simple average phase residual
      WRITE(6,871) ICYC,(PARAMS(J),J=1,2),ANGOUT,
     .  (B(J),J=1,2),BOUT,PHSRES,NPHS,RFAC,RMSMIN,CORREL,NAMP
871   FORMAT('::',I5,2F8.0,F8.2,2F6.0,F6.2,F7.2,I5,F7.4,F8.4,F8.5,I5)
749   FORMAT('::  REFINEMENT PROGRESS',/,
     .  '::  STARTING VALUES',/,'::',I5,2F8.0,F8.2,/,
     .  '::',84X,'      SF       A       B       C   RFAC  RMSMIN',/,
     .  ':: ICYC  DFMID1  DFMID2  ANGAST SHFT1 SHFT2',
     .  ' SHFT3 PHSRES NPHS  RFAC   RMSMIN',
     .  '  CORREL NAMP')
      IF(ABS(B(1)).GT.7.0) GO TO 750
      IF(ABS(B(2)).GT.7.0) GO TO 750
      IF(ABS(BOUT).GT.0.05) GO TO 750
      IF(IPHASE.EQ.2.AND.DFSTEP.GT.7.0) GO TO 750
      WRITE(6,873)
873   FORMAT(/,'::  REFINEMENT CONVERGED, SHIFTS SMALLER THAN PRESET',
     .  ' PROGRAM LIMITS')
C
C-----REFINEMENT CONVERGED OR END OF FIXED NUMBER OF CYCLES
C
2000  WRITE(6,872) DFMID1,DFMID2,ANGOUT
872   FORMAT(/'  FINAL C.T.F. PARAMETERS ARE'/
     .  '                           DFMID1......',F9.0/
     .  '                           DFMID2......',F9.0/
     .  '                           ANGAST......',F9.2/)
C
      IF(ANGOUT.GT.180.0) ANGOUT = ANGOUT-360.0
      IF(ANGOUT.GT.180.0) ANGOUT = ANGOUT-360.0
      IF(ANGOUT.LT.-180.0) ANGOUT = ANGOUT+360.0
      IF(ANGOUT.LT.-180.0) ANGOUT = ANGOUT+360.0
      OPEN(UNIT=13,FILE='TMP345876.tmp',STATUS='NEW')
      WRITE(cline,'(F9.0,'','',F9.0,'','',F8.3)') DFMID1,DFMID2,ANGOUT
      call inkomma(cline,k)
      write(13,'(A)')cline(1:k)
      CLOSE(13)
C
      DFDIF1 = DFMID1 - DFORI1
      DFDIF2 = DFMID2 - DFORI2
      ANDIF1 = ANGOUT - ANGORI
      OPEN(UNIT=13,FILE='TMP345875.tmp',STATUS='NEW')
      WRITE(13,'(F9.0,'' '',F9.0,'' '',F8.3)') DFDIF1,DFDIF2,ANDIF1
      CLOSE(13)
C
      CALL FDATE(DAT)
      WRITE(6,1502) DAT(5:24)
1502  FORMAT('  Date from fdate ----  ',A20)
CTSH        WRITE(TITLE) DFMID1,DFMID2,ANGOUT,DAT(5:24,1501)
CTSH++
        WRITE(TMPTITLE,1501) DFMID1,DFMID2,ANGOUT,DAT(5:24)
CTSH--
1501  FORMAT(F9.0,F9.0,F9.2,3X,A20,30X)
      CALL CCPDPN(4,'SUMMARY','UNKNOWN','F',0,0)      
      WRITE(4,1503) TITLE
1503  FORMAT(' CTFSEARCH: ',20A4)
      WRITE(4,97) NSER,TITLEIN
      WRITE(4,872) DFMID1,DFMID2,ANGOUT
      CLOSE(UNIT=4)
C
C
C-----DIAGNOSTIC OUTPUT AT END OF RUN.
C
      IF(LISTW)THEN
        OPEN(UNIT=2,FILE='WILSONCTF',STATUS='NEW',FORM='FORMATTED')
        WRITE(2,20001)NSER
20001   FORMAT('T=WILSON PLOT FOR DATA FROM IMAGE NUMBER ',I10)
        WRITE(2,20002)
20002   FORMAT('X=SQRT(H**2+H*K+K**2)')
        WRITE(2,20003)
20003   FORMAT('Y=LOG (RATIO IMAGE:AMP/REF:AMP*CTF)')
        WRITE(2,20004)QMAX
20004   FORMAT('L=Q <',F5.2,'  !*!0!1')
        WRITE(6,11002)QMAX
11002   FORMAT(/' INFORMATION FOR WILSON PLOT; (Q <=',F5.2,')'//)
        END IF
        IF(LIST)WRITE(6,2001)
2001    FORMAT(/' THIS IS WHAT FINALLY HAPPENED TO THE SPOTS'/
     .  '                      REF               CTFCORR'/
     .  '                     SCALED   UNSCALED   REF  ',
     .  '                OBSERVED'/
     .  '             IMAGE ANISOTROPIC  REF   +SCALED ',
     .  '  CALCULATED      IM/ED   OBSERVED'/
     .  '  IH  IK     AMPL     AMPL     AMPL     AMPL  ',
     .  '      CNTRST DP   RATIO   ERROR'/)
        IF(LISTW)THEN
        NSTAT=0
        WRITE(6,12001)
12001   FORMAT(/'                      REF               CTFCORR'/
     .  '                     SCALED   UNSCALED   REF  ',
     .  '                                     RADIUS     IM/     Q',
     .  '  IQ'/
     .  '             IMAGE ANISOTROPIC  REF   +SCALED ',
     .  '  CALCULATED       IM/    OBSERVED           (REF*CTF)'/
     .  '  IH  IK     AMPL     AMPL     AMPL     AMPL  ',
     .  '      CNTRST DP  REFSCLD   ERROR'/)
      END IF
C
      DO 2100 IN=1,NSPOTS
        IF(AREF(IN).EQ.-999.0) GO TO 2100
        IF(PREF(IN).EQ.-999.0) THEN
CTSH                    ICTF=' '
CTSH++
                ICTF=ICHAR(' ')
CTSH--
        ELSE
          DP = AMOD((PREF(IN)-PIM(IN)),360.0)
          IF(ABS(DP).GT.180.0) DP=DP-SIGN(360.0,DP)
          DP = ABS(DP)
CTSH            IF(DP.GE.90.0) ICTF='-'
CTSH            IF(DP.LT.90.0) ICTF='+'
CTSH++
                IF(DP.GE.90.0) ICTF=ICHAR('-')
                IF(DP.LT.90.0) ICTF=ICHAR('+')
CTSH--
        ENDIF           
        ERROR=BIM(IN)/AREFSCALE(IN)
        IF(LIST)WRITE(6,2050) IHIN(IN),IKIN(IN),AIM(IN),AREFSCALE(IN),
     .    AREF(IN),AREFSCALE(IN)*ABS(CONT(IN)),
     .    CONT(IN),ICTF,AIM(IN)/AREFSCALE(IN),ERROR
2050    FORMAT(2I4,4F9.1,5X,F9.4,1X,A1,2F9.4)
        IF(LISTW)THEN
          Q=8.0
          IF(AIM(IN).GT.0.0)Q=(BIM(IN)/AIM(IN))*PHSCNST
          IF(Q.LT.QMAX)THEN
            IQ=1+Q
            NSTAT=NSTAT+1
            RADSQ=(IHIN(IN)**2+IKIN(IN)**2+IHIN(IN)*IKIN(IN))
            RADIUS=SQRT(RADSQ)
            XXX=(AREF(IN)*ABS(CONT(IN)))
            RATIO=0.0
            IF(XXX.GT.0.0)RATIO=AIM(IN)/XXX
            WRITE(6,12050) IHIN(IN),IKIN(IN),AIM(IN),AREFSCALE(IN),
     .        AREF(IN),AREFSCALE(IN)*ABS(CONT(IN)),
     .        CONT(IN),ICTF,AIM(IN)/AREFSCALE(IN),ERROR,RADIUS,RATIO,Q,IQ
12050       FORMAT(2I4,4F9.1,5X,F9.4,1X,A1,2F9.4,F11.4,F9.4,F6.1,I4)
            RATIOLG=LOG(RATIO)
            WRITE(2,12051)RADIUS,RATIOLG
12051       FORMAT(2F10.5)
          END IF
12100     CONTINUE
        END IF
2100  CONTINUE
C
C
      IF(LISTW)WRITE(6,15000)QMAX,NSTAT
15000 FORMAT(//' NUMBER OF SPOTS Q <=',F5.2,' LISTED',I10/)
C
      STOP              ! END OF NORMAL EXECUTION.
C
5000  WRITE(6,5001)NIM
5001  FORMAT(' TOO MANY IMAGE SPOTS FOR PROGRAM DIMS',I6)
C
      STOP
      END
C
C******************************************************************************
      SUBROUTINE CTFCALC(IH,IK,AX,AY,BX,BY,THETATR,
     .  DFMID1,DFMID2,ANGAST,CS,WL,CNTRST,GRAD,PHACON)
      DIMENSION GRAD(3)
C  GRAD(J) are the three gradients of the amplitude ctf wrt each of the three
C  parameters DFMID1,DFMID2 and ANGAST.
      TWOPI = 2.0 * 3.1415926
      X= IH*AX+IK*BX
      Y= IH*AY+IK*BY
      RAD = SQRT(X**2+Y**2)
      ANGLE=RAD*THETATR
      ANGSPT=ATAN2(Y,X)
      C1=TWOPI*ANGLE*ANGLE/(2.0*WL)
      C2=-C1*CS*ANGLE*ANGLE/2.0
      ANGDIF=ANGSPT-ANGAST
      CCOS=COS(2.0*ANGDIF)
      CSIN=SIN(2.0*ANGDIF)
      DF=0.5*(DFMID1+DFMID2+CCOS*(DFMID1-DFMID2))
      CHI=C1*DF+C2
      AMPCON=SQRT(1.0-PHACON*PHACON)
      CNTRST=-SIN(CHI)*PHACON-COS(CHI)*AMPCON
C      WRITE(6,2)CNTRST,CHI,DF,C1,ANGLE,RAD
2     FORMAT(' CNTRST,CHI,DF,C1,ANGLE,RAD',F7.4,F7.2,F8.0,2F10.7,F7.0)
      DCTFDF1=-COS(CHI)*C1*0.5*(1.0+CCOS)
      DCTFDF2=-COS(CHI)*C1*0.5*(1.0-CCOS)
      DCTFDANG=-COS(CHI)*C1*(DFMID1-DFMID2)*CSIN
      GRAD(1)=DCTFDF1
      GRAD(2)=DCTFDF2
      GRAD(3)=DCTFDANG
C
C      WRITE(6,1)IH,IK,AX,AY,BX,BY,THETATR,DFMID1,DFMID2,
C     .         ANGAST,CS,WL,CNTRST,GRAD
1     FORMAT(' IH...DFMID2',2I4,4F7.1,F9.6,2F7.0/
     .  ' ANGAST...GRAD(3)',F6.2,F10.0,F6.3,F6.2,2F8.4,F8.1)
C
      RETURN
      END
C******************************************************************************
      SUBROUTINE GETREFDAT(NSPOTS,IHIN,IKIN,AREF,PREF,TAXA,TANGL,
     .  REVHK,SGNXCH,ROT180,ROT90,ISPGRP,IREF,LIST,IAQP2)
C
C##############################################################################
C  18.8.84 ############  IMPORTANT CHANGE #####################################
C            THE MATRICES IMAT, MAT, IGO HAVE BEEN CHANGED, TOGETHER WITH THE 
C            LREV TEST IN ASYM SO THAT THE CONVENTION IN P4, P3, AND P6 IS
C            FOR THE AXIAL INDICES TO BE H,0 RATHER THAN 0,K.
C            ( THEY ARE THE SAME AS IN EDLCF ).
C
C  PROGRAM MUST NOW BE LINKED USING COMMAND
C         :-   PIMLINK ORIGTILT
C               (INCLUDES LCFLIB,IMLIB,MODLIB,PLOT82, (LIBRARIES) ETC)
C
C##############################################################################
C
C SPACE GROUP MATRICES --- convention for p3, p4 and p6 is H,0 (not 0,K).
C
C----Original Version:
C      DATA ISPEC/7*0,1,3*0,1,4*0,1,4*0,1,3*0,3*1,2*0,3*1,0,-1,3*1,0,1,
C     A 3*1,4*0,1,0,0,4*1,0,5*1,8*0,1,0,1,1,5*0,1,4*0,1,1,0/
C
C----Current Version:
C
      INTEGER*4 ISPEC(5,17)
      DATA ISPEC/
     A  0,0,0,0,0,
     B  0,0,1,0,0,
     C  0,1,0,0,0,
     D  0,1,0,0,0,
     E  0,1,0,0,0,
     F  1,1,1,0,0,
     G  1,1,1,0,-1,
     H  1,1,1,0,1,
     I  1,1,1,0,0,
     J  0,0,1,0,0,
     K  1,1,1,1,0,
     L  1,1,1,1,1,
     M  0,0,0,0,0,
     N  0,0,0,1,0,
     O  1,1,0,0,0,
     P  0,0,1,0,0,
     Q  0,0,1,1,0/
C
      INTEGER*4 IGO(8,17)
      DATA IGO/
     A 5,5,5,5,5,5,5,5,
     B 4,4,5,5,4,4,5,5,
     C 4,5,4,5,4,5,4,5,
     D 4,5,4,5,4,5,4,5,
     E 4,5,4,5,4,5,4,5,
     F 2,4,2,5,2,4,2,5,
     G 2,4,2,5,2,4,2,5,
     H 2,4,2,5,2,4,2,5,
     I 2,4,2,5,2,4,2,5,
     J 3,4,3,5,3,4,3,5,
     K 1,2,1,4,1,2,1,5,
     L 1,2,1,4,1,2,1,5,
     M 4,5,4,5,3,5,3,5,
     N 2,4,2,4,1,5,1,5,
     O 2,4,2,4,1,5,1,5,
     P 3,4,3,5,1,4,1,5,
     Q 2,3,2,4,1,3,1,5/
C
      INTEGER*4 IMAT(5,17)
      DATA IMAT/ 1,1,1,1,1,    1,2,1,1,1,    1,3,1,1,1,
     A           1,4,1,1,1,    1,3,1,1,1,    1,2,1,3,1,
     B           1,2,1,4,1,    1,2,1,6,1,    1,2,1,3,1,
     C           1,2,7,5,1,    1,8,1,2,3,    1,8,1,9,6,
     D           1,10,11,12,1, 1,8,1,10,11,  1,9,1,10,11,
     E           1,2,10,5,11,  1,8,9,10,11/
C
      INTEGER*4 MAT(8,12)
      DATA MAT/   -1,0,0,-1,-1,0,0,-1,      1,0,0,1,-1,0,0,-1,
     A            1,0,0,-1,1,0,0,-1,        1,0,0,-1,1,0,180,-1,
     B            0,1,-1,0,1,0,0,1,        1,0,0,-1,1,180,180,-1,
     C            0,-1,1,0,1,0,0,1,         0,1,1,0,1,0,0,-1,
     D            0,1,1,0,-1,0,0,1,         0,-1,1,1,-1,0,0,-1,
     E            -1,-1,1,0,1,0,0,1,         1,1,-1,0,-1,0,0,-1/
      LOGICAL LREV(17)
      DATA LREV/9*.FALSE.,.TRUE.,2*.FALSE.,.TRUE.,2*.FALSE.,.TRUE.,
     1          .FALSE./
      REAL STANG(17)            ! STANDARD SPACE GROUP ANGLES.
      DATA STANG/2*0.0,10*90.0,5*120.0/
C        ANGLE BETWEEN A AND B FOR ALL SPACEGROUPS EXCEPT P1, P2 IS FIXED.
C
C        THE ABOVE MATRICES ARE USED BY ASYM TO TRANSFORM ALL REFLECTIONS
C           TO THE STANDARD ASYMMETRIC UNIT AND TO PICK OUT THE SPECIAL
C           REFLECTIONS.
C
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
C******************************************************************************
      PARAMETER (NMAXC=201000)
      INTEGER TOTRFL
      PARAMETER (TOTRFL=3000)
      PARAMETER (MAXINDEX=50)
      PARAMETER (NINIT=(2*MAXINDEX+1)**2)
C

C  DIMESNION statement for loop
      INTEGER*4 J
C  DIMENSION STATEMENTS FOR IREF=1 LCF DATA INPUT
C      DIMENSION CELL(6)                        ! also used in MTZ section
      INTEGER*4 IDATAIN(40)
C     .         ,LOOKUP(40)                     ! also used in MTZ section
      INTEGER*4 IHC(NMAXC),IKC(NMAXC),ILC(NMAXC),
     .  ISC(NMAXC),IFCC(NMAXC),IPHC(NMAXC),IFOM(NMAXC),
     .  IBEGIN(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),
     .  IFINISH(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),
     .  IBEGINPH(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),
     .  IFINISHPH(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX)
C     MAXINDEX=50 => (50+50+1)*(50+50+1)=10201
      DATA IBEGIN/10201*-999/,IFINISH/10201*-999/
      DATA IBEGINPH/10201*-999/,IFINISHPH/10201*-999/
C
C----------------------------------------------------------------from here
C  DIMENSION STATEMENTS FOR IREF=2 MTZ DATA INPUT
C     .. parameters for mtz aspects
      PARAMETER (NLOC=40)
      PARAMETER (MCOLS=200)
      PARAMETER (NPAR=200)
C
      DIMENSION CELL(6),RSYMX(4,4,96)
      LOGICAL EOF
      INTEGER*8 ISER
C
C     .. Local Arrays ..
c      REAL ADATAIN(MCOLS),ADATAOUT(MCOLS),DUM(2,MCOLS)
      REAL ADATAIN(MCOLS),DUM(2,MCOLS)
      INTEGER JPOINT(NLOC),LOOKUP(NLOC)
c      CHARACTER OUTTYP(NLOC)*1,LSPRGI(NLOC)*30,LSPRGO(NLOC)*30,
c     +  TITNEW*70,HISNEW(20)*80,CTPRGI(NLOC)*1,DUMMY*10
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
C---- NLPRGI  =  number of input labels
      DATA NLPRGI,LSPRGI/7,'H','K','L','AMP','SIG','PHASE',
     +     'FOM',33*' '/
C---- .. This code signs which input columns are essential (LOOKUP)
      DATA CTPRGI/'H','H','H','F','Q','P','W',33*' '/
      DATA LOOKUP/-1,-1,-1,-1,-1,-1,-1,33*0/
      DATA JPOINT/40*0/
C----------------------------------------------------------------to here
C
C
C  DIMENSION STATEMENTS FOR IREF=0 INPUT OF UNTILTED PROJECTION DATA.
      INTEGER*4 JH(TOTRFL),JK(TOTRFL)
      REAL PHS(TOTRFL),AMP(TOTRFL),ZSTAR(TOTRFL)
C
      DIMENSION IHIN(1),IKIN(1),AREF(1),PREF(1)
      INTEGER      IPTEST
      INTEGER*4    IP1,IP2
      LOGICAL      LSPEC        ! TRUE IF PHASE IS RESTRICTED BY SYMMETRY.
C
      LOGICAL IOK
      DATA JREFL/0/             ! JREFL IS COUNT ON UNTILTED REFERENCE DATA.
      DATA DRAD,RDEG/0.0174532,57.295779/
      DATA IFOMLIMIT/1/
C
CTSH++
      LOGICAL LIST
      CHARACTER*4 INOTE
CTSH--
      SMIN=40000.
      SMAX=0.
C
      ABANG=STANG(ISPGRP)
      WRITE(6,115)ISPGRP
115   FORMAT('  TWO SIDED PLANE GROUP ',I3,//)
      ABANG=180.-ABANG
C
C     READ REFERENCE DATA
C      (FOR IREF.EQ.-1) NO DATA IS READ, PROGRAM SKIPS PAST INPUT & USES AMP=100
C      (FOR IREF.EQ.0) THIS DATA SHOULD BE UNTILTED AND ONLY THE ASYMMETRIC
C           UNIT SHOULD BE INPUT WITH ALL REFLECTIONS ON THE PROPER PHASE
C           ORIGIN.
C      (FOR IREF.EQ.1) THE FIRST DATASET IS A FULLY-FLEDGED LCF FILE.
C      (FOR IREF.EQ.2) INPUT IS AN MTZ FILE.
C
        WRITE(6,125)
      IF(IREF.EQ.-1) GO TO 220          ! No input of reference data
      IF(IREF.EQ.0) THEN
C       CALL DOPEN(2,'INREF','RO','F')
        CALL CCPDPN(2,'INREF','READONLY','F',0,0)
        READ(2,*)ISER
201     IF(LIST)WRITE(6,130)            ! THIS IF IREF.EQ.0
130     FORMAT(//'    UNTILTED REFERENCE IMAGE (NON-LCF)   '///)
        DO 200 I=1,TOTRFL+1
        READ(2,*,END=210)IH,IK,A,P      ! UNTILTED DATA
        IF(IH.GE.900) GO TO 210
        JREFL=JREFL+1
        JH(JREFL)=IH
        JK(JREFL)=IK
        ZSTAR(JREFL)=0.0
        AMP(JREFL)=A
        PHS(JREFL)=P
215     IF(LIST)WRITE(6,145)IH,IK,ZSTAR(JREFL),PHS(JREFL),A
145     FORMAT(2I5,F10.4,2F10.1)
200      CONTINUE
         WRITE(6,148) TOTRFL
148      FORMAT(///'0MORE THAN',I5,' REFLECTIONS FOR THIS IMAGE')
         STOP
210             CLOSE(2)
                WRITE(6,211)JREFL
211             FORMAT(I6,' UNTILTED REFERENCE REFLECTIONS READ IN')
        GO TO 220
      ENDIF
C
207     CONTINUE        ! IREF .EQ. 1
C                         REFERENCE CURVE INPUT TO STORAGE.
C  INPUT OF AMPL & PHASES FROM LCF FILE.
C  USE POINTERS TO INDICATE THE BEGINNING OR END OF EACH LATTICE LINE.
C
C  NOTES:-   THE FORMULA FOR CALCULATION OF PHASE AT AN ARBITRARY ZSTAR
C            POSITION DOES NOT TREAT THE SYMMETRY OF SPACE GROUPS WITH
C            LATTICE LINES FOR WHICH ZSTAR IS ONLY POSITIVE PROPERLY.
C
C       IMAT SHOWS WHICH MATRICES WILL BE USED FROM MAT FOR EACH SPACE GROUP
C       THE FIRST ELEMENT OF EACH IS PASSED TO ASYM FOR LATER USE.
C       THE SAME IS DONE FOR IGO WHICH CONTROLS PROGRAM FLOW IN SET,ASYM
C       AND FOR ISPEC WHICH INDICATES SPECIAL REFLECTIONS.
C
C       ALL PHASES ARE STORED AS VALUES BETWEEN -180.0 AND 180.0 DEGREES
C
C  INPUT OF PHASES OF NATIVE DATA 
C        (TITLE WITH THE PHASES IS CARRIED INVISIBLY IN /LCF/ COMMON BLOCK)
C
      IF(IREF.EQ.1) THEN
                CALL SRLCF1(1,'HKLIN',-26,'H K L S FC SIGFC PHCAL FOM',LOOKUP,
     .                  .TRUE.,NCOL,CELL)
                IF(STANG(ISPGRP).EQ.CELL(6))  GO TO 1105
                WRITE(6,1109) STANG(ISPGRP),CELL(6)
1109            FORMAT(' CONFLICT BETWEEN CELL ANGLES FROM SPACE GROUP'/
     .          ' AND LCF INPUT FILE')
                STOP
1105            ABANG = 180.0 - CELL(6)
                WRITE(6,1113)CELL(1),CELL(2),CELL(3)
1113            FORMAT('  CELL DIMENSIONS READ IN'/
     .          '  A=',F15.2/'  B=',F15.2/'  C=',F15.2)
                MH=LOOKUP(1)
                MK=LOOKUP(2)
                ML=LOOKUP(3)
                MS=LOOKUP(4)
                MFC=LOOKUP(5)
                MSIGFC=LOOKUP(6)
                MPHCAL=LOOKUP(7)
                MFOM=LOOKUP(8)
                NPHASES=0
                NAMPS=0
                N=0
                NREC=0
                WRITE(6,197)
197             FORMAT(' ****  REFERENCE LCF CURVE INPUT BEGINNING  ****')
1101            CALL RLCF1(IDATAIN,*1110,*1110)
                NREC=NREC+1
                ITH=IDATAIN(MH)
                ITK=IDATAIN(MK)
CTSH                    IF(IABS(ITH).GT.MAXINDEX.OR.IABS(ITK).GT.MAXINDEX) THEN
CTSH++
                IF(ABS(ITH).GT.MAXINDEX.OR.ABS(ITK).GT.MAXINDEX) THEN
CTSH--
                  WRITE(6,196) MAXINDEX
196               FORMAT(' LATTICE LINE POINTER ARRAY TOO SMALL, MAXINDEX=',I5)
                  STOP
                ENDIF
C         HERE TEST SIGFC TO BE  > 0    BEFORE ACCEPTING A RECORD FOR INPUT.
C         ALSO HERE TEST  FOM  TO BE  > IFOMLIMIT  BEFORE ACCEPTING CURVE DATA.
                IF(IDATAIN(MSIGFC).EQ.0) GO TO 1101     ! CRITERIA SIGFC.
                NAMPS=NAMPS+1
                IF(IDATAIN(MFOM).GE.IFOMLIMIT) NPHASES=NPHASES+1
                N=NAMPS
                IF(N.GT.NMAXC) GO TO 1150
                IHC(N)=ITH
                IKC(N)=ITK
                ILC(N)=IDATAIN(ML)
                ISC(N)=IDATAIN(MS)
                IF(ISC(N).LT.SMIN) SMIN=ISC(N)  ! SMIN,SMAX NOT USED YET.
                IF(ISC(N).GT.SMAX) SMAX=ISC(N)
                IFCC(N)=IDATAIN(MFC)
                IPHC(N)=IDATAIN(MPHCAL)
                IFOM(N)=IDATAIN(MFOM)
C         CREATE POINTERS TO BEGINNING AND END OF EACH LATTICE LINE.
                IF(N.NE.1) GO TO 1103
                IHOLD=IHC(N)
                IKOLD=IKC(N)
                IBEGIN(IHOLD,IKOLD)=1
1103            IF((IHC(N).EQ.IHOLD).AND.(IKC(N).EQ.IKOLD)) GO TO 1101
                IFINISH(IHOLD,IKOLD)=N-1
                IHOLD=IHC(N)
                IKOLD=IKC(N)
                IBEGIN(IHOLD,IKOLD)=N
                        GO TO 1101
1110            IFINISH(IHOLD,IKOLD)=N
                CALL CRLCF1
      ENDIF
C
      IF(IREF.EQ.2) THEN
        write(*,'('' IREF=2: Opening HKLIN'')')
                CALL CCPFYP
                CALL MTZINI
                CALL LROPEN(1,'HKLIN',3,IERR)
                IF(IERR.NE.0) THEN
                        WRITE (6,11006)IERR
11006                   FORMAT(' ERROR ON INPUT OF MTZ FILE, IERR=',I5)
                        STOP
                ENDIF
                CALL LRCELL(1,CELL)
                CALL LRSYMM(1,NSYMX,RSYMX)
C--------- Find out how many columns and reflections in input file
                CALL LRINFO(1,DUMMY,NCOL,NREF,DUM)
                CALL LKYASN(1,NLPRGI,LSPRGI,CTPRGI,LOOKUP)
C
                IF(ISPGRP.GE.3.AND.STANG(ISPGRP).NE.CELL(6)) THEN
                        WRITE(6,11008) STANG(ISPGRP),CELL(6)
11008                   FORMAT(' Conflict between cell angles from space group',
     $                  ' and mtz input file, STANG, CELL=',2F8.3)
                        STOP
                ENDIF
C
                IF(ABANG.NE.180.0 - CELL(6))THEN
                  write(*,'(/,/,''WARNING: ABANG = '',F12.3,'', CELL(6) = '',F12.3)')ABANG,CELL(6)
                  if(ISPGRP.ne.1)then
                    stop ' Conflict between GAMMA and MTZ cell angle'
                  else
                    CELL(6)=0.0
                    write(*,'(/,/,''continuing in P1'',/,/)')
                  endif
                endif
                N=0
                NAMPS=0
                NPHASES=0
                NREC=0
C
11007           CALL LRREFF(1,RESOL,ADATAIN,EOF)
                IF(EOF) GO TO 11003
                NREC=NREC+1
                IF(ADATAIN(5).EQ.0) GO TO 11007 ! CRITERIA ON SIGF
                N=N+1
                NAMPS=N
                IF(N.GT.NMAXC) GO TO 1150
C
                IHC(N)=ADATAIN(1)
                IKC(N)=ADATAIN(2)
CTSH                      IF(IABS(IHC(N)).GT.MAXINDEX.OR.IABS(IKC(N)).GT.MAXINDEX) THEN
CTSH++
                  IF(ABS(IHC(N)).GT.MAXINDEX.OR.ABS(IKC(N)).GT.MAXINDEX) THEN
CTSH--
                        WRITE(6,196) MAXINDEX
                        STOP
                  ENDIF
                ILC(N)=ADATAIN(3)
                IFCC(N)=ADATAIN(4)
                IPHC(N)=ADATAIN(6)
                IFOM(N)=ADATAIN(7)*100.0        ! compatibility LCF vs MTZ
                IF(IFOM(N).GE.IFOMLIMIT) NPHASES=NPHASES+1
                IF(N.EQ.1) THEN
                        IHOLD=IHC(N)
                        IKOLD=IKC(N)
                        IBEGIN(IHOLD,IKOLD)=1
                ENDIF
                IF(.NOT.((IHC(N).EQ.IHOLD).AND.(IKC(N).EQ.IKOLD))) THEN
                        IFINISH(IHOLD,IKOLD)=N-1
                        IHOLD=IHC(N)
                        IKOLD=IKC(N)
                        IBEGIN(IHOLD,IKOLD)=N
                ENDIF
                GO TO 11007
C
11003           WRITE(6,11004)
11004           FORMAT(' end of mtz input')
                IFINISH(IHOLD,IKOLD)=N
C
                WRITE(6,11005) NPHASES,NREC
11005           FORMAT(I10,' Phases input on stream 1'/
     $                  I10,' total records on stream 1.')
                CALL LRCLOS(1)
      ENDIF
      ASTAR=1.0/(CELL(1)*SIN(DRAD*ABANG))
      BSTAR=1.0/(CELL(2)*SIN(DRAD*ABANG))
      CSTAR=1.0/ CELL(3)
      WSTAR=CSTAR/3.0
      FOMLIMIT=IFOMLIMIT/100.0
      WRITE(6,1100) NAMPS,NPHASES,FOMLIMIT,NREC
1100  FORMAT(I10,' NON-ZERO AMPLITUDES AND',
     .  I10,' PHASES WITH FOM >',F5.2,' READ IN ON STREAM 1'/
     .  I10,' TOTAL RECORDS ON STREAM 1')
C
C-----
C-----CREATE POINTERS TO REGIONS OF LATTICE LINES WITH MEASURED PHASES.
C-----
C
C      IF(LIST)WRITE(6,5401)
5401  FORMAT(' IH,IK,ILC(begin),ILC(finish),IBEGIN,IFINISH,',
     .  ' same for phases')
      DO 5400 IH=-MAXINDEX,MAXINDEX
        DO 5400 IK=-MAXINDEX,MAXINDEX
          IF(IBEGIN(IH,IK).NE.-999.AND.IFINISH(IH,IK).NE.-999) THEN
            DO 5000 J=IBEGIN(IH,IK),IFINISH(IH,IK)
              IF(IFOM(J).GE.IFOMLIMIT) THEN     ! First phase point
                IBEGINPH(IH,IK) = J
                GO TO 5100
              ENDIF
5000        CONTINUE
C           IF(LIST)WRITE(6,'(6I8)')IH,IK,ILC(IBEGIN(IH,IK)),ILC(IFINISH(IH,IK)),
C     .         IBEGIN(IH,IK),  IFINISH(IH,IK)
            GO TO 5400          ! No phases on this lattice line.
5100        DO 5200 K=J+1,IFINISH(IH,IK)
              IF(IFOM(K).LT.IFOMLIMIT) THEN     ! Last phase point
                IF(K.NE.IFINISH(IH,IK).AND.IFOM(K+1).GE.IFOMLIMIT) GO TO 5200
C---------------ABOVE EXCLUDES SINGLE POINTS IN MIDDLE OF LINE WITH IFOM < IFOMLIMIT.
                IFINISHPH(IH,IK) = K-1
                GO TO 5300
              ENDIF
5200        CONTINUE
            IFINISHPH(IH,IK)=IFINISH(IH,IK)     ! Phases all way to end.
5300        continue
C            IF(LIST)WRITE(6,'(8I8)')IH,IK,ILC(IBEGIN(IH,IK)),ILC(IFINISH(IH,IK)),
C     .                         IBEGIN(IH,IK),  IFINISH(IH,IK),
C     .                         IBEGINPH(IH,IK),IFINISHPH(IH,IK)
           ENDIF
5400  CONTINUE
C
C-----
C-----NOW GENERATE REFERENCE AMP AND PHASE FROM DATA READ IN ABOVE.
C-----
C
220    WRITE(6,125)
125   FORMAT(119('*')//)
C  WHEN (IREF.GE.1), NEW FILMS ARE COMPARED ONLY TO REFERENCE DATASET.
C  WHEN (IREF.EQ.0), NEW FILMS ARE COMPARED ONLY TO FIRST DATASET(UNTILTED)
C  WHEN (IREF.EQ.-1), spots are compared to identical amplitudes of 100.
        TAXB=TAXA+ABANG
      WRITE(6,155)TAXA,TAXB,TANGL
155   FORMAT('  A-STAR WAS ',F8.3, ' DEGREE FROM TILTAXIS, B-STAR WAS '
     1 ,F8.3, ' DEGREES FROM TILT AXIS ',/,'  THE TILT ANGLE WAS ',F8.3,
     1 ' DEGREES ')
      IF(LIST)WRITE(6,156)
156   FORMAT(/,/,' REFERENCE DATA CORRESPONDING TO THE OBSERVED IMAGE INPUT'
     1  /' HIN KIN   IP1  IHREF IKREF   ZSTAR      REF       REF     OBS      OBS'/
     2  '          PHASE                         AMP     PHASE',
     2  '      AMP     PHASE       DISAGREES'/
     3  '           REL.')
      STAXA=ASTAR*SIN(DRAD*TAXA)
      STAXB=BSTAR*SIN(DRAD*TAXB)
      TTANGL=TAN(TANGL*DRAD)
      NFINDA=0
      NFINDP=0
      DO 290 IN=1,NSPOTS
        IH = IHIN(IN)
        IK = IKIN(IN)
        IP1=1
        IP2=0
        IF(IREF.GE.1) THEN
            DPERP=IH*STAXA+IK*STAXB
            Z=DPERP*TTANGL
        ELSE
            Z=0.0
        ENDIF
C
C       write(*,'('' H,K,Z,DPERP,TTANGL = '',2I5,3F12.3)')IH,IK,Z,DPERP,TTANGL
C
       CALL FIDDLE(IH,IK,Z,REVHK,SGNXCH,ROT180,ROT90)
C
        CALL ASYM(IH,IK,Z,IP1,IP2,LSPEC,IPTEST,
     1    WSTAR,MAT(1,IMAT(1,ISPGRP)),MAT(1,IMAT(2,ISPGRP)),
     2    MAT(1,IMAT(3,ISPGRP)),MAT(1,IMAT(4,ISPGRP)),
     3    MAT(1,IMAT(5,ISPGRP)),
     4    IGO(1,ISPGRP),ISPEC(1,ISPGRP),LREV(ISPGRP),IAQP2)
C
C       IP1 AND IP2 GENERATE THE RELATIONSHIP BETWEEN PHASES OF REFLECTIONS
C         IN THE UNIQUE ASYMMETRIC UNIT AND THE INPUT REFLECTIONS. THE
C         REFLECTIONS FROM PREVIOUS FILMS WILL BE TRANSFORMED TO LIE IN THE
C         SAME POSITIONS AS THE INPUT REFLECTIONS AND ORIGIN REFINEMENT
C         WILL BE PERFORMED IN P1.
C       LSPEC IS TRUE A REFLECTION IS SPECIAL, HAS ITS PHASE RESTRICTED BY
C         SYMMETRY. IPTEST IS 0 IF THE REFLECTION SHOULD BE REAL AND 90
C         IF IT SHOULD BE IMAGINARY
C
        IF(IREF.EQ.-1) THEN
          AREF(IN) =  100.0
          PREF(IN) = -999.0
          NFINDA =NFINDA+1
        ENDIF
        IF(IREF.EQ.0)THEN
C         THIS SECTION FOR COMPARISON WITH UNTILTED DATA.
          DO 240 JREF=1,JREFL
            IF(IH.NE.JH(JREF)) GO TO 240
            IF(IK.NE.JK(JREF)) GO TO 240
            AREF(IN)=AMP(JREF)
            NFINDA=NFINDA+1
            IF(PHS(JREF).NE.-999.0) THEN
              PREF(IN)=PHS(JREF)*IP1-IP2
              NFINDP=NFINDP+1
            ELSE
              PREF(IN)=-999.0
            ENDIF
            GO TO 241
240         CONTINUE
            AREF(IN)=-999.0             ! No reference input of this spot.
            PREF(IN)=-999.0
241       CONTINUE
C
C---------SPOTS FROM PREVIOUS FILMS ARE COMPARED WITH THE INPUT FILM SPOTS
C---------BY TRANSFORMING THE PREVIOUS FILMS' PHASES WITH IP1 AND IP2
C---------TO CORRESPOND TO REFLECTIONS WITH THE SAME INDICES AS THE
C---------INPUT REFLECTIONS. THE COMPARISON IS THEN DONE IN P1.
C
        ENDIF
C
        IF(IREF.GE.1)THEN
          CALL GETCRVAL(IN,IHIN,IKIN,IH,IK,Z,ILC,IFCC,IPHC,
     .      IBEGINPH,IFINISHPH,IOK,CELL(3),ADUMMY,PHASE,DPDZCU)
          PREF(IN)=PHASE*IP1-IP2
          IF(.NOT.IOK) PREF(IN)=-999.0
          IF(IOK) NFINDP = NFINDP + 1
          CALL GETCRVAMP(IN,IHIN,IKIN,IH,IK,Z,ILC,IFCC,IPHC,
     .      IBEGIN,IFINISH,CELL(3),AREF(IN))
          IF(AREF(IN).NE.-999.0) NFINDA = NFINDA + 1
          INOTE='    '
C---------CHECK THAT ADUMMY IS NEAR IN VALUE TO AREF(IN)-provides diagnostic only.
          ATEST = ABS(ADUMMY-AREF(IN))
          IF(IOK.AND.ATEST.GT.0.15*ADUMMY.AND.ATEST.GT.100.)INOTE='****'
        ENDIF
        refphs = PREF(IN)
 270    continue
        if(refphs.gt.180.0)then
          refphs=refphs-360.0
          goto 270
        endif
        if(refphs.lt.-180.0)then
          refphs=refphs+360.0
          goto 270
        endif
290     IF(LIST)WRITE(6,291)IHIN(IN),IKIN(IN),IP1,IH,IK,Z,
     .                  AREF(IN),refphs,ADUMMY,PHASE,INOTE
291     FORMAT(2I4,I5,2X,2I4,F10.4,4F10.1,5X,A4)
        WRITE(6,292) NSPOTS, NFINDA, NFINDP
292     FORMAT(' TOTAL NUMBER OF REFERENCE AMPS AND PHASES FOUND FOR',
     .  ' COMPARISON WITH',I6,' INPUT SPOTS WAS',2I6//120('*')/) 
      RETURN
1150  WRITE(6,1151)NMAXC
      STOP
1151  FORMAT(' PROGRAM DIMENSIONS TOO SMALL FOR REFERENCE CURVES',I6)
      END
C******************************************************************************
      SUBROUTINE ASYM(IH,IK,Z,IP1,IP2,SPEC,IPTEST,WSTAR,
     1  A1,A2,A3,A4,A5,IGO,ISPEC,LREV,IAQP2)
C
      INTEGER*4 A1(8),A2(8),A3(8),A4(8),A5(8),IGO(8),ISPEC(5)
      INTEGER*4 IP1,IP2
      LOGICAL SPEC,LREV
C
C Space Group p2221 (7):
C==========================
C          7       P2221         H,K,Z>=0          (0,2N,Z)  (0,2N+1,Z)
C                                                  (H,K,0)
C                                                  (H,0,Z)
C
C Asymetric unit for p2221 is
C       H,K,Z >= 0
C       if H=0: K>= 0
C
C IGO = 2,4,2,5,2,4,2,5,
C ISPEC = 1,1,1,0,-1,
C IMAT = 1,2,1,4,1
C LREV = .FALSE.
C
C     CALL ASYM(IH,IK,Z,IP1(IN),IP2(IN),LSPEC(IN),IPTEST(IN),
C    1  WSTAR,MAT(1,IMAT(1,ISPGRP)),MAT(1,IMAT(2,ISPGRP)),
C    2  MAT(1,IMAT(3,ISPGRP)),MAT(1,IMAT(4,ISPGRP)),
C    3  MAT(1,IMAT(5,ISPGRP)),
C    4  IGO(1,ISPGRP),ISPEC(1,ISPGRP),LREV(ISPGRP),IAQP2)
C
C     CALL ASYM(IH,IK,Z,IP1(IN),IP2(IN),LSPEC(IN),IPTEST(IN),
C    1  WSTAR,MAT(1,1),MAT(1,2),
C    2  MAT(1,1),MAT(1,4),
C    3  MAT(1,1),
C    4  IGO(1,ISPGRP),ISPEC(1,ISPGRP),LREV(ISPGRP),IAQP2)
C
C       A1 =  -1,0,0,-1,-1,  0,  0,-1
C       A2 =   1,0,0, 1,-1,  0,  0,-1
C       A3 =  -1,0,0,-1,-1,  0,  0,-1
C       A4 =   1,0,0,-1, 1,  0,180,-1
C       A5 =  -1,0,0,-1,-1,  0,  0,-1
C
C.........IP1 AND IP2 GENERATE THE RELATIONSHIP BETWEEN PHASES OF REFLECTIONS
C.........IN THE UNIQUE ASYMMETRIC UNIT AND THE INPUT REFLECTIONS. THE
C.........REFLECTIONS FROM PREVIOUS FILMS WILL BE TRANSFORMED TO LIE IN THE
C.........SAME POSITIONS AS THE INPUT REFLECTIONS AND ORIGIN REFINEMENT
C.........WILL BE PERFORMED IN P1.
C.........
C.........LSPEC IS TRUE A REFLECTION IS SPECIAL, HAS ITS PHASE RESTRICTED BY
C.........SYMMETRY. IPTEST IS 0 IF THE REFLECTION SHOULD BE REAL AND 90
C.........IF IT SHOULD BE IMAGINARY
C
C
C      WRITE(6,904)A1,A2,A3,A4,A5,IGO,ISPEC,IH,IK,Z,IP1,IP2,SPEC,LREV,IPTEST,WSTAR
C 904  FORMAT(/,'ASYM called with',/,
C     .  ' A1   =',8I5,/,' A2   ='8I5,/,' A3   =',8I5,/,
C     1  ' A4   =',8I5,/,' A5   =',8I5,/,
C     3  ' IGO  =',8I5,/,
C     3  ' ISPEC=',5I5,/,
C     2  50X,'IH,IK,Z      =',2I9,F9.3,/,
C     3  50X,'IP1,IP2      =',2I9,/,
C     3  50X,'SPEC,LREV    =',2L,/,
C     4  50X,'IPTEST,WSTAR =',I9,F9.3)
C
      IF(IH.LT.0) CALL MULT(A1,IH,IK,Z,IP1,IP2)
C
C-----MULT does matrix multiplication as:
C     (H' K' Z' AMP' PHS')=(H K Z AMP PHI) <A>
C        <A> HAS FORM     IA(1)  IA(3)     0      0  IA(6)
C                         IA(2)  IA(4)     0      0  IA(7)
C                             0      0 IA(5)      0      0
C                             0      0     0      1      0
C                             0      0     0      0  IA(8)
C
C-----This effects:
C     H=-H
C     K=-K
C     Z=-Z
C     IP1=IP1
C     IP2=-IP2
C
      PASS=0    ! second pass (22.1.90) to check all changes made correctly.
50    INDEX=1
      IF(IK.GE.0) INDEX=INDEX+1
      IF(Z.GE.0.0) INDEX=INDEX+2
CTSH      IF(IH.LT.IABS(IK)) INDEX=INDEX+4
CTSH++
      IF(IH.LT.ABS(IK)) INDEX=INDEX+4
CTSH--
C
      INDEX=IGO(INDEX)
C
C      WRITE(6,902) INDEX
902   FORMAT (' INDEX = ',I10)
C      WRITE(6,901) IH,IK,Z,IP1,IP2
901   FORMAT(' IH,IK,Z,IP1,IP2 = ',2I5,F10.5,2I5)
      GO TO (100,150,200,250,500), INDEX
C
C     INDEX CLASSIFIES THE REFLECTION BY ITS INDICES
C     IGO INDICATES WHICH MATRIX WILL BRING THE REFLECTION
C        INTO THE UNIQUE ASYMMETRIC UNIT FOR A GIVEN INDEX
C
C    INDEX    K>=0     Z>=0   /K/>=/H/          IGO     GOTO
C      1       NO       NO      NO              = 2     -> 150  => MULT(A4)     H=H,K=-K,Z=Z, IP1=IP1+180, IP2=-IP2
C      2       YES      NO      NO              = 4     -> 250  => MULT(A2)     H=H,K=K,Z=-Z, IP1=IP1,     IP2=-IP2
C      3       NO       YES     NO              = 2     -> 150  => MULT(A4)     H=H,K=-K,Z=Z, IP1=IP1+180, IP2=-IP2
C      4       YES      YES     NO              = 5     -> 500
C      5       NO       NO      YES             = 2     -> 150  => MULT(A4)     H=H,K=-K,Z=Z, IP1=IP1+180, IP2=-IP2
C      6       YES      NO      YES             = 4     -> 250  => MULT(A2)     H=H,K=K,Z=-Z, IP1=IP1,     IP2=-IP2
C      7       NO       YES     YES             = 2     -> 150  => MULT(A4)     H=H,K=-K,Z=Z, IP1=IP1+180, IP2=-IP2
C      8       YES      YES     YES             = 5     -> 500
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
C
C------If H=0, then it should be: K>0: Do H=-H, K=-K, Z=-Z, IP1=IP1, IP2=-IP2:
       IF(IH.EQ.0 .AND. IK.LT.0) CALL MULT(A1,IH,IK,Z,IP1,IP2)
C
C------LREV=T and H=0 : Do H=H, K=-K, Z=Z, IP1=IP1+180, IP2=-IP2
       IF(LREV .AND. IH.EQ.0) CALL MULT(A4,IH,IK,Z,IP1,IP2)

      PASS=PASS+1                       !
      IF(PASS.EQ.1) GO TO 50            ! Check through again once only.
C                                       ! Done 22.1.90 to fix the -ve zstar
C                                       ! obtained for h=0,k=-ve refls in p2.
C
C------Now, reflections should be in the asymetric unit.
C
C       write(6,'('' In asymmetric unit: '',2I6,F9.3,2I6)')IH,IK,Z,IP1,IP2
C
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
C
C-----WSTAR is a parameter (0.5) here and not an epsion ???
      IF(ABS(Z).LT.WSTAR) GO TO 570
530   CONTINUE
      IF(ISPEC(4).LT.1) GO TO 600
C-----IF(IH.EQ.IK) GO TO 570
C-----For A2: P2221 with additional: H=K=even: real, H=K=odd: imaginary
      IF(IH.EQ.IK)then
        if(IAQP2.eq.1)then
          goto 563
        else
          goto 570
        endif
      endif
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
C
600   CONTINUE
C
C      WRITE(6,903)IH,IK,Z,IP1,IP2,SPEC
C 903  FORMAT(' Final situation in ASYM: IH,IK,Z,IP1,IP2,SPEC = ',
C     1  2I5,F10.5,2I10,L)
C
      RETURN
      END
C
C*******************************************************************************
C
      SUBROUTINE ASYMold(IH,IK,Z,IP1,IP2,SPEC,IPTEST,WSTAR,
     1  A1,A2,A3,A4,A5,IGO,ISPEC,LREV)
C
      INTEGER*4 A1(8),A2(8),A3(8),A4(8),A5(8),IGO(8),ISPEC(5)
      INTEGER*4 IP1,IP2
      LOGICAL SPEC,LREV
C
C      WRITE(6,904)A1,A2,A3,A4,A5,IH,IK,Z,IP1,IP2,SPEC,IPTEST,WSTAR
      IF(IH.LT.0) CALL MULT(A1,IH,IK,Z,IP1,IP2)
50    INDEX=1
      IF(IK.GE.0) INDEX=INDEX+1
      IF(Z.GE.0.0) INDEX=INDEX+2
CTSH      IF(IH.LT.IABS(IK)) INDEX=INDEX+4
CTSH++
      IF(IH.LT.ABS(IK)) INDEX=INDEX+4
CTSH--
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
      INTEGER*4 IA(8),IP1,IP2
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
C******************************************************************************
      SUBROUTINE GETCRVAL(ISPOT,IHIN,IKIN,IH,IK,ZASYM,
     .  JLC,IFCC,IPHC,IBEGIN,IFINISH,IOK,C,FREF,PREF,DPDZCU)
        LOGICAL IOK
      PARAMETER (MAXINDEX=50)
      INTEGER IH,IK,IHIN(1),IKIN(1)
      INTEGER*4 JLC(1),IFCC(1),IPHC(1)
      INTEGER*4 IBEGIN(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),
     .  IFINISH(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX)
C
c      REAL*8 A(2,2),B(2),W(20),E
C              THESE BELOW ARE JUST DUMMY VARIABLES FOR ASYM.
c      INTEGER*2 A1(8),A2(8),A3(8),A4(8),A5(8),IGO(8),ISPEC(5)
c      INTEGER*2 IP1,IP2
C
      DATA DRAD,RDEG,PI/0.0174532,57.295779,3.14159/
      DATA BTEMP/80.0/
C
      IOK=.TRUE.
C
      NBEGIN=IBEGIN(IH,IK)
      NFINISH=IFINISH(IH,IK)
C
      IF((NBEGIN.NE.-999).AND.(NFINISH.NE.-999)) GO TO 70
        IOK=.FALSE.
        FREF=-999.
        PREF=-999.
        WRITE(6,1107)IH,IK,IHIN(ISPOT),IKIN(ISPOT)
1107    FORMAT(' LATTICE LINE NOT FOUND',2I5,'   SPOT',2I5)
        RETURN
70    continue
C
      ZBEGIN=JLC(NBEGIN)/C
      ZFINISH=JLC(NFINISH)/C
C
      IF((ZASYM.GE.ZBEGIN).AND.(ZASYM.LE.ZFINISH)) GO TO 80
        IOK=.FALSE.
        FREF=-999.
        PREF=-999.
        WRITE(6,1108)IH,IK,IHIN(ISPOT),IKIN(ISPOT),ZASYM,
     .    ZBEGIN,ZFINISH,JLC(NBEGIN),JLC(NFINISH),NBEGIN,NFINISH
1108    FORMAT(' ZSTAR OUTSIDE RANGE ON LINE',2I5,'   SPOT',2I5,
     .  '        ZSTAR=',F8.4,' RANGE=',2F8.4,' (',2I5,2I8,')')
        RETURN
80    CONTINUE
C
C********************BELOW IS CALCULATION OF PHASE AT EXACT VALUE OF ZSTAR.
C                    IT IS BASED ON THE SUM OF DAMPED SINC FUNCTIONS, WITH
C                    DAMPING SET TO BTEMP=80, AND A SINC FUNCTION WHICH FALLS
C                    TO ZERO AT TWO REFLECTIONS AWAY FROM THE POINT BEING
C                    CALCULATED. THUS THE CALCULATION GIVES DOUBLE THE VALUE
C                    OF F WHICH WOULD BE OBTAINED BY SIMPLE INTERPOLATION.
C                    AFTER DIVIDING THE RESULT BY TWO,THE OUTPUT COLUMN 
C                    FREF IS THEREFORE DIRECTLY COMPARABLE WITH THE INPUT
C                    AMPLITUDES.
C
      CPART=0.0
      SPART=0.0
      DZ=0.0004 !  SET DZ FOR GRADIENT CALC HERE.
      CPARTDZ=0.0
      SPARTDZ=0.0
      DO 85 I=NBEGIN,NFINISH
        ZI=JLC(I)/C
        ZDIFF=ZASYM-ZI
        ZDIFFDZ=ZASYM+DZ-ZI
        IF(ZDIFF.NE.0) GO TO 81
          SINCDAMP=1.0
          GO TO 82
81      continue
          ARGEXP=-0.25*BTEMP*ZDIFF**2
          ARGSINC=0.5*PI*ZDIFF*C
          SINCF=SIN(ARGSINC)/ARGSINC
          SINCDAMP=SINCF*EXP(ARGEXP)
82      CONTINUE
        IF(ZDIFFDZ.NE.0) GO TO 83
          SINCDMPDZ=1.0
          GO TO 84
83      continue
          ARGEXP=-0.25*BTEMP*ZDIFFDZ**2
          ARGSINC=0.5*PI*ZDIFFDZ*C
          SINCF=SIN(ARGSINC)/ARGSINC
          SINCDMPDZ=SINCF*EXP(ARGEXP)
84      CONTINUE
        PHS=IPHC(I)*DRAD
        CPART=CPART+SINCDAMP*COS(PHS)*IFCC(I)
        SPART=SPART+SINCDAMP*SIN(PHS)*IFCC(I)
        CPARTDZ=CPARTDZ+SINCDMPDZ*COS(PHS)*IFCC(I)
        SPARTDZ=SPARTDZ+SINCDMPDZ*SIN(PHS)*IFCC(I)
85    CONTINUE
      FREF=0.5*SQRT(SPART**2+CPART**2)
      PREF=RDEG*ATAN2(SPART,CPART)
      FREFDZ=0.5*SQRT(SPARTDZ**2+CPARTDZ**2)
      PREFDZ=RDEG*ATAN2(SPARTDZ,CPARTDZ)
      PDIFF=PREFDZ-PREF ! MAX 8 DEG IN 0.0004 DZ == 180 DEG IN 0.01 DZ.
      IF(ABS(PDIFF).GT.180.0) PDIFF=PDIFF-SIGN(360.0,PDIFF)
      IF(ABS(PDIFF).GT.8.0) PDIFF=SIGN(8.0,PDIFF)
      DPDZCU = PDIFF/DZ
C
C      WRITE(6,86)IH,IK,ZASYM,FREF,FREFDZ,PREF,PREFDZ,DPDZCU
86    FORMAT('GETCRVAL: H,K,Z,F,F+DZ,P,P+DZ',2I5,F8.4,2F10.2,2F10.3,F15.0)
C
      RETURN
      END
C******************************************************************************
      SUBROUTINE GETCRVAMP(ISPOT,IHIN,IKIN,IH,IK,ZASYM,
     .  JLC,IFCC,IPHC,IBEGIN,IFINISH,C,FREF)
      PARAMETER (MAXINDEX=50)
      INTEGER IH,IK,IHIN(1),IKIN(1)
      INTEGER*4 JLC(1),IFCC(1),IPHC(1)
      INTEGER*4 IBEGIN(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),
     .  IFINISH(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX)
C
c      REAL*8 A(2,2),B(2),W(20),E
C              THESE BELOW ARE JUST DUMMY VARIABLES FOR ASYM.
c      INTEGER*2 A1(8),A2(8),A3(8),A4(8),A5(8),IGO(8),ISPEC(5)
c      INTEGER*2 IP1,IP2
C
      LOGICAL IOK
      DATA DRAD,RDEG,PI/0.0174532,57.295779,3.14159/
      DATA BTEMP/80.0/
        IOK=.TRUE.
        NBEGIN=IBEGIN(IH,IK)
        NFINISH=IFINISH(IH,IK)
        IF((NBEGIN.NE.-999).AND.(NFINISH.NE.-999)) GO TO 70
                FREF=-999.
C               WRITE(6,1107)IH,IK,IHIN(ISPOT),IKIN(ISPOT)
1107            FORMAT(' LATTICE LINE NOT FOUND',2I5,'   SPOT',2I5)
                RETURN
70      ZBEGIN=JLC(NBEGIN)/C
        ZFINISH=JLC(NFINISH)/C
      IF((ZASYM.GE.ZBEGIN).AND.(ZASYM.LE.ZFINISH)) GO TO 80
                FREF=-999.
C               WRITE(6,1108)IH,IK,IHIN(ISPOT),IKIN(ISPOT),ZASYM,
C     .         JLC(NBEGIN),JLC(NFINISH)
1108    FORMAT(' ZSTAR OUTSIDE RANGE ON LINE',2I5,'   SPOT',2I5,
     .  '        ZSTAR=',F8.4,' RANGE=',2I5)
                RETURN
80      CONTINUE
C@@@@@@@@@@@@@@@@@@@@BELOW IS CALCULATION OF AMPLITUDE AT EXACT VALUE OF ZSTAR.
C                    IT IS BASED ON THE SUM OF DAMPED SINC FUNCTIONS, WITH
C                    DAMPING SET TO BTEMP=80, AND A SINC FUNCTION WHICH FALLS
C                    TO ZERO ONE REFLECTION AWAY FROM THE POINT BEING
C                    CALCULATED.  
C                    THE OUTPUT COLUMN FREF IS THEREFORE DIRECTLY COMPARABLE
C                    WITH THE INPUT AMPLITUDES.  AT LATTICE POINTS IT IS
C                    IDENTICAL WITH THE INPUT.
        FREF=0.0
        DO 85 I=NBEGIN,NFINISH
        ZI=JLC(I)/C
        ZDIFF=ZASYM-ZI
      IF(ZDIFF.NE.0) GO TO 81
        SINCDAMP=1.0
        GO TO 82
81      ARGEXP=-0.25*BTEMP*ZDIFF**2
        ARGSINC=PI*ZDIFF*C
        SINCF=SIN(ARGSINC)/ARGSINC
        SINCDAMP=SINCF*EXP(2.0*ARGEXP)
82      CONTINUE
        FREF = FREF + SINCDAMP * (FLOAT(IFCC(I)))**2
85      CONTINUE
      IF(FREF.LE.0.0) FREF=1.0
      FREF=SQRT(FREF)
C      WRITE(6,86)IH,IK,ZASYM,FREF
86    FORMAT(' H,K,Z,F',2I5,F8.4,F10.2)
      RETURN
      END
C******************************************************************************
C  FIDDLING WITH THE INDEXING TO GET CORRECT MATCH TO INDEXING CONVENTION
C  USEFUL IN A NUMBER OF SPACE GROUPS -- SEE WRITE-UP AT TOP OF PROGRAM.
      SUBROUTINE FIDDLE(IH,IK,Z,REVHK,SGNXCH,ROT180,ROT90)
C
      IF(REVHK.EQ.0.0) GO TO 225
        I=IH
        IH=IK
        IK=I
        Z=-Z
 225  CONTINUE 
C
      IF(SGNXCH.EQ.0.0) GO TO 230
        IK=-IK
        Z=-Z
 230  continue
C
      IF(ROT180.EQ.0.0) GO TO 231
       IH=-IH
       IK=-IK
 231  CONTINUE
C
      if(ROT90.ne.0.0)then
        I=IH
        IH=-IK
        IK=I
      endif
C
      RETURN
      END
C***************************************************************************
C   ANISOTROPIC IMAGE SCALING TO ALLOW FOR DIFFERENTIAL LOSS OF SIGNAL
C   IN DIFFERENT IMAGE DIRECTIONS.
C   Minimises Sum of [1/sigma**2]*[Aim - K*Aed]**2
C    where sigma = SQRT(BIM**2 + 0.1*AIM**2)
C
      SUBROUTINE SCALEOLD(NSPOTS,IH,IK,AIM,BIM,AREF,AREFS,
     .  AX,AY,BX,BY,THETATR,
     .  DFMID1,DFMID2,ANGAST,CS,WL,CNTRST,GRAD,PHACON)
      DIMENSION IH(1),IK(1),AIM(1),BIM(1),AREF(1),AREFS(1),GRAD(3)
      DIMENSION SLOPE(4), PSFABC(4)
      REAL*8 A(4,4),B(4),W(40),E
      EQUIVALENCE (PSFABC(1),SF),(PSFABC(2),PA),(PSFABC(3),PB),
     .          (PSFABC(4),PC)
        PSFABC(1)=1.0
        DO 20 I=2,4
20      PSFABC(I)=0.000
        FSHIFT = 0.5
      DO 200 ICYC=1,25
                DO 50 I=1,4
                B(I)=0.0
                DO 50 J=1,4
50              A(I,J)=0.0
        NAMP=0
        RFACNUMER=0.0
        RFACDENOM=0.0
        RMSMIN=0.0
      DO 100 IN=1,NSPOTS
      IF(AREF(IN).EQ.-999.0) GO TO 100
      SIGMASQ = BIM(IN)**2
      WGT=1.0/SIGMASQ
C  This gives equal (high) weight to reflections with IQ values better than 2.5.
C
      CALL CTFCALC(IH(IN),IK(IN),AX,AY,BX,BY,THETATR,
     .  DFMID1,DFMID2,ANGAST,CS,WL,CNTRST,GRAD,PHACON)
C
      SLOPE(1) = AREF(IN) * ABS(CNTRST) *
     .           EXP(-PA*IH(IN)**2-PB*IK(IN)**2-PC*IH(IN)*IK(IN))
      SLOPE(2) = -SLOPE(1)*SF*IH(IN)**2
      SLOPE(3) = -SLOPE(1)*SF*IK(IN)**2
      SLOPE(4) = -SLOPE(1)*SF*IH(IN)*IK(IN)
      AREFS(IN) = SLOPE(1)*SF
C      WRITE(6,51) AREFS(IN),CNTRST,(SLOPE(J),J=1,4)
51      FORMAT(' AREFS,CNTRST,SLOPE1-4',F10.4,F8.3,4F15.6)
      NAMP=NAMP+1
      RFACNUMER=RFACNUMER+ABS(AIM(IN)-AREFS(IN))/BIM(IN)
      RFACDENOM=RFACDENOM+AIM(IN)/BIM(IN)
      RMSMIN=RMSMIN+WGT*(AIM(IN)-AREFS(IN))**2
        DO 70 I=1,4
        B(I)=B(I)+(AIM(IN)-AREFS(IN))*SLOPE(I)*WGT
        DO 70 J=1,4
70      A(I,J)=A(I,J)+SLOPE(I)*SLOPE(J)*WGT
100   CONTINUE
C      WRITE(6,101) ((A(I,J),J=1,4),B(I),I=1,4)
101     FORMAT(4F15.4,F25.4)
        IA=4
        N=4
        E=-1.0
        CALL MA21AD(A,IA,N,B,W,E)
        IF(E.EQ.0.0) GO TO 75
        WRITE(6,76) E
76      FORMAT('  MA21AD FAILED IN SUBROUTINE SCALE, E=',F10.5)
        STOP
75    DO 80 J=1,4
80    PSFABC(J)=PSFABC(J)+FSHIFT*B(J)
      RMSMIN=SQRT(RMSMIN/NAMP)
      RFACTOR=RFACNUMER/RFACDENOM
200   CONTINUE
C
C   FINAL SCALING WITHOUT USE OF C.T.F. FACTOR READY FOR C.T.F. REFINEMENT.
C
      WRITE(6,81)SF,PA,PB,PC,RFACTOR,RMSMIN
81    FORMAT(84X,F10.3,3F8.5,F7.4,F7.3)
      DO 300 IN=1,NSPOTS
      FADING=EXP(-PA*IH(IN)**2-PB*IK(IN)**2-PC*IH(IN)*IK(IN))
300   AREFS(IN) = SF * FADING * AREF(IN)
      RETURN
      END
C*****************************************************************************
C   ANISOTROPIC IMAGE SCALING TO ALLOW FOR DIFFERENTIAL LOSS OF SIGNAL
C   IN DIFFERENT IMAGE DIRECTIONS. --- NEW TYPE OF SCALING -- 11.6.85
C   Minimises Sum of [1/sigma**2]*[Aim/C - Aed*C]**2
C    where C = sqrt(K) --- K is conventional scale factor.
C    where sigma = SQRT(BIM**2 + 0.1*AIM**2)
C
      SUBROUTINE SCALENEW(NSPOTS,IH,IK,AIM,BIM,AREF,AREFS,
     .  AX,AY,BX,BY,THETATR,
     .  DFMID1,DFMID2,ANGAST,CS,WL,CNTRST,GRAD)
      DIMENSION IH(1),IK(1),AIM(1),BIM(1),AREF(1),AREFS(1),GRAD(3)
      DIMENSION SLOPE(4), PSFABC(4)
      REAL*8 A(4,4),B(4),W(40),E
      EQUIVALENCE (PSFABC(1),SF),(PSFABC(2),PA),(PSFABC(3),PB),
     .          (PSFABC(4),PC)
        PSFABC(1)=1.0
        DO 20 I=2,4
20      PSFABC(I)=0.000
        FSHIFT = 0.5
      DO 200 ICYC=1,25
                DO 50 I=1,4
                B(I)=0.0
                DO 50 J=1,4
50              A(I,J)=0.0
        NAMP=0
        RFACNUMER=0.0
        RFACDENOM=0.0
        RMSMIN=0.0
      DO 100 IN=1,NSPOTS
      IF(AREF(IN).EQ.-999.0) GO TO 100
      SIGMASQ = BIM(IN)**2
      WGT=1.0/SIGMASQ
        FADING=EXP(-PA*IH(IN)**2-PB*IK(IN)**2-PC*IH(IN)*IK(IN))
        C0SQ=SF*FADING
        C0=SQRT(C0SQ)
C
      CALL CTFCALC(IH(IN),IK(IN),AX,AY,BX,BY,THETATR,
     .  DFMID1,DFMID2,ANGAST,CS,WL,CNTRST,GRAD,PHACON)
C
      SLOPE(1) =(AREF(IN)*ABS(CNTRST)+AIM(IN)/C0SQ)*FADING*0.5/C0
      SLOPE(2) = -SLOPE(1)*SF*IH(IN)**2
      SLOPE(3) = -SLOPE(1)*SF*IK(IN)**2
      SLOPE(4) = -SLOPE(1)*SF*IH(IN)*IK(IN)
      AREFS(IN) = AREF(IN)*ABS(CNTRST)*C0SQ
C      WRITE(6,51) AREFS(IN),CNTRST,(SLOPE(J),J=1,4)
51      FORMAT(' AREFS,CNTRST,SLOPE1-4',F10.4,F8.3,4F15.6)
      NAMP=NAMP+1
      RFACNUMER=RFACNUMER+ABS(AIM(IN)-AREFS(IN))/BIM(IN)
      RFACDENOM=RFACDENOM+AIM(IN)/BIM(IN)
      RMSMIN=RMSMIN+WGT*(AIM(IN)-AREFS(IN))**2/C0SQ
        DO 70 I=1,4
        B(I)=B(I)+(AIM(IN)-AREFS(IN))*SLOPE(I)*WGT/C0
        DO 70 J=1,4
70      A(I,J)=A(I,J)+SLOPE(I)*SLOPE(J)*WGT
100   CONTINUE
C      WRITE(6,101) ((A(I,J),J=1,4),B(I),I=1,4)
101     FORMAT(4F15.4,F25.4)
        IA=4
        N=4
        E=-1.0
        CALL MA21AD(A,IA,N,B,W,E)
        IF(E.EQ.0.0) GO TO 75
        WRITE(6,76) E
76      FORMAT('  MA21AD FAILED IN SUBROUTINE SCALE, E=',F10.5)
        STOP
75    DO 80 J=1,4
80    PSFABC(J)=PSFABC(J)+FSHIFT*B(J)
      RMSMIN=SQRT(RMSMIN/NAMP)
      RFACTOR=RFACNUMER/RFACDENOM
200   CONTINUE
C
C   FINAL SCALING WITHOUT USE OF C.T.F. FACTOR READY FOR C.T.F. REFINEMENT.
C
      WRITE(6,81)SF,PA,PB,PC,RFACTOR,RMSMIN
81    FORMAT(84X,F10.3,3F8.5,F7.4,F7.3)
      DO 300 IN=1,NSPOTS
      FADING=EXP(-PA*IH(IN)**2-PB*IK(IN)**2-PC*IH(IN)*IK(IN))
300   AREFS(IN) = SF * FADING * AREF(IN)
      RETURN
      END
C*****************************************************************************
      SUBROUTINE CTFPHASE(NSPOTS,IHIN,IKIN,WP,AX,AY,BX,BY,THETATR,
     .          DFMID1,DFMID2,ANGAST,CS,WL,PIM,AIM,BIM,PREF,AREFSCALE,B,
     .          DFSTEP,DFRANGE,PHACON)
      REAL*8 B(3)       ! compatibility with old program
      DIMENSION IHIN(1),IKIN(1),WP(1),PIM(1),AIM(1),BIM(1),PREF(1),
     .          AREFSCALE(1),GRAD(3)
      TWOPI = 2.0 * 3.14159265437
C
C   simple search of CTF space in steps of DFSTEP, with range DFRANGE
C   corresponding angular step is worked out from astigmatism in each case
C
      NS=(0.5*DFRANGE/DFSTEP)+0.5       ! ensures the existing defocus 
      NS=NS*2                           !  is one of the values tested
      DFRANGE=DFSTEP*NS
      DFMIN1=DFMID1-DFRANGE/2.0
      DFMIN2=DFMID2-DFRANGE/2.0
      PHSRESMIN=100.0
      DO I1=1,NS
        DO I2=1,NS
          DF1=DFMIN1 + (I1-1)*DFSTEP
          DF2=DFMIN2 + (I2-1)*DFSTEP
          ASTIG=ABS(DF1-DF2)
          ANGSTEP=90.0*DFSTEP/AMAX1(ASTIG,1.0)
          ANGRANGE=180.0
          IF(DFRANGE.LT.ASTIG)ANGRANGE=180.0*DFRANGE/ASTIG
          NA=0.5*(ANGRANGE/ANGSTEP)
          NA=NA*2+1             ! ensures odd number of steps which keeps
          NA=MAX0(1,NA)         ! current ANGAST as one of values tested
          DO IANG=1,NA
            ANGA=ANGAST + (FLOAT(IANG-1)-(FLOAT(NA-1)/2.0))*ANGSTEP*TWOPI/360.0
            PHSRES=0.
            NPHS=0
            WSUM=0.
            DO IN=1,NSPOTS
              IF(PREF(IN).NE.-999.) THEN
                CALL CTFCALC(IHIN(IN),IKIN(IN),AX,AY,BX,BY,THETATR,
     .                  DF1,DF2,ANGA,CS,WL,CNTRST,GRAD,PHACON)
                DP=PIM(IN)-PREF(IN)
                IF(CNTRST.LT.0.0) DP=DP+180.0
                DP=AMOD(DP,360.0)
                IF(DP.GT.180.0)DP=DP-360.0
                IF(DP.LT.-180.0)DP=DP+360.0
                PHSRES=PHSRES+ABS(DP)*WP(IN)
                NPHS=NPHS+1
                WSUM=WSUM+WP(IN)
              ENDIF
            enddo
            IF(NPHS.NE.0) PHSRES=PHSRES/WSUM
            IF(PHSRES.LT.PHSRESMIN) THEN
              DFBEST1=DF1
              DFBEST2=DF2
              ANGBEST=ANGA
              PHSRESMIN=PHSRES
            ENDIF
          enddo
        enddo
      enddo 
      B(1)=DFBEST1-DFMID1
      B(2)=DFBEST2-DFMID2
      B(3)=ANGBEST-ANGAST
      RETURN
      END
C
c==========================================================
c
      SUBROUTINE SHORTSHRINK(czeile,k)
C
C counts the number of actual characters not ' ' in czeile
C and gives the result out in k.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1
      CHARACTER * 1 CTMP2
      CHARACTER * 200 CZEIL2
      CTMP2=' '
C
C-----find the leading spaces and remove them
C
      ilen=len(czeile)
      k=ilen
C
      DO 90 J=1,k
         READ(CZEILE(J:J),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)THEN
           GOTO 95
         ENDIF
 90   CONTINUE
 95   CONTINUE
C
      WRITE(CZEIL2(1:k),'(A)')CZEILE(J:k)
      WRITE(CZEILE(1:k),'(A)')CZEIL2(1:k)
C
      DO 100 I=1,ilen
         k=ilen+1-I
         READ(CZEILE(k:k),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)GOTO 300
  100 CONTINUE
  300 CONTINUE
      IF(k.LT.1)k=1
C
      RETURN
      END
C
C
c==========================================================
c
      SUBROUTINE INKOMMA(CZEILE,k)
C
C replaces intermedieate spaces within the actual text string
C in CZEILE up to the length k by komma.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1,CTMP2,CTMP3
      CHARACTER * 200 CZEIL2
      CTMP2=' '
      CTMP3=','
C
      ilen=len(czeile)
      DO 70 I=1,ilen
         k=ilen+1-I
         READ(CZEILE(k:k),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)GOTO 80
  70  CONTINUE
  80  CONTINUE
      IF(k.LT.1)k=1
C
C-----find the leading spaces and remove them
C
      DO 90 J=1,k
         READ(CZEILE(J:J),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)THEN
           GOTO 95
         ENDIF
 90   CONTINUE
 95   CONTINUE
C
      WRITE(CZEIL2(1:k),'(A)') CZEILE(J:k)
C
C     WRITE(*,'('' INKOMMA 2: k='',I4,'' J='',I4,'':'',A)')
C    1      k,j,CZEIL2(1:k)
C
C-----Was there a komma recently ?
      KWAR=0
C
      I=1
      L=1
 100  continue
        READ(CZEIL2(I:I),'(A1)')CTMP1
        IF(CTMP1.EQ.CTMP3)THEN
C---------There is a komma.
          IF(KWAR.EQ.0)THEN
C-----------There is a komma, before was no komma. Insert one.
            WRITE(CZEILE(L:L),'('','')')
            KWAR=1
          ELSE
C-----------There is a komma, before was already a komma. Shrink.
            L=L-1
          ENDIF
        ELSEIF(CTMP1.EQ.CTMP2)THEN
C---------There is a space.
          IF(KWAR.EQ.0)THEN
C-----------There is a space, before was no komma. Insert komma.
            WRITE(CZEILE(L:L),'('','')')
            KWAR=1
          ELSE
C-----------There is a space, before was a komma. Shrink.
            L=L-1
          ENDIF
        ELSE
C---------There is no komma, no space. Anulate KWAR, do nothing.
          WRITE(CZEILE(L:L),'(A1)')CZEIL2(I:I)
          KWAR=0
        ENDIF
        I=I+1
        L=L+1
C       WRITE(*,'('' INKOMMA 2b: I='',I4,'' L='',I3,'':'',A)')I,L,CTMP1
C
      IF(I.LE.K)GOTO 100
C
C     WRITE(*,'('' INKOMMA 3: k='',I4,'':'',A)')k,CZEILE(1:k)
C
      IF(L.LE.K)THEN
        WRITE(CZEILE(L:K),'('' '')')
      ENDIF
C
C-----Now check, if the last sign is a komma. If so, remove it.
C
      K=L
      I=K
 200  continue
        READ(CZEILE(I:I),'(A1)')CTMP1
        IF(CTMP1.NE.CTMP2)GOTO 220
        I=I-1
      IF(I.GE.1)GOTO 200
 220  CONTINUE
C
      K=I
C
      IF(CTMP1.EQ.CTMP3)then
        WRITE(CZEILE(I:I),'('' '')')
        K=K-1
      endif
C
      if(k.lt.1)k=1
C
C      WRITE(*,'('':: INKOMMA 4: k='',I4,'': "'',A,''"'')')k,CZEILE(1:k)
C
      RETURN
      END

