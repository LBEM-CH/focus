C       PREPARE LATLINE OUTPUT DATA FOR MKLCF
C       VX 1.0          18.11.88        RH
C       VX 1.1          22.09.93        RH
C       V  2.0          29.10.12        HST
C
C       CONTROL CARDS
C
C       1.   RESOLUTION,REDUCAC (*)  resolution in Angstroms
C                                    factor to reduce estim. phase accuracy.
C       2.   a,b,gamma,c        (*)  cell dimensions
C
C       3.   SCALE              (*)  scale factor to ensure data >1 and <32000
C                                       for LCF file.
C
C       4.   ifomcalc           (*)  switch, deciding if FOM should be calculated from
C                                    0: FOM=100*cos(PHASE_ERROR),   or
C                                    1: FOM=100*cos(PHASE_ERROR)*BESSEL_Ratio(SNR)
C                                       only if FOM=100 and PHASE=0,180,or 90
C
      character*300 czeile
      character*200 cline
      REAL*8 XARG,S18AEF,S18AFF
C
      DEGTORAD=3.1415962/180.0
      NSIGZERO=0
      NAZERO=0
      NFOMLOW=0
      NRESHI=0
      NGOOD=0
      NGFOM1=0
      NGFOM50=0
      NIN=0
      WRITE(6,51)
51    FORMAT('   2DX_PREPMKLCF VX 1.1(22.9.93) -',
     . ' prepares LATLINE data for MKLCF, with automatic scaling')
      READ(5,*)RESOLUTION,REDUCAC
      READ(5,*)CELLA,CELLB,GAMMA,CAXIS
      READ(5,*)SCALE
CHEN>
C      IF(SCALE.EQ.0.0) SCALE=1.0
      READ(5,*)ifomcalc
      if(ifomcalc.eq.0)then
        write(cline,'(''SIGP'')')
      else
        write(cline,'(''SIGP & SIGF'')')
      endif
      call shorten(cline,k)
CHEN<
      WRITE(6,50)REDUCAC,CELLA,CELLB,GAMMA,CAXIS,RESOLUTION,SCALE,cline(1:k)
50    FORMAT('            Reduce phase accuracy by factor ===',F6.2,/,
     .       '            a-axis cell dimension =============',F6.2,/,
     .       '            b-axis cell dimension =============',F6.2,/,
     .       '            gamma angle =======================',F6.2,/,
     .       '            c-axis cell dimension =============',F6.2,/,
     .       '            Resolution ========================',F6.2,/,
     .       '            Amplitudes scaled by ==============',F6.2,/,
     .       '            Calculate FOM from ================',A)
      IF(GAMMA.GT.90.0)GAMMA=180.0-GAMMA
      ASTAR=1.0/(CELLA*SIN(GAMMA*DEGTORAD))
      BSTAR=1.0/(CELLB*SIN(GAMMA*DEGTORAD))
      CSTAR=1.0/CAXIS
      CALL CCPDPN(1,'IN','READONLY','F',0,0)
      CALL CCPDPN(2,'OUT','UNKNOWN','F',0,0)
      CALL CCPDPN(3,'REFHKL','UNKNOWN','F',0,0)
C      CALL DOPEN(1,'IN','RO','F')
C      CALL DOPEN(2,'OUT','NEW','F')
CHEN>
54    FORMAT(3I6,G16.8,G16.8,G16.8,G16.8)
55    FORMAT(3I6,G16.8,G16.8,G16.8)
59    FORMAT('Used:    ',3I6,X,   F13.3,F7.1,F12.3,F9.1,F9.1)
60    FORMAT(/,'              H     K     L         A        P     ',
     .    '   FOM        SIGA     SIGP        RES')
61    FORMAT('IGNORED: ',2I6,F7.3,F13.3,F7.1,12X,  F9.1,F9.1,A)
C
      if(SCALE.EQ.0.0)then
        AMAX=0.0
 70     continue
          READ(1,*,END=80)IH,IK,ZSTAR,A,P,SIGA,SIGP,FOM
          if(A.gt.AMAX)AMAX=A
        goto 70
 80     continue
        rewind(1)
        if(AMAX.gt.0.0001)then
          SCALE=32000.0/AMAX
        else
          SCALE=1.0
        endif
        write(6,'(/,'':: SCALE corrected to '',F12.6,/)')SCALE
        write(6,'(/,''SCALE corrected:'',/)')
        WRITE(6,50)REDUCAC,CELLA,CELLB,GAMMA,CAXIS,RESOLUTION,SCALE
        write(6,'(/)')
      endif
C
CHEN<
      HOLD = -9999
100   continue
        read(1,'(A)',END=400)czeile
        call shorten(czeile,k)
        READ(czeile,*,ERR=120)IH,IK,ZSTAR,A,P,SIGA,SIGP,FOM
        goto 130
120     continue
          write(6,'(''::ERROR during file read in prephklcf.'')')
          write(6,'(''::Last read line:'')')
          write(6,'(''::'',A)')czeile(1:k)
          write(6,'(''::Experted format: 2 integer, 6 floats'')')
          write(6,'(''::ABORTING.'')')
          stop -1
130     continue
        if(HOLD.ne.IH)then
          WRITE(6,60)
          HOLD=IH
        endif
C 
        A=A*SCALE       ! too big for LCF file
CHEN>
        if(SIGA.lt.99998.0 .and. SIGA.gt.-99998.0)then
          SIGA=SIGA*SCALE       ! too big for LCF file
        endif
CHEN<
        NIN=NIN+1
        IF(SIGA.EQ.0.0.OR.SIGP.EQ.0.0) THEN
          NSIGZERO=NSIGZERO+1
          write(cline,'('' (SIG is zero)'')')
          call shorten(cline,k)
          WRITE(6,61)IH,IK,ZSTAR,A,P,SIGA,SIGP,cline(1:k)
          GO TO 100
        ENDIF
        IF(A.LE.0.0) THEN
          NAZERO=NAZERO+1
          write(cline,'('' (A is below zero)'')')
          call shorten(cline,k)
          WRITE(6,61)IH,IK,ZSTAR,A,P,SIGA,SIGP,cline(1:k)
          GO TO 100
        ENDIF
C
        SIGPR=SIGP*REDUCAC
        IF(SIGPR.GT.90.0)SIGPR=90.0
C
        FOMCALC=100.0*COS(SIGPR*DEGTORAD)
C
        IF(FOMCALC.LT.1.0) THEN
          NFOMLOW=NFOMLOW+1
          RARG=XARG
          write(cline,'('' (FOMCALC<1.0)   XARG,IFAIL,JFAIL,'',
     .      ''R1,R2,SIGMA,FOMAMP,FOMCALC: '',
     .      F12.3,2I2,5G15.3)') RARG,IFAIL,JFAIL,R1,R2,SIGMA,FOMAMP,FOMCALC
          call shorten(cline,k)
          WRITE(6,61)IH,IK,ZSTAR,A,P,SIGA,SIGP,cline(1:k)

          GO TO 100       ! skip this one
        ENDIF
        IL=NINT(ZSTAR*CAXIS)
        RSQ=IH**2*ASTAR**2+IK**2*BSTAR**2+ZSTAR**2+
     .          2.0*IH*IK*ASTAR*BSTAR*COS(GAMMA*DEGTORAD)
        IF(RSQ.GT.1.0/RESOLUTION**2) THEN
          NRESHI=NRESHI+1
          RES=SQRT(1.0/RSQ)
          write(cline,'('' (Resolution is '',F7.3,'')'')')RES
          call shorten(cline,k)
          WRITE(6,61)IH,IK,ZSTAR,A,P,SIGA,SIGP,cline(1:k)
          GO TO 100
        ENDIF
        NGOOD=NGOOD+1
        if(FOMCALC.ge.1.0)then
            NGFOM1=NGFOM1+1
        endif
        if(FOMCALC.ge.50.0)then
          NGFOM50=NGFOM50+1
        endif
        WRITE(2,55)IH,IK,IL,A,P,FOMCALC
        WRITE(3,54)IH,IK,IL,A,P,FOMCALC,SIGA
        WRITE(6,59)IH,IK,IL,A,P,FOMCALC,SIGA,SIGP
      GO TO 100
C
CHEN>
C-----Calculate the number of possible phases
CHEN<
C
400   WRITE(6,56)NGOOD,NIN,NSIGZERO,NAZERO,NFOMLOW,NRESHI,RESOLUTION,NGFOM1,NGFOM50
56    FORMAT(': MKLCF FILE COMPLETED',/,/,
     .  ':',I10,' good phases written out',
     .  ', from a total read in of',I10,/,
     .  ':',I10,' of these had zero sigma(s)',/,
     .  ':',I10,' of them had zero amplitude',/,
     .  ':',I10,' of them had figure of merit <0.01, and',/,
     .  ':',I10,' were beyond resolution limit of',F8.1,/,
     .  ':',I10,' of them had FOM>1%',/,
     .  ':',I10,' of them had FOM>50%',/)
CHEN>
      call system("\rm -f 2dx_prepmklcf.statistics")
      open(27,FILE="2dx_prepmklcf.statistics",STATUS="NEW",ERR=404)
      write(27,'(''Number of good phases = '',I10)')NGOOD
      write(27,'(''Number of phases with FOM over 1% = '',I10)')NGFOM1
      write(27,'(''Number of phases with FOM over 50% = '',I10)')NGFOM50
      close(27)
      goto 405
404   continue
        write(*,'(''::ERROR on file open of 2dx_prepmklcf.statistics'')')
405   continue
CHEN<
      STOP
      END
C
C
c==========================================================
c
      SUBROUTINE shorten(czeile,k)
C
C counts the number of actual characters not ' ' in czeile
C and gives the result out in k.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1
      CHARACTER * 1 CTMP2
      CTMP2=' '
C
      ilen=len(czeile)
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

