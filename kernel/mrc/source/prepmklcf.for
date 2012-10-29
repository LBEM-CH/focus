C       PREPARE LATLINE OUTPUT DATA FOR MKLCF
C       VX 1.0          18.11.88        RH
C       VX 1.1          22.09.93        RH
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
      character*300 czeile
      DEGTORAD=3.1415962/180.0
      NSIGZERO=0
      NAZERO=0
      NFOMLOW=0
      NRESHI=0
      NGOOD=0
      NIN=0
      WRITE(6,51)
51    FORMAT('   PREPMKLCF VX 1.1(22.9.93) -',
     . ' prepares LATLINE data for MKLCF')
      READ(5,*)RESOLUTION,REDUCAC
      READ(5,*)CELLA,CELLB,GAMMA,CAXIS
      READ(5,*)SCALE
      IF(SCALE.EQ.0.0) SCALE=1.0
      WRITE(6,50)REDUCAC,CELLA,CELLB,GAMMA,CAXIS,RESOLUTION,SCALE
50    FORMAT('            Reduce phase accuracy by factor ===',F6.2,/,
     .       '            a-axis cell dimension =============',F6.2,/,
     .       '            b-axis cell dimension =============',F6.2,/,
     .       '            gamma angle =======================',F6.2,/,
     .       '            c-axis cell dimension =============',F6.2,/,
     .       '            Resolution ========================',F6.2,/,
     .       '            Amplitudes scaled by===============',F6.2)
      IF(GAMMA.GT.90.0)GAMMA=180.0-GAMMA
      ASTAR=1.0/(CELLA*SIN(GAMMA*DEGTORAD))
      BSTAR=1.0/(CELLB*SIN(GAMMA*DEGTORAD))
      CSTAR=1.0/CAXIS
      CALL CCPDPN(1,'IN','READONLY','F',0,0)
      CALL CCPDPN(2,'OUT','UNKNOWN','F',0,0)
      CALL CCPDPN(3,'REFHKL','UNKNOWN','F',0,0)
C      CALL DOPEN(1,'IN','RO','F')
C      CALL DOPEN(2,'OUT','NEW','F')
      WRITE(6,60)
60    FORMAT('   H   K   L      A      P     FOM*100         REJECTS')
CHEN>
C61    FORMAT(35X,2I4,F7.3,F8.1,F7.1,F8.1,F6.1,F10.1)
61    FORMAT(35X,2I4,F7.3,G13.6,F7.1,G13.6,F6.1,F10.1)
CHEN<
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
        A=A*SCALE       ! too big for LCF file
        NIN=NIN+1
        IF(SIGA.EQ.0.0.OR.SIGP.EQ.0.0) THEN
                NSIGZERO=NSIGZERO+1
                WRITE(6,61)IH,IK,ZSTAR,A,P,SIGA,SIGP
                GO TO 100
        ENDIF
        IF(A.LE.0.0) THEN
                NAZERO=NAZERO+1
                WRITE(6,61)IH,IK,ZSTAR,A,P,SIGA,SIGP
                GO TO 100
        ENDIF
        SIGPR=SIGP*REDUCAC
        IF(SIGPR.GT.90.0)SIGPR=90.0
        FOMCALC=100.0*COS(SIGPR*DEGTORAD)
        IF(FOMCALC.LT.1.0) THEN
                NFOMLOW=NFOMLOW+1
                WRITE(6,61)IH,IK,ZSTAR,A,P,SIGA,SIGP
                GO TO 100       ! skip this one
        ENDIF
      IL=NINT(ZSTAR*CAXIS)
      RSQ=IH**2*ASTAR**2+IK**2*BSTAR**2+ZSTAR**2+
     .          2.0*IH*IK*ASTAR*BSTAR*COS(GAMMA*DEGTORAD)
      IF(RSQ.GT.1.0/RESOLUTION**2) THEN
                NRESHI=NRESHI+1
                RES=SQRT(1.0/RSQ)
                WRITE(6,61)IH,IK,ZSTAR,A,P,SIGA,SIGP,RES
                GO TO 100
      ENDIF
      NGOOD=NGOOD+1
      WRITE(2,55)IH,IK,IL,A,P,FOMCALC
      WRITE(3,54)IH,IK,IL,A,P,FOMCALC,SIGA
      WRITE(6,55)IH,IK,IL,A,P,FOMCALC
54    FORMAT(3I6,G16.8,G16.8,G16.8,G16.8)
55    FORMAT(3I6,G16.8,G16.8,G16.8)
C54    FORMAT(3I4,F8.1,F7.1,F6.1,F10.1)
C55    FORMAT(3I4,F8.1,F7.1,F6.1)
      GO TO 100
400   WRITE(6,56)NGOOD,NIN,NSIGZERO,NAZERO,NFOMLOW,NRESHI,RESOLUTION
56    FORMAT(' MKLCF FILE COMPLETED',/,/,
     .  I10,' good phases written out',
     .  ', from a total read in of',I10,/,
     .  I10,' of these had zero sigma(s)',/,
     .  I10,' of them had zero amplitude',/,
     .  I10,' of them had figure of merit <0.01, and',/,
     .  I10,' were beyond resolution limit of',F8.1)
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

