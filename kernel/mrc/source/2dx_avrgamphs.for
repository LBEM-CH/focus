C AVRGAMPHS
C
C
C       Version  1.1    RH      05-Oct-1992     include ctf amplitude correction
C       Version  1.2    RH      18-Oct-1992     print out ctf-correction factor.
C       Version  1.3    RH      22-Oct-1992     more spots
C       Version  1.4    RH      25-Mar-1994     delete COMBAMP*QFACTOR
C                                                - FOM sufficient.
C       Version  1.5    RH      10-Nov-1995     SAVEBACK, SAVECTF debug
C       Version  1.6    RH      08-Nov-1999     debug COMBPHASEXX(NMAX)
C       Version  1.7    RH      27-Oct-2000     divide-by-zero for one measurement
C       Version  1.8    RH      12-Apr-2001     check NMAX overflow, give diagnostic
C       Version  1.9    RH      17-Apr-2001     increase NMAX to 5000
C       Version  2.0    HS      26-Jun-2008     additional parameters
C
C
C       Input card data:-
C
C       Card 0          FLAG                    (T or F) post ORIGTILTC ?
C                               FLAG allows merging of ORIGTILT files
C                               with or without ctf values.
C       Card 1          NSER,ZMIN,ZMAX          (*) serial number, z-range.
C       Card 2          IQMAX                   (*) maximum IQ to use.
C       Card 3          Number of entries in output table
C       Card 4          RESolution of highest line in output table
C       Card 5          A,B,GAMMA               (*) cell dimensions.
C       Card 6          Maximal amplitude correction value (default=5).
C
C       PROGRAM TO OBTAIN WEIGHTED AVERAGE PHASES FROM MERGED LIST
C       INPUT IS OUTPUT FROM ORIGMERG
C       OUTPUT TO INCLUDE EXPERIMENTAL RESIDUAL AND MERGED FOM
C
        PARAMETER (NMAX=5000)
        COMMON IH(NMAX),IK(NMAX),Z(NMAX),AMP(NMAX),PHASE(NMAX),
     .  IQ(NMAX),BACK(NMAX),CTF(NMAX)
C
        COMMON/HISTOC/NN,SERR,ERR,NRES,SRES,NQ,SQ
        COMMON/QHISTOC/SUMFOMANG,SUMQFACT,NQF,SUMFOM
CHENN>
        COMMON/COM2DX/IRESTEP,NPSLOTS
C
C        PARAMETER (NSLOTS=15)
        PARAMETER (NSLOTS=2000)
        PARAMETER (RMAXRES=1.0)
CHENN<
        PARAMETER (NIQ=8)
        DIMENSION SUMFOMANG(NSLOTS),SUMQFACT(NSLOTS),NQF(NSLOTS)
        DIMENSION SUMFOM(NSLOTS)
        DIMENSION COMBPHASEXX(NMAX),FOMOUTXX(NMAX)
        DIMENSION NN(NSLOTS,NIQ),SERR(NSLOTS,NIQ)
        DIMENSION ERR(NSLOTS,NIQ)
        DIMENSION NRES(NSLOTS),SRES(NSLOTS)
        DIMENSION NQ(NIQ),SQ(NIQ),TQ(NIQ)
        DIMENSION TITLE(15)
        CHARACTER*4 CHAR,CHAR1
        LOGICAL*1 FLAG
        INTEGER * 8 ISER,IFILM,NSER
C
        DIMENSION FOM(8)
C       FIGURE OF MERIT TABLE USED WHEN ONLY ONE MEASUREMENT OF SPOT
C       AND TO CONVERT BACK FROM FOMOUT TO IQOUT
        DATA FOM/0.990,0.982,0.939,0.870,0.763,0.630,0.505,0.124/
C
CHENN>
C        DATA IRESTEP/103/
CHENN<
        DATA CHAR1/' "  '/
        DRAD=3.141592654/180.0
        IZFIX=0
        AMPFIX=10.0
CHENN>
        SERRESALL=0.0
        TOTQFACT=0.0
        TOTFOMANG=0.0
        TOTFOM=0.0
        NTOT=0.0
        NRESALL=0
CHENN<
C
        WRITE(6,1000)
1000    FORMAT(/,' 2dx_AVRGAMPHS - averages amps,phases V2.0',
     .    '(17-Apr-2007)',/)
C
        READ(5,*)FLAG
        READ(5,*)NSER,ZMIN,ZMAX
        READ(5,*)IQMAX
        write(*,'(''IQMAX ='',I10)')IQMAX
        IF(IQMAX.GT.8) STOP 'IQ table only reaches 8'
        READ(5,*)NPSLOTS
        IF(NPSLOTS.GT.NSLOTS)NPSLOT=NSLOTS
        write(*,'(''NPSLOTS='',I10)')NPSLOTS
        READ(5,*)RLOCRES
        IF(RLOCRES.LT.RMAXRES)RLOCRES=RMAXRES
        IF(RLOCRES.GT.200.0)then
          RLOCRES=200.0
          write(*,'(''::WARNING: avrgamphsRESOL limited to '',F10.1)')RLOCRES
        endif
        write(*,'(''RLOCRES'',F10.3)')RLOCRES
        IRESTEP=1000000.0/(NPSLOTS*RLOCRES*RLOCRES)
        write(*,'(''IRESTEP='',I10)')IRESTEP
        READ(5,*)A,B,GAMMA
        GAMMASTAR=DRAD*(180.0-GAMMA)
        GAMMA=GAMMA*DRAD
        ASTAR=1.0/(A*SIN(GAMMA))
        BSTAR=1.0/(B*SIN(GAMMA))
C
        READ(5,*)RMAXCOR
        write(*,'(''Max Amp Correction = '',F12.1)')RMAXCOR
        if(RMAXCOR.gt.1.0)then
          RMAXVAL=1.0/RMAXCOR
          write(*,'(''This corresponds to= '',F12.1)')RMAXVAL
        else
          RMAXVAL=1.0
          write(*,'(''This is corrected to= '',F12.1)')RMAXVAL
        endif
C
        READ(1,10)ISER
10      FORMAT(I10)
        IF(ISER.NE.NSER)THEN
                WRITE(6,13)NSER,ISER
13      FORMAT(' UNEXPECTED SERIAL NUMBER; REQUESTED',I10,' FOUND',I10)
                GO TO 500
        ELSE
                WRITE(6,12)ISER
12      FORMAT(' CORRECT SERIAL NUMBER FOUND',I10)
        END IF
        BACKSPACE 1
        READ(1,11)TITLE
11      FORMAT(15A4)
C
        WRITE(6,14)TITLE
14      FORMAT(' TITLE OF MERGE LIST',15A4,/)
        WRITE(6,16)ZMIN,ZMAX
16      FORMAT(' RANGE OF Z VALUES FOR INCLUSION IN AVERAGE',2F10.4,/)
        WRITE(6,916)IQMAX
916     FORMAT(' DATA WILL BE INCLUDED UP TO IQMAX OF',I5,/)
CHENN>
        OPEN(2,FILE="fort.2",STATUS='NEW')
CHENN<
        WRITE(2,10)ISER
CHENN>
        OPEN(3,FILE="fort.3",STATUS='NEW')
        WRITE(3,10)ISER
CHENN<
        WRITE(6,17)
17      FORMAT(/,' * - very sharp probability, fom near 1.0',/,
     .     '  " - only one measurement, no averaging',/,
     .     '    - normal averaging + calculation of FOMOUT')
C
        WRITE(6,22)
22      FORMAT(/,' Refln.  No. ctf_sf    COMBINED(A+P)     QFACT',
     .    '      FOM ',
     .    '         FOM   STD ERROR',/,
     .    '                       AMP    PHASE                          
     .    ',
     .    '    ANGLE   OF MEAN',/,/)
C
        NEXP=0
        IEND=0
        LH=0
        LK=0
        NUSED=0
        NPASS=0
        NREFL0=0
        NREFL1=0
        NREFLN=0
        NREAD=0
        NOBS=0
        OBSERRSQ=0.0
        FOMANGSQ=0.0
C
        I=1
19     IF(FLAG) THEN            ! new ORIGTILTC data
          READ(1,*,END=49)IH(I),IK(I),Z(I),AMP(I),PHASE(I),IFILM,
     .          IQ(I),WGT,BACK(I),CTF(I)
        ELSE                    ! old ORIGTILTB and preceding data
          READ(1,*,END=49)IH(I),IK(I),Z(I),AMP(I),PHASE(I),IFILM,IQ(I)
                BACK(I)=1.0
                CTF(I)=1.0
        ENDIF
C
C  maximum CTF correction is set to 5-fold
        IF(ABS(CTF(I)).LT.RMAXVAL) CTF(I)=SIGN(RMAXVAL,CTF(I))
C
        NREAD=NREAD+1
20      FORMAT(1X,2I5,F10.4,2F10.2,I10,I5)      ! used to be input format
C        print *,"Starting table ", IH(I), " ", IK(I), "<-------------\n"
48      continue
21      IF(IH(I).EQ.LH.AND.IK(I).EQ.LK.and.IEND.ne.1)THEN
C               CONTINUING SAME INDICES
                IQQ=IABS(IQ(I))
                IF(Z(I).GE.ZMIN.AND.Z(I).LE.ZMAX.AND.IQQ.LE.IQMAX)THEN
C                 Z NEAR ENOUGH TO ZERO AND IQ WITHIN RANGE
                  I=I+1
CHEN:              PRINT *,I," ", LH, " ", LK, "<-------------------\n"
                  IF(I.GT.NMAX) 
     .              STOP ' Too many spots for program dimensions'
                  GO TO 19
                ELSE
C                 Z TOO FAR FROM ZERO; OR IQ TOO LARGE; BYPASS THIS DATA POINT
                  NPASS=NPASS+1
C                 WRITE(6,5018) IH(I),IK(I),Z(I),NPASS
5018              FORMAT(2I5,F10.4,I5)
                  GO TO 19
                ENDIF
        ELSE
                if(IEND.ne.1) then
C                 INDICES HAVE CHANGED
C                 STORE LINE OF DATA
                  ISAVEH=IH(I)
                  ISAVEK=IK(I)
                  SAVEZ=Z(I)
                  SAVEAMP=AMP(I)
                  SAVEPHS=PHASE(I)
                  ISAVEIQ=IQ(I)
                  SAVEBACK=BACK(I)
                  SAVECTF=CTF(I)
                endif
C48             continue
                NSPOT=I-1
                IF(LH.EQ.0.AND.LK.EQ.0)GO TO 31
                IF(NSPOT.EQ.0)THEN
                        NREFL0=NREFL0+1
                        GO TO 31
                END IF
                NUSED=NUSED+NSPOT
                IF(NSPOT.EQ.1)THEN
                        NREFL1=NREFL1+1
C                       ONLY ONE PHASE ANGLE
                        COMBPHASE=PHASE(1)
                        COMBAMP=AMP(1)
CHEN>
C                       for only one measurement, maximum correction is 2.0
C                       IF(ABS(CTF(1)).LT.0.5) THEN
C                               COMBAMP=COMBAMP*2.0
C                               CTFSF=2.0
C
C                       for only one measurement, maximum correction is 7.0
                        IF(ABS(CTF(1)).LT.0.15) THEN
                                COMBAMP=COMBAMP*7.0
                                CTFSF=7.0
                        ELSE
C---------------------------Wiener filter should be:
C-----------------------------   Filter = ( Amplitude * CTF/Noise**2 ) / (CTF**2/Noise**2 + 1/Amplitude**2)
C-----------------------------     Here with ABS(CTF), because phase-flipping was already done.
C
C                               COMBAMP=COMBAMP/ABS(CTF(1))
C                               CTFSF=1.0/ABS(CTF(1))
C
                                COMBBACK = BACK(1)
                                IF(COMBBACK.EQ.0.0) COMBBACK=7.0*ABS(COMBAMP/IQ(1))     ! fudge BACK=0
                                COMBAMP=COMBAMP * ABS(CTF(1)) / COMBBACK**2 / (CTF(1)**2/COMBBACK**2 + 1.0/COMBAMP**2)
                                CTFSF=1.0/ABS(CTF(1))
CHEN<
                        ENDIF
                        PHSERROR=0.0
                        IARG=IABS(IQ(1))
C                       IF(IARG.EQ.1)IARG=2
                        FOMERIT=FOM(IARG)
                        FOMOUT=FOMERIT
                        ANG=ACOS(FOMOUT)
                        FOMANG=ANG/DRAD
                        IQOUT=IARG
                        QFACTOR=1.0
                        WRITE(6,23)LH,LK,NSPOT,CTFSF,
     .                    COMBAMP,COMBPHASE,QFACTOR,
     .                    FOMOUT,CHAR1,FOMANG,PHSERROR
                        FOMCALC=100.0*COS(FOMANG*3.1415927/180.)
                        WRITE(2,30)LH,LK,IZFIX,COMBAMP,COMBPHASE,FOMCALC
CHENN>
                        WRITE(3,29)LH,LK,COMBAMP,COMBPHASE,FOMCALC
                        IQVAL = 1
C                        OPEN(4,FILE="fort.4",STATUS='NEW')
                        WRITE(4,28)LH,LK,COMBAMP,COMBPHASE,IQVAL,FOMCALC
CHENN<
                        GO TO 31
                ELSE
                        NREFLN=NREFLN+1
                        CALL COMBINE(NSPOT,COMBPHASE,FOMOUT,QFACTOR,
     .                    CHAR,COMBAMP,CTFSF)
C      write(6,7777) LH,LK,NSPOT,CTFSF,COMBAMP,COMBPHASE,QFACTOR,
C     .         FOMOUT,CHAR,IEND
C7777           FORMAT(1X,2I3,I4,F7.2,F10.1,F8.1,F10.3,F10.3,A4,10X,I2)
C
                        DO 121 NEX=1,NSPOT
                          NEXCLUDE=NEX
                          CALL COMBINEX(NEXCLUDE,NSPOT,
     .                        COMBPHASEX,FOMOUTX)
                          COMBPHASEXX(NEX)=COMBPHASEX
                          FOMOUTXX(NEX)=FOMOUTX
121                     CONTINUE
C      write(6,7777) LH,LK,NSPOT,CTFSF,COMBAMP,COMBPHASE,QFACTOR,
C     .         FOMOUT,CHAR,IEND
C
                        ANG=ACOS(FOMOUT)
                        FOMANG=ANG/DRAD
                        CALL HISTO(NSPOT,COMBPHASE,COMBPHASEXX,FOMOUTXX,
     .                    ASTAR,BSTAR,GAMMASTAR)
                        CALL QHISTO(NSPOT,QFACTOR,FOMOUT,FOMANG,ASTAR,
     .                    BSTAR,GAMMASTAR)
                        CALL ERROR(NSPOT,COMBPHASE,PHSERROR)
                        WRITE(6,23)LH,LK,NSPOT,CTFSF,COMBAMP,COMBPHASE,
     .                    QFACTOR,FOMOUT,CHAR,FOMANG,PHSERROR
23                      FORMAT(1X,2I3,I4,F7.2,F10.1,F8.1,F10.3,F10.3,
     .                    A4,2F10.2)
C
                        IF(FOMOUT.GE.FOM(1))IQOUT=1
                        IF(FOMOUT.LT.FOM(1).AND.FOMOUT.GE.FOM(2))IQOUT=2
                        IF(FOMOUT.LT.FOM(2).AND.FOMOUT.GE.FOM(3))IQOUT=3
                        IF(FOMOUT.LT.FOM(3).AND.FOMOUT.GE.FOM(4))IQOUT=4
                        IF(FOMOUT.LT.FOM(4).AND.FOMOUT.GE.FOM(5))IQOUT=5
                        IF(FOMOUT.LT.FOM(5).AND.FOMOUT.GE.FOM(6))IQOUT=6
                        IF(FOMOUT.LT.FOM(6).AND.FOMOUT.GE.FOM(7))IQOUT=7
                        IF(FOMOUT.LT.FOM(7).AND.FOMOUT.GE.FOM(8))IQOUT=8
                        IF(FOMOUT.LT.FOM(8))IQOUT=9
C
                        FOMCALC=100.0*COS(FOMANG*3.142/180.)
                        WRITE(2,30)LH,LK,IZFIX,COMBAMP,COMBPHASE,FOMCALC
CHENN>
                        WRITE(3,29)LH,LK,COMBAMP,COMBPHASE,FOMCALC
                        IQVAL = 1
                        WRITE(4,28)LH,LK,COMBAMP,COMBPHASE,IQVAL,FOMCALC
28                      FORMAT(2I6,2G16.6,I6,G16.6)
29                      FORMAT(2I6,2G16.6,G16.6)
CHENN<
30                      FORMAT(3I6,2G16.6,G16.6) 
C
                        OBSERRSQ=OBSERRSQ+PHSERROR**2
                        FOMANGSQ=FOMANGSQ+FOMANG**2
                        NOBS=NOBS+1
                END IF
C
C               NEW INDICES FOR NEXT SPOT
31              IF(IEND.EQ.1)GO TO 50
                LH=ISAVEH
                LK=ISAVEK
                I=1
                IH(I)=ISAVEH
                IK(I)=ISAVEK
                Z(I)=SAVEZ
                AMP(I)=SAVEAMP
                PHASE(I)=SAVEPHS
                IQ(I)=ISAVEIQ
                BACK(I)=SAVEBACK
                CTF(I)=SAVECTF
                GO TO 21
        END IF
C
49      IEND=1
        print *,"Closed the file <-------------------<<"
        GO TO 48
50      WRITE(6,51)
51      FORMAT(' END OF MERGED LIST')
        WRITE(6,52)NREAD,NUSED,NPASS
52      FORMAT(' # DATA POINTS READ',I10,/,
     .  ' # DATA POINTS USED',I10,/,' # DATA POINTS SKIPPED AS',
     .  ' OUTSIDE Z OR IQ RANGE CHOSEN',I10)
        WRITE(6,53)NREFLN,NREFL1,NREFL0
53      FORMAT(' # REFLECTIONS WITH MORE THAN ONE MEASUREMENT AVERAGED',
     .  I5,/,' # WITH ONLY ONE MEASUREMENT',I5,/,
     .  ' # WITH ZERO MEASUREMENTS',
     .  ' IN CHOSEN Z RANGE',I5)
C
        IF(FOMANGSQ.NE.0.0)THEN
                RATIO=SQRT(OBSERRSQ/FOMANGSQ)
                WRITE(6,501)RATIO,NOBS
501             FORMAT(' RATIO RMS OBSERVED RESIDUAL TO RMS FOM ANGLE',F10.3,/,
     .  '       CALCULATED ON',I5,
     .  ' SPOTS WITH MORE THAN ONE MEASUREMENT')
        ENDIF
500     CONTINUE
C********
C
C       WRITE TABLE OF RESIDUAL AS FUNCTION OF RESOLUTION
C
CHEN>
        OPEN(14,FILE="TMP444789.tmp",STATUS='NEW',ERR=10170)
        goto 10169
10170   continue
        write(*,
     1  '('' ERROR: on opening TMP444787.tmp as new file !'')')
        write(*,
     1  '('' or     on opening TMP444789.tmp as new file !'')')
10169   continue
CHEN<
C
        WRITE(6,10173)
        WRITE(14,20173)
10173   FORMAT(/,30X,' PHASE RESIDUALS IN RESOLUTION RANGES',/)
20173   FORMAT('::',/,'::',24X,
     .    'PHASE RESIDUALS IN RESOLUTION RANGES',/,'::')
        WRITE(6,10171)(I,I=1,8)
        WRITE(14,20171)(I,I=1,8)
10171   FORMAT(' RANGE  DMIN  DMAX     IQ =',/,18X,8(5X,I1))
20171   FORMAT('::  #   DMIN DMAX  IQ=',I2,7(I6),
     1  ' all IQs',/,'::',72('-'))
C
        DO 10175 I=1,NPSLOTS
CHEN>
          RIQWALL=0.0
          SIQWALL=0.0
CHEN<
          DO 10176 J=1,NIQ
            IF(NN(I,J).EQ.0)THEN
              GO TO 10176
            END IF
            ERR(I,J)=SERR(I,J)/NN(I,J)
            NRES(I)=NRES(I)+NN(I,J)
            SRES(I)=SRES(I)+SERR(I,J)
            NQ(J)=NQ(J)+NN(I,J)
            SQ(J)=SQ(J)+SERR(I,J)
            NRESALL=NRESALL+NN(I,J)
CHEN>
C--------- RIQWALL is N IQ-Weighted of all spots from 1 to 7.
            if(J.le.7)then
              JTEMP=J
              if(JTEMP.EQ.1)JTEMP=2
              RIQWALL=RIQWALL+(REAL(NN(I,J))/REAL(JTEMP))
              SIQWALL=SIQWALL+ERR(I,J)*(REAL(NN(I,J))/REAL(JTEMP))
            endif
CHEN<
10176     SERRESALL=SERR(I,J)+SERRESALL
          DMIN=SQRT(1000000.0/((I-1)*IRESTEP + 1))
          DMAX=SQRT(1000000.0/(I*IRESTEP))
C
          IF(NRES(I).LE.0)THEN
            TRES=0.0
          ELSE
            TRES=SRES(I)/NRES(I)
          END IF
          IF(RIQWALL.LE.0.000001)THEN
            SIQWALL=0.0
          ELSE
            SIQWALL=SIQWALL/RIQWALL
          ENDIF
C
          WRITE(6,10172)I,DMIN,DMAX,(ERR(I,J),J=1,8),TRES
          WRITE(6,10179)I,(NN(I,J),J=1,8),NRES(I)
          WRITE(14,20172)I,DMIN,DMAX,(ERR(I,J),J=1,8),TRES
          WRITE(14,20179)I,(NN(I,J),J=1,8),NRES(I)
          if(I.lt.NPSLOTS) write(14,'(''::'')')
10175   CONTINUE
C
      DO 10181 J=1,NIQ
      IF(NQ(J).EQ.0)THEN
        TQ(J)=0.0
      ELSE
        TQ(J)=SQ(J)/NQ(J)
      END IF
10181   CONTINUE
C
        write(14,'(''::'',72(''-''))')
        WRITE(6,10177) (TQ(J),J=1,8)
        WRITE(6,10178) (NQ(J),J=1,8)
        WRITE(14,20177) (TQ(J),J=1,8)
        WRITE(14,20178) (NQ(J),J=1,8)
10172   FORMAT(I5,F7.1,F5.1,1X,8F6.1,F8.1)
10179   FORMAT(I5,12X,1X,8I6,I8,/)
10177   FORMAT(18X,8F6.1)
10178   FORMAT(18X,8I6,/)
20172   FORMAT('::',I3,F7.1,F5.1,1X,8(F6.1),F8.1)
20179   FORMAT('::',I3,7X,5X,1X,8(I6),I8)
20177   FORMAT('::',16X,7(F6.1),F6.1)
20178   FORMAT('::',16X,7(I6),I6,/,'::',72('-'))
        WRITE(6,10174)SERRESALL/NRESALL,NRESALL
        WRITE(14,20174)SERRESALL/NRESALL,NRESALL
C
CHENN>
        call system("\rm -f 2dx_avrgamphs.phares.txt")
        open(15,FILE="2dx_avrgamphs.phares.txt",STATUS='NEW',ERR=10186)
        write(15,'(F8.3)')SERRESALL/NRESALL
        write(15,'(I8)')NRESALL
        close(15)
10186   continue
CHENN<
C
        IF(NRESALL.NE.0)WRITE(6,10174)SERRESALL/NRESALL,NRESALL
10174   FORMAT(/,'  OVERALL',19X,F10.3,I7,/,/)
20174   FORMAT('::  Overall:    Phaseresidual =',F8.3,
     .    '          Number of spots =',I7,/,'::',/)
C*****
C
CHENN>
        OPEN(11,FILE="TMP444888.tmp",STATUS='NEW',ERR=10187)
        WRITE(11,'(F10.3)') SERRESALL/NRESALL
        CLOSE(11)
        goto 10188
10187   continue
        write(*,
     1  '('' ERROR: on opening TMP444888.tmp as new file !'')')
10188   continue
CHENN<
C
C       WRITE TABLE OF MEAN QFACTOR, FOM AND FOMANG VERSES RESOLUTION 
        WRITE(6,30173)
30173   FORMAT(":",/,":",13X,
     .  ' MEAN QFACTOR AND MEAN FOMANG IN RESOLUTION',
     .  ' RANGES',/,":",/,":")
        WRITE(6,30171)
30171   FORMAT(': RANGE  DMIN  DMAX               MEAN QFACTOR',
     .  '      MEAN FOM     MEAN FOMANG',/,/)
        DO 30175 I=1,NPSLOTS
          DMIN=SQRT(1000000.0/((I-1)*IRESTEP + 1))
          DMAX=SQRT(1000000.0/(I*IRESTEP))
          IF(NQF(I).EQ.0)THEN
            AVQFACTOR=0.0
            AVFOMANG=90.0
            AVFOM=0.0
          ELSE
            AVQFACTOR=SUMQFACT(I)/NQF(I)
            AVFOMANG=SUMFOMANG(I)/NQF(I)
            AVFOM=SUMFOM(I)/NQF(I)
          END IF
          TOTQFACT=TOTQFACT+SUMQFACT(I)
          TOTFOMANG=TOTFOMANG+SUMFOMANG(I)
          TOTFOM=TOTFOM+SUMFOM(I)
          NTOT=NTOT+NQF(I)
          WRITE(6,30172)I,DMIN,DMAX,NQF(I),AVQFACTOR,AVFOM,AVFOMANG
30172     FORMAT(":",I6,2F6.1,5X,I5,5X,F10.5,5X,F10.5,5X,F10.3)
30175   CONTINUE
        IF(NTOT.NE.0) THEN
                AVQFACTOR=TOTQFACT/NTOT
                AVFOMANG=TOTFOMANG/NTOT
                AVFOM=TOTFOM/NTOT
                WRITE(6,30174)NTOT,AVQFACTOR,AVFOM,AVFOMANG
30174           FORMAT(/,":"/,
     .          ":",23X,I5,5X,F10.5,5X,F10.5,5X,F10.3,/,
     .          ":",/,":",78("="),/,":",/)
        ENDIF
        CLOSE(14)
        WRITE(6,*) ' Normal program termination'
        END
C**************************************************************************
      SUBROUTINE COMBINE(NSPOT,COMBPHASE,FOMOUT,QFACTOR,CHAR,COMBAMP,
     .  CTFSF)
      PARAMETER (NMAX=5000)
      COMMON IH(NMAX),IK(NMAX),Z(NMAX),AMP(NMAX),PHASE(NMAX),
     .  IQ(NMAX),BACK(NMAX),CTF(NMAX)
C
      DIMENSION WEIGHTABLE(8)
      DATA WEIGHTABLE/49.00,27.56,8.51,4.17,2.48,1.65,1.17,0.25/
      REAL*8 XARG,S18AEF,S18AFF
C     DIMENSION CHARACTER(2)
      CHARACTER*4 CHAR,CHARACTER(2)
      DATA CHARACTER/' *  ','    '/
      DRAD=3.141592654/180.0
      SUMAMP=0.0
      SUMAMPOLD=0.0
      SUMAMPW=0.0
      NAMP=0
      SUMWEIGHT=0.0
      SUMCOS=0.0
      SUMSIN=0.0
      IVERBOSE=1
      DO 10 I=1,NSPOT
C       WRITE(6,20)I,PHASE(I)
20      FORMAT(I5,F10.2)
        ANG=DRAD*PHASE(I)
        IARG=IABS(IQ(I))
CHENN>
C       WEIGHT=WEIGHTABLE(IARG)
        if(iarg.ge.1.and.iarg.le.8)then
          WEIGHT=WEIGHTABLE(IARG)
        else
          WEIGHT=0
        endif
CHENN<
        COSANG=COS(ANG)
        SINANG=SIN(ANG)
C       WRITE(6,23)WEIGHT,COSANG,SINANG
23      FORMAT(' WEIGHT,COSANG,SINANG',3F10.5)
        SUMAMPOLD=SUMAMPOLD+AMP(I)
        IF(BACK(I).EQ.0.0) BACK(I)=7.0*AMP(I)/IQ(I)     ! fudge BACK=0
CHEN>
C       SUMAMP =SUMAMP + AMP(I)*ABS(CTF(I))/BACK(I)**2
C       SUMAMPW=SUMAMPW + CTF(I)**2/BACK(I)**2
C
C-------Wiener filter should be:
C---------   Filter = ( Amplitude * CTF/Noise**2 ) / (CTF**2/Noise**2 + 1/Amplitude**2)
C---------     Here with ABS(CTF), because phase-flipping was already done.
C
        SUMAMP =SUMAMP + AMP(I)*ABS(CTF(I))/BACK(I)**2
        SUMAMPW=SUMAMPW + CTF(I)**2/BACK(I)**2 + 1.0/AMP(I)**2
CHEN<
        NAMP=NAMP+1
        SUMCOS=SUMCOS+WEIGHT*COSANG
        SUMSIN=SUMSIN+WEIGHT*SINANG
        SUMWEIGHT=SUMWEIGHT+WEIGHT
10    CONTINUE
      ACOEFF=SUMCOS
      BCOEFF=SUMSIN
C     WRITE(6,24)ACOEFF,BCOEFF
24    FORMAT(' ACOEFF,BCOEFF',2G20.4)
      IF(SUMCOS.EQ.0.0.AND.SUMSIN.EQ.0.0)THEN
        COMBPHASE=0.0
        FOMOUT=0.0
        QFACTOR=0.0
        COMBAMP=0.0
        write(*,'(''::WARNING n 2dx_avrgamphs. '',
     1            ''No data for spot '',I5,''.'')')NSPOT
        write(*,'('':: There may be a problem with the data.'')')
C        STOP ' should never be reached'
      ELSE
        AVRGANG=ATAN2(SUMSIN,SUMCOS)
        COMBPHASE=AVRGANG/DRAD
        COMBAMP=SUMAMP/SUMAMPW
        CTFSF=(SUMAMP/SUMAMPW)/(SUMAMPOLD/NAMP)
        IF(CTFSF.GT.2.5) THEN
          IQMIN=9
          if(IVERBOSE.gt.3)then
            WRITE(6,45)
45          FORMAT(10X,' Warning - high ctf_sf calculated ',
     .        /,10X,
     .        '  H  K    Z      AMP    PHASE  IQ   BACK    CTF')
          endif
46        FORMAT(10X,2I3,F8.4,2F8.1,I3,F8.1,F8.3)
          DO 50 J=1,NSPOT
            IQMIN=MIN0(IQMIN,ABS(IQ(J)))
            if(IVERBOSE.gt.3)then
              WRITE(6,46)IH(J),IK(J),Z(J),
     .          AMP(J),PHASE(J),IQ(J),BACK(J),CTF(J)
            endif
50        CONTINUE
          IF(IQMIN.GT.3) THEN
            COMBAMP=COMBAMP*2.0/CTFSF ! reduce to 2.0
            CTFSF=2.0
            WRITE(6,47)
47          FORMAT(10X,' No spots with IQ<3,',
     .       ' ctf_sf=2.0')
          ENDIF
        ENDIF
C
C
        XARG=SQRT(ACOEFF**2+BCOEFF**2)
        QFACTOR=XARG/SUMWEIGHT
        IFAIL=1
        JFAIL=1
C       
        WT=S18AFF(XARG,JFAIL)/S18AEF(XARG,IFAIL)
C
C IF ABOVE FAILS, GAUSSIAN WILL DO AS PROBABILITY MUST BE VERY SHARP
C
C       WRITE(6,999)JFAIL,IFAIL
        IF(JFAIL.NE.IFAIL) WRITE(6,999)JFAIL,IFAIL,NSPOT
999     FORMAT(' I1 FAIL;, I0 FAIL',2I10,'  NSPOT',I10)
        IF(IFAIL.EQ.1.OR.JFAIL.EQ.1)THEN
          CHAR=CHARACTER(1)
          SIGMA=SQRT(1.0/XARG)
          FOMOUT=COS(SIGMA)
        ELSE
          IF(IFAIL.EQ.0.AND.JFAIL.EQ.0)CHAR=CHARACTER(2)
          FOMOUT=WT
        END IF
C
      END IF
C     COMBAMP=COMBAMP*QFACTOR         ! takes account of bad phases
      RETURN
      END
C*****************************************************************************
      SUBROUTINE COMBINEX(NEX,NSPOT,COMBPHASE,FOMOUT)
      PARAMETER (NMAX=5000)
      COMMON IH(NMAX),IK(NMAX),Z(NMAX),AMP(NMAX),PHASE(NMAX),
     .  IQ(NMAX),BACK(NMAX),CTF(NMAX)
C
      DIMENSION WEIGHTABLE(8)
      DATA WEIGHTABLE/49.00,27.56,8.51,4.17,2.48,1.65,1.17,0.25/
      REAL*8 XARG,S18AEF,S18AFF
C     DIMENSION CHARACTER(2)
      CHARACTER*4 CHARX,CHARACTER(2)
      DATA CHARACTER/' *  ','    '/
      DRAD=3.141592654/180.0
      SUMWEIGHT=0.0
      SUMCOS=0.0
      SUMSIN=0.0
      DO 10 I=1,NSPOT
C       WRITE(6,20)I,PHASE(I)
20      FORMAT(I5,F10.2)
C       EXCLUDE ONE DATA POINT
        IF(I.EQ.NEX)GO TO 10
          ANG=DRAD*PHASE(I)
          IARG=IABS(IQ(I))
CHENN>
C         WEIGHT=WEIGHTABLE(IARG)
          if(iarg.ge.1.and.iarg.le.8)then
            WEIGHT=WEIGHTABLE(IARG)
          else
            WEIGHT=0
          endif
CHENN<
          COSANG=COS(ANG)
          SINANG=SIN(ANG)
C         WRITE(6,23)WEIGHT,COSANG,SINANG
23        FORMAT(' WEIGHT,COSANG,SINANG',3F10.5)
          SUMCOS=SUMCOS+WEIGHT*COSANG
          SUMSIN=SUMSIN+WEIGHT*SINANG
          SUMWEIGHT=SUMWEIGHT+WEIGHT
10      CONTINUE
        ACOEFF=SUMCOS
        BCOEFF=SUMSIN
C       WRITE(6,24)ACOEFF,BCOEFF
24      FORMAT(' ACOEFF,BCOEFF',2G20.4)
        IF(SUMCOS.EQ.0.0.AND.SUMSIN.EQ.0.0)THEN
          COMBPHASE=0.0
          FOMOUT=0.0
          QFACTOR=0.0
          GO TO 11
        ELSE
          AVRGANG=ATAN2(SUMSIN,SUMCOS)
          COMBPHASE=AVRGANG/DRAD
C
          XARG=SQRT(ACOEFF**2+BCOEFF**2)
          QFACTOR=XARG/SUMWEIGHT
          IFAIL=1
          JFAIL=1
C       
          WT=S18AFF(XARG,JFAIL)/S18AEF(XARG,IFAIL)
C
C IF ABOVE FAILS, GAUSSIAN WILL DO AS PROBABILITY MUST BE VERY SHARP
C
C         WRITE(6,999)JFAIL,IFAIL
999       FORMAT(' I1 FAIL;, I0 FAIL',2I10)
          IF(IFAIL.EQ.1.OR.JFAIL.EQ.1)THEN
            CHARX=CHARACTER(1)
            SIGMA=SQRT(1.0/XARG)
            FOMOUT=COS(SIGMA)
          ELSE
            IF(IFAIL.EQ.0.AND.JFAIL.EQ.0)CHARX=CHARACTER(2)
            FOMOUT=WT
          END IF
C
        END IF
11      RETURN
        END
C*****************************************************************************
      SUBROUTINE ERROR(NSPOT,COMBPHASE,PHSERROR)
      PARAMETER (NMAX=5000)
      COMMON IH(NMAX),IK(NMAX),Z(NMAX),AMP(NMAX),PHASE(NMAX),
     .  IQ(NMAX),BACK(NMAX),CTF(NMAX)
C
      DIMENSION WEIGHTABLE(8)
      DATA WEIGHTABLE/49.00,27.56,8.51,4.17,2.48,1.65,1.17,0.25/
      DRAD=3.141592654/180.0
      SUM=0.0
      WN=0.0
      DO 10 I=1,NSPOT
        IARG=IABS(IQ(I))
CHENN>
C       WEIGHT=WEIGHTABLE(IARG)
        if(iarg.ge.1.and.iarg.le.8)then
          WEIGHT=WEIGHTABLE(IARG)
        else
          WEIGHT=0
        endif
CHENN<
        DIFF=PHASE(I)-COMBPHASE
        IF(DIFF.GT.180.0)DIFF=DIFF-360.0
        IF(DIFF.LT.-180.0)DIFF=DIFF+360.0
        IF(DIFF.LT.0.0) DIFF=-DIFF
        IF(ABS(DIFF).GT.180.0) STOP
        SUM=SUM+(DIFF**2)*WEIGHT
        WN=WN+WEIGHT
10    CONTINUE
      PHSERROR=SQRT(SUM/(WN*(NSPOT-1)))
      RETURN
      END
C****************************************************************************
      SUBROUTINE HISTO(NSPOT,COMBPHASE,COMBPHASEXX,FOMOUTXX,ASTAR,
     .  BSTAR,GAMMASTAR)
      PARAMETER (NMAX=5000)
      COMMON IH(NMAX),IK(NMAX),Z(NMAX),AMP(NMAX),PHASE(NMAX),
     .  IQ(NMAX),BACK(NMAX),CTF(NMAX)
C
      COMMON/HISTOC/NN,SERR,ERR,NRES,SRES,NQ,SQ
      COMMON/COM2DX/IRESTEP,NPSLOTS
CHENN>
C     PARAMETER (NSLOTS=15)
      PARAMETER (NSLOTS=2000)
CHENN<
      PARAMETER (NIQ=8)
      DIMENSION COMBPHASEXX(NMAX),FOMOUTXX(NMAX)
      DIMENSION NN(NSLOTS,NIQ),SERR(NSLOTS,NIQ)
      DIMENSION ERR(NSLOTS,NIQ)
      DIMENSION NRES(NSLOTS),SRES(NSLOTS)
      DIMENSION NQ(NIQ),SQ(NIQ)
CHENN>
C     DATA IRESTEP/103/
CHENN<
C
      DO 1340 I=1,NSPOT
C       CALCULATE RESOLUTION OF SPOT 
        DSTARSQ = IH(I)**2*ASTAR*ASTAR + IK(I)**2*BSTAR*BSTAR + 
     .    2.0*IH(I)*IK(I)*ASTAR*BSTAR*COS(GAMMASTAR)
        IRES=DSTARSQ*1000000.0
C****
        ISLOT= 1 + (IRES-1)/IRESTEP
        IF(ISLOT.LT.1)then
          WRITE(6,20000)ISLOT
20000     FORMAT(' ERROR, ISLOT=',I10,' is too small.')
          STOP
        endif
        IF(ISLOT.GT.NSLOTS) THEN
          WRITE(6,20001)NSLOTS
20001     FORMAT(' ERROR in 2dx_avrgamphs, ',
     .      'NSLOTS too small=',I10)
          write(*,'('' NSPOT  ='',I10  )')NSPOT
          write(*,'('' DSTARSQ='',F12.3)')DSTARSQ
          write(*,'('' IRES   ='',I10)')IRES
          write(*,'('' IRESTEP='',I10)')IRESTEP
          write(*,'('' ISLOT  ='',I10)')ISLOT 
          write(*,'('' IH,IK,I='',3I10)')IH(I),IK(I),I
          write(*,'('' ABSTAR ='',2F12.3)')ASTAR,BSTAR
          STOP
        endif
        IF(COMBPHASEXX(I).EQ.0.0.AND.FOMOUTXX(I).EQ.0.0)GO TO 1340
        DELTA=PHASE(I)-COMBPHASEXX(I)
        IF(DELTA.LT.0.0) DELTA=-DELTA
1310    IF(DELTA.LE.180.0) GO TO 1320
          DELTA=DELTA-360.0
          GO TO 1310
1320    IF(DELTA.LT.0.0) DELTA=-DELTA
C
          JQ=IQ(I)
        IF(JQ.GT.NIQ)THEN
          WRITE (6,1341)JQ,NIQ
1341      FORMAT(' IQ GREATER THAN NIQ LIMIT',2I5,/)
          GO TO 1340
        END IF
        JQ=IABS(JQ)
        SERR(ISLOT,JQ)=SERR(ISLOT,JQ)+DELTA
        NN(ISLOT,JQ)=NN(ISLOT,JQ)+1
1340  CONTINUE
      RETURN
      END
C############################################################################
      SUBROUTINE QHISTO(NSPOT,QFACTOR,FOMOUT,FOMANG,ASTAR,
     .  BSTAR,GAMMASTAR)
      PARAMETER (NMAX=5000)
      COMMON IH(NMAX),IK(NMAX),Z(NMAX),AMP(NMAX),PHASE(NMAX),
     .  IQ(NMAX),BACK(NMAX),CTF(NMAX)
C
      COMMON/QHISTOC/SUMFOMANG,SUMQFACT,NQF,SUMFOM
      COMMON/COM2DX/IRESTEP,NPSLOTS
CHENN>
C     PARAMETER (NSLOTS=15)
      PARAMETER (NSLOTS=2000)
CHENN<
      DIMENSION SUMFOMANG(NSLOTS),SUMQFACT(NSLOTS),NQF(NSLOTS)
      DIMENSION SUMFOM(NSLOTS)

CHENN>
C     DATA IRESTEP/103/
CHENN<
C
C     CALCULATE RESOLUTION OF SPOT 
      I=1
      DSTARSQ = IH(I)**2*ASTAR*ASTAR + IK(I)**2*BSTAR*BSTAR + 
     .  2.0*IH(I)*IK(I)*ASTAR*BSTAR*COS(GAMMASTAR)
      IRES=DSTARSQ*1000000.0
C****
      ISLOT= 1 + (IRES-1)/IRESTEP
      IF(ISLOT.LT.1.OR.ISLOT.GT.NSLOTS) THEN
        WRITE(6,20000)ISLOT
20000   FORMAT(' ERROR, ISLOT=',I10)
        STOP
      END IF
C
C     WRITE(6,9999)ISLOT
9999  FORMAT(' ISLOT',I5)
      SUMFOMANG(ISLOT)=SUMFOMANG(ISLOT)+FOMANG
      SUMQFACT(ISLOT)=SUMQFACT(ISLOT)+QFACTOR
      SUMFOM(ISLOT)=SUMFOM(ISLOT)+FOMOUT
      NQF(ISLOT)=NQF(ISLOT)+1
      RETURN
      END
C############################################################################
