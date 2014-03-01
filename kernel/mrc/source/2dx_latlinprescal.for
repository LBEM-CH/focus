C LATLINPRESCAL : program to correct image amplitudes for effects of CTF and to
C                calculate appropriate weights for output to LATLINE.
C
C       vx 1.0  RH      18.9.93    original program fused from READBOTH + SCALIMAMP
C       vx 1.1  RH      25.7.95    debug of WTFACTOR on move to Dec Alpha
C       vx 1.2  AS      07.07.2003 changed IFILM to 10 digits
C
C********************************************************************************
C    Control cards:
C
C    Card 1:      NSER,ZMIN,ZMAX     (*)
C                 serial number on ORIGTILT input
C                 and ZSTAR limits to be passed on to LATLINE.
C    Card 2:      IQMAX    (*) 
C                 maximum value of IQ for spots to be used
C    Card 3:      IDOCTF
C                 0 for no CTF correction, else do CTF correction
C
C    INPUT file is raw file straight from ORIGTILT     : on unit FOR001
C    OUTPUT has the format required by LATLINE         : on unit FOR003
C
C*******************************************************************************
      integer * 8 IFILM,ISER,NSER
      character * 160 czeile
      DIMENSION SIGTABLE(9),WEIGHTABLE(9)
      DATA WEIGHTABLE/49.00,27.56,8.51,4.17,2.48,1.65,1.17,0.25,0.0/
      DEG=180.0/3.1415926
C
      WRITE(6,4000)
4000  FORMAT('  LATLINPRESCAL VX1.2(07.01.2003): CTF correction and'/
     .  ' PREPARATION OF WEIGHTS FOR LATLINE PROGRAM'/
     .  ' DATA TAKEN DIRECTLY FROM MERG_3D.APH',
     .  ' FILE, SIGPHI FOR EACH DATA POINT IS DETERMINED'/' FROM THE IQ',
     .  ' VALUE ACCORDING TO THE SCHEME BELOW'/' WEIGHT DETERMINED FROM',
     .  ' IQ MAY BE ADJUSTED BY WTFACTOR BEFORE CONVERSION TO SIGANG'/
     .  ' WTFACTOR FOR EACH SPOT DEPENDS ON THE QUALITY OF THE IMAGE IT',
     .  ' CAME FROM'/)
C
      DO 4001 IQ=1,8
        WT=WEIGHTABLE(IQ)
        SIGPHI=1.0/(SQRT(WT))
        SIGTABLE(IQ)=SIGPHI*DEG
4001  CONTINUE
      SIGTABLE(9)=200.0
C
      WRITE(6,4002)(WEIGHTABLE(I),I=1,9),(SIGTABLE(I),I=1,9)
4002  FORMAT(' WEIGHTING TABLE'/
     .'       IQ        1       2       3       4       5       6',
     .'       7       8       9'/
     .'   WEIGHT ',9F8.2/'   SIGPHI ',9F8.2/)
C
C  READ DETAILS OF MERGE FILE AND DESIRED RANGES OF DATA TO BE READ
      READ(5,*)NSER,ZMIN,ZMAX
      READ(1,10)ISER
10    FORMAT(I10)
      IF(ISER.NE.NSER)THEN
        WRITE(6,13)NSER,ISER
13      FORMAT(' UNEXPECTED SERIAL NUMBER; REQUESTED',I10,' FOUND',I10)
      STOP
      ELSE
        WRITE(6,12)ISER
12      FORMAT(' CORRECT SERIAL NUMBER FOUND',I10)
      END IF
      BACKSPACE 1
      READ(1,11)TITLE
11    FORMAT(10A4)
C
      WRITE(6,14)TITLE
14    FORMAT(' TITLE OF MERGED LIST',10A4/)
      WRITE(6,16)ZMIN,ZMAX
16    FORMAT(' RANGE OF Z VALUES FOR INCLUSION IN AVERAGE',2F10.4/)
      READ(5,*)IQMAX
      WRITE(6,916)IQMAX
916   FORMAT(' DATA WILL BE INCLUDED UP TO IQMAX OF',I5/)
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
      write(*,'(/,''Give Scale Factor to downscale AMP and BCK (e.g. 0.001):'')')
      READ(5,*)RSCALEFAC
      if(RSCALEFAC.lt.0.000001)RSCALEFAC=0.001
      if(RSCALEFAC.gt.1.0)RSCALEFAC=1.0
      write(*,'(''Scale Factor for AMPlitudes and BACKground values is '',F16.9)')
      write(*,'(''Use this to make sure that the SIGF values stay below 999.0'')')
      write(*,'(''so that errors can be encoded with that number'')')
C
      NPASS=0
      NOUT=0
      NREAD=0
      RMAXAMP=0.0
C
C  READ MERG_3D.APH, CORRECT AMP FOR CTF, AND WRITE OUT
C
      goto 19
18    continue
        write(*,'('' ERROR reading: '',A79)')czeile
19    continue
      read(1,'(A)',END=49)czeile
      READ(czeile,*,ERR=18)
     1 IHM,IKM,ZM,AMP,PHASE,IFILM,IQ,WTFACTOR,BCK,CTF
C
CHEN>
      RSCALEFAC=0.0001
      AMP=AMP*RSCALEFAC
      BCK=BCK*RSCALEFAC
CHEN<
C       write(*,'('' read: '',2I,3F,2I,3F)')
C      1 IHM,IKM,ZM,AMP,PHASE,IFILM,IQ,WTFACTOR,BCK,CTF
C
      NREAD=NREAD+1
      IQQ=IABS(IQ)
      WT=WEIGHTABLE(IQQ)*WTFACTOR
C
      SIGPHI=1.0/(SQRT(WT))
      SIGANG=SIGPHI*DEG
C  maximum CTF correction is set to 5-fold
C      IF(ABS(CTF).LT.0.2) CTF=SIGN(0.2,CTF)
      CTF=ABS(CTF)
      IF(CTF.LT.RMAXVAL)CTF=RMAXVAL
      CTFSF=1.0/CTF
      AMPCORR=AMP*CTFSF
      SIGAMP=BCK*CTFSF
      IF(ZM.GE.ZMIN.AND.ZM.LE.ZMAX.AND.IQQ.LE.IQMAX)THEN
C       ZSTAR  AND IQ WITHIN RANGE, SO WRITE OUT (LATLINE INPUT IS * FORMAT)
        WRITE(3,2000)IHM,IKM,ZM,AMPCORR,PHASE,SIGAMP,SIGANG,IQQ
2000    FORMAT(2I6,F12.5,3G16.8,G16.8,I6)
CHEN>
        if(RMAXAMP.LT.AMPCORR)RMAXAMP=AMPCORR
CHEN<
        NOUT=NOUT+1
        GO TO 19
      ELSE
C       ZSTAR OR IQ TOO LARGE; BYPASS THIS DATA POINT
        NPASS=NPASS+1
        GO TO 19
      ENDIF
C
49    WRITE(6,51)
CHEN>
      call system("\rm -f TMP_RMAXAMP.dat")
      open(18,FILE='TMP_RMAXAMP.dat',STATUS='NEW')
      write(18,'(G16.8)')RMAXAMP
      close(18)
CHEN<
51    FORMAT(' END OF MERGED LIST')
      WRITE(6,52)NREAD,NPASS,NOUT
52    FORMAT(' # DATA POINTS READ',I10,/,
     .  ' # DATA POINTS SKIPPED AS',
     .  ' OUTSIDE Z OR IQ RANGE CHOSEN',I10,/,
     .  ' # DATA POINTS WRITTEN OUT',I10)
C
      END
