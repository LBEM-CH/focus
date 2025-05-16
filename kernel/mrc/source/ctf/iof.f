C**************************************************************************
      SUBROUTINE GUESSF(CNAME,CFORM,EX)
C**************************************************************************
C Guesses file format. Uses unit 99 to open file.
C**************************************************************************
      IMPLICIT NONE
C
      INTEGER*4 I,HDR,IFILE,NXYZ(3),MO,NLABL
      INTEGER*4 SLEN,IFOL,IXLP,IYLP,IZLP
      INTEGER*4 I1(3),I13,I221
      PARAMETER (HDR=1024)
      INTEGER*1 BHDR(HDR)
      INTEGER*1 I1BHDR
      REAL*4 DMIN,DMAX,PSZ,AVDENS,SIGMA,DENSMAX
      REAL*4 DENSMIN,CELLI,CELLA,R77,R81,R85,R217
      REAL*4 R1,R5,RMODE,RMAX,RMIN,RMEAN,RRMS,R45
      REAL*8 DDRMS,DDMEAN
      CHARACTER CFORM,CNAME*200,LABEL(10)*80
      CHARACTER INAME*200,TYPE*4
      LOGICAL BS,EX
C
      EQUIVALENCE (BHDR     ,I1BHDR)
      EQUIVALENCE (BHDR(5)  ,IFOL)
      EQUIVALENCE (BHDR(49) ,IXLP)
      EQUIVALENCE (BHDR(53) ,IYLP)
      EQUIVALENCE (BHDR(57) ,TYPE)
      EQUIVALENCE (BHDR(69) ,AVDENS)
      EQUIVALENCE (BHDR(73) ,SIGMA)
      EQUIVALENCE (BHDR(85) ,DENSMAX)
      EQUIVALENCE (BHDR(89) ,DENSMIN)
      EQUIVALENCE (BHDR(97) ,CELLI)
      EQUIVALENCE (BHDR(241),IZLP)
C
      EQUIVALENCE (BHDR(1)  ,I1)
      EQUIVALENCE (BHDR(13) ,I13)
      EQUIVALENCE (BHDR(41) ,CELLA)
      EQUIVALENCE (BHDR(77) ,R77)
      EQUIVALENCE (BHDR(81) ,R81)
      EQUIVALENCE (BHDR(85) ,R85)
      EQUIVALENCE (BHDR(217),R217)
      EQUIVALENCE (BHDR(221),I221)
C
      EQUIVALENCE (BHDR(1)  ,R1)
      EQUIVALENCE (BHDR(5)  ,R5)
      EQUIVALENCE (BHDR(17) ,RMODE)
      EQUIVALENCE (BHDR(25) ,RMAX)
      EQUIVALENCE (BHDR(29) ,RMIN)
      EQUIVALENCE (BHDR(33) ,RMEAN)
      EQUIVALENCE (BHDR(37) ,RRMS)
      EQUIVALENCE (BHDR(45) ,R45)
C**************************************************************************
C
      IFILE=99
      BS=.FALSE.
      CFORM=''
C
      I=SLEN(CNAME)
      INAME=CNAME(1:I)
      INQUIRE(FILE=INAME,ERR=9999,EXIST=EX)
C
      IF (.NOT.EX) THEN
        IF (I.GT.4) THEN
          IF ((CNAME(I-3:I).NE.'.hed').AND.
     +        (CNAME(I-3:I).NE.'.img')) THEN
            INAME=CNAME(1:I)
          ELSE
            INAME=CNAME(1:I-4)
          ENDIF
        ENDIF
C
        INAME=INAME(1:SLEN(INAME))//'.hed'
        INQUIRE(FILE=INAME,ERR=9999,EXIST=EX)
      ENDIF
C
      IF (EX) THEN
        CALL COPEN(INAME,IFILE,0)
        CALL CREAD(BHDR,0,HDR,1,IFILE)
        CALL CCLOSE(IFILE)
C
C Test SPIDER format
C
        IF ((RMODE.NE.1.0).AND.(RMODE.NE.3.0)
     +      .AND.(RMODE.NE.-11.).AND.(RMODE.NE.-12.0)
     +      .AND.(RMODE.NE.-21.).AND.(RMODE.NE.-22.0)) THEN
          CALL BYTESWAP(BHDR,HDR)
          BS=.TRUE.
        ENDIF
C
        NXYZ(1)=R45
        NXYZ(2)=R5
        NXYZ(3)=R1
        DMIN=RMIN
        DMAX=RMAX
        DDMEAN=DBLE(RMEAN)
        DDRMS=DBLE(RRMS)
        IF (((RMODE.EQ.1.0).OR.(RMODE.EQ.3.0).OR.
     +      (RMODE.EQ.-11.).OR.(RMODE.EQ.-12.0).OR.
     +      (RMODE.EQ.-21.).OR.(RMODE.EQ.-22.0)).AND.
     +      (NXYZ(1).GT.0).AND.(NXYZ(1).LT.1.0E6).AND.
     +      (NXYZ(2).GT.0).AND.(NXYZ(2).LT.1.0E6).AND.
     +      (NXYZ(3).GT.0).AND.(NXYZ(3).LT.1.0E6)) THEN
          CFORM='S'
          GOTO 99
        ENDIF
C
        IF (BS) THEN
          CALL BYTESWAP(BHDR,HDR)
          BS=.FALSE.
        ENDIF
C
C Test IMAGIC format
C
        NXYZ(1)=IYLP
        NXYZ(2)=IXLP
        NXYZ(3)=IZLP*(IFOL+1)
        IF (IZLP.EQ.IFOL+1) NXYZ(3)=IFOL+1
        IF (IZLP.EQ.0) NXYZ(3)=IFOL+1
        MO=-1
        IF (TYPE.EQ.'PACK') MO=0
        IF (TYPE.EQ.'INTG') MO=1
        IF (TYPE.EQ.'REAL') MO=2
        IF (TYPE.EQ.'COMP') MO=3
        IF (TYPE.EQ.'RECO') MO=4
        DMIN=DENSMIN
        DMAX=DENSMAX
        PSZ=CELLI/IXLP
        DDMEAN=DBLE(AVDENS)
        DDRMS=DBLE(SIGMA)
        IF (MO.NE.-1) THEN
          CFORM='I'
          GOTO 99
        ENDIF
C
C Test MRC/CCP4 format
C
        IF ((I1(1).LE.0).OR.(I1(1).GE.1.0E6).OR.
     +      (I1(2).LE.0).OR.(I1(2).GE.1.0E6).OR.
     +      (I1(3).LE.0).OR.(I1(3).GE.1.0E6).OR.
     +      (I13.LT.0).OR.(I13.GT.4).OR.(I221.LT.0)) THEN
          CALL BYTESWAP(BHDR,HDR)
          BS=.TRUE.
        ENDIF
C
        NXYZ(1)=I1(1)
        NXYZ(2)=I1(2)
        NXYZ(3)=I1(3)
        MO=I13
        DMIN=R77
        DMAX=R81
        DDMEAN=DBLE(R85)
        DDRMS=DBLE(R217)
        NLABL=I221
        IF ((I1(1).GT.0).AND.(I1(1).LT.1.0E6).AND.
     +      (I1(2).GT.0).AND.(I1(2).LT.1.0E6).AND.
     +      (I1(3).GT.0).AND.(I1(3).LT.1.0E6).AND.
     +      (I13.GE.0).AND.(I13.LE.4).AND.(I221.GE.0)) THEN
          CFORM='M'
          GOTO 99
        ENDIF
C
      ENDIF
      GOTO 99
C
9999  CONTINUE
      WRITE(*,*) ' ERROR: FILE INACCESSIBLE'
C
99    CONTINUE
C
      RETURN
      END
C
C**************************************************************************
      SUBROUTINE IOPEN(CNAME,IFILE,CFORM,MODE,NX,NY,NZ,
     .                 CSTIN,PSIZE,TITLE)
C**************************************************************************
      IMPLICIT NONE
C
      INTEGER*4 I,HDR,IM,IFILE,MODE,NX,NY,NZ
      PARAMETER (HDR=1024,IM=200)
      INTEGER*4 NXYZ(3,IM),MO(IM),NSYMBT,NLABL(IM),BL
      INTEGER*4 LABBYT,IOMODE(IM),OFF(IM),RL(IM),SLEN
      REAL*4 DMIN(IM),DMAX(IM),PSIZE,PSZ(IM)
      REAL*8 DDRMS(IM),DDMEAN(IM)
      INTEGER*1 BHDR(HDR),SYM(80),BB
      CHARACTER LABEL(10,IM)*80,TITLE*80
      CHARACTER CFORM,CSTIN*20,CF(IM),CNAME*200
      CHARACTER INAME(IM)*200,INAME1*200
      INTEGER*1 TMPBB(1024)
      LOGICAL BS(IM)
C
      EQUIVALENCE (TMPBB,BB)
      DATA BB/0/
C
      COMMON /IODATA/NXYZ,MO,NLABL,IOMODE,OFF,RL,
     +       DMIN,DMAX,PSZ,DDRMS,DDMEAN,LABEL,CF,BS,INAME
C
      SAVE /IODATA/
C**************************************************************************
C
C     Assume native byteorder
      BS(IFILE)=.FALSE.
C
      IOMODE(IFILE)=0
      IF (CSTIN(1:1).EQ.'N') IOMODE(IFILE)=1
      IF (CSTIN(1:1).EQ.'n') IOMODE(IFILE)=1
      IF (CSTIN(1:1).EQ.'U') IOMODE(IFILE)=1
      IF (CSTIN(1:1).EQ.'u') IOMODE(IFILE)=1
      IF (IOMODE(IFILE).EQ.1) THEN
        DMIN(IFILE)=1.0E30
        DMAX(IFILE)=-1.0E30
        PSZ(IFILE)=PSIZE
        DDMEAN(IFILE)=0.0D0
        DDRMS(IFILE)=0.0D0
        NXYZ(1,IFILE)=NX
        NXYZ(2,IFILE)=NY
        NXYZ(3,IFILE)=NZ
        MO(IFILE)=MODE
        LABEL(1,IFILE)=TITLE
        NLABL(IFILE)=1
      ENDIF
C
      CF(IFILE)=CFORM
C
C**************************************************************************
C     IMAGIC FORMAT
C**************************************************************************
C
      IF ((CF(IFILE).EQ.'I').OR.(CF(IFILE).EQ.'i')) THEN
C
      I=SLEN(CNAME)
      IF (I.GE.4) THEN
        IF ((CNAME(I-3:I).NE.'.hed').AND.
     +      (CNAME(I-3:I).NE.'.img')) THEN
          INAME(IFILE)=CNAME(1:I)
        ELSE
          IF (I.LE.4) STOP ' ERROR: INVALID FILE NAME'
          INAME(IFILE)=CNAME(1:I-4)
        ENDIF
      ELSE
        INAME(IFILE)=CNAME(1:I)
      ENDIF
C
      IF (IOMODE(IFILE).EQ.0) THEN
C
C     *** READ IMAGIC ***
C
        WRITE(*,*)
        WRITE(*,*)'Opening IMAGIC file for READ...'
C
        WRITE(*,*)'File      : '//
     +    INAME(IFILE)(1:SLEN(INAME(IFILE)))//'.hed/img'
C
        INAME1=INAME(IFILE)(1:SLEN(INAME(IFILE)))//'.hed'
        CALL COPEN(INAME1,IFILE,IOMODE(IFILE))
C
        CALL CREAD(BHDR,0,HDR,1,IFILE)
C
        CALL SET_IHDR(BHDR,NXYZ(1,IFILE),MO(IFILE),
     +          DMIN(IFILE),DMAX(IFILE),PSZ(IFILE),
     +          DDMEAN(IFILE),DDRMS(IFILE),
     +          NLABL(IFILE),LABEL(1,IFILE),0)
C
        WRITE(*,'(A13,3I6)')' NX, NY, NZ: ',(NXYZ(I,IFILE),I=1,3)
        IF (MO(IFILE).EQ.0) WRITE(*,'(A18)')' MODE      : int*1'
        IF (MO(IFILE).EQ.1) WRITE(*,'(A18)')' MODE      : int*2'
        IF (MO(IFILE).EQ.2) WRITE(*,'(A17)')' MODE      : real'
        WRITE(*,'(A13,2G16.7)')' Min, max  : ',DMIN(IFILE),DMAX(IFILE)
        WRITE(*,'(A13,2G16.7)')' Mean, RMS : ',REAL(DDMEAN(IFILE)),
     +                                        REAL(DDRMS(IFILE))
C
        DO 10 I=1,NLABL(IFILE)
          WRITE(*,'(A8,I3,A2,A80)')' TITLE  ',I,': ',LABEL(I,IFILE)
10      CONTINUE
        WRITE(*,*)
C
        CALL CCLOSE(IFILE)
C
        INAME1=INAME(IFILE)(1:SLEN(INAME(IFILE)))//'.img'
        CALL COPEN(INAME1,IFILE,IOMODE(IFILE))
C
        BL=2*MO(IFILE)
        IF (BL.EQ.0) BL=1
        RL(IFILE)=NXYZ(1,IFILE)*BL
        OFF(IFILE)=0
C
      ELSE
C
C     *** WRITE IMAGIC ***
C
        WRITE(*,*)
        WRITE(*,*)'Opening IMAGIC file for WRITE...'
C
        WRITE(*,*)'File      : '//
     +    INAME(IFILE)(1:SLEN(INAME(IFILE)))//'.hed/img'
C
        WRITE(*,'(A13,3I6)')' NX, NY, NZ: ',(NXYZ(I,IFILE),I=1,3)
        IF (MO(IFILE).EQ.0) WRITE(*,'(A18)')' MODE      : int*1'
        IF (MO(IFILE).EQ.1) WRITE(*,'(A18)')' MODE      : int*2'
        IF (MO(IFILE).EQ.2) WRITE(*,'(A17/)')' MODE      : real'
C
        CALL SET_IHDR(BHDR,NXYZ(1,IFILE),MO(IFILE),0.0,
     +          0.0,PSZ(IFILE),0.0D0,0.0D0,
     +          NLABL(IFILE),LABEL(1,IFILE),1)
C
        INAME1=INAME(IFILE)(1:SLEN(INAME(IFILE)))//'.hed'
        CALL COPEN(INAME1,IFILE,IOMODE(IFILE))
C
        CALL CWRITE(BHDR,0,HDR,1,IFILE)
C
        CALL CCLOSE(IFILE)
C
        INAME1=INAME(IFILE)(1:SLEN(INAME(IFILE)))//'.img'
        CALL COPEN(INAME1,IFILE,IOMODE(IFILE))
C
        BL=2*MO(IFILE)
        IF (BL.EQ.0) BL=1
        RL(IFILE)=NXYZ(1,IFILE)*BL
        OFF(IFILE)=0
C
      ENDIF
C
C**************************************************************************
C     MRC FORMAT
C**************************************************************************
C
      ELSEIF ((CF(IFILE).EQ.'M').OR.(CF(IFILE).EQ.'m')) THEN
C
      IF (IOMODE(IFILE).EQ.0) THEN
C
C     *** READ MRC/CCP4 ***
C
        WRITE(*,*)
        WRITE(*,*)'Opening MRC/CCP4 file for READ...'
        I=SLEN(CNAME)
        WRITE(*,*)'File      : '//CNAME(1:I)
C
        CALL COPEN(CNAME,IFILE,IOMODE(IFILE))
C
        CALL CREAD(BHDR,0,HDR,1,IFILE)
C
        CALL SET_MHDR(BHDR,NXYZ(1,IFILE),MO(IFILE),
     +          DMIN(IFILE),DMAX(IFILE),PSZ(IFILE),
     +          DDMEAN(IFILE),DDRMS(IFILE),NSYMBT,
     +          NLABL(IFILE),LABEL(1,IFILE),BS(IFILE),0)
C
        WRITE(*,'(A13,3I6)')' NX, NY, NZ: ',(NXYZ(I,IFILE),I=1,3)
        IF (MO(IFILE).EQ.0) WRITE(*,'(A18)')' MODE      : int*1'
        IF (MO(IFILE).EQ.1) WRITE(*,'(A18)')' MODE      : int*2'
        IF (MO(IFILE).EQ.2) WRITE(*,'(A17)')' MODE      : real'
        WRITE(*,'(A13,2G16.7)')' Min, max  : ',DMIN(IFILE),DMAX(IFILE)
        WRITE(*,'(A13,2G16.7)')' Mean, RMS : ',REAL(DDMEAN(IFILE)),
     +                                        REAL(DDRMS(IFILE))
C
        DO 30 I=1,NLABL(IFILE)
          WRITE(*,'(A8,I3,A2,A80)')' TITLE  ',I,': ',LABEL(I,IFILE)
30      CONTINUE
        WRITE(*,*)
C
        IF (NSYMBT.NE.0) CALL CREAD(SYM,HDR,NSYMBT,1,IFILE)
C
        BL=2*MO(IFILE)
        IF (BL.EQ.0) BL=1
        RL(IFILE)=NXYZ(1,IFILE)*BL
        OFF(IFILE)=HDR+NSYMBT
C
      ELSE
C
C     *** WRITE MRC/CCP4 ***
C
        WRITE(*,*)
        WRITE(*,*)'Opening MRC/CCP4 file for WRITE...'
        I=SLEN(CNAME)
        WRITE(*,*)'File      : '//CNAME(1:I)
C
        WRITE(*,'(A13,3I6)')' NX, NY, NZ: ',(NXYZ(I,IFILE),I=1,3)
        IF (MO(IFILE).EQ.0) WRITE(*,'(A18)')' MODE      : int*1'
        IF (MO(IFILE).EQ.1) WRITE(*,'(A18)')' MODE      : int*2'
        IF (MO(IFILE).EQ.2) WRITE(*,'(A17/)')' MODE      : real'
C
C       Symmetrie entry not used for now
        NSYMBT=0
C
        CALL SET_MHDR(BHDR,NXYZ(1,IFILE),MO(IFILE),0.0,
     +          0.0,PSZ(IFILE),0.0D0,0.0D0,NSYMBT,
     +          NLABL(IFILE),LABEL(1,IFILE),BS(IFILE),1)
C
        CALL COPEN(CNAME,IFILE,IOMODE(IFILE))
C
        CALL CWRITE(BHDR,0,HDR,1,IFILE)
C
        IF (NSYMBT.NE.0) CALL CWRITE(SYM,HDR,NSYMBT,1,IFILE)
C
        BL=2*MO(IFILE)
        IF (BL.EQ.0) BL=1
        RL(IFILE)=NXYZ(1,IFILE)*BL
        OFF(IFILE)=HDR+NSYMBT
C
      ENDIF
C
C**************************************************************************
C     SPIDER FORMAT
C**************************************************************************
C
      ELSEIF ((CF(IFILE).EQ.'S').OR.(CF(IFILE).EQ.'s')) THEN
C
      IF (IOMODE(IFILE).EQ.0) THEN
C
C     *** READ SPIDER ***
C
        WRITE(*,*)
        WRITE(*,*)'Opening SPIDER file for READ...'
        I=SLEN(CNAME)
        WRITE(*,*)'File      : '//CNAME(1:I)
C
        CALL COPEN(CNAME,IFILE,IOMODE(IFILE))
C
        CALL CREAD(BHDR,0,HDR,1,IFILE)
C
        CALL SET_SHDR(BHDR,NXYZ(1,IFILE),MO(IFILE),
     +              DMIN(IFILE),DMAX(IFILE),PSZ(IFILE),
     +              DDMEAN(IFILE),DDRMS(IFILE),NLABL(IFILE),
     +              LABEL(1,IFILE),LABBYT,BS(IFILE),0)
C
        RL(IFILE)=NXYZ(1,IFILE)*4
        OFF(IFILE)=LABBYT
C
        WRITE(*,'(A13,3I6)')' NX, NY, NZ: ',(NXYZ(I,IFILE),I=1,3)
        WRITE(*,'(A17)')' MODE      : real'
        WRITE(*,'(A13,2G16.7)')' Min, max  : ',DMIN(IFILE),DMAX(IFILE)
        WRITE(*,'(A13,2G16.7)')' Mean, RMS : ',REAL(DDMEAN(IFILE)),
     +                                        REAL(DDRMS(IFILE))
C
        DO 20 I=1,NLABL(IFILE)
          WRITE(*,'(A8,I3,A2,A80)')' TITLE  ',I,': ',LABEL(I,IFILE)
20      CONTINUE
        WRITE(*,*)
C
      ELSE
C
C     *** WRITE SPIDER ***
C
        WRITE(*,*)
        WRITE(*,*)'Opening SPIDER file for WRITE...'
        I=SLEN(CNAME)
        WRITE(*,*)'File      : '//CNAME(1:I)
C
        WRITE(*,'(A13,3I6)')' NX, NY, NZ: ',(NXYZ(I,IFILE),I=1,3)
        WRITE(*,'(A17/)')' MODE      : real'
C
        IF (MO(IFILE).NE.2)
     +    STOP ' ERROR: UNKNOWN MODE'
C
        CALL SET_SHDR(BHDR,NXYZ(1,IFILE),2,0.0,0.0,
     +          PSZ(IFILE),0.0D0,0.0D0,NLABL(IFILE),
     +          LABEL(1,IFILE),LABBYT,BS(IFILE),1)
C
        CALL COPEN(CNAME,IFILE,IOMODE(IFILE))
C
        CALL CWRITE(BHDR,0,HDR,1,IFILE)
C
        IF (LABBYT.GT.HDR) THEN
          DO 50 I=1,LABBYT-HDR
            CALL CWRITE(TMPBB,HDR+I,1,1,IFILE)
50        CONTINUE 
        ENDIF
C
        RL(IFILE)=NXYZ(1,IFILE)*4
        OFF(IFILE)=LABBYT
C
      ENDIF
C
      ELSE
C
        STOP ' ERROR: UNKNOWN FORMAT'
C
      ENDIF
C
      IF (IOMODE(IFILE).EQ.0) THEN
        NX=NXYZ(1,IFILE)
        NY=NXYZ(2,IFILE)
        NZ=NXYZ(3,IFILE)
        MODE=MO(IFILE)
        PSIZE=PSZ(IFILE)
        TITLE=LABEL(NLABL(IFILE),IFILE)
      ENDIF
C
      RETURN
      END
C
C**************************************************************************
      SUBROUTINE IREAD(IFILE,LINE,IREC)
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER*4 I,IFILE,IM,IREC
      PARAMETER (IM=200)
      INTEGER*4 NXYZ(3,IM),MO(IM),NLABL(IM)
      INTEGER*4 IOMODE(IM),OFF(IM),RL(IM),I2,I4
      REAL*4 DMIN(IM),DMAX(IM),PSZ(IM),RR,R
      REAL*8 DDRMS(IM),DDMEAN(IM)
      INTEGER*2 II(2)
      INTEGER*1 LINE(*),BB(4)
      CHARACTER LABEL(10,IM)*80,CF(IM),INAME(IM)*200
      LOGICAL BS(IM)
C
      EQUIVALENCE (BB,II,RR)
C
      COMMON /IODATA/NXYZ,MO,NLABL,IOMODE,OFF,RL,
     +       DMIN,DMAX,PSZ,DDRMS,DDMEAN,LABEL,CF,BS,INAME
C
      SAVE /IODATA/
C**************************************************************************
C
      IF ((CF(IFILE).EQ.'I').OR.(CF(IFILE).EQ.'i')) THEN
C
        CALL CREAD(LINE,OFF(IFILE),RL(IFILE),IREC,IFILE)
C
      ELSEIF ((CF(IFILE).EQ.'M').OR.(CF(IFILE).EQ.'m')) THEN
C
        CALL CREAD(LINE,OFF(IFILE),RL(IFILE),IREC,IFILE)
C
        IF (BS(IFILE)) CALL BYTESWAP(LINE,RL(IFILE))
C
      ELSEIF ((CF(IFILE).EQ.'S').OR.(CF(IFILE).EQ.'s')) THEN
C
        CALL CREAD(LINE,OFF(IFILE),RL(IFILE),IREC,IFILE)
C
        IF (BS(IFILE)) CALL BYTESWAP(LINE,RL(IFILE))
C
      ELSE
C
        STOP ' ERROR: UNKNOWN FORMAT'
C
      ENDIF
C
      IF (MO(IFILE).EQ.0) THEN
        DO 10 I=NXYZ(1,IFILE),1,-1
          RR=LINE(I)
          IF (RR.LT.0) RR=RR+256
          I4=I*4
          LINE(I4-3)=BB(1)
          LINE(I4-2)=BB(2)
          LINE(I4-1)=BB(3)
          LINE(I4)=BB(4)
10      CONTINUE
      ELSEIF (MO(IFILE).EQ.1) THEN
        DO 20 I=NXYZ(1,IFILE),1,-1
          I2=I*2
          BB(1)=LINE(I2-1)
          BB(2)=LINE(I2)
          R=II(1)
          RR=R
          I4=I*4
          LINE(I4-3)=BB(1)
          LINE(I4-2)=BB(2)
          LINE(I4-1)=BB(3)
          LINE(I4)=BB(4)
20      CONTINUE
      ENDIF
C
      RETURN
      END
C
C**************************************************************************
      SUBROUTINE IWRITE(IFILE,LINE,IREC)
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER*4 I,IFILE,IM,IREC,I4,I2
      PARAMETER (IM=200)
      INTEGER*4 NXYZ(3,IM),MO(IM),NLABL(IM)
      INTEGER*4 IOMODE(IM),OFF(IM),RL(IM)
      REAL*4 DMIN(IM),DMAX(IM),RR,PSZ(IM),R
      REAL*8 DDRMS(IM),DDMEAN(IM)
      INTEGER*2 II(2)
      INTEGER*1 LINE(*),BB(4)
      CHARACTER LABEL(10,IM)*80,CF(IM),INAME(IM)*200
      LOGICAL BS(IM)
C
      EQUIVALENCE (BB,II,RR)
C
      COMMON /IODATA/NXYZ,MO,NLABL,IOMODE,OFF,RL,
     +       DMIN,DMAX,PSZ,DDRMS,DDMEAN,LABEL,CF,BS,INAME
C
      SAVE /IODATA/
C**************************************************************************
C
      IF (MO(IFILE).EQ.0) THEN
        DO 10 I=1,NXYZ(1,IFILE)
          I4=I*4
          BB(1)=LINE(I4-3)
          BB(2)=LINE(I4-2)
          BB(3)=LINE(I4-1)
          BB(4)=LINE(I4)
          LINE(I)=RR
          DDMEAN(IFILE)=DDMEAN(IFILE)+RR
          DDRMS(IFILE)=DDRMS(IFILE)+RR**2
          IF (RR.GT.DMAX(IFILE)) DMAX(IFILE)=RR
          IF (RR.LT.DMIN(IFILE)) DMIN(IFILE)=RR
10      CONTINUE
      ELSEIF (MO(IFILE).EQ.1) THEN
        DO 20 I=1,NXYZ(1,IFILE)
          I4=I*4
          BB(1)=LINE(I4-3)
          BB(2)=LINE(I4-2)
          BB(3)=LINE(I4-1)
          BB(4)=LINE(I4)
          R=RR
          II(1)=R
          I2=I*2
          LINE(I2-1)=BB(1)
          LINE(I2)=BB(2)
          DDMEAN(IFILE)=DDMEAN(IFILE)+R
          DDRMS(IFILE)=DDRMS(IFILE)+R**2
          IF (RR.GT.DMAX(IFILE)) DMAX(IFILE)=R
          IF (RR.LT.DMIN(IFILE)) DMIN(IFILE)=R
20      CONTINUE
      ELSE
        DO 30 I=1,NXYZ(1,IFILE)
          I4=I*4
          BB(1)=LINE(I4-3)
          BB(2)=LINE(I4-2)
          BB(3)=LINE(I4-1)
          BB(4)=LINE(I4)
          DDMEAN(IFILE)=DDMEAN(IFILE)+RR
          DDRMS(IFILE)=DDRMS(IFILE)+RR**2
          IF (RR.GT.DMAX(IFILE)) DMAX(IFILE)=RR
          IF (RR.LT.DMIN(IFILE)) DMIN(IFILE)=RR
30      CONTINUE
      ENDIF
C
      IF ((CF(IFILE).EQ.'I').OR.(CF(IFILE).EQ.'i')) THEN
C
        CALL CWRITE(LINE,OFF(IFILE),RL(IFILE),IREC,IFILE)
C
      ELSEIF ((CF(IFILE).EQ.'M').OR.(CF(IFILE).EQ.'m')) THEN
C
        IF (BS(IFILE)) CALL BYTESWAP(LINE,RL(IFILE))
C
        CALL CWRITE(LINE,OFF(IFILE),RL(IFILE),IREC,IFILE)
C
        IF (BS(IFILE)) CALL BYTESWAP(LINE,RL(IFILE))
C
      ELSEIF ((CF(IFILE).EQ.'S').OR.(CF(IFILE).EQ.'s')) THEN
C
        IF (BS(IFILE)) CALL BYTESWAP(LINE,RL(IFILE))
C
        CALL CWRITE(LINE,OFF(IFILE),RL(IFILE),IREC,IFILE)
C
        IF (BS(IFILE)) CALL BYTESWAP(LINE,RL(IFILE))
C
      ELSE
C
        STOP ' ERROR: UNKNOWN FORMAT'
C
      ENDIF
C
      RETURN
      END
C
C**************************************************************************
      SUBROUTINE ICLOSE(IFILE)
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER*4 HDR,IM,I,SLEN
      PARAMETER (HDR=1024,IM=200)
      INTEGER*4 IFILE,NXYZ(3,IM),MO(IM),OFF(IM),RL(IM)
      INTEGER*4 NLABL(IM),IOMODE(IM),LABBYT,NSYMBT,IMN
      REAL*4 DMIN(IM),DMAX(IM),PSZ(IM)
      REAL*8 DDRMS(IM),DDMEAN(IM)
      INTEGER*1 BHDR(HDR),SYM(80)
      CHARACTER LABEL(10,IM)*80,CF(IM),INAME(IM)*200
      CHARACTER INAME1*200
      LOGICAL BS(IM)
C
      EQUIVALENCE (BHDR,IMN)
C
      COMMON /IODATA/NXYZ,MO,NLABL,IOMODE,OFF,RL,
     +       DMIN,DMAX,PSZ,DDRMS,DDMEAN,LABEL,CF,BS,INAME
C
      SAVE /IODATA/
C**************************************************************************
C
      IF (IOMODE(IFILE).EQ.1) THEN
C
        DDMEAN(IFILE)=DDMEAN(IFILE)/NXYZ(2,IFILE)/NXYZ(3,IFILE)
        DDMEAN(IFILE)=DDMEAN(IFILE)/NXYZ(1,IFILE)
        DDRMS(IFILE)=DDRMS(IFILE)/NXYZ(2,IFILE)/NXYZ(3,IFILE)
        DDRMS(IFILE)=DDRMS(IFILE)/NXYZ(1,IFILE)
        DDRMS(IFILE)=SQRT(DDRMS(IFILE)-DDMEAN(IFILE)**2)
C
      ENDIF
C
      IF (IOMODE(IFILE).EQ.0) THEN
C
        CALL CCLOSE(IFILE)
C
      ELSEIF ((CF(IFILE).EQ.'I').OR.(CF(IFILE).EQ.'i')) THEN
C
        CALL CCLOSE(IFILE)
C
        INAME1=INAME(IFILE)(1:SLEN(INAME(IFILE)))//'.hed'
        CALL COPEN(INAME1,IFILE,IOMODE(IFILE))
C
        CALL SET_IHDR(BHDR,NXYZ(1,IFILE),MO(IFILE),
     +                DMIN(IFILE),DMAX(IFILE),PSZ(IFILE),
     +                DDMEAN(IFILE),DDRMS(IFILE),
     +                NLABL(IFILE),LABEL(1,IFILE),1)
C
        DO 10 I=1,NXYZ(3,IFILE)
          IMN=I
          CALL CWRITE(BHDR,(I-1)*HDR,HDR,1,IFILE)
10      CONTINUE
C
        CALL CCLOSE(IFILE)
C
      ELSEIF ((CF(IFILE).EQ.'M').OR.(CF(IFILE).EQ.'m')) THEN
C
C       Symmetrie entry not used for now
        NSYMBT=0
C
        CALL SET_MHDR(BHDR,NXYZ(1,IFILE),MO(IFILE),
     +             DMIN(IFILE),DMAX(IFILE),PSZ(IFILE),
     +             DDMEAN(IFILE),DDRMS(IFILE),NSYMBT,
     +             NLABL(IFILE),LABEL(1,IFILE),BS(IFILE),1)
C
        IF (BS(IFILE)) CALL BYTESWAP(BHDR,HDR)
C
        CALL CWRITE(BHDR,0,HDR,1,IFILE)
C
        IF (NSYMBT.NE.0) CALL CWRITE(SYM,HDR,NSYMBT,1,IFILE)
C
        CALL CCLOSE(IFILE)
C
      ELSEIF ((CF(IFILE).EQ.'S').OR.(CF(IFILE).EQ.'s')) THEN
C
        CALL SET_SHDR(BHDR,NXYZ(1,IFILE),2,DMIN(IFILE),
     +             DMAX(IFILE),PSZ(IFILE),DDMEAN(IFILE),
     +             DDRMS(IFILE),NLABL(IFILE),
     +             LABEL(1,IFILE),LABBYT,BS(IFILE),1)
C
        IF (BS(IFILE)) CALL BYTESWAP(BHDR,HDR)
C
        CALL CWRITE(BHDR,0,HDR,1,IFILE)
C
        CALL CCLOSE(IFILE)
C
      ELSE
C
        STOP ' ERROR: UNKNOWN FORMAT'
C
      ENDIF
C
      RETURN
C
9999  CONTINUE
      STOP ' ERROR CLOSING FILE'
C
      END
C
C**************************************************************************
      SUBROUTINE BYTESWAP(BUF,HDR)
C**************************************************************************
      IMPLICIT NONE
C
      INTEGER*4 HDR,I,ID1,ID2,ID3,I4
      INTEGER*1 BUF(HDR),B
C
      DO 10 I=1,HDR/4
        I4=4*I
        ID1=I4-3
        ID2=I4-2
        ID3=I4-1
        B=BUF(ID1)
        BUF(ID1)=BUF(I4)
        BUF(I4)=B
        B=BUF(ID2)
        BUF(ID2)=BUF(ID3)
        BUF(ID3)=B
10    CONTINUE
C
      RETURN
      END
C
C**************************************************************************
      SUBROUTINE SET_IHDR(HDR1,NXYZ,MODE,DMIN,DMAX,PSIZE,
     +                  DDMEAN,DDRMS,NLABL,LABEL,K)
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER*4 I,K,HDR,IMN,IFOL,IERROR,NHFR,NMONTH,IZLP
      PARAMETER (HDR=1024)
      INTEGER*4 NXYZ(3),MODE,EXTRA1(7),EXTRA2(53)
      INTEGER*4 NLABL,NDAY,NYEAR,NHOUR,NMINUT,NXYZSTART(3)
      INTEGER*4 NSEC,NPIX2,NPIXEL,IXLP,IYLP,REALTYPE
      INTEGER*4 IXOLD,IYOLD,COMPLEX,MAPCRS(3),ISPG
      INTEGER*4 MXYZ(3),REALTYPE1,EXTRA3(74),NMON(12)
      REAL*4 PSIZE,DMIN,DMAX,AVDENS,SIGMA,VARIAN,OLDAVD
      REAL*4 DENSMAX,DENSMIN,CALPHA,CBETA,CGAMMA,CELLA(3)
      REAL*4 RESOLXYZ(3)
      REAL*8 DDMEAN,DDRMS
      INTEGER*1 HDR1(HDR),IHDR(HDR)
      CHARACTER LABEL(10)*80,NAME*80,HDAT*228,TYPE*4,FDAT*24
      CHARACTER MONTH(12)*3
C
      EQUIVALENCE (IHDR(1)  ,IMN)
      EQUIVALENCE (IHDR(5)  ,IFOL)
      EQUIVALENCE (IHDR(9)  ,IERROR)
      EQUIVALENCE (IHDR(13) ,NHFR)
      EQUIVALENCE (IHDR(17) ,NMONTH)
      EQUIVALENCE (IHDR(21) ,NDAY)
      EQUIVALENCE (IHDR(25) ,NYEAR)
      EQUIVALENCE (IHDR(29) ,NHOUR)
      EQUIVALENCE (IHDR(33) ,NMINUT)
      EQUIVALENCE (IHDR(37) ,NSEC)
      EQUIVALENCE (IHDR(41) ,NPIX2)
      EQUIVALENCE (IHDR(45) ,NPIXEL)
      EQUIVALENCE (IHDR(49) ,IXLP)
      EQUIVALENCE (IHDR(53) ,IYLP)
      EQUIVALENCE (IHDR(57) ,TYPE)
      EQUIVALENCE (IHDR(61) ,IXOLD)
      EQUIVALENCE (IHDR(65) ,IYOLD)
      EQUIVALENCE (IHDR(69) ,AVDENS)
      EQUIVALENCE (IHDR(73) ,SIGMA)
      EQUIVALENCE (IHDR(77) ,VARIAN)
      EQUIVALENCE (IHDR(81) ,OLDAVD)
      EQUIVALENCE (IHDR(85) ,DENSMAX)
      EQUIVALENCE (IHDR(89) ,DENSMIN)
      EQUIVALENCE (IHDR(93) ,COMPLEX)
      EQUIVALENCE (IHDR(97) ,CELLA)
      EQUIVALENCE (IHDR(109),CALPHA)
      EQUIVALENCE (IHDR(113),CBETA)
      EQUIVALENCE (IHDR(117),NAME)
      EQUIVALENCE (IHDR(197),CGAMMA)
      EQUIVALENCE (IHDR(201),MAPCRS)
      EQUIVALENCE (IHDR(213),ISPG)
      EQUIVALENCE (IHDR(217),NXYZSTART)
      EQUIVALENCE (IHDR(229),MXYZ)
      EQUIVALENCE (IHDR(241),IZLP)
      EQUIVALENCE (IHDR(245),EXTRA1)
      EQUIVALENCE (IHDR(273),REALTYPE)
      EQUIVALENCE (IHDR(277),EXTRA2)
      EQUIVALENCE (IHDR(489),RESOLXYZ)
      EQUIVALENCE (IHDR(501),EXTRA3)
      EQUIVALENCE (IHDR(797),HDAT)
C
      DATA MONTH/'Jan','Feb','Mar','Apr','May','Jun',
     +           'Jul','Aug','Sep','Oct','Nov','Dec'/
      DATA NMON /1,2,3,4,5,6,7,8,9,10,11,12/
C**************************************************************************
C
      IF (K.EQ.0) THEN
C
C     Get parameters from IHDR
C
        DO 30 I=1,HDR
          IHDR(I)=HDR1(I)
30      CONTINUE
C
        CALL CHKEND2(REALTYPE1)
        IF (REALTYPE1.NE.REALTYPE)
     +    STOP ' ARCHITECTURE INCOMPATIBILITY'
C
        IF ((MODE.LT.0).OR.(MODE.GT.2))
     +    STOP ' ERROR: DATA FORMAT NOT SUPPORTED'
C
        NXYZ(1)=IYLP
        NXYZ(2)=IXLP
        NXYZ(3)=IZLP*(IFOL+1)
        IF (IZLP.EQ.IFOL+1) NXYZ(3)=IFOL+1
        IF (IZLP.EQ.0) NXYZ(3)=IFOL+1
        IF (TYPE.EQ.'PACK') MODE=0
        IF (TYPE.EQ.'INTG') MODE=1
        IF (TYPE.EQ.'REAL') MODE=2
        DMIN=DENSMIN
        DMAX=DENSMAX
        PSIZE=CELLA(1)/IXLP
        DDMEAN=DBLE(AVDENS)
        DDRMS=DBLE(SIGMA)
        NLABL=1
        DO 10 I=1,NLABL
          LABEL(I)=NAME
10      CONTINUE
        DO 20 I=NLABL+1,10
          LABEL(I)=""
20      CONTINUE
C
      ELSE
C
C     Put parameters into IHDR
C
        IF ((MODE.LT.0).OR.(MODE.GT.2))
     +    STOP ' ERROR: DATA FORMAT NOT SUPPORTED'
C
        IYLP=NXYZ(1)
        IXLP=NXYZ(2)
        IZLP=NXYZ(3)
        IFOL=IZLP-1
        NPIX2=IYLP*IXLP
        NPIXEL=NPIX2
        IF (MODE.EQ.0) TYPE='PACK'
        IF (MODE.EQ.1) TYPE='INTG'
        IF (MODE.EQ.2) TYPE='REAL'
        DENSMIN=DMIN
        DENSMAX=DMAX
        AVDENS=REAL(DDMEAN)
        SIGMA=REAL(DDRMS)
        VARIAN=REAL(DDRMS**2)
        NAME=LABEL(1)
C
        IMN=1
        IERROR=0
        NHFR=1
        CALL FDATE(FDAT)
        DO 80 I=1,12
          IF (MONTH(I).EQ.FDAT(5:7)) NMONTH=I
80      CONTINUE
        READ(FDAT,'(8X,I2)')NDAY
        READ(FDAT,'(20X,I4)')NYEAR
        READ(FDAT,'(11X,I2)')NHOUR
        READ(FDAT,'(14X,I2)')NMINUT
        READ(FDAT,'(17X,I2)')NSEC
        IXOLD=0
        IYOLD=0
        COMPLEX=0
        NXYZSTART(1)=0
        NXYZSTART(2)=0
        NXYZSTART(3)=0
        MXYZ(1)=IYLP
        MXYZ(2)=IXLP
        MXYZ(3)=IZLP
        CELLA(1)=PSIZE*IYLP
        CELLA(2)=PSIZE*IXLP
        CELLA(3)=PSIZE*IZLP
        CALPHA=90.0
        CBETA=90.0
        CGAMMA=90.0
        MAPCRS(1)=1
        MAPCRS(2)=2
        MAPCRS(3)=3
        ISPG=0
        DO 40 I=1,7
          EXTRA1(I)=0
40      CONTINUE
        DO 50 I=1,53
          EXTRA2(I)=0
50      CONTINUE
        DO 60 I=1,74
          EXTRA3(I)=0
60      CONTINUE
        RESOLXYZ(1)=PSIZE
        RESOLXYZ(2)=PSIZE
        RESOLXYZ(3)=PSIZE
        HDAT=""
C       Machine stamp
        CALL CHKEND2(REALTYPE)
C
        DO 70 I=1,HDR
          HDR1(I)=IHDR(I)
70      CONTINUE
C
      ENDIF
C
      RETURN
      END
C
C**************************************************************************
      SUBROUTINE SET_MHDR(HDR1,NXYZ,MODE,DMIN,DMAX,PSIZE,
     +              DDMEAN,DDRMS,NSYMBT,NLABL,LABEL,BS,K)
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER*4 MXYZ(3),MAPCRS(3),I,K,I1(3),I13,HDR
      PARAMETER (HDR=1024)
      INTEGER*4 NXYZ(3),NXYZSTART(3),I93,I221,MODE
      INTEGER*4 ISPG,NSYMBT,EXTRA(25),NLABL
      REAL*4 CELLA(3),CELLB(3),R77,R81,R85,PSIZE
      REAL*4 ORIGIN(3),DMIN,DMAX,R217
      REAL*8 DDMEAN,DDRMS
      INTEGER*1 HDR1(HDR),MHDR(HDR),MACHST(4),MACHST1(4)
      CHARACTER MAP*4,LABEL(10)*80,C225(10)*80
      LOGICAL BS
C
      EQUIVALENCE (MHDR(1)  ,I1)
      EQUIVALENCE (MHDR(13) ,I13)
      EQUIVALENCE (MHDR(17) ,NXYZSTART)
      EQUIVALENCE (MHDR(29) ,MXYZ)
      EQUIVALENCE (MHDR(41) ,CELLA)
      EQUIVALENCE (MHDR(53) ,CELLB)
      EQUIVALENCE (MHDR(65) ,MAPCRS)
      EQUIVALENCE (MHDR(77) ,R77)
      EQUIVALENCE (MHDR(81) ,R81)
      EQUIVALENCE (MHDR(85) ,R85)
      EQUIVALENCE (MHDR(89) ,ISPG)
      EQUIVALENCE (MHDR(93) ,I93)
      EQUIVALENCE (MHDR(97) ,EXTRA)
      EQUIVALENCE (MHDR(197),ORIGIN)
      EQUIVALENCE (MHDR(209),MAP)
      EQUIVALENCE (MHDR(213),MACHST)
      EQUIVALENCE (MHDR(217),R217)
      EQUIVALENCE (MHDR(221),I221)
      EQUIVALENCE (MHDR(225),C225)
C**************************************************************************
C
      IF (K.EQ.0) THEN
C
C     Get parameters from MHDR
C
        DO 30 I=1,HDR
          MHDR(I)=HDR1(I)
30      CONTINUE
C
        CALL CHKEND(MACHST1)
        IF (MACHST1(1).NE.MACHST(1)) THEN
          IF ((MACHST(1).EQ.17).AND.
     +        (MACHST1(1).EQ.68)) THEN
            CALL BYTESWAP(MHDR,HDR)
            WRITE(*,*)'Non-native byte order'
            BS=.TRUE.
          ELSEIF ((MACHST1(1).EQ.17).AND.
     +        (MACHST(1).EQ.68)) THEN
            CALL BYTESWAP(MHDR,HDR)
            WRITE(*,*)'Non-native byte order'
            BS=.TRUE.
          ELSEIF (MACHST(1).EQ.0) THEN
            WRITE(*,*)' WARNING: NO MACHINE STAMP'
          ELSE
            STOP ' ARCHITECTURE INCOMPATIBILITY'
          ENDIF
        ENDIF
C
        IF ((MODE.LT.0).OR.(MODE.GT.2))
     +    STOP ' ERROR: DATA FORMAT NOT SUPPORTED'
C
        NXYZ(1)=I1(1)
        NXYZ(2)=I1(2)
        NXYZ(3)=I1(3)
        MODE=I13
        DMIN=R77
        DMAX=R81
        PSIZE=CELLA(1)/I1(1)
        DDMEAN=DBLE(R85)
        DDRMS=DBLE(R217)
        NSYMBT=I93
        NLABL=I221
        DO 10 I=1,NLABL
          LABEL(I)=C225(I)
10      CONTINUE
        DO 20 I=NLABL+1,10
          LABEL(I)=""
20      CONTINUE
C
      ELSE
C
C     Put parameters into MHDR
C
        IF ((MODE.LT.0).OR.(MODE.GT.2))
     +    STOP ' ERROR: DATA FORMAT NOT SUPPORTED'
C
        I1(1)=NXYZ(1)
        I1(2)=NXYZ(2)
        I1(3)=NXYZ(3)
        I13=MODE
        R77=DMIN
        R81=DMAX
        R85=REAL(DDMEAN)
        R217=REAL(DDRMS)
        I93=NSYMBT
        I221=NLABL
        DO 40 I=1,NLABL
          C225(I)=LABEL(I)
40      CONTINUE
        DO 60 I=NLABL+1,10
          C225(I)=""
60      CONTINUE
C
        NXYZSTART(1)=0
        NXYZSTART(2)=0
        NXYZSTART(3)=0
        MXYZ(1)=I1(1)
        MXYZ(2)=I1(2)
        MXYZ(3)=I1(3)
        CELLA(1)=PSIZE*I1(1)
        CELLA(2)=PSIZE*I1(2)
        CELLA(3)=PSIZE*I1(3)
        CELLB(1)=90.0
        CELLB(2)=90.0
        CELLB(3)=90.0
        MAPCRS(1)=1
        MAPCRS(2)=2
        MAPCRS(3)=3
        ISPG=0
        DO 50 I=1,25
          EXTRA(I)=0
50      CONTINUE
        ORIGIN(1)=0.0
        ORIGIN(2)=0.0
        ORIGIN(3)=0.0
        MAP='MAP '
C       Machine stamp
        CALL CHKEND(MACHST)
C
        IF (BS) THEN
          IF ((MACHST(1).EQ.17).AND.
     +        (MACHST(2).EQ.17)) THEN
            MACHST(1)=68
            MACHST(2)=65
          ELSEIF ((MACHST(1).EQ.68).AND.
     +        (MACHST(2).EQ.65)) THEN
            MACHST(1)=17
            MACHST(2)=17
          ENDIF
          CALL BYTESWAP(MHDR,HDR)
        ENDIF
C
        DO 70 I=1,HDR
          HDR1(I)=MHDR(I)
70      CONTINUE
C
      ENDIF
C
      RETURN
      END
C
C**************************************************************************
      SUBROUTINE SET_SHDR(HDR1,NXYZ,MODE,DMIN,DMAX,
     +                PSIZE,DDMEAN,DDRMS,NLABL,LABEL,
     +                LABBYT,BS,K)
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER*4 NXYZ(3),LABBYT,LENBYT,LABREC,I,K,MODE
      INTEGER*4 NLABL,HDR
      PARAMETER (HDR=1024)
      REAL*4 RMAMI,RLABREC,RANGLE,RLABBYT,RLENBYT
      REAL*4 RSTACK,RMAXIM,RMGNUM,RMODE,RLASTINDX,RANGLE2
      REAL*4 PARA(7),ANG(6),JM(174),RREC,PSIZE
      REAL*4 R1,R5,RMAX,RMIN,RMEAN,RRMS,R45,R161,R385
      REAL*4 R449,R465,DMIN,DMAX
      REAL*8 DDMEAN,DDRMS
      INTEGER*1 HDR1(HDR),SHDR(HDR)
      CHARACTER LABEL(10)*80
      CHARACTER CDAT*11,CTIM*8,CTIT*160,FDAT*24
      LOGICAL BS
C
      EQUIVALENCE (SHDR(1)  ,R1)
      EQUIVALENCE (SHDR(5)  ,R5)
      EQUIVALENCE (SHDR(9)  ,RREC)
      EQUIVALENCE (SHDR(17) ,RMODE)
      EQUIVALENCE (SHDR(21) ,RMAMI)
      EQUIVALENCE (SHDR(25) ,RMAX)
      EQUIVALENCE (SHDR(29) ,RMIN)
      EQUIVALENCE (SHDR(33) ,RMEAN)
      EQUIVALENCE (SHDR(37) ,RRMS)
      EQUIVALENCE (SHDR(45) ,R45)
      EQUIVALENCE (SHDR(49) ,RLABREC)
      EQUIVALENCE (SHDR(53) ,RANGLE)
      EQUIVALENCE (SHDR(57) ,PARA)
      EQUIVALENCE (SHDR(85) ,RLABBYT)
      EQUIVALENCE (SHDR(89) ,RLENBYT)
      EQUIVALENCE (SHDR(93) ,RSTACK)
      EQUIVALENCE (SHDR(101),RMAXIM)
      EQUIVALENCE (SHDR(105),RMGNUM)
      EQUIVALENCE (SHDR(109),RLASTINDX)
      EQUIVALENCE (SHDR(121),RANGLE2)
      EQUIVALENCE (SHDR(125),ANG)
      EQUIVALENCE (SHDR(129),JM)
      EQUIVALENCE (SHDR(161),R161)
      EQUIVALENCE (SHDR(385),R385)
      EQUIVALENCE (SHDR(449),R449)
      EQUIVALENCE (SHDR(465),R465)
      EQUIVALENCE (SHDR(845),CDAT)
      EQUIVALENCE (SHDR(857),CTIM)
      EQUIVALENCE (SHDR(865),CTIT)
C**************************************************************************
C
      IF (K.EQ.0) THEN
C
C     Get parameters from SHDR
C
        DO 30 I=1,HDR
          SHDR(I)=HDR1(I)
30      CONTINUE
C
        IF ((RMODE.NE.1.0).AND.(RMODE.NE.3.0)
     +      .AND.(RMODE.NE.-11.).AND.(RMODE.NE.-12.0)
     +      .AND.(RMODE.NE.-21.).AND.(RMODE.NE.-22.0)) THEN
          CALL BYTESWAP(SHDR,HDR)
          IF ((RMODE.NE.1.0).AND.(RMODE.NE.3.0))
     +      STOP ' ERROR: DATA FORMAT NOT SUPPORTED'
C
          WRITE(*,*)'Non-native byte order'
          BS=.TRUE.
        ELSEIF ((RMODE.NE.1.0).AND.(RMODE.NE.3.0)) THEN
          STOP ' ERROR: DATA FORMAT NOT SUPPORTED'
        ENDIF
C
        IF (RSTACK.NE.0)
     +    STOP ' ERROR: STACK MUST BE A SIMPLE 3D FILE'
C
        NXYZ(1)=R45
        NXYZ(2)=R5
        NXYZ(3)=R1
        MODE=2
        DMIN=RMIN
        DMAX=RMAX
        PSIZE=1.0
        DDMEAN=DBLE(RMEAN)
        DDRMS=DBLE(RRMS)
        LABBYT=RLABBYT
        NLABL=3
        LABEL(1)=CTIT(1:80)
        LABEL(2)=CTIT(81:160)
C 1234567890123456789012345678901
C CREATED 01-DEC-2010 AT 08:54:08
        LABEL(3)(1:8)="CREATED "
        LABEL(3)(9:19)=CDAT
        LABEL(3)(20:23)=" AT "
        LABEL(3)(24:31)=CTIM
        DO 10 I=NLABL+1,10
          LABEL(I)=""
10      CONTINUE
C
      ELSE
C
C     Put parameters into SHDR
C
        IF (MODE.NE.2)
     +    STOP ' ERROR: DATA FORMAT NOT SUPPORTED'
C
        R45=NXYZ(1)
        R5=NXYZ(2)
        R1=NXYZ(3)
        RMIN=DMIN
        RMAX=DMAX
        RMEAN=REAL(DDMEAN)
        RRMS=REAL(DDRMS)
        R161=0.0
        R385=0.0
        R449=0.0
        R465=0.0
        LENBYT=NXYZ(1)*4
        LABREC=1024/LENBYT
        IF (MOD(1024,LENBYT).NE.0) LABREC = LABREC+1
        LABBYT=LABREC*LENBYT
        RLENBYT=LENBYT
        RLABREC=LABREC
        RLABBYT=LABBYT
        CTIT=""
        IF (NLABL.GT.0) CTIT(1:80)=LABEL(1)
        IF (NLABL.GT.1) CTIT(81:160)=LABEL(2)
        RMODE=3.0
        IF (NXYZ(3).EQ.1) RMODE=1.0
        DO 100 I=1,174
          JM(I)=0.0
100     CONTINUE
        CALL FDATE(FDAT)
        CDAT(1:11)=FDAT(9:10)//'-'//FDAT(5:7)//'-'//FDAT(21:24)
        IF (CDAT(1:1).EQ." ") CDAT(1:1)="0"
        CTIM=FDAT(12:19)
        RMAMI=0.0
        DO 110 I=1,6
          PARA(I)=0.0
          ANG(I)=0.0
110     CONTINUE
        PARA(7)=0.0
        RANGLE=0.0
        RSTACK=0.0
        RMAXIM=0.0
        RMGNUM=0.0
        RLASTINDX=0.0
        RANGLE2=0.0
        RMAMI=1.0
C
        IF (BS) CALL BYTESWAP(SHDR,HDR)
C
        DO 70 I=1,HDR
          HDR1(I)=SHDR(I)
70      CONTINUE
C
      ENDIF
C
      RETURN
      END
C
C**************************************************************************
      SUBROUTINE CHKEND(MACHST)
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER*4 I,A0,A1,A2,A3
      INTEGER*1 MACHST(4)
      PARAMETER (A0=48,A1=49,A2=50,A3=51)
C
      COMMON /ENDIAN/I
C**************************************************************************
C
      I=A0+A1*256+A2*(256**2)+A3*(256**3)
      CALL TESTE(MACHST)
      END
C
C**************************************************************************
      SUBROUTINE TESTE(MACHST)
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER*1 MACHST(4)
      CHARACTER I*4
C
      COMMON /ENDIAN/I
C**************************************************************************
C
      IF (I.EQ.'0123') THEN
C       Machine is Little-Endian (DEC/OSF, Intel, AMD ...)
C       0100 0100
        MACHST(1)=68
C       0100 0001
        MACHST(2)=65
        MACHST(3)=0
        MACHST(4)=0
      ELSEIF (I.EQ.'3210') THEN
C       Machine is Big-Endian (SGI, SUN, HP, IBM)
C       0001 0001
        MACHST(1)=17
C       0001 0001
        MACHST(2)=17
        MACHST(3)=0
        MACHST(4)=0
      ELSE
C       Mixed endianity machine (VAX)
C       0010 0010
        MACHST(1)=34
C       0010 0001
        MACHST(2)=33
        MACHST(3)=0
        MACHST(4)=0
      ENDIF
C
      RETURN
      END
C
C**************************************************************************
      SUBROUTINE CHKEND2(REALTYPE)
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER*4 I,A0,A1,A2,A3,REALTYPE
      PARAMETER (A0=48,A1=49,A2=50,A3=51)
C
      COMMON /ENDIAN/I
C**************************************************************************
C
      I=A0+A1*256+A2*(256**2)+A3*(256**3)
      CALL TESTE2(REALTYPE)
      END
C
C**************************************************************************
      SUBROUTINE TESTE2(REALTYPE)
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER*4 REALTYPE
      CHARACTER I*4
C
      COMMON /ENDIAN/I
C**************************************************************************
C
      IF (I.EQ.'0123') THEN
C       Machine is Little-Endian (DEC/OSF, Intel, AMD ...)
        REALTYPE=33686018
      ELSEIF (I.EQ.'3210') THEN
C       Machine is Big-Endian (SGI, SUN, HP, IBM)
        REALTYPE=67372036
      ELSE
C       Mixed endianity machine (VAX)
        REALTYPE=16777216
      ENDIF
C
      RETURN
      END
C
C**************************************************************************
      INTEGER*4 FUNCTION SLEN(STRG)
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER*4 I
      CHARACTER STRG*200
C**************************************************************************
C
      DO 10 I=1,200
        IF (STRG(I:I).EQ." ") GOTO 20
10    CONTINUE
C
20    CONTINUE
      SLEN=I-1
C
      RETURN
      END

