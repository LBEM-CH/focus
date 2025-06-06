C*****************************************************************************
C
C       CTFFIND3 - determines defocus and astigmatism for images of
C       arbitrary size (MRC format). Astigmatic angle is measured form
C       x axis (same convetiones as in the MRC 2D image processing 
C       programs).
C
C       CARD 1: Input file name for image
C       CARD 2: Output file name to check result
C       CARD 3: CS[mm], HT[kV], AmpCnst, XMAG, DStep[um]
C       CARD 4: Box, ResMin[A], ResMax[A], dFMin[A], dFMax[A], FStep[A], dAst[A]
CHEN>
C       CARD 5: INOAST, RefDef1, RefDef2, RefDefAng
CHEN<
C
C               The output image file to check the result of the fitting
C               shows the filtered average power spectrum of the input
C               image in one half, and the fitted CTF (squared) in the
C               other half. The two halfs should agree very well for a
C               sucessfull fit.
C
C               CS: Spherical aberration coefficient of the objective in mm
C               HT: Electron beam voltage in kV
C               AmpCnst: Amount of amplitude contrast (fraction). For ice
C                        images 0.07, for negative stain about 0.15.
C               XMAG: Magnification of original image
C               DStep: Pixel size on scanner in microns
C               Box: Tile size. The program devides the image into square
C                    tiles and calculates the average power spectrum. Tiles
C                    with a significatly higher or lower variance are 
C                    excluded; these are parts of the image which are unlikely
C                    to contain useful information (beam edge, film number 
C                    etc). IMPORTANT: Box must have even pixel dimensions.
C               ResMin: Low resolution end of data to be fitted.
C               ResMaX: High resolution end of data to be fitted.
C               dFMin: Starting defocus value for grid search in Angstrom. 
C                      Positive values represent an underfocus. The program
C                      performs a systematic grid search of defocus values 
C                      and astigmatism before fitting a CTF to machine 
C                      precision.
C               dFMax: End defocus value for grid search in Angstrom.
C               FStep: Step width for grid search in Angstrom.
C               dAst: Expected amount of astigmatism in Angstrom.
CHEN>
C               INOAST: If 0, then normal mode.
C                       If 1, then only one-dimensional search without astig.
C               RefDef1: Reference defocus value for one-dimensional search
C               RefDef2 
C               RefDefAng 
CHEN<
C
C*****************************************************************************
C       example command file (UNIX):
C
C       #!/bin/tcsh -x
C       #
C       #   ctffind3
C       #
C       time /public/image/bin/ctffind3.exe << eof
C       image.mrc
C       power.mrc
C       2.6,200.0,0.07,60000.0,28.0
C       128,100.0,15.0,30000.0,90000.0,5000.0,500.0,100.0
CHEN>
C       0,1234.0,1234.0,0.0
CHEN<
C       eof
C       #
C*****************************************************************************
C
      PROGRAM CTFFIND3
C
C       NIKO, 21 SEPTEMBER 2002
C       with 2dx additions, Oct. 31, 2013, Henning.
C
      IMPLICIT NONE
C
      INTEGER NXYZ(3),MODE,JXYZ(3),I,J,NX,NY,IX,IS,KXYZ(3)
      INTEGER K,CNT,ID,L,M,LL,MM,ITEST,IP,NBIN,IMP,IERR
      INTEGER OMP_GET_NUM_PROCS
CHEN>
      INTEGER INOAST
      REAL DEFREF(3),DRMSH
CHEN<
      PARAMETER (NBIN=100)
      REAL DMAX,DMEAN,DRMS,RMS,WGH1,WGH2,RMSMIN
      REAL SCAL,MEAN,WL,PI,MIN,MAX,ANGAST
      REAL CS,KV,WGH,XMAG,DSTEP,RESMIN,RESMAX,DFMID1,DFMID2
      REAL THETATR,STEPR,RMIN2,RMAX2,HW,RMSMAX,DAST
      REAL RES2,CTF,CTFV,TMP,FLT,DFMIN,DFMAX,FSTEP,DRMS1,CMAX
      REAL,ALLOCATABLE :: AIN(:),ABOX(:),POWER(:),OUT(:),BUF1(:)
      REAL,ALLOCATABLE :: RMSA(:),BINS(:)
      PARAMETER (FLT=-0.1)
      PARAMETER (PI=3.1415926535898)
      COMPLEX,ALLOCATABLE :: CBOXS(:)
      CHARACTER FILEIN*200,FILEOUT*200,TITLE*1600,CFORM,NCPUS*10
      LOGICAL EX
C
      WRITE(6,1000)
1000  FORMAT(/' CTF DETERMINATION, V3.5 (9-Mar-2013)',
     +       /' Distributed under the GNU',
     +        ' General Public License (GPL)')
CHEN>
      WRITE(6,'(''This version was adapted to the 2dx environment.'')')
CHEN<
      IMP=0
CHEN>
#ifdef _OPENMP
CHEN<
      CALL GETENV('OMP_NUM_THREADS',NCPUS)
      READ(NCPUS,*,ERR=111,END=111)IMP
111   CONTINUE
      IF (IMP.LE.0) THEN
        CALL GETENV('NCPUS',NCPUS)
        READ(NCPUS,*,ERR=112,END=112)IMP
112     CONTINUE
      ENDIF
CHEN>
      IF (IMP.LE.0) THEN
        IMP=OMP_GET_NUM_PROCS()
      ENDIF
      CALL OMP_SET_NUM_THREADS(IMP)
#endif
CHEN<
      IF (IMP.LE.0) IMP=1
C
      IF (IMP.GT.1) THEN
        WRITE(*,7000) IMP
7000    FORMAT(/' Parallel processing: NCPUS =   ',I8)
      ENDIF
C
      WRITE(6,1002)
1002  FORMAT(/' Input image file name')
      READ(5,1010)FILEIN
1010  FORMAT(A)
      WRITE(6,1010)FILEIN
      WRITE(6,1060)
1060  FORMAT(/' Output diagnostic file name')
      READ(5,1010)FILEOUT
      WRITE(6,1010)FILEOUT
      WRITE(6,1020)
1020  FORMAT(/' CS[mm], HT[kV], AmpCnst, XMAG, DStep[um]')
      READ(5,*)CS,KV,WGH,XMAG,DSTEP
      WRITE(6,1031)CS,KV,WGH,XMAG,DSTEP
1031  FORMAT(F5.1,F9.1,F8.2,F10.1,F9.3)
      WRITE(6,1040)
1040  FORMAT(/' Positive defocus values for underfocus',
     +       /' Box, ResMin[A], ResMax[A], dFMin[A], dFMax[A],',
     +        ' FStep[A], dAst[A]')
      READ(5,*,END=98)JXYZ(1),RESMIN,RESMAX,DFMIN,DFMAX,FSTEP,DAST
      WRITE(6,1051)JXYZ(1),RESMIN,RESMAX,DFMIN,DFMAX,FSTEP,DAST
1051  FORMAT(I4,2F11.1,2F10.1,2F7.1/)
      GOTO 99
C
98    CONTINUE
      WRITE(6,*) ' ERROR reading CARD 4. Trying old CARD 4...'
C      READ(5,*)JXYZ(1),RESMIN,RESMAX,DFMIN,DFMAX,FSTEP
      WRITE(6,1052)JXYZ(1),RESMIN,RESMAX,DFMIN,DFMAX,FSTEP
1052  FORMAT(I4,2F11.1,2F10.1,F7.1/)
      WRITE(6,*) ' Setting DAST = 100.0'
      DAST=100.0
C
99    CONTINUE
      ITEST=JXYZ(1)/2
      IF (2*ITEST.NE.JXYZ(1)) THEN
        WRITE(6,1090)
1090    FORMAT(/' Box size must be even number')
        STOP
      ENDIF
      IF (DAST.LT.0.0) THEN
        DAST=500.0
        WRITE(6,1120)
1120    FORMAT(/' Invalid dAst value; reset to 500.0')
      ENDIF
      JXYZ(2)=JXYZ(1)
      JXYZ(3)=1
C
CHEN>
      READ(5,*)INOAST,DEFREF(1),DEFREF(2),DEFREF(3),DRMSH
      WRITE(6,'(''INOAST = '',I4)')INOAST
      WRITE(6,'(''DEFREF = '',3F12.1)')(DEFREF(i),i=1,3)
CHEN<
C       Make sure RESMIN is larger than RESMAX
C
      IF (RESMIN.LT.RESMAX) THEN
        TMP=RESMAX
        RESMAX=RESMIN
        RESMIN=TMP
      ENDIF
C
C       Same for DFMIN, DFMAX
C
      IF (DFMAX.LT.DFMIN) THEN
        TMP=DFMAX
        DFMAX=DFMIN
        DFMIN=TMP
      ENDIF
C
      CALL GUESSF(FILEIN,CFORM,EX)
      IF  (.NOT.EX) THEN
        WRITE(*,1001) FILEIN
1001    FORMAT(' File not found ',A80)
        STOP
      ENDIF
      CALL IOPEN(FILEIN,10,CFORM,MODE,NXYZ(1),NXYZ(2),
     +           NXYZ(3),'OLD',STEPR,TITLE)
C
      WRITE(*,1110)NXYZ(1),NXYZ(2)
1110  FORMAT(/,' READING IMAGE...'/' NX, NY= ',2I10)
C
      NX=NXYZ(1)/JXYZ(1)
      NY=NXYZ(2)/JXYZ(2)
C
      ALLOCATE(AIN(NXYZ(1)*JXYZ(2)),ABOX(JXYZ(1)*JXYZ(2)),
     +  OUT(JXYZ(1)*JXYZ(2)),
     +  BUF1(JXYZ(1)*JXYZ(2)),RMSA(NX*NY),BINS(NBIN),
     +  CBOXS(JXYZ(2)),STAT=IERR)
      IF (IERR.NE.0) THEN
        WRITE(*,*) ' ERROR: Memory allocation failed in MAIN'
        STOP ' Try reducing the tile size'
      ENDIF
C
      DRMS=0.0D0
      CNT=0
      DO 10 I=1,NY
        IP=(I-1)*JXYZ(2)
        DO 21 J=1,JXYZ(2)
          ID=1+NXYZ(1)*(J-1)
          CALL IREAD(10,AIN(ID),J+IP)
21      CONTINUE
        DO 10 J=1,NX
          IX=(J-1)*JXYZ(1)+1
          CALL BOXIMG(AIN,NXYZ,ABOX,JXYZ,IX,1,MEAN,RMS)
          DRMS=DRMS+RMS**2
          CNT=CNT+1
          IF (CNT.LE.50000) RMSA(CNT)=RMS
10    CONTINUE
      DRMS=SQRT(DRMS/CNT)
      CALL HISTO(CNT,NBIN,RMSA,BINS,MIN,MAX)
      RMSMIN=MIN
      RMSMAX=MAX
      CMAX=0
      DO 70 I=1,NBIN
        IF (BINS(I).GT.CMAX) THEN
          CMAX=BINS(I)
          J=I
        ENDIF
70    CONTINUE
      IF (J.GT.1) THEN
        DO 71 I=1,J-1
          IF (BINS(I).GE.CMAX/10.0) THEN
            RMSMIN=I*(MAX-MIN)/(NBIN-1)+MIN
            GOTO 72
          ENDIF
71      CONTINUE
      ENDIF
72    CONTINUE
      IF (J.LT.NBIN) THEN
        DO 73 I=NBIN,J+1,-1
          IF (BINS(I).GE.CMAX/10.0) THEN
            RMSMAX=I*(MAX-MIN)/(NBIN-1)+MIN
            GOTO 74
          ENDIF
73      CONTINUE
      ENDIF
74    CONTINUE
C
      KXYZ(1)=JXYZ(1)/2
      KXYZ(2)=JXYZ(2)
      KXYZ(3)=JXYZ(3)
      IF (2*(KXYZ(1)/2).NE.KXYZ(1)) KXYZ(1)=KXYZ(1)+1
      ALLOCATE(POWER(KXYZ(1)*KXYZ(2)),STAT=IERR)
      IF (IERR.NE.0) THEN
        WRITE(*,*) ' ERROR: Memory allocation failed for POWER'
        STOP ' Try reducing the tile size'
      ENDIF
      DO 30 K=1,KXYZ(1)*KXYZ(2)
        POWER(K)=0.0
30    CONTINUE
      SCAL=1.0/SQRT(REAL(JXYZ(1)*JXYZ(2)))
      CNT=0
      WRITE(*,1100)
1100  FORMAT(/,' TILING IMAGE...'/)
C
      DO 100 I=1,NY
        IP=(I-1)*JXYZ(2)
        DO 20 J=1,JXYZ(2)
          ID=1+NXYZ(1)*(J-1)
          CALL IREAD(10,AIN(ID),J+IP)
20      CONTINUE
        DO 100 J=1,NX
          IX=(J-1)*JXYZ(1)+1
          CALL BOXIMG(AIN,NXYZ,ABOX,JXYZ,IX,1,MEAN,RMS)
          IF ((RMS.LT.RMSMAX).AND.(RMS.GT.RMSMIN)) THEN
            CALL RLFT3(ABOX,CBOXS,JXYZ(1),JXYZ(2),1,1)
            DO 40 L=1,JXYZ(2)
              DO 41 K=1,JXYZ(1)/2
                ID=(K+JXYZ(1)/2*(L-1))*2
                IS=K+KXYZ(1)*(L-1)
                POWER(IS)=POWER(IS)
     +            +(ABOX(ID-1)**2+ABOX(ID)**2)*SCAL**2
41            CONTINUE
              IF (KXYZ(1).GT.JXYZ(1)/2)
     +          POWER(IS+1)=POWER(IS+1)+CABS(CBOXS(L)*SCAL)**2
40          CONTINUE
            CNT=CNT+1
          ENDIF
100   CONTINUE
      WRITE(*,*)' Total tiles and number used',NX*NY,CNT
C
CHEN>
      if(CNT.lt.1)then
        write(*,'('':: ERROR: BOXIMG did not produce correct tiles'')')
        write(*,*)':: RMSMIN:',RMSMIN,'  RMSMAX:',RMSMAX,'   last RMS:',RMS
        stop
      endif
CHEN<
C
      SCAL=1.0/CNT
      DO 60 K=1,KXYZ(1)*KXYZ(2)
        POWER(K)=SQRT(POWER(K)*SCAL)
60    CONTINUE
C
C       Filter power spectrum to remove slowly varying background
C       DMAX is maximum of filtered power spectrum (for later scaling)
C
      STEPR=DSTEP*(10.0**4.0)/XMAG
      RESMIN=STEPR/RESMIN
      RESMAX=STEPR/RESMAX
      IF (RESMIN.LT.STEPR/50.0) THEN
        RESMIN=STEPR/50.0
        WRITE(*,*)' Lower resolution limit reset to',
     +            STEPR/RESMIN,' A'
      ENDIF
      IF (RESMIN.GE.RESMAX)
     +  STOP ' RESMIN >= RESMAX; increase RESMAX'
C
      CALL FILTER(JXYZ,KXYZ,POWER,BUF1,DMEAN,DRMS1,DMAX,
     +            RESMIN)
C
      CS=CS*(10.0**7.0)                          ! Angstroms
      KV=KV*1000.0                               ! Volts
      WL=12.26/SQRT(KV+0.9785*KV**2/(10.0**6.0)) ! Angstroms
      WGH1=SQRT(1.0-WGH**2)
      WGH2=WGH
C
      THETATR=WL/(STEPR*JXYZ(1))
C
      DFMID1=DFMIN
      DFMID2=DFMAX
CHEN>
C      CALL SEARCH_CTF(CS,WL,WGH1,WGH2,THETATR,RESMIN,RESMAX,
C     +            POWER,JXYZ,DFMID1,DFMID2,ANGAST,FSTEP,DAST)
      write(*,'(''Entering SEARCH_CTF subroutine'')')
C-----input parameters; DFMID1, DFMID2, for lower and higher boundaries of search.
      CALL SEARCH_CTF(CS,WL,WGH1,WGH2,THETATR,RESMIN,RESMAX,
     +             POWER,JXYZ,DFMID1,DFMID2,ANGAST,FSTEP,DAST,INOAST,DEFREF)
c-----output parameters: DFMID1,DFMID2,ANGAST for A,B,angle of defocus.
CHEN<

      RMIN2=RESMIN**2
      RMAX2=RESMAX**2
      HW=-1.0/RMAX2
      hw=0.0

CHEN>
C
      if(INOAST.eq.0)then
C-------Considering astigmatism variation:
        write(*,'(''Entering REFINE_CTF subroutine'')')
        CALL REFINE_CTF(DFMID1,DFMID2,ANGAST,POWER,CS,WL,
     +       WGH1,WGH2,THETATR,RMIN2,RMAX2,JXYZ,HW,DAST)
      else
C-------Not considering astigmatism variation:
        write(*,'(''Skipping REFINE_CTF subroutine'')')
C-------From here on, ANGAST has to change from DEG to RAD... (!)
        ANGAST = ANGAST * PI / 180.0
      endif
C
      if(DRMSH.ne.0.0)then
        DRMS1 = DRMSH
      endif
C
CHEN<
C
      DO 50 I=1,JXYZ(1)*JXYZ(2)
        OUT(I)=0.0
50    CONTINUE
      DO 200 L=1,JXYZ(1)/2
        LL=L-1
        DO 200 M=1,JXYZ(2)
          MM=M-1
          IF (MM.GT.JXYZ(2)/2) MM=MM-JXYZ(2)
          ID=L+JXYZ(1)/2*(M-1)
          I=L+JXYZ(1)/2
          J=M+JXYZ(2)/2
          IF (J.GT.JXYZ(2)) J=J-JXYZ(2)
          IS=I+JXYZ(1)*(J-1)
C         OUT(IS)=POWER(ID)/DRMS1*SQRT(2.0*PI)
          OUT(IS)=POWER(ID)/DRMS1/2.0+0.5
          IF (OUT(IS).GT.1.0) OUT(IS)=1.0
          IF (OUT(IS).LT.-1.0) OUT(IS)=-1.0
          RES2=(REAL(LL)/JXYZ(1))**2+(REAL(MM)/JXYZ(2))**2
          IF ((RES2.LE.RMAX2).AND.(RES2.GE.RMIN2)) THEN
            CTFV=CTF(CS,WL,WGH1,WGH2,DFMID1,DFMID2,
     +               ANGAST,THETATR,LL,MM)
            I=JXYZ(1)/2-L+1
            J=JXYZ(2)-J+2
            IF (J.LE.JXYZ(2)) THEN
              IS=I+JXYZ(1)*(J-1)
              OUT(IS)=CTFV**2
            ENDIF
          ENDIF
200   CONTINUE
C
      CALL ICLOSE(10)
      MODE=2
      IF (STEPR.NE.0.0) STEPR=1.0/STEPR
      CALL IOPEN(FILEOUT,10,CFORM,MODE,JXYZ(1),JXYZ(2),
     +           JXYZ(3),'NEW',STEPR,TITLE)
      DO 210 J=1,JXYZ(2)
        ID=1+JXYZ(1)*(J-1)
        CALL IWRITE(10,OUT(ID),J)
210   CONTINUE
      CALL ICLOSE(10)
C
CHEN>
      call system("\rm -f SCRATCH/2dx_ctffind3_result.tmp")
      open(11,FILE='SCRATCH/2dx_ctffind3_result.tmp',STATUS='NEW',ERR=998)
        write(11,'(3F12.2)')DFMID1,DFMID2,ANGAST/PI*180.0
        write(11,'(G18.6)')DRMS1
      close(11)
      GOTO 9999
998   stop ':: ERROR on file open of SCRATCH/2dx_ctffind3_result.tmp'
9999  continue
CHEN<
C
      END
C
C**************************************************************************
      SUBROUTINE HISTO(N,NBIN,DATA,BINS,MIN,MAX)
C**************************************************************************
      IMPLICIT NONE
C
      INTEGER I,J,N,NBIN  
      REAL DATA(*),MIN,MAX,BINS(*)
C
      MIN=1.0E30
      MAX=-1.0E30
      DO 10 I=1,N
        IF (DATA(I).GT.MAX) MAX=DATA(I)
        IF (DATA(I).LT.MIN) MIN=DATA(I)
10    CONTINUE
C
      DO 20 I=1,NBIN
        BINS(I)=0.0
20    CONTINUE
C
      DO 30 I=1,N
        J=INT((DATA(I)-MIN)/(MAX-MIN)*(NBIN-1)+0.5)+1
        BINS(J)=BINS(J)+1.0
30    CONTINUE
C
      RETURN
      END
C
C**************************************************************************
      SUBROUTINE FILTER(JXYZ,KXYZ,POWER,BUF1,DMEAN,DRMS,DMAX,
     +                  RESMIN)
C**************************************************************************
C       Filters power spectrum by removing smooth background. This
C       is necessary to obtain a good CTF fit. Also calculates
C       mean, STD and maximum of filtered spectrum.
C       Resizes the power spectrum to be exactly of dimension
C       JXYZ(1) x JXYZ(2)
C**************************************************************************
      IMPLICIT NONE
C
      INTEGER I,J,JXYZ(*),KXYZ(*),IS,ID,NW
      REAL SCAL,POWER(*),DRMS,DMEAN,DSQR,BUF1(*),DMAX
      REAL RESMIN
C
      WRITE(*,1101)
1101  FORMAT(/,' FILTERING POWER SPECTRUM...'/)
C
C      NW=INT(KXYZ(1)*DSTEP/20.0)
      NW=INT(KXYZ(1)*RESMIN*SQRT(2.0))
C
C       subtract smooth background
C
      CALL MSMOOTH(POWER,KXYZ,NW,BUF1)
C
C       calculate mean, STD, resize power spectrum
C
      DMEAN=0.0
      DSQR=0.0
      DMAX=-1.0E30
      DO 61 J=3,JXYZ(2)-2
        DO 61 I=3,JXYZ(1)/2-2
          ID=I+JXYZ(1)/2*(J-1)
          IS=I+KXYZ(1)*(J-1)
          POWER(ID)=POWER(IS)
          DMEAN=DMEAN+POWER(ID)
          DSQR=DSQR+POWER(ID)**2
          IF (POWER(ID).GT.DMAX) DMAX=POWER(ID)
61    CONTINUE
      DMEAN=DMEAN/JXYZ(1)/JXYZ(2)*2
      DSQR=DSQR/JXYZ(1)/JXYZ(2)*2
      DRMS=SQRT(DSQR-DMEAN**2)
      DO 62 J=1,JXYZ(2)
        DO 62 I=1,JXYZ(1)/2
          ID=I+JXYZ(1)/2*(J-1)
          IF (POWER(ID).GT.DMAX) POWER(ID)=DMAX
62    CONTINUE
C
      RETURN
      END
C
C**************************************************************************
      SUBROUTINE MSMOOTH(ABOX,NXYZ,NW,BUF)
C**************************************************************************
C       Calculates a smooth background in the power spectrum
C       in ABOX using a box convolution with box size 2NW+1 x 2NW+1.
C       Replaces input with background-subtracted power spectrum.
C**************************************************************************
      IMPLICIT NONE
C
      INTEGER NXYZ(*),NW,I,J,K,L,IX,IY,ID,CNT
      REAL ABOX(*),SUM,BUF(*)
C
C       loop over X and Y
C
      DO 10 I=1,NXYZ(1)
        DO 10 J=1,NXYZ(2)
          SUM=0.0
          CNT=0
C
C       loop over box to average
C
          DO 20 K=-NW,NW
            DO 20 L=-NW,NW
              IX=I+K
              IY=J+L
C       
C       here reset IX to wrap around spectrum
C
              IF (IX.GT.NXYZ(1)) IX=IX-2*NXYZ(1)
              IF (IX.LT.1) THEN
                IX=1-IX
                IY=1-IY
              ENDIF
C
C       here reset IY to wrap around spectrum
C       
              IF (IY.GT.NXYZ(2)) IY=IY-NXYZ(2)
              IF (IY.LE.-NXYZ(2)) IY=IY+NXYZ(2)
              IF (IY.LT.1) IY=1-IY
              ID=IX+NXYZ(1)*(IY-1)
C              IF (ID.NE.1) THEN
              IF ((IX.GT.1).AND.(IY.GT.1)) THEN
                SUM=SUM+ABOX(ID)
                CNT=CNT+1
              ENDIF
20        CONTINUE
          SUM=SUM/CNT
          ID=I+NXYZ(1)*(J-1)
          IF (ID.NE.1) THEN
            BUF(ID)=SUM
          ELSE
            BUF(ID)=ABOX(ID)
          ENDIF
10    CONTINUE
C       
C       replace input with background-subtracted spectrum
C
      DO 40 I=1,NXYZ(1)*NXYZ(2)
        ABOX(I)=ABOX(I)**2-BUF(I)**2
40    CONTINUE
C
      RETURN
      END
C
C**************************************************************************
      SUBROUTINE BOXIMG(AIN,NXYZ,ABOX,JXYZ,IX,IY,MEAN,RMS)
C**************************************************************************
C       Cuts out an area of size JXYZ from array AIN. IX, IY are
C       upper left corner of boxed area.
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER I,J,NXYZ(3),JXYZ(3),IX,IY,ID,IDB,II,JJ
      REAL AIN(*),ABOX(*),MEAN,RMS,M1,M2,M3,M4
      REAL*8 DRMS,DMEAN,DM1,DM2,DM3,DM4,DTMP
C
      DMEAN=0.0
      DM1=0.0
      DM2=0.0
      DM3=0.0
      DM4=0.0
      DO 10 J=1,JXYZ(2)
        DO 10 I=1,JXYZ(1)
          II=I+IX-1
          JJ=J+IY-1
          ID=II+NXYZ(1)*(JJ-1)
          IDB=I+JXYZ(1)*(J-1)
          ABOX(IDB)=AIN(ID)
          DMEAN=DMEAN+ABOX(IDB)
          IF (I.EQ.1) DM1=DM1+ABOX(IDB)
          IF (I.EQ.JXYZ(1)) DM2=DM2+ABOX(IDB)
          IF (J.EQ.1) DM3=DM3+ABOX(IDB)
          IF (J.EQ.JXYZ(2)) DM4=DM4+ABOX(IDB)
10    CONTINUE
      DMEAN=DMEAN/JXYZ(1)/JXYZ(2)
      MEAN=DMEAN
      DM1=DM1/JXYZ(2)
      DM2=DM2/JXYZ(2)
      DM3=DM3/JXYZ(1)
      DM4=DM4/JXYZ(1)
      DRMS=0.0
      DO 20 I=1,JXYZ(1)*JXYZ(2)
        DRMS=DRMS+(ABOX(I)-DMEAN)**2
20    CONTINUE
      DRMS=SQRT(DRMS/JXYZ(1)/JXYZ(2))
      RMS=DRMS
      DO 30 J=1,JXYZ(2)
        DO 30 I=1,JXYZ(1)
          IDB=I+JXYZ(1)*(J-1)
          ABOX(IDB)=ABOX(IDB)-(DM1+(DM2-DM1)/(JXYZ(1)-1)*(I-1))
     +                 -(DM3+(DM4-DM3)/(JXYZ(2)-1)*(J-1))+DMEAN
30    CONTINUE
C
      RETURN
      END
C
C**************************************************************************
      REAL FUNCTION CTF(CS,WL,WGH1,WGH2,DFMID1,DFMID2,ANGAST,
     +                  THETATR,IX,IY)
C**************************************************************************
C
      PARAMETER (TWOPI=6.2831853071796)
C
      RAD=IX**2+IY**2
      IF (RAD.NE.0.0) THEN
        RAD=SQRT(RAD)
        ANGLE=RAD*THETATR
        ANGSPT=ATAN2(REAL(IY),REAL(IX))
        C1=TWOPI*ANGLE*ANGLE/(2.0*WL)
        C2=-C1*CS*ANGLE*ANGLE/2.0
        ANGDIF=ANGSPT-ANGAST
        CCOS=COS(2.0*ANGDIF)
        DF=0.5*(DFMID1+DFMID2+CCOS*(DFMID1-DFMID2))
        CHI=C1*DF+C2
        CTF=-WGH1*SIN(CHI)-WGH2*COS(CHI)
      ELSE
        CTF=-WGH2
      ENDIF
C
      RETURN
      END
C
C**************************************************************************
CHEN>
C      SUBROUTINE SEARCH_CTF(CS,WL,WGH1,WGH2,THETATR,RMIN,RMAX,
C     +          AIN,NXYZ,DFMID1,DFMID2,ANGAST,FSTEP,DAST)
      SUBROUTINE SEARCH_CTF(CS,WL,WGH1,WGH2,THETATR,RMIN,RMAX,
     +          AIN,NXYZ,DFMID1,DFMID2,ANGAST,FSTEP,DAST,INOAST,DEFREF)
CHEN<
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER I,J,K,NXYZ(3),I1,I2,ID,IERR
CHEN>
      INTEGER INOAST
      REAL DEFREF(3)
CHEN<
      REAL CS,WL,WGH1,WGH2,THETATR,DFMID1,DFMID2,ANGAST
      REAL RMIN2,RMAX2,RMIN,RMAX,AIN(*),SUMMAX,FSTEP
      REAL DFMID1S,DFMID2S,ANGASTS,HW,PI,EVALCTF,DAST
      REAL,ALLOCATABLE :: SUMS(:),DF1(:),DF2(:),ANG(:)
      PARAMETER (PI=3.1415926535898)
C
      WRITE(*,1000)
1000  FORMAT(/,' SEARCHING CTF PARAMETERS...'/,
     +       /,'      DFMID1      DFMID2      ANGAST          CC'/)
C
      RMIN2=RMIN**2
      RMAX2=RMAX**2
      HW=-1.0/RMAX2
      hw=0.0
      SUMMAX=-1.0E20
CHEN>
      if(INOAST.eq.0)then
C-------Normal mode: Search X,Y,Angls for defocus and astigmatism
CHEN<
      I1=INT(DFMID1/FSTEP)
      I2=INT(DFMID2/FSTEP)
      ID=(I2-I1+1)*(I2-I1+1)
      ALLOCATE(SUMS(ID),DF1(ID),DF2(ID),ANG(ID),STAT=IERR)
      IF (IERR.NE.0) THEN
        WRITE(*,*) ' ERROR: Memory allocation failed in SEARCH_CTF'
        STOP ' Try reducing size of defocus search grid'
      ENDIF
C      DO 10 K=0,3
      DO 10 K=0,17
        DO 11 I=I1,I2
!$OMP PARALLEL DO
          DO 12 J=I1,I2
            CALL SEARCH_CTF_S(CS,WL,WGH1,WGH2,THETATR,RMIN2,
     +            RMAX2,AIN,NXYZ,DF1,DF2,ANG,FSTEP,SUMS,HW,DAST,
     +            SUMMAX,DFMID1S,DFMID2S,ANGASTS,I,J,K,I1,I2)
12        CONTINUE
11      CONTINUE
        DO 13 I=1,ID
          IF (SUMS(I).GT.SUMMAX) THEN
            WRITE(*,1100)DF1(I),DF2(I),ANG(I)/PI*180.0,SUMS(I)
1100        FORMAT(3F12.2,F12.5)
            SUMMAX=SUMS(I)
            DFMID1S=DF1(I)
            DFMID2S=DF2(I)
            ANGASTS=ANG(I)
          ENDIF
13      CONTINUE
10    CONTINUE
      DEALLOCATE(SUMS,DF1,DF2,ANG)
C
      DFMID1=DFMID1S
      DFMID2=DFMID2S
      ANGAST=ANGASTS
CHEN>
        WRITE(*,'(''Best Values: '',3G12.2,G12.5)')
     .     DFMID1,DFMID2,ANGAST,SUMMAX
C
      else
C-------One-dimensinal defocus search, while keeping astigmatism constant
C
        write(*,'('':Reference defocus was '',3F12.3)')DEFREF(1),DEFREF(2),DEFREF(3)
C
C-------How many steps to do in each direction:
        I2=ABS(INT((DFMID2-DFMID1)/FSTEP))
C-------and in the other direction the same:
        I1=-I2
C-------The total number of search steps:
        ID=I2-I1+1
C
        ALLOCATE(SUMS(ID),DF1(ID),DF2(ID),ANG(ID),STAT=IERR)
        IF (IERR.NE.0) THEN
          WRITE(*,*) '::  ERROR: Memory allocation failed in SEARCH_CTF'
          STOP '::  Try reducing size of defocus search grid'
        ENDIF
        K=0
        J=0
        ANGASTS=ANGAST
!$OMP PARALLEL DO
        DO 21 I=1,ID
C---------This search is from the reference position -I2 to +I2 in ID steps:
          J=I1+I-1
C           write(*,'(''calling SEARCH_CTF_S_INOAST with '',3I6)')I,I1,I2
          CALL SEARCH_CTF_S_INOAST(CS,WL,WGH1,WGH2,THETATR,RMIN2,
     +          RMAX2,AIN,NXYZ,DF1,DF2,ANG,FSTEP,SUMS,HW,DAST,
     +          SUMMAX,DFMID1S,DFMID2S,ANGASTS,I,J,K,I1,I2,DEFREF)
21      CONTINUE
        DO 23 I=1,ID
C         WRITE(*,1200)DF1(I),DF2(I),ANG(I)/PI*180.0,SUMS(I)
          IF (SUMS(I).GT.SUMMAX) THEN
1200        FORMAT(3F12.2,F12.5)
            SUMMAX=SUMS(I)
            DFMID1S=DF1(I)
            DFMID2S=DF2(I)
          ENDIF
23      CONTINUE
        DEALLOCATE(SUMS,DF1,DF2,ANG)
        DFMID1=DFMID1S
        DFMID2=DFMID2S
        ANGAST=DEFREF(3)
        write(*,'('':Best Defocus found at '',4F12.3)')DFMID1,DFMID2,ANGAST,SUMMAX
C
      endif
CHEN<
C
      RETURN
      END      
C
C**************************************************************************
      SUBROUTINE SEARCH_CTF_S(CS,WL,WGH1,WGH2,THETATR,RMIN2,
     +            RMAX2,AIN,NXYZ,DF1,DF2,ANG,FSTEP,SUMS,HW,DAST,
     +            SUMMAX,DFMID1S,DFMID2S,ANGASTS,I,J,K,I1,I2)
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER I,J,K,NXYZ(3),I1,I2,ID
      REAL CS,WL,WGH1,WGH2,THETATR,DF1(*),DF2(*),ANG(*)
      REAL RMIN2,RMAX2,SUMS(*),AIN(*),SUMMAX,FSTEP
      REAL DFMID1S,DFMID2S,ANGASTS,HW,PI,EVALCTF,DAST
      PARAMETER (PI=3.1415926535898)
C
      ID=I-I1+1+(I2-I1+1)*(J-I1)
      DF1(ID)=FSTEP*I
      DF2(ID)=FSTEP*J
C      ANG(ID)=22.5*K
      ANG(ID)=5.0*K
      ANG(ID)=ANG(ID)/180.0*PI
      SUMS(ID)=EVALCTF(CS,WL,WGH1,WGH2,DF1(ID),DF2(ID),
     +  ANG(ID),THETATR,HW,AIN,NXYZ,RMIN2,RMAX2,DAST)
C
      RETURN
      END      
C
CHEN>
C**************************************************************************
      SUBROUTINE SEARCH_CTF_S_INOAST(CS,WL,WGH1,WGH2,THETATR,RMIN2,
     +            RMAX2,AIN,NXYZ,DF1,DF2,ANG,FSTEP,SUMS,HW,DAST,
     +            SUMMAX,DFMID1S,DFMID2S,ANGASTS,I,J,K,I1,I2,DEFREF)
C**************************************************************************
C
      IMPLICIT NONE
C
CHEN>
      REAL DEFREF(3)
CHEN<
      INTEGER I,J,K,NXYZ(3),I1,I2,ID
      REAL CS,WL,WGH1,WGH2,THETATR,DF1(*),DF2(*),ANG(*)
      REAL RMIN2,RMAX2,SUMS(*),AIN(*),SUMMAX,FSTEP
      REAL DFMID1S,DFMID2S,ANGASTS,HW,PI,EVALCTF,DAST
      PARAMETER (PI=3.1415926535898)
C
      ID=I
      DF1(ID)=DEFREF(1)+FSTEP*J
      DF2(ID)=DEFREF(2)+FSTEP*J
      ANG(ID)=DEFREF(3)
      ANG(ID)=ANG(ID)/180.0*PI
C      write(*,'(''testing DF1,DF2,ANG of '',3F12.3)')DF1(ID),DF2(ID),ANG(ID)
      SUMS(ID)=EVALCTF(CS,WL,WGH1,WGH2,DF1(ID),DF2(ID),
     +  ANG(ID),THETATR,HW,AIN,NXYZ,RMIN2,RMAX2,DAST)
C
      RETURN
      END
CHEN<

C**************************************************************************
      REAL FUNCTION EVALCTF(CS,WL,WGH1,WGH2,DFMID1,DFMID2,ANGAST,
     +                      THETATR,HW,AIN,NXYZ,RMIN2,RMAX2,DAST)
C**************************************************************************
C     This part of the code was made more efficient by
C
C     Robert Sinkovits
C     Department of Chemistry and Biochemistry
C     San Diego Supercomputer Center
C
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER L,LL,M,MM,NXYZ(3),ID,IS
      REAL CS,WL,WGH1,WGH2,DFMID1,DFMID2,ANGAST,THETATR,SUM1
      REAL SUM,AIN(*),RES2,RMIN2,RMAX2,CTF,CTFV,HW,SUM2,DAST
      real rad2, hangle2, angspt, c1, c2, angdif, ccos, df, chi
      real expv, twopi_wli, ctfv2, dsum, ddif, rpart1, rpart2
      real half_thetatrsq, recip_nxyz1, recip_nxyz2
      real :: twopi=6.2831853071796

      twopi_wli  = twopi/wl      
      dsum = dfmid1 + dfmid2
      ddif = dfmid1 - dfmid2
      half_thetatrsq = 0.5*thetatr*thetatr
      recip_nxyz1 = 1.0/nxyz(1)
      recip_nxyz2 = 1.0/nxyz(2)

      SUM  = 0.0
      SUM1 = 0.0
      SUM2 = 0.0
      IS   = 0

      DO M=1,NXYZ(2)
         MM=M-1
         IF (MM > NXYZ(2)/2) MM=MM-NXYZ(2)
         rpart2 = real(mm) * recip_nxyz2
         
         DO L=1,NXYZ(1)/2
            LL=L-1
            rpart1 = real(ll) * recip_nxyz1
            RES2 = rpart1*rpart1 + rpart2*rpart2
            
            IF (RES2 <= RMAX2 .AND. RES2 > RMIN2) THEN
               RAD2 = LL*LL + MM*MM
               IF (RAD2.NE.0.0) THEN
                  ANGSPT = ATAN2(REAL(MM), REAL(LL))
                  ANGDIF = ANGSPT - ANGAST
                  CCOS   = COS(2.0*ANGDIF)
                  DF     = 0.5*(DSUM + CCOS*DDIF)
                  
                  HANGLE2 = rad2 * half_thetatrsq
                  C1      = twopi_wli*HANGLE2
                  C2      = -C1*CS*HANGLE2
                  CHI     = C1*DF + C2
                  CTFV    = -WGH1*SIN(CHI)-WGH2*COS(CHI)
               ELSE
                  CTFV    = -WGH2
               ENDIF
               
               ctfv2 = ctfv*ctfv
               ID   = L+NXYZ(1)/2*(M-1)
               IS   = IS + 1

               if(hw == 0.0) then
                  SUM  = SUM  + AIN(ID)*ctfv2
                  SUM1 = SUM1 + ctfv2*ctfv2
                  SUM2 = SUM2 + AIN(ID)*AIN(ID)
               else
                  expv = exp(hw*res2)
                  SUM  = SUM  + AIN(ID)*ctfv2*expv
                  SUM1 = SUM1 + ctfv2*ctfv2
                  SUM2 = SUM2 + AIN(ID)*AIN(ID)*expv*expv
               endif
               
            ENDIF

         enddo
      enddo

      IF (IS.NE.0) THEN
        SUM=SUM/SQRT(SUM1*SUM2)
        IF (DAST.GT.0.0) SUM=SUM-DDIF**2/2.0/DAST**2/IS
      ENDIF
      EVALCTF=SUM
      RETURN
      END
C
C**************************************************************************
      SUBROUTINE REFINE_CTF(DFMID1,DFMID2,ANGAST,POWER,
     +  CS,WL,WGH1,WGH2,THETATR,RMIN2,RMAX2,NXYZ,HW,DAST)
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER NCYCLS,NXYZ(3)
      PARAMETER (NCYCLS=50)
      REAL DFMID1,DFMID2,ANGAST,XPAR(4),EPAR(3),RF,ESCALE,PI
      REAL POWER(*),CS,WL,WGH1,WGH2,THETATR,RMIN2,RMAX2,HW,DAST
      PARAMETER (PI=3.1415926535898)
      DATA EPAR/100.0,100.0,0.5/
      DATA ESCALE/100.0/
C
      WRITE(*,1000)
1000  FORMAT(/,' REFINING CTF PARAMETERS...'/,
     +       /,'      DFMID1      DFMID2      ANGAST          CC'/)
C
      XPAR(1)=DFMID1
      XPAR(2)=DFMID2
      XPAR(3)=ANGAST
      XPAR(4)=0.0
      IF (XPAR(1).EQ.XPAR(2)) XPAR(1)=XPAR(1)+1.0
      CALL VA04A(XPAR,EPAR,3,RF,ESCALE,0,1,NCYCLS,POWER,
     +       CS,WL,WGH1,WGH2,THETATR,RMIN2,RMAX2,NXYZ,HW,DAST)
      DFMID1=XPAR(1)
      DFMID2=XPAR(2)
      ANGAST=XPAR(3)-PI*NINT(XPAR(3)/PI)
      WRITE(*,1100)DFMID1,DFMID2,ANGAST/PI*180.0,-RF
1100  FORMAT(3F12.2,F12.5,'  Final Values')
C
      RETURN
      END
C
C******************************************************************************
C
      SUBROUTINE CALCFX(NX,XPAR,RF,AIN,
     +  CS,WL,WGH1,WGH2,THETATR,RMIN2,RMAX2,NXYZ,HW,DAST)
C
C     CALCULATES NEW VALUE FOR F TO INPUT TO SUBROUTINE VA04A
C
C******************************************************************************
C
      IMPLICIT NONE
C
      INTEGER NXYZ(3),NX,IXBMX
      PARAMETER (IXBMX=512)
      REAL CS,WL,WGH1,WGH2,THETATR,DFMID1,DFMID2,ANGAST
      REAL RMIN2,RMAX2,AIN(*),EVALCTF,DAST
      REAL DFMID1S,DFMID2S,ANGASTS,HW,PI,XPAR(3),RF
      PARAMETER (PI=3.1415926535898)
C
      RF=-EVALCTF(CS,WL,WGH1,WGH2,XPAR(1),XPAR(2),XPAR(3),
     +            THETATR,HW,AIN,NXYZ,RMIN2,RMAX2,DAST)
C
      RETURN
      END
C
C**************************************************************************
      SUBROUTINE SCLIMG(AIN,NXYZ,SCAL)
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER I,J,NXYZ(3),ID
      REAL SCAL,AIN(*)
C
      DO 10 J=1,NXYZ(2)
        DO 10 I=1,NXYZ(1)
          ID=I+NXYZ(1)*(J-1)
          AIN(ID)=AIN(ID)*SCAL
10    CONTINUE
C
      RETURN
      END
C
C**************************************************************************
C
      SUBROUTINE rlft3(realdata,speq,nn1,nn2,nn3,isign)
C
C-----Was called with:
C-----CALL RLFT3(ABOX,CBOXS,JXYZ(1),JXYZ(2),1,1)
C-----ALLOCATE(ABOX(JXYZ(1)*JXYZ(2)),CBOXS(JXYZ(2)))
C
      INTEGER isign,nn1,nn2,nn3,istat,iw
      PARAMETER (iw=4096)
      REAL realdata(nn1,nn2,nn3)
      COMPLEX, ALLOCATABLE :: data(:,:,:)

      COMPLEX speq(nn2,nn3)
      REAL work(6*iw+15)
      INTEGER i1,i2,i3,j1,j2,j3,nn(3),nnh,nnq
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      COMPLEX c1,c2,h1,h2,w
C
C         ! Convert real -> complex
      allocate(data(nn1/2,nn2,nn3))
C     write(6,'(''::Hier1: Copy realdata to data, using i1,i2,i3: '',3I8)')nn1,nn2,nn3
      do i3=1,nn3
        do i2=1,nn2
          do i1=1,nn1,2
            data(i1/2+1,i2,i3)=CMPLX(realdata(i1,i2,i3),realdata(i1+1,i2,i3))
          enddo
        enddo
      enddo 
c
      c1=cmplx(0.5,0.0)
      c2=cmplx(0.0,-0.5*isign)
      theta=6.28318530717959d0/dble(isign*nn1)
      wpr=-2.0d0*sin(0.5d0*theta)**2
      wpi=sin(theta)
      nnh=nn1/2
      nnq=nn1/4
      nn(1)=nnh
      nn(2)=nn2
      nn(3)=nn3
      if(isign.eq.1)then
        call pda_nfftf(3,nn,data,work,istat)
        do 12 i3=1,nn3
          do 11 i2=1,nn2
            speq(i2,i3)=data(1,i2,i3)
11        continue
12      continue
      endif
C
      if(isign.eq.-1)then
        call flip_array(data,speq,nn1,nn2,nn3)
      endif
C
      do 15 i3=1,nn3
        j3=1
        if (i3.ne.1) j3=nn3-i3+2
        wr=1.0d0
        wi=0.0d0
        do 14 i1=1,nnq+1
          j1=nnh-i1+2
          do 13 i2=1,nn2
            j2=1
            if (i2.ne.1) j2=nn2-i2+2
            if(i1.eq.1)then
              h1=c1*(data(1,j2,j3)+conjg(speq(i2,i3)))
              h2=c2*(data(1,j2,j3)-conjg(speq(i2,i3)))
              data(1,j2,j3)=h1+h2
              speq(i2,i3)=conjg(h1-h2)
            else
              h1=c1*(data(j1,j2,j3)+conjg(data(i1,i2,i3)))
              h2=c2*(data(j1,j2,j3)-conjg(data(i1,i2,i3)))
              data(j1,j2,j3)=h1+w*h2
              data(i1,i2,i3)=conjg(h1-w*h2)
            endif
13        continue
          wtemp=wr
          wr=wr*wpr-wi*wpi+wr
          wi=wi*wpr+wtemp*wpi+wi
          w=cmplx(sngl(wr),sngl(wi))
14      continue
15    continue
C
      if(isign.eq.1)then
        call flip_array(data,speq,nn1,nn2,nn3)
      endif
C
      if(isign.eq.-1)then
        call pda_nfftb(3,nn,data,work,istat)
      endif
      do i3=1,nn3
        do i2=1,nn2
          do i1=1,nn1,2
            realdata(i1  ,i2,i3)=REAL( data(i1/2+1,i2,i3))
            realdata(i1+1,i2,i3)=AIMAG(data(i1/2+1,i2,i3))
          enddo
        enddo
      enddo 
c
      return
      END
C
C**************************************************************************
C
      SUBROUTINE FLIP_ARRAY(DATA,SPEQ,NN1,NN2,NN3)
C
      IMPLICIT NONE
C
      INTEGER NN1,NN2,NN3,I1,I2,I3,J1,J2,J3,NNH
      COMPLEX DATA(NN1/2,NN2,NN3),SPEQ(NN2,NN3),W
C
      nnh=nn1/2
      do 13 i1=1,nnh/2+1
        do 14 i3=1,nn3
          do 15 i2=1,nn2
            j1=1
            if (i1.ne.1) j1=nnh-i1+2
            w=data(i1,i2,i3)
            data(i1,i2,i3)=data(j1,i2,i3)
            data(j1,i2,i3)=w
15        continue
14      continue
13    continue
      do 10 i2=1,nn2/2+1
        do 11 i3=1,nn3
          j2=1
          if (i2.ne.1) j2=nn2-i2+2
          w=speq(i2,i3)
          speq(i2,i3)=speq(j2,i3)
          speq(j2,i3)=w
          do 12 i1=1,nnh
            w=data(i1,i2,i3)
            data(i1,i2,i3)=data(i1,j2,i3)
            data(i1,j2,i3)=w
12        continue
11      continue
10    continue
      do 16 i3=1,nn3/2+1
        j3=1
        if (i3.ne.1) j3=nn3-i3+2
        do 17 i2=1,nn2
          w=speq(i2,i3)
          speq(i2,i3)=speq(i2,j3)
          speq(i2,j3)=w
          do 18 i1=1,nnh
            w=data(i1,i2,i3)
            data(i1,i2,i3)=data(i1,i2,j3)
            data(i1,i2,j3)=w
18        continue
17      continue
16    continue
C
      return
      end
C**************************************************************************
      SUBROUTINE PDA_CFFTB1 (N,C,CH,WA,AFAC)
      DIMENSION       CH(*)      ,C(*)       ,WA(*)      ,AFAC(*)
      NF = AFAC(2)
      NA = 0
      L1 = 1
      IW = 1
      DO 116 K1=1,NF
         IP = AFAC(K1+2)
         L2 = IP*L1
         IDO = N/L2
         IDOT = IDO+IDO
         IDL1 = IDOT*L1
         IF (IP .NE. 4) GO TO 103
         IX2 = IW+IDOT
         IX3 = IX2+IDOT
         IF (NA .NE. 0) GO TO 101
         CALL PDA_PASSB4 (IDOT,L1,C,CH,WA(IW),WA(IX2),WA(IX3))
         GO TO 102
  101    CALL PDA_PASSB4 (IDOT,L1,CH,C,WA(IW),WA(IX2),WA(IX3))
  102    NA = 1-NA
         GO TO 115
  103    IF (IP .NE. 2) GO TO 106
         IF (NA .NE. 0) GO TO 104
         CALL PDA_PASSB2 (IDOT,L1,C,CH,WA(IW))
         GO TO 105
  104    CALL PDA_PASSB2 (IDOT,L1,CH,C,WA(IW))
  105    NA = 1-NA
         GO TO 115
  106    IF (IP .NE. 3) GO TO 109
         IX2 = IW+IDOT
         IF (NA .NE. 0) GO TO 107
         CALL PDA_PASSB3 (IDOT,L1,C,CH,WA(IW),WA(IX2))
         GO TO 108
  107    CALL PDA_PASSB3 (IDOT,L1,CH,C,WA(IW),WA(IX2))
  108    NA = 1-NA
         GO TO 115
  109    IF (IP .NE. 5) GO TO 112
         IX2 = IW+IDOT
         IX3 = IX2+IDOT
         IX4 = IX3+IDOT
         IF (NA .NE. 0) GO TO 110
         CALL PDA_PASSB5 (IDOT,L1,C,CH,WA(IW),WA(IX2),WA(IX3),WA(IX4))
         GO TO 111
  110    CALL PDA_PASSB5 (IDOT,L1,CH,C,WA(IW),WA(IX2),WA(IX3),WA(IX4))
  111    NA = 1-NA
         GO TO 115
  112    IF (NA .NE. 0) GO TO 113
         CALL PDA_PASSB (NAC,IDOT,IP,L1,IDL1,C,C,C,CH,CH,WA(IW))
         GO TO 114
  113    CALL PDA_PASSB (NAC,IDOT,IP,L1,IDL1,CH,CH,CH,C,C,WA(IW))
  114    IF (NAC .NE. 0) NA = 1-NA
  115    L1 = L2
         IW = IW+(IP-1)*IDOT
  116 CONTINUE
      IF (NA .EQ. 0) RETURN
      N2 = N+N
      DO 117 I=1,N2
         C(I) = CH(I)
  117 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_CFFTB (N,C,WSAVE)
      DIMENSION       C(*)       ,WSAVE(*)
      IF (N .EQ. 1) RETURN
      IW1 = N+N+1
      IW2 = IW1+N+N
      CALL PDA_CFFTB1 (N,C,WSAVE,WSAVE(IW1),WSAVE(IW2))
      RETURN
      END
      SUBROUTINE PDA_CFFTF1 (N,C,CH,WA,AFAC)
      DIMENSION       CH(*)      ,C(*)       ,WA(*)      ,AFAC(*)
      NF = AFAC(2)
      NA = 0
      L1 = 1
      IW = 1
      DO 116 K1=1,NF
         IP = AFAC(K1+2)
         L2 = IP*L1
         IDO = N/L2
         IDOT = IDO+IDO
         IDL1 = IDOT*L1
         IF (IP .NE. 4) GO TO 103
         IX2 = IW+IDOT
         IX3 = IX2+IDOT
         IF (NA .NE. 0) GO TO 101
         CALL PDA_PASSF4 (IDOT,L1,C,CH,WA(IW),WA(IX2),WA(IX3))
         GO TO 102
  101    CALL PDA_PASSF4 (IDOT,L1,CH,C,WA(IW),WA(IX2),WA(IX3))
  102    NA = 1-NA
         GO TO 115
  103    IF (IP .NE. 2) GO TO 106
         IF (NA .NE. 0) GO TO 104
         CALL PDA_PASSF2 (IDOT,L1,C,CH,WA(IW))
         GO TO 105
  104    CALL PDA_PASSF2 (IDOT,L1,CH,C,WA(IW))
  105    NA = 1-NA
         GO TO 115
  106    IF (IP .NE. 3) GO TO 109
         IX2 = IW+IDOT
         IF (NA .NE. 0) GO TO 107
         CALL PDA_PASSF3 (IDOT,L1,C,CH,WA(IW),WA(IX2))
         GO TO 108
  107    CALL PDA_PASSF3 (IDOT,L1,CH,C,WA(IW),WA(IX2))
  108    NA = 1-NA
         GO TO 115
  109    IF (IP .NE. 5) GO TO 112
         IX2 = IW+IDOT
         IX3 = IX2+IDOT
         IX4 = IX3+IDOT
         IF (NA .NE. 0) GO TO 110
         CALL PDA_PASSF5 (IDOT,L1,C,CH,WA(IW),WA(IX2),WA(IX3),WA(IX4))
         GO TO 111
  110    CALL PDA_PASSF5 (IDOT,L1,CH,C,WA(IW),WA(IX2),WA(IX3),WA(IX4))
  111    NA = 1-NA
         GO TO 115
  112    IF (NA .NE. 0) GO TO 113
         CALL PDA_PASSF (NAC,IDOT,IP,L1,IDL1,C,C,C,CH,CH,WA(IW))
         GO TO 114
  113    CALL PDA_PASSF (NAC,IDOT,IP,L1,IDL1,CH,CH,CH,C,C,WA(IW))
  114    IF (NAC .NE. 0) NA = 1-NA
  115    L1 = L2
         IW = IW+(IP-1)*IDOT
  116 CONTINUE
      IF (NA .EQ. 0) RETURN
      N2 = N+N
      DO 117 I=1,N2
         C(I) = CH(I)
  117 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_CFFTF (N,C,WSAVE)
      DIMENSION       C(*)       ,WSAVE(*)
      IF (N .EQ. 1) RETURN
      IW1 = N+N+1
      IW2 = IW1+N+N
      CALL PDA_CFFTF1 (N,C,WSAVE,WSAVE(IW1),WSAVE(IW2))
      RETURN
      END
      SUBROUTINE PDA_CFFTI1 (N,WA,AFAC)
      DIMENSION       WA(*)      ,AFAC(*)    ,NTRYH(4)
      DATA NTRYH(1),NTRYH(2),NTRYH(3),NTRYH(4)/3,4,2,5/
      NL = N
      NF = 0
      J = 0
  101 J = J+1
      IF (J-4) 102,102,103
  102 NTRY = NTRYH(J)
      GO TO 104
  103 NTRY = NTRY+2
  104 NQ = NL/NTRY
      NR = NL-NTRY*NQ
      IF (NR) 101,105,101
  105 NF = NF+1
      AFAC(NF+2) = NTRY
      NL = NQ
      IF (NTRY .NE. 2) GO TO 107
      IF (NF .EQ. 1) GO TO 107
      DO 106 I=2,NF
         IB = NF-I+2
         AFAC(IB+2) = AFAC(IB+1)
  106 CONTINUE
      AFAC(3) = 2
  107 IF (NL .NE. 1) GO TO 104
      AFAC(1) = N
      AFAC(2) = NF
      TPI = 6.28318530717959
      ARGH = TPI/FLOAT(N)
      I = 2
      L1 = 1
      DO 110 K1=1,NF
         IP = AFAC(K1+2)
         LD = 0
         L2 = L1*IP
         IDO = N/L2
         IDOT = IDO+IDO+2
         IPM = IP-1
         DO 109 J=1,IPM
            I1 = I
            WA(I-1) = 1.
            WA(I) = 0.
            LD = LD+L1
            FI = 0.
            ARGLD = FLOAT(LD)*ARGH
            DO 108 II=4,IDOT,2
               I = I+2
               FI = FI+1.
               ARG = FI*ARGLD
               WA(I-1) = COS(ARG)
               WA(I) = SIN(ARG)
  108       CONTINUE
            IF (IP .LE. 5) GO TO 109
            WA(I1-1) = WA(I-1)
            WA(I1) = WA(I)
  109    CONTINUE
         L1 = L2
  110 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_CFFTI (N,WSAVE)
      DIMENSION       WSAVE(*)
      IF (N .EQ. 1) RETURN
      IW1 = N+N+1
      IW2 = IW1+N+N
      CALL PDA_CFFTI1 (N,WSAVE(IW1),WSAVE(IW2))
      RETURN
      END
      SUBROUTINE PDA_NFFTB( NDIM, DIM, DATA, WORK, ISTAT )
*+
*  Name:
*     PDA_NFFTB

*  Purpose:
*     Take the backward FFT of an N-dimensional complex array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PDA_NFFTB( NDIM, DIM, X, Y, WORK, ISTAT )

*  Description:
*     The supplied Fourier co-efficients in X and Y are replaced by the 
*     corresponding spatial data obtained by doing an inverse Fourier
*     transform. See the forward FFT routine PDA_NFFTF for more details.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of dimensions. This should be no more than 20.
*     DIM( NDIM ) = INTEGER (Given)
*        The size of each dimension.
*     X( * ) = REAL (Given and Returned)
*        Supplied holding the real parts of the Fourier co-efficients.
*        Returned holding the real parts of the spatial data. The array 
*        should have the number of elements implied by NDIM and DIM.
*     Y( * ) = REAL (Given and Returned)
*        Supplied holding the imaginary parts of the Fourier co-efficients.
*        Returned holding the imaginary parts of the spatial data. The array 
*        should have the number of elements implied by NDIM and DIM.
*     WORK( * ) = REAL (Given and Returned)
*        A work array. This should have at least ( 6*DimMax + 15 )
*        elements where DimMax is the maximum of the values supplied in
*        DIM.
*     ISTAT = INTEGER (Returned)
*        If the value of NDIM is greater than 20 or less than 1, then
*        ISTAT is returned equal to 1, and the values in X and Y are
*        left unchanged. Otherwise, ISTAT is returned equal to 0.
      
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-FEB-1995 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER NDIM
      INTEGER DIM( NDIM )
      
*  Arguments Given and Returned:
C      REAL X( * )
C      REAL Y( * )
      COMPLEX DATA( * )
      REAL WORK( * )

*  Arguments Returned:
      INTEGER ISTAT
      
*  Local Constants:
      INTEGER MXDIM              ! Max number of dimensions
      PARAMETER( MXDIM = 20 )
      
*  Local Variables:
      INTEGER
     :     CART( MXDIM + 1 ),    ! Current Cartesian pixel indices
     :     I,                    ! Index of current dimension
     :     INC,                  ! Vector increment to next row element
     :     IW,                   ! Index into the work array
     :     IWN,                  ! Index of first free work array element
     :     J,                    ! Row counter
     :     K,                    ! Pixel index on current axis
     :     M,                    ! Size of current dimension
     :     N,                    ! Total no. of pixels
     :     STEP,                 ! Vector step to start of next row
     :     V,                    ! Vector address
     :     V0                    ! Vector address of start of current row

      REAL
     :     FAC                   ! Normalisation factor

*.

*  Check that the supplied number of dimensions is not too high, and
*  not too low. Return 1 for the status variable and abort otherwise.
      IF( NDIM .GT. MXDIM .OR. NDIM .LE. 0 ) THEN
         ISTAT = 1

*  If the number of dimensions is ok, return 0 for the status value and
*  continue.
      ELSE
         ISTAT = 0

*  Find the total number of pixels.
         N = 1
         DO I = 1, NDIM
            N = N*DIM( I )
         END DO

*  The first dimension can be processed using a faster algorithm
*  because the elements to be processed occupy adjacent elements in the
*  supplied array. Set up the step (in vector address) between the
*  start of each row, and initialise the vector address of the start of
*  the first row.
         M = DIM( 1 )
         V0 = 1

*  Initialise the FFT work array for the current dimension. Save the
*  index of the next un-used element of the work array.
         CALL PDA_CFFTI( M, WORK )
         IWN = 4*M + 16

*  Store the factor which will normalise the Fourier co-efficients
*  returned by this routine (i.e. so that a call to PDA_NFFTB followed by a
*  call to PDA_NFFTB will result in no change to the data).
C         FAC = 1.0/SQRT( REAL ( N ) )

*  Loop round copying each row.
         DO J = 1, N/M

*  Copy this row into the unused part of the work array.
            IW = IWN
            V = V0
            DO K = 1, M
               WORK( IW ) = REAL(DATA( V ))
               WORK( IW + 1 ) = AIMAG(DATA( V ))
               IW = IW + 2
               V = V + 1
            END DO

*  Take the FFT of it.
            CALL PDA_CFFTB( M, WORK( IWN ), WORK )         

*  Copy it back to the supplied arrays, normalising it in the process.
            IW = IWN
            V = V0
            DO K = 1, M
               DATA( V ) = CMPLX(WORK( IW ),WORK( IW + 1 ))
               IW = IW + 2
               V = V + 1
            END DO

*  Increment the vector address of the start of the next row.
            V0 = V0 + M

         END DO
         
*  Now set up the increment between adjacent elements of "rows" parallel
*  to the second dimension.
         INC = DIM( 1 )         

*  Process the remaining dimensions. Store the durrent dimensions.
         DO I = 2, NDIM
            M = DIM( I )
            
*  Initialise the co-ordinates (vector and Cartesian) of the first
*  element of the first row.
            V0 = 1

            DO J = 1, NDIM
               CART( J ) = 1
            END DO

*  Initialise the FFT work array for this dimension, and save the index
*  of the next un-used element in the work array. 
            CALL PDA_CFFTI( M, WORK )
            IWN = 4*M + 16
            
*  Store the step (in vector address) between the end of one "row" and
*  the start of the next.
            STEP = INC*( M - 1 )            

*  Loop round each "row" parallel to the current dimensions.
            DO J = 1, N/M

*  Copy the current "row" into the work space.
               V = V0
               IW = IWN

               DO K = 1, M
                  WORK( IW ) = REAL(DATA( V ))
                  WORK( IW + 1 ) = AIMAG(DATA( V ))
                  V = V + INC
                  IW = IW + 2
               END DO

*  Take the FFT of the current "row".
               CALL PDA_CFFTB( M, WORK( IWN ), WORK )               

*  Copy the FFT of the current "row" back into the supplied array.
               V = V0
               IW = IWN

               DO K = 1, M
                  DATA( V ) = CMPLX(WORK( IW ),WORK( IW + 1 ))
                  V = V + INC
                  IW = IW + 2
               END DO
   
*  Increment the co-ordinates of the start of the current "row".
               V0 = V0 + 1
               K = 1
               CART( 1 ) = CART( 1 ) + 1

*  If the upper pixel index bound for the current dimension has been
*  exceeded, reset the pixel index to 1 and increment the next
*  dimension. If the next dimension is the dimension currently being
*  transformed, skip over it so that it stays at 1 (but increment the
*  vector address to account for the skip).
C
C  The following 16 lines contain code corrected by Alexis Rohou
C  7 Feb 13
               DO WHILE( (K .LE. NDIM))
                  IF ((CART( K ) .GT. DIM( K )) ) THEN
                      CART( K ) = 1
                      K = K + 1

                      IF( K .EQ. I ) THEN
                         K = K + 1
                         V0 = V0 + STEP
                      END IF

                      CART( K ) = CART( K ) + 1
                  ELSE
                      K = K + 1
                  ENDIF

               END DO

            END DO

*  Store the increment in vector address between adjacent elements of
*  the next "row".
            INC = INC*M
            
         END DO

      END IF
         
      END
      SUBROUTINE PDA_NFFTF( NDIM, DIM, DATA, WORK, ISTAT )
*+
*  Name:
*     PDA_NFFTF

*  Purpose:
*     Take the forward FFT of an N-dimensional complex array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PDA_NFFTF( NDIM, DIM, X, Y, WORK, ISTAT )

*  Description:
*     The supplied data values in X and Y are replaced by the 
*     co-efficients of the Fourier transform of the supplied data.
*     The co-efficients are normalised so that a subsequent call to
*     PDA_NFFTB to perform a backward FFT would restore the original data
*     values.
*
*     The multi-dimensional FFT is implemented using 1-dimensional FFTPACK
*     routines. First each row (i.e. a line of pixels parallel to the first
*     axis) in the supplied array is transformed, the Fourier co-efficients 
*     replacing the supplied data. Then each column (i.e. a line of pixels
*     parallel to the second axis) is transformed. Then each line of pixels
*     parallel to the third axis is transformed, etc. Each dimension is 
*     transformed in this way. Most of the complications in the code come
*     from needing to work in an unknown number of dimensions. Two
*     addressing systems are used for each pixel; 1) the vector (i.e.
*     1-dimensional ) index into the supplied arrays, and 2) the
*     corresponding Cartesian pixel indices.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of dimensions. This should be no more than 20.
*     DIM( NDIM ) = INTEGER (Given)
*        The size of each dimension.
*     X( * ) = REAL (Given and Returned)
*        Supplied holding the real parts of the complex data
*        values. Returned holding the real parts of the Fourier
*        co-efficients. The array should have the number of elements
*        implied by NDIM ande DIM.
*     Y( * ) = REAL (Given and Returned)
*        Supplied holding the imaginary parts of the complex data
*        values. Returned holding the imaginary parts of the Fourier
*        co-efficients. The array should have the number of elements
*        implied by NDIM ande DIM.
*     WORK( * ) = REAL (Given and Returned)
*        A work array. This should have at least ( 6*DimMax + 15 )
*        elements where DimMax is the maximum of the values supplied in
*        DIM.
*     ISTAT = INTEGER (Returned)
*        If the value of NDIM is greater than 20 or less than 1, then
*        ISTAT is returned equal to 1, and the values in X and Y are
*        left unchanged. Otherwise, ISTAT is returned equal to 0.
      
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-FEB-1995 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER NDIM
      INTEGER DIM( NDIM )
      
*  Arguments Given and Returned:
C      REAL X( * )
C      REAL Y( * )
      COMPLEX DATA( * )
      REAL WORK( * )

*  Arguments Returned:
      INTEGER ISTAT
      
*  Local Constants:
      INTEGER MXDIM              ! Max number of dimensions
      PARAMETER( MXDIM = 20 )
      
*  Local Variables:
      INTEGER
     :     CART( MXDIM + 1 ),    ! Current Cartesian pixel indices
     :     I,                    ! Index of current dimension
     :     INC,                  ! Vector increment to next row element
     :     IW,                   ! Index into the work array
     :     IWN,                  ! Index of first free work array element
     :     J,                    ! Row counter
     :     K,                    ! Pixel index on current axis
     :     M,                    ! Size of current dimension
     :     N,                    ! Total no. of pixels
     :     STEP,                 ! Vector step to start of next row
     :     V,                    ! Vector address
     :     V0                    ! Vector address of start of current row

      REAL
     :     FAC                   ! Normalisation factor

*.

*  Check that the supplied number of dimensions is not too high, and
*  not too low. Return 1 for the status variable and abort otherwise.
      IF( NDIM .GT. MXDIM .OR. NDIM .LE. 0 ) THEN
         ISTAT = 1

*  If the number of dimensions is ok, return 0 for the status value and
*  continue.
      ELSE
         ISTAT = 0

*  Find the total number of pixels.
         N = 1
         DO I = 1, NDIM
            N = N*DIM( I )
         END DO

*  The first dimension can be processed using a faster algorithm
*  because the elements to be processed occupy adjacent elements in the
*  supplied array. Set up the step (in vector address) between the
*  start of each row, and initialise the vector address of the start of
*  the first row.
         M = DIM( 1 )
         V0 = 1

*  Initialise the FFT work array for the current dimension. Save the
*  index of the next un-used element of the work array.
         CALL PDA_CFFTI( M, WORK )
         IWN = 4*M + 16

*  Store the factor which will normalise the Fourier co-efficients
*  returned by this routine (i.e. so that a call to PDA_NFFTF followed by a
*  call to PDA_NFFTB will result in no change to the data).
C         FAC = 1.0/SQRT( REAL ( N ) )

*  Loop round copying each row.
         DO J = 1, N/M

*  Copy this row into the unused part of the work array.
            IW = IWN
            V = V0
            DO K = 1, M
               WORK( IW ) = REAL(DATA( V ))
               WORK( IW + 1 ) = AIMAG(DATA( V ))
               IW = IW + 2
               V = V + 1
            END DO

*  Take the FFT of it.
            CALL PDA_CFFTF( M, WORK( IWN ), WORK )         

*  Copy it back to the supplied arrays, normalising it in the process.
            IW = IWN
            V = V0
            DO K = 1, M
               DATA( V ) = CMPLX(WORK( IW ),WORK( IW + 1 ))
               IW = IW + 2
               V = V + 1
            END DO

*  Increment the vector address of the start of the next row.
            V0 = V0 + M

         END DO
         
*  Now set up the increment between adjacent elements of "rows" parallel
*  to the second dimension.
         INC = DIM( 1 )         

*  Process the remaining dimensions. Store the durrent dimensions.
         DO I = 2, NDIM
            M = DIM( I )
            
*  Initialise the co-ordinates (vector and Cartesian) of the first
*  element of the first row.
            V0 = 1

            DO J = 1, NDIM
               CART( J ) = 1
            END DO

*  Initialise the FFT work array for this dimension, and save the index
*  of the next un-used element in the work array. 
            CALL PDA_CFFTI( M, WORK )
            IWN = 4*M + 16
            
*  Store the step (in vector address) between the end of one "row" and
*  the start of the next.
            STEP = INC*( M - 1 )            

*  Loop round each "row" parallel to the current dimensions.
            DO J = 1, N/M

*  Copy the current "row" into the work space.
               V = V0
               IW = IWN

               DO K = 1, M
                  WORK( IW ) = REAL(DATA( V ))
                  WORK( IW + 1 ) = AIMAG(DATA( V ))
                  V = V + INC
                  IW = IW + 2
               END DO

*  Take the FFT of the current "row".
               CALL PDA_CFFTF( M, WORK( IWN ), WORK )               

*  Copy the FFT of the current "row" back into the supplied array.
               V = V0
               IW = IWN

               DO K = 1, M
                  DATA( V ) = CMPLX(WORK( IW ),WORK( IW + 1 ))
                  V = V + INC
                  IW = IW + 2
               END DO

*  Increment the co-ordinates of the start of the current "row".
               V0 = V0 + 1
               K = 1
               CART( 1 ) = CART( 1 ) + 1

*  If the upper pixel index bound for the current dimension has been
*  exceeded, reset the pixel index to 1 and increment the next
*  dimension. If the next dimension is the dimension currently being
*  transformed, skip over it so that it stays at 1 (but increment the
*  vector address to account for the skip).
C
C  The following 16 lines contain code corrected by Alexis Rohou
C  7 Feb 13
               DO WHILE( (K .LE. NDIM))
                  IF ((CART( K ) .GT. DIM( K )) ) THEN
                      CART( K ) = 1
                      K = K + 1

                      IF( K .EQ. I ) THEN
                         K = K + 1
                         V0 = V0 + STEP
                      END IF

                      CART( K ) = CART( K ) + 1
                  ELSE
                      K = K + 1
                  ENDIF

               END DO

            END DO

*  Store the increment in vector address between adjacent elements of
*  the next "row".
            INC = INC*M

         END DO

      END IF

      END
      SUBROUTINE PDA_PASSB2 (IDO,L1,CC,CH,WA1)
      DIMENSION       CC(IDO,2,L1)           ,CH(IDO,L1,2)           ,
     1                WA1(*)
      IF (IDO .GT. 2) GO TO 102
      DO 101 K=1,L1
         CH(1,K,1) = CC(1,1,K)+CC(1,2,K)
         CH(1,K,2) = CC(1,1,K)-CC(1,2,K)
         CH(2,K,1) = CC(2,1,K)+CC(2,2,K)
         CH(2,K,2) = CC(2,1,K)-CC(2,2,K)
  101 CONTINUE
      RETURN
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            CH(I-1,K,1) = CC(I-1,1,K)+CC(I-1,2,K)
            TR2 = CC(I-1,1,K)-CC(I-1,2,K)
            CH(I,K,1) = CC(I,1,K)+CC(I,2,K)
            TI2 = CC(I,1,K)-CC(I,2,K)
            CH(I,K,2) = WA1(I-1)*TI2+WA1(I)*TR2
            CH(I-1,K,2) = WA1(I-1)*TR2-WA1(I)*TI2
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSB3 (IDO,L1,CC,CH,WA1,WA2)
      DIMENSION       CC(IDO,3,L1)           ,CH(IDO,L1,3)           ,
     1                WA1(*)     ,WA2(*)
      DATA TAUR,TAUI /-.5,.866025403784439/
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TR2 = CC(1,2,K)+CC(1,3,K)
         CR2 = CC(1,1,K)+TAUR*TR2
         CH(1,K,1) = CC(1,1,K)+TR2
         TI2 = CC(2,2,K)+CC(2,3,K)
         CI2 = CC(2,1,K)+TAUR*TI2
         CH(2,K,1) = CC(2,1,K)+TI2
         CR3 = TAUI*(CC(1,2,K)-CC(1,3,K))
         CI3 = TAUI*(CC(2,2,K)-CC(2,3,K))
         CH(1,K,2) = CR2-CI3
         CH(1,K,3) = CR2+CI3
         CH(2,K,2) = CI2+CR3
         CH(2,K,3) = CI2-CR3
  101 CONTINUE
      RETURN
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TR2 = CC(I-1,2,K)+CC(I-1,3,K)
            CR2 = CC(I-1,1,K)+TAUR*TR2
            CH(I-1,K,1) = CC(I-1,1,K)+TR2
            TI2 = CC(I,2,K)+CC(I,3,K)
            CI2 = CC(I,1,K)+TAUR*TI2
            CH(I,K,1) = CC(I,1,K)+TI2
            CR3 = TAUI*(CC(I-1,2,K)-CC(I-1,3,K))
            CI3 = TAUI*(CC(I,2,K)-CC(I,3,K))
            DR2 = CR2-CI3
            DR3 = CR2+CI3
            DI2 = CI2+CR3
            DI3 = CI2-CR3
            CH(I,K,2) = WA1(I-1)*DI2+WA1(I)*DR2
            CH(I-1,K,2) = WA1(I-1)*DR2-WA1(I)*DI2
            CH(I,K,3) = WA2(I-1)*DI3+WA2(I)*DR3
            CH(I-1,K,3) = WA2(I-1)*DR3-WA2(I)*DI3
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSB4 (IDO,L1,CC,CH,WA1,WA2,WA3)
      DIMENSION       CC(IDO,4,L1)           ,CH(IDO,L1,4)           ,
     1                WA1(*)     ,WA2(*)     ,WA3(*)
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TI1 = CC(2,1,K)-CC(2,3,K)
         TI2 = CC(2,1,K)+CC(2,3,K)
         TR4 = CC(2,4,K)-CC(2,2,K)
         TI3 = CC(2,2,K)+CC(2,4,K)
         TR1 = CC(1,1,K)-CC(1,3,K)
         TR2 = CC(1,1,K)+CC(1,3,K)
         TI4 = CC(1,2,K)-CC(1,4,K)
         TR3 = CC(1,2,K)+CC(1,4,K)
         CH(1,K,1) = TR2+TR3
         CH(1,K,3) = TR2-TR3
         CH(2,K,1) = TI2+TI3
         CH(2,K,3) = TI2-TI3
         CH(1,K,2) = TR1+TR4
         CH(1,K,4) = TR1-TR4
         CH(2,K,2) = TI1+TI4
         CH(2,K,4) = TI1-TI4
  101 CONTINUE
      RETURN
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TI1 = CC(I,1,K)-CC(I,3,K)
            TI2 = CC(I,1,K)+CC(I,3,K)
            TI3 = CC(I,2,K)+CC(I,4,K)
            TR4 = CC(I,4,K)-CC(I,2,K)
            TR1 = CC(I-1,1,K)-CC(I-1,3,K)
            TR2 = CC(I-1,1,K)+CC(I-1,3,K)
            TI4 = CC(I-1,2,K)-CC(I-1,4,K)
            TR3 = CC(I-1,2,K)+CC(I-1,4,K)
            CH(I-1,K,1) = TR2+TR3
            CR3 = TR2-TR3
            CH(I,K,1) = TI2+TI3
            CI3 = TI2-TI3
            CR2 = TR1+TR4
            CR4 = TR1-TR4
            CI2 = TI1+TI4
            CI4 = TI1-TI4
            CH(I-1,K,2) = WA1(I-1)*CR2-WA1(I)*CI2
            CH(I,K,2) = WA1(I-1)*CI2+WA1(I)*CR2
            CH(I-1,K,3) = WA2(I-1)*CR3-WA2(I)*CI3
            CH(I,K,3) = WA2(I-1)*CI3+WA2(I)*CR3
            CH(I-1,K,4) = WA3(I-1)*CR4-WA3(I)*CI4
            CH(I,K,4) = WA3(I-1)*CI4+WA3(I)*CR4
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSB5 (IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
      DIMENSION       CC(IDO,5,L1)           ,CH(IDO,L1,5)           ,
     1                WA1(*)     ,WA2(*)     ,WA3(*)     ,WA4(*)
      DATA TR11,TI11,TR12,TI12 /.309016994374947,.951056516295154,
     1-.809016994374947,.587785252292473/
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TI5 = CC(2,2,K)-CC(2,5,K)
         TI2 = CC(2,2,K)+CC(2,5,K)
         TI4 = CC(2,3,K)-CC(2,4,K)
         TI3 = CC(2,3,K)+CC(2,4,K)
         TR5 = CC(1,2,K)-CC(1,5,K)
         TR2 = CC(1,2,K)+CC(1,5,K)
         TR4 = CC(1,3,K)-CC(1,4,K)
         TR3 = CC(1,3,K)+CC(1,4,K)
         CH(1,K,1) = CC(1,1,K)+TR2+TR3
         CH(2,K,1) = CC(2,1,K)+TI2+TI3
         CR2 = CC(1,1,K)+TR11*TR2+TR12*TR3
         CI2 = CC(2,1,K)+TR11*TI2+TR12*TI3
         CR3 = CC(1,1,K)+TR12*TR2+TR11*TR3
         CI3 = CC(2,1,K)+TR12*TI2+TR11*TI3
         CR5 = TI11*TR5+TI12*TR4
         CI5 = TI11*TI5+TI12*TI4
         CR4 = TI12*TR5-TI11*TR4
         CI4 = TI12*TI5-TI11*TI4
         CH(1,K,2) = CR2-CI5
         CH(1,K,5) = CR2+CI5
         CH(2,K,2) = CI2+CR5
         CH(2,K,3) = CI3+CR4
         CH(1,K,3) = CR3-CI4
         CH(1,K,4) = CR3+CI4
         CH(2,K,4) = CI3-CR4
         CH(2,K,5) = CI2-CR5
  101 CONTINUE
      RETURN
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TI5 = CC(I,2,K)-CC(I,5,K)
            TI2 = CC(I,2,K)+CC(I,5,K)
            TI4 = CC(I,3,K)-CC(I,4,K)
            TI3 = CC(I,3,K)+CC(I,4,K)
            TR5 = CC(I-1,2,K)-CC(I-1,5,K)
            TR2 = CC(I-1,2,K)+CC(I-1,5,K)
            TR4 = CC(I-1,3,K)-CC(I-1,4,K)
            TR3 = CC(I-1,3,K)+CC(I-1,4,K)
            CH(I-1,K,1) = CC(I-1,1,K)+TR2+TR3
            CH(I,K,1) = CC(I,1,K)+TI2+TI3
            CR2 = CC(I-1,1,K)+TR11*TR2+TR12*TR3
            CI2 = CC(I,1,K)+TR11*TI2+TR12*TI3
            CR3 = CC(I-1,1,K)+TR12*TR2+TR11*TR3
            CI3 = CC(I,1,K)+TR12*TI2+TR11*TI3
            CR5 = TI11*TR5+TI12*TR4
            CI5 = TI11*TI5+TI12*TI4
            CR4 = TI12*TR5-TI11*TR4
            CI4 = TI12*TI5-TI11*TI4
            DR3 = CR3-CI4
            DR4 = CR3+CI4
            DI3 = CI3+CR4
            DI4 = CI3-CR4
            DR5 = CR2+CI5
            DR2 = CR2-CI5
            DI5 = CI2-CR5
            DI2 = CI2+CR5
            CH(I-1,K,2) = WA1(I-1)*DR2-WA1(I)*DI2
            CH(I,K,2) = WA1(I-1)*DI2+WA1(I)*DR2
            CH(I-1,K,3) = WA2(I-1)*DR3-WA2(I)*DI3
            CH(I,K,3) = WA2(I-1)*DI3+WA2(I)*DR3
            CH(I-1,K,4) = WA3(I-1)*DR4-WA3(I)*DI4
            CH(I,K,4) = WA3(I-1)*DI4+WA3(I)*DR4
            CH(I-1,K,5) = WA4(I-1)*DR5-WA4(I)*DI5
            CH(I,K,5) = WA4(I-1)*DI5+WA4(I)*DR5
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSB (NAC,IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DIMENSION       CH(IDO,L1,IP)          ,CC(IDO,IP,L1)          ,
     1                C1(IDO,L1,IP)          ,WA(*)      ,C2(IDL1,IP),
     2                CH2(IDL1,IP)
      IDOT = IDO/2
      NT = IP*IDL1
      IPP2 = IP+2
      IPPH = (IP+1)/2
      IDP = IP*IDO
C
      IF (IDO .LT. L1) GO TO 106
      DO 103 J=2,IPPH
         JC = IPP2-J
         DO 102 K=1,L1
            DO 101 I=1,IDO
               CH(I,K,J) = CC(I,J,K)+CC(I,JC,K)
               CH(I,K,JC) = CC(I,J,K)-CC(I,JC,K)
  101       CONTINUE
  102    CONTINUE
  103 CONTINUE
      DO 105 K=1,L1
         DO 104 I=1,IDO
            CH(I,K,1) = CC(I,1,K)
  104    CONTINUE
  105 CONTINUE
      GO TO 112
  106 DO 109 J=2,IPPH
         JC = IPP2-J
         DO 108 I=1,IDO
            DO 107 K=1,L1
               CH(I,K,J) = CC(I,J,K)+CC(I,JC,K)
               CH(I,K,JC) = CC(I,J,K)-CC(I,JC,K)
  107       CONTINUE
  108    CONTINUE
  109 CONTINUE
      DO 111 I=1,IDO
         DO 110 K=1,L1
            CH(I,K,1) = CC(I,1,K)
  110    CONTINUE
  111 CONTINUE
  112 IDL = 2-IDO
      INC = 0
      DO 116 L=2,IPPH
         LC = IPP2-L
         IDL = IDL+IDO
         DO 113 IK=1,IDL1
            C2(IK,L) = CH2(IK,1)+WA(IDL-1)*CH2(IK,2)
            C2(IK,LC) = WA(IDL)*CH2(IK,IP)
  113    CONTINUE
         IDLJ = IDL
         INC = INC+IDO
         DO 115 J=3,IPPH
            JC = IPP2-J
            IDLJ = IDLJ+INC
            IF (IDLJ .GT. IDP) IDLJ = IDLJ-IDP
            WAR = WA(IDLJ-1)
            WAI = WA(IDLJ)
            DO 114 IK=1,IDL1
               C2(IK,L) = C2(IK,L)+WAR*CH2(IK,J)
               C2(IK,LC) = C2(IK,LC)+WAI*CH2(IK,JC)
  114       CONTINUE
  115    CONTINUE
  116 CONTINUE
      DO 118 J=2,IPPH
         DO 117 IK=1,IDL1
            CH2(IK,1) = CH2(IK,1)+CH2(IK,J)
  117    CONTINUE
  118 CONTINUE
      DO 120 J=2,IPPH
         JC = IPP2-J
         DO 119 IK=2,IDL1,2
            CH2(IK-1,J) = C2(IK-1,J)-C2(IK,JC)
            CH2(IK-1,JC) = C2(IK-1,J)+C2(IK,JC)
            CH2(IK,J) = C2(IK,J)+C2(IK-1,JC)
            CH2(IK,JC) = C2(IK,J)-C2(IK-1,JC)
  119    CONTINUE
  120 CONTINUE
      NAC = 1
      IF (IDO .EQ. 2) RETURN
      NAC = 0
      DO 121 IK=1,IDL1
         C2(IK,1) = CH2(IK,1)
  121 CONTINUE
      DO 123 J=2,IP
         DO 122 K=1,L1
            C1(1,K,J) = CH(1,K,J)
            C1(2,K,J) = CH(2,K,J)
  122    CONTINUE
  123 CONTINUE
      IF (IDOT .GT. L1) GO TO 127
      IDIJ = 0
      DO 126 J=2,IP
         IDIJ = IDIJ+2
         DO 125 I=4,IDO,2
            IDIJ = IDIJ+2
            DO 124 K=1,L1
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)-WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)+WA(IDIJ)*CH(I-1,K,J)
  124       CONTINUE
  125    CONTINUE
  126 CONTINUE
      RETURN
  127 IDJ = 2-IDO
      DO 130 J=2,IP
         IDJ = IDJ+IDO
         DO 129 K=1,L1
            IDIJ = IDJ
            DO 128 I=4,IDO,2
               IDIJ = IDIJ+2
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)-WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)+WA(IDIJ)*CH(I-1,K,J)
  128       CONTINUE
  129    CONTINUE
  130 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSF2 (IDO,L1,CC,CH,WA1)
      DIMENSION       CC(IDO,2,L1)           ,CH(IDO,L1,2)           ,
     1                WA1(*)
      IF (IDO .GT. 2) GO TO 102
      DO 101 K=1,L1
         CH(1,K,1) = CC(1,1,K)+CC(1,2,K)
         CH(1,K,2) = CC(1,1,K)-CC(1,2,K)
         CH(2,K,1) = CC(2,1,K)+CC(2,2,K)
         CH(2,K,2) = CC(2,1,K)-CC(2,2,K)
  101 CONTINUE
      RETURN
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            CH(I-1,K,1) = CC(I-1,1,K)+CC(I-1,2,K)
            TR2 = CC(I-1,1,K)-CC(I-1,2,K)
            CH(I,K,1) = CC(I,1,K)+CC(I,2,K)
            TI2 = CC(I,1,K)-CC(I,2,K)
            CH(I,K,2) = WA1(I-1)*TI2-WA1(I)*TR2
            CH(I-1,K,2) = WA1(I-1)*TR2+WA1(I)*TI2
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSF3 (IDO,L1,CC,CH,WA1,WA2)
      DIMENSION       CC(IDO,3,L1)           ,CH(IDO,L1,3)           ,
     1                WA1(*)     ,WA2(*)
      DATA TAUR,TAUI /-.5,-.866025403784439/
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TR2 = CC(1,2,K)+CC(1,3,K)
         CR2 = CC(1,1,K)+TAUR*TR2
         CH(1,K,1) = CC(1,1,K)+TR2
         TI2 = CC(2,2,K)+CC(2,3,K)
         CI2 = CC(2,1,K)+TAUR*TI2
         CH(2,K,1) = CC(2,1,K)+TI2
         CR3 = TAUI*(CC(1,2,K)-CC(1,3,K))
         CI3 = TAUI*(CC(2,2,K)-CC(2,3,K))
         CH(1,K,2) = CR2-CI3
         CH(1,K,3) = CR2+CI3
         CH(2,K,2) = CI2+CR3
         CH(2,K,3) = CI2-CR3
  101 CONTINUE
      RETURN
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TR2 = CC(I-1,2,K)+CC(I-1,3,K)
            CR2 = CC(I-1,1,K)+TAUR*TR2
            CH(I-1,K,1) = CC(I-1,1,K)+TR2
            TI2 = CC(I,2,K)+CC(I,3,K)
            CI2 = CC(I,1,K)+TAUR*TI2
            CH(I,K,1) = CC(I,1,K)+TI2
            CR3 = TAUI*(CC(I-1,2,K)-CC(I-1,3,K))
            CI3 = TAUI*(CC(I,2,K)-CC(I,3,K))
            DR2 = CR2-CI3
            DR3 = CR2+CI3
            DI2 = CI2+CR3
            DI3 = CI2-CR3
            CH(I,K,2) = WA1(I-1)*DI2-WA1(I)*DR2
            CH(I-1,K,2) = WA1(I-1)*DR2+WA1(I)*DI2
            CH(I,K,3) = WA2(I-1)*DI3-WA2(I)*DR3
            CH(I-1,K,3) = WA2(I-1)*DR3+WA2(I)*DI3
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSF4 (IDO,L1,CC,CH,WA1,WA2,WA3)
      DIMENSION       CC(IDO,4,L1)           ,CH(IDO,L1,4)           ,
     1                WA1(*)     ,WA2(*)     ,WA3(*)
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TI1 = CC(2,1,K)-CC(2,3,K)
         TI2 = CC(2,1,K)+CC(2,3,K)
         TR4 = CC(2,2,K)-CC(2,4,K)
         TI3 = CC(2,2,K)+CC(2,4,K)
         TR1 = CC(1,1,K)-CC(1,3,K)
         TR2 = CC(1,1,K)+CC(1,3,K)
         TI4 = CC(1,4,K)-CC(1,2,K)
         TR3 = CC(1,2,K)+CC(1,4,K)
         CH(1,K,1) = TR2+TR3
         CH(1,K,3) = TR2-TR3
         CH(2,K,1) = TI2+TI3
         CH(2,K,3) = TI2-TI3
         CH(1,K,2) = TR1+TR4
         CH(1,K,4) = TR1-TR4
         CH(2,K,2) = TI1+TI4
         CH(2,K,4) = TI1-TI4
  101 CONTINUE
      RETURN
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TI1 = CC(I,1,K)-CC(I,3,K)
            TI2 = CC(I,1,K)+CC(I,3,K)
            TI3 = CC(I,2,K)+CC(I,4,K)
            TR4 = CC(I,2,K)-CC(I,4,K)
            TR1 = CC(I-1,1,K)-CC(I-1,3,K)
            TR2 = CC(I-1,1,K)+CC(I-1,3,K)
            TI4 = CC(I-1,4,K)-CC(I-1,2,K)
            TR3 = CC(I-1,2,K)+CC(I-1,4,K)
            CH(I-1,K,1) = TR2+TR3
            CR3 = TR2-TR3
            CH(I,K,1) = TI2+TI3
            CI3 = TI2-TI3
            CR2 = TR1+TR4
            CR4 = TR1-TR4
            CI2 = TI1+TI4
            CI4 = TI1-TI4
            CH(I-1,K,2) = WA1(I-1)*CR2+WA1(I)*CI2
            CH(I,K,2) = WA1(I-1)*CI2-WA1(I)*CR2
            CH(I-1,K,3) = WA2(I-1)*CR3+WA2(I)*CI3
            CH(I,K,3) = WA2(I-1)*CI3-WA2(I)*CR3
            CH(I-1,K,4) = WA3(I-1)*CR4+WA3(I)*CI4
            CH(I,K,4) = WA3(I-1)*CI4-WA3(I)*CR4
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSF5 (IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
      DIMENSION       CC(IDO,5,L1)           ,CH(IDO,L1,5)           ,
     1                WA1(*)     ,WA2(*)     ,WA3(*)     ,WA4(*)
      DATA TR11,TI11,TR12,TI12 /.309016994374947,-.951056516295154,
     1-.809016994374947,-.587785252292473/
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TI5 = CC(2,2,K)-CC(2,5,K)
         TI2 = CC(2,2,K)+CC(2,5,K)
         TI4 = CC(2,3,K)-CC(2,4,K)
         TI3 = CC(2,3,K)+CC(2,4,K)
         TR5 = CC(1,2,K)-CC(1,5,K)
         TR2 = CC(1,2,K)+CC(1,5,K)
         TR4 = CC(1,3,K)-CC(1,4,K)
         TR3 = CC(1,3,K)+CC(1,4,K)
         CH(1,K,1) = CC(1,1,K)+TR2+TR3
         CH(2,K,1) = CC(2,1,K)+TI2+TI3
         CR2 = CC(1,1,K)+TR11*TR2+TR12*TR3
         CI2 = CC(2,1,K)+TR11*TI2+TR12*TI3
         CR3 = CC(1,1,K)+TR12*TR2+TR11*TR3
         CI3 = CC(2,1,K)+TR12*TI2+TR11*TI3
         CR5 = TI11*TR5+TI12*TR4
         CI5 = TI11*TI5+TI12*TI4
         CR4 = TI12*TR5-TI11*TR4
         CI4 = TI12*TI5-TI11*TI4
         CH(1,K,2) = CR2-CI5
         CH(1,K,5) = CR2+CI5
         CH(2,K,2) = CI2+CR5
         CH(2,K,3) = CI3+CR4
         CH(1,K,3) = CR3-CI4
         CH(1,K,4) = CR3+CI4
         CH(2,K,4) = CI3-CR4
         CH(2,K,5) = CI2-CR5
  101 CONTINUE
      RETURN
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TI5 = CC(I,2,K)-CC(I,5,K)
            TI2 = CC(I,2,K)+CC(I,5,K)
            TI4 = CC(I,3,K)-CC(I,4,K)
            TI3 = CC(I,3,K)+CC(I,4,K)
            TR5 = CC(I-1,2,K)-CC(I-1,5,K)
            TR2 = CC(I-1,2,K)+CC(I-1,5,K)
            TR4 = CC(I-1,3,K)-CC(I-1,4,K)
            TR3 = CC(I-1,3,K)+CC(I-1,4,K)
            CH(I-1,K,1) = CC(I-1,1,K)+TR2+TR3
            CH(I,K,1) = CC(I,1,K)+TI2+TI3
            CR2 = CC(I-1,1,K)+TR11*TR2+TR12*TR3
            CI2 = CC(I,1,K)+TR11*TI2+TR12*TI3
            CR3 = CC(I-1,1,K)+TR12*TR2+TR11*TR3
            CI3 = CC(I,1,K)+TR12*TI2+TR11*TI3
            CR5 = TI11*TR5+TI12*TR4
            CI5 = TI11*TI5+TI12*TI4
            CR4 = TI12*TR5-TI11*TR4
            CI4 = TI12*TI5-TI11*TI4
            DR3 = CR3-CI4
            DR4 = CR3+CI4
            DI3 = CI3+CR4
            DI4 = CI3-CR4
            DR5 = CR2+CI5
            DR2 = CR2-CI5
            DI5 = CI2-CR5
            DI2 = CI2+CR5
            CH(I-1,K,2) = WA1(I-1)*DR2+WA1(I)*DI2
            CH(I,K,2) = WA1(I-1)*DI2-WA1(I)*DR2
            CH(I-1,K,3) = WA2(I-1)*DR3+WA2(I)*DI3
            CH(I,K,3) = WA2(I-1)*DI3-WA2(I)*DR3
            CH(I-1,K,4) = WA3(I-1)*DR4+WA3(I)*DI4
            CH(I,K,4) = WA3(I-1)*DI4-WA3(I)*DR4
            CH(I-1,K,5) = WA4(I-1)*DR5+WA4(I)*DI5
            CH(I,K,5) = WA4(I-1)*DI5-WA4(I)*DR5
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSF (NAC,IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DIMENSION       CH(IDO,L1,IP)          ,CC(IDO,IP,L1)          ,
     1                C1(IDO,L1,IP)          ,WA(*)      ,C2(IDL1,IP),
     2                CH2(IDL1,IP)
      IDOT = IDO/2
      NT = IP*IDL1
      IPP2 = IP+2
      IPPH = (IP+1)/2
      IDP = IP*IDO
C
      IF (IDO .LT. L1) GO TO 106
      DO 103 J=2,IPPH
         JC = IPP2-J
         DO 102 K=1,L1
            DO 101 I=1,IDO
               CH(I,K,J) = CC(I,J,K)+CC(I,JC,K)
               CH(I,K,JC) = CC(I,J,K)-CC(I,JC,K)
  101       CONTINUE
  102    CONTINUE
  103 CONTINUE
      DO 105 K=1,L1
         DO 104 I=1,IDO
            CH(I,K,1) = CC(I,1,K)
  104    CONTINUE
  105 CONTINUE
      GO TO 112
  106 DO 109 J=2,IPPH
         JC = IPP2-J
         DO 108 I=1,IDO
            DO 107 K=1,L1
               CH(I,K,J) = CC(I,J,K)+CC(I,JC,K)
               CH(I,K,JC) = CC(I,J,K)-CC(I,JC,K)
  107       CONTINUE
  108    CONTINUE
  109 CONTINUE
      DO 111 I=1,IDO
         DO 110 K=1,L1
            CH(I,K,1) = CC(I,1,K)
  110    CONTINUE
  111 CONTINUE
  112 IDL = 2-IDO
      INC = 0
      DO 116 L=2,IPPH
         LC = IPP2-L
         IDL = IDL+IDO
         DO 113 IK=1,IDL1
            C2(IK,L) = CH2(IK,1)+WA(IDL-1)*CH2(IK,2)
            C2(IK,LC) = -WA(IDL)*CH2(IK,IP)
  113    CONTINUE
         IDLJ = IDL
         INC = INC+IDO
         DO 115 J=3,IPPH
            JC = IPP2-J
            IDLJ = IDLJ+INC
            IF (IDLJ .GT. IDP) IDLJ = IDLJ-IDP
            WAR = WA(IDLJ-1)
            WAI = WA(IDLJ)
            DO 114 IK=1,IDL1
               C2(IK,L) = C2(IK,L)+WAR*CH2(IK,J)
               C2(IK,LC) = C2(IK,LC)-WAI*CH2(IK,JC)
  114       CONTINUE
  115    CONTINUE
  116 CONTINUE
      DO 118 J=2,IPPH
         DO 117 IK=1,IDL1
            CH2(IK,1) = CH2(IK,1)+CH2(IK,J)
  117    CONTINUE
  118 CONTINUE
      DO 120 J=2,IPPH
         JC = IPP2-J
         DO 119 IK=2,IDL1,2
            CH2(IK-1,J) = C2(IK-1,J)-C2(IK,JC)
            CH2(IK-1,JC) = C2(IK-1,J)+C2(IK,JC)
            CH2(IK,J) = C2(IK,J)+C2(IK-1,JC)
            CH2(IK,JC) = C2(IK,J)-C2(IK-1,JC)
  119    CONTINUE
  120 CONTINUE
      NAC = 1
      IF (IDO .EQ. 2) RETURN
      NAC = 0
      DO 121 IK=1,IDL1
         C2(IK,1) = CH2(IK,1)
  121 CONTINUE
      DO 123 J=2,IP
         DO 122 K=1,L1
            C1(1,K,J) = CH(1,K,J)
            C1(2,K,J) = CH(2,K,J)
  122    CONTINUE
  123 CONTINUE
      IF (IDOT .GT. L1) GO TO 127
      IDIJ = 0
      DO 126 J=2,IP
         IDIJ = IDIJ+2
         DO 125 I=4,IDO,2
            IDIJ = IDIJ+2
            DO 124 K=1,L1
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)+WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)-WA(IDIJ)*CH(I-1,K,J)
  124       CONTINUE
  125    CONTINUE
  126 CONTINUE
      RETURN
  127 IDJ = 2-IDO
      DO 130 J=2,IP
         IDJ = IDJ+IDO
         DO 129 K=1,L1
            IDIJ = IDJ
            DO 128 I=4,IDO,2
               IDIJ = IDIJ+2
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)+WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)-WA(IDIJ)*CH(I-1,K,J)
  128       CONTINUE
  129    CONTINUE
  130 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_RADB2 (IDO,L1,CC,CH,WA1)
      DIMENSION       CC(IDO,2,L1)           ,CH(IDO,L1,2)           ,
     1                WA1(*)
      DO 101 K=1,L1
         CH(1,K,1) = CC(1,1,K)+CC(IDO,2,K)
         CH(1,K,2) = CC(1,1,K)-CC(IDO,2,K)
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            CH(I-1,K,1) = CC(I-1,1,K)+CC(IC-1,2,K)
            TR2 = CC(I-1,1,K)-CC(IC-1,2,K)
            CH(I,K,1) = CC(I,1,K)-CC(IC,2,K)
            TI2 = CC(I,1,K)+CC(IC,2,K)
            CH(I-1,K,2) = WA1(I-2)*TR2-WA1(I-1)*TI2
            CH(I,K,2) = WA1(I-2)*TI2+WA1(I-1)*TR2
  103    CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
  105 DO 106 K=1,L1
         CH(IDO,K,1) = CC(IDO,1,K)+CC(IDO,1,K)
         CH(IDO,K,2) = -(CC(1,2,K)+CC(1,2,K))
  106 CONTINUE
  107 RETURN
      END
      SUBROUTINE PDA_RADB3 (IDO,L1,CC,CH,WA1,WA2)
      DIMENSION       CC(IDO,3,L1)           ,CH(IDO,L1,3)           ,
     1                WA1(*)     ,WA2(*)
      DATA TAUR,TAUI /-.5,.866025403784439/
      DO 101 K=1,L1
         TR2 = CC(IDO,2,K)+CC(IDO,2,K)
         CR2 = CC(1,1,K)+TAUR*TR2
         CH(1,K,1) = CC(1,1,K)+TR2
         CI3 = TAUI*(CC(1,3,K)+CC(1,3,K))
         CH(1,K,2) = CR2-CI3
         CH(1,K,3) = CR2+CI3
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            TR2 = CC(I-1,3,K)+CC(IC-1,2,K)
            CR2 = CC(I-1,1,K)+TAUR*TR2
            CH(I-1,K,1) = CC(I-1,1,K)+TR2
            TI2 = CC(I,3,K)-CC(IC,2,K)
            CI2 = CC(I,1,K)+TAUR*TI2
            CH(I,K,1) = CC(I,1,K)+TI2
            CR3 = TAUI*(CC(I-1,3,K)-CC(IC-1,2,K))
            CI3 = TAUI*(CC(I,3,K)+CC(IC,2,K))
            DR2 = CR2-CI3
            DR3 = CR2+CI3
            DI2 = CI2+CR3
            DI3 = CI2-CR3
            CH(I-1,K,2) = WA1(I-2)*DR2-WA1(I-1)*DI2
            CH(I,K,2) = WA1(I-2)*DI2+WA1(I-1)*DR2
            CH(I-1,K,3) = WA2(I-2)*DR3-WA2(I-1)*DI3
            CH(I,K,3) = WA2(I-2)*DI3+WA2(I-1)*DR3
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_RADB4 (IDO,L1,CC,CH,WA1,WA2,WA3)
      DIMENSION       CC(IDO,4,L1)           ,CH(IDO,L1,4)           ,
     1                WA1(*)     ,WA2(*)     ,WA3(*)
      DATA SQRT2 /1.414213562373095/
      DO 101 K=1,L1
         TR1 = CC(1,1,K)-CC(IDO,4,K)
         TR2 = CC(1,1,K)+CC(IDO,4,K)
         TR3 = CC(IDO,2,K)+CC(IDO,2,K)
         TR4 = CC(1,3,K)+CC(1,3,K)
         CH(1,K,1) = TR2+TR3
         CH(1,K,2) = TR1-TR4
         CH(1,K,3) = TR2-TR3
         CH(1,K,4) = TR1+TR4
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            TI1 = CC(I,1,K)+CC(IC,4,K)
            TI2 = CC(I,1,K)-CC(IC,4,K)
            TI3 = CC(I,3,K)-CC(IC,2,K)
            TR4 = CC(I,3,K)+CC(IC,2,K)
            TR1 = CC(I-1,1,K)-CC(IC-1,4,K)
            TR2 = CC(I-1,1,K)+CC(IC-1,4,K)
            TI4 = CC(I-1,3,K)-CC(IC-1,2,K)
            TR3 = CC(I-1,3,K)+CC(IC-1,2,K)
            CH(I-1,K,1) = TR2+TR3
            CR3 = TR2-TR3
            CH(I,K,1) = TI2+TI3
            CI3 = TI2-TI3
            CR2 = TR1-TR4
            CR4 = TR1+TR4
            CI2 = TI1+TI4
            CI4 = TI1-TI4
            CH(I-1,K,2) = WA1(I-2)*CR2-WA1(I-1)*CI2
            CH(I,K,2) = WA1(I-2)*CI2+WA1(I-1)*CR2
            CH(I-1,K,3) = WA2(I-2)*CR3-WA2(I-1)*CI3
            CH(I,K,3) = WA2(I-2)*CI3+WA2(I-1)*CR3
            CH(I-1,K,4) = WA3(I-2)*CR4-WA3(I-1)*CI4
            CH(I,K,4) = WA3(I-2)*CI4+WA3(I-1)*CR4
  103    CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
  105 CONTINUE
      DO 106 K=1,L1
         TI1 = CC(1,2,K)+CC(1,4,K)
         TI2 = CC(1,4,K)-CC(1,2,K)
         TR1 = CC(IDO,1,K)-CC(IDO,3,K)
         TR2 = CC(IDO,1,K)+CC(IDO,3,K)
         CH(IDO,K,1) = TR2+TR2
         CH(IDO,K,2) = SQRT2*(TR1-TI1)
         CH(IDO,K,3) = TI2+TI2
         CH(IDO,K,4) = -SQRT2*(TR1+TI1)
  106 CONTINUE
  107 RETURN
      END
      SUBROUTINE PDA_RADB5 (IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
      DIMENSION       CC(IDO,5,L1)           ,CH(IDO,L1,5)           ,
     1                WA1(*)     ,WA2(*)     ,WA3(*)     ,WA4(*)
      DATA TR11,TI11,TR12,TI12 /.309016994374947,.951056516295154,
     1-.809016994374947,.587785252292473/
      DO 101 K=1,L1
         TI5 = CC(1,3,K)+CC(1,3,K)
         TI4 = CC(1,5,K)+CC(1,5,K)
         TR2 = CC(IDO,2,K)+CC(IDO,2,K)
         TR3 = CC(IDO,4,K)+CC(IDO,4,K)
         CH(1,K,1) = CC(1,1,K)+TR2+TR3
         CR2 = CC(1,1,K)+TR11*TR2+TR12*TR3
         CR3 = CC(1,1,K)+TR12*TR2+TR11*TR3
         CI5 = TI11*TI5+TI12*TI4
         CI4 = TI12*TI5-TI11*TI4
         CH(1,K,2) = CR2-CI5
         CH(1,K,3) = CR3-CI4
         CH(1,K,4) = CR3+CI4
         CH(1,K,5) = CR2+CI5
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            TI5 = CC(I,3,K)+CC(IC,2,K)
            TI2 = CC(I,3,K)-CC(IC,2,K)
            TI4 = CC(I,5,K)+CC(IC,4,K)
            TI3 = CC(I,5,K)-CC(IC,4,K)
            TR5 = CC(I-1,3,K)-CC(IC-1,2,K)
            TR2 = CC(I-1,3,K)+CC(IC-1,2,K)
            TR4 = CC(I-1,5,K)-CC(IC-1,4,K)
            TR3 = CC(I-1,5,K)+CC(IC-1,4,K)
            CH(I-1,K,1) = CC(I-1,1,K)+TR2+TR3
            CH(I,K,1) = CC(I,1,K)+TI2+TI3
            CR2 = CC(I-1,1,K)+TR11*TR2+TR12*TR3
            CI2 = CC(I,1,K)+TR11*TI2+TR12*TI3
            CR3 = CC(I-1,1,K)+TR12*TR2+TR11*TR3
            CI3 = CC(I,1,K)+TR12*TI2+TR11*TI3
            CR5 = TI11*TR5+TI12*TR4
            CI5 = TI11*TI5+TI12*TI4
            CR4 = TI12*TR5-TI11*TR4
            CI4 = TI12*TI5-TI11*TI4
            DR3 = CR3-CI4
            DR4 = CR3+CI4
            DI3 = CI3+CR4
            DI4 = CI3-CR4
            DR5 = CR2+CI5
            DR2 = CR2-CI5
            DI5 = CI2-CR5
            DI2 = CI2+CR5
            CH(I-1,K,2) = WA1(I-2)*DR2-WA1(I-1)*DI2
            CH(I,K,2) = WA1(I-2)*DI2+WA1(I-1)*DR2
            CH(I-1,K,3) = WA2(I-2)*DR3-WA2(I-1)*DI3
            CH(I,K,3) = WA2(I-2)*DI3+WA2(I-1)*DR3
            CH(I-1,K,4) = WA3(I-2)*DR4-WA3(I-1)*DI4
            CH(I,K,4) = WA3(I-2)*DI4+WA3(I-1)*DR4
            CH(I-1,K,5) = WA4(I-2)*DR5-WA4(I-1)*DI5
            CH(I,K,5) = WA4(I-2)*DI5+WA4(I-1)*DR5
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_RADBG (IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DIMENSION       CH(IDO,L1,IP)          ,CC(IDO,IP,L1)          ,
     1                C1(IDO,L1,IP)          ,C2(IDL1,IP),
     2                CH2(IDL1,IP)           ,WA(*)
      DATA TPI/6.28318530717959/
      ARG = TPI/FLOAT(IP)
      DCP = COS(ARG)
      DSP = SIN(ARG)
      IDP2 = IDO+2
      NBD = (IDO-1)/2
      IPP2 = IP+2
      IPPH = (IP+1)/2
      IF (IDO .LT. L1) GO TO 103
      DO 102 K=1,L1
         DO 101 I=1,IDO
            CH(I,K,1) = CC(I,1,K)
  101    CONTINUE
  102 CONTINUE
      GO TO 106
  103 DO 105 I=1,IDO
         DO 104 K=1,L1
            CH(I,K,1) = CC(I,1,K)
  104    CONTINUE
  105 CONTINUE
  106 DO 108 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 107 K=1,L1
            CH(1,K,J) = CC(IDO,J2-2,K)+CC(IDO,J2-2,K)
            CH(1,K,JC) = CC(1,J2-1,K)+CC(1,J2-1,K)
  107    CONTINUE
  108 CONTINUE
      IF (IDO .EQ. 1) GO TO 116
      IF (NBD .LT. L1) GO TO 112
      DO 111 J=2,IPPH
         JC = IPP2-J
         DO 110 K=1,L1
            DO 109 I=3,IDO,2
               IC = IDP2-I
               CH(I-1,K,J) = CC(I-1,2*J-1,K)+CC(IC-1,2*J-2,K)
               CH(I-1,K,JC) = CC(I-1,2*J-1,K)-CC(IC-1,2*J-2,K)
               CH(I,K,J) = CC(I,2*J-1,K)-CC(IC,2*J-2,K)
               CH(I,K,JC) = CC(I,2*J-1,K)+CC(IC,2*J-2,K)
  109       CONTINUE
  110    CONTINUE
  111 CONTINUE
      GO TO 116
  112 DO 115 J=2,IPPH
         JC = IPP2-J
         DO 114 I=3,IDO,2
            IC = IDP2-I
            DO 113 K=1,L1
               CH(I-1,K,J) = CC(I-1,2*J-1,K)+CC(IC-1,2*J-2,K)
               CH(I-1,K,JC) = CC(I-1,2*J-1,K)-CC(IC-1,2*J-2,K)
               CH(I,K,J) = CC(I,2*J-1,K)-CC(IC,2*J-2,K)
               CH(I,K,JC) = CC(I,2*J-1,K)+CC(IC,2*J-2,K)
  113       CONTINUE
  114    CONTINUE
  115 CONTINUE
  116 AR1 = 1.
      AI1 = 0.
      DO 120 L=2,IPPH
         LC = IPP2-L
         AR1H = DCP*AR1-DSP*AI1
         AI1 = DCP*AI1+DSP*AR1
         AR1 = AR1H
         DO 117 IK=1,IDL1
            C2(IK,L) = CH2(IK,1)+AR1*CH2(IK,2)
            C2(IK,LC) = AI1*CH2(IK,IP)
  117    CONTINUE
         DC2 = AR1
         DS2 = AI1
         AR2 = AR1
         AI2 = AI1
         DO 119 J=3,IPPH
            JC = IPP2-J
            AR2H = DC2*AR2-DS2*AI2
            AI2 = DC2*AI2+DS2*AR2
            AR2 = AR2H
            DO 118 IK=1,IDL1
               C2(IK,L) = C2(IK,L)+AR2*CH2(IK,J)
               C2(IK,LC) = C2(IK,LC)+AI2*CH2(IK,JC)
  118       CONTINUE
  119    CONTINUE
  120 CONTINUE
      DO 122 J=2,IPPH
         DO 121 IK=1,IDL1
            CH2(IK,1) = CH2(IK,1)+CH2(IK,J)
  121    CONTINUE
  122 CONTINUE
      DO 124 J=2,IPPH
         JC = IPP2-J
         DO 123 K=1,L1
            CH(1,K,J) = C1(1,K,J)-C1(1,K,JC)
            CH(1,K,JC) = C1(1,K,J)+C1(1,K,JC)
  123    CONTINUE
  124 CONTINUE
      IF (IDO .EQ. 1) GO TO 132
      IF (NBD .LT. L1) GO TO 128
      DO 127 J=2,IPPH
         JC = IPP2-J
         DO 126 K=1,L1
            DO 125 I=3,IDO,2
               CH(I-1,K,J) = C1(I-1,K,J)-C1(I,K,JC)
               CH(I-1,K,JC) = C1(I-1,K,J)+C1(I,K,JC)
               CH(I,K,J) = C1(I,K,J)+C1(I-1,K,JC)
               CH(I,K,JC) = C1(I,K,J)-C1(I-1,K,JC)
  125       CONTINUE
  126    CONTINUE
  127 CONTINUE
      GO TO 132
  128 DO 131 J=2,IPPH
         JC = IPP2-J
         DO 130 I=3,IDO,2
            DO 129 K=1,L1
               CH(I-1,K,J) = C1(I-1,K,J)-C1(I,K,JC)
               CH(I-1,K,JC) = C1(I-1,K,J)+C1(I,K,JC)
               CH(I,K,J) = C1(I,K,J)+C1(I-1,K,JC)
               CH(I,K,JC) = C1(I,K,J)-C1(I-1,K,JC)
  129       CONTINUE
  130    CONTINUE
  131 CONTINUE
  132 CONTINUE
      IF (IDO .EQ. 1) RETURN
      DO 133 IK=1,IDL1
         C2(IK,1) = CH2(IK,1)
  133 CONTINUE
      DO 135 J=2,IP
         DO 134 K=1,L1
            C1(1,K,J) = CH(1,K,J)
  134    CONTINUE
  135 CONTINUE
      IF (NBD .GT. L1) GO TO 139
      IS = -IDO
      DO 138 J=2,IP
         IS = IS+IDO
         IDIJ = IS
         DO 137 I=3,IDO,2
            IDIJ = IDIJ+2
            DO 136 K=1,L1
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)-WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)+WA(IDIJ)*CH(I-1,K,J)
  136       CONTINUE
  137    CONTINUE
  138 CONTINUE
      GO TO 143
  139 IS = -IDO
      DO 142 J=2,IP
         IS = IS+IDO
         DO 141 K=1,L1
            IDIJ = IS
            DO 140 I=3,IDO,2
               IDIJ = IDIJ+2
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)-WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)+WA(IDIJ)*CH(I-1,K,J)
  140       CONTINUE
  141    CONTINUE
  142 CONTINUE
  143 RETURN
      END
      SUBROUTINE PDA_RADF2 (IDO,L1,CC,CH,WA1)
      DIMENSION       CH(IDO,2,L1)           ,CC(IDO,L1,2)           ,
     1                WA1(*)
      DO 101 K=1,L1
         CH(1,1,K) = CC(1,K,1)+CC(1,K,2)
         CH(IDO,2,K) = CC(1,K,1)-CC(1,K,2)
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            TR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)
            TI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)
            CH(I,1,K) = CC(I,K,1)+TI2
            CH(IC,2,K) = TI2-CC(I,K,1)
            CH(I-1,1,K) = CC(I-1,K,1)+TR2
            CH(IC-1,2,K) = CC(I-1,K,1)-TR2
  103    CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
  105 DO 106 K=1,L1
         CH(1,2,K) = -CC(IDO,K,2)
         CH(IDO,1,K) = CC(IDO,K,1)
  106 CONTINUE
  107 RETURN
      END
      SUBROUTINE PDA_RADF3 (IDO,L1,CC,CH,WA1,WA2)
      DIMENSION       CH(IDO,3,L1)           ,CC(IDO,L1,3)           ,
     1                WA1(*)     ,WA2(*)
      DATA TAUR,TAUI /-.5,.866025403784439/
      DO 101 K=1,L1
         CR2 = CC(1,K,2)+CC(1,K,3)
         CH(1,1,K) = CC(1,K,1)+CR2
         CH(1,3,K) = TAUI*(CC(1,K,3)-CC(1,K,2))
         CH(IDO,2,K) = CC(1,K,1)+TAUR*CR2
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            DR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)
            DI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)
            DR3 = WA2(I-2)*CC(I-1,K,3)+WA2(I-1)*CC(I,K,3)
            DI3 = WA2(I-2)*CC(I,K,3)-WA2(I-1)*CC(I-1,K,3)
            CR2 = DR2+DR3
            CI2 = DI2+DI3
            CH(I-1,1,K) = CC(I-1,K,1)+CR2
            CH(I,1,K) = CC(I,K,1)+CI2
            TR2 = CC(I-1,K,1)+TAUR*CR2
            TI2 = CC(I,K,1)+TAUR*CI2
            TR3 = TAUI*(DI2-DI3)
            TI3 = TAUI*(DR3-DR2)
            CH(I-1,3,K) = TR2+TR3
            CH(IC-1,2,K) = TR2-TR3
            CH(I,3,K) = TI2+TI3
            CH(IC,2,K) = TI3-TI2
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_RADF4 (IDO,L1,CC,CH,WA1,WA2,WA3)
      DIMENSION       CC(IDO,L1,4)           ,CH(IDO,4,L1)           ,
     1                WA1(*)     ,WA2(*)     ,WA3(*)
      DATA HSQT2 /.7071067811865475/
      DO 101 K=1,L1
         TR1 = CC(1,K,2)+CC(1,K,4)
         TR2 = CC(1,K,1)+CC(1,K,3)
         CH(1,1,K) = TR1+TR2
         CH(IDO,4,K) = TR2-TR1
         CH(IDO,2,K) = CC(1,K,1)-CC(1,K,3)
         CH(1,3,K) = CC(1,K,4)-CC(1,K,2)
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            CR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)
            CI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)
            CR3 = WA2(I-2)*CC(I-1,K,3)+WA2(I-1)*CC(I,K,3)
            CI3 = WA2(I-2)*CC(I,K,3)-WA2(I-1)*CC(I-1,K,3)
            CR4 = WA3(I-2)*CC(I-1,K,4)+WA3(I-1)*CC(I,K,4)
            CI4 = WA3(I-2)*CC(I,K,4)-WA3(I-1)*CC(I-1,K,4)
            TR1 = CR2+CR4
            TR4 = CR4-CR2
            TI1 = CI2+CI4
            TI4 = CI2-CI4
            TI2 = CC(I,K,1)+CI3
            TI3 = CC(I,K,1)-CI3
            TR2 = CC(I-1,K,1)+CR3
            TR3 = CC(I-1,K,1)-CR3
            CH(I-1,1,K) = TR1+TR2
            CH(IC-1,4,K) = TR2-TR1
            CH(I,1,K) = TI1+TI2
            CH(IC,4,K) = TI1-TI2
            CH(I-1,3,K) = TI4+TR3
            CH(IC-1,2,K) = TR3-TI4
            CH(I,3,K) = TR4+TI3
            CH(IC,2,K) = TR4-TI3
  103    CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
  105 CONTINUE
      DO 106 K=1,L1
         TI1 = -HSQT2*(CC(IDO,K,2)+CC(IDO,K,4))
         TR1 = HSQT2*(CC(IDO,K,2)-CC(IDO,K,4))
         CH(IDO,1,K) = TR1+CC(IDO,K,1)
         CH(IDO,3,K) = CC(IDO,K,1)-TR1
         CH(1,2,K) = TI1-CC(IDO,K,3)
         CH(1,4,K) = TI1+CC(IDO,K,3)
  106 CONTINUE
  107 RETURN
      END
      SUBROUTINE PDA_RADF5 (IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
      DIMENSION       CC(IDO,L1,5)           ,CH(IDO,5,L1)           ,
     1                WA1(*)     ,WA2(*)     ,WA3(*)     ,WA4(*)
      DATA TR11,TI11,TR12,TI12 /.309016994374947,.951056516295154,
     1-.809016994374947,.587785252292473/
      DO 101 K=1,L1
         CR2 = CC(1,K,5)+CC(1,K,2)
         CI5 = CC(1,K,5)-CC(1,K,2)
         CR3 = CC(1,K,4)+CC(1,K,3)
         CI4 = CC(1,K,4)-CC(1,K,3)
         CH(1,1,K) = CC(1,K,1)+CR2+CR3
         CH(IDO,2,K) = CC(1,K,1)+TR11*CR2+TR12*CR3
         CH(1,3,K) = TI11*CI5+TI12*CI4
         CH(IDO,4,K) = CC(1,K,1)+TR12*CR2+TR11*CR3
         CH(1,5,K) = TI12*CI5-TI11*CI4
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            DR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)
            DI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)
            DR3 = WA2(I-2)*CC(I-1,K,3)+WA2(I-1)*CC(I,K,3)
            DI3 = WA2(I-2)*CC(I,K,3)-WA2(I-1)*CC(I-1,K,3)
            DR4 = WA3(I-2)*CC(I-1,K,4)+WA3(I-1)*CC(I,K,4)
            DI4 = WA3(I-2)*CC(I,K,4)-WA3(I-1)*CC(I-1,K,4)
            DR5 = WA4(I-2)*CC(I-1,K,5)+WA4(I-1)*CC(I,K,5)
            DI5 = WA4(I-2)*CC(I,K,5)-WA4(I-1)*CC(I-1,K,5)
            CR2 = DR2+DR5
            CI5 = DR5-DR2
            CR5 = DI2-DI5
            CI2 = DI2+DI5
            CR3 = DR3+DR4
            CI4 = DR4-DR3
            CR4 = DI3-DI4
            CI3 = DI3+DI4
            CH(I-1,1,K) = CC(I-1,K,1)+CR2+CR3
            CH(I,1,K) = CC(I,K,1)+CI2+CI3
            TR2 = CC(I-1,K,1)+TR11*CR2+TR12*CR3
            TI2 = CC(I,K,1)+TR11*CI2+TR12*CI3
            TR3 = CC(I-1,K,1)+TR12*CR2+TR11*CR3
            TI3 = CC(I,K,1)+TR12*CI2+TR11*CI3
            TR5 = TI11*CR5+TI12*CR4
            TI5 = TI11*CI5+TI12*CI4
            TR4 = TI12*CR5-TI11*CR4
            TI4 = TI12*CI5-TI11*CI4
            CH(I-1,3,K) = TR2+TR5
            CH(IC-1,2,K) = TR2-TR5
            CH(I,3,K) = TI2+TI5
            CH(IC,2,K) = TI5-TI2
            CH(I-1,5,K) = TR3+TR4
            CH(IC-1,4,K) = TR3-TR4
            CH(I,5,K) = TI3+TI4
            CH(IC,4,K) = TI4-TI3
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_RADFG (IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DIMENSION       CH(IDO,L1,IP)          ,CC(IDO,IP,L1)          ,
     1                C1(IDO,L1,IP)          ,C2(IDL1,IP),
     2                CH2(IDL1,IP)           ,WA(*)
      DATA TPI/6.28318530717959/
      ARG = TPI/FLOAT(IP)
      DCP = COS(ARG)
      DSP = SIN(ARG)
      IPPH = (IP+1)/2
      IPP2 = IP+2
      IDP2 = IDO+2
      NBD = (IDO-1)/2
      IF (IDO .EQ. 1) GO TO 119
      DO 101 IK=1,IDL1
         CH2(IK,1) = C2(IK,1)
  101 CONTINUE
      DO 103 J=2,IP
         DO 102 K=1,L1
            CH(1,K,J) = C1(1,K,J)
  102    CONTINUE
  103 CONTINUE
      IF (NBD .GT. L1) GO TO 107
      IS = -IDO
      DO 106 J=2,IP
         IS = IS+IDO
         IDIJ = IS
         DO 105 I=3,IDO,2
            IDIJ = IDIJ+2
            DO 104 K=1,L1
               CH(I-1,K,J) = WA(IDIJ-1)*C1(I-1,K,J)+WA(IDIJ)*C1(I,K,J)
               CH(I,K,J) = WA(IDIJ-1)*C1(I,K,J)-WA(IDIJ)*C1(I-1,K,J)
  104       CONTINUE
  105    CONTINUE
  106 CONTINUE
      GO TO 111
  107 IS = -IDO
      DO 110 J=2,IP
         IS = IS+IDO
         DO 109 K=1,L1
            IDIJ = IS
            DO 108 I=3,IDO,2
               IDIJ = IDIJ+2
               CH(I-1,K,J) = WA(IDIJ-1)*C1(I-1,K,J)+WA(IDIJ)*C1(I,K,J)
               CH(I,K,J) = WA(IDIJ-1)*C1(I,K,J)-WA(IDIJ)*C1(I-1,K,J)
  108       CONTINUE
  109    CONTINUE
  110 CONTINUE
  111 IF (NBD .LT. L1) GO TO 115
      DO 114 J=2,IPPH
         JC = IPP2-J
         DO 113 K=1,L1
            DO 112 I=3,IDO,2
               C1(I-1,K,J) = CH(I-1,K,J)+CH(I-1,K,JC)
               C1(I-1,K,JC) = CH(I,K,J)-CH(I,K,JC)
               C1(I,K,J) = CH(I,K,J)+CH(I,K,JC)
               C1(I,K,JC) = CH(I-1,K,JC)-CH(I-1,K,J)
  112       CONTINUE
  113    CONTINUE
  114 CONTINUE
      GO TO 121
  115 DO 118 J=2,IPPH
         JC = IPP2-J
         DO 117 I=3,IDO,2
            DO 116 K=1,L1
               C1(I-1,K,J) = CH(I-1,K,J)+CH(I-1,K,JC)
               C1(I-1,K,JC) = CH(I,K,J)-CH(I,K,JC)
               C1(I,K,J) = CH(I,K,J)+CH(I,K,JC)
               C1(I,K,JC) = CH(I-1,K,JC)-CH(I-1,K,J)
  116       CONTINUE
  117    CONTINUE
  118 CONTINUE
      GO TO 121
  119 DO 120 IK=1,IDL1
         C2(IK,1) = CH2(IK,1)
  120 CONTINUE
  121 DO 123 J=2,IPPH
         JC = IPP2-J
         DO 122 K=1,L1
            C1(1,K,J) = CH(1,K,J)+CH(1,K,JC)
            C1(1,K,JC) = CH(1,K,JC)-CH(1,K,J)
  122    CONTINUE
  123 CONTINUE
C
      AR1 = 1.
      AI1 = 0.
      DO 127 L=2,IPPH
         LC = IPP2-L
         AR1H = DCP*AR1-DSP*AI1
         AI1 = DCP*AI1+DSP*AR1
         AR1 = AR1H
         DO 124 IK=1,IDL1
            CH2(IK,L) = C2(IK,1)+AR1*C2(IK,2)
            CH2(IK,LC) = AI1*C2(IK,IP)
  124    CONTINUE
         DC2 = AR1
         DS2 = AI1
         AR2 = AR1
         AI2 = AI1
         DO 126 J=3,IPPH
            JC = IPP2-J
            AR2H = DC2*AR2-DS2*AI2
            AI2 = DC2*AI2+DS2*AR2
            AR2 = AR2H
            DO 125 IK=1,IDL1
               CH2(IK,L) = CH2(IK,L)+AR2*C2(IK,J)
               CH2(IK,LC) = CH2(IK,LC)+AI2*C2(IK,JC)
  125       CONTINUE
  126    CONTINUE
  127 CONTINUE
      DO 129 J=2,IPPH
         DO 128 IK=1,IDL1
            CH2(IK,1) = CH2(IK,1)+C2(IK,J)
  128    CONTINUE
  129 CONTINUE
C
      IF (IDO .LT. L1) GO TO 132
      DO 131 K=1,L1
         DO 130 I=1,IDO
            CC(I,1,K) = CH(I,K,1)
  130    CONTINUE
  131 CONTINUE
      GO TO 135
  132 DO 134 I=1,IDO
         DO 133 K=1,L1
            CC(I,1,K) = CH(I,K,1)
  133    CONTINUE
  134 CONTINUE
  135 DO 137 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 136 K=1,L1
            CC(IDO,J2-2,K) = CH(1,K,J)
            CC(1,J2-1,K) = CH(1,K,JC)
  136    CONTINUE
  137 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IF (NBD .LT. L1) GO TO 141
      DO 140 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 139 K=1,L1
            DO 138 I=3,IDO,2
               IC = IDP2-I
               CC(I-1,J2-1,K) = CH(I-1,K,J)+CH(I-1,K,JC)
               CC(IC-1,J2-2,K) = CH(I-1,K,J)-CH(I-1,K,JC)
               CC(I,J2-1,K) = CH(I,K,J)+CH(I,K,JC)
               CC(IC,J2-2,K) = CH(I,K,JC)-CH(I,K,J)
  138       CONTINUE
  139    CONTINUE
  140 CONTINUE
      RETURN
  141 DO 144 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 143 I=3,IDO,2
            IC = IDP2-I
            DO 142 K=1,L1
               CC(I-1,J2-1,K) = CH(I-1,K,J)+CH(I-1,K,JC)
               CC(IC-1,J2-2,K) = CH(I-1,K,J)-CH(I-1,K,JC)
               CC(I,J2-1,K) = CH(I,K,J)+CH(I,K,JC)
               CC(IC,J2-2,K) = CH(I,K,JC)-CH(I,K,J)
  142       CONTINUE
  143    CONTINUE
  144 CONTINUE
      RETURN
      END
C**************************************************************************
      SUBROUTINE VA04A(X,E,N,F,ESCALE,IPRINT,ICON,MAXIT,AIN,
     +      CS,WL,WGH1,WGH2,THETATR,RMIN2,RMAX2,NXYZ,HW,DAST)
C**************************************************************************
C  STANDARD FORTRAN 66 (A VERIFIED PFORT SUBROUTINE)
      DIMENSION W(40),X(4),E(1),AIN(*),NXYZ(3)
C       W[N*(N+3)]
      DDMAG=0.1*ESCALE
      SCER=0.05/ESCALE
      JJ=N*N+N
      JJJ=JJ+N
      K=N+1
      NFCC=1
      IND=1
      INN=1
      DO 1 I=1,N
      DO 2 J=1,N
      W(K)=0.
      IF(I-J)4,3,4
    3 W(K)=ABS(E(I))
      W(I)=ESCALE
    4 K=K+1
    2 CONTINUE
    1 CONTINUE
      ITERC=1
      ISGRAD=2
      CALL CALCFX(N,X,F,AIN,
     +  CS,WL,WGH1,WGH2,THETATR,RMIN2,RMAX2,NXYZ,HW,DAST)
      FKEEP=ABS(F)+ABS(F)
    5 ITONE=1
      FP=F
      SUM=0.
      IXP=JJ
      DO 6 I=1,N
      IXP=IXP+1
      W(IXP)=X(I)
    6 CONTINUE
      IDIRN=N+1
      ILINE=1
    7 DMAX=W(ILINE)
      DACC=DMAX*SCER
      DMAG=AMIN1(DDMAG,0.1*DMAX)
      DMAG=AMAX1(DMAG,20.*DACC)
      DDMAX=10.*DMAG
      GO TO (70,70,71),ITONE
   70 DL=0.
      D=DMAG
      FPREV=F
      IS=5
      FA=F
      DA=DL
    8 DD=D-DL
      DL=D
   58 K=IDIRN
      DO 9 I=1,N
      X(I)=X(I)+DD*W(K)
      K=K+1
    9 CONTINUE
      CALL CALCFX(N,X,F,AIN,
     +  CS,WL,WGH1,WGH2,THETATR,RMIN2,RMAX2,NXYZ,HW,DAST)
C
      NFCC=NFCC+1
      GO TO (10,11,12,13,14,96),IS
   14 IF(F-FA)15,16,24
   16 IF (ABS(D)-DMAX) 17,17,18
   17 D=D+D
      GO TO 8
   18 WRITE(6,19)
   19 FORMAT(5X,44HVA04A MAXIMUM CHANGE DOES NOT ALTER FUNCTION)
      GO TO 20
   15 FB=F
      DB=D
      GO TO 21
   24 FB=FA
      DB=DA
      FA=F
      DA=D
   21 GO TO (83,23),ISGRAD
   23 D=DB+DB-DA
      IS=1
      GO TO 8
   83 D=0.5*(DA+DB-(FA-FB)/(DA-DB))
      IS=4
      IF((DA-D)*(D-DB))25,8,8
   25 IS=1
      IF(ABS(D-DB)-DDMAX)8,8,26
   26 D=DB+SIGN(DDMAX,DB-DA)
      IS=1
      DDMAX=DDMAX+DDMAX
      DDMAG=DDMAG+DDMAG
      IF(DDMAX-DMAX)8,8,27
   27 DDMAX=DMAX
      GO TO 8
   13 IF(F-FA)28,23,23
   28 FC=FB
      DC=DB
   29 FB=F
      DB=D
      GO TO 30
   12 IF(F-FB)28,28,31
   31 FA=F
      DA=D
      GO TO 30
   11 IF(F-FB)32,10,10
   32 FA=FB
      DA=DB
      GO TO 29
   71 DL=1.
      DDMAX=5.
      FA=FP
      DA=-1.
      FB=FHOLD
      DB=0.
      D=1.
   10 FC=F
      DC=D
   30 A=(DB-DC)*(FA-FC)
      B=(DC-DA)*(FB-FC)
      IF((A+B)*(DA-DC))33,33,34
   33 FA=FB
      DA=DB
      FB=FC
      DB=DC
      GO TO 26
   34 D=0.5*(A*(DB+DC)+B*(DA+DC))/(A+B)
      DI=DB
      FI=FB
      IF(FB-FC)44,44,43
   43 DI=DC
      FI=FC
   44 GO TO (86,86,85),ITONE
   85 ITONE=2
      GO TO 45
   86 IF (ABS(D-DI)-DACC) 41,41,93
   93 IF (ABS(D-DI)-0.03*ABS(D)) 41,41,45
   45 IF ((DA-DC)*(DC-D)) 47,46,46
   46 FA=FB
      DA=DB
      FB=FC
      DB=DC
      GO TO 25
   47 IS=2
      IF ((DB-D)*(D-DC)) 48,8,8
   48 IS=3
      GO TO 8
   41 F=FI
      D=DI-DL
      DD=SQRT((DC-DB)*(DC-DA)*(DA-DB)/(A+B))
      DO 49 I=1,N
      X(I)=X(I)+D*W(IDIRN)
      W(IDIRN)=DD*W(IDIRN)
      IDIRN=IDIRN+1
   49 CONTINUE
      IF (DD.EQ.0.0) DD=1E-10
      W(ILINE)=W(ILINE)/DD
      ILINE=ILINE+1
      IF(IPRINT-1)51,50,51
   50 WRITE(6,52) ITERC,NFCC,F,(X(I),I=1,N)
   52 FORMAT (/1X,9HITERATION,I5,I15,16H FUNCTION VALUES,
     110X,3HF =,E21.14/(5E24.14))
      GO TO(51,53),IPRINT
   51 GO TO (55,38),ITONE
   55 IF (FPREV-F-SUM) 94,95,95
   95 SUM=FPREV-F
      JIL=ILINE
   94 IF (IDIRN-JJ) 7,7,84
   84 GO TO (92,72),IND
   92 FHOLD=F
      IS=6
      IXP=JJ
      DO 59 I=1,N
      IXP=IXP+1
      W(IXP)=X(I)-W(IXP)
   59 CONTINUE
      DD=1.
      GO TO 58
   96 GO TO (112,87),IND
  112 IF (FP-F) 37,37,91
   91 D=2.*(FP+F-2.*FHOLD)/(FP-F)**2
      IF (D*(FP-FHOLD-SUM)**2-SUM) 87,37,37
   87 J=JIL*N+1
      IF (J-JJ) 60,60,61
   60 DO 62 I=J,JJ
      K=I-N
      W(K)=W(I)
   62 CONTINUE
      DO 97 I=JIL,N
      W(I-1)=W(I)
   97 CONTINUE
   61 IDIRN=IDIRN-N
      ITONE=3
      K=IDIRN
      IXP=JJ
      AAA=0.
      DO 65 I=1,N
      IXP=IXP+1
      W(K)=W(IXP)
      IF (AAA-ABS(W(K)/E(I))) 66,67,67
   66 AAA=ABS(W(K)/E(I))
   67 K=K+1
   65 CONTINUE
      DDMAG=1.
      IF (AAA.EQ.0.0) AAA=1E-10
      W(N)=ESCALE/AAA
      ILINE=N
      GO TO 7
   37 IXP=JJ
      AAA=0.
      F=FHOLD
      DO 99 I=1,N
      IXP=IXP+1
      X(I)=X(I)-W(IXP)
      IF (AAA*ABS(E(I))-ABS(W(IXP))) 98,99,99
   98 AAA=ABS(W(IXP)/E(I))
   99 CONTINUE
      GO TO 72
   38 AAA=AAA*(1.+DI)
      GO TO (72,106),IND
   72 IF (IPRINT-2) 53,50,50
   53 GO TO (109,88),IND
  109 IF (AAA-0.1) 89,89,76
   89 GO TO (20,116),ICON
  116 IND=2
      GO TO (100,101),INN
  100 INN=2
      K=JJJ
      DO 102 I=1,N
      K=K+1
      W(K)=X(I)
      X(I)=X(I)+10.*E(I)
  102 CONTINUE
      FKEEP=F
      CALL CALCFX (N,X,F,AIN,
     +  CS,WL,WGH1,WGH2,THETATR,RMIN2,RMAX2,NXYZ,HW,DAST)
      NFCC=NFCC+1
      DDMAG=0.
      GO TO 108
   76 IF (F-FP) 35,78,78
   78 WRITE(6,80)
   80 FORMAT (5X,37HVA04A ACCURACY LIMITED BY ERRORS IN F)
      GO TO 20
   88 IND=1
   35 TMP=FP-F
      IF (TMP.GT.0.0) THEN
      DDMAG=0.4*SQRT(TMP)
      ELSE
      DDMAG=0.0
      ENDIF
      ISGRAD=1
  108 ITERC=ITERC+1
      IF (ITERC-MAXIT) 5,5,81
81    CONTINUE
C   81 WRITE(6,82) MAXIT
   82 FORMAT(I5,30H ITERATIONS COMPLETED BY VA04A)
      IF (F-FKEEP) 20,20,110
  110 F=FKEEP
      DO 111 I=1,N
      JJJ=JJJ+1
      X(I)=W(JJJ)
  111 CONTINUE
      GO TO 20
  101 JIL=1
      FP=FKEEP
      IF (F-FKEEP) 105,78,104
  104 JIL=2
      FP=F
      F=FKEEP
  105 IXP=JJ
      DO 113 I=1,N
      IXP=IXP+1
      K=IXP+N
      GO TO (114,115),JIL
  114 W(IXP)=W(K)
      GO TO 113
  115 W(IXP)=X(I)
      X(I)=W(K)
  113 CONTINUE
      JIL=2
      GO TO 92
  106 IF (AAA-0.1) 20,20,107
   20 RETURN
  107 INN=1
      GO TO 35
      END
