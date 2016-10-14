C  CTFAPPLY : derived from CTFPLOT giving automatic application of C.T.F. to
C             phases input from MMBOX and output ready for ORIGTILT.
C  PROGRAM PLOTS SPOTS FROM MMBOX IN RECIPROCAL SPACE AND C.T.F. PLOT OF IMAGE.
C
C       V1.01   20.4.87         RH      changed to allow IQMAX = 9 to be passed.
C       V1.02   24.1.89         RH      plots box sizes proportional to 8.1-IQ.
C       V1.03   13.6.90         RH      Minor change to FORMAT statement 105.
C       V1.04   6.10.92         RH      extended o/p format h,k,A,P,IQ,BCK,CTF
C       V1.05   21.5.93         RH      compatible with Alliant - CCPDPN.
C       V1.06   14.4.95         RH      minor change to I/P FORMAT statement 99
C       V2.00   13.8.00         TSH     major change from plot82 to plot2k
C        ""     13.6.01         TSH     P2K_FONT needed string terminator
C       V3.00   01.2.97         HS      PHACON,RESMAX, ps-file completed.
C       V3.01   02.2.98         HS      RESHIG,RESLOW
C               remember to change version number and date in title record.
C
C  INPUT PARAMETERS
C      CARD 1    AX AY BX BY ISIZE DSTEP XMAG
CHEN>
C------CARD 2    DFMID1 DFMID2 ANGAST CS KV
C      CARD 2    DFMID1 DFMID2 ANGAST CS KV RESMAX
CHEN<
C      CARD 3    ISER TITLE
CHEN>
C      CARD 4    PHACON
C      CARD 5    RESHIG,RESLOW
C      CARD 6    CTFCOR_IMODE  
CHEN<
C
C         AX,AY   - LATTICE PARAMETERS (FROM NNBOX) OF (1,0) AND (0,1)
C         BX,BY   -                                     IN GRID UNITS.
C         ISIZE   - SIZE OF DENSITOMETERED ARRAY (E.G. 2048)
C         DSTEP   - DENSITOMETER STEPSIZE IN MICRONS
C         XMAG    - PRECISE MAGNIFICATION NORMALLY WORKED OUT FROM LATTICE
C                    PARAMETERS AND KNOWN CELL DIMENSIONS.
C         DFMID1  - DEFOCUS LEVEL (UNDERFOCUS +VE). IF DFMID2=DFMID1, IMAGE
C         DFMID2  -  IS NON-ASTIGMATIC. OTHERWISE AMOUNT OF DEFOCUS IN TWO
C                    ORTHOGONAL DIRECTIONS, DFMID1 BEING DEFOCUS IN DIRECTION
C                    ANGAST (DEGS) RELATIVE TO X AND Y OF THE FOURIER TRANSFORM
C         CS      - SPHERICAL ABERRATION IN MM.
C         KV      - E.M. ACCELERATING VOLTAGE
C         ISER    - SERIAL NUMBER AT HEAD OF OUTPUT FILE.
C         TITLE   - TITLE FOR OUTPUT FILE.
CHEN>
C         RESMAX  - Maximum resolution for output ps-file.
C         PHACON  - PHASECONTRAST, should be approx. 0.93(Cryo),0.65(neg.stain)
C         RESHIG  - HIGHER VALUE FOR RESOLUTION CUTOFF (JUST ON OUTPUT)
C         RESLOW  - LOWER  VALUE FOR RESOLUTION CUTOFF (JUST ON OUTPUT)
C         ICTFCOR - Switch to indicate how CTF correction is to be done:
C                   0 = conventional way: CTF correction done here.
C                   1 = Original image has been Phase flipped. Here only AMP correction.
C                   2 = Original image has been multiplied by CTF. Here only AMP correction (taking care of multiplied CTF).
C                   3 = Original image has been Wiener filtered. Here nothing is to be done.
C                   
CHEN<
C
C   INPUT  DATASTREAM  'IN'
C   OUTPUT DATASTREAM  'OUT'
C
      PARAMETER (ID=150)
CHEN>
C      PARAMETER (RESMAX=0.3)
      PARAMETER (IPICSIZ=1024)
CHEN<

      PARAMETER (PLTSIZ=300.0)
      PARAMETER (CHRSIZ=0.6)
      PARAMETER (IPTMAX=1000)
      LOGICAL BITS
      REAL KV
      DIMENSION ARRAY(-ID:ID,-ID:ID),BITS(8*ID**2+8*ID+2)
      DIMENSION TITLE(15),TITLEIN(15),TEXT(20)
      DIMENSION B(2*ID+1,2*ID+1)
      DIMENSION XV(IPTMAX),YV(IPTMAX),CONT(2)
      EQUIVALENCE(ARRAY,B)
CTSH++
      CHARACTER*80 TMPTEXT
      EQUIVALENCE (TMPTEXT,TEXT)
CTSH--
CHEN>
      INTEGER IPICTU(IPICSIZ,IPICSIZ)
      CHARACTER * 60 CZEILE(60)
      CHARACTER*80 FILENAM
      DIMENSION TZEILE(15)
      INTEGER * 8 ISER,NSER
CHEN<
      TWOPI=6.28318
      FONTSIZE=4.0      ! SELECT 4MM CHAR HEIGHT FOR TEXT
      WRITE(6,1)
1     FORMAT(/'  CTFAPPLY V2.00 : 13.8.00'//)
      READ(5,*) AX,AY,BX,BY,ISIZE,DSTEP,XMAG
CHEN>
C      READ(5,*)DFMID1,DFMID2,ANGAST,CS,KV
      READ(5,*)DFMID1,DFMID2,ANGAST,CS,KV,RESMAX
CHEN<
      READ(5,99)ISER,TITLE
CHEN>
      READ(5,*)PHACON
      AMPCON=SQRT(1.0-(PHACON*PHACON))
      READ(5,*)RESHIG,RESLOW
      READ(5,*)ICTFCOR  
CHEN<
      WRITE(6,98)TITLE, ISER
96    FORMAT(I10,15A4,A40)
99    FORMAT(I10,15A4)
98    FORMAT(' TITLE FOR PLOT AND OUTPUT FILE :',15A4/
     . ' SERIAL NUMBER ON OUTPUT',I10)
      WRITE(6,101)AX,AY,BX,BY,ISIZE,DSTEP,XMAG
CHEN>
C      WRITE(6,102)DFMID1,DFMID2,ANGAST,CS,KV
      WRITE(6,102)DFMID1,DFMID2,ANGAST,CS,KV,RESMAX,PHACON
      WRITE(6,203)RESHIG,RESLOW
CHEN<
101   FORMAT(' LATTICE PARAMETERS AX,AY............',2F10.2/
     . '                    BX,BY............',2F10.2/
     . ' SIZE OF DENSITOMETERED ARRAY........',I7/
     . ' DENSITOMETERED STEPSIZE(MICRONS)....',F10.2/
     . ' MAGNIFICATION OF MICROGRAPH.........',F8.0)
CHEN>
C 102   FORMAT(' UNDERFOCUS 1 .......................',F8.0/
C      .       ' UNDERFOCUS 2 .......................',F8.0/
C      .       ' DIRECTION FOR UNDERFOCUS 1 .........',F9.1/
C      .       ' SPHERICAL ABERRATION (MM) ..........',F10.2/
C      .       ' ACCELERATING VOLTAGE (KV) ..........',F8.0)
102   FORMAT(' UNDERFOCUS 1 .......................',F8.0/
     . ' UNDERFOCUS 2 .......................',F8.0/
     . ' DIRECTION FOR UNDERFOCUS 1 .........',F9.1/
     . ' SPHERICAL ABERRATION (MM) ..........',F10.2/
     . ' ACCELERATING VOLTAGE (KV) ..........',F8.0/
     . ' RESOLUTION MAXIMUM .................',F9.5/
     . ' PHASECONTRAST PORTION ..............',F9.5)
203   FORMAT(' HIGHER RES. CUTOFF .................',F8.0/
     . ' LOWER  RES. CUTOFF .................',F8.0)
C
C-----Prepare output picture, all white
C
      do K=1,IPICSIZ
        do J=1,IPICSIZ
          IPICTU(J,K)=255
        enddo
      enddo
C
C
CHEN<
      CALL CCPDPN(1,'IN','READONLY','F',0,0)
      CALL CCPDPN(2,'OUT','UNKNOWN','F',0,0)
C      CALL DOPEN(1,'IN','RO','F')      ! old vax open
C      CALL DOPEN(2,'OUT','NEW','F')    !  "   "   "
      READ(1,99)  NSER,TITLEIN
      WRITE(6,97) NSER,TITLEIN
97     FORMAT(' Serial no and tilted on input file of uncorrected data'/
     . 40X,I10,15A4)
      WRITE(2,96) ISER,TITLE,
     .'  This is: H,K,A,P,IQ,Back,CTF(phase already applied)'
CHEN>
      ANGAST0=ANGAST
CHEN<
      ANGAST=ANGAST*TWOPI/360.0
      CALL P2K_OUTFILE('CTFPLOT.PS',10)
      CALL P2K_HOME
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
      CALL P2K_GRID(0.5*PLTSIZ,0.5*PLTSIZ,1.0)
      CALL P2K_ORIGIN(-0.5*PLTSIZ,-0.7*PLTSIZ,0.)
      CALL P2K_COLOUR(0)
C
      ICOL=1
      if(ICOL.eq.1)then
        YPOSSTE=12.0
        YPOSN=PLTSIZ+10.0+5.0*YPOSSTE
        call P2K_MOVE(XPOSN,YPOSN,0.)
        XPOSN=PLTSIZ-5.0 
        XP=PLTSIZ-27.0
        do IQIN=1,8
          CALL P2K_FONT('Courier'//CHAR(0),0.6*FONTSIZE)  !REDUCE FONT SIZE
          if(IQIN.eq.1)ICO=2 ! Red
          if(IQIN.eq.2)ICO=3 ! Orange
          if(IQIN.eq.3)ICO=6 ! Blue
          if(IQIN.eq.4)ICO=1 ! Brown
          if(IQIN.eq.5)ICO=7 ! Purple
          if(IQIN.eq.6)ICO=5 ! Green
          if(IQIN.eq.7)ICO=4 ! Yellow
          if(IQIN.eq.8)ICO=8 ! Grey
          if(IQIN.eq.9)ICO=8 ! Grey
          call p2k_colour(ICO)
          RRAD=CHRSIZ*(8.1-IQIN)/1.5
          call P2K_MOVE(XPOSN,YPOSN,0.)
          call p2k_circle(RRAD)
          call p2k_colour(0)
C
          YP=YPOSN-1.0                    ! ADJUST CHARACTER TO BE CENTRAL IN Y.
          WRITE(TMPTEXT(1:1),160)
          IF(IQIN.EQ.1) WRITE(TMPTEXT(1:1),161) ! IQIN=1 include number
          IF(IQIN.EQ.2) WRITE(TMPTEXT(1:1),162) ! IQIN=2 include number
          IF(IQIN.EQ.3) WRITE(TMPTEXT(1:1),163) ! IQIN=3 include number
          IF(IQIN.EQ.4) WRITE(TMPTEXT(1:1),164) ! IQIN=4 include number
          CALL P2K_MOVE(XPOSN,YP,0.)
          CALL P2K_CSTRING(TEXT,1,0.)
C
          if(IQIN.eq.1)then
            WRITE (CZEILE,'(''IQ '',I1)') IQIN
          else
            WRITE (CZEILE,'(''   '',I1)') IQIN
          endif
          READ(CZEILE,'(15A4)') TZEILE
          CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
          YP=YPOSN-1.5
          call P2K_MOVE(XP,YP,0.)
          CALL P2K_STRING(TZEILE,60,0.)
          YPOSN=YPOSN-YPOSSTE/2.0
        enddo
      endif
C
      YPOSSTE=12.0
      YPOSN=PLTSIZ+10.0+6.0*YPOSSTE
      XPOSN=10.0
      CALL P2K_MOVE(XPOSN,YPOSN,0.)
      CALL P2K_STRING(TITLE,60,0.)
C
      YPOSN=YPOSN-YPOSSTE
      CALL P2K_MOVE(XPOSN,YPOSN,0.)
      RRESOL = 1.0/RESMAX
      WRITE (CZEILE,'(''Border = '',F6.1,'' Ang.'')') RRESOL
      READ(CZEILE,'(15A4)') TZEILE
      CALL P2K_STRING(TZEILE,60,0.)
C
      YPOSN=YPOSN-YPOSSTE
      CALL P2K_MOVE(XPOSN,YPOSN,0.)
      WRITE (CZEILE,'(''Res from '',F6.1,'' Ang. to '',F6.1,'' Ang.'')')
     .       RESLOW,RESHIG
      READ(CZEILE,'(15A4)') TZEILE
      CALL P2K_STRING(TZEILE,60,0.)
C
      YPOSN=YPOSN-YPOSSTE
      CALL P2K_MOVE(XPOSN,YPOSN,0.)
      WRITE (CZEILE,'(''Dens.Step = '',F6.1,'' Micrometer'')') DSTEP
      READ(CZEILE,'(15A4)') TZEILE
      CALL P2K_STRING(TZEILE,60,0.)
C
      YPOSN=YPOSN-YPOSSTE
      CALL P2K_MOVE(XPOSN,YPOSN,0.)
      WRITE (CZEILE,'(''Magn. of negative = '',F8.0)') XMAG
      READ(CZEILE,'(15A4)') TZEILE
      CALL P2K_STRING(TZEILE,60,0.)
C
      YPOSN=YPOSN-YPOSSTE
      CALL P2K_MOVE(XPOSN,YPOSN,0.)
      WRITE (CZEILE,1234)DFMID1,DFMID2,ANGAST0
 1234 FORMAT('Underfocus = ',F8.0,' Ang., ',F8.0,' Ang., Angle=',F5.1)
      READ(CZEILE,'(15A4)') TZEILE
      CALL P2K_STRING(TZEILE,60,0.)
C
      YPOSN=YPOSN-YPOSSTE
      CALL P2K_MOVE(XPOSN,YPOSN,0.)
      WRITE (CZEILE,1235)PHACON,AMPCON
 1235 FORMAT('Phase Contrast = ',F6.3,', Amplitude Contrast = ',F6.3)
      READ(CZEILE,'(15A4)') TZEILE
      CALL P2K_STRING(TZEILE,60,0.)
C
CHEN<
C
      SCALE=PLTSIZ/(2.0*RESMAX)         !MAXIMUM RESOLUTION, 0.3=3.33 ANGSTROMS
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(PLTSIZ,0.,0.)
      CALL P2K_DRAW(PLTSIZ,PLTSIZ,0.)
      CALL P2K_DRAW(0.,PLTSIZ,0.)
      CALL P2K_DRAW(0.,0.,0.)
      CENTRE=PLTSIZ/2.0
      CALL P2K_ORIGIN(CENTRE,CENTRE,0.)
      CS=CS*(10.0**7.0)
      KV=KV*1000.0
      WL=12.3/SQRT(KV+KV**2/(10.0**6.0))
      WRITE(6,103)WL
103   FORMAT(' WAVELENGTH (ANGSTROMS)',F10.4)
      STEPR=DSTEP*(10.0**4.0)/XMAG
      THETATR=WL/(STEPR*ISIZE)
      THETAPL=WL*RESMAX/ID
C
C  THETATR IS DIFFRACTION ANGLE OF POINT (0,1) IN TRANSFORM (IN GRID UNITS)
C  THETAPL IS DIFFRACTION ANGLE OF POINT IN PLOT ARRAY IN ARRAY INDEX UNITS.
C
      CALL P2K_MOVE(-CHRSIZ,-CHRSIZ,0.)
      CALL P2K_DRAW(CHRSIZ,CHRSIZ,0.)
      CALL P2K_MOVE(CHRSIZ,-CHRSIZ,0.)
      CALL P2K_DRAW(-CHRSIZ,CHRSIZ,0.)  ! CENTRAL CROSS AT ORIGIN.
      ALENGTH=SQRT(AX**2+AY**2)
      X=(AX/ALENGTH)*(PLTSIZ/2.0)
      Y=(AY/ALENGTH)*(PLTSIZ/2.0)
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(X,Y,0.)           ! PLOT ASTAR VECTOR
      X=X+10.
      CALL P2K_MOVE(X,Y,0.)
      WRITE(TMPTEXT(1:1),151)
151   FORMAT('H')
152   FORMAT('K')
      CALL P2K_CSTRING(TEXT,1,0.)
      BLENGTH=SQRT(BX**2+BY**2)
      X=(BX/BLENGTH)*(PLTSIZ/2.0)
      Y=(BY/BLENGTH)*(PLTSIZ/2.0)
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(X,Y,0.)           ! PLOT BSTAR VECTOR
      X=X+10.
      CALL P2K_MOVE(X,Y,0.)
      WRITE(TMPTEXT(1:1),152)
      CALL P2K_CSTRING(TEXT,1,0.)
      NSPOTS=0
      WRITE(6,1002)
1002  FORMAT(/' LIST OFSPOTS TO WHICH CTF IS APPLIED'/
     . '   IH  IK     A       P  IQIN            PCORR IQOUT CNTRST')
      NTOTSPOTS = 0
      CALL P2K_FONT('Courier'//CHAR(0),0.6*FONTSIZE)  !REDUCE FONT SIZE
C
      if(RESLOW.gt.0.0)then
        SpotLimit2 = (1.0 / RESLOW)**2
      else
        SpotLimit2 = 1000000.0
      endif
109   READ(1,*,END=110) IHIN,IKIN,AIN,PIN,IQIN,BIN
      IF(IQIN.GT.8) GO TO 107         ! PLOTS SPOTS WITH IQIN 8 OR LESS.
      DO 100 J=-1,1,2
        IH=J*IHIN
        IK=J*IKIN
        X=IH*AX+IK*BX
        Y=IH*AY+IK*BY
        X=X/(STEPR*ISIZE)
        Y=Y/(STEPR*ISIZE)
        IF(ABS(X).GE.RESMAX)GO TO 100
        IF(ABS(Y).GE.RESMAX)GO TO 100
        SpotRes2 = X*X+Y*Y
        IF(SpotRes2.GE.SpotLimit2)THEN
C         write(*,'(''Skipping '',2I4,'' Res2='',F12.6,
C     .     '',  Limit2='',F12.6)')
C     1     IH,IK,SpotRes2,SpotLimit2
C         write(*,'(''A,B = '',4F12.3,'', X,Y = '',2F12.3)')
C     .     AX,AY,BX,BY,X,Y
         GOTO 100
        ENDIF
C       SCALE=PLTSIZ/(2.0*RESMAX)         !MAXIMUM RESOLUTION, 0.3=3.33 ANGSTROMS
        X=X*SCALE
        Y=Y*SCALE
        PICX=X*IPICSIZ/PLTSIZ + IPICSIZ/2
        PICY=Y*IPICSIZ/PLTSIZ + IPICSIZ/2
C       WRITE(6,104)X,Y
104     FORMAT(2F10.1)
        XN=X-CHRSIZ*(8.1-IQIN)/2      ! work this one out if you can.
        XP=X+CHRSIZ*(8.1-IQIN)/2
        YN=Y-CHRSIZ*(8.1-IQIN)/2
        YP=Y+CHRSIZ*(8.1-IQIN)/2
        NSPOTS=NSPOTS+1
CHEN>
        if(ICOL.eq.1)then
          if(IQIN.eq.1)ICO=2 ! Red
          if(IQIN.eq.2)ICO=3 ! Orange
          if(IQIN.eq.3)ICO=6 ! Blue
          if(IQIN.eq.4)ICO=1 ! Brown
          if(IQIN.eq.5)ICO=7 ! Purple
          if(IQIN.eq.6)ICO=5 ! Green
          if(IQIN.eq.7)ICO=4 ! Yellow
          if(IQIN.eq.8)ICO=8 ! Grey
          if(IQIN.eq.9)ICO=8 ! Grey
          call p2k_colour(ICO)
          call P2K_MOVE(X,Y,0.)
          RRAD=CHRSIZ*(8.1-IQIN)/1.5
          call p2k_circle(RRAD)
          call p2k_colour(0)
        else
          CALL P2K_MOVE(XN,YN,0.)
          CALL P2K_DRAW(XP,YN,0.)
          CALL P2K_DRAW(XP,YP,0.)
          CALL P2K_DRAW(XN,YP,0.)
          CALL P2K_DRAW(XN,YN,0.)               ! SQUARE ROUND EACH SPOT.
        endif
        if(IQIN.eq.1)IPMAX=4
        if(IQIN.eq.2)IPMAX=3
        if(IQIN.eq.3)IPMAX=2
        if(IQIN.eq.4)IPMAX=1
        if(IQIN.eq.5)IPMAX=0
        IPMIN=-IPMAX
        do IPICK=IPMIN,IPMAX
          do IPICJ=IPMIN,IPMAX
            IPCXPLOT=PICX+IPICK
            IPCYPLOT=PICY+IPICJ
            if(IPCXPLOT.gt.0 .and. IPCXPLOT.lt.IPICSIZ .and. 
     .         IPCYPLOT.gt.0 .and. IPCYPLOT.lt.IPICSIZ       )then
              if (IPICK.eq.IPMIN .or. IPICK.eq.IPMAX .or. 
     .            IPICJ.eq.IPMIN .or. IPICJ.eq.IPMAX         ) then
                IPICTU(IPCXPLOT,IPCYPLOT)=0
              else
                IPICTU(IPCXPLOT,IPCYPLOT)=128
              endif
            endif
          enddo
        enddo
CHEN<
cc      X=X-0.3                               ! ADJUST CHARACTER TO BE CENTRAL IN X.
cc      Y=Y+0.5                               ! ADJUST CHARACTER TO BE CENTRAL IN Y.
        Y=Y-1                         ! ADJUST CHARACTER TO BE CENTRAL IN Y.
        WRITE(TMPTEXT(1:1),160)
        IF(IQIN.EQ.1) WRITE(TMPTEXT(1:1),161) ! IQIN=1 include number
        IF(IQIN.EQ.2) WRITE(TMPTEXT(1:1),162) ! IQIN=2 include number
        IF(IQIN.EQ.3) WRITE(TMPTEXT(1:1),163) ! IQIN=3 include number
        IF(IQIN.EQ.4) WRITE(TMPTEXT(1:1),164) ! IQIN=4 include number
160     FORMAT(' ')
161     FORMAT('1')
162     FORMAT('2')
163     FORMAT('3')
164     FORMAT('4')
        CALL P2K_MOVE(X,Y,0.)
        CALL P2K_CSTRING(TEXT,1,0.)
100   CONTINUE
107   X = IHIN*AX + IKIN*BX
      Y = IHIN*AY + IKIN*BY
      RAD = SQRT(X**2+Y**2)
      ANGLE=RAD*THETATR
      ANGSPT=ATAN2(Y,X)
      C1=TWOPI*ANGLE*ANGLE/(2.0*WL)
      C2=-C1*CS*ANGLE*ANGLE/2.0
      ANGDIF=ANGSPT-ANGAST
      CCOS=COS(2.0*ANGDIF)
CHEN>
C
C################################################################################
C################################################################################
C################################################################################
C-------Phase flipping will be done here.
C-------AVRAMPHS will later devide the AMPLITUDE by CNTRST.
C-------  or, more precisely with CTF(I)=CNTRST:
C
C            In loop over all reflections I for same spot:
C              IF(BACK(I).EQ.0.0) BACK(I)=7.0*AMP(I)/IQ(I)     ! fudge BACK=0
C              SUMAMP =SUMAMP + AMP(I)*ABS(CTF(I))/BACK(I)**2
C              SUMAMPW=SUMAMPW + CTF(I)**2/BACK(I)**2
C            end loop
C            COMBAMP=SUMAMP/SUMAMPW
C
C-------Here, we take this into account by doing the opposite.
C-------That means: CTF (or CNTRST) and IQ have to be defined.
C
C################################################################################
C################################################################################
C################################################################################
C
        if(ICTFCOR.eq.0)then
C
C--------- 0 = conventional way: CTF correction done here.
C
          if(DFMID1.ne.0.0 .or. DFMID2.ne.0.0)then
            DF=0.5*(DFMID1+DFMID2+CCOS*(DFMID1-DFMID2))
            CHI=C1*DF+C2
            CNTRST=-SIN(CHI)*PHACON-COS(CHI)*AMPCON
          else
            CNTRST=-AMPCON
          endif
          IQ=IQIN
C
        else if(ICTFCOR.eq.1)then
C
C--------- 1 = Original image has been Phase flipped, and also already corrected by CTF+Noise**2
C
          IQ=IQIN
          CNTRST = -1.0
C
        else if(ICTFCOR.eq.2)then
C
C--------- 2 = Original image has been multiplied by CTF and also already correctedy by CTF**2+Noise**2. 
C
          IQ=IQIN
          CNTRST = -1.0
C
        else if(ICTFCOR.eq.3 .or. ICTFCOR.eq.9)then
C
C--------- 3 = Original image has been Wiener filtered. Here nothing is to be done.
C
          IQ=IQIN
          CNTRST = -1.0
C
        endif
CHEN<
C
        P=PIN
        IF(CNTRST.LT.0.0) P=PIN+180.0
        IF(P.GE.360.0) P=P-360.0
        WRITE(2,1000) IHIN,IKIN,AIN,P,IQ,BIN,CNTRST ! O/P of SPOTS.
1000    FORMAT(2I5,2G15.5,I4,G15.5,F12.3)
        NTOTSPOTS = NTOTSPOTS +1
        IF(IQ.ne.8) THEN
          WRITE(6,1001)IHIN,IKIN,AIN,PIN,IQIN,P,IQ,CNTRST
1001      FORMAT(1X,2I5,2G15.5,I4,G18.5,I3,F12.3)
1003      FORMAT(1X,2I5,2G15.5,I4,G18.5,I3,F12.3,
     1        ' WRITTEN OUT, BUT IQ=8,9')
        ELSE
          WRITE(6,1003)IHIN,IKIN,AIN,PIN,IQIN,P,IQ,CNTRST
        ENDIF
      GO TO 109
110   CLOSE(1)
      CLOSE(2)
      CALL P2K_ROR
C
      WRITE(6,108)NTOTSPOTS
108   FORMAT(' THERE WERE',I10,'  TOTAL SPOTS PASSED TO OUTPUT FILE')
      NUNIQUE=NSPOTS/2
      WRITE(6,105)NSPOTS,NUNIQUE
105   FORMAT(I10,'  SPOTS WITH IQ 8 OR LESS PLOTTED,',I10,
     $  ' OF THEM UNIQUE')
C
C-----Now for making the plot:
C
      NCALC=0
      CTFAVG=0.0
C
      DO 200 IX=-ID,ID
        DO 200 IY=-ID,ID
          IF(IX.EQ.0.AND.IY.EQ.0) THEN
            CNTRST=0.0
            GO TO 200
          ENDIF
          T1=IX
          T2=IY
          RAD2=T1*T1+T2*T2
          RAD=SQRT(RAD2)
          ANGLE=RAD*THETAPL
          C1=TWOPI*ANGLE*ANGLE/(2.0*WL)
          C2=-C1*CS*ANGLE*ANGLE/2.0
          ANGSPT=ATAN2(T2,T1)
          ANGDIF=ANGSPT-ANGAST
          CCOS=COS(2.0*ANGDIF)
CHEN>
          if(DFMID1.ne.0.0 .or. DFMID2.ne.0.0)then
            DF=0.5*(DFMID1+DFMID2+CCOS*(DFMID1-DFMID2))
            CHI=C1*DF+C2
            CNTRST=-SIN(CHI)*PHACON-COS(CHI)*AMPCON
          else
            CNTRST=1.0
          endif
CHEN<
          CTFAVG=CTFAVG+ABS(CNTRST)
          NCALC=NCALC+1
          ARRAY(IX,IY)=CNTRST
200   continue
C
      IF(NCALC.NE.0) CTFAVG=CTFAVG/NCALC
      WRITE(6,106)NCALC,CTFAVG
106   FORMAT(' C.T.F. NOW GENERATED',I10,' CALCULATED POINTS',
     . ' AVERAGE CTF VALUE',F5.2,' - PROCEED TO CONTOURING')
      CONT(1)=0.0
      CONT(2)=5.0               ! OFF TOP OF CTFPLOT
      NCONT=2
      M=2*ID+1
      SCALE=PLTSIZ/(2.0*ID)
CHEN>
C     CALL HPCNTR(B,M,M,SCALE,BITS,CONT,NCONT,XV,YV,IPTMAX)
      if(DFMID1.ne.0.0 .or. DFMID2.ne.0.0) then
        CALL HPCNTR(B,M,M,SCALE,BITS,CONT,NCONT,XV,YV,IPTMAX)
      endif
CHEN<
C HPCNTR IS SAME AS PLUTO SUBROUTINE BUT WITHH OTHER CALLS REMOVED AND SCALE
C ADDED TO ARGUMENT LIST.
      CALL P2K_PAGE
C
C-----Write out IQ PLot image
C
      write(TMPTEXT,1952)
1952  FORMAT('CTFAPPLY: IQ Plot')
      write(FILENAM(1:80),'(''ctfapply-IQplot.mrc'')')
      CALL PICWRI(IPICTU,FILENAM,TMPTEXT,IPICSIZ,IPICSIZ)
C
      STOP
      END
C
C**APLOT***********************************************************************
      SUBROUTINE APLOT(XV,YV,NPT,SCALE)
      DIMENSION XV(NPT),YV(NPT)
C      WRITE(6,10)NPT,SCALE
10    FORMAT(' ENTERING APLOT, NPT,SCALE',I10,F10.3)
      X=XV(1)*SCALE
      Y=YV(1)*SCALE
      CALL P2K_MOVE(X,Y,0.)
      DO 1 I=2,NPT
      X=XV(I)*SCALE
      Y=YV(I)*SCALE
1     CALL P2K_DRAW(X,Y,0.)
      RETURN
      END
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
C**APLOT***********************************************************************
C
      SUBROUTINE PICWRI(IPICTU,FILENAM,TEXT,IPICDIM,IPICSIZ)
C
      PARAMETER (LMAX=20100)
C
      INTEGER IPICTU(IPICDIM,IPICDIM)
C
      COMMON//NX,NY,NZ,IXMIN,IYMIN,IZMIN,IXMAX,IYMAX,IZMAX
C
      DIMENSION ALINE(LMAX),TITLE(20),NXYZ(3),MXYZ(3)
      DIMENSION NXYZST(3)
      DIMENSION LABELS(20,10)
C
      DIMENSION TEXT(20),PROGTIT(4)
C
      CHARACTER*80 FILENAM
      CHARACTER*200 cline
C
      DMIN =  0.0
      DMAX =  255.0
      DMEAN = 0.5
      MODE = 0
C
      NX=IPICSIZ
      NY=IPICSIZ
      NXYZ(1)=NX
      NXYZ(2)=NY
      NXYZ(3)=1
      NXYZST(1) = NXYZ(1)
      NXYZST(2) = NXYZ(2)
      NXYZST(3) = NXYZ(3)
C
      write(6,'('' Writing picture into '',A40)')FILENAM
      write(6,'('' Size '',2I15)')IPICDIM,IPICSIZ
C
      MODE=0
      call shorten(FILENAM,k)
      write(cline,'(''\rm -f '',A)')FILENAM(1:k)
      call system(cline)
      CALL IMOPEN(2,FILENAM,'NEW')
      CALL ICRHDR(2,NXYZST,NXYZST,MODE,LABELS,0)
      CALL ITRLAB(2,1)
      CALL IWRHDR(2,TEXT,1,DMIN,DMAX,DMEAN)
C
      DMIN= 1.0e10
      DMAX=-1.0e10
      DOUBLMEAN=0.0
      DO K=1,IPICSIZ
        DO J=1,IPICSIZ
          ALINE(J) = IPICTU(J,K)
          if(IPICTU(J,K).lt.DMIN)DMIN=IPICTU(J,K)
          if(IPICTU(J,K).gt.DMAX)DMAX=IPICTU(J,K)
          DOUBLMEAN=DOUBLMEAN+IPICTU(J,K)
        enddo
        CALL IWRLIN(2,ALINE)
      enddo
      DMEAN=DOUBLMEAN/(IPICSIZ*IPICSIZ)
      CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
      CALL IMCLOSE(2)
C
      RETURN
      END
C
C**HPCNTR**********************************************************************
      SUBROUTINE HPCNTR (A,M,N,SCALE,BITS,CONT,NCONT,X,Y,IPTMAX)
C
C***************************************
C
C                  CONTUR ROUTINE
C
C***************************************
C
C  A IS FUNCTION TO BE CONTOURED, DIMENSIONED A(M,N) OR A(M*N)
C  M = NUMBER OF ROWS OF A
C  N = NUMBER OF COLUMN
C  BITS IS LOGICAL*1 ARRAY DIMENSIONED AT LEAST 2*M*N IN CALLING ROUTINE.
C  CONT  ARRAY OF NCONT CONTOUR LEVELS
C  X,Y  ARRAYS OF DIMENSION IPTMAX USED IN SUBROUTINE TO STORE CONTOUR LINES
C
C  IDIV = NUMBER OF LINE SEGMENTS BETWEEN CONTOUR LEVEL CROSSINGS IN UNIT GRID
C     CELL.  IF >= 1, THEN ROUTINE FINDS IDIV-1 COORDINATES BETWEEN CROSSINGS,
C     BY LINEAR INTERPOLATION ON THE UNIT CELL.
C  CONTUR OUTPUT VIA CALLS TO SUBROUTINE APLOT (X,Y,IPT,SCALE)
C     X,Y ARE COORDINATES WITH 0<=X<M AND 0<=Y<N.
C  SUBROUTINE CONSTL(CL) IS CALLED TO SET UP PLOTTING STYLE FOR CONTOUR LEVEL CL
C
      DIMENSION IDIR(4), ISIDE(5), IVCT(4), JVCT(4), KVCT(4), LVCT(4),
     *   SIDE(4), NVCT(4)
      LOGICAL*1 BITS
      DIMENSION A(1),BITS(1),C(4),CONT(NCONT)
      DIMENSION X(1),Y(1)
      DATA  IDIR/3,4,1,2/, ISIDE/1,2,3,4,1/, IVCT/0,-1,0,1/,
     * JVCT/-1,0,1,0/, SIDE/0.,0.,1.0,1.0/, KVCT/4,1,2,3/, LVCT/2,3,4,1/
C
        CLOLD=1.E28
        IDIV=1
      MN=M*N
      MN2=2*MN
      DIV=IDIV
      ICONT=1
C
C LOOP CONTOUR LEVELS CL
1     CL=CONT(ICONT)
      ICONT=ICONT+1
C SET UP PLOTTING STYLE FOR THIS CONTOUR LEVEL
C      CALL CONSTL(CL)  ! USED TO SET CONTOUR LINE TYPE
C
C
C
        DO 10 I=1,MN2
10    BITS(I)=.FALSE.
      NVCT(1)=0
      NVCT(2)=MN
      NVCT(3)=M
      NVCT(4)=MN+1
      IPT=1
      MM=M-1
      NN=N-1
C     SEARCH FOR CONTOUR CROSSING BETWEEN ADJACENT COLUMN OF ARRAY A(I,J)
      I=0
      J=1
      ISUB=0
      JSUB=0
      IRTN=1
  100 IF (J .GT. N) GO TO 140
  110 IF (I .GE. MM) GO TO 130
      I=I+1
      ISUB=ISUB+1
      JSUB=JSUB+1
      IF (A(ISUB)-CL) 115,600,120
115   IF (A(ISUB+1)-CL) 110,110,125
120   IF (A(ISUB+1)-CL) 125,110,110
125   IF (BITS(JSUB+NVCT(1)))  GO TO 110
      XSTART=(CL-A(ISUB))/(A(ISUB+1)-A(ISUB))
      YSTART=0
      GO TO 200
  130 I=0
      ISUB=ISUB+1
      JSUB=JSUB+1
      J=J+1
      GO TO 100
C     SEARCH FOR CONTOUR CROSSING BETWEEN ADJACENT ROWS OF ARRAY A(I,J)
  140 I=0
      J=1
      JSUB=0
      ISUB=0
      IRTN=2
  150 IF (J .GT. NN) GO TO 190
  160 IF (I .GE. M) GO TO 180
      I=I+1
      ISUB=ISUB+1
      JSUB=JSUB+1
      IF (A(ISUB)-CL) 165,160,170
165   IF (A(ISUB+M)-CL) 160,160,175
170   IF (A(ISUB+M)-CL) 175,160,160
175   IF (BITS(JSUB+NVCT(2)))  GO TO 160
      YSTART=(CL-A(ISUB))/(A(ISUB+M)-A(ISUB))
      XSTART=0
      GO TO 200
  180 I=0
      J=J+1
      GO TO 150
190   IF(ICONT.GT.NCONT)RETURN
      GO TO 1
C
C
C     BEGIN FOLLOWING CONTOUR LINE... SAVE INDICIES FOR RETURN TO SEARCH
  200 ISAVE=I
      JSAVE=J
      ISUBSV=ISUB
      JSUBSV=JSUB
      XSAVE=XSTART
      YSAVE=YSTART
      X(1)=XSTART+FLOAT(I-1)
      Y(1)=YSTART+FLOAT(J-1)
      IENT=IRTN
      IRS=0
      GO TO 250
C     DUMP LINE AND FOLLOW CONTOUR LINE ON OPPOSITE SIDE OF STARTING PIONT
C     WHEN USED A SECOND TIME THIS ENTRY RETURNS TO SEARCH
  205 IRS=1
  210 IF (IPT .GT. 1) CALL  APLOT(X,Y,IPT,SCALE)
      IPT=1
      I=ISAVE
      J=JSAVE
      ISUB=ISUBSV
      JSUB=JSUBSV
      XSTART=XSAVE
      YSTART=YSAVE
      X(1)=XSTART+FLOAT(I-1)
      Y(1)=YSTART+FLOAT(J-1)
      IF (IRS.NE.0) GO TO (110,160), IRTN
      IEXIT=IRTN
      IRS=1
      GO TO 240
C     RETURN FROM FOLLOWING CONTOUR LINE THROUGH A CELL
230   IF (BITS(JSUB+NVCT(IEXIT)))  GO TO 205
  240 I=I+IVCT(IEXIT)
      J=J+JVCT(IEXIT)
      JSUB=I+(J-1)*M
      ISUB=JSUB
      IENT=IDIR(IEXIT)
250   BITS(JSUB+NVCT(IENT))=.TRUE.
      IF (I.LT.1 .OR. I.GT.MM .OR. J.LT.1 .OR. J.GT.NN)  GO TO 210
C     FIND CONTOUR CROSSING IN NEW CELL
260   IF (ISUB+1.GT.MN .OR. ISUB+M.GT.MN
     1     .OR. ISUB+1+M.GT.MN)  GO TO 210
      C(1)=A(ISUB+1)
      C(2)=A(ISUB)
      C(3)=A(ISUB+M)
      C(4)=A(ISUB+1+M)
      JRTN=1
      ICNT=1
      JCNT=1
      DO 290 IROUND=1,4
      IF (IROUND .EQ. IENT) GO TO 290
      I1=ISIDE(IROUND)
      I2=ISIDE(IROUND+1)
      IF (C(I1)-CL) 270,285,275
  270 IF (C(I2)-CL) 290,290,280
  275 IF (C(I2)-CL) 280,290,290
  280 IEXIT=IROUND
      ICNT=ICNT+1
      GO TO 290
  285 JEXIT=IROUND
      JCNT=JCNT+1
  290 CONTINUE
      GO TO (300,310,700,210), JCNT
  300 GO TO (210,320,210,800), ICNT
  310 GO TO (710,320,210,210), ICNT
  320 GO TO (330,340,350,360), IENT
  330 GO TO (210,410,500,410), IEXIT
  340 GO TO (510,210,510,400), IEXIT
  350 GO TO (500,410,210,410), IEXIT
  360 GO TO (510,400,510,210), IEXIT
C     FOLLOW CONTOUR LINE ACROSS A CELL TO A SIDE
  400 XSTART=SIDE(IENT)
  410 XFIN=SIDE(IEXIT)
      XINC=(XFIN-XSTART)/DIV
      XBASE=FLOAT(I-1)
      YBASE=FLOAT(J-1)
      A1=CL-C(2)
      A2=C(1)-C(2)
      A3=C(3)-C(2)
      A4=C(2)-C(1)+C(4)-C(3)
      DO 440 INTERP=1,IDIV
      XSTART=XSTART+XINC
      YSTART=(A1-A2*XSTART)/(A3+A4*XSTART)
      IF (IPT.LT.IPTMAX)  GO TO 430
      CALL APLOT(X, Y, IPT,SCALE)
      X(1)=X(IPT)
      Y(1)=Y(IPT)
      IPT=1
  430 IPT=IPT+1
      X(IPT)=XBASE+XSTART
      Y(IPT)=YBASE+YSTART
  440 CONTINUE
      GO TO (230,210,615,635), JRTN
  500 YSTART=SIDE(IENT)
C     FOLLOW CONTOUR LINE ACROSS A CELL TO A TOP OR BOTTOM
  510 YFIN=SIDE(IEXIT)
      XBASE=FLOAT(I-1)
      YINC=(YFIN-YSTART)/DIV
      YBASE=FLOAT(J-1)
      A1=CL-C(2)
      A2=C(3)-C(2)
      A3=C(1)-C(2)
      A4=C(2)-C(1)+C(4)-C(3)
      DO 540 INTERP=1,IDIV
      YSTART=YSTART+YINC
      XSTART=(A1-A2*YSTART)/(A3+A4*YSTART)
      IF (IPT.LT.IPTMAX) GO TO 530
      CALL APLOT(X, Y, IPT,SCALE)
      X(1)=X(IPT)
      Y(1)=Y(IPT)
      IPT=1
  530 IPT=IPT+1
      X(IPT)=XBASE+XSTART
      Y(IPT)=YBASE+YSTART
  540 CONTINUE
      GO TO (230,210,615,635), JRTN
C     FOLLOW CONTOUR LINE FROM CORNER TO CORNER
600   K1=ISUB-M
      K2=ISUB+1-M
      K3=ISUB+1
      K4=ISUB+1+M
      K5=ISUB+M
      K6=ISUB-1+M
      K7=ISUB-1
      C1=A(K1)
      C2=A(K2)
      C3=A(K3)
      C4=A(K4)
      C5=A(K5)
      C6=A(K6)
      C7=A(K7)
      C8=A(ISUB)
      IF (ISUB.LT.1 .OR. ISUB.GT.MN) GO TO 640
      X(1)=FLOAT(I-1)
      Y(1)=FLOAT(J-1)
      IF (J .EQ. 1 .OR. J .EQ. M)  GO TO 610
      IF (K1.LT.1 .OR. K1.GT.MN)  GO TO 610
      IF (K2.LT.1 .OR. K2.GT.MN)  GO TO 610
      IF (K3.LT.1 .OR. K3.GT.MN)  GO TO 610
      IF (K4.LT.1 .OR. K4.GT.MN)  GO TO 610
      IF (K5.LT.1 .OR. K5.GT.MN)  GO TO 610
      IF (C3.NE.CL)  GO TO 610
      IF (C1 .EQ. CL .AND. C2 .EQ. CL .AND.
     *    C4 .EQ. CL .AND. C5 .EQ. CL)  GO TO 610
      X(2)=X(1)+1.
      Y(2)=Y(1)
      CALL APLOT (X, Y, 2,SCALE)
      GO TO 620
  610 IF (J .EQ. 1)  GO TO 620
      IF (K1.LT.1 .OR. K1.GT.MN)  GO TO 620
      IF (K2.LT.1 .OR. K2.GT.MN)  GO TO 620
      IF (K3.LT.1 .OR. K3.GT.MN)  GO TO 620
      IF (C2 .NE. CL)  GO TO 620
      IF (C1 .EQ. CL .OR. C3 .EQ. CL)  GO TO 620
      IF (C1 .GT. CL .AND. C3 .GT. CL .OR.
     *    C1 .LT. CL .AND. C3 .LT. CL)  GO TO 620
      C(1)=C2
      C(2)=C1
      C(3)=C8
      C(4)=C3
      J=J-1
      JRTN=3
      IENT=3
      IEXIT=1
      GO TO 500
  615 IF (IPT .GT. 1)  CALL APLOT (X, Y, IPT,SCALE)
      IPT=1
      J=J+1
      X(1)=FLOAT(I-1)
      Y(1)=FLOAT(J-1)
  620  IF (J .EQ. M .OR. I .EQ. 1)  GO TO 630
      IF (K3.LT.1 .OR. K3.GT.MN)  GO TO 630
      IF (K4.LT.1 .OR. K4.GT.MN)  GO TO 630
      IF (K5.LT.1 .OR. K5.GT.MN)  GO TO 630
      IF (K6.LT.1 .OR. K6.GT.MN)  GO TO 630
      IF (K7.LT.1 .OR. K7.GT.MN)  GO TO 630
      IF (C5 .NE. CL)  GO TO 630
      IF (C3 .EQ. CL .AND. C4 .EQ. CL .AND.
     *    C6 .EQ. CL .AND. C7 .EQ. CL)   GO TO 630
      X(2)=X(1)
      Y(2)=Y(1)+1.
      CALL APLOT (X, Y, 2,SCALE)
      GO TO 640
  630 IF (J .EQ. M)  GO TO 640
      IF (K3.LT.1 .OR. K3.GT.MN)  GO TO 640
      IF (K4.LT.1 .OR. K4.GT.MN)  GO TO 640
      IF (K5.LT.1 .OR. K5.GT.MN)  GO TO 640
      IF (C4 .NE. CL)  GO TO 640
      IF (C3 .EQ. CL .OR. C5 .EQ. CL)  GO TO 640
      IF (C3 .GT. CL .AND. C5 .GT. CL .OR.
     *    C3 .LT. CL .AND. C5 .LT. CL)  GO TO 640
      C(1)=C3
      C(2)=C8
      C(3)=C5
      C(4)=C4
      JRTN=4
      IENT=1
      IEXIT=3
      GO TO 500
  635 IF (IPT .GT. 1) CALL APLOT (X, Y, IPT,SCALE)
      IPT=1
      X(1)=FLOAT(I-1)
      Y(1)=FLOAT(J-1)
  640 GO TO (110,160), IRTN
C    FOLLOW CONTOUR LINE FROM SIDE TO CORNER OR CORNERS
  700 JRTN=2
      IOPP=IDIR(IENT)
      I1=ISIDE(IOPP)
      I2=ISIDE(IOPP+1)
      IEXIT=IOPP
      C(I1)=C(KVCT(I1))
      C(I2)=C(LVCT(I2))
      GO TO 320
  710 JRTN=2
      IEXIT=JEXIT
      GO TO 320
C     FOLLOW CONTOUR LINE THROUGH SADDLE POINT
  800 IOPP=IDIR(IENT)
      I1=ISIDE(IENT)
      C1=C(I1)
      I2=ISIDE(IENT+1)
      C2=C(I2)
      I3=ISIDE(IOPP)
      C3=C(I3)
      I4=ISIDE(IOPP+1)
      C4=C(I4)
      IF ((C1-CL)/(C1-C2) .EQ. (C4-CL)/(C4-C3))  GO TO 820
      IF ((C1-CL)/(C1-C4) .GT. (C2-CL)/(C2-C3))  GO TO 810
      IEXIT=I4
      GO TO 320
  810 IEXIT=I2
      GO TO 320
  820 C(I3)=C(I2)
      C(I4)=C(I1)
      IEXIT=I3
      GO TO 320
      END
