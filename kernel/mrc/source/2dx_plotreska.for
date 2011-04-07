C**PLOTRES********************************************************************
C
C PLOTRES : is like PLOTALL, but uses IQ values from input of an expanded
C           single image and pltos spots in resolution bands
C
C           I dont know who wrote it originally, someone at MRC probably.   AC           
C
C           5/15/96 add options of A,B,GAMMA or AX,AY,BX,BY etc.  AC
C           9/7/96 to move H label for astar to X,Y-10  AC
C  PLOTRESA 9/29/98 to remove IQ labels as an option  AC
C  PLOTRESB 1/23/01 to allow 3D resolution ring w/ tilt  AC
C           3/19/02 fix the resolution limit problem with non-orthogonal
C                   A,B vectors and include 3D resolution limit  AC
C  PLOTRESC 4/28/03 remove 3D resolution ring feature because it has being
C                   accounted for in drawing the lattice.  AC
C  PLOTRESK v1.0 11/10/03 image2000 style postscript plot and clean up.  AC
C  PLOTRESKA v1.0 12/17/03 Add option for ploting with untilted cell  AC
C  PLOTRESKA v1.1 4/05/05  Make data input to REAL*8 because an error on
C                          alpha machines AC
C  PLOTRESKA v1.2 20/12/10 Adapted to 2dx, added IOPTION=3, HSt
C  
C  INPUT  IOPTION  1=plot the data with A,B, GAMMA (use with caution
C                    not properly debugged)
C                    plot resolution rings as 3D resolution projected to
C                    untilted crystals.  Note that cell parameters should be
C                    that of the untilted crystal
C                  2=plot the data with AX,AY,BX,BY (better tested)
C                    plot 3D resolution as is
C                  3=plot the merged data with A,B,GAMMA
C                    otherwise as for option=1 above.
C         ACELL    direct space lattice parameter A in angstrom
C         BCELL    direct space lattice parameter B in angstrom
C         ABANG1   direct space lattice parameter REALANG in degree
C         A2X1     reciprocal A* axis to x-axis of the plot in degree
C         AX,AY    reciprocal lattice vector for A* in 1/pixel for image
C         BX,BY    reciprocal lattice vector for B* in 1/pixel for image
C         MDIM     image size in pixel
C         STEP     image scanning step in micron
C         AMAG     magnification
C         IQLABEL  1=include IQ label for spots withe IQ 1-4
C                  0=no label
C         iellipse 0=plot resolution rings for tilted samples as rings
C                  1=plot resolution rings for tilted samples as ellipses
C         RESMAX   maximal resolution of the plot which goes to the edge of the plot
C         TAXA1    angle from the tilt axis to the A* axis in degree
C         TANGL1   tilt angle in degree (tilt axis not shown if TANGL=0)
C         NRES     number of resolution rings to display (up to 5 rings
C         RESTORE(5)  resolution ring value in angstrum
C          
C

C Dimension Statements for the plot
      PARAMETER (PLTSIZ = 300.0)
      PARAMETER (CHRSIZ = 0.6)
      DIMENSION TITLE(20)
      DIMENSION RTITLE(80)
      DIMENSION TEXT(20)
      DIMENSION RESTORE(100) !UP TO 100 RESOLUTION CYRCLE
      CHARACTER*20 TMPTEXT
      CHARACTER*200 CLINE
      CHARACTER*200 CLIN2
      CHARACTER*80 CTITLE
      CHARACTER*80 CNAME
      CHARACTER*200 CRINGS
      EQUIVALENCE (CTITLE,RTITLE)
      EQUIVALENCE (TMPTEXT,TEXT)
      REAL*4 SCALEP
C
      REAL*4 DRAD,FACTOR,pi
      REAL*4 AX0,AY0,BX0,BY0,AX,AY,BX,BY,AX9,AY9,BX9,BY9
      REAL*4 ACELL,BCELL,ABANG1,A2X1,A2X,AMAG,STEP
      REAL*8 AMP,PHASE,BCK,CTF
      INTEGER IOPTION,MDIM
      INTEGER IH,IK,IQ,IQLABEL
C
      COMMON //RESMAX,SCALEP,NRES,NSPOTS,
     .  FONTSIZE,IQLABEL,CNAME
C
C==============================================================================
C=====Input of parameters======================================================
C==============================================================================
C
      FONTSIZE=2.0      ! SELECT 4MM CHAR HEIGHT FOR TEXT
      pi=3.1415926537
      DRAD=pi/180.0

      WRITE(6,102)
102   FORMAT(/,/,': PLOTRESKA - v2.0 Oct. 17, 2008',/)

      write(*,'('' INPUT TAXA, TANGL in degrees '',
     .   ''(tilt axis not ploted if TANGL=0)'')')
      READ (5,*)TAXA1,TANGL1
      WRITE(*,'('' Read:   TAXA = '',F12.3,''  TANGL = '',F12.3)')TAXA1,TANGL1
      TAXA=TAXA1*DRAD
      if(TAXA.lt.0.0)TAXA=TAXA+pi
      TANGL=TANGL1*DRAD
C
      write(*,'(''OPTIONS: (1) (tilted) section, '',
     .    ''input A,B,GAMMA,AX,AY,BX,BY'',/,
     .    ''         (2) untilted projection, '', 
     .    ''input AX,AY,BX,BY,IMAGE SIZE,STEP,MAG.'',/,
     .    ''         (3) non-tilted merge section, '',
     .    ''input A,B,GAMMA,AX,AY,BX,BY'')')
      READ(5,*)IOPTION
      write(6,'('' Read:'',I3)')IOPTION
C
      IF (IOPTION.EQ.1 .or. IOPTION.eq.3) THEN
        PRINT *,'INPUT A,B,GAMMA,AX,AY,BX,BY'
        READ(5,*)ACELL,BCELL,ABANG1,AX,AY,BX,BY
        WRITE(6,12)ACELL,BCELL,ABANG1,AX,AY,BX,BY
12      FORMAT(' Read: A,B,ABANG,Lattice:',3F8.2,4F8.3)
      else
        PRINT *,'INPUT AX,AY,BX,BY,IMAGE SIZE,STEP,MAG.'
        READ(5,*)AX,AY,BX,BY,MDIM,STEP,AMAG
        write(6,'('' Read: '',4F10.3,I8,F8.2,F12.1)')AX,AY,BX,BY,MDIM,STEP,AMAG
      endif
C
      write(*,'('' Input name of input APH file'')')
      read(5,'(A80)')CNAME
      call shorten(CNAME,k)
      write(*,'('' Read: '',A)')CNAME(1:k)
C
      PRINT *,'INCLUDE IQ value label? YES-1 NO-0'
      READ (5,*)IQLABEL
      write(*,'('' Read: '',I2)')IQLABEL
C
      PRINT *,'Plot resolution rings as ellipses (0=n;1=y)'
      READ (5,*)iellipse
      write(*,'('' Read: '',I2)')iellipse
C
      PRINT *,'INPUT RESOLUTION MAXIMUM IN ANGSTROMS'
      READ (5,*)RESMAX
      if(RESMAX.GT.0.0)THEN
        RESMAX = 1.0/RESMAX
      else
        write(*,'(''::ERROR in resolution specification'')')
        RESMAX=0.1
      endif
      write(*,'('' Rreciprocal RESOLUTION '',F12.3)'),RESMAX
      RORIRESMAX=1.0/RESMAX
      SCALEP=PLTSIZ/(2.0*RESMAX)
C
      write(*,'(/,'' Input comma-separated list of resolution values'')')
      read(5,'(A)')CRINGS
      write(*,'('' Read: '',A40)')CRINGS
C
      call inkomma(CRINGS,k)
      call getnumbers(CRINGS,NRES,RESTORE)
C
      write(*,'('' Got '',I6,'' numbers:'')')NRES
      do i=1,NRES
        if(RORIRESMAX.le.RESTORE(i))then
          write(*,'(''   Ring '',I3,'' = '',F10.3)')i,RESTORE(i)
        else
          write(*,'(''::WARNING: Ring'',I3,
     .        ''('',F8.1,''A) is beyond limit of ''
     .        ,F10.3,''A. Not plotted.'')')i,RESTORE(i),RORIRESMAX
        endif
      enddo
      write(6,'('' '')')
C
C==============================================================================
C=====Calculation of vectors===================================================
C==============================================================================
C
C     Calculate tilt axis relative to tilted a* axis
C
      if(IOPTION.eq.1.or.IOPTION.eq.3)then
C-------Take untilted TAXA if ploting plane in 3D space
        TLTAXA = TAXA
      else
C-------Take untilted TLTAXA if ploting projection onto non-tilted 2D plane
        IF (abs(TANGL).GT.0.001) THEN
          write(6,'(''TAXA='',F12.6)')TAXA/DRAD
          COSPHI = COS(TAXA)/(SQRT(1.0 + SIN(TAXA)*SIN(TAXA)*TAN(TANGL)*TAN(TANGL)))
          TLTAXA = ACOS(COSPHI)
        else
          TLTAXA = TAXA
        endif
      endif
      write(6,'(''TLTAXA='',F12.6)')TLTAXA/DRAD
C
      call hand(AX,AY,BX,BY,IHAND)
      RTLTAXA=REAL(IHAND)*TLTAXA
      RTAXA =REAL(IHAND)*TAXA
      write(6,'(''RTLTAXA='',F12.6)')RTLTAXA/DRAD
C
      IF (IOPTION.EQ.1.or.IOPTION.eq.3) THEN
        GAMMA1=180.0-ABANG1
        AX=1.0/(ACELL*SIN(DRAD*GAMMA1))
        AY=0
        BX=COS(DRAD*GAMMA1)/(BCELL*SIN(DRAD*GAMMA1))
        BY=1.0/BCELL
      ELSE
        AX9=AX
        AY9=AY
        BX9=BX
        BY9=BY
        FACTOR = AMAG/(MDIM*STEP*10000)
        AX = AX*FACTOR
        AY = AY*FACTOR
        BX = BX*FACTOR
        BY = BY*FACTOR
      ENDIF
      A2X=-atan2(AY,AX)
      A2X1=A2X/DRAD
C
C     write(6,'(''Lattice is now: '',4F10.3)')AX,AY,BX,BY
C
      CALL CCPDPN(1,CNAME,'READONLY','F', 0, 0)
C
      if(IOPTION.ne.3)then
        READ(1,10)TITLE
        WRITE(6,10)TITLE
10      FORMAT(20A4)
      endif
C
C==============================================================================
C=====Plot initialization======================================================
C==============================================================================
C
      WRITE(6,420)
420   FORMAT(' ENTERING TTPLOT INITIALISATION')
      NSPOTS = 0
      CALL P2K_OUTFILE('PLOTRES.PS',10)
      CALL P2K_HOME
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
      CALL P2K_GRID(0.5*PLTSIZ,0.5*PLTSIZ,1.0)
      CALL P2K_ORIGIN(-0.5*PLTSIZ,-0.7*PLTSIZ,0.)
      CALL P2K_COLOUR(0)
C
      YPOSN=PLTSIZ+100.
C
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_ORIGIN(0.,0.,0.)
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_MOVE(10.,YPOSN,0.)
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
C
      if(IOPTION.eq.3)then
        write(CTITLE,'(''PLOTRES resolution circle plot of'',
     .      '' non-tilted merged data.'')')
      else
        write(CTITLE,'(''PLOTRES resolution circle plot of'',
     .      '' APH file.'')')
      endif
      call shorten(CTITLE,k)
      CALL P2K_STRING(RTITLE,k,0.)
      YPOSN=YPOSN-10.0
C
      call shorten(CNAME,k)
      write(CTITLE,'(''Input file: '',A)')CNAME(1:k)
      call shorten(CTITLE,k)
      CALL P2K_MOVE(10.,YPOSN,0.)
      CALL P2K_STRING(RTITLE,k,0.)
      YPOSN=YPOSN-15.0
C
      if(IOPTION.eq.3)then
        write(CTITLE,'(''Symbols based on FOM: '')')
        call shorten(CTITLE,k)
        CALL P2K_MOVE(10.,YPOSN,0.)
        CALL P2K_STRING(RTITLE,k,0.)
        YPOSN=YPOSN+3.0
C
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*1.3)
        write(CTITLE,'(''Symbol:   1    2    3    4    5'',
     .                 ''    6    7    8    9'')')
        CALL P2K_MOVE(110.,YPOSN,0.)
        call shorten(CTITLE,k)
        CALL P2K_STRING(RTITLE,k,0.)
        YPOSN=YPOSN-6.0
        write(CTITLE,'(''FOM:     >95  >90  >85  >80  >75'',
     .                 ''  >70  >65  >60  <60'')')
        CALL P2K_MOVE(110.,YPOSN,0.)
        call shorten(CTITLE,k)
        CALL P2K_STRING(RTITLE,k,0.)
        YPOSN=YPOSN-12.0
      else
        CALL P2K_MOVE(10.,YPOSN,0.)
        CALL P2K_STRING(TITLE,60,0.)
        YPOSN=YPOSN-15.0
      endif
C
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
      write(CTITLE,'(''Resolution Max (Nyquist of plot) '',
     .       ''at '',F10.2,'' A'')')RORIRESMAX
      CALL P2K_MOVE(10.,YPOSN,0.)
      CALL P2K_STRING(RTITLE,60,0.)
      YPOSN=YPOSN-3.0
C
      YHEAD=YPOSN
C
      DO N=1,NRES
        if(RESTORE(N).GE.RORIRESMAX)then
          write(CTITLE,'(''Resolution Ring at '',F10.2,'' A'')')RESTORE(N)
          YPOSN=YPOSN-8.0
          if(YPOSN.gt.PLTSIZ+10.0) then
            CALL P2K_MOVE(10.,YPOSN,0.)
            CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
            CALL P2K_STRING(RTITLE,60,0.)
          else
            YPOSN=PLTSIZ+5.0
            CALL P2K_MOVE(10.,YPOSN,0.)
            CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
            write(CTITLE,'(''and more rings.'')')
            CALL P2K_STRING(RTITLE,60,0.)
          endif
        endif
      enddo
C
      YPOSN=YHEAD-10.
      XPOSN=170.
C
      IF (abs(TANGL).GT.0.001) THEN
C
        write(CTITLE,'(''TAXA  = '',F8.3)')TAXA1
        CALL P2K_MOVE(XPOSN,YPOSN,0.)
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
        call shorten(CTITLE,k)
        CALL P2K_STRING(RTITLE,k,0.)
        YPOSN=YPOSN-10.0
C
        IF(IOPTION.EQ.2)then
          IF (abs(TANGL).GT.0.001) THEN
            TLTAXA1=TLTAXA/DRAD
            if(TLTAXA1.gt.90.0)TLTAXA1=TLTAXA1-180.0
            write(CTITLE,'(''TLTAXA= '',F8.3)')TLTAXA1
            CALL P2K_MOVE(XPOSN,YPOSN,0.)
            CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
            call shorten(CTITLE,k)
            CALL P2K_STRING(RTITLE,k,0.)
            YPOSN=YPOSN-10.0
          endif
        endif
C
        write(CTITLE,'(''TANGL = '',F8.3)')TANGL/DRAD
        CALL P2K_MOVE(XPOSN,YPOSN,0.)
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
        call shorten(CTITLE,k)
        CALL P2K_STRING(RTITLE,k,0.)
        YPOSN=YPOSN-10.0
      endif
C
      if(IOPTION.ne.3)then
        if(IHAND.GT.0)then
          write(CTITLE,'(''Right-handed lattice'')')
        else
          write(CTITLE,'(''Left-handed lattice'')')
        endif
        CALL P2K_MOVE(XPOSN,YPOSN,0.)
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
        call shorten(CTITLE,k)
        write(*,'(A)')CTITLE(1:k)
        CALL P2K_STRING(RTITLE,k,0.)
        YPOSN=YPOSN-10.0
      endif
C
      YPOSN=-5.0
      CALL P2K_MOVE(10.,YPOSN,0.)
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*1.3)
      IF (IOPTION.EQ.1) THEN
        write(CTITLE,'(''Plot is based on real-space lattice'',
     .      '' (A,B,Realang): '')')
        call shorten(CTITLE,k)
        CALL P2K_STRING(RTITLE,k,0.)
        CALL P2K_MOVE(130.,YPOSN,0.)
        write(CTITLE,'(F10.2,'' A,  '',F10.2,'' A,  '',F10.3,
     .      '' deg'')')ACELL,BCELL,ABANG1
        call shorten(CTITLE,k)
        CALL P2K_STRING(RTITLE,k,0.)
      elseif (IOPTION.EQ.3) THEN
        write(CTITLE,'(''Plot of merged data for real-space lattice'',
     .      '' (A,B,Realang): '')')
        call shorten(CTITLE,k)
        CALL P2K_STRING(RTITLE,k,0.)
        CALL P2K_MOVE(160.,YPOSN,0.)
        write(CTITLE,'(F10.2,'' A,  '',F10.2,'' A,  '',F10.3,
     .      '' deg'')')ACELL,BCELL,ABANG1
        call shorten(CTITLE,k)
        CALL P2K_STRING(RTITLE,k,0.)
      else
        write(CTITLE,'(''Plot is based on measured reciprocal'',
     .    '' lattice: '')')
        call shorten(CTITLE,k)
        CALL P2K_STRING(RTITLE,k,0.)
        CALL P2K_MOVE(130.,YPOSN,0.)
        write(CLINE,'(2F16.3)')AX9,AY9
        call inkomma(CLINE,k1)
        write(CLIN2,'(2F16.3)')BX9,BY9
        call inkomma(CLIN2,k2)
        write(CTITLE,'(''U = '',A,''    V = '',A)')CLINE(1:k1),CLIN2(1:k2)
        call shorten(CTITLE,k)
        CALL P2K_STRING(RTITLE,k,0.)
      endif
C
      YPOSN=YPOSN-5.0
      CALL P2K_MOVE(10.,YPOSN,0.)
      IF (IOPTION.EQ.1) THEN
        if(abs(TANGL).gt.0.001)then
          write(CTITLE,'(''Plot shows the tilted plane in 3D Fourier '',
     .      ''space. The tilt axis is TAXA.'')')
        else
          write(CTITLE,'(''Plot shows the plane in canonical '',
     .      ''Fourier space.'')')
        endif
        call shorten(CTITLE,k)
        CALL P2K_STRING(RTITLE,k,0.)
      elseif (IOPTION.EQ.3) THEN
        write(CTITLE,'(''Plot shows the plane in canonical '',
     .      ''Fourier space.'')')
        call shorten(CTITLE,k)
        CALL P2K_STRING(RTITLE,k,0.)
      else
        if(abs(TANGL).gt.0.001)then
          write(CTITLE,'(''Plot shows the projection of the (tilted) '',
     .       ''Fourier plane onto the untilted plane.'')')
        else
          write(CTITLE,'(''Plot shows the measured data.'')')
        endif
        call shorten(CTITLE,k)
        CALL P2K_STRING(RTITLE,k,0.)
        if(abs(TANGL).gt.0.001)then
          write(CTITLE,'(''The tilt axis is TLTAXA.'')')
          call shorten(CTITLE,k)
          CALL P2K_MOVE(220.,YPOSN,0.)
          CALL P2K_STRING(RTITLE,k,0.)
        endif
      endif
C
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(PLTSIZ,0.,0.)
      CALL P2K_DRAW(PLTSIZ,PLTSIZ,0.)
      CALL P2K_DRAW(0.,PLTSIZ,0.)
      CALL P2K_DRAW(0.,0.,0.)
C
      CENTRE=PLTSIZ/2.0
      CALL P2K_ORIGIN(CENTRE,CENTRE,0.)
      CALL P2K_MOVE(-CHRSIZ,-CHRSIZ,0.)
      CALL P2K_DRAW(CHRSIZ,CHRSIZ,0.)
      CALL P2K_MOVE(CHRSIZ,-CHRSIZ,0.)
      CALL P2K_DRAW(-CHRSIZ,CHRSIZ,0.)  ! CENTRAL CROSS AT ORIGIN.
C
C==============================================================================
C=====Plot vectors H and K=====================================================
C==============================================================================
C
      ALENGTH=SQRT(AX**2+AY**2)
      if(IOPTION.eq.1.or.IOPTION.eq.3)ALENGTH=ALENGTH*1.05
      X=(AX/ALENGTH)*(PLTSIZ/2.0)
      Y=(AY/ALENGTH)*(PLTSIZ/2.0)
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(X,Y,0.)             ! PLOT ASTAR VECTOR
      Y=Y-8.
      IF (Y.LE.-PLTSIZ/2.0) THEN
        Y=Y+10.
        X=X+10.
      ENDIF
      CALL P2K_MOVE(X,Y,0)
      WRITE(TMPTEXT(1:1),451)
451   FORMAT('H')
452   FORMAT('K')
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
      CALL P2K_CSTRING(TEXT,1,0.)
      CALL P2K_MOVE(X,Y,0)
      CALL P2K_CSTRING(TEXT,1,0.)
      CALL P2K_MOVE(X,Y,0)
      CALL P2K_CSTRING(TEXT,1,0.)
      CALL P2K_MOVE(X,Y,0)
      CALL P2K_CSTRING(TEXT,1,0.)

      BLENGTH=SQRT(BX**2+BY**2)
      if(IOPTION.eq.1.or.IOPTION.eq.3)BLENGTH=BLENGTH*1.05
      X=(BX/BLENGTH)*(PLTSIZ/2.0)
      Y=(BY/BLENGTH)*(PLTSIZ/2.0)
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(X,Y,0.)             ! PLOT BSTAR VECTOR
      X=X+8.
      IF (X.GE.PLTSIZ/2.0) THEN
        X=X-10.
        Y=Y-10.
      ENDIF
      CALL P2K_MOVE(X,Y,0)
      WRITE(TMPTEXT(1:1),452)
      CALL P2K_CSTRING(TEXT,1,0.)
      CALL P2K_MOVE(X,Y,0)
      CALL P2K_CSTRING(TEXT,1,0.)
      CALL P2K_MOVE(X,Y,0)
      CALL P2K_CSTRING(TEXT,1,0.)
      CALL P2K_MOVE(X,Y,0)
      CALL P2K_CSTRING(TEXT,1,0.)
C
C==============================================================================
C=====Plot tilt axis===========================================================
C==============================================================================
C
      IF (abs(TANGL).GT.0.001) THEN
C
        X=(AX/ALENGTH)*PLTSIZ/2.1
        Y=(AY/ALENGTH)*PLTSIZ/2.1
        RXY=SQRT(X**2+Y**2)
        XSHIFT=2*Y/RXY
        YSHIFT=-2*X/RXY
C
        if(IOPTION.eq.1.or.IOPTION.eq.3)then
          if(IHAND.gt.0)then
            CALL XYROTATE(RTAXA,X,Y,XR,YR)
          else
            CALL XYROTATE(-RTAXA,X,Y,XR,YR)
          endif
        else
          CALL XYROTATE(RTAXA,X,Y,XR,YR)
        endif
        CALL P2K_MOVE(-XR,-YR,0.)
        CALL P2K_DRAW(XR,YR,0.)         ! PLOT TILTAXIS IN Single LINE
        XR=XR*1.05
        YR=YR*1.05
        if(XR.gt.0)then
          XR=-XR
          YR=-YR
        endif
        if(XR.lt.20.)then
          XR=XR+10.
          YR=YR+3.
        endif
        CALL P2K_MOVE(XR,YR,0.)
        WRITE(TMPTEXT(1:4),'(''TAXA'')')
        CALL P2K_CSTRING(TEXT,4,0.)
C
        if(IOPTION.eq.2)then
          CALL XYROTATE(RTLTAXA,X,Y,XR,YR)
          CALL XYROTATE(RTLTAXA,XSHIFT,YSHIFT,XSHIFTR,YSHIFTR)
          CALL P2K_MOVE(-XR,-YR,0.)
          CALL P2K_DRAW(XR,YR,0.)               ! PLOT TILTAXIS IN TRIPLE LINES
          CALL P2K_MOVE(XR+XSHIFTR,YR+YSHIFTR,0.)
          CALL P2K_DRAW(-XR+XSHIFTR,-YR+YSHIFTR,0.)
          CALL P2K_MOVE(XR-XSHIFTR,YR-YSHIFTR,0.)
          CALL P2K_DRAW(-XR-XSHIFTR,-YR-YSHIFTR,0.)
          XR=XR*1.05
          YR=YR*1.05
          if(XR.gt.0)then
            XR=-XR
            YR=-YR
          endif
          if(XR.lt.20.)then
            XR=XR+10.
            YR=YR+3.
          endif
          CALL P2K_MOVE(XR,YR,0.)
          WRITE(TMPTEXT(1:6),'(''TLTAXA'')')
          CALL P2K_CSTRING(TEXT,6,0.)
        endif
C
      ENDIF
C
C==============================================================================
C=====Plot Resolution Rings====================================================
C==============================================================================
C
      DO 370 N=1,NRES                   ! NRES resoultion ranges
        if(RESTORE(N).GE.RORIRESMAX)then
          RES=1.0/RESTORE(N)
          RAD=SCALEP*RES

          DO 360 J=1,4                    
            DO 360 I=1,89         ! Resolution circles
              ANG=(90*J+I)*DRAD
              X=RAD*COS(ANG)
              Y=RAD*SIN(ANG)
              IF ((IOPTION.eq.1.or.IOPTION.eq.3).AND.abs(TANGL).GT.0.001) THEN
                if(iellipse.eq.1)then
                  X0=X
                  Y0=COS(TANGL)*Y
                  AXANG=atan2(AY,AX)
                  CALL XYROTATE(-AXANG,X0,Y0,X1,Y1)
C                 CALL XYROTATE(RTLTAXA,X1,Y1,X2,Y2)
                  if(IHAND.gt.0)then
                    CALL XYROTATE(RTAXA,X1,Y1,X2,Y2)
                  else
                    CALL XYROTATE(-RTAXA,X1,Y1,X2,Y2)
                  endif
                  X=X2
                  Y=Y2
                endif
              ENDIF
              IF (J.EQ.1.AND.I.EQ.1) THEN
                XOR=X
                YOR=Y
                CALL P2K_MOVE(X,Y,0.)
              ELSE
                CALL P2K_DRAW(X,Y,0.)
            ENDIF
360       CONTINUE
          CALL P2K_DRAW(XOR,YOR,0.)               !complete the circle
        endif
370   CONTINUE
C
C==============================================================================
C=====Plot data================================================================
C==============================================================================
C
20    continue
      READ(1,*,END=900)IH,IK,AMP,PHASE,IQ,BCK,CTF 
        call PLOTIT( IH, IK,IQ,AX,BX,AY,BY)
        call PLOTIT(-IH,-IK,IQ,AX,BX,AY,BY)
      goto 20
C
 900  continue
C
C  Here for termination
C
      WRITE(6,410) NSPOTS
410   FORMAT(' TOTAL SPOTS PLOTTED IN FILE =',I5,
     . ' and plot file closed')
      CALL P2K_PAGE
C
      CLOSE(1)
C
      STOP
      END
C
c==========================================================
c==========================================================
c==========================================================
c==========================================================
c==========================================================
c
      subroutine PLOTIT(IH,IK,IQ,AX,BX,AY,BY)
C
      PARAMETER (PLTSIZ = 300.0)
      PARAMETER (CHRSIZ = 0.6)
C
      CHARACTER*80 CNAME
C
      INTEGER IH,IK,IQ,IQLABEL
      DIMENSION TEXT(20)
      CHARACTER*20 TMPTEXT
      EQUIVALENCE (TMPTEXT,TEXT)
C
      COMMON //RESMAX,SCALEP,NRES,NSPOTS,
     .  FONTSIZE,IQLABEL,CNAME
C
      IF (IQ.GT.8) GO TO 100            ! PLOTS SPOTS WITH IQ =8 OR LESS.
      CALL P2K_FONT('Courier'//CHAR(0),0.6*FONTSIZE)    !REDUCE FONT SIZE
C       WRITE(6,421)IH,IK
421     FORMAT(' SPOT PLOTTED & FRIEDEL PAIR',2I5)
      J=1
      K=1
      X=IH*AX+IK*BX
      Y=IH*AY+IK*BY
      RESXY=SQRT(X*X+Y*Y)
C      IF(RESXY.GE.RESMAX)GO TO 100
C      IF (ABS(X).GE.RESMAX)GO TO 100
C      IF (ABS(Y).GE.RESMAX)GO TO 100

      X=X*SCALEP
      Y=Y*SCALEP
      IF ((ABS(X).GE.PLTSIZ/2.0).OR.(ABS(Y).GE.PLTSIZ/2.0)) GO TO 100
      XN=X-CHRSIZ*(8.1-IQ)/2
      XP=X+CHRSIZ*(8.1-IQ)/2
      YN=Y-CHRSIZ*(8.1-IQ)/2
      YP=Y+CHRSIZ*(8.1-IQ)/2
      NSPOTS=NSPOTS+1
      CALL P2K_MOVE(XN,YN,0.)
      CALL P2K_DRAW(XP,YN,0.)
      CALL P2K_DRAW(XP,YP,0.)
      CALL P2K_DRAW(XN,YP,0.)
      CALL P2K_DRAW(XN,YN,0.)           ! SQUARE ROUND EACH SPOT.

      X=X-0.1                           ! ADJUST CHARACTER TO BE CENTRAL IN X.
      Y=Y-0.8                           ! ADJUST CHARACTER TO BE CENTRAL IN Y.
      CALL P2K_MOVE(X,Y,0.)
      WRITE(TMPTEXT(1:1),460)
      IF (IQLABEL.EQ.0) GO TO 100
      IF (IQ.EQ.1) WRITE(TMPTEXT(1:1),461)      ! INCLUDE NUMBER FOR SPOTS WITH GOOD
      IF (IQ.EQ.2) WRITE(TMPTEXT(1:1),462)      ! 'IQ' VALUES
      IF (IQ.EQ.3) WRITE(TMPTEXT(1:1),463)      ! 
      IF (IQ.EQ.4) WRITE(TMPTEXT(1:1),464)      ! 
460   FORMAT(' ')
461   FORMAT('1')
462   FORMAT('2')
463   FORMAT('3')
464   FORMAT('4')
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*1.0)
      IF (IQ.LE.4) CALL P2K_CSTRING(TEXT,1,0)
100   CONTINUE
C
      RETURN
      END
C
c==========================================================
c
C-----This rotates counterclockwise:
C
      SUBROUTINE ROTATE(TAXAN,XA,YA,XB,YB)

      XB=XA*COS(TAXAN)-YA*SIN(TAXAN)
      YB=XA*SIN(TAXAN)+YA*COS(TAXAN)

      RETURN
      END
C
c==========================================================
c
C-----This rotates clockwise:
C
      SUBROUTINE XYROTATE(TAXAN,XA,YA,XB,YB)

      XB= XA*COS(TAXAN)+YA*SIN(TAXAN)
      YB=-XA*SIN(TAXAN)+YA*COS(TAXAN)

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
c==========================================================
c
      SUBROUTINE INKOMMA(CZEILE,k)
C
C replaces intermedieate spaces within the actual text string
C in CZEILE up to the length k by komma.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1,CTMP2,CTMP3
      CHARACTER * 100 CZEIL2
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
      IF(CTMP1.EQ.CTMP3)then
        WRITE(CZEILE(I:I),'('' '')')
        K=K-1
      endif
C
      if(k.lt.1)k=1
C
C     WRITE(*,'('' INKOMMA 4: k='',I4,'':'',A)')k,CZEILE(1:k)
C
      RETURN
      END
C
C===========================================================
C
      SUBROUTINE getnumbers(cline,NRES,RESTORE)
C
C Gets a list of komma-separated numbers from cline and places them into RESTORE
C The number of found values is returned in NRES
C
      character * (*) cline
      DIMENSION RESTORE(100)
      CHARACTER * 1 CTMP1
      CHARACTER * 1 CTMP2
      CHARACTER * 1 CTMP3
      CTMP1=' '
      CTMP2=' '
      CTMP3=','
C
      ilen=len(cline)
C
      call shorten(cline,k)
C      write(*,'(''::Entering getnumbers. cline='',A)')cline(1:k)
C
      NRES=0
      istart=1
C
C-----get numbers up to each komma
C
 100   continue
         k=istart
C--------find next komma
 200     continue
C           write(*,'(''::k='',I4,'', cline(k:k)='',A)')k,cline(k:k)
           READ(cline(k:k),'(A1)')CTMP1
           IF(CTMP1.NE.CTMP3)then
             k=k+1
             if(k.gt.ilen)goto300
             goto 200
           endif
  300    CONTINUE
C--------komma or end found, get number
         iend=k-1
         if(iend.lt.1)then
C----------string starts with kommas:
           istart=istart+1
           goto 100
         endif
         NRES=NRES+1
         if(NRES.GT.100)then
           NRES=100
           goto 900
         endif
C         write(*,'(''::Reading '',I6,'' from position '',
C     .      I4,'' to '',I4)')NRES,istart,iend
C         write(*,'(''::Reading from '',A,'' from position '',
C     .      I4,'' to '',I4)')cline(istart:iend),istart,iend
C
         read(cline(istart:iend),*)RESTORE(NRES)
         if(iend.ge.ilen)goto 999
         istart=iend+2
         if(istart.gt.ilen)goto 999
       goto 100
C
 900  continue
      write(*,'(''::WARNING: too many numbers'')')
C
 999  continue
C
      RETURN
      END
C
C===========================================================
C
      SUBROUTINE hand(AX,AY,BX,BY,IHAND)
C
C     Output in IHAND is either "1" for a right-handed lattice, 
C     or "-1" for a left-handed lattice.
C
      REAL RC1(3),RC2(3),RC3(3),RC4(3)
C
      pi=3.141592654
C
C-----Define reciprocal cell
C
      RC1(1) = AX
      RC2(1) = AY
      RC1(2) = BX
      RC2(2) = BY
      RC1(3) = 0.0
      RC2(3) = 0.0
C
      call norm(RC1)
      call norm(RC2)
C
      call cross(RC1,RC2,RC3)
      call spat(RC1,RC2,C)
C
      RTMP=abs(acos(C))*180.0/pi
      if(RTMP.gt. 90.0)RTMP=RTMP-180.0
      if(RTMP.lt.-90.0)RTMP=RTMP+180.0
      if(RC3(3).LT.0.0)RTMP=-RTMP
      RANGLE=RTMP
C
C      write(*,'(''Lattice is '',4F12.3)')AX,AY,BX,BY
C      write(*,'(''Included angle is '',F12.3)')RANGLE
      if(RC3(3).ge.0.0)then
C        write(*,'(''Right-handed system'')')
        IHAND=1
      else
C        write(*,'(''Left-handed system'')')
        IHAND=-1
      endif
C
      RETURN
      END
C
C------------------------------------------------------------------------------
C
      SUBROUTINE NORM(A)
C
C*****Normalizes the vector A
C
      dimension A(3)
C
      call leng(A,RL)
      IF(RL.GT.0.0)then
        A(1) = A(1) / RL
        A(2) = A(2) / RL
        A(3) = A(3) / RL
      ELSE
        WRITE(*,'('' WARNING: Normalize zero vector.'')')
      ENDIF
C
      return
      end
C
C------------------------------------------------------------------------------
C
      SUBROUTINE CROSS(A,B,C)
C
C*****Calculates C=AxB  (output is a vector)
C
      dimension A(3),B(3),C(3)
C
      C(1) = A(2)*B(3) - A(3)*B(2)
      C(2) = A(3)*B(1) - A(1)*B(3)
      C(3) = A(1)*B(2) - A(2)*B(1)
C
      return
      end
C
C------------------------------------------------------------------------------
C
      SUBROUTINE SPAT(A,B,C)
C
C*****Calculates C=A.B (output is scalar)
C
      dimension A(3),B(3)
C
      C = A(1)*B(1) + A(2)*B(2) + A(3)*B(3)
C
      return
      end
C
C------------------------------------------------------------------------------
C
      SUBROUTINE LENG(A,RL)
C
C*****Calculates the length RL of Vector A  (output is scalar)
C
      dimension A(3)
C
      RL = SQRT(A(1)*A(1) + A(2)*A(2) + A(3)*A(3))
C
      return
      end
C
C------------------------------------------------------------------------------
C
      SUBROUTINE vecdistort(RX,RY,TAXA,TANGL)
C
C-----rotate vector by TAXA degrees
C
      call rotate(TAXA,RX,RY,RNX,RNY)
C
C-----elongate Y-coordinate according to TANGL
C
      if(abs(cos(TANGL)).gt.0.0001)then
        RNY=RNY/cos(TANGL)
      else
        write(6,'(''::ERROR: 90 degree tilt angle not possible.'')')
        stop
      endif
C
C-----rotate vector by -TAXA degrees
C
      call rotate(-TAXA,RNX,RNY,RX,RY)
C
      RETURN
      END
C

