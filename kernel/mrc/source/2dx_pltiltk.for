C**PLTILTK********************************************************************
C
C PLOT TILT GEOMETRY DISTRIBUTION AFTER ALL CONVERSION IS MADE
C BASED ON EQUIVALENT TILT DEFINED BY NORMAL OF THE TILT PLANE
C ALSO PLOT KAPPA50 AS ESTIMATE OF REQUIRED TILT AXIS OVERLAP
C
C CHENG, ANCHI; YEAGER, MARK (2004)
C Acta Crystallographica Section A, in press.
C
C for questions and bugs in the programs, contact acheng@scripps.edu
C
C version 1.0   5.24.01         AC
C version 2.0K 11.03.03         AC  Converted to plot2k format
C                                   Fix Extra FIDDLET problem
C                                   Include Minimal TAXA overlap arcs and
C                                   various labels and rm subroutine
C                                   tltplot because of linux problem
C version 2.1  12.05.03         AC  Plot kappa50 and kappa0 only at max TANGL
C version 2.2  01.05.04         AC  Rename the program to PLTILTK for mrc
C                                   distribution
C##############################################################################
C
C   INPUT CARD
C
C 1     TILTEP                  (A)
C
C 2     TANGLST,TANGLMAX        (*,*)
C
C 3     IMQLABEL                (*)
C
C 4     RMAX,IQMAX              (*)
C
C 5     RGOOD                   (*)
C
C 6     ISPGRP,NPROG,NTILT,NBEAM,ILIST,ALNG,BLNG,WIDTH,ANG,IPLOT,MINRFL   (*)
C---------1st input card for origtiltd
C
C 7     IFILM,TITLE                                            (I10,10A4)
C---------1st real film input, i.e., non-dummy
C 8     CFILIN                                                        (A)
C
C 9     NWGT  (Not used)                                              (*)
C
C 10    TAXA,TANGL,IORIGT                                             (*)
C
C 11    ORIGH,ORIGK,STEP,WIN,ISGNXCH,SCALE,ROT180,IREVHK,CTFREV,IROT90,IIREVHND,IREVXSGN   (*)
C
C 12    CS,KV,TILTH,TILTK   (Not used)                                (*)
C
C 13    DRESMAX, DRESMIN (Not used)                                   (*)
C
C 14    IFILM<0  --- THIS ENDS DATA INPUT
C
C##############################################################################
C
C  OUTPUT
C
C  PLTLT.PS - ps file in polar coordinate where Radius is TANGL
C               Angle is TAXA.
C               Note that TANGL<0 is plotted as TAXA+180,ABS(TANGL)
C
C  TLTASM    - text file of the results
C##############################################################################
C
C Since this program uses most of the origtilt input for convenience of editing,
C many variables are not used.  Only the variables that matter are explained
C here
C
C       TILTEP          Plot title
C       TANGLST         TANGL arc step size in degree
C       TANGLMAX        Maximal TANGL for the plot
C       IMQLABEL                If =0 no IMQ label in the plot
C                          =1 include IMQ label in the plot
C       RMAX            Maximal resolution in 3D to be used in determining IMQ
C       IQMAX           Maximal IQ to be included in determining IMQ
C       RGOOD           Completeness cutoff used in determining IMQ
C                       0.0 < RGOOD =< 1.0
C       ISPGRP          NUMBER OF SPACE GROUP AS IN ORIGTILT
C       ALNG            A AXIS IN ANGSTROMS for untilted crystal.
C       BLNG            B AXIS IN ANGSTROMS        "        "   .
C       WIDTH           C AXIS IN ANGSTROMS        "        "   .
C       ANGAB           ANGLE BETWEEN A AND B - ONLY FOR P1 OR P2
C       IFILM           INTEGER FILM IDENTIFIER
C       TITLE           DESCRIPTION OF FILM
C       CFILIN          NAME OF FILE CONTAINING H,K,A,P,IQ DATA
C       TAXA            ANGLE MEASURED FROM THE TILT AXIS TO THE A-AXIS,
C                       MEASURED IN DIRECTION OF A TO B BEING POSITIVE.
C       TANGL           TILT ANGLE IN DEGREES
C       ISGNXCH         IF NOT EQUAL TO 0, FLIP AROUND A AXIS, USEFUL IN P121
C       ROT180          IF NOT=0, ROTATE 180 DEG ABOUT Z-AXIS, USEFUL IN P1,P3
C       IREVHK          IF NOT = 0, H AND K ARE INTERCHANGED ON INPUT.
C       IROT90          IF NOT=0, ROTATE 90  DEG ABOUT Z-AXIS, USEFUL IN P2
C       IREVHND         IF NOT = 0, H AND K ARE INTERCHANGED ON INPUT, and Z flipped
C       REVXSGN         IF NOT = 0, H is transformed into -H
C
C       IMQ     Image Quality value. IMQ is determined by the highest
C               resolution bin in which more than RGOOD of the spots have IQ
C               better than or equal to IQMAX
C               The resolution bins are divided equally in the reciprocal
C               Space in JRMAX.  Therefore, currently
C               IMQ=1 if ,in resolution range of RMAX to (4/3)*RMAX A, at least
C                       RGOOD of the spots have IQ =< IQMAX
C               IMQ=2 if the above condition is not met in such as resolution
C                       bin but is valid in the resolution of (4/3)*RMAX to
C                       2*RMAX
C               IMQ=3 if the highest resolution bin in which the condition is
C                       met is 2*RMAX to 4*RMAX
C               IMQ=4 if the highest resolution bin in which the condition is
C                       met is 4*RMAX to infinity
C               IMQ=5 if the condition is never met in all resolution bins
C                       (The image should not be used in the merging if IMQ=5)
C
C###############################################################################
C
C
C SPACE GROUP MATRICES --- convention for p3, p4 and p6 is H,0 (not 0,K).
      INTEGER*2 IGO(8,17)
      DATA IGO/8*5,2*4,2*5,2*4,2*5,
     A 4,5,4,5,4,5,4,5,  4,5,4,5,4,5,4,5,  4,5,4,5,4,5,4,5,
     B 2,4,2,5,2,4,2,5,  2,4,2,5,2,4,2,5,  2,4,2,5,2,4,2,5,
     C 2,4,2,5,2,4,2,5,  3,4,3,5,3,4,3,5,  1,2,1,4,1,2,1,5,
     D 1,2,1,4,1,2,1,5,  4,5,4,5,3,5,3,5,  2,4,2,4,1,5,1,5,
     E 2,4,2,4,1,5,1,5,  3,4,3,5,1,4,1,5,  2,3,2,4,1,3,1,5/
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
     E            -1,-1,1,0,1,0,0,1,         1,1,-1,0,-1,0,0,-1/
      REAL STANG(17)            ! STANDARD SPACE GROUP ANGLES.
      DATA STANG/2*0.0,10*90.0,5*120.0/
      REAL ASANG(17)            ! ASYMETRIC UNIT SPACE GROUP ANGLES.
      LOGICAl AZ(17)            ! ASYMETRIC UNIT SPACE GROUP Z>=0 FLAG
      DATA ASANG/2*180.0,8*90.0,2*45.0,60.0,2*30.0,60.0,30.0/
      DATA AZ/.FALSE.,.TRUE.,3*.FALSE.,7*.TRUE.,3*.FALSE.,2*.TRUE./

C     NUMBER   SPACEGROUP    ASYMMETRIC TILT SPACE
C
C          1          P1         180>TAXA>=0
C
C          2         P21         180>TAXA>0,TANGL>=0
C
C          3         P12         90>TAXA>=0
C
C          4        P121         90>TAXA>=0
C
C          5         C12         90>TAXA>=0
C
C          6        P222         90>TAXA>=0,TANGL>=0
C
C          7       P2221         90>TAXA>=0,TANGL>=0
C
C          8      P22121         90>TAXA>=0,TANGL>=0
C
C          9        C222         90>TAXA>=0,TANGL>=0
C
C         10          P4         90>TAXA>=0,TANGL>=0
C
C         11        P422         45>TAXA>=0,TANGL>=0
C
C         12       P4212         45>TAXA>=0,TANGL>=0
C
C         13          P3         60>TAXA>=0
C
C         14        P312         30>TAXA>=0
C
C         15        P321         30>TAXA>=0
C
C         16          P6         60>TAXA>=0,TANGL>=0
C
C         17        P622         30>TAXA>=0,TANGL>=0
C
C##############################################################################

      INTEGER TOTRFL
      PARAMETER (TOTRFL=1000000)
      PARAMETER (MAXRFL=10000)
      PARAMETER (JRMAX=4)

      PARAMETER (ID=150)
      PARAMETER (RESMAX=0.3)
      PARAMETER (PLTSIZ=300.0)
      PARAMETER (CHRSIZ=0.3)
      PARAMETER (IPTMAX=1000)

C  DIMENSION STATEMENTS FOR PLOT
      DIMENSION TITLP(15),TEXT(20),RESTORE(10)
     . !UP TO 10 TANGL ARCS
      INTEGER IARC,ISTEP,IMQLABEL,ITARC,ITEN,NAB,LZERO
      REAL X,Y,SCALEP,ANG,RADIUS,RAD,RAD2,RAD3
      REAL ASANGL,TANGLST,TANGLMAX,RES,TARC,ROT,ROFF
      REAL*8 COSKAHLF,KAPPAF,KAPPAZ
C      CHARACTER*60 TITLP
       CHARACTER*1 ATEN(3),ATENT(3)
      CHARACTER*9 ANUM
      CHARACTER*3 BAXIS
CTSH++
      CHARACTER*4 TMPTEXT,ARCV,ARCSTR
      EQUIVALENCE (TMPTEXT,TEXT)
CTSH--
C
C  DIMENSION STATEMENTS FOR REFLECTION LIST INPUT.
      INTEGER IHIN(MAXRFL),IKIN(MAXRFL),IQIN(MAXRFL)
      INTEGER*8 IFILM,ISER
      INTEGER IMQ,IORIGT,ISPOT(JRMAX),IGOOD(JRMAX)
      INTEGER IREVHK,ISGNXCH,IROT180,IROT90,CTFREV
      INTEGER IH,IK,IQ
      CHARACTER FTITLE*80,CFILIN*200,CTITLE*200,CLINE*200
      REAL*4 CS,KV,TILTH,TILTK
      REAL*8 ORIGH,ORIGK,STEP,WIN,SCALE,TAXA,TANGL,DRESMAX,DRESMIN,TAXBN
      REAL*4 A,P,BCK,CTF,W
      LOGICAL IFILE

C  DIMENSION STATEMENTS FOR MAIN INPUT PROGRAM.
      DIMENSION TITLE(10),RLIMIT(JRMAX+1),RR(JRMAX+1)
      DIMENSION KAPPAFDEG(10),KAPPA0(10)
      INTEGER ISPGRP,NPROG,ILIST,IPLOT,MINRFL
      REAL*4 ALNG,BLNG,WIDTH,ANGAB,TITLE,ABANG
      REAL*4 ASTAR,BSTAR,CSTAR,ABANGST
      LOGICAL NBEAM,NTILT,NREFOUT,NSHFTIN,NWGT
C      CHARACTER*60 TITLP
      REAL DRAD,FACTOR
      REAL GOODRAT,RGOOD
      REAL ASTANGL,AROT,AMIR,ZT
      REAL*8 TANGLNOW
      REAL*8 RTAXA(5),RH2(5),RK2(5),Z2(5),RDIF,KAPPAFDEG,KAPPAOUTDEG
      REAL OZ,COSKAHALF,KAPPA0,SQRTAC,AREAK
      INTEGER IN1,MAX1,IHK
      INTEGER*4 JSIGN(TOTRFL)
      LOGICAL ASZ
C
      DRAD=3.14159/180
      MAX1=MAXRFL+1
      FONTSIZE=4.0      ! SELECT 4MM CHAR HEIGHT FOR TEXT
      DRAD=3.1415962/180.0
      ROFF=5.0

25      FORMAT(2I4,2F8.1,I3,F8.1,F8.3)
110   FORMAT(/,' TILT ANGLE & AXIS DISTRIBUTION PLOT,'
     .          ,/,' 2.1 (05.12.03 AC)',/,
     .             ' 3.0 (10.19.08 2dx)',/)
115   FORMAT('  TWO SIDED PLANE GROUP ',I3,/)
117   FORMAT('  CELLAXES: A ',F7.2,'  B ',F7.2,
     1  '  AB ANGLE ',F7.2,' DEGREES',/)
120   FORMAT(I10,10A4)
125   FORMAT(' *********************************************************
     1*************',/)
135   FORMAT(' IMAGE ',I10,5X,10A4)
136   FORMAT(' FOUND ',10A4)
138   FORMAT(10A4)
148   FORMAT(/,/,'ERROR: MORE THAN',I5,' REFLECTIONS FOR THIS IMAGE')
149   FORMAT(/,/,'0TOTAL NUMBER OF REFLECTIONS IS MORE THAN',I5)
151   FORMAT(' REQUIRED FILM IDENTIFIER DOES NOT MATCH FILM SERIAL NUMBER
     1 AT',/,'  HEAD OF DATALIST,  IFILM=',I10,'    ISER=',I10)
157   FORMAT('  INITIAL TAXA=',F8.3,' TANGL=',F8.3)
158   FORMAT('    FINAL TAXA=',F8.3,' TANGL=',F8.3)
163   FORMAT(' ISGNXCH=',I3,', ROT180=',I3,', IREVHK=',I3,', IROT90=',
     .  I3,', IREVHND=',I3,', REVXSGN=',I3)
166   FORMAT(/,3X,I10,' REFLECTIONS READ INTO CORE. ')
167   FORMAT(3X,I10,' REFLECTIONS IN THE HIGH TILT QUADRANTS')
168   FORMAT(' Cur_RESMAX TOTAL GOOD Completeness Curr_IMQ')
169   FORMAT(3X,F6.2,3X,I3,2X,I3,4X,F6.3,7X,I2)
170   FORMAT(' TANGL(deg) KAPPA50(deg) KAPPAO(deg) KAPAAREA(Ang^-2)')
171   FORMAT(2X,F5.1,3X,F9.1,3X,F9.1,6X,F12.8)
172   FORMAT(' ISPGRP=',I3,' A=',F6.2,' B=',F6.2,' ABANG=',F6.2)
173   FORMAT(' completeness cutoff at ',F4.2,' and IQMAX= ',I1)
174   FORMAT('     TAXA    TANGL IMQ FILE')
175   FORMAT(2F9.3,I3,' ',A)
1005  FORMAT(A)
1010  FORMAT(' RESOLUTION BINS:')
1011  FORMAT(' IMQ=',I1,' ',F6.2,' A to ',F6.2,' A')
9201    FORMAT(' INPUT FILE NAME ',A)

      READ(5,10)TITLP
10    FORMAT(15A4)
      WRITE(6,10)TITLP
      PRINT *,'INPUT TANGL RING STEP SIZE & MAX IN DEGREES'
      READ(5,*)TANGLST,TANGLMAX

      PRINT *,'INCLUDE IMQ value label? YES-1 NO-0'
      READ (5,*)IMQLABEL
      PRINT *,'RESOLUTION LIMIT AND IQMAX for quality asessment'
      READ (5,*)RMAX,IQMAX
      READ (5,*)RGOOD
      PRINT *,'DATA BINNED IN',JRMAX
C
      RLIMIT(JRMAX+1)=0
      RR(JRMAX+1)=999.0
      DO 50 JR=1,JRMAX
        RLIMIT(JR)=(JRMAX+1-JR)/(RMAX*JRMAX)
        RR(JR)=1/RLIMIT(JR)
50    CONTINUE
C
C
C        READ SPACE GROUP NUMBER, LIST PARAMETER, UNIT CELL AXES AND IF
C              SPACE GROUP IS P1 OR P2, THE INTER AXIS ANGLE
C
C
      READ(5,*) ISPGRP,NPROG,NTILT,NBEAM,ILIST,ALNG,BLNG,WIDTH,ANGAB,
     1          IPLOT,MINRFL
C
C     IMAT SHOWS WHICH MATRICES WILL BE USED FROM MAT FOR EACH SPACE GROUP
C       THE FIRST ELEMENT OF EACH IS PASSED TO SET,ASYM FOR LATER USE.
C       THE SAME IS DONE FOR IGO WHICH CONTROLS PROGRAM FLOW IN SET,ASYM.
C
      ABANG=STANG(ISPGRP)
      ASANGL=ASANG(ISPGRP)
      ASZ=AZ(ISPGRP)
      IF(ISPGRP.LE.2) ABANG=ANGAB
      WRITE(6,110)              ! PLTLT header output, with version number.
      WRITE(6,115)ISPGRP
      IF(ISPGRP.GT.9) BLNG=ALNG
      WRITE(6,117)ALNG,BLNG,ABANG
      WRITE(6,125)
      ASTAR=1.0/(ALNG*SIN(DRAD*ABANG))
      BSTAR=1.0/(BLNG*SIN(DRAD*ABANG))
      CSTAR=1.0/(WIDTH)
      ABANGST=180.-ABANG  ! NOW ABANGST IS RECIPROCAL SPACE ANGLE.

      CALL CCPDPN(9,'TLTASM','UNKNOWN','F',0,0)
C
      write(6,'('' TLTASM opened'')')
C
      NARC = TANGLMAX /TANGLST
      WRITE(9,170)
      DO 1500 N=1, NARC
         TANGLNOW = TANGLST*N
         TANGLNOW1 = TANGLNOW*DRAD                             !KAPPA50
         CALL KAPPA50PERC(CSTAR,TANGLNOW,RLIMIT(1),KAPPAOUTDEG,AREAK)
         KAPPAFDEG(N) = KAPPAOUTDEG
            OZ=RLIMIT(1)*SIN(TANGLNOW1)
            SQRTAC=SQRT(RLIMIT(1)*RLIMIT(1)-(OZ-CSTAR)**2)
            COSKAHALF=(OZ-CSTAR)/(TAN(TANGLNOW1)*SQRTAC)
            KAPPA0(N)=2*ACOS(COSKAHALF)/ DRAD
        WRITE(9,171) TANGLNOW,KAPPAFDEG(N),KAPPA0(N),AREAK
1500  CONTINUE
C
      write(6,'('' Initializing image data reading and plotting'')')
C     INITIALIZE IMAGE DATA READING & PLOTING
C
      JREFL=0
C
C
C  Here for initialisation
C

        WRITE(6,3019)TITLP
3019      FORMAT(1X,15A4)
        RES=RLIMIT(1)
        ASANGL1=0.0
        TANGLMAX1=0.0
        TANGLST1=1.0
        ASANGL1=ASANGL*DRAD
        TANGLST1=TANGLST*DRAD
        TANGLMAX1=TANGLMAX*DRAD
        SCALEP=PLTSIZ/(2.0*TANGLMAX1)
        IARC=TANGLMAX1/TANGLST1
        ANUM='123456789'
        IF(IARC.GT.10) THEN
          PRINT *,'TOO MANY TANGL ARCS (<10)'
          GOTO 3100
        ENDIF
3400     CONTINUE

        WRITE(6,3020)
3020      FORMAT(' ENTERING TLTPLOT INITIALISATION')
        NSPOTS = 0
        YPOSN=PLTSIZ+5.
        CALL P2K_OUTFILE('TLTPLOT.PS',10)
        CALL P2K_HOME
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
        CALL P2K_GRID(0.5*PLTSIZ,0.5*PLTSIZ,1.0)
        CALL P2K_ORIGIN(-0.5*PLTSIZ,-0.7*PLTSIZ,0.)
        CALL P2K_COLOUR(0)
        CALL P2K_MOVE(10.0,YPOSN,0.)
        CALL P2K_STRING(TITLP,60,0.)
        CALL P2K_MOVE(0.,0.,0.)
C        CALL P2K_DRAW(PLTSIZ,0.,0.)            !No box drawing
C        CALL P2K_DRAW(PLTSIZ,PLTSIZ,0.)
C        CALL P2K_DRAW(0.,PLTSIZ,0.)
C        CALL P2K_DRAW(0.,0.,0.)
        CENTRE=PLTSIZ/2.0
        CALL P2K_ORIGIN(CENTRE,CENTRE,0.)
        CALL P2K_MOVE(-CHRSIZ,-CHRSIZ,0.)
        CALL P2K_DRAW(CHRSIZ,CHRSIZ,0.)
        CALL P2K_MOVE(CHRSIZ,-CHRSIZ,0.)
        CALL P2K_DRAW(-CHRSIZ,CHRSIZ,0.)        ! CENTRAL CROSS AT ORIGIN.
        X= PLTSIZ/2.0
        Y= 0.0
        IF (ASZ) THEN
          CALL P2K_MOVE(0.,0.,0.)
        ELSE
           CALL P2K_MOVE(-X,-Y,0.)
        ENDIF
        CALL P2K_DRAW(X,Y,0.)           ! PLOT 1st axis VECTOR
        X=PLTSIZ/2.0+ROFF
        CALL P2K_MOVE(X,Y,0)
        CALL P2K_STRING('0',1,0)        !0 TAXA LABEL
        X2=X/2
        Y=-4*ROFF
        CALL P2K_MOVE(X2,Y,0)
        CALL P2K_CSTRING('TANGL(deg)',10,0)   !TANGL LEGEND
        BLENGTH=SQRT(BX**2+BY**2)
        X=COS(ASANGL1)*(PLTSIZ/2.0)
        Y=SIN(ASANGL1)*(PLTSIZ/2.0)
        IF (ASZ) THEN
          CALL P2K_MOVE(0.,0.,0.)
        ELSE
          CALL P2K_MOVE(-X,-Y,0.)
        ENDIF
        CALL P2K_DRAW(X,Y,0.)           ! PLOT 2nd axis VECTOR

        NAB=INT(ASANGL)
        LZERO=0

        Do 3339 NMOD=2,0,-1
          ITEN=INT(NAB/(10**NMOD))
          NAB=NAB-ITEN*(10**NMOD)
          IF (ITEN.NE.0) THEN
          ATENT(NMOD+1)=ANUM(ITEN:ITEN)
            LZERO=1
          ELSE
            IF (LZERO.EQ.0) THEN
               ATENT(NMOD+1)=' '
            ELSE
               ATENT(NMOD+1)='0'
            ENDIF
          ENDIF
3339     CONTINUE
        BAXIS=ATENT(3)//ATENT(2)//ATENT(1)
        X=COS(ASANGL1)*(PLTSIZ/2.0+ROFF)
        Y=SIN(ASANGL1)*(PLTSIZ/2.0+ROFF)
        CALL P2K_MOVE(X,Y,0)
        CALL P2K_STRING(BAXIS,3,0)

        NRN=0

        X=COS(ASANGL1/2)*(PLTSIZ/2.0+ROFF) !TAXA
        Y=SIN(ASANGL1/2)*(PLTSIZ/2.0+ROFF)
        ROT=ASANGL/2-90.0
        CALL P2K_MOVE(X,Y,0)
        CALL P2K_CSTRING('TAXA',4,ROT)

        CALL P2K_MOVE(0,-2*ROFF,0)
        CALL P2K_CSTRING('0.0',3,0)

3340    DO 3370 N=1,IARC                        ! TANGL arcs
          RAD=SCALEP*TANGLST1*N
          IF (NRN.EQ.0) THEN
            TARC=TANGLST1*N/DRAD
            ITARC=INT(TARC*10)
            write(6,'(''TARC='',F12.3)')TARC
            Do 3341 NMOD= 2,0,-1
              ITEN=ITARC/(10**NMOD)
              ITARC=ITARC-ITEN*10**NMOD
              IF (ITEN.NE.0) THEN
                ATEN(NMOD+1)=ANUM(ITEN:ITEN)
              ELSE
                ATEN(NMOD+1)='0'
              ENDIF
3341         CONTINUE
            ARCSTR=ATEN(3)//ATEN(2)//'.'//ATEN(1)
            X=RAD
            Y=-2*ROFF
            CALL P2K_MOVE(X,Y,0)
            CALL P2K_CSTRING(ARCSTR,4,0)
          ENDIF
          ISTEP=ASANGL
           DO 3360 I=1,ISTEP
             ANG=I*DRAD
             X=RAD*COS(ANG)
             Y=RAD*SIN(ANG)
             IF (I.EQ.1) THEN
               XOR=X
               YOR=Y
               CALL P2K_MOVE(RAD,0.,0.)
               CALL P2K_DRAW(X,Y,0.)
             ELSE
               CALL P2K_DRAW(X,Y,0.)
             ENDIF
3360       CONTINUE

           IF (NRN.EQ.0) THEN                 !KAPPA50
            OZ=RES*SIN(TANGLST1*N)
            RAD2=SCALEP*ASIN((OZ-CSTAR)/RES)
            KAPPAF = KAPPAFDEG(N)*DRAD
            IF (KAPPAF.GT.ASANGL1) KAPPAF=ASANGL1
             IF (N.EQ.IARC) THEN
              DO 3365 I=1,ISTEP
               ANG=I*DRAD
               IF (ANG.LE.KAPPAF) THEN
                 X=(RAD2)*COS(ANG)
                 Y=(RAD2)*SIN(ANG)
                 IF (I.EQ.1) THEN
                   XOR=X
                   YOR=Y
                   CALL P2K_MOVE(RAD,0.,0.)
                   CALL P2K_DRAW(RAD2,0.,0.)
                   CALL P2K_DRAW(X,Y,0.)
                 ELSE
                   CALL P2K_DRAW(X,Y,0.)
                   IKAPPA50 = I
                 ENDIF
               ENDIF
3365         CONTINUE

             X=(RAD2)*COS(KAPPAF)              !last two points
             Y=(RAD2)*SIN(KAPPAF)
             CALL P2K_DRAW(X,Y,0)

             X=(RAD)*COS(KAPPAF)
             Y=(RAD)*SIN(KAPPAF)
             CALL P2K_DRAW(X,Y,0)

            KAPPAZ = KAPPA0(N)*DRAD            !KAPPA0
            RAD3=RAD-0.3*(RAD-RAD2)
            XK50 = (RAD3)*COS(KAPPAF)
            YK50 = (RAD3)*SIN(KAPPAF)
            CALL P2K_MOVE(XK50,YK50,0)

            IF (KAPPAZ.GT.ASANGL1) KAPPAZ=ASANGL1
              DO 3367 I=IKAPPA50,ISTEP
               ANG=I*DRAD
               IF (ANG.LE.KAPPAZ.AND.ANG.GE.KAPPAF) THEN
                 X=(RAD3)*COS(ANG)
                 Y=(RAD3)*SIN(ANG)
                 CALL P2K_DRAW(X,Y,0.)
               ENDIF
3367         CONTINUE

             X=(RAD3)*COS(KAPPAZ)              !last two points
             Y=(RAD3)*SIN(KAPPAZ)
             CALL P2K_DRAW(X,Y,0)

             X=(RAD)*COS(KAPPAZ)
             Y=(RAD)*SIN(KAPPAZ)
             CALL P2K_DRAW(X,Y,0)
            ENDIF
           ENDIF
3370     CONTINUE
         NRN=NRN+1
         IF ((.NOT.ASZ).AND.(NRN.EQ.1)) THEN
           TANGLST1=-TANGLST1
           GOTO 3340
         ENDIF

      WRITE(9,172)ISPGRP,ALNG,BLNG,ABANG
      WRITE(6,172)ISPGRP,ALNG,BLNG,ABANG
      WRITE(9,173)RGOOD,IQMAX
      DO 51 JR=1,JRMAX
        WRITE(6,1011) JR,RR(JR+1),RR(JR)
        WRITE(9,1011) JR,RR(JR+1),RR(JR)
51    CONTINUE
      WRITE(9,174)

C
C     READ TILTED DATA----THIS PART FOLLOWS ORIGTILT FORMAT EVEN
C     THOUGH MANY OF THE VARIABLES ARE NOT USED IN THIS PROGRAM
C
220   WRITE(6,125)
CHEN>
      read(5,'(A)')CLINE
      call shorten(CLINE,k)
      write(6,'('' Read: '',A)')CLINE(1:k)
      if(k.lt.11)then
        read(CLINE(1:k),'(I10)')IFILM
        write(CTITLE,'(10X)')
      else
        read(CLINE(1:k),'(I10,A200)')IFILM,CTITLE
      endif
C     READ(5,120)IFILM,TITLE
      IF(IFILM.LT.0) GO TO 500
      call shorten(CTITLE,k)
      WRITE(6,'('' IMAGE '',I10,5X,A)') IFILM,CTITLE(1:k)
CHEN<
      READ(5,1005) CFILIN
      OPEN(UNIT=11,FILE=CFILIN,STATUS='OLD')
      WRITE(6,9201)CFILIN
      READ(5,*) NWGT
      READ(5,*) TAXA,TANGL,IORIGT

        TAXB=TAXA+ABANGST
        TAXA0=TAXA
        TANGL0=TANGL
      READ(5,*)ORIGH,ORIGK,STEP,WIN,ISGNXCH,SCALE,IIOT180,IREVHK,CTFREV,IROT90,IIREVHND,IREVXSGN
      READ(5,*)CS,KV,TLTH,TLTK
      WRITE(6,163)ISGNXCH,IROT180,IREVHK,IROT90,IIREVHND,IREVXSGN
C
C     READ RESOLUTION LIMITS FOR THIS FILM; DEFAULT 100.0  3.5
C     SAME AS ORIGTILT BUT NOT USED HERE
      READ(5,*)DRESMAX,DRESMIN
C
C     READ A,P DATA
C
      NIN=11
      READ(NIN,*)ISER
      IF(ISER.NE.IFILM) GO TO 602
C       BACKSPACE NIN
C       READ(NIN,138)FTITLE
C        WRITE(6,136)FTITLE
        ITOTAL=0
      DO 9249 JR = 1,JRMAX
        ISPOT(JR)=0
        IGOOD(JR)=0
9249  CONTINUE
      DO 250 IN=1,MAX1
9250    IF (NWGT) THEN
           READ(NIN,*,END=260)IH,IK,A,P,IQ,BCK,CTF,W
        ELSE
           READ(NIN,*,END=260)IH,IK,A,P,IQ,BCK,CTF
           W=1.000
        ENDIF
      IF(IQ.LT.1)IQ=1
      IF(IQ.GT.9)IQ=9           ! DANGEROUS STATEMENT FOR FUTURE -- BEWARE!
      IF(IH.GE.900) GO TO 260
C
C     In-plane resolution
C
      DSTARSQ=(IH*ASTAR)**2+2*IH*IK*ASTAR*BSTAR*COS(DRAD*ABANGST)
     .+(IK*BSTAR)**2
      DSTARABS=SQRT(DSTARSQ)

270   TAXB = TAXA + ABANGST
      STAXA=ASTAR*SIN(DRAD*TAXA)
      STAXB=BSTAR*SIN(DRAD*TAXB)
      TTANGL=TAN(TANGL*DRAD)
      DPERP=IH*STAXA+IK*STAXB
      Z=DPERP*TTANGL
C
C     Convert to conventional unit cells
C
      CALL FIDDLE2(IH,IK,Z,IREVHK,ISGNXCH,IROT180,IROT90,IREVHND,REVXSGN)

C
C     3D resolution
C
      RADSQ=IH**2*ASTAR**2 + IK**2*BSTAR**2 +
     .       2.0*IH*IK*COS(0.0174532*ABANGST)*ASTAR*BSTAR+Z**2
      RADABS=SQRT(RADSQ)
      IF(RADABS.GT.RLIMIT(1))GO TO 9250
      ZMAX=SQRT(DSTARSQ)*SIN(ABANGST*DRAD)*TTANGL
      ZMIN=SQRT(RADSQ)*SIN(TANGL*DRAD)/SQRT(COS(TANGL*DRAD)**2+1)
      IF (ABS(Z).GE.ABS(ZMIN)) THEN
        ITOTAL=ITOTAL+1
        DO 273 JR=1,JRMAX
           IF ((RADABS.LE.RLIMIT(JR)).AND.(RADABS.GT.RLIMIT(JR+1))) THEN
             ISPOT(JR)=ISPOT(JR)+1
             IF (IQ.LE.IQMAX) IGOOD(JR)=IGOOD(JR)+1
           ENDIF
273     CONTINUE
      ENDIF

C
224   CONTINUE
C
250   CONTINUE
      WRITE(6,148) MAXRFL
      STOP
260   CONTINUE
      IN1=IN-1
      WRITE(6,166) IN1
      WRITE(6,167) ITOTAL
      WRITE(6,168)
      IMQ=JRMAX+1
      DO 262 JR=1,JRMAX
        JR1=JRMAX+1-JR
        IF (ISPOT(JR1).EQ.0) ISPOT(JR1)=1
        GOODRAT=1.0*IGOOD(JR1)/ISPOT(JR1)
        IF (GOODRAT.GE.(RGOOD-0.0005)) IMQ=JR1
        WRITE(6,169)RR(JR1),ISPOT(JR1),IGOOD(JR1),GOODRAT,IMQ
262   CONTINUE
C
C     convert tilt axis to conventional, useful in certain space group
C
      CALL FIDDL2T(ABANGST,TAXA,TANGL,IREVHK,ISGNXCH,IROT180)
C
C
C     TAXA,TANGL conversion into asymmetric unit
C
      TAXB=TAXA+ABANGST
      WRITE(6,157)TAXA0,TANGL0
      SIGNTL=1
      RH=-COS((TAXB)*DRAD)*SIN(TANGL*DRAD)
      IF (RH.LT.0.0) SIGNTL=-1
      RH=SIGNTL*RH
      RK=SIGNTL*COS((TAXA)*DRAD)*SIN(TANGL*DRAD)
      ZT=SIGNTL*COS(TANGL*DRAD)
      CALL ASYMT(RH,RK,ZT,
     1  MAT(1,IMAT(1,ISPGRP)),MAT(1,IMAT(2,ISPGRP)),
     2  MAT(1,IMAT(3,ISPGRP)),MAT(1,IMAT(4,ISPGRP)),
     3  MAT(1,IMAT(5,ISPGRP)),
     4  IGO(1,ISPGRP))
      TANGL=ACOS(ZT)/DRAD
C     TANGL=SIGNTL*TANGL
      IF ((TANGL.LT.0.01).AND.(TANGL.GT.-0.01)) THEN
        TAXA=0
        GOTO 9170
      ENDIF
      IF (TANGL.GE.90.0) TANGL=TANGL-180
      TAXA=ACOS(RK/SIN(TANGL*DRAD))/DRAD
C
C     TAXA calculated above may not necessarily be the right answer because
C     ACOS is not unique in the range of 0-360 deg.  The following is
C     a quick fix by back calculating RH,RK,Z from the four possible
C     values
C
      RTAXA(1)=180-TAXA
      ATANGL=TANGL
      RMIN=10000

      DO 2200 IHK=2,5
      IF (IHK.EQ.4) THEN
        RTAXA(IHK)=-RTAXA(IHK-1)
      ELSE
        RTAXA(IHK)= 180-RTAXA(IHK-1)
      ENDIF
      TAXB2=RTAXA(IHK)+ABANGST
      SIGNTL=1
      RH2(IHK)=-COS((TAXB2)*DRAD)*SIN(TANGL*DRAD)
      IF (RH2(IHK).LT.0.0) SIGNTL=-1
      RH2(IHK)=SIGNTL*RH2(IHK)
      RK2(IHK)=SIGNTL*COS((RTAXA(IHK))*DRAD)*SIN(TANGL*DRAD)
      Z2(IHK)=SIGNTL*COS(TANGL*DRAD)
      RDIF=ABS(RH2(IHK)-RH)+ABS(RK2(IHK)-RK)+ABS(Z2(IHK)-ZT)
      IF (RDIF.LT.RMIN) THEN
        TAXA=RTAXA(IHK)
        RMIN=RDIF
      ENDIF
2200  CONTINUE

      TAXB=TAXA+ABANGST

      IF (TAXA.GE.180) THEN
         TAXA=TAXA-180
         TANGL=-TANGL
      ENDIF
C
C     MOVING TAXA INTO THE ASYM UNIT SINCE THE CALC. IS BASED ON
C     THE NORMAL OF THE CENTRAL ZONE AND CAN BE OFF THE PARTICULAR
C     ASYM UNIT THAT WE WANT TO PLOT
C     NOTE THAT ALL ASYM HAS 180 > TAXA >=0, -90<TANGL<=90
C
      IF (TAXA.LT.0) THEN
         TAXA=TAXA+180
         TANGL=-TANGL
      ENDIF

      IF ((ISPGRP.EQ.2).AND.(TANGL.LT.0)) TANGL=-TANGL

      IF ((ISPGRP.EQ.13).AND.(TAXA.GE.60)) THEN
         TAXA=TAXA-60
         TANGL=-TANGL
      ENDIF

      IF ((ISPGRP.EQ.14)) THEN
         IF (TAXA.GE.30) THEN
           TAXA=60-TAXA
           TANGL=-TANGL
         ENDIF
      ENDIF

      IF ((ISPGRP.EQ.15)) THEN
         IF (TAXA.GE.30) TAXA=60-TAXA
      ENDIF

      IF ((ISPGRP.EQ.16).AND.(TAXA.GE.60)) TAXA=TAXA-60

      IF (ISPGRP.EQ.17) THEN
        IF(TAXA.GE.30) TAXA=60-TAXA
      ENDIF

C
C     WRITING AND PLOTING THE FINAL RESULTS
C
9170  CONTINUE
      WRITE(6,158)TAXA,TANGL
      WRITE(9,175)TAXA,TANGL,IMQ,CFILIN
C
C  Here for spot plots
C
C      print *,TANGL,TAXA,SCALEP
        NSPOTS=NSPOTS+1
        RADIUS=ABS(SCALEP*TANGL*DRAD)
        ANG=TAXA*DRAD
        X=RADIUS*COS(ANG)
        Y=RADIUS*SIN(ANG)
10001 FORMAT(3F12.5)
10002 FORMAT(2F12.5)
C      WRITE(6,10002)SCALEP,DRAD
C      WRITE(6,10001)X,Y,RADIUS
      XN=X-CHRSIZ*1.5*(8.1-IMQ)
      XP=X+CHRSIZ*1.5*(8.1-IMQ)
      YN=Y-CHRSIZ*1.5*(8.1-IMQ)
      YP=Y+CHRSIZ*1.5*(8.1-IMQ)
      NSPOTS=NSPOTS+1
      CALL P2K_MOVE(XN,Y,0.)
      CALL P2K_DRAW(X,YN,0.)
      CALL P2K_DRAW(XP,Y,0.)
      CALL P2K_DRAW(X,YP,0.)
      CALL P2K_DRAW(XN,Y,0.)            ! DIAMOND ROUND EACH SPOT.
      IF(IMQLABEL.EQ.0) GO TO 3100
      X=X-0.2                           ! ADJUST CHARACTER TO BE CENTRAL IN X.
      Y=Y-1.5                           ! ADJUST CHARACTER TO BE CENTRAL IN Y.
      CALL P2K_MOVE(X,Y,0.)
CTSH++
        WRITE(TMPTEXT(1:1),3160)
        IF(IMQ.EQ.1) THEN
          CALL P2K_FONT('Helvatica'//CHAR(0),0.8*FONTSIZE)!REDUCE FONT SIZE
          WRITE(TMPTEXT(1:1),3161)      ! IMQ=1 include number
        ELSE
          IF(IMQ.EQ.2) THEN
            CALL P2K_FONT('Helvatica'//CHAR(0),0.6*FONTSIZE)!REDUCE FONT SIZE
            WRITE(TMPTEXT(1:1),3162)    ! IMQ=2 include number
          ELSE
            IF(IMQ.EQ.3) THEN
              CALL P2K_FONT('Helvatica'//CHAR(0),0.6*FONTSIZE)!REDUCE FONT SIZE
              WRITE(TMPTEXT(1:1),3163)  ! IMQ=3 include number
            ELSE
              IF(IMQ.EQ.4) THEN
                CALL P2K_FONT('Helvatica'//CHAR(0),0.5*FONTSIZE)!REDUCE FONT SIZE
                WRITE(TMPTEXT(1:1),3164)        ! IMQ=4 include number
              ENDIF
            ENDIF
          ENDIF
        ENDIF
CTSH--
3160    FORMAT(' ')
3161    FORMAT('1')
3162    FORMAT('2')
3163    FORMAT('3')
3164    FORMAT('4')
        IF (IMQ.LE.4) CALL P2K_CSTRING(TMPTEXT,1,0.)
3100   CONTINUE
      GO TO 220         ! BACK TO DO INPUT FOR ANOTHER FILM.

500   CONTINUE
C
C  Here for termination
C
      CLOSE(UNIT=9)
            WRITE(6,3110) NSPOTS
3110        FORMAT(' TOTAL SPOTS PLOTTED IN TTPLOT FILE =',I5,
     .          ' and plot file closed')
            CALL P2K_PAGE

      GO TO 1107
601   WRITE(6,149) TOTRFL
      STOP
602   WRITE(6,151)IFILM,ISER
      STOP
1107  CONTINUE
      STOP
      END

C******************************************************************************
C  FIDDLING WITH THE INDEXING TO GET CORRECT MATCH TO INDEXING CONVENTION
C  USEFUL IN A NUMBER OF SPACE GROUPS -- SEE WRITE-UP AT TOP OF PROGRAM.
      SUBROUTINE FIDDLE(IH,IK,Z,IREVHK,ISGNXCH,IROT180)
      IF(IREVHK.EQ.0.0) GO TO 225
      I=IH
      IH=IK
      IK=I
      Z=-Z
225   CONTINUE
      IF(ISGNXCH.EQ.0.0) GO TO 230
      IK=-IK
      Z=-Z
  230 IF(IROT180.EQ.0) GO TO 231
      IH=-IH
      IK=-IK
231   CONTINUE
      RETURN
      END
C******************************************************************************
C  FIDDLING WITH THE INDEXING TO GET CORRECT MATCH TO INDEXING CONVENTION
C  USEFUL IN A NUMBER OF SPACE GROUPS -- SEE WRITE-UP AT TOP OF PROGRAM.
      SUBROUTINE FIDDLE2(IH,IK,Z,IREVHK,ISGNXCH,IROT180,IROT90,IREVHND,
     1  REVXSGN)
      IF(IREVHK.EQ.0.0) GO TO 225
        I=IH
        IH=IK
        IK=I
        Z=-Z
 225  CONTINUE
      IF(ISGNXCH.EQ.0.0) GO TO 230
        IK=-IK
        Z=-Z
 230  IF(IROT180.EQ.0) GO TO 231
        IH=-IH
        IK=-IK
 231  CONTINUE
      if(IROT90.ne.0)then
        I=IH
        IH=-IK
        IK=I
      endif
      if(IREVHND.ne.0)then
        Z=-Z
      endif
      if(REVXSGN.ne.0)then
        IH=-IH
      endif
      RETURN
      END
C******************************************************************************
C  FIDDLING WITH THE TILT AXIS AND TILT ANGLE TO GET CORRECT MATCH TO
C  CONVENTION
C  USEFUL IN A NUMBER OF SPACE GROUPS

      SUBROUTINE FIDDLET(ANG,TAXA,TANGL,IREVHK,ISGNXCH,IROT180)
      REAL*8 TAXA,TANGL

      IF(IREVHK.EQ.0.0) GO TO 225
      TAXA=-ANG-TAXA
      TANGL=-TANGL
225   CONTINUE
      IF(ISGNXCH.EQ.0.0) GO TO 230
      TAXA=-TAXA
      TANGL=-TANGL
230   IF(IROT180.EQ.0) GO TO 231
      TANGL=-TANGL
231   CONTINUE
235   IF (TAXA.GE.180.0) THEN
        TAXA=TAXA-180.0
        TANGL=-TANGL
        GO TO 235
      ELSE
        IF (TAXA.LT.0) THEN
          TAXA=TAXA+180.0
          TANGL=-TANGL
          GO TO 235
        ENDIF
      ENDIF
      RETURN
      END
C******************************************************************************
C  FIDDLING WITH THE TILT AXIS AND TILT ANGLE TO GET CORRECT MATCH TO
C  CONVENTION
C  USEFUL IN A NUMBER OF SPACE GROUPS

      SUBROUTINE FIDDL2T(ANG,TAXA,TANGL,IREVHK,ISGNXCH,IROT180,IROT90,IREVHND,
     1  REVXSGN)
      REAL*8 TAXA,TANGL

      IF(IREVHK.EQ.0.0) GO TO 225
      TAXA=-ANG-TAXA
      TANGL=-TANGL
225   CONTINUE
      IF(ISGNXCH.EQ.0.0) GO TO 230
      TAXA=-TAXA
      TANGL=-TANGL
230   IF(IROT180.EQ.0) GO TO 231
      TANGL=-TANGL
231   CONTINUE
      if(IROT90.ne.0)then
        TAXA=TAXA+90
      endif
      if(IREVHND.ne.0)then
        TANGL=-TANGL
      endif
235   IF (TAXA.GE.180.0) THEN
        TAXA=TAXA-180.0
        TANGL=-TANGL
        GO TO 235
      ELSE
        IF (TAXA.LT.0) THEN
          TAXA=TAXA+180.0
          TANGL=-TANGL
          GO TO 235
        ENDIF
      ENDIF
      if(IREVXSGN.ne.0)then
        write(*,'(/,/,''::ERROR: this needs to be implemented still.'')')
      endif
      RETURN
      END
C**************************************************************************
C  ASYMT  Modified from subroutine ASYM of origtiltd.f to handle real values
C         in tilt vectors
C******************************************************************************
      SUBROUTINE ASYMT(RH,RK,ZT,
     1  A1,A2,A3,A4,A5,IGO)
      INTEGER*2 A1(8),A2(8),A3(8),A4(8),A5(8),IGO(8)

      IF(RH.LT.0) CALL MULTR(A1,RH,RK,Z)
      PASS=0    ! second pass (22.1.90) to check all changes made correctly.
50    INDEX=1
      IF(RK.GE.0) INDEX=INDEX+1
      IF(ZT.GE.0.0) INDEX=INDEX+2
      IF(RH.LT.ABS(RK)) INDEX=INDEX+4
      INDEX=IGO(INDEX)
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
100    CALL MULTR(A5,RH,RK,ZT)
       GO TO 50
150    CALL MULTR(A4,RH,RK,ZT)
       GO TO 50
200    CALL MULTR(A3,RH,RK,ZT)
       GO TO 50
250    CALL MULTR(A2,RH,RK,ZT)

500    CONTINUE

      PASS=PASS+1                       !
      IF(PASS.EQ.1) GO TO 50            ! Check through again once only.
C                                       ! Done 22.1.90 to fix the -ve zstar
C                                       ! obtained for h=0,k=-ve refls in p2.

600    CONTINUE
      RETURN
      END
C*******************************************************************************
      SUBROUTINE MULTR(IA,RH,RK,ZT)
C
C     DOES MATRIX MULTIPLICATION TO BRING POINTS IN RECIPROCAL SPACE
C     INTO THE ASYMMETRIC UNIT.
C
C     (X' Y' Z')=(X Y Z) <A>
C
C
C        <A> HAS FORM     IA(1)  IA(3)     0
C                         IA(2)  IA(4)     0
C                             0      0 IA(5)
C           FOR ALL CASES.
C
C
      INTEGER*2 IA(5)
      RH1=IA(1)*RH+IA(2)*RK
      RK=IA(3)*RH+IA(4)*RK
      RH=RH1
      ZT=IA(5)*ZT
      RETURN
      END

C******************************************************************************
      SUBROUTINE KAPPA50PERC(CSTARN,TANGLDEG,RSTAR,KAPPA50,AREA)
      PARAMETER (NXTOTAL=20)
      PARAMETER (NYTOTAL=40)
      PARAMETER (NOMEMAX=270)

      DIMENSION OMEGADEG(NOMEMAX),OPERC(NOMEMAX)

      REAL*4 CSTARN
      REAL*8 TODEG
      REAL*8 TANGLDEG,TANGL,OA,AB,AC,BC,KAPPA,OMEGA
      REAL*8 XDELTA,YDELTA,OMEDELTA,X,Y,OMEGADEG,KAPPADEG
      REAL*8 ARC1,ARC2,LINE1,LINE2
      REAL OPERC,AREA
      REAL*8 KAPPA50,KAPPA50HIGH,KAPPA50LOW,OPERCHIGH,OPERCLOW
      INTEGER NX,NY,NOMEGA,NOVERLAP,NTOTAL,INDEX50,INDEX50M
      INTEGER NOMETOTAL

      TODEG = 180/3.1415926535898

      NOMETOTAL = INT(TANGLDEG*3)
      KAPPA50 = 0.0
      TANGL = TANGLDEG / TODEG
      OA = RSTAR*SIN(TANGL)-CSTARN
      AB = OA/TAN(TANGL)
      AC = SQRT(RSTAR*RSTAR-OA*OA)
      BC = SQRT(AC*AC-AB*AB)
      KAPPA = 2*ACOS(AB/AC)
      KAPPADEG = KAPPA * TODEG
      XMIN = AB
      XMAX = RSTAR*COS(TANGL)
      XDELTA = (XMAX-XMIN)/NXTOTAL
      YMIN = -BC
      YMAX = BC
      YDELTA = (YMAX-YMIN)/NYTOTAL
      OMEDELTA = KAPPA/NOMETOTAL
      INDEX50 = 0
      DO 2000 NOMEGA = 0,NOMETOTAL-1
        NTOTAL = 0
        NOVERLAP = 0
        OMEGA = NOMEGA * OMEDELTA
        DO 1000 NX = 1,NXTOTAL
          DO 1000 NY = 1,NYTOTAL
            X = XMIN + XDELTA*NX
            Y = YMIN + YDELTA*NY
            ARC1 = (X/COS(TANGL))**2 + Y**2 -RSTAR**2
            LINE1 = X - AB
            IF (ARC1.LT.0.0.AND.LINE1.GT.0.0) THEN
              NTOTAL = NTOTAL + 1
              XROT = X*COS(OMEGA) - Y*SIN(OMEGA)
              YROT = X*SIN(OMEGA) + Y*COS(OMEGA)
              ARC2 = (XROT/COS(TANGL))**2 + YROT**2 -RSTAR**2
              LINE2 = XROT - AB
              IF (ARC2.LT.0.0.AND.LINE2.GT.0.0) THEN
                NOVERLAP = NOVERLAP + 1
              ENDIF
            ENDIF

1000    CONTINUE
        NOMINDEX = NOMEGA + 1
        OPERC(NOMINDEX) = 100.0 * NOVERLAP/ NTOTAL
        OMEGADEG(NOMINDEX) = OMEGA*TODEG
        IF (OPERC(NOMINDEX).LE.50.0.AND.INDEX50.EQ.0) THEN
          INDEX50 = NOMINDEX
          INDEX50M = INDEX50 - 1
          OPERCHIGH =  OPERC(INDEX50M) - 50.0
          OPERCLOW = OPERC(INDEX50) - 50.0
          KAPPA50HIGH = OMEGADEG(INDEX50M)
          KAPPA50LOW = OMEGADEG(INDEX50)
          if(abs(OPERCHIGH-OPERCLOW).lt.0.0000000001)then
            write(6,'(''ERROR: OPERCHIGH-OPERCLOW is zero.'')')
          endif
          KAPPA50 = KAPPA50HIGH - (KAPPA50HIGH-KAPPA50LOW) *
     .    (OPERCHIGH)/(OPERCHIGH-OPERCLOW)
          AREA=(XMAX-XMIN)*(YMAX-YMIN)*NTOTAL/(NXTOTAL*NYTOTAL)
        ENDIF

2000  CONTINUE
      print *,KAPPA50
      RETURN
      END
c
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

