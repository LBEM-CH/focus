C*HISTOK.FOR *******************************************************************
C
C       Program to calculate histogram of an image.
C       Remember also to change version number in format statement
C       
C       Version 1.0 Revised output formats                      8.6.87  RH
C       Version 1.1 List number of densities in each bin        1.9.89  RH
C       Version 1.2 Increase dimensions to 15000x15000          7.1.96  RH
C       Version 1.3 remove references to %REF                   22.4.96 JMS
C       Version 1.4 Minor do loop correction (J = JC)           11.9.98 JMS
C       Version 1.5 Convert to P2K O/P direct to postscript    14.11.00 TSH
C       Version 1.6 old program name HISTO changed to HISTOK   20.11.00 RH
C                   P2K_FONT needed string terminator          13.6.01  TSH
C
        COMMON//NX,NY,NZ,IXMIN,IYMIN,IZMIN,IXMAX,IYMAX,IZMAX
C       DIMENSION ALINE(0:16383),NXYZ(3),MXYZ(3),HISTO(1400)
        DIMENSION ALINE(0:21000)
        DIMENSION NXYZ(3),MXYZ(3),HISTO(1400)
        DIMENSION LABELS(20,10)
        DIMENSION RLABELS(20,10)
        DIMENSION TEXT(20)
        CHARACTER*80 TMPTEXT
        REAL*8 AMEAN
        CHARACTER*60 FULLNAME
C       COMPLEX CLINE(0:8191)
        COMPLEX CLINE(0:10500)
        EQUIVALENCE(NX,NXYZ), (ALINE,CLINE)
        EQUIVALENCE (TMPTEXT,TEXT)
        EQUIVALENCE (LABELS,RLABELS)
        DATA HISTO/1400*0/ NHIS/1001/
        INTEGER IQD
C
        WRITE(6,1000)
1000    FORMAT(//,' HISTOK 1.6 (20.11.00): Image histogram calculation',//)
        CALL IMOPEN(1,'IN','RO')
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C
        CALL ICLLIM(1,IXMIN,IXMAX,MXYZ) 
C
        WRITE(6,1200)
1200    FORMAT(/,'$Linear (0) or Logrithmic (1) display ? ')
        READ(5,*)IQD

        AMIN = 1.E10
        AMAX = -1.E10
        AMEAN = 0.0
        AMODE = 0.0
        HMIN =  1.E10
        HMAX = -1.E10
        NTOT = MXYZ(1)*MXYZ(2)*MXYZ(3)
C
        DO 100 IZ = IZMIN,IZMAX
          CALL IMPOSN(1,IZ,IYMIN)
          DO 100 IY = IYMIN,IYMAX
            CALL IRDLIN(1,ALINE,*98)
            DO 100 IX = IXMIN,IXMAX
              IF (MODE .LE. 2) THEN
                VAL = ALINE(IX)
              ELSE
                VAL = CABS(CLINE(IX))
              END IF
C
              IF (VAL .LT. AMIN) AMIN = VAL
              IF (VAL .GT. AMAX) AMAX = VAL
              AMEAN = AMEAN + VAL
100     CONTINUE
        SCL = (NHIS - 1)/(AMAX - AMIN)    
        DO 200 IZ = IZMIN,IZMAX
          CALL IMPOSN(1,IZ,IYMIN)
          DO 200 IY = IYMIN,IYMAX
            CALL IRDLIN(1,ALINE,*98)
            DO 200 IX = IXMIN,IXMAX
              IF (MODE .LE. 2) THEN
                VAL = ALINE(IX)
              ELSE
                VAL = CABS(CLINE(IX))
              END IF
C
              INDEX = (VAL - AMIN)*SCL + 1.5
CHENN>
              if (INDEX.le.1) INDEX = 1
              if (INDEX.ge.1400) INDEX = 1400
CHENN<
              HISTO(INDEX) = HISTO(INDEX) + 1
200     CONTINUE
C
        AMEAN = AMEAN/NTOT
        K = 0
        NTOT2 = NTOT/2
        WRITE(6,298)
        DO 300 J = 1,NHIS
          H = HISTO(J)
          VAL = (J - 1)/SCL + AMIN
          AMODE = AMODE + H*VAL
          K = K + H
          IF (IQD .EQ. 1) THEN
            HISTO(J) = 0.
            IF (H .GT. 1.) HISTO(J) = ALOG10(H)
            H = HISTO(J)
          ELSE
C             IF(HISTO(J).NE.0) WRITE(6,299) VAL,HISTO(J)
298             FORMAT(//'NUMBER OF DENSITIES IN EACH BIN'//)
299             FORMAT(2F15.2)
          END IF
          IF (H .LT. HMIN) HMIN = H
          IF (H .GT. HMAX) HMAX = H
          IF (K .GE. NTOT2) GOTO 300
          AMEDIAN = VAL
300     CONTINUE
10      AMODE = AMODE/NTOT
C
        WRITE(6,2000) NTOT,AMIN,AMAX,AMEAN,AMODE,AMEDIAN
2000    FORMAT(/,' Histogram analysis for ',I15,'  points:',/,
     .  10x,' MIN,MAX values ......... ',2G14.5,/,
     .  10x,' MEAN,MODE,MEDIAN ....... ',3G14.5,/)
C
C  NOW DO PLOTTING
C
        CALL IRTLAB(1,LABELS,NL)
C       Change next line for Alliant
C       CALL QINQUIRE(IMUNIT(1),NAME,LEN)
        CALL QQINQ(IMUNIT(1),'IN',FULLNAME,NFILSZ)
C       Change next line for Alliant
C       CALL CRTPLT('PLOT',0)
        CALL P2K_OUTFILE('HISTO.PS',8)
        CALL P2K_HOME
        CALL P2K_LWIDTH(0.3)
        CALL P2K_FONT('Helvetica'//CHAR(0),3.0)
        FLABELHT=1.5*3.0/105
        FLABELSPACE=NL*FLABELHT
C make a landscape-mode plot origin
        CALL P2K_MOVE(0.8-FLABELSPACE,-1.2,0.0)
        CALL P2K_TWIST(90.0,180.0,0.0)
        CALL P2K_HERE
        NWID = 1400/NHIS
C
C   FIRST PLOT AXES
C
        HMAX=1.2*HMAX
        CALL P2K_GRID((AMAX-AMIN)/1.6,(HMAX-HMIN)/(1.5-FLABELSPACE),1.0)
        IF (IQD.EQ.0) THEN
          CALL P2K_AXES(AMIN,AMAX,20,0.0,HMAX,-15,0.0,0.0,ADJUSTEDX, ADJUSTEDY,'Density Values', 14,'Number',6)
        ENDIF
        IF (IQD.NE.0) THEN
          CALL P2K_AXES(AMIN,AMAX,20,0.0,HMAX,-15,0.0,0.0,ADJUSTEDX, ADJUSTEDY,'Density Values', 14,'LOG 10 Number',13)
        ENDIF
C
C  THEN HISTOGRAM
C
        DX=1.0
        DY=1.0
        AXMIN=0.0
        AYMIN=0.0
CCC     SCL=1.0
        DO 400 J = 1,NHIS
          X = (((J - 1)/SCL + AMIN) - AXMIN)/DX
          Y = (HISTO(J) - AYMIN)/DY
          CALL P2K_MOVE(X-ADJUSTEDX,0.0,0.0)
          CALL P2K_DRAW(X-ADJUSTEDX,Y,0.0)
400     CONTINUE
C
C  THEN FINALLY, TITLES AND LABELS
C
C  set an origin at the (approx) bottom l.h. corner of the paper
        CALL P2K_MOVE(0., -FLABELSPACE, 0.0)
        CALL P2K_HERE
C  set a simple text-positioning gridsize
        CALL P2K_GRID(10.0,10.0,1.0)
        FONTSIZE=6.0
C  draw the labels at a position relative to the plot origin
        CALL P2K_FONT('Helvetica'//CHAR(0),0.7*0.75*FONTSIZE)
        DO 500 J = 1,NL
ccc       Y = -1.5 - .37*J
          Y = -1.5 - 10.*FLABELHT*J
          CALL P2K_MOVE(0.,Y,0.)
          CALL P2K_STRING(RLABELS(1,J),80,0.)
500     CONTINUE
C  Draw the plot titles at a constant position on the paper.
        CALL P2K_MOVE(0.0, -10.0*FLABELSPACE, 0.0)
        CALL P2K_HERE
        CALL P2K_FONT('Helvetica'//CHAR(0),FONTSIZE)
        CALL P2K_MOVE(10., 15., 0.)
        DO JC = 60,1,-1
          J = JC
          IF (FULLNAME(J:J) .NE. ' ') GOTO 5
        END DO
5       WRITE(TMPTEXT,3000) FULLNAME(1:J)
3000    FORMAT('Histogram of : ',A)
        CALL P2K_CSTRING(TEXT,J+15,0.)
        CALL P2K_FONT('Helvetica'//CHAR(0),0.7*FONTSIZE)
        WRITE(TMPTEXT,3100) IXMIN,IXMAX,IYMIN,IYMAX,IZMIN,IZMAX
3100    FORMAT('Min,Max XYZ:',6I5)
        CALL P2K_MOVE(17.,10.,0.)
        CALL P2K_STRING(TEXT,42,0.)
        WRITE(TMPTEXT,3200) AMIN,AMAX
3200    FORMAT('Min,Max Vals: ',2G11.4)
        CALL P2K_MOVE(17.,9.5,0.)
        CALL P2K_STRING(TEXT,40,0.0)
        WRITE(TMPTEXT,3300) AMEAN,AMODE
3300    FORMAT('Mean,Mode: ',2G11.4)
        CALL P2K_MOVE(17.,9.,0.)
        CALL P2K_STRING(TEXT,40,0.0)
        WRITE(TMPTEXT,3400) AMEDIAN,NTOT
3400    FORMAT('Median, # points: ',G11.4,I8)
        CALL P2K_MOVE(17.,8.5,0.)
        CALL P2K_STRING(TEXT,40,0.)
C
        GOTO 99
98      WRITE(6,9000)
9000    FORMAT(//,' ******* END-OF-FILE ERROR ON READ ******!!!',//)
99      CALL P2K_PAGE
        CALL EXIT
        END
