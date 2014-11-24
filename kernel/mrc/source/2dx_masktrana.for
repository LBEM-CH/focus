C*MASKTRANA.FOR***************************************************************
C  Program for masking a Fourier transform                                   *
C     Input transform in standard format on stream 1 (IN)                    *
C     Output masked transform in standard format on stream 2 (OUT)           *
C     Control file contains parameters for generation of list of holes in    *
C         mask on stream 5, normal input stream (MASK)                       *
C                                                                            *
C     Mask file in free format                                               *
C                                                                            *
C  -  first record  ISHAPE, IAMPLIMIT, ISPOTFILE                             *
C     ISHAPE=1 Hard edge circular holes                                      *
C          2 Soft edge circular holes (Gaussian weighted to EXP(-2) at edge) *
C          3 Hard edge rectangular holes                                     *
C     IAMPLIMIT  if T then spots allowed through mask are limited in their   *
C                     maximum amplitude (they are reduced to 2x average)     *
C                if F then no amplitude reduction is applied                 *
C     ISPOTFILE  if F then input from stream 5 as in old program MASKTRAN    *
C                if T then input from file SPOTS
CHENN>
C     IHOLEFILE  if F then input holediameter from stream 5                  *
C                if T then input holediameter from file hole1.tmp            *
CHENN<
C                                                                            *
C  -  second record RAD       (ISHAPE = 1 or 2) radius (circular holes)      *
C                DELX,DELY (ISHAPE = 3)  half edge lengths(rectangular holes)*
C                                                                            *
C  -  third record AX,AY,BX,BY,IHMIN,IHMAX,IKMIN,IKMAX,RMAX,ITYPE            *
C       AX    etc, lattice parameters from refined NNBOX output in grid units*
C       IHMIN etc, set min/max limits on H,K for lattice generator           *
C       RMAX  Cutoff radius in transform for lattice generator in grid units *
C       ITYPE if = 0, then generate all spots                                *
C             if = 1, then include only spots of given index (see below)     *
C                                                                            *
C     Followed by record for each required spot to be allowed through mask   *
C             (only if ITYPE=1; input either from stream 5 or file SPOTS)    *
C         IH,IK                                                              *
C                                                                            *
C     Derived from TRMASK(Version 1.04)22-SEP-82 (RAC)                       *
C                                                                            *
C     Version 1.00    3-JUL-84        RH   See above.                        *
C     Version 1.01   27-JUL-87        RH   Option to limit maximum amplitude *
C     Version 1.02   31-MAR-88        RH   Debug spots for which XPOS=0      *
C          Converted to Alliant 12-NOV-91  RB   no changes                   *
C     Version 2.00   19-MAY-93        RH   read spots from file SPOTS        *
C     Version 2.01   17-APR-95        RH   check overflow of NHOLEMAX        *
C     Version 2.02   18-APR-95        RH   no (0,0) using lattice generator  * 
C     Version 2.03   25-JUL-95        RH   cosmetic removal of statement 99  * 
C     Version 2.04   03-SEP-95        RH   IDEEP=200 plus several checks for * 
C                                          hole geometry within transform    *
C                                                                            *
C*****************************************************************************
C
C  IATOT SHOULD BE SET TO BE ISIZE*IDEEP -- DONE IN PROGRAM
C
CHENN>
C      PARAMETER (ISIZE=5500)
C
      PARAMETER (ISIZE=20100)
CHENN<
      PARAMETER (IDEEP=200)
      PARAMETER (NHOLEMAX=7000)
      COMMON//NX,NY,NZ
      DIMENSION ARRAY(ISIZE,IDEEP),TITLE(20),NXYZR(3),MXYZR(3),
     .IHS(NHOLEMAX),IKS(NHOLEMAX),XC(NHOLEMAX),YC(NHOLEMAX),
     .ASUM(NHOLEMAX),NSUM(NHOLEMAX),SCALESPOT(NHOLEMAX)
      DIMENSION BRRAY(ISIZE*IDEEP)
CHENN>
C      LOGICAL IAMPLIMIT,IPASS2,ISPOTFILE
C
      LOGICAL IAMPLIMIT,IPASS2,ISPOTFILE,IHOLFILE
CHENN<
      CHARACTER DAT*24
      EQUIVALENCE (NX,NXYZR),(ARRAY,BRRAY)
CTSH++
      CHARACTER*80 TMPTITLE
      EQUIVALENCE (TMPTITLE,TITLE)
CTSH--
C     DATA ZERO/0.0/,IPASS2/.FALSE./,SCALESPOT/NHOLEMAX*1.0/,IASK/'*'/
      DATA ZERO/0.0/,IPASS2/.FALSE./,SCALESPOT/NHOLEMAX*1.0/
      DATA ASUM/NHOLEMAX*0.0/,NSUM/NHOLEMAX*0/
C*** initialization added by jms 06.03.96
      do i=1,nholemax
        asum(i) = 0.
        nsum(i) = 0
        scalespot(i) = 1.0
      end do
      ISIZE2=ISIZE/2
      IATOT=ISIZE*IDEEP
C
      WRITE(6,1000)
1000  FORMAT(//,'   MASKTRANA: ',
     . 'Transform masking program V2.04(3-Sep-95)',//)
C
C  Read in input transform header data and open output file.
      CALL IMOPEN(1,'IN','RO')
      CALL IMOPEN(2,'OUT','NEW')
      CALL FDATE(DAT)
      CALL IRDHDR(1,NXYZR,MXYZR,MODE,DMIN,DMAX,DMEAN)
      IF(NX*2.GT.ISIZE) THEN
        WRITE(6,1499) NX*2
1499    FORMAT(' PROG DIMS NOT BIG ENOUGH, 2*NX=',I6)
        STOP
      ENDIF
      CALL ICRHDR(2,NXYZR,MXYZR,4,TITLE,0)
      CALL ITRLAB(2,1)
CTSH      IF(IAMPLIMIT) WRITE(TITLE) DAT(5:24,1501)
CTSH      IF(.NOT.IAMPLIMIT) WRITE(TITLE) DAT(5:24,1500)
CTSH++
      IF(IAMPLIMIT) WRITE(TMPTITLE,1501) DAT(5:24)
      IF(.NOT.IAMPLIMIT) WRITE(TMPTITLE,1500) DAT(5:24)
CTSH--
1500  FORMAT('MASKTRAN: Transform masked',
     . ' - amplitude unchanged ',2X,A20)
1501  FORMAT('MASKTRAN: Transform masked',
     . ' - amplitude reduction applied ',2X,A20)
      CALL IWRHDR(2,TITLE,1,ZERO,ZERO,ZERO)
C
      NX1=NX-1
      NXP2=2*NX
      NY2=NY/2
      NY21=NY2+1
      NZ1=NZ-1
      TMIN=1.E10
      TMAX=-1.E10
      TMEAN=0.
C
C     Read in mask params on stream 5.    
      WRITE(6,1600)
1600  FORMAT(///,'   Parameters of mask')
CHENN>
C      READ(5,*) ISHAPE,IAMPLIMIT,ISPOTFILE
C      WRITE(6,1700) ISHAPE,IAMPLIMIT,ISPOTFILE
C 1700  FORMAT(/,'   Hole shape',I12/
C      .        '   Amplitude reduction  ',L1/
C      .        '   separate spotfile ?  ',L1)
C
      READ(5,*) ISHAPE,IAMPLIMIT,ISPOTFILE,IHOLFILE
      WRITE(6,1700) ISHAPE,IAMPLIMIT,ISPOTFILE,IHOLFILE
1700  FORMAT(/,'   Hole shape',I12,/
     .        '   Amplitude reduction  ',L1,/,
     .        '   separate spotfile ?  ',L1,/,
     .         '   Hole diameter from file HOLE.TMP ? ',L1)
CHENN<
      IF(ISHAPE.LE.2) THEN
        READ(5,*) RAD
CHENN>
        IF(IHOLFILE)THEN
          OPEN(UNIT=11,FILE='HOLE.TMP',STATUS='OLD')
          READ(11,*) RAD
          CLOSE(11)
        ENDIF
CHENN<
        WRITE(6,1701) RAD
1701    FORMAT('   Hole radius',F10.3)
      ENDIF
      IF(ISHAPE.EQ.3) THEN
        READ(5,*) DELX,DELY
        WRITE(6,1702) DELX,DELY
1702    FORMAT('   Hole half edge lengths, DELX,DELY',2F10.3)
      ENDIF
      READ(5,*) AX,AY,BX,BY,IHMIN,IHMAX,IKMIN,IKMAX,RMAX,ITYPE
      IF(RMAX.GT.FLOAT(NX)) RMAX = FLOAT(NX)
      IF(RMAX.GT.FLOAT(NY2)) RMAX = FLOAT(NY2)
      WRITE(6,1703) AX,AY,BX,BY,IHMIN,IHMAX,IKMIN,IKMAX,RMAX,ITYPE
1703    FORMAT('   Lattice parameters',4F10.3/
     . '   Min and Max indices for lattice generator',4I7/
     . '   Maximum transform radius for lattice generator',F10.3/
     . '   ITYPE (full lattice or selected spots) =',I5)
      ASTAR = SQRT(AX**2+AY**2)
      BSTAR = SQRT(BX**2+BY**2)
      DSTAR = SQRT((AX-BX)**2+(AY-BY)**2)
      RADMAX = AMIN1(ASTAR,BSTAR,DSTAR)/2.0
      IF(RAD.GT.RADMAX) THEN
        WRITE(6,1705) RAD,RADMAX
1705    FORMAT(' Overlapped holes, radius reduced from',F6.1,' to',F6.1)
        RAD=RADMAX
      ENDIF
      WRITE(6,1704)
1704    FORMAT('   Hole positions used in masking')
      RMAXSQ=RMAX**2
      NHOLE=0
C     Set scale factor to be 1.0 since this version uses grid units as input.
      SCALE=1.00
      IF(ITYPE.EQ.0) THEN
        IHRANG=IHMAX-IHMIN+1
        IKRANG=IKMAX-IKMIN+1
        DO 120 I=1,IHRANG
          DO 120 J=1,IKRANG
            IH = IHMIN+I-1
            IK = IKMIN+J-1
            XPOS=IH*AX+IK*BX
            YPOS=IH*AY+IK*BY
            IF(XPOS.LT.0.) GO TO 120                    !   Use only positive X,
            IF(XPOS.EQ.0.0.AND.YPOS.LE.0.) GO TO 120    ! and positive Y when X=0
            IF(ISHAPE.LE.2) RSQ=(ABS(XPOS)+RAD)**2+(ABS(YPOS)+RAD)**2
            IF(ISHAPE.GE.3) RSQ=(ABS(XPOS)+DELX)**2+(ABS(YPOS)+DELY)**2
            IF(RSQ.GT.RMAXSQ) GO TO 120
            NHOLE=NHOLE+1
            IF(NHOLE.GT.NHOLEMAX) 
     .       STOP ' Too many holes for program dimension '
            IHS(NHOLE)=IH
            IKS(NHOLE)=IK
            XC(NHOLE)=XPOS
            YC(NHOLE)=YPOS
C            WRITE(6,1800) IH,IK,XC(NHOLE),YC(NHOLE)
1800        FORMAT(5X,2I6,3F14.4)
120     CONTINUE
      ENDIF
      IF(ITYPE.EQ.1) THEN
        IF(ISPOTFILE)  THEN
          CALL CCPDPN(10,'SPOTS','OLD','F',0,0)
        endif
125     continue
        IF(ISPOTFILE)  THEN
          READ(10,*,END=130) IH,IK      ! take spotdata from file SPOTS.
        ELSE
          READ(5,*,END=130) IH,IK               ! take spotdata from stream 5.
        ENDIF
        IF(IH.EQ.100) GO TO 130
        XPOS=IH*AX+IK*BX
        YPOS=IH*AY+IK*BY
        IF(XPOS.LT.0.OR.(XPOS.EQ.0.0.AND.YPOS.LT.0.)) THEN
          IH=-IH
          IK=-IK
          XPOS=-XPOS
          YPOS=-YPOS
C          WRITE(6,1803)IH,IK,XPOS,YPOS
C1803      FORMAT(' THIS HOLE X,Y COORDS MOVED TO POSITIVE X',2I5,2F10.4)
        ENDIF
C
C       TEST FOR REPEAT SPECIFICATION OF INDICES and for spot within transform.
        IF(NHOLE.EQ.0) GO TO 20131
        DO 20130 ITEST=1,NHOLE
          IF(XPOS.EQ.XC(ITEST).AND.YPOS.EQ.YC(ITEST))THEN
            WRITE(6,20132)IH,IK
20132       FORMAT(' REPEATED SPOT NOT USED',2I5)
            GO TO 125
          ENDIF
          IF(ISHAPE.LE.2) THEN
            IF(XPOS+RAD.GT.NX.OR.ABS(YPOS)+RAD.GT.NY2) THEN
              WRITE(6,20133)IH,IK
20133         FORMAT(' SPOT PLUS MASK OUTSIDE TRANSFORM NOT USED',2I5)
              GO TO 125
            ENDIF
          ELSE            ! i.e. for ISHAPE=3
            IF(XPOS+DELX.GT.NX.OR.ABS(YPOS)+DELY.GT.NY2) THEN
              WRITE(6,20133)IH,IK
              GO TO 125
            ENDIF
          ENDIF
20130   CONTINUE
20131  CONTINUE
       NHOLE=NHOLE+1
       IF(NHOLE.GT.NHOLEMAX) 
     . STOP ' Too many holes requestedfor program dimension '
       IHS(NHOLE)=IH
       IKS(NHOLE)=IK
       XC(NHOLE)=XPOS
       YC(NHOLE)=YPOS
C       WRITE(6,1800) IH,IK,XC(NHOLE),YC(NHOLE)
       GO TO 125
      ENDIF
130   WRITE(6,1801)NHOLE
1801  FORMAT('   Total number of holes = ',I7)
      IF(ISPOTFILE) CLOSE (UNIT=10)
C
C     Treat transform in strips of MDEEP, where MDEEP=IDEEP for all
c     strips except the last where MDEEP may be less than IDEEP.
      NSEC=NY/IDEEP
C
C     IF(NSEC*IDEEP.NE.NY) THEN
C       WRITE(6,1802) NSEC,IDEEP,NY
C1802   FORMAT(' NSEC,IDEEP NOT FACTORS OF NY',3I10)
C       STOP
C     ENDIF
C
      IRES=NY-(NSEC*IDEEP)
      IF(IRES.EQ.0)THEN
      ILAST=IDEEP
      ELSE
      ILAST=IRES
      NSEC=NSEC+1
      END IF
      WRITE(6,1802) NSEC,IDEEP,ILAST
1802  FORMAT(' DATA READ IN ',I5,' STRIPS, WITH ',I5,' LINES IN',
     .' EACH STRIP, EXCEPT FOR THE LAST ONE WITH',I5,' LINES'/)
100   CALL IMPOSN(1,0,0)
C
C
      DO 500 ISEC=1,NSEC
C
C
C       Read in strips of transform
        IF(ISEC.EQ.NSEC)THEN
          MDEEP=ILAST
        ELSE
          MDEEP=IDEEP
        END IF
        DO 190 M=1,MDEEP
          CALL IRDLIN(1,ARRAY(1,M))
190     CONTINUE
C
C       Check all parts of all holes to see if present in each strip.
C       Wasteful but simple ???
        DO 200 NH=1,NHOLE
C
C         Set hole centre
          XXC=XC(NH)/SCALE
          YYC=NY2+YC(NH)/SCALE
          IXC=XXC+0.5
          IYC=YYC+0.5
          IF(ISHAPE.EQ.3) GO TO 150
C
C         Set hole limits
          RA=RAD/SCALE
          RADSQ=RA*RA
          IRAD=RA+0.5
          IX1=IXC-IRAD
          IX2=IXC+IRAD
          IY1=IYC-IRAD
          IY2=IYC+IRAD
          GO TO 160
C
150       XSIDE2=DELX/SCALE
          YSIDE2=DELY/SCALE
          IXS=XSIDE2+0.5
          IYS=YSIDE2+0.5
          IX1=IXC-IXS
          IX2=IXC+IXS
          IY1=IYC-IYS
          IY2=IYC+IYS
C
C         Scan over hole
160       DO 300 IY=IY1,IY2
            YSQ=(IY-YYC)**2
C           IX can be negative
            DO 310 IX=IX1,IX2
              IF(ISHAPE.EQ.3) GO TO 290
              RSQ=(IX-XXC)**2+YSQ
              IF(RSQ.GT.RADSQ) GO TO 310
              IF(ISHAPE.EQ.2) GWT=EXP(-2.0*RSQ/RADSQ)
C             Check if point in neg X half transform - use Friedel mate
290           IF(IX.GE.0) THEN
                INDEX=IY*ISIZE+2*IX+1
              ELSE
                INDEX=(NY-IY)*ISIZE-2*IX+1
              END IF
C             Check not negative half of F(0,0) centre hole
              IF((IX.LT.0).AND.(XC(NH).EQ.0.).AND.(YC(NH).EQ.0.)) GO TO 310
C
C             Mark points within mask by scaling
              INDEX=INDEX-ISIZE*IDEEP*(ISEC-1)
              IF((INDEX.LT.1).OR.(INDEX.GT.IATOT)) GO TO 320
              ASUM(NH) = ASUM(NH) + SQRT(BRRAY(INDEX)**2+BRRAY(INDEX+1)**2)
              NSUM(NH) = NSUM(NH) + 1
              BRRAY(INDEX)=BRRAY(INDEX)*1.E10*SCALESPOT(NH)
              BRRAY(INDEX+1)=BRRAY(INDEX+1)*1.E10*SCALESPOT(NH)
              IF(ISHAPE.NE.2) GO TO 320
C             Gaussian weight for soft holes
              BRRAY(INDEX)=BRRAY(INDEX)*GWT
              BRRAY(INDEX+1)=BRRAY(INDEX+1)*GWT
C
C             On IX=0 need another segment
320           IF(IX.NE.0) GO TO 310
C             But not for centre hole
              IF((XC(NH).EQ.0.).AND.(YC(NH).EQ.0.)) GO TO 310
              INDEX=(NY-IY)*ISIZE+1
              INDEX=INDEX-ISIZE*IDEEP*(ISEC-1)
              IF((INDEX.LT.1).OR.(INDEX.GT.IATOT)) GO TO 310
              BRRAY(INDEX)=BRRAY(INDEX)*1.E10*SCALESPOT(NH)
              BRRAY(INDEX+1)=BRRAY(INDEX+1)*1.E10*SCALESPOT(NH)
              IF(ISHAPE.NE.2) GO TO 310
              BRRAY(INDEX)=BRRAY(INDEX)*GWT
              BRRAY(INDEX+1)=BRRAY(INDEX+1)*GWT
C
310         CONTINUE
C
300       CONTINUE
C
200     CONTINUE
C
C       Rescale transform and write out if no amplitude reduction or on second pass
C       with the application of amplitude reduction.
        IF(.NOT.IAMPLIMIT.OR.IPASS2) THEN
C
C         Scan transform and set everything outside mask to zero
          DO 400 I=1,NXP2
            DO 400 J=1,MDEEP
              IF(ABS(ARRAY(I,J)).LT.1.E10) THEN
                ARRAY(I,J)=0.
              ELSE
                ARRAY(I,J)=ARRAY(I,J)*1.E-10
                IF(ARRAY(I,J).GT.1.E10) GO TO 98
              END IF
400       CONTINUE
C
C         Write masked transform back to disc
          DO 495 M=1,MDEEP
            CALL IWRLIN(2,ARRAY(1,M))
495       CONTINUE
          CALL ICLCDN(ARRAY,ISIZE2,IDEEP,1,NX,1,MDEEP,SMIN,SMAX,SMEAN)
          IF(SMAX.GT.TMAX) TMAX=SMAX
          IF(SMIN.LT.TMIN) TMIN=SMIN
          TMEAN=TMEAN+SMEAN*MDEEP
        ENDIF
500   CONTINUE
C
C  Calculate statistics of spot amplitude distribution on first pass only.
      IF(IPASS2) GO TO 700
      AMPTOT=0.0
      NSPOTS=0
      DO 600 NH=1,NHOLE
      IF(NSUM(NH).NE.0) THEN
        ASUM(NH)=ASUM(NH)/NSUM(NH)
        AMPTOT=AMPTOT+ASUM(NH)
        NSPOTS=NSPOTS+1
      ENDIF
600   CONTINUE
      AVAMP=AMPTOT/NSPOTS
      FACTORMAX=1.5
C      WRITE(6,601) AVAMP
601   FORMAT(/,' MEAN AMPLITUDE INSIDE MASKS (OVERALL AVER',F12.2,')'
     .   ,/,/,'   IH   IK               AMPMEAN     NIN         ',
     .  '   AMPREDUCED ')
      DO 610 NH=1,NHOLE
        IF(IAMPLIMIT.AND.ASUM(NH).GT.FACTORMAX*AVAMP) THEN
          SCALESPOT(NH)=FACTORMAX*AVAMP/ASUM(NH)
C          WRITE(6,602)IHS(NH),IKS(NH),ASUM(NH),NSUM(NH),
C     .    SCALESPOT(NH)*ASUM(NH)
602       FORMAT(2I5,F12.2,I8,F12.2)
       ELSE
         SCALESPOT(NH)=1.0
C         WRITE(6,602)IHS(NH),IKS(NH),ASUM(NH),NSUM(NH),
C     .    SCALESPOT(NH)*ASUM(NH)
       ENDIF
610   CONTINUE
      IPASS2=.TRUE.
      IF(IAMPLIMIT)GO TO 100
C
C
700   TMEAN=TMEAN/NY
      CALL IWRHDR(2,TITLE,-1,TMIN,TMAX,TMEAN)
      WRITE(6,1900) TMIN,TMAX,TMEAN
1900  FORMAT(//,'   Overall min, max and mean masked transform 
     1 values are:   ',3G18.9)
      CALL IMCLOSE(1)
      CALL IMCLOSE(2)
      CALL EXIT
C
98    IXT=I/2
      IYT=J+(ISEC-1)*IDEEP
      IF(IYT.GT.NY2) IYT=IYT-NY
      WRITE(6,2010) ISEC,I,J,IXT,IYT
2010  FORMAT(///'   Masking failed because two holes overlap'/
     . '    probably the holes are bigger than the lattice dimension'/
     . 20X,' ISEC,I,J =',3I6/
     . 20X,' position in transform, IXT,IYT =',2I6)
      STOP
C
      END
