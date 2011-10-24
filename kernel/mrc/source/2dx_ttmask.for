C*TTMASK.FOR******************************************************************
C  Program for masking a Fourier transform, with simultaneous correction for *
C     tilted contrast transfer function.                                     *
C                                                                            *
C     Output is written on top of input, so keep a copy if you think it will *
C     need to be done more than once using different parameters.             *
C                                                                            *
C     Input and output masked transform is on stream 1 (INOUT)               *
C                                                                            *
C     Data cards for control data file in free format                        *
C                                                                            *
C  1.   ISIZEX,ISIZEY,DSTEP,XMAG,CS,KVOLT              (*)                   *
C                                                                            *
C  2.   DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL   (*)                           *
C                                                                            *
C  3.   ISHAPE=1 Hard edge circular holes      (*)                           *
C       2= Soft edge circular holes (Gaussian weighted to EXP(-2) at edge)   *
C       3= Rectangular mask                                                  *
C       4= Circular hard-edge mask, with the maximally possible radius, so   *
C          that the holes do not overlap each other.                         *
C          This is used to TTF correct the entire image, (almost) without    *
C          any masking of a diffraction spot lattice.                        *
C                                                                            *
C  4.    RADMIN,RADMAX (ISHAPE = 1 or 2) radius (circular holes)   (*)       *
C         MIN VALUE ON TILT AXIS; MAX VALUE AT 1200 rlu FROM TILT AXIS       *
C         ISHAPE = 1 (Hard edged), = 2 (Gaussian fall off to exp(-2) at edge)*
C                                                                            *
C  5.  AX,AY,BX,BY,IHMIN,IHMAX,IKMIN,IKMAX,RMAX,ITYPE,NUMSPOT  (*)           *
C       AX    etc,lattice parameters from refined NNBOX output in grid units *
C       IHMIN etc, set min/max limits on H,K for lattice generator           *
C       RMAX  Cutoff radius in transform for lattice generator in grid units *
C       ITYPE if = 0, then generate all spots                                *
C             if = 1, then include only spots of given index (see below)     *
C                                                                            *
C  6. Followed by record for each required spot to be allowed through mask   *
C         IH,IK      (*)                                                     *
C                                                                            *
C     ISIZEX,Y    size of image in x and y, checked against file-header.
C     DSTEP       densitometer stepsize in microns.
C     XMAG        magnification of micrograph.
C     CS          spherical aberration coefficient in mm.
C     KVOLT       microscope voltage in KV, used to calculate wavelength. 
C     DFMID1      defocus in one direction (underfocus +ve) 
C     DFMID2      defocus at 90-degs to above
C     ANGAST      direction for DFMID1 in degrees relative to x,y in transform.
C     TLTAXIS     direction of tiltaxis in degrees relative to x,y in transform,
C                   should be between -90 and +90 degrees.
C     TLTANGL     magnitude of tiltangle.
C                       (+ve for less underfocus at start of scan(y=0)).
C                       if tiltaxis is precisely parallel to y, then TLTANGL
C                       should be positive for less underfocus at x=0.
C                                                                            *
C     Derived from MASKTRAN(Version 1.00) and TTBOX      3-JUL-84 (RH)       *
c                                                                            *
C     Version 1.00   3-OCT-85        RH                                      *
C     Version 1.01   5-NOV-85        RH                                      *
C     Version 1.02  29-NOV-85        RH                                      *
C     Version 1.03  17-JAN-86      JMB/RH   Variable hole size               *
C     Version 1.04  17-AUG-87        RH     Bug along y-axis removed         *
C     Version 1.05  18-NOV-87        JMB    Rectangular images               *
C     Version 2.00   3-JAN-92        RH     Converted to UNIX for Alliant    *
C     Version 2.01  16-APR-02        JMS    Bug fix to call IRTORG           *
C                                                                            *
C*****************************************************************************
C
      PARAMETER (IMSIZE=20100)
      PARAMETER (IDEEP=30)
      PARAMETER (NMAX=7000)
      PARAMETER (IBOXMAX=61)
      PARAMETER (ICTFBXMAX=401)
      PARAMETER (INBOXMAX=341)
      PARAMETER (ICTFHALF=200)
C
      COMMON NX,NY,NZ
      REAL KVOLT
      DIMENSION ARRAY(IMSIZE,IDEEP),TITLE(20),NXYZR(3),MXYZR(3),
     .  IHA(NMAX),IKA(NMAX),XA(NMAX),YA(NMAX),IXC(NMAX),IYC(NMAX),
     .  RAD(NMAX)
      DIMENSION BRRAY(IMSIZE*IDEEP)
      CHARACTER DAT*24
      CHARACTER*80 TMPTITLE
      EQUIVALENCE (TMPTITLE,TITLE)
      CHARACTER*80 CZEILE,FILOU2
      EQUIVALENCE (NX,NXYZR),(ARRAY,BRRAY)
      DATA ZERO/0.0/,TWOPI/6.2831853/,PI/3.141592654/
      ISIZE2=IMSIZE/2
      IATOT=IMSIZE*IDEEP
C
      WRITE(6,1000)
1000  FORMAT(/,/,'   TTMASK: Transform masking and ctf tilt',
     .  ' correction program V2.00(3-Jan-92)',/,/)
C
      CALL IMOPEN(1,'INOUT','OLD')
      CALL FDATE(DAT)
      CALL IRDHDR(1,NXYZR,MXYZR,MODE,DMIN,DMAX,DMEAN)
      IF(NX*2.GT.IMSIZE.OR.NY.GT.IMSIZE) THEN
        WRITE(6,1501) NX*2,NY
1501    FORMAT(' PROG DIMS NOT BIG ENOUGH, 2*NX=',I6,' NY=',I6)
        STOP
      ENDIF
      NY2 = NY / 2
      NXP2 = NX * 2
      WRITE(TMPTITLE,1500) DAT(5:24)
1500  FORMAT(' TTMASK V2.00 : Transform masking and ctf tilt',
     .  ' correction ',A20)
      CALL IWRHDR(1,TITLE,1,ZERO,ZERO,ZERO)
C
C     Read in control data on unit 5. ******************************************
C
      WRITE(6,1600)
1600  FORMAT(/,/,/,' Control data as read in',/)
C
C  Input first c.t.f. and tilt data for image and caculate preliminaries.
C
      READ(5,*) ISIZEX,ISIZEY,DSTEP,XMAG,CS,KVOLT
      IF(ISIZEY.NE.NY)     GOTO 6001
      IF(ISIZEX.NE.NXP2-2) GOTO 6001
      READ(5,*) DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
98    IF(TLTAXIS.LE.-90.0) THEN
        TLTAXIS=TLTAXIS+180.0
        GOTO 98
      ENDIF
99    IF(TLTAXIS.GT.90.0) THEN
        TLTAXIS=TLTAXIS-180.0
        GOTO 99
      ENDIF
      WRITE(6,101) ISIZEX,ISIZEY,DSTEP,XMAG,CS,KVOLT
      WRITE(6,102) DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
101   FORMAT(/,' SIZE OF DENSITOMETERED ARRAY; X,Y...',2I7,/,
     .       ' DENSITOMETERED STEPSIZE(MICRONS)....',F10.2,/,
     .       ' MAGNIFICATION OF MICROGRAPH.........',F8.0,/,
     .       ' SPHERICAL ABERRATION (MM) ..........',F10.2,/,
     .       ' ACCELERATING VOLTAGE (KV) ..........',F8.0)
102   FORMAT(' UNDERFOCUS 1 .......................',F8.0,/,
     .       ' UNDERFOCUS 2 .......................',F8.0,/,
     .       ' DIRECTION FOR UNDERFOCUS 1 .........',F9.1,/,
     .       ' TILT AXIS DIRECTION ................',F9.1,/,
     .       ' TILT ANGLE .........................',F9.1,
     .       ' +VE FOR LESS UNDERFOCUS AT START OF SCAN(Y=0)',/)
      ANGAST=ANGAST*TWOPI/360.0
      TLTAXIS=TLTAXIS*TWOPI/360.0
      TLTANGL=TLTANGL*TWOPI/360.0
C
      COSTLTX=COS(TLTAXIS)
      SINTLTX=SIN(TLTAXIS)
      TANTILT=TAN(TLTANGL)
C
      CS=CS*(10.0**7.0)
      KVOLT=KVOLT*1000.0
      WL=12.3/SQRT(KVOLT+KVOLT**2/(10.0**6.0))
      WRITE(6,103)WL
103   FORMAT(' WAVELENGTH (ANGSTROMS)',F10.4)
      STEPR=DSTEP*(10.0**4.0)/XMAG
      THETATR=WL/(STEPR*ISIZEX)
C  THETATR IS DIFFRACTION ANGLE OF FIRST GRID POINT IN X DIRECTION
C   OF TRANSFORM.
C
C       Calculate height difference across image
C
      PERP=ISIZEY*COSTLTX + ISIZEX*(ABS(SINTLTX))
C      WRITE(6,91701)STEPR,TANTILT,COSTLTX,SINTLTX,ISIZEX,ISIZEY
91701 FORMAT(4F10.5,2I10)
      DELHEIGHT=ABS(STEPR*PERP*TANTILT)
      WRITE(6,91700)PERP,DELHEIGHT
91700 FORMAT(' PERPENDICULAR DISTANCE FROM CORNER OF FILM TO TILT',
     .  ' AXIS',F10.3,/,' DIFFERENCE IN HEIGHT OF FAR CORNERS OF IMAGE',
     .  F10.3,/)
C
C  Now input of mask parameters
      READ(5,*) ISHAPE
      WRITE(6,1700) ISHAPE
1700  FORMAT(/,'   Hole shape',I10)
      READ(5,*) RADMIN,RADMAX
      NHOR=2*INT(RADMAX+0.9999)+1
      NVERT=NHOR
      WRITE(6,1701) RADMIN,RADMAX,NHOR,NVERT
1701  FORMAT(/,' Hole radii used for masking; min on tilt axis',F10.3,/,
     . '       max at distance 1200rlu from tilt axis ',F10.3,/,
     . ' NHOR, NVERT for ctf convolution (from radmax) ',2I5,/)
      PERPMAX=1200
      FCTR=(RADMAX-RADMIN)/PERPMAX
      READ(5,*)AX,AY,BX,BY,IHMIN,IHMAX,IKMIN,IKMAX,RMAX,ITYPE,NUMSPOT
      WRITE(6,1703)AX,AY,BX,BY,
     .  IHMIN,IHMAX,IKMIN,IKMAX,RMAX,ITYPE,NUMSPOT
1703  FORMAT('   Lattice parameters',4F10.3,/,
     .  '   Min and Max indices for lattice generator',4I7,/,
     .  '   Maximum transform radius for lattice generator',F10.3,/,
     .  '   ITYPE (full lattice or selected spots) =',I5,/,
     .  '   NUMSPOT - Number for detailed printout =',I5)
C
CHEN
      if(ISHAPE.eq.4)then
        rmaxa=sqrt(ax*ax+ay*ay)/2.0
        rmaxb=sqrt(bx*bx+by*by)/2.0
        RADMIN=rmaxa
        if(RADMIN.gt.rmaxb)RADMIN=rmaxb
        RADMIN=RADMIN-1.0
        RADMAX=RADMIN
        WRITE(6,'('' RADMIN,RADMAX corrected to '',2F9.2)')
     .  RADMIN,RADMAX
      endif
      READ(5,'(A)') FILOU2
CHEN
C
C  Check to make sure holes do not overlap in the convolution step which
C       would be a real disaster.
      IF(SQRT(AX**2+AY**2).LT.FLOAT(NHOR)
     .          .OR.SQRT(BX**2+BY**2).LT.FLOAT(NHOR)) THEN
        WRITE(6,1705)
1705    FORMAT(' Mask hole size is too big for lattice spot spacing')
        STOP
      ENDIF
      WRITE(6,1704)
1704  FORMAT('   Hole positions used in masking')
      RMAXSQ=RMAX**2
      NHOLE=0
C
      SIZEX=ISIZEX
      RATIOXY=SIZEX/ISIZEY
      ACY=AY*RATIOXY
      BCY=BY*RATIOXY
C
      IF(ITYPE.EQ.0) THEN
        IHRANG=IHMAX-IHMIN+1
        IKRANG=IKMAX-IKMIN+1
        DO 120 I=1,IHRANG
          DO 120 J=1,IKRANG
            IH = IHMIN+I-1
            IK = IKMIN+J-1
            IF(IH.EQ.0.AND.IK.EQ.0)GOTO 120
            XPOS=IH*AX+IK*BX
            YPOS=IH*AY+IK*BY
            YCPOS=IH*ACY+IK*BCY
            IF(XPOS.LT.0.) GOTO 120
            RSQ=XPOS**2 + YPOS**2
            IF(RSQ.GT.RMAXSQ) GOTO 120
            NHOLE=NHOLE+1
            IF(NHOLE.GT.NMAX)GOTO 4550
            XA(NHOLE)=XPOS
            YA(NHOLE)=YPOS
            IHA(NHOLE)=IH
            IKA(NHOLE)=IK
C
            CALL CALCRAD(RADCLC,RADMIN,FCTR,PERPMAX,XPOS,YPOS,YCPOS,
     .      COSTLTX,SINTLTX)
C
            RAD(NHOLE)=RADCLC
            WRITE(6,1800) IH,IK,XA(NHOLE),YA(NHOLE),RAD(NHOLE)
1800        FORMAT(5X,2I5,3F10.4,F10.2)
120     CONTINUE
      ENDIF
C
      IF(ITYPE.EQ.1) THEN
125     READ(5,*,END=130) IH,IK
        IF(IH.EQ.100) GOTO 130
        XPOS=IH*AX+IK*BX
        YPOS=IH*AY+IK*BY
        YCPOS=IH*ACY+IK*BCY
        IF(XPOS.LT.0.) THEN
          IH=-IH
          IK=-IK
          XPOS=-XPOS
          YPOS=-YPOS
          YCPOS=-YCPOS
C         WRITE(6,1803)IH,IK,XPOS,YPOS
1803      FORMAT(' THIS HOLE X,Y COORDS MOVED TO POSITIVE X',2I5,2F10.4)
        ENDIF
C
C-------TEST FOR REPEAT SPECIFICATION OF INDICES
        IF(NHOLE.EQ.0)GOTO 20131
          DO 20130 ITEST=1,NHOLE
            IF(XPOS.EQ.XA(ITEST).AND.YPOS.EQ.YA(ITEST))THEN
C             WRITE(6,20132)IH,IK
20132         FORMAT(' REPEATED SPOT NOT USED',2I5)
            GOTO 125
            ENDIF
20130     CONTINUE
20131   CONTINUE
        NHOLE=NHOLE+1
        IF(NHOLE.GT.NMAX)GOTO 4550
        XA(NHOLE)=XPOS
        YA(NHOLE)=YPOS
        IHA(NHOLE)=IH
        IKA(NHOLE)=IK
C
        CALL CALCRAD(RADCLC,RADMIN,FCTR,PERPMAX,XPOS,YPOS,YCPOS,
     .      COSTLTX,SINTLTX)
C
        RAD(NHOLE)=RADCLC
        WRITE(6,1800) IH,IK,XA(NHOLE),YA(NHOLE),RAD(NHOLE)
        GOTO 125
      ENDIF
C
130   WRITE(6,1801)NHOLE
1801  FORMAT('   Total number of holes = ',I7)
C
C   Input of control data now complete *****************************************
C
      DO 10 I=1,NHOLE
        IXC(I)=XA(I)+SIGN(0.5,XA(I))
        IYC(I)=YA(I)+SIGN(0.5,YA(I))
 10   continue
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
C-----Now call subroutine to correct for tilt and ctf.
C
      CALL TTCORRECT(IHA,IKA,IXC,IYC,XA,YA,RATIOXY,NHOR,NVERT,NHOLE,
     .     THETATR,
     .     DFMID1,DFMID2,ANGAST,CS,WL,STEPR,ISIZEX,ISIZEY,DELHEIGHT,
     .     TLTAXIS,TLTANGL,TANTILT,COSTLTX,SINTLTX,NUMSPOT,FILOU2)
C
C-----Treat transform in strips of MDEEP, where MDEEP=IDEEP for all
C-----strips except the last where MDEEP may be less than IDEEP.
C
      NSEC=NY/IDEEP
      IRES=NY-(NSEC*IDEEP)
      IF(IRES.EQ.0)THEN
        ILAST=IDEEP
      ELSE
        ILAST=IRES
        NSEC=NSEC+1
      END IF
      WRITE(6,1802) NSEC,IDEEP,ILAST
1802  FORMAT(' DATA READ IN ',I5,' STRIPS, WITH ',I5,' LINES IN',
     .  ' EACH STRIP, EXCEPT FOR THE LAST ONE WITH',I5,' LINES',/)
C
      CALL IMPOSN(1,0,0)
C
      DO 500 ISEC=1,NSEC
C-------Read in strip of transform
        IF(ISEC.EQ.NSEC)THEN
          MDEEP=ILAST
        ELSE
          MDEEP=IDEEP
        END IF
        DO 190 M=1,MDEEP
          CALL IRDLIN(1,ARRAY(1,M))
190     CONTINUE
        IPOSIT=(ISEC-1)*IDEEP           ! Reposition ready for output again.
        CALL IMPOSN(1,0,IPOSIT)
C
C-------Check all parts of all holes to see if present in each strip.
C-------Wasteful but simple ???
C
        DO 200 NH=1,NHOLE
C
C---------Set hole centre
C
          XXC=XA(NH)
          YYC=NY2+YA(NH)
          JXC=XXC+0.5
          JYC=YYC+0.5
C
C---------Set hole limits
C
          RADSQ = RAD(NH)*RAD(NH)
          IRAD = RAD(NH)+0.5
          IX1=JXC-IRAD
          IX2=JXC+IRAD
          IY1=JYC-IRAD
          IY2=JYC+IRAD
          GOTO 160
C
C---------Scan over hole
C
160       DO 300 IY=IY1,IY2
            YSQ=(IY-YYC)**2
C-----------IX can be negative
            DO 310 IX=IX1,IX2
              RSQ=(IX-XXC)**2+YSQ
              IF(RSQ.GT.RADSQ) GOTO 310
              IF(ISHAPE.EQ.2) GWT=EXP(-2.0*RSQ/RADSQ)
C
C-------------Check if point in neg X half transform - use Friedel mate
290           IF(IX.GE.0) THEN
                INDEX=IY*IMSIZE+2*IX+1
              ELSE
                INDEX=(NY-IY)*IMSIZE-2*IX+1
              END IF
C
C-------------Check not negative half of F(0,0) centre hole
              IF((IX.LT.0).AND.(XA(NH).EQ.0.)
     1           .AND.(YA(NH).EQ.0.))GOTO 310
C
C-------------Mark points within mask by scaling
              INDEX=INDEX-IMSIZE*IDEEP*(ISEC-1)
              IF((INDEX.LT.1).OR.(INDEX.GT.IATOT)) GOTO 320
              BRRAY(INDEX)=BRRAY(INDEX)*1.E10
              BRRAY(INDEX+1)=BRRAY(INDEX+1)*1.E10
              IF(ISHAPE.NE.2) GOTO 320
C
C-------------Gaussian weight for soft holes
              BRRAY(INDEX)=BRRAY(INDEX)*GWT
              BRRAY(INDEX+1)=BRRAY(INDEX+1)*GWT
C
C-------------On IX=0 need another segment
320           IF(IX.NE.0) GOTO 310
C-------------But not for centre hole
              IF((XA(NH).EQ.0.).AND.(YA(NH).EQ.0.)) GOTO 310
              INDEX=(NY-IY)*IMSIZE+1
              INDEX=INDEX-IMSIZE*IDEEP*(ISEC-1)
              IF((INDEX.LT.1).OR.(INDEX.GT.IATOT)) GOTO 310
              BRRAY(INDEX)=BRRAY(INDEX)*1.E10
              BRRAY(INDEX+1)=BRRAY(INDEX+1)*1.E10
              IF(ISHAPE.NE.2) GOTO 310
              BRRAY(INDEX)=BRRAY(INDEX)*GWT
              BRRAY(INDEX+1)=BRRAY(INDEX+1)*GWT
C
310         CONTINUE
C
300       CONTINUE
C
200     CONTINUE
C
C-------Scan transform and set everything outside mask to zero
        DO 400 I=1,NXP2
          DO 400 J=1,MDEEP
            IF(ABS(ARRAY(I,J)).LT.1.E5) THEN
              ARRAY(I,J)=0.
            ELSE
            ARRAY(I,J)=ARRAY(I,J)*1.E-10
            IF(ARRAY(I,J).GT.1.E10) GOTO 2009
          END IF
400     CONTINUE
C
C-------Write masked transform back to disc
        DO 495 M=1,MDEEP
          CALL IWRLIN(1,ARRAY(1,M))
495     CONTINUE
        CALL ICLCDN(ARRAY,ISIZE2,IDEEP,1,NX,1,MDEEP,SMIN,SMAX,SMEAN)
        IF(SMAX.GT.TMAX) TMAX=SMAX
        IF(SMIN.LT.TMIN) TMIN=SMIN
        TMEAN=TMEAN+SMEAN*MDEEP
500   CONTINUE
C
      TMEAN=TMEAN/NY
      CALL IWRHDR(1,TITLE,-1,TMIN,TMAX,TMEAN)
      WRITE(6,1900) TMIN,TMAX,TMEAN
1900  FORMAT(/,/,'   Overall min, max and mean masked transform 
     1 values are:   ',3G13.5)
      CALL IMCLOSE(1)
      CALL EXIT
C
2009  WRITE(6,2010)
2010  FORMAT(/,/,/,'   Masking failed because two holes overlap')
      STOP
6001    WRITE(6,6002)ISIZEX,ISIZEY,NX,NY
6002    FORMAT(' Image is not of size ISIZEX by ISIZEY',4I8)
      STOP 
4550    WRITE(6,4551)NMAX
4551    FORMAT(' Too many spots for current prog dimensions',I5)
      STOP
      END
C*******************************************************************************
C
C
      SUBROUTINE TTCORRECT(IH,IK,IXC,IYC,XA,YA,RATIOXY,NHOR,NVERT,NSPOT,
     .  THETATR,
     .  DFMID1,DFMID2,ANGAST,CS,WL,STEPR,ISIZEX,ISIZEY,DELHEIGHT,
     .  TLTAXIS,TLTANGL,TANTILT,COSTLTX,SINTLTX,NUMSPOT,FILOU2)
C
C TTCORRECT : calculates amplitudes & phases in N * N boxes from a 
C         Fourier transform, fully corrected for contrast transfer function
C         in tilted image and writes the answer back to disc.
C         Also gives extensive output if required.
C
      PARAMETER (IMSIZE=20100)
      PARAMETER (IDEEP=30)
      PARAMETER (NMAX=7000)
      PARAMETER (IBOXMAX=61)
      PARAMETER (ICTFBXMAX=401)
      PARAMETER (INBOXMAX=341)
      PARAMETER (ICTFHALF=200)
c      REAL KVOLT
      DIMENSION XA(NMAX),YA(NMAX),IXC(NMAX),IYC(NMAX),IH(NMAX),IK(NMAX)
      DIMENSION IAMP(IBOXMAX,IBOXMAX),IPHI(IBOXMAX,IBOXMAX),
     .  IXGU(IBOXMAX),IYGU(IBOXMAX),ISUM(IBOXMAX,IBOXMAX),
     .  ISUMI(IBOXMAX,IBOXMAX),RSUMI(IBOXMAX,IBOXMAX),
     .  ACORR(IBOXMAX,IBOXMAX),BCORR(IBOXMAX,IBOXMAX)
      DIMENSION AP(INBOXMAX,INBOXMAX),BP(INBOXMAX,INBOXMAX)
      DIMENSION ACTF(ICTFBXMAX,ICTFBXMAX),BCTF(ICTFBXMAX,ICTFBXMAX)
c      DIMENSION TITLE(15),NXYZ(3),MXYZ(3),PHANG(4),WTS(4),
c     .         DELX(2),DELY(2),NIQ(8)
      DIMENSION NXYZ(3),PHANG(4),WTS(4),
     .   DELX(2),DELY(2),NIQ(8)
      LOGICAL TURN,ILIST,LIST
      CHARACTER*80 FILOU2
      INTEGER*4 IPERIM
      COMMON NX,NY,NZ
      EQUIVALENCE (NXYZ,NX)
      DATA NIQ/8*0/,TWOPI/6.2831853/,PI/3.141592654/
      DATA ILIST/.TRUE./,LIST/.FALSE./,TURN/.FALSE./
C
      WRITE(6,1000)
 1000 FORMAT('0',' TTCORRECT : calculates NxN boxes of amps & phases', 
     1       ' from normal Fourier transform, corrected for c.t.f.')
C
      IF(NHOR.GT.IBOXMAX.OR.NVERT.GT.IBOXMAX) THEN
        WRITE(6,1301) NHOR,NVERT,IBOXMAX
1301    FORMAT(' Subroutine TTCORRECT dim too small, NHOR or',
     .  ' NVERT .GT. IBOXMAX',3I5)
        STOP
      ENDIF
C
      CALL IRTORG(1,XOR,YOR,ZOR)
      WRITE(6,1010) XOR,YOR,ZOR
 1010 FORMAT(/,' X & Y phase origin shift read from transform',
     1           3F10.2)
C
      NUMOUT = 0
      NGOOD=0
      NBAD=0
      NPHI = 4
      NY2 = NY / 2
      NY2M1 = NY2 - 1
      NY2M2 = NY2 - 2
      NXP2 = NX * 2
      NXM1 = NX - 1
      NXM2 = NX - 2
C
      XORIG=ISIZEX/2            ! PUTS ORIGIN IN MIDDLE OF IMAGE AREA
      YORIG=ISIZEY/2
      DELPX = -TWOPI * (XOR + XORIG) / (2. * (NXM1))
      DELPY = -TWOPI * (YOR + YORIG) / NY
      WRITE(6,1300)(XOR+XORIG)/(2.*NXM1),(YOR+YORIG)/NY
1300  FORMAT(' FRACT POSITION OF PHASE ORIGIN',2F10.5)
C
  120 IHOR2 = NHOR / 2
      IVERT2 = NVERT / 2
C
C     make sure odd number of elements in box
C
      NHOR = IHOR2 * 2 + 1
      NVERT = IVERT2 * 2 + 1
      WRITE(6,1080) NHOR,NVERT
 1080 FORMAT(/,' Box size in transform grid units :',I3,' *',I3)
      DO 130 K=1,IBOXMAX
        DO 130 J=1,IBOXMAX
          ISUM(J,K) = 0
          ISUMI(J,K) = 0
          RSUMI(J,K) = 0.0
  130 CONTINUE
      WRITE(6,1103)             !  Asterisks to mark beginning of output.
C
C  Beginning of Do-loop over all required spots.
C
      DO 500 I=1,NSPOT
        IHOR = NHOR
        IVERT = NVERT
C
C  IHOR,IVERT is required output box size.
C  ICTFHOR,ICTFVERT is necessary CTF box size for convolution -- this will
C   get bigger with increasing resolution, becoming its maximum size for big,
C   highly tilted images at high resolution. For example, a 5000x5000 area
C   which is tilted to 60-degrees will require ICTFHOR,ICTFVERT = 100 , if
C   a resolution of 3 Angstroms is desired.  Both of these parameters are 
C   calculated inside subroutine CTFGEN.
C  INHOR,INVERT is then the necessary input box size from the transform needed
C   to carry out the convolution multiplication successfully.
c   It is always bigger than either ctf box or the output box.
C               INHOR  = IHOR  + ICTFHOR  (- 1)
C               INVERT = IVERT + ICTFVERT (- 1)
C
        CALL CTFGEN(IH(I),IK(I),XA(I),YA(I),RATIOXY,
     .      THETATR,DFMID1,DFMID2,ANGAST,
     .      CS,WL,STEPR,ISIZEX,ISIZEY,DELHEIGHT,
     .      TLTAXIS,TLTANGL,TANTILT,COSTLTX,SINTLTX,
     .      ICTFHOR,ICTFVERT,ACTF,BCTF,
     .      ILIST,DFMID,DELCHI,CTFMID,FACTOR,ISENS)
C
        INHOR  = IHOR  + ICTFHOR        ! ODD = ODD +EVEN
        INVERT = IVERT + ICTFVERT       ! ODD = ODD +EVEN
        INHOR2 = INHOR / 2
        INVERT2= INVERT/ 2
C
        IXL = IXC(I) - INHOR2 
        IXR = IXC(I) + INHOR2 
        IYL = IYC(I) - INVERT2 
        IYU = IYC(I) + INVERT2 
C
C       check edge spots
C
        IF(IXL.LT.-NXM1) GOTO 160
        IF(IXR.GT.NXM1) GOTO 160
        IF(IYL.LT.-NY2M1) GOTO 160
        IF(IYU.GT.NY2M1) GOTO 160
        GOTO 180
160       WRITE(6,161) IH(I),IK(I)
161       FORMAT(2I4,' SPOT TOO NEAR EDGE FOR CTF TILT CORRECTION')
          GOTO 500
180     CONTINUE
C
C       set up box edge coordinates
C
C       simple case +ve quadrant in X.
C
        IXOUT1 = 1
        IXOUT2 = INHOR
        IF(IXL.GE.0) THEN
          IX1 = IXL 
          IX2 = IX1 + INHOR - 1
          IY1 = NY2 + IYL 
          IY2 = IY1 + INVERT - 1
          IX = IXL - 1
          IY = IYL - 1
          CALL RDSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT,
     .       IX,IY,AP,BP,DELPX,DELPY,TURN)
          GOTO 280
C
C         simple case -ve quadrant in X.
C
        ELSE IF(IXR.LT.0) THEN
          IX1 = -IXR  
          IX2 = IX1 + INHOR - 1
          IY1 = NY2 - IYU 
          IY2 = IY1 + INVERT - 1
          IX = IXR + 1
          IY = IYU + 1
          TURN = .TRUE.
          CALL RDSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT,
     .        IX,IY,AP,BP,DELPX,DELPY,TURN)
          GOTO 280
        END IF
C
C       complicated cases : spots split about Y axis
C
C       set up LHS of box
C
        IX1 = 1
        IX2 = -IXL 
        IY1 = NY2 - IYU 
        IY2 = IY1 + INVERT - 1
        IXOUT2 = -IXL
        IX = 0
        IY = IYU + 1
        TURN = .TRUE.
        CALL RDSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT,
     .      IX,IY,AP,BP,DELPX,DELPY,TURN)
C
C       set up RHS of box
C
        IXOUT1 = IXOUT2 + 1
        IXOUT2 = INHOR
        IX1 = 0
        IX2 = IXR  
        IY1 = NY2 + IYL 
        IY2 = IY1 + INVERT - 1
        IX = -1
        IY = IYL - 1
        CALL RDSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT,
     .      IX,IY,AP,BP,DELPX,DELPY,TURN)
C
280     CONTINUE
C
C       set up array for X and Y axis description coordinates.
C
        KXL = IXL + ICTFHOR/2
        DO 305 J=1,IHOR
          IXGU(J) = KXL
          KXL = KXL + 1
  305   CONTINUE
        KYL = IYL + ICTFVERT/2
        DO 306 J=1,IVERT
          IYGU(J) = KYL
          KYL = KYL + 1
  306   CONTINUE
C
C new convolution routine using scsl
C This is currently deactivated, until further debugging for FFTW usage.
C        CALL SCSL_CONVOLUTE(AP,BP,ACTF,BCTF,IAMP,IPHI,IHOR,IVERT,
C     .       ICTFHOR,ICTFVERT,ACORR,BCORR)
C
C
        CALL CONVOLUTE(AP,BP,ACTF,BCTF,IAMP,IPHI,IHOR,IVERT,
     .       ICTFHOR,ICTFVERT,ACORR,BCORR)
C
C       First redo IXL,IXR,IYL,IYR for the output box.
        IXL = IXC(I) - IHOR2 
        IXR = IXC(I) + IHOR2 
        IYL = IYC(I) - IVERT2 
        IYU = IYC(I) + IVERT2 
C       set up box edge coordinates for output
C
C       simple case +ve X-quadrant for output
C
        IXOUT1 = 1
        IXOUT2 = IHOR
        IF(IXL.GE.0) THEN
          IX1 = IXL 
          IX2 = IX1 + IHOR - 1
          IY1 = NY2 + IYL 
          IY2 = IY1 + IVERT - 1
          IX = IXL - 1
          IY = IYL - 1
          CALL WRTSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,IVERT,
     .       IX,IY,ACORR,BCORR,DELPX,DELPY,TURN)
          GOTO 1280
C
C         simple case -ve X-quadrant for output
C
        ELSE IF(IXR.LT.0) THEN
          IX1 = -IXR  
          IX2 = IX1 + IHOR - 1
          IY1 = NY2 - IYU 
          IY2 = IY1 + IVERT - 1
          IX = IXR + 1
          IY = IYU + 1
          TURN = .TRUE.
          CALL WRTSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,IVERT,
     .      IX,IY,ACORR,BCORR,DELPX,DELPY,TURN)
          GOTO 1280
        END IF
C
C       complicated cases : spots split about Y axis -- for output
C
C       set up LHS of box for output (IMPORTANT NOTE : these are very slightly
C       different from the input (RDSECT) parameters, to allow for the output
C       of the strip X=0 along the Y-axis --- this involves a change by 1 in the
C       values of IX1, IXOUT2 and IX.
C
        IX1 = 0                 ! was 1
        IX2 = -IXL 
        IY1 = NY2 - IYU 
        IY2 = IY1 + IVERT - 1
        IXOUT2 = -IXL+1         ! was -IXL
        IX = 1                  ! was 0
        IY = IYU + 1
        TURN = .TRUE.
        CALL WRTSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,IVERT,
     .      IX,IY,ACORR,BCORR,DELPX,DELPY,TURN)
C
C       set up RHS of box for output
C
        IXOUT1 = IXOUT2 + 1
        IXOUT2 = IHOR
        IX1 = 0
        IX2 = IXR  
        IY1 = NY2 + IYL 
        IY2 = IY1 + IVERT - 1
        IX = -1
        IY = IYL - 1
        CALL WRTSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,IVERT,
     .      IX,IY,ACORR,BCORR,DELPX,DELPY,TURN)
C
1280    CONTINUE
C
C       From here onwards the program produces cosmetic output only which is left
C       over from TTBOX.
C
C       Arrays IAMP, IPHI now filled with correct numbers.
C       Now calculate RMS background -- here onwards as in MMBOX.
C       Already redone IXL,IXR,IYL,IYR for the output box.
C
        AMPSQ = 0.
        KV = IVERT - 1
        KH = IHOR - 1
        DO 290 K=1,IVERT,KV
          DO 290 J=1,IHOR
            F = IAMP(J,K)
            AMPSQ = AMPSQ + F * F
  290   CONTINUE
        DO 300 K=2,KV
          DO 300 J=1,IHOR,KH
            F = IAMP(J,K)
            AMPSQ = AMPSQ + F * F
  300   CONTINUE
        AMPTOT = AMPSQ / (2*(IHOR + IVERT - 2))
        RMSBK = SQRT(AMPTOT)
C
C       Calculate integrated amplitude
C
        J1 = IHOR / 2
        J2 = J1 + 2
        K1 = IVERT / 2
        K2 = K1 + 2
        AMPSQ = 0.
        DO 310 K=K1,K2
          DO 310 J=J1,J2
            F = IAMP(J,K)
            AMPSQ = AMPSQ + F * F
  310   CONTINUE
        AMPTOT = AMPTOT * 9.
        IF(AMPSQ.GE.AMPTOT) THEN
          AMPINT = SQRT(AMPSQ - AMPTOT)
        ELSE
          AMPINT = 0.
        END IF
C
C       Calculate phase from vector sum of phase
C       First find requested centre of output array
C
        XSCALE = XA(I)
        INTXA = XSCALE
        IF(XSCALE.LT.0.) INTXA = INTXA - 1
        J1 = INTXA - IXL + 1
        DELX(1) = XSCALE - INTXA
        DELX(2) = 1. - DELX(1)
        YSCALE = YA(I)
        INTYA = YSCALE
        IF(YSCALE.LT.0.) INTYA = INTYA - 1
        DELY(1) = YSCALE - INTYA
        DELY(2) = 1. - DELY(1)
        K1 = INTYA - IYL + 1
        J2 = J1 + 1
        K2 = K1 + 1
        ASUM1 = 0.
        BSUM1 = 0.
        ASUM2 = 0.
        BSUM2 = 0.
        DENOM=0.
        DO 320 L2 = 1,2 ! Vector phases over 2x2 points only.
          K = K1 + L2 - 1
          DO 320 L1 = 1,2       !
            J = J1 + L1 - 1
            AMP = IAMP(J,K)
            PHASE = IPHI(J,K) / 57.2958
            ASUM1 = ASUM1 + AMP * COS(PHASE)
            BSUM1 = BSUM1 + AMP * SIN(PHASE)
C
C           calculated sinc function weighted phase
C
            IF(DELX(L1).EQ.0) GOTO 315
            IF(DELY(L2).EQ.0) THEN
              SINC = (SIN(PI * DELX(L1))) / (PI * DELX(L1))
            ELSE
              SINC = (SIN(PI * DELX(L1)) * SIN(PI * DELY(L2))) /
     .                          (PI**2 * DELX(L1) * DELY(L2))
            END IF
            GOTO 318
C
  315       IF(DELY(L2).EQ.0) THEN
              SINC = 1.
            ELSE
              SINC = (SIN(PI * DELY(L2))) / (PI * DELY(L2))
            END IF
C
  318       ASUM2 = ASUM2 + AMP * COS(PHASE) * SINC
            BSUM2 = BSUM2 + AMP * SIN(PHASE) * SINC
            DENOM = DENOM + SINC**2
  320   CONTINUE
C
        IF(ASUM1.NE.0..OR.BSUM1.NE.0.) THEN 
          VECPHA1 = ATAN2(BSUM1,ASUM1) * 57.2958
        ELSE
          VECPHA1 = 0.
        END IF
        IF(VECPHA1.LT.0.) VECPHA1 = VECPHA1 + 360.
C
        IF(ASUM2.NE.0..OR.BSUM2.NE.0.) THEN 
          VECPHA2 = ATAN2(BSUM2,ASUM2) * 57.2958
          AMPSINC = SQRT(ASUM2**2 + BSUM2**2)/DENOM
          IF(AMPSINC.LE.RMSBK) THEN
            AMPOUT = 0.00001
          ELSE
            AMPOUT = SQRT(AMPSINC**2 - RMSBK**2)
          ENDIF
        ELSE
          VECPHA2 = 0.
          AMPSINC = 0.
          AMPOUT = 0.00001
        END IF
        IF(VECPHA2.LT.0.) VECPHA2 = VECPHA2 + 360.
        PHSOUT=VECPHA2
        PHSERR = (180.0/PI)*RMSBK/AMPOUT
        IQ = 1 + (PHSERR/7.0)           ! THIS MEANS IQ=1 HAS AMP= 8x RMSBK
        IQ = MIN(IQ,8)                  !            IQ=7     AMP= 1x RMSBK
C
C       sum squared amplitudes
C
        DO 350 K=1,IVERT
          DO 350 J=1,IHOR
          ISUM(J,K) = ISUM(J,K) + IAMP(J,K) * IAMP(J,K)
  350   CONTINUE
C
C       calculate phase & amplitude @ requested point by linear
C       interpolation
C       calculate weights for interpolated phase angle
C
        PHANG(1) = IPHI(J1,K1)
        PHANG(2) = IPHI(J2,K1)
        PHANG(3) = IPHI(J1,K2)
        PHANG(4) = IPHI(J2,K2)
        WTS(1) = DELX(2) * DELY(2) * IAMP(J1,K1)
        WTS(2) = DELX(1) * DELY(2) * IAMP(J2,K1)
        WTS(3) = DELX(2) * DELY(1) * IAMP(J1,K2)
        WTS(4) = DELX(1) * DELY(1) * IAMP(J2,K2)
C
        CALL ANGAVE(NPHI,PHANG,WTS,PINTP,GDMEAN) 
C
        IF(IQ.LE.7) NGOOD=NGOOD+1
        IF(IQ.GT.7) NBAD=NBAD+1
        NIQ(IQ) = NIQ(IQ)+1
C
C       set up pagination
C
        NUMOUT = NUMOUT + 1
        IF(NUMOUT.GE.NUMSPOT)   ILIST=.FALSE.
        IF(NUMOUT.EQ.NUMSPOT+1) THEN
          WRITE(6,1103)
          WRITE(6,1102)
        ENDIF
1102    FORMAT(/,' OTHER SPOTS NOT PRINTED OUT WITH FULL DIAGNOSTICS')
1108    FORMAT('   H   K  AMPOUT  PHSOUT IQ   RMSBK     DFMID    ',
     .    'NCTFSAMPLES    CTFINMIDDLE   RESCALING BY')
        IF(NUMOUT.GT.NUMSPOT) THEN
          NUMAFTER=NUMOUT-NUMSPOT-1     ! Test for table heading output.
          IF(60*((NUMAFTER)/60).EQ.NUMAFTER) WRITE(6,1108)
          WRITE(6,1101)IH(I),IK(I),ISENS,AMPOUT,PHSOUT,IQ,RMSBK,
     .    DFMID,DELCHI,ICTFHOR,CTFMID,FACTOR
        ENDIF
1101    FORMAT(2I4,A1,F7.1,F8.1,I3,F8.1,F10.1,F12.2,
     .    '(',I3,')',F11.4,F12.3)
        IF(NUMOUT.GT.NUMSPOT) GOTO 500
C
C       write up to NUMSPOT spots
C
        WRITE(6,1105) IH(I),IK(I),XA(I),YA(I)
1103    FORMAT(/,/,132('*'),/)
1105    FORMAT(/,' Reflection  H',I3,'  K',I3,10X,
     1       'Lattice coordinates in grid units ',2F8.2)
C
        WRITE(6,1110) RMSBK, AMPINT, VECPHA1, VECPHA2, PINTP, GDMEAN
1110    FORMAT(/,' RMS backgd =',F6.1,
     .   ' Integrated bgd-corr amp over 3x3 box =',F6.1,/,
     .   '  Ampl-weighted vec. sum of phase =',F6.1, 
     .   '  sinc func-weighted vec. sum of phase =',F6.1, 
     .   ' Interp. phase/goodness=',2F6.1) 
        WRITE(6,1121) AMPOUT,PHSOUT,IQ
1121    FORMAT(' Amplitude, phase and IQ to be output =',2F8.1,I3)
        IF(IHOR.GT.10) GOTO 400
C  
C       amps & phases side by side
C
        WRITE(6,1120)
 1120   FORMAT(/,/,19X,'Amplitudes',65X,'Phases')
        WRITE(6,1140) (IXGU(J),J=1,IHOR)
 1140   FORMAT(/,'  X(grid units)',21I5,5(/,15X,21I5))
        WRITE(6,1150) (IXGU(J),J=1,IHOR)
 1150   FORMAT('+',67X,'X(grid units)',10I5)
        WRITE(6,1160)
 1160   FORMAT(/,'  Y(grid units)',53X,'Y(grid units)')
        L = IVERT
        DO 390 K=1,IVERT
          WRITE(6,1170) IYGU(L),(IAMP(J,L),J=1,IHOR)
 1170     FORMAT(/,6X,I5,4X,21I5,5(/,15X,21I5))
          WRITE(6,1180) IYGU(L),(IPHI(J,L),J=1,IHOR)
 1180     FORMAT('+',72X,I5,4X,10I5)
          L = L - 1
  390   CONTINUE
        IF(ILIST)WRITE(6,1103)  ! Asterisks.
        GOTO 500
C
C       write amps first then phases
C
  400   WRITE(6,1200)
 1200   FORMAT(/,/,19X,'Amplitudes')
        WRITE(6,1140) (IXGU(J),J=1,IHOR)
        WRITE(6,1210)
 1210   FORMAT(/,'  Y(grid units)')
        L = IVERT
        DO 420 K=1,IVERT
          WRITE(6,1170) IYGU(L),(IAMP(J,L),J=1,IHOR)
          L = L - 1
  420   CONTINUE
        WRITE(6,1230)
 1230   FORMAT(/,19X,'Phases')
        WRITE(6,1140) (IXGU(J),J=1,IHOR)
        WRITE(6,1210)
        L = IVERT
        DO 440 K=1,IVERT
          WRITE(6,1170) IYGU(L),(IPHI(J,L),J=1,IHOR)
          L = L - 1
  440   CONTINUE
        IF(ILIST)WRITE(6,1103)  !   Asterisks
  500 CONTINUE
C
C     write summed,squared amplitudes
C
      IPERIM=0
      DO 530 K=1,IVERT
530   IPERIM=IPERIM+ISUM(1,K)+ISUM(IHOR,K)
      DO 535 J=2,IHOR-1
535   IPERIM=IPERIM+ISUM(J,1)+ISUM(J,IVERT)
      PERIM=FLOAT(IPERIM)/(2.0*(IVERT+IHOR-2))
C
      DO 520 K=1,IVERT
        DO 520 J=1,IHOR
          ISUMI(J,K)=ISUM(J,K)*7.0/PERIM + 0.5
          RSUMI(J,K)=ISUM(J,K)*7.0/PERIM + 0.5
          ISUM(J,K) = SQRT(FLOAT(ISUM(J,K)) /(NSPOT)) + 0.5
  520 CONTINUE
C
      WRITE(6,1250)     ! Amplitude output
 1250 FORMAT(/,/,132('*'),/,/,19X,'SQRT of summed,squared amplitudes',
     .  /,19X,33('-'),/)
      L = IVERT
      DO 540 K=1,IVERT
        WRITE(6,1270) (ISUM(J,L),J=1,IHOR)
 1270   FORMAT(/,15X,21I5,5(/,15X,21I5))
        L = L - 1
  540 CONTINUE
      WRITE(6,1252)
C
      SCALEFAC = 7.0/PERIM
      WRITE(6,1251) SCALEFAC            ! Intensity output
 1251 FORMAT(/,/,132('*'),/,/,19X,'scaled intensities (perimeter',
     .  ' averaged to 7.0)',
     .  '        scale factor = ',F12.7,/,19X,40('-'))
C
C  Here calculate how close the intensity average approaches theoretical.
C
      ICENTRE=ISUMI(IHOR2+1,IVERT2+1)
      INEAR=ISUMI(IHOR2+1,IVERT2) + ISUMI(IHOR2+1,IVERT2+2) +
     .   ISUMI(IHOR2,IVERT2+1) + ISUMI(IHOR2+2,IVERT2+1)
      IF(INEAR.LE.28) THEN
        IF(ICENTRE.GT.7)PERCENT=100.0
        IF(ICENTRE.LE.7)PERCENT=0.0
      ELSE
        PERCENT=((ICENTRE-7)*100.0)/((INEAR-28)*2.5)
      ENDIF
C
CHEN
C
      RSCAMAX=0.0
      L = IVERT
      DO 545 K=1,IVERT
        WRITE(6,1270) (ISUMI(J,L),J=1,IHOR)
         DO 544 ITEMP1=1,NHOR
            IF(RSUMI(ITEMP1,L).GT.RSCAMAX)RSCAMAX=RSUMI(ITEMP1,L)
  544    CONTINUE
        L = L - 1
  545 CONTINUE
C
CHEN
C
      WRITE(6,1254)PERCENT
1254  FORMAT(/,19X,'above indicates shape that is',F7.1,' % perfect')
      IF(PERCENT.GE.85.0) WRITE(6,1255)
      IF(PERCENT.GE.50.0.AND.PERCENT.LT.85.) WRITE(6,1256)
      IF(PERCENT.LT.50.0) WRITE(6,1257)
1255  FORMAT(19X,'this is quite satisfactory, well done!!')
1256  FORMAT(19X,'this is not bad, but could be improved!')
1257  FORMAT(19x,'this is not good enough, you must try harder!')
      WRITE(6,1252)
1252  FORMAT(/,/)
      WRITE(6,1253) NUMOUT,NGOOD,NBAD,(J,NIQ(J),J=1,8)
1253  FORMAT(I10,'  Total spots found',/,
     .  I10,'  Good spots for output',/,I10,'  Bad spots not used',/,
     .  '    IQ    NUMBER',/,8(I6,I10,/))
1258  FORMAT(' List of',I6,'  Spots processed')
      IOU2=11
      IF(IOU2.NE.0)THEN
        OPEN(UNIT=IOU2,FILE=FILOU2,STATUS='NEW')
        WRITE(6,'(''Opened file : '',A40)') FILOU2
C
        WRITE(IOU2,1259)RSCAMAX,(NIQ(J),J=1,9)
1259  FORMAT('MAX=',F10.3,6X,'IQ=',9(I4))
        IF(PERCENT.GE.85.0)WRITE(IOU2,1260)PERCENT
        IF(PERCENT.GE.50.0.AND.PERCENT.LT.85.0)THEN
          WRITE(IOU2,1261)PERCENT
        ENDIF
        IF(PERCENT.LT.50.0)WRITE(IOU2,1262)PERCENT
1260    FORMAT(F6.1,'% perfect peak-shapes. Good !')
1261    FORMAT(F6.1,'% perfect peak-shapes. ',
     1              'Not bad, but could be better.')
1262    FORMAT(F6.1,'% perfect peak-shapes. Not good enough.')
        CLOSE(IOU2)
        WRITE(6,'(''FILE written and closed .'')') 
      ENDIF
C
      RETURN
      END
C
C*******************************************************************************
C
      SUBROUTINE AMPHA(IX,IY,APART,BPART,AMP,PHASE,DELPX,DELPY,TURN)
      LOGICAL TURN
C
C     subroutine to translate APART,BPART into amplitude & phase in 
C     degrees and apply origin phase shift to APART,BPART,AMP and PHASE.
C     --includes the Friedel relation B=-B used when the transform has
C       has to be turned to get hold of the right bit (normally this is 
C       when X is negative).
C
      PSHIFT = IX * DELPX + IY * DELPY
      IF(TURN) PSHIFT = - PSHIFT        ! apply phase shift to stored transform
C      IF(IX.LT.0) PSHIFT = - PSHIFT    ! this in old MMBOX program
      C = COS(PSHIFT)
      S = SIN(PSHIFT)
      A = APART * C - BPART * S
      B = APART * S + BPART * C
      IF(TURN) B=-B                     ! Friedel relation for turned transform
C      IF(IX.LT.0) B = - B              ! this in old MMBOX program.
      APART = A
      BPART = B
      AMP = SQRT(A * A + B * B)
      IF(AMP.EQ.0.) THEN
      PHASE = 0.
      ELSE 
      PHASE = ATAN2(B,A) * 57.2958
      END IF
      IF(PHASE.LT.0.) PHASE = PHASE + 360.      ! Phase bet 0 and 360 degs.
      RETURN
      END
C*******************************************************************************
C
      SUBROUTINE RDSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,INVERT,
     .   IX,IY,AP,BP,DELPX,DELPY,TURN)
C
C     subroutine to read part of section required plus the extra area needed
C     for the ctf-dependent convolution, then store in array AP,BP for return
C     to main program ---- phases are corrected to desired phase origin.
C
      PARAMETER (IMSIZE=20100)
      PARAMETER (IDEEP=30)
      PARAMETER (NMAX=7000)
      PARAMETER (IBOXMAX=61)
      PARAMETER (ICTFBXMAX=401)
      PARAMETER (INBOXMAX=341)
      PARAMETER (ICTFHALF=200)
      DIMENSION ARRAY(2*INBOXMAX,INBOXMAX)      ! Square array of complex no's.
      DIMENSION AP(INBOXMAX,INBOXMAX),BP(INBOXMAX,INBOXMAX)
      LOGICAL TURN
      CALL IRDPAS(1,ARRAY,2*INBOXMAX,INBOXMAX,IX1,IX2,IY1,IY2,*900)
      CALL IMPOSN(1,0,0)
      KX = IX
      IF(.NOT.TURN) THEN
C
C     straightforward case, no turning
C
      DO 100 K=1,INVERT
      L = 1
      IY = IY + 1
      IX = KX
      DO 100 J=IXOUT1,IXOUT2
      APART = ARRAY(L,K)
      BPART = ARRAY(L+1,K)
      IX = IX + 1
      CALL AMPHA(IX,IY,APART,BPART,AMP,PHASE,DELPX,DELPY,TURN)
      AP(J,K) = APART
      BP(J,K) = BPART
      L = L + 2
  100 CONTINUE
      ELSE
C
C     turn upside down & back to front
C
      KK = INVERT + 1
      DO 200 K=1,INVERT
      KK = KK - 1
      JJ = IXOUT2 + 1
      L = 1
      IY = IY - 1
      IX = KX
      DO 200 J=IXOUT1,IXOUT2
      JJ = JJ - 1
      APART = ARRAY(L,K)
      BPART = ARRAY(L+1,K)
      IX = IX - 1
      CALL AMPHA(IX,IY,APART,BPART,AMP,PHASE,DELPX,DELPY,TURN)
      AP(JJ,KK) = APART
      BP(JJ,KK) = BPART
      L = L + 2
  200 CONTINUE
      END IF
      TURN = .FALSE.
      RETURN
C
C     diagnostics
C
  900 WRITE(6,10)
   10 FORMAT(/,' error on reading transform')
      STOP
      END
C
C*******************************************************************************
C
      SUBROUTINE WRTSECT(IX1,IX2,IY1,IY2,IXOUT1,IXOUT2,IVERT,
     .   IX,IY,ACORR,BCORR,DELPX,DELPY,TURN)
C
C     subroutine to write part of Fourier transform back to disc.
C     this subroutine should do precisely the opposite to RDSECT.
C
      PARAMETER (IMSIZE=20100)
      PARAMETER (IDEEP=30)
      PARAMETER (NMAX=7000)
      PARAMETER (IBOXMAX=61)
      PARAMETER (ICTFBXMAX=401)
      PARAMETER (INBOXMAX=341)
      PARAMETER (ICTFHALF=200)
      DIMENSION ARRAY(2*IBOXMAX,IBOXMAX)        ! Square array of complex no's.
      DIMENSION ACORR(IBOXMAX,IBOXMAX),BCORR(IBOXMAX,IBOXMAX)
      DIMENSION ALINE(IMSIZE)           ! MAX TRANSFORM SIZE IS (IMSIZE-2)/2
      LOGICAL TURN
      KX = IX
      IF(.NOT.TURN) THEN
C
C     straightforward case, no turning
C
      DO 100 K=1,IVERT
      L = 1
      IY = IY + 1
      IX = KX
      DO 100 J=IXOUT1,IXOUT2
      APART = ACORR(J,K)
      BPART = BCORR(J,K)
      IX = IX + 1
      CALL AMPHA(IX,IY,APART,BPART,AMP,PHASE,-DELPX,-DELPY,TURN)
      ARRAY(L,K) = APART
      ARRAY(L+1,K) = BPART
      L = L + 2
  100 CONTINUE
      ELSE
C
C     turn upside down & back to front
C
      KK = IVERT + 1
      DO 200 K=1,IVERT
      KK = KK - 1
      JJ = IXOUT2 + 1
      L = 1
      IY = IY - 1
      IX = KX
      DO 200 J=IXOUT1,IXOUT2
      JJ = JJ - 1
      APART = ACORR(JJ,KK)
      BPART = BCORR(JJ,KK)
      IX = IX - 1
      CALL AMPHA(IX,IY,APART,BPART,AMP,PHASE,-DELPX,-DELPY,TURN)
      ARRAY(L,K) = APART
      ARRAY(L+1,K) = BPART
      L = L + 2
  200 CONTINUE
      END IF
      TURN = .FALSE.
      DO 1200 KY=IY1,IY2
      JY=KY-IY1+1
      CALL IMPOSN(1,0,KY)
      CALL IRDLIN(1,ALINE,*900)
      DO 1300 KX=IX1,IX2
        JX=2*(KX-IX1+1)-1
        LX=2*KX+1
        ALINE(LX)=ARRAY(JX,JY)
1300    ALINE(LX+1)=ARRAY(JX+1,JY)
      CALL IMPOSN(1,0,KY)
      CALL IWRLIN(1,ALINE)
1200  CONTINUE
      CALL IMPOSN(1,0,0)
      RETURN
900   WRITE(6,901)KY
901   FORMAT(' READ ERROR IN SUBROUTINE WRTSECT, LINE',I6)
      STOP
      END
C
C*******************************************************************************
C
      SUBROUTINE ANGAVE(N, THETAS, WEIGHTS, THMEAN, THGOOD)
C     Function: to average a set of angles in degrees
C     Created: 27/7/84 by D.J.Thomas
C     Modified:  by R.HENDERSON 20.5.85
      INTEGER*4 N               !number of input angles
      REAL*4    THETAS(1)       !array of input angles
      REAL*4    THGOOD          !goodness of average (0 to 1)
      REAL*4    THMEAN          !weighted average of input angles
      REAL      COMEAN          !mean value of cosines
      REAL      SIMEAN          !mean value of sines
      REAL              WEIGHT          !total input weight
      REAL*4            WEIGHTS(1)      !weights on input angles
C
      IF (N .LE. 0) GOTO 20
      WEIGHT = 0.0
      COMEAN = 0.0
      SIMEAN = 0.0
      DO 10 I=1,N
        WEIGHT = WEIGHT + WEIGHTS(I)
        COMEAN = COMEAN + (COS(THETAS(I)*0.01745329252)*WEIGHTS(I))
        SIMEAN = SIMEAN + (SIN(THETAS(I)*0.01745329252)*WEIGHTS(I))
10    CONTINUE
      IF ((SIMEAN .EQ. 0.0) .AND. (COMEAN .EQ. 0.0)) GOTO 20
        THMEAN = 57.295779513*ATAN2(SIMEAN,COMEAN)
        IF(THMEAN.LT.0.0) THMEAN=THMEAN+360.0
        IF (WEIGHT .EQ. 0.0) GOTO 20
        THGOOD = SQRT((SIMEAN*SIMEAN) + (COMEAN*COMEAN))/WEIGHT
        RETURN
20    THGOOD = 0.0                      !average is undefined
      RETURN
      END
C
C*******************************************************************************
C
      SUBROUTINE CONVOLUTE(AP,BP,ACTF,BCTF,IAMP,IPHI,IHOR,IVERT,
     .   ICTFHOR,ICTFVERT,ACORR,BCORR)
C
C  Subroutine to perform convolution of raw transform with transform
C   of ctf for tilted image correction.
C
      PARAMETER (IMSIZE=20100)
      PARAMETER (IDEEP=30)
      PARAMETER (NMAX=7000)
      PARAMETER (IBOXMAX=61)
      PARAMETER (ICTFBXMAX=401)
      PARAMETER (INBOXMAX=341)
      PARAMETER (ICTFHALF=200)
      DIMENSION IAMP(IBOXMAX,IBOXMAX),IPHI(IBOXMAX,IBOXMAX),
     .          ACORR(IBOXMAX,IBOXMAX),BCORR(IBOXMAX,IBOXMAX),
     .          ACTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .          BCTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .          AP(INBOXMAX,INBOXMAX),BP(INBOXMAX,INBOXMAX)
c      DIMENSION AT(5,5),PT(5,5)                ! TEST DIAGNOSTIC O/P
C
      ICTFHOR2 = ICTFHOR/2
      ICTFVERT2 = ICTFVERT/2
      DO 100 I=1,IHOR
      DO 100 J=1,IVERT
        A=0.
        B=0.
        DO 50 ICTF=-ICTFHOR2,ICTFHOR2
        DO 50 JCTF=-ICTFVERT2,ICTFVERT2
           K=I-ICTF+ICTFHOR2
           L=J-JCTF+ICTFVERT2
           A = A + AP(K,L)*ACTF(ICTF,JCTF) - BP(K,L)*BCTF(ICTF,JCTF)
           B = B + AP(K,L)*BCTF(ICTF,JCTF) + BP(K,L)*ACTF(ICTF,JCTF)
50      CONTINUE
        AMP = SQRT(A*A + B*B)
        IF(AMP.EQ.0.0) THEN
                PHASE = 0.
        ELSE
                PHASE = ATAN2(B,A) * 57.2958
        ENDIF
        IF(PHASE.LT.0.0) PHASE = PHASE + 360.0  ! Phase bet 0 and 360 deg.
        IAMP(I,J) = AMP  +  0.5         ! ROUNDED TO NEAREST INTEGER
        IPHI(I,J) = PHASE + 0.5         ! ROUNDED TO NEAREST INTEGER
        ACORR(I,J) = A
        BCORR(I,J) = B
100   CONTINUE
C
CHEN>
C      write(*,'('' Results: '')')
C      do J = 1,ICTFVERT
C        write(*,'(I5,'' = '',200I5)')J,(IAMP(I,J),I=1,ICTFHOR)
C      enddo
CHEN<
C
      RETURN
      END
C
C*******************************************************************************
C
      SUBROUTINE SCSL_CONVOLUTE(AP,BP,ACTF,BCTF,IAMP,IPHI,IHOR,IVERT,
     .           ICTFHOR,ICTFVERT,ACORR,BCORR)
C
C  Subroutine to perform convolution of raw transform with transform
C   of ctf for tilted image correction. (using SCSL)
C
C--------------------------ICTFBXMAX muss 2*ICTFHALF+1 sein
      PARAMETER (IMSIZE=20100)
      PARAMETER (IDEEP=30)
      PARAMETER (NMAX=7000)
      PARAMETER (IBOXMAX=61)
      PARAMETER (ICTFBXMAX=401)
      PARAMETER (INBOXMAX=341)
      PARAMETER (ICTFHALF=200)
      DIMENSION IAMP(IBOXMAX,IBOXMAX),IPHI(IBOXMAX,IBOXMAX),
     .          ACORR(IBOXMAX,IBOXMAX),BCORR(IBOXMAX,IBOXMAX),
     .          ACTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .          BCTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .          AP(INBOXMAX,INBOXMAX),BP(INBOXMAX,INBOXMAX)
C
      COMPLEX P      (IHOR,IVERT)
      COMPLEX CTF    (ICTFHOR,ICTFVERT)
      COMPLEX OUTPUT (ICTFHOR+IHOR-1,ICTFVERT+IVERT-1)
      COMPLEX APLHA,BETA
C
C-----Fill complex array p(1:IHOR+ICTFHOR,1:IVERT+ICTFVERT)
C
      DO I=1,IHOR
        DO J=1,IVERT
          p(I,J)=CMPLX(AP(I,J),BP(I,J))
        enddo
      enddo
C      print *,"\n\nInput: "
C      print *,((p(I,J),'\t',I=1,IHOR),'\n',J=1,IVERT)
C
      ICTFHOR2 = ICTFHOR/2
      ICTFVERT2 = ICTFVERT/2
C
C-----Fill complex array ctf centered arround ICTFHOR2 from
C-----arrays that were centered arround origin (-ICTFHOR2:ICTFHOR2,-ICTFVERT2:ICTFVERT2)
C
      DO ICTF=-ICTFHOR2,ICTFHOR2
        DO JCTF=-ICTFVERT2,ICTFVERT2
          ctf(ICTF+ICTFHOR2+1,JCTF+ICTFVERT2+1)=
     .       CMPLX(ACTF(ICTF,JCTF),BCTF(ICTF,JCTF))
        ENDDO
      ENDDO
C
C      print *,"\n\nCTF-Input: "
C      print *,((ctf(I,J),'\t',I=1,ICTFHOR),'\n',J=1,ICTFVERT)
C
C
      ALPHA=CMPLX(1,0)
      BETA=CMPLX(0,0)
C
C      CALL CFIR2D(
C       CALL TDXCONV(
C     .       P(1,1),                    1,INBOXMAX,  1,
C     .             ICTFHOR+IHOR, 1,         ICTFVERT+IVERT,
C     .       CTF(-ICTFHOR2,-ICTFVERT2), 1,ICTFBXMAX, 1,
C     .             ICTFHOR+1,    1,         ICTFVERT+1,
C     .       OUTPUT(1,1),               1,INBOXMAX,    
C     .           2+ICTFHOR,        IHOR, 2+ICTFVERT,         IVERT,
C     .       ALPHA,BETA)
C
C      CALL TDXCONV(P(1,1),IHOR,IVERT,
      CALL CONVOLUTE(P(1,1),IHOR,IVERT,
     .  CTF(1,1), ICTFHOR, ICTFVERT,
     .  OUTPUT(1,1))
C
C     CALL CFIR2D (x, incx, ldx, i1x0, nx1, i2x0, nx2,
C                  h, inch, ldh, i1h0, nh1, i2h0, nh2,
C                  y, incy, ldy, i1y0, ny1, i2y0, ny2,
C                  alpha, beta)
C
C      print *,"\n\nOutput: "
C      print *,((OUTPUT(I,J),'\t',I=1,IHOR+ICTFHOR-1),'\n',J=1,IVERT+ICTFVERT-1)
 
      DO I=1,IHOR
        DO J=1,IVERT
          K = MOD(I,IHOR-ICTFHOR)+ICTFHOR
          L = MOD(J,IVERT-ICTFVERT)+ICTFVERT
          IAMP(I,J) = CABS(OUTPUT(K,L)) + 0.5
          RIMA = AIMAG(OUTPUT(K,L))
          RREA = REAL (OUTPUT(K,L))
          IF(IAMP(I,J).EQ.0.0) THEN
            PHASE = 0.
          ELSE
            PHASE = ATAN2(RIMA,RREA) * 57.2958
          ENDIF
          IF(PHASE.LT.0.0) PHASE = PHASE + 360.0        ! Phase bet 0 and 360 deg.
          IPHI(I,J) = PHASE + 0.5
          ACORR(I,J) = RREA
          BCORR(I,J) = RIMA
        enddo
      enddo
C
C      print *,"\n\nResults: "
C      print *,((IAMP(I,J),'\t',I=1,IHOR),'\n',J=1,IVERT)
      RETURN
      END
C
C*******************************************************************************
C
      SUBROUTINE CTFGEN(IH,IK,X,Y,RATIOXY,
     .THETATR,DFMID1,DFMID2,ANGAST,
     .CS,WL,STEPR,ISIZEX,ISIZEY,DELHEIGHT,
     .TLTAXIS,TLTANGL,TANTILT,COST,SINT,
     .ICTFHOR,ICTFVERT,ACTF,BCTF,
     .ILIST,DFMID,DELCHI,CTFMID,FACTOR,ISENS)
C
      PARAMETER (IMSIZE=20100)
      PARAMETER (IDEEP=30)
      PARAMETER (NMAX=7000)
      PARAMETER (IBOXMAX=61)
      PARAMETER (ICTFBXMAX=401)
      PARAMETER (INBOXMAX=341)
      PARAMETER (ICTFHALF=200)
      DIMENSION ACTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .          BCTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .           CTF(ICTFBXMAX*ICTFBXMAX)
      LOGICAL ILIST
      ISENS=ICHAR(' ')
      TWOPI = 2.0 * 3.1415926
      YC=Y*RATIOXY
      RAD = SQRT(X**2+YC**2)
      ANGLE=RAD*THETATR
      ANGSPT=ATAN2(YC,X)
      C1=TWOPI*ANGLE*ANGLE/(2.0*WL)
      DELCHI=C1*DELHEIGHT
      SINEWAVES=DELCHI/TWOPI
          ICTFHOR=MAX(10,INT(DELCHI))
          ICTFHOR=(ICTFHOR/2)*2                     ! ensure ICTFHOR is even.
          IF(ICTFHOR.GT.38) ICTFHOR=(ICTFHOR/8)*8   ! ensures prime factor < 19
          IF(ICTFHOR.GT.ICTFBXMAX-1) THEN           ! ensures storage ok.
                WRITE(6,101)ICTFHOR
101             FORMAT(' Subroutine CTFGEN dimensions too small,',
     .                 '  ICTFHOR needs',I16)
                STOP
          ENDIF
          ICTFVERT=ICTFHOR
          ICTFHOR2=ICTFHOR/2
          ICTFVERT2=ICTFVERT/2
      C2=-C1*CS*ANGLE*ANGLE/2.0
      ANGDIF=ANGSPT-ANGAST
        CCOS=COS(2.0*ANGDIF)
        CSIN=SIN(2.0*ANGDIF)
C       COST=COS(TLTAXIS)
C       SINT=SIN(TLTAXIS)
        DFMID=0.5*(DFMID1+DFMID2+CCOS*(DFMID1-DFMID2))
        CTFMID=-SIN(C1*DFMID+C2)
        IF(DELCHI/2.GT.ASIN(ABS(CTFMID))) ISENS=ICHAR('*')  !Spot has a zero in ctf
        SUMC=0.0
      DO 100 I=1,ICTFHOR
      DO 100 J=1,ICTFVERT
        ISTORE=I+(ICTFHOR+2)*(J-1)      ! indexing for array CTF.
C         Calculate height of this element of image.
        XP=((I-0.5-ICTFHOR2)/(ICTFHOR))*ISIZEX*STEPR! 0.5 rounding error
        YP=((J-0.5-ICTFVERT2)/(ICTFVERT))*ISIZEY*STEPR! 0.5 rounding error
        DF = DFMID + TANTILT*(-XP*SINT+YP*COST)
        CHI=C1*DF+C2
        CNTRST=-SIN(CHI)
        SUMC=SUMC+ABS(CNTRST)
100   CTF(ISTORE)=CNTRST
C
C  Now rescale so that the same power is present in the spot after convolution.
      RESCALE=ICTFHOR/SUMC
      FACTOR =ICTFHOR*ICTFVERT/SUMC
      DO 120 I=1,ICTFHOR
      DO 120 J=1,ICTFVERT
        ISTORE=I+(ICTFHOR+2)*(J-1)      ! indexing for array CTF.
120   CTF(ISTORE)=RESCALE*CTF(ISTORE)
C
C
CHEN
C-----CALL TODFFT(CTF,ICTFHOR,ICTFVERT,0)
      CALL TDXFFT(CTF,ICTFHOR,ICTFVERT,0)
CHEN
C
C
C  Here to transfer the transform of ctf to ACTF,BCTF. At the same time,
C     move phase origin for CTF to middle of image, as is presumed to be
C     the case for the requested phase origin in input XORIG, YORIG.
C
C  Now calculate correct phase shift for slightly offset origin for the
C     contrast distribution function.
      PHSHFT = -TWOPI*(ICTFHOR2-0.5)/ICTFHOR
C
        DO 150 I=1,ICTFHOR2+1
        DO 150 J=1,ICTFVERT
        ISTORE = (2*I-1)+(ICTFHOR+2)*(J-1)
        IF(J.LE.1+ICTFVERT2) THEN
                IX=I-1
                IY=J-1
                C=COS((IX+IY)*PHSHFT)   ! assumes ICTFHOR=ICTFVERT
                S=SIN((IX+IY)*PHSHFT)   ! assumes ICTFHOR=ICTFVERT
                A = CTF(ISTORE)
                B = CTF(ISTORE+1)
                ACTF(IX,IY) = A*C-B*S
                BCTF(IX,IY) = A*S+B*C
        ENDIF
        IF(J.GE.1+ICTFVERT2) THEN
                IX=I-1
                IY=J-1-ICTFVERT
                C=COS((IX+IY)*PHSHFT)   ! assumes ICTFHOR=ICTFVERT
                S=SIN((IX+IY)*PHSHFT)   ! assumes ICTFHOR=ICTFVERT
                A = CTF(ISTORE)
                B = CTF(ISTORE+1)
                ACTF(IX,IY) = A*C-B*S
                BCTF(IX,IY) = A*S+B*C
        ENDIF
150     CONTINUE
        DO 160 I= 1,ICTFHOR2
        DO 160 J=-ICTFVERT2,ICTFVERT2
                ACTF(-I,-J) =  ACTF(I,J)
                BCTF(-I,-J) = -BCTF(I,J)
160     CONTINUE
C
      IF(ILIST) WRITE(6,103)DELHEIGHT,DFMID,SINEWAVES,
     .          DELCHI,ICTFHOR,CTFMID,FACTOR
103   FORMAT(' Contrast transfer function description -----',
     .  '-------- Height difference ===================',F10.1,/,
     .  54X,'Midpoint defocus ====================',F10.1,/,
     .  54X,'Number ctf cycles ===================',F10.3,/,
     .  54X,'Number ctf samples needed (used)=====',F10.1,'(',I2,')',/,
     .  54X,'Midpoint C.T.F. =====================',F10.4,/,
     .  54X,'Rescaling factor (keeps noise const)=',F10.4)
      RETURN
      END
C
C*******************************************************************************
C
C
      SUBROUTINE CALCRAD(RADCLC,RADMIN,FCTR,PERPMAX,XPOS,YPOS,YCPOS,
     .COSTLTX,SINTLTX)
C
C       FCTR=(RADMAX-RADMIN)/PERPMAX
C
      PERP=YCPOS*COSTLTX-XPOS*SINTLTX
      IF(PERP.LT.0.0)PERP=-PERP
      IF(PERP.GT.PERPMAX)PERP=PERPMAX
      RADCLC= RADMIN  +  PERP*FCTR 
C      WRITE(6,10)RADCLC,XPOS,YPOS,COSTLTX,SINTLTX,PERPMAX,FCTR
10      FORMAT(7F10.4)
      RETURN
      END
