C*******************************************************************************
C
C TTREFINE : refines defocus, astigmatism, tiltaxis and tiltangle in images
C            of thin tilted crystals.
C         VERY IMPORTANT NOTE : The quality of image data required for success
C            in this type of least squares refinement is high, particularly
C            if more than one parameter is to be refined. BE VERY CAREFUL.
C
C       Version 1.01    9 Dec 1985      RH
C       Version 1.02    27 Dec 1985     RH
C       Version 1.03    13 Sep 1986     JMB     Corr stat interm output.
C       Version 1.04    1 May 1987      RH      Allows points off curve
C       Version 1.05    1 July 1987     RH      Allows NX.ne. NY
C       Version 1.06    2 July 1987     JMB     Allows up to 800 spots
C       Version 1.07    28 Oct 1987     JMB     ISHIFT option on card 1
C       Version 1.08    2  Nov 1987     JMB     IREF=0 option on card 1
C       Version 1.09    7  Dec 1987     JMB     Some tidying
C       Version 1.10    30 Dec 1987     RH      Correction to TANGL sign
C       Version 1.11    19 Feb 1988     JMB     Some Format alterations
C       Version 1.12    30 Mar 1989     RH      FSHIFT drops if oscill.
C       Version 1.13     8.Jun 1989     JMB     Better converg. test & formats.
C       Version 1.14    14 Jun 1989     RH      IQ=9 added.
C       Version 2.00     3 Jan 1992     RH      Convert to UNIX for Alliant
C       Version 2.01    25 Jul 1995     RH      Convert to Alpha, extraneous bit
C       Version 2.02     4 Jul 1999     RH      add mtz input, remove all lcf
C       Version 2.03    30 Oct 1999     RH      STOP if ISHIFT=T and NCYC.gt.1
C       Version 2.04    29 Mar 2000    JMS      irtorg mod to include zorigin
C       Version 2.05    29 Oct 2001     RH      change FILIN to CHARACTER*80
C       Version 3.00    30 Oct 2005     HS      2dx
C
C DATACARDS :
C
C  1.   IMODE,NCYC,FSHIFT,LIST,MTZ,MAXIM,LISTS,ISHIFT,IREF        (*)
C
C  2.   FILIN -- full name of input file (.FFT)
C
C  3.   ISIZEX,ISIZEY,DSTEP,XMAG,CS,KVOLT           (*)
C
C  4.   NUMSPOT, NOH, NOK, NHOR, NVERT              (*)
C
C  5.   RESMIN, RESMAX                              (*)
C
C  6.   AX, AY, BX, BY                              (*)
C
C  7.   ISPGRP,ORIGH,ORIGK,REVHK,ROT180,SGNXCH      (*)
C
C  8.   DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL   (*)  These are parameters which
C                                                       can be refined.
C  9.   LABIN FC=F SIGFC=SIGF PHCAL=PHS FOM=FOM ##      Program uses SIGFC as test for
C                                                       the presence of F and FOM as
C                                                       test for presence of phase. This
C                                                       data is input on stream HKLIN
C
C
C     IMODE       if = 1 Defocus refined
C                    = 2 Defocus and astigmatism refined.
C                    = 3 Tiltaxis refined.
C                    = 4 Tiltangle refined.
C                    = 5 All five parameters refined.
C     NCYC        Maximum number of cycles of refinement.
C     FSHIFT      Fractional shifts to be applied at each cycle.
C     LIST        if T, list spots whose amps and phases will be used,
C                       and list their resulting details.
C     MTZ         if T, reads in ref amplitudes from MTZ(or non-MTZ) file.
C                 if F, uses dummy amplitudes, in which case the various
C                       R-factors and residuals do not mean a lot.
C     MAXIM       IF T, the program simply maximises the corrected diffraction
C                        spot intensities.
C                 if F, the program minimises the rms R-factor against MTZ amps.
C     LISTS     IF T, write output to data set 10 for use in statistics 
C                 program
C     ISHIFT    IF T, adds 1/3(ax + bx); 1/3(ay +by) to lattice points for use
C                 in control calculations. If F does nothing. As an added 
C                 caution, ISHIFT.eq.T requires NCYC.lt.2
C     IREF        if = 0 Set IREF=0. Read formatted data h,k,a,p; not MTZ file
C                    = 1 Use MTZ file ---- only needed if MTZ.eq.T
C
C     ISIZEX      size of image, used to check against image file-header.
C     ISIZEY
C     DSTEP       densitometer stepsize in microns.
C     XMAG        magnification of micrograph.
C     CS          spherical aberration coefficient in mm.
C     KVOLT       microscope voltage in KV, used to calculate wavelength. 
C     NUMSPOT     number of spots to be printed out at each cycle.
C     NOH, NOK    number of orders of spots in H & K directions to be generated.
C     NHOR, NVERT box size in grid units in horizontal & vertical directions,
C                 i.e. X & Y resp. ( up to 20 grid units in each
C                 direction).
C     RESMIN, RESMAX inner & outer resolution limits in Angstroms within which
C                        spots(centre of box) must fall.
C     AX,AY,BX,BY coordinates in grid units of 1,0 & 0,1 spots
C                 respectively.
C     DFMID1      defocus in one direction (underfocus +ve) 
C     DFMID2      defocus at 90-degs to above
C     ANGAST      direction for DFMID1 in degrees relative to x,y in transform.
C     TLTAXIS     direction of tiltaxis in degrees relative to x,y in transform,
C                   should be between -90 and +90 degrees.
C     TLTANGL     magnitude of tiltangle.
C                       (+ve for less underfocus at start of scan(y=0))
C                          (or if TLTAXIS is parallel to y, at x=0)
C     ISPGRP      two-dimensional space group number (1 - 17)
C     ORIGH,ORIGK phase origin shifts for (1,0) and (0,1) reflection to bring
C                 phases to the precise crystallographic origin (from ORIGMERG).
C     REVHK       reverses H and K before comparing to reference data.
C     ROT180      rotates by 180 degs about c-axis. (needed in p3)
C     SGNXCH      rotates by 180 degs about a-axis. (needed in p121)
C
C  The function which this program minimises (when MTZ=.TRUE. and MAXIM=.FALSE.)
C    is :-
C
C    L = Sum of [1/sigma**2]*[Aobs-Aed*<ctf**2>]**2
C
C                                       where Aed is the scaled (sf+tf)
C                                          electron diffraction data.
C                                       and Aobs is the result of con-
C                                          volution of the F.T. of the
C                                          image with the F.T. of the
C                                          function of ctf with position.
C                                       See Henderson et al, Utramic.(1986).
C
C*******************************************************************************
C
      PARAMETER (NMAX=1500)
      PARAMETER (IDSUM=25)
      PARAMETER (IBOXMAX=41)
      PARAMETER (ICTFBXMAX=401)
      PARAMETER (INBOXMAX=361)
      REAL KVOLT
      REAL*8 A(5,5),B(5),BOLD(5)                ! for matrix inversion
      DIMENSION XA(NMAX),YA(NMAX),IXC(NMAX),IYC(NMAX),IH(NMAX),IK(NMAX)
      DIMENSION CTFSQ(NMAX),AIM(NMAX),PIM(NMAX),BIM(NMAX),IHTRUE(NMAX),
     .          IKTRUE(NMAX),AREF(NMAX),PREF(NMAX),AREFSCALE(NMAX)
      DIMENSION IAMP(IBOXMAX,IBOXMAX),IPHI(IBOXMAX,IBOXMAX),
     .           AMP(IBOXMAX,IBOXMAX), PHI(IBOXMAX,IBOXMAX),
     .           ACORR(IBOXMAX,IBOXMAX),BCORR(IBOXMAX,IBOXMAX),
     .           DA(IBOXMAX,IBOXMAX,5),DB(IBOXMAX,IBOXMAX,5),
     .          IXGU(IBOXMAX),IYGU(IBOXMAX),ISUM(IBOXMAX,IBOXMAX),
     .          ISUMI(IBOXMAX,IBOXMAX)
      DIMENSION AP(INBOXMAX,INBOXMAX),BP(INBOXMAX,INBOXMAX)
      DIMENSION ACTF(ICTFBXMAX,ICTFBXMAX),BCTF(ICTFBXMAX,ICTFBXMAX)
      DIMENSION GRADCTFSQ(5,NMAX),CJ(5),FSTORE(5),              !
     .          GRADAMP(5,NMAX),GRADAPART(5),GRADBPART(5),      ! Derivatives
     .          DACTF(ICTFBXMAX,ICTFBXMAX,5),                   ! of various
     .          DBCTF(ICTFBXMAX,ICTFBXMAX,5)                    ! thingies.
      DIMENSION ISUMMARY(IDSUM,5),SUMMARY(IDSUM,10)     ! for summary table
      DIMENSION NXYZ(3),MXYZ(3),PHANG(4),WTS(4),DELX(2),DELY(2),NIQ(9)
      DIMENSION PARAMS(5), PARAMSLIM(5), APPLYSHFT(5)
      LOGICAL TURN,ILIST,LIST,MTZ,MAXIM,LISTS,ISHIFT
      CHARACTER*80 FILIN
      INTEGER*4 IPERIM
      EQUIVALENCE (NXYZ(1),NX),(NXYZ(2),NY),(NXYZ(3),NZ)
      EQUIVALENCE (PARAMS(1),DFMID1),(PARAMS(2),DFMID2),
     .  (PARAMS(3),ANGAST),(PARAMS(4),TLTAXIS),(PARAMS(5),TLTANGL)
      DATA PI/3.1415926/,TWOPI/6.2831853/,ILIST/.TRUE./
      DATA AREF/NMAX*1.0/,PREF/NMAX*-999./,FSHIFT/0.5/
      DATA PARAMSLIM/2*500.,2*0.174533,0.03491/
                RDEG=360.0/TWOPI
                DRAD=TWOPI/360.0
C
      WRITE(6,1000)
 1000 FORMAT('0',' TTREFINE-2dx VX 2.03(30-Oct-99) :',/,
     .  ' Refines CTF and',
     1' crystal tilt by',
     2' comparison of image and e.d. amplitudes',/,'            ',
     3' taking c.t.f. in tilted images fully into account')
      READ(5,*) IMODE, NCYC, FSHIFT, LIST, MTZ,MAXIM,LISTS,ISHIFT,IREF
      IF(ISHIFT.and.NCYC.gt.1)
     .  STOP 'WARNING --- ISHIFT.eq.T only allowed if NCYC.lt.2'
      WRITE(6,1006) IMODE, NCYC, FSHIFT, LIST, MTZ,MAXIM,LISTS,ISHIFT,
     .IREF
1006  FORMAT(' IMODE =',I5,/,' NCYC  =',I5,/,' FSHIFT=',F6.2,/
     .       ' LIST  =',3X,L1/' MTZ   =',3X,L1/' MAXIM =',3X,L1/,
     .' LISTS =',3X,L1,/,' ISHIFT=',3X,L1,/,' IREF  =',I4,/)
      IF(.NOT.MTZ.AND..NOT.MAXIM) THEN
                WRITE(6,1007)
1007            FORMAT(' It is very unwise to do R-factor minimisation',
     .           ' (MAXIM=F) when no reference MTZ data is available')
                STOP
      ENDIF
      READ(5,1005) FILIN
 1005 FORMAT(A)
      if (IMODE.ne.0)then
        CALL  IMOPEN(1,FILIN,'RO')
        CALL  IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
        CALL  IRTORG(1,XOR,YOR,ZOR)
        WRITE(6,1010) XOR,YOR,ZOR
 1010 FORMAT('1 X & Y phase origin shift read from transform',
     1           2F10.2)
        NPHI = 4
        NY2 = NY / 2
        NY2M1 = NY2 - 1
        NY2M2 = NY2 - 2
        NXP2 = NX * 2
        NXM1 = NX - 1
        NXM2 = NX - 2
        XORIG = NXM1            ! ALWAYS PUTS ORIGIN IN MIDDLE OF IMAGE
        YORIG = NY2             ! ALWAYS PUTS ORIGIN IN MIDDLE OF IMAGE
      endif
C
C  Input of all c.t.f. and tilt data for image, calculation of all the needed
C      preliminaries except for calculation of c.t.f. itself.
C
      READ(5,*) ISIZEX,ISIZEY,DSTEP,XMAG,CS,KVOLT
        IF(IMODE.ne.0 .AND. ISIZEY.NE.NY)     GO TO 6001
        IF(IMODE.ne.0 .AND. ISIZEX.NE.NXP2-2) GO TO 6001
        RATIOYX = FLOAT(ISIZEY)/FLOAT(ISIZEX)
      READ(5,*) NUMSPOT,NOH,NOK,NHOR,NVERT
      IF(NUMSPOT.EQ.0) ILIST=.FALSE.
C
      READ(5,*) RESMIN,RESMAX
      IF(RESMIN.LT.RESMAX) THEN
        R=RESMIN        ! Reverse if input in wrong order !
        RESMIN=RESMAX
        RESMAX=R
      ENDIF
      WRITE(6,1030) NUMSPOT,NOH,NOK,NHOR,NVERT,
     1              RESMIN,RESMAX,XORIG,YORIG
1030  FORMAT(/,' Number of spots printed =====================',I5,/,
     2        ' No. of orders in h & k=======================',2I5,/,
     3        ' No. of points in box horiz & vert direction =',2I5,/,
     4        ' Inner & outer resolution limits (Angstroms)==',2F8.1,/,
     5        ' X & Y phase origin (default) ================',2F8.1)
      DELPX = -TWOPI * (XOR + XORIG) / (2. * (NXM1))
      DELPY = -TWOPI * (YOR + YORIG) / NY
C
      READ(5,*) AX,AY,BX,BY,ABANG
      WRITE(6,1050) AX,AY,BX,BY,ABANG
 1050 FORMAT(/,' coordinates of 1,0 & 0,1 , ABANG ',5F10.3)
      AYR = AY/RATIOYX  ! coords referred to x-scale.
      BYR = BY/RATIOYX
      READ(5,*) ISPGRP,ORIGH,ORIGK,REVHK,ROT180,SGNXCH
      WRITE(6,1051) ISPGRP,ORIGH,ORIGK,REVHK,ROT180,SGNXCH
1051  FORMAT(/,'          ISPGRP -------------------',I5,/,
     .  '          ORIGH --------------------',F7.1,/,
     .  '          ORIGK --------------------',F7.1,/,
     .  '          REVHK --------------------',F7.1,/,
     .  '          ROT180 -------------------',F7.1,/,
     .  '          SGNXCH -------------------',F7.1,/)
      READ(5,*) DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
      IF(DFMID1.EQ.DFMID2.AND.IMODE.EQ.2) DFMID2=DFMID2+1
      IF(DFMID1.EQ.DFMID2.AND.IMODE.EQ.5) DFMID2=DFMID2+1
      IF(TLTANGL.EQ.0.0.AND.IMODE.GE.4) TLTANGL=0.1
98    IF(TLTAXIS.LT.-90.0) THEN
        TLTAXIS=TLTAXIS+180.0   ! PUTS TLTAXIS BETWEEN +/-90.
        GO TO 98
      ENDIF
99    IF(TLTAXIS.GE.90.0) THEN
        TLTAXIS=TLTAXIS-180.0   ! PUTS TLTAXIS BETWEEN +/-90.
        GO TO 99
      ENDIF
C
C  Calculate TAXA,TANGL from input values TLTAXIS,TLTANGL.
      ANGA=ATAN2(AYR,AX)*RDEG
      ANGB=ATAN2(BYR,BX)*RDEG
      IF(ABS(ANGB-ANGA).GT.180.0) ANGB=ANGB-SIGN(360.0,ANGB-ANGA)
C  angle between A and B now between -180 and +180 degs.
      TLTAXA=ANGA-TLTAXIS
      TLTAXB=ANGB-TLTAXIS
      IF(TLTAXB-TLTAXA.GT.0.0) THEN             ! TLTAXB always greater than TLTAXA.
        HAND = 1.0
      ELSE
        HAND = -1.0
        TLTAXA=-TLTAXA
        TLTAXB=-TLTAXB
      ENDIF
96    IF(TLTAXA.GE.90.0) THEN
        TLTAXA=TLTAXA-180.0             ! PUTS TLTAXA BETWEEN +/-90.
        TLTAXB=TLTAXB-180.0
        GO TO 96
      ENDIF
97    IF(TLTAXA.LE.-90.0) THEN
        TLTAXA=TLTAXA+180.0             ! PUTS TLTAXA BETWEEN +/-90.
        TLTAXB=TLTAXB+180.0
        GO TO 97
      ENDIF
C
C Now get sign of crystallographic tiltangle.
C  by first calculating whether A_IS_ABOVE the original TLTAXIS on the film.
C
          TLTNORM = TLTAXIS + 90.0      ! TLTNORM now GE.0 and LT.180(see above)
          ANGACOMP= ABS(ANGA-TLTNORM)   ! Should be between 0 and 360
          IF((ANGACOMP.GT.90.0).AND.(ANGACOMP.LT.270.0)) THEN
                AISABOVE = -1.0
          ELSE
                AISABOVE =  1.0
          ENDIF
        SIGNTLTAXA = SIGN(1.0,TLTAXA)
C
        TANGL = (AISABOVE*SIGNTLTAXA*HAND) * TLTANGL ! Here TANGL gets sign. 
C
C Change to true rather than tilted TAXA, TAXB -- i.e. not as on film.
      DENOM = SQRT(1.0-(SIN(TLTAXA*DRAD)*SIN(TLTANGL*DRAD))**2)
      TAXA = RDEG*ACOS((COS(TLTAXA*DRAD))/DENOM)
      TAXA = SIGN(TAXA,TLTAXA)
      DENOM = SQRT(1.0-(SIN(TLTAXB*DRAD)*SIN(TLTANGL*DRAD))**2)
      TAXB = RDEG*ACOS((COS(TLTAXB*DRAD))/DENOM)
      TAXB = SIGN(TAXB,TLTAXB)
C
      WRITE(6,101) ISIZEX,ISIZEY,DSTEP,XMAG,CS,KVOLT
      IF(LISTS)WRITE(10,5001)TLTAXIS,TLTANGL
5001    FORMAT(2F10.5)      
      WRITE(6,102) DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL,
     .          TLTAXA,TLTAXB,TAXA,TAXB,TANGL
101   FORMAT(/,' SIZE OF DENSITOMETERED ARRAY NX.........',I7,/,
     .       '                              NY.........',I7,/,
     .       ' DENSITOMETERED STEPSIZE(MICRONS) .......',F10.2,/,
     .       ' MAGNIFICATION OF MICROGRAPH ............',F8.0,/,
     .       ' SPHERICAL ABERRATION (MM) ..............',F10.2,/,
     .       ' ACCELERATING VOLTAGE (KVOLT) ...........',F8.0)
102   FORMAT(' UNDERFOCUS 1 ...............',F8.0,/,
     .       ' UNDERFOCUS 2 ...............',F8.0,/,
     .       ' DIRECTION FOR UNDERFOCUS 1 .',F9.1,/,
     .       ' TILT AXIS DIRECTION ........',F9.1,/,
     .       ' TILT ANGLE .................',F9.1,
     .  ' +VE FOR LESS UNDERFOCUS AT scan start(Y=0)',/,/,
     .       ' TLTAXA  (On film) ..........',F9.1,/,
     .       ' TLTAXB  (On film) ..........',F9.1,/,
     .       ' TAXA  ( for ORIGTILT) ......',F9.1,
     .  '    These two angles should differ by',/,
     .       ' TAXB  ( for ORIGTILT) ......',F9.1,
     .  '    the correct value of gammastar.',/,
     .       ' TANGL ( for ORIGTILT) ......',F9.1)
C
CHEN
      OPEN(17,FILE='TMP123333.tmp',STATUS='NEW')
      write(17,'(''set refine_defocus = "'',F9.0,'','',F9.0,'','',
     .     F9.2,''"'')')DFMID1,DFMID2,ANGAST
      write(17,'(''set refine_TLTAXIS = "'',F12.4,''"'')')TLTAXIS
      write(17,'(''set refine_TLTANGL = "'',F12.4,''"'')')TLTANGL
      write(17,'(''set refine_TLTAXA = "'',F12.4,''"'')')TLTAXA
      write(17,'(''set refine_TAXA = "'',F12.4,''"'')')TAXA  
      write(17,'(''set refine_TANGL = "'',F12.4,''"'')')TANGL 
      CLOSE(17)
CHEN
      if (IMODE .eq. 0) STOP
C
      ANGAST=ANGAST*DRAD
      TLTAXIS=TLTAXIS*DRAD
      TLTANGL=TLTANGL*DRAD
      CS=CS*(10.0**7.0)
      KVOLT=KVOLT*1000.0
      WL=12.3/SQRT(KVOLT+KVOLT**2/(10.0**6.0))
      WRITE(6,103)WL
103   FORMAT(/,' WAVELENGTH (ANGSTROMS)',F10.4)
      STEPR=DSTEP*(10.0**4.0)/XMAG
C
C
      TRNSTEPX=1.0/(STEPR*ISIZEX)
C  TRNASTEPX IS SIZE OF TRANSFORM GRID POINT IN X DIRECTION     
C
      THETATRX=WL/(STEPR*ISIZEX)
      THETATRY=WL/(STEPR*ISIZEY)
C  THETATRX AND THETATRY ARE DIFFRACTION ANGLES OF FIRST GRID POINTS IN 
C  X AND Y DIRECTIONS OF TRANSFORM.
C
      RESMINSQ=RESMIN**2
      RESMAXSQ=RESMAX**2
C
C      RINNERX = STEPR*ISIZEX/RESMIN
C      ROUTERX = STEPR*ISIZEX/RESMAX
C      ROUTERX = MIN(ROUTERX,NXM1*SQRT(2.0))
C      ROUTERX = ROUTERX * ROUTERX
C      RINNERX = RINNERX * RINNERX
C
      IF(NHOR.GT.IBOXMAX) NHOR = IBOXMAX
      IF(NVERT.GT.IBOXMAX) NVERT = IBOXMAX  
      TURN = .FALSE.    ! Specifies whether desired spot comes from negative X.
C
C
C           grid generation follows
C
      CALL GRID(AX,AY,BX,BY,AYR,BYR,IXC,IYC,IH,IK,XA,YA,NOH,NOK,
     .  RESMINSQ,RESMAXSQ,NXM2,NY2M2,NSPOT,LIST,LISTS,ISHIFT,TRNSTEPX)
C
           DO 120 I=1,NSPOT
                IHTRUE(I)=IH(I)
                IKTRUE(I)=IK(I)
120        CALL FIDDLE(IHTRUE(I),IKTRUE(I),Z,REVHK,SGNXCH,ROT180)
C
      IF(MTZ)CALL GETREFDAT(NSPOT,IH,IK,AREF,PREF,TAXA,TANGL,
     .          REVHK,SGNXCH,ROT180,ISPGRP,IREF,LIST,LISTS,ABANG)
C
C     data read in proceed
C
      IHOR2 = NHOR / 2
      IVERT2 = NVERT / 2
C
C     make sure odd number of elements in box
C
      NHOR = IHOR2 * 2 + 1
      NVERT = IVERT2 * 2 + 1
      WRITE(6,1080) NHOR,NVERT
 1080 FORMAT(/,' Box size in transform grid units :',I3,' *',I3)
C
C  OUTER LOOP FOR NUMBER OF CYCLES OF REFINEMENT.
      DO 8000 ICYC=1,NCYC       ! Total number of cycles allowed.
C
      NAMP=0
      NPHS=0
      PHSRES=0.0
      AMPTOTAL=0.0
      AMPWGTOT=0.0
      WGTOT=0.0
      RMSAMPL=0.0
      CORNUMER=0.0
      CORDENOM1=0.0
      CORDENOM2=0.0
      RFACNUMER=0.0
      RFACDENOM=0.0
      RMSMIN=0.0
      NUMOUT = 0
      NGOOD=0
      NBAD=0
      DO 128 I=1,5
      B(I) = 0.0
      DO 128 J=1,5
128   A(I,J) = 0.0
      DO 129 I=1,9
129   NIQ(I) = 0
      DO 130 K=1,IBOXMAX
      DO 130 J=1,IBOXMAX
      ISUM(J,K) = 0
      ISUMI(J,K) = 0
130   CONTINUE
      IF(LIST)WRITE(6,1103)     !  Asterisks to mark beginning of printout.
C
C  Beginning of Do-loop over all required spots.
C
      DO 500 I=1,NSPOT
      IHOR = NHOR
      IVERT = NVERT
C
C  IHOR,IVERT is required box size to be used.
C  ICTFHOR,ICTFVERT is necessary CTF box size for convolution -- this will
C   get bigger with increasing resolution, becoming its maximum size for big,
C   highly tilted images at high resolution. For example, a 5000x5000 area
C   which is tilted to 60-degrees will require ICTFHOR,ICTFVERT = 100 , if
C   a resolution of 3 Angstroms is desired.  Both of these parameters are 
C   calculated inside subroutine CTFGEN.
C  INHOR,INVERT is then the necessary input box size from the transform needed
C   to carry out the convolution multiplication successfully.
c   It is always bigger than either ctf box or the requested box.
C               INHOR  = IHOR  + ICTFHOR  (- 1)
C               INVERT = IVERT + ICTFVERT (- 1)
C
      CALL CTFGEN(IH(I),IK(I),XA(I),YA(I),THETATRX,THETATRY,
     .    DFMID1,DFMID2,ANGAST,
     .    CS,WL,STEPR,ISIZEX,ISIZEY,
     .    TLTAXIS,TLTANGL,ICTFHOR,ICTFVERT,ACTF,BCTF,
     .    ILIST,DFMID,DELCHI,CTFMID,FACTOR,ISENS,CTFSQ(I),
     .    GRADCTFSQ(1,I),DACTF,DBCTF,FSTORE)
C
      INHOR  = IHOR  + ICTFHOR          ! ODD = ODD +EVEN
      INVERT = IVERT + ICTFVERT         ! ODD = ODD +EVEN
      INHOR2 = INHOR / 2
      INVERT2= INVERT/ 2
C
      IXL = IXC(I) - INHOR2 
      IXR = IXC(I) + INHOR2 
      IYL = IYC(I) - INVERT2 
      IYU = IYC(I) + INVERT2 
C
C     check edge spots
C
      IF(IXL.LT.-NXM1) GO TO 160
      IF(IXR.GT.NXM1) GO TO 160
      IF(IYL.LT.-NY2M1) GO TO 160
      IF(IYU.GT.NY2M1) GO TO 160
      GO TO 180
160     WRITE(6,161) IH(I),IK(I)
161     FORMAT(' SPOT TOO NEAR EDGE FOR CTF TILT CORRECTION',2I8)
        BIM(I)=10000000.0   ! Arbitrarily high rmsbackground to give zero wgt.
        GO TO 500
180   CONTINUE
C
C     set up box edge coordinates
C
C     simple case +ve quadrant
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
     .          IX,IY,AP,BP,DELPX,DELPY,TURN)
      GO TO 280
C
C     simple case -ve quadrant
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
     .          IX,IY,AP,BP,DELPX,DELPY,TURN)
      GO TO 280
      END IF
C
C     complicated cases : spots split about Y axis
C
C     set up LHS of box
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
     .          IX,IY,AP,BP,DELPX,DELPY,TURN)
C
C     set up RHS of box
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
     .          IX,IY,AP,BP,DELPX,DELPY,TURN)
C
280   CONTINUE
C
C     set up array for X and Y axis description coordinates.
C
      KXL = IXL + ICTFHOR/2
      DO 305 J=1,IHOR
      IXGU(J) = KXL
      KXL = KXL + 1
  305 CONTINUE
      KYL = IYL + ICTFVERT/2
      DO 306 J=1,IVERT
      IYGU(J) = KYL
      KYL = KYL + 1
  306 CONTINUE
C
      DO 270 J=3,5      ! First the gradients of the (complex)transform points.
C        CALL SCSL_CONVOLUTE(AP,BP,DACTF(1,1,J),DBCTF(1,1,J),
C     .         DA(1,1,J),DB(1,1,J),AMP,PHI,
C     .         IHOR,IVERT,ICTFHOR,ICTFVERT,0)
      CALL CONVOLUTE(AP,BP,DACTF(1,1,J),DBCTF(1,1,J),
     .          DA(1,1,J),DB(1,1,J),AMP,PHI,
     .          IHOR,IVERT,ICTFHOR,ICTFVERT,0)
 270  continue
        F1 = FSTORE(1)
        F2 = FSTORE(2)
        F3 = FSTORE(3)
      DO 275 K=1,IBOXMAX
      DO 275 L=1,IBOXMAX
        DA(K,L,1) = F1 * DA(K,L,3)
        DB(K,L,1) = F1 * DB(K,L,3)
        DA(K,L,2) = F2 * DA(K,L,3)
        DB(K,L,2) = F2 * DB(K,L,3)
        DA(K,L,3) = F3 * DA(K,L,3)
275     DB(K,L,3) = F3 * DB(K,L,3)
C                       ! Then the convoluted amps and phases themselves.
C      CALL SCSL_CONVOLUTE(AP,BP,ACTF,BCTF,ACORR,BCORR,AMP,PHI,
C     .         IHOR,IVERT,ICTFHOR,ICTFVERT,1)
       CALL CONVOLUTE(AP,BP,ACTF,BCTF,ACORR,BCORR,AMP,PHI,
     .        IHOR,IVERT,ICTFHOR,ICTFVERT,1)
C
C     Arrays AMP, PHI now filled with correct numbers.
C     Now calculate RMS background -- here onwards as in MMBOX.
C       First redo IXL,IXR,IYL,IYR for the desired box.
C
      IXL = IXC(I) - IHOR2 
      IXR = IXC(I) + IHOR2 
      IYL = IYC(I) - IVERT2 
      IYU = IYC(I) + IVERT2 
      AMPSQ = 0.
      KV = IVERT - 1
      KH = IHOR - 1
      DO 290 K=1,IVERT,KV
      DO 290 J=1,IHOR
      F = AMP(J,K)
      AMPSQ = AMPSQ + F * F
  290 CONTINUE
      DO 300 K=2,KV
      DO 300 J=1,IHOR,KH
      F = AMP(J,K)
      AMPSQ = AMPSQ + F * F
  300 CONTINUE
      AMPTOT = AMPSQ / (2*(IHOR + IVERT - 2))
      RMSBK = SQRT(AMPTOT)
C
C     Calculate integrated amplitude
C
      J1 = IHOR / 2
      J2 = J1 + 2
      K1 = IVERT / 2
      K2 = K1 + 2
      AMPSQ = 0.
      DO 310 K=K1,K2
      DO 310 J=J1,J2
      F = AMP(J,K)
      AMPSQ = AMPSQ + F * F
  310 CONTINUE
      AMPTOT = AMPTOT * 9.
      IF(AMPSQ.GE.AMPTOT) THEN
      AMPINT = SQRT(AMPSQ - AMPTOT)
      ELSE
      AMPINT = 0.
      END IF
C
C     Calculate phase from vector sum of phase
C     First find requested centre of box.
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
        DO 311 IVAR = 1,5
        GRADAPART(IVAR)=0.0
        GRADBPART(IVAR)=0.0
311     GRADAMP(IVAR,I) = 0.0
      DO 320 L2 = 1,2   ! Vector phases over 2x2 points only.
      K = K1 + L2 - 1
      DO 320 L1 = 1,2   !
      J = J1 + L1 - 1
      AMPL = AMP(J,K)
      PHASE = PHI(J,K) / 57.2958
      ASUM1 = ASUM1 + AMPL * COS(PHASE)
      BSUM1 = BSUM1 + AMPL * SIN(PHASE)
C
C     calculated sinc function weighted phase
C
      IF(DELX(L1).EQ.0) GO TO 315
      IF(DELY(L2).EQ.0) THEN
      SINC = (SIN(PI * DELX(L1))) / (PI * DELX(L1))
      ELSE
      SINC = (SIN(PI * DELX(L1)) * SIN(PI * DELY(L2))) /
     .                          (PI**2 * DELX(L1) * DELY(L2))
      END IF
      GO TO 318
C
  315 IF(DELY(L2).EQ.0) THEN
      SINC = 1.
      ELSE
      SINC = (SIN(PI * DELY(L2))) / (PI * DELY(L2))
      END IF
C
  318 ASUM2 = ASUM2 + AMPL * COS(PHASE) * SINC
      BSUM2 = BSUM2 + AMPL * SIN(PHASE) * SINC
        DO 312 IVAR=1,5
        GRADAPART(IVAR) = GRADAPART(IVAR) + SINC * DA(J,K,IVAR)
312     GRADBPART(IVAR) = GRADBPART(IVAR) + SINC * DB(J,K,IVAR)
      DENOM = DENOM + SINC**2
  320 CONTINUE
C
      IF(ASUM1.NE.0..OR.BSUM1.NE.0.) THEN 
        VECPHA1 = ATAN2(BSUM1,ASUM1) * 57.2958  ! simple vector sum
        ELSE
        VECPHA1 = 0.
      END IF
      IF(VECPHA1.LT.0.) VECPHA1 = VECPHA1 + 360.
C
      IF(ASUM2.NE.0..OR.BSUM2.NE.0.) THEN 
        VECPHA2 = ATAN2(BSUM2,ASUM2) * 57.2958    ! sinc fn weight vector.
        AMPSINC = SQRT(ASUM2**2 + BSUM2**2)/DENOM ! sinc fn least sq fit.
        IF(AMPSINC.LE.RMSBK) THEN
                AMPOUT = 0.00001
                AINTOUT=AMPSINC**2-RMSBK**2
              ELSE
                AINTOUT=AMPSINC**2-RMSBK**2
                AMPOUT = SQRT(AMPSINC**2 - RMSBK**2)
        ENDIF
      ELSE
        VECPHA2 = 0.
        AMPSINC = 0.00001
        AMPOUT = 0.00001
      END IF
C
        DO 322 IVAR=1,5
        GRADAMP(IVAR,I)=ASUM2*GRADAPART(IVAR)+BSUM2*GRADBPART(IVAR)
322     GRADAMP(IVAR,I)=GRADAMP(IVAR,I)/(AMPSINC*DENOM**2)
C
      IF(VECPHA2.LT.0.) VECPHA2 = VECPHA2 + 360.
      PHSOUT=VECPHA2
        PHSERR = RDEG*RMSBK/AMPOUT
        IQ = 1 + (PHSERR/7.0)           ! THIS MEANS IQ=1 HAS AMP= 8x RMSBK
        IQ = MIN(IQ,8)                  !            IQ=7     AMP= 1x RMSBK
        IF(AMPOUT.EQ.0.00001)IQ=9
      AIM(I) = AMPOUT
      PIM(I) = PHSOUT           ! store current values for ampl, backg & phase.
        PHSHFT = ORIGH*IHTRUE(I) + ORIGK*IKTRUE(I)
        PIM(I) = PIM(I) + PHSHFT        ! APPLY INPUT PHASE ORIGIN SHIFT.
        PIM(I)=AMOD(PIM(I),360.0)
      BIM(I) = RMSBK
      AMPOUT = AMPOUT * FACTOR
      RMSBK  = RMSBK * FACTOR
      AMPINT = AMPINT * FACTOR
      AMPTOTAL = AMPTOTAL + AMPOUT      ! to see if average amplitude increases.
C
C     sum squared amplitudes
C
      DO 350 K=1,IVERT
      DO 350 J=1,IHOR
      IAMP(J,K) = AMP(J,K) * FACTOR + 0.5
      IPHI(J,K) = PHI(J,K) + 0.5
      ISUM(J,K) = ISUM(J,K) + IAMP(J,K) * IAMP(J,K)
  350 CONTINUE
C
C     calculate phase & amplitude @ requested point by linear
C     interpolation
C     calculate weights for interpolated phase angle
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
      CALL  ANGAVE(NPHI,PHANG,WTS,PINTP,GDMEAN) 
C
        IF(IQ.LE.7) NGOOD=NGOOD+1
        IF(IQ.GT.7) NBAD=NBAD+1
        NIQ(IQ) = NIQ(IQ)+1
C
C     set up pagination
C
      NUMOUT = NUMOUT + 1
          IF(NUMOUT.GE.NUMSPOT) ILIST=.FALSE.
          IF(NUMOUT.EQ.NUMSPOT+1) THEN
                IF(LIST) WRITE(6,1103)
                IF(LIST) WRITE(6,1102)
          ENDIF
1102   FORMAT(/,' OTHER SPOTS NOT PRINTED OUT WITH FULL DIAGNOSTICS')
1108   FORMAT('   H   K  AMPOUT  PHSOUT IQ   RMSBK     DFMID    ',
     .   'NCTFSAMPLES    CTFINMIDDLE   RESCALING BY')
       IF(LISTS)WRITE(10,5002)IH(I),IK(I),AMPOUT,AINTOUT,PHSOUT,IQ,
     .RMSBK,CTFSQ(I),FACTOR
5002  FORMAT(2I5,F12.5,F20.5,F9.3,I5,F10.5,F10.5,F10.5)     
         IF(NUMOUT.GT.NUMSPOT) THEN
          NUMAFTER=NUMOUT-NUMSPOT-1     ! Test for table heading printout.
         IF(60*((NUMAFTER)/60).EQ.NUMAFTER.AND.LIST) WRITE(6,1108)
           IF(LIST)WRITE(6,1101)IH(I),IK(I),ISENS,AMPOUT,
     .     PHSOUT,IQ,RMSBK,DFMID,DELCHI,ICTFHOR,CTFMID,FACTOR
          ENDIF
1101   FORMAT(2I4,A1,F7.1,F8.1,I3,F8.1,F10.1,F12.2,
     .  '(',I2,')',F11.4,F12.3)
      IF(NUMOUT.GT.NUMSPOT) GO TO 500
C
C     write up to NUMSPOT spots
C
      WRITE(6,1105) IH(I),IK(I),XA(I),YA(I)
1103  FORMAT(/,/,132('*'),/)
1105  FORMAT(/,' Reflection  H',I3,'  K',I3,10X,
     1       'Lattice coordinates in grid units ',2F8.2)
C
      WRITE(6,1110) RMSBK, AMPINT, VECPHA1, VECPHA2, PINTP, GDMEAN
1110  FORMAT(/,' RMS backgd =',F6.1,
     . ' Integrated bgd-corr amp over 3x3 box =',F6.1,/,
     . '  Ampl-weighted vec. sum of phase =',F6.1, 
     . '  sinc func-weighted vec. sum of phase =',F6.1, 
     .       ' Interp. phase/goodness=',2F6.1) 
      WRITE(6,1121) AMPOUT,PHSOUT,IQ
1121  FORMAT(' Amplitude, phase and IQ at this stage =',2F8.1,I3)
      IF(IHOR.GT.10) GO TO 400
C  
C     amps & phases side by side
C
      WRITE(6,1120)
 1120 FORMAT(/,/,19X,'Amplitudes',65X,'Phases')
      WRITE(6,1140) (IXGU(J),J=1,IHOR)
 1140 FORMAT(/,'  X(grid units)',21I5)
      WRITE(6,1150) (IXGU(J),J=1,IHOR)
 1150 FORMAT('+',67X,'X(grid units)',10I5)
      WRITE(6,1160)
 1160 FORMAT(/,'  Y(grid units)',53X,'Y(grid units)')
      L = IVERT
      DO 390 K=1,IVERT
      WRITE(6,1170) IYGU(L),(IAMP(J,L),J=1,IHOR)
 1170 FORMAT(/,6X,I5,4X,21I5)
      WRITE(6,1180) IYGU(L),(IPHI(J,L),J=1,IHOR)
 1180 FORMAT('+',72X,I5,4X,10I5)
      L = L - 1
  390 CONTINUE
      IF(ILIST)WRITE(6,1103)    ! Asterisks.
      GO TO 500
C
C     write amps first then phases
C
  400 WRITE(6,1200)
 1200 FORMAT(/,/,19X,'Amplitudes')
      WRITE(6,1140) (IXGU(J),J=1,IHOR)
      WRITE(6,1210)
 1210 FORMAT(/,'  Y(grid units)')
      L = IVERT
      DO 420 K=1,IVERT
      WRITE(6,1170) IYGU(L),(IAMP(J,L),J=1,IHOR)
      L = L - 1
  420 CONTINUE
      WRITE(6,1230)
 1230 FORMAT(/,19X,'Phases')
      WRITE(6,1140) (IXGU(J),J=1,IHOR)
      WRITE(6,1210)
      L = IVERT
      DO 440 K=1,IVERT
      WRITE(6,1170) IYGU(L),(IPHI(J,L),J=1,IHOR)
      L = L - 1
  440 CONTINUE
      IF(ILIST)WRITE(6,1103)    !   Asterisks
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
      ISUM(J,K) = SQRT(FLOAT(ISUM(J,K)) /(NSPOT)) + 0.5
  520 CONTINUE
C
      WRITE(6,1250)     ! Amplitude printout.
 1250 FORMAT('1',131('*'),/,1X,'SQRT of summed,squared amplitudes',
     .  /,'$',18X,33('-'))
      L = IVERT
      DO 540 K=1,IVERT
      WRITE(6,1270) (ISUM(J,L),J=1,IHOR)
 1270 FORMAT(1X,21I5,/)
      L = L - 1
  540 CONTINUE
      WRITE(6,1280) AMPTOTAL/NSPOT
1280  FORMAT(18X,'overall average unweighted amplitude =',F10.3)
C
      SCALEFAC = 7.0/PERIM
      WRITE(6,1251) SCALEFAC            ! Intensity printout.
 1251 FORMAT(132('*'),/,1X,'scaled intensities (perimeter',
     .  ' averaged to 7.0)',/,
     .  '        scale factor = ',F12.7,/,'$',1X,40('-'))
C
C  Here calculate how close the intensity average approaches theoretical.
C
      ICENTRE=ISUMI(IHOR2+1,IVERT2+1)
      INEXT1=0
      DO 1260 IX=IHOR2,IHOR2+2
      DO 1260 IY=IVERT2,IVERT2+2
1260  INEXT1=INEXT1+ISUMI(IX,IY)
      INEXT2=0
      IF(IHOR.GE.5.AND.IVERT.GE.5) THEN
        DO 1262 IX=IHOR2-1,IHOR2+3
        DO 1262 IY=IVERT2-1,IVERT2+3
1262    INEXT2=INEXT2+ISUMI(IX,IY)
      ENDIF
      INEXT3=0
      IF(IHOR.GE.7.AND.IVERT.GE.7) THEN
        DO 1264 IX=IHOR2-2,IHOR2+4
        DO 1264 IY=IVERT2-2,IVERT2+4
1264    INEXT3=INEXT3+ISUMI(IX,IY)
      ENDIF
      IALL=0
        DO 1266 IX=1,IHOR
        DO 1266 IY=1,IVERT
1266    IALL=IALL+ISUMI(IX,IY)
      ICENTRE=ICENTRE-7
      INEXT1=INEXT1-9*7
      INEXT2=INEXT2-25*7
      INEXT3=INEXT3-49*7
      IALL=IALL-IHOR*IVERT*7
C
      L = IVERT
      DO 545 K=1,IVERT
      WRITE(6,1270) (ISUMI(J,L),J=1,IHOR)
      L = L - 1
  545 CONTINUE
      PC=FLOAT(ICENTRE)*100.0/IALL
      PC1=FLOAT(INEXT1)*100.0/IALL
      PC2=FLOAT(INEXT2)*100.0/IALL
      PC3=FLOAT(INEXT3)*100.0/IALL
      WRITE(6,1254)IALL,ICENTRE,PC,INEXT1,PC1,INEXT2,PC2,INEXT3,PC3
1254  FORMAT(5X,'above box contains total intensity above background',
     .  ' of',I7,/,
     .  9X,'of which',I6,' (',F6.2,' %) is at centre',/,
     .  9X,'     and',I6,' (',F6.2,' %) is within 3x3 box',/,
     .  9X,'     and',I6,' (',F6.2,' %) is within 5x5 box',/,
     .  9X,'     and',I6,' (',F6.2,' %) is within 7x7 box')
      WRITE(6,1253) NUMOUT,NGOOD,NBAD,(NIQ(J),J=1,9)
1253  FORMAT(/,1X,I8,' Total spots,',I5,' Good,',I5,' Bad',/,/,
     .' IQ  =         1     2     3     4     5     6',
     .'     7     8     9',/,
     .' NUMBER',3X,9I6,/)
      J99=99
      DUMMY=0.0
      IF(LISTS)WRITE(10,5002)J99,J99,DUMMY,DUMMY,DUMMY,J99,DUMMY,
     .DUMMY,DUMMY
C
      CALL SCALENEW(NSPOT,IH,IK,AIM,BIM,AREF,AREFSCALE,CTFSQ,LISTS)
C
C  Set up matrices for refinement.
      DO 7000 I=1,NSPOT
                IF(AREF(I).EQ.-999.) GO TO 7000
                NAMP=NAMP+1
                IF(PREF(I).NE.-999.) THEN
                        DP=PIM(I)-PREF(I)
                        DP=AMOD(DP,360.0)
                        IF(DP.GT.180.0) DP=DP-360.0
                        IF(DP.LT.-180.0) DP=DP+360.0
                        PHSRES=PHSRES+ABS(DP)
                        NPHS=NPHS+1
                ENDIF
                CORNUMER=CORNUMER+AIM(I)*AREFSCALE(I) *
     .                                  CTFSQ(I)/BIM(I)
                CORDENOM1=CORDENOM1+(AIM(I)/BIM(I))**2
                CORDENOM2=CORDENOM2+(AREFSCALE(I)*CTFSQ(I))**2
        RFACNUMER=RFACNUMER+ABS(AIM(I)-AREFSCALE(I)*CTFSQ(I))
     .                                          /BIM(I)
        RFACDENOM=RFACDENOM+AIM(I)/BIM(I)
C
        WGT=1.0/(0.1*AIM(I)**2 + BIM(I)**2)
C  Gives equal (high)weight for all spots with IQ values better than 2.5.
        RMSMIN=RMSMIN+WGT*(AIM(I)-AREFSCALE(I)*CTFSQ(I))**2
        AMPWGTOT = AMPWGTOT + WGT*AIM(I)
        RMSAMPL  = RMSAMPL+ WGT*AIM(I)**2
        WGTOT    = WGTOT + WGT
      IF (MAXIM) THEN
        IF (MTZ) THEN
                C0=AREFSCALE(I)*CTFSQ(I)
        ELSE
                C0=AIM(I)
        ENDIF
        DO 6800 J=1,5
6800    CJ(J)=GRADAMP(J,I)
      ELSE
        IF(.NOT.MTZ) STOP
        C0=AREFSCALE(I)*CTFSQ(I)-AIM(I)
        DO 6900 J=1,5
6900    CJ(J)=GRADAMP(J,I) - AREFSCALE(I)*GRADCTFSQ(J,I)
      ENDIF
      DO 6950 J=1,5
      B(J) = B(J) + WGT*C0*CJ(J)
      DO 6950 K=1,5
6950  A(J,K) = A(J,K) + WGT*CJ(J)*CJ(K)
C       WRITE(6,*)I,IH(I),IK(I),AIM(I),BIM(I),PIM(I)
C       WRITE(6,*)AIM(I),AREFSCALE(I),CTFSQ(I)
C       WRITE(6,*) (AREFSCALE(I)*GRADCTFSQ(J,I),J=1,5)
C       WRITE(6,*) (GRADAMP(J,I),J=1,5)
7000  CONTINUE
C
        AMPWGTOT=AMPWGTOT/WGTOT
        RMSAMPL=SQRT(RMSAMPL/WGTOT)
      WRITE(6,7005) AMPWGTOT
7005  FORMAT(19X,'Overall average weighted amplitude =',F10.4)
      WRITE(6,1255) RMSAMPL
1255  FORMAT(19X,    'Overall rms weighted amplitude     =',F10.4)
C
      CALL MATRIXINV(IMODE,A,B)         ! shifts returned in B
C
        CORREL=CORNUMER/SQRT(CORDENOM1*CORDENOM2)
        RFAC = RFACNUMER/RFACDENOM
        RMSMIN = SQRT(RMSMIN/NAMP)
        IF(NPHS.NE.0) PHSRES = PHSRES/NPHS
      WRITE(6,748)
      WRITE(6,871) ICYC,(PARAMS(J),J=1,2),(RDEG*PARAMS(J),J=3,5),
     .  (B(J),J=1,2),(RDEG*B(J),J=3,5),PHSRES,NPHS,
     .  RFAC,RMSMIN,CORREL,NAMP
748   FORMAT(/,'  ICYC  DFMID1  DFMID2  ANGAST TLTAXIS TLTANGL',
     .  '  SHFT1  SHFT2   SHFT3   SHFT4',/,'   SHFT5 PHSRES NPHS ',
     . ' RFAC   RMSMIN  CORREL NAMP')
871   FORMAT(' ZZ',I3,2F8.0,3F8.2,2F7.0,2F8.2,/,
     .F8.2,F7.2,I5,F7.4,F8.4,F8.5,I5)
C
CHEN
C
      if (ICYC .EQ. 1) then
        OPEN(18,FILE='TMP124444.tmp',STATUS='NEW')
        WRITE(18,873)
      endif
873   FORMAT(':  ICYC  DFMID1  DFMID2  ANGAST TLTAXIS TLTANGL',
     .  '  SHFT1  SHFT2   SHFT3 ')
      WRITE(18,874) ICYC,(PARAMS(J),J=1,2),(RDEG*PARAMS(J),J=3,5),
     .  (B(J),J=1,2),(RDEG*B(3))
874   FORMAT(':',2X,I3,2F8.0,3F8.2,2F7.0,F8.2)
C
CHEN
C
C  Limit shifts of defocus to 500 Angstrom, axes to 10 deg, angle to 2 deg.
C
      IF(ICYC.GE.3) THEN
        IF(IMODE.LE.2.AND.B(1).NE.SIGN(B(1),BOLD(1)).AND.
     .                    B(2).NE.SIGN(B(2),BOLD(2))) THEN      
                FSHIFT=0.5*FSHIFT
                WRITE(6,869)FSHIFT
869     FORMAT(' Oscillation - FSHIFT dropped to',F8.5)
        ENDIF
      ENDIF
      DO 870 J=1,5
      BOLD(J)=B(J)
      IF(ABS(B(J)*FSHIFT).GT.PARAMSLIM(J)) THEN
          ARG1 = PARAMSLIM(J)
          ARG2 = B(J)
        APPLYSHFT(J) = SIGN(ARG1,ARG2)
      ELSE
        APPLYSHFT(J) = B(J)*FSHIFT
      ENDIF
        PARAMS(J) = PARAMS(J) + APPLYSHFT(J)
870   CONTINUE
          WRITE(6,872)(APPLYSHFT(J),J=1,2),(RDEG*APPLYSHFT(J),J=3,5)
872       FORMAT(25X,'applied shifts were  ',2F7.0,3F8.2)
C
C  Enter data into summary table.
      IF (ICYC.LE.25) THEN
        ISUMMARY(ICYC,1)=ICYC
        ISUMMARY(ICYC,2)=NGOOD
        SUMMARY(ICYC,1)=AMPWGTOT
        SUMMARY(ICYC,2)=RMSAMPL
        SUMMARY(ICYC,3)=B(1)
        SUMMARY(ICYC,4)=B(2)
        SUMMARY(ICYC,5)=B(3)*RDEG
        SUMMARY(ICYC,6)=PHSRES
        SUMMARY(ICYC,7)=RFAC
        SUMMARY(ICYC,8)=RMSMIN
      ENDIF
      IF(ABS(B(1)).GT.7.0) GO TO 8000
      IF(ABS(B(2)).GT.7.0) GO TO 8000
      IF(ABS((DFMID1-DFMID2)*B(3)).GT.7.0) GO TO 8000   
C      IF(ABS(B(3)).GT.0.10*DRAD) GO TO 8000    ! ANGLES MUST BE IN RADIANS
      IF(ABS(B(4)).GT.0.10*DRAD) GO TO 8000     !
      IF(ABS(B(5)).GT.0.10*DRAD) GO TO 8000     !
      WRITE(6,8003)
8003  FORMAT(/,'    REFINEMENT CONVERGED, SHIFTS SMALLER THAN PRESET',
     .  ' PROGRAM LIMITS')
      GO TO 8001
8000  CONTINUE
        WRITE(6,8004) NCYC
8004   FORMAT(/,'  END OF FIXED NUMBER OF CYCLES, NCYC =',I5)
C
C  REFINEMENT CONVERGED OR END OF FIXED NUMBER OF CYCLES
8001  WRITE(6,8002) DFMID1,DFMID2,ANGAST*RDEG,TLTAXIS*RDEG,TLTANGL*RDEG
8002  FORMAT(': Final parameters ......',/,
     .':',25X,'DFMID1  =',F9.0,/,
     .':',25X,'DFMID2  =',F9.0,/,
     .':',25X,'ANGAST  =',F9.2,/,/,
     .':',25X,'TLTAXIS =',F9.2,/,
     .':',25X,'TLTANGL =',F9.2,/)
      WRITE(6,8006)
8006  FORMAT(/,/,/,/,':  Summary of refinement .......',/,/,
     .': CYC NGOOD WEIGHTAMP    RMSAMP SHIFT1 SHIFT2  SHIFT3',
     .' PHSRES   RFAC  RMSMIN',/)
8005  FORMAT(':',I4,I6,2F10.4,2F7.0,F8.2,F7.2,F7.4,F8.4)
        MCYC = MIN(ICYC,IDSUM,NCYC)
      DO 8007 I=1,MCYC
8007  WRITE(6,8005) (ISUMMARY(I,J),J=1,2),(SUMMARY(I,J),J=1,8)
      WRITE(6,8008)
8008  FORMAT(/,/,/,/)
C
CHEN
      CLOSE(18)
CHEN
C
      STOP
CHEN
        write(18,
     1 '(''ERROR: Header NX,NY differ from ISIZEX,ISIZEY'',4I8)')
     2  ISIZEX,ISIZEY,NX,NY
        close(18)
CHEN
C
6001   WRITE(6,6002)ISIZEX,ISIZEY,NX,NY
6002   FORMAT(':: Header NX,NY differ from ISIZEX,ISIZEY',4I8)
      STOP 
      END
C
C*******************************************************************************
C
      SUBROUTINE GRID(AX,AY,BX,BY,AYR,BYR,IXC,IYC,IH,IK,XA,YA,NOH,NOK,
     .  RESMINSQ,RESMAXSQ,NXM2,NY2M2,NSPOT,LIST,LISTS,ISHIFT,TRNSTEPX)
C
C     subroutine to generate a lattice from 1,0 & 0,1 coordinates
C
      PARAMETER (NMAX=1500)
      DIMENSION IH(NMAX),IK(NMAX),IXC(NMAX),IYC(NMAX),
     1          XA(NMAX),YA(NMAX)
      LOGICAL ISHIFT,LIST,LISTS
      IF(LIST)WRITE(6,10)
   10 FORMAT(/,' Lattice generated coordinates',/,8X,'H',9X,'K',
     1       7X,'X',9X,'Y',/,'0')
C
      TRNSTEPXSQ=TRNSTEPX**2
      NOHD = 2 * NOH + 1
      NOKD = 2 * NOK + 1
      NSPOT = 0
      DO 100 NH=1,NOHD
      DO 100 NK=1,NOKD
        JH = NH - NOH - 1
        JK = NK - NOK - 1
        X = JH * AX + JK * BX
        Y = JH * AY + JK * BY
       YC = JH * AYR + JK * BYR
      IF(ISHIFT)THEN
C       SHIFT LATTICE POINT TO BETWEEN REAL LATTICE POINTS FOR CONTROLS
          X = X + ( AX + BX )/3.0
          Y = Y + ( AY + BY )/3.0
      END IF    
        IF(Y.LT.0.) GO TO 100
C
C       Resolution calculated from X and YC
C
      DSTARSQ=(X**2+YC**2)*TRNSTEPXSQ
C
C       DSTAR = X * X + (Y * Y / RATIOYX**2)    !normalise to x-scale for resol
C       IF(DSTAR.GT.ROUTERX) GO TO 100
C       IF(DSTAR.LT.RINNERX) GO TO 100
C
      IF(DSTARSQ.EQ.0.0)GO TO 100
      DSQ=1.0/DSTARSQ
      IF(DSQ.LT.RESMAXSQ.OR.DSQ.GT.RESMINSQ)GO TO 100
C
        IF(ABS(NINT(X)).GT.NXM2.OR.ABS(NINT(Y)).GT.NY2M2) GO TO 100
C
C     spot within radius criterion  and within box.
C
        NSPOT = NSPOT + 1
         IF (NSPOT.GT.NMAX) GO TO 4550
        IXC(NSPOT) = X + SIGN(0.5,X)
        IYC(NSPOT) = Y + SIGN(0.5,Y)
        XA(NSPOT) = X
        YA(NSPOT) = Y
        IH(NSPOT) = JH
        IK(NSPOT) = JK
        IF(LIST)WRITE(6,20) JH,JK,X,Y
      IF(LISTS)WRITE(10,20)JH,JK,X,Y
   20   FORMAT(2I10,2F10.1)
  100 CONTINUE
      WRITE(6,4552)NSPOT
4552  FORMAT(/,'  THERE WERE A TOTAL OF',I5,'  SPOTS GENERATED',/)
      J99=99
      DUMMY=99.0
      IF(LISTS)WRITE(10,20)J99,J99,DUMMY,DUMMY
      RETURN
4550    WRITE(6,4551) NMAX
4551    FORMAT(' TOO MANY SPOTS FOR CURRENT PROG DIMENSION',I5)
       STOP
      END
C
C*******************************************************************************
C
      SUBROUTINE AMPHA(IX,IY,APART,BPART,AMP,PHASE,DELPX,DELPY)
C
C     subroutine to translate APART,BPART into amplitude & phase in 
C     degrees and apply origin phase shift to APART,BPART,AMP and PHASE.
C
      PSHIFT = IX * DELPX + IY * DELPY
      IF(IX.LT.0) PSHIFT = - PSHIFT
      C = COS(PSHIFT)
      S = SIN(PSHIFT)
      A = APART * C - BPART * S
      B = APART * S + BPART * C
      IF(IX.LT.0) B = - B
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
     .          IX,IY,AP,BP,DELPX,DELPY,TURN)
C
C     subroutine to read part of section required plus the extra area needed
C     for the ctf-dependent convolution, then store in array AP,BP for return
C     to main program ---- phases are corrected to desired phase origin.
C
      PARAMETER (INBOXMAX=361)
      DIMENSION ARRAY(2*INBOXMAX,INBOXMAX)      ! Square array of complex no's.
      DIMENSION AP(INBOXMAX,INBOXMAX),BP(INBOXMAX,INBOXMAX)
      LOGICAL TURN
      CALL  IRDPAS(1,ARRAY,2*INBOXMAX,INBOXMAX,IX1,IX2,IY1,IY2,*900)
      CALL  IMPOSN(1,0,0)
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
      CALL  AMPHA(IX,IY,APART,BPART,AMP,PHASE,DELPX,DELPY)
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
      CALL  AMPHA(IX,IY,APART,BPART,AMP,PHASE,DELPX,DELPY)
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
        SUBROUTINE ANGAVE(N, THETAS, WEIGHTS, THMEAN, THGOOD)
C       Function: to average a set of angles in degrees
C       Created: 27/7/84 by D.J.Thomas
C       Modified:  by R.HENDERSON 20.5.85
        INTEGER*4       N               !number of input angles
        REAL*4          THETAS(1)       !array of input angles
        REAL*4          THGOOD          !goodness of average (0 to 1)
        REAL*4          THMEAN          !weighted average of input angles
        REAL            COMEAN          !mean value of cosines
        REAL            SIMEAN          !mean value of sines
        REAL            WEIGHT          !total input weight
        REAL*4          WEIGHTS(1)      !weights on input angles
C
        IF (N .LE. 0) GO TO 20
        WEIGHT = 0.0
        COMEAN = 0.0
        SIMEAN = 0.0
        DO 10 I=1,N
        WEIGHT = WEIGHT + WEIGHTS(I)
        COMEAN = COMEAN + (COS(THETAS(I)*0.01745329252)*WEIGHTS(I))
        SIMEAN = SIMEAN + (SIN(THETAS(I)*0.01745329252)*WEIGHTS(I))
10      CONTINUE
        IF ((SIMEAN .EQ. 0.0) .AND. (COMEAN .EQ. 0.0)) GO TO 20
        THMEAN = 57.295779513*ATAN2(SIMEAN,COMEAN)
        IF(THMEAN.LT.0.0) THMEAN=THMEAN+360.0
        IF (WEIGHT .EQ. 0.0) GO TO 20
        THGOOD = SQRT((SIMEAN*SIMEAN) + (COMEAN*COMEAN))/WEIGHT
        RETURN
20      THGOOD = 0.0                    !average is undefined
        RETURN
        END
C
C*******************************************************************************
      SUBROUTINE GETREFDAT(NSPOTS,IHIN,IKIN,AREF,PREF,TAXA,TANGL,
     .  REVHK,SGNXCH,ROT180,ISPGRP,IREF,LIST,LISTS,ABANG)
C
C##############################################################################
C  18.8.84 ############  IMPORTANT CHANGE #####################################
C            THE MATRICES IMAT, MAT, IGO HAVE BEEN CHANGED, TOGETHER WITH THE 
C            LREV TEST IN ASYM SO THAT THE CONVENTION IN P4, P3, AND P6 IS
C            FOR THE AXIAL INDICES TO BE H,0 RATHER THAN 0,K.
C
C  PROGRAM MUST NOW BE LINKED USING COMMAND
C         :-   PIMLINK ORIGTILT
C               (INCLUDES MTZLIB,IMLIB,MODLIB,PLOT82, (LIBRARIES) ETC)
C
C##############################################################################
C
C SPACE GROUP MATRICES --- convention for p3, p4 and p6 is H,0 (not 0,K).
      INTEGER*2 ISPEC(5,17)
      DATA ISPEC/7*0,1,3*0,1,4*0,1,4*0,1,3*0,3*1,2*0,3*1,0,-1,3*1,0,1,
     A 3*1,4*0,1,0,0,4*1,0,5*1,8*0,1,0,1,1,5*0,1,4*0,1,1,0/
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
      LOGICAL LREV(17)
      DATA LREV/9*.FALSE.,.TRUE.,2*.FALSE.,.TRUE.,2*.FALSE.,.TRUE.,
     1          .FALSE./
      REAL STANG(17)            ! STANDARD SPACE GROUP ANGLES.
      DATA STANG/2*0.0,10*90.0,5*120.0/
C        ANGLE BETWEEN A AND B FOR ALL SPACEGROUPS EXCEPT P1, P2 IS FIXED.
C
C        THE ABOVE MATRICES ARE USED BY ASYM TO TRANSFORM ALL REFLECTIONS
C           TO THE STANDARD ASYMMETRIC UNIT AND TO PICK OUT THE SPECIAL
C           REFLECTIONS.
C
C
C     NUMBER   SPACEGROUP    ASYMMETRIC UNIT        REAL   IMAGINARY
C
C          1          P1         H>=0
C
C          2         P21         H,Z>=0              Z=0
C
C          3         P12         H,K>=0              K=0
C
C          4        P121         H,K>=0              K=0
C
C          5         C12         H,K>=0              K=0
C
C          6        P222         H,K,Z>=0            H=0
C                                                    K=0
C                                                    Z=0
C
C          7       P2221         H,K,Z>=0          (0,2N,Z)  (0,2N+1,Z)
C                                                    (H,K,0)
C                                                    (H,0,Z)
C
C          8      P22121         H,K,Z>=0            (H,K,0)
C                                                   (2N,0,Z)  (2N+1,0,Z)
C                                                   (0,2N,Z)  (0,2N+1,Z)
C
C          9        C222         H,K,Z>=0            (H,K,0)
C                                                    (H,0,Z)
C                                                    (0,K,Z)
C
C         10          P4         H,K,Z>=0            (H,K,0)
C
C         11        P422         H,K,Z>=0            (H,K,0)
C                                K>=H                (H,0,Z)
C                                                    (0,K,Z)
C                                                    (H,H,Z)
C
C         12       P4212         H,K,Z>=0            (H,K,0)
C                                K>=H                (H,H,Z)
C                                                   (2N,0,Z)   (2N+1,0,Z)
C                                                   (0,2N,Z)   (0,2N+1,Z)
C
C         13          P3         H,K>=0
C
C         14        P312         H,K>=0              (H,H,Z)
C                                K>=H
C
C         15        P321         H,K>=0              (H,0,Z)
C                                 K>H                (0,K,Z)
C
C         16          P6       H,K,Z>=0             (H,K,0)
C
C         17        P622         H,K,Z>=0            (H,K,0)
C                                K>=H                (H,H,Z)
C
C******************************************************************************
      PARAMETER (NMAXC=150000)
      INTEGER TOTRFL
      PARAMETER (TOTRFL=300)
      PARAMETER (MAXINDEX=40)
      PARAMETER (MAXTWOP1SQ=(2*MAXINDEX+1)**2)
C
C----------------------------------------------------------------from here
C  DIMENSION STATEMENTS FOR IREF=1 MTZ DATA INPUT
      INTEGER*2 IHC(NMAXC),IKC(NMAXC),ILC(NMAXC),
     .  ISC(NMAXC),IFCC(NMAXC),IPHC(NMAXC),IFOM(NMAXC),
     .  IBEGIN(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),
     .  IFINISH(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),
     .  IBEGINPH(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),
     .  IFINISHPH(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX)
      DATA IBEGIN/MAXTWOP1SQ*-999/,IFINISH/MAXTWOP1SQ*-999/
      DATA IBEGINPH/MAXTWOP1SQ*-999/,IFINISHPH/MAXTWOP1SQ*-999/
C
C     .. parameters for mtz aspects
      PARAMETER (NLOC=40)
      PARAMETER (MCOLS=200)
      PARAMETER (NPAR=200)
C
      DIMENSION CELL(6),RSYMX(4,4,96)
      LOGICAL EOF
C
C     .. Local Arrays ..
      REAL ADATAIN(MCOLS),ADATAOUT(MCOLS),DUM(2,MCOLS)
      INTEGER JPOINT(NLOC),LOOKUP(NLOC)
      CHARACTER OUTTYP(NLOC)*1,LSPRGI(NLOC)*30,LSPRGO(NLOC)*30,
     +  TITNEW*70,HISNEW(20)*80,CTPRGI(NLOC)*1,DUMMY*10
C
C     .. Scalars for Parser ..
      INTEGER NTOK
      LOGICAL LEND
      CHARACTER KEY*4,LINE*400
C     ..
C     .. Arrays for Parser ..
      REAL FVALUE(NPAR)
      INTEGER IBEG(NPAR),IDEC(NPAR),IEND(NPAR),ITYP(NPAR)
      CHARACTER CVALUE(NPAR)*4
C
C
C---- NLPRGI  =  number of input labels
      DATA NLPRGI,LSPRGI/7,'H','K','L','AMP','SIG','PHASE',
     +     'FOM',33*' '/
C---- .. This code signs which input columns are essential (LOOKUP)
      DATA CTPRGI/'H','H','H','F','Q','P','W',33*' '/
      DATA LOOKUP/-1,-1,-1,-1,-1,-1,-1,33*0/
      DATA JPOINT/40*0/
C----------------------------------------------------------------to here
C
C
C  DIMENSION STATEMENTS FOR IREF=0 INPUT OF UNTILTED PROJECTION DATA.
      INTEGER*2 JH(TOTRFL),JK(TOTRFL)
      REAL PHS(TOTRFL),AMP(TOTRFL),ZSTAR(TOTRFL)
C
      DIMENSION IHIN(1),IKIN(1),AREF(1),PREF(1)
      INTEGER      IPTEST
      INTEGER*2    IP1,IP2
      LOGICAL      LSPEC        ! TRUE IF PHASE IS RESTRICTED BY SYMMETRY.
C
      LOGICAL IOK
      DATA JREFL/0/             ! JREFL IS COUNT ON UNTILTED REFERENCE DATA.
      DATA DRAD,RDEG/0.0174532,57.295779/
C
      LOGICAL LIST,LISTS
      CHARACTER*4 INOTE
C
      INTEGER * 8 ISER
C
      SMIN=40000.
      SMAX=0.
C
CHEN>
      if(ISPGRP.ne.1)then
        ABANG=STANG(ISPGRP)
        ABANG=180.-ABANG
      endif
      write(6,'('' ABANG = '',F12.4)')ABANG
CHEN<
      WRITE(6,115)ISPGRP
115   FORMAT('  TWO SIDED PLANE GROUP ',I3,/,/)
C
C     READ REFERENCE DATA
C      (FOR IREF.EQ.0) THIS DATA SHOULD BE UNTILTED AND ONLY THE ASYMMETRIC
C           UNIT SHOULD BE INPUT WITH ALL REFLECTIONS ON THE PROPER PHASE
C           ORIGIN.
C      (FOR IREF.EQ.1) THE FIRST DATASET IS A FULLY-FLEDGED MTZ FILE.
C
        WRITE(6,125)
      IF(IREF.EQ.0) THEN
                CALL CCPDPN(2,'INREF','READONLY','F',0,0)
C               CALL DOPEN(2,'INREF','RO','F')
                READ(2,*)ISER
201             IF(LIST)WRITE(6,130)            ! THIS IF IREF.EQ.0
130             FORMAT(/,/,'    UNTILTED REFERENCE IMAGE (NON-MTZ)   ',/,/,/)
                DO 200 I=1,TOTRFL+1
                        READ(2,*,END=210)IH,IK,A,P      ! UNTILTED DATA
                        IF(IH.GE.900) GO TO 210
                        JREFL=JREFL+1
                        JH(JREFL)=IH
                        JK(JREFL)=IK
                        ZSTAR(JREFL)=0.0
                        AMP(JREFL)=A
                        PHS(JREFL)=P
215                     IF(LIST)WRITE(6,145)IH,IK,ZSTAR(JREFL),PHS(JREFL),A
145                     FORMAT(2I5,F10.4,2F10.1)
200             CONTINUE
                  WRITE(6,148) TOTRFL
148               FORMAT(/,/,/,'0MORE THAN',I5,' REFLECTIONS FOR THIS IMAGE')
                  STOP
210             CONTINUE
                CLOSE(2)                ! Alf1
                WRITE(6,211)JREFL
211             FORMAT(I6,' UNTILTED REFERENCE REFLECTIONS READ IN')
        GO TO 220
      ELSE
C
C  here only if IREF.eq.1 -- i.e. for mtz file input
C  REFERENCE CURVE INPUT TO STORAGE
C  INPUT OF AMPL & PHASES FROM MTZ FILE.
C  USE POINTERS TO INDICATE THE BEGINNING OR END OF EACH LATTICE LINE.
C  NOTES:-   THE FORMULA FOR CALCULATION OF PHASE AT AN ARBITRARY ZSTAR
C            POSITION DOES NOT TREAT THE SYMMETRY OF SPACE GROUPS WITH
C            LATTICE LINES FOR WHICH ZSTAR IS ONLY POSITIVE PROPERLY.
C
                WRITE(6,197)
197             FORMAT(' ****  REFERENCE MTZ CURVE INPUT BEGINNING  ****')
                CALL CCPFYP
                CALL MTZINI
                CALL LROPEN(1,'HKLIN',3,IERR)
                CALL LRCELL(1,CELL)
                CALL LRSYMM(1,NSYMX,RSYMX)
                IF(IERR.NE.0) THEN
                        WRITE (6,11006)IERR
11006                   FORMAT(' ERROR ON INPUT OF MTZ FILE, IERR=',I5)
                        STOP
                ENDIF
C--------- Find out how many columns and reflections in input file
                CALL LRINFO(1,DUMMY,NCOL,NREF,DUM)
                CALL LKYASN(1,NLPRGI,LSPRGI,CTPRGI,LOOKUP)
C
                IF(ISPGRP.GE.3.AND.STANG(ISPGRP).NE.CELL(6)) THEN
                        WRITE(6,11008) STANG(ISPGRP),CELL(6)
11008                   FORMAT(' Conflict between cell angles from space group',
     $                  ' and mtz input file, STANG, CELL=',2F8.3)
                        STOP
                ENDIF
C
CAND
                IF(ABANG.NE.180.0 - CELL(6)) THEN 
                        WRITE(6,11011) ABANG,CELL(6)
11011                   FORMAT(' Conflict between GAMMA and MTZ cell angle',
     $                  ' ABANG,CELL= ',2F8.3)
                        STOP
                ENDIF
                WRITE(6,1113)CELL(1),CELL(2),CELL(3)
1113            FORMAT('  CELL DIMENSIONS READ IN',/,
     .                  '  A=',F15.2,/,'  B=',F15.2,/,'  C=',F15.2)
                ASTAR=1.0/(CELL(1)*SIN(DRAD*ABANG))
                BSTAR=1.0/(CELL(2)*SIN(DRAD*ABANG))
                CSTAR=1.0/ CELL(3)
                WSTAR=CSTAR/3.0
C
                N=0
                NAMPS=0
                NPHASES=0
                NREC=0
                IFOMLIMIT=1     ! corresponds to FOM of 0.01
C
11007    CALL LRREFF(1,RESOL,ADATAIN,EOF)
         IF(EOF) GO TO 11003
         NREC=NREC+1
         IF(ADATAIN(5).EQ.0) GO TO 11007        ! CRITERIA ON SIGF
         N=N+1
         NAMPS=N
         IF(N.GT.NMAXC) GO TO 1150
C
                IHC(N)=ADATAIN(1)
                IKC(N)=ADATAIN(2)
                  IF(ABS(IHC(N)).GT.MAXINDEX.OR.ABS(IKC(N)).GT.MAXINDEX) THEN
                        WRITE(6,196) MAXINDEX
196                     FORMAT(' LATTICE LINE POINTER ARRAY TOO SMALL, MAXINDEX=',I5)
                        STOP
                  ENDIF
                ILC(N)=ADATAIN(3)
                ISC(N)= 10000.0 * ((IHC(N)*ASTAR)**2 + 
     .                  2*IHC(N)*IKC(N)*ASTAR*BSTAR*COS(DRAD*ABANG) + 
     .                  (IKC(N)*BSTAR)**2+(ILC(N)*WSTAR)**2)
                IFCC(N)=ADATAIN(4)
                IPHC(N)=ADATAIN(6)
                IFOM(N)=ADATAIN(7)*100.0        ! compatibility LCF vs MTZ
                IF(IFOM(N).GE.IFOMLIMIT) NPHASES=NPHASES+1
                IF(N.EQ.1) THEN
                        IHOLD=IHC(N)
                        IKOLD=IKC(N)
                        IBEGIN(IHOLD,IKOLD)=1
                ENDIF
                IF(.NOT.((IHC(N).EQ.IHOLD).AND.(IKC(N).EQ.IKOLD))) THEN
                        IFINISH(IHOLD,IKOLD)=N-1
                        IHOLD=IHC(N)
                        IKOLD=IKC(N)
                        IBEGIN(IHOLD,IKOLD)=N
                ENDIF
                GO TO 11007
C
11003                   WRITE(6,11004)
11004                   FORMAT(' end of mtz input')
                IFINISH(IHOLD,IKOLD)=N
C
                WRITE(6,11005) NPHASES,NREC
11005           FORMAT(I10,' Phases input on stream 1',/,
     $                  I10,' total records on stream 1.')
                CALL LRCLOS(1)
      ENDIF
C
C
C  CREATE POINTERS TO REGIONS OF LATTICE LINES WITH MEASURED PHASES.
      DO 5400 IH=-MAXINDEX,MAXINDEX
      DO 5400 IK=-MAXINDEX,MAXINDEX
      IF(IBEGIN(IH,IK).NE.-999.AND.IFINISH(IH,IK).NE.-999) THEN
        DO 5000 J=IBEGIN(IH,IK),IFINISH(IH,IK)
        IF(IFOM(J).GE.IFOMLIMIT) THEN   ! First phase point
          IBEGINPH(IH,IK) = J
          GO TO 5100
        ENDIF
5000    CONTINUE
      IF(LIST)WRITE(6,*)IH,IK,ILC(IBEGIN(IH,IK)),ILC(IFINISH(IH,IK)),
     .          IBEGIN(IH,IK),  IFINISH(IH,IK)
            GO TO 5400                  ! No phases on this lattice line.
5100   DO 5200 K=J+1,IFINISH(IH,IK)
        IF(IFOM(K).LT.IFOMLIMIT) THEN   ! Last phase point
        IF(K.NE.IFINISH(IH,IK).AND.IFOM(K+1).GE.IFOMLIMIT) GO TO 5200
C       ABOVE EXCLUDES SINGLE POINTS IN MIDDLE OF LINE WITH IFOM < IFOMLIMIT.
          IFINISHPH(IH,IK) = K-1
          GO TO 5300
        ENDIF
5200    CONTINUE
        IFINISHPH(IH,IK)=IFINISH(IH,IK) ! Phases all way to end.
5300    IF(LIST)WRITE(6,*)IH,IK,ILC(IBEGIN(IH,IK)),ILC(IFINISH(IH,IK)),
     .          IBEGIN(IH,IK),  IFINISH(IH,IK),
     .          IBEGINPH(IH,IK),IFINISHPH(IH,IK)
      ENDIF
5400  CONTINUE
        FOMLIMIT=IFOMLIMIT/100.0
        WRITE(6,1100)NAMPS,NPHASES,FOMLIMIT,NREC
1100    FORMAT(I10,' NON-ZERO AMPLITUDES AND',
     .  I10,' PHASES WITH FOM >',F5.2,' READ IN ON STREAM 1',/,
     .  I10,' TOTAL RECORDS ON STREAM 1')
        GO TO 220
C
C    NOW GENERATE REFERENCE AMP AND PHASE FROM DATA READ IN ABOVE.
C
220    WRITE(6,125)
125   FORMAT(119('*'),/,/)
C  WHEN (IREF.EQ.1), NEW FILMS ARE COMPARED ONLY TO REFERENCE DATASET.
C  WHEN (IREF.EQ.0), NEW FILMS ARE COMPARED ONLY TO FIRST DATASET(UNTILTED)
        TAXB=TAXA+ABANG
      WRITE(6,155)TAXA,TAXB,TANGL
155   FORMAT('  A-STAR WAS ',F8.3, ' DEG FROM TILTAXIS, B-STAR WAS '
     1 ,F8.3, ' DEG FROM TILT AXIS ',/,'  THE TILT ANGLE WAS ',F8.3,
     1 ' DEG ')
      IF(LIST)WRITE(6,156)
156   FORMAT(' REFERENCE DATA CORRESPONDING TO THE OBSERVED IMAGE INPUT'
     1  ,/,' HIN KIN   IP1  IHREF IKREF   ZSTAR      REF       REF',/,
     2  '          PHASE                       PHASE    AMPLIT',
     2  '    AMP(PHS)   DISAGREES',/,
     3  '           REL.')
      STAXA=ASTAR*SIN(DRAD*TAXA)
      STAXB=BSTAR*SIN(DRAD*TAXB)
      TTANGL=TAN(TANGL*DRAD)
      NFINDA=0
      NFINDP=0
        DO 290 IN=1,NSPOTS
        IH = IHIN(IN)
        IK = IKIN(IN)
      IP1=1
      IP2=0
      IF(IREF.EQ.1) THEN
            DPERP=IH*STAXA+IK*STAXB
            Z=DPERP*TTANGL
      ELSE
            Z=0.0
      ENDIF
            CALL FIDDLE(IH,IK,Z,REVHK,SGNXCH,ROT180)
C
      CALL ASYM(IH,IK,Z,IP1,IP2,LSPEC,IPTEST,
     1  WSTAR,MAT(1,IMAT(1,ISPGRP)),MAT(1,IMAT(2,ISPGRP)),
     2  MAT(1,IMAT(3,ISPGRP)),MAT(1,IMAT(4,ISPGRP)),
     3  MAT(1,IMAT(5,ISPGRP)),
     4  IGO(1,ISPGRP),ISPEC(1,ISPGRP),LREV(ISPGRP))
C
C       IP1 AND IP2 GENERATE THE RELATIONSHIP BETWEEN PHASES OF REFLECTIONS
C         IN THE UNIQUE ASYMMETRIC UNIT AND THE INPUT REFLECTIONS. THE
C         REFLECTIONS FROM PREVIOUS FILMS WILL BE TRANSFORMED TO LIE IN THE
C         SAME POSITIONS AS THE INPUT REFLECTIONS AND ORIGIN REFINEMENT
C         WILL BE PERFORMED IN P1.
C       LSPEC IS TRUE A REFLECTION IS SPECIAL, HAS ITS PHASE RESTRICTED BY
C         SYMMETRY. IPTEST IS 0 IF THE REFLECTION SHOULD BE REAL AND 90
C         IF IT SHOULD BE IMAGINARY
C
C
      IF(IREF.EQ.0)   THEN
C               THIS SECTION FOR COMPARISON WITH UNTILTED DATA.
            DO 240 JREF=1,JREFL
            IF(IH.NE.JH(JREF)) GO TO 240
            IF(IK.NE.JK(JREF)) GO TO 240
                AREF(IN)=AMP(JREF)
                NFINDA=NFINDA+1
                IF(PHS(JREF).NE.-999.0) THEN
                        PREF(IN)=PHS(JREF)*IP1-IP2
                        NFINDP=NFINDP+1
                ELSE
                        PREF(IN)=-999.0
                ENDIF
                GO TO 241
240         CONTINUE
                AREF(IN)=-999.0         ! No reference input of this spot.
                PREF(IN)=-999.0
241         CONTINUE
C
C              SPOTS FROM PREVIOUS FILMS ARE COMPARED WITH THE INPUT FILM SPOTS
C              BY TRANSFORMING THE PREVIOUS FILMS' PHASES WITH IP1 AND IP2
C              TO CORRESPOND TO REFLECTIONS WITH THE SAME INDICES AS THE
C              INPUT REFLECTIONS. THE COMPARISON IS THEN DONE IN P1.
C
      ENDIF
      IF(IREF.EQ.1)    THEN
            CALL GETCRVAL(IN,IHIN,IKIN,IH,IK,Z,ILC,IFCC,IPHC,
     .          IBEGINPH,IFINISHPH,IOK,CELL(3),ADUMMY,PHASE,DPDZCU)
            PREF(IN)=PHASE*IP1-IP2
            IF(.NOT.IOK) PREF(IN)=-999.0
            IF(IOK) NFINDP = NFINDP + 1
            CALL GETCRVAMP(IN,IHIN,IKIN,IH,IK,Z,ILC,IFCC,IPHC,
     .          IBEGIN,IFINISH,CELL(3),AREF(IN))
            IF(AREF(IN).NE.-999.0) NFINDA = NFINDA + 1
        INOTE='    '
C       CHECK THAT ADUMMY IS NEAR IN VALUE TO AREF(IN)
        ATEST = ABS(ADUMMY-AREF(IN))
        IF(IOK.AND.ATEST.GT.0.15*ADUMMY.AND.ATEST.GT.100.)INOTE='****'
      ENDIF
      IF(LISTS)WRITE(10,5003)IHIN(IN),IKIN(IN),Z,AREF(IN)
5003    FORMAT(2I5,F10.5,F15.5)
290   IF(LIST)WRITE(6,291)IHIN(IN),IKIN(IN),IP1,IH,IK,Z,
     .                  PREF(IN),AREF(IN),ADUMMY,INOTE
291   FORMAT(2I4,I5,2X,2I4,F10.4,3F10.1,5X,A)
CTSH-      J99=99
      DUMMEY=0.0
      IF(LISTS)WRITE(10,5003)J99,J99,DUMMEY,DUMMEY
      WRITE(6,292) NSPOTS, NFINDA, NFINDP
292     FORMAT(' # OF REF AMPS AND PHASES TO',
     .         ' COMPARE WITH',I5,' INPUT SPOTS;',2I5,/,/,80('*'),/) 
      RETURN
1150  WRITE(6,1151)NMAXC
      STOP
1151  FORMAT(' PROGRAM DIMENSIONS TOO SMALL FOR REFERENCE CURVES',I6)
      END
C******************************************************************************
      SUBROUTINE ASYM(IH,IK,Z,IP1,IP2,SPEC,IPTEST,WSTAR,
     1  A1,A2,A3,A4,A5,IGO,ISPEC,LREV)
      INTEGER*2 A1(8),A2(8),A3(8),A4(8),A5(8),IGO(8),ISPEC(5)
      INTEGER*2 IP1,IP2
      LOGICAL SPEC,LREV
C
C      WRITE(6,904)A1,A2,A3,A4,A5,IH,IK,Z,IP1,IP2,SPEC,IPTEST,WSTAR
      IF(IH.LT.0) CALL MULT(A1,IH,IK,Z,IP1,IP2)
50    INDEX=1
      IF(IK.GE.0) INDEX=INDEX+1
      IF(Z.GE.0.0) INDEX=INDEX+2
      IF(IH.LT.IABS(IK)) INDEX=INDEX+4
      INDEX=IGO(INDEX)
C      WRITE(6,902) INDEX
902   FORMAT (I10)
C      WRITE(6,901) IH,IK,Z,IP1,IP2
901   FORMAT(2I5,F10.5,2I5)
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
100    CALL MULT(A5,IH,IK,Z,IP1,IP2)
C       WRITE(6,900) A5,IH,IK,Z,IP1,IP2
900    FORMAT(8I5,5X,2I5,F10.5,2I10)
       GO TO 50
150    CALL MULT(A4,IH,IK,Z,IP1,IP2)
C       WRITE(6,900)A4,IH,IK,Z,IP1,IP2
       GO TO 50
200    CALL MULT(A3,IH,IK,Z,IP1,IP2)
C       WRITE(6,900)A3,IH,IK,Z,IP1,IP2
       GO TO 50
250    CALL MULT(A2,IH,IK,Z,IP1,IP2)
C       WRITE(6,900)A2,IH,IK,Z,IP1,IP2
C
C      AFTER REFLECTIONS HAVE BEEN PLACED INTO THE ASYMMETRIC UNIT
C       THEY ARE EXAMINED TO SEE IF THEY ARE SPECIAL REFLECTIONS,
C       ONES WHOSE PHASE MUST BE EITHER REAL (0 OR PI) OR IMAGINARY
C       (PI/2 OR 3*PI/2)
C
500    CONTINUE
       IF(IH.EQ.0 .AND. IK.LT.0) CALL MULT(A1,IH,IK,Z,IP1,IP2)
       IF(LREV .AND. IH.EQ.0) CALL MULT(A4,IH,IK,Z,IP1,IP2)
       SPEC=.FALSE.
       IPTEST=0
C      SPEC WILL BE TRUE IF THE REFLECTION IS SPECIAL .
C      IPTEST WILL BE 0 IF REAL AND 90 IF IMAGINARY
C      ISPEC INDICATES THE CONDITIONS FOR THE REFLECTIONS
C          ISPEC(1)=1  H=0 SPECIAL
C          ISPEC(2)=1  K=0 SPECIAL
C          ISPEC(3)=1  Z=0 SPECIAL
C          ISPEC(4)=1  H=K SPECIAL
C          ISPEC(5)=1  IF FOR H=0 OR K=0 K+H ODD INDICATES AN
C                       IMAGINARY VALUE FOR THE REFLECTION
C                      ALL OTHER SPECIAL REFLECTIONS ARE REAL
C
      IF(ISPEC(1).LT.1) GO TO 510
      IF(IH.EQ.0) GO TO 560
510   CONTINUE
      IF(ISPEC(2).LT.1) GO TO 520
      IF(IK.EQ.0) GO TO 560
520   CONTINUE
      IF(ISPEC(3).LT.1) GO TO 530
      IF(ABS(Z).LT.WSTAR) GO TO 570
530   CONTINUE
      IF(ISPEC(4).LT.1) GO TO 600
      IF(IH.EQ.IK) GO TO 570
      GO TO 600
560   CONTINUE
      IF(ISPEC(5).EQ.0) GO TO 570
      IF(ISPEC(5).EQ.-1) GO TO 563
      I=IH+IK
      GO TO 565
563   I=IK
565   I2=2*(I/2)
      IF(I.GT.I2) IPTEST=90
570   SPEC=.TRUE.
600    CONTINUE
C      WRITE(6,903)IH,IK,Z,IP1,IP2,SPEC,IPTEST,WSTAR
903   FORMAT(2I5,F10.5,2I10,L4,I5,F10.5)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE MULT(IA,IH,IK,Z,IP1,IP2)
C
C     DOES MATRIX MULTIPLICATION TO BRING REFLECTIONS INTO THE
C       ASYMMETRIC UNIT.
C
C     (H' K' Z' AMP' PHS')=(H K Z AMP PHI) <A>
C
C
C        <A> HAS FORM     IA(1)  IA(3)     0      0  IA(6)
C                         IA(2)  IA(4)     0      0  IA(7)
C                             0      0 IA(5)      0      0
C                             0      0     0      1      0
C                             0      0     0      0  IA(8)
C           FOR ALL CASES.
C
C
      INTEGER*2 IA(8),IP1,IP2
C      WRITE(6,900)IA,IH,IK,Z,IP1,IP2
      IH1=IA(1)*IH+IA(2)*IK
      IK=IA(3)*IH+IA(4)*IK
      IH=IH1
      Z=IA(5)*Z
      IP1=IA(8)*IP1
      IP2=IP2+IA(6)*IH+IA(7)*IK
C      WRITE(6,900)IA,IH,IK,Z,IP1,IP2
C900   FORMAT(' IA,IH,IK,Z,IP1,IP2 ',8G5.1,10X,5G10.5)
      RETURN
      END
C******************************************************************************
      SUBROUTINE GETCRVAL(ISPOT,IHIN,IKIN,IH,IK,ZASYM,
     .  JLC,IFCC,IPHC,IBEGIN,IFINISH,IOK,C,FREF,PREF,DPDZCU)
      PARAMETER (MAXINDEX=40)
      INTEGER IH,IK,IHIN(1),IKIN(1)
      INTEGER*2 JLC(1),IFCC(1),IPHC(1)
      INTEGER*2 IBEGIN(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),
     .  IFINISH(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX)
c      REAL*8 A(2,2),B(2),W(20),E
C              THESE BELOW ARE JUST DUMMY VARIABLES FOR ASYM.
c      INTEGER*2 A1(8),A2(8),A3(8),A4(8),A5(8),IGO(8),ISPEC(5)
c      INTEGER*2 IP1,IP2
       LOGICAL IOK
      DATA DRAD,RDEG,PI/0.0174532,57.295779,3.14159/
      DATA BTEMP/80.0/
        IOK=.TRUE.
        NBEGIN=IBEGIN(IH,IK)
        NFINISH=IFINISH(IH,IK)
        IF((NBEGIN.NE.-999).AND.(NFINISH.NE.-999)) GO TO 70
                IOK=.FALSE.
                FREF=-999.
                PREF=-999.
C               WRITE(6,1107)IH,IK,IHIN(ISPOT),IKIN(ISPOT)
1107            FORMAT(' LATTICE LINE NOT FOUND',2I5,'   SPOT',2I5)
                RETURN
70      ZBEGIN=JLC(NBEGIN)/C
        ZFINISH=JLC(NFINISH)/C
        ZALLOW = 1.0/(C*2.0)
      IF((ZASYM.GE.ZBEGIN-ZALLOW).AND.(ZASYM.LE.ZFINISH+ZALLOW))GO TO 80
                IOK=.FALSE.
                FREF=-999.
                PREF=-999.
C               WRITE(6,1108)IH,IK,IHIN(ISPOT),IKIN(ISPOT),ZASYM,
C     .         JLC(NBEGIN),JLC(NFINISH)
1108    FORMAT(' ZSTAR OUTSIDE RANGE ON LINE',2I5,'   SPOT',2I5,
     .  '        ZSTAR=',F8.4,' RANGE=',2I5)
                RETURN
80      CONTINUE
C********************BELOW IS CALCULATION OF PHASE AT EXACT VALUE OF ZSTAR.
C                    IT IS BASED ON THE SUM OF DAMPED SINC FUNCTIONS, WITH
C                    DAMPING SET TO BTEMP=80, AND A SINC FUNCTION WHICH FALLS
C                    TO ZERO AT TWO REFLECTIONS AWAY FROM THE POINT BEING
C                    CALCULATED. THUS THE CALCULATION GIVES DOUBLE THE VALUE
C                    OF F WHICH WOULD BE OBTAINED BY SIMPLE INTERPOLATION.
C                    AFTER DIVIDING THE RESULT BY TWO,THE OUTPUT COLUMN 
C                    FREF IS THEREFORE DIRECTLY COMPARABLE WITH THE INPUT
C                    AMPLITUDES.
        CPART=0.0
        SPART=0.0
          DZ=0.0004     !  SET DZ FOR GRADIENT CALC HERE.
          CPARTDZ=0.0
          SPARTDZ=0.0
        DO 85 I=NBEGIN,NFINISH
        ZI=JLC(I)/C
        ZDIFF=ZASYM-ZI
          ZDIFFDZ=ZASYM+DZ-ZI
      IF(ZDIFF.NE.0) GO TO 81
        SINCDAMP=1.0
        GO TO 82
81      ARGEXP=-0.25*BTEMP*ZDIFF**2
        ARGSINC=0.5*PI*ZDIFF*C
        SINCF=SIN(ARGSINC)/ARGSINC
        SINCDAMP=SINCF*EXP(ARGEXP)
82      CONTINUE
      IF(ZDIFFDZ.NE.0) GO TO 83
        SINCDMPDZ=1.0
        GO TO 84
83      ARGEXP=-0.25*BTEMP*ZDIFFDZ**2
        ARGSINC=0.5*PI*ZDIFFDZ*C
        SINCF=SIN(ARGSINC)/ARGSINC
        SINCDMPDZ=SINCF*EXP(ARGEXP)
84      CONTINUE
      PHS=IPHC(I)*DRAD
      CPART=CPART+SINCDAMP*COS(PHS)*IFCC(I)
      SPART=SPART+SINCDAMP*SIN(PHS)*IFCC(I)
      CPARTDZ=CPARTDZ+SINCDMPDZ*COS(PHS)*IFCC(I)
      SPARTDZ=SPARTDZ+SINCDMPDZ*SIN(PHS)*IFCC(I)
85      CONTINUE
      FREF=0.5*SQRT(SPART**2+CPART**2)
      PREF=RDEG*ATAN2(SPART,CPART)
      FREFDZ=0.5*SQRT(SPARTDZ**2+CPARTDZ**2)
      PREFDZ=RDEG*ATAN2(SPARTDZ,CPARTDZ)
        PDIFF=PREFDZ-PREF       ! MAX 8 DEG IN 0.0004 DZ == 180 DEG IN 0.01 DZ.
        IF(ABS(PDIFF).GT.180.0) PDIFF=PDIFF-SIGN(360.0,PDIFF)
        IF(ABS(PDIFF).GT.8.0) PDIFF=SIGN(8.0,PDIFF)
      DPDZCU = PDIFF/DZ
C      WRITE(6,86)IH,IK,ZASYM,FREF,FREFDZ,PREF,PREFDZ,DPDZCU
86    FORMAT(' H,K,Z,F,F+DZ,P,P+DZ',2I5,F8.4,2F10.2,2F10.3,F15.0)
      RETURN
      END
C******************************************************************************
      SUBROUTINE GETCRVAMP(ISPOT,IHIN,IKIN,IH,IK,ZASYM,
     .  JLC,IFCC,IPHC,IBEGIN,IFINISH,C,FREF)
      PARAMETER (MAXINDEX=40)
      INTEGER IH,IK,IHIN(1),IKIN(1)
      INTEGER*4 IFCC(1)
      INTEGER*2 JLC(1),IPHC(1)
      INTEGER*2 IBEGIN(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX),
     .  IFINISH(-MAXINDEX:MAXINDEX,-MAXINDEX:MAXINDEX)
c      REAL*8 A(2,2),B(2),W(20),E
C              THESE BELOW ARE JUST DUMMY VARIABLES FOR ASYM.
c      INTEGER*2 A1(8),A2(8),A3(8),A4(8),A5(8),IGO(8),ISPEC(5)
c      INTEGER*2 IP1,IP2
      LOGICAL IOK
      DATA DRAD,RDEG,PI/0.0174532,57.295779,3.14159/
      DATA BTEMP/80.0/
        IOK=.TRUE.
        NBEGIN=IBEGIN(IH,IK)
        NFINISH=IFINISH(IH,IK)
        IF((NBEGIN.NE.-999).AND.(NFINISH.NE.-999)) GO TO 70
                FREF=-999.
C               WRITE(6,1107)IH,IK,IHIN(ISPOT),IKIN(ISPOT)
1107            FORMAT(' LATTICE LINE NOT FOUND',2I5,'   SPOT',2I5)
                RETURN
70      ZBEGIN=JLC(NBEGIN)/C
        ZFINISH=JLC(NFINISH)/C
      IF((ZASYM.GE.ZBEGIN).AND.(ZASYM.LE.ZFINISH)) GO TO 80
                FREF=-999.
C               WRITE(6,1108)IH,IK,IHIN(ISPOT),IKIN(ISPOT),ZASYM,
C     .         JLC(NBEGIN),JLC(NFINISH)
1108    FORMAT(' ZSTAR OUTSIDE RANGE ON LINE',2I5,'   SPOT',2I5,
     .  '        ZSTAR=',F8.4,' RANGE=',2I5)
                RETURN
80      CONTINUE
C@@@@@@@@@@@@@@@@@@@@BELOW IS CALCULATION OF AMPLITUDE AT EXACT VALUE OF ZSTAR.
C                    IT IS BASED ON THE SUM OF DAMPED SINC FUNCTIONS, WITH
C                    DAMPING SET TO BTEMP=80, AND A SINC FUNCTION WHICH FALLS
C                    TO ZERO ONE REFLECTION AWAY FROM THE POINT BEING
C                    CALCULATED.  
C                    THE OUTPUT COLUMN FREF IS THEREFORE DIRECTLY COMPARABLE
C                    WITH THE INPUT AMPLITUDES.  AT LATTICE POINTS IT IS
C                    IDENTICAL WITH THE INPUT.
        FREF=0.0
        DO 85 I=NBEGIN,NFINISH
        ZI=JLC(I)/C
        ZDIFF=ZASYM-ZI
      IF(ZDIFF.NE.0) GO TO 81
        SINCDAMP=1.0
        GO TO 82
81      ARGEXP=-0.25*BTEMP*ZDIFF**2
        ARGSINC=PI*ZDIFF*C
        SINCF=SIN(ARGSINC)/ARGSINC
        SINCDAMP=SINCF*EXP(2.0*ARGEXP)
82      CONTINUE
        FREF = FREF + SINCDAMP * (FLOAT(IFCC(I)))**2
85      CONTINUE
      IF(FREF.LE.0.0) FREF=1.0
      FREF=SQRT(FREF)
C      WRITE(6,86)IH,IK,ZASYM,FREF
86    FORMAT(' H,K,Z,F',2I5,F8.4,F10.2)
      RETURN
      END
C******************************************************************************
C  FIDDLING WITH THE INDEXING TO GET CORRECT MATCH TO INDEXING CONVENTION
C  USEFUL IN A NUMBER OF SPACE GROUPS -- SEE WRITE-UP AT TOP OF PROGRAM.
      SUBROUTINE FIDDLE(IH,IK,Z,REVHK,SGNXCH,ROT180)
      IF(REVHK.EQ.0.0) GO TO 225
      I=IH
      IH=IK
      IK=I
      Z=-Z
225   CONTINUE 
      IF(SGNXCH.EQ.0.0) GO TO 230
      IK=-IK
      Z=-Z
  230 IF(ROT180.EQ.0.0) GO TO 231
      IH=-IH
      IK=-IK
231   CONTINUE
      RETURN
      END
C*****************************************************************************
C
      SUBROUTINE CONVOLUTE(AP,BP,ACTF,BCTF,ACORR,BCORR,AMP,PHI,
     .          IHOR,IVERT,ICTFHOR,ICTFVERT,IBOTH)
C
C  Subroutine to perform convolution of raw transform with transform
C   of ctf for tilted image correction.
C
      PARAMETER (IBOXMAX=41)
      PARAMETER (ICTFBXMAX=401)
      PARAMETER (ICTFHALF=200)
      PARAMETER (INBOXMAX=361)
      DIMENSION  AMP(IBOXMAX,IBOXMAX), PHI(IBOXMAX,IBOXMAX),
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
        ACORR(I,J) = A
        BCORR(I,J) = B
      IF(IBOTH.EQ.1) THEN
        AMPL = SQRT(A*A + B*B)
        IF(AMPL.EQ.0.0) THEN
                PHASE = 0.
        ELSE
                PHASE = ATAN2(B,A) * 57.2958
        ENDIF
        IF(PHASE.LT.0.0) PHASE = PHASE + 360.0  ! Phase bet 0 and 360 deg.
        AMP(I,J) = AMPL
        PHI(I,J) = PHASE
      ENDIF
100   CONTINUE
      RETURN
      END
C
C*******************************************************************************
C
      SUBROUTINE SCSL_CONVOLUTE(AP,BP,ACTF,BCTF,ACORR,BCORR,AMP,PHI,
     .          IHOR,IVERT,ICTFHOR,ICTFVERT,IBOTH)
C
C
C  Subroutine to perform convolution of raw transform with transform
C   of ctf for tilted image correction. (using SCSL)
C
C--------------------------ICTFBXMAX muss 2*ICTFHALF+1 sein
      PARAMETER (IBOXMAX=41)
      PARAMETER (ICTFBXMAX=401)
      PARAMETER (ICTFHALF=200)
      PARAMETER (INBOXMAX=361)
      DIMENSION  AMP(IBOXMAX,IBOXMAX), PHI(IBOXMAX,IBOXMAX),
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
      ALPHA=CMPLX(1,0)
      BETA=CMPLX(0,0)
C
C      CALL CFIR2D(
C      CALL TDXCONV(
C     .       P(1,1),                    1,INBOXMAX,  1,        ICTFHOR+IHOR, 1,         ICTFVERT+IVERT,
C     .       CTF(-ICTFHOR2,-ICTFVERT2), 1,ICTFBXMAX, 1,        ICTFHOR+1,    1,         ICTFVERT+1,
C     .       OUTPUT(1,1),               1,INBOXMAX,  2+ICTFHOR,        IHOR, 2+ICTFVERT,         IVERT,
C     .       ALPHA,BETA)
C
C      CALL TDXCONV(P(1,1),IHOR,IVERT,
      CALL CONVOLUTE(P(1,1),IHOR,IVERT,
     .  CTF(1,1), ICTFHOR, ICTFVERT,
     .  OUTPUT(1,1))

C     CALL CFIR2D (x, incx, ldx, i1x0, nx1, i2x0, nx2,
C                  h, inch, ldh, i1h0, nh1, i2h0, nh2,
C                  y, incy, ldy, i1y0, ny1, i2y0, ny2,
C                  alpha, beta)
C
      DO I=1,IHOR
        DO J=1,IVERT
          K = MOD(I,IHOR-ICTFHOR)+ICTFHOR
          L = MOD(J,IVERT-ICTFVERT)+ICTFVERT
          RIMA = AIMAG(OUTPUT(K,L))
          RREA = REAL (OUTPUT(K,L))
          ACORR(I,J)=RREA
          BCORR(I,J)=RIMA
          if(IBOTH.eq.1)then
            AMP(I,J) = CABS(OUTPUT(K,L))
            IF(AMP(I,J).EQ.0.0) THEN
              PHASE = 0.
            ELSE
              PHASE = ATAN2(RIMA,RREA) * 57.2958
            ENDIF
            IF(PHASE.LT.0.0) PHASE = PHASE + 360.0        ! Phase bet 0 and 360 deg.
              PHI(I,J) = PHASE
          endif
        enddo
      enddo
C
      RETURN
      END
C
C*******************************************************************************
C
      SUBROUTINE CTFGEN(IH,IK,X,Y,THETATRX,THETATRY,
     .  DFMID1,DFMID2,ANGAST,CS,
     .  WL,STEPR,ISIZEX,ISIZEY,
     .  TLTAXIS,TLTANGL,ICTFHOR,ICTFVERT,ACTF,BCTF,
     .  ILIST,DFMID,DELCHI,CTFMID,FACTOR,ISENS,CTFSQ,GRADCTFSQ,DA,DB,F)
C
      PARAMETER (ICTFBXMAX=401)
      PARAMETER (ICTFHALF=200)
      DIMENSION ACTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .          BCTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .           CTF(ICTFBXMAX*ICTFBXMAX)
      DIMENSION GRADCTFSQ(5),F(5),
     .          DA(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF,5),
     .          DB(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF,5),
     .           DCTF(ICTFBXMAX*ICTFBXMAX,5)
      LOGICAL ILIST
      ISENS=ICHAR(' ')
      TWOPI = 2.0 * 3.1415926
      ANGLE = SQRT((X*THETATRX)**2+(Y*THETATRY)**2)
      ANGSPT=ATAN2(Y*THETATRY,X*THETATRX)
      TANTILT=TAN(TLTANGL)
      DELHEIGHTX=ABS(STEPR*ISIZEX*TANTILT)
      DELHEIGHTY=ABS(STEPR*ISIZEY*TANTILT)
      C1=TWOPI*ANGLE*ANGLE/(2.0*WL)
      DELCHIX=C1*DELHEIGHTX
      DELCHIY=C1*DELHEIGHTY
      DELHEIGHT=AMAX1(DELHEIGHTX,DELHEIGHTY)
      DELCHI=AMAX1(DELCHIX,DELCHIY)
      SINEWAVES=DELCHI/TWOPI
          ICTFHOR=MAX(10,INT(DELCHI))
          ICTFHOR=(ICTFHOR/2)*2                     ! ensure ICTFHOR is even.
          IF(ICTFHOR.GT.38) ICTFHOR=(ICTFHOR/8)*8   ! ensures prime factor < 19
          IF(ICTFHOR.GT.ICTFBXMAX-1) THEN           ! ensures storage ok.
                WRITE(6,101)ICTFHOR
101             FORMAT(' Subroutine CTFGEN dimensions too small,',
     .                  '  ICTFHOR needs',I8)
                STOP
          ENDIF
          ICTFVERT=ICTFHOR
          ICTFHOR2=ICTFHOR/2
          ICTFVERT2=ICTFVERT/2
      C2=-C1*CS*ANGLE*ANGLE/2.0
      ANGDIF=ANGSPT-ANGAST
        CCOS=COS(2.0*ANGDIF)
        CSIN=SIN(2.0*ANGDIF)
        COST=COS(TLTAXIS)
        SINT=SIN(TLTAXIS)
        DFMID=0.5*(DFMID1+DFMID2+CCOS*(DFMID1-DFMID2))
        CTFMID=-SIN(C1*DFMID+C2)
        IF(DELCHI/2.GT.ASIN(ABS(CTFMID))) ISENS=ICHAR('*')  !Spot has a zero in ctf
        SUMC=0.0
        CTFSQ=0.0
        DO 50 I=1,5
50      GRADCTFSQ(I)=0.0
C
        F(1)=C1*(1.0+CCOS)/(2*ICTFVERT)
        F(2)=C1*(1.0-CCOS)/(2*ICTFVERT)
        F(3)=C1*(DFMID1-DFMID2)*CSIN/ICTFVERT
        F(4)=C1*TANTILT/ICTFVERT
        F(5)=C1/(ICTFVERT*(COS(TLTANGL))**2)
C
      DO 100 I=1,ICTFHOR
      DO 100 J=1,ICTFVERT
        ISTORE=I+(ICTFHOR+2)*(J-1)      ! indexing for array CTF.
C         Calculate height of this element of image.
        XP=((I-0.5-ICTFHOR2)/(ICTFHOR))*ISIZEX*STEPR    ! 0.5 TO ROUNDOFF.
        YP=((J-0.5-ICTFVERT2)/(ICTFVERT))*ISIZEY*STEPR  ! 0.5 TO ROUNDOFF.
        DF = DFMID + TANTILT*(-XP*SINT+YP*COST)
                CHI=C1*DF+C2
                SCHI=SIN(CHI)
                CCHI=COS(CHI)
                SCTEMP=SCHI*CCHI
        SUMC=SUMC+ABS(-SCHI)
        CTFSQ=CTFSQ+SCHI**2
        GRADCTFSQ(1) = GRADCTFSQ(1) + SCTEMP
        GRADCTFSQ(4) = GRADCTFSQ(4) + SCTEMP*(-XP*COST-YP*SINT)
        GRADCTFSQ(5) = GRADCTFSQ(5) + SCTEMP*(-XP*SINT+YP*COST)
      DCTF(ISTORE,3) = -CCHI
      DCTF(ISTORE,4) = -CCHI*F(4)*(-XP*COST-YP*SINT)
      DCTF(ISTORE,5) = -CCHI*F(5)*(-XP*SINT+YP*COST)
100   CTF(ISTORE) = -SCHI/ICTFVERT  ! normalises contrast due to the number of
C                                   !    samples and FFT convolution.
C
C  Now calculate rescaling factor (applied in CONVOLUTE subroutine) so that
C   the same power is present in the spot after convolution.
      FACTOR = ICTFHOR*ICTFVERT/SUMC
      AREA  = ICTFHOR*ICTFVERT
      CTFSQ = CTFSQ/AREA                        ! mean value of CTF**2
C  Now the full expression for gradients of CTF**2.
      GRADCTFSQ(3) = GRADCTFSQ(1)*C1*2.0*(DFMID1-DFMID2)*CSIN/AREA
      GRADCTFSQ(2) = GRADCTFSQ(1)*C1*(1.0-CCOS)/AREA    !
      GRADCTFSQ(1) = GRADCTFSQ(1)*C1*(1.0+CCOS)/AREA    ! CTF**2 gradients
      GRADCTFSQ(4) = GRADCTFSQ(4)*C1*2.0*TANTILT/AREA   !
      GRADCTFSQ(5) = GRADCTFSQ(5)*C1*2.0/(AREA*(COS(TLTANGL))**2)
C
CHEN
C-----CALL TODFFT(CTF,ICTFHOR,ICTFVERT,0)
      CALL TDXFFT(CTF,ICTFHOR,ICTFVERT,0)
CHEN
      DO 120 J=3,5                      ! leave 1 & 2 till after CONVOLUTE.
CHEN
120   CALL TDXFFT(DCTF(1,J),ICTFHOR,ICTFVERT,0)
C120--CALL TODFFT(DCTF(1,J),ICTFHOR,ICTFVERT,0)
CHEN
C
C  Here to transfer the transform of ctf to ACTF,BCTF
      CALL TRANSFER(CTF,ACTF,BCTF,ICTFHOR,ICTFVERT)
      M=-ICTFHALF
      DO 121 J=3,5                      !  ditto
121   CALL TRANSFER(DCTF(1,J),DA(M,M,J),DB(M,M,J),ICTFHOR,ICTFVERT)
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
C  This subroutine transfers the transform of ctf to ACTF,BCTF
C   including an origin phase shift to the centre of the image.
      SUBROUTINE TRANSFER(CTF,ACTF,BCTF,ICTFHOR,ICTFVERT)
      PARAMETER (ICTFBXMAX=401)
      PARAMETER (ICTFHALF=200)
      DIMENSION ACTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .          BCTF(-ICTFHALF:ICTFHALF,-ICTFHALF:ICTFHALF),
     .           CTF(ICTFBXMAX*ICTFBXMAX)
C
        ICTFHOR2=ICTFHOR/2
        ICTFVERT2=ICTFVERT/2
C
C  First calculate correct phase shift for slightly offset origin for the
C     contrast distribution function.
      PHSHFT = -2.0*3.1415926*(ICTFHOR2-0.5)/ICTFHOR
C
        DO 150 I=1,ICTFHOR2+1
        DO 150 J=1,ICTFVERT
        ISTORE = (2*I-1)+(ICTFHOR+2)*(J-1)
        IF(J.LE.1+ICTFVERT2) THEN
                IX=I-1
                IY=J-1
                C=COS((IX+IY)*PHSHFT)   ! assumes ICTFHOR=ICTFVERT
                S=SIN((IX+IY)*PHSHFT)   ! assumes ICTFHOR=ICTFVERT
                A=CTF(ISTORE)
                B=CTF(ISTORE+1)
                ACTF(IX,IY) = A*C-B*S
                BCTF(IX,IY) = A*S+B*C
        ENDIF
        IF(J.GE.1+ICTFVERT2) THEN
                IX=I-1
                IY=J-1-ICTFVERT
                C=COS((IX+IY)*PHSHFT)   ! assumes ICTFHOR=ICTFVERT
                S=SIN((IX+IY)*PHSHFT)   ! assumes ICTFHOR=ICTFVERT
                A=CTF(ISTORE)
                B=CTF(ISTORE+1)
                ACTF(IX,IY) = A*C-B*S
                BCTF(IX,IY) = A*S+B*C
        ENDIF
150     CONTINUE
        DO 160 I= 1,ICTFHOR2
        DO 160 J=-ICTFVERT2,ICTFVERT2
                ACTF(-I,-J) =  ACTF(I,J)
                BCTF(-I,-J) = -BCTF(I,J)
160     CONTINUE
      RETURN
      END
C
C*******************************************************************************
      SUBROUTINE MATRIXINV(IMODE,A,B)
C  this subroutine's function is precisely tied to the definition of IMODE.
      REAL*8 A(5,5),B(5),E,W(100)       ! for matrix inversion
      IA=5
      E=-1.0
      IF(IMODE.EQ.1) BOUT=(B(1)+B(2))/(A(1,1)+A(2,2)+2.0*A(1,2))  ! defocus.
      IF(IMODE.EQ.3) BOUT=B(4)/A(4,4)                             ! tiltaxis.
      IF(IMODE.EQ.4) BOUT=B(5)/A(5,5)                             ! tiltangle.
      IF(IMODE.NE.2.AND.IMODE.NE.5) THEN
        DO 10 J=1,5
10      B(J)=0.0
      ENDIF
      IF(IMODE.EQ.1) THEN
        B(1)=BOUT
        B(2)=BOUT
      ENDIF
      IF(IMODE.EQ.3) B(4)=BOUT
      IF(IMODE.EQ.4) B(5)=BOUT
      IF(IMODE.NE.2.AND.IMODE.NE.5) RETURN
      IF(IMODE.EQ.2) THEN                       ! defocus and astigmatism.
        N=3
        B(4)=0.0
        B(5)=0.0
      ENDIF
      IF(IMODE.EQ.5) N=5                        ! all parameters.
        CALL MA21AD(A,IA,N,B,W,E)
        IF(E.EQ.0.0) RETURN
        WRITE(6,861)E
C
CHEN
        write(18,'(''ERROR:  MA21AD FAILED'',F10.5)')E
CHEN
C
      RETURN
861   FORMAT('    MA21AD FAILED',F10.5)
      END
C*******************************************************************************
C   ANISOTROPIC IMAGE SCALING TO ALLOW FOR DIFFERENTIAL LOSS OF SIGNAL
C   IN DIFFERENT IMAGE DIRECTIONS.
C    TRY OUT NEW TYPE OF SCALING -- MINIMISE (AIM/C0 - AREF*C0)**2.
C
      SUBROUTINE SCALENEW(NSPOTS,IH,IK,AIM,BIM,AREF,AREFS,CTFSQ,LISTS)
C
      DIMENSION IH(1),IK(1),AIM(1),BIM(1),AREF(1),AREFS(1),CTFSQ(1)
      DIMENSION SLOPE(4), PSFABC(4)
      REAL*8 A(4,4),B(4),W(40),E
      EQUIVALENCE (PSFABC(1),SF),(PSFABC(2),PA),(PSFABC(3),PB),
     .          (PSFABC(4),PC)
      LOGICAL LISTS
        SF = 1.0        ! same as PSFABC(1)
        DO 20 I=2,4
20      PSFABC(I)=0.000 ! same as PA, PB, PC
        FSHIFT = 0.5
      DO 200 ICYC=1,25
                DO 50 I=1,4
                B(I)=0.0
                DO 50 J=1,4
50              A(I,J)=0.0
        NAMP=0
        RFACNUMER=0.0
        RFACDENOM=0.0
        RMSMIN=0.0
      DO 100 IN=1,NSPOTS
      IF(AREF(IN).EQ.-999.0) GO TO 100
      WGT=1.0/BIM(IN)**2
      FADING=EXP(-PA*IH(IN)**2-PB*IK(IN)**2-PC*IH(IN)*IK(IN))
      C0SQ=SF*FADING
      C0=SQRT(C0SQ)
C  Equal weight to all spots with IQ values better than 2.5.
      SLOPE(1) =(AREF(IN)*CTFSQ(IN)+AIM(IN)/C0SQ)*FADING*0.5/C0
      SLOPE(2) = -SLOPE(1)*SF*IH(IN)**2
      SLOPE(3) = -SLOPE(1)*SF*IK(IN)**2
      SLOPE(4) = -SLOPE(1)*SF*IH(IN)*IK(IN)
      AREFS(IN) = AREF(IN)*CTFSQ(IN)*C0SQ
C      WRITE(6,51) AREFS(IN),CTFSQ,(SLOPE(J),J=1,4)
51      FORMAT(' AREFS,CTFSQ,SLOPE1-4',F10.4,F8.3,4F15.6)
      NAMP=NAMP+1
      RFACNUMER=RFACNUMER+ABS(AIM(IN)-AREFS(IN))/BIM(IN)
      RFACDENOM=RFACDENOM+AIM(IN)/BIM(IN)
      RMSMIN=RMSMIN+WGT*(AIM(IN)-AREFS(IN))**2/C0SQ
        DO 70 I=1,4
        B(I)=B(I)+(AIM(IN)-AREFS(IN))*SLOPE(I)*WGT/C0
        DO 70 J=1,4
70      A(I,J)=A(I,J)+SLOPE(I)*SLOPE(J)*WGT
100   CONTINUE
C      WRITE(6,101) ((A(I,J),J=1,4),B(I),I=1,4)
101     FORMAT(4F15.4,F25.4)
        IA=4
        N=4
        E=-1.0
        CALL MA21AD(A,IA,N,B,W,E)
        IF(E.EQ.0.0) GO TO 75
        WRITE(6,76) E
76      FORMAT('  MA21AD FAILED IN SUBROUTINE SCALE, E=',F10.5)
        STOP
75    DO 80 J=1,4
80    PSFABC(J)=PSFABC(J)+FSHIFT*B(J)
      RMSMIN=SQRT(RMSMIN/NAMP)
      RFACTOR=RFACNUMER/RFACDENOM
200   CONTINUE
C
C   FINAL SCALING WITHOUT USE OF C.T.F. FACTOR READY FOR C.T.F. REFINEMENT.
C
      WRITE(6,749)
749   FORMAT('$ REFINEMENT PROGRESS',
     .'      SF       A       B       C ',
     .'  RFAC  RMSMIN')
      WRITE(6,81)SF,PA,PB,PC,RFACTOR,RMSMIN
81    FORMAT('$',20X,F10.3,3F8.5,F7.4,F7.3)
      IF(LISTS)WRITE(10,5005)SF,PA,PB,PC
5005    FORMAT(4F10.5)
      DO 300 IN=1,NSPOTS
      FADING=EXP(-PA*IH(IN)**2-PB*IK(IN)**2-PC*IH(IN)*IK(IN))
300   AREFS(IN) = SF * FADING * AREF(IN)
      RETURN
      END
C***************************************************************************
C   ANISOTROPIC IMAGE SCALING TO ALLOW FOR DIFFERENTIAL LOSS OF SIGNAL
C   IN DIFFERENT IMAGE DIRECTIONS.
C
      SUBROUTINE SCALEOLD(NSPOTS,IH,IK,AIM,BIM,AREF,AREFS,CTFSQ)
C
      DIMENSION IH(1),IK(1),AIM(1),BIM(1),AREF(1),AREFS(1),CTFSQ(1)
      DIMENSION SLOPE(4), PSFABC(4)
      REAL*8 A(4,4),B(4),W(40),E
      EQUIVALENCE (PSFABC(1),SF),(PSFABC(2),PA),(PSFABC(3),PB),
     .          (PSFABC(4),PC)
        SF = 1.0        ! same as PSFABC(1)
        DO 20 I=2,4
20      PSFABC(I)=0.000 ! same as PA, PB, PC
        FSHIFT = 0.5
      DO 200 ICYC=1,25
                DO 50 I=1,4
                B(I)=0.0
                DO 50 J=1,4
50              A(I,J)=0.0
        NAMP=0
        RFACNUMER=0.0
        RFACDENOM=0.0
        RMSMIN=0.0
      DO 100 IN=1,NSPOTS
      IF(AREF(IN).EQ.-999.0) GO TO 100
      WGT=1.0/BIM(IN)**2
C  Equal weight to all spots with IQ values better than 2.5.
      SLOPE(1) = AREF(IN) * CTFSQ(IN) *
     .           EXP(-PA*IH(IN)**2-PB*IK(IN)**2-PC*IH(IN)*IK(IN))
      SLOPE(2) = -SLOPE(1)*SF*IH(IN)**2
      SLOPE(3) = -SLOPE(1)*SF*IK(IN)**2
      SLOPE(4) = -SLOPE(1)*SF*IH(IN)*IK(IN)
      AREFS(IN) = SLOPE(1)*SF
C      WRITE(6,51) AREFS(IN),CTFSQ,(SLOPE(J),J=1,4)
51      FORMAT(' AREFS,CTFSQ,SLOPE1-4',F10.4,F8.3,4F15.6)
      NAMP=NAMP+1
      RFACNUMER=RFACNUMER+ABS(AIM(IN)-AREFS(IN))/BIM(IN)
      RFACDENOM=RFACDENOM+AIM(IN)/BIM(IN)
      RMSMIN=RMSMIN+WGT*(AIM(IN)-AREFS(IN))**2
        DO 70 I=1,4
        B(I)=B(I)+(AIM(IN)-AREFS(IN))*SLOPE(I)*WGT
        DO 70 J=1,4
70      A(I,J)=A(I,J)+SLOPE(I)*SLOPE(J)*WGT
100   CONTINUE
C      WRITE(6,101) ((A(I,J),J=1,4),B(I),I=1,4)
101     FORMAT(4F15.4,F25.4)
        IA=4
        N=4
        E=-1.0
        CALL MA21AD(A,IA,N,B,W,E)
        IF(E.EQ.0.0) GO TO 75
        WRITE(6,76) E
76      FORMAT('  MA21AD FAILED IN SUBROUTINE SCALE, E=',F10.5)
        STOP
75    DO 80 J=1,4
80    PSFABC(J)=PSFABC(J)+FSHIFT*B(J)
      RMSMIN=SQRT(RMSMIN/NAMP)
      RFACTOR=RFACNUMER/RFACDENOM
200   CONTINUE
C
C   FINAL SCALING WITHOUT USE OF C.T.F. FACTOR READY FOR C.T.F. REFINEMENT.
C
      WRITE(6,749)
749   FORMAT(' REFINEMENT PROGRESS',
     .'       SF       A       B       C ',
     .'  RFAC  RMSMIN')
      WRITE(6,81)SF,PA,PB,PC,RFACTOR,RMSMIN
81    FORMAT('$',20X,F10.3,3F8.5,F7.4,F7.3)
      DO 300 IN=1,NSPOTS
      FADING=EXP(-PA*IH(IN)**2-PB*IK(IN)**2-PC*IH(IN)*IK(IN))
300   AREFS(IN) = SF * FADING * AREF(IN)
      RETURN
      END
C*****************************************************************************
