C  SCALIMAMP3D
C
C       Program to scale up image amplitudes to restore the resolution-dependent
C       fall off due to image blurring, radiation damage, temperature factor,
C       static disorder, charging or any other source of power loss.  Data will
C       be scaled to a selected reference which has no resolution-dependent
C       fall-off and which is hoped will have a similar amplitude distribution.
C
C       Original version        23-Mar-1994     derived from SCALIMAMP
C       Version 2.00    RH      27-Mar-1994     generalised to use in 2- or 3-D
C       Version 2.01    RH      01-Apr-1994     limits on resolution, B-factor
C       Version 2.02    RH      05-Aug-1994     apply extra manual B-factor
C       Version 2.03    RH      09-Aug-1994     debug - add film scale factor
C       Version 2.04    RH      23-Jan-1995     move to UNIX includes CCPDPN
C       Version 2.05    RH      27-Apr-1995     debug AMPCORR, SCALFACT
C       Version 2.06    RH      05-Jul-1995     checks for ABS(PHASE).GT.180
C       Version 2.07    RH      14-Jan-1998     sets BZ to 0 if film untilted
C       Version 2.08    RH      10-Feb-1999     NMAX -> 8000
C       Version 2.09    RH      31-May-1999     BR3DT test changed to warning
C       Version 2.10    RH      03-Aug-1999     increase NFILMS to 150
C       Version 2.11    RH      19-Aug-1999     debug BR array outer edge
C       Version 2.12    RH      29-Oct-2001     change FILIN to CHARACTER*80
C       Version 2.13    JMS     30-Oct-2001     Correction to format statement
C
C       card input:
C
CHENN>
C C  card 1: selected reference data, chosen from FF,BT,LZ,BR:   RREF - (A2)
C  card 1: selected reference data, chosen from FF,BT,LZ,BR,AQ:   RREF - (A2)
CHENN<
C       FF - eldiff formfactors for IAM C,N,O average from Internl Tab Cryst.
C       BT - Bacillus thuringiensis toxin cry3a - X-ray amplitudes to 2.5 A.
C       LZ - tetragonal lyzozyme.
C       BR - bacteriorhodopsin, average of p3 and orthorhombic crystal amps.
C               only the BR selection allows anisotropic scaling to account for
C               the likely orientation of helices perpendicular to membrane.
CHENN>
C       AQ - same as LZ.
CHENN<
C
C  card 2: type of input data to be scaled: 
C               NPROG,TWOFOLD,BXYMIN,BXYMAX,BZMIN,BZMAX - (*)
C               - NPROG=0, input is ORIGMERG o/p - H,K,Z,A,P,filmno,IQ,WT,B,CTF
C               - NPROG=1, input is AVRGAMPHS o/p - H,K,L,A,P,FOM
C               - NPROG=2, input is AVRGAMP o/p - H,K,L,A,SIGA
C               - if NPROG=1, TWOFOLD is active (T/F) for setting phase to 0/180
C
C               - if NPROG=0, scaling is via B-factors, with BMIN/MAX active
C               - BXYMIN/MAX minimum and maximum in-plane B-factors to apply
C               - BZMIN/MAX minimum and maximum vertical B-factors to apply
C
C               - if NPROG=1 or 2, scaling is in zones vs chosen reference data
C
C  card 3:  RESLIMXY, RESLIMZ, BEXTRA 
C               - resolution limits (Angstroms) in xy,z for SCALORIGTILT.
C
C  card 4: filename for data to be scaled               - (character*80)
C
C  card 5: cell dimensions and resolution for above: A,B,GAMMA,RESOL - (*)
C
C       cards 4 and 5 can be repeated to produce up to 8 multiple plots if 
C       the primary purpose of the run is to produce graphical output from
C       separate sets of projection data - only possible for AVRGAMPS data.
C
C       INPUT : - UNIT number is 11, filename from stream 5.
C       OUTPUT: 
C               - output for CURVY on 'SCALIMAMP3D.DAT'         - unit 2
C               - scaled up version of required data on  'OUT'  - unit 3
C
C****************************************************************************** 
      PARAMETER (NMAX=80000)
      PARAMETER (NSLOTS=80)
      PARAMETER (NREF=5)
      PARAMETER (INTERVALBR=25)
      DIMENSION IH(NMAX),IK(NMAX),IL(NMAX),ZSTAR(NMAX),AMP(NMAX),
     .  PHASE(NMAX),JQ(NMAX),WGT(NMAX),BCK(NMAX),
     .  CTFS(NMAX),FOM(NMAX),AMPOUT(NMAX),BCKOUT(NMAX),
     .  SIGAMP(NMAX),SIGAMPOUT(NMAX)
      INTEGER*8 JFILM(NMAX)
      DIMENSION NSPOTDAT(NSLOTS),AVERREF(NSLOTS),
     .  AVERDAT(NSLOTS),SCALFACT(NSLOTS),PHASERR(NSLOTS),SUMN(NSLOTS)
CTSH      DIMENSION TITLE(12),TEXT(13),DREF(NREF),ATAB(400),BTCRY3A(33)
CTSH++
      DIMENSION TITLE(12),DREF(NREF),ATAB(400),BTCRY3A(33)
        CHARACTER*52 TEXT
CTSH--
C               BR-3D transform(xy*,z*) in steps of 1/d=BRSPACING A**-1
      DIMENSION BR3DT(INTERVALBR,INTERVALBR)
      CHARACTER*20 DESCRIPTION(NREF)
      LOGICAL TWOFOLD
      CHARACTER*80 FILIN
C
CTSH++
      CHARACTER*4 TMPDREF(NREF)
        EQUIVALENCE (TMPDREF,DREF)
      CHARACTER*4 TMPRREF
        EQUIVALENCE (TMPRREF, RREF)
CTSH--
      DATA IRESTEP/20/, NSMOOTH/2/, NIN/0/, NDATAMAX/8/
      DATA AVERREF/NSLOTS*0.0/, AVERDAT/NSLOTS*0.0/
      DATA NSPOTDAT/NSLOTS*0/, SUMN/NSLOTS*0/
      DATA SCALFACT/NSLOTS*-999.0/, DRAD/0.017453/
C       B.t. average amplitudes in intervals of s=48.5 out to 2.5 Angstroms
      DATA BTCRY3A/8070,9068,8001,6330,5662,5710,6129,6778,7320,
     .  7423,7111,6285,6052,5824,5412,4979,4708,4404,4112,3836,
     .  3507,3240,3037,2946,2666,2551,2463,2307,2202,2003,2056,
     .  1894,1902/, BTSPACING/48.5/
C      DATA ALYSOZ/  /, LZSPACING/  /   ! to be added
      DATA BR3DT/1400.,1400.,1445.,769.,1026.,1217.,1133.,1193.,904.,
     +  658.,499.,433.,383.,472.,493.,504.,350.,313.,224.,221.,158.,
     +  153.,119.,110.,100.,
     +  900.,900.,937.,557.,545.,830.,984.,1013.,688.,573.,514.,461.,
     +  409.,455.,510.,446.,387.,332.,298.,251.,216.,145.,126.,  0.,  0.,
     +  500.,500.,568.,377.,344.,640.,676.,547.,448.,434.,453.,395.,
     +  373.,550.,551.,471.,386.,301.,284.,233.,201.,142.,123.,  0.,  0.,
     +   0.,  0.,342.,297.,298.,310.,308.,352.,336.,347.,343.,428.,
     +  432.,521.,486.,440.,322.,280.,269.,212.,153.,140.,  0.,  0.,  0.,
     +   0.,  0.,229.,304.,238.,261.,320.,308.,287.,316.,309.,412.,
     +  431.,479.,458.,394.,312.,254.,247.,181.,148.,134.,113.,  0.,  0.,
     +   0.,  0.,262.,229.,225.,301.,279.,283.,295.,303.,316.,421.,
     +  427.,464.,424.,393.,304.,237.,227.,181.,142.,133.,  0.,  0.,  0.,
     +   0.,  0.,  0.,201.,243.,292.,272.,290.,324.,293.,341.,420.,
     +  424.,446.,360.,333.,280.,238.,208.,179.,142.,126.,  0.,  0.,  0.,
     +   0.,  0.,  0.,  0.,288.,306.,228.,288.,313.,335.,395.,371.,
     +  467.,424.,320.,294.,261.,215.,186.,170.,139.,  0.,  0.,  0.,  0.,
     +   0.,  0.,  0.,  0.,234.,250.,250.,299.,300.,426.,423.,422.,
     +  482.,432.,389.,304.,220.,204.,179.,141.,  0.,  0.,  0.,  0.,  0.,
     +   0.,  0.,  0.,  0.,  0.,267.,276.,361.,365.,461.,489.,432.,
     +  486.,420.,373.,269.,215.,196.,175.,138.,  0.,  0.,  0.,  0.,  0.,
     +   0.,  0.,  0.,  0.,  0.,  0.,353.,534.,543.,504.,548.,475.,
     +  435.,339.,262.,231.,204.,172.,164.,  0.,  0.,  0.,  0.,  0.,  0.,
     +   0.,  0.,  0.,  0.,  0.,  0.,475.,680.,663.,559.,428.,424.,
     +  298.,269.,237.,211.,188.,170.,160.,  0.,  0.,  0.,  0.,  0.,  0.,
     +   0.,  0.,  0.,  0.,  0.,  0.,  0.,723.,513.,467.,363.,303.,
     +  263.,262.,221.,196.,168.,163.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     +   0.,  0.,  0.,  0.,  0.,  0.,  0.,561.,301.,313.,296.,314.,
     +  252.,226.,196.,180.,155.,150.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     +   0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,264.,308.,279.,261.,
     +  200.,191.,172.,161.,130.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     +   0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,278.,245.,220.,
     +  182.,169.,159.,141.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     +   0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,188.,231.,195.,
     +  160.,140.,122.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     +   0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,158.,138.,
     +  132.,124.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     +   0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,140.,127.,
     +    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     +   0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,112.,
     +    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
     +   125*0./, BRSPACING/0.015/
C
CTSH      DATA DREF/'FF  ','BT  ','LZ  ','BR  '/
CTSH++
CHENN>
C       DATA TMPDREF/'FF  ','BT  ','LZ  ','BR  '/
C
      DATA TMPDREF/'FF  ','BT  ','LZ  ','BR  ','AQ  '/
CHENN<
CTSH--
      DATA DESCRIPTION/'IAM formfactors     ','B.t. cry3a          ',
     .  'Lysozyme            ','Bacteriorhodopsin   ',
     .  'Aquaporin           '/
C
      WRITE(6,1)
1     FORMAT(/'   SCALIMAMP3D - VX 3.00 (30-Oct-2005),'/
     .'    rescales amplitude fall-off & may put phase to 0/180')
C
C  The reference data -average radial amplitude - is now part of the program.
C   and has been calculated previously using the CCP4 program WILSON, and the
C   xy/z* program for BR3DT.
C
      IR=1
      READ(5,30) RREF   ! card 1 : select the reference data for scaling
30    FORMAT(A4)
CTSH      IF(RREF.EQ.'LZ  ') THEN
CTSH++
      IF(TMPRREF.EQ.'LZ  ') THEN
CTSH--
        WRITE(6,29)
29      FORMAT(' Lysozyme data not yet added, please use another option')
      ENDIF
      DO 31 J=1,NREF
      IF(RREF.EQ.DREF(J)) THEN
        WRITE(6,32) DESCRIPTION(J)
32      FORMAT(/' Reference data selected - ',A)
        IR=J
CHENN>
        if(IR.eq.5)IR=3
CHENN<
      ENDIF
31    CONTINUE
      READ(5,*) NPROG,TWOFOLD,BXYMIN,BXYMAX,BZMIN,BZMAX ! card 2 : 
      WRITE(6,34) NPROG,TWOFOLD,BXYMIN,BXYMAX,BZMIN,BZMAX
34    FORMAT(' Type of input data ORIGMERG(0)/AVRGAMPHS(1) =',I3/
     .       ' Phases will be constrained to 0/180 (T/F)      ', L1/
     .  ' BXY minimum and maximum B-factors to apply',2F10.0/
     .  ' BZ minimum and maximum B-factors to apply',2F10.0)
      READ(5,*) RESLIMXY,RESLIMZ,BEXTRA
      WRITE(6,35) RESLIMXY,RESLIMZ,BEXTRA
35    FORMAT(' RESOLUTION CUT-OFF FOR NPROG=0 SCALING, XY & Z',2F6.1/
     .  ' ADDITIONAL TEMPERATURE FACTOR TO APPLY TO SCALED DATA',F8.1)
      RESLIMXY=1.0/RESLIMXY
      RESLIMZ=1.0/RESLIMZ
C
C  Create reference data, reinterpolating on to new 1/d**2 resolution steps.
      IF(IR.EQ.1) THEN
        CALL FSTAB(ATAB)
        WRITE(6,49) (ATAB(J),J=1,400,50)
49      FORMAT(' Refamps in 1/10 A**-1 steps', 10F5.1)
        DO 50 J=1,NSLOTS
                SVALUE=(J-0.5)*IRESTEP
                NFTAB=SQRT(SVALUE/10000.)/0.002
                IF(NFTAB.GT.400) STOP ' FTAB dimensions too small'
50      AVERREF(J)=ATAB(NFTAB)
      ENDIF
      IF(IR.EQ.2) THEN
        DO 52 J=1,NSLOTS
                SVALUE=(J-0.5)*IRESTEP
                SAMPLING=SVALUE/BTSPACING
                IF(SAMPLING.LT.1.0) SAMPLING=1.0
                JS=SAMPLING
                FS=SAMPLING-JS
        IF(JS.LT.33) THEN 
          AVERREF(J)=BTCRY3A(JS)*(1.0-FS) + BTCRY3A(JS+1)*FS ! linear interp
        ELSE
          WRITE(6,48)J
48        FORMAT(' BT reference data limited, ISLOT=',I5)
        ENDIF
52      CONTINUE
        INTERV=250/IRESTEP
        WRITE(6,53) ((0.1*AVERREF(J)),J=1,NSLOTS,INTERV)
53      FORMAT(' Refamp approx 1/40 A**-2 step', 10F5.0)
      ENDIF
      IF(IR.EQ.3) THEN
C transfer data from array not yet created
      ENDIF
      IF(IR.EQ.4) THEN
        IF(NPROG.GE.1) THEN
           DO 56 J=1,NSLOTS
              SVALUE=(J-0.5)*IRESTEP
              BRTAB=SQRT(SVALUE/10000.)/BRSPACING
              IF(BRTAB.LE.1.0) BRTAB=1.000001
              NBRTAB=BRTAB
              NBRTAB1=NBRTAB+1
              FBRTAB=BRTAB-NBRTAB
C  this has been fudged to allow slight extrapolation 1 slot beyond INTERVALBR
              IF(NBRTAB.GT.INTERVALBR+1) THEN
                 WRITE(6,55)J
55               FORMAT(' BR3DT projection data limited, ISLOT=',I5)
                 STOP
              ENDIF
              IF(NBRTAB.GE.INTERVALBR) THEN
                      NBRTAB=INTERVALBR
                      NBRTAB1=INTERVALBR
                      FBRTAB=0.0
              ENDIF
              AVERREF(J)=BR3DT(NBRTAB,1)*(1.0-FBRTAB) +
     .              BR3DT(NBRTAB1,1)*FBRTAB
56         CONTINUE
        ENDIF
      ENDIF
C
C  next, read in new data to be scaled
C
100   READ(5,40,END=300) FILIN          ! can be multiple datasets.
40    FORMAT(A)
      NIN=NIN+1
      CALL CCPDPN(11,FILIN,'READONLY','F',0,0)
C      OPEN(UNIT=11,FILE=FILIN,READONLY,STATUS='OLD')
44     FORMAT(/' Data to be scaled       ',40('*'))
      WRITE(6,44)
      WRITE(6,42) FILIN
42    FORMAT(5X,A)
      READ(11,45)TITLE
      WRITE(6,46)TITLE
45     FORMAT(12A4)
46     FORMAT(' title on input data -',12A4)
      READ(5,*)A,B,GAMMA,RESOL
      WRITE(6,43)A,B,GAMMA,RESOL
      IF(GAMMA.LT.90.0) GAMMA=180.0-GAMMA
43    FORMAT(' Cell dimensions',3F10.2/
     .       ' Resolution     ', F10.2)
      ASTAR=1.0/(A*SIN(DRAD*GAMMA))
      BSTAR=1.0/(B*SIN(DRAD*GAMMA))
      GAMMASTAR=DRAD*(180.0-GAMMA)
      DO 150 I=1,NMAX
        IF(NPROG.EQ.0) THEN     ! origmerg style data
                READ(11,*,END=160) IH(I),IK(I),ZSTAR(I),AMP(I),PHASE(I),
     .          JFILM(I),JQ(I),WGT(I),BCK(I),CTFS(I)
        ELSE                    ! avrgamphs style data
          IF(NPROG.EQ.2) THEN
                READ(11,*,END=160) IH(I),IK(I),IL(I),AMP(I),SIGAMP(I)
          ELSE          ! i.e. for NPROG.EQ.1
                READ(11,*,END=160) IH(I),IK(I),IL(I),AMP(I),PHASE(I),FOM(I)
          ENDIF
        ENDIF
150   CONTINUE
160   NDATA=I-1
      WRITE(6,161) NDATA
161    FORMAT(' Number of data for scaling =',I6/)
      CLOSE(UNIT=11)
C
C  need to treat two types of data separately
C
      IF(NPROG.EQ.0) THEN
        CALL SCALORIGTILT(NDATA,IH,IK,ZSTAR,AMP,JFILM,BCK,CTFS,
     .          AMPOUT,BCKOUT,ASTAR,BSTAR,GAMMASTAR,
     .          BR3DT,BRSPACING,AVERREF,IR,IRESTEP,
     .          BXYMIN,BXYMAX,BZMIN,BZMAX,RESLIMXY,RESLIMZ,BEXTRA)
      ELSE
C  put data into resolution bins
        DO 170 J=1,NSLOTS
        AVERDAT(J)=0.0
        PHASERR(J)=0.0
170     NSPOTDAT(J)=0
C
        DO 180 I=1,NDATA
          DSTARSQ = IH(I)**2*ASTAR*ASTAR + IK(I)**2*BSTAR*BSTAR + 
     .          2.0*IH(I)*IK(I)*ASTAR*BSTAR*COS(GAMMASTAR)
          IF(DSTARSQ.LT.1.0/RESOL**2) THEN
                IRES=DSTARSQ*10000.
                ISLOT= 1 + IRES/IRESTEP
                IF(ISLOT.LT.1.OR.ISLOT.GT.NSLOTS) THEN
                        WRITE(6,20000)ISLOT
20000                   FORMAT(' ERROR, ISLOT=',I10)
                        STOP
                END IF
                AVERDAT(ISLOT) = AVERDAT(ISLOT) + AMP(I)
                ERROR=AMIN1(ABS(PHASE(I)),ABS(180.0-ABS(PHASE(I))))
                PHASERR(ISLOT) = PHASERR(ISLOT) + ERROR
                NSPOTDAT(ISLOT)= NSPOTDAT(ISLOT) + 1
          ENDIF
180     CONTINUE
        DO 190 J=1,NSLOTS
          IF(NSPOTDAT(J).NE.0)
     .          AVERDAT(J)=AVERDAT(J)/NSPOTDAT(J)
190     CONTINUE
C
C       now calculate scale factor between new data and reference data and 
C       plot out resolution dependent fall-off for CURVY.
C
        CALL CCPDPN(2,'SCALIMAMP3D.DAT','UNKNOWN','F',0,0)
C       OPEN(UNIT=2,FILE='SCALIMAMP3D.DAT',FORM='FORMATTED',
C     .         STATUS='UNKNOWN')
        IF(NIN.EQ.1) THEN
          WRITE(2,20001) DESCRIPTION(IR)
20001     FORMAT('T=Resolution Dependence of Scale-Factor vs ',A)
          WRITE(2,20002)
20002     FORMAT('X=10000/D**2')
          WRITE(2,20003)
20003     FORMAT('Y=log (ratio AMP/REFAMP)')
        ENDIF
CTSH              IF(NIN.EQ.1) WRITE(TEXT,20004) FILIN
CTSH              IF(NIN.EQ.2) WRITE(TEXT,20005) FILIN
CTSH              IF(NIN.EQ.3) WRITE(TEXT,20006) FILIN  
CTSH              IF(NIN.EQ.4) WRITE(TEXT,20007) FILIN  
CTSH              IF(NIN.EQ.5) WRITE(TEXT,20008) FILIN  
CTSH              IF(NIN.EQ.6) WRITE(TEXT,20009) FILIN  
CTSH              IF(NIN.EQ.7) WRITE(TEXT,20010) FILIN  
CTSH              IF(NIN.EQ.8) WRITE(TEXT,20011) FILIN  
CTSH++
          IF(NIN.EQ.1) WRITE(TEXT,20004) FILIN(1:30)
          IF(NIN.EQ.2) WRITE(TEXT,20005) FILIN(1:30)
          IF(NIN.EQ.3) WRITE(TEXT,20006) FILIN(1:30)
          IF(NIN.EQ.4) WRITE(TEXT,20007) FILIN(1:30)
          IF(NIN.EQ.5) WRITE(TEXT,20008) FILIN(1:30)
          IF(NIN.EQ.6) WRITE(TEXT,20009) FILIN(1:30)
          IF(NIN.EQ.7) WRITE(TEXT,20010) FILIN(1:30)
          IF(NIN.EQ.8) WRITE(TEXT,20011) FILIN(1:30)
CTSH--
        WRITE(2,20020) TEXT
20004   FORMAT('L=',A,' !*!1!1   ')
20005   FORMAT('L=',A,' !+!1!1   ')
20006   FORMAT('L=',A,' !O!1!1   ')
20007   FORMAT('L=',A,' !X!1!1   ')
20008   FORMAT('L=',A,' !#!1!1   ')
20009   FORMAT('L=',A,' !=!1!1   ')
20010   FORMAT('L=',A,' !I!1!1   ')
20011   FORMAT('L=',A,' !H!1!1   ')
CJMS--
C20020  FORMAT(13A4)
20020   FORMAT(A)
        WRITE(6,20051)
        PLOTSCALE=1000.
        DO 195 J=1,NSLOTS
          SUMREF=0.0
          SUMDAT=0.0
          SUMN(J)=0.0
          DO 200 K=-NSMOOTH,NSMOOTH     ! smooth out ratio over +/- NSMOOTH
                L=J+K
                IF(L.GT.0.AND.L.LE.NSLOTS) THEN
                        IF(NSPOTDAT(L).NE.0.AND.AVERREF(L).NE.0.) THEN
                                SUMREF=SUMREF+AVERREF(L)
                                SUMDAT=SUMDAT+AVERDAT(L)        
                                SUMN(J)=SUMN(J)+1.0
                        ENDIF
                ENDIF
200       CONTINUE
          IF(SUMN(J).NE.0.AND.SUMREF.NE.0.AND.SUMDAT.NE.0) THEN
                RESPLOT=(J-0.5)*IRESTEP ! 10000/D**2, D in Angstroms
                RESOUT=1.0/SQRT(RESPLOT/10000.0)
C         IF(SUMREF.EQ.0.0) THEN
C         WRITE(6,*)J,SUMN(J),SUMDAT,SUMREF
C         ENDIF
                RATIOSMOOTH=SUMDAT/SUMREF
                SCALFACT(J)=1.0/RATIOSMOOTH     ! scale factor to be applied.
                RATPLOTSMOOTH=LOG(RATIOSMOOTH)
                IF(PLOTSCALE.GT.999.) PLOTSCALE=RATPLOTSMOOTH
                RATPLOTSMOOTH=RATPLOTSMOOTH-PLOTSCALE   ! scale to 0 at origin
                IF(AVERREF(J).NE.0.0) THEN
                        RATIO=AVERDAT(J)/AVERREF(J)
                ELSE
                        RATIO=0.0       ! no data in this resolution slot
                ENDIF
                IF(NSPOTDAT(J).NE.0) PHASERR(J)=PHASERR(J)/NSPOTDAT(J)
                IF(RESOL.LE.RESOUT) THEN
                   WRITE(2,20052) RESPLOT,RATPLOTSMOOTH
                   WRITE(6,20050) J,RESOUT,RATIO,RATIOSMOOTH,
     .                  AVERREF(J),AVERDAT(J),NSPOTDAT(J),
     .                  PHASERR(J)
                ENDIF
          ENDIF
195     CONTINUE
20052   FORMAT(2F10.5)
20050   FORMAT(I4,F11.1,2F10.5,2F10.1,I7,F10.1)
20051   FORMAT('   N Resolution  Fall-off  Smoothed    Refamp',
     .          '  Avdatamp   Ndat    phaserr(0/180)')
C
        SCALMIN=1.0E10
        DO 250 J=1,NSLOTS
          IF(SCALFACT(J).GT.0.0) SCALMIN=AMIN1(SCALMIN,SCALFACT(J))
250     CONTINUE
      ENDIF
C
C write out rescaled image amplitudes, using linearly interpolated scale-factor
C
      CALL CCPDPN(3,'OUT','UNKNOWN','F',0,0)
C      OPEN(UNIT=3,FILE='OUT',FORM='FORMATTED',STATUS='UNKNOWN')
      WRITE(3,281)TITLE
281   FORMAT(12A4,'     rescaled by SCALIMAMP3D    ')
      IF(NPROG.EQ.0) THEN
C      origmerg scaling
        DO 260 I=1,NDATA
          WRITE(3,262) IH(I),IK(I),ZSTAR(I),AMPOUT(I),PHASE(I),
     .            JFILM(I),JQ(I),WGT(I),BCKOUT(I),CTFS(I)
262       FORMAT(1X,2I4,F8.4,F10.1,F7.1,I7,I3,F8.5,F10.1,F7.3)
260     CONTINUE
      ELSE
        DO 280 I=1,NDATA
          DSTARSQ = IH(I)**2*ASTAR*ASTAR + IK(I)**2*BSTAR*BSTAR + 
     .    2.0*IH(I)*IK(I)*ASTAR*BSTAR*COS(GAMMASTAR)
          IF(DSTARSQ.LT.1.0/RESOL**2) THEN
                  IRES=DSTARSQ*10000. 
                  ISLOT= 1 + IRES/IRESTEP
                  DELTA=0.5 + (FLOAT(IRES)/FLOAT(IRESTEP))-ISLOT ! between +/- 0.5
             IF(DELTA.LT.0.0.AND.ISLOT.GE.2) THEN
                  ISLOT=ISLOT-1
                  DELTA=DELTA+1.0
             ELSE
                  DELTA=0.0       ! for ISLOT=1
             ENDIF
            IF(SCALFACT(ISLOT).GT.0.) THEN
             IF(SCALFACT(ISLOT+1).LT.0.)DELTA=0. ! treats outer edge
             SCALE=(1.0-DELTA)*SCALFACT(ISLOT)+DELTA*SCALFACT(ISLOT+1)
             IF(SCALE.LE.0.0) THEN
                  WRITE(6,*)ISLOT,DELTA,SCALFACT(ISLOT),SCALFACT(ISLOT+1)
             ENDIF
             SCALE=SCALE/SCALMIN  ! minimise scale change.
             AMPOUT(I)=AMP(I)*SCALE
             SIGAMPOUT(I)=SIGAMP(I)*SCALE
             IF(TWOFOLD) THEN
                  FOM(I)=FOM(I)*ABS(COS(DRAD*PHASE(I)))
                  IF(ABS(PHASE(I)).GT.180.0) THEN ! normally -180 to +180
                     PHASE(I)=PHASE(I)-SIGN(360.0,PHASE(I)) ! just in case
                  ENDIF
                  IF(ABS(PHASE(I)).GT.90.0) PHASE(I)=180.0
                  IF(ABS(PHASE(I)).LE.90.0) PHASE(I)=0.0
             ENDIF
             IF(NPROG.EQ.2) THEN
                  WRITE(3,282) IH(I),IK(I),IL(I),AMPOUT(I),SIGAMPOUT(I)
             ELSE         ! i.e. NPROG=1
                  WRITE(3,282) IH(I),IK(I),IL(I),AMPOUT(I),PHASE(I),FOM(I)
282          FORMAT(3I4,2F8.1,F8.3)
             ENDIF
            ENDIF
          ENDIF
280     CONTINUE
        CLOSE(UNIT=3)
      ENDIF
C
      IF(NPROG.GE.1.AND.NIN.LT.NDATAMAX)GO TO 100    ! repeat AVRGAMPHS possible
C
300   CONTINUE          ! stop here
      END
C*FSTAB*********************************************************************
C
C     SUBROUTINE TO SETUP FORM FACTOR TABLE
C     THIS VERSION FOR ELECTRON FORM FACTORS
C
      SUBROUTINE FSTAB(ATAB)
        DIMENSION FX(9,5),FRDIV(100),FTAB(400,5),ATAB(400)
C
C 1/d     0.0    0.1    0.2    0.3    0.4    0.5    0.6    0.7    0.8
        DATA FX/
     C   2.509, 2.406, 2.138, 1.796, 1.460, 1.168, 0.932, 0.748, 0.606,
     N   2.211, 2.144, 1.963, 1.718, 1.458, 1.216, 1.006, 0.831, 0.689,
     O   1.983, 1.937, 1.808, 1.625, 1.422, 1.222, 1.040, 0.881, 0.747,
     P   5.488, 5.192, 4.457, 3.586, 2.796, 2.169, 1.702, 1.362, 1.115,
     S   5.161, 4.938, 4.362, 3.635, 2.927, 2.326, 1.851, 1.490, 1.218/
C
C   SET UP FORM FACTOR TABLE BY QUADRATIC INTERPOLATION
C
C   THESE FORMFACTORS ARE EXACTLY AS IN THE INTERNATIONAL TABLES, BUT
C   WERE INCREASED 4-FOLD IN THE GENSFC INPUT CALCULATIONS, THEREFORE
C   THEY HAVE ALSO BEEN INCREASED BY 4-FOLD HERE.
C
        NDIV = 50
        NF = 5
        DO 300 J = 1,NF         ! five atom types
C   LOOP IS ONLY OVER 7 BECAUSE OF QUAD INTERP
          DO 200 I = 1,7 
            NDIVDO = 50
            IF (I .EQ. 7) NDIVDO = 100
            DO 100 NUDIV = 1,NDIVDO
              ANUDIV = NUDIV - 1
              FRDIV(NUDIV) = ANUDIV/NDIV
              K = (I - 1)*NDIV + NUDIV
              FTAB(K,J) = 4.0 *
     .        (FX(I,J) + (FX(I+1,J) - FX(I,J))*FRDIV(NUDIV) +
     .        0.5*FRDIV(NUDIV)*(FRDIV(NUDIV) - 1.0)*(FX(I+2,J) -
     .        2.0*FX(I+1,J) + FX(I,J)))
100         CONTINUE
200       CONTINUE
300     CONTINUE
C
      DO 400 J=1,400    ! adjust for approximate proportion atoms in protein
400   ATAB(J)=0.5*FTAB(J,1)+0.2*FTAB(J,2)+0.2*FTAB(J,3)+
     .          0.05*FTAB(J,4)+0.05*FTAB(J,5)
C
      RETURN
      END
C*SCALORIGTILT*****************************************************************
      SUBROUTINE SCALORIGTILT(NDATA,IH,IK,ZSTAR,AMP,JFILM,BCK,CTFS,
     .          AMPOUT,BCKOUT,ASTAR,BSTAR,GSTAR,
     .          BR3DT,BRSPAC,AVERREF,IR,IRESTEP,
     .          BXYMIN,BXYMAX,BZMIN,BZMAX,RESLIMXY,RESLIMZ,BEXTRA)
      PARAMETER (NMAX=80000)
      PARAMETER (NSLOTS=200)
      PARAMETER (NFILMS=200)
      PARAMETER (INTERVALBR=25)
      DIMENSION IH(NMAX),IK(NMAX),ZSTAR(NMAX),AMP(NMAX),CTFS(NMAX),
     .  BCK(NMAX),AMPOUT(NMAX),BCKOUT(NMAX),
     .  XYSTARSQ(NMAX),IFP(NMAX)
      INTEGER*8 JFILM(NMAX),IFILM(NFILMS)
      DIMENSION BR3DT(INTERVALBR,INTERVALBR),AVERREF(NSLOTS)
      DIMENSION A1(NFILMS),A2(NFILMS),A3(NFILMS),
     .  B1(NFILMS),B2(NFILMS),B3(NFILMS),C1(NFILMS),C2(NFILMS),
     .  C3(NFILMS),D1(NFILMS),D2(NFILMS),D3(NFILMS),
     .  BXY(NFILMS),BZ(NFILMS),ASCALE(NFILMS)
      DATA A1/NFILMS*0./,A2/NFILMS*0./,A3/NFILMS*0./,
     .  B1/NFILMS*0./,B2/NFILMS*0./,B3/NFILMS*0./,
     .  C1/NFILMS*0./,C2/NFILMS*0./,C3/NFILMS*0./,
     .  D1/NFILMS*0./,D2/NFILMS*0./,D3/NFILMS*0./
C
C  keep in same sorted order as in origtilt o/p - inefficient but simpler.
C  first count the number of different films in list.
      NF=1
      IFILM(1)=JFILM(1)
      IFP(1)=1          ! film pointer
      DO 100 J=2,NDATA
        DO 120 K=1,NF
                IF(JFILM(J).EQ.IFILM(K)) THEN
                        IFP(J)=K        ! film pointer
                        GO TO 100
                ENDIF
120     CONTINUE
        NF=NF+1
        IFILM(NF)=JFILM(J)
        IFP(J)=NF       ! film pointer
100   CONTINUE
C
      WRITE(6,101) NF
101   FORMAT(' For ORIGTILT data, number of different films =',I5)
      IF(NF.GT.NFILMS) STOP ' Number of films exceeds param NFILMS'
      DO 200 J=1,NDATA
200   XYSTARSQ(J)=IH(J)**2*ASTAR**2+IK(J)**2*BSTAR**2+2.0*
     .                  IH(J)*IK(J)*ASTAR*BSTAR*COS(GSTAR)
C
C  Accumulate data needed to calculate Bfactors needed to rescale the data
C    with both resolution in the plane and with zstar.
C
      DO 300 J=1,NDATA
        LF=IFP(J)
C  calculation of Aref and Aobs
        AOBS=AMP(J)/AMAX1(0.2,ABS(CTFS(J))) ! correct maximum 5x for CTFscale
C       W = AMIN1((AMP(J)/BCK(J))**2,10.0)   ! not too much weight for big AMPs
        W = 1.0
        IF(AOBS.LT.BCK(J)) AOBS=0.25*BCK(J)     ! noise control
        XYST = SQRT(XYSTARSQ(J))
        DSTARSQ = XYSTARSQ(J) + ZSTAR(J)**2
        IF(XYST.GT.RESLIMXY.OR.ZSTAR(J).GT.RESLIMZ) GO TO 300
        IF(IR.LE.3) THEN        ! i.e. not 3D-BR data
          ISLOT = 1 + DSTARSQ*10000./IRESTEP
          AREF = AVERREF(ISLOT)
        ELSE
C  bilinear interpolation in BR3DT (IR.eq.4) beyond the first bin.
          NZ = ABS(ZSTAR(J)/BRSPAC)
          FZ = ABS(ZSTAR(J)/BRSPAC) - NZ
          IF(NZ.EQ.0) THEN
                  NZ=1
                  FZ=0.
          ENDIF
          NXY = XYST/BRSPAC
          FXY = XYST/BRSPAC - NXY
          IF(NXY.EQ.0) THEN
                  NXY=1
                  FXY=0.
          ENDIF
          IF(NZ.LT.INTERVALBR.AND.NXY.LT.INTERVALBR.AND.
     .                            BR3DT(NXY+1,NZ+1).GT.0.) THEN
            AREF=FZ*(FXY*BR3DT(NXY+1,NZ+1)+(1.0-FXY)*BR3DT(NXY,NZ+1))
     .            +(1.0-FZ)*(FXY*BR3DT(NXY+1,NZ)+(1.0-FXY)*BR3DT(NXY,NZ))
          ELSE
            WRITE(6,201) 
201         FORMAT(' WARNING - BR3DT data too low resolution or too big',
     .            ' missing cone for input data')
            GO TO 300
C           STOP ' BR3DT data too low resolution for input data'
          ENDIF
        ENDIF
        A1(LF)=A1(LF)+0.25*XYSTARSQ(J)*W
        A2(LF)=A2(LF)+0.25*XYSTARSQ(J)**2*W
        A3(LF)=A3(LF)+0.25*XYSTARSQ(J)*ZSTAR(J)**2*W
        B1(LF)=B1(LF)+0.25*ZSTAR(J)**2*W
        B2(LF)=B2(LF)+0.25*XYSTARSQ(J)*ZSTAR(J)**2*W
        B3(LF)=B3(LF)+0.25*ZSTAR(J)**4*W
        C1(LF)=C1(LF)+W
        C2(LF)=C2(LF)+XYSTARSQ(J)*W
        C3(LF)=C3(LF)+ZSTAR(J)**2*W
        D1(LF)=D1(LF)+ALOG(AREF/AOBS)*W
        D2(LF)=D2(LF)+ALOG(AREF/AOBS)*XYSTARSQ(J)*W
        D3(LF)=D3(LF)+ALOG(AREF/AOBS)*ZSTAR(J)**2*W
300   CONTINUE
C  calculate and write out sharpening factors
      WRITE(6,380)
380   FORMAT('  FILMNO       BXY        BZ',
     .  '  B-factors, ASCALE truncated by input limits')
      DO 400 I=1,NF
        P1=C2(I)*A1(I)-A2(I)*C1(I)
        Q1=C2(I)*B1(I)-B2(I)*C1(I)
        R1=C2(I)*D1(I)-D2(I)*C1(I)
        P2=C3(I)*A2(I)-A3(I)*C2(I)
        Q2=C3(I)*B2(I)-B3(I)*C2(I)
        R2=C3(I)*D2(I)-D3(I)*C2(I)
        IF(P2*Q1-Q2*P1.EQ.0.0)  THEN
          IF(P2*R1-R2*P1.NE.0.0) 
     .      STOP ' P2*R1-R2*P1 is not zero, but P2*Q1-Q2*P1 is zero'
          BZ(I)=0.0       ! does not matter because film is probably untilted
        ELSE
          BZ(I)=(P2*R1-R2*P1)/(P2*Q1-Q2*P1)
        ENDIF
        BXY(I)=(R1-Q1*BZ(I))/P1
        IF(BZ(I).LT.BZMIN.OR.BZ(I).GT.BZMAX.OR.
     .    BXY(I).LT.BXYMIN.OR.BXY(I).GT.BXYMAX) THEN
          BXYOLD=BXY(I)
          BZOLD=BZ(I)
          BZ(I)=AMAX1(BZ(I),BZMIN)
          BZ(I)=AMIN1(BZ(I),BZMAX)
          BXY(I)=AMAX1(BXY(I),BXYMIN)
          BXY(I)=AMIN1(BXY(I),BXYMAX)
          ASCALE(I)=EXP((D1(I)-BXY(I)*A1(I)-BZ(I)*B1(I))/C1(I))
          WRITE(6,392) IFILM(I),BXYOLD,BZOLD,BXY(I),BZ(I),ASCALE(I)
392       FORMAT(I10,2F10.1,5X,2F10.1,F10.3)
393       FORMAT(I10,2F10.1,25X,F10.3)
        ELSE
          ASCALE(I)=EXP((D1(I)-BXY(I)*A1(I)-BZ(I)*B1(I))/C1(I))
          WRITE(6,393) IFILM(I),BXY(I),BZ(I),ASCALE(I)
        ENDIF
400   CONTINUE
C  normalise scale factors to leave the average near 1.0
      RENORM=0.0
      WRITE(6,451)
451   FORMAT(/' Scale factors readjusted'/)
      DO 450 I=1,NF
        RENORM=RENORM+ASCALE(I)
450   CONTINUE
        RENORM=RENORM/NF
      DO 460 I=1,NF
        ASCALE(I)=ASCALE(I)/RENORM
        WRITE(6,461)I,ASCALE(I)      
461     FORMAT(' Film',I5,'  normalised scale factor',F8.3)
460   CONTINUE
C  apply correction to all data
      DO 500 J=1,NDATA
        LF=IFP(J)
        RESCALE=ASCALE(LF) * EXP( (BEXTRA+BXY(LF)) * XYSTARSQ(J)/4.0 + 
     .          (BEXTRA+BZ(LF)) * ZSTAR(J)**2/4.0)
        AMPOUT(J)=AMP(J)*RESCALE
500   BCKOUT(J)=BCK(J)*RESCALE
      RETURN
      END
