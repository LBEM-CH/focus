C*LLFILT.FOR******************************************************************
C     WHITTAKER-SHANNON INTERPOLATION ALONG LATTICE LINES
C     VERSION 1.0 FOR P1 WITH GRAPHICAL OUTPUT 25NOV81
C     VERSION 1.1 WITH FILTERING FOR LEAST SQUARES    25 AUG 82
C     VERSION 1.2 Adapted for VAX   1JUL83
C
C     Compile LLFILT+LINFIL+SHLSRT     Then PIMLINK.
C
C     TITLE is header for plots
C     THICK is virtual specimen thickness in angstroms used to set
C            spacing of sinc functions
C     FILT  is filter level for least squares  e.g. .01
C     CUTOFF  is Fourier cutoff for fitted structure factors in
C             reciprocal Angstroms.
C     IGRPH = 0  no graphical output
C             1  graphs of layer line data plotted
C     FSCA  = 0. gives auto scaling
C         .NE.0. gives constant scaling with FMAX=FSCA
C     ZMIN, ZMAX  min max zstar along lattice lines - used for drawing
C            box for graphs
C     ACELL, BCELL, GAMMA  cell dimensions (angstroms, degrees) in plane
C           (used for calculating Fourier cutoff)
C
      character*80 cline
      DIMENSION IH(5000),IK(5000),ZS(5000),AMP(5000),PHASE(5000),
     1 IFILM(5000),IQ(5000)
      DIMENSION AB(200,2),ZLDASH(200),F(200),P(200),ZSTAR(200),
     1 GMAT(200,50),ABCALC(200,2),ATA(50,50),ATB(50,2),VEC(50,50),
     2 VAL(50),IPOINT(50),W(250),ZOUT(50),FOUT(50),PHIOUT(50)
      DIMENSION TITLE(10)
      LOGICAL LAST
C
      READ(5,9) TITLE
9     FORMAT(10A4)
      READ(5,*) THICK,FILT,CUTOFF,IGRPH,FSCA,ZMIN,ZMAX
      WRITE(6,10) TITLE,THICK,FILT,CUTOFF,IGRPH,FSCA,ZMIN,ZMAX
10    FORMAT('1'///' WHITTAKER-SHANNON INTERPOLATION ON LATTICE LINES',
     .       ///'  Title :   ',10A4,
     1       ///'  Specimen thickness    ',F10.1,'  Angstroms',
     2         /'  Filter level          ',F10.4,
     3         /'  Cutoff                ',F10.5,' Rec Angstroms',
     4         /'  IGRPH (0/1 N/Y)       ',I5,
     5         /'  FSCA (Auto/fixed scale)',F10.1,
     6         /'  ZMIN for graph         ',F10.4,
     7         /'  ZMAX for graph         ',F10.4)
      READ(5,*) ACELL,BCELL,GAMMA
      WRITE(6,11) ACELL,BCELL,GAMMA
11    FORMAT(///'  ACELL=',F10.1,' Angstroms',
     1         /'  BCELL=',F10.1,' Angstroms',
     2         /'  GAMMA=',F10.1,' degrees')
C
      WRITE(6,20)
20    FORMAT(///'   Input data from file :',//)
      CALL CCPDPN(1,'IN','READONLY','F',0,0)
      CALL CCPDPN(2,'OUT','NEW','F',0,0)
      LAST=.FALSE.
      N=0
100   N=N+1
      READ(1,*,END=110) IH(N),IK(N),ZS(N),AMP(N),PHASE(N),IFILM(N),IQ(N)
      GO TO 100
110   NREFL=N-1
      CRAD=3.14159/180.
      CGAMST=COS((180.-GAMMA)*CRAD)
      ASTAR=1./(ACELL*SIN(GAMMA*CRAD))
      BSTAR=1./(BCELL*SIN(GAMMA*CRAD))
      IIH=IH(1)
      IIK=IK(1)
      NR=0
200   NPTS=0
      FMAX=0.
205   NR=NR+1
      IF(IIH.NE.IH(NR).OR.IIK.NE.IK(NR)) GO TO 210
      NPTS=NPTS+1
      ARG=PHASE(NR)*CRAD
      AB(NPTS,1)=AMP(NR)*COS(ARG)
      AB(NPTS,2)=AMP(NR)*SIN(ARG)
      ZLDASH(NPTS)=ZS(NR)*THICK
      ZSTAR(NPTS)=ZS(NR)
      F(NPTS)=AMP(NR)
      IF(F(NPTS).GT.FMAX) FMAX=F(NPTS)
      P(NPTS)=PHASE(NR)
      IF(NR.EQ.NREFL) GO TO 210
      GO TO 205
210   D=SQRT((IIH*ASTAR)**2+(IIK*BSTAR)**2+2.*IIH*IIK*ASTAR*
     1BSTAR*CGAMST)
      IF(D.GT.CUTOFF) GO TO 211
C
C     Set limits
      LMAX=ZLDASH(NPTS)+0.5
      LMIN=ZLDASH(1)-0.5
      NL=LMAX-LMIN+1
      WRITE(6,12) NL,LMAX,LMIN
12    FORMAT(3I10)
C
C     Set up observational matrix with sinc interpolation
      DO 310 NP=1,NPTS
      DO 320 L=1,NL
      LL=L+LMIN-1
      ARG=(LL-ZLDASH(NP))*3.14159
      IF(ABS(ARG).LE.1.E-5) GO TO 315
      GMAT(NP,L)=SIN(ARG)/ARG
      GO TO 320
315   GMAT(NP,L)=1.
320   CONTINUE
310   CONTINUE
C
      CALL LINFIL(GMAT,AB,ABCALC,200,NPTS,NL,2,FILT,NFILT,ATA,ATB,
     1 VEC,VAL,IPOINT,W)
      WRITE(6,13)IIH,IIK,NFILT,NL
13    FORMAT(//' Lattice line  H=',I5,'  K=',I5,
     1 //I10,' out of',I6,' eigenvalues above filter level')
      WRITE(6,14) (VAL(IPOINT(L)),L=1,NL)
14    FORMAT(//' Eigenvalues',5(/5X,10E12.4))
C
C     Output
      CDEG=180./3.14159
      DO 300 I=1,NL
      L=I+LMIN-1
      FITF=SQRT(ABCALC(I,1)*ABCALC(I,1)+ABCALC(I,2)*ABCALC(I,2))
      AP=ABCALC(I,1)/FITF
      BP=ABCALC(I,2)/FITF
      PHI=ATAN2(BP,AP)*CDEG
      WRITE(6,15) IIH,IIK,L,ABCALC(I,1),ABCALC(I,2),FITF,PHI
15    FORMAT(3I5,4F10.0)
      D1=SQRT(D*D+FLOAT(L*L)/(THICK*THICK))
      IF(D1.GT.CUTOFF) GO TO 410
      WRITE(2,16) IIH,IIK,L,FITF,PHI
16    FORMAT(3I5,2F10.1)
410   ZOUT(I)=L/THICK
      FOUT(I)=FITF
      PHIOUT(I)=PHI
300   CONTINUE
C
      IF(IGRPH.EQ.0) GO TO 211
      IF(NR.EQ.NREFL) LAST=.TRUE.
      IF(FSCA.NE.0.) FMAX=FSCA
      DELPLT=1./(4.*THICK)
      CALL GRAPH(ZMIN,ZMAX,FMAX,IIH,IIK,NPTS,F,P,ZSTAR,DELPLT,TITLE,
     1            LAST,NL,ZOUT,FOUT,PHIOUT,THICK)
211   IF(NR.EQ.NREFL) GO TO 400
      IIH=IH(NR)
      IIK=IK(NR)
      NR=NR-1
      GO TO 200
400   STOP
      END
C*GRAPH**********************************************************************
C
C  PLOT AMPLITUDES AND PHASES ALONG EACH (H,K) LINE ON PLOTTER.
C  PART OF LATLINE LATTICE-LINE FITTING PACKAGE
C    Adapted RAC   4JUL83  for LLFILT output
C
C  NOBS    - NO. OF OBSERVATIONS OF AMP AND PHASE.
C  FOBS    - ARRAY OF OBSERVED AMPS.
C  PHIOBS  - ARRAY OF OBSERVED PHASES.
C  ZSTAR   - ARRAY OF OBSERVED ZSTARS.
C  ZMIN    - MINIMUM Z IN PLOT.
C  ZMAX    - MAXIMUM Z IN PLOT.
C  FMAX    - MAXIMUM AMP IN PLOT.
C  IHIN    - H INDEX OF LATTICE LINE.
C  IKIN    - K INDEX OF LATTIEC LINE.
C  DELPLT  - PLOT INTERVAL FOR FITTED CURVE
C  LAST    - .TRUE. IF THIS IS LAST PLOT (ELSE= .FALSE.)
C  NOUT    - # OF LATTICE POINTS
C  ZOUT    - ZSTAR FOR LATTICE POINTS
C  FOUT    - F'S FOR LATTICE
C  PHIOUT  - PHSES FOR LATTICE
C
C  ZSCALE IS 6MM=0.01 A-1,FSCALE HEIGHT=100MM,PSCALE HEIGHT=60MM.
C  FSCALE = 150MM FOR INTENSITY PLOTS
C       ZSCALE IS NOW AUTOMATICALLY DOUBLED UNTIL GRAPH IS AT LEAST 100 MM WIDE!
C
C
        SUBROUTINE GRAPH(ZMIN,ZMAX,FMAX,IHIN,IKIN,NOBS,FOBS,PHIOBS,
     .   ZSTAR,DELPLT,TITLE,LAST,NOUT,ZOUT,FOUT,PHIOUT,THICK)
        character*80 cline
        DIMENSION ZOUT(1),FOUT(1),PHIOUT(1),FOBS(1),PHIOBS(1),
     .   ZSTAR(1),TITLE(1)
        DIMENSION LINE(20),FINT(200),PHASE(200)
        LOGICAL LAST
        DATA CNV/57.2957795/, INIT/0/
C
        ZMAG=600.
        FMAG=100.
        PMAG=60.
        GAP=8.
        DELZ = .05
        IF(INIT.EQ.1) GO TO 5
        CALL P2K_OUTFILE('PLOT.PS',7)
        CALL P2K_HOME
        INIT=1
5       ZRANG=ZMAX-ZMIN
6       ZMM=ZRANG*ZMAG
        IF (ZMM .GT. 100.0) GOTO 7
        ZMAG = ZMAG*2.0
        DELZ = DELZ*0.5
        GOTO 6
7       ZERO=-ZMIN*ZMAG
C
C  DRAW AXES FOR AMPLITUDE BOX
C
        IF(NOBS.LE.2) GO TO 100
        CALL P2K_ORIGIN(20.0,30.0,0)
        CALL P2K_MOVE(10.0,-15.0,0)
        FONTSIZE=10.0
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
        CALL P2K_STRING(TITLE,40)
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
        CALL P2K_MOVE(0.0,0.0,0.)
        CALL P2K_DRAW(0.0,FMAG,0.)
        CALL P2K_DRAW(ZMM,FMAG,0.)
        CALL P2K_DRAW(ZMM,0.0,0.)
        CALL P2K_DRAW(0.0,0.0,0.)
        CALL P2K_MOVE(ZERO,0.0,0.)
        CALL P2K_DRAW(ZERO,FMAG,0.)
        POSN=ZRANG*ZMAG-22.
        CALL P2K_MOVE(POSN,90.0,0)
C        ENCODE(20,103,LINE) IHIN,IKIN
C103     FORMAT('(',I2,',',I2,')')
        write(cline,'(''('',I2,'','',I2,'')'')')IHIN,IKIN
        call shorten(cline,k)
        CALL P2K_STRING(cline,k,0)
        IZ=ZRANG/DELZ
C
        DO 25 J=1,100
          ZPOS=-0.5+J*DELZ
          IF((ZPOS.LT.ZMIN).OR.(ZPOS.GE.ZMAX))GO TO 25
          XPOS=ZERO+ZPOS*ZMAG
          CALL P2K_MOVE(XPOS,0.0,0.)
          CALL P2K_DRAW(XPOS,2.0,0.)
          XPOS=XPOS-7.0
          CALL P2K_MOVE(XPOS,-7.5,0)
C          ENCODE(6,26,LINE) ZPOS
C          CALL P2K_STRING(LINE,6)
          write(cline,'(F6.3)')ZPOS
          call shorten(cline,k)
          CALL P2K_STRING(cline,k,0)
25      CONTINUE
26      FORMAT(F6.3)
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
        POSN=ZRANG*ZMAG-45.
        CALL P2K_MOVE(POSN,90.0,0)
c       CALL P2K_STRING(%REF('LATTICE LINE'),12)
        CALL P2K_STRING('LATTICE LINE',12)
        POSN=ZRANG*ZMAG+2.
        CALL P2K_MOVE(POSN,-4.5,0)
c       CALL P2K_STRING(%REF('RECIPROCAL'),10)
        CALL P2K_STRING('RECIPROCAL',10)
        CALL P2K_MOVE(POSN,-7.5,0)
c       CALL P2K_STRING(%REF('ANGSTROMS'),9)
        CALL P2K_STRING('ANGSTROMS',9)
        CALL P2K_ORIGIN(ZERO,0.0,1)
        SCALE=FMAG/(1.05*FMAX)
        IA=ALOG10(1.05*FMAX)
        B=10.0**IA
        IC=FMAX*1.05/B
        DO 200 J=1,IC
          F=J*B
          YPOS=F*SCALE
          ZA=ZMIN*ZMAG
          ZB=ZMAX*ZMAG
          CALL P2K_MOVE(ZA,YPOS,0.)
          ZD=ZA+2.0
          CALL P2K_DRAW(ZD,YPOS,0.)
          ZD=ZB-2.0
          CALL P2K_MOVE(ZB,YPOS,0.)
          CALL P2K_DRAW(ZD,YPOS,0.)
          XPOS=ZB
          CALL P2K_MOVE(XPOS,YPOS,0)
C          ENCODE(7,201,LINE) F
C          CALL P2K_STRING(LINE,7)
          write(cline,'(F7.1)')F
          call shorten(cline,k)
          CALL P2K_STRING(cline,k,0)
200     CONTINUE
        CALL P2K_MOVE(XPOS,0.,0)
c       CALL P2K_STRING(%REF('    0.0'),7)
        CALL P2K_STRING('    0.0',7)
201     FORMAT(F7.1)
C
C  PLOT OBSERVED AMPLITUDES FIRST
C
        FONTSIZE=8.0
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
        DO 50 J=1,NOBS
          IF(FOBS(J).EQ.-999.) GO TO 50
          XP=ZSTAR(J)*ZMAG
          YP=FOBS(J)*SCALE
          CALL P2K_MOVE(XP,YP,0)
          CALL P2K_CSTRING('X',1,0.)
50      CONTINUE
        FONTSIZE=1.25
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
C  PLOT FITTED AMPLITUDES
        DO 60 N=1,NOUT
        XP=ZOUT(N)*ZMAG
        YP=FOUT(N)*SCALE
        CALL P2K_MOVE(XP,YP,0)
        CALL P2K_CSTRING('O',1,0.)
60      CONTINUE
C
C  CALCULATE FITTED CURVES BY INTERPOLATION
        NPLT=(ZOUT(NOUT)-ZOUT(1))/DELPLT+1.1
        CALL INTERP(NOUT,NPLT,DELPLT,THICK,FOUT,PHIOUT,ZOUT,FINT,
     .  PHASE)
C  DRAW FITTED AMPL CURVE.
        Z=ZOUT(1)-DELPLT
        DO 600 J=1,NPLT
          Z=Z+DELPLT
          XP=Z*ZMAG
          YP=FINT(J)*SCALE
          IF(J.EQ.1) CALL P2K_MOVE(XP,YP)
          CALL P2K_DRAW(XP,YP,0.)
600     CONTINUE
C
C  DRAW AXES FOR PHASE BOX.
C
        PMAG2 = PMAG/360.
        YPOS=FMAG+GAP+PMAG/2.0
        CALL P2K_ORIGIN(0.0,YPOS,1)
        ZA=ZMIN*ZMAG
        ZB=ZMAX*ZMAG
        YAXIS=180.0*PMAG2
        CALL P2K_MOVE(ZA,-YAXIS,0.)
        CALL P2K_DRAW(ZA,+YAXIS,0.)
        CALL P2K_DRAW(ZB,+YAXIS,0.)
        CALL P2K_DRAW(ZB,-YAXIS,0.)
        CALL P2K_DRAW(ZA,-YAXIS,0.)
        CALL P2K_MOVE(0.0,-YAXIS,0.)
        CALL P2K_DRAW(0.0,+YAXIS,0.)
        DO 620 J=1,7
          YPOS=(PMAG/8)*J -YAXIS
          CALL P2K_MOVE(ZA,YPOS,0.)
          ZD=ZA+2.0
          CALL P2K_DRAW(ZD,YPOS,0.)
          ZD=ZB-2.0
          CALL P2K_MOVE(ZB,YPOS,0.)
          CALL P2K_DRAW(ZD,YPOS,0.)
620     CONTINUE
        DO 630 J=1,5
          IANG=-180+(J-1)*90
          XPOS=ZB+1.0
          YPOS=IANG*PMAG2
          CALL P2K_MOVE(XPOS,YPOS,0)
C          ENCODE(4,631,LINE) IANG
C          CALL P2K_STRING(LINE,4)
          write(cline,'(I4)')IANG
          call shorten(cline,k)
          CALL P2K_STRING(cline,k,0)
630     CONTINUE
631     FORMAT(I4)
C
C  PLOT OBS PHASE POINTS
C
        FONTSIZE=0.8
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
        DO 500 J=1,NOBS
          IF(PHIOBS(J).EQ.-999.) GO TO 500
          XP=ZSTAR(J)*ZMAG
          YP=PHIOBS(J)*PMAG2
          CALL P2K_MOVE(XP,YP,0)
          CALL P2K_CSTRING('X',1,0.)
500     CONTINUE
        FONTSIZE=1.25
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
C  PLOT FITTED PHASES
        DO 510 N=1,NOUT
        XP=ZOUT(N)*ZMAG
        YP=PHIOUT(N)*PMAG2
        CALL P2K_MOVE(XP,YP,0)
        CALL P2K_CSTRING('O',1,0.)
510     CONTINUE
C  DRAW  CALCULATED PHASE CURVE
C
        Z=ZOUT(1)-DELPLT
        DO 660 J=1,NPLT
          Z=Z+DELPLT
          P = PHASE(J)
          XP=Z*ZMAG
          YP=P*PMAG2
          IF(J.EQ.1) THEN
                CALL P2K_MOVE(XP,YP)
                GO TO 660
          ENDIF
          IF(ABS(PHASE(J)-PHASE(J-1)).GT.180.) THEN
                CALL P2K_MOVE(XP,YP,0.)
          ELSE
                CALL P2K_DRAW(XP,YP,0.)
          ENDIF
660     CONTINUE
C
C  GET READY FOR NEXT PLOTTED LINE.
C
99      continue
        FONTSIZE=1.66
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
        GOTO 9998
C
100     CALL P2K_MOVE(10.0,20.0,0)
C        ENCODE(80,104,LINE) IHIN,IKIN
C104     FORMAT('TOO FEW SPOTS ON LINE (',I2,',',I2,')')
C        CALL P2K_STRING(LINE,80)
        write(cline,'(''TOO FEW SPOTS ON LINE ('',I2,'','',I2,'')'')')IHIN,IKIN
        call shorten(cline,k)
        CALL P2K_STRING(cline,k,0)
C
9998    IF (LAST) GOTO 9999
        CALL P2K_PAGE
        RETURN
9999    continue
        CALL P2K_PAGE
        RETURN
C
        END
C
C   Interpolate calculated amps and phases for plotting curve
        SUBROUTINE INTERP(NL,NPLT,DELPLT,THICK,FOUT,PHIOUT,ZOUT,
     .  FINT,PHASE)
        DIMENSION FOUT(1),PHIOUT(1),ZOUT(1),FINT(1),PHASE(1)
        DIMENSION A(200),B(200)
        CDEG=57.2958
        CONV=0.0174532
        PI=3.14159
        DO 100 N=1,NL
        ANG=PHIOUT(N)*CONV
        A(N)=FOUT(N)*COS(ANG)
100     B(N)=FOUT(N)*SIN(ANG)
C
        Z=ZOUT(1)
        DO 110 N=1,NPLT
        ACALC=0.
        BCALC=0.
        DO 120 NLAT=1,NL
        ARG=(Z-ZOUT(NLAT))*THICK*PI     
        IF(ABS(ARG).GT.1.E-5) THEN
                G=SIN(ARG)/ARG
        ELSE
                G=1.
        ENDIF
        ACALC=ACALC+A(NLAT)*G
120     BCALC=BCALC+B(NLAT)*G
        FINT(N)=SQRT(ACALC*ACALC+BCALC*BCALC)
        PHASE(N)=0.
        IF(FINT(N).GE.1.E-5) PHASE(N)=ATAN2(BCALC,ACALC)*CDEG
        Z=Z+DELPLT
110     CONTINUE
        RETURN
        END
C*LINFIL.FOR*****************************************************************
C     LINEAR LEAST SQUARES FILTERING PACKAGE
C     Solves rectangular set of linear equations by least squares, using
C     filter level set by user to avoid noise amplification.
C     Given observational equations   Ax(j)=b(j)    j=1,NRHS
C     computes solutions     x(j)=VZ(1/lam)V'A'b(j)    for arbitrary
C     number NRHS of righthandside vectors b(j).
C     Where ' denotes transposed matrix, (1/lam) contains inverses of 
C     eigenvalues of normal matrix A'A, Z=diag(1,1,1,...0,0,0,) is
C     filter matrix which cuts out inverses of eigenvalues below filter
C     level.
C
C*****CALL LINFIL(A,B,C,IABDIM,NOBS,NPARM,NRHS,FILT,NFILT,ATA,ATB,VEC,VAL,
C      IPOINT,W)
C        A(I,J) is observational matrix, I=1,NOBS, J=1,NPARM
C        NOBS is number of observations
C        NPARM is number of parameters to be fitted
C        B(I,K) is matrix of RHS vectors  I=1,NOBS  K=1,NRHS containing
C            NRHS data vectors all to be fitted by single inversion of 
C            normal matrix  Contains residuals on return.
C        C(I,K) returns solutions
C        IABDIM is first dimension of arrays A B and C in calling program, 
C            which may be greater than NOBS
C        FILT is filter level.  Eigenvectors with eigenvalue
C            LAMBDA<FILT*LABDAMAX are not used in fitting.  Appropriate
C            setting of FILT depends on noise level in data but 1.E-2
C            may often be OK.
C        NFILT is returned as number of eigenvalues above filter level.
C        ATA(I,J) ATB(I,K) VEC(I,J) VAL(I) IPOINT(I)  I,J=1,NPARM K=1,NRHS
C            are arrays used for storing intermediate results and must be
C            dimensioned sufficiently large in calling program.
C        W(I)  is work area which must be dimensioned greater than
C            5*NPARM in calling program.
C
C        Uses subroutines EA06C for computing eigensystem and SHLSRT
C            for sorting eigenvalues into descending order.
C
C                 Version 1.01      22-Mar-82      RAC    FOR VAX
C
C************************************************************************
C
      SUBROUTINE LINFIL(A,B,C,IABDIM,NOBS,NPARM,NRHS,FILT,NFILT,ATA,
     1ATB,VEC,VAL,IPOINT,W)
      DIMENSION A(IABDIM,NPARM),B(IABDIM,NRHS),C(IABDIM,NRHS),
     1ATA(NPARM,NPARM),ATB(NPARM,NRHS),VEC(NPARM,NPARM),
     2VAL(NPARM),IPOINT(1),W(1)
C
C     Construct lower triangle of normal matrix A'A used by EA06C
      DO 100 J=1,NPARM
      DO 100 I=J,NPARM
      ATA(I,J)=0.
      DO 100 K=1,NOBS
100   ATA(I,J)=ATA(I,J)+A(K,I)*A(K,J)
C
C     Construct modified RHS  A'b(j)
      DO 110 J=1,NRHS
      DO 110 I=1,NPARM
      ATB(I,J)=0.
      DO 110 K=1,NOBS
110   ATB(I,J)=ATB(I,J)+A(K,I)*B(K,J)
C
C     Compute eigenvalues and eigenvectors of normal matrix
      CALL EA06C(ATA,VAL,VEC,NPARM,NPARM,NPARM,W)
C
C     Write out eigenvectors
C      WRITE(6,1000)
C1000  FORMAT(//'  Eigenvectors across page')
C      DO 900 I=1,NPARM
C900   WRITE(6,1010) (VEC(J,I),J=1,NPARM)
C1010  FORMAT(5X,10E12.4)
C
C     Sort eigenvalues into descending order using pointers
      CALL SHLSRT(VAL,NPARM,IPOINT,-1)
C
C     Count how many eigenvalues above filter level
      NFILT=1
      DO 120 N=2,NPARM
      IF(VAL(IPOINT(N)).LT.FILT*VAL(IPOINT(1))) GO TO 130
120   NFILT=NFILT+1
C
C     Solve using only NFILT eigenvecs corresponding to largest evals
C     First multiply by VEC', storing result in C
130   DO 140 NR=1,NRHS
      DO 140 N=1,NFILT
      I=IPOINT(N)
      C(N,NR)=0.
      DO 140 J=1,NPARM
140   C(N,NR)=C(N,NR)+VEC(J,I)*ATB(J,NR)
C
C     Now weight with NFILT inverse eigenvalues, storing result in ATB
      DO 150 N=1,NFILT
      WT=1./VAL(IPOINT(N))
      DO 150 NR=1,NRHS
150   ATB(N,NR)=C(N,NR)*WT
C
C     Dump eigensolutions
C      WRITE(6,1020)
C1020  FORMAT(//' Eigensolutions across page')
C      DO 910 NR=1,NRHS
C910   WRITE(6,1030) (ATB(N,NR),N=1,NFILT)
C1030  FORMAT(5X,10E12.4)
C
C     Now mult by VEC, returning final parameters in C
      DO 160 NR=1,NRHS
      DO 160 J=1,NPARM
      C(J,NR)=0.
      DO 160 N=1,NFILT
      I=IPOINT(N)
160   C(J,NR)=C(J,NR)+VEC(J,I)*ATB(N,NR)
C
C     Now calculate residuals in B
      DO 170 NR=1,NRHS
      DO 170 K=1,NOBS
      DO 170 J=1,NPARM
170   B(K,NR)=B(K,NR)-A(K,J)*C(J,NR)
C  
      RETURN
      END
      SUBROUTINE SHLSRT(KEY,N,IPOINT,INCDEC)
C
C     SHELL SORT
C     REFERENCES:  D.L. SHELL, CACM 2, 32 (JULY 1959)
C                  D.E. KNUTH, TAOCP III, SECT. 5.2.1
C
C     CALLING SEQUENCE:
C
C     KEY    IS AN ARRAY OF KEYS ON WHICH TO SORT
C     N      IS THE NUMBER OF ITEMS
C     IPOINT IS THE ARRAY OF POINTERS
C            (ONLY THE POINTERS WILL MOVE)
C     INCDEC .GE. 0 FOR SORTING INTO INCREASING ORDER;
C            .LT. 0 FOR SORTING INTO DECREASING ORDER
C
C
      REAL*4 KEY,K
      INTEGER H,S,T
      DIMENSION KEY(1),IPOINT(1)
C
C     NEXT STATEMENT JUST IN CASE N .EQ. 1
      IPOINT(1) = 1
C
C     CHECK N
C
      IF (N - 1) 11,11,1
C
C
C     INITIALIZE POINTER ARRAY
C
C     (NOTE THAT N MUST BE .GT. 1 IF WE GOT HERE)
    1 DO 2 I = 2,N
    2 IPOINT(I) = I
C
C     CHOICE OF SEQUENCE OF INCREMENTS SUGGESTED
C     BY KNUTH III, EQ. 8, P. 95.   HIS FORMULA
C     IS EQUIVALENT TO:
C
C            H(S) = (3**S - 1)/2
C            INITIAL VALUE OF S IS MINIMAL INTEGER
C              SUCH THAT H(S+2) .GE. N
C
C
C     SMAX = (ALOG(2N + 1)/ALOG(3)) - 2 + 1
      S = INT( (ALOG(FLOAT(2*N+1))/1.09861229) - 0.95 )
      S = MAX0(S,1)
C
      H = (3**S - 1)/2
C
C
      DO 7 T = 1,S
C
      JMIN = H + 1
      DO 6 J = JMIN,N
C
      I = J - H
      JJ = IPOINT(J)
      K = KEY(JJ)
      IPT = IPOINT(J)
C
    3 II = IPOINT(I)
      IF (K - KEY(II)) 4,4,5
C
    4 IPLUSH = I + H
      IPOINT(IPLUSH) = IPOINT(I)
      I = I - H
      IF (I) 5,5,3
C
    5 IPLUSH = I + H
      IPOINT(IPLUSH) = IPT
C
    6 CONTINUE
C
C     CHANGE INCREMENT
C
      IF (H - 1) 8,8,7
    7 H = (H-1)/3
C
C
C      CHECK INCDEC: IF NEGATIVE, SWITCH POINTER ARRAY
C
    8 IF (INCDEC) 9,11,11
C
    9 M = N/2
C     NP1MI = N + 1 - I
      NP1MI = N
      DO 10 I = 1,M
      NTEMP = IPOINT(I)
      IPOINT(I) = IPOINT(NP1MI)
      IPOINT(NP1MI) = NTEMP
   10 NP1MI = NP1MI - 1
C
   11 RETURN
      END
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

