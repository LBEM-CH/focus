      PROGRAM RMEASURE
C
      IMPLICIT NONE
C
      INTEGER NN1,I,ID,IP,IRTFLG,LUNARA(20),IMAX,NBIN
      PARAMETER (NN1=400,NBIN=100)
      INTEGER NSAM,K,ITEMP,NX,NXF(NN1/2),ISUM,J,JJ,J2
      INTEGER LL,MM,NN,L,M,N,NSAMH,JC,IRMIN,IRMAX,IRAN,IS
      INTEGER ICOM,ITOT,N1,N2,N3,ND
      REAL DATA1(NN1*NN1*NN1),PSIZE,F,F1,F2
      REAL PI,SUM1,DF,PD2(NN1/2),PD1(NN1/2)
      REAL PSTD,SUM2,PR(NN1/2),PRAVE,PRSTD,BINS(NBIN),DENS
      REAL XM(4),RMIN,RMAX,R,DATA2(NN1*NN1*NN1),GASDEV
      REAL DATA3(NN1*NN1*NN1),NCOR,SCOR,R2,D(NBIN),DD(NBIN)
      REAL PD3(NN1/2),PD(NN1/2),SAVE,SVAR,BAVE,BVAR,PAVE
      PARAMETER (PI=3.1415926535897,RMIN=0.04,RMAX=0.4)
      PARAMETER (DENS=0.735)
      DOUBLE PRECISION DMMM(100,4)
      COMPLEX SPEC1(NN1*NN1*NN1/2),SPEQ1(NN1*NN1)
      COMPLEX SPEC2(NN1*NN1*NN1/2),SPEQ2(NN1*NN1)
      COMPLEX SPEC3(NN1*NN1*NN1/2),SPEQ3(NN1*NN1)
      CHARACTER FNAME*80,CFORM,VX*15,ASYM*3,TITLE*1600
C
      EQUIVALENCE (DATA1,SPEC1)
      EQUIVALENCE (DATA2,SPEC2)
      EQUIVALENCE (DATA3,SPEC3)
C
      DATA  VX/'1.03 - 13.06.07'/
C     15 chars 'X.XX - XX.XX.XX' <--<--<--<--<--<--<--<--
C
      WRITE(*,1010)VX
1010  FORMAT('RMEASURE ',A15,/)
C
      WRITE(*,*)'Input 2D or 3D map?'
      READ(*,1000)FNAME
      WRITE(*,1000)FNAME
1000  FORMAT(A80)
      CFORM='S'
      ASYM='P1 '
      CALL IOPEN(FNAME,10,CFORM,N1,N2,N3,'OLD',IRTFLG,
     .           ASYM,PSIZE,VX,DMMM,LUNARA,NSAM,TITLE)
      IF  (IRTFLG .EQ. -1) THEN
        WRITE(*,1001) FNAME
1001    FORMAT(' File not found ',A80)
        STOP
      ENDIF
      IF ((N1.NE.N2).OR.(N1.NE.N3).OR.(N2.NE.N3)) THEN
        IF (N3.NE.1)
     .  STOP 'X,Y,Z dimensions not equal or Z dimension not 1'
      ENDIF
      IF (MOD(N1,2).NE.0) STOP 'Volume dimensions must be even'
      NSAM=N1
C
      WRITE(*,*)'Pixel size in A?'
      READ(*,*)PSIZE
      WRITE(*,*)PSIZE
C
      DO 10 K=1,N3
        IP=NSAM*(K-1)
        DO 10 I=1,NSAM
          ID=1+NSAM*((I-1)+NSAM*(K-1))
          CALL IREAD(10,CFORM,NSAM,DATA1(ID),I+IP,IRTFLG,
     +               LUNARA,NSAM)
          IF (IRTFLG.EQ.-1) GOTO 9999
10    CONTINUE
C
      CALL ICLOSE(10,CFORM,DMMM,TITLE)
C
      NSAMH=NSAM/2
      JC=NSAMH+1
      IRMIN=NSAM*RMIN
      IRMAX=NSAM*RMAX
C
      CALL D3MASK(NSAM,N3,DATA1,DATA3,SPEC3,SPEQ3,NBIN,BINS,
     +            D,DD,PSIZE)
C
      PAVE=0.0
      SAVE=0.0
      SVAR=0.0
      ICOM=0
      ND=1
      IF (N3.EQ.1) ND=0
      DO 210 I=1,NSAM
        DO 210 J=1,NSAM
          DO 210 K=1,N3
            ID=I+NSAM*((J-1)+NSAM*(K-1))
            IF (DATA3(ID).EQ.0.0) THEN
              ISUM=0
              SUM1=0.0
              SUM2=0.0
              DO 220 L=-1,1
                LL=I+L
                IF ((LL.GE.1).AND.(LL.LE.NSAM)) THEN
                DO 221 M=-1,1
                  MM=J+M
                  IF ((MM.GE.1).AND.(MM.LE.NSAM)) THEN
                  DO 222 N=-ND,ND
                    NN=K+N
                    IF ((NN.GE.1).AND.(NN.LE.NSAM)) THEN
                      IS=LL+NSAM*((MM-1)+NSAM*(NN-1))
                      IF (DATA3(IS).EQ.0.0) THEN
                        SUM1=SUM1+DATA1(IS)
                        SUM2=SUM2+DATA1(IS)**2
                        ISUM=ISUM+1
                      ENDIF
                    ENDIF
222               CONTINUE
                  ENDIF
221             CONTINUE
                ENDIF
220           CONTINUE
              IF (ISUM.NE.0) THEN
                SUM1=SUM1/ISUM
                SUM2=SUM2/ISUM
                SUM2=SUM2-SUM1**2
              ENDIF
              DATA2(ID)=SUM2
              PAVE=PAVE+SUM2
            ELSE
              DATA2(ID)=0.0
              SAVE=SAVE+DATA1(ID)
              SVAR=SVAR+DATA1(ID)**2
              ICOM=ICOM+1
            ENDIF
210   CONTINUE
      PAVE=PAVE/NSAM/NSAM/N3/2.0
      SAVE=SAVE/ICOM
      SVAR=SVAR/ICOM
C      SVAR=SVAR-SAVE**2
C
      IRAN=-100
      F=GASDEV(IRAN)
      ITOT=0
      BAVE=0.0
      BVAR=0.0
      DO 200 I=1,NSAM*NSAM*N3
        F=DATA2(I)
        IF (DATA3(I).NE.0.0) THEN
          DATA2(I)=GASDEV(IRAN)
        ELSE
          DATA2(I)=0.0
        ENDIF
        IF ((F.GT.PAVE).OR.(DATA3(I).NE.0.0)) THEN
          BAVE=BAVE+DATA1(I)
          BVAR=BVAR+DATA1(I)**2
          DATA3(I)=GASDEV(IRAN)
          ITOT=ITOT+1
        ELSE
          DATA3(I)=0.0
        ENDIF
200   CONTINUE
      BAVE=BAVE/ITOT
      BVAR=BVAR/ITOT
      SVAR=SVAR-BVAR
      BVAR=BVAR-BAVE**2
      SAVE=SAVE-BAVE
C
      PRINT *
      IF (N3.EQ.1) THEN
      PRINT *,'Structure, background area [A**2]    = ',
     +        ICOM*PSIZE**2,ITOT*PSIZE**2
      PRINT *,'Area ratio structure / background    = ',
     +        REAL(ICOM)/ITOT
      ELSE
      PRINT *,'Structure, background volume [A**3]    = ',
     +        ICOM*PSIZE**3,ITOT*PSIZE**3
      PRINT *,'Volume ratio structure / background    = ',
     +        REAL(ICOM)/ITOT
      PRINT *,'Mass of structure [Da] (0.735 Da/A**3) = ',
     +        DENS*ICOM*PSIZE**3
      ENDIF
      PRINT *,'Average density, STD of structure'
      PRINT *,'above background                       = ',
     +        SAVE,SQRT(SVAR)
      PRINT *,'Average density, STD of background     = ',
     +        BAVE,SQRT(BVAR)
      PRINT *,'Overall signal-to-noise ratio          = ',
     +        REAL(ICOM)*(SVAR+SAVE**2)/BVAR/ITOT
      PRINT *
      IF (REAL(ICOM)/ITOT.GT.0.66) WRITE (*,1008)
1008    FORMAT(/,' ***** WARNING *****',/,
     +        ' Structure appears to be masked very tightly.',/,
     +        ' This may influence the resolution measurement.',//,
     +        ' If possible, repeat measurement on a more',
     +        ' generously masked',/,' reconstruction.',/)
C
C     DATA1: Original structure
C     DATA2: Molecular envelope filled with noise
C     DATA3: Original mask of structure filled with noise
C
      CALL RLFT3(DATA1,SPEQ1,NSAM,NSAM,N3,1)
C
      PAVE=0.0
      PSTD=0.0
      DO 40 J=1,NSAM/2-1
        J2=J**2
        JJ=(J+1)**2
        NX=0
        SUM1=0.0
        DO 112 L=1,JC-1
          LL=L-1
          DO 112 M=1,NSAM
            MM=M-1
            IF (MM.GE.JC) MM=MM-NSAM
            DO 112 N=1,N3
              NN=N-1
              IF (NN.GE.JC) NN=NN-NSAM
              ITEMP=LL**2+MM**2+NN**2
              IF ((ITEMP.GE.J2).AND.(ITEMP.LT.JJ)) THEN
                NX=NX+1
                ID=L+NSAMH*((M-1)+NSAM*(N-1))
                SUM1=SUM1+CABS(SPEC1(ID))**2
              ENDIF
112     CONTINUE
        SUM1=SQRT(SUM1/NX)
        IF (J.NE.1) THEN
          PR(J)=SUM2/SUM1
          PAVE=PAVE+PR(J)
          PSTD=PSTD+PR(J)**2
        ENDIF
        SUM2=SUM1
        DO 111 L=1,JC-1
          LL=L-1
          DO 111 M=1,NSAM
            MM=M-1
            IF (MM.GE.JC) MM=MM-NSAM
            DO 111 N=1,N3
              NN=N-1
              IF (NN.GE.JC) NN=NN-NSAM
              ITEMP=LL**2+MM**2+NN**2
              IF ((ITEMP.GE.J2).AND.(ITEMP.LT.JJ)) THEN
                ID=L+NSAMH*((M-1)+NSAM*(N-1))
              ENDIF
111     CONTINUE
40    CONTINUE
      PRAVE=PAVE/(NSAM/2-2)
      PRSTD=SQRT(PSTD/(NSAM/2-2)-PRAVE**2)
C
      DO 60 J=INT(PSIZE*NSAM/40.0),NSAM/2-1
        IF ((J.GE.2).AND.(J.LE.NSAM/2-1)) THEN
          IF (ABS(PR(J)-PRAVE).GT.3.0*PRSTD) THEN
            WRITE(*,1005) REAL(NSAM)/J*PSIZE
1005        FORMAT(/,' ***** WARNING *****',/,
     +        ' Volume appears to be filtered with a sharp',
     +        ' cut-off at ',F10.4,' A.',/,
     +        ' This may influence the resolution measurement.',//,
     +        ' If possible, repeat measurement on an unfiltered',
     +        ' reconstruction.',/)
            GOTO 81
          ENDIF
        ENDIF
60    CONTINUE
C
81    CONTINUE
C
      CALL CORN(NSAM,N3,SPEC1,PD,NXF,0.0,0.0,0.0,NSAM/2-2)
      CALL SMOOTH(NSAM/2-2,PD,PD1,1)
C
C     Assemble modeled volume
C     DATA1: Original structure
C     DATA2: Molecular envelope filled with noise
C     DATA3: Original mask of structure filled with noise
C
      CALL RLFT3(DATA2,SPEQ2,NSAM,NSAM,NSAM,1)
      CALL CORN(NSAM,N3,SPEC2,PD2,NXF,0.0,0.0,0.0,NSAM/2-2)
      SCOR=0
      ISUM=0
      DO 230 I=2,NSAM/2-1
        SCOR=SCOR+NXF(I)*PD2(I)
        ISUM=ISUM+NXF(I)
230   CONTINUE
      SCOR=SCOR/ISUM
      PRINT *
      PRINT *,'Signal correlation = ',SCOR
C
      CALL RLFT3(DATA3,SPEQ3,NSAM,NSAM,NSAM,1)
      CALL CORN(NSAM,N3,SPEC3,PD3,NXF,0.0,0.0,0.0,NSAM/2-2)
      NCOR=0
      ISUM=0
      DO 240 I=2,NSAM/2-1
        NCOR=NCOR+NXF(I)*PD3(I)
        ISUM=ISUM+NXF(I)
240   CONTINUE
      NCOR=NCOR/ISUM
      PRINT *,'Noise correlation = ',NCOR
      IF ((SCOR-NCOR)/(NCOR+SCOR).LT.0.1) WRITE (*,1009)
1009    FORMAT(/,' ***** WARNING *****',/,
     +        ' Predicted signal and noise correlation are very',
     +        ' similar.',/,
     +        ' This may be due to a tightly masked volume.',/,
     +        ' This may influence the resolution measurement.',//,
     +        ' If possible, repeat measurement on a more',
     +        ' generously masked',/,' reconstruction.',/)
C
      PRINT *
      WRITE(*,1006)
1006  FORMAT('    Resoln   Correln   Pred. FSC      Nvox',/,
     +       '  ========================================')
      SUM1=0.0
      R=REAL(NSAM)
      R2=REAL(NSAM)
      DO 30 I=2,NSAM/2-1
        F=(PD1(I)-NCOR)/(2.0*SCOR-NCOR-PD1(I))
        IF ((F.LT.0.5).AND.(R.EQ.REAL(NSAM)).AND.
     +  (REAL(NSAM)/I*PSIZE.LT.30.0).AND.(I.LE.NSAM/2-3)) THEN
          F1=(PD1(I+1)-NCOR)/(2.0*SCOR-NCOR-PD1(I+1))
          F2=(PD1(I+2)-NCOR)/(2.0*SCOR-NCOR-PD1(I+2))
          IF ((F1.LT.0.5).AND.(F2.LT.0.5)) R=REAL(NSAM)/(I-1)
        ENDIF
        IF ((F.LT.0.143).AND.(R2.EQ.REAL(NSAM)).AND.
     +  (REAL(NSAM)/I*PSIZE.LT.30.0).AND.(I.LE.NSAM/2-3)) THEN
          F1=(PD1(I+1)-NCOR)/(2.0*SCOR-NCOR-PD1(I+1))
          F2=(PD1(I+2)-NCOR)/(2.0*SCOR-NCOR-PD1(I+2))
          IF ((F1.LT.0.143).AND.(F2.LT.0.143)) R2=REAL(NSAM)/(I-1)
        ENDIF
        WRITE(*,1003)REAL(NSAM)/I*PSIZE,PD1(I),
     +  F,NXF(I)
1003    FORMAT(2F10.4,F12.4,I10)
30    CONTINUE
C
      IF (R.NE.REAL(NSAM)) THEN
        PRINT *
        WRITE(*,1007)PSIZE*R
1007    FORMAT(' Resolution at FSC = 0.5:   ',F11.5)
        WRITE(*,1011)PSIZE*R2
1011    FORMAT(' Resolution at FSC = 0.143: ',F11.5,/,
     +         ' ======================================')
      ENDIF
C
      PRINT *
      STOP ' NORMAL TERMINATION OF RMEASURE'
9999  STOP ' Error opening/reading/writing file'
C
      END
C
C**************************************************************************
      SUBROUTINE HISTO(NSAM,N3,NBIN,A3DV,BINS,MIN,MAX)
C**************************************************************************
      IMPLICIT NONE
C
      INTEGER I,J,NSAM,NBIN,N3
      REAL A3DV(*),MIN,MAX,BINS(*)
C
      MIN=1.0E30
      MAX=-1.0E30
      DO 10 I=1,NSAM*NSAM*N3
        IF (A3DV(I).GT.MAX) MAX=A3DV(I)
        IF (A3DV(I).LT.MIN) MIN=A3DV(I)
10    CONTINUE
C
      DO 20 I=1,NBIN
        BINS(I)=0.0
20    CONTINUE
C
      DO 30 I=1,NSAM*NSAM*N3
        J=(A3DV(I)-MIN)/(MAX-MIN)*(NBIN-1)+1
        BINS(J)=BINS(J)+1.0
30    CONTINUE
C
      RETURN
      END
C**************************************************************************
      SUBROUTINE DERIV(NBIN,BINS,D,DD)
C**************************************************************************
      IMPLICIT NONE
C
      INTEGER NBIN,I
      REAL D(*),DD(*),BINS(*)
C
      DO 10 I=2,NBIN
        D(I)=BINS(I)-BINS(I-1)
10    CONTINUE
      D(1)=D(2)
C
      DO 20 I=2,NBIN
        DD(I)=D(I)-D(I-1)
20    CONTINUE
      DD(1)=DD(2)
C
      RETURN
      END
C**************************************************************************
      REAL FUNCTION RAVE(NBIN,A,IW,N)
C**************************************************************************
      IMPLICIT NONE
C
      INTEGER NBIN,IW,I,N,J
      REAL AVE,A(*)
C
      J=N-IW
      IF (J.LE.0) J=1
      IF (J+2*IW+1.GT.NBIN) J=NBIN-2*IW-1
C
      AVE=0.0
      DO 10 I=J,J+2*IW+1
        AVE=AVE+A(I)
10    CONTINUE
C
      RAVE=AVE/(2*IW+1)
      RETURN
      END
C**************************************************************************
      SUBROUTINE D3MASK(NSAM,N3,C3DV,B3DV,B3DF,B3DS,NBIN,BINS,
     +                  D,DD,PSIZE)
C**************************************************************************
C When XSTD is positive (see CALL), produces 5-pixel-cosine-bell smoothed 
C mask boundary defined by density being above the mean level of a low pass 
C filtered map by an amount equal to XSTD times the standard deviation of 
C the map.

C Calls rlft3.  
C Used by MASK.
C**************************************************************************
      IMPLICIT NONE
C
      INTEGER NSAM,L,LL,M,MM,N,NN,ID,IS,I,J,K,NSAMH,IRAD,JC,NSAM3
      INTEGER NM,NBIN,IW,HMAX,IM,I1,I2,NNEG,N3
      REAL RAD2,FRAD,B3DV(*),TLEVEL,EDGE,PI,C3DV(*),D(*),DD(*)
      REAL PSIZE,RESF,RAVE,BINS(*),MIN,MAX,DDMAX
      PARAMETER (IW=3)
      PARAMETER (RESF=20.0)
C      PARAMETER (FRAD=0.1)
      PARAMETER (IRAD=5)
      PARAMETER (PI=3.1415926535897)
      DOUBLE PRECISION DMEAN,DSTD
      COMPLEX B3DF(*),B3DS(*)
C**************************************************************************
      JC=NSAM/2+1
      NSAMH=NSAM/2
      NSAM3=NSAM*NSAM*N3
C
      DO 10 I=1,NSAM3
      	B3DV(I)=C3DV(I)
10    CONTINUE
      CALL RLFT3(B3DV,B3DS,NSAM,NSAM,N3,1)
C
C     Low-pass filter 3D map....
      FRAD=PSIZE/RESF
      DO 88 L=1,JC
        LL=L-1
        DO 88 M=1,NSAM
          MM=M-1
          IF (MM.GE.JC) MM=MM-NSAM
          DO 88 N=1,N3
            NN=N-1
            IF (NN.GE.JC) NN=NN-NSAM
            RAD2=REAL(LL**2+MM**2+NN**2)/NSAM**2/FRAD**2
            IF (L.NE.JC) THEN
              ID=L+NSAMH*((M-1)+NSAM*(N-1))
              B3DF(ID)=B3DF(ID)*EXP(-RAD2)
            ELSE
              IS=M+NSAM*(N-1)
              B3DS(IS)=B3DS(IS)*EXP(-RAD2)
            ENDIF
88    CONTINUE
C
      CALL RLFT3(B3DV,B3DS,NSAM,NSAM,N3,-1)
C
      CALL HISTO(NSAM,N3,NBIN,B3DV,BINS,MIN,MAX)
      CALL DERIV(NBIN,BINS,D,DD)
C
      HMAX=0
      DO 100 I=1,NBIN
C        print *,i,REAL(I-1)/(NBIN-1)*(MAX-MIN)+MIN,bins(i),
C     +          RAVE(NBIN,D,IW,I),RAVE(NBIN,DD,IW,I)
        IF (BINS(I).GT.HMAX) THEN
          HMAX=BINS(I)
          IM=I
        ENDIF
100   CONTINUE
C
      DDMAX=0
      DO 110 I=IM+IW,NBIN
        IF (RAVE(NBIN,DD,IW,I).GT.DDMAX) THEN
          DDMAX=RAVE(NBIN,DD,IW,I)
          I1=I
        ENDIF
110   CONTINUE
C
130   CONTINUE
      DO 140 I2=I1+IW,NBIN-6
        NNEG=0
        DO 150 I=I2,I2+6
          IF (RAVE(NBIN,DD,IW,I).LT.0.0) NNEG=NNEG+1
150     CONTINUE
        IF (NNEG.GE.5) GOTO 160
140   CONTINUE
C
160   CONTINUE
C      print *,i1,i2
      I1=(I1+I2)/2.0
      TLEVEL=REAL(I1-1)/(NBIN-1)*(MAX-MIN)+MIN
      DO 86 I=1,NSAM3
        IF (B3DV(I).GE.TLEVEL) THEN
          B3DV(I)=1.0
        ELSE
          B3DV(I)=0.0
        ENDIF
86    CONTINUE
C
      RETURN
      END
C**************************************************************************
      FUNCTION gasdev(idum)
C**************************************************************************
      INTEGER idum
      REAL gasdev
CU    USES ran1
      INTEGER iset
      REAL fac,gset,rsq,v1,v2,ran1
      SAVE iset,gset
      DATA iset/0/
      if (iset.eq.0) then
1       v1=2.*ran1(idum)-1.
        v2=2.*ran1(idum)-1.
        rsq=v1**2+v2**2
        if(rsq.ge.1..or.rsq.eq.0.)goto 1
        fac=sqrt(-2.*log(rsq)/rsq)
        gset=v1*fac
        gasdev=v2*fac
        iset=1
      else
        gasdev=gset
        iset=0
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software -2#,-".
C**************************************************************************
      FUNCTION ran1(idum)
C**************************************************************************
      INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV
      REAL ran1,AM,EPS,RNMX
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,
     *NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER j,k,iv(NTAB),iy
      SAVE iv,iy
      DATA iv /NTAB*0/, iy /0/
      if (idum.le.0.or.iy.eq.0) then
        idum=max(-idum,1)
        do 11 j=NTAB+8,1,-1
          k=idum/IQ
          idum=IA*(idum-k*IQ)-IR*k
          if (idum.lt.0) idum=idum+IM
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k
      if (idum.lt.0) idum=idum+IM
      j=1+iy/NDIV
      iy=iv(j)
      iv(j)=idum
      ran1=min(AM*iy,RNMX)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software -2#,-".
C**************************************************************************
      SUBROUTINE SMOOTH(KM,PD1,PD2,NW)
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER KM,NW,I,J,N,K
      REAL PD1(*),PD2(*)
C
      DO 10 I=1,KM+1
        PD2(I)=0.0
        N=0
        DO 20 J=-NW,NW
          K=I+J
          IF ((K.GE.1).AND.(K.LE.KM+1)) THEN
            PD2(I)=PD2(I)+PD1(K)
            N=N+1
          ENDIF
20      CONTINUE
        PD2(I)=PD2(I)/N
10    CONTINUE
C
      RETURN
      END
C
C**************************************************************************
      SUBROUTINE CORN(NSAM,N3,SPEC1,PD,NXF,SHX,SHY,SHZ,KM)
C**************************************************************************
C
      IMPLICIT NONE
C
      INTEGER NSAM,I1,K,K2,KK,ISUM,L,M,N,LL,MM,NN,ID2
      INTEGER NXF(*),ITEMP,ID,ID1,NSAMH,I2,I3,LI,MI,NI,KM
      INTEGER ND,N3,NH3
      REAL SUM1,SUM2,SUM3,PD(*),SHX,SHY,SHZ,PHASE,PSHFTR
      REAL SS1
      COMPLEX SPEC1(*),S1,S2,PSHFT1,PSHFT2
C
      NSAMH=NSAM/2
      NH3=N3/2
      ND=1
      IF (N3.EQ.1) ND=0
C
      DO 50 K=1,KM+1
        NXF(K)=0
        SUM1=0.0
        SUM2=0.0
        SUM3=0.0
        K2=(K-1)**2
        KK=K**2
        DO 111 L=0,NSAMH-1
          DO 111 M=-NSAMH,NSAMH-1
            DO 111 N=-NH3,NH3-1
              ITEMP=L**2+M**2+N**2
              IF ((ITEMP.GT.K2).AND.(ITEMP.LE.KK)) THEN
                ISUM=(L+M+N)
                PSHFTR=1.0
                IF (MOD(ISUM,2).NE.0) PSHFTR=-1.0
                PHASE=-SHX*L-SHY*M-SHZ*N
                PSHFT1=CMPLX(COS(PHASE),SIN(PHASE))*PSHFTR
                ID1=ID(NSAM,L,M,N)
                S1=SPEC1(ID1)*PSHFT1
                SS1=CABS(S1)**2
                DO 40 I1=-1,1
                  LI=L+I1
                  IF ((LI.LT.NSAMH).AND.(LI.GE.0)) THEN
                  DO 41 I2=-1,1
                    MI=M+I2
                    IF ((MI.LT.NSAMH-1).AND.(MI.GE.-NSAMH)) THEN
                    DO 42 I3=-ND,ND
                      NI=N+I3
                      IF ((NI.LT.NSAMH-1).AND.(NI.GE.-NSAMH)) THEN
                        ITEMP=I1**2+I2**2+I3**2
C                        IF ((I1.NE.0).OR.(I2.NE.0).OR.(I3.NE.0)) THEN
C                        IF (ABS(I1)+ABS(I2)+ABS(I3).EQ.2) THEN
                        IF (ITEMP.EQ.1) THEN
                          NXF(K)=NXF(K)+1
                          ISUM=(LI+MI+NI)
                          PSHFTR=1.0
                          IF (MOD(ISUM,2).NE.0) PSHFTR=-1.0
                          PHASE=-SHX*LI-SHY*MI-SHZ*NI
                          PSHFT2=CMPLX(COS(PHASE),SIN(PHASE))*PSHFTR
                          ID2=ID(NSAM,LI,MI,NI)
                          S2=SPEC1(ID2)*PSHFT2
                          SUM1=SUM1+SS1
                          SUM2=SUM2+CABS(S2)**2
                          SUM3=SUM3+REAL(S1*CONJG(S2))
                        ENDIF
                      ENDIF
42                  CONTINUE
                    ENDIF
41                CONTINUE
                  ENDIF
40              CONTINUE
              ENDIF
111     CONTINUE
        PD(K)=SUM3/SQRT(SUM1*SUM2)
50    CONTINUE
C
      RETURN
      END
C
C**************************************************************************
      INTEGER FUNCTION ID(NSAM,L,M,N)
      LL=L+1
      MM=M+1
      IF (MM.LT.1) MM=MM+NSAM
      NN=N+1
      IF (NN.LT.1) NN=NN+NSAM
      ID=LL+NSAM/2*((MM-1)+NSAM*(NN-1))
      RETURN
      END
C**************************************************************************
      INTEGER FUNCTION IS(NSAM,M,N)
      MM=M+1
      IF (MM.LT.1) MM=MM+NSAM
      NN=N+1
      IF (NN.LT.1) NN=NN+NSAM
      IS=MM+NSAM*(NN-1)
      RETURN
      END
C**************************************************************************
      SUBROUTINE IOPEN(CNAME,IFILE,CFORM,N1,N2,N3,CSTIN,IRTFLG,ASYM,
     .			PSIZE,VX,DMMM,LUNARA,NSAM,TITLE)
C**************************************************************************
C  Opens a file using formats defined by Imagic, MRC or Spider systems
C  Calls IREAD.
C  Used in CARD8and9, CARD13AND14, MAIN and OPMAPS.
C**************************************************************************
      IMPLICIT NONE

      INTEGER IFILE,N1,N2,N3,IRTFLG,MODE,LENBYT,LABREC,LABBYT
      INTEGER LABBY8,IFORM,LABRECT,I,LUNARA(*),IREC,NHISTREC
      INTEGER NXYZ(3),NXYZST(3),MXYZ(3),NSAM,KSPG,KBS
      REAL DMIN,DMAX,DMEAN,HEAD(256),ZBUF(45),PSIZE,CELL(6),CELLIN(6)
      DOUBLE PRECISION DMMM(10,4)
      CHARACTER*3 ASYM
      CHARACTER*15 VX
      CHARACTER CNAME*80,CFORM,CSTAT*20,TITLE*1600,CLINE*180
      CHARACTER CDAT*24,CSTIN*20
      LOGICAL EX
      DATA CELL/6*90.0/, KSPG/1/, KBS/80/
      EQUIVALENCE (CLINE,ZBUF)
C**************************************************************************
CTSH++
      TITLE='SNR      VX.XX - XX.XX.XX                  '
CTSH--
      IF (CSTIN(1:1).EQ.'O') CSTAT='OLD'
      IF (CSTIN(1:1).EQ.'N') CSTAT='NEW'
      IF (CSTIN(1:1).EQ.'U') CSTAT='UNKNOWN'
      IF (CSTIN(1:1).EQ.'M') CSTAT='MATCH'
      IF (CSTAT.EQ.'NEW'.OR.CSTAT.EQ.'MATCH') THEN
        INQUIRE(FILE=CNAME,ERR=9999,EXIST=EX)
      	IF (EX) THEN
      	  OPEN(UNIT=IFILE,FILE=CNAME,STATUS='OLD')
      	  CLOSE(UNIT=IFILE,STATUS='DELETE',IOSTAT=IRTFLG)
      	  IF (IRTFLG.NE.0) STOP ' ERROR DELETING FILE'
      	ENDIF
      	DMMM(IFILE,1)=9.9D20
      	DMMM(IFILE,2)=-9.9D20
      	DMMM(IFILE,3)=0.0D0
      	DMMM(IFILE,4)=0.0D0
      ENDIF
      IF (CFORM.EQ.'I') THEN
      	STOP ' IMAGIC FORMAT NOT YET IMPLEMENTED .... SORRY!'
      ELSEIF (CFORM.EQ.'M') THEN
98      CONTINUE
      	WRITE(*,*) ' OPENING MRC FORMAT FILE....'
      	IRTFLG=0
      	IF (CSTAT.EQ.'UNKNOWN') THEN
C		this section used only for FWEIGHT file
        	INQUIRE(FILE=CNAME,ERR=9999,EXIST=EX)
      		IF (EX) THEN
      			CSTAT='OLD'
      			IRTFLG=0
      		ELSE
      			WRITE(*,*) ' File not found'
      			IRTFLG=-1
C      			CSTAT='NEW'
      			RETURN
      		ENDIF
      	ENDIF
C     	CALL IMOPEN(IFILE,CNAME,CSTAT)
      		DO 5 I=1,3
5		CELL(I)=PSIZE*NSAM
      		IF(CSTAT.EQ.'NEW') THEN
      			KSPG=1
      			KBS=80
      		ENDIF
      		IF(CSTAT.EQ.'MATCH') THEN
      			KSPG=0
      			KBS=0
                        CSTAT='NEW'
      		ENDIF
        CALL IMOPEN(IFILE,CNAME,CSTAT)
      	IF (CSTAT.EQ.'OLD') THEN
      	  CALL IRDHDR(IFILE,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      	  CALL IRTCEL(IFILE,CELLIN)
      	  N1=NXYZ(1)
      	  N2=NXYZ(2)
      	  N3=NXYZ(3)
 	  CELL(1)=PSIZE*N1
 	  CELL(2)=PSIZE*N2
 	  CELL(3)=PSIZE*N3
C  write out apparent cell dimensions for a 3D input map
      	  IF(N1.EQ.N3) THEN
CTSH      		WRITE(*,6) ((CELLIN(I),CELL(I)),I=1,3)
CTSH++
                WRITE(*,6) CELLIN(1), CELL(1), CELLIN(2),
     .                     CELL(2),CELLIN(3), CELL(3)
CTSH--
6		FORMAT('  cell dimensions from'/'   input 3D map   PSIZE*NX'/
     .			'    a',2F10.3/'    b',2F10.3/'    c',2F10.3)
C      		IF(ABS(CELLIN(1)-CELL(1))/CELL(1).GT.0.01) WRITE(*,7)
7		FORMAT(' ********WARNING******** requested PSIZE differs',
     .			' from that on input 3D map'/)
      	  ENDIF
          DMMM(IFILE,1)=DMIN
          DMMM(IFILE,2)=DMAX
          DMMM(IFILE,3)=DMEAN*N1*N2*N3
          DMMM(IFILE,4)=N1*N2*N3
      	ELSE
      	  MODE=2
      	  NXYZ(1)=N1
      	  NXYZ(2)=N2
      	  NXYZ(3)=N3
      	  MXYZ(1)=N1
      	  MXYZ(2)=N2
      	  MXYZ(3)=N3
      	  NXYZST(1)=-0.5*N1
      	  NXYZST(2)=-0.5*N2
      	  NXYZST(3)=-0.5*N3
      	  DMIN=0.0
      	  DMAX=0.0
      	  DMEAN=0.0
      	  CALL FDATE(CDAT)
      	  TITLE(11:25)=VX
      	  TITLE(44:46)=ASYM
      	  TITLE(47:48)='  '
      	  TITLE(49:72)=CDAT
      	  CALL ICRHDR(IFILE,NXYZ,MXYZ,MODE,TITLE,1)
      	  CALL IALSYM(IFILE,KSPG,KBS)
      	  CALL IALSIZ(IFILE,NXYZ,NXYZST)
      	  CALL IALCEL(IFILE,CELL)
      	  CALL IWRHDR(IFILE,TITLE,0,DMIN,DMAX,DMEAN)
      	ENDIF
      ELSEIF (CFORM.EQ.'S') THEN
      	IRTFLG=0
      	IF (CSTAT.EQ.'UNKNOWN') THEN
C		this section used only for FWEIGHT file
        	INQUIRE(FILE=CNAME,ERR=9999,EXIST=EX)
      		IF (EX) THEN
      			CSTAT='OLD'
      			IRTFLG=0
            ELSE
      			WRITE(*,*) ' File not found'
                IRTFLG=-1
C                CSTAT='NEW'
      		RETURN
      		ENDIF
      	ENDIF
      	IF (CSTAT.EQ.'OLD') THEN
      	  LABBY8=1024
      	ELSE
      	  LENBYT=4*N1
      	  LABREC=1024/LENBYT
          IF (MOD(1024,LENBYT).NE.0) LABREC=LABREC+1
      	  LABBYT=LABREC*LENBYT
          LABBY8=1024
C         RECL IS IN UNITS OF BYTES ON SGI, FLOATS ON OSF

C         ONE LINE OF OSF UNIX SPECIFIC CODE FOLLOWS
CC          LABBY8=LABBYT/4
c      print *,LENBYT,LABREC,LABBYT,LABBY8
      	ENDIF

C      print *,1,N1,CNAME,CSTAT,LABBY8
        OPEN(IFILE,FILE=CNAME,STATUS=CSTAT,FORM='UNFORMATTED',
     &  ACCESS='DIRECT',ERR=9999,RECL=LABBY8)

      	IF (CSTAT.EQ.'OLD') THEN
      	  LUNARA(IFILE)=1
      	  CALL IREAD(IFILE,CFORM,256,HEAD,0,IRTFLG,LUNARA,NSAM)
	  IF  (IRTFLG .EQ. -1)  GOTO 9998
      	  N1=HEAD(12)
      	  N2=HEAD(2)
      	  N3=ABS(HEAD(1))+0.5
          IF(N1*N2*N3.EQ.0) THEN
            CFORM='M'
            CLOSE(IFILE)
            GOTO 98
          ENDIF
      	  WRITE(*,*) ' OPENING SPIDER FORMAT FILE....'
      	  IFORM=HEAD(5)
      	  IF (HEAD(5).LT.0.0) IFORM=HEAD(5)-0.5
      	  IF (IABS(IFORM).EQ.1) N3=1
      	  LENBYT=HEAD(23)
      	  IF (LENBYT.LE.0) LENBYT=4*N1
      	  LABREC=HEAD(13)
      	  LABRECT=1024/LENBYT
      	  IF (MOD(1024,LENBYT).NE.0) LABRECT=LABRECT+1
          IF (LABRECT.LE.0.OR.(IFORM.LT.7.AND.
     &        LABREC.NE.LABRECT)) THEN
C             UNREASONABLE LABREC NUMBER SO DEFAULT IT
              LABREC=LABRECT
          ENDIF
          LABBYT=HEAD(22)
          LABBY8=LENBYT
C         ALPHA OSF/1 UNIX SPECIFIC STATEMENT FOLLOWS
CC          LABBY8=LENBYT/4
      	  CLOSE(IFILE)
C      print *,2,N1,CNAME,CSTAT,LABBY8
          OPEN(IFILE,FILE=CNAME,STATUS='OLD',ACCESS='DIRECT',
     &         FORM='UNFORMATTED',RECL=LABBY8,ERR=9999)
      	  LUNARA(IFILE)=LABREC
      	ELSE
      	  WRITE(*,*) ' OPENING SPIDER FORMAT FILE....'
      	  DO 10 I=1,256
      	    HEAD(I)=0.0
10	  CONTINUE
      	  IFORM=1
      	  IF (N3.GT.1) IFORM=3
      	  HEAD(1)=N3
      	  HEAD(2)=N2
      	  NHISTREC=256/N1+1
      	  IREC=N2*N3+NHISTREC+LABREC
      	  HEAD(3)=IREC
      	  HEAD(4)=NHISTREC
      	  HEAD(5)=IFORM
      	  HEAD(12)=N1
      	  HEAD(13)=LABREC
      	  HEAD(22)=LABBYT
      	  HEAD(23)=LENBYT
      	  CALL FDATE(CDAT)
CTSH      	  CALL TIME(CTIM)
C         DATE TAKES 2 & 1/4 FLOATING POINT VARIABLES IN BUF (9 CHAR)
C          CLINE(1:12)  = CDAT(1:9) // '   '  ! old CALL DATE locations
C          CLINE(1:2)  = CDAT(9:10)           ! new CALL FDATE locations
C          CLINE(3:3)  = '-'
C          CLINE(4:6)  = CDAT(5:7)
C          CLINE(7:7)  = '-'
C          CLINE(8:9)  = CDAT(23:24)
C          CLINE(10:12)  = '   '
         CLINE(1:12)=CDAT(9:10)//'-'//CDAT(5:7)//'-'//CDAT(23:24)//'   '

C         TIME TAKES 2 FLOATING POINT VARIABLES IN BUF (8 CHAR)
CTSH          CLINE(13:20) = CTIM(1:8)
CTSH++
         CLINE(13:20)=CDAT(12:19)
CTSH--
        
C         TITLE TAKES 40 FLOATING POINT VARIABLES IN BUF (160 CHAR)
          CLINE(21:180) = TITLE(1:160)
          DO  I = 1,45
             HEAD(I+211)=ZBUF(I)
          ENDDO
      	  LUNARA(IFILE)=1
C      print *,LENBYT,LABREC,LABBYT,LABBY8
      	  CALL IWRITE(IFILE,CFORM,256,HEAD,0,IRTFLG,DMMM,LUNARA,NSAM)
          CLOSE(IFILE)
          LABBY8=LENBYT
C         ALPHA OSF/1 UNIX SPECIFIC STATEMENT FOLLOWS
CC          LABBY8=LENBYT/4
C      print *,3,N1,CNAME,CSTAT,LABBY8
          OPEN(IFILE,FILE=CNAME,STATUS='OLD',ACCESS='DIRECT',
     &         FORM='UNFORMATTED',RECL=LABBY8,ERR=9999)
C       The following line was added to force the record length RECL
C       (not necessary for g77 but needed for pgf77: compiler bug?)
          WRITE(IFILE,REC=LABREC+1,ERR=9999)
      	  LUNARA(IFILE)=LABREC
      	ENDIF
      	IRTFLG=0
      ELSE
      	STOP ' UNKNOWN MAP FORMAT OPTION'
      ENDIF
      RETURN
9998  CONTINUE
      WRITE(*,*) ' ERROR OPENING/READING SPIDER FILE'
      IRTFLG=-1
      RETURN
9999  CONTINUE
      WRITE(*,*) ' ERROR OPENING SPIDER FILE'
      IRTFLG=-1
      RETURN
      END
C**************************************************************************
      SUBROUTINE ICLOSE(IFILE,CFORM,DMMM,TITLE)
C**************************************************************************
C  Closes a file using formats defined by Imagic, MRC or Spider systems
C  Used in Frealign, CARD8and9, CARD13AND14, MAIN and OPMAPS.
C**************************************************************************
      IMPLICIT NONE

      INTEGER IFILE
      REAL DMIN,DMAX,DMEAN
      DOUBLE PRECISION DMMM(10,4)
      CHARACTER CFORM,TITLE*1600
C**************************************************************************
      DMIN=DMMM(IFILE,1)
      DMAX=DMMM(IFILE,2)
      DMEAN=0.0
      IF (DMMM(IFILE,4).NE.0.0D0) DMEAN=DMMM(IFILE,3)/DMMM(IFILE,4)
      IF (CFORM.EQ.'I') THEN
      	STOP ' IMAGIC FORMAT NOT YET IMPLEMENTED .... SORRY!'
      ELSEIF (CFORM.EQ.'M') THEN
      	CALL IWRHDR(IFILE,TITLE,-1,DMIN,DMAX,DMEAN)
      	CALL IMCLOSE(IFILE)
      ELSEIF (CFORM.EQ.'S') THEN
      	CLOSE(IFILE)
      ELSE
      	STOP ' UNKNOWN MAP FORMAT OPTION'
      ENDIF

      RETURN
      END
C**************************************************************************
      SUBROUTINE IREAD(IFILE,CFORM,N,LINE,IREC,IRTFLG,LUNARA,NSAM)
C**************************************************************************
C  Reads one line of density from a file in Imagic, MRC or Spider format
C  Used in CARD13AND14, IOPEN ANDMAIN.
C**************************************************************************
      IMPLICIT NONE

      INTEGER IFILE,N,IRTFLG,IREC,LUNARA(*),NSAM,NZ,NY
      REAL LINE(N)
      CHARACTER CFORM
C**************************************************************************
      IF (CFORM.EQ.'I') THEN
      	STOP ' IMAGIC FORMAT NOT YET IMPLEMENTED .... SORRY!'
      ELSEIF (CFORM.EQ.'M') THEN
      	NZ=(IREC-1)/NSAM
      	NY=IREC-NZ*NSAM-1
C      if (IREC.lt.3) print *,'R',IFILE,IREC,NY,NZ
      	CALL IMPOSN(IFILE,NZ,NY)
      	CALL IRDLIN(IFILE,LINE,*9998)
C      if (IREC.lt.3) print *,'R',LINE
      ELSEIF (CFORM.EQ.'S') THEN
      	READ(IFILE,REC=IREC+LUNARA(IFILE),ERR=9999) LINE
      ELSE
      	STOP ' UNKNOWN MAP FORMAT OPTION'
      ENDIF

      IRTFLG=0
      RETURN

9998  CONTINUE
      WRITE(*,*) ' ERROR READING MRC FILE'
      IRTFLG=-1
      RETURN
9999  CONTINUE
      WRITE(*,*) ' ERROR READING SPIDER FILE'
      IRTFLG=-1
      RETURN
      END
C**************************************************************************
      SUBROUTINE IWRITE(IFILE,CFORM,N,LINE,IREC,IRTFLG,DMMM,LUNARA,NSAM)
C**************************************************************************
C  Writes one line of density from a file in Imagic, MRC or Spider format
C  Used in MAIN, OPMAPS, IOPEN and MATCH.
C**************************************************************************
      IMPLICIT NONE

      INTEGER IFILE,N,I,IRTFLG,IREC,LUNARA(*),NSAM,NZ,NY
      REAL LINE(N)
      DOUBLE PRECISION DMMM(10,4)
      CHARACTER CFORM
C**************************************************************************
      DO 10 I=1,N
      	IF (DMMM(IFILE,1).GT.LINE(I)) DMMM(IFILE,1)=LINE(I)
      	IF (DMMM(IFILE,2).LT.LINE(I)) DMMM(IFILE,2)=LINE(I)
      	DMMM(IFILE,3)=DMMM(IFILE,3)+LINE(I)
      	DMMM(IFILE,4)=DMMM(IFILE,4)+1.0
10    CONTINUE
      IF (CFORM.EQ.'I') THEN
      	STOP ' IMAGIC FORMAT NOT YET IMPLEMENTED .... SORRY!'
      ELSEIF (CFORM.EQ.'M') THEN
      	NZ=(IREC-1)/NSAM
      	NY=IREC-NZ*NSAM-1
C      if (IREC.lt.3) print *,'W',IFILE,IREC,NY,NZ
      	CALL IMPOSN(IFILE,NZ,NY)
      	CALL IWRLIN(IFILE,LINE)
C      if (IREC.lt.3) print *,'W',LINE
      ELSEIF (CFORM.EQ.'S') THEN
      	WRITE(IFILE,REC=IREC+LUNARA(IFILE),ERR=9999) LINE
      ELSE
      	STOP ' UNKNOWN MAP FORMAT OPTION'
      ENDIF

      IRTFLG=0
      RETURN

9999  CONTINUE
      WRITE(*,*) ' ERROR WRITING SPIDER FILE'
      IRTFLG=-1
      RETURN
      END
C
      SUBROUTINE rlft3(data,speq,nn1,nn2,nn3,isign)
C
      INTEGER isign,nn1,nn2,nn3,istat,iw
      PARAMETER (iw=2048)
      COMPLEX data(nn1/2,nn2,nn3),speq(nn2,nn3)
C
      REAL work(6*iw+15)
C
      INTEGER i1,i2,i3,j1,j2,j3,nn(3),nnh,nnq
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      COMPLEX c1,c2,h1,h2,w
C
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
      return
      END
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
      SUBROUTINE PDA_CFFTB (N,C,WSAVE)
      DIMENSION       C(1)       ,WSAVE(1)
      IF (N .EQ. 1) RETURN
      IW1 = N+N+1
      IW2 = IW1+N+N
      CALL PDA_CFFTB1 (N,C,WSAVE,WSAVE(IW1),WSAVE(IW2))
      RETURN
      END
      SUBROUTINE PDA_CFFTB1 (N,C,CH,WA,IFAC)
      DIMENSION       CH(1)      ,C(1)       ,WA(1)      ,IFAC(1)
      NF = IFAC(2)
      NA = 0
      L1 = 1
      IW = 1
      DO 116 K1=1,NF
         IP = IFAC(K1+2)
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
      SUBROUTINE PDA_CFFTF (N,C,WSAVE)
      DIMENSION       C(1)       ,WSAVE(1)
      IF (N .EQ. 1) RETURN
      IW1 = N+N+1
      IW2 = IW1+N+N
      CALL PDA_CFFTF1 (N,C,WSAVE,WSAVE(IW1),WSAVE(IW2))
      RETURN
      END
      SUBROUTINE PDA_CFFTF1 (N,C,CH,WA,IFAC)
      DIMENSION       CH(1)      ,C(1)       ,WA(1)      ,IFAC(1)
      NF = IFAC(2)
      NA = 0
      L1 = 1
      IW = 1
      DO 116 K1=1,NF
         IP = IFAC(K1+2)
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
      SUBROUTINE PDA_CFFTI (N,WSAVE)
      DIMENSION       WSAVE(1)
      IF (N .EQ. 1) RETURN
      IW1 = N+N+1
      IW2 = IW1+N+N
      CALL PDA_CFFTI1 (N,WSAVE(IW1),WSAVE(IW2))
      RETURN
      END
      SUBROUTINE PDA_CFFTI1 (N,WA,IFAC)
      DIMENSION       WA(1)      ,IFAC(1)    ,NTRYH(4)
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
      IFAC(NF+2) = NTRY
      NL = NQ
      IF (NTRY .NE. 2) GO TO 107
      IF (NF .EQ. 1) GO TO 107
      DO 106 I=2,NF
         IB = NF-I+2
         IFAC(IB+2) = IFAC(IB+1)
  106 CONTINUE
      IFAC(3) = 2
  107 IF (NL .NE. 1) GO TO 104
      IFAC(1) = N
      IFAC(2) = NF
      TPI = 6.28318530717959
      ARGH = TPI/FLOAT(N)
      I = 2
      L1 = 1
      DO 110 K1=1,NF
         IP = IFAC(K1+2)
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
               DO WHILE( CART( K ) .GT. DIM( K ) ) 
                  CART( K ) = 1
                  K = K + 1

                  IF( K .EQ. I ) THEN
                     K = K + 1
                     V0 = V0 + STEP
                  END IF

                  CART( K ) = CART( K ) + 1

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
               DO WHILE( CART( K ) .GT. DIM( K ) ) 
                  CART( K ) = 1
                  K = K + 1

                  IF( K .EQ. I ) THEN
                     K = K + 1
                     V0 = V0 + STEP
                  END IF

                  CART( K ) = CART( K ) + 1

               END DO
                  
            END DO

*  Store the increment in vector address between adjacent elements of
*  the next "row".
            INC = INC*M
            
         END DO

      END IF
         
      END
      SUBROUTINE PDA_PASSB (NAC,IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DIMENSION       CH(IDO,L1,IP)          ,CC(IDO,IP,L1)          ,
     1                C1(IDO,L1,IP)          ,WA(1)      ,C2(IDL1,IP),
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
      SUBROUTINE PDA_PASSB2 (IDO,L1,CC,CH,WA1)
      DIMENSION       CC(IDO,2,L1)           ,CH(IDO,L1,2)           ,
     1                WA1(1)
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
     1                WA1(1)     ,WA2(1)
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
     1                WA1(1)     ,WA2(1)     ,WA3(1)
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
     1                WA1(1)     ,WA2(1)     ,WA3(1)     ,WA4(1)
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
      SUBROUTINE PDA_PASSF (NAC,IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DIMENSION       CH(IDO,L1,IP)          ,CC(IDO,IP,L1)          ,
     1                C1(IDO,L1,IP)          ,WA(1)      ,C2(IDL1,IP),
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
      SUBROUTINE PDA_PASSF2 (IDO,L1,CC,CH,WA1)
      DIMENSION       CC(IDO,2,L1)           ,CH(IDO,L1,2)           ,
     1                WA1(1)
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
     1                WA1(1)     ,WA2(1)
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
     1                WA1(1)     ,WA2(1)     ,WA3(1)
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
     1                WA1(1)     ,WA2(1)     ,WA3(1)     ,WA4(1)
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
      SUBROUTINE PDA_RADB2 (IDO,L1,CC,CH,WA1)
      DIMENSION       CC(IDO,2,L1)           ,CH(IDO,L1,2)           ,
     1                WA1(1)
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
     1                WA1(1)     ,WA2(1)
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
     1                WA1(1)     ,WA2(1)     ,WA3(1)
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
     1                WA1(1)     ,WA2(1)     ,WA3(1)     ,WA4(1)
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
     2                CH2(IDL1,IP)           ,WA(1)
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
     1                WA1(1)
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
     1                WA1(1)     ,WA2(1)
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
     1                WA1(1)     ,WA2(1)     ,WA3(1)
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
     1                WA1(1)     ,WA2(1)     ,WA3(1)     ,WA4(1)
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
     2                CH2(IDL1,IP)           ,WA(1)
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
