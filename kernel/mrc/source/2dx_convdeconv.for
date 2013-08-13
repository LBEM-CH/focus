C CONVDECONV *******************************************************************
C
C       Program to read in a list of amplitudes and phases and calculate
C       a new list of amplitudes and phases which represent either the
C       convolution or deconvolution of the original.  The necessary 
C       information to describe the relative (x,y) coordinates of the layers
C       required in the convolution or known to be present in the image which
C       is to be deconvoluted, must be given as input data.
C
C       Formula used for convolution is 
C           (P+iQ) = (A + iB) * (C1 + i*C2) = [(C1*A - C2*B) + i*(C2*A + C1*B)]
C       or, for deconvolution, is
C           (P+iQ) = [(C1*A + C2*B) + i*(-C2*A + C1*B)] / [C1**2 + C2**2 + WIEN]
C       or, for mirror-deconvolution, is 
C           (P+iQ) = [A+iB-Trans(A+iB)*(C1+iC2)] / [1-Trans(C1+iC2)*(C1+iC2)]
C
C                       where C1 = Sum of cos(2*PI*(x/a+y/b)))
C                       and   C2 = Sum of sin(2*PI*(x/a+y/b)))
C                       where (x,y) are the coordinate shifts to be applied to the
C                               map whose structure factors are read from the 
C                               input file in convolution, and which describe the 
C                               coordinates of the layers in deconvolution relative
C                               to the final deconvoluted map obtained.
C                       and a and b are the unit cell dimensions
C                       WIEN is a WIENer term to avoid divide-by-zero
C       Input data cards
C       1.  C/D/M       Convolute or Deconvolute or Mirror-Deconvolute
C       In case of Convolute or Deconvolute:
C         2.  x/a,y/b   fractional shifts of all required layers
C                       number of these cards will equal the number of layers
C       In case of Mirror-deconvolution:
C         2.  x/a,y/b   fractional shifts of second layer (e.g. 0.0,0.25)
C                       the first layer is assumed to be centered after phase 
C                       phase origin shift.
C         3.  XORI,YORI phase origin in degrees
C
C       Input file is a .aph file, consisting of IH,IK,AMP,PHASE,IQ,BACK,CTF
C
C  Vx 1.00      06.02.2000      RH      Original version
C  Vx 1.01      18.10.2000      RH      change spelling to WIEN (Wiener filter)
C  Vx 2.00      27.11.2001      HS      mirror-deconvolution
C  Vx 2.01      15.08.2013      HS      adaptation to 2dx
C
C           remember to change date in first format statement
C
C*******************************************************************************
      PARAMETER (NMAX=50)
C      PARAMETER (NINIT=(NMAX+1)*(2*NMAX+1))
C      PARAMETER (NINIT=(51)*(101))
      PARAMETER (NINIT=5151)
      REAL A(0:NMAX,-NMAX:NMAX),B(0:NMAX,-NMAX:NMAX)
      REAL C1(0:NMAX,-NMAX:NMAX),C2(0:NMAX,-NMAX:NMAX)
      REAL CTF(0:NMAX,-NMAX:NMAX),FXSH(20),FYSH(20)
      REAL BCK(0:NMAX,-NMAX:NMAX)
      COMPLEX CP,CA,CB,CAT,CBT,CTMP
      INTEGER IQ(0:NMAX,-NMAX:NMAX)
      REAL TITLE(20),TITLEOUT(20)
      CHARACTER*4 CTITLE(20),CTITLEOUT(20)
      CHARACTER*1 CICD 
      REAL PI
      DATA A/NINIT*-999.0/,B/NINIT*-999.0/
      DATA PI/3.141596254/
      DATA CTITLEOUT/' CON','VDEC','ONV,',' new','stru','ctur',
     .  'e fa','ctor','s   ',11*'    '/
C
C  read in first file
C
      READ(5,'(A1)') CICD
      IF(CICD.EQ.'C'.OR.CICD.EQ.'c') then
        WRITE(6,2)
        CICD='C'
      endif
      IF(CICD.EQ.'D'.OR.CICD.EQ.'d') then
        WRITE(6,3)
        CICD='D'
      endif
      IF(CICD.EQ.'R'.OR.CICD.EQ.'r') then
        WRITE(6,4)
        CICD='R'
      endif
      IF(CICD.EQ.'T'.OR.CICD.EQ.'t') then
        WRITE(6,4)
        CICD='T'
      endif
      IF(CICD.EQ.'M'.OR.CICD.EQ.'m') then
        WRITE(6,5)
        CICD='M'
      endif
      IF(CICD.EQ.'S'.OR.CICD.EQ.'s') then
        WRITE(6,5)
        CICD='S'
      endif
      IF(CICD.EQ.'U'.OR.CICD.EQ.'u') then
        WRITE(6,5)
        CICD='U'
      endif
2     FORMAT(' Program 2dx_CONVDECONV 14.08.13 - convolution mode')
3     FORMAT(' Program 2dx_CONVDECONV 14.08.13 - deconvolution mode')
4     FORMAT(' Program 2dx_CONVDECONV 14.08.13 - ',
     .       'mirror-convolution mode')
5     FORMAT(' Program 2dx_CONVDECONV 14.08.13 - ',
     .       'mirror-deconvolution mode')
C
      if((CICD.EQ.'M').or.(CICD.EQ.'R').or.(CICD.EQ.'U'))then
        READ(5,*) FXSH(1),FYSH(1)
        WRITE(6,*) FXSH(1),FYSH(1)
        NLAYERS=1
      elseif((CICD.EQ.'S').or.(CICD.EQ.'T').or.(CICD.EQ.'U'))then
        READ(5,*) RXDES,RYDES,RXDESS,RYDESS,IXN,IYN,ISTART
        RXLDES=RXDES-(ISTART*RXDESS)+(IXN*RXDESS)
        RYLDES=RYDES-(ISTART*RYDESS)+(IYN*RYDESS)
        FXSH(1)=(RXLDES-200.0)/200.0
        FYSH(1)=(RYLDES-200.0)/200.0
        WRITE(6,*) FXSH(1),FYSH(1)
        NLAYERS=1
        if(CICD.eq.'S')CICD='M'
        if(CICD.eq.'T')CICD='R'
      else
        DO 10 I=1,20
          READ(5,*,END=11) FXSH(I),FYSH(I)
          WRITE(6,*) FXSH(I),FYSH(I)
10      CONTINUE
        STOP ' current program accepts only 20 layers'
11      NLAYERS=I-1
      endif
      WRITE(6,12) NLAYERS
12    FORMAT(' coordinates of',I5,' layers read in')
C
      if((CICD.EQ.'M').or.(CICD.EQ.'R'))then
        READ(5,*)RXORI,RYORI
        WRITE(6,'('' XORI,YORI = '',2F10.3)')RXORI,RYORI
      elseif(CICD.eq.'U')then
        READ(5,*) RXDES,RYDES,RXDESS,RYDESS,IXN,IYN,ISTART
        RXORI=RXDES-(ISTART*RXDESS)+(IXN*RXDESS)
        RYORI=RYDES-(ISTART*RYDESS)+(IYN*RYDESS)
        WRITE(6,'('' XORI,YORI = '',2F10.3)')RXORI,RYORI
        CICD='M'
      endif
C        
      CALL CCPDPN(1,'IN','READONLY','F',0,0)
      READ(1,21) NSER,TITLE
      WRITE(6,22) TITLE
21    FORMAT(I10,20A4)
22    FORMAT(' Input structure factor file title'/20A4)
      NIN=0
20    READ(1,*,END=30) IH,IK,AMP,PHASE,IQIN,BACK,CTFIN
        IF(AMP.LE.0.0) GOTO 20
        NIN=NIN+1
        IF(IH.LT.0) THEN
          IH=-IH
          IK=-IK
          PHASE=-PHASE
        ELSEIF(IH.EQ.0) THEN
          IF(IK.LT.0) THEN
            IH=-IH
            IK=-IK
            PHASE=-PHASE
          ENDIF
        ENDIF
        IF(IH.GT.NMAX.OR.IABS(IK).GT.NMAX) 
     1    STOP 'spot index too large for program dimension NMAX'
C
        if((CICD.EQ.'M').or.(CICD.EQ.'R'))then
          PHASE=PHASE+(RXORI*IH)+(RYORI*IK)
        endif
C
        A(IH,IK)=AMP*COS(PHASE*PI/180.0)
        B(IH,IK)=AMP*SIN(PHASE*PI/180.0)
        IQ(IH,IK)=IQIN
        CTF(IH,IK)=CTFIN
        BCK(IH,IK)=BACK
      GO TO 20
30    WRITE(6,25) NIN
25    FORMAT(I10,' non-zero spots read in from the input file')
      CLOSE(1)
C
      rabs=1.0e-7
C  Now calculate C1 and C2
      DO 51 IH=0,NMAX
        DO 50 IK=-NMAX,NMAX
          IF(A(IH,IK).NE.-999.0) THEN
            C1(IH,IK)=0.0
            C2(IH,IK)=0.0
            DO 55 I=1,NLAYERS
              C1(IH,IK)=C1(IH,IK)+COS(2.0*PI*(IH*FXSH(I)-IK*FYSH(I)))
              C2(IH,IK)=C2(IH,IK)+SIN(2.0*PI*(IH*FXSH(I)-IK*FYSH(I)))
55          CONTINUE
          ENDIF
50      CONTINUE
51    CONTINUE
C
C  Now calculate either the convoluted or deconvoluted output coefficients
C
      SCALE=1.0
70    CALL CCPDPN(3,'OUT','NEW','F',0,0)
      WRITE(3,21) NSER,TITLEOUT
      NOUT=0
      NONZERO=0
      AMAX=0.0
      if(CICD.EQ.'C') THEN
        WRITE(*,
     1  '(''  IH, IK,     P,        Q, A(IH,IK),B(IH,IK),'',
     2  '' C1(IH,IK),C2(IH,IK)'')')
      else
        WRITE(*,
     1  '(''  IH, IK,     P,        Q, A(IH,IK),B(IH,IK),'',
     2  '' A(IH,-IK),B(IH,-IK), C1(IH,IK),C2(IH,IK)'')')
        WRITE(*,
     1  '(''      CA                   CAT                    CB'',
     2    ''                  CBT                CP  '')')
      endif
      DO 110 IH=0,NMAX
        DO 100 IK=-NMAX,NMAX
          IF(    (((CICD.EQ.'C').or.(CICD.EQ.'D'))
     1            .and.(A(IH, IK).NE.-999.0)    )
     2       .OR.(((CICD.EQ.'M').or.(CICD.EQ.'R'))
     3            .and.(A(IH, IK).NE.-999.0)
     4            .and.(A(IH,-IK).NE.-999.0)    ))then
            NOUT=NOUT+1
C-----------Convolution
            IF(CICD.EQ.'C') THEN
              P = C1(IH,IK)*A(IH,IK) - C2(IH,IK)*B(IH,IK)
              Q = C2(IH,IK)*A(IH,IK) + C1(IH,IK)*B(IH,IK)
              BACK=BCK(IH,IK)*NLAYERS
              WRITE(*,'(2I4,6F9.2)')IH,IK,P,Q,A(IH,IK),B(IH,IK),
     1          C1(IH,IK),C2(IH,IK)
C-----------Deconvolution
            ELSEIF(CICD.EQ.'D') THEN
              P = C1(IH,IK)*A(IH,IK) + C2(IH,IK)*B(IH,IK)
              Q = -C2(IH,IK)*A(IH,IK) + C1(IH,IK)*B(IH,IK)
              WIEN = IQ(IH,IK)/7.0
              DENOM = C1(IH,IK)**2 + C2(IH,IK)**2 + WIEN**2
              IF(DENOM.NE.0.0) THEN     
                P=P/DENOM
                Q=Q/DENOM
              ENDIF
              BACK=BCK(IH,IK)/NLAYERS
              WRITE(*,'(2I4,6F9.2)')IH,IK,P,Q,A(IH,IK),B(IH,IK),
     1          C1(IH,IK),C2(IH,IK)
C-----------Mirror-convolution
            ELSEIF(CICD.EQ.'R') THEN
C
              CA =COMPLEX(A(IH, IK), B(IH, IK))
              CAT=COMPLEX(A(IH,-IK), B(IH,-IK))
C
              CB =COMPLEX(C1(IH, IK),C2(IH, IK))
              CBT=COMPLEX(C1(IH,-IK),C2(IH,-IK))
C
              CP=CAT*CB+CA
C
              P = REAL(CP)
              Q = IMAG(CP)
C
              BACK=(BCK(IH,IK)+BCK(IH,-IK))/2.0
C
              WRITE(*,'(90(''-''))')
              WRITE(*,'(2I4,8F9.2)')IH,IK,P,Q,A(IH,IK),B(IH,IK),
     1          A(IH,-IK),B(IH,-IK),C1(IH,IK),C2(IH,IK)
              WRITE(*,'(5(2F9.2,''___''))')CA,CAT,CB,CBT,CP
C
C-----------Mirror-deconvolution
C-----------(P+iQ) = [A+iB-Trans(A+iB)*(C1+iC2)] / [1-Trans(C1+iC2)*(C1+iC2)]
            ELSEIF(CICD.EQ.'M') THEN
C
              CA =COMPLEX(A(IH, IK), B(IH, IK))
              CAT=COMPLEX(A(IH,-IK), B(IH,-IK))
C
              CB =COMPLEX(C1(IH, IK),C2(IH, IK))
              CBT=COMPLEX(C1(IH,-IK),C2(IH,-IK))
C
C-------------Calculate now:   CP=(CA-CAT*CB)/(1-CBT*CB)
              CTMP=(1-CBT*CB)*conjg(1-CBT*CB)
              RTMP=REAL(CTMP)
              if(RTMP.GT.1e-7)then
                CP=(CA-CAT*CB)*conjg(1-CBT*CB)/RTMP
              else
                CP=COMPLEX(0.0,0.0)
              endif
C
              P = REAL(CP)
              Q = IMAG(CP)
C
              BACK=(BCK(IH,IK)+BCK(IH,-IK))/2.0
C
              WRITE(*,'(90(''-''))')
              WRITE(*,'(2I4,8F9.2)')IH,IK,P,Q,A(IH,IK),B(IH,IK),
     1          A(IH,-IK),B(IH,-IK),C1(IH,IK),C2(IH,IK)
              WRITE(*,'(5(2F9.2,''___''))')CA,CAT,CB,CBT,CP
C
            ELSE
              P = 0.0
              Q = 0.0
            ENDIF
C
C-----------
C
            R=P*P+Q*Q
            IF(R.GT.0.0) THEN
              AMP=SCALE*SQRT(R)
              PHASE=ATAN2(Q,P)*180.0/PI
C
              if((CICD.EQ.'M').or.(CICD.EQ.'R'))then
                PHASE=PHASE-(RXORI*IH)-(RYORI*IK)
              endif
C
              if(PHASE.lt.  0.0)PHASE=PHASE+360.0
              if(PHASE.gt.360.0)PHASE=PHASE-360.0
C
              NONZERO=NONZERO+1
            ELSE
              AMP=0.0
              PHASE=0.0
            ENDIF
            PHSERR = (180.0/PI)*BACK/AMP
            IQOUT = 1 + (PHSERR/7.0)           ! THIS MEANS IQ=1 HAS AMP= 8x BACK
            IQOUT = MIN(IQOUT,8)               !            IQ=7     AMP= 1x BACK
            CTFOUT=CTF(IH,IK)
C
            WRITE(3,99)IH,IK,AMP,PHASE,IQOUT,BACK,CTFOUT
99          FORMAT(2I4,2F8.1,I3,F8.1,F8.3)
            IF(AMP.GT.AMAX) AMAX=AMP
C
          ENDIF
100     CONTINUE
110   CONTINUE
      CLOSE(3)
      IF(AMAX.GT.99990.0) THEN
C***    SCALE=99990.0/AMAX
        SCALE=990.0/AMAX
        WRITE(6,112) SCALE
112     FORMAT(' Coefficients too large for format statement,',
     .          ' scaled down by',F8.5)
        GO TO 70
      ENDIF
      WRITE(6,111) NOUT,NONZERO
111   FORMAT(I6,' CONVDECONV structures factors written'/
     .  I6,' non-zero'/'       normal termination'/)
      END
