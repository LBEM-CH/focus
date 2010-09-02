C  CALCULATE TILT ANGLES FROM TILTED AND UNTILTED RECIPROCAL
C  CELL DIMENSIONS.
C
C  CONVENTION FOR MEASURING TILT AXIS TO ASTAR IS THAT THE ANGLE IS
C  FROM TILTAXIS TO ASTAR IN THE DIRECTION GIVEN BY ASTAR TO BSTAR
C  BEING POSITIVE.
C
        IMPLICIT REAL*8 (A-H,O-Z)
        character*80 cname
C
CHENN>
        PI=3.141592654
CHENN<
C
        WRITE(6,102)
102     FORMAT(' TILT ANGLE PROGRAM - PETER SHAW 3.5.81')
        WRITE(6,100)
100     FORMAT('$UNTILTED ASTAR,BSTAR,GAMMASTAR ...')
        READ(5,*) A,B,GAMMA
CHENN>
        WRITE(6,'('' Read '',3F12.3)')A,B,GAMMA
CHENN<
103     WRITE(6,101)
101     FORMAT('$TILTED ASTAR,BSTAR,GAMMASTAR ...')
        READ(5,*) AT,BT,GAMMAT
CHENN>
        WRITE(6,'('' Read '',3F12.3)')AT,BT,GAMMAT
C
        WRITE(6,'(''$OLD TILTAXIS ...'')')
        READ(5,*) OLDTLTAXIS
        WRITE(6,'('' Read '',1F12.3)')OLDTLTAXIS
C
        WRITE(6,'(''$OLD TILTANGL ...'')')
        READ(5,*) OLDTLTANG
        WRITE(6,'('' Read '',1F12.3)')OLDTLTANG
C
        WRITE(6,'(''$HANDEDNESS of lattice'')')
        READ(5,*) IHAND
        WRITE(6,'('' Read '',I6)')IHAND
C
C-------ALPHA is the angle from X-Axis to H-Vector.
        WRITE(6,'(''$ALPHA'')')
        READ(5,*) ALPHA
        WRITE(6,'('' Read '',F12.3)')ALPHA
C
        WRITE(6,'('' input docfile name for output '')')
        READ(5,*) cname
        WRITE(6,'('' Read '',A40)')cname
C
        COSG = DCOS(GAMMA*PI/180.0)
        COSGT= DCOS(GAMMAT*PI/180.0)
        SING = DSIN(GAMMA*PI/180.0)
C
C       COSG = DCOS(GAMMA*3.14159/180.0)
C       COSGT= DCOS(GAMMAT*3.14159/180.0)
C       SING = DSIN(GAMMA*3.14159/180.0)
C
CHENN<
C
        IF(AT.LE.0.)STOP
C
        C1 = (A*A)/(AT*AT)
        C2 = (B*B)/(BT*BT)
        C3 = (A*B)/(AT*BT*COSGT)
        C4 = (A*B*COSG)/(AT*BT*COSGT)
C
        AX = C2*(C1/C3)*(C1/C3) - C1
        BX = C2 + 2.0*C2*(C1/C3)*((C1-C4)/C3) - C1
        CX = C2*((C1-C4)/C3)*((C1-C4)/C3)
        DISC = BX*BX - 4.0*AX*CX
        IF (DISC .LT. 0.0) GO TO 200
        PSQ1 = (-BX + DSQRT(DISC))/(2.0*AX)
        PSQ2 = (-BX - DSQRT(DISC))/(2.0*AX)
        IF (PSQ1 .LT. 0.0 .AND. PSQ2 .LT. 0.0) GO TO 210
        IF (PSQ1 .GT. 0.0) PP = DSQRT(PSQ1)
        IF (PSQ2 .GT. 0.0) PP = DSQRT(PSQ2)
        QQ = (C1/C3)*PP + ((C1-C4)/C3)*(1.0/PP)
        XK = DSQRT(C1*(PP*PP + 1.0))
        WRITE(6,110) PP,QQ,XK
110     FORMAT (' P,Q, AND SCALE FACTOR ',3F15.5)
        PHI = DATAN2(SING,QQ/PP-COSG)
        SINPHI = DSIN(PHI)
        COSPHI = DCOS(PHI)
        THETA = DATAN2(PP,SINPHI)
        TANTHE = DTAN(THETA)
CHENN>
C       PHI = PHI*180.0/3.14159
C       THETA = THETA*180.0/3.14159
C
        PHI = PHI*180.0/PI
        THETA = THETA*180.0/PI
C
        if(OLDTLTANG.gt.0.0)then
          THETA=ABS(THETA)
        else
          THETA=-ABS(THETA)
        endif
C
        WRITE(*,'('' XK = '',F15.5)') XK
C
        WRITE(*,'('' PHI = ANGLE FROM TILT AXIS TO ASTAR'',
     1   '' = '',F15.5)') PHI
        WRITE(*,'('' THETA = TILT ANGLE = '',F15.5)')
     1   THETA
C
        if(PHI.gt. 90)PHI=PHI-180.0
        if(PHI.lt.-90)PHI=PHI+180.0
C
        WRITE(*,'('' PHI = '',F15.5)') PHI
        WRITE(*,'('' THETA = '',F15.5)') THETA
C
C       WRITE(6,111) PHI,THETA
C111    FORMAT (' ANGLE FROM TILT AXIS TO ASTAR, AND TILT ANGLE',2F15.5)
C
CHENN<
C
C  CALCULATE ANGLE TO TILTED ASTAR ... THIS IS THE ANGLE ONE
C  WOULD GET DIRECTLY FROM THE FILM, FOR EXAMPLE BY FINDING
C  THE DIRECTION OF ZERO CHANGE IN THE C.T.F.
C
        COSPHI = COSPHI/(DSQRT(1.0 + SINPHI*SINPHI*TANTHE*TANTHE))
        PHITLT = DACOS(COSPHI)*180.0/3.14159
CHENN>
        if(PHITLT.gt. 90.0)PHITLT=PHITLT-180.0
        if(PHITLT.lt.-90.0)PHITLT=PHITLT+180.0
C        WRITE(13,112) PHITLT
C
C       WRITE(6,112) PHITLT
C112    FORMAT(' ANGLE FROM TILT AXIS TO TILTED ASTAR ...',F15.5)
C
        WRITE(*,'(
     1   '' ANGLE FROM TILT AXIS TO TILTED ASTAR ...'',
     2   '' PHITLT = '',F15.5)') PHITLT
        if(PHITLT.lt.0.0)PHITLT=-PHITLT
        WRITE(*,'('' PHITLT (abs) = '',F15.5)') PHITLT
C
C--------------------------------
C
        write(*,'(/''----------------------------------------'',
     1             ''----------------------------------------''/)')
C
        write(*,'('' PHI    = '',F15.5,''    (Tilt Axis -> A*)'')')
     1    PHI
C
C-------ALPHA is the angle from X-Axis to H-Vector.
        write(*,'('' ALPHA  = '',F15.5,''    (X-Axis -> H)'')')
     1    ALPHA
C
        write(*,'('' PHITLT = '',F15.5,''    (Tilt Axis -> '',
     1    ''tilted A*)'')')PHITLT
C
        TLTANG = THETA
        write(*,'('' TLTANG = '',F15.5)')TLTANG
C
        TLTAXIS = ALPHA - PHITLT
        if(TLTAXIS.gt. 90.0)TLTAXIS=TLTAXIS-180.0
        if(TLTAXIS.gt. 90.0)TLTAXIS=TLTAXIS-180.0
        if(TLTAXIS.lt.-90.0)TLTAXIS=TLTAXIS+180.0
        if(TLTAXIS.lt.-90.0)TLTAXIS=TLTAXIS+180.0
        write(*,'('' 1. TILTAXIS = '',F15.5,''    = ALPHA - PHITLT''
     1    )')TLTAXIS
        TLTAXS1 = TLTAXIS
C
        TLTAXIS = ALPHA + PHITLT
        if(TLTAXIS.gt. 90.0)TLTAXIS=TLTAXIS-180.0
        if(TLTAXIS.gt. 90.0)TLTAXIS=TLTAXIS-180.0
        if(TLTAXIS.lt.-90.0)TLTAXIS=TLTAXIS+180.0
        if(TLTAXIS.lt.-90.0)TLTAXIS=TLTAXIS+180.0
        write(*,'('' 2. TILTAXIS = '',F15.5,''    = ALPHA + PHITLT''
     1    )')TLTAXIS
        TLTAXS2 = TLTAXIS
C
        RDIST1 = abs(OLDTLTAXIS - TLTAXS1)
        RDIST2 = abs(OLDTLTAXIS - TLTAXS2)
C
        ISGNTLTAN1=1
        ISGNTLTAN2=1
        if(RDIST1.gt.90.0)then
          RDIST1=180.0-RDIST1
          ISGNTLTAN1=-1
        endif
        if(RDIST2.gt.90.0)then
          RDIST2=180.0-RDIST2
          ISGNTLTAN2=-1
        endif
C
        write(*,'('' TLTAXIS distance for first  option is '',F15.5)')
     1    RDIST1
        write(*,'('' TLTAXIS distance for second option is '',F15.5)')
     1    RDIST2
C
        if(RDIST1.lt.RDIST2)then
          write(*,'('' Taking first option.'')')
          TLTAXIS = TLTAXS1
          TLTAXA = PHITLT
          ISGNTLTANG=ISGNTLTAN1
        else
          write(*,'('' Taking second option.'')')
          TLTAXIS = TLTAXS2
          TLTAXA = -PHITLT
          ISGNTLTANG=ISGNTLTAN2
        endif
        if(IHAND.lt.0)then
          write(*,'('' Left-handed lattice, revertings sign of TLTAXA'')')
          TLTAXA=-TLTAXA
        endif
        write(*,'('' Sign of TLTANG is '',I6)')ISGNTLTANG
        write(*,'('' TLTAXIS = '',F15.5)')TLTAXIS
        write(*,'('' TLTAXA = '',F15.5)')TLTAXA
C
        if(OLDTLTAXIS.ge.0.0)THEN
          ISGNTLTAXIS = 1
        else
          ISGNTLTAXIS = -1
        endif
        write(*,'('' Sign of OLDTLTAXIS is '',I6)')ISGNTLTAXIS
C
        TAXA = abs(PHI)
        if(TLTAXA.lt.0.0)TAXA=-TAXA
        write(*,'('' TAXA is '',F15.5)')TAXA
C
        TLTNORM = TLTAXIS + 90.0
        ANGACOMP = abs(ALPHA - TLTNORM)
        if(ANGACOMP.gt.90.0 .and. ANGACOMP.lt.270.0)then
          ISGNAISABOVE = -1
        else
          ISGNAISABOVE = 1
        endif
        write(*,'('' ISIGNAISABOVE is if A is above TLTAXIS = '',I6)')
     1    ISGNAISABOVE
C
        if(TLTAXA.ge.0.0)then
          ISGNTLTAXA = 1
        else
          ISGNTLTAXA = -1
        endif
        write(*,'('' ISIGNTLTAXA = '',I6)')ISGNTLTAXA
C
        TANGL = TLTANG * ISGNAISABOVE * ISGNTLTAXA * IHAND
        write(*,'('' TANGL = '',F15.5)')TANGL
C
        OPEN(UNIT=15,FILE=cname,STATUS='NEW')
        write(15,'('' TLTAXIS = '',/,F15.5)')TLTAXIS
        write(15,'('' TLTANG  = '',/,F15.5)')TLTANG
        write(15,'('' TLTAXA  = '',/,F15.5)')TLTAXA
        write(15,'('' TAXA    = '',/,F15.5)')TAXA
        write(15,'('' TANGL   = '',/,F15.5)')TANGL
        CLOSE(15)
C
C       GO TO 103
CHENN<
200     WRITE (6,201)
201     FORMAT (' DISCRIMINANT LESS THAN ZERO')
        STOP
210     WRITE (6,211)
211     FORMAT(' TWO NEGATIVE ROOTS - SOMETHING WRONG')
        STOP
        END

