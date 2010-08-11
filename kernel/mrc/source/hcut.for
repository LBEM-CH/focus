C         Program removes layer-lines which begin with resolutions lower 
C         than RESMIN or higher than RESMAX grid units. 
C         Used for trimming layer-line lengths after applying, e.g., HFIT;
C         also for evaluating phase errors as a function of resolution.
C         
C
C         Res: 50   25   16.6   12.5   10   8.3  A
C         G.U. 44   88   132    176   220
C
C         LLFACT  - 1 FOR LL NUMBERS EQUAL TO GRID UNITS
C                 - 2 FOR LL NUMBERS EVERY 2 GRID UNITS
C         ZFACT   - NUMBER OF GRID UNITS ALONG MERIDIAN IN TRANSFORM/ 
C                   NUMBER OF GRID UNITS ALONG MERIDIAN IN BOX  
C
C
      REAL*8 RECSP
      CHARACTER*98 LLTEXT
C
      READ(5,*)LLFACT,ZFACT,RESMIN,RESMAX
c     CALL DOPEN(1,'IN','RO','F')
c     CALL DOPEN(2,'OUT','NEW','F')
      CALL ccpdpn(1,'IN','READONLY','F',0,0)
      CALL ccpdpn(2,'OUT','NEW','F',0,0)
203   I=0
      J=0
      READ(1,200,END=110)LLTEXT,WTFAC,NORD,LLNO 
      WRITE(6,201)LLTEXT(1:50),WTFAC,NORD,LLNO 
109   READ(1,202)RECSP,AIN,PIN
      HIN=RECSP/0.000454215
      KIN=LLNO*LLFACT
      X = HIN
      Y = KIN*ZFACT*0.5
      IF(Y.GE.RESMAX) GO TO 110
      RAD = SQRT(X**2+Y**2)
      IF(RAD.LT.RESMIN.AND.AIN.NE.0.0) THEN
      J=1
      GO TO 109
      ENDIF
      IF(J.EQ.1.AND.AIN.NE.0.0) GO TO 109
      IF(RAD.GE.RESMAX) GO TO 109
      IF(I.EQ.0) THEN
      IF(AIN.EQ.0.0) GO TO 203
      WRITE(2,200)LLTEXT,WTFAC,NORD,LLNO 
      START=HIN
      ENDIF
      I=I+1
      WRITE(2,202)RECSP,AIN,PIN
      IF(RECSP.EQ.0.0.AND.AIN.EQ.0.0.AND.PIN.EQ.0.0) THEN
      IEND=START+I-0.5
      WRITE(6,205)IEND
      GO TO 203
      ENDIF
      GO TO 109
110   CLOSE(1)
      CLOSE(2)
C
C
202   FORMAT(3E12.5)
200   FORMAT(A,E12.5,2I5)
201   FORMAT(1X,A,E12.5,2I5)
205   FORMAT(2X,'Maximum value for R: ',I5,' grid units'/)
      STOP
      END
