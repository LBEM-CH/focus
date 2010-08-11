c	CENTRIC.FOR - set average phases to 0 or 180


c	Per Bullough 6-4-94
c                    5-5-94   redetermines figure of merit and residual in resolution ranges
c

      CHARACTER*50  TITLE 
      INTEGER H,K,L
      DIMENSION DELTA(-100:100,-100:100)

      READ (10,100) TITLE
      WRITE (11,100) TITLE
      WRITE (12,100) TITLE


      READ (*,*) A,B,ARMIN,ARMAX

      if(ARMIN.lt.ARMAX)then
        rtmp=ARMIN
        ARMIN=ARMAX
        ARMAX=rtmp
      endif

      RMIN = 1 / ARMIN
      RMAX = 1 / ARMAX 

      N = 0
      DELTOT = 0.0
      VARTOT = 0.0
      FOMTOT = 0.0

      DO 10 H=-100,100
       DO 10 K=-100,100
        DELTA(H,K) = 1000.0
10    CONTINUE

1000  READ (10,200,END=1010) H,K,L,AMP,PHASE,FOM
      	
      	ASTAR = H / A
        BSTAR = K / B
      	RES = SQRT( ASTAR**2 + BSTAR**2 )
      	
      	IF (RES.LT.RMIN.OR.RES.GT.RMAX) GO TO 1000
      	
      	N = N + 1

      	IF (PHASE. LT. 90.0 .AND. PHASE .GT. -90.0) THEN
          DELTA(H,K) = ABS(PHASE)
          FOM2 = COS(DELTA(H,K)*3.1415927/180)*100
          IF (FOM2.LT.FOM) FOM = FOM2
          if (L.EQ.0) PHASE = 0.0
        END IF
      	IF (PHASE. GE. 90.0 .OR. PHASE .LE. -90.0) THEN 
          DELTA(H,K) = 180.0 - ABS(PHASE)
      	  FOM2 = COS(DELTA(H,K)*3.1415927/180)*100
          IF (FOM2.LT.FOM) FOM = FOM2
          if (L.EQ.0) PHASE = 180.0
      	END IF
      	
      	DELTOT = DELTOT + DELTA(H,K)
      	FOMTOT = FOMTOT + FOM
      WRITE (11,200) H,K,L,AMP,PHASE,FOM
      WRITE (12,210) H,K,AMP,PHASE,FOM
      GO TO 1000
1010  CONTINUE

      IF(N.EQ.0) THEN
         WRITE(*,290) N,ARMIN,ARMAX
      ELSE

         DELAV = DELTOT / N
         FOMAV = FOMTOT / N / 100

         DO 1020 H=-100,100
          DO 1020 K=-100,100
           IF (DELTA(H,K).NE.1000.0) THEN
      	   VAR = (DELTA(H,K) - DELAV)**2
           VARTOT = VARTOT + VAR
           ENDIF
1020     CONTINUE

C
         VARTOT = SQRT(VARTOT / N)
         RN = N
         STANERR = VARTOT / SQRT(RN)

         WRITE (*,300) N,ARMIN,ARMAX,DELAV,STANERR,FOMAV
      ENDIF


      STOP

100   FORMAT (A50)
200   FORMAT (3I6,2F12.1,F12.3)
210   FORMAT (2I6,2F12.1,F12.3)
290   FORMAT (' ',I5,' REFL. OVER ',F5.1,' - ',
     . F5.1,' ANG. NO STATISTICS CALCULATED.')
300   FORMAT (' ',I5,' Refl, Res= ',F5.1,' - ',
     . F5.1,' A, Resid= ',F7.3,' (45=random), Err= ',F7.3,
     . ' FOM= ',F7.3)
 
      END     
