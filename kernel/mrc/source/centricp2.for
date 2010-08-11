c	CENTRIC.FOR - set average phases to 0 or 180

      PARAMETER (ISIZE=50)

c	Per Bullough 6-4-94
c                    5-5-94   redetermines figure of merit and residual in resolution ranges
C       Henning Stahlberg 6.12.2001 p42
c

      CHARACTER*50  TITLE 
      INTEGER H,K,L
      DIMENSION DELTA(-ISIZE:ISIZE,-ISIZE:ISIZE,-ISIZE:ISIZE)
      DIMENSION AMPF(-ISIZE:ISIZE,-ISIZE:ISIZE,-ISIZE:ISIZE)
      DIMENSION PHASEF(-ISIZE:ISIZE,-ISIZE:ISIZE,-ISIZE:ISIZE)
      DIMENSION FOMF(-ISIZE:ISIZE,-ISIZE:ISIZE,-ISIZE:ISIZE)
      DIMENSION ISHERE(-ISIZE:ISIZE,-ISIZE:ISIZE,-ISIZE:ISIZE)

      READ (10,100) TITLE
      WRITE (11,100) TITLE
      WRITE (12,100) TITLE


      READ (*,*) A,B,ARMIN,ARMAX

      if(ARMIN.lt.ARMAX)then
        rtmp=ARMIN
        ARMIN=ARMAX
        ARMAX=rtmp
      endif

      RFAC=3.141592654/180.0
    
      RMIN = 1 / ARMIN
      RMAX = 1 / ARMAX 

      N = 0
      DELTOT = 0.0
      VARTOT = 0.0
      FOMTOT = 0.0

      DO 10 H=-ISIZE,ISIZE
       DO 10 K=-ISIZE,ISIZE
        DO 10 L=-ISIZE,ISIZE
         DELTA(H,K,L) = 1000000.0
         ISHERE(H,K,L) = 0
10    CONTINUE

1000  READ (10,200,END=1010) H,K,L,AMP,PHASE,FOM
   
        if(PHASE.GT. 180.0)PHASE=PHASE-360.0
        if(PHASE.LT.-180.0)PHASE=PHASE+360.0

        AMPF(H,K,L) = AMP
        PHASEF(H,K,L) = PHASE
        FOMF(H,K,L) = FOM
        ISHERE(H,K,L)=1

      	ASTAR = H / A
        BSTAR = K / B
      	RES = SQRT( ASTAR**2 + BSTAR**2 )
      	
      	IF (RES.LT.RMIN.OR.RES.GT.RMAX) GO TO 1000
      	
      	N = N + 1

      	IF (PHASE. LT. 90.0 .AND. PHASE .GT. -90.0) THEN
          DELTA(H,K,L) = ABS(PHASE)
          FOM2 = COS(DELTA(H,K,L)*3.1415927/180)*100
          IF (FOM2.LT.FOM) FOM = FOM2
        END IF
      	IF (PHASE. GE. 90.0 .OR. PHASE .LE. -90.0) THEN 
          DELTA(H,K,L) = 180.0 - ABS(PHASE)
      	  FOM2 = COS(DELTA(H,K,L)*3.1415927/180)*100
          IF (FOM2.LT.FOM) FOM = FOM2
      	END IF
      	
      	DELTOT = DELTOT + DELTA(H,K,L)
      	FOMTOT = FOMTOT + FOM
      goto 1000
C
1010  CONTINUE
C
      do H = 0,ISIZE
        do K = -ISIZE,ISIZE
          do L = -ISIZE,ISIZE
            ICOUNT=0
            AMP=0.0
            PHASEX=0.0
            PHASEY=0.0
            FOM=0.0
            if(ISHERE(H,K,L).EQ.1)then
              AMP=AMP+AMPF(H,K,L)
              PHASEX=PHASEX+cos(PHASEF(H,K,L)*RFAC)
              PHASEY=PHASEY+sin(PHASEF(H,K,L)*RFAC)
              FOM=FOM+FOMF(H,K,L)
              ICOUNT=ICOUNT+1
            endif
            if(ISHERE(-H,-K,L).EQ.1.and.H.ne.0.and.K.ne.0)then
              AMP=AMP+AMPF(-H,-K,L)
              PHASEX=PHASEX+cos(PHASEF(-H,-K,L)*RFAC)
              PHASEY=PHASEY+sin(PHASEF(-H,-K,L)*RFAC)
              FOM=FOM+FOMF(-H,-K,L)
              ICOUNT=ICOUNT+1
            endif
            if(ICOUNT.GT.0)then
              AMP=AMP/ICOUNT
              if(PHASEX.eq.0)then
                if(PHASEY.le.1.0)then
                  PHASE= 90.0
                else
                  PHASE=-90.0
                endif
              else
                PHASE=atan(PHASEY/PHASEX)/RFAC
              endif
              if(PHASEX.lt.0.0)PHASE=PHASE-180.0
              FOM=FOM/ICOUNT
              if(PHASE.GT. 180.0)PHASE=PHASE-360.0
              if(PHASE.LT.-180.0)PHASE=PHASE+360.0
              if(PHASE. LT. 90.0 .AND. PHASE .GT. -90.0) THEN
                 if(L.eq.0) PHASE=0.0
              else 
                 if(L.eq.0) PHASE=180.0
              endif
              WRITE (11,200) H,K,L,AMP,PHASE,FOM
              WRITE (12,210) H,K,AMP,PHASE,FOM
            endif
          enddo
        enddo
      enddo

      IF(N.EQ.0) THEN
         WRITE(*,290) N,ARMIN,ARMAX
      ELSE

         DELAV = DELTOT / N
         FOMAV = FOMTOT / N / 100

         DO 1020 H=-ISIZE,ISIZE
          DO 1020 K=-ISIZE,ISIZE
           DO 1020 L=-ISIZE,ISIZE
            IF (DELTA(H,K,L).NE.1000.0) THEN
      	     VAR = (DELTA(H,K,L) - DELAV)**2
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
200   FORMAT (3I4,2F8.1,F8.3)
210   FORMAT (2I4,2F8.1,F8.3)
290   FORMAT (' ',I5,' REFL. OVER ',F5.1,' - ',
     . F5.1,' ANG. NO STATISTICS CALCULATED.')
300   FORMAT (' ',I5,' Refl, Res= ',F5.1,' - ',
     . F5.1,' A, Resid= ',F7.3,' (45=random), Err= ',F7.3,
     . ' FOM= ',F7.3)
 
      END     
