C  DIVIDEQ.FOR
C
C	Vx 1.0	RH	21-Dec-1995	updated input format, brought on to ALF2
C	Vx 1.1	RH	04-Sep-1999	dimension increase
C
C  DIVIDE - EQUALLY ACCURATELY  6.10.87
C	PROGRAM TO SPLIT MERGED LIST INTO TWO PARTS
C	WITH AS EQUAL ACCURACY HALVES AS POSSIBLE.
C
C	Control cards
C
C	Card 0 :	FLAG    (*)	.T. for data after ORIGTILTC
C	Card 1 :	ISER1,ISER2 (*)	Serial numbers of output files
C	Card 2 :	IQMAX   (*)	Maximum IQ allowed through
C
C
C
C
C	Program tests first sorts the measurements for each spot into descending
C	order of accuracy, then tests to see whether one measurement of a spot
C	with each index is dominant, before dividing the data into two lists.
C	Finally, the lists are modified to try to improve the balance of 
C	accuracy.
C
      PARAMETER (NS=5000)
      DIMENSION IHIN(NS),IKIN(NS),ZIN(NS),AMPIN(NS),PHASEIN(NS),
     .	IQIN(NS),IOUT(NS),FLMWGT(NS),BACK(NS),CTF(NS)
      LOGICAL FLAG
      INTEGER*8 IFILMIN(NS)
      INTEGER*8 ISER,ISER1,ISER2
      WRITE(6,1)
1     FORMAT(/' DIVIDEQ Vx 1.1 (4-Sep-1999) -',
     .  ' Divide data into two halves with equal weight'/)
      READ(3,*)ISER
      WRITE(6,2)ISER
2 	FORMAT(' SERIAL NUMBER ON MERGED LIST',I10)
      READ(5,*) FLAG
      READ(5,*) ISER1,ISER2
      WRITE(6,3) FLAG,ISER1,ISER2
3     FORMAT(' FLAG  = ',L1/' ISER1 =',I10/' ISER1 =',I10)
      WRITE(1,9)ISER1,ISER
9	FORMAT(I10,' HALF MERGED DATA SET',I10)
      WRITE(2,11)ISER2,ISER
11	FORMAT(I10,' OTHER HALF MERGED DATA SET',I10)
      READ(5,*)IQMAX
      WRITE(6,8)IQMAX
8	FORMAT(' DATA INCLUDED UP TO IQ OF',I5/)
      N2=0
      N3=0
      IEND=0
C
C  READ IN ALL MEASUREMENTS OF SPOTS OF ONE INDEX AT ONCE
C
      I=1
5     IF(FLAG) THEN
      	READ(3,10,END=20)IHIN(I),IKIN(I),ZIN(I),AMPIN(I),PHASEIN(I),
     .		IFILMIN(I),IQIN(I),FLMWGT(I),BACK(I),CTF(I)
      ELSE
      	READ(3,*,END=20)IHIN(I),IKIN(I),ZIN(I),AMPIN(I),PHASEIN(I),
     .		IFILMIN(I),IQIN(I)
      ENDIF
C
10    FORMAT(1X,2I4,F8.4,F10.1,F7.1,I10,I3,F8.5,F10.1,F7.3)
C
      IQQ=IABS(IQIN(I))
      IF(IQQ.GT.IQMAX)GO TO 5
      IF(IHIN(I).EQ.IHIN(1).AND.IKIN(I).EQ.IKIN(1)) THEN
      	I=I+1
      	GO TO 5	! read anoter spot.
      ENDIF
      GO TO 21
20    IEND=1
21    L=I-1	! end of one reflection index.
      DO 23 J=1,L
23    WRITE(6,10)IHIN(J),IKIN(J),ZIN(J),AMPIN(J),PHASEIN(J),
     .	IFILMIN(J),IQIN(J),FLMWGT(I),BACK(I),CTF(I)
      WRITE(6,22)IHIN(1),IKIN(1),L
22    FORMAT(' Reflection',2I5,'  has',I5,'  measurements')
      IF(L.GT.1) THEN
      	CALL SORT(L,IHIN,IKIN,ZIN,AMPIN,PHASEIN,IFILMIN,IQIN,
     .		FLMWGT,BACK,CTF)
      	CALL SUBDIVIDE(L,IHIN,IKIN,IQIN,IOUT)
      	DO 15 J=1,L
      	IF(FLAG) THEN
      	   IF(IOUT(J).EQ.1) THEN
      		WRITE(1,10)IHIN(J),IKIN(J),ZIN(J),AMPIN(J),PHASEIN(J),
     .		IFILMIN(J),IQIN(J),FLMWGT(I),BACK(I),CTF(I)
      		N2=N2+1
      	   ELSE
      		WRITE(2,10)IHIN(J),IKIN(J),ZIN(J),AMPIN(J),PHASEIN(J),
     .		IFILMIN(J),IQIN(J),FLMWGT(I),BACK(I),CTF(I)
      		N3=N3+1
      	   ENDIF
      	ELSE
      	   IF(IOUT(J).EQ.1) THEN
      		WRITE(1,10)IHIN(J),IKIN(J),ZIN(J),AMPIN(J),PHASEIN(J),
     .		IFILMIN(J),IQIN(J)
      		N2=N2+1
      	   ELSE
      		WRITE(2,10)IHIN(J),IKIN(J),ZIN(J),AMPIN(J),PHASEIN(J),
     .		IFILMIN(J),IQIN(J)
      		N3=N3+1
      	   ENDIF
      	ENDIF
15	CONTINUE
      ENDIF
      IF(IEND.EQ.1) GO TO 99
      IHIN(1)   = IHIN(I)
      IKIN(1)   = IKIN(I)
      ZIN(1)    = ZIN(I)
      AMPIN(1)  = AMPIN(I)
      PHASEIN(1)= PHASEIN(I)
      IFILMIN(1)= IFILMIN(I)
      IQIN(1)   = IQIN(I)
      FLMWGT(1)   = FLMWGT(I)
      BACK(1)   = BACK(I)
      CTF(1)   = CTF(I)
      I=2
      GO TO 5
C
99    WRITE(6,98)N2,N3
98    FORMAT(' NUMBER OF DATA POINTS WRITTEN TO EACH DATA SET',
     .	2I10)
      STOP
      END
C******************************************************************************
C  TRY TO DIVIDE UP MULTIPLE MEASUREMENTS OF EACH INDEX TO GIVE ROUGHLY THE SAME
C   ACCURACY OF SPOT DATA IN EACH PILE
      SUBROUTINE SUBDIVIDE(L,IH,IK,IQIN,IOUT)
      DIMENSION IQIN(1),IOUT(1),IH(1),IK(1)
      DIMENSION WEIGHTABLE(8)
      DATA WEIGHTABLE/49.00,27.56,8.51,4.17,2.48,1.65,1.17,0.25/
C  first check that the highest weighted spot is not more than half the power.
      TOTALW=0.0
      	K1=ABS(IQIN(1))
      DO 5 J=1,L
      	K=ABS(IQIN(J))
5     TOTALW=TOTALW+WEIGHTABLE(K)
      IF(WEIGHTABLE(K1).GT.0.5*TOTALW) THEN	! single spot in first pile
      	IOUT(1)=1
      	SCOR1=WEIGHTABLE(K1)
      	NS1=1
      	NS2=0
      	SCOR2=0.0
      	DO 7 J=2,L
      		IOUT(J)=2
      		K=ABS(IQIN(J))
      		NS2=NS2+1
7      		SCOR2=SCOR2+WEIGHTABLE(K)
      	GO TO 50	! exit with printout
      ENDIF
C  If reasonably well distributed, divide into two equal size piles.
      SCOR1=0
      SCOR2=0
      NS1=0
      NS2=0
      DO 10 J=1,L
      K=ABS(IQIN(J))
      IF(J/2*2.NE.J)THEN	! Odd spots
      	IOUT(J)=1
      	SCOR1=SCOR1+WEIGHTABLE(K)
      	NS1=NS1+1
      ELSE			! Even spots
      	IOUT(J)=2
      	SCOR2=SCOR2+WEIGHTABLE(K)
      	NS2=NS2+1
      ENDIF
10    CONTINUE
      IF(ABS(SCOR1-SCOR2).LE.(SCOR1+SCOR2)/4) go to 50	! exit with printout.
C
C  transfer weakest spots from strong half into weak half until balance of score
C   would be other way round.
      ISH=2
      IF(SCOR1.GT.SCOR2) ISH=1
20	CALL FINDWEAKEST(L,IOUT,ISH,M)
      	K=ABS(IQIN(M))
      	IF(WEIGHTABLE(K).LT.ABS(SCOR1-SCOR2)) THEN	! transfer to other pile
      		IOUT(M)=3-ISH	! 2>1, 1>2
      		IND=2*ISH-3	! +1 for shift from 2, -1 for shift from 1.
      		SCOR1=SCOR1+IND*WEIGHTABLE(K)
      		SCOR2=SCOR2-IND*WEIGHTABLE(K)
      		NS1=NS1+IND
      		NS2=NS2-IND
      		GO TO 20	! repeat transfers until optimised.
      	ENDIF
50    WRITE(6,51) IH(1),IK(1),NS1,SCOR1,NS2,SCOR2
51    FORMAT(' Number & weight in piles',2I5,I8,F8.2,I8,F8.2)
      WRITE(6,52)
52    FORMAT(//)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE FINDWEAKEST(L,IOUT,ISH,M)
      DIMENSION IOUT(1)
      DO 10 J=1,L
      	N=L-J+1
      	IF(IOUT(N).NE.ISH) GO TO 10
      	  M=N
      	  RETURN
10    CONTINUE
      WRITE(6,11)
11    FORMAT(' No spots in pile',I5,' at this index')
      STOP
      END
C*******************************************************************************
C  This could be a time consuming sorting step.
      SUBROUTINE SORT(L,IHIN,IKIN,ZIN,AMPIN,PHASEIN,IFILMIN,IQIN,
     .		FLMWGT,BACK,CTF)
      DIMENSION IHIN(1),IKIN(1),ZIN(1),AMPIN(1),PHASEIN(1),
     .		IQIN(1),FLMWGT(1),BACK(1),CTF(1)
      INTEGER*8 IFILMIN(1),IT
      DO 10 J=1,L-1
      	DO 10 J1=J+1,L
      	K=L-J1+J+1
      	IF(ABS(IQIN(K)).GE.ABS(IQIN(J))) GO TO 10	! don't swap
      		IT=IQIN(J)
      		IQIN(J)=IQIN(K)		! swap IQ
      		IQIN(K)=IT
      		  IT=IFILMIN(J)
      		  IFILMIN(J)=IFILMIN(K)	! swap IFILM
      		  IFILMIN(K)=IT
      		ZT=ZIN(J)
      		ZIN(J)=ZIN(K)		! swap Z
      		ZIN(K)=ZT
      		  ZT=AMPIN(J)
      		  AMPIN(J)=AMPIN(K)	! swap AMP
      		  AMPIN(K)=ZT
      		ZT=PHASEIN(J)
      		PHASEIN(J)=PHASEIN(K)	! swap PHASE
      		PHASEIN(K)=ZT
      		  ZT=FLMWGT(J)
      		  FLMWGT(J)=FLMWGT(K)	! swap FLMWGT
      		  FLMWGT(K)=ZT
      		ZT=BACK(J)
      		BACK(J)=BACK(K)		! swap BACK
      		BACK(K)=ZT
      		  ZT=CTF(J)
      		  CTF(J)=CTF(K)		! swap CTF
      		  CTF(K)=ZT
10	CONTINUE
      RETURN
      END
C*******************************************************************************
