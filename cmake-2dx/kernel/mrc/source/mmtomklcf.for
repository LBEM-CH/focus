C  MMTOMKLCF
C  Jiffy program to create MKLCF file from MMBOX or CTFAPPLY output.
C
C	VX 1.00		RH	4-SEP-87
C	VX 1.01		RH	1-NOV-88
C	VX 1.02		RH	27-JUL-92	for H-positive
C	VX 1.03		RH	31-AUG-92	debug K-negative with H=zero
C
      DIMENSION TITLE(20),WGT(9),NIQ(9)
      DATA WGT/1.0,1.0,0.9,0.5,0.1,0.0,0.0,0.0,0.0/
      DATA NIQ/9*0/
      WRITE(6,3)
3     FORMAT('  MMTOMKLCF - Creates MKLCF file from MMBOX or'
     .	' CTFAPPLY format,  VX 1.03(31-Aug-1992)')
      IZERO=0
      N=0
      READ(5,*)ACELL,BCELL,RESOL
      WRITE(6,4)ACELL,BCELL,RESOL
4     FORMAT(' Cell dimensions and resolution applied',3F7.1)
      	ACELLSQ=ACELL**2
      	BCELLSQ=BCELL**2
      	RESOLSQ=RESOL**2
      READ(1,1)TITLE
      WRITE(2,1)TITLE
10    READ(1,*,END=100)IH,IK,AMP,PHASE,IQ,RMSBK
      IF(AMP.NE.0.0)THEN
      	AMP = AMP*WGT(IQ)
C
C Convention to be H > 0 in the P1 asymmetric unit for FFT program.
C
      	IF(IH.LT.0) THEN
      		IH=-IH
      		IK=-IK
      		PHASE=-PHASE
      	ENDIF
C
C If H=0 then K > 0
C
      	IF(IH.EQ.0) THEN
      		IF(IK.LT.0) THEN
      			IH=-IH
      			IK=-IK
      			PHASE=-PHASE
      	ENDIF
      	ENDIF
      	IF(AMP.GT.0.0) THEN
      	   IF(IH**2/ACELLSQ+IK**2/BCELLSQ.LT.1.0/RESOLSQ) THEN
      		WRITE(2,2)IH,IK,IZERO,AMP,PHASE
      		NIQ(IQ)=NIQ(IQ)+1
      		N=N+1
      	   ENDIF
      	ENDIF
      ENDIF
      GO TO 10
100   WRITE(6,101)N
      WRITE(6,102)(I,NIQ(I),I=1,9)
1     FORMAT(20A4)
2     FORMAT(3I5,2F10.2)
101   FORMAT(I5,' TOTAL SPOTS, END OF INPUT FILE SUCCESSFULLY REACHED')
102   FORMAT(' DISTRIBUTION'/9(2I5/))
      END
