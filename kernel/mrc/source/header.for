C****HEADER*******************************************************
C
C	VX1.00		11-JUN-87		RH
C
C	This program simply writes out the normal header records.
C	If the extra records contain anything it is also printed.	
C
      INTEGER NXYZ(3),MXYZ(3),IEXT(29)
      ISTREAM = 1
C***      CALL  IMOPEN(ISTREAM,'IN','OLD')
      write(6,'(''Here 1, ISTREAM = '',I8)')ISTREAM
      CALL  IMOPEN(ISTREAM,'IN','READONLY')
      write(6,'(''Here 2'')')
      CALL  IRDHDR(ISTREAM,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      write(6,'(''Here 3'')')
      CALL  IRTEXT(ISTREAM,IEXT,1,29)
      write(6,'(''Here 4'')')
C
C
C  extra header data
	IOUT=0
	DO 100 J=1,29
	IF(IEXT(J).NE.0) IOUT=IOUT+1
100	CONTINUE
	IF(IOUT.NE.0) THEN
		WRITE(6,201)
		DO 200 J=1,29
		IF(IEXT(J).NE.0)WRITE(6,202)J,IEXT(J)
200		CONTINUE
	ENDIF
C
C
      CALL  IMCLOSE(ISTREAM)
201   FORMAT(' Extra information in header'/'    N    contents')
202   FORMAT(I5,I10)
      END
