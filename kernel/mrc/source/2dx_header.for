C****HEADER*******************************************************
C
C	VX1.00		11-JUN-87		RH
C
C	This program simply writes out the normal header records.
C	If the extra records contain anything it is also printed.	
C
      INTEGER NXYZ(3),MXYZ(3),NXYZST(3),IEXT(29)
      REAL CELL(6)
C
      ISTREAM = 1
C
      CALL IMOPEN(ISTREAM,'IN','READONLY')
      CALL IRDHDR(ISTREAM,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      CALL IRTEXT(ISTREAM,IEXT,1,29)
      CALL IRTCEL(ISTREAM,CELL)
C
C
C  extra header data
        IOUT=0
        DO 100 J=1,29
          IF(IEXT(J).NE.0) IOUT=IOUT+1
100     CONTINUE
        IF(IOUT.NE.0) THEN
          WRITE(6,201)
          DO 200 J=1,29
            IF(IEXT(J).NE.0)WRITE(6,202)J,IEXT(J)
200       CONTINUE
        ENDIF
C
      call system("\rm -f 2dx_header.out")
      open(11,FILE='2dx_header.out',STATUS='NEW')
      write(11,'(''Number of columns, rows, sections ........ '',3I8)') (NXYZ(I),I=1,3)
      write(11,'(''Map mode ................................. '',I8)') MODE
      write(11,'(''Start points on columns, rows, sections .. '',3I8)') (NXYZST(I),I=1,3)
      write(11,'(''Grid sampling on x, y, z ................. '',3I8)')  (MXYZ(I),I=1,3)
      write(11,'(''Cell axes ................................ '',3F12.3)') (CELL(I),I=1,3)
      write(11,'(''Cell angles .............................. '',3F12.3)') (CELL(I),I=4,6)
      write(11,'(''Fast, medium, slow axes .................. '')')
      write(11,'(''Minimum density .......................... '',F16.9)') DMIN
      write(11,'(''Maximum density .......................... '',F16.9)') DMAX
      write(11,'(''Mean density ............................. '',F16.9)') DMEAN

      close(11)
C
      CALL  IMCLOSE(ISTREAM)
201   FORMAT(' Extra information in header'/'    N    contents')
202   FORMAT(I5,I10)
      END
