      PROGRAM GETMAX
C
      character * 100 czeile
c
      rmaxout = -99999999.99
      imaxin = 2
C
 100  continue
         read(*,'(A)',end=800) czeile
C        write(*,'('' Input was '',A)') czeile(1:20)
         read(czeile,*,err=200) inval,routval
C        write(*,'('' Input was '',I6,F16.3)') inval,routval
         if (routval.GE.rmaxout) then
            rmaxout = routval
            imaxin = inval
         endif
      goto 100
 200  continue
         write(*,'('' ======== ERROR OCCURRED in GETMAX ========'')') 
      goto 100
c
 800  continue
c
C     write(*,'(I6,' ',F12.3,'' was maximal values.'')') imaxin,rmaxout
      write(czeile,'(I5)') imaxin
      write(*,'(A60)') czeile
      open(11,FILE='TMP442211.tmp',STATUS='NEW',ERR=900)
      write(11,'(F12.4)') rmaxout
      close(11)
      goto 999
C
 900  continue
      stop "ERROR in getmax"
 999  continue
C
      stop
      END
             
