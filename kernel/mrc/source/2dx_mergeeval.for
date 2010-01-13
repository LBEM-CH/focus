      PROGRAM MERGEEVAL
C
      character * 100 czeile
c
      rmaxpha=100000000.0
C
 100  continue
         read(*,'(A)',end=800) czeile
C        write(*,'('' Input was '',A)') czeile(1:20)
         read(czeile,*,err=200) iperm,irot,irev,irot90,phares
         if (phares.LT.rmaxpha) then
            imaxper=iperm
            imaxrot=irot
            imaxrev=irev
            imaxrot90=irot90
            rmaxpha=phares
         endif
      goto 100
 200  continue
         write(*,'('':: ===== ERROR OCCURRED in mergeeval ========'')') 
      goto 100
c
 800  continue
c
C     write(*,'(4I3,'' '',F12.3,'' was maximal value.'')') 
C    1    imaxper,imaxrot,imaxrev,imaxrot90,rmaxpha,
      write(czeile,'(4I3,'' '',F12.3)')
     1    imaxper,imaxrot,imaxrev,imaxrot90,rmaxpha
      open(11,FILE='TMP442211.tmp',STATUS='NEW',ERR=900)
      write(11,'(A60)') czeile
      close(11)
      goto 999
C
 900  continue
      stop "ERROR in mergeeval"
 999  continue
C
      END
             
