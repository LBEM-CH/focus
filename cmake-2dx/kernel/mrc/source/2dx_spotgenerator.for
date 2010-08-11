      PROGRAM spotgen
C
      character*200 cname1
      write(*,'('' Input name for output file'')')
      read(*,*,ERR=800) cname1
      call shorten(cname1,k)
      write(*,'(''read: '',A)')cname1(1:k)
C
      write(*,'('' Input max hk index'')')
      read(*,*,ERR=800) ihkmax
      if(ihkmax.gt.50)ihkmax=50
      write(*,'(''read: '',I8)')ihkmax
C
      open(9,FILE=cname1,STATUS='NEW',ERR=810)
C
      do i = -ihkmax,ihkmax
        do j = -ihkmax,ihkmax
          write(9,'(I4,'','',I4)')i,j
        enddo
      enddo
C
      close(9)
C
      goto 999
C
 800  continue
      STOP 'ERROR on reading of name'
 810  continue
      STOP 'ERROR on file open'
C
 999  continue
      STOP
      END
C
c==========================================================
c
      SUBROUTINE SHORTEN(czeile,k)
C
C counts the number of actual characters not ' ' in czeile
C and gives the result out in k.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1,CTMP2
      CTMP2=' '
      ilen=len(czeile)
      DO 100 I=1,ilen
         k=ilen-I
         READ(CZEILE(k:k),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)GOTO 300
  100 CONTINUE
  300 CONTINUE
      IF(k.LT.1)k=1
C
      RETURN
      END
C

