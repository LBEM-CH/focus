      PROGRAM GETLINE
C
C-----Program to extract line number n from a file
C-----Useful to replace tail and head commands.
C
      character * 200 czeile
C
      read(*,'(A)') czeile
      read(*,*) inum
C 
      open(1,FILE=czeile,STATUS='OLD')
C
      do i=1,inum
        read(1,'(A)')czeile
      enddo
C
      call shorten(czeile,k)
C
      write(6,'(A)')czeile(1:k)
C
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
C
      ilen=len(czeile)
C     WRITE(*,'('' INKOMMA 2: k='',I4,'' J='',I4,'':'',A)')
      DO 100 I=1,ilen
         k=ilen+1-I
         READ(CZEILE(k:k),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)GOTO 300
  100 CONTINUE
  300 CONTINUE
      IF(k.LT.1)k=1
C
      RETURN
      END


