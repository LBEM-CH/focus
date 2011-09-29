c     2dx_permutate_file.FOR permutates HKL into KLH
C
C     Henning Stahlberg, Sept. 2011
C
      PROGRAM PERMUF
C
      IMPLICIT NONE
C 
      character*200 czeile,cfile1,cfile2
      integer H,K,L,iheader
      real RF,RP,RFOM
C
      write(*,'('' 2dx_permutate_file: Give input file name'')')
      read(*,'(A190)')cfile1
      call shorten(cfile1,k)
      write(*,'('' read:'',A)')cfile1(1:k)
C
      write(*,'('' 2dx_permutate_file: Give output file name'')')
      read(*,'(A190)')cfile2
      call shorten(cfile2,k)
      write(*,'('' read:'',A)')cfile2(1:k)
C
      write(*,'('' 2dx_permutate_file: Head line ? (1=y,0=n) '')')
      read(*,*)iheader
      write(*,'('' read:'',I3)')iheader
C
      open(11,FILE=cfile1,status='old',ERR=950)
      open(12,FILE=cfile2,status='new',ERR=950)
      
C-----header line:
C  
      if ( iheader.eq.1 ) then
        READ(11,'(A180)')czeile
        call shorten(czeile,k)
        WRITE(12,'(A)')czeile(1:k)
      endif
C
 100  continue
        read(11,*,END=800,ERR=900)H,K,L,RF,RP,RFOM
        write(12,2000)K,L,H,RF,RP,RFOM
 2000   FORMAT(3I6,F12.1,F12.1,F12.3)
      goto 100
C
 800  continue
      close(11)
      close(12)
C
      write(*,'('':2dx_permutate_file: permutated '',I7,'' reflections.'')')
C
      goto 999
C
 900  continue
      write(*,'(''::2dx_permutate_file: ERROR on reading'')')
      stop
C
 950  continue
      write(*,'(''::2dx_permutate_file: ERROR on file opening'')')
      stop
C
 999  continue
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
