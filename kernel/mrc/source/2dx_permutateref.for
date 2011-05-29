c     permutateref.FOR permutates HKL into KLH
C
C     Henning Stahlberg, May 2011
C
      PROGRAM PERMUTREF
C
      IMPLICIT NONE
C 
      character*80 czeile
      integer H,K,L
      real RF,RP,RFOM,SIGA
C
C-----No header line:
C      READ(*,'(A)')czeile
C      WRITE(*,'(A)')czeile
C
 100  continue
        read(*,*,END=900,ERR=900)H,K,L,RF,RP,RFOM,SIGA
        write(*,2000)K,L,H,RF,RP,RFOM,SIGA
 2000   FORMAT(3I6,F12.1,F12.1,F12.3,F12.3)
      goto 100
C
      goto 999
C
 900  continue
C
 999  continue
C
      STOP
      END
C

