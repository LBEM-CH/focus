      PROGRAM PHASEZERO
C
C-----Program to zero all phases, for the calculation of a PSF.
C
C-----2dx.org, Aug. 2011, Henning Stahlberg
C
      integer H,K,L
      character*50 TITLE
      character*200 CINFILE,COUT1,COUT2
C
      write(*,'('' '')')
      write(*,'('': '')')
      write(*,'('': 2dx_phasezero.exe - to zero phases for PSF calculation'')')
      write(*,'('': '')')
C
      WRITE(*,'(/,'' Input Input File Name of APH file'')')
      READ(*,'(A)')CINFILE
      call shorten(CINFILE,k)
      WRITE(*,'('' Read: '',A)')CINFILE(1:k)
C
      WRITE(*,'(/,'' Input Ouput File Name of APH file'')')
      READ(*,'(A)')COUT1
      call shorten(COUT1,k)
      WRITE(*,'('' Read: '',A)')COUT1(1:k)
C
      write(*,'(/,'' With SIGA column? (1=yes,0=no)'')')
      read(*,*)isiga
      if(isiga.gt.1)isiga=1
      if(isiga.lt.0)isiga=0
      write(*,'('' Read: '',I1)')isiga
C
      write(*,'('' '')')
C
C-----Open input and output files
C
      open(10,FILE=CINFILE,STATUS='OLD',ERR=900)
      open(11,FILE=COUT1,STATUS='NEW',ERR=900)
C
C-----Initiate output files' titles
C
C      READ (10,100) TITLE
C      WRITE (11,100) TITLE
C 100  FORMAT (A50)
C
C-----Read input file
C
1000  continue
        if(isiga.eq.1)then
          READ (10,*,END=1010) H,K,L,AMP,PHASE,FOM,SIGA
          WRITE (11,200) H,K,L,AMP,FOM,SIGA
 200      FORMAT (3I6,G16.6,' 0.0 ',2G16.6)
        else
          READ (10,*,END=1010) H,K,L,AMP,PHASE,FOM
          WRITE (11,210) H,K,L,AMP,FOM
 210      FORMAT (3I6,G16.6,' 0.0 ',G16.6)
        endif
C
      goto 1000
C
1010  continue
      goto 999
C
 900  continue
      write(*,'(''::ERROR in 2dx_phasezero on file open'')')
      goto 999
C
 999  continue
C
      close(10)
      close(11)
C
      stop
      end
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


