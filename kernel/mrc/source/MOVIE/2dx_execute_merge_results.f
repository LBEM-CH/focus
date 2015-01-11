      PROGRAM EXRESUL
C
      IMPLICIT NONE
C
      character*200 CFILE1,CFILE2,cline,cline2
      character*200 cdir,cfgfile,cfile
      INTEGER       i,k,k1,k2
C
      write(6,'(''Input results file name'')')
      read(*,'(A)')CFILE1
      call shorten(CFILE1,k)
      write(*,'(''Read: '',A)')CFILE1(1:k)
C
      write(6,'(''Input output script file name'')')
      read(*,'(A)')CFILE2
      call shorten(CFILE2,k)
      write(*,'(''Read: '',A)')CFILE2(1:k)
C
      write(cline,'(''\rm -f '',A)')CFILE2(1:k)
      call shorten(cline,k2)
      call system(cline(1:k2))
C
      open(11,FILE=CFILE1,STATUS='OLD',ERR=900)
      open(12,FILE=CFILE2,STATUS='NEW',ERR=900)
C
 100  continue
C
        read(11,'(A)',END=200)cline
        call shorten(cline,k)
        if(cline(1:1).eq.'#')goto 100
        if(cline(2:2).eq.'I')then
          write(cdir,'(A)')cline(12:k-2)
          write(cfgfile,'(A,''/2dx_image.cfg'')')cline(12:k-2)
          call shorten(cfgfile,k2)
          write(*,'(''Working on '',A)')cdir(1:k2)
          write(12,'(''cat >> '',A,'' << eot'')')cfgfile(1:k2)
          goto 100
        endif
        if(cline(1:1).eq.'s')then
          call shorten(cline,k)
          write(12,'(A)')cline(1:k)
          goto 100
        endif
        if(cline(2:2).eq.'/')then
          write(12,'(''eot'')')
          call shorten(cdir,k2)
          write(12,'(''${app_2dx_image} '',A,'' "2dx_initialize"'')')
     .      cdir(1:k2)
          write(12,'(''#'')')
          goto 100
        endif
 900  continue
        write(*,'(''::ERROR'')')
 200  continue
C
      close(11)
      close(12)
C
      STOP
      END
C
C==========================================================
C
      SUBROUTINE shorten(czeile,k)
C
C counts the number of actual characters not ' ' in czeile
C and gives the result out in k.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1
      CHARACTER * 1 CTMP2
      CTMP2=' '
C
      ilen=len(czeile)
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

