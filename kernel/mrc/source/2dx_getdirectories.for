      PROGRAM GETDIR
C
C-----This program is supposed to receive the list of image directories (including 
C-----absolute pathnames), and also the path to the current merge directory, and
C-----write-out the list of tilt-angle directories. 
C
C-----This is then to be used to correct the links of the 2dx_master.cfg files.
C
C-----created: August 18, 2011, by Henning Stahlberg. 2dx.org
C
      PARAMETER (LISTMAX = 10000)
C
      character*200 CDIRLIST,CMERGEDIR,CDIR,COUTFILE
      character*200 CLIST(LISTMAX)
      logical DEBUG
C
      DEBUG = .false.
C
      write(*,'(/,/,''2dx_getdirectories.exe: Program to extract tilt-range directories from paths'')')
      write(*,'(/,''input filename with directories list'')')
      read(*,'(A)')CDIRLIST
      call shorten(CDIRLIST,k)
      write(*,'(''Read: '',A)')CDIRLIST(1:k)
C
      open(10,FILE=CDIRLIST,STATUS='OLD',ERR=990)
C
      write(*,'(/,''input file and path for local merge directory'')')
      read(*,'(A)')CMERGEDIR
      call shorten(CMERGEDIR,k)
      write(*,'(''Read: '',A)')CMERGEDIR(1:k)
C
      write(*,'(/,''input file for output list'')')
      read(*,'(A)')COUTFILE
      call shorten(COUTFILE,k)
      write(*,'(''Read: '',A)')COUTFILE(1:k)
C
      idircount = 0
C
C-----Read all tilt-range directories
C
 100  continue
C
        read(10,'(A)',END=200)CDIR
        call shorten(CDIR,k)
        if (DEBUG) write(*,'(''Got '',A)')CDIR(1:k)
        call shortslash(CDIR,l)
        if (DEBUG) write(*,'(''Path is '',A)')CDIR(1:l)
C
        if(idircount.eq.0)then
          idircount=1
          write(CLIST(idircount),'(A)')CDIR(1:l)
        else
          iisthere=0
C---------Make sure any "tilt-range" directory is not the project directory:
          if(CMERGEDIR(1:l).eq.CDIR(1:l))then
            iisthere=1
          endif
C---------Make sure any "tilt-range" directory doesn't appear twice:
          do i=1,idircount
            if(CLIST(i)(1:l).eq.CDIR(1:l))then
              iisthere=1
            endif
          enddo
C---------If not, then store it:
          if(iisthere.eq.0)then
            idircount=idircount+1
            write(CLIST(idircount),'(A)')CDIR(1:l)
          endif
        endif
      goto 100
C
 200  continue
C
C-----Now we got idircount tilt-range directories
C
      write(*,'(/,'':Found '',I4,'' tilt-range directories'')')idircount
C
C-----Write them out:
C
      open(11,FILE=COUTFILE,STATUS='NEW',ERR=991)
C
      do i=1,idircount
        call shorten(CLIST(i),k)
        write(*,'('' Dir: '',A)')CLIST(i)(1:k)
        write(11,'(A)')CLIST(i)(1:k)
      enddo
C
      goto 999
C
 990  continue
      write(*,'(''::ERROR on file open in 2dx_getdirectories (flag 990)'')')
      goto 999
C
 991  continue
      write(*,'(''::ERROR on file open in 2dx_getdirectories (flag 991)'')')
      goto 999
C
 999  continue
      close(10)
      close(11)
      STOP
      END
C
c==========================================================
c
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
C
c==========================================================
c
      SUBROUTINE SHORTSLASH(czeile,k)
C
C counts the number of characters until the last '/' in czeile
C and gives the result out in k.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1
      CHARACTER * 1 CTMP2
      CHARACTER * 200 CZEIL2
      CTMP2='/'
C
      ilen=len(czeile)
      k=ilen
C
      DO 100 I=1,ilen
         k=ilen+1-I
         READ(czeile(k:k),'(A1)')CTMP1
         IF(CTMP1.EQ.CTMP2)GOTO 300
  100 CONTINUE
  300 CONTINUE
      IF(k.LT.1)k=1
C
      RETURN
      END
C


