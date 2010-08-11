c     HSPOTREFINE.FOR  refines the spot list
C
C     Henning Stahlberg, 29.1.2003
C
      parameter (IMAX=50)
C
      character * 80 cfile1,cfile2,cfile3,czeile
C
      integer ispotlist(0:IMAX,-IMAX:IMAX)
C
      write(*,'('' give name of old spotlist '')')
      read(*,'(A)')cfile1
      call shorten(cfile1,k)
      write(*,'('' read: '',A)')cfile1(1:k)
C
      write(*,'('' give name of APH file '')')
      read(*,'(A)')cfile2
      call shorten(cfile2,k)
      write(*,'('' read: '',A)')cfile2(1:k)
C
      write(*,'('' give maximum IQ value '')')
      read(*,'(I2)')IQmax
      if(IQmax.gt.9)IQmax=9
      if(IQmax.lt.1)IQmax=1
      write(*,'('' read: '',I2)')IQmax
C
      write(*,'('' give maximum HK value '')')
      read(*,'(I4)')IHKmax
      if(IHKmax.gt.IMAX) IHKmax=IMAX
      write(*,'('' read: '',I4)')IHKmax
C
      write(*,'('' give name for new spotlist '')')
      read(*,'(A)')cfile3
      call shorten(cfile3,k)
      write(*,'('' read: '',A)')cfile3(1:k)
C
       
      do ix=0,IHKmax
        do iy=-IHKmax,IHKmax
          ispotlist(ix,iy)=0
        enddo
      enddo
C
      icount = 0
      open(11,FILE=cfile1,ERR=120)
      goto 100
  90  continue
        write(*,'('' ERROR, while reading old spot list'')')
 100  continue
        read(11,*,END=110,ERR=90)ix,iy
        icount = icount + 1
        if(ix.lt.0)then
          ix=-ix
	  iy=-iy
        endif
        ispotlist(ix,iy)=1
      goto 100
 110  continue
      close(11)
 120  continue
      write(*,'('' read '',I6,'' spots from old list.'')')icount
C
      open(12,FILE=cfile2,ERR=900)
      icount1 = 0
      icount2 = 0
      read(12,'(A1)')czeile
      goto 200
 190  continue
        write(*,'('' ERROR, while reading APH file'')')
 200  continue
        read(12,*,END=210,ERR=190)ix,iy,ramp,rpha,IQ
        icount1 = icount1 + 1
        if(ix.lt.0)then
          ix=-ix
          iy=-iy
        endif
        if(IQ.le.IQmax)then
          ispotlist(ix,iy)=1
          icount2 = icount2 + 1
        endif
      goto 200
 210  continue
      close(12)
      write(*,'('' read '',I6,'' new spots from '',
     1     I6,'' lines in APH file.'')')
     1    icount2,icount1
C
      open(13,FILE=cfile3,ERR=900)
      icount = 0
      do ix=0,IHKmax
        do iy=-IHKmax,IHKmax
          if(ispotlist(ix,iy).eq.1)then
            write(13,'(I6,'','',I6)')ix,iy
            icount = icount + 1
          endif
        enddo
      enddo
      close(13)
      write(*,'('' wrote '',I6,'' spots into new list.'')')icount
C
      goto 999
 900  continue
      write(*,'('' hspotrefine: ERROR on file open.'')')
C
 999  continue
C
C      return
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

