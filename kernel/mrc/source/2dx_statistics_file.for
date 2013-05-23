      PROGRAM STATISFIL
C
C-----Program to count the number of images in each tilt angle category
C
      CHARACTER * 200 cfile,cname,clastname,cspace
      character * 1 CTMP,CTMP1
c
      CTMP1 = '/'
      clastname = ' '
      ilastlen = 0
      inewrun = 0
C
      write(cspace(1:200),'(200('' ''))')
C
      ilast=1
 100  continue
        read(*,'(A)',err=800,end=700) cfile
C
        i=0
        j=len(cfile)
 200    continue
          i=i+1
          if(i.ge.j)goto 250
          read(cfile(i:i),'(A1)')CTMP
          if(CTMP.EQ.CTMP1)goto 250
          goto 200
 250    continue
C
C-------Now i points to the backslash or the end of the string
C
C-------Fill the remainder with blanks:
        write(cfile(i:200),'(A)')cspace(i:200)
        i=i-1
        if(ilastlen.eq.0)ilastlen=i-1
C
C-------Check if that is the same as before:
        if(i.lt.1)i=1
        read(cfile(1:i),'(A)')cname
        n=1
        do k=1,i
          if(clastname(k:k).ne.cname(k:k))n=0
        enddo
C
        if(n.eq.1)then
C---------If same as before: count.
          ilast=ilast+1
        else
C---------If new, then output last one and restart counters:
          if(inewrun.eq.0)then
            inewrun=1
          else
            write(*,'(''Tiltangle '',A,'': '',i5,'' images'')')clastname(1:ilastlen),ilast
          endif
          ilast=1
        endif
        write(clastname,'(A)')cname(1:200)
        ilastlen=i
C
      goto 100
C
 700  continue
C
C-----Output last tiltanglerange:
C
      write(*,'(''Tiltangle '',A,'': '',i5,'' images'')')clastname(1:ilastlen),ilast
C
      goto 900
C
 800  continue
         write(*,'('' ======== ERROR OCCURRED in '',
     .    ''2dx_statistics_file.for ========'')')
c
 900  continue
C
      END
             
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


