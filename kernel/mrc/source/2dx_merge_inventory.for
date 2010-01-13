       PROGRAM INVENTORY
C
C (C) 2dx.org, GNU Plublic License.
C                                   
C Created..........: 01/03/2007      
C Last Modification: 01/03/2007       
C Author...........: 2dx.org           
C
      character*200 cname1,cname2,cname3,cname4,cdir,cprocdir,cbindir
      character CROT90,CROT180
      character*80 cspcgrp,crealcell,CBMTLT,CPHORI,CIMAGENAME,CTITLE
      character*80 CIMAGENUMBER,CLATTICE
      character*200 CFILE1,CFILE2,cline
C
      write(*,'(''::2dx_merge_inventory - '',
     .    ''compiling a list of final maps'')')
C
      write(*,'(/,''input name of results output file'')')
      read(*,'(A)')CFILE1
C
      write(*,'(/,''input name of file with directory info'')')
      read(*,'(A)')cname1
      call shorten(cname1,k)
      write(*,'(A)')cname1(1:k)
C
      open(10,FILE=cname1,STATUS='OLD',ERR=900)
C
      open(11,FILE=CFILE1,STATUS='UNKNOWN',ERR=900)
C
 100  continue
C
        read(10,'(A)',END=200)cdir
        call shorten(cdir,k)
        write(cname3,'(A,''/2dx_image.cfg'')')cdir(1:k)
        write(*,'(/,'':opening '',A)')cname3
        open(12,FILE=cname3,STATUS='OLD',ERR=900)
        call cgetline(CIMAGENAME,"imagename")
        close(12)
C
        call shorten(cdir,k1)
        call shortshrink(CIMAGENAME,k2)
        write(cname4,'(''# IMAGE-IMPORTANT: '',A,''/'',A,''-p1.mrc <'',A,''-p1.mrc>'')')
     .    cdir(1:k1),CIMAGENAME(1:k2),CIMAGENAME(1:k2)
        call shortshrink(cname4,k1)
        write(11,'(A)')cname4(1:k1)
C
        call shorten(cdir,k1)
        call shortshrink(CIMAGENAME,k2)
        write(cname4,'(A,''/'',A,''-p1.mrc'')')
     .    cdir(1:k1),CIMAGENAME(1:k2)
        call shortshrink(cname4,k1)
        write(*,'(''::  Map is '',A)')cname4(1:k1)
C
        goto 100
C
 200  continue
C
      close(10)
      close(11)
C
      goto 999
C
 900  continue
        write(*,'(''::ERROR on file open in 2dx_merge_inventory'')')
        stop
C
 999  continue
      stop
      end

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
      SUBROUTINE SHORTSHRINK(czeile,k)
C
C counts the number of actual characters not ' ' in czeile
C and gives the result out in k.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1
      CHARACTER * 1 CTMP2
      CHARACTER * 200 CZEIL2
      CTMP2=' '
C
C-----find the leading spaces and remove them
C
      ilen=len(czeile)
      k=ilen
C
      DO 90 J=1,k
         READ(CZEILE(J:J),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)THEN
           GOTO 95
         ENDIF
 90   CONTINUE
 95   CONTINUE
C
      WRITE(CZEIL2(1:k),'(A)')CZEILE(J:k)
      WRITE(CZEILE(1:k),'(A)')CZEIL2(1:k)
C
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
      SUBROUTINE cgetline(cval,cname)
C
      CHARACTER * (*) cname,cval
      character*200 cline
C
      call shorten(cname,k)
C
      rewind(12)
C
 100  continue
C 
        read(12,'(A)',END=900,ERR=900)cline
        if(cline(1:3).ne."set") goto 100
        if(cline(5:4+k).ne.cname(1:k)) goto 100
C
      call shorten(cline,l)
      ilen=l-k-9
      do i=1,ilen
        cval(i:i) = cline(8+i+k:8+i+k)
      enddo
      n=len(cval)
      write(cval(ilen:n),'(A)')cline(l-1:l-1)
      call shorten(cval,i)
      write(*,'(''value for '',A,'' is '',A)')cname(1:k),cval(1:i)
C
      goto 999
C
 900  continue
        write(*,'(''::ERROR on value read:'',A30)')cname
        stop
C
 999  continue
      RETURN
      END
C




