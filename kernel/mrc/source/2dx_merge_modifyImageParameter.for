       PROGRAM MODIFIMPA
C
C (C) 2dx.org, GNU Plublic License.
C                                   
C Created..........: 01/03/2007      
C Last Modification: 01/03/2007       
C Author...........: 2dx.org           
C
      character*200 cname1,cname3,cdir,ctmpdir
      character*80 cphaori,cphaoriFouFilter,cbeamtilt
      character*200 CFILE1
      integer*8 imnum(10000)
C
      write(*,'('':2dx_merge_modifyImageParameter - '',
     .    ''Update the Image Parameters'')')
C
      write(*,'(/,''input name of results output file'')')
      read(*,'(A)')CFILE1
      call shorten(CFILE1,k)
      write(*,'(A)')CFILE1(1:k)
C
      write(*,'(/,''input name of file with directory info'')')
      read(*,'(A)')cname1
      call shorten(cname1,k)
      write(*,'(A)')cname1(1:k)
C
      write(*,'(/,''input Additional Phase Shift for phaori'')')
      read(*,*)phaorix,phaoriy
      write(*,'(2F10.3)')phaorix,phaoriy
C
      write(*,'(/,''input Additional Phase Shift for'',
     .    '' phaoriFouFilter'')')
      read(*,*)phaoriFouFilterx,phaoriFouFiltery
      write(*,'(2F10.3)')phaoriFouFilterx,phaoriFouFiltery
C
      write(*,'(/,''input Additional Beam Tilt'')')
      read(*,*)bmtiltx,bmtilty
      write(*,'(2F10.3)')bmtiltx,bmtilty
C
      open(10,FILE=cname1,STATUS='OLD',ERR=900)
C
      open(11,FILE=CFILE1,STATUS='UNKNOWN',ERR=905)
C
      imcount = 0
C
 100  continue
C
        read(10,'(A)',END=800)ctmpdir
        call shorten(ctmpdir,k)
        if(ctmpdir(1:1).eq.'/')then
          write(cdir,'(A)')ctmpdir(1:k)
        else
          write(cdir,'(''../'',A)')ctmpdir(1:k)
        endif
        call shorten(cdir,k)
        write(cname3,'(A,''/2dx_image.cfg'')')cdir(1:k)
        write(*,'(/,''opening '',A)')cname3
        open(12,FILE=cname3,STATUS='OLD',ERR=200)
        goto 210
 200    continue
          call shorten(cname3,k)
          write(*,'(''::WARNING: Could not open '',A)')cname3(1:k)
          goto 100
 210    continue
C
        imcount=imcount+1
C
        call shorten(cdir,k)
        write(11,'(''<IMAGEDIR="'',A,''">'')')cdir(1:k)
        write(6,'('':Working on '',A)')cdir(1:k)
C
        call cgetline(cphaori,"phaori")
        read(cphaori,*,ERR=300)rpx,rpy
        rpx=rpx+phaorix
        rpy=rpy+phaoriy
        write(cphaori,'(F12.3,'','',F12.3)')rpx,rpy
        if(abs(phaorix)+abs(phaoriy).gt.0.0001)then
          call inkomma(cphaori,k)
          write(11,'(''set phaori = "'',A,''"'')')cphaori(1:k)
        endif
        goto 310
 300    continue
          call shorten(cname3,k)
          write(*,'(''::WARNING: Problem getting phaori from ''
     .         ,A)')cname3(1:k)
 310    continue
C
        call cgetline(cphaoriFouFilter,"phaoriFouFilter")
        read(cphaoriFouFilter,*,ERR=400)rpFx,rpFy
        rpFx=rpFx+phaoriFouFilterx
        rpFy=rpFy+phaoriFouFiltery
        write(cphaoriFouFilter,'(F12.3,'','',F12.3)')rpFx,rpFy
        if(abs(phaoriFouFilterx)+abs(phaoriFouFiltery).gt.0.0001)then
          call inkomma(cphaoriFouFilter,k)
          write(11,'(''set phaoriFouFilter = "'',A,''"'')')
     .      cphaoriFouFilter(1:k)
        endif
        goto 410
 400    continue
          call shorten(cname3,k)
          write(*,'(''::WARNING: Problem getting phaoriFouFilter from ''
     .         ,A)')cname3(1:k)
 410    continue
C
C
        call cgetline(cbeamtilt,"beamtilt")
        read(cbeamtilt,*,ERR=500)rtx,rty
        rtx=rtx+bmtiltx
        rty=rty+bmtilty
        write(cbeamtilt,'(F12.3,'','',F12.3)')rtx,rty
        if(abs(bmtiltx)+abs(bmtilty).gt.0.0001)then
          call inkomma(cbeamtilt,k)
          write(11,'(''set beamtilt = "'',A,''"'')')
     .      cbeamtilt(1:k)
        endif
        goto 510
 500    continue
          call shorten(cname3,k)
          write(*,'(''::WARNING: Problem getting beamtilt from ''
     .         ,A)')cname3(1:k)
 510    continue
C
        write(11,'(''</IMAGEDIR>'')')
        write(11,'(''#'')')
C
        close(12)
C
        goto 100
C
 800  continue
C
      close(10)
      close(11)
C
      goto 999
C
 900  continue
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::ERROR on file open in '',
     .    ''2dx_merge_modifyImageParameter'')')
        call shorten(cname1,k)
        write(*,'(''::for file '',A)')cname1(1:k)
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        stop
C
 905  continue
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::ERROR on file open in '',
     .    ''2dx_merge_modifyImageParameter'')')
        call shorten(CFILE1,k)
        write(*,'(''::for file '',A)')CFILE1(1:k)
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        stop
C
 910  continue
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::ERROR on directory file open '',
     .     ''in 2dx_merge_modifyImageParameter'')')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
        write(*,'(''::'',79(''#''))')
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
      SUBROUTINE igetline(ival,cname)
C
      CHARACTER * (*) cname
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
      write(*,'(''value for '',A,'' is '',A)')cname(1:k),cline(9+k:l-1)
      if(cline(9+k:9+k).eq."n")then
        ival=0
        goto 999
      endif
      if(cline(9+k:9+k).eq."y")then
        ival=1
        goto 999
      endif
      read(cline(9+k:l-1),*,ERR=800)ival
C
      goto 999
C
 800  continue
        write(*,'(''::WARNING: no value vor '',A,
     .    '', setting to zero.'')')cname(1:k)
        ival=0
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
c==========================================================
c
      SUBROUTINE rgetline(rval,cname)
C
      CHARACTER * (*) cname
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
      write(*,'(''value for '',A,'' is '',A)')cname(1:k),cline(9+k:l-1)
      read(cline(9+k:l-1),*,ERR=800)rval
C
      goto 999
C
 800  continue
        write(*,'(''::WARNING: no value vor '',A,
     .    '', setting to zero.'')')cname(1:k)
        rval=0.0
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
c==========================================================
c
      SUBROUTINE INKOMMA(CZEILE,k)
C
C replaces intermedieate spaces within the actual text string
C in CZEILE up to the length k by komma.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1,CTMP2,CTMP3
      CHARACTER * 200 CZEIL2
      CTMP2=' '
      CTMP3=','
C
      ilen=len(czeile)
      DO 70 I=1,ilen
         k=ilen+1-I
         READ(CZEILE(k:k),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)GOTO 80
  70  CONTINUE
  80  CONTINUE
      IF(k.LT.1)k=1
C
C-----find the leading spaces and remove them
C
      DO 90 J=1,k
         READ(CZEILE(J:J),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)THEN
           GOTO 95
         ENDIF
 90   CONTINUE
 95   CONTINUE
C
      WRITE(CZEIL2(1:k),'(A)') CZEILE(J:k)
C
C     WRITE(*,'('' INKOMMA 2: k='',I4,'' J='',I4,'':'',A)')
C    1      k,j,CZEIL2(1:k)
C
C-----Was there a komma recently ?
      KWAR=0
C
      I=1
      L=1
 100  continue
        READ(CZEIL2(I:I),'(A1)')CTMP1
        IF(CTMP1.EQ.CTMP3)THEN
C---------There is a komma.
          IF(KWAR.EQ.0)THEN
C-----------There is a komma, before was no komma. Insert one.
            WRITE(CZEILE(L:L),'('','')')
            KWAR=1
          ELSE
C-----------There is a komma, before was already a komma. Shrink.
            L=L-1
          ENDIF
        ELSEIF(CTMP1.EQ.CTMP2)THEN
C---------There is a space.
          IF(KWAR.EQ.0)THEN
C-----------There is a space, before was no komma. Insert komma.
            WRITE(CZEILE(L:L),'('','')')
            KWAR=1
          ELSE
C-----------There is a space, before was a komma. Shrink.
            L=L-1
          ENDIF
        ELSE
C---------There is no komma, no space. Anulate KWAR, do nothing.
          WRITE(CZEILE(L:L),'(A1)')CZEIL2(I:I)
          KWAR=0
        ENDIF
        I=I+1
        L=L+1
C       WRITE(*,'('' INKOMMA 2b: I='',I4,'' L='',I3,'':'',A)')I,L,CTMP1
C
      IF(I.LE.K)GOTO 100
C
C     WRITE(*,'('' INKOMMA 3: k='',I4,'':'',A)')k,CZEILE(1:k)
C
      IF(L.LE.K)THEN
        WRITE(CZEILE(L:K),'('' '')')
      ENDIF
C
C-----Now check, if the last sign is a komma. If so, remove it.
C
      K=L
      I=K
 200  continue
        READ(CZEILE(I:I),'(A1)')CTMP1
        IF(CTMP1.NE.CTMP2)GOTO 220
        I=I-1
      IF(I.GE.1)GOTO 200
 220  CONTINUE
C
      K=I
C
      IF(CTMP1.EQ.CTMP3)then
        WRITE(CZEILE(I:I),'('' '')')
        K=K-1
      endif
C
      if(k.lt.1)k=1
C
C      WRITE(*,'('':: INKOMMA 4: k='',I4,'': "'',A,''"'')')k,CZEILE(1:k)
C
      RETURN
      END
C
c==========================================================





