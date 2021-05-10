      PROGRAM mergeeval
C
      character*80 cfile1,cfile2,cfile3,cfile4
      character*80 czeile1,czeile2,czeile3,czeile4
C
      read(5,'(A)') cfile1
      call shorten(cfile1,k)
      write(*,'('' infile = '',A40)')cfile1(1:k)
C
      read(5,'(A)') cfile2
      call shorten(cfile2,k)
      write(*,'('' outfile = '',A40)')cfile2(1:k)
C
      read(5,'(A)') cfile4
      call shorten(cfile4,k)
      write(*,'('' listfile = '',A40)')cfile4(1:k)
C
      read(5,*) ispcgrp
      write(*,'('' ispcgrp = '',I10)')ispcgrp
C
      read(5,*) realangle
      write(*,'('' realangle = '',F12.3)')realangle
C
      read(5,*)ibeamtiltref
      write(*,'('' ibeamtiltref = '',I10)')ibeamtiltref
C
      read(5,*)itiltaxref
      write(*,'('' itiltaxref = '',I10)')itiltaxref
C
      open(1,FILE=cfile1,STATUS='OLD',ERR=900)
      open(2,FILE=cfile2,STATUS='NEW',ERR=900)
      open(4,FILE=cfile4,STATUS='NEW',ERR=900)
C
      write(4,'('' Phase     Origin       TAXA,TANGL    TAXA,  TANGL      BeamTilt       BeamTilt       Directory'')')
      write(4,'('' -Res.     -Shift         -Shift      (New)  (New)       -Shift'')')
      write(4,'(''-----------------------------------------------'',
     1          ''-----------------------------------------------'',
     2          ''-----------------'')')
C
      write(2,'(''#!/bin/tcsh -e'')')
      write(2,'(''#'')')
      write(2,'(''#'')')
      write(2,'(''set olddir = $PWD'')')
      write(2,'(''#'')')
C
 100  continue
        read(1,'(A)',end=200)cfile3
        read(1,*,end=910,err=910)RHHORIORI,RHKORIORI
        read(1,*,end=910,err=910)RHSHMIN,RHSKMIN
        read(1,*,end=910,err=910)RHOHRIGSH,RHOKRIGSH
        read(1,*,end=910,err=910)RHORITAXA,RHORITANGL
        read(1,*,end=910,err=910)RHSHTAXA,RHSHTANGL
        read(1,*,end=910,err=910)RTAXA,RTANGL
        read(1,*,end=910,err=910)RHORITLTH,RHORITLTK
        read(1,*,end=910,err=910)RHSHTHLT,RHSHTKLT
        read(1,*,end=910,err=910)RHTHLT,RHTKLT
        read(1,*,end=910,err=910)RERRMIN,RNRESALL
C
        call shorten(cfile3,ih)
        write(2,'(''cd '',A)')cfile3(1:ih)
        write(2,'(''# Phase Residual:'')')
        write(2,'(''tredat 4 DATAFILE.dat 126 '',F12.3)')RERRMIN
        write(czeile1,'(2F12.3)')RHOHRIGSH,RHOKRIGSH
        call inkomma(czeile1,k)
        write(2,'(''# Phase Origin:'')')
        write(2,'(''tredat 4 DATAFILE.dat 103 "'',A,''"'')')czeile1(1:k)
        if(ispcgrp.eq.1)then
          write(2,'(''  tredat 4 DATAFILE.dat 109 "'',A,''"'')')
     .      czeile1(1:k)
        else
          write(2,'(''tredat 4 DATAFILE.dat 104 "'',A,''"'')')
     .      czeile1(1:k)
          write(2,'(''set lastjob = `getline DATAFILE.dat 84`'')')
          write(2,'(''if ( ${lastjob} == 0 ) then'')')
          write(2,'(''  tredat 4 DATAFILE.dat 109 "'',A,''"'')')
     .      czeile1(1:k)
          write(2,'(''endif'')')
        endif
        if(ibeamtiltref.eq.1)then
          write(czeile2,'(2F12.3)')RHTHLT,RHTKLT
          call inkomma(czeile2,l)
          write(2,'(''# Beam Tilt:'')')
          write(2,'(''tredat 4 DATAFILE.dat 111 "'',A,''"'')')
     .      czeile2(1:l)
        endif
        if(itiltaxref.eq.1)then
          write(2,'(''# TAXA,TANGL,TLTAXIS,TLTANG,TLTAXA:'')')
          write(2,'(''tredat 4 DATAFILE.dat 20 '',F12.3)')RTAXA
          write(2,'(''tredat 4 DATAFILE.dat 24 '',F12.3)')RTANGL
          write(2,'(''mrctestangle 1'')')
        endif
        write(2,'(''cd $olddir'')')
        write(2,'(''#'')')
C
        write(czeile1,'(2F12.3)')RHSHMIN,RHSKMIN
        call inkomma(czeile1,i1)
C
        write(czeile2,'(2F12.3)')RHSHTAXA,RHSHTANGL
        call inkomma(czeile2,i2)
C
        write(czeile3,'(2F12.3)')RHSHTHLT,RHSHTKLT
        call inkomma(czeile3,i3)
C
        write(czeile4,'(2F12.3)')RHTHLT,RHTKLT
        call inkomma(czeile4,i4)
C
        write(4,'(F5.1,X,A15,A15,F7.1,F7.1,X,A15,X,A15,X,A)')
     .   RERRMIN,czeile1(1:i1),czeile2(1:i2),RTAXA,RTANGL,
     .   czeile3(1:i3),czeile4(1:i4),cfile3(1:ih)
C
      goto 100
C
 200  continue
C
      write(2,'(''${proc_2dx}/lin "done."'')')
C
      close(1)
      close(2)
      close(3)
      write(*,'('' mergeeval: finished ok.'')')
C
      goto 999
C
 900  continue
        write(*,'('' mergeeval: ERROR on file open'')')
        stop 'error occured'
 910  continue
        write(*,'('' mergeeval: ERRO on file read'')')
        stop 'error occured'
C
 999  continue
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
      SUBROUTINE INKOMMA(CZEILE,k)
C
C replaces intermedieate spaces within the actual text string
C in CZEILE up to the length k by komma.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1,CTMP2,CTMP3
      CHARACTER * 100 CZEIL2
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
      IF(CTMP1.EQ.CTMP3)then
        WRITE(CZEILE(I:I),'('' '')')
        K=K-1
      endif
C
      if(k.lt.1)k=1
C
C     WRITE(*,'('' INKOMMA 4: k='',I4,'':'',A)')k,CZEILE(1:k)
C
      RETURN
      END
C

