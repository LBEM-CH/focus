      PROGRAM MODIFP
C
      character * 80 czeile,czeil2
      character * 80 filin,filout,filcor,fildim
      character * 1 ctmp1
      INTEGER ifield(1000,2),isize
      REAL rfact
      ctmp1 = ' '
C
      READ(5,'(A)') filin
      READ(5,'(A)') filout
      READ(5,'(A)') filcor
      READ(5,'(A)') fildim
      READ(5,*) isize
      READ(5,*) rfact
      READ(5,*) iborder
C
      WRITE(*,'('' Input File        = '',A40)')filin
      WRITE(*,'('' Output File       = '',A40)')filout
      WRITE(*,'('' Coordinate File   = '',A40)')filcor
      WRITE(*,'('' New Dimension File= '',A40)')fildim
      WRITE(*,'('' Image Size        = '',I8)')isize
      WRITE(*,'('' Correction Factor = '',F12.3)')rfact
      WRITE(*,'('' Cropping border   = '',I8,/)')iborder
C
      OPEN(unit=11,FILE=filin,STATUS='OLD')
C
      read(11,*,END=200)ilsizx,ilsizy
      rfacx=rfact*isize/ilsizx
      rfacy=rfact*isize/ilsizy
C
      icoun = 1
 100  continue
        read(11,*,END=200)ifield(icoun,1),ifield(icoun,2)
C
        ifield(icoun,1)=ifield(icoun,1)*rfacx
        if(ifield(icoun,1).ge.isize)ifield(icoun,1)=isize-1
        if(ifield(icoun,1).le.0    )ifield(icoun,1)=1
C
        ifield(icoun,2)=ifield(icoun,2)*rfacy
        if(ifield(icoun,2).ge.isize)ifield(icoun,2)=isize-1
        if(ifield(icoun,2).le.0    )ifield(icoun,2)=1
C
        icoun = icoun+1
        if(icoun.ge.1000) goto 900
      goto 100
C
 200  continue
C
      IXSTART=ifield(1,1)
      IXEND  =ifield(1,1)
      IYSTART=ifield(1,2)
      IYEND  =ifield(1,2)
C
      icoun=icoun-1
      OPEN(unit=12,FILE=filout,STATUS='NEW')
C---------inum, ioutzero
      write(czeile,'(I5,'',1   ! NumberOfVertices, IOUTZERO'')')icoun
      call shorten(czeile,k)
      write(12,'(A)')czeile(1:k)
C-------phase origin
      write(12,'(''0,0   ! Phase origin bottom left corner'')')
      do 300 i=1,icoun
        if(ifield(i,1).lt.IXSTART)IXSTART=ifield(i,1)
        if(ifield(i,1).gt.IXEND  )IXEND  =ifield(i,1)
        if(ifield(i,2).lt.IYSTART)IYSTART=ifield(i,2)
        if(ifield(i,2).gt.IYEND  )IYEND  =ifield(i,2)
        write(czeile,'(I8,'','',I8)')ifield(i,1),ifield(i,2)
        call inkomma(czeile,k)
        write(12,'(A)')czeile(1:k)
 300  continue
      write(czeile,'(I8,'','',I8)')ifield(1,1),ifield(1,2)
      call inkomma(czeile,k)
      write(12,'(A)')czeile(1:k)
C
      close(11)
      close(12)
C
      IXSTART=IXSTART-iborder
      IXEND  =IXEND  +iborder
      IYSTART=IYSTART-iborder
      IYEND  =IYEND  +iborder
C
      if(IXSTART.lt.0    )IXSTART=0
      if(IXEND  .ge.isize)IXEND  =isize-1
      if(IYSTART.lt.0    )IYSTART=0
      if(IYEND  .ge.isize)IYEND  =isize-1
C
      IXWID = IXEND-IXSTART
      IYWID = IYEND-IYSTART
C
      IRAD=IXWID/2
      if(IYWID.gt.IXWID)then
        IRAD=IYWID/2
      endif
C
      IWID = 2 * IRAD
C
      ixgood = 0
      call PRITES(IWID,iok,0,czeil2)
      if (iok.eq.0) then
         write(*,'('':'',I8,'' is a good image width.'')') IWID
         call shorten(czeil2,k)
         write(*,'(A)') czeil2(1:k)
         ixgood = 1
      else
         write(*,'('':'',I8,'' is a BAD image width for FFTs.'')') IWID
         call shorten(czeil2,k)
         write(*,'(A)') czeil2(1:k)
         write(*,'('':Trying to find better...'')')
 400     continue
           IWID = IWID-1
           call PRITES(IWID,iok,0,czeil2)
           if (iok .eq. 0) then
             call shorten(czeil2,k)
             write(*,'(A)') czeil2(1:k)
             ixgood = 1
             goto 450
           endif
         if (IWID .gt. 1) goto 400
      endif
C
 450  continue
C
      if (ixgood .eq. 0) goto 950
C
      IRAD = IWID / 2
C
      IXMID = (IXEND+IXSTART)/2
      IYMID = (IYEND+IYSTART)/2
C
      if ( IXMID - IRAD .lt. 0     ) IXMID = IRAD
      if ( IXMID + IRAD .ge. isize ) IXMID = isize - IRAD - 1
      if ( IYMID - IRAD .lt. 0     ) IYMID = IRAD
      if ( IYMID + IRAD .ge. isize ) IYMID = isize - IRAD - 1
C   
      IXSTART = IXMID - IRAD
      IXEND   = IXMID + IRAD - 1
      IYSTART = IYMID - IRAD
      IYEND   = IYMID + IRAD - 1
C
      OPEN(unit=13,FILE=filcor,STATUS='NEW')
      write(13,'(4I8)')IXSTART,IXEND,IYSTART,IYEND
      close(13)
C
      OPEN(unit=14,FILE=fildim,STATUS='NEW')
      write(14,'(I8)')IWID
      close(14)
C
      goto 999
 900  continue
        STOP 'error in MODIFP'
      goto 999
 950  continue
        STOP 'error in PRITES'
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
      k=ilen
C
C-----find the leading spaces and remove them
C
      DO 90 J=1,k
         READ(CZEILE(J:J),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2) GOTO 95
 90   CONTINUE
 95   CONTINUE
C
      WRITE(CZEILE(1:k),'(A)') CZEILE(J:k)
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
c==========================================================
C
      SUBROUTINE PRITES(iwert,iok,ilog,czeil2)
C             
      character * 80 czeil2
C
      INTEGER ifeld(8)
      DATA ifeld/2,3,5,7,11,13,17,19/
C
      irun=iwert
      itest=1
      ianf=13
      write(czeil2,'(''::'',I8,'' = '')') iwert
C
C-----First test, if the number iwert can be devided by 4. This should be, because
C     the fft is also calculated of the reduced image.
C
      irest = int(iwert/4)*4
      if (irest .ne. iwert) then
        write(czeil2(ianf:80),'('' not multiple of 4 ! '')')
        iok = -1
        goto 990
      endif
C
 310  continue
      irest = int(irun/ifeld(itest))*ifeld(itest)
      if (irest .eq. irun) then
         irun=irun/ifeld(itest)
         if (ifeld(itest) .lt. 10) then
           write(czeil2(ianf:80),'(I2,'' *'')') ifeld(itest)
           ianf=ianf+4
         else
           write(czeil2(ianf:80),'(I3,'' *'')') ifeld(itest)
           ianf=ianf+5
         endif
         if (ianf .gt. 70) then
            write(czeil2(10:80),'('' many many many.... '')')
            ianf=71
         endif
         itest=1
      else
         itest=itest+1
         if (itest .gt. 8) then
           if (ilog .eq. 1)
     1     write(*,'(''::Primefactor exceeds 19, rest = '',I8)') irun
           write(czeil2(ianf:80),'('' (too big)'')')
           iok = -1
           goto 990
         endif
      endif
      if (irun .gt. 1) goto 310
      ianf=ianf-2
      write(czeil2(ianf:80),'(''  '')')
      iok = 0
c
 800  continue
c
 990  continue
C
      RETURN
      END

c==========================================================


