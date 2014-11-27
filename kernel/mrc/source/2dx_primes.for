      PROGRAM PRIMES
C
C     Henning Stahlberg
C
      character * 100 czeile,cname
      character * 80 czeil2,czeil3
c
      isumme=0
c
      read(*,'(A)',end=200) czeile
      read(*,'(A)',end=203) cname
C     write(*,'('' Read: '',A)') czeile(1:20)
      read(czeile(1:20),'(I20)',err=200) iwert
C     write(*,'('' iwert = '',I8)') iwert
      goto 210
 200  continue
         write(*,'('' ======== Error ============'')') 
         write(*,'('' Input was : '',A)') czeile(1:20)
      goto 990
 203  continue
         write(*,'('' ======== Error ============'')') 
         write(*,'('' Input was : '',A)') cname(1:60)
      goto 990
C
 210  continue
C
      iowert = iwert
C
      ixgood = 0
      call PRITES(iwert,iok,0,czeil2)
      if (iok.eq.0) then
         write(*,'('':'',I8,'' is a good image size.'')') iwert
         call shorten(czeil2,k)
         write(*,'(A)') czeil2(1:k)
      else
         write(*,'('':'',I8,'' is a BAD image size.'')') iwert
         call shorten(czeil2,k)
         write(*,'(A)') czeil2(1:k)
         write(*,'('':Trying to find better ...'')')
 300     continue
           iwert = iwert-1
           call PRITES(iwert,iok,0,czeil2)
           if (iok .eq. 0) then
             call shorten(czeil2,k)
             write(*,'(A)') czeil2(1:k)
             ixgood = 1
             goto 350
           endif
         if (iwert .gt. 1) goto 300
      endif
C
 350  continue
c
      if (ixgood .eq. 0) goto 990
C
      OPEN(11,FILE=cname,STATUS='NEW')
      write(11,'(I8)') iwert
C
      ixstart = (iowert - iwert) / 2
      if(ixstart.lt.0)ixstart=0
      ixend   = ixstart + iwert - 1
      iystart = ixstart
      iyend   = ixend
C
      write(11,'(4I8)')ixstart,ixend,iystart,iyend
C
      close(11)
c
 990  continue
C
      STOP
      END
C
C============================================================================
c
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
      write(czeil2,'('':'',I8,'' = '')') iwert
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
     1     write(*,'('':Primefactor exceeds 19, rest = '',I8)') irun
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
c==========================================================
c
      SUBROUTINE compre(czeile,k)
C
C compresses the text in czeile, length out in k. 
C
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1,CTMP2
      CTMP2=' '
      ilen=len(czeile)
      if(ilen.gt.100)ilen=100
      DO 100 I=1,ilen
         READ(CZEILE(i:i),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)GOTO 200
  100 CONTINUE
  200 CONTINUE
      if (i.ge.ilen)i=ilen-1
      write(czeile,'(A)') czeile(i:ilen)
      DO 300 j=1,ilen
         k=ilen-j
         READ(CZEILE(k:k),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)GOTO 400
  300 CONTINUE
  400 CONTINUE
      IF(k.LT.1)k=1
C
      RETURN
      END
C
