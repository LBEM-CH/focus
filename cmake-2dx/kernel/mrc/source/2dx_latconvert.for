      PROGRAM LATCONVERT
C
      character * 100 czeile
c
      read(*,'(A)',ERR=200,END=200) czeile
      read(czeile(1:39),*,ERR=200,END=200) r1ax,r1ay,r1bx,r1by
      read(*,'(A)',ERR=200,END=200) czeile
      read(czeile(1:39),*,ERR=200,END=200) r2ax,r2ay,r2bx,r2by
      read(*,'(A)',ERR=200,END=200) czeile
      read(czeile(1:39),*,ERR=200,END=200) rsizeo
      read(*,'(A)',ERR=200,END=200) czeile
      read(czeile(1:39),*,ERR=200,END=200) rsizen
C
      read(*,'(A)',ERR=200,END=200) czeile
      read(czeile(1:39),*,ERR=200,END=200) mask1
      read(*,'(A)',ERR=200,END=200) czeile
      read(czeile(1:39),*,ERR=200,END=200) mask2
      read(*,'(A)',ERR=200,END=200) czeile
      read(czeile(1:39),*,ERR=200,END=200) mask3
      read(*,'(A)',ERR=200,END=200) czeile
      read(czeile(1:39),*,ERR=200,END=200) mask4
      read(*,'(A)',ERR=200,END=200) czeile
      read(czeile(1:39),*,ERR=200,END=200) mask5
      read(*,'(A)',ERR=200,END=200) czeile
      read(czeile(1:39),*,ERR=200,END=200) mask6
      read(*,'(A)',ERR=200,END=200) czeile
      read(czeile(1:39),*,ERR=200,END=200) mask7
      read(*,'(A)',ERR=200,END=200) czeile
      read(czeile(1:39),*,ERR=200,END=200) mask8
C
      r1ax=r1ax/rsizeo*rsizen
      r1ay=r1ay/rsizeo*rsizen
      r1bx=r1bx/rsizeo*rsizen
      r1by=r1by/rsizeo*rsizen
C
      r2ax=r2ax/rsizeo*rsizen
      r2ay=r2ay/rsizeo*rsizen
      r2bx=r2bx/rsizeo*rsizen
      r2by=r2by/rsizeo*rsizen
C
      mask1=int(real(mask1)/rsizeo*rsizen+0.5)
      mask2=int(real(mask2)/rsizeo*rsizen+0.5)
      mask3=int(real(mask3)/rsizeo*rsizen+0.5)
      mask4=int(real(mask4)/rsizeo*rsizen+0.5)
      mask5=int(real(mask5)/rsizeo*rsizen+0.5)
      mask6=int(real(mask6)/rsizeo*rsizen+0.5)
      mask7=int(real(mask7)/rsizeo*rsizen+0.5)
      mask8=int(real(mask8)/rsizeo*rsizen+0.5)
C
      OPEN(12,FILE='TMP456770.tmp',STATUS='NEW',ERR=200)
      write(czeile,'(F8.3,'','',F8.3,'','',F8.3,'','',F8.3)')
     1   r1ax,r1ay,r1bx,r1by
      call inkomma(czeile,k)
      write(12,'(A)') czeile(1:k)
      close(12)
C
      OPEN(12,FILE='TMP456779.tmp',STATUS='NEW',ERR=200)
      write(czeile,'(F8.3,'','',F8.3,'','',F8.3,'','',F8.3)')
     1   r2ax,r2ay,r2bx,r2by
      call inkomma(czeile,k)
      write(12,'(A)') czeile(1:k)
      close(12)
C
      OPEN(12,FILE='TMP456771.tmp',STATUS='NEW',ERR=200)
      write(czeile,'(I8)')mask1
      call shorten(czeile,k)
      write(12,'(A)') czeile(1:k)
      close(12)
C
      OPEN(12,FILE='TMP456772.tmp',STATUS='NEW',ERR=200)
      write(czeile,'(I8)')mask2
      call shorten(czeile,k)
      write(12,'(A)') czeile(1:k)
      close(12)
C
      OPEN(12,FILE='TMP456773.tmp',STATUS='NEW',ERR=200)
      write(czeile,'(I8)')mask3
      call shorten(czeile,k)
      write(12,'(A)') czeile(1:k)
      close(12)
C
      OPEN(12,FILE='TMP456774.tmp',STATUS='NEW',ERR=200)
      write(czeile,'(I8)')mask4
      call shorten(czeile,k)
      write(12,'(A)') czeile(1:k)
      close(12)
C
      OPEN(12,FILE='TMP456775.tmp',STATUS='NEW',ERR=200)
      write(czeile,'(I8)')mask5
      call shorten(czeile,k)
      write(12,'(A)') czeile(1:k)
      close(12)
C
      OPEN(12,FILE='TMP456776.tmp',STATUS='NEW',ERR=200)
      write(czeile,'(I8)')mask6
      call shorten(czeile,k)
      write(12,'(A)') czeile(1:k)
      close(12)
C
      OPEN(12,FILE='TMP456777.tmp',STATUS='NEW',ERR=200)
      write(czeile,'(I8)')mask7
      call shorten(czeile,k)
      write(12,'(A)') czeile(1:k)
      close(12)
C
      OPEN(12,FILE='TMP456778.tmp',STATUS='NEW',ERR=200)
      write(czeile,'(I8)')mask8
      call shorten(czeile,k)
      write(12,'(A)') czeile(1:k)
      close(12)
C
      goto 800
 200  continue
         write(*,'('' ======== ERROR in LATCONVERT ========'')') 
      goto 800
c
 800  continue
c
      END
             
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
