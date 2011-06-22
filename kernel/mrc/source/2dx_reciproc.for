      PROGRAM reciproc
C
C
C---- (C) 2dx.org
C
C---- to transform a given lattice in real space into the corresponding lattice in 
C---- reciprocal space, or vice versa.
C
C---- This program calculates the reciprocal lattice vectors for a given real-space 
C---- lattice. It requires the image dimension, and only works for square images.
C
C     It uses:
C
C     Real space:
C     U=(u1, u2);  V=(v1,v2); 
C     f = image size (e.g. 512, for an image of 512x512 pixels).        
C
C     Then the lattice in the Reciprocal space is:
C     U*=(v2,-v1) * f / length(UxV)        
C     V*=(-u2,u1) * f / length(UxV)
C     with length(UxV) = u1*v2 - v1*u2
C
C     Created on 4/2/2007 by Henning.
C
      character*200 cline,cname
C
      write(*,'(''2dx_reciproc: Transform lattice from real '',
     .   ''to reciprocal space'',
     .   '', or vice versa.'')')
C
      write(*,'(/,''Give lattice vectors ax,ay,bx,by'')')
      read(*,*,ERR=990)ax,ay,bx,by
      write(*,'('' Read: '',4F12.3)')ax,ay,bx,by
C
      write(*,'(/,''Give image side length (for square image only)'')')
      read(*,*,ERR=990)ilength
      write(*,'('' Read: '',I9)')ilength
C
      write(*,'(/,''Give filename for output file'')')
      read(*,'(A)',ERR=990)cname
      call shorten(cname,k)
      write(*,'('' Read: '',A)')cname(1:k)
C
      rAxB = ax*by-bx*ay
      rmodAxB = abs(rAxB)
C
      if(rmodAxB.lt.0.000000001)then
        write(*,'(''::ERROR: Lattice vectors not independent.'')')
        goto 990
      endif
C
      rax =  by * ilength / rAxB
      ray = -bx * ilength / rAxB
      rbx = -ay * ilength / rAxB
      rby =  ax * ilength / rAxB
C
      write(*,'(/,''Reciprocal lattice in pixel is'',4F12.3,/)')
     .  rax,ray,rbx,rby
C
      open(11,FILE=cname,STATUS="NEW",ERR=990)
      write(cline,'(4F15.3)')
     .  rax,ray,rbx,rby
      call inkomma(cline,k)
      write(11,'(A)')cline(1:k)
      close(11)
C
      goto 999
C
 990  continue
      write(*,'(''::ERROR: Error in 2dx_reciproc.'')')
C
 999  continue
C
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

