      PROGRAM GETNUMBER
C
C-----Program to extract a numerical string from the given input string.
C-----Useful to get the image number from the file name.
C
      character * 100 czeile
      character * 1 CTMP,CTMP0,CTMP1,CTMP9
c
      CTMP0 = '0'
      CTMP1 = '1'
      CTMP9 = '9'
C
      read(*,'(A)',err=800,end=800) czeile
C
C-----Now czeile contains the input string.
c
      i=len(czeile)
 100  continue
        i=i-1
        if(i.lt.1)goto 300
        read(czeile(i:i),'(A1)')CTMP
        if((CTMP.LT.CTMP1 .OR. CTMP.GT.CTMP9)
     1    .AND. CTMP.NE.CTMP0)goto 100
C
C-----Now i points to the last numerical character in string.
C
      k=i
 200  continue
        k=k-1
        if(k.lt.1)goto 250
        read(czeile(k:k),'(A1)')CTMP
        if((CTMP.GE.CTMP1 .AND. CTMP.LE.CTMP9)
     1    .OR. CTMP.EQ.CTMP0)goto 200
C
 250  continue
      k=k+1
C
C-----Now k points to the first numerical character in that number.
C
      write(*,'(A)')czeile(k:i)
C
      goto 900
C
 300  continue
C-----There is no number in that name. Set it to 1000
      i=len(czeile)
C      write(*,'(''1000-'',A)')czeile(1:i)
      write(*,'(''1000'')')
      goto 900
C
 800  continue
         write(*,'('' ======== ERROR OCCURRED in GETNUMBER ========'')')
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


