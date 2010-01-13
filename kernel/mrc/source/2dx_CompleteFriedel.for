       PROGRAM FRIEDEL

       character*80 cfile1,cfile2,cline1

       write(*,'('' INPUT APH file name'')')
       read(*,'(A)')cfile1
       call shorten(cfile1,k)
       write(*,'('' input was '',A)')cfile1(1:k)

       write(*,'('' OUPUT APH file name'')')
       read(*,'(A)')cfile2
       call shorten(cfile2,k)
       write(*,'('' input was '',A)')cfile2(1:k)

       open(11,FILE=cfile1,STATUS='OLD',ERR=900)
       open(12,FILE=cfile2,STATUS='NEW',ERR=900)

C      filenumber
       read(11,'(A)')cline1
       call shorten(cline1,k)
       write(12,'(A)')cline1(1:k)
C
 100   continue
         READ(11,*,END=200,ERR=200)
     .      IH,IK,Z,AMP,PHASE,IFILM,IQ,WGT,BACK,CTF
         WRITE(12,110) IH, IK, Z,AMP, PHASE,IFILM,IQ,WGT,BACK,CTF
         WRITE(12,110)-IH,-IK,-Z,AMP,-PHASE,IFILM,IQ,WGT,BACK,CTF
 110     FORMAT(1X,2I4,F8.4,F10.1,F7.1,I12,I3,F8.5,F10.1,F7.3)
C    0   2  0.0000    1273.1 -178.0      100300  1 1.00000      56.3 -0.149
C    0   3  0.0000     121.0  -11.8      100300  3 0.54752      34.0 -0.245
C    0   4  0.0000     267.4   -5.1      100300  2 0.60502      37.6 -0.374
C    0   5  0.0000     310.1   -2.9      100300  1 0.62179      36.3 -0.526
       goto 100
C
 200   continue

       close(11)
       close(12)
 
       goto 999
 
 900   continue
         write(*,'('':: ERROR in FRIEDEL.for'')')
     
 999   continue
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

