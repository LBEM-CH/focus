      PROGRAM powerhisto
C
      character*200 cfile1,cfile2,cline1,cline2
      real rtab(5000)
C
      write(6,'(/,'':Powerhisto: to calculate the radial power '',
     .    ''of the peaks in the powerspectrum'',/)')
      write(6,'(''Give input file name'')')
      read(5,'(A)')cfile1
      call shorten(cfile1,k)
      write(6,'(''Input file name is '',A)')cfile1(1:k)
C
      write(6,'(''Give output file name'')')
      read(5,'(A)')cfile2
      call shorten(cfile2,k)
      write(6,'(''Output file name is '',A)')cfile2(1:k)
C
      write(6,'(''Give reciprocal lattice'')')
      read(5,*)rax,ray,rbx,rby
      write(6,'(''read: '',4F12.3)')rax,ray,rbx,rby
C
      write(6,'(''Give Histogram scale'')')
      read(5,*)scale
      write(6,'(''read: '',F15.6)')scale
C
      open(10,FILE=cfile1,STATUS="OLD",ERR=900)
C
      open(11,FILE=cfile2,STATUS="NEW",ERR=900)
C
      READ(10,'(A)')cline1
      call shorten(cline1,k)
      write(11,'(A)')cline1(1:k)
C
      do i = 1,1000
        rtab(i)=0.0
      enddo
C
      imax = 200 
 100  continue
C
        read(10,*,END=800,ERR=910)IH,IK,RAMP,RPHS,IQ,RBACK,RCTF
        rposx=IH*rax+IK*rbx
        rposy=IH*ray+IK*rby
        rpos=sqrt(rposx*rposx+rposy*rposy)
        amp=RAMP-RBACK
        ipos=INT(rpos*scale)
        if(ipos.gt.imax)imax=ipos
        rtab(ipos)=rtab(ipos)+amp
C
      goto 100
C
 800  continue
C
      do i = 1,imax
        res=real(i)/scale
        write(11,'(I9,F15.6,F12.3)')i,res,rtab(i)
      enddo
C
      close(10)
      close(11)
C
      goto 999
C
 900  continue
      write(6,'('':: ERROR on file open'')')
      goto 999
C
 910  continue
      write(6,'('':: ERROR on file read'')')
      goto 999
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

