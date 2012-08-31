c     2dx_LENCALC.FOR  calculates the lenghts of lattice vectors.
C
C     Henning Stahlberg, May 2006
C
      PROGRAM LENCALC
C
      IMPLICIT NONE
C
      real RA(3),RB(3),RC(3)
      real realta(2),realtb(2)
      real realtal,realtbl,realtang
      real realctal,realctbl,realctang
      real PI
      real reciu(2),reciv(2)
      real rtest(2),ralpha
      real rtiltul,rtiltvl,rangl
      real reciang,realx,realy
      real reala(2),realb(2)
      real reciul,recivl
      real rmag,rpix
      real realspace_angle
      integer ISIZE,ihand
      character * 80 cname
C
      PI=3.141592654
C
      write(6,'(/,'' input reciprocal lattice in pixel '')')
      read(5,*)RA(1),RA(2),RB(1),RB(2)
      RA(3)=0.0
      RB(3)=0.0
      write(6,'('' read: '',4F12.4)')RA(1),RA(2),RB(1),RB(2)
C
      write(6,'(/,'' input real-space lattice in Angstroem '')')
      read(5,*)realx,realy
      write(6,'('' read: '',2F12.3)')realx,realy
C
      write(6,'(/,'' input real-space lattice angle '')')
      read(5,*)realspace_angle
      write(6,'('' read: '',F12.3)')realspace_angle
C
      write(6,'(/,'' input original picture size  '')')
      read(5,*)ISIZE
      write(6,'('' read: '',I10)')ISIZE
C
      write(6,'(/,'' input magnification '')')
      read(5,*)rmag
      write(6,'('' read: '',F12.1)')rmag
C
      write(6,'(/,'' input stepsize digitizer '')')
      read(5,*)rpix
      write(6,'('' read: '',F12.3)')rpix
C
      write(6,'(/,'' input name of output file '')')
      read(5,'(A)')cname
      write(6,'('' read: '',A40)')cname
C     
      write(6,'('' '')')
C
      open(11,FILE=cname,STATUS='NEW',ERR=900)
C
      call cross(RA,RB,RC)
C
      if(RC(3).ge.0)then
        ihand = 1
        WRITE(11,'('': Right-handed lattice '',/,I4)')ihand
      else
        ihand = -1 
        WRITE(11,'('':: ##### ATTENTION: Left-handed lattice ####### '',/,I4)')ihand
      endif
C
C-----transform tilted reciprocal lattice into a tilted real-space lattice
C
      call reciproc(RA,RB,realta,realtb,ISIZE)
C
C-----Calculate the lengths of the measured tilted real lattice, in Pixels
C
      call length(realta,realtal)
      call length(realtb,realtbl)
C
C-----Calculate the length of the measured tilted real lattice in Angstroems
C
      realtal = realtal * rpix * 10000.0 / rmag
      realtbl = realtbl * rpix * 10000.0 / rmag
C
C-----Calculate the included angle in the measured tilted real lattice
C
      call getang(realta,realtb,realtang)
C
C-----Create the non-tilted theoretical real-space lattice
C
C-----This would produce a real-space lattice with a in X-axis, b up.
C      reala(1) = (realx*rmag)/(rpix*10000.0)
C      reala(2) = 0.0
C      realb(1) = (realy*rmag*cos(realspace_angle*pi/180.0))/(rpix*10000.0)
C      realb(2) = (realy*rmag*sin(realspace_angle*pi/180.0))/(rpix*10000.0)
C
C-----Instead, we use a real-space lattice with b in Y-axis, and a somewhere right.
C-----This then gives a reciprocal lattice with reciproc. u in X-axis direction.
      reala(1) = (realx*rmag*sin(realspace_angle*pi/180.0))/(rpix*10000.0)
      reala(2) = (realx*rmag*cos(realspace_angle*pi/180.0))/(rpix*10000.0)
      realb(1) = 0.0
      realb(2) = (realy*rmag)/(rpix*10000.0)
C
      write(*,'(''real lattice = '',4F12.3)')reala(1),reala(2),realb(1),realb(2)
      write(*,'(''included angle='',F12.3)')realspace_angle
C
C-----transform non-tilted theoretical real-space lattice into reciprocal space
C
      call reciproc(reala,realb,reciu,reciv,ISIZE)
      write(*,'(''reci lattice = '',4F12.3)')reciu(1),reciu(2),reciv(1),reciv(2)
C
C-----Calculate the included angle in the non-tilted theoretical reciprocal lattice
C
      call getang(reciu,reciv,reciang)
      write(*,'(''included angle='',F12.3)')reciang
C
C-----Calculate the lengths of the non-tilted theoretical reciprocal lattice, in rec. Pixels
C
      call length(reciu,reciul)
      call length(reciv,recivl)
C
C-----Output non-tilted theoretical reciprocal lattice
C
      write(11,'('': Theoretical untilted rec. lattice = '',
     .  3F12.3,/,3F12.3)')
     .  reciul,recivl,reciang,reciul,recivl,reciang
C
C-----Output the angle alpha: the angle between the X-axis and the tilted reciprocal lattice vector H
C
      rtest(1) = 1.0
      rtest(2) = 0.0
      call getang(rtest,RA,ralpha)
C
      write(11,'('': alpha: angle between X and tilt. rec. H = ''
     1   ,F12.3,/,F12.3)')ralpha,ralpha
C
C-----Calculate the length of the measured tilted reciprocal lattice:
C
      call length(RA,rtiltul)
      call length(RB,rtiltvl)
C
C-----Calculate the angle between the measured tilted reciprocal lattice vectors
C
      call getang(RA,RB,rangl)
C
C-----Output the measured tilted reciprocal lattice
C
      write(11,'('': Measured tilted rec. lattice = ''
     1  3F12.3,/,3F12.3)')rtiltul,rtiltvl,rangl,rtiltul,rtiltvl,rangl
C
C-----Output the measured tilted real-space lattice
C
      if(ihand.eq.1)then
        write(11,'('': Measured tilted real-space lattice = ''
     1    3F12.3,/,3F12.3)')realtal,realtbl,realtang,
     2    realtal,realtbl,realtang 
      else
C          realctal=realtbl
C          realctbl=realtal
C---------Leave the vectors as they are, but invert the angle
          realctal=realtal
          realctbl=realtbl
          realctang=-realtang
        write(11,'('': Measured tilted real-space lattice '',
     1             ''(after handedness correction) = ''
     2    3F12.3,/,3F12.3)')realctal,realctbl,realctang,
     3    realctal,realctbl,realctang 
      endif
C
      close(11)
C
      goto 999
C
 900  continue
        write(*,'('':: ERROR on file open'')')
        STOP ' ERROR on file open'
C
 999  continue
C
      STOP
      END     
C
C***************************************************************************
C***************************************************************************
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
      SUBROUTINE CROSS(A,B,C)
C
C*****Calculates C=AxB
C
      dimension A(3),B(3),C(3)
C
      C(1) = A(2)*B(3) - A(3)*B(2)
      C(2) = A(3)*B(1) - A(1)*B(3)
      C(3) = A(1)*B(2) - A(2)*B(1)
C
      return
      end

C===========================================================================
C
      SUBROUTINE getang(RLATH,RLATK,RANGrec)
C
      dimension RLATH(2),RLATK(2)
      real rtest1(3),rtest2(3),rtest3(3)
C
      PI=3.141592654
C
      rtest1(1)=RLATH(1)
      rtest1(2)=RLATH(2)
      rtest1(3)=0.0
C
      rtest2(1)=RLATK(1)
      rtest2(2)=RLATK(2)
      rtest2(3)=0.0
C
      call CROSS(rtest1,rtest2,rtest3)
C
      if(rtest3(3).ge.0)then
        rsign=1.0
      else
        rsign=-1.0
      endif
C
      rdot = RLATH(1)*RLATK(1)+RLATH(2)*RLATK(2)
      rlenh = SQRT(RLATH(1)*RLATH(1)+RLATH(2)*RLATH(2))
      rlenk = SQRT(RLATK(1)*RLATK(1)+RLATK(2)*RLATK(2))
C
      RANGrec = rsign * acos(rdot / (rlenh * rlenk)) * 180.0 / PI
C
      RETURN
      END
C
C===========================================================================
C
      SUBROUTINE reciproc(realu,realv,RLATH,RLATK,ISIZE)
C
C-----calculates the reciprocal lattice of a given real-space lattice
C-----or the other direction, using
C
C     Real space:
C     U=(u1, u2);  V=(v1,v2); f = image size (e.g. 512, for an image of 512x512 pixels).
C     
C     Reciprocal space:
C     U*=(v2,-v1) * f / mod(UxV)
C     V*=(-u2,u1) * f / mod(UxV)
C     
C     with UxV = u1*v2 - v1*u2
C     
      DIMENSION realu(2),realv(2),RLATH(2),RLATK(2)
C
      rfact = real(ISIZE)/abs(realu(1)*realv(2)-realu(2)*realv(1))
      RLATH(1) =  realv(2) * rfact
      RLATH(2) = -realv(1) * rfact
      RLATK(1) = -realu(2) * rfact
      RLATK(2) =  realu(1) * rfact
C
      RETURN
      END
C
C===========================================================================
C
      SUBROUTINE vecrot(RLAT,RLATORI,RNLAT)
C
      dimension RLAT(2),RNLAT(2)
C
      rlocang=RLATORI*3.141592657/180.0
C
C       write(6,'('' RLATORI '',F)')RLATORI
C       write(6,'('' RLAT    '',2F)')RLAT(1),RLAT(2)
C
      RNLAT(1)=cos(rlocang)*RLAT(1)-sin(rlocang)*RLAT(2)
      RNLAT(2)=sin(rlocang)*RLAT(1)+cos(rlocang)*RLAT(2)
C
C     write(6,'('' RNLAT   '',2F)')RNLAT(1),RNLAT(2)
C
      RETURN
      END
C

C===========================================================================
C
      SUBROUTINE length(RLAT,rlen)
C
      dimension RLAT(2)
C
      rlen = sqrt(RLAT(1)*RLAT(1)+RLAT(2)*RLAT(2))
C
      RETURN
      END
C

