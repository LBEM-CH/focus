C
C  2dx_calcmag
C
C  to calculate the theoretical magnification.
C
C  Davis, May 2006, Henning Stahlberg.
C
C
      PROGRAM calcmag
C
      IMPLICIT NONE
C
      integer ISIZE
      real rx,ry,realspace_angle
      real TLTAXIS,TLTANG
      real ru1,ru2,rv1,rv2
      real rmag,rpix
      real PI
      real RLATH(2),RLATK(2)
      real rrealu(2),rrealv(2)
      real rsurfsoll,rsurfis,rmagth
      real RANGrec,RANGrealsample,RANGrealimage
      real rrealul,rrealvl,rrealang
      character * 80 cname
C
      PI=3.141592654
C
      write(6,'(/,'' input real lattice in A '')')
      read(5,*)rx,ry
      write(6,'('' read: '',2F12.4)')rx,ry
C
      write(6,'(/,'' input realspace angle -> realspace_angle '')')
      read(5,*)realspace_angle
      write(6,'('' read: '',F12.4)')realspace_angle
C
      write(6,'(/,'' input angle Xax -> tilt-axis in deg '')')
      read(5,*)TLTAXIS
      write(6,'('' read: '',F12.4)')TLTAXIS
C
      write(6,'(/,'' input tilt angle in deg '')')
      read(5,*)TLTANG
      write(6,'('' read: '',F12.4)')TLTANG
C
      write(6,'(/,'' input reciprocal lattice in pixel '')')
      read(5,*)ru1,ru2,rv1,rv2
      write(6,'('' read: '',4F12.4)')ru1,ru2,rv1,rv2
C
      write(6,'(/,'' input original picture size  '')')
      read(5,*)ISIZE
      write(6,'('' read: '',I10)')ISIZE
C
      write(6,'(/,'' input magnification '')')
      read(5,*)rmag
      write(6,'('' read: '',F12.1)')rmag
C
      write(6,'(/,'' input pixel size of scan in microns '')')
      read(5,*)rpix
      write(6,'('' read: '',F12.3)')rpix
C
      write(6,'(/,'' input name of output file '')')
      read(5,'(A)')cname
      write(6,'('' read: '',A40)')cname
C     
      RLATH(1)=ru1
      RLATH(2)=ru2
      RLATK(1)=rv1
      RLATK(2)=rv2
C
C      write(*,'(''RLATHK:'',4F12.3)')
C     1  RLATH(1),RLATH(2),RLATK(1),RLATK(2)
C
C-----Calculate the included angle in the measured reciprocal lattice
C
      call getang(RLATH,RLATK,RANGrec)
C
C-----Calculate the included angle in the measured real-space lattice
C
      call reciproc(RLATH,RLATK,rrealu,rrealv,ISIZE)
      call getang(rrealu,rrealv,RANGrealimage)
C
C-----undistort measured reciprocal lattice from tilt geometry
C
      call vecundist(RLATH,TLTAXIS,TLTANG)
      call vecundist(RLATK,TLTAXIS,TLTANG)
C
C      write(*,'(''RLATHK:'',4F12.3)')
C     1  RLATH(1),RLATH(2),RLATK(1),RLATK(2)
C
C-----transform now non-tilted measured reciprocal lattice into real-space lattice
C
      call reciproc(RLATH,RLATK,rrealu,rrealv,ISIZE)
C
C      write(*,'(''rrealuv = '',4F12.3)')
C     1  rrealu(1),rrealu(2),rrealv(1),rrealv(2)
C
C-----transform the real-space lattice from pixels into Angstroems
C
      rrealu(1) = rrealu(1) * rpix * 10000.0 / rmag
      rrealu(2) = rrealu(2) * rpix * 10000.0 / rmag
      rrealv(1) = rrealv(1) * rpix * 10000.0 / rmag
      rrealv(2) = rrealv(2) * rpix * 10000.0 / rmag
C
C      write(*,'(''rrealuv = '',4F12.3)')
C     1  rrealu(1),rrealu(2),rrealv(1),rrealv(2)
C
C-----Calculate vector lengths and angle of this now non-tilted measured lattice:
C
      call length(rrealu,rrealul)
      call length(rrealv,rrealvl)
      call getang(rrealu,rrealv,rrealang)
C
C-----Output this now non-tilted measured lattice:
C
      write(6,'('' Corresponding non-tilted real-space lattice is '',
     .  3F12.3)') rrealul,rrealvl,rrealang
C
C-----Calculate the included angle in the real-space lattice
C
      call getang(rrealu,rrealv,RANGrealsample)
C
C-----Calculate the measured included real-space unit cell surface
C
      rsurfis = abs(rrealu(1)*rrealv(2) - rrealu(2)*rrealv(1))
C
C-----Calculate the given included real-space unit cell surface
C
      rsurfsoll = abs(rx * ry * sin(realspace_angle*PI/180.0))
C
C-----Calculate the theoretical magnification
C
      if(rsurfsoll.lt.0.000001)then
        rmagth = 0.0
      else
        rmagth = rmag * SQRT(rsurfis / rsurfsoll)
      endif
C
      write(6,'('' Surfis='',F12.3,'' SurfSoll='',F12.3)')
     1   rsurfis,rsurfsoll
C
      write(6,'('' Theoretical Magnification is '',F12.3)') rmagth
      write(6,'('' Angle in reciprocal lattice is '',F12.3)') RANGrec
      write(6,'('' Angle in real space in image is '',F12.3)') 
     1    RANGrealimage
      write(6,'('' Angle in real space in sample is '',F12.3)') 
     1    RANGrealsample
C
      open(9,FILE=CNAME,STATUS='NEW',ERR=900)
      write(9,'(F12.3)') rmagth
      write(9,'(F12.3)') RANGrec
      write(9,'(F12.3)') RANGrealimage
      write(9,'(F12.3)') RANGrealsample
      write(9,'(F12.3,'','',F12.3)') rrealul,rrealvl
      write(9,'(F12.3)') rrealang
C
      close(9)
C
      goto 999
C
 900  continue
        write(6,'('' ERROR on file open'')')
        goto 999
C
 999  continue
C
      STOP
C
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
      SUBROUTINE getang(RLATH,RLATK,RANGrec)
C
      dimension RLATH(2),RLATK(2)
C
      PI=3.141592654
C
      rdot = RLATH(1)*RLATK(1)+RLATH(2)*RLATK(2)
      rlenh = SQRT(RLATH(1)*RLATH(1)+RLATH(2)*RLATH(2))
      rlenk = SQRT(RLATK(1)*RLATK(1)+RLATK(2)*RLATK(2))
C
      RANGrec = acos(rdot / (rlenh * rlenk)) * 180.0 / PI
C
      RETURN
      END
C
C===========================================================================
C
      SUBROUTINE vecundist(RLAT,TLTAXIS,TLTANG)
C
      dimension RLAT(2)
      real RNLAT(2)
C
      PI=3.141592654
C
      rloca1=TLTAXIS*PI/180.0
C
C-----rotate vector RNLAT around -TLTAXIS degrees
C
      RNLAT(1)= cos(rloca1)*RLAT(1)-sin(rloca1)*RLAT(2)
      RNLAT(2)= sin(rloca1)*RLAT(1)+cos(rloca1)*RLAT(2)
C
C-----shorten Y-coordinate according to TLTANG
C
      rloca2=TLTANG*PI/180.0
C
      RNLAT(2)=RNLAT(2)*cos(rloca2)
C
C-----rotate vector RNLAT around TLTAXIS degrees
C
      RLAT(1)= cos(rloca1)*RNLAT(1)+sin(rloca1)*RNLAT(2)
      RLAT(2)=-sin(rloca1)*RNLAT(1)+cos(rloca1)*RNLAT(2)
C
      RETURN
      END
C
C===========================================================================
C
      SUBROUTINE reciproc(rrealu,rrealv,RLATH,RLATK,ISIZE)
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
      DIMENSION rrealu(2),rrealv(2),RLATH(2),RLATK(2)
C
      rfact = real(ISIZE)/abs(rrealu(1)*rrealv(2)-rrealu(2)*rrealv(1))
      RLATH(1) =  rrealv(2) * rfact
      RLATH(2) = -rrealv(1) * rfact
      RLATK(1) = -rrealu(2) * rfact
      RLATK(2) =  rrealu(1) * rfact
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



