      PROGRAM CALLAT
Csdlfkj
      PI=3.141592654
C
C-----Input first vector from existing rec. lattice
      read(*,*)rx1,rx2
C
C-----Input real space angle
      read(*,*)rang
C
C-----calc reciprocal angle
      rrang = 180.0 - ABS(rang)
      rrang = (rrang * PI ) / 180.0
C
C-----Input real space vector lengths
      read(*,*)rlena,rlenb
C
C-----Input version selector
      read(*,*)iversion
C
      if ( iversion.eq.2) then
        rrang = -rrang
      endif
C
      rfac = abs(rlenb / rlena)
C
      ry1 = ( RX1 * cos(rrang) - RX2 * sin(rrang) ) / rfac
      ry2 = ( RX1 * sin(rrang) + RX2 * cos(rrang) ) / rfac
C
C-----Output complete lattice
      write(*,'(4F11.4)')rx1,rx2,ry1,ry2
C
C      test = cos(rrang)
C      write(*,'(''rrang = '',F12.5)')rrang
C      write(*,'(''cos(rrang) = '',F12.5)')test
C
      STOP
      END
