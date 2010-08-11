      PROGRAM CALLAT
C
      REAL RC1(3),RC2(3),RC3(3),RC4(3)
C
      pi=3.141592654
C
C-----Input reciprocal cell
      read(*,*)RC1(1),RC1(2),RC2(1),RC2(2)
C
      RC1(3) = 0.0
      RC2(3) = 0.0
C
      call norm(RC1)
      call norm(RC2)
C
      call cross(RC1,RC2,RC3)
      call spat(RC1,RC2,C)
C
      RTMP=abs(acos(C))*180.0/pi
      if(RTMP.gt. 90.0)RTMP=RTMP-180.0
      if(RTMP.lt.-90.0)RTMP=RTMP+180.0
      if(RC3(3).LT.0.0)RTMP=-RTMP
      RANGLE=RTMP
C
      write(*,'(''Included angle is '',F12.3)')RANGLE
      if(RC3(3).ge.0.0)then
        write(*,'(''Right-handed system'')')
      else
        write(*,'(''Left-handed system'')')
      endif
C
      STOP
      END
C
C------------------------------------------------------------------------------
C
      SUBROUTINE NORM(A)
C
C*****Normalizes the vector A
C
      dimension A(3)
C
      call leng(A,RL)
      IF(RL.GT.0.0)then
        A(1) = A(1) / RL
        A(2) = A(2) / RL
        A(3) = A(3) / RL
      ELSE
        WRITE(*,'('' WARNING: Normalize zero vector.'')')
      ENDIF
C
      return
      end
C
C------------------------------------------------------------------------------
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
C
C------------------------------------------------------------------------------
C
      SUBROUTINE SPAT(A,B,C)
C
C*****Calculates C=A.B (output is scalar)
C
      dimension A(3),B(3)
C
      C = A(1)*B(1) + A(2)*B(2) + A(3)*B(3)
C
      return
      end
C
C------------------------------------------------------------------------------
C
      SUBROUTINE LENG(A,RL)
C
C*****Calculates the length RL of Vector A
C
      dimension A(3)
C
      RL = SQRT(A(1)*A(1) + A(2)*A(2) + A(3)*A(3))
C
      return
      end
C
C------------------------------------------------------------------------------
C

