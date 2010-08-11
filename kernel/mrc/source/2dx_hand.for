      PROGRAM MRCHAND
C
      character * 80 czeile
C
      REAL RC1(3),RC2(3),RC3(3),RC4(3)
      REAL RV1(3),RV2(3),RV3(3),RV4(3)
      REAL RAX1(3),RAX2(3),RAX3(3),RAX4(3)
      REAL RX(3),RY(3),RZ(3)
      REAL RA(3),RB(3),RC(3)
      REAL RRA(3),RRB(3),RRC(3)
      REAL RTA(3),RTB(3),RTC(3)
      REAL RRTA(3),RRTB(3),RRTC(3)
      REAL RVERTI(3),RHAXIS(3)
      REAL RTEMP1(3),RTEMP2(3),RTEMP3(3),RTEMP4(3)
      REAL MAT1(3,3)
C
      common//idebug
C
      idebug=1
      pi=3.141592654
C
      READ(*,*)RA(1),RA(2),RB(1),RB(2)
      RA(3)=0.0
      RB(3)=0.0
      call cross(RA,RB,RC)
      if(RC(3).ge.0)then
        ihand = 1
      else 
        ihand = 0
      endif
      WRITE(*,'(I1)')ihand
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
C
