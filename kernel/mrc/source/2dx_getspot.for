      PROGRAM TDXGETSPOT
C
C     (C) Henning Stahlberg, 20.3.2003
C
      IMPLICIT NONE
C
      integer IMAX,IREPORT
      PARAMETER (IMAX=1000)
C
C----------------IREPORT=1: Verbose logging, 
C----------------IREPORT=0: Short logging.
      PARAMETER (IREPORT=0)
C
      integer i,j,IANZ,iwasthere,NDUM1,NDUM2,k
      integer iminrad,imaxrad
      real rux,ruy,rvx,rvy,rtestx,rtesty,PI,radius,RF
      real REPS,RFX(IMAX),RFY(IMAX),RFV(IMAX)
      character * 80 cname
C
      PI=3.141592654
C
      write(6,'(/,'' input reciprocal lattice'')')
      read(5,*)rux,ruy,rvx,rvy
      write(6,'('' read: '',4F12.4)')rux,ruy,rvx,rvy
C
      write(6,'(/,'' input name of doc-file with peaks '')')
      read(5,'(A)')cname
      write(6,'('' read: '',A40)')cname
C
      write(6,'(/,'' input tolerance radius '')')
      read(5,*)radius
      write(6,'('' read: '',F12.1)')radius
C
      write(6,'(/,'' input maximum spot index radius '')')
      read(5,*)imaxrad
      write(6,'('' read: '',I12)')imaxrad
C
      open(11,FILE=CNAME,STATUS='OLD',ERR=900)
C
      read(11,'(A)')
      REPS=3.0
      I=1
  100 continue
        read(11,*,ERR=110,END=110)RFX(I),RFY(I),RF,RFV(I)
C-------All reflexes in the right half of the screen
        if(RFX(I).lt.0.0)then
          RFX(I)=-RFX(I)
          RFY(I)=-RFY(I)
        endif
C-------eliminate douplicates due to Friedel symmetry
        iwasthere=0
        do j=1,I-1
          if((abs(RFX(j)-RFX(I)).lt.REPS) .and.
     1       (abs(RFY(j)-RFY(I)).lt.REPS) .and.
     2       (iwasthere.eq.0            )       ) then
            RFX(j)=(RFX(j)+RFX(I))/2.0
            RFY(j)=(RFY(j)+RFY(I))/2.0
            RFV(j)=(RFV(j)+RFV(I))/2.0
            iwasthere=1
          endif
        enddo
        if(iwasthere.eq.0)then
          I=I+1
        endif
      if(I.lt.IMAX)goto 100
  110 continue
      IANZ=I-1
      write(6,'(/,'' Number of peaks read is '',I10)')IANZ
C
      close(11)
C
C      if(IANZ.gt.30) IANZ=30
C
      do I=1,IANZ
        write(6,'('' read spot: '',3F12.3)')
     1    RFX(I),RFY(I),RFV(I)
      enddo
C
      write(*,'('' Searching on lattice: '',4F12.3)')rux,ruy,rvx,rvy
C
      open(11,FILE='2dx_getspot.out',STATUS='NEW',ERR=900)
C
C-----Loop over the lattice
C
      imaxrad = abs(imaxrad)
      iminrad = -imaxrad
      do i=iminrad,imaxrad
        do j=iminrad,imaxrad
          rtestx=rux*i+rvx*j
          rtesty=ruy*i+rvy*j
          iwasthere=0
          do k=1,IANZ
            if((abs(RFX(k)-rtestx).lt.radius).and.
     1         (abs(RFY(k)-rtesty).lt.radius)     )then
               iwasthere = 1
               goto 200
            endif
          enddo
 200      continue
          if(iwasthere.eq.1)then
            write(11,'(2I6)')i,j
            write(*,'('' Found lattice spot: '',2I6)')i,j
          endif
        enddo
      enddo
C
      close(11)
C
C-----Done.
C
      goto 999
 900  continue
        STOP 'ERROR on file open'
 999  continue
C
      STOP
      END

