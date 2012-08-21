      PROGRAM findlat
C     PROGRAM 2dx_findlat.for
C
C     (C) 2dx.org
C
C     version of April 28,2006
C
C     The size of the latice is determined initially with the dimension of FFT images 
C     and the real unit angle
C     The refine subroutine is modified for 2-dimensional crystal only   
C
C
      IMPLICIT NONE
C
      INTEGER IMAXN,IREPORT
      PARAMETER (IMAXN=50000)
C
C----------------IREPORT=2: Verbose logging, 
C----------------IREPORT=1: Medium logging.
C----------------IREPORT=0: Short logging.
      PARAMETER (IREPORT=1)
C
C
      integer IMAXPOS,ISIZE,ilocmag,ICOUNT,IERR,NUM
      integer i,j,k,l,IPOS,IANZ,ISPOTANZ
      integer iwasthere,flag,IANZ1,LOOP,ifirst,itesthand,ihandto
      integer IFOUND2
      real RLATORI,rmag,rpix,RX,RY,PI
      real REALAN
      real TLTAXIS,TLTANG,RMAXCOU,RCOUNT,RMAXMAG,RLOCMAG,RMSD,RMAXRMSD
      real RFX(IMAXN),RFY(IMAXN),RFV(IMAXN)
      real RFXTEMP(IMAXN),RFYTEMP(IMAXN),RFVTEMP(IMAXN)
      real RLATH(2),RLATK(2),RNLATH(2),RNLATK(2)
      real RTEST(2),RLOCEPS
      real RSPOTX(IMAXN),RSPOTY(IMAXN),X(IMAXN),Y(IMAXN)
      real RRLATH(2),RRLATK(2)
      real RGOODH(2),RGOODK(2)
      real RREALU(2),RREALV(2)
      real RLAT1H(2),RLAT1K(2)
      real RDIFFH,RDIFFK,RDIFFALL
      real RDIFF1,RDIFF2,RDIFF3,RDIFF4
      real RDIFF5,RDIFF6,RDIFF7,RDIFF8
      integer nmin,maxhk,maxspot
      integer ihand
      real rtmp1,rtmp2
      real stepsize,magvari,delta
      character * 80 cname,cline
C
      PI=3.141592654
      NUM=1
      do I = 1,2
        RLAT1H(I)=0.0
        RLAT1K(I)=0.0
      enddo
C
      write(6,'(/,'' input real lattice in A '')')
      read(5,*)rx,ry
      write(6,'('' read: '',2F12.4)')rx,ry
C
      write(6,'(/,'' input realspace angle -> REALAN '')')
      read(5,*)REALAN
      write(6,'('' read: '',F12.4)')REALAN
C
      write(6,'(/,'' input angle Xax -> tilt-axis in deg '')')
      read(5,*)TLTAXIS
      write(6,'('' read: '',F12.4)')TLTAXIS
C
      write(6,'(/,'' input tilt angle in deg '')')
      read(5,*)TLTANG
      write(6,'('' read: '',F12.4)')TLTANG
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
      write(6,'(/,'' input name of ASCII-file with peaks '')')
      read(5,'(A)')cname
      write(6,'('' read: '',A40)')cname
C
      write(6,'(/,'' input AngleStepSize,MagVariation,Nmin,'',
     1    ''Delta,MaxHK,MaxSpot,itesthand '')')
      write(6,'(''(itesthand=1 for testing inverted hand.)'')')
      read(5,*)stepsize,magvari,nmin,delta,maxhk,maxspot,itesthand
      if(maxspot.gt.80)maxspot=80
      write(6,'('' read: '',2F12.3,I5,F12.6,3I6,/)')
     1    stepsize,magvari,nmin,delta,maxhk,maxspot,itesthand
C
      ifirst = 0
C
      open(11,FILE=CNAME,STATUS='OLD',ERR=900)
C
C      read(11,'(A)')
      I=1
  100 continue
        read(11,'(A79)',ERR=109,END=110)cline
        read(cline,*,ERR=101)RFX(I),RFY(I),RFV(I)
        goto 104
  101   continue
          if(ifirst.eq.0)then
            write(*,'('':: ERROR: There is a problem with '',
     1        A40)')CNAME
            write(*,'('':: ERROR reading input file, but ignoring'')')
            ifirst = 1
          else
            write(*,'('': ERROR reading input file, but ignoring'')')
          endif
          read(cline,*,ERR=109)RFX(I),RFY(I)
          RFV(I)=1.0-I*0.01
          if(RFV(I).lt.0.0)RFV(I)=0.0
  104   continue
C-------All reflexes in the right half of the screen
         if(RFX(I).lt.0.0)then
           RFX(I)=-RFX(I)
           RFY(I)=-RFY(I)
          endif
C-------eliminate douplicates due to Friedel symmetry
        iwasthere=0
        do j=1,I-1
          if((abs(RFX(j)-RFX(I)).lt.delta) .and.
     1       (abs(RFY(j)-RFY(I)).lt.delta) .and.
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
        if ( I.lt.IMAXN ) then
          goto 100
        else
          write(*,'('':: WARNING: too many spots in 2dx_findlat.'')')
          write(*,'('':: Truncating.'')')
          goto 110
        endif
C
  109 continue
        write(*,'('':: ERROR reading input file'')')
        goto 999
C
  110 continue
      IANZ=I-1
      IANZ1=I-1
      if(IANZ.gt.maxspot) IANZ=maxspot
C
      if(IANZ.lt.5)then
        write(6,'('':: Not enough spots:'',I5)')IANZ
        goto 999
      endif
C
      do I=1,IANZ
        write(6,'('' read spot: '',3F12.3)')
     1    RFX(I),RFY(I),RFV(I)
      enddo
C
      write(6,'(/,'' Number of peaks read is '',I10)')IANZ
C
      close(11)
C
      write(*,'(/,'' Opening new file 2dx_findlat.out.'')')
      open(12,FILE='2dx_findlat.out',STATUS='NEW',ERR=900)
C
C-----Currently, only two lattices are determined.
C
      IFOUND2 = 0
C
      do 850 LOOP=1,20
C
        RMAXCOU=0
C
C-------Loop over lattice orientation 
C
     
        if(itesthand.eq.1) then
          ihandto = 2
        else
          ihandto = 1
        endif
C
        do 310 ihand=1,ihandto
C
          IMAXPOS=0
          write(6,'(/,'' testing hand '',I3)')ihand
C
C---------Prepare non-rotated hypothetical lattice
C
          do 300 ilocmag = -10,10
C
            RLOCMAG=rmag*(1.0+ilocmag*magvari)
C
            if(IREPORT.gt.0)then               
              write(6,'('' testing magnification '',F12.3)')RLOCMAG
            endif
C
C-----------calculate real-space lattice (u,v)
C
C-----------This would produce a real-space lattice with u in X-axis, v up.
           RREALU(1) = (rx*RLOCMAG)/(rpix*10000.0)
           RREALU(2) = 0.0
           RREALV(1) = (ry*RLOCMAG*cos(REALAN*pi/180.0))
     .                 /(rpix*10000.0)
           RREALV(2) = (ry*RLOCMAG*sin(REALAN*pi/180.0))
     .                 /(rpix*10000.0)
C
C-----------Instead, we use a real-space lattice with v in Y-axis, and x somewhere right.
C-----------This then gives a reciprocal lattice with H in X-axis direction.
C           RREALU(1) = -(rx*RLOCMAG*sin(REALAN*pi/180.0))/
C     .        (rpix*10000.0)
C            RREALU(2) = 0
C            RREALV(1) = 0.0
C            RREALV(2) = (ry*RLOCMAG*sin(REALAN*pi/180.0))/
C     .        (rpix*10000.0)
C
            if ( ihand.eq.2 ) then
              rtmp1 = RREALU(1)
              rtmp2 = RREALU(2)
              RREALU(1) = RREALV(1)
              RREALU(2) = RREALV(2)
              RREALV(1) = rtmp1
              RREALV(2) = rtmp2
            endif
C
C-----------calculate reciprocal lattice
C
            call reciproc(RREALU,RREALV,RLATH,RLATK,ISIZE)
C
            if(IREPORT.gt.0)then               
              write(6,'('' real_space lattice is '',4F12.3)')
     1        RREALU(1),RREALU(2),RREALV(1),RREALV(2)
              write(6,'('' reciprocal lattice is '',4F12.3)')
     1        RLATH(1),RLATH(2),RLATK(1),RLATK(2)
            endif
C
            IPOS=1
C
 200        continue
C
C-------------rotate hypothetical lattice
C
              RLATORI=IPOS*stepsize
C
              call vecrot(RLATH,RLATORI,RNLATH)
              call vecrot(RLATK,RLATORI,RNLATK)
C
C-------------distort hypothetical lattice according to tilt geometry
C
              call vecdist(RNLATH,TLTAXIS,TLTANG)
              call vecdist(RNLATK,TLTAXIS,TLTANG)
C
C-------------Test, if the  first lattice vector is in the right half of the FFT
C
              if(RNLATH(1).lt.0.0) then
                IPOS=IPOS+1
                RLATORI=IPOS*stepsize
                if(RLATORI.LT.360.0)then
                  goto 200
                else
                  goto 300
                endif
              endif
C
C-------------Test, how many spots fit into this lattice
C
              ISPOTANZ=0
              RCOUNT=0.0
              ICOUNT=0
              RMSD=0.0
              do i=-maxhk,maxhk
                do j=-maxhk,maxhk
                  RTEST(1)=RNLATH(1)*i+RNLATK(1)*j
                  RTEST(2)=RNLATH(2)*i+RNLATK(2)*j
                  RLOCEPS=delta*SQRT(1.0*(i*i+j*j))
                  do k=1,IANZ
                    if((abs(RFX(k)-RTEST(1)).lt.RLOCEPS).and.
     1                 (abs(RFY(k)-RTEST(2)).lt.RLOCEPS)     )then
                      ICOUNT=ICOUNT+1
                      RMSD=RMSD+(RFX(k)-RTEST(1))*(RFX(k)-RTEST(1))
     1                         +(RFY(k)-RTEST(2))*(RFY(k)-RTEST(2))
C                      RCOUNT=ICOUNT
C                       RCOUNT=RCOUNT+((RFV(k)-RFV(IANZ))
C     1                         *(RFV(k)-RFV(IANZ)))
                       RCOUNT=RCOUNT+RFV(k)
                    endif
                  enddo
                enddo
              enddo
              RMSD=sqrt(RMSD/ICOUNT)
C
C-------------Refine this lattice
C
              if(ICOUNT.gt.nmin)then
C
C---------------Generate spotlist for this lattice
C
                l=1
                do i=-maxhk,maxhk
                  do j=-maxhk,maxhk
                    RTEST(1)=RNLATH(1)*i+RNLATK(1)*j
                    RTEST(2)=RNLATH(2)*i+RNLATK(2)*j
                    RLOCEPS=delta*SQRT(1.0*(i*i+j*j))
                    do k=1,IANZ
                      if((abs(RFX(k)-RTEST(1)).lt.RLOCEPS).and.
     1                   (abs(RFY(k)-RTEST(2)).lt.RLOCEPS)     )then
                        RSPOTX(l)=RFX(k)
                        RSPOTY(l)=RFY(k)
                        l=l+1
                        if(l.GT.IMAXN)then
                          write(*,'(''::ERROR: ISPOTANZ dimensions'',
     .                        '' too small'',
     .                        '' in 2dx_findlattice.exe'')')
                          stop
                        endif
                      endif
                    enddo
                  enddo
                enddo
                ISPOTANZ=l-1
C
C---------------Refine lattice according to ISPOTANZ spots in RSPOTX/Y
C
                if(ISPOTANZ.GT.nmin)then
C                  write(6,'(/,
C     1              '' reciprocal local lattice before refine is '',
C     .               4F12.3)')
C     2              RNLATH(1),RNLATH(2),RNLATK(1),RNLATK(2)
C
                  call refine(RSPOTX,RSPOTY,ISPOTANZ,
     1              RNLATH,RNLATK,RRLATH,RRLATK,IERR)
C
                  if(IERR.eq.0)then
                    RNLATH(1)=RRLATH(1)
                    RNLATH(2)=RRLATH(2)
                    RNLATK(1)=RRLATK(1)
                    RNLATK(2)=RRLATK(2)
C
                    if(IREPORT.eq.2)then               
                      write(6,'(/,
     1                  '' lat after 1st refine is '',4F9.3,
     2                  '' RCOUNT='',F12.3)')
     3                  RNLATH(1),RNLATH(2),RNLATK(1),RNLATK(2),RCOUNT 
                    endif
                  endif
C
C-----------------Determine the score again
C
                  RCOUNT=0.0
                  ICOUNT=0
                  RMSD=0.0
                  l=1
                  do i=-maxhk,maxhk
                    do j=-maxhk,maxhk
                      RTEST(1)=RNLATH(1)*i+RNLATK(1)*j
                      RTEST(2)=RNLATH(2)*i+RNLATK(2)*j
                      do k=1,IANZ
                        if((abs(RFX(k)-RTEST(1)).lt.delta).and.
     1                   (abs(RFY(k)-RTEST(2)).lt.delta)     )then
                          ICOUNT=ICOUNT+1
                          RMSD=RMSD+(RFX(k)-RTEST(1))*(RFX(k)-RTEST(1))
     1                             +(RFY(k)-RTEST(2))*(RFY(k)-RTEST(2))
C                          RCOUNT=ICOUNT
C                          RCOUNT=RCOUNT+((RFV(k)-RFV(IANZ))
C     1                         *(RFV(k)-RFV(IANZ)))
                          RCOUNT=RCOUNT+RFV(k)
                          RSPOTX(l)=RFX(k)
                          RSPOTY(l)=RFY(k)
                          l=l+1
                          if(l.GT.IMAXN)then
                           write(*,'(''::WARNING: IMAXN dimensions'',
     .                          '' too small'',
     .                          '' in 2dx_findlattice.exe,'',
     .                          '' or delta too large.'')')
                            write(*,'(''l = '',I15)')l
                            write(*,'(''IMAXN = '',I15)')IMAXN
                            write(*,'(''delta    = '',F15.3)')delta
                            goto 734
                          endif
                        endif
                      enddo
                    enddo
                  enddo
 734              continue
                  ISPOTANZ=l-1
                  RMSD=sqrt(RMSD/ICOUNT)
C
C-----------------Refine lattice again with ISPOTANZ spots in RSPOTX/Y
C
                  if(ISPOTANZ.GT.nmin)then
                    call refine(RSPOTX,RSPOTY,ISPOTANZ,
     1                RNLATH,RNLATK,RRLATH,RRLATK,IERR)
C
                    if(IERR.eq.0)then
                      RNLATH(1)=RRLATH(1)
                      RNLATH(2)=RRLATH(2)
                      RNLATK(1)=RRLATK(1)
                      RNLATK(2)=RRLATK(2)
C                 
                      if(IREPORT.eq.2)then               
                        write(6,'(
     1                    '' lat after 2nd refine is '',4F9.3,
     2                    '' RCOUNT='',F12.3)')
     3                    RNLATH(1),RNLATH(2),RNLATK(1),
     4                    RNLATK(2),RCOUNT 
                      endif
                    else
                      if(IREPORT.gt.0)then               
                        write(6,'(
     1                    '' ERROR in 2nd refine for '',4F9.3,
     2                    '' RCOUNT='',F12.3)')
     3                    RNLATH(1),RNLATH(2),RNLATK(1),
     4                    RNLATK(2),RCOUNT 
                      endif
                    endif
                  else
                    if(IREPORT.eq.2)then               
                      write(6,'(
     1                  '' not enough spots ('',I2,
     2                  '') for 2nd refine for '',4F9.3,
     3                  '' RCOUNT='',F12.3)')
     4                  ISPOTANZ,RNLATH(1),RNLATH(2),
     5                  RNLATK(1),RNLATK(2),RCOUNT
                    endif
                  endif
                endif
              endif
C
C-------------Evaluate the maximum and save 
C
              if(RCOUNT.gt.RMAXCOU)then
                RMAXCOU=RCOUNT
                IMAXPOS=IPOS
                RMAXRMSD=RMSD
                RMAXMAG=RLOCMAG
                RGOODH(1)=RNLATH(1)
                RGOODH(2)=RNLATH(2)
                RGOODK(1)=RNLATK(1)
                RGOODK(2)=RNLATK(2)
                NUM=ISPOTANZ
                do 290 i=1,ISPOTANZ
                  X(i)=RSPOTX(i)
                  Y(i)=RSPOTY(i)
290             enddo
C
                write(6,'('' Score = '',F12.4,'',  Lat='',4F10.3,
     1                    '' Mag='',F10.1)')
     2            RCOUNT,RNLATH(1),RNLATH(2),RNLATK(1),RNLATK(2),RLOCMAG
C
C                 write(6,'(/,'' Score = '',I9,'',  Lat='',4F12.3)')
C     2            ICOUNT,RNLATH(1),RNLATH(2),RNLATK(1),RNLATK(2)
C
              endif
C        
              IPOS=IPOS+1
              RLATORI=IPOS*stepsize
              if(RLATORI.LT.360.0)goto 200
            continue
C
 300      continue
 310    continue
C
C-------Report best lattice:
C
        write(6,'(/,/,'': best lattice was at position '',I8,
     1      '' with score of '',F12.3)')IMAXPOS,RMAXCOU
C
        write(6,'(/,'': reciprocal local lattice is '',4F12.3)')
     1      RGOODH(1),RGOODH(2),RGOODK(1),RGOODK(2)
C
C-------Prepare non-rotated hypothetical lattice for original picture dimensions
C
         RLATH(1)=RGOODH(1) 
         RLATH(2)=RGOODH(2) 
         RLATK(1)=RGOODK(1) 
         RLATK(2)=RGOODK(2) 
C
        write(6,'(/,'' reciprocal lattice for original image is '',
     1    4F12.3)')RLATH(1),RLATH(2),RLATK(1),RLATK(2)
C
        write(6,'('' MaxRMSD is '',F12.3)')RMAXRMSD
C
C-------First vector on the right side:
        if(RLATH(1).lt.0.0)then
          RLATH(1) = -RLATH(1) 
          RLATH(2) = -RLATH(2) 
          RLATK(1) = -RLATK(1) 
          RLATK(2) = -RLATK(2) 
        endif
C
C-------Check if this is the second lattice, and if it is the same as before
C
        if ( LOOP .eq. 1 ) then
          RLAT1H(1) = RLATH(1)
          RLAT1H(2) = RLATH(2)
          RLAT1K(1) = RLATK(1)
          RLAT1K(2) = RLATK(2)
        else
          RDIFF1 = sqrt(   (RLAT1H(1)-RLATH(1))**2 
     1                   + (RLAT1H(2)-RLATH(2))**2 )
          RDIFF2 = sqrt(   (RLAT1K(1)-RLATK(1))**2 
     1                   + (RLAT1K(2)-RLATK(2))**2 )
          RDIFF3 = sqrt(   (RLAT1K(1)-RLATH(1))**2 
     1                   + (RLAT1K(2)-RLATH(2))**2 )
          RDIFF4 = sqrt(   (RLAT1H(1)-RLATK(1))**2 
     1                   + (RLAT1H(2)-RLATK(2))**2 )
          RDIFF5 = sqrt(   (RLAT1H(1)+RLATH(1))**2 
     1                   + (RLAT1H(2)+RLATH(2))**2 )
          RDIFF6 = sqrt(   (RLAT1K(1)+RLATK(1))**2 
     1                   + (RLAT1K(2)+RLATK(2))**2 )
          RDIFF7 = sqrt(   (RLAT1K(1)+RLATH(1))**2 
     1                   + (RLAT1K(2)+RLATH(2))**2 )
          RDIFF8 = sqrt(   (RLAT1H(1)+RLATK(1))**2 
     1                   + (RLAT1H(2)+RLATK(2))**2 )
          if ( RDIFF1.lt.3 .or. RDIFF2.lt.3 .or. 
     .         RDIFF3.lt.3 .or. RDIFF4.lt.3 .or.
     .         RDIFF5.lt.3 .or. RDIFF6.lt.3 .or.
     .         RDIFF7.lt.3 .or. RDIFF8.lt.3) then
            write(6,'(/,'': This second lattice is the same as '',
     1        ''the first one. It is deleted'')')
            RLATH(1) = 0.0
            RLATH(2) = 0.0
            RLATK(1) = 0.0
            RLATK(2) = 0.0
            RMAXCOU = 0.0
          else
            IFOUND2 = 1
          endif
        endif
C
C-------Output the final lattice
C
        if(RMAXCOU.ne.0.0)then
          write(12,'(4F12.3)')
     1      RLATH(1),RLATH(2),RLATK(1),RLATK(2)
C
          write(12,'(F12.3)')
     1      RMAXCOU
        endif
C
C-------For the second lattice:
C-------Find the spots that are not on the first determined lattice.
      
        l=1
        do k=1,IANZ1 
          flag=1
          do j=1,NUM
            if((abs(RFX(k)-X(j)).lt.delta).and.
     1         (abs(RFY(k)-Y(j)).lt.delta)     )then
C              write(6,'(/,/,'' PEAK SPOT at position '',
C     1           2F12.3)')RFX(k),RFY(k)
              flag=0
            endif
          enddo
          if(flag.eq.1) then
            RFXTEMP(l)=RFX(k)
            RFYTEMP(l)=RFY(k)
            RFVTEMP(l)=RFV(k)
            l=l+1
          endif     
        enddo
C
C-------Now redo the entire lattice finding, with the remaining spots
C
        IANZ=l-1
        if(IANZ.gt.maxspot) IANZ=maxspot
        do k=1,IANZ
          RFX(k)=RFXTEMP(k)
          RFY(k)=RFYTEMP(k)
          RFV(k)=RFVTEMP(k)
        enddo    
C
        if (IFOUND2.eq.1) then
          goto 999
        endif
 850  continue
C
      if (IFOUND2.eq.0)then
C-------Output the zero second lattice
C
        write(12,'(4F12.3)')
     1    RLATH(1),RLATH(2),RLATK(1),RLATK(2)
C
        write(12,'(F12.3)')
     1    RMAXCOU
C
C-------For the second lattice:
C-------Find the spots that are not on the first determined lattice.
      endif
C
      goto 999
C
 900    continue
          STOP ':: ERROR on file open'
 999    continue
C  
      close(12)
      STOP
      END
C
C===========================================================================
C
      SUBROUTINE vecrot(RLAT,RLATORI,RNLAT)
C
      dimension RLAT(2),RNLAT(2)
C
      RLOCANG=RLATORI*3.141592657/180.0
C
C       write(6,'('' RLATORI '',F)')RLATORI
C       write(6,'('' RLAT    '',2F)')RLAT(1),RLAT(2)
C
      RNLAT(1)=cos(RLOCANG)*RLAT(1)-sin(RLOCANG)*RLAT(2)
      RNLAT(2)=sin(RLOCANG)*RLAT(1)+cos(RLOCANG)*RLAT(2)
C
C     write(6,'('' RNLAT   '',2F)')RNLAT(1),RNLAT(2)
C
      RETURN
      END
C
C===========================================================================
C
      SUBROUTINE vecdist(RLAT,TLTAXIS,TLTANG)
C
      dimension RLAT(2)
      real RNLAT(2),tmp1
C
      PI=3.141592654
C
      RLOCA1=TLTAXIS*PI/180.0
C
C-----rotate vector RNLAT around -TLTAXIS degrees
C
      RNLAT(1)= cos(RLOCA1)*RLAT(1)+sin(RLOCA1)*RLAT(2)
      RNLAT(2)=-sin(RLOCA1)*RLAT(1)+cos(RLOCA1)*RLAT(2)
C
C-----extend Y-coordinate according to TLTANG
C
      RLOCA2=TLTANG*PI/180.0
      tmp1=cos(RLOCA2)
C
      if(tmp1.ne.0.0)then
        RNLAT(2)=RNLAT(2)/tmp1
      else
        STOP ':: ERROR in 2dx_latfind: illegal angle.'
      endif
C
C-----rotate vector RNLAT around TLTAXIS degrees
C
      RLAT(1)= cos(RLOCA1)*RNLAT(1)-sin(RLOCA1)*RNLAT(2)
      RLAT(2)= sin(RLOCA1)*RNLAT(1)+cos(RLOCA1)*RNLAT(2)
C
      RETURN
      END
C
C===========================================================================
C
      SUBROUTINE reciproc(RREALU,RREALV,RLATH,RLATK,ISIZE)
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
      DIMENSION RREALU(2),RREALV(2),RLATH(2),RLATK(2)
      real tmp1
C
      tmp1 = abs(RREALU(1)*RREALV(2)-RREALU(2)*RREALV(1))
      if(tmp1.ne.0.0)then
        RFACT = real(ISIZE)/tmp1
      else
        STOP ':: ERROR in 2dx_latfind: illegal angle.'
      endif
      RLATH(1) =  RREALV(2) * RFACT
      RLATH(2) = -RREALV(1) * RFACT
      RLATK(1) = -RREALU(2) * RFACT
      RLATK(2) =  RREALU(1) * RFACT
C
      RETURN
      END
C
C===========================================================================
C
      SUBROUTINE refine(RSPOTX,RSPOTY,ISPOTANZ,
     1                  RNLATH,RNLATK,RRLATH,RRLATK,IERR)
C
      IMPLICIT NONE
C
      real RSPOTX(*),RSPOTY(*)
      real RNLATH(2),RNLATK(2),RRLATH(2),RRLATK(2)
C
      logical BAS2
      real VCMPS(2,2),C(4),CSAVE(4)
C
      integer IERR,I,J,N,M,IRANK,ISPOTANZ
      real U1,U2,V1,V2,D,XI,X,YI,Y,R,R2,RN,RM
C
      IERR=0
      RRLATH(1)=0.0
      RRLATH(2)=0.0
      RRLATK(1)=0.0
      RRLATK(2)=0.0
C
      do I=1,4
        C(I)=0.0
      enddo
      do I=1,2
        do J=1,2
          VCMPS(I,J)=0.0
        enddo
      enddo
C
      U1=RNLATH(1)
      U2=RNLATH(2)
      V1=RNLATK(1)
      V2=RNLATK(2)
C
      D=U1*V2-U2*V1
C
      if(D.eq.0.0)then
        STOP ':: ERROR in 2dx_latfind: illegal angle.'
      endif
C
C-----write(6,'('' Determinat D = '',F12.4)')D
C
      DO 80 I=1,ISPOTANZ
        XI=RSPOTX(I)
        X=XI
        YI=RSPOTY(I)
        Y=YI
        R2=X*X+Y*Y
C
C-------write(*,'('' I='',I4,'', X='',F9.3,'', Y='',F9.3)')I,XI,YI
C
C Map into lattice coordinates
        RM=(X*V2-Y*V1)/D
        RN=(Y*U1-X*U2)/D
C Note mapped displacements in DX,DY; raw in X,Y and RB3,RB5
        M=NINT(RM)
        N=NINT(RN)
C        DX=ABS(REAL(M)-RM)
C        DY=ABS(REAL(N)-RN)
        RM=REAL(M)
        RN=REAL(N)
C        X=XI-(RM*U1+RN*V1)
C        Y=YI-(RM*U2+RN*V2)
C
        C(1)=C(1)+RM*RM
        C(2)=C(2)+RM*RN
C        C(3)=C(3)+RM
        VCMPS(1,1)=VCMPS(1,1)+XI*RM
        VCMPS(1,2)=VCMPS(1,2)+YI*RM
        C(4)=C(4)+RN*RN
C        C(6)=C(6)+RN
        VCMPS(2,1)=VCMPS(2,1)+XI*RN
        VCMPS(2,2)=VCMPS(2,2)+YI*RN
C        VCMPS(3,1)=VCMPS(3,1)+XI
C        VCMPS(3,2)=VCMPS(3,2)+YI
C
   80 CONTINUE
C
C Fill in remaining elements of coefficient array (by symmetry)
C
       C(3)=C(2)
C      C(7)=C(3)
C      C(8)=C(6)
C      C(9)=REAL(ISPOTANZ)
C
C Solve for X and Y components in turn
C - Keep copy of coefficients for 2nd time
C
      DO 90 I=1,4
        CSAVE(I)=C(I)
C.......write(6,'('' C('',I1,'')='',G12.3)')I,C(I)
   90 CONTINUE
C
      IRANK=2
      IF (BAS2(C,VCMPS,2,IRANK)) GOTO 900
      IF (BAS2(CSAVE,VCMPS(1,2),2,IRANK)) GOTO 900
C
C Return lattice vectors
C
      RRLATH(1)=VCMPS(1,1)
      RRLATH(2)=VCMPS(1,2)
      RRLATK(1)=VCMPS(2,1)
      RRLATK(2)=VCMPS(2,2)
C
      goto 999
C
 900  continue
C.....write(6,'('' ERROR occured.'')')
      IERR=1
C
 999  continue
C
      RETURN
      END
C
C===========================================================================
C
      LOGICAL FUNCTION BAS2(A,B,MA,M)
C
C Solves linear equations A.X=B using Gaussian elimination
C and back-substitution, but without any pivoting
C - solution replaces B, and A is used as workspace
C - FALSE returned unless singular or pivot vanishes
C - NB Coefficient ordering is transposed wrt normal matrix notation
C
      INTEGER M,MA
      REAL A(MA,*),B(*)
C
      REAL F,X
      INTEGER I,J,JP,K
C
C Elimination
C
      BAS2 = .FALSE.
      DO 30 J=2,M
         JP=J-1
         X=A(JP,JP)
         IF (X.EQ.0.) GOTO 70
         DO 20 K=J,M
            F=A(JP,K)/X
            DO 10 I=J,M
               A(I,K)=A(I,K)-F*A(I,JP)
   10       CONTINUE
            B(K)=B(K)-F*B(JP)
   20    CONTINUE
   30 CONTINUE
C
C Back substitution
C
      X=0.
      J=M
      GOTO 60
C
   40 JP=J+1
      X=0.
      DO 50 I=JP,M
         X=X+A(I,J)*B(I)
   50 CONTINUE
   60 F=A(J,J)
      IF (F.NE.0.) THEN
         B(J)=(B(J)-X)/F
         IF (J.EQ.1) GOTO 80
         J=J-1
         GOTO 40
      ENDIF
C
C Pivot or determinant vanishes
C
   70 BAS2=.TRUE.
   80 RETURN
C
      END

