      PROGRAM MRCCHECKLAT3
C
C       Henning Stahlberg, 11.2.2003
C
      real            rmag,rstepz,rphacon
      real            rcs,rkv,rdef1,rdef2,rdefan
      parameter       (ictfdim = 2048)
      real            rctf1(ictfdim),rctf2(ictfdim)
      real            M_PI
      parameter       (M_PI = 3.141592654)
      parameter       (maxthon = 100)
      integer         ithon1(maxthon),ithon2(maxthon)
C
      write(6,'('' MRCCHECKLAT3 '')')
C
      read(5,*) rlat1x,rlat1y,rlat2x,rlat2y
      write(6,'('' lattice       = '',4F12.3)')rlat1x,rlat1y,rlat2x,rlat2y
C
      read(5,*) rdef1,rdef2,rdefan
      write(6,'('' defocus       = '',3F12.3)')rdef1,rdef2,rdefan
C
      read(5,*) isize
      write(6,'('' Image size    = '',I9)')isize
C
      read(5,*) rmag
      write(6,'('' Magnification = '',F12.2)')rmag
C
      read(5,*) rstepz
      write(6,'('' Stepsize      = '',F12.3)')rstepz
C
      read(5,*) rphacon
      write(6,'('' Phasecon      = '',F12.3)')rphacon
C
      read(5,*) rcs,rkv
      write(6,'('' Cs,kV         = '',2F12.3)')rcs,rkv
C
C-----Stepsize in Angstroem
      rstepz = rstepz*10000.0
C
C-----Cs in Angstroem
      rcs = rcs * 10000000.0
C
      rvolt=1000.0*rkv
C
      if(rdefan.lt.  0.0)rdefan=rdefan+180.0
      if(rdefan.lt.  0.0)rdefan=rdefan+180.0
      if(rdefan.gt.180.0)rdefan=rdefan-180.0
      if(rdefan.gt.180.0)rdefan=rdefan-180.0
      rdefan=rdefan*M_PI/180.0
C
      ixmax=1024
      iymax=1024
C
C-----WL=rh*rc/sqrt((2*rvolt*rE0+rvolt*rvolt)*rL*rL)
C-----WL is in meters now.
C-----WL=WL*10000000000.0
C-----WL is in ANGSTROEM now.
C
      WL=12.3/SQRT(rvolt+rvolt**2/(10.0**6.0))
C
      STEPR=rstepz*isize/rmag
      THETATR=WL/STEPR
C-----THETATR is the angle of the first FFT pixel.
C
      write(6,'(/,'' WL      = '',G15.5)')WL
      write(6,'('' STEPR   = '',G15.5)')STEPR
      write(6,'('' THETATR = '',G15.5)')THETATR
      write(6,'('' '')')
C
C-----CHI=(PI*ANGLE*ANGLE/WL)*(RCS*ANGLE*ANGLE/2-rdef1)
C
C-----Fill vectors rctf1 and rctf2 with CTF values
C
      DO IX=1,ictfdim
        ANGLE=IX*THETATR
        C1=M_PI*ANGLE*ANGLE/WL
        C2=-C1*rcs*ANGLE*ANGLE/2.0
        CHI1=C1*rdef1+C2
        CHI2=C1*rdef2+C2
        rctf1(IX)=-sin(CHI1)*rphacon-cos(CHI1)*(1.0-rphacon)
        rctf2(IX)=-sin(CHI2)*rphacon-cos(CHI2)*(1.0-rphacon)
C       write(*,'('' IX,ANGLE,C1,C2,CHI,rctf ='',I6,'': '',6G12.2)')
C    1     IX,ANGLE,C1,C2,CHI1,rctf1(IX),rctf2(IX)
      enddo
C
C-----Find zero crossings in CTF oszillations
C
      IY1=1
      IY2=1
      rctf1(1)=1.0
      rctf2(1)=1.0
      DO IX=2,ictfdim
        if((rctf1(IX-1).ge.0.0 .and. rctf1(IX).lt.0.0) .or.
     1     (rctf1(IX-1).lt.0.0 .and. rctf1(IX).ge.0.0))then
          ithon1(IY1)=IX
          IY1=IY1+1
        endif
        if((rctf2(IX-1).ge.0.0 .and. rctf2(IX).lt.0.0) .or.
     1     (rctf2(IX-1).lt.0.0 .and. rctf2(IX).ge.0.0))then
          ithon2(IY2)=IX
          IY2=IY2+1
        endif
        if(IY1.ge.maxthon .or. IY2.ge.maxthon) goto 598
      enddo
 598  continue
C
      if(IY1.gt.IY2)IY1=IY2
      IY1=IY1-1
C
C-----Draw eliptical thon rings
C
      iloc=IY1
      iallthon=0
      if(iallthon.eq.0 .and. iloc.gt.4)iloc=4
      do i=1,iloc
        if(ithon1(i).lt.ixmax/2 .and. ithon2(i).lt.ixmax/2)then
          write(*,'(I6''. Thonring at '',2I8)')i,ithon1(i),ithon2(i)
        endif
      enddo
C
      rlat1x = rlat1x / 2.0
      rlat1y = - rlat1y / 2.0
      rlat2x = rlat2x / 2.0
      rlat2y = - rlat2y / 2.0
C
      open(9,FILE='B99.spi',ERR=900)
C
      ilim = 12
      rfac1 = 0.3
      rfac2 = 1.0 - rfac1
C
      write(9,'('';'')')
      write(9,'(''fs'')')
      write(9,'(''mrcchecklat001'')')
      write(9,'(''X71=X3-X4'')')
      write(9,'(''X70=X4'')')
      write(9,'('';'')')
      write(9,'(''AR'')')
      write(9,'(''mrcchecklat001'')')
      write(9,'(''_1'')')
      write(9,'(''((P1-X70)/X71)'')')
      write(9,'('';'')')
      write(9,'(''PT'')')
      write(9,'(''_2'')')
      write(9,'(I6,'','',I6)')ixmax,iymax
C
C-----Draw Thon rings
C
      write(6,'('' Writing instructions for Thon rings ...'')')
      ibordx = ixmax - 2
      ibordy = iymax - 2
      do i = 1,iloc
        rx=ithon1(i) * cos(0.0)
        ry=ithon2(i) * sin(0.0)
        sx= cos(rdefan)*rx + sin(rdefan)*ry
        sy=-sin(rdefan)*rx + cos(rdefan)*ry
        ipos2x = sx + ixmax/2+1
        ipos2y = sy + iymax/2+1
        do iang = 1,45
 	  rang = real(iang*8)*M_PI/180.0
          if(ithon1(i).lt.ixmax .and. ithon2(i).lt.ixmax)then
	    rx=ithon1(i) * cos(rang)
	    ry=ithon2(i) * sin(rang)
 	    sx= cos(rdefan)*rx + sin(rdefan)*ry
            sy=-sin(rdefan)*rx + cos(rdefan)*ry
	    ipos1x = ipos2x
	    ipos1y = ipos2y
	    ipos2x = sx + ixmax/2+1
	    ipos2y = sy + iymax/2+1
            if(ipos1x.lt.ibordx .and. ipos1y.lt.ibordy .and.
     1         ipos2x.lt.ibordx .and. ipos2y.lt.ibordy .and.
     2         ipos1x.gt.  0.0  .and. ipos1y.gt. 0.0   .and.
     3         ipos2x.gt.  0.0  .and. ipos2y.gt. 0.0         ) then
              write(9,'(''L'')')
              write(9,'(I6,'','',I6)')ipos1x+0,ipos1y+0
              write(9,'(I6,'','',I6)')ipos2x+0,ipos2y+0
              write(9,'(''Y'')')
            endif
          endif
        enddo
      enddo
C
C-----Draw lattice
C
      write(6,'('' Writing instructions for lattice ...'')')
      ibordx = ixmax - 2
      ibordy = iymax - 2
      do i = -ilim,ilim
        do j = -ilim,ilim
          ipos1x = (i+rfac1)*rlat1x + (j+0.0)*rlat2x + ixmax/2+1
          ipos1y = (i+rfac1)*rlat1y + (j+0.0)*rlat2y + iymax/2+1
          ipos2x = (i+rfac2)*rlat1x + (j+0.0)*rlat2x + ixmax/2+1
          ipos2y = (i+rfac2)*rlat1y + (j+0.0)*rlat2y + iymax/2+1
          if(ipos1x.lt.ibordx .and. ipos1y.lt.ibordy .and.
     1       ipos2x.lt.ibordx .and. ipos2y.lt.ibordy .and.
     2       ipos1x.gt.  0.0  .and. ipos1y.gt. 0.0   .and.
     3       ipos2x.gt.  0.0  .and. ipos2y.gt. 0.0         ) then
            write(9,'(''L'')')
            write(9,'(I6,'','',I6)')ipos1x+0,ipos1y+0
            write(9,'(I6,'','',I6)')ipos2x+0,ipos2y+0
            write(9,'(''Y'')')
            write(9,'(''L'')')
            write(9,'(I6,'','',I6)')ipos1x+2,ipos1y+0
            write(9,'(I6,'','',I6)')ipos2x+2,ipos2y+0
            write(9,'(''Y'')')
            write(9,'(''L'')')
            write(9,'(I6,'','',I6)')ipos1x+0,ipos1y+2
            write(9,'(I6,'','',I6)')ipos2x+0,ipos2y+2
            write(9,'(''Y'')')
            write(9,'(''L'')')
            write(9,'(I6,'','',I6)')ipos1x+1,ipos1y+1
            write(9,'(I6,'','',I6)')ipos2x+1,ipos2y+1
            write(9,'(''Y'')')
          endif
        enddo
      enddo
C
      do i = -ilim,ilim
        do j = -ilim,ilim
          ipos1x = (i+0.0)*rlat1x + (j+rfac1)*rlat2x + ixmax/2+1
          ipos1y = (i+0.0)*rlat1y + (j+rfac1)*rlat2y + iymax/2+1
          ipos2x = (i+0.0)*rlat1x + (j+rfac2)*rlat2x + ixmax/2+1
          ipos2y = (i+0.0)*rlat1y + (j+rfac2)*rlat2y + iymax/2+1
          if(ipos1x.lt.ibordx .and. ipos1y.lt.ibordy.and.
     1       ipos2x.lt.ibordx .and. ipos2y.lt.ibordy.and.
     2       ipos1x.gt.  0.0  .and. ipos1y.gt.  0.0  .and.
     3       ipos2x.gt.  0.0  .and. ipos2y.gt.  0.0        ) then
            write(9,'(''L'')')
            write(9,'(I6,'','',I6)')ipos1x+0,ipos1y+0
            write(9,'(I6,'','',I6)')ipos2x+0,ipos2y+0
            write(9,'(''Y'')')
            write(9,'(''L'')')
            write(9,'(I6,'','',I6)')ipos1x+2,ipos1y+0
            write(9,'(I6,'','',I6)')ipos2x+2,ipos2y+0
            write(9,'(''Y'')')
            write(9,'(''L'')')
            write(9,'(I6,'','',I6)')ipos1x+0,ipos1y+2
            write(9,'(I6,'','',I6)')ipos2x+0,ipos2y+2
            write(9,'(''Y'')')
            write(9,'(''L'')')
            write(9,'(I6,'','',I6)')ipos1x+1,ipos1y+1
            write(9,'(I6,'','',I6)')ipos2x+1,ipos2y+1
            write(9,'(''Y'')')
          endif
        enddo
      enddo
C
      write(9,'(''C'')')
      write(9,'(''513,513'')')
      write(9,'(''2'')')
      write(9,'(''N'')')
      write(9,'('';'')')
      write(9,'(''AR'')')
      write(9,'(''_2'')')
      write(9,'(''_3'')')
      write(9,'(''((P1-1)/(-1))'')')
      write(9,'('';'')')
      write(9,'(''MM'')')
      write(9,'(''_3'')')
      write(9,'(''_1'')')
      write(9,'(''0.0'')')
      write(9,'('';'')')
      write(9,'(''cp to tiff'')')
      write(9,'(''_1'')')
      write(9,'(''mrcchecklat.tif'')')
      write(9,'('';'')')
      write(9,'(''en'')')
C
      close(9)
C
      goto 999
 900  continue
        write(6,'('' ERROR during file open'')')
 999  continue
      STOP
      END     
