C
      PROGRAM HPREP
C
C     Henning, Dec. 4, 2002
C
C     INPUT:
C     1    filename-core ! core of name of big input file
C     2    Size of sub-windows
C
C
C
      PARAMETER (LOCDIMMAX=2000)
      PARAMETER (LMAX=20100)
      PARAMETER (LOCLINEMAX=320)
C
      DIMENSION TITLE(20),NXYZ(3),MXYZ(3),NXYZ1(3),NXYZST(3)
C
      REAL rave(LOCDIMMAX,LOCDIMMAX),rstd(LOCDIMMAX,LOCDIMMAX)
      REAL ALINE(LMAX),BLINE(LMAX)
      REAL RFIELD(LMAX,LOCLINEMAX)
C
      REAL DMIN,DMAX,DMEAN
C
      REAL*8 DOUBLMEAN
C
      CHARACTER*200 INFILE,OUTFILE,cline
C
      COMMON //NX,NY,NZ,IXMIN,IYMIN,IZMIN,IXMAX,IYMAX,IZMAX
C
      DIMENSION LABELS(20,10)
C
C-----Constant values:
C
      istep = 32
C      ioutsize = 8192
C
      WRITE(6,1)
1     FORMAT(/,' 2dx_prep : Program to treat raw scanned images.',/,/)
      WRITE(6,9003)
9003  FORMAT(' Core of the name of the MRC input file ? ')
      READ(5,'(A80)') cline
      call shorten(cline,k)
      write(INFILE,'(A,''.mrc'')')cline(1:k)
      call shorten(INFILE,k)
      WRITE(6,'(A)') INFILE(1:k)
C
      write(6,'('' Input size of smaller windows'')')
      read(*,*)ioutsize
      write(*,'('' Read: '',I6)')ioutsize
C
      CALL IMOPEN(1,INFILE,'RO')
C
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C
C-----Initialize dimensions
C
      nbigx = NXYZ(1)
      nbigy = NXYZ(2)
C
      iadd = 0
      if(nbigx.lt.ioutsize .or. nbigy.lt.ioutsize)then
	write(6,'('' WARNING: picture smaller than '',I8)')ioutsize
	write(6,'('' Will add average pixels on sides'')')
        iadd = 1
      endif
C
      NX=NXYZ(1)
      NY=NXYZ(2)
      NZ=NXYZ(3)
C
      locdimx = nbigx / istep
      locdimy = nbigy / istep
C
      write(6,'('' Initial picture dimension = '',2I8)')
     1  nbigx,nbigy
      write(6,'('' Internal picture dimension = '',2I8)')
     1  locdimx,locdimy
C
      if(locdimx.gt.LOCDIMMAX .or. locdimy.gt.LOCDIMMAX)then
	write(6,'('' ARRAY dimensions '',I8,'' too small.'')')
     1    LOCDIMMAX
	goto 999
      endif
C
      do j = 1,locdimy
        do i = 1,locdimx
          rave(i,j)=0.0
          rstd(i,j)=0.0
        enddo
      enddo
C
C----------------------------------------------------
C-----Calculate local average and standard deviation
C----------------------------------------------------
C
      jline = 0
      do j = 1,32
        CALL IRDLIN(1,RFIELD(1,j),*9400)
        jline = jline + 1
      enddo
 100  continue
C
        jend = LOCLINEMAX
        if(jend.gt.nbigy-jline)jend=nbigy-jline
C
C	write(6,'('' Reading strip until '',I8)')jline
C
        jlinestart = jline
        do j = 33,jend
          CALL IRDLIN(1,RFIELD(1,j),*9400)
          jline = jline + 1
	enddo
C
C-------Calculate local ave and std at position for this strip
        do j = 1,jend-32,istep
C
          jloc = ((j+jlinestart)*locdimy)/nbigy
C
C	  write(6,'('' Calc ave '',I8,''/'',I8,'' j='',I8)')
C     1      jloc,jlinestart,j
C
          do i = 33,nbigx-32,istep
            iloc = (i*locdimx)/nbigx
	    call calcave(RFIELD,i,j,rave,rstd,iloc,jloc)
          enddo
        enddo
C-------Copy last 32 lines to beginning of this strip
        do j=1,32
	  do i=1,nbigx
	    RFIELD(i,j)=RFIELD(i,j+LOCLINEMAX-32)
          enddo
        enddo
      if(jline.lt.nbigy-32) goto 100
C
C---------------------------------------------
C-----Calculate average of local ave and std
C---------------------------------------------
C
      ravemin = -999999999.9
      ravemax =  999999999.9
      rstdmin = -999999999.9
      rstdmax =  999999999.9
C
      call calclocave(rave,rstd,locdimx,locdimy,raveave,rstdave,
     1           ravemin,ravemax,rstdmin,rstdmax)
C
C-----Define borders of good area
C
      ravemin = raveave * 0.5
      ravemax = raveave * 2.5
      rstdmin = rstdave * 0.1
      rstdmax = rstdave * 2.0
C
C-----Calculate average of local ave and std only in good area
C
      call calclocave(rave,rstd,locdimx,locdimy,raveave,rstdave,
     1           ravemin,ravemax,rstdmin,rstdmax)
C
C-----Define borders of good area
C
      ravemin = raveave * 0.5
      ravemax = raveave * 2.5
      rstdmin = rstdave * 0.1
      rstdmax = rstdave * 2.0
C
C-----Calculate average of local ave and std only in good area
C
      call calclocave(rave,rstd,locdimx,locdimy,raveave,rstdave,
     1           ravemin,ravemax,rstdmin,rstdmax)
C
C-----Define borders of good area
C
      ravemin = raveave * 0.5
      ravemax = raveave * 2.5
      rstdmin = rstdave * 0.1
      rstdmax = rstdave * 2.0
C
C-----Select and mask good area of original picture
C
      write(6,'(/,'' Limits are:'')')
      write(6,'('' average: '',2F14.1)')ravemin,ravemax
      write(6,'('' std    : '',2F14.1)')rstdmin,rstdmax
C
      call threshold(rave,rstd,locdimx,locdimy,raveave,rstdave,
     1           ravemin,ravemax,rstdmin,rstdmax)
C
C-----Expand to get rid of little holes and ilands
C
      call expand(rave,rstd,locdimx,locdimy,5)
      call contra(rstd,rave,locdimx,locdimy,5)
      call contra(rave,rstd,locdimx,locdimy,3)
      call expand(rstd,rave,locdimx,locdimy,4)
C
C-----Define borders of good area
C-----Determine pictures to output
      ix=1
      iy=1
C-----Additional stripes of "itolerance" pixels will not lead to another copy
C-----of the image:
      itolerance = 300
      do i = 1,16
        if(nbigx.gt.i*ioutsize+itolerance)ix=ix+1
        if(nbigy.gt.i*ioutsize+itolerance)iy=iy+1
      enddo
C
      write(6,'('' Writing '',I2,'' x '',I2,'' pictures'')')
     1  ix,iy
C
      ipicnum = 0
      do ipicy = 1,iy
        do ipicx = 1,ix
C
C---------Rewind image
          CALL IMPOSN(1,0,0)
C
          write(6,'(''------------------------'',
     1    ''-------------------------------------------------'')')
C
          if(ix.gt.1)then
            ixoff = (ipicx-1) * (nbigx-ioutsize)/(ix-1)
          else
            ixoff = 0
            if(nbigx.lt.ioutsize)then
              ixoff = (nbigx-ioutsize)/2
            endif
          endif
          if(iy.gt.1)then
            iyoff = (ipicy-1) * (nbigy-ioutsize)/(iy-1)
          else
            iyoff = 0
            if(nbigy.lt.ioutsize)then
              iyoff = (nbigy-ioutsize)/2
            endif
          endif
C
          ipicnum=ipicnum+1
          call shorten(cline,k)
          write(OUTFILE,'(A,I2,''.mrc'')')cline(1:k),ipicnum
          call shorten(OUTFILE,k)
          if(OUTFILE(k-5:k-5).eq.' ')write(OUTFILE(k-5:k-5),'(''0'')')
C
          write(6,'(/,'':: File '',A,'' has offset of '',2I8,/)')
     1      OUTFILE(1:k),ixoff,iyoff
C
          do i=1,iyoff
            CALL IRDLIN(1,ALINE,*9500)
          enddo
C
          DMIN =  1.E10
          DMAX = -1.E10
          DMEAN = 0.0
          DOUBLMEAN = 0.0
C
          CALL IMOPEN(2,OUTFILE,'NEW')
          CALL ITRHDR(2,1)
          NXYZ1(1)=ioutsize
          NXYZ1(2)=ioutsize
          NXYZ1(3)=1
          NXYZST(1)=0
          NXYZST(2)=0
          NXYZST(3)=0
          CALL IALSIZ(2,NXYZ1,NXYZST)
C
          do IYrun = 1,ioutsize
            ily=IYrun+iyoff
            ilocy=(ily*locdimy)/nbigy
	    if(ilocy.lt.1)ilocy=1
            if(ilocy.gt.locdimy)ilocy=locdimy
            iouty=(IYrun*locdimy)/nbigy
	    if(iouty.lt.1)iouty=1
            if(iouty.gt.locdimy)iouty=locdimy
            DO IXrun = 1,ioutsize
              ilx=IXrun+ixoff
              ilocx=(ilx*locdimx)/nbigx
	      if(ilocx.lt.1)ilocx=1
              if(ilocx.gt.locdimx)ilocx=locdimx
              ioutx=(IXrun*locdimx)/nbigx
	      if(ioutx.lt.1)ioutx=1
              if(ioutx.gt.locdimx)ioutx=locdimx
C
              rstd(ioutx,iouty)=rave(ilocx,ilocy)
C
            enddo
          enddo
C
          itmpdimx = (locdimx*ioutsize)/nbigx
          itmpdimy = (locdimy*ioutsize)/nbigy
C
          call taper(rstd,itmpdimx,itmpdimx,1)
C
          idiff = nbigx-ioutsize
C
          ibadcount = 0
          DO IYrun = 1,ioutsize
            iouty=(IYrun*locdimy)/nbigy
	    if(iouty.lt.1)iouty=1
            if(iouty.gt.locdimy)iouty=locdimy
C
	    if(IYrun+iyoff.ge.1 .and.
     1         IYrun+iyoff.le.NY)then
              CALL IRDLIN(1,ALINE,*9500)
C
              if(idiff.lt.0)then
                do itmp1 = 1,idiff
                  ALINE(idiff+ioutsize) = raveave
                enddo
              endif
	    else
C.............Will be set to raveave later
            endif
C
            DO IXrun = 1,ioutsize
 	      IXloc=IXrun+ixoff
	      if(IXloc.lt.1)IXloc=1
              VAL=ALINE(IXloc)
              ioutx=(IXrun*locdimx)/nbigx
	      if(ioutx.lt.1)ioutx=1
              if(ioutx.gt.locdimx)ioutx=locdimx
C
              if(rstd(ioutx,iouty).gt.0.5 
     1           .or. IXrun+ixoff.lt.1
     2           .or. IXrun+ixoff.gt.NX
     3           .or. IYrun+iyoff.lt.1
     4           .or. IYrun+iyoff.gt.NY)then
                VAL=raveave
                ibadcount = ibadcount + 1
              endif
              IF (VAL .LT. DMIN) DMIN = VAL
              IF (VAL .GT. DMAX) DMAX = VAL
              DOUBLMEAN = DOUBLMEAN + VAL
              BLINE(IXrun) = VAL
            enddo
            CALL IWRLIN(2,BLINE)
          enddo
          DMEAN = DOUBLMEAN/(ioutsize*ioutsize)
          write(6,'(/,'' file written, DMIN,DMAX,DMEAN='',3G12.3)')
     1      DMIN,DMAX,DMEAN
          rbadcount = (100.0*ibadcount) / (ioutsize*ioutsize)
          write(6,'('' Deleted fraction was '',F7.2,''%.'',/)')
     1      rbadcount
          CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
          CALL IMCLOSE(2)
        enddo
      enddo
C
      CALL IMCLOSE(1)
C
      STOP
9400  WRITE(6,9013)
9013  FORMAT(' Error reading in the entire map at beginning')
      STOP
9500  WRITE(6,9014)
9014  FORMAT(' Error reading in the entire map at end')
      STOP
 999  continue
      STOP
      END
C
C******************************************************************************
C
      SUBROUTINE calcave(RFIELD,i,j,rave,rstd,iloc,jloc)
C
      PARAMETER (LOCDIMMAX=2000)
      PARAMETER (LMAX=20100)
      PARAMETER (LOCLINEMAX=320)
C
      REAL rave(LOCDIMMAX,LOCDIMMAX),rstd(LOCDIMMAX,LOCDIMMAX)
      REAL RFIELD(LMAX,LOCLINEMAX)
C
      real*8 rtmp1
C
C      write(6,'('' calcave called with '',4I)')
C     1  i,j,iloc,jloc
C
      rtmp1 = 0.0
      do jtmp = j-32,j+32
        jloctmp=jtmp
        if(jloctmp.lt.1)jloctmp=1
	do itmp = i-32,i+32
          rtmp1=rtmp1+RFIELD(itmp,jloctmp)         
	enddo
      enddo
C
      rave(iloc,jloc)=rtmp1/(65.0*65.0)
C
      rtmp1 = 0.0
      do jtmp = j-32,j+32
        jloctmp=jtmp
        if(jloctmp.lt.1)jloctmp=1
	do itmp = i-32,i+32
	  rdiff = RFIELD(itmp,jloctmp)-rave(iloc,jloc)
          rtmp1=rtmp1+(rdiff*rdiff)
	enddo
      enddo
C
      rstd(iloc,jloc)=sqrt(rtmp1)/(65.0*65.0-1.0)
C
      RETURN
      END
C
C******************************************************************************
C
      SUBROUTINE calclocave(rave,rstd,locdimx,locdimy,raveave,rstdave,
     1           ravemin,ravemax,rstdmin,rstdmax)
C
      PARAMETER (LOCDIMMAX=2000)
      PARAMETER (LMAX=20100)
      PARAMETER (LOCLINEMAX=320)
C
      REAL rave(LOCDIMMAX,LOCDIMMAX),rstd(LOCDIMMAX,LOCDIMMAX)
C
      INTEGER*8 iamin,iamax,ismin,ismax
C
      write(6,
     1'(/,'' ravemin/max = '',2G12.1,'' rstdmin/max = '',2G12.1)')
     1  ravemin,ravemax,rstdmin,rstdmax
C
      raveave = 0.0
      rstdave = 0.0
      icount = 0
      iamin=0
      iamax=0
      ismin=0
      ismax=0
      do j=1,locdimy
	do i=1,locdimx
          if(rave(i,j).gt.ravemin .and.
     1       rave(i,j).lt.ravemax .and. 
     2       rstd(i,j).gt.rstdmin .and.
     3       rstd(i,j).lt.rstdmax       ) then
	    raveave = raveave + rave(i,j)
	    rstdave = rstdave + rstd(i,j)
            icount = icount + 1
          else 
            if(rave(i,j).le.ravemin)iamin=iamin+1
            if(rave(i,j).ge.ravemax)iamax=iamax+1
            if(rstd(i,j).le.rstdmin)ismin=ismin+1
            if(rstd(i,j).ge.rstdmax)ismax=ismax+1
          endif
	enddo
      enddo
      if(icount.lt.1)icount=1
      raveave = raveave / icount
      rstdave = rstdave / icount
C
      write(6,'('' raveave = '',F12.1,'' rstdave = '',F12.1)')
     1  raveave,rstdave
C
      write(6,'('' AVE under/over = '',2I8)')
     1  iamin,iamax
      write(6,'('' STD under/over = '',2I8)')
     1  ismin,ismax
C
      RETURN
      END
C
C******************************************************************************
C
      SUBROUTINE threshold(rave,rstd,locdimx,locdimy,raveave,rstdave,
     1           ravemin,ravemax,rstdmin,rstdmax)
C
      PARAMETER (LOCDIMMAX=2000)
      PARAMETER (LMAX=20100)
      PARAMETER (LOCLINEMAX=320)
C
      REAL rave(LOCDIMMAX,LOCDIMMAX),rstd(LOCDIMMAX,LOCDIMMAX)
C
      INTEGER * 8 ibadcount
C
      ibadcount = 0
      do j=1,locdimy
	do i=1,locdimx
          if(rave(i,j).gt.ravemin .and.
     1       rave(i,j).lt.ravemax .and. 
     2       rstd(i,j).gt.rstdmin .and.
     3       rstd(i,j).lt.rstdmax       ) then
	    rave(i,j)=0.0
          else 
            ibadcount = ibadcount + 1
            rave(i,j)=1.0
          endif
	enddo
      enddo
      rbadcount = (100.0 * ibadcount)/(locdimx*locdimy)
C
      write(6,'(/,'' THRESHOLD: Deleted fraction of '',F7.1,''%'')')
     1  rbadcount
C
      RETURN
      END
C
C******************************************************************************
C
      SUBROUTINE expand(rave,rstd,locdimx,locdimy,idist)
C
      PARAMETER (LOCDIMMAX=2000)
      PARAMETER (LMAX=20100)
      PARAMETER (LOCLINEMAX=320)
C
      REAL rave(LOCDIMMAX,LOCDIMMAX),rstd(LOCDIMMAX,LOCDIMMAX)
C
      INTEGER * 8 ibadcount
C
      write(6,'(/,'' EXPAND called with '',I8)')idist
C
      ianf = -idist
      iend =  idist
      id2 = idist * idist
      ibadcount = 0 
      DO K=1,locdimy
        DO J=1,locdimx
          rOUT=0.0
          do ikrun=ianf,iend
            ikloc=K+ikrun
            if(ikloc.lt.1       )ikloc=1
            if(ikloc.gt.locdimy)ikloc=locdimy
            ik2 = ikrun * ikrun
            do ijrun=ianf,iend
              ijloc=J+ijrun
              if(ijloc.lt.1       )ijloc=1
              if(ijloc.gt.locdimx)ijloc=locdimx
              if(ik2+ijrun*ijrun.le.id2)then
                if(rave(ijloc,ikloc).gt.0.5)rOUT=1.0
              endif
            enddo
          enddo
          rstd(J,K)=rOUT
          if(rOUT.gt.0.5)ibadcount = ibadcount + 1
        ENDDO
      ENDDO
      rbadcount = (100.0 * ibadcount)/(locdimx*locdimy)
C
      write(6,'('' EXPAND: Deleted fraction is '',F7.1,''%'')')
     1  rbadcount
C
      RETURN
      END
C
C************************************************************************
C
      SUBROUTINE contra(rave,rstd,locdimx,locdimy,idist)
C
      PARAMETER (LOCDIMMAX=2000)
      PARAMETER (LMAX=20100)
      PARAMETER (LOCLINEMAX=320)
C
      REAL rave(LOCDIMMAX,LOCDIMMAX),rstd(LOCDIMMAX,LOCDIMMAX)
C
      INTEGER * 8 ibadcount
C
      write(6,'(/,'' CONTRA called with '',I8)')idist
C
      ianf = -idist
      iend =  idist
      id2 = idist * idist
      ibadcount = 0
      DO K=1,locdimy
        DO J=1,locdimx
          rOUT=1.0
          do ikrun=ianf,iend
            ikloc=K+ikrun
            if(ikloc.lt.1      )ikloc=1
            if(ikloc.gt.locdimy)ikloc=locdimy
            ik2 = ikrun * ikrun
            do ijrun=ianf,iend
              ijloc=J+ijrun
              if(ijloc.lt.1      )ijloc=1
              if(ijloc.gt.locdimx)ijloc=locdimx
              if(ik2+ijrun*ijrun.le.id2)then
                if(rave(ijloc,ikloc).lt.0.5)rOUT=0.0
              endif
            enddo
          enddo
          rstd(J,K)=rOUT
          if(rOUT.gt.0.5)ibadcount = ibadcount + 1
        ENDDO
      ENDDO
      rbadcount = (100.0 * ibadcount)/(locdimx*locdimy)
C
      write(6,'('' CONTRA: Deleted fraction is '',F7.1,''%'')')
     1  rbadcount
C
      RETURN
      RETURN
      END
C
C************************************************************************
C
      SUBROUTINE taper(rfield,locdimx,locdimy,idist)
C
      PARAMETER (LOCDIMMAX=2000)
      PARAMETER (LMAX=20100)
      PARAMETER (LOCLINEMAX=320)
C
      REAL rfield(LOCDIMMAX,LOCDIMMAX)
C
      INTEGER * 8 ibadcount
C
      write(6,'('' TAPER called with '',I8)')idist
C
      ibadcount = 0
      DO K=1,locdimy
        DO J=1,locdimx
          if((J.le.idist        ) .or.
     1       (J.ge.locdimx-idist) .or. 
     2       (K.le.idist        ) .or. 
     3       (K.ge.locdimy-idist)     )then
            rfield(J,K)=1.0
          endif
          if(rfield(J,K).gt.0.5)ibadcount = ibadcount + 1
        ENDDO
      ENDDO
      rbadcount = (100.0 * ibadcount)/(locdimx*locdimy)
C
      write(6,'('' TAPER: Deleted fraction of '',F7.1,''%'')')
     1  rbadcount
C
      RETURN
      END
c
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

