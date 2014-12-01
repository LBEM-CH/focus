      PROGRAM SPOTSCANPAD
C
C Program to merge several SpotScan spot images into one large output image.
C
C  (C) 2dx.org
C
C
      PARAMETER (LMAX=22000)
      PARAMETER (LCMX=11000)
      PARAMETER (LPIC=21000)
      PARAMETER (IMAXFILE=200)
      PARAMETER (IOUTDIM=20000)
      PARAMETER (IOUTDIM2=400000000)
C
      COMMON //NX,NY,NZ,IXMIN,IYMIN,IZMIN,IXMAX,IYMAX,IZMAX
      DIMENSION ALINE(LMAX),NXYZ(3),MXYZ(3),NXYZST(3)
      DIMENSION APIC(LPIC,LPIC)
      DIMENSION IXYZMIN(3),IXYZMAX(3),OUT(LMAX),OU2(LMAX)
      DIMENSION LABELS(20,10),CELL(6),EXTRA(29)
      DIMENSION DNCELL(6),MXYZN(3)
      COMPLEX CLINE(LCMX),COUT(LCMX),CVAL
      CHARACTER*60 INFILE,OUTFILE
      character*80 TITLE
      character*200 cfile,cname,coutfile
      character*200 cinimage(IMAXFILE)
      character*200 cinparameters(IMAXFILE)
C
      real rgridpos_x(IMAXFILE)
      real rgridpos_y(IMAXFILE)
      integer ispotdim_x(IMAXFILE)
      integer ispotdim_y(IMAXFILE)
C
      REAL*8 DOUBLMEAN,DOUBLOMEAN,DOUBLTMP
      INTEGER*4 iover,iunder,ilow
      INTEGER*8 inumber
C
      INTEGER*2 tmpimage(IOUTDIM,IOUTDIM)
      INTEGER*2 outimage(IOUTDIM,IOUTDIM)
C
      EQUIVALENCE (NX,NXYZ), (ALINE,CLINE), (OUT,COUT)
      EQUIVALENCE (IXYZMIN, IXMIN), (IXYZMAX, IXMAX)
C
      DATA NXYZST/3*0/, CNV/57.29578/
      DATA tmpimage/IOUTDIM2*0/
      DATA outimage/IOUTDIM2*0/
C
      write(6,'(/,/,''2dx_SpotScan_Pad: Merge several SpotScan images'',
     .  '' into one'')')
C
      write(6,'(/,''Input name for output file'')')
      read(5,'(A)')coutfile
      call shorten(coutfile,k)
      write(6,'(''Read: '',A)')coutfile(1:k)
C
      imagenumber = 1
C
 100  continue
C   
        write(6,'(/,''Input name for input image file'')')
        read(5,'(A)',END=110,ERR=901)cinimage(imagenumber)
        call shorten(cinimage(imagenumber),k)
        write(6,'(''Read: '',A)')cinimage(imagenumber)(1:k)
C
        write(6,'(/,''Input name for input parameter file'')')
        read(5,'(A)',END=901,ERR=901)cinparameters(imagenumber)
        call shorten(cinparameters(imagenumber),k)
        write(6,'(''Read: '',A)')cinparameters(imagenumber)(1:k)
C
        imagenumber = imagenumber + 1
        if ( imagenumber.gt.IMAXFILE) then
          write(6,'(''::ERROR: maximum file number reached.'',
     .    '' Increase IMAXFILE'')')
          stop
        endif
C
      goto 100
C
 110  continue
      imagenumber = imagenumber - 1
      write(6,'(''Read '',I6,'' input images'')') imagenumber
C
C-----Calculate total dimensions of output file
C
      imaxx = 0
      imaxy = 0
C
      do i=1,imagenumber
        open(11,FILE=cinparameters(i),STATUS='OLD',ERR=902)
C
        rgridpos_x(i) = 1.0
        rgridpos_y(i) = 1.0
        ispotdim_x(i) = 2048
        ispotdim_y(i) = 2048
        iendx = rgridpos_x(i)+ispotdim_x(i)
        iendy = rgridpos_y(i)+ispotdim_y(i)
        if(iendx.gt.imaxx)imaxx=iendx
        if(iendy.gt.imaxy)imaxy=iendy
C
      enddo
      if(imaxx.lt.imaxy)imaxx=imaxy
C
      write(6,'(''Required output dimensions are '',I16)')imaxx
C
C-----Paste input images into outimage
C
      DMIN =  1.E10
      DMAX = -1.E10
      DMEAN = 0.0
      DOUBLMEAN = 0.0
      inumber = 0
C
      do i=1,imagenumber
        CALL IMOPEN(1,cinimage(i),'RO')
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
        NXT = NX
        MXT = MX
        IF (MODE .GE. 3) then
          NXT = NXT*2
          MXT = MXT*2
        endif
        CALL IRTLAB(1,LABELS,NL)
        CALL IRTEXT(1,EXTRA,1,29)
        CALL IRTCEL(1,CELL)
C
        write(*,'('' Opened file has dimensions '',2I6)')NX,NY
        if(i.eq.1)then
          CALL IMOPEN(2,coutfile,'NEW')
          CALL ITRHDR(2,1)
        endif

C-------Calculate offset for first corner of output image
        IOFFSX=rgridpos_x(i)
        IOFFSY=rgridpos_y(i)
C
C-------Copy this spot image into output picture
        do iy = 1,ispotdim_y(i)
          CALL IRDLIN(1,ALINE,*999)
          do ix = 1,ispotdim_x(i)
            VAL=ALINE(ix)
            tmpimage(IOFFSX+ix,IOFFSY+iy)=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
            inumber = inumber + 1
          enddo
        enddo
        CALL IMCLOSE(1)
      enddo
C
      DOUBLMEAN = DOUBLMEAN/inumber
      DMEAN = DOUBLMEAN
C
C-----Mask the area arond the spots with average grey
C
      call maskpic(tmpimage,outimage,IOUTDIM,NX,DMEAN)
C
C-----Put title labels, new cell and extra information only into header
C
      NX=imaxx
      NY=imaxx
      NXYZ(1)=NX
      NXYZ(2)=NY
C
      MODE = 2
C
      CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
      CELL(1) = REAL(NX)
      CELL(2) = REAL(NY)
      CALL IALEXT(2,EXTRA,1,29)
      CALL IALCEL(2,CELL)
      CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
      write(*,'('' Output file has dimensions '',2I6)')NX,NY
C
      do iy = 1,NX
        do ix = 1,NY
          ALINE(ix)=outimage(ix,iy)
        enddo
        CALL IWRLIN(2,ALINE)
      enddo
C
      write(*,'('' Min, Max, Mean of output file is '',3F12.3)')
     .     DMIN,DMAX,DMEAN
C
      CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
      call IMCLOSE(2)
C
      goto 999
C
C----------------------------------------------------------------
C
 901  continue
      write(6,'(''::ERROR during file read.'')')
      goto 999
C
 902  continue
      write(6,'(''::ERROR opening parameters file.'')')
      goto 999
C
 999  continue
C
      stop
      end
C
C==========================================================
C
      SUBROUTINE maskpic(inimage,outimage,IOUTDIM,NX,DMEAN)
C
C-----Replace the areas around the spots in the input image with 
C-----average grey
C
      PARAMETER (IPICDIM=2048)
C
      INTEGER*2 inimage(IOUTDIM,IOUTDIM)
      INTEGER*2 outimage(IOUTDIM,IOUTDIM)
C
      DIMENSION TEXT(20),PROGTIT(4)
C
      CHARACTER*80 FILENAM,TEXT
C
      REAL*8 DOUBLMEAN
      INTEGER*8 INT8COUNTER
C
      INTEGER iworkimage(IPICDIM,IPICDIM)
      INTEGER ipict1(IPICDIM,IPICDIM)
C
      rmax=inimage(1,1)
      iposmaxx=1
      iposmaxy=1
C
      rscale=NX/IPICDIM
C
C-----Get PEAKMAX and its position
C
      DO i=1,NX
        DO j=1,NX
          IF(inimage(i,j).gt.rmax)then
            iposmaxx=i
            iposmaxy=j
            rmax=inimage(i,j)
          END IF
        ENDDO
      ENDDO
C
      rthresh = 0.35
C
C-----Prepare workimage with 0 and 1
C
      DOUBLMEAN=0.0
      INT8COUNTER=0
C
      DO i=1,IPICDIM
        DO j=1,IPICDIM
          ix=i*rscale 
          iy=j*rscale 
          if(inimage(ix,iy).gt.rthresh)then
            iworkimage(i,j)=255
            DOUBLMEAN=DOUBLMEAN+inimage(ix,iy)
            INT8COUNTER=INT8COUNTER+1
          else
            iworkimage(i,j)=0
          endif
        ENDDO
      ENDDO
C
      DOUBLMEAN=DOUBLMEAN/INT8COUNTER
      write(6,'(''Mean of entire image is '',F12.3)')DMEAN
      DMEAN=DOUBLMEAN
      write(6,'(''Mean of spot areas   is '',F12.3)')DMEAN
C
C-----Write out workimage
C
      write(FILENAM(1:80),'(''TMP_2dx_spotscan_pad_1.mrc'')')
      write(TEXT(1:80),'(''2dx_spotscan_pad: Initial '',
     .  ''thresholded image'')')
      CALL PICWRIINT(iworkimage,FILENAM,TEXT,IPICDIM,IPICDIM)
C
C-----Do morphological operations on test picture iworkimage
C
      isize1 = IPICDIM/25
      isize2 = isize1 - 1
      isize3 = isize1/4
      rsize4 = real(isize1)/20.0
      if(rsize4.lt.10.0)rsize4=10.0
C      rsize4 = 3.0
C
C.....Expand to get rid of holes
      CALL EXPAND(iworkimage,ipict1,IPICDIM,IPICDIM,isize1)
C  
      write(FILENAM(1:80),'(''TMP_2dx_spotscan_pad_2.mrc'')')
      if(icontrol.gt.0)CALL PICWRI(ipict1,FILENAM,TEXT,IPICDIM,IPICDIM)
C
C.....Contract to get the original size back
      CALL CONTRA(ipict1,iworkimage,IPICDIM,IPICDIM,isize2)
C
      write(FILENAM(1:80),'(''TMP_2dx_spotscan_pad_3.mrc'')')
      if(icontrol.gt.0)CALL PICWRI(iworkimage,FILENAM,TEXT,IPICDIM,IPICDIM)
C
C.....Contract to get rid of islands
      CALL CONTRA(iworkimage,ipict1,IPICDIM,IPICDIM,isize2)
C
      write(FILENAM(1:80),'(''TMP_2dx_spotscan_pad_4.mrc'')')
      if(icontrol.gt.0)CALL PICWRI(ipict1,FILENAM,TEXT,IPICDIM,IPICDIM)
C
C.....Expand to get the original size back
      CALL EXPAND(ipict1,iworkimage,IPICDIM,IPICDIM,isize1)
C
      write(FILENAM(1:80),'(''TMP_2dx_spotscan_pad_5.mrc'')')
      if(icontrol.gt.0)CALL PICWRI(iworkimage,FILENAM,TEXT,IPICDIM,IPICDIM)
C
C.....Mask edges of image 
      CALL MASKEDGE(iworkimage,ipict1,IPICDIM,IPICDIM,isize3)
C
      write(FILENAM(1:80),'(''TMP_2dx_spotscan_pad_6.mrc'')')
      if(icontrol.ne.0)CALL PICWRI(ipict1,FILENAM,TEXT,IPICDIM,IPICDIM)
C
C.....Smooth picture
      CALL SMOOTH(ipict1,iworkimage,IPICDIM,IPICDIM,rsize4)
C     
      write(FILENAM(1:80),'(''TMP_2dx_spotscan_pad_7.mrc'')')
      if(icontrol.ne.0)CALL PICWRI(iworkimage,FILENAM,TEXT,IPICDI2,IPICDIM)
C
C-----mask the original image with this pattern
C
      write(6,'('' masking picture'')')
C
      iunder=0
      iover=0
      rmin=0.0
      rmax=32000.0
      DMIN= 1.0E30
      DMAX=-1.0E30
      DOUBLMEAN=0.0
      write(*,'(''starting masking big image'')')
      DO IY = 1,NX
        ilk=(IY*IPICDIM)/NX
        if(ilk.lt.1)ilk=1
        if(ilk.gt.IPICDIM)ilk=IPICDIM
        DO IX = 1,NX
          ilj=(IX*IPICDIM)/NX
          if(ilj.lt.1)ilj=1
          if(ilj.gt.IPICDI2)ilj=IPICDI2
          VAL=(real(inimage(IX,IY))-DMEAN)*(real(iworkimage(ilj,ilk))/255.0)+DMEAN
          if(VAL.lt.rmin)then
            iunder=iunder+1
            VAL=rmin
          endif
          if(VAL.gt.rmax)then
            iover=iover+1
            VAL=rmax
          endif
          IF (VAL .LT. DMIN) DMIN = VAL
          IF (VAL .GT. DMAX) DMAX = VAL
          DOUBLMEAN = DOUBLMEAN + VAL
          outimage(IX,IY)=VAL
        enddo
      enddo
      DMEAN = DOUBLMEAN/(NX*NX)
C
      if(iunder.ne.0)then
        print *,'WARNING: iunder = ',iunder
      endif
C
      if(iover.ne.0)then
        print *,'WARNING: iover = ',iover
      endif
C
      RETURN
      END
C
C==========================================================
c
      SUBROUTINE shorten(czeile,k)
C
C counts the number of actual characters not ' ' in czeile
C and gives the result out in k.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1
      CHARACTER * 1 CTMP2
      CTMP2=' '
C
      ilen=len(czeile)
      DO 100 I=1,ilen
         k=ilen+1-I
         READ(CZEILE(k:k),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)GOTO 300
  100 CONTINUE
  300 CONTINUE
      IF(k.LT.1)k=1
C
      RETURN
      END
C
C************************************************************************
C
      SUBROUTINE EXPAND(IPICTU,IPICT1,IPICDIM,IPICSIZ,idist)
C
C-----The input image IPICTU is an integer image, containing either 255 or 0.
C-----This routine expands the 255-islands by a radius of idist.
C-----The output image IPICT1 is also an integer image.
C
      INTEGER IPICTU(IPICDIM,IPICDIM)
      INTEGER IPICT1(IPICDIM,IPICDIM)
C
      print *,'EXPAND called with ',idist
C
      ianf = -idist
      iend =  idist
      id2 = idist * idist
      DO K=1,IPICSIZ
        DO J=1,IPICSIZ
          IOUT=0
          do ikrun=ianf,iend
            ikloc=K+ikrun
            if(ikloc.lt.1      )ikloc=1
            if(ikloc.gt.IPICSIZ)ikloc=IPICSIZ
            ik2 = ikrun * ikrun
            do ijrun=ianf,iend
              ijloc=J+ijrun
              if(ijloc.lt.1      )ijloc=1
              if(ijloc.gt.IPICSIZ)ijloc=IPICSIZ
              if(ik2+ijrun*ijrun.le.id2)then
                if(IPICTU(ijloc,ikloc).ne.0)IOUT=255
              endif
            enddo
          enddo
          IPICT1(J,K)=IOUT
        ENDDO
      ENDDO
C
      RETURN
      END
C
C************************************************************************
C
      SUBROUTINE CONTRA(IPICTU,IPICT1,IPICDIM,IPICSIZ,idist)
C
C-----The input image IPICTU is an integer image, containing either 255 or 0.
C-----This routine contracts the 255-islands by a radius of idist.
C-----The output image IPICT1 is also an integer image.
C
      INTEGER IPICTU(IPICDIM,IPICDIM)
      INTEGER IPICT1(IPICDIM,IPICDIM)
C
      print *,'CONTRA called with ',idist
C
      ianf = -idist
      iend =  idist
      id2 = idist * idist
      DO K=1,IPICSIZ
        DO J=1,IPICSIZ
          IOUT=255
          do ikrun=ianf,iend
            ikloc=K+ikrun
            if(ikloc.lt.1      )ikloc=1
            if(ikloc.gt.IPICSIZ)ikloc=IPICSIZ
            ik2 = ikrun * ikrun
            do ijrun=ianf,iend
              ijloc=J+ijrun
              if(ijloc.lt.1      )ijloc=1
              if(ijloc.gt.IPICSIZ)ijloc=IPICSIZ
              if(ik2+ijrun*ijrun.le.id2)then
                if(IPICTU(ijloc,ikloc).eq.0)IOUT=0
              endif
            enddo
          enddo
          IPICT1(J,K)=IOUT
        ENDDO
      ENDDO
C
      RETURN
      END
C
C************************************************************************
C
      SUBROUTINE MASKEDGE(IPICTU,IPICT1,IPICDIM,IPICSIZ,idist)
C
C-----The input image IPICTU is an integer image, containing either 255 or 0.
C-----This routine masks the edges of idist width with 0
C-----The output image IPICT1 is also an integer image.
C
      INTEGER IPICTU(IPICDIM,IPICDIM)
      INTEGER IPICT1(IPICDIM,IPICDIM)
C
      print *,'MASKEDGE called with ',idist
C
      DO K=1,IPICSIZ
        DO J=1,IPICSIZ
          IVAL=IPICTU(J,K)
          if((J.le.idist        ) .or.
     1       (J.ge.IPICSIZ-idist) .or. 
     2       (K.le.idist        ) .or. 
     3       (K.ge.IPICSIZ-idist)     )then
            IVAL=0
          endif
          IPICT1(J,K)=IVAL
        ENDDO
      ENDDO
C
      RETURN
      END
C
C************************************************************************
C
      SUBROUTINE SMOOTH(IPICTU,IPICT1,IPICDIM,IPICSIZ,rdist)
C
C-----The input image IPICTU is an integer image, containing either 255 or 0.
C-----This routine smoothes the edges of 255-areas with a Gaussian.
C-----The output image IPICT1 is also an integer image.
C
      PARAMETER (IGAURA1=-100)
      PARAMETER (IGAURA2=100)
C
      INTEGER IPICTU(IPICDIM,IPICDIM)
      INTEGER IPICT1(IPICDIM,IPICDIM)
C
      REAL RGAUS(IGAURA1:IGAURA2,IGAURA1:IGAURA2)
      REAL*8 RSUM,RINT
C
      print *,'SMOOTH called with ',rdist
C
      do K=IGAURA1,IGAURA2
       do J=IGAURA1,IGAURA2
          rrad=SQRT(1.0*K*K+1.0*J*J)
          RGAUS(J,K)=exp(-rrad/rdist)
        enddo
      enddo
C
      ILIMIT=IGAURA2
      do I=1,IGAURA2
C-------if(RGAUS(I,0).gt.0.01)ILIMIT=I
C-------Here only for masking info, can be faster:
        if(RGAUS(I,0).gt.0.02)ILIMIT=I
      enddo
      if(ILIMIT.gt.IGAURA2)ILIMIT=IGAURA2
C
      ianf = -ILIMIT
      iend =  ILIMIT
      RINT=0.0
      do K=ianf,iend
        do J=ianf,iend
          RINT=RINT+RGAUS(J,K)
        enddo
      enddo
C
      write(6,'('' RGAUS field calculated'')')
      print *,' reaching until ',ILIMIT,' with integral = ',RINT
C
      write(*,'(''Convoluting: IPICDIM='',I8,
     .  '', ianf,iend = '',2I8)')IPICDIM,ianf,iend
      call system("echo `date`")
C
      DO K=1,IPICDIM
        DO J=1,IPICDIM
          RSUM=0.0
          do ikrun=ianf,iend
            ikloc=K+ikrun
            if(ikloc.lt.1      )ikloc=1
            if(ikloc.gt.IPICDIM)ikloc=IPICDIM
            do ijrun=ianf,iend
              ijloc=J+ijrun
              if(ijloc.lt.1      )ijloc=1
              if(ijloc.gt.IPICDI2)ijloc=IPICDIM
              RSUM=RSUM+IPICTU(ijloc,ikloc)*RGAUS(ijrun,ikrun)
            enddo
          enddo
          RSUM=RSUM/RINT
          IPICT1(J,K)=RSUM
        ENDDO
      ENDDO
C
      write(6,'('' IPICTU smoothed to IPICT1'')')
      call system("echo `date`")
C
      RETURN
      END
C
C************************************************************************
C
      SUBROUTINE PICWRIINT(IPICTU,FILENAM,TEXT,IPICDIM,IPICSIZ)
C
      PARAMETER (LMAX=20100)
      PARAMETER (LCMX=10050)
C
      INTEGER IPICTU(IPICDIM,IPICDIM)
C
      COMMON //NX,NY,NZ,IXMIN,IYMIN,IZMIN,IXMAX,IYMAX,IZMAX
C
      DIMENSION ALINE(LMAX),TITLE(20),NXYZ(3),MXYZ(3)
      DIMENSION NXYZST(3)
      DIMENSION LABELS(20,10)
C
      DIMENSION TEXT(20),PROGTIT(4)
C
      CHARACTER*80 FILENAM
C
      DMIN =  0.0
      DMAX =  255.0
      DMEAN = 0.5
      MODE = 0
C
      NX=IPICSIZ
      NY=IPICSIZ
      NXYZ(1)=NX
      NXYZ(2)=NY
      NXYZ(3)=1 
      NXYZST(1) = NXYZ(1)
      NXYZST(2) = NXYZ(2)
      NXYZST(3) = NXYZ(3)
C
      write(6,'('' Writing picture into '',A40)')FILENAM
      write(6,'('' Size '',I15,'' (total array size: '',I15)')IPICDIM,IPICSIZ
C
      MODE=0
      CALL IMOPEN(2,FILENAM,'NEW')
      CALL ICRHDR(2,NXYZST,NXYZST,MODE,LABELS,0)
      CALL ITRLAB(2,1)
      CALL IWRHDR(2,TEXT,1,DMIN,DMAX,DMEAN)
C
      DMIN=100.0
      DMAX=-100.0
      DOUBLMEAN=0.0
      DO K=1,IPICSIZ
        DO J=1,IPICSIZ
          ALINE(J) = IPICTU(J,K)
          if(IPICTU(J,K).lt.DMIN)DMIN=IPICTU(J,K)
          if(IPICTU(J,K).gt.DMAX)DMAX=IPICTU(J,K)
          DOUBLMEAN=DOUBLMEAN+IPICTU(J,K) 
        enddo 
        CALL IWRLIN(2,ALINE)
      enddo
      DMEAN=DOUBLMEAN/(IPICSIZ*IPICSIZ)
      CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
      CALL IMCLOSE(2)
C
      RETURN
      END
C

