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
      REAL outimage(IOUTDIM,IOUTDIM)
C
      EQUIVALENCE (NX,NXYZ), (ALINE,CLINE), (OUT,COUT)
      EQUIVALENCE (IXYZMIN, IXMIN), (IXYZMAX, IXMAX)
C
      DATA NXYZST/3*0/, CNV/57.29578/
      DATA outimage/IOUTDIM2*0.0/
C
      write(6,'(/,/,''2dx_SpotScan_Pad: Merge several SpotScan images into one'')')
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
          write(6,'(''::ERROR: maximum file number reached. Increase IMAXFILE'')')
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
C-------Copy entire file into output picture
        do iy = 1,ispotdim_y(i)
          CALL IRDLIN(1,ALINE,*999)
          do ix = 1,ispotdim_x(i)
            VAL=ALINE(ix)
            outimage(IOFFSX+ix,IOFFSY+iy)=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
            inumber = inumber + 1
          enddo
        enddo
        CALL IMCLOSE(1)
      enddo
C
C  Put title labels, new cell and extra information only into header
C
      NX=imaxx
      NY=imaxx
      NXYZ(1)=NX
      NXYZ(2)=NY
C
      DOUBLMEAN = DOUBLMEAN/inumber
      DMEAN = DOUBLMEAN
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
c==========================================================
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
C


