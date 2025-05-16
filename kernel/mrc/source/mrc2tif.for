C************************************************************************
C***
C*** mrc2tif - Program to convert MRC IMAGE format map to 8-bit TIFF
C***
C************************************************************************
        real*4   greylevels
        parameter  (greylevels = 255.)
        integer*4  i2
        parameter  (i2 = 2)
        integer*4  i4
        parameter  (i4 = 4)
        integer*4  i8
        parameter  (i8 = 8)
        integer*2  ibyte
        parameter  (ibyte = 1)
        integer*2  iascii
        parameter  (iascii = 2)
        integer*2  ishort
        parameter  (ishort = 3)
        integer*2  ilong
        parameter  (ilong = 4)
        integer*2  irational
        parameter  (irational = 5)
        integer*4  idevin
        parameter  (idevin = 1)
        integer*4  ifd_offset
        parameter  (ifd_offset = 8)
        integer*4  maxcols
        parameter  (maxcols = 20100)
        integer*4  maxrows
        parameter  (maxrows = 20100)
        integer*4  maxnum
        parameter  (maxnum = 6)
        integer*4  maxwidth
        parameter  (maxwidth = 15)
        integer*2  nbitsperbyte
        parameter  (nbitsperbyte=8)
        integer*2  ntags
        parameter  (ntags = 14)
        integer*4  noffset_values
        parameter  (noffset_values = 4)
        integer*4  tag_size
        parameter  (tag_size = 12)
        integer*4  tag_offset
        parameter  (tag_offset = ifd_offset + i2
     *                                      + ntags * tag_size + i4)
        integer*4  stripoffsets
        parameter  (stripoffsets = tag_offset +
     *       noffset_values * 4)
        integer*2  version_number
        parameter  (version_number = 42)
C***
        byte             btest(2)
        byte             buf(maxcols)
C***
        character*2  byte_order
        character*32     filenumber
        character*256  filin
        character*256    filout
        character*80  title
C***
        integer*2  ishort_value
        integer*2  itest
        integer*2  tag(6)
        integer*2  i2tag
C***
        integer*4  ibuf
        integer*4  idevout
        integer*4  ilong_value
        integer*4  mode
        integer*4  modeout
        integer*4  mxyz(3)
        integer*4  nmcitm
        integer*4  nxyz(3)
        integer*4  nbytesperitem
        integer*4  ncols
        integer*4  nrows
        integer*4  nvalues
        integer*4  rational(2)
C***
        real*4   aline(maxcols)
        real*4   dmin
        real*4   dmax
        real*4   dmean
C***
        equivalence  (btest(1),itest)
        equivalence  (tag(1),i2tag)
        equivalence  (tag(3),nvalues)
        equivalence  (tag(5),ishort_value)
        equivalence  (tag(5),ilong_value)
C***
C*******************************************************************
C*** start of program
C*******************************************************************
        write(6,'(
     *  '' image_tiff V2.1 - 18.03.2011'',
     *  '' convert MRC IMAGE to 8-bit TIFF format''/)')
        write(6,'('' Type input filename ...'')')
        read(5,'(a)') filin
C*** read image format header
        call imopen(idevin,filin(1:lnblnk(filin)),'old')
        call irdhdr(idevin,nxyz,mxyz,mode,dmin,dmax,dmean)
C*** input file diagnostics
        if(mode .ge. 1) then
         write(6,'(''!!!Warning - program converts data to 8 bits''/)')
        else if(mode .gt. 2) then
         write(6,'('' Program aborted -'',
     *             '' mode greater than 2 cannot be processed.'')')
         stop
        end if
        ncols = nxyz(1)
        nrows = nxyz(2)
        nsecs = nxyz(3)
        if(ncols .gt. maxcols .or. nrows .gt. maxrows) then
         write(6,'('' Program aborted -'',
     *             '' dimensions insufficient.'')')
CHENN>
         write(6,'('' NCOLS='',I12,'', MAXCOLS='',I12)') ncols,maxcols
         write(6,'('' NROWS='',I12,'', MAXROWS='',I12)') nrows,maxrows
CHENN<
         stop
        end if
        modeout = 0
c       nmcitm = 1
C*** open output tiff file for single section image
        if(nsecs .eq. 1) then
         write(6,'('' Type output filename ...'')')
         read(5,'(a)') filout
         call qopen(idevout,filout(1:lnblnk(filout)),'UNKNOWN')
         call qmode(idevout,0,nmcitm)
C*** set section number greater than number of sections
         nosec = 1
        else
         write(6,'('' Type output file precursor ...''/
     *             '' e.g. datafile (files will be datafile01.tif,'',
     *             ''datafile02.tif ...'')')
         read(5,'(a)') filout
         ntext = lnblnk(filout)
         nnum = 1
         itmp = nsecs
  500    itmp = itmp / 10
         nnum = nnum + 1
         if(itmp .ge. 10) go to 500
         if(nnum .gt. maxnum) then
          write(6,'(''Error - too many sections for program'')')
          stop
         end if
         nosec = 1
        end if
C*** test machine little or big endian
        btest(1) = 0
        btest(2) = 0
        itest = 1
C*** big endian
        if(btest(1) .eq. 0) then
         byte_order = 'MM'
C*** little endian
        else
         byte_order = 'II'
        end if
C*** open output tiff file for multi-section image
 1000   if(nsecs .gt. 1) then
         if(nnum .eq. 1) then
          write(filenumber,'(i1.1)') nosec
         else if(nnum .eq. 2) then
          write(filenumber,'(i2.2)') nosec
         else if(nnum .eq. 3) then
          write(filenumber,'(i3.3)') nosec
         else if(nnum .eq. 4) then
          write(filenumber,'(i4.4)') nosec
         else if(nnum .eq. 5) then
          write(filenumber,'(i5.5)') nosec
         else if(nnum .eq. 6) then
          write(filenumber,'(i6.6)') nosec
         end if
         filout(ntext+1:ntext+nnum) = filenumber(1:nnum)
         filout(ntext+nnum+1:ntext+nnum+5) = '.tif'
         call qopen(idevout,filout(1:lnblnk(filout)),'UNKNOWN')
         call qmode(idevout,modeout,nmcitm)
        end if
C*** write tiff header section
        call qwritc(idevout,byte_order,i2)
        call qwriti2(idevout,version_number,i2)
        call qwriti(idevout,ifd_offset,i4)
        call qwriti2(idevout,ntags,i2)
C******************************************************************
C*** write tiff format tags
C******************************************************************
C*** NewSubFileType
        tag(1) = 254
        tag(2) = ilong
        nvalues = 1
        ilong_value = 0
        call qwriti2(idevout,i2tag,tag_size)
        write(6,'(''NewSubFileType'',i8)') ilong_value
C*** Imagewidth
        tag(1) = 256
        tag(2) = ilong
        nvalues = 1
        ilong_value = ncols
        call qwriti2(idevout,i2tag,tag_size)
        write(6,'(''ImageWidth'',i8)') ilong_value
C*** Imagelength
        tag(1) = 257
        tag(2) = ilong
        nvalues = 1
        ilong_value = nrows
        call qwriti2(idevout,i2tag,tag_size)
        write(6,'(''ImageLength'',i8)') ilong_value
C*** BitsPerSample
        tag(1) = 258
        tag(2) = ishort
        nvalues = 1
        ishort_value = nbitsperbyte
        call qwriti2(idevout,i2tag,tag_size)
        write(6,'(''BitsPerSample'',i8)') ishort_value
C*** Compression
        tag(1) = 259
        tag(2) = ishort
        nvalues = 1
        ishort_value = 1
        call qwriti2(idevout,i2tag,tag_size)
        write(6,'(''Compression'',i8)') ishort_value
C*** PhotometricInterpretation
        tag(1) = 262
        tag(2) = ishort
        nvalues = 1
        ishort_value = 1
        call qwriti2(idevout,i2tag,tag_size)
        write(6,'(''PhotometricInterpretation'',i8)') ishort_value
C*** StripOffsets
        tag(1) = 273
        tag(2) = ilong
        nvalues = 1
        ilong_value = stripoffsets
        call qwriti2(idevout,i2tag,tag_size)
        write(6,'(''StripOffsets'',i8)') ilong_value
C*** Orientation
        tag(1) = 274
        tag(2) = ishort
        nvalues = 1
        ishort_value = 1
        call qwriti2(idevout,i2tag,tag_size)
        write(6,'(''Orientation'',i8)') ishort_value
C*** SamplesPerPixel
        tag(1) = 277
        tag(2) = ishort
        nvalues = 1
        ishort_value = 1
        call qwriti2(idevout,i2tag,tag_size)
        write(6,'(''SamplesPerPixel'',i8)') ishort_value
C*** RowsPerStrip
        tag(1) = 278
        tag(2) = ilong
        nvalues = 1
        ilong_value = nrows
        call qwriti2(idevout,i2tag,tag_size)
        write(6,'(''RowsPerStrip'',i8)') ilong_value
C*** StripByteCounts
        tag(1) = 279
        tag(2) = ilong
        nvalues = 1
        ilong_value = ncols * nrows
        call qwriti2(idevout,i2tag,tag_size)
        write(6,'(''StripByteCounts'',i12)') ilong_value
C*** XResolution
        tag(1) = 282
        tag(2) = irational
        nvalues = 1
        ilong_value = tag_offset
        call qwriti2(idevout,i2tag,tag_size)
        write(6,'(''XResolution'',i8)') ilong_value
C*** YResolution
        tag(1) = 283
        tag(2) = irational
        nvalues = 1
        ilong_value = tag_offset + i8
        call qwriti2(idevout,i2tag,tag_size)
        write(6,'(''YResolution'',i8)') ilong_value
C*** ResolutionUnit - set to cm
        tag(1) = 296
        tag(2) = ishort
        nvalues = 1
        ishort_value = 3
        call qwriti2(idevout,i2tag,tag_size)
        write(6,'(''ResolutionUnit'',i8)') ishort_value
C*** final record before data
        ilong_value = 0
        call qwriti(idevout,ilong_value,i4)
C*** XResolution value
        rational(1) = ncols
        rational(2) = maxwidth
        call qwritr(idevout,rational,i8)
        xres = real(ncols) / real(maxwidth)
        write(6,'(''XResolution value'',f10.0)') xres
C*** YResolution value
        call qwritr(idevout,rational,i8)
        write(6,'(''YResolution value here'',f10.0)') xres
C****************************************************************
C*** write data to output file turned upside down to preserve orientation
C****************************************************************
        scale = greylevels / (dmax - dmin)
        do ny=1,nrows
C*** jms 14.05.2012      call imposn(idevin,nosec-1,ny-1)
         call imposn(idevin,nosec-1,nrows-ny)
         call irdlin(idevin,aline)
         do nx=1,ncols
          ibuf = nint(min(greylevels,max(0.,(aline(nx)-dmin)*scale)))
          buf(nx) = ibuf
         end do
         call qwritb(idevout,buf,ncols)
        end do
        call qclose(idevout)
C*** move to next section
        nosec = nosec + 1
        if(nsecs .gt. 1 .and. nosec .lt. nsecs) go to 1000
        call imclose(idevin)
        write(6,'('' Normal termination.'')')
        end
