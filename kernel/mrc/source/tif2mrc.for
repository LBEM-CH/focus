C******************************************************************
C***       tif2mrc
C***    Program to read tiff format file reformat the output as 
C***       MRC IMAGE format
C******************************************************************
C***
       integer*4       dirpoint
       parameter      (dirpoint = 5)
       integer*4       idevout
       parameter      (idevout = 2)
       integer*4       istartblock
       parameter      (istartblock = 1)
       integer*4       lenfile
       parameter      (lenfile = 256)
       integer*4       lrecl
       parameter      (lrecl = 0)
       integer*4       maxcols
       parameter      (maxcols = 20100)
       integer*4       maxrows
       parameter      (maxrows = 20100)
       integer*4       mode_tiff
       parameter      (mode_tiff = 0)
       integer*4      standard_step_size
       parameter      (standard_step_size = 400)
       integer*4      pixels_per_cm
       parameter     (pixels_per_cm = 10000 / standard_step_size)
C***
       byte              btest(2)
       byte           ibuf(maxcols*2)
       byte              value_type1
C***
       character       filin*256
       character       filout*256
       character       text*80
       character       title_string*80
       character       value_type2
       character       yesno*1
C***
CHENN>
C------This may be a compiler/platform dependent thing: 
C------See also line 572.
C       integer*4       buf(maxcols)
       integer*2       buf(maxcols)
CHENN<
       integer*2       field_type
       integer*2       int2
       integer*2       itest
       integer*2       orientation
       integer*2       tag
       integer*2       value_type3
C***
       integer*4        denominator
       integer*4        filetag
       integer*4        ioffset
       integer*4        int4
       integer*4        length
       integer*4        lnblank
       integer*4        mxyz(3)
       integer*4        nxyz(3)
       integer*4        numerator
       integer*4        resolutionunit
       integer*4        sampleformat
       integer*4        stripbytecounts
       integer*4        stripoffsets
       integer*4        value_offset
       integer*4        value_type4
       integer*4        xresolution
       integer*4        yresolution
C***
       logical               bigendian
       logical               byteswap
       logical               chess
       logical               molecular_dynamics
       logical               adobe_photoshop
       logical               newtag
C***
       real*4               mapbuf(maxcols,maxrows)
       real*4               rbuf(maxcols)
       real*4               scalepixel
       real*4           title(20)
C***
       real*8              dnum
       real*8              dtot
C***
C*** test machine little or big endian 
       equivalence       (btest(1),itest)
       equivalence       (buf,ibuf)
       equivalence       (ioffset,ibuf(1))
       equivalence       (tag,ibuf(1))
       equivalence       (field_type,ibuf(3))
       equivalence       (length,ibuf(5))
       equivalence       (value_offset,ibuf(9))
       equivalence       (value_type1,ibuf(9))
       equivalence       (value_type2,ibuf(9))
       equivalence       (value_type3,ibuf(9))
       equivalence       (value_type4,ibuf(9))
       equivalence       (numerator,ibuf(1))
       equivalence       (denominator,ibuf(5))
       equivalence       (int2,ibuf(1))
       equivalence       (int4,ibuf(1))
C***
       common/big/mapbuf
C***
C**************** start of program *********************************
       write(6,'(/'' *** tif2mrc V5.16 convert TIFF'',
     *          '' format file to MRC IMAGE format 02.09.04''/)')
C*** initialize
       scalepixel = 1.0
       chess = .false.
       adobe_photoshop = .false.
       molecular_dynamics = .false.
       newtag = .false.
       nsamples_per_pixel = 1
       orientation = 1
       btest(1) = 0
       btest(2) = 0
       itest = 1
C*** big endian
       if(btest(1) .eq. 0) then
        bigendian = .true.
        write(6,'('' Big endian computer'')')
C*** little endian
       else
        bigendian = .false.
        write(6,'('' Little endian computer'')')
       end if
C*** input file names
       filin = ' '
       write(6,'(
     *  '' Type input file name e.g. /pcnfs/phosim1/jms.gel ...'')')
       read(5,'(a)') filin
  900       write(6,'(
     *  '' Type output file name e.g. /scr0/jms/jms.map ...'')')
       read(5,'(a)') filout
       if(filin(1:lnblank(filin)) .eq. 
     *  filout(1:lnblank(filout))) then
        write(6,'(
     *   '' Output file must be different from input file'')')
        go to 900
       end if
C*** open input tiff file and set the mode
       write(6,'(''opening input file '',a)')
     *  filin(1:lnblank(filin))
       call qopen(idevin,filin(1:lnblank(filin)),'READONLY')
       call qmode(idevin,mode_tiff,nchitm)
C*** read tiff header
C*** read first 2 bytes
       ielement = 1
       nitems = 4
       call qseek(idevin,istartblock,ielement,lrecl)
       call qread(idevin,ibuf,nitems,ier)
       if(ier.ne.0) go to 9000
C*** test first two bytes contain intel format
       write(text(1:1),'(a)') ibuf(1)
       write(text(2:2),'(a)') ibuf(2)
       byteswap = .false.
       if(text(1:2) .eq. 'II') then
        write(6,'(''Intel format'')')
        if(bigendian) then
         byteswap = .true.
        end if
       else if(text(1:2) .eq. 'MM') then
        write(6,'(''Motorola format'')')
        if(.not.bigendian) then
         byteswap = .true.
        end if
       else
        write(6,'(
     *   '':: Neither Motorola nor Intel format- cannot proceed.'')')
        go to 9000
       end if
C*** ignore next two bytes, (version number)
       if(ier.ne.0) go to 9000
C*** position pointer to next byte
       ielement = ielement + nitems
C*****************************************************
C*** extract offset for Image File Directory
C*****************************************************
 1000        call qseek(idevin,istartblock,ielement,lrecl)
       nitems = 4
       call qread(idevin,ibuf,nitems,ier)
       if(ier.ne.0) go to 9000
       if(byteswap) call byteswapfourbytes(ibuf(1),nitems)
       if(ioffset .eq. 0) go to 3000
C*** read number of entries for this directory
       ielement = ioffset + 1
       call qseek(idevin,istartblock,ielement,lrecl)
       nitems = 2
       call qread(idevin,ibuf,nitems,ier)
       if(byteswap) call byteswaptwobytes(ibuf(1),nitems)
       nentries = int2
       write(6,'(i5,'' directory entries '')') nentries
C*** loop to extract tag information
       ielement = ielement + nitems
       nitems = 12
       open(unit=4,file='tag_data',status='unknown')
       do n=1,nentries
        call qseek(idevin,istartblock,ielement,lrecl)
        call qread(idevin,ibuf,nitems,ier)
        if(byteswap) then
         call byteswaptwobytes(ibuf(1),4)
         call byteswapfourbytes(ibuf(5),4)
         if(field_type .eq. 3) then
          call byteswaptwobytes(ibuf(9),2)
         else
          call byteswapfourbytes(ibuf(9),4)
         end if
        end if
        itag = tag
        if(itag .lt. 0) itag = 65536 + itag
C*****************************************************************
C*** extract tag information
C*****************************************************************
C*** NewSubfileType
        if(itag .eq. 254) then
         if(value_type4 .ne. 0) then
          write(6,'(''::Warning - multiple image file,''
     *    '' only first image used'')')
          go to 3000
         end if
C*** SubfileType
        else if(itag .eq. 255) then
         if(value_type3 .ne. 1) then
          write(6,'(''::Image not full resolution'')')
          go to 8100
         end if
C*** ImageWidth
         else if(itag .eq. 256) then
         if(field_type .eq. 3) then
          ncols = value_type3
         else
          ncols = value_type4
         end if
         write(6,'('' ImageWidth = '',i8)')ncols
C*** ImageLength
        else if(itag .eq. 257) then
         if(field_type .eq. 3) then
          nrows = value_type3
         else
          nrows = value_type4
         end if
         write(6,'('' ImageLength = '',i8)')nrows
C***  BitsPerSample
        else if(itag .eq. 258) then
         if(value_type3 .eq. 8) then
          mode_image = 0
         else if(value_type3 .eq. 16) then
          mode_image = 1
         else
          write(6,'(/
     *     ''::!! Tag '',i3,'' indicates RGB colour image''/ 
     *     ''::   tif2mrc cannot read colour images : '',
     *     ''try tiff2bw to convert to grayscale first.'')')
     *     itag
          stop
         end if
         nbits_per_sample = value_type3
         write(6,'('' BitsPerSample = '',i5)') nbits_per_sample
C*** Compression
        else if(itag .eq. 259) then
         if(value_type3 .ne. 1) go to 8100
C*** Photometric
        else if(itag .eq. 262) then
         if(value_type3 .lt. 0 .or. value_type3 .gt. 3) 
     *    go to 8100
C*** FillOrder
        else if(itag .eq. 266) then
         if(value_type3 .ne. 1) go to 8100
C*** DocumentName
        else if(itag .eq. 269) then
         go to 2000
C*** ImageDescription
        else if(itag .eq. 270) then
         go to 2000
C*** Make
        else if(itag .eq. 271) then
         go to 2000
C*** Model
        else if(itag .eq. 272) then
         go to 2000
C*** StripOffsets
        else if(itag .eq. 273) then
         if(field_type .eq. 3) then
          stripoffsets = value_type3
         else
          stripoffsets = value_type4
         end if
C*** if length > 1 read offset to get value for start of image
         if(length .gt. 1) then
          call qseek(idevin,istartblock,stripoffsets+1,lrecl)
          if(field_type .eq. 3) then
           call qread(idevin,ibuf,2,ier)
           if(byteswap) 
     *      call byteswaptwobytes(ibuf(1),2)
           stripoffsets = int2
          else
           call qread(idevin,ibuf,4,ier)
           if(byteswap) 
     *      call byteswapfourbytes(ibuf(1),4)
           stripoffsets = int4
          end if
         end if
C*** should really read all the values and check their differences are identical
         write(6,'('' StripOffsets ='',i8)') stripoffsets
C*** Orientation
        else if(itag .eq. 274) then
         orientation = value_type3
         write(6,'('' Orientation ='',i5)') orientation
         if(orientation .ne. 1 .and. orientation .ne. 4) go to 8100
C*** SamplesPerPixel
        else if(itag .eq. 277) then
         if(value_type3 .lt. 1 .or. value_type3 .gt.2) 
     *    go to 8100
         nsamples_per_pixel = value_type3
         write(6,'('' SamplesPerPixel ='',i8)')
     *    nsamples_per_pixel 
C*** RowsPerStrip
        else if(itag .eq. 278) then
         if(field_type .eq. 3) then
          nrowsperstrip = value_type3
         else
          nrowsperstrip = value_type4
         end if
         write(6,'('' RowsPerStrip ='',i8)') nrowsperstrip
         nstripsperimage = (nrows + nrowsperstrip - 1) / nrowsperstrip
C*** StripByteCounts
        else if(itag .eq. 279) then
C*** read value directly
         if(length .eq. 1) then
          if(field_type .eq. 3) then
           stripbytecounts = value_type3
          else
           stripbytecounts = value_type4
          end if
C*** read value from offset
         else
          call qseek(idevin,istartblock,value_offset+1,lrecl)
          if(field_type .eq. 3) then
           call qread(idevin,ibuf,2,ier)
           if(byteswap) 
     *      call byteswaptwobytes(ibuf(1),2)
           stripbytecounts = int2
          else
           call qread(idevin,ibuf,4,ier)
           if(byteswap) 
     *      call byteswapfourbytes(ibuf(1),4)
           stripbytecounts = int4
          end if
C*** check stripbytecounts identical. If not, program needs rewrite
C*** to accommodate pointer reading for each strip
          if(nstripsperimage .gt. 1) then
           do i=1,nstripsperimage
            itemp = stripbytecounts
            call qseek(idevin,istartblock,value_offset+1,lrecl)
            if(field_type .eq. 3) then
             call qread(idevin,ibuf,2,ier)
             if(byteswap) 
     *        call byteswaptwobytes(ibuf(1),2)
             stripbytecounts = int2
            else
             call qread(idevin,ibuf,4,ier)
             if(byteswap) 
     *        call byteswapfourbytes(ibuf(1),4)
             stripbytecounts = int4
            end if
            if(itemp .ne. stripbytecounts) then
             write(6,'(
     *        '':: Warning - uneven stripbytecounts. '',
     *        ''Output file may be incorrect'')')
             go to 2000
            end if
           end do
          end if
         end if
         write(6,'('' StripByteCounts ='',i8)') stripbytecounts
C*** MinSampleValue
        else if(itag .eq. 280) then
         min_value = value_type3
         if(min_value .lt. 0) min_value = min_value + 65536
         write(6,'('' MinSampleValue = '',i8)') min_value
         go to 2000
C*** MaxSampleValue
        else if(itag .eq. 281) then
         max_value = value_type3
         if(max_value .lt. 0) max_value = max_value + 65536
         write(6,'('' MaxSampleValue = '',i8)') max_value
         go to 2000
C*** XResolution
        else if(itag .eq. 282) then
         call qseek(idevin,istartblock,value_offset+1,lrecl)
         call qread(idevin,ibuf,8,ier)
         if(byteswap) call byteswapfourbytes(ibuf(1),8)
         xresolution = nint(float(numerator) / float(denominator))
         go to 2000
C*** YResolution
        else if(itag .eq. 283) then
         call qseek(idevin,istartblock,value_offset+1,lrecl)
         call qread(idevin,ibuf,8,ier)
         if(byteswap) call byteswapfourbytes(ibuf(1),8)
         yresolution = nint(float(numerator) / float(denominator))
         go to 2000
C*** PlanarConfiguration
        else if(itag .eq. 284) then
         if(value_type3 .ne. 1) go to 8100
C*** PageName
        else if(itag .eq. 285) then
         go to 2000
C*** XPosition
        else if(itag .eq. 286) then
         go to 2000
C*** YPosition
        else if(itag .eq. 287) then
         go to 2000
C*** ResolutionUnit
        else if(itag .eq. 296) then
         resolutionunit = value_type3
         go to 2000
C*** Software
        else if(itag .eq. 305) then
         go to 2000
C*** DateTime
        else if(itag .eq. 306) then
         go to 2000
C*** Artist
        else if(itag .eq. 315) then
         go to 2000
C*** HostComputer
        else if(itag .eq. 316) then
         go to 2000
C*** SampleFormat
        else if(itag .eq. 339) then
         sampleformat = value_type3
         write(6,'('' SampleFormat ='',i6)') sampleformat
         if(sampleformat .ne. 1) go to 8100
C*** ImageDepth
        else if(itag .eq. 32997) then
         if(field_type .eq. 3) then
          nsecs = value_type3
         else
          nsecs = value_type4
         end if
         if(nsecs .ne. 1) then
          write(6,'('':: Multiple section file..cannot continue'')')
          go to 8200
         end if
         write(6,'('' ImageDepth = '',i6)') nsecs
C*** Private Molecular Dynamics tag - md_filetag
        else if(itag .eq. 33445) then
         molecular_dynamics = .true.
         filetag = value_type4
         go to 2000
C*** Private Molecular Dynamics tag - md_scalepixel
        else if(itag .eq. 33446) then
         molecular_dynamics = .true.
         call qseek(idevin,istartblock,value_offset+1,lrecl)
         call qread(idevin,ibuf,8,ier)
         if(byteswap) call byteswapfourbytes(ibuf(1),8)
         scalepixel = float(numerator) / float(denominator)
         go to 2000
C*** Private Molecular Dynamics tag - md_colortable
        else if(itag .eq. 33447) then
         molecular_dynamics = .true.
         go to 2000
C*** Private Molecular Dynamics tag - md_labname
        else if(itag .eq. 33448) then
         molecular_dynamics = .true.
         go to 2000
C*** Private Molecular Dynamics tag - md_sampleinfo
        else if(itag .eq. 33449) then
         molecular_dynamics = .true.
         go to 2000
C*** Private Molecular Dynamics tag - md_prepdate
        else if(itag .eq. 33450) then
         molecular_dynamics = .true.
         go to 2000
C*** Private Molecular Dynamics tag - md_preptime
        else if(itag .eq. 33451) then
         molecular_dynamics = .true.
         go to 2000
C*** Private Molecular Dynamics tag - md_fileunits
        else if(itag .eq. 33452) then
         molecular_dynamics = .true.
         go to 2000
C*** Private Adobe Photoshop tag - unknown value
        else if(itag .eq. 34377) then
         adobe_photoshop = .true.
         write(6,'('' Private Adobe Photoshop tag = '',i5, 
     *    '' type = '',i5,'' length = '',i5,'' offset ='',i8)') 
     *    itag, field_type, length, value_offset
         go to 2000
C*** Private Chess tag(s)
        else if(itag .ge. 36864 .and. itag .le. 37120) then
         chess = .true.
         go to 2000
C*** unknown tag
        else
         newtag = .true.
         write(6,'('' Unknown tag = '',i5, 
     *    '' type = '',i5,'' length = '',i5,'' offset ='',i8)') 
     *    itag, field_type, length, value_offset
        end if
 2000        ielement = ielement + nitems
       end do
       go to 1000
C*****************************************************
C*** open output image format file
C*****************************************************
C*** check map not too large for program
 3000       if(ncols.gt.maxcols) then
        write(6,'('' ncols > maxcols = '',2i8)') ncols, maxcols        
         go to 8200
       else if(nrows.gt.maxrows) then
        write(6,'('' nrows > maxrows = '',2i8)') nrows, maxrows        
         go to 8200
       end if
C*** unknown tag ?
       if(newtag) then
         write(6,'('' Unknown tag(s) present, continue ? (y/n)'')')
         read(5,'(a1)') yesno
         if(yesno .ne. 'y' .and. yesno .ne. 'Y') go to 8200
       end if
       nbytes_per_row = (ncols * nsamples_per_pixel * 
     *                    nbits_per_sample + 7) / 8
C*** initialize header parameters, turn map round by 90 degrees antic
       nxyz(1) = ncols
       nxyz(2) = nrows
       nxyz(3) = 1
       if(molecular_dynamics) then
C*** initialize header parameters, turn map round by 90 degrees antic
C*** force ncols,nrows even
        nxyz(1) = (nrows / 2) * 2
        nxyz(2) = (ncols / 2) * 2
        nxyz(3) = 1
        mxyz(1) = nxyz(1) / (yresolution / pixels_per_cm)
        mxyz(2) = nxyz(2) / (xresolution / pixels_per_cm)
        mxyz(3) = 1
C*** calculate scaling to return +ve values without the parity bit set
        fmin = float(min_value * min_value) * scalepixel
        fmax = float(max_value * max_value) * scalepixel
        frange = fmax - fmin
        range_scale = 32767. / frange
       else
        mxyz(1) = nxyz(1)
        mxyz(2) = nxyz(2)
        mxyz(3) = 1
       end if
C***
       dmin = 100000000.
       dmax = -100000000.
       dtot = 0.
C*** open output file,  write header record
       title_string = 'TIFF format converted MRC image '
     *  //filin(1:lnblank(filin))
        read(title_string,'(20a4)') title
       call imopen(idevout,filout,'new')
       call icrhdr(idevout,nxyz,mxyz,mode_image,title,0)
       call iwrhdr(idevout,title,0,dmin,dmax,dmean)
       ielement = stripoffsets + 1
C*** loop through data to write image format map
       real_max = 0.
       do n = 1,nrows
          call qseek(idevin,istartblock,ielement,lrecl)
        call qread(idevin,ibuf,nbytes_per_row,ier)
        if(byteswap .and. mode_image .eq. 1) 
     *        call byteswaptwobytes(ibuf(1),nbytes_per_row)
        ielement = ielement + nbytes_per_row
        do i = 1,ncols
         if(mode_image .eq. 0) then
          fbuf = ibuf(i)
          if(fbuf .lt. 0) fbuf = fbuf + 256
         else if(mode_image .eq. 1) then
          fbuf = float(INT(buf(i)))
          if(fbuf .lt. 0) fbuf = fbuf + 65536
         end if
         if(molecular_dynamics) then
C*** square the data and divide by the magic number
          real_max = max(real_max,fbuf)
          den = ((fbuf * fbuf * scalepixel) - fmin) * range_scale
         else
          real_max = max(real_max,fbuf)
          den = fbuf
         end if
         mapbuf(i,n) = den
          dmin = min(dmin,den)
          dmax = max(dmax,den)
          dtot = dtot + den
        end do
       end do
       if(nint(real_max) .eq. max_value)
     *       write(6,'(/
     *  ''!!! Warning - maximum reached, possible saturation''/)') 
C*** write output image format map, turned round
       if(molecular_dynamics) then
       write(6,'(''starting write ncols,nrows'',2i8)')ncols,nrows
C*** write exact number of output rows
        do i=1,nxyz(2)
         do n=1,nrows
          rbuf(n) = mapbuf(ncols-i+1,n)
         end do
         call  iwrlin(idevout,rbuf)
        end do
       write(6,'(''file written'')')
C*** turn upside down
       else if(orientation .eq. 1) then
        do n=1,nrows
         do i=1,ncols
          rbuf(i) = mapbuf(i,nrows-n+1)
         end do
         call  iwrlin(idevout,rbuf)
        end do
C*** do not turn upside down
       else
        do n=1,nrows
         do i=1,ncols
          rbuf(i) = mapbuf(i,n)
         end do
         call  iwrlin(idevout,rbuf)
        end do
       end if
C*** rewrite header with new min, max, mean
       dnum = float(ncols * nrows)
       dmean = dtot / dnum
       write(6,'('' Map calculated, min, max, mean =''/3f10.1)')
     *  dmin, dmax, dmean
       call  iwrhdr(idevout,title,-1,dmin,dmax,dmean)
       call  imclose(idevout)
C       call system('\rm tag_data')
       stop
C*** tag not found
C 8000   call system('\rm tag_data')
       stop
C*** tag error
 8100   write(6,'('' Tag '',i6,'' value unknown/incorrect''/
     *  '' tag values :'',4i10)') 
     *   itag, field_type, length, value_offset
C       call system('\rm tag_data')
       stop
C*** value check error
 8200   write(6,'('' Output file not written'')')
C       call system('\rm tag_data')
       stop
C*** error
 9000   write(6,'('' Error reading input file'')')
C       call system('\rm tag_data')
       stop
       end
C***********************************************************************
C***
       subroutine byteswaptwobytes(buffer,nbytes)
C***
C**********************************************************************
       byte       buffer(nbytes)
       byte       btemp
C***
       do i=1,nbytes,2
        btemp = buffer(i)
        buffer(i) = buffer(i+1)
        buffer(i+1) = btemp
       end do
       return
       end
C***********************************************************************
C***
       subroutine byteswapfourbytes(buffer,nbytes)
C***
C**********************************************************************
       byte       buffer(nbytes)
       byte       btemp
C***
       do i=1,nbytes,4
        btemp = buffer(i)
        buffer(i) = buffer(i+3)
        buffer(i+3) = btemp
        btemp = buffer(i+1)
        buffer(i+1) = buffer(i+2)
        buffer(i+2) = btemp
       end do
       return
       end
C**************************************************************
       integer function lnblank(text)
C***************************************************************
C*** function to return the number of non-blank characters in a 
C*** string and set remainder to ' '
       character*(*) text
       character zer0
       zer0 = char(0)
       jchars = len(text)
       do 100 i=jchars,1,-1
        lnblank = i
        if(text(i:i) .eq. zer0) then
         text(i:i) = ' '
         go to 100
        end if
        if(text(i:i).ne.' ') return
  100        continue
       return
       end
