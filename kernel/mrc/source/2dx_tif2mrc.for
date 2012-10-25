C******************************************************************
C***       tif2mrc
C***    Program to read tiff format file reformat the output as 
C***       MRC IMAGE format
C
C This version was extended for interplay with the 2dx package.
C
C Specifically, it interprets the TVIPS TIFF file header and outputs
C image-specific values into a 2dx readable data file.
C
C  Last modified: 2012/10/23, Henning Stahlberg
C   http://2dx.org
C
C******************************************************************
C***
       implicit none
C
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
       integer*4       istandard_step_size
       parameter      (istandard_step_size = 400)
       integer*4       ipixels_per_cm
       parameter      (ipixels_per_cm = 10000 / istandard_step_size)
C***
       byte            btest(2)
       byte            byte_buf(maxcols*2)
       byte            byte_value_type1
C***
       character       filin*256
       character       filout*256
       character       text*80
       character       title_string*80
       character       cvalue_type2
       character       yesno*1
       character       cstring*200
C
       character       cline*200
       character       cline1*200
       character       cline2*200
       character       cline3*200
C***
CHENN>
C------This may be a compiler/platform dependent thing: 
C------See also line 572.
C       integer*4       buf(maxcols)
       integer*2       int2_buf(maxcols)
CHENN<
       integer*2       int2_field_type
       integer*2       int2
       integer*2       int2_test
       integer*2       int2_orientation
       integer*2       int2_tag
       integer*2       int2_value_type3
C***
       integer*4       idenominator
       integer*4       ifiletag
       integer*4       ioffset
       integer*4       int4
       integer*4       itmp
       integer*4       length
       integer*4       lnblank
       integer*4       mxyz(3)
       integer*4       nxyz(3)
       integer*4       numerator
       integer*4       iresolutionunit
       integer*4       isampleformat
       integer*4       istripbytecounts
       integer*4       istripoffsets
       integer*4       ivalue_offset
       integer*4       ivalue_type4
       integer*4       ixresolution
       integer*4       iyresolution
       integer*4       itvips_offset
C***
       logical         bigendian
       logical         byteswap
       logical         chess
       logical         LTVIPS
       logical         molecular_dynamics
       logical         adobe_photoshop
       logical         newtag
C***
       real*4          mapbuf(maxcols,maxrows)
       real*4          rbuf(maxcols)
       real*4          scalepixel
       real*4          title(20)
       real*4          ftmp
       real*4          postmag
       real*4          fvector(20)
C***
       real*8          dnum
       real*8          dtot
C***
C
       integer i,ielement,ier,itemp,max_value,min_value,mode_image,n
       integer idevin,nbits_per_sample,nbytes_per_row,nchitm,ncols
       integer nentries,nitems,nrows,nrowsperstrip,nstaples_per_pixel
       integer nsecs,nstripsperimage,nsamples_per_pixel,itag
       integer j,k,l
C
       real den,dmin,dmax,dmean,fbuf,fmin,fmax,frange
       real range_scale,real_max
       real rdef,rdef1,rdef2,rdefang,rastig,rtmp
       real rbinning,rpixsize
C
       real PI
C
C*** test machine little or big endian 
       equivalence       (btest(1),int2_test)
       equivalence       (int2_buf,byte_buf)
       equivalence       (ioffset,byte_buf(1))
       equivalence       (int2_tag,byte_buf(1))
       equivalence       (int2_field_type,byte_buf(3))
       equivalence       (length,byte_buf(5))
       equivalence       (ivalue_offset,byte_buf(9))
       equivalence       (byte_value_type1,byte_buf(9))
       equivalence       (cvalue_type2,byte_buf(9))
       equivalence       (int2_value_type3,byte_buf(9))
       equivalence       (ivalue_type4,byte_buf(9))
       equivalence       (numerator,byte_buf(1))
       equivalence       (idenominator,byte_buf(5))
       equivalence       (int2,byte_buf(1))
       equivalence       (int4,byte_buf(1))
       equivalence       (cstring,byte_buf(1))
       equivalence       (fvector(1),byte_buf(1))
C***
       common/big/mapbuf
C
       PI = 3.14159265437
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
       int2_orientation = 1
       btest(1) = 0
       btest(2) = 0
       int2_test = 1
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
     *  '' Type input file name'')')
       read(5,'(a)') filin
  900       write(6,'(
     *  '' Type output file name'')')
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
       call qread(idevin,byte_buf,nitems,ier)
       if(ier.ne.0) go to 9000
C*** test first two bytes contain intel format
       write(text(1:1),'(a)') byte_buf(1)
       write(text(2:2),'(a)') byte_buf(2)
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
C
 1000  continue
C
       call qseek(idevin,istartblock,ielement,lrecl)
       nitems = 4
       call qread(idevin,byte_buf,nitems,ier)
       if(ier.ne.0) go to 9000
       if(byteswap)then
         call byteswapfourbytes(byte_buf(1),nitems)
       endif
       if(ioffset .eq. 0)then
         write(6,'(''...no further directory entries.'',/)')
         go to 3000
       endif
C*** read number of entries for this directory
       ielement = ioffset + 1
       write(6,'(''Reading position '',I6)') ielement
       call qseek(idevin,istartblock,ielement,lrecl)
       nitems = 2
       call qread(idevin,byte_buf,nitems,ier)
       if(byteswap) call byteswaptwobytes(byte_buf(1),nitems)
       nentries = int2
       write(6,'(/,''Reading '',I6,'' directory entries'')') nentries
C*** loop to extract tag information
       ielement = ielement + nitems
       nitems = 12
C*****************************************************************
       open(unit=4,file='tag_data',status='unknown')
C*****************************************************************
C*****************************************************************
C*****************************************************************
       do n=1,nentries
        write(6,'(/,''Reading position '',I6)') ielement
        call qseek(idevin,istartblock,ielement,lrecl)
        call qread(idevin,byte_buf,nitems,ier)
        if(byteswap) then
         call byteswaptwobytes(byte_buf(1),4)
         call byteswapfourbytes(byte_buf(5),4)
         if(int2_field_type .eq. 3) then
          call byteswaptwobytes(byte_buf(9),2)
         else
          call byteswapfourbytes(byte_buf(9),4)
         end if
        end if
        itag = int2_tag
        if(itag .lt. 0) itag = 65536 + itag
C*****************************************************************
C*** extract tag information
C*****************************************************************
C
C*** NewSubfileType
        if(itag .eq. 254) then
         write(6,'(''TAG '',I6,'' NewSubfileType'')')itag
         if(ivalue_type4 .ne. 0) then
          write(6,'(''::Warning - multiple image file,''
     *    '' only first image used'')')
          go to 3000
         end if
C*** SubfileType
        else if(itag .eq. 255) then
         write(6,'(''TAG '',I6,'' SubfileType'')')itag
         if(int2_value_type3 .ne. 1) then
          write(6,'(''::Image not full resolution'')')
          go to 8100
         end if
C*** ImageWidth
         else if(itag .eq. 256) then
         if(int2_field_type .eq. 3) then
          ncols = int2_value_type3
         else
          ncols = ivalue_type4
         end if
         write(6,'(''TAG '',I6,'' ImageWidth = '',i8)')itag,ncols
C*** ImageLength
        else if(itag .eq. 257) then
         if(int2_field_type .eq. 3) then
          nrows = int2_value_type3
         else
          nrows = ivalue_type4
         end if
         write(6,'(''TAG '',I6,'' ImageLength = '',i8)')itag,nrows
C***  BitsPerSample
        else if(itag .eq. 258) then
         if(int2_value_type3 .eq. 8) then
          mode_image = 0
         else if(int2_value_type3 .eq. 16) then
          mode_image = 1
         else
          write(6,'(/
     *     ''::!! Tag '',i3,'' indicates RGB colour image''/ 
     *     ''::   tif2mrc cannot read colour images : '',
     *     ''try tiff2bw to convert to grayscale first.'')')
     *     itag
          stop
         end if
         nbits_per_sample = int2_value_type3
         write(6,'(''TAG '',I6,'' BitsPerSample = '',i5)') itag,nbits_per_sample
C*** Compression
        else if(itag .eq. 259) then
         write(6,'(''TAG '',I6,'' Compression'')')itag
         if(int2_value_type3 .ne. 1) go to 8100
C*** Photometric
        else if(itag .eq. 262) then
         write(6,'(''TAG '',I6,'' Photometric'')')itag
         if(int2_value_type3 .lt. 0 .or. int2_value_type3 .gt. 3) 
     *    go to 8100
C*** FillOrder
        else if(itag .eq. 266) then
         write(6,'(''TAG '',I6,'' FillOrder'')')itag
         if(int2_value_type3 .ne. 1) go to 8100
C*** DocumentName
        else if(itag .eq. 269) then
         write(6,'(''TAG '',I6,'' DocumentName'')')itag
         go to 2000
C*** ImageDescription
        else if(itag .eq. 270) then
         write(6,'(''TAG '',I6,'' ImageDescription'')')itag
         go to 2000
C*** Make
        else if(itag .eq. 271) then
         write(6,'(''TAG '',I6,'' Make'')')itag
         go to 2000
C*** Model
        else if(itag .eq. 272) then
         write(6,'(''TAG '',I6,'' Model'')')itag
         go to 2000
C*** StripOffsets
        else if(itag .eq. 273) then
         if(int2_field_type .eq. 3) then
          istripoffsets = int2_value_type3
         else
          istripoffsets = ivalue_type4
         end if
C*** if length > 1 read offset to get value for start of image
         if(length .gt. 1) then
          call qseek(idevin,istartblock,istripoffsets+1,lrecl)
          if(int2_field_type .eq. 3) then
           call qread(idevin,byte_buf,2,ier)
           if(byteswap) 
     *      call byteswaptwobytes(byte_buf(1),2)
           istripoffsets = int2
          else
           call qread(idevin,byte_buf,4,ier)
           if(byteswap) 
     *      call byteswapfourbytes(byte_buf(1),4)
           istripoffsets = int4
          end if
         end if
C*** should really read all the values and check their differences are identical
         write(6,'(''TAG '',I6,'' StripOffsets ='',i8)') itag,istripoffsets
C*** Orientation
        else if(itag .eq. 274) then
         int2_orientation = int2_value_type3
         write(6,'(''TAG '',I6,'' Orientation ='',i5)') itag,int2_orientation
         if(int2_orientation .ne. 1 .and. int2_orientation .ne. 4) go to 8100
C*** SamplesPerPixel
        else if(itag .eq. 277) then
         if(int2_value_type3 .lt. 1 .or. int2_value_type3 .gt.2) 
     *    go to 8100
         nsamples_per_pixel = int2_value_type3
         write(6,'(''TAG '',I6,'' SamplesPerPixel ='',i8)') itag,
     *    nsamples_per_pixel 
C*** RowsPerStrip
        else if(itag .eq. 278) then
         if(int2_field_type .eq. 3) then
          nrowsperstrip = int2_value_type3
         else
          nrowsperstrip = ivalue_type4
         end if
         write(6,'(''TAG '',I6,'' RowsPerStrip ='',i8)') itag,nrowsperstrip
         nstripsperimage = (nrows + nrowsperstrip - 1) / nrowsperstrip
C*** StripByteCounts
        else if(itag .eq. 279) then
C*** read value directly
         if(length .eq. 1) then
          if(int2_field_type .eq. 3) then
           istripbytecounts = int2_value_type3
          else
           istripbytecounts = ivalue_type4
          end if
C*** read value from offset
         else
          call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
          if(int2_field_type .eq. 3) then
           call qread(idevin,byte_buf,2,ier)
           if(byteswap) 
     *      call byteswaptwobytes(byte_buf(1),2)
           istripbytecounts = int2
          else
           call qread(idevin,byte_buf,4,ier)
           if(byteswap) 
     *      call byteswapfourbytes(byte_buf(1),4)
           istripbytecounts = int4
          end if
C*** check istripbytecounts identical. If not, program needs rewrite
C*** to accommodate pointer reading for each strip
          if(nstripsperimage .gt. 1) then
           do i=1,nstripsperimage
            itemp = istripbytecounts
            call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
            if(int2_field_type .eq. 3) then
             call qread(idevin,byte_buf,2,ier)
             if(byteswap) 
     *        call byteswaptwobytes(byte_buf(1),2)
             istripbytecounts = int2
            else
             call qread(idevin,byte_buf,4,ier)
             if(byteswap) 
     *        call byteswapfourbytes(byte_buf(1),4)
             istripbytecounts = int4
            end if
            if(itemp .ne. istripbytecounts) then
             write(6,'(
     *        '':: Warning - uneven istripbytecounts. '',
     *        ''Output file may be incorrect'')')
             go to 2000
            end if
           end do
          end if
         end if
         write(6,'(''TAG '',I6,'' StripByteCounts ='',i8)') itag,istripbytecounts
C*** MinSampleValue
        else if(itag .eq. 280) then
         min_value = int2_value_type3
         if(min_value .lt. 0) min_value = min_value + 65536
         write(6,'(''TAG '',I6,'' MinSampleValue = '',i8)') itag,min_value
         go to 2000
C*** MaxSampleValue
        else if(itag .eq. 281) then
         max_value = int2_value_type3
         if(max_value .lt. 0) max_value = max_value + 65536
         write(6,'(''TAG '',I6,'' MaxSampleValue = '',i8)') itag,max_value
         go to 2000
C*** XResolution
        else if(itag .eq. 282) then
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,8,ier)
         if(byteswap) call byteswapfourbytes(byte_buf(1),8)
         ixresolution = nint(float(numerator) / float(idenominator))
         write(6,'(''TAG '',I6,'' XResolution = '',I9)')itag,ixresolution
         go to 2000
C*** YResolution
        else if(itag .eq. 283) then
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,8,ier)
         if(byteswap) call byteswapfourbytes(byte_buf(1),8)
         iyresolution = nint(float(numerator) / float(idenominator))
         write(6,'(''TAG '',I6,'' YResolution = '',I9)')itag,iyresolution
         go to 2000
C*** PlanarConfiguration
        else if(itag .eq. 284) then
         write(6,'(''TAG '',I6,'' PlanarConfiguration'')')itag
         if(int2_value_type3 .ne. 1) go to 8100
C*** PageName
        else if(itag .eq. 285) then
         write(6,'(''TAG '',I6,'' PageName'')')itag
         go to 2000
C*** XPosition
        else if(itag .eq. 286) then
         write(6,'(''TAG '',I6,'' XPosition'')')itag
         go to 2000
C*** YPosition
        else if(itag .eq. 287) then
         write(6,'(''TAG '',I6,'' YPosition'')')itag
         go to 2000
C*** ResolutionUnit
        else if(itag .eq. 296) then
         iresolutionunit = int2_value_type3
         write(6,'(''TAG '',I6,'' ResolutionUnit = '',I8)')itag,iresolutionunit
         go to 2000
C*** Software
        else if(itag .eq. 305) then
         write(6,'(''TAG '',I6,'' Software'')')itag
         go to 2000
C*** DateTime
        else if(itag .eq. 306) then
         write(6,'(''TAG '',I6,'' DateTime'')')itag
         go to 2000
C*** Artist
        else if(itag .eq. 315) then
         write(6,'(''TAG '',I6,'' Artist'')')itag
         go to 2000
C*** HostComputer
        else if(itag .eq. 316) then
         write(6,'(''TAG '',I6,'' HostComputer'')')itag
         go to 2000
C*** SampleFormat
        else if(itag .eq. 339) then
         isampleformat = int2_value_type3
         write(6,'(''TAG '',I6,'' SampleFormat ='',I6)') itag,isampleformat
         if(isampleformat .ne. 1) go to 8100
C*** ImageDepth
        else if(itag .eq. 32997) then
         if(int2_field_type .eq. 3) then
          nsecs = int2_value_type3
         else
          nsecs = ivalue_type4
         end if
         if(nsecs .ne. 1) then
          write(6,'('':: Multiple section file..cannot continue'')')
          go to 8200
         end if
         write(6,'(''TAG '',I6,'' ImageDepth = '',i6)') itag,nsecs
C*** Private Molecular Dynamics tag - md_filetag
        else if(itag .eq. 33445) then
         molecular_dynamics = .true.
         ifiletag = ivalue_type4
         go to 2000
C*** Private Molecular Dynamics tag - md_scalepixel
        else if(itag .eq. 33446) then
         molecular_dynamics = .true.
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,8,ier)
         if(byteswap) call byteswapfourbytes(byte_buf(1),8)
         scalepixel = float(numerator) / float(idenominator)
         go to 2000
C*** Private Molecular Dynamics tag - md_colortable
        else if(itag .eq. 33447) then
         write(6,'(''TAG '',I6,'' Private Molecular Dynamics tag'')')itag
         molecular_dynamics = .true.
         go to 2000
C*** Private Molecular Dynamics tag - md_labname
        else if(itag .eq. 33448) then
         write(6,'(''TAG '',I6,'' Private Molecular Dynamics tag'')')itag
         molecular_dynamics = .true.
         go to 2000
C*** Private Molecular Dynamics tag - md_sampleinfo
        else if(itag .eq. 33449) then
         write(6,'(''TAG '',I6,'' Private Molecular Dynamics tag'')')itag
         molecular_dynamics = .true.
         go to 2000
C*** Private Molecular Dynamics tag - md_prepdate
        else if(itag .eq. 33450) then
         write(6,'(''TAG '',I6,'' Private Molecular Dynamics tag'')')itag
         molecular_dynamics = .true.
         go to 2000
C*** Private Molecular Dynamics tag - md_preptime
        else if(itag .eq. 33451) then
         write(6,'(''TAG '',I6,'' Private Molecular Dynamics tag'')')itag
         molecular_dynamics = .true.
         go to 2000
C*** Private Molecular Dynamics tag - md_fileunits
        else if(itag .eq. 33452) then
         write(6,'(''TAG '',I6,'' Private Molecular Dynamics tag'')')itag
         molecular_dynamics = .true.
         go to 2000
C*** Private Adobe Photoshop tag - unknown value
        else if(itag .eq. 34377) then
         adobe_photoshop = .true.
         write(6,'('' Private Adobe Photoshop tag = '',i5, 
     *    '' type = '',i5,'' length = '',i5,'' offset ='',i8)') 
     *    itag, int2_field_type, length, ivalue_offset
         go to 2000
C*** Private Chess tag(s)
        else if(itag .ge. 36864 .and. itag .le. 37120) then
         write(6,'(''TAG '',I6,'' Private Chess tag(s)'')')itag
         chess = .true.
         go to 2000
C*** Private TVIPS tag(s)
        else if(itag .eq. 37706) then
         write(6,'(''TAG '',I6,'' Private TVIPS tag'')')itag
         LTVIPS = .true.
         itvips_offset = ivalue_type4
         go to 2000
C*** unknown tag
        else
         newtag = .true.
         write(6,'('' Unknown tag = '',i5, 
     *    '' type = '',i5,'' length = '',i5,'' offset ='',i8)') 
     *    itag, int2_field_type, length, ivalue_offset
        end if
 2000   continue
        ielement = ielement + nitems
        write(6,'(''At end of loop, ielement is '',I6)') ielement
       end do
       go to 1000
C*****************************************************
C*** open output image format file
C*****************************************************
C*** check map not too large for program
 3000  continue
       if(ncols.gt.maxcols) then
         write(6,'('' ncols > maxcols = '',2i8)') ncols, maxcols        
         go to 8200
       else if(nrows.gt.maxrows) then
          write(6,'('' nrows > maxrows = '',2i8)') nrows, maxrows        
         go to 8200
       end if
C*** unknown tag ?
       if(newtag) then
          write(6,'(''::WARNING: Unknown tags encountered.'')')
       end if
C
C------TVIPS tag?
       if(LTVIPS)then
         write(6,'(//,'':Reading TVIPS private tags'')')
C
         call system("\rm -f TVIPS_data.txt")
         open(26,FILE="TVIPS_data.txt",STATUS='NEW')
C
         ivalue_offset = itvips_offset
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,4,ier)
         if(byteswap) call byteswapfourbytes(byte_buf(1),8)
         itmp = int4 
         write(6,'(/,''TVIPS file, Version number '',I8)')itmp
C
         ivalue_offset = itvips_offset + 4
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,80,ier)
         read(cstring(1:80),'(A)')cline
         call shorten(cline,k)
         write(6,'(''Comment: '',A)')cline(1:k)
         write(26,'(''set comment = '',A)')cline(1:k)
C
         ivalue_offset = itvips_offset + 84
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,4,ier)
         ftmp = real(int4) * 0.0001
         write(6,'(''High Tension [kV]............: '',F12.3)')ftmp
         write(26,'(''set kV = '',F12.3)')ftmp
C
         ivalue_offset = itvips_offset + 88
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,4,ier)
         ftmp = real(int4) * 0.0001
         write(6,'(''Spherical Abberation [mm]....: '',F12.3)')ftmp
         write(26,'(''set CS = '',F12.3)')ftmp
C
         ivalue_offset = itvips_offset + 96
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,4,ier)
         ftmp = real(int4) * 0.0001
         write(6,'(''Magnification [kx]...........: '',F12.3)')ftmp
         write(26,'(''set magnification = '',F12.3)')ftmp
C
         ivalue_offset = itvips_offset + 100
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,4,ier)
         ftmp = real(int4) * 0.0001
         postmag = ftmp
         write(6,'(''Post Magnification [x].......: '',F12.3)')ftmp
C
         ivalue_offset = itvips_offset + 108
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,4,ier)
         ftmp = real(int4) * 0.0001
         rdef = ftmp
         write(6,'(''Defocus [nm].................: '',F12.3)')ftmp
C
         ivalue_offset = itvips_offset + 112
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,4,ier)
         ftmp = real(int4) * 0.0001
         rastig = ftmp
         write(6,'(''Astigmatism [nm].............: '',F12.3)')ftmp
C
         ivalue_offset = itvips_offset + 116
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,4,ier)
         ftmp = real(int4) * 0.0001
         rdefang = ftmp
         write(6,'(''Astigmatism direction [mrad].: '',F12.3)')ftmp
C
         rdef1=rdef+rastig*cos(rdefang*PI/180.0)
         rdef2=rdef+rastig*sin(rdefang*PI/180.0)
         write(cline,'(F12.3,'','',F12.3,'','',F12.3)')rdef1,rdef2,rdefang
         call inkomma(cline,k)
         write(6,'(''Defocus and Astig............: '',A)')cline(1:k)
         write(26,'(''set defocus = '',A)')cline(1:k)
C
         ivalue_offset = itvips_offset + 124
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,4,ier)
         ftmp = real(int4) * 0.0001
         write(6,'(''Specimen tilt angle [deg]....: '',F12.3)')ftmp
         write(26,'(''set TLTANG = '',F12.3)')ftmp
C
         ivalue_offset = itvips_offset + 128
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,4,ier)
         ftmp = real(int4) * 0.0001
         write(6,'(''Specimen tilt direction [deg]: '',F12.3)')ftmp
         write(26,'(''set TLTAXA = '',F12.3)')ftmp
C
         ivalue_offset = itvips_offset + 140
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,4,ier)
         if(int4.eq.0)then
           write(cline,'(''        Image'')')
         else
           write(cline,'(''PowerSpectrum'')')
         endif
         call shorten(cline,k)
         write(6,'(''Image Mode...................:'',A)')cline(1:k)
C
         ivalue_offset = itvips_offset + 168
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,4,ier)
         ftmp = int4 * 0.0001
         write(6,'(''Origin X.....................: '',F12.3)')ftmp
C
         ivalue_offset = itvips_offset + 172
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,4,ier)
         ftmp = int4 * 0.0001
         write(6,'(''Origin Y.....................: '',F12.3)')ftmp
C
         ivalue_offset = itvips_offset + 176
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,4,ier)
         ftmp = int4 * 0.0001
         rpixsize = ftmp
         write(6,'(''Pixel size [um]..............: '',F12.3)')ftmp
C
         ivalue_offset = itvips_offset + 180
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,4,ier)
         ftmp = int4 * 0.0001
         rbinning = ftmp
         write(6,'(''Binning Factor...............: '',F12.3)')ftmp
         ftmp = ( rpixsize * rbinning ) / postmag
         write(26,'(''set stepdigitizer = '',F12.3)')ftmp
C
         ivalue_offset = itvips_offset + 580
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,4,ier)
         write(6,'(''Data Type ...................: '',I12  )')int4
C
C         ivalue_offset = itvips_offset + 3272
C         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
C         call qread(idevin,byte_buf,4,ier)
C         write(6,'(''High Tension [Volts].........: '',F12.3)')fvector(1)
C
C         ivalue_offset = itvips_offset + 3536
C         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
C         call qread(idevin,byte_buf,4,ier)
C         write(6,'(''Current Mag rel.to plate.....: '',F12.3)')fvector(1)
C
C         ivalue_offset = itvips_offset + 3540
C         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
C         call qread(idevin,byte_buf,4,ier)
C         write(6,'(''Corrected Magnification......: '',F12.3)')fvector(1)
C
C         ivalue_offset = itvips_offset + 3544
C         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
C         call qread(idevin,byte_buf,4,ier)
C         write(6,'(''Post Magnification...........: '',F12.3)')fvector(1)
C
         ivalue_offset = itvips_offset + 3552
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,20,ier)
         write(6,'(''Stage Position X [um] .......: '',F12.3)')fvector(1)
         write(6,'(''Stage Position Y [um] .......: '',F12.3)')fvector(2)
         write(6,'(''Stage Position Z [um] .......: '',F12.3)')fvector(3)
         write(6,'(''Stage Position A [deg] ......: '',F12.3)')fvector(4)
         write(6,'(''Stage Position B [deg] ......: '',F12.3)')fvector(5)
         write(cline,'(F12.3,'','',F12.3)')fvector(1),fvector(2)
         call inkomma(cline,k)
         write(26,'(''set grid_stage_position = '',A)')cline(1:k)
C
         ivalue_offset = itvips_offset + 3572
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,8,ier)
         write(6,'(''Image Shift X [a.u.].........: '',F12.3)')fvector(1)
         write(6,'(''Image Shift Y [a.u.].........: '',F12.3)')fvector(2)
         write(cline,'(F12.3,'','',F12.3)')fvector(1),fvector(2)
         call inkomma(cline,k)
         write(26,'(''set grid_image_shift = '',A)')cline(1:k)
C
         ivalue_offset = itvips_offset + 3580
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,8,ier)
         write(6,'(''Beam Shift X [a.u.]..........: '',F12.3)')fvector(1)
         write(6,'(''Beam Shift Y [a.u.]..........: '',F12.3)')fvector(2)
         write(cline,'(F12.3,'','',F12.3)')fvector(1),fvector(2)
         call inkomma(cline,k)
         write(26,'(''set grid_beam_shift = '',A)')cline(1:k)
C
         ivalue_offset = itvips_offset + 3596
         call qseek(idevin,istartblock,ivalue_offset+1,lrecl)
         call qread(idevin,byte_buf,28,ier)
         if(fvector(1).lt.0.1)then
           write(6,'(''No Image Tiling'')')
           write(6,'(''Current Tile Number X........: '',F12.3)')fvector(2)
           write(6,'(''Current Tile Number X........: '',F12.3)')fvector(3)
           write(6,'(''Max Tile Number X............: '',F12.3)')fvector(4)
           write(6,'(''Max Tile Number Y............: '',F12.3)')fvector(5)
           write(6,'(''Overlap X....................: '',F12.3)')fvector(6)
           write(6,'(''Overlap Y....................: '',F12.3)')fvector(7)
         else
           write(6,'(''Image Tiling:'')')
           write(6,'(''Current Tile Number X........: '',F12.3)')fvector(2)
           write(6,'(''Current Tile Number X........: '',F12.3)')fvector(3)
           write(6,'(''Max Tile Number X............: '',F12.3)')fvector(4)
           write(6,'(''Max Tile Number Y............: '',F12.3)')fvector(5)
           write(6,'(''Overlap X....................: '',F12.3)')fvector(6)
           write(6,'(''Overlap Y....................: '',F12.3)')fvector(7)
         endif
C
         write(cline,'(F12.3,'','',F12.3)')fvector(2),fvector(3)
         call inkomma(cline,k)
         write(26,'(''set tile_pos_current = '',A)')cline(1:k)
C
         write(cline,'(F12.3,'','',F12.3)')fvector(4),fvector(5)
         call inkomma(cline,k)
         write(26,'(''set tile_pos_max = '',A)')cline(1:k)
C
         write(cline,'(F12.3,'','',F12.3)')fvector(6),fvector(7)
         call inkomma(cline,k)
         write(26,'(''set tile_pos_overlap = '',A)')cline(1:k)
C
         write(6,'(/,''TVIPS finished.'',/,/)')
         close(26)
       endif
C
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
        mxyz(1) = nxyz(1) / (iyresolution / ipixels_per_cm)
        mxyz(2) = nxyz(2) / (ixresolution / ipixels_per_cm)
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
       ielement = istripoffsets + 1
C*** loop through data to write image format map
       real_max = 0.
       do n = 1,nrows
          call qseek(idevin,istartblock,ielement,lrecl)
        call qread(idevin,byte_buf,nbytes_per_row,ier)
        if(byteswap .and. mode_image .eq. 1) 
     *        call byteswaptwobytes(byte_buf(1),nbytes_per_row)
        ielement = ielement + nbytes_per_row
        do i = 1,ncols
         if(mode_image .eq. 0) then
          fbuf = byte_buf(i)
          if(fbuf .lt. 0) fbuf = fbuf + 256
         else if(mode_image .eq. 1) then
          fbuf = float(INT(int2_buf(i)))
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
       else if(int2_orientation .eq. 1) then
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
     *   itag, int2_field_type, length, ivalue_offset
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
c==========================================================
c
      SUBROUTINE INKOMMA(CZEILE,k)
C
C replaces intermedieate spaces within the actual text string
C in CZEILE up to the length k by komma.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1,CTMP2,CTMP3
      CHARACTER * 200 CZEIL2
      CTMP2=' '
      CTMP3=','
C
      ilen=len(czeile)
      DO 70 I=1,ilen
         k=ilen+1-I
         READ(CZEILE(k:k),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)GOTO 80
  70  CONTINUE
  80  CONTINUE
      IF(k.LT.1)k=1
C
C-----find the leading spaces and remove them
C
      DO 90 J=1,k
         READ(CZEILE(J:J),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)THEN
           GOTO 95
         ENDIF
 90   CONTINUE
 95   CONTINUE
C
      WRITE(CZEIL2(1:k),'(A)') CZEILE(J:k)
C
C     WRITE(*,'('' INKOMMA 2: k='',I4,'' J='',I4,'':'',A)')
C    1      k,j,CZEIL2(1:k)
C
C-----Was there a komma recently ?
      KWAR=0
C
      I=1
      L=1
 100  continue
        READ(CZEIL2(I:I),'(A1)')CTMP1
        IF(CTMP1.EQ.CTMP3)THEN
C---------There is a komma.
          IF(KWAR.EQ.0)THEN
C-----------There is a komma, before was no komma. Insert one.
            WRITE(CZEILE(L:L),'('','')')
            KWAR=1
          ELSE
C-----------There is a komma, before was already a komma. Shrink.
            L=L-1
          ENDIF
        ELSEIF(CTMP1.EQ.CTMP2)THEN
C---------There is a space.
          IF(KWAR.EQ.0)THEN
C-----------There is a space, before was no komma. Insert komma.
            WRITE(CZEILE(L:L),'('','')')
            KWAR=1
          ELSE
C-----------There is a space, before was a komma. Shrink.
            L=L-1
          ENDIF
        ELSE
C---------There is no komma, no space. Anulate KWAR, do nothing.
          WRITE(CZEILE(L:L),'(A1)')CZEIL2(I:I)
          KWAR=0
        ENDIF
        I=I+1
        L=L+1
C       WRITE(*,'('' INKOMMA 2b: I='',I4,'' L='',I3,'':'',A)')I,L,CTMP1
C
      IF(I.LE.K)GOTO 100
C
C     WRITE(*,'('' INKOMMA 3: k='',I4,'':'',A)')k,CZEILE(1:k)
C
      IF(L.LE.K)THEN
        WRITE(CZEILE(L:K),'('' '')')
      ENDIF
C
C-----Now check, if the last sign is a komma. If so, remove it.
C
      K=L
      I=K
 200  continue
        READ(CZEILE(I:I),'(A1)')CTMP1
        IF(CTMP1.NE.CTMP2)GOTO 220
        I=I-1
      IF(I.GE.1)GOTO 200
 220  CONTINUE
C
      K=I
C
      IF(CTMP1.EQ.CTMP3)then
        WRITE(CZEILE(I:I),'('' '')')
        K=K-1
      endif
C
      if(k.lt.1)k=1
C
C      WRITE(*,'('':: INKOMMA 4: k='',I4,'': "'',A,''"'')')k,CZEILE(1:k)
C
      RETURN
      END
C
c==========================================================


