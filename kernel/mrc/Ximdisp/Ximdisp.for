C*********************************************************************
C        Ximdisp
C	26.08.05 MARK 19.0 RELEASE. 
C	Medical Research Council,20 Park Crescent, LONDON W1N 4Al
C	COPYRIGHT 1999.
C     	WRITTEN BY Judith M. Short
C       J. Struct. Biol. (1999) 125, 223-228
C*********************************************************************
C
C	Ximdisp program to menu driven interactive graphics display 
C	program to display/manipulate images/transforms in MRC image 
C	format
C
C**********************************************************************
C
C       libraries called : imsubs.for, diskio, unix, ccplib, library.c
C	Ximagelibf.for, Ximagelibc.c, ifftsub.for, subsc.c, harwell.for
C
C**********************************************************************
C
C      note:
C         1) this program  inverts along y !!!!!
C	thus point 0,0 which is the first point in the map
C	is located in the lower-left corner.
C
C			top of page
C		^ !
C		! !
C		! !
C		y !
C		  !_____________   x-->
C		0,0
C
C
C         2) unit numbers used:
C                         1 input image file
C                         2 output coordinate file
C                         3 input coordinate file
C                         4 output boxed area
C                         5 input data
C                         6 terminal output
C                         7 output lattice vectors
C                         8 output indexed films
C                         9 output sections
C                        10 output averaged density
C
C***
C*** The program can (optionally) be driven directly from a 
C*** command script as far as the main menu using the switch -s. 
C*** It reads the input parameters for the standard input, and
C*** only works for single section images. The script should 
C*** look something like this :
C***
C*** #!/bin/tcsh
C*** #
C*** Ximdisp -s << 'eot'
C*** /ss1/jms/maps/huge.map                       (input file name)
C*** / 						  (file limits)
C*** /						  (compression factor) 
C*** /                                            (density limits)
C*** / 						  (centre of image)
C*** 'eot'
C***
C************************************************************************
C***	V16.6 : 11.05.2001
C***		phaseworld colour table error corrected
C***		interactive fft - scaling of calculated transform correction
C***		interactive fft - rewrite of logic
C***		lattice refinement - origin fault corrected
C***		multisection display - new feature on adding will display
C***		below previous images
C***		pointer tracking - displays screen coords (not map)
C***		for multisection images
C***		scrolling sections inverted for compatibility
C***		splinefit moves fitted window with panning main image
C***	V16.9   autolabelling switch in output coords
C***	V17.0	Menu bug fixed for LINUX versions
C***	V17.1   Bug fix in displaying scrolling sections for compressed 
C***            images.
C***		Modifications to vector measurements in output_coords
C***	V17.15  Bug fix to average boxes - calculate all boxes added
C***	        together twice when output to a file
C***	V17.21  Bug fix to polygonal box - iolabel overflowed
C***	V18.0   31.03.04
C***		Mods to multisection display including deletion of
C***	        sections, use of off-screen pixmap and section numbering
C***		Mods to output_coords - slider bar with contrast modification
C***		and ability to read coords, edit then output to a stack
C***		but cannot modify and existing stack. Various bug fixes follow.
C***		Bug fix to average boxed areas. Bug fix to box an area (sections)
C***	V18.1   14.03.05
C***		Bug fix to box area of compressed file where compression
C***		factor not a multiple of the box size.
C***	V18.15  16.03.05 Bug fix to fft display from a section from
C***		a multisection image.
C***    V18.16  07.04.05 Bug fix to interactive FFT - call to polygon
C***		had not reset min/max values properly
C***	V18.2   20.06.05 modified section editing to remove numbers and
C***            draw a cross to indicate deletion.
C***	V19.0	26.08.05 24-bit colour installed. Various bug fixes.
C************************************************************************
C***
C*** start of main program
C***

C************************************************************************
	include		'Ximdisp_common.for'
C***
C*** check date for trial version
        if(trial) then
         call getdate(date,nsecs)
         ndays = ntrialdays - (nsecs - nstartdate) / nsecsperday
         if(ndays .le. 0) then
          write(6,'(''Ximdisp - 1 year trial version expired.'')')
          stop
         end if
        end if
C*** decide if interactive version or driven by standard input
	call getarg(1,switch)
C*** normal interactive mode
	if(switch .eq. ' ') then
	 idevin = 0
	 outswitch = .false.
C*** program driven by batch job as far as main menu
	else if(switch .eq. '-s') then
	 idevin = 5
	 outswitch = .false.
C*** options read from data file as far as main menu
	else if(switch .eq. '-f') then
	 outswitch = .true.
	 idevin = idevdef
	 call getarg(2,defaultfile)
C*** check for presence of input file
	 there = .false.
         inquire(file=defaultfile,exist=there)
	 if(.not.there) then
	  idevin = 0
	 else
	  open(unit=idevin,file=defaultfile,status='old')
	 end if
C*** open file for default output
	 open(unit=idevout,file=outputfile,status='unknown')
	else
	 write(6,'('' Unknown switch, exiting.'')')
	end if
C***
C*** initialize screen
	icurs = 0
	max_screen_width = 1
	max_screen_height = 1
	call ximageinit(max_screen_width, max_screen_height)
	if(max_screen_width * max_screen_height .gt. max_phasebuf) then
	 write(6,'(''ERROR - Screen size too great for program !!''/
     *             '' max_phasebuf must be increased to'',i8)')
     *   max_screen_width * max_screen_height
	 stop
	end if
	call ximagechangeorigin
	izoom_length = izoom_length_default
  500	call ximagechangezoom
     *  (izoom_default,izoom_default,izoom_length,izoom_length,ierr)
	if(ierr.ne.0) then
	 izoom_length = izoom_length / 2
	 go to 500
	end if
C***
C*** initialize lookup table to monochrome
	first = .true.
	call colour_table
C***
C*** initialize logical variables
        newmap = .true.
	montage = .false.
        mapread = .false.
	phasemap = .false.
	pointer = .false.
C*** set auto y position for subsequent multisection map position
	iyheight = 0
C***
C*** type in file name
 1000	mapfile = ' '
	nlabels = 1
        iolabel(nlabels) = 
     *  'Ximdisp - v19.0 26.08.05 J. Struct. Biol. (1999) 125, 223-228'
	iolabel(nlabels+1) = 
     *  'To use previous version, type Ximdispold'
	if(idevin .gt. 0) then
	 read(idevin,'(a)') mapfile
	 if(idevin .eq. idevdef) 
     *    iolabel(nlabels+2) = 'Input filename ...'
	else
	 iolabel(nlabels+2) = 'Type in file name ...'
	end if
	if(idevin .eq. 0 .or. idevin .eq. idevdef) then
	 call ximageioboxdisplay(iolabel,mapfile,nlabels+2)
	 old = .true.
	 call check_file(mapfile)
	else
	 there = .false.
         inquire(file=mapfile,exist=there)
	 if(.not.there) then
	  write(6,'(''Input file not found'',a)') mapfile
	  stop
	 end if
	end if
	if(outswitch) write(idevout,'(a)') mapfile
C**********************************************************************
C*** open image input file
C**********************************************************************
 1100   mapread = .true.
        iolabel(nlabels) = 
     *  'Ximdisp - v19.0 26.08.05 J.Str.Biol(1999)125,223-228: '
     *  //mapfile(1:lnblank(mapfile))
  	call imopen(idevmap,mapfile,'ro')
	call irdhdr(idevmap,nxyz,mxyz,mode,dmin,dmax,dmean)
        amin = dmin
        amax = dmax
C*** check that if read from file, compatible with file input
	if(idevin .eq. idevdef) then
	 read(idevin,*) mapmode, nsecs
	 if(mode .lt. 3 .and. mapmode .ge. 3 .or.
     *      mode .ge. 3 .and. mapmode .lt. 3 .or.
     *      nsecs .gt. 1 .and. nxyz(3) .eq. 1   .or.
     *      nsecs .eq. 1 .and. nxyz(3) .gt. 1) then
	  idevin = 0
	  nlabels = nlabels + 1
	  iolabel(nlabels) =
     *    'Default file specification incompatible with new file,'//
     *    ' proceeding interactively.'
	  call ximagelabeldisplay(iolabel,nlabels)
	 end if
	end if
	if(outswitch) write(idevout,*) mode, nxyz(3)
C*** check array size
	if(nxyz(1).gt.max_width) then
	 iolabel(nlabels+1) = 
     *   'Map dimensions too large for program'
	 iolabel(nlabels+2) = 
     *   'Type any key to exit'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	 call imclose(idevmap)
	 stop
	end if
C**********************************************************************
C*** initialize
C**********************************************************************
 2000	do i=1,3
	 ixyzmin(i) = 0
	 ixyzmax(i) = 0
	end do
	nox = 0
	noy = 0
	noz = 0
	ixyzmax(3) = nxyz(3) - 1
C**********************************************************************
C*** check for image or transform
C**********************************************************************
 2100	if(mode .lt. 3) then
	 image = .true.
C*** single section
	 if(nxyz(3) .eq. 1) then
C*** interactive read
	  if(idevin .eq. 0) then
	   write(iolabel(nlabels+1),
     *     '('' number of points (x,y)='',2i6)') nxyz(1), nxyz(2)
	   iolabel(nlabels+2) = ' $enter limits (xmin,max,ymin,max)'
	   iolabel(nlabels+3) = ' [ (0,0) is lower-left corner ] :'
	   return_string = ' '
 	   call ximageioboxdisplay(iolabel,return_string,nlabels+3)
	   if(return_string .ne. '/' .and. return_string .ne. ' ') then
	    call extract_integers(4,return_string,
     *      ixyzmin(1), ixyzmax(1), ixyzmin(2), ixyzmax(2), i5, i6)
	   end if
C*** read from file
	  else
	   read(idevin,*) ixyzmin(1), ixyzmax(1), ixyzmin(2), ixyzmax(2)
C*** display if default file
	   if(idevin .ne. 5) then
	    write(iolabel(nlabels+1),
     *      '('' number of points (x,y)='',2i5)') nxyz(1), nxyz(2)
	    iolabel(nlabels+2) = ' Default limits (xmin,max,ymin,max)'
	    iolabel(nlabels+3) = ' Type new limits or <cr> to use defaults :'
	    write(return_string,'(i8,'','',i8,'','',i8,'','',i8)') 
     *      ixyzmin(1), ixyzmax(1), ixyzmin(2), ixyzmax(2)
	    return_string = rmblank(return_string,nchars)
 	    call ximageioboxdisplay(iolabel,return_string,nlabels+3)
	    call extract_integers(4,return_string,
     *      ixyzmin(1), ixyzmax(1), ixyzmin(2), ixyzmax(2), i5, i6)
	   end if
	  end if
	 else
C*** interactive read for multisection image
	  if(idevin .eq. 0) then
	   write(iolabel(nlabels+1),
     *     '('' number of points (x,y,z)='',3i5)') nxyz
	   iolabel(nlabels+2) = 
     *     ' $enter limits (xmin,max,ymin,max,zmin,max)'
	   iolabel(nlabels+3) = ' [ (0,0) is lower-left corner ] :'
	   return_string = ' '
 	   call ximageioboxdisplay(iolabel,return_string,nlabels+3)
	   if(return_string .ne. '/' .and. return_string .ne. ' ') then
	    call extract_integers(6,return_string,
     *      ixyzmin(1), ixyzmax(1), ixyzmin(2), 
     *      ixyzmax(2), ixyzmin(3), ixyzmax(3))
	   else
C*** all sections to be displayed
	    ixyzmax(3) =  nxyz(3) - 1
	   end if
C*** read from file
	  else
	   read(idevin,*)
     *      ixyzmin(1), ixyzmax(1), ixyzmin(2), 
     *      ixyzmax(2), ixyzmin(3), ixyzmax(3)
	   if(idevin .ne. 5) then
	    write(iolabel(nlabels+1),
     *      '('' number of points (x,y,z)='',3i5)') nxyz
	    iolabel(nlabels+2) = ' Default limits (xmin,max,ymin,max,zmin,max)'
	    iolabel(nlabels+3) = ' Type new limits or <cr> to use defaults :'
	    write(return_string,'
     *      (i8,'','',i8,'','',i8,'','',i8,'','',i8,'','',i8)') 
     *      ixyzmin(1), ixyzmax(1), ixyzmin(2), 
     *      ixyzmax(2), ixyzmin(3), ixyzmax(3)
	    return_string = rmblank(return_string,nchars)
 	    call ximageioboxdisplay(iolabel,return_string,nlabels+3)
	    call extract_integers(6,return_string,
     *      ixyzmin(1), ixyzmax(1), ixyzmin(2), ixyzmax(2), 
     *      ixyzmin(3), ixyzmax(3))
	   end if
	  end if
	 end if
C***
C*** if min,max both=0 use full range (except in z) else make sure in range
	 do k = 1,3
 	  imin = 0
	  imax = imin + nxyz(k) - 1
	  if (ixyzmin(k).eq.0 .and. 
     *        ixyzmax(k).eq.0 .and. 
     *        k.ne.3) then
	    ixyzmin(k) = imin
	    ixyzmax(k) = imax
	  else
	    if (ixyzmin(k) .lt. imin) ixyzmin(k) = imin
	    if (ixyzmax(k) .gt. imax) ixyzmax(k) = imax
	  end if
	  mxyz(k) = ixyzmax(k) - ixyzmin(k) + 1
	 end do
C*** write to default file
	 if(outswitch) then
	  if(nxyz(3) .eq. 1) then
	   write(idevout,*) ixyzmin(1), ixyzmax(1), ixyzmin(2), ixyzmax(2)
	  else
	   write(idevout,*)
     *     ixyzmin(1), ixyzmax(1), ixyzmin(2), 
     *     ixyzmax(2), ixyzmin(3), ixyzmax(3)
	  end if
	 end if
C**********************************************************************
C*** transform
C**********************************************************************
        else
	 image = .false.
C*** interactive read
	 if(idevin .eq. 0) then
	  write(iolabel(nlabels+1),
     *    '('' number of points (x,y) ='',2i5)') nxyz(1),nxyz(2)
	  iolabel(nlabels+2) = 
     *    '$ enter half number in x, half number in y  :'
	  return_string = ' '
          call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	  if(return_string .ne. '/' .and. return_string .ne. ' ') then
	    call extract_integers(2,return_string,
     *      nox, noy, i3, i4, i5, i6)
C*** add 1 to both to inlude equator and meridian
	   nox = nox + 1
	   noy = noy + 1
	  end if
C*** read from file
	 else
	  read(idevin,*) nox,noy
	  if(idevin .ne. 5) then
	   write(iolabel(nlabels+1),
     *     '('' number of points (x,y) ='',2i5)') nxyz(1),nxyz(2)
	   iolabel(nlabels+2) = 'Modify half number in x, half number in y  :'
	   write(return_string,'(i8,'','',i8)') nox, noy
	   return_string = rmblank(return_string,nchars)
           call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	   call extract_integers(2,return_string, nox, noy, i3, i4, i5, i6)
C*** add 1 to both to inlude equator and meridian
	   nox = nox + 1
	   noy = noy + 1
	  end if
	 end if
	 if(outswitch) write(idevout,*) nox,noy
C***
C*** if min,max both 0 use full range
	 nyhalf = nxyz(2) / 2
         if(nox .eq. 0) then
          nox = nxyz(1)
          noy = nyhalf
         end if
C*** set x values. There are NX complex numbers but first column (meridian)
C*** written once while all other columns twofold mirrored about equator.
         ixyzmin(1) = 0
         ixyzmax(1) = nox - 1
C*** similarly, equator is at bottom of 1st half, and note that 1st row 
C*** will be discarded in fftread
         ixyzmin(2) = nyhalf - noy + 1
         ixyzmax(2) = nyhalf + noy - 1
         ixyzmin(3) = 0
         ixyzmax(3) = 0
C***
         do i=1,3
          mxyz(i) = ixyzmax(i) - ixyzmin(i) + 1
         end do
        end if
	nlabels = 1
C**********************************************************************
C*** pixel compression for images
C**********************************************************************
	icompress = 1
	if(image) then
C*** interactive read
	 if(idevin .eq. 0) then
	  iolabel(nlabels+1) = 
     *    'Enter pixel compression factor <cr> for default (none)'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	  if (return_string .ne. ' ') 
     *	   call extract_integers(1,return_string,icompress,
     *     i2,i3,i4,i5,i6)
C*** read from file
	 else
	  read(idevin,*) icompress
	  if(idevin .ne. 5) then
	   write(iolabel(nlabels+1),'(''Pixel compression factor = '',i1,
     *     '' type a new one or <cr> for default'')') icompress
	   return_string = ' '
	   call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	   if (return_string .ne. ' ') 
     *	   call extract_integers(1,return_string,icompress,
     *      i2,i3,i4,i5,i6)
	  end if
	 end if
	 if(outswitch) write(idevout,*) icompress
	end if
C**********************************************************************
C*** read file limits
C**********************************************************************
C***
C*** note that nxyz is uncorrupted and stands throughout the
C*** program as the original size of the map.mxyz is used to
C*** carry the values specified by the user of this program
C*** to carve out the piece of the map to be displayed.
	nxstart = ixyzmin(1)
        nox = mxyz(1) / icompress
	nxend = nxstart + nox * icompress - 1
C*** set up y values
	nystart = ixyzmin(2)
        noy = mxyz(2) / icompress
	nyend = nystart + noy * icompress - 1
C*** set up z values
 2600   nzstart = ixyzmin(3)
        nzend = ixyzmax(3)
C**********************************************************************
C*** change density limits
C**********************************************************************
 2700   if(idevin .eq. 0) then
	 write(iolabel(nlabels+1),
     *   '('' current min,max density limits = '',2g12.4)')
     *   amin,amax
         write(iolabel(nlabels+2),'(
     *   '' type density limits ("/" or "<cr>"=current limits):'')')  
 2800	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	 nlabels = 1
	 if(return_string .eq. ' ' .or. return_string .eq. '/') then
	  bmin = amin
	  bmax = amax
	 else
	  call extract_reals(2,return_string,bmin,bmax,f3,f4,f5,f6)
	  if(bmin .gt. bmax) go to 2800
	 end if
C*** read from file
	else
	 read(idevin,*) bmin,bmax
	 if(idevin .ne. 5) then
	  write(iolabel(nlabels+1),
     *    '('' file min,max density limits = '',2g12.4)') amin,amax
	  iolabel(nlabels+2) = 'Modify default values :'
	  return_string = ' '
	  write(return_string,'(f12.1,'','',f12.1)') bmin,bmax
	  return_string = rmblank(return_string,nchars)
	  call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	  call extract_reals(2,return_string,bmin,bmax,f3,f4,f5,f6)
	 end if
	end if
	if(outswitch) write(idevout,'(2g12.4)') bmin,bmax
	if(bmin .ne. 0. .or. bmax .ne. 0.) then
	  amin = bmin	 
	  amax = bmax
	end if
	if(.not.image .and. amax*amax - amin*amin .eq. 0.) then
	  nlabels = nlabels + 1
	  iolabel(nlabels) = 'Increase amax or decrease amin'
	  go to 2700
	end if
C***********************************************************************
C*** call routines for reading image/transform and displaying to screen
C***********************************************************************
	call ximagelabeldisplay(iolabel,nlabels)
        if(image) then
         call imread
	 if(ierr .ne. 0) go to 2000
C*** Movie request for new map
	 if(nmaps .lt. 0) then
	  call ximagemenuhide
	  call ximageclearimage
	  nmaps = 0
	  big = .false.
          newmap = .true.
	  iyheight = 0
	  go to 1000
	 end if
C*** set pointer tracking shifts zero for auto positioned multisection images
	 if(multisection) then
	  ixshift = 0
	  iyshift = 0
	  icomp = 1
	 else
	  ixshift = nxstart - ixmin * icompress
	  iyshift = nystart - iymin * icompress
	  icomp = icompress
	 end if
        else
         call fftread
	 if(ierr .ne. 0) go to 1000
	 ixshift = -icenx
	 iyshift = -iceny
	 icomp = icompress
        end if
	if(pointer) then
	 call ximagepointertrackoff
	 call ximagepointertrackon(ixshift,iyshift,icomp)
	end if
	call ximageresetcursor(icurs)
	if(outswitch) then
	 close(idevout)
	 call system
     *   ('mv '//outputfile(1:lnblank(outputfile))//' '//
     *           defaultfile(1:lnblank(defaultfile)))
	end if
	if(idevin .gt. 0) close(idevin)
	idevin = 0
        newmap =.false.
C*******************************************************************
C***
C*** main menu
C***
C*******************************************************************
 4000	nlabels = 1
	call ximagelabeldisplay(iolabel,nlabels)
 4100	menulist(1) = 'Re-scale image'
	menulist(2) = 'Erase vectors'
	menulist(3) = 'Hide zoom window'
	menulist(4) = 'Modify zoom window'
	menulist(5) = 'Hide menu/labels'
	menulist(6) = 'Draw text string'
	menulist(7) = 'Change colour table'
	menulist(8) = 'Change cursor type'
	if(pointer) then
	 menulist(9) = 'Disable pointer tracking'
	else
	 menulist(9) = 'Enable pointer tracking'
	end if
	menulist(10) = 'Draw another map'
	menulist(11) = 'Add/number/edit sections'
	menulist(12) = 'Average boxed densities'
	menulist(13) = 'Box/Dump an area'
	menulist(14) = 'Output/measure coordinates'
	menulist(15) = 'Compute interactive FFT'
	menulist(16) = 'Lattice refinement'
	menulist(17) = 'Splinefit image'
	menulist(18) = 'Quit'
	call ximagemenuinit(menulist,18)
	job = -1
 5000   call ximagewait(job)
	if(job.le.0) then
	 go to 5000
C*** rescale image
	else if(job.eq.1) then
	 call ximagemenuhide
	 if(redisplay) then
	  iolabel(nlabels+1) = ' option not possible...'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	  go to 4100
	 end if
	 noz = 0
	 call ximageclearimage
	 newmap = .true.
	 big = .false.
	 iyheight = 0
	 go to 2700
C*** erase vectors
	else if(job.eq.2) then
	 call ximageremovevectors
	 go to 4000
C*** hide zoom window
	else if(job.eq.3) then
	 call ximagezoomhide
	 go to 4000
C*** modify zoom window
	else if(job.eq.4) then
	 call modify_zoom
	 go to 4000
C*** hide menu/labels/slider
	else if(job.eq.5) then
	 call hide_widgets
	 go to 5000
C*** draw text
	else if(job.eq.6) then
	 call draw_string
	 go to 4000
C*** change colour table
	else if(job.eq.7) then
	 rescale = .false.
	 newmap = .true.
	 call colour_table
	 if(rescale) then
	  call ximageclearimage
	  if(image) then
C*** if sections, reset iyheight
	   if(multisection) then
	    iyheight = iyheight - numsecy * iycr
	    noz = noz - (nzend - nzstart + 1)
	   end if
	   call imread
	  else
	   call fftread
	  end if
	  first = .true.
	  call colour_table
	 end if
	 go to 4000
C*** change cursor type
	else if(job.eq.8) then
	 call ximagemenuhide
	 menulist(1) = 'Default cursor'
	 menulist(2) = 'Fine crosshair'
	 menulist(3) = 'Box crosshair'
	 menulist(4) = 'Giant crosshair'
	 menulist(5) = 'Ring crosshair'
	 menulist(6) = 'Cross'
	 menulist(7) = 'Circle'
	 menulist(8) = 'Hand'
	 menulist(9) = 'Bird'
	 menulist(10) = 'Skull'
	 call ximagemenuinit(menulist,10)
	 job = -1
 5100    call ximagewait(job)
	 if(job .le. 0) go to 5100
	 icurs = job - 1
	 call ximagemenuhide
	 call ximagechangecursor(icurs)
	 go to 4000
C*** cursor tracking switch
	else if(job .eq. 9) then
	 if(phasemap) then
	  iolabel(nlabels+1) = 
     *    'Option not available for phase colour table...'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	  go to 4100
	 end if
	 if(pointer) then
	  call ximagepointertrackoff
	  pointer = .false.
	 else
	  call ximagepointertrackon(ixshift,iyshift,icomp)
	  pointer = .true.
	 end if
	 go to 4000
C*** read another map
	else if(job .eq. 10) then
	 if(phasemap) then
	  call colour_blackwhite
	  phasemap = .false.
	 end if
	 call ximagemenuhide
	 menulist(1) = 'Clear screen'
	 menulist(2) = 'Add new map'
	 menulist(3) = 'Return main menu'
	 call ximagemenuinit(menulist,3)
	 job = -1
 5200    call ximagewait(job)
	 if(job.le.0) then
	  go to 5200
C*** clear screen
	 else if(job .eq. 1) then
	  if(pointer) then
	   call ximagepointertrackoff
	   pointer = .false.
	  end if
	  if(mapread) call imclose(idevmap)
	  call ximagemenuhide
	  call removevectors
	  call ximageclearimage
C*** set auto y position for subsequent multisection map position
	  iyheight = 0
          newmap = .true.
	  big = .false.
	  montage = .false.
C*** add new map
	 else if(job .eq. 2) then
	  call ximagemenuhide
          if(max_display_width .gt. max_screen_width .or.
     *       max_display_height .gt. max_screen_height) then
           iolabel(nlabels+1) = 
     *     'Map already offscreen, cannot add another'
	   call ximagelabeldisplay(iolabel,nlabels+1)
           go to 4100
          end if
	  if(mapread) call imclose(idevmap)
	  iolabel(nlabels+1) = 'Wait for map re-positioning...'
	  call ximagelabeldisplay(iolabel,nlabels+1)
C*** re-position pixmap
	  call ximagedrawimage
     *    (max_display_width,max_display_height,imap,mapbuf,ierr)
	  call ximagelabelhide
	  if(ierr .eq. 1) then
	   iolabel(nlabels+1) = 'Warning : X event error'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	  else if(ierr .eq. 2) then
	   iolabel(nlabels+1) = 'Warning : error reading back pixmap'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	  else if(ierr .eq. 3) then
	   iolabel(nlabels+1) = 'Warning : map all zeroes'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	  end if
	  call ximagelabelhide
	  newmap = .false.
	  montage = .true.
C*** return main menu
	 else if(job.eq.3) then
	  call ximagemenuhide
	  go to 4000
	 end if
	 call ximagemenuhide
	 go to 1000
C*** display/number/delete sections
	else if(job.eq.11) then
	 call ximagemenuhide
	 menulist(1) = 'Add sections to display'
	 menulist(2) = 'Number sections'
	 menulist(3) = 'Edit sections'
	 menulist(4) = 'Return main menu'
	 call ximagemenuinit(menulist,4)
	 job = -1
 5300    call ximagewait(job)
	 if(job.le.0) then
	  go to 5300
C*** display more sections
	 else if(job .eq. 1) then
	  call ximagemenuhide
	  if(image) go to 2000
	  go to 8000
C*** number sections
	 else if(job .eq. 2) then
	  call sectionnumber
C*** delete sections
	 else if(job .eq. 3) then
	  call sectionedit
	 end if
	 call ximagemenuhide
	 go to 4000
C*** average boxed densities
	else if(job.eq.12) then
	 if(redisplay .or. icompress .ne. 1 .or. multisection) 
     *   go to 8000
	 call ximagemenuhide
	 call average	 
	 go to 4000
C*** box/dump an area
	else if(job.eq.13) then
	 call box_area
	 go to 4100
C*** display coords/density
	else if(job.eq.14) then
	 if(redisplay) go to 8000
	 outputcoords = .true.
	 call output_coords
	 outputcoords = .false.
	 go to 4000
C*** compute FFT
	else if(job.eq.15) then
	 if(nmaps .eq. 1 .and. image) call fftdisplay
	 go to 4000
C*** lattice refinement
	else if(job.eq.16) then
	 call lattice
	 go to 4000
C*** splinefit
	else if(job.eq.17) then
	 if(image) then
	  call splinefit
	 else
	  go to 8000
	 end if
	 go to 4000
C*** quit
	else if(job.eq.18) then
	 call ximagemenuhide
	 call ximagelabelhide
	 go to 9000
	end if
	go to 4000
C*** diagnostic
 8000   call ximagemenuhide
	call ximagelabelhide
	iolabel(nlabels+1) = 'Option not available'
	call ximagelabeldisplay(iolabel,nlabels+1)
	go to 4100
C*****************************************************************
C*** end of menu items
C*****************************************************************
9000	if(mapread) call  imclose(idevmap)
	stop
        end
C*****************************************************************
C***
C*** end of main program
C***
C*****************************************************************
C***
        subroutine imread
C***
C*******************************************************************
C*** subroutine to read image format file & display on screen
C***
	include		 'Ximdisp_common.for'
C***
C*** loop over sections : check for auto positioning of sections
  40  	scl = grey/(amax - amin)
	nmaps = 1
	ierr = 0
	noz = noz + nzend - nzstart + 1
        auto = .false.
	multisection = .false.
	mwidth = max_screen_width
	mheight = max_screen_height
C*******************************************************************
C*** test for multisection image
C*******************************************************************
  50 	if(nzstart .lt. nzend) then
	 multisection = .true.
C*** interactive mode
	 if(idevin .eq. 0) then
	  call ximagelabeldisplay(iolabel,nlabels)
	  nlabels = 1
	  menulist(1) = 'Auto display of sections'
	  menulist(2) = 'Specify number of sections per row'
	  menulist(3) = 'Semi-auto display of sections'
	  menulist(4) = 'Manual display of sections'
	  menulist(5) = 'Scrolling display of sections'
	  menulist(6) = 'Movie display'
	  menulist(7) = 'Return main menu'
	  call ximagemenuinit(menulist,7)
	  job = -1
  100     call ximagewait(job)
	  if(job .le. 0) then
	   go to 100
C*** Auto display
	  else if(job .le. 2) then
	   call ximagemenuhide
C*** set increment size in x and y
	   ixcr = nox + iautoinc
	   iycr = noy + iautoinc
C*** specify number of sections per row
	   if(job .eq. 2) then
	    iolabel(nlabels+1) = 
     *      'Specify number of sections per row'
	    return_string = ' '
 	    call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	    call extract_integers(1,return_string,numsecx,i2,i3,i4,i5,i6)
	   else
	    numsecx = max_screen_width / ixcr
	    numsecy = noz / numsecx + min(1,mod(noz,numsecx))
C*** check to see if montage too big for screen, 
C*** if so reset number of sections to a multiple of 10
	    if(numsecx * (nox + iautoinc) .gt. max_screen_width .or.
     *         numsecy * (noy + iautoinc) .gt. max_screen_height)
     *         numsecx = max(10,10 * int(sqrt(float(noz))/10.0))
	   end if
  150	   numsecy = noz / numsecx + min(1,mod(noz,numsecx))
C*** reset display size if appropriate
	   ixinc = iautoinc
	   iyinc = iautoinc
C*** new map
	   if(newmap) then
	    max_display_width = 
     *      max(max_screen_width,numsecx * (nox + ixinc))
	    max_display_height = 
     *      max(max_screen_height,iyheight + numsecy * (noy + iyinc))
	   else
	    mwidth = numsecx * (nox + ixinc)
	    mheight = iyheight + numsecy * (noy + iyinc)
	    if(mwidth .gt. max_screen_width .or.
     *         mheight .gt. max_screen_height) then
	     nlabels = nlabels + 1
             iolabel(nlabels) = 'Map too large to add to screen'
	     ierr = 1
	     return
	    end if
	    max_display_width = max_screen_width
	    max_display_height = max_screen_height
	   end if
C*** set centre position of first section 
	   icx = nox / 2 + iautoinc
	   icy = max_display_height - iyheight - (noy / 2 + iautoinc)
	   inum = 0
C*** set centre position, this number is incremented with each section
C*** along a row
           icenx = icx
           iceny = icy
	   auto = .true.
C*** semi-auto display of sections
	  else if(job .eq. 3) then
	   call ximagemenuhide
	   auto = .true.
	   write(iolabel(nlabels+1),
     *     '('' number of points (x,y,z)='',3i5,
     *       '' max screen width, height ='',2i5)') 
     *      nxyz, max_screen_width, max_screen_height
           iolabel(nlabels+2) = 
     *     ' type compressed x and y coordinates for section 1 centre'
	   iolabel(nlabels+3) =
     *     'Note : origin topleft of screen'
	   iolabel(nlabels+4) =
     *     ' compressed increments in x and y, number of '//
     *     'sections per line'
  200	   return_string = ' '
	   call ximageioboxdisplay(iolabel,return_string,nlabels+4)
	   call extract_integers(5,return_string,
     *     icx,icy,ixcr,iycr,numsecx,i6)
	   if(numsecx .le. 0) go to 200
	   numsecy = noz / numsecx + min(1,mod(noz,numsecx))
	   ixinc = ixcr - nox
	   iyinc = iycr - noy
	   if(newmap) then
	    max_display_width = 
     *      max(max_screen_width,numsecx * (nox + ixinc))
	    max_display_height = 
     *      max(max_screen_height,iyheight + numsecy * (noy + iyinc))
	   else
	    mwidth = numsecx * (nox + ixinc)
	    mheight = iyheight + numsecy * (noy + iyinc)
	    if(mwidth .gt. max_screen_width .or.
     *         mheight .gt. max_screen_height) then
	     nlabels = nlabels + 1
             iolabel(nlabels) = 'Map too large to add to screen'
	     ierr = 1
	     return
	    end if
	    max_display_width = max_screen_width
	    max_display_height = max_screen_height
	   end if
           inum = 0
           icenx = icx
           iceny = max_display_height - icy
C*** set iyheight to icy - half size
	   iyheight = icy - noy / 2
C*** manual display
	  else if(job .eq. 4) then
	   call ximagemenuhide
C*** scrolling or movie display
	  else if(job .eq. 5 .or. job .eq. 6) then
C*** clear if not new map
	   if(.not. newmap) then
	    call ximageclearimage
	    iyheight = 0
	    nmaps = 0
	    newmap = .true.
	    big = .false.
	   end if
	   call ximagemenuhide
	   nmaps = noz
	   if(job .eq. 6) nmaps = -nmaps
C*** separate check for resources
           call checkimage(nox,noy,noz,ierr)
	   if(ierr .eq. 1) then
            iolabel(nlabels+1) = 
     *      'Insufficient memory for this request.'
	    iolabel(nlabels+2) = 
     *      ' Re-enter section range nzstart, nzend : '
	    return_string = ' '
 	    call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	    call extract_integers(2,return_string,
     *      nzstart, nzend, i3, i4, i5, i6)
	    go to 40
	   end if
C*** added 09.09.03
	   max_display_width = max_screen_width
	   max_display_height = max_screen_height
	   go to 250
C*** return main menu
	  else if(job .eq. 7) then
	   call ximagemenuhide
	   return
	  end if
C*** display multiple sections in non-interactive mode - use auto option
	 else
	  auto = .true.
	  ixinc = iautoinc
	  iyinc = iautoinc
	  ixcr = nox + iautoinc
	  iycr = noy + iautoinc
	  numsecx = max_screen_width / ixcr
	  numsecy = noz / numsecx + min(1,mod(noz,numsecx))
          if(newmap) then
	   max_display_width = 
     *     max(max_screen_width,numsecx * (nox + ixinc))
	   max_display_height = 
     *     max(max_screen_height,iyheight + numsecy * (noy + iyinc))
	  else
	   mwidth = numsecx * (nox + ixinc)
	   mheight = iyheight + numsecy * (noy + iyinc)
	   if(mwidth .gt. max_screen_width .or.
     *        mheight .gt. max_screen_height) then
	    nlabels = nlabels + 1
            iolabel(nlabels) = 'Map too large to add to screen'
	    ierr = 1
	    return
	   end if
	   max_display_width = max_screen_width
	   max_display_height = max_screen_height
	  end if
	  icx = nox / 2 + iautoinc
	  icy = max_display_height - iyheight - (noy / 2 + iautoinc)
	  inum = 0
          icenx = icx
          iceny = icy
	 end if
C*************************************************************
C*** test number of sections does not exceed resources
C*************************************************************
	 if(max_display_width .gt. max_screen_width .or.
     *      max_display_height .gt. max_screen_height) then
	  big = .true.
	  call ximagecheckimage
     *    (max_display_width,max_display_height,1,ierr)
	  if(ierr .eq. 1) then
           iolabel(nlabels+1) = 
     *     'Insufficient memory for this request.'
	   iolabel(nlabels+2) = 
     *     ' Re-enter section range nzstart, nzend : '
	   return_string = ' '
 	   call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	   call extract_integers(2,return_string,
     *     nzstart, nzend, i3, i4, i5, i6)
	   go to 40
	  end if
	 end if
C*************************************************************
C*** single section map
C*************************************************************
	else
         if(newmap) then
	  max_display_width = max(nox,max_screen_width)
	  max_display_height = max(noy,max_screen_height)
C*** problem if adding a very large map to an existing smaller one
	 else
	  if(nox .gt. max_screen_width .or. 
     *       noy .gt. max_screen_height) then
	   nlabels = nlabels + 1
	   iolabel(nlabels) = 
     *     'Map too large for adding, please re-specify'
	   ierr = 1
	   return
	  end if
	  max_display_width = max_screen_width
	  max_display_height = max_screen_height
	 end if
	 mwidth = max(max_display_width,max_screen_width)
	 mheight = max(max_display_height,max_screen_height)
	 if(idevin .eq. 0) then
	  write(iolabel(nlabels+1), '('' section '',i4,
     *      '' enter image centre in screen coords:'')') iz
	  write(iolabel(nlabels+2),
     *    '('' ("/" or "<cr>" = auto center) '')')
	  write(iolabel(nlabels+3),
     *    '('' ("0,0" = cursor positioning)'')')
	  return_string = ' '
  	  call ximageioboxdisplay(iolabel,return_string,nlabels+3)
	  nlabels = 1
	 else
	  read(idevin,'(a)') return_string
	 end if
	 if(outswitch) write(idevout,'(a)') return_string
C*** auto centre
	 if(return_string .eq. '/' .or. return_string .eq. ' ') then
          icenx = max_display_width / 2
          iceny = max_display_height / 2
	 else
	  call extract_integers(2,return_string,
     *    icenx,iceny,i3,i4,i5,i6)
C*** cursor positioned centre
	  if(icenx .eq. 0 .and. iceny .eq. 0) then
	   iolabel(nlabels+1) = 
     *     'Mark cursor position bottom left corner'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	   call ximagereadpointer(kx,ky)
	   call ximagelabelhide
	   icenx = kx + (nox/2)
	   iceny = ky + (noy/2)
	  end if
	 end if        
	end if
	iyheight = iyheight + numsecy * iycr
C*** passed all checks
  250	imap = 0
	compsq = float(icompress * icompress)
	do 1000 iz = nzstart,nzend
C***
C*** auto positioning
  	 if(auto) then
          inum = inum + 1
          if(inum.gt.numsecx) then
           inum = 1
           icenx = icx
           iceny = iceny - iycr
          else if(inum.gt.1) then
           icenx = icenx + ixcr
          end if
C*** set y value in case new maps to be added
	  ixmin = icenx - nox / 2
          ixmax = ixmin + nox - 1
	  iymin = iceny - noy / 2
          iymax = iymin + noy - 1
          go to 600
         end if
C***
	 if(abs(nmaps) .gt. 1) then
	  if(iz.eq.nzstart) then
           icenx = max_screen_width / 2
           iceny = max_screen_height / 2
	  end if
	 end if
C*****************************************************************
C*** check for out of bounds
C*****************************************************************
  400    ixmin = icenx - nox / 2
C*** horizontal min -ve
         if(ixmin .lt. 0) then
	  nlabels = nlabels + 1
	  write(iolabel(nlabels),
     *    '('' *** Error - image out of bounds, '',
     *      ''horizontal minimum negative '',i6)') ixmin
	  call ximagelabeldisplay(iolabel,nlabels)
	  go to 50
	 end if
         ixmax = ixmin + nox - 1
C*** horizontal max too large
	 if(ixmax .gt. max_display_width) then
	  nlabels = nlabels + 1
	  write(iolabel(nlabels),
     *    '('' *** Error - image out of bounds, horizontal max '',i5,
     *      '' > screen width '',i4)') 
     *    ixmax, max_display_width
	  call ximagelabeldisplay(iolabel,nlabels)
	  go to 50
	 end if
         iymin = iceny - noy / 2
C*** vertical min -ve
         if(iymin .lt. 0) then
	  nlabels = nlabels + 1
	  write(iolabel(nlabels),
     *    '('' *** Error - image out of bounds, '',
     *      ''vertical minimum negative '',i8)') iymin
	  call ximagelabeldisplay(iolabel,nlabels)
	  go to 50
	 end if
         iymax = iymin + noy - 1
C*** vertical max too large
	 if(iymax .gt. max_display_height) then
	  nlabels = nlabels + 1
	  write(iolabel(nlabels),
     *    '('' *** Error - image out of bounds, vertical max '',i5,
     *    '' > screen height '',i4)') 
     *    iymax, max_display_height
	  call ximagelabeldisplay(iolabel,nlabels)
          go to 50
	 end if
C*******************************************************************
C*** check to see if single section map too large
C*******************************************************************
	 if(nzend - nzstart .eq. 0) then
	  if(newmap) then
	   if(max_display_height .gt. max_screen_height .or.
     *       max_display_width .gt. max_screen_width) then
	    big = .true.
	    ierr = 0
	    call ximagecheckimage
     *      (max_display_width,max_display_height,1,ierr)
	    if(ierr .ne. 0) then
	     iolabel(nlabels+1) = 
     *       'Insufficient resources for map, press <cr> to continue'
	     return_string = ' '
 	     call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	     ierr = 1
	     return
	    end if
	   end if
	  end if
	 end if
C**************************************************************
C*** put out message to give mouse information
C**************************************************************
  600    if(iz .eq. nzstart) then
	  iolabel(nlabels+1) = 
     *    'Wait for map display ...'
	  iolabel(nlabels+2) =
     *    'To mark points, press left hand mouse button'
	  iolabel(nlabels+3) =
     *    'To pan, hold down centre mouse button'
	  iolabel(nlabels+4) =
     *    'To zoom, press right hand mouse button'
	  call ximagelabeldisplay(iolabel,nlabels+4)
	 end if
C*******************************************************************
C*** initialize output buffer
C*******************************************************************
         if(newmap) then
          do i=0,max_display_width * max_display_height / 8 - 1
            mapreal8(i) = 0
	  end do
         end if
C***
C*** set up parameters for x term screen, origin (0,0) top left.
C*** load single map into buffer
         call imposn(idevmap,iz,nystart)
	 if(nmaps .eq. 1) then
	  iystart = max_display_height - iymin
          do iy=1,noy * icompress,icompress
           ny = max_display_width * (iystart - (iy-1) / icompress)
C*** initialize real array
	   do ix = 1,nox
	    density(ix) = 0.
	   end do
	   do ky=1,icompress
            call irdlin(idevmap,aline)
            do ix = 1,nox * icompress,icompress
	     jx = (ix - 1) / icompress + 1
	     do kx = 1,icompress
	      lx = nxstart + ix + kx - 1
	      density(jx) = density(jx) + aline(lx)
	     end do
	    end do
	   end do
	   do ix = 1,nox
            nx = ixmin + ix - 1
	    nxy = ny + nx
	    den = (density(ix) / compsq - amin) * scl
	    den = min(grey, max(0.,den))
            mapbuf(nxy) = nint(den)
	   end do
	  end do
          newmap = .false.
C*** set y position of single map for next time
	 if(.not. multisection)	iyheight = iystart + iautoinc
	 else
C********
C*** load map for scrolling sections and write to pixmap
C********
	  iolabel(nlabels+1) =
     *    'Loading sections, please wait ...'
	  call ximagelabeldisplay(iolabel,nlabels+1)
C*** calculate uncompressed input numbers in x and y 
	  noxin = nox * icompress
	  noyin = noy * icompress
          do iy=1,noyin,icompress
C*** initialize real array
	   do ix = 1,nox
	    density(ix) = 0.
	   end do
	   do ky=1,icompress
            call  irdlin(idevmap,aline)
            do ix = 1,noxin,icompress
	     nxcomp = (ix - 1) / icompress + 1
	     ixoffset = nxstart + ix - 1
	     do kx = 1,icompress
	      density(nxcomp) = density(nxcomp) + aline(ixoffset + kx)
	     end do
	    end do
	   end do
	   nxyout = nox * (noy - iy / icompress)
	   do ix = 1,nox
	    den = (density(ix) / compsq - amin) * scl
	    den = min(grey, max(0.,den))
            mapbuf(nxyout + ix) = nint(den)
	   end do
	  end do
	  imap = iz - nzstart
	  call ximagedrawimage(nox,noy,imap,mapbuf,ierr)
	  call ximagelabelhide
	  if(ierr .eq. 1) then
	   iolabel(nlabels+1) = 'Warning : X event error'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	  else if(ierr .eq. 2) then
	   iolabel(nlabels+1) = 'Warning : error reading back pixmap'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	  else if(ierr .eq. 3) then
	   iolabel(nlabels+1) = 'Warning : map all zeroes'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	  end if
	 end if
 1000   continue
C******************************************************************
C*** if single pixmap, display and return
C******************************************************************
	if(nmaps .eq. 1) then
	 call ximagedrawimage
     *   (max_display_width,max_display_height,imap,mapbuf,ierr)
	 call ximagelabelhide
	 if(ierr .eq. 1) then
	  iolabel(nlabels+1) = 'Warning : X event error'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	 else if(ierr .eq. 2) then
	  iolabel(nlabels+1) = 'Warning : error reading back pixmap'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	 else if(ierr .eq. 3) then
	  iolabel(nlabels+1) = 'Warning : map all zeroes'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	 end if
	 return
	end if
C******************************************************************
C*** scrolling sections...
C******************************************************************
	if(nmaps .gt. 1) then
	 call ximagelabelhide
 1050	 iolabel(nlabels+1) = 'Pan/zoom disabled for scrolling'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 menulist(1) = 'Scroll through sections'
	 menulist(2) = 'Change colour table'
	 menulist(3) = 'Read another map'
	 menulist(4) = 'Quit Ximdisp'
	 call ximagemenuinit(menulist,4)
	 job = -1
 1100    call ximagewait(job)
	 if(job.le.0) then
	  go to 1100
C*** scroll
	 else if(job.eq.1) then
	  call ximagemenuhide
	  nmap = -1
C*** set up new wait loop to service slider
	  call ximagesliderinit(1,0.,0.)
	  iopt = -1
 1200     call ximagewait(iopt)
	  if(iopt.lt.0) then
	   go to 1200
	  else if(iopt.eq.0) then
	   call ximagesliderhide
	   call ximagemenudisplay
	   go to 1100
C*** read slider position
	  else if(iopt.eq.101) then
	   call ximagesliderread(percent)
	   imap = nint(float(nmaps) * percent * 0.01)
	   imap = max(min(imap,nmaps-1),0)
	   if(nmap .ne. imap) call ximagedrawimages(imap)
	   nmap = imap
	   go to 1200
C*** zoom ?
	  else
	   go to 1200
	  end if
C*** change colour table
	 else if(job.eq.2) then
	  call ximagemenuhide
	  call ximagesliderhide
	  call colour_table
	  go to 1050
C*** read another map
	 else if(job .eq. 3) then
	  call ximagemenuhide
	  call ximagelabelhide
	  call ximagesliderhide
	  nmaps = -nmaps
	  return
C*** quit Ximdisp
	 else if(job .eq. 4) then
	  call ximagemenuhide
	  call ximagelabelhide
	  call ximagesliderhide
	  stop
C*** maybe slider callback in wrong place
	 else
	  go to 1100
	 end if
	 write(6,'('' error - job = '',i)') job
         return
C*************************************************************
C*** Movie display of sections
C*************************************************************
	else
	 call ximagelabelhide
	 percent = 50.0
 2000    idelay = nint(percent * 10000)
	 sliderpos = percent * 0.01
	 iolabel(nlabels+1) = 
     *   'Use slider bar to set delay : range 0 - 1 seconds'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagesliderinit(1,sliderpos,0.)
 2100    imap = 0
	 iadd = 1
C*** establish continuous event processing to rotate the map
         call ximagemenuevent
	 menulist(1) = 'Change colour table'
	 menulist(2) = 'Hide menu/labels/slider'
	 menulist(3) = 'Read another map'
	 menulist(4) = 'Quit Ximdisp'
	 call ximagemenuinit(menulist,4)
	 job = -1
 2300    call ximagewait(job)
C*** rotate by 1 more section
	 if(job .le. 0) then
	  call ximagedelay(idelay)
	  call ximagedrawimages(imap)
C*** reverse sign if at end
	  imap = imap + iadd
	  if(imap .eq. abs(nmaps)-1) iadd = -1
	  if(imap .eq. 0) iadd = 1
	  go to 2300
C*** change colour table
	 else if(job .eq. 1) then
	  call ximagelabelhide
	  call ximagesliderhide
	  call ximagemenuhide
	  call colour_table
	  go to 2000
C*** hide menu/labels/slider
	 else if(job .eq. 2) then
	  call hide_widgets
	  go to 2300
C*** read another map
	 else if(job .eq. 3) then
	  call ximagemenuhide
	  call ximagelabelhide
	  call ximagesliderhide
	  return
C*** Quit
	 else if(job .eq. 4) then
	  call ximagemenuhide
	  call ximagelabelhide
	  call ximagesliderhide
	  stop
C*** read slider position
	 else if(job .eq. 101) then
	  call ximagesliderread(percent)
	  idelay = nint(percent * 10000)
	  go to 2100
	 end if
	end if
        end
C**********************************************************************
C***
        subroutine fftread
C***
C**********************************************************************
C*** subroutine to read transform & display on screen
	include		'Ximdisp_common.for'
C***
	mheight = max(max_screen_height,max_display_height)
	mwidth = max(max_screen_width,max_display_width)
	ierr = 0
        nox2 = nox * 2
C*** check resources for this size of transform
	if(.not. newmap) then
	 if(nox2 .gt. max_screen_width .or. 
     *      noy .gt. max_screen_height) then
	  nlabels = nlabels + 1
	  iolabel(nlabels) = 
     *    'Map too large for adding, please re-specify'
	  ierr = 1
	  return
	 end if
C*** new map, check resources
	else
	 max_display_width = max(max_screen_width,nox2)
	 max_display_height = max(max_screen_height,noy)
	 if(nox2 .gt. max_screen_width) big = .true.
	 call ximagecheckimage
     *   (max_display_width,max_display_height,1,ierr)
	 if(ierr .ne. 0) then
	  iolabel(nlabels+1) = 
     *    'Insufficient resources for this map, please re-specify'
	  ierr = 1
	  return
	 end if
	end if
C*** add 1 to this as noy is now the actual number to be read
C*** i.e. 1st line discarded so in a 512*512 noy is 511
        nyhalf = (noy + 1) / 2
	iphasex = max_screen_width / 2
	iphasey = max_screen_height / 2
C*** nystart set to 1 not 0 as first line of a transform is rubbish
	nystart = max(1,nystart)
	left = 1
C*** set up delp for x and y for phase shifts
	call irtorg(idevmap,xorigin,yorigin,zorigin)
	delpx = -2.0 * pi * xorigin / (2.0 * (nxyz(1) - 1))
	delpy = -2.0 * pi * yorigin / nxyz(2)
C***
C*** amplitudes or intensities ?
	amp = .true.
	if(idevin .eq. 0) then
	 iolabel(nlabels+1) = ' Enter display choice : '
	 iolabel(nlabels+2) = ' ("<cr>" = amplitudes)'
	 iolabel(nlabels+3) = ' (" 1  " = intensities)'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+3)
	 if(return_string(1:1) .ne. ' ') amp = .false.
	else
	 read(idevin,'(a)') return_string
	 if(return_string .ne. ' ') amp = .false.
	end if
	if(outswitch) write(idevout,'(a)') return_string
C***
C*** calculate size of transform on screen
 100	icenx=0
	iceny=0
	if(idevin .eq. 0) then
	 iolabel(nlabels+1) = ' enter centre of transform : '
	 iolabel(nlabels+2) = ' ("/" or "<cr>" = auto centre)'
	 iolabel(nlabels+3) = ' ("0,0" = cursor positioning)'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+3)
	else
	 read(idevin,'(a)') return_string
	end if
	if(outswitch) write(idevout,'(a)') return_string
	nlabels = 1
C*** calculate centre of transform
	if(return_string .eq. '/' .or. return_string .eq. ' ') then
         icenx = max_display_width / 2
         iceny = max_display_height / 2
	else
	 call extract_integers(2,return_string,
     *   icenx,iceny,i3,i4,i5,i6)
	 if(return_string .eq. ' ' .or. return_string .eq. '/') then
          icenx = max_display_width / 2
          iceny = max_display_height / 2
C*** cursor positioning
         else if(icenx.eq.0) then
	  if(idevin .eq. 0) then
	   iolabel(nlabels+1) = 
     *     'Mark bottom left corner with left hand mouse button'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	   call ximagereadpointer(kx,ky)
	   call ximagelabelhide
C*** use zero default for file input
	  else
	   kx = 0
	   ky = 0
	  end if
          icenx = kx + nox - 1
          iceny = ky + nyhalf - 1
C*** error
	 else
	  go to 100
	 end if
        end if
C***
C*** calculate scale factors
 	aminsq = amin * amin
	scl = grey / (amax - amin)
	sclsq = grey / (amax * amax - aminsq)
	ampscl = 15. / (amax - amin + 1.0)
C***
C*** check for out of bounds
        ixmin = icenx - nox + 1
        if(ixmin .lt. 0) go to 300
        ixmax = icenx + nox - 1
	if(ixmax .gt. max_display_width) go to 300
	iymin = iceny - nyhalf + 1
        if(iymin .lt. 0) go to 300
        iymax = iceny + nyhalf - 1
	if(iymax .gt. max_display_height) go to 300
        go to 400
C***
 300	if(idevin .eq. 0) then
	 iolabel(nlabels+1) =
     *   ' values for the center take image out of bounds'
	 nlabels = nlabels + 1
	 go to 100
	else
	 write(6,'(''Values for centre take image out of bounds'')')
	 stop
	end if
C***
C*** put out message to give mouse information
  400	iolabel(nlabels+1) = 
     *  'wait for map display ...'
	iolabel(nlabels+2) =
     *  'to mark points, press left hand mouse button'
	iolabel(nlabels+3) =
     *  'to pan, hold down centre mouse button'
	iolabel(nlabels+4) =
     *  'to zoom, press right hand mouse button'
	call ximagelabeldisplay(iolabel,nlabels+4)
C***
C*** initialize output buffers
	if(newmap) then
         do i=0,max_display_width * max_display_height / 8 - 1
          mapreal8(i) = 0
	 end do
        end if
	do i=0,max_phasebuf-1
	 phasebuf(i) = 0
	end do
C***
C*** set up parameters for x term, origin top left, so mapbuf will be
C*** turned upside-down relative to the screen. This means that the
C*** first line read in from the transform will be displayed at the
C*** bottom right/top left of the screen.
C*** transforms store only the rh half of the image which has an even number
C*** of lines. the origin is at coords 0,ny/2 (see image.doc).
        iystart1 = max_display_height - iymin
        iystart2 = max_display_height - iymax
	jystart1 = iphasey + nyhalf - 1
	jystart2 = iphasey - nyhalf + 1
        call imposn(idevmap,0,nystart)
C***
C*** display amplitudes
        if(amp) then
          do 500  iy=1,noy
           call  irdlin(idevmap,aline)
           ny1 = iystart1 - iy + 1
           ny2 = iystart2 + iy - 1
	   iytrans = nyhalf - iy
	   iytrans = nyhalf - iy
	   jy1 = jystart1 - iy + 1
	   jy2 = jystart2 + iy - 1
           do 500 ix = 1,nox2,2
CHEN---------------------nox2 = 2 * nox, because of REAL and IMAG in the FFT
            aval = aline(nxstart + ix)
            bval = aline(nxstart + ix + 1)
            ixtrans = (ix + 1) / 2
            nx1 = icenx + ixtrans - 1
            nx2 = icenx - ixtrans + 1
	    nxy1 = ny1 * max_display_width + nx1
	    nxy2 = ny2 * max_display_width + nx2
	    den = scl*(sqrt(aval*aval+bval*bval)-amin)
	    den = min(grey,max(0.,den))
            mapbuf(nxy1) = nint(den)
            mapbuf(nxy2) = mapbuf(nxy1)
C*** load phase map if within 1026 * 1024
	    if(ixtrans .le. iphasex .and. 
     *                      abs(iytrans) .le. iphasey) then
C*** transform amps & phases :
C*** apart = aline(ix), bpart = aline(ix+1)
C*** delpx = -2*pi * (xorigin + xshift) / (2. * (nx-1))
C*** delpy = -2*pi * (yorigin + yshift) / ny
C*** pshift = ix * delpx + iy * delpy
C*** set left to -1 for bottom half of transform
	     ixstep = ixtrans - 1
	     iystep = -iytrans
	     left = isign(1,iystep)
C*** multiply by sign to make -ve if from bottom right(or top left) of transform
             pshift = delpx * left * ixstep + delpy * iystep
             call fftextract(aval,bval)
	     nphase = nint(phase / 45.)
	     if(nphase .gt. 7) nphase = 0
	     ampsub = min(max(amin,amplitude-amin),amax)
	     nden = nphase + 8 * nint(ampscl * ampsub)
	     jx1 = iphasex + ixtrans - 1
	     jx2 = iphasex - ixtrans + 1
	     jxy1 = jy1 * max_screen_width + jx1
	     jxy2 = jy2 * max_screen_width + jx2
             phasebuf(jxy1) = nden
             phasebuf(jxy2) = nden
	    end if
  500      continue
        else
C***
C*** display intensities
         do 600 iy=1,noy
           call  irdlin(idevmap,aline)
           ny1 = iystart1 - iy + 1
           ny2 = iystart2 + iy - 1
	   iytrans = nyhalf - iy
	   jy1 = jystart1 - iy + 1
	   jy2 = jystart2 + iy - 1
           do 600 ix=1,nox2,2
            aval = aline(nxstart + ix)
            bval = aline(nxstart + ix + 1)
            ixtrans = (ix + 1) / 2
            nx1 = icenx + ixtrans - 1
            nx2 = icenx - ixtrans + 1
	    nxy1 = ny1 * max_display_width + nx1
	    nxy2 = ny2 * max_display_width + nx2
	    den = sclsq*((aval*aval+bval*bval)-aminsq)
	    den = min(grey,max(0.,den))
            mapbuf(nxy1) = nint(den)
            mapbuf(nxy2) = mapbuf(nxy1)
C*** load phase map if within 1026 * 1024
	    if(ixtrans .le. iphasex .and. 
     *                      abs(iytrans) .lt. iphasey) then
	     ixstep = ixtrans - 1
	     iystep = -iytrans
	     left = isign(1,iystep)
C*** multiply by sign to make -ve if from bottom right(or top left) of transform
             pshift = delpx * left * ixstep + delpy * iystep
             call fftextract(aval,bval)
	     nphase = nint(phase / 45.)
	     if(nphase .gt. 7) nphase = 0
	     ampsub = min(max(amin,amplitude-amin),amax)
	     nden = nphase + 8 * nint(ampscl * ampsub)
	     jx1 = iphasex + ixtrans - 1
	     jx2 = iphasex - ixtrans + 1
	     jxy1 = jy1 * max_screen_width + jx1
	     jxy2 = jy2 * max_screen_width + jx2
             phasebuf(jxy1) = nden
             phasebuf(jxy2) = nden
	    end if
 600     continue
        end if
C***
C*** write map to screen
	if(phasemap) then
	 call colour_phase
	 call ximagedrawimage
     *   (max_screen_width,max_screen_height,0,phasebuf,ierr)
	else
	 call ximagedrawimage
     *   (max_display_width,max_display_height,0,mapbuf,ierr)
	end if
	call ximagelabelhide
	if(ierr .eq. 1) then
	 iolabel(nlabels+1) = 'Warning : X event error'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	else if(ierr .eq. 2) then
	 iolabel(nlabels+1) = 'Warning : error reading back pixmap'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	else if(ierr .eq. 3) then
	 iolabel(nlabels+1) = 'Warning : map all zeroes'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	end if
        return
        end
C*************************************************************************
C***
        subroutine average
C***
C*************************************************************************
C*** subroutine to draw boxes on screen, read density in boxes
C*** from the input map and average all the density
C*************************************************************************
        include    'Ximdisp_common.for'
C***
 	menulist(1) = 'Write output to screen'
	menulist(2) = 'Write output to screen and file'
	menulist(3) = 'Return main menu'
	call ximagemenuinit(menulist,3)
	job = -1
  100   call ximagewait(job)
	if(job.le.0) then
	 go to 100
C*** screen output
	else if(job.eq.1) then
	 call ximagemenuhide
         avout = .false.
C*** file output
	else if(job.eq.2) then
	 call ximagemenuhide
	 avout = .true.
	 iolabel(nlabels+1) = 'Output file name...'
	 avgfile = ' '
	 call ximageioboxdisplay(iolabel,avgfile,nlabels+1)
	 old = .false.
	 call check_file(avgfile)
	 open(unit=idevavg,file=avgfile,status='new')
	else if(job.eq.3) then
	 call ximagemenuhide
	 return
	end if
	nlabels = 2
C***
 150    nboxes = 0
	npixels = 0
	ameantot = 0.
	bmeantot = 0.
C*** mark corner positions for box
 200    iolabel(nlabels) = 
     *  'Draw boxes to indicate densities to be averaged'
 	call ximagelabeldisplay(iolabel,nlabels)
 300    menulist(1) = 'Draw box by rubberbanding'
	menulist(2) = 'Draw box by point specification'
	menulist(3) = 'Specify size and position cursor'
  	menulist(4) = 'Re-draw box'
	menulist(5) = 'Calculate total average for all boxes'
	menulist(6) = 'Return main menu'
	call ximagemenuinit(menulist,6)
	job = -1
  500   call ximagewait(job)
	if(job .le. 0) then
	 go to 500
C*******************************************************
C*** draw a box by rubberbanding
C*******************************************************
	else if(job .eq. 1) then
	 call ximagemenuhide
	 call ximagelabelhide
	 call ximagerubberenable(2)
C*** write previous mean to output file
	 if(nboxes .gt. 0) call average_write
	 iolabel(nlabels+1) = 
     *   'Ctrl/left button down to mark bottom left hand box corner'
	 iolabel(nlabels+2) = 
     *   'Hold mouse button down, drag mouse to top right of box'
         iolabel(nlabels+3) =
     *   'Release mouse button to mark final position'
	 call ximagelabeldisplay(iolabel,nlabels+3)
	 menulist(1) = 'Return previous menu'
	 menulist(2) = 'Return main menu'
	 call ximagemenuinit(menulist,2)
	 job = -1
  600    call ximagewait(job)
	 if(job .le. 0) then
	  go to 600
C*** return previous menu
	 else if(job .eq. 1) then
	  call ximagemenuhide
	  call ximagelabelhide
	  go to 200
C*** return main menu
	 else if(job .eq. 2) then
	  call ximagemenuhide
	  call ximagelabelhide
	  call ximageremovevectors
	  return
C*** box drawn, read coords
	 else if(job .eq. 105) then
	  nboxes = nboxes + 1
	  call ximagerubberread
     *    (ixscreen1,iyscreen1,ixscreen2,iyscreen2)
	  call average_calc
     *    (ixscreen1,iyscreen1,ixscreen2,iyscreen2)
	  call ximagemenuhide
	  if(ierr .eq. 1) then
	   iolabel(nlabels+1) = '!!Error - box too large for program'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	  else if(ierr .eq. 2) then
	   iolabel(nlabels+1) = '!!Error - coordinates out of range'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	  end if
	 end if
	 go to 300
C*******************************************************
C*** draw a box by point specification
C*******************************************************
	else if(job .eq. 2) then
	 call ximagelabelhide
	 call ximagemenuhide
C*** write previous mean to output file
	 if(nboxes .gt. 0) call average_write
	 iolabel(nlabels+1) = 
     *   'Mark bottom left corner of box with left hand mouse button'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagereadpointer(ixscreen1,iyscreen1)
	 call draw_cross(ixscreen1,iyscreen1,4)
	 iolabel(nlabels+1) = 
     *   'Mark top right corner of box with left hand mouse button'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagereadpointer(ixscreen2,iyscreen2)
	 call remove_cross(ixscreen1,iyscreen1,4)
	 call ximagedrawbox(ixscreen1,iyscreen1,ixscreen2,iyscreen2)
	 call average_calc
     *   (ixscreen1,iyscreen1,ixscreen2,iyscreen2)
	 if(ierr .eq. 1) then
	  iolabel(nlabels+1) = '!!Error - box too large for program'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	 else if(ierr .eq. 2) then
	  iolabel(nlabels+1) = '!!Error - coordinates out of range'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	 else
	  nboxes = nboxes + 1
	 end if
	 go to 300
C*************************************************
C*** specify size and position cursor
C*************************************************
	else if(job .eq. 3) then
	 call ximagelabelhide
	 call ximagemenuhide
	 nlabels = 2
	 iolabel(nlabels) = 
     *   'Type in box size x and y in pixels'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels)
	 call extract_integers(2,return_string,
     *   ixsize,iysize,i3,i4,i5,i6)
         iolabel(nlabels+1) = 
     *   'Mark box centres successively with left hand mouse button'
	 call ximagelabeldisplay(iolabel,nlabels+1)
 700     menulist(1) = 'Delete box just drawn'
	 menulist(2) = 'Return average box menu'
	 call ximagemenuinit(menulist,2)
	 iopt = -1
 750     call ximagewait(iopt)
	 if(iopt .lt. 0) then
	  go to 750
C*** read pointer position for box centre
	 else if(iopt .eq. 0) then
	  call ximagereadmenupointer(ixscreen,iyscreen)
C*** write previous mean to output file provided delete not specified
	  if(nboxes .gt. 0 .and. numxy .gt. 0) 
     *     call average_write
	  call ximagedrawpoint(ixscreen,iyscreen)
	  ixscreen1 = ixscreen - ixsize / 2
	  ixscreen2 = ixscreen1 + ixsize - 1
	  iyscreen1 = iyscreen - iysize / 2
	  iyscreen2 = iyscreen1 + iysize - 1
	  call ximagedrawbox(ixscreen1,iyscreen1,ixscreen2,iyscreen2)
	  call average_calc
     *    (ixscreen1,iyscreen1,ixscreen2,iyscreen2)
	  if(ierr .eq. 1) then
	   iolabel(nlabels+2) = '!!Error - box too large for program'
	   call ximagelabeldisplay(iolabel,nlabels+2)
	  else if(ierr .eq. 2) then
	   iolabel(nlabels+2) = '!!Error - coordinates out of range'
	   call ximagelabeldisplay(iolabel,nlabels+2)
	  else
	   nboxes = nboxes + 1
	  end if
	  go to 750
C*** delete box just drawn
	 else if(iopt .eq. 1) then
	  call ximageremovepoint(ixscreen,iyscreen)
	  call ximageremovebox(ixscreen1,iyscreen1,ixscreen2,iyscreen2)
	  ameantot = ameantot - amean
	  if(.not. image) bmeantot = bmeantot - bmean
	  npixels = npixels - numxy
	  numxy = 0
	  nboxes = nboxes - 1
	  nlabels = nlabels - 1
	  go to 700
C*** return average box menu
	 else if (iopt .eq. 2) then
	  if(nboxes .gt. 0 .and. numxy .gt. 0) 
     *     call average_write
	  call ximagemenuhide
	  call ximagelabelhide
	  nlabels = 2
	  go to 300
	 end if
C*************************************************
C*** redraw box
C*************************************************
	else if(job .eq. 4) then
	 call ximagelabelhide
	 call ximagemenuhide
	 iolabel(nlabels+1) = 
     *   'Mark bottom left corner of box with left hand mouse button'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagereadpointer(kxscreen1,kyscreen1)
	 call draw_cross(kxscreen1,kyscreen1,4)
	 iolabel(nlabels+1) = 
     *   'Mark top right corner of box with left hand mouse button'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagereadpointer(kxscreen2,kyscreen2)
	 call remove_cross(kxscreen1,kyscreen1,4)
	 call ximagedrawbox(kxscreen1,kyscreen1,kxscreen2,kyscreen2)
	 call ximageremovebox(ixscreen1,iyscreen1,ixscreen2,iyscreen2)
C*** subtract previous values
	 ameantot = ameantot - amean
	 if(.not. image) bmeantot = bmeantot - bmean
	 npixels = npixels - numxy
	 ixscreen1 = kxscreen1
	 iyscreen1 = kyscreen1
	 ixscreen2 = kxscreen2
	 iyscreen2 = kyscreen2
	 nlabels = nlabels - 1
	 call average_calc
     *   (ixscreen1,iyscreen1,ixscreen2,iyscreen2)
	 if(ierr .eq. 1) then
	  iolabel(nlabels+1) = '!!Error - box too large for program'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	 else if(ierr .eq. 2) then
	  iolabel(nlabels+1) = '!!Error - coordinates out of range'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	 end if
	 go to 300
C******************************************************
C*** calculate total average for all boxes
C******************************************************
	else if (job .eq. 5) then
	 call ximagemenuhide
	 if(nboxes .le. 0) go to 200
C*** write previous mean to output file
	 call average_write
	 nlabels = min(nlabels+1,max_lines_per_page)
	 ameantot = ameantot / float(nboxes)
C*** image
	 if(image) then
          write(iolabel(nlabels),'(
     *    '' Over '',i3,'' boxes '',i8,'' pixels, mean density ='',
     *    f12.2)') nboxes, npixels, ameantot
          if(avout) write(idevavg,'(
     *    '' Over '',i3,'' boxes '',i8,'' pixels, mean density ='',
     *    f12.2)') nboxes, npixels, ameantot
C*** transform
	 else
	  bmeantot = bmeantot / float(nboxes)
          write(iolabel(nlabels),'('' Over '',i3,'' boxes'',i8,
     *    '' pixels, mean int, amp ='',2f12.2)')
     *    nboxes, npixels, ameantot, bmeantot
          if(avout) write(idevavg,'('' Over '',i3,'' boxes'',i8,
     *    '' pixels, mean int, amp ='',2f12.2)')
     *    nboxes, npixels, ameantot, bmeantot
	 end if
	 call ximagelabelhide
	 iolabel(nlabels+1) = 'Erase boxes ? (y/n)'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 if(return_string .eq. 'y' .or. return_string .eq. 'Y')
     *   call ximageremovevectors
	 nlabels = 2
	 go to 150
C******************************************************
C*** Return main menu
C******************************************************
	else if (job .eq. 6) then
	 if(nboxes .gt. 0) then
	  call average_write
	  if(avout) close(idevavg)
	 end if
	 call ximagemenuhide
	 call ximagelabelhide
	end if
	return
	end
C*************************************************************************
C***
        subroutine average_calc(ixscreen1,iyscreen1,ixscreen2,iyscreen2)
C***
C*************************************************************************
C*** subroutine to calculate average value for a boxes drawn on screen, 
C*************************************************************************
        include    'Ximdisp_common.for'

C*** convert screen to map coordinates
	call convert_to_image(ixscreen1,iyscreen1,iximage1,iyimage1)
	call convert_to_image(ixscreen2,iyscreen2,iximage2,iyimage2)
        numx = iximage2 - iximage1 + 1
        numy = iyimage2 - iyimage1 + 1
C*** error traps
	ierr = 0
	if(numx .gt. max_spline_width .or. numy .gt. max_spline_height) then
	 ierr = 1
	 return
	else if(iximage1 .le. 0 .or. iximage2 .le. 0 .or. 
     *          iyimage1 .le. 0 .or. iyimage2 .le. 0 .or.
     *          iximage1 .gt. nxyz(1) .or. iximage2 .gt. nxyz(1) .or.
     *          iyimage1 .gt. nxyz(2) .or. iyimage2 .gt. nxyz(2)) then
	 ierr = 2
	 return
	end if
C*** image
	if(image) then
	 amean = 0.
C*** nzstart for display of 1 section of multisection image
         call  imposn(idevmap,nzstart,iyimage1)
C*** start line reading loop
         do iy=1,numy
	  call irdlin(idevmap,aline)
          do ix=iximage1,iximage2
           amean = amean + aline(ix+1)
	   chunk(ix-iximage1+1,iy) = aline(ix+1)
          end do
         end do
	else
C***
C*** transform
	 amean = 0.
	 bmean = 0.
	 call convert_to_transform
     *   (iximage1,iyimage1,ixtrans1,iytrans1)
	 call convert_to_transform
     *   (iximage2,iyimage2,ixtrans2,iytrans2)
	 ix1 = min(ixtrans1,ixtrans2)
	 ix2 = max(ixtrans1,ixtrans2)
	 iy1 = min(iytrans1,iytrans2)
	 iy2 = max(iytrans1,iytrans2)
C*** x values same sign therefore in same quadrant
	 if(iximage1 * iximage2 .ge. 0) then
	  call imposn(idevmap,0,iy1)
C*** start loop to read intensities
	  do iy=1,numy
	   call irdlin(idevmap,aline)
	   do ix=ix1,ix2,2
	    aval = aline(ix)
	    bval = aline(ix + 1)
	    den = aval * aval + bval * bval
	    amean = amean + den
	    bmean = bmean + sqrt(den)
	   end do
	  end do
C*** x values different sign therefore in different quadrants
	 else
C*** extract lh side
	  call imposn(idevmap,0,iy2-numy+1)
	  do iy=1,numy
	   call irdlin(idevmap,aline)
	   do ix=1,ix1,2
	    aval = aline(ix)
	    bval = aline(ix+1)
	    den = aval * aval + bval * bval
	    amean = amean + den
	    bmean = bmean + sqrt(den)
	   end do
	  end do
C*** extract rhs
	  call imposn(idevmap,0,iy1-numy+1)
	  do iy=1,numy
	   call irdlin(idevmap,aline)
	   do ix=1,ix2,2
	    aval = aline(ix)
	    bval = aline(ix+1)
	    den = aval * aval + bval * bval
	    amean = amean + den
	    bmean = bmean + sqrt(den)
	   end do
	  end do
	 end if
	end if
        numxy = numx * numy
	fnumxy = float(numxy)
        amean = amean / fnumxy
	nlabels = nlabels + 1
	if(nlabels .ge. max_lines_per_page) nlabels = 4
	if(image) then
C*** calculate sdev
	 sumxsq = 0.
	 do iy=1,numy
	  do ix = 1,iximage2-iximage1+1
	   sumxsq = sumxsq + chunk(ix,iy) * chunk(ix,iy)
	  end do
	 end do
	 sdev = 0.
	 var = (sumxsq - fnumxy * amean * amean) / (fnumxy - 1.)
	 if(var .gt. varmin) sdev = sqrt(var)
         write(iolabel(nlabels),'('' Mean density = '',
     *   f12.2,'' over '',i8,'' pixels,  sdev =''f10.5)') 
     *   amean, numxy, sdev
	else
	 bmean = bmean / fnumxy
         write(iolabel(nlabels),'('' Mean int, amp = '',
     *   2f12.2,'' over  '',i8,'' pixels'')') amean, bmean, numxy
	end if
	call ximagelabeldisplay(iolabel,nlabels)
	return
	end
C*************************************************************************
C***
        subroutine average_write
C***
C*************************************************************************
C*** subroutine to write mean values to output file
C*************************************************************************
        include    'Ximdisp_common.for'
	if(image) then
	 if(avout) write(idevavg,'('' Mean density = '',
     *   f12.2,'' over '',i8,'' pixels'')') amean, numxy
	 ameantot = ameantot + amean
	else
         if(avout) write(idevavg,'('' Mean amp, int = '',
     *   2f12.2,'' over '',i8,'' pixels'')') amean, bmean, numxy
	 ameantot = ameantot + amean
	 bmeantot = bmeantot + bmean
	end if
	npixels = npixels + numxy
	return
	end
C*************************************************************************
C***
        subroutine box_area
C***
C*************************************************************************
C***
C*** subroutine to control boxing/dumping an area in image or postscript
C*** format
C*** calls box_shape, box_out

        include           'Ximdisp_common.for'
C***
	ipixmap = 0
	call ximagemenuhide
	call ximagelabelhide
	iolabel(nlabels+1) = 'Postscript dumps on large Xterms only'
	call ximagelabeldisplay(iolabel,nlabels+1) 
	menulist(1) = 'Box map area    - image format'
	menulist(2) = 'Box screen area - image format'
	menulist(3) = 'Whole screen    - image format'
	menulist(4) = 'Whole screen    - postscript format'
	menulist(5) = 'Box screen area - postscript format'
	menulist(6) = 'Return main menu'
	call ximagemenuinit(menulist,6)
	job = -1
 500    call ximagewait(job)
	if(job.le.0) then
	 go to 500
	else if(job .le. 3) then
C*** Box map area - image format
	 if(job .eq. 1) then
C*** return if transform
          if(.not.image) then
	   call ximagemenuhide
           iolabel(nlabels+1) = 
     *     'Cannot create boxed area from transform'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	   return
C*** return if multisection
	  else if(multisection) then
           iolabel(nlabels+1) = 
     *     'Cannot read map from multisection image'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	   return
C*** return if montage
	  else if(montage) then
           iolabel(nlabels+1) = 'Cannot read map from montage'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	   return
          end if
	  postscript = .false.
	  dump_screen = .false.
	  screen = .false.
C*** Box screen area - image format
	 else if(job .eq. 2) then
	  postscript = .false.
	  dump_screen = .false.
	  screen = .true.
C*** Whole screen - image format
	 else if(job .eq. 3) then
	  if(max_display_width .gt. max_screen_width .or.
     *       max_display_height .gt. max_screen_height) then
           iolabel(nlabels+1) = 
     *     'Dumping whole map including off screen area'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	  end if
	  postscript = .false.
	  dump_screen = .true.
	  screen = .true.
	 end if
	 go to 550
C*** Whole screen - postscript format
	else if(job .eq. 4) then
	 if(max_display_width .gt. max_screen_width .or.
     *      max_display_height .gt. max_screen_height) then
          iolabel(nlabels+1) = 
     *    'Dumping whole map including off screen area'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	 end if
	 postscript = .true.
	 dump_screen = .true.
	 screen = .true.
C*** Box screen area - postscript format
	else if(job .eq. 5) then
	 postscript = .true.
	 dump_screen = .false.
	 screen = .true.
C*** return main menu
	else if(job .eq. 6) then
	 call ximagemenuhide
	 nlabels = 1
	 call ximagelabeldisplay(iolabel,nlabels)
	 return
	end if
  550	call ximagemenuhide
	call ximagelabelhide
C*****************************************************
C*** postscript format output
C*****************************************************
	if(postscript) then
	 call ximagelabelhide
	 call ximagemenuhide
C*** get -ontal size in mm
	 write(iolabel(nlabels+1),'(
     *   ''Type horizontal size in mm, <cr> to fill page'')')
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 if(return_string .eq. ' ' .or. return_string .eq. '/') then
	  xsize = 0.
	 else
	  call extract_reals(1,return_string,
     *    xsize,f2,f3,f4,f5,f6)
	 end if
	 call ximagelabeldisplay(iolabel,nlabels)
	 menulist(1) = 'Grey-scale'	
	 menulist(2) = 'Colour'	
	 menulist(3) = 'Return main menu'
	 call ximagemenuinit(menulist,3)
	 job = -1
 600     call ximagewait(job)
	 if(job.le.0) then
	  go to 600
C*** Grey-scale
	 else if(job.eq.1) then
	  colour = .false.
C*** Colour
	 else if(job.eq.2) then
	  colour = .true.
C*** return main menu
	 else if(job.eq.3) then
	  call ximagemenuhide
	  nlabels = 1
	  call ximagelabeldisplay(iolabel,nlabels)
	  return
	 end if
	 call ximagemenuhide
C***
C*** Whole screen
	 if(dump_screen) then
	  mnx = 0
	  mxx = 0
	  mny = 0
	  mxy = 0
C*** Dump boxed screen area
	 else
	  call rectangle
	  if(ierr .ne. 0) then
	   call ximageremovevectors
	   nlabels = 1
	   call ximagelabeldisplay(iolabel,nlabels)
	   return
	  end if
	 end if
c	 call convert_to_screen(mnx,mny,iminx,iminy)
c	 call convert_to_screen(mxx,mxy,imaxx,imaxy)
c	 mnx = iminx
c	 mxx = imaxx
c	 mny = iminy
c	 mxy = imaxy
	 numx = 2 * (mxx - mnx + 1) / 2
	 numy = 2 * (mxy - mny + 1) / 2
	 iolabel(nlabels+1) = 
     *   'Wait for postscript dump to file : Ximage.ps ...'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagepostscriptdump
     *   (idevpost,colour,mnx,mxx,mny,mxy,xsize,ipixmap,ierr)
	 if(ierr .ne. 0) then
	  iolabel(nlabels+1) = 
     *    'Consistency check failure - postscript dump aborted'
	  iolabel(nlabels+2) = 'Type <cr> to return main menu'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	  nlabels = nlabels - 1
	 end if
	 call ximageremovevectors
	 nlabels = 1
	 call ximagelabeldisplay(iolabel,nlabels)
	 return
	end if
C*****************************************************
C*** Image format output
C*****************************************************
  900   ierr = 0
C***
C*** screen input to image format output
	if(screen) then
	 call convert_to_screen(mnx,mny,iminx,iminy)
	 call convert_to_screen(mxx,mxy,imaxx,imaxy)
	 mnx = iminx
	 mxx = imaxx
	 mny = iminy
	 mxy = imaxy
	 numx = 2 * (mxx - mnx + 1) / 2
	 numy = 2 * (mxy - mny + 1) / 2
C*** dump whole screen
	 if(dump_screen) then
	  mnx = 0
          mxx = max_display_width - 1
	  mny = 0
          mxy = max_display_height - 1
	  numx = mxx - mnx + 1
	  numy = mxy - mny + 1
          nxpad = numx
          nypad = numy
C*** dump boxed area of screen
	 else
          call rectangle
	  if(ierr .ne. 0) then
	   call ximageremovevectors
	   nlabels = 1
	   call ximagelabeldisplay(iolabel,nlabels)
	   return
	  end if
	  nxpad = numx
	  nypad = numy
	 end if
	 ncenx = numx / 2
	 nceny = numy / 2
         iolabel(nlabels+1) = 'Output file name ...'
	 boxfile = ' '
	 call ximageioboxdisplay(iolabel,boxfile,nlabels+1)
	 old = .false.
	 call check_file(boxfile)
         call box_out
	 call ximagemenuhide
	 call ximageremovevectors
	 nlabels = 1
	 call ximagelabeldisplay(iolabel,nlabels)
         return
	end if
C*****************************************************
C*** box area from input map
C*****************************************************
        iolabel(nlabels+1) = 'Output file name ...'
	boxfile = ' '
	call ximageioboxdisplay(iolabel,boxfile,nlabels+1)
	old = .false.
	call check_file(boxfile)
	rect = .false.
	poly = .false.
	circ = .false.
C*** menu to decide box type
	call ximagelabeldisplay(iolabel,nlabels)
	menulist(1) = 'Rectangular box'
	menulist(2) = 'Polygonal box'
	menulist(3) = 'Circular box'
	menulist(4) = 'Return main menu'
	call ximagemenuinit(menulist,4)
	job = -1
 1200   call ximagewait(job)
	if(job.le.0) then
	 go to 1200
	else if(job.eq.1) then
	 call ximagemenuhide
	 rect = .true.
	 call rectangle
	else if(job.eq.2) then
	 call ximagemenuhide
	 poly = .true.
	 call polygon
	else if(job.eq.3) then
	 call ximagemenuhide
	 circ = .true.
	 call circle
	else if(job.eq.4) then
	 call ximagemenuhide
	 call ximageremovevectors
	 nlabels = 1
	 call ximagelabeldisplay(iolabel,nlabels)
	 return
	end if
C*** return main menu
        if(ierr.eq.1) then
	 call ximageremovevectors
	 nlabels = 1
	 call ximagelabeldisplay(iolabel,nlabels)
	 return
C*** error - start again
	else if(ierr.eq.2) then
	 call ximageremovevectors
	 go to 900
	end if
C*******************************************************************
C*** boxing finished, set up padding and/or floating for output file
C*******************************************************************
 1300   invert = .false.
	nlabels = 1
	if(screen) then
         floatim = .false.
        else
	 iolabel(nlabels+1) =
     *   'Pad and/or float or zero the boxed image ?'
	 if(mode .eq. 0) then
	  iolabel(nlabels+2) = 
     *   'Warning : mode 0, if you float the image, '//
     *    'the background will be'
	  iolabel(nlabels+3) = 
     *   'set to perimeter average or you may output the'//
     *   ' file as a 2-byte map'
	  call ximagelabeldisplay(iolabel,nlabels+3)
	 else
	  call ximagelabeldisplay(iolabel,nlabels)
	 end if
C************************************************************
C*** set up menu to decide padding/floating
C************************************************************
	 menulist(1) = 'Do not pad, do not float'
	 menulist(2) = 'Do not pad, do float'
	 menulist(3) = 'Do pad, do not float'
	 menulist(4) = 'Do pad, do float'
	 menulist(5) = 'Set boxed area to zero'
	 menulist(6) = 'Return main menu'
	 call ximagemenuinit(menulist,6)
	 job = -1
 1400    call ximagewait(job)
	 if(job.le.0) then
	  go to 1400
C*** do not pad
	 else if(job .le. 2) then
	  call ximagelabelhide
	  call ximagemenuhide
          nxpad = numx
          nypad = numy
          ncenx = numx / 2
          nceny = numy / 2
C*** do not pad, do not float
	  if(job .eq. 1) then
	   floatim = .false.
C*** do not pad, do float
	  else
	   floatim = .true.
	  end if
C*** do pad
	 else if(job .le. 4) then
	  call ximagelabelhide
	  call ximagemenuhide
C*** calculate padded box size
	  nxpad = max(numx,numy)
	  nypad = 2
 1450	  nypad = nypad * 2
	  if(nxpad .gt. nypad) go to 1450
	  nxpad = nypad
	  write(iolabel(nlabels+1),
     *    '(''Minimum padded box size = '',i6,'' * '',i6)') nxpad, nypad
	  iolabel(nlabels+2) =
     *    'Type new size or <cr> to accept default'
 1500	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	  if(return_string .ne. '/' .and. return_string .ne. ' ') then
	   call extract_integers(2,return_string,
     *     nxpad,nypad,i3,i4,i5,i6)
	   if(nypad .le. 0) go to 1500
	  end if
C*** specify box centre
	  nlabels = 1
	  iolabel(nlabels+1) = 'Specify padded box centre'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	  menulist(1) = 'Image bottom left'
	  menulist(2) = 'Image centre padded area'
	  menulist(3) = 'Type centre coords relative to padded area'
	  menulist(4) = 'Return main menu'
	  call ximagemenuinit(menulist,4)
	  iopt = -1
 1420     call ximagewait(iopt)
	  if(iopt .le. 0) then
	   go to 1420
C*** image bottom left
	  else if(iopt .eq. 1) then
	   ncenx = numx / 2
	   nceny = numy / 2
C*** image centre padded area
	  else if(iopt .eq. 2) then
	   ncenx = nxpad / 2
	   nceny = nypad / 2
C*** Type centre coords
	  else if(iopt .eq. 3) then
	   call ximagemenuhide
	   call ximagelabelhide
	   return_string = ' '
	   iolabel(nlabels+1) = 'Type centre coordinates ...'
	   call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	   call extract_integers(2,return_string,
     *     ncenx,nceny,i3,i4,i5,i6)
	   if(ncenx .gt. 0 .and. nceny .le. 0) go to 1500
           ncenx = max(ncenx,numx/2)
           nceny = max(nceny,numy/2)
C*** return main menu
	  else if(iopt .eq. 4) then
	   call ximagemenuhide
	   call ximagelabelhide
	   call ximageremovevectors
	   nlabels = 1
	   call ximagelabeldisplay(iolabel,nlabels)
	   return
	  end if
	  call ximagemenuhide
	  call ximagelabelhide
C*** do pad, do not float
	  if(job .eq. 3) then
	   floatim = .false.
C*** do pad, do float
	  else
	   floatim = .true.
	  end if
C*** do not pad, zero boxed area
	else if(job .eq. 5) then
	  call ximagelabelhide
	  if(.not.rect) then
	   iolabel(nlabels+1) = 
     *     'Option not available for circles or polygons'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	   go to 1400
	  end if
	  call ximagemenuhide
C*** mod 26.08.05
c          nxpad = nox
c          nypad = noy
c          ncenx = nox / 2
c          nceny = noy / 2
          nxpad = nxyz(1)
          nypad = nxyz(2)
          ncenx = nxyz(1) / 2
          nceny = nxyz(2) / 2
	  floatim = .false.
	  invert = .true.
C*** return main menu
	 else if(job .eq. 6) then
	  call ximagemenuhide
	  call ximageremovevectors
	  nlabels = 1
	  call ximagelabeldisplay(iolabel,nlabels)
	  return
	 end if
	end if
C*******************************************************************
C*** Output boxed area
C*******************************************************************
        call box_out
	if(ierr .eq. 1) return
	write(iolabel(nlabels+1),'(''Boxed area written to : '',a)')
     *  boxfile(1:lnblank(boxfile))
	call ximagelabeldisplay(iolabel,nlabels+1)
	menulist(1) = 'Box another area'
	menulist(2) = 'Return main menu'
	call ximagemenuinit(menulist,2)
        job = -1
 1600   call ximagewait(job)
	if(job.le.0) then
	 go to 1600
C*** box another area
	else if(job.eq.1) then
	 call ximageremovevectors
	 call ximagemenuhide
	 call ximagelabelhide
	 go to 900
C*** return main menu
	else if(job.eq.2) then
	 call ximageremovevectors
	 call ximagemenuhide
	 nlabels = 1
	 call ximagelabeldisplay(iolabel,nlabels)
	 return
	end if
        end
C**************************************************************************
        subroutine box_shape
C**************************************************************************
C***
C*** subroutine to draw shapes and cut for boxing
C*** entry points - circle, polygon, rectangle
        include        'Ximdisp_common.for'
        entry circle
	nlabels = 1
C**************************************************************************
C       circle
C**************************************************************************
 1000   menulist(1) = 'Rubberband circle'
	menulist(2) = 'Centre specification by cursor'
	menulist(3) = 'Centre specification by keyboard input'
	menulist(4) = 'Return main menu'
	call ximagemenuinit(menulist,4)
	job = -1
 1020   call ximagewait(job)
	if(job.le.0) then
	 go to 1020
C*** rubberband circle
	else if(job .eq. 1) then
	 call ximagelabelhide
	 call ximagemenuhide
 	 call ximagerubberenable(3)
         iolabel(nlabels+1) = 
     *   'Ctrl/Left button down at circle centre'
         iolabel(nlabels+2) = 
     *   'Drag circumference to required limit'
         iolabel(nlabels+3) = 
     *   'Release mouse button to record final position'
	 call ximagelabeldisplay(iolabel,nlabels+3)
	 menulist(1) = 'Re-start box specification'
	 menulist(2) = 'Return main menu'
	 call ximagemenuinit(menulist,2)
	 iopt = -1
 1030    call ximagewait(iopt)
	 if(iopt .lt. 0) then
	  go to 1030
C*** read centre position
	 else if(iopt .eq. 0) then
	  call ximagereadmenupointer(icircx,icircy)
	  call draw_cross(icircx,icircy,4)
	  call convert_to_image(icircx,icircy,mcircx,mcircy)
	 else if(iopt .eq. 1) then
	  call ximagemenuhide
	  go to 1000
C*** return main menu
	 else if(iopt .eq. 2) then
	  call ximagemenuhide
          ierr = 1
	  return
	 else if(iopt .eq. 105) then 
	  call remove_cross(icx,icy,4)
	  call ximagerubberread(icircx,icircy,iex,iey)
	  call draw_cross(icircx,icircy,4)
C*** calculate radius and convert screen to map coords
          xsq = float(icircx - iex)
          xsq = xsq * xsq
          ysq = float(icircy - iey)
          ysq = ysq * ysq
          radcomp = sqrt(ysq + xsq)
	  radius = radcomp * float(icompress)
	  call convert_to_image(icircx,icircy,mcircx,mcircy)
	  go to 1100
	 end if
C*** choose centre with cursor
	else if(job .eq. 2) then
	 call ximagelabelhide
	 call ximagemenuhide
	 iolabel(nlabels+1) = 
     *   'Mark cursor position at centre of circle'
 	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagereadpointer(icircx,icircy)
	 call convert_to_image(icircx,icircy,mcircx,mcircy)
	 call ximagelabelhide
C*** type in centre
	else if(job .eq. 3) then
	 call ximagelabelhide
	 call ximagemenuhide
 1040	 iolabel(nlabels+1) = 'Type in centre position in map coordinates'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 if(screen) then
	  call extract_integers(2,return_string,
     *    icircx,icircy,i3,i4,i5,i6)
	  if(icircy .le. 0) go to 1040
	  call convert_to_image(icircx,icircy,mcircx,mcircy)
	 else
	  call extract_integers(2,return_string,
     *    mcircx,mcircy,i3,i4,i5,i6)
	  if(mcircy .le. 0) go to 1040
	  call convert_to_screen(mcircx,mcircy,icircx,icircy)
	 end if
C*** return main menu
	else if(job .eq. 4) then
	 call ximagelabelhide
	 call ximagemenuhide
	 ierr = 1
	 return
	end if
C*** draw dot to mark centre of circle
	call draw_cross(icircx,icircy,4)
	call ximagelabelhide
C*** menu to control radius specification
 1050	call ximagelabeldisplay(iolabel,nlabels)
	menulist(1) = 'Radius specification by keyboard input'
	menulist(2) = 'Re-specify circle centre'
	menulist(3) = 'Return main menu'
	call ximagemenuinit(menulist,3)
	job = -1
 1060   call ximagewait(job)
	if(job.le.0) then
	 go to 1060
	else if(job .eq. 1) then
C*** type in radius
	 call ximagelabelhide
	 call ximagemenuhide
 1080	 iolabel(nlabels+1) = 
     *   'Type in radius in uncompressed map pixels'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 call extract_integers(1,return_string,
     *   irad,i2,i3,i4,i5,i6)
	 radius = float(irad)
	 radcomp = radius / float(icompress)
	 if(radcomp .le. 0) go to 1080
	 irad = nint(radcomp)
C*** modify circle centre
	else if(job .eq. 2) then
	 icx = icircx
	 icy = icircy
	 iolabel(nlabels+1) = 
     *   'Mark cursor position at centre of circle'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagereadpointer(icircx,icircy)
C*** remove old dot and draw new dot to mark centre of circle
	 call remove_cross(icx,icy,4)
	 call draw_cross(icircx,icircy,4)
	 call convert_to_image(icircx,icircy,mcircx,mcircy)
	 call ximagelabelhide
C*** return main menu
	else if(job .eq. 3) then
	 call ximagelabelhide
	 call ximagemenuhide
	 ierr = 1
	 return
	end if
C***
C*** calculate min, max x, y values 
 1100   call ximageremovevectors
	call draw_cross(icircx,icircy,4)
	irad = nint(radcomp)
	call ximagedrawcircle(icircx,icircy,irad)
	nrad = nint(radius)
	mnx = mcircx - nrad
        mxx = mcircx + nrad
        mny = mcircy - nrad
        mxy = mcircy + nrad
        if(mxx .ge. nxyz(1)) go to 1300
        if(mnx .lt. 0) go to 1300
        if(mxy .ge. nxyz(2)) go to 1300
        if(mny .lt. 0) go to 1300
        go to 1400
C*** diagnostic
 1300   iolabel(nlabels+1) = 'radius out of bounds'
	call ximagelabeldisplay(iolabel,nlabels+1)
        go to 1000
C***
C*** check circle acceptable - write out map coords and radius
 1400   write(iolabel(nlabels+1),'(
     *  '' centre coords ='',2i6,'' Radius ='',i6)')
     *  mcircx,mcircy,nint(radius)
	call ximagelabeldisplay(iolabel,nlabels+1)
 	menulist(1) = 'Accept circle'
	menulist(2) = 'Re-draw circle'
	menulist(3) = 'Return main menu'
	call ximagemenuinit(menulist,3)
	job = -1
 1500   call ximagewait(job)
	if(job.le.0) then
	 go to 1500
C*** circle ok
	else if(job .eq. 1) then
	 call ximagemenuhide
	 go to 1600
C*** redraw circle
	else if(job .eq. 2) then
	 call ximagemenuhide
	 icx = icircx
	 icy = icircy
	 go to 1000
C*** return main menu
	else if(job .eq. 3) then
	 call ximagemenuhide
	 ierr = 1
	 return
	end if
C***
C*** calculate box size from radius
 1600   numx = nint(2. * radius)
        numy = numx
	ierr = 0
	return
C*********************************************************************
        entry rectangle
C*********************************************************************
C*** rectangular box
        npts = 4
C*** get method of box specification
 2000   iolabel(nlabels+1) = 
     *  'Indicate method of rectangle specification'
 2050   call ximagelabeldisplay(iolabel,nlabels+1)
        nlabels = 1
	menulist(1) = 'Rubberband box'
	menulist(2) = 'Mark bottom left, top right'
	menulist(3) = 'Mark mid top,bottom and width'
	menulist(4) = 'Mark centre, type in size'
	menulist(5) = 'Return main menu'
	call ximagemenuinit(menulist,5)
	job = -1
 2100   call ximagewait(job)
	if(job.le.0) then
	 go to 2100
C**********************************************************
C*** rubberbanded box
C**********************************************************
	else if(job .eq. 1) then
         call ximagemenuhide
	 call ximagelabelhide
 	 call ximagerubberenable(2)
         iolabel(nlabels+1) = 
     *   'Ctrl/Left button down to start at bottom left corner'
         iolabel(nlabels+2) = 
     *   'Drag box to top right corner'
         iolabel(nlabels+3) = 
     *   'Release mouse button to record final position'
	 call ximagelabeldisplay(iolabel,nlabels+3)
	 menulist(1) = 'Re-start box specification'
	 menulist(2) = 'Return main menu'
	 call ximagemenuinit(menulist,2)
	 job = -1
 2150    call ximagewait(job)
	 if(job .le. 0) then
	  go to 2150
	 else if(job .eq. 1) then
	  call ximagemenuhide
	  go to 2000
C*** return main menu
	 else if(job .eq. 2) then
	  call ximagemenuhide
          ierr = 1
	  return
	 else if(job .eq. 105) then 
	  call ximagerubberread(ix1,iy1,ix2,iy2)
	  ixcomp(1) = ix1
	  ixcomp(2) = ix2
	  ixcomp(3) = ix2
	  ixcomp(4) = ix1
	  iycomp(1) = iy1
	  iycomp(2) = iy1
	  iycomp(3) = iy2
	  iycomp(4) = iy2
C*** convert screen coordinates if reading from map
	  if(.not.screen) then
	   do i=1,4
	    call convert_to_image(ixcomp(i),iycomp(i),ixp(i),iyp(i))
	   end do
	  end if
	 end if
C**********************************************************
C*** bottom left, top right
C**********************************************************
	else if (job .eq. 2) then
         call ximagemenuhide
         call ximagelabelhide
         iolabel(nlabels+1) =
     *   'Mark cursor position bottom left corner of rectangle'
         call ximagelabeldisplay(iolabel,nlabels+1)
         call ximagereadpointer(ixcomp(1),iycomp(1))
         call ximagelabelhide
C*** draw dot to mark bottom lh corner of box
	 call draw_cross(ixcomp(1),iycomp(1),4)
         call ximagelabeldisplay(iolabel,nlabels)
C*** mini-menu to decide how to get box specs
 2180    call ximagelabeldisplay(iolabel,nlabels)
	 menulist(1) = 'Specify top right corner with cursor'
	 menulist(2) = 'Specify size by keyboard input'
	 menulist(3) = 'Re-specify bottom left corner'
	 menulist(4) = 'Return main menu'
         call ximagemenuinit(menulist,4)
	 job = -1
 2200    call ximagewait(job)
	 if(job.le.0) then
	  go to 2200
C*** cursor driven box
	 else if(job.eq.1) then
	  call ximagemenuhide
	  iolabel(nlabels+1) = 
     *    'Mark cursor position top right corner of rectangle'
 	  call ximagelabeldisplay(iolabel,nlabels+1)
	  call ximagereadpointer(ixcomp(3),iycomp(3))
	  call ximagelabelhide
          ixcomp(2) = ixcomp(1)
          iycomp(2) = iycomp(3)
          ixcomp(4) = ixcomp(3)
          iycomp(4) = iycomp(1)
C*** convert screen coordinates if reading from map
	  if(.not.screen) then
	   do i=1,4
	    call convert_to_image(ixcomp(i),iycomp(i),ixp(i),iyp(i))
	   end do
	  end if
C*** type in box size
	 else if(job.eq.2) then
	  call ximagemenuhide
	  if(multisection .or. montage) then
	   iolabel(nlabels+1) = 
     *     'Keyboard specification not possible for this type of image'
           call ximagelabeldisplay(iolabel,nlabels+1)
	   go to 2180
	  end if
 2300     iolabel(nlabels+1) = 
     *    'Type in -ontal and vertical uncompressed box size'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	  call extract_integers(2,return_string,
     *    ihor,ivert,i3,i4,i5,i6)
	  if(ivert .le. 0) go to 2300
	  call convert_to_image(ixcomp(1),iycomp(1),ixp(1),iyp(1))
          ixp(2) = ixp(1)
          iyp(2) = iyp(1) + ivert - 1
          ixp(3) = ixp(1) + ihor - 1
          iyp(3) = iyp(2)
          ixp(4) = ixp(3)
          iyp(4) = iyp(1)
	  do i=1,4
	   call convert_to_screen(ixp(i),iyp(i),ixcomp(i),iycomp(i))
	  end do
C*** re-specify bottom left corner
	 else if(job.eq.3) then
	  call ximagemenuhide
	  ix = ixcomp(1)
	  iy = iycomp(1)
	  iolabel(nlabels+1) = 
     *    'Mark cursor position bottom left corner of rectangle'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	  call ximagereadpointer(ixcomp(1),iycomp(1))
	  call ximagelabelhide
C*** remove old dot and draw new dot to mark bottom lh corner of box
	  call remove_cross(ix,iy,4)
	  call draw_cross(ixcomp(1),iycomp(1),4)
c	  call convert_to_image(ixcomp(1),iycomp(1),ixp(1),iyp(1))
	  call ximagemenudisplay
	  go to 2200
C*** return main menu
	 else if(job.eq.4) then
	  call ximagemenuhide
          ierr = 1
	  return
	 end if
	 call ximagemenuhide
C**********************************************************
C*** mid top, bottom and width
C**********************************************************
	else if(job .eq. 3) then
	 call ximagemenuhide
	 if(multisection .or. montage) then
	  iolabel(nlabels+1) = 
     *    'Boxing method not possible for multiple images'
	  go to 2050
	 end if
	 call ximagelabelhide
	 iolabel(nlabels+1) = 'Mark cursor position centre bottom'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagereadpointer(ixb,iyb)
	 call draw_cross(ixb,iyb,4)
	 iolabel(nlabels+1) = 'Mark cursor position centre top'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagereadpointer(ixt,iyt)
	 call draw_cross(ixt,iyt,4)
	 iolabel(nlabels+1) = 
     *   'Mark cursor position halfwidth away from either point'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagereadpointer(ixw,iyw)
	 call draw_cross(ixw,iyw,4)
	 call ximagelabelhide
C*** calculate line length
         dv = float(iyt - iyb)
         dh = float(ixt - ixb)
         dvsq = dv * dv
         dhsq = dh * dh
         dist1 = sqrt(dvsq + dhsq)
C*** calculate distance of width point from centre line
         cosphi = dh / dist1
         sinphi = dv / dist1
         dist2 = cosphi * float(iyw - iyb) - sinphi * float(ixw - ixb)
C*** calculate left & right increments from centre line
         inch = nint(dist2 * sinphi)
         incv = nint(dist2 * cosphi)
         ixcomp(1) = ixb + inch
         iycomp(1) = iyb - incv
         ixcomp(2) = ixb - inch
         iycomp(2) = iyb + incv
         ixcomp(3) = ixt - inch
         iycomp(3) = iyt + incv
         ixcomp(4) = ixt + inch
         iycomp(4) = iyt - incv
	 do i=1,4
	  call convert_to_image(ixcomp(i),iycomp(i),ixp(i),iyp(i))
	 end do
C**********************************************************
C*** centre, type size
C**********************************************************
	else if(job .eq. 4) then
	 call ximagemenuhide
	 if(multisection .or. montage) then
	  iolabel(nlabels+1) = 
     *    'Boxing method not possible for multiple images'
	  go to 2050
	 end if
	 iolabel(nlabels+1) = 'Mark cursor position at box centre'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagereadpointer(ixc,iyc)
	 call draw_cross(ixc,iyc,4)
 2400    iolabel(nlabels+1) = 
     *   'Type in uncompressed horizontal,vertical box size in pixels'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 call extract_integers(2,return_string,
     *   ihor,ivert,i3,i4,i5,i6)
	 if(ivert .le. 0) go to 2400
         ih2 = ihor / 2
         iv2 = ivert / 2
         iofx = mod(ihor,2)
         iofy = mod(ivert,2)
	 call convert_to_image(ixc,iyc,ixcent,iycent)	 
         ixp(1) = ixcent - ih2
         ixp(2) = ixp(1)
         ixp(3) = ixcent + ih2 - 1 + iofx
         ixp(4) = ixp(3)
         iyp(1) = iycent - iv2
         iyp(4) = iyp(1)
         iyp(2) = iycent + iv2 - 1 + iofy
         iyp(3) = iyp(2)
	 do i=1,4
	  call convert_to_screen(ixp(i),iyp(i),ixcomp(i),iycomp(i))
	 end do
C***
C*** return main menu
C***
	else if(job .eq. 5) then
	 call ximagemenuhide
	 call ximagelabelhide
	 ierr = 1
	 return
	end if
C*** write vertices to screen
	coordsout = .false.
        iolabel(nlabels+1) = 'Box coordinates'
        do i=1,npts
	 nlabels = nlabels + 1
	 if(screen) then
	  mapx = ixcomp(i)
	  mapy = iycomp(i)
	  mapmaxx = max_display_width
	  mapmaxy = max_display_height
	 else
	  mapx = ixp(i)
	  mapy = iyp(i)
	  mapmaxx = nxyz(1)
	  mapmaxy = nxyz(2)
	 end if
         write(iolabel(nlabels+1),'(2i6)') mapx, mapy
	 if(mapx .lt. 0 .or. mapx .ge. mapmaxx .or.
     *      mapy .lt. 0 .or. mapy .ge. mapmaxy)
     *   coordsout = .true.
	end do
C*** check box not out of bounds
	if(coordsout) then
	 nlabels = nlabels + 1
	 iolabel(nlabels+1) = 'Box out of bounds, please re-specify'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 coordsout = .false.
	 nlabels = nlabels + 1
	 go to 2000
	end if
	call ximagelabeldisplay(iolabel,nlabels+1)
C*** draw box for inspection
	call ximagedrawbox(ixcomp(1),iycomp(1), ixcomp(3), iycomp(3))
C*** final menu to check box
	menulist(1) = 'Accept box'
	menulist(2) = 'Re-draw box'
	menulist(3) = 'Erase boxes from screen'
	menulist(4) = 'Return main menu'
	call ximagemenuinit(menulist,4)
	job = -1
 2500   call ximagewait(job)
	if(job.le.0) then
	 go to 2500
C*** accept box, continue
	else if(job.eq.1) then
	 call ximagemenuhide
	 if(.not. postscript) call ximageremovevectors
	 call ximagedrawbox(ixcomp(1),iycomp(1), ixcomp(3), iycomp(3))
	 go to 5000
C*** redraw box
	else if(job.eq.2) then
	 call ximagemenuhide
	 call ximagelabelhide
	 go to 2000
C*** erase boxes from screen
	else if(job.eq.3) then
	 call ximageremovevectors
	 go to 2500
C*** return main menu
	else if(job.eq.4) then
	 call ximagemenuhide
	 ierr = 1
	 return
	end if
C**********************************************************************
        entry polygon
C**********************************************************************
C*** polygonal box, not corrected for compressed map as rectangular box
 3000   npts = 0
 3100	call ximagerubberenable(1)
C*** menu to get vertices
        iolabel(nlabels+1) = 
     *  'Mark cursor position successively at each vertex'
        iolabel(nlabels+2) = 
     *  'Move cursor with ctrl/left button down'
        iolabel(nlabels+3) = 
     *  'Release mouse button to record position'
        iolabel(nlabels+4) = 
     *  'Release ctrl, double click button on final position'
	call ximagelabeldisplay(iolabel,nlabels+4)
	menulist(1) = 'Delete line'
	menulist(2) = 'Re-start polygon specification'
	menulist(3) = 'Return main menu'
	call ximagemenuinit(menulist,3)
	job = -1
 3200   call ximagewait(job)
	if(job .le. 0) then
	 go to 3200
C*** finished vertex specification
	else if(job .eq. 1) then
	 call ximageremovelines(ix1,iy1,ix2,iy2,1)
	 if(npts .gt. 1) then
	  ix1 = ixcomp(npts-1)
	  iy1 = iycomp(npts-1)
	  ix2 = ixcomp(npts)
	  iy2 = iycomp(npts)
	 end if
	 npts = npts - 1
	 go to 3200
C*** re-start polygon specification
	else if(job .eq. 2) then
	 call ximageremovevectors
	 npts = 0
	 go to 3200
C*** return main menu
	else if(job .eq. 3) then
	 call ximagemenuhide
	 call ximagelabelhide
	 ierr = 1
	 return
C*** get line coords
	else if(job .eq. 104) then
	 call ximagerubberread(ix1,iy1,ix2,iy2)
	 if(ix1 .eq. ix2 .and. iy1 .eq. iy2) go to 3200
	 npts = npts + 1
C*** check array size
	 if(npts.gt.max_points) then
	  nlabels = 2
	  iolabel(nlabels) = 'Too many vertices for program'
	  ierr = 1
	  return
	 end if
	 ixcomp(npts) = ix1
	 iycomp(npts) = iy1
	 call ximagedrawlines(ix1,iy1,ix2,iy2,1)
	 go to 3200
C*** final position
	else if(job .eq. 105) then
	 call ximagelabelhide
	 call ximagemenuhide
	 call ximagerubberread(ix1,iy1,ix2,iy2)
	 if(ix1 .eq. ixcomp(npts) .and. iy1 .eq. iycomp(npts)) 
     *   go to 3300
	 npts = npts + 1
	 if(npts.gt.max_points) then
	  nlabels = 2
	  iolabel(nlabels) = 'Too many vertices for program'
	  ierr = 1
	  return
	 end if
	 ixcomp(npts) = ix1
	 iycomp(npts) = iy1
C*** reject if not enough points recorded
	 if(npts .eq. 1) go to 3100
C*** same point recorded twice, skip 2nd set of coords
 3300	 if(ix1 .eq. ix2 .and. iy1 .eq. iy2) go to 3400
	 npts = npts + 1
	 ixcomp(npts) = ix2
	 iycomp(npts) = iy2
	 call ximagedrawlines(ix1,iy1,ix2,iy2,1)
C*** draw extra line to close polygon
 3400   if(ixcomp(npts) .eq. ixcomp(1) .and. 
     *     iycomp(npts) .eq. iycomp(1)) then
	  npts = npts - 1
	 else
	  call ximagedrawlines
     *    (ixcomp(1),iycomp(1),ixcomp(npts),iycomp(npts),1)
	 end if
	end if
C***
C*** box finished, draw final line if not drawn already
 3500	if(ixcomp(1).eq.ixcomp(npts) .and. 
     *     iycomp(1).eq.iycomp(npts)) then
         npts = npts - 1
	else
         call ximagedrawlines
     *   (ixcomp(npts),iycomp(npts),ixcomp(1),iycomp(1),1)
        end if
C*** write vertices
	nlabels = 2
        iolabel(nlabels) = 'Map vertex coordinates'
        do i=1,npts
	 nlabels = nlabels + 1
C*** shuffle labels up
	 if(nlabels .eq. max_lines_per_page) then
	  do n=3,max_lines_per_page - 1
	   iolabel(n) = iolabel(n + 1)
	  end do
	  nlabels = nlabels - 1
	 end if
	 call convert_to_image(ixcomp(i),iycomp(i),ixp(i),iyp(i))
         write(iolabel(nlabels+1),'(2i6)') ixp(i), iyp(i)
	end do
	call ximagelabeldisplay(iolabel,nlabels+1)
	nlabels = 1
C*** menu to check box ok
	menulist(1) = 'Accept box'
	menulist(2) = 'Re-draw box'
	menulist(3) = 'Return main menu'
	call ximagemenuinit(menulist,3)
	job = -1
 3600   call ximagewait(job)
	if(job.le.0) then
	 go to 3600
C*** box ok
	else if(job .eq. 1) then
	 call ximagemenuhide
	 go to 5000
C*** redraw box
	else if(job .eq. 2) then
C*** remove all but most recent box
	 call ximageremovevectors
	 do i=1,npts-1
	  call ximagedrawlines
     *    (ixcomp(i),iycomp(i),ixcomp(i+1),iycomp(i+1),1)
	 end do
         call ximagedrawlines
     *   (ixcomp(npts),iycomp(npts),ixcomp(1),iycomp(1),1)
	 call ximagemenuhide
	 npts = 0
	 go to 3000
C*** return main menu
	else if(job .eq. 3) then
	 call ximagemenuhide
	 ierr = 1
	 return
	end if
C******************************************************************
C*** all boxes drawn, calculate max, min etc.
C******************************************************************
C*** Calculate box edges
 5000   nlabels = 1
C*** box extremities for screen dumps
	if(screen) then
	 mnx = max_screen_width
	 mxx = 0
	 mny = max_screen_height
	 mxy = 0
         do i=1,npts
          mnx = min(mnx,ixcomp(i))
          mxx = max(mxx,ixcomp(i))
          mny = min(mny,iycomp(i))
          mxy = max(mxy,iycomp(i))
	 end do
C*** box extremities for map dumps
	else
	 mnx = nxyz(1)
         mxx = 0
         mny = nxyz(2)
         mxy = 0
         do i=1,npts
          mnx = min(mnx,ixp(i))
          mxx = max(mxx,ixp(i))
          mny = min(mny,iyp(i))
          mxy = max(mxy,iyp(i))
	 end do
	 ixp(npts+1) = ixp(1)
         iyp(npts+1) = iyp(1)
	end if
C***
	numx = 2 * (mxx - mnx + 1) / 2
	numy = 2 * (mxy - mny + 1) / 2
	ierr = 0
        return
        end
C*************************************************************************
C***
        subroutine box_out
C***
C*************************************************************************
C***
C*** subroutine to write the output file either from the input
C*** map or dumped directly from the screen
        include    'Ximdisp_common.for'
C***
C*** create output file for boxed image
        call  imopen(idevout,boxfile,'new')
        ixyz(1) = nxpad
        ixyz(2) = nypad
        ixyz(3) = 1
        kxyz(1) = nxpad
        kxyz(2) = nypad
        kxyz(3) = 1
        boxmin = bignum
        boxmax = 0.
        boxmean = 0.
C*** set output mode
	imode = mode
        if(screen) then
	 imode = 0
	else if(floatim .and. mode .eq. 0) then
	 iolabel(nlabels+1) = 
     *   'Select mode : 0 sets background to average perimeter,'//
     *   ' 1 sets background 0'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 menulist(1) = 'Mode 0 (byte)'
	 menulist(2) = 'Mode 1 (integer*2)'
	 menulist(3) = 'Return main menu'
	 call ximagemenuinit(menulist,3)
	 job = -1
 800    call ximagewait(job)
	 if(job.le.0) then
	  go to 800
C*** mode 0
	 else if(job .eq. 1) then
	  imode = 0
C*** mode 1
	 else if(job .eq. 2) then
	  imode = 1
C*** return main menu
	 else if(job .eq. 3) then
	  ierr = 1
	  return
	 end if
	end if
        xorigin = float(ncenx)
        yorigin = float(nceny)
	zorigin = 0.0
        call icrhdr(idevout,ixyz,kxyz,imode,title,0)
        call ialorg(idevout,xorigin,yorigin,zorigin)
        call itrlab(idevout,1)
	call getdate(date,nsecs)
	title = 'Ximdisp boxed off selected area '//date(5:24)
        call  iwrhdr(idevout,title,1,boxmin,boxmax,boxmean)
C***
C*** calculate start & finishing positions
        ixstart = ncenx - numx / 2
        iystart = nceny - numy / 2
	ixend = ixstart + numx + 1
	iyend = iystart + numy + 1
	background = 0.
	backgd = 0.
C*************************************************************
C*** Output file boxed from screen
C*************************************************************
        if(screen) then
C*** convert map coords back to screen
         do i = mny, mxy
	  do j=1,nxpad
	   density(j) = 0.
	  end do
c	  nshift = mnx + mwidth * (mheight - i)
	  nshift = mnx + max_display_width * (max_display_height - i)
          do k = 1, numx
           den = mapbuf(nshift + k)
           density(ixstart+k) = den
	   boxmin = min(boxmin,den)
	   boxmax = max(boxmax,den)
           boxmean = boxmean + den
          end do
	  call iwrlin(idevout,density)
	 end do
	 go to 1000
	end if
C*************************************************************
C*** boxed input from map
C*************************************************************
C*************************************************************
C*** circle
C****************************************************************
	if(circ) then
	 radsq = radius * radius
	 if(floatim) then
	  icount = 0
	  call imposn(idevmap,0,mny)
	  do iy = 1,numy
	   call irdlin(idevmap,aline)
	   fy = radius - float(iy)
	   fysq = fy * fy
	   fxsq = radsq - fysq
	   if(fxsq .gt. 0.) then
	    ishift = nint(sqrt(fxsq))
	   else
	    ishift = 0
	   end if
C*** calculate start, end horiz positions, add 1 as aline starts at 1
	   ixl = mcircx - ishift + 1
	   ixr = mcircx + ishift + 1
C*** use only perimeter for background
	   background = background + aline(ixl) + aline(ixr)
	   icount = icount + 2
	  end do	  
	  background = background / float(icount)
C*** swap zero for background if byte map
	  if(imode .eq. 0) then
	   backgd = background
	   background = 0.
	  end if
C*** pad first section of map
	  if(iystart .gt. 0) then
	   do i=1,nxpad
	    density(i) = backgd
	   end do
	   do i=1,iystart
            call iwrlin(idevout,density)
           end do
	   boxmin = min(boxmin,backgd)
	   boxmax = max(boxmax,backgd)
	   boxmean = boxmean + iystart * nxpad * backgd
	  end if
	 end if
C*** box circle for real
	 call imposn(idevmap,0,mny)
	 do iy = 1,numy
	  do ix = 1,nxpad
	   density(ix) = backgd
	  end do
	  call irdlin(idevmap,aline)
	  fy = radius - float(iy)
	  fysq = fy * fy
	  fxsq = radsq - fysq
	  if(fxsq .gt. 0.) then
	   ishift = nint(sqrt(fxsq))
	  else
	   ishift = 0
	  end if
	  ix1 = mcircx - ishift + 1
	  ix2 = mcircx + ishift + 1
	  nshift = ncenx - ishift - ix1
	  do ix = ix1,ix2
	   den = aline(ix) - background
	   boxmin = min(boxmin,den)
	   boxmax = max(boxmax,den)
	   boxmean = boxmean + den
C*** mod add 1 or else this goes to 0 08.08.97
	   density(nshift+ix+1) = den
	  end do
	  call iwrlin(idevout,density)
	 end do	  
	 go to 1000
C****************************************************************
C*** rectangle
C****************************************************************
	else if(rect) then
	 ix1 = mnx + 1
	 ix2 = mxx + 1
	 if(invert) then
	  call imposn(idevmap,0,0)
	  do iy=1,nypad
	   call irdlin(idevmap,aline)
	   if(iy .gt. mny .and. iy .lt. mxy) then
	    do ix=1,nxpad
	     if(ix .lt. ix1) then
	      den = aline(ix)
	     else if(ix .gt. ix2) then
	      den = aline(ix)
	     else
	      den = 0.
	     end if
	     boxmin = min(boxmin,den)
	     boxmax = max(boxmax,den)
	     boxmean = boxmean + den
	     aline(ix) = den
	    end do
	   end if
	   call iwrlin(idevout,aline)
	  end do
	  go to 1100
	 else
C*** calculate background
	  if(floatim) then
	   call imposn(idevmap,0,mny)
C*** first line of data
	   icount = 0
	   call irdlin(idevmap,aline)
	   do ix=ix1,ix2
	    background = background + aline(ix)
	    icount = icount + 1
	   end do
C*** central section of data
	   do iy=mny+1,mxy-1
	    call irdlin(idevmap,aline)
	    background = background + aline(ix1) + aline(ix2)
	    icount = icount + 2
	   end do
C*** last line of data
	   call irdlin(idevmap,aline)
	   do ix=ix1,ix2
	    background = background + aline(ix)
	    icount = icount + 1
	   end do
	   background = background / float(icount)
C*** swap zero for background if byte map
	   if(imode .eq. 0) then
	    backgd = background
	    background = 0.
	   end if
	  end if
C*** pad first section of map
	  if(iystart .gt. 0) then
	   do i=1,nxpad
	    density(i) = backgd
	   end do
	   do i=1,iystart
            call iwrlin(idevout,density)
           end do
	   boxmin = min(boxmin,backgd)
	   boxmax = max(boxmax,backgd)
	   boxmean = boxmean + iystart * nxpad * backgd
	  end if
C*** cut image for real
	  call imposn(idevmap,0,mny)
	  do ix=1,nxpad
	   density(ix) = backgd
	  end do
	  nshift = ixstart - mnx
	  do iy=1,numy
	   call irdlin(idevmap,aline)
	   do ix = ix1, ix2
	    den = aline(ix) - background
	    boxmin = min(boxmin,den)
	    boxmax = max(boxmax,den)
	    boxmean = boxmean + den
	    density(ix+nshift) = den
	   end do
	   call iwrlin(idevout,density)
	  end do
	 end if
C****************************************************************
C*** polygon
C****************************************************************
	else if(poly) then
	 ixl = mnx + 1
	 ixr = mxx + 1
	 do i=1,npts+1
	  xd(i) = float(ixp(i))
	  yd(i) = float(iyp(i))
	 end do
C*** calculate background for polygon from perimeter
	 if(floatim) then
	  icount = 0
	  background = 0.
  	  do 300 n=1,npts
	   ix1 = xd(n)
	   iy1 = yd(n)
	   ix2 = xd(n+1)
	   iy2 = yd(n+1)
C*** vertical line
	   if(ix1 .eq. ix2) then
	    if(iy1 .lt. iy2) then
	     istep = 1
	    else
	     istep = -1
	    end if
C*** add 1 to aline element as array aline starts at 1
	    do iy=iy1,iy2-istep,istep
	     call imposn(idevmap,0,iy)
	     call irdlin(idevmap,aline)
	     background = background + aline(ix1+1)
	     icount = icount + 1
	    end do
	    go to 300
	   else
	    slope = float(iy2 - iy1) / float(ix2 - ix1)
	   end if
	   c = float(iy1) - slope * float(ix1)
C*** horizontal line
	   if(iy1 .eq. iy2) then
	    call imposn(idevmap,0,iy1)
	    call irdlin(idevmap,aline)
	    if(ix1 .lt. ix2) then
	     istep = 1
	    else 
	     istep = -1
	    end if
	    do ix = ix1,ix2-istep,istep
	     background = background + aline(ix+1)	
	     icount = icount + 1 
	    end do
	    go to 300
	   else if(iy1 .lt. iy2) then
	    istep = 1
	   else
	    istep = -1
	   end if
C*** lines with slopes
 	   do iy=iy1,iy2-istep,istep
	    call imposn(idevmap,0,iy)
	    call irdlin(idevmap,aline)
C*** calculate x position from y = mx + c
	    ix = nint((float(iy) - c) / slope)
	    background = background + aline(ix+1)
	    icount = icount + 1
	   end do
  300     continue
C*** measurements finished, calculate final background density
          background = background / float(icount)
C*** swap zero for background if byte map
	  if(imode .eq. 0) then
	   backgd = background
	   background = 0.
	  end if
C*** pad first section of map
	  if(iystart .gt. 0) then
	   do i=1,nxpad
	    density(i) = backgd
	   end do
	   do i=1,iystart
            call iwrlin(idevout,density)
           end do
	   boxmin = min(boxmin,backgd)
	   boxmax = max(boxmax,backgd)
	   boxmean = boxmean + iystart * nxpad * backgd
	  end if
	 end if
C*** box polygon for real
	 call imposn(idevmap,0,mny)
	 do iy=mny,mxy
	  call irdlin(idevmap,aline)
	  y = float(iy)
	  do ix=1,nxpad
	   density(ix) = backgd
	  end do
	  nshift = ixstart - mnx
	  do ix = ixl,ixr
	   x = float(ix)
C*** if point inside polygon include background
	   if(box_inside(x,y,npts,xd,yd)) then
            den = aline(ix+1) - background
	   else
	    den = backgd
	   end if
	   boxmin = min(boxmin,den)
	   boxmax = max(boxmax,den)
	   boxmean = boxmean + den
	   density(ix+nshift) = den
	  end do
	  call iwrlin(idevout,density)
	 end do
	end if
C*************************************************************
C*** write boxed area to output file
C*************************************************************
 1000   if(nypad .gt. iyend) then
C*** pad output file with blanks
         do i=1,nxpad
          density(i) = backgd
         end do
         do i=iyend,nypad
          call iwrlin(idevout,density)
         end do
	 boxmean = boxmean + (nypad - iyend + 1) * nxpad * backgd
        end if
C*************************************************************
C*** rewrite header
C*************************************************************
 1100	boxmean = boxmean / float(nxpad * nypad)
        call  iwrhdr(idevout,title,-1,boxmin,boxmax,boxmean)
        call  imclose(idevout)
	ierr = 0
	return
        end
C********************************************************************
C***
	logical function box_inside(x,y,nvert,xp,yp)
C***
C********************************************************************
	real*4		xp(*)
	real*4		yp(*)
	logical		box_cross
	box_inside = .false.
	do i=1,nvert
	 if(box_cross(x,y,xp(i),yp(i),xp(i+1),yp(i+1)))
     *   box_inside = .not. box_inside
	end do
	return
	end
C********************************************************************
C***
	logical function box_cross(x,y,x1,y1,x2,y2)
C***
C********************************************************************
	if(((y .lt. y1) .eqv. (y .lt. y2)) .or.
     *      (x .ge. x1  .and. x .ge. x2)) then
	 box_cross = .false.
	else if (x .lt. x1 .and. x .lt. x2) then
	 box_cross = .true.
	else if (x1 .lt. x2) then
	 box_cross = x .lt. (x1 + (y - y1) * (x2 - x1) / (y2 - y1))
	else
	 box_cross = x .lt. (x2 + (y - y2) * (x1 - x2) / (y1 - y2))
	end if
	return
	end
C********************************************************************
C***
	subroutine change_font
C***
C*******************************************************************
	include 'Ximdisp_common.for'
	call ximagemenuhide
	menulist(1) = 'Keep current text font'
	menulist(2) = 'Select new text font'
	menulist(3) = 'Return main menu'
	call ximagemenuinit(menulist,3)
	job = -1
  100	call ximagewait(job)
	if(job .le. 0) then
	 go to 100
C*** keep current font
	else if (job .eq. 1) then
	 call ximagemenuhide
	 return
C*** select new font
	else if (job .eq. 2) then
	 call ximagemenuhide
 	 call ximagecheckfonts(fontlist,nfonts)
	 nfont = 0
	 go to 200
C*** return main menu
	else
	 call ximagemenuhide
	 return
	end if
C*** select text for labelling
  200	item = 0
	ipage = 0
  300   item = item + 1
	if(nfont .le. nfonts .and. item .le. max_items) then
	 nfont = nfont + 1
	 menulist(item)(1:64) = fontlist(nfont)(1:64)
	 go to 300
C*** display font selection menu
	else
	 item = item - 1
	 nfont = min(nfont,nfonts)
	 ipage = ipage + 1
  	 iolabel(nlabels+1) = 'Click in main window to continue list'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagemenuinit(menulist,item)
	 ifont = -1
  400 	 call ximagewait(ifont)
	 if(ifont.le.-1) then
	  go to 400
C*** next page or repeat font list
	 else if (ifont .eq. 0) then
	  if(nfont .eq. nfonts) nfont = 0
	  call ximagemenuhide
	  go to 200
C*** font selected, find out which one.
	 else
	  number = (ipage - 1) * max_items + ifont
	  call ximagemenuhide
	 end if
	end if
	call ximagechangefont(number,ierr)
	if(ierr .ne. 0) then
	 iolabel(nlabels+1) = fontlist(number)
	 iolabel(nlabels+2) = 'Font type not available, type cr to continue'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	 call ximagemenudisplay
	 nfont = 0
	 go to 200
	end if
	return
	end
C********************************************************************
C***
	subroutine check_file(filename)
C***
C********************************************************************
C*** subroutine to check existence of file
	character	filename*(*)
	include 'Ximdisp_common.for'
 100	there = .false.
        inquire(file=filename,exist=there)
        if(old) then
	 if(.not.there) then
          write(iolabel(nlabels+1),
     *    '(''File cannot be found, please retype it :'')') 
	  call ximageioboxdisplay(iolabel,filename,nlabels+1)
          go to 100
	 end if
	else
	 if(there) then
          write(iolabel(nlabels+1),
     *    '(''File already present please re-type :'')') 
	  call ximageioboxdisplay(iolabel,filename,nlabels+1)
          go to 100
	 end if
        end if
	return
	end
C**************************************************************************
C***
         subroutine colour_table
C***
C**************************************************************************
C*** subroutine to generate colour lookup tables or 
C*** read them from previously built tables;also provides 
C*** discrete lookup for colour or black & white
        include    'Ximdisp_common.for'
C*** set variables for general use
	fmaxcolour = float(max_colour)
	frange = float(max_den - min_den)
	colourscale = (fmaxcolour + 1.0) / frange
C*** set background to black
	red(min_den) = min_den
	blue(min_den) = min_den
	green(min_den) = min_den
C*** set first entry to monochrome black/white
	if(first) then
	 call colour_blackwhite
	 first = .false.
	 return
	end if
	call ximagemenuhide
C*** reload original map if phase map in image window
	if(phasemap) then
	 iolabel(nlabels+1) = 'Wait for map to reload...'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call colour_blackwhite
	 call ximagedrawimage
     *   (max_display_width,max_display_height,0,mapbuf,ierr)
	 phasemap = .false.
	 call ximagelabelhide
	 call ximagelabeldisplay(iolabel,nlabels)
	end if
	menulist(1) = 'Black/White'
	menulist(2) = 'White/Black'
	menulist(3) = 'Monochrome'
	menulist(4) = 'Metallic'
	menulist(5) = 'Black/blue/violet/white'
	menulist(6) = 'Magicworld'
	menulist(7) = 'Waterworld'
	menulist(8) = 'Dreamworld'
	menulist(9) = 'Complementary'
	menulist(10) = 'Saturation'
	menulist(11) = 'Read from file'
	if(image) then
	 menulist(12) = 'Return main menu'
	 call ximagemenuinit(menulist,12)
	else
	 menulist(12) = 'Phaseworld'
	 menulist(13) = 'Return main menu'
	 call ximagemenuinit(menulist,13)
	end if
	job = -1
  100   call ximagewait(job)
	if(job.le.0) then
	 go to 100
C*******************************************************************
C*** Black/white
C*******************************************************************
	else if(job .eq. 1) then
	 call ximagemenuhide
	 call colour_contrast
	 return
C*******************************************************************
C*** White/Black
C*******************************************************************
	else if(job .eq. 2) then
	 red(min_den) = max_colour
	 green(min_den) = max_colour
	 blue(min_den) = max_colour
  	 do i=min_den+1,max_den
	  irgb = 
     *    min(max_colour,nint(colourscale * float(abs(i - max_den))))
	  red(i) = irgb
	  green(i) = irgb
	  blue(i) = irgb
	 end do
	 call ximagesetcolourtable(min_den,max_den,red,green,blue)
C*******************************************************************
C*** Monochrome
C*******************************************************************
	else if(job .eq. 3) then
	 call ximagemenuhide
	 cutoff1 = 100.0 / 3.0
	 cutoff2 = 2.0 * cutoff1
	 factor = 1.0 / cutoff1
	 menulist(1) = 'Red/green/blue'
	 menulist(2) = 'Yellow/cyan/magenta'
	 menulist(3) = 'Return main menu'
	 call ximagemenuinit(menulist,3)
	 iopt = -1
 120	 call ximagewait(iopt)
	 if(iopt .le. 0) then
	  go to 120
C*** red/green/blue
	 else if(iopt .eq. 1) then
	  do i=min_den+1,max_den-2
	   floati = float(i)
	   red(i) = colourscale * floati
	   green(i) = 0
	   blue(i) = 0
	   primary = .true.
	  end do
C*** yellow/cyan/magenta
	 else if(iopt .eq. 2) then
	  do i=min_den+1,max_den-2
	   floati = float(i)
	   red(i) = colourscale * floati
	   green(i) = colourscale * floati
	   blue(i) = 0
	   primary = .false.
	  end do
C*** return main menu
	 else
	  call ximagemenuhide
	  call ximagelabelhide
	  return
	 end if
C*** set initial colour table
	 call ximagemenuhide
	 call ximagesetcolourtable(min_den,max_den,red,green,blue)
C*** set up slider bars for controlling background and brightness
	 iolabel(nlabels+1) = 
     *   'Use left/right sliders to set background/colour'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 percent1 = 0.0
	 percent2 = 0.0
	 call ximagesliderinit(2,0.0,0.0)
	 call ximagecolourbarinit
	 iopt = -1
  150    call ximagewait(iopt)
	 if(iopt .lt. 0) then
	  go to 150
C*** return main menu
	 else if(iopt .eq.0) then
	  call ximagelabelhide
	  call ximagecolourbarhide
	  call ximagesliderhide
	  return
C*** reset background
	 else if(iopt .eq. 101) then
	  call ximagesliderread(percent1)
	  irgb = min(max_colour,nint(fmaxcolour * percent1 * 0.01))
	  red(min_den) = irgb
	  green(min_den) = irgb
	  blue(min_den) = irgb
	  call ximagesetcolourtable(min_den,max_den,red,green,blue)
	  go to 150
C*** reset colour
	 else if(iopt .eq. 102) then
	  call ximagesliderread(percent2)
	  if(primary) then
	   if(percent2 .lt. cutoff1) then
	    do i=min_den+1,max_den-2
	     scfac = float(i) * colourscale * factor
	     red(i) = (cutoff1 - percent2) * scfac
	     green(i) = percent2 * scfac
	     blue(i) = 0
	    end do
	   else if(percent2 .lt. cutoff2) then
	    do i=min_den+1,max_den-2
	     scfac = float(i) * colourscale * factor
	     red(i) = 0
	     green(i) = (cutoff2 - percent2) * scfac
	     blue(i) = (percent2 - cutoff1) * scfac
	    end do
	   else
	    do i=min_den+1,max_den-2
	     scfac = float(i) * colourscale * factor
	     red(i) = (percent2 - cutoff2) * scfac
	     green(i) = 0
	     blue(i) = (100. - percent2) * scfac
	    end do
	   end if
C*** secondary colours
	  else
	   if(percent2 .lt. cutoff1) then
	    do i=min_den+1,max_den-2
	     fac = float(i) * colourscale
	     scfac = fac * factor
	     red(i) = (cutoff1 - percent2) * scfac
	     green(i) = fac
	     blue(i) = percent2 * scfac
	    end do
	   else if(percent2 .lt. cutoff2) then
	    do i=min_den+1,max_den-2
	     fac = float(i) * colourscale
	     scfac = fac * factor
	     red(i) = (percent2 - cutoff1) * scfac
	     green(i) = (cutoff2 - percent2) * scfac
	     blue(i) = fac
	    end do
	   else
	    do i=min_den+1,max_den-2
	     fac = float(i) * colourscale
	     scfac = fac * factor
	     red(i) = fac
	     green(i) = (percent2 - cutoff2) * scfac
	     blue(i) = (100. - percent2) * scfac
	    end do
	   end if
	  end if
	  call ximagesetcolourtable(min_den,max_den,red,green,blue)
	  go to 150
	 end if
C*******************************************************************
C*** Metallic
C*******************************************************************
	else if(job .eq. 4) then
	 call ximagemenuhide
	 menulist(1) = 'Gold'
	 menulist(2) = 'Silver'
	 menulist(3) = 'Bronze'
	 menulist(4) = 'Return main menu'
	 call ximagemenuinit(menulist,4)
	 jopt = -1
  170    call ximagewait(jopt)
	 if(jopt .le. 0) then
	  go to 170
C*** gold
	 else if(jopt .eq. 1) then
	  rfactor = 1.0
	  gfactor = 0.7
	  bfactor = 0.1
C*** silver
	 else if(jopt .eq. 2) then
	  rfactor = 0.6
	  gfactor = 0.6
	  bfactor = 0.7
C*** bronze
	 else if(jopt .eq. 3) then
	  rfactor = 0.8
	  gfactor = 0.4
	  bfactor = 0.2
C*** return main menu
	 else if(jopt .eq. 4) then
	  call ximagemenuhide
	  return
	 end if
	 call ximagemenuhide
C*** set scale factor to make exp(1.0) to give maximum brightness
C*** to the top 5 densities in the green gun for gold
	 scfactor = fmaxcolour / (0.7 * exp(1.0) * colourscale * 122.)
	 power = 2.0
	 do i=min_den+1,max_den - 2
	  brightness = exp((float(i) * 0.01)**power) * scfactor
	  total = brightness * colourscale * float(i)
	  red(i) = min(max_colour,nint(total * rfactor))
	  green(i) = min(max_colour,nint(total * gfactor))
	  blue(i) = min(max_colour,nint(total * bfactor))
	 end do
	 do i=max_den-1, max_den
	  red(i) = max_colour
	  green(i) = max_colour
	  blue(i) = max_colour
	 end do
	 call ximagesetcolourtable(min_den,max_den,red,green,blue)
C*** set up slider bar for controlling background
	 iolabel(nlabels+1) = 
     *   'Use left/right sliders to set background/brightness'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 percent1 = 0.0
	 percent2 = 50.0
	 call ximagesliderinit(2,0.0,0.5)
	 call ximagecolourbarinit
	 iopt = -1
  180    call ximagewait(iopt)
	 if(iopt .lt. 0) then
	  go to 180
C*** return main menu
	 else if(iopt .eq.0) then
	  call ximagelabelhide
	  call ximagecolourbarhide
	  call ximagesliderhide
	  return
C*** reset background
	 else if(iopt .eq. 101) then
	  call ximagesliderread(percent1)
	  irgb = nint(fmaxcolour * percent1 * 0.01)
	  red(min_den) = irgb
	  green(min_den) = irgb
	  blue(min_den) = irgb
	  call ximagesetcolourtable(min_den,max_den,red,green,blue)
	  go to 180
C*** reset background
	 else if(iopt .eq. 102) then
	  call ximagesliderread(percent2)
	  power = percent2 * 0.04
	  do i=min_den+1,max_den - 2
	   brightness = exp((float(i) * 0.01)**power) * scfactor
	   total = brightness * colourscale * float(i)
	   red(i) = min(max_colour,nint(total * rfactor))
	   green(i) = min(max_colour,nint(total * gfactor))
	   blue(i) = min(max_colour,nint(total * bfactor))
	  end do
	  call ximagesetcolourtable(min_den,max_den,red,green,blue)
	  go to 180
	 end if
C*******************************************************************
C*** Black/blue/violet/white
C*******************************************************************
	else if(job .eq. 5) then
	 blocks = 3.
	 nentries = nint(frange / blocks)
	 istart = min_den + 1
	 iend = istart + nentries - 1
C*** blue
	 do i=istart,iend
	  blue(i) = nint(colourscale * float(i - istart + 1))
	  red(i) = min_den
	  green(i) = min_den
	 end do
	 istart = iend + 1
	 iend = istart + nentries - 1
C*** violet
	 do i=istart,iend
	  blue(i) = max_colour
	  red(i) = nint(colourscale * float(i))
	  green(i) = min_den
	 end do
	 istart = iend + 1
	 iend = istart + nentries - 1
C*** white
	 do i=istart,iend
	  blue(i) = max_colour
	  red(i) = max_colour
	  green(i) = nint(colourscale * float(i))
	 end do
	 do i=iend,max_den
	  red(i) = max_colour
	  blue(i) = max_colour
	  green(i) = max_colour
	 end do
	 call ximagesetcolourtable(min_den,max_den,red,green,blue)
C*******************************************************************
C*** magicworld
C*******************************************************************
	else if(job .eq. 6) then
	 call ximagemenuhide
	 nblue = 25
	 nmagenta = 25
	 nred = 25
	 norange = 25
	 nyellow = 26	 
	 call colour_magicworld
     *   (nblue,nmagenta,nred,norange,nyellow,red,green,blue)
C*** set up slider bars for controlling background and brightness
	 iolabel(nlabels+1) = 
     *   'Use left and right sliders to set background and brightness'
	 call ximagelabeldisplay(iolabel,nlabels+1)
C*** set up scale factors, based on exp(x/2.5) and exp(x/-1.8)/3.784
	 scfactor1 = 0.01 / 2.5
	 scfactor2 = -0.01 / 1.8
	 div2 = 3.784
	 percent1 = 0.0
	 percent2 = 50.0
	 call ximagesliderinit(2,0.0,0.5)
	 call ximagecolourbarinit
	 iopt = -1
  350    call ximagewait(iopt)
	 if(iopt .lt. 0) then
	  go to 350
C*** return main menu
	 else if(iopt .eq.0) then
	  call ximagelabelhide
	  call ximagecolourbarhide
	  call ximagesliderhide
	  return
C*** reset background
	 else if(iopt .eq. 101) then
	  call ximagesliderread(percent1)
	  irgb = nint(fmaxcolour * percent1 * 0.01)
	  red(min_den) = irgb
	  green(min_den) = irgb
	  blue(min_den) = irgb
	  call ximagesetcolourtable(min_den,max_den,red,green,blue)
	  go to 350
C*** reset brightness
	 else if(iopt .eq. 102) then
	  call ximagesliderread(percent2)
	  reverse = 100. - percent2
	  nyellow = 
     *    max(1,nint(frange * (exp(percent2 * scfactor1) - 1.0)))
	  nblue = 
     *    max(1,nint(frange * (exp(reverse * scfactor1) - 1.0)))
	  norange = 
     *    max(1,nint(frange * (exp(reverse * scfactor2)/div2)))
 	  nmagenta = 
     *    max(1,nint(frange * (exp(percent2 * scfactor2)/div2)))
C*** magenta and yellow split, make sure they are even
	  nyellow = 2 * nyellow / 2
	  nmagenta = 2 * nmagenta / 2
	  nred = max_range - (nblue + nyellow + nmagenta + norange + 1)
	  call colour_magicworld
     *    (nblue,nmagenta,nred,norange,nyellow,red,green,blue)
	  go to 350
	 end if
C*******************************************************************
C*** Waterworld
C*******************************************************************
	else if(job .eq. 7) then
	 call ximagemenuhide
	 nmagenta = 42
	 nblue = 42
	 ncyan = 43
	 call colour_waterworld
     *   (nmagenta,nblue,ncyan,red,green,blue)
C*** set up slider bars for controlling background and brightness
	 iolabel(nlabels+1) = 
     *   'Use left and right sliders to set background and brightness'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 percent1 = 0.0
	 percent2 = 50.0
	 call ximagesliderinit(2,0.0,0.5)
	 call ximagecolourbarinit
	 iopt = -1
  380    call ximagewait(iopt)
	 if(iopt .lt. 0) then
	  go to 380
C*** return main menu
	 else if(iopt .eq.0) then
	  call ximagelabelhide
	  call ximagecolourbarhide
	  call ximagesliderhide
	  return
C*** reset background
	 else if(iopt .eq. 101) then
	  call ximagesliderread(percent1)
	  irgb = nint(fmaxcolour * percent1 * 0.01)
	  red(min_den) = irgb
	  green(min_den) = irgb
	  blue(min_den) = irgb
	  call ximagesetcolourtable(min_den,max_den,red,green,blue)
	  go to 380
C*** reset brightness
	 else if(iopt .eq. 102) then
	  call ximagesliderread(percent2)
	  proportion = percent2 * 0.01
	  ncyan = 
     *    nint((proportion * .33333 + .166667) * frange)
 	  nmagenta = 
     *    nint((proportion * -0.33333 + 0.5) * frange)
	  nblue = max_range - (nmagenta + ncyan)
	  call colour_waterworld
     *    (nmagenta,nblue,ncyan,red,green,blue)
	  go to 380
	 end if
C*******************************************************************
C*** Dreamworld
C*******************************************************************
	else if(job .eq. 8) then
	 call ximagemenuhide
C*** set up slider bars for controlling background and brightness
	 iolabel(nlabels+1) = 
     *   'Use left / right sliders to set background / number of cycles'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 cyc = 0.01 * 8.0 / frange
	 call colour_dreamworld(cyc,frange,red,green,blue)
	 percent1 = 0.0
	 percent2 = 50.0
	 call ximagesliderinit(2,0.,0.)
	 call ximagecolourbarinit
	 iopt = -1
  500    call ximagewait(iopt)
	 if(iopt.lt.0) then
	  go to 500
C*** return main menu
	 else if(iopt.eq.0) then
	  call ximagecolourbarhide
	  call ximagesliderhide
	  return
C*** reset background
	 else if(iopt .eq. 101) then
	  call ximagesliderread(percent1)
	  irgb = nint(fmaxcolour * percent1 * 0.01)
	  red(min_den) = irgb
	  green(min_den) = irgb
	  blue(min_den) = irgb
	  call ximagesetcolourtable(min_den,max_den,red,green,blue)
	  go to 500
C*** change colour table
	 else if(iopt .eq. 102) then
	  call ximagesliderread(percent)
	  cyc = percent * 0.01 * 8.0 / frange 
	  call colour_dreamworld(cyc,frange,red,green,blue)
	 end if
         go to 500
C*******************************************************************
C*** Complementary
C*******************************************************************
	else if(job .eq. 9) then
	 call ximagemenuhide
	 menulist(1) = 'Red/green'
	 menulist(2) = 'Blue/orange'
	 menulist(3) = 'Return main menu'
	 call ximagemenuinit(menulist,3)
	 iopt = -1
  600    call ximagewait(iopt)
	 if(iopt.le.0) then
	  go to 600
C*** red/green
	 else if(iopt.eq.1) then
	  redgrn = .true.
C*** blue/orange
	 else if(iopt.eq.2) then
	  redgrn = .false.
	 else if(iopt.eq.3) then
	  call ximagemenuhide
	  return
	 end if
C*** end of menu, set default threshold
	 call ximagemenuhide
	 iolabel(nlabels+1) = 
     *   'Move slider to increase background threshold'
	 call ximagelabeldisplay(iolabel,nlabels+1)
C*** set starting threshold
	 icut = nint(frange * 0.5)
         if(redgrn) then
C*** red/green
          do i=min_den+1,icut
           red(i) = min_den
           green(i) = nint(float(i) * colourscale)
           blue(i) = min_den
          end do
          do i=icut,max_den
           red(i) = nint(float(i) * colourscale)
	   green(i) = min_den
	   blue(i) = min_den
          end do
C*** blue/orange
	 else
C*** set up background
          do i=min_den+1,icut
           red(i) = min_den
           green(i) = min_den
           blue(i) = nint(float(i) * colourscale)
          end do
C*** above threshold
          do i=icut,max_den
           blue(i) = nint(float(i) * colourscale / 8.)
           red(i) = nint(float(i) * colourscale)
           green(i) = 
     *     nint(float(i) * colourscale / float(max_den - i + 1))
          end do
	 end if
         call ximagesetcolourtable(min_den,max_den,red,green,blue)
	 call ximagesliderinit(1,0.5,0.)
	 call ximagecolourbarinit
650	 iopt = -1
         call ximagewait(iopt)
	 if(iopt.lt.0) then
	  go to 650
C*** return main menu
	 else if(iopt.eq.0) then
	  call ximagecolourbarhide
	  call ximagesliderhide
	  return
C*** change colour table
	 else if(iopt .eq. 101) then
	  call ximagesliderread(percent)
	  icut = nint(frange * percent * 0.01)
C************************
C*** red/green
C************************
          if(redgrn) then
C*** set up background
           do i=min_den+1,icut
            red(i) = min_den
            green(i) = nint(float(i) * colourscale)
            blue(i) = min_den
           end do
C*** above threshold
	   if(icut .lt. max_den) then
            do i=icut,max_den
             red(i) = nint(float(i) * colourscale)
	     green(i) = min_den
	     blue(i) = min_den
            end do
	   end if
C************************
C*** blue/orange
C************************
	  else
C*** set up background
           do i=min_den+1,icut
            red(i) = min_den
            green(i) = min_den
            blue(i) = nint(float(i) * colourscale)
           end do
C*** above threshold
	   if(icut .lt. max_den) then
            do i=icut,max_den
             blue(i) = nint(float(i) * colourscale / 8.)
             red(i) = nint(float(i) * colourscale)
             green(i) = 
     *       nint(float(i) * colourscale / float(max_den - i + 1))
            end do
           end if
	  end if
          call ximagesetcolourtable(min_den,max_den,red,green,blue)
	 end if
         go to 650
C*******************************************************************
C*** Saturation
C*******************************************************************
	else if(job .eq. 10) then
	 call ximagemenuhide
	 menulist(1) = 'Background black/white'
	 menulist(2) = 'Background white/black'
	 menulist(3) = 'Return main menu'
	 call ximagemenuinit(menulist,3)
	 iopt = -1
  700    call ximagewait(iopt)
	 if(iopt.le.0) then
	  go to 700
C*** black/white
	 else if(iopt.eq.1) then
	  blackwhite = .true.
C*** white/black
	 else if(iopt.eq.2) then
	  blackwhite = .false.
	 else if(iopt.eq.3) then
	  call ximagemenuhide
	  return
	 end if
C***  set default threshold
	 call ximagemenuhide
	 icut = 125
	 if(blackwhite) then
          do i=min_den+1,icut
	   irgb = nint(float(i) * colourscale)
	   red(i) = irgb
	   green(i) = irgb
	   blue(i) = irgb
          end do
	 else
	  do i=min_den+1,icut
	   irgb = nint(colourscale * float(max_den - i - 1))
	   red(i) = irgb
	   green(i) = irgb
	   blue(i) = irgb
	  end do
	 end if
C*** above threshold
         do i=icut,max_den
	  rgb = min(max_colour,nint(colourscale * frange))
          red(i) = nint(rgb)
          blue(i) = 0
          green(i) = nint(rgb * 165./255.)
         end do
         call ximagesetcolourtable(min_den,max_den,red,green,blue)
	 iolabel(nlabels+1) = 'Use slider bar to set saturation'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagesliderinit(1,0.95,0.)
	 call ximagecolourbarinit
750	 iopt = -1
         call ximagewait(iopt)
	 if(iopt.lt.0) then
	  go to 750
C*** return main menu
	 else if(iopt.eq.0) then
	  call ximagecolourbarhide
	  call ximagesliderhide
	  return
C*** change colour table
	 else if(iopt .eq. 101) then
	  call ximagesliderread(percent)
	  icut = nint(max_den * percent * 0.01)
C*** set up background
	  if(blackwhite) then
           do i=min_den+1,icut
	    irgb = nint(float(i) * colourscale)
	    red(i) = irgb
	    green(i) = irgb
	    blue(i) = irgb
           end do
	  else
	   do i=min_den+1,icut
	    irgb = nint(colourscale * float(max_den - i - 1))
	    red(i) = irgb
	    green(i) = irgb
	    blue(i) = irgb
	   end do
	  end if
C*** above threshold
	  if(icut .lt. max_den) then
           do i=icut,max_den
	     rgb = min(max_colour,nint(colourscale * frange))
             red(i) = nint(rgb)
             blue(i) = 0
             green(i) = nint(rgb * 165./255.)
           end do
	  end if
          call ximagesetcolourtable(min_den,max_den,red,green,blue)
	 end if
	 go to 750
C*******************************************************************
C*** read colour table from file
C*******************************************************************
	else if(job .eq. 11) then
	 call ximagemenuhide
	 iolabel(nlabels+1) = 'Colour table file name ...'
	 colour_file = ' '
	 call ximageioboxdisplay(iolabel,colour_file,nlabels+1)
	 if(colour_file .eq. ' ') then
	  call ximagemenudisplay
	  go to 100
	 end if
	 old = .true.
	 call check_file(colour_file)
	 open(unit=idevcol,file=colour_file,status='old')
	 do i=min_den+1,max_den
	  read(idevcol,*,err=800) red(i), green(i), blue(i)
	 end do
	 close (idevcol)
  800    call ximagesetcolourtable(min_den,max_den,red,green,blue)
C*******************************************************************
C*** phase colour map/return if image
C*******************************************************************
	else if(job .eq. 12) then
	 call ximagecolourbarhide
	 call ximagesliderhide
	 call ximagemenuhide
	 if(image) return
	 iolabel(nlabels+1) = 'Wait for map reload...'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 phasemap = .true.
	 call colour_phase
C*** load phase map
	 call ximagedrawimage
     *   (max_screen_width,max_screen_height,0,phasebuf,ierr)
	 call ximagelabelhide
	 iolabel(nlabels+1) = 
     *   'Phase angle colour representation :'
	 iolabel(nlabels+2) = 'Red     :   0 degrees'
	 iolabel(nlabels+3) = 'Orange  :  45 degrees'
	 iolabel(nlabels+4) = 'Yellow  :  90 degrees'
	 iolabel(nlabels+5) = 'Green   : 135 degrees'
	 iolabel(nlabels+6) = 'Cyan    : 180 degrees'
	 iolabel(nlabels+7) = 'Blue    : 225 degrees'
	 iolabel(nlabels+8) = 'Violet  : 270 degrees'
	 iolabel(nlabels+9) = 'Magenta : 315 degrees'
	 iolabel(nlabels+10) = 'Press <cr> to continue'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+10)
C*******************************************************************
C*** return main menu
C*******************************************************************
	else if(job .eq. 13) then
	 call ximagecolourbarhide
	 call ximagesliderhide
	end if
	call ximagemenuhide
	return
	end
C******************************************************************
C***
	subroutine colour_contrast
C***
C******************************************************************
C*** subroutine to put up slider bar to adjust contrast for b/w
C***
	include 'Ximdisp_common.for'
	red(min_den) = min_den
	blue(min_den) = min_den
	green(min_den) = min_den
	do i=min_den+1,max_den
	 irgb = min(max_colour,nint(colourscale * float(i)))
	 red(i) = irgb
	 green(i) = irgb
	 blue(i) = irgb
	end do
	call ximagesetcolourtable(min_den,max_den,red,green,blue)
C*** set up slider bar for resetting limits
	write(iolabel(nlabels+1),'(''min, max, mean ='',3f8.1)') 
     *  amin, amax, dmean
	iolabel(nlabels+2) = 
     *  'Use left and right sliders to set lower and upper thresholds'
	call ximagelabeldisplay(iolabel,nlabels+2)
	arange = amax - amin
	min_thresh = min_den + 1
	max_thresh = max_den
	bmin = max(amin, amin + arange * float(min_thresh) / grey)
	bmax = min(amax, amin + arange * float(max_thresh) / grey)
	percent1 = 0.0
	percent2 = 100.0
	call ximagesliderinit(2,0.0,0.95)
	call ximagecolourbarinit
	iopt = -1
  200   call ximagewait(iopt)
	if(iopt .lt. 0) then
	 go to 200
C*** stop reading slider, return main menu
	else if(iopt .eq. 0) then
	 call ximagelabelhide
         call ximagecolourbarhide
         call ximagesliderhide
	 if(outputcoords) return
	 write(iolabel(nlabels+1),'(''min, max ='',2f10.0)') bmin, bmax
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 menulist(1) = 'Re-scale to these thresholds'
	 menulist(2) = 'Return main menu'
	 call ximagemenuinit(menulist,2)
         jopt = -1
  300    call ximagewait(jopt)
	 if(jopt .lt. 0) then
	  go to 300
C*** rescale
	 else if(jopt .eq. 1) then
	  call ximagemenuhide
	  call ximagelabelhide
	  amin = bmin
	  amax = bmax
	  rescale = .true.
	  return
C*** return main menu
         else if(jopt .eq. 2) then
	  return
	 end if
C*** reset lower threshold
        else if(iopt .eq. 101) then
	 call ximagesliderread(percent1)
	 min_thresh = max(min_den+1,
     *   min(nint(percent1 * grey * 0.01),max_thresh - 1))
	 scale = fmaxcolour / float(max_thresh - min_thresh + 1)
C*** set to min_den up to threshold
	 do i=min_den+1,max(min_den+1,min_thresh - 1)
	  red(i) = min_den
	  green(i) = min_den
	  blue(i) = min_den
	 end do
C*** set from min to max threshold
         do i=min_thresh,max_thresh
	  icolour = nint(scale * float(i - min_thresh))
	  red(i) = icolour
	  green(i) = icolour
	  blue(i) = icolour
	 end do
	 bmin = max(amin, amin + arange * float(min_thresh) / grey)
	 call ximagesetcolourtable(min_den,max_den,red,green,blue)
C*** reset upper threshold
	else if(iopt .eq. 102) then
	 call ximagesliderread(percent2)
	 max_thresh = max(nint(percent2 * grey * 0.01),min_thresh + 1)
	 scale = fmaxcolour / float(max_thresh - min_thresh + 1)
C*** set to from min_thresh to max_thresh
        do i=min_thresh,max_thresh
	  icolour = nint(scale * float(i - min_thresh))
	  red(i) = icolour
	  green(i) = icolour
	  blue(i) = icolour
	 end do
C*** set above upper threshold to max
	 do i=min(max_den,max_thresh+1),max_den
	  red(i) = max_colour
	  green(i) = max_colour
	  blue(i) = max_colour
	 end do
	 bmax = min(amax, amin + arange * float(max_thresh) / grey)
	 call ximagesetcolourtable(min_den,max_den,red,green,blue)
	end if
	go to 200
	return
	end
C******************************************************************
C***
	subroutine colour_blackwhite
C***
C******************************************************************
C*** subroutine to set up black/white colour table
        include    'Ximdisp_common.for'
	fmaxcolour = float(max_colour)
	frange = float(max_den - min_den)
	colourscale = (fmaxcolour + 1.0) / frange
	do i=min_den+1,max_den
	 irgb = min(max_colour,nint(colourscale * float(i)))
	 red(i) = irgb
	 green(i) = irgb
	 blue(i) = irgb
	end do
	call ximagesetcolourtable(min_den,max_den,red,green,blue)
	return
	end
C******************************************************************
C***
	subroutine colour_dreamworld(cyc,frange,red,green,blue)
C***
C******************************************************************
C*** subroutine to set up dreamworld colour table
        include    'Ximdisp_common.for'
        rcor = -0.35
        gcor = 0.25
        bcor = 0.35
        do i=min_den+1,max_den
         floati = float(i)
         relint = floati*(frange - floati) / 16384.
	 cfi = cyc * floati
	 fscale = floati / frange
	 rfactor = relint * colour_compon(cfi + 0.6666666667)
     *           + fscale
         red(i)   = colour_icurve(rfactor, rcor)
	 gfactor = relint * colour_compon(cfi + 0.0000000000)
     *           + fscale
         green(i) = colour_icurve(gfactor, gcor)
	 bfactor = relint * colour_compon(cfi + 0.3333333333)
     *           + fscale
         blue(i)  = colour_icurve(bfactor, bcor)
        end do
        call ximagesetcolourtable(min_den,max_den,red,green,blue)
	return
	end
C**************************************************************************
C***
        function colour_icurve(bright,correc)
C***
C**************************************************************************
	include 'Ximdisp_common.for'
        colour_icurve = float(max_colour) * bright * 
     *                  (1.+correc - correc*bright)
        colour_icurve = min(float(max_colour), colour_icurve)
        colour_icurve = max(float(min_den), colour_icurve)
        return
        end
C**************************************************************************
C***
        function colour_compon(colour)
C***
C**************************************************************************
        colour = 6.283185307 * colour
C*** 1.28 about best as judged by eye
        colour = 1.28 * sin(colour)
        colpls = exp(colour)
        colmns = exp(-colour)
C*** no. is 1/tanh(1.28)
        colour_compon = 1.16756*(colpls-colmns)/(colpls+colmns)
        return
        end
C********************************************************************
C***
	subroutine colour_magicworld
     *  (nblue,nmagenta,nred,norange,nyellow,red,green,blue)
C***
C********************************************************************
C*** subroutine to set up magicworld colour table
C*** this routine has 5 colour blocks - blue, magenta, red, orange 
C*** and yellow whose components are under the control of the
C*** slider bar. The component of each gun varies from band to band,
C*** colour being added and subtracted gradually in the case of the
C*** two and three gun colours. To make the total (c) of the guns,
C*** from the two (a and b), a + b = c. From here, b = c / (a/b+1)
C*** and from 3 guns, a + a + b = c, so b = c / (2a/b + 1).
C*** The brightness is controlled by an exponential- from -4.0 in 
C*** darkest blue to 0.0 at the top of the red band.
C*** the second exponential goes from 1.0 to 3.0 from the bottom of 
C*** the orange to the top of the yellow band.
        include    'Ximdisp_common.for'
C***
	scale = float(max_colour) / float(max_den - min_den)
C*** set blue gun only for first block
	istart = min_den + 1
	iend = istart + nblue - 1
C*** start first exponential brightness addition e**-4.0 - e**0.0
	range = float(nblue + nmagenta + nred)
	step = 4.0 / range
	power = -4.0
	do i=istart,iend
	 brightness = exp(power)
	 power = power + step
	 total = brightness * scale * float(i)
	 red(i) = 0
	 green(i) = 0
	 blue(i) = nint(total)
	end do
C*** first half of magenta band - increasing blue + red
	istart = iend + 1
	iend = istart + nmagenta / 2 - 1
	scaleup = scale * float(iend) / float(iend - istart)
	do i=istart,iend
	 brightness = exp(power)
	 power = power + step
	 a = scaleup * float(i - istart + 1)
	 b = scale * float(i)
	 total = brightness * b
	 b = total / (a/b + 1.)
	 a = total - b
	 red(i) = nint(a)
	 green(i) = 0
	 blue(i) = nint(b)
	end do
C*** second half of magenta band - decreasing blue + increasing red
	istart = iend + 1
	iend = istart + nmagenta / 2 - 1
	scaledown = nint(scale * float(istart) / float(iend - istart))
	do i=istart,iend
	 brightness = exp(power)
	 power = power + step
	 a = scale * float(i)
	 b = scaledown * float(iend - i + 1)
	 total = brightness * a
	 b = total / (a/b + 1.)
	 a = total - b
	 red(i) = nint(a)
	 green(i) = 0
	 blue(i) = nint(b)
	end do
C*** red band
	istart = iend + 1
	iend = istart + nred - 1
	do i=istart,iend
	 brightness = exp(power)
	 power = power + step
	 total = brightness * scale * float(i)
	 red(i) = nint(total)
	 green(i) = 0
	 blue(i) = 0
	end do
C*** orange band	 
	istart = iend + 1
	iend = istart + norange - 1
	scaleup = scale * float(iend) / float(iend - istart)
C*** start second exponential brightness addition e**0 - e**1.1
	range = float(max_den - istart + 1)
	step = 1.1 / range
	power = 0.0
	do i=istart,iend
	 brightness = exp(power)
	 power = power + step
	 a = scale * float(i)
	 b = scaleup * float(i - istart + 1)
	 total = a * brightness
	 b = total / (a/b + 1.)
	 a = total - b
	 red(i) = min(max_colour,nint(a))
	 green(i) = min(max_colour,nint(b))
	 blue(i) = 0
	end do
C*** yellow band - first half
	istart = iend + 1
	iend = istart + nyellow / 2 - 1
	do i=istart,iend
	 brightness = exp(power)
	 power = power + step
	 total = brightness * scale * float(i)
	 red(i) = min(max_colour,nint(0.5 * total))
	 green(i) = min(max_colour,nint(0.5 * total))
	 blue(i) = 0
	end do
C*** yellow band - second half
	istart = iend + 1
	if(istart .ge. max_den) then
	 red(max_den) = max_colour
	 green(max_den) = max_colour
	 blue(max_den) = max_colour
	else
	 iend = max_den
	 scaleup = scale * float(iend) / float(iend - istart)
	 do i=istart,iend
	  brightness = exp(power)
	  power = power + step
	  a = scale * float(i)
	  b = scaleup * float(i - istart + 1)
	  total = brightness * a
	  b = total / (2. * a / b + 1.)
	  a = total - b
	  red(i) = min(max_colour,nint(a))
	  green(i) = min(max_colour,nint(a))
	  blue(i) = min(max_colour,nint(b))
	 end do
	end if
	call ximagesetcolourtable(min_den,max_den,red,green,blue)
	return
	end
C********************************************************************
C***
	subroutine colour_phase
C***
C********************************************************************
C*** subroutine to set up special colour phase map
        include    'Ximdisp_common.for'
C*** turn off pointer tracking if screen and display sizes do not match
c	if(max_display_width .ne. max_screen_width .or.
c     *      max_display_height .ne. max_screen_height) then
c	 if(pointer) then
c	  call ximagepointertrackoff
c	  pointer = .false.
c	 end if
c	end if
	nentries = (max_den - min_den) / nphasecolours
	do iamp = 0,nentries
	 do iphase = 0,7
	  i = iphase + 8 * iamp
	  red(i) = (iphasecolours(1,iphase)/nentries) * iamp
	  green(i) = (iphasecolours(2,iphase)/nentries) * iamp
	  blue(i) = (iphasecolours(3,iphase)/nentries) * iamp
	 end do
	end do
	call ximagesetcolourtable(min_den,max_den,red,green,blue)
	return
	end
C********************************************************************
C***
	 subroutine colour_waterworld
     *   (nmagenta,nblue,ncyan,red,green,blue)
C***
C********************************************************************
C*** subroutine to set up waterworld colour table
        include    'Ximdisp_common.for'
	scale = float(max_colour) / float(max_den - min_den)
C*** magenta - equal red / blue
	istart = min_den + 1
	iend = istart + nmagenta - 1
	range = float(max_den - min_den + 1)
	step = 2.0 / range
	power = -1.0
	do i=istart,iend
	 brightness = exp(power)
	 power = power + step
	 total = brightness * scale * float(i)
	 red(i) = min(max_colour,nint(0.5 * total))
	 green(i) = 0
	 blue(i) = min(max_colour,nint(0.5 * total))
	end do
C*** first half of blue band - scale down the red
	istart = iend + 1
	iend = istart + nblue / 2 - 1
	scaledown = nint(scale * float(istart) / float(iend - istart))
	do i=istart,iend
	 brightness = exp(power)
	 power = power + step
	 a = scale * float(i)
	 b = scaledown * float(iend - i + 1)
	 total = brightness * a
	 b = total / (a/b + 1.)
	 a = total - b
	 red(i) = min(max_colour,nint(b))
	 green(i) = 0
	 blue(i) = min(max_colour,nint(a))
	end do
C*** second half of blue band - scale up the green
	istart = iend + 1
	iend = istart + nblue / 2 - 1
	scaleup = scale * float(iend) / float(iend - istart)
	do i=istart,iend
	 brightness = exp(power)
	 power = power + step
	 a = scaleup * float(i - istart + 1)
	 b = scale * float(i)
	 total = brightness * b
	 b = total / (a/b + 1.)
	 a = total - b
	 red(i) = 0
	 green(i) = min(max_colour,nint(a))
	 blue(i) = min(max_colour,nint(b))
	end do
C*** cyan band
	istart = iend + 1
	iend = max_den - 1
 	do i=istart,iend
	 brightness = exp(power)
	 power = power + step
	 total = brightness * scale * float(i)
	 red(i) = 0
	 green(i) = min(max_colour,nint(0.5 * total))
	 blue(i) = min(max_colour,nint(0.5 * total))
	end do
	red(max_den) = max_colour
	green(max_den) = max_colour
	blue(max_den) = max_colour
	call ximagesetcolourtable(min_den,max_den,red,green,blue)
	return
	end
C********************************************************************
C***
        subroutine convert_to_image
     *  (ixscreen,iyscreen,iximage,iyimage)
C***
C********************************************************************
C***
C*** subroutine to convert screen coordinates to image or transform
C*** when converting to read the map, don't forget to add 1 to
C*** the array element (in x) as this starts at 1
        include    'Ximdisp_common.for'
        if(image) then
          iximage = icompress * (ixscreen - ixmin) + nxstart
  	  iyimage = icompress * (iyscreen - iymin) + nystart
        else
	 if(phasemap) then
	  iximage = ixscreen - iphasex
	  iyimage = iyscreen - iphasey
C	  iyimage = -(iyscreen - iphasey)
	 else
          iximage = ixscreen - icenx
C          iyimage = -(iyscreen - iceny)
          iyimage = iyscreen - iceny
	 end if
        end if
	return
	end
C********************************************************************
C***
        subroutine convert_to_screen
     *  (iximage,iyimage,ixscreen,iyscreen)
C***
C********************************************************************
C***
C*** subroutine to convert image coordinates to screen
        include    'Ximdisp_common.for'
        if(image) then
	  compression = float(icompress)
          ixscreen = nint(float(iximage - nxstart) / compression)
     *             + ixmin 
  	  iyscreen = nint(float(iyimage - nystart) / compression)
     *             + iymin
        else
	 if(phasemap) then
          ixscreen = iximage + iphasex
          iyscreen = iyimage + iphasey
	 else
          ixscreen = iximage + icenx
          iyscreen = iyimage + iceny
	 end if
        end if
	return
	end
C********************************************************************
C***
        subroutine convert_to_transform
     *  (iximage,iyimage,ixtrans,iytrans)
C***
C********************************************************************
C***
C*** subroutine to convert transform image coordinates with origin
C*** at centre to map transform coordinates starting at 1,0
C*** results will be the same for normal or phaseworld map
        include    'Ximdisp_common.for'
        if(image) return
C*** add 1 as arrays start at 1
	ixtrans = abs(iximage) * 2 + 1
C*** use sign to decide whether to add or subtract
C*** add if top right, bottom left, subtract otherwise
	isgn = isign(1,iximage*iyimage)
C	iytrans = nxyz(2) / 2 + isign(iyimage,iximage*iyimage)
	iytrans = nxyz(2) / 2 + isgn * abs(iyimage)
C*** add 1 to top half to allow for equator
c	if(isgn .gt. 0) iytrans = iytrans + 1
	return
	end
C****************************************************************************
C***
      subroutine draw_cross(ix,iy,isize)
C***
C****************************************************************************
C*** subroutine to draw a small cross to the screen
C****************************************************************************
	include    'Ximdisp_common.for'
	inc = isize / 2
	minx = ix - inc
	maxx = ix + inc
	miny = iy - inc
	maxy = iy + inc
	call ximagedrawlines(minx,miny,maxx,maxy,1)
	call ximagedrawlines(minx,maxy,maxx,miny,1)
	return
	end
C*************************************************************************
C***
        subroutine draw_line
     *  (sinphi,cosphi,ixl,iyl,ixr,iyr,ix,iy,fix,fiy)
C***
C*************************************************************************
C       subroutine to draw line cut off @ edges of box
C*************************************************************************
        include    'Ximdisp_common.for'
c***
        fixmin = float(ixmin)
        fixmax = float(ixmax)
        fiymin = float(iymin)
        fiymax = float(iymax)
c*** set up lhs parametres
        if(cosphi.eq.0) go to 200
        if(sinphi.eq.0) go to 300
        ixl = ixmin
        distl = (fix - fixmin)/cosphi
        iyl = fiy - distl * sinphi
c*** test to see if bottom axis cut 
        if(iyl.lt.iymin) then
         iyl = iymin
         distl = (fiy - fiymin) / sinphi
         ixl = fix - distl * cosphi
        end if
c*** test to see if top axis cut
        if(iyl.gt.iymax) then
         iyl = iymax
         distl = (fiymax - fiy) / sinphi
         ixl = fix + distl * cosphi
        end if
c*** rhs parameters
        ixr = ixmax
        distr = (fixmax - fix) / cosphi
        iyr = fiy + distr * sinphi
c*** test to see if top axis cut
        if(iyr.gt.iymax) then
         iyr = iymax
         distr = (fiymax - fiy) / sinphi
         ixr = fix + distr * cosphi
        end if
c*** test to see if bottom axes cut
        if(iyr.lt.iymin) then
         iyr = iymin
         distr = (fiy - fiymin) / sinphi
         ixr = fix - distr * cosphi
        end if
        go to 500
c*** special case cosphi = 0.
  200   ixl = ix
        iyl = iymin
        ixr = ix
        iyr = iymax
        distl = fiy - fiymin
        distr = fiymax - fiy
        go to 500
c***  special case sinphi = 0.
  300   ixl = ixmin
        iyl = iy
        ixr = ixmax
        iyr = iy
        distl = fix - fixmin
        distr = fixmax - fix
c***
c*** write to screen
  500   call ximagedrawlines(ixl,iyl,ixr,iyr,1)
        return
        end    
C****************************************************************************
C***
      subroutine draw_string
C***
C****************************************************************************
C*** subroutine to draw text string to the screen
C****************************************************************************
	include    'Ximdisp_common.for'
	call change_font
C*** font selected and loaded, proceed with text
  	iolabel(nlabels+1) = 
     *  'Mark cursor position for start of string'//
     *  ' with left mouse button'
	call ximagelabeldisplay(iolabel,nlabels+1)
	call ximagereadpointer(ix,iy)
	call ximagelabelhide
	iolabel(nlabels+1) = 'Type in text string'
	return_string = ' '
	call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	call ximagedrawtext(ix,iy,return_string)
	return
	end
C*************************************************************************
C***
        subroutine extract_integers
     *  (nvalues,return_string,i1,i2,i3,i4,i5,i6)
C***
C*************************************************************************
C*** subroutine to return integer values from a string
C*************************************************************************
	include 'Ximdisp_common.for'
  100	if(nvalues .eq. 1) then
	 read(return_string,*,err=900,end=900) i1
	 return
	else if(nvalues .eq. 2) then
	 read(return_string,*,err=900,end=900) i1,i2
	 return
	else if(nvalues .eq. 3) then
	 read(return_string,*,err=900,end=900) i1,i2,i3
	 return
	else if(nvalues .eq. 4) then
	 read(return_string,*,err=900,end=900) i1,i2,i3,i4
	 return
	else if(nvalues .eq. 5) then
	 read(return_string,*,err=900,end=900) i1,i2,i3,i4,i5
	 return
	else if(nvalues .eq. 6) then
	 read(return_string,*,err=900,end=900) i1,i2,i3,i4,i5,i6
	 return
	end if
  900   write(iolabel(nlabels+1),'
     *  (''Typing error, re-type '',i1,'' integers...'')')
     *  nvalues
	return_string = ' '
	call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	if(return_string .eq. ' ' .or. return_string .eq. '/') then
	 i1 = 0
	 i2 = 0
	 i3 = 0
	 i4 = 0
	 i5 = 0
	 i6 = 0
	 return
	else
	 go to 100
	end if
	end
C*************************************************************************
C***
        subroutine extract_reals
     *  (nvalues,return_string,f1,f2,f3,f4,f5,f6)
C***
C*************************************************************************
C*** subroutine to return integer values from a string
C*************************************************************************
	include 'Ximdisp_common.for'
  100	if(nvalues .eq. 1) then
	 read(return_string,*,err=900,end=900) f1
	 return
	else if(nvalues .eq. 2) then
	 read(return_string,*,err=900,end=900) f1,f2
	 return
	else if(nvalues .eq. 3) then
	 read(return_string,*,err=900,end=900) f1,f2,f3
	 return
	else if(nvalues .eq. 4) then
	 read(return_string,*,err=900,end=900) f1,f2,f3,f4
	 return
	else if(nvalues .eq. 5) then
	 read(return_string,*,err=900,end=900) f1,f2,f3,f4,f5
	 return
	else if(nvalues .eq. 6) then
	 read(return_string,*,err=900,end=900) f1,f2,f3,f4,f5,f6
	 return
	end if
  900   write(iolabel(nlabels+1),'
     *  (''Typing error, re-type '',i1,'' reals...'')')
     *  nvalues
	return_string = ' '
	call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	if(return_string .eq. ' ' .or. return_string .eq. '/') then
	 f1 = 0.
	 f2 = 0.
	 f3 = 0.
	 f4 = 0.
	 f5 = 0.
	 f6 = 0.
	 return
	else
	 go to 100
	end if
	end
C****************************************************************
C***
	subroutine fftdisplay
C***
C****************************************************************
C*** subroutines to calculate and display fft from selected array
C***  or from boxed area.
C****************************************************************
	include 'Ximdisp_common.for'
C***
	imove = 1
	ierr = 0
	autoscale = .true.
	amp = .true.
	poly = .false.
	ixpos = 0
	iypos = 0
	ipixmap = 1
	xsize = 0.
	transmin = 0.
	transmax = 20.
	imode = 1
C*** set variables for black/white colour table use
	fmaxcolour = float(max_colour)
	frange = float(max_den - min_den)
	colourscale = (fmaxcolour + 1.0) / frange
  50	flip = .false.
	rotang = 0.
	call ximagemenuhide
	call ximageoverlayhide
	iolabel(nlabels+1) = 'Select transform size'
  60    call ximagelabeldisplay(iolabel,nlabels+1)
	ifirst = .true.
	menulist(1) = '128 x 128'
	menulist(2) = '256 x 256'
	menulist(3) = '512 x 512'
	menulist(4) = '1024 x 1024'
	menulist(5) = 'Specify rectangular size'
	menulist(6) = 'Box/pad/float an area first'
	menulist(7) = 'Return main menu'
	call ximagemenuinit(menulist,7)
	job = -1
  100   call ximagewait(job)
	if(job .le. 0) then
	 go to 100
C********************************************************
C*** 128 x 128
C********************************************************
	else if(job .eq. 1) then
	 nxbox = 128
	 nybox = 128
	 nytrans = 128
	 call ximageremovevectors
C********************************************************
C*** 256 x 256
C********************************************************
	else if(job .eq. 2) then
	 nxbox = 256
	 nybox = 256
	 nytrans = 256
	 call ximageremovevectors
C********************************************************
C*** 512 x 512
C********************************************************
	else if(job .eq. 3) then
	 nxbox = 512
	 nybox = 512
	 nytrans = 512
	 call ximageremovevectors
C********************************************************
C*** 1024 x 1024
C********************************************************
	else if(job .eq. 4) then
	 nxbox = 1024
	 nybox = 1024
	 nytrans = 1024
	 call ximageremovevectors
C********************************************************
C*** specify box size
C********************************************************
	else if(job .eq. 5) then
	 call ximagemenuhide
	 call ximagelabelhide
	 iolabel(nlabels+1) = 
     *   'Type (uncompressed )box size in pixels in x and y'
  110	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 call extract_integers(2,return_string,
     *   nxbox,nybox,i3,i4,i5,i6)
C*** pad/float the area ?
	 nytrans = max(nxbox,nybox)
	 itemp = 64
  120    itemp = itemp * 2
	 if(itemp .lt. nytrans) go to 120
	 nytrans = itemp
	 newpad2 = min(1024,nytrans*2)
	 newpad4 = min(1024,nytrans*4)
	 newpad8 = min(1024,nytrans*8)
C*** modify box size
	 write(iolabel(nlabels+1),
     *   '(''Minimum padded box size = '',i6,'' * '',i6)') nytrans, nytrans
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 menulist(1) = 'Do not pad/float'
	 write(menulist(2),
     *   '(''Pad/float image to '',i5,'' * '',i5)') nytrans, nytrans
	 write(menulist(3),
     *   '(''Pad/float image to '',i5,'' * '',i5)') newpad2, newpad2
	 write(menulist(4),
     *   '(''Pad/float image to '',i5,'' * '',i5)') newpad4, newpad4
	 write(menulist(5),
     *   '(''Pad/float image to '',i5,'' * '',i5)') newpad8, newpad8
         menulist(6) = 'Type in new padded box size'
	 menulist(7) = 'Return main menu'
	 call ximagemenuinit(menulist,7)
	 iopt = -1
  130    call ximagewait(iopt)
	 if(iopt .le. 0) then
	  go to 130
C*** accept default
	 else if(iopt .eq. 1) then
	  nytrans = nybox
C*** nxbox * 2
	 else if(iopt .eq. 3) then
	  nytrans = newpad2
C*** nxbox * 4
	 else if(iopt .eq. 4) then
	  nytrans = newpad4
C*** nxbox * 8
	 else if(iopt .eq. 5) then
	  nytrans = newpad8
	 else if(iopt .eq. 6) then
	  call ximagemenuhide
	  call ximagelabelhide
   	  iolabel(nlabels+2) = 'Type new padded box size'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	  if(return_string .ne. ' ') 
     *    call extract_integers(1,return_string,
     *    nytrans,i2,i3,i4,i5,i6)
C*** return main menu
	 else if(iopt .eq. 7) then
	  call ximagemenuhide
	  call ximagelabelhide
	  return
	 end if
C*** force transform even
	 if(nytrans * nytrans .gt. max_overlay) then
	  iolabel(nlabels+1) = 'Transform too large, re-specify :'
	  go to 110
	 end if
	 call ximageremovevectors
C*** note : code goes to label 300
C********************************************************
C*** box/pad/float an area first
C********************************************************
	else if(job .eq. 6) then
	 call fftboxpad
	 if(ierr .eq. 1) then
	  go to 50
	 else if(ierr .eq. 2) then
	  go to 60
	 else if(ierr .eq. 3) then
	  return
	 end if
	 go to 300
C********************************************************
C*** return main menu
C********************************************************
	else if(job .eq. 7) then
	 call ximagelabelhide
	 call ximageremovevectors
	 nlabels = 1
	 return
	end if
	call ximagemenuhide
	call ximagelabelhide
C*****************************************************************
C***
C*** menu loop to compute transforms
C***
C*****************************************************************
  300	if(amp) then
	 iolabel(nlabels+1) = 'Amplitudes displayed'
	else
	 iolabel(nlabels+1) = 'Intensities displayed'
	end if
	if(poly) then
	 iadd = 1
	else
	 iadd = 2
	 iolabel(nlabels+iadd) = 
     *   'Select transform centre with cursor'
	end if
	call ximagelabeldisplay(iolabel,nlabels+iadd)
	menulist(1) = 'Re-specify box'
	menulist(2) = 'Modify scale factors'
	if(phasemap) then
	 menulist(3) = 'Switch colour table black/white'
	else
	 menulist(3) = 'Switch colour table to phase colours'
	end if
	menulist(4) = 'Switch amplitudes/intensities'
	menulist(5) = 'Save boxed area to file'
	menulist(6) = 'Save transform as postscript file'
	menulist(7) = 'Hide transform overlay'
	menulist(8) = 'Erase vectors'
	menulist(9) = 'Return main menu'
	call ximagemenuinit(menulist,9)
	job = -1
  500   call ximagewait(job)
	if(job .lt. 0) then
	 go to 500
C********************************************************
C*** compute a transform
C********************************************************
	else if(job .eq. 0) then
	 if(poly) go to 500
	 call ximagereadmenupointer(ixpos, iypos)
	 call ximagedrawcircle(ixpos,iypos,2)
	 if(nybox .ne. nytrans) then
          maxx = ixpos + nxbox / 2
          minx = ixpos - nxbox / 2
          maxy = iypos + nybox / 2
          miny = iypos - nybox / 2
          call ximagedrawbox(minx,miny,maxx,maxy)
	 end if
	 call convert_to_image(ixpos,iypos,ixim,iyim)
c	 avx = ixim
c	 avy = iyim
	 mapcenx = ixim
	 mapceny = iyim
	 rotang = 0.
	 go to 1000
C********************************************************
C*** modify box
C********************************************************
	else if(job .eq. 1) then
	 call ximagelabelhide
	 call ximagemenuhide
C*** back to black/white
	 if(phasemap) then
	  phasemap = .false.
	  call colour_blackwhite
          call ximageoverlayinit
     *    (ixpos,iypos,nxtrans-1,nytrans-1,transbuf,imove,ierr)
	 end if
	 poly = .false.
	 go to 50
C********************************************************
C*** modify scale factors
C********************************************************
	else if(job .eq. 2) then
	 autoscale = .false.
	 call ximagelabelhide
	 call ximagemenuhide
         write(iolabel(nlabels+1),
     *   '('' Densities currently scaled = '',2g12.4)')
     *   transmin,transmax
         write(iolabel(nlabels+2),
     *   '('' Available min,max density limits = '',2g12.4)')
     *   trmin, trmax
         write(iolabel(nlabels+3),'(
     *   '' type density limits for display :'')')  
 700	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+3)
	 if(return_string .ne. ' ' .and. return_string .ne. '/') then
	  call extract_reals(2,return_string,tmin,tmax,f3,f4,f5,f6)
	  if(tmin .gt. tmax) go to 700
	  if(tmin .ne. 0. .or. tmax .ne. 0.) then
	   transmin = tmin	 
	   transmax = tmax
	  end if
	 end if
C*** no transform displayed, set ifirst false so it is not reset until
C*** requested.
	 if(ifirst) then
	  ifirst = .false.
          go to 300
	 end if
	 call fftcalc
C*** send transform map to display
	 if(phasemap) then
          call ximageoverlayinit
     *    (ixpos,iypos,nxtrans-1,nytrans-1,phasebuf,imove,ierr)
	 else
	  call ximageoverlayinit
     *    (ixpos,iypos,nxtrans-1,nytrans-1,transbuf,imove,ierr)
	 end if
	 if(ierr .ne. 0) then
	  iolabel(nlabels+1) = 
     *    'Error in display, press any key to return main menu'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	  return
	 end if
	 go to 300
C********************************************************
C*** reset colour table to/from special phase map
C********************************************************
	 else if(job .eq. 3) then
	  call ximagemenuhide
	  call ximagelabelhide
C*** back to black/white
	  if(phasemap) then
	   phasemap = .false.
	   call colour_blackwhite
           call ximageoverlayinit
     *     (ixpos,iypos,nxtrans-1,nytrans-1,transbuf,imove,ierr)
C*** set colours for special phase map
	  else
	   phasemap = .true.
	   call colour_phase
           call ximageoverlayinit
     *     (ixpos,iypos,nxtrans-1,nytrans-1,phasebuf,imove,ierr)
	  end if
C*** return if error in display
	  if(ierr .ne. 0) then
	   iolabel(nlabels+1) = 
     *     'Error in display, press any key to return main menu'
	   return_string = ' '
	   call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	   return
	  end if
	  go to 300
C********************************************************
C*** switch amplitudes/intensities
C********************************************************
	else if(job .eq. 4) then
	 call ximagelabelhide
	 call ximagemenuhide
	 if(amp) then
	  amp = .false.
	 else
	  amp = .true.
	 end if
	 if(ifirst) go to 300
	 call fftcalc
C*** back to black/white
	 if(phasemap) then
	  phasemap = .false.
	  call colour_blackwhite
	 end if
C*** send transform map to display
	 call ximageoverlayinit
     *   (ixpos,iypos,nxtrans-1,nytrans-1,transbuf,imove,ierr)
	 if(ierr .ne. 0) then
	  iolabel(nlabels+1) = 
     *    'Error in display, press any key to return main menu'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	  return
	 end if
	 go to 300
C********************************************************
C*** save boxed area
C********************************************************
	else if(job .eq. 5) then
	 call ximagelabelhide
	 call ximagemenuhide
	 iolabel(nlabels+1) = 'Output file name'
	 boxfile = ' '
	 call ximageioboxdisplay(iolabel,boxfile,nlabels+1)
	 old = .false.
 	 call check_file(boxfile)
         call imopen(idevout,boxfile,'new')
	 if(nybox .eq. nytrans) then
          ixyz(1) = nxbox
          ixyz(2) = nybox
          ixyz(3) = 1
          kxyz(1) = nxbox
          kxyz(2) = nybox
          kxyz(3) = 1
	  xorigin = float(nxbox / 2)
	  yorigin = float(nybox / 2)
	  zorigin = 0.0
	 else
          ixyz(1) = nxtrans - 2
          ixyz(2) = nytrans
          ixyz(3) = 1
          kxyz(1) = nxtrans - 2
          kxyz(2) = nytrans
          kxyz(3) = 1
	  xorigin = float(ixyz(1) / 2)
	  yorigin = float(ixyz(2) / 2)
	  zorigin = 0.0
	 end if
         call icrhdr(idevout,ixyz,kxyz,imode,title,0)
         call ialorg(idevout,xorigin,yorigin,zorigin)
	 call getdate(date,nsecs)
	 itheta = nint(rotang / degrad)
	 if(flip) then
	  write(title,'(
     *    ''Ximdisp boxed '',a,''  Rotated '',i4,
     *    '' degs & flipped, map centre '',2i6)') 
     *    date(5:16), itheta, mapcenx, mapceny
	 else
	  write(title,'(
     *    ''Ximdisp boxed area '',a,''  Rotated '',i4,
     *    '' degrees, map centre '',2i6)') date(5:16), 
     *       itheta, mapcenx, mapceny
	 end if
         call iwrhdr(idevout,title,1,boxmin,boxmax,boxmean)
C*** write in a loop as you write nxbox, not nxtrans in x
	 if(nybox .eq. nytrans) then
	  do iy=1,nytrans
	   ixy = nxtrans * (iy - 1)
	   do ix=1,nxbox
	    aline(ix) = mapout(ixy + ix)
	   end do
	   call iwrlin(idevout,aline)
	  end do
	 else
	  do iy=1,nytrans
	   ixy = nxtrans * (iy - 1)
	   do ix=1,nxtrans - 2
	    aline(ix) = mapout(ixy + ix)
	   end do
	   call iwrlin(idevout,aline)
	  end do
	 end if
	 call imclose(idevout)
	 go to 300
C********************************************************
C*** dump transform to postscript file
C********************************************************
	else if(job .eq. 6) then
	 call ximagemenuhide
	 iolabel(nlabels+1) = 
     *   'Wait for postscript dump to file : Ximage.ps ...'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 colour = .false.
	 call ximagepostscriptdump
     *   (idevpost,colour,0,nxtrans-2,0,nytrans-2,xsize,ipixmap,ierr)
	 if(ierr .ne. 0) then
	  iolabel(nlabels+1) = 
     *    'Consistency check failure - postscript dump aborted'
	  iolabel(nlabels+2) = 'Type <cr> to return main menu'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	  nlabels = nlabels - 1
	 end if
	 go to 300
C********************************************************
C*** hide transform
C********************************************************
	else if(job .eq. 7) then
	 call ximageoverlayhide
	 go to 500
C********************************************************
C*** erase vectors
C********************************************************
	else if(job .eq. 8) then
	 call ximageremovevectors
	 go to 500 
C********************************************************
C*** return main menu
C********************************************************
	else if(job .eq. 9) then
	 call ximageoverlayhide
	 call ximageremovevectors
	 call ximagemenuhide
	 call ximagelabelhide
	 return
	end if
	return
C*******************************************************************
C***
C*** compute and display a transform
C***
C*******************************************************************
 1000   call ximageoverlayhide
C*** read box from map
C*** add 1 as arrays start at 1
	ixstart = ixim - nxbox / 2 + 1
	iystart = iyim - nybox / 2
 	ixend = ixstart + nxbox - 1
	iyend = iystart + nybox - 1
	if(ixstart .lt. 1 .or. iystart .lt. 0
     *   .or. ixend .gt. nxyz(1) .or. iyend .ge. nxyz(2)) then
	 iolabel(nlabels+1) = 'Box outside map limits'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 go to 500
	end if
	boxmin = bignum
	boxmax = -bignum
	boxmean = 0.
C*** box area without padding/floating
	if(nytrans .eq. nybox) then
	 nxtrans = nxbox + 2
C*** position map to correct section
	 call imposn(idevmap,nzstart,iystart)
	 do iy=1,nytrans
	  ny = (iy - 1) * nxtrans
	  call irdlin(idevmap,aline)
	  do ix=1,nxtrans
	   ixy = ny + ix
	   den = aline(ix+ixstart-1)
	   transform(ixy) = den
	   if(ix .le. nxbox) mapout(ixy) = den
	   boxmin = min(boxmin,den)
	   boxmax = max(boxmax,den)
	   boxmean = boxmean + den
	  end do
	 end do
	 boxmean = boxmean / float(nxtrans * nytrans)
C*** calculate transform
	 call TDXFFT(transform,nxbox,nybox,0)
C*** box area with padding/floating
	else
	 nxtrans = nytrans + 2
	 background = 0
	 icount = 0
C*** position map to correct section
	 call imposn(idevmap,nzstart,iystart)
C*** first line of data
	 call irdlin(idevmap,aline)
	 do ix=ixstart,ixend
	  background = background + aline(ix)
	  icount = icount + 1
	 end do
C*** central section of data
	 do iy=iystart+1,iyend-1
	  call irdlin(idevmap,aline)
	  background = background + aline(ixstart) + aline(ixend)
	  icount = icount + 2
	 end do
C*** last line of data
	 call irdlin(idevmap,aline)
	 do ix=ixstart,ixend
	  background = background + aline(ix)
	  icount = icount + 1
	 end do
	 background = background / icount
C*** box padded/floated area
	 do ixy=1,nxtrans * nytrans
	  transform(ixy) = 0
	  mapout(ixy) = 0
	 end do
C*** position map to correct section
	 call imposn(idevmap,nzstart,iystart)
	 nxyshift = nxtrans * ((nytrans - nybox + 1) /2)
     *                      + (nxtrans - nxbox - 1) / 2
	 do iy = 1,nybox
	  call irdlin(idevmap,aline)
	  nxy = nxtrans * (iy - 1) + nxyshift
	  do ix = 1,nxbox
	   ixy = nxy + ix
	   den = aline(ix+ixstart-1) - background
	   transform(ixy) = den
	   mapout(ixy) = den
	   boxmin = min(boxmin,den)
	   boxmax = max(boxmax,den)
	   boxmean = boxmean + den
	  end do
	 end do
	 boxmean = boxmean / float(nxtrans * nytrans)
C*** calculate transform
	 call TDXFFT(transform,nytrans,nytrans,0)
	end if
C*** calculate min,max,mean
	if(ifirst) then
         call fftscale(transform,nxtrans/2,nytrans,
     *    transmin,transmax,transmean)
	 trmax = transmax	
	 trmin = transmin
	 transmax = transmean
	 ifirst = .false.
	end if
	call fftcalc
C*** send transform map to display
	if(phasemap) then
         call ximageoverlayinit
     *   (ixpos,iypos,nxtrans-1,nytrans-1,phasebuf,imove,ierr)
	else
	 call ximageoverlayinit
     *   (ixpos,iypos,nxtrans-1,nytrans-1,transbuf,imove,ierr)
	end if
	if(ierr .ne. 0) then
	 iolabel(nlabels+1) = 
     *   'Error in display, press any key to return main menu'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 return
	end if
	boxmean = boxmean / float(nxtrans * nytrans)
	go to 300
	end
C****************************************************************
C***
	subroutine fftboxpad
C***
C****************************************************************
C*** subroutine to box, pad, float, rotate, fftrans
	include 'Ximdisp_common.for'
	imove = 1
	poly = .true.
	call polygon
	if(npts .eq. 0) then
	 call ximagelabelhide
	 call ximageremovevectors
	 nlabels = 1
	 ierr = 3
	 return
	end if
C*** calculate box edges and centre of gravity
	avx = 0.
	avy = 0.
c	call convert_to_image(mnx,mny,iminx,iminy)
c	call convert_to_image(mxx,mxy,imaxx,imaxy)
C*** make sure box edges do not go out of image size
c	iminx = max(0,iminx)
c	imaxx = min(nox*icompress,imaxx)
c	iminy = max(0,iminy)
c	imaxy = min(noy*icompress,imaxy)
C*** mod 26.08.05
c	iminx = max(0,mnx)
c	imaxx = min(nox,mxx)
c	iminy = max(0,mny)
c	imaxy = min(noy,mxy)
	iminx = max(0,mnx)
	imaxx = min(nxyz(1),mxx)
	iminy = max(0,mny)
	imaxy = min(nxyz(2),mxy)
	call convert_to_screen(mnx,mxx,minscreenx,maxscreenx)
	call convert_to_screen(mny,mxy,minscreeny,maxscreeny)
C*** force number in x,y even
	if(mod(imaxx - iminx,2) .ne. 0) iminx = iminx + 1
	if(mod(imaxy - iminy,2) .ne. 0) iminy = iminy + 1
C*** convert to image and calculate c of g
	do i=1,npts + 1
c	 call convert_to_image(ixp(i),iyp(i),ixpt,iypt)
	 ixpt = ixp(i)
	 iypt = iyp(i)
	 xd(i) = float(ixpt)
	 yd(i) = float(iypt)
	 if(i .le. npts) then
	  avx = avx + float(ixpt - iminx)
	  avy = avy + float(iypt - iminy)
	 end if
	end do
	pts = float(npts)
	avx = avx / pts
	avy = avy / pts
	mapcenx = avx + iminx
	mapceny = avy + iminy
C*** calculate padded box sixe
	xdist = float(imaxx - iminx)
	ydist = float(imaxy - iminy)
	nxbox = nint(sqrt(xdist * xdist + ydist * ydist))
	if(nxbox .gt. max_overlay_width) then
	 iolabel(nlabels+1) = 'Boxed area too long for program'
	 ierr = 2
	 return
	end if
	nybox = 64
 140	nybox = nybox * 2
	if(nxbox .gt. nybox) go to 140
	nxbox = nybox
	newbox2 = min(1024,nxbox*2)
	newbox4 = min(1024,nxbox*4)
	newbox8 = min(1024,nxbox*8)
C*** modify box size
	write(iolabel(nlabels+1),
     *  '(''Minimum padded box size = '',i6,'' * '',i6)') nxbox, nybox
	call ximagelabeldisplay(iolabel,nlabels+1)
	write(menulist(1),
     *  '(''Pad image to '',i5,'' * '',i5)') nxbox, nxbox
	write(menulist(2),
     *  '(''Pad image to '',i5,'' * '',i5)') newbox2, newbox2
	write(menulist(3),
     *  '(''Pad image to '',i5,'' * '',i5)') newbox4, newbox4
	write(menulist(4),
     *  '(''Pad image to '',i5,'' * '',i5)') newbox8, newbox8
        menulist(5) = 'Type in new box size'
	menulist(6) = 'Return main menu'
	call ximagemenuinit(menulist,6)
	opt = -1
  150   call ximagewait(iopt)
	if(iopt .le. 0) then
	 go to 150
C*** accept default
C*** nxbox * 2
	else if(iopt .eq. 2) then
	 nxbox = newbox2
	 nybox = nxbox
C*** nxbox * 4
	else if(iopt .eq. 3) then
	 nxbox = newbox4
	 nybox = nxbox
C*** nxbox * 8
	else if(iopt .eq. 4) then
	 nxbox = newbox8
	 nybox = nxbox
	else if(iopt .eq. 5) then
	 call ximagemenuhide
	 call ximagelabelhide
   	 iolabel(nlabels+2) = 'Type new box size'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	 if(return_string .ne. ' ') 
     *   call extract_integers(2,return_string,
     *   nxbox,nybox,i3,i4,i5,i6)
C*** return main menu
	else if(iopt .eq. 6) then
	 call ximagemenuhide
	 call ximagelabelhide
	 ierr = 3
	 return
	end if
C*** box pad size decided
	call ximagemenuhide
	call ximagelabelhide
c	ixpos = mnx + (mxx - mnx) / 2
c	iypos = mny + (mxy - mny) / 2
	ixpos = minscreenx + (maxscreenx - minscreenx) / 2
	iypos = minscreeny + (maxscreeny - minscreeny) / 2
        nxtrans = nxbox + 2
	nytrans = nybox
	if(nxtrans * nytrans .gt. max_overlay) then
	 iolabel(nlabels+1) = 'Box too large for program'
	 ierr = 2
	 return
	end if
C*** calculate background
	icount = 0
	background = 0.
  	do 160 n=1,npts
	 ix1 = xd(n)
	 iy1 = yd(n)
	 ix2 = xd(n+1)
	 iy2 = yd(n+1)
C*** vertical line
	 if(ix1 .eq. ix2) then
	  if(iy1 .lt. iy2) then
	   istep = 1
	  else
	   istep = -1
	  end if
C*** add 1 to aline element as array aline starts at 1
	  do iy=iy1,iy2-istep,istep
C*** position map to correct section
	   call imposn(idevmap,nzstart,iy)
	   call irdlin(idevmap,aline)
	   background = background + aline(ix1+1)
	   icount = icount + 1
	  end do
	  go to 160
	 else
	  slope = float(iy2 - iy1) / float(ix2 - ix1)
	 end if
	 c = float(iy1) - slope * float(ix1)
C*** horizontal line
	 if(iy1 .eq. iy2) then
C*** position map to correct section
	  call imposn(idevmap,nzstart,iy1)
	  call irdlin(idevmap,aline)
	  if(ix1 .lt. ix2) then
	   istep = 1
	  else 
	   istep = -1
	  end if
	  do ix = ix1,ix2-istep,istep
	   background = background + aline(ix+1)	
	   icount = icount + 1 
	  end do
	  go to 160
	 else if(iy1 .lt. iy2) then
	  istep = 1
	 else
	  istep = -1
	 end if
C*** lines with slopes
 	 do iy=iy1,iy2-istep,istep
C*** position map to correct section
	  call imposn(idevmap,nzstart,iy)
	  call irdlin(idevmap,aline)
C*** calculate x position from y = mx + c
	  ix = nint((float(iy) - c) / slope)
	  background = background + aline(ix+1)
	  icount = icount + 1
	 end do
  160   continue
        background = background / float(icount)
	boxmin = 1000000.
	boxmax = -1000000.
	boxmean = 0.
C*** initialize map to background or 0
	nyshift = nytrans / 2 - avy
	do ixy=1,nxtrans * nytrans
	  transform(ixy) = 0
	end do
C*** box polygon for real
	nxshift = nxbox / 2 - avx
C*** position map to correct section
	call imposn(idevmap,nzstart,iminy)
C*** add 1 as coords origin starts at 0 but array starts at 1
	ixl = iminx + 1
	ixr = imaxx + 1
	do iy=iminy,imaxy
	 ixy = nxtrans * (nytrans - nyshift - 1)
	 nyshift = nyshift + 1
	 call irdlin(idevmap,aline)
	 y = float(iy)
C*** test point inside polygon
	 do ix = ixl,ixr
	  x = float(ix)
C*** if point inside polygon include background
	  if(box_inside(x,y,npts,xd,yd)) then
           den = aline(ix) - background
	  else
	   den = 0
	  end if
	  boxmin = min(boxmin,den)
	  boxmax = max(boxmax,den)
	  boxmean = boxmean + den
	  jxy = nxshift + ix - ixl
	  nxy = ixy + jxy
	  transform(nxy) = den
	 end do
	end do
	boxmean = boxmean / float(nxbox * nybox)
	boxscl = grey / (boxmax - boxmin)
C*** send boxed,padded, floated area to display
  220   do i=1,nxbox * nxtrans
	 transbuf(i) = min(grey,max(0.,boxscl * (transform(i)-boxmin)))
	end do
        call ximageoverlayinit
     *  (ixpos,iypos,nxtrans,nytrans,transbuf,imove,ierr)
	if(ierr .ne. 0) then
	 iolabel(nlabels+1) = 
     *   'Error in display, press any key to return main menu'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 ierr = 3
	 return
	end if
C*** rotate before transform ?
C******************************************************************
C***
C*** menu to control rotation/rescaling/compute fft
C***
C******************************************************************
  240   iolabel(nlabels+1) = 'Select rotation method or compute fft'
  250   call ximagelabeldisplay(iolabel,nlabels+1)
	menulist(1) = 'Automatic rotation'
	menulist(2) = 'Specify rotation angle'
	menulist(3) = 'Rescale image'
	menulist(4) = 'Compute fft'
	menulist(5) = 'Re-specify box'
	menulist(6) = 'Return main menu'
	call ximagemenuinit(menulist,6)
	iopt = -1
  260   call ximagewait(iopt)
	if(iopt .le. 0) then
	 go to 260
C******************************************************************
C*** rotate the boxed area
C******************************************************************
	else if(iopt .le. 2) then
	 call ximagemenuhide
	 call ximagelabelhide
C******************************************************************
C*** calculate rotation angle automatically
C******************************************************************
	 if(iopt .eq. 1) then
C*** find the longest side
	  distmax = 0.
	  do i=1,npts
	   if(i .lt. npts) then
	    distx = ixp(i+1) - ixp(i)
	    disty = iyp(i+1) - iyp(i)
	   else
	    distx = ixp(npts) - ixp(1)
	    disty = iyp(npts) - iyp(1)
	   end if
	   dist = distx * distx + disty * disty
	   if(dist .gt. distmax) then
	    distmax = dist
	    xdist = distx
	    ydist = disty
	   end if
	  end do
C*** find smallest angle, and flip by 90 degrees if necessary
	  absx = abs(xdist)
	  absy = abs(ydist)
	  if(absy .lt. absx) flip = .true.
	  sidemin = min(absx, absy)
	  sidemax = max(absx, absy)
	  rotang = atan2(sidemin, sidemax)
C*** negate rotang if necessary
	  if(flip) then
	   if(xdist * ydist .lt. 0.0) rotang = -rotang
	  else
	   if(xdist * ydist .gt. 0.0) rotang = -rotang
	  end if
C*** rotation by specification
	 else 
	  iolabel(nlabels+1) = 
     *    'Type rotation angle in degrees (+ve clockwise)'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	  if(return_string .ne. ' ') 
     *    call extract_integers(1,return_string,
     *    itheta,i2,i3,i4,i5,i6)
	  iatheta = abs(itheta)
	  if(iatheta .gt. 45) then
	   itheta = -sign(90 - iatheta,itheta)
	   flip = .true.
	  end if
	  rotang = degrad * float(itheta)
	 end if
C*** now perform the rotation
	 if(flip) then
	  write(iolabel(nlabels+1),'(''Rotating by '',i4,
     *    '' degrees with 90 deg flip'')')
     *    nint(rotang/degrad)
	 else
	  write(iolabel(nlabels+1),'(''Rotating by '',i4,'' degrees'')')
     *    nint(rotang/degrad)
	 end if
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call fftrotate(rotang,flip,nxtrans,nytrans,transform,mapout)
C*** copy real map to byte
	 do i=1,nxbox * nxtrans
	  transbuf(i) = min(grey,max(0.,boxscl * (mapout(i)-boxmin)))
	 end do
         call ximageoverlayinit
     *   (ixpos,iypos,nxtrans,nytrans,transbuf,imove,ierr)
	 if(ierr .ne. 0) then
	  iolabel(nlabels+1) = 
     *    'Error in display, press any key to return main menu'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	  ierr = 3
	  return
	 end if
C*** now perform the rotation
	 if(flip) then
	  write(iolabel(nlabels+1),'(
     *    ''Area Rotated by '',i4,'' degrees with 90 deg flip'')')
     *    nint(rotang/degrad)
	 else
	  write(iolabel(nlabels+1),'(
     *    ''Area rotated by '',i4,'' degrees'')') nint(rotang/degrad)
	 end if
	 go to 250
C******************************************************************
C*** rescale image
C******************************************************************
	else if(iopt .eq. 3) then
	 if(phasemap) go to 260
	 call ximagelabelhide
	 call ximagemenuhide
         write(iolabel(nlabels+1),
     *   '('' current min,max density limits = '',2g12.4)')
     *   boxmin, boxmax
         write(iolabel(nlabels+2),'(
     *   '' type density limits for display :'')')  
 270	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	 if(return_string .ne. ' ' .and. return_string .ne. '/') then
	  call extract_reals(2,return_string,tmin,tmax,f3,f4,f5,f6)
	  if(tmin .gt. tmax) go to 270
	  if(tmin .ne. 0. .or. tmax .ne. 0.) then
	   boxmin = tmin	 
	   boxmax = tmax
	  end if
	 end if
	 boxscl = grey / (boxmax - boxmin)
	 go to 220
C******************************************************************
C*** compute fft
C******************************************************************
	else if(iopt .eq. 4) then
	 call ximagemenuhide
	 call ximagelabelhide
C*** copy map to transform array
	 if(rotang .eq. 0) then
	  do i=1,nxbox * nxtrans
	   mapout(i)= transform(i) 
	  end do
	 end if
	 do i=1,nxbox * nxtrans
	  transform(i) = mapout(i)
	 end do
	 go to 280
C******************************************************************
C*** respecify box
C******************************************************************
	else if(iopt .eq. 5) then
	 call ximagelabelhide
	 call ximagemenuhide
	 poly = .false.
	 ierr = 1
	 return
C******************************************************************
C*** return main menu
C******************************************************************
	else if(iopt .eq. 6) then
	 call ximageoverlayhide
	 call ximageremovevectors
	 call ximagemenuhide
	 call ximagelabelhide
	 ierr = 3
	 return
	end if
C******************************************************************
C*** calculate transform
C******************************************************************
  280   call TDXFFT(transform,nxbox,nybox,0)
C*** calculate min,max,mean if first time round
	if(ifirst) then
         call fftscale(transform,nxtrans/2,nytrans,
     *   transmin,transmax,transmean)
	 trmax = transmax	
	 trmin = transmin
	 transmax = transmean
	 ifirst = .false.
	end if
	call fftcalc
C*** send transform map to display
	call ximageoverlayinit
     *  (ixpos,iypos,nxtrans-1,nytrans-1,transbuf,imove,ierr)
	if(ierr .ne. 0) then
	 iolabel(nlabels+1) = 
     *   'Error in display, press any key to return main menu'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 ierr = 3
	 return
	end if
	return
	end
C****************************************************************
C***
	subroutine fftcalc
C***
C****************************************************************
	include 'Ximdisp_common.for'
C*** extract transform and write to byte map
	nxhalf = nxtrans / 2
	nyhalf = nytrans / 2
C*** display amplitudes
	if(amp) then
C*** calculate scale factors
	 if(autoscale) then
	  scl = grey / transmean
	 else
	  scl = grey / (transmax - transmin)
	 end if
	 ampscl = 15. / (transmax - transmin + 1.)
C*** calculate transform and transfer to byte map
C*** calculate transform and invert.
C*** top right, bottom left from top half of transform
C*** note that first row of complex numbers is the equator, the first 
C*** column is the meridian. The first row of the bottom half is thrown away.
	 do iy=1,nyhalf
	  ixy = nxtrans * (iy - 1)
	  ny1 = (nxtrans - 1) * (nyhalf - iy + 1)
	  ny2 = (nxtrans - 1) * (nyhalf + iy - 1)
	  do ix=1,nxhalf
	   ixyin = ixy + ix * 2
           aval = transform(ixyin - 1)
           bval = transform(ixyin)
	   den = scl*(sqrt(aval*aval+bval*bval)-transmin)
	   if(den .lt. 1.) den = 1.
	   if(den .gt. grey) den = grey
	   nden = nint(den)
	   nxy1 = ny1 - nxhalf + ix - 1
	   nxy2 = ny2 - nxhalf - ix + 1
C*** load main transform buffer
	   transbuf(nxy1) = nden
	   transbuf(nxy2) = nden
C*** calculate transform coords - subtract 1 from x and y to place origin at 0,0
	   ixtrans = ix - 1
	   iytrans = iy - 1
C*** calculate phase/amplitude
           left = 1
           pshift = -pi * (ixtrans + iytrans)
           call fftextract(aval,bval)
	   nphase = nint(phase / 45.)
	   if(nphase .gt. 7) nphase = 0
	   ampsub = min(max(transmin,amplitude-transmin),transmax)
	   nden = nphase + 8 * nint(ampscl * ampsub)
C*** load phase buffer
	   phasebuf(nxy1) = nden 
	   phasebuf(nxy2) = nden
	  end do
	 end do
C*** bottom right, top left from bottom of transform, discard first line
	 do iy=1,nyhalf-1
	  ixy = nxtrans * (nyhalf + iy)
	  ny1 = (nxtrans - 1) * (nytrans - iy)
	  ny2 = (nxtrans - 1) * iy
	  do ix=1,nxhalf
	   ixyin = ixy + ix * 2
           aval = transform(ixyin - 1)
           bval = transform(ixyin)
	   den = scl*(sqrt(aval*aval+bval*bval)-transmin)
	   if(den .lt. 1.) den = 1.
	   if(den .gt. grey) den = grey
	   nden = nint(den)
	   nxy1 = ny1 - nxhalf + ix - 1
	   nxy2 = ny2 - nxhalf - ix + 1
C*** load main transform buffer
	   transbuf(nxy1) = nden
	   transbuf(nxy2) = nden
C*** calculate transform coords, line closest to equator is last
	   ixtrans = ix - 1
	   iytrans = nyhalf - iy
C*** calculate phase/amplitude
           left = -1
C*** subtract 1 from x to place origin at 0,0
           pshift = -pi * (ixtrans + iytrans)
           call fftextract(aval,bval)
	   nphase = nint(phase / 45.)
	   if(nphase .gt. 7) nphase = 0
	   ampsub = min(max(transmin,amplitude-transmin),transmax)
	   nden = nphase + 8 * nint(ampscl * ampsub)
C*** load phase buffer
	   phasebuf(nxy1) = nden 
	   phasebuf(nxy2) = nden
	  end do
	 end do
C*** display intensities
	else
C*** calculate scale factors
	 tmin = transmin * transmin
	 if(autoscale) then
	  scl = grey / (transmean * transmean - tmin)
	 else
	  scl = grey / (transmax * transmax - tmin)
	 end if
C*** calculate transform and invert.
C*** top right, bottom left from top half of transform
C*** note that first row of complex numbers is the equator, the first 
C*** column is the meridian. The first row of the bottom half is thrown away.
	 do iy=1,nyhalf
	  ixy = nxtrans * (iy - 1)
	  ny1 = (nxtrans- 1) * (nyhalf - iy + 1)
	  ny2 = (nxtrans - 1) * (nyhalf + iy - 1)
	  do ix=1,nxhalf
	   ixyin = ixy + ix * 2
           aval = transform(ixyin - 1)
           bval = transform(ixyin)
	   den = scl*((aval*aval+bval*bval)-tmin)
	   if(den .lt. 1.) den = 1.
	   if(den .gt. grey) den = grey
	   nden = nint(den)
	   nxy1 = ny1 - nxhalf + ix - 1
	   nxy2 = ny2 - nxhalf - ix + 1
C*** load main transform buffer
	   transbuf(nxy1) = nden
	   transbuf(nxy2) = nden
C*** calculate transform coords
	   ixtrans = ix - 1
	   iytrans = iy - 1
C*** calculate phase/amplitude
           left = 1
C*** subtract 1 from each x and y to place origin at 0,0
           pshift = -pi * (ixtrans + iytrans)
           call fftextract(aval,bval)
	   nphase = nint(phase / 45.)
	   if(nphase .gt. 7) nphase = 0
	   ampsub = min(max(tmin,amplitude-tmin),transmax)
	   nden = nphase + 8 * nint(ampscl * ampsub)
C*** load phase buffer
	   phasebuf(nxy1) = nden 
	   phasebuf(nxy2) = nden
	  end do
	 end do
C*** bottom right, top left from bottom of transform
	 do iy=1,nyhalf-1
	  ixy = nxtrans * (nyhalf + iy)
	  ny1 = (nxtrans - 1) * (nytrans - iy)
	  ny2 = (nxtrans - 1) * iy
	  do ix=1,nxhalf
	   ixyin = ixy + ix * 2
           aval = transform(ixyin - 1)
           bval = transform(ixyin)
	   den = scl*((aval*aval+bval*bval)-tmin)
	   if(den .lt. 1.) den = 1.
	   if(den .gt. grey) den = grey
	   nden = nint(den)
	   nxy1 = ny1 - nxhalf + ix - 1
	   nxy2 = ny2 - nxhalf - ix + 1
C*** load main transform buffer
	   transbuf(nxy1) = nden
	   transbuf(nxy2) = nden
C*** calculate transform coords
	   ixtrans = ix - 1
	   iytrans = nyhalf - iy
C*** calculate phase/amplitude
           left = -1
C*** subtract 1 from x to place origin at 0,0
           pshift = -pi * (ixtrans + iytrans)
           call fftextract(aval,bval)
	   nphase = nint(phase / 45.)
	   if(nphase .gt. 7) nphase = 0
	   ampsub = min(max(tmin,amplitude-tmin),transmax)
	   nden = nphase + 8 * nint(ampscl * ampsub)
C*** load phase buffer
	   phasebuf(nxy1) = nden 
	   phasebuf(nxy2) = nden
	  end do
	 end do
	end if
	return
	end
C****************************************************************
C***
	subroutine fftextract(apart,bpart)

C***
C****************************************************************
C*** subroutine to extract amplitude and phase from input data
C***
	include    'Ximdisp_common.for'
	pshift = pshift * left
        cosp = cos(pshift)
        sinp = sin(pshift)
        aval = apart * cosp - bpart * sinp
        bval = apart * sinp + bpart * cosp
	bval = bval * left
        den = aval*aval + bval*bval
        amplitude = sqrt(den)
        if(den .eq. 0) then
         phase = 0.
        else
         phase = radcon * atan2(bval,aval)
         if(phase .lt. 0.) phase = phase + 360.
        end if
  	return
	end
C****************************************************************
C***
	subroutine fftrotate(rotang,flip,nx,ny,mapin,mapout)

C***
C****************************************************************
C*** subroutine to rotate the map
C****************************************************************
C*** assumes centre of gravity in box centre, rotates about the centre
C*** initialize output array
	real*4	mapin(*)
	real*4	mapout(*)
	logical flip
	do i=1,nx * ny
	 mapout(i) = 0
	end do
        sinang = sin(-rotang)
        cosang = cos(-rotang)
	xcen = float(nx) * 0.5
	ycen = float(ny) * 0.5
C*****************************************************************
C*** 90 degree flip
C*****************************************************************
	if(flip) then
C*** subtract half box width and height to make origin at box centre
         do iy = 1,ny
          ydist = float(iy-1) - ycen
          ysinang = ydist * sinang
          ycosang = ydist * cosang
C*** start x loop
c          do 100 ix = 1,nx
          do 100 ix = 2,nx-1
           xdist = float(ix-1) - xcen
           xsinang = xdist * sinang
           xcosang = xdist * cosang
C*** calculate X,Y coords adding half-width and height back on to
C*** reposition box origin bottom left
           xcoord = xcosang - ysinang + xcen
           ycoord = xsinang + ycosang + ycen
C*** interpolate to extract density at exact point
           kx = int(xcoord)
           if(kx .lt. 1 .or. kx .ge. nx) go to 100
           ky = int(ycoord)
           if(ky .lt. 1. or. ky. ge. ny) go to 100
C*** calculate interpolation lengths
           xbit = xcoord - float(kx)
           ybit = ycoord - float(ky)
           xbar = 1. - xbit
           ybar = 1. - ybit
C*** extract densities
	   kxy = (ky - 1) * nx
           den1 = mapin(kxy + kx)
           den2 = mapin(kxy + kx+1)
	   kxy = ky * nx
           den3 = mapin(kxy + kx)
           den4 = mapin(kxy + kx+1)
C*** load into new map
           mapout(nx * (ix - 1) - iy) = 
     *     ybar * (xbar * den1 + xbit * den2) 
     *                  + ybit * (xbar * den3 + xbit * den4)
  100     continue
         end do
C*****************************************************************
C*** no 90 degree flip
C*****************************************************************
	else
         do iy = 1,ny
          ydist = float(iy-1) - ycen
          ysinang = ydist * sinang
          ycosang = ydist * cosang
C*** start x loop
          do 200 ix = 1,nx
           xdist = float(ix-1) - xcen
           xsinang = xdist * sinang
           xcosang = xdist * cosang
C*** calculate X,Y coords adding half-width and height back on to
C*** reposition box origin bottom left
           xcoord = xcosang - ysinang + xcen
           ycoord = xsinang + ycosang + ycen
C*** interpolate to extract density at exact point
           kx = int(xcoord)
           if(kx .lt. 1 .or. kx .ge. nx) go to 200
           ky = int(ycoord)
           if(ky .lt. 1. or. ky. ge. ny) go to 200
C*** calculate interpolation lengths
           xbit = xcoord - float(kx)
           ybit = ycoord - float(ky)
           xbar = 1. - xbit
           ybar = 1. - ybit
C*** extract densities
	   kxy = (ky - 1) * nx
           den1 = mapin(kxy + kx)
           den2 = mapin(kxy + kx+1)
	   kxy = ky * nx
           den3 = mapin(kxy + kx)
           den4 = mapin(kxy + kx+1)
C*** load into new map
           mapout(nx * (iy-1) + ix) = 
     *     ybar * (xbar * den1 + xbit * den2) 
     *                  + ybit * (xbar * den3 + xbit * den4)
  200     continue
         end do
	end if
	return
	end
C****************************************************************************
C***
      subroutine fftscale
     * (array,nx,ny,tmin,tmax,tmean)
C***
C****************************************************************************
C*** subroutine to calculate min, max, mean, standard deviation of a fft
C****************************************************************************
	complex 	array(nx,ny)
	complex 	cval
	real*4		aval
	real*4		bval
	common//aval,bval
	equivalence	(aval,cval)
C***
	tmin = 1000000000.
	tmax = -tmin
	tmean = 0.
C*** calculate min,max,mean
	do iy=1,ny
	 do ix=1,nx
	  cval = cabs(array(ix,iy))
	  den = aval * aval + bval * bval
	  tmin = min(tmin,den)
	  tmax = max(tmax,den)
	  tmean = tmean + den
	 end do
	end do
	fxy = float(nx * ny)
	tmean = tmean / fxy
	return
	end
C****************************************************************************
C***
      subroutine file_error(filename,nlabs)
C***
C****************************************************************************
C*** subroutine to get new filename after error writing a file
C****************************************************************************
	include    'Ximdisp_common.for'
	character	filename*(*)
	nlabs = min(nlabs,max_lines_per_page-3) + 1
	write(iolabel(nlabs),'(''ERROR opening/writing : '',a)') 
     *  filename(1:lnblank(filename))
	iolabel(nlabs+1) = 
     *  'Possibly out of diskspace / disk quota. '//
     *  'Please type new filename :'
	call ximageioboxdisplay(iolabel,filename,nlabs+1)
	return
	end	
C****************************************************************
C***
	subroutine hide_widgets
C***
C****************************************************************
C*** subroutines to hide menu/labels/slider
C****************************************************************
	include 'Ximdisp_common.for'
	call ximagemenuhide
	call ximagesliderhide
	iolabel(nlabels+1) = 'Type ctrl/m to restore menu'
	iolabel(nlabels+2) = 'Type ctrl/l to restore label'
	iolabel(nlabels+3) = 'Type ctrl/s to restore slider'
	iolabel(nlabels+4) = 'Type any key to acknowledge and continue'
	return_string = ' '
	call ximageioboxdisplay(iolabel,return_string,nlabels+4)
	return
	end
C****************************************************************
C***
	subroutine lattice
C***
C****************************************************************
C*** subroutines to refine lattice parameters
C*** lattice calls lattice_read and lattice_refine
C****************************************************************
C***
        include    'Ximdisp_common.for'
	call ximagemenuhide
	call ximagelabelhide
	ifirst = .true.
  600	nlabels = 1
	iolabel(nlabels+1) = 'Type in origin x0, y0, default (nx/2,ny/2)'
	return_string = ' '
	call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	if(return_string .eq. ' ') then
	 if(image) then
C*** mod 26.08.05
c	  x0 = float(nox / 2)
c	  y0 = float(noy / 2)
	  x0 = float(nxyz(1) / 2)
	  y0 = float(nxyz(2) / 2)
	 else
	  x0 = 0.
	  y0 = 0.
	 end if
	else
	 call extract_reals(2,return_string,x0,y0,f3,f4,f5,f6)
	end if
	xorigin = x0
	yorigin = y0
C***********************************************************
C*** menu to control lattice input
C***********************************************************
  700	call ximagelabeldisplay(iolabel,nlabels)
	menulist(1) = 'Input indices/spot positions manually'
	menulist(2) = 'Input file of indices/spot positions'
	menulist(3) = 'Input lattice vectors manually'
	menulist(4) = 'Return main menu'
	call ximagemenuinit(menulist,4)
	job = -1
  800   call ximagewait(job)
	if(job .le. 0) then
	 go to 800
C*** input spot positions, advance to lattice_read
	else if(job .eq. 1) then
	 call ximagemenuhide
C*** input file of indices and spot positions
	else if(job .eq. 2) then
	 call ximagemenuhide
  820	 iolabel(nlabels+1) = 'Type input file name'
	 latfile = ' '
	 call ximageioboxdisplay(iolabel,latfile,nlabels+1)
	 old = .true.
	 call check_file(latfile)
	 open(unit=idevlat,file=latfile,status='old')
	 nspots = 0
  830    nspots = nspots + 1
	 if(nspots .gt. max_points) then
	  return_string = ' '
	  iolabel(nlabels+1) =
     *    'Too many spots, <cr> to continue with current number'
	  call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	  go to 1500
	 end if
	 read(idevlat,*,end=840,err=840)
     *   ih(nspots), ik(nspots), ixim, iyim
	 xd(nspots) = float(ixim)
	 yd(nspots) = float(iyim)
	 go to 830
  840    nspots = nspots - 1
	 if(nspots .le. 0) then
	  iolabel(nlabels+1) = 'No spots found in file'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	  go to 700
	 end if
	 close(idevlat)
	 go to 1000
C*** input lattice vectors
	else if(job .eq. 3) then
	 call ximagemenuhide
	 nspots = 0
	 iolabel(nlabels+1) = 'Type in A vector x1, y1'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 call extract_reals(2,return_string,x1,y1,f3,f4,f5,f6)
	 iolabel(nlabels+1) = 'Type in B vector x2, y2'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 call extract_reals(2,return_string,x2,y2,f3,f4,f5,f6)
  880	 write(iolabel(nlabels+1),'('' How many orders?'')')
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
C*** get number of orders
	 call extract_integers(1,return_string,
     *   irang,i2,i3,i4,i5,i6)
	 if(irang .le. 0) go to 880
C*** reset number of orders if too many
         if(irang.gt.max_lat) then
          write(iolabel(nlabels+1),'(
     *   '' Number of orders reset to'',i4)') max_lat
	  call ximagelabeldisplay(iolabel,nlabels+1)
          irang = max_lat
         end if
	 go to 2000
C*** return main menu
	else if(job .eq. 4) then
	 call ximagemenuhide
	 return
	end if
C****************************************************************
C*** read spot positions manually
C****************************************************************
  900   nspots = 0
 1000   call lattice_read
	if(ierr.ne.0) return
C****************************************************************
C*** read number of orders to refine
C****************************************************************
 1500   nlabels = 1
	write(iolabel(nlabels+1),'('' How many orders?'')')
	return_string = ' '
	call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	call extract_integers(1,return_string,
     *  irang,i2,i3,i4,i5,i6)
	if(irang .le. 0) go to 1500
C*** reset number of orders if too many
        if(irang.gt.max_lat) then
         write(iolabel(nlabels+1),'(
     *   '' Number of orders reset to'',i4)') max_lat
	 call ximagelabeldisplay(iolabel,nlabels+1)
         irang = max_lat
        end if
C***********************************************************
C*** refine the lattice
C***********************************************************
        call lattice_refine
C*** return if refinement failed
        if(.not.refined) then
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 menulist(1) = 'Start spot selection again'
	 menulist(2) = 'Add/remove spots'
	 menulist(3) = 'Return main menu'
	 call ximagemenuinit(menulist,3)
	 job = -1
 1600    call ximagewait(job)
	 if(job.le.0) then
	  go to 1600
C*** start again
	 else if(job.eq.1) then
	  call ximagemenuhide
	  call ximagelabelhide
	  go to 900
C*** add/remove spots
	 else if(job.eq.2) then
	  call ximagemenuhide
	  call ximagelabelhide
	  go to 1000
C*** return main menu
	 else if(job.eq.2) then
	  call ximagemenuhide
	  call ximagelabelhide
	  return
	 end if
	end if
C***********************************************************
C*** refinement successful
C***********************************************************
        nspt = 0
        xdevsq = 0.
        ydevsq = 0.
        do 1700 i=-irang,irang
         do 1700 j=-irang,irang
          ihin=i
          ikin=j
C*** spot was read in
          indx=0
          do jj=1,nspots
           if(ihin.eq.ih(jj).and.ikin.eq.ik(jj)) indx=jj
          end do
          if(indx.eq.0) go to 1700
          xpos=ihin*x1+ikin*x2+x0
          ypos=ihin*y1+ikin*y2+y0
          xdev=xpos-xd(indx)
          ydev=ypos-yd(indx)
          xdevsq=xdevsq+xdev*xdev
          ydevsq=ydevsq+ydev*ydev
          nspt=nspt+1
 1700   continue
C***
        if(nspt .eq. 0) then
         iolabel(nlabels+2) = 
     *   'Indices do not match number of orders requested'
	 call ximagelabeldisplay(iolabel,nlabels+2)
         go to 1500
        end if
C***
        rms=sqrt((xdevsq+ydevsq)/nspt)
        write(iolabel(nlabels+8),'(
     *  '' RMS error for this pass = '',f10.5)') rms
	iolabel(nlabels+9) = 
     *  'To save this data type filename, <cr> otherwise'
 	latfile = ' '
	call ximageioboxdisplay(iolabel,latfile,nlabels+9)
 1800	if(latfile .ne. ' ') then
	 old = .false.
	 call check_file(latfile)
         open(unit=idevlat,file=latfile,status='new',err=1900)
         write(idevlat,'(1x,a)',err=1900) mapfile
	 do n=nlabels+1,nlabels+8
	  write(idevlat,'(a)',err=1900) iolabel(n)
	 end do
	 close(idevlat)
	end if
	go to 2000
C*** error writing output file
 1900   call file_error(latfile,nlabels+8)
	go to 1800
C***********************************************************
C**** set up menu to output refined lattice or modify spots
C***********************************************************
 2000   vbox = .false.
        ibox1 = 0
        ibox2 = 0
	nlabels = 1
	call ximagelabeldisplay(iolabel,nlabels)
        menulist(1) = 'Display lattice with standard size boxes'
        menulist(2) = 'Display lattice with standard size circles'
	menulist(3) = 'Display lattice with variable size boxes'
	menulist(4) = 'Add/remove spots and re-refine'
	menulist(5) = 'Return main menu'
	call ximagemenuinit(menulist,5)
	job = -1
 2500   call ximagewait(job)
	if(job.le.0) then
	 go to 2500
C*** Standard boxes
	else if(job .lt. 3) then
	 call ximagelabelhide
	 call ximagemenuhide
 2600	 iolabel(nlabels+1) = 'Type in symbol size in pixels'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 call extract_integers(1,return_string,
     *   ibox1,i2,i3,i4,i5,i6)
	 if(ibox1 .le. 0) go to 2600
	 isymb = job
	 labeldist = nint(float(ibox1) * 0.5)
	 if(isymb .eq. 2) irad = labeldist
C*** variable boxes
	else if(job .eq. 3) then	
	 call ximagelabelhide
	 call ximagemenuhide
 2700	 iolabel(nlabels+1) = 'Type in box sizes in pixels'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 call extract_integers(2,return_string,
     *   ibox1,ibox2,i3,i4,i5,i6)
	 if(ibox1 .le. 0 .or. ibox2 .le. 0) go to 2700
	 labeldist = nint(float(ibox1 + ibox2) * 0.25)
         ibox2 = (ibox2 / 2) * 2 + 1
	 vbox = .true.
	 isymb = 1
C*** modify spots and re-refine
	else if(job .eq. 4) then
	 if(nspots .eq. 0) then
	  iolabel(nlabels+1) = 'No spots to be modified'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	  go to 2000
	 end if
	 call ximageremovevectors
	 call ximagelabelhide
	 call ximagemenuhide	
	 go to 1000
C*** return main menu
	else if(job .eq. 5) then
	 call ximagelabelhide
	 call ximagemenuhide	
	 call ximageremovevectors
	 return
	end if
C***
C*** end of menu
	ibox1 = max(ibox1,minbox)
	idev = ibox1 / 2
        ibox1 = idev * 2 + 1
C*** remove crosses marking selected spot positions
	do i=1,nspots
	 ixim = nint(xd(i))
	 iyim = nint(yd(i))
	 call convert_to_screen(ixim,iyim,ixscreen,iyscreen)
	 call remove_cross(ixscreen, iyscreen, icross_size)
	end do
C*** if first time round and variable boxes, get tilt axis distance
        if(ifirst) then
         if(vbox) then
	  ifirst = .false.
C*** variable boxes, calculate maximum distance from tilt axis
 2800     iolabel(nlabels+1) = 
     *    ' Type angle(degrees) tilt axis makes with horizontal'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	  call extract_reals(1,return_string,phi,f2,f3,f4,f5,f6)
          phi = phi * radians
          sinphi = sin(phi)
          cosphi = cos(phi)
          distmax = 0.
C***********************************************************
C*** set up menu to control centre specification
C***********************************************************
	  call ximagelabeldisplay(iolabel,nlabels)
	  menulist(1) = 'Select centre with cursor'
	  menulist(2) = 'Auto centre'
	  menulist(3) = 'Return main menu'
	  call ximagemenuinit(menulist,3)
	  job = -1
 3000     call ximagewait(job)
	  if(job.le.0) then
	   go to 3000
C*** use cursor
	  else if(job.eq.1) then
	   call ximagemenuhide
	   iolabel(nlabels+1) = 'Mark cursor position at tilt axis centre'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	   call ximagereadpointer(ixscreen,iyscreen)
	   call convert_to_image(ixscreen,iyscreen,ixcen,iycen)
	   call ximagemenuhide
	   call ximagelabelhide
C*** auto centre
	  else if(job.eq.2) then
           ixcen = nint(x0)
           iycen = nint(y0)
	   call ximagemenuhide
C*** return main menu
	  else if(job.eq.3) then
	   call ximagemenuhide
	   call ximageremovevectors
	   return
	  end if
          fixcen = float(ixcen)
          fiycen = float(iycen)
         end if
C***
         do i=-irang,irang
          do j=-irang,irang
C*** calculate coordinates
           xn_lat(i,j) = i * x1 + j * x2 + x0
           yn_lat(i,j) = i * y1 + j * y2 + y0
           if(vbox) then
            dist_lat(i,j) = 
     *      abs(cosphi*(yn_lat(i,j)-fiycen) - 
     *          sinphi * (xn_lat(i,j)-fixcen))
            distmax = max(dist_lat(i,j),distmax)
           end if
          end do
         end do
        end if
C***
        if(vbox) then
	 call convert_to_screen(ixcen,iycen,ixscreen,iyscreen)
	 fix = float(ixscreen)
	 fiy = float(iyscreen)
         call draw_line(sinphi,cosphi,ixl,iyl,ixr,iyr,ix,iy,fix,fiy)
         nsizes = iabs(ibox2 - ibox1) + 1
         distinc = distmax / nsizes
        end if
C***
        do 3200 i=-irang,irang
         do 3200 j=-irang,irang
          if(vbox) then
           d1 = 0.
           distmin = 10000000.
           do n=1,nsizes
            d2 = abs(d1 - dist_lat(i,j))
            if(d2.lt.distmin) then
             distmin = d2
             l = n
            end if
            d1 = d1 + distinc
           end do
           idev = (ibox1 + l) / 2
          end if
	  iximage = nint(xn_lat(i,j))
	  iyimage = nint(yn_lat(i,j))
	  call convert_to_screen(iximage,iyimage,ixscreen,iyscreen)
	  ixlat(i,j) = ixscreen
	  iylat(i,j) = iyscreen
C*** draw a box
	  if(isymb .eq. 1) then
           kxmin = ixscreen - idev
           kymin = iyscreen - idev
           if(kxmin.le.ixmin .or. kymin.le.iymin) go to 3200
           kxmax = ixscreen + idev
           kymax = iyscreen + idev
           if(kxmax.ge.ixmax .or. kymax.ge.iymax) go to 3200
           call ximagedrawbox(kxmin,kymin,kxmax,kymax)
C*** draw a circle
	  else if(isymb .eq. 2) then
	   call ximagedrawcircle(ixscreen,iyscreen,irad)
	  end if
C*** display indices every 5th index
	  if(mod(i,5) .eq. 0 .and. mod(j,5) .eq. 0) then
	   string = ' '
	   string = intoch(i,nchars)
	   string(nchars+1:nchars+1) = ','
	   return_string = ' '
	   return_string = intoch(j,ichars)
	   string(nchars+2:nchars+ichars+1) = intoch(j,ichars)
	   call ximagedrawtext(ixscreen+labeldist,iyscreen+labeldist,string)
	  end if
3200    continue
C***********************************************************
C*** lattice vectors read in for viewing only
C***********************************************************
	if(nspots .eq. 0) then
	 menulist(1) = 'Refine another lattice'
	 menulist(2) = 'Return main menu'
         call ximagemenuinit(menulist,2)
	 job = -1
 3400    call ximagewait(job)
	 if(job.le.0) then
	  go to 3400
C*** refine another lattice
 	 else if(job .eq. 1) then
	  call ximagemenuhide
	  call ximagelabelhide
	  go to 600
C*** return main menu
	 else if(job .eq. 2) then
	  call ximagemenuhide
	  call ximageremovevectors
	  return
	 end if
	end if
C***********************************************************
C*** Lattice displayed, how to proceed ?
C***********************************************************
	call ximagelabeldisplay(iolabel,nlabels)
	menulist(1) = 'Save lattice vectors to file'
	menulist(2) = 'Change symbol sizes'
	menulist(3) = 'Add/remove spots and re-refine'
	menulist(4) = 'Refine another lattice'
	menulist(5) = 'Return main menu'
        call ximagemenuinit(menulist,5)
	job = -1
 4000   call ximagewait(job)
	if(job .le. 0) then
	 go to 4000
C*** keep symbol sizes
        else if(job .eq. 1) then
	 call ximagemenuhide
	 go to 5000
C*** change symbol sizes
        else if(job .eq. 2) then
	 call ximagemenuhide
	 call ximageremovevectors
	 go to 2000
C*** modify spots and re-refine
	else if(job .eq. 3) then
C*** remove only current lattice
         do 4200 i=-irang,irang
          do 4200 j=-irang,irang
	   ixscreen = ixlat(i,j)
	   iyscreen = iylat(i,j)
C*** draw a box
	   if(isymb .eq. 1) then
            kxmin = ixscreen - idev
            kymin = iyscreen - idev
            if(kxmin.le.ixmin .or. kymin.le.iymin) go to 4200
            kxmax = ixscreen + idev
            kymax = iyscreen + idev
            if(kxmax.ge.ixmax .or. kymax.ge.iymax) go to 4200
            call ximageremovebox(kxmin,kymin,kxmax,kymax)
C*** draw a circle
	   else if(isymb .eq. 2) then
	    call ximageremovecircle(ixscreen,iyscreen,irad)
	   end if
C*** remove indices 
	   if(mod(i,5) .eq. 0 .and. mod(j,5) .eq. 0) then
	    string = ' '
	    string = intoch(i,nchars)
	    string(nchars+1:nchars+1) = ','
	    return_string = ' '
	    return_string = intoch(j,ichars)
	    string(nchars+2:nchars+ichars+1) = intoch(j,ichars)
	    call ximageremovetext(ixscreen+labeldist,iyscreen+labeldist,string)
	   end if
4200     continue
	 call ximagelabelhide
	 call ximagemenuhide	
	 go to 1000
C*** refine another lattice
	else if(job .eq. 4) then
	 call ximagemenuhide
	 call ximagelabelhide
	 go to 600
C*** return main menu
        else if(job .eq. 5) then
	 call ximagemenuhide
	 call ximageremovevectors
	 return
	end if
C***
C*** send lattice vectors to output file
 5000   iolabel(nlabels+1) = 'Type output filename...'
	latfile = ' '
	call ximageioboxdisplay(iolabel,latfile,nlabels+1)
 5200   old = .false.
	call check_file(latfile)
        open(unit=idevlat,file=latfile,status='new',err=5500)
        write(idevlat,'(1x,a)',err=5500) mapfile
	go to 6000
C*** error writing output file
 5500   call file_error(latfile,nlabels)
	go to 5200
C*** scale factor option for RH group as they compress their maps
C*** to get whole map on screen     JMS 04.05.87
 6000   iolabel(nlabels+1) = 
     *  'Type in scale factor for o/p parameters (default=1)'
	return_string = ' '
 	call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	if(return_string .eq. ' ') then
	 iscfac = 1
	else
	 call extract_integers(1,return_string,
     *   iscfac,i2,i3,i4,i5,i6)
	end if
        scfac = float(iscfac)
        numx = nxyz(1) * iscfac
        numy = nxyz(2) * iscfac
        x1 = x1 * scfac
        y1 = y1 * scfac
        x2 = x2 * scfac
        y2 = y2 * scfac
C***********************************************************
C*** calculate centre for image
C***********************************************************
        if(image) then
         cenx = x0 * scfac
         ceny = y0 * scfac
         ecenx = cenx - xorigin * scfac
         eceny = ceny - yorigin * scfac
C*** calculate centre for transform
        else
         cenx = 0.
         ceny = 0.
         ecenx = (x0 - xorigin) * scfac
         eceny = (y0 - yorigin) * scfac
        end if
C*** output to lattice file
        write(idevlat,'(
     *   2i10,'' Array size (for refinement)''/
     *          4f10.4,'' Lattice vectors''/
     *          2f12.4,'' Lattice centre (in image)''/
     *          2f12.4,'' Error from centre''/
     *          2i10,  '' Symbol sizes in pixels'')',err=5500)
     *  numx,numy,x1,y1,x2,y2,cenx,ceny,ecenx,eceny,ibox1,ibox2
C***
C*** write coords/indices used for refinement
	if(nspots .ge. 1) then
         write(idevlat,'('' Spot number  H    K      X       Y'')'
     *   ,err=5500)
         do kk=1,nspots
          x = xd(kk) * scfac
          y = yd(kk) * scfac
          write(idevlat,'(1x,i10,2i5,2f8.1)',err=5500) 
     *    kk, ih(kk), ik(kk), x, y
         end do
	end if
	close(idevlat)
	write(iolabel(nlabels+1),'(''Lattice vectors written to :'',a)')
     *  latfile(1:lnblank(latfile))
	call ximagelabeldisplay(iolabel,nlabels+1)
	menulist(1) = 'Calculate another lattice'
	menulist(2) = 'return main menu'
	call ximagemenuinit(menulist,2)
	job = -1
 7000   call ximagewait(job)	
	if(job .le. 0) go to 7000
	if(job .eq. 1) then
	 call ximagemenuhide
	 call ximagelabelhide
	 call ximageremovevectors
	 go to 900
	else if(job .eq. 2) then
	 call ximagemenuhide
	 call ximagelabelhide
	 call ximageremovevectors
	 return
	end if
        end
C***************************************************************
C***
        subroutine lattice_display
C***
C***************************************************************
C*** subroutine to display spots selected for lattice refinement
C***
	include 'Ximdisp_common.for'
	nlabels = 1
	ilabel = nspots - max_lines_per_page + 2
        do i=1,nspots
	 ixim = nint(xd(i))
	 iyim = nint(yd(i))
	 call convert_to_screen(ixim,iyim,ixscreen,iyscreen)
	 call draw_cross(ixscreen,iyscreen,icross_size)
	 if(i .gt. ilabel) then
	  nlabels = nlabels + 1
          write(iolabel(nlabels),'(
     *    '' Spot number='',i3,'' h='',i3,'' k='',i3,
     *    '' X='',i6,'' Y='',i6)') i,ih(i),ik(i),ixim,iyim
	 end if
        end do
	call ximagelabeldisplay(iolabel,nlabels)
	return
	end
C***************************************************************
C***
        subroutine lattice_read
C***
C***************************************************************
C*** subroutine to read positions & indices of spots
C*** for lattice refinement
C***
	include 'Ximdisp_common.for'
C***
	call ximagemenuhide
C***
C*** start new menu
C***
	ierr = 0
	nlabels = 1
	if(nspots.gt.0) call lattice_display
  100   iolabel(nlabels+1) = 
     *  'Mark next spot position with lh mouse button'
	call ximagelabeldisplay(iolabel,nlabels+1)
  150	menulist(1) = 'Mark spot for deletion'
	menulist(2) = 'Edit spot indices'
	menulist(3) = 'Hide zoom area'
	menulist(4) = 'Save spots to a file'
	menulist(5) = 'Calculate a lattice'
	menulist(6) = 'Refresh vectors'
	menulist(7) = 'Return main menu'
	call ximagemenuinit(menulist,7)
	job = -1
  200   call ximagewait(job)
	if(job.lt.0) then
	 go to 200
C***************************************************
C*** read spot position, and indices
C***************************************************
	else if(job.eq.0) then
	 call ximagelabelhide
	 call ximagemenuhide
	 nspots = nspots + 1
C*** too many points, error
         if(nspots .gt. max_points) then 
	  write(iolabel(nlabels+1),'(
     *    '' Too many points entered, will proceed with'',i4)') 
     *    max_points
 	  call ximagelabeldisplay(iolabel,nlabels+1)
	  call lattice_display
	  return
	 end if
C*** read cursor position
	 call ximagereadmenupointer(ixscreen, iyscreen)
	 call draw_cross(ixscreen,iyscreen,icross_size)
C*** correct y position so that origin bottom left
	 call convert_to_image(ixscreen,iyscreen,ixim,iyim)
         xd(nspots) = float(ixim)
         yd(nspots) = float(iyim)
  300	 iolabel(nlabels+1) = 'Type h,k indices for this spot'
  350    return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 call extract_integers(2,return_string,
     *   ih(nspots),ik(nspots),i3,i4,i5,i6)
C*** check indices not already specified
	 do n=1,nspots-1
	  if(ih(n) .eq. ih(nspots) .and. ik(n) .eq. ik(nspots)) then
	   write(iolabel(nlabels+1),
     *     '(''Indices '',2i5,'' already used, please retype'')')
     *     ih(nspots), ik(nspots)
	   go to 350
	  end if
	 end do
C*** spot ok, include in list, shuffle label lines if necessary
	 nlabels = nlabels + 1
	 if(nlabels .eq. max_lines_per_page) then
	  do n=2,max_lines_per_page-2
	   iolabel(n) = iolabel(n+1)
	  end do
	  nlabels = max_lines_per_page - 1
	 end if
         write(iolabel(nlabels),'(
     *   '' Spot number='',i3,'' h='',i3,'' k='',i3,
     *   '' X='',i6,'' Y='',i6)') 
     *   nspots,ih(nspots),ik(nspots),ixim,iyim
	 go to 100
C***************************************************
C*** delete next spot
C***************************************************
	else if(job.eq.1) then
	 call ximagelabelhide
	 call ximagemenuhide
  	 iolabel(nlabels+1) = 'Type indices of spot to be deleted'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 call extract_integers(2,return_string,
     *   jh,jk,i3,i4,i5,i6)
C*** remove point from list
	 do i=1,nspots
	  if(ih(i).eq.jh .and. ik(i).eq.jk) then
	   ixim = nint(xd(i))
	   iyim = nint(yd(i))
	   call convert_to_screen(ixim,iyim,ixscreen,iyscreen)
	   call remove_cross(ixscreen, iyscreen, icross_size)
	   nspots = nspots - 1
	   do j=i,nspots
	    ih(j) = ih(j+1)
	    ik(j) = ik(j+1)
	    xd(j) = xd(j+1)
	    yd(j) = yd(j+1)
	   end do
	   go to 400
	  end if
	 end do
	 write(iolabel(nlabels+1),'('' Spot with indices '',2i5,
     *   '' not found in list for refinement'')') jh, jk
  400    call lattice_display
	 go to 100
C***********************************************************
C*** Edit spot indices
C***********************************************************
	else if(job .eq. 2) then
	 call ximagemenuhide
	 call ximagelabelhide
	 iolabel(nlabels+1) = 'Type indices to be edited'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 call extract_integers(2,return_string,jh,jk,i3,i4,i5,i6)
	 iolabel(nlabels+1) = 'Type new indices'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 call extract_integers(2,return_string,lh,lk,i3,i4,i5,i6)
	 do i=1,nspots
	  if(ih(i) .eq. jh .and. ik(i) .eq. jk) then
	   ih(i) = lh
	   ik(i) = lk
	   go to 500
	  end if
	 end do
  500  	 write(iolabel(nlabels+1),
     *   '(''Indices changed from '',2i3,'' to'',2i3)') jh, jk, lh, lk
    	 call lattice_display
	 call ximagemenudisplay
	 go to 100
C***********************************************************
C*** hide zoom area
C***********************************************************
	else if(job.eq.3) then
	 call ximagezoomhide
	 go to 200
C***********************************************************
C*** save spots to a file
C***********************************************************
	else if(job .eq. 4) then
	 nlabels = 1
	 call ximagemenuhide
  600    iolabel(nlabels+1) = 'Type output filename'
	 latfile = ' '
	 call ximageioboxdisplay(iolabel,latfile,nlabels+1)
  700    old = .false.
	 call check_file(latfile)
	 open(unit=idevlat,file=latfile,status='new',err=800)
	 do i=1,nspots
	  write(idevlat,*,err=800)  ih(i), ik(i), xd(i), yd(i)
	 end do 
	 close(idevlat)
	 write(iolabel(nlabels+1),'(''Spot data written to '',a)')
     *   latfile(1:lnblank(latfile))
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 go to 150
C*** error writing file
  800    call file_error(latfile,nlabels)
C***********************************************************
C*** finished spot selection, return to lattice
C***********************************************************
	else if(job .eq. 5) then
	 call ximagelabelhide
	 call ximagemenuhide
	 return
C***********************************************************
C*** refresh vectors
C***********************************************************
	else if(job .eq. 6) then
	 call ximageredrawvectors
	 go to 150
C***********************************************************
C*** return main menu
C***********************************************************
	else if(job .eq. 7) then
	 call ximagelabelhide
	 call ximagemenuhide
	 ierr = 1
	 return
	end if
        end
C************************************************************************
C***
	subroutine lattice_refine
C***
C************************************************************************
C*** subroutine to carry out lattice refinement
C***
C***  variables passed to it are:
C***  IH,IK indices of chosen spots
C***  XD,YD coordinates of chosen spots
C***
C***  variables returned:
C***  X0,Y0 coordinates of origin
C***  X1,Y1     "       "  (1,0)
C***  X2,Y2     "       "  (0,1)
C***  ALN, BLN lengths of sides of lattice
C***
        include 'Ximdisp_common.for'
C***
C*** check that there are enough reflections to use
	nlabels = 1
	if(nspots.le.2) then
         iolabel(nlabels+1) = 
     *   ' Less than 3 reflections found, refinement impossible'
       	 refined=.false.
      	 return
	end if
C*** proceed
	s=0.
	sh=0.
	sk=0.
	sx=0.
	sy=0.
	shh=0.
	skk=0.
	shk=0.
	sxh=0.
	sxk=0.
	syh=0.
	syk=0.
	do j=1,nspots
         fih = float(ih(j))
         fik = float(ik(j)) 
         s=s+1.0
         sh=sh+fih
         sk=sk+fik
         sx=sx+xd(j)
         sy=sy+yd(j)
         shh=shh+fih*fih
         skk=skk+fik*fik
         shk=shk+fih*fik
         sxh=sxh+xd(j)*fih
         sxk=sxk+xd(j)*fik
         syh=syh+yd(j)*fih
         syk=syk+yd(j)*fik
        end do
C***
C*** compute denominator for differences
        shsk = sh*sk-s*shk
        shsksq = shsk * shsk
        sksk = sk*sk-s*skk
        shsh = sh*sh-s*shh
	bottom = shsksq - sksk * shsh
C*** refinement failed
	if(bottom.eq.0.0) go to 500
C***
        sxsh = sx*sh-s*sxh
        sxsk = sx*sk-s*sxk
        sysk = sy*sk-s*syk
        sysh = sy*sh-s*syh
	x1=(sxsk * shsk - sxsh * sksk) / bottom
	x2=(sxsh * shsk - sxsk * shsh) / bottom
	x0=(sx-sk*x2-sh*x1)/s
	y1=(sysk * shsk - sysh * sksk) / bottom
	y2=(sysh * shsk - sysk * shsh) / bottom
	y0=(sy-sk*y2-sh*y1)/s
	write(iolabel(nlabels+1),
     *  '('' Origin, lattice errors:'' ,6f8.1)')
     *  x0, y0, x1, y1, x2, y2
C***
C*** figure out angle between axes and print this inf to user
        shx=0.
        skx=0.
        shy=0.
        sky=0.
	do j=1,nspots
         fih = float(ih(j))
         fik = float(ik(j)) 
         shx=shx+(xd(j)-x0)*fih
	 skx=skx+(xd(j)-x0)*fik
	 shy=shy+(yd(j)-y0)*fih
	 sky=sky+(yd(j)-y0)*fik
        end do
        denom=shk*shk-shh*skk
        if(denom.eq.0.) go to 500
        ax1 = (skx*shk-shx*skk) / denom
        ax2 = (shx*shk-skx*shh) / denom
        ay1 = (sky*shk-shy*skk) / denom
        ay2 = (shy*shk-sky*shh) / denom
        write(iolabel(nlabels+2),'(
     *  ''  Lattice dimensions :'',2f10.3,2f10.3)') 
     *  ax1, ay1, ax2, ay2
        bln = sqrt(ax2*ax2+ay2*ay2)
        aln = sqrt(ax1*ax1+ay1*ay1)
        aang = radcon * atan2(ay1,ax1)
        bang = radcon * atan2(ay2,ax2)
        difng = amin1(abs(aang-bang),abs(aang-bang+360.))
        difng = amin1(difng,abs(360.-difng))
        write(iolabel(nlabels+3),'(
     *  ''   Length of A'',f10.3,'' RLU '')') aln
        write(iolabel(nlabels+4),'(
     *  '' Angle '',f10.3,'' Degrees'')') aang
        write(iolabel(nlabels+5),'(
     *  '' Length of B'',f10.3,'' RLU '')') bln
        write(iolabel(nlabels+6),'(
     *  '' Angle  '',f10.3,'' Degrees '')') bang
        write(iolabel(nlabels+7),'(
     *  '' Included angle '',f10.3,'' Degrees'')') difng
      	refined=.true.
	return
C***
C*** diagnostic
  500	iolabel(nlabels+1) = ' Selection of indices too symmetric'
	refined=.false.
      	return
	end
C********************************************************************
C***
        subroutine output_coords
C***
C********************************************************************
C***
C*** subroutine to read cursor coords, optionally draw numbered
C*** boxes around them &/or write them to an output file
        include    'Ximdisp_common.for'
C***
	nlabels = 1
	nspot = 0
	call ximagelabelhide
	call ximagemenuhide
	standard = .false.
	spider = .false.
	imagic = .false.
	stack = .false.
	autolabel = .false.
C*** return if multisection map
	if(noz .gt. 1) then
	 nlabels = nlabels + 1
	 iolabel(nlabels) = 'Warning - this option cannot find map values'
	end if
C*********************************************************************
C*** select symbol type
C*********************************************************************
	iolabel(nlabels+1) = 'Select symbol type'
	call ximagelabeldisplay(iolabel,nlabels+1)
        menulist(1) = 'Square box'
        menulist(2) = 'Square box with numbers'
	menulist(3) = 'Circle'
	menulist(4) = 'Circle with numbers'
	menulist(5) = 'Point'
	menulist(6) = 'Point with numbers'
	menulist(7) = 'Return main menu'
	call ximagemenuinit(menulist,7)
	job = -1
  100   call ximagewait(job)
	if(job .le. 0) then
	 go to 100
C*** square box
	else if(job .eq. 1) then
	 isymb = 1
C*** square box with numbers
	else if(job .eq. 2) then
	 isymb = 1
	 autolabel = .true.
C*** circle
	else if(job .eq. 3) then
	 isymb = 2
C*** circle with numbers
	else if(job .eq. 4) then
	 isymb = 2
	 autolabel = .true.
C*** point
	else if(job .eq. 5) then
	 isymb = 3
C*** point with numbers
	else if(job .eq. 6) then
	 isymb = 3
	 autolabel = .true.
C*** return main menu
	else
	 call ximagelabelhide
	 call ximagemenuhide
	 return
	end if
C**********************************************************************
C*** select format
C**********************************************************************
	iolabel(nlabels+1) = 'Select format'
	call ximagelabeldisplay(iolabel,nlabels+1)
	nlabels = 2
        menulist(1) = 'Screen only'
	menulist(2) = 'Standard format'
	menulist(3) = 'Spider format'
	menulist(4) = 'Imagic format'
	menulist(5) = 'Measure vector lengths'
	menulist(6) = 'Return main menu'
	call ximagemenuinit(menulist,6)
	job = -1
  200   call ximagewait(job)
	if(job .le. 0) then
	 go to 200
C*** coords to screen
	else if(job .eq. 1) then
	 call ximagemenuhide
	 call ximagelabelhide
         iolabel(nlabels) = 
     *  'Mark cursor positions with left hand mouse button'
	 call ximagelabeldisplay(iolabel,nlabels)
	 go to 300
C*** standard format
	else if(job .eq. 2) then
	 standard = .true.
C*** spider format
	else if(job .eq. 3) then
	 spider = .true.
C*** imagic format
	else if(job .eq. 4) then
	 imagic = .true.
C*** measure vector lengths
	else if(job .eq. 5) then
	 call ximagemenuhide
	 call ximagelabelhide
	 call output_vectors
	 return
C*** return main menu
	else if(job .eq. 6) then
	 call ximagemenuhide
	 return
	end if
	call ximagelabelhide
	call ximagemenuhide
C**********************************************************************
C*** create stack of boxes ?
C**********************************************************************
	call ximagelabeldisplay(iolabel,nlabels)
	menulist(1) = 'Create a stack of boxes in MRC image format'
	menulist(2) = 'Do not create a stack'
	menulist(3) = 'Return main menu'
	call ximagemenuinit(menulist,3)
	job = -1
  205   call ximagewait(job)
	if(job .le. 0) then
	 go to 205
C*** stack of boxes
	else if(job .eq. 1) then
	 call ximagemenuhide
	 stack = .true.
  	 iolabel(nlabels) = 'Type output stack file name ...'
  210    outputfile = ' '
	 call ximageioboxdisplay(iolabel,outputfile,nlabels)
C*** filename already present, read from it ?
         there = .false.
         inquire(file=outputfile,exist=there)
	 if(there) then
	  iolabel(nlabels) = 'File already present, retype file name...'
	  go to 210
	 end if
	 iolabel(nlabels) = 'Type in box size in pixels'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels)
	 call extract_integers(1,return_string,iboxsiz,i2,i3,i4,i5,i6)
C*** return main menu
	else if(job .eq. 3) then
	 call ximagelabelhide
	 call ximagemenuhide
	 return
	end if
	call ximagelabelhide
	call ximagemenuhide
C**********************************************************************
C*** open coordinate file, read coords if existing file
C**********************************************************************
  215	iolabel(nlabels) = 'Coordinate file name ...'
	coordfile = ' '
	call ximageioboxdisplay(iolabel,coordfile,nlabels)
C*** filename already present, read from it ?
        there = .false.
        inquire(file=coordfile,exist=there)
C*** open existing coord file
	if(there) then
         write(iolabel(nlabels),'
     *   (''Reading from an existing file, saving it as coords.sav'')')
         call system('cp '//coordfile(1:lnblank(coordfile))//
     *   ' coords.sav')
	 nlabels = nlabels + 1
         iolabel(nlabels) = 
     *   'Mark cursor positions with left hand mouse button'
	 call ximagelabeldisplay(iolabel,nlabels)
	 open(unit=idevcoord,file=coordfile(1:lnblank(coordfile)),
     *        status='old',err=215)
C*** read first record of spider or standard
	 if(standard .or. spider) read(idevcoord,'(a)') return_string
C*******************************************************************
C*** read coords now
C*******************************************************************
C*** standard format
	 if(standard) then
  220     read(idevcoord,'(2i10,f12.1)',end=270) ixim, iyim, den
	  nspot = nspot + 1
	  if(nspot .gt. max_coords) then
	   return_string = ' '
	   iolabel(nlabels) = 
     *     'Too many spots for program, <cr> to return main menu'
	   call ximageioboxdisplay(iolabel,return_string,nlabels)
	   close(idevcoord)
	   return
	  end if
	  ixcoord(nspot) = ixim
	  iycoord(nspot) = iyim
	  density(nspot) = den
	  go to 220
C*** spider format
	 else if(spider) then
  230     read(idevcoord,'(i5,i2,2f12.6)',end=270) nspot, i, xim,yim
	  if(nspot .gt. max_coords) then
	   return_string = ' '
	   iolabel(nlabels) = 
     *     'Too many spots for program, <cr> to return main menu'
	   call ximageioboxdisplay(iolabel,return_string,nlabels)
	   close(idevcoord)
	   return
	  end if
	  ixcoord(nspot) = nint(xim)
	  iycoord(nspot) = nxyz(2) - nint(yim)
	  go to 230
C*** imagic format
	 else if(imagic) then
  240     read(idevcoord,*,end=270) xim,yim
	  nspot = nspot + 1
	  if(nspot .gt. max_coords) then
	   return_string = ' '
	   iolabel(nlabels) = 
     *     'Too many spots for program, <cr> to return main menu'
	   call ximageioboxdisplay(iolabel,return_string,nlabels)
	   close(idevcoord)
	   return
	  end if
C*** imagic coordinate system horiz +ve y to right, vertical +ve x down
	  ixcoord(nspot) = nint(yim)
	  iycoord(nspot) = nxyz(2) - nint(xim)
	  go to 240
	 end if
C*********************************************************************
C*** open new file for coordinates
C*********************************************************************
	else
  250    iolabel(nlabels) = 'Opening a new file...'
	 nlabels = nlabels + 1
         iolabel(nlabels) = 
     *   'Mark cursor positions with left hand mouse button'
	 call ximagelabeldisplay(iolabel,nlabels)
	 open(unit=idevcoord,
     *        file=coordfile(1:lnblank(coordfile)),
     *        status='new',err=260)
C*** standard format
	 if(standard) then
	  if(image) then
	   write(idevcoord,'(''        x         y      density'')',
     *     err=260)
	  else
	   write(idevcoord,'(
     *     ''        x         y    intensity  amplitude  phase'')',
     *     err=260)
	  end if
C*** spider format
	 else if(spider) then
	  write(idevcoord,'('' ; File output from Ximdisp : '',a)',
     *    err=260) 
     *    mapfile(1:lnblank(mapfile))
	 end if
	 go to 300
  260    call file_error(coordfile,nlabels-1)
	 go to 250
	end if
	go to 300
C*******************************************************************
C*** all coords read in, decompress them and write to screen
C*******************************************************************
  270   do n=1,nspot	  
	 call convert_to_screen(ixcoord(n),iycoord(n),ixscreen,iyscreen)
C*** draw box around spot
         maxx = ixscreen + ibox_size
         minx = ixscreen - ibox_size
         maxy = iyscreen + ibox_size
         miny = iyscreen - ibox_size
	 if(isymb .eq. 1) then
          call ximagedrawbox(minx,miny,maxx,maxy)
	 else if(isymb .eq. 2) then
	  call ximagedrawcircle(ixscreen,iyscreen,ibox_size)
	 else if(isymb .eq. 3) then
	  call ximagedrawpoint(ixscreen,iyscreen)
	 end if
C*** draw number beside it if switch set
	 if(autolabel) then
	  string = ' '
	  string = intoch(n,nchars)
	  call ximagedrawtext(maxx,maxy,string)
	 end if
C*** prepare label
	 nlabels = nlabels + 1
C*** spot ok, include in list, shuffle label lines if necessary
C*** leave space for labels for contrast mods
	 if(nlabels .eq. max_lines_per_page - 2) then
	  do i=4,max_lines_per_page-5
	   iolabel(i) = iolabel(i+2)
	  end do
	  nlabels = max_lines_per_page - 4
	 end if
 	 write(iolabel(nlabels),'(''Screen coordinates = '',2i6)')
     *   ixscreen,iyscreen
	 nlabels = nlabels + 1
	 if(image) then
	  write(iolabel(nlabels),'(''Map coordinates = '',2i6,
     *    '' density = '',f8.1)') ixcoord(n),iycoord(n),density(n)
	 else
	  den = density(n)
	  amplitude = sqrt(den)
	  write(iolabel(nlabels),'(''Transform origin f(0,0) = '',2i5,
     *    '' int,amp = '',2f10.1)') ixcoord(n),iycoord(n),den,amplitude
	 end if
	end do
C************************************************************************
C*** close and re-open file in case any already in file are to be deleted
C************************************************************************
	close(idevcoord)
  280   open(unit=idevcoord,
     *       file=coordfile(1:lnblank(coordfile)),status='unknown',
     *       err=290)
	if(spider) then
	 write(idevcoord,'('' ; File output from Ximdisp : '',a)'
     *   ,err=290)  mapfile(1:lnblank(mapfile))
	else if(standard) then
	 if(image) then
	  write(idevcoord,
     *    '(''        x         y      density'')',err=290)
	 else
	  write(idevcoord,'(
     *    ''        x         y    intensity  amplitude  phase'')',
     *   err=290)
	 end if
	end if
	go to 300
  290   call file_error(coordfile,nlabels)
	go to 280
C****************************************************************************
C*** all spots from previously created files displayed, add new ones
C****************************************************************************
  300	call ximagelabeldisplay(iolabel,nlabels)
	menulist(1) = 'Hide zoom area'
	menulist(2) = 'Delete last position'
	menulist(3) = 'Modify contrast'
	menulist(4) = 'Return main menu'
	call ximagemenuinit(menulist,4)
	job = -1
  500   call ximagewait(job)
	if(job .lt. 0) then
	 go to 500
C*** read coord position
	else if(job .eq. 0) then
	 call ximagereadmenupointer(ixscreen,iyscreen)
	 maxwidth = max(max_screen_width,max_display_width)
	 maxheight = max(max_screen_height,max_display_height)
	 if(ixscreen .lt. 0 .or. ixscreen .gt. maxwidth .or.
     *      iyscreen .lt. 0 .or. iyscreen .gt. maxheight)
     *   then
	  iolabel(nlabels) = 'Out of bounds, try again!'
	  call ximagelabeldisplay(iolabel,nlabels)
	  go to 500
	 end if
C*** draw point on spot
	 call ximagedrawpoint(ixscreen,iyscreen)
C*** draw box around spot
         maxx = ixscreen + ibox_size
         minx = ixscreen - ibox_size
         maxy = iyscreen + ibox_size
         miny = iyscreen - ibox_size
	 if(isymb .eq. 1) then
          call ximagedrawbox(minx,miny,maxx,maxy)
	 else if(isymb .eq. 2) then
	  call ximagedrawcircle(ixscreen,iyscreen,ibox_size)
	 else if(isymb .eq. 3) then
	  call ximagedrawpoint(ixscreen,iyscreen)
	 end if
C*** draw number beside it
         nspot = nspot + 1
	  if(nspot .gt. max_coords) then
	   return_string = ' '
	   iolabel(nlabels) = 
     *     'Too many spots for program, <cr> to return main menu'
	   call ximageioboxdisplay(iolabel,return_string,nlabels)
	   close(idevcoord)
	   return
	  end if
	 if(autolabel) then
	  string = ' '
	  string = intoch(nspot,nchars)
	  call ximagedrawtext(maxx,maxy,string)
	 end if
	 call convert_to_image(ixscreen,iyscreen,ixim,iyim)
C*** read pixel colour !!! is this now correct ???
	 ixycol = (max_display_height - iyscreen) * max_display_width
     *           + ixscreen
C*** check for out of bounds
	 if(ixycol .lt. 0 .or. ixycol .ge. screen_size) then
	  iolabel(nlabels) = 'Out of bounds, try again!'
	  call ximagelabeldisplay(iolabel,nlabels)
	  go to 500
	 end if
	 ipixcol = mapbuf(ixycol)
C*** read map density from image
	 if(image) then
	  if(ixim .lt. nxstart .or. ixim .gt. nxend .or.
     *      iyim .lt. nystart .or. iyim .gt. nyend) then
	   den = 0.
	  else
C*** only read from map if 1 section displayed
	   if(noz .eq. 1) then
            call imposn(idevmap,nzstart,iyim)
	    call irdlin(idevmap,aline)
	    den = aline(ixim+1)
	   end if
	  end if
C*** read map density from transform
	 else
	  call convert_to_transform(ixim,iyim,ixtrans,iytrans)
	  call imposn(idevmap,0,iytrans)
	  call irdlin(idevmap,aline)
C*** -ve shift if bottom right/top left +ve otherwise
	  left = isign(1,ixim*iyim)
C*** multiply by sign to make -ve if from bottom right(or top left) of transform
	  pshift = delpx * abs(ixim) * left + delpy * abs(iyim)
	  apart = aline(ixtrans)
	  bpart = aline(ixtrans+1)
	  call fftextract(apart,bpart)
	  den = amplitude * amplitude
	  phaseangle(nspot) = phase
	 end if
C*** store coords and density
	 ixcoord(nspot) = ixim
	 iycoord(nspot) = iyim
	 density(nspot) = den
C***
C*** shuffle label lines if necessary
	 nlabels = nlabels + 1
	 if(nlabels .ge. max_lines_per_page - 2) then
	  do i=4,max_lines_per_page-5
	   iolabel(i) = iolabel(i+2)
	  end do
	  nlabels = max_lines_per_page - 4
	 end if
         if(image) then
  	  write(iolabel(nlabels),'(''Screen coordinates = '',2i6,
     *                             '' colour  ='',i4)')
     *    ixscreen,iyscreen,ipixcol
	  if(noz .eq. 1) then
	   nlabels = nlabels + 1
	   write(iolabel(nlabels),'(''Map coordinates = '',2i6,
     *     '' density = '',f8.1)') ixim,iyim,den
	  end if
         else
  	  write(iolabel(nlabels),'(''Screen coordinates = '',2i6,
     *                             '' colour  ='',i4)')
     *    ixscreen,iyscreen,ipixcol
	  nlabels = nlabels + 1
	  write(iolabel(nlabels),'(''Transform origin f(0,0) = '',2i5,
     *    '' int,amp,phase = '',f12.0,f8.0,f6.0)') 
     *    ixim,iyim,den,amplitude,phase
         end if
	 call ximagelabeldisplay(iolabel,nlabels)
	 go to 500
C*** hide zoom area
	else if(job .eq. 1) then
	 call ximagezoomhide
	 go to 500
C*** delete previous spot
	else if(job .eq. 2) then
	 ixim = ixcoord(nspot)
	 iyim = iycoord(nspot)
	 call convert_to_screen(ixim,iyim,ixscreen,iyscreen)
C*** remove point on spot
	 call ximageremovepoint(ixscreen,iyscreen)
C*** remove box around spot
         maxx = ixscreen + ibox_size
         minx = ixscreen - ibox_size
         maxy = iyscreen + ibox_size
         miny = iyscreen - ibox_size
	 if(isymb .eq. 1) then
C*** remove box and number from screen
          call ximageremovebox(minx,miny,maxx,maxy)
	 else if(isymb .eq. 2) then
	  call ximageremovecircle(ixscreen,iyscreen,ibox_size)
	 else if(isymb .eq. 3) then
	  call ximageremovepoint(ixscreen,iyscreen)
	 end if
	 if(autolabel) then
	  string = ' '
	  string = intoch(nspot,nchars)
	  call ximageremovetext(maxx,maxy,string)
	 end if
C*** redisplay label
	 nlabels = nlabels - 2
	 call ximagelabeldisplay(iolabel,nlabels)
	 nspot = nspot - 1
	 go to 500	
C*** change colour contrast with slider bar
	else if(job .eq. 3) then
	 call ximagemenuhide
	 call ximagelabelhide
	 call colour_contrast
	 go to 300
C*** return main menu
	else if(job .eq. 4) then
	 call ximagemenuhide
	 call ximagelabelhide
C**********************************************************
C*** write output stack
C**********************************************************
	 if(nspot .gt. 0 .and. stack) then        
	  call imopen(idevout,outputfile,'new')
          ixyz(1) = iboxsiz
          ixyz(2) = iboxsiz
          ixyz(3) = nspot
          kxyz(1) = iboxsiz
          kxyz(2) = iboxsiz
          kxyz(3) = nspot
C*** set output mode
	  imode = mode
          call icrhdr(idevout,ixyz,kxyz,imode,title,0)
          call itrlab(idevout,1)
	  call getdate(date,nsecs)
	  title = 'Ximdisp created stack from coordinates'//date(5:24)
          boxmin = bignum
          boxmax = 0.
          boxmean = 0.
	  call imposn(idevout,0,0)
C*** box areas and write to file
	  ihbox = iboxsiz / 2
	  do n=1,nspot
	   ixstart = ixcoord(n) - ihbox
	   ixend = ixstart + iboxsiz - 1
	   iystart = iycoord(n) - ihbox
	   iyend = iystart + iboxsiz - 1
	   do iy=iystart,iyend
C*** if part of box outside map, pad with blanks
	    if(iy .lt. 0 .or. iy .gt. nxyz(2)) then
	     do ix=1,iboxsiz
	      aline(ix) = 0.
	     end do
	     go to 550
	    end if
	    call imposn(idevmap,0,iy)
	    call irdlin(idevmap,aline)
	    do ix=ixstart,ixend
	     if(ix .lt. 1 .or. ix .gt. nxyz(1)) then
	      den = 0.
	     else
	      den = aline(ix)
	     end if
	     density(ix-ixstart+1) = den
	     boxmin = min(boxmin,den)
	     boxmax = max(boxmax,den)
	     boxmean = boxmean + den
	    end do
  550	    call iwrlin(idevout,density)
	   end do
	  end do
	  boxmean = boxmean / float(nspot * iboxsiz * iboxsiz)
          call iwrhdr(idevout,title,1,boxmin,boxmax,boxmean)
	  call imclose(idevout)
	 end if
C**********************************************************
C*** write standard format spots
C**********************************************************
  600    if(standard) then
	  if(image) then
	   do n=1,nspot
	    write(idevcoord,'(2i10,f12.1)',err=700)
     *      ixcoord(n),iycoord(n),density(n)
	   end do
	  else
	   do n=1,nspot
            write(idevcoord,'(2i10,3f10.1)',err=700)
     *      ixcoord(n),iycoord(n),density(n),sqrt(density(n)),
     *      phaseangle(n)
	   end do
	  end if
C**********************************************************
C*** write spider format spots
C**********************************************************
	 else if(spider) then
	  do n=1,nspot
	   xim = float(ixcoord(n)) 
	   yim = nxyz(2) - float(iycoord(n))
	   write(idevcoord,'(i5,i2,2f12.6)',err=700) n, 2, xim,yim
	  end do
C**********************************************************
C*** write imagic format spots
C**********************************************************
	 else if(imagic) then
C*** imagic coordinate system horiz +ve y to right, vertical +ve x down
	  do n=1,nspot
	   xim = float(nxyz(2) - iycoord(n)) 
	   yim = float(ixcoord(n))
	   write(idevcoord,*,err=700) xim,yim,one
	  end do
	 end if
	 close(idevcoord)
	 return
  700    nlabels = 1
	 call file_error(coordfile,nlabels)
	 close(idevcoord)
         open(unit=idevcoord,
     *       file=coordfile(1:lnblank(coordfile)),status='unknown')
	 if(spider) then
	  write(idevcoord,'('' ; File output from Ximdisp : '',a)') 
     *    mapfile(1:lnblank(mapfile))
	 else if(standard) then
	  if(image) then
	   write(idevcoord,'(''        x         y      density'')')
	  else
	   write(idevcoord,'(
     *     ''        x         y    intensity  amplitude  phase'')')
	  end if
	 end if
	 go to 600
	end if
        end
C********************************************************************
C***
	subroutine output_vectors
C***
C********************************************************************
C*** subroutine to measure vector lengths
C***
	include 'Ximdisp_common.for'
 100    iolabel(nlabels) = 
     *  'Type coordinate file name or <cr> if screen display only ...'
	coordfile = ' '
	call ximageioboxdisplay(iolabel,coordfile,nlabels)
	nlabels = 2
	if(coordfile .ne. ' ') then
C*** filename already present, read from it ?
	 coordsout = .true.
         there = .false.
         inquire(file=coordfile,exist=there)
	 if(there) then
	  iolabel(nlabels) = 
     *    'File already exists, please re-type filename'
	  nlabels = nlabels+1
	  go to 100
	 else
          iolabel(nlabels) = 'Opening a new file...'
	  open(unit=idevcoord,file=coordfile(1:lnblank(coordfile)),
     *        status='new',err=100)
	  write(idevcoord,'(10x,''x1'',4x,''y1'',4x,''x2'',4x,''y2'',
     *    4x,''length'',4x,''angle'')')
	 end if
	else
	 coordsout = .false.
	end if
	nvectors = 0
	call ximagelabeldisplay(iolabel,nlabels)
  500   menulist(1) = 'Select points separately'
	menulist(2) = 'Rubberband line'
	menulist(3) = 'Return main menu'
	call ximagemenuinit(menulist,3)
	job = -1
  600   call ximagewait(job)
	if(job .le. 0) then
	 go to 600
C*** select points separately
	else if(job .eq. 1) then
	 iopt = 1
C*** rubberband line
	else if(job .eq. 2) then
	  iopt = 2
C*** return main menu
	else if(job .eq. 3) then
	 call ximagemenuhide
	 call ximagelabelhide
	 call ximageremovevectors
	 nlabels = 1
	 return
	end if
	call ximagemenuhide
C******************************************
C*** select points separately
C******************************************
  620   if(iopt .eq. 1) then
	 iolabel(nlabels) = 'Mark first vector position'
	 call ximagelabeldisplay(iolabel,nlabels)
	 nlabels = 3
	 menulist(1) = 'Delete vector'
	 menulist(2) = 'Return main menu'
	 call ximagemenuinit(menulist,2)
	 job = -1
 640     call ximagewait(job)
	 if(job .lt. 0) then
	  go to 640
C*****************
C*** draw a vector
C*****************
	 else if(job .eq. 0) then
	  call ximagereadmenupointer(ix1,iy1)
	  if(ix1 .lt. 0 .or. ix1 .gt. max_display_width .or.
     *       iy1 .lt. 0 .or. iy1 .gt. max_display_height)
     *    then
	   iolabel(nlabels) = 'Out of bounds, try again!'
	   call ximagelabeldisplay(iolabel,nlabels)
	   go to 640
	  end if
C*** draw point on first spot
	  call ximagedrawpoint(ix1,iy1)
	  iolabel(nlabels) = 'Mark second vector position'
	  call ximagelabeldisplay(iolabel,nlabels)
	  call ximagereadpointer(ix2,iy2)
	  if(ix2 .lt. 0 .or. ix2 .gt. max_display_width .or.
     *       iy2 .lt. 0 .or. iy2 .gt. max_display_height)
     *    then
	   iolabel(nlabels) = 'Out of bounds, try again!'
	   call ximagelabeldisplay(iolabel,nlabels)
	   go to 640
	  end if
C*** draw point on spot
	  call ximagedrawpoint(ix2,iy2)
	  call ximagedrawlines(ix1,iy1,ix2,iy2,1)
	  go to 900
C*****************
C*** delete line
C*****************
	 else if(job .eq. 1) then
	  go to 850
C*****************
C*** return main menu
C*****************
	 else
	  go to 1000
	 end if
C**************************************************
C*** rubberband line
C**************************************************
	else if(iopt .eq. 2) then
 700	 call ximagerubberenable(1)
         iolabel(nlabels) = 
     *   'Mark cursor position from first to last point'
         iolabel(nlabels+1) = 
     *   'Move cursor with ctrl/left button down'
         iolabel(nlabels+2) = 
     *   'Release mouse button to record position'
         iolabel(nlabels+3) = 
     *   'Release ctrl, double click button on final position'
	 call ximagelabeldisplay(iolabel,nlabels+3)
	 nlabels = 3
	 menulist(1) = 'Delete vector'
	 menulist(2) = 'Return main menu'
	 call ximagemenuinit(menulist,2)
	 job = -1
 800     call ximagewait(job)
	 if(job .le. 0) then
	  go to 800
C******************************
C*** delete vector
C******************************
	 else if(job .eq. 1) then
	  go to 850
C******************************
C*** return main menu
C******************************
	 else if(job .eq. 2) then
	  go to 1000
C********************************
C*** final position
C********************************
	 else if(job .eq. 104 .or. job .eq. 105) then
	  call ximagelabelhide
	  call ximagemenuhide
	  call ximagerubberread(ix1,iy1,ix2,iy2)
	 end if
	 go to 900
	end if
C****************************
C*** delete vector
C****************************
  850   kx1 = ixv1(nvectors)
	ky1 = iyv1(nvectors)
	kx2 = ixv2(nvectors)
	ky2 = iyv2(nvectors)
	call convert_to_screen(kx1,ky1,ix1,iy1)
	call convert_to_screen(kx2,ky2,ix2,iy2)
	if(isymb .eq. 1) then
         maxx = ix1 + ibox_size
         minx = ix1 - ibox_size
         maxy = iy1 + ibox_size
         miny = iy1 - ibox_size
         call ximageremovebox(minx,miny,maxx,maxy)
         maxx = ix2 + ibox_size
         minx = ix2 - ibox_size
         maxy = iy2 + ibox_size
         miny = iy2 - ibox_size
         call ximageremovebox(minx,miny,maxx,maxy)
	else if(isymb .eq. 2) then
	 call ximageremovecircle(ix1,iy1,ibox_size)
	 call ximageremovecircle(ix2,iy2,ibox_size)
	else if(isymb .eq. 3) then
	 call ximageremovepoint(ix1,iy1)
	 call ximageremovepoint(ix2,iy2)
	end if
C*** remove text label
	if(autolabel) then
	 string = ' '
	 string = intoch(nvectors,nchars)
	 call ximageremovetext(ix1+ibox_size,iy1,string)
	end if
C*** remove line itself
	call ximageremovelines(ix1,iy1,ix2,iy2,1)
C*** display previous vector
	nvectors = nvectors - 1
	if(nvectors .lt. 1) go to 1100
        kx1 = ixv1(nvectors)
	ky1 = iyv1(nvectors)
	kx2 = ixv2(nvectors)
	ky2 = iyv2(nvectors)
	call convert_to_screen(kx1,ky1,ix1,iy1)
	call convert_to_screen(kx2,ky2,ix2,iy2)
	call output_vectorcalc(kx1,ky1,kx2,ky2)
	write(iolabel(nlabels-1),
     *  '(i4,'' x1='',i6,'' y1='',i6,'' x2='',i6,'' y2='',i6,
     *  '' length='',f8.1,'' angle='',f6.1)') 
     *  nvectors,kx1,ky1,kx2,ky2,alength,angle
	call ximagelabeldisplay(iolabel,nlabels)
	go to 620
C*** draw box,circle or point around ends of line
  900   if(isymb .eq. 1) then
         maxx = ix1 + ibox_size
         minx = ix1 - ibox_size
         maxy = iy1 + ibox_size
         miny = iy1 - ibox_size
         call ximagedrawbox(minx,miny,maxx,maxy)
         maxx = ix2 + ibox_size
         minx = ix2 - ibox_size
         maxy = iy2 + ibox_size
         miny = iy2 - ibox_size
         call ximagedrawbox(minx,miny,maxx,maxy)
	else if(isymb .eq. 2) then
	 call ximagedrawcircle(ix1,iy1,ibox_size)
	 call ximagedrawcircle(ix2,iy2,ibox_size)
	else if(isymb .eq. 3) then
	 call ximagedrawpoint(ix1,iy1)
	 call ximagedrawpoint(ix2,iy2)
	end if
	nvectors = nvectors + 1
	if(nvectors .gt. max_points) then
	 write(iolabel(nlabels),'(
     *   ''Error - too many vectors, increase max_points'')')
	 return_string = ' '
	 write(iolabel(nlabels+1),'(
     *   ''Press any key to return main menu'')')
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 go to 1100
	end if
	if(autolabel) then
	 string = ' '
	 string = intoch(nvectors,nchars)
	 call ximagedrawtext(ix1+ibox_size,iy1,string)
	end if
C*** calculate length and angle in degrees
	call convert_to_image(ix1,iy1,kx1,ky1)
	call convert_to_image(ix2,iy2,kx2,ky2)
C*** store positions
	ixv1(nvectors) = kx1
	iyv1(nvectors) = ky1
	ixv2(nvectors) = kx2
	iyv2(nvectors) = ky2
	call output_vectorcalc(kx1,ky1,kx2,ky2)
	write(iolabel(nlabels-1),
     *  '(i4,'' x1='',i6,'' y1='',i6,'' x2='',i6,'' y2='',i6,
     *  '' length='',f8.1,'' angle='',f6.1)') 
     *  nvectors,kx1,ky1,kx2,ky2,alength,angle
	call ximagelabeldisplay(iolabel,nlabels)
	call ximagemenuhide
	go to 620
C************************************************************
C*** list complete, write to file
C************************************************************
 1000	if(coordsout) then
	 do n=1,nvectors
	  kx1 = ixv1(n)
	  ky1 = iyv1(n)
	  kx2 = ixv2(n)
	  ky2 = iyv2(n)
	  call output_vectorcalc(kx1,ky1,kx2,ky2)
	  write(idevcoord,'(5i6,f10.1,f10.4)') 
     *         n,kx1,ky1,kx2,ky2,alength,angle
	 end do
	end if
 1100   close(idevcoord)
	call ximagemenuhide
	call ximagelabelhide
	call ximageremovevectors
	nlabels = 1
	return	
	end
C********************************************************************
C***
	subroutine output_vectorcalc(kx1,ky1,kx2,ky2)
C***
C********************************************************************
C*** subroutine to calculate vector lengths and angles
C***
	include 'Ximdisp_common.for'
	xlength = float(kx2 - kx1)
	ylength = float(ky2 - ky1)
	alength = sqrt(xlength * xlength + ylength * ylength)
C*** make vertical line +ve or -ve
	if(xlength .eq. 0.0) then
	 if(ky1 .lt. ky2) then
	  angle = 90.
	 else
	  angle = -90.
	 end if
C*** make horizontal line 0 or 180
	else if(ylength .eq. 0) then
	 if(kx1 .lt. kx2) then
	  angle = 0.
	 else
	  angle = 180.
	 end if
	else
	 angle = radcon * atan2(ylength,xlength)
	end if
	return
	end
C********************************************************************
C***
	subroutine modify_zoom
C***
C********************************************************************
C*** subroutine to change zoom factor
	include 'Ximdisp_common.for'
	call ximagemenuhide
	menulist(1) = 'Type in new zoom factors'
	menulist(2) = 'Expand image to fill screen'
	menulist(3) = 'Return image to original'
	menulist(4) = 'Return main menu'
	call ximagemenuinit(menulist,4)
	job = -1
  100   call ximagewait(job)
	if(job.le.0) then
	 go to 100
C*** Type new zoom factors
	else if(job.eq.1) then
	 call ximagemenuhide
  200	 iolabel(nlabels+1) =
     *   'Type size in x and y in pixels, zoom factors in x and y'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 nlabels = 1
	 call extract_integers(4,return_string,
     *   izoomx, izoomy, izoomfx, izoomfy,i5,i6)
	 call ximagechangezoom(izoomfx,izoomfy,izoomx,izoomy,ierr)
C*** insufficient resources ?
	 if(ierr.ne.0) then
	  iolabel(nlabels+1) = 
     *    'Insufficient resources for this zoom size'
	  nlabels = nlabels + 1
	  go to 200
	 end if
C*** Expand image to fill screen
	else if(job.eq.2) then
	 izoom = 
     *   min((max_screen_width / nox),(max_screen_height / noy))
	 izoomx = nox * izoom
	 izoomy = noy * izoom
	 if(izoom .gt. 1) then
	  call ximagemenuhide
	  iolabel(nlabels+1) = 'Wait for zoom window to be computed...'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	  call ximagechangezoom(izoom,izoom,izoomx,izoomy,ierr)
	 else
	  iolabel(nlabels+1) = 'Image too large to zoom'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	  go to 100
	 end if
C*** insufficient resources ?
	 if(ierr.ne.0) then
	  iolabel(nlabels+1) = 
     *    'Insufficient resources for this zoom size'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	  go to 100
	 end if
	 call ximagezoominit
	 call ximagelabelhide
C*** Return image to original
	else if(job.eq.3) then
	 call ximagemenuhide
	 call ximagezoomhide
	 call ximagechangezoom
     *   (izoom_default,izoom_default,
     *    izoom_length_default,izoom_length_default,ierr)
C*** Return main menu
	else if(job.eq.4) then
	 call ximagemenuhide
	end if
	return
	end
C****************************************************************************
C***
      subroutine remove_cross(ix,iy,isize)
C***
C****************************************************************************
C*** subroutine to remove a small cross from the screen
C****************************************************************************
	include    'Ximdisp_common.for'
	inc = isize / 2
	minx = ix - inc
	maxx = ix + inc
	miny = iy - inc
	maxy = iy + inc
	call ximageremovelines(minx,miny,maxx,maxy,1)
	call ximageremovelines(minx,maxy,maxx,miny,1)
	return
	end
C*************************************************************************
C***
        subroutine sectionedit
C***
C*************************************************************************
C*** delete sections from a list
C*************************************************************************
        include    'Ximdisp_common.for'
	ndel = 0
C*** calculate starting y position
	kystart = iyheight - numsecy * iycr
C*** cross size
	icross_width = min(ixcr/2,iycr/2)
C*** get font size
	call ximagegetfontsize(ifontsize)
	call ximagechangeorigin
	call ximagemenuhide
	call ximagelabeldisplay(iolabel,nlabels)
	menulist(1) = 'Type in section numbers'
	menulist(2) = 'Type in control file name'
	menulist(3) = 'Return main menu'
	call ximagemenuinit(menulist,3)
	iopt = -1
 100    call ximagewait(iopt)
	if(iopt .le. 0) then
	 go to 100
C*** type in section numbers
	else if(iopt .eq. 1) then
	 interactive = .true.
C*** type in control file name
	else if(iopt .eq. 2) then
	 call ximagemenuhide
	 interactive = .false.
         iolabel(nlabels+1) = 'Input control file name ...'
	 coordfile = ' '
	 call ximageioboxdisplay(iolabel,coordfile,nlabels+1)
	 old = .true.
	 call check_file(coordfile)
C*** open and read section numbers for deletion
	 open(unit=idevcoord,file=coordfile(1:lnblank(coordfile)),
     *        status='old')
  200    read(idevcoord,*,end=300) idel
	 ndel = ndel + 1
	 ixcoord(ndel) = idel
	 go to 200
  300    close(idevcoord)
C*** return main menu
	else if(iopt .eq. 3) then
	 call ximagemenuhide
	 call ximagechangeorigin
	 return
	end if
	call ximagemenuhide
        iolabel(nlabels+1) = 'Output name for multisection file ...'
	boxfile = ' '
	call ximageioboxdisplay(iolabel,boxfile,nlabels+1)
C*** read interactive section numbers
	if(interactive) then
 400	 return_string = ' '
	 iolabel(nlabels+1) = 'Type section numbers, -1 to quit'
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 call extract_integers(1,return_string,idel,i2,i3,i4,i5,i6)
	 if(idel .lt. 0) go to 500
	 ndel = ndel + 1
	 ixcoord(ndel) = idel
C*** remove number from screen
         ixpos = mod(idel-nzstart,numsecx) * ixcr
         iypos = kystart + int((idel - nzstart) / numsecx)  * iycr
         if(idel .lt. 100) then
          write(string,'(i2)') idel
         else if(idel .lt. 1000) then
          write(string,'(i3)') idel
         else if(idel .lt. 10000) then
          write(string,'(i4)') idel
         else
          write(string,'(i5)')
         end if
         call ximageremovetext(ixpos+iautoinc,iypos+ifontsize,string)
C*** draw cross on section to be deleted
	 call draw_cross(ixpos+ixcr/2,iypos+iycr/2,icross_width)
	 go to 400
	end if
C*** set up output file header
  500	old = .false.
	call check_file(boxfile)
        call imopen(idevout,boxfile,'new')
        ixyz(1) = nxyz(1)
        ixyz(2) = nxyz(2)
        ixyz(3) = nxyz(3) - ndel 
        kxyz(1) = ixyz(1)
        kxyz(2) = ixyz(2)
        kxyz(3) = ixyz(3)
        boxmin = bignum
        boxmax = 0.
        boxmean = 0.
C*** set output mode
	imode = mode
	write(title,'(a,i6,'' sections deleted by Ximdisp '',a)') 
     *  mapfile(1:lnblank(mapfile)),ndel,date(5:24)
        call icrhdr(idevout,ixyz,kxyz,imode,title,-1)
C*** loop for writing output file omitting edited sections 
	idel = 1
	isec = -1
	ksec = -1
	nsec = nxyz(3)
 1300   isec = isec + 1
	if(isec .gt. nsec) go to 1500
	if(isec .ne. ixcoord(idel)) go to 1400
	idel = idel + 1
	go to 1300
C*** copy this section
 1400   ksec = ksec + 1
	call imposn(idevmap,isec,0)
	call imposn(idevout,ksec,0)
	do iy=1,nxyz(2)
	 call irdlin(idevmap,aline)
	 call iwrlin(idevout,aline)
	 do ix=1,nxyz(1)
	  den = aline(ix)
	  boxmin = min(boxmin,den)
	  boxmax = max(boxmax,den)
	  boxmean = 0.
	 end do
	end do
	go to 1300
C*** all sections copied, write header
 1500   boxmean = boxmean / 
     *  float(nxyz(1) * nxyz(2) * (nxyz(3) - ndel))
	call iwrhdr(idevout,title,0,boxmin,boxmax,boxmean)
	call imclose(idevout)
	call ximagechangeorigin
	return
	end
C*************************************************************************
C***
        subroutine sectionnumber
C***
C*************************************************************************
C*** subroutine to number sections
C*************************************************************************
        include    'Ximdisp_common.for'
C*** calculate starting y position
	kystart = iyheight - numsecy * iycr
	call change_font
C*** get font size - may need to be able to change font
	call ximagegetfontsize(ifontsize)
	call ximagechangeorigin
        call ximagemenuhide
	iolabel(nlabels+1) = 'Select numbering interval'
	call ximagelabeldisplay(iolabel,nlabels+1)
	menulist(1) = 'Every section'
	menulist(2) = 'Every 5 sections'
	menulist(3) = 'Every 10 sections'
	menulist(4) = 'Type in interval'
	menulist(5) = 'Return main menu'
	call ximagemenuinit(menulist,5)
	iopt = -1
 1000   call ximagewait(iopt)
	if(iopt .le. 0) then
	 go to 1000
C*** every section
	else if(iopt .eq. 1) then
	 interval = 1
C*** every 5 sections
	else if(iopt .eq. 2) then
	 interval = 5
C*** every 10 sections
	else if(iopt .eq. 3) then
	 interval = 10
C*** type in interval
	else if(iopt .eq. 4) then
	 call ximagemenuhide
	 call ximagelabelhide
	 iolabel(nlabels+1) = 'Type in interval for numbering sections'
	 return_string = ' '
 	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 call extract_integers(1,return_string,
     *   interval, i2, i3, i4, i5, i6)
C*** return main menu
	else if(iopt .eq. 5) then
	 call ximagelabelhide
	 call ximagemenuhide
	 return
	end if
	call ximagemenuhide
	call ximagelabeldisplay(iolabel,nlabels)
C*** start loop for numbering
	do nz=nzstart, nzend, interval
	 ixpos = mod(nz-nzstart,numsecx) * ixcr
	 iypos = kystart + int((nz - nzstart) / numsecx)  * iycr
C*** write character to screen, find a neater way of doing this !!
	 if(nz .lt. 100) then
	  write(string,'(i2)') nz
	 else if(nz .lt. 1000) then
	  write(string,'(i3)') nz
	 else if(nz .lt. 10000) then
	  write(string,'(i4)') nz
	 else 
	  write(string,'(i5)')
	 end if
	 call ximagedrawtext(ixpos+iautoinc,iypos+ifontsize,string)
	end do
	call ximagechangeorigin
	return
	end
C*************************************************************************
C***
        subroutine splinefit
C***
C*************************************************************************
C*** subroutine to fit a spline to a filament,
C*** straightening the filament by shear, and writing new image
C*************************************************************************
        include    'Ximdisp_common.for'
C***
	imove = 2
 1000   call ximagemenuhide
	call ximagelabeldisplay(iolabel,nlabels)
	menulist(1) = 'Rubberband box'
	menulist(2) = 'Mark box corners with cursor'
	menulist(3) = 'Return main menu'
	call ximagemenuinit(menulist,3)
	iopt = -1
 1050   call ximagewait(iopt)
	if(iopt .le. 0) then
	 go to 1050
C*** rubberband
	else if(iopt .eq. 1) then
	 call ximagemenuhide
 	 call ximagerubberenable(2)
         iolabel(nlabels+1) = 
     *   'Ctrl/Left button down to start at bottom left corner'
         iolabel(nlabels+2) = 
     *   'Drag box to top right corner'
         iolabel(nlabels+3) = 
     *   'Release mouse button to record final position'
	 call ximagelabeldisplay(iolabel,nlabels+3)
	 menulist(1) = 'Re-start box specification'
	 menulist(2) = 'Return main menu'
	 call ximagemenuinit(menulist,2)
	 job = -1
 1100    call ximagewait(job)
	 if(job .le. 0) then
	  go to 1100
	 else if(job .eq. 1) then
	  call ximagemenuhide
	  go to 1000
C*** return main menu
	 else if(job .eq. 2) then
	  call ximagemenuhide
	  return
C*** box finished, read final position
	 else if(job .eq. 105) then 
	  call ximagerubberread(mnx,mny,mxx,mxy)
	 end if
C*** input box corners
	else if(iopt .eq. 2) then
	 call ximagemenuhide
	 iolabel(nlabels+1) = 
     *   'Mark cursor position bottom left corner'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagereadpointer(mnx,mny)
	 call draw_cross(mnx,mny,icross_size)
	 iolabel(nlabels+1) = 
     *   'Mark cursor position top right corner'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagereadpointer(mxx,mxy)
	 call draw_cross(mxx,mxy,icross_size)
	 call ximagedrawbox(mnx,mny,mxx,mxy)
	 call ximagelabelhide
C*** return main menu
	else 
	 call ximagemenuhide
	 call ximagelabelhide
	 return
	end if
	call ximagemenuhide
	call ximagelabelhide
	call convert_to_image(mnx,mny,iminx,iminy)
	call convert_to_image(mxx,mxy,imaxx,imaxy)
	numx = mxx - mnx + 1
	numy = mxy - mny + 1
C*** check dimensions
	if(numx * numy .gt. max_overlay .or.
     *     imaxx - iminx .ge. max_spline_width .or.
     *     imaxy - iminy .ge. max_spline_height) then
	 iolabel(nlabels+1) = 
     *   'Spline area too large, cannot proceed.'//
     *   ' Type any key to return main menu'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 return
	end if
C*** check box menu
	call ximagelabeldisplay(iolabel,nlabels)
	menulist(1) = 'Accept box'
	menulist(2) = 'Re-draw box'
	menulist(3) = 'Return main menu'
	call ximagemenuinit(menulist,3)
	job = -1
1400    call ximagewait(job)
	if(job.lt.0) then
	 go to 1400
C*** accept box
	else if(job .eq. 1) then
	 call ximagemenuhide
	 call ximagelabelhide
	 go to 1500
C*** redraw box
	else if(job .eq. 2) then
	 go to 1000
C*** return main menu
	else
	 call ximagemenuhide
	 call ximagelabelhide
	 call ximageremovevectors
	 return
	end if
C***
C*** initialize
 1500   call ximageremovevectors
	call ximagedrawbox(mnx,mny,mxx,mxy)
	noxout = imaxx - iminx + 1
	noyout = imaxy - iminy + 1
	xmean = 0.
        npts = 1
	do i=1,max_points
	 xden(i) = 0.
	 yden(i) = 0.
	 wd(i) = 0.
	 ixp(i) = 0
	 iyp(i) = 0
	end do
C*** menu to get vectors
 1700   iolabel(nlabels+1) = 
     *  'Mark cursor position successively at each spline point'
	iolabel(nlabels+2) = 'Move from bottom to top'
	call ximagelabeldisplay(iolabel,nlabels+2)
	nlabels = 1
	menulist(1) = 'Delete point just drawn'
	menulist(2) = 'Re-start spline specification'
	menulist(3) = 'Finished spline'
	menulist(4) = 'Return main menu'
	call ximagemenuinit(menulist,4)
	job = -1
 1800   call ximagewait(job)
	if(job.lt.0) then
	 go to 1800
C********************************
C*** read cursor position
C********************************
	else if(job .eq. 0) then
	 npts = npts + 1
C*** check array size
	 if(npts.gt.max_points) then
	  iolabel(nlabels+1) = 
     *    'Too many spline points for program, start again'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	  call ximagedrawbox(mnx,mny,mxx,mxy)
	  npts = 1
	  go to 1800
	 end if
C*** checks ok, read cursor position
	 call ximagereadmenupointer(ixp(npts),iyp(npts))
 	 if(iyp(npts) .le. iyp(npts-1)) then
	  call ximagemenuhide
	  call ximagelabelhide
	  iolabel(nlabels+1) = 
     *    'Points must go from bottom to top, repeat this one'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	  call ximagereadpointer(ixp(npts),iyp(npts))
	  go to 1700
	 end if
C*** draw point if first spline point, line otherwise
	 if(npts .gt. 1) call ximagedrawcircle(ixp(npts),iyp(npts),2)
C*** point accepted
	 call convert_to_image(ixp(npts),iyp(npts),ixim,iyim)
	 yden(npts) = ixim - iminx
	 xden(npts) = iyim - iminy
	 wd(npts) = 1.0
         xmean = xmean + yden(npts)
	 write(string,'(i2)') npts - 1
	 call ximagedrawtext(ixp(npts),iyp(npts),string(1:2))
	 go to 1800
C*******************************
C*** delete point just drawn
C********************************
	else if(job .eq. 1) then
	 call ximageremovecircle(ixp(npts),iyp(npts),2)
	 call ximageremovetext(ixp(npts),iyp(npts),string(1:2))
	 npts = npts - 1
	 go to 1800
C********************************
C*** restart spline
C********************************
	else if(job .eq. 2) then
	 call ximageremovevectors
	 call ximagedrawbox(mnx,mny,mxx,mxy)
	 npts = 1
	 go to 1800
C********************************
C*** finished spline
C********************************
	else if(job .eq. 3) then
	 call ximagemenuhide
	 call ximagelabelhide
	 if(npts .lt. 5) then
	  iolabel(nlabels+1) = 
     *    'Please input some more points - must have at least 4'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	  call ximageremovevectors
	  call ximagedrawbox(mnx,mny,mxx,mxy)
	  call ximagemenudisplay
	  go to 1800
	 end if
	 call ximagemenuhide
	 call ximagelabelhide
	 go to 2000
C********************************
C*** return main menu
C********************************
	else if(job.eq.4) then
	 call ximagemenuhide
	 call ximagelabelhide
	 ierr = 1
	 return
	end if
C**************************************************************
C*** spline specifications finished, begin curve fitting
C**************************************************************
C*** interchange x and y for fitting spline - X dependent variable
C*** set bounds for spline over range of image
C***
 2000	npts = npts + 1
	yden(npts) = 0.
	xden(npts) = noyout
	wd(npts) = 0.
	do i=2,npts-1
	 wd(i) = 1.
	end do
	map_mean = nint((dmean - amin) * scl)
	knots = npts
	xmean = xmean / float(npts-2)
C*****************************************************************
C*** calculate fitted curve
C*****************************************************************
 2200	call vc03ad
     *  (npts,knots,xden,yden,wd,rd,xn,fn,gn,dn,theta,iprint,w)	
C*** loop over y
	nlabels = 1
	ienter = -1
	do iy = 1,noyout
	 yspline = iy - 1
	 xspline = tg01bd(ienter,knots,xn,fn,gn,yspline)
C*** now translate back to screen image
	 ixspline(iy) = nint(xspline) + iminx
	 iyspline(iy) = nint(yspline) + iminy
	 call convert_to_screen(ixspline(iy),iyspline(iy),ix2,iy2)
C*** calculate local angle (normal to filament axis)
	 if(iy .gt. 1) then
	  alpha(iy) = atan(xspline - delx(iy - 1))
	  call ximagedrawlines(ix1,iy1,ix2,iy2,1)
	 end if
	 delx(iy) = xspline
	 ienter = 1
	 ix1 = ix2
	 iy1 = iy2
	end do
	ienter = 0
	alpha(1) = alpha(2)
	call ximagelabeldisplay(iolabel,nlabels)
 2400	menulist(1) = 'Shear - linear interpolation each line'
	menulist(2) = 'Normal mode of bending (bi-linear)'
	menulist(3) = 'Refit spline'
	menulist(4) = 'Return main menu'
	call ximagemenuinit(menulist,4)
	job = -1
 2500	call ximagewait(job)
	if(job .le. 0) then
	 go to 2500
C*****************************************************************
C*** display straightened filament by shear(linear interp on every line)
C*****************************************************************
	else if(job.eq.1) then
	 call ximagemenuhide
	 iolabel(nlabels+1) = 'Wait for calculation and display...'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 do iy=1,noyout
	  xshift = delx(iy) - xmean
	  call imposn(idevmap,0,iminy+iy-1)
	  call irdlin(idevmap,aline)
	  do ix=1,noxout
C*** shift each row by delx
	   xp = float(ix) + xshift
	   intxp = int(xp)
	   deltax = xp - float(intxp)
C*** 
	   if(intxp.lt.1 .or. intxp.ge.noxout) then
	    chunk(ix,iy) = dmean
	   else
	    fden = deltax        * aline(iminx+intxp+1) +
     *             (1. - deltax) * aline(iminx+intxp)
	    chunk(ix,iy) = fden
	   end if
	  end do
	 end do
C*** compress image to be output
	 do iy=1,numy
	  ny = numx * (numy - iy)
	  do ix=1,numx
	   nxy = ny + ix
	   fden = chunk(ix*icompress,iy*icompress)
	   mapbuf(nxy) = nint(min(grey,max(1.,(fden - amin) * scl)))
	  end do
	 end do
C*** write to screen
	 call ximageoverlayinit
     *   (mxx+numx/2+10,mny+numy/2-5,numx,numy,mapbuf,imove,ierr)
	 call ximagelabelhide
	 if(ierr .eq. 1) then
	  iolabel(nlabels+1) = 'Warning : X event error'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	 end if
	 go to 3000
C*********************************************************************
C*** display straightened filament -normal mode of bending, bi-linear
C*********************************************************************
	else if(job.eq.2) then
	 call ximagemenuhide
	 call ximagelabelhide
	 iolabel(nlabels+1) = 'Wait for calculation and display...'
	 call ximagelabeldisplay(iolabel,nlabels+1)
C*** read input map
	 do iy=1,noyout
	  call imposn(idevmap,0,iminy+iy-1)
	  call irdlin(idevmap,aline)
	  do ix=1,noxout
	   iden(ix,iy) = nint(aline(iminx+ix-1))
	  end do
	 end do
C***
	 yinc = 0.01
	 iyinc = 1
	 do iy=1,noyout
	  yinc = yinc + cos(alpha(iyinc))
	  iyinc = nint(yinc)
	  cost = cos(alpha(iyinc))
	  sint = sin(alpha(iyinc))
	  do ix=1,noxout
C*** shift each row by delx
	   xdel = float(ix) - delx(iyinc)
	   xim = 2.0 * delx(iyinc) - xmean + (xdel * cost)
	   yim = yinc - xdel * sint
	   kx = int(xim)
	   ky = int(yim)
	   if( kx .lt. 1 .or. kx .ge. noxout .or.
     *         ky .lt. 1 .or. ky .ge. noyout) then      
	    chunk(ix,iy) = dmean
	   else
	    deltax = xim - float(kx)
	    deltay = yim - float(ky)
	    a = ((1. - deltax) * iden(kx,ky) +
     *            deltax * iden(kx+1,ky)) * (1. - deltay)
	    b = ((1. - deltax) * iden(kx,ky+1) +
     *            deltax * iden(kx+1,ky+1)) * deltay
	    fden = a + b
	    chunk(ix,iy) = fden
	   end if
	  end do
	 end do
C*** compress image to be output
	 do iy=1,numy
	  ny = numx * (numy - iy)
	  do ix=1,numx
	   nxy = ny + ix
	   fden = chunk(ix*icompress,iy*icompress)
	   mapbuf(nxy) = nint(min(grey,max(1.,(fden - amin) * scl)))
	  end do
	 end do
C*** write to screen
	 call ximageoverlayinit
     *   (mxx+numx/2+10,mny+numy/2-5,numx,numy,mapbuf,imove,ierr)
	 call ximagelabelhide
	 if(ierr .eq. 1) then
	  iolabel(nlabels+1) = 'Warning : X event error'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	 end if
	 go to 3000
C*********************************************************************
C*** refit spline
C*********************************************************************
	else if(job.eq.3) then
	 call ximagemenuhide
C*** write coordinates of existing points into labels
	 nlabels = 1
	 do i = 2,npts - 1
	  nval = i - 1
	  nlabels = nlabels + 1
	  write(iolabel(nlabels),
     *    '(3x,i4,'' x='',f7.2,'' y='',f7.2)')
     *    nval, yden(i), xden(i)
	 end do
C*** recalculate xmean
	 xmean = xmean * float(npts - 2)
C*** input new points
  	 iolabel(nlabels+1) = 
     *   'Type point number and new x coordinate, <cr> to finish'
 2600	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
C*** check to see if finished 
	 if(return_string .eq. ' ') then
	  call ximageremovevectors
	  call ximagedrawbox(mnx,mny,mxx,mxy)
	  do i = 2,npts - 1
	   nval = i - 1
	   write(string,'(i3)') nval
	   ix = yden(i) + mnx
	   iy = xden(i) + mny
	   write(string,'(i3)') i - 1
	   call ximagedrawtext(ix,iy,string)
	   call ximagedrawcircle(ix,iy,2)
	  end do
	  xmean = xmean / float(npts - 2)
	  nlabels = 1
	  go to 2200
	 end if
	 call extract_integers(2,return_string,
     *   npoint, iyvalue,i3,i4,i5,i6)
	 nval = npoint + 1
	 ix = yden(nval) + mnx
	 iy = xden(nval) + mny
	 write(string,'(i3)') npoint
	 call ximageremovetext(ix,iy,string)
	 call ximageremovecircle(ix,iy,2)
	 xmean = xmean - yden(nval)
	 yden(nval) = iyvalue
	 ix = yden(nval) + mnx
	 xmean = xmean + yden(nval)
	 call ximagedrawtext(ix,iy,string)
	 call ximagedrawcircle(ix,iy,2)
	 write(iolabel(nval),'(3x,i4,'' x='',f7.2,'' y='',f7.2)')
     *   npoint, yden(nval), xden(nval)
	 go to 2600
C*****************************************************************
C*** return main menu
C*****************************************************************
	else if(job.eq.4) then
	 call ximagemenuhide
	 call ximageoverlayhide
	 call ximageremovevectors
	 return
	end if
C*****************************************************************
C*** Calculated image displayed, create output file or fit further
C*****************************************************************
 3000   call ximagelabeldisplay(iolabel,nlabels)
	menulist(1) = 'Create image output file'
	menulist(2) = 'Further image calculation'
	menulist(3) = 'Return main menu'
	call ximagemenuinit(menulist,3)
	job = -1
 3100	call ximagewait(job)
	if(job .le. 0) then
	 go to 3100
C*****************************************************************
C*** create output file
C*****************************************************************
	else if(job.eq.1) then
	 call ximagemenuhide
	 call ximagelabelhide
	 iolabel(nlabels+1) = 'Output file name ...'
	 boxfile = ' '
	 call ximageioboxdisplay(iolabel,boxfile,nlabels+1)
	 old = .false.
 	 call check_file(boxfile)
	 ixyz(1) = noxout
	 ixyz(2) = noyout
	 ixyz(3) = 1
	 do i=1,3
	  kxyz(i) = ixyz(i)
	 end do
	 call imopen(idevout,boxfile,'new')
	 call icrhdr(idevout,ixyz,kxyz,mode,title,0)
	 call itrlab(idevout,idevmap)
	 call getdate(date,nsecs)
	 title = 'Ximdisp splinefit: straightened'//date(5:24)
	 smin = 100000000.
	 smax = -smin
	 smean = 0.
	 call iwrhdr(idevout,title,1,smin,smax,smean)
	 do iy = 1,noyout
	  do ix = 1,noxout
	   den = chunk(ix,iy)
	   smin = min(den,smin)
	   smax = max(den,smax)
	   smean = smean + den
	   aline(ix) = den
	  end do
	  call iwrlin(idevout,aline)
	 end do
	 smean = smean / float(noxout * noyout)
	 call iwrhdr(idevout,title,-1,smin,smax,smean)
	 call imclose(idevout)
	 call ximagelabelhide
	 go to 3000
C*****************************************************************
C*** further calculations
C*****************************************************************
	else if(job.eq.2) then
	 call ximagemenuhide
	 call ximagelabelhide
	 go to 2400
C*****************************************************************
C*** return main menu
C*****************************************************************
	else if(job.eq.3) then
	 call ximagemenuhide
	 call ximagelabelhide
	 call ximageoverlayhide
	 call ximageremovevectors
	 return
	end if
	end
