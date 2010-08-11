C**********************************************************************
C	Ximagelibf.f 				05.06.98
C	- a set of fortran subroutines to interface 
C	an X windows application program with Ximagelibc.c
C**********************************************************************
C	The first call must always be to ximageinit to initialize
C	the drawing area and then ximagesetcolour, and
C	ximagedrawimage to write the image into a window on the screen
C
C	Images need to be the screen size if it will be required
C	to draw vectors in areas where there is no actual image.
C
C	Menus need to be initialized (ximagemenuinit) first. A call to
C	ximagemenudisplay will redisplay the same menu after a call to
C	ximagemenuhide.
C	ximagewait must be called after ximagemenuinit. This should
C	be part of a loop which will return a value from ximagewait
C	which indicates selection of a menu item (numbered 1 - n)
C**********************************************************************
C example :
C	call ximageinit
C       call ximagesetcolourtable(index,ientries,red,green,blue)
C       call ximagedrawimage(nox,noy,isec,map,ierr)	
C 100	call ximagemenuinit(menulist,nitems)
C	job = -1
C 200	call ximagewait(job)
C	if(job.eq.-1) then
C	 go to 200
C*** menu item 1
C	else if (job.eq.1) then
C	 call ximagedrawcircle(ix,iy,irad)
C*** menu item 2
C	else if (job.eq.2) then
C	 call ximagechangezoom(zoom_factor_x,zoom_factor_y,
C		 zoom_width,zoom_height,ierr)
C*** menu item 3
C	else if (job.eq.3) then
C	 call ximageioboxdisplay(out_string,in_string,1)
C	end if
C	go to 200
C
C*********************************************************************
C	Standard controls :
C
C	Left hand mouse button  reads image coordinates
C			        selects menu items
C	Centre mouse button     pans the image - stops on release
C				hides the zoomed area if present
C	Right hand mouse button displays a zoomed area of the image
C	Ctrl/m			redisplays a hidden menu
C**********************************************************************
C***    List of subroutines :
C
C	ximagedrawbox(ix1,iy1,ix2,iy2)
C		ix1, iy1,ix2,iy2 coords of 2 points of rectangle
C
C	ximagedrawcircle(ix,iy,irad)
C		ix, iy i*4 coords of centre in pixels i*4
C		irad i*4 radius in pixels
C
C	ximagedrawelipse(ix,iy,iradx,irady,rang)
C		ix, iy i*4 coords of centre in pixels i*4
C		iradx i*4 radius in pixels in first axis
C		irady i*4 radius in pixels in second axis
C		rang float angle of first axis 
C
C	ximagedrawlines(ix1,iy1,ix2,iy2,nlines)
C		ix1, iy1 i*4 arrays of start coords for line in pixels
C		ix2, iy2 i*4 arrays of end coords for line in pixels
C		nlines i*4 number if lines in arrays
C
C	ximagedrawpoint(ix,iy)
C		ix, iy i*4 coords of point in pixels
C
C	ximagechangecursor(icurs)
C
C	ximagechangefont(nfont,ierr)
C
C	ximagechangeorigin
C
C	ximagechangezoom (izoom_factor_x,izoom_factor_y,
C			izoom_width,izoom_height,ierr)
C		izoom_factor_x i*4 multiplication factor in x
C		izoom_factor_y i*4 multiplication factor in y
C		izoom_width i*4 width of new zoom window in pixels
C		izoom_height i*4 height of new zoom window in pixels
C		ierr         i*4 non zero if error
C
C	ximagecheckfonts(fonts,nfonts)
C		fontlist*64 array of available fonts
C		nfonts i*4 number of fonts available
C
C	ximagecheckimage(image_width,image_height,nsecs,ierr)
C		image_width i*4 width of image in pixels
C		image_height i*4 height of image in pixels
C		nsecs i*4 number of map sections to be loaded
C		ierr i*4 returned 1 if image request too large 0 otherwise
C
C	ximageclearimage
C
C	ximagecolourbarinit
C
C	ximagecolourbarhide
C
C	ximagedelay(idelay)
C		idelay i*4 delay in microseconds
C
C	ximagedrawimage(nox,noy,isec,map,ierr)
C		nox, noy i*4 number in x, number in y
C		isec i*4 number of section to be loaded (starts from 0)
C		map single dimensioned byte array of densities
C		ierr i*4 0 if ok
C			 1 if read error
C			 2 if pixmap cannot be read back correctly
C			 3 if all zeroes
C
C	ximagedrawimages(isec)
C		isec i*4 section number to be written into window
C
C	ximagedrawtext(ix,iy,text_string)
C		ix, iy i*4 coords for botton left corner of string
C		string - character string to be displayed
C
C	ximageinit(max_window_wid, max_window_ht)
C		max_window_wid, max_window_ht i*4)
C			window width, height which you see
C		note: this routine initializes fontlist.
C		string fontlist(100)*64
C
C	ximageioboxdisplay(out_string,in_string,nlines)
C		out_string character string to be displayed in iobox
C		in_string character string to be returned
C		nlines i*4 number of lines displayed
C
C	ximagelabeldisplay(string,nlines)
C		string character string to be displayed
C		nlines i*4 number of lines displayed
C
C	ximagelabelhide
C
C	ximagemenudisplay
C
C	ximagemenuevent
C
C	ximagemenuhide
C
C	ximagemenuinit(menulist,nitems)
C		menulist character array with strings to be displayed
C			in menu boxes
C		nitems i*4 number of menu items 
C
C	ximageoverlayinit(ix,iy,ioverlay_width,ioverlay_height,overlay,ierr)
C		ix, iy screen coordinates for centre of displayed overlay
C		ioverlay_width,ioverlay_height dimensions of overlay
C
C	ximageoverlayhide
C
C	ximagepointertrackon(ixshift,iyshift)
C		ixshift, iyshift coordinate shift to be added to screen coords
C
C	ximagepointertrackoff
C
C	ximagepostscriptdump(idevp,colour,mnx,mxx,mny,mxy,xsize,ipixmap,ierr)
C		idevp i*4 output device number for postscript file
C		colour logical true for colour
C		mnx,mxx,mny,mxy rectangular box vertices
C		set these values to 0 for whole window dump
C		xsize horizontal width in mm
C		ipixmap 0 for main window, 1 for overlay
C		ierr returned as 1 if consistency check fails, o otherwise
C
C	ximagereadmenupointer(ix,iy)
C		ix,iy i*4 window coords returned from cursor
C
C	ximagereadpointer(ix,iy)
C		ix, iy i*4 window coords returned from cursor
C
C	ximagerubberenable(itype)
C		itype = 0 for lines, 2 for box, 3 for circle
C
C	ximagerubberread(ix1,iy1,ix2,iy2)
C		i*4 screen coords of 2 ends of line, box or centre
C                   and point on radius of circle
C
C	ximagerubberremoveline(ix1,iy1,ix2,iy2)
C		i*4 screen coords of 2 ends of line
C
C	ximagesetcolourtable
C
C	ximagequit
C
C	ximageremovecircle(ix,iy,irad)
C		ix, iy i*4 coords of centre in pixels i*4
C		irad i*4 radius in pixels
C
C	ximageremovelines(ix1,iy1,ix2,iy2,nlines)
C		ix1, iy1 i*4 arrays of start coords for line in pixels
C		ix2, iy2 i*4 arrays of end coords for line in pixels
C		nlines i*4 number if lines in arrays
C
C	ximageremovebox(ix1,iy1,ix2,iy2)
C		ix1, iy1,ix2, iy2 points describing the box opposite
C		vertices
C
C	ximageremovepoint(ix,iy)
C		ix, iy i*4 coords of point in pixels
C	ximageremovetext(ix,iy,text_string)
C		ix, iy i*4 coords for botton left corner of string
C		string - character string to be displayed
C
C	ximageremovevectors
C
C	ximagesetdash
C
C	ximagesetsolid
C
C	ximagesliderhide
C
C	ximagesliderinit(nsliders,slider1,slider2)
C		nsliders can be 1 or 2 depending on how many slider bars
C		slider1, slider2 starting positions of thumb
C
C	ximagesliderread(ipercent)
C
C	subroutine ximagezoominit
C
C	subroutine ximagezoomhide
C
C	subroutine ximagewait(job)
C		job i*4 returns menu item number
C
C******************************************************************
C***
	subroutine Ximagelib
C***
C******************************************************************
	integer*4	ipaper_size
	parameter	(ipaper_size = 0)
	integer*4	maxcolours
	parameter	(maxcolours = 256)
	integer*4	maxfonts
	parameter	(maxfonts = 100)
	integer*4	maxlines
	parameter	(maxlines = 8192)
	integer*4	irle
	parameter	(irle = 0)
	real*4		paper_width
	parameter	(paper_width = 210.0)
	real*4		paper_height
	parameter	(paper_height = 295.0)
	real*4		paper_width_limit
	parameter	(paper_width_limit = 190.0)
	real*4		paper_height_limit
	parameter	(paper_height_limit = 270.0)
C***
	integer*4	cflag
	integer*4	icflag
	integer*4	ix1(maxlines)
	integer*4	iy1(maxlines)
	integer*4	ix2(maxlines)
	integer*4	iy2(maxlines)
	integer*4	kx1(*)
	integer*4	ky1(*)
	integer*4	kx2(*)
	integer*4	ky2(*)
	integer*4	local_job
	integer*4	max_window_height
	integer*4	max_window_width
	integer*4	nfont
	integer*4	nfonts
	integer*4	nofonts
	integer*4	red(0:maxcolours-1)
	integer*4	green(0:maxcolours-1)
	integer*4	blue(0:maxcolours-1)
	integer*4	ired(maxcolours)
	integer*4	igreen(maxcolours)
	integer*4	iblue(maxcolours)
C***
	logical 	colour
	logical		change_origin
C***
	integer*2 	itest
C***
	byte	btest(2)
	byte	map(*)
	byte	pixels(0:maxlines)
	byte	overlay(*)
C***
	character*64	fontlist(maxfonts)
	character*64	fonts(maxfonts)
	character	in_string*(*)
	character	label_string*4096
	character	menulist(*)*(*)
	character	menuline*64
	character	out_string(*)*(*)
	character	string(*)*(*)
	character	temp_string*512
	character	text_string*(*)
C***
	equivalence 	(btest(1),itest)
C***
	save		change_origin,
     *			fonts,
     *			nfonts,
     *                  icflag,
     *			map_width,
     *			map_height,
     *			max_window_width,
     *			max_window_height,
     *			max_zoom_size,
     *			nox,
     *			noy,
     *			ired,igreen,iblue
C***
C
C**********************************************************************
C***	start of entries
C**********************************************************************
C	ximagechangecursor - change cursor type
C**********************************************************************
	entry ximagechangecursor(icurs)
C*** icurs < 0 changes bit pattern in bitmap
C*** icurs = 0 default arrow
C*** icurs = 1 fine crosshair
C*** icurs = 2 boxed crosshair
C*** icurs = 3 huge crosshair
C*** icurs = 4 cross
C*** icurs = 5 circle
C*** icurs = 6 hand
C*** icurs = 7 bird
C*** icurs = 8 skull
	icurs = min(icurs,8)
	call changecursor(icurs)
	return
C**********************************************************************
C	ximagechangefont - change font type
C**********************************************************************
	entry ximagechangefont(nfont,ierr)
	ierr = 0
	if(nfont .lt. 1 .or. nfont .gt. nfonts) then
	 ierr = 1
	 return
	end if
	call changefont(nfont)
	if(nfont .lt. 0) then
	 ierr = 1
	 nfont = -nfont
	end if
	return
C**********************************************************************
C	ximagechangeorigin - coordinate origin now bottom left corner
C**********************************************************************
	entry ximagechangeorigin
	change_origin = .true.
	return
C**********************************************************************
C	ximagechangezoom - modify zoom factors
C**********************************************************************
	entry ximagechangezoom
     *  (izoom_factor_x,izoom_factor_y,izoom_width,izoom_height,ierr)
	ierr = 0
	if(izoom_width*izoom_height .gt. max_zoom_size) then
	 ierr = 1
	 return
	end if
	call changezoom
     *  (izoom_factor_x,izoom_factor_y,izoom_width,izoom_height)
	call ximageerror(job)
	if(job.eq.999) ierr = 1
	return
C**********************************************************************
C	ximagecheckfonts - returns available fonts
C**********************************************************************
	entry ximagecheckfonts(fontlist,nofonts)
	nofonts = nfonts
	do i=1,nfonts
	 fontlist(i) = fonts(i)
	end do
	return
C**********************************************************************
C	ximagecheckimage - check image size
C**********************************************************************
	entry ximagecheckimage(image_width,image_height,nmaps,ierr)
	ierr = 0
	call checkimage(image_width,image_height,nmaps)
	call ximageerror(job)
	if(job .eq. 999) ierr = 1
	map_width = image_width
	map_height = image_height
	return
C**********************************************************************
C	ximageclearmap - clear map
C**********************************************************************
	entry ximageclearimage
	call changecursor(0)
	call clearimage
	return
C**********************************************************************
C	ximagecolourbarinit - initialize/display colour bar
C**********************************************************************
	entry ximagecolourbarinit
	call colourbarinit
	return
C**********************************************************************
C	ximagecolourbarhide - remove colour bar
C**********************************************************************
	entry ximagecolourbarhide
	call colourbarhide
	return
C**********************************************************************
C	ximagesetpan - set pan_x and pan_y (Andreas)
C**********************************************************************
	entry ximagesetpan(inx,iny)
	call setpan(inx,iny)
	return
C**********************************************************************
C	ximagedelay - sets delay in microseconds
C**********************************************************************
	entry ximagedelay(idelay)
	call delay(idelay)
	return
C**********************************************************************
C	ximagedrawbox - draw a box
C**********************************************************************
	entry ximagedrawbox(nx1,ny1,nx2,ny2)
	if(nx1 .gt. nx2) then
	 jx1 = nx2
	 jx2 = nx1
	else
	 jx1 = nx1
	 jx2 = nx2
	end if
	if(change_origin) then
	 jy1 = map_height - ny1
	 jy2 = map_height - ny2
	else
	 jy1 = ny1
	 jy2 = ny2
	end if
C*** switch if not bottom left first
	if(jy1 .gt. jy2) then
	 iytemp = jy1
	 jy1 = jy2
	 jy2 = iytemp
	end if
	call drawbox(jx1,jy1,jx2,jy2)
	return
C**********************************************************************
C	ximagedrawcircle - draw a circle
C**********************************************************************
	entry ximagedrawcircle(kx,ky,irad)
	ix = kx
	if(change_origin) then
	 iy = map_height - ky
	else
	 iy = ky
	end if
	radius = float(irad)
	call drawcircle(ix,iy,radius)
	return
C**********************************************************************
C       ximagedrawelipse - draw an elipse
C**********************************************************************
        entry ximagedrawelipse(kx,ky,iradx,irady,rang)
        ix = kx
        if(change_origin) then
         iy = map_height - ky
        else
         iy = ky
        end if
        radiux = float(iradx)
        radiuy = float(irady)
        call drawelipse(ix,iy,radiux,radiuy,rang)
        return
C**********************************************************************
C	ximagedrawlines - draw lines
C**********************************************************************
	entry ximagedrawlines(kx1,ky1,kx2,ky2,nlines)
C***
	if(nlines.gt.maxlines) then
	 write(6,'(''Too many lines for program'')')
	 return
	end if
C*** invert y if change_origin true
	if(change_origin) then
	 do i=1,nlines +1
	  ix1(i) = kx1(i)
	  iy1(i) = map_height - ky1(i)
	  ix2(i) = kx2(i)
	  iy2(i) = map_height - ky2(i)
	 end do
	else
	 do i=1,nlines+1
	  ix1(i) = kx1(i)
	  iy1(i) = ky1(i)
	  ix2(i) = kx2(i)
	  iy2(i) = ky2(i)
	 end do
	end if
	call drawlines(ix1,iy1,ix2,iy2,nlines)
	return
C**********************************************************************
C	ximagedrawpoint - draw a point
C**********************************************************************
	entry ximagedrawpoint(kx,ky)
	ix = kx
	if(change_origin) then
	 iy = map_height - ky
	else
	 iy = ky
	end if
	call drawpoint(ix,iy)
	return
C**********************************************************************
C	ximagedrawimage - draw a map
C**********************************************************************
	entry ximagedrawimage(nx,ny,isec,map,ierr)
	ierr = 0
	nox = nx
	noy = ny
	call drawimage(nox,noy,isec,map,cflag)
	icflag = cflag
	if(nox .gt. max_window_width .or. noy .gt. max_window_height) then
	 map_width = nox
	 map_height = noy
	else
	 map_width = max_window_width
	 map_height = max_window_height
	end if
	call ximageerror(job)
	if(job.eq.999) ierr = 1
	if(icflag .eq. 1) ierr = 2
	if(icflag .eq. 2) ierr = 3
	return
C**********************************************************************
C	ximagedrawimages - draw a section
C**********************************************************************
	entry ximagedrawimages(isec)
	call drawpixmap(isec)
	return
C**********************************************************************
C	ximagedrawtext - draw a text string
C**********************************************************************
	entry ximagedrawtext(kx,ky,text_string)
	length = lnblank(text_string)
	ix = kx
	if(change_origin) then
	 iy = map_height - ky
	else
	 iy = ky
	end if
	call drawtext(ix,iy,text_string,length)
	return
C**********************************************************************
C	ximageinit - initialize the drawing window
C**********************************************************************
	entry ximageinit(max_window_wid, max_window_ht)
C*** origin top left corner of screen by default
	change_origin = .false.
C*** test machine little or big endian 
c	btest(1) = 0
c	btest(2) = 0
c	itest = 1
C*** big endian
c	if(btest(1) .eq. 0) then
c	 iend = 1
C*** little endian
c	else
c	 iend = 0
c	end if
C*** initialize X
	max_window_width = max_window_wid
	max_window_height = max_window_ht
	call init(max_window_width, max_window_height, max_zoom_size,
     *  fonts, nfonts)
	max_window_wid = max_window_width
	max_window_ht = max_window_height
C*** set map height, width to default window size
	map_height = max_window_height
	map_width = max_window_width
C*** strip off the null character in font list
	do 50 n=1,nfonts
	 len_string = len(fonts(n))
	 do i=1,len_string
	  if(fonts(n)(i:i).eq.char(0)) then
	   fonts(n)(i:len_string) = ' '
	   go to 50
	  end if
	 end do
   50   continue
	return
C**********************************************************************
C	ximageioboxdisplay - display an iobox requiring interaction
C**********************************************************************
	entry ximageioboxdisplay(out_string,in_string,nlines)
C***
C*** restructure output string with null character at each line end
C*** note in_string must be defined by the calling program, even
C*** if it is set to ' '.
	icount = 1
	icount_length = 0
	do i=1,nlines
	 temp_string = ' '
	 length = len(out_string(i))
	 temp_string(1:length) = out_string(i)(1:length)
	 length = lnblank(temp_string)
	 icount_length = icount+length
	 label_string(icount:icount_length-1) = out_string(i)(1:length)
	 label_string(icount_length:icount_length) = char(10)
	 icount = icount_length + 1
	end do
	label_string(icount_length:icount_length) = char(0)
C*** add null character after last character of editable string
	length = lnblank(in_string)
	in_string(length+1:length+1) = char(0)
	call ioboxdisplay(label_string,in_string,nlines)
  100	call ximagewait(job)
	if(job.eq.-1.or.job.eq.0) go to 100
	if(job.ne.100) then
	 write(6,'('' Error in ximagewait, job = '',i)')
	else
	 call ioboxreadstring(in_string)
C*** strip off the null character
	 len_string = len(in_string)
	 do i=1,len_string
	  if(in_string(i:i).eq.char(0)) then
	   in_string(i:len_string) = ' '
	   go to 200
	  end if
	 end do
	end if
  200	call ioboxhide
	return
C**********************************************************************
C	ximagelabeldisplay - display a label
C**********************************************************************
	entry ximagelabeldisplay(string,nlines)
	icount = 1
	do i=1,nlines
	 temp_string = ' '
	 length = len(string(i))
	 temp_string = string(i)(1:length)
	 length = lnblank(temp_string)
	 icount_length = icount+length
	 label_string(icount:icount_length-1) = string(i)(1:length)
	 label_string(icount_length:icount_length) = char(10)
	 icount = icount_length + 1
	end do
	label_string(icount_length:icount_length) = char(0)
	call labeldisplay(label_string)
	return
C**********************************************************************
C	ximagelabelhide - hide label
C**********************************************************************
	entry ximagelabelhide
	call labelhide
	return
C**********************************************************************
C	ximagemenudisplay - display menu
C**********************************************************************
	entry ximagemenudisplay
	call actionmenudisplay
	return
C**********************************************************************
C	ximagemenuevent - process menu events continuously
C**********************************************************************
	entry ximagemenuevent
	call menuevent
	return
C**********************************************************************
C	ximagemenuhide - push menu behind image window
C**********************************************************************
	entry ximagemenuhide
	call menuhide
	return
C**********************************************************************
C	ximagemenuinit - initialize menu
C**********************************************************************
	entry ximagemenuinit(menulist,nitems)
	maxlength = len(menulist(1))
	do i=1,nitems
	 length = len(menulist(i))
	 menuline(1:length) = menulist(i)(1:length)
	 length = min(lnblank(menuline)+1,maxlength)
	 maxlength = max(maxlength,length)
	 menulist(i)(length:length) = char(0)
	end do
	call menuinit(nitems,maxlength,menulist)
	return
C**********************************************************************
C	ximageoverlayhide - hide overlay area
C**********************************************************************
	entry ximageoverlayhide
	call overlayhide
	return
C**********************************************************************
C	ximageoverlayinit - initialize overlay area
C**********************************************************************
	entry ximageoverlayinit
     *  (kx,ky,ioverlay_width,ioverlay_height,overlay,ierr)
	ix = kx
	if(change_origin) then
	 iy = map_height - ky
	else
	 iy = ky
	end if
	call overlayinit(ix,iy,ioverlay_width,ioverlay_height,overlay)
	call ximageerror(job)
	if(job.eq.999) ierr = 1
	return
C**********************************************************************
C	ximagepointertrackon - enable pointer tracking
C**********************************************************************
	entry ximagepointertrackon(ixshift,iyshift)
	if(change_origin) then
	 ichange_origin = map_height
	else 
	 ichange_origin = 0
	end if
	call pointertrackon(ixshift,iyshift,ichange_origin)
	return
C**********************************************************************
C	ximagepointertrackoff - disable pointer tracking
C**********************************************************************
	entry ximagepointertrackoff
	call pointertrackoff
	return
C**********************************************************************
C	ximagepostscriptdump - dump image area to postscript file
C**********************************************************************
	entry ximagepostscriptdump
     *  (idevp,colour,mnx,mxx,mny,mxy,xsize,ipixmap,ierr)
C*** return if icflag non-zero i.e. pixmap consistency check fails
	if(icflag .ne. 0) then
	 ierr = 1
	 return
	end if
C*** set up number of columns, rows
C*** main window
	if(ipixmap .eq. 0) then
C*** whole window 
	 if(mxx .eq. 0 .and. mxy .eq. 0) then
	  minx = 0
	  maxx = map_width - 1
	  miny = 0
	  maxy = map_height - 1
	 else
C*** boxed area
	  ioffset = max(0,(max_window_width - nox) / 2)
	  minx = mnx + ioffset + 2
	  maxx = mxx + ioffset - 2
	  miny = mny + 1
	  maxy = mxy - 1
	  if(change_origin) then
	   miny = map_height - mxy + 1
	   maxy = map_height - mny - 1
	  end if
	 end if
C*** overlay window
	else if(ipixmap .eq. 1) then
	 minx = mnx
	 miny = mny
	 maxx = mxx
	 maxy = mxy
	end if
	ncols = maxx - minx + 1
	nrows = maxy - miny + 1
C*** set horizontal size
	if(nrows .gt. ncols) then
	 landscape = 0
	 if(xsize .le. 0.) xsize = paper_width_limit
C*** set vertical size
	 ysize = xsize * nrows / ncols
	 if(ysize .gt. paper_height_limit) then
	  ysize = paper_height_limit
	  xsize = ysize * ncols / nrows
	 end if
C*** set origin
	 xorigin = (paper_width - xsize) * 0.5
	 yorigin = (paper_height - ysize) * 0.5
C*** landscape
	else
	 landscape = 1
	 if(xsize .le. 0.) xsize = paper_height_limit
C*** set vertical size
	 ysize = xsize * nrows / ncols
	 if(ysize .gt. paper_width_limit) then
	  ysize = paper_width_limit
	  xsize = ysize * ncols / nrows
	 end if
C*** set origin
	 xorigin = (paper_height - xsize) * 0.5
	 yorigin = (paper_width - ysize) * 0.5
	end if
C*** open output file
	open(unit=idevp,file='Ximage.ps',status='unknown')
	call PSCRIPTinit(
     *  idevp,
     *  ipaper_size,
     *  landscape,
     *  xorigin,
     *  yorigin,
     *  xsize,
     *  ysize,
     *  irle,
     *  ncols,
     *  nrows)
C***
C*** colour
	if(colour) then
	 call PSCRIPTcolourtable(ired,igreen,iblue)
	 do i=miny,maxy
	  call readimage(0,i,ipixmap,pixels)
	  call PSCRIPTcolourimage(pixels(minx))
	 end do
C*** grey-scale
	else
	 do i=miny,maxy
	  call readimage(0,i,ipixmap,pixels)
C*** maximise scaling
	  do j=minx,maxx
	   k = pixels(j)
	   if(k .lt. 0) k = k + 256
C*** set vectors to white
	   if(k .gt. 127) then
	    k = 255
C*** double densities to mimic full 8 bits
	   else
	    k = k * 2
	   end if
	   pixels(j) = k
	  end do
	  call PSCRIPTgreyimage(pixels(minx))
	 end do
	end if
	call PSCRIPTendimage
	call PSCRIPTend
	return
C**********************************************************************
C	ximagereadmenupointer - read pointer coords while in menu loop
C**********************************************************************
	entry ximagereadmenupointer(kx,ky)
C*** read cursor position while in a menu loop
	call readpointer(ix,iy)
	kx = ix
	if(change_origin) then
	 ky = map_height - iy
	else
	 ky = iy
	end if
	return
C**********************************************************************
C	ximagereadpointer - read pointer coords
C**********************************************************************
	entry ximagereadpointer(kx,ky)
C*** read cursor position
  300   call ximagewait(job)
	if(job.eq.-1) go to 300
	if(job.ne.0) then
     	  write(6,'('' error in ximagewait, job = '',i)') job
	else
	  call readpointer(ix,iy)
	  kx = ix
	  if(change_origin) then
	   ky = map_height - iy
	  else
	   ky = iy
	  end if
	end if
	return
C**********************************************************************
C	ximagerubberenable - starts read rubber band coords
C**********************************************************************
	entry ximagerubberenable(irubbertype)
	 irtype = irubbertype
	 call rubberenable(irtype)
	 return
C**********************************************************************
C	ximagerubberread - gets coords of rubber band line, box or circle
C**********************************************************************
	entry ximagerubberread(nx1,ny1,nx2,ny2)
C*** read cursor position while in a menu loop
	call rubberread(jx1,jy1,jx2,jy2)
	nx1 = jx1
	nx2 = jx2
	if(change_origin) then
	 ny1 = map_height - jy1
	 ny2 = map_height - jy2
	else
	 ny1 = jy1
	 ny2 = jy2
	end if
	return
C**********************************************************************
C	ximagerubberremoveline - remove rubber banded line
C**********************************************************************
	entry ximagerubberremoveline(nx1,ny1,nx2,ny2)
	jx1 = nx1
	jx2 = nx2
	if(change_origin) then
	 jy1 = map_height - ny1
	 jy2 = map_height - ny2
	else
	 jy1 = ny1
	 jy2 = ny2
	end if
	call rubberremoveline(jx1,jy1,jx2,jy2)
	return
C**********************************************************************
C	ximagesetcolourtable
C**********************************************************************
	entry ximagesetcolourtable(index1,index2,red,green,blue)
	index_start = max(index1,0)
	nentries = max(index2-index1+1,128)
C*** save colours for postscript dump
	do i=1,maxcolours
	 ired(i) = (red(i-1) * 255) / 65535
	 igreen(i) = (green(i-1) * 255) / 65535
	 iblue(i) = (blue(i-1) * 255) / 65535
	end do
	call setcolourtable(index_start,nentries,red,green,blue)
	return
C**********************************************************************
C	ximagequit - quit from program
C**********************************************************************
	entry ximagequit
	call quit
	return
C**********************************************************************
C	ximageremovebox - remove a box
C**********************************************************************
	entry ximageremovebox(nx1,ny1,nx2,ny2)
	if(nx1 .gt. nx2) then
	 jx1 = nx2
	 jx2 = nx1
	else
	 jx1 = nx1
	 jx2 = nx2
	end if
	if(change_origin) then
	 jy1 = map_height - ny1
	 jy2 = map_height - ny2
	else
	 jy1 = ny1
	 jy2 = ny2
	end if
C*** switch if not bottom left first
	if(jy1 .gt. jy2) then
	 iytemp = jy1
	 jy1 = jy2
	 jy2 = iytemp
	end if
	call removebox(jx1,jy1,jx2,jy2)
	return
C**********************************************************************
C	ximageremovecircle - remove a circle 
C**********************************************************************
	entry ximageremovecircle(kx,ky,irad)
	ix = kx
	if(change_origin) then
	 iy = map_height - ky
	else
	 iy = ky
	end if
	radius = float(irad)
	call removecircle(ix,iy,radius)
	return
C*********************************************************************
C	ximageremovelines - remove an array of vectors 
C*********************************************************************
	entry ximageremovelines(kx1,ky1,kx2,ky2,nlines)
C***
C***    nlines 0 if last line only to be removed
	if(nlines .gt. 0) then
C*** invert y if change_origin true
	 if(change_origin) then
	  do i=1,nlines +1
	   ix1(i) = kx1(i)
	   iy1(i) = map_height - ky1(i)
	   ix2(i) = kx2(i)
	   iy2(i) = map_height - ky2(i)
	  end do
	 else
	  do i=1,nlines+1
	   ix1(i) = kx1(i)
	   iy1(i) = ky1(i)
	   ix2(i) = kx2(i)
	   iy2(i) = ky2(i)
	  end do
	 end if
	end if
	call removelines(ix1,iy1,ix2,iy2,nlines)
	return
C************************************************************************
C	ximageremovepoint - remove a point 
C************************************************************************
	entry ximageremovepoint(kx,ky)
	ix = kx
	if(change_origin) then
	 iy = map_height - ky
	else
	 iy = ky
	end if
	call removepoint(ix,iy)
	return
C**********************************************************************
C	ximageremovetext - remove a text string
C**********************************************************************
	entry ximageremovetext(kx,ky,text_string)
	length = lnblank(text_string)
	ix = kx
	if(change_origin) then
	 iy = map_height - ky
	else
	 iy = ky
	end if
	call removetext(ix,iy,text_string,length)
	return
C**********************************************************************
C	ximageremovevectors - clear application window of all vectors
C**********************************************************************
	entry ximageremovevectors
	call removevectors
	return
C**********************************************************************
C	ximagesetdash - change line style to dashed
C**********************************************************************
	entry ximagesetdash
	call setdash
	return
C**********************************************************************
C	ximagesetsolid - change line style to solid
C**********************************************************************
	entry ximagesetsolid
	call setsolid
	return
C**********************************************************************
C	ximagesliderhide - hide slider bar
C**********************************************************************
	entry ximagesliderhide
	call sliderhide
	return
C**********************************************************************
C	ximagesliderinit - display slider bar
C**********************************************************************
	entry ximagesliderinit(nsliders,slider1,slider2)
	call sliderinit(nsliders,slider1,slider2)
	return
C**********************************************************************
C	ximagesliderread(slider_position)
C**********************************************************************
	entry ximagesliderread(slider_position)
        call readslider(percent)
        slider_position = percent
	return
C**********************************************************************
C	ximagezoomhide - hide zoom area
C**********************************************************************
	entry ximagezoomhide
	call zoomhide
	return
C**********************************************************************
C	ximagezoominit - zoom an area
C**********************************************************************
	entry ximagezoominit
	call zoominit
	return
	end
C**********************************************************************
C	ximagewait - wait for an event
C**********************************************************************
	subroutine ximagewait(iopt)
	common /locjob/ local_job
C***
	call eventdispatch
C*** store variables as local from callback entry
	iopt = local_job
	local_job = -1
	return	
C*** store value passed back from X
	entry ximagecallback(ival)
	local_job = ival
	return
C*** test for error
	entry ximageerror(iopt)
	iopt = local_job
	local_job = -1
	return	
	end
C*********************************************************************
C*********************************************************************
C*********************************************************************
	SUBROUTINE PSCRIPTIMAGE
C  routines to generate a postscript image file
C  the routines should be called in the following order:
C  CALL PSCRIPTINIT(		to initialise the postscript system
C  1  OUTSTREAM,		INTEGER*4. fortran stream number for this file,
C                                          already opened by caller
C  1  PAPERSIZE,		INTEGER*4. 0=A4, 1=A3 
C  1  ORIENTATION,		INTEGER*4. 0=portrait, 1=landscape
C  1  XORIGIN,YORIGIN,		REAL	 . plot origin relative to bottom
C					   l.h. corner of paper (in mm)
C  1  XSIZE,YSIZE,		REAL	 . image size in mm 
C                                          (relate to ncols,nrows)
C  1  RLE,			INTEGER*4. Run-Length-Encoding. 0=no, 1=yes.
C					   run-length-encoding can make for
C					   much shorter postscript files but
C					   doesnt work on all printers. OK
C					   for clc, not for laser5.
C  1  NCOLS,NROWS)		INTEGER*4. No. of cols and rows in image

C  CALL PSCRIPTCOLOURTABLE	if its a colour plot
C  CALL PSCRIPTxxxIMAGE	for each line of image. xxx= GREY or COLOUR
C  CALL PSCRIPTENDIMAGE	at the end of the image
C  CALL PSCRIPTCOPY		for each line of raw postscript required
C  CALL PSCRIPTEND		at job completion


	IMPLICIT	NONE
	INTEGER*4	BLUE(256)
	INTEGER*4	GREEN(256)
	INTEGER*4	I
	INTEGER*4	J
	INTEGER*4	K
	INTEGER*4	N
	INTEGER*4	NCOLS
	INTEGER*4	NROWS
	INTEGER*4	ORIENTATION	!0=portrait, 1=landscape
	INTEGER*4	OSTREAM
	INTEGER*4	PAPERSIZE	!0=A4, 1=A3
	REAL		XORIGIN
	REAL		YORIGIN
	REAL		XSIZE
	REAL		YSIZE
	INTEGER*4	RED(256)
	INTEGER*4	RLE		!0=dont run-length-encode it, 1=do.
	byte	ROW(1)
	CHARACTER	STRING*(*)	!string to copy thro'

C  local storage
	INTEGER*4	L_BLUE(256)
	  SAVE		L_BLUE
	LOGICAL		L_COLOUR
	LOGICAL		L_HEADERDONE
	  SAVE		L_HEADERDONE
	INTEGER*4	L_GREEN(256)
	  SAVE		L_GREEN
	INTEGER*4	L_NCOLS
	  SAVE		L_NCOLS
	INTEGER*4	L_NROWS
	  SAVE		L_NROWS
	INTEGER*4	L_NVALS
	  SAVE		L_NVALS
	INTEGER*4	L_ORIENTATION
	  SAVE		L_ORIENTATION
	INTEGER*4	L_OSTREAM
	  SAVE		L_OSTREAM
	CHARACTER	L_PAPERSIZE*16
	  SAVE		L_PAPERSIZE
	REAL		L_ORIGINOFFSET
	  SAVE		L_ORIGINOFFSET
	INTEGER*4	L_RED(256)
	  SAVE		L_RED
	LOGICAL		L_RLE
	  SAVE		L_RLE
	INTEGER*4	L_RLE_LIST(2,20)
	  SAVE		L_RLE_LIST
	REAL		L_XORIGIN
	  SAVE		L_XORIGIN
	REAL		L_YORIGIN
	  SAVE		L_YORIGIN
	REAL		L_XSIZE
	  SAVE		L_XSIZE
	REAL		L_YSIZE
	  SAVE		L_YSIZE


	CHARACTER	OLINE*80
	INTEGER*4	NCHUNKS
	INTEGER*4	NEWPIX
	INTEGER*4	NREM
	INTEGER*4	PIXCOUNT
	INTEGER*4	PREVPIX

Ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
	ENTRY PSCRIPTINIT(
     1  OSTREAM,
     2  PAPERSIZE,ORIENTATION,
     3  XORIGIN,YORIGIN,XSIZE,YSIZE,
     4  RLE,NCOLS,NROWS)
	L_OSTREAM=OSTREAM
	L_PAPERSIZE=' '			!default to A4
	L_ORIGINOFFSET=210
	IF (PAPERSIZE.EQ.1) THEN
	  L_PAPERSIZE='/a3 load exec'
	  L_ORIGINOFFSET=L_ORIGINOFFSET*SQRT(2.0)
	END IF
	L_ORIENTATION=0
	IF (ORIENTATION.EQ.1) L_ORIENTATION=90
	L_XORIGIN=XORIGIN
	L_YORIGIN=YORIGIN-ORIENTATION*L_ORIGINOFFSET
	L_XSIZE=XSIZE
	L_YSIZE=YSIZE
	L_RLE=(RLE.NE.0)
	L_NCOLS=NCOLS
	L_NROWS=NROWS
	DO I=1,256
	  L_RED(I)=I-1
	  L_GREEN(I)=I-1
	  L_BLUE(I)=I-1
	END DO
	L_HEADERDONE=.FALSE.
	L_NVALS=0
	WRITE(L_OSTREAM,1000)
     1  L_PAPERSIZE
1000	FORMAT(
     1 '%!PS-Adobe-'/
     1 '%EndComments'/
     1 'initgraphics'/
     1 A/
     1 '/Helvetica findfont 20 scalefont setfont'
     1 )
	RETURN

Ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
	ENTRY PSCRIPTCOLOURTABLE(RED,GREEN,BLUE)
	DO I=1,256
	  L_RED(I)=RED(I)
	  L_GREEN(I)=GREEN(I)
	  L_BLUE(I)=BLUE(I)
	END DO
	RETURN

Ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
	ENTRY PSCRIPTCOLOURIMAGE(ROW)
	L_COLOUR=.TRUE.
	GOTO 10

Ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
	ENTRY PSCRIPTGREYIMAGE(ROW)
	L_COLOUR=.FALSE.

10	CONTINUE
	IF (L_HEADERDONE) GOTO 20
	L_HEADERDONE=.TRUE.

	IF ((.NOT.L_RLE).AND.(.NOT.L_COLOUR)) WRITE(L_OSTREAM,1010)
     1 L_ORIENTATION,L_XORIGIN,L_YORIGIN,L_XSIZE,L_YSIZE,
     1 L_NCOLS,
     1 L_NCOLS,L_NROWS,
     1 L_NCOLS,-L_NROWS,L_NROWS
1010	FORMAT(
     1 '10 dict begin'/
     1 '2.836464 2.836464 scale'/
     1 I6,' rotate ',F8.2,F8.2,' translate ',F8.2,F8.2,' scale'/
     1 '/pixels',I6,' string def'/
     1 I6,I6,' 8'/
     1 '[',I6,' 0 0 ',I6,' 0',I6,']'/
     1 '{currentfile pixels readhexstring pop} bind'/
     1 'image'
     1 )

	IF ((.NOT.L_RLE).AND.L_COLOUR) WRITE(L_OSTREAM,1020)
     1 L_ORIENTATION,L_XORIGIN,L_YORIGIN,L_XSIZE,L_YSIZE,
     1 L_RED,
     1 L_GREEN,
     1 L_BLUE,
     1 L_NCOLS,
     1 3*L_NCOLS
1020	FORMAT(
     1 '10 dict begin'/
     1 '2.836464 2.836464 scale'/
     1 I6,' rotate ',F8.2,F8.2,' translate ',F8.2,F8.2,' scale'/
     1 '/red ['/
     1 16(16I4.3/),'] def'/
     1 '/green ['/16(16I4.3/),'] def'/
     1 '/blue ['/16(16I4.3/),'] def'/
     1 '/inpixels',I6,' string def'/
     1 '/outpixels',I6,' string def'/
     1 '/temp 0 store'/
     1 '/pixel 0 store'/
     1 '/setpixel'
     1 )
	IF ((.NOT.L_RLE).AND.L_COLOUR) WRITE(L_OSTREAM,1021)
     1 L_NCOLS,L_NROWS,
     1 L_NCOLS,-L_NROWS,L_NROWS,
     1 L_NCOLS-1
1021	FORMAT(
     1 '{'/
     1 '  /temp exch store'/
     1 '  /pixel inpixels temp get store'/
     1 '  /temp temp 3 mul store'/
     1 '  outpixels temp red pixel get put'/
     1 '  outpixels temp 1 add green pixel get put'/
     1 '  outpixels temp 2 add blue pixel get put'/
     1 '} bind def'/
     1 I6,I6,' 8'/
     1 '[',I6,' 0 0 ',I6,' 0',I6,']'/
     1 '{'/
     1 'currentfile inpixels readhexstring pop pop'/
     1 '0 1',I6,' {setpixel} for'/
     1 'outpixels'/
     1 '} bind'/
     1 'false 3 colorimage'
     1 )

	IF (L_RLE.AND.(.NOT.L_COLOUR)) WRITE(L_OSTREAM,1030)
     1 L_ORIENTATION,L_XORIGIN,L_YORIGIN,L_XSIZE,L_YSIZE,
     1 L_NCOLS,L_NROWS,
     1 L_NCOLS,-L_NROWS,L_NROWS
1030	FORMAT(
     1 '10 dict begin'/
     1 '2.836464 2.836464 scale'/
     1 I6,' rotate ',F8.2,F8.2,' translate ',F8.2,F8.2,' scale'/
     1 '/runlen 2 string def'/
     1 '/pixel 0 def'/
     1 '/npixels -1 def'/
     1 '/pixels 256 string def'/
     1 I6,I6,' 8'/
     1 '[',I6,' 0 0 ',I6,' 0',I6,']'/
     1 '{'/
     1 'currentfile runlen readhexstring pop pop'/
     1 '/npixels runlen 0 get store'/
     1 '/pixel runlen 1 get store'/
     1 '0 1 npixels {pixels exch pixel put} for'/
     1 'pixels 0 npixels 1 add getinterval'/
     1 '} bind'/
     1 'image'
     1 )

	IF (L_RLE.AND.L_COLOUR) WRITE(L_OSTREAM,1040)
     1 L_ORIENTATION,L_XORIGIN,L_YORIGIN,L_XSIZE,L_YSIZE,
     1 L_RED,
     1 L_GREEN,
     1 L_BLUE
1040	FORMAT(
     1 '10 dict begin'/
     1 '2.836464 2.836464 scale'/
     1 I6,' rotate ',F8.2,F8.2,' translate ',F8.2,F8.2,' scale'/
     1 '/red ['/
     1 16(16I4.3/),'] def'/
     1 '/green ['/16(16I4.3/),'] def'/
     1 '/blue ['/16(16I4.3/), '] def'/
     1 '/runlen 2 string def'/
     1 '/pixel 0 def'/
     1 '/npixels -1 def'/
     1 '/pixels 768 string def'/
     1 '/temp 0 def'/
     1 '/setpixel'/)
	IF (L_RLE.AND.L_COLOUR) WRITE(L_OSTREAM,1041)
     1 L_NCOLS,L_NROWS,
     1 L_NCOLS,-L_NROWS,L_NROWS
1041	FORMAT(
     1 '{'/
     1 '  /temp exch 3 mul store'/
     1 '  pixels temp red pixel get put'/
     1 '  pixels temp 1 add green pixel get put'/
     1 '  pixels temp 2 add blue pixel get put'/
     1 '} bind def'/I6,I6,' 8'/'[',I6,' 0 0 ',I6,' 0',I6,']'/
     1 '{'/'currentfile runlen readhexstring pop pop '/
     1 '/npixels runlen 0 get store'/
     1 '/pixel runlen 1 get store'/
     1 '0 1 npixels {setpixel} for'/
     1 'pixels 0 npixels 1 add 3 mul getinterval'/
     1 '} bind'/'false 3 colorimage')

20	CONTINUE
C  write the optionally run-length-encoded line
	IF (L_RLE) THEN
	  PIXCOUNT=0
	  PREVPIX=-1
	  L_NVALS=0
	  DO I=1,L_NCOLS
	    NEWPIX=ROW(I)
	    IF (NEWPIX.LT.0) NEWPIX=NEWPIX+256
	    IF ((NEWPIX.NE.PREVPIX.AND.PIXCOUNT.NE.0).OR.PIXCOUNT.EQ.256) THEN
	      L_NVALS=L_NVALS+1
	      L_RLE_LIST(1,L_NVALS)=PIXCOUNT-1
	      L_RLE_LIST(2,L_NVALS)=PREVPIX
	      PIXCOUNT=1
	      PREVPIX=NEWPIX
	      N=1
	      IF (L_NVALS.EQ.20) THEN
	        DO K=1,20
	          DO J=1,2
	            CALL HEX_FORMAT(L_RLE_LIST(J,K),OLINE(N:))
	            N=N+2
	          END DO
	        END DO
	        WRITE(L_OSTREAM,FMT='(A)') OLINE(1:N-1)
	        L_NVALS=0
	      END IF
	    ELSE
	      PIXCOUNT=PIXCOUNT+1
	      PREVPIX=NEWPIX
	    END IF
	  END DO
	  IF (PIXCOUNT.NE.0) THEN
	    L_NVALS=L_NVALS+1
	    L_RLE_LIST(1,L_NVALS)=PIXCOUNT-1
	    L_RLE_LIST(2,L_NVALS)=NEWPIX
	  END IF
	  IF (L_NVALS.NE.0) THEN
	    N=1
	    DO K=1,L_NVALS
	      DO J=1,2
	        CALL HEX_FORMAT(L_RLE_LIST(J,K),OLINE(N:))
	        N=N+2
	      END DO
	    END DO
	    WRITE(L_OSTREAM,FMT='(A)') OLINE(1:N-1)
	    WRITE(L_OSTREAM,FMT='(1X)')
	  END IF
	ELSE
C  write line in 25-byte chunks
	  NCHUNKS=L_NCOLS/25
	  NREM=L_NCOLS-25*NCHUNKS
	  DO J=1,NCHUNKS
	    N=1
	    DO I=1,25
C  beware endian-ness. cant pass byte variable directly here
	      K=ROW(25*(J-1)+I)
	      CALL HEX_FORMAT(K,OLINE(N:))
	      N=N+2
	    END DO
	    WRITE(L_OSTREAM,FMT='(A)') OLINE(1:N-1)
	  END DO
C  write remainder
	  IF(NREM.GT.0) THEN
	   N=1
	   DO I=1,NREM
	     K=ROW(25*(J-1)+I)
	     CALL HEX_FORMAT(K,OLINE(N:))
	     N=N+2
	   END DO
	   WRITE(L_OSTREAM,FMT='(A)') OLINE(1:N-1)
	  END IF
	ENDIF
	RETURN

Ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
	ENTRY PSCRIPTCOPY(STRING)
	WRITE(L_OSTREAM,FMT='(A)') STRING
	RETURN

Ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
	ENTRY PSCRIPTENDIMAGE
	WRITE(L_OSTREAM,1080)
1080	FORMAT(
     1 'end'
     1 )
	RETURN

Ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
	ENTRY PSCRIPTEND
	WRITE(L_OSTREAM,1090)
1090	FORMAT(
     1 'showpage'/
     1 '%Trailer'
     1 )
	CLOSE(L_OSTREAM)
	RETURN
	END

Cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
	SUBROUTINE HEX_FORMAT(IVAL,CHARDEST)
C  format a byte into 2 hex chars
	CHARACTER	CHARDEST*2
	INTEGER*4	IVAL
C the following mess is to avoid lots of continuation lines...
	CHARACTER	HEXLIST(0:255)*2
	CHARACTER	HLIST(0:15)*32
	EQUIVALENCE	(HLIST,HEXLIST)
	DATA HLIST/
     1  '000102030405060708090A0B0C0D0E0F',
     1  '101112131415161718191A1B1C1D1E1F',
     1  '202122232425262728292A2B2C2D2E2F',
     1  '303132333435363738393A3B3C3D3E3F',
     1  '404142434445464748494A4B4C4D4E4F',
     1  '505152535455565758595A5B5C5D5E5F',
     1  '606162636465666768696A6B6C6D6E6F',
     1  '707172737475767778797A7B7C7D7E7F',
     1  '808182838485868788898A8B8C8D8E8F',
     1  '909192939495969798999A9B9C9D9E9F',
     1  'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF',
     1  'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF',
     1  'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF',
     1  'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF',
     1  'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF',
     1  'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'
     1  /

	I=IVAL
	IF (I.LT.0) I=I+256

C  START OF BITSWAP
c	 NEWVAL=0
c	 MASK=128
c	 NEWMASK=1
c	 DO K=1,8
c	   IF ((I.AND.MASK).NE.0) NEWVAL=NEWVAL.OR.NEWMASK
c	   MASK=MASK/2
c	   NEWMASK=NEWMASK*2
c	 END DO
c	 I=NEWVAL
C  END OF BITSWAP

	CHARDEST=HEXLIST(I)
	RETURN
	END
