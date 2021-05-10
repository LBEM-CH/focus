C        Ximdisp
C*********************************************************************
C
C	program to display/manipulate images/transforms 01.02.2001
C
C       imdisp is a menu driven interactive graphics display 
C       program with outines providing options
C
C
C**********************************************************************
C
C       libraries called : imlib, genlib, ximagelib,ifftlib,subs,misclib
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
C			 25 input DATAFILE
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
C*** lattice refinement input file free format :
C*** ih, ik, x, y
C************************************************************************
C***
C*** start of main program
C***

C************************************************************************
	include		'hXM_common.for'
C***
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
C
C*** initialize screen
	icurs = 0
	max_screen_width = 1
	max_screen_height = 1
	call ximageinit(max_screen_width, max_screen_height)
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
        mapread = .false.
        lkup = .false.
	pointer = .false.
C***
         
C***
C
 4102   continue
C
C***    **************************datafile einlesen******************
	open(unit=idevdat,file='DATAFILE.dat',err=4099,
     1       status='old')
	do 1 i=1,11
		read(idevdat,'(a)') 
    1	continue
	read(idevdat,'(a)') df_filename
	read(df_filename,'(a)') df_fileori
        j=0
        idflen = 0
	do i=1,40
	 if(df_filename(i:i) .eq. ' ' .and. j.eq.0)  then
           write(spotfile,'(A,''.spt'')')df_filename(1:i-1)
           idflen = i-1
           j=1
         endif
        end do
	do i=13,22
	  read(idevdat,'(a)') df_recell
     	enddo
        read(idevdat,'(I6)') idf_mapmode
	do i=24,31
	  read(idevdat,'(a)') df_recell
     	enddo
	read(idevdat,'(a)') df_recell
	read(idevdat,'(a)') df_badrecell
	read(idevdat,'(a)') df_badrecell
	read(idevdat,'(a)') df_badrecell
	do i=36,57
	  read(idevdat,'(a)') czeile
     	enddo
	read(idevdat,*) df_orix,df_oriy
C        write(*,'(/,/,'' origin = '',2F12.3)')df_orix,df_oriy
	close(idevdat)
        goto 4101
C
 4099   continue
C-------DATAFILE.dat does not exist here.
        df_filename = ' '
        idflen = 0
        idefoci = 0
        df_recell = '1.0,0.0,0.0,0.1'
        df_badrecell = '1.0,0.0,0.0,0.1'
        job=7
        goto 4098
C	
C
 4101	continue
        ifftscl = 0
        iredcen = 0
	if(df_filename(1:1).eq.'m')then
          menulist(1) = 'Display unmasked image'
          menulist(2) = 'Display masked image'
	  menulist(3) = 'Display fourier transformation'
	  menulist(4) = 'Display red. fourier transformation'
	  menulist(5) = 'Display red. central fourier transformation'
	  menulist(6) = 'Determine defoci in corners'
	  menulist(7) = 'mrcclick.mrc'
	  menulist(8) = 'Other image'
	  menulist(9) = 'Quit'
	  call ximagemenuinit(menulist,9)
        else if (idf_mapmode .eq. 2 ) then
          menulist(1) = 'Display Original Diff-pattern'
          menulist(2) = 'Display Background-subtracted Diff-pattern'
          menulist(3) = 'Display averaged peak positions'
          menulist(4) = 'Display origin refinement map'
          menulist(5) = 'Other image'
          menulist(6) = 'Quit'
          call ximagemenuinit(menulist,5)
        else
          menulist(1) = 'Display image'
	  menulist(2) = 'Display fourier transformation'
	  menulist(3) = 'Display red. fourier transformation'
	  menulist(4) = 'Display red. central fourier transformation'
	  menulist(5) = 'Determine defoci in corners'
	  menulist(6) = 'mrcclick.mrc'
	  menulist(7) = 'Other image'
	  menulist(8) = 'Quit'
	  call ximagemenuinit(menulist,8)
        endif
        iclick=0
	idefoci=0
	job = -1
 5001   call ximagewait(job)
 4098   continue
	if(job.le.0) then
	  go to 5001
C*** rescale image
        endif
C
        if(idf_mapmode.eq.2)then
C
          locjob = job
	  if(locjob.le.4) then
  	    call ximagemenuhide
	    read(df_fileori,'(a)')df_filename
            call shorten(df_filename(1:20),k)
            i=k+1
	    write(*,'('' df_filename: '',I6,'' = '',A20)')k,df_filename(1:k)
C
	    if(locjob.eq.1)then
C-------------Name.mrc
	      df_filename(i:i+3)='.mrc'
	      idf_picmode = 1
            endif
C
	    if(locjob.eq.2)then
C-------------Name.crt.mrc
	      df_filename(i:i+7)='.crt.mrc'
	      idf_picmode = 2
            endif
C
	    if(locjob.eq.3)then
C-------------Name.ave.mrc
	      df_filename(i:i+7)='.ave.mrc'
	      idf_picmode = 3
            endif
C
	    if(locjob.eq.4)then
C-------------Name.ave.mrc
	      df_filename(i:i+10)='.refine.mrc'
	      idf_picmode = 4
            endif
C
	    mapfile = df_filename(1:40)
	    write(*,'('' getting: '',A20,'' or '',A20)')df_filename(1:20),mapfile(1:20)
            inquire(file=mapfile,exist=there)
	    old = .true.
	    call check_file(mapfile)
	    if(outswitch) write(idevout,'(a)') mapfile
	  else if(locjob.eq.5) then
C*** type in file name
	    call ximagemenuhide
 1002	    continue
C     	    mapfile = df_filename(1:idflen)
     	    mapfile = ''
	    nlabels = 1
            iolabel(nlabels) = 
     *      'Ximdisp - image display program H14.76'
	    iolabel(nlabels+1) = 
     *      'Version 23.11.2001'
	    there = .false.
            inquire(file=mapfile,exist=there)
	    if(idevin .gt. 0) then
	      read(idevin,'(a)') mapfile
	      if(idevin .eq. idevdef) 
     *          iolabel(nlabels+2) = 'Input filename ...'
	    else
	      iolabel(nlabels+2) = 'Type in file name ...'
	    end if
	    if(idevin .eq. 0 .or. idevin .eq. idevdef) then
	      call ximageioboxdisplay(iolabel,mapfile,nlabels+2)
	      old = .true.
	      call check_file(mapfile)
	    else
	      if(.not.there) then
	        write(6,'(''Input file not found'',a)') mapfile
	        stop
	      end if
	    end if
	    if(outswitch) write(idevout,'(a)') mapfile
C***
          else
	    stop
	  endif
C
C*****************************************************************
        else
          locjob = job
	  if(df_filename(1:1).ne.'m')locjob=locjob+1
C
	  if(locjob.le.6) then
		call ximagemenuhide
		read(df_fileori,'(a)')df_filename
                i=1
 4097           continue
			if(df_filename(i:i) .eq. ' ')  then
				if(locjob.eq.1)then
C---------------------------------FFTIR/Name.mrc
				  if(df_filename(1:1).eq.'m')then
				    czeile(1:i-2)=df_filename(2:i-1)
				    i=i-1
				    df_filename(1:i)=czeile(1:i)
				  endif
				  df_filename(i:i+3)='.mrc'
          			endif
				if(locjob.eq.2)then
C---------------------------------FFTIR/mName.mrc
				  df_filename(i:i+3)='.mrc'
          			endif
				if(locjob.eq.3)then
C---------------------------------FFTIR/mName.fft.mrc
				  write(czeile,'(''FFTIR/'',A,''.fft.mrc'')')df_filename(1:i-1)
				  write(df_filename,'(A)')czeile(1:i+15)
			  	  ifftscl = 1
 				endif
				if(locjob.eq.4)then
C---------------------------------FFTIR/mName.red.fft.mrc
				  write(czeile,'(''FFTIR/'',A,''.red.fft.mrc'')')df_filename(1:i-1)
				  write(df_filename,'(A)')czeile(1:i+19)
				  ifftscl = 2
 				endif
				if(locjob.eq.5)then
C---------------------------------CUT/mName.red.fft.mrc
				  czeile(1:4)='CUT/'
                                  czeile(5:5+i-1)=df_filename(1:i-1)
                                  i=i+4
                                  df_filename(1:i-1)=czeile(1:i-1)
				  df_filename(i:i+18)='.red.center.fft.mrc'
				  ifftscl = 1
                                  iredcen = 1
 				endif
				if(locjob.eq.6)then
C---------------------------------CUT/mName.tl.fft.mrc
				  if(inewdef.ne.0)then
				    idefoci=inewdef-1
				  endif
				  if(df_filename(1:1).eq.'m')then
				    czeile(1:i-2)=df_filename(2:i-1)
 				    i=i-1
				    df_filename(1:i)=czeile(1:i)
 				  endif
				  if(idefoci.eq.0.or.idefoci.eq.4)then
				    czeile(1:4)='CUT/'
                                    czeile(5:5+i-1)=df_filename(1:i-1)
                                    i=i+4
                                    df_filename(1:i-1)=czeile(1:i-1)
				    df_filename(i:i+10)='.tl.fft.mrc'
				  elseif(idefoci.eq.1)then
				    czeile(1:4)='CUT/'
                                    czeile(5:5+i-1)=df_filename(1:i-1)
                                    i=i+4
                                    df_filename(1:i-1)=czeile(1:i-1)
				    df_filename(i:i+10)='.tr.fft.mrc'
				  elseif(idefoci.eq.2)then
				    czeile(1:4)='CUT/'
                                    czeile(5:5+i-1)=df_filename(1:i-1)
                                    i=i+4
                                    df_filename(1:i-1)=czeile(1:i-1)
				    df_filename(i:i+10)='.bl.fft.mrc'
				  elseif(idefoci.eq.3)then
				    czeile(1:4)='CUT/'
                                    czeile(5:5+i-1)=df_filename(1:i-1)
                                    i=i+4
                                    df_filename(1:i-1)=czeile(1:i-1)
				    df_filename(i:i+10)='.br.fft.mrc'
				  endif
				  idefoci = idefoci+1
				  if(idefoci.gt.4)idefoci=1
				  ifftscl = 1
 				endif
				goto 1001
			end if
  			i=i+1
		if(i.le.40)goto 4097
 1001		continue
		mapfile = df_filename(1:40)
        	inquire(file=mapfile,exist=there)
	 	old = .true.
	 	call check_file(mapfile)
		if(outswitch) write(idevout,'(a)') mapfile
	  else if(locjob.eq.7) then
		call ximagemenuhide
 		mapfile = 'mrcclick.mrc'
                iclick=1
        	inquire(file=mapfile,exist=there)
	 	old = .true.
	 	call check_file(mapfile)
		if(outswitch) write(idevout,'(a)') mapfile
	  else if(locjob.eq.8) then
C*** type in file name
	    call ximagemenuhide
 1000	    continue
C     	    mapfile = df_filename(1:idflen)
     	    mapfile = ''
	    nlabels = 1
            iolabel(nlabels) = 
     *      'Ximdisp - image display program H14.76'
	    iolabel(nlabels+1) = 
     *      'Version 23.11.2001'
	    there = .false.
            inquire(file=mapfile,exist=there)
	    if(idevin .gt. 0) then
	      read(idevin,'(a)') mapfile
	      if(idevin .eq. idevdef) 
     *          iolabel(nlabels+2) = 'Input filename ...'
	    else
	      iolabel(nlabels+2) = 'Type in file name ...'
	    end if
	    if(idevin .eq. 0 .or. idevin .eq. idevdef) then
	      call ximageioboxdisplay(iolabel,mapfile,nlabels+2)
	      old = .true.
	      call check_file(mapfile)
	    else
	      if(.not.there) then
	        write(6,'(''Input file not found'',a)') mapfile
	        stop
	      end if
	    end if
	    if(outswitch) write(idevout,'(a)') mapfile
C***
          else
	    stop
C*****************************************************************
	  endif
        endif
C***
C	write(*,'('' opening mapfile '',a)') mapfile
C
C*** open image input file
 1100   continue
        mapread = .true.
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
C***
C*** move parameters
 2000	do i=1,3
	 ixyzmin(i) = 0
	 ixyzmax(i) = 0
	end do
	nox = 0
	noy = 0
	noz = 0
	ixyzmax(3) = nxyz(3) - 1
C***
C*** check for image or transform
 2100	if(mode .lt. 3) then
	 image = .true.
C*** single section
	 if(nxyz(3) .eq. 1) then
C*** interactive read
	  if(idevin .eq. 0) then
	    write(iolabel(nlabels+1),
     *      '('' number of points (x,y)='',2i5)') nxyz(1), nxyz(2)
	    iolabel(nlabels+2) = ' $enter limits (xmin,max,ymin,max)'
	    iolabel(nlabels+3) = ' [ (0,0) is lower-left corner ] :'
 	    return_string = ' '
           if(iclick .eq. 0) then
  	    call ximageioboxdisplay(iolabel,return_string,nlabels+3)
	    if(return_string .ne. '/' .and. return_string .ne. ' ') then
	     call extract_integers(4,return_string,
     *       ixyzmin(1), ixyzmax(1), ixyzmin(2), ixyzmax(2), i5, i6)
	    end if
C           else
C            write(6,'(''Skipping picture limits definition'')')
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
C*** interactive read
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
C*** transform
        else
	 image = .false.
C*** interactive read
	 if(idevin .eq. 0) then
	  write(iolabel(nlabels+1),
     *    '('' number of points (x,y) ='',2i5)') nxyz(1),nxyz(2)
	  iolabel(nlabels+2) = 
     *    '$ enter half number in x, half number in y  :'
	  return_string = ' '
	  if(idefoci.eq.0)then
            call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	    if(return_string .ne. '/' .and. return_string .ne. ' ') then
	      call extract_integers(2,return_string,
     *        nox, noy, i3, i4, i5, i6)
C*** add 1 to both to inlude equator and meridian
	     nox = nox + 1
	     noy = noy + 1
	    end if
	  endif
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
         if(nox.eq.0) then
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
C***
C*** pixel compression for images
	icompress = 1
        if(image) then
C*** interactive read
	 if(idevin .eq. 0) then
          if(iclick .eq. 0) then
	   iolabel(nlabels+1) = 
     *     'Enter pixel compression factor <cr> for default (none)'
	   return_string = ' '
	   call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	   if (return_string .ne. ' ') 
     *	    call extract_integers(1,return_string,icompress,
     *      i2,i3,i4,i5,i6)
C          else
C           write(6,'(''Skipping pixel compression factor definition'')')
          endif
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
C***
C*** make sure that max-min is 1024 or less
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
C***
C*** reset end values of outside range, total number must
C*** not exceed 1024, start with x values
	if(newmap) then
	 big = .false.
	 if(image) then
	  max_display_width = max(max_screen_width,nox)
	  if(nox .gt. max_screen_width) big = .true.
	 else
	  max_display_width = max(max_screen_width,nox*2)
	  if(nox*2 .gt. max_screen_width) big = .true.
	 end if
	 if(noy .gt. max_screen_height) big = .true.
	 max_display_height = max(max_screen_height,noy)
	 go to 2500
	else
C*** add a new map
	 if(image) then
	  if (nox .gt. max_display_width .or.
     *        noy .gt. max_display_height) then
	   iolabel(nlabels+1) = 
     *     'Map too large for adding, re-specify'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	   nlabels = nlabels + 1
	   go to 2100
	  end if
	 else
C*** transform
	  if (nox*2 .gt. max_display_width .or.
     *        noy .gt.max_display_height) then
	   iolabel(nlabels+1) = 
     *     'Transform too large for adding, re-specify'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	   nlabels = nlabels + 1
	   go to 2100
	  end if
	 end if
	end if
	go to 2600
C*** check image size not too big for pixmap
 2500	call ximagecheckimage
     *  (max_display_width,max_display_height,1,ierr)
	if(ierr.ne.0) then
	 if(idevin .eq. 0) then
	  iolabel(nlabels+1) =
     *    'Insufficient resources for size of image, please re-enter:'
	  nlabels = nlabels + 1
	  go to 2100
	 else
	  write(6,'(
     *    ''*** Error - insufficient resources for size of image'')')
	  stop
	 end if
	end if
C***
C*** set up z values
 2600   nzstart = ixyzmin(3)
        nzend = ixyzmax(3)
C*** needs resetting when rescaling
 2700   if(nxyz(3) .eq. 1) noz = 0
C*** change density limits
 2750   if(idevin .eq. 0) then
         if(iclick .eq. 0)then
	  write(iolabel(nlabels+1),
     *    '('' current min,max density limits = '',2g12.4)')
     *    amin,amax
          if(mode .lt. 3)then
            if(idf_mapmode.eq.2)then
              if(idf_picmode.eq.1) then
                imin = 0
                imax = 2000
	      else if (idf_picmode.eq.2) then
                imin = -10
                imax = 50
	      else if (idf_picmode.eq.3) then
                imin = -5
                imax = 10
	      else if (idf_picmode.eq.4) then
                imin = -10
                imax = 50
	      else
                imin = -10
                imax = 50
              endif
              write(iolabel(nlabels+2),'(
     *        '' type density limits ("<cr>"= '',I4,'','',I6,''):'')')imin,imax
            else
              write(iolabel(nlabels+2),'(
     *        '' type density limits ("<cr>"= current):'')')  
            endif
          else
	    if(iredcen.eq.1)then
              write(iolabel(nlabels+2),'(
     *        '' type density limits ("<cr>"= min, 300):'')')  
            else
              write(iolabel(nlabels+2),'(
     *        '' type density limits ("<cr>"= min, 1000):'')')  
            endif
          endif
 2800	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	  nlabels = 1
	  if(return_string .eq. ' ' .or. return_string .eq. '/') then
  	    bmin = amin
            if(mode .lt. 3)then
              if(idf_mapmode.eq.2)then
                bmin = imin
                bmax = imax
              else
    	        bmax = amax
              endif
            else
              if(iredcen.eq.1)then
	        bmax = 300.0
              else
	        bmax = 1000.0
	      endif
            endif
            if(amax.lt.bmax) then
              bmax=amax
              endif
	    else
	      call extract_reals(2,return_string,bmin,bmax,f3,f4,f5,f6)
	      if(bmin .gt. bmax) go to 2800
	    end if
          else
            bmin=amin
            bmax=amax
C           write(6,'(''Skipping min,max density definition'')')
	  end if
C*** read from file
	else
	 read(idevin,*) bmin,bmax
	 if(idevin .ne. 5) then
	  write(iolabel(nlabels+1),
     *    '('' file min,max density limits = '',2g12.4)') amin,amax
	  iolabel(nlabels+2) = 'Modify default values :'
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
	  go to 2750
	end if
C***********************************************************************
C*** call routines for reading image/transform and displaying to screen
C***********************************************************************
	call ximagelabeldisplay(iolabel,nlabels)
        if(image) then
         call imread
C*** Movie request for new map
	 if(nmaps .lt. 0) then
	  call ximagemenuhide
	  call ximageclearimage
	  nmaps = 0
          newmap = .true.
	  go to 1000
	 end if
	 ixshift = nxstart - ixmin
	 iyshift = nystart - iymin
        else
         call fftread
	 ixshift = -icenx
	 iyshift = -iceny
        end if
	call ximagechangecursor(icurs)
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
	menulist(11) = 'Display more sections'
	menulist(12) = 'Average boxed densities'
	menulist(13) = 'Box/Dump an area'
	menulist(14) = 'Output map coordinates'
	menulist(15) = 'Compute interactive FFT'
	menulist(16) = 'Lattice refinement'
	menulist(17) = 'CTF fitting'
	menulist(18) = 'Splinefit image'
	menulist(19) = 'Center image'
	menulist(20) = 'Display Peaklist'
	menulist(21) = 'Return'
	call ximagemenuinit(menulist,21)
	job = -1
        if(iclick .eq. 1)then
          job=14
          goto 5002
        endif
	if(idefoci.ne.0)then
	  job=17
C	  write(*,'('' Splitting to ctffit'')')
	  goto 5002
	endif
 5000   call ximagewait(job)
	if(job.le.0) then
	 go to 5000
        endif
 5002   continue
C*** rescale image
	if(job.eq.1) then
	 call ximagemenuhide
	 if(redisplay) then
	  iolabel(nlabels+1) = ' option not possible...'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	  go to 4100
	 end if
	 newmap = .true.
	 call ximageclearimage
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
	 call colour_table
	 if(rescale) then
	  if(image) then
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
	 menulist(5) = 'Cross'
	 menulist(6) = 'Circle'
	 menulist(7) = 'Hand'
	 menulist(8) = 'Bird'
	 menulist(9) = 'Skull'
	 call ximagemenuinit(menulist,9)
	 job = -1
 5100    call ximagewait(job)
	 if(job.le.0) go to 5100
	 icurs = job - 1
	 call ximagechangecursor(icurs)
	 call ximagemenuhide
C*** test for incorrect bit pattern reverse
	 if(icurs .gt. 0 .and. icurs .lt. 4) then
	  return_string = ' '
	  iolabel(nlabels+1) = 
     *    'If the cursor bit pattern is correct, type ... <cr>'
	  iolabel(nlabels+2) = 
     *    'To reverse cursor bit pattern type ............  1'
	  call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	  if(return_string .ne. ' ') then
	   icurs = -icurs
	   call ximagechangecursor(icurs)
	  end if
	 end if
	 go to 4000
C*** cursor tracking switch
	else if(job.eq.9) then
	 if(icompress .ne. 1) go to 8000
	 if(pointer) then
	  call ximagepointertrackoff
	  pointer = .false.
	 else
	  call ximagepointertrackon(ixshift,iyshift)
	  pointer = .true.
	 end if
	 go to 4000
C*** read another map
	else if(job.eq.10) then
	 if(mapread) call imclose(idevmap)
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
	 else if(job.eq.1) then
	  call ximagemenuhide
	  call removevectors
	  call ximageclearimage
          newmap = .true.
C*** add new map
	 else if(job.eq.2) then
	  call ximagemenuhide
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
C*** return main menu
	 else if(job.eq.3) then
	  call ximagemenuhide
	  go to 4000
	 end if
	 call ximagemenuhide
	 go to 1000
C*** display more sections
	else if(job.eq.11) then
	 call ximagemenuhide
	 call ximagelabelhide
	 if(image) then
	  go to 2000
	 else
	  go to 8000
	 end if
C*** average boxed densities
	else if(job.eq.12) then
	 if(redisplay .or. icompress .ne. 1) go to 8000
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
	 call output_coords
	 go to 4000
C*** compute FFT
	else if(job.eq.15) then
	 if(nmaps .eq. 1 .and. image) call fftdisplay
	 go to 4000
C*** lattice refinement
	else if(job.eq.16) then
	 call lattice
	 go to 4000
C*** ctffit
	else if(job.eq.17) then
	  call ctffit
	  if(idefoci.gt.0)then
	    job=5
	    nlabels=0
	    call imclose(idevmap)
 	    goto 4098
  	  else
	    goto 4000
	  endif
C*** splinefit
	else if(job.eq.18) then
	 if(image) then
	  call splinefit
	 else
	  go to 8000
	 end if
	 go to 4000
CAND set pan_x and pan_y to center of image
	else if(job.eq.19) then
	  call ximagesetpan((max_screen_width-2*nox)/2,(max_screen_height-noy)/2)
	  go to 4000
CHEN display peak list
	else if(job.eq.20) then
	  call ximagemenuhide
	  call ximagelabelhide
	  call show_peaks
	  go to 4000
C*** quit
	else if(job.eq.21) then
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
	go to 4000
C*****************************************************************
C*** end of menu items
C*****************************************************************
9000	continue
        if(mapread) call  imclose(idevmap)
	call ximagemenuhide
	call removevectors
	call ximageclearimage
        newmap = .true.
        goto 4102
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
	include		 'hXM_common.for'
C***
C*** loop over sections : check for auto positioning of sections
	scl = grey/(amax - amin)
	nmaps = 1
	noz = noz + nzend - nzstart + 1
        auto = .false.
  50 	if(nzstart.lt.nzend) then
	 if(idevin .eq. 0) then
	  menulist(1) = 'Auto display of sections'
	  menulist(2) = 'Semi-auto display of sections'
	  menulist(3) = 'Manual display of sections'
	  menulist(4) = 'Scrolling display of sections'
	  menulist(5) = 'Movie display'
	  menulist(6) = 'Return main menu'
	  call ximagemenuinit(menulist,6)
	  job = -1
  100     call ximagewait(job)
	  if(job.le.0) then
	   go to 100
C*** Auto display
	  else if(job.eq.1) then
	   call ximagemenuhide
	   icx = nox / 2 + 5
	   icy = max_screen_height - (noy / 2 + 5)
	   ixcr = nox + 5
	   iycr = noy + 5
	   numsecx = max_screen_width / ixcr
	   inum = 0
           icenx = icx
           iceny = icy
C*** check final size
	   numsecy = noz / numsecx + min(1,mod(noz,numsecx))
	   if( noy * numsecy .gt. max_screen_height) then
	    iolabel(nlabels+1) = 
     *      'Too many sections, re-specify start and end sections'
	    return_string = ' '
 	    call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	    call extract_integers(2,return_string,
     *      nzstart,nzend,i3,i4,i5,i6)
	    noz = nzend - nzstart + 1
	    go to 50
	   end if
	   auto = .true.
C*** semi-auto display of sections
	  else if(job.eq.2) then
	   call ximagemenuhide
	   auto = .true.
	   write(iolabel(nlabels+1),
     *     '('' number of points (x,y,z)='',3i5,
     *       '' max screen width, height ='',2i5)') 
     *      nxyz, max_screen_width, max_screen_height
           iolabel(nlabels+2) = 
     *     ' type x and y coordinates for the centre of section 1'
	   iolabel(nlabels+3) =
     *     ' increments in x and y, and the number of sections per line'
  200	   return_string = ' '
	   call ximageioboxdisplay(iolabel,return_string,nlabels+3)
	   call extract_integers(5,return_string,
     *     icx,icy,ixcr,iycr,numsecx,i6)
	   if(numsecx .le. 0) go to 200
           inum = 0
           icenx = icx
           iceny = max_screen_height - icy
C*** check boundaries
	   numsecy = noz / numsecx
	   if(icenx + (numsecx - 1) * ixcr + nox / 2 + 1 
     *                      .gt. max_screen_width) then
	    iolabel(nlabels+1) = 'These values take image out of bounds in x'
	    call ximagelabeldisplay(iolabel,nlabels+1)
	    go to 50
	   else if(iceny - (numsecy - 1) * iycr - noy / 2 + 1 .lt. 0) then
	    iolabel(nlabels+1) = 'These values take image out of bounds in y'
	    call ximagelabeldisplay(iolabel,nlabels+1)
	    go to 50
	   end if
C*** manual display
	  else if(job.eq.3) then
	   call ximagemenuhide
C*** scrolling or mivie display
	  else if(job .eq. 4 .or. job .eq. 5) then
	   call ximagemenuhide
  220      call ximagecheckimage(nox,noy,noz,ierr)
C*** insufficient memory for this number of sections
	   if(ierr .eq. 1) then
  250	    iolabel(nlabels+1) = 
     *      'Insufficient memory for this request, please re-enter'
	    iolabel(nlabels+2) = 
     *      ' limits (xmin,max,ymin,max,zmin,max)'
	    iolabel(nlabels+3) = ' [ (0,0) is lower-left corner ] :'
	    return_string = ' '
	    call ximageioboxdisplay(iolabel,return_string,nlabels+3)
	    call extract_integers(2,return_string,
     *      nzstart,nzend,i3,i4,i5,i6)
	    if(nzend .le. 0) go to 250
	    noz = nzend - nzstart + 1
	    go to 220
	   end if
	   nmaps = noz
	   if(job .eq. 5) nmaps = -nmaps
C*** return main menu
	  else if(job .eq. 6) then
	   call ximagemenuhide
	   return
	  end if
C*** display multiple sections in non-interactive mode - use auto option
	 else
	  auto = .true.
	  icx = nox / 2 + 5
	  icy = max_screen_height - (noy / 2 + 5)
	  ixcr = nox + 5
	  iycr = noy + 5
	  numsecx = max_screen_width / ixcr
	  inum = 0
          icenx = icx
          iceny = icy
	  numsecy = noz / numsecx + min(1,mod(noz,numsecx))
C*** too many sections, calculate maximum
	  if( noy * numsecy .gt. max_screen_height) then
	   numsecy = max_screen_height / noy
	   noz = numsecy / numsecx
	   nzend = nzstart + noz - 1
	  end if
	 end if
	end if
C***
C*** extract/calculate image centre
	imap = 0
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
          go to 400
         end if
C***
	 if(abs(nmaps) .gt. 1) then
	  if(iz.eq.nzstart) then
           icenx = max_display_width / 2
           iceny = max_display_height / 2
	  end if
	  go to 400
	 end if
C***
 300	 if(idevin .eq. 0) then
	  write(iolabel(nlabels+1),
     *    '('' section '',i4,'' enter center of image:'')') iz
	  write(iolabel(nlabels+2),
     *    '('' ("/" or "<cr>" = auto center) '')')
	  write(iolabel(nlabels+3),
     *    '('' ("0,0" = cursor positioning)'')')
	  return_string = ' '
          if(iclick.eq.0)then
  	    call ximageioboxdisplay(iolabel,return_string,nlabels+3)
          endif
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
	  if(icenx.eq.0) then
	   iolabel(nlabels+1) = 
     *     'Mark cursor position bottom left corner'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	   call ximagereadpointer(kx,ky)
	   call ximagelabelhide
	   icenx = kx + (nox/2)
	   iceny = ky + (noy/2)	
C*** error
	  else
	   go to 300
  	  end if
	 end if
C***
C*** check for out of bounds
  400    ixmin = icenx - nox / 2
         if(ixmin.lt.0) go to 500
         ixmax = ixmin + nox - 1
	 if(ixmax.gt.max_display_width) go to 500
         iymin = iceny - noy / 2
         if(iymin.lt.0) go to 500
         iymax = iymin + noy - 1
	 if(iymax.gt.max_display_height) go to 500
	 if(nox * noy .gt. screen_size) go to 500
         go to 600
500      iolabel(nlabels+1) =
     *   ' these values for centre take image out of bounds'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 nlabels = nlabels + 1
         go to 300
C***
C*** put out message to give mouse information
  600	 if(iz.eq.nzstart) then
	  iolabel(nlabels+1) = 
     *    'wait for map display ...'
	  iolabel(nlabels+2) =
     *    'to mark points, press left hand mouse button'
	  iolabel(nlabels+3) =
     *    'to pan, hold down centre mouse button'
	  iolabel(nlabels+4) =
     *    'to zoom, press right hand mouse button'
	  call ximagelabeldisplay(iolabel,nlabels+4)
	 end if
C***
C*** initialize output buffer
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
	 else
C********
C*** load map for scrolling sections and write to pixmap
C********
          do iy=1,noy * icompress,icompress
           ny = nox * (iy-1) / icompress
C*** initialize real array
	   do ix = 1,nox
	    density(ix) = 0.
	   end do
	   do ky=1,icompress
            call  irdlin(idevmap,aline)
            do ix = 1,nox * icompress,icompress
	     jx = (ix - 1) / icompress + 1
	     do kx = 1,icompress
	      lx = nxstart + ix + kx - 1
	      density(jx) = density(jx) + aline(lx)
	     end do
	    end do
	   end do
	   do ix = 1,nox
            nx = ix - 1
	    nxy = ny + nx
	    den = (density(ix) / compsq - amin) * scl
	    den = min(grey, max(0.,den))
            mapbuf(nxy) = nint(den)
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
	include		'hXM_common.for'
C***
	ierr = 0
        nox2 = nox * 2
        nyhalf = noy / 2 + 1
C***
C*** amplitudes or intensities ?
	amp = .true.
	if(idefoci.eq.0)then
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
	endif
	if(outswitch) write(idevout,'(a)') return_string
C***
C*** calculate size of transform on screen
 100	icenx=0
	iceny=0
	return_string = ' '	
CHEN>
C	if(idevin .eq. 0) then
C	 iolabel(nlabels+1) = ' enter centre of transform : '
C	 iolabel(nlabels+2) = ' ("/" or "<cr>" = auto centre)'
C	 iolabel(nlabels+3) = ' ("0,0" = cursor positioning)'
C	 return_string = ' '
C	 call ximageioboxdisplay(iolabel,return_string,nlabels+3)
C	else
C	 read(idevin,'(a)') return_string
C	end if
CHEN<
	if(outswitch) write(idevout,'(a)') return_string
	nlabels = 1
CC*** calculate centre of transform
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
C***
C*** check for out of bounds
        ixmin = icenx - nox + 1
        if(ixmin.lt.0) go to 300
        ixmax = icenx + nox - 1
        iymin = iceny - nyhalf + 1
        if(iymin.lt.0) go to 300
        iymax = iceny + nyhalf - 1
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
C*** initialize output buffer
	if(newmap) then
         do i=0,max_display_width * max_display_height / 8 - 1
          mapreal8(i) = 0
	 end do
        end if
C***
C*** set up parameters for x term, origin top left, so mapbuf will be
C*** turned upside-down relative to the screen
C*** transforms store only the rh half of the image which has an even number
C*** of lines. the origin is at coords 0,ny/2 (see image.doc).
        iystart1 = max_display_height - iymin
        iystart2 = max_display_height - iymax
        call imposn(idevmap,0,nystart)
C***
C*** display amplitudes
        if(amp) then
CHENNING----Adding fft interpolation
C C*** load single map into buffer
C          call imposn(idevmap,iz,nystart)
C 	 if(nmaps .eq. 1) then
C 	  iystart = max_display_height - iymin
C           do iy=1,noy * icompress,icompress
C            ny = max_display_width * (iystart - (iy-1) / icompress)
C C*** initialize real array
C 	   do ix = 1,nox
C 	    density(ix) = 0.
C 	   end do
C 	   do ky=1,icompress
C             call irdlin(idevmap,aline)
C             do ix = 1,nox * icompress,icompress
C 	     jx = (ix - 1) / icompress + 1
C 	     do kx = 1,icompress
C 	      lx = nxstart + ix + kx - 1
C 	      density(jx) = density(jx) + aline(lx)
C 	     end do
C 	    end do
C 	   end do
C 	   do ix = 1,nox
C             nx = ixmin + ix - 1
C 	    nxy = ny + nx
C 	    den = (density(ix) / compsq - amin) * scl
C 	    den = min(grey, max(0.,den))
C             mapbuf(nxy) = nint(den)
C 	   end do
C 	  end do
C           newmap = .false.
C 	 else
CHENNING
           do 500  iy=1,noy-1
             call  irdlin(idevmap,aline)
             ny1 = iystart1 - iy + 1
             ny2 = iystart2 + iy - 1
             do 500 ix = 1,nox2,2
               aval = aline(nxstart + ix)
               bval = aline(nxstart + ix + 1)
               istep = (ix + 1) / 2
               nx1 = icenx + istep - 1
               nx2 = icenx - istep + 1
 	       nxy1 = ny1 * max_display_width + nx1
 	       nxy2 = ny2 * max_display_width + nx2
 	       den = scl*(sqrt(aval*aval+bval*bval)-amin)
 	       den = min(grey,max(0.,den))
               mapbuf(nxy1) = nint(den)
               mapbuf(nxy2) = mapbuf(nxy1)
 500      continue
        else
C***
C*** display intensities
         do 600 iy=1,noy-1
           call  irdlin(idevmap,aline)
           ny1 = iystart1 - iy + 1
           ny2 = iystart2 + iy - 1
           do 600 ix=1,nox2,2
            aval = aline(nxstart + ix)
            bval = aline(nxstart + ix + 1)
            istep = (ix + 1) / 2
            nx1 = icenx + istep - 1
            nx2 = icenx - istep + 1
	    nxy1 = ny1 * max_display_width + nx1
	    nxy2 = ny2 * max_display_width + nx2
	    den = sclsq*((aval*aval+bval*bval)-aminsq)
	    den = min(grey,max(0.,den))
            mapbuf(nxy1) = nint(den)
            mapbuf(nxy2) = mapbuf(nxy1)
 600     continue
        end if
C***
C*** write map to screen
	call ximagedrawimage
     *  (max_display_width,max_display_height,0,mapbuf,ierr)
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
CAND****************center image
	call ximagesetpan((max_screen_width-2*nox)/2,(max_screen_height-noy)/2)
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
        include    'hXM_common.for'
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
  	menulist(3) = 'Re-draw box'
	menulist(4) = 'Calculate total average for all boxes'
	menulist(5) = 'Return main menu'
	call ximagemenuinit(menulist,5)
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
	 if(nboxes .gt. 0 .and. avout) call average_write
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
	 end if
	 go to 300
C*******************************************************
C*** draw a box by point specification
C*******************************************************
	else if(job .eq. 2) then
	 call ximagelabelhide
	 call ximagemenuhide
C*** write previous mean to output file
	 if(nboxes .gt. 0 .and. avout) call average_write
	 nboxes = nboxes + 1
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
	 go to 300
C*************************************************
C*** redraw box
C*************************************************
	else if(job .eq. 3) then
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
	 go to 300
C******************************************************
C*** calculate total average for all boxes
C******************************************************
	else if (job .eq. 4) then
	 call ximagemenuhide
	 if(nboxes .le. 0) go to 200
C*** write previous mean to output file
	 if(avout) call average_write
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
	else if (job .eq. 5) then
	 call ximagemenuhide
	 call ximagelabelhide
	 call ximageremovevectors
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
        include    'hXM_common.for'

C*** convert screen to map coordinates
	call convert_to_image(ixscreen1,iyscreen1,iximage1,iyimage1)
	call convert_to_image(ixscreen2,iyscreen2,iximage2,iyimage2)
        numx = iximage2 - iximage1 + 1
        numy = iyimage2 - iyimage1 + 1
	if(numx .gt. max_spline_width .or. numy .gt. max_spline_height) then
	 iolabel(nlabels+1) = 'Box too large for program'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	end if
C*** image
	if(image) then
	 amean = 0.
         call  imposn(idevmap,0,iyimage1)
C*** start line reading loop
         do iy=1,numy
	  call irdlin(idevmap,aline)
          do ix=iximage1,iximage2
           amean = amean + aline(ix+1)
c	   chunk(ix,iy) = aline(ix+1)
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
	ameantot = ameantot + amean
	npixels = npixels + numxy
	nlabels = nlabels + 1
	if(nlabels .ge. max_lines_per_page) nlabels = 4
	if(image) then
C*** calculate sdev
	 sumxsq = 0.
	 do iy=1,numy
c	  do ix = iximage1,iximage2
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
	 bmeantot = bmeantot + bmean
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
        include    'hXM_common.for'
	if(image) then
	 write(idevavg,'('' Mean density = '',
     *   f12.2,'' over '',i8,'' pixels'')') amean, numxy
	 ameantot = ameantot + amean
	else
         write(idevavg,'('' Mean amp, int = '',
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

        include           'hXM_common.for'
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
	  postscript = .false.
	  dump_screen = .true.
	  screen = .true.
	 end if
	 go to 550
C*** Whole screen - postscript format
	else if(job .eq. 4) then
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
	   call ximageioboxdisplay(iolabel,return_string,nlabels+3)
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
          nxpad = nox
          nypad = noy
          ncenx = nox / 2
          nceny = noy / 2
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
        include        'hXM_common.for'
        entry circle
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
	 job = -1
 1030    call ximagewait(job)
	 if(job .lt. 0) then
	  go to 1030
C*** read centre position
	 else if(job .eq. 0) then
	  call ximagereadmenupointer(icircx,icircy)
	  call draw_cross(icircx,icircy,4)
	 else if(job .eq. 1) then
	  call ximagemenuhide
	  go to 1000
C*** return main menu
	 else if(job .eq. 2) then
	  call ximagemenuhide
          ierr = 1
	  return
	 else if(job .eq. 105) then 
	  call remove_cross(icx,icy,4)
	  call ximagerubberread(icircx,icircy,iex,iey)
	  call draw_cross(icircx,icircy,4)
C*** calculate radius
          xsq = float(icircx - iex)
          xsq = xsq * xsq
          ysq = float(icircy - iey)
          ysq = ysq * ysq
          irad = nint(sqrt(ysq + xsq))
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
	 else
	  call extract_integers(2,return_string,
     *    mapx,mapy,i3,i4,i5,i6)
	  if(mapy .le. 0) go to 1040
	  call convert_to_screen(mapx,mapy,icircx,icircy)
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
 1050	menulist(1) = 'Radius specification by keyboard input'
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
 1080	 iolabel(nlabels+1) = 'Type in radius in map pixels'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 call extract_integers(1,return_string,
     *   irad,i2,i3,i4,i5,i6)
	 irad = nint(float(irad) / float(icompress))
	 if(irad .le. 0) go to 1080
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
	 call ximagelabelhide
	 go to 1080	 
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
	call ximagedrawcircle(icircx,icircy,irad)
	mnx = icircx - irad
        mxx = icircx + irad
        mny = icircy - irad
        mxy = icircy + irad
        if(mxx.gt.ixmax) go to 1300
        if(mnx.lt.ixmin) go to 1300
        if(mxy.gt.iymax) go to 1300
        if(mny.lt.iymin) go to 1300
        go to 1400
C*** diagnostic
 1300   iolabel(nlabels+1) = 'radius out of bounds'
	call ximagelabeldisplay(iolabel,nlabels+1)
        go to 1000
C***
C*** check circle acceptable - write out map coords and radius
 1400	call convert_to_image(icircx,icircy,mapx,mapy)
        write(iolabel(nlabels+1),'(
     *  '' centre coords ='',2i6,'' Radius ='',i6)')
     *  mapx,mapy,irad * icompress
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
C*** calculate box edges
 1600   numx = 2 * (icompress * (mxx - mnx + 1) / 2)
        numy = 2 * (icompress * (mxy - mny + 1) / 2)
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
	call ximagelabeldisplay(iolabel,nlabels+1)
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
	  ixp(1) = ix1
	  ixp(2) = ix2
	  ixp(3) = ix2
	  ixp(4) = ix1
	  iyp(1) = iy1
	  iyp(2) = iy1
	  iyp(3) = iy2
	  iyp(4) = iy2
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
         call ximagereadpointer(ixp(1),iyp(1))
         call ximagelabelhide
C*** draw dot to mark bottom lh corner of box
	 call draw_cross(ixp(1),iyp(1),4)
	 call ximagelabeldisplay(iolabel,nlabels)
C*** mini-menu to decide how to get box specs
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
	  call ximagereadpointer(ixp(3),iyp(3))
	  call ximagelabelhide
          ixp(2) = ixp(1)
          iyp(2) = iyp(3)
          ixp(4) = ixp(3)
          iyp(4) = iyp(1)
C*** type in box size
	 else if(job.eq.2) then
	  call ximagemenuhide
 2300     iolabel(nlabels+1) = 'Type in -ontal and vertical box size'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	  call extract_integers(2,return_string,
     *    ihor,ivert,i3,i4,i5,i6)
	  if(ivert .le. 0) go to 2300
          ixp(2) = ixp(1)
          iyp(2) = iyp(1) + ivert - 1
          ixp(3) = ixp(1) + ihor - 1
          iyp(3) = iyp(2)
          ixp(4) = ixp(3)
          iyp(4) = iyp(1)
C*** re-specify bottom left corner
	 else if(job.eq.3) then
	  call ximagemenuhide
	  ix = ixp(1)
	  iy = iyp(1)
	  iolabel(nlabels+1) = 
     *    'Mark cursor position bottom left corner of rectangle'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	  call ximagereadpointer(ixp(1),iyp(1))
	  call ximagelabelhide
C*** remove old dot and draw new dot to mark bottom lh corner of box
	  call remove_cross(ix,iy,4)
	  call draw_cross(ixp(1),iyp(1),4)
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
         ixp(1) = ixb + inch
         iyp(1) = iyb - incv
         ixp(2) = ixb - inch
         iyp(2) = iyb + incv
         ixp(3) = ixt - inch
         iyp(3) = iyt + incv
         ixp(4) = ixt + inch
         iyp(4) = iyt - incv
C**********************************************************
C*** centre, type size
C**********************************************************
	else if(job .eq. 4) then
	 call ximagemenuhide
	 iolabel(nlabels+1) = 'Mark cursor position at box centre'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 call ximagereadpointer(ixc,iyc)
	 call draw_cross(ixc,iyc,4)
 2400    iolabel(nlabels+1) = 
     *   'Type in horizontal,vertical box size in pixels'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 call extract_integers(2,return_string,
     *   ihsize,ivsize,i3,i4,i5,i6)
	 if(ivsize .le. 0) go to 2400
	 ihsize = ihsize / icompress
	 ivsize = ivsize / icompress
         ih2 = ihsize / 2
         iv2 = ivsize / 2
         iofx = mod(ihsize,2)
         iofy = mod(ivsize,2)
         ixp(1) = ixc - ih2
         ixp(2) = ixp(1)
         ixp(3) = ixc + ih2 - 1 + iofx
         ixp(4) = ixp(3)
         iyp(1) = iyc - iv2
         iyp(4) = iyp(1)
         iyp(2) = iyc + iv2 - 1 + iofy
         iyp(3) = iyp(2)
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
        iolabel(nlabels+1) = 'Map box coordinates'
        do i=1,npts
	 nlabels = nlabels + 1
	 if(screen) then
          write(iolabel(nlabels+1),'(2i6)') ixp(i), iyp(i)
	  if(ixp(i) .lt. 0 .or. ixp(i) .gt. max_display_width .or.
     *       iyp(i) .lt. 0 .or. iyp(i) .gt. max_display_height) then
	   iolabel(nlabels+1) = 
     *     'Specified box out of bounds for pixmap, resetting bounds...'
	   call ximagelabeldisplay(iolabel,nlabels+1)
	   ixp(i) = max(ixp(i),0)
	   iyp(i) = max(iyp(i),0)
	   ixp(i) = min(ixp(i),max_display_width)
	   iyp(i) = min(iyp(i),max_display_height)
	  end if
	 else
	  call convert_to_image(ixp(i),iyp(i),mapx,mapy)
          write(iolabel(nlabels+1),'(2i6)') mapx, mapy
	  if(mapx .lt. nxstart .or. mapx .gt. nxend .or.
     *       mapy .lt. nystart .or. mapy .gt. nyend)
     *    coordsout = .true.
	 end if
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
	call ximagedrawbox(ixp(1),iyp(1), ixp(3), iyp(3))
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
	 call ximagedrawbox(ixp(1),iyp(1), ixp(3), iyp(3))
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
C*** polygonal box
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
	  ix1 = ixp(npts-1)
	  iy1 = iyp(npts-1)
	  ix2 = ixp(npts)
	  iy2 = iyp(npts)
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
	 ixp(npts) = ix1
	 iyp(npts) = iy1
	 call ximagedrawlines(ix1,iy1,ix2,iy2,1)
	 go to 3200
C*** final position
	else if(job .eq. 105) then
	 call ximagelabelhide
	 call ximagemenuhide
	 call ximagerubberread(ix1,iy1,ix2,iy2)
	 if(ix1 .eq. ixp(npts) .and. iy1 .eq. iyp(npts)) go to 3300
	 npts = npts + 1
	 if(npts.gt.max_points) then
	  nlabels = 2
	  iolabel(nlabels) = 'Too many vertices for program'
	  ierr = 1
	  return
	 end if
	 ixp(npts) = ix1
	 iyp(npts) = iy1
C*** reject if not enough points recorded
	 if(npts .eq. 1) go to 3100
C*** same point recorded twice, skip 2nd set of coords
 3300	 if(ix1 .eq. ix2 .and. iy1 .eq. iy2) go to 3400
	 npts = npts + 1
	 ixp(npts) = ix2
	 iyp(npts) = iy2
	 call ximagedrawlines(ix1,iy1,ix2,iy2,1)
C*** draw extra line to close polygon
 3400   if(ixp(npts) .eq. ixp(1) .and. iyp(npts) .eq. iyp(1)) then
	  npts = npts - 1
	 else
	  call ximagedrawlines(ixp(1),iyp(1),ixp(npts),iyp(npts),1)
	 end if
	end if
C***
C*** box finished, draw final line if not drawn already
 3500	if(ixp(1).eq.ixp(npts) .and. iyp(1).eq.iyp(npts)) then
         npts = npts - 1
	else
         call ximagedrawlines(ixp(npts),iyp(npts),ixp(1),iyp(1),1)
        end if
C*** write vertices
	nlabels = 1
        iolabel(nlabels+1) = 'Map vertex coordinates'
        do i=1,npts
	 nlabels = nlabels + 1
	 call convert_to_image(ixp(i),iyp(i),mapx,mapy)
         write(iolabel(nlabels+1),'(2i6)') mapx, mapy
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
	  call ximagedrawlines(ixp(i),iyp(i),ixp(i+1),iyp(i+1),1)
	 end do
         call ximagedrawlines(ixp(npts),iyp(npts),ixp(1),iyp(1),1)
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
	mnx = max_display_width
        mxx = 0
        mny = max_display_height
        mxy = 0
        do i=1,npts
         mnx = min(mnx,ixp(i))
         mxx = max(mxx,ixp(i))
         mny = min(mny,iyp(i))
         mxy = max(mxy,iyp(i))
	end do
C***
	ixp(npts+1) = ixp(1)
        iyp(npts+1) = iyp(1)
C***
	numx = 2 * (icompress * (mxx - mnx + 1) / 2)
	numy = 2 * (icompress * (mxy - mny + 1) / 2)
	ierr = 0
        return
        end
C*************************************************************************
        subroutine box_out
C*************************************************************************
C***
C*** subroutine to write the output file either from the input
C*** map or dumped directly from the screen
        include    'hXM_common.for'
C***
C*** create output file for boxed image
        call imopen(idevout,boxfile,'new')
        ixyz(1) = nxpad
        ixyz(2) = nypad
        ixyz(3) = 1
        kxyz(1) = nxpad
        kxyz(2) = nypad
        kxyz(3) = 1
        bignum = 10.0e+10
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
        call icrhdr(idevout,ixyz,kxyz,imode,title,0)
        call ialorg(idevout,xorigin,yorigin)
        call itrlab(idevout,1)
	call getdate(date)
	title = ' IMDISP boxed off selected area '//date
        call  iwrhdr(idevout,title,1,boxmin,boxmax,boxmean)
C***
C*** calculate start & finishing positions
        ixstart = ncenx - numx / 2
        iystart = nceny - numy / 2
C*** mod 07.08.97
c	 ixend = ncenx + numx / 2
c	 iyend = nceny + numy / 2
	ixend = ixstart + numx + 1
	iyend = iystart + numy + 1
	call convert_to_image(mnx,mny,iminx,iminy)
C*** mod 08.08.97
c	mxx = mnx + numx - 1
c	mxy = mny + numy - 1
	imaxx = iminx + numx - 1
	imaxy = iminy + numy - 1
	background = 0.
	backgd = 0.
C*************************************************************
C*** Output file boxed from screen
C*************************************************************
        if(screen) then
         do i = mny, mxy
	  do j=1,nxpad
	   density(j) = 0.
	  end do
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
	 call convert_to_image(icircx,icircy,mcircx,mcircy)
	 rad = float(irad * icompress)
	 radsq = rad * rad
	 if(floatim) then
	  icount = 0
	  call imposn(idevmap,0,iminy)
	  do iy = 1,numy
	   call irdlin(idevmap,aline)
	   fy = rad - float(iy)
	   fysq = fy * fy
	   fxsq = radsq - fysq
	   if(fxsq .gt. 0.) then
	    ishift = nint(sqrt(fxsq))
	   else
	    ishift = 0
	   end if
C*** calculate start, end horiz positions, add 1 as aline starts at 1
	   ix1 = mcircx - ishift + 1
	   ix2 = mcircx + ishift + 1
	   icount = icount + ix2 - ix1 + 1	
	   do ix = ix1,ix2
	    background = background + aline(ix)
	   end do
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
	  end if
	 end if
C*** box circle for real
	 call imposn(idevmap,0,iminy)
	 do iy = 1,numy
	  do ix = 1,nxpad
	   density(ix) = backgd
	  end do
	  call irdlin(idevmap,aline)
	  fy = rad - float(iy)
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
	 ix1 = iminx + 1
	 ix2 = imaxx + 1
	 if(invert) then
	  call imposn(idevmap,0,0)
	  do iy=1,nypad
	   call irdlin(idevmap,aline)
	   if(iy .gt. iminy .and. iy .lt. imaxy) then
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
	   call imposn(idevmap,0,iminy)
	   do iy=1,numy
	    call irdlin(idevmap,aline)
	    do ix = ix1, ix2
	     background = background + aline(ix)
	    end do
	   end do
	   background = background / float(numx * numy)
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
	   end if
	  end if
C*** cut image for real
	  call imposn(idevmap,0,iminy)
	  do ix=1,nxpad
	   density(ix) = backgd
	  end do
	  do iy=1,numy
	   call irdlin(idevmap,aline)
	   nshift = ixstart - iminx
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
	 ix1 = iminx + 1
	 ix2 = imaxx + 1
	 do i=1,npts+1
	  call convert_to_image(ixp(i),iyp(i),mx,my)
	  xd(i) = float(mx)
	  yd(i) = float(my)
	 end do
C*** calculate background for polygon
	 if(floatim) then
	  call imposn(idevmap,0,iminy)
	  icount = 0
	  do iy = iminy,imaxy
	   call irdlin(idevmap,aline)
	   y = float(iy)
	   do ix = ix1, ix2
	    x = float(ix)
C*** if point inside polygon include background
	    if(box_inside(x,y,npts,xd,yd)) then
	     background = background + aline(ix)
	     icount = icount + 1
	    end if
	   end do
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
	  end if
	 end if
C*** box polygon for real
	 call imposn(idevmap,0,iminy)
	 do iy=iminy,imaxy
	  call irdlin(idevmap,aline)
	  y = float(iy)
	  do ix=1,nxpad
	   density(ix) = backgd
	  end do
	  nshift = ixstart - iminx
	  do ix = ix1,ix2
	   x = float(ix)
C*** if point inside polygon include background
	   if(box_inside(x,y,npts,xd,yd)) then
            den = aline(ix) - background
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
          call  iwrlin(idevout,density)
         end do
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
	subroutine check_file(filename)
C***
C********************************************************************
C*** subroutine to check existence of file
	character	filename*(*)
	include 'hXM_common.for'
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
        include    'hXM_common.for'
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
	 do i=min_den+1,max_den
	  irgb = min(max_colour,nint(colourscale * float(i)))
	  red(i) = irgb
	  green(i) = irgb
	  blue(i) = irgb
	 end do
	 call ximagesetcolourtable(min_den,max_den,red,green,blue)
	 first = .false.
	 return
	end if
	call ximagemenuhide
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
	menulist(12) = 'Return main menu'
	call ximagemenuinit(menulist,12)
	job = -1
  100   call ximagewait(job)
	if(job.le.0) then
	 go to 100
C*******************************************************************
C*** Black/white
C*******************************************************************
	else if(job .eq. 1) then
	 call ximagemenuhide
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
	 write(iolabel(nlabels+1),'(''min, max, mean ='',3f)') 
     *   amin, amax, dmean
	 iolabel(nlabels+2) = 
     *   'Use left and right sliders to set lower and upper thresholds'
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
  200    call ximagewait(iopt)
	 if(iopt .lt. 0) then
	  go to 200
C*** return main menu
	 else if(iopt .eq.0) then
	  call ximagelabelhide
	  call ximagecolourbarhide
	  call ximagesliderhide
	  write(iolabel(nlabels+1),'(''min, max ='',2f10.0)') bmin, bmax
	  call ximagelabeldisplay(iolabel,nlabels+1)
	  menulist(1) = 'Re-scale to these thresholds'
	  menulist(2) = 'Return main menu'
	  call ximagemenuinit(menulist,2)
	  jopt = -1
  300     call ximagewait(jopt)
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
     *    min(nint(percent1 * grey * 0.01),max_thresh - 1))
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
	 menulist(1) = 'Red'
	 menulist(2) = 'Green'
	 menulist(3) = 'Blue'
	 menulist(4) = 'Yellow'
	 menulist(5) = 'Magenta'
	 menulist(6) = 'Cyan'
	 menulist(7) = 'Return main menu'
	 call ximagemenuinit(menulist,7)
	 jopt = -1
  120    call ximagewait(jopt)
	 if(jopt.le.0) then
	  go to 120
C*** red
	 else if(jopt .eq. 1) then
	  rfactor = 1.0
	  gfactor = 0.0
	  bfactor = 0.0
C*** green
	 else if(jopt .eq. 2) then
	  rfactor = 0.0
	  gfactor = 1.0
	  bfactor = 0.0
C*** blue
	 else if(jopt .eq. 3) then
	  rfactor = 0.0
	  gfactor = 0.0
	  bfactor = 1.0
C*** yellow
	 else if(jopt .eq. 4) then
	  rfactor = 1.0
	  gfactor = 1.0
	  bfactor = 0.0
C*** magenta
	 else if(jopt .eq. 5) then
	  rfactor = 1.0
	  gfactor = 0.0
	  bfactor = 1.0
C*** cyan
	 else if(jopt .eq. 6) then
	  rfactor = 0.0
	  gfactor = 1.0
	  bfactor = 1.0
C*** return
	 else if(jopt .eq. 7) then
	  return
	 end if
	 call ximagemenuhide
	 do i=min_den+1,max_den
	  rgb = colourscale * float(i)
	  red(i) = min(max_colour,nint(rgb * rfactor))
	  green(i) = min(max_colour,nint(rgb * gfactor))
	  blue(i) = min(max_colour,nint(rgb * bfactor))
	 end do
	 call ximagesetcolourtable(min_den,max_den,red,green,blue)
C*** set up slider bars for controlling background and brightness
	 iolabel(nlabels+1) = 'Use slider to set background'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 percent1 = 0.0
	 call ximagesliderinit(1,0.0,0.0)
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
	  call ximagesliderread(percent)
	  irgb = min(max_colour,nint(fmaxcolour * percent * 0.01))
	  red(min_den) = irgb
	  green(min_den) = irgb
	  blue(min_den) = irgb
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
C*** return main menu
C*******************************************************************
	else if(job .eq. 12) then
	 call ximagecolourbarhide
	 call ximagesliderhide
	end if
	call ximagemenuhide
	return
	end
C******************************************************************
C***
	subroutine colour_dreamworld(cyc,frange,red,green,blue)
C***
C******************************************************************
C*** subroutine to set up dreamworld colour table
        include    'hXM_common.for'
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
	include 'hXM_common.for'
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
        include    'hXM_common.for'
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
	 subroutine colour_waterworld
     *   (nmagenta,nblue,ncyan,red,green,blue)
C***
C********************************************************************
C*** subroutine to set up waterworld colour table
        include    'hXM_common.for'
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
C*** subroutine to convert screen coordinates to image
        include    'hXM_common.for'
        if(image) then
          iximage = icompress * (ixscreen - ixmin) + nxstart
  	  iyimage = icompress * (iyscreen - iymin) + nystart
        else
          iximage = ixscreen - icenx
          iyimage = iyscreen - iceny
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
        include    'hXM_common.for'
        if(image) then
	  compression = float(icompress)
          ixscreen = nint(float(iximage - nxstart) / compression)
     *             + ixmin 
  	  iyscreen = nint(float(iyimage - nystart) / compression)
     *             + iymin
        else
          ixscreen = iximage + icenx
          iyscreen = iyimage + iceny
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
C*** at centre to map tranform coordinates starting at 1,0
        include    'hXM_common.for'
        if(image) return
C*** add 1 as arrays start at 1
	ixtrans = abs(iximage) * 2 + 1
C*** add y if on the meridian
	if(iximage .eq. 0) then
	 iytrans = nxyz(2) / 2 + iyimage
C*** add if on rhs of transform even if below equator
	else if(iximage .gt. 0) then
	 iytrans = nxyz(2) / 2 + iyimage
C*** subt if on lhs
	else
	 iytrans = nxyz(2) / 2 - iyimage
	end if 
	return
	end
C****************************************************************************
C***
      subroutine draw_cross(ix,iy,isize)
C***
C****************************************************************************
C*** subroutine to draw a small cross to the screen
C****************************************************************************
	include    'hXM_common.for'
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
        include    'hXM_common.for'
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
	include    'hXM_common.for'
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
	 go to 500
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
C*** font selected and loaded, proceed with text
  500	iolabel(nlabels+1) = 
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
	include 'hXM_common.for'
  100	if(nvalues .eq. 1) then
	 read(return_string,*,err=900) i1
	 return
	else if(nvalues .eq. 2) then
	 read(return_string,*,err=900) i1,i2
	 return
	else if(nvalues .eq. 3) then
	 read(return_string,*,err=900) i1,i2,i3
	 return
	else if(nvalues .eq. 4) then
	 read(return_string,*,err=900) i1,i2,i3,i4
	 return
	else if(nvalues .eq. 5) then
	 read(return_string,*,err=900) i1,i2,i3,i4,i5
	 return
	else if(nvalues .eq. 6) then
	 read(return_string,*,err=900) i1,i2,i3,i4,i5,i6
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
	include 'hXM_common.for'
  100	if(nvalues .eq. 1) then
	 read(return_string,*,err=900) f1
	 return
	else if(nvalues .eq. 2) then
	 read(return_string,*,err=900) f1,f2
	 return
	else if(nvalues .eq. 3) then
	 read(return_string,*,err=900) f1,f2,f3
	 return
	else if(nvalues .eq. 4) then
	 read(return_string,*,err=900) f1,f2,f3,f4
	 return
	else if(nvalues .eq. 5) then
	 read(return_string,*,err=900) f1,f2,f3,f4,f5
	 return
	else if(nvalues .eq. 6) then
	 read(return_string,*,err=900) f1,f2,f3,f4,f5,f6
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
	include 'hXM_common.for'
C***
	autoscale = .true.
	amp = .true.
	poly = .false.
	ifirst = .true.
	ixscreen = 0
	iyscreen = 0
	ipixmap = 1
	xsize = 0.
	transmin = 0.
	transmax = 20.
	imode = 1
  50	call ximagemenuhide
	call ximageoverlayhide
	iolabel(nlabels+1) = 'Select transform size'
  60    call ximagelabeldisplay(iolabel,nlabels+1)
	menulist(1) = '128 x 128'
	menulist(2) = '256 x 256'
	menulist(3) = 'Specify rectangular size'
	menulist(4) = 'Box/pad/float an area first'
	menulist(5) = 'Return main menu'
	call ximagemenuinit(menulist,5)
	job = -1
  100   call ximagewait(job)
	if(job .le. 0) then
	 go to 100
C*** 128 x 128
	else if(job .eq. 1) then
	 nxbox = 128
	 nybox = 128
	 call ximageremovevectors
C*** 256 x 256
	else if(job .eq. 2) then
	 nxbox = 256
	 nybox = 256
	 call ximageremovevectors
C*** specify box size
	else if(job .eq. 3) then
	 call ximagemenuhide
	 call ximagelabelhide
	 iolabel(nlabels+1) = 'Type number in x and y (e.g. 256 512)'
  200	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 call extract_integers(2,return_string,
     *   nxbox,nybox,i3,i4,i5,i6)
C*** force transform even
	 nxbox = nxbox * 2 / 2
	 nybox = nybox * 2 / 2
	 if(nxbox * nybox .gt. max_overlay) then
	  iolabel(nlabels+1) = 'Transform too large, re-specify :'
	  go to 200
	 end if
CHEN****	 call ximageremovevectors
C*** box/pad/float an area first
	else if(job .eq. 4) then
	 poly = .true.
	 call polygon
	 if(npts .eq. 0) then
	  call ximagelabelhide
CHEN****         ximageremovevectors
	  nlabels = 1
	  return
	 end if
C*** calculate box edges and centre of gravity
	 avx = 0.
	 avy = 0.
	 call convert_to_image(mnx,mny,iminx,iminy)
	 call convert_to_image(mxx,mxy,imaxx,imaxy)
C*** force number in x,y even
	 if(mod(imaxx - iminx,2) .ne. 0) iminx = iminx + 1
	 if(mod(imaxy - iminy,2) .ne. 0) iminy = iminy + 1
C*** box centre in screen coords
	 ixscreen = mnx + (mxx - mnx) / 2
	 iyscreen = mny + (mxy - mny) / 2
C*** convert to image and calculate c of g
	 do i=1,npts + 1
	  call convert_to_image(ixp(i),iyp(i),ixpt,iypt)
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
C*** calculate padded box sixe
	 xdist = float(imaxx - iminx)
	 ydist = float(imaxy - iminy)
	 nxbox = nint(sqrt(xdist * xdist + ydist * ydist))
	 nybox = 64
 220	 nybox = nybox * 2
	 if(nxbox .gt. nybox) go to 220
	 nxbox = nybox
C*** modify box size
	 write(iolabel(nlabels+1),
     *   '(''Minimum padded box size = '',i6,'' * '',i6)') nxbox, nybox
	 iolabel(nlabels+2) =
     *   'Type new size or <cr> to accept default'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	 if(return_string .ne. ' ') 
     *   call extract_integers(2,return_string,
     *   nxbox,nybox,i3,i4,i5,i6)
	 nxtrans = nxbox + 2
	 if(nxtrans * nybox .gt. max_overlay) then
	  iolabel(nlabels+1) = 'Box too large for program'
	  go to 60
	 end if
C*** calculate background
	 background = 0.
	 call imposn(idevmap,0,iminy)
C*** add 1 as coords origin starts at 0 but array starts at 1
	 ix1 = iminx + 1
	 ix2 = imaxx + 1
	 icount = 0
	 do iy = iminy,imaxy
	  call irdlin(idevmap,aline)
	  y = float(iy)
	  do ix = ix1, ix2
	   x = float(ix)
C*** if point inside polygon include background
	   if(box_inside(x,y,npts,xd,yd)) then
	    background = background + aline(ix)
	    icount = icount + 1
	   end if
	  end do
	 end do
	 background = background / float(icount)
	 boxmin = 1000000.
	 boxmax = -1000000.
	 boxmean = 0.
C*** initialize map to background or 0
	 nyshift = nybox / 2 - avy
	 do iy=1,nybox
	  ixy = nxtrans * (iy-1)
	  do ix=1,nxtrans
	   transform(ixy+ix) = 0
	  end do
	 end do
C*** box polygon for real
	 nxshift = nxbox / 2 - avx
	 call imposn(idevmap,0,iminy)
	 do iy=iminy,imaxy
	  ixy = nxtrans * (nybox - nyshift - 1)
	  nyshift = nyshift + 1
	  call irdlin(idevmap,aline)
	  y = float(iy)
C*** test point inside polygon
	  do ix = ix1,ix2
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
	   jxy = nxshift + ix - ix1
	   nxy = ixy + jxy
	   transform(nxy) = den
	  end do
	 end do
	 boxmean = boxmean / float(nxbox * nybox)
	 boxscl = grey / (boxmax - boxmin)
C*** send boxed,padded, floated area to display
  225    do i=1,nxbox * nxtrans
	  mapbuf(i) = min(grey,max(0.,boxscl * (transform(i)-boxmin)))
	  mapout(i) = transform(i)
	 end do
         call ximageoverlayinit
     *   (ixscreen,iyscreen,nxtrans,nybox,mapbuf,ierr)
	 if(ierr .ne. 0) then
	  iolabel(nlabels+1) = 
     *    'Error in display, press any key to return main menu'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	  return
	 end if
C*** rotate before transform ?
  230    iolabel(nlabels+1) = 'Select rotation method or compute fft'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 menulist(1) = 'Automatic rotation'
	 menulist(2) = 'Specify rotation angle'
	 menulist(3) = 'Rescale image'
	 menulist(4) = 'Compute fft'
	 menulist(5) = 'Return main menu'
	 call ximagemenuinit(menulist,5)
	 job = -1
  250    call ximagewait(job)
	 if(job .le. 0) then
	  go to 250
C*** rotate the boxed area
	 else if(job .eq. 1 .or. job. eq. 2) then
	  call ximagemenuhide
	  call ximagelabelhide
C*** calculate rotation angle automatically
	  if(job .eq. 1) then
C*** find the longest side
	   distmax = 0.
	   do i=1,npts
	    if(i .lt. npts) then
	     xdist = ixp(i+1) - ixp(i)
	     ydist = iyp(i+1) - iyp(i)
	    else
	     xdist = ixp(npts) - ixp(1)
	     ydist = iyp(npts) - iyp(1)
	    end if
	    dist = xdist * xdist + ydist * ydist
	    if(dist .gt. distmax) then
	     distmax = dist
	     rotang = atan2(xdist , ydist)
	    end if
	   end do	   
C*** rotation by specification
	  else 
	   iolabel(nlabels+1) = 
     *     'Type rotation angle in degrees (+ve clockwise)'
	   return_string = ' '
	   call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	   if(return_string .ne. ' ') 
     *     call extract_integers(1,return_string,
     *     itheta,i2,i3,i4,i5,i6)
	   rotang = -degrad * float(itheta)
	  end if
C*** now perform the rotation
	  call rotate(rotang,nxtrans,nybox,transform,mapout)
C*** copy real map to byte
	  do i=1,nxbox * nxtrans
	   mapbuf(i) = min(grey,max(0.,boxscl * (mapout(i)-boxmin)))
	  end do
          call ximageoverlayinit
     *    (ixscreen,iyscreen,nxtrans,nybox,mapbuf,ierr)
	  if(ierr .ne. 0) then
	   iolabel(nlabels+1) = 
     *     'Error in display, press any key to return main menu'
	   return_string = ' '
	   call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	   return
	  end if
	  go to 230
C*** rescale image
	 else if(job .eq. 3) then
	  call ximagelabelhide
	  call ximagemenuhide
          write(iolabel(nlabels+1),
     *    '('' current min,max density limits = '',2g12.4)')
     *    boxmin, boxmax
          write(iolabel(nlabels+2),'(
     *    '' type density limits for display :'')')  
 260	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	  if(return_string .ne. ' ' .and. return_string .ne. '/') then
	   call extract_reals(2,return_string,tmin,tmax,f3,f4,f5,f6)
	   if(tmin .gt. tmax) go to 260
	   if(tmin .ne. 0. .or. tmax .ne. 0.) then
	    boxmin = tmin	 
	    boxmax = tmax
	   end if
	  end if
	  boxscl = grey / (boxmax - boxmin)
	  go to 225
C*** compute fft
	 else if(job .eq. 4) then
	  call ximagemenuhide
	  call ximagelabelhide
C*** copy map to transform array
	  do i=1,nxbox * nxtrans
	   transform(i) = mapout(i)
	  end do
	  go to 270
C*** return main menu
	 else if(job .eq. 5) then
	  call ximageoverlayhide
	  call ximageremovevectors
	  call ximagemenuhide
	  call ximagelabelhide
	  return
	 end if
C*** calculate transform
  270    call TDXFFT(transform,nxbox,nybox,0)
C*** calculate min,max,mean if first time round
	 if(ifirst) then
	  call iclcdn(transform,nxtrans/2,nybox,1,nxtrans/2,1,nybox,
     *              transmin,transmax,transmean)
	  ifirst = .false.
	 end if
	 call fftcalc
C*** send transform map to display
	 call ximageoverlayinit
     *   (ixscreen,iyscreen,nxtrans-1,nybox-1,transbuf,ierr)
	 if(ierr .ne. 0) then
	  iolabel(nlabels+1) = 
     *    'Error in display, press any key to return main menu'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	  return
	 end if
	 go to 300
C*** return main menu
	else if(job .eq. 5) then
	 call ximagelabelhide
	 call ximageremovevectors
	 nlabels = 1
	 return
	end if
C***
C*** pre-selected transform size
	call ximagemenuhide
	call ximagelabelhide
C***
C*** menu loop to compute transforms
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
	menulist(3) = 'Switch amplitudes/intensities'
	menulist(4) = 'Save boxed area to file'
	menulist(5) = 'Save transform as postscript file'
	menulist(6) = 'Hide transform overlay'
	menulist(7) = 'Return main menu'
	call ximagemenuinit(menulist,7)
	job = -1
  500   call ximagewait(job)
	if(job .lt. 0) then
	 go to 500
C*** compute a transform
	else if(job .eq. 0) then
	 if(poly) go to 500
	 call ximagereadmenupointer(ixscreen, iyscreen)
	 call convert_to_image(ixscreen,iyscreen,ixim,iyim)
	 go to 1000
C*** modify box
	else if(job .eq. 1) then
	 call ximagelabelhide
	 call ximagemenuhide
	 poly = .false.
	 go to 50
C*** modify scale factors
	else if(job .eq. 2) then
	 autoscale = .false.
	 call ximagelabelhide
	 call ximagemenuhide
         write(iolabel(nlabels+1),
     *   '('' min,max density limits = '',2g12.4)')
     *   transmin,transmax
         write(iolabel(nlabels+2),'(
     *   '' type density limits for display :'')')  
 700	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+2)
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
	 call ximageoverlayinit
     *   (ixscreen,iyscreen,nxtrans-1,nybox-1,transbuf,ierr)
	 if(ierr .ne. 0) then
	  iolabel(nlabels+1) = 
     *    'Error in display, press any key to return main menu'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	  return
	 end if
	 go to 300
C*** switch amplitudes/intensities
	else if(job .eq. 3) then
	 call ximagelabelhide
	 call ximagemenuhide
	 if(amp) then
	  amp = .false.
	 else
	  amp = .true.
	 end if
	 if(ifirst) go to 300
	 call fftcalc
C*** send transform map to display
	 call ximageoverlayinit
     *   (ixscreen,iyscreen,nxtrans-1,nybox-1,transbuf,ierr)
	 if(ierr .ne. 0) then
	  iolabel(nlabels+1) = 
     *    'Error in display, press any key to return main menu'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	  return
	 end if
	 go to 300
C*** save boxed area
	else if(job .eq. 4) then
	 call ximagelabelhide
	 call ximagemenuhide
	 iolabel(nlabels+1) = 'Output file name'
	 boxfile = ' '
	 call ximageioboxdisplay(iolabel,boxfile,nlabels+1)
	 old = .false.
 	 call check_file(boxfile)
         call imopen(idevout,boxfile,'new')
         ixyz(1) = nxbox
         ixyz(2) = nybox
         ixyz(3) = 1
         kxyz(1) = nxbox
         kxyz(2) = nybox
         kxyz(3) = 1
         call icrhdr(idevout,ixyz,kxyz,imode,title,0)
	 xorigin = float(nxbox / 2)
	 yorigin = float(nybox / 2)
         call ialorg(idevout,xorigin,yorigin)
	 call getdate(date)
	 itheta = -nint(rotang / degrad)
	 lcx = nint(avx) + iminx
	 lcy = nint(avy) + iminy
	 write(title,'(
     *   ''Ximdisp boxed area '',a,'' rotation '',i4,
     *   '' degrees, map centre '',2i6)') date, itheta, lcx, lcy
         call iwrhdr(idevout,title,1,boxmin,boxmax,boxmean)
C*** write in a loop as you write nxbox, not nxtrans in x
	 do iy=1,nybox
	  ixy = nxtrans * (iy - 1)
	  do ix=1,nxbox
	   aline(ix) = mapout(ixy + ix)
	   den = aline(ix)
	  end do
	  call iwrlin(idevout,aline)
	 end do
	 call imclose(idevout)
	 go to 300
C*** dump transform to postscript file
	else if(job .eq. 5) then
	 call ximagemenuhide
	 iolabel(nlabels+1) = 
     *   'Wait for postscript dump to file : Ximage.ps ...'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 colour = .false.
	 call ximagepostscriptdump
     *   (idevpost,colour,0,nxtrans-2,0,nxbox-2,xsize,ipixmap,ierr)
	 if(ierr .ne. 0) then
	  iolabel(nlabels+1) = 
     *    'Consistency check failure - postscript dump aborted'
	  iolabel(nlabels+2) = 'Type <cr> to return main menu'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+2)
	  nlabels = nlabels - 1
	 end if
	 go to 300
C*** hide transform
	else if(job .eq. 6) then
	 call ximageoverlayhide
	 go to 500
C*** return main menu
	else if(job .eq. 7) then
	 call ximageoverlayhide
	 call ximageremovevectors
	 call ximagemenuhide
	 call ximagelabelhide
	 return
	end if
	return
C*** compute and display a transform
 1000   call ximageoverlayhide
C*** read box from map
	nxtrans = nxbox + 2
	ixstart = ixim - nxbox / 2
	iystart = iyim - nybox / 2
	ixend = ixim + nxbox / 2
	iyend = iyim + nybox / 2
	if(ixstart .lt. 1 .or. iystart .lt. 0
     *   .or. ixend .gt. nxyz(1) .or. iyend .ge. nxyz(2)) then
	 iolabel(nlabels+1) = 'Box outside map limits'
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 go to 500
	end if
	call imposn(idevmap,0,iystart)
	do iy=1,nybox
	 ny = (iy - 1) * nxtrans
	 call irdlin(idevmap,aline)
	 do ix=1,nxtrans
	  ixy = ny + ix
	  den = aline(ix+ixstart-1)
	  transform(ixy) = den
	  if(ix .le. nxbox) then
	   mapout(ny + ix) = den
	  end if
	 end do
	end do
C*** calculate transform
	call TDXFFT(transform,nxbox,nybox,0)
C*** calculate min,max,mean
	if(ifirst) then
         call iclcdn(transform,nxtrans/2,nybox,1,nxtrans/2,1,nybox,
     *              transmin,transmax,transmean)
	 ifirst = .false.
	end if
	call fftcalc
C*** send transform map to display
	call ximageoverlayinit
     *  (ixscreen,iyscreen,nxtrans-1,nybox-1,transbuf,ierr)
	if(ierr .ne. 0) then
	 iolabel(nlabels+1) = 
     *   'Error in display, press any key to return main menu'
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 return
	end if	  
	go to 500
	end
C****************************************************************
C***
	subroutine fftcalc
C***
C****************************************************************
	include 'hXM_common.for'
C*** extract transform and write to byte map
	nxhalf = nxtrans / 2
	nyhalf = nybox / 2
C*** display amplitudes
	if(amp) then
C*** calculate scale factors
	 if(autoscale) then
	  tmin = greymin
	  scl = grey / greyrange
	 else
	  tmin = transmin
	  scl = grey / (transmax - transmin)
	 end if
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
	   den = scl*(sqrt(aval*aval+bval*bval)-tmin)
	   if(den .lt. 1.) den = 1.
	   if(den .gt. grey) den = grey
	   nden = nint(den)
	   nxy1 = ny1 - nxhalf + ix - 1
	   nxy2 = ny2 - nxhalf - ix + 1
	   transbuf(nxy1) = nden
	   transbuf(nxy2) = nden
	  end do
	 end do
C*** bottom right, top left from bottm of transform
	 do iy=1,nyhalf-1
	  ixy = nxtrans * (nyhalf + iy)
	  ny1 = (nxtrans - 1) * (nybox - iy)
	  ny2 = (nxtrans - 1) * iy
	  do ix=1,nxhalf
	   ixyin = ixy + ix * 2
           aval = transform(ixyin - 1)
           bval = transform(ixyin)
	   den = scl*(sqrt(aval*aval+bval*bval)-tmin)
	   if(den .lt. 1.) den = 1.
	   if(den .gt. grey) den = grey
	   nden = nint(den)
	   nxy1 = ny1 - nxhalf + ix - 1
	   nxy2 = ny2 - nxhalf - ix + 1
	   transbuf(nxy1) = nden
	   transbuf(nxy2) = nden
	  end do
	 end do
C*** display intensities
	else
C*** calculate scale factors
	 if(autoscale) then
 	  tminsq = 0.
	  sclsq = grey / ((transmax - transmean) * 0.5)
	 else
	  tminsq = transmin * transmin
	  sclsq = grey / (transmax * transmax - tminsq)
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
	   den = sclsq*((aval*aval+bval*bval)-tminsq)
	   if(den .lt. 1.) den = 1.
	   if(den .gt. grey) den = grey
	   nden = nint(den)
	   nxy1 = ny1 - nxhalf + ix - 1
	   nxy2 = ny2 - nxhalf - ix + 1
	   transbuf(nxy1) = nden
	   transbuf(nxy2) = nden
	  end do
	 end do
C*** bottom right, top left from bottm of transform
	 do iy=1,nyhalf-1
	  ixy = nxtrans * (nyhalf + iy)
	  ny1 = (nxtrans - 1) * (nybox - iy)
	  ny2 = (nxtrans - 1) * iy
	  do ix=1,nxhalf
	   ixyin = ixy + ix * 2
           aval = transform(ixyin - 1)
           bval = transform(ixyin)
	   den = sclsq*((aval*aval+bval*bval)-tminsq)
	   if(den .lt. 1.) den = 1.
	   if(den .gt. grey) den = grey
	   nden = nint(den)
	   nxy1 = ny1 - nxhalf + ix - 1
	   nxy2 = ny2 - nxhalf - ix + 1
	   transbuf(nxy1) = nden
	   transbuf(nxy2) = nden
	  end do
	 end do
	end if
	return
	end
C****************************************************************
C***
	subroutine rotate(rotang,nx,ny,mapin,mapout)

C***
C****************************************************************
C*** subroutine to rotate the map
C****************************************************************
C*** assumes centre of gravity in box centre, rotates about the centre
C*** initialize output array
	real*4	mapin(*)
	real*4	mapout(*)
	do i=1,nx * ny
	 mapout(i) = 0
	end do
        sinang = sin(rotang)
        cosang = cos(rotang)
	xcen = float(nx) * 0.5
	ycen = float(ny) * 0.5
C*** subtract half box width and height to make origin at box centre
C*** start y loop
        do iy = 1,ny
         ydist = float(iy-1) - ycen
         ysinang = ydist * sinang
         ycosang = ydist * cosang
C*** start x loop
         do 100 ix = 1,nx
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
          mapout(nx * (iy-1) + ix) = ybar * (xbar * den1 + xbit * den2) 
     *                  + ybit * (xbar * den3 + xbit * den4)
  100    continue
        end do
	return
	end
C****************************************************************
C***
	subroutine hide_widgets
C***
C****************************************************************
C*** subroutines to hide menu/labels/slider
C****************************************************************
	include 'hXM_common.for'
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
        include    'hXM_common.for'
	call ximagemenuhide
	call ximagelabelhide
	ifirst = .true.
  600	nlabels = 1
        if(idf_mapmode.eq.2)then
          if(idf_picmode.le.4)then
            if(idf_picmode.eq.3 .or. idf_picmode.eq.4)then
              xorigin = int(nxyz(1)/2)
              yorigin = int(nxyz(2)/2)
            else
              xorigin = df_orix
              yorigin = df_oriy
            endif
	    write(return_string,'(F12.2,'','',F12.2)')
     1        xorigin,yorigin
	    return_string = rmblank(return_string,nchars)
	    write(iolabel(nlabels+1),
     1        '(''Type in origin x0, y0, default ('',A,'')'')')
     2        return_string(1:nchars)
          else
	    iolabel(nlabels+1) = 'Type in origin x0, y0, default (0,0)'
          endif
	  return_string = ' '
	else
	  iolabel(nlabels+1) = 'Type in origin x0, y0, default (0,0)'
	  return_string = ' '
	endif
	call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	if(return_string .eq. ' ') then
          if(idf_mapmode.eq.2)then
            if(idf_picmode.le.4)then
	      x0 = xorigin
	      y0 = yorigin
            endif
          else
	    x0 = 0.0
	    y0 = 0.0
          endif
	else
	 call extract_reals(2,return_string,x0,y0,f3,f4,f5,f6)
	end if
	xorigin = x0
	yorigin = y0
        df_orix = xorigin
        df_oriy = yorigin
        call convert_to_screen(int(xorigin),int(yorigin),ix1,iy1)
        write(*,'(/,'' Drawing origin at '',2F12.3,2I10)')xorigin,yorigin,ix1,iy1
        if(idf_mapmode.eq.2)then
	  call ximagedrawcircle(ix1,iy1, 5)  
	  call ximagedrawcircle(ix1,iy1,15)  
        else
	  call ximagedrawcircle(ix1,iy1,50)  
        endif
C***********************************************************
C*** menu to control lattice input
C***********************************************************
  700	call ximagelabeldisplay(iolabel,nlabels)
	menulist(1) = 'Input indices/spot positions manually'
	menulist(2) = 'Input file of indices/spot positions'
	menulist(3) = 'Input lattice vectors manually'
	menulist(4) = 'Read lattice vectors from Datafile'
	menulist(5) = 'Read second set of vectors from Datafile'
	menulist(6) = 'Return main menu'
	call ximagemenuinit(menulist,6)
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
	else if(job .eq. 4) then
C****	*********************************************	 

	 call ximagemenuhide

         df_recell = '1.0,0.0,0.0,0.1'
         df_badrecell = '1.0,0.0,0.0,0.1'
	 open(unit=idevdat,file='DATAFILE.dat',err=881,
     1         status='old')
	 do i=1,31
    	   read(idevdat,'(a)') df_recell
         enddo
	 read(idevdat,'(a)') df_recell
	 do i=33,34
    	   read(idevdat,'(a)') df_badrecell
         enddo
	 read(idevdat,'(a)') df_badrecell
	 do i=36,57
    	   read(idevdat,'(a)') czeile
         enddo
	 read(idevdat,*) df_orix,df_oriy
	 close(idevdat)
 881     continue

	 read(df_recell,*) x1,y1,x2,y2
         if(idf_mapmode.eq.2.and.idf_picmode.le.2)then
           irang = 25
         else
           irang = 12
         endif
     	 write(iolabel(nlabels+1),'('' How many orders (default'',I3,'') ?'')')irang
	 return_string = ' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
C*** get number of orders
	 if(return_string(1:1) .ne. ' ') then
  	   call extract_integers(1,return_string,
     *     irang,i2,i3,i4,i5,i6)
         endif
C*** reset number of orders if too many
         if(irang.gt.max_lat) then
          write(iolabel(nlabels+1),'(
     *   '' Number of orders reset to'',i4)') max_lat
	  call ximagelabeldisplay(iolabel,nlabels+1)
          irang = max_lat
         end if
	 go to 2000
        else if(job .eq. 5) then
C****   *********************************************
         call ximagemenuhide

         df_recell = '1.0,0.0,0.0,0.1'
         df_badrecell = '1.0,0.0,0.0,0.1'
	 open(unit=idevdat,file='DATAFILE.dat',err=882,
     1         status='old')
	 do i=1,31
    	   read(idevdat,'(a)') df_recell
         enddo
	 read(idevdat,'(a)') df_recell
	 read(idevdat,'(a)') df_badrecell
	 read(idevdat,'(a)') df_badrecell
 	 read(idevdat,'(a)') df_badrecell
	 do i=36,57
    	   read(idevdat,'(a)') czeile
         enddo
	 read(idevdat,*) df_orix,df_oriy
	 close(idevdat)
 882     continue

         read(df_badrecell,*) x1,y1,x2,y2
         if(idf_mapmode.eq.2)then
           irang = 25
         else
           irang = 12
         endif
         write(iolabel(nlabels+1),'('' How many orders (default'',I3,'') ?'')')irang
         return_string = ' '
         call ximageioboxdisplay(iolabel,return_string,nlabels+1)
C*** get number of orders
         if(return_string(1:1) .ne. ' ') then
           call extract_integers(1,return_string,
     *     irang,i2,i3,i4,i5,i6)
         endif
C*** reset number of orders if too many
         if(irang.gt.max_lat) then
          write(iolabel(nlabels+1),'(
     *   '' Number of orders reset to'',i4)') max_lat
          call ximagelabeldisplay(iolabel,nlabels+1)
          irang = max_lat
         end if
         go to 2000
C****	*********************************************	 
C*** return main menu
	else if(job .eq. 6) then
	 call ximagemenuhide
	 return
	end if
  900   nspots = 0
 1000   call lattice_read
	if(ierr.ne.0) return
C***
C*** read number of orders to refine
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
C***
C*** refine the lattice
        call lattice_refine
C***********************************************************
C*** return if refinement failed
C***********************************************************
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
C***
C*** refinement successful
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
 1800	latfile = ' '
	call ximageioboxdisplay(iolabel,latfile,nlabels+9)
	if(latfile .ne. ' ') then
	 old = .false.
	 call check_file(latfile)
         open(unit=idevlat,file=latfile,status='new',err=1800)
         write(idevlat,'(1x,a)') mapfile
	 do n=nlabels+1,nlabels+8
	  write(idevlat,'(a)') iolabel(n)
	 end do
	 close(idevlat)
	end if
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
	menulist(5) = 'Edit spot list'
	menulist(6) = 'Save to 1st (good) lattice in DATAFILE.dat'
	menulist(7) = 'Save to 2nd (bad ) lattice in DATAFILE.dat'
	menulist(8) = 'Return main menu'
	call ximagemenuinit(menulist,8)
	job = -1
 2500   call ximagewait(job)
	if(job.le.0) then
	 go to 2500
C*** Standard boxes
	else if(job .lt. 3) then
	 call ximagelabelhide
	 call ximagemenuhide
 2600	 continue
         if(idf_mapmode.eq.2)then
           iolabel(nlabels+1) = 'Type in symbol size in pixels (default 20)'
	   return_string = ' '
         else
           iolabel(nlabels+1) = 'Type in symbol size in pixels (default 25)'
	   return_string = ' '
         endif
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	 if(return_string(1:1) .ne. ' ') then
	   call extract_integers(1,return_string,
     *     ibox1,i2,i3,i4,i5,i6)
         else
           if(idf_mapmode.eq.2)then
	     ibox1 = 20
           else
	     ibox1 = 25
           endif
         endif
	 isymb = job
	 if(isymb .eq. 2) irad = nint(float(ibox1) * 0.5)
C*** variable boxes
	else if(job .eq. 3) then	
	  call ximagelabelhide
	  call ximagemenuhide
 2700	  iolabel(nlabels+1) = 'Type in box sizes in pixels'
	  return_string = ' '
	  call ximageioboxdisplay(iolabel,return_string,nlabels+1)
	  call extract_integers(2,return_string,
     *    ibox1,ibox2,i3,i4,i5,i6)
	  if(ibox1 .le. 0 .or. ibox2 .le. 0) go to 2700
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
CHEN****	 call ximageremovevectors
	  call ximagelabelhide
	  call ximagemenuhide	
	  go to 1000
C*** Define spot list
	else if(job .eq. 5) then
	  call ximagemenuhide
	  call ximagelabelhide
C
          call getspo
C
	  call ximagelabelhide
	  call ximagemenuhide	
	  call ximageremovevectors
	  return
C*** save to good lattice in DATAFILE.dat 
        else if(job.eq.6) then
C
	  call savlat(1)
C
          goto 2000
C*** save to bad  lattice in DATAFILE.dat 
        else if(job.eq.7) then
C
          call savlat(2)
C
          goto 2000
C*** return main menu
	else if(job .eq. 8) then
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
 2800       iolabel(nlabels+1) = 
     *      ' Type angle(degrees) tilt axis makes with horizontal'
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
 3000       call ximagewait(job)
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
C*** auto  centre
	    else if(job.eq.2) then
             ixcen = nint(x0)
             iycen = nint(y0)
	     call ximagemenuhide
C*** return main menu
	    else if(job.eq.3) then
	     call ximagemenuhide
CHEN****	   call ximageremovevectors
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
     *           abs(cosphi*(yn_lat(i,j)-fiycen) - 
     *               sinphi * (xn_lat(i,j)-fixcen))
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
C-------Prepare empty spotlist field
        do I=0,maxsptlst
          do J=-maxsptlst,maxsptlst
            ispotlst(I,J)=0
          enddo
        enddo
C-------Read existing spotlist 
        inquire(file=spotfile,exist=there)
        if(there)then
          open(unit=idevspt,file=spotfile,status='old',err=3405)
 3404     continue
            read(idevspt,*,ERR=3405,END=3405)isptx,ispty,ierp
            if(isptx.lt.0)then
              isptx=-isptx
              ispty=-ispty
            endif
            if(isptx.le.maxsptlst .and. ispty.le.maxsptlst)then
              ispotlst(isptx,ispty)=ierp
            endif
            goto 3404
 3405     continue
          close(idevspt)
        endif
C
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
             ilocrad=irad
             if(idf_mapmode.eq.2.and.idf_picmode.le.2)then
	       iloc=i
	       jloc=j
               if(iloc.lt.0)then
                 iloc=-iloc
                 jloc=-jloc
               endif
               if(ispotlst(iloc,jloc).ne.0)then
                 ilocrad=irad/ispotlst(iloc,jloc)
               else
                 ilocrad=1
               endif
             endif
	     call ximagedrawcircle(ixscreen,iyscreen,ilocrad)
	    end if
3200    continue
C***********************************************************
C*** lattice vectors read in for viewing only
C***********************************************************
	if(nspots .eq. 0) then
 3397    continue
	 menulist(1) = 'Edit spot list'
	 menulist(2) = 'Refine another lattice'
	 menulist(3) = 'Save to 1st (good) lattice in DATAFILE.dat'
	 menulist(4) = 'Save to 2nd (bad ) lattice in DATAFILE.dat'
	 menulist(5) = 'Return main menu'
         call ximagemenuinit(menulist,5)
	 job = -1
 3400    call ximagewait(job)
	 if(job.le.0) then
	  go to 3400
C*** define spot list
 	 else if(job .eq. 1) then
	  call ximagemenuhide
	  call ximagelabelhide
C
          call getspo
C
	  call ximagemenuhide
	  call ximageremovevectors
	  return
C*** refine another lattice
 	 else if(job .eq. 2) then
	  call ximagemenuhide
	  call ximagelabelhide
	  go to 600
C*** save to good lattice in DATAFILE.dat 
        else if(job.eq.3) then
C
	  call savlat(1)
C
          goto 3397
C*** save to bad  lattice in DATAFILE.dat 
        else if(job.eq.4) then
C
          call savlat(2)
C
          goto 3397
C*** return main menu
	 else if(job .eq. 5) then
	  call ximagemenuhide
	  call ximageremovevectors
	  return
	 end if
	end if
C***********************************************************
C*** Lattice displayed, how to proceed ?
C***********************************************************
 3998   continue
	call ximagelabeldisplay(iolabel,nlabels)
	menulist(1) = 'Save lattice vectors to file'
	menulist(2) = 'Change symbol sizes'
	menulist(3) = 'Add/remove spots and re-refine'
	menulist(4) = 'Refine another lattice'
	menulist(5) = 'Edit spot list'
	menulist(6) = 'Save to 1st (good) lattice in DATAFILE.dat'
	menulist(7) = 'Save to 2nd (bad ) lattice in DATAFILE.dat'
	menulist(8) = 'Return main menu'
        call ximagemenuinit(menulist,8)
	job = -1
 4000   call ximagewait(job)
	if(job.le.0) then
	 go to 4000
C*** keep symbol sizes
        else if(job.eq.1) then
	 call ximagemenuhide
	 go to 5000
C*** change symbol sizes
        else if(job.eq.2) then
	 call ximagemenuhide
	 call ximageremovevectors
	 go to 2000
C*** modify spots and re-refine
	else if(job.eq.3) then
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
C 3.06.98            call remove_box(kxmin,kymin,kxmax,kymax)
CHEN****    call ximageremovebox(kxmin,kymin,kxmax,kymax)
C*** draw a circle
	   else if(isymb .eq. 2) then
CHEN****	    call ximageremovecircle(ixscreen,iyscreen,irad)
	   end if
4200     continue
	 call ximagelabelhide
	 call ximagemenuhide	
	 go to 1000
C*** refine another lattice
	else if(job.eq.4) then
	 call ximagemenuhide
	 call ximagelabelhide
	 go to 600
C*** get spotlist
        else if(job.eq.5) then
	 call ximagemenuhide
	 call ximagelabelhide
C
         call getspo
C
	 call ximagelabelhide
	 call ximagemenuhide	
	 call ximageremovevectors
	 return
C*** save to good lattice in DATAFILE.dat 
        else if(job.eq.6) then
C
	  call savlat(1)
C
          goto 3998
C*** save to bad  lattice in DATAFILE.dat 
        else if(job.eq.7) then
C
          call savlat(2)
C
          goto 3998
C*** return main menu
        else if(job.eq.8) then
	 call ximagemenuhide
	 call ximageremovevectors
	 return
	end if
C***
C*** send lattice vectors to output file
 5000   iolabel(nlabels+1) = 'Type output filename...'
	latfile = ' '
	call ximageioboxdisplay(iolabel,latfile,nlabels+1)
	old = .false.
	call check_file(latfile)
        open(unit=idevlat,file=latfile,status='new',err=5000)
        write(idevlat,'(1x,a)') mapfile
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
     *          2i10,  '' Symbol sizes in pixels'')')
     *  numx,numy,x1,y1,x2,y2,cenx,ceny,ecenx,eceny,ibox1,ibox2
C***
C*** write coords/indices used for refinement
	if(nspots .ge. 1) then
         write(idevlat,'('' Spot number  H    K      X       Y'')')
         do kk=1,nspots
          x = xd(kk) * scfac
          y = yd(kk) * scfac
          write(idevlat,'(1x,i10,2i5,2f8.1)') kk, ih(kk), ik(kk), x, y
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
	if(job.le.0) go to 7000
	if(job.eq.1) then
	 call ximagemenuhide
	 call ximagelabelhide
	 call ximageremovevectors
	 go to 900
	else if(job.eq.2) then
	 call ximagemenuhide
	 call ximagelabelhide
	 call ximageremovevectors
	 return
	end if
        end
C***************************************************************
C***
        subroutine ctffit
C***
C***************************************************************
C*** subroutine to fit a CTF to the image
C***
C***
        include    'hXM_common.for'
	call ximagemenuhide
	call ximagelabelhide
C
        ihidethon=0
        iallthon=0
C
        open(unit=idevdat,file='DATAFILE.dat',err=603,status='old')
        do i=1,160
          read(idevdat,'(a)') df_zeile(i)
        enddo
        close(idevdat)
C
C-------read(df_zeile( 14),*) isize
        isize = (nxyz(1)-1)*2
        read(df_zeile( 15),*) rmag
        read(df_zeile( 16),*) rstepz
	if(iredcen.eq.1)then
 	  rstepz=rstepz*2.0
	endif
	rstepz=rstepz*10000.0
C-------rstepz is in ANGSTROEM.
C
	read(df_zeile( 25),*)rdeftl
	read(df_zeile( 26),*)rdeftr
	read(df_zeile( 27),*)rdefbl
	read(df_zeile( 28),*)rdefbr
C
        read(df_zeile( 34),*) rdef1,rdef2,rdefan
	if(rdefan.lt.0.0)rdefan=rdefan+180.0
	if(rdefan.ge.180.0)rdefan=rdefan-180.0
        read(df_zeile( 93),*) rphacon
        read(df_zeile(97),*) rcs,rkv
	rcs=rcs*10000000.0
C-------rcs is in Anstroem now.
	rvolt=1000.0*rkv
C
C       write(*,'('' isize  = '',I6)')isize
C       write(*,'('' rmag   = '',G15.1)')rmag 
C       write(*,'('' rstepz = '',G15.3)')rstepz
C       write(*,'('' rdef1  = '',G15.1)')rdef1 
C       write(*,'('' rdef2  = '',G15.1)')rdef2
C       write(*,'('' rdefan = '',G15.2)')rdefan
C       write(*,'('' rphacon= '',G15.3)')rphacon
C       write(*,'('' rcs    = '',G15.3)')rcs
C       write(*,'('' rkv    = '',G15.2)')rkv
C
C	rL=1.602E-19
C	rE0=511000.0
C	rh=6.63E-34
C	rc=3.0E8
C
C-------WL=rh*rc/sqrt((2*rvolt*rE0+rvolt*rvolt)*rL*rL)
C-------WL is in meters now.
C-------WL=WL*10000000000.0
C-------WL is in ANGSTROEM now.
C
        WL=12.3/SQRT(rvolt+rvolt**2/(10.0**6.0))
C
        STEPR=ifftscl*rstepz*isize/rmag
        THETATR=WL/STEPR
C-------THETATR is the angle of the first FFT pixel.
C
C       write(*,'('' WL     = '',G15.5)')WL
C       write(*,'('' STEPR  = '',G15.5)')STEPR
C       write(*,'('' THETATR= '',G15.5)')THETATR
C
	if(idefoci.ne.0)then
	  rast=(rdef1-rdef2)/2.0
	  if(idefoci.eq.1)then
	    rdef1=rdeftl
	  else if(idefoci.eq.2)then 
	    rdef1=rdeftr
	  else if(idefoci.eq.3)then 
	    rdef1=rdefbl
	  else if(idefoci.eq.4)then 
	    rdef1=rdefbr
	  endif
	  rdef2=rdef1
	  rdef1=rdef1+rast
	  rdef2=rdef2-rast
	endif
C
 595	continue
C
C-------CHI=(PI*ANGLE*ANGLE/WL)*(RCS*ANGLE*ANGLE/2-rdef1)
        DO 596 IX=1,ctfdim
          ANGLE=IX*THETATR
          C1=M_PI*ANGLE*ANGLE/WL
          C2=-C1*rcs*ANGLE*ANGLE/2.0
          CHI1=C1*rdef1+C2
          CHI2=C1*rdef2+C2
          rctf1(IX)=-SIN(CHI1)*rphacon-COS(CHI1)*(1.0-rphacon)
          rctf2(IX)=-SIN(CHI2)*rphacon-COS(CHI2)*(1.0-rphacon)
C	  write(*,'('' IX,ANGLE,C1,C2,CHI,rctf ='',I6,'': '',5G15.3)')
C    1       IX,ANGLE,C1,C2,CHI1,rctf1(IX)
C
 596    continue
C
        IY1=1
        IY2=1
        rctf1(1)=1.0
        rctf2(1)=1.0
        DO 597 IX=2,ctfdim
          if((rctf1(IX-1).ge.0.0 .and. rctf1(IX).lt.0.0) .or.
     1       (rctf1(IX-1).lt.0.0 .and. rctf1(IX).ge.0.0))then
            ithon1(IY1)=IX
            IY1=IY1+1
          endif
          if((rctf2(IX-1).ge.0.0 .and. rctf2(IX).lt.0.0) .or.
     1       (rctf2(IX-1).lt.0.0 .and. rctf2(IX).ge.0.0))then
            ithon2(IY2)=IX
            IY2=IY2+1
          endif
          if(IY1.ge.maxthon .or. IY2.ge.maxthon) goto 598
 597    continue
 598    continue
C
        if(IY1.gt.IY2)IY1=IY2
        IY1=IY1-1
C
	ixim = 0
	iyim = 0
        call convert_to_screen(ixim,iyim,ilxscree2,ilyscree2)
	call ximageremovevectors
        if(ihidethon.eq.0)then
          iloc=IY1
          if(iallthon.eq.1 .and. iloc.gt.4)iloc=4
          do i=1,iloc
	    if(ithon1(i).lt.isize/2 .and. ithon2(i).lt.isize/2)then
C             write(*,'(I6''. Thonring at '',2I8)')i,ithon1(i),ithon2(i)
	      call ximagedrawelipse(ilxscree2,ilyscree2,ithon1(i),ithon2(i),rdefan)  
 	    endif
          enddo
        endif
C
	ifirst = .true.
  600	continue
	if(idefoci.eq.0)then
     	  nlabels = 1
	  write(iolabel(nlabels),'(''Current defocus = '',F9.2,F9.2,F9.3)')
     1      rdef1,rdef2,rdefan
	  call ximagelabeldisplay(iolabel,nlabels)
          if(ihidethon.eq.0)then
	     menulist(1) = 'Thon-Rings OFF'
          else
	     menulist(1) = 'Thon-Rings ON'
	  endif
          if(iallthon.eq.0)then
	     menulist(2) = 'Show only some Thon-Rings'
          else
	     menulist(2) = 'Show all Thon-Rings'
          endif
	  menulist(3) = 'Increase defocus slow'
	  menulist(4) = 'Decrease defocus slow'
	  menulist(5) = 'Increase defocus fast'
	  menulist(6) = 'Decrease defocus fast'
	  menulist(7) = 'Increase defocus in X-dir'
	  menulist(8) = 'Increase defocus in Y-dir'
	  menulist(9) = 'Reset astigmatism'
	  menulist(10) = 'Increase angle slow'
	  menulist(11) = 'Decrease angle slow'
	  menulist(12) = 'Increase angle fast'
	  menulist(13) = 'Decrease angle fast'
	  menulist(14) = 'Save to DATAFILE.dat and return'
	  menulist(15) = 'Return without saving'
	  call ximagemenuinit(menulist,15)
	else
	  nlabels=2
	  if(idefoci.eq.1)then
		rdeftl=(rdef1+rdef2)/2.0
		write(iolabel(1),'(''Fitting image Top Left'')')
	  endif
	  if(idefoci.eq.2)then
		rdeftr=(rdef1+rdef2)/2.0
		write(iolabel(1),'(''Fitting image Top Right'')')
	  endif
	  if(idefoci.eq.3)then
		rdefbl=(rdef1+rdef2)/2.0
		write(iolabel(1),'(''Fitting image Bottom Left'')')
	  endif
	  if(idefoci.eq.4)then
		rdefbr=(rdef1+rdef2)/2.0
		write(iolabel(1),'(''Fitting image Bottom Right'')')
	  endif
	  write(iolabel(2),'(''Current defocus = '',F9.2,F9.2,F9.3)')
     1      rdef1,rdef2,rdefan
	  call ximagelabeldisplay(iolabel,nlabels)
          if(ihidethon.eq.0)then
	     menulist(1) = 'Thon-Rings OFF'
          else
	     menulist(1) = 'Thon-Rings ON'
	  endif
          if(iallthon.eq.0)then
	     menulist(2) = 'Show only some Thon-Rings'
          else
	     menulist(2) = 'Show all Thon-Rings'
          endif
	  write(menulist(3),'(''Defocus Top      Left   = '',F9.0)')rdeftl
	  write(menulist(4),'(''Defocus Top      Right = '',F9.0)')rdeftr
	  write(menulist(5),'(''Defocus Bottom Left   = '',F9.0)')rdefbl
	  write(menulist(6),'(''Defocus Bottom Right = '',F9.0)')rdefbr
	  write(menulist(idefoci+1)(38-idefoci:50),'(''<====='')')
	  menulist(7) = '  ******  Increase defocus slow ******'
	  menulist(8) = '  ****** Decrease defocus slow ******'
	  menulist(9) = '  ******  Increase defocus fast ******'
	  menulist(10) = '  ****** Decrease defocus fast ******'
	  menulist(11) = 'Save to DATAFILE.dat and return'
	  menulist(12) = 'Return without saving'
	  call ximagemenuinit(menulist,12)
	endif
	job = -1
 601	call ximagewait(job)
	if(job.le.0)then
	  goto 601
	endif
	if(idefoci.eq.0)then
          if(job.eq.1)then
            ihidethon=ihidethon+1
            if(ihidethon.gt.1)ihidethon=0
	  else if(job.eq.2)then
            iallthon=iallthon+1
            if(iallthon.gt.1)iallthon=0
	  else if(job.eq.3)then
	    if(rdef1.ge.0)then
	      rdef1 = rdef1 * 1.02 + 20.0
	    else
	      rdef1 = rdef1 / 1.02 + 20.0
	    endif
	    if(rdef2.ge.0)then
	      rdef2 = rdef2 * 1.02 + 20.0
	    else
	      rdef2 = rdef2 / 1.02 + 20.0
	    endif
	  else if(job.eq.4)then
	    if(rdef1.ge.0)then
	      rdef1 = rdef1 * 0.95 - 20.0
	    else
	      rdef1 = rdef1 / 0.95 - 20.0
	    endif
	    if(rdef2.ge.0)then
	      rdef2 = rdef2 * 0.95 - 20.0
	    else
	      rdef2 = rdef2 / 0.95 - 20.0
	    endif
	  else if(job.eq.5)then
	    if(rdef1.ge.0)then
	      rdef1 = rdef1 * 1.20 + 300.0
	    else
	      rdef1 = rdef1 / 1.20 + 300.0
	    endif
	    if(rdef2.ge.0)then
	      rdef2 = rdef2 * 1.20 + 300.0
	    else
	      rdef2 = rdef2 / 1.20 + 300.0
	    endif
	  else if(job.eq.6)then
	    if(rdef1.ge.0)then
	      rdef1 = rdef1 * 0.68 - 300.0
	    else
	      rdef1 = rdef1 / 0.68 - 300.0
	    endif
	    if(rdef2.ge.0)then
	      rdef2 = rdef2 * 0.68 - 300.0
	    else
	      rdef2 = rdef2 / 0.68 - 300.0
	    endif
	  else if(job.eq.7)then
	    rdef1 = rdef1 * 1.01 + 10.0
	    rdef2 = rdef2 * 0.99 - 10.0
	  else if(job.eq.8)then
	    rdef1 = rdef1 * 0.97 - 20.0
	    rdef2 = rdef2 * 1.03 + 20.0
	  else if(job.eq.9)then
	    rdef1 = ( rdef1 + rdef2 ) / 2.0
	    rdef2 = rdef1
            rdefan = 0.0
	  else if(job.eq.10)then
	    rdefan = rdefan + 2.0
	    if(rdefan.lt.0.0)rdefan=rdefan+180.0
	    if(rdefan.ge.180.0)rdefan=rdefan-180.0
	  else if(job.eq.11)then
	    rdefan = rdefan - 2.8
	    if(rdefan.lt.0.0)rdefan=rdefan+180.0
	    if(rdefan.ge.180.0)rdefan=rdefan-180.0
	  else if(job.eq.12)then
	    rdefan = rdefan + 10.0
	    if(rdefan.lt.0.0)rdefan=rdefan+180.0
	    if(rdefan.ge.180.0)rdefan=rdefan-180.0
	  else if(job.eq.13)then
	    rdefan = rdefan - 10.0
	    if(rdefan.lt.0.0)rdefan=rdefan+180.0
	    if(rdefan.ge.180.0)rdefan=rdefan-180.0
	  else if(job.eq.14)then
	    goto 602
	  else if(job.eq.15)then
	    goto 603
	  endif
	else
	  inewdef=0
          if(job.eq.1)then
            ihidethon=ihidethon+1
            if(ihidethon.gt.1)ihidethon=0
          else if(job.eq.2)then
            iallthon=iallthon+1
            if(iallthon.gt.1)iallthon=0
          else if(job.eq.3)then
	    inewdef=1
            goto 602
          else if(job.eq.4)then
	    inewdef=2
            goto 602
          else if(job.eq.5)then
	    inewdef=3
            goto 602
          else if(job.eq.6)then
	    inewdef=4
            goto 602
          else if(job.eq.7)then
	    rdef1 = rdef1 * 1.013 + 10.0
	    rdef2 = rdef2 * 1.013 + 10.0
          else if(job.eq.8)then
	    rdef1 = rdef1 * 0.970 - 10.0
	    rdef2 = rdef2 * 0.970 - 10.0
          else if(job.eq.9)then
	    rdef1 = rdef1 * 1.10 + 100.0
	    rdef2 = rdef2 * 1.10 + 100.0
          else if(job.eq.10)then
	    rdef1 = rdef1 * 0.90 - 100.0
	    rdef2 = rdef2 * 0.90 - 100.0
          else if(job.eq.11)then
            idefoci = 0 
            goto 602
          else if(job.eq.12)then
            idefoci = 0 
            goto 603
          endif
        endif
	goto 595
C
 602    continue
C
	if(idefoci.eq.0)then
    	  write(df_zeile(1),5991)rdef1,rdef2,rdefan
 5991     FORMAT(F9.2,F9.2,F9.3)
	  call inkomma(df_zeile(1),k)
 	  write(df_zeile(2),5992)df_zeile(1)(1:k)
 5992     FORMAT('tredat 4 DATAFILE.dat 34 "',A,'"')
	  call shorten(df_zeile(2),k)
	  write(*,'('' Running: '',A)')df_zeile(2)(1:k)
	  call system(df_zeile(2)(1:k))
C
          write(df_zeile(2),'(''tredat 5 DATAFILE.dat 34 43 M'')')
	  call shorten(df_zeile(2),k)
	  write(*,'('' Running: '',A)')df_zeile(2)(1:k)
	  call system(df_zeile(2)(1:k))
C
	else
C
	  rtmp1=(rdef1+rdef2)/2.0
	  itmp1=24+idefoci
    	  write(df_zeile(2),5993)itmp1,rtmp1
 5993	  FORMAT('tredat 4 DATAFILE.dat ',I3,' ',F9.0)
	  call shorten(df_zeile(2),k)
	  write(*,'('' Running: '',A)')df_zeile(2)(1:k)
	  call system(df_zeile(2)(1:k))
C
          write(df_zeile(2),'(''tredat 5 DATAFILE.dat '',I3,'' 43 M'')')itmp1
	  call shorten(df_zeile(2),k)
	  write(*,'('' Running: '',A)')df_zeile(2)(1:k)
	  call system(df_zeile(2)(1:k))
C
	endif
C
 603    continue
C
	call ximagemenuhide
	call ximagelabelhide
	call ximageoverlayhide
	call ximageremovevectors
C
	if(idefoci.gt.0)then
          call ximageclearimage
          newmap = .true.
	endif
C
	return
	end
C
C***************************************************************
C***                                                         ***
        subroutine show_peaks
C***                                                         ***
C***************************************************************
C*** subroutine to display peaks from an external peak list
C***
        include 'hXM_common.for'
C
 3401   continue
	  call ximagemenuhide
  600     iolabel(nlabels+1) = 'Type peaklist filename'
	  peakfile = ' '
	  call ximageioboxdisplay(iolabel,peakfile,nlabels+1)
	  if(peakfile .ne. ' ') then
	    old = .false.
 	    there = .false.
            inquire(file=peakfile,exist=there)
C-----------Prepare empty peaklist field
            do I=1,maxpeaklst
              ipeaklst(I,1)=0
              ipeaklst(I,2)=0
            enddo
            inpeak=0
	    if(there) then
C-------------Read existing peaklist 
              open(unit=idevpeak,file=peakfile,status='old',err=3401)
 	      I=1
 3404         continue
                read(idevpeak,*,ERR=3405,END=3406)ipeaklst(I,1),ipeaklst(I,2)
                if(ipeak.le.maxpeaklst)then
                  ixim=ipeaklst(I,1)+x0
                  iyim=ipeaklst(I,2)+y0
                  call convert_to_screen(ixim,iyim,ilxscree1,ilyscree1)
                  call draw_cross(ilxscree1,ilyscree1,ispotcross_size)
                  I=I+1
	        else
		  write(6,'(''ERROR: too many peaks. Increase maxpeaklst.'')')
		  goto 3406
                endif
              goto 3404
C
 3405         continue
              write(6,'(''ERROR while reading peaklist'')')
 3406         continue
	      close(idevpeak)
              write(6,'(I6,'' peaks read from spotlist file.'')')I
            endif
          endif
C
        return
        end
C
C***************************************************************
C***                                                         ***
        subroutine lattice_display
C***                                                         ***
C***************************************************************
C*** subroutine to display spots selected for lattice refinement
C***
	include 'hXM_common.for'
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
	include 'hXM_common.for'
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
	menulist(6) = 'Return main menu'
	call ximagemenuinit(menulist,6)
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
C
C        write(*,'(/,'' read nspots       = '',I9)')nspots
C        write(*,'(/,'' ixscreen,iyscreen = '',2I9)')ixscreen,iyscreen
C        write(*,'(  '' ixim,iyim         = '',2I9)')ixim,iyim
C        write(*,'(  '' icenx,iceny       = '',2I9)')icenx,iceny
C        write(*,'(  '' ixmin,iymin       = '',2I9)')ixmin,iymin
C        write(*,'(  '' nxstart,nystart   = '',2I9)')nxstart,nystart
C
         ixlocmin=0
         iylocmin=0
	 x0 = df_orix
	 y0 = df_oriy
C*** calculate the coordinates of this spot
         if(x1.ne.0 .or. x2.ne.0 .or. y1.ne.0 .or. y2.ne.0)then
           rdim2=999999999999.0
           do i=-50,50
             do j=-50,50
               tx=x1*i+x2*j+x0
               ty=y1*i+y2*j+y0
               rdi2=abs(tx-xd(nspots))+abs(ty-yd(nspots))
               if(rdi2.lt.rdim2) then
                 rdim2=rdi2
                 ixlocmin=i
                 iylocmin=j
               endif
             end do
           end do
         else
          ixlocmin=0
          iylocmin=0
         endif
  300    write(iolabel(nlabels+1),
     *   '(''Type h,k indices for this spot (def='',I4,'','',I4,'')'')')
     *   ixlocmin,iylocmin
  350    return_string=' '
	 call ximageioboxdisplay(iolabel,return_string,nlabels+1)
         call shorten(return_string,k)
         if(k.gt.2)then
	   call extract_integers(2,return_string,
     *     ih(nspots),ik(nspots),i3,i4,i5,i6)
         else
           ih(nspots)=ixlocmin
           ik(nspots)=iylocmin
         endif
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
	 if(nlabels .eq. max_lines_per_page) then
	  do n=4,max_lines_per_page
	   iolabel(n) = iolabel(n+1)
	  end do
	  nlabels = max_lines_per_page
	 end if
	 nlabels = nlabels + 1
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
	 old = .false.
	 call check_file(latfile)
	 open(unit=idevlat,file=latfile,status='new')
	 do i=1,nspots
	  write(idevlat,*)  ih(i), ik(i), xd(i), yd(i)
	 end do 
	 close(idevlat)
	 write(iolabel(nlabels+1),'(''Spot data written to '',a)')
     *   latfile(1:lnblank(latfile))
	 call ximagelabeldisplay(iolabel,nlabels+1)
	 go to 150
C***********************************************************
C*** finished spot selection, return to lattice
C***********************************************************
	else if(job.eq.5) then
	 call ximagelabelhide
	 call ximagemenuhide
	 return
C***********************************************************
C*** return main menu
C***********************************************************
	else if(job.eq.6) then
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
        include 'hXM_common.for'
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
        df_orix = x0
        df_oriy = y0
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
        include    'hXM_common.for'
C***
	nlabels = 1
	nspot = 0
	call ximagelabelhide
	call ximagemenuhide
	standard = .false.
	spider = .false.
	imagic = .false.
	if(noz .gt. 1) then
	 nlabels = nlabels + 1
	 iolabel(nlabels) = 'Warning - this option cannot find map values'
	end if
	iolabel(nlabels+1) = 'Select output format for new or old file'
	call ximagelabeldisplay(iolabel,nlabels+1)
	nlabels = 2
        menulist(1) = 'Screen only'
	menulist(2) = 'Standard format'
	menulist(3) = 'Spider format'
	menulist(4) = 'Imagic format'
	menulist(5) = 'Return main menu'
	call ximagemenuinit(menulist,5)
	job = -1
        if(iclick.eq.1)then
          job=2
          goto 201
        endif
  200   call ximagewait(job)
	if(job .le. 0) then
	 go to 200
        endif
  201   continue
C*** coords to screen
	if(job .eq. 1) then
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
C*** return main menu
	else if(job .eq. 5) then
	 call ximagemenuhide
	 return
	end if
	call ximagelabelhide
	call ximagemenuhide
C*** open input/output file
  210	iolabel(nlabels) = 'Coordinate file name ...'
        if(iclick.eq.1)then
	 coordfile = 'mrcclick.dat'
         iclick = 0
        else
	 coordfile = ' '
        endif
	call ximageioboxdisplay(iolabel,coordfile,nlabels)
C*** filename already present, read from it ?
        there = .false.
        inquire(file=coordfile,exist=there)
	if(there) then
	 iolabel(nlabels) = 'Reading from an existing file...'
	 nlabels = nlabels + 1
         iolabel(nlabels) = 
     *   'Mark cursor positions with left hand mouse button'
	 call ximagelabeldisplay(iolabel,nlabels)
	 open(unit=idevcoord,file=coordfile(1:lnblank(coordfile)),
     *        status='old',err=210)
C*** read first record of spider or standard
	 if(standard .or. spider) read(idevcoord,'(a)') return_string
C*** read coords now
C*** standard format
	 if(standard) then
  220     read(idevcoord,*,end=250) ixim, iyim, den
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
  230     read(idevcoord,'(i5,i2,2f12.6)',end=250) nspot, i, xim,yim
	  if(nspot .gt. max_coords) then
	   return_string = ' '
	   iolabel(nlabels) = 
     *     'Too many spots for program, <cr> to return main menu'
	   call ximageioboxdisplay(iolabel,return_string,nlabels)
	   close(idevcoord)
	   return
	  end if
	  ixcoord(nspot) = nint(xim)
	  iycoord(nspot) = nint(yim)
	  go to 230
C*** imagic format
	 else if(imagic) then
  240     read(idevcoord,*,end=250) xim,yim
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
C*** open new file for coordinates
	else
	 iolabel(nlabels) = 'Opening a new file...'
	 nlabels = nlabels + 1
         iolabel(nlabels) = 
     *   'Mark cursor positions with left hand mouse button'
	 call ximagelabeldisplay(iolabel,nlabels)
	 open(unit=idevcoord,
     *        file=coordfile(1:lnblank(coordfile)),
     *        status='new',err=210)
C*** standard format
	 if(standard) then
	  if(image) then
	   write(idevcoord,'(''        x         y      density'')')
	  else
	   write(idevcoord,'(
     *     ''        x         y    intensity   amplitude'')')
	  end if
C*** spider format
	 else if(spider) then
	  write(idevcoord,'('' ; File output from Ximdisp : '',a)') 
     *    mapfile(1:lnblank(mapfile))
	 end if
	end if
	go to 300
C*******************************************************************
C*** all coords read in, decompress them and write to screen
C*******************************************************************
  250   do n=1,nspot	  
	 call convert_to_screen(ixcoord(n),iycoord(n),ixscreen,iyscreen)
C*** draw box around spot
         maxx = ixscreen + ibox_size
         minx = ixscreen - ibox_size
         maxy = iyscreen + ibox_size
         miny = iyscreen - ibox_size
         call ximagedrawbox(minx,miny,maxx,maxy)
C*** draw number beside it
	 string = ' '
	 string = intoch(n,nchars)
	 call ximagedrawtext(maxx,maxy,string)
C*** prepare label
	 nlabels = nlabels + 1
C*** spot ok, include in list, shuffle label lines if necessary
	 if(nlabels .eq. max_lines_per_page) then
	  do i=4,max_lines_per_page-3
	   iolabel(i) = iolabel(i+2)
	  end do
	  nlabels = max_lines_per_page - 2
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
	open(unit=idevcoord,
     *       file=coordfile(1:lnblank(coordfile)),status='unknown')
	if(spider) then
	 write(idevcoord,'('' ; File output from Ximdisp : '',a)') 
     *   mapfile(1:lnblank(mapfile))
	else if(standard) then
	 if(image) then
	  write(idevcoord,'(''        x         y      density'')')
	 else
	  write(idevcoord,'(
     *    ''        x         y    intensity   amplitude'')')
	 end if
	end if
C****************************************************************************
C*** all spots from previously created files displayed, add new ones
C****************************************************************************
  300	call ximagelabeldisplay(iolabel,nlabels)
	menulist(1) = 'Hide zoom area'
	menulist(2) = 'Delete previous position'
	menulist(3) = 'Return main menu'
	call ximagemenuinit(menulist,3)
	job = -1
  500   call ximagewait(job)
	if(job .lt. 0) then
	 go to 500
C*** read coord position
	else if(job .eq. 0) then
	 call ximagereadmenupointer(ixscreen,iyscreen)
	 if(ixscreen .lt. 0 .or. ixscreen .gt. max_display_width .or.
     *      iyscreen .lt. 0 .or. iyscreen .gt. max_display_height)
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
         call ximagedrawbox(minx,miny,maxx,maxy)
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
	 string = ' '
	 string = intoch(nspot,nchars)
	 call ximagedrawtext(maxx,maxy,string)
	 call convert_to_image(ixscreen,iyscreen,ixim,iyim)
C*** read pixel colour
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
	  aval = aline(ixtrans)
	  bval = aline(ixtrans+1)
	  den = aval*aval + bval*bval
	  amplitude = sqrt(den)
	  if(bval .eq. 0) then
	   phase = 0.
	  else
	   phase = atan(aval/bval)
	  end if
	 end if
C*** store coords and density
	 ixcoord(nspot) = ixim
	 iycoord(nspot) = iyim
	 density(nspot) = den
C***
C*** shuffle label lines if necessary
	 nlabels = nlabels + 1
	 if(nlabels .ge. max_lines_per_page) then
	  do i=4,max_lines_per_page-3
	   iolabel(i) = iolabel(i+2)
	  end do
	  nlabels = max_lines_per_page - 2
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
     *    '' int,amp,phase = '',f12.0,f8.0,f6.2)') 
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
C*** remove number from screen
         call remove_box(minx,miny,maxx,maxy)
	 string = ' '
	 string = intoch(nspot,nchars)
	 call ximageremovetext(maxx,maxy,string)
C*** redisplay label
	 nlabels = nlabels - 2
	 call ximagelabeldisplay(iolabel,nlabels)
	 nspot = nspot - 1
	 go to 500	
C*** return main menu
	else if(job .eq. 3) then
	 call ximagemenuhide
	 call ximagelabelhide
C*** write standard format spots
	 if(standard) then
	  if(image) then
	   do n=1,nspot
	    write(idevcoord,'(2i10,f12.1)')
     *      ixcoord(n),iycoord(n),density(n)
	   end do
	  else
	   do n=1,nspot
	    amplitude = sqrt(density(n))
            write(idevcoord,'(2i10,2f12.1)')
     *      ixcoord(n),iycoord(n),density(n),amplitude
	   end do
	  end if
C*** write spider format spots
	 else if(spider) then
	  do n=1,nspot
	   xim = float(ixcoord(n)) 
	   yim = float(iycoord(n))
	   write(idevcoord,'(i5,i2,2f12.6)') n, 2, xim,yim
	  end do
C*** write imagic format spots
	 else if(imagic) then
C*** imagic coordinate system horiz +ve y to right, vertical +ve x down
	  do n=1,nspot
	   xim = float(nxyz(2) - iycoord(n)) 
	   yim = float(ixcoord(n))
	   write(idevcoord,*) xim,yim,one
	  end do
	 end if
	 close(idevcoord)
	 return
	end if
        end
C********************************************************************
C***
	subroutine modify_zoom
C***
C********************************************************************
C*** subroutine to change zoom factor
	include 'hXM_common.for'
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
      subroutine remove_box(ix1,iy1,ix2,iy2)
C***
C****************************************************************************
C*** subroutine to remove a box from the screen
C****************************************************************************
	include    'hXM_common.for'
	call ximageremovelines(ix1,iy1,ix2,iy1,1)
	call ximageremovelines(ix2,iy1,ix2,iy2,1)
	call ximageremovelines(ix1,iy2,ix2,iy2,1)
	call ximageremovelines(ix1,iy1,ix1,iy2,1)
	return
	end
C****************************************************************************
C***
      subroutine remove_cross(ix,iy,isize)
C***
C****************************************************************************
C*** subroutine to remove a small cross from the screen
C****************************************************************************
	include    'hXM_common.for'
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
        subroutine splinefit
C***
C*************************************************************************
C*** subroutine to fit a spline to a filament,
C*** straightening the filament by shear, and writing new image
C*************************************************************************
        include    'hXM_common.for'
C***
 1000   call ximagemenuhide
 	call ximagerubberenable(2)
        iolabel(nlabels+1) = 
     *  'Ctrl/Left button down to start at bottom left corner'
        iolabel(nlabels+2) = 
     *  'Drag box to top right corner'
        iolabel(nlabels+3) = 
     *  'Release mouse button to record final position'
	call ximagelabeldisplay(iolabel,nlabels+3)
	menulist(1) = 'Re-start box specification'
	menulist(2) = 'Return main menu'
	call ximagemenuinit(menulist,2)
	job = -1
 1100   call ximagewait(job)
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
	call convert_to_image(mnx,mny,iminx,iminy)
	call convert_to_image(mxx,mxy,imaxx,imaxy)
	numx = mxx - mnx + 1
	numy = mxy - mny + 1
C*** check dimensions
	if(numx * numy .gt. max_overlay .or.
     *     imaxx - iminx .ge. max_spline_width .or.
     *     imaxy - iminy .ge. max_spline_length) then
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
C 20.09.98	imap = 0
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
	else if(job.eq.0) then
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
	else if(job.eq.1) then
	 call ximageremovecircle(ixp(npts),iyp(npts),2)
	 call ximageremovetext(ixp(npts),iyp(npts),string(1:2))
	 npts = npts - 1
	 go to 1800
C********************************
C*** restart spline
C********************************
	else if(job.eq.2) then
	 call ximageremovevectors
	 call ximagedrawbox(mnx,mny,mxx,mxy)
	 npts = 1
	 go to 1800
C********************************
C*** finished spline
C********************************
	else if(job.eq.3) then
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
C***
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
C***
C*** calculate fitted curve
C***
 2200	call vc03ad
     *  (npts,knots,xden,yden,wd,rd,xn,fn,gn,dn,theta,iprint,w)	
C***
C*** loop over y
C***
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
C***
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
C***
C*** display straightened filament by shear(linear interp on every line)
C***
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
     *   (mxx+numx/2+10,mny+numy/2-5,numx,numy,mapbuf,ierr)
	 call ximagelabelhide
	 if(ierr .eq. 1) then
	  iolabel(nlabels+1) = 'Warning : X event error'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	 end if
	 go to 3000
C***
C*** display straightened filament -normal mode of bending, bi-linear
C***
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
     *   (mxx+numx/2+10,mny+numy/2-5,numx,numy,mapbuf,ierr)
	 call ximagelabelhide
	 if(ierr .eq. 1) then
	  iolabel(nlabels+1) = 'Warning : X event error'
	  call ximagelabeldisplay(iolabel,nlabels+1)
	 end if
	 go to 3000
C*** refit spline
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
C*** return main menu
	else if(job.eq.4) then
	 call ximagemenuhide
	 return
	end if
C***
C*** Calculated image displayed, create output file or fit further
 3000   call ximagelabeldisplay(iolabel,nlabels)
	menulist(1) = 'Create image output file'
	menulist(2) = 'Further image calculation'
	menulist(3) = 'Return main menu'
	call ximagemenuinit(menulist,3)
	job = -1
 3100	call ximagewait(job)
	if(job .le. 0) then
	 go to 3100
C*** create output file
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
	 call getdate(date)
	 title = 'Ximdisp splinefit: straightened'//date
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
C*** further calculations
	else if(job.eq.2) then
	 call ximagemenuhide
	 call ximagelabelhide
	 go to 2400
C*** return main menu
	else if(job.eq.3) then
	 call ximagemenuhide
	 call ximagelabelhide
	 call ximageoverlayhide
	 call ximageremovevectors
	 return
	end if
	end
C
C
c==========================================================
c
        SUBROUTINE GETSPO
C
	include		'hXM_common.for'
C
          if(idf_mapmode.eq.2)then
            ispotcross_size = 0.30*(sqrt(x1*x1+y1*y1)+sqrt(x2*x2+y2*y2))
          else
            ispotcross_size = 0.15*(sqrt(x1*x1+y1*y1)+sqrt(x2*x2+y2*y2))
          endif
 3401	  continue
          inspt=0
          call shorten(spotfile,i)
          write(*,'('' i = '',I6)') i
          write(iolabel(nlabels+0),
     *    '(''Type in name for spotfile ('',A,'')'')') 
     *    spotfile(1:i)
	  call ximageioboxdisplay(iolabel,spotfile,nlabels+0)
	  if(spotfile .ne. ' ') then
	    old = .false.
 	    there = .false.
            inquire(file=spotfile,exist=there)
C-----------Prepare empty spotlist field
            do I=0,maxsptlst
              do J=-maxsptlst,maxsptlst
                ispotlst(I,J)=0
              enddo
            enddo
            inspt =0
	    if(there) then
C-------------Read existing spotlist 
              open(unit=idevspt,file=spotfile,status='old',err=3401)
 3404         continue
                read(idevspt,*,ERR=3405,END=3406)isptx,ispty
                if(isptx.lt.0)then
                  isptx=-isptx
                  ispty=-ispty
                endif
                if(isptx.le.maxsptlst .and. ispty.le.maxsptlst .and. 
     1                ispotlst(isptx,ispty).eq.0)then
                  ispotlst(isptx,ispty)=1
                  ixim=isptx*x1+ispty*x2+x0
                  iyim=isptx*y1+ispty*y2+y0
                  call convert_to_screen(ixim,iyim,ilxscree1,ilyscree1)
                  call draw_cross(ilxscree1,ilyscree1,ispotcross_size)
                  isptx=-isptx
                  ispty=-ispty
                  ixim=isptx*x1+ispty*x2+x0
                  iyim=isptx*y1+ispty*y2+y0
                  call convert_to_screen(ixim,iyim,ilxscree2,ilyscree2)
 	          call draw_cross(ilxscree2,ilyscree2,ispotcross_size)
                  inspt=inspt+1
                endif
              goto 3404
C
 3405         continue
              write(6,'(''ERROR while reading spotlist'')')
 3406         continue
	      close(idevspt)
              write(6,'(I6,'' spots read from spotlist file.'')')inspt
            endif
C
            ixlocmin=0
            iylocmin=0
C
 3410       continue
C
	    call ximagemenuhide
	    ierr = 0
            nlabels = 1
C	    if(nspots.gt.0) call lattice_display
   	    write(iolabel(nlabels+0),
     *       '(''Number of defined spots is '',I6)') inspt
   	    write(iolabel(nlabels+1),
     *       '(''Last clicked spot was '',2I6)') ixlocmin,iylocmin
            iolabel(nlabels+2) = 
     *      'Mark spot positions with lh mouse button'
	    call ximagelabeldisplay(iolabel,nlabels+2)
	    menulist(1) = 'Save spotlist and return'
	    call ximagemenuinit(menulist,1)
	    job = -1
 3411       call ximagewait(job)
	    if(job.lt.0) goto 3411
C***************************************************
C*** read spot position, and indices
C***************************************************
            if(job.eq.0) then
	      call ximagelabelhide
	      call ximagemenuhide
C*** read cursor position
              call ximagereadmenupointer(ixscreen, iyscreen)
              call convert_to_image(ixscreen,iyscreen,ixim,iyim)
C *** correct y position so that origin bottom left
              if(ixim.lt.0)then
                ixim=-ixim
                iyim=-iyim
              endif 
              rx=float(ixim)
              ry=float(iyim)
              rdist2=rx*rx+ry*ry
C
C*** calculate the coordinates of this spot
C 
              rdim2=999999999999.0
              do i=-maxsptlst,maxsptlst
                do j=-maxsptlst,maxsptlst
                  tx=x1*i+x2*j+x0
                  ty=y1*i+y2*j+y0
                  rdi2=abs(tx-rx)+abs(ty-ry)
                  if(rdi2.lt.rdim2) then
                    rdim2=rdi2
                    ixlocmin=i
                    iylocmin=j
                  endif
                end do
              end do
              if(ixlocmin.lt.0)then
                ixlocmin=-ixlocmin
                iylocmin=-iylocmin
              endif
              ixim=ixlocmin*x1+iylocmin*x2+x0
              iyim=ixlocmin*y1+iylocmin*y2+y0
C
              call convert_to_screen(ixim,iyim,ilxscree1,ilyscree1)
              ilxmin=-ixlocmin
              ilymin=-iylocmin
              ilxim=ilxmin*x1+ilymin*x2+x0
              ilyim=ilxmin*y1+ilymin*y2+y0
              call convert_to_screen(ilxim,ilyim,ilxscree2,ilyscree2)
C
              if(ispotlst(ixlocmin,iylocmin).eq.1)then
                ispotlst(ixlocmin,iylocmin)=0
                call remove_cross(ilxscree1,ilyscree1,ispotcross_size)
 	        call remove_cross(ilxscree2,ilyscree2,ispotcross_size)
                inspt=inspt-1
              else
                ispotlst(ixlocmin,iylocmin)=1
                call draw_cross(ilxscree1,ilyscree1,ispotcross_size)
 	        call draw_cross(ilxscree2,ilyscree2,ispotcross_size)
                inspt=inspt+1
              endif
C
              goto 3410
C
            else
C
C*** Write out the spot indices
C
              inspt =0
	      if(.not.there) then
                open(unit=idevspt,file=spotfile,status='new',err=3412)
              else
                open(unit=idevspt,file=spotfile,status='old',err=3412)
              endif
              do i=0,maxsptlst
                do j=-maxsptlst,maxsptlst
                  if(ispotlst(i,j).eq.1)then
                    write(idevspt,'(I6,'','',I6)')i,j
                    inspt=inspt+1
                  endif
                end do
              end do 
              goto 3413
 3412         continue
              write(6,'(''ERROR while opening spotlist for write'')')
 3413         continue
              close(idevspt)
              write(6,'(I9,'' spots written to spotlist file.'')')inspt
C
             endif
           endif
C
	  return
          end
c
c
c
c==========================================================
c
        SUBROUTINE SAVLAT(iwhich)
C
	include		'hXM_common.for'
C
 3401	  continue
        if(iwhich.eq.1)then
          write(iolabel(nlabels+0),
     1    '(''Save to 1st, good lattice position in '',
     2      ''DATAFILE.dat ? (0=no,1=yes) (default=0)'')') 
        else
          write(iolabel(nlabels+0),
     1    '(''Save to 2nd, bad lattice position in '',
     2      ''DATAFILE.dat ? (0=no,1=yes) (default=0)'')') 
        endif
        write(czeile,'('' '')')
	call ximagemenuhide
	call ximageioboxdisplay(iolabel,czeile,nlabels+0)
        write(iolabel(nlabels+0),
     1    '(''Nothing done, DATAFILE.dat not changed.'')')
	if(czeile .ne. ' ') then
	  read(czeile,*,ERR=200)ianswer
          if(ianswer.eq.1)then
            write(czeile,'(''cp -f DATAFILE.dat datafile.backup'')')
            call shorten(czeile,k)
            write(*,'('' Running: '',A)')czeile(1:k)
	    call system(czeile(1:k))
C 
            write(czeil1,'(F9.3,'','',F9.3,'','',F9.3,'','',F9.3)')
     1        x1,y1,x2,y2
            call inkomma(czeil1(1:39),k)
            if(iwhich.eq.1)then
              iline=32
            else if(iwhich.eq.2)then
              iline=35
            else
              goto 200
            endif
            write(czeile,'(''tredat 4 DATAFILE.dat '',I2,'' '',A)')
     1        iline,czeil1(1:k)
            call shorten(czeile,k)
            write(*,'('' Running: '',A)')czeile(1:k)
	    call system(czeile(1:k))
C
            write(czeile,'(''tredat 5 DATAFILE.dat '',I3,'' 43 M'')')iline
	    call shorten(czeile,k)
	    write(*,'('' Running: '',A)')czeile(1:k)
	    call system(czeile(1:k))
C
            if(iwhich.eq.1)then
C-------------Save origin only for first lattice
              write(czeil1,'(F9.3,'','',F9.3)')
     1          df_orix,df_oriy
              call inkomma(czeil1(1:39),k)
              write(czeile,'(''tredat 4 DATAFILE.dat 58 '',A)')
     1          czeil1(1:k)
              call shorten(czeile,k)
              write(*,'('' Running: '',A)')czeile(1:k)
              call system(czeile(1:k))
C
              write(czeile,'(''tredat 5 DATAFILE.dat 58 43 M'')')
              call shorten(czeile,k)
              write(*,'('' Running: '',A)')czeile(1:k)
              call system(czeile(1:k))
            endif
C
            write(czeile,'(''tredat 6 DATAFILE.dat '')')
            call shorten(czeile,k)
            write(*,'('' Running: '',A)')czeile(1:k)
	    call system(czeile(1:k))
C 
            write(iolabel(nlabels+0),
     1        '(''Lattice into DATAFILE.dat inserted.'')')
          endif
        endif
C
        goto 900
 200    continue
          write(*,'('' ERROR in SAVLAT. DATAFILE.dat not changed.'')')
         
 900    continue
	return
        end
c
c
c
c==========================================================
c
      SUBROUTINE SHORTEN(czeile,k)
C
C counts the number of actual characters not ' ' in czeile
C and gives the result out in k.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1  CTMP1, CTMP2
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
      CHARACTER * 100 CZEIL2
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
      IF(CTMP1.EQ.CTMP3)then
        WRITE(CZEILE(I:I),'('' '')')
        K=K-1
      endif
C
      if(k.lt.1)k=1
C
C     WRITE(*,'('' INKOMMA 4: k='',I4,'':'',A)')k,CZEILE(1:k)
C
      RETURN
      END
C


