C*********************************************************************
C*** imconv - program to convert old style map to imsubs2000 27.06.00
C*********************************************************************
	integer*4	idevin
	parameter	(idevin = 1)
	integer*4	idevout
	parameter	(idevout = 2)
	integer*4	maxsiz
	parameter	(maxsiz = 50000)
C***
	character	filin*512
	character	filout*512
	character	title*80
	character	titles(20)*80
C***
	integer*4	nxyz(3)
	integer*4	mxyz(3)
C***
	logical		there
C***
	real*4		aline(maxsiz)
CTSH++
	character*1	yesno
	character	date*24
CTSH--
C***
	write(6,'(''Type input file name...'')')
	read(5,'(a)') filin
  100   write(6,'(''Type output file name...'')')
	read(5,'(a)') filout
        there = .false.
        inquire(file=filout,exist=there)
        if(there) then
         write(6,'(''File already exists, overwrite it ? (y/n)'')')
         read(5,'(a)') yesno
         if(yesno .ne. 'y' .and. yesno .ne. 'Y') go to 100
        end if
C***
	call imopen(idevin,filin,'old')
	call imopen(idevout,filout,'new')
	call irdhdr(idevin,nxyz,mxyz,mode,dmin,dmax,dmean)
	call itrhdr(idevout,idevin)
	call iwrhdr(idevout,title,-1,dmin,dmax,dmean)
	nx = nxyz(1)
	ny = nxyz(2)
	nz = nxyz(3)
C***
	dmin = 100000000.
	dmax = -dmin
	dmean = 0.
	do iz=1,nz
	 do iy=1,ny
	  call irdlin(idevin,aline)
	  do ix=1,nx
	   den = aline(ix)
	   dmin = min(dmin,den)
	   dmax = max(dmax,den)
	   dmean = dmean + den
	  end do
	  call iwrlin(idevout,aline)
	 end do
	end do
	dmean = dmean / float(nx * ny * nz)
	title = filin(1:lnblnk(filin))//' converted to Image2000 format'
	call iwrhdr(idevout,title,0,dmin,dmax,dmean)
	call imclose(idevin)
	call imclose(idevout)
C***
	write(6,'(''File successfully converted to new format.'')')
	stop
	end
