C*IMSUBS2000.FOR*********************************************************
C									*
C	HIGHER-LEVEL FORTRAN SUBROUTINES FOR READING/WRITING		*
C	IMAGE FILES							*
C									*
C	Last Update:	23.May.84	DAA	 	VAX		*
C       Bug fixed in IWRHDR in DO loop 100  JMS				*
C       IALUVW,IRTUVW added by KAT 12.09.85                             *
C	Bug fixed in IALSYM by KAT 2.10.85				*
C       IRTSYM,IRTSAM added by JMS 11.10.85                             *
C       Brought to the Alliant by RAC and RB 11.9.91                    *
C       Format statement in IMOPEN changed to I10 by RH 6.1.92          *
C	Mod to IRDHDR to position map format maps correctly JMS 10.6.94 *
C	QWRITE added to IWRHDR if IALSYM spacegroup set=1 RH 25.7.99    *
C	major rewrite to add machine stamp to header and move origin 	*
C				JMS	29.11.2000			*
C	Mods to error statements to account for architecture faults	*
C				JMS	11.01.01			*
C	Bug fix to repositioning after IRDPAS, IWRPAS jms 12.06.01	*
C	error trap installed for file stream overflow jms 16.08.04      *
C	  file stream numbers are created in library.c and appear       *
C	  to accumulate, therefore you cannot open/close > 5 files      *
C									*
C************************************************************************
C									*
C	HEADER FORMAT							*
C	1	NX	number of columns (fastest changing in map)	*
C	2	NY	number of rows					*
C	3	NZ	number of sections (slowest changing in map)	*
C	4	MODE	data type :					*
C			0	image : signed 8-bit bytes range -128 	*
C					to 127				*
C			1	image : 16-bit halfwords		*
C			2	image : 32-bit reals			*
C			3	transform : complex 16-bit integers	*
C			4	transform : complex 32-bit reals	*
C	5	NXSTART	number of first column in map			*
C	6	NYSTART	number of first row in map			*
C	7	NZSTART	number of first section in map			*
C	8	MX	number of intervals along X			*
C	9	MY	number of intervals along Y			*
C	10	MZ	number of intervals along Z			*
C	11-13	CELLA	cell dimensions in angstroms			*
C	14-16	CELLB	cell angles in degrees				*
C	17	MAPC	axis corresp to cols (1,2,3 for X,Y,Z)		*
C	18	MAPR	axis corresp to rows (1,2,3 for X,Y,Z)		*
C	19	MAPS	axis corresp to sections (1,2,3 for X,Y,Z)	*
C	20	DMIN	minimum density value				*
C	21	DMAX	maximum density value				*
C	22	DMEAN	mean density value				*
C	23	ISPG	space group number 0 or 1 (default=0)		*
C	24	NSYMBT	number of bytes used for symmetry data (0 or 80)*
C	25-49   EXTRA	extra space used for anything			*
C	50-52	ORIGIN  origin in X,Y,Z used for transforms		*
C	53	MAP	character string 'MAP ' to identify file type	*
C	54	MACHST	machine stamp					*
C	55	RMS	rms deviation of map from mean density		*
C	56	NLABL	number of labels being used			*
C	57-256	LABEL(20,10) 10 80-character text labels		*
C									*
C************************************************************************
C***
C***    IMOPEN
C***
C************************************************************************
C	Open file NAME with qualities ATBUTE ('RO' 'OLD' 'NEW' SCRATCH')
C	and associate with stream ISTREAM 
C	(ISTREAM = # between 1 & 12 ; MAX of  5 files opened at any time!!)
C************************************************************************
	subroutine imopen(istream,name,atbute)
	character*(*)	name
	character*(*)	atbute
	character*7	at2
	character*60 	fullname
	character*4	mapname
C***
	include 'imsubs_common.for'
C***
	data nbhdr/1024/
	data nb/1,2,4,4,8/
	data nbl/800/
	data flag/5*.true./
	data nocon/5*.false./
	data numopen/0/
	data old/5*.false./
C*************************************************
C***   Check for valid unit number
C*************************************************
	if (istream .gt. 12) then
	  write(6,1000)
1000	  format(//' IMOPEN: Invalid STREAM number!!!'//)
	  stop 'Open Error !!!'
	end if
	numopen = numopen + 1
	if (numopen .gt. max_units) then
	  write(6,1100)
1100	  format(//' IMOPEN: No More than 5 files can be opened!!!'//)
	  stop 'open error!!!'
	endif
C*****************************************************************************
C***   Open file
C*****************************************************************************
C*** note :
C*** 'OLD'      means it *must* already exist and you can read it and write it.
C*** 'NEW'      means it *must not* already exist and you cannot read from it
C***            until you have written something to it.
C*** 'UNKNOWN'  means you can write to it whether it exists or not,
C***            but you cannot read from it until you have written to it.
C***            i.e. it is truncated at creation.
C*** 'SCRATCH'  'OLD' with automatic deletion after closing.
C*****************************************************************************
C***
        at2=atbute
	call ccpupc(at2)
	call qopen(lstream(istream),name,at2)
	j = lstream(istream)
	if(j .gt. max_units) then
	 write(6,'(//'' IMOPEN: file stream number overflow!!!''//)')
	 stop 'Open error!!!'
	end if
	flag(j) = .true.
	nocon(j) = .false.
	old(j) = .false.
	unknown(j) = .false.
	karch(j) = 0
C********************************************************************
C*** architecture+new/old-style map notes :
C***	Old style, different-architecture map cannot be read or overwritten
C***    Old style, same-architecture map can be read, not overwritten
C***    New style, different-architecture map can be read, not overwritten 
C***    New style, same-architecture map can be read, and overwritten
C*************************************************************************
	if (at2 .eq. 'NEW' .or. at2 .eq. 'SCRATCH') then
	 go to 1500
C*** 'unknown' files are created if they do not already exist,
C*** but if they do they are zeroed immediately upon the first
C*** read i.e. they are assumed write only
	else if(at2 .eq. 'UNKNOWN') then
	 unknown(j) = .true.
         write(6,
     *   '(/''!!! Warning - file : '',a,'' has UNKNOWN status !!!''/)')
     *   (name(1:numchars(name)))
	 go to 1500
	end if
        call qmode(j,0,nchhdr)
	call qseek(j,1,52*4+1,1024)
 	call qreadi(j,mapname,4,ier)
	call qseek(j,1,16*4+1,1024)
	call qmode(j,6,nchhdr)
	call qreadi(j,mapcrs(1,j),3,ier)
	do i=1,3
	 if(mapcrs(i,j) .eq. 1) go to 1200
	end do
C*** If architecture same set karch flag = 0, else architecture flag = 1
	karch(j) = 1
	write(6,
     *  '(/''!!! Warning - architecture incompatibility !!! '')') 
C***************************************************************
C*** set old / new style map flag 
C***************************************************************
 1200   if(mapname .ne. 'MAP ') then
C*** old style, architecture compatible map
	 if(karch(j) .eq. 0) then
	  write(6,'(/''!!! Warning - old style 20th century map : '',a)')
     *    (name(1:numchars(name)))
	  old(j) = .true.
	  go to 1500
	 end if
	 write(6,
     * '(''!!! Fatal error - cannot read from or write to file : '',a)') 
     *   (name(1:numchars(name)))
         write(6,
     * '(''!!! Use byte_swap_map followed by image_convert to reformat''
     *   '' maps correctly !!!'')')
	 stop
	else
	 write(6,'(/''!!! New style 21st century map : '',a)') 
     *   (name(1:numchars(name)))
	end if
C*************************************************
C*** tests over, read machine stamp
C*************************************************
        call qrarch(j,53,ireslt)
	if(ireslt .eq. 0) then
	 write(6,'(/''!!! Warning - no machine stamp in file : '',a)') 
     *   (name(1:numchars(name)))
	end if
C***************************************************************************
C***   Setting mode type to byte for compatibility with previous VMS version
C***************************************************************************
 1500   call qmode(lstream(istream),0,nchitm)
C*************************************************
C***	 Get and print file name
C*************************************************
        call qqinq(j,name,fullname,nfilsz)
	if (at2 .eq. 'NEW' .or. at2 .eq. 'SCRATCH' .or.
     *      at2 .eq. 'UNKNOWN') then
	  write(6,2000) at2,istream,fullname
	else
	  write(6,2100) at2,istream,fullname,nfilsz
	endif
2000	format(/' Filename for ',a,' image file on unit',i4,' : ',a/)
2100	format(/' Filename for ',a,' image file on unit',i4,' : ',a/
     .	'                             Size= ',i10,/)
	return
C************************************************************************
C***
C***	IMFORM
C***
C************************************************************************
C*** returns flag for old 20th century or new 21st cent map
C************************************************************************
	entry imform(istream,iform)
	j = lstream(istream)
	if(old(j)) then
	 iform = -1
	else
	 iform = 0
	end if
	return
C************************************************************************
C***
C***	IMCLOSE
C***
C************************************************************************
C	Close file on stream ISTREAM. This frees up this channel for
C	further use.
C************************************************************************
	entry imclose(istream)
	call qclose(lstream(istream))
	numopen = numopen - 1
	if (numopen .lt. 0) numopen = 0
	return
	end
C************************************************************************
C***
C***	IRDHDR
C***
C************************************************************************
C	Read the header on unit ISTREAM, Print the contents,
C	and return some parameters to the caller:
C
C	inxyz		  : size of file Columns, Rows, Sections
C	mxyz		  : # of intervals Columns, Rows, Sections
C	imode		  : Data storage mode (1-4)
C				0 = Image		Integer*1
C				1 = Image               Integer*2
C				2 = Image               Reals
C				3 = Fourier Transform   Integer*2
C				4 = Fourier Transform   Reals
C	dmin,dmax,dmean   : Min, Max, & Mean densities
C
	subroutine irdhdr(istream,inxyz,mxyz,imode,dmin,dmax,dmean)
C***
	integer*4	inxyz(3)
	integer*4	iuvw(3)
	integer*4	labels(1)
	integer*4	lxyz(3)
CHEN>
        character*1     cxyz(3)
CHEN<
	integer*4	mxyz(3)
	integer*4	nxyzst(3)
C***
	real*4		title(1)
	real*4		cell(6)
	real*4		extra(1)
C***
        character*80 	spgsym1
C***
	include 'imsubs_common.for'
C***
	logical		cflag
C*******************************************************
C*** Initialize values
C*******************************************************
CHEN>
C	data lxyz/'X','Y','Z'/
	data cxyz/'X','Y','Z'/
CHEN<
        data ier /0/
	spgsym1 = ' X,Y,Z'
C*************************************************
C***	 Read header
C*************************************************
	j = lstream(istream)
	if(unknown(j)) then
	 write(6,
     * '(''!!! Fatal error - attempted UNKNOWN status file read !!!'')')
	 stop
	end if
	flag(j) = .false.
	call qseek(j,1,1,1)
	call qmode(j,6,nchhdr)
	call qreadi(j,ncrs(1,j),3,ier)
	if (ier .ne. 0) goto 99
	call qreadi(j,mode(j),1,ier)
	call qreadi(j,ncrst(1,j),3,ier)
	call qreadi(j,nxyz(1,j),3,ier)
	call qmode(j,2,nchhdr)
	call qreadr(j,cel(1,j),6,ier)
	call qmode(j,6,nchhdr)
	call qreadi(j,mapcrs(1,j),3,ier)
	call qmode(j,2,nchhdr)
	call qreadr(j,denmmm(1,j),3,ier)
	call qmode(j,6,nchhdr)
	call qreadi(j,nspg(j),1,ier)
	call qreadi(j,nbsym(j),1,ier)
	call qreadi(j,stuff(1,j),25,ier)
	call qmode(j,2,nchhdr)
	call qreadr(j,origin(1,j),3,ier)
	call qmode(j,2,nchhdr)
	call qreadr(j,stamp(1,j),2,ier)
	call qreadr(j,rms(j),1,ier)
	call qmode(j,6,nchhdr)
	call qreadi(j,nlab(j),1,ier)
	call qmode(j,0,nchhdr)
	call qreadi(j,labls(1,1,j),nbl,ier)
C*************************************************
C*** finished reading, move arrays
C*************************************************
	call ccpmvi(inxyz,ncrs(1,j),3)
	call ccpmvi(mxyz,nxyz(1,j),3)
	imode = mode(j)
	dmin = denmmm(1,j)
	dmax = denmmm(2,j)
	dmean = denmmm(3,j)
C*****************************************************
C*** extract origin from old style map and set rms = 0
C*****************************************************
	if(old(j)) then
	 origin(1,j) = stamp(2,j)
	 origin(2,j) = rms(j)
	 origin(3,j) = 0.
	 rms(j) = 0.
	end if
C*************************************************
C***  skip over symmetry operators ?
C*************************************************
	if (nspg(j) .gt. 0 .and. nbsym(j) .gt. 0) then
	  call qmode(j,0,nchhdr)
	  call qskip(j,nbsym(j))		
	else
	  nspg(j) = 0
	  nbsym(j) = 0
	endif
C*************************************************
C*** Write out header information
C*************************************************
CHEN>
C	write(6,1000) inxyz,imode,(ncrst(k,j),k=1,3),mxyz,
C     .	(cel(k,j),k=1,6),(lxyz(mapcrs(k,j)),k=1,3),
C     .	dmin,dmax,dmean,(origin(k,j),k=1,3),rms(j),nspg(j),nbsym(j),
C     .  nlab(j),((labls(i,k,j),i=1,20),k=1,nlab(j))
C
C1000	format(/
C     .  7X,'Number of columns, rows, sections ........ ',3I6/
C     .  7X,'Map mode ................................. ',I6/
C     .  7X,'Start points on columns, rows, sections .. ',3I6/
C     .  7X,'Grid sampling on x, y, z ................. ',3I6/
C     .  7X,'Cell axes ................................ ',3F10.2/
C     .  7X,'Cell angles .............................. ',3F10.2/
C     .  7X,'Fast, medium, slow axes .................. ',3(5X,A1)/
C     .  7X,'Minimum density .......................... ',F25.12/
C     .  7X,'Maximum density .......................... ',F25.12/
C     .  7X,'Mean density ............................. ',F25.12/
C     .  7X,'Origin ................................... ',3F10.2/
C     .  7X,'RMS deviation ............................ ',F25.12/
C     .	7X,'Space group, # bytes symmetry ............ ',2I6/
C     .  7X,'Number of titles ......................... ',I6//
C     . ' Titles :'/10(1X,20A4/))
	write(6,1000) inxyz,imode,(ncrst(k,j),k=1,3),mxyz,
     .	(cel(k,j),k=1,6),(cxyz(mapcrs(k,j)),k=1,3),
     .	dmin,dmax,dmean,(origin(k,j),k=1,3),rms(j),nspg(j),nbsym(j),
     .  nlab(j),((labls(i,k,j),i=1,20),k=1,nlab(j))
C***
1000	format(/,
     .  7X,'Number of columns, rows, sections ........ ',3I6,/,
     .  7X,'Map mode ................................. ',I6,/,
     .  7X,'Start points on columns, rows, sections .. ',3I6,/,
     .  7X,'Grid sampling on x, y, z ................. ',3(A1,','),/,
     .  7X,'Cell axes ................................ ',3F10.2,/,
     .  7X,'Cell angles .............................. ',3F10.2,/,
     .  7X,'Fast, medium, slow axes .................. ',3(5X,A1),/,
     .  7X,'Minimum density .......................... ',F25.12,/,
     .  7X,'Maximum density .......................... ',F25.12,/,
     .  7X,'Mean density ............................. ',F25.12,/,
     .  7X,'Origin ................................... ',3F10.2,/,
     .  7X,'RMS deviation ............................ ',F25.12,/,
     .	7X,'Space group, # bytes symmetry ............ ',2I6,/,
     .  7X,'Number of titles ......................... ',I6/,/,
     . ' Titles :',/,10(1X,20A4,/))
CHEN<
	return
C***********************************************************************
C***
C***	IWRHDR
C***
C***********************************************************************
C	Write out Header record to OUTPUT file. This is the ONLY
C	routine that writes the header.
C
C	title is a single 80 character title.
C	ntflag controls title writing.
C	-1 = No title is added to set
C	 0 = This is only title
C	 1 = Add this title at end
C	 2 = This is first title, push others down.
C***********************************************************************
	entry iwrhdr(istream,title,ntflag,dmin,dmax,dmean)
C***
	j = lstream(istream)
	if(old(j)) then
	 write(6,6564)
6564     format
     *   ('!!! Error - cannot overwrite old-style 20th century map.'/
     *   '    Use image_convert program and retry.')
	 stop
	else if(karch(j) .ne. 0) then
	 write(6,6565)
6565     format
     *   ('!!! Error - cannot overwrite map of different machine '//
     *   'architecture - use image_convert program and retry.')
	 stop
	end if
C***
	flag(j) = .false.
	denmmm(1,j) = dmin
	denmmm(2,j) = dmax
	denmmm(3,j) = dmean
	stamp(1,j) = rstamp
C***
	goto (20,10,11,12) ntflag+2
10	nlab(j) = 1
	call ccpmvi(labls(1,1,j),title,20)
	goto 20
11	nlab(j) = min(10,nlab(j)+1)
	call ccpmvi(labls(1,nlab(j),j),title,20)
	goto 20
12	k = min(9,nlab(j))
	do 100 i = k,1,-1
	  call ccpmvi(labls(1,i+1,j),labls(1,i,j),20)
100	continue
	call ccpmvi(labls(1,1,j),title,20)
	nlab(j) = k + 1
C**************************************************************
C*** write header
C**************************************************************
20	call qseek(j,1,1,1)
        call qmode(j,6,nchhdr)
	call qwriti(j,ncrs(1,j),3)
	call qwriti(j,mode(j),1)
	call qwriti(j,ncrst(1,j),3)
	call qwriti(j,nxyz(1,j),3)
        call qmode(j,2,nchhdr)
	call qwritr(j,cel(1,j),6)
        call qmode(j,6,nchhdr)
	call qwriti(j,mapcrs(1,j),3)
        call qmode(j,2,nchhdr)
	call qwritr(j,denmmm(1,j),3)
        call qmode(j,6,nchhdr)
	call qwriti(j,nspg(j),1)
	call qwriti(j,nbsym(j),1)
	call qwriti(j,stuff(1,j),25)
        call qmode(j,2,nchhdr)
	call qwritr(j,origin(1,j),3)
	call qmode(j,2,nchhdr)
	call qwritr(j,stamp(1,j),2,ier)
	call qwritr(j,rms(j),1)
        call qmode(j,6,nchhdr)
	call qwriti(j,nlab(j),1)
        call qmode(j,0,nchhdr)
	call qwriti(j,labls(1,1,j),nbl)
C**************************************************************
C*** write machine stamp
	call qwarch(j,53)
C*** write symmetry operators
        if(nspg(j) .eq. 1 .and. nbsym(j) .ne. 0) then
C*** position pointer at end of header
	 call qmode(j,0,nchhdr)
         call qseek(j,2,1,nbhdr)
C*** assume spacegroup = 1 with 80 bytes of p1 symmetry
         nbsym(j) = 80
	 call qmode(j,0,nchhdr)
         call qwriti(j,spgsym1,80)
        else if(nspg(j) .ne. 0) then
         stop 'Error - IMSUBS spacegroup must be 0 or 1'
        endif
C*** move to start of data
	call qmode(j,0,nchhdr)
        call qseek(j,2,1,nbhdr+nbsym(j))
	return
C*********************************************************************
C***
C***	ITRHDR(ISTREAM,JSTREAM)
C***
C*********************************************************************
C	Transfer header information from JSTREAM to ISTREAM
C	Note: Information is internally transfered, NOT actually written
C	to file!!!
C	no symmetry operators are written to output file
C	ispg and nbsym are set to 0 in new header!!!!
C	unless explicitly set by a call to IALSYM
C*********************************************************************
C***
	entry itrhdr(istream,jstream)
C***
	j = lstream(istream)
	k = lstream(jstream)
	call ccpmvi(ncrs(1,j),ncrs(1,k),3)	
	mode(j) = mode(k)
	call ccpmvi(ncrst(1,j),ncrst(1,k),3)
	call ccpmvi(nxyz(1,j),nxyz(1,k),3)
	call ccpmvi(cel(1,j),cel(1,k),6)
	call ccpmvi(mapcrs(1,j),mapcrs(1,k),3)
	call ccpmvi(denmmm(1,j),denmmm(1,k),3)
	call ccpmvi(stuff(1,j),stuff(1,k),25)
	call ccpmvi(origin(1,j),origin(1,k),3)
	call ccpmvi(stamp(1,j),stamp(1,k),2)
C*** reset space group and number of bytes symmetry data to 0
	nspg(j) = 0
	nbsym(j) = 0
	rms(j) = rms(k)
	nlab(j) = nlab(k)
	call ccpmvi(labls(1,1,j),labls(1,1,k),200)
	return
C*********************************************************************
C***
C***	ITRLAB(ISTREAM,JSTREAM)
C***
C*********************************************************************
C	Transfer LABELS from JSTREAM to ISTREAM
C	Note: Information is internally transfered, NOT actually written
C	to file!!!
C*********************************************************************
	entry itrlab(istream,jstream)
C***
	j = lstream(istream)
	k = lstream(jstream)
	nlab(j) = nlab(k)
	call ccpmvi(labls(1,1,j),labls(1,1,k),200)
	return
C*********************************************************************
C***
C***	ITRCEL(ISTREAM,JSTREAM)
C***
C*********************************************************************
C	Transfer CELL parameters from JSTREAM to ISTREAM
C	Note: Information is internally transfered, NOT actually written
C	to file!!!
C*********************************************************************
	entry itrcel(istream,jstream)
C***
	j = lstream(istream)
	k = lstream(jstream)
	call ccpmvi(cel(1,j),cel(1,k),6)
	return
C*********************************************************************
C**
C***	ICRHDR
C***
C*********************************************************************
C	Create new header. All of the standard image defaults are
C	set up given the requested information. Header NOT written!!
C NOTE: The starting point for Columns,Rows,Sections
C	are set to 0 by default.!!!!!!!!!
C
C	INXYZ		  : size of file Columns, Rows, Sections
C	MXYZ		  : # of intervals Columns, Rows, Sections
C	IMODE		  : Data storage mode (1-4)
C				0 = Image		Integer*1
C				1 = Image               Integer*2
C				2 = Image               Reals
C				3 = Fourier Transform   Integer*2
C				4 = Fourier Transform   Reals
C	LABELS(20,N)	  :N=1,10 Up to 10 80 character labels
C	NL		  :Actual # of labels to use (0 is O.K.)
C
C*********************************************************************
	entry icrhdr(istream,inxyz,mxyz,imode,labels,nl)
C***
	j = lstream(istream)
	mode(j) = imode
	ml = min(nl,10)
	ml = max(ml,0)
	nlab(j) = ml
	nspg(j) = 0
	nbsym(j) = 0
	rms(j) = 0.0
	do 200 k = 1,3
	  ncrs(k,j) = inxyz(k)
	  nxyz(k,j) = mxyz(k)
	  cel(k,j) = mxyz(k)
	  cel(k+3,j) = 90.0
	  ncrst(k,j) = 0
	  mapcrs(k,j) = k
	  denmmm(k,j) = 0.0
	  origin(k,J) = 0.0
200	continue
	call ccpzi(stuff(1,j),25)
	call ccfill(labls(1,1,j),' ',nbl)
	if (ml .gt. 0) call ccpmvi(labls(1,1,j),labels,ml*20)
	return
C*********************************************************************
C***
C***	IALCEL
C***
C*********************************************************************
C	Alter CELL information for file. Header NOT actually written!!!
C
C	CELL(6)		  : new unit cell parameters (a,b,c, alpha,beta,gamma)
C*********************************************************************
	entry ialcel(istream,cell)
C***
	j = lstream(istream)
	call ccpmvi(cel(1,j),cell,6)
	return
C*********************************************************************
C***
C***	IALCON
C***
C*********************************************************************
C	Alter data Conversion mode. USE WITH GREAT CARE!!!!!!!!!!
C	By default, all data is passed to the user or received from
C	the user as REALS or COMPLEX REALS, independent of storage mode
C	on the disk.
C	This routine allows the direct transmission of data to and from
C	the disk WITHOUT ANY FORMAT conversion.
C
C	CFLAG	=	 .TRUE.  for conversion (default value)
C	CFLAG	=	 .FALSE. for NO conversion
C
C*********************************************************************
	entry ialcon(istream,cflag)
C***
	nocon(lstream(istream)) = .not.cflag
	return
C*********************************************************************
C***
C***	IALEXT
C***
C*********************************************************************
C	Alter EXTRA information stored in "unused" poritions of
C	file header (25 words max!!). Header NOT actually written!!!
C
C	EXTRA		  : Buffer containing data to be placed in extra slot.
C	ISTART		  : Which "extra" element to start at (1-25)
C	NEXTRA		  : Number of words to transfer
C*********************************************************************
	entry ialext(istream,extra,istart,nextra)
C***
	j = lstream(istream)
        if(istart+nextra .gt. 26 .or. istart .lt. 1)
     .    stop 'IALEXT: Attempt to write out of bounds'         
        call ccpmvi(stuff(istart,j),extra,nextra)
	return
C*********************************************************************
C***
C***	IALLAB
C***
C*********************************************************************
C	Alters label information. All spaces beyond NL are set = blanks
C
C	LABELS(20,10)	  : space for labels
C	NL		  : actual # of labels being used
C
C*********************************************************************
	entry iallab(istream,labels,nl)
C***
	j = lstream(istream)
	nlab(j) = min(10,nl)
	call ccpmvi(labls(1,1,j),labels,nlab(j)*20)
	if (nl .lt. 10) then
	  do k = nl+1,10
	    call ccfill(labls(1,k,j),' ',80)
	  end do
	end if
	return
C*********************************************************************
C***
C***	IALMOD
C***
C*********************************************************************
C	Alter MODE in hedaer. Header NOT actually written!!!
C	IMODE		  : Data storage mode (1-4)
C				0 = Image		Integer*1
C				1 = Image               Integer*2
C				2 = Image               Reals
C				3 = Fourier Transform   Integer*2
C				4 = Fourier Transform   Reals
C*********************************************************************
	entry ialmod(istream,imode)
	mode(lstream(istream)) = imode
	return
C*********************************************************************
C***
C****	IALORG
C***
C*********************************************************************
C	Alter ORIGIN information
C
C	XORIG,YORIG,ZORIG	  : X,Y,Z origin information
C*********************************************************************
	entry ialorg(istream,xorig,yorig,zorig)
C***
	j = lstream(istream)
	origin(1,j) = xorig
	origin(2,j) = yorig
	origin(3,j) = zorig
	return
C*********************************************************************
C***
C***	ialrms
C***
C*********************************************************************
C	Alter RMS information
C
C	rmsdev	  : rms deviation
C*********************************************************************
	entry ialrms(istream,rmsdev)
C***
	j = lstream(istream)
	rms(j) = rmsdev
	return
C*********************************************************************
C***
C***	IALSAM
C***
C*********************************************************************
C	Alter SAMPLING information for file. Header NOT actually written!!!
C
C       MXYZ		  : # of intervals Columns, Rows, Sections
C*********************************************************************
	entry ialsam(istream,mxyz)
C***
	j = lstream(istream)
	call ccpmvi(nxyz(1,j),mxyz,3)
	return
C*********************************************************************
C***
C***	IALSIZ
C***
C*********************************************************************
C	Alter SIZE information for file. Header NOT actually written!!!
C
C	INXYZ		  : size of file Columns, Rows, Sections
C	NXYZST		  : starting # for Columns, Rows, Sections (usually 1) 
C*********************************************************************
	entry ialsiz(istream,inxyz,nxyzst)
C***
	j = lstream(istream)
	call ccpmvi(ncrs(1,j),inxyz,3)
	call ccpmvi(ncrst(1,j),nxyzst,3)
	return
C*********************************************************************
C***
C***	IALSYM
C***
C*********************************************************************
C       Alters symmetry parameters in the header
C
C       KSPG            : Space group number
C       KBS             : Number of bytes of symmetry data
C                         usually written in 80 byte lines
C*********************************************************************
        entry ialsym(istream,kspg,kbs)
C***
C**************************************************************
C*** set up space group and symmetry info- assume spacegroup = 1 
C*** with 80 bytes of p1 symmetry
C**************************************************************
	if(kspg .eq. 0) then
	 kbs = 0
	else if(kspg .eq. 1) then
      	 kbs = 80
        else
      	 stop ' IMSUBS spacegroup must be 0 or 1'
      	endif
        j = lstream(istream)
	nspg(j) = kspg
	nbsym(j) = kbs
        return
C*********************************************************************
C***
C***	IALUVW
C***
C*********************************************************************
C	Alters the matrix to permute cell dimensions 
C*********************************************************************
	entry ialuvw(istream,iuvw)
C***
	j = lstream(istream)
	call ccpmvi(mapcrs(1,j),iuvw,3)
	return
C*********************************************************************
C***
C***	IRTCEL
C***
C*********************************************************************
C	Return CELL information from file. 
C
C	CELL(6)		  : unit cell parameters (a,b,c, alpha,beta,gamma)
C*********************************************************************
	entry irtcel(istream,cell)
C***
	j = lstream(istream)
	call ccpmvi(cell,cel(1,j),6)
	return
C*********************************************************************
C***
C***	IRTEXT
C***
C*********************************************************************
C	Returns EXTRA information stored in "unused" portions of
C	file header (25 words max!!).
C
C	EXTRA		  : Buffer containing data to be placed in extra slot.
C	ISTART		  : Which "extra" element to start at (1-25)
C	NEXTRA		  : Number of words to transfer
C*********************************************************************
	entry irtext(istream,extra,istart,nextra)
C***
	j = lstream(istream)
	if (istart .gt. 25 .or. istart .lt. 1) 
     .	stop 'IRTEXT: Error in start number'
	jextra = min(nextra + istart,26) - istart
	call ccpmvi(extra,stuff(istart,j),jextra)
	return
C*********************************************************************
C***
C***	IRTLAB
C***
C*********************************************************************
C	Return label information
C
C	LABELS(20,10)	  : space for labels
C	NL		  : actual # of labels being used
C*********************************************************************

	entry irtlab(istream,labels,nl)
C***
	j = lstream(istream)
	nl = nlab(j)
	call ccpmvi(labels,labls(1,1,j),nl*20)
	return
C*********************************************************************
C***
C***	IRTORG
C***
C*********************************************************************
C
C	Returns ORIGIN information
C
C	XORIG,YORIG	  : X,Y origin information
C*********************************************************************
	entry irtorg(istream,xorig,yorig,zorig)
C***
	j = lstream(istream)
	xorig = origin(1,j)
	yorig = origin(2,j)
	zorig = origin(3,j)
	return
C*********************************************************************
C***
C***	IRTRMS
C
C*********************************************************************
C	Returns RMS information
C
C	RMSDEV	  : RMS deviation information
C*********************************************************************
	entry irtrms(istream,rmsdev)
C***
	j = lstream(istream)
	rmsdev = rms(j)
	return
C*********************************************************************
C***
C***	IRTSAM
C***
C*********************************************************************
C       returns sampling information
C
C       MXYZ                    :# of intervals columns,rows,sections
C*********************************************************************
        entry irtsam(istream,mxyz)
C***
        j = lstream(istream)
        call  ccpmvi(mxyz,nxyz(1,j),3)
        return
C*********************************************************************
C***
C***	IRTSIZ
C***
C*********************************************************************
C	RETURN SIZE information for file. Header NOT actually written!!!
C
C	INXYZ		  : size of file Columns, Rows, Sections
C	MXYZ		  : size of full map Columns, Rows, Sections
C	NXYZST		  : starting # for Columns, Rows, Sections (usually 1) 
C*********************************************************************
	entry irtsiz(istream,inxyz,mxyz,nxyzst)
C***
	j = lstream(istream)
	call ccpmvi(inxyz,ncrs(1,j),3)
	call ccpmvi(mxyz,nxyz(1,j),3)
	call ccpmvi(nxyzst,ncrst(1,j),3)
	return
C*********************************************************************
C***
C***	IRTSYM
C***
C*********************************************************************
C       returns symmetry parameters
C
C       KSPG               : space group number
C       KBS                : number of bytes of symmetry data
C                            usually written as 80 byte lines
C*********************************************************************
        entry irtsym(istream,kspg,kbs)
C***
        j = lstream(istream)
	kspg = nspg(j)
	kbs = nbsym(j)
        return
C*********************************************************************
C***
C***	IRTUVW
C***
C*********************************************************************
C       returns the matrix to permute cell dimensions 
C*********************************************************************
	entry irtuvw(istream,iuvw)
C***
	j = lstream(istream)
	call ccpmvi(iuvw,mapcrs(1,j),3)
	return
C*********************************************************************
C*** end of file error handling
C*********************************************************************
99	write(6,'('' IRDHDR: End-of-File error !!!'')')
	stop 'Error'
	end
C*********************************************************************
C***
C***	IRDLIN
C***	IRDSEC
C***	IRDPAL
C***
C*********************************************************************
C	Read in a line, a section or part of a line (NX1 - NX2)
C	into the REAL or COMPLEX REAL array ARRAY. If required,
C	Integer*1 or Integer*2 data on disk is converted to REALS.
C	After reading a partial line, the pointer is positioned to
C	the START of the next line.
C       Byte data is stored with numbers from 128 to 255 interpreted 
C       as -128 to -1, but returned to the calling program as their
C       original values 128 - 255.  
C NOTE:	The start of a line is ALWAYS 0 (ie NX1,NX2 are relative)
C*********************************************************************
	subroutine irdlin(istream,array,*)
C***
	include 'imsubs_common.for'
C***
	byte 		bline(8192)
	byte		qb(2)
C***
	integer*2 	line(4096)
	integer*2	ib
	integer*4 	kb
C***
	real*4		array(*)
C***
	equivalence 	(line,bline),
     *			(ib,qb)
C***
C*** irdlin
	itype = 1
	go to 1
C*** irdsec
	entry irdsec(istream,array,*)
	itype = 2
	go to 1
C*** irdpal
	entry irdpal(istream,array,nx1,nx2,*)
	itype = 3
	go to 1
C***
1	j = lstream(istream)
	if(unknown(j)) then
	 write(6,
     * '(''!!! Fatal error - attempted UNKNOWN status file read !!!'')')
	 stop
	end if
	jmode = mode(j)
	jb = nb(jmode + 1)
	qb(2) = 0
C************************************
C*** read line
C************************************
	if (itype .eq. 1) then				
	  nread = ncrs(1,j)
C************************************
C*** read section
C************************************
	else if (itype .eq. 2) then			
	  nread = ncrs(1,j)*ncrs(2,j)
C************************************
C*** read part of line
C************************************
	else if (itype .eq. 3) then			
	  nread = (nx2 - nx1  + 1)
C************************************
C*** relative start!!!!
C************************************
	  nskip = nx1 * jb				
	  call qmode(j,0,nchhdr)
	  call qskip(j,nskip)
	end if
C************************************
C*** integer*1
C************************************
	if (jmode .eq. 0) then			
	 call qmode(j,0,nchhdr)
C*** no conversion
	 if (nocon(j)) then
	  call qreadi(j,array,nread,ier)
	  if (ier .ne. 0) goto 99
	 else
	  index = 1
10	  n = min(8192,nread)
	  call qreadi(j,bline,n,ier)
	  if (ier .ne. 0) go to 99
C************************************
C*** add 256 to -ve numbers 
C************************************
	  do k = 1,n
	   kb = bline(k)
	   if (kb .lt. 0) kb = kb + 256
	   array(index) = kb
	   index = index + 1
	  end do
	  nread = nread - 8192
	  if (nread .gt. 0) go to 10
	 end if
C************************************
C*** else integer*2
C************************************
	else if (jmode .eq. 1 .or. jmode .eq. 3) then						
	 call qmode(j,1,nchhdr)
C*** nread *2 for transform (mode 3)
	 nread = nread * jb / 2
C*** no conversion
	 if (nocon(j)) then
	  call qreadi(j,array,nread,ier)
	  if (ier .ne. 0) go to 99
	 else
	  index = 1
15	  n = min(4096,nread)
	  call qreadi(j,line,n,ier)
	  if (ier .ne. 0) go to 99
	  do k = 1,n
	   array(index) = line(k)
	   index = index + 1
	  end do
	  nread = nread - 4096
	  if (nread .gt. 0) go to 15
	 end if
C************************************
C*** else real*4
C************************************
	else
	 call qmode(j,2,nchhdr)
C*** nread *2 for transform (mode 4)
	 nread = nread * jb / 4
	 call qreadr(j,array,nread,ier)
	 if (ier .ne. 0) go to 99
	end if
C************************************
C*** skip to end of record
C************************************
	if (itype .eq. 3) then
C************************************
C** if partial read
C************************************
	 nskip = (ncrs(1,j) - nx2 - 1) * jb		
	 call qmode(j,0,nchhdr)
	 call qskip(j,nskip)
	end if
	return
99	return 1
	end
C*********************************************************************
C***
C***	IMPOSN
C***
C*********************************************************************
C	Position Read/Write pointer to RELATIVE Section NZ and Line NY
C*********************************************************************
	subroutine imposn(istream,nz,ny)
C***
	include 'imsubs_common.for'
C***
	j = lstream(istream)
	flag(j) = .false.
	jb = nb(mode(j) + 1)
	nso = nz + 1
	if (nso .lt. 1) nso = 1
	nro = ny
	if (nro .lt. 0) nro = 0
	nrc = nro*ncrs(1,j)*jb + 1
	nsize = ncrs(1,j)*ncrs(2,j)*jb
	call qmode(j,0,nchhdr)
	call qseek(j, nso, nrc + nbhdr + nbsym(j), nsize)
	return
	end
C*********************************************************************
C***
C***	IWRLIN
C***	IWRSEC
C***	IWRPAL
C***
C*********************************************************************
C	Write out a line, a section or part of a line (NX1 - NX2)
C	from the REAL or COMPLEX REAL array ARRAY. If required,
C	data is converted to Integer*2 format before writing to disk.
C	After writing a partial line, the pointer is always advanced
C	to the start of the next line.
C NOTE:	The start of a line is ALWAYS 0 (ie NX1,NX2 are relative)
C ANOTHER NOTE : in iwrpal, a part of the array denoted by nx1 and nx2
C       is written at the start of the record. The pointer is then
C       moved up 1 position by the system and then needs to be advanced
C       to the start of the next record.
C*********************************************************************
	subroutine iwrlin(istream,array)
C***
	include 'imsubs_common.for'
	byte 		bline(8192)
	byte		qb(2)
C***
	integer*2 	line(4096)
	integer*2	ib
	integer*4	kb
C***
	real*4		array(*)
C***
	equivalence 	(line,bline), 
     *			(ib,qb)
C***
	j = lstream(istream)
C***
C*** iwrlin
	itype = 1
	go to 1
C*** iwrsec
	entry iwrsec(istream,array)
	itype = 2
	go to 1
C*** iwrpal
	entry iwrpal(istream,array,nx1,nx2)
	itype = 3
	go to 1
C***
1       j = lstream(istream)
	if(old(j)) then
	 write(6,6563)
6563     format
     *   ('!!! Error - cannot overwrite old-style 20th century map.'/
     *   '    Use image_convert program and retry.')
	 stop
	else if(karch(j) .ne. 0) then
	 write(6,6562)
6562     format
     *   ('!!! Error - cannot overwrite map of different machine '//
     *   'architecture - use image_convert program and retry.')
	 stop
	end if
C***
	jmode = mode(j)
	jb = nb(jmode + 1)
	index = 1
C********************************************
C*** make sure past header
C********************************************
	if (flag(j)) then				
	 call qmode(j,0,nchhdr)
	 call qseek(j,2,1,nbhdr+nbsym(j))
	 flag(j) = .false.
	end if
C********************************************
C*** write line
C********************************************
	if (itype .eq. 1) then				
	  nwrite = ncrs(1,j)
C********************************************
C*** write section
C********************************************
	else if (itype .eq. 2) then			
	  nwrite = ncrs(1,j)*ncrs(2,j)
C********************************************
C*** write part of line
C********************************************
	else if (itype .eq. 3) then			
	  nwrite = nx2 - nx1  + 1
C********************************************
C*** for start @0
C********************************************
	  index = nx1 + 1				
	  if (jmode .ge. 3) index = 2*index - 1
	end if
C******************************************************************
C*** integer*1, subtract 256 if > 127 when converting to 4byte word
C******************************************************************
	if (jmode .eq. 0) then			
	 call qmode(j,0,nchhdr)
C*** no conversion
	 if (nocon(j)) then
	  call qwriti(j,array(index),nwrite)
	 else
10	  n = min(8192,nwrite)
	  do k = 1,n
	    kb = nint(array(index))
	    if(kb .gt. 127) kb = kb - 256
	    bline(k) = kb
	    index = index + 1
	  end do
	  call qwriti(j,bline,n)
	  nwrite = nwrite - 8192
	  if (nwrite .gt. 0) go to 10
	 end if
C********************************************
C*** else integer*2
C********************************************
	else if(jmode .eq. 1 .or. jmode .eq. 3) then					
	 call qmode(j,1,nchhdr)
C*** number to write *2 if transform - mode 3
	 nwrite = nwrite * jb / 2
C*** no conversion
	 if (nocon(j)) then
	  call qwriti(j,array(index),nwrite)
	 else
15	  n = min(4096,nwrite)
	  do k = 1,n
	    line(k) = nint(array(index))
	    index = index + 1
	  end do
	  call qwriti(j,line,n)
	  nwrite = nwrite - 4096
	  if (nwrite .gt. 0) go to 15
	 end if
C********************************************
C*** else real*4
C********************************************
	else 	
	  call qmode(j,2,nchhdr)
C*** number to write *2 if transform - mode 4
	  nwrite = nwrite * jb / 4
	  call qwritr(j,array(index),nwrite)
	end if
C**********************************************************************
C*** if partial write then make certain pointer is correctly positioned
C**********************************************************************
	if (itype .eq. 3) then
	  nskip = (ncrs(1,j) - nx2 + nx1 - 1) * jb
	  call qmode(j,0,nchhdr)
	  call qskip(j,nskip)
	end if
	return
	end
C
C
C
C_BEGIN_CCPUPC
      SUBROUTINE CCPUPC(STRING)
C     =========================
C
C---- Convert a text string to upper case in situ
C
C Arguments:
C ==========
C
C      STRING (I/O) CHARACTER*(*): STRING TO BE CONVERTED
C_END_CCPUPC
C
C     .. Scalar Arguments ..
      CHARACTER STRING* (*)
C     ..
C     .. Local Scalars ..
      INTEGER K,L,LL
      CHARACTER LC*26,UC*26
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX,LEN
C     ..
C     .. Data statements ..
      DATA LC/'abcdefghijklmnopqrstuvwxyz'/
      DATA UC/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C     ..
      LL = LEN(STRING)
      IF (LL.GT.0) THEN
        DO 10 L = 1,LL
          K = INDEX(LC,STRING(L:L))
          IF (K.NE.0) STRING(L:L) = UC(K:K)
   10   CONTINUE
      END IF
      END
C
C
C======================================================================
C_BEGIN_QLOCATE
C
C QLOCATE - return current position in file (measured in items)
C
C Usage:  CALL QLOCATE (IUNIT,LOCATE)
C         INTEGER       IUNIT,LOCATE
C
C Input:  IUNIT         stream to check
C
C Output: LOCATE        Current position in file or -1 for no file
C_END_QLOCATE
C See library.c
C
C======================================================================
C_BEGIN_QOPEN
C
C QOPEN - Open a file unit
C
C Usage:  CALL QOPEN   (IUNIT, LOGNAME, ATBUTE)
C         INTEGER       IUNIT
C         CHARACTER*(*) LOGNAME, ATBUTE
C
C Input:  IUNIT         unit number number to assign to file
C         LOGNAME       Logical name of file to open
C         ATBUTE        File status = 'UNKNOWN', 'SCRATCH', 'OLD',
C                                     'NEW', or 'READONLY'
C
C Output: None.
C_END_QOPEN
C
C======================================================================
C
      SUBROUTINE QOPEN(IUNIT,LOGNAM,ATBUTA)
C     =====================================
C
C     .. Scalar Arguments ..
      INTEGER IUNIT
      CHARACTER ATBUTA* (*),LOGNAM* (*)
C     ..
C     .. Local Scalars ..
      INTEGER ISTAT
      CHARACTER FOO*80
C     ..
C     .. External Subroutines ..
      EXTERNAL QQOPEN, CCPUPC
C     ..
      ISTAT = 0
      CALL CCPUPC(ATBUTA)
      IF (ATBUTA(:1).EQ.'U') ISTAT = 1
      IF (ATBUTA(:1).EQ.'S') ISTAT = 2
      IF (ATBUTA(:1).EQ.'O') ISTAT = 3
      IF (ATBUTA(:1).EQ.'N') ISTAT = 4
      IF (ATBUTA(:1).EQ.'R') ISTAT = 5
      IF (ISTAT.EQ.0) THEN
        FOO = ATBUTA
        CALL CCPERR(1,'Bad attribute in QOPEN: '//FOO)
      ENDIF
C
      CALL QQOPEN(IUNIT,LOGNAM,ISTAT)
      END
C**************************************************************
        integer function numchars(text)
C***************************************************************
C*** function to return the number of non-blank characters in a 
C*** string and set remainder to ' '
        character*(*) text
        character zer0
        jchars = len(text)
        zer0 = char(0)
        do 100 i=jchars,1,-1
         numchars = i
         if(text(i:i) .eq. zer0) then
          text(i:i) = ' '
          go to 100
         end if
         if(text(i:i).ne.' ') return
  100   continue
        return
        end
C======================================================================
C_BEGIN_QRARCH
C
C QRARCH - set up number conversion
C
C Usage:  CALL QRARCH   (IUNIT, IOFFSET, IRESLT)
C         INTEGER       IUNIT, IOFFSET, IRESLT
C
C Input:  IUNIT         unit number number to assign to file
C         IOFFSET       offset in words at which to find architecture
C                       information
C
C Output: IRESLT        fileFT + (16*fileIT) (see library C code)
C                       Zero if the stamp isn't present.
C
C     Reads the `machine stamp' giving information about the
C     architecture with which the file was written and arranges to
C     translate a foreign format to native with QREAD, dependent on the
C     current diskio mode.
C
C_END_QRARCH
C======================================================================
C
C======================================================================
C_BEGIN_QWARCH
C
C QWARCH - set up number conversion
C
C Usage:  CALL QWARCH   (IUNIT, IOFFSET)
C         INTEGER       IUNIT, IOFFSET
C
C Input:  IUNIT         unit number number to assign to file
C         IOFFSET       offset in words at which to write architecture
C                       information
C
C Output: None.
C
C     Writes the `machine stamp' giving information about the
C     architecture with which the file was written and which is used by
C     QRARCH
C
C_END_QWARCH
C======================================================================

C     for correct typing of qread/write calls
      SUBROUTINE QREADI (IUNIT,BUFFER,NITEMS,RESULT)
      INTEGER IUNIT, NITEMS, RESULT
      INTEGER BUFFER(*)
      CALL QREAD (IUNIT,BUFFER,NITEMS,RESULT)
      END
C
C
C======================================================================
C_BEGIN_QCLOSE
C
C QCLOSE - Close file unit
C
C Usage:  CALL QCLOSE (IUNIT)
C         INTEGER      IUNIT
C
C Input:  IUNIT        unit number assigned to file
C
C Output: None.
C_END_QCLOSE
C See library.c
C======================================================================
C_BEGIN_QMODE
C
C QMODE - Set mode for file access
C
C Usage:  CALL QMODE (IUNIT, MODE, NMCITM)
C         INTEGER     IUNIT, MODE, NMCITM
C
C Input:  IUNIT       unit number to assign to file
C         MODE        mode to switch into: 0 (BYTES), 1 (SMALL INTEGER),
C                                          2 (WORDS), 3 (SHORT COMPLEX),
C                                          4 (COMPLEX) 6 (INTEGER)
C
C Output: NMCITM      number of bytes per item on this machine.
C_END_QMODE
C See library.c
C======================================================================
C_BEGIN_QREAD
C
C QREAD - Read from IUNIT into BUFFER, NITEMS items
C
C Usage:  CALL QREAD (IUNIT,BUFFER,NITEMS,RESULT)
C         INTEGER     IUNIT, NITEMS, RESULT
C         REAL        BUFFER
C
C Input:  IUNIT       unit number assigned to file
C         NITEMS      number of items (item size set by QMODE)
C
C Output: RESULT      0 (no error), -1 (EOF) or number of items read
C         BUFFER      holds the items read
C_END_QREAD
C See library.c
C_BEGIN_QREADI
C
C QREADI - Read from IUNIT into BUFFER, NITEMS items
C
C Usage:  CALL QREADI (IUNIT,BUFFER,NITEMS,RESULT)
C         INTEGER     IUNIT, NITEMS, RESULT
C         INTEGER     BUFFER
C
C Input:  IUNIT       unit number assigned to file
C         NITEMS      number of items (item size set by QMODE)
C
C Output: RESULT      0 (no error), -1 (EOF) or number of items read
C         BUFFER      holds the items read
C_END_QREADI
C_BEGIN_QREADR
C
C QREADR - Read from IUNIT into BUFFER, NITEMS items
C
C Usage:  CALL QREADR (IUNIT,BUFFER,NITEMS,RESULT)
C         INTEGER     IUNIT, NITEMS, RESULT
C         REAL        BUFFER
C
C Input:  IUNIT       unit number assigned to file
C         NITEMS      number of items (item size set by QMODE)
C
C Output: RESULT      0 (no error), -1 (EOF) or number of items read
C         BUFFER      holds the items read
C_END_QREADR
C_BEGIN_QREADQ
C
C QREADQ - Read from IUNIT into BUFFER, NITEMS items
C
C Usage:  CALL QREADQ (IUNIT,BUFFER,NITEMS,RESULT)
C         INTEGER     IUNIT, NITEMS, RESULT
C         COMPLEX     BUFFER
C
C Input:  IUNIT       unit number assigned to file
C         NITEMS      number of items (item size set by QMODE)
C
C Output: RESULT      0 (no error), -1 (EOF) or number of items read
C         BUFFER      holds the items read
C_END_QREADQ
C_BEGIN_QREADC
C
C QREADC - Read bytes from IUNIT to fill BUFFER
C
C Usage:  CALL QREADC (IUNIT,BUFFER,RESULT)
C         INTEGER     IUNIT, RESULT
C         CHARACTER*(*) BUFFER
C
C Input:  IUNIT       unit number assigned to file
C
C Output: RESULT      0 (no error), -1 (EOF) or number of items read
C         BUFFER      holds the items read.  If necessary, use a substring
C                     of the CHARACTER variable
C_END_QREADC
C======================================================================
C_BEGIN_QWRITE
C
C QWRITE - Write to IUNIT from BUFFER, NITEMS items
C
C Usage:  CALL QWRITE (IUNIT,BUFFER,NITEMS)
C         INTEGER      IUNIT, NITEMS
C         REAL         BUFFER
C
C Input:  IUNIT        unit number assigned to file
C         NITEMS       number of items (item size set by QMODE)
C         BUFFER       holds the items to write
C
C Output: None.
C_END_QWRITE
C_BEGIN_QWRITI
C
C QWRITI - Write to IUNIT from BUFFER, NITEMS items
C
C Usage:  CALL QWRITI (IUNIT,BUFFER,NITEMS)
C         INTEGER      IUNIT, NITEMS
C         INTEGER      BUFFER
C
C Input:  IUNIT        unit number assigned to file
C         NITEMS       number of items (item size set by QMODE)
C         BUFFER       holds the items to write
C
C Output: None.
C_END_QWRITI
C_BEGIN_QWRITR
C
C QWRITR - Write to IUNIT from BUFFER, NITEMS items
C
C Usage:  CALL QWRITR (IUNIT,BUFFER,NITEMS)
C         INTEGER      IUNIT, NITEMS
C         REAL         BUFFER
C
C Input:  IUNIT        unit number assigned to file
C         NITEMS       number of items (item size set by QMODE)
C         BUFFER       holds the items to write
C
C Output: None.
C_END_QWRITR
C_BEGIN_QWRITQ
C
C QWRITQ - Write to IUNIT from BUFFER, NITEMS items
C
C Usage:  CALL QWRITQ (IUNIT,BUFFER,NITEMS)
C         INTEGER      IUNIT, NITEMS
C         COMPLEX      BUFFER
C
C Input:  IUNIT        unit number assigned to file
C         NITEMS       number of items (item size set by QMODE)
C         BUFFER       holds the items to write
C
C Output: None.
C_END_QWRITQ
C_BEGIN_QWRITEC
C
C QWRITEC - Write BUFFER to IUNIT
C
C Usage:  CALL QWRITEC (IUNIT,BUFFER)
C         INTEGER      IUNIT, NITEMS
C         CHARACTER*(*) BUFFER
C
C Input:  IUNIT        unit number assigned to file
C         BUFFER       holds the items to write.  If necessary, use a
C                      substring of the CHARACTER variable
C
C Output: None.
C_END_QWRITEC
C See library.c
C======================================================================
C_BEGIN_QSEEK
C
C QSEEK - Position a file pointer in a IUNIT
C
C Usage:  CALL QSEEK (IUNIT, IRECL, IEL, LRECL)
C         INTEGER     IUNIT, IRECL, IEL, LRECL
C
C Input:  IUNIT       unit number to assign to file
C         IRECL       "record number" to seek
C         IEL         element number to seek
C         LRECL       length of a "record"
C
C Output: None
C
C  QSEEK calculates the location as (IREC - 1)*LRECL + IEL. Note: as in
C        Fortran, addressing begins at 1 for both record & element
C        In these files, there are no true records: the use of "record length"
C        and "record number" in QSEEK, QSKIP, QBACK is purely notional.
C        For QSEEK, any combination of IREC, IEL & LRECL which gives the
C        same value of (IREC - 1)*LRECL + IEL is equivalent.
C
C_END_QSEEK
C See library.c
C======================================================================
C_BEGIN_QBACK
C
C QBACK - skip back 1 record of length LRECL
C
C Usage:  CALL QBACK (IUNIT,LRECL)
C         INTEGER     IUNIT, LRECL
C
C Input:  IUNIT       unit number assigned to file
C         LRECL       length of a record in items
C
C Output: None
C_END_QBACK
C See library.c
C======================================================================
C_BEGIN_QSKIP
C
C QSKIP - skip forward 1 record of length LRECL
C
C Usage:  CALL QSKIP (IUNIT,LRECL)
C         INTEGER     IUNIT, LRECL
C
C Input:  IUNIT       unit number assigned to file
C         LRECL       length of a record in items
C
C Output: None
C_END_QSKIP
C See library.c
C
C
C======================================================================
C_BEGIN_QQINQ
C
C QQINQ - check file name and size. Check IUNIT first, if no success
C         then try LOGNAM, if this fails use LOGNAM as filename.
C
C Usage:  CALL QQINQ   (IUNIT,LOGNAM,FILNAM,LENGTH)
C         INTEGER       IUNIT,LENGTH
C         CHARACTER*(*) LOGNAM,FILNAM
C
C Input:  IUNIT         handle to check (as returned by QOPEN)
C         LOGNAM        Logical name
C
C Output: FILNAM        the full file name or "" if no file
C         LENGTH        file size or -1 if no file
C
C_END_QQINQ
C======================================================================
C
      SUBROUTINE QQINQ(IUNIT,LFN,FILNAM,LENGTH)
C     =========================================
C
C     .. Parameters ..
      INTEGER ISTRLN
      PARAMETER (ISTRLN=500)
C     ..
C     .. Scalar Arguments ..
      INTEGER IUNIT,LENGTH
      CHARACTER FILNAM* (*),LFN* (*)
C     ..
C     .. Local Scalars ..
      CHARACTER FNAME* (ISTRLN),LNAME* (ISTRLN)
C     ..
C     .. External Subroutines ..
      EXTERNAL CQINQ,UGTENV
C     ..
      FNAME = ' '
      LNAME = LFN
      IF (LNAME.EQ.' ') LNAME = 'diskio.dft'
      CALL UGTENV(LNAME,FNAME)
      IF (FNAME.EQ.' ') FNAME = LNAME
      CALL CQINQ(IUNIT,FNAME,LENGTH)
      FILNAM = FNAME
C
      END

      SUBROUTINE QREADR (IUNIT,BUFFER,NITEMS,RESULT)
      INTEGER IUNIT, NITEMS, RESULT
      REAL BUFFER(*)
      CALL QREAD (IUNIT,BUFFER,NITEMS,RESULT)
      END
C
C
C_BEGIN_CCPMVI
      SUBROUTINE CCPMVI (IARR1,IARR2,NUM)
C     =================================
C
C  This routine assigns the first NUM words of IARR2 to IARR1
C
C Arguments:
C ==========
C
C    IARR1 (O)   INTEGER ARRAY(*)
C    IARR2 (O)   INTEGER ARRAY(*)
C     NUM (I)   Number of words to copy
C_END_CCPMVI
C
C  Arguments
      INTEGER NUM
      REAL IARR1(*),IARR2(*)
C
      INTEGER J
C
      DO 10 J=1,NUM
   10 IARR1(J)=IARR2(J)
      END

      SUBROUTINE QWRITI (IUNIT,BUFFER,NITEMS)
      INTEGER      IUNIT, NITEMS
      INTEGER      BUFFER(*)
      CALL QWRITE (IUNIT,BUFFER,NITEMS)
      END

      SUBROUTINE QWRITR (IUNIT,BUFFER,NITEMS)
      INTEGER      IUNIT, NITEMS
      REAL         BUFFER(*)
      CALL QWRITE (IUNIT,BUFFER,NITEMS)
      END
C
C
C_BEGIN_CCPZI
      SUBROUTINE CCPZI (IARR1,NUM)
C     ===========================
C
C  This routine assigns zero to IARR1 using NUM words
C
C Arguments:
C
C    IARR1 (O)   INTEGER ARRAY(*): array to be zeroed
C     NUM (I)   INTEGER: Number of words
C_END_CCPZI
C
C  Arguments ..........
      INTEGER NUM, IARR1(*)
C
      INTEGER J
C
      DO 10 J=1,NUM
   10 IARR1(J)=0
      END
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
C_BEGIN_CCPLIB
C     These are supposedly-machine-independent low-level routines.
C     They're actually machine-dependent at least insofar as some
C     contain non-standard code, but they do compile with the compilers
C     tried on unix as well as VMS.
C
C     fixme: the bit-twiddling should be in library.c, not here.
C     amalgamate ccppsf and fdir/fext/froot.  also add tests of these
C     routines to testlib.
C
C     $Id: ccplib.f,v 1.69 1999/12/16 16:04:29 mdw Exp $
C
C      CCFILL    Set specified number of elements of byte array
C      CCPALC    Call subroutine with allocated memory
C      CCPALE    Call subroutine with allocated memory set from environment
C      CCPBYI    Copy array of unsigned (or signed) bytes into integer array
C      CCPBYT    Indicate whether byte handling is available
C      CCPCPI    Copy array of BYTE or INTEGER*2 elements into integer array
C      CCPDAT    Get calendar date
C      CCPDEX    Periodicity reduction of 1024 (for PROLSQ)
C      CCPDPN    more friendly CCPOPN
C      CCPE2I    read integer from logical name value
C      CCPERR    Report error or normal termination and stop
C      CCPEXS    test if file exists
C      CCPFYP    Set up environment & parse command line arguments
C      CCPGI2    Get unsigned integer*2 value from 0 to 65535 from N'th
C                unsigned integer*2 element of array.
C      CCPGTB    Get unsigned byte value from 0 to 255 from N'th byte of
C                array.
C      CCPI2I    Copy an array of INTEGER*2 elements into an integer array
C      CCPIBY    Copy array of integers into array of bytes.
C      CCPII2    Copy array of integers into array of INTEGER*2 elements.
C      CCPLWC    Convert a string to lower case
C      CCPMDE    If byte handling available return nos. of bytes for map
C                modes
C      CCPMVB    Move bytes from one non-character array to another if
C                byte handling is available
C      CCPMVI    Move words from one integer array to another
C      CCPMVR    Move words from one real array to another
C      CCPNUN    Return an unconnected i/o unit number
C      CCPONL    See if program is being run interactively
C      CCPPSF    Parse file name into components
C      CCPRCS    Like CCPVRS but use RCS-format date string
C      CSETNV    Associate logical name with file name
C      CCPPAG    Set paging parameters if available
C      CCPSI2    Set integer value from 0 to 65535 into the N'th
C                unsigned integer*2 element of an array.
C      CCPSTB    Set integer value from 0 to 255 into N'th byte of array.
C      CCPSUM    Sum the elements of an array
C      CCPTIM    Get CPU and Elapsed times
C      CCPTOI    Convert n'th byte or I*2 in a non-character array to an
C                integer
C      CCPUFL    Supress underflow messages
C      CCPUPC    Convert a string to upper case
C      CCPVRS    Print program version number and date header
C      CCPZBI    Sets an array of bytes to zero
C      CCPZI     Set 'n' words of an integer array to zero using a simple loop
C      CCPZR     Set 'n' words of a real array to zero using a simple loop
C      FDIR      Returns the directory part of a file name
C      FEXTN     Returns the extension of a file name
C      FROOT     Returns the root of a file name
C      LITEND    determine endianness
C      LENSTR    length of string to last non-space
C      LUNSTI    Get logical unit number for input
C      LUNSTO    Get logical unit number for output
C      NBITST    Return the (unsigned) integer value held within a bit
C                field in a word
C      NOCRLF    write line supressing cr/lf to standard output
C      QPRINT    write debug messages
C      STBITS    Set a bit field within a word to a given (unsigned)
C                integer value
C_END_CCPLIB
C
C
C
C_BEGIN_CCFILL
      SUBROUTINE CCFILL(ARR1,SCAL,NTIMES)
C     ===================================
C
C      CCFILL    Set NTIMES bytes array ARR1 to value SCAL
C
C Arguments:
C ==========
C
C        ARR1 (O)   BYTE ARRAY (*): WHERE BYTES ARE TO BE COPIED
C        SCAL (I)   BYTE: value to be copied into ARR1
C      NTIMES (I)   INTEGER: NUMBER OF BYTES TO BE COPIED
C_END_CCFILL
C
C     .. Scalar Arguments ..
      INTEGER NTIMES
      BYTE SCAL
C     ..
C     .. Array Arguments ..
      BYTE ARR1(*)
C     ..
C     .. Local Scalars ..
      INTEGER N
C     ..
      DO 10 N = 1,NTIMES
        ARR1(N) = SCAL
   10 CONTINUE
C
      END
C
C
C     ===============================
C_BEGIN_CCPERR
      SUBROUTINE CCPERR(ISTAT,ERRSTR)
C     ===============================
C
C     Arguments:
C     ==========
C
C     ISTAT (I)   INTEGER: error/warning level
C
C         ISTAT=0  Normal termination and stop.
C         ISTAT=1  Fatal error and stop.
C         ISTAT=2  Report severe warning.
C         ISTAT=3  Report information.
C         ISTAT=4  Report from library
C            (ISTAT=-1 also report latest system error
C             and terminate)
C
C     ERRST (I)   CHARACTER*(*): message
C
C_END_CCPERR
C
C     .. Arguments ..
      INTEGER ISTAT
      CHARACTER ERRSTR*(*)
C     ..
C     .. Locals ..
      CHARACTER ERRBUF*100
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      LOGICAL VAXVMS
      EXTERNAL LENSTR, VAXVMS
C     ..
C     .. External Routines ..
      EXTERNAL CCPPNM, QPRINT, CEXIT, GETELAPSED, UGERR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C
C
      IF (ABS(ISTAT).LE.2) THEN
        call ccp4h_summary_beg()
      ENDIF
      IF (ISTAT.LT.0) THEN
        CALL UGERR(0,ERRBUF)
C     (avoid VMS `Message number 00000000')
        IF (ERRBUF .NE. ' ' .AND.
     +       ERRBUF.NE.'Message number 00000000') THEN
          CALL QPRINT(0,
     +         'Last system error message:')
          CALL QPRINT(0,ERRBUF)
        ENDIF
      ENDIF

C     construct appropriate ERRBUF
      IF (ABS(ISTAT).LE.1) THEN
C        report program name
         CALL CCPPNM (ERRBUF)
         IF (LENSTR(ERRBUF) .LT. 95) THEN
            ERRBUF (LENSTR(ERRBUF)+1:) = ': '
            ERRBUF (LENSTR(ERRBUF)+3:) = ERRSTR
         ENDIF
      ELSEIF (ISTAT.EQ.2) THEN
         ERRBUF = ' WARNING: '//ERRSTR
      ELSE
         ERRBUF = ERRSTR
      ENDIF

C     report messages and exit if appropriate
      IF (ABS(ISTAT).LE.1) THEN
        CALL QPRINT(0,ERRBUF)
        IF (VAXVMS()) THEN
          IF (ISTAT.EQ.0) THEN
            CALL GETELAPSED
C           success
            call ccp4h_pre_end()
            call ccp4h_summary_end()
            call ccp4h_html_close()
            CALL CEXIT(1)
          ELSE
C           FOR$_NOTFORSPE, "Not a FORTRAN-specific error"
            call ccp4h_pre_end()
            call ccp4h_summary_end()
            call ccp4h_html_close()
            CALL CEXIT(1605644)
          ENDIF
        ELSE
C         duplicate message to stderr, assumed to be connected to unit 0
          IF (ISTAT.EQ.1) WRITE (0,*) ERRBUF
          CALL GETELAPSED
          call ccp4h_pre_end()
          call ccp4h_summary_end()
          call ccp4h_html_close()
          CALL CEXIT(ISTAT)
        ENDIF
      ELSEIF (ISTAT.EQ.2) THEN
        CALL QPRINT(0,' ')
        CALL QPRINT(0,' $TEXT:Warning: $$ comment $$ ')
        CALL QPRINT(0,ERRBUF)
        CALL QPRINT(0,' $$')
        call ccp4h_summary_end()
      ELSE
        CALL QPRINT(0,ERRBUF)
      END IF
C
      RETURN
      END
C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
C     $Id: diskio.f,v 1.23 1999/12/22 16:34:25 pjx Exp $
C     
C_BEGIN_CCPLIB
C A set of Fortran subroutines to perform random access I/O on various
C data items (including bytes). Uses the C functions fopen, fclose,
C fread, fwrite, fseek, ftell, etc - by calling routines in library.c
C Note: IUNIT is NOT A Fortran Unit number, but an internal identifier
C
C  The calls provided are given below:
C
C  CALL QOPEN   (IUNIT,FILNAM,ATBUTE)        - Open file
C [CALL QQOPEN  (IUNIT,FILNAM,ISTAT)         - Open file: use QOPEN]
C  CALL QCLOSE  (IUNIT)                      - Close file
C  CALL QMODE   (IUNIT,MODE,NMCITM)          - Change mode
C  CALL QREAD   (IUNIT,ARRAY,NITEMS,IER)     - Read nitems
C  CALL QREADI  (IUNIT,ARRAY,NITEMS,IER)     - Read nitems into integer array
C  CALL QREADR  (IUNIT,ARRAY,NITEMS,IER)     - Read nitems into real array
C  CALL QREADQ  (IUNIT,ARRAY,NITEMS,IER)     - Read nitems into complex array
C  CALL QREADC  (IUNIT,CHAR,IER)             - Read bytes into character var.
C  CALL QWRITE  (IUNIT,ARRAY,NITEMS)         - Write nitems
C  CALL QWRITI  (IUNIT,ARRAY,NITEMS)         - Write nitems from integer array
C  CALL QWRITR  (IUNIT,ARRAY,NITEMS)         - Write nitems from real array
C  CALL QWRITQ  (IUNIT,ARRAY,NITEMS)         - Write nitems from complex array
C  CALL QWRITC  (IUNIT,CHAR)                 - Write bytes from character var.
C  CALL QSEEK   (IUNIT,IREC,IEL,LRECL)       - Move to irec,iel
C  CALL QBACK   (IUNIT,LRECL)                - Backspace 1 record
C  CALL QSKIP   (IUNIT,LRECL)                - Skip 1 record
C  CALL QQINQ   (IUNIT,LFILNM,FILNAM,LENGTH) - Get filename and length
C  CALL QLOCATE (IUNIT,LOCATE)               - Get position in file
C  CALL QRARCH (IUNIT, IOFFSET)              - set up number conversion
C  CALL QWARCH (IUNIT, IOFFSET)              - write conversion info
C
C  QSEEK calculates the location as (IREC - 1)*LRECL + IEL. Note: as in
C        Fortran, addressing begins at 1 for both record & element
C        In these files, there are no true records: the use of "record length"
C        and "record number" in QSEEK, QSKIP, QBACK is purely notional.
C        For QSEEK, any combination of IREC, IEL & LRECL which gives the
C        same value of (IREC - 1)*LRECL + IEL is equivalent.
C
C  Where:
C
C  IUNIT  = Variable returned by (Q)QOPEN to identify a file stream
C
C  FILNAM = file name for the stream (should be restricted to eight
C           characters for CCP4 programs)
C
C  ATBUTE = File status for opening file
C         = 'UNKNOWN', 'SCRATCH', 'OLD', 'NEW', or 'READONLY'
C
C  ISTAT  = File status on opening the file:
C           1, 'UNKNOWN'   open as 'OLD'/'NEW' check existence
C           2, 'SCRATCH'   open as 'OLD' and delete on closing
C           3, 'OLD'       file MUST exist or program halts
C           4, 'NEW'       create (overwrite) new file
C           5, 'READONLY'  self explanatory
C
C  NOTE: When using QQOPEN or QOPEN with ATBUTE = 'NEW' [ISTAT = 4],
C        a check is made on the environment variable CCP4_OPEN - 
C        if this is set to UNKNOWN then the file is opened with 
C        attribute UNKNOWN rather than NEW to allow overwriting files
C        that already exist.
C
C  MODE   = Access mode = 0, BYTES
C                       = 1, SHORT INT
C                       = 2, (REAL) WORD
C                       = 3, SHORT COMPLEX
C                       = 4, COMPLEX
C                       = 6, INTEGER
C
C  NMCITM = No. of machine items (eg bytes) per element
C  ARRAY  = Starting location for data storage in core
C     NOTE: This should normally be an array of full-word fortran items
C     (REAL or INTEGER) or double-word (COMPLEX) in the case that you
C     want to transfer complex numbers (mode 4).  If necessary, unpack
C     bytes using the routines provided in the library (or new ones).
C     In particular, DON'T try to use BYTE or INTEGER*2 arrays, as these
C     will likely cause alignment errors on RISC architectures.
C  CHAR   = CHARACTER*n buffer for transfer
C  NITEMS = Number of elements to transfer
C  IER    = Error flag (0 = no error) else number of words transferred
C  IREC   = Desired record number (starts at 1)
C  IEL    = Desired element number within record (word) (starts at 1)
C  LRECL  = Record length in elements
C
C  No. of channels and buffer length in words set in #DEFINE statements
C
C NOTE: use of QREAD/QWRITE is deprecated -- use QREAD<a>/QWRITE<a>
C with a buffer of the correct type.
C
C
C     Author: David Agard (Phil Evans and John Campbell)
C     Modified: For Unix/F77 using words (and bytes if available) (John Campbell)
C     Modified: For ccp ascii header system implemented (Jan Zelinka)
C_END_CCPLIB
C
C======================================================================
C_BEGIN_QQOPEN
C
C QQOPEN - Open a file unit
C
C    NOTE: the routine QOPEN (which calls QQOPEN) is to be preferred
C          to calling QQOPEN directly
C
C Usage:  CALL QQOPEN  (IUNIT, LOGNAME, ISTAT)
C         INTEGER       IUNIT, ISTAT
C         CHARACTER*(*) LOGNAME
C
C Input:  LOGNAME       Logical name of file to open
C         ISTAT         File status: 1 (UNKNOWN), 2 (SCRATCH), 3 (OLD),
C                                    4 (NEW) or 5 (READONLY)
C
C Output: IUNIT         Integer handle assigned to file. If negative
C                       the following error conditions occurred:
C                       -1 No more streams left
C                       -2 Could not open the file
C
C_END_QQOPEN
C======================================================================
      SUBROUTINE QQOPEN(IUNIT,LOGNAM,ISTAT)
C     =====================================
C
C     .. Parameters ..
      INTEGER ISTRLN,ISIZE
      PARAMETER (ISTRLN=500,ISIZE=20)
C     ..
C     .. Scalar Arguments ..
      INTEGER ISTAT,IUNIT
      CHARACTER LOGNAM* (*)
C     ..
C     .. Local Arrays ..
      CHARACTER MODES(5)*10
C     ..
C     .. Local Scalars ..
      INTEGER JSTAT
      CHARACTER ERRSTR*255,REWRIT* (ISIZE),USRNAM* (ISIZE),
     +     FNAME* (ISTRLN),LNAME* (ISTRLN)
      LOGICAL LNONAM
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPERR,CCPUPC,COPEN,QPRINT,UGTENV,UGTUID
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      LOGICAL CCPEXS
      EXTERNAL CCPEXS, LENSTR
C     ..
C     .. Data statements ..
      DATA MODES/'UNKNOWN','SCRATCH','OLD','NEW','READONLY'/
C     ..
C
      IF (ISTAT.LT.1 .OR. ISTAT.GT.5) THEN
        WRITE (ERRSTR,'(1X,A,I2)') ' (Q)QOPEN: bad mode: ',ISTAT
        CALL CCPERR(1,ERRSTR)
      END IF
C
C---- Test CCP4_OPEN for 'UNKNOWN' to switch mode 4 to 1
C
      JSTAT = ISTAT
      REWRIT = ' '
      IF (JSTAT.EQ.4) THEN
        CALL UGTENV('CCP4_OPEN',REWRIT)
        CALL CCPUPC(REWRIT)
        IF (REWRIT.EQ.'UNKNOWN') JSTAT = 1
      END IF
C
C---- Check Logical Names
C
      FNAME = ' '
      LNAME = LOGNAM
      LNONAM = .FALSE.
      IF (LNAME.EQ.' ') LNAME = 'diskio.dft'
      CALL UGTENV(LNAME,FNAME)
      IF (FNAME.EQ.'/dev/null') THEN
        JSTAT = 1
      ELSE IF (FNAME.EQ.' ') THEN
        IF (.NOT. CCPEXS(LNAME)) LNONAM = .TRUE.
        FNAME = LNAME
      END IF
      IF (REWRIT.EQ.'UNKNOWN') 
     +     CALL QPRINT(2, '(Q)QOPEN status changed from NEW to '
     +     //'UNKNOWN for '// LNAME)
      IF (JSTAT.EQ.4 .AND. CCPEXS(FNAME)) THEN
        ERRSTR = ' (Q)QOPEN NEW file already exists: '
        ERRSTR(LENSTR(ERRSTR)+2:) = FNAME
        CALL CCPERR(1,ERRSTR)
      ENDIF
C
C---- Open the file as requested
C
      CALL COPEN(IUNIT,FNAME,JSTAT)
C
C---- Error conditions
C
      IF (IUNIT.EQ.-1) THEN
        CALL CCPERR(1,' (Q)QOPEN failed - no streams left')
      ELSE IF (IUNIT.EQ.-2) THEN
        IF (LNONAM) THEN
          ERRSTR = '(Q)QOPEN Logical name '//LNAME
          ERRSTR(LENSTR(ERRSTR)+2:) = 'has no associated file name'
          CALL CCPERR(2,ERRSTR)
        END IF
        ERRSTR = ' (Q)QOPEN failed - File name: '
        ERRSTR(LENSTR(ERRSTR)+2:) = LOGNAM
        CALL CCPERR (-1,ERRSTR)
      END IF

      call ccp4h_summary_beg()
      CALL UGTUID(USRNAM)
      WRITE (ERRSTR,'(1X,A,I2)') '(Q)QOPEN allocated # ',IUNIT
      CALL QPRINT(1,ERRSTR)
      ERRSTR = 'User:   '//USRNAM//' Logical Name: '//LNAME
      CALL QPRINT(1,ERRSTR)
      ERRSTR = 'Status: '//MODES(JSTAT)//' Filename: '//FNAME
      CALL QPRINT(1,ERRSTR)
      call ccp4h_summary_end()
      END
C
C
C======================================================================
C
C_BEGIN_QPRINT
      SUBROUTINE QPRINT(IFLAG,MSG)
C     ============================
C
C     QPRINT - Conditionally print information.
C
C     Normally, MSG is printed iff IFLAG is not greater than the
C     `reference' level for messages.  This reference level is set to
C     the value of IFLAG on the first call (which won't print anything).
C     The first call is typically from CCPFYP.
C
C Usage:  CALL QPRINT   (IFLAG,MSG)
C         INTEGER       IFLAG
C         CHARACTER*(*) MSG
C
C Input:  IFLAG         debug level (0-9)
C         MSG           the output message itself
C
C Output: None.
C_END_QPRINT
C======================================================================
C
      INTEGER IFLAG, LEVEL
      CHARACTER MSG* (*)
      EXTERNAL LUNSTO, LENSTR
      INTEGER PFLAG, LUNSTO, LENSTR, LL, LX, LS
      SAVE PFLAG
      DATA PFLAG /-1/
C
      IF (PFLAG.EQ.-1) THEN
        PFLAG = IFLAG
        RETURN
      END IF
      IF (IFLAG.LE.PFLAG) THEN
        LL = LENSTR (MSG)
        IF (LL.GE.132) THEN
C         break lines longer than 132 characters for VMS
          LX = 1
          LS = 131
 10       CONTINUE
          WRITE (LUNSTO(1),'(1X, A)') MSG(LX:LS)
          IF (LS.EQ.LL) GOTO 20
          LX = LS  + 1
          LS = LS + 130
          IF (LS.GT.LL) LS = LL
          GO TO 10
        ELSE
          IF (LL.EQ.0) THEN
            WRITE(LUNSTO(1),'()')
          ELSE
            WRITE (LUNSTO(1),'(1X, A)') MSG(1:LL)
          END IF
        END IF
 20     CONTINUE
      END IF
      RETURN
      
      ENTRY QPRLVL (LEVEL)
C_BEGIN_QPRLVL
C     SUBROUTINE QPRLVL(LEVEL)
C
C     Returns the current `debug level' used by QPRINT in the integer
C     output variable LEVEL.
C_END_QPRLVL
      LEVEL = PFLAG
      END
C
C     ================================
      SUBROUTINE UGTENV(NAMENV,VALENV)
C     ================================
C
C UGTENV - Get value of env. variable
C
C Input:     NAMENV - Logical Name (trailing blanks are stripped)
C
C Output:    VALENV - Its value
C
C Arguments: CHARACTER*(*) NAMENV, VALENV
C
C Usage:     CALL UGTENV(NAMENV, VALENV)
C
C     .. Scalar Arguments ..
      CHARACTER NAMENV* (*),VALENV* (*)
C     ..
C     .. External Subroutines ..
C     don't declare getenv
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
      CALL GETENV(NAMENV(:LENSTR(NAMENV)),VALENV)
C
      END
	subroutine	ccp4h_summary_beg
	return
	end
C
C
C     ===============================
      SUBROUTINE UGERR(STATUS,ERRSTR)
C     ===============================
C
C UGERR - Get error message string for error number in STATUS
C     (supposedly).  Actually it ignores STATUS and always uses the
C     *last* error that occurred.
C
C Input:     STATUS - Error number (if negative print error message)
C
C Output:    ERRSTR - Error message string
C
C Arguments: INTEGER       STATUS
C            CHARACTER*(*) ERRSTR
C
C Usage:     CALL UGERR(STATUS, ERRSTR)
C
C     .. Scalar Arguments ..
      INTEGER STATUS
      CHARACTER ERRSTR* (*)
C     ..
C     .. Local Scalars ..
      LOGICAL IPRINT
C     ..
C     .. External Subroutines ..
CHEN>
C      INTEGER IERRNO, LUNSTO
C      EXTERNAL IERRNO, LUNSTO
CHEN<
C     ..
      IPRINT = .FALSE.
      IF (STATUS.LT.0) THEN
        IPRINT = .TRUE.
        STATUS = -STATUS
      END IF
C
C---- Get error message from system
C
CHEN>
C      IF (IERRNO().NE.0) THEN
C        CALL GERROR(ERRSTR)
C      ELSE
         ERRSTR = ' '
C      ENDIF
CHEN<
      IF (IPRINT) WRITE (LUNSTO(1),FMT=6000) 'UGERR',ERRSTR
C
 6000 FORMAT (' ',A,': ',A)
      END
C
C
C======================================================================
C
C_BEGIN_LENSTR
      INTEGER FUNCTION LENSTR(STRING)
C     ===============================
C
C---- Returns significant string length excluding trailing spaces
C
C Arguments:
C ==========
C
C  STRING (I)   CHARACTER*(*): Input string
C_END_LENSTR
C
C     .. Scalar Arguments ..
      CHARACTER STRING* (*)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC LEN
C     ..
      LENSTR = LEN(STRING)
 10   CONTINUE
      IF (LENSTR.NE.0) THEN
        IF(STRING(LENSTR:LENSTR).EQ.' ' .OR.
     .       ICHAR(STRING(LENSTR:LENSTR)).EQ.0) THEN
          LENSTR = LENSTR - 1
          GO TO 10
        END IF
      END IF
C
      END
C
C
C     =========================
      LOGICAL FUNCTION VAXVMS()
C     =========================
C
C VAXVMS - Operating Sytem in use returns .TRUE. if VAXVMS
C
C Input:     none
C
C Returns:   .TRUE. for VAXVMS, .FALSE. otherwise
C
C Arguments: none
C
C Usage:     VAXVMS ()
C
      VAXVMS = .FALSE.
C
      END
C
C
C     =====================
      SUBROUTINE GETELAPSED
C     =====================
C
      EXTERNAL LUNSTO, USTIME
      INTEGER LUNSTO
      REAL TARRAY(2), JUNK
      INTEGER ELAPS, START
      LOGICAL INITED
      SAVE START, INITED
      DATA INITED /.FALSE./
C     
      JUNK = ETIME(TARRAY)
      CALL USTIME(ELAPS)
      ELAPS = ELAPS - START
C     don't print anything if it hasn't been initialised (by CCPFYP)
      IF (INITED) WRITE(LUNSTO(1),6000) TARRAY(1), TARRAY(2), 
     +     ELAPS/60, MOD(ELAPS, 60)
 6000 FORMAT(' Times: User: ', F9.1, 's System: ', F6.1, 's Elapsed:',
     +     I5 , ':',I2.2)
C     
      ENTRY INITFYP
      CALL USTIME(START)
      INITED = .TRUE.
C     Machine-dependent startup, e.g. set FPE on SunOS

      END
	subroutine	ccp4h_pre_end
	return
	end
	subroutine	ccp4h_html_close
	return
	end
C
      SUBROUTINE CEXIT (ICODE)
C     trivial interface to system-dependent EXIT routine
      INTEGER ICODE
      CALL EXIT (ICODE)
      END
C
C
C
C_BEGIN_CCPEXS
      LOGICAL FUNCTION CCPEXS(NAME)
C     =============================
C
C---- Tests if file assigned to logical name NAME exists
C
C Returns CCPEXS  .true.  if file exists
C                 .false. if file does not exist
C
C Arguments:
C ==========
C
C    NAME (I)   CHARACTER*(*): Logical name
C_END_CCPEXS
C
C     .. Scalar Arguments ..
      CHARACTER NAME* (*)
C     ..
C     .. Local Scalars ..
      CHARACTER*255 NAMFIL
C     ..
      NAMFIL = ' '
      CALL UGTENV(NAME,NAMFIL)
      IF (NAMFIL.EQ.' ') NAMFIL = NAME
C
      INQUIRE (FILE=NAMFIL,EXIST=CCPEXS)
C
      END
C
C
C FUNCTION 'LUNSTO'
C =================
C
C_BEGIN_LUNSTO
      FUNCTION LUNSTO(IDUM)
C     =====================
C
C Returns the fortran standard output unit number
C
C Arguments:
C ==========
C
C       IDUM (I)   Dummy argument
C_END_LUNSTO
C
      LUNSTO = 6
      END
	subroutine	ccp4h_summary_end
	return
	end
C
C
C     =========================
      SUBROUTINE UGTUID(USRNAM)
C     =========================
C
C UGTUID - Get user ID
C
C Input:     none
C
C Output:    UID - user ID string
C
C Arguments: CHARACTER*(*) UID
C
C Usage:     CALL UGTUID(UID)
C
C     .. Scalar Arguments ..
      CHARACTER USRNAM* (*)
C     ..
C     .. External Subroutines ..
C     don't declare getenv
C     ..
      CALL GETENV('USER',USRNAM)
      IF (USRNAM.EQ.' ') CALL GETENV('LOGNAME',USRNAM)
C
      END
C
C
C     =======================
      SUBROUTINE USTIME(ISEC)
C     =======================
C
C USTIME - Get absolute time in seconds.
C          Convex uses STIME (), others seem to use TIME ().
C
C Input:     none
C
C Output:    SEC
C
C Arguments: INTEGER SEC
C
C Usage:     CALL USTIME(SEC)
C
      INTEGER ISEC
C
      INTEGER TIME
C
      ISEC = TIME()
C
      END
C
C
C
C_BEGIN_CCPVRS
      SUBROUTINE CCPVRS(ILP,PROG,VDATE)
C     =================================
C
C---- Print program name and date of current version (also prints run
C     date if available)
C
C Arguments:
C ==========
C
C         ILP (I)   INTEGER: UNIT NUMBER FOR PRINTER OUTPUT
C        PROG (I)   CHARACTER*(*): VARIABLE HOLDING PROGRAM NAME (MAX
C                   OF 10 CHARACTERS)
C       VDATE (I)   CHARACTER*(*): VARIABLE HOLDING DATE OF THE CURRENT
C                   VERSION AS DD/MM/YY
C_END_CCPVRS
C     .. Scalar Arguments ..

      INTEGER ILP
      CHARACTER PROG* (*),VDATE* (*), PNM*(*)
C     ..
C     .. Local Scalars ..
      CHARACTER CTIME*8,DT2*8,DT*10,PR*20,UID*20,TMPPRG*20,VERSION*10
      SAVE PR
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      CHARACTER FROOT*20
      EXTERNAL LENSTR, FROOT
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPDAT,UGTUID,UTIME,ccp4h_summary_beg,ccp4h_summary_end
C     ..
      DATA PR /' '/
C
C---- Output heading
C
      call ccp4h_summary_beg()
      call ccp4h_pre_beg()
      PR = PROG
      DT = VDATE
      CALL CCPDAT(DT2)
      CALL UGTUID(UID)
      CALL UTIME(CTIME)
      CALL CCP4_VERSION(VERSION)
      WRITE (ILP,FMT=6000) PR,VERSION(1:LENSTR(VERSION)),
     +  DT,UID(1:LENSTR(UID)),DT2,CTIME
 6000 FORMAT (/,/,/,/,
     + '1##########################################################',/,
     + ' ##########################################################',/,
     + ' ##########################################################',/,
     + ' ### CCP PROGRAM SUITE: ',A10,2X,'VERSION ',A,': ',A8,'##',/,
     + ' ##########################################################',/,
     + ' User: ',A,'  Run date: ',A8,'  Run time:',A,
     + /,/,/,
     + ' Please reference: Collaborative Computational Project,',
     + ' Number 4. 1994.',/,' "The CCP4 Suite: Programs for Protein',
     + ' Crystallography". Acta Cryst. D50, 760-763.',/,/,
     + ' as well as any specific reference in the program write-up.',
     + /,/)
      call ccp4h_summary_end()
C
      RETURN
C
      ENTRY CCPPNM (PNM)
C_BEGIN_CCPPNM
C     SUBROUTINE CCPPNM (PNM)
C     =======================
C
C     Returns the program name previously set by CCPVRS (/CCPRCS); if
C     that isn't set, use arg(0).
C
C     Argument:
C         PNM (O)   CHARACTER*(*)  Program name
C_END_CCPPNM
      IF (PR.EQ.' ') THEN
        CALL UGTARG(0,TMPPRG)
        PR = FROOT(TMPPRG)
      END IF
      PNM = PR
      END
	subroutine	ccp4h_pre_beg
	return
	end
C
C
C
C_BEGIN_CCPDAT
      SUBROUTINE CCPDAT(CALDAT)
C     =========================
C
C---- This subroutine returns the date if available
C
C Arguments:
C ==========
C
C      CALDAT (O)   CHARACTER*8: DATE AS DD/MM/YY
C                   (RETURNED AS A BLANK STRING IF NOT AVAILABLE)
C_END_CCPDAT
C
C SPECIFICATION STATEMENTS
C ------------------------
C
C---- Get date
C
C     .. Scalar Arguments ..
      CHARACTER CALDAT*8
C     ..
C     .. Local Scalars ..
      INTEGER ID,IM,IY
C     ..
C     .. External Subroutines ..
      EXTERNAL UIDATE
C     ..
C
      CALL UIDATE(IM,ID,IY)
      IY = MOD (IY, 100)      
      WRITE (CALDAT,FMT=6000) ID,IM,IY
C
C---- Format statements
C
 6000 FORMAT (I2,'/',I2,'/',I2)
C
      IF (CALDAT(7:7) .EQ. ' ') CALDAT(7:7) = '0'
      END
C
C
C     =======================
      SUBROUTINE UTIME(CTIME)
C     =======================
C
C UTIME - Get current time hh:mm:ss
C
C Input:     none
C
C Output:    TIME - as ASCII string
C
C Arguments: CHARACTER*(*) CTIME
C
C Usage:     CALL UTIME(CTIME)
C
C     .. Scalar Arguments ..
      CHARACTER CTIME* (*)
C     ..
C     .. Local Arrays ..
      INTEGER IARRAY(3)
C     ..
C      CALL ITIME(IARRAY)
      WRITE (CTIME,FMT=6000) IARRAY(1),IARRAY(2),IARRAY(3)
 6000 FORMAT (I2,2 (':',I2.2))
      END
C
C
C
C_BEGIN_CCP4_VERSION
      SUBROUTINE CCP4_VERSION(VERSION)
C     =================================
C
C---- Return current CCP4 version as string
C
C Arguments:
C ==========
C
C       VERSION (O)   CHARACTER*(*): current version of CCP4 suite
C_END_CCP4_VERSION
C
C     .. Scalar Arguments ..
      CHARACTER*(*) VERSION

      VERSION = '4.0'

      END
C
      SUBROUTINE UGTARG(I, ARG)
      INTEGER I
      CHARACTER *(*) ARG
      CALL GETARG(I, ARG)
      END
C
C
C     ====================================
C_BEGIN_FROOT
      FUNCTION FROOT(FILNAM)
      CHARACTER*(*) FROOT
C     ====================================
C
C---- Returns a file name minus an extension.
C
C Arguments:
C
C  FILNAM (I)   CHARACTER*(*): File name
C_END_FROOT
C
      CHARACTER FILNAM* (*)
      CHARACTER*1 PATH, TYPE, VERS
      EXTERNAL CCPPSF
C
      CALL CCPPSF(FILNAM, PATH, FROOT, TYPE, VERS)
      END
C
C
C     ====================================
      SUBROUTINE UIDATE(IMONTH,IDAY,IYEAR)
C     ====================================
C
C UIDATE - Get date in 3 integer . Alliant uses INTEGER*4
C          and order is IDAY,IMONTH,IYEAR
C
C Input:     none
C
C Output:    MONTH,DAY,YEAR
C
C Arguments: INTEGER MONTH, DAY, YEAR
C
C Usage:     CALL UIDATE(MONTH, DAY, YEAR)
C
      IMPLICIT NONE
      INTEGER DTVAL(8),I,IMONTH,IDAY,IYEAR
      CHARACTER CDATE*8,CTIME*10,CZONE*5,CDAT*24,CTIM*20
      CHARACTER*3 MONTH(12)
      CHARACTER*2 NMON(13)
      DATA MONTH/'Jan','Feb','Mar','Apr','May','Jun',
     +           'Jul','Aug','Sep','Oct','Nov','Dec'/
      DATA NMON/'01','02','03','04','05','06',
     +          '07','08','09','10','11','12','??'/
C**************************************************************************
C	Provides the date and time.
C       Calls FDATE
C**************************************************************************
      CALL FDATE(CDAT)
      DO 10 I=1,12
        IF (MONTH(I).EQ.CDAT(5:7)) GOTO 20
10    CONTINUE
20    CONTINUE
      CDATE(1:8)=CDAT(21:24)//NMON(I)(1:2)//CDAT(9:10)
      CTIME(1:10)=CDAT(12:13)//CDAT(15:16)//CDAT(18:19)//'.???'
      CZONE(1:5)='?????'
      DTVAL(2)=0
      IF (I.LT.13) DTVAL(2)=I
      DTVAL(4)=0
      DTVAL(8)=0
      READ(CDAT,1000) DTVAL(3),DTVAL(5),DTVAL(6),DTVAL(7),DTVAL(1)
1000  FORMAT(8X,I2,1X,I2,1X,I2,1X,I2,1X,I4)
      IMONTH=DTVAL(2)
      IDAY=DTVAL(3)
      IYEAR=DTVAL(1)
      print *,cdat
      RETURN
      END
C
C
C
C SUBROUTINE 'CCPPSF'
C ===================
C
C_BEGIN_CCPPSF
      SUBROUTINE CCPPSF(FILNAM,PATH,NAME,TYPE,VERS)
C     =============================================
C
C PARSE FILE NAME INTO COMPONENTS
C
C NOTE: THE ROUTINE  CONTAINS MACHINE DEPENDENT CODE
C
C
C Arguments:
C ==========
C
C      FILNAM (I)   CHARACTER*(*): FILE NAME STRING (NO EMBEDDED BLANKS ASSUMED)
C
C        PATH (O)   CHARACTER*(*): STRING RETURNING PATH OR, FOR VAX VMS,
C                   THE PART OF THE FILE SPECIFICATION UP TO THE
C                   END OF THE DIRECTORY SPECIFICATION (BLANK IF NONE)
C                   (INCLUDES TERMINATING ] or : or /)
C
C        NAME (O)   CHARACTER*(*): STRING RETURNING NAME.  (BLANK IF NONE)
C
C        TYPE (O)   CHARACTER*(*): STRING RETURNING FILE TYPE/EXTENSION
C                   (BLANK IF NONE)
C
C        VERS (O)   CHARACTER*(*): STRING RETURNING THE VERSION.
C                   (BLANK IF NONE)
C
C AFTER REMOVAL OF THE PATH PART OF THE STRING, IF PRESENT, THE VERSION ON
C A VAX IS TAKEN AS ANY TEXT FOLLOWING A SEMICOLON IN THE STRING OR, IF NO
C SEMICOLON IS PRESENT, ANY TEXT FOLLOWING THE LAST DOT IN THE STRING
C PROVIDED THAT AT LEAST TWO DOTS ARE PRESENT. ON A UNIX SYSTEM THE VERSION
C WILL ALWAYS BE RETURNED AS A BLANK.
C
C AFTER THE REMOVAL OF THE PATH AND VERSION PARTS OF THE STRING THEN, IF
C THERE IS AT LEAST ONE DOT, THE NAME IS THE STRING UP TO THE LAST DOT
C REMAINING AND THE TYPE IS THE PART OF THE STRING AFTER THE DOT. IF
C NO DOT IS PRESENT THEN THE REMAINING STRING IS THE NAME AND THE TYPE
C IS BLANK.
C_END_CCPPSF
C
C SPECIFICATION STATEMENTS
C ------------------------
C
      CHARACTER*(*) FILNAM,PATH,NAME,TYPE,VERS
      EXTERNAL VAXVMS, WINMVS, RTNBKS
      LOGICAL VAXVMS, WINMVS, VMS, MVS
      CHARACTER RTNBKS*1, BKS*1
C
C INITIALISATIONS
C ---------------
C
      PATH=' '
      NAME=' '
      TYPE=' '
      VERS=' '
      LMAX=LENSTR(FILNAM)
      IF (LMAX.EQ.0) RETURN
      LMIN=0
      VMS = VAXVMS()
      MVS = WINMVS()
      BKS = RTNBKS()      
10    LMIN=LMIN+1
      IF (FILNAM(LMIN:LMIN).EQ.' ') GO TO 10
C
C GET PATH
C --------
C
      IF (VMS) THEN
        DO 20 L=LMAX,LMIN,-1
          IF (FILNAM(L:L).EQ.':'.OR.FILNAM(L:L).EQ.']') GO TO 30
 20     CONTINUE
      ELSEIF (MVS) THEN
        DO 21 L=LMAX,LMIN,-1
          IF (FILNAM(L:L).EQ.BKS)GO TO 30
 21     CONTINUE
      ELSE
        DO 22 L=LMAX,LMIN,-1
          IF (FILNAM(L:L).EQ.'/')GO TO 30
 22     CONTINUE
      ENDIF
      GO TO 40
30    PATH=FILNAM(LMIN:L)
      LMIN=L+1
      IF (LMIN.GT.LMAX) RETURN
C
C GET VERSION IF PRESENT
C ----------------------
C
 40   CONTINUE
      IF (VMS) THEN
        LSC=INDEX(FILNAM(LMIN:LMAX),';')
        IF (LSC.GT.0) THEN
          LSC=LSC+LMIN-1
          IF (LSC.LT.LMAX) VERS=FILNAM(LSC+1:LMAX)
          LMAX=LSC-1
        ELSE
          LDOT=0
          NDOT=0
          DO 50 L=LMAX,LMIN,-1
            IF (FILNAM(L:L).EQ.'.') THEN
              NDOT=NDOT+1
              IF (LDOT.EQ.0) LDOT=L
            ENDIF
 50       CONTINUE
          IF (NDOT.GT.1) THEN
            IF (LDOT.LT.LMAX) VERS=FILNAM(LDOT+1:LMAX)
            LMAX=LDOT-1
          ENDIF
        ENDIF
      ENDIF
C
C GET NAME AND TYPE
C -----------------
C
      IF (LMAX.LT.LMIN) RETURN
      LDOT=0
      DO 60 L=LMAX,LMIN,-1
      IF (FILNAM(L:L).EQ.'.') THEN
         LDOT=L
         GO TO 70
      ENDIF
60    CONTINUE
70    IF (LDOT.EQ.0) THEN
         NAME=FILNAM(LMIN:LMAX)
         RETURN
      ELSE
         IF (LDOT.GT.LMIN) NAME=FILNAM(LMIN:LDOT-1)
         IF (LDOT.LT.LMAX) TYPE=FILNAM(LDOT+1:LMAX)
      ENDIF
      END
C
C
C     =========================
      LOGICAL FUNCTION WINMVS()
C     =========================
C
C WINMVS - Windows mircrosoft Visual Studio
C
C Input:     none
C
C Returns:   .TRUE. for WINMVS, .FALSE. otherwise
C
C Arguments: none
C
C Usage:     WINMVS ()
C
      WINMVS = .FALSE.
C
      END

C
CA dummy function for unix
C     =========================
       CHARACTER FUNCTION RTNBKS()
C     =========================
C
C RTNBKS - Returns a Backslash for nt as unix compilers are fussy!
C
C Input:     none
C
C Returns:   \ if WIN32 or not if unix or vms
C
C Arguments: none
C
C Usage:     RTNBKS ()
C
      RTNBKS=' '
C
      END
