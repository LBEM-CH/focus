C*IMSUBS2000.FOR*********************************************************
C                                                                       *
C       HIGHER-LEVEL FORTRAN SUBROUTINES FOR READING/WRITING            *
C       IMAGE FILES                                                     *
C                                                                       *
C       Last Update:    23.May.84       DAA             VAX             *
C       Bug fixed in IWRHDR in DO loop 100  JMS                         *
C       IALUVW,IRTUVW added by KAT 12.09.85                             *
C       Bug fixed in IALSYM by KAT 2.10.85                              *
C       IRTSYM,IRTSAM added by JMS 11.10.85                             *
C       Brought to the Alliant by RAC and RB 11.9.91                    *
C       Format statement in IMOPEN changed to I10 by RH 6.1.92          *
C       Mod to IRDHDR to position map format maps correctly JMS 10.6.94 *
C       QWRITE added to IWRHDR if IALSYM spacegroup set=1 RH 25.7.99    *
C       major rewrite to add machine stamp to header and move origin    *
C                               JMS     29.11.2000                      *
C       Mods to error statements to account for architecture faults     *
C                               JMS     11.01.01                        *
C       Bug fix to repositioning after IRDPAS, IWRPAS jms 12.06.01      *
C       error trap installed for file stream overflow jms 16.08.04      *
C         file stream numbers are created in library.c and appear       *
C         to accumulate, therefore you cannot open/close > 5 files      *
C                                                                       *
C************************************************************************
C                                                                       *
C       HEADER FORMAT                                                   *
C       1       NX      number of columns (fastest changing in map)     *
C       2       NY      number of rows                                  *
C       3       NZ      number of sections (slowest changing in map)    *
C       4       MODE    data type :                                     *
C                       0       image : signed 8-bit bytes range -128   *
C                                       to 127                          *
C                       1       image : 16-bit halfwords                *
C                       2       image : 32-bit reals                    *
C                       3       transform : complex 16-bit integers     *
C                       4       transform : complex 32-bit reals        *
C       5       NXSTART number of first column in map                   *
C       6       NYSTART number of first row in map                      *
C       7       NZSTART number of first section in map                  *
C       8       MX      number of intervals along X                     *
C       9       MY      number of intervals along Y                     *
C       10      MZ      number of intervals along Z                     *
C       11-13   CELLA   cell dimensions in angstroms                    *
C       14-16   CELLB   cell angles in degrees                          *
C       17      MAPC    axis corresp to cols (1,2,3 for X,Y,Z)          *
C       18      MAPR    axis corresp to rows (1,2,3 for X,Y,Z)          *
C       19      MAPS    axis corresp to sections (1,2,3 for X,Y,Z)      *
C       20      DMIN    minimum density value                           *
C       21      DMAX    maximum density value                           *
C       22      DMEAN   mean density value                              *
C       23      ISPG    space group number 0 or 1 (default=0)           *
C       24      NSYMBT  number of bytes used for symmetry data (0 or 80)*
C       25-49   EXTRA   extra space used for anything                   *
C       50-52   ORIGIN  origin in X,Y,Z used for transforms             *
C       53      MAP     character string 'MAP ' to identify file type   *
C       54      MACHST  machine stamp                                   *
C       55      RMS     rms deviation of map from mean density          *
C       56      NLABL   number of labels being used                     *
C       57-256  LABEL(20,10) 10 80-character text labels                *
C                                                                       *
C************************************************************************
C***
C***    IMOPEN
C***
C************************************************************************
C       Open file NAME with qualities ATBUTE ('RO' 'OLD' 'NEW' SCRATCH')
C       and associate with stream ISTREAM 
C       (ISTREAM = # between 1 & 12 ; MAX of  5 files opened at any time!!)
C************************************************************************
        subroutine imopen(istream,name,atbute)
        character*(*)   name
        character*(*)   atbute
        character*7     at2
        character*256   fullname
        character*4     mapname
        integer         imapname
        EQUIVALENCE (imapname,mapname)
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
1000      format(//' IMOPEN: Invalid STREAM number!!!'//)
          stop 'Open Error !!!'
        end if
        numopen = numopen + 1
        if (numopen .gt. max_units) then
          write(6,1100)
1100      format(//' IMOPEN: No More than 5 files can be opened!!!'//)
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
C***    Old style, different-architecture map cannot be read or overwritten
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
         write(6,'(/
     *   ''!!! Warning - file : '',a,'' has UNKNOWN status !!!''/)')
     *   (name(1:numchars(name)))
         go to 1500
        end if
        call qmode(j,0,nchhdr)
        call qseek(j,1,52*4+1,1024)
        call qreadi(j,imapname,4,ier)
        call qseek(j,1,16*4+1,1024)
        call qmode(j,6,nchhdr)
        call qreadi(j,mapcrs(1,j),3,ier)
        do i=1,3
         if(mapcrs(i,j) .eq. 1) go to 1200
        end do
C*** If architecture same set karch flag = 0, else architecture flag = 1
        karch(j) = 1
        write(6,'(/
     *  ''!!! Warning - architecture incompatibility !!! '')') 
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
         write(6,'(
     *   ''!!! Fatal error - cannot read from or write to file : '',a)') 
     *   (name(1:numchars(name)))
         write(6,'(
     *   ''!!! Use byte_swap_map followed by image_convert to reformat''
     *   '' maps correctly !!!'')')
         stop
        else
         write(6,'(/,'' New style 21st century map : '',a)') 
     *   (name(1:numchars(name)))
        end if
C*************************************************
C*** tests over, read machine stamp
C*************************************************
        call qrarch(j,53,ireslt)
        print *,"Machine stamp: ",ireslt
        if(ireslt .eq. 0) then
         write(6,'(/''!!! Warning - no machine stamp in file : '',a)') 
     *   (name(1:numchars(name)))
        end if
C***************************************************************************
C***   Setting mode type to byte for compatibility with previous VMS version
C***************************************************************************
 1500   call qmode(lstream(istream),0,nchitm)
C*************************************************
C***     Get and print file name
C*************************************************
        call qqinq(j,name,fullname,nfilsz)
        if (at2 .eq. 'NEW' .or. at2 .eq. 'SCRATCH' .or.
     *      at2 .eq. 'UNKNOWN') then
      print *,'Filename for ', at2,' image file on unit', istream, ' : '
     .    ,fullname
        else
          write(6,2100) at2,istream,fullname,nfilsz
        endif
2100    format(/' Filename for ',a,' image file on unit',i4,' : ',a
     . '                             Size= ',i10,/)
        return
C************************************************************************
C***
C***    IMFORM
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
C***    IMCLOSE
C***
C************************************************************************
C       Close file on stream ISTREAM. This frees up this channel for
C       further use.
C************************************************************************
        entry imclose(istream)
        call qclose(lstream(istream))
        numopen = numopen - 1
        if (numopen .lt. 0) numopen = 0
        return
        end
C************************************************************************
C***
C***    IMUNIT
C***
C************************************************************************
C***    Returns the lower-level compatible unit number (Function call)
C************************************************************************
        function imunit(istream)
C***
        include 'imsubs_common.for'
C***
        imunit = lstream(istream)
        return
        end
C************************************************************************
C***
C***    IRDHDR
C***
C************************************************************************
C       Read the header on unit ISTREAM, Print the contents,
C       and return some parameters to the caller:
C
C       inxyz             : size of file Columns, Rows, Sections
C       mxyz              : # of intervals Columns, Rows, Sections
C       imode             : Data storage mode (1-4)
C                               0 = Image               Integer*1
C                               1 = Image               Integer*2
C                               2 = Image               Reals
C                               3 = Fourier Transform   Integer*2
C                               4 = Fourier Transform   Reals
C       dmin,dmax,dmean   : Min, Max, & Mean densities
C
        subroutine irdhdr(istream,inxyz,mxyz,imode,dmin,dmax,dmean)
C***
        integer*4 inxyz(3)
        integer*4 iuvw(3)
        integer*4 labels(1)
        CHARACTER*16 lxyz(3)
        integer*4 mxyz(3)
        integer*4 nxyzst(3)
C***
        real*4 title(1)
        real*4 cell(6)
        real*4 extra(1)
C***
        character*80 spgsym1
C***
        include 'imsubs_common.for'
C***
        logical cflag
C*******************************************************
C*** Initialize values
C*******************************************************
        data lxyz/'X','Y','Z'/
        data ier /0/
        spgsym1 = ' X,Y,Z'
C*************************************************
C***     Read header
C*************************************************
        j = lstream(istream)
        if(unknown(j)) then
         write(6,'(
     *   ''!!! Fatal error - attempted UNKNOWN status file read !!!'')')
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
        call qreadr(j,stuff(1,j),25,ier)
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
        write(6,1000) inxyz,imode,(ncrst(k,j),k=1,3),mxyz,
     . (cel(k,j),k=1,6),(lxyz(mapcrs(k,j)),k=1,3),
     . dmin,dmax,dmean,(origin(k,j),k=1,3),rms(j),nspg(j),nbsym(j),
     .  nlab(j),((labls(i,k,j),i=1,20),k=1,nlab(j))
C***
1000    format(/
     .  7X,'Number of columns, rows, sections ........ ',3I6/
     .  7X,'Map mode ................................. ',I6/
     .  7X,'Start points on columns, rows, sections .. ',3I6/
     .  7X,'Grid sampling on x, y, z ................. ',3I6/
     .  7X,'Cell axes ................................ ',3F10.2/
     .  7X,'Cell angles .............................. ',3F10.2/
     .  7X,'Fast, medium, slow axes .................. ',3(5X,A1)/
     .  7X,'Minimum density .......................... ',F25.12/
     .  7X,'Maximum density .......................... ',F25.12/
     .  7X,'Mean density ............................. ',F25.12/
     .  7X,'Origin ................................... ',3F10.2/
     .  7X,'RMS deviation ............................ ',F25.12/
     . 7X,'Space group, # bytes symmetry ............ ',2I6/
     .  7X,'Number of titles ......................... ',I6//
     . ' Titles :'/10(1X,20A4/))
        return
C***********************************************************************
C***
C***    IWRHDR
C***
C***********************************************************************
C       Write out Header record to OUTPUT file. This is the ONLY
C       routine that writes the header.
C
C       title is a single 80 character title.
C       ntflag controls title writing.
C       -1 = No title is added to set
C        0 = This is only title
C        1 = Add this title at end
C        2 = This is first title, push others down.
C***********************************************************************
        entry iwrhdr(istream,title,ntflag,dmin,dmax,dmean)
C***
        j = lstream(istream)
        if(old(j)) then
         write(6,'(
     *   ''!!! Error - cannot overwrite old-style 20th century map.''/
     *   ''    Use image_convert program and retry.'')')
         stop
        else if(karch(j) .ne. 0) then
         write(6,'(
     *   ''!!! Error - cannot overwrite map of different machine ''//
     *   ''architecture - use image_convert program and retry.'')')
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
10      nlab(j) = 1
        call ccpmvir(labls(1,1,j),title,20)
        goto 20
11      nlab(j) = min(10,nlab(j)+1)
        call ccpmvir(labls(1,nlab(j),j),title,20)
        goto 20
12      k = min(9,nlab(j))
        do 100 i = k,1,-1
          call ccpmvi(labls(1,i+1,j),labls(1,i,j),20)
100     continue
        call ccpmvir(labls(1,1,j),title,20)
        nlab(j) = k + 1
C**************************************************************
C*** write header
C**************************************************************
20      call qseek(j,1,1,1)
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
        call qwritr(j,stuff(1,j),25)
        call qmode(j,2,nchhdr)
        call qwritr(j,origin(1,j),3)
        call qmode(j,2,nchhdr)
        call qwritr(j,stamp(1,j),2)
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
         call qwritc(j,spgsym1,80)
        else if(nspg(j) .ne. 0) then
         stop 'Error - IMSUBS spacegroup must be 0 or 1'
        endif
C*** move to start of data
        call qmode(j,0,nchhdr)
        call qseek(j,2,1,nbhdr+nbsym(j))
        return
C*********************************************************************
C***
C***    ITRHDR(ISTREAM,JSTREAM)
C***
C*********************************************************************
C       Transfer header information from JSTREAM to ISTREAM
C       Note: Information is internally transfered, NOT actually written
C       to file!!!
C       no symmetry operators are written to output file
C       ispg and nbsym are set to 0 in new header!!!!
C       unless explicitly set by a call to IALSYM
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
        call ccpmvr(cel(1,j),cel(1,k),6)
        call ccpmvi(mapcrs(1,j),mapcrs(1,k),3)
        call ccpmvr(denmmm(1,j),denmmm(1,k),3)
        call ccpmvr(stuff(1,j),stuff(1,k),25)
        call ccpmvr(origin(1,j),origin(1,k),3)
        call ccpmvr(stamp(1,j),stamp(1,k),2)
C*** reset space group and number of bytes symmetry data to 0
        nspg(j) = 0
        nbsym(j) = 0
        rms(j) = rms(k)
        nlab(j) = nlab(k)
        call ccpmvi(labls(1,1,j),labls(1,1,k),200)
        return
C*********************************************************************
C***
C***    ITRLAB(ISTREAM,JSTREAM)
C***
C*********************************************************************
C       Transfer LABELS from JSTREAM to ISTREAM
C       Note: Information is internally transfered, NOT actually written
C       to file!!!
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
C***    ITRCEL(ISTREAM,JSTREAM)
C***
C*********************************************************************
C       Transfer CELL parameters from JSTREAM to ISTREAM
C       Note: Information is internally transfered, NOT actually written
C       to file!!!
C*********************************************************************
        entry itrcel(istream,jstream)
C***
        j = lstream(istream)
        k = lstream(jstream)
        call ccpmvr(cel(1,j),cel(1,k),6)
        return
C*********************************************************************
C**
C***    ICRHDR
C***
C*********************************************************************
C       Create new header. All of the standard image defaults are
C       set up given the requested information. Header NOT written!!
C NOTE: The starting point for Columns,Rows,Sections
C       are set to 0 by default.!!!!!!!!!
C
C       INXYZ             : size of file Columns, Rows, Sections
C       MXYZ              : # of intervals Columns, Rows, Sections
C       IMODE             : Data storage mode (1-4)
C                               0 = Image               Integer*1
C                               1 = Image               Integer*2
C                               2 = Image               Reals
C                               3 = Fourier Transform   Integer*2
C                               4 = Fourier Transform   Reals
C       LABELS(20,N)      :N=1,10 Up to 10 80 character labels
C       NL                :Actual # of labels to use (0 is O.K.)
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
200     continue
        call ccpzi(stuff(1,j),25)
        call ccfill(labls(1,1,j),' ',nbl)
        if (ml .gt. 0) call ccpmvi(labls(1,1,j),labels,ml*20)
        return
C*********************************************************************
C***
C***    IALCEL
C***
C*********************************************************************
C       Alter CELL information for file. Header NOT actually written!!!
C
C       CELL(6)           : new unit cell parameters (a,b,c, alpha,beta,gamma)
C*********************************************************************
        entry ialcel(istream,cell)
C***
        j = lstream(istream)
        call ccpmvr(cel(1,j),cell,6)
        return
C*********************************************************************
C***
C***    IALCON
C***
C*********************************************************************
C       Alter data Conversion mode. USE WITH GREAT CARE!!!!!!!!!!
C       By default, all data is passed to the user or received from
C       the user as REALS or COMPLEX REALS, independent of storage mode
C       on the disk.
C       This routine allows the direct transmission of data to and from
C       the disk WITHOUT ANY FORMAT conversion.
C
C       CFLAG   =        .TRUE.  for conversion (default value)
C       CFLAG   =        .FALSE. for NO conversion
C
C*********************************************************************
        entry ialcon(istream,cflag)
C***
        nocon(lstream(istream)) = .not.cflag
        return
C*********************************************************************
C***
C***    IALEXT
C***
C*********************************************************************
C       Alter EXTRA information stored in "unused" poritions of
C       file header (25 words max!!). Header NOT actually written!!!
C
C       EXTRA             : Buffer containing data to be placed in extra slot.
C       ISTART            : Which "extra" element to start at (1-25)
C       NEXTRA            : Number of words to transfer
C*********************************************************************
        entry ialext(istream,extra,istart,nextra)
C***
        j = lstream(istream)
        if(istart+nextra .gt. 32 .or. istart .lt. 1)
     .    stop 'IALEXT: Attempt to write out of bounds'         
        call ccpmvr(stuff(istart,j),extra,nextra)
        return
C*********************************************************************
C***
C***    IALLAB
C***
C*********************************************************************
C       Alters label information. All spaces beyond NL are set = blanks
C
C       LABELS(20,10)     : space for labels
C       NL                : actual # of labels being used
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
C***    IALMOD
C***
C*********************************************************************
C       Alter MODE in hedaer. Header NOT actually written!!!
C       IMODE             : Data storage mode (1-4)
C                               0 = Image               Integer*1
C                               1 = Image               Integer*2
C                               2 = Image               Reals
C                               3 = Fourier Transform   Integer*2
C                               4 = Fourier Transform   Reals
C*********************************************************************
        entry ialmod(istream,imode)
        mode(lstream(istream)) = imode
        return
C*********************************************************************
C***
C****   IALORG
C***
C*********************************************************************
C       Alter ORIGIN information
C
C       XORIG,YORIG,ZORIG         : X,Y,Z origin information
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
C***    ialrms
C***
C*********************************************************************
C       Alter RMS information
C
C       rmsdev    : rms deviation
C*********************************************************************
        entry ialrms(istream,rmsdev)
C***
        j = lstream(istream)
        rms(j) = rmsdev
        return
C*********************************************************************
C***
C***    IALSAM
C***
C*********************************************************************
C       Alter SAMPLING information for file. Header NOT actually written!!!
C
C       MXYZ              : # of intervals Columns, Rows, Sections
C*********************************************************************
        entry ialsam(istream,mxyz)
C***
        j = lstream(istream)
        call ccpmvi(nxyz(1,j),mxyz,3)
        return
C*********************************************************************
C***
C***    IALSIZ
C***
C*********************************************************************
C       Alter SIZE information for file. Header NOT actually written!!!
C
C       INXYZ             : size of file Columns, Rows, Sections
C       NXYZST            : starting # for Columns, Rows, Sections (usually 1) 
C*********************************************************************
        entry ialsiz(istream,inxyz,nxyzst)
C***
        j = lstream(istream)
        call ccpmvi(ncrs(1,j),inxyz,3)
        call ccpmvi(ncrst(1,j),nxyzst,3)
        return
C*********************************************************************
C***
C***    IALSYM
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
C***    IALUVW
C***
C*********************************************************************
C       Alters the matrix to permute cell dimensions 
C*********************************************************************
        entry ialuvw(istream,iuvw)
C***
        j = lstream(istream)
        call ccpmvi(mapcrs(1,j),iuvw,3)
        return
C*********************************************************************
C***
C***    IRTCEL
C***
C*********************************************************************
C       Return CELL information from file. 
C
C       CELL(6)           : unit cell parameters (a,b,c, alpha,beta,gamma)
C*********************************************************************
        entry irtcel(istream,cell)
C***
        j = lstream(istream)
        call ccpmvr(cell,cel(1,j),6)
        return
C*********************************************************************
C***
C***    IRTEXT
C***
C*********************************************************************
C       Returns EXTRA information stored in "unused" portions of
C       file header (25 words max!!).
C
C       EXTRA             : Buffer containing data to be placed in extra slot.
C       ISTART            : Which "extra" element to start at (1-25)
C       NEXTRA            : Number of words to transfer
C*********************************************************************
        entry irtext(istream,extra,istart,nextra)
C***
        j = lstream(istream)
        if (istart .gt. 25 .or. istart .lt. 1) 
     . stop 'IRTEXT: Error in start number'
        jextra = min(nextra + istart,26) - istart
        call ccpmvr(extra,stuff(istart,j),jextra)
        return
C*********************************************************************
C***
C***    IRTLAB
C***
C*********************************************************************
C       Return label information
C
C       LABELS(20,10)     : space for labels
C       NL                : actual # of labels being used
C*********************************************************************

        entry irtlab(istream,labels,nl)
C***
        j = lstream(istream)
        nl = nlab(j)
        call ccpmvi(labels,labls(1,1,j),nl*20)
        return
C*********************************************************************
C***
C***    IRTORG
C***
C*********************************************************************
C
C       Returns ORIGIN information
C
C       XORIG,YORIG       : X,Y origin information
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
C***    IRTRMS
C
C*********************************************************************
C       Returns RMS information
C
C       RMSDEV    : RMS deviation information
C*********************************************************************
        entry irtrms(istream,rmsdev)
C***
        j = lstream(istream)
        rmsdev = rms(j)
        return
C*********************************************************************
C***
C***    IRTSAM
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
C***    IRTSIZ
C***
C*********************************************************************
C       RETURN SIZE information for file. Header NOT actually written!!!
C
C       INXYZ             : size of file Columns, Rows, Sections
C       MXYZ              : size of full map Columns, Rows, Sections
C       NXYZST            : starting # for Columns, Rows, Sections (usually 1) 
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
C***    IRTSYM
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
C***    IRTUVW
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
99      write(6,'('' IRDHDR: End-of-File error !!!'')')
        stop 'Error'
        end
C*********************************************************************
C***
C***    IMPOSN
C***
C*********************************************************************
C       Position Read/Write pointer to RELATIVE Section NZ and Line NY
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
C***    IRDLIN
C***    IRDSEC
C***    IRDPAL
C***
C*********************************************************************
C       Read in a line, a section or part of a line (NX1 - NX2)
C       into the REAL or COMPLEX REAL array ARRAY. If required,
C       Integer*1 or Integer*2 data on disk is converted to REALS.
C       After reading a partial line, the pointer is positioned to
C       the START of the next line.
C       Byte data is stored with numbers from 128 to 255 interpreted 
C       as -128 to -1, but returned to the calling program as their
C       original values 128 - 255.  
C NOTE: The start of a line is ALWAYS 0 (ie NX1,NX2 are relative)
C*********************************************************************
        subroutine irdlin(istream,array,*)
C***
        include 'imsubs_common.for'
C***
        byte            bline(8192)
        byte            qb(2)
C***
        integer*2       line(4096)
        integer*2       ib
        integer*4       kb
C***
         real*4 array(*)
C***
        equivalence (line,bline),(ib,qb)
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
1       j = lstream(istream)
        if(unknown(j)) then
         write(6,'(''!!! Fatal error - attempted UNKNOWN status file read !!!'')')
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
          call qreadr(j,array,nread,ier)
          if (ier .ne. 0) goto 99
         else
          index = 1
10        n = min(8192,nread)
          call qreadi(j,line,n,ier)
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
          call qreadr(j,array,nread,ier)
          if (ier .ne. 0) go to 99
         else
          index = 1
15        n = min(4096,nread)
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
99      continue
        return 1
        end
C*********************************************************************
C***
C***    IWRLIN
C***    IWRSEC
C***    IWRPAL
C***
C*********************************************************************
C       Write out a line, a section or part of a line (NX1 - NX2)
C       from the REAL or COMPLEX REAL array ARRAY. If required,
C       data is converted to Integer*2 format before writing to disk.
C       After writing a partial line, the pointer is always advanced
C       to the start of the next line.
C NOTE: The start of a line is ALWAYS 0 (ie NX1,NX2 are relative)
C ANOTHER NOTE : in iwrpal, a part of the array denoted by nx1 and nx2
C       is written at the start of the record. The pointer is then
C       moved up 1 position by the system and then needs to be advanced
C       to the start of the next record.
C*********************************************************************
        subroutine iwrlin(istream,array)
C***
        include 'imsubs_common.for'
        byte            bline(8192)
        byte            qb(2)
C***
        integer*2       line(4096)
        integer*2       ib
        integer*4       kb
C***
        real*4          array(*)
C***
        equivalence     (line,bline), 
     *                  (ib,qb)
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
         write(6,'(
     *   ''!!! Error - cannot overwrite old-style 20th century map.''/
     *   ''    Use image_convert program and retry.'')')
         stop
        else if(karch(j) .ne. 0) then
         write(6,'(
     *   ''!!! Error - cannot overwrite map of different machine ''//
     *   ''architecture - use image_convert program and retry.'')')
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
          call qwritr(j,array(index),nwrite)
         else
10        n = min(8192,nwrite)
          do k = 1,n
            kb = nint(array(index))
            if(kb .gt. 127) kb = kb - 256
            bline(k) = kb
            index = index + 1
          end do
          call qwriti(j,line,n)
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
          call qwritr(j,array(index),nwrite)
         else
15        n = min(4096,nwrite)
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
C*********************************************************************
C***
C***    IRDPAS
C***
C*********************************************************************
C       Reads in a part of a section, converting from Integer*2
C       as required. After the read, the pointer is positioned at
C       the start of the next section. Array is first cleared
C       and then the selected portion of the image is loaded in.
C NOTE: The start of a line is ALWAYS 0 (ie NX1,NX2, NY1,NY2 are relative)
C
C       MX,MY           : Dimesnions of ARRAY
C                       : for complex numbers (MODES 3 & 4)
C                       : MX MUST be multiplied by 2 (ie # of REALS)
C       NX1,NX2         : Start and stop Column numbers (in COMPLEX if 3,4)
C       NY1,NY2         : Start and stop Line numbers
C
C       ARRAY DIMENSIONS ARE FOR CORRECT TYPE FOR REALS!!
C       MUST MULTIPLY MX*2 FOR COMPLEX!!!
C       BUT NX1,NX2 REFER TO COMPLEX NUMBERS!!!
C*********************************************************************
        subroutine irdpas(istream,array,mx,my,nx1,nx2,ny1,ny2,*)
C***
        include 'imsubs_common.for'
        real*4          array(mx,my)
C***
        j = lstream(istream)
        if(unknown(j)) then
         write(6,'(
     *   ''!!! Fatal error - attempted UNKNOWN status file read !!!'')')
         stop
        end if
        jmode = mode(j)
        jb = nb(jmode + 1)
        ncb = ncrs(1,j)*jb
        call ccpzi(array,mx*my)
C**************************
C*** ny1 is relative offset
C**************************
        nskip = ny1*ncb                         
        call qmode(j,0,nchhdr)
        call qskip(j,nskip)
        ndo = ny2 - ny1 + 1
C***
        do 100 jy = 1,ndo
          call irdpal(istream,array(1,jy),nx1,nx2,*99)
100     continue
C****************************************
C***   may have to skip to end of section
C****************************************
c       call qmode(j,0,nchhdr)
c       call qlocate(j,loc)
c       jy = (loc - nbhdr - nbsym(j) - 1)/ncb           
c       jsec = jy/ncrs(2,j)
c       jy = jy - jsec*ncrs(2,j)
c       if (jy .eq. 0) then
c         nskip = 0
c       else
c         nskip = (ncrs(2,j) - jy)*ncb
c       end if
C*** calculate number to skip to start of next section
        nskip = ncrs(1,j) * (ncrs(2,j) - ny2 - 1) +
     *          ncrs(1,j) - nx2 - 1
        call qmode(j,0,nchhdr)
        call qskip(j,nskip)
        return
99      return 1
        end
C*********************************************************************
C***
C***    IWRPAS
C***
C*********************************************************************
C       Writes out a part of a section, converting to Integer*2
C       as required.
C       After writing a partial line, the pointer is always advanced
C       to the start of the next line.
C NOTE: The start of a line is ALWAYS 0 (ie NX1,NX2, NY1,NY2 are relative)
C
C       MX,MY           : Dimesnions of ARRAY
C                       : for complex numbers (MODES 3 & 4)
C                       : MX MUST be multiplied by 2 (ie # of REALS)
C       NX1,NX2         : Start and stop Column numbers (in COMPLEX if 3,4)
C       NY1,NY2         : Start and stop Line numbers
C
C       ARRAY DIMENSIONS ARE FOR CORRECT TYPE FOR REALS!!
C       MUST MULTIPLY MX*2 FOR COMPLEX!!!
C       BUT NX1,NX2 REFER TO COMPLEX NUMBERS!!!
C*********************************************************************
        subroutine iwrpas(istream,array,mx,my,nx1,nx2,ny1,ny2)
C***
        include 'imsubs_common.for'
        real*4          array(mx,my)
C***
        j = lstream(istream)
        if(old(j)) then
         write(6,'(
     *   ''!!! Error - cannot overwrite old-style 20th century map.''/
     *   ''    Use image_convert program and retry.'')')
         stop
        else if(karch(j) .ne. 0) then
         write(6,'(
     *   ''!!! Error - cannot overwrite map of different machine ''//
     *   ''architecture - use image_convert program and retry.'')')
         stop
        end if
C***
        jmode = mode(j)
        jb = nb(jmode + 1)
        ncb = ncrs(1,j)*jb
C************************
C*** for start @ 0
C************************
        do 100 jy = ny1+1,ny2+1                         
          call iwrpal(istream,array(1,jy),nx1,nx2)
100     continue
C*****************************************
C***   may have to skip to end of section
C*****************************************
c       call qmode(j,0,nchhdr)
c       call qlocate(j,loc)
c       jy = (loc - nbhdr - nbsym(j) - 1)/ncb           
c       jsec = jy/ncrs(2,j)
c       jy = jy - jsec*ncrs(2,j)
c       if (jy .eq. 0) then
c         nskip = 0
c       else
c         nskip = (ncrs(2,j) - jy)*ncb
c       end if
C*** calculate number to skip to start of next record
        nskip = ncrs(1,j) - nx2 - 1
        call qmode(j,0,nchhdr)
        call qskip(j,nskip)
        return
        end
C*********************************************************************
C***
C***    ICLLIM
C***
C*********************************************************************
C       Subroutine to aid with selecting a region of an image.
C       This routine will ask the user to enter region limits
C       (0,0 = lower left corner) and will return pixel coords
C       for bounds. The deafult input / will return full image
C       limits. All requests are truncated to actual image boundary.
C       For MODE 3,4 images, values refer to complex values.
C       Returned pixel coordinates are RELATIVE to start of image.
C       ie. XYZmin = 0 means @ whatever start point is on file.
C
C       IXYZMIN(3)      Lower pixel limits
C       IXYZMAX(3)      Upper pixel limits
C       MXYZ(3)         Number of pixels
C*********************************************************************
        subroutine icllim(istream,ixyzmin,ixyzmax,mxyz)
C***
        include 'imsubs_common.for'
        integer*4       ixyzmin(3)
        integer*4       ixyzmax(3)
        integer*4       mxyz(3)
C***
        j = lstream(istream)
        call ccpzi(ixyzmin,3)
        call ccpzi(ixyzmax,3)
        call ccpmvi(mxyz,ncrs(1,j),3)
        ixyzmax(3) = mxyz(3) - 1
C***
        if (mxyz(3) .eq. 1) then
          write(6,1000) mxyz(1),mxyz(2)
1000      format(/,' Number of points (X,Y)= ',2I5,/'$Enter Limits',
     .   ' (Xmin,max,Ymin,max) '/' [ (0,0) is lower-left corner ] : ')
          read(5,*,end=10) (ixyzmin(k),ixyzmax(k),k=1,2)
        else
          write(6,1100) mxyz
1100      format(/,' Number of points (X,Y,Z)= ',3I5,/,' Enter Limits',
     .   ' (Xmin,max,Ymin,max,Zmin,max)'/' [ (0,0) is',
     .   '  lower-left corner ] : ')
          read(5,*,end=10) (ixyzmin(k),ixyzmax(k),k=1,3)
        end if
C*******************************************************************************
C***If min,max both= 0 then use full range (except in Z),else make sure in range
C*******************************************************************************
10      do 100 k = 1,3
C************************
C*** for relative start
C************************
          imin = 0                              
          imax = imin + mxyz(k) - 1
          if (ixyzmin(k).eq.0 .and. ixyzmax(k).eq.0 .and. k.ne.3) then
            ixyzmin(k) = imin
            ixyzmax(k) = imax
          else
            if (ixyzmin(k) .lt. imin) ixyzmin(k) = imin
            if (ixyzmax(k) .gt. imax) ixyzmax(k) = imax
          end if
          mxyz(k) = ixyzmax(k) - ixyzmin(k) + 1
100     continue
        return
        end
C*********************************************************************
C
C*ICTLIM
C
C*********************************************************************
C       Subroutine to aid with selecting a region of a transform.
C       This routine will ask the user to enter number in x & y
C       and will return pixel coords for bounds. The default input
C       / will return full image limits. All requests are truncated 
C       to actual image boundary. Values refer to complex values.
C       Returned pixel coordinates are RELATIVE to start of image.
C       ie. XYZmin = 0 means @ whatever start point is on file.
C
C       IXYZMIN(3)      Lower pixel limits
C       IXYZMAX(3)      Upper pixel limits
C       MXYZ(3)         Number of pixels
C*********************************************************************
C
        subroutine ictlim(istream,ixyzmin,ixyzmax,mxyz)
C***
        include 'imsubs_common.for'
C***
        integer*4       ixyzmin(3)
        integer*4       ixyzmax(3)
        integer*4       mxyz(3)
C***
        j = lstream(istream)
        call ccpzi(ixyzmin,3)
        call ccpzi(ixyzmax,3)
        call ccpmvi(mxyz,ncrs(1,j),3)
        ixyzmax(3) = mxyz(3) - 1
C***
        write(6,1000) mxyz(1),mxyz(2)
 1000   format(/' Number of points (X,Y) =',2I5/
     1         '$ Enter half number in X, half number in Y  :')
        read(5,*) nox, noy
C*************************************
C*** if min,max both 0 use full range
C*************************************
        if(nox.eq.0) then
        nox = mxyz(1)
        noy = mxyz(2) / 2
        end if
C***
        ixyzmin(1) = 0
        ixyzmax(1) = nox - 1
        m2 = mxyz(2) / 2
        ixyzmin(2) = m2 - noy
        ixyzmax(2) = m2 + noy - 1
        ixyzmin(3) = 0
        ixyzmax(3) = 0
C***
        do i=1,3
        mxyz(i) = ixyzmax(i) - ixyzmin(i) + 1
        end do
        return
        end
C**********************************************************************
C***
C***    ICLDEN
C***
C**********************************************************************
C       Subroutine to calculate the min/max/ & mean densities
C       for a portiion of an array. Array is assumed to be
C       a real image array.
C
C       MX,MY           : Dimesnions of ARRAY
C                       : for complex numbers (MODES 3 & 4)
C                       : MX MUST be multiplied by 2 (ie # of REALS)
C       NX1,NX2         : Start and stop Column numbers (in COMPLEX if 3,4)
C       NY1,NY2         : Start and stop Line numbers
C       DMIN,DMAX,DMEAN : Min, Max, and Mean density values for the selected
C                       : area of ARRAY
C**********************************************************************
        subroutine iclden(array,mx,my,nx1,nx2,ny1,ny2,dmin,dmax,dmean)
        real*4           array(mx,my)
C***
        dmin = 1.e10
        dmax = -1.e10
        dtot = 0.0
        do 200 iy = ny1,ny2
          do 100 ix = nx1,nx2
            dval = array(ix,iy)
            dtot = dtot + dval
            val = dval
            dmin = min(dmin,val)
            dmax = max(dmax,val)
100       continue
200     continue
C***
        dmean = dtot/float((nx2 - nx1 + 1)*(ny2 - ny1 + 1))
C***
        return
        end
C**********************************************************************
C***
C***    ICLCDN
C***
C**********************************************************************
C       Subroutine to calculate the min/max/ & mean MODULI
C       for a portiion of an array. Array is assumed to be
C       a COMPLEX image array.
C
C       MX,MY           : Dimensions of COMPLEX ARRAY
C       NX1,NX2         : Start and stop Column numbers (in COMPLEX if 3,4)
C       NY1,NY2         : Start and stop Line numbers
C       DMIN,DMAX,DMEAN : Min, Max, and Mean density values for the selected
C                       : area of ARRAY
C**********************************************************************
        subroutine iclcdn(array,mx,my,nx1,nx2,ny1,ny2,dmin,dmax,dmean)
        complex         array(mx,my)
C***
        dmin = 1.e10
        dmax = -1.e10
        dtot = 0.
        do 200 iy = ny1,ny2
          do 100 ix = nx1,nx2
            dval = cabs(array(ix,iy))
            dtot = dtot + dval
            val = dval
            dmin = min(dmin,val)
            dmax = max(dmax,val)
100       continue
200     continue
C***
        dmean = dtot/float((nx2 - nx1 + 1)*(ny2 - ny1 + 1))
        return
        end
