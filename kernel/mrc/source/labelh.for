C*LABEL.FOR********************************************************************
C                                                                             *
C       Program to perform simple manipulations on image files                *
C                                                                             *
C       Version 1.12    31-MAR-82    DAA        for VAX                       *
C       Version 1.13    13-OCT-82    DAA        for VAX                       *
C       Version 1.14    7- JAN-85    MK         some more routines            *
C                                               for nicer displays            *
C       Version 1.15    20-APR-85    RH         line size 8192                *
C       Version 1.16    12-JUN-87    RH         extra option                  *
C       Version 1.17    06-FEB-92    RH         max/min/mean on option 4      *
C       Version 1.18    26-MAR-92    RH         transfer "extra" data         *
C       Version 1.20    07-OCT-95    RH         increased dimensions          *
C       Version 1.21    17-DEC-96    RH         new single particle option 50 *
C       Version 1.22    01-JAN-97    RH         clean up new option           *
C       Version 1.23    01-SEP-98    RH         remove title option 50 I/P    *
C       Version 1.H1    01-FEB-03    HST        more transforming options     *
C******************************************************************************
C
C Input is a single file in mapformat, output is a single file in mapformat
C
C      Card input, or online control
C
C        card  1 :  input filename
C        card  2 :  IMODE, describes which of the various simple manipulations
C                are required - can be -2,-1,0,1,2,3,4 or 99 for images
C                                      as well as 5,6,7,8 for transforms.
C                If IMODE is 50, single particle boxing and restacking option
C                If IMODE is 99, a second IMODE of 1,2,3,4 gives more options
C                                      and 5,6,7,8 more options for transforms.
C        card  3 :  output filename
C        cards 4 onwards are dependent on manipulation - see below
C -------------------------------------------------------------------------
C IMODE= -3: Change to BYTE output format with automatic scaling
C
C IMODE= -2: Change INTEGER*2/INTEGER*1 output format
C        card  4 : SCALE, scale factor for INTEGER*2/BYTE conversion
C
C IMODE= -1: Change REAL/INTEGER*2 output format
C        card  4 : SCALE, scale factor for REAL/INTEGER conversion
C
C IMODE=  0: Change Labels
C        card  4 : NLA, number of labels to ADD ?  (can be negative)
C        cards 5 : TITLE,  enter titles to add for label number #
C     or card  5 : NLR, remove labels # (0 to exit) ? 
C
C IMODE=  1: Select region
C        card  4 : enter Limits (Xmin,max,Ymin,max) if NZ=1
C     or card  4 : enter Limits (Xmin,max,Ymin,max,Zmin,max) if NZ>1
C
C IMODE=  2: Linear OD stretch  ( y = mx + b )
C        card  4 : A,B enter coefficients A & B ( Y = AX + B )
C        card  5 : IQ=0 or 1 - set values <0 to 0  (0= no, 1=yes) ?
C
C IMODE=  3: Logrithmic  OD stretch ( y = aLOGx + b )
C        card  4 : A,B enter coefficients A & B ( Y = A*ALOG(X) + B )
C
C IMODE=  4: Average adjacent pixels
C        card  4 : NREDX,NREDY enter integer reduction factor for X,Y:
C
C IMODE=  5: Output amplitudes or Intensities
C        card  4 : IQ, write out amplitudes (0) or Intensities*.01 (1) ?
C
C IMODE=  6: Output Phases (degrees)
C        no further input needed
C
C IMODE=  7: Output REAL part of Complex value
C        no further input needed
C
C IMODE=  8: Output IMAGINARY part of Complex value
C        no further input needed
C
C IMODE=  9: Fill FFT with Amplitude
C
C IMODE= 10: Transform 16 bit linear transmission into 16 bit OD
C
C IMODE= 11: Transform 16 bit linear transmission into 8 bit OD
C
C IMODE= 12: Fix STEM images for IMOD
C
C IMODE= 13: Cyclically permutate CELL information in header.
C
C IMODE= 14: Merge 7x7 images into one large image
C
C IMODE= 15: Cross out image
C
C IMODE= 16: Transform into INTEGER*2 output format (16bit) with automatic scaling to [0 ; 16000]
C
C IMODE= 17: Compute image statistics
C
C IMODE= 18: Transform into INTEGER*2 output format (16bit) with automatic scaling to [0 ; 16000] and swap unsigned/signed
C
C IMODE= 19: Transform into REAL output format with automatic scaling to [1 ; 100]
C
C IMODE= 20: Produce thumbnail image
C        card 4 : Enter X/Y dimensions of output image
C
C IMODE= 21: Merge two images into one output image
C        card 4 : Name of second image
C
C IMODE= 29: Interpolate into two times larger image
C
C IMODE= 30: Pad into new larger dimensions
C        card 4 : ISIZE: square dimension of new output image
C
C IMODE= 31: Select region with change of header,
C        card  4 : enter Limits (Xmin,max,Ymin,max) if NZ=1
C     or card  4 : enter Limits (Xmin,max,Ymin,max,Zmin,max) if NZ>1
C
C IMODE= 32: Interpolate into new given dimensions
C
C IMODE= 33: Interpolate into new given dimensions
C
C IMODE= 34: Correct Z info in header to number of particles
C
C IMODE= 35: Computer relative ice layer intensity in FFT
C
C IMODE= 39: Transform into REAL output format with automatic scaling to STDEV=100
C
C IMODE= 40: Transform into REAL output format with automatic scaling to STDEV=1
C
C IMODE= 41: Put pixel size into header
C
C IMODE= 42: Transform into REAL output format with automatic scaling to STDEV=100 and TRUNC to 4xSTD
C
C -----------------------------------------------------------
C IMODE= 50: goes to the single particle selection option, with input
C            from a file of X,Y coordinates, and desired box size.
C        card  4 : NBOXX,NBOXY
C        card  5 : input filename for the list of X,Y coordinates of
C                  the centres of the areas to be selected.
C -----------------------------------------------------------
C IMODE= 99: More options, mainly for display purposes
C      then asks for a second IMODE
C
C IMODE=  1: VARIOUS 90 DEG TURNS AND MIRRORS
C        card  4 : ITURN, TURN NO? (1:Z90,2:Z-90,3:Z180,4:Xmir,5:Ymir)
C
C IMODE=  2: GEOMETRIC STRETCH ( y = m**X )
C        card  4 : A, enter A (Y = X**A)
C
C IMODE=  3: CUT OFF OVER - AND UNDERFLOWS
C        card  4 : A,B enter LOWEST AND HIGHEST VALUE TO PRUNE IMAGE WITH
C
C IMODE=  4: GET RID OF OUTLIERS BY INTERPOLATION
C        card  4 : critdiff, enter critical difference
C
C****************************************************************************
      PARAMETER (LMAX=16384,LCMX=8192,LPIC=16384)
      COMMON //NX,NY,NZ,IXMIN,IYMIN,IZMIN,IXMAX,IYMAX,IZMAX
C     DIMENSION APIC(LPIC,LPIC),BPIC(LPIC,LPIC)
      REAL, ALLOCATABLE :: APIC(:,:),BPIC(:,:)
      DIMENSION ALINE(LMAX),NXYZ(3),MXYZ(3),NXYZST(3)
      DIMENSION IXYZMIN(3),IXYZMAX(3),OUT(LMAX),OU2(LMAX)
      DIMENSION LABELS(20,10),CELL(6),EXTRA(29)
      DIMENSION DNCELL(6),MXYZN(3)
      COMPLEX CLINE(LCMX),COUT(LCMX),CVAL
      CHARACTER*200 INFILE,OUTFILE
      character*80 TITLE
      DIMENSION ITITLE(20)
      character*200 cfile,cname,cstring,COUTFILE,CINFILE,CDEBUG
      REAL*8 DOUBLMEAN,DOUBLOMEAN,DOUBLTMP
      INTEGER*4 iover,iunder,ilow
C
      LOGICAL LDEBUG
C
      EQUIVALENCE (NX,NXYZ), (ALINE,CLINE), (OUT,COUT)
      EQUIVALENCE (IXYZMIN, IXMIN), (IXYZMAX, IXMAX)
      EQUIVALENCE (ITITLE,TITLE)
      DATA NXYZST/3*0/, CNV/57.29578/
C
      allocate(APIC(LPIC,LPIC))
      allocate(BPIC(LPIC,LPIC))
C
      WRITE(6,1000)
1000  FORMAT(/,/,' LABEL: Image Manipulation Program',
     .    '  VH1.23(1-Sep-98)',/)
      WRITE(6,1100)
1100    FORMAT('$Input filename:  ')
        READ(5,1200) INFILE
1200    FORMAT(A)
        call shorten(INFILE,k)
        write(6,'(''Read: '',A)')INFILE(1:k)

        CALL IMOPEN(1,INFILE,'RO')
        write(6,'(''File opened.'')')
C
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
        CALL IRTCEL(1,CELL)
        write(6,'(''Header read.'')')
C
        NXT = NX
        MXT = MX
        IF (MODE .GE. 3) then
          NXT = NXT*2
          MXT = MXT*2
        endif
C
1       WRITE(6,1300)
1300    FORMAT(/,' Available modes of operation are: ',/,
     . ' -3: Change to BYTE with autom. scaling',/,
     . ' -2: Change INTEGER*2/INTEGER*1 output format',/,
     . ' -1: Change REAL/INTEGER*2 output format',/,
     . '  0: Change Labels',/,
     . '  1: Select region',/,
     . '  2: Linear OD stretch  ( y = mx + b )',/,
     . '  3: Logrithmic  OD stretch ( y = aLOGx + b )',/,
     . '  4: Average adjacent pixels',/,
     . ' 10: Transform WORD (2Byte) into OD (INT*2)',/,
     . ' 11: Transform WORD (2Byte) into OD (INT*1)',/,
     . ' 12: Fix STEM image problem',/,
     . ' 13: Cyclically permutate CELL information in header',/,
     . ' 14: Merge 7x7 images into one large image',/,
     . ' 15: Cross out image',/,
     . ' 16: Transform [0;16000] INT*2 image',/,
     . ' 17: Compute image statistics',/,
     . ' 18: Swap unsigned/signed, and Transform [0;16000] INT*2 image',/,
     . ' 19: Transform [1;100] REAL image',/,
     . ' 20: Produce thumbnail image',/,
     . ' 21: Merge two images into one',/,
     . ' 29: Interpolate into two times larger image',/,
     . ' 30: Pad into square image',/,
     . ' 31: Select region (and change header)',/,
     . ' 33: Interpolate into new given dimensions',/,
     . ' 34: Correct Z info in header to number of particles',/,
     . ' 35: Compute relative ice layer intensity in FFT',/,
     . ' 39: Transform STDEV=100 REAL image',/,
     . ' 40: Transform STDEV=1 REAL image',/,
     . ' 41: Put pixel size into header',/,
     . ' 42: Transform STDEV=100 REAL image with 4xSTDEV truncation',/,
     . ' 50: create stack of small boxes centred on coords read',
     . ' in from list',/,
     .   ' 99: More options, mainly for display purposes')

        IF (MODE .GE. 3) WRITE(6,1400)
1400    FORMAT(
     . '  5: Output amplitudes or Intensities',/,
     . '  6: Output Phases (degrees)',/,
     . '  7: Output REAL part of Complex value',/,
     . '  8: Output IMAGINARY part of Complex value',/,
     . '  9: Fill FFT with Amplitude',/,
     . ' 32: Cut over and underflows from Amplitudes',/)
C
        WRITE(6,1500)
1500    FORMAT(/,'$Enter desired mode: ')
        READ(5,*) IMODE
        write(*,'('' Read MODE Number '',I6)')IMODE
C
                if (imode.eq.50) then
                        call IMCLOSE(1)
                        call LABELC(INFILE,APIC,BPIC)
                        stop
                endif
                if (IMODE.eq.99) then
                        call IMCLOSE(1)
                        call LABELB(INFILE,APIC,BPIC)
                        stop
                endif
        IF ((IMODE .GT. 4 .and. IMODE .ne. 10 .and. IMODE.ne.11 
     1      .and. IMODE.ne.12 .and. IMODE.ne.13 .and. IMODE.ne.14
     1      .and. IMODE.ne.15 .and. IMODE.ne.16 .and. IMODE.ne.17
     1      .and. IMODE.ne.18 .and. IMODE.ne.19 .and. IMODE.ne.20
     1      .and. IMODE.ne.21 .and. IMODE.ne.29 .and. IMODE.ne.30
     1      .and. IMODE.ne.31 .and. IMODE.ne.32 .and. IMODE.ne.33
     1      .and. IMODE.ne.34 .and. IMODE.ne.35
     1      .and. IMODE.ne.39 .and. IMODE.ne.40 .and. IMODE.ne.41
     1      .and. IMODE.ne.42)
     1      .AND. MODE .LT. 3) then
          write(*,'('' ERROR: Illegal mode for this file type'')')
          GOTO 1
        endif
C
        WRITE(6,1550)
1550    FORMAT(10X)
C
C        IF (IMODE .NE. 0 .and. IMODE.ne.17) THEN
        IF (IMODE.ne.17) THEN
          WRITE(6,1600)
1600      FORMAT('$Output filename:  ')
          READ(5,1200) OUTFILE
          CALL IMOPEN(2,OUTFILE,'NEW')
          CALL ITRHDR(2,1)
        END IF
        if ( IMODE.eq.10 ) goto 91
        if ( IMODE.eq.11 ) goto 92
        if ( IMODE.eq.12 ) goto 93
        if ( IMODE.eq.13 ) goto 97
        if ( IMODE.eq.14 ) goto 99
        if ( IMODE.eq.15 ) goto 107
        if ( IMODE.eq.16 ) goto 108
        if ( IMODE.eq.17 ) goto 110
        if ( IMODE.eq.18 ) goto 111
        if ( IMODE.eq.19 ) goto 112
        if ( IMODE.eq.20 ) goto 94
        if ( IMODE.eq.21 ) goto 116
        if ( IMODE.eq.29 ) goto 98
        if ( IMODE.eq.30 ) goto 95
        if ( IMODE.eq.31 ) goto 96
C-------------------------------97 already taken for "13"
        if ( IMODE.eq.32 ) goto 109
        if ( IMODE.eq.33 ) goto 114
        if ( IMODE.eq.34 ) goto 120
        if ( IMODE.eq.35 ) goto 121
        if ( IMODE.eq.39 ) goto 115
        if ( IMODE.eq.40 ) goto 117
        if ( IMODE.eq.41 ) goto 118
        if ( IMODE.eq.42 ) goto 119
        GOTO (113,3,5,10,20,30,40,45,50,60,70,80,90) IMODE+4
C
C=====================================================================
C
C  MODE 10 :  Transforming WORD*2 into OD (INT*2)
C
91      continue
        write(TITLE,'(''LABELH Mode 10: Transform p.Trans. into OD '')')
        write(6,'('' Transforming into OD'')')
        DOMIN =  1.E10
        DOMAX = -1.E10
        DOMEAN = 0.0
        DOUBLOMEAN = 0.0
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        ilow = 0
        DO IZ= 1,NZ
          DO IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            DO IX = 1,NXT
C
C-------------Image is MODE=1, i.e. unsigned short, not INTEGER*2,
C-------------because it was generated by TIF2MRC.exe.
C-------------This is interpreted in INTEGER*2 as (-32000...32000)
C-------------Flip negative values to positive end by adding 65536.
C-------------It is also Negative Transmission. 
C             neg.transm.: 0     = black=absorb.=   prot=>OD=high
C                          65535 = white=transp.=no prot=>OD=0
C            
C-------------Divide by 65536 to get values between 0 and 1, i.e.
C-------------between full transmission (transparent = 0 neg. Transm.) 
C-------------and no transmission (black = 1 neg. Transmission).
C
              VAL = ALINE(IX)
              if(VAL.lt.0)VAL=VAL+65536
              if(VAL.lt.1)VAL=1
              if (VAL .LT. DOMIN) DOMIN = VAL
              if (VAL .GT. DOMAX) DOMAX = VAL
              DOUBLOMEAN = DOUBLOMEAN + VAL
C
              VAL = VAL/65536.0
              VAL = -log(VAL) * 1000.0
C-------------Now, a value of 1000 is OD=1.
              IF (VAL .LT. DMIN) DMIN = VAL
              IF (VAL .GT. DMAX) DMAX = VAL
              DOUBLMEAN = DOUBLMEAN + VAL
              ALINE(IX) = VAL
            enddo
            CALL IWRLIN(2,ALINE)
          enddo
        enddo
        DOMEAN = DOUBLOMEAN/(NXT*NY*NZ)
        DMEAN = DOUBLMEAN/(NXT*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        write(6,'('' old file MIN,MAX,MEAN: '',3G12.4)')
     1       DOMIN,DOMAX,DOMEAN
        write(6,'('' new file MIN,MAX,MEAN: '',3G12.4)')
     1       DMIN,DMAX,DMEAN
        write(6,'('' OD range is          : '',3G12.4)')
     1       DMIN/1000.0,DMAX/1000.0,DMEAN/1000.0
        GOTO 990
C
C=====================================================================
C
C  MODE 11 :  Transforming WORD*2 into OD (INT*1)
C
92      continue
        write(TITLE,'(''LABELH Mode 11: Transform p.Trans. into OD '')')
        write(6,'('' Transforming into OD'')')
        DOMIN =  1.E10
        DOMAX = -1.E10
        DOMEAN = 0.0
        DOUBLOMEAN = 0.0
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
        MODE = 0
        CALL IALMOD(2,MODE)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        WRITE(6,'('' Scale factor : '')')
        READ(5,*) SCALE
C
        iunder=0
        iover=0
        DO IZ= 1,NZ
          DO IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            DO IX = 1,NXT
              VAL = ALINE(IX)
C-------------if(VAL.lt.0)VAL=VAL+65536
              if(VAL.lt.0)VAL=(-VAL)+32768
              if(VAL.lt.1)VAL=1
              if (VAL .LT. DOMIN) DOMIN = VAL
              if (VAL .GT. DOMAX) DOMAX = VAL
              DOUBLOMEAN = DOUBLOMEAN + VAL
C
              VAL = -log(VAL) * 1000.0 * SCALE
C-------------Now, a value of 1000*SCALE is OD=1.
              if(VAL.lt.1.0)then
                VAL=1.0
                iunder=iunder+1
              endif
              if(VAL.gt.255.0)then
                VAL=255.0
                iover=iover+1
              endif
              IF (VAL .LT. DMIN) DMIN = VAL
              IF (VAL .GT. DMAX) DMAX = VAL
              DOUBLMEAN = DOUBLMEAN + VAL
              ALINE(IX) = VAL
            enddo
            CALL IWRLIN(2,ALINE)
          enddo
        enddo
        DOMEAN = DOUBLOMEAN/(NXT*NY*NZ)
        DMEAN = DOUBLMEAN/(NXT*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        write(6,'('' old file MIN,MAX,MEAN: '',3G12.4)')
     1       DOMIN,DOMAX,DOMEAN
        write(6,'('' new file MIN,MAX,MEAN: '',3G12.4)')
     1       DMIN,DMAX,DMEAN
        write(6,'('' OD range is          : '',3G12.4)')
     1       DMIN/1000.0,DMAX/1000.0,DMEAN/1000.0
        if(ilow.gt.0)then
          write(6,'('' WARNING: there were '',I10,
     1    '' low values.'')')ilow
        endif
        if(iunder.gt.0)then
          write(6,'('' WARNING: there were '',I10,
     1    '' values under 1'')')iunder
        endif
        if(iover.gt.0)then
          write(6,'('' WARNING: there were '',I10,
     1    '' values over 255'')')iover
        endif
        GOTO 990
C
C=====================================================================

C
C  MODE 12 :  Transforming STEM problem images
C
93      continue
        write(TITLE,'(''LABELH Mode 12: Transform STEM images '')')
        write(6,'('' Transforming STEM images'')')
        DOMIN =  1.E10
        DOMAX = -1.E10
        DOMEAN = 0.0
        DOUBLOMEAN = 0.0
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
        MODE = 2
        CALL IALMOD(2,MODE)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        iunder=0
        iover=0
        DO IZ= 1,NZ
          DO IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            DO IX = 1,NXT
              VAL = ALINE(IX)
              if (VAL .LT. DOMIN) DOMIN = VAL
              if (VAL .GT. DOMAX) DOMAX = VAL
              DOUBLOMEAN = DOUBLOMEAN + VAL
C
              if(VAL.lt.20000)VAL=VAL+32768
              if(VAL.lt.1)VAL=1
C
              IF (VAL .LT. DMIN) DMIN = VAL
              IF (VAL .GT. DMAX) DMAX = VAL
              DOUBLMEAN = DOUBLMEAN + VAL
C
              ALINE(IX) = VAL
            enddo
            CALL IWRLIN(2,ALINE)
          enddo
        enddo
        DOMEAN = DOUBLOMEAN/(NXT*NY*NZ)
        DMEAN = DOUBLMEAN/(NXT*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        write(6,'('' old file MIN,MAX,MEAN: '',3G12.4)')
     1       DOMIN,DOMAX,DOMEAN
        write(6,'('' new file MIN,MAX,MEAN: '',3G12.4)')
     1       DMIN,DMAX,DMEAN
        GOTO 990
C
C=====================================================================
C
C  MODE 13 :  Cyclically permutate CELL header information
C
97      continue
        write(TITLE,'(''LABELH Mode 13: Cyclically permutate header'',
     1     '' CELL information '')')
        write(6,'('' Cyclically permutating CELL header info'')')
C
        CALL IRTCEL(1,CELL)
        DNCELL(1) = CELL(3)
        DNCELL(2) = CELL(1)
        DNCELL(3) = CELL(2)
        DNCELL(4) = CELL(4)
        DNCELL(5) = CELL(5)
        DNCELL(6) = CELL(6)
        MXYZN(1) = MXYZ(3)
        MXYZN(2) = MXYZ(1)
        MXYZN(3) = MXYZ(2)
C
        CALL ICRHDR(2,NXYZ,MXYZN,MODE,LABELS,NL)
C
        DO IZ = 1,NZ
          DO IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            CALL IWRLIN(2,ALINE)
          enddo
        enddo
C
        CALL IALCEL(2,DNCELL)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
C
        write(6,'('' old CELL labels CellA,CellB,CellC: '',3G12.4)')
     1       (CELL(I),I=1,3)
        write(6,'('' new CELL labels CellA,CellB,CellC: '',3G12.4)')
     1       (DNCELL(I),I=1,3)
        GOTO 990
C
C=====================================================================
C
C  MODE 14 :  Merge 7x7 images into one large image
C
99      continue
        write(TITLE,'(''LABELH Mode 14: Merge 7x7 images into one large ''
     1     '' image '')')
        write(6,'('' Merging 7x7 images'')')
C
        read(*,'(A)')cfile
        call shorten(cfile,ilen)
C
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
        CALL IRTLAB(1,LABELS,NL)
        CALL IRTEXT(1,EXTRA,1,29)
        CALL IRTCEL(1,CELL)
C
        write(*,'('' Opened file has dimensions '',2I6)')NX,NY
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
C-------Set 7 times larger output dimensions
C
        NFAC = 7
C
        NSX=NX
        NSY=NY
C
        do j = 1,NFAC
C---------Open 7 input files
          do i = 1,NFAC
            write(*,'(78(''=''))')
            write(*,'('' Working on tile '',2I6)')i,j
            if((j.ne.1) .or. (i.ne.1))then
              write(cname,'(A,''_'',I1,''_'',I1,''_ps.mrc'')')
     .          cfile(1:ilen),i,j
              write(*,'('': Opening file '',A40)')cname
              CALL IMOPEN(1,cname,'RO')
              CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
              NXT = NX
              IF (MODE .GE. 3) NXT = NXT*2
            endif
            write(*,'('' '')')
C
C-----------Calculate offset for first corner of output image
            IOFFSX=(i-1)*NSX
            IOFFSY=(NFAC-j)*NSY
C
C-----------Copy entire file into output picture
            do iy = 1,NSY
              CALL IRDLIN(1,ALINE,*999)
              do ix = 1,NSX
                VAL=ALINE(ix)
                APIC(IOFFSX+ix,IOFFSY+iy)=VAL
                IF (VAL .LT. DMIN) DMIN = VAL
                IF (VAL .GT. DMAX) DMAX = VAL
                DOUBLMEAN = DOUBLMEAN + VAL
              enddo
            enddo
            if(i.ne.NFAC .or. j.ne.NFAC)then
              CALL IMCLOSE(1)
            endif
          enddo
        enddo
C
C  Put title labels, new cell and extra information only into header
C
        NX=NX * NFAC
        NY=NY * NFAC
        NXYZ(1)=NX
        NXYZ(2)=NY
C
        DOUBLMEAN = DOUBLMEAN/(NX*NY)
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
C-------Write the 7x7 files into the output file
        do iy = 1,NY
          do ix = 1,NX
            ALINE(ix)=APIC(ix,iy)
          enddo
          CALL IWRLIN(2,ALINE)
        enddo
C
        write(*,'('' Min, Max, Mean of output file is '',3F12.3)')
     .     DMIN,DMAX,DMEAN
C
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
C
        GOTO 990
C
C=====================================================================
C
C  MODE 15 :  Cross out image
C
107     continue
        write(TITLE,'(''LABELH Mode 15: Cross out image '')')
        write(6,'('' Crossing out image'')')
C
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
        CALL IRTLAB(1,LABELS,NL)
        CALL IRTEXT(1,EXTRA,1,29)
        CALL IRTCEL(1,CELL)
C
        write(*,'('' Opened file has dimensions '',2I6)')NX,NY
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
C-------Copy entire file into output picture
        do iy = 1,NY
          CALL IRDLIN(1,ALINE,*999)
          do ix = 1,NX
            VAL=ALINE(ix)
            APIC(ix,iy)=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
          enddo
        enddo
C
        DOUBLMEAN = DOUBLMEAN/(NX*NY)
        DMEAN = DOUBLMEAN
C
C-------Cross out image
C
        do ix = 30,NX-30
          APIC(ix  ,ix  )=DMAX
          APIC(ix+1,ix  )=DMAX
          APIC(ix+1,ix-1)=DMAX
          APIC(ix  ,ix+1)=DMAX
          APIC(ix-1,ix+1)=DMAX
          APIC(ix  ,NX-ix+1  )=DMAX
          APIC(ix  ,NX-ix+1+1)=DMAX
          APIC(ix+1,NX-ix+1+1)=DMAX
          APIC(ix-1,NX-ix+1  )=DMAX
          APIC(ix-1,NX-ix+1-1)=DMAX
        enddo
C
C  Put title labels, new cell and extra information only into header
C
        MODE=2
C
        CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
        CALL IALEXT(2,EXTRA,1,29)
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
C-------Write the file into the output file
        do iy = 1,NY
          do ix = 1,NX
            ALINE(ix)=APIC(ix,iy)
          enddo
          CALL IWRLIN(2,ALINE)
        enddo
C
        write(*,'('' Min, Max, Mean of output file is '',3F12.3)')
     .     DMIN,DMAX,DMEAN
C
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
C
        GOTO 990
C
C=====================================================================
C
C  MODE 16 : Produce INTEGER*2 (16 bit) output image with range [0;16000]
C
108     continue
        write(TITLE,'(''LABELH Mode 16: Produce INT*2 [1;16k] image '')')
        write(6,'('' Producing INT*2 with Autoscaling [1;16000] '')')
C
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
        CALL IRTLAB(1,LABELS,NL)
        CALL IRTEXT(1,EXTRA,1,29)
        CALL IRTCEL(1,CELL)
C
        IF (MODE .GT. 2) THEN
          STOP 'Only works on real images'
        ENDIF
C
        write(*,'('' Opened file has dimensions '',2I6)')NX,NY
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
C-------Copy entire file into output picture
        do iy = 1,NY
          CALL IRDLIN(1,ALINE,*999)
          do ix = 1,NX
            VAL=ALINE(ix)
            APIC(ix,iy)=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
          enddo
        enddo
C
        DMEAN = DOUBLMEAN/(NX*NY)
C
        write(*,'('' Opened file has range MIN,MAX,MEAN: '',3G16.5)')
     1     DMIN,DMAX,DMEAN
C
        DVAL = ABS(DMAX - DMIN)
        write(*,'('' That is a dynamic range of: '',G16.5)')
     1     DVAL
C
C-------Calculate offset and scaling factors
C
C        RTARGET=256.0
        RTARGET=16000.0
        RTARGET=RTARGET-1.0
        ROFF = DMIN - 1.0
        if ( DVAL .gt. 0.00000001 ) then
          RSCALE = RTARGET / DVAL
        else
          RSCALE = RTARGET  
        endif
C
        write(*,'('' Calculated offset of '',G16.5)')ROFF
        write(*,'('' Calculated scaling factor of '',G16.5)')RSCALE
C
C-------Output file is INT*2 (16 bit, Mode=1)
C
        MODE = 1
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
C-------Put title labels, new cell and extra information only into header
C
        CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
        CALL IALEXT(2,EXTRA,1,29)
        CALL IALCEL(2,CELL)
        CALL IALMOD(2,MODE)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
C-------Write the file into the output file
        do iy = 1,NY
          do ix = 1,NX
            DOUBLTMP = APIC(ix,iy)
            DOUBLTMP = (DOUBLTMP - ROFF) * RSCALE
            VAL=DOUBLTMP
            ALINE(ix)=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
          enddo
          CALL IWRLIN(2,ALINE)
        enddo
C
        DMEAN = DOUBLMEAN/(NX*NY)
C
        write(*,'('' Min, Max, Mean of output file is '',3F12.3)')
     .     DMIN,DMAX,DMEAN
C
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
C
        GOTO 990
C
C=====================================================================
C
C  MODE 17 :  Read image statistics
C
110     continue
        write(TITLE,'(''LABELH Mode 17: Read image statistics '')')
        write(6,'('' Compute image statistics'')')
C
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
        CALL IRTLAB(1,LABELS,NL)
        CALL IRTEXT(1,EXTRA,1,29)
        CALL IRTCEL(1,CELL)
C
        IF (MODE .GT. 2) THEN
          STOP 'Only works on real images'
        ENDIF
C
        write(*,'('' Opened file has dimensions '',2I6)')NX,NY
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
        DO IZ = 1,NZ
          do iy = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            do ix = 1,NX
              VAL=ALINE(ix)
              IF (VAL .LT. DMIN) DMIN = VAL
              IF (VAL .GT. DMAX) DMAX = VAL
              DOUBLMEAN = DOUBLMEAN + VAL
            enddo
          enddo
        enddo
C
        DMEAN = DOUBLMEAN/(NX*NY)
C
        write(*,'('' Opened file has range MIN,MAX,MEAN: '',3G16.5)')
     1     DMIN,DMAX,DMEAN
C
        DVAL = ABS(DMAX - DMIN)
        write(*,'('' That is a dynamic range of: '',G16.5)')
     1     DVAL
C
        CALL IMPOSN(1,0,0)
C
C-------Now calculate standard deviation
C
        DOUBLMEAN = 0.0
        DO IZ = 1,NZ
          do iy = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            do ix = 1,NX
              VAL=(ALINE(ix)-DMEAN)**2
              DOUBLMEAN = DOUBLMEAN + VAL
            enddo
          enddo
        enddo
C
        DOUBLMEAN=DOUBLMEAN/(NX*NY-1)
        if(DOUBLMEAN.lt.0.0)DOUBLMEAN=ABS(DOUBLMEAN)
        DSTD = SQRT(DOUBLMEAN)
C
        write(*,'('' Standard Deviation is: '',G16.5)')DSTD
C
C-------Limit Min and Max to 5% to 95%:
        write(*,'('' Give percentage of cutoff (e.g. 5) '')')
        read(*,*)DVAL1
        write(*,'('' Cutting off upper and lower '',F8.5,''%'')')DVAL1
        DMIN1=DMIN + (DMAX-DMIN)*DVAL1/100.0
        DMAX1=DMAX - (DMAX-DMIN)*DVAL1/100.0
C
C-------Limit Min and Max to MEAD -/+ 2.5 * STD
        write(*,'('' Give multiple of STD as width (e.g. 3) '')')
        read(*,*)DVAL2
        write(*,'('' Cutting off beyond '',F8.5,'' times STD'')')DVAL2
        DMIN2=DMEAN - DSTD*DVAL2
        DMAX2=DMEAN + DSTD*DVAL2
C
C-------Take the more narrow of the two:
C        if(DMIN1.lt.DMIN2)DMIN1=DMIN2
        if(DMAX1.gt.DMAX2)DMAX1=DMAX2
C
        write(*,'('' Useful truncation limits are: '',2G16.5)')DMIN1,DMAX1
C
        call system('\rm -f labelh.tmp')
        open(13,FILE='labelh.tmp',STATUS='NEW')
        write(13,'('' Min, Max, Mean, STD, GoodMin, GoodMax: '')')
        write(13,'(G16.5)')DMIN
        write(13,'(G16.5)')DMAX 
        write(13,'(G16.5)')DMEAN
        write(13,'(G16.5)')DSTD 
        write(13,'(G16.5)')DMIN1
        write(13,'(G16.5)')DMAX2
        close(13)
C
        GOTO 990
C
C=====================================================================
C
C  MODE 18 : Swap Unsigned/Signed, and Produce INTEGER*2 (16 bit) output image with range [0;16000]
C
111     continue
        write(TITLE,'(''LABELH Mode 18: Swap unsigned/signed, and '',
     .                ''Produce INT*2 [1;16k] image '')')
        write(6,'('' Swapping UNSIGNED/SIGNED, and '')')
        write(6,'('' Producing INT*2 with Autoscaling [1;16000] '')')
C
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
        CALL IRTLAB(1,LABELS,NL)
        CALL IRTEXT(1,EXTRA,1,29)
        CALL IRTCEL(1,CELL)
C
        IF (MODE .GT. 2) THEN
          STOP 'Only works on real images'
        ENDIF
C
        write(*,'('' Opened file has dimensions '',2I6)')NX,NY
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
C-------Copy entire file into output picture, while swapping unsigned/signed:
        do iy = 1,NY
          CALL IRDLIN(1,ALINE,*999)
          do ix = 1,NX
            VAL=ALINE(ix)
            if (VAL.lt.0.0) then
               VAL=VAL+65536.0
            endif
            APIC(ix,iy)=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
          enddo
        enddo
C
        DMEAN = DOUBLMEAN/(NX*NY)
C
        write(*,'('' Opened file has range MIN,MAX,MEAN: '',3G16.5)')
     1     DMIN,DMAX,DMEAN
C
        DVAL = ABS(DMAX - DMIN)
        write(*,'('' That is a dynamic range of: '',G16.5)')
     1     DVAL
C
C-------Calculate offset and scaling factors
C
C        RTARGET=256.0
        RTARGET=16000.0
        RTARGET=RTARGET-1.0
        ROFF = DMIN - 1.0
        if ( DVAL .gt. 0.00000001 ) then
          RSCALE = RTARGET / DVAL
        else
          RSCALE = RTARGET  
        endif
C
        write(*,'('' Calculated offset of '',G16.5)')ROFF
        write(*,'('' Calculated scaling factor of '',G16.5)')RSCALE
C
C-------Output file is INT*2 (16 bit, Mode=1)
C
        MODE = 1
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
C-------Put title labels, new cell and extra information only into header
C
        CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
        CALL IALEXT(2,EXTRA,1,29)
        CALL IALCEL(2,CELL)
        CALL IALMOD(2,MODE)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
C-------Write the file into the output file
        do iy = 1,NY
          do ix = 1,NX
            DOUBLTMP = APIC(ix,iy)
            DOUBLTMP = (DOUBLTMP - ROFF) * RSCALE
            VAL=DOUBLTMP
            ALINE(ix)=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
          enddo
          CALL IWRLIN(2,ALINE)
        enddo
C
        DMEAN = DOUBLMEAN/(NX*NY)
C
        write(*,'('' Min, Max, Mean of output file is '',3F12.3)')
     .     DMIN,DMAX,DMEAN
C
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
C
        GOTO 990
C
C=====================================================================
C
C  MODE 19 : Produce REAL output image with range [1;100]
C
112     continue
        write(TITLE,'(''LABELH Mode 19: Produce REAL image '')')
        write(6,'('' Producing REAL with Autoscaling [1;100] '')')
C
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
        CALL IRTLAB(1,LABELS,NL)
        CALL IRTEXT(1,EXTRA,1,29)
        CALL IRTCEL(1,CELL)
C
        IF (MODE .GT. 2) THEN
          STOP 'Only works on real images'
        ENDIF
C
        write(*,'('' Opened file has dimensions '',2I6)')NX,NY
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
C-------Copy entire file into output picture
        do iy = 1,NY
          CALL IRDLIN(1,ALINE,*999)
          do ix = 1,NX
            VAL=ALINE(ix)
            APIC(ix,iy)=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
          enddo
        enddo
C
        DMEAN = DOUBLMEAN/(NX*NY)
C
        write(*,'('' Opened file has range MIN,MAX,MEAN: '',3G16.5)')
     1     DMIN,DMAX,DMEAN
C
        DVAL = ABS(DMAX - DMIN)
        write(*,'('' That is a dynamic range of: '',G16.5)')
     1     DVAL
C
C-------Calculate offset and scaling factors
C
C        RTARGET=256.0
        RTARGET=100.0
        RTARGET=RTARGET-1.0
        ROFF = DMIN - 1.0
        if ( DVAL .gt. 0.00000001 ) then
          RSCALE = RTARGET / DVAL
        else
          RSCALE = RTARGET  
        endif
C
        write(*,'('' Calculated offset of '',G16.5)')ROFF
        write(*,'('' Calculated scaling factor of '',G16.5)')RSCALE
C
C-------Output file is INT*2 (16 bit, Mode=1)
C
        MODE = 2
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
C-------Put title labels, new cell and extra information only into header
C
        CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
        CALL IALEXT(2,EXTRA,1,29)
        CALL IALCEL(2,CELL)
        CALL IALMOD(2,MODE)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
C-------Write the file into the output file
        do iy = 1,NY
          do ix = 1,NX
            DOUBLTMP = APIC(ix,iy)
            DOUBLTMP = (DOUBLTMP - ROFF) * RSCALE
            VAL=DOUBLTMP
            ALINE(ix)=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
          enddo
          CALL IWRLIN(2,ALINE)
        enddo
C
        DMEAN = DOUBLMEAN/(NX*NY)
C
        write(*,'('' Min, Max, Mean of output file is '',3F12.3)')
     .     DMIN,DMAX,DMEAN
C
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
C
        GOTO 990
C
C=====================================================================
C
C  MODE 39 : Produce REAL output image with range [-1000;1000]
C
115     continue
        write(TITLE,'(''LABELH Mode 39: Produce REAL image '')')
        write(6,'('' Producing REAL with Autoscaling Mean 0 '',
     .    ''and STD 100'')')
C
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
        CALL IRTLAB(1,LABELS,NL)
        CALL IRTEXT(1,EXTRA,1,29)
        CALL IRTCEL(1,CELL)
C
        IF (MODE .GT. 2) THEN
          STOP 'Only works on real images'
        ENDIF
C
        write(*,'('' Opened file has dimensions '',2I6)')NX,NY
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
C-------Find Mean
        do iy = 1,NY
          CALL IRDLIN(1,ALINE,*999)
          do ix = 1,NX
            VAL=ALINE(ix)
            APIC(ix,iy)=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
          enddo
        enddo
C
        DMEAN = DOUBLMEAN/(NX*NY)
C
        write(*,'('' Opened file has range MIN,MAX,MEAN: '',3G16.5)')
     1     DMIN,DMAX,DMEAN
C
        DOUBLMEAN=0.0
C-------Find STD
        do iy = 1,NY
          do ix = 1,NX
            VAL=APIC(ix,iy)
            DOUBLMEAN = DOUBLMEAN + (VAL-DMEAN)**2
          enddo
        enddo
C
        VAL = DOUBLMEAN/(NX*NY)
        DSTD = SQRT(VAL)
C
        write(*,'('' Opened file has range MIN,MAX,MEAN,STDEV: '',
     .     4G16.5)')
     .     DMIN,DMAX,DMEAN,DSTD
C
        DVAL = ABS(DMAX - DMIN)
        write(*,'('' That is a dynamic range of: '',G16.5)')
     1     DVAL
C
C-------Calculate offset and scaling factors
C
        RTARGET=100.0
        ROFF = DMEAN
        if ( DSTD .gt. 0.00000001 ) then
          RSCALE = RTARGET / DSTD
        else
          RSCALE = RTARGET  
        endif
C
        write(*,'('' Calculated offset of '',G16.5)')ROFF
        write(*,'('' Calculated scaling factor of '',G16.5)')RSCALE
C
C-------Rescale image to MEAN=0, STD=RTARGET
        do iy = 1,NY
          do ix = 1,NX
            APIC(ix,iy)=(APIC(ix,iy)-ROFF)*RSCALE
          enddo
        enddo
C-------Output file is INT*2 (16 bit, Mode=1)
C
        MODE = 2
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
C-------Put title labels, new cell and extra information only into header
C
        CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
        CALL IALEXT(2,EXTRA,1,29)
        CALL IALCEL(2,CELL)
        CALL IALMOD(2,MODE)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
C-------Write the file into the output file
        do iy = 1,NY
          do ix = 1,NX
            VAL=APIC(ix,iy)
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
            ALINE(ix)=VAL
          enddo
          CALL IWRLIN(2,ALINE)
        enddo
C
        DMEAN = DOUBLMEAN/(NX*NY)
C
        write(*,'('' Min, Max, Mean of output file is '',3F12.3)')
     .     DMIN,DMAX,DMEAN
C
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
C
        GOTO 990
C
C=====================================================================
C
C  MODE 40 : Produce REAL output image with range [-1;1]
C
117     continue
        write(TITLE,'(''LABELH Mode 40: Produce REAL image '')')
        write(6,'('' Producing REAL with Autoscaling Mean 0 '',
     .    ''and STD 1'')')
C
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
        CALL IRTLAB(1,LABELS,NL)
        CALL IRTEXT(1,EXTRA,1,29)
        CALL IRTCEL(1,CELL)
C
        IF (MODE .GT. 2) THEN
          STOP 'Only works on real images'
        ENDIF
C
        write(*,'('' Opened file has dimensions '',2I6)')NX,NY
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
C-------Find Mean
        do iy = 1,NY
          CALL IRDLIN(1,ALINE,*999)
          do ix = 1,NX
            VAL=ALINE(ix)
            APIC(ix,iy)=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
          enddo
        enddo
C
        DMEAN = DOUBLMEAN/(NX*NY)
C
        write(*,'('' Opened file has range MIN,MAX,MEAN: '',3G16.5)')
     1     DMIN,DMAX,DMEAN
C
        DOUBLMEAN=0.0
C-------Find STD
        do iy = 1,NY
          do ix = 1,NX
            VAL=APIC(ix,iy)
            DOUBLMEAN = DOUBLMEAN + (VAL-DMEAN)**2
          enddo
        enddo
C
        VAL = DOUBLMEAN/(NX*NY)
        DSTD = SQRT(VAL)
C
        write(*,'('' Opened file has range MIN,MAX,MEAN,STDEV: '',
     .     4G16.5)')
     .     DMIN,DMAX,DMEAN,DSTD
C
        DVAL = ABS(DMAX - DMIN)
        write(*,'('' That is a dynamic range of: '',G16.5)')
     1     DVAL
C
C-------Calculate offset and scaling factors
C
        RTARGET=1.0
        ROFF = DMEAN
        if ( DSTD .gt. 0.00000001 ) then
          RSCALE = RTARGET / DSTD
        else
          RSCALE = RTARGET  
        endif
C
        write(*,'('' Calculated offset of '',G16.5)')ROFF
        write(*,'('' Calculated scaling factor of '',G16.5)')RSCALE
C
C-------Rescale image to MEAN=0, STD=RTARGET
        do iy = 1,NY
          do ix = 1,NX
            APIC(ix,iy)=(APIC(ix,iy)-ROFF)*RSCALE
          enddo
        enddo
C-------Output file is INT*2 (16 bit, Mode=1)
C
        MODE = 2
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
C-------Put title labels, new cell and extra information only into header
C
        CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
        CALL IALEXT(2,EXTRA,1,29)
        CALL IALCEL(2,CELL)
        CALL IALMOD(2,MODE)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
C-------Write the file into the output file
        do iy = 1,NY
          do ix = 1,NX
            VAL=APIC(ix,iy)
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
            ALINE(ix)=VAL
          enddo
          CALL IWRLIN(2,ALINE)
        enddo
C
        DMEAN = DOUBLMEAN/(NX*NY)
C
        write(*,'('' Min, Max, Mean of output file is '',3F12.3)')
     .     DMIN,DMAX,DMEAN
C
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
C
        GOTO 990
C
C=====================================================================
C
C  MODE 41 : Put pixel size into header
C
118    continue
       write(TITLE,'(''LABELH Mode 41: Put pixel size into header'')')
       write(6,'('' Placing pixel size into header'')')
C
       CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
       CALL IRTLAB(1,LABELS,NL)
       CALL IRTEXT(1,EXTRA,1,29)
       CALL IRTCEL(1,CELL)
C
       write(*,'('' Opened file has dimensions '',2I6)')NX,NY
C
       WRITE(6,'(''Enter pixel size in Angstroems:'')')
       READ(5,*) RPIXEL
       write(6,'(''Placing '',F12.6,
     .    ''Angstroems as pixel size into header'')')
C
       CELL(1)=MXYZ(1)*RPIXEL
       CELL(2)=MXYZ(2)*RPIXEL
       CELL(3)=MXYZ(3)*RPIXEL
C
       CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
       CALL IALEXT(2,EXTRA,1,29)
       CALL IALCEL(2,CELL)
       CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
       DO IZ = 1,NZ
         DO IY = 1,NY
           CALL IRDLIN(1,ALINE,*999)
           CALL IWRLIN(2,ALINE)
         enddo
       enddo
C
       GOTO 990
C
C=====================================================================
C
C  MODE 42 : Produce REAL output image with STD 100, crop 6xSTDEV
C
119     continue
        write(TITLE,'(''LABELH Mode 41: Produce REAL image '')')
        write(6,'('' Producing REAL with Autoscaling Mean 0 '',
     .    ''and STD 100 and trunc 6xSTD'')')
C
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
        CALL IRTLAB(1,LABELS,NL)
        CALL IRTEXT(1,EXTRA,1,29)
        CALL IRTCEL(1,CELL)
C
        IF (MODE .GT. 2) THEN
          STOP 'Only works on real images'
        ENDIF
C
        write(*,'('' Opened file has dimensions '',2I6)')NX,NY
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
C-------Find Mean
        do iy = 1,NY
          CALL IRDLIN(1,ALINE,*999)
          do ix = 1,NX
            VAL=ALINE(ix)
            APIC(ix,iy)=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
          enddo
        enddo
C
        DMEAN = DOUBLMEAN/(NX*NY)
C
        write(*,'('' Opened file has range MIN,MAX,MEAN: '',3G16.5)')
     1     DMIN,DMAX,DMEAN
C
        DOUBLMEAN=0.0
C-------Find STD
        do iy = 1,NY
          do ix = 1,NX
            VAL=APIC(ix,iy)
            DOUBLMEAN = DOUBLMEAN + (VAL-DMEAN)**2
          enddo
        enddo
C
        VAL = DOUBLMEAN/(NX*NY)
        DSTD = SQRT(VAL)
C
        write(*,'('' Opened file has range MIN,MAX,MEAN,STDEV: '',
     .     4G16.5)')
     .     DMIN,DMAX,DMEAN,DSTD
C
        DVAL = ABS(DMAX - DMIN)
        write(*,'('' That is a dynamic range of: '',G16.5)')
     1     DVAL
C
C-------Calculate offset and scaling factors
C
        RTARGET=100.0
        STD6=6*RTARGET
        ROFF = DMEAN
        if ( DSTD .gt. 0.00000001 ) then
          RSCALE = RTARGET / DSTD
        else
          RSCALE = RTARGET  
        endif
C
        write(*,'('' Calculated offset of '',G16.5)')ROFF
        write(*,'('' Calculated scaling factor of '',G16.5)')RSCALE
C
        DMIN =  1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
C
C-------Rescale image to MEAN=0, STD=RTARGET
        do iy = 1,NY
          do ix = 1,NX
            VAL=(APIC(ix,iy)-ROFF)*RSCALE
            if(VAL.gt. STD6)VAL= STD6
            if(VAL.lt.-STD6)VAL=-STD6
            APIC(ix,iy)=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
          enddo
        enddo
C
        DMEAN = DOUBLMEAN/(NX*NY)
C
        write(*,'('' Min, Max, Mean of output file is '',3F12.3)')
     .     DMIN,DMAX,DMEAN
C
C-------Output file is INT*2 (16 bit, Mode=1)
C
        MODE = 2
C
C-------Put title labels, new cell and extra information only into header
C
        CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
        CALL IALEXT(2,EXTRA,1,29)
        CALL IALCEL(2,CELL)
        CALL IALMOD(2,MODE)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
C-------Write the file into the output file
        do iy = 1,NY
          do ix = 1,NX
            VAL=APIC(ix,iy)
            ALINE(ix)=VAL
          enddo
          CALL IWRLIN(2,ALINE)
        enddo
C
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
C
        GOTO 990
C
C=====================================================================
C
C  MODE 21 : Merge two images into one
C
116      continue
        write(TITLE,'(''LABELH Mode 21: Merge 2 images into one ''
     1     '' image '')')
        write(6,'('' Merging two images into one'')')
C
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
        CALL IRTLAB(1,LABELS,NL)
        CALL IRTEXT(1,EXTRA,1,29)
        CALL IRTCEL(1,CELL)
C
        write(*,'('' Opened file has dimensions '',2I6)')NX,NY
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
C-------Copy first file into output picture, while scaling to AVG=0,STD=1
C
        do iy = 1,NY
          CALL IRDLIN(1,ALINE,*999)
          do ix = 1,NX
            VAL=ALINE(ix)
            APIC(ix,iy)=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
          enddo
        enddo
        CALL IMCLOSE(1)
C
        DOUBLMEAN = DOUBLMEAN/(NX*NY)
        DMEAN = DOUBLMEAN
C
        DOUBLMEAN=0.0
C-------Find STD
        do iy = 1,NY
          do ix = 1,NX
            VAL=APIC(ix,iy)
            DOUBLMEAN = DOUBLMEAN + (VAL-DMEAN)**2
          enddo
        enddo
C
        VAL = DOUBLMEAN/(NX*NY)
        DSTD = SQRT(VAL)
C
        write(*,'('' Opened first file has range MIN,MAX,MEAN,STDEV: '',
     .     4G16.5)')
     .     DMIN,DMAX,DMEAN,DSTD
C
        DVAL = ABS(DMAX - DMIN)
        write(*,'('' That is a dynamic range of: '',G16.5)')
     1     DVAL
C
C-------Calculate offset and scaling factors
C
        RTARGET=100.0
        ROFF = DMEAN
        if ( DSTD .gt. 0.00000001 ) then
          RSCALE = RTARGET / DSTD
        else
          RSCALE = RTARGET
        endif
C
        write(*,'('' Calculated offset of '',G16.5)')ROFF
        write(*,'('' Calculated scaling factor of '',G16.5)')RSCALE
C
C-------Rescale image to MEAN=0, STD=RTARGET

        do iy = 1,NY
          do ix = 1,NX
            APIC(ix,iy)=(APIC(ix,iy)-ROFF)*RSCALE
          enddo
        enddo
C
C-------Now read the second image

        read(*,'(A)')cfile
        call shorten(cfile,ilen)
        write(6,'('' Opening second file: '',A)')cfile(1:ilen)
C
        NOX=NX
        NOY=NY
        CALL IMOPEN(1,cfile,'RO')
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
        if(NOX.ne.NX .or. NOY.ne.NY)then
          write(6,'('':: labelh.for: ERROR, both files need to have the same'',
     .      ''dimensions. Aborting. '')')
          goto 999
        endif
C
C-------Calculate offset for first corner of output image
        IOFFSX=NX/2
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
C-------Copy the right half of the second file onto the output picture
        do iy = 1,NY
          CALL IRDLIN(1,ALINE,*999)
          do ix = IOFFSX,NX
            VAL=ALINE(ix)
            APIC(ix,iy)=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
          enddo
        enddo
C
        CALL IMCLOSE(1)
C
        DOUBLMEAN = DOUBLMEAN/(NX*NY)
        DMEAN = DOUBLMEAN
C
        DOUBLMEAN=0.0
C-------Find STD
        do iy = 1,NY
          do ix = IOFFSX,NX
            VAL=APIC(ix,iy)
            DOUBLMEAN = DOUBLMEAN + (VAL-DMEAN)**2
          enddo
        enddo
C
        VAL = DOUBLMEAN/((NX/2)*NY)
        DSTD = SQRT(VAL)
C
        write(*,'('' Opened second file has range MIN,MAX,MEAN,STDEV: '',
     .     4G16.5)')
     .     DMIN,DMAX,DMEAN,DSTD
C
        DVAL = ABS(DMAX - DMIN)
        write(*,'('' That is a dynamic range of: '',G16.5)')
     1     DVAL
C
C-------Calculate offset and scaling factors
C
        RTARGET=100.0
        ROFF = DMEAN
        if ( DSTD .gt. 0.00000001 ) then
          RSCALE = RTARGET / DSTD
        else
          RSCALE = RTARGET
        endif
C
        write(*,'('' Calculated offset of '',G16.5)')ROFF
        write(*,'('' Calculated scaling factor of '',G16.5)')RSCALE
C
C-------Rescale image to MEAN=0, STD=RTARGET

        do iy = 1,NY
          do ix = IOFFSX,NX
            APIC(ix,iy)=(APIC(ix,iy)-ROFF)*RSCALE
          enddo
        enddo
C
C-------Put title labels, new cell and extra information only into header
C
        NXYZ(1)=NX
        NXYZ(2)=NY
        MODE = 2
C
        CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
        CELL(1) = REAL(NX)
        CELL(2) = REAL(NY)
        CALL IALEXT(2,EXTRA,1,29)
        CALL IALCEL(2,CELL)
        CALL IALMOD(2,MODE)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        write(*,'('' Output file has dimensions '',2I6)')NX,NY
C
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
C-------Write the file into the output file
        do iy = 1,NY
          do ix = 1,NX
            VAL=APIC(ix,iy)
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
            ALINE(ix)=VAL
          enddo
          CALL IWRLIN(2,ALINE)
        enddo
C
        DMEAN = DOUBLMEAN/(NX*NY)

        write(*,'('' Min, Max, Mean of output file is '',3F12.3)')
     .     DMIN,DMAX,DMEAN
C
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
C
        GOTO 990
C
C=====================================================================
C
C  MODE 20 :  Producing Thumbnail Image
C    
94      continue
C
        WRITE(6,'(''MODE 20: Producing Thumbnail image'')')
C       WRITE(6,'(''Enter output image dimensions in X,Y:'')')
C       READ(5,*) NOUTX,NOUTY
        NOUTX = 200
        NOUTY = 200
        if ( MODE.GE.3) NOUTX = NOUTX/2
C
        DMIN = 1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
        print *,'Label Mode 20: Producing thumbnail of dimension ',
     .      NOUTX,NOUTY
        NREDX = NX/NOUTX
        NREDY = NY/NOUTY
        NX = NOUTX
        NY = NOUTY
C  Put title labels, new cell and extra information only into header
        CALL IRTLAB(1,LABELS,NL)
        CALL IRTEXT(1,EXTRA,1,29)
        CALL IRTCEL(1,CELL)
        CELL(1) = CELL(1)*NX*NREDX/MXYZ(1)
        CELL(2) = CELL(2)*NY*NREDY/MXYZ(2)
        CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
        CALL IALEXT(2,EXTRA,1,29)
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        NX4 = NX*4
        SCL = 1./(NREDX*NREDY)
        IF (MODE .GE. 3) NX4 = NX4*2
        DO IZ = 1,NZ
          CALL IMPOSN(1,IZ-1,0)
          DO IY = 1,NY
            CALL ZERO(OUT,NX4)
            IF (MODE .LE. 2) THEN
              DO JY = 1,NREDY
                CALL IRDLIN(1,ALINE,*999)
                INDEX = 0
                DO IX = 1,NX
                  DO JX = 1,NREDX       
                    INDEX = INDEX + 1
                    OUT(IX) = OUT(IX) + ALINE(INDEX)*SCL
                  enddo
                enddo
              enddo
              DO IX=1,NX
                VAL=OUT(IX)
                IF (VAL .LT. DMIN) DMIN = VAL
                IF (VAL .GT. DMAX) DMAX = VAL
                DOUBLMEAN = DOUBLMEAN + VAL
              enddo
            ELSE
              DO JY = 1,NREDY
                CALL IRDLIN(1,ALINE,*999)
                INDEX = 0
                DO IX = 1,NX
                  DO JX = 1,NREDX       
                    INDEX = INDEX + 1
                    COUT(IX) = COUT(IX) + CLINE(INDEX)*SCL
                  enddo
                enddo
              enddo
              DO IX=1,NX
                VAL=OUT(IX)
                IF (VAL .LT. DMIN) DMIN = VAL
                IF (VAL .GT. DMAX) DMAX = VAL
                DOUBLMEAN = DOUBLMEAN + VAL
              enddo
            END IF
            CALL IWRLIN(2,OUT)
          enddo
        enddo
        DMEAN = DOUBLMEAN/(NX*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C=====================================================================
C
C  MODE 29 :  Pad into square image
C    
98     continue
C
       WRITE(6,'(''MODE 29: Interpolate into two times larger image'')')
       IF (MODE .GT. 2) THEN
         STOP '::Only works on real images'
       ENDIF
       NOUTX = 2*NX
       NOUTY = 2*NY
       NOUTZ = 0.5*NZ
       print *,'Label Mode 29: Interpolating into image of dimension ',
     .   NOUTX,NOUTY
C
       NX = NOUTX
       NY = NOUTY
C
C  Put title labels, new cell and extra information only into header
       CALL IRTLAB(1,LABELS,NL)
       CALL IRTEXT(1,EXTRA,1,29)
       CALL IRTCEL(1,CELL)
       CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
       CELL(1) = REAL(NOUTX)
       CELL(2) = REAL(NOUTY)
       CELL(3) = REAL(NOUTZ)
       CALL IALEXT(2,EXTRA,1,29)
       CALL IALCEL(2,CELL)
       CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
       DO IZ = 1,NZ
         CALL IMPOSN(1,IZ-1,0)
         DO IY = 1,NY,2
C
           CALL IRDLIN(1,ALINE,*999)
C
           if(IY.gt.1)then
             DO IX = 1,NX,2
               OUT(IX)=(ALINE(INT(IX/2)+1)+OU2(IX))/2.0
               OUT(IX+1)=(((ALINE(INT(IX/2)+1)+ALINE(INT(IX/2)+2))/2.0)+OU2(IX+1))/2.0
             ENDDO
             OUT(NX)=(ALINE(INT(NX/2))+OU2(NX))/2.0
           else
             DO IX = 1,NX,2
               OUT(IX)=ALINE(INT(IX/2)+1)
               OUT(IX+1)=(ALINE(INT(IX/2)+1)+ALINE(INT(IX/2)+2))/2.0
             enddo
             OUT(NX)=ALINE(INT(NX/2))
           endif
           CALL IWRLIN(2,OUT)
C
           DO IX = 1,NX,2
             OUT(IX)=ALINE(INT(IX/2)+1)
             OUT(IX+1)=(ALINE(INT(IX/2)+1)+ALINE(INT(IX/2)+2))/2.0
             OU2(IX)=OUT(IX)
             OU2(IX+1)=OUT(IX+1)
           ENDDO
           OUT(NX)=ALINE(INT(NX/2))
           OU2(NX)=OUT(NX)
           CALL IWRLIN(2,OUT)
C
         enddo
       enddo
       GOTO 990
C
C=====================================================================
C
C  MODE 30 :  Pad into square image
C    
95     continue
C
       WRITE(6,'(''MODE 30: Pad into square image'')')
       IF (MODE .GT. 2) THEN
         STOP 'Only works on real images'
       ENDIF
       WRITE(6,'(''Enter dimension of output image:'')')
       READ(5,*) IOUTDIM
       NOUTX = IOUTDIM
       NOUTY = IOUTDIM
       print *,'Label Mode 30: Padding into image of dimension ',
     .   NOUTX,NOUTY
C
       IXSTART = (IOUTDIM - NX) / 2
       IXSKIP=0
       if(IXSTART.lt.1)then
C--------Crop to center in X-direction
         IXSKIP=(NX-IOUTDIM)/2
         write(*,'(''IXSKIP = '',I9)')IXSKIP
         IXSTART=1
       endif
C
       IYSTART = (IOUTDIM - NY) / 2
       write(*,'(/,''IYSTART = '',I9)')IYSTART
       IYSKIP=0
       if(IYSTART.lt.1)then 
C--------crop to top edge in Y-direction
         IYSKIP=NY-IOUTDIM
         IYSTART=1
         write(*,'(''IYSTART corrected to '',I9)')IYSTART
         write(*,'(''IYSKIP = '',I9)')IYSKIP
       endif
C
       IXEND   = IXSTART + NX - 1
       if(IXEND.gt.IXSTART+IOUTDIM-1)IXEND=IXSTART+IOUTDIM-1
       IYEND   = IYSTART + NY - 1
       write(*,'(''IYEND   = '',I9)')IYEND
       if(IYEND.gt.IYSTART+IOUTDIM-1)then
         IYEND=IYSTART+IOUTDIM-1
         write(*,'(''IYEND corrected to '',I9)')IYEND
       endif
C
       NX = NOUTX
       NY = NOUTY
C
C  Put title labels, new cell and extra information only into header
       CALL IRTLAB(1,LABELS,NL)
       CALL IRTEXT(1,EXTRA,1,29)
       CALL IRTCEL(1,CELL)
       CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
       CELL(1) = REAL(IOUTDIM)
       CELL(2) = REAL(IOUTDIM)
       CALL IALEXT(2,EXTRA,1,29)
       CALL IALCEL(2,CELL)
       CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
       DO IZ = 1,NZ
         CALL IMPOSN(1,IZ-1,0)
         if(IYSKIP.gt.0)then
           do IY=1,IYSKIP
             CALL IRDLIN(1,ALINE,*999)
           enddo
         endif
         DO IY = 1,NY
           if ( IY .ge. IYSTART .and. IY .le. IYEND ) then
             CALL IRDLIN(1,ALINE,*999)
             DO IX = 1,NX
               if ( IX .ge. IXSTART .and. IX .le. IXEND ) then
                 OUT(IX) = ALINE(IXSKIP+IX-IXSTART+1)
               else
                 OUT(IX) = DMEAN
               endif
             enddo
             CALL IWRLIN(2,OUT)
           else
             DO IX = 1,NX
               OUT(IX) = DMEAN
             enddo
             CALL IWRLIN(2,OUT)
           END IF
         enddo
       enddo
       GOTO 990
C
C=====================================================================
C
C  MODE 31 : SELECTING A REGION with change of header
C
 96     continue
C
        CALL ICLLIM(1,IXYZMIN,IXYZMAX,NXYZ)
        write(TITLE,'(''LABEL Mode 31: Min/Max XYZ = '',6I6)')
     1  IXMIN,IXMAX,IYMIN,IYMAX,IZMIN,IZMAX
        write(*,'(''LABEL Mode 31: Min/Max XYZ = '',6I6)')
     1  IXMIN,IXMAX,IYMIN,IYMAX,IZMIN,IZMAX
C
        IQ = 1
C       WRITE(6,1775)
C1775   FORMAT('$Use "true" starting limits (0) or start at 0 (1) ? ')
C       READ(5,*) IQ
        DO J = 1,3
          NXYZST(J) = 0
          IF (IQ .EQ. 0) NXYZST(J) = IXYZMIN(J)
        enddo
C
       CALL IRTLAB(1,LABELS,NL)
       CALL IRTEXT(1,EXTRA,1,29)
       CALL IRTCEL(1,CELL)
C
       CELL(1)=IXMAX-IXMIN+1
       CELL(2)=IYMAX-IYMIN+1
       write(*,'(''New CELL = '',2F12.1)')CELL(1),CELL(2)
C
       CALL IALSIZ(2,NXYZ,NXYZST)
       CALL IALCEL(2,CELL)
       CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        DMIN =  1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
        DO IZ = IZMIN,IZMAX
          CALL IMPOSN(1,IZ,IYMIN)
          DO IY = 1,NY
            CALL IRDPAL(1,ALINE,IXMIN,IXMAX,*999)
            CALL IWRLIN(2,ALINE)
            DO IX = 1,NX
              IF (MODE .LT. 3) THEN
                VAL = ALINE(IX)
              ELSE
                VAL = CABS(CLINE(IX))
              END IF
              DOUBLMEAN = DOUBLMEAN + VAL
              IF (VAL .LT. DMIN) DMIN = VAL
              IF (VAL .GT. DMAX) DMAX = VAL
            enddo
          enddo
        enddo
C
        DMEAN = DOUBLMEAN/(NX*NY*NZ)
        CELL(1)=IXMAX
        CELL(2)=IYMAX
        CELL(3)=IZMAX
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C
C=====================================================================
C
C  MODE 33 :  Interpolate into new given dimensions
C    
114    continue
C
       WRITE(6,'(''MODE 33: Interpolate into new given dimensions'')')
       IF (MODE .GT. 2) THEN
         STOP '::Only works on real images'
       ENDIF
       
       NINX = NX
       NINY = NY
       if(NZ.gt.1)then
         write(6,'(''::ERROR: Only works on 2D images, '',
     .     ''not on 3D stacks.'')')
         goto 990
       endif
       WRITE(6,'(''Enter dimension of output image:'')')
       READ(5,*) IOUTDIM
       NOUTX = IOUTDIM
       NOUTY = IOUTDIM
       print *,'Label Mode 33: Interpolating into new dimension ',
     .   NOUTX,NOUTY
C
C  Put title labels, new cell and extra information only into header
       CALL IRTLAB(1,LABELS,NL)
       CALL IRTEXT(1,EXTRA,1,29)
       CALL IRTCEL(1,CELL)
C
       CALL IMPOSN(1,0,0)
       DO IY = 1,NINY
         CALL IRDLIN(1,ALINE,*999)
         DO IX = 1,NINX
           APIC(IX,IY)=ALINE(IX)
         enddo
       enddo
C
       RSCAX = REAL(NINX) / REAL(NOUTX)
       RSCAY = REAL(NINY) / REAL(NOUTY)
       if(NINX.ne.NINY .or. NOUTX.ne.NOUTY)then
         write(6,'(''WARNING: NON-SQUARE image dimensions.'',
     .     '' Using X-scale.'')')
         RSCAY=RSCAX
         NOUTY=NOUTX
       endif
       write(6,'('': Downscaling by '',F9.3,'' times.'')')RSCAX
C
       NX = NOUTX
       NY = NOUTY
C
       CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
       CELL(1) = REAL(NOUTX)
       CELL(2) = REAL(NOUTY)
       CALL IALEXT(2,EXTRA,1,29)
       CALL IALCEL(2,CELL)
       CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
       DOUBLMEAN=0.0
       DMIN= 1e30
       DMAX=-1e30
       DO IY=1,NOUTY
         DO IX=1,NOUTX
           IOX=IX*RSCAX
           IOY=IY*RSCAY
           if(RSCAX.gt.1.0)then
C------------downscaling
             IMINSCAX=IX*RSCAX-RSCAX/2
             IMAXSCAX=IX*RSCAX+RSCAX/2
             IMINSCAY=IY*RSCAY-RSCAY/2
             IMAXSCAY=IY*RSCAY+RSCAY/2
             ICOUNT=0
             DOUBLTMP=0
             do ILY=IMINSCAY,IMAXSCAY
               do ILX=IMINSCAX,IMAXSCAX
                 if(ILX.gt.1 .and. ILX.lt.NINX .and.
     .              ILY.gt.1 .and. ILY.lt.NINY) then
                   DOUBLTMP=DOUBLTMP+APIC(ILX,ILY)
                   ICOUNT=ICOUNT+1
                 endif
               enddo
             enddo
             if(ICOUNT.lt.1)then
               STOP '::ERROR in labelh'
             endif
             DOUBLTMP=DOUBLTMP/ICOUNT
             OUT(IX)=DOUBLTMP
           else
C------------Upscaling
             OUT(IX)=APIC(IOX,IOY) 
           endif
           if(OUT(IX).lt.DMIN)DMIN=OUT(IX)
           if(OUT(IX).gt.DMAX)DMAX=OUT(IX)
           DOUBLMEAN=DOUBLMEAN+OUT(IX)
         enddo
         CALL IWRLIN(2,OUT)
       enddo
       DOUBLMEAN=DOUBLMEAN/(NOUTX*NOUTY)
       DMEAN=DOUBLMEAN
C
       CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
C
       GOTO 990
C
C=====================================================================
C
C  MODE 34 : Correct Z info in header
C
120    continue
       write(TITLE,'(''LABELH Mode 34: Correct Z info in header'')')
       write(6,'('' Correcting Z info in header'')')
C
       CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
       CALL IRTLAB(1,LABELS,NL)
       CALL IRTEXT(1,EXTRA,1,29)
       CALL IRTCEL(1,CELL)
C
       write(*,'('' Opened file has dimensions '',2I6)')NX,NY
C
       CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
       CALL IALEXT(2,EXTRA,1,29)
       CALL IALCEL(2,CELL)
       CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
       DMIN =  1.E10
       DMAX = -1.E10
       DOUBLMEAN = 0.0
       IZ = 0
1201   continue
         DO IY = 1,NY
           CALL IRDLIN(1,ALINE,*1202)
           DO IX = 1,NX
             VAL = ALINE(IX)
             DOUBLMEAN = DOUBLMEAN + VAL
             IF (VAL .LT. DMIN) DMIN = VAL
             IF (VAL .GT. DMAX) DMAX = VAL
           enddo
           CALL IWRLIN(2,ALINE)
         enddo
         IZ = IZ + 1
       goto 1201
C
1202   continue
C
       NZ=IZ
       CELL(3)=IZ
       DMEAN = DOUBLMEAN/(NX*NY*NZ)
       write(6,'('' New file has dimensions of '',3I6)')
     .   NX,NY,NZ
       write(6,'('' New file has Min,Max,Mean of '',
     .   3F12.3)')DMIN,DMAX,DMEAN
C
       CALL IALCEL(2,CELL)
       CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
       GOTO 990
C
C=====================================================================
C
C  MODE 35 : Computer relative ice layer intensity
C
121   continue
      write(TITLE,'(''LABELH Mode 35: Computer relative '',
     .   ''ice layer instensity'')')
C
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      CALL IRTLAB(1,LABELS,NL)
      CALL IRTEXT(1,EXTRA,1,29)
      CALL IRTCEL(1,CELL)
C
      if(MODE.ne.3 .and. MODE.ne.4)then
        write(6,'(''::ERROR: Only works on Fourier transforms'')')
        goto 999
      endif
C
      write(*,'('' Opened file has dimensions '',2I6)')NX,NY
C
      CALL IALCEL(2,CELL)
      CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
      write(6,'(/,''Give pixel size in Angstrom'')')
      READ(5,*) rpixel
      write(6,'(''   Read: '',F12.3)')rpixel
C
      write(6,'(/,''Give resolution range low resolution'')')
      READ(5,*) rrange1l,rrange1h
      if(rrange1l.lt.rrange1h)then
        rtmp = rrange1l
        rrange1l = rrange1h
        rrange1h = rtmp
      endif
      write(6,'(''   Read: '',2F12.3)')rrange1l,rrange1h
C
      write(6,'(/,''Give resolution range 1st ice ring'')')
      READ(5,*) rrange2l,rrange2h
      if(rrange2l.lt.rrange2h)then
        rtmp = rrange2l
        rrange2l = rrange2h
        rrange2h = rtmp
      endif
      write(6,'(''   Read: '',2F12.3)')rrange2l,rrange2h
C
      write(6,'(/,''Give resolution range 2nd ice ring'')')
      READ(5,*) rrange3l,rrange3h
      if(rrange3l.lt.rrange3h)then
        rtmp = rrange3l
        rrange3l = rrange3h
        rrange3h = rtmp
      endif
      write(6,'(''   Read: '',2F12.3)')rrange3l,rrange3h
C
      WRITE(6,'(/,''Output filename:  '')')
      READ(5,'(A)') COUTFILE
      call shorten(COUTFILE,k)
      write(6,'(''   Read: '',A)')COUTFILE(1:k)
C
      WRITE(6,'(/,''Debug Mode: y or n'')')
      read(5,*)CDEBUG
      write(6,'(''    Read: '',A1)')CDEBUG
C
      if(CDEBUG.eq.'y')then
        LDEBUG=.true.
      else
        LDEBUG=.false.
      endif
C
      DOMIN =  1.E10
      DOMAX = -1.E10
      DOOUBLMEAN = 0.0
C
C-----Mean intensity in 200 ... 5.0A
      DOUBLMEAN1 = 0.0
      ICOUNT1 = 0
C-----mean intensity in 4.0 ... 3.5A
      DOUBLMEAN2  = 0.0
      ICOUNT2 = 0
C-----mean intensity in 4.0 ... 3.5A
      DOUBLMEAN3  = 0.0
      ICOUNT3 = 0
C
      rval1 = (DMEAN - DMIN) * 0.2 + DMIN
      rval2 = (DMEAN - DMIN) * 0.2 + DMIN
      rval3 = (DMEAN - DMIN) * 0.2 + DMIN
C
      IZ = 0
      NX2  = NX * NX
      NY24 = NY * NY / 4
      DO IY = 1,NY
        CALL IRDLIN(1,ALINE,*999)
        IIY = IY - (NY / 2)
        DO IX = 1,NX
          rrad=sqrt(real(IX*IX)/NX2+real(IIY*IIY)/NY24)
          rres = 2*rpixel/rrad
C
          if(LDEBUG)then
            write(*,'(''NX,NY,IX,IIY,rrad,rres'',
     .        4I6,2F12.3)')NX,NY,IX,IIY,rrad,rres
          endif
C
          VAL = CABS(CLINE(IX))
          IRANGE=0
          if(rres.lt.rrange1l .and. rres.gt.rrange1h)then
            DOUBLMEAN1 = DOUBLMEAN1 + VAL
            ICOUNT1 = ICOUNT1 + 1
            IRANGE=1
          endif
          if(rres.lt.rrange2l .and. rres.gt.rrange2h)then
            DOUBLMEAN2 = DOUBLMEAN2 + VAL
            ICOUNT2 = ICOUNT2 + 1
            IRANGE=2
          endif
          if(rres.lt.rrange3l .and. rres.gt.rrange3h)then
            DOUBLMEAN3 = DOUBLMEAN3 + VAL
            ICOUNT3 = ICOUNT3 + 1
            IRANGE=3
          endif
C
          if(IRANGE.eq.0)then
            ISTRIPE=MOD(NX+NY+IX-IY,100)
            if(ISTRIPE.lt.50)then
              CLINE(IX)=CLINE(IX)*0.4
            else
              CLINE(IX)=CLINE(IX)*0.6
            endif
          endif
C
          if(VAL.lt.DOMIN)DOMIN=VAL
          if(VAL.gt.DOMAX)DOMAX=VAL
C
         enddo
         CALL IWRLIN(2,ALINE)
       enddo
C
       DOMEAN = DOOUBLMEAN/(NY*NX)
C
       write(*,'(''Updating labels'')')
       CALL IALLAB(2,LABELS,NL)
       write(*,'(''Writing new header. DMIN,DMAX,DMEAN = '',3G16.8)')
     .     DOMIN,DOMAX,DOMEAN
       write(TITLE,'(''LABELH Mode 35: Changing labels'')')
       CALL IWRHDR(2,TITLE,-1,DOMIN,DOMAX,DOMEAN)
C
       write(*,'(''Found '',I10,'' pixels in 1st range.'')')ICOUNT1
       write(*,'(''Found '',I10,'' pixels in 2nd range.'')')ICOUNT2
       write(*,'(''Found '',I10,'' pixels in 3rd range.'')')ICOUNT3
       if(ICOUNT1.gt.0)then
         DMEAN1 = DOUBLMEAN1/ICOUNT1
       endif
       if(ICOUNT2.gt.0)then
         DMEAN2 = DOUBLMEAN2/ICOUNT2
       endif
       if(ICOUNT3.gt.0)then
         DMEAN3 = DOUBLMEAN3/ICOUNT3
       endif
       if(ABS(DMEAN1).gt.0.001)then
         RRATIO1 = DMEAN2 / DMEAN1
         RRATIO2 = DMEAN3 / DMEAN1
         write(6,'(''::Relative 1st ice ring intensity: '',F16.6)')RRATIO1
         write(6,'(''::Relative 2nd ice ring intensity: '',F16.6)')RRATIO2
         call shorten(COUTFILE,k1)
         write(cstring,'(''\rm -f '',A)')COUTFILE(1:k1)
         call shorten(cstring,k)
         call system(cstring(1:k))
         open(11,FILE=COUTFILE(1:k1),STATUS="NEW")
         write(11,'(F16.6)')RRATIO1
         write(11,'(F16.6)')RRATIO2
         close(11)
       else
         write(6,'(''ERROR: No counts in lowest resolution range'')')
       endif
C
       GOTO 990
C
C=====================================================================
C
C  MODE -3 :  CHANGING OUTPUT DATA FORMAT REAL/BYTE with autom. scaling
C
113     IF (MODE.LT.0.OR.MODE.GT.4) GO TO 997
        MODE = 0
C
        CALL IALMOD(2,MODE)
C
        ROFFSET = DMIN
        RSCALE  = 255.0/(DMAX-DMIN)
        write(6,'('' Automatic Scaling: '')')
        write(6,'('' OFFSET = '',F12.3)')ROFFSET
        write(6,'('' SCALE  = '',F12.3)')RSCALE
C
C       DMIN = (DMIN-ROFFSET)*RSCALE
C       DMAX = (DMAX-ROFFSET)*RSCALE
C       IF (DMAX.GT.255.0) DMAX=255.
C       IF (DMIN.LT.0.0) DMIN=0.
C
        DMIN=0.0
        DMAX=255.0
C
        DMEAN = (DMEAN-ROFFSET)*RSCALE
C
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN) 
C
        NTRUNC = 0
        DO IZ = 1,NZ
          DO IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            IF (MODE.EQ.1.OR.MODE.EQ.3) THEN
              DO IX = 1,NX
                ATEMP = (ALINE(IX)-ROFFSET)*RSCALE
                IF (ATEMP.GT.255.) THEN
                  ATEMP=255.
                  NTRUNC=NTRUNC+1
                ENDIF
                IF (ATEMP.LT.0.) THEN
                  ATEMP=0.
                  NTRUNC=NTRUNC+1
                ENDIF
                ALINE(IX) = ATEMP
              enddo
            ELSE
              DO IX = 1,NX
                ALINE(IX) = (ALINE(IX)-ROFFSET)*RSCALE
              enddo
            ENDIF
            CALL IWRLIN(2,ALINE)
          enddo
        enddo
        IF (NTRUNC.NE.0) WRITE(6,127)NTRUNC
127     FORMAT(//////' *********************************************',
     . ' WARNING ********************************************'//
     . ' in real to byte conversion',I10,' numbers were truncated',
     . ' to be 0...255'//////)
        GOTO 990
C
C=====================================================================
C
C  MODE -2 :  CHANGING OUTPUT DATA FORMAT INTEGER*1/INTEGER*2
C
3       IF (MODE.LT.0.OR.MODE.GT.1) GO TO 997
        IF (MODE .EQ. 1) LMODE = 0
        IF (MODE .EQ. 0) LMODE = 1
        MODE = LMODE
        SCALE = 1.0
        IF (MODE .EQ. 0) THEN
          WRITE(6,1625)
1625      FORMAT(/,'$Scale factor for INTEGER*2/BYTE conversion [1]: ')
          READ(5,*) SCALE
        END IF
C
        IF(SCALE .gt. 1.0) SCALE = 1.0
        CALL IALMOD(2,MODE)
        DMIN = DMIN*SCALE
        DMAX = DMAX*SCALE
        DMEAN = DMEAN*SCALE
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
C
        DO IZ = 1,NZ
          DO IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            DO 1790 IX = 1,NX
               ALINE(IX) = SCALE*ALINE(IX)
1790          CONTINUE
            CALL IWRLIN(2,ALINE)
          enddo
        enddo
        GOTO 990
C
C=====================================================================
C
C  MODE -1 :  CHANGING OUTPUT DATA FORMAT
C
5       IF (MODE.LT.1.OR.MODE.GT.4) GO TO 997
        IF (MODE .EQ. 1) LMODE = 2
        IF (MODE .EQ. 2) LMODE = 1
        IF (MODE .EQ. 3) LMODE = 4
        IF (MODE .EQ. 4) LMODE = 3
        MODE = LMODE
        SCALE = 1.0
        IF (MODE .EQ. 1 .OR. MODE .EQ. 3) THEN
          WRITE(6,1650)
1650      FORMAT(/,'$Scale factor for REAL/INTEGER conversion [1]: ')
          READ(5,*) SCALE
        END IF
C
        CALL IALMOD(2,MODE)
        DMIN = DMIN*SCALE
        DMAX = DMAX*SCALE
        IF (MODE.EQ.1.OR.MODE.EQ.3) THEN
                IF (DMAX.GT.32000.0) DMAX=32000.
                IF (DMIN.LT.-32000.0) DMIN=-32000.
        ENDIF
        DMEAN = DMEAN*SCALE
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN) 
C
        NTRUNC = 0
        DO IZ = 1,NZ
          DO IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            IF (MODE.EQ.1.OR.MODE.EQ.3) THEN
              DO 100 IX = 1,NX
                ATEMP = SCALE*ALINE(IX)
                IF (ATEMP.GT.32000.) THEN       ! check for overflows
                        ATEMP=32000.
                        NTRUNC=NTRUNC+1
                ENDIF
                IF (ATEMP.LT.-32000.) THEN      ! and underflows
                        ATEMP=-32000.
                        NTRUNC=NTRUNC+1
                ENDIF
                ALINE(IX) = ATEMP
100           CONTINUE
            ELSE
              DO 101 IX = 1,NX
                ALINE(IX) = SCALE*ALINE(IX)
101           CONTINUE
            ENDIF
            CALL IWRLIN(2,ALINE)
          enddo
        enddo
        IF (NTRUNC.NE.0) WRITE(6,126)NTRUNC
126     FORMAT(//////' *********************************************',
     . ' WARNING ********************************************'//
     . ' in real to integer conversion',I10,' numbers were truncated',
     . ' to be +/-32000'//////)
        GOTO 990
C
C=====================================================================
C
C  MODE 0 : CHANGE LABELS
C
10      continue
        CALL IRTLAB(1,LABELS,NL)
C
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        DO IZ = 1,NZ
          DO IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            CALL IWRLIN(2,ALINE)
          enddo
        enddo
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
C
        DO 150 J = 1,NL
          WRITE(6,1675) J,(LABELS(K,J),K=1,18)
150     CONTINUE
1675    FORMAT(I3,2X,18A4)
        WRITE(6,1680)
1680    FORMAT(/,'$Number of labels to ADD (can also be negative) ? ')
        READ(5,*) NLA
        IF (NLA .GT. 0) THEN
          NL = MIN(10,NL+NLA)
          LST = NL - NLA + 1
          DO 160 J=LST,NL
            WRITE(6,1690) J
            READ(5,1700) TITLE
C       Following statement changed for Alliant
            CALL CCPMVI(LABELS(1,J),ITITLE,20)
160       CONTINUE
1690      FORMAT(' Enter label # ',I3)
1700      FORMAT(20A4)
        ELSE
12        WRITE(6,1710)
1710      FORMAT(/,'$Remove label # (0 to exit) ? ')
          READ(5,*) NLR
          IF (NLR .LE. 0) GOTO 15
          IF (NLR .GT. NL) GOTO 12
          NL = NL - 1
          DO 170 J = NLR,NL
C       Following statement changed for Alliant
            CALL CCPMVI(LABELS(1,J),LABELS(1,J+1),20)
170       CONTINUE
          DO 180 J = 1,NL
            WRITE(6,1675) J,(LABELS(K,J),K=1,18)
180       CONTINUE
          GOTO 12
        END IF
15      continue
        write(*,'(''Updating labels'')')
        CALL IALLAB(2,LABELS,NL)
        write(*,'(''Writing new header. DMIN,DMAX,DMEAN = '',3G16.8)')
     .     DMIN,DMAX,DMEAN
        write(TITLE,'(''LABELH Mode 0: Changing labels'')')
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        write(*,'(''Done.'')')
        GOTO 995
C
C=====================================================================
C
C  MODE 1 : SELECTING A REGION
C
20      CALL ICLLIM(1,IXYZMIN,IXYZMAX,NXYZ)
C
        write(TITLE,'(''LABEL Mode 1: Min/Max XYZ = '',6I6)') 
     1  IXMIN,IXMAX,IYMIN,IYMAX,IZMIN,IZMAX
C
        IQ = 1
C       WRITE(6,1775)
C1775   FORMAT('$Use "true" starting limits (0) or start at 0 (1) ? ')
C       READ(5,*) IQ
        DO 200 J = 1,3
          NXYZST(J) = 0
          IF (IQ .EQ. 0) NXYZST(J) = IXYZMIN(J)
200     CONTINUE
C
        CALL IALSIZ(2,NXYZ,NXYZST)
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        DMIN =  1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
        DO IZ = IZMIN,IZMAX
          CALL IMPOSN(1,IZ,IYMIN)
          DO IY = 1,NY
            CALL IRDPAL(1,ALINE,IXMIN,IXMAX,*999)
            CALL IWRLIN(2,ALINE)
            DO IX = 1,NX
              IF (MODE .LT. 3) THEN
                VAL = ALINE(IX)
              ELSE
                VAL = CABS(CLINE(IX))
              END IF
              DOUBLMEAN = DOUBLMEAN + VAL
              IF (VAL .LT. DMIN) DMIN = VAL
              IF (VAL .GT. DMAX) DMAX = VAL
            enddo
          enddo
        enddo
C
        DMEAN = DOUBLMEAN/(NX*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C=====================================================================
C
C  MODE 2 : LINEAR OD STRETCH
C
30      WRITE(6,1800)
1800    FORMAT(/,'$Enter coeffecients A & B ( Y = AX + B )  ')
        READ(5,*) A,B
        WRITE(6,'(''A = '',G12.4,'' B = '',G12.4)') A,B
        WRITE(6,1850)
1850    FORMAT('$Set values <0 to 0  (0= no, 1=yes) ? ')
        READ(5,*) IQ
        WRITE(6,'(''IQ = '',I6)') IQ
        IF (IQ .EQ. 0) then
          write(TITLE,'(''LABEL Mode 2: Linear Stretch A,B = '',
     .    2G12.4)')A,B
        endif
        IF (IQ .EQ. 1) then
          write(TITLE,'(''LABEL Mode 2: Linear Stretch,Zero '',
     .    ''truncation  '',
     1    ''A,B = '',2G12.4)')
     2    A,B
        endif
        DMIN =  1.E10
        DMAX = -1.E10
        domin = 1.E10
        domax =-1.E10
        DOUBLMEAN = 0.0
C
        NXT = NX
        MXT = MX
        IF (MODE .GE. 3) then
          NXT = NXT*2
          MXT = MXT*2
        endif
        CALL IRTLAB(1,LABELS,NL)
        CALL IRTEXT(1,EXTRA,1,29)
        CALL IRTCEL(1,CELL)
C         CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
        CALL IALEXT(2,EXTRA,1,29)
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        if(MODE.lt.3)then
C
          DO IZ= 1,NZ
            DO IY = 1,NY
              CALL IRDLIN(1,ALINE,*999)
              DO IX = 1,NXT
                if(ALINE(IX) .lt. domin) domin = ALINE(IX)
                if(ALINE(IX) .gt. domax) domax = ALINE(IX)
                VAL = ALINE(IX)*A + B
                IF (IQ .EQ. 1 .AND. VAL .LT. 0.0) VAL = 0.0
                IF (VAL .LT. DMIN) DMIN = VAL
                IF (VAL .GT. DMAX) DMAX = VAL
                DOUBLMEAN = DOUBLMEAN + VAL
                ALINE(IX) = VAL
              enddo
              CALL IWRLIN(2,ALINE)
            enddo
          enddo
C
        else
C
          DO IZ= 1,NZ
            DO IY = 1,NY
              CALL IRDLIN(1,ALINE,*999)
              IYTRUE=IY-NY2
              DO IX = 1,NXT,2
                PHSORG = (-1.0)**((IX/2)+1+IYTRUE)
                RAMP = SQRT(ALINE(IX)**2+ALINE(IX+1)**2)
                RPHA = ATAN2(ALINE(IX+1),ALINE(IX))*CNV
                VAL = RAMP*A + B
                IF (IQ .EQ. 1 .AND. VAL .LT. 0.0) VAL = 0.0
                IF (VAL .LT. DMIN) DMIN = VAL
                IF (VAL .GT. DMAX) DMAX = VAL
                DOUBLMEAN = DOUBLMEAN + VAL
                ALINE(IX  ) = VAL * COS(RPHA) * PHSORG
                ALINE(IX+1) = VAL * SIN(RPHA) * PHSORG
C                write(*,'(2I8,2F12.3)')IX,IY,ALINE(IX),ALINE(IX+1)
              enddo
              CALL IWRLIN(2,ALINE)
            enddo
          enddo
C
        endif
        DMEAN = DOUBLMEAN/(NXT*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        write(6,'('' old file extrema: '',2G12.4)')domin,domax
        GOTO 990
C
C=====================================================================
C
C  MODE 3 : LOG OD STRETCH
C
40      WRITE(6,2000)
2000    FORMAT(' Enter coeffecients A & B ( Y = A*ALOG(X) + B )')
        READ(5,*) A,B
        write(TITLE,2100)A,B
2100    FORMAT('LABEL Mode 3: Logarithmic Stretch A,B = ',2G12.4)
CHEN
        WRITE(6,'(A70)') TITLE
CHEN
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
        DMIN = 1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
C
        NOVER=0
        NUNDER=0
        DO IZ= 1,NZ
          DO IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            DO 400 IX = 1,NXT
              VAL = ALINE(IX)
              ABSVAL = ABS(VAL)
              IF (ABSVAL .LT. 1.E-5) ABSVAL=1.
CHEN
C-------------VAL = A*SIGN(ALOG10(ABSVAL),VAL) + B
              VAL = A*ALOG10(ABSVAL) + B
CHEN
              IF(MODE.EQ.0) THEN
                IF(VAL.GT.255.0) THEN
                        VAL=255.0
                        NOVER=NOVER+1
                ENDIF
                IF(VAL.LT.0.0) THEN
                        VAL=0.0
                        NUNDER=NUNDER+1
                ENDIF
              ENDIF
              ALINE(IX) = VAL
              IF (VAL .LT. DMIN) DMIN = VAL
              IF (VAL .GT. DMAX) DMAX = VAL
              DOUBLMEAN = DOUBLMEAN + VAL
400         CONTINUE
            CALL IWRLIN(2,ALINE)
          enddo
        enddo
        DMEAN = DOUBLMEAN/(NXT*NY*NZ)
        WRITE(6,451) NOVER,NUNDER,DMIN,DMAX,DMEAN
451     FORMAT(' There were ',I10,' densities reduced to 255',
     .        '        and ',I10,' densities increased to 0',
     .        ' DMIN,DMAX,DMEAN ',3F10.1)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C=====================================================================
C
C  MODE 4 : PIXEL AVERAGING
C
45      WRITE(6,2200)
        DMIN = 1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
2200    FORMAT('$Enter integer reduction factor for X,Y: ')
        READ(5,*) NREDX,NREDY
        print *,'Label Mode 4: Pixel averaging factor for X,Y = ', NREDX, NREDY
c       write(TITLE,2300)NREDX,NREDY
c2300   FORMAT('LABEL Mode 4: Pixel averaging factor for X,Y = ',2I4)
        NX = NX/NREDX
        NY = NY/NREDY
C  Put title labels, new cell and extra information only into header
        CALL IRTLAB(1,LABELS,NL)
        CALL IRTEXT(1,EXTRA,1,29)
        CALL IRTCEL(1,CELL)
        CELL(1) = CELL(1)*NX*NREDX/MXYZ(1)
        CELL(2) = CELL(2)*NY*NREDY/MXYZ(2)
        CALL ICRHDR(2,NXYZ,NXYZ,MODE,LABELS,NL)
        CALL IALEXT(2,EXTRA,1,29)
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        NX4 = NX*4
        SCL = 1./(NREDX*NREDY)
        IF (MODE .GE. 3) NX4 = NX4*2
        DO 475 IZ = 1,NZ
          CALL IMPOSN(1,IZ-1,0)
          DO 475 IY = 1,NY
            CALL ZERO(OUT,NX4)
            IF (MODE .LE. 2) THEN
              DO JY = 1,NREDY
                CALL IRDLIN(1,ALINE,*999)
                INDEX = 0
                DO IX = 1,NX
                  DO JX = 1,NREDX           
                    INDEX = INDEX + 1
                    OUT(IX) = OUT(IX) + ALINE(INDEX)*SCL
                  enddo
                enddo
              enddo
              DO IX=1,NX
                VAL=OUT(IX)
                IF (VAL .LT. DMIN) DMIN = VAL
                IF (VAL .GT. DMAX) DMAX = VAL
                DOUBLMEAN = DOUBLMEAN + VAL
              enddo
            ELSE
              DO JY = 1,NREDY
                CALL IRDLIN(1,ALINE,*999)
                INDEX = 0
                DO IX = 1,NX
                  DO JX = 1,NREDX           
                    INDEX = INDEX + 1
                    COUT(IX) = COUT(IX) + CLINE(INDEX)*SCL
                  enddo
                enddo
              enddo
              DO IX=1,NX
                VAL=OUT(IX)
                IF (VAL .LT. DMIN) DMIN = VAL
                IF (VAL .GT. DMAX) DMAX = VAL
                DOUBLMEAN = DOUBLMEAN + VAL
              enddo
            END IF
            CALL IWRLIN(2,OUT)
475     CONTINUE
        DMEAN = DOUBLMEAN/(NX*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C=====================================================================
C
C  MODE 5 : GETTING AMPLITUES OR INTENSITIES
C
50      CALL IALMOD(2,2)
        WRITE(6,2350)
2350    FORMAT('$Write out amplitudes (0) or Intensities*.01 (1) ? ')
        READ(5,*) IQ
        IF (IQ .EQ. 0) THEN
          write(TITLE,2400)
        ELSE
          write(TITLE,2450)
        ENDIF
2400    FORMAT('LABEL Mode 5: Amplitudes selected')
2450    FORMAT('LABEL Mode 5: Intensities*.01 selected')
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        DMIN =  1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
        DO 550 IZ = 1,NZ
          DO 550 IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            IF (IQ .EQ. 0) THEN
              DO 500 IX = 1,NX
                VAL = CABS(CLINE(IX))
                ALINE(IX) = VAL
                DOUBLMEAN = DOUBLMEAN + VAL
                IF (VAL .LT. DMIN) DMIN = VAL
                IF (VAL .GT. DMAX) DMAX = VAL
500           CONTINUE
            ELSE
              IND = 1
              DO 525 IX = 1,NX
                VAL = .01*(ALINE(IND)**2 + ALINE(IND+1)**2)
                IND = IND + 2
                ALINE(IX) = VAL
                DOUBLMEAN = DOUBLMEAN + VAL
                IF (VAL .LT. DMIN) DMIN = VAL
                IF (VAL .GT. DMAX) DMAX = VAL
525           CONTINUE
            ENDIF
            CALL IWRLIN(2,ALINE)
550     CONTINUE
        DMEAN = DOUBLMEAN/(NX*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C=====================================================================
C
C  MODE 6 : GETTING PHASES  
C
60      CALL IALMOD(2,2)
        write(TITLE,2500)
2500    FORMAT('LABEL Mode 6: Select phases from Fourier Transform')
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        DMIN =  1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
        DO 650 IZ = 1,NZ
          DO 650 IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            DO 600 IX = 1,NX
              A = REAL(CLINE(IX))
              B = AIMAG(CLINE(IX))
              PHASE = ATAN2(B,A)*CNV
              ALINE(IX) = PHASE
              DOUBLMEAN = DOUBLMEAN + VAL
              IF (VAL .LT. DMIN) DMIN = PHASE
              IF (VAL .GT. DMAX) DMAX = PHASE
600         CONTINUE
            CALL IWRLIN(2,ALINE)
650     CONTINUE
        DMEAN = DOUBLMEAN/(NX*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C=====================================================================
C
C  MODE 7 : GETTING REAL PART
C
70      CALL IALMOD(2,2)
        write(TITLE,2600)
2600    FORMAT('LABEL Mode 7: Select real part from Fourier Transform')
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        DMIN =  1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
        DO 750 IZ = 1,NZ
          DO 750 IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            DO 700 IX = 1,NX
              VAL = REAL(CLINE(IX))
              ALINE(IX) = VAL
              DOUBLMEAN = DOUBLMEAN + VAL
              IF (VAL .LT. DMIN) DMIN = VAL
              IF (VAL .GT. DMAX) DMAX = VAL
700         CONTINUE
            CALL IWRLIN(2,ALINE)
750     CONTINUE
        DMEAN = DOUBLMEAN/(NX*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C=====================================================================
C
C  MODE 8 : GETTING IMAGINARY PART
C
80      CALL IALMOD(2,2)
        write(TITLE,2700)
2700    FORMAT('LABEL Mode 8: Select imaginary part from',
     . ' Fourier Transform')
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        DMIN =  1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
        DO 850 IZ = 1,NZ
          DO 850 IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            DO 800 IX = 1,NX
              VAL = AIMAG(CLINE(IX))
              ALINE(IX) = VAL
              DOUBLMEAN = DOUBLMEAN + VAL
              IF (VAL .LT. DMIN) DMIN = VAL
              IF (VAL .GT. DMAX) DMAX = VAL
800         CONTINUE
            CALL IWRLIN(2,ALINE)
850     CONTINUE
        DMEAN = DOUBLMEAN/(NX*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C=====================================================================
C
C  MODE 9 : Fill FFT with Amplitude
C
90      continue
        WRITE(6,2359)
2359    FORMAT('$Give Value for Amplitude  ')
        READ(5,*) RVAL
        write(TITLE,2701)
2701    FORMAT('LABEL Mode 9: Create Zeroed version of',
     . ' Fourier Transform')
        NX=NX*2
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        APART = RVAL
        BPART = 0.0
C
        DMIN = RVAL
        DMAX = RVAL
        DMEAN = RVAL
C
        NX2 = NX/2
        NY2 = NY/2
C
        DO 852 IZ = 1,NZ
          DO 852 IY = 1,NY
            IYTRUE=IY-NY2
            DO 802 IX = 1,NX2
              PHSORG = (-1.0)**(IX+IYTRUE)
C              PHSORG = 1.0
              ALINE((2*IX)-1)=PHSORG*APART
              ALINE((2*IX)  )=PHSORG*BPART
C              write(*,'(2I6,2F12.3)')IX,IY,ALINE(2*IX-1),ALINE(2*IX)
802         CONTINUE
            CALL IWRLIN(2,ALINE)
852     CONTINUE
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C=====================================================================
C
C  MODE 32 : Cut out over and underflows from FFTs Amplitude
C
109     continue
        WRITE(6,2032)
2032    FORMAT(' Enter LOWEST AND HIGHEST VALUE TO PRUNE WITH')
        READ(5,*) A,B
        WRITE(TITLE,2132) A,B
2132    FORMAT(' LABEL Mode 32: LIMIT DYNAMIC RANGE TO ',2F12.4)
        WRITE(6,2359)
C
        CALL IALCEL(2,CELL)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        if(MODE.lt.3)then
          write(*,'('':: ERROR: This mode only for FFTs'')')
          goto 990
        endif
C
        DMIN = 0.0
        DMAX = 0.0
        DO IZ= 1,NZ
          DO IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            DO IX = 1,NXT,2
              RAMP = SQRT(ALINE(IX)**2+ALINE(IX+1)**2)
              RPHA = ATAN2(ALINE(IX+1),ALINE(IX))*CNV
              RREN=RAMP
              IF (RREN.LT.A) RREN=A
              IF (RREN.GT.B) RREN=B
              ALINE(IX  )=RREN*COS(RPHA)
              ALINE(IX+1)=RREN*SIN(RPHA)
              IF (RREN.LT.DMIN) DMIN = RREN
              IF (RREN.GT.DMAX) DMAX = RREN
              DOUBLMEAN = DOUBLMEAN + RREN
            ENDDO
            CALL IWRLIN(2,ALINE)
          enddo
        enddo
        DMEAN = 2.0*DOUBLMEAN/(NXT*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C=====================================================================
C=====================================================================
C=====================================================================
C   HERE FOR FINISH
C=====================================================================
C=====================================================================
C=====================================================================
C
990     CALL IMCLOSE(2)
995     CALL IMCLOSE(1)
        CALL EXIT
997     WRITE(6,998)
998     FORMAT(' THIS OPTION NOT COMPATIBLE WITH INPUT FILE MODE')
        STOP
999     STOP 'END-OF-FILE ERROR ON READ'
        END

C*LABELB.FOR*************************************************************
C                                                                       *
C       Program to perform simple manipulations on image files          *
C                                                                       *
C       LABEL Version 1.12      31-MAR-82    DAA        for VAX         *
C       LABEL Version 1.13      13-OCT-82    DAA        for VAX         *
C       LABELB Version 1.1      7- JAN-85    MK   some more routines    *
C       LABELB Version 1.2      20-APR-85    RH   bigger line length    *
C************************************************************************
C
      SUBROUTINE LABELB(INFILE,APIC,BPIC)
      PARAMETER (LMAX=16384,LCMX=8192,LPIC=16384)
      PARAMETER (IBMX=256)
      COMMON //NX,NY,NZ,IXMIN,IYMIN,IZMIN,IXMAX,IYMAX,IZMAX
      DIMENSION APIC(LPIC,LPIC),BPIC(LPIC,LPIC)
        DIMENSION ALINE(LMAX),NXYZ(3),MXYZ(3),NXYZST(3)
        DIMENSION IXYZMIN(3),IXYZMAX(3),OUT(LMAX)
c       DIMENSION LABELS(20,10),CELL(6)
        DIMENSION LABELS(20,10)
        COMPLEX CLINE(LCMX),COUT(LCMX)
        REAL*8 DOUBLMEAN
        CHARACTER*200 INFILE,OUTFILE
        character*80 TITLE
        EQUIVALENCE (NX,NXYZ), (ALINE,CLINE), (OUT,COUT)
        EQUIVALENCE (IXYZMIN, IXMIN), (IXYZMAX, IXMAX)
        DATA NXYZST/3*0/, CNV/57.29578/
C
1000    FORMAT(//' LABELB: Image Manipulation Program  V1.2'/)
1011    format(' these are some more options to manipulate image files
     1  '/' especially for nice diplays. You can turn them around in 
     1  '/' 90 degree steps, cut off outliers, repair scratches, or
     1  '/' do geometrical stretch (square root, e.g.).') 
C       WRITE(6,1100)
c1100   FORMAT('$Input filename:  ')
c       READ(5,1200) INFILE
1200    FORMAT(A)

        CALL IMOPEN(1,INFILE,'RO')
C
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C
        NXT = NX
        IF (MODE .GE. 3) NXT = NXT*2
        write(6,1000)
        write(6,1011)
C
1       WRITE(6,1300)
1300    FORMAT(/,' Available modes of operation are: ',/,
     . '  1: VARIOUS 90 DEG TURNS AND MIRRORS',/,
     . '  2: GEOMETRIC STRETCH ( y = m**X )',/,
     . '  3: CUT OFF OVER - AND UNDERFLOWS',/,
     . '  4: GET RID OF OUTLIERS BY INTERPOLATION')
        IF (MODE .GE. 3) WRITE(6,1400)
1400    FORMAT(
     . '  5: Output amplitudes or Intensities',/,
     . '  6: Output Phases (degrees)',/,
     . '  7: Output REAL part of Complex value',/,
     . '  8: Output IMAGINARY part of Complex value',/)
C
        WRITE(6,1500)
1500    FORMAT(/,'$Enter desired mode: ')
        READ(5,*) IMODE
        IF (IMODE .GT. 4 .AND. MODE .LT. 3) GOTO 1
        WRITE(6,1550)
1550    FORMAT(10X)
C
        IF (IMODE .NE. 0) THEN
          WRITE(6,1600)
1600      FORMAT('$Output filename:  ')
          READ(5,1200) OUTFILE

        IF (IMODE.NE.1) THEN
          CALL IMOPEN(2,OUTFILE,'NEW')
          CALL ITRHDR(2,1)
        END IF
        END IF

        GOTO (5,10,20,30,40,45,50,60,70,80) IMODE+2

5       Continue
        GOTO 990
10      Continue
        GOTO 995
C
C  MODE 1 : VARIOUS 90 DEG TURNS
C
20      CONTINUE
        IF(NXYZ(1).GT.LPIC.OR.NXYZ(2).GT.LPIC) THEN
                WRITE(6,201) IBMX, NXYZ(1), NXYZ(2)
201             FORMAT(' CANNOT DO TURNS OR MIRRORS IF IMAGE SIZE',
     .                  ' . GT.',I7,', NX AND NY ARE',2I7)
                GO TO 1
        ENDIF
C
        WRITE(6,287)
        READ(5,*)ITURN
287     FORMAT('$TURN NO? (1:Z90,2:Z-90,3:Z180,4:Xmir,5:Ymir)')
        write(TITLE,1750) ITURN
1750    FORMAT(' MLABEL  Mode 1: TURN NO ',I1,
     . '   (1:Z90,2:Z-90,3:Z180,4:X180,5:Y180)')
200     CONTINUE
C
        IF (ITURN.EQ.1.OR.ITURN.EQ.2) THEN
        NXYZST(1) = NXYZ(2)
        NXYZST(2) = NXYZ(1)
        NXYZST(3) = NXYZ(3)
        ELSE
        NXYZST(1) = NXYZ(1)
        NXYZST(2) = NXYZ(2)
        NXYZST(3) = NXYZ(3)
        ENDIF

        CALL IMOPEN(2,OUTFILE,'NEW')
        CALL ICRHDR(2,NXYZST,NXYZST,2,LABELS,10)
        CALL ITRLAB(2,1)
C       the cell parameters are not transformed or transfered, because this
C       operation was intended just for beautifying pictures
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
C       READ THE WHOLE PICTURE INTO APIC. IT HAS TO BE ONE LAYER ONLY
C       AND SMALLER THAN THE DIMENSIONS OF APIC.
        DO IZ = 1,NXYZ(3) !FOR EACH SECTION SEPERATELY

        DO IY = 1,NXYZ(2)       
            CALL IRDLIN(1,APIC(1,IY),*999)
        ENDDO
C       NOW TRANSFORM THIS IN SUITABLE MANNER

        IF (ITURN.EQ.5) THEN
        DO IY = 1,NXYZ(2)
                DO IX = 1,NXYZ(1)
                        BPIC(NXYZ(1)-IX+1,IY)=APIC(IX,IY)
                ENDDO
        ENDDO
        ENDIF

        IF (ITURN.EQ.4) THEN
        DO IX = 1,NXYZ(1)
                DO IY = 1,NXYZ(2)
                        BPIC(IX,NXYZ(2)-IY+1)=APIC(IX,IY)
                ENDDO
        ENDDO
        ENDIF


        IF (ITURN.EQ.1) THEN
        DO IX = 1,NXYZ(1)
                DO IY = 1,NXYZ(2)
                        BPIC(IY,NXYZ(1)-IX+1)=APIC(IX,IY)
                ENDDO
        ENDDO
        ENDIF


        IF (ITURN.EQ.2) THEN
        DO IX = 1,NXYZ(1)
                DO IY = 1,NXYZ(2)
                        BPIC(NXYZ(2)-IY+1,IX)=APIC(IX,IY)
                ENDDO
        ENDDO
        ENDIF


        IF (ITURN.EQ.3) THEN
        DO IX = 1,NXYZ(1)
                DO IY = 1,NXYZ(2)
                        BPIC(NXYZ(1)-IX+1,NXYZ(2)-IY+1)=APIC(IX,IY)
                ENDDO
        ENDDO
        ENDIF


        DO IY = 1,NXYZST(2)
                CALL IWRLIN(2,BPIC(1,IY))
        ENDDO

        ENDDO !IZ
C
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C  MODE 2 : GEOMETRIC STRETCH
C
30      WRITE(6,1800)
1800    FORMAT(/,'$Enter A (Y = X**A) ')
        READ(5,*) A
        WRITE(TITLE,1900) A
1900    FORMAT(' MLABEL Mode 2: GEOMETRIC Stretch A,= ',F12.4)
        DMIN =  1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        DO IZ= 1,NZ
          DO IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
                IF (A.EQ.0.5) THEN  !SQRT
            DO 300 IX = 1,NXT
              VAL = SQRT(ALINE(IX))
              IF (VAL .LT. DMIN) DMIN = VAL
              IF (VAL .GT. DMAX) DMAX = VAL
              DOUBLMEAN = DOUBLMEAN + VAL
              ALINE(IX) = VAL
300         CONTINUE
                ELSE
            DO 392 IX = 1,NXT
              VAL = (ALINE(IX))**A
              IF (VAL .LT. DMIN) DMIN = VAL
              IF (VAL .GT. DMAX) DMAX = VAL
              DOUBLMEAN = DOUBLMEAN + VAL
              ALINE(IX) = VAL
392         CONTINUE
                ENDIF
            CALL IWRLIN(2,ALINE)
          enddo
        enddo
        DMEAN = DOUBLMEAN/(NXT*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C  MODE 3 : CUT OFF OVER AND UNDERFLOW
C
40      WRITE(6,2000)
2000    FORMAT(' Enter LOWEST AND HIGHEST VALUE TO PRUNE WITH')
        READ(5,*) A,B
        WRITE(TITLE,2100) A,B
2100    FORMAT(' MLABEL Mode 3: LIMIT DYNAMIC RANGE TO ',2F12.4)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
        DMIN = 1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
C
        DO IZ= 1,NZ
          DO IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            DO 400 IX = 1,NXT
              IF (ALINE(IX).LT.A) ALINE(IX)=A
              IF (ALINE(IX).GT.B) ALINE(IX)=B   
              IF (ALINE(IX) .LT. DMIN) DMIN = ALINE(IX)
              IF (ALINE(IX) .GT. DMAX) DMAX = ALINE(IX)
              DOUBLMEAN = DOUBLMEAN + ALINE(IX)
400         CONTINUE
            CALL IWRLIN(2,ALINE)
          enddo
        enddo
        DMEAN = DOUBLMEAN/(NXT*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C  MODE 4 : GET RID OF OUTLIERS BY INTERPOLATION
C     if in one line  pixel n - pixel(n-1) is opposite in sign to 
c     pixel n+1 - pixel n and greater than preset value, pixel n
c     is replaced by the average of n+1 and n-1. This should get
c     rid of vertical lines introduced through memory errrors in 
c     2-d pictures. 
45      WRITE(6,2200)
2200    FORMAT('$Enter critical difference ')
        READ(5,*) critdiff
        WRITE(TITLE,2300) critdiff
2300    FORMAT(' MLABEL Mode 4: get rid of outlier rows, critdiff= ',G10.4)
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)

        DMIN = 1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
C
        DO 454 IZ= 1,NZ
          DO 454 IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            DO 404 IX = 2,NXT-1
                DLEFT = ALINE(IX)-ALINE(IX-1)
                DRIGH = ALINE(IX+1)-ALINE(IX)
                IF (DRIGH.EQ.0.) DRIGH=1.E-10
              IF (ABS(DLEFT).GT.CRITDIFF.AND.DLEFT/DRIGH.LE.0.) 
     1        ALINE(IX)=(ALINE(IX+1)+ALINE(IX-1))/2.
              IF (ALINE(IX) .LT. DMIN) DMIN = ALINE(IX)
              IF (ALINE(IX) .GT. DMAX) DMAX = ALINE(IX)
              DOUBLMEAN = DOUBLMEAN + ALINE(IX)
404         CONTINUE
            CALL IWRLIN(2,ALINE)
454     CONTINUE
        DMEAN = DOUBLMEAN/(NXT*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C  MODE 5 : GETTING AMPLITUES OR INTENSITIES
C
50      CALL IALMOD(2,2)
        WRITE(6,2350)
2350    FORMAT('$Write out amplitudes (0) or Intensities*.01 (1) ? ')
        READ(5,*) IQ
        IF (IQ .EQ. 0) THEN
          WRITE(TITLE,2400)
        ELSE
          WRITE(TITLE,2450)
        ENDIF
2400    FORMAT(' LABEL Mode 5: Select amplitudes from Fourier')
2450    FORMAT(' LABEL Mode 5: Select Intensities *0.01 from Fourier ',
     . 'Transform')
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        DMIN =  1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
        DO 550 IZ = 1,NZ
          DO 550 IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            IF (IQ .EQ. 0) THEN
              DO 500 IX = 1,NX
                VAL = CABS(CLINE(IX))
                ALINE(IX) = VAL
                DOUBLMEAN = DOUBLMEAN + VAL
                IF (VAL .LT. DMIN) DMIN = VAL
                IF (VAL .GT. DMAX) DMAX = VAL
500           CONTINUE
            ELSE
              IND = 1
              DO 525 IX = 1,NX
                VAL = .01*(ALINE(IND)**2 + ALINE(IND+1)**2)
                IND = IND + 2
                ALINE(IX) = VAL
                DOUBLMEAN = DOUBLMEAN + VAL
                IF (VAL .LT. DMIN) DMIN = VAL
                IF (VAL .GT. DMAX) DMAX = VAL
525           CONTINUE
            ENDIF
            CALL IWRLIN(2,ALINE)
550     CONTINUE
        DMEAN = DOUBLMEAN/(NX*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C  MODE 6 : GETTING PHASES  
C
60      CALL IALMOD(2,2)
        WRITE(TITLE,2500)
2500    FORMAT(' LABEL Mode 6: Select phases from Fourier Transform')
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        DMIN =  1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
        DO 650 IZ = 1,NZ
          DO 650 IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            DO 600 IX = 1,NX
              A = REAL(CLINE(IX))
              B = AIMAG(CLINE(IX))
              PHASE = ATAN2(B,A)*CNV
              ALINE(IX) = PHASE
              DOUBLMEAN = DOUBLMEAN + VAL
              IF (VAL .LT. DMIN) DMIN = PHASE
              IF (VAL .GT. DMAX) DMAX = PHASE
600         CONTINUE
            CALL IWRLIN(2,ALINE)
650     CONTINUE
        DMEAN = DOUBLMEAN/(NX*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C  MODE 7 : GETTING REAL PART
C
70      CALL IALMOD(2,2)
        WRITE(TITLE,2600)
2600    FORMAT(' LABEL Mode 7: Select real part from Fourier Transform')
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        DMIN =  1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
        DO 750 IZ = 1,NZ
          DO 750 IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            DO 700 IX = 1,NX
              VAL = REAL(CLINE(IX))
              ALINE(IX) = VAL
              DOUBLMEAN = DOUBLMEAN + VAL
              IF (VAL .LT. DMIN) DMIN = VAL
              IF (VAL .GT. DMAX) DMAX = VAL
700         CONTINUE
            CALL IWRLIN(2,ALINE)
750     CONTINUE
        DMEAN = DOUBLMEAN/(NX*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C  MODE 8 : GETTING IMAGINARY PART
C
80      CALL IALMOD(2,2)
C       WRITE(TITLE,2400)
        WRITE(TITLE,2400)
2700    FORMAT(' LABEL Mode 8: Select imaginary part from',
     . ' Fourier Transform')
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        DMIN =  1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
        DO 850 IZ = 1,NZ
          DO 850 IY = 1,NY
            CALL IRDLIN(1,ALINE,*999)
            DO 800 IX = 1,NX
              VAL = AIMAG(CLINE(IX))
              ALINE(IX) = VAL
              DOUBLMEAN = DOUBLMEAN + VAL
              IF (VAL .LT. DMIN) DMIN = VAL
              IF (VAL .GT. DMAX) DMAX = VAL
800         CONTINUE
            CALL IWRLIN(2,ALINE)
850     CONTINUE
        DMEAN = DOUBLMEAN/(NX*NY*NZ)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
        GOTO 990
C
C   HERE FOR FINISH
C
990     CALL IMCLOSE(2)
995     CALL IMCLOSE(1)
        CALL EXIT
999     STOP 'END-OF-FILE ERROR ON READ'
        END

C*LABELC.FOR**************************************************************
C                                                                        *
C       Program to perform simple manipulations on image files           *
C       this subroutine selects many small areas and writes out a stack  *
C       of mini areas surrounding aech selected area (particle)          *
C                                                                        *
C   this subroutine edited from                                          *
C       LABEL Version 1.12      31-MAR-82    DAA        for VAX          *
C       LABEL Version 1.13      13-OCT-82    DAA        for VAX          *
C       LABELB Version 1.1      7- JAN-85    MK   some more routines     *
C       LABELB Version 1.2      20-APR-85    RH   bigger line length     *
C       LABELC Version 1.21     17-Dec-1996  RH   creation of subroutine *
C*************************************************************************
C
      SUBROUTINE LABELC(INFILE,APIC,BPIC)
      PARAMETER (LMAX=16384,LCMX=8192,LPIC=16384)
      PARAMETER (NDIM=150000)
      PARAMETER (NAREA=15000)
      COMMON //NX,NY,NZ,IXMIN,IYMIN,IZMIN,IXMAX,IYMAX,IZMAX
      DIMENSION APIC(LPIC,LPIC),BPIC(LPIC,LPIC)
        DIMENSION ARRAY(NDIM),NXYZ(3),MXYZ(3),NXYZST(3)
        DIMENSION NXYZBOX(3),NXYZTMP(3)
        DIMENSION IXYZMIN(3),IXYZMAX(3)
        DIMENSION LABELS(20,10)
        DIMENSION IAX(NAREA),IAY(NAREA)
        REAL*8 DOUBLMEAN
        CHARACTER DAT*24
        character*80 TITLE
        CHARACTER*200 INFILE,OUTFILE,DATAFILE,TITLELINE
        EQUIVALENCE (IXYZMIN, IXMIN), (IXYZMAX, IXMAX)
        DATA NXYZST/3*0/, CNV/57.29578/
C
1000    FORMAT(//' LABELC: Image Manipulation Program  V1.21'/)
1011    format(' selects many small areas and puts them into',
     . ' a file stack')
C
C       WRITE(6,1100)
c1100   FORMAT('$Input filename:  ')
c       READ(5,1200) INFILE
1200    FORMAT(A)

        CALL IMOPEN(1,INFILE,'RO')
C
        CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
        IF(MODE.LT.0.OR.MODE.GT.2) 
     . STOP ' only MODE 0,1 or 2 allowed for this option'
        write(6,1000)
        write(6,1011)
C
          WRITE(6,1500)
1500      FORMAT('$Output filename:  ')
          READ(5,1200) OUTFILE
C
        WRITE(6,1600)
1600    FORMAT(/,'$Enter size of mini boxes to be selected, NBOXX,NBOXY ')
        READ(5,*) NBOXX, NBOXY
        IF(NBOXX*NBOXY.GT.NDIM) STOP ' NBOXX*NBOXY too small for NDIM'
        NBOXX  = (NBOXX/2)*2
        NHBOXX =  NBOXX/2
        NBOXY  = (NBOXY/2)*2
        NHBOXY =  NBOXY/2
        WRITE(6,1610) NBOXX, NBOXY
1610    FORMAT(' Selected areas are',I5,' x ',I5)
C  better to be an even number
          WRITE(6,1700)
1700      FORMAT('$filename for list of X,Y centres of areas required : ')
          READ(5,1200) DATAFILE
          OPEN(UNIT=10,FILE=DATAFILE,STATUS='OLD')
          READ(10,1200) TITLELINE 
1701      FORMAT(' First (title) line discarded',A)
          WRITE(6,1701) TITLELINE
        DO 1800 IA=1,NAREA
                READ(10,*,END=1801) IAX(IA),IAY(IA)
1800    CONTINUE
        STOP ' Too many selected areas for program dimension'
1801    IF(IA.GT.1) THEN
                NAREAS = IA-1
                NXYZBOX(1) = NBOXX
                NXYZBOX(2) = NBOXY
                NXYZBOX(3) = NAREAS
                NXYZTMP(1) = NBOXX*NBOXY
                NXYZTMP(2) = NAREAS
                NXYZTMP(3) = 1
                WRITE(6,1805)NAREAS
1805            FORMAT(' Number of particles slected',I6)
        ENDIF
C
        CALL IMOPEN(2,OUTFILE,'NEW')
        CALL ITRHDR(2,1)
        CALL IALSIZ(2,NXYZTMP,NXYZST)
        CALL FDATE(DAT)
C        WRITE(TITLE) NAREAS,NBOXX,NBOXY,DAT(5:24,1900)
        WRITE(TITLE,1900) NAREAS,NBOXX,NBOXY,DAT(5:24)
1900    FORMAT(' LABELC: select',I6,
     . '  particles each with area',2I4,5X,A20)
        DMIN =  1.E10
        DMAX = -1.E10
        DOUBLMEAN = 0.0
        CALL IWRHDR(2,TITLE,1,DMIN,DMAX,DMEAN)
C
        DO 1950 IA = 1,NAREAS
                NX1 = IAX(IA) - NHBOXX + 1
                NX2 = IAX(IA) + NHBOXX
                NY1 = IAY(IA) - NHBOXY + 1
                NY2 = IAY(IA) + NHBOXY
                IF (NX1.LT.0.OR.NX2.GT.NXYZ(1)-1.OR.
     . NY1.LT.0.OR.NY2.GT.NXYZ(2)-1) THEN
        WRITE(6,1901) IAX(IA),IAY(IA)
1901    FORMAT('  particle too near edge ignored, coords',2I7)
C                 STOP ' particle too near edge'
                ENDIF
        WRITE(6,*) NX1,NX2,NY1,NY2
                CALL IMPOSN(1,0,0)
                CALL IRDPAS(1,ARRAY,NBOXX,NBOXY,NX1,NX2,NY1,NY2,*999)
                CALL IWRLIN(2,ARRAY)
                DO 1960 J=1,NBOXX*NBOXY
                        VAL = ARRAY(J)
                        IF (VAL .LT. DMIN) DMIN = VAL
                        IF (VAL .GT. DMAX) DMAX = VAL
                        DOUBLMEAN = DOUBLMEAN + VAL
1960            CONTINUE
1950    CONTINUE
        DMEAN = DOUBLMEAN/(NAREAS*NBOXX*NBOXY)
        CALL IALSIZ(2,NXYZBOX,NXYZST)
        CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
C
990     CALL IMCLOSE(2)
995     CALL IMCLOSE(1)
        CALL EXIT
999     STOP 'END-OF-FILE ERROR ON READING input image'
        END

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

