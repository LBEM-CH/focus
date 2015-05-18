C*CTFCORR.FOR******************************************************************
C                                                                             *
C       Program to apply CTF correction to an image                           *
C                                                                             *
C                                               Part of 2dx                   *
C                                                                             *
C       Version 1.00    30-SEP-14    HS         First version, using tiles    *
C       Version 1.10    04-JAN-15    HS         Changed to stripes            *
C                                                                             *
C******************************************************************************
C
C-----The image is CTF corrected in stripes that are parallel to the tilt axis.
C
C      Card input, or online control
C
C        card  1 :  input filename
C        card  2 :  output filename with CTF correction
C        card  3 :  output filename for summed CTF image
C        card  4 :  TLTAXA,TLTANG
C        card  5 :  CS[mm],HT[kV],PHACON,MAGNIFICATION,STEPSIZE_DIGITIZER[um]
C        card  6 :  DEF1,DEF2,ANGAST
C        card  7 :  NOISE (for Wiener option, otherwise give 0.0)
C        card  8 :  IMODE, describes which of the various simple manipulations
C                   IMODE=0: Nothing
C                   IMODE=1: PhaseFlip
C                   IMODE=2: CTF multiplication
C                   IMODE=3: Wiener Filter
C                   IMODE=4: Nothing
C                   IMODE=5: PhaseFlip (inverted contrast)
C                   IMODE=6: CTF multiplication (inverted contrast)
C                   IMODE=7: Wiener Filter (inverted contrast)
C        card  9 :  Debug mode: y=yes, n=no
C
C******************************************************************************
C
      IMPLICIT NONE
C
      INTEGER LMAX
      REAL PI
C
C      PARAMETER (LMAX=8100)
      PARAMETER (LMAX=4098)
      PARAMETER (PI=3.1415926535898)
C
      INTEGER   IMP
      CHARACTER NCPUS*10
      INTEGER   OMP_GET_NUM_PROCS
      INTEGER   OMP_GET_THREAD_NUM
      LOGICAL   EX
      INTEGER   ITNR
C
      CHARACTER CFORM
      INTEGER   IP,IS,IERR
      REAL      STEPR,DSTEP,R
C
      INTEGER NCX,NCY
      INTEGER ix,iy,iix,iiy,ixcor,iycor,iox,ioy,ilx,ily
      INTEGER*4 iover,iunder,ilow
      INTEGER MODE,IMODE
      INTEGER NX,NY,NZ,NL,NRX,NXP2
      INTEGER I,J,K,L,M,LL,MM,I2,J2,IR
      INTEGER*8 IBELOW,IABOVE,IHISTOFRAC
      INTEGER icount
      INTEGER idist
      INTEGER istripe,ISTRIPENUM
      REAL    rstripex,rstripey
C
      REAL CTF
      INTEGER ID,IADDSIZE
C
      COMPLEX CBOX(LMAX)
C
      INTEGER NXYZ(3),NXYZ2(3)
      REAL ALINE(LMAX)
      REAL LABELS(20,10),CELL(6),EXTRA(29)
      REAL DNCELL(6)
      REAL CNV,SCAL,CTFV
      REAL RNORMX,RNORMY
      REAL THETATR
      REAL RVAL
C
      REAL TLTAXA,TLTANG
      REAL CS,HT,PHACON,RMAG,STEPD,AMPCON,WL
      REAL VAL,DMIN,DMAX,DMEAN,D1MAX
      REAL DIMIN,DIMAX,DIMEAN
      REAL DOMIN,DOMAX,DOMEAN
      REAL DAMPMAX
      REAL RDEF1,RDEF2,ANGAST,ANGASTRAD
      REAL RNOISE,RESMAX,RDEFTOL
      REAL RPIXEL
      REAL RMAXAMPFACTOR
      REAL rgamma,rbeta
      REAL rdist1,rdist2,rdist3,RLDEF1,RLDEF2,RLDEFM
      REAL RSTRIPEWIDTH,RSTRIPETAPER
      REAL onevol
C
      REAL*8 DOUBLMEAN,DOUBLOMEAN,DVAL
C
      COMPLEX CVAL
C
      LOGICAL LDEBUG
      LOGICAL LCTFSUM
      LOGICAL LFFTW
C
      CHARACTER*200 INFILE,OUTFILE,CTFFILE
      CHARACTER*20 CSTIN
      character*1600 TITLE
      character*200 cfile,cname
      CHARACTER*1 CDEBUG
C
      INTEGER IFOR,IBAK
C
      REAL,ALLOCATABLE :: ABOX(:)
      REAL,ALLOCATABLE :: APIC(:)
      REAL,ALLOCATABLE :: ACTFPIC(:)
      REAL,ALLOCATABLE :: CFFTPIC(:)
C
      DATA CNV/57.29578/
      DATA IFOR/0/,IBAK/1/
C
C******************************************************************************
C
      WRITE(6,'(/,/,''CTFCOR: Program to apply CTF correction '',
     .      ''to an image in stripes'')')
C
C******************************************************************************
C
      LFFTW = .true.
C      LFFTW = .false.
C
C #ifdef _OPENMP
C       IMP=0
C       CALL GETENV('OMP_NUM_THREADS',NCPUS)
C       READ(NCPUS,*,ERR=111,END=111)IMP
C 111   CONTINUE
C       IF (IMP.LE.0) THEN
C         CALL GETENV('NCPUS',NCPUS)
C         READ(NCPUS,*,ERR=112,END=112)IMP
C 112     CONTINUE
C       ENDIF
C       IF (IMP.LE.0) THEN
C         IMP=OMP_GET_NUM_PROCS()
C       ENDIF
C       CALL OMP_SET_NUM_THREADS(IMP)
C #endif
C       IF (IMP.LE.0) IMP=1
C
C       IF (IMP.GT.1) THEN
C         WRITE(*,'('': Parallel processing: NCPUS = '',I8)') IMP
C       ENDIF
C
      ALLOCATE(APIC(LMAX*LMAX),STAT=IERR)
      IF (IERR.NE.0) THEN
        WRITE(*,*) ':: ERROR: Memory allocation failed in MAIN'
        STOP ':: Try reducing the image size'
      ENDIF
C
      ALLOCATE(ABOX(LMAX*LMAX),STAT=IERR)
      IF (IERR.NE.0) THEN
        WRITE(*,*) ':: ERROR: Memory allocation failed in MAIN'
        STOP ':: Try reducing the image size'
      ENDIF
C
      ALLOCATE(CFFTPIC(LMAX*LMAX),STAT=IERR)
      IF (IERR.NE.0) THEN
        WRITE(*,*) ':: ERROR: Memory allocation failed in MAIN'
        STOP ':: Try reducing the image size'
      ENDIF
C
      WRITE(6,'(''Input filename:  '')')
      READ(5,'(A)') INFILE
      call shorten(INFILE,k)
      write(6,'(''   Read: '',A)')INFILE(1:k)
C
      call GUESSF(INFILE,CFORM,EX)
      if(.not.EX)then
        write(*,'(''::ERROR: input file not found: '',A)')INFILE(1:k)
        STOP
      ENDIF
      write(CSTIN,'(''OLD'')')
      CALL IOPEN(INFILE,11,CFORM,MODE,NXYZ(1),NXYZ(2),
     +         NXYZ(3),CSTIN,STEPR,TITLE)
      NX=NXYZ(1)
      NY=NXYZ(2)
      NZ=NXYZ(3)

      IF (MODE .GE. 3) then
        write(6,'(''::ERROR, only works on mode 0 to 2'')')
        goto 900
      endif
      if(NX+2.gt.LMAX)then
        write(6,'(''::ERROR, image too large.'',
     .    '' Increase LMAX. Aborting.'')')
        goto 900
      endif
C
C-----Save output images as floating point
      MODE = 2
C
      WRITE(6,'(''Output filename:  '')')
      READ(5,'(A)') OUTFILE
      call shorten(OUTFILE,k)
      write(6,'(''   Read: '',A)')OUTFILE(1:k)
C
      WRITE(6,'(''CTFfile filename:  '')')
      READ(5,'(A)') CTFFILE
      call shorten(CTFFILE,k)
      write(6,'(''   Read: '',A)')CTFFILE(1:k)
      if(ctffile(1:1).eq."#")then
        LCTFSUM = .false. 
        write(6,'('' Will skip calculation of summed CTF image'')')
      else
        LCTFSUM = .true. 
      endif
C
      if (LCTFSUM) then
        ALLOCATE(ACTFPIC(LMAX*LMAX),STAT=IERR)
        IF (IERR.NE.0) THEN
          WRITE(*,*) ':: ERROR: Memory allocation failed in MAIN'
          STOP ':: Try reducing the image size'
        ENDIF
      endif
C
      WRITE(6,'(''TLTAXA,TLTANG'')')
      read(5,*)TLTAXA,TLTANG
      write(6,'(''    Read: '',2F12.3)')TLTAXA,TLTANG
C
      if(TLTAXA.gt.90.0 .or. TLTAXA.lt.-90.0) then
        write(6,'(''::ERROR: TLTAXA should be between +-90.0'')')
        goto 900
      endif
C-----check here if tilt axis happens to be +-90 deg.
C-----if so, apply small offset to avoid degeneracy
      IF (TLTAXA.GT. 89.9) TLTAXA= 89.9
      IF (TLTAXA.LT.-89.9) TLTAXA=-89.9
C
      if(TLTANG.gt.90.0 .or. TLTANG.lt.-90.0) then
        write(6,'(''::ERROR: TLTANG should be between +-90.0'')')
        goto 900
      endif
      if(TLTANG.gt.88.0 .or. TLTANG.lt.-88.0) then
        write(6,'(''::ERROR: TLTANG strangely high. Aborting.'')')
        goto 900
      endif
C
      WRITE(6,'(''CS,HT,PHACON,MAGNIFICATION,STEPDIGITIZER'')')
      read(5,*)CS,HT,PHACON,RMAG,STEPD
      write(6,'(''    Read: '',5F12.3)')CS,HT,PHACON,RMAG,STEPD
C
      WRITE(6,'(''DEF1,DEF2,ANGAST'')')
      read(5,*)RDEF1,RDEF2,ANGAST
      write(6,'(''    Read: '',3F12.3)')RDEF1,RDEF2,ANGAST
C
      WRITE(6,'(''RESMAX'')')
      read(5,*)RESMAX
      write(6,'(''    Read: '',F12.3)')RESMAX
C
      WRITE(6,'(''NOISE'')')
      read(5,*)RNOISE
      write(6,'(''    Read: '',F12.3)')RNOISE
C
      WRITE(6,'(''MODE'')')
      read(5,*)IMODE
      write(6,'(''    Read: '',I3)')IMODE
C
      if(IMODE.eq.1)then
        write(6,'('':Applying CTF Phase flipping only'')')
      elseif(IMODE.eq.2)then
        write(6,'('':Applying CTF multiplication'')')
      elseif(IMODE.eq.3)then
        write(6,'('':Applying Wiener filtration'')')
      elseif(IMODE.eq.4)then
        IMODE = 0
        write(6,'('':No CTF correction done'')')
      elseif(IMODE.eq.5)then
        write(6,'('':Applying CTF Phase flipping only'')')
      elseif(IMODE.eq.6)then
        write(6,'('':Applying CTF multiplication'')')
      elseif(IMODE.eq.7)then
        write(6,'('':Applying Wiener filtration'')')
      else
        IMODE = 0
        write(6,'('':No CTF correction done'')')
      endif
C
      WRITE(6,'(''Debug Mode: y or n'')')
      read(5,*)CDEBUG
      write(6,'(''    Read: '',A1)')CDEBUG
C
      if(CDEBUG.eq.'y')then
        LDEBUG=.true.
      else
        LDEBUG=.false.
      endif
C
C******************************************************************************
C******************************************************************************
C******************************************************************************
C
C-----Calculate pixel size in image in Angstroems
      RPIXEL = STEPD * 10000.0 / RMAG
      write(6,'(''Pixel size = '',F12.3,'' Angstroems'')')RPIXEL
C
C-----Dimensions of image are NX,NY.
C-----calculate center of image
      NCX=NX/2
      NCY=NY/2
      NXP2 = NX + 2
C
      SCAL=1.0/SQRT(REAL(NX*NY))
C
C-----N is normal to tilt axis, indicates the direction
C-----in which defocus varies most
C     RNORMX= SIN(TLTAXA*PI/180.0)
C     RNORMY=-COS(TLTAXA*PI/180.0)
C
C-----Parameters for CTF calculation
C
      CS=CS*(10.0**7.0)                          ! Angstroms
      HT=HT*1000.0                               ! Volts
      WL=12.26/SQRT(HT+0.9785*HT**2/(10.0**6.0)) ! Angstroms
      THETATR=WL/(RPIXEL*NX)
      AMPCON=SQRT(1.0-PHACON**2)
C
      write(*,'(''Opened file has dimensions '',2I6)')NX,NY
C
      if(NX.ne.NY)then
        write(6,'(''::ERROR: Only works for square images'')')
        goto 900
      endif
C
C-----Read in entire input image
C
      DMIN =  1.E10
      DMAX = -1.E10
      DMEAN = 0.0
      DOUBLMEAN = 0.0
C
      do iy = 1,NY
        CALL IREAD(11,ALINE,iy)
        if(LFFTW)then
          do ix = 1,NX
            VAL=ALINE(ix)
            APIC(ID(ix,iy,NXP2))=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
          enddo
        else
          do ix = 1,NX
            VAL=ALINE(ix)
            APIC(ID(ix,iy,NY))=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
          enddo
        endif
      enddo
      DOUBLMEAN = DOUBLMEAN/(NX*NY)
      DMEAN = DOUBLMEAN
C
      DIMIN=DMIN
      DIMAX=DMAX
      DIMEAN=DMEAN
      write(6,'(''Read input image. Min,Max,Mean = '',3F12.3)')
     .  DIMIN,DIMAX,DIMEAN
      CALL ICLOSE(11)
C
C-----Mark input image for debugging
C
      if (LDEBUG) then
        do ix=1,NX
          do iy=1,NY
            if(((ix-900)**2+(iy-900)**2).lt.500**2)then
              if(LFFTW)then
                APIC(ID(ix,iy,NXP2))=3.0
              else
                APIC(ID(ix,iy,NY))=3.0
              endif
            endif
          enddo
        enddo
C
        do ix=1,NX
          do iy=1,NY
            if(((ix-900)**2+(iy-900)**2).lt.3**2)then
              if(LFFTW)then
                APIC(ID(ix,iy,NXP2))=DIMAX
              else
                APIC(ID(ix,iy,NY))=DIMAX
              endif
            endif
          enddo
        enddo
      endif
C
C-----Prepare output CTF image
C
      if (LCTFSUM) then
        do ix=1,NX+1
          do iy=1,NY+1
            ACTFPIC(ID(ix,iy,NY))=0.0
          enddo
        enddo
      endif
C
C-----copy image into ABOX, and zero APIC for later use
C
      do iy = 1,NY
        do ix = 1,NX
          if(LFFTW)then
            IS=ID(ix,iy,NXP2)
          else
            IS=ID(ix,iy,NX)
          endif
          CFFTPIC(IS)=APIC(IS)
          APIC(IS)=0.0
        enddo
      enddo
C
C-----Calculate in-place Fourier Transform from entire image
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      if(LFFTW)then
        call TDXFFT(CFFTPIC,NX,NY,IFOR)
      else
        onevol = 1.0 / sqrt(REAL(NX*NY))
        do ix=1,NX
          do iy=1,NY
          IR=ID(ix,iy,NX)
              CFFTPIC(IR) = CFFTPIC(IR) * ONEVOL
          enddo
        enddo
        call RLFT3(CFFTPIC,CBOX,NX,NY,1,1)
      endif
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C-----Calculate number of stripes
C-----The defocus tolerance is EPS=RESMAX**2 / 2 / WL. (X. Zhang & H. Zhou, JSB 2011)
C----- 3A resolution requires 228A precise defocus at 300kV. (80A for 2A)
C-----The distance in Angstroems between two stripes is NX*SQRT(2) / ISTRIPENUM * RPIXEL.
C-----The hight difference between two stripes depends on the tilt angle. It is:
C-----NX * SQRT(2) / ISTRIPENUM * RPIXEL * tan(TLTANG)
C-----This should be smaller than above limits. We obtain:
C-----  RESMAX**2 / 2 / WL = NX * SQRT(2) / ISTRIPENUM * RPIXEL * tan(TLTANG)
C-----  ISTRIPENUM = NX * SQRT(2) * RPIXEL * tan(TLTANG) * 2 * WL / RESMAX**2
C
      RDEFTOL = RESMAX**2 / 2.0 / WL
      ISTRIPENUM = 1 + NX * sqrt(2.0) * RPIXEL * tan(TLTANG*PI/180.0) * 2.0 * WL / RESMAX**2
C
C     ISTRIPENUM = 1
C
      IS=ISTRIPENUM/2
      if(IS*2 .EQ. ISTRIPENUM) ISTRIPENUM = ISTRIPENUM+1
      write(6,'(/,'':Will use '',I6,'' stripes to ensure less than '',F9.1,
     .  ''A defocus difference for '',F8.1,''A resolution.'',/)')
     .  ISTRIPENUM,RDEFTOL,RESMAX
C
C-----Stripes are covering the image over a distance of NX*sqrt(2), 
C-----i.e. the length of the diagonal of the image
C-----The width of a stripe is
C
      if(ISTRIPENUM.gt.1)then
        RSTRIPEWIDTH = NX*sqrt(2.0)/real(ISTRIPENUM-1)
      else
        ISTRIPENUM=1
        RSTRIPEWIDTH = NX*sqrt(2.0)*1.1
      endif
C
C-----The taper-width connecting two stripes is 
C
      RSTRIPETAPER = RSTRIPEWIDTH * 0.1
C
      write(6,'(''RSTRIPEWIDTH = '',F12.3,''px, RSTRIPETAPER = '',
     .   F12.3,''px'',/)') RSTRIPEWIDTH,RSTRIPETAPER
C
C-----Loop over all stripes
C
      ANGASTRAD=ANGAST*PI/180.0
C
C !$OMP PARALLEL DO PRIVATE (rstripex,rstripey,rdist1,rbeta,rgamma,rdist2,rdist3,RLDEF1,RLDEF2,RLDEFM,ix,ilx,iy,ily,CTFV,iix,iiy,IS,IR,ABOX,RVAL)
C !$OMP&  CBOX,onevol)
      do istripe = 1,ISTRIPENUM
C
C-------Coordinates of center of stripe
        if(ISTRIPENUM.gt.1)then
          rstripex = real(NCX) + (real(istripe) - (real(ISTRIPENUM/2) + 1.0))
     .                * sin(TLTAXA*PI/180.0) * NX*sqrt(2.0) / real(ISTRIPENUM - 1)
          rstripey = real(NCY) + (real(istripe) - (real(ISTRIPENUM/2) + 1.0))
     .                * cos(TLTAXA*PI/180.0) * NY*sqrt(2.0) / real(ISTRIPENUM - 1)
        else
          rstripex = NCX
          rstripey = NCY
        endif
C
C-------Calculate local defocus for the center of the current stripe
C
C-------Defocus is "defocus = RDEF1,RDEF2,ANGAST".  TLTAXA, TLTANG.  Pixel size in Angstroems is RPIXEL
C
C-------Calculate distance from tilt axis for the center of this stripe, in pixels
C-------Center of image is NCX,NCY
C-------Center of stripe is rstripex,rstripey (here called px,py)
C-------distance between image center and stripe center is
C-------  sqrt((ncx-px)**2 + (ncy-py)**2)
        rdist1 = sqrt(real(rstripex-NCX)**2 + real(rstripey-NCY)**2)
C
C-------Angle "beta" between X-axis and line from image-center to stripe-center is 
C-------  arctan((py-ncy) / (px-ncx))
        rbeta = atan2(real(rstripey-NCY),real(rstripex-NCX))
C
C-------Angle between tilt axis and line from image-center to stripe-center is
C-------  beta - TLTAXA
        rgamma = rbeta - TLTAXA*PI/180.0
C
C-------Distance between stripe-center and closest point on tilt axis is
        rdist2 = sin(rgamma) * rdist1
C
C-------Distance in Angstroems is
        rdist3 = rdist2 * RPIXEL
C
C-------Defocus at local position is
C-------  rdist3 * tan(TLTANG)
        RLDEF1 = RDEF1 + rdist3 * tan(TLTANG*PI/180.0)
        RLDEF2 = RDEF2 + rdist3 * tan(TLTANG*PI/180.0)
        RLDEFM = (RLDEF1 + RLDEF2 ) / 2.0
        write(6,'(''Stripe '',I4,'' of '',I4,'': Center at position '',2F10.1,
     .      '' is at distance '',F12.3,'' px, or '',F12.3,
     .      '' A. Def = '',F12.3)')
     .      istripe,ISTRIPENUM,rstripex,rstripey,rdist2,rdist3,RLDEFM
C
C--------------------------------------
C-------Calculate local CTF profile
C--------------------------------------
C
        DO ix=1,NX/2
          ilx=ix-1
C          ITNR = OMP_GET_THREAD_NUM()  ! Aktuelle Threadnummer
C          write(6,'(''Thread '',I6,'', ix = '',I6)') ITNR, ix
          DO iy=1,NY
            ily=iy-1
            if(ily.gt.NY/2)ily=ily-NY
            if(ix.ne.0 .or. iy.ge.0)then
              CTFV=CTF(CS,WL,PHACON,AMPCON,RLDEF1,RLDEF2,
     +              ANGASTRAD,THETATR,ilx,ily)
C-------------Sum up the full-size CTFs with local defoci
              if(LCTFSUM) then
                if(IMODE.eq.1 .or. IMODE.eq.5)then
                  RVAL = abs(CTFV)
                elseif(IMODE.eq.2 .or. IMODE.eq.6)then
                  RVAL = CTFV**2
                elseif(IMODE.eq.3 .or. IMODE.eq.7)then
                  RVAL = 1.0
                else
                  RVAL = CTFV
                endif
                iix=ix+NX/2
                iiy=iy+NY/2
                if(iiy.gt.NY)iiy=iiy-NY
                IS=ID(iix,iiy,NY)
C !$OMP CRITICAL
                ACTFPIC(IS)=ACTFPIC(IS)+RVAL
C !$OMP END CRITICAL
              endif
              iix=ix
              iiy=iy
              if(LFFTW)then
                IR=ID(iix,iiy,NX/2+1)*2
              else
                IR=ID(iix,iiy,NX/2  )*2
              endif
C-------------Apply CTF correction
              if(IMODE.eq.1 .or. IMODE.eq.5)then
C                write(6,'('':Applying CTF Phase flipping only'')')
                IF(CTFV.gt.0.0)then
                  ABOX(IR-1)=-1.0*CFFTPIC(IR-1)
                  ABOX(IR  )=-1.0*CFFTPIC(IR  )
                else
                  ABOX(IR-1)= 1.0*CFFTPIC(IR-1)
                  ABOX(IR  )= 1.0*CFFTPIC(IR  )
                endif
              elseif(IMODE.eq.2 .or. IMODE.eq.6)then
C               write(6,'('':Applying CTF multiplication'')')
                ABOX(IR-1)=-1.0*CFFTPIC(IR-1)*CTFV
                ABOX(IR  )=-1.0*CFFTPIC(IR  )*CTFV
              elseif(IMODE.eq.3 .or. IMODE.eq.7)then
C               write(6,'('':Applying Wiener filtration'')')
                ABOX(IR-1)=-1.0*CFFTPIC(IR-1)*CTFV/(CTFV**2+RNOISE)
                ABOX(IR  )=-1.0*CFFTPIC(IR  )*CTFV/(CTFV**2+RNOISE)
              else
C               write(6,'('':Doing nothing (but in stripes)'')')
                ABOX(IR-1)= 1.0*CFFTPIC(IR-1)
                ABOX(IR  )= 1.0*CFFTPIC(IR  )
              endif
              if(IMODE.eq.5 .or. IMODE.eq.6 .or. IMODE.eq.7)then
                ABOX(IR-1)=-1.0*ABOX(IR-1)
                ABOX(IR  )=-1.0*ABOX(IR  )
              endif
            endif
          enddo
        enddo
C
C-------Set last column to zero
        DO iy=1,NY
          if(LFFTW)then
            ix=NX/2
            IR=ID(ix,iy,NX/2+1)*2
            ix=NX/2+1
            IR=ID(ix,iy,NX/2+1)*2
          else
            ix=NX/2
            IR=ID(ix,iy,NX/2  )*2
          endif
          ABOX(IR-1)=0.0
          ABOX(IR  )=0.0
        enddo
C
C-------Set last row to zero
        DO ix=1,NX/2
          if(LFFTW)then
            iy=NY/2
            IR=ID(ix,iy,NX/2+1)*2
            iy=NY/2+1
            IR=ID(ix,iy,NX/2+1)*2
          else
            iy=NY/2
            IR=ID(ix,iy,NX/2  )*2
          endif
          ABOX(IR-1)=0.0
          ABOX(IR  )=0.0
        enddo
C
C-------Calculate inverse in-place Fourier Transform
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        NRX = (NX - 1) * 2
        if(LFFTW)then
          call TDXFFT(ABOX,NX,NY,IBAK)
        else
          call RLFT3(ABOX,CBOX,NX,NY,1,-1)
          onevol = 1.0 / sqrt(REAL(NX*NY))
          do ix=1,NX
            do iy=1,NY
              IR=ID(ix,iy,NX)
              ABOX(IR) = ABOX(IR) * ONEVOL
            enddo
          enddo
        endif
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C------------------------------------------------------------
C-------Copy Stripe into output FFT
C------------------------------------------------------------
C
C !$OMP CRITICAL
        DO ix=1,NX
          DO iy=1,NY
            if(ISTRIPENUM.gt.1)then
              rdist1 = sqrt((real(ix)-rstripex)**2 + (real(iy)-rstripey)**2)
              rbeta = atan2((real(iy)-rstripey),(real(ix)-rstripex))
              rgamma = rbeta - TLTAXA*PI/180.0
              rdist2 = sin(rgamma) * rdist1
C              if(abs(rdist2).lt.(RSTRIPEWIDTH-RSTRIPETAPER)/2.0)then
C                IR=ID(ix,iy,NX)
C                APIC(IR) = APIC(IR) + ABOX(IR) * 1.0
C              elseif(abs(rdist2).lt.(RSTRIPEWIDTH+RSTRIPETAPER)/2.0)then
C                IR=ID(ix,iy,NX)
C                APIC(IR) = APIC(IR) + ABOX(IR) * ((RSTRIPEWIDTH+RSTRIPETAPER)/2.0-abs(rdist2)) 
C       .                   / (RSTRIPETAPER+1.0)
C              endif
              if(abs(rdist2).le.(RSTRIPEWIDTH+1)/2.0)then
                if(LFFTW)then
                  IR=ID(ix,iy,NXP2)
                else
                  IR=ID(ix,iy,NX)
                endif
                IS=ID(ix,iy,NX)
                APIC(IS) = ABOX(IR)
              endif
            else
              if(LFFTW)then
                IR=ID(ix,iy,NXP2)
              else
                IR=ID(ix,iy,NX)
              endif
              IS=ID(ix,iy,NX)
              APIC(IS) = ABOX(IR)
            endif
          enddo
        enddo
C !$OMP END CRITICAL
C
      enddo
C !$OMP END PARALLEL DO 
C
C------------------------------------
C-----Copy right half into left half of summed CTF
C------------------------------------
C
      if(LCTFSUM) then
        DO iy=-NY/2+1,NY/2-1
          DO ix=0,NX/2
            if(ix.ne.0 .or. iy.ge.0)then
              iix=ix+NX/2+1
              iiy=iy+NY/2+1
              IR=ID(iix,iiy,NX)
              iix=-ix+NX/2+1
              iiy=-iy+NY/2+1
              IS=ID(iix,iiy,NX)
C              if(IS.lt.1 .or. IR.lt.1)then
C                write(*,*)ix,iy,iix,iiy,IS,IR,NX,NY
C              endif
              ACTFPIC(IS)=ACTFPIC(IR)
            endif
          enddo
        enddo
      endif
C
C-----Write out output image
C
      write(*,'(''Output file has dimensions '',2I6)')NX,NY
C
C-----Output output image
C
      write(CSTIN,'(''NEW'')')
      CALL IOPEN(OUTFILE,12,CFORM,MODE,NXYZ(1),NXYZ(2),
     +         NXYZ(3),CSTIN,STEPR,TITLE)
C
      DMIN =  1.E10
      DMAX = -1.E10
      DMEAN = 0.0
      DOUBLMEAN = 0.0
C
      do iy = 1,NY
        do ix = 1,NX
          IR=ID(ix,iy,NY)
          VAL=APIC(IR)
          IF (VAL .LT. DMIN) DMIN = VAL
          IF (VAL .GT. DMAX) DMAX = VAL
          DOUBLMEAN = DOUBLMEAN + VAL
          ALINE(ix)=VAL
        enddo
        call IWRITE(12,ALINE,iy)
      enddo
      DOUBLMEAN = DOUBLMEAN/(NX*NY)
      DMEAN = DOUBLMEAN
C
      write(*,'(''Min, Max, Mean of output file is '',3F12.3)')
     .   DMIN,DMAX,DMEAN
      CALL ICLOSE(12)
C
      if(LCTFSUM)then
C-------Output CTF-sum image
C
C-------ATTENTION: The last row and last column aren't saved here:
        IADDSIZE = 0
C
        NXYZ2(1)=NXYZ(1)+IADDSIZE
        NXYZ2(2)=NXYZ(2)+IADDSIZE
        NXYZ2(3)=NXYZ(3)
        CALL IOPEN(CTFFILE,15,CFORM,MODE,NXYZ2(1),NXYZ2(2),
     +           NXYZ2(3),CSTIN,STEPR,TITLE)
C
C-------Normalize CTF-sum image between 0 and 1:
        DMIN =  1.E10
        DMAX = -1.E10
C
        do iy = 1,NY+IADDSIZE
          do ix = 1,NX+IADDSIZE
            IR=ID(ix,iy,NY)
            VAL=ACTFPIC(IR)
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
          enddo
        enddo
        DMAX=ABS(DMAX)
        if(ABS(DMIN).gt.ABS(DMAX))DMAX=ABS(DMIN)
        if(DMAX.lt.0.001)DMAX=0.001
        D1MAX=DMAX
C
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
        do iy = 1,NY+IADDSIZE
          do ix = 1,NX+IADDSIZE
            IR=ID(ix,iy,NY)
            VAL=ACTFPIC(IR)/D1MAX
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
            ALINE(ix)=VAL
          enddo
          call IWRITE(15,ALINE,iy)
        enddo
        DOUBLMEAN = DOUBLMEAN/((NX+1)*(NY+1))
        DMEAN = DOUBLMEAN
C
        write(*,'(''Min, Max, Mean of produced CTF-sum '',
     .    ''file is '',3F12.3)')
     .    DMIN,DMAX,DMEAN
        CALL ICLOSE(15)
      endif
C
C-----Done
C
      goto 999
C
 900  continue
      write(6,'(''::ABORTING'')')
      stop
C
 999  continue
      write(6,'(''Program normal end.'')')
C
      STOP
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
C**************************************************************************
C
      INTEGER FUNCTION ID(ix,iy,iylen)
C       this function calculates a 1D index for a 2D array of width iylen.
      ID = ix + iylen*(iy-1)
      RETURN
      END
C
C**************************************************************************
C
      REAL FUNCTION CTF(CS,WL,WGH1,WGH2,DFMID1,DFMID2,ANGAST,
     +                  THETATR,IX,IY)
C
      PARAMETER (TWOPI=6.2831853071796)
C
      RAD=IX**2+IY**2
      IF (RAD.NE.0.0) THEN
        RAD=SQRT(RAD)
        ANGLE=RAD*THETATR
        ANGSPT=ATAN2(REAL(IY),REAL(IX))
        C1=TWOPI*ANGLE*ANGLE/(2.0*WL)
        C2=-C1*CS*ANGLE*ANGLE/2.0
        ANGDIF=ANGSPT-ANGAST
        CCOS=COS(2.0*ANGDIF)
        DF=0.5*(DFMID1+DFMID2+CCOS*(DFMID1-DFMID2))
        CHI=C1*DF+C2
        CTF=-WGH1*SIN(CHI)-WGH2*COS(CHI)
      ELSE
        CTF=-WGH2
      ENDIF
C
      RETURN
      END
C
C**************************************************************************
C
      SUBROUTINE rlft3(data,speq,nn1,nn2,nn3,isign)
C
C-----Calculate the FFT of an image.
C
C-----This is copied from Niko Grigorieff's CTFFIND3.f
C
C     data: input and also output dataset. 
C        If input real image, use 2D real array.
C        If input FFT, use complex 2D array.
C
C     speq: one-dimensional temporary vector. Only needed for odd-numbered image sizes.
C
C     nn1: dimensions in x. If FFT, then width is nn1/2
C     nn2: dimensions in y
C     nn3: dimensions in z. Use 1 here.
C     isign: direction of FFT: 1=forward, -1=backward
C
C
C
      INTEGER isign,nn1,nn2,nn3,istat,iw
      PARAMETER (iw=4096)
      COMPLEX data(nn1/2,nn2,nn3),speq(nn2,nn3)
C
      REAL work(6*iw+15)
C
      INTEGER i1,i2,i3,j1,j2,j3,nn(3),nnh,nnq
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      COMPLEX c1,c2,h1,h2,w
C
      c1=cmplx(0.5,0.0)
      c2=cmplx(0.0,-0.5*isign)
      theta=6.28318530717959d0/dble(isign*nn1)
      wpr=-2.0d0*sin(0.5d0*theta)**2
      wpi=sin(theta)
      nnh=nn1/2
      nnq=nn1/4
      nn(1)=nnh
      nn(2)=nn2
      nn(3)=nn3
      if(isign.eq.1)then
        call pda_nfftf(3,nn,data,work,istat)
        do 12 i3=1,nn3
          do 11 i2=1,nn2
            speq(i2,i3)=data(1,i2,i3)
11        continue
12      continue
      endif
C
      if(isign.eq.-1)then
        call flip_array(data,speq,nn1,nn2,nn3)
      endif
C
      do 15 i3=1,nn3
        j3=1
        if (i3.ne.1) j3=nn3-i3+2
        wr=1.0d0
        wi=0.0d0
        do 14 i1=1,nnq+1
          j1=nnh-i1+2
          do 13 i2=1,nn2
            j2=1
            if (i2.ne.1) j2=nn2-i2+2
            if(i1.eq.1)then
              h1=c1*(data(1,j2,j3)+conjg(speq(i2,i3)))
              h2=c2*(data(1,j2,j3)-conjg(speq(i2,i3)))
              data(1,j2,j3)=h1+h2
              speq(i2,i3)=conjg(h1-h2)
            else
              h1=c1*(data(j1,j2,j3)+conjg(data(i1,i2,i3)))
              h2=c2*(data(j1,j2,j3)-conjg(data(i1,i2,i3)))
              data(j1,j2,j3)=h1+w*h2
              data(i1,i2,i3)=conjg(h1-w*h2)
            endif
13        continue
          wtemp=wr
          wr=wr*wpr-wi*wpi+wr
          wi=wi*wpr+wtemp*wpi+wi
          w=cmplx(sngl(wr),sngl(wi))
14      continue
15    continue
C
      if(isign.eq.1)then
        call flip_array(data,speq,nn1,nn2,nn3)
      endif
C
      if(isign.eq.-1)then
        call pda_nfftb(3,nn,data,work,istat)
      endif
      return
      END
C
C**************************************************************************
C
      SUBROUTINE FLIP_ARRAY(DATA,SPEQ,NN1,NN2,NN3)
C
      IMPLICIT NONE
C
      INTEGER NN1,NN2,NN3,I1,I2,I3,J1,J2,J3,NNH
      COMPLEX DATA(NN1/2,NN2,NN3),SPEQ(NN2,NN3),W
C
      nnh=nn1/2
      do 13 i1=1,nnh/2+1
        do 14 i3=1,nn3
          do 15 i2=1,nn2
            j1=1
            if (i1.ne.1) j1=nnh-i1+2
            w=data(i1,i2,i3)
            data(i1,i2,i3)=data(j1,i2,i3)
            data(j1,i2,i3)=w
15        continue
14      continue
13    continue
      do 10 i2=1,nn2/2+1
        do 11 i3=1,nn3
          j2=1
          if (i2.ne.1) j2=nn2-i2+2
          w=speq(i2,i3)
          speq(i2,i3)=speq(j2,i3)
          speq(j2,i3)=w
          do 12 i1=1,nnh
            w=data(i1,i2,i3)
            data(i1,i2,i3)=data(i1,j2,i3)
            data(i1,j2,i3)=w
12        continue
11      continue
10    continue
      do 16 i3=1,nn3/2+1
        j3=1
        if (i3.ne.1) j3=nn3-i3+2
        do 17 i2=1,nn2
          w=speq(i2,i3)
          speq(i2,i3)=speq(i2,j3)
          speq(i2,j3)=w
          do 18 i1=1,nnh
            w=data(i1,i2,i3)
            data(i1,i2,i3)=data(i1,i2,j3)
            data(i1,i2,j3)=w
18        continue
17      continue
16    continue
C
      return
      end
C**************************************************************************
      SUBROUTINE PDA_CFFTB1 (N,C,CH,WA,AFAC)
      DIMENSION       CH(*)      ,C(*)       ,WA(*)      ,AFAC(*)
      NF = AFAC(2)
      NA = 0
      L1 = 1
      IW = 1
      DO 116 K1=1,NF
         IP = AFAC(K1+2)
         L2 = IP*L1
         IDO = N/L2
         IDOT = IDO+IDO
         IDL1 = IDOT*L1
         IF (IP .NE. 4) GO TO 103
         IX2 = IW+IDOT
         IX3 = IX2+IDOT
         IF (NA .NE. 0) GO TO 101
         CALL PDA_PASSB4 (IDOT,L1,C,CH,WA(IW),WA(IX2),WA(IX3))
         GO TO 102
  101    CALL PDA_PASSB4 (IDOT,L1,CH,C,WA(IW),WA(IX2),WA(IX3))
  102    NA = 1-NA
         GO TO 115
  103    IF (IP .NE. 2) GO TO 106
         IF (NA .NE. 0) GO TO 104
         CALL PDA_PASSB2 (IDOT,L1,C,CH,WA(IW))
         GO TO 105
  104    CALL PDA_PASSB2 (IDOT,L1,CH,C,WA(IW))
  105    NA = 1-NA
         GO TO 115
  106    IF (IP .NE. 3) GO TO 109
         IX2 = IW+IDOT
         IF (NA .NE. 0) GO TO 107
         CALL PDA_PASSB3 (IDOT,L1,C,CH,WA(IW),WA(IX2))
         GO TO 108
  107    CALL PDA_PASSB3 (IDOT,L1,CH,C,WA(IW),WA(IX2))
  108    NA = 1-NA
         GO TO 115
  109    IF (IP .NE. 5) GO TO 112
         IX2 = IW+IDOT
         IX3 = IX2+IDOT
         IX4 = IX3+IDOT
         IF (NA .NE. 0) GO TO 110
         CALL PDA_PASSB5 (IDOT,L1,C,CH,WA(IW),WA(IX2),WA(IX3),WA(IX4))
         GO TO 111
  110    CALL PDA_PASSB5 (IDOT,L1,CH,C,WA(IW),WA(IX2),WA(IX3),WA(IX4))
  111    NA = 1-NA
         GO TO 115
  112    IF (NA .NE. 0) GO TO 113
         CALL PDA_PASSB (NAC,IDOT,IP,L1,IDL1,C,C,C,CH,CH,WA(IW))
         GO TO 114
  113    CALL PDA_PASSB (NAC,IDOT,IP,L1,IDL1,CH,CH,CH,C,C,WA(IW))
  114    IF (NAC .NE. 0) NA = 1-NA
  115    L1 = L2
         IW = IW+(IP-1)*IDOT
  116 CONTINUE
      IF (NA .EQ. 0) RETURN
      N2 = N+N
      DO 117 I=1,N2
         C(I) = CH(I)
  117 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_CFFTB (N,C,WSAVE)
      DIMENSION       C(*)       ,WSAVE(*)
      IF (N .EQ. 1) RETURN
      IW1 = N+N+1
      IW2 = IW1+N+N
      CALL PDA_CFFTB1 (N,C,WSAVE,WSAVE(IW1),WSAVE(IW2))
      RETURN
      END
      SUBROUTINE PDA_CFFTF1 (N,C,CH,WA,AFAC)
      DIMENSION       CH(*)      ,C(*)       ,WA(*)      ,AFAC(*)
      NF = AFAC(2)
      NA = 0
      L1 = 1
      IW = 1
      DO 116 K1=1,NF
         IP = AFAC(K1+2)
         L2 = IP*L1
         IDO = N/L2
         IDOT = IDO+IDO
         IDL1 = IDOT*L1
         IF (IP .NE. 4) GO TO 103
         IX2 = IW+IDOT
         IX3 = IX2+IDOT
         IF (NA .NE. 0) GO TO 101
         CALL PDA_PASSF4 (IDOT,L1,C,CH,WA(IW),WA(IX2),WA(IX3))
         GO TO 102
  101    CALL PDA_PASSF4 (IDOT,L1,CH,C,WA(IW),WA(IX2),WA(IX3))
  102    NA = 1-NA
         GO TO 115
  103    IF (IP .NE. 2) GO TO 106
         IF (NA .NE. 0) GO TO 104
         CALL PDA_PASSF2 (IDOT,L1,C,CH,WA(IW))
         GO TO 105
  104    CALL PDA_PASSF2 (IDOT,L1,CH,C,WA(IW))
  105    NA = 1-NA
         GO TO 115
  106    IF (IP .NE. 3) GO TO 109
         IX2 = IW+IDOT
         IF (NA .NE. 0) GO TO 107
         CALL PDA_PASSF3 (IDOT,L1,C,CH,WA(IW),WA(IX2))
         GO TO 108
  107    CALL PDA_PASSF3 (IDOT,L1,CH,C,WA(IW),WA(IX2))
  108    NA = 1-NA
         GO TO 115
  109    IF (IP .NE. 5) GO TO 112
         IX2 = IW+IDOT
         IX3 = IX2+IDOT
         IX4 = IX3+IDOT
         IF (NA .NE. 0) GO TO 110
         CALL PDA_PASSF5 (IDOT,L1,C,CH,WA(IW),WA(IX2),WA(IX3),WA(IX4))
         GO TO 111
  110    CALL PDA_PASSF5 (IDOT,L1,CH,C,WA(IW),WA(IX2),WA(IX3),WA(IX4))
  111    NA = 1-NA
         GO TO 115
  112    IF (NA .NE. 0) GO TO 113
         CALL PDA_PASSF (NAC,IDOT,IP,L1,IDL1,C,C,C,CH,CH,WA(IW))
         GO TO 114
  113    CALL PDA_PASSF (NAC,IDOT,IP,L1,IDL1,CH,CH,CH,C,C,WA(IW))
  114    IF (NAC .NE. 0) NA = 1-NA
  115    L1 = L2
         IW = IW+(IP-1)*IDOT
  116 CONTINUE
      IF (NA .EQ. 0) RETURN
      N2 = N+N
      DO 117 I=1,N2
         C(I) = CH(I)
  117 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_CFFTF (N,C,WSAVE)
      DIMENSION       C(*)       ,WSAVE(*)
      IF (N .EQ. 1) RETURN
      IW1 = N+N+1
      IW2 = IW1+N+N
      CALL PDA_CFFTF1 (N,C,WSAVE,WSAVE(IW1),WSAVE(IW2))
      RETURN
      END
      SUBROUTINE PDA_CFFTI1 (N,WA,AFAC)
      DIMENSION       WA(*)      ,AFAC(*)    ,NTRYH(4)
      DATA NTRYH(1),NTRYH(2),NTRYH(3),NTRYH(4)/3,4,2,5/
      NL = N
      NF = 0
      J = 0
  101 J = J+1
      IF (J-4) 102,102,103
  102 NTRY = NTRYH(J)
      GO TO 104
  103 NTRY = NTRY+2
  104 NQ = NL/NTRY
      NR = NL-NTRY*NQ
      IF (NR) 101,105,101
  105 NF = NF+1
      AFAC(NF+2) = NTRY
      NL = NQ
      IF (NTRY .NE. 2) GO TO 107
      IF (NF .EQ. 1) GO TO 107
      DO 106 I=2,NF
         IB = NF-I+2
         AFAC(IB+2) = AFAC(IB+1)
  106 CONTINUE
      AFAC(3) = 2
  107 IF (NL .NE. 1) GO TO 104
      AFAC(1) = N
      AFAC(2) = NF
      TPI = 6.28318530717959
      ARGH = TPI/FLOAT(N)
      I = 2
      L1 = 1
      DO 110 K1=1,NF
         IP = AFAC(K1+2)
         LD = 0
         L2 = L1*IP
         IDO = N/L2
         IDOT = IDO+IDO+2
         IPM = IP-1
         DO 109 J=1,IPM
            I1 = I
            WA(I-1) = 1.
            WA(I) = 0.
            LD = LD+L1
            FI = 0.
            ARGLD = FLOAT(LD)*ARGH
            DO 108 II=4,IDOT,2
               I = I+2
               FI = FI+1.
               ARG = FI*ARGLD
               WA(I-1) = COS(ARG)
               WA(I) = SIN(ARG)
  108       CONTINUE
            IF (IP .LE. 5) GO TO 109
            WA(I1-1) = WA(I-1)
            WA(I1) = WA(I)
  109    CONTINUE
         L1 = L2
  110 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_CFFTI (N,WSAVE)
      DIMENSION       WSAVE(*)
      IF (N .EQ. 1) RETURN
      IW1 = N+N+1
      IW2 = IW1+N+N
      CALL PDA_CFFTI1 (N,WSAVE(IW1),WSAVE(IW2))
      RETURN
      END
      SUBROUTINE PDA_NFFTB( NDIM, DIM, DATA, WORK, ISTAT )
*+
*  Name:
*     PDA_NFFTB

*  Purpose:
*     Take the backward FFT of an N-dimensional complex array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PDA_NFFTB( NDIM, DIM, X, Y, WORK, ISTAT )

*  Description:
*     The supplied Fourier co-efficients in X and Y are replaced by the 
*     corresponding spatial data obtained by doing an inverse Fourier
*     transform. See the forward FFT routine PDA_NFFTF for more details.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of dimensions. This should be no more than 20.
*     DIM( NDIM ) = INTEGER (Given)
*        The size of each dimension.
*     X( * ) = REAL (Given and Returned)
*        Supplied holding the real parts of the Fourier co-efficients.
*        Returned holding the real parts of the spatial data. The array 
*        should have the number of elements implied by NDIM and DIM.
*     Y( * ) = REAL (Given and Returned)
*        Supplied holding the imaginary parts of the Fourier co-efficients.
*        Returned holding the imaginary parts of the spatial data. The array 
*        should have the number of elements implied by NDIM and DIM.
*     WORK( * ) = REAL (Given and Returned)
*        A work array. This should have at least ( 6*DimMax + 15 )
*        elements where DimMax is the maximum of the values supplied in
*        DIM.
*     ISTAT = INTEGER (Returned)
*        If the value of NDIM is greater than 20 or less than 1, then
*        ISTAT is returned equal to 1, and the values in X and Y are
*        left unchanged. Otherwise, ISTAT is returned equal to 0.
      
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-FEB-1995 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER NDIM
      INTEGER DIM( NDIM )
      
*  Arguments Given and Returned:
C      REAL X( * )
C      REAL Y( * )
      COMPLEX DATA( * )
      REAL WORK( * )

*  Arguments Returned:
      INTEGER ISTAT
      
*  Local Constants:
      INTEGER MXDIM              ! Max number of dimensions
      PARAMETER( MXDIM = 20 )
      
*  Local Variables:
      INTEGER
     :     CART( MXDIM + 1 ),    ! Current Cartesian pixel indices
     :     I,                    ! Index of current dimension
     :     INC,                  ! Vector increment to next row element
     :     IW,                   ! Index into the work array
     :     IWN,                  ! Index of first free work array element
     :     J,                    ! Row counter
     :     K,                    ! Pixel index on current axis
     :     M,                    ! Size of current dimension
     :     N,                    ! Total no. of pixels
     :     STEP,                 ! Vector step to start of next row
     :     V,                    ! Vector address
     :     V0                    ! Vector address of start of current row

      REAL
     :     FAC                   ! Normalisation factor

*.

*  Check that the supplied number of dimensions is not too high, and
*  not too low. Return 1 for the status variable and abort otherwise.
      IF( NDIM .GT. MXDIM .OR. NDIM .LE. 0 ) THEN
         ISTAT = 1

*  If the number of dimensions is ok, return 0 for the status value and
*  continue.
      ELSE
         ISTAT = 0

*  Find the total number of pixels.
         N = 1
         DO I = 1, NDIM
            N = N*DIM( I )
         END DO

*  The first dimension can be processed using a faster algorithm
*  because the elements to be processed occupy adjacent elements in the
*  supplied array. Set up the step (in vector address) between the
*  start of each row, and initialise the vector address of the start of
*  the first row.
         M = DIM( 1 )
         V0 = 1

*  Initialise the FFT work array for the current dimension. Save the
*  index of the next un-used element of the work array.
         CALL PDA_CFFTI( M, WORK )
         IWN = 4*M + 16

*  Store the factor which will normalise the Fourier co-efficients
*  returned by this routine (i.e. so that a call to PDA_NFFTB followed by a
*  call to PDA_NFFTB will result in no change to the data).
C         FAC = 1.0/SQRT( REAL ( N ) )

*  Loop round copying each row.
         DO J = 1, N/M

*  Copy this row into the unused part of the work array.
            IW = IWN
            V = V0
            DO K = 1, M
               WORK( IW ) = REAL(DATA( V ))
               WORK( IW + 1 ) = AIMAG(DATA( V ))
               IW = IW + 2
               V = V + 1
            END DO

*  Take the FFT of it.
            CALL PDA_CFFTB( M, WORK( IWN ), WORK )         

*  Copy it back to the supplied arrays, normalising it in the process.
            IW = IWN
            V = V0
            DO K = 1, M
               DATA( V ) = CMPLX(WORK( IW ),WORK( IW + 1 ))
               IW = IW + 2
               V = V + 1
            END DO

*  Increment the vector address of the start of the next row.
            V0 = V0 + M

         END DO
         
*  Now set up the increment between adjacent elements of "rows" parallel
*  to the second dimension.
         INC = DIM( 1 )         

*  Process the remaining dimensions. Store the durrent dimensions.
         DO I = 2, NDIM
            M = DIM( I )
            
*  Initialise the co-ordinates (vector and Cartesian) of the first
*  element of the first row.
            V0 = 1

            DO J = 1, NDIM
               CART( J ) = 1
            END DO

*  Initialise the FFT work array for this dimension, and save the index
*  of the next un-used element in the work array. 
            CALL PDA_CFFTI( M, WORK )
            IWN = 4*M + 16
            
*  Store the step (in vector address) between the end of one "row" and
*  the start of the next.
            STEP = INC*( M - 1 )            

*  Loop round each "row" parallel to the current dimensions.
            DO J = 1, N/M

*  Copy the current "row" into the work space.
               V = V0
               IW = IWN

               DO K = 1, M
                  WORK( IW ) = REAL(DATA( V ))
                  WORK( IW + 1 ) = AIMAG(DATA( V ))
                  V = V + INC
                  IW = IW + 2
               END DO

*  Take the FFT of the current "row".
               CALL PDA_CFFTB( M, WORK( IWN ), WORK )               

*  Copy the FFT of the current "row" back into the supplied array.
               V = V0
               IW = IWN

               DO K = 1, M
                  DATA( V ) = CMPLX(WORK( IW ),WORK( IW + 1 ))
                  V = V + INC
                  IW = IW + 2
               END DO
   
*  Increment the co-ordinates of the start of the current "row".
               V0 = V0 + 1
               K = 1
               CART( 1 ) = CART( 1 ) + 1

*  If the upper pixel index bound for the current dimension has been
*  exceeded, reset the pixel index to 1 and increment the next
*  dimension. If the next dimension is the dimension currently being
*  transformed, skip over it so that it stays at 1 (but increment the
*  vector address to account for the skip).
C
C  The following 16 lines contain code corrected by Alexis Rohou
C  7 Feb 13
               DO WHILE( (K .LE. NDIM))
                  IF ((CART( K ) .GT. DIM( K )) ) THEN
                      CART( K ) = 1
                      K = K + 1

                      IF( K .EQ. I ) THEN
                         K = K + 1
                         V0 = V0 + STEP
                      END IF

                      CART( K ) = CART( K ) + 1
                  ELSE
                      K = K + 1
                  ENDIF

               END DO

            END DO

*  Store the increment in vector address between adjacent elements of
*  the next "row".
            INC = INC*M
            
         END DO

      END IF
         
      END
      SUBROUTINE PDA_NFFTF( NDIM, DIM, DATA, WORK, ISTAT )
*+
*  Name:
*     PDA_NFFTF

*  Purpose:
*     Take the forward FFT of an N-dimensional complex array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PDA_NFFTF( NDIM, DIM, X, Y, WORK, ISTAT )

*  Description:
*     The supplied data values in X and Y are replaced by the 
*     co-efficients of the Fourier transform of the supplied data.
*     The co-efficients are normalised so that a subsequent call to
*     PDA_NFFTB to perform a backward FFT would restore the original data
*     values.
*
*     The multi-dimensional FFT is implemented using 1-dimensional FFTPACK
*     routines. First each row (i.e. a line of pixels parallel to the first
*     axis) in the supplied array is transformed, the Fourier co-efficients 
*     replacing the supplied data. Then each column (i.e. a line of pixels
*     parallel to the second axis) is transformed. Then each line of pixels
*     parallel to the third axis is transformed, etc. Each dimension is 
*     transformed in this way. Most of the complications in the code come
*     from needing to work in an unknown number of dimensions. Two
*     addressing systems are used for each pixel; 1) the vector (i.e.
*     1-dimensional ) index into the supplied arrays, and 2) the
*     corresponding Cartesian pixel indices.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of dimensions. This should be no more than 20.
*     DIM( NDIM ) = INTEGER (Given)
*        The size of each dimension.
*     X( * ) = REAL (Given and Returned)
*        Supplied holding the real parts of the complex data
*        values. Returned holding the real parts of the Fourier
*        co-efficients. The array should have the number of elements
*        implied by NDIM ande DIM.
*     Y( * ) = REAL (Given and Returned)
*        Supplied holding the imaginary parts of the complex data
*        values. Returned holding the imaginary parts of the Fourier
*        co-efficients. The array should have the number of elements
*        implied by NDIM ande DIM.
*     WORK( * ) = REAL (Given and Returned)
*        A work array. This should have at least ( 6*DimMax + 15 )
*        elements where DimMax is the maximum of the values supplied in
*        DIM.
*     ISTAT = INTEGER (Returned)
*        If the value of NDIM is greater than 20 or less than 1, then
*        ISTAT is returned equal to 1, and the values in X and Y are
*        left unchanged. Otherwise, ISTAT is returned equal to 0.
      
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-FEB-1995 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER NDIM
      INTEGER DIM( NDIM )
      
*  Arguments Given and Returned:
C      REAL X( * )
C      REAL Y( * )
      COMPLEX DATA( * )
      REAL WORK( * )

*  Arguments Returned:
      INTEGER ISTAT
      
*  Local Constants:
      INTEGER MXDIM              ! Max number of dimensions
      PARAMETER( MXDIM = 20 )
      
*  Local Variables:
      INTEGER
     :     CART( MXDIM + 1 ),    ! Current Cartesian pixel indices
     :     I,                    ! Index of current dimension
     :     INC,                  ! Vector increment to next row element
     :     IW,                   ! Index into the work array
     :     IWN,                  ! Index of first free work array element
     :     J,                    ! Row counter
     :     K,                    ! Pixel index on current axis
     :     M,                    ! Size of current dimension
     :     N,                    ! Total no. of pixels
     :     STEP,                 ! Vector step to start of next row
     :     V,                    ! Vector address
     :     V0                    ! Vector address of start of current row

      REAL
     :     FAC                   ! Normalisation factor

*.

*  Check that the supplied number of dimensions is not too high, and
*  not too low. Return 1 for the status variable and abort otherwise.
      IF( NDIM .GT. MXDIM .OR. NDIM .LE. 0 ) THEN
         ISTAT = 1

*  If the number of dimensions is ok, return 0 for the status value and
*  continue.
      ELSE
         ISTAT = 0

*  Find the total number of pixels.
         N = 1
         DO I = 1, NDIM
            N = N*DIM( I )
         END DO

*  The first dimension can be processed using a faster algorithm
*  because the elements to be processed occupy adjacent elements in the
*  supplied array. Set up the step (in vector address) between the
*  start of each row, and initialise the vector address of the start of
*  the first row.
         M = DIM( 1 )
         V0 = 1

*  Initialise the FFT work array for the current dimension. Save the
*  index of the next un-used element of the work array.
         CALL PDA_CFFTI( M, WORK )
         IWN = 4*M + 16

*  Store the factor which will normalise the Fourier co-efficients
*  returned by this routine (i.e. so that a call to PDA_NFFTF followed by a
*  call to PDA_NFFTB will result in no change to the data).
C         FAC = 1.0/SQRT( REAL ( N ) )

*  Loop round copying each row.
         DO J = 1, N/M

*  Copy this row into the unused part of the work array.
            IW = IWN
            V = V0
            DO K = 1, M
               WORK( IW ) = REAL(DATA( V ))
               WORK( IW + 1 ) = AIMAG(DATA( V ))
               IW = IW + 2
               V = V + 1
            END DO

*  Take the FFT of it.
            CALL PDA_CFFTF( M, WORK( IWN ), WORK )         

*  Copy it back to the supplied arrays, normalising it in the process.
            IW = IWN
            V = V0
            DO K = 1, M
               DATA( V ) = CMPLX(WORK( IW ),WORK( IW + 1 ))
               IW = IW + 2
               V = V + 1
            END DO

*  Increment the vector address of the start of the next row.
            V0 = V0 + M

         END DO
         
*  Now set up the increment between adjacent elements of "rows" parallel
*  to the second dimension.
         INC = DIM( 1 )         

*  Process the remaining dimensions. Store the durrent dimensions.
         DO I = 2, NDIM
            M = DIM( I )
            
*  Initialise the co-ordinates (vector and Cartesian) of the first
*  element of the first row.
            V0 = 1

            DO J = 1, NDIM
               CART( J ) = 1
            END DO

*  Initialise the FFT work array for this dimension, and save the index
*  of the next un-used element in the work array. 
            CALL PDA_CFFTI( M, WORK )
            IWN = 4*M + 16
            
*  Store the step (in vector address) between the end of one "row" and
*  the start of the next.
            STEP = INC*( M - 1 )            

*  Loop round each "row" parallel to the current dimensions.
            DO J = 1, N/M

*  Copy the current "row" into the work space.
               V = V0
               IW = IWN

               DO K = 1, M
                  WORK( IW ) = REAL(DATA( V ))
                  WORK( IW + 1 ) = AIMAG(DATA( V ))
                  V = V + INC
                  IW = IW + 2
               END DO

*  Take the FFT of the current "row".
               CALL PDA_CFFTF( M, WORK( IWN ), WORK )               

*  Copy the FFT of the current "row" back into the supplied array.
               V = V0
               IW = IWN

               DO K = 1, M
                  DATA( V ) = CMPLX(WORK( IW ),WORK( IW + 1 ))
                  V = V + INC
                  IW = IW + 2
               END DO

*  Increment the co-ordinates of the start of the current "row".
               V0 = V0 + 1
               K = 1
               CART( 1 ) = CART( 1 ) + 1

*  If the upper pixel index bound for the current dimension has been
*  exceeded, reset the pixel index to 1 and increment the next
*  dimension. If the next dimension is the dimension currently being
*  transformed, skip over it so that it stays at 1 (but increment the
*  vector address to account for the skip).
C
C  The following 16 lines contain code corrected by Alexis Rohou
C  7 Feb 13
               DO WHILE( (K .LE. NDIM))
                  IF ((CART( K ) .GT. DIM( K )) ) THEN
                      CART( K ) = 1
                      K = K + 1

                      IF( K .EQ. I ) THEN
                         K = K + 1
                         V0 = V0 + STEP
                      END IF

                      CART( K ) = CART( K ) + 1
                  ELSE
                      K = K + 1
                  ENDIF

               END DO

            END DO

*  Store the increment in vector address between adjacent elements of
*  the next "row".
            INC = INC*M

         END DO

      END IF

      END
      SUBROUTINE PDA_PASSB2 (IDO,L1,CC,CH,WA1)
      DIMENSION       CC(IDO,2,L1)           ,CH(IDO,L1,2)           ,
     1                WA1(*)
      IF (IDO .GT. 2) GO TO 102
      DO 101 K=1,L1
         CH(1,K,1) = CC(1,1,K)+CC(1,2,K)
         CH(1,K,2) = CC(1,1,K)-CC(1,2,K)
         CH(2,K,1) = CC(2,1,K)+CC(2,2,K)
         CH(2,K,2) = CC(2,1,K)-CC(2,2,K)
  101 CONTINUE
      RETURN
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            CH(I-1,K,1) = CC(I-1,1,K)+CC(I-1,2,K)
            TR2 = CC(I-1,1,K)-CC(I-1,2,K)
            CH(I,K,1) = CC(I,1,K)+CC(I,2,K)
            TI2 = CC(I,1,K)-CC(I,2,K)
            CH(I,K,2) = WA1(I-1)*TI2+WA1(I)*TR2
            CH(I-1,K,2) = WA1(I-1)*TR2-WA1(I)*TI2
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSB3 (IDO,L1,CC,CH,WA1,WA2)
      DIMENSION       CC(IDO,3,L1)           ,CH(IDO,L1,3)           ,
     1                WA1(*)     ,WA2(*)
      DATA TAUR,TAUI /-.5,.866025403784439/
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TR2 = CC(1,2,K)+CC(1,3,K)
         CR2 = CC(1,1,K)+TAUR*TR2
         CH(1,K,1) = CC(1,1,K)+TR2
         TI2 = CC(2,2,K)+CC(2,3,K)
         CI2 = CC(2,1,K)+TAUR*TI2
         CH(2,K,1) = CC(2,1,K)+TI2
         CR3 = TAUI*(CC(1,2,K)-CC(1,3,K))
         CI3 = TAUI*(CC(2,2,K)-CC(2,3,K))
         CH(1,K,2) = CR2-CI3
         CH(1,K,3) = CR2+CI3
         CH(2,K,2) = CI2+CR3
         CH(2,K,3) = CI2-CR3
  101 CONTINUE
      RETURN
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TR2 = CC(I-1,2,K)+CC(I-1,3,K)
            CR2 = CC(I-1,1,K)+TAUR*TR2
            CH(I-1,K,1) = CC(I-1,1,K)+TR2
            TI2 = CC(I,2,K)+CC(I,3,K)
            CI2 = CC(I,1,K)+TAUR*TI2
            CH(I,K,1) = CC(I,1,K)+TI2
            CR3 = TAUI*(CC(I-1,2,K)-CC(I-1,3,K))
            CI3 = TAUI*(CC(I,2,K)-CC(I,3,K))
            DR2 = CR2-CI3
            DR3 = CR2+CI3
            DI2 = CI2+CR3
            DI3 = CI2-CR3
            CH(I,K,2) = WA1(I-1)*DI2+WA1(I)*DR2
            CH(I-1,K,2) = WA1(I-1)*DR2-WA1(I)*DI2
            CH(I,K,3) = WA2(I-1)*DI3+WA2(I)*DR3
            CH(I-1,K,3) = WA2(I-1)*DR3-WA2(I)*DI3
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSB4 (IDO,L1,CC,CH,WA1,WA2,WA3)
      DIMENSION       CC(IDO,4,L1)           ,CH(IDO,L1,4)           ,
     1                WA1(*)     ,WA2(*)     ,WA3(*)
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TI1 = CC(2,1,K)-CC(2,3,K)
         TI2 = CC(2,1,K)+CC(2,3,K)
         TR4 = CC(2,4,K)-CC(2,2,K)
         TI3 = CC(2,2,K)+CC(2,4,K)
         TR1 = CC(1,1,K)-CC(1,3,K)
         TR2 = CC(1,1,K)+CC(1,3,K)
         TI4 = CC(1,2,K)-CC(1,4,K)
         TR3 = CC(1,2,K)+CC(1,4,K)
         CH(1,K,1) = TR2+TR3
         CH(1,K,3) = TR2-TR3
         CH(2,K,1) = TI2+TI3
         CH(2,K,3) = TI2-TI3
         CH(1,K,2) = TR1+TR4
         CH(1,K,4) = TR1-TR4
         CH(2,K,2) = TI1+TI4
         CH(2,K,4) = TI1-TI4
  101 CONTINUE
      RETURN
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TI1 = CC(I,1,K)-CC(I,3,K)
            TI2 = CC(I,1,K)+CC(I,3,K)
            TI3 = CC(I,2,K)+CC(I,4,K)
            TR4 = CC(I,4,K)-CC(I,2,K)
            TR1 = CC(I-1,1,K)-CC(I-1,3,K)
            TR2 = CC(I-1,1,K)+CC(I-1,3,K)
            TI4 = CC(I-1,2,K)-CC(I-1,4,K)
            TR3 = CC(I-1,2,K)+CC(I-1,4,K)
            CH(I-1,K,1) = TR2+TR3
            CR3 = TR2-TR3
            CH(I,K,1) = TI2+TI3
            CI3 = TI2-TI3
            CR2 = TR1+TR4
            CR4 = TR1-TR4
            CI2 = TI1+TI4
            CI4 = TI1-TI4
            CH(I-1,K,2) = WA1(I-1)*CR2-WA1(I)*CI2
            CH(I,K,2) = WA1(I-1)*CI2+WA1(I)*CR2
            CH(I-1,K,3) = WA2(I-1)*CR3-WA2(I)*CI3
            CH(I,K,3) = WA2(I-1)*CI3+WA2(I)*CR3
            CH(I-1,K,4) = WA3(I-1)*CR4-WA3(I)*CI4
            CH(I,K,4) = WA3(I-1)*CI4+WA3(I)*CR4
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSB5 (IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
      DIMENSION       CC(IDO,5,L1)           ,CH(IDO,L1,5)           ,
     1                WA1(*)     ,WA2(*)     ,WA3(*)     ,WA4(*)
      DATA TR11,TI11,TR12,TI12 /.309016994374947,.951056516295154,
     1-.809016994374947,.587785252292473/
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TI5 = CC(2,2,K)-CC(2,5,K)
         TI2 = CC(2,2,K)+CC(2,5,K)
         TI4 = CC(2,3,K)-CC(2,4,K)
         TI3 = CC(2,3,K)+CC(2,4,K)
         TR5 = CC(1,2,K)-CC(1,5,K)
         TR2 = CC(1,2,K)+CC(1,5,K)
         TR4 = CC(1,3,K)-CC(1,4,K)
         TR3 = CC(1,3,K)+CC(1,4,K)
         CH(1,K,1) = CC(1,1,K)+TR2+TR3
         CH(2,K,1) = CC(2,1,K)+TI2+TI3
         CR2 = CC(1,1,K)+TR11*TR2+TR12*TR3
         CI2 = CC(2,1,K)+TR11*TI2+TR12*TI3
         CR3 = CC(1,1,K)+TR12*TR2+TR11*TR3
         CI3 = CC(2,1,K)+TR12*TI2+TR11*TI3
         CR5 = TI11*TR5+TI12*TR4
         CI5 = TI11*TI5+TI12*TI4
         CR4 = TI12*TR5-TI11*TR4
         CI4 = TI12*TI5-TI11*TI4
         CH(1,K,2) = CR2-CI5
         CH(1,K,5) = CR2+CI5
         CH(2,K,2) = CI2+CR5
         CH(2,K,3) = CI3+CR4
         CH(1,K,3) = CR3-CI4
         CH(1,K,4) = CR3+CI4
         CH(2,K,4) = CI3-CR4
         CH(2,K,5) = CI2-CR5
  101 CONTINUE
      RETURN
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TI5 = CC(I,2,K)-CC(I,5,K)
            TI2 = CC(I,2,K)+CC(I,5,K)
            TI4 = CC(I,3,K)-CC(I,4,K)
            TI3 = CC(I,3,K)+CC(I,4,K)
            TR5 = CC(I-1,2,K)-CC(I-1,5,K)
            TR2 = CC(I-1,2,K)+CC(I-1,5,K)
            TR4 = CC(I-1,3,K)-CC(I-1,4,K)
            TR3 = CC(I-1,3,K)+CC(I-1,4,K)
            CH(I-1,K,1) = CC(I-1,1,K)+TR2+TR3
            CH(I,K,1) = CC(I,1,K)+TI2+TI3
            CR2 = CC(I-1,1,K)+TR11*TR2+TR12*TR3
            CI2 = CC(I,1,K)+TR11*TI2+TR12*TI3
            CR3 = CC(I-1,1,K)+TR12*TR2+TR11*TR3
            CI3 = CC(I,1,K)+TR12*TI2+TR11*TI3
            CR5 = TI11*TR5+TI12*TR4
            CI5 = TI11*TI5+TI12*TI4
            CR4 = TI12*TR5-TI11*TR4
            CI4 = TI12*TI5-TI11*TI4
            DR3 = CR3-CI4
            DR4 = CR3+CI4
            DI3 = CI3+CR4
            DI4 = CI3-CR4
            DR5 = CR2+CI5
            DR2 = CR2-CI5
            DI5 = CI2-CR5
            DI2 = CI2+CR5
            CH(I-1,K,2) = WA1(I-1)*DR2-WA1(I)*DI2
            CH(I,K,2) = WA1(I-1)*DI2+WA1(I)*DR2
            CH(I-1,K,3) = WA2(I-1)*DR3-WA2(I)*DI3
            CH(I,K,3) = WA2(I-1)*DI3+WA2(I)*DR3
            CH(I-1,K,4) = WA3(I-1)*DR4-WA3(I)*DI4
            CH(I,K,4) = WA3(I-1)*DI4+WA3(I)*DR4
            CH(I-1,K,5) = WA4(I-1)*DR5-WA4(I)*DI5
            CH(I,K,5) = WA4(I-1)*DI5+WA4(I)*DR5
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSB (NAC,IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DIMENSION       CH(IDO,L1,IP)          ,CC(IDO,IP,L1)          ,
     1                C1(IDO,L1,IP)          ,WA(*)      ,C2(IDL1,IP),
     2                CH2(IDL1,IP)
      IDOT = IDO/2
      NT = IP*IDL1
      IPP2 = IP+2
      IPPH = (IP+1)/2
      IDP = IP*IDO
C
      IF (IDO .LT. L1) GO TO 106
      DO 103 J=2,IPPH
         JC = IPP2-J
         DO 102 K=1,L1
            DO 101 I=1,IDO
               CH(I,K,J) = CC(I,J,K)+CC(I,JC,K)
               CH(I,K,JC) = CC(I,J,K)-CC(I,JC,K)
  101       CONTINUE
  102    CONTINUE
  103 CONTINUE
      DO 105 K=1,L1
         DO 104 I=1,IDO
            CH(I,K,1) = CC(I,1,K)
  104    CONTINUE
  105 CONTINUE
      GO TO 112
  106 DO 109 J=2,IPPH
         JC = IPP2-J
         DO 108 I=1,IDO
            DO 107 K=1,L1
               CH(I,K,J) = CC(I,J,K)+CC(I,JC,K)
               CH(I,K,JC) = CC(I,J,K)-CC(I,JC,K)
  107       CONTINUE
  108    CONTINUE
  109 CONTINUE
      DO 111 I=1,IDO
         DO 110 K=1,L1
            CH(I,K,1) = CC(I,1,K)
  110    CONTINUE
  111 CONTINUE
  112 IDL = 2-IDO
      INC = 0
      DO 116 L=2,IPPH
         LC = IPP2-L
         IDL = IDL+IDO
         DO 113 IK=1,IDL1
            C2(IK,L) = CH2(IK,1)+WA(IDL-1)*CH2(IK,2)
            C2(IK,LC) = WA(IDL)*CH2(IK,IP)
  113    CONTINUE
         IDLJ = IDL
         INC = INC+IDO
         DO 115 J=3,IPPH
            JC = IPP2-J
            IDLJ = IDLJ+INC
            IF (IDLJ .GT. IDP) IDLJ = IDLJ-IDP
            WAR = WA(IDLJ-1)
            WAI = WA(IDLJ)
            DO 114 IK=1,IDL1
               C2(IK,L) = C2(IK,L)+WAR*CH2(IK,J)
               C2(IK,LC) = C2(IK,LC)+WAI*CH2(IK,JC)
  114       CONTINUE
  115    CONTINUE
  116 CONTINUE
      DO 118 J=2,IPPH
         DO 117 IK=1,IDL1
            CH2(IK,1) = CH2(IK,1)+CH2(IK,J)
  117    CONTINUE
  118 CONTINUE
      DO 120 J=2,IPPH
         JC = IPP2-J
         DO 119 IK=2,IDL1,2
            CH2(IK-1,J) = C2(IK-1,J)-C2(IK,JC)
            CH2(IK-1,JC) = C2(IK-1,J)+C2(IK,JC)
            CH2(IK,J) = C2(IK,J)+C2(IK-1,JC)
            CH2(IK,JC) = C2(IK,J)-C2(IK-1,JC)
  119    CONTINUE
  120 CONTINUE
      NAC = 1
      IF (IDO .EQ. 2) RETURN
      NAC = 0
      DO 121 IK=1,IDL1
         C2(IK,1) = CH2(IK,1)
  121 CONTINUE
      DO 123 J=2,IP
         DO 122 K=1,L1
            C1(1,K,J) = CH(1,K,J)
            C1(2,K,J) = CH(2,K,J)
  122    CONTINUE
  123 CONTINUE
      IF (IDOT .GT. L1) GO TO 127
      IDIJ = 0
      DO 126 J=2,IP
         IDIJ = IDIJ+2
         DO 125 I=4,IDO,2
            IDIJ = IDIJ+2
            DO 124 K=1,L1
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)-WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)+WA(IDIJ)*CH(I-1,K,J)
  124       CONTINUE
  125    CONTINUE
  126 CONTINUE
      RETURN
  127 IDJ = 2-IDO
      DO 130 J=2,IP
         IDJ = IDJ+IDO
         DO 129 K=1,L1
            IDIJ = IDJ
            DO 128 I=4,IDO,2
               IDIJ = IDIJ+2
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)-WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)+WA(IDIJ)*CH(I-1,K,J)
  128       CONTINUE
  129    CONTINUE
  130 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSF2 (IDO,L1,CC,CH,WA1)
      DIMENSION       CC(IDO,2,L1)           ,CH(IDO,L1,2)           ,
     1                WA1(*)
      IF (IDO .GT. 2) GO TO 102
      DO 101 K=1,L1
         CH(1,K,1) = CC(1,1,K)+CC(1,2,K)
         CH(1,K,2) = CC(1,1,K)-CC(1,2,K)
         CH(2,K,1) = CC(2,1,K)+CC(2,2,K)
         CH(2,K,2) = CC(2,1,K)-CC(2,2,K)
  101 CONTINUE
      RETURN
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            CH(I-1,K,1) = CC(I-1,1,K)+CC(I-1,2,K)
            TR2 = CC(I-1,1,K)-CC(I-1,2,K)
            CH(I,K,1) = CC(I,1,K)+CC(I,2,K)
            TI2 = CC(I,1,K)-CC(I,2,K)
            CH(I,K,2) = WA1(I-1)*TI2-WA1(I)*TR2
            CH(I-1,K,2) = WA1(I-1)*TR2+WA1(I)*TI2
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSF3 (IDO,L1,CC,CH,WA1,WA2)
      DIMENSION       CC(IDO,3,L1)           ,CH(IDO,L1,3)           ,
     1                WA1(*)     ,WA2(*)
      DATA TAUR,TAUI /-.5,-.866025403784439/
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TR2 = CC(1,2,K)+CC(1,3,K)
         CR2 = CC(1,1,K)+TAUR*TR2
         CH(1,K,1) = CC(1,1,K)+TR2
         TI2 = CC(2,2,K)+CC(2,3,K)
         CI2 = CC(2,1,K)+TAUR*TI2
         CH(2,K,1) = CC(2,1,K)+TI2
         CR3 = TAUI*(CC(1,2,K)-CC(1,3,K))
         CI3 = TAUI*(CC(2,2,K)-CC(2,3,K))
         CH(1,K,2) = CR2-CI3
         CH(1,K,3) = CR2+CI3
         CH(2,K,2) = CI2+CR3
         CH(2,K,3) = CI2-CR3
  101 CONTINUE
      RETURN
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TR2 = CC(I-1,2,K)+CC(I-1,3,K)
            CR2 = CC(I-1,1,K)+TAUR*TR2
            CH(I-1,K,1) = CC(I-1,1,K)+TR2
            TI2 = CC(I,2,K)+CC(I,3,K)
            CI2 = CC(I,1,K)+TAUR*TI2
            CH(I,K,1) = CC(I,1,K)+TI2
            CR3 = TAUI*(CC(I-1,2,K)-CC(I-1,3,K))
            CI3 = TAUI*(CC(I,2,K)-CC(I,3,K))
            DR2 = CR2-CI3
            DR3 = CR2+CI3
            DI2 = CI2+CR3
            DI3 = CI2-CR3
            CH(I,K,2) = WA1(I-1)*DI2-WA1(I)*DR2
            CH(I-1,K,2) = WA1(I-1)*DR2+WA1(I)*DI2
            CH(I,K,3) = WA2(I-1)*DI3-WA2(I)*DR3
            CH(I-1,K,3) = WA2(I-1)*DR3+WA2(I)*DI3
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSF4 (IDO,L1,CC,CH,WA1,WA2,WA3)
      DIMENSION       CC(IDO,4,L1)           ,CH(IDO,L1,4)           ,
     1                WA1(*)     ,WA2(*)     ,WA3(*)
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TI1 = CC(2,1,K)-CC(2,3,K)
         TI2 = CC(2,1,K)+CC(2,3,K)
         TR4 = CC(2,2,K)-CC(2,4,K)
         TI3 = CC(2,2,K)+CC(2,4,K)
         TR1 = CC(1,1,K)-CC(1,3,K)
         TR2 = CC(1,1,K)+CC(1,3,K)
         TI4 = CC(1,4,K)-CC(1,2,K)
         TR3 = CC(1,2,K)+CC(1,4,K)
         CH(1,K,1) = TR2+TR3
         CH(1,K,3) = TR2-TR3
         CH(2,K,1) = TI2+TI3
         CH(2,K,3) = TI2-TI3
         CH(1,K,2) = TR1+TR4
         CH(1,K,4) = TR1-TR4
         CH(2,K,2) = TI1+TI4
         CH(2,K,4) = TI1-TI4
  101 CONTINUE
      RETURN
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TI1 = CC(I,1,K)-CC(I,3,K)
            TI2 = CC(I,1,K)+CC(I,3,K)
            TI3 = CC(I,2,K)+CC(I,4,K)
            TR4 = CC(I,2,K)-CC(I,4,K)
            TR1 = CC(I-1,1,K)-CC(I-1,3,K)
            TR2 = CC(I-1,1,K)+CC(I-1,3,K)
            TI4 = CC(I-1,4,K)-CC(I-1,2,K)
            TR3 = CC(I-1,2,K)+CC(I-1,4,K)
            CH(I-1,K,1) = TR2+TR3
            CR3 = TR2-TR3
            CH(I,K,1) = TI2+TI3
            CI3 = TI2-TI3
            CR2 = TR1+TR4
            CR4 = TR1-TR4
            CI2 = TI1+TI4
            CI4 = TI1-TI4
            CH(I-1,K,2) = WA1(I-1)*CR2+WA1(I)*CI2
            CH(I,K,2) = WA1(I-1)*CI2-WA1(I)*CR2
            CH(I-1,K,3) = WA2(I-1)*CR3+WA2(I)*CI3
            CH(I,K,3) = WA2(I-1)*CI3-WA2(I)*CR3
            CH(I-1,K,4) = WA3(I-1)*CR4+WA3(I)*CI4
            CH(I,K,4) = WA3(I-1)*CI4-WA3(I)*CR4
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSF5 (IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
      DIMENSION       CC(IDO,5,L1)           ,CH(IDO,L1,5)           ,
     1                WA1(*)     ,WA2(*)     ,WA3(*)     ,WA4(*)
      DATA TR11,TI11,TR12,TI12 /.309016994374947,-.951056516295154,
     1-.809016994374947,-.587785252292473/
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TI5 = CC(2,2,K)-CC(2,5,K)
         TI2 = CC(2,2,K)+CC(2,5,K)
         TI4 = CC(2,3,K)-CC(2,4,K)
         TI3 = CC(2,3,K)+CC(2,4,K)
         TR5 = CC(1,2,K)-CC(1,5,K)
         TR2 = CC(1,2,K)+CC(1,5,K)
         TR4 = CC(1,3,K)-CC(1,4,K)
         TR3 = CC(1,3,K)+CC(1,4,K)
         CH(1,K,1) = CC(1,1,K)+TR2+TR3
         CH(2,K,1) = CC(2,1,K)+TI2+TI3
         CR2 = CC(1,1,K)+TR11*TR2+TR12*TR3
         CI2 = CC(2,1,K)+TR11*TI2+TR12*TI3
         CR3 = CC(1,1,K)+TR12*TR2+TR11*TR3
         CI3 = CC(2,1,K)+TR12*TI2+TR11*TI3
         CR5 = TI11*TR5+TI12*TR4
         CI5 = TI11*TI5+TI12*TI4
         CR4 = TI12*TR5-TI11*TR4
         CI4 = TI12*TI5-TI11*TI4
         CH(1,K,2) = CR2-CI5
         CH(1,K,5) = CR2+CI5
         CH(2,K,2) = CI2+CR5
         CH(2,K,3) = CI3+CR4
         CH(1,K,3) = CR3-CI4
         CH(1,K,4) = CR3+CI4
         CH(2,K,4) = CI3-CR4
         CH(2,K,5) = CI2-CR5
  101 CONTINUE
      RETURN
  102 DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TI5 = CC(I,2,K)-CC(I,5,K)
            TI2 = CC(I,2,K)+CC(I,5,K)
            TI4 = CC(I,3,K)-CC(I,4,K)
            TI3 = CC(I,3,K)+CC(I,4,K)
            TR5 = CC(I-1,2,K)-CC(I-1,5,K)
            TR2 = CC(I-1,2,K)+CC(I-1,5,K)
            TR4 = CC(I-1,3,K)-CC(I-1,4,K)
            TR3 = CC(I-1,3,K)+CC(I-1,4,K)
            CH(I-1,K,1) = CC(I-1,1,K)+TR2+TR3
            CH(I,K,1) = CC(I,1,K)+TI2+TI3
            CR2 = CC(I-1,1,K)+TR11*TR2+TR12*TR3
            CI2 = CC(I,1,K)+TR11*TI2+TR12*TI3
            CR3 = CC(I-1,1,K)+TR12*TR2+TR11*TR3
            CI3 = CC(I,1,K)+TR12*TI2+TR11*TI3
            CR5 = TI11*TR5+TI12*TR4
            CI5 = TI11*TI5+TI12*TI4
            CR4 = TI12*TR5-TI11*TR4
            CI4 = TI12*TI5-TI11*TI4
            DR3 = CR3-CI4
            DR4 = CR3+CI4
            DI3 = CI3+CR4
            DI4 = CI3-CR4
            DR5 = CR2+CI5
            DR2 = CR2-CI5
            DI5 = CI2-CR5
            DI2 = CI2+CR5
            CH(I-1,K,2) = WA1(I-1)*DR2+WA1(I)*DI2
            CH(I,K,2) = WA1(I-1)*DI2-WA1(I)*DR2
            CH(I-1,K,3) = WA2(I-1)*DR3+WA2(I)*DI3
            CH(I,K,3) = WA2(I-1)*DI3-WA2(I)*DR3
            CH(I-1,K,4) = WA3(I-1)*DR4+WA3(I)*DI4
            CH(I,K,4) = WA3(I-1)*DI4-WA3(I)*DR4
            CH(I-1,K,5) = WA4(I-1)*DR5+WA4(I)*DI5
            CH(I,K,5) = WA4(I-1)*DI5-WA4(I)*DR5
  103    CONTINUE
  104 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_PASSF (NAC,IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DIMENSION       CH(IDO,L1,IP)          ,CC(IDO,IP,L1)          ,
     1                C1(IDO,L1,IP)          ,WA(*)      ,C2(IDL1,IP),
     2                CH2(IDL1,IP)
      IDOT = IDO/2
      NT = IP*IDL1
      IPP2 = IP+2
      IPPH = (IP+1)/2
      IDP = IP*IDO
C
      IF (IDO .LT. L1) GO TO 106
      DO 103 J=2,IPPH
         JC = IPP2-J
         DO 102 K=1,L1
            DO 101 I=1,IDO
               CH(I,K,J) = CC(I,J,K)+CC(I,JC,K)
               CH(I,K,JC) = CC(I,J,K)-CC(I,JC,K)
  101       CONTINUE
  102    CONTINUE
  103 CONTINUE
      DO 105 K=1,L1
         DO 104 I=1,IDO
            CH(I,K,1) = CC(I,1,K)
  104    CONTINUE
  105 CONTINUE
      GO TO 112
  106 DO 109 J=2,IPPH
         JC = IPP2-J
         DO 108 I=1,IDO
            DO 107 K=1,L1
               CH(I,K,J) = CC(I,J,K)+CC(I,JC,K)
               CH(I,K,JC) = CC(I,J,K)-CC(I,JC,K)
  107       CONTINUE
  108    CONTINUE
  109 CONTINUE
      DO 111 I=1,IDO
         DO 110 K=1,L1
            CH(I,K,1) = CC(I,1,K)
  110    CONTINUE
  111 CONTINUE
  112 IDL = 2-IDO
      INC = 0
      DO 116 L=2,IPPH
         LC = IPP2-L
         IDL = IDL+IDO
         DO 113 IK=1,IDL1
            C2(IK,L) = CH2(IK,1)+WA(IDL-1)*CH2(IK,2)
            C2(IK,LC) = -WA(IDL)*CH2(IK,IP)
  113    CONTINUE
         IDLJ = IDL
         INC = INC+IDO
         DO 115 J=3,IPPH
            JC = IPP2-J
            IDLJ = IDLJ+INC
            IF (IDLJ .GT. IDP) IDLJ = IDLJ-IDP
            WAR = WA(IDLJ-1)
            WAI = WA(IDLJ)
            DO 114 IK=1,IDL1
               C2(IK,L) = C2(IK,L)+WAR*CH2(IK,J)
               C2(IK,LC) = C2(IK,LC)-WAI*CH2(IK,JC)
  114       CONTINUE
  115    CONTINUE
  116 CONTINUE
      DO 118 J=2,IPPH
         DO 117 IK=1,IDL1
            CH2(IK,1) = CH2(IK,1)+CH2(IK,J)
  117    CONTINUE
  118 CONTINUE
      DO 120 J=2,IPPH
         JC = IPP2-J
         DO 119 IK=2,IDL1,2
            CH2(IK-1,J) = C2(IK-1,J)-C2(IK,JC)
            CH2(IK-1,JC) = C2(IK-1,J)+C2(IK,JC)
            CH2(IK,J) = C2(IK,J)+C2(IK-1,JC)
            CH2(IK,JC) = C2(IK,J)-C2(IK-1,JC)
  119    CONTINUE
  120 CONTINUE
      NAC = 1
      IF (IDO .EQ. 2) RETURN
      NAC = 0
      DO 121 IK=1,IDL1
         C2(IK,1) = CH2(IK,1)
  121 CONTINUE
      DO 123 J=2,IP
         DO 122 K=1,L1
            C1(1,K,J) = CH(1,K,J)
            C1(2,K,J) = CH(2,K,J)
  122    CONTINUE
  123 CONTINUE
      IF (IDOT .GT. L1) GO TO 127
      IDIJ = 0
      DO 126 J=2,IP
         IDIJ = IDIJ+2
         DO 125 I=4,IDO,2
            IDIJ = IDIJ+2
            DO 124 K=1,L1
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)+WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)-WA(IDIJ)*CH(I-1,K,J)
  124       CONTINUE
  125    CONTINUE
  126 CONTINUE
      RETURN
  127 IDJ = 2-IDO
      DO 130 J=2,IP
         IDJ = IDJ+IDO
         DO 129 K=1,L1
            IDIJ = IDJ
            DO 128 I=4,IDO,2
               IDIJ = IDIJ+2
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)+WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)-WA(IDIJ)*CH(I-1,K,J)
  128       CONTINUE
  129    CONTINUE
  130 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_RADB2 (IDO,L1,CC,CH,WA1)
      DIMENSION       CC(IDO,2,L1)           ,CH(IDO,L1,2)           ,
     1                WA1(*)
      DO 101 K=1,L1
         CH(1,K,1) = CC(1,1,K)+CC(IDO,2,K)
         CH(1,K,2) = CC(1,1,K)-CC(IDO,2,K)
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            CH(I-1,K,1) = CC(I-1,1,K)+CC(IC-1,2,K)
            TR2 = CC(I-1,1,K)-CC(IC-1,2,K)
            CH(I,K,1) = CC(I,1,K)-CC(IC,2,K)
            TI2 = CC(I,1,K)+CC(IC,2,K)
            CH(I-1,K,2) = WA1(I-2)*TR2-WA1(I-1)*TI2
            CH(I,K,2) = WA1(I-2)*TI2+WA1(I-1)*TR2
  103    CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
  105 DO 106 K=1,L1
         CH(IDO,K,1) = CC(IDO,1,K)+CC(IDO,1,K)
         CH(IDO,K,2) = -(CC(1,2,K)+CC(1,2,K))
  106 CONTINUE
  107 RETURN
      END
      SUBROUTINE PDA_RADB3 (IDO,L1,CC,CH,WA1,WA2)
      DIMENSION       CC(IDO,3,L1)           ,CH(IDO,L1,3)           ,
     1                WA1(*)     ,WA2(*)
      DATA TAUR,TAUI /-.5,.866025403784439/
      DO 101 K=1,L1
         TR2 = CC(IDO,2,K)+CC(IDO,2,K)
         CR2 = CC(1,1,K)+TAUR*TR2
         CH(1,K,1) = CC(1,1,K)+TR2
         CI3 = TAUI*(CC(1,3,K)+CC(1,3,K))
         CH(1,K,2) = CR2-CI3
         CH(1,K,3) = CR2+CI3
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            TR2 = CC(I-1,3,K)+CC(IC-1,2,K)
            CR2 = CC(I-1,1,K)+TAUR*TR2
            CH(I-1,K,1) = CC(I-1,1,K)+TR2
            TI2 = CC(I,3,K)-CC(IC,2,K)
            CI2 = CC(I,1,K)+TAUR*TI2
            CH(I,K,1) = CC(I,1,K)+TI2
            CR3 = TAUI*(CC(I-1,3,K)-CC(IC-1,2,K))
            CI3 = TAUI*(CC(I,3,K)+CC(IC,2,K))
            DR2 = CR2-CI3
            DR3 = CR2+CI3
            DI2 = CI2+CR3
            DI3 = CI2-CR3
            CH(I-1,K,2) = WA1(I-2)*DR2-WA1(I-1)*DI2
            CH(I,K,2) = WA1(I-2)*DI2+WA1(I-1)*DR2
            CH(I-1,K,3) = WA2(I-2)*DR3-WA2(I-1)*DI3
            CH(I,K,3) = WA2(I-2)*DI3+WA2(I-1)*DR3
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_RADB4 (IDO,L1,CC,CH,WA1,WA2,WA3)
      DIMENSION       CC(IDO,4,L1)           ,CH(IDO,L1,4)           ,
     1                WA1(*)     ,WA2(*)     ,WA3(*)
      DATA SQRT2 /1.414213562373095/
      DO 101 K=1,L1
         TR1 = CC(1,1,K)-CC(IDO,4,K)
         TR2 = CC(1,1,K)+CC(IDO,4,K)
         TR3 = CC(IDO,2,K)+CC(IDO,2,K)
         TR4 = CC(1,3,K)+CC(1,3,K)
         CH(1,K,1) = TR2+TR3
         CH(1,K,2) = TR1-TR4
         CH(1,K,3) = TR2-TR3
         CH(1,K,4) = TR1+TR4
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            TI1 = CC(I,1,K)+CC(IC,4,K)
            TI2 = CC(I,1,K)-CC(IC,4,K)
            TI3 = CC(I,3,K)-CC(IC,2,K)
            TR4 = CC(I,3,K)+CC(IC,2,K)
            TR1 = CC(I-1,1,K)-CC(IC-1,4,K)
            TR2 = CC(I-1,1,K)+CC(IC-1,4,K)
            TI4 = CC(I-1,3,K)-CC(IC-1,2,K)
            TR3 = CC(I-1,3,K)+CC(IC-1,2,K)
            CH(I-1,K,1) = TR2+TR3
            CR3 = TR2-TR3
            CH(I,K,1) = TI2+TI3
            CI3 = TI2-TI3
            CR2 = TR1-TR4
            CR4 = TR1+TR4
            CI2 = TI1+TI4
            CI4 = TI1-TI4
            CH(I-1,K,2) = WA1(I-2)*CR2-WA1(I-1)*CI2
            CH(I,K,2) = WA1(I-2)*CI2+WA1(I-1)*CR2
            CH(I-1,K,3) = WA2(I-2)*CR3-WA2(I-1)*CI3
            CH(I,K,3) = WA2(I-2)*CI3+WA2(I-1)*CR3
            CH(I-1,K,4) = WA3(I-2)*CR4-WA3(I-1)*CI4
            CH(I,K,4) = WA3(I-2)*CI4+WA3(I-1)*CR4
  103    CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
  105 CONTINUE
      DO 106 K=1,L1
         TI1 = CC(1,2,K)+CC(1,4,K)
         TI2 = CC(1,4,K)-CC(1,2,K)
         TR1 = CC(IDO,1,K)-CC(IDO,3,K)
         TR2 = CC(IDO,1,K)+CC(IDO,3,K)
         CH(IDO,K,1) = TR2+TR2
         CH(IDO,K,2) = SQRT2*(TR1-TI1)
         CH(IDO,K,3) = TI2+TI2
         CH(IDO,K,4) = -SQRT2*(TR1+TI1)
  106 CONTINUE
  107 RETURN
      END
      SUBROUTINE PDA_RADB5 (IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
      DIMENSION       CC(IDO,5,L1)           ,CH(IDO,L1,5)           ,
     1                WA1(*)     ,WA2(*)     ,WA3(*)     ,WA4(*)
      DATA TR11,TI11,TR12,TI12 /.309016994374947,.951056516295154,
     1-.809016994374947,.587785252292473/
      DO 101 K=1,L1
         TI5 = CC(1,3,K)+CC(1,3,K)
         TI4 = CC(1,5,K)+CC(1,5,K)
         TR2 = CC(IDO,2,K)+CC(IDO,2,K)
         TR3 = CC(IDO,4,K)+CC(IDO,4,K)
         CH(1,K,1) = CC(1,1,K)+TR2+TR3
         CR2 = CC(1,1,K)+TR11*TR2+TR12*TR3
         CR3 = CC(1,1,K)+TR12*TR2+TR11*TR3
         CI5 = TI11*TI5+TI12*TI4
         CI4 = TI12*TI5-TI11*TI4
         CH(1,K,2) = CR2-CI5
         CH(1,K,3) = CR3-CI4
         CH(1,K,4) = CR3+CI4
         CH(1,K,5) = CR2+CI5
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            TI5 = CC(I,3,K)+CC(IC,2,K)
            TI2 = CC(I,3,K)-CC(IC,2,K)
            TI4 = CC(I,5,K)+CC(IC,4,K)
            TI3 = CC(I,5,K)-CC(IC,4,K)
            TR5 = CC(I-1,3,K)-CC(IC-1,2,K)
            TR2 = CC(I-1,3,K)+CC(IC-1,2,K)
            TR4 = CC(I-1,5,K)-CC(IC-1,4,K)
            TR3 = CC(I-1,5,K)+CC(IC-1,4,K)
            CH(I-1,K,1) = CC(I-1,1,K)+TR2+TR3
            CH(I,K,1) = CC(I,1,K)+TI2+TI3
            CR2 = CC(I-1,1,K)+TR11*TR2+TR12*TR3
            CI2 = CC(I,1,K)+TR11*TI2+TR12*TI3
            CR3 = CC(I-1,1,K)+TR12*TR2+TR11*TR3
            CI3 = CC(I,1,K)+TR12*TI2+TR11*TI3
            CR5 = TI11*TR5+TI12*TR4
            CI5 = TI11*TI5+TI12*TI4
            CR4 = TI12*TR5-TI11*TR4
            CI4 = TI12*TI5-TI11*TI4
            DR3 = CR3-CI4
            DR4 = CR3+CI4
            DI3 = CI3+CR4
            DI4 = CI3-CR4
            DR5 = CR2+CI5
            DR2 = CR2-CI5
            DI5 = CI2-CR5
            DI2 = CI2+CR5
            CH(I-1,K,2) = WA1(I-2)*DR2-WA1(I-1)*DI2
            CH(I,K,2) = WA1(I-2)*DI2+WA1(I-1)*DR2
            CH(I-1,K,3) = WA2(I-2)*DR3-WA2(I-1)*DI3
            CH(I,K,3) = WA2(I-2)*DI3+WA2(I-1)*DR3
            CH(I-1,K,4) = WA3(I-2)*DR4-WA3(I-1)*DI4
            CH(I,K,4) = WA3(I-2)*DI4+WA3(I-1)*DR4
            CH(I-1,K,5) = WA4(I-2)*DR5-WA4(I-1)*DI5
            CH(I,K,5) = WA4(I-2)*DI5+WA4(I-1)*DR5
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_RADBG (IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DIMENSION       CH(IDO,L1,IP)          ,CC(IDO,IP,L1)          ,
     1                C1(IDO,L1,IP)          ,C2(IDL1,IP),
     2                CH2(IDL1,IP)           ,WA(*)
      DATA TPI/6.28318530717959/
      ARG = TPI/FLOAT(IP)
      DCP = COS(ARG)
      DSP = SIN(ARG)
      IDP2 = IDO+2
      NBD = (IDO-1)/2
      IPP2 = IP+2
      IPPH = (IP+1)/2
      IF (IDO .LT. L1) GO TO 103
      DO 102 K=1,L1
         DO 101 I=1,IDO
            CH(I,K,1) = CC(I,1,K)
  101    CONTINUE
  102 CONTINUE
      GO TO 106
  103 DO 105 I=1,IDO
         DO 104 K=1,L1
            CH(I,K,1) = CC(I,1,K)
  104    CONTINUE
  105 CONTINUE
  106 DO 108 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 107 K=1,L1
            CH(1,K,J) = CC(IDO,J2-2,K)+CC(IDO,J2-2,K)
            CH(1,K,JC) = CC(1,J2-1,K)+CC(1,J2-1,K)
  107    CONTINUE
  108 CONTINUE
      IF (IDO .EQ. 1) GO TO 116
      IF (NBD .LT. L1) GO TO 112
      DO 111 J=2,IPPH
         JC = IPP2-J
         DO 110 K=1,L1
            DO 109 I=3,IDO,2
               IC = IDP2-I
               CH(I-1,K,J) = CC(I-1,2*J-1,K)+CC(IC-1,2*J-2,K)
               CH(I-1,K,JC) = CC(I-1,2*J-1,K)-CC(IC-1,2*J-2,K)
               CH(I,K,J) = CC(I,2*J-1,K)-CC(IC,2*J-2,K)
               CH(I,K,JC) = CC(I,2*J-1,K)+CC(IC,2*J-2,K)
  109       CONTINUE
  110    CONTINUE
  111 CONTINUE
      GO TO 116
  112 DO 115 J=2,IPPH
         JC = IPP2-J
         DO 114 I=3,IDO,2
            IC = IDP2-I
            DO 113 K=1,L1
               CH(I-1,K,J) = CC(I-1,2*J-1,K)+CC(IC-1,2*J-2,K)
               CH(I-1,K,JC) = CC(I-1,2*J-1,K)-CC(IC-1,2*J-2,K)
               CH(I,K,J) = CC(I,2*J-1,K)-CC(IC,2*J-2,K)
               CH(I,K,JC) = CC(I,2*J-1,K)+CC(IC,2*J-2,K)
  113       CONTINUE
  114    CONTINUE
  115 CONTINUE
  116 AR1 = 1.
      AI1 = 0.
      DO 120 L=2,IPPH
         LC = IPP2-L
         AR1H = DCP*AR1-DSP*AI1
         AI1 = DCP*AI1+DSP*AR1
         AR1 = AR1H
         DO 117 IK=1,IDL1
            C2(IK,L) = CH2(IK,1)+AR1*CH2(IK,2)
            C2(IK,LC) = AI1*CH2(IK,IP)
  117    CONTINUE
         DC2 = AR1
         DS2 = AI1
         AR2 = AR1
         AI2 = AI1
         DO 119 J=3,IPPH
            JC = IPP2-J
            AR2H = DC2*AR2-DS2*AI2
            AI2 = DC2*AI2+DS2*AR2
            AR2 = AR2H
            DO 118 IK=1,IDL1
               C2(IK,L) = C2(IK,L)+AR2*CH2(IK,J)
               C2(IK,LC) = C2(IK,LC)+AI2*CH2(IK,JC)
  118       CONTINUE
  119    CONTINUE
  120 CONTINUE
      DO 122 J=2,IPPH
         DO 121 IK=1,IDL1
            CH2(IK,1) = CH2(IK,1)+CH2(IK,J)
  121    CONTINUE
  122 CONTINUE
      DO 124 J=2,IPPH
         JC = IPP2-J
         DO 123 K=1,L1
            CH(1,K,J) = C1(1,K,J)-C1(1,K,JC)
            CH(1,K,JC) = C1(1,K,J)+C1(1,K,JC)
  123    CONTINUE
  124 CONTINUE
      IF (IDO .EQ. 1) GO TO 132
      IF (NBD .LT. L1) GO TO 128
      DO 127 J=2,IPPH
         JC = IPP2-J
         DO 126 K=1,L1
            DO 125 I=3,IDO,2
               CH(I-1,K,J) = C1(I-1,K,J)-C1(I,K,JC)
               CH(I-1,K,JC) = C1(I-1,K,J)+C1(I,K,JC)
               CH(I,K,J) = C1(I,K,J)+C1(I-1,K,JC)
               CH(I,K,JC) = C1(I,K,J)-C1(I-1,K,JC)
  125       CONTINUE
  126    CONTINUE
  127 CONTINUE
      GO TO 132
  128 DO 131 J=2,IPPH
         JC = IPP2-J
         DO 130 I=3,IDO,2
            DO 129 K=1,L1
               CH(I-1,K,J) = C1(I-1,K,J)-C1(I,K,JC)
               CH(I-1,K,JC) = C1(I-1,K,J)+C1(I,K,JC)
               CH(I,K,J) = C1(I,K,J)+C1(I-1,K,JC)
               CH(I,K,JC) = C1(I,K,J)-C1(I-1,K,JC)
  129       CONTINUE
  130    CONTINUE
  131 CONTINUE
  132 CONTINUE
      IF (IDO .EQ. 1) RETURN
      DO 133 IK=1,IDL1
         C2(IK,1) = CH2(IK,1)
  133 CONTINUE
      DO 135 J=2,IP
         DO 134 K=1,L1
            C1(1,K,J) = CH(1,K,J)
  134    CONTINUE
  135 CONTINUE
      IF (NBD .GT. L1) GO TO 139
      IS = -IDO
      DO 138 J=2,IP
         IS = IS+IDO
         IDIJ = IS
         DO 137 I=3,IDO,2
            IDIJ = IDIJ+2
            DO 136 K=1,L1
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)-WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)+WA(IDIJ)*CH(I-1,K,J)
  136       CONTINUE
  137    CONTINUE
  138 CONTINUE
      GO TO 143
  139 IS = -IDO
      DO 142 J=2,IP
         IS = IS+IDO
         DO 141 K=1,L1
            IDIJ = IS
            DO 140 I=3,IDO,2
               IDIJ = IDIJ+2
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)-WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)+WA(IDIJ)*CH(I-1,K,J)
  140       CONTINUE
  141    CONTINUE
  142 CONTINUE
  143 RETURN
      END
      SUBROUTINE PDA_RADF2 (IDO,L1,CC,CH,WA1)
      DIMENSION       CH(IDO,2,L1)           ,CC(IDO,L1,2)           ,
     1                WA1(*)
      DO 101 K=1,L1
         CH(1,1,K) = CC(1,K,1)+CC(1,K,2)
         CH(IDO,2,K) = CC(1,K,1)-CC(1,K,2)
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            TR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)
            TI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)
            CH(I,1,K) = CC(I,K,1)+TI2
            CH(IC,2,K) = TI2-CC(I,K,1)
            CH(I-1,1,K) = CC(I-1,K,1)+TR2
            CH(IC-1,2,K) = CC(I-1,K,1)-TR2
  103    CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
  105 DO 106 K=1,L1
         CH(1,2,K) = -CC(IDO,K,2)
         CH(IDO,1,K) = CC(IDO,K,1)
  106 CONTINUE
  107 RETURN
      END
      SUBROUTINE PDA_RADF3 (IDO,L1,CC,CH,WA1,WA2)
      DIMENSION       CH(IDO,3,L1)           ,CC(IDO,L1,3)           ,
     1                WA1(*)     ,WA2(*)
      DATA TAUR,TAUI /-.5,.866025403784439/
      DO 101 K=1,L1
         CR2 = CC(1,K,2)+CC(1,K,3)
         CH(1,1,K) = CC(1,K,1)+CR2
         CH(1,3,K) = TAUI*(CC(1,K,3)-CC(1,K,2))
         CH(IDO,2,K) = CC(1,K,1)+TAUR*CR2
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            DR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)
            DI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)
            DR3 = WA2(I-2)*CC(I-1,K,3)+WA2(I-1)*CC(I,K,3)
            DI3 = WA2(I-2)*CC(I,K,3)-WA2(I-1)*CC(I-1,K,3)
            CR2 = DR2+DR3
            CI2 = DI2+DI3
            CH(I-1,1,K) = CC(I-1,K,1)+CR2
            CH(I,1,K) = CC(I,K,1)+CI2
            TR2 = CC(I-1,K,1)+TAUR*CR2
            TI2 = CC(I,K,1)+TAUR*CI2
            TR3 = TAUI*(DI2-DI3)
            TI3 = TAUI*(DR3-DR2)
            CH(I-1,3,K) = TR2+TR3
            CH(IC-1,2,K) = TR2-TR3
            CH(I,3,K) = TI2+TI3
            CH(IC,2,K) = TI3-TI2
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_RADF4 (IDO,L1,CC,CH,WA1,WA2,WA3)
      DIMENSION       CC(IDO,L1,4)           ,CH(IDO,4,L1)           ,
     1                WA1(*)     ,WA2(*)     ,WA3(*)
      DATA HSQT2 /.7071067811865475/
      DO 101 K=1,L1
         TR1 = CC(1,K,2)+CC(1,K,4)
         TR2 = CC(1,K,1)+CC(1,K,3)
         CH(1,1,K) = TR1+TR2
         CH(IDO,4,K) = TR2-TR1
         CH(IDO,2,K) = CC(1,K,1)-CC(1,K,3)
         CH(1,3,K) = CC(1,K,4)-CC(1,K,2)
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            CR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)
            CI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)
            CR3 = WA2(I-2)*CC(I-1,K,3)+WA2(I-1)*CC(I,K,3)
            CI3 = WA2(I-2)*CC(I,K,3)-WA2(I-1)*CC(I-1,K,3)
            CR4 = WA3(I-2)*CC(I-1,K,4)+WA3(I-1)*CC(I,K,4)
            CI4 = WA3(I-2)*CC(I,K,4)-WA3(I-1)*CC(I-1,K,4)
            TR1 = CR2+CR4
            TR4 = CR4-CR2
            TI1 = CI2+CI4
            TI4 = CI2-CI4
            TI2 = CC(I,K,1)+CI3
            TI3 = CC(I,K,1)-CI3
            TR2 = CC(I-1,K,1)+CR3
            TR3 = CC(I-1,K,1)-CR3
            CH(I-1,1,K) = TR1+TR2
            CH(IC-1,4,K) = TR2-TR1
            CH(I,1,K) = TI1+TI2
            CH(IC,4,K) = TI1-TI2
            CH(I-1,3,K) = TI4+TR3
            CH(IC-1,2,K) = TR3-TI4
            CH(I,3,K) = TR4+TI3
            CH(IC,2,K) = TR4-TI3
  103    CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
  105 CONTINUE
      DO 106 K=1,L1
         TI1 = -HSQT2*(CC(IDO,K,2)+CC(IDO,K,4))
         TR1 = HSQT2*(CC(IDO,K,2)-CC(IDO,K,4))
         CH(IDO,1,K) = TR1+CC(IDO,K,1)
         CH(IDO,3,K) = CC(IDO,K,1)-TR1
         CH(1,2,K) = TI1-CC(IDO,K,3)
         CH(1,4,K) = TI1+CC(IDO,K,3)
  106 CONTINUE
  107 RETURN
      END
      SUBROUTINE PDA_RADF5 (IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
      DIMENSION       CC(IDO,L1,5)           ,CH(IDO,5,L1)           ,
     1                WA1(*)     ,WA2(*)     ,WA3(*)     ,WA4(*)
      DATA TR11,TI11,TR12,TI12 /.309016994374947,.951056516295154,
     1-.809016994374947,.587785252292473/
      DO 101 K=1,L1
         CR2 = CC(1,K,5)+CC(1,K,2)
         CI5 = CC(1,K,5)-CC(1,K,2)
         CR3 = CC(1,K,4)+CC(1,K,3)
         CI4 = CC(1,K,4)-CC(1,K,3)
         CH(1,1,K) = CC(1,K,1)+CR2+CR3
         CH(IDO,2,K) = CC(1,K,1)+TR11*CR2+TR12*CR3
         CH(1,3,K) = TI11*CI5+TI12*CI4
         CH(IDO,4,K) = CC(1,K,1)+TR12*CR2+TR11*CR3
         CH(1,5,K) = TI12*CI5-TI11*CI4
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            DR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)
            DI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)
            DR3 = WA2(I-2)*CC(I-1,K,3)+WA2(I-1)*CC(I,K,3)
            DI3 = WA2(I-2)*CC(I,K,3)-WA2(I-1)*CC(I-1,K,3)
            DR4 = WA3(I-2)*CC(I-1,K,4)+WA3(I-1)*CC(I,K,4)
            DI4 = WA3(I-2)*CC(I,K,4)-WA3(I-1)*CC(I-1,K,4)
            DR5 = WA4(I-2)*CC(I-1,K,5)+WA4(I-1)*CC(I,K,5)
            DI5 = WA4(I-2)*CC(I,K,5)-WA4(I-1)*CC(I-1,K,5)
            CR2 = DR2+DR5
            CI5 = DR5-DR2
            CR5 = DI2-DI5
            CI2 = DI2+DI5
            CR3 = DR3+DR4
            CI4 = DR4-DR3
            CR4 = DI3-DI4
            CI3 = DI3+DI4
            CH(I-1,1,K) = CC(I-1,K,1)+CR2+CR3
            CH(I,1,K) = CC(I,K,1)+CI2+CI3
            TR2 = CC(I-1,K,1)+TR11*CR2+TR12*CR3
            TI2 = CC(I,K,1)+TR11*CI2+TR12*CI3
            TR3 = CC(I-1,K,1)+TR12*CR2+TR11*CR3
            TI3 = CC(I,K,1)+TR12*CI2+TR11*CI3
            TR5 = TI11*CR5+TI12*CR4
            TI5 = TI11*CI5+TI12*CI4
            TR4 = TI12*CR5-TI11*CR4
            TI4 = TI12*CI5-TI11*CI4
            CH(I-1,3,K) = TR2+TR5
            CH(IC-1,2,K) = TR2-TR5
            CH(I,3,K) = TI2+TI5
            CH(IC,2,K) = TI5-TI2
            CH(I-1,5,K) = TR3+TR4
            CH(IC-1,4,K) = TR3-TR4
            CH(I,5,K) = TI3+TI4
            CH(IC,4,K) = TI4-TI3
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
      SUBROUTINE PDA_RADFG (IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DIMENSION       CH(IDO,L1,IP)          ,CC(IDO,IP,L1)          ,
     1                C1(IDO,L1,IP)          ,C2(IDL1,IP),
     2                CH2(IDL1,IP)           ,WA(*)
      DATA TPI/6.28318530717959/
      ARG = TPI/FLOAT(IP)
      DCP = COS(ARG)
      DSP = SIN(ARG)
      IPPH = (IP+1)/2
      IPP2 = IP+2
      IDP2 = IDO+2
      NBD = (IDO-1)/2
      IF (IDO .EQ. 1) GO TO 119
      DO 101 IK=1,IDL1
         CH2(IK,1) = C2(IK,1)
  101 CONTINUE
      DO 103 J=2,IP
         DO 102 K=1,L1
            CH(1,K,J) = C1(1,K,J)
  102    CONTINUE
  103 CONTINUE
      IF (NBD .GT. L1) GO TO 107
      IS = -IDO
      DO 106 J=2,IP
         IS = IS+IDO
         IDIJ = IS
         DO 105 I=3,IDO,2
            IDIJ = IDIJ+2
            DO 104 K=1,L1
               CH(I-1,K,J) = WA(IDIJ-1)*C1(I-1,K,J)+WA(IDIJ)*C1(I,K,J)
               CH(I,K,J) = WA(IDIJ-1)*C1(I,K,J)-WA(IDIJ)*C1(I-1,K,J)
  104       CONTINUE
  105    CONTINUE
  106 CONTINUE
      GO TO 111
  107 IS = -IDO
      DO 110 J=2,IP
         IS = IS+IDO
         DO 109 K=1,L1
            IDIJ = IS
            DO 108 I=3,IDO,2
               IDIJ = IDIJ+2
               CH(I-1,K,J) = WA(IDIJ-1)*C1(I-1,K,J)+WA(IDIJ)*C1(I,K,J)
               CH(I,K,J) = WA(IDIJ-1)*C1(I,K,J)-WA(IDIJ)*C1(I-1,K,J)
  108       CONTINUE
  109    CONTINUE
  110 CONTINUE
  111 IF (NBD .LT. L1) GO TO 115
      DO 114 J=2,IPPH
         JC = IPP2-J
         DO 113 K=1,L1
            DO 112 I=3,IDO,2
               C1(I-1,K,J) = CH(I-1,K,J)+CH(I-1,K,JC)
               C1(I-1,K,JC) = CH(I,K,J)-CH(I,K,JC)
               C1(I,K,J) = CH(I,K,J)+CH(I,K,JC)
               C1(I,K,JC) = CH(I-1,K,JC)-CH(I-1,K,J)
  112       CONTINUE
  113    CONTINUE
  114 CONTINUE
      GO TO 121
  115 DO 118 J=2,IPPH
         JC = IPP2-J
         DO 117 I=3,IDO,2
            DO 116 K=1,L1
               C1(I-1,K,J) = CH(I-1,K,J)+CH(I-1,K,JC)
               C1(I-1,K,JC) = CH(I,K,J)-CH(I,K,JC)
               C1(I,K,J) = CH(I,K,J)+CH(I,K,JC)
               C1(I,K,JC) = CH(I-1,K,JC)-CH(I-1,K,J)
  116       CONTINUE
  117    CONTINUE
  118 CONTINUE
      GO TO 121
  119 DO 120 IK=1,IDL1
         C2(IK,1) = CH2(IK,1)
  120 CONTINUE
  121 DO 123 J=2,IPPH
         JC = IPP2-J
         DO 122 K=1,L1
            C1(1,K,J) = CH(1,K,J)+CH(1,K,JC)
            C1(1,K,JC) = CH(1,K,JC)-CH(1,K,J)
  122    CONTINUE
  123 CONTINUE
C
      AR1 = 1.
      AI1 = 0.
      DO 127 L=2,IPPH
         LC = IPP2-L
         AR1H = DCP*AR1-DSP*AI1
         AI1 = DCP*AI1+DSP*AR1
         AR1 = AR1H
         DO 124 IK=1,IDL1
            CH2(IK,L) = C2(IK,1)+AR1*C2(IK,2)
            CH2(IK,LC) = AI1*C2(IK,IP)
  124    CONTINUE
         DC2 = AR1
         DS2 = AI1
         AR2 = AR1
         AI2 = AI1
         DO 126 J=3,IPPH
            JC = IPP2-J
            AR2H = DC2*AR2-DS2*AI2
            AI2 = DC2*AI2+DS2*AR2
            AR2 = AR2H
            DO 125 IK=1,IDL1
               CH2(IK,L) = CH2(IK,L)+AR2*C2(IK,J)
               CH2(IK,LC) = CH2(IK,LC)+AI2*C2(IK,JC)
  125       CONTINUE
  126    CONTINUE
  127 CONTINUE
      DO 129 J=2,IPPH
         DO 128 IK=1,IDL1
            CH2(IK,1) = CH2(IK,1)+C2(IK,J)
  128    CONTINUE
  129 CONTINUE
C
      IF (IDO .LT. L1) GO TO 132
      DO 131 K=1,L1
         DO 130 I=1,IDO
            CC(I,1,K) = CH(I,K,1)
  130    CONTINUE
  131 CONTINUE
      GO TO 135
  132 DO 134 I=1,IDO
         DO 133 K=1,L1
            CC(I,1,K) = CH(I,K,1)
  133    CONTINUE
  134 CONTINUE
  135 DO 137 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 136 K=1,L1
            CC(IDO,J2-2,K) = CH(1,K,J)
            CC(1,J2-1,K) = CH(1,K,JC)
  136    CONTINUE
  137 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IF (NBD .LT. L1) GO TO 141
      DO 140 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 139 K=1,L1
            DO 138 I=3,IDO,2
               IC = IDP2-I
               CC(I-1,J2-1,K) = CH(I-1,K,J)+CH(I-1,K,JC)
               CC(IC-1,J2-2,K) = CH(I-1,K,J)-CH(I-1,K,JC)
               CC(I,J2-1,K) = CH(I,K,J)+CH(I,K,JC)
               CC(IC,J2-2,K) = CH(I,K,JC)-CH(I,K,J)
  138       CONTINUE
  139    CONTINUE
  140 CONTINUE
      RETURN
  141 DO 144 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 143 I=3,IDO,2
            IC = IDP2-I
            DO 142 K=1,L1
               CC(I-1,J2-1,K) = CH(I-1,K,J)+CH(I-1,K,JC)
               CC(IC-1,J2-2,K) = CH(I-1,K,J)-CH(I-1,K,JC)
               CC(I,J2-1,K) = CH(I,K,J)+CH(I,K,JC)
               CC(IC,J2-2,K) = CH(I,K,JC)-CH(I,K,J)
  142       CONTINUE
  143    CONTINUE
  144 CONTINUE
      RETURN
      END
C**************************************************************************
