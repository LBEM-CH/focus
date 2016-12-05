C*FOCUS_FOURIERCROP************************************************************
C                                                                             *
C       Program to optionally pad image into square array and Fourier crop    *
C                                                                             *
C                                               Part of FOCUS                 *
C                                                                             *
C       Version 1.00    05-DEC-16    HS         First version                 *
C                                                                             *
C******************************************************************************
C
C-----The input image stack is padded to square array and Fourier cropped to given pixel size.
C
C      Card input, or online control
C
C        card  1 :  input filename
C        card  2 :  output filename
C        card  3 :  Pad to square: y=yes, n=no
C        card  4 :  Current pixel size
C        card  5 :  Desired pixel size
C        card  6 :  Output file for new image statistics
C        card  7 :  Debug mode: y=yes, n=no
C
C******************************************************************************
C
      IMPLICIT NONE
C
      INTEGER LMAX
      REAL PI
C
      PARAMETER (LMAX=9100)
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
      REAL      STEPRO
C
      INTEGER ID
C
      INTEGER NCX,NCY
      INTEGER ix,iy,iz,iix,iiy,ixcor,iycor,iox,ioy,ilx,ily,k2
      INTEGER icx,icy
      INTEGER*4 iover,iunder,ilow
      INTEGER MODE,IMODE
      INTEGER NX,NY,NZ,NNX,NNY,NL,NRX,NXP2,NNXP2
      INTEGER NCNX,NCNY,NCNXP2
      INTEGER I,J,K,L,M,LL,MM,I2,J2,IR,ICR
      INTEGER icount
      INTEGER idist,IREC
      INTEGER ioffx,ioffy,iok,IOR,iwert,ixgood
C
C
      COMPLEX CBOX(LMAX)
C
      INTEGER NXYZ(3),NXYZ2(3)
      REAL ALINE(LMAX)
      REAL LABELS(20,10),CELL(6),EXTRA(29)
      REAL DNCELL(6)
      REAL VAL,DMIN,DMAX,DMEAN,D1MAX
      REAL DIMIN,DIMAX,DIMEAN
      REAL D2MIN,D2MAX,D2MEAN
      REAL DOMIN,DOMAX,DOMEAN
      REAL DAMPMAX
      REAL rnewpix,roldpix
C
      REAL*8 DOUBLMEAN,DOUBLOMEAN,DVAL
      REAL*8 DOOUBLMEAN
      REAL*8 D2OUBLMEAN
C
      COMPLEX CVAL
C
      LOGICAL LDEBUG
      LOGICAL LPAD
C
      CHARACTER*200 CINFILE,COUTFILE,CSTATFILE,CPAD
      CHARACTER*20 CSTIN
      character*1600 TITLE
      character*200 cfile,cname,cline,cline2
      CHARACTER*1 CDEBUG
C
      INTEGER IFOR,IBAK
C
      REAL,ALLOCATABLE :: APIC(:)
      REAL,ALLOCATABLE :: BPIC(:)
C
      CHARACTER * 200 czeil2
C
      DATA IFOR/0/,IBAK/1/
C
C******************************************************************************
C
      WRITE(6,'(/,/,''FouierCrop: Program to pad image square and '',
     .      ''Fourier crop to desired pixel size'',/)')
C
C******************************************************************************
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
      ALLOCATE(BPIC(LMAX*LMAX),STAT=IERR)
      IF (IERR.NE.0) THEN
        WRITE(*,*) ':: ERROR: Memory allocation failed in MAIN'
        STOP ':: Try reducing the image size'
      ENDIF
C
      WRITE(6,'(''Give input filename:  '')')
      READ(5,'(A)') CINFILE
      call shorten(CINFILE,k)
      write(6,'(''   Read: '',A)')CINFILE(1:k)
C
      call GUESSF(CINFILE,CFORM,EX)
      if(.not.EX)then
        write(*,'(''::ERROR: input file not found: '',A)')CINFILE(1:k)
        STOP
      ENDIF
      write(CSTIN,'(''OLD'')')
      CALL IOPEN(CINFILE,11,CFORM,MODE,NXYZ(1),NXYZ(2),
     +         NXYZ(3),CSTIN,STEPR,TITLE)
      NX=NXYZ(1)
      NY=NXYZ(2)
      NZ=NXYZ(3)
      roldpix = STEPR

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
      WRITE(6,'(/,''Output filename:  '')')
      READ(5,'(A)') COUTFILE
      call shorten(COUTFILE,k)
      write(6,'(''   Read: '',A)')COUTFILE(1:k)
C
C        card  3 :  Pad to square: y=yes, n=no
C
      write(6,'(/,''Should image be padded into square array (y/n)'')')
      READ(5,'(A)') CPAD
      call shorten(CPAD,k)
      write(6,'(''   Read: '',A)')CPAD(1:k)
      if(CPAD(1:1).eq."y")then
        LPAD = .true. 
        write(6,'('' Will do padding.'')')
      else
        LPAD = .false.
        write(6,'('' Will not do padding.'')')
      endif
C
C        card  4 :  Current pixel size
C        card  5 :  Desired pixel size
C
C      write(6,'(''Input current pixel size in Angstroems'')')
C      read(5,*) roldpix
C      write(6,'(''    Read: '',F12.3)')roldpix
C
      write(6,'(/,'':Input image has a pixel size of '',F12.6,
     .        '' Angstroems'')')roldpix
      write(6,'(/,''Give desired pixel size in Angstroems'')')
      read(5,*) rnewpix
      write(6,'(''    Read: '',F12.3)')rnewpix
C
      if(roldpix.gt.rnewpix)then
        write(6,'(''::ERROR: Only downsampling is supported so far.'')')
        write(6,'(''::Choose a lower pixel size.'')')
        goto 900
      endif
C
C        card  6 :  Output file for new image statistics
C
      WRITE(6,'(/,''Give filename for statistcs of new image:  '')')
      READ(5,'(A)') CSTATFILE
      call shorten(CSTATFILE,k)
      write(6,'(''   Read: '',A)')CSTATFILE(1:k)
C
C        card  7 :  Debug mode: y=yes, n=no
C
      WRITE(6,'(/,''Debug Mode: y or n'')')
      read(5,*)CDEBUG
      write(6,'(''    Read: '',A1)')CDEBUG
C
      if(CDEBUG.eq.'y')then
        LDEBUG=.true.
        NZ = 3
      else
        LDEBUG=.false.
      endif
C
C******************************************************************************
C******************************************************************************
C******************************************************************************
C
      if(LPAD)then
        iwert=NY
        if(NX.gt.NY)iwert=NX
C
        write(*,'('':Input image has dimensions '',2I7)')NX,NY
C
        iwert = iwert*roldpix/rnewpix
        write(*,'('':This would give a resampled image of '',2I7)')iwert,iwert

        ixgood = 0
        call PRITES(iwert,iok,0,czeil2)
        if (iok.eq.0) then
C           write(*,'('':'',I8,'' is a good image size.'')') iwert
           call shorten(czeil2,k)
           write(*,'(A)') czeil2(1:k)
        else
           write(*,'('':'',I8,'' is a not a good image size.'')') iwert
           call shorten(czeil2,k)
           write(*,'(A)') czeil2(1:k)
           write(*,'('':Trying to find better ...'')')
 300       continue
             iwert = iwert+1
             call PRITES(iwert,iok,0,czeil2)
             if (iok .eq. 0) then
               call shorten(czeil2,k)
               write(*,'(A)') czeil2(1:k)
               ixgood = 1
               goto 350
             endif
           if (iwert .lt. LMAX) goto 300
        endif
 350    continue
        NCNX = iwert
        NCNY = iwert
        NNX = NCNX * rnewpix / roldpix
        NNY = NCNY * rnewpix / roldpix
        write(6,'('':Padding into a square array of '',I5,'' x '',I5,
     .        '' pixels'')')NNX,NNY
        write(6,'('':to give final dimensions of    '',I5,'' x '',I5,
     .        '' pixels'')')NCNX,NCNY
C
C-------Image frame is read into core directly into larger, square field:
C-------Frame dimensions are NX,NY
C-------Square dimensions are NNX,NNY
C-------Offset is:
        ioffx = (NNX-NX)/2
        ioffy = (NNY-NY)/2
      else
        ioffx = 0
        ioffy = 0
        NNX = NX
        NNY = NY
        NCNX = NNX*roldpix/rnewpix
        NCNY = NNY*roldpix/rnewpix
      endif
C
      STEPRO = STEPR*rnewpix/roldpix
      NCNXP2 = NCNX + 2
      NXP2   = NX   + 2
      NNXP2  = NNX  + 2
C
      write(*,'('':Offset for padding is ...... : '',2I7)')ioffx,ioffy
C
C******************************************************************************
C******************************************************************************
C******************************************************************************
C
C-----Output output image
C
      write(CSTIN,'(''NEW'')')
      NXYZ2(1)=NCNX
      NXYZ2(2)=NCNY
      NXYZ2(3)=NZ
      CALL IOPEN(COUTFILE,12,CFORM,MODE,NXYZ2(1),NXYZ2(2),
     +         NXYZ2(3),CSTIN,STEPRO,TITLE)
C
      DOMIN =  1.E10
      DOMAX = -1.E10
      DOMEAN = 0.0
      DOOUBLMEAN = 0.0
C
C-----Big loop over all frames:
C
      do iz = 1,NZ
C
C-------Read one frame of input stack into core memory:
C
        DMIN =  1.E10
        DMAX = -1.E10
        DMEAN = 0.0
        DOUBLMEAN = 0.0
C
        do iy = 1,NY
          IREC=iy+(iz-1)*NY
          CALL IREAD(11,ALINE,IREC)
          do ix = 1,NX
            VAL=ALINE(ix)
            BPIC(ID(ix+ioffx,iy+ioffy,NNXP2))=VAL
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
          enddo
        enddo
        DOUBLMEAN = DOUBLMEAN/(NX*NY)
        DMEAN = DOUBLMEAN
C
        DIMIN=DMIN
        DIMAX=DMAX
        DIMEAN=DMEAN
        write(6,'(''Read frame '',I5,''. Min,Max,Mean = '',3F12.3)')
     .    iz,DIMIN,DIMAX,DIMEAN
C
        if(LPAD)then
C---------bottom stripe
          do iy = 1,ioffy
            do ix = 1,NNX
              BPIC(ID(ix,iy,NNXP2))=DMEAN
            enddo
          enddo
C---------top stripe
          do iy = NNY-ioffy,NNY
            do ix = 1,NNX
              BPIC(ID(ix,iy,NNXP2))=DMEAN
            enddo
          enddo
C---------left stripe
          do iy = ioffy+1,NNY-ioffy-1
            do ix = 1,ioffx
              BPIC(ID(ix,iy,NNXP2))=DMEAN
            enddo
          enddo
C---------right stripe
          do iy = ioffy+1,NNY-ioffy-1
            do ix = NNX-ioffx,NNX
              BPIC(ID(ix,iy,NNXP2))=DMEAN
            enddo
          enddo
C
C---------------------------------------------------
C---------Taper edge to DMEAN value
C---------------------------------------------------
C
C---------Edge width is idist
          idist = 20
C
          do iy = 1,idist
C-----------bottom edge
            ily = ioffy+iy
            do ix = 1,NNX
              IS=ID(ix,ily,NNXP2)
              BPIC(IS)=DMEAN+(BPIC(IS)-DMEAN)*(REAL(iy)/REAL(idist))
            enddo
C-----------top edge
            ily = NNY-ioffy-iy
            do ix = 1,NNX
              IS=ID(ix,ily,NNXP2)
              BPIC(IS)=DMEAN+(BPIC(IS)-DMEAN)*(REAL(iy)/REAL(idist))
            enddo
          enddo
C-----------left stripe
          do ix = 1,idist
            do iy = ioffy+1,NNY-ioffy-1
              ilx = ioffx+ix
              if(ilx.lt.ily .and. ilx.gt.NNY-ily)then
C---------------Left edge
                IS=ID(ilx,iy,NNXP2)
                BPIC(IS)=DMEAN+(BPIC(IS)-DMEAN)*(REAL(ix)/REAL(idist))
C---------------Right edge
                ilx = NNX-ioffx-ix
                IS=ID(ilx,iy,NNXP2)
                BPIC(IS)=DMEAN+(BPIC(IS)-DMEAN)*(REAL(ix)/REAL(idist))
              endif
            enddo
          enddo
C
        endif
C
C-------Calculate in-place Fourier Transform from entire image
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        call TDXFFT(BPIC,NNX,NNY,IFOR)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C-------Fourier crop
C
        do icx=1,NCNX/2
          ilx=icx
          do icy=1,NCNY
            ily=icy
            if(icy.gt.NCNY/2)ily=ily+NNY-NCNY
            IOR=ID(ilx,ily,NNX /2+1)*2
            ICR=ID(icx,icy,NCNX/2+1)*2
            APIC(ICR-1)=BPIC(IOR-1)
            APIC(ICR  )=BPIC(IOR  )
          enddo
        enddo
C
C-------Set last column to zero
C
        DO iy=1,NCNY
          ix=NCNX/2+1
          IR=ID(ix,iy,NCNX/2+1)*2
          APIC(IR-1)=0.0
          APIC(IR  )=0.0
        enddo
C
C-------Set last row to zero
C
        DO ix=1,NCNX/2
          iy=NCNY/2+1
          IR=ID(ix,iy,NCNX/2+1)*2
          APIC(IR-1)=0.0
          APIC(IR  )=0.0
        enddo
C
C-------Set the origin to zero
C
        IR=ID(1,1,NCNX/2+1)*2
        APIC(IR-1)=0.0
        APIC(IR  )=0.0
C
C-----Calculate inverse in-place Fourier Transform
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        call TDXFFT(APIC,NCNX,NCNY,IBAK)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C-------Write frame to output file
C
        D2MIN =  1.E10
        D2MAX = -1.E10
        D2MEAN = 0.0
        D2OUBLMEAN = 0.0
C
        do iy = 1,NCNY
          do ix = 1,NCNX
            IR=ID(ix,iy,NCNXP2)
            VAL=APIC(IR)+DMEAN
            IF (VAL .LT. D2MIN) D2MIN = VAL
            IF (VAL .GT. D2MAX) D2MAX = VAL
            D2OUBLMEAN = D2OUBLMEAN + VAL
            IF (VAL .LT. DOMIN) DOMIN = VAL
            IF (VAL .GT. DOMAX) DOMAX = VAL
            DOOUBLMEAN = DOOUBLMEAN + VAL
            ALINE(ix)=VAL
          enddo
          IREC=iy+(iz-1)*NCNY
          call IWRITE(12,ALINE,IREC)
        enddo
        D2OUBLMEAN = D2OUBLMEAN/(NCNX*NCNY)
        D2MEAN = D2OUBLMEAN
        write(6,'(''Write frame '',I4,''. Min,Max,Mean ..............'',
     .    ''............................. = '',3F12.3)')
     .    iz,D2MIN,D2MAX,D2MEAN
C
      enddo
C
      DOOUBLMEAN = DOOUBLMEAN/(NCNX*NCNY*NZ)
      DOMEAN = DOOUBLMEAN
C
      write(*,'('' '')')
      write(*,'(''Overall Min, Max, Mean of stack is '',3F12.3)')
     .   DOMIN,DOMAX,DOMEAN
C
      CALL ICLOSE(11)
      CALL ICLOSE(12)
C
      call shorten(CSTATFILE,k)
      write(cline,'(''\rm -f '',A)')CSTATFILE(1:k)
      call shorten(cline,k2)
      call system(cline(1:k2))
      open(14,FILE=CSTATFILE(1:k),STATUS="NEW")
      write(14,'(F16.3,'' # DMIN '')')DOMIN
      write(14,'(F16.3,'' # DMAX '')')DOMAX
      write(14,'(F16.3,'' # DMEAN'')')DOMEAN
      write(14,'(I6   ,'' # NX   '')')NCNX
      write(14,'(I6   ,'' # NY   '')')NCNY
      write(14,'(I6   ,'' # NZ   '')')NZ
      close(14)
C
      write(6,'(''   Read: '',A)')CSTATFILE(1:k)
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
C
C============================================================================
c
      SUBROUTINE PRITES(iwert,iok,ilog,czeil2)
C             
      character * 200 czeil2
C
C      INTEGER ifeld(8)
C      DATA ifeld/2,3,5,7,11,13,17,19/
      INTEGER ifeld(3)
      DATA ifeld/2,3,5/
C
      irun=iwert
      itest=1
      ianf=13
      write(czeil2,'('':'',I8,'' = '')') iwert
C
C-----First test, if the number iwert can be devided by 4. This should be, because
C     the fft is also calculated of the reduced image.
C
      irest = int(iwert/4)*4
      if (irest .ne. iwert) then
        write(czeil2(ianf:80),'('' not multiple of 4 ! '')')
        iok = -1
        goto 990
      endif
C
 310  continue
      irest = int(irun/ifeld(itest))*ifeld(itest)
      if (irest .eq. irun) then
         irun=irun/ifeld(itest)
         if (ifeld(itest) .lt. 10) then
           write(czeil2(ianf:80),'(I2,'' *'')') ifeld(itest)
           ianf=ianf+4
         else
           write(czeil2(ianf:80),'(I3,'' *'')') ifeld(itest)
           ianf=ianf+5
         endif
         if (ianf .gt. 70) then
            write(czeil2(10:80),'('' many many many.... '')')
            ianf=71
         endif
         itest=1
      else
         itest=itest+1
         if (itest .gt. 3) then
           if (ilog .eq. 1)
     1     write(*,'('':Primefactor exceeds '',I2,'', rest = '',I8)') ifeld(3),irun
           write(czeil2(ianf:80),'('' (too big)'')')
           iok = -1
           goto 990
         endif
      endif
      if (irun .gt. 1) goto 310
      ianf=ianf-2
      write(czeil2(ianf:80),'(''  '')')
      iok = 0
c
 800  continue
c
 990  continue
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
