C*MARKSPOTS.FOR****************************************************************
C                                                                             *
C       Program to mark an MRC file with spots
C                                                                             *
C       Version 1.00    22-DEC-14    HS         First version                 *
C                                               Part of 2dx                   *
C                                                                             *
C******************************************************************************
C
C Input is a single file in mapformat, output is a single file in mapformat.
C
C******************************************************************************
C
      IMPLICIT NONE
C
      INTEGER LMAX
      REAL PI
C
      PARAMETER (LMAX=8100)
      PARAMETER (PI=3.1415926535898)
C
      INTEGER   IMP
      CHARACTER NCPUS*10
      INTEGER   OMP_GET_NUM_PROCS
      LOGICAL   EX
C
      CHARACTER CFORM
      INTEGER   IP,IS,IERR
      REAL      STEPR,DSTEP,R
C
      INTEGER OUT(LMAX)
      INTEGER NCX,NCY
      INTEGER ix,iy,iix,iiy,ixcor,iycor,iox,ioy,ilx,ily
      INTEGER MODE,IMODE
      INTEGER NX,NY,NZ,NXYZ(3)
      INTEGER I,k,L,M,IR,k1,J,ilen
      REAL    PEAK,VAL,XCOOR,YCOOR
      INTEGER NC,NR,IC
      REAL    A1,A2,B1,B2
      INTEGER MINA,MAXA,MINB,MAXB
      REAL    DENMAX
C
      REAL    ALINE(LMAX)
C
      INTEGER ID,IADDSIZE
C
      REAL,ALLOCATABLE :: APIC(:)
C
      REAL*8 DOUBLMEAN
      REAL RMIN,RMAX,RMEAN
C
      CHARACTER*200 INFILE,OUTFILE,SPOTFILE
      CHARACTER*20 CSTIN
      character*1600 TITLE
      character*200 cline
      CHARACTER*1 CDEBUG
C
C******************************************************************************
C
      WRITE(6,'(/,/,''MARK_SPOTS: Program to mark spot '',
     .    ''positions in image'')')
C
C******************************************************************************
C
#ifdef _OPENMP
      IMP=0
      CALL GETENV('OMP_NUM_THREADS',NCPUS)
      READ(NCPUS,*,ERR=111,END=111)IMP
111   CONTINUE
      IF (IMP.LE.0) THEN
        CALL GETENV('NCPUS',NCPUS)
        READ(NCPUS,*,ERR=112,END=112)IMP
112     CONTINUE
      ENDIF
      IF (IMP.LE.0) THEN
        IMP=OMP_GET_NUM_PROCS()
      ENDIF
      CALL OMP_SET_NUM_THREADS(IMP)
#endif
      IF (IMP.LE.0) IMP=1
C
      IF (IMP.GT.1) THEN
        WRITE(*,'('': Parallel processing: NCPUS = '',I8)') IMP
      ENDIF
C
      ALLOCATE(APIC(LMAX*LMAX),STAT=IERR)
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
      READ(5,'(A)') OUTFILE
      call shorten(OUTFILE,k)
      write(6,'(''   Read: '',A)')OUTFILE(1:k)
C
      READ(5,'(A)') SPOTFILE
      call shorten(SPOTFILE,k)
      write(6,'(''   Read: '',A)')SPOTFILE(1:k)
C
      WRITE(6,'(''Input MODE:'',/,
     . '' 1=Horizontal white cross on lattice positions,'',/,
     . '' 2=Diagonal black cross on supplied coordinates, '',/,
     . '' 3=do both'')')
      read(5,*)IMODE
      write(6,'(''    Read: '',I3)')IMODE
C
C******************************************************************************
C
      write(*,'(''Opened file has dimensions '',2I6)')NX,NY
C
C-----Read in entire input image
C
      RMIN =  1.E10
      RMAX = -1.E10
      RMEAN = 0.0
      DOUBLMEAN = 0.0
C
      do iy = 1,NY
        CALL IREAD(11,ALINE,iy)
        do ix = 1,NX
          VAL=ALINE(ix)
          APIC(ID(ix,iy,NY))=VAL
          IF (VAL .LT. RMIN) RMIN = VAL
          IF (VAL .GT. RMAX) RMAX = VAL
          DOUBLMEAN = DOUBLMEAN + VAL
        enddo
      enddo
      DOUBLMEAN = DOUBLMEAN/(NX*NY)
      RMEAN = DOUBLMEAN
C
      write(6,'(''Read input image. Min,Max,Mean = '',3F12.3)')
     .  RMIN,RMAX,RMEAN
      CALL ICLOSE(11)
C
C-----Mark input image 
C
      open(UNIT=3,STATUS='OLD',FILE=SPOTFILE,ERR=900)
      do i=1,5
        READ(3,'(A80)')cline
        write(6,'(''Read: '',A80)')cline(1:80)
      enddo
      READ(3,*) NC,NR,IC,IR,A1,A2,B1,B2,MINA,MAXA,MINB,MAXB
      READ(3,*)DENMAX
      WRITE(6,9001)NC,NR,IC,IR,A1,A2,B1,B2,MINA,MAXA,MINB,MAXB,DENMAX
9001  FORMAT(
     .  ':NC,NR...........................: ',2I5/
     .  ':IC,IR...........................: ',2I5/
     .  ':A1,A2,B1,B2.....................: ',4F10.4/
     .  ':LAST XCOR X,Y...................: ',4I5/
     .  ':MAX DENSITY IN CORRELATION MAP..: ',F12.1)
C
      if(IMODE.eq.1 .or. IMODE.eq.3)then
        do i=-200,200
          do j=-200,200
            XCOOR=i*A1+j*B1+NX/2+1
            YCOOR=i*A2+j*B2+NY/2+1
            PEAK=1.0
            call mark(APIC,NX,NY,XCOOR,YCOOR,PEAK,RMIN,1)
          enddo
        enddo
      endif
C
      ilen=0
 100  continue
        READ(3,*,END=203) I,J,XCOOR,YCOOR,PEAK
        ilen=ilen+1
        if(IMODE.eq.2 .or. IMODE.eq.3)then
          call mark(APIC,NX,NY,XCOOR,YCOOR,PEAK,RMAX,2)
        endif
        goto 100
 203  continue
      close(3)
      write(6,'('':Marked '',I6,'' spots'')')ilen
C
C-----Write out output image
C
      call shorten(OUTFILE,k)
      write(cline,'(''\rm -f '',A)')OUTFILE(1:k)
      call shorten(cline,k1)
      call system(cline(1:k1))
      write(CSTIN,'(''NEW'')')
      CALL IOPEN(OUTFILE,12,CFORM,MODE,NXYZ(1),NXYZ(2),
     +         NXYZ(3),CSTIN,STEPR,TITLE)
      do iy = 1,NY
        do ix = 1,NX
          IR=ID(ix,iy,NY)
          ALINE(ix)=APIC(IR)
        enddo
        call IWRITE(12,ALINE,iy)
      enddo
      CALL ICLOSE(12)
      write(6,'('':Output file written:'',A)')OUTFILE(1:k)
C
C-----Done
C
      goto 999
C
 900  continue
      write(6,'(''::ERROR occured in 2dx_mark_spots. ABORTING'')')
      stop
C
 999  continue
      write(6,'(''Program normal end.'')')
C
      STOP
      END
C
C==========================================================
C
      SUBROUTINE mark(APIC,NX,NY,XCOOR,YCOOR,PEAK,RVAL,IMODE)
C
      IMPLICIT NONE
      REAL APIC(*)
      INTEGER NX,NY
      REAL XCOOR,YCOOR,PEAK,RVAL
      INTEGER ilen,ix,iy,iix,iiy,IR,IMODE
      INTEGER ID
C
      if(IMODE.eq.2)then
        ilen=12
        iiy=int(YCOOR)
        do ix=-ilen,ilen
          if(abs(ix).gt.6)then
            iix=ix+int(XCOOR)
            if(iix.ge.1 .and. iix.le.NX .and. 
     .         iiy.ge.1 .and. iiy.le.NY ) then
              IR=ID(iix,iiy,NY)
              APIC(IR)=RVAL
            endif
          endif
        enddo
        iix=int(XCOOR)
        do iy=-ilen,ilen
          if(abs(iy).gt.6)then
            iiy=iy+int(YCOOR)
            if(iix.ge.1 .and. iix.le.NX .and. 
     .         iiy.ge.1 .and. iiy.le.NY ) then
              IR=ID(iix,iiy,NY)
              APIC(IR)=RVAL
            endif
          endif
        enddo
C
      else
C
        ilen=10
        do ix=-ilen,ilen
          if(abs(ix).gt.3)then
            iix=int(XCOOR)+ix
            iiy=int(YCOOR)+ix
            if(iix.ge.1 .and. iix.le.NX .and. 
     .         iiy.ge.1 .and. iiy.le.NY ) then
              IR=ID(iix,iiy,NY)
              APIC(IR)=RVAL
            endif
          endif
        enddo
        do ix=-ilen,ilen
          if(abs(ix).gt.3)then
            iix=int(XCOOR)+ix
            iiy=int(YCOOR)-ix
            if(iix.ge.1 .and. iix.le.NX .and. 
     .         iiy.ge.1 .and. iiy.le.NY ) then
              IR=ID(iix,iiy,NY)
              APIC(IR)=RVAL
            endif
          endif
        enddo
      endif
C
      RETURN
      END
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
C**************************************************************************
C
      INTEGER FUNCTION ID(ix,iy,iylen)
C       this function calculates a 1D index for a 2D array of width iylen.
      ID = ix + iylen*(iy-1)
      RETURN
      END
C
C**************************************************************************
