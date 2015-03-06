C  2dx_movie_refine.for 
C
C       VX1.0   HS     25.11.14  2dx - First version
C
      IMPLICIT NONE
C
      INTEGER IARRMXSIZ,IARRMXSI2,IMNY,IMXY,IMNI,IMXI,IMNJ,IMXJ
      INTEGER IMAXFRAMES,iinterpol,IAVE
C
C-----Downsampling ratio (use IAVE=2 for 2xbinning)
      PARAMETER (IAVE=1)
C
C-----Max input image of 16000 x 16000 is an array of 256'000'000
C-----Max input image of 12000 x 12000 is an array of 144'000'000
C-----Max input image of 6000 x 6000 is an array of 36'000'000
C-----Max input image of 4096 x 4096 is an array of 16'777'216
C-----Max input image of 2048 x 2048 is an array of 4'194'304
C-----The following has to be = (image size)**2 / IAVE
      PARAMETER (IARRMXSI2=16777216)
C      PARAMETER (IARRMXSI2=4194304)
C
C-----These have to be the same as in 2dx_quadserchk-2.for, currently 240
      PARAMETER (IMNY=-240)
      PARAMETER (IMXY= 240)
C
      PARAMETER (IMNI=-10)
      PARAMETER (IMXI= 10)
C
      PARAMETER (iinterpol=2)
C-----the following IMNJ,IMXJ have to be IMNI*iinterpol, IMXI*iinterpol
      PARAMETER (IMNJ=-20)
      PARAMETER (IMXJ= 20)
C
      PARAMETER (IMAXFRAMES=90)
C
      CHARACTER TITLE*1600
C
      INTEGER   NXYZ(3)
C
      REAL*8    DOUBLMEAN,DARRAYMEAN,DARRAYMIN,DARRAYMAX
      INTEGER   I,J,K,L,IC,IR,NC,NR,H,IVAL,IK,IL
      INTEGER   ioffx,ioffy,ix,iy,ixx,iyy
      INTEGER   IRO,ICO,NROW,NCOL,NRO2,NCO2
      INTEGER   IXMIN,IYMIN,IZMIN,IXMAX,IYMAX,IZMAX
      INTEGER   IMAXA,IMAXB,IMINA,IMINB
      INTEGER   ID
      INTEGER   iminoffset,imaxoffset,izoom
      INTEGER   ixstart,iystart,ixend,iyend
      INTEGER   NX,NY,NZ,MODE
      INTEGER   iframe,IFRAMS,IPEAK
      INTEGER   ixc,iyc
      REAL      roffstep
      REAL      DMIN,DMAX,DMEAN
      REAL      RVAL,RVAL1,RVAL2,RVAL3
      REAL      DENMAX
      REAL      RX,RY
      INTEGER   IGOFFMAX,IOFFMAX,IMAX
      REAL      IOFFSTARTX,IOFFSTARTY,IOFFENDX,IOFFENDY
      REAL      IAOFFSTARTX,IAOFFSTARTY,IAOFFENDX,IAOFFENDY
      REAL      IGOFFSTARTX,IGOFFSTARTY,IGOFFENDX,IGOFFENDY
      REAL      rmaxx,rmaxy
      INTEGER   icount
C
      INTEGER   IMP
      CHARACTER NCPUS*10
      INTEGER   OMP_GET_NUM_PROCS
C
      CHARACTER CFORM
      INTEGER   IP,IS,IERR
      REAL      STEPR,DSTEP,R
C
      CHARACTER*200 FILE1,FILE2
      CHARACTER*200 cline,cline2
      CHARACTER*200 comment(5)
      CHARACTER*20 CSTIN
C
      LOGICAL   EX
C
      REAL      A1,B1,A2,B2
C
      INTEGER   IFIELD(IMNI:IMXI,IMNI:IMXI)
      INTEGER   IFIEL2(IMNJ:IMXJ,IMNJ:IMXJ)
      INTEGER   IFIEL3(IMNJ:IMXJ,IMNJ:IMXJ)
C
      REAL      XPOSIT(IMNY:IMXY,IMNY:IMXY)
      REAL      YPOSIT(IMNY:IMXY,IMNY:IMXY)
      REAL      PEAK(IMNY:IMXY,IMNY:IMXY)
C
      REAL      ROSTARTX(IMNY:IMXY,IMNY:IMXY)
      REAL      ROSTARTY(IMNY:IMXY,IMNY:IMXY)
      REAL      ROENDX(IMNY:IMXY,IMNY:IMXY)
      REAL      ROENDY(IMNY:IMXY,IMNY:IMXY)
C
C      REAL,ALLOCATABLE :: ROCENX(:,:)
C      REAL,ALLOCATABLE :: ROCENY(:,:)
C 
C      REAL      ARRAY(IARRMXSIZ)
C      INTEGER*2 IARRAM(IARRMXSI2,IMAXFRAMES)
      REAL,ALLOCATABLE :: ARRAY(:)
      INTEGER*2,ALLOCATABLE :: IARRAM(:)
C
C-----Program start:

      WRITE(6,'('' 2dx_movie_refine '')')
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
C-----Read Stacks of CCmaps into core
      write(6,'(''Give name of CCmaps with frames without'',
     .  '' -??.mrc ending'')')
      read(5,'(A)')FILE1
      call shorten(FILE1,k)
      write(6,'(''Read: '',A)')FILE1(1:k)
C
      write(6,'(''Give number of frames to process'')')
      read(5,*)IFRAMS
      write(6,'(''Read: '',I8)')IFRAMS
      if(IFRAMS.gt.IMAXFRAMES)then
        write(6,'(''::ERROR: too many frames for program'',
     .    '' dimensions.'')')
        STOP
      endif
C
      write(6,'(''Reading all CC maps for frames'')')
      do iframe = 1,IFRAMS
        call shorten(FILE1,k)
        if(iframe.lt.10)then
          write(cline,'(A,''_'',I1,''.mrc'')')FILE1(1:k),iframe
        elseif(iframe.lt.100)then
          write(cline,'(A,''_'',I2,''.mrc'')')FILE1(1:k),iframe
        elseif(iframe.lt.1000)then
          write(cline,'(A,''_'',I3,''.mrc'')')FILE1(1:k),iframe
        else
          write(6,'(''::ERROR: too many frames for program'',
     .      '' dimensions:'',I8)')iframe
          STOP
        endif
C
        call shorten(cline,k)
        call GUESSF(cline,CFORM,EX)
        if(.not.EX)then
          write(*,'(''::ERROR: CCmap file not found: '',A)')cline(1:k)
          STOP
        ENDIF
        write(CSTIN,'(''OLD'')')
        CALL IOPEN(cline,10,CFORM,MODE,NXYZ(1),NXYZ(2),
     +           NXYZ(3),CSTIN,STEPR,TITLE)
C
        WRITE(*,'(''Reading image '',A,'', NX,NY = '',2I10)')
     .     cline(1:k),NXYZ(1),NXYZ(2)
C
C-------Allocate large arrays:
C
        if(iframe.eq.1)then
C
          ALLOCATE(ARRAY(NXYZ(1)/IAVE*NXYZ(2)/IAVE+NXYZ(1)*IAVE),
     +           STAT=IERR)
          IF (IERR.NE.0) THEN
            WRITE(*,*) ':: ERROR: Memory allocation failed in MAIN'
            STOP ':: Try reducing the image size or increase IAVE'
          ENDIF
C
C      INTEGER*2 IARRAM(IARRMXSI2,IMAXFRAMES)
          ALLOCATE(IARRAM(IARRMXSI2*IFRAMS),STAT=IERR)
          IF (IERR.NE.0) THEN
            WRITE(*,*) ':: ERROR: Memory allocation failed in MAIN'
            STOP ':: Try reducing the number of frames or interpoate'
          ENDIF

        endif
C
C-------Read in image, and compress by IAVE on the same time.
C
        DO J=1,NXYZ(2)/IAVE
          IP=(J-1)*IAVE
          DO K=1,IAVE
            ID=1+(NXYZ(1)/IAVE)*(J-1)+NXYZ(1)*(K-1)
            CALL IREAD(10,ARRAY(ID),K+IP)
            IF (K.GT.1) THEN
              DO L=1,NXYZ(1)
                ID=L+(NXYZ(1)/IAVE)*(J-1)
                IS=L+(NXYZ(1)/IAVE)*(J-1)+NXYZ(1)*(K-1)
                ARRAY(ID)=ARRAY(ID)+ARRAY(IS)
              enddo
            ENDIF
          enddo
          DO I=1,NXYZ(1)/IAVE
            R=0.0
            DO K=1,IAVE
              ID=IAVE*(I-1)+K+(NXYZ(1)/IAVE)*(J-1)
              R=R+ARRAY(ID)
            enddo
            R=R/IAVE**2
            ID=I+(NXYZ(1)/IAVE)*(J-1)
            ARRAY(ID)=R
          enddo
        enddo
C
        NROW=NXYZ(1)
        NCOL=NXYZ(2)
C
        call ICLOSE(10)
C
        if(iframe.eq.1)then
          DARRAYMIN= 1e10
          DARRAYMAX=-1e10
          NRO2=NROW/IAVE
          NCO2=NCOL/IAVE
          do IRO = 1,NRO2
            do ICO = 1,NCO2
              ID=(IRO-1)*NCO2+ICO
              if(DARRAYMIN.gt.ARRAY(ID))DARRAYMIN=ARRAY(ID)
              if(DARRAYMAX.lt.ARRAY(ID))DARRAYMAX=ARRAY(ID)
            enddo
          enddo
          write(6,'('' INPUT ARRAY MIN,MAX = '',2G15.3)')DARRAYMIN,DARRAYMAX
        endif
C
C-------Scale ARRAY between 0 and 256 and store into IARRAM
C
        do IRO = 1,NRO2
          do ICO = 1,NCO2
            ID=(IRO-1)*NCO2+ICO
            IVAL=INT((ARRAY(ID)-DARRAYMIN)*256.0/(DARRAYMAX-DARRAYMIN))
            if(IVAL.lt.0)IVAL=0
            IS=ID+IARRMXSI2*(iframe-1)
            IARRAM(IS)=IVAL
          enddo
        enddo
        write(6,'(''Read CC map '',I4)')iframe
      enddo
      write(6,'(''All '',I4,'' CC maps read into core.'')')IFRAMS
C
C-----Read Unbending lattice nodes for AVERAGE image into core:
C
      write(6,'(''Give name of PROFDATA file'')')
      read(5,'(A)')FILE2
      call shorten(FILE2,k)
      write(6,'(''Read: '',A)')FILE2(1:k)
C
      open(13,FILE=FILE2,STATUS="OLD",ERR=990)
      do i=1,5
        read(13,'(A)')comment(i)
      enddo
      read(13,*)NC,NR,IC,IR,A1,A2,B1,B2,IMINA,IMAXA,IMINB,IMAXB
      read(13,*)DENMAX

      do IK=IMINA,IMAXA
        do IL=IMINB,IMAXB
          XPOSIT(IK,IL)=0.0
          YPOSIT(IK,IL)=0.0
          PEAK(IK,IL)=0.0
        enddo
      enddo
      ipeak=0
      DO IK=IMINA,IMAXA
        DO IL=IMINB,IMAXB
          read(13,*,END=123)K,L,RVAL1,RVAL2,RVAL3
          XPOSIT(K,L)=RVAL1
          YPOSIT(K,L)=RVAL2
          PEAK(K,L)=RVAL3
          if(PEAK(K,L).ne.0.0)then
            ipeak=ipeak+1
          endif
        enddo
      enddo
 123  continue
      close(13)
      write(*,'(''Read PROFDATA field, found '',I8,
     .   '' non-zero peaks'')')ipeak
C
C-----Find the trajectory through CCmaps that gives the highest accumulated peak:
C
      imaxoffset = 12
      iminoffset = -imaxoffset
      izoom = 4
C
C-----Loop over peaks in CCmap:
      icount=0
      do K=IMINA,IMAXA
        do L=IMINB,IMAXB
          if(PEAK(K,L).ne.0.0 .and.
     .       int(XPOSIT(K,L)/IAVE).gt.     imaxoffset*(izoom+1) .and.
     .       int(XPOSIT(K,L)/IAVE).lt.NCOL-imaxoffset*(izoom+1) .and.
     .       int(YPOSIT(K,L)/IAVE).gt.     imaxoffset*(izoom+1) .and.
     .       int(YPOSIT(K,L)/IAVE).lt.NROW-imaxoffset*(izoom+1) ) then
            icount = icount + 1
            IGOFFMAX=-10000
            IAOFFSTARTX=0.0
            IAOFFSTARTY=0.0
            IAOFFENDX=0.0
            IAOFFENDY=0.0
C-----------Loop over target on fist frame:
!$OMP PARALLEL DO PRIVATE (iystart,ixend,iyend,IMAX,iframe,ixc,iyc,ID,IS,IOFFMAX,IOFFSTARTX,IOFFSTARTY,IOFFENDX,IOFFENDY)
            do ixstart=iminoffset*izoom,imaxoffset*izoom,izoom
              IOFFMAX=-10000
              IOFFSTARTX=0
              IOFFSTARTY=0
              IOFFENDX=0
              IOFFENDY=0
              do iystart=iminoffset*izoom,imaxoffset*izoom,izoom
C---------------Loop over target on fist frame:
                do ixend  =-ixstart+iminoffset,-ixstart+imaxoffset,izoom
                  do iyend  =-iystart+iminoffset,-iystart+imaxoffset,izoom
C-------------------Loop through frames:
                    IMAX = 0
                    do iframe = 1,IFRAMS
                      ixc = nint(XPOSIT(K,L)/IAVE) + ixstart + nint((real(iframe-1)*(ixend-ixstart))/(IFRAMS-1))
                      iyc = nint(YPOSIT(K,L)/IAVE) + iystart + nint((real(iframe-1)*(iyend-iystart))/(IFRAMS-1))
                      ID=(iyc-1)*NCO2+ixc
                      IS=ID+IARRMXSI2*(iframe-1)
                      IMAX = IMAX + IARRAM(IS)
                    enddo
                    if(IMAX.gt.IOFFMAX)then
                      IOFFMAX=IMAX
                      IOFFSTARTX=ixstart
                      IOFFSTARTY=iystart
                      IOFFENDX=ixend
                      IOFFENDY=iyend
                    endif
                  enddo
                enddo
              enddo
!$OMP CRITICAL 
              if(IOFFMAX.gt.IGOFFMAX)then
                IGOFFMAX=IOFFMAX
                IAOFFSTARTX=IOFFSTARTX
                IAOFFSTARTY=IOFFSTARTY
                IAOFFENDX=IOFFENDX
                IAOFFENDY=IOFFENDY
              endif
!$OMP END CRITICAL 
            enddo
!$OMP END PARALLEL DO
C
C-----------Refinement
            IGOFFMAX=-10000
            IGOFFSTARTX=0.0
            IGOFFSTARTY=0.0
            IGOFFENDX=0.0
            IGOFFENDY=0.0
!$OMP PARALLEL DO PRIVATE (iystart,ixend,iyend,IMAX,iframe,ixc,iyc,ID,IS,IOFFMAX,IOFFSTARTX,IOFFSTARTY,IOFFENDX,IOFFENDY)
            do ixstart=IAOFFSTARTX-izoom/2,IAOFFSTARTX+izoom/2
              IOFFMAX=-10000
              IOFFSTARTX=0
              IOFFSTARTY=0
              IOFFENDX=0
              IOFFENDY=0
              do iystart=IAOFFSTARTY-izoom/2,IAOFFSTARTY+izoom/2
C---------------Loop over target on fist frame:
                do ixend  =IAOFFENDX-izoom/2,IAOFFENDX+izoom/2
                  do iyend  =IAOFFENDY-izoom/2,IAOFFENDY+izoom/2
C-------------------Loop through frames:
                    IMAX = 0
                    do iframe = 1,IFRAMS
                      ixc = nint(XPOSIT(K,L)/IAVE) + ixstart + nint((real(iframe-1)*(ixend-ixstart))/(IFRAMS-1))
                      iyc = nint(YPOSIT(K,L)/IAVE) + iystart + nint((real(iframe-1)*(iyend-iystart))/(IFRAMS-1))
                      ID=(iyc-1)*NCO2+ixc
                      IS=ID+IARRMXSI2*(iframe-1)
                      IMAX = IMAX + IARRAM(IS)
                    enddo
                    if(IMAX.gt.IOFFMAX)then
                      IOFFMAX=IMAX
                      IOFFSTARTX=ixstart
                      IOFFSTARTY=iystart
                      IOFFENDX=ixend
                      IOFFENDY=iyend
                    endif
                  enddo
                enddo
              enddo
!$OMP CRITICAL 
              if(IOFFMAX.gt.IGOFFMAX)then
                IGOFFMAX=IOFFMAX
                IGOFFSTARTX=IOFFSTARTX
                IGOFFSTARTY=IOFFSTARTY
                IGOFFENDX=IOFFENDX
                IGOFFENDY=IOFFENDY
              endif
!$OMP END CRITICAL 
            enddo
!$OMP END PARALLEL DO

C
C-----------Store optimal results 
C-----------The offset " - 1.0 " originates from some unknown error, 
C-----------Probably an origin discrepancy between CCmaps and unbending
C-----------coordinates. In any case, it is needed here. 
            ROSTARTX(K,L) = real(IGOFFSTARTX) - 1.0
            ROSTARTY(K,L) = real(IGOFFSTARTY) - 1.0
            ROENDX(K,L)   = real(IGOFFENDX) - 1.0
            ROENDY(K,L)   = real(IGOFFENDY) - 1.0
            write(6,'(2I5,'': Node '',2I5,'' at '',2F10.2,
     .        '' gives max peak '',I10,
     .        '' for offset from '',2F8.1,'' to '',2F8.1)')
     .        icount,ipeak,K,L,XPOSIT(K,L),YPOSIT(K,L),IGOFFMAX,
     .        ROSTARTX(K,L),ROSTARTY(K,L),ROENDX(K,L),ROENDY(K,L)
C
          else
            PEAK(K,L)=0.0
          endif
        enddo
      enddo
C
      call system("\rm -f 2dx_movie_refine.dat")
      open(11,FILE='2dx_movie_refine.dat',STATUS='NEW',ERR=980)
C
      write(11,'(''This is the PROFDATA information for all frames'')')
      write(11,'(''IMINA,IMAXA,IMINB,IMAXB = '')')
      write(11,'(4I8)')
     .           IMINA,IMAXA,IMINB,IMAXB
      write(11,'(''NCOL,NROW,IAVE = '')')
      write(11,'(3I8)')
     .           NCOL,NROW,IAVE
      write(11,'(''IMNY,IMXY = '')')
      write(11,'(2I8)')
     .           IMNY,IMXY
C
      write(11,'(''K,L are the lattice nodes (only those with PEAK '',
     .           ''are listed)'')')
      write(11,'(''X/YPOSIT is the position of the lattice node '',
     .           ''(with QUADSERCH corrections)'')')
      write(11,'(''PEAK is the QUADSERCH peak height of this lattice '',
     .           ''node.'')')
      write(11,'(''ROSTARTX/Y is the offset of the first frame'')')
      write(11,'(''ROENDX/Y is the offset of the last frame '')')
      write(11,'(''    K,    L,  XPOSIT(K,L),YPOSIT(K,L),PEAK(K,L),'',
     .         ''ROSTARTX(K,L),ROSTARTY(K,L),'',
     .         ''ROENDX(K,L),ROENDY(K,L)'')')
      do K=IMINA,IMAXA
        do L=IMINB,IMAXB
          if(PEAK(K,L).ne.0.0)then
            write(11,'(2I6,3G13.6,6F12.3)')
     .         K,L,XPOSIT(K,L),YPOSIT(K,L),PEAK(K,L),
     .         ROSTARTX(K,L),ROSTARTY(K,L),
     .         ROENDX(K,L),ROENDY(K,L)
          endif
        enddo
      enddo
      close(11)
C
      goto 999
C
980   continue
      STOP '::ERROR in writing output file'
C
990   continue
      STOP '::END-OF-FILE ERROR ON READING input image'
C
999   continue
C
      STOP
      END
C
C==========================================================
C
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


