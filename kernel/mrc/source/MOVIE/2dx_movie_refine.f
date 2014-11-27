C  2dx_movie_refine.for 
C
C       VX1.0   HS     25.11.14  2dx - First version
C
      IMPLICIT NONE
C
      INTEGER IARRMXSIZ,IARRMXSI2,IMNY,IMXY,IMNI,IMXI
      INTEGER IMAXFRAMES
C-----Max input image is 16000 x 16000, which is an array of 256'000'000
C-----Max input image is 12000 x 12000, which is an array of 144'000'000
C-----Max input image is 4096 x 4096, which is an array of 16'777'216
      PARAMETER (IARRMXSIZ=16777216)
C-----Max input image 2xdownsampled is 6000 x 6000, which is an array of 36'000'000
C-----Max input image 2xdownsampled is 2048 x 2048, which is an array of 4'194'304
      PARAMETER (IARRMXSI2=4194304)
C      PARAMETER (IARRMXSI2=1000)
C
C-----These have to be the same as in 2dx_quadserchk-2.for, currently 240
      PARAMETER (IMNY=-240)
      PARAMETER (IMXY= 240)
C
      PARAMETER (IMNI=-20)
      PARAMETER (IMXI= 20)
C
      PARAMETER (IMAXFRAMES=40)
C
      CHARACTER TITLE*1600
C
      INTEGER   NXYZ(3)
C
      REAL*8    DOUBLMEAN,DARRAYMEAN,DARRAYMIN,DARRAYMAX
      INTEGER   I,J,K,L,IC,IR,NC,NR,H,IVAL
      INTEGER   ioffx,ioffy,ix,iy
      INTEGER   IRO,ICO,NROW,NCOL,NRO2,NCO2
      INTEGER   IXMIN,IYMIN,IZMIN,IXMAX,IYMAX,IZMAX
      INTEGER   IMAXA,IMAXB,IMINA,IMINB
      INTEGER   ID
      INTEGER   iofflocx,iofflocy
      INTEGER   iframemin,iframemax
      INTEGER   iminoffset,imaxoffset
      INTEGER   istart,iend
      INTEGER   NX,NY,NZ,MODE
      INTEGER   iframe,IFRAMS,IPEAK
      REAL      roffstep
      REAL      DMIN,DMAX,DMEAN
      REAL      RVAL
      REAL      DENMAX
      REAL      RX,RY
      INTEGER   IOFFMAX,IMAX
      REAL      ROFFFIRSTX,ROFFFIRSTY
      REAL      ROFFLASTX,ROFFLASTY
      REAL      ROFFCENX,ROFFCENY
      INTEGER   imaxx,imaxy
      INTEGER   icount
C
      INTEGER   IMP
      CHARACTER NCPUS*10
      INTEGER   OMP_GET_NUM_PROCS
C
      CHARACTER CFORM
      INTEGER   IP,IAVE,IS,IERR
      REAL      STEPR,DSTEP,R
C
      CHARACTER*200 FILE1,FILE2
      CHARACTER*200 cline,cline2
      CHARACTER*200 comment(5)
C
      LOGICAL   EX
C
      REAL      A1,B1,A2,B2
C
      INTEGER   IFIELD(IMNI:IMXI,IMNI:IMXI)
C
      REAL      XPOSIT(IMNY:IMXY,IMNY:IMXY)
      REAL      YPOSIT(IMNY:IMXY,IMNY:IMXY)
      REAL      PEAK(IMNY:IMXY,IMNY:IMXY)
C
      REAL      ROFIRSTX(IMNY:IMXY,IMNY:IMXY)
      REAL      ROFIRSTY(IMNY:IMXY,IMNY:IMXY)
      REAL      ROLASTX(IMNY:IMXY,IMNY:IMXY)
      REAL      ROLASTY(IMNY:IMXY,IMNY:IMXY)
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
        CALL IOPEN(cline,10,CFORM,MODE,NXYZ(1),NXYZ(2),
     +           NXYZ(3),'OLD',STEPR,TITLE)
C
        WRITE(*,'('':Reading image '',A,'', NX,NY = '',2I10)')
     .     cline(1:k),NXYZ(1),NXYZ(2)
C
        IAVE=2
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
          ALLOCATE(IARRAM(IARRMXSI2*IMAXFRAMES),STAT=IERR)
          IF (IERR.NE.0) THEN
            WRITE(*,*) ':: ERROR: Memory allocation failed in MAIN'
            STOP ':: Try reducing the MAXFRAMES'
          ENDIF

        endif
C
C-------Read in image, and compress by 2x on the same time.
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

      ipeak=0
      DO K=IMINA,IMAXA
        DO L=IMINB,IMAXB
          read(13,*)XPOSIT(K,L),YPOSIT(K,L),PEAK(K,L)
          if(PEAK(K,L).ne.0.0)then
            ipeak=ipeak+1
          endif
        enddo
      enddo
      close(13)
      write(*,'(''Read PROFDATA field, found '',I8,
     .   '' non-zero peaks'')')ipeak
C
C-----Calculate lattice node CCmap averages with variing offsets among frames:
C
      imaxoffset = 10
      iminoffset = -imaxoffset
C
      iframemax = 5 
      iframemin = -iframemax
      if(iframemax.gt.IMXI)then
        STOP '::ERROR, iframemax too large'
      endif
C
      icount=0
      do K=IMINA,IMAXA
        do L=IMINB,IMAXB
C
C---------Check if this drifted patch would stay within the downsampled image:
C
          roffstep = 3.0
          istart =      int(roffstep*imaxoffset) + iframemax + 1
          iend = NCO2 - int(roffstep*imaxoffset) - iframemax - 1
C
          ICO=INT(XPOSIT(K,L)/IAVE)
          IRO=INT(YPOSIT(K,L)/IAVE)
          if(PEAK(K,L).ne.0.0 .and.
     .       ICO.gt.istart .and. ICO.le.iend .and. 
     .       IRO.gt.istart .and. IRO.le.iend) then
C
            icount=icount+1
C
C-----------Initiate offset position for last frame:
            IOFFMAX = -30000
            ROFFFIRSTX = 0.0
            ROFFFIRSTY = 0.0
            ROFFLASTX = 0.0
            ROFFLASTY = 0.0
C
            do ioffx=iminoffset,imaxoffset
              do ioffy=iminoffset,imaxoffset
C
C---------------Zero aligned patch
                do ix=iframemin,iframemax
                  do iy=iframemin,iframemax
                    IFIELD(ix,iy)=0
                  enddo
                enddo
C
C---------------Add offset frames into IFIELD
C
                do iframe=1,IFRAMS
C-----------------Calculate offset in pixels
C-----------------The offset should be so that the last frame N is offset by the desired amount,
C-----------------and the frame N/4 is not offset at all.
C-----------------That means that the first frame is offset into the opposite direction.
C-----------------This means: RX=ioffx*roffstep*(iframe-IFRAMS/4)/(IFRAMS*3/4)
C-----------------This is the same as below:
                  RX=real(ioffx)*roffstep*real(4*iframe-IFRAMS)/real(3*IFRAMS)
                  RY=real(ioffy)*roffstep*real(4*iframe-IFRAMS)/real(3*IFRAMS)
                  iofflocx=INT(XPOSIT(K,L)/real(IAVE)+RX/real(IAVE))
                  iofflocy=INT(YPOSIT(K,L)/real(IAVE)+RY/real(IAVE))
C
                  do ix=iframemin,iframemax
                    do iy=iframemin,iframemax
                      ICO=iofflocx+ix
                      IRO=iofflocy+iy
                      ID=(IRO-1)*NCO2+ICO
                      IS=ID+IARRMXSI2*(iframe-1)
                      IFIELD(ix,iy)=IFIELD(ix,iy)+IARRAM(IS)
                    enddo
                  enddo
                enddo
C
C---------------Find maximum peak in IFIELD, but only in inner 10x10 pixels
C
                IMAX=-30000
                do ix=iframemin,iframemax
                  do iy=iframemin,iframemax
                    if(IMAX.lt.IFIELD(ix,iy))then
                      IMAX=IFIELD(ix,iy)
                      imaxx=ix*IAVE
                      imaxy=iy*IAVE
                    endif
                  enddo
                enddo
                if(IMAX.gt.IOFFMAX)then
                  IOFFMAX=IMAX
                  ROFFCENX=imaxx
                  ROFFCENY=imaxy
                  ROFFLASTX=RX
                  ROFFLASTY=RY
                endif
C
              enddo
            enddo
C
C-----------Repeat with 10x smaller step size:
C
            roffstep = 0.3
            do ioffx=iminoffset,imaxoffset
              do ioffy=iminoffset,imaxoffset
C
C---------------Zero aligned patch
                do ix=iframemin,iframemax
                  do iy=iframemin,iframemax
                    IFIELD(ix,iy)=0
                  enddo
                enddo
C
C---------------Add offset frames into IFIELD
                do iframe=1,IFRAMS
                  RX=real(ioffx)*roffstep*real(4*iframe-IFRAMS)/real(3*IFRAMS)
                  RY=real(ioffy)*roffstep*real(4*iframe-IFRAMS)/real(3*IFRAMS)
                  RX=RX+ROFFLASTX
                  RY=RY+ROFFLASTY
                  iofflocx=INT(XPOSIT(K,L)/real(IAVE)+RX/real(IAVE))
                  iofflocy=INT(YPOSIT(K,L)/real(IAVE)+RY/real(IAVE))
                  do ix=iframemin,iframemax
                    do iy=iframemin,iframemax
                      ICO=iofflocx+ix
                      IRO=iofflocy+iy
                      ID=(IRO-1)*NCO2+ICO
                      IS=ID+IARRMXSI2*(iframe-1)
                      IFIELD(ix,iy)=IFIELD(ix,iy)+IARRAM(IS)
                    enddo
                  enddo
                enddo
C
C---------------Find maximum peak in IFIELD, but only in inner 10x10 pixels
                IMAX=-30000
                do ix=iframemin,iframemax
                  do iy=iframemin,iframemax
                    if(IMAX.lt.IFIELD(ix,iy))then
                      IMAX=IFIELD(ix,iy)
                      imaxx=ix*IAVE
                      imaxy=iy*IAVE
                    endif
                  enddo
                enddo
                if(IMAX.gt.IOFFMAX)then
                  IOFFMAX=IMAX
                  ROFFCENX=imaxx
                  ROFFCENY=imaxy
                  ROFFLASTX=RX
                  ROFFLASTY=RY
                endif
C
              enddo
            enddo
C
C-----------Store optimal results:
            ROFFFIRSTX=-ROFFLASTX/3.0
            ROFFFIRSTY=-ROFFLASTY/3.0
            ROFIRSTX(K,L)=ROFFFIRSTX+ROFFCENX
            ROFIRSTY(K,L)=ROFFFIRSTY+ROFFCENY
            ROLASTX(K,L) =ROFFLASTX+ROFFCENX
            ROLASTY(K,L) =ROFFLASTY+ROFFCENY
            write(6,'(2I5,'': Node '',2I5,'' at '',2F10.2,
     .        '' gives max peak '',I10,
     .        '' for offset from '',2F8.1,'' to '',2F8.1)')
     .        icount,ipeak,K,L,XPOSIT(K,L),YPOSIT(K,L),IOFFMAX,
     .        ROFIRSTX(K,L),ROFIRSTY(K,L),ROLASTX(K,L),ROLASTY(K,L)
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
      write(11,'(''ROFIRSTX/Y is the offset of the first frame'')')
      write(11,'(''ROLASTX/Y is the offset of the last frame '')')
      write(11,'(''  K,  L,  XPOSIT(K,L),YPOSIT(K,L),PEAK(K,L),''
     .         ''ROFIRSTX(K,L),ROFIRSTY(K,L),'',
     .         ''ROLASTX(K,L),ROLASTY(K,L)'')')
      do K=IMINA,IMAXA
        do L=IMINB,IMAXB
          if(PEAK(K,L).ne.0.0)then
            write(11,'(2I6,3G13.6,4F10.2)')
     .         K,L,XPOSIT(K,L),YPOSIT(K,L),PEAK(K,L),
     .         ROFIRSTX(K,L),ROFIRSTY(K,L),
     .         ROLASTX(K,L),ROLASTY(K,L)
          endif
        enddo
      enddo
      close(11)
C
      do iframe = 1,IFRAMS
        if(iframe.lt.10)then
          write(cline,'(''frames/PROFDATA_'',I1,''.dat'')')iframe
        elseif(iframe.lt.100)then
          write(cline,'(''frames/PROFDATA_'',I2,''.dat'')')iframe
        else
          write(cline,'(''frames/PROFDATA_'',I3,''.dat'')')iframe
        endif
        call shorten(cline,k)
        write(cline2,'(''\rm -f '',A)')cline(1:k)
        call shorten(cline2,k)
        call system(cline2(1:k))
        call shorten(cline,k)
        open(10,FILE=cline(1:k),STATUS='NEW',ERR=980)
        do i=1,5
          call shorten(comment(i),k)
          write(10,'(A)')comment(i)(1:k)
        enddo
        write(10,*)NC,NR,IC,IR,A1,A2,B1,B2,IMINA,IMAXA,IMINB,IMAXB
        WRITE(10,*)DENMAX
        do K=IMINA,IMAXA
          do L=IMINB,IMAXB
            if(PEAK(K,L).ne.0.0)then
              RX=XPOSIT(K,L)+ROFIRSTX(K,L)+ROLASTX(K,L)*REAL(iframe)/REAL(IFRAMS)
              RY=YPOSIT(K,L)+ROFIRSTY(K,L)+ROLASTY(K,L)*REAL(iframe)/REAL(IFRAMS)
            else
              RX=0.0
              RY=0.0
            endif
            WRITE(10,'(3G16.6)')RX,RY,PEAK(K,L)
          enddo
        enddo
        close(10)
      enddo
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


