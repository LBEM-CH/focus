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
      PARAMETER (IMAXFRAMES=40)
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
      INTEGER   iofflocx,iofflocy
      INTEGER   iminoffset,imaxoffset
      INTEGER   istart,iend
      INTEGER   NX,NY,NZ,MODE
      INTEGER   iframe,IFRAMS,IPEAK
      REAL      roffstep
      REAL      DMIN,DMAX,DMEAN
      REAL      RVAL,RVAL1,RVAL2,RVAL3
      REAL      DENMAX
      REAL      RX,RY
      INTEGER   IOFFMAX,IMAX
      REAL      ROFFFIRSTX,ROFFFIRSTY
      REAL      ROFFLASTX,ROFFLASTY
      REAL      ROFFCENX,ROFFCENY
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
      REAL      ROFIRSTX(IMNY:IMXY,IMNY:IMXY)
      REAL      ROFIRSTY(IMNY:IMXY,IMNY:IMXY)
      REAL      ROLASTX(IMNY:IMXY,IMNY:IMXY)
      REAL      ROLASTY(IMNY:IMXY,IMNY:IMXY)
      REAL      ROCENX(IMNY:IMXY,IMNY:IMXY)
      REAL      ROCENY(IMNY:IMXY,IMNY:IMXY)
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
        CALL IOPEN(cline,10,CFORM,MODE,NXYZ(1),NXYZ(2),
     +           NXYZ(3),'OLD',STEPR,TITLE)
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
C-----Calculate lattice node CCmap averages with variing offsets among frames:
C
      imaxoffset = 10
      iminoffset = -imaxoffset
C
      icount=0
      do K=IMINA,IMAXA
        do L=IMINB,IMAXB
C
C---------Check if this drifted patch would stay within the downsampled image:
C
C=======================
          roffstep = 5.0
C=======================
C
          istart =      int(roffstep*imaxoffset) + IMXI + 1
          iend = NCO2 - int(roffstep*imaxoffset) - IMXI - 1
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
                do ix=IMNI,IMXI
                  do iy=IMNI,IMXI
                    IFIELD(ix,iy)=0
                  enddo
                enddo
C
C---------------Add offset frames into IFIELD
C
                do iframe=1,IFRAMS
C-----------------Calculate offset in pixels
C-----------------The offset should be so that the last frame N is offset by the desired amount,
C                 RX(IFRAMS)=D
C-----------------and the frame N/3 is not offset at all.
C                 RX(IFRAMS/3)=0.0
C-----------------and the first frame is offset into the opposite direction by 50% of the offset of the last frame..
C                 RX(1)=-D/2
C-----------------This gives:
C                 RX(iframe)=-D/2 + D*(3/2) * (iframe-1)/(IFRAMS-1)
C                           = D/2 * (-1 + 3 * (iframe-1)/(IFRAMS-1))
C-----------------Or:
                  RX=real(ioffx)*roffstep*0.5*real(-1.0 + 3.0*real(iframe-1)/real(IFRAMS-1))
                  RY=real(ioffy)*roffstep*0.5*real(-1.0 + 3.0*real(iframe-1)/real(IFRAMS-1))
                  iofflocx=INT(XPOSIT(K,L)/real(IAVE)+RX/real(IAVE))
                  iofflocy=INT(YPOSIT(K,L)/real(IAVE)+RY/real(IAVE))
C
                  do ix=IMNI,IMXI
                    do iy=IMNI,IMXI
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
                do ix=IMNI,IMXI
                  do iy=IMNI,IMXI
                    if(IMAX.lt.IFIELD(ix,iy))then
                      IMAX=IFIELD(ix,iy)
                      rmaxx=real(ix*IAVE)
                      rmaxy=real(iy*IAVE)
                    endif
                  enddo
                enddo
                if(IMAX.gt.IOFFMAX)then
                  IOFFMAX=IMAX
                  ROFFCENX=rmaxx
                  ROFFCENY=rmaxy
                  ROFFLASTX=RX
                  ROFFLASTY=RY
                endif
C
              enddo
            enddo
C
C-----------Repeat with 10x smaller step size:
            roffstep = roffstep / imaxoffset
C
            do ioffx=iminoffset,imaxoffset
              do ioffy=iminoffset,imaxoffset
C
C---------------Zero aligned patch
                do ix=IMNI,IMXI
                  do iy=IMNI,IMXI
                    IFIELD(ix,iy)=0
                  enddo
                enddo
C
C---------------Add offset frames into IFIELD
                do iframe=1,IFRAMS
                  RX=(ROFFLASTX+real(ioffx)*roffstep)*0.5*real(-1.0 + 3.0*real(iframe-1)/real(IFRAMS-1))
                  RY=(ROFFLASTY+real(ioffy)*roffstep)*0.5*real(-1.0 + 3.0*real(iframe-1)/real(IFRAMS-1))
                  iofflocx=INT(XPOSIT(K,L)/real(IAVE)+RX/real(IAVE))
                  iofflocy=INT(YPOSIT(K,L)/real(IAVE)+RY/real(IAVE))
                  do ix=IMNI,IMXI
                    do iy=IMNI,IMXI
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
C---------------Interpolate IFIELD into IFIEL2 with 4x finer sampling
C
                do ix=IMNJ,IMXJ
                  do iy=IMNJ,IMXJ
                    ixx=NINT(real(ix)/real(iinterpol))
                    iyy=NINT(real(iy)/real(iinterpol))
                    if(ixx.gt.IMXI)ixx=IMXI
                    if(ixx.lt.IMNI)ixx=IMNI
                    if(iyy.gt.IMXI)iyy=IMXI
                    if(iyy.lt.IMNI)iyy=IMNI
                    IFIEL2(ix,iy)=IFIELD(ixx,iyy)
                  enddo
                enddo
C
C---------------Blurr IFIEL2 by adding 8 neighbors
C
                do ix=IMNJ+1,IMXJ-1
                  do iy=IMNJ+1,IMXJ-1
                    IFIEL3(ix,iy)=(IFIEL2(ix  ,iy  )
     .                            +IFIEL2(ix-1,iy  )
     .                            +IFIEL2(ix+1,iy  )
     .                            +IFIEL2(ix  ,iy-1)
     .                            +IFIEL2(ix  ,iy+1)
     .                            +IFIEL2(ix-1,iy-1)
     .                            +IFIEL2(ix-1,iy+1)
     .                            +IFIEL2(ix+1,iy-1)
     .                            +IFIEL2(ix+1,iy+1))
                  enddo
                enddo
C
                do ix=IMNJ+2,IMXJ-2
                  do iy=IMNJ+2,IMXJ-2
                    IFIEL2(ix,iy)=(IFIEL3(ix  ,iy  )
     .                            +IFIEL3(ix-1,iy  )
     .                            +IFIEL3(ix+1,iy  )
     .                            +IFIEL3(ix  ,iy-1)
     .                            +IFIEL3(ix  ,iy+1)
     .                            +IFIEL3(ix-1,iy-1)
     .                            +IFIEL3(ix-1,iy+1)
     .                            +IFIEL3(ix+1,iy-1)
     .                            +IFIEL3(ix+1,iy+1))
                  enddo
                enddo
C
                IMAX=-30000
                do ix=IMNJ+3,IMXJ-3
                  do iy=IMNJ+3,IMXJ-3
                    if(IMAX.lt.IFIEL2(ix,iy))then
                      IMAX=IFIEL2(ix,iy)
                      rmaxx=real(ix*IAVE)/real(iinterpol)
                      rmaxy=real(iy*IAVE)/real(iinterpol)
                    endif
                  enddo
                enddo
                if(IMAX.gt.IOFFMAX)then
                  IOFFMAX=IMAX
                  ROFFCENX=rmaxx
                  ROFFCENY=rmaxy
                  ROFFLASTX=RX
                  ROFFLASTY=RY
                endif
C
              enddo
            enddo
C
C-----------Store optimal results 
C-----------The last frame is twice as much offset in one direction, 
C-----------as the first frame is in the other direction, so that the 
C-----------frame N/3 is not offset at all (but all is offset by ROFFCEN)):
            ROLASTX(K,L)  =  ROFFLASTX
            ROLASTY(K,L)  =  ROFFLASTY
            ROFIRSTX(K,L) = -ROFFLASTX/2.0
            ROFIRSTY(K,L) = -ROFFLASTY/2.0
            ROCENX(K,L)   =  ROFFCENX
            ROCENY(K,L)   =  ROFFCENY
            write(6,'(2I5,'': Node '',2I5,'' at '',2F10.2,
     .        '' gives max peak '',I10,
     .        '' for offset from '',2F8.1,'' to '',2F8.1,
     .        '', center offset '',2F8.1)')
     .        icount,ipeak,K,L,XPOSIT(K,L),YPOSIT(K,L),IOFFMAX,
     .        ROFIRSTX(K,L),ROFIRSTY(K,L),ROLASTX(K,L),ROLASTY(K,L),
     .        ROCENX(K,L),ROCENY(K,L)
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
      write(11,'(''  K,  L,  XPOSIT(K,L),YPOSIT(K,L),PEAK(K,L),'',
     .         ''ROFIRSTX(K,L),ROFIRSTY(K,L),'',
     .         ''ROLASTX(K,L),ROLASTY(K,L),'',
     .         ''ROCENX(K,L),ROCENY(K,L))'')')
      do K=IMINA,IMAXA
        do L=IMINB,IMAXB
          if(PEAK(K,L).ne.0.0)then
C-----------This is against ugly rounding errors, showing as 0.999 results
            ROLASTX(K,L)=ROLASTX(K,L)+0.001
            ROLASTY(K,L)=ROLASTY(K,L)+0.001
C            ROFIRSTX(K,L)=0.0
C            ROFIRSTY(K,L)=0.0
C            ROLASTX(K,L)=0.0
C            ROLASTY(K,L)=0.0
C            ROCENX(K,L)=0.0
C            ROCENY(K,L)=0.0
            write(11,'(2I6,3G13.6,6F12.3)')
     .         K,L,XPOSIT(K,L),YPOSIT(K,L),PEAK(K,L),
     .         ROFIRSTX(K,L),ROFIRSTY(K,L),
     .         ROLASTX(K,L),ROLASTY(K,L),
     .         ROCENX(K,L),ROCENY(K,L)
          endif
        enddo
      enddo
      close(11)
C
C      do iframe = 1,IFRAMS
C        if(iframe.lt.10)then
C          write(cline,'(''frames/PROFDATA_'',I1,''.dat'')')iframe
C        elseif(iframe.lt.100)then
C          write(cline,'(''frames/PROFDATA_'',I2,''.dat'')')iframe
C        else
C          write(cline,'(''frames/PROFDATA_'',I3,''.dat'')')iframe
C        endif
C        call shorten(cline,k)
C        write(cline2,'(''\rm -f '',A)')cline(1:k)
C        call shorten(cline2,k)
C        call system(cline2(1:k))
C        call shorten(cline,k)
C        open(10,FILE=cline(1:k),STATUS='NEW',ERR=980)
C        do i=1,5
C          call shorten(comment(i),k)
C          write(10,'(A)')comment(i)(1:k)
C        enddo
C        write(10,*)NC,NR,IC,IR,A1,A2,B1,B2,IMINA,IMAXA,IMINB,IMAXB
C        WRITE(10,*)DENMAX
C        do K=IMINA,IMAXA
C          do L=IMINB,IMAXB
C            if(PEAK(K,L).gt.0.0)then
C              RX=XPOSIT(K,L)+ROFIRSTX(K,L)*REAL(IFRAMS-iframe)/REAL(IFRAMS-1)
C     .                      +ROLASTX(K,L) *REAL(iframe-1)     /REAL(IFRAMS-1)
C     .                      +ROCENX(K,L)
C              RY=YPOSIT(K,L)+ROFIRSTY(K,L)*REAL(IFRAMS-iframe)/REAL(IFRAMS-1)
C     .                      +ROLASTY(K,L) *REAL(iframe-1)     /REAL(IFRAMS-1)
C     .                      +ROCENY(K,L)
C            else
C              RX=0.0
C              RY=0.0
C            endif
C            WRITE(10,'(3G16.6)')RX,RY,PEAK(K,L)
C          enddo
C        enddo
C        close(10)
C      enddo
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


