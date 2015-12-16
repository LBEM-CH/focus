C  QUADSERCHK
C
C   Remember to change version number in first write statement.
C       VX1.0   JMB     1988     MODIFIED FROM QUADSERCH2
C       VX1.1   RH      19.9.88  DIMENSIONS OF MDC, MDR EXPANDEDRA
C       VX1.2   RH      3.11.88  PLOT CHARACTERS CHANGED FOR SPEED.
C       VX1.3   RH     11.12.88  Changed plot characters, & profile printout.
C       VX1.4   RH     22.12.88  Performs profile fit over elliptical area.
C       VX1.5   RH       1.4.90  INCREASE KDC,KDR max dimensions.
C       VX1.6   RH       1.7.90  some useful control card instructions.
C       VX2.0   RH       1.1.92  convert to UNIX for Alliant
C       VX2.1   RH       9.7.94  test BIGFACTR to avoid edge of profile
C       VX2.2   RH      29.8.94  read in entire image to avoid diskio overhead
C       VX2.3   RH       3.9.94  test negative values BIGFACTR as above
C       VX2.4   RH      14.4.95  normalise PROFILE(avoid overflow)+9030 format
C       VX2.5   RH      29.4.95  add date and time to plots
C       VX2.6   RH       1.6.95  double precision CCFCALC summations
C       VX2.7   RH      25.7.95  ENCODE debug for Alpha
C       VX2.8   JMS     03.5.96  ARRAY put in common to be compatible with sgi
C       VX2.9   JMS    13.06.96  Variable NCALLPROF initialized to 0
C       VX3.0   RH     15.08.97  increase dimensions of search to 240
C       VX3.1   RH      4.12.97  optional real or reciprocal space latt params
C                                 input card 4 changed, therefore QUADSERCHC 
C       VX3.2   RH      11.3.98  add QUADSERCHC to plot title
C       VX4.0   RH      23.8.00  convert to plot2000 direct postscript output
C       VX5.0   HS     31.10.05  2dx
C       VX6.0   HS      5.10.14  2dx - Adding origin offset
C
C  MODIFIED JUN 1987 TO HANDLE RECTANGULAR IMAGES.   JMB.
C  MODIFIED FROM PROFSERCH DEC 1986.   JMB.
C
C  NOW PREDICTS DIFFERENCE BETWEEN LATTICE POSITION AND SEARCH POSITION
C  FROM LOCAL AREA NEAR POINT, OF DIMENSIONS +/- NRANGE IN A AND B.
C  ON FIRST PASS ONLY THE AREA ALREADY PASSED IN THE SEARCH PROCEDURE
C  CONTAINS USEABLE INFORMATION; 
C  IN OPTIONAL SECOND PASS THE AREA AHEAD
C  OF THE CURRENT POINT CONTAINS USEABLE INFORMATION STORED FROM PASS 1.
C
C  CROSS-CORRELATION SEARCHING PROGRAM 15.8.84
C  SEARCHES A CROSS-CORRELATION MAP CALCULATED
C  SEPARATELY BY THE FFT METHOD BUT GIVES OUTPUT IN SIMILAR FORMAT
C  TO THE CORNELL REAL-SPACE PROGRAM CCOR.
C
C  THIS PROGRAM PRODUCES :-
C        1. The file 'PROFDATA'; it contains the data for use in CCUNBENDA.
C           First the information previously transferrd in file 'PIXPARMS';
C           Then the list of best correlation peak positions and the
C           heights of their correlation peaks.   
C        2. A plot of the lattice positions searched in which; 1) error 
C           vectors are shown X10; 2) peak heights are shown as grey levels.
C        3. The file 'ERRORS'; this contains a list of the differences
C           between actual positions of peaks and lattice positions. It
C           can be used in a second pass through this program if some
C           patches gave bad correlation peaks first time through and look
C           as if they could be improved with hindsight
C
C-------CORRELATION PEAKS ARE SEARCHED FOR AROUND THEIR
C-------EXPECTED POSITION BASED ON INPUT LATTICE PARAMETERS
C
C UNIX compile and link as below
C f77 -o quadserchb.exe quadserchb.for ${IMAGELIB}/imlib.a \ 
C                                          ${IMAGELIB}/genlib.a \
C                                          ${IMAGELIB}/plot82lib.a
C
C   FILE STRUCTURE IS
C     INPUT:
C
C     CARDS ON UNIT 5 : 
C       1       IPASS,NRANGE            ! controls search learning algorithm.
C       2       FILENAME                ! name of cross-correlation file
C       2b      FILE1                   ! name of input image (only if IPASS>9)
C       2c      FILE2                   ! name of (aligned) output image (only if IPASS>9)
C       3       ISIZEX,ISIZEY           ! SIZE OF TRANSFORM
C       4       ASTR1,ASTR2,BSTR1,BSTR2,LREAL
C                                       ! Lattice vectors, real(T) or recip(F)
C       5       MINA,MAXA,MINB,MAXB     ! NUMBER UNIT CELLS TO SEARCH
C       6       KDC,KDR,IOFFX,IOFFY     ! RADIUS OF CORR SEARCH, 
C                                       ! Radius of central offset (zero for none, only if IPASS>9)
C       7       IC,IR                   ! POSN OF SEARCH START (0,0 IS ORIGIN)
C       8       IPRNT                   ! YES/NO FOR DETAILED PRINTOUT
C       9       RADLIMP,RADLIMQ,RADANGP ! ELLIPTICAL CUTOFF.
CHEN>
C      10       VALSPOTSCAN,RMAG,CCOLOR ! Defines if SpotScan processing should be done (0=no)
C      12       DoMASKpic               ! Define, if Masking maps should be generated (0=no)
C      11       DoXCFpic                ! Define, if XCF picture should be generated (0=no)
CHEN<
C
C         IPASS -0 no error input or output, simple search only.
C               -1 writes error file with peak positions for use in later pass.
C               -2 reads error file for use in better initial peak predict.
C               -3 reads and writes error file for use in better initial peak predict.
C               -10 as for 0, but reading image and writing (aligned) image out
C               -11 as for 0, but reading image and writing (aligned) image out
C               -12 as for 0, but reading image and writing (aligned) image out
C               -13 as for 0, but reading image and writing (aligned) image out
C         NRANGE- range of previous peaks used in prediction of next peak posn.
C         ISIZEX- size of transform in x-pixels (eg. 3000,3000)
C         ISIZEY-                  and y-pixels
C         ASTR1 - reciprocal space lattice vectors.
C         ASTR2 -       ""
C         BSTR1 -       ""
C         BSTR2 -       ""
C         LREAL - use real space params if T, recip space if F
C         MINA  - number of unit cells to search for in each direction from
C         MAXA  - search origin IC,IR  e.g.(-120,120,-120,120)
C         MINB  -   ""
C         MAXB  -   ""
C         KDC   - search over +/- this number of pixels on each side of the
C         KDR   - predicted centre of each correlation peak.
C         IOFFX - search over +/- this number of pixels on each side for the offset of the 
C         IOFFY - central lattice. Use 0,0 for none.
C         IC    - position of search origin for the first correlation peak
C         IR    - relative to corner of image at 0,0 -  e.g.(1500,1500)
C         IPRNT - more (Y) or less (N) printout
C         RADLIMP- radius for profile fit in profile units in one direction
C         RADLIMQ- same in orthogonal direction -- (20x smaller than pixels)
C         RADANGP- angle relative to x-axis of RADLIMP (elliptical)
C
C         RMAG   - Magnification for line length in distortion plots
C         CCOLOR - y or n (character*1) True for color output in PS plotfiles
C
C     INPUT FILES :
C         ERRORS      - (Created if IPASS=1); Read if IPASS=2,3; Contains
C                     - list of XERROR,YERROR,PEAK found when IPASS=1
C                     - not written or read if IPASS=0
C         ERROUT      - (Created if IPASS=3); Contains
C                     - list of XERROR,YERROR,PEAK found when IPASS=3
C                     - not written or read if IPASS=0v1
C         PROFILE     - Profile used for matching against correlation peaks.
C                     - This has been previously obtained from procedure
C                     - AUTOCORRL
C     OUTPUT FILES:
C         PROFDATA    - File contains;
C                     - Parameters to be transferred between programs,
C                     - including data read in here, maximum value of 
C                     - peak height, raw list of correlation peak positions
C                     - and heights produced by this program and to be used
C                     - by CCUNBENDA
C         ERRORS      - Produced when IPASS=1v2; File contains list of
C                        XERROR(IA,IB),YERROR(IA,IB),PEAK(IA,IB)
C
C-----Max input image is 16000 x 16000, which is an array of 256'000'000
C-----Max input image is 12000 x 12000, which is an array of 144'000'000
C      PARAMETER (IARRMXSIZ=256000000)
      PARAMETER (IARRMXSIZ=144000000)
      PARAMETER (MDR=120)
      PARAMETER (MDC=120)
      PARAMETER (MNY=-240)
      PARAMETER (MXY=240)
      PARAMETER (NDATA=40)
      PARAMETER (NSMOTH=5)
C
      PARAMETER (MNI=-500)
      PARAMETER (MXI= 500)
C----------------MFIELD needs to be MXI-MNI
      PARAMETER (MFIELD=1001)
C
      DIMENSION ARRAY(IARRMXSIZ)
      DIMENSION ARRA2(IARRMXSIZ)
      DIMENSION NASTOP(2)
      DIMENSION NSTART(10),NFIN(10),NSTEP(10)
      DIMENSION PROFILE(101,101),NCOUNT(11),RADSTORE(-180:180)
      DIMENSION TITLE(20)
      DIMENSION TITPLOT(20)
      DIMENSION COOR(3,MNY:MXY,MNY:MXY)
C
      DIMENSION XERROR(MNY:MXY,MNY:MXY),YERROR(MNY:MXY,MNY:MXY)
      DIMENSION PEAK(MNY:MXY,MNY:MXY)
      DIMENSION XC(MDC,MDR)           ! this should be (x,y)
C
      PARAMETER (LPIC=12000)
      PARAMETER (LMAX=20100)
      DIMENSION APIC2(LPIC,LPIC)
      DIMENSION ALINE(LMAX)
      DIMENSION NXYZ(3),MXYZ(3),NXYZST(3)
      DIMENSION NXYZ2(3),MXYZ2(3)
      DIMENSION OUT(LMAX),OU2(LMAX)
      DIMENSION LABELS(20,10),CELL(6),EXTRA(29)
      DIMENSION DNCELL(6),MXYZN(3)
C
      REAL RFIELD(MNI:MXI,MNI:MXI)
      REAL*8 DOUBLMEAN
      INTEGER IFIEL1(MNI:MXI,MNI:MXI)
      INTEGER IFIEL2(MFIELD,MFIELD)
      LOGICAL LREAL,LCOLOR
      CHARACTER*1 CCOLOR
      INTEGER IOFFX,IOFFY
      CHARACTER*200 FILE1
      CHARACTER*200 FILE2
      CHARACTER*200 FILE3
      CHARACTER*200 cline
      CHARACTER*80 FILENAM
      CHARACTER*80 TEXT
      CHARACTER*80 NAME
C
      EQUIVALENCE (TITPLOT,NAME)
C
      COMMON//NX,NY,NZ,IXMIN,IYMIN,IZMIN,IXMAX,IYMAX,IZMAX
      COMMON/BIG/ARRAY,ARRA2
      COMMON/PROFITC/PROFILE,XC
C
C     DATA FOR PROFIT
      DATA NSTEP/5,1,1,1,1,1,1,1,1,1/
      DATA NSTART/-10,-7,-5,-3,-3,-3,-3,-3,-3,-3/
      DATA NFIN/10,7,5,3,3,3,3,3,3,3/
      DATA NCOUNT/11*0/
      DATA NPASSLIM/9/
      DATA NXYZST/3*0/, CNV/57.29578/
C
      DATA CORFAC/10.0/                                      
      EQUIVALENCE (NCOL,NXYZ(1)),(NLINE,NXYZ(2))
C
      XCOORD(I,J)=A1*I+B1*J+IC+1.0
      YCOORD(I,J)=A2*I+B2*J+IR+1.0
C
C*** initialization added by JMS 06.03.96
        do k=mny,mxy
         do j=mny,mxy
          do i=1,3
           coor(i,j,k) = 0.
          end do
         end do
        end do
C*** initialization added by JMS 13.06.96
        ncallprof = 0
C
      WRITE(6,1)
1     FORMAT(/,' QUADSERCHK 5.0 (30.10.05), searches cross-correlation',
     . ' map one quadrant at a time, and fits profile to peaks',/,/)
      READ(5,*)IPASS,NRANGE
      if(IPASS.gt.9 .and. IPASS.lt.19)then
        IPASS=IPASS-10
        IALIGN=1
      else
        IALIGN=0
      endif
      WRITE(6,39003)IPASS,NRANGE
39003   FORMAT(' IPASS=',I2,' NRANGE=',I3,/)
      IF(IPASS.NE.1.AND.IPASS.NE.2.AND.IPASS.NE.3)IPASS=0
      WRITE(6,9003)
C
9003  FORMAT('$NAME OF CROSS-CORRELATION FILE TO BE SEARCHED? ')
      read(5,'(A)')FILE3
C      READ(5,19006) TITPLOT
C19006 FORMAT(20A4)
C      WRITE(6,9006) TITPLOT
C9006  FORMAT(1X,20A4)
      call shorten(FILE3,k)
      write(6,'(''Read: '',A)')FILE3(1:k)
      do i=1,20
        TITLE(i)=TITPLOT(i)
      enddo
C
      if(IALIGN.eq.1)then
        write(6,'(''Give name of input image'')')
        read(5,'(A)')FILE1
        call shorten(FILE1,k)
        write(6,'(''Read: '',A)')FILE1(1:k)
C
        write(6,'(''Give name of aligned output image'')')
        read(5,'(A)')FILE2
        call shorten(FILE2,k)
        write(6,'(''Read: '',A)')FILE2(1:k)
      endif
C
C-----Read in entire CC map into ARRAY
C
      write(6,'(''Reading CC map '',A)')FILE3
      call shorten(FILE3,k)
      call system("\rm -f tmp.mrc")
      write(cline,'(''\cp -f '',A,'' tmp.mrc'')')FILE3(1:k)
      call shorten(cline,k)
      call system(cline(1:k))
      write(NAME,'(''tmp.mrc'')')
      CALL IMOPEN(1,NAME,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      IF(NCOL*NLINE.GT.IARRMXSIZ) STOP '::  IARRMXSIZ too small'
      CALL IMPOSN(1,0,0)
      CALL IRDPAS(1,ARRAY,NCOL,NLINE,0,NCOL-1,0,NLINE-1,*9400)
      CALL IMCLOSE(1)
      write(6,'(''CC map read.'')')
C
      WRITE(6,8007)
8007  FORMAT('$ISIZEX,ISIZEY? ')
      READ(5,*)ISIZEX,ISIZEY
      WRITE(6,*)ISIZEX,ISIZEY
      WRITE(6,9008)
9008  FORMAT('$REAL/RECIPROCAL SPACE LATTICE PARAMETERS',
     .  ' ASTR1,ASTR2,BSTR1,BSTR2,LREAL? ')
      READ(5,*) ASTR1,ASTR2,BSTR1,BSTR2,LREAL
      WRITE(6,*) ASTR1,ASTR2,BSTR1,BSTR2,LREAL

C-----LREAL canbe used to bypass the next few lines of code.

      IF(LREAL) THEN    ! real space lattice parameters
        A1=ASTR1
        A2=ASTR2
        B1=BSTR1
        B2=BSTR2
      ELSE              ! reciprocal space lattice parameters
C               CALCULATE REAL SPACE LATTICE PARAMETERS FROM RECIPROCAL;
C               IE X AND Y CORRDINATES OF (1,0) AND (0,1)
C               ADJUST COMPONENTS 2 FOR DIFFERENCE IN X AND Y SAMPLING IF 
C               RECTANGULAR IMAGE
        SIZEX=ISIZEX
        ADJUST=SIZEX/ISIZEY
        ASTR2=ASTR2*ADJUST
        BSTR2=BSTR2*ADJUST
        PI2=1.570796327
        ASTR=SQRT(ASTR1**2+ASTR2**2)
        BSTR=SQRT(BSTR1**2+BSTR2**2)
C
        SINASTR=ASTR2/ASTR
        COSASTR=ASTR1/ASTR
        SINBSTR=BSTR2/BSTR
        COSBSTR=BSTR1/BSTR
C
        SINGMSTR=SINASTR*COSBSTR-COSASTR*SINBSTR
        COSGMSTR=SINASTR*SINBSTR+COSASTR*COSBSTR
        GAMMASTR=ATAN2(SINGMSTR,COSGMSTR)
C               GAMMASTR IS GT -PI AND LE PI
C               IF GAMMASTR IS +VE C IS ALONG -Z; IF -VE C IS ALONG +Z
        IF(GAMMASTR.GE.0.0)THEN
                AA1=-BSTR2
                AA2=+BSTR1
                BB1=+ASTR2
                BB2=-ASTR1
        ELSE
                AA1=+BSTR2
                AA2=-BSTR1
                BB1=-ASTR2
                BB2=+ASTR1
        END IF
        SINA=AA2/BSTR
        COSA=AA1/BSTR
        SINB=BB2/ASTR
        COSB=BB1/ASTR
C
        IF(GAMMASTR.LT.0.0)GAMMASTR=-GAMMASTR
        if(abs(sin(GAMMASTR)).gt.0.0001)then
          A=ISIZEX/(ASTR*SIN(GAMMASTR))
          B=ISIZEX/(BSTR*SIN(GAMMASTR))
        else
          A=ISIZEX/(ASTR*0.001)
          B=ISIZEX/(BSTR*0.001)
        endif
C
        A1=A*COSA
        A2=A*SINA
        B1=B*COSB
        B2=B*SINB
      ENDIF

      WRITE(6,8005)
8005  FORMAT('$REAL SPACE LATTICE PARAMETERS CALCULATED or READ IN',
     . ' A1,A2,B1,B2')
      WRITE(6,*)A1,A2,B1,B2
C     WRITE(6,*)ASTR,BSTR,ANGASTR,ANGBSTR,GAMMASTR,A,B
C
310   WRITE(6,9014)
9014  FORMAT('$NUMBER OF UNIT CELLS ALONG EACH AXIS TO',/,
     .   ' BE USED IN SEARCH, MINA,MAXA,MINB,MAXB? ')
      READ(5,*) MINA,MAXA,MINB,MAXB
      WRITE(6,*) MINA,MAXA,MINB,MAXB
      IF(MINB.LT.MNY.OR.MINB.GT.MXY)GO TO 9310
      IF(MAXB.LT.MNY.OR.MAXB.GT.MXY)GO TO 9310
      IF(MINA.LT.MNY.OR.MINA.GT.MXY)GO TO 9310
      IF(MAXA.LT.MNY.OR.MAXA.GT.MXY)GO TO 9310
      GO TO 9311
9310    WRITE(6,9312)
9312    FORMAT('::  NUMBER OF UNIT CELLS REQUIRED EXCEEDS DIMENSIONS')
      STOP
9311  CONTINUE
      MINX=0
      MINY=0
      MAXX=NCOL-1
      MAXY=NLINE-1
C     NC=0
C     NR=0
C
      NC=NXYZ(1)
      NR=NXYZ(2)
320   WRITE(6,9015)
9015  FORMAT('$HALF-WIDTH OF CCOR SEARCH KDC,KDR, IOFFX,IOFFY: ')
      if(IALIGN.eq.1)then
        READ(5,*) KDC,KDR,IOFFX,IOFFY  !HALF-WIDTH OF XCOR SEARCH
        WRITE(6,*) KDC,KDR,IOFFX,IOFFY
        if(IOFFX.gt.MXI)IOFFX=MXI
        if(IOFFY.gt.MXI)IOFFY=MXI
      else
        READ(5,*) KDC,KDR  !HALF-WIDTH OF XCOR SEARCH
        WRITE(6,*) KDC,KDR
        IOFFX=0
        IOFFY=0
      endif
C
      KDC1=KDC+1
      KDR1=KDR+1
      KDC21=2*KDC1+1
      KDR21=2*KDR1+1
      IF(KDC21.GT.MDC.OR.KDR21.GT.MDR)THEN
        imdc=((MDC-1)/2)-1
        imdr=((MDR-1)/2)-1
        WRITE(6,19016) imdc,imdr
19016   FORMAT(':: KDC,KDR TOO LARGE FOR CURRENT DIMENSIONS '
     .   ,2I4)
        STOP
      END IF
      WRITE(6,9016)
9016  FORMAT('$POSITION OF STARTING ORIGIN FOR SEARCH',/,
     . ' NORMALLY CENTRE OF CCOR MAP  IC,IR? ')
      READ(5,*) IC,IR
      WRITE(6,*) IC,IR
      WRITE(6,9035)
9035  FORMAT('$PRINTOUT OF ALL CCOR VALUES IN SEARCH? ')
      READ(5,9004)IPRNT
9004  FORMAT(A1)
C
CHENN>
C
      if(IALIGN.eq.1)then
C
C-------Calculate autocorrelation pattern from central area of CC map
C
        if ( IOFFX.ne.0 .or. IOFFY.ne.0 ) then
C
          DARRAYMIN= 1e10
          DARRAYMAX=-1e10
          do IRO = 1,NLINE
            do ICO = 1,NCOL
              INDEXO=(IRO-1)*NCOL+ICO
              if(DARRAYMIN.gt.ARRAY(INDEXO))DARRAYMIN=ARRAY(INDEXO)
              if(DARRAYMAX.lt.ARRAY(INDEXO))DARRAYMAX=ARRAY(INDEXO)
            enddo
          enddo
          write(6,'('' INPUT ARRAY MIN,MAX = '',2G15.3)')DARRAYMIN,DARRAYMAX
C
C---------Scale ARRAY between 0 and 10
          do IRO = 1,NLINE
            do ICO = 1,NCOL
              INDEXO=(IRO-1)*NCOL+ICO
              ARRAY(INDEXO)=(ARRAY(INDEXO)-DARRAYMIN)*10.0/(DARRAYMAX-DARRAYMIN)
            enddo
          enddo
          DARRAYMIN= 1e10
          DARRAYMAX=-1e10
          DOUBLMEAN=0.0
          do IRO = 1,NLINE
            do ICO = 1,NCOL
              INDEXO=(IRO-1)*NCOL+ICO
              if(DARRAYMIN.gt.ARRAY(INDEXO))DARRAYMIN=ARRAY(INDEXO)
              if(DARRAYMAX.lt.ARRAY(INDEXO))DARRAYMAX=ARRAY(INDEXO)
              DOUBLMEAN=DOUBLMEAN+ARRAY(INDEXO)
            enddo
          enddo
          DOUBLMEAN=DOUBLMEAN/NCOL
          DOUBLMEAN=DOUBLMEAN/NLINE
          DARRAYMEAN=DOUBLMEAN
C          write(6,'(''::INPUT ARRAY now MIN,MAX,MEAN = '',3G15.3)')
C     .      DARRAYMIN,DARRAYMAX,DARRAYMEAN
C
C---------Zero RFIELD
          do j = MNI,MXI
            do i = MNI,MXI
              RFIELD(i,j)=0.0
            enddo
          enddo
C
C---------Average the central 7x7 unit cells together, to have a high-contrast patch
C---------that can be used to determine the overall offset of the lattice. 
C
          do ih = -3,3
            do ik = -3,3
C
C-------------Calculate offset of sub-area to read
              IX = int( ih * A1 + ik * B1 )
              IY = int( ih * A2 + ik * B2 )
              write(6,'('' Offset for '',2I6,
     .          '' is '',2I6)')ih,ik,IX,IY
C
C-------------Add sub-areas into RFIELD
              do j = MNI,MXI
                do i = MNI,MXI
                  JC=(NCOL /2+1)+i+IX
                  JR=(NLINE/2+1)+j+IY
                  INDEXO=(JR-1)*NCOL+JC
                  if(INDEXO.lt.1 .or. INDEXO.gt.IARRMXSIZ) then
                    write(6,'(''::ERROR: no averaging possible with this'',
     .                '' dimension'',I16,6I8)')INDEXO,ih,ik,i,j,NCOL,NLINE
                    stop
                  endif
                  RFIELD(i,j)=RFIELD(i,j)+ARRAY(INDEXO)/100.0
                enddo
              enddo
C
            enddo
          enddo
C
C---------Write out accumulated RFIELD for verfication purposes
          DMIN= 1.0e10
          DMAX=-1.0e10
          do i = MNI,MXI
            do j = MNI,MXI
              ix=i-MNI+1
              iy=j-MNI+1
              if(DMIN.gt.RFIELD(ix,iy))DMIN=RFIELD(ix,iy)
              if(DMAX.lt.RFIELD(ix,iy))DMAX=RFIELD(ix,iy)
            enddo
          enddo
          if(abs(DMAX-DMIN).gt.0.001)then
            do i = MNI,MXI
              do j = MNI,MXI
                RFIELD(i,j)=(RFIELD(i,j)-DMIN)*255.0/(DMAX-DMIN)
                IFIEL1(i,j)=int(RFIELD(i,j))+2
                ix=i-MNI+1
                iy=j-MNI+1
                IFIEL2(ix,iy)=int(RFIELD(i,j))
              enddo
            enddo
          else
            do i = MNI,MXI
              do j = MNI,MXI
                RFIELD(i,j)=(RFIELD(i,j)-DMIN)*255.0/0.001
                IFIEL1(i,j)=int(RFIELD(i,j))+2
                ix=i-MNI+1
                iy=j-MNI+1
                IFIEL2(ix,iy)=int(RFIELD(i,j))
              enddo
            enddo
          endif
          IPICDIM=MXI-MNI+1
          write(TEXT(1:80),'(''Averaged central peaks'')')
          write(FILENAM(1:80),'(''TMP_quadserch3_autocor.mrc'')')
          CALL PICWRI(IFIEL2,FILENAM,TEXT,IPICDIM,IPICDIM)
C          write(6,'('': Written image, DMIN,DMAX = '',2G15.3)')
C     .      DMIN,DMAX
C
C---------Find peak in area of 0.95 * lattice unit cell
C
C---------Calculate radius of unit cell
C
          ialen=int(sqrt(A1*A1+A2*A2))
          iblen=int(sqrt(B1*B1+B2*B2))
          iradi=ialen
          if(iblen.gt.iradi)iradi=iblen
C
          iradi=iradi*0.95
C
          if(iradi.gt.IOFFX)iradi=IOFFX
          if(iradi.gt.IOFFY)iradi=IOFFY
C
          if(iradi.gt.MXI)then
            write(6,'(''ERROR: increase MNI,MXI '',
     .              ''in 2dx_quadserchk-3.for'')')
            stop
          endif
          RPEAK=-1e10
          ICMAX=0
          IRMAX=0
C---------Find peak that is closest to origin, except if other peak is at least 10% higher
          do IL = 0,iradi
            IL2=IL*IL
            do j = MNI,MXI
              do i = MNI,MXI
                ilen2=i*i+j*j
                if(ilen2.le.IL2 .and. IFIEL1(i,j).ne.0)then
                  IFIEL1(i,j)=0
C-----------------How far is this pixel from the last peak?
                  idist2=(ICMAX-i)**2+(IRMAX-j)**2
                  if(RFIELD(i,j).gt.RPEAK)then
C-------------------If this pixel is brighter than the previous peak,
                    if(idist2.lt.50 .or. RFIELD(i,j).gt.RPEAK*1.10)then
C---------------------and if this pixel is either close to the last peak, or distant but then 10% higher than the last peak:
                      RPEAK=RFIELD(i,j)
                      ICMAX=i
                      IRMAX=j
                    endif
                  endif
                endif
              enddo
            enddo
          enddo
C
C---------Ignore minimal offset (which might stem from digit errors)
          if(abs(ICMAX).le.1) ICMAX=0
          if(abs(IRMAX).le.1) IRMAX=0
C
          if(ICMAX.ne.0 .or. IRMAX.ne.0)then
            write(6,'(''::'',30X,
     .        ''Shifting image by '',2I6,'' pixels.'')')
     .        ICMAX,IRMAX
C
C-----------Shift Cross-Correlation Array by ICMAX,IRMAX pixels
            do IRN = 1,NLINE
              do ICN = 1,NCOL
                IRO = IRN + IRMAX
                ICO = ICN + ICMAX
                INDEXO=(IRO-1)*NCOL+ICO
                INDEXN=(IRN-1)*NCOL+ICN
                if(IRO.lt.1 .or. IRO.gt.NLINE .or. 
     .             ICO.lt.1 .or. ICO.gt.NCOL)then
                  ARRA2(INDEXN)=DARRAYMIN
                else
                  ARRA2(INDEXN)=ARRAY(INDEXO)
                endif
              enddo
            enddo
            do IRO = 1,NCOL
              do ICO = 1,NLINE
                INDEXO=(IRO-1)*NCOL+ICO
                ARRAY(INDEXO)=ARRA2(INDEXO)
              enddo
            enddo
C
C-----------Open input image
C
            call shorten(FILE1,k2)
            write(cline,'(''\cp -f '',A,'' tmp3.mrc'')')FILE1(1:k2)
            call shorten(cline,k3)
            call system(cline(1:k3))
C
            write(NAME,'(''tmp3.mrc'')')
            CALL IMOPEN(11,NAME,'RO')
C            CALL IMOPEN(11,FILE1,'RO')
            CALL IRDHDR(11,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
C            CALL IRTLAB(11,LABELS,NL)
C            CALL IRTEXT(11,EXTRA,1,29)
C            CALL IRTCEL(11,CELL)
            NX=NXYZ(1)
            NY=NXYZ(2)
            IF (MODE .GE. 3) then
              write(6,'(''::ERROR: Wrong usage of program.'')')
              STOP
            endif
C
C-----------Read entire input file
C
            DMIN =  1.E10
            DMAX = -1.E10
            DMEAN = 0.0
            DOUBLMEAN = 0.0
C
            do iy = 1,NY
              CALL IRDLIN(11,ALINE,*999)
              do ix = 1,NX
                INDEXO=(iy-1)*NX+ix
                VAL=ALINE(ix)
                ARRA2(INDEXO)=VAL
                IF (VAL .LT. DMIN) DMIN = VAL
                IF (VAL .GT. DMAX) DMAX = VAL
                DOUBLMEAN = DOUBLMEAN + VAL
              enddo
            enddo
            DMEAN = DOUBLMEAN/(NX*NY)
C            write(*,'('':: Dimensions of image are '',2I8)')
C     .         NX,NY
C            write(*,'('':: Min, Max, Mean of image file is '',3F12.3)')
C     .         DMIN,DMAX,DMEAN
C
C-----------Shift Image by ICMAX,IRMAX pixels
C
            do iyn = 1,NY
              do ixn = 1,NX
                iyo = iyn + IRMAX
                ixo = ixn + ICMAX
                if(ixo.lt.1 .or. ixo.gt.NX .or. 
     .             iyo.lt.1 .or. iyo.gt.NY)then
                  APIC2(ixn,iyn)=DMEAN
                else
                  INDEXO=(iyo-1)*NX+ixo
                  APIC2(ixn,iyn)=ARRA2(INDEXO)
                endif
              enddo
            enddo
C
C-----------Open output file for aligned image
C
            call shorten(FILE2,k2)
            write(cline,'(''\rm -f '',A)')FILE2(1:k2)
            call shorten(cline,k3)
            call system(cline(1:k3))
C
C            CALL IMOPEN(12,FILE2,'NEW')
            call system("\rm tmp2.mrc")
            write(NAME,'(''tmp2.mrc'')')
            CALL IMOPEN(12,NAME,'NEW')
            CALL ITRHDR(12,11)
C
C-----------Put title labels, new cell and extra information into header
C
            CALL ICRHDR(12,NXYZ,NXYZ,MODE,LABELS,NL)
C            CALL IALEXT(12,EXTRA,1,29)
C            CALL IALCEL(12,CELL)
C            CALL IALMOD(12,MODE)
            CALL IWRHDR(12,TITPLOT,1,DMIN,DMAX,DMEAN)
C
C-----------Write the file into the output file
C
            write(6,'('': Writing out aligned image '',A)')FILE2(1:k2)
C
            do iy = 1,NY
              do ix = 1,NX
                ALINE(ix)=APIC2(ix,iy)
              enddo
              CALL IWRLIN(12,ALINE)
            enddo
C
            CALL IWRHDR(12,TITPLOT,-1,DMIN,DMAX,DMEAN)
C
            CALL IMCLOSE(12)
            write(cline,'(''\cp -f tmp2.mrc '',A)')FILE2(1:k2)
            call shorten(cline,k)
            call system(cline(1:k))
            CALL IMCLOSE(11)
C
          endif
C
        endif
C
        if (IOFFX.eq.0 .and. IOFFY.eq.0) then
          ICMAX=0
          IRMAX=0
        endif
C
        if (ICMAX.eq.0 .and. IRMAX.eq.0) then
C
c---------Copy input to output file:
C
          call shorten(FILE1,k1)
          call shorten(FILE2,k2)
          write(6,'(/,/'' Moving image '',A,
     .      '' onto output image '',A,/,/)')
     .      FILE1(1:k1),FILE2(1:k2)
          write(cline,'(''\rm -f '',A)')FILE2(1:k2)
          call shorten(cline,k3)
          call system(cline(1:k3))
C
          write(cline,'(''\mv -f '',A,'' '',A)')
     .      FILE1(1:k1),FILE2(1:k2)
          call shorten(cline,k3)
          call system(cline(1:k3))
C
        endif
      endif
C
CHENN<
C
C-----READ PROFILE DATA SET
C
      write(6,'(''Reading PROFILE...'')')
      CALL IMOPEN(2,'PROFILE','RO')
      CALL IRDHDR(2,NXYZ2,MXYZ2,MODE,DMIN,DMAX,DMEAN)
      NXP=NXYZ2(1)
      NYP=NXYZ2(2)
      CALL IMPOSN(2,0,0)
      CALL IRDPAS(2,PROFILE,NXP,NYP,0,NXP-1,0,NYP-1,*9501)
      CALL IMCLOSE(2)
      write(6,'(''PROFILE read.'')')
C
C-----Normalise profile to reasonable numbers (max=100.0) - avoids overflow later.
      IF(PROFILE(51,51).GT.0.001) THEN
        FACTOR = 100.0/PROFILE(51,51)
        DO 95 I=1,101
          DO 95 J=1,101
95          PROFILE(I,J)=PROFILE(I,J)*FACTOR
        WRITE(6,90) FACTOR
      ELSE
        FACTOR = 1.0
        WRITE(6,90) FACTOR
      ENDIF
90    FORMAT(' PROFILE in steps of 10 mini-steps (0.5 pixels?), ',
     . '      NORMALISED by? --',E15.5)
      WRITE(6,91)((PROFILE(I,J),I=1,101,10),J=1,101,10)
91    FORMAT(11(2X,E10.4))
      GO TO 92
9501  STOP ':: ERROR READING PROFILE'
C
92    READ(5,*)RADLIMP,RADLIMQ,RADANGP
      RADLIMPSQ=RADLIMP**2
      RADLIMQSQ=RADLIMQ**2
      DO 93 J=-180,180
        ANGDIFFP=RADANGP-J
        RADSTORE(J)=(RADLIMP*COS(ANGDIFFP/57.295776))**2 +
     . (RADLIMQ*SIN(ANGDIFFP/57.295776))**2
93    CONTINUE
      WRITE(6,61)RADLIMP,RADLIMQ,RADANGP
61    FORMAT(/,/,' ELLIPSE RADIUS LIMITS AND ANGLE FOR PROFILE MATCH,'
     .  ,/,
     . ' IN PROFILE GRID UNITS, RADP,RADQ,ANGP',3F8.2)
C
CHEN--Read switch for SpotScan mode
      READ(5,*)IONELA,RMAG,CCOLOR
      if(CCOLOR.eq."y")then
        LCOLOR=.true.
      else
        LCOLOR=.false.
      endif
      if(IONELA.eq.1)then
        write(6,'(/,/,'' Allowing only one lattice for monocrystal '')')
      else 
        write(6,'(/,/,
     .    '' Allowing polycrystals, running classic mode '')')
      endif
      write(6,'(/,''RMAG = '',F6.1)')RMAG
C
CHEN--Read switch for output of manual Masking information
      READ(5,*)IMASINF
      if(IMASINF.eq.1)then
        write(6,'(/,/,
     1    '' Creating Information for Manual Masking'')')
      else
        write(6,'(/,/,'' no creation of manual masking info'')')
      endif
C
CHEN--Read switch for automatic masking based on XCF
      READ(5,*)IXCFPIC
      if(IXCFPIC.eq.1)then
        write(6,'(/,/,
     1    '' masking picture with XCF pattern'')')
      else
        write(6,'(/,/,'' no XCF pattern generation'')')
      endif
C
      WRITE(6,9052)
9052  FORMAT(/,/,' PARAMETERS USED IN SEARCHING FOR PROFILE MATCH',/)
      WRITE(6,9051)(I,NSTART(I),NFIN(I),NSTEP(I),I=1,10)
9051  FORMAT(' NPASS=',I3,' NSTART=',I3,' NFIN=',I3,' NSTEP=',I3,/)
C     
      write(6,'(''IPASS = '',I6)')IPASS
C-----OPEN FILE FOR ERRORS; WRITE IF IPASS=1; READ IF IPASS=2; 
      IF(IPASS.NE.0)then
        write(6,'(''Opening ERRORS...'')')
        CALL CCPDPN(4,'ERRORS','UNKNOWN','F',0,0)
        write(6,'(''ERRORS opened on channel 4.'')')
      endif
C-----OPEN FILE FOR ERRORS; WRITE IF IPASS=3;
      IF(IPASS.EQ.3)then
        write(6,'(''Opening ERROUT...'')')
        CALL CCPDPN(8,'ERROUT','UNKNOWN','F',0,0)
C        CALL CCPDPN(18,'ERROUTN','UNKNOWN','F',0,0)
      endif
C
      IF(IPASS.EQ.1)THEN
        write(6,'(''Writing TITLE into channel 4'')')
        WRITE(4,45000)(TITLE(J),J=1,10),MINA,MAXA,MINB,MAXB
      ENDIF
      IF(IPASS.EQ.3)THEN
        write(6,'(''Writing TITLE into channel 8'')')
        WRITE(8,45000)(TITLE(J),J=1,10),MINA,MAXA,MINB,MAXB
C        WRITE(18,45000)(TITLE(J),J=1,10),MINA,MAXA,MINB,MAXB
      ENDIF
C-----SET CONTENTS OF XERROR,YERROR ARRAYS TO 9999.; PEAK ARRAY TO 0.0
      IF (IPASS.EQ.0)THEN
        DO 45004 IA=MINA,MAXA
          DO 45004 IB=MINB,MAXB
            XERROR(IA,IB)=9999.
            YERROR(IA,IB)=9999.
            PEAK(IA,IB)=0.0
45004   CONTINUE
      END IF
C
C-----Read ERROR field in
C
      IF(IPASS.EQ.2 .OR. IPASS.EQ.3)THEN
        write(6,'(''Reading TITLE from channel 4'')')
        READ(4,45000)(TITLE(J),J=1,10),MINA1,MAXA1,MINB1,MAXB1
45000   FORMAT(10A4,4I5)
        WRITE(6,45002)(TITLE(J),J=1,10),MINA,MAXA,MINB,MAXB
45002   FORMAT(/,/,' SECOND PASS; ERROR FILE READ FROM UNIT 4;',
     . ' TITLE AND RANGES',/,20X,10A4,4I5,/)
C
        DO 45001 IA=MINA1,MAXA1
          DO 45001 IB=MINB1,MAXB1
            READ(4,*,ERR=45009)
     .       XERROR(IA,IB),YERROR(IA,IB),PEAK(IA,IB)
45001   CONTINUE
C
C-------New format of ERROR Field:
C        DO IA=MINA1,MAXA1
C          DO IB=MINB1,MAXB1
C            XERROR(IA,IB)=0.0
C            YERROR(IA,IB)-0.0
C            PEAK(IA,IB)=0.0
C          enddo
C        enddo
C
C45002   continue
C          READ(4,39031,END=45003,ERR=45009)IA,IB,RX,RY,RP
C          XERROR(IA,IB)=RX
C          YERROR(IA,IB)=RY
C          PEAK(IA,IB)=RP
C45003   continue
C
      END IF
      goto 45010
45009 continue
        print *,':: ERROR on file 4:',IA,IB
        STOP ':: ERROR'
45010 continue
C
C-----Write out unit cell coordinates in SPIDER format
C
      open(13,FILE="SPIDERCOORD.spi",STATUS="NEW",ERR=45020)
      goto 45021
45020 continue
      write(*,'(''::ERROR opening SPIDERCOORD.spi'')')
      stop
45021 continue
C
      write(13,'('' ;spi/spi   SPIDERCOORD.spi unit cell coords'')')
      ispidernum = 1
C
C-----WRITE OUT CCORDATA FOR USE IN CCUNBENDA
      CALL CCPDPN(3,'PROFDATA','UNKNOWN','F',0,0)
C
      WRITE(3,15000)(TITLE(J),J=1,10),KDC,KDR,RADLIMP,RADLIMQ,RADANGP
      WRITE(6,15001)
15001 FORMAT(' TITLE RECORDS WRITTEN TO CCORDATA OUTPUT FILE',/)
      WRITE(6,15000)(TITLE(J),J=1,10),KDC,KDR,RADLIMP,RADLIMQ,RADANGP
15000 FORMAT(' CROSS-CORRELATION FILE SEARCHED ',10A4,/
     . ,' HALF-WIDTH OF CCOR SEARCH KDC,KDR: ',2I5,/
     .  ,' RADII & ANGLE FOR',
     . ' PROFILE FIT IN PROFILE GRID UNITS ',3F7.2,/,/)
      WRITE(3,*)NC,NR,IC,IR,A1,A2,B1,B2,MINA,MAXA,MINB,MAXB
C
C-----SEARCH FOR CORRELATION PEAKS IN QUADRANTS. START
C-----AT CENTER OF SEARCH RANGE.  MODIFY LATTICE PARAMETERS
C-----AS SEARCH PROCEEDS TO ADJUST FOR SLOWLY CHANGING LATTICE.
C   
      CORMAX=-1E30
      CORMIN=-CORMAX
      CORAVG=0
      NCOOR=0
      NFOUNDT=0
      NOTUSED0=0
      NOTUSED2=0
      NDRIFT=0
      NOUT=0
      NJUMP=0
      XMXDSCRP=0.0
      YMXDSCRP=0.0
C
C-----WRITE DATA ON  X,Y CORRELATION PEAK HEIGHTS 
C-----FOR USE BY CCUNBEND TO UNDISTORT THE LATTICE.
C
      MXMNA=MAXA
      IF(MINA.LT.-MAXA)MXMNA=-MINA
      MXMNB=MAXB
      IF(MINB.LT.-MAXB)MXMNB=-MINB
C
C-----LA and LB determine the walk over the four quadrants
C
      DO 10021 LA=1,2
        IF(LA.EQ.1)IASGN=1
        IF(LA.EQ.2)IASGN=-1
C
        NASTOP(1)=0
        NASTOP(2)=0
C-------NASTOP(IBSGN) IS SET TO 1 ONCE A HALF-LINE IS REACHED ON WHICH ALL
C-------PREDICTED PEAKS ARE OUTSIDE IMAGE AREA
C
        DO 21 IIA=0,MXMNA
          IF(NASTOP(1).EQ.1.AND.NASTOP(2).EQ.1)THEN
            NJUMP=NJUMP+2
            GO TO 21
          END IF
          IF(LA.EQ.2.AND.IIA.EQ.0)GO TO 21
          IF(LA.EQ.1)IA=IIA
          IF(LA.EQ.2)IA=-IIA
          IF(IA.LT.MINA.OR.IA.GT.MAXA)GO TO 21
C
          DO 10022 LB=1,2
            IF(NASTOP(LB).EQ.1)THEN
              NJUMP=NJUMP+1
              GO TO 10022
            END IF
            IF(LB.EQ.1)IBSGN=1
            IF(LB.EQ.2)IBSGN=-1
            NFOUND=0
            NALINE=0
            XPRDI=0.0
            YPRDI=0.0
            XPRDL=0.0
            YPRDL=0.0      
            XPRDM=0.0
            YPRDM=0.0
            NPREDCT=0
            TOTXPREDCT=0.0
            TOTYPREDCT=0.0
C
C---------- IBSTART GETS SET TO THE FIRST IB VALUE IN EACH HALF LINE WHERE 
C---------- PREDICTION IS POSSIBLE, JUST FOR DIAGNOSTIC PRINTOUT
            IBSTART1=0
            IF(MINB.GT.0)IBSTART1=MINB
            IBSTART2=-1
            IF(MAXB.LT.0)IBSTART2=MAXB
            NBSTART=0
C            
            DO 22 IIB=0,MXMNB
C
              IF(LB.EQ.2.AND.IIB.EQ.0)GO TO 22
              IF(LB.EQ.1)IB=IIB
              IF(LB.EQ.2)IB=-IIB
              IF(IB.LT.MINB.OR.IB.GT.MAXB)GO TO 22
C
C-----------------------------------------------------
C  CALC EXPECTED CORREL LOCATION WITH ERROR ADJUSTMENT
C-----------------------------------------------------
C
              CALL PREDICT(NRANGE,IASGN,IBSGN,IA,IB,XPREDICT,
     1          YPREDICT,MINA,MAXA,MINB,MAXB,XERROR,YERROR,PEAK,
     2          NAVG,IPASS)
C
              IF(NAVG.NE.0)THEN
C-------------PREDICTION MADE
                NBSTART=1
                NPREDCT=NPREDCT+1
                TOTXPREDCT=TOTXPREDCT+XPREDICT
                TOTYPREDCT=TOTYPREDCT+YPREDICT
C---------------STORE PREDICTION; 
C---------------LAST ONE MADE FOR EACH HALF-LINE WILL APPEAR ON PRINTOUT
                XPRDL=XPREDICT
                YPRDL=YPREDICT
              ELSE
C---------------NO PREDICTION MADE
                IF(NBSTART.EQ.0.AND.LB.EQ.1)IBSTART1=IB+1
                IF(NBSTART.EQ.0.AND.LB.EQ.2)IBSTART2=IB-1
              END IF
C
              IF(LB.EQ.1.AND.IB.EQ.IBSTART1)THEN
C---------------STORE INITIAL PREDICTION FOR HALF-LINE FOR DIAGNOSTIC PRINTOUT
                XPRDI=XPREDICT
                YPRDI=YPREDICT
              END IF
              IF(LB.EQ.2.AND.IB.EQ.IBSTART2)THEN
                XPRDI=XPREDICT
                YPRDI=YPREDICT
              END IF
C
C-------------XCOORD(IA,IB): Lattice position in image of knot (IA,IB)
C-------------XPREDICT: Predicted deviation of lattice position, from
C-------------          weighted accumulation of last KDC knot errors. (SUBROUTINE PREDICT)
C
              XXX=XCOORD(IA,IB)+XPREDICT
              YYY=YCOORD(IA,IB)+YPREDICT
C             WRITE(6,'('' IA,IB,XXX,YYY,XPREDICT,YPREDICT'',2I,4F)') 
C    1               IA,IB,XXX,YYY,XPREDICT,YPREDICT
              IX=XXX
              IY=YYY
C
C             WRITE(6,'(''KDC1,KDR1,MINX,MINY,MAXX,MAXY'',6I)')
C     1                   KDC1,KDR1,MINX,MINY,MAXX,MAXY
C
CHEN----------This is to exclude knots from outside of the image:
C-------------KDC : Search over KDC pixels on each side of the predicted position
C-------------IX  : Found position ???
C-------------MINX: PIXTURE LIMITS  e.g. 0 
C-------------MAXX: PIXTURE LIMITS  e.g. 4096
C
              IF(IX-KDC1.LT.MINX.OR.IX+KDC1.GT.MAXX.OR.
     .           IY-KDR1.LT.MINY.OR.IY+KDR1.GT.MAXY)GO TO 23
C
C-------------COUNT NUMBER OF BOXES READ ON EACH HALF LINE; 
              NALINE=NALINE+1
C
C-------------HERE, FIND CENTRE OF GRAVITY OF CORRELATION PEAK IN A SMALL SEARCH AREA.
C-------------PREVIOUS CORNELL PROGRAM USED SUBROUTINE BLCK, WHICH DID HIGHEST PEAK ONLY.
C
              CALL GETXC(ARRAY,NCOL,NLINE,XC,IX,IY,KDC1,KDR1)
C             CALL IMPOSN(1,0,0)        ! replaced by reading in entire array.
C             CALL IRDPAS(1,XC,MDR,MDC,IX-KDC1,IX+KDC1,IY-KDR1,IY+KDR1,*9500)
              BIG5=-1.0E30
CTSH          IF(IPRNT.EQ.'Y') THEN
CTSH++
              IF(IPRNT.EQ.ICHAR('Y')) THEN
CTSH--
                WRITE(6,*)J,I
                DO 9037 IIY=1,KDR21
9037              WRITE(6,9036) (XC(IIX,IIY),IIX=1,KDC21)
9036              FORMAT(16F5.1)
              ENDIF
C
C-------------FIRST FIND HIGHEST PEAK, 
C-------------EACH POINT IS REPLACED BY THE SUM OF THE DENSITY AT THE POINT
C-------------AND THE DENSITIES OF THE 4 NEAR NEIGHBOURS
              DO 9031 IIX=-KDC,KDC
                DO 9031 IIY=-KDR,KDR
                 IXST =IIX+KDC+2
                 IYST =IIY+KDR+2
                 XC5=XC(IXST  ,IYST  )
     .              +XC(IXST+1,IYST  )
     .              +XC(IXST-1,IYST  )
     .              +XC(IXST  ,IYST+1)
     .              +XC(IXST  ,IYST-1)
                 IF(XC5.LE.BIG5) GO TO 9031
                   BIG5=XC5
                   BIG=XC(IXST,IYST)   
                   IGC=IX+IIX
                   IGR=IY+IIY
9031             CONTINUE
C              WRITE(6,20001)IGC,IGR,BIG        ! diagnostic
20001          FORMAT(' HIGHEST PEAK X,Y, BIG',2I10,G16.6)
C
CHEN-----------IGC,IGR: Center of gravity of found actual peak position
C--------------BIG: Cross-correlation peak height of that position
C
               IIX=IGC-IX
               IIY=IGR-IY
               JFLAG=0
               IDRIFT=0
C              WRITE(6,20000)IX,IY,IIX,IIY
20000          FORMAT(' CALL PROFIT',4I10)
               NCALLPROF=NCALLPROF+1
               CALL PROFIT(RADSTORE,RADLIMPSQ,RADLIMQSQ,
     1           IIX,IIY,KDC,KDR,KDC21,KDR21,
     2           CGXADJST,CGYADJST,CCFBEST,BIGFACTR,NCOUNT,JFLAG,IDRIFT,
     3           IONELA)
C
              IF(IDRIFT.EQ.1)NDRIFT=NDRIFT+1
              IF(IDRIFT.EQ.1)WRITE(6,20011) BIGFACTR,IX,IY,IIX,IIY
20011         FORMAT(' DRIFTED PEAK - BIGFACTR,X,Y,dX,dY',G16.6,4I8)
              IF(JFLAG.EQ.1)WRITE(6,20010)IX,IY,IIX,IIY
20010         FORMAT(' ? PROFILE FIT, IX,IY,IIX,IIY',4I10)
C
              GC=IX+IIX+CGXADJST
              GR=IY+IIY+CGYADJST
              BIG=BIG*BIGFACTR
C              WRITE(6,20002)GC,GR,BIG
C20002         FORMAT(' PEAK AT X,Y, BIG ',2G16.6,10X,G16.6)
C
CHEN
C-------------GC,GR   : Actual center of gravity of peak position
C-------------BIG     : Peak height
C-------------XXX     : XXX=XCOORD(IA,IB)+XPREDICT
C-------------XPREDICT: Predicted deviation of lattice position, from
C-------------          weighted accumulation of last KDC knot errors. (SUBROUTINE PREDICT)
C
              XERROR(IA,IB)=GC-(XXX-XPREDICT)
              YERROR(IA,IB)=GR-(YYY-YPREDICT)
C
              PEAK(IA,IB)=BIG
C
CHEN----------XERROR(IA,IB): Found position after profile-fitting 
C-------------               - (actual position before profile fitting - predicted) 
C-------------               of knot (IA,IB)
C-------------PEAK(IA,IB)  : Peak height
C
              IF(BIG.LT.CORMIN) CORMIN=BIG
              IF(BIG.GT.CORMAX) CORMAX=BIG
C              WRITE(6,'('' GC,IX,RDC,GR,IY,KDR,BIG'',
C     .          F12.6,I8,2F12.6,2I8,F16.6)')
C     1                     GC,IX,RDC,GR,IY,KDR,BIG
              CORAVG=CORAVG+BIG
              NCOOR=NCOOR+1
CHEN
C-------------COOR(1...3,IB,IA): Pos XY of real peak, Peak height.
C
              COOR(1,IB,IA)=GC
              COOR(2,IB,IA)=GR
              COOR(3,IB,IA)=CORFAC*BIG
C
              IUSE=1
C
C-------------DO NOT USE POINT IF IT IS AT EDGE OF SEARCH AREA(* MEANS IT IS TO BE USED).
              IF(INT(ABS(GC-IX)).GE.KDC.OR.INT(ABS(GR-IY)).GE.KDR)THEN
C                WRITE(6,'('': Setting IUSE to 2:  '',
C     .            ''GC,IX,KDC, GR,IY,KDR, BIG'',
C     .            F12.6,2I8,'' /// '', F12.6,2I8,'' /// '',F16.6)')
C     1                     GC,IX,KDC,GR,IY,KDR,BIG
                IUSE=2
              ENDIF
C
C-------------DO NOT USE IF BIGFACTR=0.0
              IF(BIGFACTR.EQ.0.0)IUSE=0
C
              IF(IUSE.EQ.0 .AND. IONELA.eq.1)THEN
C---------------SpotScan Mode. Extrapolate XERROR from neighbours.
C
                CALL ERREXT(XERROR,YERROR,PEAK,IA,IB)
C
C               WRITE(6,'('' Extpl. '',2I,'' XYERROR,PEAK: '',3F15.1)')
C    1           IA,IB,XERROR(IA,IB),YERROR(IA,IB),PEAK(IA,IB)
C
                COOR(1,IB,IA)=XERROR(IA,IB)+XCOORD(IA,IB)
                COOR(2,IB,IA)=YERROR(IA,IB)+YCOORD(IA,IB)
                COOR(3,IB,IA)=PEAK(IA,IB)
C                WRITE(6,'('' XCOR,YCOR,XERR,YERR,PEAK'',15X,5F10.2)')
C     1            XCOORD(IA,IB),YCOORD(IA,IB),
C     2            XERROR(IA,IB),YERROR(IA,IB),PEAK(IA,IB)
              ENDIF
C
              IF((IUSE.EQ.2) .OR.
     1           (IUSE.EQ.0 .AND. IONELA.eq.0))THEN
                if(IUSE.eq.0)NOTUSED0=NOTUSED0+1
                if(IUSE.eq.2)NOTUSED2=NOTUSED2+1
                COOR(1,IB,IA)=0
                COOR(2,IB,IA)=0
                COOR(3,IB,IA)=0
                XERROR(IA,IB)=9999.
                YERROR(IA,IB)=9999.
                PEAK(IA,IB)=0.
C                IF(BIG.EQ.0.0)WRITE(6,30003)IA,IB,IUSE,NOTUSED0
C30003           FORMAT(' Peak not found; C=0.0; PROBABLY DRIFTED ',
C     .            2I5,' IUSE=',2I8)
C                IF(BIG.NE.0.0)WRITE(6,30013)IA,IB,IUSE,NOTUSED2
C30013           FORMAT(' Peak not found; AT EDGE OF SEARCH BOX   ',
C     .            2I5,' IUSE=',2I8)
              ENDIF
C
              IF(IUSE.EQ.1)THEN
C---------------COUNT NUMBER OF PEAKS FOUND ON EACH HALF-LINE
                NFOUND=NFOUND+1
                XDISCREP=XERROR(IA,IB)-XPREDICT
                YDISCREP=YERROR(IA,IB)-YPREDICT
C---------------STORE MAXIMUM DISCREPANCY FOUND FOR DIAGNOSTIC OUTPUT
                XDSC=ABS(XDISCREP)
                YDSC=ABS(YDISCREP)
                IF(XDSC.GT.XMXDSCRP)XMXDSCRP=XDSC
                IF(YDSC.GT.YMXDSCRP)YMXDSCRP=YDSC
C               WRITE(6,'('' NORMAL '',2I,'' XYERROR,PEAK: '',3F15.1)')
C    1           IA,IB,XERROR(IA,IB),YERROR(IA,IB),PEAK(IA,IB)
              END IF
C
C-------------WRITE(6,'(''---- Used ------->  '',2I,''  <--'')')IA,IB
C
              GO TO 22
C
C-------------COUNT NUMBER OF EXPECTED PEAKS THAT FALL OUTSIDE IMAGE AREA AFTER
C-------------PREDICTED CORRECTION IS APPLIED
23            NOUT=NOUT+1
              COOR(1,IB,IA)=0
              COOR(2,IB,IA)=0
              COOR(3,IB,IA)=0
C
              XERROR(IA,IB)=9999.
              YERROR(IA,IB)=9999.
              PEAK(IA,IB)=0.
C
C-------------WRITE(6,'(''---- Deleted ---->  '',2I)')IA,IB
C
22          CONTINUE
C
            IF (NALINE.EQ.0)THEN
              NASTOP(LB)=1
C             WRITE(6,*)NALINE,LB,NASTOP(LB)
              GO TO 9922
            END IF
C-----------ONE HALF-LINE COMPLETED
            IF(NPREDCT.EQ.0)THEN
              XPRDM=0.0
              YPRDM=0.0
            ELSE
              XPRDM=TOTXPREDCT/NPREDCT
              YPRDM=TOTYPREDCT/NPREDCT
            END IF
C            WRITE(6,9021)IA,IBSGN,NFOUND,NALINE,XPRDI,XPRDL,XPRDM,
C     .        YPRDI,YPRDL,YPRDM,NPREDCT
9021        FORMAT(' *LINE IA,IBSGN',I5,I3,'; # PEAKS FOUND=',2I5,
     . '; PREDICTIONS(I,L,M:X,Y:N)',6F8.2,I4)
C-----------SUM FOR TOTAL NUMBER OF PEAKS FOUND
            NFOUNDT=NFOUNDT+NFOUND
C-----------IF NO CALLS TO READ BOX ON THE HALF-LINE JUST DONE THEN THERE
C-----------IS NO NEED TO CONSIDER FURTHER VALUES IF IA FOR THIS IBSGN
C
9922        CONTINUE
10022     CONTINUE
21      CONTINUE
10021 CONTINUE
C
C
      DENMAX=CORMAX*CORFAC
      if(abs(DENMAX).lt.0.001)then
        DENMAX=0.001
      endif
      WRITE(3,*)DENMAX
      DO 40 K=MINA,MAXA
        DO 40 J=MINB,MAXB
C         WRITE(6,9030) (COOR(I,J,K),I=1,3)
C          WRITE(3,9030) (COOR(I,J,K),I=1,3)
          if(COOR(3,J,K).ne.0.0)then
            WRITE(3,'(2I8,3G16.6)') K,J,(COOR(I,J,K),I=1,3)
            write(13,'(I5,'' 3'',F13.2,F13.2,G13.4)')
     .        ispidernum,COOR(1,J,K),COOR(2,J,K),COOR(3,J,K)/DENMAX
            ispidernum = ispidernum + 1
C            IF(IPASS.EQ.3)THEN
C              WRITE(18,39032)K,J,XERROR(K,J),YERROR(K,J),PEAK(K,J)
C            endif
          endif
          IF(IPASS.EQ.1)THEN
            WRITE(4,39031)XERROR(K,J),YERROR(K,J),PEAK(K,J)
          ENDIF
          IF(IPASS.EQ.3)THEN
            WRITE(8,39031)XERROR(K,J),YERROR(K,J),PEAK(K,J)
          ENDIF
40    CONTINUE
      close(3)
      close(13)
39031 FORMAT(3G15.5)      
39032 FORMAT(2I6,3G15.5)      
9030  FORMAT(2G16.6,G16.6)  ! increase PEAK size possibility
      WRITE(6,39020)NJUMP
39020 FORMAT(' NUMBER OF HALF-LINES COMPLETELY OUTSIDE IMAGE .......',
     .  '....:',I10)
      WRITE(6,39019)NOUT
39019 FORMAT(' NUMBER OR PEAKS OUTSIDE EDGE OF IMAGE ...............',
     .  '....:',I10)
      WRITE(6,39030)NCALLPROF
39030 FORMAT(' NUMBER OF CALLS TO PROFIT SUBROUTINE ................',
     .  '....:',I10)
      WRITE(6,29019)NFOUNDT
29019 FORMAT(': TOTAL NUMBER OF CORRELATION PEAKS FOUND .............',
     .  '....:',I10,/)
      WRITE(6,29020)NOTUSED0
29020 FORMAT(': TOTAL NUMBER OF CORRELATION PEAKS DRIFTED ...........',
     .  '....:',I10,'  (Data lost or no crystal here)',/)
      WRITE(6,29021)NOTUSED2
29021 FORMAT(': TOTAL NUMBER OF CORRELATION PEAKS AT EDGE OF BOX ....',
     .  '....:',I10,'  (Data lost or no crystal here)',/)
      WRITE(6,29022)NDRIFT
29022 FORMAT(' NUMBER OF PEAKS THAT DRIFTED FROM ORIGINAL BEST POSIT',
     .  'ION :',I10,/)
      WRITE(6,9019) CORMIN,CORMAX,CORAVG/NCOOR,NCOOR
9019  FORMAT(' CORMIN,CORMAX,CORAVG ',3G16.6,' NCOOR',I10)
      WRITE(6,9020) CORMIN*CORFAC,CORMAX*CORFAC,CORAVG/NCOOR*CORFAC
9020  FORMAT(' THE SAME SCALED =    ',3G16.6)
      WRITE(6,39021)XMXDSCRP,YMXDSCRP
39021 FORMAT(' MAXIMUM DIFFERENCE FOUND BETWEEN ACTUAL AND PREDICTED',
     . ' POSITION; IN X AND Y',2F10.3)
C
      WRITE(6,9050)(NCOUNT(I),I=1,11)
9050  FORMAT(' NUMBER OF PEAKS NEEDING N PASSES TO REACH FIT',/,
     . ' NPASS=1',I5,', NPASS=2',I6,', NPASS=3',I6,', NPASS=4',I6,
     . ', NPASS=5',I5,', NPASS=6',I5,/,
     . ' NPASS=7',I5,', NPASS=8',I5,
     . ', NPASS=9',I5,', NPASS=10',I5,', NPASS=11',I5)
C
       write(*,'('' Entering PLOTLATT'')')
C
       CALL PLOTLATT(COOR,TITPLOT,NXYZ,MINA,MAXA,
     . MINB,MAXB,A1,A2,B1,B2,IC,IR,NC,NR,IPASS,RMAG,LCOLOR)
C
CHEN----Subroutine for image masking based on Cross-Correlation-Field COOR
C
        if(IXCFPIC.ne.0)then
          write(*,'('' Entering MASKLATT'')')
          CALL MASKLATT(COOR,TITPLOT,NXYZ,MINA,MAXA,
     1      MINB,MAXB,A1,A2,B1,B2,IC,IR,NC,NR,IPASS)
        else
          write(*,'('' Not entering MASKLATT'')')
        endif
C
CHEN----Subroutine for creation of manual masking info
C
        if(IMASINF.ne.0)then
          write(*,'('' Entering MASKINF'')')
          CALL MASKINF(COOR,TITPLOT,NXYZ,MINA,MAXA,
     1      MINB,MAXB,A1,A2,B1,B2,IC,IR,NC,NR,IPASS)
        else
          write(*,'('' Not entering MASKINF'')')
        endif
C
       STOP
9400   WRITE(6,9013)
9013   FORMAT(':: Error reading in the entire map at beginning')
       STOP
C
999    STOP 'END-OF-FILE ERROR ON READING input image'
C
       END
C
C******************************************************************************
C  SUBROUTINE TO PLOT THE LATTICE OF SEARCH STARTING AND FOUND POSITIONS.
C             ALSO THE ROUGH SIZES OF THE CORRELATION PEAKS (SYMBOLICALLY).
      SUBROUTINE PLOTLATT(COOR,TITLE,NXYZ,MINA,MAXA,
     . MINB,MAXB,A1,A2,B1,B2,IC,IR,NC,NR,IPASS,RMAG,LCOLOR)
      PARAMETER (MNY=-240)
      PARAMETER (MXY=240)
      DIMENSION COOR(3,MNY:MXY,MNY:MXY),NXYZ(3)
CTSH    DIMENSION TEXT(20),TITLE(20),TITLEPLOT(20),PROGTIT(4)
CTSH++
        LOGICAL LCOLOR
        DIMENSION TITLE(20),PROGTIT(4)
        CHARACTER*80 TEXT
        CHARACTER*80 TITLEPLOT
CTSH--
        CHARACTER DAT*24
CTSH    DATA PROGTIT/'[QUA','DSER','CHK]','    '/
CTSH++
        CHARACTER*16 TMPPROGTIT
        EQUIVALENCE (TMPPROGTIT,PROGTIT)
        DATA TMPPROGTIT/'  [QUADSERCHK]  '/
CTSH--
CHEN>
        DIMENSION line(20)
        CHARACTER*80 TMPline
        EQUIVALENCE (TMPline,line)
CHEN<
        XCOORD(J,I)=A1*J+B1*I+IC+1.0
        YCOORD(J,I)=A2*J+B2*I+IR+1.0
C
CHEN>
C-------Magnification (Exaggeration) factor 10 times:
C        RMAG = 10.0
C
        WRITE(TMPline,'('' Vectors are plotted '',F4.1,'' times '',
     .    ''elongated.'')') RMAG
CHEN<
C
      ZERO=0.0
      PEAKMAX=-1.0E30
      XPEAKMAX=-100.
      YPEAKMAX=-100.
      CALL FDATE(DAT)
      WRITE(6,1502) DAT(5:24)
1502  FORMAT('  Date from fdate ----  ',A20)
      WRITE(TITLEPLOT,1501)
     . (TITLE(J),J=1,15),DAT(5:24)
1501  FORMAT(15A4,A20)
200   FORMAT('  ENTERING PLOTLATT')
103   FORMAT(' TITLE FOR PLOT  ',A)
      WRITE(6,200)
      WRITE(6,103) TITLEPLOT
      PLTSIZ=260.0
      FONTSIZE=3.6
      CALL P2K_OUTFILE('CCPLOT.PS',9)
      CALL P2K_HOME
      CALL P2K_FONT("Courier"//CHAR(0),FONTSIZE)
      CALL P2K_GRID(0.5*PLTSIZ,0.5*PLTSIZ,1.0)
      CALL P2K_ORIGIN(-0.5*PLTSIZ,-0.7*PLTSIZ,0.)
      CALL P2K_COLOUR(0)
      SPLOT=PLTSIZ/NXYZ(1)
      SIZEX=SPLOT*NXYZ(1)
      SIZEY=SPLOT*NXYZ(2)
C  BOX ROUND THE WHOLE IMAGE AREA
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(SIZEX,0.,0.)
      CALL P2K_DRAW(SIZEX,SIZEY,0.)
      CALL P2K_DRAW(0.,SIZEY,0.)
      CALL P2K_DRAW(0.,0.,0.)
C NOW PLOT THE POINTS
      CALL P2K_FONT("Courier"//CHAR(0),FONTSIZE*0.5)
CHEN>
      rlenmax=0.0
      DO 99 K=MINA,MAXA
        DO 99 J=MINB,MAXB
          XPLOT=SPLOT*XCOORD(K,J)
          YPLOT=SPLOT*YCOORD(K,J)
          IF(XPLOT.GT.SIZEX.OR.XPLOT.LT.0.) GO TO 99
          IF(YPLOT.GT.SIZEY.OR.YPLOT.LT.0.) GO TO 99
          X=COOR(1,J,K)
          Y=COOR(2,J,K)
          IF(X.EQ.0.) GO TO 99
          XPLOTC=SPLOT*X
          YPLOTC=SPLOT*Y
          XERR=RMAG*(XPLOTC-XPLOT)+XPLOT
          YERR=RMAG*(YPLOTC-YPLOT)+YPLOT
          IF(XERR.GT.SIZEX.OR.XERR.LT.0.) GO TO 99
          IF(YERR.GT.SIZEY.OR.YERR.LT.0.) GO TO 99
          rlen=sqrt((XERR-XPLOT)**2+(YERR-YPLOT)**2)
          if(rlen.gt.rlenmax)rlenmax=rlen
99    CONTINUE
C-----Adjust maximal length to width/20
      rlenmax=rlenmax/20.0
      if(rlenmax.lt.0.001)rlenmax=0.001
C
      PEAKMAX=-1.0E30
      XPEAKMAX=-100.
      YPEAKMAX=-100.
CHEN<
      DO 100 K=MINA,MAXA
        DO 100 J=MINB,MAXB
          XPLOT=SPLOT*XCOORD(K,J)
          YPLOT=SPLOT*YCOORD(K,J)
          IF(XPLOT.GT.SIZEX.OR.XPLOT.LT.0.) GO TO 100
          IF(YPLOT.GT.SIZEY.OR.YPLOT.LT.0.) GO TO 100
C         CALL P2K_MOVE(XPLOT,YPLOT,0.)
C         WRITE(TEXT,102)
C         CALL P2K_CSTRING(TEXT,1,0.)
C102      FORMAT('X')
          X=COOR(1,J,K)
          Y=COOR(2,J,K)
C          WRITE(6,20000)XCOORD(K,J),YCOORD(K,J),X,Y
20000     FORMAT(' EXPCTD, ACTUAL',2F10.1,10X,2F10.1)
          IF(COOR(3,J,K).GT.PEAKMAX)THEN
            PEAKMAX=COOR(3,J,K)
            XPEAKMAX=X
            YPEAKMAX=Y
          END IF
C         PEAKMAX=MAX(PEAKMAX,COOR(3,J,K))
          IF(X.EQ.0.) GO TO 100
          XPLOTC=SPLOT*X
          YPLOTC=SPLOT*Y
C         XERR=10*XPLOTC-9*XPLOT
C         YERR=10*YPLOTC-9*YPLOT
          XERR=RMAG*(XPLOTC-XPLOT)+XPLOT
          YERR=RMAG*(YPLOTC-YPLOT)+YPLOT
C deviations from perfect lattice plotted at 10x actual deviation
C         CALL LOCCHR(XPLOTC,YPLOTC,0)
C         WRITE(TEXT,101)
C         CALL CSTRING(TEXT,1)
C101      FORMAT('O')
          CALL P2K_MOVE(XPLOT,YPLOT,0.)
          IF(XERR.GT.SIZEX.OR.XERR.LT.0.) GO TO 100
          IF(YERR.GT.SIZEY.OR.YERR.LT.0.) GO TO 100
CHEN> 
C---------Calculate angle and length of line:
          rang=atan2(XERR-XPLOT,YERR-YPLOT)*180.0/3.141592654
          if(rang.lt.0.0)rang=rang+360.0
          rlen=sqrt((XERR-XPLOT)**2+(YERR-YPLOT)**2)/rlenmax
          rlen=(rlen*0.5)+0.5
          if(rlen.gt.1.0)rlen=1.0
          if(rlen.lt.0.5)rlen=0.5
C---------Assign HSV Values, as Hue=angle, Saturation=length, Value=1.0
          rhue=rang
          rsat=rlen
          rval=0.7
C---------Calculate RGB Values:
          call HSVTORGB(rhue,rsat,rval,rgbr,rgbg,rgbb)
C---------Set RGB color:
          if(LCOLOR)CALL P2K_RGB_COLOUR(rgbr,rgbg,rgbb)
          CALL P2K_DRAW(XERR,YERR,0.)
100   CONTINUE
      CALL P2K_COLOUR(0)
CHEN<
      IF(abs(PEAKMAX).lt.0.001)PEAKMAX=0.001
      WRITE(*,9000)XPEAKMAX,YPEAKMAX
9000  FORMAT(/,/,' POSITION OF MAXIMUM PEAK HEIGHT',2F10.2,/)
C
      CALL P2K_FONT("Courier"//CHAR(0),FONTSIZE)
CHEN>
      YPOS=SIZEY+15.0
      CALL P2K_MOVE(10.,YPOS,0.)
      CALL P2K_STRING(TITLEPLOT,80,0.)
      YPOS=SIZEY+5.0
      CALL P2K_MOVE(10.,YPOS,0.)
      CALL P2K_STRING(line,80,0.)
CHEN<
      CALL P2K_PAGE
C
C END OF FIRST PLOT OF DEVIATION VECTORS.
C NOW PLOT CORRELATION PEAK HEIGHTS SYMBOLICALLY.
C
C  BOX ROUND THE WHOLE IMAGE AREA
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(SIZEX,0.,0.)
      CALL P2K_DRAW(SIZEX,SIZEY,0.)
      CALL P2K_DRAW(0.,SIZEY,0.)
      CALL P2K_DRAW(0.,0.,0.)
C NOW PLOT THE POINTS
      CALL P2K_FONT("Courier"//CHAR(0),FONTSIZE*0.5)
C
      DO 300 K=MINA,MAXA
        DO 300 J=MINB,MAXB
          X=COOR(1,J,K)
          Y=COOR(2,J,K)
          IF(X.EQ.0.) GO TO 300
          XPLOTC=SPLOT*X
          YPLOTC=SPLOT*Y
          IF(XPLOTC.GT.SIZEX.OR.XPLOTC.LT.0.) GO TO 300
          IF(YPLOTC.GT.SIZEY.OR.YPLOTC.LT.0.) GO TO 300
          PEAKNORM=COOR(3,J,K)/PEAKMAX
          CALL P2K_MOVE(XPLOTC,YPLOTC,0.)
          IF(PEAKNORM.LE.0.0)GO TO 300
CTSH      IF(PEAKNORM.GT.0.0) WRITE(TEXT,402)
CTSH      IF(PEAKNORM.GT.0.1) WRITE(TEXT,403)
CTSH      IF(PEAKNORM.GT.0.2) WRITE(TEXT,404)
CTSH      IF(PEAKNORM.GT.0.3) WRITE(TEXT,405)
CTSH      IF(PEAKNORM.GT.0.5) WRITE(TEXT,406)
CTSH      IF(PEAKNORM.GT.0.7) WRITE(TEXT,408)
CTSH++
          IF(PEAKNORM.GT.0.0) TEXT='.'
          IF(PEAKNORM.GT.0.1) TEXT=':'
          IF(PEAKNORM.GT.0.2) TEXT='-'
          IF(PEAKNORM.GT.0.3) TEXT='+'
          IF(PEAKNORM.GT.0.5) TEXT='<'
          IF(PEAKNORM.GT.0.7) TEXT='H'
CTSH--
CHEN> 
          XPLOT=SPLOT*XCOORD(K,J)
          YPLOT=SPLOT*YCOORD(K,J)
          XERR=RMAG*(XPLOTC-XPLOT)+XPLOT
          YERR=RMAG*(YPLOTC-YPLOT)+YPLOT
C---------Calculate angle and length of line:
          rang=atan2(XERR-XPLOT,YERR-YPLOT)*180.0/3.141592654
          if(rang.lt.0.0)rang=rang+360.0
          rlen=sqrt((XERR-XPLOT)**2+(YERR-YPLOT)**2)/rlenmax
          rlen=(rlen*0.5)+0.5
          if(rlen.gt.1.0)rlen=1.0
          if(rlen.lt.0.5)rlen=0.5
C---------Assign HSV Values, as Hue=angle, Saturation=length, Value=1.0
          rhue=rang
          rsat=rlen
          rval=0.7
C---------Calculate RGB Values:
          call HSVTORGB(rhue,rsat,rval,rgbr,rgbg,rgbb)
C---------Set RGB color:
C=========Color in this plot isn't too helpful. Removed again. H.
C          if(LCOLOR)CALL P2K_RGB_COLOUR(rgbr,rgbg,rgbb)
CHEN<
          CALL P2K_CSTRING(TEXT,1,0.)

          IF(PEAKNORM.GT.0.5.AND.PEAKNORM.LT.0.7) THEN
C           overprint < and > for speed.
CTSH        WRITE(TEXT,407)
CTSH++
            TEXT='>'
CTSH--
            CALL P2K_MOVE(XPLOTC,YPLOTC,0.)
            CALL P2K_CSTRING(TEXT,1,0.)
          ENDIF
          IF(PEAKNORM.GT.0.7) THEN
C           overprint H and I (0.7).
CTSH        WRITE(TEXT,409)
CTSH++
            TEXT='I'
CTSH--
            XPLOTXTRA=XPLOTC+0.475
            CALL P2K_MOVE(XPLOTXTRA,YPLOTC,0.)
            CALL P2K_CSTRING(TEXT,1,0.)
          ENDIF
C
C 402       FORMAT('.')
C 403       FORMAT(':')
C 404       FORMAT('-')
C C---------Threshold
C 405       FORMAT('+')
C 406       FORMAT('<')
C 407       FORMAT('>')
C 408       FORMAT('H')
C 409       FORMAT('I')
C
300   CONTINUE
C
      CALL P2K_COLOUR(0)
      CALL P2K_FONT("Courier"//CHAR(0),FONTSIZE)
      YPOS=SIZEY+4.0
      CALL P2K_MOVE(10.,YPOS,0.)
      CALL P2K_STRING(TITLEPLOT,80,0.)
      CALL P2K_PAGE
C
      RETURN
C
      END
C
C
C******************************************************************************
C
      SUBROUTINE MASKLATT(COOR,TITLE,NXYZ,MINA,MAXA,
     1  MINB,MAXB,A1,A2,B1,B2,IC,IR,NC,NR,IPASS)
C
      PARAMETER (MNY=-240)
      PARAMETER (MXY=240)
      PARAMETER (IPICDIM=512)
      PARAMETER (IPICDI2=1024)
C
      PARAMETER (LMAX=20100)
C
      COMMON//NX,NY,NZ,IXMIN,IYMIN,IZMIN,IXMAX,IYMAX,IZMAX
      DIMENSION ALINE(LMAX),TITLE(20),NXYZ(3),MXYZ(3)
      DIMENSION NXYZIPIC(3),MXYZIPIC(3)
      EQUIVALENCE (NCOLIPIC,NXYZIPIC(1)),(NLINEIPIC,NXYZIPIC(2))

      DIMENSION NXYZST(3)
      DIMENSION LABELS(20,10)
C
      DIMENSION COOR(3,MNY:MXY,MNY:MXY)
      DIMENSION PROGTIT(4)
C
      CHARACTER DAT*24
      CHARACTER*80 INFILE,OUTFILE
      CHARACTER*80 FILENAM,TEXT
      CHARACTER*80 NAME
      REAL*8 DOUBLMEAN
      INTEGER IFIELD(MNY:MXY,MNY:MXY)
      INTEGER IPICTU(IPICDIM,IPICDIM)
      INTEGER IPICT1(IPICDIM,IPICDIM)
      INTEGER IPICT2(IPICDI2,IPICDI2)
      INTEGER IPICTHACK(IPICDI2,IPICDI2)
C
      XCOORD(J,I)=A1*J+B1*I+IC
      YCOORD(J,I)=A2*J+B2*I+IR
C
      write(6,'(/,/,'' in masklat '',/,/,/)')
      print *,' MINA,MAXA,MINB,MAXB = ',MINA,MAXA,MINB,MAXB
C
      write(6,'(/,'' give name for original image to mask '')')
      read(*,'(A)')INFILE
      write(6,'('' INFILE= '',A40)')INFILE
C
      write(6,'(/,'' give name for output of masked image '')')
      read(*,'(A)')OUTFILE
      write(6,'('' OUTFILE= '',A40)')OUTFILE
C
      write(6,'(/,'' create test images (0=no, <0 = one, >0 =size)'')')
      read(*,'(I10)')icontrol
      print *,' icontrol= ',icontrol
C
      write(6,'(/,'' use external masking template (0=no, 1=yes)'')')
      read(*,'(I10)')iexternmask
      print *,' iexternmask= ',iexternmask
C
      if(icontrol.le.0)then
        IPICSIZ=256
      else
        IPICSIZ=icontrol
        if(IPICSIZ.lt.256)IPICSIZ=256
        if(IPICSIZ.gt.IPICDIM)IPICSIZ=IPICDIM
      endif
C
      ZERO=0.0
      SIZE=IPICSIZ
      PEAKMAX=-1.0E30
      XPEAKMAX=-100.
      YPEAKMAX=-100.
C
C
      SPLOT=SIZE/NXYZ(1)
      SIZEX=SPLOT*NXYZ(1)
      SIZEY=SPLOT*NXYZ(2)
C
C======================================
      IF(iexternmask.eq.1)goto 555
C======================================
C
C-----Get PEAKMAX and its position
C
       DO K=MINA,MAXA
         DO J=MINB,MAXB
          IF(COOR(3,J,K).GT.PEAKMAX)THEN
            PEAKMAX=COOR(3,J,K)
            XPEAKMAX=COOR(1,J,K)
            YPEAKMAX=COOR(2,J,K)
          END IF
        ENDDO
      ENDDO
C
      RTHRESH = 0.33
C
      if(abs(PEAKMAX).lt.0.0001)PEAKMAX=0.0001
C
C-----Prepare IFIELD with 0 and 1
C
      DO K=MINA,MAXA
        DO J=MINB,MAXB
          IOUT=0
          X=COOR(1,J,K)
          Y=COOR(2,J,K)
          IF(X.EQ.0.)IOUT=1
          XPLOTC=SPLOT*X
          YPLOTC=SPLOT*Y
          IF(XPLOTC.GT.SIZEX.OR.XPLOTC.LT.0.)IOUT=1
          IF(YPLOTC.GT.SIZEY.OR.YPLOTC.LT.0.)IOUT=1
          if(IOUT.eq.0)then
            PEAKNORM=COOR(3,J,K)/PEAKMAX
            IF(PEAKNORM.LE.RTHRESH)THEN
              IFIELD(J,K)=0
            ELSE
              IFIELD(J,K)=1
            ENDIF
          else
            IFIELD(J,K)=0
          endif
        ENDDO
      ENDDO
C
C-----Prepare test picture
C
      do K=1,IPICSIZ
        do J=1,IPICSIZ
          IPICTU(J,K)=0 
        enddo
      enddo
      DO K=MINA,MAXA
        DO J=MINB,MAXB
          IVAL=0
          X=COOR(1,J,K)
          Y=COOR(2,J,K)
          IF(X.NE.0.)THEN
            XPLOTC=SPLOT*X
            YPLOTC=SPLOT*Y
            IF(XPLOTC.GT.SIZEX.OR.XPLOTC.LT.1.)IVAL=1
            IF(YPLOTC.GT.SIZEY.OR.YPLOTC.LT.1.)IVAL=1
          ELSE
            IVAL=1
          ENDIF
          if(IVAL.eq.0)then
            if(IFIELD(J,K).eq.0)then
              IPICTU(XPLOTC,YPLOTC)=0
            else
              IPICTU(XPLOTC,YPLOTC)=255
            endif
          endif
        ENDDO
      ENDDO
C
C-----Write out test picture
C
      write(TEXT,1952)
1952  FORMAT('QUADSERCHH: Initial XCF')
      write(FILENAM(1:80),'(''TMP_quadserch_1.mrc'')')
      CALL PICWRI(IPICTU,FILENAM,TEXT,IPICDIM,IPICSIZ)
C
C-----Do morphological operations on test picture IPICTU:
C
      isize1 = IPICSIZ/25
      isize2 = isize1 - 1
      isize3 = isize1/4
      rsize4 = real(isize1)/20.0
      if(rsize4.lt.10.0)rsize4=10.0
C      rsize4 = 3.0
C
C.....Expand to get rid of holes
      CALL EXPAND(IPICTU,IPICT1,IPICDIM,IPICSIZ,isize1)
C  
      write(FILENAM(1:80),'(''TMP_quadserch_2.mrc'')')
      if(icontrol.gt.0)CALL PICWRI(IPICT1,FILENAM,TEXT,IPICDIM,IPICSIZ)
C
C.....Contract to get the original size back
      CALL CONTRA(IPICT1,IPICTU,IPICDIM,IPICSIZ,isize2)
C
      write(FILENAM(1:80),'(''TMP_quadserch_3.mrc'')')
      if(icontrol.gt.0)CALL PICWRI(IPICTU,FILENAM,TEXT,IPICDIM,IPICSIZ)
C
C.....Contract to get rid of islands
      CALL CONTRA(IPICTU,IPICT1,IPICDIM,IPICSIZ,isize2)
C
      write(FILENAM(1:80),'(''TMP_quadserch_4.mrc'')')
      if(icontrol.gt.0)CALL PICWRI(IPICT1,FILENAM,TEXT,IPICDIM,IPICSIZ)
C
C.....Expand to get the original size back
      CALL EXPAND(IPICT1,IPICTU,IPICDIM,IPICSIZ,isize1)
C
      write(FILENAM(1:80),'(''TMP_quadserch_5.mrc'')')
      if(icontrol.gt.0)CALL PICWRI(IPICTU,FILENAM,TEXT,IPICDIM,IPICSIZ)
C
C.....Taper edges
      CALL TAPER(IPICTU,IPICT1,IPICDIM,IPICSIZ,isize3)
C
      write(FILENAM(1:80),'(''TMP_quadserch_6.mrc'')')
      if(icontrol.ne.0)CALL PICWRI(IPICT1,FILENAM,TEXT,IPICDIM,IPICSIZ)
C
C.....Smooth picture
      CALL SMOOTH(IPICT1,IPICTHACK,IPICT2,IPICDIM,IPICDI2,IPICSIZ,
     1 rsize4)
C     
      write(FILENAM(1:80),'(''TMP_quadserch_7.mrc'')')
      if(icontrol.ne.0)CALL PICWRI(IPICT2,FILENAM,TEXT,IPICDI2,IPICDI2)
C
C======================================
      goto 556
555   continue
      write(FILENAM(1:80),'(''TMP_quadserch_7.mrc'')')
      CALL IMOPEN(1,FILENAM,'RO')
      CALL IRDHDR(1,NXYZIPIC,MXYZIPIC,MODEIPIC,DMINIPIC,DMAXIPIC,DMEANIPIC)
C      write(6,'(''::MODEIPIC = '',I4)')MODEIPIC
C      write(6,'(''::DMINIPIC = '',G15.3)')DMINIPIC
C      write(6,'(''::DMAXIPIC = '',G15.3)')DMAXIPIC
C      write(6,'(''::DMEANPIC = '',G15.3)')DMEANPIC
C      write(6,'(''::DEBUG    = '',I8)')IPICT2(10,10)
      CALL IMPOSN(1,0,0)
      NCOLPIC=NXYZIPIC(1)
      NLINEPIC=NXYZIPIC(2)
C-----IPICT2 is an INTEGER (i.e., 2Byte), while mode=0 means 1Byte
      DO iy = 1,NLINEPIC
        CALL IRDLIN(1,ALINE,*900)
        DO ix= 1,NCOLPIC
          VAL=ALINE(ix)
          IPICT2(ix,iy)=VAL
        enddo
      enddo
C     CALL IRDPAS(1,IPICT2,NCOLIPIC,NLINEIPIC,0,NCOLIPIC-1,0,NLINEIPIC-1,*9400)

C      write(6,'(''::DEBUG2   = '',I8)')IPICT2(10,10)

      CALL IMCLOSE(1)
556   continue
C
C======================================
C
C-----mask the original image with this pattern
C
      CALL IMOPEN(1,INFILE,'RO')
C
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      DOMIN=DMIN
      DOMAX=DMAX
      DOMEAN=DMEAN
C
      CALL IMOPEN(2,OUTFILE,'NEW')
      CALL ITRHDR(2,1)
C
      write(NAME,1954)
C      write(TITLE,1954)
1954  FORMAT('QUADSERCHH: masking for XCF ')
      write(6,'('' masking picture'')')
      DMIN =  1.E10
      DMAX = -1.E10
      DMEAN = 0.0
      DOUBLMEAN = 0.0
      CALL IWRHDR(2,NAME,1,DMIN,DMAX,DMEAN)
C
      NX=NXYZ(1)
      NY=NXYZ(2)
      NZ=NXYZ(3)
      iunder=0
      iover=0
      rmin=0.0
      rmax=32000.0
      print *,'starting masking with image NX,NY,NZ=',NX,NY,NZ
      DO IZ= 1,NZ
        ilj=1
        DO IY = 1,NY
          CALL IRDLIN(1,ALINE,*900)
          ilk=(IY*IPICDI2)/NY
          if(ilk.lt.1)ilk=1
          if(ilk.gt.IPICDI2)ilk=IPICDI2
          DO IX = 1,NX
            ilj=(IX*IPICDI2)/NX
            if(ilj.lt.1)ilj=1
            if(ilj.gt.IPICDI2)ilj=IPICDI2
C
            VAL=(ALINE(IX)-DOMEAN)*(real(IPICT2(ilj,ilk))/255.0)+DOMEAN
C
            if(VAL.lt.rmin)then
              iunder=iunder+1
              VAL=rmin
            endif
            if(VAL.gt.rmax)then
              iover=iover+1
              VAL=rmax
            endif
            IF (VAL .LT. DMIN) DMIN = VAL
            IF (VAL .GT. DMAX) DMAX = VAL
            DOUBLMEAN = DOUBLMEAN + VAL
            ALINE(IX) = VAL
          enddo
          CALL IWRLIN(2,ALINE)
        enddo
      enddo
      DMEAN = DOUBLMEAN/(NX*NY*NZ)
      write(6,'('' file written, DMIN,DMAX,DMEAN='',3G12.3)')
     1  DMIN,DMAX,DMEAN
      CALL IWRHDR(2,NAME,-1,DMIN,DMAX,DMEAN)
C
      CALL IMCLOSE(2)
      CALL IMCLOSE(1)
C
      if(iunder.ne.0)then
        print *,'WARNING: iunder = ',iunder
      endif
C
      if(iover.ne.0)then
        print *,'WARNING: iover = ',iover
      endif
C
      goto 999
 900  continue
      write(6,'('':: quadserchh: ERROR: end of file'')')
      STOP ':: END of FILE on read'
C
 999  continue
      RETURN
C
9400   WRITE(6,9013)
9013   FORMAT(':: Error reading mask TMP_quadserch_7.mrc')
       STOP
C
      END
C
C******************************************************************************************************
C
      SUBROUTINE MASKINF(COOR,TITLE,NXYZ,MINA,MAXA,
     1    MINB,MAXB,A1,A2,B1,B2,IC,IR,NC,NR,IPASS)
C
      PARAMETER (MNY=-240)
      PARAMETER (MXY=240)
      PARAMETER (IPICDI3=1024)
C
      PARAMETER (LMAX=20100)
C
      COMMON//NX,NY,NZ,IXMIN,IYMIN,IZMIN,IXMAX,IYMAX,IZMAX
      DIMENSION TITLE(20),NXYZ(3),MXYZ(3)
      DIMENSION NXYZST(3)
      DIMENSION LABELS(20,10)
C
      DIMENSION COOR(3,MNY:MXY,MNY:MXY)
      DIMENSION PROGTIT(4)
C
      CHARACTER DAT*24
      CHARACTER*80 OUTFILE1,OUTFILE2
      CHARACTER*80 FILENAM,TEXT
      CHARACTER*80 NAME
      REAL*8 DOUBLMEAN
      INTEGER IFIELD(MNY:MXY,MNY:MXY)
      INTEGER IPICTU(IPICDI3,IPICDI3)
      INTEGER IPICT1(IPICDI3,IPICDI3)
C
      XCOORD(J,I)=A1*J+B1*I+IC+1.0
      YCOORD(J,I)=A2*J+B2*I+IR+1.0
C
      write(6,'(/,/,'' in MASKINF '',/,/,/)')
      print *,' MINA,MAXA,MINB,MAXB = ',MINA,MAXA,MINB,MAXB
C
      IPICSIZ=1024
C
      ZERO=0.0
      RSIZE=IPICSIZ
      PEAKMAX=-1.0E30
      XPEAKMAX=-100.
      YPEAKMAX=-100.
C
C
      SPLOT=RSIZE/NXYZ(1)
      SIZEX=SPLOT*NXYZ(1)
      SIZEY=SPLOT*NXYZ(2)
C
C-----Get PEAKMAX and its position
C
      DO K=MINA,MAXA
        DO J=MINB,MAXB
          IF(COOR(3,J,K).GT.PEAKMAX)THEN
            PEAKMAX=COOR(3,J,K)
            XPEAKMAX=COOR(1,J,K)
            YPEAKMAX=COOR(2,J,K)
          END IF
        ENDDO
      ENDDO
C
      if(PEAKMAX.gt.0.0001)then
        RFACT = 255.0/PEAKMAX
      else
        RFACT = 255.0
      endif
C
C-----Prepare output pictures
C
      do K=1,IPICSIZ
        do J=1,IPICSIZ
          IPICTU(J,K)=255
          IPICT1(J,K)=255
        enddo
      enddo
C
C-----Prepare output file for CCmap
C
      irad = 3
      irad2=irad*irad
      iwid1 = irad*2
      iwid2 = -iwid1
      rwid = real(iwid1)
      DO K=MINA,MAXA
        DO J=MINB,MAXB
          IVAL=0
          X=COOR(1,J,K)
          Y=COOR(2,J,K)
          IF(X.NE.0.)THEN
            XPLOTC=SPLOT*X
            YPLOTC=SPLOT*Y
            IF(XPLOTC.GE.(SIZEX-rwid-1).OR.XPLOTC.LE.rwid+1)IVAL=1
            IF(YPLOTC.GE.(SIZEY-rwid-1).OR.YPLOTC.LE.rwid+1)IVAL=1
          ELSE
            IVAL=1
          ENDIF
          if(IVAL.eq.0)then
C-----------Set a blob 
            do ix=iwid2,iwid1
              do iy = iwid2,iwid1
                if ( (ix*ix+iy*iy).le.irad2 ) then
                  kx=XPLOTC+ix
                  ky=YPLOTC+iy
                  IPICTU(kx,ky)=255-INT(COOR(3,J,K)*RFACT)
                endif
              enddo
            enddo
          endif
        ENDDO
      ENDDO
C
C-----Now the unbending plot
C
      DO 100 K=MINA,MAXA
        DO 100 J=MINB,MAXB
          XPLOT=SPLOT*XCOORD(K,J)
          YPLOT=SPLOT*YCOORD(K,J)
          XPLOTC=SPLOT*COOR(1,J,K)
          YPLOTC=SPLOT*COOR(2,J,K)
          XERR=10*XPLOTC-9*XPLOT
          YERR=10*YPLOTC-9*YPLOT
C deviations from perfect lattice plotted at 10x actual deviation
          IF(XERR.GE.(SIZEX-1.0).OR.XERR.LE.2.) GO TO 100
          IF(YERR.GE.(SIZEY-1.0).OR.YERR.LE.2.) GO TO 100
C---------Calculate the length of the line in pixels
          ILEN = INT(sqrt((XPLOT-XERR)*(XPLOT-XERR)+
     .                    (YPLOT-YERR)*(YPLOT-YERR))) * 2
C
C---------Draw a line from (XPLOT,YPLOT) to (XERR,YERR)
          rlen = real(ilen)
          if(rlen.lt.0.001)rlen=0.001
          do I=0,ILEN
            kx=int(XPLOT+(real(I)/rlen)*(XERR-XPLOT))
            ky=int(YPLOT+(real(I)/rlen)*(YERR-YPLOT))
            IF (kx.lt.1.OR.kx.ge.IPICSIZ) goto 100
            IF (ky.lt.1.OR.ky.ge.IPICSIZ) goto 100
            IPICT1(kx,ky)=255-INT(COOR(3,J,K)*RFACT)
          enddo
 100  continue 
C
C-----Write out Masking Info Image
C
      write(TEXT,1952)
1952  FORMAT('QUADSERCHH: Masking Info')
      write(FILENAM(1:80),'(''ManualMasking_CCmap.mrc'')')
      CALL PICWRI(IPICTU,FILENAM,TEXT,IPICDI3,IPICSIZ)
C
C-----Write out Masking Info Image of the Unbending Plot
C
      write(TEXT,1952)
      write(FILENAM(1:80),'(''ManualMasking_UnbendPlot.mrc'')')
      CALL PICWRI(IPICT1,FILENAM,TEXT,IPICDI3,IPICSIZ)
C
      goto 999
 900  continue
      write(6,'(''::  quadserchh: ERROR: end of file'')')
      STOP ':: END of FILE on read'
C
 999  continue
      RETURN
C
      END
C
C************************************************************************
C
      SUBROUTINE EXPAND(IPICTU,IPICT1,IPICDIM,IPICSIZ,idist)
C
      INTEGER IPICTU(IPICDIM,IPICDIM)
      INTEGER IPICT1(IPICDIM,IPICDIM)
C
      print *,'EXPAND called with ',idist
C
      ianf = -idist
      iend =  idist
      id2 = idist * idist
      DO K=1,IPICSIZ
        DO J=1,IPICSIZ
          IOUT=0
          do ikrun=ianf,iend
            ikloc=K+ikrun
            if(ikloc.lt.1      )ikloc=1
            if(ikloc.gt.IPICSIZ)ikloc=IPICSIZ
            ik2 = ikrun * ikrun
            do ijrun=ianf,iend
              ijloc=J+ijrun
              if(ijloc.lt.1      )ijloc=1
              if(ijloc.gt.IPICSIZ)ijloc=IPICSIZ
              if(ik2+ijrun*ijrun.le.id2)then
                if(IPICTU(ijloc,ikloc).ne.0)IOUT=255
              endif
            enddo
          enddo
          IPICT1(J,K)=IOUT
        ENDDO
      ENDDO
C
      RETURN
      END
C
C************************************************************************
C
      SUBROUTINE CONTRA(IPICTU,IPICT1,IPICDIM,IPICSIZ,idist)
C
      INTEGER IPICTU(IPICDIM,IPICDIM)
      INTEGER IPICT1(IPICDIM,IPICDIM)
C
      print *,'CONTRA called with ',idist
C
      ianf = -idist
      iend =  idist
      id2 = idist * idist
      DO K=1,IPICSIZ
        DO J=1,IPICSIZ
          IOUT=255
          do ikrun=ianf,iend
            ikloc=K+ikrun
            if(ikloc.lt.1      )ikloc=1
            if(ikloc.gt.IPICSIZ)ikloc=IPICSIZ
            ik2 = ikrun * ikrun
            do ijrun=ianf,iend
              ijloc=J+ijrun
              if(ijloc.lt.1      )ijloc=1
              if(ijloc.gt.IPICSIZ)ijloc=IPICSIZ
              if(ik2+ijrun*ijrun.le.id2)then
                if(IPICTU(ijloc,ikloc).eq.0)IOUT=0
              endif
            enddo
          enddo
          IPICT1(J,K)=IOUT
        ENDDO
      ENDDO
C
      RETURN
      END
C
C************************************************************************
C
      SUBROUTINE SMOOTH(IPICTU,IPICT1,IPICT2,IPICDIM,IPICDI2,IPICSIZ
     1 ,rdist)
C
      PARAMETER (IGAURA1=-100)
      PARAMETER (IGAURA2=100)
C
      INTEGER IPICTU(IPICDIM,IPICDIM)
      INTEGER IPICT1(IPICDI2,IPICDI2)
      INTEGER IPICT2(IPICDI2,IPICDI2)
C
      REAL RGAUS(IGAURA1:IGAURA2,IGAURA1:IGAURA2)
      REAL*8 RSUM,RINT
C
      print *,'SMOOTH called with ',rdist
C
      if(rdist.lt.0.001)rdist=0.001
      do K=IGAURA1,IGAURA2
       do J=IGAURA1,IGAURA2
          rrad=SQRT(1.0*K*K+1.0*J*J)
          RGAUS(J,K)=exp(-rrad/rdist)
        enddo
      enddo
C
      ILIMIT=IGAURA2
      do I=1,IGAURA2
C-------if(RGAUS(I,0).gt.0.01)ILIMIT=I
C-------Here only for masking info, can be faster:
        if(RGAUS(I,0).gt.0.08)ILIMIT=I
      enddo
      if(ILIMIT.gt.IGAURA2)ILIMIT=IGAURA2
C
      ianf = -ILIMIT
      iend =  ILIMIT
      RINT=0.0
      do K=ianf,iend
        do J=ianf,iend
          RINT=RINT+RGAUS(J,K)
        enddo
      enddo
C
      write(6,'('' RGAUS field calculated'')')
      print *,' reaching until ',ILIMIT,' with integral = ',RINT
C
      DO K=1,IPICDI2
        DO J=1,IPICDI2
          JL=(J*IPICSIZ)/IPICDI2
          KL=(K*IPICSIZ)/IPICDI2
          if(JL.lt.1)JL=1
          if(JL.gt.IPICSIZ)JL=IPICSIZ
          if(KL.lt.1)KL=1
          if(KL.gt.IPICSIZ)KL=IPICSIZ
          IPICT1(J,K) = IPICTU(JL,KL)
        enddo
      enddo
      write(6,'('' IPICTU copied to IPICT1'')')
C
      write(*,'(''Convoluting: IPICDI2='',I8,
     .  '', ianf,iend = '',2I8)')IPICDI2,ianf,iend
      call system("echo `date`")
C
      DO K=1,IPICDI2
        DO J=1,IPICDI2
          RSUM=0.0
          do ikrun=ianf,iend
            ikloc=K+ikrun
            if(ikloc.lt.1      )ikloc=1
            if(ikloc.gt.IPICDI2)ikloc=IPICDI2
            do ijrun=ianf,iend
              ijloc=J+ijrun
              if(ijloc.lt.1      )ijloc=1
              if(ijloc.gt.IPICDI2)ijloc=IPICDI2
              RSUM=RSUM+IPICT1(ijloc,ikloc)*RGAUS(ijrun,ikrun)
            enddo
          enddo
          RSUM=RSUM/RINT
          IPICT2(J,K)=RSUM
        ENDDO
      ENDDO
C
      write(6,'('' IPICT1 smoothed to IPICT2'')')
      call system("echo `date`")
C
      RETURN
      END
C
C************************************************************************
C
      SUBROUTINE TAPER(IPICTU,IPICT1,IPICDIM,IPICSIZ,idist)
C
      INTEGER IPICTU(IPICDIM,IPICDIM)
      INTEGER IPICT1(IPICDIM,IPICDIM)
C
      print *,'TAPER called with ',idist
C
      DO K=1,IPICSIZ
        DO J=1,IPICSIZ
          IVAL=IPICTU(J,K)
          if((J.le.idist        ) .or.
     1       (J.ge.IPICSIZ-idist) .or. 
     2       (K.le.idist        ) .or. 
     3       (K.ge.IPICSIZ-idist)     )then
            IVAL=0
          endif
          IPICT1(J,K)=IVAL
        ENDDO
      ENDDO
C
      RETURN
      END
C
C************************************************************************
C
      SUBROUTINE PICWRI(IPICTU,FILENAM,TEXT,IPICDIM,IPICSIZ)
C
      PARAMETER (LMAX=20100)
C
      INTEGER IPICTU(IPICDIM,IPICDIM)
C
      COMMON//NX,NY,NZ,IXMIN,IYMIN,IZMIN,IXMAX,IYMAX,IZMAX
C
      DIMENSION ALINE(LMAX),TITLE(20),NXYZ(3),MXYZ(3)
      DIMENSION NXYZST(3)
      DIMENSION LABELS(20,10)
C
      DIMENSION TEXT(20),PROGTIT(4)
C
      CHARACTER*80 FILENAM
      CHARACTER*200 cline
C
      DMIN =  0.0
      DMAX =  255.0
      DMEAN = 0.5
      MODE = 0
C
      NX=IPICSIZ
      NY=IPICSIZ
      NXYZ(1)=NX
      NXYZ(2)=NY
      NXYZ(3)=1 
      NXYZST(1) = NXYZ(1)
      NXYZST(2) = NXYZ(2)
      NXYZST(3) = NXYZ(3)
C
      write(6,'('' Writing picture into '',A40)')FILENAM
      write(6,'('' Size '',2I15)')IPICDIM,IPICSIZ
C
      MODE=0
      call shorten(FILENAM,k)
      write(cline,'(''\rm -f '',A)')FILENAM(1:k)
      call system(cline)
      CALL IMOPEN(2,FILENAM,'NEW')
      CALL ICRHDR(2,NXYZST,NXYZST,MODE,LABELS,0)
      CALL ITRLAB(2,1)
      CALL IWRHDR(2,TEXT,1,DMIN,DMAX,DMEAN)
C
      DMIN= 1.0e10
      DMAX=-1.0e10
      DOUBLMEAN=0.0
      DO K=1,IPICSIZ
        DO J=1,IPICSIZ
          ALINE(J) = IPICTU(J,K)
          if(IPICTU(J,K).lt.DMIN)DMIN=IPICTU(J,K)
          if(IPICTU(J,K).gt.DMAX)DMAX=IPICTU(J,K)
          DOUBLMEAN=DOUBLMEAN+IPICTU(J,K) 
        enddo 
        CALL IWRLIN(2,ALINE)
      enddo
      DMEAN=DOUBLMEAN/(IPICSIZ*IPICSIZ)
      CALL IWRHDR(2,TITLE,-1,DMIN,DMAX,DMEAN)
      CALL IMCLOSE(2)
C
      RETURN
      END
C
C********** FROM CORNELL PROG *************************************************
C
      SUBROUTINE RCAVG(X,N,AV)
C
      DIMENSION X(1)
C
      AVV=0.0
      DO 10 I=1,N
10    AVV=AVV+X(I)
      AV=AV+AVV/N
C
      RETURN
C
      END
C
C********************************************************************************
C********* ADDED MARCH 1985. JMB *********
C
        SUBROUTINE PROFIT(RADSTORE,RADLIMPSQ,RADLIMQSQ,
     1    IIX,IIY,KDC,KDR,KDC21,KDR21,
     2    CGXADJST,CGYADJST,CCFBEST,BIGFACTR,NCOUNT,JFLAG,IDRIFT,
     3    IONELA)
C     
        COMMON/PROFITC/PROFILE,XC
C
C-------PROGRAM TO MATCH PROFILE TO CORRELATION MAP PEAKS
C
        PARAMETER (MDC=120)
        PARAMETER (MDR=120)
        DIMENSION PROFILE(101,101),XC(MDC,MDR),RADSTORE(-180:180)
        DIMENSION NSTEP(10),NSTART(10),NFIN(10),NCOUNT(11)

        DATA NSTEP/5,1,1,1,1,1,1,1,1,1/
        DATA NSTART/-10,-7,-5,-3,-3,-3,-3,-3,-3,-3/
        DATA NFIN/10,7,5,3,3,3,3,3,3,3/
        DATA NPASSLIM/9/
C
C-------IX+IIX AND IY+IIY ARE COORDS IN CORRELATION-MAP ARRAY ON COARSE 
C-------GRID AT WHICH MAXIMUM HAS BEEN FOUND IN PRELIMINARY SEARCH.
C-------TRUE POSITION OF CORRELATION PEAK IS TO BE FOUND BY MATCHING
C-------THE VALUES OF DENSITIES AT SURROUNDING GRID POINTS AGAINST
C-------A MORE FINELY SAMPLED PROFILE
C
C
C       
        IFLAG=0
5       LXSV=0
        LYSV=0
        NPASS=1
        ISTEP=NSTEP(NPASS)
        ISTART=NSTART(NPASS)
        IFIN=NFIN(NPASS)
        LY=0
C
30      CONTINUE
          CCFBSTX=0.0
          CCFBSTY=0.0
          DO 40 ITRX=ISTART,IFIN,ISTEP
C
            LX=ITRX+LXSV
C-----------LX,LY ARE COORDS ON FINE GRID
C-----------CENTRE OF PROFILE TRIED AT POSITION LX,LY
C
            CALL CCFCALC(LX,LY,RADSTORE,RADLIMPSQ,RADLIMQSQ,
     .        IIX,IIY,KDC,KDR,KDC21,KDR21,CCF,
     .        IFLAG,JFLAG)
C
            IF (CCF.GT.CCFBSTX)THEN
              CCFBSTX=CCF
              LXBEST=LX
            END IF
C
40        CONTINUE
C
          IF(IFLAG.EQ.2)WRITE(6,900)LXBEST,LY,LXSV,LYSV,CCFBSTX
900       FORMAT(' AFTER X ROW',2I5,5X,2I5,F8.3)
          IF(LXBEST.EQ.LXSV.AND.NPASS.GT.2)GO TO 50
          LX=LXBEST
C
          DO 41 ITRY=ISTART,IFIN,ISTEP
            LY=ITRY+LYSV
            CALL CCFCALC(LX,LY,RADSTORE,RADLIMPSQ,RADLIMQSQ,
     .        IIX,IIY,KDC,KDR,KDC21,KDR21,CCF,
     .        IFLAG,JFLAG)
            IF(CCF.GT.CCFBSTY)THEN
              CCFBSTY=CCF
              LYBEST=LY
            END IF
41        CONTINUE
C
          IF(IFLAG.EQ.2)WRITE(6,901)LXBEST,LYBEST,LXSV,LYSV,CCFBSTY
901       FORMAT(' AFTER Y ROW',2I5,5X,2I5,F8.3)
          IF(LYBEST.EQ.LYSV.AND.NPASS.GT.1)GO TO 51
C
          NPASS=NPASS+1
          IF (NPASS.EQ.NPASSLIM)GO TO 55
          ISTEP=NSTEP(NPASS)
          ISTART=NSTART(NPASS)
          IFIN=NFIN(NPASS)
          LXSV=LXBEST
          LYSV=LYBEST
          LY=LYBEST
        GO TO 30
C
50      CCFBEST=CCFBSTX
        GO TO 52
51      CCFBEST=CCFBSTY
        GO TO 52
55      WRITE(6,56)NPASS,LXBEST,LYBEST,LXSV,LYSV
56      FORMAT(' NOT CONVERGED',5I5)
C-------REPEAT WITH PRINT
C
        IF(IFLAG.EQ.0)THEN
          IFLAG=1
          GO TO 5
        END IF
        IFLAG=0
C       
C
52      CONTINUE
        NCOUNT(NPASS)=NCOUNT(NPASS)+1
C        WRITE(6,66)LXBEST,LYBEST,CCFBEST,NPASS
C66      FORMAT(' POSITION OF BEST CORRELATION',2I5,F8.3,I5)
C
        CGXADJST=LXBEST/20.0
        CGYADJST=LYBEST/20.0
        IXP=51-LXBEST
        IYP=51-LYBEST
C
CHEN----this is against an SGI optimization error :
        ITXP=IXP
        ITYP=IYP
        if(ITXP.lt.1)   ITXP=1
        if(ITYP.lt.1)   ITYP=1
        if(ITXP.gt.101) ITXP=101
        if(ITYP.gt.101) ITYP=101
C
        IF(LXBEST.GT.20 .OR. LXBEST.LT.-20 .OR.
     .     LYBEST.GT.20 .OR. LYBEST.LT.-20)  THEN
          BIGFACTR=0.0
        ELSE
          if(PROFILE(ITXP,ITYP).gt.0.0001)then
            BIGFACTR=PROFILE(51,51)/PROFILE(ITXP,ITYP)
          else
            BIGFACTR=1000.0
          endif
        endif
C
        IF(BIGFACTR.GT.10.0 .OR. BIGFACTR.LT.0.0 .OR.
     1     LXBEST  .GT.20   .OR. LXBEST  .LT.-20 .OR.
     2     LYBEST  .GT.20   .OR. LYBEST  .LT.-20    )THEN
C          write(6,'(''BIGFACTR,LXBEST,LYBEST out of limits: '',
C     .      G16.6,2I10)')BIGFACTR,LXBEST,LYBEST
          IF(IONELA.eq.0)THEN
            CGXADJST=0.0
            CGYADJST=0.0
            BIGFACTR=0.0
          ENDIF
          IDRIFT=1
        END IF
        RETURN
C
        END
C
C******************************************************************************
C
        SUBROUTINE CCFCALC(LX,LY,RADSTORE,RADLIMPSQ,RADLIMQSQ,
     . IIX,IIY,KDC,KDR,KDC21,KDR21,
     . CCF,IFLAG,JFLAG)
C
        COMMON/PROFITC/PROFILE,XC
C
        PARAMETER (MDC=120)
        PARAMETER (MDR=120)
C
        DIMENSION PROFILE(101,101),XC(MDR,MDC),RADSTORE(-180:180)
        REAL*8 SUMCC,SUMPP,SUMPC,SUMP,SUMC
        DATA RDEG/57.295776/
C
C-------CALCULATE CORRELATION COEFF FOR EACH POSITION LX,LY
C
        SUMC=0.
        SUMP=0.
        SUMPC=0.
        SUMPP=0.
        SUMCC=0.
        NCOMP=0
C
        DO 50 ISTPX=-3,3
          XP=ISTPX*20-LX
          IXP=XP+52
          XG=IIX+ISTPX
          IXG=XG+1+KDC+1
          IF(IXG.LT.1.OR.IXG.GT.KDC21)GO TO 50
C
          DO 51 ISTPY=-3,3
            YP=ISTPY*20-LY
            IYP=YP+52
            YG=IIY+ISTPY
            IYG=YG+1+KDR+1
            IF(IYG.LT.1.OR.IYG.GT.KDR21)GO TO 51
            RADSQ=(XP**2+YP**2)
            IF(RADSQ.LT.RADLIMPSQ) GO TO 71
            IF(RADSQ.GT.RADLIMQSQ) GO TO 51
            IANGLE=RDEG*ATAN2(YP,XP)    ! Only for radius betw P & Q.
            IF(RADSQ.GT.RADSTORE(IANGLE))GO TO 51
C           WRITE(6,70)IXP,IYP,IXG,IYG
70          FORMAT(' ??',2I5,5X,2I5)
C-----------DENSITY FROM PROFILE
71          RHOP=PROFILE(IXP,IYP)
C           WRITE(6,75)IXP,IYP,RHOP
75          FORMAT(' ???',2I5,E12.4)
C-----------DENSITY FROM CORRELATION MAP
            RHOC=XC(IXG,IYG)
C
            NCOMP=NCOMP+1
            SUMC=SUMC+RHOC
            SUMP=SUMP+RHOP
            SUMPP=SUMPP+RHOP*RHOP
            SUMCC=SUMCC+RHOC*RHOC
            SUMPC=SUMPC+RHOC*RHOP
C
51        CONTINUE
50      CONTINUE
C
C       CALCULATE CORRELATION COEFF
        IF (NCOMP.EQ.0)THEN
          JFLAG=1
          CCF=0.
C         WRITE(6,65)NCOMP,LX,LY
65        FORMAT(1X,I5,' COMPARISONS FOR POSITION',2I5)
          GO TO 55 
C         STOP
        END IF
        IF(NCOMP.EQ.1)THEN
          JFLAG=1
          CCF=0.0
          WRITE(6,65)NCOMP,LX,LY
          GO TO 55
        END IF

C
        TOP=SUMPC-(SUMP*SUMC)/NCOMP
        TEMPP=SUMPP-(SUMP**2)/NCOMP
        TEMPC=SUMCC-(SUMC**2)/NCOMP
C       IF(TEMPP.LE.0.0)WRITE(6,67)TEMPP,TEMPC
C       IF(TEMPC.LE.0.0)WRITE(6,67)TEMPP,TEMPC
67      FORMAT('?',2E12.4)
C       write(6,4) TEMPP, TEMPC         ! diagnostic for overflow 
4       format(' TEMPP, TEMPC =',2E15.5)
        BOTTOMSQ=TEMPP*TEMPC
        IF(BOTTOMSQ.LE.0.0)THEN
          CCF=0.0
C         WRITE(6,63)LX,LY,CCF,NCOMP,SUMP,SUMC
63        FORMAT(' ?',2I5,F8.3,I5,1X,2E10.4)
          JFLAG=1
        ELSE
          BOTTOM=SQRT(TEMPP*TEMPC)
          CCF=TOP/BOTTOM
        END IF
55      CONTINUE
        IF(IFLAG.EQ.2)WRITE(6,64)LX,LY,CCF
64      FORMAT(' POSITION AND COEFF',2I5,F8.3)
C
        RETURN
C
        END
C
C******************************************************************************
C
        SUBROUTINE PREDICT(NRANGE,IASGN,IBSGN,IA,IB,XPREDICT,YPREDICT,
     . MINA,MAXA,MINB,MAXB,XERROR,YERROR,PEAK,NAVG,IPASS)
C
C       FOR IPASS = 1 or 0; VECTORS ARE ONLY AVAILABLE FOR PREDICTION BEHIND
C       THE MOVING POINT
C       FOR IPASS = 2; ESTIMATE OF ERROR VECTORS AVAILABLE ALL ROUND EACH POINT 
C       FOR IPASS = 3; Like IPASS=2, but result written out
C
C        XERROR(IA,IB) IS  9999.0 IF NO DATA AVAILABLE FOR ERROR AT THIS POINT; 
C       IF NOT 9999.0 XERROR(IA,IB), YERROR(IA,IB) ARE
C       THE VECTORS AVAILABLE FROM THE CURRENT PASS OR THE PREVIOUS PASS
C
C
      PARAMETER (MNY=-240)
      PARAMETER (MXY=240)
      DIMENSION XERROR(MNY:MXY,MNY:MXY),YERROR(MNY:MXY,MNY:MXY)
      DIMENSION PEAK(MNY:MXY,MNY:MXY)
C
C       FORM LOCAL AVERAGE OF X AND Y DRIFTS FROM TRUE LATTICE POSITIONS
C
C
      TOTDX=0.
      TOTDY=0.
      TOTWDX=0.
      TOTWDY=0.
      SUMWT=0.0
      NAVG=0
C
      DO 110 NA=-NRANGE,NRANGE
C
        IAN=IA+NA
C
        IF(IAN.LT.MINA.OR.IAN.GT.MAXA)GO TO 110
C
        NASQ=(NA)*(NA)
C
        DO 10 NB=-NRANGE,NRANGE
C
          IBN=IB+NB
C
          IF(IBN.LT.MINB.OR.IBN.GT.MAXB)GO TO 10
C
          DISTSQ=NASQ+(NB)*(NB)
C
          IF(DISTSQ.EQ.0.0)THEN
            RECDIST=1.0/5.0
          ELSE
            DIST=SQRT(DISTSQ)+5.0
            RECDIST=1.0/DIST
          END IF
C
          IF(XERROR(IAN,IBN).EQ.9999.)GO TO 10
C
          TOTDX=TOTDX+XERROR(IAN,IBN)
          TOTDY=TOTDY+YERROR(IAN,IBN)
          WT=PEAK(IAN,IBN)*PEAK(IAN,IBN)*RECDIST
          TOTWDX=TOTWDX+XERROR(IAN,IBN)*WT
          TOTWDY=TOTWDY+YERROR(IAN,IBN)*WT
          SUMWT=SUMWT+WT
          NAVG=NAVG+1
C
10      CONTINUE
C
110   CONTINUE
C
      IF(NAVG.EQ.0.OR.SUMWT.EQ.0.0)THEN
        XPREDICT=0.
        YPREDICT=0.
C       WRITE(6,99)
99      FORMAT(' NO PREDICTION DATA AVAILABLE ')
      ELSE
        XPREDICT=TOTWDX/SUMWT
        YPREDICT=TOTWDY/SUMWT      
      END IF
C
100   CONTINUE
C
C       WRITE(6,20)IA,IB,XPREDICT,YPREDICT,SUMWT                ! for diagnostic use
20      FORMAT(' IA,IB ;PREDICTED DX,DY AND NAVG,SUM OF WEIGHTS',
     . 2I5,2F10.5,F10.5)
C
      RETURN
      END
C
C*******************************************************************************
C
      SUBROUTINE GETXC(ARRAY,NCOL,NLINE,XC,IX,IY,KDC1,KDR1)
C
C-----ARRAY: 1D input field with image
C-----NCOL:  number of columns in input image
C-----NLINE: number of lines   in input image
C-----XC:    output array of exact dimensions
C-----IX:    X-offset of sub-area to read
C-----IY:    Y-offset of sub-area to read
C-----KDC1:  numer of columnes to read: IX +/- KDC1
C-----KDR1:  numer of rows     to read: IY +/- KDR1
C
C replacement subroutine to return region of correlation map of interest, after
C reading in the entire array in one initial IRDPAS statement.
C    defunct----CALL-IRDPAS(1,XC,MDR,MDC,IX-KDC1,IX+KDC1,IY-KDR1,IY+KDR1,*9500)
      PARAMETER (MDR=120)
      PARAMETER (MDC=120)
      DIMENSION ARRAY(1),XC(MDC,MDR)
      IR = 0
      DO 75 JR = IY-KDR1,IY+KDR1
        INDY = NCOL*JR + 1
        IR = IR+1
        IC = 0
        DO 70 JC = IX-KDC1,IX+KDC1
          IC = IC+1
          INDEX = INDY + JC
          XC(IC,IR)=ARRAY(INDEX)
70      CONTINUE
75    CONTINUE
      RETURN
      END
C
C*******************************************************************************
C
      SUBROUTINE ERREXT(XERROR,YERROR,PEAK,IA,IB)
C
      PARAMETER (MNY=-240)
      PARAMETER (MXY=240)
      DIMENSION XERROR(MNY:MXY,MNY:MXY),YERROR(MNY:MXY,MNY:MXY)
      DIMENSION PEAK(MNY:MXY,MNY:MXY)
C
C-----Extrapolate the last errors and peaks to this one
C
      IGOOD=1
C
      IAN=IA
      IBN=IB
C
C-----Calculate the average value for XERROR(IAN,IBN) from the 
C-----four neighbours
C
      ICOUNT=0
      XLOCER=0.0
C
      IF(XERROR(IAN-1,IBN).NE.9999.0)THEN
        XLOCER=XLOCER+XERROR(IAN-1,IBN)
        ICOUNT=ICOUNT+1
      ENDIF
      IF(XERROR(IAN,IBN-1).NE.9999.0)THEN
        XLOCER=XLOCER+XERROR(IAN,IBN-1)
        ICOUNT=ICOUNT+1
      ENDIF
      IF(XERROR(IAN+1,IBN).NE.9999.0)THEN
        XLOCER=XLOCER+XERROR(IAN+1,IBN)
        ICOUNT=ICOUNT+1
      ENDIF
      IF(XERROR(IAN,IBN+1).NE.9999.0)THEN
        XLOCER=XLOCER+XERROR(IAN,IBN+1)
        ICOUNT=ICOUNT+1
      ENDIF
      if(ICOUNT.NE.0)THEN
        XLOCER=XLOCER/ICOUNT
      ELSE
        XLOCER=0.0
        IGOOD=0
      ENDIF
C
C-----Extrapolate from gradients around XERROR(IAN,IBN) to the current
C-----central position
C
      ICOUNT=0
      XLOCAD=0.0
      IF(     XERROR(IAN-1,IBN).NE.9999.0 
     1  .AND. XERROR(IAN-2,IBN).NE.9999.0)THEN
        XLOCAD=XLOCAD+(XERROR(IAN-1,IBN)-XERROR(IAN-2,IBN))
        ICOUNT=ICOUNT+1
      ENDIF
      IF(     XERROR(IAN,IBN-1).NE.9999.0 
     1  .AND. XERROR(IAN,IBN-2).NE.9999.0)THEN
        XLOCAD=XLOCAD+(XERROR(IAN,IBN-1)-XERROR(IAN,IBN-2))
        ICOUNT=ICOUNT+1
      ENDIF
      IF(     XERROR(IAN+1,IBN  ).NE.9999.0 
     1  .AND. XERROR(IAN+2,IBN  ).NE.9999.0)THEN
        XLOCAD=XLOCAD+(XERROR(IAN+1,IBN)-XERROR(IAN+2,IBN))
        ICOUNT=ICOUNT+1
      ENDIF
      IF(     XERROR(IAN  ,IBN+1).NE.9999.0 
     1  .AND. XERROR(IAN  ,IBN+2).NE.9999.0)THEN
        XLOCAD=XLOCAD+(XERROR(IAN,IBN+1)-XERROR(IAN,IBN+2))
        ICOUNT=ICOUNT+1
      ENDIF
      IF(ICOUNT.NE.0)THEN
        XLOCER=XLOCER+(XLOCAD/ICOUNT)
      ENDIF
C
C-----Calculate the average value for YERROR(IAN,IBN) from the 
C-----four neighbours
C
      ICOUNT=0
      YLOCER=0.0
C
      IF(YERROR(IAN-1,IBN).NE.9999.0)THEN
        YLOCER=YLOCER+YERROR(IAN-1,IBN)
        ICOUNT=ICOUNT+1
      ENDIF
      IF(YERROR(IAN,IBN-1).NE.9999.0)THEN
        YLOCER=YLOCER+YERROR(IAN,IBN-1)
        ICOUNT=ICOUNT+1
      ENDIF
      IF(YERROR(IAN+1,IBN).NE.9999.0)THEN
        YLOCER=YLOCER+YERROR(IAN+1,IBN)
        ICOUNT=ICOUNT+1
      ENDIF
      IF(YERROR(IAN,IBN+1).NE.9999.0)THEN
        YLOCER=YLOCER+YERROR(IAN,IBN+1)
        ICOUNT=ICOUNT+1
      ENDIF
      IF(ICOUNT.NE.0)THEN
        YLOCER=YLOCER/ICOUNT
      ELSE
        YLOCER=0.0
        IGOOD=0
      ENDIF
C
C-----Extrapolate from gradients around YERROR(IAN,IBN) to the current
C-----central position
C
      ICOUNT=0
      YLOCAD=0.0
C
      IF(     YERROR(IAN-1,IBN  ).NE.9999.0 
     1  .AND. YERROR(IAN-2,IBN  ).NE.9999.0)THEN
        YLOCAD=YLOCAD+(YERROR(IAN-1,IBN)-YERROR(IAN-2,IBN))
        ICOUNT=ICOUNT+1
      ENDIF
      IF(     YERROR(IAN  ,IBN-1).NE.9999.0 
     1  .AND. YERROR(IAN  ,IBN-2).NE.9999.0)THEN
        YLOCAD=YLOCAD+(YERROR(IAN,IBN-1)-YERROR(IAN,IBN-2))
        ICOUNT=ICOUNT+1
      ENDIF
      IF(     YERROR(IAN+1,IBN  ).NE.9999.0 
     1  .AND. YERROR(IAN+2,IBN  ).NE.9999.0)THEN
        YLOCAD=YLOCAD+(YERROR(IAN+1,IBN)-YERROR(IAN+2,IBN))
        ICOUNT=ICOUNT+1
      ENDIF
      IF(     YERROR(IAN  ,IBN+1).NE.9999.0 
     1  .AND. YERROR(IAN  ,IBN+2).NE.9999.0)THEN
        YLOCAD=YLOCAD+(YERROR(IAN,IBN+1)-YERROR(IAN,IBN+2))
        ICOUNT=ICOUNT+1
      ENDIF
      IF(ICOUNT.NE.0)THEN
        YLOCER=YLOCER+(YLOCAD/ICOUNT)
      ENDIF

C
C-----Now for the peak heights
C
      ICOUNT=0
      PEALOC=0.0
C
      IF(     XERROR(IAN-1,IBN).NE.9999.0
     1  .AND. YERROR(IAN-1,IBN).NE.9999.0)THEN
        PEALOC=PEALOC+PEAK(IAN-1,IBN)
        ICOUNT=ICOUNT+1
      ENDIF
      IF(     XERROR(IAN,IBN-1).NE.9999.0
     1  .AND. YERROR(IAN,IBN-1).NE.9999.0)THEN
        PEALOC=PEALOC+PEAK(IAN,IBN-1)
        ICOUNT=ICOUNT+1
      ENDIF
      IF(     XERROR(IAN+1,IBN).NE.9999.0
     1  .AND. YERROR(IAN+1,IBN).NE.9999.0)THEN
        PEALOC=PEALOC+PEAK(IAN+1,IBN)
        ICOUNT=ICOUNT+1
      ENDIF
      IF(     XERROR(IAN,IBN+1).NE.9999.0
     1  .AND. YERROR(IAN,IBN+1).NE.9999.0)THEN
        PEALOC=PEALOC+PEAK(IAN,IBN+1)
        ICOUNT=ICOUNT+1
      ENDIF
      IF(ICOUNT.NE.0)THEN
        PEALOC=PEALOC/ICOUNT
      ELSE
        PEALOC=0.0
      ENDIF
C
C-----Save this one in the current position
      IF(IGOOD.EQ.1)THEN
        XERROR(IAN,IBN)=XLOCER
        YERROR(IAN,IBN)=YLOCER
        PEAK(IAN,IBN)=PEALOC
      ELSE
        XERROR(IAN,IBN)=9999.0
        YERROR(IAN,IBN)=9999.0
        PEAK(IAN,IBN)=0.0
      ENDIF
C
      RETURN
C
      END
C--------------------------------------------------------------
C--------------------------------------------------------------
C--------------------------------------------------------------
      SUBROUTINE HSVTORGB(rhue,rsat,rval,rgbr,rgbg,rgbb)
C--------------------------------------------------------------
C--------------------------------------------------------------
C--------------------------------------------------------------
C
C---------Calculate RGB Values:
          c=rval * rsat
          rhue60=rhue / 60.0
          x = c * (1 - abs(mod(rhue60,2.0) - 1))
          if(rhue60<1.0)then
            rgbr=c
            rgbg=x
            rgbb=0
          elseif (rhue60<2.0)then
            rgbr=x
            rgbg=c
            rgbb=0
          elseif (rhue60<3.0)then
            rgbr=0
            rgbg=c
            rgbb=x
          elseif (rhue60<4.0)then
            rgbr=0
            rgbg=x
            rgbb=c
          elseif (rhue60<5.0)then
            rgbr=x
            rgbg=0
            rgbb=c
          else 
            rgbr=c
            rgbg=0
            rgbb=x
          endif
          rgbr=rgbr+rval-c
          rgbg=rgbg+rval-c
          rgbb=rgbb+rval-c
C          write(*,'(''c,x='',2F6.3,'' rlen,rang='',2F9.3,'' r,g,b='',3F6.3)')
C     1      c,x,rlen,rang,rgbr,rgbg,rgbb
          if(rgbr.gt.1.0)rgbr=1.0
          if(rgbg.gt.1.0)rgbg=1.0
          if(rgbb.gt.1.0)rgbb=1.0
          if(rgbr.lt.0.0)rgbr=0.0
          if(rgbg.lt.0.0)rgbg=0.0
          if(rgbb.lt.0.0)rgbb=0.0
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

