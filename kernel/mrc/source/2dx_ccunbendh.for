C PROGRAM CCUNBENDK ***********************************************************
C
C		Remember to change version number in first write statement.
C		Original pre-history versions called CCUNBEND and CCUNBENDA. 
C
C	VX 1.0	RH	21.6.84 CCUNBENDB - This went through a variety of
C				changes before version numbers were started.
C	VX 2.0	JMB	20.3.86	Debug part dealing with last strip.
C	VX 2.1	JMB	16.6.87	Rectangular images
C	VX 2.2	RH	12.9.87	Extra guide points > 2*IKNOTS per axis.
C	VX 2.3	RH	04.1.88	Extra guide points > 3*IKNOTS per axis.
C	VX 2.4	RH	29.2.88	NDATA=50000
C	VX 2.5	RH	07.4.88	ISIZEX=7500
C	VX 3.0  RH	20.8.88 Efficient guide points &  non-spline option.
C				renamed CCUNBENDC, old programs retained.
C	VX 3.1  RH     13.11.88 Optimised ISIZEX=6000,ISTEPMAX=60,IDEEP=160
C       VX 3.2  RH     18.12.89 Minor changes, mainly cosmetic.
C       VX 3.3  RH     16.5.90	IMAXCOR limit on X removed.
C       VX 3.4  RH     30.5.90  Fudge on XAPPLY check.
C	VX 3.5	RH     15.6.90  REAL*8 for SUMOUT
C	VX 4.0	RH     03.1.92	Convert for UNIX on Alliant    
C	VX 4.1	RH     23.8.93	insert check for DMIN,DMAX,DMEAN sensible.
C	VX 4.2	RH     07.9.94	increased NDIMX,NDIMY for smoother unbending
C	VX 4.3	RH     09.9.94	improved efficiency for FILLEMPTIES
C	VX 4.4	RH     13.9.94	taperedge option for single molecule work
C				renames CCUNBENDD, extra input parameters
C	VX 4.5	RH     27.1.95	change plot scale-factor, debug fillempties
C	VX 4.6	RH     29.4.95	add date to plots
C	VX 4.7	RH     25.7.95	ENCODE debug for Alpha
C	VX 4.8	RH     9.10.95	increase dimensions to 8000
C	VX 4.9	RH     23.3.96	optional O/P unbend correction table CCUNBENDE
C	VX 5.0	RH     22.4.96	add initialization and reorder IF statement
C	VX 5.1	RH     11.3.98	change distortion plot to 10x error
C	VX 6.0	RH     23.8.00	convert to plot2000 direct postscript output
C               TSH    13.6.01  P2K_FONT needed string terminator
C               HEN    5.12.08  Extention to full image unbending, instead of stripes.
C
C	MODIFIED  16.6.87  TO BE CORRECT FOR RECTANGULAR IMAGES UP TO SIZE
C	7200 X 9600
C	MODIFIED  14.12.85   TO GREATLY INCREASE THE NUMBER OF KNOTS ALLOWED IN
C	     ONLY IN THE DIRECTION PERPENDICULAR TO THE TILT AXIS.
C            WORKS IN CONJUNCTION WITH CCORSERCH OR PROFSERCH.
C            THE PROGRAM UNBENDS THE CRYSTAL USING OUTPUT
C            FROM THE CROSS-CORRELATION PEAK SEARCH PROGRAM, CCORSERCH OR
C            ITS LATER VARIANTS PROFSERCH AND QUADSERCH(0, 1, and 2)
C
C  CONTROL DATA :-
C	CARD 1 : FILE NAME OF INPUT IMAGE FILE.(ONLY HEADER READ IF IOUT=0).
C	CARD 2 : IMAXCOR,ISTEP,NNUM,ROFFSET
C	CARD 3 : EPS,FACTOR,RMAG
C	CARD 4 : PLOT TITLE FOR DISTORTION CORRECTION DISPLAY.
C      and if IOUT = 1,
C	CARD 5 : FULL FILE NAME FOR OUTPUT OF CORRECTED IMAGE.
C	CARD 6 : TITLE TO BE ADDED TO CORRECTED IMAGE TITLE RECORD.
C
C   INPUT FILES:
C         CCORDATA      - FILE OF PARAMETERS CONTAINING DETAILS OF
C                          THE CCORSERCH RUN AND 
C                       - RAW LIST OF CORRELATION PEAK POSITION AND HEIGHTS
C                          PRODUCED BY CCORSERCH.
C         PIX(NAME)     - ORIGINAL DENSITOMETER RAW IMAGE FILE.
C
C   OUTPUT FILES:
C         PIXOUT(NAME1) - THE UNBENT IMAGE FILE, FULLY CORRECTED FOR
C                          THE SMOOTHED DISTORTION CORRECTIONS.
C         TABLEOUT      - The unbending table as used inside the program - may
C                          be useful for creation of fixed distortion table,
C                          for example, to correct fibre optic distortion in
C                          in another program, e.g. pickprofa.for
C   OPTIONS ARE:
C         IMAXCOR ------- SIZE OF THE MAXIMUM ALLOWED CORRECTION
C         ISTEP --------- SIZE OF GRID ON WHICH THE INTERPOLATED VECTORS ARE
C                          CALCULATED.
C         NNUM ---------- Number of nearest vectors to be interpolated over
C         ROFFSET -------
C         EPS ----------- THRESHOLD FOR DETERMINATION OF RANK OF BICUBIC
C                          SPLINE FITTING MATRIX. TRY 0.00001 --- OTHERWISE
C                          SEE WRITE-UP FOR NAGLIB E02DAF SUBROUTINE.
C         THRESH -------- THRESHOLD OF CROSS-CORRELATION PEAK HEIGHT,
C                          CALCULATED AS;
C                    DENMAX (READ FROM CCORDATA) * FACTOR (READ FROM UNIT 5),
C                        , BELOW WHICH THE PEAK IS NOT USED.
C         RMAG ---------- Magnification for line length in distortion plots
C
C*******************************************************************************
C
C  DIMENSION STATEMENTS INCLUDE THE FOLLOWING PARAMETERS :-
C	NDATA  - MAXIMUM NUMBER OF CORRELATION PEAKS ABOVE THRESH.
C	NDIMX   - MAXIMUM NUMBER OF BLOCKS IN WHICH DISTORTION CORRECTION
C	  	 IS CALCULATED. (NUMBER OF BLOCKS = NXYZ(1)/ISTEP),
C                BOTH NXYZ(1) AND ISTEP ARE INPUT PARAMETERS.
C	NDIMY  - NXYZ(2)/ISTEP
C	NMAXKN - MAXIMUM NUMBER OF KNOTS - USABLE NUMBER (IKNOTS)=(NMAXKN-8).
C	NCMAX  -      (NMAXKN+4)**2  SOMETHING TO DO WITH THE KNOT SUBROUTINE.
C	ISIZEX  - MAXIMUM IMAGE SIZE IN X-DIMENSION
C	ISTEPMAX - MAXIMUM VALUE OF ISTEP ABLE TO BE USED - CONTROLS DIMENSION
C		   OF PICOUT.
C
      PARAMETER (NDATA=2000000)
      PARAMETER (NMAXKN=80)
      PARAMETER (NCMAX=7056)
CHENN>
C      PARAMETER (NWSPCE=1622212)
      PARAMETER (NWSPCE=410000000)
C      PARAMETER (ISIZEX=8000)
      PARAMETER (ISIZEX=20100)
CHENN<
      PARAMETER (ISTEPMAX=120)
CHENN>
C      PARAMETER (IDEEP=162)
      PARAMETER (IDEEP=220)
C      PARAMETER (NDIMX=500)
      PARAMETER (NDIMX=2000)
C      PARAMETER (NDIMY=500)
      PARAMETER (NDIMY=2000)
CHENN<
C		see subroutine - fillempties, calctaper - parameters also.
      	DIMENSION TITLE(20),TITLEIN(20),TITLEERR(20),DATANAME(20)
      	REAL*8 SUMOUT
      	REAL XPOS(NDATA),YPOS(NDATA),DX(NDATA),DY(NDATA),W(NDATA)
      	REAL DXAV(NDIMX,NDIMY),DYAV(NDIMX,NDIMY)	! Bins for guide point
      	REAL NAV(NDIMX,NDIMY),WAV(NDIMX,NDIMY)		! and linear interpol.
      	REAL XOUT(NDIMX*NDIMY),YOUT(NDIMX*NDIMY)
      	REAL XCORR(NDIMX*NDIMY),YCORR(NDIMX*NDIMY),WS(NWSPCE)
      	REAL LAMBDA(NMAXKN),MU(NMAXKN),DL(NCMAX),CX(NCMAX),CY(NCMAX)
      	INTEGER PX,PY,NCREAL,RANK,IFAIL,J
      REAL MAXCOR, TAPER(ISTEPMAX,ISTEPMAX),TAPER1(ISTEPMAX,ISTEPMAX)
      REAL PICIN(ISIZEX,IDEEP),PICOUT(ISIZEX,ISTEPMAX)
C
CHEN>
C-----This is space for the entire image:
      REAL RIMAGE(ISIZEX,ISIZEX)
      REAL rdismax2(NDATA)
      REAL ALINE(ISIZEX)
      REAL LDXF(NDATA),LDYF(NDATA)
      REAL LPOSX(NDATA),LPOSY(NDATA)
      INTEGER IPOSI(NDATA),IPOSJ(NDATA),ICLOSE(NDATA)
      INTEGER ITABLE(NDIMX,NDIMY)
CHEN<
C
        DIMENSION NXYZ(3),MXYZ(3),NXYZ1(3),NXYZST(3)
      	CHARACTER*80 NAME,NAME1
      	EQUIVALENCE (PICIN(1,1),WS(1))
       	EQUIVALENCE (PICOUT(1,1),WS(1+ISIZEX*IDEEP))
CTSH++
      CHARACTER*80 TMPTITLEIN,TMPTITLEERR
      EQUIVALENCE (TMPTITLEIN,TITLEIN),(TMPTITLEERR,TITLEERR)
CTSH--
      	DATA NXYZST/3*0/
      	DATA NGUIDE/20/		! MAXIMUM EXTRA GUIDE POINTS = NGUIDE**2.
CTSH      	DATA TITLEIN/' PLO','T OF',' INP','UT D','ATA ','ABOV','E TH',
CTSH     .	'RESH',' in ','CCUN','BEND','K   ','    ','    ','    ','    ',
CTSH     .	'    ','    ','    ','    '/
CTSH      	DATA TITLEERR/' PLO','T OF',' FIT','TING',' ERR','OR A','T IN',
CTSH     .	'PUT ','DATA',' POI','NTS ','in C','CUNB','END2','K   ','    ',
CTSH     .	'    ','    ','    ','    '/
CTSH++
      	DATA TMPTITLEIN/' PLOT OF INPUT DATA ABOVE
     . THRESH in CCUNBEND K'/
      	DATA TMPTITLEERR/' PLOT OF FITTING ERROR AT
     . INPUT DATA POINTS in CCUNBEND2K'/
CTSH--
C
	XCOORD(I,J)=A1*I+B1*J+IC
	YCOORD(I,J)=A2*I+B2*J+IR
C*** initialization added by jms 06.03.96
        do j=1,ndimy
         do i=1,ndimy
          WAV(i,j) = 0.
          NAV(i,j) = 0
          DXAV(i,j) = 0.
          DYAV(i,j) = 0.
         end do
        end do
        do j=1,istepmax
         do i=1,istepmax
          taper(i,j) = 0.
         end do
        end do
C
C  INPUT OF LIST OF ALL IMAGE PARAMETERS AS IN PRODUCED BY CCORSERCH
      WRITE(6,13211)
13211 FORMAT(/,/,' CCUNBENDK VX 6.0(23.8.00) - program to reinterpolate',
     . ' an image of a crystal on a straight lattice',/,/)
      CALL CCPDPN(3,'CCORDATA','READONLY','F',0,0)
      READ(3,13210)DATANAME
13210 FORMAT (20A4)
      WRITE(6,3210)DATANAME
      READ(3,13210)DATANAME
      WRITE(6,3210)DATANAME
      READ(3,13210)DATANAME
      WRITE(6,3210)DATANAME
C
      READ(3,*) NC,NR,IC,IR,A1,A2,B1,B2,MINA,MAXA,MINB,MAXB
      READ(3,*)DENMAX
      WRITE(6,9001)NC,NR,IC,IR,A1,A2,B1,B2,MINA,MAXA,MINB,MAXB,DENMAX
9001  FORMAT('$NC,NR ',2I5/
     $  '$IC,IR ',2I5/
     $  '$A1,A2,B1,B2 ',4F10.4/
     $  '$LAST XCOR X,Y ',4I5/
     $  '$MAX DENSITY IN CORRELATION MAP',F12.1)
C READ IN NAME OF IMAGE FILE TO BE USED.
      WRITE(6,3211)
      READ(5,163)NAME
      WRITE(6,3212)NAME
C
C  INPUT OF ALL CONTROL DATA REQUIRED TO RUN THIS PROGRAM.
      WRITE(6,160)
      READ(5,*)IMAXCOR,ISTEP,NNUM,ROFFSET
      WRITE(6,161)IMAXCOR,ISTEP,NNUM,ROFFSET
      WRITE(6,169)
      READ(5,*) EPS,FACTOR,RMAG
      MAXCOR=IMAXCOR-0.0001
      THRESH=FACTOR*DENMAX
      WRITE(6,168) EPS,FACTOR,RMAG
C
C  READ IMAGE HEADER TO CHECK THAT REQUESTED PARAMETERS AND IMAGE SIZE ARE NOT
C  TOO LARGE FOR DECLARED MAXIMUM PROGRAM DIMENSIONS (IN PARAMETER STATEMENT).
      CALL IMOPEN(1,NAME,'RO')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
CHENN>
C      IF(DMEAN.LT.0.0.OR.DMEAN.GT.5000.0
C     .   .OR.DMIN.LT.1.0.OR.DMAX.LT.1.0) THEN
      IF(DMEAN.LT.0.0.OR.DMEAN.GT.50000.0
     . .OR.DMIN.LT.0.0.OR.DMAX.LT.1.0) THEN
CHENN<
        WRITE(6,7500)
7500    FORMAT(':: ',/,'::WARNING: Image values for DMIN,DMAX or',
     .    ' DMEAN indicate possible error',/,
     .    ':: check header values of the image',/,
     .    ':: ')
CHEN>
C       STOP
CHEN<
      ENDIF
C
C  CHECK NOW THAT PROGRAM DIMENSIONS ARE ADEQUATE.
      IF(ISIZEX.LT.NXYZ(1)) THEN
        IDIMOUT=1
        GO TO 199
      END IF
      IF(ISTEP.GT.ISTEPMAX) THEN
        IDIMOUT=2
        GO TO 199
      END IF
      IF(NDIMX.LT.(NXYZ(1)/ISTEP)) THEN
        IDIMOUT=3
        GO TO 199
      END IF
      IF(NDIMY.LT.(NXYZ(2)/ISTEP)) THEN
        IDIMOUT=4
        GO TO 199
      END IF
C
C  CALCULATE COMPRESSION OF CORRELATION MAP WITH RESPECT TO REAL IMAGE
C   (eg 1,2 OR 3)
C
      ANC=NC
      ANR=NR
      COMPRSSX=NXYZ(1)/ANC
      COMPRSSY=NXYZ(2)/ANR
      WRITE(6,15000)COMPRSSX,COMPRSSY
15000 FORMAT(/,' RATIO ( NUMBER OF SAMPLES IN CORRELATION MAP)',/,
     . '          -----------------------------------',/,
     . '          ( NUMBER OF SAMPLES IN REAL IMAGE )',/,/,
     . '     COMPRESS IN X DIRECTION = ',F8.5,/,
     . '                 Y DIRECTION = ',F8.5,/,/)
      ICOMPRSSX=NINT(COMPRSSX)
      ICOMPRSSY=NINT(COMPRSSY)
        WRITE(6,14999)ICOMPRSSX,ICOMPRSSY
14999 FORMAT(' ICOMPRSSX=',I5,' ICOMPRSSY=',I5,/)
      IF(IKNOTX.GT.(NMAXKN-8)) GO TO 197
      IF(IKNOTY.GT.(NMAXKN-8)) GO TO 197
      IF(NCMAX.LT.(NMAXKN+4)**2) THEN
        IDIMOUT=6
        GO TO 199
      END IF
      NTEST=4+3*(NMAXKN+4)+2*NCMAX*(6+3*(NMAXKN+4))
      IF(NWSPCE.LT.NTEST) THEN
        IDIMOUT=7
        GO TO 199
      END IF
250   CONTINUE
C
C  READ IN CCORDATA DATA AND STORE NON-ZERO CORRELATION POSITIONS
      MDATA=0
      NLOST=0
      NOUTSIDE=0
C
      XMIN = NXYZ(1)
      XMAX = 0.0
      YMIN = NXYZ(2)
      YMAX = 0.0
      DISTMAX = 0.0
      DO 200 I=MINA,MAXA
        DO 200 J=MINB,MAXB
          READ(3,*) XCOOR,YCOOR,PEAK
          IF(XCOOR.EQ.0.0) GO TO 200
C          write(*,'('' X/YCOOR = '',2F10.1,''  PEAK = '',F10.3)')
C     .      XCOOR,YCOOR,PEAK
          IF(PEAK.LT.THRESH) THEN
            NLOST=NLOST+1
            GO TO 200
          ENDIF
          MDATA=MDATA+1
          IF(MDATA.GT.NDATA) THEN
            WRITE(6,104)MDATA
            STOP
          ENDIF
          XPOS(MDATA)=XCOORD(I,J)*ICOMPRSSX
          YPOS(MDATA)=YCOORD(I,J)*ICOMPRSSY
          IF(XPOS(MDATA).LT.1.0.OR.XPOS(MDATA).GT.NXYZ(1).OR.
     .      YPOS(MDATA).LT.1.0.OR.YPOS(MDATA).GT.NXYZ(2)) THEN
CHENN>
C           WRITE(6,276) XPOS(MDATA),YPOS(MDATA),I,J
C276        FORMAT(' !!!!!!!  -- point outside image ignored --',
C     .       'XPOS(I),YPOS(I),I,J,MDATA= ',2F10.2,3I6)
            WRITE(6,276) XPOS(MDATA),YPOS(MDATA),I,J,MDATA,XCOOR,YCOOR
276         FORMAT(' !!!!!!!  -- point outside image ignored --',
     .        'XPOS(I),YPOS(I),I,J,MDATA= ',2F10.2,3I6,2F10.2)
CHENN<
            NOUTSIDE=NOUTSIDE+1
            MDATA=MDATA-1
            GO TO 200
          ENDIF
C
          XMIN=AMIN1(XPOS(MDATA),XMIN)
          XMAX=AMAX1(XPOS(MDATA),XMAX)
          YMIN=AMIN1(YPOS(MDATA),YMIN)
          YMAX=AMAX1(YPOS(MDATA),YMAX)
C
          DX(MDATA)=(XCOOR-XCOORD(I,J))*ICOMPRSSX
          DY(MDATA)=(YCOOR-YCOORD(I,J))*ICOMPRSSY
          W(MDATA)=PEAK         ! WEIGHT PROPNL TO CORREL PEAK HEIGHT.
          IPOSI(MDATA)=I
          IPOSJ(MDATA)=J
C
          DISTCORR = SQRT(DX(MDATA)**2 + DY(MDATA)**2)
          IF(DISTCORR.GT.DISTMAX) DISTMAX = DISTCORR
200   CONTINUE
C
      CALL PLOTCORR(TITLEIN,MDATA,XPOS,YPOS,DX,DY,NXYZ,100,RMAG)
C
C
CHENN>
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  NOW APPLY THIS SMOOTHED CORRECTION TO THE IMAGE DENSITY VALUES TO PRODUCE
C  A CORRECTED IMAGE.
C
C  TRANSFER OLD HEADER TO OUTPUT FILE.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CHENN<
C
      NXYZ1(1) = NXYZ(1)
      NXYZ1(2) = NXYZ(2)
      NXYZ1(3) = 1
      NX=NXYZ(1)
      NY=NXYZ(2)
      WRITE(6,162)
      READ(5,163) NAME1
      WRITE(6,164) NAME1
      WRITE(6,165)
      READ(5,166) TITLE
      WRITE(6,167) TITLE
      CALL IMOPEN(4,NAME1,'NEW')
      CALL ITRHDR(4,1)
      CALL IALSIZ(4,NXYZ1,NXYZST)
      CALL IWRHDR(4,TITLE,1,DMIN,DMAX,DMEAN)
C
C  READ IN A STRIP OF SEVERAL LINES   (ISTEP + 2*IMAXCOR)   DEEP.
C
      NJUMPX=0
      NJUMPY=0
      NCHANGE=0
      SUMOUT=0.0
      DMINOUT=DMEAN
      DMAXOUT=DMEAN
      ICALC=0
      NX1=0
      NX2=NXYZ(1)-1
C
C####################################################################################
C####################################################################################
C####################################################################################
CHEN>
C-----Read in the entire input image
C
      write(*,'('' Reading'',I7,'' x'',I7,'' pixel image into core.'')')NX,NY
      CALL IMPOSN(1,0,0)
      do IY=1,NY
        CALL IRDLIN(1,ALINE,*901)
        do IX=1,NX
          RIMAGE(IX,IY)=ALINE(IX)
        enddo
      enddo
      CALL IMCLOSE(1)
      goto 902
C
 901  continue
        write(*,'(''::ERROR during file read in 2dx_ccunbendh'')')
        stop
 902  continue
C
C-----Interpolate the vector field onto a canonical regularly spaced (ISTEP) vector field
C-----For cosmetic output
C
      ILDNUM=1
      IPLOT=40
C
      do IY=1,NY,IPLOT
        do IX=1,NX,IPLOT
C
C---------Get coordinate for this pixel
C
C--------  XPOS(MDATA)=XCOORD(I,J)*ICOMPRSSX
C--------  YPOS(MDATA)=YCOORD(I,J)*ICOMPRSSY
C--------  DX(MDATA)=(XCOOR-XCOORD(I,J))*ICOMPRSSX
C--------  DY(MDATA)=(YCOOR-YCOORD(I,J))*ICOMPRSSY
C--------  W(MDATA)=PEAK         ! WEIGHT PROPNL TO CORREL PEAK HEIGHT.
C--------  IPOSI(MDATA)=I
C--------  IPOSJ(MDATA)=J
C
C---------Find nearest NNUM knots
C
          do N=1,NNUM
            rdismax2(N)=9.9e20
            ICLOSE(N)=0
          enddo
          do M=1,MDATA
            rdist2=(IX-XPOS(M))*(IX-XPOS(M))+(IY-YPOS(M))*(IY-YPOS(M))
            ifound=0
            do N=1,NNUM
              if(ifound.eq.0 .and. rdist2.lt.rdismax2(N))then
                do Nrun=NNUM-1,N,-1
                  rdismax2(Nrun+1)=rdismax2(Nrun)
                  ICLOSE(Nrun+1)=ICLOSE(Nrun)
                enddo
                rdismax2(N)=rdist2
                ICLOSE(N)=M
                ifound=1
              endif
            enddo
          enddo
C
C---------Calculate distortion for this pixel, based on linear distance to neighbors
C
C---------Calculate summed weighted distances, so that the NNUMth peak is weightless
C
          reps=0.001
          rdissum=0.0
          do N=1,NNUM
            IPEAK=ICLOSE(N)
		    if(W(IPEAK).gt.reps)then
		      rdissum=rdissum+W(IPEAK)/(ROFFSET+sqrt(rdismax2(N)))
            endif
C           write(*,'(I8,'' rdismax2 = '',F12.3,''  W(IPEAK) = '',F12.3)')N,rdismax2(N),W(IPEAK)
          enddo
C          write(*,'(I8,'' rdissum = '',F16.3)')M,rdissum
C
C---------Calculate distortion vector for this pixel
C
          LDX=0.0
          LDY=0.0
          RPICY=IY
          do N=1,NNUM
            IPEAK=ICLOSE(N)
            if(W(IPEAK).gt.reps)then
              LDX=LDX+DX(IPEAK)*(W(IPEAK)/(ROFFSET+sqrt(rdismax2(N))))/rdissum
              LDY=LDY+DY(IPEAK)*(W(IPEAK)/(ROFFSET+sqrt(rdismax2(N))))/rdissum
            endif
          enddo
C
C---------Store this vector for later usage
C
          LDXF(ILDNUM)=LDX
          LDYF(ILDNUM)=LDY
          LPOSX(ILDNUM)=IX
          LPOSY(ILDNUM)=IY
C
          ILDNX=IX/IPLOT
          ILDNY=IY/IPLOT
          if(ILDNX.ge.NDIMX .or. ILDNY.ge.NDIMY)then
            write(*,'('':: '')')
            write(*,'(''::ERROR: dimensions too small. Increase NDIMX,NDIMY (currently '',I8,'','',I8,'')'')')
     .        NDIMX,NDIMY
            write(*,'(''::Required would be '',I8,'','',I8)')NX/IPLOT,NY/IPLOT
            write(*,'('':: '')')
            ILDNUM=ILDNUM-1
            stop
          endif
          ITABLE(ILDNX,ILDNY)=ILDNUM
C
          ILDNUM=ILDNUM+1
          if(ILDNUM.ge.NDATA)then
            write(*,'('':: '')')
            write(*,'(''::ERROR: dimensions too small. Increase NDATA (currently '',I8,'')'')')NDATA
            write(*,'(''::Required would be '',I8)')NX/IPLOT*NY/IPLOT
            write(*,'('':: '')')
            ILDNUM=ILDNUM-1
            stop
          endif
C
        enddo
      enddo
C
C=================================================================================
C
C-----Write out the re-interpolated vector field
C
      write(TMPTITLEIN,'(''Coarse display of re-interpolated vector field'')')
      CALL PLOTCORR(TITLEIN,ILDNUM,LPOSX,LPOSY,LDXF,LDYF,NXYZ,1,RMAG)
C
C=================================================================================
C
C-----Interpolate the vector field onto a canonical regularly spaced (ISTEP) vector field
C-----For the actually used field:
C
      ILDNUM=1
C
      do IY=1,NY,ISTEP
        do IX=1,NX,ISTEP
C
C---------Get coordinate for this pixel
C
C--------  DX(MDATA)=(XCOOR-XCOORD(I,J))*ICOMPRSSX
C--------  DY(MDATA)=(YCOOR-YCOORD(I,J))*ICOMPRSSY
C--------  W(MDATA)=PEAK         ! WEIGHT PROPNL TO CORREL PEAK HEIGHT.
C--------  IPOSI(MDATA)=I
C--------  IPOSJ(MDATA)=J
C
C---------Find nearest NNUM knots
C
          do N=1,NNUM
            rdismax2(N)=9.9e20
            ICLOSE(N)=0
          enddo
          do M=1,MDATA
            rdist2=(IX-XPOS(M))*(IX-XPOS(M))+(IY-YPOS(M))*(IY-YPOS(M))
            ifound=0
            do N=1,NNUM
              if(ifound.eq.0 .and. rdist2.lt.rdismax2(N))then
                do Nrun=NNUM-1,N,-1
                  rdismax2(Nrun+1)=rdismax2(Nrun)
                  ICLOSE(Nrun+1)=ICLOSE(Nrun)
                enddo
                rdismax2(N)=rdist2
                ICLOSE(N)=M
                ifound=1
              endif
            enddo
          enddo
C
C---------Calculate distortion for this pixel, based on linear distance to neighbors
C
C---------Calculate summed weighted distances, so that the NNUMth peak is weightless
C
          rdissum=0.0
          do N=1,NNUM
            IPEAK=ICLOSE(N)
            if(W(IPEAK).gt.reps)then
              rdissum=rdissum+W(IPEAK)/(ROFFSET+sqrt(rdismax2(N)))
            endif
C           write(*,'(I8,'' rdismax2 = '',F12.3,''  W(IPEAK) = '',F12.3)')N,rdismax2(N),W(IPEAK)
          enddo
C         write(*,'(I8,'' rdissum = '',F16.3)')M,rdissum
C
C---------Calculate distortion vector for this pixel
C
          LDX=0.0
          LDY=0.0
          RPICY=IY
          do N=1,NNUM
            IPEAK=ICLOSE(N)
            if(W(IPEAK).gt.reps)then
              LDX=LDX+DX(IPEAK)*(W(IPEAK)/(ROFFSET+sqrt(rdismax2(N))))/rdissum
              LDY=LDY+DY(IPEAK)*(W(IPEAK)/(ROFFSET+sqrt(rdismax2(N))))/rdissum
            endif
          enddo
C
C---------Store this vector for later usage
C
          LDXF(ILDNUM)=LDX
          LDYF(ILDNUM)=LDY
          LPOSX(ILDNUM)=IX
          LPOSY(ILDNUM)=IY
C
          ILDNX=IX/ISTEP
          ILDNY=IY/ISTEP
          if(ILDNX.ge.NDIMX .or. ILDNY.ge.NDIMY)then
            write(*,'('':: '')')
            write(*,'(''::ERROR: dimensions too small. Increase NDIMX,NDIMY (currently '',I8,'','',I8,'')'')')
     .        NDIMX,NDIMY
            write(*,'(''::Required would be '',I8,'','',I8)')NX/ISTEP,NY/ISTEP
            write(*,'('':: '')')
            ILDNUM=ILDNUM-1
            stop
          endif
          ITABLE(ILDNX,ILDNY)=ILDNUM
C
          ILDNUM=ILDNUM+1
          if(ILDNUM.ge.NDATA)then
            write(*,'('':: '')')
            write(*,'(''::ERROR: dimensions too small. Increase NDATA (currently '',I8,'')'')')NDATA
            write(*,'(''::Required would be '',I8)')NX/ISTEP*NY/ISTEP
            write(*,'('':: '')')
            ILDNUM=ILDNUM-1
            stop
          endif
C
          ITABLE(ILDNX,ILDNY+1)=ITABLE(ILDNX,ILDNY)
        enddo
        ITABLE(ILDNX+1,ILDNY+1)=ITABLE(ILDNX,ILDNY)
      enddo
C
C=================================================================================
C
C-----Write out the re-interpolated vector field
C
      write(TMPTITLEIN,'(''Actually used re-interpolated vector field'')')
      CALL PLOTCORR(TITLEIN,ILDNUM,LPOSX,LPOSY,LDXF,LDYF,NXYZ,1,RMAG)
C
C=================================================================================
C
C-----Write out the image after distortion correction
      write(*,'('' Writing out undistorted image'')')
C
      do IY=1,NY
        do IX=1,NX
C
C---------Get coordinate for this pixel
C
C--------  DX(MDATA)=(XCOOR-XCOORD(I,J))*ICOMPRSSX
C--------  DY(MDATA)=(YCOOR-YCOORD(I,J))*ICOMPRSSY
C--------  W(MDATA)=PEAK         ! WEIGHT PROPNL TO CORREL PEAK HEIGHT.
C--------  IPOSI(MDATA)=I
C--------  IPOSJ(MDATA)=J
C
C---------Calculate distortion for this pixel, based on linear distance to neighbors
C      
          RPICX=IX
          RPICY=IY
          ILDNX=IX/ISTEP
          ILDNY=IY/ISTEP
          ILDNUM00=ITABLE(ILDNX  ,ILDNY  )
          ILDNUM10=ITABLE(ILDNX+1,ILDNY  )
          ILDNUM01=ITABLE(ILDNX  ,ILDNY+1)
          ILDNUM11=ITABLE(ILDNX+1,ILDNY+1)

          XDELTA=REAL(IX)/REAL(ISTEP)-REAL(ILDNX)
          YDELTA=REAL(IY)/REAL(ISTEP)-REAL(ILDNY)
C
C---------Interpolate only using three nearest neighbors:
C
          if(XDELTA+YDELTA.lt.1.0)then
            RPICX=RPICX
     .        +LDXF(ILDNUM00)*(1.0-XDELTA)
     .        +LDXF(ILDNUM10)*(XDELTA)
            RPICY=RPICY
     .        +LDYF(ILDNUM00)*(1.0-YDELTA)
     .        +LDYF(ILDNUM01)*(YDELTA)
          else
            RPICX=RPICX
     .        +LDXF(ILDNUM01)*(1.0-XDELTA)
     .        +LDXF(ILDNUM11)*(XDELTA)
            RPICY=RPICY
     .        +LDYF(ILDNUM10)*(1.0-YDELTA)
     .        +LDYF(ILDNUM11)*(YDELTA)
          endif
C
C          write(*,'(2I8,'' RPICX,Y = '',2F12.3)')IX,IY,RPICX,RPICY
C
C---------Write out with linear interpolation
C
          IRPICX=INT(RPICX)
          IRPICY=INT(RPICY)
C
          if(IRPICX.gt.1 .and. IRPICX.lt.NX-1 .and. IRPICY.gt.1 .and. IRPICY.lt.NY-1)then
            XDELTA=RPICX-IRPICX
            YDELTA=RPICY-IRPICY
            RPIX=
     .          RIMAGE(IRPICX  ,IRPICY  ) * (1.0 - XDELTA) * (1.0 - YDELTA)
     .        + RIMAGE(IRPICX+1,IRPICY  ) * (      XDELTA) * (1.0 - YDELTA)
     .        + RIMAGE(IRPICX  ,IRPICY+1) * (1.0 - XDELTA) * (      YDELTA)
     .        + RIMAGE(IRPICX+1,IRPICY+1) * (      XDELTA) * (      YDELTA)
          else
            RPIX=DMEAN
          endif
C
          SUMOUT=SUMOUT+RPIX
          if(DMINOUT.gt.RPIX)then
            DMINOUT=RPIX
            IXMINOUT=IX
            IYMINOUT=IY
          endif
          if(DMAXOUT.lt.RPIX)then
            DMAXOUT=RPIX
          endif
          ALINE(IX)=RPIX
C
        enddo
        CALL IWRLIN(4,ALINE)
        if(MOD(IY,250).eq.0)then
          write(*,'('' Written line '',I8)')IY
        endif
      enddo
      DMEANOUT=SUMOUT/(NX*NY)
      WRITE(6,20001)DMINOUT,DMAXOUT,DMEANOUT
20001 FORMAT(/' OUTPUT DENSITIES; MIN=',E10.4,' MAX=',E10.4,
     . ' MEAN=',E10.4/)
      WRITE(6,20003)IXMINOUT,IYMINOUT
20003 FORMAT(' COORDS OF MIN DENSITY',2I10/)
      CALL IWRHDR(4,TITLE,-1,DMINOUT,DMAXOUT,DMEANOUT)
      CALL IMCLOSE(4)
C
CHEN<
C####################################################################################
C####################################################################################
C####################################################################################
C
C
      STOP
9500    WRITE(6,9012)NX1,NX2,NY1,NY2,ISIZEX,IDEEP
9012	FORMAT(' Error reading picture file from IRDPAS'/
     . '   Trying to read a strip from',I6,' to',I6,' in X'/
     . '                      and from',I6,' to',I6,' in Y'/
     . ' into array PICIN of dimension',I6,' by',I6)
      	STOP
197	WRITE(6,172) NMAXKN-8
      	STOP
198	WRITE(6,171)
      	STOP
199	WRITE(6,170)IDIMOUT
        STOP
100	FORMAT(' NUMBER OF CROSS-CORRELATION PEAKS READ IN; MDATA',I10/
     . ' NUMBER OF PEAKS BELOW THRESHOLD NOT READ IN; NLOST',I10/
     . ' MAXIMUM DISTORTION CORRECTION READ IN; DISTMAX',F20.1/
     . ' NUMBER OF POINTS NOT READ IN BECAUSE THEY WOULD BE OUTSIDE',
     . ' IMAGE AFTER UNBENDING; NOUTSIDE',I10)
101	FORMAT(' FAILED KNOT SORTING E02ZAF, IFAIL=',I5)
102	FORMAT(' CUBIC SPLINE FIT OF X-ERROR FAILED, IFAIL=',I5)
103	FORMAT(' CUBIC SPLINE FIT OF Y-ERROR FAILED, IFAIL=',I5)
104	FORMAT(' NDATA PROGRAM DIMENSION TOO SMALL',I10)
105	FORMAT(' TOTAL OF MDATA AFTER ADDING EDGE AND GUIDE POINTS',I10/
     . '          NUMBER OF EDGE AND GUIDE POINTS ADDED   ',I10)
106	FORMAT(' X,Y,DX,DY,W   ',5F10.2)
107	FORMAT(' NUMBER OF CORRECTION VECTORS TO CALCULATE',2I10)
108	FORMAT(' Min number data points in ISTEPxISTEP box - below',
     . ' which a guide point will be added',I10)
150	FORMAT(/' SUM OF SQUARES OF RESIDUAL AT DATA POINTS=',G20.2)
151	FORMAT(' RANK OF SYSTEM    =',I10/
     . ' VALUE OF NCREAL USED  =',I10/
     . ' VALUE OF EPS USED =',F15.3)
159	FORMAT(': IMAXCOR ON INPUT =',I6,
     . '  , REDUCED TO BE EQUAL TO ISTEP =',I6)
160	FORMAT('$ IMAXCOR,ISTEP ?')
161	FORMAT('               IMAXCOR--------',I5,/,
     . '               ISTEP----------',I5,/,
     . '               NNUM-----------',I5,/,
     . '               ROFFSET--------',F12.1)
162	FORMAT('$ FULL FILENAME OF OUTPUT IMAGE FILE ?')
163	FORMAT(A)
164	FORMAT(' IMAGE FILE CREATED WAS  ',A)
165	FORMAT('$ TITLE TO ADD TO OUTPUT IMAGE FILE ?')
166	FORMAT(20A4)
167	FORMAT(' TITLE ADDED TO IMAGE FILE WAS  ',20A4)
168	FORMAT('               EPS------------',F10.6,/,
     . '               FACTOR---------',F10.3,/,
     . '               RMAG-----------',F10.1)
169	FORMAT('$ EPS,FACTOR,RMAG ?')
170	FORMAT(I5,' PROGRAM DIMENSIONS INADEQUATE, PLEASE CHECK',/,
     . ' 1=ISIZEX;2=ISTEP;3=NDIMX;4=NDIMY;',
     . ' 6=NCMAX;,7=NWSPCE',/)
171	FORMAT(' PROGRAM DIMENSION IDEEP INADEQUATE, PLEASE CHECK') 
172	FORMAT(' PRESENT PROG DIM INADEQUATE: MAX KNOTS ALLOWED=',I5)
191	FORMAT(' ERROR *** IXA OUTSIDE RANGE 1 - ISIZEX',3I14,F15.6)
192	FORMAT(' ERROR *** IYA OUTSIDE RANGE 1 - ITEST',4I14,F15.6)
193	FORMAT(' I,J,IX,IY,IXTRUE,IYTRUE=',12I6)
194	FORMAT(' T,XDELTA,YDELTA,XAPPLY,YAPPLY=',5F15.4)
195	FORMAT(' INTERPOLATION DONE, THIS STRIP NOW OUTPUT, J=',I6,
     . '  INOUT=',I5)
1101	FORMAT(' FAILED KNOT SORTING IN OUTPUT E02ZAF, IFAIL=',I5)
1102	FORMAT(' CUBIC SPLINE CALCULATION OF X-ERROR, IFAIL=',I5)
1103	FORMAT(' CUBIC SPLINE CALCULATION OF Y-ERROR, IFAIL=',I5)
1105	FORMAT(' I,XPOS(I),YPOS(I),K,XOUT(K),YOUT(K)=',2(I10,2F10.2))
1106	FORMAT('  BELOW ARE ANY CORR PTS MORE THAN',F7.1,' FROM DATA')
3210	FORMAT(' TITLE FROM CCORDATA FILE; ',20A4)
3211	FORMAT(' FULL FILENAME OF INPUT IMAGE FILE? ')
3212	FORMAT('  FILENAME OF IMAGE FILE TO BE USED ',A)
	END
C
C*******************************************************************************
C
C  SUBROUTINE TO PLOT THE LATTICE OF FITTED DISTORTION CORRECTIONS.
      	SUBROUTINE PLOTCORR(TITLE,MCALC,XOUT,YOUT,XCORR,YCORR,NXYZ,IP,
     .   RMAG)
      	DIMENSION XOUT(1),YOUT(1),XCORR(1),YCORR(1)
      	DIMENSION TITLE(20),NXYZ(3),TITLEPLOT(20),TEXT(20)
        CHARACTER DAT*24
CTSH++
	CHARACTER*80 TMPTITLEPLOT
	EQUIVALENCE (TMPTITLEPLOT,TITLEPLOT)
CTSH--
CHEN>
        DIMENSION line(20)
	CHARACTER*80 TMPline
	EQUIVALENCE (TMPline,line)
C
C-------Magnification (Exaggeration) factor 10 times:
C        RMAG = 10.0
CHEN<

        CALL FDATE(DAT)
        WRITE(6,1502) DAT(5:24)
1502    FORMAT('  Date from fdate ----  ',A20)
CTSH    WRITE(TITLEPLOT) (TITLE(J),J=1,15),DAT(5:24,1501)
CTSH++
        WRITE(TMPTITLEPLOT,1501) (TITLE(J),J=1,15),DAT(5:24)
CTSH--
1501    FORMAT(15A4,A20)
CHEN>
        WRITE(TMPline,'('' Vectors are plotted '',F4.1,'' times '',
     .    ''elongated.'')') RMAG
CHEN<
200	FORMAT('  ENTERING PLOTCORR')
201	FORMAT(20A4)
       WRITE(6,200)
       WRITE(6,201)TITLEPLOT
      PLTSIZ=260.0
      FONTSIZE=3.6
      IF(IP.EQ.100)CALL P2K_OUTFILE('CCPLOT.PS',9)
      CALL P2K_HOME
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
      CALL P2K_GRID(0.5*PLTSIZ,0.5*PLTSIZ,1.0)
      CALL P2K_ORIGIN(-0.5*PLTSIZ,-0.7*PLTSIZ,0.)
      CALL P2K_COLOUR(0)
      	SPLOT=PLTSIZ/MAX(NXYZ(1),NXYZ(2))
C  BOX ROUND THE WHOLE IMAGE AREA
      		SIZEX=NXYZ(1)*SPLOT
      		SIZEY=NXYZ(2)*SPLOT
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_DRAW(SIZEX,0.,0.)
      CALL P2K_DRAW(SIZEX,SIZEY,0.)
      CALL P2K_DRAW(0.,SIZEY,0.)
      CALL P2K_DRAW(0.,0.,0.)
C NOW PLOT EACH ERROR VECTOR STARTING WITH A CROSS AT PROPER POSITION.
C  CROSS TEMPORARILY DISABLED 16.8.84
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*0.5)
      	DO 100 I=1,MCALC
	XPLOT=SPLOT*XOUT(I)
      	YPLOT=SPLOT*YOUT(I)
      	IF(XPLOT.GT.SIZEX.OR.XPLOT.LT.0.) GO TO 100
      	IF(YPLOT.GT.SIZEY.OR.YPLOT.LT.0.) GO TO 100
      CALL P2K_MOVE(XPLOT,YPLOT,0.)
C      	WRITE(TEXT,102)
C	CALL P2K_CSTRING(TEXT,1,0.)
102	FORMAT('X')
      		XERR=SPLOT*XCORR(I)*RMAG + XPLOT
      		YERR=SPLOT*YCORR(I)*RMAG + YPLOT
      CALL P2K_MOVE(XPLOT,YPLOT,0.)
      	IF(XERR.GT.SIZEX.OR.XERR.LT.0.) GO TO 100
      	IF(YERR.GT.SIZEY.OR.YERR.LT.0.) GO TO 100
      CALL P2K_DRAW(XERR,YERR,0.)
100	CONTINUE
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
      CALL P2K_MOVE(10.,SIZEY+15.0,0.)
      CALL P2K_STRING(TITLEPLOT,80,0.)
      CALL P2K_MOVE(10.,SIZEY+5.0,0.)
      CALL P2K_STRING(line,80,0.)
      		CALL P2K_PAGE
      	RETURN
      	END
C
C*******************************************************************************
C
      SUBROUTINE ROTATEXY(N,DX,DY,ANGLE)
      DIMENSION DX(1),DY(1)
      PI=3.1415926
      C=COS(ANGLE*PI/180.0)
      S=SIN(ANGLE*PI/180.0)
      DO 100 I = 1,N
      	XNEW =  DX(I)*C + DY(I)*S
      	YNEW = -DX(I)*S + DY(I)*C
      DX(I) = XNEW
100   DY(I) = YNEW
      RETURN
      END
C
C*******************************************************************************
C
      SUBROUTINE FILLEMPTIES(MXDIM,MYDIM,DXAV,DYAV,WAV,NAV)
CHENN>
C        PARAMETER (NDIMX=500)
C        PARAMETER (NDIMY=500)
      	PARAMETER (NDIMX=2000)
        PARAMETER (NDIMY=2000)
CHENN<
      	REAL DXAV(NDIMX,NDIMY),DYAV(NDIMX,NDIMY)	! Bins for guide point
      	REAL NAV(NDIMX,NDIMY),WAV(NDIMX,NDIMY)		! and linear interpol.
      		NZEROES=0
      		DO 4000 I=1,MXDIM
      		DO 4000 J=1,MYDIM
      			IF(NAV(I,J).EQ.0) NZEROES=NZEROES+1
4000		CONTINUE
      		PROPZERO=FLOAT(NZEROES)/FLOAT(MXDIM*MYDIM)
      		RATIO=SQRT(1.0/PROPZERO)
      		IR=RATIO + 0.5
      		NPUTIN=0
      DO 6000 ICYCLE=1,12
      		ISTEP=2**(ICYCLE-1)
      		IRC=IR*ISTEP
      		DSTART=2.0*IRC**2
      	DO 4300 I=1,MXDIM
      	DO 4300 J=1,MYDIM
      	IF(NAV(I,J).EQ.0) THEN	! PUT IN NEAREST REAL CORRECTION
      		DSQMIN=DSTART
      		IDONE=0
      		DO 4200 IS=I-IRC,I+IRC,ISTEP  ! inefficient but fairly simple
      		DO 4200 JS=J-IRC,J+IRC,ISTEP
C      			IF(NAV(IS,JS).LE.0.OR.IS.LE.0.OR.JS.LE.0.OR.
C     .				IS.GT.MXDIM.OR.JS.GT.MYDIM) GO TO 4200
CHENN>
C      			IF(IS.LE.0.OR.JS.LE.0.OR.IS.GT.MXDIM.OR.
C     *				JS.GT.MYDIM.OR.NAV(IS,JS).LE.0.) GO TO 4200
                        IF(IS.LE.0.OR.JS.LE.0.OR.IS.GT.MXDIM) GO TO 4200
                        IF(JS.GT.MYDIM) GO TO 4200
                        IF(NAV(IS,JS).LE.0.) GO TO 4200
CHENN<
      			DSQ=(IS-I)**2+(JS-J)**2
      			IF(DSQ.LT.DSQMIN) THEN
      				IDONE=1
      				DSQMIN=DSQ
      				DXUSE=DXAV(IS,JS)
      				DYUSE=DYAV(IS,JS)
      				WUSE=WAV(IS,JS)/(1.0+DSQ)	! w. low weight.
      			ENDIF
4200		CONTINUE
      		IF(IDONE.EQ.1) THEN
      			NPUTIN=NPUTIN+1
      			DXAV(I,J)=DXUSE
      			DYAV(I,J)=DYUSE
      			WAV(I,J) =WUSE
      			NAV(I,J) = -1		! distinguish new vectors from old
      		ENDIF
      	ENDIF
4300	CONTINUE
      		DO 4310 I=1,MXDIM
      		DO 4310 J=1,MYDIM
      			IF(NAV(I,J).LT.0) NAV(I,J)=-NAV(I,J)	! remove distinction
4310		CONTINUE
      		WRITE(6,4301) MXDIM*MYDIM,NZEROES,ICYCLE,
     . NPUTIN,IRC,NZEROES-NPUTIN
4301		FORMAT( ' Total number of bins         ',I7/
     . ' Number of empty bins         ',I7/
     . ' Number filled in cycle',I3,' was',
     . I7,' using range',I5/
     . ' Number still unfilled       ',I7/)
      	IF(NZEROES-NPUTIN.EQ.0) RETURN
6000  CONTINUE
      RETURN
      END
C
C*******************************************************************************
C
C  WRITE OUT THE DISTORTION CORRECTION TABLE AS USED IN THE UNBENDING 
C   PROGRAM SO THAT IT MIGHT BE USED IN ANOTHER PROGRAM.
C
      	SUBROUTINE  WRITETABLE(TITLE,ISTEP,MXDIM,MYDIM,NXYZ,XCORR,YCORR)
      	DIMENSION XCORR(1),YCORR(1)
      	INTEGER NXYZ(3)
        CHARACTER*4 TITLE(40),TITLEOUT(40)
        INTEGER ISTEP,MXDIM,MYDIM
CHENN>
        INTEGER*8 IBIN(-1001:1001)
CHENN<
        CHARACTER DAT*24
CTSH++
	CHARACTER*80 TMPTITLEOUT
	EQUIVALENCE (TMPTITLEOUT,TITLEOUT)
CTSH--

      		CALL CCPDPN(9,'TABLEOUT','UNKNOWN','F',0,0)
                CALL FDATE(DAT)
                WRITE(6,10) DAT(5:24)
10              FORMAT('  Date from fdate ----  ',A20)
CTSH                WRITE(TITLEOUT) (TITLE(J),J=1,15),DAT(5:24,11)
CTSH++
                WRITE(TMPTITLEOUT,11) (TITLE(J),J=1,15),DAT(5:24)
CTSH--
11              FORMAT(15A4,A20)
20	FORMAT('  ENTERING WRITETABLE')
21	FORMAT(20A4)
22	FORMAT(' ISTEP,MXDIM,MYDIM,NXYZ =',6I6)
      WRITE(6,20)
      WRITE(9,21) TITLEOUT
      WRITE(9,22) ISTEP,MXDIM,MYDIM,(NXYZ(J),J=1,3)
      MCALC=MXDIM*MYDIM
C Steps along X are indexed most rapidly in arrays XCORR,YCORR
CHENN>
C      	DO 100 I=1,MCALC
C100   		WRITE(9,30) XCORR(I),YCORR(I)
C30		FORMAT(2F8.2)
C
C        WRITE(6,*) XCORR(I),YCORR(I)
C        WRITE(9,*) XCORR(I),YCORR(I)
C          WRITE(6,30) XCORR(I)           ,YCORR(I),
C     1                XCORR(I)-XCORR(I-1),YCORR(I)-YCORR(I-1)
C           
C          WRITE(9,30) XCORR(I)           ,YCORR(I),
C     1                XCORR(I)-XCORR(I-1),YCORR(I)-YCORR(I-1)
100     continue
30      FORMAT(2F8.2,2F9.2)
CHENN<
      	CLOSE(9)
CHENN>
C
        DO I=-1000,1000
          IBIN(I)=0.0
        enddo
        do I=2,MCALC
          xdiff=XCORR(I)-XCORR(I-1)
          ydiff=YCORR(I)-YCORR(I-1)
          idiff=INT(sqrt(xdiff*xdiff+ydiff*ydiff)*10)
          if(idiff.gt. 1000)idiff= 1000
          if(idiff.lt.-1000)idiff=-1000
          IBIN(idiff)=IBIN(idiff)+1
        enddo
        do i=-1000,1000
          rval=i/10.0
          write(17,'(F9.3,I15)')rval,IBIN(i)
        enddo
C
CHENN<
      	RETURN
      	END
C
C******************************************************************************
