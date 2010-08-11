C     ****** REFINEORIGIN *********
C
C---- Based on AUTOINDEX.for 
C     VX3.0	HST	15-May-2003	adaptation to REFINEORIGIN
C
C
C  This program reads in a digitized electron diffraction pattern, to which 
C    background correction has already been applied.
C    It then averages the low-resolution lattice positions according to the 
C    given origin and lattice together, and then measures the central peak position.
C    The deviation of that central peak from (0,0) is the correction that has to 
C    be applied to the origin from backauto.
C
C
C     VX1.0     RH      22-Mar-1992     original program (autoindex)
C     VX1.1     RH       8-Apr-1992     various debugs
C     VX1.2     RH      13-Apr-1992     more debugs
C     VX1.3     RH      19-May-1992     CX,CY debug
C     VX1.4     RH      21-Mar-1993     default NSTEP=4
C     VX1.5     RH      16-Aug-1993     always return from SEARCH on failure
C     VX1.6     RH       1-Nov-1994     fine tuning FPBOX -> 0.25, NRMAX -> 15
C     VX1.7     RH      12-Nov-1994     more tuning FPBOX -> 0.19, FRACT -> 0.20
C     VX1.8     RH       6-Mar-1995     decrease FPBOX, FRACT till peaks found
C     VX1.9     RH      21-Sep-1995     changed over to  Alpha, plot on PLOTOUT
C     VX2.0     RH      17-Mar-1996     increase dimensions to 3000
C     VX2.1     RH      22-Mar-1996     ignore very edge of pattern (3 pixels)
C     VX2.2     RH      12-Jul-1996     checks radius near edge of average box
C     VX2.3     RH      14-Jul-1996     subroutine EXTEND, increased accuracy
C     VX2.4     RH      21-Jul-1996     improved lattice parameter refinement
C     VX2.5     RH      27-Jan-1997     increase search radius in 824 doloop
C     VX2.6     RH      29-Dec-1997     detect weaker spots on lower background
C     VX2.7     RH       4-Oct-1998     increase sigma from 2.8 to 7.8 spot det
C
C     Output, for cosmetic examination only, is average picture near an average 
C     spot, together with the assigned indexing, on OUT
C     There is also a plot file showing the selected rasters on PLOTOUT
C
C     For input, requires only the file name of the background-corrected 
C     pattern, on IN
C
	PARAMETER (ISIZE=3000, IAVER=50, NMAX=500)
	DIMENSION TITLE(20)
	DIMENSION ARRAY(ISIZE*ISIZE)
	DIMENSION ALINE(2*IAVER+1),AVE2(-IAVER:IAVER,-IAVER:IAVER)
	real*8 AVER(-IAVER:IAVER,-IAVER:IAVER)
        integer*4 NTOT
	DIMENSION IEXT(12),NXYZ(3),MXYZ(3),NXYZST(3)
	DIMENSION XC(NMAX),YC(NMAX)
	DIMENSION DT(NMAX),XT(NMAX),YT(NMAX),NT(NMAX)
	DATA NXYZST/3*0/,NL/1/
	CHARACTER*20 TEXT
C
C
        write(*,'('' input old origin:'')')
        read(*,*)X0,Y0
        write(*,'('' read: '',2F12.3)')X0,Y0
C
        write(*,'('' input old lattice:'')')
        read(*,*)X1,X2,Y1,Y2
        write(*,'('' read: '',4F12.3)')X1,X2,Y1,Y2
C
        write(*,'('' input resolution boundaries (in lattice orders):'')')
        read(*,*)iresomin,iresomax
        if(iresomin.gt.iresomax)then
          itmp=iresomin
          iresomin=iresomax
          iresomax=itmp
        endif
        if(iresomax.gt.IAVER)iresomax=IAVER
        write(*,'('' using: '',2I12)')iresomin,iresomax
C
C-------Initialize array AVER
C
	DO I=-IAVER,IAVER
	   DO J=-IAVER,IAVER
	     AVER(I,J)=0.0
           enddo
        enddo
C
	WRITE(6,1000)
1000	FORMAT(//' refineorigin ')
	CALL IMOPEN(1,'IN','OLD')
	CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      	CALL IRTLAB(1,TITLE,NL)
	CALL IMOPEN(3,'OUT','NEW')
	NX=NXYZ(1)
	NY=NXYZ(2)
C
 	NEARCEN=(NX+NY)/20  	! keep spots away from centre
        IFARCEN=(NX+NY)/5
C
	WRITE(TEXT,9997)
9997	FORMAT(' Averaged area around averaged spot')
	CALL ITRHDR(3,1)
C
	JAVER=2*IAVER+1
	NXYZ(1)=JAVER
	NXYZ(2)=JAVER
	NXYZ(3)=1
C
	CALL IALSIZ(3,NXYZ,NXYZST)
	CALL IWRHDR(3,TEXT,1,DMIN,DMAX,DMEAN)
	CALL IRTEXT(1,IEXT,1,7)
C
C-------read in 7 entries in IEXT for NSTEP, OX,OY,DX1,DY1,DX2,DY2
	CALL IRDSEC(1,ARRAY,*9999)
C
	GO TO 9990
C
9999	  WRITE(6,9998)
9998	  FORMAT(' Disk input error on IN -- IRDSEC')
	  STOP
C
9990	CONTINUE
C
C-------Now average together all the image areas centred around each spot
C
        IMAX=iresomax
        IMAX2=IMAX*IMAX
	IMIN2=iresomin*iresomin
        NTOT=0
	write(*,'(/,/,'' lattice = '',4F12.3)')X1,X2,Y1,Y2
	write(*,'('' origin  = '',2F12.3,/,/)')X0,Y0
        do ILATX = -IMAX,IMAX
          do ILATY = -IMAX,IMAX
C-----------very simple resolution limitation to circular IMAX'th-order:
            IRES=ILATX*ILATX+ILATY*ILATY
            if(IRES.ge.IMIN2 .and. IRES.le.IMAX2)then
              IXC=ILATX*X1+ILATY*Y1+X0+0.5
              IYC=ILATX*X2+ILATY*Y2+Y0+0.5
              if(IXC.gt.IAVER .and. IXC.lt.NX-IAVER .and. IYC.gt.IAVER .and. IYC.lt.NY-IAVER)then
                NTOT=NTOT+1
C	        write(*,'('' Averaging position '',5I10)')NTOT,ILATX,ILATY,IXC,IYC
                DO IY=-IAVER,IAVER
	          INDoffset=(IYC+IY-1)*NX
                  DO IX=-IAVER,IAVER
	            IXA=IXC+IX
	            INDEX=IXA+INDoffset
	            AVER(IX,IY)=AVER(IX,IY)+ARRAY(INDEX)
                  enddo
                enddo
              endif
            endif
          enddo
        enddo
C
	DO IX=-IAVER,IAVER
	  DO IY=-IAVER,IAVER
   	    AVE2(IX,IY)=AVER(IX,IY)/NTOT
          enddo
        enddo
	WRITE(6,221)int(NTOT)
221	FORMAT(' area round all ',I12,' spots now averaged')
C
	CALL ICLDEN(AVE2,JAVER,JAVER,1,JAVER,1,JAVER,D1,D2,D3)
	CALL IWRHDR(3,TEXT,-1,D1,D2,D3)
C
C  Output of averaged area around averaged spot
C
	DO JY=-IAVER,IAVER
	  DO JX=-IAVER,IAVER
   	    ALINE(JX+IAVER+1)=AVE2(JX,JY)		
          enddo
  	  CALL IWRLIN(3,ALINE)
        enddo
C
C  Now search for central peak, which should indicate the origin displacement
C
        AMAX=-9999999.9
	DO IX=-15,15
	  DO IY=-15,15
            if(AVE2(IX,IY).gt.AMAX)then
              AMAX=AVE2(IX,IY)
              IXMAX=IX
              IYMAX=IY
            endif
          enddo
        enddo
C
      	CALL IMCLOSE(1)
      	CALL IMCLOSE(3)
C
        RXMAX=IXMAX+X0
        RYMAX=IYMAX+Y0
        open(9,FILE='TMP123771.tmp',STATUS='NEW',ERR=990)
        write(9,'(2F12.4)')RXMAX,RYMAX
        write(*,'(/,/,'' New origin found: '',2F12.4)')RXMAX,RYMAX
        close(9)
        goto 999
C
 990    continue
        write(*,'('' ERROR on file open'')')
        stop 'ERROR occured'
C
 999    continue
        write(*,'('' autoindex finished normally. '')')
        stop
	END
C
C*******************************************************************************
C
