C**PLOTRESOLUTION*************************************************************
C
C This is reusing fragments from 2dx_PLOTRESKA.for 
C
C This prgram uses the information from the merged 3D dataset and plots
C different graphs to analyze the resolution of the dataset. 
C
C 11/01/14 Henning Stahlberg, 2dx
C
C  INPUT  IOPTION  1=normal operation
C
C         ACELL    real space lattice parameter A in angstrom
C         BCELL    real space lattice parameter B in angstrom
C         ABANG1   real space lattice parameter REALANG in degrees
C
C         RESMAX   maximal resolution of the plot which goes to the edge of the plot
C         resolutionplot_bins   Number of bins to calculate average resolution curves
C
C Dimension Statements for the plot
C
      IMPLICIT NONE
C
      REAL PLTSIZ
      INTEGER IMAX
      INTEGER IHISTBINS
      INTEGER IHKMAX
C
      PARAMETER (PLTSIZ = 300.0)
      PARAMETER (IMAX = 1000000)
      PARAMETER (IHISTBINS = 100)
      PARAMETER (IHKMAX = 400)
C
      REAL TITLE(20)
      REAL RTITLE(80)
      REAL TEXT(20)
      CHARACTER*20 TMPTEXT
      CHARACTER*200 CLINE
      CHARACTER*200 CLIN2
      CHARACTER*80 CTITLE
      CHARACTER*200 CNAME,CNAME2,CNAME3
      CHARACTER*200 CRINGS
      EQUIVALENCE (CTITLE,RTITLE)
      EQUIVALENCE (TMPTEXT,TEXT)
C
      REAL DRAD,FACTOR,pi
      REAL ACELL,BCELL,ABANG,ALAT,GAMMA
      REAL X,Y,XN,XP,YN,YP,RLENY,RLENX,RLEN
      REAL AX,AY,BX,BY
      REAL RESMAX,RecRESMAX
      REAL RESMAX3D,RESMAXVERT
      REAL RecRESMAX3D,RecRESMAXVERT
      REAL RecRESVERTMAX,RecRESHORIMAX,RecRESVALMAX
      REAL YPOSN,XPOSN,RORIX,RORIY
      REAL RORIRESMAX
      REAL FONTSIZE
      REAL RVAL,RTMP,RWGT
      REAL RLINE1,RLINE2,RLINE3
C
      REAL*8 AMP,PHASE,BCK,CTF
      INTEGER IOPTION,MDIM
      INTEGER IH,IK,IQ,IQLABEL
      INTEGER ifound,IHD,IKD
      REAL RAMP,RPHA,RSIGF,RSIGP,RDIFF,RAVAM,RZ,RZD
      REAL*8 RAMPAVE,RAMPSUM
      REAL REDUCAC,R1,R2
C
      INTEGER i,icount,k,j,ibin,ibincount,l,i2
      INTEGER IMAXBINS
      INTEGER IRUN
C
      INTEGER IPOINT(-IHKMAX:IHKMAX,-IHKMAX:IHKMAX,-IHKMAX:IHKMAX)
      INTEGER IHFIELD(IMAX)
      INTEGER IKFIELD(IMAX)
      INTEGER ILFIELD(IMAX)
      INTEGER INFIELD(IMAX)
      REAL RZFIELD(IMAX)
      REAL RAMPFIELD(IMAX)
      REAL RPHAFIELD(IMAX)
      REAL RSIGFFIELD(IMAX)
      REAL RSIGPFIELD(IMAX)
      REAL RFOM(IMAX)
      REAL RecRESHORI(IMAX)
      REAL RecRESVERT(IMAX)
      REAL RecRESVAL(IMAX)
      REAL RPHARESSUM(IMAX)
      REAL RPHARESDIV(IMAX)
C
      REAL RTMPFIELD(IMAX)
C
      REAL RSIGFFIELDMAX
      REAL RSIGPFIELDMAX
      REAL RMEDIVAL,RMEDIVAL2,RMEDIVAL3
      REAL RMEDIVALF(0:IHISTBINS)
      REAL RMEDIVAL2F(0:IHISTBINS)
      REAL RMEDIVAL3F(0:IHISTBINS)
C
      REAL*8 RBINSSIGF(0:IHISTBINS,3)
      REAL*8 RBINSSIGP(0:IHISTBINS,3)
      REAL*8 RBINSSIGPF(0:IHISTBINS,3)
      REAL*8 RBINSFOM(0:IHISTBINS,3)
C
      REAL*8 RBINSSIGF2(0:IHISTBINS,3)
      REAL*8 RBINSSIGP2(0:IHISTBINS,3)
      REAL*8 RBINSSIGPF2(0:IHISTBINS,3)
      REAL*8 RBINSFOM2(0:IHISTBINS,3)
C
      REAL*8 RBINSFOM3(0:IHISTBINS,3)
      REAL*8 RBINSFOM4(0:IHISTBINS,3)
      REAL*8 RBINSFOM5(0:IHISTBINS,3)
C 
      INTEGER IBINSSIGF(0:IHISTBINS,3)
      INTEGER IBINSSIGP(0:IHISTBINS,3)
      INTEGER IBINSSIGPF(0:IHISTBINS,3)
      INTEGER IBINSFOM(0:IHISTBINS,3)
C
      INTEGER IBINSFOM3(0:IHISTBINS,3)
C
      INTEGER   IMP
      CHARACTER NCPUS*10
      INTEGER   OMP_GET_NUM_PROCS
C
      COMMON /DATA/ IHFIELD,IKFIELD,ILFIELD,RZFIELD,
     .     RAMPFIELD,RPHAFIELD,RSIGFFIELD,RSIGPFIELD,RFOM,
     .     RecRESMAX,icount,RORIX,RORIY,
     .     RecRESHORI,RecRESVERT,RecRESVAL,
     .     RSIGFFIELDMAX,RSIGPFIELDMAX,
     .     RBINSSIGF,RBINSSIGP,RBINSSIGPF,RBINSFOM,
     .     RBINSSIGF2,RBINSSIGP2,RBINSSIGPF2,RBINSFOM2,
     .     RBINSFOM3,RBINSFOM4,
     .     RLINE1,RLINE2,RLINE3,
     .     RESMAX3D,RESMAXVERT
C
      RLINE1=0.1
      RLINE2=0.5
      RLINE3=1.3
C
C=====Input of parameters======================================================
C
      FONTSIZE=2.0      ! SELECT 4MM CHAR HEIGHT FOR TEXT
      pi=3.1415926537
      DRAD=pi/180.0

      write(6,'('': 2dx_plotresolution - analyzing the merged 3D '',
     .      ''dataset in resolution terms.'')')
C
C#ifdef _OPENMP
C      IMP=0
C      CALL GETENV('OMP_NUM_THREADS',NCPUS)
C      READ(NCPUS,*,ERR=111,END=111)IMP
C111   CONTINUE
C      IF (IMP.LE.0) THEN
C        CALL GETENV('NCPUS',NCPUS)
C        READ(NCPUS,*,ERR=112,END=112)IMP
C112     CONTINUE
C      ENDIF
C      IF (IMP.LE.0) THEN
C        IMP=OMP_GET_NUM_PROCS()
C      ENDIF
C      CALL OMP_SET_NUM_THREADS(IMP)
C#endif
C      IF (IMP.LE.0) IMP=1
CC
C      IF (IMP.GT.1) THEN
C        WRITE(*,'('': Parallel processing: NCPUS = '',I8)') IMP
C      ENDIF
C
      write(*,'('' Input operation mode: (1) latline output, '',
     .  ''(2) 2dx_process_hkz output '')')
      READ(5,*)IOPTION
      write(6,'('' Read: '',I3)')IOPTION
C
      write(6,'('' Input A,B,Z,Gamma (real space lattice)'')')
      read(5,*)ACELL,BCELL,ALAT,ABANG
      write(6,'('': Read: '',4F12.3)')ACELL,BCELL,ALAT,ABANG
C
C-----Calculate the reciprocal canonical lattice in A-1 (X-axis horizontal)
      GAMMA=180.0-ABANG
      AX=1.0/(ACELL*SIN(DRAD*GAMMA))
      AY=0
      BX=COS(DRAD*GAMMA)/(BCELL*SIN(DRAD*GAMMA))
      BY=1.0/BCELL
      write(6,'('': Reciprocal lattice: '',4G16.6)')
     .  AX,AY,BX,BY
C
      write(*,'('' Input name of input APH file'',
     .   '' (e.g. latfitteds.dat)'')')
      read(5,'(A200)')CNAME
      call shorten(CNAME,k)
      write(*,'('' Read: '',A)')CNAME(1:k)
C
      write(*,'('' Input name of output APH file'',
     .   '' (e.g. latfitteds_limit.dat)'')')
      read(5,'(A200)')CNAME2
      call shorten(CNAME2,k)
      write(*,'('' Read: '',A)')CNAME2(1:k)
C
      write(*,'('' Input name of raw lattlice line data file'',
     .   '' (e.g. latlines.dat)'')')
      read(5,'(A200)')CNAME3
      call shorten(CNAME3,k)
      write(*,'('' Read: '',A)')CNAME3(1:k)
C
      write(*,'('' Input overall resolution limit in Angstroems'')')
      READ (5,*)RESMAX3D
      if(RESMAX3D.GT.0.0)THEN
        RecRESMAX3D = 1.0/RESMAX3D
      else
        write(*,'('': WARNING: Using no resolution limit'')')
        RecRESMAX3D=1.0/0.1
      endif
C
      write(*,'('' Input vertical resolution limit in Angstroems'')')
      READ (5,*)RESMAXVERT
      if(RESMAXVERT.GT.0.0)THEN
        RecRESMAXVERT = 1.0/RESMAXVERT
      else
        write(*,'('': WARNING: Using no resolution limit'')')
        RecRESMAXVERT=1.0/0.1
      endif
C
      write(*,'('' Input resolution maximum in Angstroems for plot'')')
      READ (5,*)RESMAX
      write(6,'('' Read: '',F12.3)')RESMAX
C
      write(*,'('' Input Phase Error reduction factor (REDUCAC)'')')
      READ (5,*)REDUCAC
      write(6,'('' Read: '',F12.3)')REDUCAC
C
      write(*,'('' Input number of bins in resolution plot'')')
      READ (5,*)RTMP
      IMAXBINS=INT(RTMP)
      if(IMAXBINS.lt.15)IMAXBINS=15
      if(IMAXBINS.gt.IHISTBINS)then
        write(6,'('' WARNING: Number too high. Corrected.'')')
        IMAXBINS=IHISTBINS
      endif
      write(*,'('' Read: '',I8)')IMAXBINS
C
C-----Initialize Histogram Bins
C
      do i = 0,IMAXBINS
        do j = 1,3
          RBINSSIGF(i,j)=0.0
          RBINSSIGP(i,j)=0.0
          RBINSSIGPF(i,j)=0.0
          RBINSFOM(i,j)=0.0
C
          RBINSSIGF2(i,j)=0.0
          RBINSSIGP2(i,j)=0.0
          RBINSSIGPF2(i,j)=0.0
          RBINSFOM2(i,j)=0.0
C
          RBINSFOM3(i,j)=0.0
          RBINSFOM4(i,j)=0.0
          RBINSFOM5(i,j)=0.0
C
          IBINSSIGF(i,j)=0
          IBINSSIGP(i,j)=0
          IBINSSIGPF(i,j)=0
          IBINSFOM(i,j)=0
          IBINSFOM3(i,j)=0
        enddo
      enddo
      do i = -IHKMAX,IHKMAX
        do j = -IHKMAX,IHKMAX
          do k = -IHKMAX,IHKMAX
            IPOINT(i,j,k) = 0
          enddo
        enddo
      enddo
C
C----------------------------------------------------------------------
C=====Read input file
C----------------------------------------------------------------------
C
      open(1,FILE=CNAME,STATUS='OLD',ERR=981)
      call shorten(CNAME2,k)
      open(10,FILE=CNAME2,STATUS='NEW',ERR=982)
C
      RecRESHORIMAX=0.0
      RecRESVERTMAX=0.0
      RecRESVALMAX=0.0
      RSIGFFIELDMAX=-1.0E20
      RSIGPFIELDMAX=-1.0E20
      i = 0
      RAMPAVE = 0.0
 100  continue
        i = i + 1
C
        if(IOPTION.eq.1)then
          read(1,*,END=200,ERR=970)IHFIELD(i),IKFIELD(i),RZFIELD(i),
     .       RAMPFIELD(i),RPHAFIELD(i),RSIGFFIELD(i),RSIGPFIELD(i),
     .       RFOM(i)
          ILFIELD(i)=INT(RZFIELD(i)*ALAT)
        else
          read(1,*,END=200,ERR=970)IHFIELD(i),IKFIELD(i),ILFIELD(i),
     .       RAMPFIELD(i),RPHAFIELD(i),RFOM(i)
          RFOM(i)=RFOM(i)/100.0
          if(RFOM(i).lt.0.0)RFOM(i)=0.0
          if(RFOM(i).gt.1.0)RFOM(i)=1.0
C---------This is wrong:
          RSIGFFIELD(i)=1.0
C
          RSIGPFIELD(i)=acos(RFOM(i))*180/pi
          RZFIELD(i)=REAL(ILFIELD(i))/ALAT
        endif
        INFIELD(i)=0
        RPHARESSUM(i)=0
        RPHARESDIV(i)=0
C
        if(RFOM(i).lt.0.001)then
          i=i-1
          goto 100
        endif
C
        if(IHFIELD(i).ge.IHKMAX .or. IHFIELD(i).le.-IHKMAX .or.
     .     IKFIELD(i).ge.IHKMAX .or. IKFIELD(i).le.-IHKMAX .or.
     .     ILFIELD(i).ge.IHKMAX .or. ILFIELD(i).le.-IHKMAX) then
          write(6,'('':: ERROR in 2dx_plotresolution: i='',I8,
     .    '' IH,IK,IL = '',3I12)')i,IHFIELD(i),IKFIELD(i),ILFIELD(i)
          stop
        endif
        IPOINT(IHFIELD(i),IKFIELD(i),ILFIELD(i))=i
C
        RAMPAVE = RAMPAVE + RAMPFIELD(i)
C
        if(RSIGFFIELDMAX.lt.RSIGFFIELD(i)) RSIGFFIELDMAX=RSIGFFIELD(i)
        if(RSIGPFIELDMAX.lt.RSIGPFIELD(i)) RSIGPFIELDMAX=RSIGPFIELD(i)
C
        X=IHFIELD(i)*AX+IKFIELD(i)*BX
        Y=IHFIELD(i)*AY+IKFIELD(i)*BY
C
        RecRESHORI(i)=SQRT(X*X+Y*Y)
        RecRESVERT(i)=RZFIELD(i)       ! Is this the correct definition of zstar?
        RecRESVAL(i)=SQRT(RecRESHORI(i)*RecRESHORI(i)+RecRESVERT(i)*RecRESVERT(i))
C
        if(RecRESVAL(i).le.RecRESMAX3D .and. RecRESVERT(i).le.RecRESMAXVERT)then
          write(10,'(2I5,F10.5,4G20.8,G20.8)')IHFIELD(i),IKFIELD(i),RZFIELD(i),
     .       RAMPFIELD(i),RPHAFIELD(i),RSIGFFIELD(i),RSIGPFIELD(i),RFOM(i)
        else
          i=i-1
          goto 100
        endif
C
        if(RecRESHORIMAX.lt.RecRESHORI(i)) RecRESHORIMAX=RecRESHORI(i)
        if(RecRESVERTMAX.lt.RecRESVERT(i)) RecRESVERTMAX=RecRESVERT(i)
        if(RecRESVALMAX.lt.RecRESVAL(i)) RecRESVALMAX=RecRESVAL(i)
C
        if(i.lt.IMAX-2)then
          goto 100
        else
          write(6,'('':: ERROR in 2dx_plotresolution:  increase IMAX'')')
        endif
C
 200  continue
C
      CLOSE(1)
      CLOSE(10)
C
      RAMPAVE = RAMPAVE / i
C
      write(6,'('' Average amplitude of input file is '',F15.3)')RAMPAVE
C
      icount=i-1
      if(icount.lt.1)then
        write(6,'(''::ERROR during reading of input file'')')
        goto 999
      else
        write(6,'('': Read '',I8,'' input values.'')')icount
        close(1)
      endif
C
      if(RESMAX.GT.0.0)THEN
        RecRESMAX = 1.0/RESMAX
      else
        write(*,'('': Using resolution max from data'')')
        RecRESMAX=RecRESVALMAX
      endif
C
      write(*,'('': Reciprocal Resolution Maximum = '',F12.3)'),RecRESMAX
C
      RORIRESMAX=1.0/RecRESMAX
C
C----------------------------------------------------------------------
C-----Find median curve
C----------------------------------------------------------------------
C
      write(*,'('' Calculating Median curves for SigF, SigP, FOM'')')
C
C-----For all types of data (SigF, SigP, SigP*FOM, FOM):
      do l = 1,4
C-------For all types of resolutions (3D, horizontal, vertical):
        do j = 1,3
C---------Prepare all median values in the bins:
C!$OMP PARALLEL DO PRIVATE (ibincount,i,ibin,RTMPFIELD,RMEDIVALF,RMEDIVAL2F,RMEDIVAL3F)
          do k = 0,IMAXBINS
            ibincount=0
C-----------Find all entries for this bin, fill into RTMPFIELD:
            do i = 1,icount
              if(j.eq.1) ibin=INT(REAL(IMAXBINS)*RecRESVAL(i)/RecRESMAX+0.5)
              if(j.eq.2) ibin=INT(REAL(IMAXBINS)*RecRESHORI(i)/RecRESMAX+0.5)
              if(j.eq.3) ibin=INT(REAL(IMAXBINS)*RecRESVERT(i)/RecRESMAX+0.5)
              if(ibin.eq.k)then
C---------------if(RFOM(i).gt.0.5)then
                  ibincount=ibincount+1
                  if(l.eq.1) then
                    if(abs(RAMPFIELD(i)).gt.1E-20)then
                      RTMPFIELD(ibincount)=RSIGFFIELD(i)/RAMPFIELD(i)
                    else
                      RTMPFIELD(ibincount)=0.0
                    endif
                  endif
                  if(l.eq.2) RTMPFIELD(ibincount)=RSIGPFIELD(i)
                  if(l.eq.3) RTMPFIELD(ibincount)=RSIGPFIELD(i)*RFOM(i)
                  if(l.eq.4) RTMPFIELD(ibincount)=RFOM(i)
C---------------endif
              endif
            enddo
C-----------Find the median entry in this field:
            if(ibincount.gt.0)then
              call FINDMEDIAN(RTMPFIELD,ibincount,RMEDIVAL,RMEDIVAL2,RMEDIVAL3)
            else
              RMEDIVAL=0.0
              RMEDIVAL2=0.0
              RMEDIVAL3=0.0
            endif
            RMEDIVALF(k)=RMEDIVAL
            RMEDIVAL2F(k)=RMEDIVAL2
            RMEDIVAL3F(k)=RMEDIVAL3
          enddo
C!$OMP END PARALLEL DO
C-----------Store this median value into the curve:
          do k = 0,IMAXBINS
            if(l.eq.1) RBINSSIGF(k,j)=RMEDIVALF(k)
            if(l.eq.2) RBINSSIGP(k,j)=RMEDIVALF(k)
            if(l.eq.3) RBINSSIGPF(k,j)=RMEDIVALF(k)
            if(l.eq.4) RBINSFOM(k,j)=RMEDIVALF(k)
C
            if(l.eq.1) RBINSSIGF2(k,j)=RMEDIVAL2F(k)
            if(l.eq.2) RBINSSIGP2(k,j)=RMEDIVAL2F(k)
            if(l.eq.3) RBINSSIGPF2(k,j)=RMEDIVAL2F(k)
            if(l.eq.4) RBINSFOM2(k,j)=RMEDIVAL3F(k)
          enddo
        enddo
      enddo
C
C----------------------------------------------------------------------
C-----Find average FOM curve
C----------------------------------------------------------------------
C
      write(*,'('' Calculating average FOM curves'')')
C
      do i = 1,icount
C-------For all types of resolutions (3D, horizontal, vertical):
        do j = 1,3
          if(j.eq.1) ibin=INT(REAL(IMAXBINS)*RecRESVAL(i)/RecRESMAX+0.5)
          if(j.eq.2) ibin=INT(REAL(IMAXBINS)*RecRESHORI(i)/RecRESMAX+0.5)
          if(j.eq.3) ibin=INT(REAL(IMAXBINS)*RecRESVERT(i)/RecRESMAX+0.5)
          if(ibin.lt.0       )ibin=0
          if(ibin.gt.IMAXBINS)ibin=IMAXBINS
          RBINSFOM3(ibin,j)=RBINSFOM3(ibin,j) + RFOM(i)
          IBINSFOM3(ibin,j)=IBINSFOM3(ibin,j) + 1
        enddo
      enddo
C
C-----Prepare all values in the bins:
      do k = 0,IMAXBINS
        do j = 1,3
          if(IBINSFOM3(k,j).gt.0)then
            RBINSFOM3(k,j)=RBINSFOM3(k,j)/IBINSFOM3(k,j)
          else
            RBINSFOM3(k,j)=0.0
          endif
        enddo
      enddo
C
C----------------------------------------------------------------------
C-----Calculate Phase Residual in 3D resolution ranges:
C----------------------------------------------------------------------
C
      if(IOPTION.ne.1)then
C
        write(*,'('' Calculating Average Amplitude'')')
C
        open(9,FILE=CNAME3,STATUS='OLD',ERR=983)
C
        k = 0
        RAMPSUM = 0.0
 121    continue
          k=k+1
          read(9,*,END=122,ERR=970)IH,IK,RZ,RAMP,RPHA,RSIGF,RSIGP,IQ
          RAMPSUM = RAMPSUM + RAMP
          goto 121
 122    continue
        RAMPSUM = RAMPSUM / k
        rewind(9)
C
        R1 = 1.0 / RAMPAVE
        R2 = 1.0 / RAMPSUM
C
        write(*,'('' Calculating Phase Residual curves'')')
        k = 0
        l = 0
 131    continue
          k=k+1
          read(9,*,END=132,ERR=970)IH,IK,RZ,RAMP,RPHA,RSIGF,RSIGP,IQ
          i = IPOINT(IH,IK,int(RZ*ALAT))
          if(i.gt.0)then
C            RPHA = RPHA + 180.0 * ( IH + IK )
 133        continue
            if ( RPHA.gt.360.0 ) then
              RPHA = RPHA - 360.0
              goto 133
            endif               
            RDIFF = abs(RPHAFIELD(i)-RPHA)
            if(RDIFF.gt. 360.0)RDIFF=abs(RDIFF-360.0)
            if(RDIFF.gt. 360.0)RDIFF=abs(RDIFF-360.0)
            if(RDIFF.gt. 180.0)RDIFF=360.0-RDIFF
            RAVAM = (abs(RAMPFIELD(i)*R1)+abs(RAMP*R2))/2.0
C            RAVAM = abs(RAMPFIELD(i)/RAMPAVE)
            RWGT  = RSIGP*REDUCAC
            if(RWGT.lt. 0.0)RWGT= 0.0
            if(RWGT.gt.90.0)RWGT=90.0
            RWGT  = cos(DRAD*RWGT)
C
            RPHARESSUM(i)=RPHARESSUM(i) + RDIFF*RDIFF*RAVAM*RWGT
            RPHARESDIV(i)=RPHARESDIV(i) +             RAVAM*RWGT
C            write(6,'('' H,K,Z, Pha1F,Pha2, RDIFF = '',2I8,4F15.3)')
C     .        IH,IK,RZ,RPHAFIELD(i),RPHA,RDIFF
          else
            l = l + 1
          endif
          goto 131
 132    continue
C
        write(*,'('' Read      '',I8,'' reflections for raw data for '',
     .     ''DPR calculation.'')')k
        write(*,'('' Of these, '',I8,'' reflections were not found '',
     .     ''in perged reference dataset.'')')l
C
        do i = 1,icount
          if(RPHARESDIV(i).gt.0.0001)then
            RPHARESSUM(i)=SQRT(RPHARESSUM(i)/RPHARESDIV(i))
          else
            RPHARESSUM(i)=0.0
          endif
        enddo
C
        close(9)
C
C!$OMP PARALLEL DO PRIVATE (j,ibin,k)
        do i = 1,icount
C---------For all types of resolutions (3D, horizontal, vertical):
          do j = 1,3
            if(j.eq.1) ibin=INT(REAL(IMAXBINS)*RecRESVAL(i)/RecRESMAX+0.5)
            if(j.eq.2) ibin=INT(REAL(IMAXBINS)*RecRESHORI(i)/RecRESMAX+0.5)
            if(j.eq.3) ibin=INT(REAL(IMAXBINS)*RecRESVERT(i)/RecRESMAX+0.5)
            if(ibin.lt.0       )ibin=0
            if(ibin.gt.IMAXBINS)ibin=IMAXBINS
            if(RPHARESSUM(i).gt.0.001)then
              RBINSFOM4(ibin,j)=RBINSFOM4(ibin,j) + RPHARESSUM(i)
              RBINSFOM5(ibin,j)=RBINSFOM5(ibin,j) + 1.0
            endif
          enddo
C          if(RPHARESSUM(i).gt.0.001)then
C            write(6,'(2I8,'' RPHARES = '',3F15.3)')k,ibin,RPHARESSUM(i),RBINSFOM4(ibin,1),RBINSFOM5(ibin,1)
C          endif
        enddo
C!$OMP END PARALLEL DO
C
C----  -Prepare all values in the bins:
        do k = 0,IMAXBINS
          do j = 1,3
            if(RBINSFOM5(k,j).gt.0.001)then
              RBINSFOM4(k,j)=RBINSFOM4(k,j)/RBINSFOM5(k,j)
C              if(j.eq.1)then
C                write(6,'(I8,'' nom,div = '',2F15.3)')k,RBINSFOM4(k,j),RBINSFOM5(k,j)
C              endif
            else
              RBINSFOM4(k,j)=1000.0
            endif
          enddo
        enddo
      endif
C
C=====Plot initialization======================================================
C
      WRITE(6,'('' Plotting data'')')
C
      CALL P2K_OUTFILE('PLOTCUR.PS',10)
      CALL P2K_HOME
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
      CALL P2K_GRID(0.5*PLTSIZ,0.5*PLTSIZ,1.0)
      CALL P2K_ORIGIN(-0.5*PLTSIZ,-0.7*PLTSIZ,0.)
      CALL P2K_COLOUR(0)
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_ORIGIN(0.,0.,0.)
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_ORIGIN(10.,0.,0.)
C
      if(IOPTION.eq.1)then
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,14,IMAXBINS)
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,13,IMAXBINS)
C
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,1,IMAXBINS)
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,4,IMAXBINS)
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,7,IMAXBINS)
C
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,2,IMAXBINS)
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,5,IMAXBINS)
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,8,IMAXBINS)
C
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,3,IMAXBINS)
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,6,IMAXBINS)
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,9,IMAXBINS)
C
C       Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,10,IMAXBINS)
C       Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,11,IMAXBINS)
C       Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,12,IMAXBINS)
      else
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,18,IMAXBINS)
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,19,IMAXBINS)
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,20,IMAXBINS)
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,15,IMAXBINS)
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,16,IMAXBINS)
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,17,IMAXBINS)
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,13,IMAXBINS)
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,1,IMAXBINS)
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,2,IMAXBINS)
        Call PLOTIT(PLTSIZ,FONTSIZE,CNAME,3,IMAXBINS)
      endif
C
      goto 999
C
 970  continue
        write(6,'(''::ERRPOR during file read in 2dx_plotresolution'')')
        goto 999
C
 981  continue
        write(6,'(''::ERRPOR during file open in 2dx_plotresolution'')')
        call shorten(CNAME,k)
        write(6,'(''::for file '',A)')CNAME(1:k)
        goto 999
C
 982  continue
        write(6,'(''::ERRPOR during file open in 2dx_plotresolution'')')
        call shorten(CNAME2,k)
        write(6,'(''::for file '',A)')CNAME2(1:k)
        goto 999
C
 983  continue
        write(6,'(''::ERRPOR during file open in 2dx_plotresolution'')')
        call shorten(CNAME3,k)
        write(6,'(''::for file '',A)')CNAME3(1:k)
        goto 999
C
 999  continue
C
      STOP
      END
C
C==============================================================================
C==============================================================================
C==============================================================================
C
      SUBROUTINE FINDMEDIAN(RTMPFIELD,ibincount,RMEDIVAL,RMEDIVAL2,RMEDIVAL3)
C
      IMPLICIT NONE
C
      INTEGER IMAX
      PARAMETER (IMAX = 1000000)
C
      REAL RTMPFIELD(IMAX)
      INTEGER ibincount
      REAL RMEDIVAL,RMEDIVAL2,RMEDIVAL3
C
      INTEGER i,j
      REAL RTMP
C
      if(ibincount.eq.0)then
        RMEDIVAL=0.0
        RMEDIVAL2=0.0
        RMEDIVAL3=0.0
        RETURN
      endif
C
      if(ibincount.eq.1)then
        RMEDIVAL=RTMPFIELD(1)
        RMEDIVAL2=RTMPFIELD(1)
        RMEDIVAL3=RTMPFIELD(1)
        RETURN
      endif
C
C-----Bubblesort:
C
      do i=ibincount,2,-1
        do j=1,i-1
          if(RTMPFIELD(j).gt.RTMPFIELD(j+1))then
            RTMP=RTMPFIELD(j)
            RTMPFIELD(j)=RTMPFIELD(j+1)
            RTMPFIELD(j+1)=RTMP
          endif
        enddo
      enddo
C
      RMEDIVAL =(RTMPFIELD(ibincount/2)+RTMPFIELD(ibincount/2+1))/2.0
      RMEDIVAL2=(RTMPFIELD(ibincount/4)+RTMPFIELD(ibincount/4+1))/2.0
      RMEDIVAL3=(RTMPFIELD(ibincount*3/4)+RTMPFIELD(ibincount*3/4+1))/2.0
C
      if(ibincount.le.2)then
        RMEDIVAL=RTMPFIELD(1)
        RMEDIVAL2=RTMPFIELD(1)
        RMEDIVAL3=RTMPFIELD(1)
        RETURN
      endif
C
      if(ibincount.le.4)then
        RMEDIVAL=RTMPFIELD(3)
        RMEDIVAL2=RTMPFIELD(2)
        RMEDIVAL3=RTMPFIELD(3)
        RETURN
      endif
C
      RETURN
      END
C
C==============================================================================
C==============================================================================
C==============================================================================
C
      SUBROUTINE PLOTIT(PLTSIZ,FONTSIZE,CNAME,IRUN,IMAXBINS)
C
      IMPLICIT NONE
C
      INTEGER IRUN
      INTEGER IMAX
      INTEGER IHISTBINS
      INTEGER IMAXBINS
C
      PARAMETER (IMAX = 1000000)
      PARAMETER (IHISTBINS = 100)
C
      REAL PLTSIZ
C
      REAL TITLE(20)
      REAL RTITLE(80)
      REAL TEXT(20)
      CHARACTER*20 TMPTEXT
      CHARACTER*200 CLINE
      CHARACTER*200 CLIN2
      CHARACTER*80 CTITLE
      CHARACTER*200 CNAME
      CHARACTER*200 CRINGS
      EQUIVALENCE (CTITLE,RTITLE)
      EQUIVALENCE (TMPTEXT,TEXT)
C
      INTEGER IHFIELD(IMAX)
      INTEGER IKFIELD(IMAX)
      INTEGER ILFIELD(IMAX)
      REAL RZFIELD(IMAX)
      REAL RAMPFIELD(IMAX)
      REAL RPHAFIELD(IMAX)
      REAL RSIGFFIELD(IMAX)
      REAL RSIGPFIELD(IMAX)
      REAL RFOM(IMAX)
      REAL RecRESHORI(IMAX)
      REAL RecRESVERT(IMAX)
      REAL RecRESVAL(IMAX)
C
      REAL DRAD,FACTOR,pi
      REAL ACELL,BCELL,ABANG,ALAT,GAMMA
      REAL X,Y,XN,XP,YN,YP,RLENY,RLENX,RLEN
      REAL AX,AY,BX,BY
      REAL RESMAX,RecRESMAX
      REAL RecRESVERTMAX,RecRESHORIMAX,RecRESVALMAX
      REAL YPOSN,XPOSN,RORIX,RORIY
      REAL RORIRESMAX,RESMAX3D,RESMAXVERT
      REAL FONTSIZE
      REAL RVAL
      REAL RLINE1,RLINE2,RLINE3
      REAL XTMP,RTMP
      REAL rhue,rsat,rgbr,rgbg,rgbb
C
      REAL*8 AMP,PHASE,BCK,CTF
      INTEGER IOPTION,MDIM
      INTEGER IH,IK,IQ,IQLABEL
C
      INTEGER i,icount,k,j,ibin
C
      REAL RSIGFFIELDMAX
      REAL RSIGPFIELDMAX
C
      REAL*8 RBINSSIGF(0:IHISTBINS,3)
      REAL*8 RBINSSIGP(0:IHISTBINS,3)
      REAL*8 RBINSSIGPF(0:IHISTBINS,3)
      REAL*8 RBINSFOM(0:IHISTBINS,3)
C
      REAL*8 RBINSSIGF2(0:IHISTBINS,3)
      REAL*8 RBINSSIGP2(0:IHISTBINS,3)
      REAL*8 RBINSSIGPF2(0:IHISTBINS,3)
      REAL*8 RBINSFOM2(0:IHISTBINS,3)
C
      REAL*8 RBINSFOM3(0:IHISTBINS,3)
      REAL*8 RBINSFOM4(0:IHISTBINS,3)
C
      COMMON /DATA/ IHFIELD,IKFIELD,ILFIELD,RZFIELD,
     .     RAMPFIELD,RPHAFIELD,RSIGFFIELD,RSIGPFIELD,RFOM,
     .     RecRESMAX,icount,RORIX,RORIY,
     .     RecRESHORI,RecRESVERT,RecRESVAL,
     .     RSIGFFIELDMAX,RSIGPFIELDMAX,
     .     RBINSSIGF,RBINSSIGP,RBINSSIGPF,RBINSFOM,
     .     RBINSSIGF2,RBINSSIGP2,RBINSSIGPF2,RBINSFOM2,
     .     RBINSFOM3,RBINSFOM4,
     .     RLINE1,RLINE2,RLINE3,
     .     RESMAX3D,RESMAXVERT
C
C-----Plot initialization
C
      YPOSN=PLTSIZ+100.
C
      CALL P2K_MOVE(0.,0.,0.)
      CALL P2K_MOVE(5.,YPOSN,0.)
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
C
      write(CTITLE,'(''2dx_plotresolution resolution plot'')')
      call shorten(CTITLE,k)
      CALL P2K_STRING(RTITLE,k,0.)
      YPOSN=YPOSN-10.0
C
      call shorten(CNAME,k)
      write(CTITLE,'(''Input file: '',A)')CNAME(1:k)
      call shorten(CTITLE,k)
      CALL P2K_MOVE(5.,YPOSN,0.)
      CALL P2K_STRING(RTITLE,k,0.)
      YPOSN=YPOSN-15.0
C
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
      write(CTITLE,'(''Resolution limit horizontal '',
     .       ''is '',F10.2,'' A'')')RESMAX3D
      CALL P2K_MOVE(5.,YPOSN,0.)
      CALL P2K_STRING(RTITLE,60,0.)
      YPOSN=YPOSN-10.0
C
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
      write(CTITLE,'(''Resolution limit vertical '',
     .       ''is   '',F10.2,'' A'')')RESMAXVERT
      CALL P2K_MOVE(5.,YPOSN,0.)
      CALL P2K_STRING(RTITLE,60,0.)
      YPOSN=YPOSN-10.0
C
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
      write(CTITLE,'(''Resolution limit for plot '',
     .       ''is   '',F10.2,'' A'')')RORIRESMAX
      CALL P2K_MOVE(5.,YPOSN,0.)
      CALL P2K_STRING(RTITLE,60,0.)
      YPOSN=YPOSN-10.0
C
      if(IRUN.lt.13)then
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
        write(CTITLE,'(''Solid lines are 50% and better-25% '',
     .    ''median curves'')')
        CALL P2K_MOVE(5.,YPOSN,0.)
        CALL P2K_STRING(RTITLE,60,0.)
      endif
C
      RORIX=PLTSIZ/10.0
      RORIY=PLTSIZ/10.0
C
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*3.2)
      CALL P2K_COLOUR(0)
C
      if(IRUN.eq.1)then
        write(CTITLE,'(''FOM over 3D resolution '')')
      elseif (IRUN.eq.2)then
        write(CTITLE,'(''FOM over horizontal resolution '')')
      elseif (IRUN.eq.3)then
        write(CTITLE,'(''FOM over vertical resolution '')')
      elseif (IRUN.eq.4)then
        write(CTITLE,'(''SigAMP over 3D resolution '')')
      elseif (IRUN.eq.5)then
        write(CTITLE,'(''SigAMP over horizontal resolution '')')
      elseif (IRUN.eq.6)then
        write(CTITLE,'(''SigAMP over vertical resolution '')')
      elseif (IRUN.eq.7)then
        write(CTITLE,'(''SigP over 3D resolution '')')
      elseif (IRUN.eq.8)then
        write(CTITLE,'(''SigP over horizontal resolution '')')
      elseif (IRUN.eq.9)then
        write(CTITLE,'(''SigP over vertical resolution '')')
      elseif (IRUN.eq.10)then
        write(CTITLE,'(''FOM-weighted SigP over 3D resolution '')')
      elseif (IRUN.eq.11)then
        write(CTITLE,'(''FOM-weighted SigP over '',
     .    ''horizontal resolution '')')
      elseif (IRUN.eq.12)then
        write(CTITLE,'(''FOM-weighted SigP over vertical resolution '')')
      elseif (IRUN.eq.13)then
        write(CTITLE,'(''FOM in 3D Fourier space '')')
      elseif (IRUN.eq.14)then
        write(CTITLE,'(''SigP in 3D Fourier space '')')
      elseif (IRUN.eq.15)then
        write(CTITLE,'(''FSC in 3D Fourier space '')')
      elseif (IRUN.eq.16)then
        write(CTITLE,'(''FSC over horizontal resolution '')')
      elseif (IRUN.eq.17)then
        write(CTITLE,'(''FSC over vertical resolution '')')
      elseif (IRUN.eq.18)then
        write(CTITLE,'(''DPR in 3D Fourier space '')')
      elseif (IRUN.eq.19)then
        write(CTITLE,'(''DPR over horizontal resolution '')')
      elseif (IRUN.eq.20)then
        write(CTITLE,'(''DPR over vertical resolution '')')
      endif
C
C      write(cline,'(I2,'': '',A)')IRUN,CTITLE
C      write(CTITLE,'(A)')cline(1:80)
C
      X=5.
      Y=PLTSIZ+20.
      CALL P2K_MOVE(X,Y,0.)
      CALL P2K_STRING(RTITLE,60,0.)
C
      call P2K_LWIDTH(RLINE1)
C
      X=0.0
      Y=0.0
      CALL P2K_MOVE(X,Y,0.)
      X=PLTSIZ
      CALL P2K_DRAW(X,Y,0.)
      Y=PLTSIZ
      CALL P2K_DRAW(X,Y,0.)
      X=0.0
      CALL P2K_DRAW(X,Y,0.)
      Y=0.0
      CALL P2K_DRAW(X,Y,0.)
C
C=====Plot horizontal and vertical axes
C
      call P2K_LWIDTH(RLINE2)
C
      RLENX=PLTSIZ-2.0*RORIX
      RLENY=PLTSIZ-2.0*RORIY
C
C-----Plot horizontal axis
C
      X=RORIX
      Y=RORIY
      CALL P2K_MOVE(X,Y,0.)
      X=RORIX+RLENX
      CALL P2K_DRAW(X,Y,0.)
C
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
C
      RLEN=4.0
      do i=0,5
        RVAL=i/5.0*RecRESMAX
        X=RORIX+RLENX*RVAL/RecRESMAX
        Y=RORIY+0.0-RLEN
        CALL P2K_MOVE(X,Y,0.)
        Y=RORIY+0.0+RLEN
        CALL P2K_DRAW(X,Y,0.)             
        X=X-5.
        Y=RORIY-RLEN-7.0
        CALL P2K_MOVE(X,Y,0.)
        RVAL=1.0/RVAL
        if(i.gt.0)then
          WRITE(TMPTEXT,'(F6.1)')RVAL
          call shorten(TMPTEXT,k)
          CALL P2K_CSTRING(TEXT,k,0.)
        endif
      enddo
C
      X=RORIX+RLENX-30.
      Y=RORIY-RLEN-15.
      CALL P2K_MOVE(X,Y,0.0)
      if(IRUN.eq.1 .or. IRUN.eq.4 .or. IRUN.eq.7 .or. IRUN.eq.10 .or. 
     .   IRUN.eq.15 .or. IRUN.eq.18) then
         WRITE(CTITLE,'(''Overall Resolution [A]'')')
      elseif(IRUN.eq.2 .or. IRUN.eq.5 .or. IRUN.eq.8 .or. IRUN.eq.11 .or.
     .       IRUN.eq.13 .or. IRUN.eq.16 .or. IRUN.eq.19) then
         WRITE(CTITLE,'(''Horizontal Resolution [A]'')')
      elseif(IRUN.eq.3 .or. IRUN.eq.6 .or. IRUN.eq.9 .or. IRUN.eq.12 .or.
     .       IRUN.eq.17 .or. IRUN.eq.20) then
         WRITE(CTITLE,'(''Vertical Resolution [A]'')')
      endif
C      write(cline,'(I2,'': '',A)')IRUN,CTITLE
C      write(CTITLE,'(A)')cline(1:80)
      call shorten(CTITLE,k)
      CALL P2K_CSTRING(RTITLE,k,0.)
C
C-----Plot vertical axis
C
      X=RORIX
      Y=RORIY
      CALL P2K_MOVE(X,Y,0.)
      X=RORIX
      Y=RORIY+RLENY
      CALL P2K_DRAW(X,Y,0.)
C
      if(IRUN.ge.1 .and. IRUN.le.3)then
        do i=0,10
          X=RORIX-RLEN
          Y=RORIY+0.1*REAL(i)*RLENY
          CALL P2K_MOVE(X,Y,0.)
          X=RORIX+RLEN
          CALL P2K_DRAW(X,Y,0.)
          X=RORIX-RLEN-PLTSIZ/40.
          Y=Y-2.
          CALL P2K_MOVE(X,Y,0.)
          WRITE(TMPTEXT,'(I3)')i*10
          call shorten(TMPTEXT,k)
          CALL P2K_CSTRING(TEXT,k,0.)
        enddo
      elseif(IRUN.ge.15 .and. IRUN.le.17)then
        do i=0,10
          X=RORIX-RLEN
          Y=RORIY+0.1*REAL(i)*RLENY
          CALL P2K_MOVE(X,Y,0.)
          X=RORIX+RLEN
          CALL P2K_DRAW(X,Y,0.)
          X=RORIX-RLEN-PLTSIZ/40.
          Y=Y-2.
          CALL P2K_MOVE(X,Y,0.)
          WRITE(TMPTEXT,'(F4.1)')REAL(i)/10.0
          call shorten(TMPTEXT,k)
          CALL P2K_CSTRING(TEXT,k,0.)
        enddo
      elseif(IRUN.ge.4 .and. IRUN.le.6)then
        do i=0,10
          X=RORIX-RLEN
          Y=RORIY+0.1*REAL(i)*RLENY
          CALL P2K_MOVE(X,Y,0.)
          X=RORIX+RLEN
          CALL P2K_DRAW(X,Y,0.)
          X=RORIX-RLEN-PLTSIZ/30.
          Y=Y-2.
          CALL P2K_MOVE(X,Y,0.)
          WRITE(TMPTEXT,'(F4.1)')REAL(i)/10.
          call shorten(TMPTEXT,k)
          CALL P2K_CSTRING(TEXT,k,0.)
        enddo
      elseif(IRUN.ge. 7 .and. IRUN.le.12)then
        do i=0,9 
          X=RORIX-RLEN
          Y=RORIY+REAL(i)*RLENY/9.0
          CALL P2K_MOVE(X,Y,0.)
          X=RORIX+RLEN
          CALL P2K_DRAW(X,Y,0.)
          X=RORIX-RLEN-PLTSIZ/40.
          Y=Y-2.
          CALL P2K_MOVE(X,Y,0.)
          WRITE(TMPTEXT,'(I3)')i*10
          call shorten(TMPTEXT,k)
          CALL P2K_CSTRING(TEXT,k,0.)
        enddo
        X=RORIX
        Y=RORIY+45.0*RLENY/90.0
        CALL P2K_MOVE(X,Y,0.)
        X=RORIX+RLENX
        CALL P2K_DRAW(X,Y,0.)
        X=RORIX
        Y=RORIY+60.0*RLENY/90.0
        CALL P2K_MOVE(X,Y,0.)
        X=RORIX+RLENX
        CALL P2K_DRAW(X,Y,0.)
        X=RORIX
        Y=RORIY+90.0*RLENY/90.0
        CALL P2K_MOVE(X,Y,0.)
        X=RORIX+RLENX
        CALL P2K_DRAW(X,Y,0.)
      elseif(IRUN.ge.13 .and. IRUN.le.14)then
        do i=0,5
          RVAL=i/5.0*RecRESMAX
          X=RORIX+0.0-RLEN
          Y=RORIY+RLENY*RVAL/RecRESMAX
          CALL P2K_MOVE(X,Y,0.)
          X=RORIY+0.0+RLEN
          CALL P2K_DRAW(X,Y,0.)             
          X=RORIX-RLEN-PLTSIZ/20.
          Y=Y-2.
          CALL P2K_MOVE(X,Y,0.)
          RVAL=1.0/RVAL
          if(i.gt.0)then
            WRITE(TMPTEXT,'(F6.1)')RVAL
            call shorten(TMPTEXT,k)
            CALL P2K_CSTRING(TEXT,k,0.)
          endif
        enddo
      elseif(IRUN.ge.18 .and. IRUN.le.20)then
        do i=0,18
          X=RORIX-RLEN
          Y=RORIY+REAL(i)*RLENY/18.0
          CALL P2K_MOVE(X,Y,0.)
          X=RORIX+RLEN-2.
          CALL P2K_DRAW(X,Y,0.)
          X=RORIX-RLEN-PLTSIZ/40.
          Y=Y-2.
          CALL P2K_MOVE(X,Y,0.)
          WRITE(TMPTEXT,'(I4)')i*10
          call shorten(TMPTEXT,k)
          CALL P2K_CSTRING(TEXT,k,0.)
        enddo
        X=RORIX
        Y=RORIY+45.0*RLENY/180.0
        CALL P2K_MOVE(X,Y,0.)
        X=RORIX+RLENX
        CALL P2K_DRAW(X,Y,0.)
        X=RORIX
        Y=RORIY+60.0*RLENY/180.0
        CALL P2K_MOVE(X,Y,0.)
        X=RORIX+RLENX
        CALL P2K_DRAW(X,Y,0.)
        X=RORIX
        Y=RORIY+90.0*RLENY/180.0
        CALL P2K_MOVE(X,Y,0.)
        X=RORIX+RLENX
        CALL P2K_DRAW(X,Y,0.)
        X=RORIX
        Y=RORIY+180.0*RLENY/180.0
        CALL P2K_MOVE(X,Y,0.)
        X=RORIX+RLENX
        CALL P2K_DRAW(X,Y,0.)

      endif
C
      X=RORIX+5.0
      Y=RORIY+RLENY+12.
      if(IRUN.ge.1 .and. IRUN.le.3)then
        WRITE(CTITLE,'(''FOM [%]'')')
      elseif(IRUN.ge.4 .and. IRUN.le.6)then
        WRITE(CTITLE,'(''SigAMP / AMP'')')
      elseif(IRUN.ge.7 .and. IRUN.le.9)then
        WRITE(CTITLE,'(''SigP [deg]'')')
      elseif(IRUN.ge.10 .and. IRUN.le.12)then
        WRITE(CTITLE,'(''SigP*FOM [deg]'')')
        X=X+10.0
      elseif(IRUN.ge.13 .and. IRUN.le.14)then
        WRITE(CTITLE,'(''Vertical Resolution [A]'')')
        X=X+20.0
      elseif(IRUN.ge.15 .and. IRUN.le.17)then
        WRITE(CTITLE,'(''FSC'')')
      elseif(IRUN.ge.18 .and. IRUN.le.20)then
        WRITE(CTITLE,'(''DPR [deg]'')')
      endif
      CALL P2K_MOVE(X,Y,0.0)
      call shorten(CTITLE,k)
      CALL P2K_CSTRING(RTITLE,k,0.)
C
C-----Plot data
C
      call P2K_LWIDTH(RLINE1)
C
      RLEN=0.5
C
C        read(1,*,END=200,ERR=980)IHFIELD(i),IKFIELD(i),RZFIELD(i),
C     .     RAMPFIELD(i),RPHAFIELD(i),RSIGFFIELD(i),RSIGPFIELD(i),RFOM(i)
C
      do i = 1,icount
        rgbr=0.0
        rgbg=0.2
        rgbb=1.0
        CALL P2K_RGB_COLOUR(rgbr,rgbg,rgbb)
        if(IRUN.eq.1)then
          X=RORIX+RLENX*RecRESVAL(i)/RecRESMAX
          Y=RORIY+RLENY*RFOM(i)
        elseif (IRUN.eq.2) then
          X=RORIX+RLENX*RecRESHORI(i)/RecRESMAX
          Y=RORIY+RLENY*RFOM(i)
        elseif (IRUN.eq.3) then
          X=RORIX+RLENX*RecRESVERT(i)/RecRESMAX
          Y=RORIY+RLENY*RFOM(i)
        elseif (IRUN.eq.4)then
          X=RORIX+RLENX*RecRESVAL(i)/RecRESMAX
          Y=RORIY+RLENY*RSIGFFIELD(i)/RAMPFIELD(i)
        elseif (IRUN.eq.5) then
          X=RORIX+RLENX*RecRESHORI(i)/RecRESMAX
          Y=RORIY+RLENY*RSIGFFIELD(i)/RAMPFIELD(i)
        elseif (IRUN.eq.6) then
          X=RORIX+RLENX*RecRESVERT(i)/RecRESMAX
          Y=RORIY+RLENY*RSIGFFIELD(i)/RAMPFIELD(i)
        elseif (IRUN.eq.7)then
          X=RORIX+RLENX*RecRESVAL(i)/RecRESMAX
          Y=RORIY+RLENY*RSIGPFIELD(i)/90.0
        elseif (IRUN.eq.8) then
          X=RORIX+RLENX*RecRESHORI(i)/RecRESMAX
          Y=RORIY+RLENY*RSIGPFIELD(i)/90.0
        elseif (IRUN.eq.9) then
          X=RORIX+RLENX*RecRESVERT(i)/RecRESMAX
          Y=RORIY+RLENY*RSIGPFIELD(i)/90.0
        elseif (IRUN.eq.10)then
          X=RORIX+RLENX*RecRESVAL(i)/RecRESMAX
          Y=RORIY+RLENY*RSIGPFIELD(i)*RFOM(i)/90.0
        elseif (IRUN.eq.11) then
          X=RORIX+RLENX*RecRESHORI(i)/RecRESMAX
          Y=RORIY+RLENY*RSIGPFIELD(i)*RFOM(i)/90.0
        elseif (IRUN.eq.12) then
          X=RORIX+RLENX*RecRESVERT(i)/RecRESMAX
          Y=RORIY+RLENY*RSIGPFIELD(i)*RFOM(i)/90.0
C          write(6,'('': '',5F12.3)')RecRESVAL(i),RSIGPFIELD(i),RFOM(i),X,Y
C
        elseif (IRUN.eq.13) then
          X=RORIX+RLENX*RecRESHORI(i)/RecRESMAX
          Y=RORIY+RLENY*RecRESVERT(i)/RecRESMAX
          RLEN=RFOM(i)*RFOM(i)
C---------Calculate HSV Values:
          rhue=RLEN*300
          rsat=max(RLEN,0.5)
          RVAL=0.7
C---------Calculate RGB Values:
          call HSVTORGB(rhue,rsat,RVAL,rgbr,rgbg,rgbb)
C---------Set RGB color:
          CALL P2K_RGB_COLOUR(rgbr,rgbg,rgbb)
C
        elseif (IRUN.eq.14) then
          X=RORIX+RLENX*RecRESHORI(i)/RecRESMAX
          Y=RORIY+RLENY*RecRESVERT(i)/RecRESMAX
          RLEN=(45.0 - RSIGPFIELD(i))/45.0
          if(RLEN.gt.1.0)RLEN=1.0
          if(RLEN.lt.0.0)RLEN=0.1
C---------Calculate HSV Values:
          rhue=RLEN*300
          rsat=max(RLEN,0.5)
          RVAL=0.7
C---------Calculate RGB Values:
          call HSVTORGB(rhue,rsat,RVAL,rgbr,rgbg,rgbb)
C---------Set RGB color:
          CALL P2K_RGB_COLOUR(rgbr,rgbg,rgbb)
          if(RSIGPFIELD(i).le.10.0)then
            rgbr = 0.0
            rgbg = 0.0
            rgbb = 1.0
          elseif(RSIGPFIELD(i).le.30.0)then
            rgbr = 0.0
            rgbg = 0.8
            rgbb = 0.6
          elseif(RSIGPFIELD(i).le.45.0)then
            rgbr = 0.9
            rgbg = 0.2
            rgbb = 0.0
          elseif(RSIGPFIELD(i).le.60.0)then
            rgbr = 1.0
            rgbg = 0.5
            rgbb = 0.5
          else
            rgbr = 0.8
            rgbg = 0.8
            rgbb = 0.8
          endif
        endif
C
        if(IRUN.lt.15)then
          if(X.lt.RORIX) X=RORIX
          if(X.gt.RORIX+RLENX) X=RORIX+RLENX
          if(Y.lt.RORIY) Y=RORIY
          if(Y.gt.RORIY+RLENY) Y=RORIY+RLENY
          XN=X-RLEN
          XP=X+RLEN
          YN=Y-RLEN
          YP=Y+RLEN
          CALL P2K_MOVE(XN,Y,0.)
          CALL P2K_DRAW(XP,Y,0.)
          CALL P2K_MOVE(X,YN,0.)
          CALL P2K_DRAW(X,YP,0.)           ! small plus at each position
        endif
      enddo
C
C-----Plot legend
C
      CALL P2K_COLOUR(0)
C
      if(IRUN.eq.13)then
        X=RORIX+RLENX
        Y=RORIY+RLENY+20.0
        CALL P2K_MOVE(X,Y,0.0)
        WRITE(CTITLE,'(''FOM Symbol'')')
        call shorten(CTITLE,k)
        CALL P2K_CSTRING(RTITLE,k,0.)
C      
        XTMP=X-12.0
        do i=10,0,-1
C
          RLEN=REAL(i)/10.0
C---------Calculate HSV Values:
          rhue=RLEN*300
          rsat=max(RLEN,0.5)
          RVAL=0.7
C---------Calculate RGB Values:
          call HSVTORGB(rhue,rsat,RVAL,rgbr,rgbg,rgbb)
C---------Set RGB color:
          CALL P2K_RGB_COLOUR(rgbr,rgbg,rgbb)
C
          X=XTMP
          Y=Y-10.0
          CALL P2K_MOVE(X,Y,0.0)
          WRITE(CTITLE,'(I3,''%'')')I*10
          call shorten(CTITLE,k)
          CALL P2K_CSTRING(RTITLE,k,0.)
C
          X=XTMP+15.0
          Y=Y+2.0
C
          XN=X-RLEN
          XP=X+RLEN
          YN=Y-RLEN
          YP=Y+RLEN
          CALL P2K_MOVE(XN,Y,0.)
          CALL P2K_DRAW(XP,Y,0.)
          CALL P2K_MOVE(X,YN,0.)
          CALL P2K_DRAW(X,YP,0.)           ! small plus at each position
          Y=Y-2.0
        enddo
C
      elseif(IRUN.eq.14)then
C
        X=RORIX+RLENX+8.0
        Y=RORIY+RLENY+20.0
        CALL P2K_MOVE(X,Y,0.0)
        WRITE(CTITLE,'(''SigP Symbol'')')
        call shorten(CTITLE,k)
        CALL P2K_CSTRING(RTITLE,k,0.)
C      
        XTMP=X-5.0
        do i=0,9
C
          RLEN=(45.0 - REAL(i)*10.0)/45.0
          if(RLEN.gt.1.0)RLEN=1.0
          if(RLEN.lt.0.0)RLEN=0.1
C
C---------Calculate HSV Values:
          rhue=RLEN*300
          rsat=max(RLEN,0.5)
          RVAL=0.7
C---------Calculate RGB Values:
          call HSVTORGB(rhue,rsat,RVAL,rgbr,rgbg,rgbb)
C---------Set RGB color:
          CALL P2K_RGB_COLOUR(rgbr,rgbg,rgbb)
C
          if(REAL(i).le.1.0)then
            rgbr = 0.0
            rgbg = 0.0
            rgbb = 1.0
          elseif(REAL(i).le.3.0)then
            rgbr = 0.0
            rgbg = 0.8
            rgbb = 0.6
          elseif(REAL(i).le.4.5)then
            rgbr = 0.9
            rgbg = 0.2
            rgbb = 0.0
          elseif(REAL(i).le.6.0)then
            rgbr = 1.0
            rgbg = 0.5
            rgbb = 0.5
          else
            rgbr = 0.8
            rgbg = 0.8
            rgbb = 0.8
          endif
C
          X=XTMP
          Y=Y-10.0
          CALL P2K_MOVE(X,Y,0.0)
          WRITE(CTITLE,'(I3,''deg'')')I*10
          call shorten(CTITLE,k)
          CALL P2K_CSTRING(RTITLE,k,0.)
C
          X=XTMP+18.0
          Y=Y+2.0
          XN=X-RLEN
          XP=X+RLEN
          YN=Y-RLEN
          YP=Y+RLEN
          CALL P2K_MOVE(XN,Y,0.)
          CALL P2K_DRAW(XP,Y,0.)
          CALL P2K_MOVE(X,YN,0.)
          CALL P2K_DRAW(X,YP,0.)           ! small plus at each position
          Y=Y-2.0
        enddo
      endif
C
      CALL P2K_COLOUR(0)
C
C-----Plot first average curves
C
      call P2K_LWIDTH(RLINE3)
      rgbr=0.0
      rgbg=0.0
      rgbb=0.0
      CALL P2K_RGB_COLOUR(rgbr,rgbg,rgbb)
C
      if(IRUN.lt.13 .or. IRUN.ge.15)then
        j=0
        do i = 0,IMAXBINS
          X=RORIX+RLENX*REAL(i)/IMAXBINS
          if(IRUN.eq.1)then
            Y=RORIY+RBINSFOM(i,1)*RLENY
          elseif(IRUN.eq.2)then
            Y=RORIY+RBINSFOM(i,2)*RLENY
          elseif(IRUN.eq.3)then
            Y=RORIY+RBINSFOM(i,3)*RLENY
          elseif(IRUN.eq.4)then
            Y=RORIY+RBINSSIGF(i,1)*RLENY
          elseif(IRUN.eq.5)then
            Y=RORIY+RBINSSIGF(i,2)*RLENY
          elseif(IRUN.eq.6)then
            Y=RORIY+RBINSSIGF(i,3)*RLENY
          elseif(IRUN.eq.7)then
            Y=RORIY+RBINSSIGP(i,1)*RLENY/90.0
          elseif(IRUN.eq.8)then
            Y=RORIY+RBINSSIGP(i,2)*RLENY/90.0
          elseif(IRUN.eq.9)then
            Y=RORIY+RBINSSIGP(i,3)*RLENY/90.0
          elseif(IRUN.eq.10)then
            Y=RORIY+RBINSSIGPF(i,1)*RLENY/90.0
          elseif(IRUN.eq.11)then
            Y=RORIY+RBINSSIGPF(i,2)*RLENY/90.0
          elseif(IRUN.eq.12)then
            Y=RORIY+RBINSSIGPF(i,3)*RLENY/90.0
          elseif(IRUN.eq.15)then
            Y=RORIY+RBINSFOM3(i,1)**2 * RLENY
          elseif(IRUN.eq.16)then
            Y=RORIY+RBINSFOM3(i,2)**2 * RLENY
          elseif(IRUN.eq.17)then
            Y=RORIY+RBINSFOM3(i,3)**2 * RLENY
          elseif(IRUN.eq.18)then
            Y=RORIY+RBINSFOM4(i,1)*RLENY/180.0
          elseif(IRUN.eq.19)then
            Y=RORIY+RBINSFOM4(i,2)*RLENY/180.0
          elseif(IRUN.eq.20)then
            Y=RORIY+RBINSFOM4(i,3)*RLENY/180.0
          endif
C
          if(Y.gt.RORIY-0.01 .and. Y.le.RORIY+RLENY+0.01)then
            if(j.eq.0)then
              call P2K_MOVE(X,Y,0.)
              j=1
            else
              call P2K_DRAW(X,Y,0.)
            endif
          endif
        enddo
      endif
C
C-----Plot second average curves
C
      call P2K_LWIDTH(RLINE3)
      rgbr=0.0
      rgbg=0.7
      rgbb=0.0
      CALL P2K_RGB_COLOUR(rgbr,rgbg,rgbb)
C
      if(IRUN.lt.13)then
        j=0
        do i = 0,IMAXBINS
          X=RORIX+RLENX*REAL(i)/IMAXBINS
          if(IRUN.eq.1)then
            Y=RORIY+RBINSFOM2(i,1)*RLENY
          elseif(IRUN.eq.2)then
            Y=RORIY+RBINSFOM2(i,2)*RLENY
          elseif(IRUN.eq.3)then
            Y=RORIY+RBINSFOM2(i,3)*RLENY
          elseif(IRUN.eq.4)then
            Y=RORIY+RBINSSIGF2(i,1)*RLENY
          elseif(IRUN.eq.5)then
            Y=RORIY+RBINSSIGF2(i,2)*RLENY
          elseif(IRUN.eq.6)then
            Y=RORIY+RBINSSIGF2(i,3)*RLENY
          elseif(IRUN.eq.7)then
            Y=RORIY+RBINSSIGP2(i,1)*RLENY/90.0
          elseif(IRUN.eq.8)then
            Y=RORIY+RBINSSIGP2(i,2)*RLENY/90.0
          elseif(IRUN.eq.9)then
            Y=RORIY+RBINSSIGP2(i,3)*RLENY/90.0
          elseif(IRUN.eq.10)then
            Y=RORIY+RBINSSIGPF2(i,1)*RLENY/90.0
          elseif(IRUN.eq.11)then
            Y=RORIY+RBINSSIGPF2(i,2)*RLENY/90.0
          elseif(IRUN.eq.12)then
            Y=RORIY+RBINSSIGPF2(i,3)*RLENY/90.0
          endif
C
          if(Y.gt.RORIY-0.01 .and. Y.le.RORIY+RLENY+0.01)then
            if(j.eq.0)then
              call P2K_MOVE(X,Y,0.)
              j=1
            else
              call P2K_DRAW(X,Y,0.)
            endif
          endif
        enddo
      endif
C
      call P2K_LWIDTH(RLINE1)
      CALL P2K_COLOUR(0)
      call P2K_PAGE
C
      RETURN
C
      end
C
c==========================================================
c==========================================================
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
c==========================================================
c==========================================================
c==========================================================
C
      SUBROUTINE HSVTORGB(rhue,rsat,rval,rgbr,rgbg,rgbb)
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

