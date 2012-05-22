      PROGRAM HKLCLEAN

C       updated 4/19/2008

      PARAMETER (MAXSPOT = 100)
      CHARACTER*200  TITLE 
      CHARACTER*200 cline1,cline2,cline3
      INTEGER H,K,L
C
      WRITE(6,'('': 2dx_hklclean, to eliminate duplicates'')')
C
      write(6,'('' Give input file name'')')
      read(*,'(A)')cline1
      call shorten(cline1,k)
      write(*,'('' read: '',A)')cline1(1:k)
C
      write(*,'('' Give output file name'')')
      read(*,'(A)')cline2
      call shorten(cline2,k)
      write(*,'('' read: '',A)')cline2(1:k)
C
      write(cline3,'(''rm -f '',A)')cline2(1:k)
      call system(cline3)
C
      write(*,'('' Is there a header line '',
     .  ''(1=y,0=no,2=yes but drop it)'')')
      read(*,*)iheader
      if(iheader.eq.1)then
        write(*,'('' Assuming header line.'')')
      else if(iheader.eq.2)then
        write(*,'('' Assuming header line but droping it.'')')
      else
        write(*,'('' Assuming no header line.'')')
      endif
C
      write(*,'('' Is there a siga column (0=no,1=y,2=Background)'')')
      read(*,*)isig
      if(isig.eq.1)then
        write(*,'('' Assuming siga column.'')')
      else
        write(*,'('' Assuming no siga column.'')')
      endif
C
      open(10,FILE=cline1,STATUS='OLD',ERR=940)
      open(11,FILE=cline2,STATUS='NEW',ERR=950)
      open(12,FILE="APH/syn_nosort2D-plot.hkl",STATUS="NEW",ERR=980)
C
      if(iheader.gt.0)then
        READ (10,100) TITLE
        if(iheader.ne.2)then
          WRITE (11,100) TITLE
        endif
        WRITE (12,100) TITLE
      endif
C
      IH=-99999
      IK=-99999
      IL=-99999
 1000 continue
        BACK=0.0
        SIGA=0.0
        FOM=0.0
        if(isig.eq.1)then
          READ (10,*,END=1005) H,K,L,AMP,PHASE,FOM,SIGA
        elseif(isig.eq.2)then
          READ (10,*,END=1005) H,K,L,AMP,PHASE,BACK,FOM
        else
          READ (10,*,END=1005) H,K,L,AMP,PHASE,FOM
        endif
 1001   continue
        if(PHASE.gt.180.0)then
          PHASE=PHASE-360.0
          goto 1001
        endif
 1002   continue
        if(PHASE.lt.-180.0)then
          PHASE=PHASE+360.0
          goto 1002
        endif
        if(H.ne.IH .or. K.ne.IK .or. L.ne.IL)then
          if(isig.eq.1)then
            write (11,210) H,K,L,AMP,PHASE,FOM,SIGA
          elseif(isig.eq.2)then
            write (11,210) H,K,L,AMP,PHASE,BACK,FOM
          else
CHEN>
C------------If you define FOM=cos(SIGA), then this could give SIGA:
C
C            if((FOM.le.100.0).and.(FOM.ge.0.0))then
C              SIGA=ACOS(FOM/100.0)
C            else
C              write(*,'(''::ERROR: Illegale FOM value of '',F16.8)')FOM
C              stop
C            endif
C           write (11,210) H,K,L,AMP,PHASE,FOM,SIGA
CHEN<
            write (11,200) H,K,L,AMP,PHASE,FOM
          endif
C
          if(L.eq.0)then
            if(FOM.gt.95.0)then
              IQ=1
            elseif(FOM.gt.90.0)then
              IQ=2
            elseif(FOM.gt.85.0)then
              IQ=3
            elseif(FOM.gt.80.0)then
              IQ=4
            elseif(FOM.gt.75.0)then
              IQ=5
            elseif(FOM.gt.70.0)then
              IQ=6
            elseif(FOM.gt.65.0)then
              IQ=7
            elseif(FOM.gt.60.0)then
              IQ=8
            else
              IQ=9
            endif
            write(12,'(1X,2I5,F12.1,F12.3,I4,2F12.3)') H,K,AMP,PHASE,IQ,BACK,FOM
          endif
C
          IH=H
          IK=K
          IL=L
        endif
      goto 1000
C
 1005 continue
      close(10)
      close(11)
      close(12)
C
 100  FORMAT (A50)
 200  FORMAT (3I6,G16.8,F12.3,G16.8)
 210  FORMAT (3I6,G16.8,F12.3,G16.8,G16.8)
 300  FORMAT (3I6,G16.8,F12.3,G16.8)
C
      goto 999
C
 940  continue
      write(*,'('':: ERROR while opening input file.'')')
      goto 999
C
 950  continue
      write(*,'('':: ERROR while opening output file.'')')
      goto 999
C
 960  continue
      write(*,'('':: ERROR in input data. No FOM values.'')')
      goto 999
C
 970  continue
      write(*,'('':: ERROR in input data. Too extreme H, K, or L.'')')
      write(*,'('':: ERROR in input data. Too extreme H, K, or L.'')')
      write(*,'('':: ERROR in input data. Too extreme H, K, or L.'')')
      goto 999
C
 980  continue
      write(*,'('':: ERROR while opening output file '',
     .          ''APH/syn_nosort2D-plot.hkl.'')')
      goto 999
C
 999  continue
      STOP

      END     
C
c==========================================================
c
      SUBROUTINE SHORTEN(czeile,k)
C
C counts the number of actual characters not ' ' in czeile
C and gives the result out in k.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1,CTMP2
      CTMP2=' '
      ilen=len(czeile)
      DO 100 I=1,ilen
         k=ilen-I
         READ(CZEILE(k:k),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)GOTO 300
  100 CONTINUE
  300 CONTINUE
      IF(k.LT.1)k=1
C
      RETURN
      END
C

