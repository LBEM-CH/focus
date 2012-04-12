      PROGRAM HKLSYM

C       Henning Stahlberg, 16.5.2000
C       updated 4/19/2008

      PARAMETER (MAXSPOT = 100)
      CHARACTER*200  TITLE 
      CHARACTER*200 cline1,cline2,cline3,cline4
      INTEGER H,K,L
      REAL RFAMP(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT)
      REAL RFPHASE(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT)
      REAL RFFOM(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT)
      REAL RFSIGA(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT)
      REAL RFBACK(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT)
      REAL ROUTP(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,6)
      REAL ISTHERE(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT)
C
      PI=3.1415926537
C
      WRITE(*,'('': 2dx_sym, to symmetrize APH file '')')
C
      write(*,'('' Give input file name'')')
      read(*,'(A)')cline1
      call shorten(cline1,k)
      write(*,'('' read: '',A)')cline1(1:k)
C
      write(*,'('' Give output file name'')')
      read(*,'(A)')cline2
      call shorten(cline2,k)
      write(*,'('' read: '',A)')cline2(1:k)
C
      write(cline4,'(''rm -f '',A)')cline2(1:k)
      call system(cline4)
C
      write(*,'('' Input spacegroup'')')
      READ (5,'(I6)') ispc
      WRITE (6,'(''Spacegroup = '',I6)') ispc
C
      write(*,'('' Is there a header line (1=y,0=no)'')')
      read(*,*)iheader
      if(iheader.eq.1)then
        write(*,'('' Assuming header line.'')')
      else
        write(*,'('' Assuming no header line.'')')
      endif
C
      write(*,'('' Is there a siga column (0=no,1=y,2=Background)'')')
      read(*,*)isig
      if(isig.eq.2)then
        write(*,'('' Assuming Background, FOM columns.'')')
      elseif(isig.eq.1)then
        write(*,'('' Assuming siga column.'')')
      else
        write(*,'('' Assuming no siga column.'')')
      endif
C
      open(10,FILE=cline1,STATUS='OLD',ERR=940)
      open(11,FILE=cline2,STATUS='NEW',ERR=950)
C
      if(iheader.eq.1)then
        READ (10,100) TITLE
        WRITE (11,100) TITLE
      endif
C
      do H=-MAXSPOT,MAXSPOT
        do K=-MAXSPOT,MAXSPOT
          do L=-MAXSPOT,MAXSPOT
            RFAMP(H,K,L)=0.0
            RFPHASE(H,K,L)=0.0
            RFFOM(H,K,L)=0.0
            ROUTP(H,K,L,6)=0.0
            ISTHERE(H,K,L)=0
          enddo
        enddo
      enddo
C
      i=0
      ihmax=0
      ikmax=0
      ilmax=0
      SIGA=0.0
      FOM=0.0
      BACK=0.0
 1000 continue
        if(isig.eq.1)then
          READ (10,*,END=1005) H,K,L,AMP,PHASE,FOM,SIGA
          write (*,210) H,K,L,AMP,PHASE,FOM,SIGA
        elseif(isig.eq.2)then
          READ (10,*,END=1005) H,K,L,AMP,PHASE,BACK,FOM
          write (*,210) H,K,L,AMP,PHASE,BACK,FOM
        else
          READ (10,*,END=1005) H,K,L,AMP,PHASE,FOM
          SIGA=0.0
          write (*,200) H,K,L,AMP,PHASE,FOM
        endif
        i=i+1
        if(abs(H).gt.MAXSPOT)goto 970
        if(abs(K).gt.MAXSPOT)goto 970
        if(abs(L).gt.MAXSPOT)goto 970
C-------Treat only right half of Fourier space (H>=0)
        if(H.lt.0)then
          H=-H
          K=-K
          L=-L
          PHASE=-PHASE
        endif
C
        RFAMP(H,K,L)=AMP
        RFPHASE(H,K,L)=PHASE
        RFFOM(H,K,L)=FOM
        RFSIGA(H,K,L)=SIGA
        RFBACK(H,K,L)=BACK
        ISTHERE(H,K,L)=1
        if(ihmax.lt.abs(H))ihmax=abs(H)
        if(ikmax.lt.abs(K))ikmax=abs(K)
        if(ilmax.lt.abs(L))ilmax=abs(L)
      goto 1000
C
 1005 continue
      close(10)
      icount=i
      if(icount.lt.1)then
        write(*,'('':: ERROR in 2dx_symhkl'')')
        goto 999
      endif
      write(*,'('': '',I9,'' spots read.'')')icount
      write(*,'('': Max H = '',I6)')ihmax
      write(*,'('': Max K = '',I6)')ikmax
      write(*,'('': Max L = '',I6)')ilmax
      ihmax=abs(ihmax)+abs(ikmax)
      ikmax=ihmax
C
      do H=0,ihmax
        do K=-ikmax,ikmax
          do L=-ilmax,ilmax
C
C            if(RFAMP(H,K,L).ne.0.0)then
C              write(*,'('':: RFAMP('',3I4,'') = '',F16.3)')H,K,L,RFAMP(H,K,L)
C            endif
C
            AMP=0.0
            PX=0.0
            PY=0.0
            FOM=0.0
            SIGA=0.0
            BACK=0.0
            ianz=0
C
            if(ISTHERE(H,K,L).ne.0)then
              RPT=RFPHASE(H,K,L)*PI/180.0
              AMP=AMP+RFAMP(H,K,L)*RFFOM(H,K,L)
              PX=PX+cos(RPT)*RFFOM(H,K,L)
              PY=PY+sin(RPT)*RFFOM(H,K,L)
              FOM=FOM+RFFOM(H,K,L)
              SIGA=SIGA+RFSIGA(H,K,L)*RFFOM(H,K,L)
              BACK=BACK+RFBACK(H,K,L)*RFFOM(H,K,L)
              ianz=ianz+1
            endif
C
C-----------Symmetrize P4, if needed
            if(ispc.ge.10 .and. ispc.le.11)then
              if(K.ge.0)then
                NH=K
                NK=-H
                NL=L
                if(ISTHERE(NH,NK,NL).ne.0)then
                  RPT=RFPHASE(NH,NK,NL)*PI/180.0
                  AMP=AMP+RFAMP(NH,NK,NL)*RFFOM(NH,NK,NL)
                  PX=PX+cos(RPT)*RFFOM(NH,NK,NL)
                  PY=PY+sin(RPT)*RFFOM(NH,NK,NL)
                  FOM=FOM+RFFOM(NH,NK,NL)
                  SIGA=SIGA+RFSIGA(NH,NK,NL)*RFFOM(NH,NK,NL)
                  BACK=BACK+RFBACK(NH,NK,NL)*RFFOM(NH,NK,NL)
                  ianz=ianz+1
                endif
              endif
            endif
C
            if(ispc.eq.12)then
C-------------Symmetrize P4212
C-------------To be done.
            endif
C
C-----------Symmetrize P3, if needed
            if(ispc.ge.13 .and. ispc.le.15)then
              if(K.ge.0)then
                NH=K
                NK=-H-K
                NL=L
                if(ISTHERE(NH,NK,NL).ne.0)then
                  RPT=RFPHASE(NH,NK,NL)*PI/180.0
                  AMP=AMP+RFAMP(NH,NK,NL)*RFFOM(NH,NK,NL)
                  PX=PX+cos(RPT)*RFFOM(NH,NK,NL)
                  PY=PY+sin(RPT)*RFFOM(NH,NK,NL)
                  FOM=FOM+RFFOM(NH,NK,NL)
                  SIGA=SIGA+RFSIGA(NH,NK,NL)*RFFOM(NH,NK,NL)
                  BACK=BACK+RFBACK(NH,NK,NL)*RFFOM(NH,NK,NL)
                  ianz=ianz+1
                endif
                NH=-H-K
                NK=-H
                NL=L
                if(ISTHERE(NH,NK,NL).ne.0)then
                  RPT=RFPHASE(NH,NK,NL)*PI/180.0
                  AMP=AMP+RFAMP(NH,NK,NL)*RFFOM(NH,NK,NL)
                  PX=PX+cos(RPT)*RFFOM(NH,NK,NL)
                  PY=PY+sin(RPT)*RFFOM(NH,NK,NL)
                  FOM=FOM+RFFOM(NH,NK,NL)
                  SIGA=SIGA+RFSIGA(NH,NK,NL)*RFFOM(NH,NK,NL)
                  BACK=BACK+RFBACK(NH,NK,NL)*RFFOM(NH,NK,NL)
                  ianz=ianz+1
                endif
              endif
            endif
C
C-----------Symmetrize P6, if needed
            if(ispc.ge.16 .and. ispc.le.17)then
              if(K.ge.0 .and. L.ge.0)then
                NH=-K
                NK=H+K
                NL=L
                if(ISTHERE(NH,NK,NL).ne.0)then
                  RPT=RFPHASE(NH,NK,NL)*PI/180.0
                  AMP=AMP+RFAMP(NH,NK,NL)*RFFOM(NH,NK,NL)
                  PX=PX+cos(RPT)*RFFOM(NH,NK,NL)
                  PY=PY+sin(RPT)*RFFOM(NH,NK,NL)
                  FOM=FOM+RFFOM(NH,NK,NL)
                  SIGA=SIGA+RFSIGA(NH,NK,NL)*RFFOM(NH,NK,NL)
                  BACK=BACK+RFBACK(NH,NK,NL)*RFFOM(NH,NK,NL)
                  ianz=ianz+1
                endif
                NH=-H-K
                NK=H
                NL=L
                if(ISTHERE(NH,NK,NL).ne.0)then
                  RPT=RFPHASE(NH,NK,NL)*PI/180.0
                  AMP=AMP+RFAMP(NH,NK,NL)*RFFOM(NH,NK,NL)
                  PX=PX+cos(RPT)*RFFOM(NH,NK,NL)
                  PY=PY+sin(RPT)*RFFOM(NH,NK,NL)
                  FOM=FOM+RFFOM(NH,NK,NL)
                  SIGA=SIGA+RFSIGA(NH,NK,NL)*RFFOM(NH,NK,NL)
                  BACK=BACK+RFBACK(NH,NK,NL)*RFFOM(NH,NK,NL)
                  ianz=ianz+1
                endif
                NH=-H
                NK=-K
                NL=L
                if(ISTHERE(NH,NK,NL).ne.0)then
                  RPT=RFPHASE(NH,NK,NL)*PI/180.0
                  AMP=AMP+RFAMP(NH,NK,NL)*RFFOM(NH,NK,NL)
                  PX=PX+cos(RPT)*RFFOM(NH,NK,NL)
                  PY=PY+sin(RPT)*RFFOM(NH,NK,NL)
                  FOM=FOM+RFFOM(NH,NK,NL)
                  SIGA=SIGA+RFSIGA(NH,NK,NL)*RFFOM(NH,NK,NL)
                  BACK=BACK+RFBACK(NH,NK,NL)*RFFOM(NH,NK,NL)
                  ianz=ianz+1
                endif
                NH=K
                NK=-H-K
                NL=L
                if(ISTHERE(NH,NK,NL).ne.0)then
                  RPT=RFPHASE(NH,NK,NL)*PI/180.0
                  AMP=AMP+RFAMP(NH,NK,NL)*RFFOM(NH,NK,NL)
                  PX=PX+cos(RPT)*RFFOM(NH,NK,NL)
                  PY=PY+sin(RPT)*RFFOM(NH,NK,NL)
                  FOM=FOM+RFFOM(NH,NK,NL)
                  SIGA=SIGA+RFSIGA(NH,NK,NL)*RFFOM(NH,NK,NL)
                  BACK=BACK+RFBACK(NH,NK,NL)*RFFOM(NH,NK,NL)
                  ianz=ianz+1
                endif
                NH=H+K
                NK=-H
                NL=L
                if(ISTHERE(NH,NK,NL).ne.0)then
                  RPT=RFPHASE(NH,NK,NL)*PI/180.0
                  AMP=AMP+RFAMP(NH,NK,NL)*RFFOM(NH,NK,NL)
                  PX=PX+cos(RPT)*RFFOM(NH,NK,NL)
                  PY=PY+sin(RPT)*RFFOM(NH,NK,NL)
                  FOM=FOM+RFFOM(NH,NK,NL)
                  SIGA=SIGA+RFSIGA(NH,NK,NL)*RFFOM(NH,NK,NL)
                  BACK=BACK+RFBACK(NH,NK,NL)*RFFOM(NH,NK,NL)
                  ianz=ianz+1
                endif
              endif
            endif
C
            if(ianz.gt.0)then
C              write(*,'('':: write '',5I8)')H,K,L,ispc,ianz
              if(FOM.gt.0.0)then
                AMP=AMP/FOM
                SIGA=SIGA/FOM
                BACK=BACK/FOM
              else
                goto 960
              endif
              PHASE=atan2(PY,PX)*180.0/PI
              if(PHASE.lt.0.0)PHASE=PHASE+360.0
              if(PHASE.lt.0.0)PHASE=PHASE+360.0
              if(PHASE.lt.0.0)PHASE=PHASE+360.0
              if(PHASE.lt.0.0)PHASE=PHASE+360.0
              if(PHASE.gt.360.0)PHASE=PHASE-360.0
              if(PHASE.gt.360.0)PHASE=PHASE-360.0
              if(PHASE.gt.360.0)PHASE=PHASE-360.0
              if(PHASE.gt.360.0)PHASE=PHASE-360.0
C-------------Should FOM be plainly averaged (division by ianz here), or rather made better, by some kind of SQRT(N)? ToDo
              FOM=FOM/ianz
C
C-----------ROUTP contains AMP,PHS,BACK,FOM,SIGA
C 
C Table of phase comparisons to be made
C       -  not comparable       
C       1  directly identical
C       H  differ by 180 * H            JSIMPL  = number to compare directly
C       K  differ by 180 * K            JSCREW   = number to compare + 180 * M
C       HK differ by 180 * (H+K)         where M = H*JH180 + K*JK180
C                                        
C
C   SPACEGROUP  H=-h +h -h +k +k -k -k +h -h +k -k -h +h -h +h  JSIMPL
C               H=                                 -k +k -k +k     JSCREW
C ref in
C  prog # symb  K=+k -k -k +h -h +h -h -h +h -h +h +h -h +k -k         JH180
C               K=                     -k +k -k +k                         JK180
C
C  1    1   p1     -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  0   -   -
C  2    2   p2     -  -  1  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  3    3b  p12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  4    "a   "     -  1  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  5    4b  p121   K  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  1   -  180
C  6    "a   "     -  H  -  -  -  -  -  -  -  -  -  -  -  -  -   0  1  180  -
C  7    5b  c12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  8    "a   "     -  1  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  9    6   p222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -
C 10    7b  p2221  H  H  1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180  -
C 11    "a    "    K  K  1  -  -  -  -  -  -  -  -  -  -  -  -   1  2   -  180
C 12    8   p22121 HK HK 1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180 180
C 13    9   c222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -
C 14    10  p4     -  -  1  -  1  1  -  -  -  -  -  -  -  -  -   3  0   -   -
C 15    11  p422   1  1  1  1  1  1  1  -  -  -  -  -  -  -  -   7  0   -   -
C 16    12  p4212  HK HK 1  1  HK HK 1  -  -  -  -  -  -  -  -   3  4  180 180
C 17    13  p3     -  -  -  -  -  -  -  -  -  1  -  1  -  -  -   2  0   -   -
C 18    14  p312   -  -  -  -  -  -  1  -  1  1  -  1  -  -  1   5  0   -   -
C 19    15  p321   -  -  -  1  -  -  -  1  -  1  -  1  -  1  -   5  0   -   -
C 20    16  p6     -  -  1  -  -  -  -  -  -  1  1  1  1  -  -   5  0   -   -
C 21    17  p622   -  -  1  1  -  -  1  1  1  1  1  1  1  1  1   11 0   -   -
C
              if(ispc.lt.10 .or. K.ge.0)then
C---------------Normal case
                call ROUTFIL(ROUTP, H, K, L,AMP, PHASE,BACK,FOM,SIGA)
              endif
              if(ispc.ge.10 .and. ispc.le.11)then
                if(K.ge.0)then
C-----------------P4
                  call ROUTFIL(ROUTP,-H,-K, L,AMP, PHASE,BACK,FOM,SIGA)
                  call ROUTFIL(ROUTP,-K, H, L,AMP, PHASE,BACK,FOM,SIGA)
                  call ROUTFIL(ROUTP, K,-H, L,AMP, PHASE,BACK,FOM,SIGA)
                endif
              endif
              if(ispc.eq.12)then
                if(K.ge.0)then
C-----------------P4212
                  call ROUTFIS(ROUTP,-H, K, L,AMP, PHASE,BACK,FOM,SIGA,4)
                  call ROUTFIS(ROUTP, H,-K, L,AMP, PHASE,BACK,FOM,SIGA,4)
                  call ROUTFIS(ROUTP,-H,-K, L,AMP, PHASE,BACK,FOM,SIGA,1)
                  call ROUTFIS(ROUTP, K, H, L,AMP, PHASE,BACK,FOM,SIGA,1)
                  call ROUTFIS(ROUTP, K,-H, L,AMP, PHASE,BACK,FOM,SIGA,4)
                  call ROUTFIS(ROUTP,-K, H, L,AMP, PHASE,BACK,FOM,SIGA,4)
                  call ROUTFIS(ROUTP,-K,-H, L,AMP, PHASE,BACK,FOM,SIGA,1)
                endif
              endif
              if(ispc.ge.13 .and. ispc.le.15)then
                if(K.ge.0)then
C-----------------P3
                  call ROUTFIL(ROUTP,-H-K, H, L,AMP, PHASE,BACK,FOM,SIGA)
                  call ROUTFIL(ROUTP, K,-H-K, L,AMP, PHASE,BACK,FOM,SIGA)
                endif
              endif
              if(ispc.ge.16 .and. ispc.le.17)then
                if(K.ge.0.and.L.ge.0)then
C-----------------P6
                  call ROUTFIL(ROUTP,-K, H+K, L,AMP, PHASE,BACK,FOM,SIGA)
                  call ROUTFIL(ROUTP,-H-K, H, L,AMP, PHASE,BACK,FOM,SIGA)
                  call ROUTFIL(ROUTP,-H,  -K, L,AMP, PHASE,BACK,FOM,SIGA)
                  call ROUTFIL(ROUTP, K,-H-K, L,AMP, PHASE,BACK,FOM,SIGA)
                  call ROUTFIL(ROUTP, H+K,-H, L,AMP, PHASE,BACK,FOM,SIGA)
                endif
              endif
            endif
C
          enddo
        enddo
      enddo
C
C-----Now write out all reflexes in the correct sorting
C
      do H=0,ihmax
        do K=-ikmax,ikmax
          do L=-ilmax,ilmax
C
            AMP  =ROUTP(H,K,L,1)
            PHASE=ROUTP(H,K,L,2)
            BACK =ROUTP(H,K,L,3)
            FOM  =ROUTP(H,K,L,4)
            SIGA =ROUTP(H,K,L,5)
            RFILL=ROUTP(H,K,L,6)
C
            if(RFILL.gt.0.1)then
              if(isig.eq.1)then
                write(11,310) H,K,L,AMP,PHASE,FOM,SIGA
              elseif(isig.eq.2)then
                write(11,310) H,K,L,AMP,PHASE,BACK,FOM
              else
                write(11,300) H,K,L,AMP,PHASE,FOM
              endif
            endif
C
          enddo
        enddo
      enddo
C
      close(11)
C
 100  FORMAT (A50)
 200  FORMAT (3I6,G16.8,G16.8,G16.8)
 210  FORMAT (3I6,G16.8,G16.8,G16.8,G16.8)
 300  FORMAT (3I6,G16.8,G16.8,G16.8)
 310  FORMAT (3I6,G16.8,G16.8,G16.8,G16.8)
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
 999  continue
      STOP

      END     
C
C==========================================================
C
      SUBROUTINE ROUTFIL(ROUTP,H,K,L,AMP,PHASE,BACK,FOM,SIGA)
C
C fill in the AMP,PHASE,FOM,SIGA into ROUTP field
C
      PARAMETER (MAXSPOT = 100)
      INTEGER H,K,L
      REAL ROUTP(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,6)
C
      if(abs(H).gt.MAXSPOT)STOP'too big index for ROUTP'
      if(abs(K).gt.MAXSPOT)STOP'too big index for ROUTP'
      if(abs(L).gt.MAXSPOT)STOP'too big index for ROUTP'
C      
      ROUTP( H, K, L,1) = AMP
      ROUTP( H, K, L,2) = PHASE
      ROUTP( H, K, L,3) = BACK
      ROUTP( H, K, L,4) = FOM
      ROUTP( H, K, L,5) = SIGA
C
C-----Also fill Friedel symmetric spots:
C      ROUTP(-H,-K,-L,1) = AMP
C      ROUTP(-H,-K,-L,2) = -PHASE
C      ROUTP(-H,-K,-L,3) = BACK
C      ROUTP(-H,-K,-L,4) = FOM
C      ROUTP(-H,-K,-L,5) = SIGA
C
C      ROUTP( H, K, L,6) = 1.0
C      ROUTP(-H,-K,-L,6) = 1.0
C
      RETURN
C
      END
C
C==========================================================
C
      SUBROUTINE ROUTFIS(ROUTP,H,K,L,AMP,PHASE,BACK,FOM,SIGA,IPHSHIFT)
C
C fill in the AMP,PHASE,FOM,SIGA into ROUTP field
C IPHSHIFT is defined as 
C   0 = -  not comparable       
C   1 = 1  directly identical
C   2 = H  differ by 180 * H 
C   3 = K  differ by 180 * K 
C   4 = HK differ by 180 * (H+K)
C
C  Table of phase comparisons to be made
C       -  not comparable       
C       1  directly identical
C       H  differ by 180 * H            JSIMPL  = number to compare directly
C       K  differ by 180 * K            JSCREW   = number to compare + 180 * M
C       HK differ by 180 * (H+K)         where M = H*JH180 + K*JK180
C                                        
C
C   SPACEGROUP  H=-h +h -h +k +k -k -k +h -h +k -k -h +h -h +h  JSIMPL
C               H=                                 -k +k -k +k     JSCREW
C ref in
C  prog # symb  K=+k -k -k +h -h +h -h -h +h -h +h +h -h +k -k         JH180
C               K=                     -k +k -k +k                         JK180
C
C  1    1   p1     -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  0   -   -
C  2    2   p2     -  -  1  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  3    3b  p12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  4    "a   "     -  1  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  5    4b  p121   K  -  -  -  -  -  -  -  -  -  -  -  -  -  -   0  1   -  180
C  6    "a   "     -  H  -  -  -  -  -  -  -  -  -  -  -  -  -   0  1  180  -
C  7    5b  c12    1  -  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  8    "a   "     -  1  -  -  -  -  -  -  -  -  -  -  -  -  -   1  0   -   -
C  9    6   p222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -
C 10    7b  p2221  H  H  1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180  -
C 11    "a    "    K  K  1  -  -  -  -  -  -  -  -  -  -  -  -   1  2   -  180
C 12    8   p22121 HK HK 1  -  -  -  -  -  -  -  -  -  -  -  -   1  2  180 180
C 13    9   c222   1  1  1  -  -  -  -  -  -  -  -  -  -  -  -   3  0   -   -
C 14    10  p4     -  -  1  -  1  1  -  -  -  -  -  -  -  -  -   3  0   -   -
C 15    11  p422   1  1  1  1  1  1  1  -  -  -  -  -  -  -  -   7  0   -   -
C 16    12  p4212  HK HK 1  1  HK HK 1  -  -  -  -  -  -  -  -   3  4  180 180
C 17    13  p3     -  -  -  -  -  -  -  -  -  1  -  1  -  -  -   2  0   -   -
C 18    14  p312   -  -  -  -  -  -  1  -  1  1  -  1  -  -  1   5  0   -   -
C 19    15  p321   -  -  -  1  -  -  -  1  -  1  -  1  -  1  -   5  0   -   -
C 20    16  p6     -  -  1  -  -  -  -  -  -  1  1  1  1  -  -   5  0   -   -
C 21    17  p622   -  -  1  1  -  -  1  1  1  1  1  1  1  1  1   11 0   -   -
C
C
      PARAMETER (MAXSPOT = 100)
      INTEGER H,K,L
      REAL ROUTP(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,6)
C
      if(abs(H).gt.MAXSPOT)STOP'too big index for ROUTP'
      if(abs(K).gt.MAXSPOT)STOP'too big index for ROUTP'
      if(abs(L).gt.MAXSPOT)STOP'too big index for ROUTP'
C      
      if(ISHIFT.eq.2)then
        PHASE=PHASE+H*180.0
      endif
      if(ISHIFT.eq.3)then
        PHASE=PHASE+K*180.0
      endif
      if(ISHIFT.eq.4)then
        PHASE=PHASE+(H+K)*180.0
      endif
 100  continue
      if(PHASE.gt.360.0)then
        PHASE=PHASE-360.0
        goto 100
      endif
 110  continue
      if(PHASE.lt.0.0)then
        PHASE=PHASE+360.0
        goto 110
      endif
C
      ROUTP( H, K, L,1) = AMP
      ROUTP( H, K, L,2) = PHASE
      ROUTP( H, K, L,3) = BACK
      ROUTP( H, K, L,4) = FOM
      ROUTP( H, K, L,5) = SIGA
C
C-----Also fill Friedel symmetric spots:
      ROUTP(-H,-K,-L,1) = AMP
      ROUTP(-H,-K,-L,2) = -PHASE
      ROUTP(-H,-K,-L,3) = BACK
      ROUTP(-H,-K,-L,4) = FOM
      ROUTP(-H,-K,-L,5) = SIGA
C
      ROUTP( H, K, L,6) = 1.0
      ROUTP(-H,-K,-L,6) = 1.0
C
      RETURN
C
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

