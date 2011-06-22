      PROGRAM HKLSYM

C       Henning Stahlberg, 16.5.2000
C       updated 4/19/2008
c
C-----Program to complete the asymmetric unit into a full P1 unit
C

      PARAMETER (MAXSPOT = 100)
      CHARACTER*200  TITLE 
      CHARACTER*200 cline1,cline2,cline3,cline4
      INTEGER H,K,L
      REAL RFAMP(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT)
      REAL RFPHASE(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT)
      REAL RFFOM(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT)
      REAL RFSIGA(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT)
      REAL RFBACK(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT)
      REAL ROUTP(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,4)
      INTEGER ISTHERE(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT)
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
      write(*,'('' Give file name for output without header line'')')
      read(*,'(A)')cline3
      call shorten(cline3,k)
      write(*,'('' read: '',A)')cline3(1:k)
C
      write(cline4,'(''rm -f '',A)')cline3(1:k)
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
      write(*,'('' Is there a siga column (0=no,1=y,2=Background,'',
     1   ''3=no and setting sigf to one)'')')
      read(*,*)isig
      i1sig=0
      if(isig.eq.2)then
        write(*,'('' Assuming Background, FOM columns.'')')
      elseif(isig.eq.1)then
        write(*,'('' Assuming siga column.'')')
      elseif(isig.eq.3)then
        write(*,'('' Creating siga=1 column.'')')
        i1sig=1
        isig=0
      else
        write(*,'('' Assuming no siga column.'')')
      endif
C
      write(*,'('' Write out only asymmetric unit (1=y,0=n)'')')
      read(*,*)iwasym
      if(iwasym.eq.1)then
        write(*,'('' Writing only asymmetric unit.'')')
      else
        write(*,'('' Writing full p1 plane (but not for screw axes)'')')
      endif
C
      open(10,FILE=cline1,STATUS='OLD',ERR=940)
      open(11,FILE=cline2,STATUS='NEW',ERR=950)
      open(12,FILE=cline3,STATUS='NEW',ERR=950)
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
        if(i1sig.eq.1)SIGA=1.0
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
      if(i1sig.eq.1)then
C-------write out SIGF (as values of 1)
        isig=1
      endif
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
            if(ispc.ge.10 .and. ispc.le.12)then
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
C
C-----------Symmetrize P3, if needed
            if(ispc.ge.13 .and. ispc.le.15)then
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
C
C-----------Symmetrize P6, if needed
            if(ispc.ge.16 .and. ispc.le.17)then
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
C
            if(ianz.gt.0)then
C             write(*,'('':: write '',5I8)')H,K,L,ispc,ianz
              if(FOM.gt.0.0)then
                AMP=AMP/FOM
                SIGA=SIGA/FOM
                BACK=BACK/FOM
              else
                goto 960
              endif
              PHASE=atan2(PY,PX)*180.0/PI
              if(PHASE.lt.-180.0)PHASE=PHASE+360.0
              if(PHASE.lt.-180.0)PHASE=PHASE+360.0
              if(PHASE.gt.180.0)PHASE=PHASE-360.0
              if(PHASE.gt.180.0)PHASE=PHASE-360.0
              if(PHASE.lt.-179.9999 .and. PHASE.gt.-180.0001)PHASE=180.0
              if(PHASE.gt. 179.9999 .and. PHASE.lt. 180.0001)PHASE=180.0
              if(PHASE.gt.  -0.0001 .and. PHASE.lt.   0.0001)PHASE=  0.0
C-------------Should FOM be plainly averaged (division by ianz here), or rather made better, by some kind of SQRT(N)? ToDo
              FOM=FOM/ianz
C
C====================
C====================
C====================
C-------------Do not write out negative L values:
              ineg=0
C====================
C====================
C====================
C
C-------------ROUTP contains AMP.PHS,FOM,SIGA
C      REAL ROUTP(-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,-MAXSPOT:MAXSPOT,4)
C
              if(ispc.lt.10 .or. K.ge.0)then
                if(isig.eq.1)then
                  WRITE (11,310) H,K,L,AMP,PHASE,FOM,SIGA
                  WRITE (12,310) H,K,L,AMP,PHASE,FOM,SIGA
                  if(ineg.gt.0)then
                    WRITE (11,310) -H,-K,-L,AMP,-PHASE,FOM,SIGA
                    WRITE (12,310) -H,-K,-L,AMP,-PHASE,FOM,SIGA
                  endif
                elseif(isig.eq.2)then
                  WRITE (11,310) H,K,L,AMP,PHASE,BACK,FOM
                  WRITE (12,310) H,K,L,AMP,PHASE,BACK,FOM
                  if(ineg.gt.0)then
                    WRITE (11,310) -H,-K,-L,AMP,-PHASE,BACK,FOM
                    WRITE (12,310) -H,-K,-L,AMP,-PHASE,BACK,FOM
                  endif
                else
                  WRITE (11,300) H,K,L,AMP,PHASE,FOM
                  WRITE (12,300) H,K,L,AMP,PHASE,FOM
                  if(ineg.gt.0)then
                    WRITE (11,300) -H,-K,-L,AMP,-PHASE,FOM
                    WRITE (12,300) -H,-K,-L,AMP,-PHASE,FOM
                  endif
                endif
              endif
              if(ispc.ge.10 .and. ispc.le.12)then
                if(K.ge.0 .and. iwasym.eq.1)then
                  if(isig.eq.1)then
                    WRITE (11,310) -H,-K,L,AMP,PHASE,FOM,SIGA
                    WRITE (12,310) -H,-K,L,AMP,PHASE,FOM,SIGA
                    WRITE (11,310) -K,H,L,AMP,PHASE,FOM,SIGA
                    WRITE (11,310) K,-H,L,AMP,PHASE,FOM,SIGA
                    WRITE (12,310) -K,H,L,AMP,PHASE,FOM,SIGA
                    WRITE (12,310) K,-H,L,AMP,PHASE,FOM,SIGA
                    if(ineg.gt.0)then
                      WRITE (11,310) H,K,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (12,310) H,K,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (11,310) K,-H,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (11,310) -K,H,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (12,310) K,-H,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (12,310) -K,H,-L,AMP,-PHASE,FOM,SIGA
                    endif
                  elseif(isig.eq.2)then
                    WRITE (11,310) -H,-K,L,AMP,PHASE,BACK,FOM
                    WRITE (12,310) -H,-K,L,AMP,PHASE,BACK,FOM
                    WRITE (11,310) -K,H,L,AMP,PHASE,BACK,FOM
                    WRITE (11,310) K,-H,L,AMP,PHASE,BACK,FOM
                    WRITE (12,310) -K,H,L,AMP,PHASE,BACK,FOM
                    WRITE (12,310) K,-H,L,AMP,PHASE,BACK,FOM
                    if(ineg.gt.0)then
                      WRITE (11,310) H,K,-L,AMP,-PHASE,BACK,FOM 
                      WRITE (12,310) H,K,-L,AMP,-PHASE,BACK,FOM
                      WRITE (11,310) K,-H,-L,AMP,-PHASE,BACK,FOM
                      WRITE (11,310) -K,H,-L,AMP,-PHASE,BACK,FOM
                      WRITE (12,310) K,-H,-L,AMP,-PHASE,BACK,FOM
                      WRITE (12,310) -K,H,-L,AMP,-PHASE,BACK,FOM
                    endif
                  else
                    WRITE (11,300) -H,-K,L,AMP,PHASE,FOM
                    WRITE (12,300) -H,-K,L,AMP,PHASE,FOM
                    WRITE (11,300) -K,H,L,AMP,PHASE,FOM
                    WRITE (11,300) K,-H,L,AMP,PHASE,FOM
                    WRITE (12,300) -K,H,L,AMP,PHASE,FOM
                    WRITE (12,300) K,-H,L,AMP,PHASE,FOM
                    if(ineg.gt.0)then
                      WRITE (11,300) H,K,-L,AMP,-PHASE,FOM
                      WRITE (12,300) H,K,-L,AMP,-PHASE,FOM
                      WRITE (11,300) K,-H,-L,AMP,-PHASE,FOM
                      WRITE (11,300) -K,H,-L,AMP,-PHASE,FOM
                      WRITE (12,300) K,-H,-L,AMP,-PHASE,FOM
                      WRITE (12,300) -K,H,-L,AMP,-PHASE,FOM
                    endif
                  endif
                endif
              endif
C
              if(ispc.ge.13 .and. ispc.le.15)then
                if(K.ge.0 .and. iwasym.eq.1)then
                  if(isig.eq.1)then
                    WRITE (11,310) -H-K, H, L,AMP,PHASE,FOM,SIGA
                    WRITE (11,310)  K,-H-K, L,AMP,PHASE,FOM,SIGA
                    WRITE (12,310) -H-K, H, L,AMP,PHASE,FOM,SIGA
                    WRITE (12,310)  K,-H-K, L,AMP,PHASE,FOM,SIGA
                    if(ineg.gt.0)then
                      WRITE (11,310) +H+K,-H,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (11,310) -K,+H+K,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (12,310) +H+K,-H,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (12,310) -K,+H+K,-L,AMP,-PHASE,FOM,SIGA
                    endif
                  elseif(isig.eq.2)then
                    WRITE (11,310) -H-K, H, L,AMP,PHASE,BACK,FOM
                    WRITE (11,310)  K,-H-K, L,AMP,PHASE,BACK,FOM
                    WRITE (12,310) -H-K, H, L,AMP,PHASE,BACK,FOM
                    WRITE (12,310)  K,-H-K, L,AMP,PHASE,BACK,FOM
                    if(ineg.gt.0)then
                      WRITE (11,310) +H+K,-H,-L,AMP,-PHASE,BACK,FOM
                      WRITE (11,310) -K,+H+K,-L,AMP,-PHASE,BACK,FOM
                      WRITE (12,310) +H+K,-H,-L,AMP,-PHASE,BACK,FOM
                      WRITE (12,310) -K,+H+K,-L,AMP,-PHASE,BACK,FOM
                    endif
                  else
                    WRITE (11,300) -H-K, H, L,AMP,PHASE,FOM
                    WRITE (11,300)  K,-H-K, L,AMP,PHASE,FOM
                    WRITE (12,300) -H-K, H, L,AMP,PHASE,FOM
                    WRITE (12,300)  K,-H-K, L,AMP,PHASE,FOM
                    if(ineg.gt.0)then
                      WRITE (11,300) +H+K,-H,-L,AMP,-PHASE,FOM
                      WRITE (11,300) -K,+H+K,-L,AMP,-PHASE,FOM
                      WRITE (12,300) +H+K,-H,-L,AMP,-PHASE,FOM
                      WRITE (12,300) -K,+H+K,-L,AMP,-PHASE,FOM
                    endif
                  endif
                endif
              endif
              if(ispc.ge.16 .and. ispc.le.17)then
                if(K.ge.0.and.L.ge.0 .and. iwasym.eq.1)then
                  if(isig.eq.1)then
                    WRITE (11,310) -K, H+K, L,AMP,PHASE,FOM,SIGA
                    WRITE (11,310) -H-K, H, L,AMP,PHASE,FOM,SIGA
                    WRITE (11,310) -H,  -K, L,AMP,PHASE,FOM,SIGA
                    WRITE (11,310)  K,-H-K, L,AMP,PHASE,FOM,SIGA
                    WRITE (11,310)  H+K,-H, L,AMP,PHASE,FOM,SIGA
                    WRITE (12,310) -K, H+K, L,AMP,PHASE,FOM,SIGA
                    WRITE (12,310) -H-K, H, L,AMP,PHASE,FOM,SIGA
                    WRITE (12,310) -H,  -K, L,AMP,PHASE,FOM,SIGA
                    WRITE (12,310)  K,-H-K, L,AMP,PHASE,FOM,SIGA
                    WRITE (12,310)  H+K,-H, L,AMP,PHASE,FOM,SIGA
                    if(ineg.gt.0)then
                      WRITE (11,310) -H-K, H,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (11,310) +K,-H-K,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (11,310)  H+K,-H,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (11,310)  H,   K,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (11,310) -K, H+K,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (12,310) +K,-H-K,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (12,310)  H+K,-H,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (12,310)  H,   K,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (12,310) -K, H+K,-L,AMP,-PHASE,FOM,SIGA
                      WRITE (12,310) -H-K, H,-L,AMP,-PHASE,FOM,SIGA
                    endif
                  elseif(isig.eq.2)then
                    WRITE (11,310) -K, H+K, L,AMP,PHASE,BACK,FOM
                    WRITE (11,310) -H-K, H, L,AMP,PHASE,BACK,FOM
                    WRITE (11,310) -H,  -K, L,AMP,PHASE,BACK,FOM
                    WRITE (11,310)  K,-H-K, L,AMP,PHASE,BACK,FOM
                    WRITE (11,310)  H+K,-H, L,AMP,PHASE,BACK,FOM
                    WRITE (12,310) -K, H+K, L,AMP,PHASE,BACK,FOM
                    WRITE (12,310) -H-K, H, L,AMP,PHASE,BACK,FOM
                    WRITE (12,310) -H,  -K, L,AMP,PHASE,BACK,FOM
                    WRITE (12,310)  K,-H-K, L,AMP,PHASE,BACK,FOM
                    WRITE (12,310)  H+K,-H, L,AMP,PHASE,BACK,FOM
                    if(ineg.gt.0)then
                      WRITE (11,310) +K,-H-K,-L,AMP,-PHASE,BACK,FOM
                      WRITE (11,310)  H+K,-H,-L,AMP,-PHASE,BACK,FOM
                      WRITE (11,310)  H,   K,-L,AMP,-PHASE,BACK,FOM
                      WRITE (11,310) -K, H+K,-L,AMP,-PHASE,BACK,FOM
                      WRITE (11,310) -H-K, H,-L,AMP,-PHASE,BACK,FOM
                      WRITE (12,310) +K,-H-K,-L,AMP,-PHASE,BACK,FOM
                      WRITE (12,310)  H+K,-H,-L,AMP,-PHASE,BACK,FOM
                      WRITE (12,310)  H,   K,-L,AMP,-PHASE,BACK,FOM
                      WRITE (12,310) -K, H+K,-L,AMP,-PHASE,BACK,FOM
                      WRITE (12,310) -H-K, H,-L,AMP,-PHASE,BACK,FOM
                    endif
                  else
                    WRITE (11,300) -K, H+K, L,AMP,PHASE,FOM
                    WRITE (11,300) -H-K, H, L,AMP,PHASE,FOM
                    WRITE (11,300) -H,  -K, L,AMP,PHASE,FOM
                    WRITE (11,300)  K,-H-K, L,AMP,PHASE,FOM
                    WRITE (11,300)  H+K,-H, L,AMP,PHASE,FOM
                    WRITE (12,300) -K, H+K, L,AMP,PHASE,FOM
                    WRITE (12,300) -H-K, H, L,AMP,PHASE,FOM
                    WRITE (12,300) -H,  -K, L,AMP,PHASE,FOM
                    WRITE (12,300)  K,-H-K, L,AMP,PHASE,FOM
                    WRITE (12,300)  H+K,-H, L,AMP,PHASE,FOM
                    if(ineg.gt.0)then
                      WRITE (11,300) +K,-H-K,-L,AMP,-PHASE,FOM
                      WRITE (11,300)  H+K,-H,-L,AMP,-PHASE,FOM
                      WRITE (11,300)  H,   K,-L,AMP,-PHASE,FOM
                      WRITE (11,300) -K, H+K,-L,AMP,-PHASE,FOM
                      WRITE (11,300) -H-K, H,-L,AMP,-PHASE,FOM
                      WRITE (12,300) +K,-H-K,-L,AMP,-PHASE,FOM
                      WRITE (12,300)  H+K,-H,-L,AMP,-PHASE,FOM
                      WRITE (12,300)  H,   K,-L,AMP,-PHASE,FOM
                      WRITE (12,300) -K, H+K,-L,AMP,-PHASE,FOM
                      WRITE (12,300) -H-K, H,-L,AMP,-PHASE,FOM
                    endif
                  endif
                endif
              endif
            endif
C
          enddo
        enddo
      enddo
C
      close(11)
      close(12)
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

