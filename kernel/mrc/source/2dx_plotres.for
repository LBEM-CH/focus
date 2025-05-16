C*******************************************************************************
C
C PLOTRES.FOR
C
C This program was contributed in 2008 by Dilem HIZLAN, PhD, 
C at the MPI Frankfurt, Germany.
C It was written by Janet Vonck, PhD, at the MPI Frankfurt, Germany, in the group
C of Werner Kuehlbrandt.
C
C Included in the 2dx package on July 21, 2008. Henning.
C Adapted for 2dx and image2000 conventions by Bryant Gipson and Henning Stahlberg.
C
C*******************************************************************************
C
C PLOTRES : is like PLOTALL, but uses IQ values from input of an expanded
C           single image and plots spots in resolution bands
C  Modified 5/15/96 for options of A,B,GAMMA or AX,AY,BX,BY etc.  AC
C  Modified 6/25/08 for variable number of rings, and 2dx environment. HS
C
      DIMENSION TITLE(17)
      DIMENSION TEXT(2)
      DIMENSION RESTORE(100) !UP TO 100 RESOLUTION CYRCLE
C
      CHARACTER*80 CNAME
C    
      CHARACTER*2 TMPTXT
      EQUIVALENCE (TMPTXT,TEXT)
C
      COMMON //RESMAX,SCALE,RESTORE,NRES,NSPOTS,RES,RAD,CNAME
C
      DRAD=3.141592654/180.0
      write(*,'('':OPTIONS: (1)INPUT A,B,GAMMA.'')')
      write(*,'('':         (2)INPUT AX,AY,BX,BY,'',
     .          ''IMAGE SIZE,STEP,MAG.'')')
      READ(5,*)IOPTION
      IF (IOPTION.EQ.1) THEN
      READ(5,*)ACELL,BCELL,ABANG
      WRITE(6,12)ACELL,BCELL,ABANG
        AX=1.0/(ACELL*SIN(DRAD*ABANG))
        AY=0
        BY=1.0/BCELL
        BX=-COS(DRAD*ABANG)/(BCELL*SIN(DRAD*ABANG))
      ELSE
        READ(5,*)AX,AY,BX,BY,RDIM,RSTEP,AMAG
        FACTOR = AMAG/(RDIM*RSTEP*10000)
        AX = AX*FACTOR
        AY = AY*FACTOR
        BX = BX*FACTOR
        BY = BY*FACTOR     
      ENDIF
C
      write(*,'('':Input name of input APH file'')')
      read(5,'(A80)')CNAME
      call shorten(CNAME,k)
      write(*,'('':Read: '',A)')CNAME(1:k)
C
      CALL CCPDPN(1,CNAME,'READONLY','F', 0, 0)
      READ(1,10)TITLE
10    FORMAT(20A4)
12    FORMAT(': A,B,ABANG:',3F8.2)
      IQ=1
      CALL TTPLOT(0,0,IQ,AX,BX,AY,BY,TITLE)
C      icount=20
20    READ(1,*,END=900)IH,IK,AMP,PHASE,IQ,BCK,CTF 
C        icount=icount-1
C        write(*,'(''READ: '',2I9,2G16.6,I3)')IH,IK,AMP,PHASE,IQ
        CALL TTPLOT(IH,IK,IQ,AX,BX,AY,BY,TITLE)
        CALL TTPLOT(-IH,-IK,IQ,AX,BX,AY,BY,TITLE)
C      if(icount.gt.0) GOTO 20
       GOTO 20
C
900   CALL TTPLOT(999,999,IQ,AX,BX,AY,BY,TITLE)
      END
C*******************************************************************************
C
      SUBROUTINE TTPLOT(IH,IK,IQ,AX,BX,AY,BY,TITLE)
      PARAMETER (PLTSIZ=300.0)
      PARAMETER (CHRSIZ=0.3)
      PARAMETER (FONTSIZE=2.0)
      DIMENSION TITLE(17)
      DIMENSION RTITLE(80)
      DIMENSION TEXT(2)
      DIMENSION RESTORE(100) !UP TO 100 RESOLUTION CYRCLE
      CHARACTER*2 TMPTXT
      CHARACTER*80 CTITLE
      CHARACTER*80 CNAME
      CHARACTER*200 CRINGS
      EQUIVALENCE (CTITLE,RTITLE)
      EQUIVALENCE (TMPTXT,TEXT)
C
      COMMON //RESMAX,SCALE,RESTORE,NRES,NSPOTS,RES,RAD,CNAME
C

C       If IH=IK=0    the plot is initialised
C       If IH=IK=999  the plot is terminated
C
C  Here for initialisation
C
      IF(IH.EQ.0.AND.IK.EQ.0) THEN

        write(*,'('':Input RESOLUTION MAXIMUM IN ANGSTROMS'')')
        READ (5,*)RESMAX
        RORIRESMAX=RESMAX
        if(RESMAX.GT.0.0)THEN
          RESMAX = 1.0/RESMAX
        else
          write(*,'(''::ERROR in resolution specification'')')
          RESMAX=0.1
        endif
        write(*,'('':Reciprocal RESOLUTION '',F12.3)'),RESMAX
        SCALE=PLTSIZ/(2.0*RESMAX)
C
        write(*,'(''Input comma-separated list of resolution values'')')
        read(5,'(A)')CRINGS
C
        call inkomma(CRINGS,k)
        call getnumbers(CRINGS,NRES,RESTORE)
C
        write(*,'(''Got '',I6,'' numbers:'')')NRES
        do i=1,NRES
          if(RORIRESMAX.le.RESTORE(i))then
            write(*,'(''Number '',I6,'' = '',F10.3)')i,RESTORE(i)
          else
            write(*,'(''::WARNING: Ring'',I3,
     .        ''('',F8.1,''A) is beyond limit of ''
     .        ,F10.3,''A. Not plotted.'')')i,RESTORE(i),RORIRESMAX
          endif
        enddo
C
        WRITE(6,20)
20      FORMAT(': ENTERING TTPLOT INITIALISATION')
        NSPOTS = 0
C
        CALL P2K_OUTFILE('PLOT.PS',10)
        CALL P2K_HOME
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE)
        CALL P2K_GRID(0.55*PLTSIZ,0.55*PLTSIZ,1.0)
        CALL P2K_ORIGIN(-0.5*PLTSIZ,-0.8*PLTSIZ,0.)
        CALL P2K_COLOUR(0)
        CALL P2K_LWIDTH(0.3)
C
        YPOSN=PLTSIZ+120.
C
        CALL P2K_MOVE(0.,0.,0.)
        CALL P2K_ORIGIN(0.,0.,0.)
        CALL P2K_MOVE(0.,0.,0.)
        CALL P2K_MOVE(10.,YPOSN,0.)
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
C
        write(CTITLE,'(''PLOTRES resolution circle plot of'',
     .    '' APH file.'')')
        call shorten(CTITLE,k)
        CALL P2K_STRING(RTITLE,k,0.)
        YPOSN=YPOSN-15.0
C
        call shorten(CNAME,k)
        write(CTITLE,'(''Input file: '',A)')CNAME(1:k)
        call shorten(CTITLE,k)
        CALL P2K_MOVE(10.,YPOSN,0.)
        CALL P2K_STRING(RTITLE,k,0.)
        YPOSN=YPOSN-15.0
C
        CALL P2K_MOVE(10.,YPOSN,0.)
        CALL P2K_STRING(TITLE,60,0.)
        YPOSN=YPOSN-15.0
C
        write(CTITLE,'(''Resolution Max (Nyquist of plot) '',
     .       ''at '',F10.2,'' A'')')RORIRESMAX
        CALL P2K_MOVE(10.,YPOSN,0.)
        CALL P2K_STRING(RTITLE,60,0.)
        YPOSN=YPOSN-10.0
C
        DO N=1,NRES
          if(RESTORE(N).GE.RORIRESMAX)then
            write(CTITLE,'(''Resolution Ring at '',F10.2,'' A'')')RESTORE(N)
            YPOSN=YPOSN-10.0
            CALL P2K_MOVE(10.,YPOSN,0.)
            CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*2.0)
            CALL P2K_STRING(RTITLE,60,0.)
          endif
        enddo
C
        CALL P2K_MOVE(0.,0.,0.)
        CALL P2K_DRAW(0.,PLTSIZ,0.)
        CALL P2K_DRAW(PLTSIZ,PLTSIZ,0.)
        CALL P2K_DRAW(PLTSIZ,0.,0.)
        CALL P2K_DRAW(0.,0.,0.)
        CALL P2K_MOVE(0.,0.,0.)

        CENTRE=PLTSIZ/2.0
C
        CALL P2K_ORIGIN(CENTRE,CENTRE,0.)
C
        CALL P2K_MOVE(-CHRSIZ,-CHRSIZ,0.)
        CALL P2K_DRAW(CHRSIZ,CHRSIZ,0.)
        CALL P2K_MOVE(CHRSIZ,-CHRSIZ,0.)
        CALL P2K_DRAW(-CHRSIZ,CHRSIZ,0.)
C
        ALENGTH=SQRT(AX**2+AY**2)
        X=(AX/ALENGTH)*(PLTSIZ/2.0)
        Y=(AY/ALENGTH)*(PLTSIZ/2.0)
C
        CALL P2K_MOVE(0.,0.,0.)
        CALL P2K_DRAW(X,Y,0.)
C
        X=X+10.
        CALL P2K_MOVE(X,Y,0.)
C
        write(TMPTXT,'(''H'')')
        CALL P2K_CSTRING(TEXT,1,0.)
        BLENGTH=SQRT(BX**2+BY**2)
        X=(BX/BLENGTH)*(PLTSIZ/2.0)
        Y=(BY/BLENGTH)*(PLTSIZ/2.0)
C
        CALL P2K_MOVE(0.,0.,0.)
        CALL P2K_DRAW(X,Y,0.)
        CALL P2K_MOVE(X,Y,0.)
C
        X=X+10.
        CALL P2K_MOVE(X,Y,0.)
        write(TMPTXT,'(''K'')')
        CALL P2K_CSTRING(TEXT,1,0.)
        CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*1.0)
C
        DO 370 N=1,NRES                 ! NRES resoultion ranges
          if(RESTORE(N).GE.RORIRESMAX)then
            RES=1.0/RESTORE(N)
            RAD=SCALE*RES
            CALL P2K_MOVE(RAD,0.,0.)
            DO 360 I=1,360                      ! Resolution circles
              ANG=I*3.1415962/180.0
              X=RAD*COS(ANG)
              Y=RAD*SIN(ANG)
              CALL P2K_DRAW(X,Y,0.)
              CALL P2K_MOVE(X,Y,0.)
360         CONTINUE
          endif
370     CONTINUE
        RETURN
      ENDIF
C
C  Here for termination
C
      IF(IH.EQ.999) THEN
        WRITE(6,110) NSPOTS
110     FORMAT(' TOTAL SPOTS PLOTTED IN TTPLOT FILE =',I9,
     .    ' and plot file closed')
C
        call P2K_PAGE
C
        RETURN
      ENDIF
C
C  Here for spot plots
C
      IF(IQ.GT.8) RETURN                ! PLOTS SPOTS WITH IQ =8 OR LESS.
C     WRITE(6,21)IH,IK
21    FORMAT(' SPOT PLOTTED & FRIEDEL PAIR',2I5)
      J=1
      K=1
      X=IH*AX+IK*BX
      Y=IH*AY+IK*BY
      IF(ABS(X).GE.RESMAX)GO TO 100
      IF(ABS(Y).GE.RESMAX)GO TO 100
      X=X*SCALE
      Y=Y*SCALE
104   FORMAT(4F10.1)
      XN=X-CHRSIZ*(8.1-IQ)
      XP=X+CHRSIZ*(8.1-IQ)
      YN=Y-CHRSIZ*(8.1-IQ)
      YP=Y+CHRSIZ*(8.1-IQ)
      NSPOTS=NSPOTS+1
C
      CALL P2K_MOVE(XN,YN,0.)
      CALL P2K_DRAW(XP,YN,0.)
      CALL P2K_DRAW(XP,YP,0.)
      CALL P2K_DRAW(XN,YP,0.)
      CALL P2K_DRAW(XN,YN,0.)
C
      X=X-0.1                           ! ADJUST CHARACTER TO BE CENTRAL IN X.
      Y=Y-0.8                           ! ADJUST CHARACTER TO BE CENTRAL IN Y.
      CALL P2K_MOVE(X,Y,0.)
C
      write(TMPTXT,'('' '')')
      if(IQ.EQ.1) write(TMPTXT,'(''1'')')
      if(IQ.EQ.2) write(TMPTXT,'(''2'')')
      if(IQ.EQ.3) write(TMPTXT,'(''3'')')
      if(IQ.EQ.4) write(TMPTXT,'(''4'')')
160   FORMAT(' ')
161   FORMAT('1')
162   FORMAT('2')
163   FORMAT('3')
164   FORMAT('4')
      CALL P2K_FONT('Courier'//CHAR(0),FONTSIZE*1.0)
      IF (IQ.LE.4) CALL P2K_CSTRING(TEXT,1,0.)
100   CONTINUE
      RETURN
      END
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
c
      SUBROUTINE INKOMMA(CZEILE,k)
C
C replaces intermedieate spaces within the actual text string
C in CZEILE up to the length k by komma.
C
      CHARACTER * (*) CZEILE
      CHARACTER * 1 CTMP1,CTMP2,CTMP3
      CHARACTER * 100 CZEIL2
      CTMP2=' '
      CTMP3=','
C
      ilen=len(czeile)
      DO 70 I=1,ilen
         k=ilen+1-I
         READ(CZEILE(k:k),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)GOTO 80
  70  CONTINUE
  80  CONTINUE
      IF(k.LT.1)k=1
C
C-----find the leading spaces and remove them
C
      DO 90 J=1,k
         READ(CZEILE(J:J),'(A1)')CTMP1
         IF(CTMP1.NE.CTMP2)THEN
           GOTO 95
         ENDIF
 90   CONTINUE
 95   CONTINUE
C
      WRITE(CZEIL2(1:k),'(A)') CZEILE(J:k)
C
C     WRITE(*,'('' INKOMMA 2: k='',I4,'' J='',I4,'':'',A)')
C    1      k,j,CZEIL2(1:k)
C
C-----Was there a komma recently ?
      KWAR=0
C
      I=1
      L=1
 100  continue
        READ(CZEIL2(I:I),'(A1)')CTMP1
        IF(CTMP1.EQ.CTMP3)THEN
C---------There is a komma.
          IF(KWAR.EQ.0)THEN
C-----------There is a komma, before was no komma. Insert one.
            WRITE(CZEILE(L:L),'('','')')
            KWAR=1
          ELSE
C-----------There is a komma, before was already a komma. Shrink.
            L=L-1
          ENDIF
        ELSEIF(CTMP1.EQ.CTMP2)THEN
C---------There is a space.
          IF(KWAR.EQ.0)THEN
C-----------There is a space, before was no komma. Insert komma.
            WRITE(CZEILE(L:L),'('','')')
            KWAR=1
          ELSE
C-----------There is a space, before was a komma. Shrink.
            L=L-1
          ENDIF
        ELSE
C---------There is no komma, no space. Anulate KWAR, do nothing.
          WRITE(CZEILE(L:L),'(A1)')CZEIL2(I:I)
          KWAR=0
        ENDIF
        I=I+1
        L=L+1
C       WRITE(*,'('' INKOMMA 2b: I='',I4,'' L='',I3,'':'',A)')I,L,CTMP1
C
      IF(I.LE.K)GOTO 100
C
C     WRITE(*,'('' INKOMMA 3: k='',I4,'':'',A)')k,CZEILE(1:k)
C
      IF(L.LE.K)THEN
        WRITE(CZEILE(L:K),'('' '')')
      ENDIF
C
C-----Now check, if the last sign is a komma. If so, remove it.
C
      K=L
      I=K
 200  continue
        READ(CZEILE(I:I),'(A1)')CTMP1
        IF(CTMP1.NE.CTMP2)GOTO 220
        I=I-1
      IF(I.GE.1)GOTO 200
 220  CONTINUE
C
      IF(CTMP1.EQ.CTMP3)then
        WRITE(CZEILE(I:I),'('' '')')
        K=K-1
      endif
C
      if(k.lt.1)k=1
C
C     WRITE(*,'('' INKOMMA 4: k='',I4,'':'',A)')k,CZEILE(1:k)
C
      RETURN
      END
C
C===========================================================
C
      SUBROUTINE getnumbers(cline,NRES,RESTORE)
C
C Gets a list of komma-separated numbers from cline and places them into RESTORE
C The number of found values is returned in NRES
C
      character * (*) cline
      DIMENSION RESTORE(100)
      CHARACTER * 1 CTMP1
      CHARACTER * 1 CTMP2
      CHARACTER * 1 CTMP3
      CTMP1=' '
      CTMP2=' '
      CTMP3=','
C
      ilen=len(cline)
C
      call shorten(cline,k)
C      write(*,'(''::Entering getnumbers. cline='',A)')cline(1:k)
C
      NRES=0
      istart=1
C
C-----get numbers up to each komma
C
 100   continue
         k=istart
C--------find next komma
 200     continue
C           write(*,'(''::k='',I4,'', cline(k:k)='',A)')k,cline(k:k)
           READ(cline(k:k),'(A1)')CTMP1
           IF(CTMP1.NE.CTMP3)then
             k=k+1
             if(k.gt.ilen)goto300
             goto 200
           endif
  300    CONTINUE
C--------komma or end found, get number
         iend=k-1
         if(iend.lt.1)then
C----------string starts with kommas:
           istart=istart+1
           goto 100
         endif
         NRES=NRES+1
         if(NRES.GT.100)then
           NRES=100
           goto 900
         endif
C         write(*,'(''::Reading '',I6,'' from position '',
C     .      I4,'' to '',I4)')NRES,istart,iend
C         write(*,'(''::Reading from '',A,'' from position '',
C     .      I4,'' to '',I4)')cline(istart:iend),istart,iend
C
         read(cline(istart:iend),*)RESTORE(NRES)
         if(iend.ge.ilen)goto 999
         istart=iend+2
         if(istart.gt.ilen)goto 999
       goto 100
C
 900  continue
      write(*,'(''::WARNING: too many numbers'')')
C
 999  continue
C
      RETURN
      END
C
