      PROGRAM angback
C
      character*80 cfile1
C
      write(*,'(/,'' ANGBACK: give old values for TLTAXIS,TLTANG,TLTAXA'')')
      read(*,*)OTLTAXIS
      read(*,*)OTLTANG
      read(*,*)OTLTAXA
      write(*,'('' read: TLTAXIS = '',F12.3)')OTLTAXIS
      write(*,'(''       TLTANG  = '',F12.3)')OTLTANG
      write(*,'(''       TLTAXA  = '',F12.3)')OTLTAXA
C
      write(*,'(/,'' ANGBACK: give TAXA and TANGL'')')
      read(*,*)TAXA
      read(*,*)TANGL
      write(*,'('' read: TAXA = '',F12.3)')TAXA
      write(*,'(''       TANGL= '',F12.3)')TANGL
C
      write(*,'(/,''           give reciprocal lattice'')')
      read(*,*)rx1,ry1,rx2,ry2
      write(*,'('' read: lattice= '',4F12.3)')rx1,ry1,rx2,ry2
C
      write(*,'(/,''           give name for output file'')')
      read(*,'(A)')cfile1
      write(*,'('' read: '',A)')cfile1(1:40)
C
      DRAD = 3.14159265437 / 180.0
C 
C  Calculate TAXA,TANGL from input values TLTAXIS,TLTANGL.
      ANGA=ATAN2(ry1,rx1)/DRAD
      ANGB=ATAN2(ry2,rx2)/DRAD
      IF(ABS(ANGB-ANGA).GT.180.0) ANGB=ANGB-SIGN(360.0,ANGB-ANGA)
C
      write(*,'(/,'' ANGA      = '',2F12.3)')ANGA
      write(*,'('' ANGB      = '',2F12.3)')ANGB
C
      TLTAXA = ATAN(TAN(TAXA*DRAD) / cos(TANGL*DRAD)) / DRAD
C
      write(*,'(/,'' TLTAXA = '',F12.3)')TLTAXA
C
C-----TLTAXA should have the same sign as TAXA
      if(TLTAXA.lt.0)TLTAXA = -TLTAXA
      if(TAXA  .lt.0)TLTAXA = -TLTAXA
C
      write(*,'('' TLTAXA = '',F12.3)')TLTAXA
C
C  angle between A and B now between -180 and +180 degs.
C     TLTAXA=ANGA-TLTAXIS
C     TLTAXB=ANGB-TLTAXIS
C
      TLTAXIS = ANGA-TLTAXA
C
      write(*,'(/,'' TLTAXIS = '',F12.3)')TLTAXIS
C
      if(TLTAXIS.le.-90.0)TLTAXIS=TLTAXIS+180.0
      if(TLTAXIS.le.-90.0)TLTAXIS=TLTAXIS+180.0
      if(TLTAXIS.gt. 90.0)TLTAXIS=TLTAXIS-180.0
      if(TLTAXIS.gt. 90.0)TLTAXIS=TLTAXIS-180.0
C
      write(*,'('' TLTAXIS = '',F12.3)')TLTAXIS
C
      TLTAXA  = ANGA-TLTAXIS
      TLTAXB  = ANGB-TLTAXIS
C
      write(*,'(/,'' TLTAXA = '',F12.3)')TLTAXA
      write(*,'(/,'' TLTAXB = '',F12.3)')TLTAXB
C
      IF(TLTAXB-TLTAXA.GT.0.0) THEN             ! TLTAXB always greater than TLTAXA.
        HAND = 1.0
      ELSE
        HAND = -1.0
C
C-------??? is this correct ??? :
        TLTAXA=-TLTAXA
        TLTAXB=-TLTAXB
C
      ENDIF
C
      write(*,'(/,'' HAND   = '',F12.3)')HAND
C
      write(*,'(/,'' TLTAXA = '',F12.3)')TLTAXA
      write(*,'('' TLTAXB = '',F12.3)')TLTAXB
C
C
96    IF(TLTAXA.GE.90.0) THEN
        TLTAXA=TLTAXA-180.0             ! PUTS TLTAXA BETWEEN +/-90.
        TLTAXB=TLTAXB-180.0
        GO TO 96
      ENDIF
97    IF(TLTAXA.LE.-90.0) THEN
        TLTAXA=TLTAXA+180.0             ! PUTS TLTAXA BETWEEN +/-90.
        TLTAXB=TLTAXB+180.0
        GO TO 97
      ENDIF
C
      write(*,'(/,'' TLTAXA = '',F12.3)')TLTAXA
      write(*,'('' TLTAXB = '',F12.3)')TLTAXB
C
C Now get sign of crystallographic tiltangle.
C  by first calculating whether A_IS_ABOVE the original TLTAXIS on the film.
C
      TLTNORM = TLTAXIS + 90.0      ! TLTNORM now GE.0 and LT.180(see above)
      ANGACOMP= ABS(ANGA-TLTNORM)   ! Should be between 0 and 360
      IF((ANGACOMP.GT.90.0).AND.(ANGACOMP.LT.270.0)) THEN
        AISABOVE = -1.0
      ELSE
        AISABOVE =  1.0
      ENDIF
      SIGNTLTAXA = SIGN(1.0,TLTAXA)
C
      write(*,'(/,'' TLTAXIS   = '',F12.3)')TLTAXIS
      write(*,'('' ANGA      = '',F12.3)')ANGA    
      write(*,'('' TLTNORM   = '',F12.3)')TLTNORM
      write(*,'('' ANGACOMP  = '',F12.3)')ANGACOMP
C
      write(*,'(/,'' AISABOVE  = '',F12.3)')AISABOVE
      write(*,'('' SIGNTLTAXA= '',F12.3)')SIGNTLTAXA
C
      TLTANG  = (AISABOVE * SIGNTLTAXA * HAND) * TANGL
C
      open(1,FILE=cfile1,STATUS='NEW',ERR=900)
C
      DTLTAXIS=TLTAXIS-OTLTAXIS
      DTLTANG =TLTANG -OTLTANG
      DTLTAXA =TLTAXA -OTLTAXA 
      write(1,'('' TLTAXIS = '',F9.3,6X,''OLD='',F9.3,6X,''DIFF='',F9.3)')TLTAXIS,OTLTAXIS,DTLTAXIS
      write(1,'('' TLTANG  = '',F9.3,6X,''OLD='',F9.3,6X,''DIFF='',F9.3)')TLTANG,OTLTANG,DTLTANG
      write(1,'('' TLTAXA  = '',F9.3,6X,''OLD='',F9.3,6X,''DIFF='',F9.3)')TLTAXA,OTLTAXA,DTLTAXA
      write(1,'('' TAXA    = '',F9.3)')TAXA
      write(1,'('' TANGL   = '',F9.3)')TANGL
C
      write(*,'(/,'' TLTAXIS = '',F9.3,6X,''OLD='',F9.3,6X,''DIFF='',F9.3)')TLTAXIS,OTLTAXIS,DTLTAXIS
      write(*,'('' TLTANG  = '',F9.3,6X,''OLD='',F9.3,6X,''DIFF='',F9.3)')TLTANG,OTLTANG,DTLTANG
      write(*,'('' TLTAXA  = '',F9.3,6X,''OLD='',F9.3,6X,''DIFF='',F9.3)')TLTAXA,OTLTAXA,DTLTAXA
      write(*,'('' TAXA    = '',F9.3)')TAXA
      write(*,'('' TANGL   = '',F9.3)')TANGL
C
      close(1)
C
      goto 999
C
 900  continue
        write(*,'('' ERROR on file open'')')
        stop 'ERROR occured'
        goto 999
C
 999  continue
C
      stop
      end

