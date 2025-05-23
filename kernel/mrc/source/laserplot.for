      PROGRAM     LASERPLOT
C  To enable output of plot82/84 files on postscript laserprinters


      IMPLICIT NONE

      INTEGER*4   NOPTIONS
        PARAMETER (NOPTIONS=12)
      REAL        CMYKLIST(4,7)/
     1            0,0,0,1,          !BLACK
     1            0,1,1,0,          !RED
     1            1,0,1,0,          !GREEN
     1            1,1,0,0,          !BLUE
     1            0,0,1,0,          !YELLOW
     1            0,.35,1,0,        !ORANGE
     1            0,1,0,0/          !PURPLE
      LOGICAL           EXISTS
      REAL        F
      INTEGER*4   FILEIN                  !unit for file input
      INTEGER*4   FILEOUT                 !unit for file output
      INTEGER*4   FRAMES                  !running frame count
      INTEGER*4   I
      CHARACTER   INPUTFILE*128
      INTEGER*4   IDUMMY
      INTEGER*4   IER
      INTEGER*4   IOSTAT
      INTEGER*4   IXRANGE
      INTEGER*4   IYRANGE
      INTEGER*4   KBDIN             !unit for kbd input
      INTEGER*4   KBDOUT                  !unit for kbd output
      INTEGER*4   KX
      INTEGER*4   KY
      LOGICAL           LANDSCAPE/.FALSE./
      REAL        LINEWIDTH/0.1/          !linewidth in mm
      INTEGER*4   LNBLNK
      INTEGER*4   MMPIC
      INTEGER*4   NFRAMES                 !number of frames to plot
      INTEGER*4   NP1
      INTEGER*4   NP2
      CHARACTER   OPTLIST(NOPTIONS)*32/
     1            '*',
     1            'BOLD',
     1            'NFRAMES',
     1            'OUTPUTFILE',
     1            'LANDSCAPE',
     1            'LINEWIDTH',
     1            'SCALE',
     1            'STARTFRAME',
     1            'XORIGIN',
     1            'XWIDTH',
     1            'YHEIGHT',
     1            'YORIGIN'/
      LOGICAL           OPT_PRESENT
      CHARACTER   OUTPUTFILE*128/'plot.ps'/
      INTEGER*4   PATHLENGTH
      CHARACTER   P1*32
      CHARACTER   P2*32
      CHARACTER   PARAM*128
      INTEGER*4   PRVPIC
      LOGICAL           PLOT82
      REAL*4            USER_SCALE/0.0/         !user-supplied scale factor
      INTEGER*4   STARTFRAME/1/           !starting frame number
      REAL*4            XORIGINMM/10.0/         !X origin rel to left edge
      REAL*4            XWIDTHMM/190.0/         !X width
      REAL*4            YORIGINMM/10.0/         !Y origin rel to bottom edge
      REAL*4            YHEIGHTMM/270.0/  !Y height


C  definitions for plot data
C       This routine needs to be linked with subroutines contained
C       PUBLIC_DISK:[PUBLIC.LIB]MODLIB.OLB
C
C       .PLT files have IX,IY +ve I*2 between 1 & 32767
C       +IX, +IY draws line to IX,IY
C       +IX, -IY moves pen to (IX,IY)
C        -1, +IY end
C        -2,   0 dot: next record has IX,IY
C        -3, +IY line thickness change to IY
C        -4, +IY colour changes to IY (IY=0,7)
C        -5, +IY IY rows blank paper @ 100 rows/inch
C        -6, +IY erase VT640 only, IY=1 for on 0 for off
C
C
      INTEGER*4   DRAW
        PARAMETER (DRAW       = 2)
      INTEGER*4   MOVE
        PARAMETER (MOVE       = 1)
      INTEGER*4   NB4
        PARAMETER (NB4        = 4)
      INTEGER*4   PAPERHEIGHTMM
        PARAMETER (PAPERHEIGHTMM    = 297)
      INTEGER*4   PAPERWIDTHMM
        PARAMETER (PAPERWIDTHMM     = 211)
      INTEGER*4   POINT
        PARAMETER (POINT            = 3)

        REAL*4          AREC(128)
      REAL*4            BREC(2)
      REAL        DOTMX,DOTMY
        INTEGER*2 IXY(2),IX,IY
      INTEGER*2   PLOTEND
      LOGICAL           PLTSTARTED
      REAL        SCALE
      REAL        SCALEFAC
      REAL        SCALEX
      REAL        SCALEY
CCC        CHARACTER*80 TITLEH

C
C

C     EQUIVALENCE (AREC(1),NREC)                !0    for PLOT82
      INTEGER*4   PLOT82NREC
      EQUIVALENCE (AREC(2),DOTMX,PLOT82NREC)    !NBPI     "
      EQUIVALENCE (AREC(3),DOTMY)               !IXMIN          "
      INTEGER*4   IXMIN
      EQUIVALENCE (AREC(4),IXMIN)               !IXMAX          "
      INTEGER*4   IXMAX
      EQUIVALENCE (AREC(5),IXMAX)               !IYMIN          "
      INTEGER*4   IYMIN
      EQUIVALENCE (AREC(6),IYMIN)               !IYMAX          "
      INTEGER*4   IYMAX
      EQUIVALENCE (AREC(7),IYMAX)               !LINWT          "
C     EQUIVALENCE (AREC(8),LINWT)               !ICOLOR         "
C     EQUIVALENCE (AREC(9),ICOLOR)        !MIXCOL         "
C     EQUIVALENCE (AREC(10),MOXCOL)
C     EQUIVALENCE (AREC(11),MDEVIC)
C     EQUIVALENCE (AREC(12),MDIREC)
C     EQUIVALENCE (AREC(13),MOUT)
      INTEGER*4   MPIC
      EQUIVALENCE (AREC(14),MPIC)
C     EQUIVALENCE (AREC(15),MSCAFL)
C     EQUIVALENCE (AREC(16),MCNTFL)
C     EQUIVALENCE (AREC(17),DWLIMX)
C     EQUIVALENCE (AREC(18),DWLIMY)
C     EQUIVALENCE (AREC(19),DVXMIN)
C     EQUIVALENCE (AREC(20),DVXMAX)
C     EQUIVALENCE (AREC(21),DVYMIN)
C     EQUIVALENCE (AREC(22),DVYMAX)
C     EQUIVALENCE (AREC(21),NPICS)
        CHARACTER*8     PASWRD
      EQUIVALENCE (AREC(24),PASWRD)

C     EQUIVALENCE (AREC(41),TITLEH)
      EQUIVALENCE (BREC,IXY(1))
      EQUIVALENCE (IXY(1),IX)
      EQUIVALENCE (IXY(2),IY)


C  use private units for kbd I/O so we can keep stdin and stdout
C  as units 5/6
      KBDIN=5
      KBDOUT=6
      FILEIN=1
      FILEOUT=2
      OPEN(UNIT=KBDIN)
      OPEN(UNIT=KBDOUT)
      PATHLENGTH=0

C  -inputfile
      IF (OPT_PRESENT(OPTLIST,NOPTIONS,'*',INPUTFILE)) THEN
        INQUIRE(FILE=INPUTFILE,EXIST=EXISTS)
        IF (.NOT.EXISTS) THEN
          WRITE(KBDOUT,*) 'Cant open input file '//PARAM(1:LNBLNK(PARAM))
          CALL EXIT(1)
        END IF
      ELSE
        WRITE(KBDOUT,*) 'No input file specified'
        CALL EXIT(1)
      END IF

C  -outputfile
      IF (OPT_PRESENT(OPTLIST,NOPTIONS,'OUTPUTFILE',PARAM))
     1 OUTPUTFILE=PARAM
      OPEN(UNIT=FILEOUT,FILE=OUTPUTFILE,STATUS='UNKNOWN',IOSTAT=IOSTAT)
      IF (IOSTAT.NE.0) THEN
        WRITE(KBDOUT,FMT='(A)')
     1 'Cant open output file '//OUTPUTFILE(1:LNBLNK(PARAM))
        CALL EXIT(1)
      END IF

      STARTFRAME=0                  !start at 1st frame by default
      NFRAMES=-1              !plot all frames by default

C  -xorigin
      IF (OPT_PRESENT(OPTLIST,NOPTIONS,'XORIGIN',PARAM)) THEN
        READ(PARAM,FMT=*,IOSTAT=IOSTAT) XORIGINMM
        IF (IOSTAT.NE.0) THEN
          WRITE(KBDOUT,*) 'Illegal -xorigin'
          CALL EXIT(1)
        END IF
      END IF
C  -yorigin
      IF (OPT_PRESENT(OPTLIST,NOPTIONS,'YORIGIN',PARAM)) THEN
        READ(PARAM,FMT=*,IOSTAT=IOSTAT) YORIGINMM
        IF (IOSTAT.NE.0) THEN
          WRITE(KBDOUT,*) 'Illegal -yorigin'
          CALL EXIT(1)
        END IF
      END IF
C  -xwidth
      IF (OPT_PRESENT(OPTLIST,NOPTIONS,'XWIDTH',PARAM)) THEN
        READ(PARAM,FMT=*,IOSTAT=IOSTAT) XWIDTHMM
        IF (IOSTAT.NE.0) THEN
          WRITE(KBDOUT,*) 'Illegal -xwidth'
          CALL EXIT(1)
        END IF
      END IF
C  -yheight
      IF (OPT_PRESENT(OPTLIST,NOPTIONS,'YHEIGHT',PARAM)) THEN
        READ(PARAM,FMT=*,IOSTAT=IOSTAT) YHEIGHTMM
        IF (IOSTAT.NE.0) THEN
          WRITE(KBDOUT,*) 'Illegal -yheight'
          CALL EXIT(1)
        END IF
      END IF
C  -linewidth
      IF (OPT_PRESENT(OPTLIST,NOPTIONS,'LINEWIDTH',PARAM)) THEN
        READ(PARAM,FMT=*,IOSTAT=IOSTAT) LINEWIDTH
        IF (IOSTAT.NE.0.OR.LINEWIDTH.LT.0) THEN
          WRITE(KBDOUT,*) 'Illegal -linewidth'
          CALL EXIT(1)
        END IF
      END IF

C  -landscape
      LANDSCAPE=(OPT_PRESENT(OPTLIST,NOPTIONS,'LANDSCAPE',PARAM))
      IF (LANDSCAPE) THEN
C  interchange width/height
        F=XWIDTHMM
        XWIDTHMM=YHEIGHTMM
        YHEIGHTMM=F
      END IF

C  -startframe
      IF (OPT_PRESENT(OPTLIST,NOPTIONS,'STARTFRAME',PARAM)) THEN
        READ(PARAM,FMT=*,IOSTAT=IOSTAT) STARTFRAME
        IF (IOSTAT.NE.0.OR.STARTFRAME.LE.0) THEN
          WRITE(KBDOUT,*) 'Illegal -startframe'
          CALL EXIT(1)
        END IF
      END IF

C  -nframes
      IF (OPT_PRESENT(OPTLIST,NOPTIONS,'NFRAMES',PARAM)) THEN
        READ(PARAM,FMT=*,IOSTAT=IOSTAT) NFRAMES
        IF (IOSTAT.NE.0.OR.NFRAMES.LE.0) THEN
          WRITE(KBDOUT,*) 'Illegal -nframes'
          CALL EXIT(1)
        END IF
      END IF

C  -scale
      IF (OPT_PRESENT(OPTLIST,NOPTIONS,'SCALE',PARAM)) THEN
        READ(PARAM,FMT=*,IOSTAT=IOSTAT) USER_SCALE
        IF (IOSTAT.NE.0.OR.USER_SCALE.LE.0) THEN
          WRITE(KBDOUT,*) 'Illegal -scale'
          CALL EXIT(1)
        END IF
      END IF

C  here we go....
C  open input file
C 
        CALL QOPEN(FILEIN,INPUTFILE,'RO')
      CALL QMODE(FILEIN,0,IDUMMY)
C
C  read 1st header. try a 512-byte read first. if this fails,
C  it may be a short PLOT82 file.
      PLOT82=.TRUE.
      CALL QREAD(FILEIN,AREC,512,IER)
      IF (IER.NE.0) GOTO 10         !it cant be a PLOT84
      PLOT82=(INDEX(PASWRD,'PLOT%%84').NE.1)
10    PLOTEND=-1
      IF (PLOT82) THEN
          WRITE(*,'('' HERE IS A BUG IN LASERPLOT.FOR for PLOTEND'')')
          STOP
C       PLOTEND='100004'O
        CALL QSEEK(FILEIN,1,1,80)
        MMPIC=0
      ELSE
          CALL QSEEK(FILEIN,1,1,512)
      END IF

      IF (PLOT82) THEN
        CALL QREAD(FILEIN,AREC(2),80,IER)
        MMPIC=MMPIC+1
      ELSE
        CALL QREAD(FILEIN,AREC,512,IER)
        MMPIC=MPIC
      END IF
      IF (IER.NE.0) GOTO 998
      PRVPIC=-1

      PLTSTARTED=.FALSE.
C  Skip to start of required 1st frame
      DO WHILE (MMPIC.LT.STARTFRAME)
        WRITE(KBDOUT,*) 'Skip frame ',MMPIC
        IX=0
        DO WHILE (IX.NE.PLOTEND)
          CALL QREAD(FILEIN,BREC,NB4,IER)
            IF(IER.NE.0) GOTO 998
        END DO
        IF (PLOT82) THEN
          CALL QREAD(FILEIN,AREC(2),80,IER)
          IF (PLOT82NREC.LE.0) GOTO 998   !stop at logical EOF
          MMPIC=MMPIC+1
        ELSE
          CALL QREAD(FILEIN,AREC,512,IER)
          MMPIC=MPIC
          IF (MMPIC.EQ.PRVPIC) GOTO 998   !stop at logical EOF
        END IF
        IF (IER.NE.0) GOTO 998
      END DO
      FRAMES=1
      WRITE(FILEOUT,'(A)') '%!PS-Adobe'
      WRITE(FILEOUT,'(A,/,A)')
     1 '/M {moveto} def',
     1 '/L {lineto} def'
      GOTO 21     

20    IF (FRAMES.GT.NFRAMES.AND.NFRAMES.GT.0) GOTO 998
      IF (PLOT82) THEN
        CALL QREAD(FILEIN,AREC(2),80,IER)
        IF (PLOT82NREC.LE.0) GOTO 998           !stop at logical EOF
        MMPIC=MMPIC+1
      ELSE
        CALL QREAD(FILEIN,AREC,512,IER)
        MMPIC=MPIC
        IF (MMPIC.EQ.PRVPIC) GOTO 998           !stop at logical EOF
      END IF
21    IXRANGE = IXMAX - IXMIN
      IYRANGE = IYMAX - IYMIN
C     WRITE(KBDOUT,FMT='(A,I6,A,I6,A)')
C     1 ' Width=',IXRANGE,', height=',IYRANGE,' (plot units)'

C Set scale if USER_SCALE is given
      IF (USER_SCALE.GT.0.) THEN
        IF (PLOT82) THEN
          DOTMX=1.0
          DOTMY=1.0
        END IF
        XWIDTHMM=USER_SCALE*IXRANGE/DOTMX
        YHEIGHTMM=USER_SCALE*IYRANGE/DOTMY
c       XWIDTHMM=USER_SCALE*IXRANGE
c       YHEIGHTMM=USER_SCALE*IYRANGE
      ENDIF

      SCALEX=XWIDTHMM/(IXMAX-IXMIN)
      SCALEY=YHEIGHTMM/(IYMAX-IYMIN)
      SCALE=AMIN1(SCALEX,SCALEY)          !scale factor in mm per plot unit

C  start reading input .PLT file
1000  CONTINUE
      IXY(1)=123
      IXY(2)=456
      CALL QREAD(FILEIN,BREC,NB4,IER)
        IF(IER.NE.0) GOTO 1998
      IF (.NOT.PLTSTARTED) THEN
C  reset all graphics states
        WRITE(FILEOUT,*) 'initgraphics'
C  initialise origin, scale and orientation
        IF (LANDSCAPE) THEN
          WRITE(FILEOUT,*) '90 rotate'
          WRITE(FILEOUT,*) '0 -594 translate'
        END IF
        WRITE(FILEOUT,*) (72.0/25.4),(72.0/25.4),' scale'
        WRITE(FILEOUT,'(E9.4,A,E9.4,A)') XORIGINMM,' ',YORIGINMM,' translate'
        WRITE(FILEOUT,*) '0 setlinejoin'
        WRITE(FILEOUT,'(E9.4,A)') LINEWIDTH,' setlinewidth'
        PLTSTARTED=.TRUE.
      END IF
      IF (IX.LT.0) THEN
        IF (PLOT82) THEN
          IF (IX.EQ.100004) IX=-1   !IEND plot82
          IF (IX.EQ.100010) IX=-5   !IPAP plot82
          IF (IX.EQ.100001) IX=-4   !IPEN plot82
          IF (IX.EQ.100002) IX=-3   !ILWT plot82
        END IF
        GOTO(1300,            ! erase (VT640 only)
     1      1400,       ! rows of blank
     1      1500,       ! colour
     1      1600,       ! line thickness
     1      1700,       ! dot
     1      1800) IX+7  ! end picture
      END IF
1100  KX=IX
      KY=IY
      IF (IY.LT.0) KY=-IY
C  put picture at user s origin
      KX=KX-IXMIN
      KY=KY-IYMIN

      IF (IY.LT.0) THEN
        IF (PATHLENGTH.GT.1000) THEN
C  very long paths seem to screw up at least laser1
          WRITE(FILEOUT,*) 'stroke'
          PATHLENGTH=0
        END IF
        WRITE(FILEOUT,FMT='(E9.4,A,E9.4,A)') SCALE*KX,' ',SCALE*KY,' M'
      ELSE
        WRITE(FILEOUT,FMT='(E9.4,A,E9.4,A)') SCALE*KX,' ',SCALE*KY,' L'
        PATHLENGTH=PATHLENGTH+1
      END IF
      GOTO 1000

1300  GOTO 1000
1400  GOTO 1000

1500  CONTINUE
C  select drawing colour.
C  colours are value if IY and are:
C     value name              cmyk
C     ----- ----              ----
C   --   1 BLACK              0,0,0,1
C   --   2 RED                      0,1,1,0
C   --   3 GREEN              1,0,1,0
C   --   4 BLUE                     1,1,0,0
C   --   5 YELLOW             0,0,1,0
C   --   6 ORANGE             0,.35,1,0
C   --   7 PURPLE             .37,.875,.06,0
      WRITE(FILEOUT,FMT='(A/,4(F5.3,X),A)')
     1 'stroke',(CMYKLIST(I,IY),I=1,4), 'setcmykcolor'
      GOTO 1000
1600  GOTO 1000

C       dot
1700  CALL QREADI(FILEIN,IXY,NB4,IER)
      IF(IER.NE.0) GOTO 1998
      WRITE(FILEOUT,FMT='(E9.4,A,E9.4,A)') SCALE*KX,' ',SCALE*KY,' M'
      WRITE(FILEOUT,FMT='(E9.4,A,E9.4,A)') SCALE*KX,' ',SCALE*KY,' D'
      GOTO 1100

C     end frame
1800  CONTINUE
C     WRITE(KBDOUT,*) 'Frame',MMPIC,' done.'
      FRAMES=FRAMES+1
      PRVPIC=MMPIC
      WRITE(FILEOUT,*) 'stroke'
      WRITE(FILEOUT,*) 'showpage'
      PLTSTARTED=.FALSE.
      GOTO 20
1998  IF (PLTSTARTED)  THEN
        WRITE(FILEOUT,*) 'stroke'
        WRITE(FILEOUT,*) 'showpage'
      END IF

998   CALL QCLOSE(FILEIN)
      IF (PLTSTARTED) CLOSE(UNIT=FILEOUT)
      END

C***************************************************************************
      LOGICAL FUNCTION OPT_PRESENT(OPLIST,OPLISTLEN,OPT,PARAM)
C  options are introduced with a - sign
C  enough of the option name has to be typed to uniquely
C  identify it. If a parameter is required, it must follow
C  the option with a separating = sign
C  If spaces or tabs are part of a parameter, either the spaces/tabs
C  must be indvidually escaped with a \ char, or the whole parameter
C  must be enclosed in ".
C  If " is part of a parameter, it must be escaped with \

      IMPLICIT NONE
      INTEGER*4   ARGLEN
      INTEGER*4   ARGNUMBER
      CHARACTER   ARGUMENT*132
      CHARACTER   ARGOP*132
      INTEGER*4   ARGOPLEN
      INTEGER           I
      INTEGER*4   IOP
      INTEGER*4   JUSTIFY
      INTEGER*4   KOP
      INTEGER*4   NMATCHES
      CHARACTER   PARAM*(*)
      INTEGER*4   OPLISTLEN
      CHARACTER   OPT*(*)
      CHARACTER   OPLIST(OPLISTLEN)*(*)
      INTEGER*4   OPLEN

      OPT_PRESENT=.FALSE.
      PARAM=' '
C  make sure OPT is present in OPLIST. Fatal error if not.
      OPLEN=LEN(OPT)
      DO I=1,OPLISTLEN
        IF (OPT(1:OPLEN).EQ.OPLIST(I)(1:JUSTIFY(OPLIST(I)))) GOTO 10
      END DO
      print *,'SUBROUTINE::OPT_PRESENT: OPT not in OPT list ',OPT
      STOP
10    ARGNUMBER=0
11    ARGNUMBER=ARGNUMBER+1
      CALL GETARG(ARGNUMBER,ARGUMENT)
      IF (ARGUMENT.EQ.' ') RETURN
      ARGLEN=JUSTIFY(ARGUMENT)
C  is it a OPT?
      IF (ARGUMENT(1:1).EQ.'-') THEN
        ARGOP=ARGUMENT(2:ARGLEN)
        ARGOPLEN=ARGLEN-1
        I=INDEX(ARGUMENT(1:ARGLEN),'=')
        IF (I.NE.0) THEN
C** FIX UP FOR LONE = 
C  there is a parameter.
          PARAM=ARGUMENT(I+1:ARGLEN)
          ARGOP=ARGUMENT(2:I-1)
          ARGOPLEN=I-2
        END IF
      ELSE
        PARAM=ARGUMENT(1:ARGLEN)
        IF (OPT(1:OPLEN).EQ.'*') THEN
          OPT_PRESENT=.TRUE.
        END IF
        RETURN
      END IF
C  force case-blind
      DO I=1,ARGOPLEN
        IF (ARGOP(I:I).GE.'a'.AND.ARGOP(I:I).LE.'z')
     1 ARGOP(I:I)=CHAR(ICHAR(ARGOP(I:I))-
     *    (ICHAR('a')-ICHAR('A')))
C*** 1 ARGOP(I:I)=CHAR(ICHAR(ARGOP(I:I)).AND.'137'O)
      END DO
C  search OPT list for unambiguous entry.
      NMATCHES=0
      KOP=0
      DO IOP=1,OPLISTLEN
        IF (ARGOPLEN.LE.JUSTIFY(OPLIST(IOP))) THEN
C  this is a candidate
          IF (ARGOP(1:ARGOPLEN).EQ.OPLIST(IOP)(1:ARGOPLEN)) THEN  !got a match
            NMATCHES=NMATCHES+1
            KOP=IOP
          END IF
        END IF
      END DO
      IF (NMATCHES.EQ.0) THEN
        print *,'Illegal option ',ARGOP(1:ARGOPLEN)
        STOP
      END IF
C  was it unique?
      IF (NMATCHES.NE.1) THEN
C  no
        print *,'Ambiguous option ',ARGOP(1:ARGOPLEN)
        STOP
      END IF
C  yes.
C  does it match OPT
      IF (ARGOPLEN.GT.OPLEN) GOTO 11
      IF (ARGOP(1:ARGOPLEN).NE.OPT(1:ARGOPLEN)) GOTO 11
      OPT_PRESENT=.TRUE.
      RETURN
      END

C***************************************************************************
      INTEGER FUNCTION JUSTIFY(STRING)
      CHARACTER   STRING*(*)
      DO I=LEN(STRING),1,-1
        IF (STRING(I:I).NE.' ') GOTO 10
      END DO
      I=0
10    JUSTIFY=I
      RETURN
      END

C***************************************************************************
      SUBROUTINE UPPERCASE(SOURCE,DEST)
      CHARACTER   SOURCE*(*)
      CHARACTER   DEST*(*)
      DO I=1,MIN(LEN(SOURCE),LEN(DEST))
        DEST(I:I)=SOURCE(I:I)
        IF (SOURCE(I:I).GE.'a'.AND.SOURCE(I:I).LE.'z')
     1        DEST(I:I)=CHAR(INT(AND(ICHAR(SOURCE(I:I)),137)))
      END DO
      RETURN
      END

