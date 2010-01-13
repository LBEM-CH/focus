C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
C          CCP4 PARSER Routines
C          ====================
C
C  Original Author: Based on Mike Levitt's routine of the same name.
C  Modified By: Peter Brick, Phil Evans, Eleanor Dodson, Dave Love
C
C     Library parser.f contains the following subroutines and functions,
C     some of which are currently unsued and commented out.
C
C  SUBROUTINES 
C
C    PARSER(KEY,LINE,IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,NTOK,LEND,PRINT)
C    PARSE(LINE,IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,N)
C    PARSDL(NEWDLM,NNEWDL,NSPECD)
C    KEYNUM(N,NSTART,LINE,IBEG,IEND,ITYP,NTOK)
C  [ KEYERR(I,MODE,LINE,IBEG,IEND,ITYP) ] - internal subroutine (KEYNUM)
C  [ CHKNUM(ISYSW,N1,N2,NTOK,ITYP,IBEG,IEND,LINE) ] - not used (Sep 1993)
C  [ CHKTOK(ISYSW,I,IWANT,NTOK,ITYP,IBEG,IEND,LINE) ] - internal (CHKNUM)
C  [ GETREA(N,X,NTOK,ITYP,FVALUE) ] - not used (Sep 1993)
C  [ GETINT(N,I,NTOK,ITYP,FVALUE) ] - not used (Sep 1993)
C    GTNREA(N,M,X,NTOK,ITYP,FVALUE)
C    GTNINT(N,M,J,NTOK,ITYP,FVALUE)
C    GTPREA(N,X,NTOK,ITYP,FVALUE)
C    GTPINT(N,I,NTOK,ITYP,FVALUE)
C  [ GETSTR(N,STRING,NTOK,ITYP,IBEG,IEND,LINE) ] - not used (Sep 1993)
C    SBLANK(ARRAY,N1,N2)
C  [ GTCFLD(NFIELD,ITEXT,NCHAR,MINC,MAXC,IGFLAG) ] - not used (Sep 1993)
C  [ CPYCHR(STRINGA,STRINGB,NCHAR) ] - not used (Sep 1993)
C  [ CMOVE(STRINGA,STRINGB,NCHAR) ] - not used (Sep 1993)
C    CHKKEY(KEY,WORDS,NWORDS,IKEY)
C    PUTLIN(STROUT,OUTWIN)
C    BLANK(OUTWIN,NLINES)
C    LERROR(ERRFLG,IFAIL,ERRMSG)
C    RDSYMM(JTOK,LINE,IBEG,IEND,ITYP,FVALUE,NTOK,SPGNAM,NUMSGP,PGNAME,
C           NSYM,NSYMP,RSYM)
C    RDHEAD(JTOK,LINE,IBEG,IEND,ITYP,FVALUE,NTOK,MTZPRT,MTZBPR)
C    RDCELL(ITOK,ITYPE,FVALUE,NTOK,CELL)
C    RDRESO(ITOK,ITYPE,FVALUE,NTOK,RESMIN,RESMAX,SMIN,SMAX)
C    RDSCAL(ITOK,LINE,IBEG,IEND,ITYP,FVALUE,NTOK,NLPRGI,LSPRGI,ILPRGI,SCAL,BB)
C    RDRESL(ITOK,ITYPE,FVALUE,CVALUE,NTOK,RESMIN,RESMAX,SMIN,SMAX,ISTAT)
C    GTTREA(N,X,LFLAG,NTOK,ITYP,FVALUE)
C    GTTINT(N,I,LFLAG,NTOK,ITYP,FVALUE)
C
C FUNCTIONS
C
C    LOGICAL FUNCTION CMATCH(STRING1,STRING2,NCHAR)
C
C_BEGIN_INTRO
C          CCP4 PARSER Routines
C          ====================
C
C The PARSER module of the CCP4 library contains routines which are
C mainly used for `free-format' `keyworded' input of control data for
C programs.  Most programs have a loop over input records which are
C initially fed to the routine PARSER to tokenise them and extract the
C initial keyword.  PARSER can cope with continued, commented input
C lines and included files.  It calls PARSE to tokenise individual
C records and PARSE is sometimes useful itself to compensate for the
C lack of free-format internal READs in the fortran77 standard.  See
C the entries below for details.
C
C The library also contains routines to decode the parameters
C following the `standard' program keywords SYMMETRY, RESOLUTION,
C SCALE and CELL and to extract real and integer numbers from fields.
C 
C_END_INTRO
C
C_BEGIN_PARSER
C     =================================================================
      SUBROUTINE PARSER(KEY,LINE,IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,NTOK,
     +                  LEND,PRINT)
C     =================================================================
C
C     The normal behaviour is to read `keyworded' data from the input
C     stream, and interpret it.  This is the case if LINE is initially
C     blank.  Stream 5 is the standard input stream, but a line
C     beginning with @<name> starts reading from a file <name> (on
C     stream 11), until end-of-file.
C
C     Each logical 'card' may be continued on next line by the
C     continuation characters `&', `-' or `\'  at the end of the line: this
C     character is dropped from the list returned to the calling routine.
C
C     Trailing comments may be present, following the
C     character '#' or '!': any continuation character (`&', `-'  or `\')
C     must PRECEED the comment character -- comments can't be continued.
C     The complete (continued) line, less any comments, is returned in
C     LINE.  Lines containing ONLY comments (or blank) will not be
C     returned from this routine -- reading will continue.
C
C     Strings may be quoted or unquoted.  See also PARSE for details of
C     token delimiters etc.
C
C     Alternatively, if LINE is non-blank it will be interpreted before
C     possibly reading further data on the standard input if LINE ends
C     with a continuation character.
C
C---- Arguments :
C
C   KEY    (O)  CHARACTER*4    Keyword at beginning of line (if present),
C                              uppercased before returning.
C
C   LINE   (I/O) CHARACTER*(*) Parse this input string.  If blank read
C                              lines from unit 5.  LINE will be updated to
C                              contain the entire line read, including
C                              continuations.
C
C   IBEG   (O)  INTEGER(*)     Array of size at least NTOK.
C                              1st column number of tokens in field 
C
C   IEND   (O)  INTEGER(*)     Array of size at least NTOK.
C                              Last column number of tokens in field
C
C   ITYP   (O)  INTEGER(*)     Array of size at least NTOK. 
C                              =0  null field
C                              =1  character string
C                              =2  number
C
C   FVALUE (O)  REAL(*)        Array of size at least NTOK.
C                              Value of number.
C
C   CVALUE (O)  CHARACTER(*)*4 Array of size at least NTOK. 
C                              Character string (1st 4 characters),
C                              for numbers as well as strings.
C
C      Items in FVALUE and CVALUE are left unchanged for null fields
C
C   IDEC   (O)  INTEGER(*)     Array of size at least NTOK.
C                              Number of 'digits':
C                              for string, number of characters (=4 if.gt.4)
C                              for integer, number of digits
C                              for real number,
C                              (number of digits before point+1)*100
C                               +number of digits after point
C
C   NTOK   (I/O) INTEGER       On input sets the maximum number of fields
C                              to be parsed (if <20 then defaults to 20)
C                              On output returns the number of fields parsed.
C
C   LEND    (O)  LOGICAL       .FALSE. for control card
C                              .TRUE.  for end-of-file
C
C   PRINT   (I)  LOGICAL       .TRUE. echo line to unit 6 via PUTLIN
C                              .FALSE. don't echo
C                  
C_END_PARSER
C
C     .. Scalar Arguments ..
      INTEGER NTOK
      LOGICAL LEND,PRINT
      CHARACTER KEY*4,LINE*(*)
C     ..
C     .. Array Arguments ..
      REAL FVALUE(*)
      INTEGER IBEG(*),IDEC(*),IEND(*),ITYP(*)
      CHARACTER CVALUE(*)*4
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,K,KSTREAM,LENLIN,LINLEN,LSTREAM,MSTREAM,N,
     +        NITEM
      LOGICAL FIRST, HAVLIN
      CHARACTER FLNAME*60,LINEX*1500,LINEK1,SLASH
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPDPN,CCPUPC,PARSE,LERROR,PUTLIN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC LEN,MAX,MIN
C     ..
C     .. Scalars in Common ..
      CHARACTER STROUT*1500
C     ..
C     .. Save statement ..
      SAVE NITEM, LSTREAM
C     ..
C     .. Data statements ..
      DATA LSTREAM/5/,KSTREAM/11/,MSTREAM/5/
      DATA NITEM/0/
C     ..
C
C     NB: a compiler may or may not regard '\\' as two characters
      SLASH='\\'
      LINEX = ' '
      NINCHR = 0
      K = 1
C     We need a flag to avoid the echoing of data when parser is called
C     during processing of an `@'-included file, as in RDSYMM typically
C     called from agrovata
      HAVLIN = LINE .NE. ' '
C
   10 FIRST = .TRUE.
C
C---- Find length of line
C
      LINLEN = LEN(LINE)
C
C---- Find out dimensions of ibeg etc                        
C
      LEND = .FALSE.
C
C---- Initialisations
C     - on first call check if NTOK has been set = NITEM.
C       Default NITEM = 20 - you would only want to increase it!
C
      IF (NITEM.EQ.0) NITEM = MAX(ABS(NTOK),20)
      NTOK = 0
C
C---- Skip read if LINE already has something in it
C                                            
      IF (LINE.NE.' ') GO TO 30
C
   20 CONTINUE
      LINEX = ' '
      READ (LSTREAM,FMT='(A)',END=40) LINEX
      GO TO 45
C     End-of-file; if processing an included file switch to main input,
C     else return
   40 IF (LSTREAM.NE.MSTREAM) THEN
        CLOSE (UNIT=LSTREAM)
        LSTREAM = MSTREAM
        LINEX = ' '
        READ (LSTREAM,FMT='(A)',END=40) LINEX
      ELSE
        LEND = .TRUE.
        RETURN
      END IF
 45   CONTINUE
      HAVLIN = .FALSE.
C
      LX = LENSTR(LINEX)
C---- Count total number of characters on line
      NINCHR = NINCHR + LX
      IF (NINCHR .GT. LINLEN) THEN
C-----  Line overflow
        WRITE (6, FMT='(A,I5,A/1X,A)') ' *** WARNING - More than ',
     .       LINLEN,' characters in (continued) line ***',
     .       ' *** Parsing truncated line beginning with:'
        WRITE(6,FMT='(A)') LINEX(1:MIN(130,LX))
        NINCHR = LINLEN
      ENDIF
      IF (FIRST) THEN
C       Not a continuation line
        LINE = LINEX
        FIRST = .FALSE.
      ELSE
C       continuation -- append record just read to previous line
        LINE(K:) = LINEX
      END IF
   30 LENLIN = LENSTR(LINE)
      IF (LENLIN.EQ.0) GO TO 20
C---- Use Negative N to pass NITEM To PARSE for first call (effectively
C     always first call)
      N = -NITEM
C
C---- Interpret
C     (Don't hand more to PARSE than necessary since we already know the
C     length.)
C
C          ********************************************************
      CALL PARSE(LINE(:LENLIN),IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,N)
C          ********************************************************
C
C---- Count fields
C
      IF (N.GT.0) THEN
        NTOK = NTOK + N
C----   Copy keyword string to KEY if present
        IF (ITYP(1).EQ.1 ) THEN
          KEY = CVALUE(1)
          CALL CCPUPC(KEY)
        ELSE
C         avoid retaining previous KEY if one not present this time
          KEY = ' '
        END IF
      ELSE
C       comment line
        IF (.NOT. HAVLIN .AND. (PRINT .OR. LSTREAM.NE.MSTREAM)) THEN
          STROUT = ' '
          WRITE (STROUT,FMT=6002) 'Comment',
     .                       LINE(1:MIN(1490,LENSTR(LINE)))
 6002     FORMAT (A,' line--- ',A)
          CALL PUTLIN(STROUT,'HLPWIN')
          NINCHR = 0
        ENDIF
        FIRST = .TRUE.
        GO TO 20
      END IF 
C
C---- Test if first field begins '@' and include file if so
C
      IF (NTOK.GT.0 .AND. ITYP(1).EQ.1 .AND.
     +    CVALUE(1) (1:1).EQ.'@') THEN
C
C---- Get filename if present (just '@' resets stream to MSTREAM)
C
        IF (IDEC(1).EQ.1) THEN
          LSTREAM = MSTREAM
        ELSE
          FLNAME = LINE(IBEG(1)+1:IEND(1))
C
C---- Open file
C
          LSTREAM = KSTREAM
          IFAIL = 1
C
C              *********************************************
          CALL CCPDPN(LSTREAM,FLNAME,'READONLY','F',0,IFAIL)
C              *********************************************
C
          IF (IFAIL.GE.0) THEN
C
C---- and start to read it
C
            LINE = ' '
            GO TO 10
          ELSE
C
C---- Failed to open file
C
            LSTREAM = MSTREAM
            CALL LERROR(1,0,' Can''t open file ' //
     +           FLNAME(1:LENSTR(FLNAME)))
            NTOK = 0
          END IF
        END IF
C
        RETURN
      END IF
C
C     Check for contined line.  If there's a trailing continuation
C     character not followed by a quote (which would indicate it's part
C     of a string) back up one character and append the next line of
C     input to the current buffer (above).  The new buffer will be fed
C     afresh to PARSE.
C
      K = IEND(NTOK)
      IF (K+1 .LE. LEN(LINE)) THEN
        LINEK1 = LINE(K+1:K+1)
      ELSE
        LINEK1 = ' '
      END IF
      IF ((LINE(K:K).EQ.'&' .OR. LINE(K:K).EQ.'-' 
     +                              .OR. LINE(K:K).EQ.SLASH)
     +     .AND. LINEK1 .NE. '''' .AND. LINEK1.NE.'"') THEN
C       zap continuation character
        LINE(K:) = ' '
C       reset line length
        NINCHR = IEND(NTOK)-1
C       ready to start again
        NTOK = 0
        FIRST = .FALSE.
C       Read next line
        GO TO 20
      ELSE IF (.NOT. HAVLIN .AND. (PRINT .OR. LSTREAM.NE.MSTREAM)) THEN
C       not a continued line -- maybe echo it
        STROUT = ' '
        WRITE (STROUT,FMT=6002) 'Data', LINE(1:MIN(1490,LENSTR(LINE)))
        CALL PUTLIN(STROUT,'HLPWIN')
      END IF
      END
C
C_BEGIN_PARSE
C     ==========================================================
      SUBROUTINE PARSE(LINE,IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,N)
C     ==========================================================
C
C     Free format read routine.  This is really a scanner, not a parser.
C     It scans the LINE into N tokens which are separated by delimiters
C     and updates the information arrays for each, as below.  The
C     default delimiters are space, tab, comma and equals; they may be
C     changed using PARSDL.  Adjacent commas delimit `null' fields (the
C     same as empty strings).  Strings may be unquoted or single- or
C     double-quoted if they don't contain delimiters, but must be
C     surrounded by delimiters to be recognised.  This allows literal
C     quotes to be read, e.g. "ab"c" will be recognised as the token `ab"c'.
C     An unquoted `!' or `#' in LINE introduces a trailing comment,
C     which is ignored.
C
C---- Arguments:
C
C   LINE  (I)     CHARACTER*(*)  String to be parsed
C
C   N     (I/O)   INTEGER        Usually <0, when abs(N) is the maximum
C                                number of fields to interpret and should
C                                be <= the array dimensions.  If N>0 it
C                                is the number of tokens read so far,
C                                intended for continuation lines with PARSER.
C                                Returns number of fields scanned or 0 if
C                                line is blank or just contains a comment
C
C  For I=1,N :
C
C   IBEG(I)   (O) INTEGER(*)     1st column number in field
C
C   IEND(I)   (O) INTEGER(*)     last column number in field
C
C   ITYP(I)   (O) INTEGER(*)     =0  null field
C                                =1  character string
C                                =2  number
C
C   FVALUE(I) (O) REAL(*)        Value of number.  Use NINT(FVALUE(I)) to
C                                extract an integer.
C
C   CVALUE(I) (O) CHARACTER(*)*4 Character string (1st 4 characters)
C                                for numbers as well as strings
C
C     Items in FVALUE and CVALUE are left unchanged for null fields
C
C   IDEC(I)   (O) INTEGER(*)     Number of 'digits'
C                                for string, number of characters (=4 if.gt.4)
C                                for integer, number of digits
C                                for real number,
C                                (number of digits before point+1)*100
C                                +number of digits after point
C
C_END_PARSE
C     This routine is truly horrible and really ought to be re-written
C     in an understandable form with an outer loop over tokens rather
C     than characters...
C
C     ..
C     .. Scalar Arguments ..
      INTEGER N, NNEWDL, NSPECD
      CHARACTER LINE* (*)
C     ..
C     .. Array Arguments ..
      REAL FVALUE(*)
      INTEGER IBEG(*),IDEC(*),IEND(*),ITYP(*)
      CHARACTER CVALUE(*)*4, NEWDLM*(*)
C     ..
C     .. Local Scalars ..
      REAL F10,SIGN,SIGN0,VALUE,VALUE0
      INTEGER I,IDOT,J,L,LENG,LINLEN,NCHK,NDELM,NDIGS,NDONE,
     +        NITEM,NPLACE,NSPDLM,OPER,NDDELM,NDSDLM,INTLEN
      LOGICAL NULL,NUMBER,OPRATR,QUOTE,TOKEN,TQUOTE,COMMNT
      CHARACTER BLANK*1,LETQT*1,OLDQUT*1,DBLQT*1,TAB*1
      CHARACTER LINERR*1500,ICOMM1*1,ICOMM2*1
C     ..
C     .. Local Arrays ..
      INTEGER ISGN(2)
      INTEGER MAXDLM
      PARAMETER (MAXDLM=20)
      CHARACTER DELIM(MAXDLM)*1,DDELIM(MAXDLM)*1,DIGS(18)*1
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
      SAVE DELIM,NDELM,NSPDLM,DDELIM,NDDELM,NDSDLM
C     .. Data statements ..
      DATA LETQT,DBLQT/'''','"'/,BLANK/' '/,ICOMM1,ICOMM2/'#','!'/
      DATA DIGS/'0','1','2','3','4','5','6','7','8','9','+','-','*','/',
     +     'E','.','e',' '/
      DATA ISGN/1,-1/
      DATA NDIGS/17/
C Delimiters
C---- DELIM  array of NDELM delimiters
C---- DDELIM  default array of NDDELM delimiters
C---- NSPDLM (NDSDLM default) is number of special delimiters which
C            cannot delimit a null field these are
C            at the beginning of the delimiter array
C
C-- Note that delimiters may be changed by a call to PARSDL (entry point)
C
      DATA DDELIM/' ', ' ', '=',',',16*' '/
      DATA NDDELM/4/,NDSDLM/3/,NDELM/-1/
C     ..
C Setup delimiters if not done
      IF (NDELM .LT. 0) THEN
        NDELM=NDDELM
        NSPDLM=NDSDLM
        DO 1, I = 1, MAXDLM
          DELIM(I)=DDELIM(I)
 1      CONTINUE
C       Set tab (assumes ASCII)
        TAB = CHAR(9)
        DELIM(2) = TAB
      ENDIF
C
C---- First call  - N = - NITEM ; NDONE = 0
C
      IF (N.LT.0) THEN
        NITEM = -N
        NDONE = 0
C
C---- Continuation line: N = number already read
C
      ELSE IF (N.GE.0) THEN
        NDONE = N
        NITEM = N
      END IF
C
      N = 1
      TOKEN = .FALSE.
      VALUE = 0.0
      OPRATR = .TRUE.
      IDOT = 0
      INTLEN = 0
      SIGN = 1.0
      OPER = 0
      OLDQUT = BLANK
      QUOTE = .FALSE.
      TQUOTE = .FALSE.
      NUMBER = .FALSE.
      COMMNT = .FALSE.
C
      LINLEN = LENSTR (LINE)
      IF (LINLEN.LE.0) THEN
        N = 0
        RETURN
      END IF
C
C---- Main loop over character buffer.  The loop goes one past the end,
C     but we're careful not to index this character.
C
        DO 70 I = 1,LINLEN+1
C
C----     check for comment character (not in string)
C
          IF (I.LE.LINLEN) THEN
            IF (.NOT.QUOTE .AND.
     +           (LINE(I:I).EQ.ICOMM1 .OR. LINE(I:I).EQ.ICOMM2)) THEN
              COMMNT = .TRUE.
C     special case; comment line:
              IF (N.EQ.1) THEN
                N = 0
                RETURN
              END IF
            END IF
          END IF
C
C---- Look for quotation marks
C
          IF (I.LE.LINLEN) THEN
            IF (LINE(I:I).EQ.LETQT .OR. LINE(I:I).EQ.DBLQT) THEN
C             1st quote must come at beginning of string, otherwise
C             treat as normal
              IF (OLDQUT.EQ.BLANK .AND. .NOT. TOKEN) THEN
C               Start of quoted string
                OLDQUT = LETQT
                QUOTE = .TRUE.
              ELSE IF (OLDQUT.EQ.LETQT) THEN
C               End of quoted string
                OLDQUT = BLANK
                QUOTE = .FALSE.
              END IF
              GOTO 70
            END IF
          ELSE
            QUOTE = .FALSE.
          END IF
C
C---- Check for delimiting characters
C
            IF (I.LE.LINLEN) THEN
              DO 30 J = 1,NDELM
                IF (LINE(I:I).EQ.DELIM(J)) GO TO 40
 30           CONTINUE
            END IF
            J = NDELM + 1
   40       CONTINUE
C
            IF ((.NOT.QUOTE .AND. (J.LE.NDELM.OR.I.GT.LINLEN))
     +           .OR. COMMNT) THEN
C
C---- Have found a delimiter
C
              NULL = .FALSE.
              IF (.NOT.TOKEN .AND. .NOT.COMMNT .AND. J.GT.NSPDLM) THEN
C
C---- Allow delimiters other than
C     <space> & <tab> to delimit null fields
C
                IBEG(N) = I
                ITYP(N) = 0
                IEND(N) = I
                NULL = .TRUE.
              END IF
              IF (TOKEN) THEN
C
C---- End of token
C
                IEND(N) = I - 1
C               Exclude quote from token
                IF (TQUOTE .AND. OLDQUT.EQ.BLANK) IEND(N) = I - 2
C
C---- Store first 4 characters in cvalue for all types
C
                LENG = IEND(N) - IBEG(N) + 1
                IF (LENG.GT.4) LENG = 4
                L = IBEG(N)
                CVALUE(N) = LINE(L:L+LENG-1)
C
C---- Token is a number
C
                IF (NUMBER) THEN
                  ITYP(N) = 2
                  FVALUE(N) = VALUE*SIGN
                  IF (OPER.EQ.1) THEN
C                   unary +
                    FVALUE(N) = FVALUE(N) + SIGN0*VALUE0
                  ELSE IF (OPER.EQ.2) THEN
C                   unary -
                    FVALUE(N) = FVALUE(N) - SIGN0*VALUE0
                  ELSE IF (OPER.EQ.5) THEN
C                   exponent
                    FVALUE(N) = SIGN0*VALUE0*10.0**FVALUE(N)
                  END IF
                  IF (IDOT.EQ.1) THEN
                    IDEC(N) = 100*INTLEN + NPLACE
                  ELSE
                    IDEC(N) = INTLEN
                  END IF
                ELSE
C
C---- Token is alphameric
C
                  ITYP(N) = 1
                  IDEC(N) = LENG
                END IF
              END IF
              IF (TOKEN .OR. NULL) THEN
                N = N + 1
                NCHK = N + NDONE
                TOKEN = .FALSE.
                VALUE = 0.0
                OPRATR = .TRUE.
                IDOT = 0
                INTLEN = 0
                SIGN = 1.0
                OPER = 0
                TQUOTE = .FALSE.
                NUMBER = .FALSE.
C
C---- Check number of items.
C
                IF (NCHK.GT.NITEM) GO TO 80
              END IF
C             there's nothing else to do with a comment
              IF (COMMNT) GOTO 75
C
C             If delimiter was "+" or "-", also treat it as part of the
C             next token
              IF (DELIM(J).EQ.'+' .OR. DELIM(J).EQ.'-') THEN
                J = NDELM + 1
                GO TO 40
              END IF
              GO TO 70
            END IF
C
C---- Not a delimiter so must be a token -- suspect numeric token
C
            IF (.NOT.TQUOTE .AND. (.NOT.TOKEN.OR.NUMBER)) THEN
              IF (.NOT.QUOTE) THEN
                DO 50 J = 1,NDIGS
                  IF (LINE(I:I).EQ.DIGS(J)) GO TO 60
   50           CONTINUE
                J = NDIGS + 1
C
 60             CONTINUE
C----           Change "e" to "E"
                IF (J.EQ.17) J=15
C
                IF (J.LE.NDIGS) THEN
C
C----             May be number
C
                  NUMBER = .TRUE.
C
                  IF (J.LE.10) THEN
C                   Have a digit 0-9
                    IF (IDOT.EQ.0) THEN
                      INTLEN = INTLEN+1
                      VALUE = VALUE*10 + (J-1)
                    END IF
                    IF (IDOT.EQ.1) THEN
C                     Before decimal point
                      VALUE = (J-1)*F10 + VALUE
                      F10 = F10*0.1
C                     After decimal point
                      NPLACE = NPLACE + 1
                    END IF
                    OPRATR = .FALSE.
                  ELSE IF (OPRATR .AND. (J.EQ.11.OR.J.EQ.12)) THEN
C                   Find + or - as signs not operators
                    OPRATR = .FALSE.
C
C----               Set sign of number
C
                    SIGN = ISGN(J-10)
CCCC
CCCC---- Find + - * / e as operators
CCCC
CCC                  ELSE IF (J.GE.11 .AND. J.LE.15) THEN
C                   treat `e' as an operator.  former use of operators
C                   to do arithmetic between numbers is disallowed
                  ELSE IF (J.EQ.15) THEN
C
C---- Do not allow 2 operators
C
                    IF (OPRATR) NUMBER = .FALSE.
                    VALUE0 = VALUE
                    SIGN0 = SIGN
                    OPER = J - 10
                    VALUE = 0.0
                    SIGN = 1.0
                    IDOT = 0
                    OPRATR = .TRUE.
C
C---- Find a decimal point
C       decimal point
C
                  ELSE IF (J.EQ.16) THEN
                    IDOT = IDOT + 1
                    NPLACE = 0
                    F10 = 0.1
C                   A valid number has one point
                    IF (IDOT.EQ.2) NUMBER = .FALSE.
                    OPRATR = .FALSE.
                  END IF
                ELSE
C
C---- Token is not number
C
                  NUMBER = .FALSE.
                END IF
C
              END IF
C
C---- Start a new token
C
              IF (.NOT.TOKEN) THEN
C
C---- Of any type
C
                TOKEN = .TRUE.
                IBEG(N) = I
C
C---- Start quoted string
C
                IF (QUOTE) THEN
                  TQUOTE = .TRUE.
                  NUMBER = .FALSE.
                END IF
              END IF
            END IF
   70   CONTINUE
   75   N = N - 1
        RETURN
   80   CONTINUE
C
        WRITE (LINERR,FMT='(A,I4,A)') 
     +       '  ***** WARNING - MORE THAN ',NITEM,
     +       ' ITEMS IN THIS LINE - IGNORING THE REST****'
        CALL LERROR(1,0,LINERR)
        CALL LERROR(1,0,LINE(1:LENSTR(LINE)))
        N = N - 1
      RETURN
C
C     ==================================
      ENTRY PARSDL(NEWDLM,NNEWDL,NSPECD)
C     ==================================
C_BEGIN_PARSDL
C     =======================================
C     SUBROUTINE PARSDL(NEWDLM,NNEWDL,NSPECD)
C     =======================================
C
C     Call to change delimiters used by PARSE(R)
C
C  NEWDLM  (I) CHARACTER*(*)  Array containing NNEWDL new delimiters
C
C  NNEWDL  (I) INTEGER        Number of new delimiters.
C                             If .le. 0, reset delimiters to the standard
C                             default set (in DDELIM).
C
C  NSPECD  (I) INTEGER        Number of special delimiters which
C                             cannot delimit a null field. These are
C                             at the beginning of the delimiter array.
C                             (defaults in NDSDLM)
C_END_PARSDL
C
      IF (NNEWDL .LE. 0) THEN
C Reset default delimiters
        NDELM=NDDELM
        NSPDLM=NDSDLM
        DO 200, I = 1, MAXDLM
          DELIM(I)=DDELIM(I)
 200    CONTINUE
C  Set tab
        TAB = CHAR(9)
        DELIM(2) = TAB
C
      ELSE
        DO 210, I = 1, NNEWDL
          DELIM(I) = NEWDLM(I:I)
 210    CONTINUE
C
        NDELM  = NNEWDL
        NSPDLM = NSPECD
      ENDIF
C
      END
C
C_BEGIN_KEYNUM
C     ====================================================
      SUBROUTINE KEYNUM(N,NSTART,LINE,IBEG,IEND,ITYP,NTOK)
C     ====================================================
C  Check that correct number of numbers (numeric fields) are present
C
C--- Arguments:
C
C  N      (I) INTEGER        Number of consecutive numeric fields expected
C
C  NSTART (I) INTEGER        Number of first field to check
C
C  LINE   (I) CHARACTER*(*)  Array containing the fields
C
C  IBEG   (I) INTEGER(*)     First column number of fields (from PARSER)
C
C  IEND   (I) INTEGER(*)     Last column number of fields (from PARSER)
C
C  ITYP   (I) INTEGER(*)     =0  null field
C                            =1  character string
C                            =2  number
C                            (from PARSER)
C
C  NTOK   (I) INTEGER        Number of fields (from PARSER)
C
C_END_KEYNUM
C
C     .. Scalar Arguments ..
      INTEGER           N,NSTART,NTOK
      CHARACTER LINE*(*)
C     ..
C     .. Array Arguments ..
      INTEGER           IBEG(*),IEND(*),ITYP(*)
C     ..
C     .. Local Scalars ..
      INTEGER           I
      CHARACTER   LINERR*200
C     ..
C     .. External Subroutines ..
      EXTERNAL          KEYERR, CCPERR
C     ..
C
      DO 10 I = NSTART,NSTART + N - 1
          IF (I.GT.NTOK) THEN
              GO TO 30
          ELSE IF (ITYP(I).NE.2) THEN
              GO TO 20
          END IF
   10 CONTINUE
C
      RETURN
C
C          *******************************
   20 CALL KEYERR(I,2,LINE,IBEG,IEND,ITYP)
C          *******************************
C
      CALL CCPERR(1, 'Keyword error')
   30 CONTINUE
C
          WRITE (LINERR,FMT='(A,I4,A,I4,A)') 
     +     ' *** TOO FEW NUMBERS - ', (I - NSTART),
     +     ' FOUND WHEN ',N,' EXPECTED'
          CALL CCPERR(1, LINERR)
C
      END
C
      SUBROUTINE KEYERR(I,MODE,LINE,IBEG,IEND,ITYP)
C     =============================================
C  Print warning when token not of correct type.
C  Internal subroutine, called from KEYNUM.
C
C     .. Scalar Arguments ..
      INTEGER           I,MODE
      CHARACTER LINE*(*)
C     ..
C     .. Array Arguments ..
      INTEGER           IBEG(*),IEND(*),ITYP(*)
C     ..
C     .. Local Arrays ..
      CHARACTER         TYPE(3)*12
C     ..
C     .. Local Scalars ..
      CHARACTER LINERR*150
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Data statements ..
      DATA              TYPE/'alphanumeric','numeric     ',
     +                  'quoted      '/
C     ..
C
C
      IF (MODE.EQ.0) THEN
          WRITE (LINERR,FMT='(A,A,A)') 
     +  ' ** ERROR : Key word < ',
     +  LINE(IBEG(I) : IEND(I)),
     +  ' > not recognized and has therefore been ignored'
C
C              ****************************
          CALL LERROR(1,0,LINERR)
C              ****************************
C
      ELSE
          WRITE (LINERR,FMT='(A,A,A,A,A,A,A)') 
     + ' ** ERROR: Token < ',
     +  LINE(IBEG(I) : IEND(I)),
     + ' > is ',
     + TYPE(ITYP(I)),
     + ' while a ',TYPE(I),' token was expected'
C
C              ****************************
          CALL LERROR(1,0,LINERR)
C              ****************************
      END IF
C
      END
CCCC
CCCC
CCC      SUBROUTINE CHKNUM(ISYSW,N1,N2,NTOK,ITYP,IBEG,IEND,LINE)
CCCC     ======================================================
CCCC  Used with the parser routine to check that correct number
CCCC  of numbers are present on line
CCCC
CCCC  Not currently used by anything (Sep 1993). Hide it!
CCCC  Alternative is KEYNUM (?).  WRR
CCCC
CCCC     .. Scalar Arguments ..
CCC      INTEGER           ISYSW,N1,N2,NTOK
CCC      CHARACTER         LINE*(*)
CCCC     ..
CCCC     .. Array Arguments
CCC      INTEGER           IBEG(*),IEND(*),ITYP(*)
CCCC     ..
CCCC     .. Local Scalars ..
CCC      INTEGER           I,I1,I2,ISTERR,IFGERR
CCC      CHARACTER LINERR*200
CCCC     ..
CCCC     .. External Subroutines ..
CCC      EXTERNAL          CHKTOK,LERROR
CCCC
CCC      SAVE
CCCC
CCC      IF (N2.GT.NTOK) THEN
CCC          I1 = N2 - N1 + 1
CCC          I2 = NTOK - N1 + 1
CCCC
CCCC
CCC          WRITE (LINERR,FMT='(A,I4,A,I4,A)') 
CCC     +  ' *** TOO FEW NUMBERS :',I2,' FOUND WHEN',I1,
CCC     +       ' EXPECTED'
CCC          ISTERR = 2
CCC          IFGERR = 1
CCCC
CCCC              ****************************
CCCC          CALL LERROR(ISTERR,IFGERR,LINERR)
CCC          CALL CCPERR(1, LINERR)
CCC      ELSE
CCCC
CCC          DO 10 I = N1,N2
CCCC
CCCC                 ******************************************
CCC             CALL CHKTOK(ISYSW,I,2,NTOK,ITYP,IBEG,IEND,LINE)
CCCC                 ******************************************
CCCC
CCC   10     CONTINUE
CCCC
CCC      END IF
CCCC
CCCC
CCC      END
CCCC
CCC      SUBROUTINE CHKTOK(ISYSW,I,IWANT,NTOK,ITYP,IBEG,IEND,LINE)
CCCC     ========================================================
CCCC  Check token is of correct type
CCCC
CCCC Currently (Sep 1993) called only from CHKNUM which itself is not used.
CCCC                                                    WRR
CCCC      I     is token position in string line
CCCC      iwant is code for desired token
CCCC
CCCC     .. Scalar Arguments ..
CCC      INTEGER           I,ISYSW,IWANT,NTOK
CCC      CHARACTER         LINE*(*)
CCCC     ..
CCCC     .. Array Arguments ..
CCC      INTEGER          ITYP(*),IBEG(*),IEND(*)
CCCC     ..
CCCC     .. Local Scalars ..
CCC      CHARACTER LINERR*200
CCCC     ..
CCCC     .. Local Arrays ..
CCC      CHARACTER        TYPE(3)*12
CCCC
CCC      SAVE
CCCC     ..
CCCC     .. Data statements ..
CCC      DATA              TYPE/'ALPHANUMERIC','NUMERIC     ',
CCC     +                  'QUOTED      '/
CCCC     ..
CCCC
CCCC                   
CCC      IF (ITYP(I).NE.IWANT) THEN
CCC          WRITE (LINERR,FMT='(A,A,A,A,A,A,A)') 
CCC     +  ' Token ',
CCC     +  LINE(IBEG(I) : IEND(I)),
CCC     +  ' is ',
CCC     +  TYPE(ITYP(I)),
CCC     +  ' while a ',
CCC     +  TYPE(IWANT),
CCC     + ' token was expected'
CCCC
CCC          CALL CCPERR (1, LINERR)
CCC      END IF
CCCC
CCCC
CCC      END
CCCC
CCCC
CCC      SUBROUTINE GETREA(N,X,NTOK,ITYP,FVALUE)
CCCC     ======================================
CCCC Extract real number X from N'th value Parser array FVALUE, if possible
CCCC If no value, X = 0.0 . If illegal, write message
CCCC
CCCC Not currently used in any ccp4 programs, Sep 1993, WRR
CCCC
CCCC     .. Scalar Arguments ..
CCC      REAL              X
CCC      INTEGER           N,NTOK
CCCC     ..
CCCC     .. Array Arguments ..
CCC      REAL              FVALUE(*)
CCC      INTEGER           ITYP(*)
CCCC     ..
CCCC     .. Local Scalars ..
CCC      INTEGER ISTERR,IFGERR
CCC      CHARACTER LINERR*200
CCCC     ..
CCCC     .. External Subroutines ..
CCC      EXTERNAL LERROR
CCCC     ..
CCCC
CCCC
CCC      X = 0.0
CCCC
CCCC
CCC      IF (N.LE.NTOK) THEN
CCCC
CCCC
CCC          IF (ITYP(N).EQ.2) THEN
CCC              X = FVALUE(N)
CCC          ELSE IF (ITYP(N).EQ.1) THEN
CCC          WRITE (LINERR,FMT='(A,I4)') 
CCC     +    ' Illegal number in field ',N
CCC          ISTERR = 1
CCC          IFGERR = 0
CCCC
CCCC              ****************************
CCC          CALL LERROR(ISTERR,IFGERR,LINERR)
CCCC              ****************************
CCCC
CCC          END IF
CCC      END IF
CCCC
CCCC
CCC      END
CCCC
CCCC
CCC      SUBROUTINE GETINT(N,I,NTOK,ITYP,FVALUE)
CCCC     ======================================
CCCC Extract integer I from N'th value Parser array FVALUE, if possible
CCCC If no value, I = 0  . If illegal, write message
CCCC
CCCC Not currently used in any ccp4 programs, WRR, Sep 1993
CCCC
CCCC     .. Scalar Arguments ..
CCC      INTEGER           I,N,NTOK
CCCC     ..
CCCC     .. Array Arguments ..
CCC      REAL              FVALUE(*)
CCC      INTEGER           ITYP(*)
CCCC     ..
CCCC     .. Local scalars ..
CCC      INTEGER ISTERR,IFGERR
CCC      CHARACTER LINERR*100
CCCC     ..
CCCC     .. Intrinsic Functions ..
CCC      INTRINSIC         NINT
CCCC     ..
CCCC     .. External Subroutines ..
CCC      EXTERNAL LERROR
CCCC     ..
CCCC
CCCC
CCC      I = 0
CCCC
CCCC
CCC      IF (N.LE.NTOK) THEN
CCC          IF (ITYP(N).EQ.2) THEN
CCC              I = NINT(FVALUE(N))
CCC          ELSE IF (ITYP(N).EQ.1) THEN
CCCC 
CCC          WRITE (LINERR,FMT='(A,I4)') 
CCC     +   ' Illegal number in field ',N
CCC          ISTERR = 1
CCC          IFGERR = 0
CCCC
CCCC              ****************************
CCC          CALL LERROR(ISTERR,IFGERR,LINERR)
CCCC              ****************************
CCCC
CCC          END IF
CCC      END IF
CCCC
CCCC
CCC      END
C
C_BEGIN_GTNREA
C     ========================================
      SUBROUTINE GTNREA(N,M,X,NTOK,ITYP,FVALUE)
C     ========================================
C  Extract M real numbers X starting from N'th value of Parser
C  array FVALUE, if possible. If no value, X = 0.0 .
C  If illegal, write message.
C
C--- Arguments:
C
C N      (I) INTEGER    Number of 1st element of FVALUE to be extracted
C
C M      (I) INTEGER    Number of elements to be extracted
C
C X      (O) REAL(M)    Put extracted elements into this array
C
C NTOK   (I) INTEGER    Total number of fields (from PARSER)
C
C ITYP   (I) INTEGER(*)  =0  null field
C                        =1  character string
C                        =2  number
C
C FVALUE (I) REAL(*)     Array of numbers to be extracted (from PARSER)
C
C_END_GTNREA
C
C     .. Scalar Arguments ..
      INTEGER           M,N,NTOK
C     ..
C     .. Array Arguments ..
      INTEGER           ITYP(*)
      REAL              X(M),FVALUE(*)
C     ..
C     .. Local Scalars ..
      INTEGER           I,K
      CHARACTER LINERR*100
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C
      DO 10 I = 1,M
          K = I + N - 1
          X(I) = 0.0
C
          IF (K.LE.NTOK) THEN
              IF (ITYP(K).EQ.2) THEN
                  X(I) = FVALUE(K)
              ELSE IF (ITYP(K).EQ.1) THEN
           WRITE (LINERR,FMT='(A,I4)') 
     +    ' Illegal number in field ',K
C
C              ****************************
          CALL LERROR(1,0,LINERR)
C              ****************************
C
              END IF
          END IF
   10 CONTINUE
      END
C
C_BEGIN_GTNINT
C     ========================================
      SUBROUTINE GTNINT(N,M,J,NTOK,ITYP,FVALUE)
C     ========================================
C Extract M integers J starting from N'th value of Parser array FVALUE,
C if possible. If no value, J = 0 . If illegal, write message
C
C--- Arguments:
C
C N      (I) INTEGER     Number of 1st element of FVALUE to be extracted
C
C M      (I) INTEGER     Number of elements to be extracted
C
C J      (O) INTEGER(M)  Put extracted elements into this array
C
C NTOK   (I) INTEGER     Total number of fields (from PARSER)
C
C ITYP   (I) INTEGER(*)  =0  null field
C                        =1  character string
C                        =2  number
C
C FVALUE (I) REAL(*)     Array of numbers to be extracted (from PARSER)
C
C_END_GTNINT
C
C     .. Scalar Arguments ..
      INTEGER           M,N,NTOK
C     ..
C     .. Array Arguments ..
      INTEGER           J(M),ITYP(*)
      REAL              FVALUE(*)
C     ..
C     .. Local Scalars ..
      INTEGER           I,K
      CHARACTER LINERR*200
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         NINT
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C
      DO 10 I = 1,M
          K = I + N - 1
          J(I) = 0
          IF (K.LE.NTOK) THEN
              IF (ITYP(K).EQ.2) THEN
                  J(I) = NINT(FVALUE(K))
              ELSE IF (ITYP(K).EQ.1) THEN
           WRITE (LINERR,FMT='(A,I4)') 
     +    ' Illegal number in field ',K
C
C              ****************************
          CALL LERROR(1,0,LINERR)
C              ****************************
C
              END IF
          END IF
   10 CONTINUE
C
      END
C
C_BEGIN_GTPREA
C     ======================================
      SUBROUTINE GTPREA(N,X,NTOK,ITYP,FVALUE)
C     ======================================
C Extract real number X from N'th value Parser array FVALUE, if possible
C If no value, leave X unchanged. If illegal, write message
C
C--- Arguments:
C
C N      (I) INTEGER    Number of 1st element of FVALUE to be extracted
C
C X      (O) REAL       Extracted number put here
C
C NTOK   (I) INTEGER    Total number of fields (from PARSER)
C
C ITYP   (I) INTEGER(*)  =0  null field
C                        =1  character string
C                        =2  number
C
C FVALUE (I) REAL(*)     Array of numbers to be extracted (from PARSER)
C
C_END_GTPREA
C
C     .. Scalar Arguments ..
      REAL X
      INTEGER N,NTOK
C     ..
C     .. Array arguments ..
      REAL FVALUE(*)
      INTEGER ITYP(*)
C     ..
C     .. Local Scalars ..
      CHARACTER LINERR*200
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C
      IF (N.LE.NTOK) THEN
        IF (ITYP(N).EQ.2) THEN
          X = FVALUE(N)
        ELSE IF (ITYP(N).EQ.1) THEN
           WRITE (LINERR,FMT='(A,I4)') 
     +    ' Illegal number in field ',N
C
C              ****************************
          CALL LERROR(1,0,LINERR)
C              ****************************
C
        END IF
      ELSE
        CALL LERROR (1, 0, 'Real number expected at end of line')
      END IF
C
      END
C
C_BEGIN_GTPINT
C     ======================================
      SUBROUTINE GTPINT(N,I,NTOK,ITYP,FVALUE)
C     ======================================
C Extract integer I from N'th value Parser array FVALUE, if possible
C If no value, leave I unchanged. If illegal, write message
C
C--- Arguments:
C
C N      (I) INTEGER    Number of 1st element of FVALUE to be extracted
C
C I      (O) INTEGER    Extracted number put here
C
C NTOK   (I) INTEGER    Total number of fields (from PARSER)
C
C ITYP   (I) INTEGER(*)  =0  null field
C                        =1  character string
C                        =2  number
C
C FVALUE (I) REAL(*)     Array of numbers to be extracted (from PARSER)
C
C_END_GTPINT
C
C     .. Scalar Arguments ..
      INTEGER I,N,NTOK
C     ..
C     .. Arrays arguments ..
      REAL FVALUE(*)
      INTEGER ITYP(*)
C     ..
C     .. Local Scalars ..
      INTEGER ISTERR,IFGERR
      CHARACTER LINERR*100
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
C
      IF (N.LE.NTOK) THEN
        IF (ITYP(N).EQ.2) THEN
          I = NINT(FVALUE(N))
        ELSE IF (ITYP(N).EQ.1) THEN
           WRITE (LINERR,FMT='(A,I4)') 
     +    ' Illegal number in field ',N
          ISTERR = 1
          IFGERR = 0
C
C              ****************************
          CALL LERROR(ISTERR,IFGERR,LINERR)
C              ****************************
C
        END IF
      END IF
C
      END
CCCC
CCCC_BEGIN_GETSTR
CCCC     ===================================================
CCC      SUBROUTINE GETSTR(N,STRING,NTOK,ITYP,IBEG,IEND,LINE)
CCCC     ===================================================
CCCC Extract string STRING from N'th value Parser array CVALUE, if possible
CCCC If no value, STRING  = blank '    '.
CCCC
CCCC--- Arguments:
CCCC
CCCC N      (I) INTEGER        Number of 1st field of LINE to be extracted
CCCC
CCCC STRING (O) CHARACTER*(*)  Extracted string put here
CCCC
CCCC NTOK   (I) INTEGER        Total number of fields (from PARSER)
CCCC
CCCC ITYP   (I) INTEGER(*)     =0  null field
CCCC                           =1  character string
CCCC                           =2  number
CCCC                           (from PARSER)
CCCC
CCCC IBEG   (I) INTEGER(*)     1st column number in field (from PARSER)
CCCC
CCCC IEND   (I) INTEGER(*)     last column number in field (from PARSER)
CCCC
CCCC LINE   (I) CHARACTER*(*)  String extracted from here. (from PARSER)
CCCC
CCCC NB. This subroutine not currently used in any CCCP4 programs. WRR, Sep 1993
CCCC
CCCC_END_GETSTR
CCCC
CCCC     .. Scalar Arguments ..
CCC      INTEGER           N,NTOK
CCC      INTEGER           ITYP(*),IBEG(*),IEND(*)
CCC      CHARACTER         STRING*(*), LINE*(*)
CCCC     ..
CCC      STRING = ' '
CCC      IF (N.LE.NTOK .AND. ITYP(N).NE.0) STRING = LINE(IBEG(N):IEND(N))
CCC      END
CCCC
CCCC
C
C_BEGIN_SBLANK
C     ==============================
      SUBROUTINE SBLANK(ARRAY,N1,N2)
C     ==============================
C Blank characters N1 to N2 of ARRAY
C
C--- Arguments:
C
C ARRAY (I/O)  CHARACTER(*)
C
C N1    (I)    INTEGER
C
C N2    (I)    INTEGER
C
C_END_SBLANK
C
      CHARACTER*1 ARRAY(*)
C
      DO 10 I=N1,N2
         ARRAY(I)=' '
10     CONTINUE
C
      RETURN
      END
CCCC
CCCC_BEGIN_GTCFLD
CCCC     ====================================================
CCC      SUBROUTINE GTCFLD(NFIELD,ITEXT,NCHAR,MIN,MAX,IGFLAG)
CCCC     ====================================================
CCCC
CCCC---- This subroutine finds the minimum and maximum character 
CCCC     numbers in a packed text string for a requested field number. 
CCCC     The character fields are assumed to be separated by spaces
CCCC
CCCC---- Arguments: 
CCCC
CCCC NFIELD (I) INTEGER            The number of the field to be retrieved
CCCC
CCCC ITEXT  (I) CHARACTER*1(NCHAR) Array containing the packed character string
CCCC                               to be interpreted
CCCC
CCCC NCHAR  (I) INTEGER            The no. of characters in the text string
CCCC
CCCC MIN    (O) INTEGER            The no. of the first character
CCCC                               in the requested field
CCCC
CCCC MAX    (O) INTEGER            The no. of the final character
CCCC                               in the requested field
CCCC
CCCC IGFLAG (O) INTEGER            = -1 blank field found (end of text string)
CCCC
CCCC NB. Not currently called by anything in ccp4. WRR, Sep 1993
CCCC
CCCC_END_GTCFLD
CCCC
CCC      CHARACTER*1 ITEXT(NCHAR)
CCCC
CCCC---- initialisations
CCCC
CCC      IGFLAG=0
CCC      IFIELD=0
CCC      I=0
CCC      MIN=0
CCC      MAX=NCHAR
CCCC
CCCC---- skip spaces up to start of next field
CCCC
CCC10    I=I+1
CCCC
CCC      IF(I.GT.NCHAR) THEN
CCC       IGFLAG = -1
CCC       RETURN 
CCC       ENDIF
CCCC
CCC      IF(ITEXT(I).EQ.' ')GO TO 10
CCCC
CCCC---- character field found
CCCC
CCC      IFIELD=IFIELD+1
CCC      MIN=I
CCCC
CCCC---- search for end of the character field
CCCC
CCC20    I=I+1
CCC      IF(I.GT.NCHAR)GO TO 100
CCC      IF(ITEXT(I).NE.' ')GO TO 20
CCCC
CCCC---- end of character field found.  see if required field
CCCC     has been found
CCCC
CCC      IF(IFIELD.NE.NFIELD)GO TO 10
CCC      MAX=I-1
CCC      RETURN
CCCC
CCCC---- end of string reached
CCCC
CCC100   IF(IFIELD.NE.NFIELD) THEN
CCC            IGFLAG=-1
CCC            RETURN 
CCC            END IF
CCC      END
CCCC
CCCC_BEGIN_CPYCHR
CCCC     ========================================
CCC      SUBROUTINE CPYCHR(STRINGA,STRINGB,NCHAR)
CCCC     ========================================
CCCC---- Copy nchar characters from character array b to a
CCCC
CCCC---- Arguments:
CCCC
CCCC STRINGA (O) CHARACTER*1(*)  Array to copy to
CCCC
CCCC STRINGB (I) CHARACTER*1(*)  Array to copy from
CCCC
CCCC NCHAR   (I) INTEGER         Number of characters to copy
CCCC
CCCC NB. Not currently called by anything in ccp4. WRR, Sep 1993
CCC
CCCC
CCCC_END_CPYCHR
CCCC
CCC      INTEGER NCHAR
CCC      CHARACTER*1 STRINGA(*),STRINGB(*)
CCCC
CCCC
CCC      DO 10 I=1,NCHAR
CCC      STRINGA(I)=STRINGB(I)
CCC10    CONTINUE
CCC      END
C
C_BEGIN_CMATCH
C     ==============================================
      LOGICAL FUNCTION CMATCH(STRING1,STRING2,NCHAR)
C     ==============================================
C
C---- Compare nchar character in string1 and string2
C     return cmatch .true. if all match, else .false.
C
C---- Arguments:
C
C STRING1 (I) CHARACTER*(*)  1st string to compare
C
C STRING2 (I) CHARACTER*(*)  2nd string to compare
C
C NCHAR   (I) INTEGER        number of characters to compare
C
C_END_CMATCH
C
      CHARACTER*(*) STRING1,STRING2
      INTEGER NCHAR
C
      IF(STRING1(1:NCHAR).EQ.STRING2(1:NCHAR)) THEN
          CMATCH=.TRUE.
      ELSE
          CMATCH=.FALSE.
      ENDIF
      END
CCCC
CCCC_BEGIN_CMOVE
CCCC     =======================================
CCC      SUBROUTINE CMOVE(STRINGA,STRINGB,NCHAR)
CCCC     =======================================
CCCC
CCCC---- Copy NCHAR characters from STRINGB to STRINGA
CCCC
CCCC---- Arguments:
CCCC
CCCC STRINGA (O) CHARACTER*1(*)  Array to copy to
CCCC
CCCC STRINGB (I) CHARACTER*1(*)  Array to copy from
CCCC
CCCC NCHAR   (I) INTEGER         Number of characters to copy
CCCC
CCCC NB. Not currently called by anything in ccp4. WRR, Sep 1993
CCCC NB. Alternative is CMATCH
CCCC
CCCC_END_CMOVE
CCCC
CCC      CHARACTER*1 STRINGA(*),STRINGB(*)
CCC      INTEGER NCHAR
CCCC
CCC      IF(NCHAR.LE.0)RETURN
CCCC
CCC      DO 10 I=1,NCHAR
CCC      STRINGA(I)=STRINGB(I)
CCC10    CONTINUE
CCCC
CCC      RETURN
CCC      END
C
C_BEGIN_CHKKEY
C     ========================================
      SUBROUTINE CHKKEY(KEY,WORDS,NWORDS,IKEY)
C     ========================================
C Check keyword KEY against list of NWORDS possible keywords in WORDS.
C Allows abbreviated or extended keys provided they are not ambiguous.
C
C---- Arguments:
C
C KEY    (I) CHARACTER*(*)         Keyword for checking
C
C WORDS  (I) CHARACTER(NWORDS)*(*) List of possible keywords
C
C NWORDS (I) INTEGER               Number of keywords in WORDS
C
C IKEY (I/O) INTEGER               = '?', list all words
C                                  Returns:
C                                  = keyword number found (.gt.0)
C                                  = 0 if not found or null
C                                  = -1 if ambiguous
C
C_END_CHKKEY
C
      INTEGER NFMAX
      PARAMETER (NFMAX=20)
C     .. Scalar Arguments ..
      INTEGER NWORDS, IKEY
      CHARACTER KEY*(*)
C     ..
C     .. Array Arguments ..
      CHARACTER WORDS(NWORDS)*(*)
C     ..
C     .. Local Scalars ..
      INTEGER LK,I,L,NFOUND
      CHARACTER LINERR*200
C     ..
C     .. Local Arrays ..
      INTEGER LFOUND(20)
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL PUTLIN,LERROR
C     ..
C
C---- Get minimum significant length of KEY 
C     ( function LENSTR returns the length
C     of the character string excluding trailing blanks)
C
      IKEY=0
C
C        ***********
      LK=LENSTR(KEY)
C        ***********
C
C---- Ignore null string
C
      IF(LK.LE.0) RETURN
C
      IF(KEY(1:1).EQ.'?') THEN
C  
C           ****************
       CALL PUTLIN(' Possible keywords are:','HLPWIN')
C           ****************
C
       DO 10 JDO = 1,NWORDS
C
C            ****************
        CALL PUTLIN(WORDS(JDO),'HLPWIN')
C            ****************
C
10      CONTINUE
C
            IKEY=0
            RETURN
      ENDIF
C
      NFOUND=0
C
C---- Check all possible words in case of ambiguities
C
      DO 20 I=1,NWORDS
C
C----  Key may be longer than word in list
C
C              ****************
      L=MIN(LK,LENSTR(WORDS(I)))
C              ****************
C
      IF(L.LE.0) GO TO 20
C
C---- Find out if KEY is an initial substring of this option word
C
      IF(INDEX(WORDS(I),KEY(1:L)).EQ.1) THEN
            NFOUND=NFOUND+1
C
            IF(NFOUND.GT.NFMAX) THEN
          WRITE (LINERR,FMT='(A,I5)') 
     +  ' CHKKEY: too many ambiguities : ',NFMAX
C
C              ****************************
          CALL LERROR(1,0,LINERR)
C              ****************************
C
                  NFOUND=NFMAX
            ELSE
                  LFOUND(NFOUND)=I
            ENDIF
       ENDIF
20     CONTINUE
C
C---- If keyword is ambiguous, list possibilities
C
      IF(NFOUND.GT.1) THEN
          WRITE (LINERR,FMT='(A,A,A)') 
     +   ' Keyword ',
     +   KEY(1:LK),
     +  ' is ambiguous: possibilities are -'
C
C              ****************************
          CALL LERROR(1,0,LINERR)
C              ****************************
C
       DO 30 JDO = 1,NWORDS
C
C            ****************
        CALL PUTLIN(WORDS(JDO),'HLPWIN')
C            ****************
C
30      CONTINUE
            IKEY=-1
      ELSEIF (NFOUND.EQ.1) THEN
C
C---- Success if only 1 found
C
            IKEY=LFOUND(1)
      ENDIF
      END
C
C_BEGIN_PUTLIN
C     ================================
      SUBROUTINE PUTLIN(STROUT,OUTWIN)
C     ================================
C---- This is a dummy PUTLIN to link with the MTZ routines mark 1 -
C     all it does is write the line in STROUT to lun 6. Later the
C     routines will be linked with the Compose-Parser etc. from Kim
C     where PUTLIN does a few more things !
C
C---- Arguments:
C
C STROUT (I) CHARACTER*(*)  Input line
C
C OUTWIN (O) CHARACTER*(*)  Not used
C
C_END_PUTLIN
C
C     .. Scalar Arguments ..
      CHARACTER OUTWIN* (*)
      CHARACTER STROUT* (*)
C     ..
C     .. Local Scalars ..
      INTEGER LUNOUT
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. Data statements ..
      DATA LUNOUT/6/
C     ..
C
      LL = LENSTR(STROUT)
      IF (LL.GE.132) THEN
      LX = 1
      LS = 131
10    CONTINUE
      WRITE (LUNOUT,FMT=6000) STROUT(LX:LS)
      IF (LS.EQ.LL) GOTO 20
      LX = LS  + 1
      LS = LS + 130
      IF (LS.GT.LL) LS = LL
      GO TO 10
      ELSE
       IF (LL.EQ.0) THEN
           WRITE(LUNOUT,FMT=6000)
            ELSE
      WRITE (LUNOUT,FMT=6000) STROUT(1:LL)
           END IF
      END IF
20    CONTINUE
C
C---- Format statements
C
 6000 FORMAT (' ',A)
C
      END
C
C_BEGIN_BLANK
C     ===============================
      SUBROUTINE BLANK(OUTWIN,NLINES)
C     ===============================
C---- This subroutine calls PUTLIN to output NLINES blank lines to the
C     window OUTWIN
C
C---- Arguments:
C
C     OUTWIN  (I)   CHARACTER*6     output window
C
C     NLINES  (I)   INTEGER         number of blank lines to output
C
C_END_BLANK
C
C     .. Scalar Arguments ..
      INTEGER NLINES
      CHARACTER OUTWIN*(*)
C     ..
C     .. Local Scalars ..
      INTEGER JDO10
C     ..
C     .. External Subroutines ..
      EXTERNAL PUTLIN
C     ..
C
      DO 10 JDO10 = 1,MAX(NLINES,1)
C
C            **************
        CALL PUTLIN(' ',OUTWIN)
C            **************
C
   10 CONTINUE
      END
C
C_BEGIN_LERROR
C     =======================================
      SUBROUTINE LERROR(ERRFLG,IFAIL,ERRMSG)
C     =======================================
C---- General error reporting subroutine, for the MTZ routines, etc
C
C---- Arguments:
C
C     ERRFLG  (I)  INTEGER         =1 output meesage as warning
C                                  =2 output message as fatal
C
C     IFAIL   (I)  INTEGER         =0 return after fatal error
C                                  =-1 STOP after reporting fatal error
C
C     ERRMSG  (I)  CHARACTER*(*)   character string containing error
C                                  message to output
C
C_END_LERROR
C
C     .. Scalar Arguments ..
      INTEGER ERRFLG,IFAIL
      CHARACTER ERRMSG* (*)
C     ..
C     ..
C     .. External Subroutines ..
      EXTERNAL BLANK,PUTLIN
C     ..
C
      IF (ERRFLG.EQ.1) THEN
C
C---- Output a warning message and return
C
        CALL BLANK('ERRWIN',1)
        CALL PUTLIN('***  Warning','ERRWIN')
        CALL PUTLIN(ERRMSG,'ERRWIN')
        CALL BLANK('ERRWIN',1)
C
      ELSE IF (ERRFLG.EQ.2) THEN
C
C---- Output a fatal message, and quit or return depending on IFAIL
C
        CALL BLANK('ERRWIN',1)
        CALL PUTLIN('***  Error','ERRWIN')
        CALL PUTLIN(ERRMSG,'ERRWIN')
        IF (IFAIL.LT.0) THEN
          call ccperr(1,'*** Program Terminated ')
        ELSE
          CALL BLANK('ERRWIN',1)
        END IF
        RETURN
      ELSE
C
C---- Bad errflg, output message and continue
C
        CALL BLANK('ERRWIN',1)
        CALL PUTLIN('*** Unrecognised  error','ERRWIN')
        CALL PUTLIN(ERRMSG,'ERRWIN')
        CALL PUTLIN('Program continuing ...','ERRWIN')
        CALL BLANK('ERRWIN',1)
C
      END IF
      END
C
C
C_BEGIN_RDSYMM
C     =======================================================
      SUBROUTINE RDSYMM(JTOK,LINE,IBEG,IEND,ITYP,FVALUE,NTOK,
     .    SPGNAM,NUMSGP,PGNAME,NSYM,NSYMP,RSYM)
C     =======================================================
C---- Read and decode symmetry specification
C
C---- Arguments:
C
C   JTOK    (I)  INTEGER        Number of first field to interpret
C
C   LINE    (I)  CHARACTER*(*)  Input string (from PARSER)
C
C   IBEG    (I)  INTEGER(*)     1st column number of tokens in field 
C                               (from PARSER)
C
C   IEND    (I)  INTEGER(*)     Last column number of tokens in field
C                               (from PARSER)
C
C   ITYP    (I)  INTEGER(*)     =0  null field
C                               =1  character string
C                               =2  number
C                               (from PARSER)
C
C   FVALUE  (I)  REAL(*)        Array of numbers. (from PARSER)
C
C   NTOK    (I)  INTEGER        The number of fields parsed. (from PARSER)
C
C     
C   NSYM  (I/O)  INTEGER        Number of symmetry operations already read,
C                               including non-primitive.
C                               (should be cleared to 0 at beginning)
C
C   SPGNAM  (O) CHARACTER*(*)   Space group name
C
C   NUMSGP  (O) INTEGER         Space group number
C
C   PGNAME  (O) CHARACTER*(*)   Point group name
C
C   NSYMP   (O) INTEGER         Number of primitive symmetry operations
C
C   RSYM    (O) REAL(4,4,*)     Symmetry matrices. * should be at least =NSYM
C
C_END_RDSYMM
C     
      INTEGER JTOK,NTOK
      INTEGER IBEG(*),IEND(*),ITYP(*)
      REAL FVALUE(*)
      CHARACTER*(*)LINE,SPGNAM,PGNAME
      INTEGER NUMSGP,NSYM,NSYMP
      REAL RSYM(4,4,*)
C     
C---- Look at next field on line: this can be
C     (a) a space-group number
C     (b) a space-group name, ie a string beginning P,I,R,F,A,B or C
C     (c) a symmetry operation (anything else)
C     
C---- for cases (a) & (b), this is a single field:
C     case (c) is more than 1 field
C     
      IF (JTOK.GT.NTOK) THEN
         CALL  PUTLIN(' No symmetry data !!!','CURWIN')
      ELSE
         IF (JTOK.EQ.NTOK) THEN
            SPGNAM = ' '
            IF (NSYM.GT.0) THEN
               CALL  PUTLIN('Warning: symmetry already given','CURWIN')
            ENDIF
C     
C---- A single field, see if it is a number or a string
C     
            IF (ITYP(JTOK).EQ.2) THEN
C     
C---- it's a number, treat as space-group number
C     
               NUMSGP = NINT(FVALUE(JTOK))
            ELSE
C     
C---- it's a string, treat as space-group name
C     
               SPGNAM = LINE(IBEG(JTOK) :IEND(JTOK))
               NUMSGP = 0
            END IF
C     
C---- Read symmetry (all operations) from SYMOP
C     open symop on channel 24 - closed at end of reading
C     NSYMP returns number of primitive operations
C     
            CALL  CCPUPC(SPGNAM)
            CALL  MSYMLB(24,NUMSGP,SPGNAM,PGNAME,NSYMP,NSYM,RSYM)
         ELSE
C     
C     
C---- Read symmetry operations
C     
            NSYM = NSYM + 1
            NSYMP = NSYM
            CALL  CCPUPC(LINE)
            CALL  SYMFR2(LINE,IBEG(JTOK),NSYM,RSYM)
            NUMSGP = 0
            SPGNAM = ' '
            PGNAME = ' '
C     
         END IF
      END IF
      END
C     
C_BEGIN_RDHEAD
C     ======================================================
      SUBROUTINE RDHEAD(JTOK,LINE,IBEG,IEND,ITYP,FVALUE,NTOK,
     .    MTZPRT,MTZBPR)
C     ======================================================
C---- Read and decode HEADER command, to set print flags for MTZ headers
C
C---- Arguments:
C 
C   JTOK   (I) INTEGER       Number of first field to interpret
C
C   LINE   (I) CHARACTER*(*) Input string (from PARSER)
C
C   IBEG   (I) INTEGER(*)    1st column number of tokens in field 
C                            (from PARSER)
C
C   IEND   (I) INTEGER(*)    Last column number of tokens in field
C                            (from PARSER)
C
C   ITYP   (I) INTEGER(*)    =0  null field
C                            =1  character string
C                            =2  number
C                            (from PARSER)
C
C   FVALUE (I) REAL(*)       Array of numbers. (from PARSER)
C
C   NTOK   (I) INTEGER       The number of fields parsed. (from PARSER)
C
C     
C   MTZPRT (O) INTEGER       Flag to control printout from MTZ file header
C                            NONE    sets MTZPRT = 0
C                             no header o/p
C                            BRIEF   sets MTZPRT = 1 (default)
C                             brief header o/p
C                            HISTORY sets MTZPRT = 2
C                             brief + mtz history
C                            ALL     sets MTZPRT = 3
C                             full header o/p from mtz reads
C     
C   MTZBPR (O) INTEGER       Controls printout from BATCH HEADERS
C                            NOBATCH     sets MTZBPR = 0
C                             no batch header o/p
C                            BATCH       sets MTZBPR = 1  (default)
C                             batch titles o/p
C                            ORIENTATION sets MTZBPR = 2
C                             batch orientation also
C
C_END_RDHEAD
C     
      INTEGER JTOK,NTOK
      INTEGER IBEG(*),IEND(*),ITYP(*)
      REAL FVALUE(*)
      CHARACTER*(*) LINE
      INTEGER MTZPRT,MTZBPR
C     
C     Locals
      INTEGER I,IKEY
      CHARACTER KEY*12
C     
      INTEGER NKEYS
      PARAMETER (NKEYS=7)
      CHARACTER*12 KEYS(NKEYS)
      DATA KEYS/'NONE','BRIEF','HISTORY','ALL',
     $     'NOBATCH','BATCH','ORIENTATION'/
C     
C     Set defaults
      MTZPRT = 1
      MTZBPR = 1
C     
C     Loop keywords
      IF (NTOK .GE. JTOK) THEN
         DO 10, I=JTOK,NTOK
            KEY = LINE(IBEG(I):IEND(I))
            CALL CCPUPC(KEY)
            CALL CHKKEY(KEY,KEYS,NKEYS,IKEY)
            IF (IKEY .LE. 0) THEN
              CALL PUTLIN
     +             ('Unrecognized or ambiguous subkeyword to HEADER: '
     +             // KEY,'CURWIN')
            ELSE
               IF (IKEY .EQ. 1) MTZPRT = 0
               IF (IKEY .EQ. 2) MTZPRT = 1
               IF (IKEY .EQ. 3) MTZPRT = 2
               IF (IKEY .EQ. 4) MTZPRT = 3
               IF (IKEY .EQ. 5) MTZBPR = 0
               IF (IKEY .EQ. 6) MTZBPR = 1
               IF (IKEY .EQ. 7) MTZBPR = 2
            ENDIF
 10      CONTINUE 
      ENDIF
      END
C     
C_BEGIN_RDCELL
C     ==============================================
      SUBROUTINE RDCELL(ITOK,ITYPE,FVALUE,NTOK,CELL)
C     ==============================================     
C---- Read and decode cell parameters 
C     
C---- Arguments:
C
C   ITOK   (I) INTEGER     Number of first field to interpret
C
C   ITYPE  (I) INTEGER(*)  =0  null field
C                          =1  character string
C                          =2  number
C                          (from PARSER)
C
C   FVALUE (I) REAL(*)     Array of numbers. (from PARSER)
C
C   NTOK   (I) INTEGER     The number of fields parsed. (from PARSER)
C
C   CELL   (O) REAL(6)     Cell parameters a, b, c, alpha, beta, gamma.
C
C_END_RDCELL
C     
C     .. Scalar Arguments ..
      INTEGER           ITOK,NTOK
C     ..
C     .. Array Arguments ..
      REAL              CELL(6),FVALUE(*)
      INTEGER           ITYPE(*)
C     ..
C     .. External Subroutines ..
      EXTERNAL          GTPREA
C     ..
C     
      IF (NTOK .LT. ITOK+2) THEN
        CALL LERROR (1,0,'Cell a, b and c not given -- ignored')
        RETURN
      END IF
      CELL(4) = 90.0
      CELL(5) = 90.0
      CELL(6) = 90.0
C     
C     ***************************************
      CALL GTPREA(ITOK,CELL(1),NTOK,ITYPE,FVALUE)
      CALL GTPREA(ITOK+1,CELL(2),NTOK,ITYPE,FVALUE)
      CALL GTPREA(ITOK+2,CELL(3),NTOK,ITYPE,FVALUE)
C     ***************************************
C     
C     *********************************************
      IF (ITOK+3.LE.NTOK) CALL GTPREA(ITOK+3,CELL(4),NTOK,ITYPE,FVALUE)
      IF (ITOK+4.LE.NTOK) CALL GTPREA(ITOK+4,CELL(5),NTOK,ITYPE,FVALUE)
      IF (ITOK+5.LE.NTOK) CALL GTPREA(ITOK+5,CELL(6),NTOK,ITYPE,FVALUE)
C     *********************************************
      END
C     
C     
C_BEGIN_RDRESO
C     ================================================
      SUBROUTINE RDRESO(ITOK,ITYPE,FVALUE,NTOK,RESMIN,
     +                  RESMAX,SMIN,SMAX)
C     ================================================
C---- Read and decode resolution limits.
C     
C---- Arguments:
C
C     ITOK    (I) INTEGER     Number of first field to interpret
C     
C     ITYPE   (I) INTEGER(*)  =0  null field
C                             =1  character string
C                             =2  number
C                             (from PARSER)
C
C     FVALUE  (I) REAL(*)     Array of numbers. (from PARSER)
C
C     NTOK    (I) INTEGER     The number of fields parsed. (from PARSER)
C
C     
C     RESMIN  (O) REAL        Minimum resolution (in As)
C
C     RESMAX  (O) REAL        Maximum resolution (in As)
C
C     SMIN    (O) REAL        Minimum resolution ( 4sin**2/lambda**2)
C
C     SMAX    (O) REAL        Maximum resolution ( 4sin**2/lambda**2)
C
C_END_RDRESO
C     .. Scalar Arguments ..
      REAL              RESMAX,RESMIN,SMAX,SMIN
      INTEGER           ITOK,NTOK
C     ..
C     .. Array Arguments ..
      REAL              FVALUE(*)
      INTEGER           ITYPE(*)
C     ..
C     .. Local Scalars ..
      REAL              RESTEM,STEM
C     ..
C     .. External Subroutines ..
      EXTERNAL          GTPREA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         SQRT
C     ..
C
C---- Global defaults set here
C
        RESMAX = 10000.0
        RESMIN = .1
        IF (NTOK.LT.ITOK) THEN
          CALL LERROR(1,0,'Resolution missing')
          RETURN
        END IF
C     
C---- Look at next field on line: this can be
C     read resolution limits in A, if only one treat as high
C     resolution limit
C     
C     *************************************
      CALL GTPREA(ITOK,RESMIN,NTOK,ITYPE,FVALUE)
C     *************************************
C     
      IF (ABS(RESMIN).LE.0.000001) RESMIN = 0.00001
C     
C     ***************************************
      IF (NTOK.GT.ITOK)
     +      CALL GTPREA(ITOK+1,RESMAX,NTOK,ITYPE,FVALUE)
C     ***************************************
C     
      IF (ABS(RESMAX).LE.0.0000001) RESMAX = 100.0
C     
C     
      IF (RESMIN.LE.RESMAX) THEN
         RESTEM = RESMAX
         RESMAX = RESMIN
         RESMIN = RESTEM
      END IF
C     
C---- option to read 4sin**2/lamda**2
C     
      IF (RESMIN.LE.1.0 .AND. RESMAX.LE.1.0) THEN
C     
C---- swap over smin and resmin etc
C     
         SMIN = RESMIN
         SMAX = RESMAX
         RESMAX = SQRT(1.0/SMIN)
         RESMIN = SQRT(1.0/SMAX)
      ELSE
         SMIN = 1.0/RESMAX**2
         SMAX = 1.0/RESMIN**2
      END IF
C     
C     
      IF (SMIN.GT.SMAX) THEN
         STEM = SMAX
         SMAX = SMIN
         SMIN = STEM
      END IF
C     
C     
      RETURN
      END
C     
C     
C_BEGIN_RDSCAL     
C     ======================================================
      SUBROUTINE RDSCAL(ITOK,LINE,IBEG,IEND,ITYP,FVALUE,NTOK,
     .    NLPRGI,LSPRGI,ILPRGI,SCAL,BB)
C     ======================================================
C---- Read and decode SCALE .
C     
C---- Arguments:
C
C  ITOK   (I/O) INTEGER     Input: number of first field to interpret
C                           Output: number of next token to interpret (.gt. 0)
C                                  =  0 if line exhausted (SCAL & BB OK)
C                                  = -1 if no scale given
C                                  = -2 unrecognized label
C
C  LINE   (I) CHARACTER*(*) Input string (from PARSER)
C
C  IBEG   (I) INTEGER(*)    1st column number of tokens in field 
C                           (from PARSER)
C
C  IEND   (I) INTEGER(*)    Last column number of tokens in field
C                           (from PARSER)
C
C  ITYP   (I) INTEGER(*)    =0  null field
C                           =1  character string
C                           =2  number
C                           (from PARSER)
C
C  FVALUE (I) REAL(*)       Array of numbers. (from PARSER)
C
C  NTOK   (I) INTEGER       The number of fields parsed. (from PARSER)
C
C  LSPRGI (I) CHARACTER(*)*30  Program label strings.
C                                  L(abel) S(tring) PRG(ram) I(nput)
C
C  NLPRGI (I) INTEGER        Number of label strings in LSPRGI
C
C  ILPRGI (O) INTEGER        Number in array of LSPRGI whose scale has been reset
C
C  SCAL   (O) REAL           Scale factor, no default
C
C  BB     (O) REAL           Temperature factor, default = 0.0
C
C_END_RDSCAL
C     
      INTEGER ITOK,NTOK,ILPRGI,NLPRGI,JDO
      INTEGER IBEG(*),IEND(*),ITYP(*)
      REAL FVALUE(*)
      CHARACTER*(*) LINE
      CHARACTER*30 LSPRGI(*),CWORK
      REAL SCAL,BB
C     
      CWORK = LINE(IBEG(ITOK) :IEND(ITOK))
      DO 10 JDO = 1,NLPRGI
C     
         IF (CWORK.EQ.LSPRGI(JDO)) GO TO 20
C     
 10   CONTINUE
C     
C     ***********************
      CALL PUTLIN('**** Error input assignment does not match'//
     +     ' program labels','ERRWIN')
C     ***********************
C     
      ITOK = -2
      RETURN
C     
 20   ILPRGI = JDO
      IF(ITOK+1.GT.NTOK) THEN
         ITOK = -1
         RETURN
      ELSE
         IF (ITYP(ITOK+1) .EQ. 2) THEN
            CALL GTPREA(ITOK+1,SCAL,NTOK,ITYP,FVALUE)
         ELSE
            ITOK = -1
            RETURN
         ENDIF
      ENDIF
C
      BB = 0
      IF(ITOK+2.LE.NTOK) THEN
         IF (ITYP(ITOK+2) .EQ. 2) THEN
            CALL GTPREA(ITOK+2,BB,NTOK,ITYP,FVALUE)
            ITOK = ITOK + 3
         ELSE
            ITOK = ITOK + 2
         ENDIF
         IF (ITOK .GT. NTOK) ITOK = 0
      ELSE
         ITOK = 0
      ENDIF
C     
      RETURN
      END 
C
C
C_BEGIN_RDRESL
C     ======================================================
      SUBROUTINE RDRESL(ITOK,ITYPE,FVALUE,CVALUE,NTOK,RESMIN,
     +                  RESMAX,SMIN,SMAX,ISTAT)
C     ======================================================     
C---- Read and decode resolution limits.
C     Subkeywords in CVALUE recognized:
C       LOW   read next number as low resolution limit
C       HIGH  read next number as high resolution limit
C
C     If LOW & HIGH are both present, the limits will still be swapped
C     to the correct order
C
C     If only LOW or HIGH are given, the unset limit (ie either RESMAX, SMAX
C     or RESMIN, SMIN) will be set to -1.0. If only one number is given,
C     it is treated as a high resolution limit
C
C     If both limits are given without keywords, and both are .lt. 1.0,
c     it is assumed that the limits are 4(sin theta/lambda)**2 rather than A
C
C---- Arguments:
C
C  ITOK   (I) INTEGER         Number of first field to interpret
C
C  ITYP   (I) INTEGER(*)      =0  null field
C                             =1  character string
C                             =2  number
C                             (from PARSER)
C
C  FVALUE (I) REAL(*)         Array of numbers. (from PARSER)
C
C  NTOK   (I) INTEGER         The number of fields parsed. (from PARSER)
C
C  CVALUE (I) CHARACTER(*)*4  Parsed tokens from program input. (from PARSER)
C
C  RESMIN  (O) REAL           Minimum resolution (in As) (ie low resolution)
C
C  RESMAX  (O) REAL           Maximum resolution (in As) (ie high resolution)
C
C  SMIN    (O) REAL           Minimum resolution ( 4sin**2/lambda**2)
C                                (ie low resolution)
C
C  SMAX    (O) REAL           Maximum resolution ( 4sin**2/lambda**2)
C                                (ie high resolution)
C
C  ISTAT   (O) INTEGER        =0  OK
C                             =-1 illegal subkeyword
C                             =+1 no limits set
C                             =+2 illegal number (probably can't happen)
C_END_RDRESL
C     
C     .. Scalar Arguments ..
      REAL              RESMAX,RESMIN,SMAX,SMIN
      INTEGER           ITOK,NTOK,ISTAT
C     ..
C     .. Array Arguments ..
      REAL              FVALUE(*)
      INTEGER           ITYPE(*)
      CHARACTER*4       CVALUE(*)
C     ..
C     .. Local Scalars ..
      REAL              RESTEM,STEM
      INTEGER           N, KMNMX, NSET, LFLAG, NKEYS, IKEY
      LOGICAL           BOTH, KEYWRD
      CHARACTER*4 SUBKEY(2)
C     ..
C     .. External Subroutines ..
      EXTERNAL          GTTREA, CCPUPC, CHKKEY
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         SQRT
C     ..
      DATA SUBKEY /'LOW', 'HIGH'/
      DATA NKEYS /2/
C
C---- Global defaults set here
C
      RESMAX = -1.0
      RESMIN = -1.0
      SMIN   = -1.0
      SMAX   = -1.0
      NSET   = 0
      KMNMX  = 1
      ISTAT  = 0
      BOTH = .TRUE.
      KEYWRD = .FALSE.
C     
      N  = ITOK
C
 1    IF (N .LE. NTOK) THEN
C
         IF (ITYPE(N) .EQ. 1) THEN
C String
            CALL CCPUPC(CVALUE(N))
            CALL CHKKEY(CVALUE(N),SUBKEY,NKEYS,IKEY)
            IF(IKEY.LE.0) THEN
               ISTAT = -1
               RETURN
C              ======
            ELSEIF (IKEY .EQ. 1) THEN
C----- subkey LOW
               KMNMX = 1
            ELSEIF (IKEY .EQ. 2) THEN
C----- subkey HIGH
               KMNMX = 2
            ENDIF
            BOTH = .NOT. BOTH
            KEYWRD = .TRUE.
         ELSE
C Number
            RESTEM = 0.0
C                ******************************************
            CALL GTTREA(N,RESTEM,LFLAG,NTOK,ITYPE,FVALUE)
C                ******************************************
            IF (LFLAG .EQ. 0) THEN
               IF (KMNMX .EQ. 1) THEN
                  RESMIN = RESTEM
                  NSET   = NSET+1
                  KMNMX  = 2
               ELSEIF (KMNMX .EQ. 2) THEN
                  RESMAX = RESTEM
                  NSET = NSET+1
                  KMNMX  = 1
               ENDIF
            ELSE
               ISTAT = +2
            ENDIF
         ENDIF
         N = N+1
         GO TO 1
      ENDIF
C
C  Have any numbers been set?
      IF (NSET .EQ. 0) THEN
         ISTAT = +1
         RETURN
C        ======
      ELSEIF (NSET .EQ. 1) THEN
C One only set, if no keywords have been defined, use single number as
C     high resolution limit
         IF (BOTH) THEN
            RESMAX = RESMIN
            RESMIN = -1.0
         ENDIF
      ENDIF
C     
C---- option to read 4sin**2/lamda**2
      IF (.NOT. KEYWRD .AND. NSET .EQ. 2) THEN
         IF (RESMIN .GT. 0.0 .AND. RESMIN .LE. 1.0 .AND.
     $       RESMAX .GT. 0.0 .AND. RESMAX .LE. 1.0) THEN
C---- swap over SMIN and RESMIN
C     
            SMIN = RESMIN
            RESMIN = SQRT(1.0/SMIN)
C---- swap over SMAX and RESMAX 
            SMAX = RESMAX
            RESMAX = SQRT(1.0/SMAX)
         END IF
      ENDIF
C     
      IF (RESMIN .GT. 0.0) THEN
         SMIN = 1.0/RESMIN**2
      END IF
C     
      IF (RESMAX .GT. 0.0) THEN
            SMAX = 1.0/RESMAX**2
      ENDIF
C     
C---- Check that they are in the correct order, if both limits read
C     
      IF (NSET .EQ. 2) THEN
         IF (RESMIN.LE.RESMAX) THEN
            RESTEM = RESMAX
            RESMAX = RESMIN
            RESMIN = RESTEM
         ENDIF
         IF (SMIN.GT.SMAX) THEN
            STEM = SMAX
            SMAX = SMIN
            SMIN = STEM
         ENDIF
      ENDIF
      END
C
C_BEGIN_GTTREA
C     =============================================
      SUBROUTINE GTTREA(N,X,LFLAG,NTOK,ITYP,FVALUE)
C     =============================================
C---- Extract real number X from N'th value of Parser array FVALUE,
C     if possible.
C
C     If no value, leave X unchanged. If illegal, write message
C
C---- Arguments:
C
C  N      (I) INTEGER     Number of 1st element of FVALUE to be extracted
C
C  X      (O) REAL        Put extracted number here
C
C  LFLAG  (O) INTEGER     =  0  OK (valid number or null field)
C                         = -1  beyond end of line
C                         = +1  illegal number
C
C  NTOK   (I) INTEGER     Total number of fields (from PARSER)
C
C  ITYP   (I) INTEGER(*)  =0  null field
C                         =1  character string
C                         =2  number
C                         (from PARSER)
C
C  FVALUE (I) REAL(*)     Array of numbers to be extracted (from PARSER)
C
C_END_GTTREA
C
C     .. Scalar Arguments ..
      REAL X
      INTEGER N,NTOK,LFLAG
C     ..
C     .. Array arguments ..
      REAL FVALUE(*)
      INTEGER ITYP(*)
C     ..
C     .. Local Scalars ..
      CHARACTER LINERR*200
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C
      LFLAG = 0
      IF (N.LE.NTOK) THEN
        IF (ITYP(N).EQ.2) THEN
          X = FVALUE(N)
        ELSE IF (ITYP(N).EQ.1) THEN
           WRITE (LINERR,FMT='(A,I4)') 
     +    ' Illegal number in field ',N
C
C              ****************************
          CALL LERROR(1,0,LINERR)
C              ****************************
C
          LFLAG = +1
        END IF
      ELSE
         LFLAG = -1
      END IF
C
      END
C
C_BEGIN_GTTINT
C     =============================================
      SUBROUTINE GTTINT(N,I,LFLAG,NTOK,ITYP,FVALUE)
C     =============================================
C---- Extract integer I from N'th value of Parser array FVALUE,
C     if possible.
C
C     If no value, leave I unchanged. If illegal, write message.
C
C---- Arguments:
C
C  N      (I) INTEGER     Number of 1st element of FVALUE to be extracted
C
C  I      (O) INTEGER     Put extracted number here
C
C  LFLAG  (O) INTEGER     =  0  OK (valid number or null field)
C                         = -1  beyond end of line
C                         = +1  illegal number
C
C  NTOK   (I) INTEGER     Total number of fields (from PARSER)
C
C  ITYP   (I) INTEGER(*)  =0  null field
C                         =1  character string
C                         =2  number
C                         (from PARSER)
C
C  FVALUE (I) REAL(*)     Array of numbers to be extracted (from PARSER)
C
C_END_GTTINT
C
C      IMPLICIT NONE
C     .. Scalar Arguments ..
      INTEGER I,N,NTOK,LFLAG
C     ..
C     .. Arrays arguments ..
      REAL FVALUE(*)
      INTEGER ITYP(*)
C     ..
C     .. Local Scalars ..
      CHARACTER LINERR*100
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
C
      LFLAG = 0
      IF (N.LE.NTOK) THEN
        IF (ITYP(N).EQ.2) THEN
          I = NINT(FVALUE(N))
        ELSE IF (ITYP(N).EQ.1) THEN
           WRITE (LINERR,FMT='(A,I4)') ' Illegal number in field ',N
C
C              ****************************
          CALL LERROR(1,0,LINERR)
C              ****************************
C
          LFLAG = +1
        END IF
      ELSE
         LFLAG = -1
      END IF
      END
