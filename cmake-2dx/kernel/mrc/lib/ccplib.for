C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
C_BEGIN_CCPLIB
C     These are supposedly-machine-independent low-level routines.
C     They're actually machine-dependent at least insofar as some
C     contain non-standard code, but they do compile with the compilers
C     tried on unix as well as VMS.
C
C     fixme: the bit-twiddling should be in library.c, not here.
C     amalgamate ccppsf and fdir/fext/froot.  also add tests of these
C     routines to testlib.
C
C     $Id: ccplib.f,v 1.69 1999/12/16 16:04:29 mdw Exp $
C
C      CCFILL    Set specified number of elements of byte array
C      CCPALC    Call subroutine with allocated memory
C      CCPALE    Call subroutine with allocated memory set from environment
C      CCPBYI    Copy array of unsigned (or signed) bytes into integer array
C      CCPBYT    Indicate whether byte handling is available
C      CCPCPI    Copy array of BYTE or INTEGER*2 elements into integer array
C      CCPDAT    Get calendar date
C      CCPDEX    Periodicity reduction of 1024 (for PROLSQ)
C      CCPDPN    more friendly CCPOPN
C      CCPE2I    read integer from logical name value
C      CCPERR    Report error or normal termination and stop
C      CCPEXS    test if file exists
C      CCPFYP    Set up environment & parse command line arguments
C      CCPGI2    Get unsigned integer*2 value from 0 to 65535 from N'th
C                unsigned integer*2 element of array.
C      CCPGTB    Get unsigned byte value from 0 to 255 from N'th byte of
C                array.
C      CCPI2I    Copy an array of INTEGER*2 elements into an integer array
C      CCPIBY    Copy array of integers into array of bytes.
C      CCPII2    Copy array of integers into array of INTEGER*2 elements.
C      CCPLWC    Convert a string to lower case
C      CCPMDE    If byte handling available return nos. of bytes for map
C                modes
C      CCPMVB    Move bytes from one non-character array to another if
C                byte handling is available
C      CCPMVI    Move words from one integer array to another
C      CCPMVR    Move words from one real array to another
C      CCPNUN    Return an unconnected i/o unit number
C      CCPONL    See if program is being run interactively
C      CCPPSF    Parse file name into components
C      CCPRCS    Like CCPVRS but use RCS-format date string
C      CSETNV    Associate logical name with file name
C      CCPPAG    Set paging parameters if available
C      CCPSI2    Set integer value from 0 to 65535 into the N'th
C                unsigned integer*2 element of an array.
C      CCPSTB    Set integer value from 0 to 255 into N'th byte of array.
C      CCPSUM    Sum the elements of an array
C      CCPTIM    Get CPU and Elapsed times
C      CCPTOI    Convert n'th byte or I*2 in a non-character array to an
C                integer
C      CCPUFL    Supress underflow messages
C      CCPUPC    Convert a string to upper case
C      CCPVRS    Print program version number and date header
C      CCPZBI    Sets an array of bytes to zero
C      CCPZI     Set 'n' words of an integer array to zero using a simple loop
C      CCPZR     Set 'n' words of a real array to zero using a simple loop
C      FDIR      Returns the directory part of a file name
C      FEXTN     Returns the extension of a file name
C      FROOT     Returns the root of a file name
C      LITEND    determine endianness
C      LENSTR    length of string to last non-space
C      LUNSTI    Get logical unit number for input
C      LUNSTO    Get logical unit number for output
C      NBITST    Return the (unsigned) integer value held within a bit
C                field in a word
C      NOCRLF    write line supressing cr/lf to standard output
C      QPRINT    write debug messages
C      STBITS    Set a bit field within a word to a given (unsigned)
C                integer value
C_END_CCPLIB
C
C
C
C_BEGIN_CCFILL
      SUBROUTINE CCFILL(ARR1,SCAL,NTIMES)
C     ===================================
C
C      CCFILL    Set NTIMES bytes array ARR1 to value SCAL
C
C Arguments:
C ==========
C
C        ARR1 (O)   BYTE ARRAY (*): WHERE BYTES ARE TO BE COPIED
C        SCAL (I)   BYTE: value to be copied into ARR1
C      NTIMES (I)   INTEGER: NUMBER OF BYTES TO BE COPIED
C_END_CCFILL
C
C     .. Scalar Arguments ..
      INTEGER NTIMES
      INTEGER*1 SCAL
C     ..
C     .. Array Arguments ..
      INTEGER*1 ARR1(*)
C     ..
C     .. Local Scalars ..
      INTEGER N
C     ..
      DO 10 N = 1,NTIMES
        ARR1(N) = SCAL
   10 CONTINUE
C
      END
C
C
C_BEGIN_CCPALC
      SUBROUTINE CCPALC(ROUTNE, N, TYPE, LENGTH)
C     ==========================================
C
C     Arrange to call subroutine ROUTNE with N array arguments each of
C     length LENGTH (i) and type indicated by TYPE (i): 'i' == integer,
C     'r' == real, 'd' == double precision, 'c' == complex, 'b' ==
C     "byte" (logical*1 or integer*1, unportable and deprecated) .  TYPE
C     elements may have either case.
C     Consider `call ccpalc (fred, 3, types, lens)' with types = (/'i',
C     'r', 'c'/)  and lens = (/1000, 2000, 3000/).  This effectively does
C        call fred (1000, arr1, 2000, arr2, 3000, arr3)
C     with
C        subroutine fred (n1, foo, n2, bar, n3, baz)
C        integer n1, n2, n3, foo (n1)
C        real bar (n2)
C        complex baz (n3)
C        ...
C     Obviously all communication with ROUTNE must be by COMMON (or,
C     possibly, extra ENTRYs).  The allocated memory is freed on return
C     from ROUTNE.  As a concession, it's initially filled with zeroed
C     bytes.

C
C Arguments:
C ==========
C
C      ROUTNE (I)   EXTERNAL: routine to call
C           N (I)   INTEGER: number of arguments to ROUTNE (<=12)
C        TYPE (I)   CHARACTER*1 (*): type of arguments to ROUTNE:
C                      'I': INTEGER; 'R': REAL; 'D': DOUBLE PRECISION;
C                      'C': COMPLEX; 'B': LOGICAL*1 or INTEGER*1
C      LENGTH (I)   INTEGER*(*): number of elements in each (array)
C                       argument of ROUTNE
C_END_CCPALC
C
C     .. Scalar Arguments ..
      INTEGER N
C     ..
C     .. Array Arguments ..
      CHARACTER TYPE (*)
      INTEGER LENGTH (*)
C     ..
      EXTERNAL ROUTNE, CCPAL1, CCPUPC
      INTEGER I, ITYPE (12)
      CHARACTER TTYPE (12)
C     ..
      IF (N.LT.1 .OR. N.GT.12)
     +     CALL CCPERR (1, 'CCPALC: bad number of arguments')
      DO 10 I=1,N
        TTYPE (I) = TYPE (I)
        CALL CCPUPC (TTYPE (I))
        ITYPE (I) = INDEX ('IRDCB', TTYPE (I))
        IF (ITYPE (I) .EQ. 0) CALL CCPERR (1, 'CCPALC: bad TYPE: '//
     +       TYPE (I))
        IF (LENGTH (I).LE.0) CALL CCPERR (1, 'CCPALC: length <=0')
 10   CONTINUE
      CALL CCPAL1 (ROUTNE, N, ITYPE, LENGTH)
      END
C
C
C_BEGIN_CCPALE
      SUBROUTINE CCPALE(ROUTNE, N, TYPE, LENGTH, LENDEF, PRINT)
C     =================================================
C
C     Arrange to call subroutine ROUTNE with N array arguments each of
C     length LENGTH (i) and type indicated by TYPE (i): 'i' == integer,
C     'r' == real, 'd' == double precision, 'c' == complex, 'b' == byte.
C     TYPE elements may have either case.  LENGTH points to an array of
C     environment variable (logical) names from which integer values are
C     read.  The lengths default to values from LENDEF.
C     This is a convenient interface to CCPALC to allow configuring of
C     the memory requirements on the command line where appropriate.
C     This may be useful if the memory requirements can't be determined
C     initially and it's necessary to guess.
C
C Arguments:
C ==========
C
C      ROUTNE (I)   EXTERNAL: routine to call
C           N (I)   INTEGER: number of arguments to ROUTNE (<=12)
C        TYPE (I)   CHARACTER*1 (*): type of arguments to ROUTNE:
C                      'I': INTEGER; 'R': REAL; 'D': DOUBLE PRECISION;
C                      'C': COMPLEX; 'B': LOGICAL*1 or INTEGER*1
C     LENGTH (I)   CHARACTER *(*): logical names representing the number
C                       of elements in each (array) argument of ROUTNE
C     LENDEF (I)   INTEGER (*): default lengths for the argument arrays
C     used if the appropriate LENGTH argument doesn't represent a
C     defined logical
C     PRINT  (I)   LOGICAL: whether or not to print the values of the
C     array lengths
C_END_CCPALE
C
C     .. Scalar Arguments ..
      INTEGER N
      LOGICAL PRINT
C     ..
C     .. Array Arguments ..
      CHARACTER TYPE (*),  LENGTH (*)*(*)
      INTEGER LENDEF (*)
C     ..
      EXTERNAL ROUTNE, CCPE2I, CCPALC, LUNSTO
      INTEGER I, LENG (12), CCPE2I, LUNSTO
C     ..
      DO 10 I=1,N
        LENG (I) = CCPE2I (LENGTH (I), LENDEF (I))
 10   CONTINUE
      IF (PRINT) THEN
        WRITE (LUNSTO(1), 
     +     '(/'' Memory allocation (logical name, type, elements):'')')
        WRITE (LUNSTO(1), '(3X, A, 1X, A, 3X, I10)')
     +       (LENGTH (I), TYPE (I), LENG (I), I=1,N)
      ENDIF
      CALL CCPALC (ROUTNE, N, TYPE, LENG)
      END
C
C
C
C SUBROUTINE 'CCPBYI'
C ===================
C
C_BEGIN_CCPBYI
      SUBROUTINE CCPBYI(IA,IBYT,NB)
C     =============================
C
C COPY AN ARRAY OF UNSIGNED (OR SIGNED) BYTES INTO AN INTEGER ARRAY
C
C (MUST BE IMPLEMENTED IF CCPBYT FUNCTION RETURNS .TRUE.)
C [added for LAUE]
C
C Arguments:
C ==========
C
C      IA (O)   INTEGER ARRAY(*): TO RETURN INTEGER VALUES
C    IBYT (I)   BYTE ARRAY(*): DATA (MAY BE AN INTEGER ARRAY FOR EXAMPLE
C               WITH DATA PACKED INTO ADJACENT BYTES
C      NB (I)   INTEGER: IF >0, THE NUMBER OF UNSIGNED BYTES TO BE COPIED
C                        IF <0, -THE NUMBER OF SIGNED BYTES TO BE COPIED
C_END_CCPBYI
C
C SPECIFICATION STATEMENTS
C ------------------------
C
      INTEGER IA(*)
      INTEGER*1 IBYT(*)
      INTEGER*1 JBYT(4)
      EQUIVALENCE (JA,JBYT(1))
      LOGICAL CALLED, LITEND
      INTEGER IND
      EXTERNAL LITEND
      SAVE CALLED, IND
      DATA CALLED/.FALSE./
C
      IF (.NOT.CALLED) THEN
        CALLED=.TRUE.
        IF (LITEND(1)) THEN
          IND = 1
        ELSE
          IND = 4
        ENDIF
      ENDIF
C
C COPY DATA
C ---------
C
      NE = NB
      IF (NE.GT.0) THEN
         JA=0
         DO 10 I=1,NE
           JBYT(IND)=IBYT(I)
           IA(I)=JA
 10      CONTINUE
      ELSE
         NE = -NE
         DO 20 I=1,NE
         IA(I) = IBYT(I)
20       CONTINUE
      END IF
      END
C
C
C
C_BEGIN_CCPBYT
      LOGICAL FUNCTION CCPBYT(NBW)
C     ============================
C
C---- This function indicates whether byte handling is available or not.
C      if a value of .true. is returned then the subroutines ccpmde and
C      ccpmvb must be fully implemented.
C
C Arguments:
C ==========
C
C         NBW (O)   INTEGER: RETURNS THE NUMBER OF BYTES PER WORD OR A VALUE
C                   OF 1 IF NO BYTE HANDLING IS AVAILABLE.
C
C  RETURNS   CCPBYT  = .TRUE.  BYTE HANDLING AND ASSOCIATED CCPLIB
C                              ROUTINES AVAILABLE.
C                    = .FALSE. NO BYTE HANDLING AVAILABLE.
C_END_CCPBYT
C
C     .. Scalar Arguments ..
      INTEGER NBW
C     ..
      CCPBYT = .TRUE.
      NBW = 4
      END
C
C
C SUBROUTINE 'CCPCPI'
C ===================
C_BEGIN_CCPCPI
      SUBROUTINE CCPCPI(IA,IB,MINEL,MAXEL,ITYP)
C     =========================================
C
C Copy an array of BYTE or INTEGER*2 elements into an integer array
C
C (Must be implemented if ccpbyt function returns .TRUE.)
C [for LAUE]
C
C Arguments:
C ==========
C
C      IA (O)   INTEGER Array(*): to return values
C      IB (I)   INTEGER Array(*): holding data with data packed into adjacant 
C                                 BYTE or INTEGER*2 elements
C   MINEL (I)   INTEGER: Minimum element to copy
C   MAXEL (I)   INTEGER: Maximum element to copy
C    ITYP (I)   INTEGER: Type =1 unsigned byte
C                             =2 signed byte
C                             =3 unsigned two byte integer
C                             =4 signed two byte integer
C
C               Note: if MINEL>MAXEL elements will be copied in reverse order
C_END_CCPCPI
C
C====== Specification statements
C
      INTEGER IA(*)
      INTEGER*1 IB(*)
      INTEGER*2 J2(2)
      INTEGER*1 JBYT(4)
      EQUIVALENCE (JA,J2(1),JBYT(1))
      LOGICAL CALLED, LITEND
      EXTERNAL LITEND
      INTEGER IND1, IND2, INDB
      SAVE CALLED, IND1, IND2, INDB
      DATA CALLED/.FALSE./
C
      IF (.NOT.CALLED) THEN
        IF (LITEND(1)) THEN
          IND1 = 1
          IND2 = 2
          INDB = 1
        ELSE
          IND1 = 3
          IND2 = 4
          INDB = 4
        ENDIF
        CALLED=.TRUE.
      ENDIF
C
C====== Copy data
C
      ISTEP = 1
      IF (MINEL.GT.MAXEL) ISTEP=-1
      IF (ITYP.EQ.1) THEN
         JA=0
         J=0
         DO 10 I=MINEL,MAXEL,ISTEP
            J=J+1
            JBYT(INDB)=IB(I)
            IA(J)=JA
10       CONTINUE
      ELSE IF (ITYP.EQ.2) THEN
         J=0
         DO 20 I=MINEL,MAXEL,ISTEP
            J=J+1
            IA(J)=IB(I)
20       CONTINUE
      ELSE IF (ITYP.EQ.3) THEN
         JA=0
         J=0
         DO 30 I=MINEL,MAXEL,ISTEP
            J=J+1
            JBYT(IND1)=IB(2*I-1)
            JBYT(IND2)=IB(2*I)
            IA(J)=JA
30       CONTINUE
      ELSE IF (ITYP.EQ.4) THEN
         J=0
         DO 40 I=MINEL,MAXEL,ISTEP
            J=J+1
            JBYT(1)=IB(2*I-1)
            JBYT(2)=IB(2*I)
            IA(J)=J2(1)
40       CONTINUE
      END IF
      RETURN
      END
C
C
C
C_BEGIN_CCPDAT
      SUBROUTINE CCPDAT(CALDAT)
C     =========================
C
C---- This subroutine returns the date if available
C
C Arguments:
C ==========
C
C      CALDAT (O)   CHARACTER*8: DATE AS DD/MM/YY
C                   (RETURNED AS A BLANK STRING IF NOT AVAILABLE)
C_END_CCPDAT
C
C SPECIFICATION STATEMENTS
C ------------------------
C
C---- Get date
C
C     .. Scalar Arguments ..
      CHARACTER CALDAT*8
C     ..
C     .. Local Scalars ..
      INTEGER ID,IM,IY
C     ..
C     .. External Subroutines ..
      EXTERNAL UIDATE
C     ..
C
      CALL UIDATE(IM,ID,IY)
      IY = MOD (IY, 100)      
      WRITE (CALDAT,FMT=6000) ID,IM,IY
C
C---- Format statements
C
 6000 FORMAT (I2,'/',I2,'/',I2)
C
      IF (CALDAT(7:7) .EQ. ' ') CALDAT(7:7) = '0'
      END
C
C
C
C_BEGIN_CCPDEX
      SUBROUTINE CCPDEX(INDX,N)
C     =========================
C
C---- This subroutine performs a periodicity reduction for a period
C     of 1024 for the elements of an array. written particularly for
C     'prolsq' to allow for use of the 'and' function on the cray or
C     'moveb' on the m28oh(iap).
C      These are much faster than the mod function used in
C      the standard fortran77 version.
C
C Arguments:
C ==========
C
C        INDX (I/O) INTEGER ARRAY(*): NUMBERS FOR PERIODICITY REDUCTION
C           N (I)   INTEGER: NO. OF ELEMENTS IN INDX
C
C EXAMPLE OF FUNCTIONS:
C
C FORTRAN77     INDX(I)=MOD(INDX(I),1024)+1
C CRAY-1S       INDX(I)=AND(INDX(I),1023)+1
C M280H(IAP)    CALL MOVEB(INDX(I),1,0,1,22)
C               INDX(I)=INDX(I)+1
C_END_CCPDEX
C
C SPECIFICATION STATEMENTS AND CODE
C
C     .. Scalar Arguments ..
      INTEGER N
C     ..
C     .. Array Arguments ..
      INTEGER INDX(N)
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
      DO 10 I = 1,N
        INDX(I) = MOD(INDX(I),1024) + 1
   10 CONTINUE
      END
C
C_BEGIN_CCPDPN
      SUBROUTINE CCPDPN(IUN,LOGNAM,STATUS,TYPE,LREC,IFAIL)
C     ====================================================
C
C---- Calls CCPOPN to open a file, but with mnemonic arguments
C
C Arguments:
C ==========
C
C         IUN (I)   INTEGER: UNIT NUMBER
C
C      LOGNAM (I)   CHARACTER*(*): LOGICAL FILE NAME
C
C      STATUS (I)   CHARACTER*(*): FILE STATUS FLAG:
C                                     'UNKNOWN'
C                                     'SCRATCH'
C                                     'OLD'
C                                     'NEW'
C                                     'READONLY'
C                                     'PRINTER'
C
C        TYPE (I)   CHARACTER*(*): FILE TYPE FLAG:
C                                  ='F', 'SEQUENTIAL' 'FORMATTED'
C                                  ='U', 'SEQUENTIAL' 'UNFORMATTED'
C                                  ='DF', 'DIRECT'     'FORMATTED'
C                                  ='DU', 'DIRECT'     'UNFORMATTED'
C     [STATUS and TYPE are case-insensitive]
C
C        LREC (I)   INTEGER: RECORD LENGTH FOR DIRECT ACCESS FILE (NO. OF
C                   CHARACTERS FOR A FORMATTED FILE OR WORDS FOR
C                   AN UNFORMATTED FILE). NOT RELEVANT FOR A SEQUENTIAL
C                   FILE
C
C       IFAIL (I/O) INTEGER: ON INPUT     =0, STOP ON OPEN FAILURE
C                                         =1, CONTINUE AFTER OPEN FAILURE
C                                             (only on file not found)
C                                         =-1, As 0, but silent on success
C                                             (equivalent to negative IUN)
C                            ON OUTPUT    UNCHANGED IF FILE OPEN OK
C                                         =-1, ERROR IN OPENING FILE
C_END_CCPDPN
C
C     .. Scalar Arguments ..
      INTEGER IFAIL,IUN,IUN1,LREC
      CHARACTER LOGNAM* (*),STATUS* (*),TYPE* (*)
C     ..
C     .. Local Scalars ..
      INTEGER ISTAT,ITYPE
      CHARACTER ERRSTR*80
C     ..
C     .. Local Arrays ..
      CHARACTER TYPES(4)*2,STATS(6)*8, STAT*8, TYP*2
C     ..
C     .. External Functions ..
      INTEGER CCPNUN,LENSTR
      EXTERNAL CCPNUN,LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPOPN
C     ..
C     .. Data statements ..
      DATA STATS/'UNKNOWN','SCRATCH','OLD','NEW','READONLY','PRINTER'/
      DATA TYPES/'F','U','DF','DU'/
C     ..
C
      IF (IUN .EQ. 0) IUN = CCPNUN()
      STAT = STATUS
      TYP = TYPE
      CALL CCPUPC(STAT)
      CALL CCPUPC(TYP)
      DO 10 ISTAT = 1,6
        IF (STAT.EQ.STATS(ISTAT)) GO TO 20
   10 CONTINUE
      ERRSTR = ' CCPDPN: illegal status : '
      ERRSTR(LENSTR(ERRSTR)+2:) = STATUS
      CALL CCPERR(1,ERRSTR)
C
   20 DO 30 ITYPE = 1,4
        IF (TYP.EQ.TYPES(ITYPE)) GO TO 40
   30 CONTINUE
      ERRSTR = ' CCPDPN: illegal type: '
      ERRSTR(LENSTR(ERRSTR)+2:) = TYPE
      CALL CCPERR(1,ERRSTR)
C
 40   CONTINUE
      IUN1 = IUN
C  If IFAIL lt 0 No open message from CCPOPN
      IF(IFAIL.LT.0 .AND. IUN.GT.0) THEN
        IUN1 = -IUN
        IFAIL = 0
      ENDIF
      CALL CCPOPN(IUN1,LOGNAM,ISTAT,ITYPE,LREC,IFAIL)
C
      END
C
C
C_BEGIN_CCPE2I
      INTEGER FUNCTION CCPE2I (NAME, DEFVAL)
C     ======================================
C
C     Return an integer extracted from enviroment variable NAME.  If
C     NAME isn't defined, use DEFVAL as the default.  If the value of
C     NAME isn't a representation of an integer, abort.
C
C     Arguments
C     =========
C
C     NAME (I)    CHARACTER *(*)
C     DEFVAL (I)  INTEGER
C_END_CCPE2I
      CHARACTER *(*) NAME
      CHARACTER BUFFER*80, EMESS*100
      INTEGER DEFVAL, LENSTR
      EXTERNAL UGTENV, LENSTR
      CALL UGTENV (NAME, BUFFER)
      IF (BUFFER.EQ.' ') THEN
        CCPE2I = DEFVAL
        RETURN
      ENDIF
      READ (BUFFER, '(BN,I80)', ERR=99) CCPE2I
      RETURN 
 99   EMESS = ' Logical name '
      EMESS(LENSTR(EMESS)+2:) = NAME(1:LENSTR(NAME))
      IF(LENSTR(EMESS) .LE. 99) THEN
        EMESS(LENSTR(EMESS)+1:) =' should represent an integer and is: '
        IF(LENSTR(EMESS) .LE. 98) 
     .           EMESS(LENSTR(EMESS)+2:) = BUFFER(1:LENSTR(BUFFER))
      ENDIF
      CALL CCPERR (1, EMESS)
      END
C
C
C     ===============================
C_BEGIN_CCPERR
      SUBROUTINE CCPERR(ISTAT,ERRSTR)
C     ===============================
C
C     Arguments:
C     ==========
C
C     ISTAT (I)   INTEGER: error/warning level
C
C         ISTAT=0  Normal termination and stop.
C         ISTAT=1  Fatal error and stop.
C         ISTAT=2  Report severe warning.
C         ISTAT=3  Report information.
C         ISTAT=4  Report from library
C            (ISTAT=-1 also report latest system error
C             and terminate)
C
C     ERRST (I)   CHARACTER*(*): message
C
C_END_CCPERR
C
C     .. Arguments ..
      INTEGER ISTAT
      CHARACTER ERRSTR*(*)
C     ..
C     .. Locals ..
      CHARACTER ERRBUF*100
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      LOGICAL VAXVMS
      EXTERNAL LENSTR, VAXVMS
C     ..
C     .. External Routines ..
      EXTERNAL CCPPNM, QPRINT, CEXIT, GETELAPSED, UGERR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C
C
      IF (ABS(ISTAT).LE.2) THEN
        call ccp4h_summary_beg()
      ENDIF
      IF (ISTAT.LT.0) THEN
        CALL UGERR(0,ERRBUF)
C     (avoid VMS `Message number 00000000')
        IF (ERRBUF .NE. ' ' .AND.
     +       ERRBUF.NE.'Message number 00000000') THEN
          CALL QPRINT(0,
     +         'Last system error message:')
          CALL QPRINT(0,ERRBUF)
        ENDIF
      ENDIF

C     construct appropriate ERRBUF
      IF (ABS(ISTAT).LE.1) THEN
C        report program name
         CALL CCPPNM (ERRBUF)
         IF (LENSTR(ERRBUF) .LT. 95) THEN
            ERRBUF (LENSTR(ERRBUF)+1:) = ': '
            ERRBUF (LENSTR(ERRBUF)+3:) = ERRSTR
         ENDIF
      ELSEIF (ISTAT.EQ.2) THEN
         ERRBUF = ' WARNING: '//ERRSTR
      ELSE
         ERRBUF = ERRSTR
      ENDIF

C     report messages and exit if appropriate
      IF (ABS(ISTAT).LE.1) THEN
        CALL QPRINT(0,ERRBUF)
        IF (VAXVMS()) THEN
          IF (ISTAT.EQ.0) THEN
            CALL GETELAPSED
C           success
            call ccp4h_pre_end()
            call ccp4h_summary_end()
            call ccp4h_html_close()
            CALL CEXIT(1)
          ELSE
C           FOR$_NOTFORSPE, "Not a FORTRAN-specific error"
            call ccp4h_pre_end()
            call ccp4h_summary_end()
            call ccp4h_html_close()
            CALL CEXIT(1605644)
          ENDIF
        ELSE
C         duplicate message to stderr, assumed to be connected to unit 0
          IF (ISTAT.EQ.1) WRITE (0,*) ERRBUF
          CALL GETELAPSED
          call ccp4h_pre_end()
          call ccp4h_summary_end()
          call ccp4h_html_close()
          CALL CEXIT(ISTAT)
        ENDIF
      ELSEIF (ISTAT.EQ.2) THEN
        CALL QPRINT(0,' ')
        CALL QPRINT(0,' $TEXT:Warning: $$ comment $$ ')
        CALL QPRINT(0,ERRBUF)
        CALL QPRINT(0,' $$')
        call ccp4h_summary_end()
      ELSE
        CALL QPRINT(0,ERRBUF)
      END IF
C
      RETURN
      END
C
C
C
C_BEGIN_CCPEXS
      LOGICAL FUNCTION CCPEXS(NAME)
C     =============================
C
C---- Tests if file assigned to logical name NAME exists
C
C Returns CCPEXS  .true.  if file exists
C                 .false. if file does not exist
C
C Arguments:
C ==========
C
C    NAME (I)   CHARACTER*(*): Logical name
C_END_CCPEXS
C
C     .. Scalar Arguments ..
      CHARACTER NAME* (*)
C     ..
C     .. Local Scalars ..
      CHARACTER*255 NAMFIL
C     ..
      NAMFIL = ' '
      CALL UGTENV(NAME,NAMFIL)
      IF (NAMFIL.EQ.' ') NAMFIL = NAME
C
      INQUIRE (FILE=NAMFIL,EXIST=CCPEXS)
C
      END
C
C
C     =================
C_BEGIN_CCPFYP
      SUBROUTINE CCPFYP
C     =================
C
C---- Used to set up the environment in which the program runs
C     and then parse the command line arguments.
C
C
C---- The logic for locating the "environ.def" and "default.def" files
C     is as follows:
C
C     If the file is defined on the command line then
C       If a directory is specified then
C         Use the filename as is.
C       Else if the SYS$LOGIN or HOME variable is defined then
C         Use "SYS$LOGIN:filename", "$HOME/filename" or "%HOME%\filename".
C       Else
C         Use the filename as is (in current directory).
C     Else
C       If the CINCL variable is defined then
C         Use "CINCL:filename", "$CINCL/filename" or "%CINCL%\filename".
C       Else if the SYS$LOGIN or HOME variable is defined then
C         Use "SYS$LOGIN:filename", "$HOME/filename" or "%HOME%\filename".
C       Else
C         Use the filename as is (in current directory).
C
C Arguments:  NONE
C ==========
C_END_CCPFYP
C
C==============================================================================
C
C     .. Parameters ..
      INTEGER ILIMIT,ISTRLN,IENV
      PARAMETER (ILIMIT=150,ISTRLN=200,IENV=20)
C     ..
C     .. Local Scalars ..
      INTEGER HELP,IARG,ICOUNT,IEND,IERR,II,ILOOP,ISKIP,
     +        ISTART,IUNIT,LOOP,RDENVF,RDLOGF,IHELP,LREC,IFAIL
      LOGICAL DINIT,EINIT,VAX, MVS
      CHARACTER FILNAM* (ISTRLN), LINE* (ISTRLN),
     +     ENVFIL* (ISTRLN), LOGFIL* (ISTRLN), LOGNAM* (ISTRLN),
     +     TEMP* (ISTRLN), BKS*1
C     ..
C     .. Local Arrays ..
      CHARACTER ENAME(ILIMIT)* (IENV),ETYPE(ILIMIT)* (5),
     +          EXTN(ILIMIT)* (4)
C     ..
C     .. External Functions ..
      INTEGER LENSTR
C     don't declare iargc
      LOGICAL VAXVMS, WINMVS
      CHARACTER FEXTN* (ISTRLN), FDIR*(ISTRLN), RTNBKS*1
      EXTERNAL LENSTR,VAXVMS,FEXTN,WINMVS, RTNBKS
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPERR,CCPUPC,UGTARG,INITFYP,QPRINT,CSETNV,UGTENV
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ICHAR,INDEX
C     ..
C     .. Save statement ..
      SAVE
C     .. Data statements ..
      DATA ICOUNT/0/,IHELP/1/,DINIT/.TRUE./,EINIT/.TRUE./,
     +     RDLOGF/0/,RDENVF/0/,ILOOP/1/,IUNIT/31/,
     +     LOGFIL/'default.def'/,ENVFIL/'environ.def'/
C     ..
      VAX = VAXVMS()
      MVS = WINMVS()
      BKS = RTNBKS()
      CALL INITFYP
C
C
      CALL CCP4H_INIT_LIB()
C
      IARG = IARGC()
C
C---- Process command line option switches first
C
   10 CONTINUE
      ISKIP = 1
      IF (ILOOP.LE.IARG) THEN
        CALL UGTARG(ILOOP,LINE)
        CALL CCPUPC(LINE)
C
        IF (LINE(1:1).EQ.'-') THEN
          DO 20 II = 2,LENSTR(LINE)
            IF (LINE(II:II).EQ.'V' .OR. LINE(II:II).EQ.'H') THEN
              HELP = ILOOP + ISKIP
              IF (HELP.GT.IARG) THEN
                IHELP = 1
              ELSE
                CALL UGTARG(HELP,TEMP)
                IHELP = ICHAR(TEMP(1:1)) - ICHAR('0')
                IF (IHELP.LT.0 .OR. IHELP.GT.9) IHELP = 1
              END IF
              ISKIP = ISKIP + 1
            ELSE IF (LINE(II:II).EQ.'N') THEN
              DINIT = .FALSE.
              EINIT = .FALSE.
            ELSE IF (LINE(II:II).EQ.'D') THEN
              RDLOGF = ILOOP + ISKIP
              IF (RDLOGF.GT.IARG) CALL CCPERR(1,'Use: -d filename')
              CALL UGTARG(RDLOGF,LOGFIL)
              DINIT = .TRUE.
              ISKIP = ISKIP + 1
            ELSE IF (LINE(II:II).EQ.'E') THEN
              RDENVF = ILOOP + ISKIP
              IF (RDENVF.GT.IARG) CALL CCPERR(1,'Use: -e filename')
              CALL UGTARG(RDENVF,ENVFIL)
              EINIT = .TRUE.
              ISKIP = ISKIP + 1
            ELSE
              CALL QPRINT(1,'Ignoring switch '//LINE(II:II))
            END IF
   20     CONTINUE
          ILOOP = ILOOP + ISKIP
          GO TO 10
        END IF
      END IF
C
C---- Set up debug level
C
      CALL QPRINT(IHELP,' ')
C
C---- Update argument list
C
      IF (EINIT) THEN
        II = -1
        IF (RDENVF.GT.0) THEN
          IF (FDIR(ENVFIL).NE.' ') II=0
        ELSE
          CALL UGTENV('CINCL',FILNAM)
          IF (FILNAM.NE.' ') THEN
            IF (VAX) THEN
              FILNAM = 'CINCL:'
              II = LENSTR(FILNAM)
            ELSEIF (MVS) THEN
            	II = LENSTR(FILNAM)
              IF (FILNAM(II:II).NE.BKS) THEN
                II = II + 1
                IF (II.GT.ISTRLN) CALL CCPERR(1,
     +               'environ path name too long')
                FILNAM(II:II)=BKS
              ENDIF
            ELSE
              II = LENSTR(FILNAM)
              IF (FILNAM(II:II).NE.'/') THEN
                II = II + 1
                IF (II.GT.ISTRLN) CALL CCPERR(1,
     +               'environ path name too long')
                FILNAM(II:II)='/'
              ENDIF
            ENDIF
          ENDIF
        ENDIF
C
        IF (II.LT.0) THEN
          IF (VAX) THEN
            CALL UGTENV('SYS$LOGIN',FILNAM)
          ELSE
            CALL UGTENV('HOME',FILNAM)
          ENDIF
          IF (FILNAM.NE.' ') THEN
            IF (VAX) THEN
              FILNAM = 'SYS$LOGIN:'
              II = LENSTR(FILNAM)
            ELSEIF (MVS) THEN
              II = LENSTR(FILNAM)
              IF (FILNAM(II:II).NE.BKS) THEN
                II = II + 1
                IF (II.GT.ISTRLN) CALL CCPERR(1,
     +               'environ path name too long')
                FILNAM(II:II)=BKS
              ENDIF
            ELSE
              II = LENSTR(FILNAM)
              IF (FILNAM(II:II).NE.'/') THEN
                II = II + 1
                IF (II.GT.ISTRLN) CALL CCPERR(1,
     +               'environ path name too long')
                FILNAM(II:II)='/'
              ENDIF
            ENDIF
          ELSE
            II = 0
          ENDIF
        ENDIF
        FILNAM(II+1:) = ENVFIL
        IF (II.GT.ISTRLN) CALL CCPERR(1, 'environ path name too long')
        CALL QPRINT(2,'Opening file '//FILNAM)
        IFAIL=0
        IXUNIT = IUNIT
        IF (IHELP.LT.2) IXUNIT = -IUNIT
        CALL CCPDPN (IXUNIT,FILNAM,'READONLY','F',LREC,IFAIL)
   30   CONTINUE
        READ (UNIT=IUNIT,FMT=6000,END=40,ERR=80,IOSTAT=IERR) LINE
        TEMP = LINE
C       comments from `!' or `#' to end-of-line
        II = INDEX(LINE,'#')
        IF (II.NE.0) LINE(II:) = ' '
        II = INDEX(LINE,'!')
        IF (II.NE.0) LINE(II:) = ' '
        IF (LINE.NE.' ') THEN
          ICOUNT = ICOUNT + 1
          IF (ICOUNT.GT.ILIMIT) CALL CCPERR(1,
     +         'Too many logical names in environ file: '//TEMP)
          ISTART = INDEX(LINE,'=')
          IF (ISTART.EQ.0)
     +         CALL CCPERR(1,'Missing = in environ file: '//TEMP)
          ENAME(ICOUNT) = LINE(1:ISTART-1)
          EXTN(ICOUNT) = '.'//FEXTN(LINE)
          IF (EXTN(ICOUNT).EQ.'.')
     +         CALL CCPERR(1, 'Bad extension in environ file: '//TEMP)
          IEND = INDEX(LINE,EXTN(ICOUNT))
          ETYPE(ICOUNT) = LINE(ISTART+1:IEND-1)
        END IF
        GO TO 30
   40   CLOSE (UNIT=IUNIT)
      END IF
C
C---- Now get defaults file
C
      IF (DINIT) THEN
        II = -1
        IF (RDLOGF.GT.0) THEN
          IF (FDIR(LOGFIL).NE.' ') II=0
        ELSE
          CALL UGTENV('CINCL',FILNAM)
          IF (FILNAM.NE.' ') THEN
            IF (VAX) THEN
              FILNAM = 'CINCL:'
              II = LENSTR(FILNAM)
            ELSEIF (MVS) THEN
              II = LENSTR(FILNAM)
              IF (FILNAM(II:II).NE.BKS) THEN
                II = II + 1
                IF (II.GT.ISTRLN) CALL CCPERR(1,
     +               'default.def path name too long')
                FILNAM(II:II)=BKS
              ENDIF
            ELSE
              II = LENSTR(FILNAM)
              IF (FILNAM(II:II).NE.'/') THEN
                II = II + 1
                IF (II.GT.ISTRLN) CALL CCPERR(1,
     +               'default.def path name too long')
                FILNAM(II:II)='/'
              ENDIF
            ENDIF
          ENDIF
        ENDIF
C
        IF (II.LT.0) THEN
          IF (VAX) THEN
            CALL UGTENV('SYS$LOGIN',FILNAM)
          ELSE
            CALL UGTENV('HOME',FILNAM)
          ENDIF
          IF (FILNAM.NE.' ') THEN
            IF (VAX) THEN
              FILNAM = 'SYS$LOGIN:'
              II = LENSTR(FILNAM)
            ELSEIF (MVS) THEN
            	II = LENSTR(FILNAM)
              IF (FILNAM(II:II).NE.BKS) THEN
                II = II + 1
                IF (II.GT.ISTRLN) CALL CCPERR(1,
     +               'default.def path name too long')
                FILNAM(II:II)=BKS
              ENDIF
            ELSE
              II = LENSTR(FILNAM)
              IF (FILNAM(II:II).NE.'/') THEN
                II = II + 1
                IF (II.GT.ISTRLN) CALL CCPERR(1,
     +               'default.def path name too long')
                FILNAM(II:II)='/'
              ENDIF
            ENDIF
          ELSE
            II = 0
          ENDIF
        ENDIF
        IF (II.GT.ISTRLN) CALL CCPERR(1,
     +       'default.def path name too long')
        FILNAM(II+1:) = LOGFIL
        CALL QPRINT(2,'Opening file '//FILNAM)
        IFAIL=0
        IXUNIT = IUNIT
        IF (IHELP.LT.2) IXUNIT = -IUNIT
        CALL CCPDPN (IXUNIT,FILNAM,'READONLY','F',LREC,IFAIL)
   50   CONTINUE
        READ (UNIT=IUNIT,FMT=6000,END=60,ERR=80,IOSTAT=IERR) LINE
        TEMP = LINE
C       comments from `!' or `#' to end-of-line
        II = INDEX(LINE,'#')
        IF (II.NE.0) LINE(II:) = ' '
        II = INDEX(LINE,'!')
        IF (II.NE.0) LINE(II:) = ' '
        IF (LINE.NE.' ') THEN
          II = INDEX(LINE,'=')
          IF (II.EQ.0)
     +         CALL CCPERR(1,'Missing = in defaults file: '//TEMP)
          LOGNAM = LINE(1:II-1)
          FILNAM = LINE(II+1:)
C
C---- here skip = .true. in csetnv means don't override existing logical
C         name (defined in environment)
C
          CALL CSETNV(LOGNAM,FILNAM,ENAME,ETYPE,EXTN,ICOUNT,.TRUE.)
        END IF
        GO TO 50
   60   CLOSE (UNIT=IUNIT)
      END IF
C
C---- Loop through command line arguments
C
      CALL QPRINT(2,'Processing Command Line Arguments')
      DO 70 LOOP = ILOOP,IARG,2
        CALL UGTARG(LOOP,LOGNAM)
         CALL CCPUPC(LOGNAM)
        CALL UGTARG(LOOP+1,FILNAM)
        IF (FILNAM.EQ.' ') CALL CCPERR(1,
     +       'Use: <logical name> <filename> ...')
C
C---- here skip = .false. in csetnv means override logical name
C       defined in the environment
C
        CALL CSETNV(LOGNAM,FILNAM,ENAME,ETYPE,EXTN,ICOUNT,.FALSE.)
   70 CONTINUE
      CALL QPRINT(2,'End of pre-processing stage')
      RETURN
C
 80   CALL CCPERR (-1,'Error reading environ or default file')
C
 6000 FORMAT (A)
      END
C
C
C SUBROUTINE 'CCPGI2'
C ===================
C
C_BEGIN_CCPGI2
      SUBROUTINE CCPGI2(IVAL,IA,N)
C     ============================
C
C GET AN UNSIGNED INTEGER*2 VALUE FROM 0 TO 65535 FROM THE N'TH unsigned
C INTEGER*2 ELEMENT OF AN INTEGER (OR OTHER) ARRAY.
C
C (MUST BE IMPLEMENTED IF CCPBYT FUNCTION RETURNS .TRUE.)
C [added for LAUE]
C
C Arguments:
C ==========
C
C    IVAL (O)   INTEGER: THE RETURNED VALUE FROM 0 TO 65535
C      IA (I/O) INTEGER*2 ARRAY(*): FROM WHICH THE UNSIGNED INTEGER*2 VALUE
C               IS TO BE RETRIEVED
C       N (I)   INTEGER: POSITION IN 'IA' WHERE THE UNSIGNED INTEGER*2 VALUE
C               IS TO BE RETRIEVED
C_END_CCPGI2
C
C SPECIFICATION STATEMENTS
C ------------------------
C
      INTEGER*2 IA(*)
      INTEGER*2 JBYT(2)
      EQUIVALENCE (JA,JBYT(1))
      LOGICAL CALLED, LITEND
      EXTERNAL LITEND
      INTEGER IND
      SAVE CALLED, IND
      DATA CALLED/.FALSE./
C
      IF (.NOT.CALLED) THEN
        IF (LITEND(1)) THEN
          IND = 1
        ELSE
          IND = 2
        ENDIF
        CALLED=.TRUE.
      ENDIF
C
C GET UNSIGNED INTEGER*2
C ----------------------
C
      JA=0
      JBYT(IND)=IA(N)
      IVAL=JA
      END
C
C
C SUBROUTINE 'CCPGTB'
C ===================
C
C_BEGIN_CCPGTB
      SUBROUTINE CCPGTB(IVAL,IA,N)
C     ============================
C
C GET AN UNSIGNED BYTE VALUE FROM 0 TO 255 FROM THE N'TH BYTE OF AN INTEGER
C (OR OTHER) ARRAY.
C
C (MUST BE IMPLEMENTED IF CCPBYT FUNCTION RETURNS .TRUE.)
C [for LAUE]
C
C Arguments:
C ==========
C
C    IVAL (O)   INTEGER: THE RETURNED VALUE FROM 0 TO 255
C      IA (I/O) BYTE ARRAY(*): FROM WHICH THE BYTE VALUE IS TO BE RETRIEVED
C       N (I)   INTEGER: THE POSITION IN 'IA' WHERE THE BYTE VALUE IS
C               TO BE RETRIEVED
C_END_CCPGTB
C
C SPECIFICATION STATEMENTS
C ------------------------
C
      INTEGER*1 IA(*)
      INTEGER*1 JBYT(4)
      EQUIVALENCE (JA,JBYT(1))
      LOGICAL CALLED, LITEND
      EXTERNAL LITEND
      INTEGER IND
      SAVE CALLED, IND
      DATA CALLED/.FALSE./
C
      IF (.NOT.CALLED) THEN
        IF (LITEND(1)) THEN
          IND = 1
        ELSE
          IND = 4
        ENDIF
        CALLED=.TRUE.
      ENDIF
C
C GET BYTE
C --------
C
      JA=0
      JBYT(IND)=IA(N)
      IVAL=JA
      END
C
C
C SUBROUTINE 'CCPI2I'
C ===================
C_BEGIN_CCPI2I
      SUBROUTINE CCPI2I(IA,I2,NE,SIGNED,SWAPB)
C     ========================================
C
C Copy an array of INTEGER*2 elements into an integer array
C
C (Must be implemented if ccpbyt function returns .TRUE.)
C [for LAUE]
C
C Arguments:
C ==========
C
C      IA (O)   INTEGER Array(*): to return values
C      I2 (I)   INTEGER*2 Array(*): holding data (may be an INTEGER array for
C               example with data packed into adjacant INTEGER*2 elements
C      NE (I)   INTEGER: The number of elements to be copied
C  SIGNED (I)   LOGICAL: =.TRUE.  Copy as signed integer*2 values
C                        =.FALSE. Copy as unsigned integer*2 values
C   SWAPB (I)   LOGICAL: =.TRUE.  Swap bytes in the integer*2 elements
C                        =.FALSE. Do not swap bytes
C_END_CCPI2I
C
C====== Specification statements
C
      LOGICAL SIGNED, SWAPB
      INTEGER IA(*)
      INTEGER*2 I2(*)
      INTEGER*2 J2(2)
      INTEGER*2 IEIGHT, I255
      PARAMETER (I255=255)
      EQUIVALENCE (JA,J2(1))
      LOGICAL CALLED, LITEND
      EXTERNAL LITEND
      INTEGER IND
      SAVE CALLED, IND
      DATA CALLED/.FALSE./
C
      IF (.NOT.CALLED) THEN
        IF (LITEND(1)) THEN
          IND = 1
        ELSE
          IND = 2
        ENDIF
        CALLED=.TRUE.
      ENDIF
C
C====== Swap bytes if required
C
      IEIGHT = 8
      IF (SWAPB) THEN
         DO 10 I = 1,NE
            I2(I) = IOR(IAND(ISHFT(I2(I),-IEIGHT),I255),
     +              ISHFT(I2(I),IEIGHT))
10       CONTINUE
      END IF
C
C====== Copy data
C
      IF (SIGNED) THEN
         DO 20 I=1,NE
            IA(I) = I2(I)
20       CONTINUE
      ELSE
         JA=0
         DO 30 I=1,NE
         J2(IND)=I2(I)
         IA(I)=JA
30       CONTINUE
      END IF
      END
C
C
C
C SUBROUTINE 'CCPIBY'
C ===================
C
C_BEGIN_CCPIBY
      SUBROUTINE CCPIBY(IBYT,IA,NB)
C     =============================
C
C COPY AN ARRAY OF INTEGERS INTO AN ARRAY OF UNSIGNED (OR UNSIGNED) BYTES.
C NOTE: NO OVERFLOW CHECKING IS DONE.
C
C (MUST BE IMPLEMENTED IF CCPBYT FUNCTION RETURNS .TRUE.)
C [for LAUE]
C
C Arguments:
C ==========
C
C    IBYT (O)   BYTE ARRAY(*) RETURNING DATA (MAY BE AN INTEGER ARRAY
C               FOR EXAMPLE WITH DATA PACKED INTO ADJACENT BYTES)
C      IA (I)   INTEGER ARRAY(*): Input values
C      NB (I)   INTEGER: IF >0, THE NUMBER OF ELEMENTS TO BE COPIED TO
C                        UNSIGNED BYTES
C                        IF <0, -THE NUMBER OF ELEMENTS TO BE COPIED TO
C                        SIGNED BYTES
C_END_CCPIBY
C
C SPECIFICATION STATEMENTS
C ------------------------
C
      INTEGER IA(*)
      INTEGER*1 IBYT(*)
      INTEGER*1 JBYT(4)
      EQUIVALENCE (JA,JBYT(1))
      LOGICAL CALLED, LITEND
      EXTERNAL LITEND
      INTEGER IND
      SAVE CALLED, IND
      DATA CALLED/.FALSE./
C
      IF (.NOT.CALLED) THEN
        IF (LITEND(1)) THEN
          IND = 1
        ELSE
          IND = 4
        ENDIF
        CALLED=.TRUE.
      ENDIF
C
C COPY DATA
C ---------
C
      NE = NB
      IF (NE.GT.0) THEN
         DO 10 I=1,NE
         JA=IA(I)
         IBYT(I)=JBYT(IND)
10       CONTINUE
      ELSE
         NE = -NE
         DO 20 I=1,NE
         IBYT(I) = IA(I)
20       CONTINUE
      END IF
      END
C
C
C
C SUBROUTINE 'CCPII2'
C ===================
C
C_BEGIN_CCPII2
      SUBROUTINE CCPII2(I2,IA,NE,SIGNED,SWAPB)
C     ========================================
C
C Copy an array of integers into an array of INTEGER*2 elements.
C NOTE: No overflow checking is done.
C
C (Must be implemented if ccpbyt function returns .TRUE.)
C [for LAUE]
C
C Arguments:
C ==========
C
C      I2 (O)   INTEGER*2 ARRAY(*): returning data (may be an INTEGER array for
C               example with data packed into adjacent INTEGER*2 elements)
C      IA (I)   INTEGER ARRAY(*): holding input values
C      NE (I)   INTEGER: The number of elements to be copied
C  SIGNED (I)   LOGICAL: =.TRUE.  Copy as signed integer*2 values
C                        =.FALSE. Copy as unsigned integer*2 values
C   SWAPB (I)   LOGICAL: =.TRUE.  Swap bytes in the integer*2 elements
C                        =.FALSE. Do not swap bytes
C_END_CCPII2
C
C====== Specification statements
C
      LOGICAL SIGNED, SWAPB
      INTEGER IA(*)
      INTEGER*2 I2(*)
      INTEGER*2 J2(2)
      INTEGER*2 IEIGHT, I255
      PARAMETER (I255=255)
      EQUIVALENCE (JA,J2(1))
      LOGICAL CALLED, LITEND
      EXTERNAL LITEND
      INTEGER IND
      SAVE CALLED, IND
      DATA CALLED/.FALSE./
C
      IF (.NOT.CALLED) THEN
        IF (LITEND(1)) THEN
          IND = 1
        ELSE
          IND = 2
        ENDIF
        CALLED=.TRUE.
      ENDIF
C
C====== Copy data
C
      IEIGHT = 8
      IF (SIGNED) THEN
         DO 10 I=1,NE
            I2(I) = IA(I)
10       CONTINUE
      ELSE
         DO 20 I=1,NE
            JA=IA(I)
            I2(I)=J2(IND)
20       CONTINUE
      ENDIF
C
C====== Swap bytes if required
C
      IF (SWAPB) THEN
         DO 30 I = 1,NE
            I2(I) = IOR(IAND(ISHFT(I2(I),-IEIGHT),I255),
     +              ISHFT(I2(I),IEIGHT))
30       CONTINUE
      END IF
      END
C
C
C
C_BEGIN_CCPLWC
      SUBROUTINE CCPLWC(STRING)
C     =========================
C
C---- convert a text string to lower case in situ
C
C Arguments:
C ==========
C
C  STRING (I/O) CHARACTER*(*): string to convert
C_END_CCPLWC
C
C     .. Scalar Arguments ..
      CHARACTER STRING* (*)
C     ..
C     .. Local Scalars ..
      INTEGER K,L,LL
      CHARACTER LC*26,UC*26
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX,LEN
C     ..
C     .. Data statements ..
      DATA LC/'abcdefghijklmnopqrstuvwxyz'/
      DATA UC/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C     ..
C
      LL = LEN(STRING)
      IF (LL.GT.0) THEN
        DO 10 L = 1,LL
          K = INDEX(UC,STRING(L:L))
          IF (K.NE.0) STRING(L:L) = LC(K:K)
   10   CONTINUE
      END IF
C
      END
C
C
C
C_BEGIN_CCPMDE
      SUBROUTINE CCPMDE(MODE,NBYT)
C     ============================
C
C---- If byte handling is available (see ccpbyt) then this subroutine
C     returns the number of bytes per data item for the different modes
C     used, in particular, in the map handling subroutines.
C
C---- If byte handling is not available, then the number of words per
C     item is returned with zeros for the undefined items
C
C Arguments:
C ==========
C
C        MODE (I)   INTEGER:
C                   MODE = 0,   BYTES
C                        = 1,   SHORT (2 BYTE) INTEGERS
C                        = 2,   REAL/INTEGER (SINGLE WORD)
C                        = 3,   SHORT COMPLEX (2 * 2 BYTE INTEGERS)
C                        = 4,   COMPLEX (TWO WORDS)
C
C        NBYT (O)   INTEGER:
C                        > 0,   THE NUMBER OF BYTES FOR THE ITEM  IF
C                               CCPBYT RETURNS .TRUE. OR THE  NUMBER
C                               OF WORDS IF CCPBYT RETURNS .FALSE.
C                        = 0,   NO VALUE AVAILABLE FOR THIS MODE
C                        = -1,  INVALID MODE
C
C  TYPICAL VALUES:  1  2  4  4  8    IF BYTE HANDLING AVAILABLE WITH 4
C                                    BYTES/WORD
C                   0  0  1  0  2    IF BYTE HANDLING UNAVAILABLE
C_END_CCPMDE
C
C SPECIFICATION STATEMENTS
C ------------------------
C
C     .. Scalar Arguments ..
      INTEGER MODE,NBYT
C     ..
C     .. Local Arrays ..
      INTEGER MODES(0:4)
C     ..
C     .. Data statements ..
      DATA MODES/1,2,4,4,8/
C     ..
C
C---- Get number of bytes or words
C
      NBYT = -1
      IF (MODE.GE.0 .AND. MODE.LE.4) NBYT = MODES(MODE)
      END
C
C
C
C_BEGIN_CCPMVB
      SUBROUTINE CCPMVB(ARR1,I1,ARR2,I2,NTOMOV)
C     =========================================
C
C---- This subroutine moves bytes from one non-character array
C     to another. I must be implemented if ccpbyt returns .true.
C     but will otherwise be a dummy routine.
C
C Arguments:
C ==========
C
C        ARR1 (I/O) BYTE ARRAY(*): TO WHICH BYTES ARE TO BE COPIED
C          I1 (I)   INTEGER: THE START BYTE NUMBER IN ARR1 WHERE THE BYTES ARE
C                   TO BE COPIED
C        ARR2 (I)   BYTE ARRAY(*): FROM WHICH BYTES ARE TO BE COPIED
C          I2 (I)   THE START BYTE NUMBER IN ARR2 FROM WHICH THE BYTES
C                   ARE TO BE COPIED
C      NTOMOV (I)   INTEGER: THE NUMBER OF BYTES TO BE COPIED
C_END_CCPMVB
C
C     .. Scalar Arguments ..
      INTEGER I1,I2,NTOMOV
C     ..
C     .. Array Arguments ..
      INTEGER*1 ARR1(*),ARR2(*)
C     ..
C     .. Local Scalars ..
      INTEGER I,J,N
C     ..
      I = I1 - 1
      J = I2 - 1
      DO 10 N = 1,NTOMOV
        I = I + 1
        J = J + 1
        ARR1(I) = ARR2(J)
   10 CONTINUE
C
      END
C
C
C_BEGIN_CCPMVI
      SUBROUTINE CCPMVI (IARR1,IARR2,NUM)
C     =================================
C
C  This routine assigns the first NUM words of IARR2 to IARR1
C
C Arguments:
C ==========
C
C    IARR1 (O)   INTEGER ARRAY(*)
C    IARR2 (O)   INTEGER ARRAY(*)
C     NUM (I)   Number of words to copy
C_END_CCPMVI
C
C  Arguments
      INTEGER NUM
      REAL IARR1(*),IARR2(*)
C
      INTEGER J
C
      DO 10 J=1,NUM
   10 IARR1(J)=IARR2(J)
      END
C
C
C_BEGIN_CCPMVR
      SUBROUTINE CCPMVR (ARR1,ARR2,NUM)
C     =================================
C
C  This routine assigns the first NUM elements of ARR2 to ARR1
C
C Arguments:
C ==========
C
C    ARR1 (O)   REAL ARRAY(*)
C    ARR2 (O)   REAL ARRAY(*)
C     NUM (I)   Number of words to copy
C_END_CCPMVR
C
C  Arguments
      INTEGER NUM
      REAL ARR1(*),ARR2(*)
C
      INTEGER J
C
      DO 10 J=1,NUM
   10 ARR1(J)=ARR2(J)
      END
C
C_BEGIN_CCPNUN
      INTEGER FUNCTION CCPNUN ()
C     ==========================
C
C     Return (the next) unused (not connected) i/o unit number.
C     Use this to select an arbitrary unit for i/o to avoid clashes with
C     other code.  (The value returned will be the same until the unit in
C     question is opened or a lower-numbered one is closed.)
C
C_END_CCPNUN
      LOGICAL OD, EX
      EXTERNAL CCPERR
      INTEGER IOS
C     The `standard' unit 5 and 6 may or may not be reported as open,
C     normally depending on whether an appropriate read or write has
C     happened, so we'll start at 7.  Lower-numbered ones might be used
C     for other things such as standard error.  99 seems a reasonable
C     place to stop.
      DO 10 CCPNUN=7,99
        INQUIRE (UNIT=CCPNUN, OPENED=OD, IOSTAT=IOS, EXIST=EX)
        IF (EX .AND. (.NOT.OD) .AND. IOS.EQ.0) RETURN
 10   CONTINUE
      CALL CCPERR (1, 'CCPNUN: Can''t find an unused unit')
      END
C
C
C
C_BEGIN_CCPONL
      LOGICAL FUNCTION CCPONL(IDUM)
C     =============================
C
C---- This function determines whether a program is being run on-line
C     if this information is available
C
C Arguments:
C ==========
C
C        IDUM (D)   DUMMY
C
C RETURNS .TRUE.  IF PROGRAM IS BEING RUN ON-LINE
C RETURNS .FALSE. IF BATCH MODE OR STATUS UNKNOWN
C_END_CCPONL
C
C     .. Scalar Arguments ..
      INTEGER IDUM
C     ..
C     .. Local Scalars ..
      INTEGER IYES,ITERM
C     ..
C     .. External Functions ..
      EXTERNAL UISATT
C     ..
C
C      test for fortran unit=6 o/p
C
      IYES = 0
      ITERM = 6
      CALL UISATT(ITERM,IYES)
      CCPONL = IYES.EQ.1
      END
C
C
C
C SUBROUTINE 'CCPPSF'
C ===================
C
C_BEGIN_CCPPSF
      SUBROUTINE CCPPSF(FILNAM,PATH,NAME,TYPE,VERS)
C     =============================================
C
C PARSE FILE NAME INTO COMPONENTS
C
C NOTE: THE ROUTINE  CONTAINS MACHINE DEPENDENT CODE
C
C
C Arguments:
C ==========
C
C      FILNAM (I)   CHARACTER*(*): FILE NAME STRING (NO EMBEDDED BLANKS ASSUMED)
C
C        PATH (O)   CHARACTER*(*): STRING RETURNING PATH OR, FOR VAX VMS,
C                   THE PART OF THE FILE SPECIFICATION UP TO THE
C                   END OF THE DIRECTORY SPECIFICATION (BLANK IF NONE)
C                   (INCLUDES TERMINATING ] or : or /)
C
C        NAME (O)   CHARACTER*(*): STRING RETURNING NAME.  (BLANK IF NONE)
C
C        TYPE (O)   CHARACTER*(*): STRING RETURNING FILE TYPE/EXTENSION
C                   (BLANK IF NONE)
C
C        VERS (O)   CHARACTER*(*): STRING RETURNING THE VERSION.
C                   (BLANK IF NONE)
C
C AFTER REMOVAL OF THE PATH PART OF THE STRING, IF PRESENT, THE VERSION ON
C A VAX IS TAKEN AS ANY TEXT FOLLOWING A SEMICOLON IN THE STRING OR, IF NO
C SEMICOLON IS PRESENT, ANY TEXT FOLLOWING THE LAST DOT IN THE STRING
C PROVIDED THAT AT LEAST TWO DOTS ARE PRESENT. ON A UNIX SYSTEM THE VERSION
C WILL ALWAYS BE RETURNED AS A BLANK.
C
C AFTER THE REMOVAL OF THE PATH AND VERSION PARTS OF THE STRING THEN, IF
C THERE IS AT LEAST ONE DOT, THE NAME IS THE STRING UP TO THE LAST DOT
C REMAINING AND THE TYPE IS THE PART OF THE STRING AFTER THE DOT. IF
C NO DOT IS PRESENT THEN THE REMAINING STRING IS THE NAME AND THE TYPE
C IS BLANK.
C_END_CCPPSF
C
C SPECIFICATION STATEMENTS
C ------------------------
C
      CHARACTER*(*) FILNAM,PATH,NAME,TYPE,VERS
      EXTERNAL VAXVMS, WINMVS, RTNBKS
      LOGICAL VAXVMS, WINMVS, VMS, MVS
      CHARACTER RTNBKS*1, BKS*1
C
C INITIALISATIONS
C ---------------
C
      PATH=' '
      NAME=' '
      TYPE=' '
      VERS=' '
      LMAX=LENSTR(FILNAM)
      IF (LMAX.EQ.0) RETURN
      LMIN=0
      VMS = VAXVMS()
      MVS = WINMVS()
      BKS = RTNBKS()      
10    LMIN=LMIN+1
      IF (FILNAM(LMIN:LMIN).EQ.' ') GO TO 10
C
C GET PATH
C --------
C
      IF (VMS) THEN
        DO 20 L=LMAX,LMIN,-1
          IF (FILNAM(L:L).EQ.':'.OR.FILNAM(L:L).EQ.']') GO TO 30
 20     CONTINUE
      ELSEIF (MVS) THEN
        DO 21 L=LMAX,LMIN,-1
          IF (FILNAM(L:L).EQ.BKS)GO TO 30
 21     CONTINUE
      ELSE
        DO 22 L=LMAX,LMIN,-1
          IF (FILNAM(L:L).EQ.'/')GO TO 30
 22     CONTINUE
      ENDIF
      GO TO 40
30    PATH=FILNAM(LMIN:L)
      LMIN=L+1
      IF (LMIN.GT.LMAX) RETURN
C
C GET VERSION IF PRESENT
C ----------------------
C
 40   CONTINUE
      IF (VMS) THEN
        LSC=INDEX(FILNAM(LMIN:LMAX),';')
        IF (LSC.GT.0) THEN
          LSC=LSC+LMIN-1
          IF (LSC.LT.LMAX) VERS=FILNAM(LSC+1:LMAX)
          LMAX=LSC-1
        ELSE
          LDOT=0
          NDOT=0
          DO 50 L=LMAX,LMIN,-1
            IF (FILNAM(L:L).EQ.'.') THEN
              NDOT=NDOT+1
              IF (LDOT.EQ.0) LDOT=L
            ENDIF
 50       CONTINUE
          IF (NDOT.GT.1) THEN
            IF (LDOT.LT.LMAX) VERS=FILNAM(LDOT+1:LMAX)
            LMAX=LDOT-1
          ENDIF
        ENDIF
      ENDIF
C
C GET NAME AND TYPE
C -----------------
C
      IF (LMAX.LT.LMIN) RETURN
      LDOT=0
      DO 60 L=LMAX,LMIN,-1
      IF (FILNAM(L:L).EQ.'.') THEN
         LDOT=L
         GO TO 70
      ENDIF
60    CONTINUE
70    IF (LDOT.EQ.0) THEN
         NAME=FILNAM(LMIN:LMAX)
         RETURN
      ELSE
         IF (LDOT.GT.LMIN) NAME=FILNAM(LMIN:LDOT-1)
         IF (LDOT.LT.LMAX) TYPE=FILNAM(LDOT+1:LMAX)
      ENDIF
      END
C
C
C
C_BEGIN_CCPRCS
      SUBROUTINE CCPRCS(ILP,PROG,RCSDAT)
C     ==================================
C
C     Interface to CCPVRS using RCS-format date e.g.,
C     '1992/09/14 18:47:13' or its form expanded with RCS
C     option `-kv' as for a CVS export, in which case it will have the
C     `1992/09/14 18:47:13' stripped.
C
C Arguments:
C ==========
C
C         ILP (I)   INTEGER: UNIT NUMBER FOR PRINTER OUTPUT
C        PROG (I)   CHARACTER*(*): VARIABLE HOLDING PROGRAM NAME (MAX
C                   OF 10 CHARACTERS)
C      RCSDAT (I)   CHARACTER*(*): VARIABLE HOLDING DATE IN RCS FORMAT
C_END_CCPRCS
C
      CHARACTER*(*) RCSDAT, PROG
      CHARACTER*8 DATE
      INTEGER ILP
      EXTERNAL CCPVRS
C
      IF (RCSDAT(:7) .EQ. '$Date: ') THEN
C       raw form (not exported)
        DATE = '  /  /'
        DATE(1:2) = RCSDAT(16:17)
        DATE(4:5) = RCSDAT(13:14)
        DATE(7:8) = RCSDAT(10:11)
      ELSE IF (LEN(RCSDAT).GE.10 .AND. (RCSDAT(:2).EQ.'19'
     $                             .OR. RCSDAT(:2).EQ.'20')) THEN
C       after export
        DATE = '  /  /'
        DATE(1:2) = RCSDAT(9:10)
        DATE(4:5) = RCSDAT(6:7)
        DATE(7:8) = RCSDAT(3:4)
      ELSE
C       fallback
        DATE = ' '
      ENDIF
      CALL CCPVRS(ILP,PROG,DATE)
      END
C
C
C
C_BEGIN_CSETNV
      SUBROUTINE CSETNV(LNAME,FILNAM,ENAME,ETYPE,EXTN,ICOUNT,LSKIP)
C     =============================================================
C
C     Associate `logical name' LNAME with value FILNAM.  It is passed
C     arrays of (name, type, extension) for ICOUNT number of name lines
C     read from environ.def.  Doesn't re-define existing name if LSKIP is true.
C
C Arguments:
C ==========
C
C   LNAME (I)   CHARACTER*(*): Logical name (environment variable).
C
C  FILNAM (I/O) CHARACTER*(*): File name, if extension is omitted it is appended
C
C   ENAME (I/O) CHARACTER(150)*20 ARRAY: containing list of environment
C               variables; if LNAME is not in list it is appended
C               (also to ETYPE & EXTN arrays).
C
C   ETYPE (I,O) CHARACTER(150)*5 ARRAY: containing list of in/out types.
C
C    EXTN (I/O) CHARACTER(150)*4 ARRAY: containing list of extensions.
C
C  ICOUNT (I/O) INTEGER: Length of arrays ENAME, ETYPE & EXTN.
C
C   LSKIP (I)   LOGICAL: If .TRUE. existing name not re-defined.
C
C_END_CSETNV
C
C     .. Parameters ..
      INTEGER ILIMIT,ISTRLN,IENV
      PARAMETER (ILIMIT=150,ISTRLN=200,IENV=20)
C     ..
C     .. Scalar Arguments ..
      INTEGER ICOUNT
      CHARACTER LNAME* (*),FILNAM* (*)
      LOGICAL LSKIP
C     ..
C     .. Array Arguments ..
      CHARACTER ENAME(ILIMIT)* (IENV),ETYPE(ILIMIT)* (5),
     +          EXTN(ILIMIT)* (4)
C     ..
C     .. Local Scalars ..
      INTEGER I,II,ISTAT,JJ,PROCID
      LOGICAL VAX,MVS,EXIST
      CHARACTER ERRSTR* (ISTRLN),LIBFIL* (ISTRLN),PROGNM* (ISTRLN),
     +          TMPNAM* (ISTRLN),LINE* (ISTRLN),SCRFIL* (ISTRLN),
     +          BKS*(1)
C     ..
C     .. External Functions ..
      INTEGER GETPID,LENSTR
      LOGICAL VAXVMS, WINMVS
      CHARACTER FDIR* (ISTRLN),FEXTN* (ISTRLN),FROOT* (ISTRLN), RTNBKS
      EXTERNAL GETPID,LENSTR,VAXVMS,FDIR,FEXTN,FROOT
      INTEGER ifield(8)
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPERR,UGTARG,QPRINT,UGTENV,USTENV
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX
C     ..
      SAVE
      DATA PROGNM/' '/
C
C---- Check Logical Name does not already exist (unless processing
C     command line, in which case LSKIP will be true to override
C     environment)
C
      CALL UGTENV(LNAME,TMPNAM)
      IF (TMPNAM.NE.' ' .AND. LSKIP ) RETURN
      VAX = VAXVMS()
      MVS = WINMVS()
      BKS = RTNBKS()
C
C---- Get program name (argv[0]), but check if we have it already
C
      IF (PROGNM.EQ.' ') THEN
        CALL UGTARG(0,TMPNAM)
        PROGNM = FROOT(TMPNAM)
      ENDIF
C
C---- look through list for a match (possibly abbreviated) [is this
C     abbreviation possibility documented?]
C
      DO 10 JJ = 1,ICOUNT
        IF (ENAME(JJ).EQ.LNAME(1:LENSTR(ENAME(JJ)))) GO TO 20
   10 CONTINUE
C
C---- Unknown logical name add it to the list.
C
      TMPNAM = 'Non standard logical name '
      TMPNAM(27:) = LNAME
      CALL QPRINT(2,TMPNAM)
      ICOUNT = ICOUNT + 1
      IF (ICOUNT.GT.ILIMIT)
     +     CALL CCPERR(1,'Too many logical names')
      ENAME(ICOUNT) = LNAME
      ETYPE(ICOUNT) = 'undef'
      EXTN(ICOUNT) = FEXTN(FILNAM)
      JJ = ICOUNT
C
C---- Known logical name processing
C
   20 IF (FEXTN(FILNAM).EQ.' ') THEN
C
C---- Add extension
C
        IF (FILNAM.EQ.'/dev/null' .OR. FILNAM.EQ.'NL:') THEN
C          but not if FILNAM is /dev/null or NL:
          GOTO 333
        ELSE
          II = LENSTR(FILNAM) + 1
          FILNAM(II:) = EXTN(JJ)
        ENDIF
      ENDIF
      IF (FDIR(FILNAM).EQ.' ') THEN
CCC       this didn't agree with documentation:
CCC        IF (EXTN(JJ).EQ.'.lib' .OR. EXTN(JJ).EQ.'.prt' .OR.
CCC     +      EXTN(JJ).EQ.'.bes' .OR. EXTN(JJ).EQ.'.dic') THEN
        TMPNAM = FEXTN(FILNAM)
        IF (VAX) CALL CCPLWC(TMPNAM)
        IF (TMPNAM.EQ.'lib' .OR. TMPNAM.EQ.'prt' .OR.
     +      TMPNAM.EQ.'bes' .OR. TMPNAM.EQ.'dic') THEN
C         look for files without path but with standard extension in the
C         standard place
          CALL UGTENV('CLIBD',LIBFIL)
C         add the standard directory qualifier
          IF (VAX) THEN
C           fixme: should we insist that VMS defines CLIBD as well as un*x?
            IF (LIBFIL.NE.' ') THEN
              TMPNAM = 'CLIBD:'
              TMPNAM(7:) = FILNAM
            ELSE
              TMPNAM = FILNAM
            ENDIF
          ELSEIF (MVS) THEN
            IF (LIBFIL.EQ.' ') CALL CCPERR(1,'CLIBD not defined')
            II = LENSTR(LIBFIL)
            TMPNAM = LIBFIL(:II)//BKS
            II = II + 2
            TMPNAM(II:) = FILNAM
          ELSE
            IF (LIBFIL.EQ.' ') CALL CCPERR(1,'CLIBD not defined')
            II = LENSTR(LIBFIL)
            TMPNAM = LIBFIL(:II)//'/'
            II = II + 2
            TMPNAM(II:) = FILNAM
          END IF
          FILNAM = TMPNAM
        ELSE IF (EXTN(JJ).EQ.'.scr' .OR. FEXTN(FILNAM).EQ.'scr') THEN
C         scratch files in a special place
C         actually create <ccp4_scr>/<prognm>_.<pid>
          CALL UGTENV('CCP4_SCR',TMPNAM)
          IF (VAX) THEN
            IF (TMPNAM.EQ.' ') THEN
              TMPNAM = PROGNM
            ELSE
              TMPNAM = 'CCP4_SCR:' // PROGNM
            ENDIF
          ELSEIF (MVS) THEN
            IF (TMPNAM.EQ.' ') CALL CCPERR(1,'CCP4_SCR not defined')
            II = LENSTR(TMPNAM) + 1
            TMPNAM(II:) = BKS//PROGNM
          ELSE
            IF (TMPNAM.EQ.' ') CALL CCPERR(1,'CCP4_SCR not defined')
            II = LENSTR(TMPNAM) + 1
            TMPNAM(II:) = '/'//PROGNM
          END IF
          II = LENSTR(TMPNAM) + 1
          TMPNAM(II:II) = '_'
          II = II + 1
          I = INDEX(FILNAM,'.')
          TMPNAM(II:) = FILNAM(:I)
CHENN>
C          IF (VAX) THEN
C            WRITE (SCRFIL,'(Z8.8)') GETPID()
C          ELSE
C            PROCID = MOD(GETPID(),100000)
C            WRITE (SCRFIL,'(I5.5)') PROCID
C          ENDIF
           call date_and_time(VALUES=ifield)
            write (SCRFIL,'(I5)')ifield(8)*ifield(7)
CHENN<
          FILNAM = TMPNAM(1:LENSTR(TMPNAM))//SCRFIL
        END IF
      END IF
333   CONTINUE
C
C---- Now test input files do exist (but not for defaults, to avoid
C     checking 40 or 50 files listed in default.def which the setup
C     should guarantee)
C
      IF (ETYPE(JJ).EQ.'in' .AND. .NOT.LSKIP) THEN
        INQUIRE(FILE=FILNAM,EXIST=EXIST)
        IF (.NOT.EXIST) THEN
          ERRSTR = 'Cannot find file '
          ERRSTR(18:) = FILNAM
          CALL CCPERR (-1,ERRSTR)
        END IF
      END IF
      II = LENSTR(LNAME) + 1
      LINE = LNAME
      LINE(II:II) = '='
      II = II + 1
      LINE(II:) = FILNAM
C     =======================================
      CALL USTENV(LINE(1:LENSTR(LINE)),ISTAT)
C     =======================================
      IF (ISTAT.NE.0) THEN
        IF (VAX) THEN
          ERRSTR = 'Cannot create environment variable '
        ELSE
          ERRSTR = 'Cannot create logical name '
        ENDIF
        ERRSTR(36:) = LNAME
        CALL CCPERR (-1,ERRSTR)
      END IF
      CALL QPRINT(3,LINE)
      END
C
C
C
C_BEGIN_CCPPAG
      SUBROUTINE CCPPAG(IUN,NCOL,NLIN)
C     ================================
C
C---- This subroutine returns the number of columns and lines
C     for a printer output page on a given fortran unit number
C     if the information is available
C
C Arguments:
C ==========
C
C         IUN (I)   INTEGER: FORTRAN UNIT NUMBER
C        NCOL (O)   INTEGER: NUMBER OF COLUMNS IN THE PAGE
C        NLIN (O)   INTEGER: NUMBER OF LINES IN THE PAGE
C
C Return 80,132 unless a terminal whence 0,80
C_END_CCPPAG
C
C     .. Scalar Arguments ..
      INTEGER IUN,NCOL,NLIN
C     ..
C     .. Local Scalars ..
      INTEGER IYES
C     ..
C     .. External Subroutines ..
      EXTERNAL UISATT
C     ..
      CALL UISATT(IUN,IYES)
      IF (IYES.EQ.1) THEN
        NLIN = 0
        NCOL = 80
      ELSE
        NLIN = 80
        NCOL = 132
      END IF
      END
C
C
C
C SUBROUTINE 'CCPSI2'
C ===================
C
C_BEGIN_CCPSI2
      SUBROUTINE CCPSI2(IVAL,IA,N)
C     ============================
C
C SET AN INTEGER VALUE FROM 0 TO 65535 INTO THE N'TH UNSIGNED INTEGER*2 ELEMENT
C OF AN INTEGER (OR OTHER) ARRAY.
C NOTE: NO OVERFLOW CHECKING IS DONE.
C
C (MUST BE IMPLEMENTED IF CCPBYT FUNCTION RETURNS .TRUE.)
C [for LAUE]
C
C Arguments:
C ==========
C
C    IVAL (I)   INTEGER: VALUE FROM 0 TO 65535
C
C      IA (I/O) INTEGER*2 ARRAY: WHERE THE UNSIGNED INTEGER*2 VALUE IS TO BE
C               INSERTED
C
C       N (I)   INTEGER: THE POSITION IN 'IA' WHERE THE UNSIGNED INTEGER*2
C               VALUE IS TO BE INSERTED
C_END_CCPSI2
C
C SPECIFICATION STATEMENTS
C ------------------------
C
      INTEGER*2 IA(*)
      INTEGER*2 JBYT(2)
      EQUIVALENCE (JA,JBYT(1))
      LOGICAL CALLED, LITEND
      EXTERNAL LITEND
      INTEGER IND
      SAVE CALLED, IND
      DATA CALLED/.FALSE./
C
      IF (.NOT.CALLED) THEN
        IF (LITEND(1)) THEN
          IND = 1
        ELSE
          IND = 2
        ENDIF
        CALLED=.TRUE.
      ENDIF
C
C SET UNSIGNED INTEGER*2
C ----------------------
C
      JA=IVAL
      IA(N)=JBYT(IND)
      END
C
C
C
C SUBROUTINE 'CCPSTB'
C ===================
C
C_BEGIN_CCPSTB
      SUBROUTINE CCPSTB(IVAL,IA,N)
C     ============================
C
C SET AN INTEGER VALUE FROM 0 TO 255 INTO THE N'TH BYTE OF AN INTEGER
C (OR OTHER) ARRAY.
C NOTE: NO OVERFLOW CHECKING IS DONE.
C
C (MUST BE IMPLEMENTED IF CCPBYT FUNCTION RETURNS .TRUE.)
C [for LAUE]
C
C Arguments:
C ==========
C
C    IVAL (I)   INTEGER: VALUE FROM 0 TO 255
C      IA (I/O) BYTE ARRAY(*): WHERE THE BYTE VALUE IS TO BE INSERTED
C       N (I)   INTEGER: THE POSITION IN 'IA' WHERE THE BYTE VALUE IS TO
C               BE INSERTED
C_END_CCPSTB
C
C SPECIFICATION STATEMENTS
C ------------------------
C
      INTEGER*1 IA(*)
      INTEGER*1 JBYT(4)
      EQUIVALENCE (JA,JBYT(1))
      EQUIVALENCE (JA,JBYT(1))
      LOGICAL CALLED, LITEND
      EXTERNAL LITEND
      INTEGER IND
      SAVE CALLED, IND
      DATA CALLED/.FALSE./
C
      IF (.NOT.CALLED) THEN
        IF (LITEND(1)) THEN
          IND = 1
        ELSE
          IND = 4
        ENDIF
        CALLED=.TRUE.
      ENDIF
C
C SET BYTE
C --------
C
      JA=IVAL
      IA(N)=JBYT(IND)
      END
C
C
C
C_BEGIN_CCPSUM
      REAL FUNCTION CCPSUM(A,N,L)
C     ===========================
C
C---- This function sums the elements of an array. (for the cray this
C     function will call the cray 'ssum' function)
C
C Arguments:
C ==========
C
C           A (I)   REAL ARRAY(N): ARRAY TO BE SUMMED
C           N (I)   INTEGER: NO. OF ELEMENTS IN THE ARRAY
C           L (I)   INTEGER: SUM EVERY L'TH ELEMENT
C
C  CCPSUM RETURNS THE SUM
C_END_CCPSUM
C
C SPECIFICATION STATEMENTS AND CODE
C
C     .. Scalar Arguments ..
      INTEGER L,N
C     ..
C     .. Array Arguments ..
      REAL A(N)
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
      CCPSUM = 0.0
      DO 10 I = 1,N,L
        CCPSUM = A(I) + CCPSUM
   10 CONTINUE
      END
C
C
C
C_BEGIN_CCPTIM
      SUBROUTINE CCPTIM(IFLAG,CPU,ELAPS)
C     ==================================
C
C---- Return cpu time and elapsed time in seconds as
C     intervals from the initial call with iflag=0.  Note that there is
C     only one timer!
C
C Arguments:
C ==========
C
C       IFLAG (I/O) INTEGER: =0, initialise, =1, return times, =-1 dummy call
C                   returns -1 if time not available, in which case CPU
C                   and ELAPS are zero
C         CPU (O)   REAL: cpu time in seconds
C       ELAPS (O)   REAL: elapsed time in seconds
C_END_CCPTIM
C
C     .. Scalar Arguments ..
      REAL CPU,ELAPS
      INTEGER IFLAG
C     ..
C     .. Local Scalars ..
      INTEGER STIME, TIM0
C     ..
C     .. External Subroutines ..
      EXTERNAL UCPUTM,USTIME
C     ..
C     .. Save statement ..
      SAVE TIM0
C     ..
      IF (IFLAG.EQ.0) THEN
        ELAPS = 0.0
        CPU = 0.0
        CALL USTIME(TIM0)
        CALL UCPUTM(CPU)
      ELSE
        CALL USTIME(STIME)
        ELAPS = STIME - TIM0
        CPU = 1.0
        CALL UCPUTM(CPU)
      END IF
      END
C
C_BEGIN_CCPTOI
      SUBROUTINE CCPTOI(ARRAY,N,II,ITYP,IFAIL)
C     ========================================
C
C---- This subroutine converts the n'th byte or integer*2 element in a
C     non-character array to an integer value. it is used by the
C     map file handling routines and must be implemented if map modes
C     0,1,3 or 5 are to be used.
C
C Arguments:
C ==========
C
C       ARRAY (I)   REAL ARRAY(*): CONTAINING THE ELEMENTS TO BE CONVERTED
C
C           N (I)   INTEGER: THE NUMBER OF THE ELEMENT TO BE CONVERTED
C
C          II (O)   INTEGER: THE CALCULATED INTEGER VALUE (FOR BYTES THIS WILL
C                   BE IN THE RANGE 0-255)
C
C        ITYP (I)   INTEGER: THE CONVERSION TYPE =1, BYTE TO INTEGER
C                                                =2, INTEGER*2 TO INTEGER
C
C       IFAIL (I/O) INTEGER: ON INPUT   =0, STOP IF CONVERSION NOT AVAILABLE
C                                       =1, RETURN FROM SUBROUTINE ALWAYS
C                            ON OUTPUT  UNCHANGED IF CONVERSION CARRIED OUT
C                                       =-1 IF CONVERSION NOT AVAILABLE
C_END_CCPTOI
C
C     .. Scalar Arguments ..
      INTEGER IFAIL,II,ITYP,N
C     ..
C     .. Array Arguments ..
      REAL ARRAY(*)
C     ..
C     .. Local Scalars ..
      REAL RR
      INTEGER IA,NB,NIH,NW
C     ..
C     .. Local Arrays ..
      INTEGER*1 IBYT(4),JBYT(4)
      INTEGER*2 JHALF(2)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
C     .. Equivalences ..
      EQUIVALENCE (IA,IBYT(1))
      EQUIVALENCE (RR,JHALF(1),JBYT(1))
C     ..
      LOGICAL CALLED, LITEND
      INTEGER IND
      EXTERNAL LITEND, CCPERR
      SAVE CALLED, IND
      DATA CALLED/.FALSE./
C
      IF (.NOT.CALLED) THEN
        CALLED=.TRUE.
        IF (LITEND(1)) THEN
          IND = 1
        ELSE
          IND = 4
        ENDIF
      ENDIF
C
      GO TO (10,20) ITYP
C
C---- Byte to integer value
C
   10 NW = (N-1)/4 + 1
      NB = MOD(N-1,4) + 1
      IA = 0
      RR = ARRAY(NW)
      IBYT(IND) = JBYT(NB)
      II = IA
      IF (II.LT.0 .OR. II.GT.255) THEN
        IF (IFAIL .EQ. 0) THEN
          CALL CCPERR(1,' *** Error in CCPTOI, bad convertion ***') 
        ELSE
          IFAIL = -1
        ENDIF
      ENDIF
      RETURN
C
C---- Integer*2 to integer value
C
   20 NW = (N-1)/2 + 1
      NIH = MOD(N-1,2) + 1
      RR = ARRAY(NW)
      II = JHALF(NIH)
      IF (II.LT.0 .OR. II.GT.65535) THEN
        IF (IFAIL .EQ. 0) THEN
          CALL CCPERR(1,' *** Error in CCPTOI, bad convertion ***')
        ELSE
          IFAIL = -1
        ENDIF
      ENDIF
      END
C
C
C
C_BEGIN_CCPUFL
      SUBROUTINE CCPUFL
C     =================
C
C---- This subroutine is called to suppress underflow error messages
C     if required and if the facility is available.
C
C Arguments:  NONE
C ==========
C
C----  Not implemented.
C_END_CCPUFL
      END
C
C
C
C_BEGIN_CCPUPC
      SUBROUTINE CCPUPC(STRING)
C     =========================
C
C---- Convert a text string to upper case in situ
C
C Arguments:
C ==========
C
C      STRING (I/O) CHARACTER*(*): STRING TO BE CONVERTED
C_END_CCPUPC
C
C     .. Scalar Arguments ..
      CHARACTER STRING* (*)
C     ..
C     .. Local Scalars ..
      INTEGER K,L,LL
      CHARACTER LC*26,UC*26
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC INDEX,LEN
C     ..
C     .. Data statements ..
      DATA LC/'abcdefghijklmnopqrstuvwxyz'/
      DATA UC/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C     ..
      LL = LEN(STRING)
      IF (LL.GT.0) THEN
        DO 10 L = 1,LL
          K = INDEX(LC,STRING(L:L))
          IF (K.NE.0) STRING(L:L) = UC(K:K)
   10   CONTINUE
      END IF
      END
C
C
C
C_BEGIN_CCP4_VERSION
      SUBROUTINE CCP4_VERSION(VERSION)
C     =================================
C
C---- Return current CCP4 version as string
C
C Arguments:
C ==========
C
C       VERSION (O)   CHARACTER*(*): current version of CCP4 suite
C_END_CCP4_VERSION
C
C     .. Scalar Arguments ..
      CHARACTER*(*) VERSION

      VERSION = '4.0'

      END
C
C
C
C_BEGIN_CCPVRS
      SUBROUTINE CCPVRS(ILP,PROG,VDATE)
C     =================================
C
C---- Print program name and date of current version (also prints run
C     date if available)
C
C Arguments:
C ==========
C
C         ILP (I)   INTEGER: UNIT NUMBER FOR PRINTER OUTPUT
C        PROG (I)   CHARACTER*(*): VARIABLE HOLDING PROGRAM NAME (MAX
C                   OF 10 CHARACTERS)
C       VDATE (I)   CHARACTER*(*): VARIABLE HOLDING DATE OF THE CURRENT
C                   VERSION AS DD/MM/YY
C_END_CCPVRS
C     .. Scalar Arguments ..

      INTEGER ILP
      CHARACTER PROG* (*),VDATE* (*), PNM*(*)
C     ..
C     .. Local Scalars ..
      CHARACTER CTIME*8,DT2*8,DT*10,PR*20,UID*20,TMPPRG*20,VERSION*10
      SAVE PR
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      CHARACTER FROOT*20
      EXTERNAL LENSTR, FROOT
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPDAT,UGTUID,UTIME,ccp4h_summary_beg,ccp4h_summary_end
C     ..
      DATA PR /' '/
C
C---- Output heading
C
      call ccp4h_summary_beg()
      call ccp4h_pre_beg()
      PR = PROG
      DT = VDATE
      CALL CCPDAT(DT2)
      CALL UGTUID(UID)
      CALL UTIME(CTIME)
      CALL CCP4_VERSION(VERSION)
      WRITE (ILP,FMT=6000) PR,VERSION(1:LENSTR(VERSION)),
     +  DT,UID(1:LENSTR(UID)),DT2,CTIME
 6000 FORMAT (/,/,/,/,
     + '1##########################################################',/,
     + ' ##########################################################',/,
     + ' ##########################################################',/,
     + ' ### CCP PROGRAM SUITE: ',A10,2X,'VERSION ',A,': ',A8,'##',/,
     + ' ##########################################################',/,
     + ' User: ',A,'  Run date: ',A8,'  Run time:',A,
     + /,/,/,
     + ' Please reference: Collaborative Computational Project,',
     + ' Number 4. 1994.',/,' "The CCP4 Suite: Programs for Protein',
     + ' Crystallography". Acta Cryst. D50, 760-763.',/,/,
     + ' as well as any specific reference in the program write-up.',
     + /,/)
      call ccp4h_summary_end()
C
      RETURN
C
      ENTRY CCPPNM (PNM)
C_BEGIN_CCPPNM
C     SUBROUTINE CCPPNM (PNM)
C     =======================
C
C     Returns the program name previously set by CCPVRS (/CCPRCS); if
C     that isn't set, use arg(0).
C
C     Argument:
C         PNM (O)   CHARACTER*(*)  Program name
C_END_CCPPNM
      IF (PR.EQ.' ') THEN
        CALL UGTARG(0,TMPPRG)
        PR = FROOT(TMPPRG)
      END IF
      PNM = PR
      END
C
C
C
C_BEGIN_CCPZBI
      SUBROUTINE CCPZBI (ARR1,NUM)
C     ============================
C
C  This routine zeros NUM bytes of the array ARR1
C
C Arguments:
C
C    ARR1 (O)   BYTE ARRAY(*): array to be zeroed
C     NUM (I)   INTEGER: Number of bytes
C_END_CCPZBI
C
C  Arguments ......
      INTEGER NUM
      INTEGER*1 ARR1(*)
C
      INTEGER J
C
      DO 10 J=1,NUM
   10 ARR1(J)=0
      END
C
C
C_BEGIN_CCPZI
      SUBROUTINE CCPZI (IARR1,NUM)
C     ===========================
C
C  This routine assigns zero to IARR1 using NUM words
C
C Arguments:
C
C    IARR1 (O)   INTEGER ARRAY(*): array to be zeroed
C     NUM (I)   INTEGER: Number of words
C_END_CCPZI
C
C  Arguments ..........
      INTEGER NUM, IARR1(*)
C
      INTEGER J
C
      DO 10 J=1,NUM
   10 IARR1(J)=0
      END
C
C
C_BEGIN_CCPZR
      SUBROUTINE CCPZR (ARR1,NUM)
C     ===========================
C
C  This routine assigns zero to ARR1 using NUM words
C
C Arguments:
C
C    ARR1 (O)   REAL ARRAY(*): array to be zeroed
C     NUM (I)   INTEGER: Number of words
C_END_CCPZR
C
C  Arguments ..........
      INTEGER NUM
      REAL ARR1(*)
C
      INTEGER J
C
      DO 10 J=1,NUM
   10 ARR1(J)=0.0
      END
C
C
C     ===================================
C_BEGIN_FDIR
      FUNCTION FDIR(FILNAM)
      CHARACTER*(*) FDIR
C     ===================================
C
C---- Returns the path (directory) of a file name or ' '
C
C Arguments:
C
C  FILNAM (I)   CHARACTER*(*): File name
C_END_FDIR
      CHARACTER FILNAM* (*)
      CHARACTER*1 NAME, TYPE, VERS
      EXTERNAL CCPPSF
C
      CALL CCPPSF(FILNAM, FDIR, NAME, TYPE, VERS)
      END
C
C
C     ====================================
C_BEGIN_FEXTN
      FUNCTION FEXTN(FILNAM)
      CHARACTER*(*) FEXTN
C     ====================================
C
C---- Returns the extension of a file name or ' '
C
C Arguments:
C
C  FILNAM (I)   CHARACTER*(*): File name
C_END_FEXTN
      CHARACTER FILNAM* (*)
      CHARACTER*1 PATH, NAME, VERS
      EXTERNAL CCPPSF
C
      CALL CCPPSF(FILNAM, PATH, NAME, FEXTN, VERS)
      END
C
C
C     ====================================
C_BEGIN_FROOT
      FUNCTION FROOT(FILNAM)
      CHARACTER*(*) FROOT
C     ====================================
C
C---- Returns a file name minus an extension.
C
C Arguments:
C
C  FILNAM (I)   CHARACTER*(*): File name
C_END_FROOT
C
      CHARACTER FILNAM* (*)
      CHARACTER*1 PATH, TYPE, VERS
      EXTERNAL CCPPSF
C
      CALL CCPPSF(FILNAM, PATH, FROOT, TYPE, VERS)
      END
C
C
C
C_BEGIN_LITEND
         LOGICAL FUNCTION LITEND(IDUM)
C        =============================
C
C---- Check endedness, Returns TRUE if little endian (VAX, FX2800,
C                                                   Ultrix)
C                              FALSE if big endian (IBM,IRIS,ESV)
C
C Arguments:
C ==========
C
C    IDUM (D)   DUMMY
C_END_LITEND
C
         INTEGER I, IDUM
         INTEGER*1 B(4)
         EQUIVALENCE (I,B(1))
C
C---- Initialise B
C
          DO 10 JDO=1,4
            B(JDO) = 0
 10       CONTINUE
C
          I = 1
C
          IF (B(1) .NE. 0) THEN
              LITEND = .TRUE.
          ELSE
              LITEND = .FALSE.
          END IF
C
        END
C
C
C======================================================================
C
C_BEGIN_LENSTR
      INTEGER FUNCTION LENSTR(STRING)
C     ===============================
C
C---- Returns significant string length excluding trailing spaces
C
C Arguments:
C ==========
C
C  STRING (I)   CHARACTER*(*): Input string
C_END_LENSTR
C
C     .. Scalar Arguments ..
      CHARACTER STRING* (*)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC LEN
C     ..
      LENSTR = LEN(STRING)
 10   CONTINUE
      IF (LENSTR.NE.0) THEN
        IF(STRING(LENSTR:LENSTR).EQ.' ' .OR.
     .       ICHAR(STRING(LENSTR:LENSTR)).EQ.0) THEN
          LENSTR = LENSTR - 1
          GO TO 10
        END IF
      END IF
C
      END
C
C
C FUNCTION 'LUNSTI'
C =================
C
C_BEGIN_LUNSTI
      FUNCTION LUNSTI(IDUM)
C     =====================
C
C Returns the fortran standard input unit number
C
C Arguments:
C ==========
C
C       IDUM (D)   Dummy
C_END_LUNSTI
C
      LUNSTI = 5
      END
C
C
C FUNCTION 'LUNSTO'
C =================
C
C_BEGIN_LUNSTO
      FUNCTION LUNSTO(IDUM)
C     =====================
C
C Returns the fortran standard output unit number
C
C Arguments:
C ==========
C
C       IDUM (I)   Dummy argument
C_END_LUNSTO
C
      LUNSTO = 6
      END
C
C
C FUNCTION 'NBITST'
C =================
C
C_BEGIN_NBITST
      FUNCTION NBITST(IWORD,LSB,NBITS)
C     ================================
C
C Return the (unsigned) integer value held within a bit field in a word
C [for LAUE]
C
C Arguments:
C ==========
C
C      IWORD (I)    INTEGER: The word containing the bits to be examined
C
C        LSB (I)    INTEGER: The least significant bit offset for the bit field
C
C      NBITS (I)    INTEGER: The number of bits in the bit field (Must be less
C                   than the word length)
C_END_NBITST
C
C====== Get the bit value
C
      KMSK = 2**NBITS - 1
      NBITST = IAND(ISHFT(IWORD,-LSB),KMSK)
      END
C
C
C     =======================
C_BEGIN_NOCRLF
      SUBROUTINE NOCRLF(LINE)
C     =======================
C
C---- Output a line supressing cr/lf.
C
C Arguments:
C ==========
C
C    LINE (I)   CHARACTER*(*): Line to output.
C_END_NOCRLF
C
      EXTERNAL LUNSTO,TTSEND
      INTEGER LUNSTO
      CHARACTER*(*) LINE
      CALL TTSEND(LUNSTO(1),LINE,0)
      END
C
C
C======================================================================
C
C_BEGIN_QPRINT
      SUBROUTINE QPRINT(IFLAG,MSG)
C     ============================
C
C     QPRINT - Conditionally print information.
C
C     Normally, MSG is printed iff IFLAG is not greater than the
C     `reference' level for messages.  This reference level is set to
C     the value of IFLAG on the first call (which won't print anything).
C     The first call is typically from CCPFYP.
C
C Usage:  CALL QPRINT   (IFLAG,MSG)
C         INTEGER       IFLAG
C         CHARACTER*(*) MSG
C
C Input:  IFLAG         debug level (0-9)
C         MSG           the output message itself
C
C Output: None.
C_END_QPRINT
C======================================================================
C
      INTEGER IFLAG, LEVEL
      CHARACTER MSG* (*)
      EXTERNAL LUNSTO, LENSTR
      INTEGER PFLAG, LUNSTO, LENSTR, LL, LX, LS
      SAVE PFLAG
      DATA PFLAG /-1/
C
      IF (PFLAG.EQ.-1) THEN
        PFLAG = IFLAG
        RETURN
      END IF
      IF (IFLAG.LE.PFLAG) THEN
        LL = LENSTR (MSG)
        IF (LL.GE.132) THEN
C         break lines longer than 132 characters for VMS
          LX = 1
          LS = 131
 10       CONTINUE
          WRITE (LUNSTO(1),'(1X, A)') MSG(LX:LS)
          IF (LS.EQ.LL) GOTO 20
          LX = LS  + 1
          LS = LS + 130
          IF (LS.GT.LL) LS = LL
          GO TO 10
        ELSE
          IF (LL.EQ.0) THEN
            WRITE(LUNSTO(1),'()')
          ELSE
            WRITE (LUNSTO(1),'(1X, A)') MSG(1:LL)
          END IF
        END IF
 20     CONTINUE
      END IF
      RETURN
      
      ENTRY QPRLVL (LEVEL)
C_BEGIN_QPRLVL
C     SUBROUTINE QPRLVL(LEVEL)
C
C     Returns the current `debug level' used by QPRINT in the integer
C     output variable LEVEL.
C_END_QPRLVL
      LEVEL = PFLAG
      END
C
C
C SUBROUTINE 'STBITS'
C ===================
C
C_BEGIN_STBITS
      SUBROUTINE STBITS (IWORD,LSB,NBITS,IVAL)
C     ========================================
C Set a bit field within a word to a given (unsigned) integer value
C [for LAUE]
C
C Arguments:
C ==========
C
C      IWORD (I/O)  INTEGER: The word in which the bits are to be set
C
C        LSB (I)    INTEGER: The least significant bit offset for the bit field
C
C      NBITS (I)    INTEGER: The number of bits in the bit field (must be less
C                   than the word length)
C
C       IVAL (I)    INTEGER: The unsigned integer value to be set in the bit
C                   field (The user should ensure that this value will
C                   fit within the requested bit field)
C_END_STBITS
C
C====== Set the bits
C
      KMSK = 2**NBITS - 1
      KVAL = IVAL
      KMSK = ISHFT(KMSK,LSB)
      KMSK = NOT(KMSK)
      KVAL = ISHFT(KVAL,LSB)
      IWORD = IOR(IAND(IWORD,KMSK),KVAL)
      END
