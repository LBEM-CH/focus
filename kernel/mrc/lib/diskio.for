C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
C     $Id: diskio.f,v 1.23 1999/12/22 16:34:25 pjx Exp $
C     
C_BEGIN_CCPLIB
C A set of Fortran subroutines to perform random access I/O on various
C data items (including bytes). Uses the C functions fopen, fclose,
C fread, fwrite, fseek, ftell, etc - by calling routines in library.c
C Note: IUNIT is NOT A Fortran Unit number, but an internal identifier
C
C  The calls provided are given below:
C
C  CALL QOPEN   (IUNIT,FILNAM,ATBUTE)        - Open file
C [CALL QQOPEN  (IUNIT,FILNAM,ISTAT)         - Open file: use QOPEN]
C  CALL QCLOSE  (IUNIT)                      - Close file
C  CALL QMODE   (IUNIT,MODE,NMCITM)          - Change mode
C  CALL QREAD   (IUNIT,ARRAY,NITEMS,IER)     - Read nitems
C  CALL QREADI  (IUNIT,ARRAY,NITEMS,IER)     - Read nitems into integer array
C  CALL QREADR  (IUNIT,ARRAY,NITEMS,IER)     - Read nitems into real array
C  CALL QREADQ  (IUNIT,ARRAY,NITEMS,IER)     - Read nitems into complex array
C  CALL QREADC  (IUNIT,CHAR,IER)             - Read bytes into character var.
C  CALL QWRITE  (IUNIT,ARRAY,NITEMS)         - Write nitems
C  CALL QWRITI  (IUNIT,ARRAY,NITEMS)         - Write nitems from integer array
C  CALL QWRITI2 (IUNIT,ARRAY,NITEMS)         - Write nitems from integer*2 array
C  CALL QWRITR  (IUNIT,ARRAY,NITEMS)         - Write nitems from real array
C  CALL QWRITQ  (IUNIT,ARRAY,NITEMS)         - Write nitems from complex array
C  CALL QWRITC  (IUNIT,CHAR)                 - Write bytes from character var.
C  CALL QWRITB  (IUNIT,BYTE)                 - Write bytes from byte array
C  CALL QSEEK   (IUNIT,IREC,IEL,LRECL)       - Move to irec,iel
C  CALL QBACK   (IUNIT,LRECL)                - Backspace 1 record
C  CALL QSKIP   (IUNIT,LRECL)                - Skip 1 record
C  CALL QQINQ   (IUNIT,LFILNM,FILNAM,LENGTH) - Get filename and length
C  CALL QLOCATE (IUNIT,LOCATE)               - Get position in file
C  CALL QRARCH (IUNIT, IOFFSET)              - set up number conversion
C  CALL QWARCH (IUNIT, IOFFSET)              - write conversion info
C
C  QSEEK calculates the location as (IREC - 1)*LRECL + IEL. Note: as in
C        Fortran, addressing begins at 1 for both record & element
C        In these files, there are no true records: the use of "record length"
C        and "record number" in QSEEK, QSKIP, QBACK is purely notional.
C        For QSEEK, any combination of IREC, IEL & LRECL which gives the
C        same value of (IREC - 1)*LRECL + IEL is equivalent.
C
C  Where:
C
C  IUNIT  = Variable returned by (Q)QOPEN to identify a file stream
C
C  FILNAM = file name for the stream (should be restricted to eight
C           characters for CCP4 programs)
C
C  ATBUTE = File status for opening file
C         = 'UNKNOWN', 'SCRATCH', 'OLD', 'NEW', or 'READONLY'
C
C  ISTAT  = File status on opening the file:
C           1, 'UNKNOWN'   open as 'OLD'/'NEW' check existence
C           2, 'SCRATCH'   open as 'OLD' and delete on closing
C           3, 'OLD'       file MUST exist or program halts
C           4, 'NEW'       create (overwrite) new file
C           5, 'READONLY'  self explanatory
C
C  NOTE: When using QQOPEN or QOPEN with ATBUTE = 'NEW' [ISTAT = 4],
C        a check is made on the environment variable CCP4_OPEN - 
C        if this is set to UNKNOWN then the file is opened with 
C        attribute UNKNOWN rather than NEW to allow overwriting files
C        that already exist.
C
C  MODE   = Access mode = 0, BYTES
C                       = 1, SHORT INT
C                       = 2, (REAL) WORD
C                       = 3, SHORT COMPLEX
C                       = 4, COMPLEX
C                       = 6, INTEGER
C
C  NMCITM = No. of machine items (eg bytes) per element
C  ARRAY  = Starting location for data storage in core
C     NOTE: This should normally be an array of full-word fortran items
C     (REAL or INTEGER) or double-word (COMPLEX) in the case that you
C     want to transfer complex numbers (mode 4).  If necessary, unpack
C     bytes using the routines provided in the library (or new ones).
C     In particular, DON'T try to use BYTE or INTEGER*2 arrays, as these
C     will likely cause alignment errors on RISC architectures.
C  CHAR   = CHARACTER*n buffer for transfer
C  NITEMS = Number of elements to transfer
C  IER    = Error flag (0 = no error) else number of words transferred
C  IREC   = Desired record number (starts at 1)
C  IEL    = Desired element number within record (word) (starts at 1)
C  LRECL  = Record length in elements
C
C  No. of channels and buffer length in words set in #DEFINE statements
C
C NOTE: use of QREAD/QWRITE is deprecated -- use QREAD<a>/QWRITE<a>
C with a buffer of the correct type.
C
C
C     Author: David Agard (Phil Evans and John Campbell)
C     Modified: For Unix/F77 using words (and bytes if available) (John Campbell)
C     Modified: For ccp ascii header system implemented (Jan Zelinka)
C_END_CCPLIB
C
C======================================================================
C_BEGIN_QQOPEN
C
C QQOPEN - Open a file unit
C
C    NOTE: the routine QOPEN (which calls QQOPEN) is to be preferred
C          to calling QQOPEN directly
C
C Usage:  CALL QQOPEN  (IUNIT, LOGNAME, ISTAT)
C         INTEGER       IUNIT, ISTAT
C         CHARACTER*(*) LOGNAME
C
C Input:  LOGNAME       Logical name of file to open
C         ISTAT         File status: 1 (UNKNOWN), 2 (SCRATCH), 3 (OLD),
C                                    4 (NEW) or 5 (READONLY)
C
C Output: IUNIT         Integer handle assigned to file. If negative
C                       the following error conditions occurred:
C                       -1 No more streams left
C                       -2 Could not open the file
C
C_END_QQOPEN
C======================================================================
      SUBROUTINE QQOPEN(IUNIT,LOGNAM,ISTAT)
C     =====================================
C
C     .. Parameters ..
      INTEGER ISTRLN,ISIZE
      PARAMETER (ISTRLN=500,ISIZE=20)
C     ..
C     .. Scalar Arguments ..
      INTEGER ISTAT,IUNIT
      CHARACTER LOGNAM* (*)
C     ..
C     .. Local Arrays ..
      CHARACTER MODES(5)*10
C     ..
C     .. Local Scalars ..
      INTEGER JSTAT
      CHARACTER ERRSTR*255,REWRIT* (ISIZE),USRNAM* (ISIZE),
     +     FNAME* (ISTRLN),LNAME* (ISTRLN)
      LOGICAL LNONAM
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPERR,CCPUPC,COPEN,QPRINT,UGTENV,UGTUID
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      LOGICAL CCPEXS
      EXTERNAL CCPEXS, LENSTR
C     ..
C     .. Data statements ..
      DATA MODES/'UNKNOWN','SCRATCH','OLD','NEW','READONLY'/
C     ..
C
      IF (ISTAT.LT.1 .OR. ISTAT.GT.5) THEN
        WRITE (ERRSTR,'(1X,A,I2)') ' (Q)QOPEN: bad mode: ',ISTAT
        CALL CCPERR(1,ERRSTR)
      END IF
C
C---- Test CCP4_OPEN for 'UNKNOWN' to switch mode 4 to 1
C
      JSTAT = ISTAT
      REWRIT = ' '
      IF (JSTAT.EQ.4) THEN
        CALL UGTENV('CCP4_OPEN',REWRIT)
        CALL CCPUPC(REWRIT)
        IF (REWRIT.EQ.'UNKNOWN') JSTAT = 1
      END IF
C
C---- Check Logical Names
C
      FNAME = ' '
      LNAME = LOGNAM
      LNONAM = .FALSE.
      IF (LNAME.EQ.' ') LNAME = 'diskio.dft'
      CALL UGTENV(LNAME,FNAME)
      IF (FNAME.EQ.'/dev/null') THEN
        JSTAT = 1
      ELSE IF (FNAME.EQ.' ') THEN
        IF (.NOT. CCPEXS(LNAME)) LNONAM = .TRUE.
        FNAME = LNAME
      END IF
      IF (REWRIT.EQ.'UNKNOWN') 
     +     CALL QPRINT(2, '(Q)QOPEN status changed from NEW to '
     +     //'UNKNOWN for '// LNAME)
      IF (JSTAT.EQ.4 .AND. CCPEXS(FNAME)) THEN
        ERRSTR = ' (Q)QOPEN NEW file already exists: '
        ERRSTR(LENSTR(ERRSTR)+2:) = FNAME
        CALL CCPERR(1,ERRSTR)
      ENDIF
C
C---- Open the file as requested
C
      CALL COPEN(IUNIT,FNAME,JSTAT)
C
C---- Error conditions
C
      IF (IUNIT.EQ.-1) THEN
        CALL CCPERR(1,' (Q)QOPEN failed - no streams left')
      ELSE IF (IUNIT.EQ.-2) THEN
        IF (LNONAM) THEN
          ERRSTR = '(Q)QOPEN Logical name '//LNAME
          ERRSTR(LENSTR(ERRSTR)+2:) = 'has no associated file name'
          CALL CCPERR(2,ERRSTR)
        END IF
        ERRSTR = ' (Q)QOPEN failed - File name: '
        ERRSTR(LENSTR(ERRSTR)+2:) = LOGNAM
        CALL CCPERR (-1,ERRSTR)
      END IF

      call ccp4h_summary_beg()
      CALL UGTUID(USRNAM)
      WRITE (ERRSTR,'(1X,A,I2)') '(Q)QOPEN allocated # ',IUNIT
      CALL QPRINT(1,ERRSTR)
      ERRSTR = 'User:   '//USRNAM//' Logical Name: '//LNAME
      CALL QPRINT(1,ERRSTR)
      ERRSTR = 'Status: '//MODES(JSTAT)//' Filename: '//FNAME
      CALL QPRINT(1,ERRSTR)
      call ccp4h_summary_end()
      END
C
C
C======================================================================
C_BEGIN_QCLOSE
C
C QCLOSE - Close file unit
C
C Usage:  CALL QCLOSE (IUNIT)
C         INTEGER      IUNIT
C
C Input:  IUNIT        unit number assigned to file
C
C Output: None.
C_END_QCLOSE
C See library.c
C======================================================================
C_BEGIN_QMODE
C
C QMODE - Set mode for file access
C
C Usage:  CALL QMODE (IUNIT, MODE, NMCITM)
C         INTEGER     IUNIT, MODE, NMCITM
C
C Input:  IUNIT       unit number to assign to file
C         MODE        mode to switch into: 0 (BYTES), 1 (SMALL INTEGER),
C                                          2 (WORDS), 3 (SHORT COMPLEX),
C                                          4 (COMPLEX) 6 (INTEGER)
C
C Output: NMCITM      number of bytes per item on this machine.
C_END_QMODE
C See library.c
C======================================================================
C_BEGIN_QREAD
C
C QREAD - Read from IUNIT into BUFFER, NITEMS items
C
C Usage:  CALL QREAD (IUNIT,BUFFER,NITEMS,RESULT)
C         INTEGER     IUNIT, NITEMS, RESULT
C         REAL        BUFFER
C
C Input:  IUNIT       unit number assigned to file
C         NITEMS      number of items (item size set by QMODE)
C
C Output: RESULT      0 (no error), -1 (EOF) or number of items read
C         BUFFER      holds the items read
C_END_QREAD
C See library.c
C_BEGIN_QREADI
C
C QREADI - Read from IUNIT into BUFFER, NITEMS items
C
C Usage:  CALL QREADI (IUNIT,BUFFER,NITEMS,RESULT)
C         INTEGER     IUNIT, NITEMS, RESULT
C         INTEGER     BUFFER
C
C Input:  IUNIT       unit number assigned to file
C         NITEMS      number of items (item size set by QMODE)
C
C Output: RESULT      0 (no error), -1 (EOF) or number of items read
C         BUFFER      holds the items read
C_END_QREADI
C_BEGIN_QREADR
C
C QREADR - Read from IUNIT into BUFFER, NITEMS items
C
C Usage:  CALL QREADR (IUNIT,BUFFER,NITEMS,RESULT)
C         INTEGER     IUNIT, NITEMS, RESULT
C         REAL        BUFFER
C
C Input:  IUNIT       unit number assigned to file
C         NITEMS      number of items (item size set by QMODE)
C
C Output: RESULT      0 (no error), -1 (EOF) or number of items read
C         BUFFER      holds the items read
C_END_QREADR
C_BEGIN_QREADQ
C
C QREADQ - Read from IUNIT into BUFFER, NITEMS items
C
C Usage:  CALL QREADQ (IUNIT,BUFFER,NITEMS,RESULT)
C         INTEGER     IUNIT, NITEMS, RESULT
C         COMPLEX     BUFFER
C
C Input:  IUNIT       unit number assigned to file
C         NITEMS      number of items (item size set by QMODE)
C
C Output: RESULT      0 (no error), -1 (EOF) or number of items read
C         BUFFER      holds the items read
C_END_QREADQ
C_BEGIN_QREADC
C
C QREADC - Read bytes from IUNIT to fill BUFFER
C
C Usage:  CALL QREADC (IUNIT,BUFFER,RESULT)
C         INTEGER     IUNIT, RESULT
C         CHARACTER*(*) BUFFER
C
C Input:  IUNIT       unit number assigned to file
C
C Output: RESULT      0 (no error), -1 (EOF) or number of items read
C         BUFFER      holds the items read.  If necessary, use a substring
C                     of the CHARACTER variable
C_END_QREADC
C======================================================================
C_BEGIN_QWRITE
C
C QWRITE - Write to IUNIT from BUFFER, NITEMS items
C
C Usage:  CALL QWRITE (IUNIT,BUFFER,NITEMS)
C         INTEGER      IUNIT, NITEMS
C         REAL         BUFFER
C
C Input:  IUNIT        unit number assigned to file
C         NITEMS       number of items (item size set by QMODE)
C         BUFFER       holds the items to write
C
C Output: None.
C_END_QWRITE
C_BEGIN_QWRITI
C
C QWRITI - Write to IUNIT from BUFFER, NITEMS items
C
C Usage:  CALL QWRITI (IUNIT,BUFFER,NITEMS)
C         INTEGER      IUNIT, NITEMS
C         INTEGER      BUFFER
C
C Input:  IUNIT        unit number assigned to file
C         NITEMS       number of items (item size set by QMODE)
C         BUFFER       holds the items to write
C
C Output: None.
C_END_QWRITI
C_BEGIN_QWRITI2
C
C QWRITI - Write to IUNIT from BUFFER, NITEMS items
C
C Usage:  CALL QWRITI (IUNIT,BUFFER,NITEMS)
C         INTEGER      IUNIT, NITEMS
C         INTEGER*2    BUFFER
C
C Input:  IUNIT        unit number assigned to file
C         NITEMS       number of items (item size set by QMODE)
C         BUFFER       holds the items to write
C
C Output: None.
C_END_QWRITI2
C_BEGIN_QWRITR
C
C QWRITR - Write to IUNIT from BUFFER, NITEMS items
C
C Usage:  CALL QWRITR (IUNIT,BUFFER,NITEMS)
C         INTEGER      IUNIT, NITEMS
C         REAL         BUFFER
C
C Input:  IUNIT        unit number assigned to file
C         NITEMS       number of items (item size set by QMODE)
C         BUFFER       holds the items to write
C
C Output: None.
C_END_QWRITR
C_BEGIN_QWRITQ
C
C QWRITQ - Write to IUNIT from BUFFER, NITEMS items
C
C Usage:  CALL QWRITQ (IUNIT,BUFFER,NITEMS)
C         INTEGER      IUNIT, NITEMS
C         COMPLEX      BUFFER
C
C Input:  IUNIT        unit number assigned to file
C         NITEMS       number of items (item size set by QMODE)
C         BUFFER       holds the items to write
C
C Output: None.
C_END_QWRITQ
C_BEGIN_QWRITC
C
C QWRITC - Write BUFFER to IUNIT
C
C Usage:  CALL QWRITEC (IUNIT,BUFFER)
C         INTEGER      IUNIT, NITEMS
C         CHARACTER*(*) BUFFER
C
C Input:  IUNIT        unit number assigned to file
C         BUFFER       holds the items to write.  If necessary, use a
C                      substring of the CHARACTER variable
C
C Output: None.
C_END_QWRITC
C_BEGIN_QWRITB
C
C QWRITEB - Write BUFFER to IUNIT
C
C Usage:  CALL QWRITEB (IUNIT,BUFFER)
C         INTEGER      IUNIT, NITEMS
C         BYTE*(*) BUFFER
C
C Input:  IUNIT        unit number assigned to file
C         BUFFER       holds the items to write.  If necessary, use a
C                      substring of the CHARACTER variable
C
C Output: None.
C_END_QWRITB
C See library.c
C======================================================================
C_BEGIN_QSEEK
C
C QSEEK - Position a file pointer in a IUNIT
C
C Usage:  CALL QSEEK (IUNIT, IRECL, IEL, LRECL)
C         INTEGER     IUNIT, IRECL, IEL, LRECL
C
C Input:  IUNIT       unit number to assign to file
C         IRECL       "record number" to seek
C         IEL         element number to seek
C         LRECL       length of a "record"
C
C Output: None
C
C  QSEEK calculates the location as (IREC - 1)*LRECL + IEL. Note: as in
C        Fortran, addressing begins at 1 for both record & element
C        In these files, there are no true records: the use of "record length"
C        and "record number" in QSEEK, QSKIP, QBACK is purely notional.
C        For QSEEK, any combination of IREC, IEL & LRECL which gives the
C        same value of (IREC - 1)*LRECL + IEL is equivalent.
C
C_END_QSEEK
C See library.c
C======================================================================
C_BEGIN_QBACK
C
C QBACK - skip back 1 record of length LRECL
C
C Usage:  CALL QBACK (IUNIT,LRECL)
C         INTEGER     IUNIT, LRECL
C
C Input:  IUNIT       unit number assigned to file
C         LRECL       length of a record in items
C
C Output: None
C_END_QBACK
C See library.c
C======================================================================
C_BEGIN_QSKIP
C
C QSKIP - skip forward 1 record of length LRECL
C
C Usage:  CALL QSKIP (IUNIT,LRECL)
C         INTEGER     IUNIT, LRECL
C
C Input:  IUNIT       unit number assigned to file
C         LRECL       length of a record in items
C
C Output: None
C_END_QSKIP
C See library.c
C
C
C======================================================================
C_BEGIN_QQINQ
C
C QQINQ - check file name and size. Check IUNIT first, if no success
C         then try LOGNAM, if this fails use LOGNAM as filename.
C
C Usage:  CALL QQINQ   (IUNIT,LOGNAM,FILNAM,LENGTH)
C         INTEGER       IUNIT,LENGTH
C         CHARACTER*(*) LOGNAM,FILNAM
C
C Input:  IUNIT         handle to check (as returned by QOPEN)
C         LOGNAM        Logical name
C
C Output: FILNAM        the full file name or "" if no file
C         LENGTH        file size or -1 if no file
C
C_END_QQINQ
C======================================================================
C
      SUBROUTINE QQINQ(IUNIT,LFN,FILNAM,LENGTH)
C     =========================================
C
C     .. Parameters ..
      INTEGER ISTRLN
      PARAMETER (ISTRLN=500)
C     ..
C     .. Scalar Arguments ..
      INTEGER IUNIT,LENGTH
      CHARACTER FILNAM* (*),LFN* (*)
C     ..
C     .. Local Scalars ..
      CHARACTER FNAME* (ISTRLN),LNAME* (ISTRLN)
C     ..
C     .. External Subroutines ..
      EXTERNAL CQINQ,UGTENV
C     ..
      FNAME = ' '
      LNAME = LFN
      IF (LNAME.EQ.' ') LNAME = 'diskio.dft'
      CALL UGTENV(LNAME,FNAME)
      IF (FNAME.EQ.' ') FNAME = LNAME
      CALL CQINQ(IUNIT,FNAME,LENGTH)
      FILNAM = FNAME
C
      END
C
C
C======================================================================
C_BEGIN_QLOCATE
C
C QLOCATE - return current position in file (measured in items)
C
C Usage:  CALL QLOCATE (IUNIT,LOCATE)
C         INTEGER       IUNIT,LOCATE
C
C Input:  IUNIT         stream to check
C
C Output: LOCATE        Current position in file or -1 for no file
C_END_QLOCATE
C See library.c
C
C======================================================================
C_BEGIN_QOPEN
C
C QOPEN - Open a file unit
C
C Usage:  CALL QOPEN   (IUNIT, LOGNAME, ATBUTE)
C         INTEGER       IUNIT
C         CHARACTER*(*) LOGNAME, ATBUTE
C
C Input:  IUNIT         unit number number to assign to file
C         LOGNAME       Logical name of file to open
C         ATBUTE        File status = 'UNKNOWN', 'SCRATCH', 'OLD',
C                                     'NEW', or 'READONLY'
C
C Output: None.
C_END_QOPEN
C
C======================================================================
C
      SUBROUTINE QOPEN(IUNIT,LOGNAM,ATBUTA)
C     =====================================
C
C     .. Scalar Arguments ..
      INTEGER IUNIT
      CHARACTER ATBUTA* (*),LOGNAM* (*)
C     ..
C     .. Local Scalars ..
      INTEGER ISTAT
      CHARACTER FOO*80
C     ..
C     .. External Subroutines ..
      EXTERNAL QQOPEN, CCPUPC
C     ..
      ISTAT = 0
      CALL CCPUPC(ATBUTA)
      IF (ATBUTA(:1).EQ.'U') ISTAT = 1
      IF (ATBUTA(:1).EQ.'S') ISTAT = 2
      IF (ATBUTA(:1).EQ.'O') ISTAT = 3
      IF (ATBUTA(:1).EQ.'N') ISTAT = 4
      IF (ATBUTA(:1).EQ.'R') ISTAT = 5
      IF (ISTAT.EQ.0) THEN
        FOO = ATBUTA
        CALL CCPERR(1,'Bad attribute in QOPEN: '//FOO)
      ENDIF
C
      CALL QQOPEN(IUNIT,LOGNAM,ISTAT)
      END
C======================================================================
C_BEGIN_QRARCH
C
C QRARCH - set up number conversion
C
C Usage:  CALL QRARCH   (IUNIT, IOFFSET, IRESLT)
C         INTEGER       IUNIT, IOFFSET, IRESLT
C
C Input:  IUNIT         unit number number to assign to file
C         IOFFSET       offset in words at which to find architecture
C                       information
C
C Output: IRESLT        fileFT + (16*fileIT) (see library C code)
C                       Zero if the stamp isn't present.
C
C     Reads the `machine stamp' giving information about the
C     architecture with which the file was written and arranges to
C     translate a foreign format to native with QREAD, dependent on the
C     current diskio mode.
C
C_END_QRARCH
C======================================================================
C
C======================================================================
C_BEGIN_QWARCH
C
C QWARCH - set up number conversion
C
C Usage:  CALL QWARCH   (IUNIT, IOFFSET)
C         INTEGER       IUNIT, IOFFSET
C
C Input:  IUNIT         unit number number to assign to file
C         IOFFSET       offset in words at which to write architecture
C                       information
C
C Output: None.
C
C     Writes the `machine stamp' giving information about the
C     architecture with which the file was written and which is used by
C     QRARCH
C
C_END_QWARCH
C======================================================================

C     for correct typing of qread/write calls
      SUBROUTINE QREADI (IUNIT,BUFFER,NITEMS,RESULT)
      INTEGER IUNIT, NITEMS, RESULT
      INTEGER BUFFER(*)
      CALL QREAD (IUNIT,BUFFER,NITEMS,RESULT)
      END

      SUBROUTINE QREADQ (IUNIT,BUFFER,NITEMS,RESULT)
      INTEGER IUNIT, NITEMS, RESULT
      COMPLEX BUFFER(*)
      CALL QREAD (IUNIT,BUFFER,NITEMS,RESULT)
      END

      SUBROUTINE QREADR (IUNIT,BUFFER,NITEMS,RESULT)
      INTEGER IUNIT, NITEMS, RESULT
      REAL BUFFER(*)
      CALL QREAD (IUNIT,BUFFER,NITEMS,RESULT)
      END

      SUBROUTINE QWRITR (IUNIT,BUFFER,NITEMS)
      INTEGER      IUNIT, NITEMS
      REAL         BUFFER(*)
      CALL QWRITE (IUNIT,BUFFER,NITEMS)
      END

      SUBROUTINE QWRITI (IUNIT,BUFFER,NITEMS)
      INTEGER      IUNIT, NITEMS
      INTEGER      BUFFER(*)
      CALL QWRITE (IUNIT,BUFFER,NITEMS)
      END

      SUBROUTINE QWRITI2 (IUNIT,BUFFER,NITEMS)
      INTEGER      IUNIT, NITEMS
      INTEGER*2    BUFFER(*)
      CALL QWRITE (IUNIT,BUFFER,NITEMS)
      END

      SUBROUTINE QWRITB (IUNIT,BUFFER,NITEMS)
      INTEGER      IUNIT, NITEMS
      BYTE      BUFFER(*)
      CALL QWRITE (IUNIT,BUFFER,NITEMS)
      END

      SUBROUTINE QWRITQ (IUNIT,BUFFER,NITEMS)
      INTEGER      IUNIT, NITEMS
      COMPLEX      BUFFER(*)
      CALL QWRITE (IUNIT,BUFFER,NITEMS)
      END
C======================================================================
C_BEGIN_QISNAN
C
C QISNAN - check for `magic number'
C
C Usage:  LOGICAL FUNCTION QISNAN (VALUE)
C
C Input:  VALUE         REAL value to test
C
C     Returns .true. if VALUE is a `magic number' indicating the
C     absence of data.  In the current implementation, this is a NaN in
C     IEEE or Rop on a VAX or Convex native.  Any NaN (or Infinity)
C     will return .true.
C
C_END_QISNAN
C======================================================================
      LOGICAL FUNCTION QISNAN (VALUE)
      REAL VALUE
      INTEGER CISNAN
      EXTERNAL CISNAN
      QISNAN = CISNAN (VALUE) .NE. 0
      END
C======================================================================
C_BEGIN_QNAN
C
C QNAN - return canonical `magic number'
C
C Usage:  SUBROUTINE QNAN (VALUE)
C Output: VALUE         REAL `magic' value
C
C     Returns a `magic number' which can be used to indicate the absence
C     of data in an MTZ file.  In the current implementation, this is a
C     NaN in IEEE or Rop on a VAX or Convex native.
C
C_END_QNAN
C======================================================================
