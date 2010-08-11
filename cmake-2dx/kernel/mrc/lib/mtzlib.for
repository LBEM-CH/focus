C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
C     12/10/99 AWA
C     initialise the html routines - a call to ccp4h_init_lib
C
C     19/4/99 AWA
C     AWA/PRE MBATCH incresed to 5000
C
C     July 98  MDW
C     Added routines LRID, LRCLID, LWID, LWIDAS for reading/writing
C     dataset info, for use with data harvesting.
C
C     6/5/95  EJD
C      Add subroutines to recognise and use Missing nos - 
C      LRREFM   SET_MAGIC  IS_MAGIC  RESET_MAGIC EQUAL_MAGIC
C
C     16/6/94  DJGL
C     Change 96s to (parameterised) 192s to allow the possibility of
C     using the 3 SGs in the complete symop file with this number of
C     operators, although not relevant for protein work.
C
C     6th June 1993                                   Phil Evans
C      Increased MBATCH to 1000
C      Check NBATCH .le. MBATCH in LROPEN, LWBAT
C
C    REVISED on						SMM  23-Oct-1992
C    (1) Make mtzlib.1 (from WR) match library and vice versa
C    (2) General tidy up of comments, quite a lot removed
C    (3) Removed M column type (as per ccpdev mail)
C    (4) Removed unused common blocks
C    (5) Enforce 3 as minimum number of columns - not necessary, but previously
C        could open with 1 column, and they ARE reflection data files
C        Put a few more safety checks in
C
C
C    Fixed small bug in LWASSN re 0 input columns	SMM  22-June-1992
C    Renamed colasn as LKYASN (LKY because it's a utility subroutine)
C
C    Added Phil's colasn subroutine	21-Apr-1992
C
C
C    REVISED on
C    From: Sandra			11-Mar-1992
C          Removed 'S' column type from CTYPES array
C          Changed LWASSN and LWCLAB to check for duplicate column labels
C          Some general tidying
C
C    From: David Wild                 12-MAY-1992
C          Changed method of reading orientation block
C        in RBATHD to read NINTGR integers (QMODE=6)
C        followed by NREALS reals (QMODE=2)- this is 
C          needed for CONVERT stuff to work
C
C    REVISED on
C    From: David Wild                 8-MAY-1992
C          Changed QMODE call in LROPEN to read header
C          offset with mode 6 for CONVERT stuff to work
C
C    REVISED:                          Thu Jan 16 11:58:07 GMT 1992
C
C     LWCELL resets SRANGE to dummy values (0.00001,500.0) PRE
C
C     Fixed format in LRREFL/LRREFF    PRE
C
C     Small fixes from IJT
C
C     Force RESOL in LRREFL/F to lie in range SRANGE  PRE
C
C     Revised 26 nov 1991 added write statement to lwassn
C
C     Replaced LRASSN and LWASSN fropm ejd
C
C          changed arguments to LKYIN and LKYOUT
C          to add file_number (Index).
C
C          changed variable INDEX from fortran Intrinsic to MINDX
C
C          changed dummy arguements for character arrays to
C             CHARACTER*(*)  xxx(*)
C            
C          changed other REAL/INTEGER dummy arguements to
C            REAL xxx(*)   (Removing xxx(MCOLS)
C
C    REVISED on
C    From: Phil Evans			15-Oct-1991
C	 Added subroutine LWBTIT
C
C    From: SANDRA at Workshop/Cambridge 1-Oct-1991
C          add resolution range stuff, therefore changed calls LRREFF
C          and LRREFL, and added subroutine LRRSOL.
C          Changed code to allow byte swapping diskio version
C
C    From: CBS%UK.AC.EARN-RELAY::EARN.DHHEMBL5::SANDRA
C          5-JULY-1991 ... fix LWASSN,LRBRES, MTZINI becomes a subroutine
C                          changed MCOLS = 200 throughout
C
C    From: CBS%UK.AC.EARN-RELAY::EARN.DHHEMBL5::SANDRA
C          31-MAY-1991 15:24:05.81 ... addition of subroutine LRBRES
C
C    From: CBS%UK.AC.EARN-RELAY::EARN.DHHEMBL5::SANDRA
C            8-MAY-1991 13:43:22.35
C
C     STARTING version
C     From:  CBS%UK.AC.EARN-RELAY::EARN.DHHEMBL5::SANDRA
C            26-APR-1991 15:19:37.16
C
C
C    ==========
C    MTZLIB.FOR
C    ==========
C
C    MTZini.f
C    lhprt.f      lrassn.f      lrbres.f      lrcell.f      lrrsol.f
C    lrclab.f     lrclos.f      lrhist.f      lrinfo.f      lrnref.f
C    lropen.f     lrrefm.f      lrreff.f      lrrefl.f      lrrewd.f
C    lrsort.f     lrseek.f      lrsymi.f      lrsymm.f      lrtitl.f
C    lwassn.f     lwcell.f      lwclab.f      lwclos.f      lwhist.f
C    lwopen.f     lwrefl.f      lwsort.f      lwsymm.f      lwtitl.f
C    lwbscl.f     lkyin.f       lkyout.f
C    labprt.f     lbprt.f       lrbat.f       lwbat.f       rbathd.f
C    wbathd.f     lrbscl.f      lrbtit.f      lwbscl.f      lwbtit.f
C    lrhdrl.f     lwhdrl.f      lphist.f      sortup.f      addlin.f
C    nextln.f     lrncol.f      lkyset.f      lstrsl.f      lstlsq.f
C    lkyasn.f     equal_magic.f is_magic.f    reset_magic.f set_magic.f
C    lrid.f       lrclid.f      lwid.f        lwidas.f
C
C   The development of the MTZ files and associated software mark 1
C   is part of the masterplan of the ESF/EACBM Working Group 2.1 for
C   better Protein Crystallographic software for Europe.
C   It was developed jointly by members of ESF/EACBM WG2.1 & CCP4 WG2.
C
C
C  MTZ Version 1.0
C  Subroutines for standard files         SMM/HCT/EMBLHH    November 1989
C  Subroutines for multi-record files     SMM/EMBLHH/KH/DL  February 1990
C
C  MTZ Version 1.1
C  Complete rewrite of header handling    SMM/EMBLHH        November 1990
C  using new diskio routines from         PJD/DL
C  First release (sort of)                                  June 1991
C
C
C
C A complete list of subroutines in the MTZ package follows :
C
C The initialisation routine for the file handling system :
C
C   MTZINI      This MUST be called first
C
C The subroutines for MTZ files which have been opened for read are :
C
C   LROPEN      Open MTZ file for read
C
C   LRASSN      Set up the input column assignments
C
C   LRBATS      Return total number of batches, and their serial numbers
C               from the MTZ header (for multi-record files)
C
C   LRBRES      Reset batch header reading pointer
C
C   LRCELL      Return the Cell dimensions from the MTZ header
C
C   LRCLAB      Return the Column labels and types from the MTZ header
C
C   LRHIST      Return the history information from the MTZ header
C
C   LRINFO      Return general info (no.of cols, ranges etc.)
C               from the MTZ header
C
C   LRNCOL	Returns number of columns from the MTZ header
C
C   LRRSOL	Return the minimum and maximum resolution from
C       	the MTZ header (1/d-squared)
C
C   LRSORT      Return the sort order from the MTZ header
C
C   LRSYMI      Return symmetry information (if available)
C               from the MTZ header
C
C   LRSYMM      Return symmetry operations (if available)
C               from the MTZ header
C
C   LRTITL      Return the title from the MTZ header
C
C   LRREFL      Read a reflection record in file order from the MTZ file
C
C   LRREFF      Read a reflection record in Lookup order
C               from the MTZ file
C
C   LRREFM      Called after a reflection is read in it returns a logical 
C               array. If datum=MNF then the corresponding element is true, in 
C               all other cases it is false
C
C   LRNREF      Return the number of the reflection record last read
C               ie reports on position in file
C
C   LRSEEK      Move to a specific reflection record number
C
C   LRREWD      Rewind an MTZ file for re-read
C
C   LRCLOS      Close an MTZ file which has been opened for read
C
C
C The subroutines for MTZ files which have been opened for write are :
C
C   LWOPEN      Open MTZ file for write
C
C   LWASSN      SeT up the output column assignments, new columns etc.
C
C   LWCELL      Update the Cell dimensions in the MTZ header
C
C   LWCLAB      Write Column labels and types to an output MTZ file
C
C   LWHIST      Append to the history information in the MTZ header
C
C   LWHSTL      Append standard line to the history information
C
C   LWSORT      Update the Sort order in the MTZ header
C
C   LWSYMM      Update the Symmetry operation in the MTZ header
C
C   LWTITL      Append to or replace the title in the MTZ header
C
C   LWREFL      Write a reflection record to the MTZ file
C
C   LWCLOS      Close an MTZ file which has been opened for write
C
C
C The (extra) subroutines for multi-record MTZ files are :
C
C   LRBAT       Return batch information from the header for one batch
C
C   LWBAT       Write updated batch header for one batch
C
C   LRBSCL      Return selected values from the header for one batch
C
C   LRBTIT      Return the batch title from the header for one batch
C
C   LWBSCL      Update selected values in the header for one batch
C
C   LWBTIT      Write batch title & dummy orientation block for one batch
C
C   LBPRT       Print batch information in pretty format -
C               called by LRBAT - calls LBPRTH (does the actual print)
C
C
C The utility subroutines for MTZ files are :
C
C   LKYIN       These subroutines should be used by the calling program
C   LKYOUT      to parse any label assignments (input and output) made
C               by the user
C
C   LKYSET      Similar to LKYIN except the "lookup" array is passed back
C               to the program
C
C   LKYASN      Jiffy to call LKYIN & LRASSN, no need for parser in main prog
C
C   LHPRT       Subroutine to output data from an MTZ header mark 1
C
C   LPHIST      Print the history information from the MTZ header
C
C
C These routines are specifically for missing number flags (MNF)
C
C   SET_MAGIC   Either returns the value of MNF in the MTZ header
C               or overwrites it.
C
C   RESET_MAGIC Alters the MNF per reflection in the data itself 
C
C   EQUAL_MAGIC Initialises an array so all elements are set to MNF
C
C   IS_MAGIC    Routine that checks any datum to see if datum=MNF
C
C
C The next 13 are internal subroutines
C
C   LBPRTH      Does the actual printing for LBPRT
C
C   RBATHD      Read the actual header from the file and return the data
C               items packed into a REAL and a CHAR array - called by LROPEN
C
C   WBATHD      Write the batch headers to the MTZ file open for write
C               the batch headers are written at the end of the file -
C               called by LWCLOS
C
C   LRHDRL      Read one line from the header on the file (calls qreadc)
C
C   SORTUP      INTEGER version of indexed Singleton sort -
C               copied from STILLS program.
C
C   LABPRT      Subroutine used to output character strings (eg column
C               labels) nicely across 80 character page
C
C   ADDLIN      Subroutine to add a new line to a character array
C
C   NEXTLN      Function to find the next empty line in a character array
C
C   LSTRSL      Routine to calculate coefficients for (sin(theta)/lambda)**2 
C               from h,k,l for general axes
C
C   LSTLSQ      Function to calculate (sin(theta)/lambda)**2 from h,k,l
C               coef's set by call to LSTRSL, for the file open on index MINDX
C
C
C COMMON blocks for MTZ subroutines
C
C     Common MTZHDR contains INTEGER and REAL variables
C            from the MTZ Headers
C
C     Common MTZCHR contains CHARACTER variables from the MTZ headers
C
C     Common MTZWRK contains work variables for the MTZ routines
C
C     Common MTZWRC contains character work variables for MTZ routines
C
C     Since they are not in the main program, we must SAVE them
C
C
C
C     =================
      SUBROUTINE MTZINI
C     =================
C
C---- This subroutine initialises the MTZ file handling system.
C
C     THIS SUBROUTINE MUST BE CALLED BEFORE ANY OTHER MTZLIB SUBROUTINE.
C     Failure to do so may result in unpredictable behaviour (!?)
C
C     This subroutine replaces (6/91) the BLOCK DATA MTZINI.
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH,MFILEX
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000,MFILEX=9)
C
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
C     ..
C     .. Arrays in Common ..
      REAL RBATW,WRANGE,WSRNGE
      INTEGER NLUSRI,NLUSRO
      INTEGER HDRST,NBATR,NBATW,NCOLW,NHISTL,NPLABS,NREFR,NREFW,RLUN,
     +        RPOINT,WLUN,WOMBAT,NDATMSS
      LOGICAL SORTB,DATMSS
      CHARACTER LSUSRI*30,LSUSRO*30
C     ..
C     .. Local Scalars ..
      INTEGER JDO10,JDO20,JDO30
C     ..
C     .. External subroutines
      EXTERNAL CCP4H_INIT_LIB
C     ..
C     .. Common blocks ..
      COMMON /MTZLAB/NLUSRI(MFILEX),NLUSRO(MFILEX)
      COMMON /MTZLBC/LSUSRI(MFILEX,MCOLS),LSUSRO(MFILEX,MCOLS)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZLAB/, /MTZLBC/, /MTZWRK/
C
C---- Initialize those variables which MUST be 0 or ' ' at the very start
C
C     NLUSRI - no. of user input labels (changed by LKYIN)
C     LSUSRI - the labels
C     NLUSRO - no. of user output labels (changed by LKYOUT)
C     LSUSRO - the labels
C     RLUN   - unit numbers for files open for read (>0 means file open)
C     WLUN   - unit numbers for files open for write (>0 means file open)
C
C
      DO 20 JDO20  = 1,MFILEX
        NLUSRI(JDO20) = 0
        NLUSRO(JDO20) = 0
C
        DO 10 JDO10 = 1,MCOLS
          LSUSRI(JDO20,JDO10) = ' '
          LSUSRO(JDO20,JDO10) = ' '
   10   CONTINUE
C
   20 CONTINUE
C
      DO 30 JDO30 = 1,MFILES
        RLUN(JDO30) = 0
        WLUN(JDO30) = 0
 30   CONTINUE
C Iniitialise the html flag so there are no prblems with stderr
      CALL CCP4H_INIT_LIB()

      END
C
C
C
C     ====================================================
      SUBROUTINE LRASSN(MINDX,LSPRGI,NLPRGI,LOOKUP,CTPRGI)
C     ====================================================
C
C---- Subroutine to apply the input column label assignments to the
C     MTZ file which is open for read and setup the Lookup pointer array
C
C     Also note that having an unassigned program label is a
C     fatal error, but this subroutine uses LERROR to report
C     this as a warning, so that all the program label assignments
C     can be checked before stopping.
C
C     First program label which is assigned and is not an index
C     defines default dataset for new MTZ columns
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     LSPRGI    (I)	CHARACTER*30    array of dimension at least NLPRGI
C                               	containing the program label strings
C
C     NLPRGI    (I)	INTEGER         number of input program labels
C
C     LOOKUP    (I/O)	INTEGER         array of dimension at least NLPRGI
C                               	containing index from program labels
C                               	to file labels. On entry these should 
C                               	be set to -1 for compulsory labels 
C                               	and 0 for optional labels, and on exit
C                               	labels which are not present in the 
C                               	file have their lookup entry set to 0.
C
C     CTPRGI    (I)	CHARACTER*1	array of dimension at least NLPRGI
C                               	containing the program label types
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH,MFILEX
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000,MFILEX=9)
      INTEGER MSETS
      PARAMETER (MSETS=MCOLS)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER NHISLM
      PARAMETER (NHISLM=30)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,NLPRGI
C     ..
C     .. Array Arguments ..
      INTEGER LOOKUP(*)
      CHARACTER*1  CTPRGI(*)
      CHARACTER*30 LSPRGI(*)
C     ..
C     .. Scalars in Common ..
      INTEGER NLUSRI,NLUSRO
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS,NSETW,SET_ID,CSET_ID,IDEFSET
      LOGICAL SORTB,DATMSS,VAL_SET
      CHARACTER CBATR*1,CBATW*1,CTYPE*1,LTYPE*1,PGNAM*10,SPGNAM*10,
     +          CLABEL*30,LSUSRI*30,LSUSRO*30,PLABS*30,TITLE*70,HSCR*80,
     +          ENTRY_ID*64,DIFFRN_ID*64
C     ..
C     .. Local Scalars ..
      INTEGER IEND,IERR,IFAIL,II,IST,ISTAT,JDO10,JDO20,JDO30,
     +        JDO50,JDO60,JDO90,JDO100,JDO110,JLENG
      CHARACTER CWORK*30,LINE*400,STROUT*400
C     ..
C     .. External Functions .. 
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL BLANK,LABPRT,LERROR,PUTLIN, CCPERR
C     ..
C     .. Common blocks ..
      COMMON /MTZCHR/TITLE(MFILES),CLABEL(MCOLS,MFILES),
     +       CTYPE(MCOLS,MFILES),SPGNAM(MFILES),LTYPE(MFILES),
     +       CBATR(CBLENG,MBATCH,MFILES),PGNAM(MFILES)
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZLAB/NLUSRI(MFILEX),NLUSRO(MFILEX)
      COMMON /MTZLBC/LSUSRI(MFILEX,MCOLS),LSUSRO(MFILEX,MCOLS)
      COMMON /MTZWRC/PLABS(MCOLS,MFILES),HSCR(NHISLM,MFILES),
     +       CBATW(CBLENG,MBATCH,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
      COMMON /MTZHAR/NSETW(MFILES),SET_ID(MSETS,MFILES),
     +       ENTRY_ID(MSETS,MFILES),DIFFRN_ID(MSETS,MFILES),
     +       CSET_ID(MCOLS,MFILES),IDEFSET(MFILES)
C     ..
C     .. Save statement ..
      SAVE
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRASSN : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Rpoint is the same as lookup, but held in the MTZWRK COMMON
C     work with this and copy to LOOKUP at the end, so set to 0
C     Also store the number of program labels in MTZWRK, and the
C     program labels in MTZWRC
C
        DO 10 JDO10 = 1,MCOLS
          RPOINT(JDO10,MINDX) = 0
   10   CONTINUE
C
        NPLABS(MINDX) = NLPRGI
C
        DO 20 JDO20 = 1,NLPRGI
          PLABS(JDO20,MINDX) = LSPRGI(JDO20)
   20   CONTINUE
C
C---- First loop over user input labels and assign them to file labels
C     If NLUSRI(MINDX)=0 then this loop is never executed
C
        DO 70 JDO50 = 1,NLPRGI
          CWORK = LSUSRI(MINDX,JDO50)
          IF ((NLUSRI(MINDX).GT.0) .AND. (CWORK.NE.' ')) THEN
C
            DO 30 JDO30 = 1,NCOLS(MINDX)
              IF (CWORK.EQ.CLABEL(JDO30,MINDX)) GO TO 40
   30       CONTINUE
C
            JLENG = LENSTR(CWORK)
            ISTAT = 1
C
C                ************************
            CALL LERROR(ISTAT, IFAIL,
     +           'From LRASSN : User input label '//
     +           CWORK(1:JLENG) // ' does not match any file label')
C                ************************
C
            IERR = 10
            GO TO 70
   40       RPOINT(JDO50,MINDX) = JDO30
          ENDIF
   70   CONTINUE
C
C---- No input assignment given for hard/soft compulsory program labels
C
        DO 75 JDO50 = 1,NLPRGI
          CWORK = LSUSRI(MINDX,JDO50)
          IF ((NLUSRI(MINDX).EQ.0) .OR. (CWORK.EQ.' ')) THEN
C
C---- Check to see if the program label is compulsory, or has been assigned
C      before hand.
            CWORK = LSPRGI(JDO50)
            DO 50 JDO60 = 1,NCOLS(MINDX)
              IF (CWORK.EQ.CLABEL(JDO60,MINDX)) THEN
               IF(ABS(LOOKUP(JDO50)).NE.1)GOTO 75
                DO 55 JDO55 = 1,NCOLS(MINDX)
                  IF (RPOINT(JDO55,MINDX).EQ.JDO60) GOTO 75
   55           CONTINUE
                GO TO 60
              END IF
   50       CONTINUE
C
C---- Terminate if lookup=-1 (hard), if lookup=1 (soft) set to zero
C
            IF (LOOKUP(JDO50).EQ.-1) THEN
              JLENG = LENSTR(CWORK)
              ISTAT = 1
              WRITE (LINE,FMT='(10A)') 'From LRASSN : Program Label ',
     +          CWORK(1:JLENG),' was not assigned'
C
C                  ************************
              CALL LERROR(ISTAT,IFAIL,LINE)
C                  ************************
C
              IERR = 10
            END IF
            IF (LOOKUP(JDO50).EQ.1) LOOKUP(JDO50) = 0
            GO TO 75
   60       RPOINT(JDO50,MINDX) = JDO60
          END IF
   75   CONTINUE
C
C---- Copy RPOINT to LOOKUP
C
        DO 80 JDO90 = 1,NLPRGI
          LOOKUP(JDO90) = RPOINT(JDO90,MINDX)
   80   CONTINUE
C
C---- If we had some errors then return or stop now
C
        IF (IERR.NE.10) THEN
C
C---- Print out program labels, column labels and lookup
C
          STROUT = '* Input Program Labels :'
C
C              ****************
          CALL PUTLIN(STROUT,'CURWIN')
          CALL BLANK('CURWIN',1)
          CALL LABPRT(LSPRGI,NLPRGI)
          CALL BLANK('CURWIN',1)
          STROUT = '* Input File Labels :'
          CALL PUTLIN(STROUT,'CURWIN')
          CALL BLANK('CURWIN',1)
          CALL LABPRT(CLABEL(1,MINDX),NCOLS(MINDX))
          CALL BLANK('CURWIN',1)
          STROUT = 
     +     '* Lookup Table : the number indicates the input column no.'
          CALL PUTLIN(STROUT,'CURWIN')
          STROUT = 
     +     '* Array element n corresponds to the nth program label'
          CALL PUTLIN(STROUT,'CURWIN')
          CALL BLANK('CURWIN',1)
C              *****************
C
          IST = 0
   90     CONTINUE
C
          IEND = IST + 15
          IF (IEND.GT.NLPRGI) IEND = NLPRGI
          WRITE (STROUT,FMT='(15I5)') (LOOKUP(II),II=IST+1,IEND)
C
C              ****************
          CALL PUTLIN(STROUT,'CURWIN')
C              ****************
C
          IST = IEND
          IF (IST.LT.NLPRGI) GO TO 90
C
C              *****************
          CALL BLANK('CURWIN',1)
C              *****************
C
C---- Check the column types if any given
C
          DO 100 JDO100 = 1,NLPRGI
            IF (LOOKUP(JDO100).NE.0) THEN
C
C---- CTPRGI = blank
C
              IF (CTPRGI(JDO100).EQ.' ') THEN
                IF (CTYPE(LOOKUP(JDO100),MINDX) .NE.' ') THEN
                  CTPRGI(JDO100) =  CTYPE(LOOKUP(JDO100),MINDX)
                ELSE
                  CTPRGI(JDO100) =  'R'
                END IF
C
C---- CTPRGI is given
C
              ELSE 
C               The check for 'U' and 'V' here is for the translation
C               function special in CAD to avoid the warning
                IF ((CTYPE(LOOKUP(JDO100),MINDX).NE.CTPRGI(JDO100)).AND.
     +              (CTYPE(LOOKUP(JDO100),MINDX).NE.'R') .AND.
     +              (CTPRGI(JDO100).NE.'U') .AND.
     +              (CTPRGI(JDO100).NE.'V') .AND.
     +              (CTYPE(LOOKUP(JDO100),MINDX).NE.' ')) THEN
                  JLENG = LENSTR(LSPRGI(JDO100))
                  ISTAT = 1
                  WRITE (LINE,FMT='(10A)')
     +              'From LRASSN : Column type for program label ',
     +              LSPRGI(JDO100) (1:JLENG),' does not match -',
     +              ' file label is ', CLABEL(LOOKUP(JDO100),MINDX)
C
C                      ************************
                  CALL LERROR(ISTAT,IFAIL,LINE)
C                      ************************
C
                END IF
              END IF 
            END IF
  100     CONTINUE
C
C---- First program label which is assigned and is not an index
C     defines default dataset
C
          DO 110 JDO110 = 1,NLPRGI
            IF (LOOKUP(JDO110).NE.0 .AND. CTPRGI(JDO110).NE.'H') THEN
              IDEFSET(MINDX) = CSET_ID(LOOKUP(JDO110),MINDX)
              GOTO 120
            ENDIF
  110     CONTINUE
C
  120     CONTINUE
C
        ELSE
C
C              ********************************
          CALL CCPERR(1,'Error in label assignments')
C              ********************************
C
        END IF
      END IF
C
      END
C
C
C
C     =====================================
      SUBROUTINE LRBATS(MINDX,NBATX,BATCHX)
C     =====================================
C
C---- Subroutine to return the number of batches and their batch
C     serial numbers from the header of a multi-record MTZ file
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     NBATX     (I/O)	INTEGER         Dimension of BATCHX on entry;
C                                       number of batches in the file on
C                               	exit
C
C     BATCHX    (O)	INTEGER         array of dimension at least NBATX
C                               	containing the serial numbers of the
C                               	batches in the file
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,NBATX
C     ..
C     .. Array Arguments ..
      INTEGER BATCHX(*)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,RBATR,RSYM,SRANGE,VAL_MISS
      INTEGER BATNUM,ISORT,NBATCH,NCOLS,NREFS,NSPGRP,NSYM,NSYMP
      LOGICAL VAL_SET
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT,JDO10
      CHARACTER LINE*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRBATS : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      END IF
C
C---- Copy number of batches from header
C
      NBATX = NBATCH(MINDX)
C
C---- And copy the batch serial numbers from BATNUM to BATCHX
C     if a multi-record file
C
      IF (NBATCH(MINDX).GT.0) THEN
C
        DO 10 JDO10 = 1,NBATCH(MINDX)
          BATCHX(JDO10) = BATNUM(JDO10,MINDX)
   10   CONTINUE
C
      END IF
C
      END
C
C
C
C     ==============================
      SUBROUTINE LRBRES (MINDX,BATNO)
C     ==============================
C
C---- Subroutine to reset the batch header info pointer to the
C     the batch BATNO, for re-read by subroutine LRBAT, for an MTZ file which
C     has been opened for read on index MINDX.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     BATNO     (I)	INTEGER         batch number to reset pointer to
C                               	If BATNO = 0, reset to beginning of
C                               	batch headers
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX, BATNO
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RSYM,WRANGE,WSRNGE,VAL_MISS,RBATR,RBATW
      INTEGER ISORT,NCOLS,NCOLW,NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,
     +        NSYMP,RLUN,RPOINT,WLUN,NBATCH,BATNUM,NBATW,WOMBAT,NDATMSS,
     +        NBATR,HDRST,NHISTL
      LOGICAL SORTB,DATMSS,VAL_SET
C     ..
C     .. Local Scalars ..
      CHARACTER  LINE2*400
      INTEGER    ISTAT,IFAIL,JDO10
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),
     +       ISORT(5,MFILES),CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZWRK/
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE2,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRBRES : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            *************************
        CALL LERROR(ISTAT,IFAIL,LINE2)
C            *************************
C
      END IF
C
C---- Check if it's really a multi-record file
C
      IF (NBATCH(MINDX).EQ.0) THEN
        WRITE (LINE2,FMT='(A,A,I2,A)') 'From LRBRES : File open on ',
     +   'index',MINDX,' is NOT a multi-record file'
        ISTAT = 2
        IFAIL = -1
C
C            *************************
        CALL LERROR(ISTAT,IFAIL,LINE2)
C            *************************
C
      END IF
C
      IF (BATNO .EQ. 0) THEN
C
C---- Reset the pointer to the current batch header to the start
C
         NBATR(MINDX) =  0
C
      ELSE
C
C---- Reset the pointer to batch BATNO
C     Find it
C
        DO 10, JDO10 = 1, NBATCH(MINDX)
          IF (BATNO .EQ. BATNUM(JDO10,MINDX)) THEN
C
C---- Found, set pointer to previous batch (incremented in LRBAT)
C
            NBATR(MINDX) = JDO10 - 1
            RETURN
C           ======
C
          END IF
 10     CONTINUE
C
C---- Not found
C
        WRITE (LINE2,FMT='(A,A,I2,A,I8)') 
     +        'From LRBRES : File open on ',
     +        'index',MINDX,' has no batch number ',BATNO
        ISTAT = 2
        IFAIL = -1
C
C            *************************
        CALL LERROR(ISTAT,IFAIL,LINE2)
C            *************************
      END IF
C
      RETURN
C
      END
C
C
C
C     ============================================
      SUBROUTINE LRBSCL (MINDX,BATNO,BATSCL,NBATSC)
C     ============================================
C
C---- Subroutine to read batch scale, Bfactor & batch title for
C     batch BATNO from batch headers from multi-record file open
C     on index MINDX
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     BATNO     (I)	INTEGER         batch number 
C
C     NBATSC    (I)	INTEGER         maximum number of batch scales expected
C
C     BATSCL	(O)	REAL            array of dimension at least NBATSC
C					containing batch scale, relative Bfactor,
C                               	SD(Bscale), SD(Bfactor) if NBATSC = 4
C                                	else NBATSC/2 scales or B-factors + Sd's
C					on exit
C
C     NBATSC    (O)	INTEGER         number of batch scales found
C
C     .. Arguments ..
      INTEGER MINDX, BATNO, NBATSC
      REAL BATSCL(NBATSC)
C
C     .. Common blocks ..
      INTEGER NWORDS,NINTGR,NREALS,IORTYP,LBCELL,MISFLG,
     .     JUMPAX,NCRYST,LCRFLG,LDTYPE,JSCAXS,NBSCAL,NGONAX,LBMFLG,
     $     NDET,LBSETID,INTPAD
      REAL   CELL,UMAT,PHIXYZ,CRYDAT,DATUM,
     $     PHISTT,PHIEND,PHIRANGE,SCANAX,TIME1,TIME2,
     $     BSCALE,BBFAC,SDBSCL,SDBFAC,BATPAD,E1,E2,E3,GONPAD,
     $     SOURCE,S0,BEMDAT,
     $     DX1,THETA1,DETLM1,DX2,THETA2,DETLM2,DETPAD
      CHARACTER BTITLE*70, GONLAB*8, BTITL*94
C     
      COMMON /CBTHDR/BTITLE,GONLAB(3)
C     
      COMMON /MBTHDR/ NWORDS,NINTGR,NREALS,IORTYP,LBCELL(6),MISFLG,
     .     JUMPAX,NCRYST,LCRFLG,LDTYPE,JSCAXS,NBSCAL,NGONAX,LBMFLG,
     +     NDET,LBSETID,INTPAD(8),
     +     CELL(6),UMAT(3,3),PHIXYZ(3,2),CRYDAT(12),DATUM(3),
     $     PHISTT,PHIEND,SCANAX(3),TIME1,TIME2,
     $     BSCALE,BBFAC,SDBSCL,SDBFAC,PHIRANGE,BATPAD(11),
     $     E1(3),E2(3),E3(3),
     $     GONPAD(12),SOURCE(3),S0(3),BEMDAT(25),
     $     DX1,THETA1,DETLM1(2,2),DX2,THETA2,DETLM2(2,2),DETPAD(33)
      SAVE /MBTHDR/, /CBTHDR/
      EXTERNAL LRBRES, LRBAT
C     
C*** Equivalence undetermined number of scale factors to BSCALE
      REAL SCALES(16)
      EQUIVALENCE (SCALES(1),BSCALE)
      REAL RNWRDS (1)
      EQUIVALENCE (NWORDS,RNWRDS)
C
      INTEGER ISTAT,IFAIL,I
      CHARACTER*100 LINE2
      EXTERNAL LERROR
C
C--- Set read pointer to this batch
      CALL LRBRES(MINDX,BATNO)
C--- Read Orientation block
C     NB the original of this had BTITLE in the call, but it's in common
C     and updated during the call, so illegal
      BTITL = BTITLE
      CALL LRBAT(MINDX,BATNO,RNWRDS,BTITL,0)
      BTITLE = BTITL
C
C NBSCAL is number of scales etc found in orientation block
      IF (NBSCAL .GT. NBATSC) THEN
C--- Too many found
        WRITE (LINE2,FMT='(A,I4,A,A,I6,A,I4)') 
     +    'From LRBSCL : ',NBSCAL,' too many batch scales in',
     +    ' orientation block for batch ',BATNO,', maximum ',NBATSC
        ISTAT = 2
        IFAIL = -1
C
C            *************************
        CALL LERROR(ISTAT,IFAIL,LINE2)
C            *************************

      ELSE
C--- Copy scales
         NBATSC = NBSCAL
         IF (NBATSC .GT. 0) THEN
            DO 10, I=1,NBATSC
               BATSCL(I) = SCALES(I)
 10         CONTINUE
         ENDIF
      ENDIF
C
      RETURN
C
      END
C
C
C
C     ============================================
      SUBROUTINE LRBTIT (MINDX,BATNO,TBATCH,IPRINT)
C     ============================================
C
C---- Subroutine to read batch title for
C     batch BATNO from batch headers from multi-record mtz file open
C     on index MINDX
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     BATNO     (I)	INTEGER         batch number 
C
C     IPRINT    (I)	INTEGER         print flag
C                             		= 0  no print
C                             		= 1  print title
C					= 2  print title & orientation block
C
C     TBATCH    (O)	CHARACTER       batch title
C
C ... Arguments
      INTEGER MINDX, BATNO, IPRINT
      CHARACTER*(*) TBATCH
C
C     .. Common blocks ..
      INTEGER NWORDS,NINTGR,NREALS,IORTYP,LBCELL,MISFLG,
     .     JUMPAX,NCRYST,LCRFLG,LDTYPE,JSCAXS,NBSCAL,NGONAX,LBMFLG,NDET,
     $     LBSETID,INTPAD
      REAL   CELL,UMAT,PHIXYZ,CRYDAT,DATUM,
     $     PHISTT,PHIEND,PHIRANGE,SCANAX,TIME1,TIME2,
     $     BSCALE,BBFAC,SDBSCL,SDBFAC,BATPAD,E1,E2,E3,GONPAD,
     $     SOURCE,S0,BEMDAT,
     $     DX1,THETA1,DETLM1,DX2,THETA2,DETLM2,DETPAD
      CHARACTER BTITLE*70, GONLAB*8, BTITL*94
C     
      COMMON /CBTHDR/BTITLE,GONLAB(3)
C     
      COMMON /MBTHDR/ NWORDS,NINTGR,NREALS,IORTYP,LBCELL(6),MISFLG,
     .     JUMPAX,NCRYST,LCRFLG,LDTYPE,JSCAXS,NBSCAL,NGONAX,LBMFLG,
     +     NDET,LBSETID,INTPAD(8),
     +     CELL(6),UMAT(3,3),PHIXYZ(3,2),CRYDAT(12),DATUM(3),
     $     PHISTT,PHIEND,SCANAX(3),TIME1,TIME2,
     $     BSCALE,BBFAC,SDBSCL,SDBFAC,PHIRANGE,BATPAD(11),
     $     E1(3),E2(3),E3(3),
     $     GONPAD(12),SOURCE(3),S0(3),BEMDAT(25),
     $     DX1,THETA1,DETLM1(2,2),DX2,THETA2,DETLM2(2,2),DETPAD(33)
      SAVE /CBTHDR/, /MBTHDR/
      REAL RNWRDS (1)
      EXTERNAL LRBRES, LRBAT
      EQUIVALENCE (NWORDS,RNWRDS)
C     
C--- Set read pointer to this batch
      CALL LRBRES(MINDX,BATNO)
C--- Read Orientation block
C     NB the original of this had BTITLE in the call, but it's in common
C     and updated during the call, so illegal
      BTITL = BTITLE
      CALL LRBAT(MINDX,BATNO,RNWRDS,BTITL,IPRINT)
      BTITLE = BTITL
C
C Copy title
      TBATCH = BTITLE
C
      RETURN
C
      END
C
C
C
C     ==============================
      SUBROUTINE LRCELL(MINDX,CELLP)
C     ==============================
C
C
C---- Subroutine to return Cell Parameters from header of MTZ file open for read
C     on index MINDX
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     CELLP     (O)	REAL            array of dimension (6) containing cell
C                               	parameters from header on exit
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX
C     ..
C     .. Array Arguments ..
      REAL CELLP(6)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RSYM,VAL_MISS
      INTEGER BATNUM,ISORT,NBATCH,NCOLS,NREFS,NSPGRP,NSYM,NSYMP
      LOGICAL VAL_SET
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT,JDO10
      CHARACTER LINE*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRCELL : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Then return the cell parameters
C
        DO 10 JDO10 = 1,6
          CELLP(JDO10) = CELL(JDO10,MINDX)
   10   CONTINUE
C
      END IF
C
      END
C
C
C
C     =========================================
      SUBROUTINE LRCLAB(MINDX,CLABS,CTYPS,NCOL)
C     =========================================
C
C
C---- Subroutine to return the column labels, column types and number
C     of columns from the header of an MTZ file.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     CLABS     (O)	CHARACTER*30    array of dimension at least NCOL
C                               	containing the column labels on exit
C
C     CTYPS     (O)	CHARACTER*1     array of dimension at least NCOL
C                               	containing the column types on exit
C
C     NCOL      (I/O)	INTEGER         on entry, dimension of CLABS, CTYPS on
C                                       exit, number of columns in the
C                                       MTZ file
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,NCOL
C     ..
C     .. Array Arguments ..
      CHARACTER*1  CTYPS(*)
      CHARACTER*30 CLABS(*)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RSYM,VAL_MISS
      INTEGER BATNUM,ISORT,NBATCH,NCOLS,NREFS,NSPGRP,NSYM,NSYMP
      CHARACTER CBATR*1,CTYPE*1,LTYPE*1,PGNAM*10,SPGNAM*10,
     +          CLABEL*30,TITLE*70
      LOGICAL VAL_SET
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT,JDO10
      CHARACTER LINE*400
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZCHR/TITLE(MFILES),CLABEL(MCOLS,MFILES),
     +       CTYPE(MCOLS,MFILES),SPGNAM(MFILES),LTYPE(MFILES),
     +       CBATR(CBLENG,MBATCH,MFILES),PGNAM(MFILES)
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZCHR/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRCLAB : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Then return column information
C
        NCOL = NCOLS(MINDX)
C
        DO 10 JDO10 = 1,NCOL
          ISTAT = LENSTR(CLABEL(JDO10,MINDX))
          CLABS(JDO10) = CLABEL(JDO10,MINDX) (1:ISTAT)
          CTYPS(JDO10) = CTYPE(JDO10,MINDX)
   10   CONTINUE
C
      END IF
C
      END
C
C
C     =========================================
      SUBROUTINE LRCLID(MINDX,CSETID,NCOL)
C     =========================================
C
C
C---- Subroutine to return the ID of the related dataset for each 
C     column of the input MTZ file, as obtained from the header.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     CSETID    (O)	INTEGER         array of dimension at least NCOL
C                               	containing the dataset IDs on exit
C
C     NCOL      (O)	INTEGER         number of columns in the MTZ file
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MSETS
      PARAMETER (MSETS=MCOLS)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,NCOL
C     ..
C     .. Array Arguments ..
      INTEGER CSETID(*)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RSYM,VAL_MISS
      INTEGER BATNUM,ISORT,NBATCH,NCOLS,NREFS,NSPGRP,NSYM,NSYMP,
     +          NSETW,SET_ID,CSET_ID,IDEFSET
      CHARACTER ENTRY_ID*64,DIFFRN_ID*64
      LOGICAL VAL_SET
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT,JDO10
      CHARACTER LINE*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZHAR/NSETW(MFILES),SET_ID(MSETS,MFILES),
     +       ENTRY_ID(MSETS,MFILES),DIFFRN_ID(MSETS,MFILES),
     +       CSET_ID(MCOLS,MFILES),IDEFSET(MFILES)
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRCLID : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Then return column information
C
        NCOL = NCOLS(MINDX)
C
        DO 10 JDO10 = 1,NCOL
          CSETID(JDO10) = CSET_ID(JDO10,MINDX)
   10   CONTINUE
C
      END IF
C
      END
C
C
C
C     ========================
      SUBROUTINE LRCLOS(MINDX)
C     ========================
C
C---- Subroutine to close an MTZ file which has been opened for read.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX
C     ..
C     .. Arrays in Common ..
      REAL RBATW,WRANGE,WSRNGE
      INTEGER HDRST,NBATR,NBATW,NCOLW,NHISTL,NPLABS,NREFR,NREFW,RLUN,
     +        RPOINT,WLUN,WOMBAT,NDATMSS
      LOGICAL SORTB,DATMSS
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT
      CHARACTER LINE*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR,QCLOSE
C     ..
C     .. Common blocks ..
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZWRK/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRCLOS : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Close the file (for safety, check it was open)
C
C            *******************
        IF (RLUN(MINDX).NE.0) CALL QCLOSE(RLUN(MINDX))
C            *******************
C
C---- Mark this index as unoccupied for read
C
        RLUN(MINDX) = 0
C
      END IF
C
      END
C
C
C
C     =====================================
      SUBROUTINE LRHIST(MINDX,HSTRNG,NLINES)
C     =====================================
C
C
C---- Subroutine to return the history information from the MTZ
C     file header.  This text can be anything the programmer chooses,
C     (but probably programs stamps at least).
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     HSTRNG    (O)	CHARACTER       array of (NLINES) with the history lines
C
C     NLINES    (I/O)	INTEGER         on entry, dimension of HSTRNG; on
C                                       exit, number of history lines being
C                                       returned.
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER NHISLM
      PARAMETER (NHISLM=30)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,NLINES
C     ..
C     .. Array Arguments ..
      CHARACTER HSTRNG(*)*(*)
C     ..
C     .. Arrays in Common ..
      REAL RBATW,WRANGE,WSRNGE
      INTEGER HDRST,NBATR,NBATW,NCOLW,NHISTL,NPLABS,NREFR,NREFW,RLUN,
     +        RPOINT,WLUN,WOMBAT,NDATMSS
      LOGICAL SORTB,DATMSS
      CHARACTER CBATW*1,PLABS*30,HSCR*80
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT,JDO10
      CHARACTER LINE*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZWRC/PLABS(MCOLS,MFILES),HSCR(NHISLM,MFILES),
     +       CBATW(CBLENG,MBATCH,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZWRK/,/MTZWRC/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRHIST : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Then check if there are any history lines present
C
        IF (NHISTL(MINDX).EQ.0) THEN
C
          NLINES = 0
C
        ELSE
C
C---- Copy the history lines to HSTRNG
C
          DO 10 JDO10 = 1,NHISTL(MINDX)
            HSTRNG(JDO10) = HSCR(JDO10,MINDX)
   10     CONTINUE
          NLINES = NHISTL(MINDX)
C
        END IF
      END IF
C
      END
C
C
C
C     ===================================================
      SUBROUTINE LRINFO(MINDX,VERSNX,NCOLX,NREFLX,RANGES)
C     ===================================================
C
C
C---- Subroutine to return information about the MTZ file open for
C     read on index MINDX.
C
C---- Arguments : 
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     VERSNX    (O)	CHARACTER*10    version stamp of MTZ subroutines used
C                               	to write the file.
C
C     NCOLX     (I/O)	INTEGER         on entry, last dimension of RANGES; on
C                                       exit, number of columns in the file
C
C     NREFLX    (O)	INTEGER         number of reflection records in the file
C
C     RANGES    (O)	REAL            array(2,ncolx) containing the minimum
C                               	and maximum values in each column
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      CHARACTER*10 VERSN
      PARAMETER (VERSN='MTZ:V1.1')
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,NCOLX,NREFLX
      CHARACTER VERSNX*10
C     ..
C     .. Array Arguments ..
      REAL RANGES(2,*)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RSYM,VAL_MISS
      INTEGER BATNUM,ISORT,NBATCH,NCOLS,NREFS,NSPGRP,NSYM,NSYMP
      LOGICAL VAL_SET
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT,JDO10
      CHARACTER LINE*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRINFO : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Then return the information
C
        VERSNX = VERSN
        NCOLX = NCOLS(MINDX)
        NREFLX = NREFS(MINDX)
C
        DO 10 JDO10 = 1,NCOLS(MINDX)
          RANGES(1,JDO10) = CRANGE(1,JDO10,MINDX)
          RANGES(2,JDO10) = CRANGE(2,JDO10,MINDX)
   10   CONTINUE
C
      END IF
C
      END
C
C^L
C
C     ==============================
      SUBROUTINE LRNCOL(MINDX,NCOLX)
C     ==============================
C
C---- Subroutine to return the number of columns in the MTZ file open for
C     read on index MINDX. A simpler call than LRINFO.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     NCOLX     (O)	INTEGER         number of columns in MTZ file
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar arguments ..
      INTEGER MINDX,NCOLX
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RSYM,VAL_MISS
      INTEGER BATNUM,ISORT,NBATCH,NCOLS,NREFS,NSPGRP,NSYM,NSYMP 
      LOGICAL VAL_SET
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT
      CHARACTER LINE*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common Blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRNCOL : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
        NCOLX = NCOLS(MINDX)
C
      END IF
C
      END
C
C
C
C     ===============================
      SUBROUTINE LRNREF(MINDX,REFNUM)
C     ===============================
C
C
C---- Subroutine to return the current reflection number from an
C     MTZ file opened for read. Files are normally read sequentially,
C     the number returned is the number of the *NEXT* reflection
C     record to be read. If you are going to jump about the file
C     with LRSEEK then use this to record the current position before
C     you start, so that it can be restored afterwards, if required.
C
C
C---- Arguments:
C
C     MINDX     (I)	INTEGER         indicates which MTZ file (up to MFILES
C                               	possible open at once)
C
C     REFNUM    (O)	INTEGER         the reflection record number of the
C                               	next reflection to be read
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,REFNUM
C     ..
C     .. Arrays in Common ..
      REAL RBATW,WRANGE,WSRNGE
      INTEGER HDRST,NBATR,NBATW,NCOLW,NHISTL,NPLABS,NREFR,NREFW,RLUN,
     +        RPOINT,WLUN,WOMBAT,NDATMSS
      LOGICAL SORTB,DATMSS
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT
      CHARACTER LINE*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZWRK/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRNREF : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Return the reflection number
C
        REFNUM = NREFR(MINDX) + 1
C
      END IF
C
      END
C
C
C
C     =============================================
      SUBROUTINE LROPEN(MINDX,FILNAM,IPRINT,IFAIL)
C     =============================================
C
C---- Subroutine to open an MTZ file for read.
C     This should be the first MTZ subroutine called in a set of
C     operations on a pair of files (ie one input and one output).
C     The subroutine zeros all the Header COMMON block entries,
C     opens the specified file and loads the header into the COMMONs,
C     and positions the file ready to read the first data record.
C
C---- Arguments :
C
C     MINDX     (I) 	INTEGER         indicates which MTZ file - 1 index
C                       	        points to both input and output files
C
C     FILNAM    (I)	CHARACTER       name of file to be opened
C
C     IPRINT    (I)	INTEGER         print indicator : meaning :
C     					=0 No MTZ info printed at all
C	                               	=1 Brief header info printed (default)
C	                               	=2 as above plus history info
C        	                       	=3 Full header dump; symmetry
C               	                any other value; nothing happens
C     IFAIL     (O)	INTEGER         error indicator : meaning :
C                               	on exit
C                               	=0 OK
C                               	=-1 no such file (but broken!)
C                               	all other errors cause stop
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MSETS
      PARAMETER (MSETS=MCOLS)
      INTEGER MINCOL
      PARAMETER (MINCOL=3)
      INTEGER NTYP,LTYP
      PARAMETER (NTYP=16,LTYP=9)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER SIZE1
      PARAMETER (SIZE1=20)
      INTEGER NHISLM
      PARAMETER (NHISLM=30)
      INTEGER NPARSE
      PARAMETER (NPARSE=200)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER IFAIL,MINDX,IPRINT
      CHARACTER FILNAM* (*)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS,NSETW,SET_ID,CSET_ID,IDEFSET
      LOGICAL SORTB,DATMSS,VAL_SET
      CHARACTER CBATR*1,CBATW*1,CTYPE*1,LTYPE*1,PGNAM*10,SPGNAM*10,
     +          CLABEL*30,PLABS*30,TITLE*70,HSCR*80,ENTRY_ID*64,
     +          DIFFRN_ID*64
C     ..
C     .. Local Scalars ..
      INTEGER BATFLG,EFLAG,ENDLOP,IER,ISTAT,ITEND,IUNIN,JDO10,JDO55,
     +        JDO100,JDO110,JDO120,JDO130,JDO140,JDO150,JDO190,JDO20,
     +        JDO200,JDO30,JDO40,JDO50,JDO60,JDO80,JDO90,NBATRF,NCOLR,
     +        NITEM,NJUNK,NSYMIN,NTOK,SYFLAG,IRESLT,NSETP,NSETD
      LOGICAL LEND
      CHARACTER KEY*4,MKEY*4,LINE*80,LINE2*400,STROUT*400
C     ..
C     .. Local Arrays ..
      REAL FVALUE(NPARSE)
      INTEGER IBEG(NPARSE),IDEC(NPARSE),IEND(NPARSE),ITYP(NPARSE)
      CHARACTER CTYPES(NTYP)*1,LTYPES(LTYP)*1,CVALUE(NPARSE)*4
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
      EXTERNAL QNAN
C     ..
C     .. External Subroutines ..
      EXTERNAL BLANK,GTPINT,GTPREA,LERROR,LHPRT,LRCLOS,LRHDRL,PARSER,
     +     PUTLIN,QMODE,QOPEN,QSEEK,RBATHD,LSTRSL,SYMFR3, QRARCH,
     +     QREADI, QPRINT, QREADC
C     ..
C     .. Common blocks ..
      COMMON /MTZCHR/TITLE(MFILES),CLABEL(MCOLS,MFILES),
     +       CTYPE(MCOLS,MFILES),SPGNAM(MFILES),LTYPE(MFILES),
     +       CBATR(CBLENG,MBATCH,MFILES),PGNAM(MFILES)
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRC/PLABS(MCOLS,MFILES),HSCR(NHISLM,MFILES),
     +       CBATW(CBLENG,MBATCH,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
      COMMON /MTZHAR/NSETW(MFILES),SET_ID(MSETS,MFILES),
     +       ENTRY_ID(MSETS,MFILES),DIFFRN_ID(MSETS,MFILES),
     +       CSET_ID(MCOLS,MFILES),IDEFSET(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZCHR/,/MTZWRK/,/MTZWRC/
C     ..
C     .. Data statements ..
C  Adding column types to indicate pairs associated with hkl and -h-k-l
C  Need new indicator for F Q; using G L
C                         J Q; using K M
      DATA CTYPES/'H','J','F','D','Q','P','W','A','B','Y','I','R',
     +            'G','K','L','M'/
      DATA LTYPES/'P','A','B','C','I','F','R','H','?'/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE2,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LROPEN : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
C
C            *************************
        CALL LERROR(2,-1,LINE2)
C            ************************
C
      ELSE
C
C---- Then check that nothing is already open for this index
C     If it is continue, but print warning
C     If already open for read, then probably forgot to call LRCLOS
C     If already open for write, then programmer hasn't read manual!
C     but might not be so bad
C     If open for read AND write, then stop
C
        IF (RLUN(MINDX).NE.0) THEN
          IF (WLUN(MINDX).NE.0) THEN
            WRITE (LINE2,FMT='(A,A,I3,A)')
     +        'From LROPEN : Wrong index in call, files already open',
     +        ' for read AND write on index',MINDX,'  pretty serious'
C
C                *************************
            CALL LERROR(2,-1,LINE2)
C                *************************
C
          ELSE
            WRITE (LINE2,FMT='(A,A,I3,A)')
     +        'From LROPEN : Wrong index in call, file already open for'
     +        ,' read on index',MINDX,' all header info will be lost'
C
C                *************************
            CALL LERROR(1,1,LINE2)
C                *************************
C
C---- Get rid of existing file info with LRCLOS
C
C                *************
            CALL LRCLOS(MINDX)
C                *************
C
          END IF
        END IF
C
        IF (WLUN(MINDX).NE.0) THEN
          WRITE (LINE2,FMT='(A,A,I3,A)')
     +      'From LROPEN : Wrong index in call, file already open for',
     +      ' write on index',MINDX,' output header may be corrupted'
C
C              *************************
          CALL LERROR(1,1,LINE2)
C              *************************
C
        END IF
C
C---- Open the file and record the stream, and set mode=bytes
C     The  MTZ header is written with mode=0, except the header
C     offset and the orientation blocks
C     The reflection records are written with mode=2, words/reals
C
C            ************************
        CALL QOPEN(IUNIN,FILNAM,'RO')
C       set up transparent numbers if necessary:
        CALL QRARCH (IUNIN, 2, IRESLT)
        IF (IRESLT.EQ.0) CALL QPRINT(1,
     +       ' WARNING: no architecture information in file --'//
     +       ' assuming native.')
        CALL QMODE(IUNIN,0,NITEM)
C            ************************
C
        RLUN(MINDX) = IUNIN
C
C---- Zero the header arrays etc.
C
        DO 10 JDO10 = 1,6
          CELL(JDO10,MINDX) = 0.0
   10   CONTINUE
C
        NSYM(MINDX) = 0
        NSYMP(MINDX) = 0
C
        DO 40 JDO40 = 1,MAXSYM
          DO 30 JDO30 = 1,4
            DO 20 JDO20 = 1,4
              RSYM(JDO20,JDO30,JDO40,MINDX) = 0.0
   20       CONTINUE
   30     CONTINUE
   40   CONTINUE
C
        NCOLS(MINDX) = 0
        NDATMSS(MINDX) = 0
        NREFS(MINDX) = 0
        NREFR(MINDX) = 0
        NBATCH(MINDX) = 0
        NPLABS(MINDX) = 0
        SRANGE(1,MINDX) = 1.0E-05
        SRANGE(2,MINDX) = 500.0
C  Default values - VAL_MISS(1,..) ( File for reading) is NaN, 
C                   VAL_MISS(2,..) ( File for writing) is Nan
        CALL QNAN(VAL_MISS(1,MINDX))
        CALL QNAN(VAL_MISS(2,MINDX))
        VAL_SET(1,MINDX) = .FALSE.
        VAL_SET(2,MINDX) = .FALSE.
        NHISTL(MINDX) = 0
        HDRST(MINDX) = 0
        NSETW(MINDX) = 0
        IDEFSET(MINDX) = 0
        TITLE(MINDX) = '   '
        NSPGRP(MINDX) = 0
        SPGNAM(MINDX) = '?'
        LTYPE(MINDX) = '?'
        PGNAM(MINDX) = '?'
C
        DO 50 JDO50 = 1,5
          ISORT(JDO50,MINDX) = 0
   50   CONTINUE
C
        DO 55 JDO55 = 1,MSETS
          SET_ID(JDO55,MINDX) = 0
          ENTRY_ID(JDO55,MINDX) = ' '
          DIFFRN_ID(JDO55,MINDX) = ' '
   55   CONTINUE

        DO 60 JDO60 = 1,MCOLS
          CRANGE(1,JDO60,MINDX) = 0.0
          CRANGE(2,JDO60,MINDX) = 0.0
          CSET_ID(JDO60,MINDX) = 0
          CLABEL(JDO60,MINDX) = '   '
          CTYPE(JDO60,MINDX) = ' '
          RPOINT(JDO60,MINDX) = 0
          PLABS(JDO60,MINDX) = ' '
   60   CONTINUE
C
        NBATRF = 0
        NCOLR = 0
        NSETP = 0
        NSETD = 0
        NSYMIN = 0
        SYFLAG = 1
        BATFLG = 1
C
C---- Read the 1st record of the file, check it's an MTZ file,
C     and go to the start of the header records
C     Read in header records with mode=0 (bytes), except offset
C
C            ******************************
        CALL QSEEK(RLUN(MINDX),1,1,1)
        CALL QREADC(RLUN(MINDX),MKEY,IER)
C            ******************************
C
        IF (IER.NE.0) THEN
          LINE2 = ' From LROPEN : Error reading 1st record of MTZ file'
          LINE2(LENSTR(LINE2)+2:) = FILNAM
C
C              *************************
          CALL LERROR(2,-1,LINE2)
C              *************************
        ELSE
          IF (MKEY(1:3).NE.'MTZ') THEN
            LINE2 = ' From LROPEN : File '
            LINE2(LENSTR(LINE2)+2:) = FILNAM(1:LENSTR(FILNAM))
            IF (LENSTR(LINE2) .NE. 399) 
     .                 LINE2(LENSTR(LINE2)+1:) = ' is not an MTZ file'
C
C                *************************
            CALL LERROR(2,-1,LINE2)
C                *************************
C
          ELSE
C
            IF (IPRINT.GT.0) THEN
              WRITE (STROUT,FMT='(A,I3)')
     +          'HEADER INFORMATION FROM INPUT MTZ FILE ON INDEX',MINDX
C
C                  ***********************
              CALL PUTLIN(STROUT,'CURWIN')
              CALL BLANK('CURWIN',1)
C                  ***********************
C
            END IF
C
C                *************************************
            CALL QMODE(RLUN(MINDX),6,NITEM)
            CALL QREADI(RLUN(MINDX),HDRST(MINDX),1,IER)
            CALL QSEEK(RLUN(MINDX),1,HDRST(MINDX),1)
            CALL QMODE(RLUN(MINDX),0,NITEM)
C                *************************************
C
   70       CONTINUE
C
C---- Got this far, so read and parse the header, and load COMMON blocks
C
C---- Start of loop over header lines
C     -------------------------------
C
C                ************************
            CALL LRHDRL(RLUN(MINDX),LINE)
C                ************************
C
            NTOK = NPARSE
C
C---- Pass header line to parser
C
C                *******************************************************
            CALL PARSER(KEY,LINE,IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,NTOK,
     +                  LEND,.FALSE.)
C                *******************************************************
C
C---- And loop over possible keywords
C     -------------------------------
C
C---- VERS - version of routines which wrote the file
C
            IF (KEY.EQ.'VERS') THEN
              IF (LINE(IBEG(2):IEND(2)) .NE. 'MTZ:V1.1')
     +           CALL CCPERR (1,' MTZ version ' // LINE(IBEG(2):IEND(2))
     +     // ' of file incompatible with this version of the software')
              IF (IPRINT.EQ.3) THEN
C
C                    ***********************
                CALL PUTLIN('MTZ file, Version Stamp '//
     +               LINE(IBEG(2):IEND(2)),'CURWIN')
                CALL BLANK('CURWIN',1)
C                    ***********************
C
              END IF
              GO TO 70
C
C---- TITL - read file title
C
            ELSE IF (KEY.EQ.'TITL') THEN
              TITLE(MINDX) = ' '
              IF(NTOK.EQ.1) GO TO 70
              ITEND = IEND(NTOK)
              IF(ITEND .GT.70) ITEND = 70
              TITLE(MINDX) = LINE(IBEG(2) :ITEND)
              GO TO 70
C
C---- NCOL - no. of cols and refls in file
C
            ELSE IF (KEY.EQ.'NCOL') THEN
C
C                  **************************************
              CALL GTPINT(2,NCOLS(MINDX),NTOK,ITYP,FVALUE)
              CALL GTPINT(3,NREFS(MINDX),NTOK,ITYP,FVALUE)
              CALL GTPINT(4,NBATCH(MINDX),NTOK,ITYP,FVALUE)
C                  **************************************
              IF (NBATCH(MINDX).GT.MBATCH) THEN
                 WRITE (LINE2,FMT='(A,I6)')
     +   ' From LROPEN : too many batches in file, maximum is', MBATCH
C
C                     *************************
                 CALL LERROR(2,-1,LINE2)
C                     *************************
              ENDIF
C
              GO TO 70
C
C---- CELL - read in the Cell parameters
C
            ELSE IF (KEY.EQ.'CELL') THEN
C
              DO 80 JDO80 = 1,6
C
C                    *************************************************
             CALL GTPREA(JDO80+1,CELL(JDO80,MINDX),NTOK,ITYP,FVALUE)
C                    *************************************************
C
   80         CONTINUE
C
C---- Set up the S calculation
C
C                  *************************************************
              CALL LSTRSL(MINDX,CELL(1,MINDX),CELL(2,MINDX),
     +                    CELL(3,MINDX),CELL(4,MINDX),CELL(5,MINDX),
     +                    CELL(6,MINDX))
C                  *************************************************
C
              GO TO 70
C
C---- SORT - read in sort order
C
            ELSE IF (KEY.EQ.'SORT') THEN
C
              DO 90 JDO90 = 1,5
C
C                    **************************************************
                CALL GTPINT(JDO90+1,ISORT(JDO90,MINDX),NTOK,ITYP,FVALUE)
C                    **************************************************
C
   90         CONTINUE
              GO TO 70
C
C---- SYMINF - various symmetry stuff
C
            ELSE IF (KEY.EQ.'SYMI') THEN
C
C                  **************************************
              CALL GTPINT(2,NSYM(MINDX),NTOK,ITYP,FVALUE)
              CALL GTPINT(3,NSYMP(MINDX),NTOK,ITYP,FVALUE)
C                  ***************************************
C
              LTYPE(MINDX) = LINE(IBEG(4) :IBEG(4))
C
C                  ***************************************
              CALL GTPINT(5,NSPGRP(MINDX),NTOK,ITYP,FVALUE)
C                  ***************************************
C
              SPGNAM(MINDX) = LINE(IBEG(6) :IEND(6))
              PGNAM(MINDX) = LINE(IBEG(7) :IEND(7))
C
              DO 100 JDO100 = 1,LTYP
                IF (LTYPE(MINDX).EQ.LTYPES(JDO100)) GO TO 70
  100         CONTINUE
C
              WRITE (LINE2,FMT='(A)')
     +     'From LROPEN : Unrecognised Lattice type in header, set to P'
C
C                  *************************
              CALL LERROR(1,1,LINE2)
C                  *************************
C
              LTYPE(MINDX) = 'P'
              GO TO 70
C
C---- SYMM - read in the symmetry cards and convert to matrices
C
            ELSE IF (KEY.EQ.'SYMM') THEN
              IF (SYFLAG.GE.0) THEN
                NSYMIN = NSYMIN + 1
                NJUNK = NSYMIN
                ISTAT = 5
C
C                    *************************************************
                CALL SYMFR3(LINE,ISTAT,NSYMIN,RSYM(1,1,1,MINDX),EFLAG)
C                    *************************************************
C
                IF (EFLAG.GT.0) THEN
                  WRITE (LINE2,FMT='(A,A)')
     +    'From LROPEN : An error occured reading symmetry from header,'
     +              ,' No symmetry information stored'
C
C                      *************************
                  CALL LERROR(1,1,LINE2)
C                      *************************
C
                  NSYM(MINDX) = 0
                  NSYMP(MINDX) = 0
C
                  DO 130 JDO130 = 1,MAXSYM
                    DO 120 JDO120 = 1,4
                      DO 110 JDO110 = 1,4
                        RSYM(JDO110,JDO120,JDO130,MINDX) = 0.0
  110                 CONTINUE
  120               CONTINUE
  130             CONTINUE
C
                  SYFLAG = -1
                END IF
              END IF
              GO TO 70
C
C---- RESO - resolution range
C
            ELSE IF (KEY.EQ.'RESO') THEN
C
C                  ******************************************
              CALL GTPREA(2,SRANGE(1,MINDX),NTOK,ITYP,FVALUE)
              CALL GTPREA(3,SRANGE(2,MINDX),NTOK,ITYP,FVALUE)
C                  ******************************************
C
              GOTO 70
C
C---- VAL_MAGIC - Missing value flag
C---  will either say VAL_MAGIC NAN   or VAL_MAGIC   real
C
            ELSE IF (KEY.EQ.'VALM') THEN
C
C   Second token is NAN -  Call QNAN function.
              IF (ITYP(2).EQ.1)  THEN
                CALL QNAN (VAL_MISS(1,MINDX))
              ELSE
                 CALL GTPREA(2,VAL_MISS(1,MINDX),NTOK,ITYP,FVALUE)
              END IF
              VAL_SET(1,MINDX) = .TRUE.
C  Set output VAL_MISS same as input by default.
              VAL_MISS(2,MINDX) = VAL_MISS(1,MINDX)
C
              GOTO 70
C
C---- COLS - column labels, types and ranges
C
            ELSE IF (KEY.EQ.'COLU') THEN
              NCOLR = NCOLR + 1
              CLABEL(NCOLR,MINDX) = LINE(IBEG(2) :IEND(2))
              CTYPE(NCOLR,MINDX) = LINE(IBEG(3) :IBEG(3))
C
C                  ************************************************
              CALL GTPREA(4,CRANGE(1,NCOLR,MINDX),NTOK,ITYP,FVALUE)
              CALL GTPREA(5,CRANGE(2,NCOLR,MINDX),NTOK,ITYP,FVALUE)
              IF (NTOK.GE.6) 
     +         CALL GTPINT(6,CSET_ID(NCOLR,MINDX),NTOK,ITYP,FVALUE)
C                  ************************************************
C
              DO 140 JDO140 = 1,NTYP
                IF (CTYPE(NCOLR,MINDX).EQ.CTYPES(JDO140)) GO TO 70
  140         CONTINUE
C
              WRITE (LINE2,FMT='(A,I4,A,A,A,A,A)')
     +          'From LROPEN : Column',NCOLR,' Label ',
     +          CLABEL(NCOLR,MINDX),'has unrecognised type (',
     +          CTYPE(NCOLR,MINDX),') - set to R'
C
C                  *************************
              CALL LERROR(1,1,LINE2)
C                  *************************
C
              CTYPE(NCOLR,MINDX) = 'R'
              GO TO 70

C---- Information on datasets used in harvesting

            ELSEIF (KEY.EQ.'NDIF') THEN

              CALL GTPINT(2,NSETW(MINDX),NTOK,ITYP,FVALUE)
              GO TO 70

C---- This works with MTZ files written by current LWCLOS, but
C     might be problems if PROJ or DATA records get out of order.
            ELSEIF (KEY.EQ.'PROJ') THEN
              NSETP = NSETP + 1
              CALL GTPINT(2,SET_ID(NSETP,MINDX),NTOK,ITYP,FVALUE)
              ENTRY_ID(NSETP,MINDX) = LINE(IBEG(3) :IEND(3))
              GO TO 70

            ELSEIF (KEY.EQ.'DATA') THEN
              NSETD = NSETD + 1
              CALL GTPINT(2,SET_ID(NSETD,MINDX),NTOK,ITYP,FVALUE)
              DIFFRN_ID(NSETD,MINDX) = LINE(IBEG(3) :IEND(3))
              GO TO 70
C
C---- BATCH - serial numbers of the batches in the file 
C             (only if NBATCH>0)
C             written 18 numbers per card
C
            ELSE IF (KEY.EQ.'BATC') THEN
              IF (NBATCH(MINDX).NE.0) THEN
                IF (NTOK.LT.13) THEN
                  ENDLOP = NBATRF + NTOK - 1
                ELSE
                  ENDLOP = NBATRF + 12
                END IF
C
                DO 150 JDO150 = NBATRF + 1,ENDLOP
C
C                      ************************************************
                  CALL GTPINT(JDO150-NBATRF+1,BATNUM(JDO150,MINDX),NTOK,
     +                        ITYP,FVALUE)
C                      ************************************************
C
  150           CONTINUE
C
                NBATRF = ENDLOP
              ELSE IF (BATFLG.GT.0) THEN
                WRITE (LINE2,FMT='(A)')
     +      'From LROPEN : Batch information in MTZ header inconsistent'
C
C                    *************************
                CALL LERROR(1,1,LINE2)
C                    *************************
C
                BATFLG = -1
              END IF
              GO TO 70
C
C---- END of header card
C
            ELSE IF (KEY.NE.'END') THEN
C
C---- illegal line, ignore it
C
              WRITE (LINE2,FMT='(A)')
     +      'From LROPEN : Illegal keyword in MTZ header - line ignored'
C
C                  *************************
              CALL LERROR(1,1,LINE2)
              CALL PUTLIN(LINE,'ERRWIN')
              CALL PUTLIN(' ','ERRWIN')
C                  ***********************
C
              GO TO 70
            END IF
C
C---- End of loop over keywords
C     -------------------------
C
C---- Check we have minimum number of columns - should always be true because
C     you can't get out of LWCLOS with less, but check anyway
C
            IF (NCOLS(MINDX).LT.MINCOL) THEN
              WRITE (LINE2,FMT='(A,I4,A,I4)')
     +          'From LROPEN : Header indicates',NCOLS(MINDX),
     +          ' columns, but minimum allowed columns in MTZ file is',
     +          MINCOL
C
C                  *************************
              CALL LERROR(1,1,LINE2)
C                  *************************
C
            END IF
C
C---- Check no. of column cards input equals NCOLS from 1st line
C
            IF (NCOLR.NE.NCOLS(MINDX)) THEN
              WRITE (LINE2,FMT='(A,I4,A,A,I4,A)')
     +          'From LROPEN : Header indicates',NCOLS(MINDX),
     +          ' columns, but ',' there were',NCOLR,
     +          ' column labels in header'
C
C                  *************************
              CALL LERROR(1,1,LINE2)
C                  *************************
C
            END IF
C
C---- Check no. of datasets input equals NSETW from NDIFFRN line
C
            IF (NSETP.NE.NSETW(MINDX)) THEN
              WRITE (LINE2,FMT='(A,I4,A,A,I4,A)')
     +          'From LROPEN : Header indicates',NSETW(MINDX),
     +          ' datasets, but ',' there were',NSETP,
     +          ' PROJECT records in header'
C
C                  *************************
              CALL LERROR(2,-1,LINE2)
C                  *************************
C
            ELSEIF (NSETD.NE.NSETW(MINDX)) THEN
              WRITE (LINE2,FMT='(A,I4,A,A,I4,A)')
     +          'From LROPEN : Header indicates',NSETW(MINDX),
     +          ' datasets, but ',' there were',NSETD,
     +          ' DATASET records in header'
C
C                  *************************
              CALL LERROR(2,-1,LINE2)
C                  *************************
C
            END IF
C
C----Default dataset is that of 1st column (H)
C
            IF (NSETW(MINDX).GT.0) THEN
              IDEFSET(MINDX) = CSET_ID(1,MINDX)
            END IF
C
C---- Check correct number of symmetry cards
C
            IF ((NSYMIN.NE.NSYM(MINDX)) .AND. (SYFLAG.GE.0)) THEN
              WRITE (LINE2,FMT='(A,I3,A,A,I3,A)')
     +          'From LROPEN : Header indicates',NSYM(MINDX),
     +          ' symmetry ops,',' but',NSYMIN,
     +          ' ops were present in header'
C
C                  *************************
              CALL LERROR(1,1,LINE2)
C                  *************************
C
            END IF
C
C---- If BATCH cards in header prepare to read batch headers
C
            IF (NBATCH(MINDX).GT.0) THEN
              IF (NBATRF.NE.NBATCH(MINDX)) THEN
                WRITE (LINE2,FMT='(A,I4,A,I4,A)')
     +            'From LROPEN : Header indicates',NBATCH(MINDX),
     +            ' batches, but',NBATRF,' serial numbers present'
C
C                    *************************
                CALL LERROR(1,1,LINE2)
C                    *************************
C
                NBATCH(MINDX) = NBATRF
              END IF
              NBATR(MINDX) = 0
            END IF
  160       CONTINUE
C
C---- Main header read, continue with any other headers
C
C                ************************
            CALL LRHDRL(RLUN(MINDX),LINE)
C                ************************
C
C---- History header ?
C
            IF (LINE(1:7).EQ.'MTZHIST') THEN
              READ (LINE(9:),FMT='(I3)') NHISTL(MINDX)
C
              DO 170 JDO190 = 1,NHISTL(MINDX)
C
C                    **************************************
                CALL LRHDRL(RLUN(MINDX),HSCR(JDO190,MINDX))
C                    **************************************
C
  170         CONTINUE
              GO TO 160
C
C---- Batch headers
C
            ELSE IF (LINE(1:7).EQ.'MTZBATS') THEN
              IF (NBATCH(MINDX).EQ.0) THEN
                GO TO 190
              ELSE
C
                DO 180 JDO200 = 1,NBATCH(MINDX)
C
C                      **********************************************
                  CALL RBATHD(RLUN(MINDX),NJUNK,RBATR(1,JDO200,MINDX),
     +                        CBATR(1,JDO200,MINDX))
C                      **********************************************
C
  180           CONTINUE
C
                GO TO 160
              END IF
            END IF
C
C---- Absolute end of MTZ file record
C
            IF (LINE(1:15).NE.'MTZENDOFHEADERS') THEN
C
C---- Or there is an error in the file (very unlikely)
C
              WRITE (LINE2,FMT='(A,A)')
     +          'From LROPEN : Unrecognised line in MTZ file header - ',
     +          'file may be corrupt, continuing ...'
C
C                  *************************
              CALL LERROR(1,1,LINE2)
C                  *************************
C
            END IF
            GO TO 200
  190       WRITE (LINE2,FMT='(A,A)')
     +    'From LROPEN : MTZ header indicates that file is standard MTZ'
     +        ,' but there are batch headers present - ignoring them'
C
C                *************************
            CALL LERROR(1,1,LINE2)
C                *************************
C
C---- End of MTZ header processing
C     ============================
C
  200       CONTINUE
C
C---- Output information
C
C                                 ******************
            IF (IPRINT.GT.0) CALL LHPRT(MINDX,IPRINT)
C                                 ******************
C
C---- Position file ready to read reflection records
C     Always call the QMODE=2 before QSEEK - all QSEEKs in MTZLIB are
C     done on words, not bytes
C
C                ******************************
            CALL QMODE(RLUN(MINDX),2,NITEM)
            CALL QSEEK(RLUN(MINDX),1,SIZE1+1,1)
C                ******************************
C
            IFAIL = 0
          END IF
        END IF
      END IF
C
      END
C
C
C
C     ============================
      SUBROUTINE LRHDRL(ILUN,LINE)
C     ============================
C
C---- MTZLIB internal subroutine to read one header record from an MTZ
C     file open for read on unit ILUN, and place the result in the
C     character string LINE.
C
C---- Arguments :
C
C     ILINE	(I)	INTEGER		LUN on which the MTZ file is opened
C
C     LINE	(O)	CHARACTER*(*)	80 Character header line
C
C
C     .. Scalar Arguments ..
      INTEGER ILUN
      CHARACTER LINE* (*)
C     ..
C     .. Local Scalars ..
      INTEGER IER
      CHARACTER ELINE*80
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR,QREADC
C     ..
C          ************************
      CALL QREADC(ILUN,LINE,IER)
C          ************************
      IF (IER.GT.0) THEN
        WRITE (ELINE,FMT='(A,I2)')
     +       'From LRHDRL : Error reading header record from MTZ file, '
     +       // 'want 80 bytes, got ',IER
C            *************************
        CALL LERROR(ISTAT,-1,ELINE)
C            ************************
      END IF
      END
C
C
C
C     ===============================
      SUBROUTINE LRREFM(MINDX,LOGMSS)
C     ===============================
C
C     Returns Logical array which flags missing data entries
C      Array DATMSS set in LRREFF and LRREFL
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER		indicates which MTZ file - 1 index
C     					points to both input and output files
C
C     LOGMSS    (O)	LOGICAL         Array of dimension at least NPLABS(MINDX)
C                                       if LRREFF is being used, or NCOLS(MINDX)
C                                       if LRREFL is being used. In practice,
C                                       dimension MCOLS is safest.
C                                       Contains the logical array LOGMSS on exit
C                                       in order of LRREFF or LRREFL, whichever
C                                       was most recently called.
C                                       IF LOGMSS(..) is TRUE the entry is "missing".
C                                       Maps onto DATMSS.
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX
C     ..
C     .. Array Arguments ..
      LOGICAL LOGMSS(*)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS
      LOGICAL SORTB,DATMSS,VAL_SET
C     ..
C     .. Local Scalars
      INTEGER JDO
      CHARACTER LINE*132
C     ..
C     .. External Routines ..
      EXTERNAL LERROR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MAX
C     .. 
C     .. Common Blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZWRK/
C     ..
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +       'From LRREFM: Index',MINDX,
     +       ' is out of range (allowed 1..',MFILES,')'
        CALL LERROR(2,-1,LINE)
C
C---- Then check that there is a file open
C
      ELSE IF (RLUN(MINDX).EQ.0) THEN
        WRITE (LINE,FMT='(A,I3)')
     +       'From LRREFM : There is no file open for read on index',
     +       MINDX
        CALL LERROR(2,-1,LINE)
      END IF
C     NDATMSS(MINDX) is set as NPLABS(MINDX) or NCOLS(MINDX) in
C     LRREFF and LRREFL respectively.
      DO 20 JDO = 1, NDATMSS(MINDX)
        LOGMSS(JDO) = DATMSS(JDO) 
 20   CONTINUE
      END
C
C
C
C     ========================================
      SUBROUTINE LRREFF(MINDX,RESOL,ADATA,EOF)
C     ========================================
C
C---- BIOMOL-compatible version of the CCP4-subroutine. If the index MINDX
C     is larger then 1000, then it is assumed that the subroutine is
C     called by a BIOMOL program or another program that is capable to
C     interpret the BIOMOL absence flags (e.g. a value of -1.0E+10 indicates
C     absent data). If MINDX is in the normal range of 1 to 3, then the
C     subroutine will convert values of -1.0E+10 to zero to allow normal
C     processing by the CCP4 programs.
C
C---- Subroutine to read a reflection record from an MTZ file which
C     has been opened for read. This returns the record in Lookup order
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER		indicates which MTZ file - 1 index
C     					points to both input and output files
C
C     RESOL     (O)	REAL		resolution (4 * sin**theta/lambda**2)
C     					if 1st 3 cols are not of type H value has
C     					no meaning and is -1.0
C
C     ADATA     (O)	REAL		array of dimension at least NPLABS(MINDX)
C     					containing the reflection record on exit
C     					in lookup order
C
C     EOF       (O)	LOGICAL		End-of-File indicator
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,MINDEX
      REAL RESOL
      LOGICAL EOF
C     ..
C     .. Array Arguments ..
      REAL ADATA(*)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS
      LOGICAL SORTB,DATMSS,VAL_SET
      CHARACTER CBATR*1,CTYPE*1,LTYPE*1,PGNAM*10,SPGNAM*10,
     +          CLABEL*30,TITLE*70
C     ..
C     .. Local Scalars ..
      INTEGER IERR,IFAIL,ISTAT,JDO10,IH,IK,IL
      CHARACTER LINE*400
      REAL RSOL
      LOGICAL BIOMOL
C     ..
C     .. Local Arrays ..
      REAL BDATA(MCOLS)
C     .. 
C     .. Intrinsic Functions
      INTRINSIC ABS
C     ..
C     .. External Functions ..
      REAL LSTLSQ
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR, LSTLSQ, CCPERR, QREADR, CCPBML
C     ..
C     .. Common blocks ..
      COMMON /MTZCHR/TITLE(MFILES),CLABEL(MCOLS,MFILES),
     +       CTYPE(MCOLS,MFILES),SPGNAM(MFILES),LTYPE(MFILES),
     +       CBATR(CBLENG,MBATCH,MFILES),PGNAM(MFILES)
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZCHR/,/MTZWRK/
C     ..
C
C---- Check if the calling program can interpret BIOMOL absence flags
C     Use the variable MINDEX in the remainder of the routine to assure
C     that the value of MINDX in the calling routine is not altered
C
      IF (MINDX.GT.1000) THEN
        BIOMOL = .TRUE.
        MINDEX  = MINDX-1000
      ELSE
        BIOMOL = .FALSE.
        MINDEX  = MINDX
      ENDIF
C
C---- First check that the MINDEX is valid
C
      IF ((MINDEX.LE.0) .OR. (MINDEX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRREFF : Index',MINDEX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
C---- Then check that there is a file open
C
      ELSE IF (RLUN(MINDEX).EQ.0) THEN
        WRITE (LINE,FMT='(A,I3)')
     +    'From LRREFF : There is no file open for read on index',MINDEX
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Everything OK, update the number of records read
C
        NREFR(MINDEX) = NREFR(MINDEX) + 1
C
C---- If not end of file read a record and rearrange it
C
        IF (NREFR(MINDEX).GT.NREFS(MINDEX)) THEN
          EOF = .TRUE.
          NREFR(MINDEX) = NREFR(MINDEX) - 1
        ELSE
C
C              ******************************************
          CALL QREADR(RLUN(MINDEX),BDATA,NCOLS(MINDEX),IERR)
C              ******************************************
C
          IF (IERR.GT.0) THEN
            ISTAT = 2
            IFAIL = -1
            WRITE (LINE,FMT='(A,I4,A,I4)')
     +        'From LRREFF : Error reading file - only',IERR,
     +        ' words transferred, wanted',NCOLS(MINDEX)
C
C                ************************
            CALL LERROR(ISTAT,IFAIL,LINE)
C                ************************
C
          ELSE
C
C---- First calculate resolution, only look for HKL in 1st 3 cols
C     regardless of lookup
C
            IF ((NCOLS(MINDEX).GE.3).AND.(CTYPE(1,MINDEX).EQ.'H')
     +          .AND.(CTYPE(2,MINDEX).EQ.'H')
     +          .AND.(CTYPE(3,MINDEX).EQ.'H')) THEN
C
              IH = BDATA(1)
              IK = BDATA(2)
              IL = BDATA(3)
              RSOL = 4.0 * LSTLSQ(MINDEX,IH,IK,IL)
C
C---- Force resolution (1/d**2) to lie within range stored in header,
C     but print warning if too far away
C
              RESOL = MIN(MAX(RSOL,SRANGE(1,MINDEX)),SRANGE(2,MINDEX))
              IF (ABS(RESOL-RSOL) .GT. 0.000006) THEN
                 WRITE (LINE,FMT='(2A,3I4,2A,F10.6)')
     $              'From LRREFF : S value outside range from header',
     $              ', reflection ',IH,IK,IL,' S =',RSOL
                 ISTAT = 1
                 IFAIL = 0
C                     ************************
                 CALL LERROR(ISTAT,IFAIL,LINE)
C                     ************************
                 CALL CCPERR (2, 'Corrupt MTZ file?')
              ENDIF
C
            ELSE
C
              RESOL = -1.0
C
            END IF
C
C---- Transfer data to ADATA in LOOKUP order, if RPOINT is 0 then
C     presume it was an optional column and set it to 0.0
C
            DO 10 JDO10 = 1,NPLABS(MINDEX)
              DATMSS(JDO10) =.FALSE.
C
              IF (RPOINT(JDO10,MINDEX).GT.0) THEN
                ADATA(JDO10) = BDATA(RPOINT(JDO10,MINDEX))
                IF( (VAL_SET(1,MINDEX)))
     +    CALL IS_MAGIC(VAL_MISS(1,MINDEX),ADATA(JDO10),DATMSS(JDO10))
              ELSE
                ADATA(JDO10) = 0.0
              END IF
C
   10       CONTINUE
            NDATMSS(MINDEX) = NPLABS(MINDEX)
C
C  Prob not nec now??
C---- Set BIOMOL absence flags to zero if the calling program can not
C     interpret them
C
            IF (.NOT. BIOMOL) CALL CCPBML (NPLABS(MINDEX), ADATA)
C
            EOF = .FALSE.
          END IF
        END IF
C
      END IF
C
      END
C
C     ========================================
      SUBROUTINE LRREFL(MINDX,RESOL,ADATA,EOF)
C     ========================================
C
C---- BIOMOL-compatible version of the CCP4-subroutine. If the index MINDX
C     is larger then 1000, then it is assumed that the subroutine is
C     called by a BIOMOL program or another program that is able to
C     interpret the BIOMOL absence flags (e.g. a value of -1.0E+10 indicates
C     absent data). If MINDX is in the normal range of 1 to 3, then the
C     subroutine will convert values of -1.0E+10 to zero to allow normal
C     processing by the CCP4 programs.
C
C---- Subroutine to read a reflection record from an MTZ file which
C     has been opened for read. This returns the record in file order.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     RESOL     (O)	REAL            resolution (4 * sin**theta/lambda**2)
C                               	if 1st 3 cols are not of type H value has
C                               	no meaning and is -1.0
C
C     ADATA     (O)	REAL            array of dimension at least NCOLS(MINDX)
C                               	containing the reflection record on exit
C                               	in file order
C
C     EOF       (O)	LOGICAL         End-of-File indicator
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX, MINDEX
      REAL RESOL
      LOGICAL EOF
C     ..
C     .. Array Arguments ..
      REAL ADATA(*)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS
      LOGICAL SORTB,DATMSS,VAL_SET
      CHARACTER CBATR*1,CTYPE*1,LTYPE*1,PGNAM*10,SPGNAM*10,
     +          CLABEL*30,TITLE*70
C     ..
C     .. Local Scalars ..
      INTEGER IERR,IFAIL,ISTAT,IH,IK,IL,JDO10
      CHARACTER LINE*400
      REAL RSOL
C     .. 
C     .. Intrinsic Functions
      INTRINSIC ABS
      LOGICAL BIOMOL
C     ..
C     .. External Functions ..
      REAL LSTLSQ
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR,QREADR, LSTLSQ, CCPERR, CCPBML
C     ..
C     .. Common blocks ..
      COMMON /MTZCHR/TITLE(MFILES),CLABEL(MCOLS,MFILES),
     +       CTYPE(MCOLS,MFILES),SPGNAM(MFILES),LTYPE(MFILES),
     +       CBATR(CBLENG,MBATCH,MFILES),PGNAM(MFILES)
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZCHR/,/MTZWRK/
C     ..
C
C---- Check if the calling program can interpret BIOMOL absence flags
C     Use the variable MINDEX in the remainder of the routine to assure
C     that the value of MINDX in the calling routine is not altered
C
      IF (MINDX.GT.1000) THEN
        BIOMOL = .TRUE.
        MINDEX  = MINDX-1000
      ELSE
        MINDEX  = MINDX
        BIOMOL = .FALSE.
      ENDIF
C
C---- First check that the MINDEX is valid
C
      IF ((MINDEX.LE.0) .OR. (MINDEX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRREFL : Index',MINDEX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
C---- Then check that there is a file open
C
      ELSE IF (RLUN(MINDEX).EQ.0) THEN
        WRITE (LINE,FMT='(A,I3)')
     +    'From LRREFL : There is no file open for read on index',MINDEX
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C 
C---- Everything OK, update the number of records read
C
        NREFR(MINDEX) = NREFR(MINDEX) + 1
C
C---- If not end of file read a record
C
        IF (NREFR(MINDEX).GT.NREFS(MINDEX)) THEN
          EOF = .TRUE.
          NREFR(MINDEX) = NREFR(MINDEX) - 1
        ELSE
C
C              ******************************************
          CALL QREADR(RLUN(MINDEX),ADATA,NCOLS(MINDEX),IERR)
C              ******************************************
C
          IF (IERR.GT.0) THEN
            ISTAT = 2
            IFAIL = -1
            WRITE (LINE,FMT='(A,I4,A,I4)')
     +        'From LRREFL : Error reading file - only',IERR,
     +        ' words transferred, wanted',NCOLS(MINDEX)
C
C                ************************
            CALL LERROR(ISTAT,IFAIL,LINE)
C                ************************
C
          ELSE
C
C     Check "missing data" flag
            DO 10 JDO10 = 1,NCOLS(MINDEX)
              DATMSS(JDO10) =.FALSE.
              IF ((VAL_SET(1,MINDEX)))
     +             CALL IS_MAGIC(VAL_MISS(1,MINDEX),ADATA(JDO10),
     +             DATMSS(JDO10))
 10         CONTINUE
            NDATMSS(MINDEX) = NCOLS(MINDEX)
C---- Calculate resolution, only look for HKL in 1st 3 cols
C
            IF ((NCOLS(MINDEX).GE.3).AND.(CTYPE(1,MINDEX).EQ.'H')
     +          .AND.(CTYPE(2,MINDEX).EQ.'H')
     +          .AND.(CTYPE(3,MINDEX).EQ.'H')) THEN
C
              IH = ADATA(1)
              IK = ADATA(2)
              IL = ADATA(3)
              RSOL = 4.0 * LSTLSQ(MINDEX,IH,IK,IL)
C
C---- Force resolution (1/d**2) to lie within range stored in header,
C     but print warning if too far away
C
              RESOL = MIN(MAX(RSOL,SRANGE(1,MINDEX)),SRANGE(2,MINDEX))
              IF (ABS(RESOL-RSOL) .GT. 0.000006) THEN
                 WRITE (LINE,FMT='(2A,3I4,A,F10.6)')
     $              'From LRREFL : S value outside range from header',
     $              ', reflection ',IH,IK,IL,' S =',RSOL
                 ISTAT = 1
                 IFAIL = 0
C                     ************************
                 CALL LERROR(ISTAT,IFAIL,LINE)
C                     ************************
                 CALL CCPERR (2, 'Corrupt MTZ file?')
              ENDIF
C
            ELSE
C
              RESOL = -1.0
C
            END IF
C
C---- Set BIOMOL absence flags to zero if the calling program can not
C     interpret them
C
            IF (.NOT. BIOMOL) CALL CCPBML (NCOLS(MINDEX), ADATA)

            EOF = .FALSE.
          END IF
        END IF
C
      END IF
C
      END
C
C
C
C     =========================
      SUBROUTINE LRREWD(MINDX)
C     =========================
C
C --- Subroutine to rewind an MTZ file for re-reading of reflections.
C     The file must already have been opened for read with LROPEN.
C     The file is postioned at the start of the reflection records.
C
C --- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER SIZE1
      PARAMETER (SIZE1=20)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS
      LOGICAL SORTB,DATMSS,VAL_SET
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT
      CHARACTER LINE*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR,QSEEK, LRBRES
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZWRK/,/MTZHDR/
C     ..
C
C --- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRREWD : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      END IF
C
C---- Check that there is a file open
C
      IF (RLUN(MINDX).LE.0) THEN
        WRITE (LINE,FMT='(A,I2)')
     +    'From LRREWD : There is no file open on index',MINDX
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      END IF
C
C --- Is it multi-record file?
C
      IF (NBATCH(MINDX) .GT. 0) THEN
C
C---- Yes, so reset header pointer back to beginning
C
         CALL LRBRES(MINDX, 0)
C
      END IF
C
C --- Position file at start of reflection records
C     and resetno. of refls read counter
C
C          ******************************
      CALL QSEEK(RLUN(MINDX),1,SIZE1+1,1)
C          ******************************
C
      NREFR(MINDX) = 0
C
      END
C
C
C
C     ======================================
      SUBROUTINE LRRSOL(MINDX,MINRES,MAXRES)
C     ======================================
C
C---- Subroutine to return the resolution range for the reflections in the
C     MTZ file open for read on index MINDX.  (Resolution is 1/d-squared).
C     The resolution limits are calculated when the file is written. No 'S'
C     column is required in MTZ files (see also subroutines LRREFF, LRREFL).
C     If MINRES and MAXRES return as 0.0 there is no resolution limits
C     present in the header. This will happend if H,K,L are not in the
C     first 3 columns.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     MINRES    (O)	REAL            minimum resolution for reflections
C                               	in file (smallest number)
C
C     MAXRES    (O)	REAL            maximum resolution for reflections
C                               	in file (largest number)
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX
      REAL MINRES,MAXRES
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RSYM,VAL_MISS
      INTEGER BATNUM,ISORT,NBATCH,NCOLS,NREFS,NSPGRP,NSYM,NSYMP
      LOGICAL VAL_SET
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT
      CHARACTER LINE*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/
C     ..
C
C --- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRRSOL : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE 
C
C---- Return the values
C
        MINRES = SRANGE(1,MINDX)
        MAXRES = SRANGE(2,MINDX)
C
      END IF
C
      END
C
C
C
C     ===============================
      SUBROUTINE LRSEEK(MINDX,REFNUM)
C     ===============================
C
C
C---- Subroutine to move to a specific reflection record in an
C     MTZ file opened for read. Files are normally read sequentially,
C     so this should be viewed as a special case. The file read
C     pointer is positioned so that the next record read with a call
C     to LRREFF or LRREFL will be the one requested.
C
C---- Arguments:
C
C     MINDX     (I)	INTEGER         indicates which MTZ file (up to MFILES
C                               	possible open at once)
C
C     REFNUM    (I)	INTEGER         the reflection record number to which
C                               	to move in the file
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER SIZE1
      PARAMETER (SIZE1=20)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,REFNUM
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS
      LOGICAL SORTB,DATMSS,VAL_SET
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT
      CHARACTER LINE*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR,QSEEK
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZWRK/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRSEEK : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Check requested reflection is in range
C
        IF (REFNUM.GT.NREFS(MINDX)) THEN
          WRITE (LINE,FMT='(A,I7,A,I7,A)') 'From LRSEEK : Bad Argument',
     +      REFNUM,' there are only ',NREFS(MINDX),
     +      ' reflections records in the file'
          ISTAT = 2
          IFAIL = -1
C
C              ************************
          CALL LERROR(ISTAT,IFAIL,LINE)
C              ************************
C
        ELSE
C
C---- Move to start of this record in the file
C
C              **********************************************
          CALL QSEEK(RLUN(MINDX),REFNUM,SIZE1+1,NCOLS(MINDX))
C              **********************************************
C
C---- And update NREFR - file read pointer (or no. of refls read)
C     Is this correct ? Ask Phil. Well no-one's complained so far.
C
          NREFR(MINDX) = REFNUM - 1
C
        END IF
      END IF
C
      END
C
C
C
C     ==============================
      SUBROUTINE LRSORT(MINDX,SORTX)
C     ==============================
C
C
C---- Subroutine to return sort order from header of MTZ file
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     SORTX     (O)	INTEGER         array of dimension (5) containing sort
C                               	order of 1st 5 columns in MTZ file
C                               	negative numbers for descending order
C                               	0 for not sorted
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX
C     ..
C     .. Array Arguments ..
      INTEGER SORTX(5)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RSYM,VAL_MISS
      INTEGER BATNUM,ISORT,NBATCH,NCOLS,NREFS,NSPGRP,NSYM,NSYMP
      LOGICAL VAL_SET
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT,JDO10
      CHARACTER LINE*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRSORT : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Then return the sort order
C
        DO 10 JDO10 = 1,5
          SORTX(JDO10) = ISORT(JDO10,MINDX)
   10   CONTINUE
C
      END IF
C
      END
C
C
C
C     ===========================================================
      SUBROUTINE LRSYMI(MINDX,NSYMPX,LTYPEX,NSPGRX,SPGRNX,PGNAMX)
C     ===========================================================
C
C
C---- Subroutine to return symmetry information (other than symmetry
C     operations) from the MTZ header
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     NSYMPX    (O)	INTEGER         no. of primitive symmetry ops
C
C     LTYPEX    (O)	CHARACTER*1     single character denoting the lattice
C                               	type (possible values are P,A,B,C,I,F,R)
C                               	if blank then not present in header
C
C     NSPGRX    (O)	INTEGER         space group number, 0 if not present
C
C     SPGRNX    (O)	CHARACTER*10    space group name, blank if not present
C
C     PGNAMX    (O)	CHARACTER*10    point group name, blank if not present
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,NSPGRX,NSYMPX
      CHARACTER LTYPEX*1,PGNAMX*10,SPGRNX*10
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RSYM,VAL_MISS
      INTEGER BATNUM,ISORT,NBATCH,NCOLS,NREFS,NSPGRP,NSYM,NSYMP
      CHARACTER CBATR*1,CTYPE*1,LTYPE*1,PGNAM*10,SPGNAM*10,
     +          CLABEL*30,TITLE*70
      LOGICAL VAL_SET
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT
      CHARACTER LINE*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZCHR/TITLE(MFILES),CLABEL(MCOLS,MFILES),
     +       CTYPE(MCOLS,MFILES),SPGNAM(MFILES),LTYPE(MFILES),
     +       CBATR(CBLENG,MBATCH,MFILES),PGNAM(MFILES)
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZCHR/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRSYMI : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Then return the information if it is present
C     NSPGRP is 0 in Common blocks if never set
C     The character variables are all '?' if never set, but return
C     a blank to the calling program
C
        NSYMPX = NSYMP(MINDX)
        LTYPEX = LTYPE(MINDX)
        IF (LTYPEX.EQ.'?') LTYPEX = ' '
        NSPGRX = NSPGRP(MINDX)
        SPGRNX = SPGNAM(MINDX)
        IF (SPGRNX(1:1).EQ.'?') SPGRNX = ' '
        PGNAMX = PGNAM(MINDX)
        IF (PGNAMX(1:1).EQ.'?') PGNAMX = ' '
C
      END IF
C
      END
C
C
C
C     ====================================
      SUBROUTINE LRSYMM(MINDX,NSYMX,RSYMX)
C     ====================================
C
C
C---- Subroutine to return Symmetry operations from header of MTZ file
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     NSYMX     (I/O)	INTEGER         Pn entry, the last dimension of RSYMX;
C                                       on exit the total no. of symmetry
C                                       operations, 0 if no symmetry
C                                       information is present
C
C     RSYMX     (O)	REAL            array of dimensions (4,4,NSYM) of
C                               	symmetry ops on exit (max. NSYM is 192)
C
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,NSYMX
C     ..
C     .. Array Arguments ..
      REAL RSYMX(4,4,*)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RSYM,VAL_MISS
      INTEGER BATNUM,ISORT,NBATCH,NCOLS,NREFS,NSPGRP,NSYM,NSYMP
      LOGICAL VAL_SET
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT,JDO10,JDO20,JDO30
      CHARACTER LINE*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRSYMM : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Copy symmetry ops - if no symm info present in header then nsym=0
C     and the loop will never be executed
C
        NSYMX = NSYM(MINDX)
        DO 30 JDO30 = 1,NSYMX
          DO 20 JDO20 = 1,4
            DO 10 JDO10 = 1,4
              RSYMX(JDO10,JDO20,JDO30) = RSYM(JDO10,JDO20,JDO30,MINDX)
   10       CONTINUE
   20     CONTINUE
   30   CONTINUE
C
      END IF
C
      END
C
C
C
C     ===================================
      SUBROUTINE LRTITL(MINDX,FTITLE,LEN)
C     ===================================
C
C
C---- Subroutine to return the title and it's length from the
C     header of an MTZ file.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     FTITLE    (O)	CHARACTER*(*)   Character string containing the title
C                               	on exit - maximum possible length 70
C
C     LEN       (O)	INTEGER         Length of the title string - ie no. of
C                               	chars from start to last non-blank char
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,LEN
      CHARACTER FTITLE* (*)
C     ..
C     .. Arrays in Common ..
      CHARACTER CBATR*1,CTYPE*1,LTYPE*1,PGNAM*10,SPGNAM*10,
     +          CLABEL*30,TITLE*70
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT
      CHARACTER LINE*400
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZCHR/TITLE(MFILES),CLABEL(MCOLS,MFILES),
     +       CTYPE(MCOLS,MFILES),SPGNAM(MFILES),LTYPE(MFILES),
     +       CBATR(CBLENG,MBATCH,MFILES),PGNAM(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZCHR/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRTITL : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Get actual length of the title  and return title and length
C
        LEN = LENSTR(TITLE(MINDX))
        FTITLE = TITLE(MINDX) (1:LEN)
C
      END IF
C
      END
C
C
C
C     ====================================================
      SUBROUTINE LWASSN(MINDX,LSPRGO,NLPRGO,CTPRGO,IAPPND)
C     ====================================================
C
C
C---- Subroutine to setup the column labels and column types for
C     the output MTZ file. This allows for the user to have specified
C     output column assignments.  Note that this subroutine changes
C     the values in the header common block, so all information about
C     the input files labels and types should already have been
C     extracted.
C
C     If datasets are defined, check all columns have an associated dataset
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     LSPRGO    (I)	CHARACTER*30    array of dimension at least NLPRGO
C                               	containing the program output labels
C
C     NLPRGO    (I)	INTEGER         number of output program labels
C
C     CTPRGO    (I)	CHARACTER*1     array of dimension at least NLPRGO
C                               	containing the output column types
C
C     IAPPND    (I)	INTEGER         =0 replace all existing labels and types
C                               	=1 append to the existing lbls & types
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH,MFILEX
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000,MFILEX=9)
      INTEGER MSETS
      PARAMETER (MSETS=MCOLS)
      INTEGER MINCOL
      PARAMETER (MINCOL=3)
      INTEGER NTYP
      PARAMETER (NTYP=16)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER NHISLM
      PARAMETER (NHISLM=30)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER IAPPND,MINDX,NLPRGO
C     ..
C     .. Array Arguments ..
      CHARACTER*1  CTPRGO(*)
      CHARACTER*30 LSPRGO(*)
C     ..
C     .. Scalars in Common ..
      INTEGER NLUSRI,NLUSRO
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS,NSETW,SET_ID,CSET_ID,IDEFSET
      LOGICAL SORTB,DATMSS,VAL_SET
      CHARACTER CBATR*1,CBATW*1,CTYPE*1,LTYPE*1,PGNAM*10,SPGNAM*10,
     +          CLABEL*30,LSUSRI*30,LSUSRO*30,PLABS*30,TITLE*70,HSCR*80,
     +          ENTRY_ID*64,DIFFRN_ID*64
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ILEN,IOUT,ISTAT,JDO10,JDO30,JDO40,JDO50,JDO60,JDO70,
     +        JDO80,JDO90,LSTART,IREFSET
      CHARACTER LINE*400,CWORK*30
C     ..
C     .. Local Arrays ..
      CHARACTER CTYPES(NTYP)*1,CLABTM(MCOLS)*30, CTYPTM(MCOLS)*1
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL BLANK,LABPRT,LERROR,PUTLIN
C     ..
C     .. Common blocks ..
      COMMON /MTZCHR/TITLE(MFILES),CLABEL(MCOLS,MFILES),
     +       CTYPE(MCOLS,MFILES),SPGNAM(MFILES),LTYPE(MFILES),
     +       CBATR(CBLENG,MBATCH,MFILES),PGNAM(MFILES)
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZLAB/NLUSRI(MFILEX),NLUSRO(MFILEX)
      COMMON /MTZLBC/LSUSRI(MFILEX,MCOLS),LSUSRO(MFILEX,MCOLS)
      COMMON /MTZWRC/PLABS(MCOLS,MFILES),HSCR(NHISLM,MFILES),
     +       CBATW(CBLENG,MBATCH,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
      COMMON /MTZHAR/NSETW(MFILES),SET_ID(MSETS,MFILES),
     +       ENTRY_ID(MSETS,MFILES),DIFFRN_ID(MSETS,MFILES),
     +       CSET_ID(MCOLS,MFILES),IDEFSET(MFILES)
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
      DATA CTYPES/'H','J','F','D','Q','P','W','A','B','Y','I','R',
     +            'G','K','L','M'/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LWASSN : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Work out starting column
C
        IF (IAPPND.EQ.0) THEN
          LSTART = 1
        ELSE
          LSTART = NCOLS(MINDX) + 1
        END IF
C
C---- Work out total number of columns to be written to output file
C
        NCOLW(MINDX) = LSTART + NLPRGO - 1
        IF (NCOLW(MINDX).LT.MINCOL) THEN
          WRITE (LINE,FMT='(A,A,I4)')
     +      'From LWASSN : Not enough output columns -',
     +      ' minimum allowed is',MINCOL
          ISTAT = 2
          IFAIL = -1
C
C              ************************
          CALL LERROR(ISTAT,IFAIL,LINE)
C              ************************
C
        ELSE IF (NCOLW(MINDX).GT.MCOLS) THEN
          WRITE (LINE,FMT='(A,I4)')
     +      'From LWASSN : Too many output columns - maximum allowed is'
     +      ,MCOLS
          ISTAT = 2
          IFAIL = -1
C
C              ************************
          CALL LERROR(ISTAT,IFAIL,LINE)
C              ************************
C
        ELSE
C
C---- Now loop over the new labels, and check the assignments
C
          DO 30 JDO30 = LSTART,NCOLW(MINDX)
            IOUT = JDO30 - LSTART + 1
            IF ((LSUSRO(MINDX,IOUT).NE.' ') .AND. 
     +          (NLUSRO(MINDX).GT.0)) THEN
C
C---- There was a user output assignment, use it
C
              ILEN = LENSTR(LSUSRO(MINDX,IOUT))
              CLABTM(JDO30) = LSUSRO(MINDX,IOUT) (1:ILEN)
              CTYPTM(JDO30) = CTPRGO(IOUT)
            ELSE
C
C---- Check if this column was also an input column, with a user assignment
C
              DO 10 JDO10 = 1,NPLABS(MINDX)
                IF ((LSPRGO(IOUT).EQ.PLABS(JDO10,MINDX)).AND.
     +              (RPOINT(JDO10,MINDX).GT.0)) GO TO 20
   10         CONTINUE
C
C---- Fall through here if no assignments, use output program label
C
              CLABTM(JDO30) = LSPRGO(IOUT)
              CTYPTM(JDO30) = CTPRGO(IOUT)
              GO TO 30
C
C---- Here if using input assignment, but check types match
C
   20         CONTINUE
              CLABTM(JDO30) = CLABEL(RPOINT(JDO10,MINDX),MINDX)
              CTYPTM(JDO30) = CTYPE(RPOINT(JDO10,MINDX),MINDX)
C
C---- Some type checking is done in LRASSN also, but file types not updated
C     if 'R' or blank, this is done here, trust me, this makes sense
C
              IF ((CTYPTM(JDO30).NE.CTPRGO(IOUT)) .AND.
     +            (CTPRGO(IOUT).NE.' ')) THEN 
                IF ((CTYPTM(JDO30).NE.'R') .AND. 
     +              (CTYPTM(JDO30).NE.' ')) THEN
                  ILEN = LENSTR(LSPRGO(IOUT))
                  ISTAT = 1
                  WRITE (LINE,FMT='(10A)')
     +            'From LWASSN : Column type for output program label ',
     +               LSPRGO(IOUT) (1:ILEN),' does not match -',
     +              'file label is ', CLABTM(JDO30)
C
C                      ************************
                  CALL LERROR(ISTAT,IFAIL,LINE)
C                      ************************
                ELSE
                  CTYPTM(JDO30) = CTPRGO(IOUT)
                END IF
              END IF
C
            END IF
   30     CONTINUE
C
C---- Copy the checked labels over, and the (unchecked) types
C
          DO 40 JDO40 = LSTART,NCOLW(MINDX)
            CLABEL(JDO40,MINDX) = CLABTM(JDO40)
            CTYPE(JDO40,MINDX) = CTYPTM(JDO40)
   40     CONTINUE
C
C---- Check that the column labels are not duplicated, stop if they are
C
          DO 60 JDO60 = 1,NCOLW(MINDX)
            CWORK = CLABEL(JDO60,MINDX)(1:LENSTR(CLABEL(JDO60,MINDX)))
            DO 50 JDO50 = 1,NCOLW(MINDX)
              IF ((CWORK.EQ.CLABEL(JDO50,MINDX)).AND.(JDO50.NE.JDO60)) 
     +                                                             THEN
                ISTAT = 2
                IFAIL = -1
                WRITE (LINE,FMT='(A,1X,A,I3,A,I3,A,1X,A)')
     +           'From LWASSN : Duplicate column labels in output file,'
     +           ,'columns ',JDO60,' and ',JDO50,' both have the label'
     +           ,CWORK(1:LENSTR(CWORK))
C
C                    ************************
                CALL LERROR(ISTAT,IFAIL,LINE)
C                    ************************
C
              END IF
   50       CONTINUE
   60     CONTINUE
C
C---- Also check that the new column types are legal
C     If column type is STILL blank, set it to 'R'
C
          DO 80 JDO80 = LSTART,NCOLW(MINDX)
            IF (CTYPE(JDO80,MINDX).NE.' ') THEN
              ISTAT = 1
              DO 70 JDO70 = 1,NTYP
                IF (CTYPE(JDO80,MINDX).EQ.CTYPES(JDO70)) ISTAT = 0
   70         CONTINUE
C
              IF (ISTAT.EQ.1) THEN
                WRITE (LINE,FMT='(A,I4,A)')
     +          'From LWASSN : Unrecognised output column type, column',
     +          JDO80,', set to R'
C
C                    ************************
                CALL LERROR(ISTAT,IFAIL,LINE)
C                    ************************
C
                CTYPE(JDO80,MINDX) = 'R'
              END IF
C
            ELSE
              CTYPE(JDO80,MINDX) = 'R'
            END IF
   80     CONTINUE
C
C---- If datasets are defined, check all columns have an associated dataset
C     If not, set to that of first column dealt with here.
C
          IF (NSETW(MINDX).GT.0) THEN
            IREFSET = IDEFSET(MINDX)
            IF (IREFSET.EQ.0) IREFSET = CSET_ID(1,MINDX)
            IF (IREFSET.EQ.0) IREFSET = SET_ID(1,MINDX)
            DO 90 JDO90 = LSTART,NCOLW(MINDX)
              IF (CSET_ID(JDO90,MINDX).EQ.0) 
     +            CSET_ID(JDO90,MINDX) = IREFSET
 90         CONTINUE
          ENDIF
C
C---- Print out program labels and column labels
C
C              ****************
          CALL PUTLIN('* Output Program Labels :','CURWIN')
          CALL BLANK('CURWIN',1)
          IF (NLPRGO.GT.0) THEN
            CALL LABPRT(LSPRGO,NLPRGO)
          ELSE
            CALL PUTLIN('There were NO output program labels','CURWIN')
          END IF
          CALL BLANK('CURWIN',1)
          CALL PUTLIN('* Output File Labels :','CURWIN')
          CALL BLANK('CURWIN',1)
          CALL LABPRT(CLABEL(1,MINDX),NCOLW(MINDX))
          CALL BLANK('CURWIN',1)
          CALL PUTLIN('* Output File Column Types :','CURWIN')
          CALL BLANK('CURWIN',1)
          CALL LABPRT(CTYPE(1,MINDX),NCOLW(MINDX))
          CALL BLANK('CURWIN',1)
C              *****************
C
        END IF
      END IF
C
      END
C
C
C     ====================================================
      SUBROUTINE LWIDAS(MINDX,NLPRGO,PNAME,DNAME,IAPPND)
C     ====================================================
C
C
C---- Subroutine to associate dataset entry with each column for
C     the output MTZ file. Note that this subroutine changes
C     the values in the header common block, so all information about
C     the input files datasets should already have been
C     extracted.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     NLPRGO    (I)	INTEGER         number of output program labels
C
C     PNAME     (I)	CHARACTER       array of dimension at least NLPRGO
C                               	containing the output project name
C
C     DNAME     (I)	CHARACTER       array of dimension at least NLPRGO
C                               	containing the output dataset name
C
C     IAPPND    (I)	INTEGER         =0 replace all existing column info
C                               	=1 append to the existing column info
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH,MFILEX
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000,MFILEX=9)
      INTEGER MSETS
      PARAMETER (MSETS=MCOLS)
      INTEGER MINCOL
      PARAMETER (MINCOL=3)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER NHISLM
      PARAMETER (NHISLM=30)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER IAPPND,MINDX,NLPRGO
C     ..
C     .. Array Arguments ..
      character*(*) PNAME(*),DNAME(*)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS,NSETW,SET_ID,CSET_ID,IDEFSET
      LOGICAL SORTB,DATMSS,VAL_SET
      CHARACTER ENTRY_ID*64,DIFFRN_ID*64
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT,JDO40,JDO50,IREFSET
      CHARACTER LINE*400,PNAMEL*64,DNAMEL*64
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
      COMMON /MTZHAR/NSETW(MFILES),SET_ID(MSETS,MFILES),
     +       ENTRY_ID(MSETS,MFILES),DIFFRN_ID(MSETS,MFILES),
     +       CSET_ID(MCOLS,MFILES),IDEFSET(MFILES)

C     ..
C     .. Save statement ..
      SAVE
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LWIDAS : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Work out starting column
C
        IF (IAPPND.EQ.0) THEN
          LSTART = 1
        ELSE
          LSTART = NCOLS(MINDX) + 1
        END IF
C
C---- Work out total number of columns to be written to output file
C
        NCOLW(MINDX) = LSTART + NLPRGO - 1
        IF (NCOLW(MINDX).LT.MINCOL) THEN
          WRITE (LINE,FMT='(A,A,I4)')
     +      'From LWIDAS : Not enough output columns -',
     +      ' minimum allowed is',MINCOL
          ISTAT = 2
          IFAIL = -1
C
C              ************************
          CALL LERROR(ISTAT,IFAIL,LINE)
C              ************************
C
        ELSE IF (NCOLW(MINDX).GT.MCOLS) THEN
          WRITE (LINE,FMT='(A,I4)')
     +      'From LWIDAS : Too many output columns - maximum allowed is'
     +      ,MCOLS
          ISTAT = 2
          IFAIL = -1
C
C              ************************
          CALL LERROR(ISTAT,IFAIL,LINE)
C              ************************
C
        ELSE
C
C---- Find dataset ID for each column and store in common block
C
          DO 40 JDO40 = LSTART,NCOLW(MINDX)
            PNAMEL = PNAME(JDO40 - LSTART + 1)
            DNAMEL = DNAME(JDO40 - LSTART + 1)
            
            DO 50 JDO50 = 1,NSETW(MINDX)
              IF (PNAMEL.EQ.ENTRY_ID(JDO50,MINDX) .AND.
     +            DNAMEL.EQ.DIFFRN_ID(JDO50,MINDX)) THEN
                CSET_ID(JDO40,MINDX) = SET_ID(JDO50,MINDX)
                GOTO 40
              ENDIF
 50         CONTINUE

C---- Dataset not found in header common blocks
C     CSET_ID might be set by LWASSN/LWCLAB but do here as well.
            IREFSET = IDEFSET(MINDX)
            IF (IREFSET.EQ.0) IREFSET = CSET_ID(1,MINDX)
            IF (IREFSET.EQ.0) IREFSET = SET_ID(1,MINDX)
            CSET_ID(JDO40,MINDX) = IREFSET

 40       CONTINUE
C
        END IF
      END IF
C
      END
C
C
C
C     ============================================
      SUBROUTINE LWBSCL (MINDX,BATNO,BATSCL,NBATSC)
C     ============================================
C
C---- Subroutine to write batch scale & Bfactor for batch BATNO to
C     batch headers for multi-record file open on index MINDX
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     BATNO     (I)	INTEGER         batch number 
C
C     BATSCL	(I)	REAL            array of dimension NBATSC containing
C					batch scale, relative Bfactor,
C                               	SD(Bscale), SD(Bfactor) if NBATSC = 4
C                                	else NBATSC/2 scales or B-factors + Sd's
C
C     NBATSC    (I)	INTEGER         number of batch scales (=0 to clear)
C
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX, BATNO, NBATSC
C     ..
C     .. Array Arguments ..
      REAL BATSCL(NBATSC)
C     ..
C     .. Common blocks ..
      INTEGER NWORDS,NINTGR,NREALS,IORTYP,LBCELL,MISFLG,
     +     JUMPAX,NCRYST,LCRFLG,LDTYPE,JSCAXS,NBSCAL,NGONAX,LBMFLG,NDET,
     +     LBSETID,INTPAD
      REAL   CELL,UMAT,PHIXYZ,CRYDAT,DATUM,
     +     PHISTT,PHIEND,PHIRANGE,SCANAX,TIME1,TIME2,
     +     BSCALE,BBFAC,SDBSCL,SDBFAC,BATPAD,E1,E2,E3,GONPAD,
     +     SOURCE,S0,BEMDAT,
     +     DX1,THETA1,DETLM1,DX2,THETA2,DETLM2,DETPAD
      CHARACTER BTITLE*70, GONLAB*8, BTITL*94
C     
      COMMON /CBTHDR/BTITLE,GONLAB(3)
C     
      COMMON /MBTHDR/ NWORDS,NINTGR,NREALS,IORTYP,LBCELL(6),MISFLG,
     .     JUMPAX,NCRYST,LCRFLG,LDTYPE,JSCAXS,NBSCAL,NGONAX,LBMFLG,
     +     NDET,LBSETID,INTPAD(8),
     +     CELL(6),UMAT(3,3),PHIXYZ(3,2),CRYDAT(12),DATUM(3),
     +     PHISTT,PHIEND,SCANAX(3),TIME1,TIME2,
     +     BSCALE,BBFAC,SDBSCL,SDBFAC,PHIRANGE,BATPAD(11),
     +     E1(3),E2(3),E3(3),
     +     GONPAD(12),SOURCE(3),S0(3),BEMDAT(25),
     +     DX1,THETA1,DETLM1(2,2),DX2,THETA2,DETLM2(2,2),DETPAD(33)
      SAVE /CBTHDR/, /MBTHDR/
      EXTERNAL LRBRES, LRBAT, LWBAT
C     
C*** Equivalence undetermined number of scale factors to BSCALE
      REAL SCALES(16)
      EQUIVALENCE (SCALES(1),BSCALE)
      REAL RNWRDS(1)
      EQUIVALENCE (NWORDS, RNWRDS)
C     ..
C     .. Local Scalars ..
      INTEGER JDO10
C
C--- Set read pointer to this batch and read orientation block
C
C          ****************************************
      CALL LRBRES(MINDX,BATNO)
C     NB the original of this had BTITLE in the call, but it's in common
C     and updated during the call, so illegal
      BTITL = BTITLE
      CALL LRBAT(MINDX,BATNO,RNWRDS,BTITL,0)
      BTITLE = BTITL
C          ****************************************
C
      IF (NBATSC .EQ. 0) THEN
         NBSCAL = 0
      ELSE
C
C--- Copy scales
C
         NBSCAL = NBATSC
         DO 10, JDO10=1,NBATSC
            SCALES(JDO10) = BATSCL(JDO10)
 10      CONTINUE
C
      ENDIF
C
C--- Write Orientation block
C
      CALL LWBAT(MINDX,BATNO,RNWRDS,BTITLE)
C
      RETURN
C
      END
C
C
C     ===========================================================
      SUBROUTINE LWBSETID (MINDX,BATNO,PNAME,DNAME)
C     ===========================================================
C
C---- Subroutine to write dataset ID for batch BATNO to
C     batch headers for multi-record file open on index MINDX
C     If LWBAT has been used, the batch headers in RBATW are used,
C     else those in RBATR are used.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     BATNO     (I)	INTEGER         batch number 
C
C     PNAME     (I)	CHARACTER       project name of dataset
C
C     DNAME     (I)	CHARACTER       dataset name of dataset
C
C     ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
      INTEGER MSETS
      PARAMETER (MSETS=MCOLS)
      INTEGER MBLENG, MBLINT, MBLREA
      PARAMETER (MBLENG=185, MBLINT=29, MBLREA=156)

C     .. Scalar Arguments ..
      INTEGER MINDX, BATNO
      CHARACTER*(*) PNAME,DNAME
C     ..
C     .. Common blocks ..
      INTEGER NWORDS,NINTGR,NREALS,IORTYP,LBCELL,MISFLG,
     +     JUMPAX,NCRYST,LCRFLG,LDTYPE,JSCAXS,NBSCAL,NGONAX,LBMFLG,NDET,
     +     LBSETID,INTPAD
      REAL  BCELL,UMAT,PHIXYZ,CRYDAT,DATUM,
     +     PHISTT,PHIEND,PHIRANGE,SCANAX,TIME1,TIME2,
     +     BSCALE,BBFAC,SDBSCL,SDBFAC,BATPAD,E1,E2,E3,GONPAD,
     +     SOURCE,S0,BEMDAT,
     +     DX1,THETA1,DETLM1,DX2,THETA2,DETLM2,DETPAD
      CHARACTER BTITLE*70, GONLAB*8
C     
      COMMON /CBTHDR/BTITLE,GONLAB(3)
C     
      COMMON /MBTHDR/ NWORDS,NINTGR,NREALS,IORTYP,LBCELL(6),MISFLG,
     .     JUMPAX,NCRYST,LCRFLG,LDTYPE,JSCAXS,NBSCAL,NGONAX,LBMFLG,
     +     NDET,LBSETID,INTPAD(8),
     +     BCELL(6),UMAT(3,3),PHIXYZ(3,2),CRYDAT(12),DATUM(3),
     +     PHISTT,PHIEND,SCANAX(3),TIME1,TIME2,
     +     BSCALE,BBFAC,SDBSCL,SDBFAC,PHIRANGE,BATPAD(11),
     +     E1(3),E2(3),E3(3),
     +     GONPAD(12),SOURCE(3),S0(3),BEMDAT(25),
     +     DX1,THETA1,DETLM1(2,2),DX2,THETA2,DETLM2(2,2),DETPAD(33)
      SAVE /CBTHDR/, /MBTHDR/
      EXTERNAL LRBRES, LRBAT, LWBAT
C
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS
      LOGICAL SORTB,DATMSS,VAL_SET
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)

      INTEGER NSETW,SET_ID,CSET_ID,IDEFSET
      CHARACTER ENTRY_ID*64,DIFFRN_ID*64
      COMMON /MTZHAR/NSETW(MFILES),SET_ID(MSETS,MFILES),
     +       ENTRY_ID(MSETS,MFILES),DIFFRN_ID(MSETS,MFILES),
     +       CSET_ID(MCOLS,MFILES),IDEFSET(MFILES)
C
C.. Local variables
      INTEGER BSETID
      CHARACTER LINE*400

      REAL RNWRDS(MBLENG)
      EQUIVALENCE (NWORDS, RNWRDS)
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LWBSETID : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ENDIF
C
C---- Find dataset ID corresponding to PNAME/DNAME
C     These datasets should have been read in from input MTZ
C     file or appended using LWID.
C
      DO 50 JDO50 = 1,NSETW(MINDX)
        IF (PNAME.EQ.ENTRY_ID(JDO50,MINDX) .AND.
     +      DNAME.EQ.DIFFRN_ID(JDO50,MINDX)) THEN
          BSETID = SET_ID(JDO50,MINDX)
          GOTO 40
        ENDIF
 50   CONTINUE

      CALL CCPERR(2,' Failed to set dataset ID in batch header.')
      RETURN

 40   CONTINUE

C--- Batches written with LWBAT
      IF (NBATW(MINDX).GT.0) THEN

        DO 10 I = 1,NBATW(MINDX)
         IF (BATNO.EQ.WOMBAT(I,MINDX)) THEN

          DO 15 J = 1,MBLENG
            RNWRDS(J) = RBATW(J,I,MINDX)
 15       CONTINUE
          LBSETID = BSETID
          DO 18 J = 1,MBLENG
            RBATW(J,I,MINDX) = RNWRDS(J) 
 18       CONTINUE
          GOTO 100

         ENDIF
 10     CONTINUE

C--- Batches from input file
      ELSEIF (NBATCH(MINDX).GT.0) THEN

        DO 20 I = 1,NBATCH(MINDX)
         IF (BATNO.EQ.BATNUM(I,MINDX)) THEN

          DO 25 J = 1,MBLENG
            RNWRDS(J) = RBATR(J,I,MINDX)
 25       CONTINUE
          LBSETID = BSETID
          DO 28 J = 1,MBLENG
            RBATR(J,I,MINDX) = RNWRDS(J) 
 28       CONTINUE
          GOTO 100

         ENDIF
 20     CONTINUE

      ELSE
        WRITE (LINE,FMT='(A)') 
     +    'From LWBSETID : no batches present to write to!'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ENDIF

 100  CONTINUE

      RETURN
      END
C
C     ============================================
      SUBROUTINE LRBSETID (MINDX,BATNO,BSETID)
C     ============================================
C
C---- Subroutine to read dataset ID for batch BATNO from
C     batch headers for multi-record file open on index MINDX
C     If LWBAT has been used, the batch headers in RBATW are used,
C     else those in RBATR are used.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     BATNO     (I)	INTEGER         batch number 
C
C     BSETID    (O)	INTEGER         dataset ID for batch
C
C     ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
      INTEGER MSETS
      PARAMETER (MSETS=MCOLS)
      INTEGER MBLENG, MBLINT, MBLREA
      PARAMETER (MBLENG=185, MBLINT=29, MBLREA=156)

C     .. Scalar Arguments ..
      INTEGER MINDX, BATNO, BSETID
C     ..
C     .. Common blocks ..
      INTEGER NWORDS,NINTGR,NREALS,IORTYP,LBCELL,MISFLG,
     +     JUMPAX,NCRYST,LCRFLG,LDTYPE,JSCAXS,NBSCAL,NGONAX,LBMFLG,NDET,
     +     LBSETID,INTPAD
      REAL  BCELL,UMAT,PHIXYZ,CRYDAT,DATUM,
     +     PHISTT,PHIEND,PHIRANGE,SCANAX,TIME1,TIME2,
     +     BSCALE,BBFAC,SDBSCL,SDBFAC,BATPAD,E1,E2,E3,GONPAD,
     +     SOURCE,S0,BEMDAT,
     +     DX1,THETA1,DETLM1,DX2,THETA2,DETLM2,DETPAD
      CHARACTER BTITLE*70, GONLAB*8
C     
      COMMON /CBTHDR/BTITLE,GONLAB(3)
C     
      COMMON /MBTHDR/ NWORDS,NINTGR,NREALS,IORTYP,LBCELL(6),MISFLG,
     .     JUMPAX,NCRYST,LCRFLG,LDTYPE,JSCAXS,NBSCAL,NGONAX,LBMFLG,
     +     NDET,LBSETID,INTPAD(8),
     +     BCELL(6),UMAT(3,3),PHIXYZ(3,2),CRYDAT(12),DATUM(3),
     +     PHISTT,PHIEND,SCANAX(3),TIME1,TIME2,
     +     BSCALE,BBFAC,SDBSCL,SDBFAC,PHIRANGE,BATPAD(11),
     +     E1(3),E2(3),E3(3),
     +     GONPAD(12),SOURCE(3),S0(3),BEMDAT(25),
     +     DX1,THETA1,DETLM1(2,2),DX2,THETA2,DETLM2(2,2),DETPAD(33)
      SAVE /CBTHDR/, /MBTHDR/
      EXTERNAL LRBRES, LRBAT, LWBAT
C
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS
      LOGICAL SORTB,DATMSS,VAL_SET
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
      SAVE /MTZHDR/, /MTZWRK/

      INTEGER NSETW,SET_ID,CSET_ID,IDEFSET
      CHARACTER ENTRY_ID*64,DIFFRN_ID*64
      COMMON /MTZHAR/NSETW(MFILES),SET_ID(MSETS,MFILES),
     +       ENTRY_ID(MSETS,MFILES),DIFFRN_ID(MSETS,MFILES),
     +       CSET_ID(MCOLS,MFILES),IDEFSET(MFILES)
C
C.. Local variables
      CHARACTER LINE*400

      REAL RNWRDS(MBLENG)
      EQUIVALENCE (NWORDS, RNWRDS)

C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRBSETID : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ENDIF

      LBSETID = 0

C--- Batches written with LWBAT
      IF (NBATW(MINDX).GT.0) THEN

        DO 10 I = 1,NBATW(MINDX)
         IF (BATNO.EQ.WOMBAT(I,MINDX)) THEN

          DO 15 J = 1,MBLENG
            RNWRDS(J) = RBATW(J,I,MINDX)
 15       CONTINUE
          GOTO 100

         ENDIF
 10     CONTINUE

C--- Batches from input file
      ELSEIF (NBATCH(MINDX).GT.0) THEN

        DO 20 I = 1,NBATCH(MINDX)
         IF (BATNO.EQ.BATNUM(I,MINDX)) THEN

          DO 25 J = 1,MBLENG
            RNWRDS(J) = RBATR(J,I,MINDX)
 25       CONTINUE
          GOTO 100

         ENDIF
 20     CONTINUE

      ELSE
        WRITE (LINE,FMT='(A)') 
     +    'From LRBSETID : no batches present to read!'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ENDIF

 100  CONTINUE
      
      BSETID = LBSETID

      RETURN
C
      END
C
C
C
C     =====================================
      SUBROUTINE LWBTIT (MINDX,BATNO,TBATCH)
C     =====================================
C
C---- Subroutine to write batch title & dummy orientation block for
C     batch BATNO to multi-record file open
C     on index MINDX
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     BATNO     (I)	INTEGER         batch number 
C
C     TBATCH    (I)	CHARACTER       batch title
C
C ... Arguments
      INTEGER MINDX, BATNO
      CHARACTER*(*) TBATCH
C
C Orientation block (dummy)
C Lengths: MBLENG is total length of block
C  
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER RBATCH(MBLENG)
      CHARACTER*94 BTITLE
      REAL BATCH(1)
      EXTERNAL LWBAT
      EQUIVALENCE (BATCH,RBATCH)
C
      DATA RBATCH/MBLENG*0/
C
C Copy title
      BTITLE = TBATCH
C
C Set dummy lengths (strictly illegal, setting BATCH)
      RBATCH(1) = MBLENG
      RBATCH(2) = MBLENG
      RBATCH(3) = 0
C
C Write batch header
      CALL LWBAT(MINDX,BATNO,BATCH,BTITLE)
C
      END
C
C
C
C     ==============================
      SUBROUTINE LWCELL(MINDX,CELLP)
C     ==============================
C
C
C---- Subroutine to write Cell Parameters into the header common block
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     CELLP     (I)	REAL            array of dimension (6) containing cell
C                               	parameters to write to header
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX
C     ..
C     .. Array Arguments ..
      REAL CELLP(6)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS
      LOGICAL SORTB,DATMSS,VAL_SET
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT,JDO10
      CHARACTER LINE*400
      LOGICAL CELLCH
C     .. 
C     .. Intrinsic Functions
      INTRINSIC ABS
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR,LSTRSL
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZWRK/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LWCELL : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Then update the header block
C
        CELLCH = .FALSE.
        DO 10 JDO10 = 1,6
          IF (ABS(CELL(JDO10,MINDX) - CELLP(JDO10)).GT.0.000006) 
     +      CELLCH=.TRUE.
          CELL(JDO10,MINDX) = CELLP(JDO10)
   10   CONTINUE
C
C---- Set S range to dummy values - no easy way to correct them
C
        SRANGE(1,MINDX) = 1.0E-05
        SRANGE(2,MINDX) = 500.0
C
C---- Print warning is this is done after reflections have been written
C
        IF ((NREFW(MINDX).GT.0).AND. CELLCH) THEN
          WRITE (LINE,FMT='(A,A,A)')
     +      'From LWCELL : You are changing the cell after you have',
     +      'written reflections to file - ',
     +      'resolution limits will be wrong'
          ISTAT = 1
C
C              ************************
          CALL LERROR(ISTAT,IFAIL,LINE)
C              ************************
C
        END IF
C
C---- And setup again for the S calculation
C
C            ******************************************************
        CALL LSTRSL(MINDX,CELL(1,MINDX),CELL(2,MINDX),CELL(3,MINDX),
     +              CELL(4,MINDX),CELL(5,MINDX),CELL(6,MINDX))
C            ******************************************************

      END IF
C
      END
C
C
C
C     ====================================================
      SUBROUTINE LRID(MINDX,PNAME,DNAME,ISETS,NDATASETS)
C     ====================================================
C
C---- Subroutine to return information for all datasets from the MTZ file
C     header.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     PNAME     (O)     CHARACTER       array of dimension at least NDATASETS
C                               	containing the project name on exit
C
C     DNAME     (O)     CHARACTER       array of dimension at least NDATASETS
C                               	containing the dataset name on exit
C
C     ISETS     (O)     INTEGER         array of dimension at least NDATASETS
C                               	containing the dataset id on exit
C
C     NDATASETS (O)     INTEGER         number of datasets in MTZ header
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS
      PARAMETER (MFILES=4,MCOLS=200)
      INTEGER MSETS
      PARAMETER (MSETS=MCOLS)
C     ..
C     .. Scalar Arguments ..
      INTEGER NDATASETS,MINDX,ISETS(*)
      CHARACTER*(*) PNAME(*),DNAME(*)
C     ..
C     .. Local Scalars ..
      INTEGER ISET,ISTAT,IFAIL
      CHARACTER LINE*400
C     ..
C     .. Arrays in Common ..
      INTEGER NSETW,SET_ID,CSET_ID,IDEFSET
      CHARACTER ENTRY_ID*64,DIFFRN_ID*64

      COMMON /MTZHAR/NSETW(MFILES),SET_ID(MSETS,MFILES),
     +       ENTRY_ID(MSETS,MFILES),DIFFRN_ID(MSETS,MFILES),
     +       CSET_ID(MCOLS,MFILES),IDEFSET(MFILES)
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRID : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ENDIF

C     New dataset to be added to header
      NDATASETS = NSETW(MINDX)
      DO 10 ISET = 1,NDATASETS
        PNAME(ISET) = ENTRY_ID(ISET,MINDX)
        DNAME(ISET) = DIFFRN_ID(ISET,MINDX)
        ISETS(ISET) = SET_ID(ISET,MINDX)
 10   CONTINUE

      END
C
C
C     ====================================================
      SUBROUTINE LWID(MINDX,PROJECT_NAME,DATASET_NAME)
C     ====================================================
C
C---- Subroutine to add dataset information to the output MTZ file header.
C     Datasets identified by the PROJECT_NAME/DATASET_NAME pair are 
C     appended to the MTZ header one at a time.
C     Checks to see if the PROJECT_NAME/DATASET_NAME pair is already
C     included; if so, the dataset is not appended.
C     Redundant datasets are removed in LWCLOS.
C
C---- Arguments :
C
C     MINDX         (I)	    INTEGER        indicates which MTZ file - 1 index
C                                          points to both input and output files
C
C     PROJECT_NAME  (I)     CHARACTER      project name of dataset to be added
C                                          (strings longer than 64 will be truncated)
C
C     DATASET_NAME  (I)     CHARACTER      dataset name of dataset to be added
C                                          (strings longer than 64 will be truncated)
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS
      PARAMETER (MFILES=4,MCOLS=200)
      INTEGER MSETS
      PARAMETER (MSETS=MCOLS)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX
      CHARACTER PROJECT_NAME*(*),DATASET_NAME*(*)
C     ..
C     .. Local Scalars ..
      INTEGER ISET,ISTAT,IFAIL,JDO50,MAXSETID
      CHARACTER LINE*400
C     ..
C     .. Arrays in Common ..
      INTEGER NSETW,SET_ID,CSET_ID,IDEFSET
      CHARACTER ENTRY_ID*64,DIFFRN_ID*64

      COMMON /MTZHAR/NSETW(MFILES),SET_ID(MSETS,MFILES),
     +       ENTRY_ID(MSETS,MFILES),DIFFRN_ID(MSETS,MFILES),
     +       CSET_ID(MCOLS,MFILES),IDEFSET(MFILES)
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LWID : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ENDIF

      MAXSETID = 0
C     Check whether this project/dataset already exists.
      DO 50 JDO50 = 1,NSETW(MINDX)
        IF (PROJECT_NAME(1:LENSTR(PROJECT_NAME)).EQ.
     +    ENTRY_ID(JDO50,MINDX) .AND. 
     +    DATASET_NAME(1:LENSTR(DATASET_NAME)).EQ.
     +    DIFFRN_ID(JDO50,MINDX)) RETURN
        IF (SET_ID(JDO50,MINDX).GT.MAXSETID)
     +    MAXSETID = SET_ID(JDO50,MINDX)
 50   CONTINUE

C     Check if PROJECT_NAME / DATASET_NAME are too long.
      IF (LENSTR(PROJECT_NAME).GT.64)
     +  PROJECT_NAME = PROJECT_NAME(1:64)
      IF (LENSTR(DATASET_NAME).GT.64)
     +  DATASET_NAME = DATASET_NAME(1:64)

C     New dataset to be added to header
      NSETW(MINDX) = NSETW(MINDX) + 1
      ISET = NSETW(MINDX)
      SET_ID(ISET,MINDX) = MAXSETID + 1
      ENTRY_ID(ISET,MINDX) = PROJECT_NAME(1:LENSTR(PROJECT_NAME))
      DIFFRN_ID(ISET,MINDX) = DATASET_NAME(1:LENSTR(DATASET_NAME))

      END
C
C
C
C     ====================================================
      SUBROUTINE LWCLAB(MINDX,LSPRGO,NLPRGO,CTPRGO,IAPPND)
C     ====================================================
C
C
C---- Subroutine to write the column labels and column types
C     to the header of an output MTZ file. This is simpler than LWASSN
C     as it doesn't look for column assignments and doesn't check
C     back to the input file at all - so the output column labels
C     are exactly what come into this subroutine in CLABS.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     LSPRGO    (I)	CHARACTER*30    array of dimension at least NLPRGO
C                               	containing the column labels on entry
C
C     NLPRGO    (I)	INTEGER         number of columns input
C
C     CTPRGO    (I)	CHARACTER*1     array of dimension at least NLPRGO
C                               	containing the column types on entry
C
C     IAPPND    (I)	INTEGER         =0 replace all existing labels and types
C                               	=1 append to the existing lbls & types
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MSETS
      PARAMETER (MSETS=MCOLS)
      INTEGER MINCOL
      PARAMETER (MINCOL=3)
      INTEGER NTYP
      PARAMETER (NTYP=16)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER IAPPND,MINDX,NLPRGO
C     ..
C     .. Array Arguments ..
      CHARACTER*1  CTPRGO(*)
      CHARACTER*30 LSPRGO(*)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS,NSETW,SET_ID,CSET_ID,IDEFSET
      LOGICAL SORTB,DATMSS,VAL_SET
      CHARACTER CBATR*1,CTYPE*1,LTYPE*1,PGNAM*10,SPGNAM*10,
     +          CLABEL*30,TITLE*70,ENTRY_ID*64,DIFFRN_ID*64
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,IOUT,ISTAT,JDO10,JDO20,JDO30,JDO40,JDO50,JDO60,
     +        LSTART,IREFSET
      CHARACTER LINE*400,CWORK*30
C     ..
C     .. Local Arrays ..
      CHARACTER CTYPES(NTYP)*1
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZCHR/TITLE(MFILES),CLABEL(MCOLS,MFILES),
     +       CTYPE(MCOLS,MFILES),SPGNAM(MFILES),LTYPE(MFILES),
     +       CBATR(CBLENG,MBATCH,MFILES),PGNAM(MFILES)
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
      COMMON /MTZHAR/NSETW(MFILES),SET_ID(MSETS,MFILES),
     +       ENTRY_ID(MSETS,MFILES),DIFFRN_ID(MSETS,MFILES),
     +       CSET_ID(MCOLS,MFILES),IDEFSET(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZCHR/,/MTZWRK/
C     ..
C     .. Data statements ..
      DATA CTYPES/'H','J','F','D','Q','P','W','A','B','Y','I','R',
     +            'G','K','L','M'/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LWCLAB : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Work out starting column
C
        IF (IAPPND.EQ.0) THEN
          LSTART = 1
        ELSE
          LSTART = NCOLS(MINDX) + 1
        END IF
C
C---- Work out total number of columns to be written to output file
C
        NCOLW(MINDX) = LSTART + NLPRGO - 1
        IF (NCOLW(MINDX).LT.MINCOL) THEN
          WRITE (LINE,FMT='(A,A,I4)')
     +      'From LWCLAB : Not enough output columns -',
     +      ' minimum allowed is',MINCOL
          ISTAT = 2
          IFAIL = -1
C
C              ************************
          CALL LERROR(ISTAT,IFAIL,LINE)
C              ************************
C
        ELSE IF (NCOLW(MINDX).GT.MCOLS) THEN
          WRITE (LINE,FMT='(A,I4)')
     +      'From LWCLAB : Too many output columns - maximum allowed is'
     +      ,MCOLS
          ISTAT = 2
          IFAIL = -1
C
C              ************************
          CALL LERROR(ISTAT,IFAIL,LINE)
C              ************************
C
        ELSE
C
C---- Then copy column information
C
          DO 10 JDO10 = LSTART,NCOLW(MINDX)
            IOUT = JDO10 - LSTART + 1
            ISTAT = LENSTR(LSPRGO(IOUT))
            CLABEL(JDO10,MINDX) = LSPRGO(IOUT) (1:ISTAT)
            CTYPE(JDO10,MINDX) = CTPRGO(IOUT)
   10     CONTINUE
C
C---- Check that the column labels are not duplicated, stop if they are
C
          DO 30 JDO30 = 1,NCOLW(MINDX)
            CWORK = CLABEL(JDO30,MINDX)(1:LENSTR(CLABEL(JDO30,MINDX)))
            DO 20 JDO20 = 1,NCOLW(MINDX)
              IF ((CWORK.EQ.CLABEL(JDO20,MINDX)).AND.(JDO20.NE.JDO30)) 
     +                                                             THEN
                ISTAT = 2
                IFAIL = -1
                WRITE (LINE,FMT='(A,1X,A,I3,A,I3,A,1X,A)')
     +           'From LWCLAB : Duplicate column labels in output file,'
     +           ,'columns ',JDO30,' and ',JDO20,' both have the label'
     +           ,CWORK(1:LENSTR(CWORK))
C
C                     ************************
                 CALL LERROR(ISTAT,IFAIL,LINE)
C                     ************************
C
              END IF
   20       CONTINUE
   30     CONTINUE
C
C---- Also check that the new column types are legal
C     If blank, set to 'R'
C
          DO 50 JDO50 = LSTART,NCOLW(MINDX)
            IF (CTYPE(JDO50,MINDX).NE.' ') THEN
              ISTAT = 1
              DO 40 JDO40 = 1,NTYP
                IF (CTYPE(JDO50,MINDX).EQ.CTYPES(JDO40)) ISTAT = 0
   40         CONTINUE
C
              IF (ISTAT.EQ.1) THEN
                WRITE (LINE,FMT='(A,I4,A)')
     +          'From LWCLAB : Unrecognised output column type, column',
     +          JDO50,', set to R'
C
C                    ************************
                CALL LERROR(ISTAT,IFAIL,LINE)
C                    ************************
C
                CTYPE(JDO50,MINDX) = 'R'
              END IF
            ELSE
              CTYPE(JDO50,MINDX) = 'R'
            END IF
C
   50     CONTINUE
C
C---- If datasets are defined, check all columns have an associated dataset
C     If not, set to that of first column dealt with here.
C
          IF (NSETW(MINDX).GT.0) THEN
            IREFSET = IDEFSET(MINDX)
            IF (IREFSET.EQ.0) IREFSET = CSET_ID(1,MINDX)
            IF (IREFSET.EQ.0) IREFSET = SET_ID(1,MINDX)
            DO 60 JDO60 = LSTART,NCOLW(MINDX)
              IF (CSET_ID(JDO60,MINDX).EQ.0) 
     +            CSET_ID(JDO60,MINDX) = IREFSET
 60         CONTINUE
          ENDIF

        END IF
      END IF
C
      END
C
C
C
C     ============================================
      SUBROUTINE SET_MAGIC(MINDX,VAL_MAGIC,SETVAL)
C     ============================================
C
C
C---- Subroutine to pass the "magic" value for this mtz file 
C     either into the file or back to the calling program
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     VAL_MAGIC (I)	REAL            variable giving value assigned to 
C                                       "missing data"; it may be passed to 
C                                        an MTZ  file, or return a preset value.
C
C     SETVAL    (I)	LOGICAL         if TRUE on entry, the mtz missing flag is 
C                                       set to VAL_MAGIC - ie the value in the mtz 
C                                       file is OVERWRITTEN!
C
C                                       if FALSE on entry and there is a 
C                                       "missing value" set in the input MTZ file 
C                                       that will be returned as VAL_MAGIC, and
C                                       SETVAL will be returned TRUE.
C
C                                       if FALSE on entry and there is NO 
C                                       "missing value" set in the input MTZ file 
C                                       VAL_MAGIC will be set to the default
C                                       for both input and output
C                                       SETVAL returned TRUE
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX
      REAL VAL_MAGIC
      LOGICAL SETVAL
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS
      LOGICAL SORTB,DATMSS,VAL_SET
C     ..
C     .. Local Scalars ..
      CHARACTER LINE*132
C     ..
C     .. External Functions ..
      LOGICAL QISNAN
      EXTERNAL QISNAN
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR, QNAN
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZWRK/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From SET_MAGIC : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        CALL LERROR(2,-1,LINE)
      ELSE
C   Passing a "missing value" to the output file ie - SETVAL TRUE
C   set VAL_MISS and VAL_SET 
        IF(SETVAL)THEN
           IF(QISNAN(VAL_MAGIC)) THEN
             CALL QNAN (VAL_MISS(2,MINDX))
           ELSE
             VAL_MISS(2,MINDX) = VAL_MAGIC
           END IF
           VAL_SET(2,MINDX)   =.TRUE.
C
C---- If SETVAL FALSE check if VAL_MISS(1,MINDX) set for this mtz file.
        ELSE
C
C---- If VAL_SET(..) TRUE missing value present in mtz file.
C         Return it in VAL_MAGIC
C---- If VAL_SET FALSE set VAL_MAGIC to the default - currently Nan
C---- Also transfer VAL_MISS(1,MINDX) taken from input to
C         VAL_MISS(2,.. for output
           IF(VAL_SET(1,MINDX))THEN
              IF(QISNAN(VAL_MISS(1,MINDX))) THEN
                CALL QNAN (VAL_MAGIC)
                CALL QNAN (VAL_MISS(2,MINDX))
              ELSE
                VAL_MAGIC=VAL_MISS(1,MINDX) 
                VAL_MISS(2,MINDX)=VAL_MISS(1,MINDX) 
              END IF
              VAL_SET(2,MINDX)=.TRUE.
           ELSE
C   Set default - currently NAN
             CALL QNAN (VAL_MAGIC)
             CALL QNAN (VAL_MISS(2,MINDX))
             VAL_SET(2,MINDX)=.TRUE.
           END IF
        END IF
C    Reset  SETVAL
        SETVAL = .TRUE.
CCCC
CCCC---- Print warning is this is done after reflections have been written
CCCC
CCC          IF (NREFW(MINDX).GT.0) THEN
CCC            CALL LERROR(1,1, 'From SET_MAGIC: You are changing '//
CCC     +           'the missing value parameter after you have '//
CCC     +           'written reflections to file')
CCC            CALL LERROR(1,1,
CCC     +           '          Lets hope you know what you are doing!')
CCC          END IF
      END IF
      END
C
C
C
C     ===============================
      SUBROUTINE LWCLOS(MINDX,IPRINT)
C     ===============================
C
C
C---- Subroutine to close an MTZ file which has been opened for write.
C     The new header information should already have been supplied by
C     calls to other LW* routines. This subroutine writes the MTZ
C     header and its associated history header to the output file,
C     writes a pointer to the headers in the first record of the file,
C     and a machine stamp and closes the file. The headers are actually 
C     written at the end of the file.
C
C---- Arguments:
C
C     MINDX      (I)	INTEGER         indicates which MTZ file (up to MFILES
C                               	possible open at once)
C
C     IPRINT    (I)	INTEGER         print indicator : meaning :
C                               	=0 No MTZ info printed at all
C                               	=1 Brief header info printed (default)
C                               	=2 Brief header plus history
C                               	=3 Full header dump
C
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MSETS
      PARAMETER (MSETS=MCOLS)
      CHARACTER*10 VERSN
      PARAMETER (VERSN='MTZ:V1.1')
      INTEGER SIZE1
      PARAMETER (SIZE1=20)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER NHISLM
      PARAMETER (NHISLM=30)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,IPRINT
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS,NSETW,SET_ID,CSET_ID,IDEFSET
      LOGICAL SORTB,DATMSS,VAL_SET
      CHARACTER CBATR*1,CBATW*1,CTYPE*1,LTYPE*1,PGNAM*10,SPGNAM*10,
     +          CLABEL*30,PLABS*30,TITLE*70,HSCR*80,ENTRY_ID*64,
     +          DIFFRN_ID*64
C     ..
C     .. Local Scalars ..
      INTEGER ENDLOP,I,IFAIL,ISTAT,JDO10,JDO100,JDO20,JDO30,JDO40,
     +        JDO50,JDO60,JDO65,JDO80,JDO22,JDO23,NITEM,JDO25,
     +        ISET,IDSET,NREDUND,IBAT,BSETID
      CHARACTER LINE*400, STROUT*80
      LOGICAL VS
C     ..
C     .. Local Arrays ..
      INTEGER WINDEX(MBATCH)
      CHARACTER SYMCHS(MAXSYM)*80
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
      LOGICAL QISNAN
      EXTERNAL QISNAN
C     ..
C     .. External Subroutines ..
      EXTERNAL BLANK,LERROR,LHPRT,QWRITC,PUTLIN,QCLOSE,QSEEK,
     +         QMODE,QWARCH,SORTUP,SYMTR3,WBATHD, QWRITI
C     ..
C     .. Common blocks ..
      COMMON /MTZCHR/TITLE(MFILES),CLABEL(MCOLS,MFILES),
     +       CTYPE(MCOLS,MFILES),SPGNAM(MFILES),LTYPE(MFILES),
     +       CBATR(CBLENG,MBATCH,MFILES),PGNAM(MFILES)
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRC/PLABS(MCOLS,MFILES),HSCR(NHISLM,MFILES),
     +       CBATW(CBLENG,MBATCH,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
      COMMON /MTZHAR/NSETW(MFILES),SET_ID(MSETS,MFILES),
     +       ENTRY_ID(MSETS,MFILES),DIFFRN_ID(MSETS,MFILES),
     +       CSET_ID(MCOLS,MFILES),IDEFSET(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZCHR/,/MTZWRK/,/MTZWRC/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LWCLOS : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Calculate position of start of header records, and move to it
C---- - no.refls*no.cols+size of 1st record + 1
C
        HDRST(MINDX) = NREFW(MINDX)*NCOLW(MINDX) + SIZE1 + 1
C
C---- Change mode to bytes for the header record
C
C            **************************
        CALL QMODE(WLUN(MINDX),0,NITEM)
C            **************************
C
C---- MTZ Version stamp
C
        LINE = 'VERS '//VERSN
C
C            ******************************
        CALL QWRITC(WLUN(MINDX),LINE(1:80))
C            ******************************
C
C---- File Title
C
        LINE = 'TITLE ' // TITLE(MINDX) (1:LENSTR(TITLE(MINDX))) // '.'
C
C            ******************************
        CALL QWRITC(WLUN(MINDX),LINE(1:80))
C            ******************************
C
C---- No. of columns, no. of refls, also copy to header, for print
C
        IF (NBATW(MINDX).GT.0) NBATCH(MINDX) = NBATW(MINDX)
        WRITE (LINE,FMT='(A4,1X,I8,1X,I12,1X,I8)') 
     +    'NCOL',NCOLW(MINDX),NREFW(MINDX),NBATCH(MINDX)
C
C            ******************************
        CALL QWRITC(WLUN(MINDX),LINE(1:80))
C            ******************************
C
        NCOLS(MINDX) = NCOLW(MINDX)
        NREFS(MINDX) = NREFW(MINDX)
C
C---- Cell
C
        WRITE (LINE,FMT='(A4,1X,6F10.4)') 'CELL', (CELL(I,MINDX),I=1,6)
C
C            ******************************
        CALL QWRITC(WLUN(MINDX),LINE(1:80))
C            ******************************
C
C---- Sort Order
C
        WRITE (LINE,FMT='(A4,1X,5I4)') 'SORT', (ISORT(I,MINDX),I=1,5)
C
C            ******************************
        CALL QWRITC(WLUN(MINDX),LINE(1:80))
C            ******************************
C
C---- Symmetry - convert from matrices to international tables style
C                also write an informational line
C
        IF (NSYM(MINDX).GT.0) THEN
          WRITE (LINE,FMT='(A6,1X,2I3,1X,A1,1X,I5,1X,A,1X,A)') 
     +      'SYMINF',
     +      NSYM(MINDX),NSYMP(MINDX),LTYPE(MINDX),NSPGRP(MINDX),
     +      SPGNAM(MINDX),PGNAM(MINDX)
C
C              ******************************
          CALL QWRITC(WLUN(MINDX),LINE(1:80))
C              ******************************
C
          ISTAT = 0
C
C              **************************************************
          CALL SYMTR3(NSYM(MINDX),RSYM(1,1,1,MINDX),SYMCHS,ISTAT)
C              **************************************************
C
          DO 10 JDO10 = 1,NSYM(MINDX)
            LINE = 'SYMM ' // SYMCHS(JDO10) (1:LENSTR(SYMCHS(JDO10)))
C
C                ******************************
            CALL QWRITC(WLUN(MINDX),LINE(1:80))
C                ******************************
C
   10     CONTINUE
C
        END IF
C
C---- Resolution range
C
        WRITE (LINE,FMT='(A4,1X,2F12.5)') 'RESO',
     +                   WSRNGE(1,MINDX),WSRNGE(2,MINDX)
C
C            ******************************
        CALL QWRITC(WLUN(MINDX),LINE(1:80))
C            ******************************
C
        SRANGE(1,MINDX) = WSRNGE(1,MINDX)
        SRANGE(2,MINDX) = WSRNGE(2,MINDX)
C
C---- Missing value  - if it hasnt been set take the input value or the default.
C
C
        IF(.NOT.VAL_SET(2,MINDX)) THEN
          VS = VAL_SET(2,MINDX)
          CALL SET_MAGIC(MINDX,VAL_MAGIC,VS)
        END IF
        IF( QISNAN(VAL_MISS(2,MINDX)))  THEN
          WRITE (LINE,FMT='(A4,1X,A4)') 'VALM',' NAN'
        ELSE
          WRITE (LINE,FMT='(A4,1X,F18.5)') 'VALM',VAL_MISS(2,MINDX)
        END IF
C
C            ******************************
        CALL QWRITC(WLUN(MINDX),LINE(1:80))
C            ******************************
C
C
C---- Column info - labels, types, ranges
C
        DO 20 JDO20 = 1,NCOLW(MINDX)
         IF (QISNAN(WRANGE(1,JDO20,MINDX))) WRANGE(1,JDO20,MINDX)=0
         IF( QISNAN(WRANGE(2,JDO20,MINDX))) WRANGE(2,JDO20,MINDX)=999
         IF (CSET_ID(JDO20,MINDX).GT.0) THEN
           WRITE (LINE,FMT='(A6,1X,A,1X,A,1X,2F17.4,1X,I4)') 
     +      'COLUMN',
     +      CLABEL(JDO20,MINDX),CTYPE(JDO20,MINDX),
     +      WRANGE(1,JDO20,MINDX),WRANGE(2,JDO20,MINDX),
     +      CSET_ID(JDO20,MINDX)
         ELSE
           WRITE (LINE,FMT='(A6,1X,A,1X,A,1X,2F17.4)') 
     +      'COLUMN',
     +      CLABEL(JDO20,MINDX),CTYPE(JDO20,MINDX),
     +      WRANGE(1,JDO20,MINDX),WRANGE(2,JDO20,MINDX)
         ENDIF
C
C              ******************************
          CALL QWRITC(WLUN(MINDX),LINE(1:80))
C              ******************************
C
          CRANGE(1,JDO20,MINDX) = WRANGE(1,JDO20,MINDX)
          CRANGE(2,JDO20,MINDX) = WRANGE(2,JDO20,MINDX)
   20   CONTINUE

C---- Write out NDIF, PROJECT and DATASET header lines containing
C     primary list of datasets included in file.

        IF (NSETW(MINDX).GT.0) THEN

C---- Check to see if we have any redundant datasets,
C     e.g. if subset of columns are output, not all datasets
C     in header will be relevant.
     
C---- No batches, i.e. merged file, so check columns
          IF (NBATCH(MINDX).LE.0) THEN

           NREDUND = 0
           ISET = 0
           DO 22 JDO22 = 1,NSETW(MINDX)
            IDSET = SET_ID(JDO22,MINDX)
            DO 23 JDO23 = 1,NCOLW(MINDX)
              IF (CSET_ID(JDO23,MINDX).EQ.IDSET) THEN
                ISET = ISET + 1
                SET_ID(ISET,MINDX) = SET_ID(JDO22,MINDX)
                ENTRY_ID(ISET,MINDX) = ENTRY_ID(JDO22,MINDX)
                DIFFRN_ID(ISET,MINDX) = DIFFRN_ID(JDO22,MINDX)
                GOTO 22
              ENDIF
 23         CONTINUE
C     Dataset not used for any column. Delete.
            NREDUND = NREDUND + 1
 22        CONTINUE
           NSETW(MINDX) = NSETW(MINDX) - NREDUND

C---- Batches present so check them instead (column dataset pointers
C     should not have been set yet).
          ELSE

           NREDUND = 0
           ISET = 0
           DO 24 JDO22 = 1,NSETW(MINDX)
            IDSET = SET_ID(JDO22,MINDX)
            DO 25 JDO23 = 1,NBATCH(MINDX)
              IF (NBATW(MINDX).GT.0) THEN
                IBAT = WOMBAT(JDO23,MINDX)
              ELSE
                IBAT = BATNUM(JDO23,MINDX)
              ENDIF
              CALL LRBSETID(MINDX,IBAT,BSETID)
              IF (BSETID.EQ.IDSET) THEN
                ISET = ISET + 1
                SET_ID(ISET,MINDX) = SET_ID(JDO22,MINDX)
                ENTRY_ID(ISET,MINDX) = ENTRY_ID(JDO22,MINDX)
                DIFFRN_ID(ISET,MINDX) = DIFFRN_ID(JDO22,MINDX)
                GOTO 24
              ENDIF
 25         CONTINUE
C     Dataset not used for any batch. Delete.
            NREDUND = NREDUND + 1
 24        CONTINUE
           NSETW(MINDX) = NSETW(MINDX) - NREDUND
 
          ENDIF

          WRITE (LINE,FMT='(A4,1X,I8)') 
     +      'NDIFFRN',NSETW(MINDX)
C
C            ******************************
          CALL QWRITC(WLUN(MINDX),LINE(1:80))
C            ******************************

          DO 26 JDO25 = 1,NSETW(MINDX)
            WRITE (LINE,FMT='(A7,1X,I7,1X,A)') 
     +        'PROJECT',SET_ID(JDO25,MINDX),
     +        ENTRY_ID(JDO25,MINDX)
            CALL QWRITC(WLUN(MINDX),LINE(1:80))
            WRITE (LINE,FMT='(A7,1X,I7,1X,A)') 
     +        'DATASET',SET_ID(JDO25,MINDX),
     +        DIFFRN_ID(JDO25,MINDX)
            CALL QWRITC(WLUN(MINDX),LINE(1:80))
 26       CONTINUE

C---- End of dataset info
        END IF
C
C---- Write out the batch serial numbers, if a multi-record file
C
        IF (NBATCH(MINDX).GT.0) THEN
          IF (NBATW(MINDX).GT.0) THEN
C
C---- If a batch number was out of order, then sort WOMBAT array
C
            IF (SORTB(MINDX)) THEN
C
C                  **********************************************
              CALL SORTUP(NBATW(MINDX),WOMBAT(1,MINDX),WINDEX(1))
C                  **********************************************
C
            ELSE
C
              DO 30 JDO30 = 1,NBATW(MINDX)
                WINDEX(JDO30) = JDO30
   30         CONTINUE
            END IF
C
            DO 40 JDO40 = 1,NBATW(MINDX),12
              IF ((JDO40+11).GT.NBATW(MINDX)) THEN
                ENDLOP = NBATW(MINDX)
              ELSE
                ENDLOP = JDO40 + 11
              END IF
              WRITE (LINE,FMT='(A5,1X,12I6)') 'BATCH',
     +          (WOMBAT(WINDEX(JDO30),MINDX),JDO30=JDO40,ENDLOP)
C
C                  ******************************
              CALL QWRITC(WLUN(MINDX),LINE(1:80))
C                  ******************************
C
   40       CONTINUE
          ELSE
C
            DO 50 JDO60 = 1,NBATCH(MINDX),12
              IF ((JDO60+11).GT.NBATCH(MINDX)) THEN
                ENDLOP = NBATCH(MINDX)
              ELSE
                ENDLOP = JDO60 + 11
              END IF
              WRITE (LINE,FMT='(A5,1X,12I6)') 'BATCH',
     +          (BATNUM(JDO50,MINDX),JDO50=JDO60,ENDLOP)
C
C                  ******************************
              CALL QWRITC(WLUN(MINDX),LINE(1:80))
C                  ******************************
C
   50       CONTINUE
C
          END IF
        END IF
C
C---- Header end record
C
        LINE = 'END '
C
C            ******************************
        CALL QWRITC(WLUN(MINDX),LINE(1:80))
C            ******************************
C
C---- And now History header (which has been filled in LWHIST)
C
        IF (NHISTL(MINDX).GT.0) THEN
          WRITE (LINE,FMT='(A7,1X,I3)') 'MTZHIST',NHISTL(MINDX)
C
C              ******************************
          CALL QWRITC(WLUN(MINDX),LINE(1:80))
C              ******************************
C
          DO 60 JDO65 = 1,NHISTL(MINDX)
C
C                *************************************
            CALL QWRITC(WLUN(MINDX),HSCR(JDO65,MINDX))
C                *************************************
C
   60     CONTINUE
C
        END IF
C
C---- And now the batch headers, if a multi-column file
C
        IF (NBATCH(MINDX).GT.0) THEN
          WRITE (LINE,FMT='(A7)') 'MTZBATS'
C
C              ******************************
          CALL QWRITC(WLUN(MINDX),LINE(1:80))
C              ******************************
C
          IF (NBATW(MINDX).GT.0) THEN
C
C---- LWBAT has been called, use WOMBAT array to get batch numbers
C
            DO 70 JDO80 = 1,NBATW(MINDX)
C
C                  ***********************************************
              CALL WBATHD(WLUN(MINDX),WOMBAT(WINDEX(JDO80),MINDX),
     +                    RBATW(1,WINDEX(JDO80),MINDX),
     +                    CBATW(1,WINDEX(JDO80),MINDX))
C                  ***********************************************
C
   70       CONTINUE
C
          ELSE
C
C---- Copy input batch headers to output, use BATNUM array
C
            DO 80 JDO100 = 1,NBATCH(MINDX)
C
C                  ***************************************************
              CALL WBATHD(WLUN(MINDX),BATNUM(JDO100,MINDX),
     +                    RBATR(1,JDO100,MINDX),CBATR(1,JDO100,MINDX))
C                  ***************************************************
C
   80       CONTINUE
C
          END IF
        END IF
C
C---- Write complete end of headers and file record
C
        WRITE (LINE,FMT='(A15)') 'MTZENDOFHEADERS'
C
C            ******************************
        CALL QWRITC(WLUN(MINDX),LINE(1:80))
C            ******************************
C
C---- Go back to start of file and fill up first record
C
C            ************************
        CALL QSEEK(WLUN(MINDX),1,1,1)
        CALL QWRITC(WLUN(MINDX),'MTZ ')
        CALL QMODE (WLUN(MINDX),2,NITEM)
        CALL QWRITI(WLUN(MINDX),HDRST(MINDX),1)
C       architecture info:
        CALL QWARCH(WLUN(MINDX),2)
C            **********************************
C
C---- Output information
C
        IF (IPRINT.GT.0) THEN
          WRITE (STROUT,FMT='(A,I3)')
     +      'HEADER INFORMATION FOR OUTPUT MTZ FILE ON INDEX',MINDX
C
C              ****************
          CALL PUTLIN(STROUT,'CURWIN')
          CALL BLANK('CURWIN',1)
          CALL LHPRT(MINDX,IPRINT)
C              *******************
C
        END IF
C
C---- Close the file
C
C            *******************
        CALL QCLOSE(WLUN(MINDX))
C            *******************
C
C---- Zero a few variables and return
C
        NCOLW(MINDX) = 0
        WLUN(MINDX) = 0
C
      END IF
C
      END
C
C
C     =====================================
      SUBROUTINE LWHIST(MINDX,HSTRNG,NLINES)
C     =====================================
C
C
C---- Subroutine to write new history lines to history header.
C     History headers have a maximum of NHISTL lines, we write the new
C     NLINES of history to the header and then fill up any free lines
C     with the older lines.
C
C     Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     HSTRNG    (I)	CHARACTER       array of (NLINES) with the history lines
C
C     NLINES    (I)	INTEGER         number of history lines to be written
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER NHISLM
      PARAMETER (NHISLM=30)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,NLINES
C     ..
C     .. Array Arguments ..
      CHARACTER HSTRNG(NLINES)*80
C     ..
C     .. Arrays in Common ..
      REAL RBATW,WRANGE,WSRNGE
      INTEGER HDRST,NBATR,NBATW,NCOLW,NHISTL,NREFR,NREFW,RLUN,
     +        RPOINT,WLUN,WOMBAT,NDATMSS,NPLABS
      LOGICAL SORTB,DATMSS
      CHARACTER CBATW*1,PLABS*30,HSCR*80
C     ..
C     .. Local Scalars ..
      INTEGER ENDLOP,IFAIL,ISTAT,JDO10,JDO20,JDO5,NLINS,NNONBLANK
      CHARACTER LINE*400
C     ..
C     .. Local Arrays ..
      CHARACTER HISTX(NHISLM)*80
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZWRC/PLABS(MCOLS,MFILES),HSCR(NHISLM,MFILES),
     +       CBATW(CBLENG,MBATCH,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZWRK/,/MTZWRC/
C     ..
C     NLINS is updated, NLINESshouldn't be
      NLINS = NLINES
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LWHIST : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Check not too many lines - if so take first NHISTL lines
C
        IF (NLINS.GT.NHISLM) THEN
          WRITE (LINE,FMT='(A,I2,A,I2,A)')
     +      'From LWHIST : too many history lines (',NLINS,')',NHISLM,
     +      ' lines written'
          ISTAT = 1
C
C              ************************
          CALL LERROR(ISTAT,IFAIL,LINE)
C              ************************
C
          NLINS = NHISLM
        END IF
C
C---- Copy existing history lines to scratch array
C
        DO 10 JDO5 = 1,NHISTL(MINDX)
          HISTX(JDO5) = HSCR(JDO5,MINDX)
   10   CONTINUE
C
        NNONBLANK = 0
        DO 20 JDO10 = 1,NLINS
          IF (HSTRNG(JDO10).NE.' ') THEN
            NNONBLANK = NNONBLANK + 1
            HSCR(NNONBLANK,MINDX) = HSTRNG(JDO10)
          ENDIF
   20   CONTINUE
        NLINS = NNONBLANK
C
C---- Fill up any space left with lines already in history header
C
        IF (NHISTL(MINDX).GT.0) THEN
          ENDLOP = NHISTL(MINDX) + NLINS
          IF (ENDLOP.GT.NHISLM) ENDLOP = NHISLM
          DO 30 JDO20 = NLINS + 1,ENDLOP
            HSCR(JDO20,MINDX) = HISTX(JDO20-NLINS)
   30     CONTINUE
          NHISTL(MINDX) = ENDLOP
        ELSE
          NHISTL(MINDX) = NLINS
        END IF

      END IF
C
      END
C
C
C     ====================================
      SUBROUTINE LWHSTL (MINDX,EXTRA)
C     ====================================
C
C     Write a single line of hstory information to an MTZ file with
C     index MINDX indicating that it was output from the program whose
C     name was previously set with CCPVRS (/CCPRCS) at the
C     current date and time.  EXTRA is more information to append to the
C     record (or blank) .  An example of the information produced might be:
C     From FREERFLAG, 21/ 6/94 18:38:48 with fraction 0.050
C          ^^^^^^^^^                    ^^^^^^^^^^^^^^^^^^^
C          CCPVRS arg                           EXTRA
C     This is just a simplified interface to LWHIST.  The history line
C     will be truncated to 80 characters. 
C
C     Arguments:
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C     PROG      (I)     CHARACTER*(*)   indicates the program responsible
C     EXTRA     (I)     CHARACTER*(*)   extra information
C
      CHARACTER*(*) EXTRA
      INTEGER MINDX
      CHARACTER HIST(1)*80, TIME*8, DATE*8, BUFFER*200, PROG*20
      INTEGER LENSTR
      EXTERNAL LWHIST, CCPDAT, UTIME, LENSTR, CCPPNM
C
      CALL CCPPNM (PROG)
      CALL CCPDAT (DATE)
      CALL UTIME (TIME)
C     Use a largeish buffer and truncate it later if necessary
      WRITE (BUFFER, 10) PROG(:MAX(LENSTR(PROG),1)), DATE, TIME
      BUFFER(LENSTR(BUFFER)+2:) = EXTRA(:MAX(LENSTR(EXTRA),1))
      HIST (1) = BUFFER
 10   FORMAT ('From ',A,', ',A,' ',A)
      CALL LWHIST (MINDX, HIST, 1)
      RETURN
      END
C
C
C
C     ===============================
      SUBROUTINE LWOPEN(MINDX,FILNAM)
C     ===============================
C
C
C---- Subroutine to open an MTZ file for write.
C
C     Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     FILNAM    (I)	CHARACTER       name of file to be opened
C
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MSETS
      PARAMETER (MSETS=MCOLS)
      INTEGER SIZE1
      PARAMETER (SIZE1=20)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER NHISLM
      PARAMETER (NHISLM=30)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX
      CHARACTER FILNAM* (*)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS,NSETW,SET_ID,CSET_ID,IDEFSET
      LOGICAL SORTB,DATMSS,VAL_SET
      CHARACTER CBATR*1,CBATW*1,CTYPE*1,LTYPE*1,PGNAM*10,SPGNAM*10,
     +          CLABEL*30,PLABS*30,TITLE*70,HSCR*80,ENTRY_ID*64,
     +          DIFFRN_ID*64
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT,IUNIN,JDO10,JDO20,JDO30,JDO40,JDO50,JDO55,
     +        JDO60,JDO70,JDO80,NITEM
      CHARACTER LINE*400
C     ..
C     .. Local Arrays ..
      INTEGER IDUMMY(SIZE1)
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR,QMODE,QOPEN,QWRITI,QNAN
C     ..
C     .. Common blocks ..
      COMMON /MTZCHR/TITLE(MFILES),CLABEL(MCOLS,MFILES),
     +       CTYPE(MCOLS,MFILES),SPGNAM(MFILES),LTYPE(MFILES),
     +       CBATR(CBLENG,MBATCH,MFILES),PGNAM(MFILES)
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRC/PLABS(MCOLS,MFILES),HSCR(NHISLM,MFILES),
     +       CBATW(CBLENG,MBATCH,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
      COMMON /MTZHAR/NSETW(MFILES),SET_ID(MSETS,MFILES),
     +       ENTRY_ID(MSETS,MFILES),DIFFRN_ID(MSETS,MFILES),
     +       CSET_ID(MCOLS,MFILES),IDEFSET(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZCHR/,/MTZWRK/,/MTZWRC/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LWOPEN : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Check for other open files on this index - first read files
C  If another file open with a missing value flag set,
C    - transfer it to output as default.
C
        IF (RLUN(MINDX).NE.0 .AND. VAL_SET(1,MINDX) ) THEN
            VAL_MISS(2,MINDX) = VAL_MISS(1,MINDX) 
            VAL_SET(2,MINDX) = VAL_SET(1,MINDX) 
        END IF 
C     If no read file open, then LROPEN has not been called - init. here
C     Some of these not necc. if no read file, but do it for consistency
C
        IF (RLUN(MINDX).EQ.0) THEN
C
          DO 10 JDO10 = 1,6
            CELL(JDO10,MINDX) = 0.0
   10     CONTINUE
C
          NSYM(MINDX) = 0
          NSYMP(MINDX) = 0
C
          DO 40 JDO40 = 1,MAXSYM
            DO 30 JDO30 = 1,4
              DO 20 JDO20 = 1,4
                RSYM(JDO20,JDO30,JDO40,MINDX) = 0.0
   20         CONTINUE
   30       CONTINUE
   40     CONTINUE
C
          NCOLS(MINDX) = 0
          NSETW(MINDX) = 0
          IDEFSET(MINDX) = 0
          NREFS(MINDX) = 0
          NREFR(MINDX) = 0
          NPLABS(MINDX) = 0
          NBATCH(MINDX) = 0
          SRANGE(1,MINDX) = 0.0
          SRANGE(2,MINDX) = 0.0
          TITLE(MINDX) = '   '
          NHISTL(MINDX) = 0
          NSPGRP(MINDX) = 0
          SPGNAM(MINDX) = '?'
          LTYPE(MINDX) = '?'
          PGNAM(MINDX) = '?'
C
          DO 50 JDO50 = 1,5
            ISORT(JDO50,MINDX) = 0
   50     CONTINUE
C
          DO 55 JDO55 = 1,MSETS
            SET_ID(JDO55,MINDX) = 0
            ENTRY_ID(JDO55,MINDX) = ' '
            DIFFRN_ID(JDO55,MINDX) = ' '
   55     CONTINUE

          DO 60 JDO55 = 1,MBATCH
            BATNUM(JDO55,MINDX) = 0
   60     CONTINUE
C
          DO 70 JDO60 = 1,MCOLS
            CRANGE(1,JDO60,MINDX) = 0.0
            CRANGE(2,JDO60,MINDX) = 0.0
            CLABEL(JDO60,MINDX) = '   '
            CTYPE(JDO60,MINDX) = ' '
            RPOINT(JDO60,MINDX) = 0
            PLABS(JDO60,MINDX) = ' '
            CSET_ID(JDO60,MINDX) = 0
   70     CONTINUE
          CALL QNAN (VAL_MISS(2,MINDX))
            VAL_SET(2,MINDX) = .TRUE.
C
        END IF
C
C---- Then the write files - pretty serious if already open
C
        IF (WLUN(MINDX).NE.0) THEN
          WRITE (LINE,FMT='(A,I2,A)')
     +      'From LWOPEN : File already open for write on index',MINDX,
     +      ' - disaster!'
          ISTAT = 2
          IFAIL = -1
C
C              ************************
          CALL LERROR(ISTAT,IFAIL,LINE)
C              ************************
C
        ELSE
C
C---- Everything OK, open the file
C
C              *************************
          CALL QOPEN(IUNIN,FILNAM,'NEW')
          CALL QMODE(IUNIN,2,NITEM)
C              *************************
C
          WLUN(MINDX) = IUNIN
C
C---- Write a dummy first record to the file, to be filled in LWCLOS
C
C              **************************
          CALL QWRITI(IUNIN,IDUMMY,SIZE1)
C              **************************
C
C---- Zero a few variables
C
          DO 80 JDO70 = 1,MCOLS
            WRANGE(1,JDO70,MINDX) = 1.0E6
            WRANGE(2,JDO70,MINDX) = -1.0E6
   80     CONTINUE
C
          NREFW(MINDX) = 0
          NCOLW(MINDX) = 0
          NBATW(MINDX) = 0
          WSRNGE(1,MINDX) = 500.0
          WSRNGE(2,MINDX) = 0.0
          SORTB(MINDX) = .FALSE.
C
          DO 90 JDO80 = 1,MBATCH
            WOMBAT(JDO80,MINDX) = 0
   90     CONTINUE
C
        END IF
      END IF
C
      END
C
C
C
C     =============================
      SUBROUTINE LWREFL(MINDX,ADATA)
C     =============================
C
C
C---- Subroutine to write a reflection record to an MTZ file which
C     has been opened for write. Also output column labels etc.
C     should already have been setup using LWASSN.
C
C     Note : the Constant MDFBIG, is the flag used by the Groningen
C            programs to signal that a column is absent, and therefore
C            this should not be counted in the range determination.
C            Is there also one for CCP4 programs ?
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     ADATA     (I)	REAL            array of dimension at least NCOLW(MINDX)
C                               	containing the reflection record
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      REAL MDFBIG
      PARAMETER (MDFBIG=-1.0E10)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX
C     ..
C     .. Array Arguments ..
      REAL ADATA(*)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS
      LOGICAL SORTB,DATMSS,VAL_SET
      CHARACTER CBATR*1,CTYPE*1,LTYPE*1,PGNAM*10,SPGNAM*10,
     +          CLABEL*30,TITLE*70
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT,JDO10,IH,IK,IL
      REAL RESOL
      CHARACTER LINE*400
      LOGICAL LVALMS
C     ..
C     .. External Functions ..
      REAL     LSTLSQ
      EXTERNAL LSTLSQ
C     ..
C     .. External Subroutines ..
      EXTERNAL IS_MAGIC,LERROR,QWRITR
C     ..
C     .. Common blocks ..
      COMMON /MTZCHR/TITLE(MFILES),CLABEL(MCOLS,MFILES),
     +       CTYPE(MCOLS,MFILES),SPGNAM(MFILES),LTYPE(MFILES),
     +       CBATR(CBLENG,MBATCH,MFILES),PGNAM(MFILES)
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZCHR/,/MTZWRK/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LWREFL : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
C---- Then check that there is a file open
C
      ELSE IF (WLUN(MINDX).EQ.0) THEN
        WRITE (LINE,FMT='(A,I3)')
     +    'From LWREFL : There is no file open for write on index',MINDX
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Everything OK, so increment no. of reflection records written
C
        IF ((NREFW(MINDX).EQ.0) .AND. (NCOLW(MINDX).EQ.0))
     +       NCOLW(MINDX) = NCOLS(MINDX)
        NREFW(MINDX) = NREFW(MINDX) + 1
C
C---- Update the column ranges
C
C         Set the min. and max. values for the column ranges to the data
C         values in the first record
C
C   rewrite of library.c CCPWRG
C  Ejd - April 25 - might be better to check all adata at once like LRREFM SR
          DO 10 JDO10 = 1,NCOLW(MINDX)
            CALL IS_MAGIC(VAL_MISS(2,MINDX),ADATA(JDO10),LVALMS)
            IF (.NOT.LVALMS)THEN
              IF(ADATA(JDO10).LT.WRANGE(1,JDO10,MINDX)) 
     +                           WRANGE(1,JDO10,MINDX) = ADATA(JDO10)
              IF(ADATA(JDO10).GT.WRANGE(2,JDO10,MINDX)) 
     +                           WRANGE(2,JDO10,MINDX) = ADATA(JDO10)
            END IF 
 10       CONTINUE
C
C---- Update the resolution range if appropriate
C
        IF ((NCOLW(MINDX).GE.3).AND.(CTYPE(1,MINDX).EQ.'H')
     +       .AND.(CTYPE(2,MINDX).EQ.'H')
     +       .AND.(CTYPE(3,MINDX).EQ.'H')) THEN

          IH = ADATA(1)
          IK = ADATA(2)
          IL = ADATA(3)
C
C                 ****************************
          RESOL = 4.0 * LSTLSQ(MINDX,IH,IK,IL)
C                 ****************************
C
          IF (RESOL.LT.WSRNGE(1,MINDX))  WSRNGE(1,MINDX) = RESOL
          IF (RESOL.GT.WSRNGE(2,MINDX))  WSRNGE(2,MINDX) = RESOL
C
        END IF
C
C---- Write the reflection record to file
C
C            **************************************
        CALL QWRITR(WLUN(MINDX),ADATA,NCOLW(MINDX))
C            **************************************
C

      END IF
C
      END
C
C
C
C     ==============================
      SUBROUTINE LWSORT(MINDX,SORTX)
C     ==============================
C
C
C---- Subroutine to write the sort order of the output file to the
C     MTZ header.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     SORTX     (I)	INTEGER         array of dimension (5) containing sort
C                               	order of 1st 5 columns in MTZ file
C                               	negative numbers for descending order
C                               	0 for not sorted
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX
C     ..
C     .. Array Arguments ..
      INTEGER SORTX(5)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RSYM,VAL_MISS
      INTEGER BATNUM,ISORT,NBATCH,NCOLS,NREFS,NSPGRP,NSYM,NSYMP
      LOGICAL VAL_SET
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ISTAT,JDO10
      CHARACTER LINE*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LWSORT : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Then write the sort order to the header
C
        DO 10 JDO10 = 1,5
          ISORT(JDO10,MINDX) = SORTX(JDO10)
   10   CONTINUE
C
      END IF
C
      END
C
C
C
C     ================================================================
      SUBROUTINE LWSYMM(MINDX,NSYMX,NSYMPX,RSYMX,LTYPEX,NSPGRX,SPGRNX,
     +                  PGNAMX)
C     ================================================================
C
C
C---- Subroutine to update the symmetry operations and information
C     in the MTZ header.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     NSYMX     (I)	INTEGER         total no. of symmetry operations
C                               	if this is 0 then symm operation values
C                               	are not changed
C
C     NSYMPX    (I)	INTEGER         no. of primitive operations
C
C     RSYMX     (I)	REAL            array of dimensions (4,4,N) of
C                               	symmetry ops on entry, where N>=NSYMX
C
C     LTYPEX    (I)	CHARACTER*1     single character denoting the lattice
C                               	type (possible values are P,A,B,C,I,F,R)
C                               	if blank then current value not changed
C
C     NSPGRX    (I)	INTEGER         space group number, if 0 not changed
C
C     SPGRNX    (I)	CHARACTER*10    space group name, if blank not changed
C
C     PGNAMX    (I)	CHARACTER*10    point group name, if blank not changed
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER LTYP
      PARAMETER (LTYP=9)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,NSPGRX,NSYMPX,NSYMX
      CHARACTER LTYPEX*1,PGNAMX*10,SPGRNX*10
      LOGICAL VAL_SET
C     ..
C     .. Array Arguments ..
      REAL RSYMX(4,4,192)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RSYM,VAL_MISS
      INTEGER BATNUM,ISORT,NBATCH,NCOLS,NREFS,NSPGRP,NSYM,NSYMP
      CHARACTER CBATR*1,CTYPE*1,LTYPE*1,PGNAM*10,SPGNAM*10,
     +          CLABEL*30,TITLE*70
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,IPRINT,ISTAT,JDO10,JDO20,JDO30,JDO40
      CHARACTER LINE*400
C     ..
C     .. Local Arrays ..
      CHARACTER LTYPES(LTYP)*1,SYMCHS(MAXSYM)*80
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR,PUTLIN,SYMTR3
C     ..
C     .. Common blocks ..
      COMMON /MTZCHR/TITLE(MFILES),CLABEL(MCOLS,MFILES),
     +       CTYPE(MCOLS,MFILES),SPGNAM(MFILES),LTYPE(MFILES),
     +       CBATR(CBLENG,MBATCH,MFILES),PGNAM(MFILES)
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZCHR/
C     ..
C     .. Data statements ..
      DATA LTYPES/'P','A','B','C','I','F','R','H','?'/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LWSYMM : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- First deal with symmetry operations - if NSYMX .gt. 0
C
        IF (NSYMX.GT.0) THEN
C
C----    If there is already symmetry info there, print warning
C
          IF ((NSYM(MINDX).GT.0).AND.(NSYM(MINDX).NE.NSYMX)) THEN
C
C                ***********************
            CALL PUTLIN('***Warning : You are changing your ' //
     +           'symmetry operations from :','CURWIN')
C                ***********************
C
            IPRINT = 1
C
C                ***************************************************
            CALL SYMTR3(NSYM(MINDX),RSYM(1,1,1,MINDX),SYMCHS,IPRINT)
            CALL PUTLIN('***To :','CURWIN')
            CALL SYMTR3(NSYMX,RSYMX(1,1,1),SYMCHS,IPRINT)
C                ****************************************
C
          END IF
C
C----    And copy the symmetry operations into the header block
C
          NSYM(MINDX) = NSYMX
          NSYMP(MINDX) = NSYMPX
C
          DO 30 JDO30 = 1,NSYMX
            DO 20 JDO20 = 1,4
              DO 10 JDO10 = 1,4
                RSYM(JDO10,JDO20,JDO30,MINDX) = RSYMX(JDO10,JDO20,JDO30)
   10         CONTINUE
   20       CONTINUE
   30     CONTINUE
C
        END IF
C
C---- Then deal with the other arguments
C
        IF (LTYPEX.NE.' ') THEN
          ISTAT = 1
C
          DO 40 JDO40 = 1,LTYP
            IF (LTYPEX.EQ.LTYPES(JDO40)) ISTAT = 0
   40     CONTINUE
C
          IF (ISTAT.EQ.1) THEN
            WRITE (LINE,FMT='(A)')
     + 'From LWSYMM : Unrecognised lattice type input ... value ignored'
C
C                ************************
            CALL LERROR(ISTAT,IFAIL,LINE)
C                ************************
C
          ELSE
            LTYPE(MINDX) = LTYPEX
          END IF
        END IF
        IF (NSPGRX.NE.0) NSPGRP(MINDX) = NSPGRX
        IF (SPGRNX(1:1).NE.' ') SPGNAM(MINDX) = SPGRNX
        IF (PGNAMX(1:1).NE.' ') PGNAM(MINDX) = PGNAMX
C
      END IF
C
      END
C
C
C
C     ====================================
      SUBROUTINE LWTITL(MINDX,NTITLE,FLAG)
C     ====================================
C
C
C---- Subroutine to update the title of an MTZ file in the header
C     common block
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     NTITLE    (I)	CHARACTER*(*)   Character string containing the title
C                               	 - maximum possible length 70
C
C     FLAG      (I)	INTEGER         =0 replace old title with new one
C                               	=1 append new one to old, with one space
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
C     ..
C     .. Scalar Arguments ..
      INTEGER FLAG,MINDX
      CHARACTER NTITLE* (*)
C     ..
C     .. Arrays in Common ..
      CHARACTER CBATR*1,CTYPE*1,LTYPE*1,PGNAM*10,SPGNAM*10,
     +          CLABEL*30,TITLE*70
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ILEN,ISTAT,JLEN,KLEN
      CHARACTER LINE*400
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC LEN
C     ..
C     .. Common blocks ..
      COMMON /MTZCHR/TITLE(MFILES),CLABEL(MCOLS,MFILES),
     +       CTYPE(MCOLS,MFILES),SPGNAM(MFILES),LTYPE(MFILES),
     +       CBATR(CBLENG,MBATCH,MFILES),PGNAM(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZCHR/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LWTITL : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Then update the title
C
        IF (FLAG.EQ.0) THEN
C
C----    Replace the title
C
          ILEN = LEN(TITLE(MINDX))
          JLEN = LENSTR(NTITLE)
          IF (JLEN.GT.ILEN) THEN
            ISTAT = 1
            WRITE (LINE,FMT='(A,I3,A)')
     +        'From LWTITL : New title is too long, truncated to',ILEN,
     +        ' chars'
C
C                ************************
            CALL LERROR(ISTAT,IFAIL,LINE)
C                ************************
C
            JLEN = ILEN
          END IF
          IF(JLEN.GE.1)TITLE(MINDX) = NTITLE(1:JLEN)
          IF(JLEN.EQ.0)TITLE(MINDX) = ' '
        ELSE
C
C----    Append the new title to the old one
C
          ILEN = LENSTR(TITLE(MINDX))
          JLEN = LENSTR(NTITLE)
          KLEN = LEN(TITLE(MINDX))
C
          IF ((ILEN+JLEN).GT.KLEN) THEN
            ISTAT = 1
            WRITE (LINE,FMT='(A,I3,A)')
     +        'From LWTITL : Title is too long, truncated to',KLEN,
     +        ' chars'
C
C                ************************
            CALL LERROR(ISTAT,IFAIL,LINE)
C                ************************
C
            JLEN = KLEN - (ILEN+1)
          END IF
C
          IF (JLEN .GT. 0) THEN
             TITLE(MINDX) (ILEN+1:ILEN+1) = ' '
             TITLE(MINDX) (ILEN+2:ILEN+JLEN+1) = NTITLE(1:JLEN)
          ENDIF
C
        END IF
C
      END IF
C
      END
C
C
C
C     ===================================================
      SUBROUTINE LRBAT(MINDX,BATNO,RBATCH,CBATCH,IPRINT)
C     ===================================================
C
C---- Subroutine to return the header info for the next batch from the
C     multi-record MTZ file (or pair of files) open on index MINDX, and
C     to return the information in the two arrays RBATCH (for numbers)
C     and CBATCH (for characters).
C
C     This will also optionally print out all this information in
C     a pretty format.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     BATNO     (O)	INTEGER         serial number of this batch
C                               	returned as -1 if all batch headers
C                               	have been read
C
C     RBATCH    (O)	REAL(*)         array into which integer and real batch
C                               	info is stored by subroutine rbathd
C                               	this should be equivalenced onto the
C                               	appropriate COMMON block in the calling
C                               	program. The first item is Nwords,ie how
C                               	many items in the array:-
C                               	Nwords = 0 if no orientation
C                               	data is present in the batch header
C
C     CBATCH    (O)	CHARACTER*(*)   as RBATCH, but for character items - no
C                               	nwords however
C
C     IPRINT    (I)	INTEGER         print indicator
C                                 	= 0 no print
C                                 	= 1 print batch title only
C                                 	= 2 print orientation block as well
C                                 	.GT. 30 print orientation block as well
C                                    	BUT TO IPRINT unit number
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER BATNO,MINDX,IPRINT
C     ..
C     .. Array Arguments ..
      REAL RBATCH(MBLENG)
      CHARACTER CBATCH*(*)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS
      LOGICAL SORTB,DATMSS,VAL_SET
      CHARACTER CBATR*1,CTYPE*1,LTYPE*1,PGNAM*10,SPGNAM*10,
     +          CLABEL*30,TITLE*70
C     ..
C     .. Local Scalars ..
      REAL RBATCX
      INTEGER IFAIL,ISTAT,JDO10,JDO20,NWORDS
      CHARACTER LINE2*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LBPRT,LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZCHR/TITLE(MFILES),CLABEL(MCOLS,MFILES),
     +       CTYPE(MCOLS,MFILES),SPGNAM(MFILES),LTYPE(MFILES),
     +       CBATR(CBLENG,MBATCH,MFILES),PGNAM(MFILES)
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C
C---- Here are the important EQUIVALENCE statements
C
C     .. Equivalences ..
      EQUIVALENCE (RBATCX,NWORDS)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZCHR/,/MTZWRK/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE2,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LRBAT : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            *************************
        CALL LERROR(ISTAT,IFAIL,LINE2)
C            *************************
C
      END IF
C
C---- Check if it's really a multi-record file
C
      IF (NBATCH(MINDX).EQ.0) THEN
        WRITE (LINE2,FMT='(A,A,I2,A)') 'From LRBAT : File open on ',
     +    'index',MINDX,' is NOT a multi-record file'
        ISTAT = 2
        IFAIL = -1
C
C            *************************
        CALL LERROR(ISTAT,IFAIL,LINE2)
C            *************************
C
      END IF
C
C---- Copy the batch info from the header arrays to the arguments
C
      NBATR(MINDX) = NBATR(MINDX) + 1
C
      IF (NBATR(MINDX).GT.NBATCH(MINDX)) THEN
        BATNO = -1
        NBATR(MINDX) = NBATR(MINDX) - 1
      ELSE
        BATNO = BATNUM(NBATR(MINDX),MINDX)
C       following is strictly illegal, setting NWORDS
        RBATCX = RBATR(1,NBATR(MINDX),MINDX)
C
        DO 10 JDO10 = 1,NWORDS
          RBATCH(JDO10) = RBATR(JDO10,NBATR(MINDX),MINDX)
   10   CONTINUE
C
        DO 20 JDO20 = 1,CBLENG
          CBATCH(JDO20:JDO20) = CBATR(JDO20,NBATR(MINDX),MINDX)
   20   CONTINUE
C
C---- print if we want to
C
C                        **************************************
        IF (IPRINT.GT.0) CALL LBPRT(BATNO,IPRINT,RBATCH,CBATCH)
C                        **************************************
C
      END IF
C
      END
C
C
C
C     ============================================
      SUBROUTINE LWBAT(MINDX,BATNO,RBATCH,CBATCH)
C     ============================================
C
C---- Subroutine to write the header for batch with serial number BATNO
C     to the MTZ file open for write  on index MINDX, batch info
C     stored in the two arrays RBATCH (for numbers)
C     and CBATCH (for characters).
C
C     If this routine is called at all then it must be called for
C     every batch which is to be output - these batch serial numbers
C     are stored in array WOMBAT, and are used in LWCLOS.
C
C     If it is called with BATNO = 0 this is a flag to say that the
C     output MTZ file will be a standard file, and not a multi-record
C     one, ie no batches. After this call no batch information is
C     available to the calling program, so don't call it too soon !
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     BATNO     (I)	INTEGER         serial number of this batch
C                               	if 0 wipe away all batch info for file
C
C     RBATCH    (I)	REAL(*)         array from which integer and real batch
C                               	info is decoded by subroutine wbathd
C                               	this should be equivalenced onto the
C                               	appropriate COMMON block in the calling
C                               	program. The first item is nwords,ie how
C                               	many items in the array, if nword is 0
C                               	then only the title is written to header
C
C     CBATCH    (I)	CHARACTER(*)    as RBATCH, but for character items - no
C                               	nwords however; title is 1st 70 chars of
C                               	CBATCH.
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER NHISLM
      PARAMETER (NHISLM=30)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER BATNO,MINDX
C     ..
C     .. Array Arguments ..
      REAL RBATCH(MBLENG)
      CHARACTER CBATCH*(*)
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RBATW,RSYM,WRANGE,WSRNGE,VAL_MISS
      INTEGER BATNUM,HDRST,ISORT,NBATCH,NBATR,NBATW,NCOLS,NCOLW,NHISTL,
     +        NPLABS,NREFR,NREFS,NREFW,NSPGRP,NSYM,NSYMP,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS
      LOGICAL SORTB,DATMSS,VAL_SET
      CHARACTER CBATW*1,PLABS*30,HSCR*80
C     ..
C     .. Local Scalars ..
      REAL RBATCX
      INTEGER IFAIL,ISTAT,JDO10,JDO20,JDO30,NWORDS
      CHARACTER LINE2*400
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZWRC/PLABS(MCOLS,MFILES),HSCR(NHISLM,MFILES),
     +       CBATW(CBLENG,MBATCH,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C
C---- Here are the important EQUIVALENCE statements
C
C     .. Equivalences ..
      EQUIVALENCE (RBATCX,NWORDS)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZWRK/,/MTZWRC/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE2,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LWBAT : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            *************************
        CALL LERROR(ISTAT,IFAIL,LINE2)
C            *************************
C
      END IF
C
C---- Check if it's really a multi-record file
C---- NBATCH could be zero for multi-record file only if no input file
C
      IF ((NBATCH(MINDX).EQ.0) .AND. (RLUN(MINDX).GT.0)) THEN
        WRITE (LINE2,FMT='(A,A,I2,A)') 'From LWBAT : File open on ',
     +    'Index ',MINDX,' is NOT a multi-record file'
        ISTAT = 2
        IFAIL = -1
C
C            *************************
        CALL LERROR(ISTAT,IFAIL,LINE2)
C            *************************
C
      END IF
C
C---- Check if BATNO = 0, if so no more multi-record file
C
      IF (BATNO.EQ.0) THEN
        NBATCH(MINDX) = 0
        NBATW(MINDX) = 0
      ELSE
C
C---- Check if the new batch serial number is out of order
C---- or is already present
C
        IF (NBATW(MINDX).GT.0) THEN
          IF (WOMBAT(NBATW(MINDX),MINDX).GE.BATNO) THEN
C
            DO 10 JDO10 = 1,NBATW(MINDX)
              IF (WOMBAT(JDO10,MINDX).EQ.BATNO) THEN
                WRITE (LINE2,FMT='(A,A,A,I5)') 'From LWBAT : Attempt ',
     +            'made to output 2 batches with the same serial ',
     +            'number to file : batch serial number is :',BATNO
                ISTAT = 2
                IFAIL = -1
C
C                  *************************
                CALL LERROR(ISTAT,IFAIL,LINE2)
C                  *************************
C
              END IF
   10       CONTINUE
            SORTB(MINDX) = .TRUE.
          END IF
        END IF
C
C---- Update array of batch serial numbers written and counter
C
        NBATW(MINDX) = NBATW(MINDX) + 1
        IF (NBATW(MINDX).GT.MBATCH) THEN
           WRITE (LINE2, FMT='(A,I6)')
     $          ' From LWBAT: too many batches, maximum is', MBATCH
           ISTAT = 2
           IFAIL = -1
C
C               *************************
           CALL LERROR(ISTAT,IFAIL,LINE2)
C               *************************
C
        END IF

        WOMBAT(NBATW(MINDX),MINDX) = BATNO
C
C---- Check NWORDS
C
        RBATCX = RBATCH(1)
C
        IF (NWORDS.GT.MBLENG) THEN
          WRITE (LINE2,FMT='(A,I8,A,I8)') ' From LWBAT : NWORDS = ',
     +      NWORDS,' is greater than buffer length ',MBLENG
          ISTAT = 2
          IFAIL = -1
C
C              *************************
          CALL LERROR(ISTAT,IFAIL,LINE2)
C              *************************
C
        END IF
C
C---- Copy batch info from arguments to header scratch arrays for
C     later output
C
        DO 20 JDO20 = 1,NWORDS
          RBATW(JDO20,NBATW(MINDX),MINDX) = RBATCH(JDO20)
   20   CONTINUE
C
        DO 30 JDO30 = 1,CBLENG
          CBATW(JDO30,NBATW(MINDX),MINDX) = CBATCH(JDO30:JDO30)
   30   CONTINUE
C
      END IF
C
      END
C
C
C
C     ===========================================
      SUBROUTINE RBATHD(ILUN,BATCH,RBATCH,CBATCH)
C     ===========================================
C
C     Read next batch header from multi-record MTZ file open on
C     unit ILUN, and return its batch number, plus real and
C     character arrays.
C
C---- Arguments :
C
C     ILUN      (I)	INTEGER         LUN on which the file is open
C
C     BATCH     (O)	INTEGER         serial number of this batch
C
C     RBATCH    (O)	REAL(*)         array into which integer and real batch
C                               	info is decoded by subroutine
C                               	this should be equivalenced onto the
C                               	appropriate COMMON block in the calling
C                               	program. The first item is nwords,ie how
C                               	many items in the array, if nword is 0
C                               	then only the title is present
C
C     CBATCH    (O)	CHARACTER(*)*1  as RBATCH, but for character items - no
C                               	nwords however; title is 1st 70 chars of
C                               	CBATCH.
C
C     The lengths of the RBATCH & CBATCH arrays are parameters here!
C
C     .. Parameters ..
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
C     ..
C     .. Scalar Arguments ..
      INTEGER BATCH,ILUN
C     ..
C     .. Array Arguments ..
      REAL RBATCH(MBLENG)
      CHARACTER CBATCH(CBLENG)*1
C     ..
C     .. Local Scalars ..
      INTEGER I,IEND,IFGERR,ISTERR,J,L,NWORDS,NINTGR,NREALS,NITEM,IER
      CHARACTER LINE*80,LINERR*100
C     ..
C     .. External Subroutines ..
      EXTERNAL LERROR,LRHDRL,QMODE,QREADR
C     ..
C
C---- Read first header line to get NWORDS etc., and Batch number
C
C          *****************
      CALL LRHDRL(ILUN,LINE)
C          *****************
C
      READ (LINE(4:),FMT='(4I8)') BATCH,NWORDS,NINTGR,NREALS
C
C---- Read batch title == 1st 70 characters of CBATCH
C
C          *****************
      CALL LRHDRL(ILUN,LINE)
C          *****************
C
      DO 10 I = 1,70
        J = I + 6
        CBATCH(I) = LINE(J:J)
   10 CONTINUE
C
C---- If NWORDS = 0, don't read anything else
C
      IF (NWORDS.GT.0) THEN
Cdw---- Read NINTGR integers followed by NREALS reals as BINARY
         CALL QMODE(ILUN,6,NITEM)
         CALL QREADR(ILUN,RBATCH(1),NINTGR,IER)
         IF (IER.GT.0) GO TO 50
         CALL QMODE(ILUN,2,NITEM)
         CALL QREADR(ILUN,RBATCH(NINTGR+1),NREALS,IER)
         IF (IER.GT.0) GO TO 50
         CALL QMODE(ILUN,0,NITEM)
C             ********************************
C
C---- Read rest of CBATCH (we need to know here how much to read!)
C     Using 2 loops means that CBATCH can expand 
C     in size beyond 1 line later
C
        DO 40 L = 71,CBLENG,75
C
C              *******************
          CALL LRHDRL(ILUN,LINE)
C              *******************
C
          J = 5
          IEND = L + 74
          IF (IEND.GT.CBLENG) IEND = CBLENG
          DO 30 I = L,IEND
            J = J + 1
            CBATCH(I) = LINE(J:J)
   30     CONTINUE
   40   CONTINUE
C
        GO TO 60
C
C---- Error on reading header
C
   50   WRITE (LINERR,FMT='(A,A,I2,A)')
     +  'RBATHD error: Error reading batch header record from',
     +  ' MTZ file, only ',IER,' words read'

        ISTERR = 2
        IFGERR = -1
C
C            ****************************
        CALL LERROR(ISTERR,IFGERR,LINERR)
C            ****************************
C
        RETURN
      END IF
C
   60 CONTINUE
C
      END
C
C
C
C     ==========================================
      SUBROUTINE WBATHD(ILUN,BATCH,RBATCH,CBATCH)
C     ==========================================
C
C     Write batch header to Multi-record MTZ file open for write
C     on ILUN, with batch number BATCH, and data in RBATCH and CBATCH
C
C---- Arguments :
C
C     ILUN      (I)	INTEGER         LUN on which the file is open
C
C     BATCH     (I)	INTEGER         serial number of this batch
C
C     RBATCH    (I)	REAL(*)         array from which integer and real batch
C                               	info is encoded by subroutine
C                               	this should be equivalenced onto the
C                               	appropriate COMMON block in the calling
C                               	program. The first item is nwords,ie how
C                               	many items in the array, if nword is 0
C                               	then only the title is written
C
C     CBATCH    (I)	CHARACTER(*)*1  as RBATCH, but for character items - no
C                               	nwords however; title is 1st 70 chars of
C                               	CBATCH.
C
C     The lengths of the RBATCH & CBATCH arrays are parameters here!
C
C     .. Parameters ..
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
C     ..
C     .. Scalar Arguments ..
      INTEGER BATCH,ILUN
C     ..
C     .. Array Arguments ..
      REAL RBATCH(MBLENG)
      CHARACTER CBATCH(CBLENG)*1
C     ..
C     .. Local Scalars ..
      REAL RBATCX,RBATCY,RBATCZ
      INTEGER I,IEND,J,L,NWORDS,NINTGR,NREALS,NITEM
      CHARACTER LINE*80
C     ..
C     .. External Subroutines ..
      EXTERNAL QWRITC,QMODE,QWRITR
C     ..
C
C---- Here are the important EQUIVALENCE statements
C
C     .. Equivalences ..
      EQUIVALENCE (RBATCX,NWORDS)
      EQUIVALENCE (RBATCY,NINTGR)
      EQUIVALENCE (RBATCZ,NREALS)
C     ..
C
C----  Copy RBATCH(1) == NWORDS for testing
C
      RBATCX = RBATCH(1)
      RBATCY = RBATCH(2)
      RBATCZ = RBATCH(3)
C
C---- Write out batch header name and NWORDS
C
      WRITE (LINE,FMT='(A,4I8)') 'BH ',BATCH,NWORDS,NINTGR,NREALS
C
C     ============================
      CALL QWRITC(ILUN,LINE(1:80))
C     ============================
C
C---- Write out batch title == 1st 70 characters of CBATCH
C
      LINE = 'TITLE '
C
      DO 10 I = 1,70
        J = I + 6
        LINE(J:J) = CBATCH(I)
   10 CONTINUE
C
C     ============================
      CALL QWRITC(ILUN,LINE(1:80))
C     ============================
C
C---- If NWORDS = 0, only write out batch title
C
      IF (NWORDS.GT.0) THEN
C---- Write out the orientation block as REALs with one QWRITE call -
C     have to change the mode to 2 and change back to 0 after
C
C            *****************************
        CALL QMODE(ILUN,2,NITEM)
        CALL QWRITR(ILUN,RBATCH(1),NWORDS)
        CALL QMODE(ILUN,0,NITEM)
C            *****************************
C
C---- Write out rest of CBATCH 
C     (we need to know here how much to write!)
C
        LINE = 'BHCH '
C
        DO 40 L = 71,CBLENG,75
          IEND = L + 74
          IF (IEND.GT.CBLENG) IEND = CBLENG
          J = 5
C
          DO 30 I = L,IEND
            J = J + 1
            LINE(J:J) = CBATCH(I)
   30     CONTINUE
C
C         ============================
          CALL QWRITC(ILUN,LINE(1:80))
C         ============================
C
   40   CONTINUE
C
      END IF
C
      END
C
C
C
C     =========================================================
      SUBROUTINE LKYIN(MINDX,LSPRGI,NLPRGI,NTOK,LINE,IBEG,IEND)
C     ==========================================================
C
C-----Subroutine to read standard input lines of the form
C              LABIN  item1=name1 item2=name2 ...
C     Use this to read the inputs, and then LRASSN to setup the
C     program label to file label assignments.
C
C---- Arguments:
C
C     LSPRGI    (I)	CHARACTER*30    program label strings (array)
C                               	L(abel) S(tring) PRG(rammme) I(nput)
C
C     NLPRGI    (I)	INTEGER         number of program input labels
C                               	N(umber of) L(abels) PRG(ramme) I(nput)
C
C     NTOK      (I)	INTEGER         from Parser, number of tokens on line
C
C     LINE      (I)	CHARACTER*(*)   the input line
C
C     IBEG,IEND (I)	INTEGER         arrays from the parser, delimiters
C                               	for each token
C
C
C
C     .. Parameters ..
      INTEGER MCOLS,MFILEX
      PARAMETER (MCOLS=200,MFILEX=9)
C     ..
C     .. Scalar Arguments ..
      INTEGER NLPRGI,NTOK,MINDX
      CHARACTER LINE* (*)
C     ..
C     .. Array Arguments ..
      INTEGER IBEG(*),IEND(*)
      CHARACTER*30 LSPRGI(*)
C     ..
C     .. Arrays in Common ..
      CHARACTER LSUSRI*30,LSUSRO*30
      INTEGER NLUSRI,NLUSRO
C     ..
C     .. Local Scalars ..
      INTEGER JDO,JLOOP,JSTART,JTOK
      CHARACTER CWORK*30,CWORK2*30,LC1*30,LC2*30,STROUT*400
C     ..
C     .. Local Arrays ..
      LOGICAL SetPrgLab(MCOLS)
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL PUTLIN,CCPERR
C     ..
C     .. Common blocks ..
      COMMON /MTZLAB/NLUSRI(MFILEX),NLUSRO(MFILEX)
      COMMON /MTZLBC/LSUSRI(MFILEX,MCOLS),LSUSRO(MFILEX,MCOLS)
C     ..
C     .. Save statement ..
      SAVE /MTZLAB/, /MTZLBC/
C     ..
C
      JTOK = NTOK
      JSTART = 2
      DO 10 JDO = 1,NLPRGI
        SetPrgLab(JDO) = .FALSE.
   10 CONTINUE
C
C---- Keyword  LABIN  item1=name1 item2=name2 ...
C
C      item == program label
C      name == user label
C
      IF (NLPRGI.LE.0) THEN
C
C            ***********************
        CALL PUTLIN(' Error this program has no PROGRAM LABELS',
     +       'ERRWIN')
C            ***********************
C
      ELSE IF (JTOK.LE.1) THEN
C
C            ***********************
        CALL PUTLIN(' **** Warning no argument given for LABIN ',
     +       'ERRWIN')
C            ***********************
C
      ELSE
C
C---- Find input label assignments
C
        DO 70 JLOOP = JSTART,NTOK,2
C
          IF ((JLOOP+1).GT.JTOK) THEN
            GO TO 80
          ELSE
            CWORK = LINE(IBEG(JLOOP) :IEND(JLOOP))
            LC1 = CWORK
C
C                *************
C-- file labels are case sensitive!!            CALL CCPUPC(CWORK)
C                *************
C
            CWORK2 = LINE(IBEG(JLOOP+1) :IEND(JLOOP+1))
            LC2 = CWORK2
C
C                **************
C-- file labels are case sinsitive!!            CALL CCPUPC(CWORK2)
C                **************
C
C           first try to match file label to left of assignment
C           (canonical order) after capitalisation
            DO 20 JDO = 1,NLPRGI
              IF (CWORK.EQ.LSPRGI(JDO)) THEN
                IF (SetPrgLab(JDO)) GOTO 30
                GO TO 60
              ENDIF
   20       CONTINUE
C           else, try to match file label on rhs as an option
   30       DO 40 JDO = 1,NLPRGI
              IF (CWORK2.EQ.LSPRGI(JDO)) THEN
                IF (SetPrgLab(JDO)) THEN
                 WRITE (STROUT,FMT=
     +            '('' ERROR with label assignments '',A,'' and '',A)') 
     +            CWORK(1:LENSTR(CWORK)),CWORK2(1:LENSTR(CWORK2))
C
C                      ***********************************************
                  CALL PUTLIN(STROUT,'ERRWIN')
                  CALL CCPERR(1,' ERROR Program label assigned twice')
C                      ***********************************************
C
                ENDIF
                GO TO 50
              ENDIF
   40       CONTINUE
C
            WRITE (STROUT,FMT=
     +        '(''  Neither '',A,'' nor '',A,'' recognised'')')
     +         CWORK(1:LENSTR(CWORK)),CWORK2(1:LENSTR(CWORK2))
C
C                ***********************
            CALL PUTLIN(STROUT,'ERRWIN')
C                ***********************
C
            WRITE (STROUT,FMT=
     +        '(''  OR maybe '',A,'' has been assigned twice '')')
     +        CWORK(1:LENSTR(CWORK))
C
C                ***********************
            CALL PUTLIN(STROUT,'ERRWIN')
            CALL CCPERR(1, 'Input column assignment does not match'//
     +           ' program labels')
C                ***********************
C
   50       NLUSRI(MINDX) = NLUSRI(MINDX) + 1
            LSUSRI(MINDX,JDO) = LC1
            SetPrgLab(JDO) = .TRUE.
            GO TO 70
   60       NLUSRI(MINDX) = NLUSRI(MINDX) + 1
            LSUSRI(MINDX,JDO) = LC2
            SetPrgLab(JDO) = .TRUE.
          END IF
   70   CONTINUE
        RETURN
   80   CONTINUE
C
C            ***********************
        CALL PUTLIN(' **** Error !!!! for LABIN ****','ERRWIN')
        CALL PUTLIN(' **** NOT ENOUGH ARGUMENT PAIRS of type ','ERRWIN')
        CALL PUTLIN('Prog_label = User_label','ERRWIN')
C            ***********************
C
      END IF
C
      END
C
C
C
C     ==========================================================
      SUBROUTINE LKYOUT(MINDX,LSPRGO,NLPRGO,NTOK,LINE,IBEG,IEND)
C     ===========================================================
C
C-----Subroutine to read standard input lines of the form
C              LABOUT LabelFC=userFC   LabelPHCAL=userPHCAL ...
C     Use this to read the inputs, and then LWASSN to setup the
C     program label to (new) file label assignments.
C
C---- Arguments:
C
C     LSPRGO    (I)	CHARACTER*30    program label strings (array)
C                               	L(abel) S(tring) PRG(rammme) O(utput)
C
C     NLPRGO    (I)	INTEGER         number of program output labels
C                               	N(umber of) L(abels) PRG(ramme) O(utput)
C
C     NTOK      (I)	INTEGER         from Parser, number of tokens on line
C
C     LINE      (I)	CHARACTER*(*)   the input line
C
C     IBEG,IEND (I)	INTEGER         arrays from the parser, delimiters
C                               	for each token
C
C--- Local or Common variables of interest
C
C    LSUSRO    user supplied label strings
C              L(abel) S(tring) USR() O(utput)
C
C    NLUSRO    number of user output "assignment labels"
C              N(umber of) L(abels) USR() O(utput)
C
C
C     .. Parameters ..
      INTEGER MCOLS,MFILEX
      PARAMETER (MCOLS=200,MFILEX=9)
C     ..
C     .. Scalar Arguments ..
      INTEGER NLPRGO,NTOK,MINDX
      CHARACTER LINE* (*)
C     ..
C     .. Array Arguments ..
      INTEGER IBEG(*),IEND(*)
      CHARACTER*30 LSPRGO(*)
C     ..
C     .. Arrays in Common ..
      INTEGER NLUSRI,NLUSRO
      CHARACTER LSUSRI*30,LSUSRO*30
C     ..
C     .. Local Scalars ..
      INTEGER JDO,JLOOP,JSTART,JTOK
      CHARACTER CWORK*30,CWORK2*30,LC1*30,LC2*30,STROUT*400
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPUPC,PUTLIN
C     ..
C     .. Common blocks ..
      COMMON /MTZLAB/NLUSRI(MFILEX),NLUSRO(MFILEX)
      COMMON /MTZLBC/LSUSRI(MFILEX,MCOLS),LSUSRO(MFILEX,MCOLS)
C     ..
C     .. Save statement ..
      SAVE /MTZLAB/, /MTZLBC/
C     ..
C
      JTOK = NTOK
      JSTART = 2
C
C---- Keyword 
C             LabelFC=userFC   LabelPHCAL=userPHCAL ...
C
      IF (NLPRGO.LE.0) THEN
C
C            ***********************
        CALL PUTLIN(' Error this program has no OUPUT LABELS','ERRWIN')
C            ***********************
C
      ELSE IF (JTOK.LE.1) THEN
C
C            ***********************
        CALL PUTLIN(' **** Warning no argument given for LABOUT',
     +       'ERRWIN')
        CALL PUTLIN('For This Program Default labels will be used' //
     +       ' if possible','ERRWIN')
C            ***********************
C
      ELSE
C
C---- Find output label assignments
C
        DO 40 JLOOP = JSTART,NTOK,2
C
          IF ((JLOOP+1).GT.JTOK) THEN
            GO TO 50
          ELSE
            CWORK = LINE(IBEG(JLOOP) :IEND(JLOOP))
            LC1 = CWORK
C
C                *************
            CALL CCPUPC(CWORK)
C                *************
C
            CWORK2 = LINE(IBEG(JLOOP+1) :IEND(JLOOP+1))
            LC2 = CWORK2
C
C                **************
            CALL CCPUPC(CWORK2)
C                **************
C
            DO 10 JDO = 1,NLPRGO
              IF (CWORK.EQ.LSPRGO(JDO)) THEN
                GO TO 30
              ELSE IF (CWORK2.EQ.LSPRGO(JDO)) THEN
                GO TO 20
              END IF
   10       CONTINUE
C
C                ***********************
            WRITE (STROUT,FMT=
     +        '(''  Neither '',A,'' nor '',A,'' recognised'')') CWORK(1:
     +        LENSTR(CWORK)),CWORK2(1:LENSTR(CWORK2))
            CALL PUTLIN(STROUT,'ERRWIN')
            CALL CCPERR(1, 'Output column assignment does not match'//
     +           ' program labels')
C                ***********************
C
            GO TO 40
   20       NLUSRO(MINDX) = NLUSRO(MINDX) + 1
            LSUSRO(MINDX,JDO) = LC1
            GO TO 40
   30       NLUSRO(MINDX) = NLUSRO(MINDX) + 1
            LSUSRO(MINDX,JDO) = LC2
          END IF
   40   CONTINUE
        RETURN
C
   50   CONTINUE
C
        CALL PUTLIN(' **** Error !!!! for LABOUT ****','ERRWIN')
        CALL PUTLIN(' Not enough argument pairs of type '//
     +           ' Prog_label = User_label','ERRWIN')
C            ***********************
C
      END IF
C
      END
C
C
C
C     ==================================================================
      SUBROUTINE LKYSET(LSPRGI,NLPRGI,LSUSRJ,KPOINT,ITOK,NTOK,LINE,IBEG,
     +                  IEND)
C     ==================================================================
C
C
C-----Subroutine to parse standard input lines of the form
C              LABIN  item1=name1 item2=name2 ...
C
C     This subroutine returns the User input labels to the calling program
C     (LKYIN doesn't) and the array Kpoint to indicate which of the program
C     labels were set.
C     
C     Note : (i) there is no MINDX argument in this call - it is not tied to any
C                specific file, and the header common blocks are not affected
C                by this subroutine
C
C           (ii) although it looks similar to LKYIN IT DOES NOT REPLACE IT,
C                it just adds extra functionality
C
C---- Arguments:
C
C     LSPRGI    (I)	CHARACTER*30    program label strings (array)
C                               	L(abel) S(tring) PRG(rammme) I(nput)
C
C     NLPRGI    (I)	INTEGER         number of program input labels
C                               	N(umber of) L(abels) PRG(ramme) I(nput)
C
C     LSPRGJ    (O)	CHARACTER*30    user supplied  label strings (array)
C                               	L(abel) S(tring) USR(rammme) I(nput)
C                              		Copy of LSUSRI to return to calling program
C
C     KPOINT    (O)	INTEGER         Indicator whether this LSPRGI is set.
C					-1 if set, 0 if not
C
C     ITOK      (I)	INTEGER         First token to search on line
C     					(allows you to skip the LABIN token)
C
C     NTOK      (I)	INTEGER         from Parser, number of tokens on line
C
C     LINE      (I)	CHARACTER*(*)   the input line
C
C     IBEG,IEND (I)	INTEGER         arrays from the parser, delimiters
C                               	for each token
C
C     .. Parameters ..
      INTEGER           MCOLS
      PARAMETER         (MCOLS=200)
C     ..
C     .. Scalar Arguments ..
      INTEGER           ITOK,NLPRGI,NTOK
      CHARACTER         LINE* (*)
C     ..
C     .. Array Arguments ..
      INTEGER           IBEG(*),IEND(*),KPOINT(*)
      CHARACTER*30      LSPRGI(*),LSUSRJ(*)
C     ..
C     .. Local Scalars ..
      INTEGER           JDO,JLOOP,JSTART,JTOK
      CHARACTER         CWORK*30,CWORK2*30,LC1*30,LC2*30,STROUT*400
C     ..
C     .. Local Arrays ..
      LOGICAL SetPrgLab(MCOLS)
C     ..
C     .. External Functions ..
      INTEGER           LENSTR
      EXTERNAL          LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL          CCPUPC,PUTLIN,CCPERR
C     ..
C
      JSTART = ITOK
      JTOK = NTOK
      DO 10 JDO = 1,NLPRGI
        SetPrgLab(JDO) = .FALSE.
   10 CONTINUE
C
C---- Keyword  LABIN - ASSIGN_HKL_LABELS  item1=name1 item2=name2 ...
C
C      item == program label
C      name == user label
C
      IF (NLPRGI.LE.0) THEN
C
C            ***********************
        CALL PUTLIN(' Error this program has no PROGRAM LABELS',
     +       'ERRWIN')
C            ***********************
C
      ELSE IF (JTOK.LE.1) THEN
C
C            ***********************
        CALL PUTLIN('**** Warning: no argument given for LABIN ',
     +       'ERRWIN')
C            ***********************
C
      ELSE
C
C---- Find input label assignments
C
        DO 60 JLOOP = JSTART,NTOK,2
C
          IF ((JLOOP+1).GT.JTOK) THEN
            GO TO 70
          ELSE
            CWORK = LINE(IBEG(JLOOP) :IEND(JLOOP))
            LC1 = CWORK
C
C                *************
            CALL CCPUPC(CWORK)
C                *************
C
            CWORK2 = LINE(IBEG(JLOOP+1) :IEND(JLOOP+1))
            LC2 = CWORK2
C
C                **************
            CALL CCPUPC(CWORK2)
C                **************
C
            DO 20 JDO = 1,NLPRGI
              IF (CWORK.EQ.LSPRGI(JDO)) THEN
                IF (SetPrgLab(JDO)) GOTO 25
                GO TO 50
              ENDIF
   20       CONTINUE

   25       DO 30 JDO = 1,NLPRGI
              IF (CWORK2.EQ.LSPRGI(JDO)) THEN
                IF (SetPrgLab(JDO)) THEN
                 WRITE (STROUT,FMT=
     +            '('' ERROR with label assignments '',A,'' and '',A)') 
     +            CWORK(1:LENSTR(CWORK)),CWORK2(1:LENSTR(CWORK2))
C
C                      ***********************************************
                  CALL PUTLIN(STROUT,'ERRWIN')
                  CALL CCPERR(1,' ERROR Program label assigned twice')
C                      ***********************************************
C
                ENDIF
                GO TO 40
              END IF
   30       CONTINUE
C
            WRITE (STROUT,FMT=
     +        '(''  Neither '',A,'' nor '',A,'' recognised'')')
     +        CWORK(1:LENSTR(CWORK)),CWORK2(1:LENSTR(CWORK2))
C
C                ***********************
            CALL PUTLIN(STROUT,'ERRWIN')
C                ***********************
C
            WRITE (STROUT,FMT=
     +        '(''  OR maybe '',A,'' has been assigned twice'')') 
     +        CWORK(1:LENSTR(CWORK))
C
C                ***********************
            CALL PUTLIN(STROUT,'ERRWIN')
            CALL CCPERR(1, 'Input column assignment does not match'//
     +               ' program labels')
C                ***********************
   40       CONTINUE
            KPOINT(JDO) = -1
            LSUSRJ(JDO) = LC1
            SetPrgLab(JDO) = .TRUE.
            GO TO 60
   50       CONTINUE
            KPOINT(JDO) = -1
            LSUSRJ(JDO) = LC2
            SetPrgLab(JDO) = .TRUE.
          END IF
   60   CONTINUE
        RETURN
   70   CONTINUE
C
C            ***********************
        CALL PUTLIN(' **** Error !!!! for LABIN ****','ERRWIN')
        CALL PUTLIN(' **** Not enough argument pairs of type ',
     +       'ERRWIN')
        CALL PUTLIN('Prog_label = User_label','ERRWIN')
C            ***********************
C
      END IF
C
      END
C
C
C
C     ====================================================
      SUBROUTINE LKYASN(MINDX,NLPRGI,LSPRGI,CTPRGI,LOOKUP)
C     ====================================================
C
C---- There follows a jiffy subroutine to do column assignments, bypassing
C     the need for keyworded input. This is useful in writing little mtz
C     programs, without using Parser in the main program.          PRE
C
C     Read column assignments and make them, for input MTZ file 
C     open for read on index MINDX
C
C     It expects to read from stream 5 a line of the form
C       LABIN  program_label=file_label program_label=file_label . . .
C
C     This routine is useful for simple jiffy programs that don't want 
C     full keyworded input
C
C     MINDX	(I)	INTEGER		file index number for opened MTZ input file
C
C     NLPRGI    (I)	INTEGER		number of input program labels
C
C     LSPRGI	(I)	CHARACTER*30	array of dimension at least NLPRGI
C                               	containing the program label strings
C
C     CTPRGI	(I)	CHAR*1		array of column types for each column: 
C					these will be checked to see that they 
C					match the actual column types in the file. 
C					If you don't want to check column types, 
C					provide blank types here
C					(dimension at least NLPRGI)
C
C     LOOKUP	(O)	INTEGER		array of dimension at least NLPRGI
C					containing column numbers for each 
C					assigned label
C
C     .. Parameters ..
      INTEGER MFILES
      PARAMETER (MFILES=4)
      INTEGER MAXTOK
      PARAMETER (MAXTOK=100)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,NLPRGI
C     ..
C     .. Array Arguments ..
      INTEGER LOOKUP(*)
      CHARACTER*1  CTPRGI(*)
      CHARACTER*30 LSPRGI(*)
C     ..
C     .. Local Scalars ..
      CHARACTER KEY*4,LINE*400,LINE2*400
      INTEGER NTOK,ISTAT,IFAIL
      LOGICAL LEND
C     ..
C     .. Local Arrays ..
      CHARACTER CVALUE(MAXTOK)*4
      INTEGER IBEG(MAXTOK),IEND(MAXTOK),ITYP(MAXTOK),IDEC(MAXTOK)
      REAL    FVALUE(MAXTOK)
C     ..
C     .. External Subroutines ..
      EXTERNAL PARSER,LKYIN,LRASSN,LERROR
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE2,FMT='(A,I3,A,1X,I1,1X,A)')
     +    'From LKYASN : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE2)
C            ************************
C
      ELSE
C
C---- loop to read control input
C
 1      LINE=' '
        NTOK=MAXTOK
C
C            *********************************************************
        CALL PARSER(
     +    KEY,LINE,IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,NTOK,LEND,.TRUE.)
C            *********************************************************
C
        IF(LEND) THEN
          WRITE (LINE2,FMT='(A)')
     +    'From LKYASN : *** End of file found in column assignment ***'
          ISTAT = 1
C
C              ************************
          CALL LERROR(ISTAT,IFAIL,LINE2)
C              ************************
C
          RETURN
        END IF
C
        IF(NTOK.EQ.0) GO TO 1
C
        IF (KEY .EQ. 'LABI') THEN
C
C---- Labin column assignments
C
C              **********************************************
          CALL LKYIN(MINDX,LSPRGI,NLPRGI,NTOK,LINE,IBEG,IEND)
C              **********************************************
C
C---- Column assignments, set lookup
C
C              ****************************************
          CALL LRASSN(MINDX,LSPRGI,NLPRGI,LOOKUP,CTPRGI)
C              ****************************************
C
        ELSE
C
          WRITE (LINE2,FMT='(A,A)')
     +    'From LKYASN:  *** Column assignment should begin',
     +    ' with keyword LABIN ***'
          ISTAT = 1
C
C              ************************
          CALL LERROR(ISTAT,IFAIL,LINE2)
C              ************************
C
        END IF
C
      END IF
C
      END
C
C
C
C     ================================
      SUBROUTINE LABPRT(LABELS,NLABS)
C     ================================
C
C
C---- Utility subroutine for MTZ routines - to output an
C     array of character strings across the page nicely
C     eg labels or types
C     Uses subroutine PUTLIN to output to window CURWIN.
C
C---- Arguments :
C
C     LABELS    (I)	CHARACTER*(*)   Array of dimension (NLABS) containing
C                               	the character strings to be output
C
C     NLABS     (I)	INTEGER         number of labels to be output
C
C
C     .. Scalar Arguments ..
      INTEGER NLABS
C     ..
C     .. Array Arguments ..
      CHARACTER LABELS(NLABS)* (*)
C     ..
C     .. Local Scalars ..
      INTEGER ICOL1,ICOL2,ILEN,JDO10,MAXLEN
      CHARACTER STROUT*400
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL PUTLIN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC LEN
C     ..
C
C---- Set up the length of the output line, also set STROUT to blanks
C
      MAXLEN = 80
      STROUT = ' '
      ICOL1 = 0
      ILEN = LEN(LABELS(1))
C
      IF (ILEN.LE.MAXLEN) THEN
C
C---- Loop over the strings
C     Fill up STROUT until MAXLEN, then output a line, leave one
C     blank between each string
C
        DO 10 JDO10 = 1,NLABS
          ILEN = MAX(LENSTR(LABELS(JDO10)),1)
          ICOL2 = ICOL1 + ILEN
C
          IF (ICOL2.LE.MAXLEN) THEN
            STROUT(ICOL1+1:ICOL2) = LABELS(JDO10) (1:ILEN)
            ICOL1 = ICOL2 + 1
          ELSE
C
C              *************************
            CALL PUTLIN(STROUT,'CURWIN')
C              *************************
C
            STROUT = LABELS(JDO10) (1:ILEN)
            ICOL1 = ILEN + 1
          END IF
   10   CONTINUE
C
C---- Output the last line and return
C
C            ***********************
        CALL PUTLIN(STROUT,'CURWIN')
C            ***********************
C
      END IF
C
      END
C
C
C
C     ========================
      SUBROUTINE LPHIST(MINDX)
C     ========================
C
C
C---- Subroutine to output the history information from the MTZ
C     file header.  This text can be anything the programmer chooses,
C     (but probably programs stamps at least).
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER NHISLM
      PARAMETER (NHISLM=30)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX
C     ..
C     .. Arrays in Common ..
      REAL RBATW,WRANGE,WSRNGE
      INTEGER HDRST,NBATR,NBATW,NCOLW,NHISTL,NREFR,NREFW,RLUN,RPOINT,
     +        WLUN,WOMBAT,NDATMSS,NPLABS
      LOGICAL SORTB,DATMSS
      CHARACTER CBATW*1,PLABS*30,HSCR*80
C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,ILEN,ISTAT,JDO10
      CHARACTER LINE*400
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL BLANK,LERROR,PUTLIN
C     ..
C     .. Common blocks ..
      COMMON /MTZWRC/PLABS(MCOLS,MFILES),HSCR(NHISLM,MFILES),
     +       CBATW(CBLENG,MBATCH,MFILES)
      COMMON /MTZWRK/NCOLW(MFILES),RLUN(MFILES),WLUN(MFILES),
     +       RPOINT(MCOLS,MFILES),WRANGE(2,MCOLS,MFILES),NREFW(MFILES),
     +       NREFR(MFILES),NPLABS(MFILES),NBATW(MFILES),NBATR(MFILES),
     +       WOMBAT(MBATCH,MFILES),HDRST(MFILES),SORTB(MFILES),
     +       NHISTL(MFILES),RBATW(MBLENG,MBATCH,MFILES),WSRNGE(2,MFILES)
     +       ,DATMSS(MCOLS),NDATMSS(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZWRK/,/MTZWRC/
C     ..
C
C---- First check that the MINDX is valid
C
      IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
        WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +    'From LPHIST : Index',MINDX,
     +    ' is out of range (allowed 1..',MFILES,')'
        ISTAT = 2
        IFAIL = -1
C
C            ************************
        CALL LERROR(ISTAT,IFAIL,LINE)
C            ************************
C
      ELSE
C
C---- Then check if there are any history lines present
C
        IF (NHISTL(MINDX).EQ.0) THEN
C
C              ****************
          CALL PUTLIN('* There is no History information ' //
     +         'in this MTZ file','CURWIN')
          CALL BLANK('CURWIN',1)
C              *****************
C
        ELSE
C
C---- Read and print the history lines
C
C              ****************
          CALL PUTLIN('* HISTORY for current MTZ file : ','CURWIN')
          CALL BLANK('CURWIN',1)
C              *****************
C
          DO 10 JDO10 = 1,NHISTL(MINDX)
            ILEN = LENSTR(HSCR(JDO10,MINDX))
C
C                ****************
            CALL PUTLIN(HSCR(JDO10,MINDX) (1:ILEN),'CURWIN')
C                ****************
C
   10     CONTINUE
C
C             *****************
          CALL BLANK('CURWIN',1)
C             *****************
C
        END IF
      END IF
C
      END

C
C
C
C     ==============================
      SUBROUTINE LHPRT(MINDX,IPRINT)
C     ==============================
C
C
C---- Subroutine to print out the header information from the MTZ
C     COMMON blocks for the MTZ file open on index MINDX.
C
C---- Arguments :
C
C     MINDX     (I)	INTEGER         indicates which MTZ file - 1 index
C                               	points to both input and output files
C
C     IPRINT    (I)	INTEGER         print indicator : meaning :
C                               	=1 Brief header info printed (default)
C                               	=2 As above plus history info
C                               	=3 Full header dump, symmetry, alles !
C                               	=4 As 1 plus full symmetry 
C                               	any other value, nothing happens
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MSETS
      PARAMETER (MSETS=MCOLS)
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,IPRINT
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RSYM,VAL_MISS
      INTEGER BATNUM,ISORT,NBATCH,NCOLS,NREFS,NSPGRP,NSYM,NSYMP,
     +          NSETW,SET_ID,CSET_ID,IDEFSET
      CHARACTER CBATR*1,CTYPE*1,LTYPE*1,PGNAM*10,SPGNAM*10,CLABEL*30,
     +          TITLE*70,ENTRY_ID*64,DIFFRN_ID*64

C     ..
C     .. Local Scalars ..
      INTEGER IFAIL,IMAX,IMIN,IPR,ISTAT,JDO10,JDO20,JDO30,JJ
      LOGICAL SORTED,VAL_SET
      CHARACTER CTEMP*1,LINE*400,STROUT*400
      REAL RESMIN,RESMAX
C     ..
C     .. Local Arrays ..
      CHARACTER SYMCHS(MAXSYM)*80
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
      LOGICAL QISNAN
C     ..
C     .. External Subroutines ..
      EXTERNAL BLANK,LABPRT,LERROR,LPHIST,PUTLIN,SYMTR3
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
C     .. Common blocks ..
      COMMON /MTZCHR/TITLE(MFILES),CLABEL(MCOLS,MFILES),
     +       CTYPE(MCOLS,MFILES),SPGNAM(MFILES),LTYPE(MFILES),
     +       CBATR(CBLENG,MBATCH,MFILES),PGNAM(MFILES)
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
      COMMON /MTZHAR/NSETW(MFILES),SET_ID(MSETS,MFILES),
     +       ENTRY_ID(MSETS,MFILES),DIFFRN_ID(MSETS,MFILES),
     +       CSET_ID(MCOLS,MFILES),IDEFSET(MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/,/MTZCHR/
C     ..
C
C---- Check IPRINT is OK
C
      IF ((IPRINT.GE.1) .AND. (IPRINT.LE.4)) THEN
C
C---- Then check that the MINDX is valid
C
        IF ((MINDX.LE.0) .OR. (MINDX.GT.MFILES)) THEN
          WRITE (LINE,FMT='(A,I3,A,1X,I1,1X,A)') 
     +      'From LHPRT : Index',MINDX,
     +      ' is out of range (allowed 1..',MFILES,')'
C
C---- Stop here, since this is a bad error, probably from program
C
          ISTAT = 2
          IFAIL = -1
C
C              ************************
          CALL LERROR(ISTAT,IFAIL,LINE)
C              ************************
C
        ELSE
C
C---- Print title, harvesting dataset info, no. of cols, no of refls
C
C              ***********************
          CALL PUTLIN('* Title:','CURWIN')
          CALL BLANK('CURWIN',1)
          CALL PUTLIN(TITLE(MINDX) (1:LENSTR(TITLE(MINDX))),'CURWIN')
          CALL BLANK('CURWIN',1)

          IF (NSETW(MINDX).GT.0) THEN
            WRITE (STROUT,FMT='(A,I4)') '* Number of Datasets =',
     +        NSETW(MINDX)
            CALL PUTLIN(STROUT,'CURWIN')
            CALL BLANK('CURWIN',1)
            CALL PUTLIN('* Dataset ID, project name, dataset name:',
     +        'CURWIN')
            CALL BLANK('CURWIN',1)
            DO 30 JDO30 = 1,NSETW(MINDX)

              WRITE (STROUT,FMT='(I8,1X,A)') 
     +          SET_ID(JDO30,MINDX),ENTRY_ID(JDO30,MINDX)
              CALL PUTLIN(STROUT,'CURWIN')
              WRITE (STROUT,FMT='(9X,A)') 
     +          DIFFRN_ID(JDO30,MINDX)
              CALL PUTLIN(STROUT,'CURWIN')
C
   30       CONTINUE
            CALL BLANK('CURWIN',1)
          ENDIF

          WRITE (STROUT,FMT='(A,I4)') '* Number of Columns =',
     +      NCOLS(MINDX)
          CALL PUTLIN(STROUT,'CURWIN')
          CALL BLANK('CURWIN',1)
          WRITE (STROUT,FMT='(A,I7)') '* Number of Reflections =',
     +      NREFS(MINDX)
C   Something about missing value flag.
          IF(VAL_SET(1,MINDX)) THEN
              CALL PUTLIN(STROUT,'CURWIN')
              CALL BLANK('CURWIN',1)
              IF( QISNAN(VAL_MISS(1,MINDX))) 
     +        WRITE (STROUT,FMT='(A)') 
     +        '* Missing value set to NaN in input mtz  file'
              IF( .NOT.QISNAN(VAL_MISS(1,MINDX))) 
     +        WRITE (STROUT,FMT='(A,F18.2,A)') 
     +        '* Missing value set to ',VAL_MISS(1,MINDX),
     +        ' in input mtz  file'
          END IF
C
          IF(VAL_SET(2,MINDX)) THEN
              CALL PUTLIN(STROUT,'CURWIN')
              CALL BLANK('CURWIN',1)
              IF( QISNAN(VAL_MISS(2,MINDX))) 
     +        WRITE (STROUT,FMT='(A)') 
     +        '* Missing value set to NaN in output mtz  file'
              IF( .NOT.QISNAN(VAL_MISS(2,MINDX))) 
     +        WRITE (STROUT,FMT='(A,F18.2,A)') 
     +        '* Missing value set to ',VAL_MISS(2,MINDX),
     +        ' in output mtz  file'
          END IF
          CALL PUTLIN(STROUT,'CURWIN')
          CALL BLANK('CURWIN',1)
C              ***********************
C
          IF (NBATCH(MINDX).GT.0) THEN
            WRITE (STROUT,FMT='(A,I4)') '* Number of Batches =',
     +        NBATCH(MINDX)
C
C                ***********************
            CALL PUTLIN(STROUT,'CURWIN')
            CALL BLANK('CURWIN',1)
C                ***********************
C
          END IF
C
C---- Print History lines if required
C
          IF ((IPRINT.EQ.2) .OR. (IPRINT.EQ.3)) THEN
C
C                *************
            CALL LPHIST(MINDX)
C                *************
C
          END IF
C
C---- Output label info - different if long output
C
          IF ((IPRINT.EQ.1) .OR. (IPRINT.EQ.2) .OR. (IPRINT.EQ.4)) THEN
C
C                ************************************
            CALL PUTLIN('* Column Labels :','CURWIN')
            CALL BLANK('CURWIN',1)
            CALL LABPRT(CLABEL(1,MINDX),NCOLS(MINDX))
            CALL BLANK('CURWIN',1)
            CALL PUTLIN( '* Column Types :','CURWIN')
            CALL BLANK('CURWIN',1)
            CALL LABPRT(CTYPE(1,MINDX),NCOLS(MINDX))
            IF (NSETW(MINDX).GT.0.AND.NBATCH(MINDX).EQ.0) THEN
             CALL BLANK('CURWIN',1)
             CALL PUTLIN( '* Associated datasets :','CURWIN')
             CALL BLANK('CURWIN',1)
             WRITE (STROUT,FMT='(200I4)')
     +         (CSET_ID(JDO10,MINDX),JDO10 = 1,NCOLS(MINDX))
             CALL PUTLIN(STROUT,'CURWIN')
            ENDIF
C                ************************************
C
          ELSE IF (IPRINT.EQ.3) THEN
            CALL PUTLIN(
     +   '* Column Labels, Types, Ranges [and Dataset IDs] :','CURWIN')
            CALL BLANK('CURWIN',1)
C                ***********************
C
            DO 10 JDO10 = 1,NCOLS(MINDX)
              CTEMP = CTYPE(JDO10,MINDX)
C
              IF ((CTEMP.EQ.'H') .OR. (CTEMP.EQ.'B') .OR.
     +            (CTEMP.EQ.'Y')) THEN
                IMIN = NINT(CRANGE(1,JDO10,MINDX))
                IMAX = NINT(CRANGE(2,JDO10,MINDX))
                IF (CSET_ID(JDO10,MINDX).GT.0) THEN
                 WRITE (STROUT,FMT='(A,1X,A,2X,2I19,1X,I8)') 
     +            CLABEL(JDO10,
     +            MINDX),CTEMP,IMIN,IMAX,CSET_ID(JDO10,MINDX)
                ELSE
                 WRITE (STROUT,FMT='(A,1X,A,2X,2I19)') 
     +            CLABEL(JDO10,
     +            MINDX),CTEMP,IMIN,IMAX
                ENDIF
              ELSE
                IF (CSET_ID(JDO10,MINDX).GT.0) THEN
                 WRITE (STROUT,FMT='(A,1X,A,2X,2F19.4,1X,I8)') 
     +            CLABEL(JDO10,
     +            MINDX),CTEMP,CRANGE(1,JDO10,MINDX),
     +            CRANGE(2,JDO10,MINDX),CSET_ID(JDO10,MINDX)
                ELSE
                 WRITE (STROUT,FMT='(A,1X,A,2X,2F19.4)') 
     +            CLABEL(JDO10,
     +            MINDX),CTEMP,CRANGE(1,JDO10,MINDX),
     +            CRANGE(2,JDO10,MINDX)
                ENDIF
              END IF
C
C                  ***********************
              CALL PUTLIN(STROUT,'CURWIN')
C                  ***********************
C
   10       CONTINUE
C
          END IF
C
C              *****************
          CALL BLANK('CURWIN',1)
C              *****************
C
C---- Cell dimensions for all
C
C              ***********************
          CALL PUTLIN('* Cell Dimensions :','CURWIN')
          CALL BLANK('CURWIN',1)
          WRITE (STROUT,FMT='(6F9.3)') (CELL(JJ,MINDX),JJ=1,6)
          CALL PUTLIN(STROUT,'CURWIN')
          CALL BLANK('CURWIN',1)
C              ***********************
C
C---- Resolution range for all
C
          IF ((NCOLS(MINDX).GE.3).AND.(CTYPE(1,MINDX).EQ.'H')
     +        .AND.(CTYPE(2,MINDX).EQ.'H')
     +        .AND.(CTYPE(3,MINDX).EQ.'H')
     +        .AND.((SRANGE(1,MINDX).GT.0.0)
     +        .OR.(SRANGE(2,MINDX).GT.0.0) ) ) THEN
C
            RESMIN = 1000.0
            IF(SRANGE(1,MINDX).GT.0.000001)
     +        RESMIN = 1./SQRT(SRANGE(1,MINDX))
            RESMAX = 0.5
            IF(SRANGE(2,MINDX).GT.0.000001)
     +        RESMAX = 1./SQRT(SRANGE(2,MINDX))
C
C                *************************
            CALL PUTLIN('*  Resolution Range :','CURWIN')
            CALL BLANK('CURWIN',1)
            WRITE(STROUT,FMT='(2F12.5,6X,A,F9.3,A,F9.3,A)')
     $        (SRANGE(JJ,MINDX),JJ=1,2),'(',RESMIN,' - ',RESMAX,' A )'
            CALL PUTLIN(STROUT,'CURWIN')
            CALL BLANK('CURWIN',1)
C                *************************
C
          END IF
C
C---- Sort order if it is there
C
          SORTED = .FALSE.
C
          DO 20 JDO20 = 1,5
            IF (ISORT(JDO20,MINDX).NE.0) SORTED = .TRUE.
   20     CONTINUE
C
          IF (SORTED) THEN
C
C                ***********************
            CALL PUTLIN('* Sort Order :','CURWIN')
            CALL BLANK('CURWIN',1)
            WRITE (STROUT,FMT='(5I6)') (ISORT(JJ,MINDX),JJ=1,5)
            CALL PUTLIN(STROUT,'CURWIN')
            CALL BLANK('CURWIN',1)
C                ***********************
C
          ELSE
C
C                ***********************
            CALL PUTLIN('* There is no sort order recorded ' //
     +           'in the MTZ header','CURWIN')
            CALL BLANK('CURWIN',1)
C                ***********************
C
          END IF
C
C---- Symmetry: everything for =3 or =4, else just name & number
C
          IF ((IPRINT.EQ.3) .OR. (IPRINT.EQ.4)) THEN
C
            IF (NSYM(MINDX).GT.0) THEN
              WRITE (STROUT,FMT='(A,I3)')
     +          '* Number of Symmetry Operations = ',NSYM(MINDX)
C
C                  ***********************
              CALL PUTLIN(STROUT,'CURWIN')
C                  ***********************
C
              IF (NSYMP(MINDX).GT.0) THEN
                WRITE (STROUT,FMT='(A,I3)')
     +            '* Number of Primitive Operations = ',NSYMP(MINDX)
C
C                    ***********************
                CALL PUTLIN(STROUT,'CURWIN')
C                    ***********************
C
              END IF
C
              IF (NSPGRP(MINDX).GT.0) THEN
                WRITE (STROUT,FMT='(A,I4,6X,A)') '* Space Group =',
     +            NSPGRP(MINDX),SPGNAM(MINDX)
C
C                    ***********************
                CALL PUTLIN(STROUT,'CURWIN')
C                    ***********************
C
              END IF
C
              IF (LTYPE(MINDX).NE.'?') THEN
                CALL PUTLIN('* Lattice Type = ' // LTYPE(MINDX),
     +               'CURWIN')
C                    ***********************
C
              END IF
C
              IF (PGNAM(MINDX).NE.'?') THEN
                CALL PUTLIN('* Point Group Name = ' //
     +            PGNAM(MINDX),'CURWIN')
C                    ***********************
C
              END IF
C
C                  ***********************
              CALL BLANK('CURWIN',1)
              CALL PUTLIN('* Symmetry Operations :','CURWIN')
              CALL BLANK('CURWIN',1)
C                  ***********************
C
              IPR = 1
C
C                  ************************************************
              CALL SYMTR3(NSYM(MINDX),RSYM(1,1,1,MINDX),SYMCHS,IPR)
C                  ************************************************
C
            END IF
          ELSE
             WRITE (STROUT,FMT='(A,A,A,I5,A)')
     $            '* Space group = ',
     $            SPGNAM(MINDX)(1:LENSTR(SPGNAM(MINDX))),
     $            '  (number ',NSPGRP(MINDX),')'
             CALL PUTLIN(STROUT,'CURWIN')
             CALL BLANK('CURWIN',1)
          END IF
        END IF
      END IF
C
      END
C
C
C
C     =============================================
      SUBROUTINE LBPRT(IBATCH,IPRINT,RBATCH,CBATCH)
C     =============================================
C
C---- Internal MTZ subroutine (I think!) which copies the batch header
C     information as it is stored in raw form on the files, into the
C     common blocks MBTHDR and CBTHDR, and then calls LBPRTH to make
C     some sense of these numbers.
C
C---- Arguments :
C
C     IBATCH    (I)	INTEGER         batch number
C
C     IPRINT    (I)	INTEGER         print indicator : meaning :
C                               	=0 no print
C                               	=1 print batch title only
C                               	=2 also print orientation block
C                               	.GT. 30 also print orientation block
C                                    	BUT TO IPRINT UNIT NUMBER
C
C     RBATCH    (I)	REAL(*)         array from which integer and real batch
C                               	info is decoded by subroutine wbathd
C                               	this should be equivalenced onto the
C                               	appropriate COMMON block in the calling
C                               	program. The first item is nwords,ie how
C                               	many items in the array, if nword is 0
C                               	then only the title is written to header
C
C     CBATCH    (I)	CHARACTER*(*)   as RBATCH, but for character items - no
C                               	nwords however; title is 1st 70 chars of
C                               	CBATCH.
C
C     The lengths of the RBATCH & CBATCH arrays are parameters here!
C
C
C     .. Parameters ..
      INTEGER MBLENG,CBLENG
      PARAMETER (MBLENG=185,CBLENG=70+3*8)
C     ..
C     .. Scalar Arguments ..
      INTEGER IBATCH,IPRINT
C     ..
C     .. Array Arguments ..
      REAL RBATCH(MBLENG)
      CHARACTER CBATCH*(*)
C     ..
C     .. Arrays in Common ..
      REAL RARRAY
      CHARACTER CARRAY*1
C     ..
C     .. Local Scalars ..
      INTEGER JDO20,JDO30,NWORDS
C     ..
C     .. External Subroutines ..
      EXTERNAL LBPRTH
C     ..
C     .. Common blocks ..
      COMMON /CBTHDR/CARRAY(CBLENG)
      COMMON /MBTHDR/RARRAY(MBLENG)
C     ..
C     .. Equivalences ..
      EQUIVALENCE (NWORDS,RARRAY(1))
C     ..
C     .. Save statement ..
      SAVE /MBTHDR/,/CBTHDR/
C     ..
C
C---- Copy from the input arguments to the dummy common block
C     first we have to get NWORDS out
C
      RARRAY(1) = RBATCH(1)
C
      DO 10 JDO20 = 1,NWORDS
        RARRAY(JDO20) = RBATCH(JDO20)
   10 CONTINUE
C
      DO 20 JDO30 = 1,CBLENG
        CARRAY(JDO30) = CBATCH(JDO30:JDO30)
   20 CONTINUE
C
C          *********************
      CALL LBPRTH(IBATCH,IPRINT)
C          *********************
C
      END
C     
C   
C     
C     ================================
      SUBROUTINE LBPRTH(IBATCH,IPRINT)
C     ================================
C     
C     
C     
C---- Internal Subroutine to print out the batch header information
C     from the MTZ COMMON blocks /MBTHDR/ and /CBTHDR/ (already there)
C     
C---- Arguments :
C     
C     IBATCH	(I)	INTEGER         batch number
C     
C     IPRINT    (I)	INTEGER         print indicator : meaning :
C     					=0 no print
C     					=1 print batch title only
C     					=2 also print orientation block
C     					.GT. 30 also print orientation block 
C					TO UNIT NUMBER IPRINT
C     
C     
C     
C     Orientation block data
C     
C     This contains slots for all information that seems to be essential
C     at present. Each group of parameters is padded at the end for future
C     expansion.
C     
C     Data in the orientation block are referred to the "Cambridge"
C     laboratory axis frame: x along the (idealized) X-ray beam, z along
C     usual rotation axis E1 (omega on 3-axis system). The matrix Q converts
C     a vector in the Madnes frame to the Cambridge frame.  Note that the
C     laboratory frame is essentially defined by the vectors e1,e2,e3 &
C     source. It doesn't really seem necessary to carry through a whole lot
C     of crystal and beam tensors, particularly as we have integrated
C     intensities at this stage, but maybe someone will want to, using the
C     allocated padding
C     
C     The general orientation equation is
C     
C     x  =   R M U B h
C     
C     where x    position in laboratory frame
C     R    goniostat matrix
C     M    missetting angle matrix (if relevant, see MISFLG)
C                 PhiZ PhiY PhiX (PHIXYZ)
C     U    crystal orientation matrix UMAT
C     B    cell orthogonalization matrix, derived from cell dimensions
C     h    reflection indices
C     
C     Note that the description below is NOT is the same order as in the
C     common block, in which all the integers come before all the reals 
C     (flagged as I or R in the description below)
C     
CI    NWORDS       number of words in orientation block
CI    NINTGR       number of integers (first part of block,
C                        includes these counts)
CI    NREALS       number of reals
CI    IORTYP       type of orientation block (for possible future use, now = 0)
CI    INTPAD(8)    padding for future use (integers)
C     
C---  Information for this crystal
C     
CR    CELL(6)      cell dimensions  (A & degrees)
CI    LBCELL(6)    refinement flags for cell dimensions
CR    UMAT(3,3)    orientation matrix U. If MISFLG .gt. 0, U is the
C                   "standard" setting when PhiXYZ ==0 
CI    MISFLG       status of "missetting" angles PHIXYZ
C                   = 0  PHIXYZ not used, all orientation in UMAT
C                   = 1  1 set of missetting angles (PHIXYZ(I,1))
C                   = 2  2 sets PHIXYZ(I,J), J=1,2
CR    PHIXYZ(3,2)  missetting angles at beginning & end of rotation 
CI    JUMPAX       reciprocal axis closest to principle goniostat axis E1
C                       (only used for printing)
CI    NCRYST       crystal number: a crystal may contain several batches
CI    LBSETID      dataset number (this indexes a list of datasets in the
C                  file header)
CI    LCRFLG       type of crystal mosaicity information
C                  (=0 for isotropic, =1 anisotropic)
C     *** CRYDAT(12) equivalenced to following ***
CR    ETAD         reflection width (full width) (degrees)  (if LCRFLG=0)
C      or
CR    ETADH,ETADV  horizontal & vertical reflection width   (if LCRFLG=1)
CR    rest of CRYDAT: padding for crystal information (eg more complicated
C                           mosaicity model)
C     ***
C     
C---  Information for this batch
C
CI    LDTYPE       type of data
C                   = 1    oscillation data   (2D spots)
C                   = 2    area detector data (3D spots)
C                   = 3    Laue data
CR    DATUM(3)     datum values of goniostat axes, from which Phi is measured
C                      (degrees)
CR    PHISTT,PHIEND start & stop values of Phi (degrees) relative to datum
CR    PHIRANGE     range of Phi values: typically this will be PHIEND-PHISTT,
C                    but storing this explicitly allows a distinction 
C                    eg between a rotation of +160 degrees from a rotation
C                    of -200 degrees
CI    JSCAXS       goniostat scan axis number (=1,2,3, or =0 for
C                        multiple axis scan
CR    SCANAX(3)    rotation axis in laboratory frame (not yet implemented:
C                      only relevant if JSCAXS=0)
CR    TIME1, TIME2 start & stop times in minutes
CI    NBSCAL       number of batch scales & Bfactors plus SD's
C                   (4 at present, BSCALE, BBFAC & sd's)
C                  set = 0 if batch scales unset
CR    BSCALE       batch scale
CR    BBFAC        batch temperature factor
C                   corresponding scale is exp(-2 B (sin theta/lambda)**2)
CR    SDBSCL       sd (Bscale)
CR    SDBFAC       sd (BBfac)
CR    BATPAD(11)   padding for batch information
C     
C---  Crystal goniostat information
C     
CI    NGONAX       number of goniostat axes (normally 1 or 3)
CI    E1(3),E2(3),E3(3) vectors (in "Cambridge" laboratory frame, see below)
C                            defining the NGONAX goniostat axes
CC    GONLAB(3)  names of the three goniostat axes
CR    GONPAD(12) padding for goniostat information
C     
C---  Beam information
C     
CR    SOURCE(3)    Idealized (ie excluding tilts) source vector 
C                   (antiparallel to beam), in "Cambridge" laboratory frame
CR    S0(3)        Source vector (antiparallel ! to beam), in
C                   "Cambridge" laboratory frame, including tilts
CI    LBMFLG       flag for type of beam information following 
C                   = 0 for ALAMBD, DELAMB only (laboratory source)
C                   = 1     ALAMBD,DELAMB,DELCOR,DIVHD,DIVVD (synchrotron) 
C                           (other options could include white beam)
C     *** BEMDAT(25) equivalenced to following ***
CR    ALAMBD       Wavelength in Angstroms
CR    DELAMB       dispersion Deltalambda / lambda.
CR    DELCOR       Correlated component of wavelength dispersion.
CR    DIVHD        Horizontal beam divergence in degrees.
CR    DIVVD        Vertical beam divergence (may be 0.0 for isotropic beam
C                        divergence.
CR    rest of BEMDAT: padding for beam information
C                      (*** How much here for Laue? ***)
C     ***
C     
C---  Detector information
C     
CI    NDET         number of detectors (current maximum 2)
C     -- for each detector
CR    DXn          crystal to detector distance (mm)
CR    THETAn       detector tilt angle (=Madnes:tau2) (degrees)
CR    DETLMn(2,2)  minimum & maximum values of detector coordinates (pixels)
C                     (i,j): i = 1 minimum, = 2 maximum
C                            j = 1 Ydet,    = 2 Zdet
CR    DETPAD(34)     padding for detector information
C     
C     
C Lengths: MBLENG is total length of block
C     ..
C     .. Common blocks ..
      INTEGER NWORDS,NINTGR,NREALS,IORTYP,LBCELL,MISFLG,
     .     JUMPAX,NCRYST,LCRFLG,LDTYPE,JSCAXS,NBSCAL,NGONAX,LBMFLG,
     +     NDET,LBSETID,INTPAD
      REAL   CELL,UMAT,PHIXYZ,CRYDAT,ETAD,DATUM,
     +     PHISTT,PHIEND,PHIRANGE,SCANAX,TIME1,TIME2,
     +     BSCALE,BBFAC,SDBSCL,SDBFAC,BATPAD,E1,E2,E3,GONPAD,
     +     SOURCE,S0,BEMDAT,ALAMBD,DELAMB,DELCOR,DIVHD,DIVVD,
     +     DX1,THETA1,DETLM1,DX2,THETA2,DETLM2,DETPAD,ETADH,ETADV
      CHARACTER BTITLE*70, GONLAB*8
C     
      COMMON /CBTHDR/BTITLE,GONLAB(3)
C     
      COMMON /MBTHDR/ NWORDS,NINTGR,NREALS,IORTYP,LBCELL(6),MISFLG,
     .     JUMPAX,NCRYST,LCRFLG,LDTYPE,JSCAXS,NBSCAL,NGONAX,LBMFLG,
     +     NDET,LBSETID,INTPAD(8),
     +     CELL(6),UMAT(3,3),PHIXYZ(3,2),CRYDAT(12),DATUM(3),
     +     PHISTT,PHIEND,SCANAX(3),TIME1,TIME2,
     +     BSCALE,BBFAC,SDBSCL,SDBFAC,PHIRANGE,BATPAD(11),
     +     E1(3),E2(3),E3(3),
     +     GONPAD(12),SOURCE(3),S0(3),BEMDAT(25),
     +     DX1,THETA1,DETLM1(2,2),DX2,THETA2,DETLM2(2,2),DETPAD(33)
C     
C     *** Note well the following equivalences: these are to allow for
C     alternative definitions of crystal mosaicity, beam parameters, etc
      EQUIVALENCE (ETAD,CRYDAT(1))
      EQUIVALENCE (ETADH,CRYDAT(1)),(ETADV,CRYDAT(2))
      EQUIVALENCE (BEMDAT(1),ALAMBD)
      EQUIVALENCE (BEMDAT(2),DELAMB)
      EQUIVALENCE (BEMDAT(3),DELCOR)
      EQUIVALENCE (BEMDAT(4),DIVHD)
      EQUIVALENCE (BEMDAT(5),DIVVD)
C ***
C This equivalence is strictly for convenience in printing
      REAL E123(3,3)
      EQUIVALENCE (E123(1,1),E1(1))
C     ..
C
C Save it
      SAVE /CBTHDR/,/MBTHDR/
C
C     .. Parameters ..
      INTEGER MXLLEN,MXLLIN
      PARAMETER (MXLLEN=80,MXLLIN=50)
C     ..                                                     
C     .. Scalar Arguments ..
      INTEGER IBATCH,IPRINT
C     ..
C     .. Local Scalars ..
      INTEGER I,J
      CHARACTER BAXIS*4,STROUT*100
C     ..
C     .. Local Arrays ..
      CHARACTER AXES(4)*4,LINES(MXLLIN)*(MXLLEN),SOMELN(MXLLIN)*(MXLLEN)
      CHARACTER*25 LABTYP(0:3)
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      INTEGER NEXTLN
      EXTERNAL BLANK,PUTLIN,NEXTLN,ADDLIN
C     ..
C     ..
C     .. Data statements ..
      DATA AXES/'none','a*','b*','c*'/
C Types of data
      DATA LABTYP/'*** unknown data type ***','oscillation data',
     +     'area detector data','Laue data'/
C     ..
C     
C     
C---- Check IPRINT is OK
C     
      IF (IPRINT.EQ.0) RETURN
C                      ======
C     
      BAXIS = ' '
C     
C     
      BAXIS = AXES(JUMPAX+1)
C     
C---- Print batch number and title for all
C     
      IF (IPRINT.GT.30) THEN
        WRITE (IPRINT,FMT='(A)') ' Batch number: '
C
      ELSE
C     
C            ***********************
        CALL PUTLIN(' Batch number: ','CURWIN')
C            ***********************
C
      END IF
C
      STROUT = ' '
      WRITE (STROUT,FMT='(1X,I6,4X,A)') IBATCH,BTITLE
C
      IF (IPRINT.GT.30) THEN
        WRITE (IPRINT,FMT='(A)') STROUT(1:MIN(130,LENSTR(STROUT)))
C
      ELSE
C     
C            ***********************
        CALL PUTLIN(STROUT,'CURWIN')
C            ***********************
C
      END IF
C
      IF (IPRINT.EQ.1) RETURN
C                      ======
C     
C---- Clear LINES arrays
C     
      DO 10 I = 1,MXLLIN
        LINES(I) = ' '
        SOMELN(I) = ' '
 10   CONTINUE
C     
C     
C *** Print it all here
C
C---- Set type of data = 0 if invalid number
C
      J = LDTYPE
      IF (J .LT. 1 .OR. J .GT. 3) J=0
C
      WRITE (SOMELN, FMT=6000) 
     +    IBATCH,LABTYP(J),NCRYST,LBSETID,CELL,LBCELL
C
      IF (IPRINT.GT.30) WRITE (IPRINT,FMT=6000) 
     +    IBATCH,LABTYP(J),NCRYST,LBSETID,CELL,LBCELL
C
      CALL ADDLIN(SOMELN,LINES,MXLLIN)
C
      IF (MISFLG .EQ. 0) THEN
        WRITE (SOMELN, FMT=6005) UMAT
C
        IF (IPRINT.GT.30) WRITE (IPRINT, FMT=6005) UMAT
C
        CALL ADDLIN(SOMELN,LINES,MXLLIN)
C
      ELSEIF (MISFLG .GT. 0) THEN
C
        WRITE (SOMELN, FMT=6006) UMAT
C
        IF (IPRINT.GT.30) WRITE (IPRINT, FMT=6006) UMAT
C
        CALL ADDLIN(SOMELN,LINES,MXLLIN)
        WRITE (SOMELN, FMT=6010) ((PHIXYZ(I,J),I=1,3),J=1,MISFLG)
C
        IF (IPRINT.GT.30) 
     +     WRITE (IPRINT, FMT=6010) ((PHIXYZ(I,J),I=1,3),J=1,MISFLG)
C
         CALL ADDLIN(SOMELN,LINES,MXLLIN)
      ENDIF
C
      WRITE (SOMELN, FMT=6020) GONLAB(1),BAXIS
C
      IF (IPRINT.GT.30) 
     +  WRITE (IPRINT, FMT=6020) GONLAB(1),BAXIS
C
      CALL ADDLIN(SOMELN,LINES,MXLLIN)
C
      IF (LCRFLG .EQ. 0) THEN
C
C---- Isotropic mosaicity
C
        WRITE (SOMELN, FMT=6030) ETAD
C
        IF (IPRINT.GT.30) WRITE (IPRINT, FMT=6030) ETAD
C
        CALL ADDLIN(SOMELN,LINES,MXLLIN)
C
      ELSEIF (LCRFLG .EQ. 1) THEN
C
C---- Anisotropic mosaicity
C
        WRITE (SOMELN, FMT=6035) ETADH,ETADV
C
        IF (IPRINT.GT.30) WRITE (IPRINT, FMT=6035) ETADH,ETADV
C
        CALL ADDLIN(SOMELN,LINES,MXLLIN)
      END IF
      WRITE (SOMELN, FMT=6040) (DATUM(I),I=1,NGONAX)
C
      IF (IPRINT.GT.30) 
     +    WRITE (IPRINT, FMT=6040) (DATUM(I),I=1,NGONAX)
C
      CALL ADDLIN(SOMELN,LINES,MXLLIN)
C
      IF (JSCAXS .GT. 0 .AND. JSCAXS .LE. NGONAX) THEN
        WRITE (SOMELN, FMT=6050) GONLAB(JSCAXS)
C
        IF (IPRINT.GT.30) WRITE (IPRINT, FMT=6050) GONLAB(JSCAXS)
C
        CALL ADDLIN(SOMELN,LINES,MXLLIN)
      ENDIF
C
      WRITE (SOMELN, FMT=6060) PHISTT,PHIEND,PHIRANGE,TIME1,TIME2
C
      IF (IPRINT.GT.30) 
     +    WRITE (IPRINT, FMT=6060) PHISTT,PHIEND,PHIRANGE,TIME1,TIME2
C
      CALL ADDLIN(SOMELN,LINES,MXLLIN)
C
C---- Batch scale factors (if set)
C
      IF (NBSCAL .GT. 0) THEN
        IF (NBSCAL .EQ. 4) THEN
          WRITE (SOMELN, 6065) BSCALE,SDBSCL,BBFAC,SDBFAC 
          IF (IPRINT.GT.30) 
     +            WRITE (IPRINT, 6065) BSCALE,SDBSCL,BBFAC,SDBFAC 
          CALL ADDLIN(SOMELN,LINES,MXLLIN)
        ENDIF
      ENDIF
C
      WRITE (SOMELN, FMT=6070) NGONAX,
     +     (GONLAB(J),(E123(I,J),I=1,3),J=1,3)
C
      IF (IPRINT.GT.30) WRITE (IPRINT, FMT=6070) NGONAX,
     +     (GONLAB(J),(E123(I,J),I=1,3),J=1,3)
C
      CALL ADDLIN(SOMELN,LINES,MXLLIN)
      WRITE (SOMELN, FMT=6080) SOURCE,S0
      IF (IPRINT.GT.30) WRITE (IPRINT, FMT=6080) SOURCE,S0
      CALL ADDLIN(SOMELN,LINES,MXLLIN)
C
      IF (LBMFLG .EQ. 0) THEN
C
C---- Monochromatic (laboratory) beam
C
        WRITE (SOMELN, FMT=6090) ALAMBD,DELAMB
C
        IF (IPRINT.GT.30) WRITE (IPRINT, FMT=6090)  ALAMBD,DELAMB
C
        CALL ADDLIN(SOMELN,LINES,MXLLIN)
C
      ELSEIF (LBMFLG .EQ. 1) THEN
C
C---- Monochromatic (synchrotron) beam
C
        WRITE (SOMELN, FMT=6095) ALAMBD,DELAMB,DELCOR,DIVHD,DIVVD
        IF (IPRINT.GT.30)
     +     WRITE (IPRINT, FMT=6095) ALAMBD,DELAMB,DELCOR,DIVHD,DIVVD
        CALL ADDLIN(SOMELN,LINES,MXLLIN)
      END IF
C
      WRITE (SOMELN, FMT=6100) NDET
      IF (IPRINT.GT.30) WRITE (IPRINT, FMT=6100) NDET
      CALL ADDLIN(SOMELN,LINES,MXLLIN)
      WRITE (SOMELN, FMT=6110) DX1, THETA1, DETLM1
      IF (IPRINT.GT.30) WRITE (IPRINT, FMT=6110) DX1, THETA1, DETLM1
      CALL ADDLIN(SOMELN,LINES,MXLLIN)
C
      WRITE (SOMELN, FMT=6120)
      IF (IPRINT.GT.30) WRITE (IPRINT, FMT=6120)
      CALL ADDLIN(SOMELN,LINES,MXLLIN)
C
 6000 FORMAT(/1X,19('++++')//
     * ' Orientation data for batch',I8,5X,A//
     . '   Crystal number ...................',I7/
     . '   Associated dataset ID ............',I7/
     . '   Cell dimensions ..................',6F7.2/
     . '   Cell fix flags ...................',6I7)
 6005 FORMAT(
     . '   Orientation matrix U .............',3F10.4/
     . '       (including setting angles)    ',3F10.4/
     .   37X,3F10.4)
 6006 FORMAT(
     . '   Standard orientation matrix U ....',3F10.4,
     .   2(/37X,3F10.4))
 6010 FORMAT(
     . '   Missetting angles PhiX PhiY PhiZ..',6F7.2)
 6020 FORMAT(
     . '   Reciprocal axis nearest ',A8,'..',3X,A4)
 6030 FORMAT(
     . '   Mosaicity ........................',F7.3)
 6035 FORMAT(
     . '   Mosaicity (horizontal, vertical)..',2F7.3)
 6040 FORMAT(
     . '   Datum goniostat angles (degrees)..',F8.3,2F9.3)
 6050 FORMAT(
     . '   Scan axis ........................',2X,A8)
 6060 FORMAT(
     . '   Start & stop Phi angles (degrees).',2F9.3/
     . '   Range of Phi angles (degrees).....',F9.3/
     . '   Start & stop time (minutes).......',2F9.0)
 6065 FORMAT(
     + '   Batch scale & SD .................',2F9.4/
     + '   Batch B-factor & SD ..............',2F9.4)
 6070 FORMAT(
     . ' Crystal goniostat information :-'/
     . '   Number of goniostat axes..........',I7/
     . '   Goniostat vectors.....',A8,'....',3F9.4/
     . '                    .....',A8,'....',3F9.4/
     . '                    .....',A8,'....',3F9.4)
 6080 FORMAT(
     . ' Beam information :-'/
     . '   Idealized X-ray beam vector.......',3F9.4/
     . '   X-ray beam vector with tilts......',3F9.4)
 6090 FORMAT(
     . '   Wavelength and dispersion ........',2F9.5)
 6095 FORMAT(
     . '   Wavelength and dispersion ........',3F9.5/
     . '   Divergence .......................',2F7.3)
 6100 FORMAT(
     . ' Detector information :-'/
     . '   Number of detectors...............',I7)
 6110 FORMAT(
     . '   Crystal to Detector distance (mm).',F9.3/
     . '   Detector swing angle..............',F9.3/
     . '   Pixel limits on detector..........',4F7.1/)
 6120 FORMAT(1X,19('++++')/)
C
C---- Return if IPRINT.gt. 30
C
      IF (IPRINT.GT.30) RETURN
C
C---- Find out how many lines
C
      J = NEXTLN(LINES,MXLLIN) - 1
      IF (J .LE. 0) RETURN
C                   ======
C     
C     
      DO 40 I = 1,J
C     
C            ***********************
        CALL PUTLIN(LINES(I),'CURWIN')
C            ***********************
C     
 40   CONTINUE
C     
C          *****************
      CALL BLANK('CURWIN',1)
C          *****************
C     
      RETURN
      END 
C
C
C     =========================
      SUBROUTINE SORTUP(N,A,IN)
C     =========================
C
C---- Ref:  Comm. ACM VOL.12 #3 MARCH 1969, R.C.SINGLETON
C
C---- Routine returns order of A in IN - Index Sort, for integer array
C     Lifted from STILLS program.
C
C     .. Scalar Arguments ..
      INTEGER N
C     ..
C     .. Array Arguments ..
      INTEGER A(N),IN(N)
C     ..
C     .. Local Scalars ..
      INTEGER I,IJ,J,K,L,M,T,TT
C     ..
C     .. Local Arrays ..
      INTEGER IL(16),IU(16)
C     ..
C
      DO 10 I = 1,N
        IN(I) = I
   10 CONTINUE
C
      M = 1
      I = 1
      J = N
C
   20 IF (I.GE.J) GO TO 100
   30 K = I
      IJ = (I+J)/2
      T = IN(IJ)
      IF (A(IN(I)).LE.A(T)) GO TO 40
      IN(IJ) = IN(I)
      IN(I) = T
      T = IN(IJ)
C
   40 L = J
      IF (A(IN(J)).GE.A(T)) GO TO 70
      IF (A(IN(J)).LT.A(IN(I))) GO TO 50
      IN(IJ) = IN(J)
      IN(J) = T
      T = IN(IJ)
      GO TO 70
C
   50 IN(IJ) = IN(I)
      IN(I) = IN(J)
      IN(J) = T
      T = IN(IJ)
      GO TO 70
C
   60 IN(L) = IN(K)
      IN(K) = TT
   70 L = L - 1
      IF (A(IN(L)).GT.A(T)) GO TO 70
      TT = IN(L)
   80 K = K + 1
      IF (A(IN(K)).LT.A(T)) GO TO 80
      IF (K.LE.L) GO TO 60
      IF ((L-I).LE. (J-K)) GO TO 90
      IL(M) = I
      IU(M) = L
      I = K
      M = M + 1
      GO TO 110
   90 IL(M) = K
      IU(M) = J
      J = L
      M = M + 1
      GO TO 110
C
  100 M = M - 1
      IF (M.EQ.0) GO TO 140
      I = IL(M)
      J = IU(M)
  110 IF ((J-I).GE.11) GO TO 30
      IF (I.EQ.1) GO TO 20
      I = I - 1
  120 I = I + 1
      IF (I.EQ.J) GO TO 100
      T = IN(I+1)
      IF (A(IN(I)).LE.A(T)) GO TO 120
      K = I
  130 IN(K+1) = IN(K)
      K = K - 1
      IF (A(T).LT.A(IN(K))) GO TO 130
      IN(K+1) = T
      GO TO 120
 140  END
C
C
C
C     ======================================
      SUBROUTINE ADDLIN(NEWLIN,LINES,MAXLIN)
C     ======================================
C
C---- Add newlines from NEWLIN to array LINES
C
C     Input:  NEWLIN(MAXLIN) character array
C
C     Output: LINES(MAXLIN)  character array
C             NEWLIN   cleared on output
C
      INTEGER MAXLIN
      CHARACTER*(*) NEWLIN(MAXLIN),LINES(MAXLIN)
C
      INTEGER I, N, M
C
      INTEGER NEXTLN
      EXTERNAL NEXTLN
C
C Get number of lines to add
      N = NEXTLN(NEWLIN,MAXLIN)-1
      IF (N .LE. 0) RETURN
C Get position to put first line
      M = MAX(1,NEXTLN(LINES,MAXLIN))
C
      DO 10, I = 1, N
         LINES(M) = NEWLIN(I)
         M = M+1
         NEWLIN(I) = ' '
 10   CONTINUE
      END
C
C
C
C     =====================================
      INTEGER FUNCTION NEXTLN(LINES,MAXLIN)
C     =====================================
C
C---- Find next blank line in character array LINES
C
C     Input: LINES(MAXLIN)  character array
C
C     Returns: NEXTLN       number of next blank line (= -1 if full)
C
      INTEGER MAXLIN
      CHARACTER*(*) LINES(MAXLIN)
C
      INTEGER I
C
      DO 10, I = MAXLIN,1,-1
         IF (LINES(I) .NE. ' ') THEN
            NEXTLN = I+1
            RETURN
         END IF
 10   CONTINUE 
C Array full
      NEXTLN = -1
      END
C
C
C     ===============================================
      SUBROUTINE LSTRSL(MINDX,A,B,C,ALPHA,BETA,GAMMA)
C     ===============================================
C
C---- Routine to calculate coefficients for (sin(theta)/lambda)**2 from
C     h,k,l for general axes
C
C     first calculated the components of input axes in an orthonormal
C     basis, then calculate components of reciprocal axes in same basis
C
C---- Input angles are in degrees
C
C---- This is exactly the same as the subroutine SETRSL in SYMLIB,
C     except that it stores it's variables in the internal MTZ
C     COMMON blocks, and will work if more than one file is open at once.
C
C---- Arguments :
C
C     MINDX	(I)	INTEGER		indicates which MTZ file
C
C     A,B,C,ALPHA,BETA,GAMMA (I) REAL	cell parameters
C     
C     .. Parameters ..
      INTEGER MFILES
      PARAMETER (MFILES=4)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX
      REAL A,ALPHA,B,BETA,C,GAMMA
C     ..
C     .. Arrays in Common ..
      REAL COEFHH,COEFHK,COEFHL,COEFKK,COEFKL,COEFLL
C     ..
C     .. Local Scalars ..
      REAL AXST,AYST,AZST,BYST,BZST,CZST
      REAL AR,AX,BR,BX,BY,CX,CY,CZ,DTORAD,GR,HALF,QMIN,STMAX,TMAX,TWO,
     +     XX,ZERO
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,COS,MAX,SIN,SQRT
C     ..
C     .. Common blocks ..
      COMMON /MRCPLT/COEFHH(MFILES),COEFHK(MFILES),COEFHL(MFILES),
     +       COEFKK(MFILES),COEFKL(MFILES),COEFLL(MFILES)
C     ..
      SAVE /MRCPLT/
C     ..
C     .. Data statements ..
      DATA QMIN,ZERO/5.0E-7,0.0/
      DATA DTORAD/0.01745329/
      DATA TWO/2.0/
      DATA HALF/0.5/
C     ..
C
C---- dtorad = 3.1415927/180.0
C
C     set up formulae for monoclinic or orthorhombic crystals
C     alpha, beta, gamma set to 90 where appropriate
C
      IF (ALPHA.EQ.90.0) THEN
        AR = 1.57079635
      ELSE
        AR = ALPHA*DTORAD
      END IF
      IF (BETA.EQ.90.0) THEN
        BR = 1.57079635
      ELSE
        BR = BETA*DTORAD
      END IF
      IF (GAMMA.EQ.90.0) THEN
        GR = 1.57079635
      ELSE
        GR = GAMMA*DTORAD
      END IF
C
C---- Put a-axis along x
C
      AX = A
C
C---- put b-axis in x-y plane
C
      BX = COS(GR)*B
      BY = SIN(GR)*B
C
C---- Be sure by is positive
C
      BY = ABS(BY)
C
C---- C falls where it is
C
      CX = COS(BR)*C
      CY = (B*C*COS(AR)-BX*CX)/BY
C
C---- CZ determined by length of c
C
      XX = C*C - CX*CX - CY*CY
      CZ = SQRT(XX)
      TMAX = MAX(AX,BY)
      TMAX = MAX(TMAX,CZ)
      IF (ABS(BX/TMAX).LT.QMIN) BX = ZERO
      IF (ABS(CX/TMAX).LT.QMIN) CX = ZERO
      IF (ABS(CY/TMAX).LT.QMIN) CY = ZERO
C
C     WRITE (6,FMT=6000) AX,BX,BY,CX,CY,CZ
C
C---- Now for reciprocal vectors
C
      AXST = HALF/AX
      AYST = -AXST*BX/BY
      AZST = - (AXST*CX+AYST*CY)/CZ
      BYST = HALF/BY
      BZST = -CY*BYST/CZ
      CZST = HALF/CZ
      STMAX = MAX(AXST,BYST)
      STMAX = MAX(STMAX,CZST)
      IF (ABS(AYST/STMAX).LT.QMIN) AYST = ZERO
      IF (ABS(AZST/STMAX).LT.QMIN) AZST = ZERO
      IF (ABS(BZST/STMAX).LT.QMIN) BZST = ZERO
C     WRITE (6,FMT=6002) AXST,AYST,BYST,AZST,BZST,CZST
C
C---- The other three components of reciprocal vectors are zero
C     coefficient of h*h
C
      COEFHH(MINDX) = AXST*AXST + AYST*AYST + AZST*AZST
C
C---- Coefficient of h*k
C
      COEFHK(MINDX) = (AYST*BYST+AZST*BZST)*TWO
C
C---- coefficient of h*l
C
      COEFHL(MINDX) = AZST*CZST*TWO
C
C---- coefficient of k*k
C
      COEFKK(MINDX) = BYST*BYST + BZST*BZST
C
C---- coef of k*l
C
      COEFKL(MINDX) = BZST*CZST*TWO
C
C---- coef of l*l
C
      COEFLL(MINDX) = CZST*CZST
      END
C
C
C     ===================================
      REAL FUNCTION LSTLSQ(MINDX,IH,IK,IL)
C     ===================================
C
C---- Calculate (sin(theta)/lambda)**2 from h,k,l; coef's set by call to
C     LSTRSL, for the file open on index MINDX
C
C---- This is exactly the same as the function STHLSQ in SYMLIB,
C     except that it stores it's variables in the internal MTZ
C     COMMON blocks, and will work if more than one file is open at once.
C
C---- Arguments :
C
C     MINDX	(I)	INTEGER		indicates which MTZ file
C
C     IH,IK,IL	(I)	INTEGER 	Miller indices for the reflection
C
C
C     .. Parameters ..
      INTEGER MFILES
      PARAMETER (MFILES=4)
C     ..
C     .. Scalar Arguments ..
      INTEGER MINDX,IH,IK,IL
C     ..
C     .. Arrays in Common ..
      REAL COEFHH,COEFHK,COEFHL,COEFKK,COEFKL,COEFLL
C     ..
C     .. Common blocks ..
      COMMON /MRCPLT/COEFHH(MFILES),COEFHK(MFILES),COEFHL(MFILES),
     +       COEFKK(MFILES),COEFKL(MFILES),COEFLL(MFILES)
C     ..
C     .. Save Statements ..
      SAVE /MRCPLT/
C     ..
C
      LSTLSQ = IH*IH*COEFHH(MINDX) + IH*IK*COEFHK(MINDX) + 
     +         IH*IL*COEFHL(MINDX) + IK*IK*COEFKK(MINDX) + 
     +         IK*IL*COEFKL(MINDX) + IL*IL*COEFLL(MINDX)
C
      END

C
C
C
C     =============================================
      SUBROUTINE IS_MAGIC (VAL_MAGIC,VALTST,LVALMS)
C     =============================================
C
C---- Function  to test whether a number is  "magic" 
C     Returns LVALMS TRUE if it is - otherwise LVALMS FALSE
C
C
C---- Arguments :
C
C     VAL_MAGIC (I)     REAL            Missing value flag
C                                       as "magic" for this mtz file.
C
C     VALTST    (I)     REAL            Number to test to see if it is defined 
C                                       as "magic" for this mtz file.
C
C     LVALMS    (O)     LOGICAL        Returns LVALMS TRUE if VALTST is "magic"
C                                      FALSE if it is not, or if
C                                      there is no "missing" number set.
C
C     ..
C     .. Scalar Arguments ..
      REAL VAL_MAGIC, VALTST
      LOGICAL LVALMS
C     ..
C     .. External Functions ..
      LOGICAL QISNAN
      EXTERNAL QISNAN
C     ..
      LVALMS = .FALSE.
      IF (QISNAN(VAL_MAGIC)) THEN
        IF (QISNAN(VALTST)) LVALMS = .TRUE.
      ELSE
        IF(VALTST .EQ. VAL_MAGIC) LVALMS = .TRUE.
      END IF
      END
C
C
C
C     ========================================
      SUBROUTINE EQUAL_MAGIC(MINDX,ADATA,NCOL)
C     ========================================
C
C     Sets an array of NCOL to VAL_MISS(2,MINDX), the appropriate value
C                                                 for the output file.
C
C---- Arguments :
C
C
C     MINDX     (I)     INTEGER         indicates which MTZ file - 1 index
C                                       points to both input and output files
C
C     ADATA      (I)     REAL            array of dimension at least NCOL
C     					containing the reflection record  with
C     					"missing" values set  to VAL_MAGICA
C
C
C     NCOL       (I)	INTEGER         Array size of ADATA
C
C
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Arguments ..
      REAL ADATA(*)
      INTEGER MINDX, NCOL
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RSYM,VAL_MISS
      INTEGER BATNUM,ISORT,NBATCH,NCOLS,NREFS,NSPGRP,NSYM,NSYMP
      LOGICAL VAL_SET
C     ..
C     .. Local Scalars ..
      INTEGER JDO10
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +       VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/
C     ..
C
C   If the file has been opened for Writing VAL_MISS(2,MINDX) will be set
C
      DO 10 JDO10 = 1,NCOL
        ADATA(JDO10) = VAL_MISS(2,MINDX)
 10   CONTINUE
      END
C
C
C
C     =============================================================
      SUBROUTINE RESET_MAGIC(MINDX,ADATA,BDATA,NCOL,VAL_MAGICA,
     +                                                  VAL_MAGICB)
C     =============================================================
C
C     Resets an array containing Missing value flags VAL_MAGICA  to
C                one  containing Missing value flags VAL_MAGICB
C     If MINDX > 0 then VAL_MAGICA is taken to be the value of the MNF
C                stored in the MTZ header, for MTZ file MINDX. Else the 
C                passed value is taken. The MNF pertaining to the data 
C                need not necessarily be the same as that stored in the
C                header.
C     This allows you to list arrays containing Nan entries.
C     
C
C---- Arguments :
C
C
C     MINDX     (I)     INTEGER         indicates which MTZ file - 1 index
C                                       points to both input and output files
C
C     ADATA      (I)     REAL            array of dimension at least NCOL
C     					containing the reflection record  with
C     					"missing" values set  to VAL_MAGICA
C
C     BDATA      (O)	REAL            Array of dimension at least NCOL
C     					containing the reflection record  with
C     					"missing" values reset  to VAL_MAGIC
C
C     NCOL       (I)	INTEGER         Array size of ADATA
C
C     VAL_MAGICA (I)    REAL            If MINDX = 0 then this value will be
C                                       treated as the MNF pertaining to the
C                                       data. Otherwise 
C
C     VAL_MAGICB (I)	REAL            "Missing value" flag to reset in BDATA
C                                        to allow record to be printed.
C
C     .. Parameters ..
      INTEGER MFILES,MCOLS,MBATCH
      PARAMETER (MFILES=4,MCOLS=200,MBATCH=5000)
      INTEGER MBLENG
      PARAMETER (MBLENG=185)
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Arguments ..
      REAL ADATA(*),BDATA(*),VAL_MAGICA,VAL_MAGICB
      INTEGER MINDX,NCOL
C     ..
C     .. Arrays in Common ..
      REAL CELL,CRANGE,SRANGE,RBATR,RSYM,VAL_MISS
      INTEGER BATNUM,ISORT,NBATCH,NCOLS,
     +        NREFS,NSPGRP,NSYM,NSYMP
      LOGICAL VAL_SET
C     ..
C     .. Local Scalars ..
      INTEGER JDO10
      LOGICAL LVALMS
C     ..
C     .. External Rotuines ..
      EXTERNAL IS_MAGIC
C     ..
C     .. Common blocks ..
      COMMON /MTZHDR/CELL(6,MFILES),NSYM(MFILES),NSYMP(MFILES),
     +       RSYM(4,4,MAXSYM,MFILES),NCOLS(MFILES),NREFS(MFILES),
     +       NBATCH(MFILES),BATNUM(MBATCH,MFILES),ISORT(5,MFILES),
     +       CRANGE(2,MCOLS,MFILES),NSPGRP(MFILES),
     +       RBATR(MBLENG,MBATCH,MFILES),SRANGE(2,MFILES),
     +     VAL_MISS(2,MFILES),VAL_SET(2,MFILES)
C     ..
C     .. Save statement ..
      SAVE /MTZHDR/
C     ..
C
C  If file only opened for Reading .
      IF (MINDX .GT. 0) VAL_MAGICA = VAL_MISS(1,MINDX)
C
C   If the file has been opened for Writing VAL_SET(2,MINDX) will be set
C      and VAL_MISS(2,MINDX) too 
C
      IF (MINDX .GT. 0) THEN
        IF (VAL_SET(2,MINDX)) VAL_MAGICA = VAL_MISS(2,MINDX)
      ENDIF
C
      BDATA(1) = ADATA(1)
      BDATA(2) = ADATA(2)
      BDATA(3) = ADATA(3)
      DO 10 JDO10 = 4,NCOL
        BDATA(JDO10) = ADATA(JDO10)
        CALL IS_MAGIC (VAL_MAGICA,ADATA(JDO10),LVALMS)
        IF(LVALMS) BDATA(JDO10) = VAL_MAGICB
 10   CONTINUE
      END
         
      SUBROUTINE  CELLCHK(CELL1,CELL2,ERRFRC,IERR)
C    
C     Check consistency of CELL1 and CELL2 
C
C---- Arguments :
C
C     CELL1     (I)     REAL            array containing first set of
C                                       cell parameters
C
C     CELL2     (I)     REAL            array containing second set of
C                                       cell parameters
C
C     ERRFRC    (I)     REAL            fractional error allowed.
C
C     IERR      (O)     INTEGER         =1 if problem, =0 otherwise
C

C     Arguments
      INTEGER IERR
      REAL CELL1(6),CELL2(6),ERRFRC

C     Local variables
      INTEGER II
      REAL ACHK,CONV,ALPH,BET,GAMM,SUM,V,VOL1,VOL2
      CHARACTER WORD*5

      IERR = 0
      ACHK = 0.00
      WORD = '  '
      CONV = 3.14159/180.0

C   Calculate volume of CELL1
      ALPH = CELL1(4)*CONV
      BET = CELL1(5)*CONV
      GAMM = CELL1(6)*CONV
      SUM = (ALPH+BET+GAMM)*0.5
      V = SQRT(SIN(SUM-ALPH)*SIN(SUM-BET)*SIN(SUM-GAMM)*SIN(SUM))
      VOL1 = 2.0*CELL1(1)*CELL1(2)*CELL1(3)*V

C   Calculate volume of CELL2
      ALPH = CELL2(4)*CONV
      BET = CELL2(5)*CONV
      GAMM = CELL2(6)*CONV
      SUM = (ALPH+BET+GAMM)*0.5
      V = SQRT(SIN(SUM-ALPH)*SIN(SUM-BET)*SIN(SUM-GAMM)*SIN(SUM))
      VOL2 = 2.0*CELL2(1)*CELL2(2)*CELL2(3)*V

C   Check agreement of volumes
      ACHK = ABS(0.5*(Vol1 - Vol2))/(Vol1 + Vol2)
      IF(ACHK .GT. ERRFRC) GO TO 10

C   Check individual dimensions
      ACHK = 0.00
      DO 550 II = 1,6
        ACHK = ABS(0.5*(Cell2(II)-Cell1(II)))/
     +                   (Cell2(II)+Cell1(II)) + ACHK
        IF (ACHK.GT.3.0*ERRFRC) WORD='Large'
        IF (ACHK.GT.  ERRFRC) WORD='Small'
 550  CONTINUE

C   If cells agree, return
      IF(WORD.EQ.'  ') RETURN
C
  10  CONTINUE
      WRITE(6,'(/,3A)')'-----',
     +         WORD,'Difference in cell parameters detected'
      WRITE(6,'(A,6F8.3,F14.2)')' ----- First  Cell and Volume :',
     +                               CELL1,VOL1
      WRITE(6,'(A,6F8.3,F14.2)')' ----- Second Cell and Volume :',
     +                               CELL2,VOL2
      IERR = 1
C
      RETURN
      END
