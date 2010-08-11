/*                                                                          */
/* % Copyright Daresbury Laboratory 1992--1995                              */
/* % This is a CCP4 `part (i)' file for the purposes of copyright.          */
/* % See the CCP4 distribution conditions for explanation.                  */
/*                                                                          */
/* % \documentstyle[a4wide,times,noweb,makeidx]{article}                    */
/*                                                                          */
/* % \newcommand{\ac}[1]{{\rm\normalshape\sc #1}}   % acronym               */
/*                                                                          */
/* \documentclass{article}                                                  */
/* \usepackage{a4wide,times,noweb,makeidx}                                  */
/* \newcommand{\ac}[1]{\textsc{#1}}   % acronym                             */
/* \newcommand{\meta}[1]{\mbox{$\langle$\sl #1\/$\rangle$}}                 */
/* \newcommand{\ft}{\idx{Fortran}}                                          */
/* \newcommand{\idx}[1]{#1\index{#1}}                                       */
/* \newcommand{\fixme}[1]{\index{Fixme!}[{\bf Fixme!:} #1\@.]}              */
/*                                                                          */
/* \title{C library routines}\date{$ $Date: 1999/12/21 12:39:21 $ $}        */
/* \author{This version: Dave Love, Daresbury}                              */
/*                                                                          */
/* \makeindex                                                               */
/*                                                                          */
/* \noweboptions{longchunks,smallcode}                                      */
/*                                                                          */
/* \begin{document}                                                         */
/*                                                                          */
/* \maketitle                                                               */
/*                                                                          */
/* \noindent                                                                */
/* This file contains the lowest level routines for the CCP4 Program        */
/* Suite, mainly for i/o (as required by the {\tt diskio} routines) and     */
/* bit-twiddling.  It's been partly re-engineered from a non-literate C     */
/* file and isn't properly documented yet.  \fixme{It should generate user  */
/*   documentation eventually}                                              */
/* \bigskip                                                                 */
/*                                                                          */
/* \tableofcontents                                                         */
/*                                                                          */
/*                                                                          */
/* \section{Summary}                                                        */
/*                                                                          */
/* The following routines are defined:                                      */
/* \bigskip                                                                 */
/*                                                                          */
/* \noindent                                                                */
/* \begin{tabular}{ll}                                                      */
/*                                                                          */
/* Routine and  arguments &                      Purpose \\                 */
/* \hline                                                                   */
/* [[ustenv(string, result)]]         & set an environment variable  \\     */
/* [[copen(iunit,filnam,istat)]]      & open random access file using [[fopen]] \\ */
/* [[qclose(iunit)]]                  & shut random access file using [[fclose]] \\ */
/* [[qmode(iunit,mode,nmcitm)]]       & change size of item in file ops. \\ */
/* [[qread(iunit,array,nitems,ier)]]  & [[fread]] from random access file \\ */
/* [[qwrite(iunit,array,nitems)]]     & [[fwrite]] to random access file \\ */
/* [[qrarch(iunit,ipos,ireslt)]] & set up diskio number translation \\      */
/* [[qwarch(iunit,ipos)]] & write `machine stamp' to diskio file \\         */
/* [[qseek(iunit,irec,iel,lrecl)]]    & [[fseek]] within random access file \\ */
/* [[qback(iunit,lrecl)]]             & backspace within random access file \\ */
/* [[qskip(iunit,lrecl)]]             & skip forward within random access file \\ */
/* [[cqinq(iunit,lfilnm,filnam,length)]] & inquire file status on the given stream \\ */
/* [[qlocate(iunit,locate)]]          & current position within random access file  */
/* \end{tabular}                                                            */
/*                                                                          */
/*                                                                          */
/* \section{Portability}                                                    */
/*                                                                          */
/* We aim for compatibility with K\&R\index{K&R@K\&R C} and                 */
/* \index{ANSI C@\ac{ansi} C} \ac{ansi}~C\@ as well as \idx{VAX             */
/*   C}\index{C!VAX}.  One particularly annoying                            */
/* consequence is that we can't rely on [[#elif]] preprocessor              */
/* directives.  I don't know whether anything needs to be changed for the   */
/* new \idx{DEC C}, which is apparently \ac{ansi}\dots{}                    */
/*                                                                          */
/*                                                                          */
/* \section{Code}                                                           */
/*                                                                          */
/* These are the components of the code.  The \LA{}guarded code\RA{} is     */
/* executed when we've identified the platform.                             */
/*                                                                          */
/* A literate program like this is designed for human consumption.  It      */
/* comprises `chunks' of code interspersed in commentary.  Chunks,          */
/* indicated by \LA{}\dots\RA{} in code, are macro-substituted by their     */
/* definitions (starting with \LA{}\dots\endmoddef) to produce              */
/* compilable code.  The definitions of chunks may be added to later, as    */
/* inidcated by \LA{}\dots\plusendmoddef.  Chunks are cross-referenced      */
/* by their trailing tag.                                                   */
/*                                                                          */
/* <*>=                                                                     */
/* This was a literate program.  library.nw is the original source,
   from which library.c was generated by `notangle' and from which
   printable LaTeX can be produced by `noweave' if you have those
   tools.  The noweb system is available in
   anonymous@bellcore.com:pub/norman at the time of writing. */
/* \section{Global variables}                                               */
/* \subsection{Initialised variables}                                       */
/*                                                                          */
/*  Include library.h                                                       */
#include "library.h"
/* <global variables>=                                                      */
static char rcsid[] = "$Id: library.c,v 1.50 1999/12/21 12:39:21 mdw Exp $";
static int initialised =  0;    /* flag to initialise data and file streams */
/*  These DISKIO file modes used to include a [[b]] since they're           */
/* binary.  This caused serious lossage when reading a (scratch) file       */
/* just after writing it ([[fread]] returned [[0]] but without any error    */
/* indication).  I've no idea why.  However, I note that the \ac{ansi} C    */
/* [[b]] doesn't have any effect in \ac{posix} anyway.  In \idx{VAX} C      */
/* [[b]] means `no conversion of carriage-control information is            */
/* attempted'.  It's not clear to me whether you want this or not.  Both    */
/* possibilites {\em seem\/} to work OK \dots\ but not in \idx{DEC~C}, at   */
/* least in \idx{OpenVMS}\@.                                                */
/* This is something to watch out for on other                              */
/* systems.\index{portability!possible problem}                             */
/*                                                                          */
/* <global variables>=                                                      */
#if defined(__DECC) && defined(VMS) || defined (_WIN32)
static char *file_attribute[] = { /* DISKIO file modes */
  "wb+",   /* 'UNKNOWN'   open as 'OLD'/'NEW' check existence */
  "wb+",   /* 'SCRATCH'   open as 'OLD' and delete on closing */
  "rb+",   /* 'OLD'       file MUST exist or program halts */
  "wb+",   /* 'NEW'       create (overwrite) new file */
  "rb"     /* 'READONLY'  self explanatory */
#else
static char *file_attribute[] = {
  "w+",   /* 'UNKNOWN'   open as 'OLD'/'NEW' check existence */
  "w+",   /* 'SCRATCH'   open as 'OLD' and delete on closing */
  "r+",   /* 'OLD'       file MUST exist or program halts */
  "w+",   /* 'NEW'       create (overwrite) new file */
  "r"     /* 'READONLY'  self explanatory */
#endif
};                                                                      
/* Here is a table of bytes per item for the different i/o modes            */
/* available.  Note the \idx{machine dependencies} in here.  The            */
/* \idx{assumption} is that we have a 32-bit machine and that               */
/* [[int]]$\equiv$[[INTEGER]]([[*4]]), [[short]]$\equiv$[[INTEGER*2]],      */
/* [[float]]$\equiv$[[REAL]].                                               */
/*                                                                          */
/* <global variables>=                                                      */
static int item_sizes[] = {
  (int) sizeof (char),                                          /* 0: bytes */
  (int) sizeof (short int),                      /* 1: (integer) half words */
  (int) sizeof (float),                                   /* 2: reals/words */
  (int) sizeof (int),           /* 3: `short complex' (pairs of half words).
                                   NB int rather than 2*short since must fit
                                   into fortran integer */
  (int) 2*sizeof (float),                    /* 4: complex (pairs of words) */
  (int) sizeof (int),           /* 5: not used */
  (int) sizeof (int)            /* 6: integers */
};
/* \subsection{Uninitialised variables}                                     */
/*                                                                          */
/* <global variables>=                                                      */
static FILE *file_stream[MAXFILES];                 /* Pointer to disk file */
static char file_name[MAXFILES][MAXFLEN];      /* Pointer to disk file name */
static int  file_bytes_per_item[MAXFILES];/* Pointer to disk file item size */
static int  file_is_scratch[MAXFILES];    /* Indicates if file is 'SCRATCH' */
static int  file_last_op [MAXFILES];    /* see man fopen rd/wr combinations */
static int file_mode[MAXFILES];               /* diskio mode of each stream */
/* <global variables>=                                                      */
static uint16 nativeIT = NATIVEIT; /* machine integer type */ 
static uint16 nativeFT = NATIVEFT; /* machine float type */
/* <global variables>=                                                      */
union float_uint_uchar {
    float32 f;
    uint32 i;
    uint8 c[4];
/*    sint8 s[4]; */
  };
static int
    Iconvert[MAXFILES],         /* integer convserion needed on read*/
    Fconvert[MAXFILES];         /* real convserion needed on read*/
/****************************************************************************
 * Prototype subroutines                                                    *
 ****************************************************************************/
#if defined (PROTOTYPE)
  static size_t flength (char *s, int len);

  static void fatal (char *message);

  static void cqprint (char *message);

  static void file_fatal (char *message, char *file);

  static void vaxF2ieeeF (union float_uint_uchar *buffer, int size);

  static void ieeeF2vaxF (union float_uint_uchar *buffer, int size);

  static void convexF2ieeeF (union float_uint_uchar *buffer, int size);

  static void ieeeF2convexF (union float_uint_uchar *buffer, int size);

#if CALL_LIKE_HPUX
  void ustenv (char *str, int *result, int Lstr);
#endif
#if CALL_LIKE_STARDENT
  void USTENV (struct Str_Desc *str, int *result);
#endif
#if CALL_LIKE_SUN
  void ustenv_ (char *str, int *result, int Lstr);
#endif
#if CALL_LIKE_MVS
  void __stdcall USTENV (char *str, int *result, int Lstr);
#endif

#if CALL_LIKE_HPUX
  void cunlink (char *filename, int Lfilename);
#endif
#if CALL_LIKE_STARDENT
  void CUNLINK (struct Str_Desc *filename);
#endif
#if defined (VMS)
  void CUNLINK (struct dsc$descriptor_s *filename);
#endif
#if CALL_LIKE_SUN
  void cunlink_ (char *filename, int Lfilename);
#endif
#if CALL_LIKE_MVS
  void __stdcall CUNLINK (char *filename, int Lfilename);
#endif

#if CALL_LIKE_HPUX
  void copen (int *iunit, char *filename, int *istat, int Lfilename);
#endif
#if CALL_LIKE_STARDENT
  void COPEN (int *iunit, struct Str_Desc *filename, int *istat);
#endif
#if defined (VMS)
  void COPEN (int *iunit, struct dsc$descriptor_s *filename, int *istat);
#endif
#if CALL_LIKE_SUN
  void copen_ (int *iunit, char *filename, int *istat, int Lfilename);
#endif
#if CALL_LIKE_MVS
  void __stdcall COPEN (int *iunit, char *filename, int Lfilename, int *istat);
#endif

#if CALL_LIKE_HPUX
  void qrarch (int *iunit, int *ipos, int *ireslt);
#endif
#if CALL_LIKE_STARDENT
  void QRARCH (int *iunit, int *ipos, int *ireslt);
#endif
#if defined (VMS)
  void QRARCH (int *iunit, int *ipos, int *ireslt);
#endif
#if CALL_LIKE_SUN
  void qrarch_ (int *iunit, int *ipos, int *ireslt);
#endif
#if CALL_LIKE_MVS
  void __stdcall QRARCH (int *iunit, int *ipos, int *ireslt);
#endif

#if CALL_LIKE_HPUX
  void qwarch (int *iunit, int *ipos);
#endif
#if CALL_LIKE_STARDENT
  void QWARCH (int *iunit, int *ipos);
#endif
#if defined (VMS)
  void QWARCH (int *iunit, int *ipos);
#endif
#if CALL_LIKE_SUN
  void qwarch_ (int *iunit, int *ipos);
#endif
#if CALL_LIKE_MVS
  void __stdcall QWARCH (int *iunit, int *ipos);
#endif

#if CALL_LIKE_HPUX
  void qclose (int *iunit);
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void QCLOSE (int *iunit);
#endif
#if CALL_LIKE_SUN
  void qclose_ (int *iunit);
#endif
#if CALL_LIKE_MVS
  void __stdcall QCLOSE (int *iunit);
#endif

#if CALL_LIKE_HPUX
  void qmode (int *iunit, int *mode, int *size);
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void QMODE (int *iunit, int *mode, int *size);
#endif
#if CALL_LIKE_SUN
  void qmode_ (int *iunit, int *mode, int *size);
#endif
#if CALL_LIKE_MVS
  void __stdcall QMODE (int *iunit, int *mode, int *size);
#endif

#if CALL_LIKE_HPUX
  void qread (int *iunit, uint8 * buffer, int *nitems, int *result);
#endif
#if defined (VMS) || defined (ardent) || defined (titan) || defined (stardent)
  void QREAD (int *iunit, uint8 * buffer, int *nitems, int *result);
#endif
#if CALL_LIKE_SUN
  void qread_ (int *iunit, uint8 * buffer, int *nitems, int *result);
#endif
#if CALL_LIKE_MVS
  void __stdcall QREAD (int *iunit, uint8 * buffer, int *nitems, int *result);
#endif

#if CALL_LIKE_HPUX
  void qreadc (int *iunit, char * buffer, int *result, int Lbuffer);
#endif
#ifdef VMS
  void QREADC (int *iunit, struct dsc$descriptor_s *buffer, int *result);
#endif
#if CALL_LIKE_STARDENT
  void QREADC (int *iunit, struct Str_Desc *buffer, int *result);
#endif
#if CALL_LIKE_SUN
  void qreadc_ (int *iunit, char * buffer, int *result, int Lbuffer);
#endif
#if CALL_LIKE_MVS 
  void __stdcall QREADC (int *iunit, char * buffer, int *result, int Lbuffer);
#endif

#if CALL_LIKE_HPUX
  void qwrite (int *iunit, uint8 * buffer, int *nitems);
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void QWRITE (int *iunit, uint8 * buffer, int *nitems);
#endif
#if CALL_LIKE_SUN
  void qwrite_ (int *iunit, uint8 * buffer, int *nitems);
#endif
#if CALL_LIKE_MVS
  void __stdcall QWRITE (int *iunit, uint8 * buffer, int *nitems);
#endif

#if CALL_LIKE_HPUX
  void qwritc (int *iunit, char * buffer, int Lbuffer);
#endif
#if defined (VMS)
  void QWRITC (int *iunit, struct dsc$descriptor_s *buffer);
#endif
#if defined CALL_LIKE_STARDENT
  void QWRITC (int *iunit, struct Str_Desc *buffer);
#endif
#if CALL_LIKE_SUN
  void qwritc_ (int *iunit, char * buffer, int Lbuffer);
#endif
#if CALL_LIKE_MVS
  void __stdcall QWRITC (int *iunit, char * buffer, int Lbuffer);
#endif

#if CALL_LIKE_HPUX
  void qseek (int *iunit, int *irec, int *iel, int *lrecl);
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void QSEEK (int *iunit, int *irec, int *iel, int *lrecl);
#endif
#if CALL_LIKE_SUN
  void qseek_ (int *iunit, int *irec, int *iel, int *lrecl);
#endif
#if CALL_LIKE_MVS
  void __stdcall QSEEK (int *iunit, int *irec, int *iel, int *lrecl);
#endif

#if CALL_LIKE_HPUX
  void qback (int *iunit, int *lrecl);
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void QBACK (int *iunit, int *lrecl);
#endif
#if CALL_LIKE_SUN
  void qback_ (int *iunit, int *lrecl);
#endif
#if CALL_LIKE_MVS
  void __stdcall QBACK (int *iunit, int *lrecl);
#endif

#if CALL_LIKE_HPUX
  void qskip (int *iunit, int *lrecl);
#endif
#if defined (VMS) || defined (ardent) || defined (titan) || defined (stardent)
  void QSKIP (int *iunit, int *lrecl);
#endif
#if CALL_LIKE_SUN
  void qskip_ (int *iunit, int *lrecl);
#endif
#if CALL_LIKE_MVS
  void __stdcall QSKIP (int *iunit, int *lrecl);
#endif

#if CALL_LIKE_HPUX
  void cqinq (int *istrm, char *filnam, int *length, int len_filnam);
#endif
#if CALL_LIKE_STARDENT
  void CQINQ (int *istrm, struct Str_Desc *filnam, int *length);
#endif
#if defined (VMS)
  void CQINQ (int *istrm, struct dsc$descriptor_s *filnam, int *length);
#endif
#if CALL_LIKE_SUN
  void cqinq_ (int *istrm, char *filnam, int *length, int len_filnam);
#endif
#if CALL_LIKE_MVS
  void __stdcall CQINQ (int *istrm, char *filnam, int len_filnam, int *length);
#endif

#if CALL_LIKE_HPUX
  void qlocate (int *iunit, int *locate);
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void QLOCATE (int *iunit, int *locate);
#endif
#if CALL_LIKE_SUN
  void qlocate_ (int *iunit, int *locate);
#endif
#if CALL_LIKE_MVS
  void __stdcall QLOCATE (int *iunit, int *locate);
#endif

#ifdef _AIX
  void idate (int iarray);
#endif

#if defined (__hpux) || defined (_AIX)
  void gerror (char *str, int Lstr);

  int ierrno ();

  void itime (int array);

  float etime (float tarray);
#endif

#if defined(F2C) || defined(G77)
  int exit_ (int *status);

  int time_ ();

  int getpid_ ();

  int isatty_ (int *lunit);

  int idate_ (int *iarray);

  int gerror_ (char *str, int Lstr);

  int ierrno_ ();

  int itime_ (int *array);

  doublereal etime_ (float *tarray);

  int ibset_ (int *a, int *b);

  int ibclr_ (int *a, int *b);

  int btest_ (int *a, int *b);
#endif

#if CALL_LIKE_HPUX
  void qnan (union float_uint_uchar *realnum);
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void QNAN (union float_uint_uchar *realnum);
#endif
#if CALL_LIKE_SUN
  void qnan_ (union float_uint_uchar *realnum);
#endif
#if CALL_LIKE_MVS
  void __stdcall QNAN (union float_uint_uchar *realnum);
#endif

#if CALL_LIKE_HPUX
  int cisnan (union float_uint_uchar *realnum);
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  int CISNAN (union float_uint_uchar *realnum);
#endif
#if CALL_LIKE_SUN
  int cisnan_ (union float_uint_uchar *realnum);
#endif
#if CALL_LIKE_MVS
  int __stdcall CISNAN (union float_uint_uchar *realnum);
#endif

#if CALL_LIKE_HPUX
  void hgetlimits (int *IValueNotDet, float *ValueNotDet);
  void cmkdir (const char *path, const char *cmode, int *result, int Lpath, int Lmode);
  void cchmod (const char *path, const char *cmode, int *result, int Lpath, int Lmode);
#endif
#if defined (VMS) 
  void HGETLIMITS (int *IValueNotDet, float *ValueNotDet);
  void CMKDIR (struct dsc$descriptor_s *path, struct dsc$descriptor_s *cmode, 
          int *result);
  void CCHMOD (struct dsc$descriptor_s *path, struct dsc$descriptor_s *cmode, 
          int *result);
#endif
#if CALL_LIKE_STARDENT
  void HGETLIMITS (int *IValueNotDet, float *ValueNotDet);
  void CMKDIR (struct Str_Desc *path, struct Str_Desc *cmode, int *result);
  void CCHMOD (struct Str_Desc *path, struct Str_Desc *cmode, int *result);
#endif
#if CALL_LIKE_SUN
  void hgetlimits_ (int *IValueNotDet, float *ValueNotDet);
  void cmkdir_ (const char *path, const char *cmode, int *result, int Lpath, int Lmode);
  void cchmod_ (const char *path, const char *cmode, int *result, int Lpath, int Lmode);
#endif
#if CALL_LIKE_MVS
  void __stdcall HGETLIMITS (int *IValueNotDet, float *ValueNotDet);
  void __stdcall CMKDIR (const char *path, int Lpath, const char *cmode, int Lmode, 
           int *result);
  void __stdcall CCHMOD (const char *path, int Lpath, const char *cmode, int Lmode,
           int *result);
#endif
/****************************************************************************
*  End of prototypes                                                        *
*****************************************************************************/
#endif

/****************************************************************************/
/* %def file_last_op file_mode                                              */
/*                                                                          */
/* \section{Internal routines}                                              */
/*                                                                          */
/* This gets the length of a \ft{} string ([[character*]]\meta{len}         */
/* variable) \meta{s} with trailing blanks removed.  \fixme{Avoid lossage   */
/*   on null/blank string}                                                  */
/*                                                                          */
/* <internal routines>=                                                     */
#if defined (PROTOTYPE)
static size_t flength (char *s, int len)
#else
static size_t flength(s, len)
char *s;
int len;
#endif
{
  while (s[--len] == ' ');
  return (++len);
}
/* This interface to [[ccperr]] avoids mixing C and \ft{} i/o, as was       */
/* originally done.\index{error reporting}                                  */
/*                                                                          */
/* <internal routines>=                                                     */
#if defined (PROTOTYPE)
static void fatal (char *message)
#else
static void fatal(message)
char *message;
#endif
{
  int mone = -1;
#if CALL_LIKE_HPUX
  extern void ccperr();

  ccperr (&mone, message, (int) strlen(message));
#endif
#if CALL_LIKE_STARDENT
  extern void CCPERR();
  struct Str_Desc str;

  str.Str_length = (int) strlen(message);
  str.Str_pointer = message;
  CCPERR (&mone, &str);
#endif
#if defined (VMS)
  int zero=0;
  extern void CCPERR();
  extern void QPRINT();
  struct dsc$descriptor_s str;

  str.dsc$a_pointer = strerror(errno, vaxc$errno);
  str.dsc$w_length = (int) strlen(str.dsc$a_pointer);
  str.dsc$b_dtype = DSC$K_DTYPE_T;
  str.dsc$b_class = DSC$K_CLASS_S;
  QPRINT (&zero, &str);
  str.dsc$a_pointer = message;
  str.dsc$w_length = (int) strlen(message);
  str.dsc$b_dtype = DSC$K_DTYPE_T;
  str.dsc$b_class = DSC$K_CLASS_S;
  CCPERR (&mone, &str);
#endif
#if CALL_LIKE_SUN
  extern void ccperr_();

  ccperr_ (&mone, message, (int) strlen(message));
#endif
#if CALL_LIKE_MVS
  extern void __stdcall _CCPERR();
  CCPERR (&mone, message, (int) strlen(message));
#endif
 }
/* This prints a non-fatal [[message]] using the Fortran i/o.               */
/*                                                                          */
/* <internal routines>=                                                     */
#if defined (PROTOTYPE)
static void cqprint (char *message)
#else
static void cqprint (message)
char *message;
#endif
{
  int zero = 0;
#if CALL_LIKE_HPUX
  extern void qprint();

  qprint (&zero, message, (int) strlen(message));
#endif
#if CALL_LIKE_STARDENT
  extern void QPRINT();
  struct Str_Desc str;

  str.Str_length = (int) strlen(message);
  str.Str_pointer = message;
  QPRINT (&zero, &str);
#endif
#if defined (VMS)
  extern void QPRINT();
  struct dsc$descriptor_s str;

  str.dsc$a_pointer = message;
  str.dsc$w_length = (int) strlen(message);
  str.dsc$b_dtype = DSC$K_DTYPE_T;
  str.dsc$b_class = DSC$K_CLASS_S;
  QPRINT (&zero, &str);
#endif
#if CALL_LIKE_SUN
  extern void qprint_();

  qprint_ (&zero, message, (int) strlen(message));
#endif
#if CALL_LIKE_MVS
  extern void __stdcall _QPRINT();
  QPRINT (&zero, message, (int) strlen(message));
#endif
 }
/* This reports a fatal error with a given file.                            */
/*                                                                          */
/* <internal routines>=                                                     */
#if defined (PROTOTYPE)
static void file_fatal (char *message, char *file)
#else
static void file_fatal (message, file)
char *message;
char *file;
#endif
{
  char *buff;
  size_t l;

  l = strlen (message) + strlen (file) + 1;
  buff = malloc (l);
  if (buff == NULL)
    fatal ("Memory allocation failed");
  buff[0] = '\0';
  strcat (buff, message);
  strcat (buff, file);
  fatal (buff);
}
/* \subsection{Non-\ac{ieee} floating-point conversion}                     */
/*                                                                          */
/* These conversion routines are based on \idx{HDF}, but do the             */
/* conversion in-place.  They do the obvious conversion between \idx{VAX},  */
/* \ac{ieee}\index{IEEE@\ac{ieee}} and \idx{Convex} formats implied by      */
/* the routine names.                                                       */
/*                                                                          */
/* <internal routines>=                                                     */
#if defined (PROTOTYPE)
static void vaxF2ieeeF(union float_uint_uchar buffer[], int size)
#else
static void vaxF2ieeeF(buffer, size)
union float_uint_uchar buffer[];
int size;
#endif
{
  union float_uint_uchar out;
  unsigned char exp;
  int i;
  
  for (i = 0; i < size; i++) {
    exp = (buffer[i].c[1] << 1) | (buffer[i].c[0] >> 7); /* extract exponent */
    if (!exp && !buffer[i].c[1])        /* zero value */
      out.c[0] = out.c[1] = out.c[2] = out.c[3] = 0;
    else if (exp > 2) {         /* normal value */
      out.c[0] = buffer[i].c[1] - (uint8)1; /* subtracts 2 from exponent */
      /* copy mantissa, LSB of exponent */
      out.c[1] = buffer[i].c[0];
      out.c[2] = buffer[i].c[3];
      out.c[3] = buffer[i].c[2];
    } else if (exp) {           /* denormalized number */
      int shft;

      out.c[0] = buffer[i].c[1] & 0x80; /* keep sign, zero exponent */
      shft = 3 - exp;
      /* shift original mant by 1 or 2 to get denormalized mant */
      /* prefix mantissa with '1'b or '01'b as appropriate */
      out.c[1] = (uint8)((buffer[i].c[0] & 0x7f) >> shft) |
        (uint8)(0x10 << exp);
      out.c[2] = (uint8)(buffer[i].c[0] << (8-shft)) |
        (uint8)(buffer[i].c[3] >> shft);
      out.c[3] = (uint8)(buffer[i].c[3] << (8-shft)) |
        (uint8)(buffer[i].c[2] >> shft);
    } else {                    /* sign=1 -> infinity or NaN */
      out.c[0] = 0xff;          /* set exp to 255 */
      /* copy mantissa */
      out.c[1] = buffer[i].c[0] | (uint8)0x80; /* LSB of exp = 1 */
      out.c[2] = buffer[i].c[3];
      out.c[3] = buffer[i].c[2];
    }
    buffer[i] = out;            /* copy back result */
  }
}
/* <internal routines>=                                                     */
#if defined (PROTOTYPE)
static void ieeeF2vaxF(union float_uint_uchar buffer[], int size)
#else
static void ieeeF2vaxF(buffer, size)
union float_uint_uchar buffer[];
int size;
#endif
{
  union float_uint_uchar out;
  unsigned char exp;
  int i;

  for (i=0; i<size; i++) {
    exp = (buffer[i].c[0]<<1) | (buffer[i].c[1]>>7); /* extract exponent */
    if (exp) {                  /* non-zero exponent */
      /* copy mantissa, last bit of exponent */
      out.c[0] = buffer[i].c[1];
      out.c[2] = buffer[i].c[3];
      out.c[3] = buffer[i].c[2];
      if (exp < 254)            /* normal value */
        out.c[1] = buffer[i].c[0] + (uint8)1; /* actually adds two to exp */
      else {                    /* infinity or NaN */
        if (exp == 254)         /* unrepresentable - OFL */
          /* set mant=0 for overflow */
          out.c[0] = out.c[1] = out.c[2] = out.c[3] = 0; 
        out.c[0] &= 0x7f;       /* set last bit of exp to 0 */
        out.c[1] = 0x80;        /* sign=1 exp=0 -> OFL or NaN.  this will raise
                                   a reserved operand exception if used. */
      }
    } else if (buffer[i].c[1] & 0x60) { /* denormalized value */
      int shft;
      
      shft = (buffer[i].c[1] & 0x40) ? 1 : 2; /* shift needed to normalize */
      /* shift mantissa */
      /* note last bit of exp set to 1 implicitly */
      out.c[0] = (uint8)(buffer[i].c[1] << shft) |
        (uint8)(buffer[i].c[2] >> (8-shft));
      out.c[3] = (uint8)(buffer[i].c[2] << shft) |
        (uint8)(buffer[i].c[3] >> (8-shft));
      out.c[2] = (uint8)(buffer[i].c[3] << shft);
      out.c[1] = (uint8)(buffer[i].c[0] & 0x80); /* sign */
      if (shft==1) {            /* set exp to 2 */
        out.c[1] |= 0x01;
        out.c[0] &= 0x7f;       /* set LSB of exp to 0 */
      }
    } else                      /* zero */
      out.c[0] = out.c[1] = out.c[2] = out.c[3] = 0;
    buffer[i] = out;            /* copy back the result */
  }
}
/* The \idx{Convex} format is like the \idx{VAX} with a different byte      */
/* order.  Convex does provide                                              */
/* \ac{ieee}$\leftrightarrow$native\index{IEEE@\ac{ieee}}                   */
/* conversion routines, but we need [[convexF2ieeeF]] anyhow.               */
/*                                                                          */
/* <internal routines>=                                                     */
#if defined (PROTOTYPE)
static void convexF2ieeeF(union float_uint_uchar buffer[], int size)
#else
static void convexF2ieeeF(buffer, size)
union float_uint_uchar buffer[];
int size;
#endif
{
  union float_uint_uchar out;
  unsigned char exp;
  int i;
  
  for (i = 0; i < size; i++) {
    exp = (buffer[i].c[0]<<1) | (buffer[i].c[1]>>7); /* extract exponent */
    if (!exp && !buffer[i].c[0])        /* zero value */
      out.c[0] = out.c[1] = out.c[2] = out.c[3] = 0;
    else if (exp > 2) {         /* normal value */
      out.c[0] = buffer[i].c[0] - (uint8)1; /* subtracts 2 from exponent */
      /* copy mantissa, LSB of exponent */
      out.c[1] = buffer[i].c[1];
      out.c[2] = buffer[i].c[2];
      out.c[3] = buffer[i].c[3];
    } else if (exp) {           /* denormalized number */
      int shft;
      
      out.c[0] = buffer[i].c[0] & 0x80; /* keep sign, zero exponent */
      shft = 3 - exp;
      /* shift original mant by 1 or 2 to get denormalized mant */
      /* prefix mantissa with '1'b or '01'b as appropriate */
      out.c[1] = (uint8)((buffer[i].c[1] & 0x7f) >> shft) |
        (uint8)(0x10 << exp);
      out.c[2] = (uint8)(buffer[i].c[1] << (8-shft)) |
        (uint8)(buffer[i].c[2] >> shft);
      out.c[3] = (uint8)(buffer[i].c[2] << (8-shft)) |
        (uint8)(buffer[i].c[3] >> shft);
    } else {                    /* sign=1 -> infinity or NaN */
      out.c[0] = 0xff;          /* set exp to 255 */
      /* copy mantissa */
      out.c[1] = buffer[i].c[1] | (uint8)0x80; /* LSB of exp = 1 */
      out.c[2] = buffer[i].c[2];
      out.c[3] = buffer[i].c[3];
    }
    buffer[i] = out;            /* copy back result */
  }
}
/* <internal routines>=                                                     */
#if defined (PROTOTYPE)
static void ieeeF2convexF(union float_uint_uchar buffer[], int size)
#else
static void ieeeF2convexF(buffer, size)
union float_uint_uchar buffer[];
int size;
#endif
{
  union float_uint_uchar out;
  unsigned char exp;
  int i;

  for (i=0; i < size; i++) {
    exp = (uint8)(buffer[i].c[0] << 1) |
      (uint8)(buffer[i].c[1] >> 7); /* extract exponent */
    if (exp) {                  /* non-zero exponent */
      /* copy mantissa, last bit of exponent */
      out.c[1] = buffer[i].c[1];
      out.c[3] = buffer[i].c[3];
      out.c[2] = buffer[i].c[2];
      if (exp < 254)            /* normal value */
        out.c[0] = buffer[i].c[0] + (uint8)1; /* actually adds two to exp */
      else {                    /* infinity or NaN */
        if (exp == 254)         /* unrepresentable - OFL */
          /* set mant=0 for overflow */
          out.c[0] = out.c[1] = out.c[2] = out.c[3] = 0; 
        out.c[1] &= 0x7f;       /* set last bit of exp to 0 */
        out.c[0] = 0x80;        /* sign=1 exp=0 -> OFL or NaN.  this will raise
                                   a reserved operand exception if used. */
      }
    } else if (buffer[i].c[1] & 0x60) { /* denormalized value */
      int shft;
      
      shft = (buffer[i].c[1] & 0x40) ? 1 : 2; /* shift needed to normalize */
      /* shift mantissa */
      /* note last bit of exp set to 1 implicitly */
      out.c[1] = (uint8)(buffer[i].c[1] << shft) |
        (uint8)(buffer[i].c[2] >> (8-shft));
      out.c[2] = (uint8)(buffer[i].c[2] << shft) |
        (uint8)(buffer[i].c[3] >> (8-shft));
      out.c[3] = (uint8)(buffer[i].c[3] << shft);
      out.c[0] = (uint8)(buffer[i].c[0] & 0x80); /* sign */
      if (shft==1) {            /* set exp to 2 */
        out.c[0] |= 0x01;
        out.c[1] &= 0x7f;       /* set LSB of exp to 0 */
      }
    } else                      /* zero */
      out.c[0] = out.c[1] = out.c[2] = out.c[3] = 0;
    buffer[i] = out;            /* copy back the result */
  }
}
/* \section{Miscellaneous routines}                                         */
/* \subsection{{\tt subroutine ustenv(\meta{string}, \meta{result})}}       */
/*                                                                          */
/* This sets an environment variable \meta{var} to \meta{val}, where the    */
/* argument \meta{string}[[==']]\meta{var}[['//'='//']]\meta{val}[[']].     */
/* This is for use by the `\idx{logical name}' mechanism for specifying     */
/* file connexions.  Note that a \idx{VMS} varsion is supplied in {\tt      */
/*   vms.for} and that there is no standard way of setting and              */
/* environment variable.  In a minimal \ac{posix} system it might be        */
/* necessary to twiddle the environment strings explicitly.                 */
/*                                                                          */
/*                                                                          */
/* <miscellaneous routines>=                                                */
#if ! defined (VMS)
/* <ustenv code>=                                                           */
#if CALL_LIKE_HPUX
  void ustenv (str, result, Lstr)
  char *str;
  int *result;
  int Lstr;
#endif
#if CALL_LIKE_STARDENT
  void USTENV (struct Str_Desc *str, int *result)
#endif
#if CALL_LIKE_SUN
  void ustenv_ (char *str, int *result, int Lstr)
#endif
#if CALL_LIKE_MVS
  void __stdcall USTENV (char *str, int Lstr, int *result)
#endif
{
  int putenv ();
  size_t Length;
  char name[MAXFLEN], value[MAXFLEN], *temp;

#if CALL_LIKE_STARDENT
  Length = flength (str->Str_pointer, str->Str_length);
  if (Length > MAXFLEN) Length = MAXFLEN - 1;
  (void) strncpy (name, str->Str_pointer, Length);
#else
  Length = flength (str, Lstr);
  if (Length > MAXFLEN) Length = MAXFLEN - 1;
  (void) strncpy (name, str, Length);
#endif
  name[Length] = '\0'; 
#if defined (sgi) || defined (sun) || defined (__hpux) || \
    defined(_AIX) || defined(ultrix) || defined (__OSF1__) || \
    defined (__osf__) || defined (__FreeBSD__) || defined (linux) || \
    defined (titan) || defined (_WIN32)
      /* putenv is the POSIX.1, draft 3 proposed mechanism */
      /* ESV seems to have it in the SysVile universe */
  temp = (char *) malloc (MAXFLEN);
  if (temp == NULL) fatal("USTENV: Memory allocation failed");
  (void) strcpy (temp, name);
  *result = putenv (temp);
  /* note the necessary lack of free() */
#else
  /* setenv is not POSIX */
  temp = (char *) strchr (name, '='); /* BSD might have to use `index' */
  if (temp != NULL) {
    *temp = '\0';
    temp++;
    (void) strcpy (value, temp);
  };
  *result = setenv (name, value, 1);
#endif
}
#endif
/* \subsection{{\tt subroutine cunlink (\meta{filename})}}                  */
/* This unlinks \meta{filename} from the directory.  It's intended for      */
/* use with scratch files, so that they can be hidden when opened but       */
/* still be available as long as they remain connected (see [[CCPOPN]]).    */
/* This functionality doesn't seem to exist in \idx{VMS}\@.  Failure to     */
/* unlink isn't fatal (it's been observed, apparently spuriously).          */
/*                                                                          */
/* <miscellaneous routines>=                                                */
#if CALL_LIKE_HPUX
  void cunlink (filename, Lfilename)
  char *filename;
  int Lfilename;
#endif
#if CALL_LIKE_STARDENT
  void CUNLINK (struct Str_Desc *filename)
#endif
#if defined (VMS)
  void CUNLINK (struct dsc$descriptor_s *filename)
#endif
#if CALL_LIKE_SUN
  void cunlink_ (char *filename, int Lfilename)
#endif
#if CALL_LIKE_MVS
  void __stdcall CUNLINK(char *filename, int Lfilename)
#endif
{
  size_t Length;
  char tempfile[MAXFLEN];

#ifdef VMS
  return;                       /* can't do it */
#else
#  if CALL_LIKE_STARDENT
    Length = flength (filename->Str_pointer, filename->Str_length);
    if (Length > MAXFLEN) Length = MAXFLEN - 1;
    (void) strncpy (tempfile, filename->Str_pointer, Length);
#  else
    Length = flength (filename, Lfilename);
    if (Length > MAXFLEN) Length = MAXFLEN - 1;
    (void) strncpy (tempfile, filename, Length);
#  endif
  tempfile[Length] = '\0';
  if (unlink (tempfile) != 0)
    cqprint("CUNLINK: Can't unlink");
#endif /* VMS */
}
/* \section{Dynamic memory allocation}                                      */
/* It's nice to be able to determine array sizes at run time to avoid       */
/* messy recompilation.  The only way effectively to get dynamic            */
/* allocation in Fortran77 reasonably portably is to do the allocation,     */
/* e.g.\ in C, and invoke the Fortran routine passed as a parameter with    */
/* pointers to the allocated memory which it will treat as arrays.  If we   */
/* want to allow more than one array, it's more tricky.                     */
/*                                                                          */
/* \subsection{{\tt subroutine ccpal1 (\meta{routne}, \meta{n}.             */
/*     \meta{type}, \meta{length})}}                                        */
/* Arranges to call subroutine \meta{routne} with \meta{n} array            */
/* arguments.  Each has a type indicated by \meta{type}$(i)$ and a length   */
/* given by \meta{length}($i$).  \meta{type} is an integer array with       */
/* values 1, 2, 3, 4 inidcating {\tt                                        */
/*   INTEGER}, {\tt REAL}, {\tt DOUBLE PRECISION} and {\tt COMPLEX}         */
/* respectively.                                                            */
/* It's not immediately clear what all the Fortran/C                        */
/* conventions are for passing [[CHARACTER]] arrays, so we'll arrange a     */
/* higher-level interface and have [[types]] here just numeric.  The        */
/* Fortran ([[CCPALC]]) will also do argument validation.  Also the rules   */
/* for passing external routines as arguments aren't clear---assume         */
/* the obvious way.                                                         */
/*                                                                          */
/* There's a \idx{VMS} Fortran version of this, although the code here      */
/* does work fine in VMS\@.                                                 */
/*                                                                          */
/* NB: there's a possibility of a hook here to use memory-mapped files on   */
/* systems with the capability and insufficient VM\@.                       */
/*                                                                          */
/* Under protest, this now allocates zeroed storage for where programs      */
/* make bad assumptions.                                                    */
/*                                                                          */
/* <miscellaneous routines>=                                                */
#ifndef VMS                     /* we'll use the Fortran version in VMS*/
#ifndef _MVS
#if CALL_LIKE_HPUX
  void ccpal1 (routne, n, type, length)
  void (* routne) ();
  int *n, type[], length[];
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void CCPAL1 (void (* routne) (), int *n, int type[], int length[])
#endif
#if CALL_LIKE_SUN
  void ccpal1_ (void (* routne) (), int *n, int type[], int length[])
#endif
#if CALL_LIKE_MVS
  void __stdcall CCPAL1 (void (* routne) (), int *n, int type[], int length[])
#endif
{
  int i, size, *leng[13];
  void *pointer[13];

  for (i=0; i<*n; i++) {
    switch (type[i]) {
    case 1:
      size = item_sizes[6]; break; /* integer */
    case 2:
      size = item_sizes[2]; break; /* real */
    case 3:
      size = 2*item_sizes[2]; break; /* double */
    case 4:
      size = 2*item_sizes[2]; break; /* complex */
    case 5:
      size = item_sizes[1]; break; /* bytes (logical or integer *1) */
    }
    pointer[i+1] = calloc ((size_t) length[i], (size_t) size);
    if (pointer[i+1] == NULL) fatal ("CCPALC: can't allocate memory");
    leng[i+1] = &(length[i]);   /* convenience */
  }
  switch (*n) {
  case 1:
    (* routne) (leng[1], pointer[1]);
    break;
  case 2:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2]);
    break;
  case 3:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3]);
    break;
  case 4:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4]);
    break;
  case 5:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4],
                leng[5], pointer[5]);
    break;
  case 6:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4],
                leng[5], pointer[5], leng[6], pointer[6]);
    break;
  case 7:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4],
                leng[5], pointer[5], leng[6], pointer[6],
                leng[7], pointer[7]);
    break;
  case 8:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4],
                leng[5], pointer[5], leng[6], pointer[6],
                leng[7], pointer[7], leng[8], pointer[8]);
    break;
  case 9:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4],
                leng[5], pointer[5], leng[6], pointer[6],
                leng[7], pointer[7], leng[8], pointer[8],
                leng[9], pointer[9]);
    break;
  case 10:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4],
                leng[5], pointer[5], leng[6], pointer[6],
                leng[7], pointer[7], leng[8], pointer[8],
                leng[9], pointer[9], leng[10], pointer[10]);
    break;
  case 11:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4],
                leng[5], pointer[5], leng[6], pointer[6],
                leng[7], pointer[7], leng[8], pointer[8],
                leng[9], pointer[9], leng[10], pointer[10],
                leng[11], pointer[11]);
    break;
  case 12:
    (* routne) (leng[1], pointer[1], leng[2], pointer[2],
                leng[3], pointer[3], leng[4], pointer[4],
                leng[5], pointer[5], leng[6], pointer[6],
                leng[7], pointer[7], leng[8], pointer[8],
                leng[9], pointer[9], leng[10], pointer[10],
                leng[11], pointer[11], leng[12], pointer[12]);
    break;
  }
  for (i=0; i<*n; i++)
    free (pointer[i+1]);
}
#endif /* VMS */
#endif
/* \section{Diskio routines}                                                */
/*                                                                          */
/* \subsection{{\tt subroutine copen(\meta{iunit}, \meta{filename},         */
/*     \meta{istat})}}                                                      */
/* Opens \meta{filename} on diskio stream \meta{iunit}.  \meta{istat}       */
/* corresponds to the open mode given to [[qopen]], from which [[copen]]    */
/* is always called---see diskio documentation.                             */
/*                                                                          */
/* <diskio routines>=                                                       */
#if CALL_LIKE_HPUX
  void copen (iunit, filename, istat, Lfilename)
  int *iunit, *istat, Lfilename;
  char *filename;
#endif
#if CALL_LIKE_STARDENT
  void COPEN (int *iunit, struct Str_Desc *filename, int *istat)
#endif
#if defined (VMS)
  void COPEN (int *iunit, struct dsc$descriptor_s *filename, int *istat)
#endif
#if CALL_LIKE_SUN
  void copen_ (int *iunit, char *filename, int *istat, int Lfilename)
#endif
#if CALL_LIKE_MVS
  void __stdcall COPEN (int *iunit, char *filename, int Lfilename, int *istat)
#endif
{
  size_t Length;
  int i, jstat;

  jstat = *istat;
  if (! initialised) {
    /* note that array element 0 is unused -- using it produced
       complaints from mtzlib about a zero stream */
    for (i = 1; i < MAXFILES; i++) {
      file_stream[i]         = NULL;
      file_name[i][0]        = '\0';
      file_bytes_per_item[i] = item_sizes[DEFMODE];  /* default item size */
      file_is_scratch[i]     = 0;
      file_last_op[i]        = IRRELEVANT_OP;
      file_mode[i] = DEFMODE;
    }
    initialised = 1;
  }
  for (i = 1; i < MAXFILES; i++) /* Find next available stream */
    if (file_stream[i] == NULL) break;
  if (i == MAXFILES) {
    *iunit = -1;                /* return no more units flag */
    return;
  } else {
    *iunit = i;}                 /* will return the stream number */
#if CALL_LIKE_STARDENT
  Length = flength (filename->Str_pointer, filename->Str_length);
  if (Length > MAXFLEN) Length = MAXFLEN - 1;
  (void) strncpy (file_name[i], filename->Str_pointer, Length);
#else
#  if defined (VMS)
  Length = flength (filename->dsc$a_pointer, filename->dsc$w_length);
  if (Length > MAXFLEN) Length = MAXFLEN - 1;
  (void) strncpy (file_name[i], filename->dsc$a_pointer, Length);
#  else
  Length = flength (filename, Lfilename);
  if (Length > MAXFLEN) Length = MAXFLEN - 1;
  (void) strncpy (file_name[i], filename, Length);
#  endif
#endif
  file_name[i][Length] = '\0';
  file_last_op[i] = IRRELEVANT_OP;
  file_bytes_per_item[i] = item_sizes[DEFMODE]; /* default item size */
  file_mode[i] = DEFMODE;
  file_is_scratch[i] = (jstat == 2);
/* There are complications involved with the \idx{VMS} code:                */
/* \begin{itemize}                                                          */
/* \item We want to be able to read files written by the old assembler      */
/*   library\index{VAX!assembler library} which wrote fixed-length records and */
/*   you can't do arbitrary seeks in such a file format.  Fortunately, in   */
/*   VAX C the file can be opened as StreamLF\index{StreamLF files} (as     */
/*   we want for C i/o) regardless of what the file header says.  Thanks    */
/*   to Peter Keller for suggesting this.  (This should also work for       */
/*   files ftp'd from a Unix box.);                                         */
/* \item We can't [[unlink]] the open file from the directory a             */
/*   posteriori.  Instead it's opened with the [[tmd]] RMS option as the    */
/*   assembler routines did;                                                */
/* \item Following the suggestion in the VMS 6.0 release notes about        */
/*   faster stream i/o, we use open option [["mbc=16"]] to increase the     */
/*   block size.  (This is supposed to be the default value with            */
/*   \idx{DEC C}.)                                                          */
/* \item However, the VAX C syntax for this ([[fopen]] with varargs)        */
/*   might not be supported by non-DEC compilers (although {\tt             */
/*   gcc}\index{GCC} does seem to have it).                                 */
/* \end{itemize}                                                            */
/*                                                                          */
/* <diskio routines>=                                                       */
#ifdef VMS
  if (file_is_scratch[i])
    file_stream[i] = fopen (file_name[i], file_attribute[jstat - 1],
                            "mbc=16", /* bigger blocksize */
                            "fop=tmd"); /* temporary, delete on close */
  else
    file_stream[i] = fopen (file_name[i], file_attribute[jstat - 1],
                            "mbc=16", /* bigger blocksize */
                            "ctx=stm", "mrs=0", "rat=cr", "rfm=stmlf");
  if (file_stream[i] == NULL)
    file_fatal ("(Q)QOPEN: can't open ", file_name[i]);
#else
# ifdef _MVS
  if (file_is_scratch[i]) {
    if ((file_stream[i] = tmpfile()) == NULL) 
      file_fatal ("(Q)QOPEN: can't open ", file_name[i]);}
  else {
    file_stream[i] = fopen (file_name[i], file_attribute[jstat - 1]);
  }
# else
  file_stream[i] = fopen (file_name[i], file_attribute[jstat - 1]);
  if (file_stream[i] == NULL)
    { 
      file_fatal ("(Q)QOPEN: can't open ", file_name[i]);
    }
  if (file_is_scratch[i] && unlink (file_name[i])!=0)
    {
      file_fatal ("(Q)QOPEN: error unlinking ", file_name[i]);
    }
# endif
#endif
  if (file_stream[i] == NULL) {
    *iunit = -2;                /* return open failure flag */
    return; }
  Iconvert[i] = Fconvert[i] = 0;
/* It seems the \idx{OpenVMS} (don't know which version) can easily lose its */
/* place in files.  Try flushing the output buffer before messing around    */
/* with [[fseek]].  (Thanks to Richard Bryan.)  N.B.: assumes [[*iunit]]!   */
/*                                                                          */
/* <OpenVMS seek fudge>=                                                    */
#if defined (__alpha) && defined (vms)
(void) fflush (file_stream[*iunit]);
#endif
  if (fseek (file_stream[*iunit], 0L, SEEK_SET) != 0)
    file_fatal("(Q)QOPEN: fseek failed on", file_name[i]);
  *iunit = i;
}
/* \subsection{{\tt subroutine qrarch (\meta{iunit},                        */
/*     \meta{ipos}, \meta{ireslt})}}                                        */
/*                                                                          */
/* For binary files with a well-determined structure in terms of            */
/* [[float]]s and [[int]]s we may want to set up the connected stream to    */
/* do transparent reading of files written on a machine with a different    */
/* architecture.  This is currently the case for map\index{map files} and   */
/* \idx{MTZ files} and this routine is called from \idx{mtzlib} and         */
/* \idx{maplib}.                                                            */
/*                                                                          */
/* [[qrarch]] reads the \idx{machine stamp} at {\em word\/} \meta{ipos}     */
/* for the diskio file on stream \meta{iunit} and sets up the appropriate   */
/* bit-twiddling for subsequent [[qread]]s on that stream.  The             */
/* information read from the file is returned in \meta{ireslt} in the       */
/* form $\mbox{[[fileFT]]}+16\mbox{[[fileIT]]}$.  If the stamp is zero      */
/* (as it would be for files written with a previous version of the         */
/* library) we assume the file is in native format and needs no             */
/* conversion in [[qread]]; in this case \meta{ireslt} will be zero and     */
/* the caller can issue a warning.  [[Iconvert]] and [[Fconvert]] are       */
/* used by [[qread]] to determine the type of conversion (if any) to be     */
/* applied to integers and reals.                                           */
/*                                                                          */
/* Fudge:\index{fudge} Ian Tickle reports old VAX files which have a machine */
/* stamp which is byte-flipped from the correct VAX value, although it should */
/* always have been zero as far as I can see.  To accommodate this, set the */
/* logical \idx{NATIVEMTZ} and the machine stamp won't be read for any      */
/* input files for which [[qrarch]] is called.                              */
/*                                                                          */
/* Extra feature: logical/environment variable [[CONVERT_FROM]] may be set to one */
/* of [[BEIEEE]], [[LEIEEE]], [[VAX]] or [[CONVEXNATIVE]] to avoid reading the */
/* machine stamp and assume the file is from the stipulated archictecture   */
/* for all input MTZ and map files for which [[qrarch]] is called.          */
/*                                                                          */
/* N.B.: leaves the stream positioned just after the machine stamp.         */
/*                                                                          */
/* <diskio routines>=                                                       */
#if CALL_LIKE_HPUX
  void qrarch (iunit, ipos, ireslt)
  int *iunit, *ipos, *ireslt;
#endif
#if CALL_LIKE_STARDENT
  void QRARCH (int *iunit, int *ipos, int *ireslt)
#endif
#if defined (VMS)
  void QRARCH (int *iunit, int *ipos, int *ireslt)
#endif
#if CALL_LIKE_SUN
  void qrarch_ (int *iunit, int *ipos, int *ireslt)
#endif
#if CALL_LIKE_MVS
  void __stdcall QRARCH (int *iunit, int *ipos, int *ireslt)
#endif
{
  uint16 fileFT, fileIT;        /* float and integer machine types of file */
  unsigned char mtstring[4];    /* machine stamp */
  char *native = getenv ("NATIVEMTZ");
  char *foreign = getenv ("CONVERT_FROM");

  if (native != NULL) { *ireslt = 0; return; }
  if (foreign != NULL) {
    if (strcmp (foreign, "BEIEEE") == 0) {
      mtstring[0] = DFNTF_BEIEEE | (DFNTF_BEIEEE << 4);
      mtstring[1] = 1 | (DFNTI_MBO << 4); }
    else if (strcmp (foreign, "LEIEEE") == 0) {
      mtstring[0] = DFNTF_LEIEEE | (DFNTF_LEIEEE << 4);
      mtstring[1] = 1 | (DFNTI_IBO << 4); }
    else if (strcmp (foreign, "VAX") == 0) {
      mtstring[0] = DFNTF_VAX | (DFNTF_VAX << 4);
      mtstring[1] = 1 | (DFNTI_IBO << 4); }
    else if (strcmp (foreign, "CONVEXNATIVE") == 0) {
      mtstring[0] = DFNTF_CONVEXNATIVE | (DFNTF_CONVEXNATIVE << 4);
      mtstring[1] = 1 | (DFNTI_MBO << 4); }  
  } else {
/* It seems the \idx{OpenVMS} (don't know which version) can easily lose its */
/* place in files.  Try flushing the output buffer before messing around    */
/* with [[fseek]].  (Thanks to Richard Bryan.)  N.B.: assumes [[*iunit]]!   */
/*                                                                          */
/* <OpenVMS seek fudge>=                                                    */
#if defined (__alpha) && defined (vms)
(void) fflush (file_stream[*iunit]);
#endif
    if ((fseek (file_stream[*iunit], (long int) ((*ipos)*item_sizes[2]),
                SEEK_SET) != 0))
      file_fatal ("QRARCH: seek failed on ", file_name[*iunit]);
    file_last_op[*iunit] = READ_OP;
    if (fread (mtstring, (size_t) sizeof(char), (size_t) 4,
               file_stream[*iunit]) != 4)
      file_fatal ("QRARCH: can't read machine stamp in ", file_name[*iunit]);
  }
  fileIT = (mtstring[1]>>4) & 0x0f;
  fileFT = (mtstring[0]>>4) & 0x0f;
  /* Record the need for conversion and what the file type is: */
  printf("Native Architecture:  %d\n",nativeIT); 
  if (fileFT != 0 && fileFT != nativeFT)
    Fconvert[*iunit] = fileFT;  /* else assume native */
  if (fileIT != 0 && fileIT != nativeIT)
    Iconvert[*iunit] = fileIT;  /* else assume native */
  *ireslt = fileFT + (16*fileIT);
}
/* \subsection{{\tt subroutine qwarch(\meta{iunit}, \meta{ipos})}}          */
/* This is the complement of [[qrarch]], writing the native machine         */
/* architecture information (`\idx{machine stamp}') to diskio stream        */
/* \meta{iunit} at {\em word\/} \meta{ipos}.  Currently called              */
/* from \idx{mtzlib} and \idx{maplib}.                                      */
/*                                                                          */
/* The machine stamp in [[mtstring]] is four nibbles in order, indicating   */
/* complex and real format (must both be the same), integer format and      */
/* character format (currently irrelevant).  The last two bytes of          */
/* [[mtstring]] are currently unused and always zero.                       */
/*                                                                          */
/* N.B.: leaves the stream positioned just after the machine stamp.         */
/*                                                                          */
/* <diskio routines>=                                                       */
#if CALL_LIKE_HPUX
  void qwarch (iunit, ipos)
  int *iunit, *ipos;
#endif
#if CALL_LIKE_STARDENT
  void QWARCH (int *iunit, int *ipos)
#endif
#if defined (VMS)
  void QWARCH (int *iunit, int *ipos)
#endif
#if CALL_LIKE_SUN
  void qwarch_ (int *iunit, int *ipos)
#endif
#if CALL_LIKE_MVS
  void __stdcall QWARCH (int *iunit, int *ipos)
#endif
{
  unsigned char mtstring[4];    /* machine stamp */
/* It seems the \idx{OpenVMS} (don't know which version) can easily lose its */
/* place in files.  Try flushing the output buffer before messing around    */
/* with [[fseek]].  (Thanks to Richard Bryan.)  N.B.: assumes [[*iunit]]!   */
/*                                                                          */
/* <OpenVMS seek fudge>=                                                    */
#if defined (__alpha) && defined (vms)
(void) fflush (file_stream[*iunit]);
#endif
  if (fseek (file_stream[*iunit], (long int) ((*ipos)*item_sizes[2]),
             SEEK_SET) != 0)
    file_fatal ("QWARCH: seek failed on ", file_name[*iunit]);
  /* nibbles packed by masking and ORing: */
  mtstring[0] = nativeFT | (nativeFT << 4);
  mtstring[1] = 1 | (nativeIT << 4);
  mtstring[2] = mtstring[3] = 0;
  file_last_op[*iunit] = WRITE_OP;
  if (fwrite (mtstring, (size_t) sizeof(char), (size_t) 4,
             file_stream[*iunit]) != 4)
    file_fatal ("QWARCH: can't write machine stamp to ", file_name[*iunit]);
}
/* \subsection{{\tt subroutine qclose (\meta{iunit})}}                      */
/* Closes the file open on \idx{diskio} stream \meta{iunit}.                */
/*                                                                          */
/* <diskio routines>=                                                       */
#if CALL_LIKE_HPUX
  void qclose (iunit)
  int *iunit;
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void QCLOSE (int *iunit)
#endif
#if CALL_LIKE_SUN
  void qclose_ (int *iunit)
#endif
#if CALL_LIKE_MVS
  void __stdcall QCLOSE (int *iunit)
#endif
{
  if (! initialised) 
    fatal ("QCLOSE: qopen/qqopen not yet called");
  if (file_stream[*iunit] != NULL) {
    if (fclose (file_stream[*iunit]) == EOF) 
      file_fatal ("QCLOSE: failed on ", file_name[*iunit]);
    file_stream[*iunit] = NULL;
  }
  file_name[*iunit][0] = '\0';
}
/* \subsection{{\tt subroutine qmode (\meta{iunit}, \meta{mode},            */
/*     \meta{size})}}                                                       */
/* Changes the \idx{diskio} \idx{access mode} for stream \meta{iunit} to    */
/* \meta{mode}.  The resulting size in bytes of items for transfer is       */
/* returned as \meta{size}.                                                 */
/*                                                                          */
/* <diskio routines>=                                                       */
#if CALL_LIKE_HPUX
  void qmode (iunit, mode, size)
  int *iunit, *mode, *size;
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void QMODE (int *iunit, int *mode, int *size)
#endif
#if CALL_LIKE_SUN
  void qmode_ (int *iunit, int *mode, int *size)
#endif
#if CALL_LIKE_MVS
  void __stdcall QMODE (int *iunit, int *mode, int *size)
#endif
{
  if (! initialised) 
    fatal ("QMODE: qopen/qqopen not yet called");

  if (*mode >= 0 && *mode <= 6 && *mode != 5)
    file_bytes_per_item[*iunit] = item_sizes[*mode];
  else
    fatal ("QMODE: bad mode");
  *size = file_bytes_per_item[*iunit];       /* return number of bytes/item */
  file_mode[*iunit] = *mode;
}
/* \subsection{{\tt subroutine qread(\meta{iunit}, \meta{buffer},           */
/*     \meta{nitems}, \meta{result})}}                                      */
/*                                                                          */
/* Reads \meta{nitems} in the current mode (set by [[qmode]]) from diskio stream */
/* \meta{iunit} previously opened by [[qopen]](/[[copen]]) and returns      */
/* \meta{result} which is [[0]] on success or [[-1]] at EOF\@.              */
/* It aborts on an i/o error.                                               */
/* Numbers written in a foreign format will be translated if necessary if   */
/* the stream is connected to an MTZ or map file.                           */
/*                                                                          */
/* <diskio routines>=                                                       */
#if CALL_LIKE_HPUX
  void qread (iunit, buffer, nitems, result)
  int *iunit, *nitems, *result;
  uint8 * buffer;
#endif
#if defined (VMS) || defined (ardent) || defined (titan) || defined (stardent)
  void QREAD (int *iunit, uint8 * buffer, int *nitems, int *result)
#endif
#if CALL_LIKE_SUN
  void qread_ (int *iunit, uint8 * buffer, int *nitems, int *result)
#endif
#if CALL_LIKE_MVS
  void __stdcall QREAD (int *iunit, uint8 * buffer, int *nitems, int *result)
#endif
{
  int i, n;

  if (! initialised) 
    fatal ("QREAD: qopen/qqopen not yet called");
  if (file_last_op[*iunit] == WRITE_OP) {
/* It seems the \idx{OpenVMS} (don't know which version) can easily lose its */
/* place in files.  Try flushing the output buffer before messing around    */
/* with [[fseek]].  (Thanks to Richard Bryan.)  N.B.: assumes [[*iunit]]!   */
/*                                                                          */
/* <OpenVMS seek fudge>=                                                    */
#if defined (__alpha) && defined (vms)
(void) fflush (file_stream[*iunit]);
#endif
    if (fseek (file_stream[*iunit], 0L, SEEK_CUR) != 0) {
      /**result = -1;*/
      file_fatal ("QREAD: seek error on file ", file_name[*iunit]);
      return; } }
  file_last_op[*iunit] = READ_OP;
  errno = 0;
  i = (int) fread (buffer, (size_t) file_bytes_per_item[*iunit], 
                (size_t) *nitems, file_stream[*iunit]);
  if (i != *nitems) {
    if (feof (file_stream[*iunit])) *result = -1;
    else {
      /**result = i;*/
      file_fatal ("QREAD: i/o error on ", file_name[*iunit]);
    }
    return;
  }
  *result = 0;
  n = *nitems;
  /* <convert numbers if necessary>=                                          */
    switch (file_mode[*iunit]) {
    case BYTE:
      break;
    case INT16:
      if (Iconvert[*iunit])
        /* \subsubsection{Converting integers}                                      */
        /* The only possibility at present is byte-swapping (since we only deal     */
        /* with \idx{twos complement} integers).  The test in the following         */
        /* [[if]] could be short-circuited on this assumption.                      */
        /*                                                                          */
        /* <convert [[n]] short integers in [[buffer]]>=                            */
        {
        if ((Iconvert[*iunit]==DFNTI_MBO && nativeIT==DFNTI_IBO) ||
            (Iconvert[*iunit]==DFNTI_IBO && nativeIT==DFNTI_MBO)) {
          char j;
          for (i=0; i < n*2; i+=2) {
            j = buffer[i];
            buffer[i] = buffer[i+1];
            buffer[i+1] = j; } }
        else
          fatal("QREAD: bad file integer type in conversion");
        }
      break;
    case INT32:
      if (Iconvert[*iunit])
        /* <convert [[n]] long integers in [[buffer]]>=                             */
        {
        if ((Iconvert[*iunit]==DFNTI_MBO && nativeIT==DFNTI_IBO) ||
            (Iconvert[*iunit]==DFNTI_IBO && nativeIT==DFNTI_MBO))
          /* <byte-swap [[n]] full words in [[buffer]]>=                              */
          {
            char j;
            for (i=0; i < n*4; i+=4) {
              j = buffer[i];
              buffer[i] = buffer[i+3];
              buffer[i+3] = j;
              j = buffer[i+1];
              buffer[i+1] = buffer[i+2];
              buffer[i+2] =j; }
          }
        else
          fatal("QREAD: bad file integer type in conversion");
        }
      break;
    case FLOAT32:
      if (Fconvert[*iunit])
        /* \subsubsection{Converting reals}                                         */
        /* There are more possibilities than for integers\dots{}  Remember we use   */
        /* two stages and a canonical form.                                         */
        /*                                                                          */
        /* <convert [[n]] reals in [[buffer]]>=                                     */
        {
        switch (Fconvert[*iunit]) {     /* get to BE IEEE */
           case DFNTF_VAX :
             vaxF2ieeeF(buffer, n);
             break;   
           case DFNTF_CONVEXNATIVE :
             convexF2ieeeF(buffer, n);
             break;
           case DFNTF_BEIEEE :
             break;
           case DFNTF_LEIEEE :
             /* <byte-swap [[n]] full words in [[buffer]]>=                              */
             {
               char j;
               for (i=0; i < n*4; i+=4) {
                 j = buffer[i];
                 buffer[i] = buffer[i+3];
                 buffer[i+3] = j;
                 j = buffer[i+1];
                 buffer[i+1] = buffer[i+2];
                 buffer[i+2] =j; }
             }
             break;
           default :
             fatal("QREAD: bad file real type in conversion");
           }
        /* We've now got a guaranteed big-endian \ac{ieee} [[buffer]].  Turn it     */
        /* into the native form if necessary.  (This could be done with             */
        /* [[#ifdef]] since [[nativeFT]] is constant, but presumably the compiler   */
        /* can spot that.)                                                          */
        /*                                                                          */
        /* <convert [[n]] reals in [[buffer]]>=                                     */
        switch (nativeFT) {
          case DFNTF_BEIEEE :
            break;                      /* done enough */
          case DFNTF_LEIEEE :
            /* <byte-swap [[n]] full words in [[buffer]]>=                              */
            {
              char j;
              for (i=0; i < n*4; i+=4) {
                j = buffer[i];
                buffer[i] = buffer[i+3];
                buffer[i+3] = j;
                j = buffer[i+1];
                buffer[i+1] = buffer[i+2];
                buffer[i+2] =j; }
            }
            break;
          case DFNTF_CONVEXNATIVE :
            ieeeF2convexF(buffer, n);
            break;
          case DFNTF_VAX :
            ieeeF2vaxF(buffer, n);
            break;
          default :
            fatal("QREAD: bad native real type in conversion");
          }
        }
      break;
    case COMP32:
      if (Fconvert[*iunit]) {
        n = 2*n;                  /* pairs of ints */
        /* \subsubsection{Converting integers}                                      */
        /* The only possibility at present is byte-swapping (since we only deal     */
        /* with \idx{twos complement} integers).  The test in the following         */
        /* [[if]] could be short-circuited on this assumption.                      */
        /*                                                                          */
        /* <convert [[n]] short integers in [[buffer]]>=                            */
        {
        if ((Iconvert[*iunit]==DFNTI_MBO && nativeIT==DFNTI_IBO) ||
            (Iconvert[*iunit]==DFNTI_IBO && nativeIT==DFNTI_MBO)) {
          char j;
          for (i=0; i < n*2; i+=2) {
            j = buffer[i];
            buffer[i] = buffer[i+1];
            buffer[i+1] = j; } }
        else
          fatal("QREAD: bad file integer type in conversion");
        }
      }
      break;
    case COMP64:
      if (Fconvert[*iunit]) {
        n = 2*n;                  /* pairs of reals */
        /* \subsubsection{Converting reals}                                         */
        /* There are more possibilities than for integers\dots{}  Remember we use   */
        /* two stages and a canonical form.                                         */
        /*                                                                          */
        /* <convert [[n]] reals in [[buffer]]>=                                     */
        {
        switch (Fconvert[*iunit]) {     /* get to BE IEEE */
           case DFNTF_VAX :
             vaxF2ieeeF(buffer, n);
             break;   
           case DFNTF_CONVEXNATIVE :
             convexF2ieeeF(buffer, n);
             break;
           case DFNTF_BEIEEE :
             break;
           case DFNTF_LEIEEE :
             /* <byte-swap [[n]] full words in [[buffer]]>=                              */
             {
               char j;
               for (i=0; i < n*4; i+=4) {
                 j = buffer[i];
                 buffer[i] = buffer[i+3];
                 buffer[i+3] = j;
                 j = buffer[i+1];
                 buffer[i+1] = buffer[i+2];
                 buffer[i+2] =j; }
             }
             break;
           default :
             fatal("QREAD: bad file real type in conversion");
           }
        /* We've now got a guaranteed big-endian \ac{ieee} [[buffer]].  Turn it     */
        /* into the native form if necessary.  (This could be done with             */
        /* [[#ifdef]] since [[nativeFT]] is constant, but presumably the compiler   */
        /* can spot that.)                                                          */
        /*                                                                          */
        /* <convert [[n]] reals in [[buffer]]>=                                     */
        switch (nativeFT) {
          case DFNTF_BEIEEE :
            break;                      /* done enough */
          case DFNTF_LEIEEE :
            /* <byte-swap [[n]] full words in [[buffer]]>=                              */
            {
              char j;
              for (i=0; i < n*4; i+=4) {
                j = buffer[i];
                buffer[i] = buffer[i+3];
                buffer[i+3] = j;
                j = buffer[i+1];
                buffer[i+1] = buffer[i+2];
                buffer[i+2] =j; }
            }
            break;
          case DFNTF_CONVEXNATIVE :
            ieeeF2convexF(buffer, n);
            break;
          case DFNTF_VAX :
            ieeeF2vaxF(buffer, n);
            break;
          default :
            fatal("QREAD: bad native real type in conversion");
          }
        }
      }
      break;
    default:
      fatal ("QREAD: Bad mode");
    }
}
/* \subsection{{\tt subroutine qreadc(\meta{iunit}, \meta{buffer},          */
/*     \meta{result})}}                                                     */
/*                                                                          */
/* Fills [[CHARACTER]] buffer in byte mode from diskio stream               */
/* \meta{iunit} previously opened by [[qopen]](/[[copen]]) and returns      */
/* \meta{result} which is the number of items read or [[0]] on failure.     */
/* Call it with a character substring if necessary to control the number    */
/* of bytes read.                                                           */
/*                                                                          */
/* <diskio routines>=                                                       */
#if CALL_LIKE_HPUX
  void qreadc (iunit, buffer, result, Lbuffer)
  int *iunit, *result, Lbuffer;
  char * buffer;
#endif
#ifdef VMS
  void QREADC (int *iunit, struct dsc$descriptor_s *buffer, int *result)
#endif
#if CALL_LIKE_STARDENT
  void QREADC (int *iunit, struct Str_Desc *buffer, int *result)
#endif
#if CALL_LIKE_SUN
  void qreadc_ (int *iunit, char * buffer, int *result, int Lbuffer)
#endif
#if CALL_LIKE_MVS
  void __stdcall QREADC (int *iunit, char * buffer, int Lbuffer, int *result)
#endif
{
  int i, n;

  if (! initialised) 
    fatal ("QREAD: qopen/qqopen not yet called");
  if (file_last_op[*iunit] == WRITE_OP) {
/* It seems the \idx{OpenVMS} (don't know which version) can easily lose its */
/* place in files.  Try flushing the output buffer before messing around    */
/* with [[fseek]].  (Thanks to Richard Bryan.)  N.B.: assumes [[*iunit]]!   */
/*                                                                          */
/* <OpenVMS seek fudge>=                                                    */
#if defined (__alpha) && defined (vms)
(void) fflush (file_stream[*iunit]);
#endif
    if (fseek (file_stream[*iunit], 0L, SEEK_CUR) != 0) {
      *result = -1;
      return; } }
  file_last_op[*iunit] = READ_OP;
#if defined (VMS)
  n = buffer->dsc$w_length;
  i = (int) fread (buffer->dsc$a_pointer, (size_t) item_sizes[BYTE], 
                (size_t) n, file_stream[*iunit]);
#else
#  if CALL_LIKE_STARDENT
  n = buffer->Str_length;
  i = fread (buffer->Str_pointer, (size_t) item_sizes[BYTE], 
                (size_t) n, file_stream[*iunit]);
#  else                         /* normal */
  n = Lbuffer;
  i = (int) fread (buffer, (size_t) item_sizes[BYTE], 
                (size_t) n, file_stream[*iunit]);
#  endif
#endif
  if (i != n) {
    if (feof (file_stream[*iunit])) *result = -1;
    else *result = i;
    return;
  }
  *result = 0;
}
/* \subsection{{\tt subroutine qwrite (\meta{iunit}, \meta{buffer},         */
/*     \meta{nitems})}}                                                     */
/* This write \meta{nitems} items from \meta{buffer} to [[qopen]]ed         */
/* stream \meta{iunit} using the current mode.                              */
/*                                                                          */
/* <diskio routines>=                                                       */
#if CALL_LIKE_HPUX
  void qwrite (iunit, buffer, nitems)
  int *iunit, *nitems;
  uint8 * buffer;
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void QWRITE (int *iunit, uint8 * buffer, int *nitems)
#endif
#if CALL_LIKE_SUN
  void qwrite_ (int *iunit, uint8 * buffer, int *nitems)
#endif
#if CALL_LIKE_MVS
  void __stdcall QWRITE (int *iunit, uint8 * buffer, int *nitems)
#endif
{
  int i;

  if (! initialised) 
    fatal ("QWRITE: qopen/qqopen not yet called");
  if (file_last_op[*iunit] == READ_OP) {
/* It seems the \idx{OpenVMS} (don't know which version) can easily lose its */
/* place in files.  Try flushing the output buffer before messing around    */
/* with [[fseek]].  (Thanks to Richard Bryan.)  N.B.: assumes [[*iunit]]!   */
/*                                                                          */
/* <OpenVMS seek fudge>=                                                    */
#if defined (__alpha) && defined (vms)
(void) fflush (file_stream[*iunit]);
#endif
    if (fseek (file_stream[*iunit], 0L, SEEK_CUR) != 0)
      file_fatal ("QWRITE: seek failed on ", file_name[*iunit]); }
  file_last_op[*iunit] = WRITE_OP;
  i = (int) fwrite (buffer, (size_t) file_bytes_per_item[*iunit],
                    (size_t) *nitems, file_stream[*iunit]);
/* We don't (necessarily?)\ get a useful system error message from          */
/* [[fatal]] if the write fails (e.g.\ in \idx{Irix}), hance the hint       */
/* about disc space.                                                        */
/*                                                                          */
/* <diskio routines>=                                                       */
  if (i != *nitems)
    file_fatal ("QWRITE: i/o error (may be out of disc space): ",
           file_name[*iunit]);
}
/* \subsection{{\tt subroutine qwritc (\meta{iunit}, \meta{buffer})}}       */
/*                                                                          */
/* Writes [[CHARACTER*(*)]] \meta{buffer} to [[qopen]]ed                    */
/* stream \meta{iunit} in byte mode.                                        */
/*                                                                          */
/* <diskio routines>=                                                       */
#if CALL_LIKE_HPUX
  void qwritc (iunit, buffer, Lbuffer)
  int *iunit, Lbuffer;
  char * buffer;
#endif
#if defined (VMS)
  void QWRITC (int *iunit, struct dsc$descriptor_s *buffer)
#endif
#if defined CALL_LIKE_STARDENT
  void QWRITC (int *iunit, struct Str_Desc *buffer)
#endif
#if CALL_LIKE_SUN
  void qwritc_ (int *iunit, char * buffer, int Lbuffer)
#endif
#if CALL_LIKE_MVS
  void __stdcall QWRITC (int *iunit, char * buffer, int Lbuffer)
#endif
{
  int i, n;

  if (! initialised) 
    fatal ("QWRITC: qopen/qqopen not yet called");
  if (file_last_op[*iunit] == READ_OP) {
/* It seems the \idx{OpenVMS} (don't know which version) can easily lose its */
/* place in files.  Try flushing the output buffer before messing around    */
/* with [[fseek]].  (Thanks to Richard Bryan.)  N.B.: assumes [[*iunit]]!   */
/*                                                                          */
/* <OpenVMS seek fudge>=                                                    */
#if defined (__alpha) && defined (vms)
(void) fflush (file_stream[*iunit]);
#endif
    if (fseek (file_stream[*iunit], 0L, SEEK_CUR) != 0)
      file_fatal ("QWRITC: seek failed on", file_name[*iunit]); }
  file_last_op[*iunit] = WRITE_OP;
#if defined (VMS)
  n = buffer->dsc$w_length;
  i = (int) fwrite (buffer->dsc$a_pointer, (size_t) item_sizes[BYTE],
                    (size_t) n, file_stream[*iunit]);
#else
#  if CALL_LIKE_STARDENT
  n = buffer->Str_length;
  i = (int) fwrite (buffer->Str_pointer, (size_t) item_sizes[BYTE],
                    (size_t) n, file_stream[*iunit]);
#  else                         /* normal */
  n = Lbuffer;
#  endif
#endif
  i = (int) fwrite (buffer, (size_t) item_sizes[BYTE],
                    (size_t) n, file_stream[*iunit]);
  if (i != n) file_fatal ("QWRITC: i/o error (may be out of disc space): ",
                           file_name[*iunit]);
}
/* \subsection{{\tt subroutine qseek (\meta{iunit}, \meta{irec},            */
/*     \meta{iel}, \meta{lrecl})}}                                          */
/* Seeks to element \meta{iel} in record \meta{irec} in diskio stream       */
/* \meta{iunit} whose record length is \meta{lrecl}.                        */
/*                                                                          */
/* <diskio routines>=                                                       */
#if CALL_LIKE_HPUX
  void qseek (iunit, irec, iel, lrecl)
  int *iunit, *irec, *iel, *lrecl;
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void QSEEK (int *iunit, int *irec, int *iel, int *lrecl)
#endif
#if CALL_LIKE_SUN
  void qseek_ (int *iunit, int *irec, int *iel, int *lrecl)
#endif
#if CALL_LIKE_MVS
  void __stdcall QSEEK (int *iunit, int *irec, int *iel, int *lrecl)
#endif
{
  long int position;

  if (! initialised) 
    fatal ("QSEEK: qopen/qqopen not yet called");
  position = (long) ((*lrecl)*(*irec - 1) + (*iel - 1));
  position *= (long) file_bytes_per_item[*iunit];
  file_last_op[*iunit] = IRRELEVANT_OP;
/* It seems the \idx{OpenVMS} (don't know which version) can easily lose its */
/* place in files.  Try flushing the output buffer before messing around    */
/* with [[fseek]].  (Thanks to Richard Bryan.)  N.B.: assumes [[*iunit]]!   */
/*                                                                          */
/* <OpenVMS seek fudge>=                                                    */
#if defined (__alpha) && defined (vms)
(void) fflush (file_stream[*iunit]);
#endif
  if (fseek (file_stream[*iunit],position,SEEK_SET) != 0)
    file_fatal ("QSEEK failed -- maybe corrupt file: ",file_name[*iunit]);
}
/* \subsection{{\tt subroutine qback (\meta{iunit}, \meta{lrecl})}}         */
/* Backspaces one record, of length \meta{lrecl} on diskio stream \meta{iunit}. */
/*                                                                          */
/* <diskio routines>=                                                       */
#if CALL_LIKE_HPUX
  void qback (iunit, lrecl)
  int *iunit, *lrecl;
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void QBACK (int *iunit, int *lrecl)
#endif
#if CALL_LIKE_SUN
  void qback_ (int *iunit, int *lrecl)
#endif
#if CALL_LIKE_MVS
  void __stdcall QBACK (int *iunit, int *lrecl)
#endif
{
  long int position;

  if (! initialised) 
    fatal ("QBACK: qopen/qqopen not yet called");
  position = ftell (file_stream[*iunit]) - (*lrecl)*file_bytes_per_item[*iunit];
  file_last_op[*iunit] = IRRELEVANT_OP;
/* It seems the \idx{OpenVMS} (don't know which version) can easily lose its */
/* place in files.  Try flushing the output buffer before messing around    */
/* with [[fseek]].  (Thanks to Richard Bryan.)  N.B.: assumes [[*iunit]]!   */
/*                                                                          */
/* <OpenVMS seek fudge>=                                                    */
#if defined (__alpha) && defined (vms)
(void) fflush (file_stream[*iunit]);
#endif
  if (fseek (file_stream[*iunit], position, SEEK_SET) != 0)
    file_fatal ("QBACK failed on ", file_name[*iunit]);
}
/* \subsection{{\tt subroutine qskip (\meta{iunit}, \meta{lrecl})}}         */
/* Skip forward 1 record of length \meta{lrecl} on diskio stream \meta{iunit}. */
/*                                                                          */
/* <diskio routines>=                                                       */
#if CALL_LIKE_HPUX
  void qskip (iunit, lrecl)
  int *iunit, *lrecl;
#endif
#if defined (VMS) || defined (ardent) || defined (titan) || defined (stardent)
  void QSKIP (int *iunit, int *lrecl)
#endif
#if CALL_LIKE_SUN
  void qskip_ (int *iunit, int *lrecl)
#endif
#if CALL_LIKE_MVS
  void __stdcall QSKIP (int *iunit, int *lrecl)
#endif
{
  long int position;

  if (! initialised) 
    fatal ("QSKIP: qopen/qqopen not yet called");
  position = ftell (file_stream[*iunit]) +
    (*lrecl)*file_bytes_per_item[*iunit];
  file_last_op[*iunit] = IRRELEVANT_OP;
/* It seems the \idx{OpenVMS} (don't know which version) can easily lose its */
/* place in files.  Try flushing the output buffer before messing around    */
/* with [[fseek]].  (Thanks to Richard Bryan.)  N.B.: assumes [[*iunit]]!   */
/*                                                                          */
/* <OpenVMS seek fudge>=                                                    */
#if defined (__alpha) && defined (vms)
(void) fflush (file_stream[*iunit]);
#endif
  if (fseek (file_stream[*iunit],position,SEEK_SET) != 0)
    file_fatal ("QSKIP failed on ", file_name[*iunit]);
}
/* \subsection{{\tt subroutine cqinq (\meta{istrm}, \meta{filnam},          */
/*     \meta{length})}}                                                     */
/* Returns the name \meta{filnam} and \meta{length} of the file (if any)    */
/* open on diskio stream \meta{istrm}.                                      */
/*                                                                          */
/* <diskio routines>=                                                       */
#if CALL_LIKE_HPUX
  void cqinq (istrm, filnam, length, len_filnam)
  int *istrm, *length, len_filnam;
  char *filnam;
#endif
#if CALL_LIKE_STARDENT
  void CQINQ (int *istrm, struct Str_Desc *filnam, int *length)
#endif
#if defined (VMS)
  void CQINQ (int *istrm, struct dsc$descriptor_s *filnam, int *length)
#endif
#if CALL_LIKE_SUN
  void cqinq_ (int *istrm, char *filnam, int *length, int len_filnam)
#endif
#if CALL_LIKE_MVS
  void __stdcall CQINQ (int *istrm, char *filnam, int len_filnam, int *length)
#endif
{
  char real_name[MAXFLEN];
  int *iunit, i;
  long position;
  size_t Length;

  if (! initialised) 
    fatal ("QQINQ: qopen/qqopen not yet called");
  *length = -1;                                    /* default return value */
  iunit = istrm;
  if (file_stream[*iunit] == NULL) { 
    /* no unit open -- try file name */
#if CALL_LIKE_STARDENT
    Length = flength (filnam->Str_pointer, filnam->Str_length);
    if (Length > (size_t) MAXFLEN) Length = (size_t) MAXFLEN - 1;
    (void) strncpy (real_name, filnam->Str_pointer, Length);
#else
#  if defined (VMS)
     Length = flength (filnam->dsc$a_pointer, filnam->dsc$w_length);
     if (Length > (size_t) MAXFLEN) Length = (size_t) MAXFLEN - 1;
     (void) strncpy (real_name, filnam->dsc$a_pointer, Length);
#  else
     Length = flength (filnam, len_filnam);
     if (Length > (size_t) MAXFLEN) Length = (size_t) MAXFLEN - 1;
     (void) strncpy (real_name, filnam, Length);
#  endif
#endif
    real_name[Length] = '\0';
    for (i = 1; i < MAXFILES; i++)
      if (! strcmp (real_name, file_name[i])) break;
    *iunit = i % MAXFILES;
  }
  if (file_stream[*iunit] != NULL) {
    file_last_op[*iunit] = IRRELEVANT_OP;
    (void) fflush (file_stream[*iunit]); /* flush the output stream */
#if 0
    /* checking the return value reportedly causes problems in ultrix
       under unknown circumstances... */
    if (fflush (file_stream[*iunit]) != 0)
      file_fatal ("QQINQ: flush failed on ", file_name[*iunit]);
#endif
    position = ftell (file_stream[*iunit]);   /* remember current position */
/* It seems the \idx{OpenVMS} (don't know which version) can easily lose its */
/* place in files.  Try flushing the output buffer before messing around    */
/* with [[fseek]].  (Thanks to Richard Bryan.)  N.B.: assumes [[*iunit]]!   */
/*                                                                          */
/* <OpenVMS seek fudge>=                                                    */
#if defined (__alpha) && defined (vms)
(void) fflush (file_stream[*iunit]);
#endif
    (void) fseek (file_stream[*iunit],0L,SEEK_END); /* seek EOF */
    *length = (int) ftell (file_stream[*iunit]); /* get file size */
    if (fseek (file_stream[*iunit],position,SEEK_SET) != 0) /* seek position */
      file_fatal ("QQINQ: seek failed on ", file_name[*iunit]);
  }
}
/* \subsection{{\tt subroutine qlocate (\meta{iunit}, \meta{locate})}}      */
/* Returns the current position \meta{locate} in the diskio stream \meta{iunit}. */
/*                                                                          */
/* <diskio routines>=                                                       */
#if CALL_LIKE_HPUX
  void qlocate (iunit, locate)
  int *iunit, *locate;
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void QLOCATE (int *iunit, int *locate)
#endif
#if CALL_LIKE_SUN
  void qlocate_ (int *iunit, int *locate)
#endif
#if CALL_LIKE_MVS
  void __stdcall QLOCATE (int *iunit, int *locate)
#endif
{
  if (! initialised) 
    fatal ("QLOCATE: qopen/qqopen not yet called");
  *locate = -1;
  if (file_stream[*iunit] != NULL)
    *locate = (int) ftell (file_stream[*iunit]) / file_bytes_per_item[*iunit];
}
#ifdef _AIX
/* \section{Missing system support}                                         */
/*                                                                          */
/* Routines often found in {\tt \idx{libU77}.a} or somesuch are missing     */
/* on some systems.\index{HPUX}\index{AIX}                                  */
/*                                                                          */
/* <AIX support>=                                                           */
void idate (iarray)
     int iarray[3];
{
     struct tm *lt;
     time_t tim;
     tim = time(NULL);
     lt = localtime(&tim);
     iarray[0] = lt->tm_mday;
     iarray[1] = lt->tm_mon+1;  /* need range 1-12 */
     iarray[2] = lt->tm_year + 1900;
}
#endif
#if defined (__hpux) || defined (_AIX)
/* <AIX and HPUX support>=                                                  */
void gerror (str, Lstr)
char *str;
int  Lstr;
{
  int i;

  if (errno == 0) {             /* Avoid `Error 0' or some such message */
    for (i=1; Lstr; i++)
      str[i] = ' ';
  } else {
    (void) strncpy (str, strerror (errno), Lstr);
    for (i = strlen (str); i < Lstr; i++) str[i] = ' ';  /* pad with spaces */
  }
} /* End of gerror (str, Lstr) */

int ierrno () {
  return errno;
}

void itime (array)
     int array[3];
{
     struct tm *lt;
     time_t tim;
     tim = time(NULL);
     lt = localtime(&tim);
     array[0] = lt->tm_hour; array[1] = lt->tm_min; array[2] = lt->tm_sec;
}

static long clk_tck = 0;

#if 0                           /* dtime isn't used at present */
float dtime (tarray)
     float tarray[2];
{
  struct tms buffer;
  time_t utime, stime;
  static time_t old_utime = 0, old_stime = 0;
  if (! clk_tck) clk_tck = sysconf(_SC_CLK_TCK);
  (void) times(&buffer);
  utime = buffer.tms_utime; stime = buffer.tms_stime;
  tarray[0] = ((float)(utime - old_utime)) / (float)clk_tck;
  tarray[1] = ((float)(stime - old_stime)) / (float)clk_tck;
  old_utime = utime; old_stime = stime;
  return (tarray[0]+tarray[1]);
}
#endif                          /* dtime */

float etime (tarray)
     float tarray[2];
{
  struct tms buffer;
  time_t utime, stime;
  if (! clk_tck) clk_tck = sysconf(_SC_CLK_TCK);
  (void) times(&buffer);
  tarray[0] = (float) buffer.tms_utime / (float)clk_tck;
  tarray[1] = (float) buffer.tms_stime / (float)clk_tck;
  return (tarray[0]+tarray[1]);
}

#endif             /*  HPUX and AIX support */
#if defined(F2C) || defined(G77)
/* <f2c support>=                                                           */
int exit_ (status)
     int *status;
{
  f_exit ();                    /* may or may not be registered with
                                   exit, depending on the C libraries
                                   capabilities, but is idempotent */
  exit (*status);
}

int time_ ()
{
  return (int) time (NULL);
}

int getpid_ ()
{
  return (int) getpid ();
}

/* following are from libI77/fio.h */
#define MXUNIT 100
typedef struct
{       FILE *ufd;      /*0=unconnected*/
        char *ufnm;
        long uinode;
        int udev;
        int url;        /*0=sequential*/
        flag useek;     /*true=can backspace, use dir, ...*/
        flag ufmt;
        flag uprnt;
        flag ublnk;
        flag uend;
        flag uwrt;      /*last io was write*/
        flag uscrtch;
} unit;
extern unit f__units[];
#define TRUE_ (1)
#define FALSE_ (0)
#define err(f,m,s) {if(f) errno= m; else f__fatal(m,s); return(m);}
/* end of fio.h extract */

int isatty_ (lunit)
     int *lunit;
{
  if (*lunit>=MXUNIT || *lunit<0)
    err(1,101,"isatty");
  /* f__units is a table of descriptions for the unit numbers (defined
     in io.h) with file descriptors rather than streams */
  return (isatty(fileno((f__units[*lunit]).ufd)) ? TRUE_ : FALSE_);
}

int idate_ (iarray)
     int iarray[3];
{
     struct tm *lt;
     time_t tim;
     tim = time(NULL);
     lt = localtime(&tim);
     iarray[0] = lt->tm_mday;
     iarray[1] = lt->tm_mon+1;  /* need range 1-12 */
     iarray[2] = lt->tm_year + 1900;
     return 0;
}

int gerror_ (str, Lstr)
char *str;
int  Lstr;
{
  int i;

  if (errno == 0) {             /* Avoid `Error 0' or some such message */
    for (i=1; Lstr; i++)
      str[i] = ' ';
  } else {
    (void) strncpy (str, strerror (errno), Lstr);
    for (i = strlen (str); i < Lstr; i++) str[i] = ' ';  /* pad with spaces */
  }
  return 0;
}

int ierrno_ () {
  return errno;
}

int itime_ (array)
     int array[3];
{
     struct tm *lt;
     time_t tim;
     tim = time(NULL);
     lt = localtime(&tim);
     array[0] = lt->tm_hour; array[1] = lt->tm_min; array[2] = lt->tm_sec;
}

static long clk_tck = 0;

doublereal etime_ (tarray)      /* NB `doublereal' return for f2c. */
     float tarray[2];
{
  struct tms buffer;
  time_t utime, stime;
  if (! clk_tck) clk_tck = sysconf(_SC_CLK_TCK);
  (void) times(&buffer);
  tarray[0] = (float) buffer.tms_utime / (float)clk_tck;
  tarray[1] = (float) buffer.tms_stime / (float)clk_tck;
  return (tarray[0]+tarray[1]);
}
/* These ought to be intrinsic, but they should only be applied to          */
/* [[INTEGER]] arguments.  The types [[integer]] and [[logical]] are both   */
/* assumed to be [[int]].                                                   */
/*                                                                          */
/* <f2c support>=                                                           */
int /* integer */ ibset_ (a, b)
     int /* integer */ *a, *b;
{
  return (*a) | 1<<(*b);
}

int /* integer */ ibclr_ (a, b)
     int /* integer */ *a, *b;
{
  return (*a) & ~(1<<(*b));
}

int /* logical */ btest_ (a, b)
     int /* integer */ *a, *b;
{
  return ((((unsigned long) *a)>>(*b)))&1 ? TRUE_ : FALSE_;
}
#endif              /* F2C support  */
/* isatty doesnt seem to be in Mircrosoft Visual Studdio so this is a fudge */
#if CALL_LIKE_MVS
int __stdcall ISATTY (int *lunit)
{
  lunit = 0 ;
  return lunit;
}
#endif

/* \section{`Magic' numbers}                                                */
/*                                                                          */
/* When, for instance, an $F$ is unobserved in a derivative, we might       */
/* want to give it a special value---a `\idx{magic number}'---possibly in   */
/* addition to a special value of the $\sigma$, like a negative one.        */
/* Using such a number in a calculation (by mistake, through ignoring the   */
/* value of $\sigma$, say) should not allow one to get half-sensible        */
/* results as one might if this number was $-9999$ or some such.  (There    */
/* is non-enforced connexion between the $F$ and its $\sigma$ in the MTZ    */
/* file, although one could think of adding extra columns to the file       */
/* with bit-encoded flags telling whether the $F$ in a given column was     */
/* observed.)                                                               */
/*                                                                          */
/* The obvious tactic with \ac{ieee} arithmetic is to use a \idx{NaN}       */
/* value in such situations.  Things may be set up so that we either get    */
/* an exception on using it in arithmetic or it silently propagates to all  */
/* values using it and its presence is indicated by a NaN in the output.    */
/* On a \idx{VAX} architecture we can't use NaN, but there is the           */
/* possibility of using a                                                   */
/* `reserved operand'\index{reserved operand|see{Rop}}                      */
/* (`\idx{Rop}') value,                                                     */
/* which will cause an exception (by experiment: when used for              */
/* floating-point arithmetic {\em or\/} printed, but not when assigned).    */
/* The \idx{Convex} native mode is similar, except that the Rop may be      */
/* printed (in the form {\tt Rop0x}\meta{fraction part}).                   */
/*                                                                          */
/* On, say, the \idx{IBM 370 architecture}---which we don't currently       */
/* support---anything's a valid floating point number, and the best ploy    */
/* is probably to use the largest representable number as the `magic'       */
/* value.  This would stand a good chance of raising an overflow            */
/* exception if used.  Anyhow, if such bad use of an undefined value is     */
/* made in a program due to insufficient checking by the code, it should    */
/* be spotted on the \ac{ieee} systems and the bug fixed---it's not         */
/* strictly necessary that it should cause a fatal error on all             */
/* architectures.                                                           */
/*                                                                          */
/* We need to provide a means of setting the magic number and checking      */
/* whether a given value is such.  These are architecture-dependent         */
/* bit-level operations, hence their presence in the C code.                */
/*                                                                          */
/* The suite doesn't currently use these routines, but should do soon.      */
/* \subsection{Setting a value: {\tt subroutine qnan(value)}}               */
/*                                                                          */
/* [[qnan]] was originally a \ft{} [[real function]] returning the value    */
/* (and actually done in 2 stages) with a subroutine implementation like    */
/* this called by the \ft{} function to avoid problems under \idx{VMS}      */
/* and native \idx{Convex}.  However, the \idx{f2c} calling convention      */
/* for a function loses in that case since it assumes a [[double]] value    */
/* returned which is cast to [[float]] with a SIGFPE, sigh.                 */
/*                                                                          */
/* <magic numbers>=                                                         */
#if CALL_LIKE_HPUX
  void qnan (realnum)
  union float_uint_uchar *realnum;
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void QNAN (union float_uint_uchar *realnum)
#endif
#if CALL_LIKE_SUN
  void qnan_ (union float_uint_uchar *realnum)
#endif
#if CALL_LIKE_MVS
  void __stdcall QNAN (union float_uint_uchar *realnum)
#endif
/* We have a choice of \idx{NaN} values in                                  */
/* \ac{ieee}\index{IEEE@\ac{ieee}} arithmetic.                              */
/* [[0xfffa5a5a]] is the one used by the \idx{MIPS} compilers as an         */
/* undefined value.  Note the hex constant is the same for both byte sexes! */
/*                                                                          */
/* <magic numbers>=                                                         */
#if NATIVEFT == DFNTF_BEIEEE || NATIVEFT == DFNTF_LEIEEE
#  define NAN 0xfffa5a5a
#endif
/* For \idx{Convex} native mode and \idx{VAX} use a \idx{Rop} value:        */
/*                                                                          */
/* <magic numbers>=                                                         */
#if NATIVEFT == DFNTF_CONVEXNATIVE
#  define NAN 0x80000000
#endif
#if NATIVEFT == DFNTF_VAX
#  define NAN 0x00008000
#endif
#ifndef NAN
  #error "NAN isn't defined (needs NATIVEFT)"
#endif
{
  realnum->i = NAN;
}
/* \subsection{Testing a value: {\tt int cisnan(\meta{real})}}              */
/*                                                                          */
/* We want a \ft{} logical function [[qisnan]] to test whether its argument */
/* is a \idx{NaN} or \idx{Rop}.  We have to do this by writing a C          */
/* [[int]]-valued procedure and testing the returned value in the \ft{}     */
/* so that we don't have to assume how it represents logical values.  The   */
/* {\tt diskio}\index{diskio} library module provides the                   */
/* trivial interface [[QISNAN]].                                            */
/*                                                                          */
/* <magic numbers>=                                                         */
#if CALL_LIKE_HPUX
  int cisnan (realnum)
  union float_uint_uchar *realnum;
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  int CISNAN (union float_uint_uchar *realnum)
#endif
#if CALL_LIKE_SUN
  int cisnan_ (union float_uint_uchar *realnum)
#endif
#if CALL_LIKE_MVS
  int __stdcall CISNAN (union float_uint_uchar *realnum)
#endif
{
    /* In the \ac{ieee} case we actually return true both for \idx{NaN}s        */
    /* and for \idx{Infinity}; in either case the exponent is all ones---the    */
    /* fraction is zero for Infinity and non-zero for NaN\@.  The canonical     */
    /* test for a NaN is that it doesn't compare equal to itself, but we        */
    /* don't want to rely on the compiler avoiding a bogus optimisation anyhow. */
    /*                                                                          */
    /* <test for magic number>=                                                 */
    switch (nativeFT) {
     case DFNTF_BEIEEE :
     case DFNTF_LEIEEE :
       return ((realnum->i & 0x7f800000) == 0x7f800000); /* exponent all 1s */
    /* \idx{VAX} and \idx{Convex} \idx{Rop} has sign $=1$ and zero exponent     */
    /* with the appropriate byte sex---bit 15 and bits 7--14 respectively in    */
    /* the appropriate half-word (counting from 0).                             */
    /*                                                                          */
    /* <test for magic number>=                                                 */
      case DFNTF_CONVEXNATIVE :
        return ((realnum->i & 0xff800000) == 0x80000000);      
      case DFNTF_VAX :
        return ((realnum->i & 0x0000ff80) == 0x00008000);
      default :
        fatal("CISNAN: bad nativeFT");
        return 0;                   /* avoid compiler warning */
      }
}
/* \subsection{Absent data test for {\tt mtzlib}: {\tt subroutine           */
/*     ccpbml (\meta{ncols}, \meta{cols})}}                                 */
/* In {\tt mtzlib} there's a fudge for \idx{BIOMOL}-convention absence      */
/* flags, which are re-written to zeroes.  To do the real number            */
/* comparison, though, it's necessary to do a [[qnan]]-type test first.     */
/* We don't want to call [[qnan]] (which calls [[cisnan]]) on every         */
/* number in the data file, so the tests are amortised in this routine      */
/* which deals with a whole array \meta{cols} of length \meta{ncols}.       */
/*                                                                          */
/* <magic numbers>=                                                         */
#define MDFBIG -1.0E10          /* BIOMOL absence flag value */
#if CALL_LIKE_HPUX
  void ccpbml (ncols, cols)
  int *ncols;
  union float_uint_uchar cols[];
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void CCPBML (int *ncols, union float_uint_uchar cols[])
#endif
#if CALL_LIKE_SUN
  void ccpbml_ (int *ncols, union float_uint_uchar cols[])
#endif
#if CALL_LIKE_MVS
  void __stdcall CCPBML (int *ncols, union float_uint_uchar cols[])
#endif
{
  int i;
  for (i=0; i<*ncols; i++)
    if (cols[i].i != NAN)
      if (cols[i].f <= MDFBIG) cols[i].f = 0.0;
}
/* \subsection{Updating MTZ column ranges: {\tt subroutine ccpwrg           */
/*     (\meta{ncols}, \meta{rcols}, \meta{wmin}, \meta{wmax})}}             */
/* This is a similar fudge to [[ccpbml]] to avoid [[QISNAN]] calls in       */
/* updating the MTZ column ranges in {\tt mtzlib}.  Note that [[wminmax]]   */
/* actually indexes a 3-D Fortran array with the first                      */
/* dimension range of 2, indicating minimum and maximum values respectively. */
/*                                                                          */
/* <magic numbers>=                                                         */
#if CALL_LIKE_HPUX
  void ccpwrg (ncols, cols, wminmax)
  int *ncols;
  union float_uint_uchar cols[];
  float wminmax[];
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void CCPWRG (int *ncols, union float_uint_uchar cols[], float wminmax[])
#endif
#if CALL_LIKE_SUN
  void ccpwrg_ (int *ncols, union float_uint_uchar cols[], float wminmax[])
#endif
#if CALL_LIKE_MVS
  void __stdcall CCPWRG (int *ncols, union float_uint_uchar cols[], float wminmax[])
#endif
{
  int i;
  for (i=0; i<*ncols; i++)
    if (cols[i].i != NAN)
       if (cols[i].f > MDFBIG) {
         if (cols[i].f < wminmax[2*i]) wminmax[2*i] = cols[i].f;
         if (cols[i].f > wminmax[1+2*i]) wminmax[1+2*i] = cols[i].f; }
}
/* \subsection{Routines for Data Harvesting: {\tt subroutine hgetlimits}}    */
/* Returns largest int and largest float as defined in <limits.h> and       */
/* <float.h>                                                                 */
#if CALL_LIKE_HPUX
  void hgetlimits (IValueNotDet, ValueNotDet)
  int *IValueNotDet;
  float *ValueNotDet;
#endif
#if defined (VMS) || CALL_LIKE_STARDENT
  void HGETLIMITS (int *IValueNotDet, float *ValueNotDet)
#endif
#if CALL_LIKE_SUN
  void hgetlimits_ (int *IValueNotDet, float *ValueNotDet)
#endif
#if CALL_LIKE_MVS
  void __stdcall HGETLIMITS (int *IValueNotDet, float *ValueNotDet)
#endif
{
  *IValueNotDet = INT_MAX;
  *ValueNotDet  = FLT_MAX;
}

/* Wrap-around for mkdir function. Returns 0 if successful, 1 if directory already exists,  */
/* and -1 if other error.                                                                   */
#ifndef _MVS
#if CALL_LIKE_HPUX
  void cmkdir (path, cmode, result, Lpath, Lmode)
  int *result, Lpath, Lmode;
  const char *path, *cmode;
#endif
#if defined (VMS)
  void CMKDIR (struct dsc$descriptor_s *path, struct dsc$descriptor_s *cmode, 
      int *result)
#endif
#if CALL_LIKE_STARDENT
  void CMKDIR (struct Str_Desc *path, struct Str_Desc *cmode, int *result)
#endif
#if CALL_LIKE_SUN
  void cmkdir_ (const char *path, const char *cmode, int *result, int Lpath, int Lmode)
#endif
/*#if CALL_LIKE_MVS
  void __stdcall CMKDIR (const char *path, int Lpath, const char *cmode, int Lmode,
        int *result)
#endif*/
{ size_t Length;
  char name[MAXFLEN];
  mode_t mode;

  /* truncate path to MAXFLEN - 1 characters, MAXFLEN defined in library.h */
  Length = (size_t) Lpath;
  if (Length > MAXFLEN) Length = MAXFLEN - 1; 
  (void) strncpy (name, path, Length);
  name[Length] = '\0'; 

/* Possible modes (see stat.h)
  Currently pass 3-character string and interpret as octal.
  Try also S_IRWXU, S_IRWXG, etc. */
  sscanf(cmode,"%o",&mode);
   
  *result = mkdir(name,mode); 

  if (*result == -1) {
/* Distinguish directory-exists error from others, since usually not a problem. */
    if (errno == EEXIST) {
      *result = 1;
    }
  }
    
}

#if CALL_LIKE_HPUX
  void cchmod (path, cmode, result, Lpath, Lmode)
  int *result, Lpath, Lmode;
  const char *path, *cmode;
#endif
#if defined (VMS)
  void CCHMOD (struct dsc$descriptor_s *path, struct dsc$descriptor_s *cmode, 
     int *result)
#endif
#if CALL_LIKE_STARDENT
  void CCHMOD (struct Str_Desc *path, struct Str_Desc *cmode, int *result)
#endif
#if CALL_LIKE_SUN
  void cchmod_ (const char *path, const char *cmode, int *result, int Lpath, int Lmode)
#endif
/*#if CALL_LIKE_MVS
  void __stdcall CCHMOD (const char *path, int Lpath,const char *cmode, int Lmode,
        int *result)
#endif*/
{ size_t Length;
  char name[MAXFLEN];
  mode_t mode;

  /* truncate path to MAXFLEN - 1 characters, MAXFLEN defined in library.h */
  Length = (size_t) Lpath;
  if (Length > MAXFLEN) Length = MAXFLEN - 1;
  (void) strncpy (name, path, Length);
  name[Length] = '\0'; 

/* Possible modes (see stat.h)
  Currently pass 3-character string and interpret as octal.
  Try also S_IRWXU, S_IRWXG, etc. */
  sscanf(cmode,"%o",&mode);

  *result = chmod(name,mode); 
}
#else
#  if CALL_LIKE_MVS
   void __stdcall CMKDIR (const char *path, int Lpath, const char *cmode, int Lmode,
         int *result)
#  endif
   {
     printf("No harvesting on NT.");
     *result = -1;
   }
#  if CALL_LIKE_MVS
     void __stdcall CCHMOD (const char *path, int Lpath,const char *cmode, int Lmode,
          int *result)
#  endif
   {
     printf("No harvesting on NT.");
     *result = -1;
   }
#endif

/* CCP4MALLOC
   This is a wrapper for the malloc function, which adds some
   error trapping */

#if defined (PROTOTYPE)
  void *ccp4malloc(size_t size)
#else
  void *ccp4malloc(size)
  size_t size;
#endif
/* Wrapper for malloc function; should trap for errors */

{ void *val; 

  val = malloc (size);
  if (!val && size)
    {
      perror ("Failure in ccp4malloc");
      abort ();
    }
  return val;}

/* CCP4REALLOC
   This is a wrapper for the realloc function, which adds some
   error trapping */

#if defined (PROTOTYPE)
  void *ccp4realloc(void *ptr, size_t size)
#else
  void *ccp4realloc(ptr, size)
  void *ptr;
  size_t size;
#endif

{ void *val; 

  val = realloc (ptr, size);
  if (!val && size)
    {
      perror ("Failure in ccp4realloc");
      abort ();
    }
  return val;}

/* CCP4CALLOC
   This is a wrapper for the calloc function, which adds some
   error trapping */

#if defined (PROTOTYPE)
  void *ccp4calloc(size_t nelem , size_t elsize)
#else
  void *ccp4calloc(nelem, elsize)
  size_t nelem;
  size_t elsize;
#endif

{ void *val; 

  val = calloc (nelem, elsize);
  if (!val && elsize)
    {
      perror ("Failure in ccp4calloc");
      abort ();
    }
  return val;}
