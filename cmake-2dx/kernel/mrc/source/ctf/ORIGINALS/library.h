/* % This was apart of library.c but has been split off so it can be used   */
/* % with other c programs in the suite.                                    */
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
/*                                                                          */
/* There are several platform dependencies in the code which we need to     */
/* get right:                                                               */
/* \begin{description}                                                      */
/* \item[Fortran/C conventions] The calling conventions for C from          */
/*   \ft{} vary from compiler to compiler;                                  */
/* \item[Header files etc.] We can't assume everything has \ac{ansi} C      */
/*   or \ac{posix} libraries;                                               */
/* \item[Real number format] for the transparent binary i/o;                */
/* \item[Special things] The [[putenv]]/[[setenv]] call isn't defined in    */
/*   the current standards.                                                 */
/* \end{description}                                                        */
/*                                                                          */
/* Each type of system we know about should cause [[KNOWN_MACHINE]] to be   */
/* defined and also define \verb+CALL_LIKE_+\meta{something} to be          */
/* defined.  Thus if you know system \meta{foo} has a \ft{}                 */
/* calling convention like the native Sun compiler uses, define             */
/* [[CALL_LIKE_SUN]] and you won't need to examine the definitions of the   */
/* interface functions below.  Further tests on the system type may be      */
/* necessary e.g., to get the include files right.                          */
/*                                                                          */
/* \subsection{Assumptions}\index{assumption}                               */
/*                                                                          */
/* Note that it's assumed below that a \ft{} [[INTEGER]]                    */
/* corresponds to a C [[int]] and a \ft{} [[REAL]] corresponds to           */
/* a C [[float]].                                                           */
/*                                                                          */
/* Also, the identity of certain \idx{calling conventions} is only          */
/* guaranteed if the routines have only a single \ft{}                      */
/* \index{CHARACTER variables@{\tt CHARACTER} variables}                    */
/* [[CHARACTER]]-type argument since sometimes the length of each such      */
/* argument is given after it in the parameter list and sometimes they      */
/* are all collected at the end of the list.                                */
/*                                                                          */
/* \subsection{Platform identification}                                     */
/*                                                                          */
/* Apart from the possibility of using the \idx{Netlib} {\tt                */
/*   f2c}\index{f2c@{\tt f2c}} compiler we currently assume that each       */
/* system uses the vendor-supplied \ft{} compiler\index{FORTRAN             */
/*   compiler@\ft{} compiler}.\index{platforms}                             */
/*                                                                          */
/*                                                                          */
/* This is for \idx{IBM} Unix systems---\idx{RS/6000} models, at least.     */
/* The compiler can append \verb+_+ to external names, but we assume the    */
/* default where this doesn't happen.  See {\tt configure} for the          */
/* enforcement of this.                                                     */
/*                                                                          */
/* <identifying the platform>=                                              */
#if defined (_AIX) || defined(___AIX)
#  define KNOWN_MACHINE
#  define CALL_LIKE_HPUX 1
#endif
/* This is for \idx{Alliant} \idx{FX28xx}, at least, e.g.\ the FX2800 at LMB\@. */
/*                                                                          */
/* <identifying the platform>=                                              */
#if defined (alliant)
#  define KNOWN_MACHINE
#  define CALL_LIKE_SUN 1
#endif
/* The \idx{Ardent} \idx{Stardent}/\idx{Titan} support probably doesn't     */
/* work at present.                                                         */
/*                                                                          */
/* <identifying the platform>=                                              */
#if defined (ardent) || defined (titan)
#  ifndef stardent
#    define stardent
#  endif
#endif
#if defined (stardent)
#  define KNOWN_MACHINE
#  define CALL_LIKE_STARDENT 1
#endif
/* There seem to be two possible ways of identifying a \idx{Convex}         */
/* (`C' series, at least) system.  [[__convexc__]] is documented in OS10    */
/* but [[__convex__]] seems to be there as well (and was probably           */
/* documented in OS8).                                                      */
/*                                                                          */
/* <identifying the platform>=                                              */
#if defined (__convex__) || defined (__convexc__)
#  define KNOWN_MACHINE
#  define CALL_LIKE_SUN 1
#endif
/* The \idx{Evans and Sutherland} \idx{ESV1} workstation can operate in the */
/* \idx{BSD} or \idx{SYSV} universes.  It doesn't seem to be properly       */
/* \index{POSIX@\ac{posix}}\ac{posix}- or \ac{ansi}                         */
/* C-compliant.\index{ANSI C@\ac{ansi} C}                                   */
/*                                                                          */
/* <identifying the platform>=                                              */
#if defined (ESV)
#  define KNOWN_MACHINE
#  define CALL_LIKE_SUN 1
#endif
/* This cover \idx{Hewlett Packard} 9000/750 (RISC) models, at least.  Others */
/* may vary.                                                                */
/*                                                                          */
/* <identifying the platform>=                                              */
#if defined (__hpux) 
#  define KNOWN_MACHINE
#  define CALL_LIKE_HPUX 1
#endif
/* \idx{Silicon Graphics} \idx{IRIX} systems \idx{Iris}es, \idx{Indigo}s,   */
/* \idx{Crimson}s etc. (at least version 4 up) are                          */
/* \ac{ansi}\index{ANSI C@\ac{ansi} C} and                                  */
/*  \ac{posix}\index{POSIX@\ac{posix}} compliant.                           */
/*                                                                          */
/* <identifying the platform>=                                              */
#ifdef __sgi   /* in ANSI mode */
#  ifndef sgi
#    define sgi
#  endif
#endif
#if defined (sgi)
#  define KNOWN_MACHINE
#  define CALL_LIKE_SUN 1
#endif
/* \idx{Solbourne}s are \idx{Sun} clones.                                   */
/*                                                                          */
/* <identifying the platform>=                                              */
#if defined (solbourne) 
#  ifndef sun
#   define sun               /* don't know whether it's defined or not */
#  endif
#endif
/* THis is OK for \idx{Solaris}1 and~2.                                     */
/*                                                                          */
/* <identifying the platform>=                                              */
#if defined (sun) || defined (__sun)
#  define KNOWN_MACHINE
#  define CALL_LIKE_SUN 1
#  if !defined(__STDC__) || defined(__GNUC__)
#    if !defined(G77)
      extern char *sys_errlist [];
#     define strerror(i) sys_errlist[i] /* k&r compiler doesn't have it */
#    endif
#  endif
#endif
/* \idx{DEC} \idx{OSF/1} (\idx{Alpha}) and \idx{Ultrix} use the same        */
/* calling conventions, at least.  The documentation I saw for OSF/1 said   */
/* that [[__OSF1__]] is defined, but it's reported that you need            */
/* [[__osf__]] (in come cases?).                                            */
/*                                                                          */
/* <identifying the platform>=                                              */
#if defined (ultrix) || defined(__OSF1__) || defined(__osf__)
#  define KNOWN_MACHINE
#  define CALL_LIKE_SUN 1
#endif
/* \idx{VMS} is a law unto itself, of course.  Help for VAX C doesn't       */
/* actually say [[VMS]] is defined (as opposed to [[vms]]), although it     */
/* seems to be.  The other possibilities are for DEC C in strict            */
/* ANSI mode.  NB: we now don't use the C code under VMS due to the         */
/* apparent bugs in the DEC C RTL\@.                                        */
/*                                                                          */
/* <identifying the platform>=                                              */
#ifndef VMS
#  if defined (vms) || defined (__vms) || defined (__VMS)
#    define VMS
#  endif
#endif
#if defined (VMS)
#  define KNOWN_MACHINE
#endif

/* First attemt at porting to nt wsing the visual fortran and MVC++         */
/* MVS stands for Microsoft Visual Studio - better than VMS                 */
#if defined(_MVS) 
#  define CALL_LIKE_MVS 1
#  define KNOWN_MACHINE
#endif

/* {\tt f2c}\index{f2c@{\tt f2c}} misses the MIL--STD                       */
/* \idx{bit-twiddling intrinsics}.  Its calling                             */
/* convention is like \idx{Sun} only for a {\em single\/} [[CHARACTER]]     */
/* variable in the parameter list!  {\tt g77}\index{g77@{\tt g77}} has      */
/* (will have!)\ the same calling convention and library as {\tt f2c} but   */
/* does support the MIL--STD intrinsics (amongst other things).  The        */
/* alpha-test version doesn't have a {\tt BYTE} (or {\tt INTEGER*1})        */
/* type, so can't be used at present.                                       */
/*                                                                          */
/* <identifying the platform>=                                              */
#if defined(F2C) || defined(G77)
#  undef CALL_LIKE_SUN
#  define CALL_LIKE_SUN 1
#  define KNOWN_MACHINE
#endif
/* If we haven't identified the system type, we want to stop with an        */
/* error message.  Indenting [[#error]] works with \ac{ansi} C              */
/* \index{ANSI C@\ac{ansi} C}                                               */
/* and doesn't fall over with K\&R\index{K&R@K\&R C} as                     */
/* it would if un-indented, even when the test is false.                    */
/*                                                                          */
/* <guarded code>=                                                          */
#if ! defined (KNOWN_MACHINE)
  #error System type is not known -- see the Installation Guide
#else
/* At this stage we've identified the platform and are in business.  Here   */
/* are the components we have to put together.                              */
/*                                                                          */
/* <general code>=                                                          */
/* \section{Header files}                                                   */
/*                                                                          */
/* If the system has \ac{posix} stuff, we want to ensure it's used.         */
/*                                                                          */
/* <header files>=                                                          */
#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
/* <header files>=                                                          */
#include <stdio.h>

#if defined (VMS)
#  include <descrip.h>          /* non-POSIX */
#  define NOUNISTD
#else
#  include <sys/types.h>
#  include <sys/stat.h>
#  ifndef _MVS
#    include <sys/times.h>
#  endif
#  ifdef _MVS
#    define NOUNISTD
#  endif
#endif

#ifdef stardent                 /* who knows if this works anyhow... */
#  include <sys/types.h>
#  include <malloc.h>           /* non-POSIX */
#else
#  include <stddef.h>
#endif
/* BSD might need {\tt strings.h} and [[index]] instead of [[strchr]].      */
/* This is an \ac{ansi} header, not \ac{posix.1}.                           */
/*                                                                          */
/* <header files>=                                                          */
#include <string.h>
/* Some systems like \idx{ESV} don't have {\tt unistd.h}, and the           */
/* configuration makes the appropriate [[#define]] of [[NOUNISTD]].         */
/*                                                                          */
/* <header files>=                                                          */
#ifndef NOUNISTD
#  include <unistd.h>
#else
#  ifndef VMS 
#    ifndef _MVS
#      include <sys/file.h>     /* ESV, old Concentrix */ /* non-POSIX */
#    endif
#  endif
#endif
#ifndef NOSTDLIB                /* for TitanOS 4.2, at least? */
#  include <stdlib.h>
#endif

#include <errno.h>
#include <ctype.h>

#if defined(_AIX) || defined (__hpux) || defined(F2C) ||\
    defined(G77) || defined(_WIN32)/* would do no harm on others, though */
#  include <time.h>
#endif
/* We need INT_MAX and DBL_MAX defined for routine Hgetlimits               */
/* These should be in {\tt limits.h} and {\tt float.h} respectively         */
/* (this is POSIX standard?).                                               */
/*                                                                          */
/* <header files>=                                                          */

#include <limits.h>
#include <float.h>

/* For f2c we need this for typedefs.  We assume it's on an include         */
/* path where (g)cc will find it.  The [[#define]] is to avoid the          */
/* undefinition of macros like [[sgi]].                                     */
/*                                                                          */
/* this has been altered for g2c.h as f2c.h is not always distributed       */
/* <header files>=                                                          */
#if defined (F2C)
#  define Skip_f2c_Undefs
#  include "f2c.h"
#endif
#if defined (G77)
#  define Skip_f2c_Undefs       /* g2c.h infelicity... */
#  if defined (HAVE_G2C_H)
#    include "g2c.h"
#  else
#    include "f2c.h"
#  endif
#endif
/* \section{[[#define]]s}                                                   */
/* \subsection{Defaults and customisable items}                             */
/*                                                                          */
/* \fixme{We should be able to get help from \ac{posix} on the filename     */
/* length and open files limits}                                            */
/*                                                                          */
/* <[[#define]]s>=                                                          */
#define MAXFLEN       500       /* the maximum length of a filename in CCP4 */
#define MAXFILES       10    /* maximum number of files open symultaneously */
#define DEFMODE         2    /* default mode access for random access files */
/* These constants record the current i/o status of a stream (needed to     */
/* know if an [[fseek]] is needed or not before the next i/o operation).    */
/*                                                                          */
/* <[[#define]]s>=                                                          */
#define IRRELEVANT_OP   0
#define READ_OP         1
#define WRITE_OP        2
/* \subsection{Machine dependent stuff}                                     */
/* These should be defined in {\tt stdlib.h}, but this isn't present, for   */
/* instance in \idx{Alliant} \idx{Concentrix} before release~3 or with the  */
/* bundled \idx{SunOS} {\tt cc}.                                            */
/*                                                                          */
/* <[[#define]]s>=                                                          */
#ifndef SEEK_SET
#  define SEEK_SET 0
#  define SEEK_CUR 1
#  define SEEK_END 2
#endif /* ! SEEK_SET */
/* <[[#define]]s>=                                                          */
#if defined (ardent) || defined (titan) || defined (stardent)
  struct Str_Desc {
    char *Str_pointer;
    int  Str_length;
    int id;
  };
#endif
/* \subsection{File mode definitions}                                       */
/*                                                                          */
/* Here are the deinfitions of the {\tt diskio} modes, specifying the       */
/* type of data transfer: bytes, half-words, integers, reals,               */
/* half(integer)-word complex and complex, respectively:                    */
/*                                                                          */
/* <[[#define]]s>=                                                          */
#define BYTE  0
#define INT16 1   
#define INT32 6
#define FLOAT32 2
#define COMP32  3
#define COMP64  4
/* \section{Converting foreign binary number formats}                       */
/*                                                                          */
/* The library is intended to allow the binary file formats (\idx{MTZ}      */
/* and map\index{map files}) to be read satisfactorily if they were         */
/* written on another platform.  Such files are always written in the       */
/* {\em native\/} real or integer number format with a `\idx{machine        */
/*   stamp}' in the file to identify the formats involved.  Then, if        */
/* necessary, conversion is done from the foreign format to native when     */
/* the file is read.  There is thus only a significant overhead for files   */
/* imported                                                                 */
/* from platforms with different number formats; locally-written files      */
/* are read back optimally and there is no write overhead.  This is in      */
/* contrast, for instance, to the \idx{XDR} approach (and \idx{HDF}?),      */
/* where a canonical external format is used.                               */
/*                                                                          */
/* When converting from foreign to native formats we're potentially faced   */
/* with a combinatorial explosion---currently combinations of \ac{ieee}     */
/* little-endian, \ac{ieee} big-endian, \idx{VAX} and \idx{Convex} native   */
/* formats.  (This applies only to real number formats---fortunately        */
/* everything we're interested in has \idx{twos complement} integers.)  Thus we */
/* first make sure that the format is converted to canonical form (which    */
/* we choose as big-endian \ac{ieee}, following XDR) and then, if           */
/* necessary, to the native format in a separate stage.  This is going to   */
/* be somewhat slower than it might be, but what the heck\dots{}            */
/*                                                                          */
/* The basic idea of this is due to David Wild (EMBL, Hamburg, 1991).       */
/* His original, partially-functional implementation used code from the     */
/* \idx{HDF} 3.1 distribution.  This re-write is by Dave Love, very         */
/* loosely based on HDF3.3, but doing the conversion in-place.  It works    */
/* for the full set of relevant systems and no longer has MTZ- and          */
/* map-specific code in [[copen]].  (HDF stuff can be found on {\tt         */
/*   ftp.ncsa.uiuc.edu}.)                                                   */
/*                                                                          */
/* \subsection{`Machine stamps'}                                            */
/*                                                                          */
/* Here's how we specify the number formats for machines.  The              */
/* `\idx{machine stamp}' is a 32-bit quantity containing a set of four      */
/* `nibbles' (half-bytes)---only half the space is used.  Each nibble is    */
/* a number specifying the representation of (in C terms) [[double]]        */
/* ($d$), [[float]] ($f$), [[int]] ($i$)) and [[unsigned char]] ($c$)       */
/* types.  Thus each stamp is of the form $\mbox{{\tt 0x}}dfic0000$.  The   */
/* values for the nibbles may be taken from the list (following HDF):       */
/* \begin{quote}                                                            */
/*   \begin{tabular}{ll}                                                    */
/*       1 & Big-endian \ac{ieee}\\                                         */
/*       2 & VAX \\                                                         */
/*       3 & Cray \\                                                        */
/*       4 & Little-endian \ac{ieee}\\                                      */
/*       5 & Convex native \\                                               */
/*       6 & Fijitsu VP                                                     */
/*   \end{tabular}                                                          */
/* \end{quote}                                                              */
/* \idx{Cray} isn't relevant to us because it's not a 32-bit machine        */
/* and we don't currently have a use for the \idx{Fujitsu} one, which isn't */
/* implemented here.  We ignore the possibility of                          */
/* non-\ac{ascii}\index{ASCII@\ac{ascii}} characters which might need       */
/* converting e.g., from \ac{ebcdic}\index{EBCDIC@\ac{ebcdic}} and $c$ is   */
/* always $1$; also $f$ and $d$ are the same (as per \idx{Fortran}).  See the */
/* \idx{HDF} code for character code possibilities.                         */
/*                                                                          */
/* Here are the tags for different formats (`\idx{class info codes}'),      */
/* not all relevant:                                                        */
/*                                                                          */
/* <[[#define]]s>=                                                          */
/* class info codes for int */
#define DFNTI_MBO       1       /* Motorola byte order 2's compl */
#define DFNTI_IBO       4       /* Intel byte order 2's compl */

/* class info codes for float */
#define DFNTF_BEIEEE    1       /* big endian IEEE (canonical) */
#define DFNTF_VAX       2       /* Vax format */
#define DFNTF_CONVEXNATIVE 5    /* Convex native floats */
#define DFNTF_LEIEEE    4       /* little-endian IEEE format */
/* Here are the definitions                                                 */
/* we're interested in.  Note\index{assumption} that some of the symbols    */
/* tested here to determine the machine type might need to be qualified     */
/* in the future where they don't necessarily determine the architecture.   */
/* We just need to set [[nativeFT]] and [[nativeIT]], which determine the   */
/* native real and integer formats.                                         */
/* First an obvious one:                                                    */
/*                                                                          */
/* <[[#define]]s>=                                                          */
#if defined (VAX) || defined (vax) /* gcc seems to use vax */
#  define NATIVEFT DFNTF_VAX
#  define NATIVEIT DFNTI_IBO
#endif
/* Here are the possibilities for little-endian \ac{ieee}.  (The            */
/* \idx{MIPS} compilers define [[MIPSEL]] or [[MIPSEB]] depending on the    */
/* mode in which the the chip operates.)  The architectures covered here    */
/* include some R\meta{nnnn} (e.g., \idx{DECstations}), \idx{i860} and      */
/* other \idx{Intel} chips like \idx{PCs} and \idx{Alpha} (sometimes!).     */
/*                                                                          */
/* <[[#define]]s>=                                                          */
#if defined(MIPSEL) || defined(alliant) || defined(i386) || defined(i860)
#  define NATIVEIT DFNTI_IBO
#  define NATIVEFT DFNTF_LEIEEE
#endif
/* Here is a first attempt at machines using the powerPC chip.              */
/* Specifically, this has been tried on PowerMacs running LinuxPPC, which   */
/* appears to be big-endian. But in principle the powerPC chip can support  */
/* both big-endian and little-endian OS's under software control. The       */
/* symbol "powerpc" appears in gcc-2.8.1/config/rs6000/linux.h and appears  */
/* to distinguish LinuxPPC from other OS's for this chip.                   */
#if defined (powerpc)
#  define NATIVEIT DFNTI_MBO
#  define NATIVEFT DFNTF_BEIEEE
#endif
/* \idx{Alpha} \idx{VMS} is a pain: compiler switches can force             */
/* \idx{VAX} or \ac{ieee} number formats.  Thus if we know it's an Alpha,   */
/* we have to check for VMS and then what sort of VMS numbers.  [OSF and    */
/* OpenVMS define [[__alpha]], OpenVMS, only [[__ALPHA]].\index{Alpha}      */
/*                                                                          */
/* <[[#define]]s>=                                                          */
#ifdef __alpha
#  ifdef VMS
#    if __IEEE_FLOAT == 1
#      define NATIVEFT DFNTF_LEIEEE
#    else
#      define NATIVEFT DFNTF_VAX
#    endif
#  else                       /* assume OSF/1 */
#    define NATIVEFT DFNTF_LEIEEE
#  endif
#  define NATIVEIT DFNTI_IBO
#endif
/* Big-endian \ac{ieee} includes other R\meta{nnnn} like SGI machines,      */
/* \idx{HP} beasts (\idx{68k}-based or \idx{RISC}), \idx{RS/6000} and all   */
/* \idx{Sun}s except the obsolete i386-based ones.                          */
/* \idx{Apollo}s are also apparently in this category.                      */
/*                                                                          */
/* <[[#define]]s>=                                                          */
/* the VAX VMS compiler objected to splitting the following line */
#if defined(MIPSEB) || defined(__hpux) || defined(_AIX) || defined(m68k) || defined(mc68000) || defined(sparc)
#  define NATIVEIT DFNTI_MBO
#  define NATIVEFT DFNTF_BEIEEE
#endif
/* \idx{Convex}s can operate in either native or \ac{ieee} mode:            */
/*                                                                          */
/* <[[#define]]s>=                                                          */
#if defined(__convex__) || defined(__convexc__)
#  define NATIVEIT DFNTI_MBO
#  ifdef _IEEE_FLOAT_
#    define NATIVEFT DFNTF_BEIEEE
#  else
#    ifdef _CONVEX_FLOAT_
#      define NATIVEFT DFNTF_CONVEXNATIVE
#    else
       #error "Can't determine Convex floating point type. Use native compiler"
#    endif
#  endif
#endif
#ifndef NATIVEFT
  #error "Can't determine machine number format"
#endif
/* Here are the codes for data types which we can read from files and       */
/* translate.                                                               */
/*                                                                          */
/* <[[#define]]s>=                                                          */
#define DFNT_UINT       0       /* unsigned int */
#define DFNT_SINT       1       /* short int */
#define DFNT_INT        2       /* int */
#define DFNT_UCHAR      3       /* unsigned char */
#define DFNT_CHAR       4       /* char */
#define DFNT_FLOAT      5       /* float */
#define DFNT_DOUBLE     6       /* double */
/* These typedefs define 16-bit unsigned, 32-bit unsigned, 32-bit float     */
/* and 8-bit unsigned char types respectively.  You'd need to define        */
/* [[SIXTEENBIT]] for a compiler with 16-bit ints; using [[long]] here is   */
/* wrong, for instance, on \idx{OSF/1} \idx{Alpha} systems.                 */
/*                                                                          */
/* <typedefs>=                                                              */
typedef unsigned short uint16;
#ifdef SIXTEENBIT
typedef unsigned long uint32;
#else
typedef unsigned int uint32;
#endif
typedef float float32;
typedef unsigned char uint8;
/* typedef signed char sint8; */ /* not K&R ? */
#endif
