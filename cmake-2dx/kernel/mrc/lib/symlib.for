C
C     This code is distributed under the terms and conditions of the
C     CCP4 licence agreement as `Part i)' software.  See the conditions
C     in the CCP4 manual for a copyright statement.
C
      SUBROUTINE SYMLIB_OUTLINE

C_BEGIN_SYMLIB
C   SYMLIB
C
C $Date: 1999/11/17 13:52:26 $
C
C 1) Subroutines for manipulating symmetry operators.
C
C   1a) Deriving symmetry operator matrices
C    invsym        msyget        msymlb        symfr2
C    symtrn        msymlb2       msymlb3       symfr3
C    symtr3        symtr4
C
c  Internal routines:-
C    determ     
C
C   1b) Deriving information from symmetry operators
C    pgdefn        pgmdf         pgnlau        patspg
C    hklrange
C
C 2) Subroutines for testing reflection data.
C
C   2a) Centric reflections
C    centr         centric       centrc
C
C   2b) Epsilon zones
C    epsln         epslon        sysab        hkleq
C
C 3) Subroutines for choosing asymmetric units.
C
C  3a) Asymmetric units for reflection data
C    asuset       asuput         asuget  	asuphp
C
c  Internal routines:-
C    prtrsm       inasu 
C
C  3b) Asymmetric units for choosing asymmetric units
C    in real space consistent with FFT expectations; FFT grids etc.
C    setlim       setgrd
C
c  Internal routines:-
C    fndsmp       factrz
C
C 4) Subroutines for testing coordinate  data.
C    xspecials    krotC    calc_orig_ps
C
C 5) Subroutines for permuting symmetry operators, etc.
C    prmvci        prmvcr        rotfix      
C
C 6) Subroutines for generating and accessing hash table.
C    ccp4_hash_lookup            ccp4_hash_setup
C    ccp4_hash_zeroit
C
C 7) Miscellaneous subroutines.
C    setrsl        sthlsq        sts3r4        pstoph
C
C_END_SYMLIB
C_BEGIN_OUTLINE
C
C  Brief Description.
C  =================
C
C Group 1: Subroutines for deriving and manipulating symmetry operations
C=======================================================================
C
C Group 1a: Deriving symmetry operator matrices:
C-----------------------------------------------
C
C---- SUBROUTINE MSYGET(IST,LSPGRP,NSYM,ROT)
C                Get symmetry operations for space-group LSPGRP
C                from library file on stream IST, logical name SYMOP.
C         Returns NSYM           = number of symmetry operations
C                 ROT(4,4,NSYM)  = rotation/translation  matrices
C
C---- SUBROUTINE MSYMLB(IST,LSPGRP,NAMSPG,NAMPG,NSYMP,NSYM,ROT)
C
C Get symmetry operations from library file
C on stream IST, logical name SYMOP.
C  Space group defined by LSPGRP - spacegroup number or
C                         NAMSPG - spacegroup name.
C
C Returns
C   LSPGRP      spacegroup number
C   NAMSPG      spacegroup name
C   NAMPG       pointgroup name
C   NSYMP        number of primitive symmetry operations
C   NSYM         number of symmetry operations
C   ROT(4,4,NSYM)  rotation/translation  matrices
C
C---- SUBROUTINE MSYMLB2(IST,LSPGRP,NAMSPG_CIF,NAMPG,NSYMP,NSYM,ROT)
C
C     Identical to MSYMLB, except that on output NAMSPG_CIF
C     has correct CIF format, e.g. 'P 21 21 21'
C     NAMSPG_CIF should be as in _symmetry.space_group_name_H-M
C
C---- SUBROUTINE MSYMLB3(IST,LSPGRP,NAMSPG_CIF,NAMSPG_CIFS,
C    +                   NAMPG,NSYMP,NSYM,RlSymmMatrx)
C
C     Another version of MSYMLB, with the following changes:
C
C   1) The routine will try first to match the assigned NAMSPG_CIF
C      to ANY name given on the spacegroup line:
C      It is satisfied by the first fit it finds:
C      eg: 47 8 8 Pmmm PGmmm ORTHORHOMBIC 'P 2/m 2/m 2/m' 'P m m m'
C      You could call the subroutine with 
C      NAMSPG_CIF = 'Pmmm' or 'P 2/m 2/m 2/m' or 'P m m m'
C
C      But it will always return the LONGEST possible name. ie 'P 2/m 2/m 2/m'

C   2) If there is no match to the spacegroup NAME, the space group is
C      identified by its number.
C      This requires that the number is UNIQUE, so alternate settings are
C      numbered n000 + Int Tab number
C
C   3) The point group name is always guessed at by the SR PGDEFN
C      and the guess on the data line is ignored.
C
C   4) The number of primitive symmetry operators is also determined
C      by the SR PGDEFN. The non-primitive operators are stored in the
C      first NSYMP symmetry matrices.
C      and the guess on the data line is ignored.
C
C   5) The symmetry operators are checked to make sure they are a
C      closed group.
C
C   In the library file, the header for each entry may contain
C   Order not guaranteed, but must start with:
C   LSPGRP   NLINS and contain either NAMSPG or NAMSPG_CIF
C
C      LSPGRP   NLINS   NLINP   NAMSPG  NAMPG CRYSTAL  NAMSPG_CIF
C
C  where  LSPGRP        spacegroup number
C         NLINS         total number of lines of symmetry operators.
C         NLINP         number of LINES of primitive symmetry operators
C                       (Not used now..)
C         NAMSPG_CIF    spacegroup name
C         NAMPG         name of corresponding pointgroup
C                       (Not used now..)
C
C On entry:
C   IST         stream number to read file
C   LSPGRP      spacegroup number
C   NAMSPG or NAMSPG_CIF
C               any acceptable spacegroup name: this will be used to 
C                       identify the spacegroup if possible
C Returns
C   LSPGRP      spacegroup number
C   NAMSPG_CIF  full spacegroup name 
C   NAMSPG_CIFS name without any spaces
C   NAMPG       pointgroup name ( obtained from pgdefn - not 100% reliable!)
C   NSYMP       number of primitive symmetry operations obtained from pgdefn- 
C               only different from NSYM in non-primitive spacegroups
C   NSYM        total number of symmetry operations
C   RlSymmMatrx(4,4,NSYM)  Symmetry Rotation/translation  matrices
C
C---- SUBROUTINE SYMFR2(ICOL,I1,NS,ROT)
C         Input ICOL - character string
C               I1   - first character to interpret from.
C
C                translates a character string  into a 4*4 matrix.
C         On exit, ROT(4,4,NS) contains the real-space symmetry
C                  matrices, in standard convention, ie
C
C                      [x']    = [s][x]
C
C                x'(I)=Sum(J=1,3)ROT(I,J,NS)*x(J) + ROT(I,4,NS)
C                 ROT(I,4,NS)    contains the fractional translations
C
C
C---- SUBROUTINE SYMFR3(ICOL,I1,NS,ROT,EFLAG)
C
C       Read and interpret symmetry operations
C       Arguments :
C       ICOL      (I)	CHARACTER*80    Line containing the symmetry ops
C       I1        (I)	INTEGER         First character to look at
C                               	(say after keyword 'SYM')
C       NS        (I/O)	INTEGER         is the number of the first symmetry
C                               	operation to be read, & returns with the
C                               	number of the last one read (ie you can
C                               	have more than one on a line!)
C       ROT       (O)	REAL            Array (4,4,at_least_NS),
C                               	on exit contains the real-space
C                               	symmetry matrices, in standard
C                               	convention, ie
C                                 	[x']    = [s][x]
C                     			x'(I)=Sum(J=1,3)ROT(I,J,NS)*x(J) + ROT(I,4,NS)
C                     			ROT(I,4,NS) contains the fractional translations
C       EFLAG     (O)	INTEGER         Error flag - on exit,
C                                  	if 0 then OK,
C                                  	gt 0, an error occurred.
C
C
C---- SUBROUTINE INVSYM(S,ST)
C          Input S    - 4*4 matrix
C          Output ST  - 4*4 matrix
C                Inverts 4*4 matrices - used here to get inverse
C                symmetry operation for generating equivalent h k l.
C                    ie                 [h']    = [h][St]
C
C                   h'(j) =Sum(I=1,3)[ h(i)*St(I,J,NS)]
C

C
C---- SUBROUTINE SYMTRN(NSM,RSM)
C           symmetry translation from matrix back to characters
C
C           This translates the symmetry matrices RSM(4,4,NSM) into INT TAB
C           character strings
C
C           It gives the real and reciprocal space operations.
C                eg     X,Y,Z        H  , K, L
C                eg     -Y,X-Y, Z   -H-K, H, L  etc
C           That is more complicated than you might think!!
C
C
C---- SUBROUTINE SYMTR3(NSM,RSM)
C           symmetry translation from matrix back to characters
C
C           This translates the Symmetry matrices into INT TAB
C           character strings
C
C           It gives the real space operations.
C                eg     X,Y,Z
C                eg     -Y,X-Y, Z
C           That is more complicated than you might think!!
C
C     Arguments :
C     NSM       (I)     INTEGER         Number of Symmetry operations
C     RSM       (I)     REAL            Array of dimension (4,4,at least NSM)
C                                       containing symmetry operations on input
C     SYMCHS    (O)     CHARACTER*(*)   Array of dimension at least NSM
C                                       containing int tab char strings on output
C     IPRINT    (I)     INTEGER         Print flag
C                                       =0 No printing
C                                       =1 Print the int tab strings
C
C---- SUBROUTINE SYMTR4(NSYM,RSM,SYMCHS)
C           symmetry translation from matrix back to characters
C
C           This translates the Symmetry matrices into INT TAB
C           character strings
C
C           It gives the real space operations.
C                eg     X,Y,Z
C                eg     -Y,X-Y, Z
C           That is more complicated than you might think!!
C
C      Arguments :
C      Nsym (I) INTEGER   Number of Symmetry operations
C      Rsm  (I) REAL      Array of dimension (4,4,at least Nsym)
C                         coNTaining symmetry operations on input
c      Symchs (O) CHARACTER*(*)   Array of dimension at least Nsym
C                         coNTaining int tab char strings on output
C
C---- SUBROUTINE DETERM(det,a)  
C          Input A - 4*4 matrix  (real)
C          Output DET - determinant of A.
C
C
C Group 1b: Deriving information from the symmetry operations:
C ------------------------------------------------------------
C
C---- SUBROUTINE PGDEFN(NAMPG,NSYMP,NSYM,RSYMT,LPRINT)
C        Input NSYM  - number of symmetry operators. ( integer)
C        Input RSYMT - 4*4 symmetry matrices. ( real)
C        Input LPRINT  - printing flag. ( logical)
C        Returns  NAMPG - character - name of point group.
C        Returns  NSYMP - integer - number of primitive symmetry operators.
C        Returns  RSYMT - possibly reordered.
C
C      This subroutine chooses the primitive set of symmetry operators.
C
C      If necessary it re-orders the symmetry operators to give the 
C      primitive ones first.
C
C      This subroutine works out the point group name NAMPG 
C      That is ; it checks rotation axes, etc etc and recognises these
C      point groups.  (It DOES NOT cope with mirror planes etc)
C
C       Gronigen MDF stuff:  It now sets up the common block MDFPAR for 
C       MDF file mods and  fills in the symmetry info.  See subroutine for
C       details
C
C
C---- SUBROUTINE PGMDF(JLASS,JCENTR,JSCREW)
C
C       Gronigen subroutine.
C     Use this subroutine to transfer information to and from MDFPAR.
C  If JLASS eq 0   then fill JLASS JCENTR JSCREW from common block.
C  If JLASS gt 0   then fill KLASS ICENTR ISCREW in common block.
C
C
C---- SUBROUTINE PGNLAU(NAMPG,NLAUE,LAUNAM)
C---- Choose Laue group from PG name.
C
C     On entry:
C     NAMPG      point-group name ( character)
C
C     On exit:
C     NLAUE     Laue group number ( integer)
C     LAUNAM    Laue group name ( character)
C
C       This subroutine returns a laue code number used to choose
C      the unique region of reciprocal space for
C      each point group.  
C       The number nlaue is the same as the one set in CAD for
C       this purpose.
C
C   Pointgroup Laue group        Limits
C
C   3 pg1     1bar       hkl:l>=0  hk0:h>=0  0k0:k>=0   1,2
C     pg1bar
C   4 pg2 (b) 2/m        hkl:k>=0, l>=0  hk0:h>=0       3/b,4/b....
C     pgm pg2/m
C   5 pg2 (c) 2/m        hkl:k>=0, l>=0  h0l:h>=0       1003,1004
C   6 pg222   mmm        hkl:h>=0, k>=0, l>=0            16 ...
C     pgmm2 pgmmm 
C   7 pg4     4/m        hkl:h>=0, l>=0 with k>=0 if  h=0  and
C     pg4bar pg4/m                            k>0 if h>0
C   8 pg422   4/mmm       hkl:h>=0, k>=0, l>=0            89..
C     pg4mm pg4bar2m pg4barm2 pg4/mmm
C   9 pg3     3bar      hkl:h>=0, k>0  00l:l>0         143..
C     pg3bar
C  10 pg312   3/m        hkl:h>=0, k>=0 with k<=h for all l.
C     pg32 pg3m pg3m1 pg3barm1 if k = 0  l>=0
C           Space group numbers :   149-151-153 157 159 162 163
C  11 pg321   3bar1m     hkl:h>=0, k>=0 with k<=h for all l.
C     pg31m pg3bar1m      if h = k  l>=0
C           Space group numbers :   150-152-154
C  12 pg6     6/m        hkl:h>=0, k>=0, l>=0 with k>=0 if  h=0
C     pg6bar  6/m        and k> 0 if h>0
C  13 pg622   6/mmm       hkl:h>=0, k>=0, l>=0 with h>=k 177..
C     pg6mm pg6barm2 pg6bar2m  pg 6/mmm
C  14 pg23    m3         hkl:h>=0, k>=0, l>=0 with l>=h,  k>=h
C     pgm3bar 
C  15 pg432   m3m        hkl:h>=0, k>=0, l>=0  with  k>=l
C     pg4bar3m pgm3barm
C
C
C---- SUBROUTINE PATSGP(SPGNAM, PGNAME, PATNAM, LPATSG)
C
C Determine Patterson spacegroup from true space-group
C
C On entry:
C     SPGNAM    space-group name. Only used to determine lattice centering
C     PGNAME    point-group name
C
C On exit:
C     PATNAM    name of Patterson spacegroup
C     LPATSG    number of Patterson spacegroup
C
C
C---- SUBROUTINE HKLRANGE(IHRNG0,IKRNG1,IKRNG0,IKRNG1,ILRNG0,ILRNG1)
C     
C     Return HKL ranges chosen in PGNLAUE
C     
C       INTEGER HRNG0,HRNG1,KRNG0,KRNG1,LRNG0,LRNG1
C
C
C Group 2: Subroutines for testing reflection data
C======================================================================
C
C Group 2a: Centric Reflections:
C-------------------------------
C
C---- SUBROUTINE CENTRIC(NSM,RSMT,IPRINT)
C       This is Randy Read's method of defining centric reflections.
C       It uses NSM and the symmetry operators stored in RSMT(4,4,NSM)
C
C         It decides how many centric zones there are, and flags them.
C----     set up tests for 0kl h0l hk0 hhl hkh hkk h,-hl hk-h hk-k
C          -h 2h l   2h -h l  hkl
C         Zones are encoded using an idea from a program by bricogne.
C         If h*zone(1) + k*zone(2) + l*zone(3) is equal to 0.0,
C         that reflection is in that zone.  all that is needed is the
C         most general conditions--a reflection is either centric or
C         not.
C
C---- SUBROUTINE CENTR(IH,IC)
C
C        Input IH(3) - reflection indices
C        Returns IC
C         Determine whether a reflection is centric (return ic=1)
C         or not (ic=0).  If none of the zone tests is satisfied,
C         the reflection is non-centric.
C
C---- LOGICAL FUNCTION CENTRC(KHKL,ICENT)
C
C     returns value true if reflection khkl is centric, false otherwise.
C     general for all point groups - but only for the unique set of
C     indices which conforms to the criterion of maximising the value
C     of        (khkl(3)*256 + khkl(2))*256 + khkl(1)
C
C    as produced by e.g. subroutine turnip in protin and ulysses.
C
C---- in this case the required tests are controlled by 7 flags in
C     icent fo
C
C  0KL  H0L  HK0  HKK  HKH  HHL  H,-2H,L
C     (the last is needed in pg312)
C
C
C Group 2b: Epsilon Zones:
C-------------------------
C
C---- SUBROUTINE EPSLN(NSM,NSMP,RSMT,IPRINT)
C
C       It works out the epsilon cards
C       It uses NSM and the symmetry operators stored in RSMT(4,4,NSM)
C       This is Randys program description
C
C         zones defined as for centric zones, but
C         fourth number on each line is the multiplicity corresponding
C         to this zone.  last card should always be 0 0 0 n, where n is
C         appropriate for the lattice (1 for primitive, 2 for face-
C         centred, etc.), so that general reflections get a value
C         for epsilon. be very careful of the order of the zones. cards
C         for reciprocal lattice rows should be given before those for
C         planes, because the first test that is satisfied defines
C         the zone.
C
C       set up tests for
C        h00 0k0 00l hh0 h0h 0kk h,-h0 h0-h 0k-k -h2h0 2h-h0 hhh hkl
C
C
C---- SUBROUTINE EPSLON(IH,EPSI,ISYSAB)
C        Input IH(3) - reflection indices
C        Returns EPSI ( epsilon zone) , and ISYSAB flag.
C        Systematic absences flagged with ISYSAB = 1
C
C       Find the zone a reflection falls into, and return the
C       appropriate value for the reflection multiplicity factor.
C       each reflection must have a zone.
C
C---- LOGICAL FUNCTION HKLEQ(IH,KH)
C       Returns true if indices ih = kh
C
C---- SUBROUTINE SYSAB(IN,ISYSAB)
C        Input IN(3) - reflection indices
C        Returns  ISYSAB flag.
C       Systematic absences flagged with ISYSAB = 1
C       Only reflns with EPSI gt 1 need be considered
C
C
C Group 3: Subroutines for choosing asymmetric units 
C===================================================
C
C Group 3a: Subroutines for choosing asymmetric units for reflection data:
C-------------------------------------------------------------------------
C
c       ASUSET  set up symmetry for ASUPUT & ASUGET, print it
c       ASUPUT  put reflection into asymmetric unit defined in ASUSET
c       ASUGET  recover original indices, ie reverse of ASUPUT
c       ASUPHP  change phase for symmetry related reflection
c  Internal routines:-
c       PRTRSM  print reciprocal space symmetry, called by ASUSET
c       INASU   (function) test if reflection is in asymmetric unit
c               called by ASUPUT
C
C----   SUBROUTINE ASUSET(
C    .     SPGNAM,NUMSGP,PGNAME,MSYM,RRSYM,MSYMP,MLAUE,LPRINT)
C
C  Set up & store symmetry informtion for later use in ASUPUT or ASUGET
C
C  On input:
C    SPGNAM  space-group name (not used) ( character)
C    NUMSGP  space-group number (not used)
C    PGNAME  point-group name (if returned from SYMOP.LIB) ( character)
C    MSYM    total number of symmetry operations
C    RRSYM(4,4,MSYM) symmetry matrices (real-space)
C    LPRINT  - printing flag. ( logical)
C
C  On output:
C    PGNAME  point-group name ( character)
C    MSYMP   number of primitive symmetry operations
C    MLAUE   Laue group number - See PGNLAU for details
C
C---- SUBROUTINE ASUPUT(IHKL,JHKL,ISYM)
C
C Put reflection into asymmetric unit defined by call to ASUSET
C
C On input:
C    IHKL(3)    input indices hkl
C
C On output:
C    JHKL(3)    output indices hkl
C    ISYM       symmetry number for output
C                 odd  numbers are for I+
C                 even numbers are for I-
C               real-space symmetry operation number L = (ISYM-1)/2 + 1
C
C  The real-space symmetry matrices are applied by premultiplying them
C  by a row vector hkl,  ie  (h'k'l') = (hkl)R
C
C--- SUBROUTINE ASUGET(IHKL,JHKL,ISYM)
C
C Get original indices of reflection from  asymmetric unit,
C   ie reverse operation of ASUPUT
C   symmetry defined by call to ASUSET
C
C On input:
C    IHKL(3)    input unique indices hkl
C    ISYM       symmetry number for output
C                 odd  numbers are for I+
C                 even numbers are for I-
C               real-space symmetry operation number L = (ISYM-1)/2 + 1
C
C On output:
C    JHKL(3)    output original indices hkl
C
C  The real-space symmetry matrices are applied in ASUPUT by
C premultiplying them by a row vector hkl,  ie  (h'k'l') = (hkl)R
C  So here we calculate (hkl) = (h'k'l') R**-1

C---- SUBROUTINE ASUPHP(JHKL,LSYM,ISIGN,PHASIN,PHSOUT)
C
C---- Generate phase of symmetry equivalent JHKL from that of IHKL
C
C     On input:
C
C    JHKL(3)    indices hkl generated in ASUPUT
C    LSYM       symmetry number for generating JHKL ( found by ASUPUT)
C    ISIGN         =  1   for I+
C                  = -1   for I-
C    PHASIN     phase for reflection IHKL(3)
C
C     On output:
C
C    PHSOUT     phase for reflection JHKL(3)
C
C    Internal cals to:
C
C-1-- INTEGER FUNCTION INASU(IH, NLAUE)
C
C  Arguments:                                               
C   NLAUE - code number for this pointgroup
C   IH(3) - indices
C
C Returns:
C   INASU = +1  if  h k l chosen
C   INASU = -1  if -h-k-l chosen
C   INASU =  0   if reflection is out-of-bounds

C-2-- SUBROUTINE PRTRSM(PGNAME, NSYMP, RSYMIV)
C
C Print reciprocal space symmetry operations
C
C  The real-space symmetry matrices are applied by premultiplying them
C  by a row vector hkl,  ie  (h'k'l') = (hkl)R
C
C
C Group 3b: Subroutines for choosing asymmetric units in real space
C           consistent with FFT expectations; FFT grids etc.
C------------------------------------------------------------------
C
C---- SUBROUTINE SETLIM(LSPGRP,XYZLIM)
C
C Set appropriate box (asymmetric unit) for spacegroup (true spacegroup)
C     LSPGRP. For cubic symmetry spacegroups, this will be more than
C     one asymmetric unit
C
C On entry:
C     lspgrp    true spacegroup (not FFT spacegroup)
C
C On exit
C     xyzlim(2,3)  minimum, maximum limits on x,y,z (fractions of cell)
C                  if spacegroup not recognized, returns xzylim(1,1) = -1.0
C                  Note that the minimum limits (xyzlim(1,)) will always
C                   = 0.0
C
C
C---- SUBROUTINE SETGRD(NLAUE,SAMPLE,NXMIN,NYMIN,NZMIN,NX,NY,NZ)
C
C Set up a suitable sampling grid for FFT
C
C Input:
C     NLAUE         Laue-group for FFT/SF calculation
C     SAMPLE        default fineness of sample, ie if = 1.0 (minimum),
C                   try to get sampling as close to minimum as possible
C                   Typically = 1.5 to get sample at traditional
C                   3 * maximum index
C     NXMIN NYMIN NZMIN minimum sampling (true XYZ)
C
C Output:
C     NX,NY,NZ       sampling intervals along X,Y,Z
C
C  The sampling intervals must satisfying the following conditions:
C
C     1) approximately SAMPLE * minimum sampling
C     2) no prime factor .gt. 19
C     3) special restrictions for particular space-groups
C
C  Subsidiary calls:
C     SUBROUTINE FNDSMP(MINSMP, NMUL, SAMPLE, NSAMPL)
C
C----  Find suitable grid sample, approximately = SAMPLE/2 * maximum index,
C     with required factor, & no prime factor .gt. 19
C
C  On entry:
C     MINSMP     minimum sample, approximately 2 * maximum index
C     NMUL       required factor
C     SAMPLE     desired sample factor, ie if = 1.0 (minimum), try to
C                get sample close to MINSMP
C
C  On exit:
C     nsampl     grid sample
C                if MINSMP<=0, nsampl=nmul
C
C     LOGICAL FUNCTION FACTRZ(N)
C
C---- Returns true if N has all prime factors .le. 19
C
C
C Group 4: Subroutines for testing coordinate data
C======================================================================
C
C      SUBROUTINE CALC_ORIG_PS(NAMSPG_CIF,NSYM,RSYM,NORIG,ORIG,
C     +                         LPAXISX,LPAXISY,LPAXISZ)
C
C -P- CALC_ORIG_PS - creates list of equivalent origins for the named spacegroup.
C
C                  ARGUMENTS
C                  ---------
C                    (I) NAMSPG_CIF - spacegroup name (character)
C                    (I) NSYM - number of symmetry operations
C                    (I) RSYM(4,4,NSYM) - symmetry ops stored as 4x4 matrices
C                    (O) NORIG - number of origins.
C                    (O) ORIG(3,i) - vector of alternate origin 
C                               (for example : 0.5,0.0,0.5)
C                               only positive components.
C                               include vector: (0,0,0)
C                    (O) LPAXISX - logical; set true if s/grp is polar along x axis
C                    (O) LPAXISY - logical; set true if s/grp is polar along y axis
C                    (O) LPAXISZ - logical; set true if s/grp is polar along z axis
C
C
C    Taken from Alexei Vagin
C
C---- SUBROUTINE XSPECIALS(NSYM,RSYM,XF,YF,ZF,NSPEC)
C        Input NSYM  - number of symmetry operators. ( integer)
C        Input RSYM - 4*4*NSYM symmetry matrices. ( real)
C        Input XF YF ZF - a coordinate in fractional coordinates.
C        Output NSPEC - the multiplicity of the coordinate.
C                       eg: NSPEC = 3 for an atom on a 3fod axis.
C
C---- This subroutine finds what coordinates occupy special positions
C     ie have occupancies less than 1.0
C     from consideration of the Symmetry Operations.
C
C---- INTEGER FUNCTION KROT(NS)
C       Apply ns'th symmetry operation to jp to get lp,
C       check if lies in asymmetric unit given by nau
C
C       Returns KROT=0  correct operation
C                   =1  if not
C
C Group 5: Subroutines for permuting symmetry operators
C======================================================================
C
C---- ROTFIX PRMVCI PRMVCR
C
C        Three subroutines for permuting symmetry operations
C        They do not really belong here but they are widely used
C        invisibly in FFT routines using symmetry operations to
C        permute axes for easier fast fourier calculations.
C
C---- SUBROUTINE ROTFIX
C                Permutes inverse symmetry operations
C                Matrices passed in Common block ATSYM
C
C---- SUBROUTINE PRMVCI(PERM,JV,N,N1)
C          Input PERM - 4*4 matrix  (real)
C          Input JV   - N1*3 matrix (integer)
C          Output JV  - N1*3 matrix (integer) 
C                       This has been modified by permuting the  
C                       Nth column by matrix PERM.
C           Here is the code for clarity.
C---- Permute
C
C     DO 10 I = 1,3
C       BV(I) = PERM(I,1)*JV(N,1) + PERM(I,2)*JV(N,2) +
C    +          PERM(I,3)*JV(N,3)
C  10 CONTINUE
C
C---- Copy back
C
C     DO 20 I = 1,3
C       JV(N,I) = NINT(BV(I))
C  20 CONTINUE
C
C---- SUBROUTINE PRMVCR(PERM,AV,N,N1)
C          Input PERM - 4*4 matrix  (real)
C          Input AV   - N1*3 matrix (real)
C          Output AV  - N1*3 matrix (real)
C                       This has been modified by permuting the  
C                       Nth column by matrix PERM.
C      See PRMVCI - real equivalent.
C
C
C Group 7: Subroutines for generating and accessing a hash table:
C======================================================================
C
C---- CCP4_HASH_SETUP CCP4_HASH_LOOKUP CCP4_HASH_ZEROIT
C
C         Routines and functions used to initialise, set up and access
C         an internal look-up table. Not clear why these routines are
C         here in particular.
C
C---- SUBROUTINE CCP4_HASH_SETUP(NSER,NFIND)
C
C---- This subroutine sets up a value for the function ccp4_hash_lookup
C     when ccp4_hash_lookup(nser) is later evaluated it will return nfind
C     this function will allow the efficient retrieval of an identifier
C     for a large range variable (such as a crystal number).  the values
C     of the function ccp4_hash_lookup(nser) are stored in the array
C     it(2, kpri) where kpri is the prime number used to generate the
C     function.
C     The array it  lives in the common look which is shared by
C     ccp4_hash_setup and the function ccp4_hash_lookup
C
C     NOTES: A hash table is a way of storing information so that it
C     easily be retrieved without the need for indexing or long searches.
C     NSER is referred to as the "key", which is "hashed" (computer-
C     science speak for "messed up") by the hashing function (in this
C     case MOD(NSER4,KPRI) + 1) to determine where the value pair will
C     be stored. The function LOOKUP can then search on the same basis
C     when supplied with the key, to retreive the pair in (at most) 3
C     calculations. Note that KPRI (the table size) MUST BE A PRIME in
C     order for this method to work.
C
C     IT(1, NDX) = NSER,  IT(2, NDX) = NFIND
C
C---- INTEGER FUNCTION CCP4_HASH_LOOKUP(NSER)
C
C---- The function ccp4_hash_lookup returns the value nfind (which was
C     input when setting up the function in the subroutine ccp4_hash_setup)
C     for the large range variable nser.  Uses hashing. (see comments for
C     CCP4_HASH_SETUP for description of hashing method).
C
C---- SUBROUTINE CCP4_HASH_ZEROIT()
C
C     Initialises elements of array it used in ccp4_hash_setup and
C     ccp4_hash_lookup to zero.
C
C Group 7: Miscellaneous subroutines
C======================================================================
C
C---- SETRSL STHLSQ STS3R4
C
C         Routines and functions used to obtain values of
C         (sin theta/lamba)**2.0. Not clear why these routines are
C         here in particular.
C
C---- SUBROUTINE SETRSL(A,B,C,ALPHA,BETA,GAMMA)
C
C---- Routine to calculate coefficients for (sin(theta)/lambda)**2 from
C     h,k,l for general axes
C     first calculated the components of input axes in an orthonormal
C     basis, then calculate components of reciprocal axes in same basis
C
C    Input angles are in degrees
C
C---- REAL FUNCTION STHLSQ(IH,IK,IL)
C
C      Calculate (sin(theta)/lambda)**2 from h,k,l; coef's set by call to
C        SETRSL : good for any kind of axes
C
C---- REAL FUNCTION STS3R4(IH,IK,IL)
C
C      calculate (sin(theta)/lambda)**2 from h,k,l; coef's set by call to
C        setrsl : good for any kind of axes
C
C
C---- SUBROUTINE PSTOPH (PSIX,PSIY,PSIZ,PHIX,PHIY,PHIZ,AVPHI)
C
C***   *****  PSTOPH  *****
C***   Convert PSIX,PSIY,PSIZ (= epsx,epsy,epsz) to PHIX,PHIY,PHIZ ,
C***    using AVPHI
C      All angles in radians
C
C
C  End of Brief Description.
C +++++++++++++++++++++++++
C_END_OUTLINE

      END

C_BEGIN_CENTR
C     =======================
      SUBROUTINE CENTR(HKL,IC)
C     =======================
C
C---- Determine whether a reflection is centric (return ic=1)
C     or not (ic=0).  If none of the zone tests is satisfied,
C     the reflection is non-centric.  CENTRIC must be called before this
C     to set up the symmetry elements.
C
C     Arguments:
C     IC (O) INTEGER
C       Centricity flag
C     HKL(3) (I) INTEGER
C       Miller indices
C
C_END_CENTR
C
C     .. Scalar Arguments ..
      INTEGER IC, NSM, IPRINT
C     ..
C     .. Array Arguments ..
      INTEGER HKL(3)
      REAL RSM(4,4,*)
C     ..
C     ..
C     .. Local Scalars ..
      INTEGER I, NCENT, IH, IK, IL, J, NC, ISYM, ICENT
      REAL RR,SS,PIDEG,PI2,CONV
      LOGICAL SETUP
      CHARACTER STROUT*400
C     ..
C     .. Local Arrays ..
      REAL CPROJ(3,20),CPRJ(3,12),TRANSCEN(3,12)
      INTEGER IHKL(3,12),IN(3)
      CHARACTER REFTYP(12)*7
C     ..
C     .. Save statement ..
      SAVE CPROJ, NCENT, SETUP, TRANSCEN
C     ..
C     .. External Subroutines ..
      EXTERNAL PUTLIN
C     ..
C     .. Data statements ..
C---- set up tests for 0kl h0l hk0 hhl hkh hkk h,-hl hk-h hk-k
C      -h 2h l   2h -h l  hkl
C
      DATA REFTYP/'0kl','h0l','hk0','hhl','hkh','hkk','h -hl',' hk-h',
     +     ' hk-k','-h 2h l','2h -h l','hkl'/
      DATA IHKL/0,1,2, 1,0,2, 1,2,0, 1,1,10, 1,10,1, 10,1,1, 1,-1,10,
     +          1,10,-1, 10,1,-1, -1,2,10, 2,-1,10, 1,4,8/
      DATA CPRJ/1.0,0.0,0.0, 0.0,1.0,0.0, 0.0,0.0,1.0, 1.0,-1.0,0.0,
     +     1.0,0.0,-1.0, 0.0,1.0,-1.0, 1.0,1.0,0.0, 1.0,0.0,1.0,
     +     0.0,1.0,1.0, 2.0,1.0,0.0, 1.0,2.0,0.0, 0.0,0.0,0.0/
      DATA SETUP /.FALSE./
C     ..
C
      IF (.NOT.SETUP) CALL CCPERR (1, 'CENTR: CENTRIC not called first')
      IC = 0
      IF (NCENT.NE.0) THEN
        DO 10 I = 1,NCENT
          IF ((CPROJ(1,I)*HKL(1)+CPROJ(2,I)*HKL(2)+CPROJ(3,I)*HKL(3))
     +        .EQ. 0.0) GO TO 20
   10   CONTINUE
        RETURN
   20   IC = 1
      END IF
      RETURN

C     ============================
      ENTRY CENTPHASE(HKL,CENPHS)
c     ============================
C     Return CENPHS - the centric phase should be CENPHS or CENPHS + pi

      IF (.NOT.SETUP) 
     +       CALL CCPERR(1,'CENTPHASE: CENTRIC not called first')
      PIDEG = 180.0

      IF (NCENT.GE.0) THEN
        DO 60 I = 1,NCENT
          ICENT = I
          IF ((CPROJ(1,I)*HKL(1)+CPROJ(2,I)*HKL(2)+CPROJ(3,I)*HKL(3))
     +        .EQ. 0.0) GO TO 70
   60   CONTINUE
        CALL CCPERR(1,'CENTPHASE: This is not a centric reflection!')
        RETURN
   70   RR = hkl(1)*TRANSCEN(1,ICENT)+hkl(2)*TRANSCEN(2,ICENT)
     +                        +hkl(3)*TRANSCEN(3,ICENT)
        SS = REAL(INT(RR))
        IF (RR.GE.SS) THEN
          CENPHS = PIDEG * (RR - SS)
        ELSE
          CENPHS = PIDEG * (RR - SS + 1.0)
        ENDIF
      ELSE
        CALL CCPERR(1,'CENTPHASE: There are no centric zones defined!')
      END IF

      RETURN
C
C^L
C     ============================
      ENTRY CENTPHS(HKL,NSM,RSM,CENPHS,IPRINT)
c     ============================
      PI2 = 8*ATAN2(1.0,1.0)
      CONV = 360.0/PI2
C   Choose any 3 coordinates which are not rational and generate the structure factor       
       Xtest= sqrt(2.0)
       Ytest= sqrt(3.0)
       Ztest= sqrt(5.0)
       Asf = 0.0
       Bsf = 0.0
        DO 25 J = 1,NSM
          Xs = RSM(1,1,J)*Xtest + RSM(1,2,J)*Ytest
     +       + RSM(1,3,J)*Ztest + RSM(1,4,J)
          Ys = RSM(2,1,J)*Xtest + RSM(2,2,J)*Ytest
     +       + RSM(2,3,J)*Ztest + RSM(2,4,J)
          Zs = RSM(3,1,J)*Xtest + RSM(3,2,J)*Ytest
     +       + RSM(3,3,J)*Ztest + RSM(3,4,J)
       Asf = Asf + cos(PI2*(HKL(1)*Xs +HKL(2)*Ys +HKL(3)*Zs))
       Bsf = Bsf + sin(PI2*(HKL(1)*Xs +HKL(2)*Ys +HKL(3)*Zs))
   25   CONTINUE
      CENPHS = MOD( (ATAN2(Bsf,Asf)*CONV  + 360.0),180.0)
      IF(IPRINT.NE.0) THEN
        WRITE(6,'(A,3I4,F8.1,A,F8.1)')
     +  ' Centric phase possibilities',HKL,CENPHS,
     +  '    or ',CENPHS-180.0
      END IF
      RETURN

C     ============================
      ENTRY CENTRIC(NSM,RSM,IPRINT)
C     ============================
C_BEGIN_CENTRIC
C
C     ==================================
C     SUBROUTINE CENTRIC(NSM,RSM,IPRINT)
C     ==================================
C
C---- This is Randy Read's method of defining centric reflections.
C       It uses NSM and the symmetry operators stored in RSMT(4,4,NSM)
C
C---- set up tests for 0kl h0l hk0 hhl hkh hkk h,-hl hk-h hk-k
C      -h 2h l   2h -h l  hkl
C     zones are encoded using an idea from a program by bricogne.
C     if h*zone(1) + k*zone(2) + l*zone(3) is equal to 0.0,
C     that reflection is in that zone.  all that is needed is the
C     most general conditions--a reflection is either centric or
C     not.  some care is warranted in setting up these cards.  for
C     example, for the zone 0 k 0, the most obvious test would be
C     1 0 1.  however, if l can take negative values, then
C     reflections of the type h k -h could incorrectly satisfy
C     the test. if the maximum h is < 100, then 1 0 100 would work.
C     so it is necessary to think, for each test, about what
C     other reflections in the data set could spuriously satisfy
C     the test.
C_END_CENTRIC
C
      SETUP = .TRUE.
      NCENT = 0
C
      DO 30 NC = 1,12
        IN(1) = IHKL(1,NC)
        IN(2) = IHKL(2,NC)
        IN(3) = IHKL(3,NC)
C
C---- Generate symm equivs
C
C---- test whether h' k' l' equals -h -k -l
C
        DO 40 J = 1,NSM
          ISYM = J
          IH = IN(1)*RSM(1,1,J) + IN(2)*RSM(2,1,J) + IN(3)*RSM(3,1,J)
          IF (IH.EQ.-IN(1)) THEN
            IK = IN(1)*RSM(1,2,J) + IN(2)*RSM(2,2,J) +
     +           IN(3)*RSM(3,2,J)
            IF (IK.EQ.-IN(2)) THEN
              IL = IN(1)*RSM(1,3,J) + IN(2)*RSM(2,3,J) +
     +             IN(3)*RSM(3,3,J)
              IF (IL.EQ.-IN(3)) GO TO 50
            END IF
          END IF
   40   CONTINUE
C
C---- next symm opn
C
        GO TO 30
   50   NCENT = NCENT + 1
        CPROJ(1,NCENT) = CPRJ(1,NC)
        CPROJ(2,NCENT) = CPRJ(2,NC)
        CPROJ(3,NCENT) = CPRJ(3,NC)
        TRANSCEN(1,NCENT) = RSM(1,4,ISYM)
        TRANSCEN(2,NCENT) = RSM(2,4,ISYM)
        TRANSCEN(3,NCENT) = RSM(3,4,ISYM)
C
        IF(IPRINT.GE.1) THEN
        WRITE (STROUT,FMT=6000) NCENT,REFTYP(NC)
 6000 FORMAT ('  Centric Zone ',I3,' Reflections of Type  ',A7)
        CALL PUTLIN(STROUT,'CURWIN')
        END IF
   30 CONTINUE
C
       END
C
C     ========================
C_BEGIN_DETERM
      SUBROUTINE DETERM(DET,A)
C     ========================
C
C---- Parameters
C     ==========
C
C          A (I)     4*4 matrix to be inverted
C          DET       Determinant of A
C_END_DETERM
C
C---- Get cofactors of 'a' in array 'c'
C
C     .. Scalar Arguments ..
      REAL DET
C     ..
C     .. Array Arguments ..
      REAL A(4,4)
C     ..
C     .. Local Scalars ..
      REAL AM,D
      INTEGER I,I1,II,J,J1,JJ
C     ..
C     .. Local Arrays ..
      REAL C(4,4),X(3,3)
C     ..
C
      DO 40 II = 1,4
        DO 30 JJ = 1,4
          I = 0
C
          DO 20 I1 = 1,4
            IF (I1.NE.II) THEN
              I = I + 1
              J = 0
              DO 10 J1 = 1,4
C
                IF (J1.NE.JJ) THEN
                  J = J + 1
                  X(I,J) = A(I1,J1)
                END IF
   10         CONTINUE
            END IF
   20     CONTINUE
C
          AM = X(1,1)*X(2,2)*X(3,3) - X(1,1)*X(2,3)*X(3,2) +
     +         X(1,2)*X(2,3)*X(3,1) - X(1,2)*X(2,1)*X(3,3) +
     +         X(1,3)*X(2,1)*X(3,2) - X(1,3)*X(2,2)*X(3,1)
          C(II,JJ) = (-1)** (II+JJ)*AM
   30   CONTINUE
   40 CONTINUE
C
C---- Calculate determinant
C
      D = 0
C
      DO 50 I = 1,4
        D = A(I,1)*C(I,1) + D
   50 CONTINUE
C
C---- Get inverse matrix
C
CCC      DO 70 I = 1,4
CCC        DO 60 J = 1,4
CCC          AI(I,J) = C(J,I)/D
CCC   60   CONTINUE
CCC   70 CONTINUE
C
      DET = D
C
      END
C
C     ====================================
      SUBROUTINE EPSLN(NSM,NSMP,RSM,IPRINT)
C     =====================================
C
C---- It works out the epsilon cards from consideration of the Symmetry
C      and a set of standard reflections.
C
C    This is Randys program description
C
C     read number of epsilon zones (see rollett p. 126)
C     then epsilon cards. zones defined as for centric zones, but
C     fourth number on each line is the multiplicity corresponding
C     to this zone.  last card should always be 0 0 0 n, where n is
C     appropriate for the lattice (1 for primitive, 2 for face-
C     centred, etc.), so that general reflections get a value
C     for epsilon. be very careful of the order of the zones. cards
C     for reciprocal lattice rows should be given before those for
C     planes, because the first test that is satisfied defines
C     the zone.
C
C    set up tests for
C     h00 0k0 00l hh0 h0h 0kk h,-h0 h0-h 0k-k -h2h0 2h-h0 hhh hkl
C
C---- SIGMAA commons
C      COMMON /CP/ CPROJ(3,20),NCENT
C      COMMON /EPS/ EPZONE(4,20),NEZONE
C
C   Set up this common block for use when searching for systematic absences
C
C     ..
C     .. Parameters ..
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Common blocks ..
      INTEGER NSMT
      REAL RSMM,RSMTT
      COMMON /SYSABS/NSMT,RSMM(4,4,MAXSYM),RSMTT(4,4,MAXSYM)
C
C     .. Scalar Arguments ..
      INTEGER NSM,NSMP,IPRINT
C     ..
C     .. Array Arguments ..
      REAL RSMT(4,4,MAXSYM)
      REAL RSM(4,4,*)
C     ..
C     .. Scalars in Common ..
      INTEGER NEZONE
C     ..
C     .. Arrays in Common ..
      REAL EPZONE
C     ..
C     .. Local Scalars ..
      INTEGER IH,IK,IL,J,NC,NEPS,LATMUL,N,I
      CHARACTER STROUT*400
C     ..
C     .. Local Arrays ..
      REAL EPZNE(3,13)
      INTEGER IHKL(3,13),IN(3)
      CHARACTER REFTYP(13)*7
C     ..
C     .. External Subroutines ..
      EXTERNAL PUTLIN,LERROR,INVSYM
C     ..
C     .. Common blocks ..
      COMMON /EPS/EPZONE(4,20),NEZONE
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
C
C---- Example h k l for testing possible epsln zones
C
      DATA REFTYP/'h00','0k0','00l','hh0','h0h','0kk','h -h0',' h0-h',
     +     ' 0k-k','-h 2h 0','2h -h 0','hhh','hkl'/
      DATA IHKL/1,0,0, 0,2,0, 0,0,2, 1,1,0, 1,0,1, 0,1,1, 1,-1,0,
     +         1,0,-1, 0,1,-1, -1,2,0, 2,-1,0, 1,1,1, 1,2,3/
       DATA EPZNE/
     +       0.0,500.0,  1.0,   1.0,  0.0,500.0,     1.0,500.0,   0.0,
     +       1.0, -1.0,500.0,   1.0,500.0, -1.0,   500.0,  1.0,  -1.0,
     +       1.0,  1.0,500.0,   1.0,500.0,  1.0,   500.0,  1.0,   1.0,
     +       2.0,  1.0,500.0,   1.0,  2.0,500.0,     1.0,500.0,-501.0, 
     +       0.0,  0.0,  0.0/
C     ..
C
C---- Now works for all symms. Theory: specify 2 vectors perpendicular
C---- to a specific epsilon zone. If the dot product with both of these
C---- vectors is 0 then we are in that epsilon zone.
C---- To save time we combine both vectors into 1 with a+500b.
C---- e.g. for h00 a=(0 0 1) b=(0 1 0), for hhh a=(1 0 -1) b=(0 1 -1)
C---- KDC 29/6/94
C
      LATMUL = NSM/NSMP
C
      IF(IPRINT.GT.0)THEN
C
C          ****************
      CALL PUTLIN(' ******   EPSILON ZONES -  Reflection Classes'//
     +       ' and their multiplicity ****** ','CURWIN')
C          ****************
         END IF
C
      NEZONE = 0
      NEPS = 1
      IF (NSM.NE.1) THEN
C
        DO 20 NC = 1,12
          IN(1) = IHKL(1,NC)
          IN(2) = IHKL(2,NC)
          IN(3) = IHKL(3,NC)
          NEPS = 1
C
C---- Generate symm equivs
C
C---- test whether h' k' l' equals h k l
C
          DO 10 J = 2,NSMP
            IH = IN(1)*RSM(1,1,J) + IN(2)*RSM(2,1,J) +
     +           IN(3)*RSM(3,1,J)
            IF (IH.EQ.IN(1)) THEN
              IK = IN(1)*RSM(1,2,J) + IN(2)*RSM(2,2,J) +
     +             IN(3)*RSM(3,2,J)
              IF (IK.EQ.IN(2)) THEN
                IL = IN(1)*RSM(1,3,J) + IN(2)*RSM(2,3,J) +
     +               IN(3)*RSM(3,3,J)
C
C---- next symm opn
C
                IF (IL.EQ.IN(3)) NEPS = NEPS + 1
              END IF
            END IF
   10     CONTINUE
C
          IF (NEPS.NE.1) THEN
            NEZONE = NEZONE + 1
            EPZONE(4,NEZONE) = NEPS*LATMUL
            EPZONE(1,NEZONE) = EPZNE(1,NC)
            EPZONE(2,NEZONE) = EPZNE(2,NC)
            EPZONE(3,NEZONE) = EPZNE(3,NC)
C
            IF(IPRINT.GT.0)THEN
            WRITE (STROUT,FMT=6000) NEZONE
            CALL PUTLIN(STROUT,'CURWIN')
            WRITE (STROUT,FMT=6010) REFTYP(NC)
            CALL PUTLIN(STROUT,'CURWIN')
            WRITE (STROUT,FMT=6020)NEPS
            CALL PUTLIN(STROUT,'CURWIN')
            END IF
          END IF
   20   CONTINUE
C
C---- next reflection class
C
      END IF
C
C---- add H K L ezone 0 0 0   1.
C     If hkl already set this will be ignored..
C
      NEZONE = NEZONE + 1
      EPZONE(4,NEZONE) = LATMUL
      EPZONE(1,NEZONE) = EPZNE(1,13)
      EPZONE(2,NEZONE) = EPZNE(2,13)
      EPZONE(3,NEZONE) = EPZNE(3,13)
C
            IF(IPRINT.GT.0)THEN
            WRITE (STROUT,FMT=6000) NEZONE
            CALL PUTLIN(STROUT,'CURWIN')
            WRITE (STROUT,FMT=6010) REFTYP(13)
            CALL PUTLIN(STROUT,'CURWIN')
            WRITE (STROUT,FMT=6020)LATMUL
            CALL PUTLIN(STROUT,'CURWIN')
            END IF
C
      IF ((NEZONE.GT.20) .OR. (NEZONE.LT.1)) THEN
C
        IF (NEZONE.LT.1) THEN
C
C              ****************************
          CALL LERROR(2,-1,
     +         'EPSLN: have to have at least one EPSILON ZONE')
C              ****************************
C
      END IF
      END IF
C
C---- Fill common /sysabs/
C
      NSMT = NSM
C
      DO 80 N = 1,NSM
           CALL INVSYM(RSM(1,1,N),RSMT(1,1,N) )
        DO 70 I = 1,4
          DO 60 J = 1,4
            RSMM(J,I,N) = RSM(J,I,N)
            RSMTT(J,I,N) = RSMT(J,I,N)
   60     CONTINUE
   70   CONTINUE
   80 CONTINUE
C
C---- Format statements
C
 6000 FORMAT (' EPSILON Zone ',I3)
 6010 FORMAT (' Reflections of type ',A7)
 6020 FORMAT (' Multiplicity ',I3)
C
      END
C
C
C     ================================
      SUBROUTINE EPSLON(IH,EPSI,ISYSAB)
C     ================================
C
C---- Find the zone a reflection falls into, and return the
C     appropriate value for the reflection multiplicity factor.
C     each reflection must have a zone.
C     Systematic absences flagged with ISYSAB = 1
C
C     .. Scalar Arguments ..
      REAL EPSI
      INTEGER ISYSAB
C     ..
C     .. Array Arguments ..
      INTEGER IH(3)
C     ..
C     .. Scalars in Common ..
      INTEGER NEZONE
C     ..
C     .. Arrays in Common ..
      REAL EPZONE
C     ..
C     .. Local Scalars ..
      REAL TEST
      INTEGER I,J
      CHARACTER LINERR*200
C     ..
C     .. External Subroutines ..
      EXTERNAL SYSAB,LERROR
C     ..
C     .. Common blocks ..
      COMMON /EPS/EPZONE(4,20),NEZONE
C     ..
C     .. Save statement ..
      SAVE
C     ..
C
      DO 20 I = 1,NEZONE
        TEST = 0.0
C
        DO 10 J = 1,3
          TEST = EPZONE(J,I)*IH(J) + TEST
   10   CONTINUE
C
        IF (TEST.EQ.0.0) GO TO 30
   20 CONTINUE
C
          WRITE (LINERR,FMT='(A,3I5)')
     +     ' NO EPSILON ZONE found for reflection ',IH
C
C              ****************************
          CALL LERROR(2,-1,LINERR)
C              ****************************
C
   30 EPSI = EPZONE(4,I)
      ISYSAB = 0
C
C                    *********************
      IF (EPSI.GT.1) CALL SYSAB(IH,ISYSAB)
C                    *********************
C
      END
C
C
C     =======================
      SUBROUTINE INVSYM(A,AI)
C     =======================
C
C---- subroutine to invert 4*4 matrices for conversion between
C     real space and reciprocal space symmetry operators.
C
C---- Parameters
C     ==========
C
C           A (I)   4*4 matrix to be inverted
C          AI (O)   inverse matrix
C
C---- get cofactors of 'a' in array 'c'
C
C     .. Array Arguments ..
      REAL A(4,4),AI(4,4)
C     ..
C     .. Local Scalars ..
      REAL AM,D
      INTEGER I,I1,II,J,J1,JJ
C     ..
C     .. Local Arrays ..
      REAL C(4,4),X(3,3)
C     ..
C
      DO 40 II = 1,4
        DO 30 JJ = 1,4
          I = 0
          DO 20 I1 = 1,4
            IF (I1.NE.II) THEN
              I = I + 1
              J = 0
              DO 10 J1 = 1,4
                IF (J1.NE.JJ) THEN
                  J = J + 1
                  X(I,J) = A(I1,J1)
                END IF
   10         CONTINUE
            END IF
   20     CONTINUE
C
          AM = X(1,1)*X(2,2)*X(3,3) - X(1,1)*X(2,3)*X(3,2) +
     +         X(1,2)*X(2,3)*X(3,1) - X(1,2)*X(2,1)*X(3,3) +
     +         X(1,3)*X(2,1)*X(3,2) - X(1,3)*X(2,2)*X(3,1)
          C(II,JJ) = (-1)** (II+JJ)*AM
   30   CONTINUE
   40 CONTINUE
C
C---- Calculate determinant
C
      D = 0
C
      DO 50 I = 1,4
        D = A(I,1)*C(I,1) + D
   50 CONTINUE
C
C---- Get inverse matrix
C
      DO 70 I = 1,4
        DO 60 J = 1,4
          AI(I,J) = C(J,I)/D
   60   CONTINUE
   70 CONTINUE
C
      END
C
C
C     ======================================
      SUBROUTINE MSYGET(IST,LSPGRP,NSYM,ROT)
C     ======================================
C
C----  Get symmetry operations for space-group LSPGRP from library
C      file  on stream IST, logical name SYMOP.
C         Returns NSYM = number of symmetry operations
C                 ROT(4,4,NSYM)  rotation/translation  matrices
C
C     .. Scalar Arguments ..
      INTEGER IST,LSPGRP,NSYM
C     ..
C     .. Array Arguments ..
      REAL ROT(4,4,*)
C     ..
C     .. Local Scalars ..
      INTEGER I,IFAIL,ISG,NLIN
      CHARACTER LINE*80,LINERR*200
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPDPN,SYMFR2,LERROR
C     ..
      IFAIL = 0
      CALL CCPDPN(IST,'SYMOP','READONLY','F',0,IFAIL)
      NSYM = 0
C
 10   CONTINUE
C
C---- Find correct space-group in file.
C     Each space-group has header line of space-group number,
C     number of lines of symmetry operations
C
      READ (IST,FMT=*,END=30) ISG,NLIN
      IF (ISG.EQ.LSPGRP) THEN
        GO TO 40
      ELSE
C
C---- Skip NLIN lines
C
        DO 20 I = 1,NLIN
          READ (IST,FMT=*)
 20     CONTINUE
C
C       try again
        GO TO 10
      END IF
C
 30   CONTINUE
      WRITE (LINERR,FMT='(A,A,I5,A)')
     +     ' **MSYGET: No symmetry information for space group ',
     +     ' number',LSPGRP,' in SYMOP file**'
      CALL LERROR(2,-1,LINERR)
C
C---- Space-group found,
C     convert NLIN lines of symmetry operators to matrices
C
 40   CONTINUE
C
      DO 50 I = 1,NLIN
        READ (IST,FMT='(A)') LINE
C       Convert line to matrices
        NSYM = NSYM + 1
        CALL SYMFR2(LINE,1,NSYM,ROT)
 50   CONTINUE
      CLOSE(IST)
C
      END
C
C
C     =========================================================
      SUBROUTINE MSYMLB(IST,LSPGRP,NAMSPG,NAMPG,NSYMP,NSYM,ROT)
C     =========================================================
C
C---- Get symmetry operations for spacegroup LSPGRP from library file
C     on stream IST, logical name SYMOP.
C
C   In the library file, the header for each entry is
C
C      LSPGRP   NLINS   NLINP   NAMSPG  NAMPG
C
C  where  LSPGRP        spacegroup number
C         NLINS         total number of lines of symmetry operators.
C         NLINP         number of LINES of primitive symmetry operators
C         NAMSPG        spacegroup name
C         NAMPG         name of corresponding pointgroup
C
C On entry:
C   IST         stream number to read file
C   LSPGRP      spacegroup number
C   NAMSPG      spacegroup name: this will be used to find the
C                       spacegroup only if LSPGRP = 0
C
C Returns
C   LSPGRP      spacegroup number
C   NAMSPG      spacegroup name
C   NAMPG       pointgroup name
C   NSYMP       number of primitive symmetry operations - only different
C               from NSYM in non-primitive spacegroups
C   NSYM        total number of symmetry operations
C   ROT(4,4,NSYM)  rotation/translation  matrices
C
C     .. Parameters ..
      INTEGER NPARSE
      PARAMETER (NPARSE=200)
C     ..
C     .. Scalar Arguments ..
      INTEGER IST,LSPGRP,NSYM,NSYMP
      CHARACTER NAMPG* (*),NAMSPG* (*)
C     ..
C     .. Array Arguments ..
      REAL ROT(4,4,*)
C     ..
C     .. Local Scalars ..
      INTEGER I,IFAIL,ISG,NLIN,NLINS,NTOK
      CHARACTER LINE*400,LINERR*400
C     ..
C     .. Local Arrays ..
      REAL FVALUE(NPARSE)
      INTEGER IBEG(NPARSE),IDEC(NPARSE),IEND(NPARSE),ITYP(NPARSE)
      CHARACTER CVALUE(NPARSE)*4
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPDPN,CCPUPC,PARSE,SYMFR2,LERROR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
      IFAIL = 0
      CALL CCPDPN(IST,'SYMOP','READONLY','F',0,IFAIL)
C
      NTOK = 0
      NSYM = 0
C
   10 CONTINUE
C
C---- Find correct space-group in file.
C     Each space-group has header line of space-group number,
C     number of line of symmetry operations for non-primitive
C     and primitive cells.
C
      READ (IST,FMT='(A)',ERR=30,END=30) LINE
      CALL CCPUPC(LINE)
      NTOK = -NPARSE
      CALL PARSE(LINE,IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,NTOK)
C
C---- Fields are space group number,
C                number of lines,
C                number of lines in primitive cell symmetry,
C                spacegroup name
C
      IF (ITYP(1).NE.2 .OR. ITYP(2).NE.2 .OR. ITYP(3).NE.2)
     +     CALL LERROR(2,-1,'MSYMLB: Error in format of SYMOP file: '
     +     // LINE)
      ISG = NINT(FVALUE(1))
      NLIN = NINT(FVALUE(2))
      NLINS = NINT(FVALUE(3))
      IF (LSPGRP.GT.0) THEN
C
C---- Check for spacegroup number given
C
        IF (LSPGRP.EQ.ISG) GO TO 40
C
C---- Check for spacegroup name given
C
      ELSE IF (NAMSPG.EQ.LINE(IBEG(4) :IEND(4))) THEN
        GO TO 40
      END IF
C
C---- Not this one, skip NLIN lines
C
      DO 20 I = 1,NLIN
        READ (IST,FMT=*)
 20   CONTINUE
C     try again
      GO TO 10
C
 40   CONTINUE
C
C---- Space-group found, convert NLIN lines of
C     symmetry operators to matrices
C
      LSPGRP = ISG
      NAMSPG = LINE(IBEG(4) :IEND(4))
      NAMPG = LINE(IBEG(5) :IEND(5))
C
      DO 50 I = 1,NLINS
        READ (IST,FMT='(A)') LINE
C       Convert line to matrices
        NSYM = NSYM + 1
        CALL SYMFR2(LINE,1,NSYM,ROT)
 50   CONTINUE
C
      NSYMP = NSYM
      IF (NLIN.GT.NLINS) THEN
        DO 60 I = NLINS + 1,NLIN
          READ (IST,FMT='(A)') LINE
C         Convert line to matrices
          NSYM = NSYM + 1
          CALL SYMFR2(LINE,1,NSYM,ROT)
 60     CONTINUE
      END IF
C
      CLOSE (IST)
      RETURN
C
 30   CONTINUE
      WRITE (LINERR,FMT='(A,A,I5,A)')
     +     'MSYGET: No symmetry information for space group ',
     +     ' number',LSPGRP,' in SYMOP file'
      CALL LERROR(2,-1,LINERR)
      END
C
C
C     =========================================================
      SUBROUTINE MSYMLB2(IST,LSPGRP,NAMSPG_CIF,NAMPG,NSYMP,NSYM,ROT)
C     =========================================================
C
C     Identical to MSYMLB, except that on output NAMSPG_CIF
C     has correct CIF format, e.g. 'P 21 21 21'
C     NAMSPG_CIF should be as in _symmetry.space_group_name_H-M
C
C---- Get symmetry operations for spacegroup LSPGRP from library file
C     on stream IST, logical name SYMOP.
C
C   In the library file, the header for each entry is
C
C      LSPGRP   NLINS   NLINP   NAMSPG  NAMPG  CRYSTAL  NAMSPG_CIF
C
C  where  LSPGRP        spacegroup number
C         NLINS         total number of lines of symmetry operators.
C         NLINP         number of LINES of primitive symmetry operators
C         NAMSPG        spacegroup name
C         NAMPG         name of corresponding pointgroup
C         CRYSTAL       crystal system
C         NAMSPG_CIF    spacegroup name in CIF format
C
C On entry:
C   IST         stream number to read file
C   LSPGRP      spacegroup number
C   NAMSPG_CIF  spacegroup name: this will be used to find the
C                       spacegroup only if LSPGRP = 0
C
C Returns
C   LSPGRP      spacegroup number
C   NAMSPG_CIF  spacegroup name in CIF format
C   NAMPG       pointgroup name
C   NSYMP       number of primitive symmetry operations - only different
C               from NSYM in non-primitive spacegroups
C   NSYM        total number of symmetry operations
C   ROT(4,4,NSYM)  rotation/translation  matrices
C
C     .. Parameters ..
      INTEGER NPARSE
      PARAMETER (NPARSE=200)
C     ..
C     .. Scalar Arguments ..
      INTEGER IST,LSPGRP,NSYM,NSYMP
      CHARACTER NAMPG* (*),NAMSPG_CIF* (*)
C     ..
C     .. Array Arguments ..
      REAL ROT(4,4,*)
C     ..
C     .. Local Scalars ..
      INTEGER I,IFAIL,ISG,NLIN,NLINS,NTOK
      CHARACTER LINE*400,LINERR*400
C     ..
C     .. Local Arrays ..
      REAL FVALUE(NPARSE)
      INTEGER IBEG(NPARSE),IDEC(NPARSE),IEND(NPARSE),ITYP(NPARSE)
      CHARACTER CVALUE(NPARSE)*4
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPDPN,CCPUPC,PARSE,SYMFR2,LERROR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
      IFAIL = 0
      CALL CCPDPN(IST,'SYMOP','READONLY','F',0,IFAIL)
C
      NTOK = 0
      NSYM = 0
C
   10 CONTINUE
C
C---- Find correct space-group in file.
C     Each space-group has header line of space-group number,
C     number of line of symmetry operations for non-primitive
C     and primitive cells.
C
      READ (IST,FMT='(A)',ERR=30,END=30) LINE
      CALL CCPUPC(LINE)
      NTOK = -NPARSE
      CALL PARSE(LINE,IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,NTOK)
C
C---- Fields are space group number,
C                number of lines,
C                number of lines in primitive cell symmetry,
C                spacegroup name
C
      IF (ITYP(1).NE.2.OR.ITYP(2).NE.2.OR.ITYP(3).NE.2.OR.NTOK.LT.7)
     +     CALL LERROR(2,-1,'MSYMLB2: Error in format of SYMOP file: '
     +     // LINE)
      ISG = NINT(FVALUE(1))
      NLIN = NINT(FVALUE(2))
      NLINS = NINT(FVALUE(3))
      IF (LSPGRP.GT.0) THEN
C
C---- Check for spacegroup number given
C
        IF (LSPGRP.EQ.ISG) GO TO 40
C
C---- Check for spacegroup name given against CIF name
C
      ELSE IF (NAMSPG_CIF.EQ.LINE(IBEG(7) :IEND(7))) THEN
        GO TO 40
C
C---- Check for spacegroup name given against short name
C
      ELSE IF (NAMSPG_CIF.EQ.LINE(IBEG(4) :IEND(4))) THEN
        GO TO 40

      END IF
C
C---- Not this one, skip NLIN lines
C
      DO 20 I = 1,NLIN
        READ (IST,FMT=*)
 20   CONTINUE
C     try again
      GO TO 10
C
 40   CONTINUE
C
C---- Space-group found, convert NLIN lines of
C     symmetry operators to matrices
C
      LSPGRP = ISG
      NAMSPG_CIF = LINE(IBEG(7) :IEND(7))
      NAMPG = LINE(IBEG(5) :IEND(5))
C
      DO 50 I = 1,NLINS
        READ (IST,FMT='(A)') LINE
C       Convert line to matrices
        NSYM = NSYM + 1
        CALL SYMFR2(LINE,1,NSYM,ROT)
 50   CONTINUE
C
      NSYMP = NSYM
      IF (NLIN.GT.NLINS) THEN
        DO 60 I = NLINS + 1,NLIN
          READ (IST,FMT='(A)') LINE
C         Convert line to matrices
          NSYM = NSYM + 1
          CALL SYMFR2(LINE,1,NSYM,ROT)
 60     CONTINUE
      END IF
C
      CLOSE (IST)
      RETURN
C
 30   CONTINUE
      WRITE (LINERR,FMT='(A,A,I5,A)')
     +     'MSYMLB2: No symmetry information for space group ',
     +     ' number',LSPGRP,' in SYMOP file'
      CALL LERROR(2,-1,LINERR)
      END
C
C
C     =========================================================
      SUBROUTINE MSYMLB3(IST,LSPGRP,NAMSPG_CIF,NAMSPG_CIFS,
     +                   NAMPG,NSYMP,NSYM,RlSymmMatrx)
C     =========================================================
C
C---- Get symmetry operations for spacegroup LSPGRP from library file
C     on stream IST, logical name SYMOP.
C
C   In the library file, the header for each entry may contain
C   Order not guaranteed, but must start with:
C   LSPGRP   NLINS and contain either NAMSPG or NAMSPG_CIF
C
C      LSPGRP   NLINS   NLINP   NAMSPG  NAMPG CRYSTAL  NAMSPG_CIF
C
C  where  LSPGRP        spacegroup number
C         NLINS         total number of lines of symmetry operators.
C         NLINP         number of LINES of primitive symmetry operators
C  Not used now..
C         NAMSPG_CIF    spacegroup name
C         NAMPG         name of corresponding pointgroup
C  Not used now..
C
C On entry:
C   IST         stream number to read file
C   LSPGRP      spacegroup number
C   NAMSPG or NAMSPG_CIF
C               any acceptable spacegroup name: this will be used to 
C                       identify the spacegroup if possible
C
C Returns
C   LSPGRP      spacegroup number
C   NAMSPG_CIF  full spacegroup name 
C   NAMSPG_CIFS name without any spaces
C   NAMPG       pointgroup name ( obtained from pgdefn - not 100% reliable!)
C   NSYMP       number of primitive symmetry operations obtained from pgdefn- 
C               only different from NSYM in non-primitive spacegroups
C   NSYM        total number of symmetry operations
C   RlSymmMatrx(4,4,NSYM)  Symmetry Rotation/translation  matrices
C
C     .. Parameters ..
      INTEGER NPARSE
      PARAMETER (NPARSE=200)
C     ..
C     .. Scalar Arguments ..
      INTEGER IST,LSPGRP,NSYM,NSYMP
      CHARACTER NAMPG* (*),NAMSPG_CIF* (*),NAMSAV*20,NAMSPG_CIFS*20
C     ..
C     .. Array Arguments ..
      REAL RlSymmMatrx(4,4,*),ROTCHK(4,4)
C     ..
C     .. Local Scalars ..
      INTEGER I,IFAIL,ISG,NLIN,NTOK
      CHARACTER LINE*400,LINERR*400
C      INTEGER NLINS
C     ..
C     .. Local Arrays ..
      REAL FVALUE(NPARSE)
      INTEGER IBEG(NPARSE),IDEC(NPARSE),IEND(NPARSE),ITYP(NPARSE),
     +        IRCHK(192)
      CHARACTER CVALUE(NPARSE)*4
      LOGICAL NAMFIT
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPDPN,CCPUPC,PARSE,SYMFR2,LERROR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
      IFAIL = 0
      CALL CCPDPN(IST,'SYMOP','READONLY','F',0,IFAIL)
C
      NTOK = 0
      NSYM = 0
C  Remove all spaces from SG name
C
      ILEN = LENSTR(NAMSPG_CIF)
         NAMSPG_CIFS = NAMSPG_CIF(1:1)

         IF(ILEN.GE.2) THEN
          J = 1
          DO I = 2,ILEN
           IF( NAMSPG_CIF(I:I).NE.' ') THEN
            NAMSPG_CIFS = NAMSPG_CIFS(1:J)//NAMSPG_CIF(J:J)
            J = J + 1
           END IF
          END DO 
         END IF
C
   10 CONTINUE
C
C---- Find correct space-group in file.
C     Each space-group has header line of space-group number,
C     number of line of symmetry operations for non-primitive
C     and primitive cells.
C
      READ (IST,FMT='(A)',ERR=30,END=30) LINE
      CALL CCPUPC(LINE)
      NTOK = -NPARSE
      CALL PARSE(LINE,IBEG,IEND,ITYP,FVALUE,CVALUE,IDEC,NTOK)
C
C---- Compulsory Fields are space group number,
C                           number of lines,
C                spacegroup name

C
      IF (ITYP(1).NE.2 .OR. ITYP(2).NE.2 )
     +     CALL LERROR(2,-1,'MSYMLB3: Error in format of SYMOP file: '
     +     // LINE)
      ISG = NINT(FVALUE(1))
      NLIN = NINT(FVALUE(2))
c      NLINS = NINT(FVALUE(3))
C
C---- Check for spacegroup name given - may be anywhere on line..
C---- Record the longest name on the line; 
C---- This will be returned as NAMSPG_CIF
C
        NAMLGTH = 1
        NAMSAV = ' '
        NAMFIT = .false.
        DO 15 ITOK = 3,NTOK
C Spacegroup name must begin P A B C F I "H " R
          IF (LINE(IBEG(ITOK):IBEG(ITOK)).NE.'P' .AND.
     +        LINE(IBEG(ITOK):IBEG(ITOK)).NE.'A' .AND.
     +        LINE(IBEG(ITOK):IBEG(ITOK)).NE.'B' .AND.
     +        LINE(IBEG(ITOK):IBEG(ITOK)).NE.'C' .AND.
     +        LINE(IBEG(ITOK):IBEG(ITOK)).NE.'F' .AND.
     +        LINE(IBEG(ITOK):IBEG(ITOK)).NE.'I' .AND.
     +        LINE(IBEG(ITOK):IBEG(ITOK)+1).NE.'H ' .AND.
     +        LINE(IBEG(ITOK):IBEG(ITOK)).NE.'R' ) GO to 15
C  Ooo - get rid of CUBIC  and any PG
         IF(LINE(IBEG(ITOK):IBEG(ITOK)+1).EQ.'CU' .OR.
     +      LINE(IBEG(ITOK):IBEG(ITOK)+1).EQ.'PG' ) GO to 15
C
         LGTHCHK = MAX(NAMLGTH ,(IEND(ITOK)-IBEG(ITOK)+1))
         IF(LGTHCHK .GT. NAMLGTH ) NAMSAV = LINE(IBEG(ITOK) :IEND(ITOK))
         NAMLGTH = LGTHCHK
         IF (NAMSPG_CIF.NE.LINE(IBEG(ITOK):IEND(ITOK)) .AND.
     +       NAMSPG_CIFS.NE.LINE(IBEG(ITOK):IEND(ITOK))) GO TO 15
C   Found a suitable match to space group name
             NAMFIT = .true.
             GO TO 40
  15    CONTINUE
C
C
C---- No name match; check for spacegroup number if given
C
       IF (LSPGRP.GT.0 .AND. LSPGRP.EQ.ISG) GO TO 40
C
C---- Not this one, skip NLIN lines
C
      DO 20 I = 1,NLIN
        READ (IST,FMT=*)
 20   CONTINUE
C     try again
      GO TO 10
C
 40   CONTINUE
C----- Reset space group name to longest on offer
          LSPGRP = ISG
          NAMSPG_CIF = NAMSAV
C
C
C---- Space-group found, convert NLIN lines of
C     symmetry operators to matrices
C
C  read all sym ops at once; PGDEFN will sort out primitive and non primitive
      DO 50 I = 1,NLIN
        READ (IST,FMT='(A)') LINE
C       Convert line to matrices
        NSYM = NSYM + 1
        CALL SYMFR2(LINE,1,NSYM,RlSymmMatrx)
 50   CONTINUE
C
C-----Endeavor to test these sym ops form a closed group
C
      do n = 1,nsym
        irchk(n) = 0
      end do
C
      DO ISYM = 1,NSYM
C   Determinant should be +1 or -1
        CALL DETERM(DET,RlSymmMatrx(1,1,ISYM))
        if(abs(det).lt.0.5) GO TO 25 
       DO JSYM = 1,NSYM
       CALL MATMULNM(4,4,ROTCHK,RlSymmMatrx(1,1,ISYM),
     +                          RlSymmMatrx(1,1,JSYM))
C   Check ROTCHK is also a symop
C---- Check This RSM Matrx for rotation and translation
C
            IGOOD = 0
          DO 90 N = 1,NSym
            DO 95 I = 1,3
              DO 98 J = 1,4
                DCHK = ABS(ROTCHK(I,J) - RlSymmMatrx(I,J,N))
C
C---- This may be needed for translation components; no harm for others.
C
                IF(J.EQ.4)
     +          DCHK = ABS(MOD(ROTCHK(I,J) - RlSymmMatrx(I,J,N)
     +                                                   +99.5,1.0)-0.5)

                IF (DCHK.LT.0.01) GO TO 98
C
C---  This MTZ symm op  no good - off to check the next..
C
                GO TO 90
  98          CONTINUE
  95        CONTINUE
C
C---- Found a good match - now check next ISM
C
            IGOOD = 1
            irchk(n) = irchk(n) + 1
            GO to 80
  90      Continue
C
C
C---- If this symmetry operator is missing no point going on. Try next SG
C
          IF(IGOOD.EQ.0) THEN
            Write(6,'(A,I4,A,I4,A)') '  Symm operators ', ISYM,' and',
     +                               JSYM,' have a problem.'
            GO TO 35
          END IF
  80    CONTINUE
C
C
       END DO 
      END DO 
c
C
C    Check all symmetry operators are equally likely to be generated 
          DO 100 N = 1,NSym
           if(irchk(n) .ne.nsym ) go to 35
 100      continue
C
        call PGDEFN(NAMPG,NSYMP,NSYM,RlSymmMatrx,.FALSE.)
C
      CLOSE (IST)
      RETURN
C
 25   CONTINUE
      WRITE (LINERR,FMT='(A,A,I5,A)')
     +     'MSYLB3: Problem with sym op - determinant ne -+1',
     +     ' space group number',LSPGRP,' in SYMOP file'
      CALL LERROR(2,-1,LINERR)
C
 30   CONTINUE
      WRITE (LINERR,FMT='(A,A,I5,A)')
     +     'MSYLB3: No symmetry information for space group ',
     +     ' number',LSPGRP,' in SYMOP file'
      CALL LERROR(2,-1,LINERR)
C
 35   CONTINUE
      WRITE (LINERR,FMT='(A,A,I5,A)')
     +     'MSYLB3: Symmetry operators are not a closed group',
     +     ' Something wrong for space group number',
     +     LSPGRP,' in SYMOP file'
      CALL LERROR(2,-1,LINERR)
      END
C
C
C     =====================================
      SUBROUTINE PGMDF(JLASS,JCENTR,JSCREW)
C     =====================================
C
C---- Use this subroutine to transfer information
C  If JLASS eq 0   then fill JLASS JCENTR JSCREW from common block.
C  If JLASS gt 0   then fill KLASS ICENTR ISCREW in common block.
C
C     .. Scalar Arguments ..
      INTEGER JCENTR,JLASS
C     ..
C     .. Array Arguments ..
      INTEGER JSCREW(3)
C     ..
C     .. Scalars in Common ..
      INTEGER ICENTR,IFAST,INTER,ISLOW,IVERSN,KLASS,MAXB,MAXR
      CHARACTER STROUT*400
C     ..
C     .. Arrays in Common ..
      REAL CELL
      INTEGER ISCREW
C     ..
C     .. Local Scalars ..
      INTEGER ISCR
C     ..
C     .. Common blocks ..
      COMMON /MDFPAR/MAXR,MAXB,CELL(6),ISLOW,INTER,IFAST,KLASS,ICENTR,
     +     ISCREW(3),IVERSN
      SAVE /MDFPAR/
C     ..
C
      IF (JLASS.EQ.0) THEN
       CALL PUTLIN(' Filling  JLASS JCENTR JSCREW from common block.',
     +      'CURWIN')
        JLASS = KLASS
        JCENTR = ICENTR
        JSCREW(1) = ISCREW(1)
        JSCREW(2) = ISCREW(2)
        JSCREW(3) = ISCREW(3)
      END IF
C
      IF (JLASS.GT.0) THEN
C
C            ****************
        CALL PUTLIN(' Filling  KLASS ICENTR ISCREW in common block.',
     +       'CURWIN')
C            ****************
C
        KLASS = JLASS
        ICENTR = JCENTR
        ISCREW(1) = JSCREW(1)
        ISCREW(2) = JSCREW(2)
        ISCREW(3) = JSCREW(3)
C
C            *****************
        CALL BLANK('CURWIN',2)
        CALL PUTLIN(' **** ICENTR   gives  axis of centering *****',
     +       'CURWIN')
C            *****************
C
        IF (ICENTR.EQ.0) THEN
         STROUT = '  No centering              (P spacegroups)'
        ELSE IF (ICENTR.EQ.1) THEN
         STROUT = '  Centering around a-axis    (A spacegroups)'
        ELSE IF (ICENTR.EQ.2) THEN
         STROUT = '  Centering around b-axis    (B spacegroups)'
        ELSE IF (ICENTR.EQ.3) THEN
         STROUT = '  Centering around c-axis    (C spacegroups)'
        ELSE IF (ICENTR.EQ.4) THEN
         STROUT = '  Centering on all faces     (F spacegroups)'
        ELSE IF (ICENTR.EQ.5) THEN
         STROUT = '  Body centering             (I spacegroups)'
        ELSE IF (ICENTR.EQ.6) THEN
         STROUT = '  Rhombohedral centering     (R spacegroups with'//
     +      '  hexagonal axes)'//
     +      '      (NOTE: R-spacegroups with rhombohedral axes'//
     +      ' have ICENTR = 0!)'
         END IF
          CALL PUTLIN(STROUT,'CURWIN')
C
        ISCR = ISCREW(1) + ISCREW(2) + ISCREW(3)
C
        IF (ISCR.GT.0) WRITE (6,FMT='(//,A)')
     +      ' **** Screw axes are: *****'
        IF (ISCREW(1).GT.0) WRITE (6,FMT='(//,I4,A)') ISCREW(1),
     +      'fold screw axis along A '
        IF (ISCREW(2).GT.0) WRITE (6,FMT='(//,I4,A)') ISCREW(2),
     +      'fold screw axis along B '
        IF (ISCREW(3).GT.0) WRITE (6,FMT='(//,I4,A)') ISCREW(3),
     +      'fold screw axis along C '
C
        WRITE (6,FMT='(//,A)')
     +    ' *** KLASS    : a crystal class name used in MDF files ***'
C
C---- (int. tables)
C
        IF (KLASS.EQ.1) WRITE (6,FMT='(//,A)')
     +      '  TRICLINIC       1_BAR (PG1)       sgs  1 '
        IF (KLASS.EQ.2) WRITE (6,FMT='(//,A)')
     +      '  MONOCLINIC   I  2/M (PG4) B-UNIQUE  sgs  3 -  5'
        IF (KLASS.EQ.3) WRITE (6,FMT='(//,A)')
     +      '  ORTHORHOMBIC    MMM (PG222)         sgs 16 - 24'
        IF (KLASS.EQ.4) WRITE (6,FMT='(//,A)')
     +      '  TETRAGONAL   I  4/M (PG4)          sgs 75 - 80'
        IF (KLASS.EQ.5) WRITE (6,FMT='(//,A)')
     +      '  TETRAGONAL  II  4/MMM (PG422)      sgs 89 - 98 '
        IF (KLASS.EQ.6) WRITE (6,FMT='(//,A)')
     +      '  TRIGONAL     I  3_BAR (PG3)HEXAGONAL AXES sgs  143-146'
        IF (KLASS.EQ.7) WRITE (6,FMT='(//,A)')
     +      '  TRIGONAL    II  3_BAR (??) RHOMBOHEDRAL AXES sgs   146'
        IF (KLASS.EQ.8) WRITE (6,FMT='(//,A)')
     +      ' TRIGONAL   III  3_BAR1M (PG312)       sgs 149,151,153 '
        IF (KLASS.EQ.9) WRITE (6,FMT='(//,A,A)')
     +      ' TRIGONAL    IV  3_BARM1 (PG321)HEXAGONAL AXES ',
     +      ' sgs 150,152,154,155'
        IF (KLASS.EQ.10) WRITE (6,FMT='(//,A)')
     +      '  TRIGONAL     V  3_BARM1 (??)RHOMBOHEDRAL AXES sgs 155'
        IF (KLASS.EQ.11) WRITE (6,FMT='(//,A)')
     +      '  HEXAGONAL    I  6/M  (PG6)   sgs        168 - 173'
        IF (KLASS.EQ.12) WRITE (6,FMT='(//,A)')
     +      '  HEXAGONAL   II  6/MMM (PG622)    sgs 177 - 182'
        IF (KLASS.EQ.13) WRITE (6,FMT='(//,A)')
     +      '  CUBIC        I  M3_BAR (PG23)    sgs 195 - 199'
        IF (KLASS.EQ.14) WRITE (6,FMT='(//,A)')
     +      '  CUBIC       II  M3_BARM (PG432)  sgs 207 - 214'
        IF (KLASS.EQ.15) WRITE (6,FMT='(//,A)')
     +      '  MONOCLINIC  II  2/M (PG2c)  C UNIQUE  sgs 3 -   5'
        IF (KLASS.EQ.16) WRITE (6,FMT='(//,A)')
     +      '  MONOCLINIC III  2/M (PG2a)  A UNIQUE sgs 3 -   5'
C
      END IF
C
      END
C
C
C     ===============================
      SUBROUTINE PRMVCI(PERM,JV,N,N1)
C     ===============================
C
C---- Permute vector JV(N,3) by permutation matrix PERM
C      N1 is first dimension of JV
C
C     .. Scalar Arguments ..
      INTEGER N,N1
C     ..
C     .. Array Arguments ..
      REAL PERM(4,4)
      INTEGER JV(N1,3)
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     .. Local Arrays ..
      REAL BV(3)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT
C     ..
C
C---- Permute
C
      DO 10 I = 1,3
        BV(I) = PERM(I,1)*JV(N,1) + PERM(I,2)*JV(N,2) +
     +          PERM(I,3)*JV(N,3)
   10 CONTINUE
C
C---- Copy back
C
      DO 20 I = 1,3
        JV(N,I) = NINT(BV(I))
   20 CONTINUE
C
      END
C
C
C     ===============================
      SUBROUTINE PRMVCR(PERM,AV,N,N1)
C     ===============================
C
C---- Permute vector AV(N,3) by permutation vector KP
C           N1 is first dimension of AV
C
C     .. Scalar Arguments ..
      INTEGER N,N1
C     ..
C     .. Array Arguments ..
      REAL AV(N1,3),PERM(4,4)
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     .. Local Arrays ..
      REAL BV(3)
C     ..
C
C---- Permute
C
      DO 10 I = 1,3
        BV(I) = PERM(I,1)*AV(N,1) + PERM(I,2)*AV(N,2) +
     +          PERM(I,3)*AV(N,3)
   10 CONTINUE
C
C---- Copy back
C
      DO 20 I = 1,3
        AV(N,I) = BV(I)
   20 CONTINUE
C
      END
C
C
C     =================
      SUBROUTINE ROTFIX
C     =================
C
C     .. Parameters ..
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalars in Common ..
      INTEGER NSYM
C     ..
C     .. Arrays in Common ..
      REAL PERM,ROT,ROTT
      INTEGER JJJNK
C     ..
C     .. Local Scalars ..
      INTEGER I,ISYM,J,JX,JY,JZ
C     ..
C     .. Local Arrays ..
      REAL R1(4,4),R2(4,4)
      CHARACTER NAME(3)*1
C     ..
C     .. Common blocks ..
      COMMON /ATSYM/ROT(4,4,MAXSYM),ROTT(4,4,MAXSYM),NSYM,PERM(4,4),
     +              JJJNK(9)
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
      DATA NAME/'X','Y','Z'/
C     ..
C
      DO 10 I = 1,3
        IF (PERM(1,I).EQ.1.0) JX = I
        IF (PERM(2,I).EQ.1.0) JY = I
        IF (PERM(3,I).EQ.1.0) JZ = I
   10 CONTINUE
C
      WRITE (6,FMT=6000) NAME(JX),NAME(JY),NAME(JZ)
C
      DO 60 ISYM = 1,NSYM
        WRITE (6,FMT=6002) ISYM, ((ROT(I,J,ISYM),J=1,3),I=1,3),
     +    (ROT(J,4,ISYM),J=1,3)
        IF (JX.NE.1 .OR. JY.NE.2) THEN
C
          DO 30 I = 1,4
            DO 20 J = 1,4
              R2(J,I) = ROTT(J,1,ISYM)*PERM(I,1) +
     +                  ROTT(J,2,ISYM)*PERM(I,2) +
     +                  ROTT(J,3,ISYM)*PERM(I,3) +
     +                  ROTT(J,4,ISYM)*PERM(I,4)
              R1(J,I) = PERM(J,1)*ROT(1,I,ISYM) +
     +                  PERM(J,2)*ROT(2,I,ISYM) +
     +                  PERM(J,3)*ROT(3,I,ISYM) +
     +                  PERM(J,4)*ROT(4,I,ISYM)
   20       CONTINUE
   30     CONTINUE
C
          DO 50 J = 1,4
            DO 40 I = 1,4
              ROT(J,I,ISYM) = R1(J,I)
              ROTT(J,I,ISYM) = R2(J,I)
   40       CONTINUE
   50     CONTINUE
C
          WRITE (6,FMT=6004) ISYM, ((ROT(I,J,ISYM),J=1,3),I=1,3),
     +      (ROT(J,4,ISYM),J=1,3)
        END IF
   60 CONTINUE
C
C---- Format statements
C
 6000 FORMAT ('  Input X used as  ',A1,'    Input Y used as  ',A1,
     +        '    Input IZ used as  ',A1)
 6002 FORMAT (' Int Tab Symmetry ',I3,4 (5X,3F6.2))
 6004 FORMAT (' Transformed Symmetry ',I3,4 (5X,3F6.2))
C
      END
C
C
C_BEGIN_SYMFR2
C     =================================
      SUBROUTINE SYMFR2(ICOL,I1,NS,ROT)
C     =================================
C
C---- Read and interpret symmetry operations
C                                    
C     On entry I1 is the first character of ICOL to look at (say after
C     keyword 'SYMM')
C
C NS is the number of the first symmetry operation to be read, & returns
C    with the number of the last one read.
C
C On exit, ROT(4,4,NS) contains the real-space symmetry matrices, in
C                     standard convention, ie
C                     x'(I)=Sum(J=1,3)ROT(I,J,NS)*x(J) + ROT(I,4,NS)
C          ROT(I,4,NS)    contains the fractional translations
C
C_END_SYMFR2
C
C     .. Scalar Arguments ..
      INTEGER I1,NS,I11
      CHARACTER ICOL*(*)
C     ..
C     .. Array Arguments ..
      REAL ROT(4,4,*)
C     ..
C     .. Local Scalars ..
      REAL A,REAL,RECIP,S,T
      INTEGER I,ICOMST,IERR,IFOUND,IMAX,IP,ISL,J,K,NOP,NP,NSYM
      CHARACTER ICH*1, OUTLIN*100
C     ..
C     .. Local Arrays ..
      INTEGER NUM(10)
      CHARACTER INUM(10)*1
C
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. Data statements ..
      DATA NUM/1,2,3,4,5,6,7,8,9,0/
      DATA INUM/'1','2','3','4','5','6','7','8','9','0'/
C     ..
C
      IMAX = LENSTR(ICOL)
      IERR = 0
C
C---- Search for first blank to skip flag sym symtr symmetry
C     or whatever
C
      I11 = I1
CCC      IF (I11.NE.1) THEN
CCC        I11 = 1
CCC        IF (ICOL(I11:I11).EQ.'s' .OR. ICOL(I11:I11).EQ.'S') THEN
CCC   10     CONTINUE
CCC          IF (ICOL(I11:I11).EQ.' ') THEN
CCC            GO TO 20
CCC          ELSE
CCC            I11 = I11 + 1
CCC            IF (I11.LE.IMAX) GO TO 10
CCC          END IF
CCC          CALL CCPERR(1,
CCC     +         'No space between keyword SYM... and first operator')
CCC        END IF
CCC      END IF
C
      I = I11 - 1
      NS = NS - 1
   30 CONTINUE
      NS = NS + 1
      REAL = 0.0
      RECIP = 0.0
      NOP = 1
C
      DO 50 J = 1,4
        DO 40 K = 1,4
          ROT(J,K,NS) = 0.0
   40   CONTINUE
   50 CONTINUE
C
      ROT(4,4,NS) = 1.0
   60 CONTINUE
C
      S = 1.0
C
C---- Set j=4 for translation vector
C
      J = 4
      T = 0.0
      IP = 0
      NP = 0
      ISL = 0
      IFOUND = 0
      ICOMST = 0
   70 CONTINUE
      I = I + 1
C
      IF (I.LE.IMAX) THEN
        ICH = ICOL(I:I)
        IF (ICH.EQ.' ') THEN
          GO TO 70
        ELSE IF (ICH.NE.',' .AND. ICH.NE.'*') THEN
          IFOUND = 1
          IF (ICH.EQ.'X' .OR. ICH.EQ.'x' .OR. ICH.EQ.'H' .OR.
     +        ICH.EQ.'h') THEN
            IF (ICH.EQ.'X' .OR. ICH.EQ.'x') REAL = REAL + 1.0
            IF (ICH.EQ.'H' .OR. ICH.EQ.'h') RECIP = RECIP + 1.0
            J = 1
            IF (T.EQ.0.0) T = S
            GO TO 70
          ELSE IF (ICH.EQ.'Y' .OR. ICH.EQ.'y' .OR. ICH.EQ.'K' .OR.
     +             ICH.EQ.'k') THEN
            IF (ICH.EQ.'Y' .OR. ICH.EQ.'y') REAL = REAL + 1.0
            IF (ICH.EQ.'K' .OR. ICH.EQ.'k') RECIP = RECIP + 1.0
            J = 2
            IF (T.EQ.0.0) T = S
            GO TO 70
          ELSE IF (ICH.EQ.'Z' .OR. ICH.EQ.'z' .OR. ICH.EQ.'L' .OR.
     +             ICH.EQ.'l') THEN
            IF (ICH.EQ.'Z' .OR. ICH.EQ.'z') REAL = REAL + 1.0
            IF (ICH.EQ.'L' .OR. ICH.EQ.'l') RECIP = RECIP + 1.0
            J = 3
            IF (T.EQ.0.0) T = S
            GO TO 70
          ELSE IF (ICH.EQ.'+') THEN
            S = 1.0
            IF (T.EQ.0.0 .AND. J.EQ.4) THEN
              GO TO 70
            ELSE
              GO TO 100
            END IF
          ELSE IF (ICH.EQ.'-') THEN
            S = -1.0
            IF (T.EQ.0.0 .AND. J.EQ.4) THEN
              GO TO 70
            ELSE
              GO TO 100
            END IF
          ELSE IF (ICH.EQ.'/') THEN
            ISL = 1
            GO TO 70
          ELSE IF (ICH.EQ.'.') THEN
            IP = 1
            GO TO 70
          ELSE
            DO 80 K = 1,10
              IF (ICH.EQ.INUM(K)) GO TO 90
   80       CONTINUE
            WRITE (6,FMT=6000)
            WRITE (6,FMT=6002) ICH
            OUTLIN(1:) = ICOL
            WRITE (6,FMT='(1X,A)') OUTLIN(1:LENSTR(OUTLIN))
            IERR = 1
            GO TO 70
   90       A = NUM(K)
            IF (ISL.EQ.1) THEN
              T = T/A
            ELSE IF (IP.EQ.1) THEN
              NP = NP + 1
              T = S*A/10**NP + T
            ELSE
              T = 10.0*T + A*S
            END IF
            GO TO 70
          END IF
        END IF
      END IF
      IF (T.EQ.0.0 .AND. J.EQ.4) THEN
        GO TO 110
      ELSE
        ICOMST = 1
      END IF
  100 ROT(NOP,J,NS) = T
      J = 4
      T = 0.0
      IP = 0
      NP = 0
      ISL = 0
C
      IF (ICOMST.EQ.0) GO TO 70
C
      IF (IFOUND.EQ.0 .AND. I.LE.IMAX) THEN
        IERR = 1
        WRITE (6,FMT=6000)
        WRITE (6,FMT=6006)
        OUTLIN(1:) = ICOL
        WRITE(6,FMT='(1X,A)') OUTLIN(1:LENSTR(OUTLIN))
      END IF
C
      IF (I.LE.IMAX) THEN
        NOP = NOP + 1
        IF (NOP.LE.3) THEN
          GO TO 60
        ELSE
          GO TO 30
        END IF
      ELSE
        GO TO 120
      END IF
  110 WRITE (6,FMT=6000)
      WRITE (6,FMT=6004)
      OUTLIN(1:) = ICOL
      WRITE (6,FMT='(1X,A)') OUTLIN(1:LENSTR(OUTLIN))
      IERR = 1
      GO TO 140
  120 IF (NOP.NE.1 .OR. IFOUND.NE.0) THEN
        IF (NOP.EQ.3 .AND. IFOUND.EQ.1) THEN
          GO TO 130
        ELSE
          IERR = 1
          WRITE (6,FMT=6000)
          WRITE (6,FMT=6008)
          OUTLIN(1:) = ICOL
          WRITE (6,FMT='(1X,A)') OUTLIN(1:LENSTR(OUTLIN))
        END IF
      END IF
      NS = NS - 1
  130 NSYM = NS
      IF (REAL.LT.3.0 .AND. RECIP.LT.3.0) IERR = 1
      IF (RECIP.GE.3.0) NSYM = -NSYM
      IF (IERR.NE.1) RETURN          
  140 WRITE (6,FMT='(A,I4,2F6.1,4(/,4F10.3))') ' NSYM REAL RECIP ROT',
     +  NSYM,REAL,RECIP, ((ROT(I,J,NS),J=1,4),I=1,4)
      CALL CCPERR(1, '**SYMMETRY OPERATOR ERROR**')
C
C---- Format statements
C
 6000 FORMAT (/' **SYMMETRY OPERATOR ERROR**')
 6002 FORMAT (' **INVALID CHARACTER...',A1,' **')
 6004 FORMAT (/' **NO OPERATOR**')
 6006 FORMAT (' **BLANK OPERATOR FIELD**')
 6008 FORMAT (' **LAST GENERAL POSITION IS INCOMPLETE**')
C
      END
C
C
C_BEGIN_SYMFR3
C
C     =======================================
      SUBROUTINE SYMFR3(ICOL,I1,NS,ROT,EFLAG)
C     =======================================
C
C
C---- Read and interpret symmetry operations
C
C---- Arguments :
C
C     ICOL      (I)	CHARACTER*80    Line containing the symmetry ops
C
C     I1        (I)	INTEGER         First character to look at
C                               	(say after keyword 'SYM')
C
C     NS        (I/O)	INTEGER         is the number of the first symmetry
C                               	operation to be read, & returns with the
C                               	number of the last one read (ie you can
C                               	have more than one on a line!)
C
C     ROT       (O)	REAL            Array (4,4,at_least_NS),
C                               	on exit contains the real-space
C                               	symmetry matrices, in standard
C                               	convention, ie
C                                 	[x']    = [s][x]
C                     			x'(I)=Sum(J=1,3)ROT(I,J,NS)*x(J) + ROT(I,4,NS)
C
C                     			ROT(I,4,NS) contains the fractional translations
C
C     EFLAG     (O)	INTEGER         Error flag - on exit,
C                                  	if 0 then OK,
C                                  	gt 0, an error occurred.
C
C_END_SYMFR3
C
C     .. Scalar Arguments ..
      INTEGER EFLAG,I1,NS
      CHARACTER ICOL*80
C     ..
C     .. Array Arguments ..
      REAL ROT(4,4,*)
C     ..
C     .. Local Scalars ..
      REAL A,S,T
      INTEGER I,ICOMST,IERR,IFOUND,IMAX,IP,ISL,J,JDO40,JDO50,JDO80,NOP,
     +        NP
      CHARACTER ICH*1
C     ..
C     .. Local Arrays ..
      INTEGER NUM(10)
      CHARACTER INUM(10)*1
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL BLANK,PUTLIN
C     ..
C     .. Data statements ..
      DATA NUM/1,2,3,4,5,6,7,8,9,0/
      DATA INUM/'1','2','3','4','5','6','7','8','9','0'/
C     ..
C
      IMAX = 80
      IERR = 0
      EFLAG = 0
C
C---- Search for first blank to skip flag sym symtr symmetry
C     or whatever
C
CCC      IF (I1.NE.1) THEN
CCC        I1 = 1
CCC        IF (ICOL(I1:I1).EQ.'s' .OR. ICOL(I1:I1).EQ.'S') THEN
CCC   10     CONTINUE
CCCC
CCC          IF (ICOL(I1:I1).EQ.' ') THEN
CCC            GO TO 20
CCC          ELSE
CCC            I1 = I1 + 1
CCC            IF (I1.LE.80) GO TO 10
CCC          END IF
CCCC
CCCC              ***********************
CCC          CALL PUTLIN(' Error - no space between codeword SYM and ' //
CCC     +         'first operator','ERRWIN')
CCCC              ***********************
CCCC
CCC          EFLAG = EFLAG + 1
CCC          RETURN
CCC        END IF
CCC      END IF
CCCC
CCC 20   CONTINUE
      I = I1 - 1
      NS = NS - 1
   30 CONTINUE
      NS = NS + 1
      NOP = 1
C
      DO 50 JDO50 = 1,4
        DO 40 JDO40 = 1,4
          ROT(JDO50,JDO40,NS) = 0.0
   40   CONTINUE
   50 CONTINUE
C
      ROT(4,4,NS) = 1.0
   60 CONTINUE
      S = 1.0
C
C---- Set j=4 for translation vector
C
      J = 4
      T = 0.0
      IP = 0
      NP = 0
      ISL = 0
      IFOUND = 0
      ICOMST = 0
   70 CONTINUE
      I = I + 1
C
      IF (I.LE.IMAX) THEN
        ICH = ICOL(I:I)
C
        IF (ICH.EQ.' ') THEN
          GO TO 70
        ELSE IF (ICH.NE.',' .AND. ICH.NE.'*') THEN
          IFOUND = 1
C
          IF (ICH.EQ.'X' .OR. ICH.EQ.'x') THEN
            J = 1
            IF (T.EQ.0.0) T = S
            GO TO 70
          ELSE IF (ICH.EQ.'Y' .OR. ICH.EQ.'y') THEN
            J = 2
            IF (T.EQ.0.0) T = S
            GO TO 70
          ELSE IF (ICH.EQ.'Z' .OR. ICH.EQ.'z') THEN
            J = 3
            IF (T.EQ.0.0) T = S
            GO TO 70
          ELSE IF (ICH.EQ.'+') THEN
            S = 1.0
C
            IF (T.EQ.0.0 .AND. J.EQ.4) THEN
              GO TO 70
            ELSE
              GO TO 100
            END IF
C
          ELSE IF (ICH.EQ.'-') THEN
            S = -1.0
C
            IF (T.EQ.0.0 .AND. J.EQ.4) THEN
              GO TO 70
            ELSE
              GO TO 100
            END IF
C
          ELSE IF (ICH.EQ.'/') THEN
            ISL = 1
            GO TO 70
          ELSE IF (ICH.EQ.'.') THEN
            IP = 1
            GO TO 70
          ELSE
C
            DO 80 JDO80 = 1,10
              IF (ICH.EQ.INUM(JDO80)) GO TO 90
   80       CONTINUE
C
C                *********************************
            CALL BLANK('ERRWIN',1)
            CALL PUTLIN(' **Symmetry Operator ERROR**','ERRWIN')
            CALL PUTLIN(' **Invalid Character...' // ICH // ' **',
     +           'ERRWIN')
            CALL PUTLIN(ICOL(1:LENSTR(ICOL)),'ERRWIN')
C                **********************************
C
            EFLAG = EFLAG + 1
            IERR = 1
            GO TO 70
   90       A = NUM(JDO80)
C
            IF (ISL.EQ.1) THEN
              T = T/A
            ELSE IF (IP.EQ.1) THEN
              NP = NP + 1
              T = S*A/10**NP + T
            ELSE
              T = 10.0*T + A*S
            END IF
C
            GO TO 70
          END IF
        END IF
      END IF
C
      IF (T.EQ.0.0 .AND. J.EQ.4) THEN
        GO TO 110
      ELSE
        ICOMST = 1
      END IF
C
  100 ROT(NOP,J,NS) = T
      J = 4
      T = 0.0
      IP = 0
      NP = 0
      ISL = 0
      IF (ICOMST.EQ.0) GO TO 70
C
      IF (IFOUND.EQ.0 .AND. I.LE.IMAX) THEN
C
C            *********************************
        CALL BLANK('ERRWIN',1)
        CALL PUTLIN(' **Symmetry Operator ERROR**','ERRWIN')
        CALL PUTLIN(' **Blank Operator Field**','ERRWIN')
        CALL PUTLIN(ICOL(1:LENSTR(ICOL)),'ERRWIN')
C            ***********************************
C
      END IF
C
      IF (I.LE.IMAX) THEN
        NOP = NOP + 1
C
        IF (NOP.LE.3) THEN
          GO TO 60
        ELSE
          GO TO 30
        END IF
C
      ELSE
        GO TO 120
      END IF
C
C          ***********************************
  110 CALL BLANK('ERRWIN',1)
      CALL PUTLIN('**Symmetry Operator ERROR**','ERRWIN')
      CALL PUTLIN('**No Operator**','ERRWIN')
      CALL PUTLIN(ICOL(1:LENSTR(ICOL)),'ERRWIN')
C          ***********************************
C
      GO TO 140
C
  120 IF (NOP.NE.1 .OR. IFOUND.NE.0) THEN
        IF (NOP.EQ.3 .AND. IFOUND.EQ.1) THEN
          GO TO 130
        ELSE
          IERR = 1
C
C              ********************************
          CALL BLANK('ERRWIN',1)
          CALL PUTLIN('**Symmetry Operator ERROR**','ERRWIN')
          CALL PUTLIN('**Last General Position is INCOMPLETE**',
     +         'ERRWIN')
          CALL PUTLIN(ICOL(1:LENSTR(ICOL)),'ERRWIN')
C              *********************************
C
        END IF
      END IF
      NS = NS - 1
 130  IF (IERR.NE.1) RETURN
C
  140 CALL PUTLIN('**SYMMETRY OPERATOR ERROR**','ERRWIN')
C
      EFLAG = EFLAG + 1
C
      END
C
C
C_BEGIN_SYMTR3
C
C     ========================================
      SUBROUTINE SYMTR3(NSM,RSM,SYMCHS,IPRINT)
C     ========================================
C
C---- SYMTR3(NSM,RSM)
C           symmetry translation from matrix back to characters
C
C           This translates the Symmetry matrices into INT TAB
C           character strings
C
C           It gives the real space operations.
C                eg     X,Y,Z
C                eg     -Y,X-Y, Z
C           That is more complicated than you might think!!
C
C---- Arguments :
C
C     NSM       (I)     INTEGER         Number of Symmetry operations
C
C     RSM       (I)     REAL            Array of dimension (4,4,at least NSM)
C                                       containing symmetry operations on input
C
C     SYMCHS    (O)     CHARACTER*(*)   Array of dimension at least NSM
C                                       containing int tab char strings on output
C
C     IPRINT    (I)     INTEGER         Print flag
C                                       =0 No printing
C                                       =1 Print the int tab strings
C_END_SYMTR3
C
C     .. Scalar Arguments ..
      INTEGER IPRINT,NSM
C     ..
C     .. Array Arguments ..
      REAL RSM(4,4,*)
      CHARACTER SYMCHS(*)*(*)
C     ..
C     .. Local Scalars ..
      INTEGER I1,I2,ICH,IST,ITR,JCOUNT,JDO10,JDO20,JDO30,JDO40
      CHARACTER STROUT*400,SYMCHK*80
C     ..
C     .. Local Arrays ..
      INTEGER NPNTR1(10),NPNTR2(10)
      CHARACTER AXISCR(3)*1,NUMB(9)*1
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL PUTLIN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MIN,NINT
C     ..
C     .. Data statements ..
C
      DATA AXISCR/'X','Y','Z'/
      DATA NUMB/'1','2','3','4','5','6','7','8','9'/
      DATA NPNTR1/0,1,1,1,0,1,0,2,3,5/
      DATA NPNTR2/0,6,4,3,0,2,0,3,4,6/
C     ..
C
      DO 40 JDO40 = 1,NSM
C
C---- Clear symchs
C
        SYMCHS(JDO40) = ' '
        ICH = 1
        SYMCHS(JDO40) (ICH:ICH) = '0'
C
        DO 20 JDO20 = 1,3
C
C---- Ist is flag for first character of operator
C
          IST = 0
C
          DO 10 JDO10 = 1,4
C
            IF (RSM(JDO20,JDO10,JDO40).NE.0) THEN
              IRSM = NINT(ABS(RSM(JDO20,JDO10,JDO40)))
C
              IF (RSM(JDO20,JDO10,JDO40).GT.0.0 .AND. IST.GT.0) THEN
                IF (ICH.GT.LEN(SYMCHS(1)))
     +               CALL CCPERR(1, 'SYMTR3: character array too short')
                SYMCHS(JDO40) (ICH:ICH) = '+'
                ICH = ICH + 1
              END IF
C
              IF (RSM(JDO20,JDO10,JDO40).LT.0) THEN
                IF (ICH.GT.LEN(SYMCHS(1)))
     +               CALL CCPERR(1, 'SYMTR3: character array too short')
                SYMCHS(JDO40) (ICH:ICH) = '-'
                IST = 1
                ICH = ICH + 1
              END IF
C
              IF (JDO10.NE.4) THEN
                IF (ICH.GT.LEN(SYMCHS(1)))
     +               CALL CCPERR(1, 'SYMTR3: character array too short')
                IF(IRSM.NE.1) WRITE(SYMCHS(JDO40) (ICH:ICH+1),'(I1,A1)')
     +                                               IRSM, AXISCR(JDO10)
                IF(IRSM.EQ.1) WRITE(SYMCHS(JDO40) (ICH:ICH+1),'(1X,A1)')
     +                                                     AXISCR(JDO10)
C                SYMCHS(JDO40) (ICH:ICH) = AXISCR(JDO10)
                IST = 1
                ICH = ICH + 2
              END IF
C
              IF (JDO10.EQ.4 .AND. RSM(JDO20,4,JDO40).NE.0) THEN
                ITR = NINT(ABS(RSM(JDO20,4,JDO40)*12.0))
                I1 = NPNTR1(ITR)
                I2 = NPNTR2(ITR)
                IF (ICH+2.GT.LEN(SYMCHS(1)))
     +               CALL CCPERR(1, 'SYMTR3: character array too short')
                SYMCHS(JDO40) (ICH:ICH+2) = NUMB(I1)//'/'//NUMB(I2)
                ICH = ICH + 3
              END IF
            END IF
   10     CONTINUE
C
C---- ADD COMMA  space
C
          IF (JDO20.NE.3) THEN
C Nothing reset into this space - advance counter
            IF( SYMCHS(JDO40) (ICH:ICH) .EQ. '0') ICH = ICH + 1
            IF (ICH+2.GT.LEN(SYMCHS(1)))
     +           CALL CCPERR(1, 'SYMTR3: character array too short')
            SYMCHS(JDO40) (ICH:ICH+2) = ',  '
            ICH = ICH + 3
            SYMCHS(JDO40) (ICH:ICH) = '0'
          END IF
   20   CONTINUE
C
C---- Get rid of spaces after - sign; get rid of first  ;
C----- formats " - X " etc..
C
        LSTR = LENSTR(SYMCHS(JDO40))
          SYMCHK = '                                   '
          ICOUNT = 0
          ISTART = 1
          IF( SYMCHS(JDO40)(1:1) .EQ. ' ') ISTART = 2
          INEG = 0
          DO 22 ISTR = ISTART,LSTR
            IF(INEG .EQ. -1 .AND. 
     +       SYMCHS(JDO40)(ISTR:ISTR) .EQ. ' ') GO TO 22
          
            INEG = 1
            IF(SYMCHS(JDO40)(ISTR:ISTR) .EQ. '-' ) INEG = -1
            ICOUNT = ICOUNT + 1
            SYMCHK(ICOUNT:ICOUNT) = SYMCHS(JDO40)(ISTR:ISTR)
   22     CONTINUE

          SYMCHS(JDO40) = SYMCHK
C
C---- write a message if required
C
        IF (IPRINT.EQ.1) THEN
          WRITE (STROUT,FMT='(A,I3,5X,A)') 'Symmetry',JDO40,
     +      SYMCHS(JDO40) (1:MIN(350,LENSTR(SYMCHS(JDO40))))
C
C              ***********************
          CALL PUTLIN(STROUT,'CURWIN')
C              ***********************
C
          DO 30 JDO30 = 1,4
            WRITE (STROUT,FMT='(4F6.2)') (RSM(JDO30,JCOUNT,JDO40),
     +        JCOUNT=1,4)
C
C                ***********************
            CALL PUTLIN(STROUT,'CURWIN')
C                ***********************
C
   30     CONTINUE
        END IF
   40 CONTINUE
      END
C

C     ========================================
      SUBROUTINE SYMTR4(NSYM,RSM,SYMCHS)
C     ========================================
C
C           symmetry translation from matrix back to characters
C
C           This translates the Symmetry matrices into INT TAB
C           character strings
C
C           It gives the real space operations.
C                eg     X,Y,Z
C                eg     -Y,X-Y, Z
C           That is more complicated than you might think!!
C
C---- Arguments :
C
C Nsym (I) INTEGER   Number of Symmetry operations
C
C Rsm  (I) REAL      Array of dimension (4,4,at least Nsym)
C                    coNTaining symmetry operations on input
C
C Symchs (O) CHARACTER*(*)   Array of dimension at least Nsym
C                            coNTaining int tab char strings on output
C
C
C     .. Scalar Arguments ..
      INTEGER NSYM
C     ..
C     .. Array Arguments ..
      REAL RSM(4,4,*)
      CHARACTER SYMCHS(*)*80
C     ..
C     .. Local Scalars ..
      REAL PPP,RRR
      INTEGER I1,I2,ICH,IST,ITR,Jdo10,Jdo20,Jdo40
C     ..
C     .. Local Arrays ..
      INTEGER NPNTR1(10),NPNTR2(10)
      CHARACTER AXISCR(3)*1,NUMB(9)*1
C     ..
C     .. External Functions ..
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,INT,LEN,NINT,REAL
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPERR
C     ..
C     .. Data statements ..
C
      DATA AXISCR/'X','Y','Z'/
      DATA NUMB/'1','2','3','4','5','6','7','8','9'/
      DATA NPNTR1/0,1,1,1,0,1,0,2,3,5/
      DATA NPNTR2/0,6,4,3,0,2,0,3,4,6/
C     ..
C
      DO 40 Jdo40 = 1,NSYM
C
C---- Clear Symchs
C
        SYMCHS(Jdo40) = ' '
        ICH = 1
C
        DO 30 Jdo20 = 1,3
C
C---- Ist is flag for first character of operator
C
          IST = 0
C
          DO 20 Jdo10 = 1,4
            IF (RSM(Jdo20,Jdo10,Jdo40) .ne. 0.0) THEN
              IF (RSM(Jdo20,Jdo10,Jdo40) .gt. 0.0 .and. 
     +               IST .gt. 0) THEN
                IF (ICH .gt. LEN(SYMCHS(1))) CALL CCPERR(1,
     +              'SYMTR4: character array too short')
                SYMCHS(Jdo40) (ICH:ICH) = '+'
                ICH = ICH + 1
              END IF
C
              IF (RSM(Jdo20,Jdo10,Jdo40) .lt. 0.0) THEN
                IF (ICH .gt. LEN(SYMCHS(1))) CALL CCPERR(1,
     +              'SYMTR4: character array too short')
                SYMCHS(Jdo40) (ICH:ICH) = '-'
                IST = 1
                ICH = ICH + 1
              END IF
C
              IF (Jdo10 .ne. 4) THEN
                IF (ICH .gt. LEN(SYMCHS(1))) CALL CCPERR(1,
     +              'SYMTR4: character array too short')
                SYMCHS(Jdo40) (ICH:ICH) = AXISCR(Jdo10)
                IST = 1
                ICH = ICH + 1
              END IF
C
C
              IF (Jdo10 .eq. 4 .and. 
     +               RSM(Jdo20,4,Jdo40) .ne. 0) THEN
                ITR = ABS(INT(RSM(Jdo20,4,Jdo40)))
                RRR = REAL(ITR)
                IF (RRR .gt. ABS(RSM(Jdo20,4,Jdo40))-0.0001 .and.
     +              RRR .lt. ABS(RSM(Jdo20,4,Jdo40))+0.0001) THEN
                  write (SYMCHS(Jdo40) (ICH:ICH+2),fmt=6000) ITR
 6000             FORMAT (i2,' ')
                  GO TO 10
                END IF
C
C
                RRR = ABS(RSM(Jdo20,4,Jdo40))
                PPP = RRR
                IF (RRR .gt. 1.0) THEN
                  PPP = RRR - REAL(ITR)
                  ITR = ABS(INT(RRR))
                  write (SYMCHS(Jdo40) (ICH:ICH+2),fmt=6002) ITR
 6002             FORMAT (i2,' ')
                  ICH = ICH + 3
                END IF
C
C
                ITR = NINT(ABS(PPP)*12.0)
                I1 = NPNTR1(ITR)
                I2 = NPNTR2(ITR)
                IF (ICH+2 .gt. LEN(SYMCHS(1))) CALL CCPERR(1,
     +              'SYMTR4: character array too short')
                SYMCHS(Jdo40) (ICH:ICH+2) = NUMB(I1)//'/'//NUMB(I2)
   10           ICH = ICH + 3
              END IF
            END IF
   20     CONTINUE
C
C---- ADD COMMA  space
C
          IF (Jdo20 .ne. 3) THEN
            IF (ICH+2 .gt. LEN(SYMCHS(1))) CALL CCPERR(1,
     +          'SYMTR4: character array too short')
            SYMCHS(Jdo40) (ICH:ICH+2) = ',  '
            ICH = ICH + 3
          END IF
   30   CONTINUE
   40 CONTINUE
      END


C
C
C     ==========================
      SUBROUTINE SYMTRN(NSM,RSM)
C     ==========================
C
C---- This translates the Symmetry matrices into INT TAB
C     character strings for each symmetry operation and prints the real
C     and reciprocal space operators on standard output.
C
C     It gives the real and reciprocal space operations.
C   eg     X,Y,Z    H,K,L
C   eg     -Y,X-Y, Z   -H-K, H, L  etc
C   That is more complicated than you might think!!
C
C---- Inverse symmetry needed to test systematic absences -
C     copy rsmm rsmtt this common block.
C
C      COMMON /SYSABS/ NSMT,RSMM(4,4,MAXSYM),RSMTT(4,4,MAXSYM)
C
C     .. Parameters ..
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER NSM
C     ..
C     .. Array Arguments ..
      REAL RSM(4,4,*)
C     ..
C     .. Local Scalars ..
      INTEGER I,ICH,IST,J,K,NS
C     ..
C     .. Local Arrays ..
      REAL RSMT(4,4,MAXSYM)
      CHARACTER HKLCR(3)*1,SYMCHS(MAXSYM)*80
C     ..
C     .. External Subroutines ..
      EXTERNAL INVSYM, SYMTR3, LUNSTO
      INTEGER LUNSTO
C     ..
C     .. Data statements ..
C
      DATA HKLCR/'H','K','L'/
C     ..
      CALL SYMTR3 (NSM, RSM, SYMCHS, 0)
C
      DO 50 NS = 1,NSM
C
C---- H K L   - get inverse symmetry operation
C
        CALL INVSYM(RSM(1,1,NS),RSMT(1,1,NS))
        ICH = 40
        DO 40 J = 1,3
          IST = 0
          DO 30 I = 1,3
            IF (RSMT(I,J,NS).NE.0) THEN
              IF (RSMT(I,J,NS).GT.0 .AND. IST.GT.0) THEN
                IF (ICH.GT.LEN(SYMCHS(1)))
     +               CALL CCPERR(1, 'SYMTR: character array too short')
                SYMCHS(NS) (ICH:ICH) = '+'
                ICH = ICH + 1
              END IF
              IF (RSMT(I,J,NS).LT.0) THEN
                IF (ICH.GT.LEN(SYMCHS(1)))
     +               CALL CCPERR(1, 'SYMTR: character array too short')
                SYMCHS(NS) (ICH:ICH) = '-'
                IST = 1
                ICH = ICH + 1
              END IF
              IF (ICH.GT.LEN(SYMCHS(1)))
     +             CALL CCPERR(1, 'SYMTR: character array too short')
              SYMCHS(NS) (ICH:ICH) = HKLCR(I)
              ICH = ICH + 1
              IST = 1
            END IF
   30     CONTINUE
C
C---- ADD COMMA space
C
          IF (J.NE.3) THEN
            IF (ICH+2.GT.LEN(SYMCHS(1)))
     +           CALL CCPERR(1, 'SYMTR: character array too short')
            SYMCHS(NS) (ICH:ICH+2) = ',  '
            ICH = ICH + 3
          END IF
   40   CONTINUE
C
C---- write a message
C
        WRITE (LUNSTO(1),FMT='(A,I3,A,/,2X,A,4(/,4F6.2,10X,4F6.2))')
     +    ' SYMMETRY ',NS,'   REAL SPACE            RECIPROCAL SPACE',
     +    SYMCHS(NS), ((RSM(I,J,NS),J=1,4), (RSMT(I,K,NS),K=1,4),I=1,4)
   50 CONTINUE
      END
C
C
C     ===========================
      SUBROUTINE SYSAB(IN,ISYSAB)
C     ===========================
C
C---- Test reflections for Systematic absences
C     Only reflns with EPSI gt 1 need be considered
C     Systematic absences flagged with ISYSAB = 1
C
C---- Inverse symmetry needed to test systematic absences
C     - copy into this common block.
C      COMMON /SYSABS/ NSM,RSM(4,4,MAXSYM),RSMT(4,4,MAXSYM)
C
C     .. Parmaters ..
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER ISYSAB
C     ..
C     .. Array Arguments ..
      INTEGER IN(3)
C     ..
C     .. Scalars in Common ..
      INTEGER NSM
C     ..
C     .. Arrays in Common ..
      REAL RSM,RSMT
C     ..
C     .. Local Scalars ..
      REAL ERR,PHAS
      INTEGER IH,IK,IL,J
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,NINT
C     ..
C     .. Common blocks ..
      COMMON /SYSABS/NSM,RSM(4,4,MAXSYM),RSMT(4,4,MAXSYM)
C     ..
C     .. Save statement ..
      SAVE
C     ..
C
C---- Generate symm equivs
C
C---- test whether h' k' l' equals h k l
C
      ISYSAB = 0
      IF (NSM.NE.1) THEN
C
        DO 10 J = 2,NSM
          IH = IN(1)*RSMT(1,1,J) + IN(2)*RSMT(2,1,J) + IN(3)*RSMT(3,1,J)
          IF (IH.EQ.IN(1)) THEN
            IK = IN(1)*RSMT(1,2,J) + IN(2)*RSMT(2,2,J) +
     +           IN(3)*RSMT(3,2,J)
            IF (IK.EQ.IN(2)) THEN
              IL = IN(1)*RSMT(1,3,J) + IN(2)*RSMT(2,3,J) +
     +             IN(3)*RSMT(3,3,J)
              IF (IL.EQ.IN(3)) THEN
C
C---- Test whether this Symmetry equivalent has a different phase to
C     in(1) in(2) in(3)  - If so it is a systematic absence
C     .... Believe me EJD
C
                PHAS = IN(1)*RSM(1,4,J) + IN(2)*RSM(2,4,J) +
     +                 IN(3)*RSM(3,4,J)
                ERR = ABS(PHAS-NINT(PHAS))
                IF (ERR.GT.0.05) ISYSAB = 1
              END IF
            END IF
          END IF
   10   CONTINUE
C
CC        IF (ISYSAB.EQ.1) WRITE (6,FMT=6000) IN
      END IF
C
C---- Format statements
C
CCC 6000 FORMAT (/'  REFLECTION',3I4,' SYSTEMATIC ABSENCE')
C
      END
C
C
C     ============================================
      SUBROUTINE XSPECIALS(NSM,RSM,XF,YF,ZF,NSPEC)
C     ============================================
C
C---- This subroutine finds what coordinates occupy special positions
C     ie have occupancies less than 1.0
C     from consideration of the Symmetry Operations.
C
C     .. Array Arguments ..
      REAL RSM(4,4,*)
C     ..
C     .. Scalar Arguments ..
      REAL XF,YF,ZF
      INTEGER NSM,NSPEC
C     ..
C     .. Local Scalars ..
      REAL DELX,DELXX,DELY,DELYY,DELZ,DELZZ,XXF,YYF,ZZF
      INTEGER J
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MOD
C     ..
C
      NSPEC = 1
C
      IF (NSM.NE.1) THEN
C
C---- Generate symm equivs
C
C---- test whether xxf yyf zzf equals xf yf zf  - possibly with a unit
C---- cell translation...
C
        DO 10 J = 2,NSM
          XXF = RSM(1,1,J)*XF + RSM(1,2,J)*YF + RSM(1,3,J)*ZF +
     +          RSM(1,4,J)
C
C---- Is DELX = n - ie - have we generated the same coordinate?
C
          DELX = ABS(XXF-XF) + 0.01
          DELXX = MOD(DELX,1.0)
          IF (DELXX.LE.0.01) THEN
C
            YYF = RSM(2,1,J)*XF + RSM(2,2,J)*YF + RSM(2,3,J)*ZF +
     +            RSM(2,4,J)
C
C---- Check DELY
C
            DELY = ABS(YYF-YF) + 0.01
            DELYY = MOD(DELY,1.0)
            IF (DELYY.LE.0.01) THEN
C
              ZZF = RSM(3,1,J)*XF + RSM(3,2,J)*YF + RSM(3,3,J)*ZF +
     +              RSM(3,4,J)
C
C---- Check DELZ
C
              DELZ = ABS(ZZF-ZF) + 0.01
              DELZZ = MOD(DELZ,1.0)
              IF (DELZZ.LE.0.01) NSPEC = NSPEC + 1
C
            END IF
          END IF
   10   CONTINUE
C
C---- next symm opn
C
C---- next reflection class
C
      END IF
C
      END
C     ============================
      LOGICAL FUNCTION HKLEQ(IH,KH)
C     =============================
C
C---- Returns true if indices ih = kh
C
C     .. Array Arguments ..
      INTEGER IH(3),KH(3)
C     ..
C
      HKLEQ = .FALSE.
C
      IF (IH(1).EQ.KH(1) .AND. IH(2).EQ.KH(2) .AND.
     +    IH(3).EQ.KH(3)) HKLEQ = .TRUE.
C
      END
C
C
C     =======================================
      INTEGER FUNCTION CCP4_HASH_LOOKUP(NSER)
C     =======================================
C
C---- The function ccp4_hash_lookup returns the value nfind (which was
C     input when setting up the function in the subroutine ccp4_hash_setup)
C     for the large range variable nser.  Uses hashing. (see comments for
C     CCP4_HASH_SETUP for description of hashing method).
C
      IMPLICIT NONE
C     .. Parameter (table size: MUST BE A PRIME NUMBER)
      INTEGER KPRI
      PARAMETER (KPRI=1999)
C
C     .. Scalar Arguments ..
      INTEGER NSER
C     ..
C     .. Arrays in Common ..
      INTEGER IT
C     ..
C     .. Local Scalars ..
      INTEGER NDX,NSER4
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
C     .. Common blocks ..
      COMMON /LOOK/IT(2,KPRI)
      SAVE /LOOK/
C     ..
C
      NSER4 = NSER
C
   10 CONTINUE
C
      NDX = MOD(NSER4,KPRI) + 1
      IF (NSER.NE.IT(1,NDX)) THEN
        IF (IT(1,NDX).NE.0) THEN
          NSER4 = NSER4 + 3
          GO TO 10
        END IF
      END IF
C
      CCP4_HASH_LOOKUP = IT(2,NDX)
C
      END
C
C
C     ======================================
      SUBROUTINE CCP4_HASH_SETUP(NSER,NFIND)
C     ======================================
C
C---- This subroutine sets up a value for the function ccp4_hash_lookup
C     when ccp4_hash_lookup(nser) is later evaluated it will return nfind
C     this function will allow the efficient retrieval of an identifier
C     for a large range variable (such as a crystal number).  the values
C     of the function ccp4_hash_lookup(nser) are stored in the array
C     it(2, kpri) where kpri is the prime number used to generate the
C     function
C     The array it  lives in the common look which is shared by
C     ccp4_hash_setup and the function ccp4_hash_lookup
C
C     NOTES: A hash table is a way of storing information so that it
C     easily be retrieved without the need for indexing or long searches.
C     NSER is referred to as the "key", which is "hashed" (computer-
C     science speak for "messed up") by the hashing function (in this
C     case MOD(NSER4,KPRI) + 1) to determine where the value pair will
C     be stored. The function LOOKUP can then search on the same basis
C     when supplied with the key, to retreive the pair in (at most) 3
C     calculations. Note that KPRI (the table size) MUST BE A PRIME in
C     order for this method to work.
C
C     IT(1, NDX) = NSER,  IT(2, NDX) = NFIND
C
      IMPLICIT NONE
C     .. Parameter (table size: MUST BE A PRIME NUMBER)
      INTEGER KPRI
      PARAMETER (KPRI=1999)
C
C     .. Scalar Arguments ..
      INTEGER NFIND,NSER
C     ..
C     .. Arrays in Common ..
      INTEGER IT
C     ..
C     .. Local Scalars ..
      INTEGER NDX,NSER4
      CHARACTER STROUT*140
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
C     .. Common blocks ..
      COMMON /LOOK/IT(2,KPRI)
      SAVE /LOOK/
C     ..
C
      NSER4 = NSER
   10 CONTINUE
      NDX = MOD(NSER4,KPRI) + 1
      IF ((NSER4-NSER) .GE. 3*KPRI) THEN
         WRITE (STROUT, '(A,I8)')
     $     ' **** Error in SETUP: overflowed hash table, size ', KPRI
         CALL PUTLIN(STROUT,'CURWIN')
         CALL CCPERR(1,'*** Filled hash table in SETUP ***')
      ENDIF
      IF (IT(1,NDX).NE.0) THEN
        NSER4 = NSER4 + 3
        GO TO 10
      END IF
C
      IT(1,NDX) = NSER
      IT(2,NDX) = NFIND
      RETURN
      END
C
C     =============================
      SUBROUTINE CCP4_HASH_ZEROIT()
C     =============================
C
      IMPLICIT NONE
C     .. Parameter (table size: MUST BE A PRIME NUMBER)
      INTEGER KPRI
      PARAMETER (KPRI=1999)
C
C     .. Arrays in Common ..
      INTEGER IT
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     ..
C     .. Common blocks ..
      COMMON /LOOK/IT(2,KPRI)
      SAVE /LOOK/
C
      DO 20 I = 1,KPRI
        IT(1,I) = 0
        IT(2,I) = 0
   20 CONTINUE
C
      END
C
C
C     ================================================
      SUBROUTINE SETRSL(A,B,C,ALPHA,BETA,GAMMA)
C     ================================================
C
C---- Routine to calculate coefficients for (sin(theta)/lambda)**2 from
C     h,k,l for general axes
C
C     first calculated the components of input axes in an orthonormal
C     basis, then calculate components of reciprocal axes in same basis
C
C---- Input angles are in degrees
C
C     .. Scalar Arguments ..
      REAL A,ALPHA,B,BETA,C,GAMMA
C     ..
C     .. Scalars in Common ..
      REAL AXST,AYST,AZST,BYST,BZST,COEFHH,COEFHK,COEFHL,COEFKK,COEFKL,
     +     COEFLL,CZST
C     ..
C     .. Local Scalars ..
      REAL AR,AX,BR,BX,BY,CX,CY,CZ,DTORAD,GR,HALF,QMIN,STMAX,TMAX,TWO,
     +     XX,ZERO
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,COS,MAX,SIN,SQRT
C     ..
C     .. Common blocks ..
      COMMON /DSTAR/AXST,AYST,AZST,BYST,BZST,CZST
      COMMON /RECPLT/COEFHH,COEFHK,COEFHL,COEFKK,COEFKL,COEFLL
      SAVE /DSTAR/, /RECPLT/
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
C---- CZ determined by length of C
      XX = C*C - CX*CX - CY*CY
      CZ = SQRT(XX)
      TMAX = MAX(AX,BY)
      TMAX = MAX(TMAX,CZ)
      IF (ABS(BX/TMAX).LT.QMIN) BX = ZERO
      IF (ABS(CX/TMAX).LT.QMIN) CX = ZERO
      IF (ABS(CY/TMAX).LT.QMIN) CY = ZERO
C
c      WRITE (6,FMT=6000) AX,BX,BY,CX,CY,CZ
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
c      WRITE (6,FMT=6002) AXST,AYST,BYST,AZST,BZST,CZST
C
C---- The other three components of reciprocal vectors are zero
C     coefficient of h*h
C
      COEFHH = AXST*AXST + AYST*AYST + AZST*AZST
C
C---- Coefficient of h*k
C
      COEFHK = (AYST*BYST+AZST*BZST)*TWO
C
C---- coefficient of h*l
C
      COEFHL = AZST*CZST*TWO
C
C---- coefficient of k*k
C
      COEFKK = BYST*BYST + BZST*BZST
C
C---- coef of k*l
C
      COEFKL = BZST*CZST*TWO
C
C---- coef of l*l
C
      COEFLL = CZST*CZST
C
C---- Format statements
C
CCC 6000 FORMAT (' Direct Matrix     :',T25,1P,E15.6,2 (12X,'0.0'),/,
CCC     +                               T25,2E15.6,12X,'0.0',/,
CCC     +                               T25,3E15.6,/)
CCC 6002 FORMAT (' Reciprocal Matrix :',T25,1P,E15.6,2 (12X,'0.0'),/,
CCC     +                               T25,2E15.6,12X,'0.0',/,
CCC     +                               T25,3E15.6,/)
C
      END
C
C
C     ===========================
      REAL FUNCTION STHLSQ(IH,IK,IL)
C     ============================
C
C---- Calculate (sin(theta)/lambda)**2 from h,k,l; coef's set by call to
C        SETRSL : good for any kind of axes
C
C
C     .. Scalar Arguments ..
      INTEGER IH,IK,IL
C     ..
C     .. Scalars in Common ..
      REAL COEFHH,COEFHK,COEFHL,COEFKK,COEFKL,COEFLL
C     ..
C     .. Common blocks ..
      COMMON /RECPLT/COEFHH,COEFHK,COEFHL,COEFKK,COEFKL,COEFLL
      SAVE /RECPLT/
C     ..
C
      STHLSQ = IH*IH*COEFHH + IH*IK*COEFHK + IH*IL*COEFHL +
     +         IK*IK*COEFKK + IK*IL*COEFKL + IL*IL*COEFLL
C
      END
C
C
C     ==================================
      LOGICAL FUNCTION CENTRC(KHKL,ICENT)
C     ==================================
C
C---- returns value true if reflection khkl is centric, false otherwise.
C     general for all point groups - but only for the unique set of
C     indices which conforms to the criterion of maximising the value
C     of        (khkl(3)*256 + khkl(2))*256 + khkl(1)
C
C    as produced by e.g. subroutine turnip in protin and ulysses.
C
C---- in this case the required tests are controlled by 7 flags in
C     icent fo
C
C  0KL  H0L  HK0  HKK  HKH  HHL  H,-2H,L
C     (the last is needed in pg312)
C
C     .. Array Arguments ..
      INTEGER ICENT(7),KHKL(3)
C     ..
C     .. Local Scalars ..
      INTEGER JJ
C     ..
C
      CENTRC = .FALSE.
      IF (ICENT(1).NE.0) THEN
        IF (KHKL(1).EQ.0) GO TO 10
      END IF
      IF (ICENT(2).NE.0) THEN
        IF (KHKL(2).EQ.0) GO TO 10
      END IF
      IF (ICENT(3).NE.0) THEN
        IF (KHKL(3).EQ.0) GO TO 10
      END IF
      IF (ICENT(4).NE.0) THEN
        IF (KHKL(2).EQ.KHKL(3)) GO TO 10
      END IF
      IF (ICENT(5).NE.0) THEN
        IF (KHKL(3).EQ.KHKL(1)) GO TO 10
      END IF
      IF (ICENT(6).NE.0) THEN
        IF (KHKL(1).EQ.KHKL(2)) GO TO 10
      END IF
      IF (ICENT(7).EQ.0) THEN
        GO TO 20
      ELSE
        JJ = -KHKL(1)*2
        IF (KHKL(2).NE.JJ) GO TO 20
      END IF
   10 CENTRC = .TRUE.
   20 RETURN
C
      END
C
C
C
      INTEGER FUNCTION KROT(NS)
C     =========================
C
C---- Apply ns'th symmetry operation to jp to get lp,
C     check if lies in asymmetric unit given by nau
C
C      Returns KROT=0  correct operation
C                  =1  if not
C
C     .. Parameters ..
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      INTEGER NS
C     ..
C     .. Scalars in Common ..
      REAL RHMAX,RHMEAN,RHMIN
      INTEGER LUNIN,LUNOUT,INMAP,OUTMAP,SYMFIL
      INTEGER IBCD,IX1,IX2,IY1,IY2,IZ1,IZ2,JSEC,JX1,JX2,JZ1,JZ2,LSEC,
     +        LSPGRP,NSEC,NSYM
C     ..
C     .. Arrays in Common ..
      REAL CELL
      INTEGER IUVW,JP,JS,JT,KP,LP,NAU,NXYZ,NXYZ10
C     ..
C     .. Local Scalars ..
      INTEGER I,J,L
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
C     .. Common blocks ..
      COMMON /INPUT/NXYZ(3),IX1,IX2,IY1,IY2,IZ1,IZ2,JSEC,JX1,JX2,JZ1,
     +       JZ2,IUVW(3),NSEC,KP(3),CELL(6),LSPGRP,RHMIN,RHMAX,RHMEAN,
     +       IBCD
      COMMON /POINTS/JP(3),LP(3),LSEC,NXYZ10(3)
      COMMON /SYM/NSYM,NAU(3),JS(3,3,MAXSYM),JT(3,MAXSYM)
      COMMON /INOUT/ LUNIN,LUNOUT,INMAP,OUTMAP,SYMFIL
      SAVE /INPUT/, /POINTS/, /SYM/, /INOUT/
C     ..
C
C---- Error trap
C
      IF (NS.LE.0) CALL CCPERR(1,'internal error: NS <= 0 in KROT')
      KROT = 1
      DO 20 I = 1,3
        L = JT(I,NS)
        DO 10 J = 1,3
          L = JP(J)*JS(I,J,NS) + L
   10   CONTINUE
C
C---- Get into cell
C
        L = NXYZ10(I) + L
        L = MOD(L,NXYZ(I))
        IF (I.EQ.2) THEN
C
C---- For W axis, check same section
C
          IF (L.NE.LSEC) GO TO 30
C
C---- U or V axes
C
        ELSE IF (L.GT.NAU(I)) THEN
          GO TO 30
        END IF
C
C---- OK
C
        LP(I) = L
   20 CONTINUE
C
C---- Successful
C
      KROT = 0
   30 END
C
      SUBROUTINE PSTOPH (PSIX,PSIY,PSIZ,PHIX,PHIY,PHIZ,AVPHI)
C     =======================================================
C
C***   *****  PSTOPH  *****
C***   Convert PSIX,PSIY,PSIZ (= epsx,epsy,epsz) to PHIX,PHIY,PHIZ ,
C***    using AVPHI
C      All angles in radians
C
      CP1 = COS(PSIX)
      SP1 = SIN(PSIX)
      CP2 = COS(PSIY)
      SP2 = SIN(PSIY)
      CP3 = COS(PSIZ)
      SP3 = SIN(PSIZ)
      CP = COS(AVPHI)
      SP = SIN(AVPHI)
 
C***CALCULATE PHIX
      SPX = SP2*SP + CP2*SP1*CP
      CPX = CP2*CP1
      PHIX = ATAN2(SPX,CPX)
 
C***CALCULATE PHIY
      SPY =-CP2*SP1*SP + SP2*CP
      CPY = CP2*CP1/COS(PHIX)
      PHIY = ATAN2(SPY,CPY)
 
C***CALCULATE PHIZ
      P11 = CP3*CP2*CP + SP*(CP3*SP2*SP1 - SP3*CP1)
      P21 = SP3*CP2*CP+SP*(SP3*SP2*SP1+CP3*CP1)
      SPZ =-SP*P11 + CP*P21
      CPZ = CP*P11 + SP*P21
      PHIZ = ATAN2(SPZ,CPZ)
 
      RETURN
C**   DEBUG SUBCHK
      END
C
C
C     ==============================
      REAL FUNCTION STS3R4(IH,IK,IL)
C     ==============================
C
C---- calculate (sin(theta)/lambda)**2 from h,k,l; coef's set by call to
C        setrsl : good for any kind of axes
C
C     .. Scalar Arguments ..
      REAL IH,IK,IL
C     ..
C     .. Scalars in Common ..
      REAL COEFHH,COEFHK,COEFHL,COEFKK,COEFKL,COEFLL
C     ..
C     .. Common blocks ..
      COMMON /RECPLT/COEFHH,COEFHK,COEFHL,COEFKK,COEFKL,COEFLL
C     ..
C     .. Save statement ..
      SAVE /RECPLT/
C     ..
C
      STS3R4 = IH*IH*COEFHH + IH*IK*COEFHK + IH*IL*COEFHL +
     +         IK*IK*COEFKK + IK*IL*COEFKL + IL*IL*COEFLL
C
      END
C----------------------------------------------------------------
      SUBROUTINE PGDEFN(NAMPG,NSYMP,NSYM,RSMT,LPRINT)
C     ==============================================
C
c  this subroutine assigns a possible point group, using the 
C  symmetry operators. Only simplest ones returned.
C
C  Things for MDF files... Draft for Bauke...
C
C     ICENTR   : axis of centering
C                0  =  no centering               (P spacegroups)
C                1  =  centering around a-axis    (A spacegroups)
C                2  =  centering around b-axis    (B spacegroups)
C                3  =  centering around c-axis    (C spacegroups)
C                4  =  centering on all faces     (F spacegroups)
C                5  =  body centering             (I spacegroups)
C                6  =  rhombohedral centering     (R spacegroups with
C                                                    hexagonal axes)
C                      (NOTE: R-spacegroups with rhombohedral axes
C                             have ICENTR = 0!)
C
C     ISCREW(3): type of screw axis for A, B  and C
C                so ISCREW(I) must be 0 (no screw), 2, 3, 4, or 6
C EJD:     I think I may have this wrong for non primitive spacegroups.
C          The routine only looks at the primitive sym ops ....
C
C     KLASS    : a crystal class number of the set below
C     KLASS  crystal system  laue class comments  spacegroup number
C  EJD
C          Our nlaue code number ...
C          We do not allow KLASS 7 10 15 16
C
C                                                 (INT. TABLES)
C       1    TRICLINIC       1_BAR (PG1)                        1
C       2    MONOCLINIC   I  2/M (PG2) B-UNIQUE           3 -   5
C       3    ORTHORHOMBIC    MMM (PG222)                 16 -  24
C       4    TETRAGONAL   I  4/M (PG4)                   75 -  80
C       5    TETRAGONAL  II  4/MMM (PG422)               89 -  98
C       6    TRIGONAL     I  3_BAR (PG3)HEXAGONAL AXES   143 - 146
C       7    TRIGONAL    II  3_BAR (??) RHOMBOHEDRAL AXES      146
C       8    TRIGONAL   III  3_BAR1M (PG312)           149,151,153
C       9    TRIGONAL    IV  3_BARM1 (PG321)HEXAGONAL AXES  
C                                                   150,152,154,155
C      10    TRIGONAL     V  3_BARM1 (??)RHOMBOHEDRAL AXES      155
C      11    HEXAGONAL    I  6/M  (PG6)                   168 - 173
C      12    HEXAGONAL   II  6/MMM (PG622)                177 - 182
C      13    CUBIC        I  M3_BAR (PG23)                195 - 199
C      14    CUBIC       II  M3_BARM (PG432)              207 - 214
C      15    MONOCLINIC  II  2/M (PG2c)  C UNIQUE         3 -   5
C      16    MONOCLINIC III  2/M (PG2a)  A UNIQUE         3 -   5
C
C---- In this table only the enantiomorphic spacegroups are
C     included. For the other spacgroups (which contain (glide)
C     mirrors or an inversion center) this routine can still be
C     used, but then isym has no meaning anymore.
C
C   3 pg1     1bar      hkl:l>=0  hk0:h>=0  0k0:k>=0   1,2
C     pg1bar
C   4 pg2     2/m        hkl:k>=0, l>=0  hk0:h>=0       3/b,4/b....
C     pgm pg2/m
C   5 pg2     2/m        hkl:k>=0, l>=0  h0l:h>=0       1003,1004
C     pgm pg2/m
C   6 pg222   mmm        hkl:h>=0, k>=0, l>=0            16 ...
C     pgmm2 pgmmm 
C   7 pg4     4/m        hkl:h>=0, l>=0 with k>=0 if  h=0  and
C     pg4bar pg4/m                            k>0 if h>0
C   8 pg422   4/mmm       hkl:h>=0, k>=0, l>=0            89..
C     pg4mm pg4bar2m pg4barm2 pg4/mmm
C   9 pg3     3bar      hkl:h>=0, k>0  00l:l>0         143..
C     pg3bar
C  10 pg312   3/m        hkl:h>=0, k>=0 with k<=h for all l.
C     pg32 pg3m pg3m1 pg3barm1 if k = 0  l>=0
C           Space group numbers :   149-151-153 157 159 162 163
C  11 pg321   3bar1m     hkl:h>=0, k>=0 with k<=h for all l.
C     pg31m pg3bar1m      if h = k  l>=0
C           Space group numbers :   150-152-154
C  12 pg6     6/m        hkl:h>=0, k>=0, l>=0 with k>=0 if  h=0
C     pg6bar  6/m        and k> 0 if h>0
C  13 pg622   6/mmm       hkl:h>=0, k>=0, l>=0 with h>=k 177..
C     pg6mm pg6barm2 pg6bar2m  pg 6/mmm
C  14 pg23    m3         hkl:h>=0, k>=0, l>=0 with l>=h,  k>=h
C     pgm3bar 
C  15 pg432   m3m        hkl:h>=0, k>=0, l>=0  with  k>=l
C     pg4bar3m pgm3barm
C
C---- Find unique set of rsmt - these are the reciprocal space symmetry
C                               operators and there will be duplicate
C                               rsmt(3,3) for non primitivs spacegroups.
C
C   Copy  symmetry into RJUNK(i,j,n) to preserve it.
C   Set rsmt(4,4,??) = 0.0 as a flag for a duplicate while checking.
C
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
C     .. Scalar Arguments ..
      INTEGER NSYM,NSYMP
      CHARACTER NAMPG* (*)
      LOGICAL LPRINT
C     ..
C     .. Array Arguments ..
      REAL RSMT(4,4,*)
C     ..
C     .. Scalars in Common ..
      INTEGER ICENTR,IFAST,INTER,ISLOW,IVERSN,KLASS,MAXB,MAXR
      CHARACTER STROUT*140
C     ..
C     .. Arrays in Common ..
      REAL CELL
      INTEGER ISCREW
C     ..
C     .. Local Scalars ..
      REAL DET,DX,DXY,DXYZ,DXZ,DY,DYZ,DZ
      INTEGER I,IH,IHR,IK,IKR,IL,ILR,IRAXIS,IRMIN,IROT,ISCR,ISM1,ISM2,
     +        ISS,ISYM,J,JMIN,JROT,JSM,JUNIQU,N,NREP,NREPET
C     ..
C     .. Local Arrays ..
      REAL RJUNK(4,4,MAXSYM)
      INTEGER IN(3),JROTS(MAXSYM),NORIG(MAXSYM),NREPP(MAXSYM),
     +     NROT(MAXSYM),NROTS(MAXSYM)
C     ..
C     .. External Functions ..
      INTEGER LENSTR
      EXTERNAL LENSTR
C     ..
C     .. External Subroutines ..
      EXTERNAL DETERM
      EXTERNAL PUTLIN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,NINT
C     ..
C     .. Common blocks ..
      COMMON /MDFPAR/MAXR,MAXB,CELL(6),ISLOW,INTER,IFAST,KLASS,ICENTR,
     +       ISCREW(3),IVERSN
C     ..
C     .. Save statement ..
      SAVE
C     ..
C
      IF (LPRINT) THEN
        WRITE (STROUT,FMT=6020) NSYM
 6020   FORMAT(' In PGDEFN: Nsym = ',I6)
        CALL PUTLIN(STROUT,'CURWIN')
      ENDIF
C
      DO 30 N = 1,NSYM
C---- Clear all repeat counts
        NREPP(N) = 0
        DO 20 J = 1,4
          DO 10 I = 1,4
            RJUNK(I,J,N) = RSMT(I,J,N)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
C
      NSYMP = 0
C
C---- Which sym ops belong to primitive set? Check 1 - nsym-1 first.
C
      DO 70 ISM1 = 1,NSYM - 1
C
C---- We have already dealt with this Symm op...
C
        IF (RSMT(4,4,ISM1).NE.0.0) THEN
          NREP = 0
          NSYMP = NSYMP + 1
          NORIG(ISM1) = NSYMP
C
C---- Search all remaining ones for duplicates...
C
          DO 60 ISM2 = ISM1 + 1,NSYM
            DO 50 I = 1,3
              DO 40 J = 1,3
                IF (RSMT(I,J,ISM2).NE.RSMT(I,J,ISM1)) GO TO 60
   40         CONTINUE
   50       CONTINUE
C
C---- This is a duplicate... - count how often there is repetition and
C     record which sym op it is a duplicate of.
C     Modify rsmt(4,4,ism2) to act as a flag.
C
            NREP = NREP + 1
            NREPP(ISM2) = NREP
            NORIG(ISM2) = NORIG(ISM1)
            RSMT(4,4,ISM2) = 0.0
            IF (LPRINT) THEN
              WRITE (STROUT,FMT='(1X,A,I3,A,I3)')
     +     '  Reciprocal space symmetry operator ',ISM2,' same as ',
     +        ISM1
              CALL PUTLIN(STROUT,'CURWIN')
            ENDIF
   60     CONTINUE
        END IF
   70 CONTINUE
C
C---- Check last symmetry operator now.
C
      IF (RSMT(4,4,NSYM).NE.0.0) THEN
        NREP = 0
        NSYMP = NSYMP + 1
        NORIG(NSYM) = NSYMP
      END IF
C
C---- Now rewrite symmetry with all primitives
C     first and define centring.
C
C   If nsym=nsymp   you have a primitive spacegroup.
C
      NREPET = NSYM/NSYMP
      ICENTR = 0
      IF (NREPET.NE.1) THEN
C
C---- If nrepet = 4 you must have FACE centring. (icentr = 4)
C
        IF (NREPET.EQ.4) ICENTR = 4
C
C---- If nrepet = 3 you must have Rhombehedral centring. (icentr = 6)
C
        IF (NREPET.EQ.3) ICENTR = 6
C
C---- If nrepet.eq 2 you may have A B C or I centring.
C
C     Check translation components for appropriate
C     symmetry axes after reordering.
C
        ISYM = 0
C
        DO 100 N = 1,NSYM
          ISYM = NREPP(N)*NSYMP + NORIG(N)
C
          DO 90 J = 1,4
            DO 80 I = 1,4
              RSMT(I,J,ISYM) = RJUNK(I,J,N)
   80       CONTINUE
   90     CONTINUE
C
C---- Reset rsmt(4,4,...) = 1.0
C
          RSMT(4,4,N) = 1.0
  100   CONTINUE
      END IF
C
C---- More centring.
C     If nrepet.eq 2 you may have A B C or I centring.
C
      IF (NREPET.EQ.2) THEN
        DX = ABS(RSMT(1,4,1)-RSMT(1,4,NSYMP+1))
        DY = ABS(RSMT(2,4,1)-RSMT(2,4,NSYMP+1))
        DZ = ABS(RSMT(3,4,1)-RSMT(3,4,NSYMP+1))
C
C---- If dx dy and dz are all = 0.5 you must
C     have body centring. (icentr = 5)
C
        DXYZ = DX*DY*DZ
        IF (DXYZ.GT.0.1) THEN
          ICENTR = 5
        ELSE
          DXY = DX*DY
          DXZ = DX*DZ
          DYZ = DY*DZ
C
C---- If dy and dz  = 0.5 and dx=0 you must
C     have A  centring. (icentr = 1)
C
          IF (DYZ.GT.0.2) ICENTR = 1
C
C---- If dx and dz  = 0.5 and dy=0 you must
C     have B  centring. (icentr = 2)
C
          IF (DXZ.GT.0.2) ICENTR = 2
C
C---- If dx and dy  = 0.5 and dz=0 you must
C     have C  centring. (icentr = 3)
C
          IF (DXY.GT.0.2) ICENTR = 3
        END IF
      END IF
C
C---- Find whether 2fold 3 fold etc and define ISCREW(...)
C     use general reflection  - taken as (1,4,8)
C
      IN(1) = 1
      IN(2) = 4
      IN(3) = 8
C
C---- Generate primitive symm equivs and look at rotations.
C
C---- test whether h' k' l' equals  h  k  l
C
C----- Choose rotation axis - either a(3) b(2) c(1)  or
C                             lines 1 1 0(4)  or  1 1 1(5)
C
C      Define ISCREW(i) = 0,2,3,4,6
C
      ISCREW(1) = 0
      ISCREW(2) = 0
      ISCREW(3) = 0
      JSM = 0
C
      DO 130 J = 1,NSYMP
C
C---- Is it a rotation at all?
C
C            ***********************
        CALL DETERM(DET,RSMT(1,1,J))
C            ***********************
C
        IF (DET.EQ.1.0) THEN
C
          NROT(J) = 0
          IRAXIS = 0
C
C---- Unique rotation axes must be
C     a  b  c  or the lines 1 1 0   or 1 1 1.
C
C----   a
C
          IF (RSMT(1,1,J).EQ.1.0) IRAXIS = 3
C
C---- b
C
          IF (RSMT(2,2,J).EQ.1.0) IRAXIS = 2
C
C---- C
          IF (RSMT(3,3,J).EQ.1.0) IRAXIS = 1
C
C---- 1 1 0
C
          IF (RSMT(2,1,J).EQ.1.0 .AND. RSMT(1,2,J).EQ.1.0 .AND.
     +        RSMT(3,3,J).EQ.-1.0) IRAXIS = 4
C
C---- 1 1 1
C
          IF (RSMT(2,1,J).EQ.1.0 .AND. RSMT(3,2,J).EQ.1.0 .AND.
     +        RSMT(1,3,J).EQ.1.0) IRAXIS = 5
C
C---- 1 1 1
C
          IF (RSMT(3,1,J).EQ.1.0 .AND. RSMT(1,2,J).EQ.1.0 .AND.
     +        RSMT(2,3,J).EQ.1.0) IRAXIS = 5
C
          IHR = IN(1)
          IKR = IN(2)
          ILR = IN(3)
C
C---- Ignore anything else eg -1 1 0  - they are consequences of others
C
          IF (IRAXIS.NE.0) THEN
C
C---- What sort of rotation ? 2fold? 3fold? 4fold? 6fold?
C
            JSM = JSM + 1
            NROT(JSM) = 0
C
            DO 110 IROT = 1,6
              IH = RSMT(1,1,J)*IHR + RSMT(2,1,J)*IKR + RSMT(3,1,J)*ILR
              IK = RSMT(1,2,J)*IHR + RSMT(2,2,J)*IKR + RSMT(3,2,J)*ILR
              IL = RSMT(1,3,J)*IHR + RSMT(2,3,J)*IKR + RSMT(3,3,J)*ILR
C
C---- Back to h k l - how many rotations to get here?
C
              IF (IH.EQ.IN(1) .AND. IK.EQ.IN(2) .AND. IL.EQ.IN(3)) THEN
                GO TO 120
              ELSE
C
C---- apply symmetry again....
C
                IHR = IH
                IKR = IK
                ILR = IL
              END IF
  110       CONTINUE
C
            GO TO 130
  120       NROT(JSM) = 10*IRAXIS + IROT
C
C---- Check screwiness for a b and c axes -
C     keep the highest order of it.
C   ( a 6 fold axis will also produce a 2 fold and a 3 fold...)
C
            IF (IRAXIS.LE.3) THEN
              ISCR = 0
              JROT = 4 - IRAXIS
C
C---- Test translation component
C
              IF (RSMT(4,JROT,J).NE.0.0) ISCR = NINT(1.0/
     +            ABS(RSMT(4,JROT,J)))
C
C---- If that one was 0 try this "translation component"
C
              IF (ISCR.EQ.0 .AND. RSMT(JROT,4,J).NE.
     +            0.0) ISCR = NINT(1.0/ABS(RSMT(JROT,4,J)))
              IF (ISCR.GT.ISCREW(JROT)) ISCREW(JROT) = ISCR
            END IF
          END IF
        END IF
  130 CONTINUE
C
C---- Sort rotation info - nrot(...) gt 50 means iraxis = 5
C                                       means cubic   etc...
C
      DO 150 I = 1,JSM
        IRMIN = 1000000
        JMIN = 0
C
        DO 140 J = 1,JSM
          IF (IRMIN.GT.NROT(J)) THEN
            JMIN = J
            IRMIN = NROT(J)
          END IF
  140   CONTINUE
C
        NROTS(I) = IRMIN
        JROTS(I) = JMIN
        NROT(JMIN) = 1000001
  150 CONTINUE
C
C---- Get rid of  duplications
C
      JUNIQU = 1
C
      DO 160 I = 2,JSM
        IF (NROTS(I).NE.NROTS(JUNIQU)) THEN
          JUNIQU = JUNIQU + 1
          NROTS(JUNIQU) = NROTS(I)
          JROTS(JUNIQU) = JROTS(I)
        END IF
  160 CONTINUE
C
C---- Choose point group
C
C     Rarest first
C
C  Cubic
C  14 pg23   m3        KLASS 13
C  15 pg432  m3m       KLASS 14
C
      IF (NROTS(JUNIQU).GT.50) THEN
        IF (NROTS(2).EQ.12) THEN
          NAMPG = 'PG23'
          KLASS = 13
          IF (NROTS(3).EQ.14) THEN
            NAMPG = 'PG432'
            KLASS = 14
          END IF
        END IF
C
C---- Space groups with 2 fold axes along 1 1 0
C     8 pg422 4/mmm       KLASS 5
C    11 pg321  3/m        KLASS 9
C    13 pg622 6/mmm       KLASS 12
C
      ELSE IF (NROTS(JUNIQU).GT.40) THEN
        IF (NROTS(2).EQ.12 .AND. NROTS(3).EQ.14) THEN
          NAMPG = 'PG422'
          KLASS = 5
        END IF
        IF (NROTS(2).EQ.13) THEN
          NAMPG = 'PG321'
          KLASS = 9
        END IF
        IF (NROTS(2).EQ.12 .AND. NROTS(3).EQ.13) THEN
          NAMPG = 'PG622'
          KLASS = 12
        END IF
C
C--- 6 pg222  mmm     KLASS = 3
C   11 pg312  3/m     KLASS = 8
C   ?? pg2a   2       KLASS = 16    monoclinic A unique.
C
      ELSE IF (NROTS(JUNIQU).GT.30) THEN
        IF (NROTS(2).EQ.13) THEN
          NAMPG = 'PG312'
          KLASS = 8
        ELSE IF (NROTS(2).EQ.12 .AND. NROTS(3).EQ.22) THEN
          NAMPG = 'PG222'
          KLASS = 3
        ELSE IF (NROTS(JUNIQU).EQ.32) THEN
          NAMPG = 'PG2A'
          KLASS = 16
        END IF
C
C---- 4 pg2    2/m
C
      ELSE IF (NROTS(JUNIQU).EQ.22) THEN
        NAMPG = 'PG2'
        KLASS = 2
C
C---- 3 pg1     1bar          KLASS 1
C     5 pg2c    2/m  C unique KLASS 15
C     7 pg4    4/m            KLASS 4
C     9 pg3     3bar          KLASS 6
C    12 pg6    6/m            KLASS 11
C
      ELSE IF (NROTS(JUNIQU).LT.20) THEN
        IF (NROTS(JUNIQU).EQ.11) NAMPG = 'PG1'
        IF (NROTS(JUNIQU).EQ.11) KLASS = 1
        IF (NROTS(JUNIQU).EQ.12) NAMPG = 'PG2C'
        IF (NROTS(JUNIQU).EQ.12) KLASS = 15
        IF (NROTS(JUNIQU).EQ.13) NAMPG = 'PG3'
        IF (NROTS(JUNIQU).EQ.13) KLASS = 6
        IF (NROTS(JUNIQU).EQ.14) NAMPG = 'PG4'
        IF (NROTS(JUNIQU).EQ.14) KLASS = 4
        IF (NROTS(JUNIQU).EQ.16) NAMPG = 'PG6'
        IF (NROTS(JUNIQU).EQ.16) KLASS = 11
      END IF
C
C---- next symm opn
C
      IF (LPRINT) THEN
        WRITE (STROUT,FMT='(A,4X,I4)')
     +       '  Number of primitive symmetry operators',NSYMP
        CALL PUTLIN(STROUT,'CURWIN')
        WRITE (STROUT,FMT='(A,4X,I4)')
     +       '  Number of  symmetry operators         ',NSYM
        CALL PUTLIN(STROUT,'CURWIN')
        WRITE (STROUT,FMT='(A,4X)')
     +       '  The point group for these symmetry operators is '
        STROUT(LENSTR(STROUT)+2:) = NAMPG
        CALL PUTLIN(STROUT,'CURWIN')
C     
C     
        DO 170 ISS = 1,NSYM
          WRITE (STROUT,FMT='(A,4X,I3)')
     +         ' Resorted symmetry( all primitives first)',ISS
          CALL PUTLIN(STROUT,'CURWIN')
          DO 175 IDO = 1,4
            WRITE (STROUT,FMT='(4(2X,F8.3))') (RSMT(IDO,J,ISS),J=1,4)
            CALL PUTLIN(STROUT,'CURWIN')
 175      CONTINUE
 170    CONTINUE
C     
        CALL PUTLIN(' **** ICENTR   gives  axis of centering *****',
     +       'CURWIN')
        IF (ICENTR.EQ.0) THEN
          STROUT = '  No centering              (P spacegroups)'
        ELSE IF (ICENTR.EQ.1) THEN
          STROUT = '  Centering around a-axis    (A spacegroups)'
        ELSE IF (ICENTR.EQ.2) THEN
          STROUT = '  Centering around b-axis    (B spacegroups)'
        ELSE IF (ICENTR.EQ.3) THEN
          STROUT = '  Centering around c-axis    (C spacegroups)'
        ELSE IF (ICENTR.EQ.4) THEN
          STROUT = '  Centering on all faces     (F spacegroups)'
        ELSE IF (ICENTR.EQ.5) THEN
          STROUT = '  Body centering             (I spacegroups)'
        ELSE IF (ICENTR.EQ.6) THEN
          CALL BLANK('CURWIN',2)
          WRITE (STROUT,FMT='(A)') '  Rhombohedral centering'
          CALL PUTLIN(STROUT,'CURWIN')
          CALL PUTLIN(' (R spacegroups with hexagonal axes)','CURWIN')
          CALL PUTLIN(' (NOTE: R-spacegroups with rhombohedral axes',
     +         'CURWIN')
          CALL PUTLIN(' have ICENTR = 0 !)','CURWIN')
          GO TO 9876
        END IF
C     
C     
        CALL BLANK('CURWIN',2)
        CALL PUTLIN(STROUT,'CURWIN')
C
 9876   ISCR = ISCREW(1) + ISCREW(2) + ISCREW(3)
        IF (ISCR.GT.0) THEN
          CALL BLANK('CURWIN',2)
          CALL PUTLIN(' **** Screw axes are: *****','CURWIN')
        END IF
C
        IF (ISCREW(1).GT.0) THEN
          WRITE (STROUT,FMT='(I4,A)') ISCREW(1),
     +         '  fold screw axis along A '
          CALL BLANK('CURWIN',2)
          CALL PUTLIN(STROUT,'CURWIN')
        END IF
C     
        IF (ISCREW(2).GT.0) THEN
          WRITE (STROUT,FMT='(I4,A)') ISCREW(2),
     +         ' fold screw axis along B '
          CALL BLANK('CURWIN',2)
          CALL PUTLIN(STROUT,'CURWIN')
        END IF
C     
        IF (ISCREW(3).GT.0) THEN
          WRITE (STROUT,FMT='(I4,A)') ISCREW(3),
     +         ' fold screw axis along C '
          CALL BLANK('CURWIN',2)
          CALL PUTLIN(STROUT,'CURWIN')
        END IF
C     
        WRITE (STROUT,FMT='(A)')
     +   
        CALL BLANK('CURWIN',3)
        CALL PUTLIN('*** KLASS: a crystal class name used in MDF '//
     +       'files ***','CURWIN')  
C
C---- (int. tables)
C
        CALL BLANK('CURWIN',2)
        IF (KLASS.EQ.1) THEN
          STROUT = '  TRICLINIC       1_BAR (PG1)       sgs  1 '
        ELSE IF (KLASS.EQ.2) THEN
          STROUT = '  MONOCLINIC   I  2/M (PG2) B-UNIQUE  sgs  3 -  5'
        ELSE IF (KLASS.EQ.3) THEN
          STROUT = '  ORTHORHOMBIC    MMM (PG222)         sgs 16 - 24'
        ELSE IF (KLASS.EQ.4) THEN
          STROUT = '  TETRAGONAL   I  4/M (PG4)          sgs 75 - 80'
        ELSE IF (KLASS.EQ.5) THEN
          STROUT = '  TETRAGONAL  II  4/MMM (PG422)      sgs 89 - 98 '
        ELSE IF (KLASS.EQ.6) THEN
          STROUT = 
     .     '  TRIGONAL I  3_BAR (PG3)HEXAGONAL AXES sgs  143-146'
        ELSE IF (KLASS.EQ.7) THEN
          STROUT = 
     .     '  TRIGONAL II  3_BAR (??) RHOMBOHEDRAL AXES sgs   146'
        ELSE IF (KLASS.EQ.8) THEN
          STROUT = 
     .      ' TRIGONAL III  3_BAR1M (PG312)       sgs 149,151,153 '
        ELSE IF (KLASS.EQ.9) THEN
          STROUT = ' TRIGONAL IV  3_BARM1 (PG321)HEXAGONAL AXES '//
     +         ' sgs 150,152,154,155'
        ELSE IF (KLASS.EQ.10) THEN
          STROUT = '  TRIGONAL V  3_BARM1 (??)RHOMBOHEDRAL AXES sgs 155'
        ELSE IF (KLASS.EQ.11) THEN
          STROUT = '  HEXAGONAL    I  6/M  (PG6)   sgs        168 - 173'
        ELSE IF (KLASS.EQ.12) THEN
          STROUT = '  HEXAGONAL   II  6/MMM (PG622)    sgs 177 - 182'
        ELSE IF (KLASS.EQ.13) THEN
          STROUT ='  CUBIC        I  M3_BAR (PG23)    sgs 195 - 199'
        ELSE IF (KLASS.EQ.14) THEN 
          STROUT = '  CUBIC       II  M3_BARM (PG432)  sgs 207 - 214'
        ELSE IF (KLASS.EQ.15) THEN
          STROUT = '  MONOCLINIC  II  2/M (PG2c)  C UNIQUE sgs 3 -   5'
        ELSE IF (KLASS.EQ.16) THEN
          STROUT = '  MONOCLINIC III  2/M (PG2a)  A UNIQUE sgs 3 -   5'
        END IF
        CALL PUTLIN(STROUT,'CURWIN')
      ENDIF
C
      END
C
C   
C     
C     ====================================
      SUBROUTINE PGNLAU(NAMPG,NLAUE,LAUNAM)
C     ====================================
C     
C---- Choose Laue group from PG name.
C     
C     On entry:
C     NAMPG      point-group name (Int Tab A or `CCP4' (from PGDEFN))
C     
C     On exit:
C     NLAUE     Laue group number
C     LAUNAM    Laue group name (Int Tab A)
C     
C     .. Scalar Arguments ..
      INTEGER NLAUE
      CHARACTER NAMPG*(*), LAUNAM*(*)
C     ..
      INTEGER LPG
      CHARACTER LOCNAM*12
      INTEGER LENSTR
      EXTERNAL LENSTR
      INTEGER HRNG0,HRNG1,KRNG0,KRNG1,LRNG0,LRNG1
      COMMON/HKLLMS/HRNG0,HRNG1,KRNG0,KRNG1,LRNG0,LRNG1
C     ..
C     
      HRNG0 = -9999
      KRNG0 = -9999
      LRNG0 = -9999
      HRNG1 =  9999
      KRNG1 =  9999
      LRNG1 =  9999
      NLAUE = 0
      LOCNAM = NAMPG
C     be case-insensitive
      CALL CCPUPC (LOCNAM)
C     Strip off 'PG' if present
      IF (NAMPG(1:2) .EQ. 'PG') LOCNAM = NAMPG(3:)
      LPG = LENSTR(LOCNAM)
C     
C     Rarest first
C     
C     Cubic
C     14 pg23   pgm3bar - Laue m3
C     15 pg432  pg4bar3m pgm3barm - Laue m3m
C     
C  14 pg23    m3         hkl:h>=0, k>=0, l>=0 with l>=h,  k>=h
C     pgm3bar
      IF (LOCNAM.EQ.'23'
     +     .OR.LOCNAM.EQ.'M3BAR') THEN
        NLAUE = 14
        LAUNAM = 'm3bar'
        HRNG0 = 0
        KRNG0 = 0
        LRNG0 = 0
C  15 pg432   m3m        hkl:h>=0, k>=0, l>=0  with  k>=l
C     pg4bar3m pgm3barm
      ELSE IF (LOCNAM.EQ.'432'
     +       .OR.LOCNAM.EQ.'4BAR3M'
     +       .OR.LOCNAM.EQ.'M3BARM') THEN
        NLAUE = 15
        HRNG0 = 0
        KRNG0 = 0
        LRNG0 = 0
        LAUNAM = 'm3barm'
C       
C----   8 pg422 pg4mm pg4bar2m pg4barm2 - Laue 4/mmm
C       11 pg321 pg32 pg3m1 pg3barm1 pg3m - Laue  3/m
C       hkl:h>=0, k>=0 with k<=h for all l.
C       if h = k  l>=0
C       Space group numbers :   150-152-154
C       13 pg622 pg6mm pg6bar2m pg6barm2 pg6/mmm - Laue 6/mmm
C       
C   8 pg422   4/mmm       hkl:h>=0, k>=0, l>=0            89..
C     pg4mm pg4bar2m pg4barm2 pg4/mmm
      ELSE IF (LOCNAM.EQ.'422'
     +       .OR. LOCNAM.EQ.'4/MMM'
     +       .OR.LOCNAM.EQ.'4MM'
     +       .OR.LOCNAM.EQ.'4BAR2M'
     +       .OR.LOCNAM.EQ.'4BARM2') THEN
        HRNG0 = 0
        KRNG0 = 0
        LRNG0 = 0
        NLAUE = 8
        LAUNAM = '4/mmm'
C  11 pg321   3bar1m     hkl:h>=0, k>=0 with k<=h for all l.
C     pg31m pg3bar1m      if h = k  l>=0
C           Space group numbers :   150-152-154
      ELSE IF (LOCNAM.EQ.'321'
     +       .OR.LOCNAM.EQ.'32'
     +       .OR.LOCNAM.EQ.'3M1'
     +       .OR.LOCNAM.EQ.'3BARM1'
     +       .OR. LOCNAM.EQ.'3BARM'
     +       .OR.LOCNAM.EQ.'3M') THEN
        NLAUE = 11
        HRNG0 = 0
        KRNG0 = 0
        LAUNAM = '3barm'
C  13 pg622   6/mmm       hkl:h>=0, k>=0, l>=0 with h>=k 177..
C     pg6mm pg6barm2 pg6bar2m  pg 6/mmm
      ELSE IF (LOCNAM.EQ.'622'
     +       .OR.LOCNAM.EQ.'6MM'
     +       .OR.LOCNAM.EQ.'6BAR2M'
     +       .OR.LOCNAM.EQ.'6BARM2'
     +       .OR.LOCNAM.EQ.'6/MMM') THEN
        NLAUE = 13
        HRNG0 = 0
        KRNG0 = 0
        LRNG0 = 0
        LAUNAM = '6/mmm'
C       
C       10 pg312  pg31m pg3bar1m - Laue 3bar1m 
C       hkl:h>=0, k>=0 with k<=h for all l.
C       if k = 0  l>=0
C       Space group numbers :   149-151-153
C       
C  10 pg312   3/m        hkl:h>=0, k>=0 with k<=h for all l.
C     pg32 pg3m pg3m1 pg3barm1 if k = 0  l>=0
C           Space group numbers :   149-151-153 157 159 162 163
      ELSE IF (LOCNAM.EQ.'312'
     +       .OR.LOCNAM.EQ.'31M'
     +       .OR.LOCNAM.EQ.'3BAR1M') THEN
        NLAUE = 10
        HRNG0 = 0
        KRNG0 = 0
        LAUNAM = '3bar1m'
C   6 pg222   mmm        hkl:h>=0, k>=0, l>=0            16 ...
C     pgmm2 pgmmm
      ELSE IF (LOCNAM.EQ.'222'
     +       .OR.LOCNAM.EQ.'MMM'
     +       .OR.LOCNAM.EQ.'MM2'
     +       .OR.LOCNAM.EQ.'2MM'
     +       .OR.LOCNAM.EQ.'M2M') THEN
        NLAUE = 6
        HRNG0 = 0
        KRNG0 = 0
        LRNG0 = 0
        LAUNAM = 'mmm'
C
C----   5 pg2 pgm pg2/m - Laue   2/m    C axis unique
C
C   5 pg2     2/m        hkl:k>=0, l>=0  h0l:h>=0       1003,1004
C     pgm pg2/m
      ELSE IF (LOCNAM.EQ.'2C') THEN
        NLAUE = 5
        HRNG0 = 0
        KRNG0 = 0
        LAUNAM = '2/m'
C       
C----   4 pg2 pgm pg2/m - Laue   2/m
C       
C   4 pg2     2/m        hkl:k>=0, l>=0  hk0:h>=0       3/b,4/b....
C     pgm pg2/m
      ELSE IF (LOCNAM.EQ.'2'
     +       .OR.LOCNAM.EQ.'M'
     +       .OR.LOCNAM.EQ.'2/M') THEN
        NLAUE = 4
        KRNG0 = 0
        LRNG0 = 0
        LAUNAM = '2/m'
C       
C----   3 pg1     1bar
C       7 pg4    4/m
C       9  pg3     3bar
C       12 pg6    6/m
C       
C   3 pg1     1bar      hkl:l>=0  hk0:h>=0  0k0:k>=0   1,2
C     pg1bar
      ELSE IF (LOCNAM.EQ.'1'
     +       .OR.LOCNAM.EQ.'1BAR') THEN
        NLAUE = 3
C       why not `1bar' here (etc.)?
        LRNG0 = 0
        LAUNAM = '-1'
      ELSE IF (LOCNAM.EQ.'3'
     +       .OR.LOCNAM.EQ.'3BAR') THEN
        NLAUE = 9
        HRNG0 = 0
        KRNG0 = 0
        LAUNAM = '-3'
C   7 pg4     4/m        hkl:h>=0, l>=0 with k>=0 if  h=0  and
C     pg4bar pg4/m                            k>0 if h>0
      ELSE IF (LOCNAM.EQ.'4'
     +       .OR.LOCNAM.EQ.'4/M'
     +       .OR.LOCNAM.EQ.'4BAR') THEN
        NLAUE = 7
        HRNG0 = 0
        KRNG0 = 0
        LRNG0 = 0
        LAUNAM = '4/m'
C  12 pg6     6/m        hkl:h>=0, k>=0, l>=0 with k>=0 if  h=0
C     pg6bar  6/m        and k> 0 if h>0
      ELSE IF (LOCNAM.EQ.'6'
     +       .OR.LOCNAM.EQ.'6/M'
     +       .OR.LOCNAM.EQ.'6BAR') THEN
        NLAUE = 12
        LAUNAM = '6/M'
        HRNG0 = 0
        KRNG0 = 0
        LRNG0 = 0
      END IF
C     
      IF (NLAUE.EQ.0)           CALL CCPERR(1,
     +  'You have not defined PG name properly')
C     
      END
C
C   
C     
C     ==============================================================
      SUBROUTINE HKLRANGE(IHRNG0,IHRNG1,IKRNG0,IKRNG1,ILRNG0,ILRNG1)
C     ==============================================================
C     
C---- Return HKL ranges chosen in PGNLAUE
C     
       INTEGER IHRNG0,IHRNG1,IKRNG0,IKRNG1,ILRNG0,ILRNG1
       INTEGER HRNG0,HRNG1,KRNG0,KRNG1,LRNG0,LRNG1
       COMMON/HKLLMS/HRNG0,HRNG1,KRNG0,KRNG1,LRNG0,LRNG1
C     ..
C     
       IHRNG0 =  HRNG0
       IKRNG0 =  KRNG0
       ILRNG0 =  LRNG0
       IHRNG1 =  HRNG1
       IKRNG1 =  KRNG1
       ILRNG1 =  LRNG1
C     
      END
C
C
C
      SUBROUTINE ASUSET(
     .     SPGNAM,NUMSGP,PGNAME,MSYM,RRSYM,MSYMP,MLAUE,LPRINT)
C     ========================================================
C
C  Set up & store symmetry for later use in ASUPUT or ASUGET
C
C  On input:
C    SPGNAM  space-group name (not used)
C    NUMSGP  space-group number (not used)
C    PGNAME  point-group name (if returned from SYMOP.LIB)
C    MSYM    total number of symmetry operations
C    RRSYM(4,4,MSYM) symmetry matrices (real-space)
C
C  On output:
C    PGNAME  point-group name
C    MSYMP   number of primitive symmetry operations
C    MLAUE   Laue group number
C
C Arguments:
      INTEGER NUMSGP, MSYM, MSYMP, MLAUE
      REAL    RRSYM(4,4,192) 
ccMSYM)
      CHARACTER*(*) SPGNAM, PGNAME
      LOGICAL LPRINT
C
C Common blocks
C
C Common block /RECSYM/
C   RSYM     real-space symmetry operators
C   RSYMIV   their inverse
C   NSYM     number of symmetry operations
C   NSYMP    number of primitive symmetry operations
C   NLAUE    number of Laue group
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
      COMMON /RECSYM/RSYM(4,4,MAXSYM),RSYMIV(4,4,MAXSYM),
     .    NSYM,NSYMP,NLAUE
      INTEGER NSYM,NSYMP,NLAUE
      REAL RSYM,RSYMIV
      SAVE /RECSYM/
C
C Functions
      INTEGER LENSTR
C Locals
      INTEGER I, L
      CHARACTER*100 STROUT, NAME*8, LAUNAM*8
C
C Get point group and primitive-only operations from symmetry matrices
      CALL PGDEFN(PGNAME,MSYMP,MSYM,RRSYM,.FALSE.)
C
C Get Laue group number
      CALL PGNLAU(PGNAME,MLAUE,LAUNAM)
C
C Store in common block
      NSYM = MSYM
      NSYMP = MSYMP
      NLAUE = MLAUE
      DO 10, I=1,NSYM
        CALL CCPMVI(RSYM(1,1,I), RRSYM(1,1,I), 16)
C Invert all symmetry operators
        CALL INVSYM(RSYM(1,1,I), RSYMIV(1,1,I))
 10   CONTINUE
C
C Print
      IF (LPRINT) THEN
        CALL BLANK('CURWIN',1)
        CALL PUTLIN('          Reciprocal space symmetry','CURWIN')
        NAME(1:) = PGNAME
        L = LENSTR(NAME)
        IF (NAME(1:2) .EQ. 'PG') NAME = NAME(3:L)
        L = LENSTR(NAME)
        I = MIN(20,LENSTR(SPGNAM))
        J = LENSTR(LAUNAM)
        WRITE (STROUT, 6001) 
     .         SPGNAM(1:I),NUMSGP,NAME(1:L),LAUNAM(1:J)
 6001   FORMAT('    Space group: ',A,' (',I3,')',5X,
     .    'Point group: ',A,5X,'Laue group: ',A)
        CALL PUTLIN(STROUT,'CURWIN')
C        
        IF (NLAUE .EQ. 3) THEN
          STROUT = '[-1] hkl:l>=0  hk0:h>=0  0k0:k>=0'
        ELSEIF (NLAUE .EQ. 4) THEN
          STROUT = '[2/m] hkl:k>=0, l>=0  hk0:h>=0'
        ELSEIF (NLAUE .EQ. 5) THEN
          STROUT = '[2/m] hkl:k>=0, l>=0  h0l:h>=0'
        ELSEIF (NLAUE .EQ. 6) THEN
          STROUT = '[mmm] hkl:h>=0, k>=0, l>=0'
        ELSEIF (NLAUE .EQ. 7) THEN
          STROUT = '[4/m] hkl:h>=0, l>=0 with k>=0 if h=0'//
     .       ' and k>0 if h>0'
        ELSEIF (NLAUE .EQ. 8) THEN
          STROUT = '[4/mmm] hkl:h>=0, k>=0, l>=0 and h>=k'
        ELSEIF (NLAUE .EQ. 9) THEN
          STROUT = '[-3] hkl:h>=0, k>0  00l:l>0'
        ELSEIF (NLAUE .EQ. 10) THEN
          STROUT = '[312] hkl:h>=0, k>=0 with k<=h '//
     .          'for all l. If h = 0  l>=0'
        ELSEIF (NLAUE .EQ. 11) THEN
          STROUT = '[321] hkl:h>=0, k>=0 with k<=h '//
     .          'for all l. If h = k  l>=0'
        ELSEIF (NLAUE .EQ. 12) THEN
          STROUT = '[6/m] hkl:h>=0, k>=0, l>=0 with k>=0'//
     .          ' if h=0, and k> 0 if h>0'
        ELSEIF (NLAUE .EQ. 13) THEN
          STROUT = '[6/mmm] hkl:h>=0, k>=0, l>=0 with h>=k'
        ELSEIF (NLAUE .EQ. 14) THEN
          STROUT = '[m3] hkl:h>=0, k>=0, l>=0 with l>=h,'//
     .          ' k>=h if l=h, k> h if l>h'
        ELSEIF (NLAUE .EQ. 15) THEN
          STROUT = 
     .          '[m3m] hkl:h>=0, k>=0, l>=0 with k>=l, and l>=h'
        ELSE
          WRITE(STROUT, 6020) ' **** Illegal Laue group : ',NLAUE
 6020     FORMAT(1X,A,I6)
          CALL PUTLIN(STROUT,'CURWIN')
                    CALL CCPERR(1,'ASUSET: Fatal error')
        ENDIF
C
        CALL PUTLIN('Asymmetric unit: '//STROUT,'CURWIN')
C
C Print symmetry operations
        CALL PRTRSM(PGNAME, NSYMP, RSYMIV)
C
      ENDIF
C
      END
C
      SUBROUTINE ASUPUT(IHKL,JHKL,ISYM)
C     =================================
C
C Put reflection into asymmetric unit defined by call to ASUSET
C
C On input:
C    IHKL(3)    input indices hkl
C
C On output:
C    JHKL(3)    output indices hkl
C    ISYM       symmetry number for output
C                 odd  numbers are for I+
C                 even numbers are for I-
C               real-space symmetry operation number L = (ISYM-1)/2 + 1
C
C  The real-space symmetry matrices are applied by premultiplying them
C  by a row vector hkl,  ie  (h'k'l') = (hkl)R
C
C  Arguments
      INTEGER IHKL(3), JHKL(3), ISYM
C
C Common block /RECSYM/
C   RSYM     real-space symmetry operators
C   RSYMIV   their inverse
C   NSYM     number of symmetry operations
C   NSYMP    number of primitive symmetry operations
C   NLAUE    number of Laue group
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
      COMMON /RECSYM/RSYM(4,4,MAXSYM),RSYMIV(4,4,MAXSYM),
     .    NSYM,NSYMP,NLAUE
      INTEGER NSYM,NSYMP,NLAUE
      REAL RSYM,RSYMIV
      SAVE /RECSYM/
C
C Routines
      INTEGER INASU
      EXTERNAL INASU
C Locals
      INTEGER I,L,ISGN
      CHARACTER*100 LINERR
C
      DO 10, L=1,NSYMP
C  h' = h R   ie row vector h premultiplies symmetry matrix
        DO 20, I=1,3
          JHKL(I) = IHKL(1)*RSYM(1,I,L) + IHKL(2)*RSYM(2,I,L)
     .            + IHKL(3)*RSYM(3,I,L)
 20     CONTINUE
C Test this index against the asymmetric unit
C   Function INASU returns 0 if outside au, else +-1 depending on sign
C 
        ISGN = INASU(JHKL, NLAUE)
        IF (ISGN .NE. 0) GO TO 100
C Failed, try next symmetry
 10   CONTINUE
C
C Shouldn't get here, can't reduce reflection to asymmetric unit
      WRITE (LINERR,'(A,3I4,A)') 
     .    'ASUPUT: can''t put reflection ',IHKL,
     .     ' into asymmetric unit'
C Fatal error, stop in routine
      CALL LERROR(2,-1,LINERR)
C
C Succesful, multiply by sign
 100  DO 101, I=1,3
        JHKL(I) = JHKL(I)*ISGN
 101  CONTINUE
C 
      IF (ISGN .GT. 0) THEN
C I+, odd ISYM
        ISYM = L*2 - 1
      ELSE
C I-, even ISYM
        ISYM = L*2
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE ASUGET(IHKL,JHKL,ISYM)
C     =================================
C
C Get original indices of reflection from  asymmetric unit,
C   ie reverse operation of ASUPUT
C   symmetry defined by call to ASUSET
C
C On input:
C    IHKL(3)    input unique indices hkl
C    ISYM       symmetry number for output
C                 odd  numbers are for I+
C                 even numbers are for I-
C               real-space symmetry operation number L = (ISYM-1)/2 + 1
C
C On output:
C    JHKL(3)    output original indices hkl
C
C  The real-space symmetry matrices are applied in ASUPUT by 
C premultiplying them by a row vector hkl,  ie  (h'k'l') = (hkl)R
C  So here we calculate (hkl) = (h'k'l') R**-1
C
C  Arguments
      INTEGER IHKL(3), JHKL(3), ISYM
C
C Common block /RECSYM/
C   RSYM     real-space symmetry operators
C   RSYMIV   their inverse
C   NSYM     number of symmetry operations
C   NSYMP    number of primitive symmetry operations
C   NLAUE    number of Laue group
      INTEGER MAXSYM
      PARAMETER (MAXSYM=192)
      COMMON /RECSYM/RSYM(4,4,MAXSYM),RSYMIV(4,4,MAXSYM),
     .    NSYM,NSYMP,NLAUE
      SAVE /RECSYM/
      INTEGER NSYM,NSYMP,NLAUE
      REAL RSYM,RSYMIV
C
C Locals
      INTEGER I,L,ISGN
      CHARACTER*100 LINERR
C
C L is symmetry operation number
      L = (ISYM+1)/2
      IF (L .LT. 1 .OR. L .GT. NSYMP) THEN
        WRITE (LINERR,6001) ISYM, NSYMP
 6001   FORMAT(' ASUGET: illegal symmetry number ',I5,
     .       ' outside range of ',i5,' symmetry operators')
        CALL LERROR(2,-1,LINERR)
C  Stop in error routine
      ENDIF
C
C  Sign code +-1
      ISGN = MOD(ISYM,2)
      IF (ISGN.EQ.0) ISGN = -1
C
C  h' = h R   ie row vector h premultiplies symmetry matrix
        DO 20, I=1,3
          JHKL(I) = IHKL(1)*RSYMIV(1,I,L) + IHKL(2)*RSYMIV(2,I,L)
     .            + IHKL(3)*RSYMIV(3,I,L)
 20     CONTINUE
C
C  Multiply by sign
        DO 101, I=1,3
          JHKL(I) = JHKL(I)*ISGN
 101    CONTINUE
C 
      RETURN
      END
C
C
C
      INTEGER FUNCTION INASU(IH, NLAUE)
C     =================================
C
C   This used to be FUNCTION XRY173(NLAUE,IH,ISGN)
C***********************************************************************
C***********************************************************************
C*****                         *****************************************
C***** ROUTINE ( XRY173        *****************************************
C*****                         *****************************************
C*****     From XRAY77 or something like it ****************************
C***********************************************************************
C***********************************************************************
C
C  Arguments:
C   NLAUE - code number for this pointgroup
C   IH(3) - indices
C
C Returns:
C   INASU = +1  if  h k l chosen
C   INASU = -1  if -h-k-l chosen
C   INASU =  0   if reflection is out-of-bounds
C
C  Reciprocal space zones: please check spacegroup numbers.
C
Code:3 pg1     1bar      hkl:l>=0  hk0:h>=0  0k0:k>=0
C           Space group numbers :   1,2
Code:4 pg2    2/m        hkl:k>=0, l>=0  hk0:h>=0       3/b,4/b....
C           Space group numbers :   3 -15 B axis unique.
Code:5 pg2    2/m        hkl:k>=0, l>=0  h0l:>=0        1003,1004
C           Space group numbers :   3 -15 C axis unique.
Code:6 pg222  mmm        hkl:h>=0, k>=0, l>=0            16 ...
C           Space group numbers :   16-74
Code:7 pg4    4/m        hkl:h>=0, l>=0 with k>=0 if  h=0  and
C                                           k> 0 if  h>0.
C           Space group numbers :   75-88
Code:8 pg422 4/mmm       hkl:h>=0, k>=0, l>=0 and h>=k
C           Space group numbers :   89-142
Code:9  pg3     3bar      hkl:h>=0, k>0  00l:l>0
C           Space group numbers :   143-148
Code:  10 pg312  3/m        hkl:h>=0, k>=0 with k<=h for all l.
C                           if k = 0  l>=0
C           Space group numbers :   149-151-153
Code:  11 pg321  3/m        hkl:h>=0, k>=0 with k<=h for all l.
C                           if h = k  l>=0
C           Space group numbers :   150-152-154
Code:12 pg6    6/m        hkl:h>=0, k>=0, l>=0 with k>=0 if  h=0
C                                            and  k> 0 if  h>0.
C           Space group numbers :   168-176
Code:13 pg622 6/mmm       hkl:h>=0, k>=0, l>=0 with h>=k
C           Space group numbers :   177-194
Code:14 pg23   m3         hkl:h>=0, k>=0, l>=0 with l>=h,
C                                                 k>=h if l=h
C                                                 k> h if l>h.
C           Space group numbers :   195-206
Code:15 pg432  m3m        hkl:h>=0, k>=0, l>=0 with k>=l, and l>=h.
C           Space group numbers :   207-232
C
C -- TEST FOR HKL IN ASYMMETRIC UNIT
C     INCORPORATED INTO DATCO5 JUNE 70
C
C Arguments
      INTEGER IH(3), NLAUE
C Locals
      INTEGER J,K,L,ISGN
C
C----MOVE INDICES INTO J,K, AND L
      J = IH(1)
      K = IH(2)
      L = IH(3)
      ISGN=1
C----GO TO TEST IF INDICES ARE IN DESIRED UNIT
1     GO TO (100,150,200,250,300,350,400,450,550,570,600,650,700,750,
     1  800),NLAUE
 100  CONTINUE
C----1 BAR (ALTERNATE 1)
C     HKL --    H.GE.0
C     0KL --    K.GE.0
C     00L --    L.GE.0
      IF (J) 7,105,5
 105  IF (K) 7,110,5
 110  IF (L) 7,5,5
C
 150  CONTINUE
C----1 BAR (ALTERNATE I)
C     HKL --    K.GE.0
C     H0L --    L.GE.0
C     H00 --    H.GE.0
      IF (K) 7,155,5
 155  IF (L) 7,160,5
 160  IF (J) 7,5,5
C
C  Corresponds to Data reduction unique set for  pg1
C   3 pg1     1bar      hkl:l>=0  hk0:h>=0  0k0:k>=0   1,2
 200  CONTINUE
C----1 BAR (ALTERNATE 3)
C     HKL --    L.GE.0
C     HK0 --    H.GE.0
C     0K0 --    K.GE.0
      IF (L) 7,205,5
 205  IF (J) 7,210,5
 210  IF (K) 7,5,5
C
C  Corresponds to Data reduction unique set for  pg2
C   4 pg2    2/m        hkl:k>=0, l>=0  hk0:h>=0       3/b,4/b....
 250  CONTINUE
C----2/M (ALTERNATE 1)
C     HKL --    K.GE.0 AND L.GE.0
C     HK0 --    H.GE.0
      IF (K) 7,255,255
 255  IF (L) 7,260,5
 260  IF (J) 7,5,5
C
C  Corresponds to Data reduction unique set for  pg2
C   5 pg2    2/m        hkl:k>=0, l>=0  h0l:h>=0       1003,1004
 300  CONTINUE
C----2/M (ALTERNATE 2)
C     HKL --    L.GE.0 AND H.GE.0
C     H0L --    H.GE.0
      IF (L) 7,305,305
 305  IF (K) 7,310,5
 310  IF (J) 7,5,5
C
C  Corresponds to Data reduction unique set for  pg222
C   6 pg222  mmm        hkl:h>=0, k>=0, l>=0            16 ...
 350  CONTINUE
C----MMM
C     HKL --    H.GE.0, K.GE.0, AND L.GE.0
      IF (J) 7,355,355
 355  IF (K) 7,361,361
C****NO 3-SICKLIES PLEASE
 361  IF (L) 7,5,5
C
C  Corresponds to Data reduction unique set for  pg4
C   7 pg4    4/m        hkl:h>=0, l>=0 with k>=0 if  h=0  and
C                                           k> 0 if  h>0.
 400  CONTINUE
C----4/M
C     HKL --    H.GE.0, L.GE.0, WITH K.GE.0 IF H.EQ.0 OR
C               K.GE.1 IF H.GT.0
      IF (L) 7,405,405
 405  IF (J) 7,410,415
 410  IF (K) 7,5,5
 415  IF (K) 7,7,5
C
C  Corresponds to Data reduction unique set for  pg422
C   8 pg422 4/mmm       hkl:h>=0, k>=0, l>=0 and h>=k   89..
 450  CONTINUE
C----4/MMM
C     HKL --    H.GE.0, K.GE.0, AND L.GE.0 WITH H.GE.K
      IF (J) 7,455,455
 455  IF (K) 7,460,460
 460  IF (L) 7,465,465
 465  IF (J-K) 7,5,5
C
C 500  CONTINUE
C----3 BAR
C      HKL ---   H.LE.0, L.GE.0, WITH K.GE.0 IF H.LE.0 OR
C               K.GE.1 IF H.LT.0
C      HK0  --  K.GT.-H
CC
C  1978 CHANGED TO FIT N.ISAACS
C  INCLUDE 0 0 L,  HK0 K.LT.0 H.GE.-K     HKL H.GE.0 K.LT.0
CC
CCC      IF(J)7,505,505
CCC505   IF(K)510,510,7
CCC510   IF(L)7,515,520
CCC515   IF(K)516,7,7
CCC516   IF(J+K)7,5,5
CCC520   IF(J-K)521,5,521
CCC521   IF(K)5,7,7
C
C  Corresponds to Data reduction unique set for  pg3
C  9 pg3     3bar      hkl:h>=0, k>0  00l:l>0         143..
550   CONTINUE
C   ALTERNATIVE FOR R3
C  H.GE.0 K.GT.0    ALL L
C  H=0 K=0 L.GT.0
      IF(J)7,555,556
555   IF(K)7,557,5
556   IF(K)7,7,5
557   IF(L)7,7,5
C
C  Corresponds to Data reduction unique set for  pg312 
C  10 pg312        hkl:h>=0, k>=0 with k<=h for all l.
C                           if k = 0  l>=0
 570  CONTINUE
C----3 BAR M
C  H.GE.0 K.GE.0 K.LE.H   ALL L
C  H=K  L.GE.0
      IF(J)7,575,575
575   IF(K)7,577,576
576   IF(J-K)7,5,5
577   IF(L)7,5,5
C
C  Corresponds to Data reduction unique set for  pg321
C  11 pg321        hkl:h>=0, k>=0 with k<=h for all l.
C                           if h = k  l>=0
 600  CONTINUE
C----3 BAR M
C  H.GE.0 K.GE.0 K.LE.H   ALL L
C  H=K  L.GE.0
      IF(J)7,605,605
605   IF(K)7,606,606
606   IF(J-K)7,607,5
607   IF(L)7,5,5
C
C  Corresponds to Data reduction unique set for  pg6
C  12 pg6    6/m        hkl:h>=0, k>=0, l>=0 with k>=0 if  h=0
C                                            and  k> 0 if  h>0.
 650  CONTINUE
C----6/M
C     HKL --    H.GE.0, L.GE.0, WITH K.GE.0 IF H.EQ.0 OR
C               K.GE.1 IF H.GT.0
      IF (L) 7,655,655
 655  IF (J) 7,660,665
 660  IF (K) 7,5,5
 665  IF (K) 7,7,5
C
C  Corresponds to Data reduction unique set for  pg622
C  13 pg622 6/mmm       hkl:h>=0, k>=0, l>=0 with h>=k 177..
  700 CONTINUE
C----6/MMM
C     HKL --    H.GE.0, K.GE.0, AND L.GE.0 WITH H.GE.K
      IF (J) 7,705,705
 705  IF (K) 7,710,710
710   IF(J-K)7,715,715
715   IF(L)7,5,5
C
C  Corresponds to Data reduction unique set for  pg23
C  14 pg23   m3         hkl:h>=0, k>=0, l>=0 with l>=h,
C                                                 k>=h if l=h
C                                                 k> h if l>h.
 750  CONTINUE
C----M3
C     HKL --    H.GE.0, K.GE.0, AND L.GE.0 WITH L.GE.H AND WITH
C               K.GE.H IF L.EQ.H OR K.GT.H IF L.GT.H
      IF (J) 7,755,755
 755  IF (K) 7,760,760
 760  IF (L) 7,765,765
 765  IF (L-J) 7,770,775
 770  IF (K-J) 7,5,5
 775  IF (K-J) 7,7,5
C
C  Corresponds to Data reduction unique set for  pg432
C  15 pg432  m3m        hkl:h>=0, k>=0, l>=0 with k>=l, and l>=h.
 800  CONTINUE
C----M3M
C     HKL --    H.GE.0, K.GE.0, AND L.GE.0 WITH
C              K.GE.L AND L.GE.H
      IF (J) 7,805,805
 805  IF (K) 7,810,810
 810  IF (L) 7,815,815
 815  IF (K-L) 7,820,820
 820  IF (L-J) 7,5,5
C
C----REFLECTION IS IN BOUNDS
 5    INASU = ISGN
      RETURN
C     ======
C
C----REFLECTION IS OUT OF BOUNDS
 7    IF (ISGN .EQ. +1) THEN
C     TRY -H -K -L
        ISGN=-1
        J=-J
        K=-K
        L=-L
        GO TO 1
      ENDIF
C Index failed, exit
      INASU = 0
      RETURN
C     ======
C
      END
C
      SUBROUTINE PRTRSM(PGNAME, NSYMP, RSYMIV)
C     ========================================
C
C Print reciprocal space symmetry operations
C
C PGNAME              point-group name
C NSYMP               number of primitive symmetry operators
C RSYMIV(4,4,NSYMP)   inverse real-space symmetry matrices
C
C  The real-space symmetry matrices are applied by premultiplying them
C  by a row vector hkl,  ie  (h'k'l') = (hkl)R
C
C  Arguments
      INTEGER NSYMP
      REAL RSYMIV(4,4,*)
C
      CHARACTER*(*) PGNAME
C
C Locals
      INTEGER I,J,K,L,M,ISYM,ISGN,LP,NLINC,NLMAX
      CHARACTER LINE*80, HKL(3)*1
      DATA   HKL/'h','k','l'/
C
      CALL BLANK('CURWIN',1)
C
      CALL PUTLIN('Original indices for reflection hkl with '//
     .     'symmetry number ISYM','CURWIN')
C
      NLINC = (NSYMP+3)/4
C
C Loop positive & negative operations
C
      DO 100, ISGN = +1, -1, -2
        IF (ISGN .EQ. +1) THEN
          LINE = '                             Bijvoet positive'
        ELSE
          LINE = '                             Bijvoet negative'
        ENDIF
        CALL BLANK('CURWIN',1)
        CALL PUTLIN(LINE,'CURWIN')
C
        LINE = ' '
        NLMAX = MIN(NSYMP,1+NLINC*3)
        L = 7
        DO 105, K=1,NLMAX,NLINC
          LINE(L:) = 'ISYM'
          L = L+18
 105    CONTINUE
        CALL PUTLIN(LINE,'CURWIN')
C Loop symmetry 4 / line
        DO 110, L = 1,(NSYMP+3)/4
          LINE = ' ISYM'
          NLMAX = MIN(NSYMP,L+NLINC*3)
          DO 120, K = L, NLMAX, NLINC
C   LP is character pointer in LINE
            LP = ((K-L)/NLINC)*18 + 7
C   ISYM is symmetry number, odd for I+, even for I-
            ISYM = K*2
            IF (ISGN .EQ. +1) ISYM = ISYM - 1
            WRITE(LINE(LP:),'(I3)') ISYM
            LP = LP+4
            DO 130, J=1,3
              DO 140, I=1,3
C     Extract matrix element as +-1
                M = NINT(RSYMIV(I,J,K))*ISGN
C     Add +-h,k,l to line if M .ne. 0
                IF (M .LT. 0) THEN
                  LINE(LP+1:LP+2) = '-'//HKL(I)
                  LP = LP+2
                ELSEIF (M .GT. 0) THEN
                  LINE(LP+1:LP+2) = '+'//HKL(I)
                  LP = LP+2
                ENDIF
 140          CONTINUE
C add ','
              LP = LP+1
              IF (J .LT. 3) LINE(LP:LP) = ','
 130        CONTINUE
 120      CONTINUE
          CALL PUTLIN(LINE,'CURWIN')
 110    CONTINUE
 100  CONTINUE
      CALL BLANK('CURWIN',1)
      RETURN
      END
C
C
C     ================================================
      SUBROUTINE ASUPHP(JHKL,LSYM,ISIGN,PHASIN,PHSOUT)
C     ================================================
C
C---- Generate phase of symmetry equivalent JHKL from that of IHKL
C
C     On input:
C
C    JHKL(3)    indices hkl generated in ASUPUT
C    LSYM       symmetry number for generating JHKL
C    ISIGN         =  1   for I+
C                  = -1   for I-
C    PHASIN     phase for reflection IHKL(3)
C
C     On output:
C
C    PHSOUT     phase for reflection JHKL(3)
C
C  The real-space symmetry matrices are applied by premultiplying them
C  by a row vector hkl,  ie  (h'k'l') = (hkl)R
C
C Common block /RECSYM/
C   RSYM     real-space symmetry operators
C   RSYMIV   their inverse
C   NSYM     number of symmetry operations
C   NSYMP    number of primitive symmetry operations
C   NLAUE    number of Laue group
C
C     .. Parameters ..
      INTEGER           MAXSYM
      PARAMETER         (MAXSYM=192)
C     ..
C     .. Scalar Arguments ..
      REAL              PHASIN,PHSOUT
      INTEGER           ISIGN,LSYM
C     ..
C     .. Array Arguments ..
      INTEGER           JHKL(3)
C     ..
C     .. Scalars in Common ..
      INTEGER           NLAUE,NSYM,NSYMP
C     ..
C     .. Arrays in Common ..
      REAL              RSYM,RSYMIV
C     ..
C     .. Local Scalars ..
      REAL              PHASCH
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         MOD
C     ..
C     .. Common blocks ..
      COMMON            /RECSYM/RSYM(4,4,MAXSYM),RSYMIV(4,4,MAXSYM),
     +                  NSYM,NSYMP,NLAUE
C     ..
      SAVE
C
C---- Get symmetry operator and sign from isym
C
      PHSOUT = PHASIN
C
C----  phase = -phase for Friedel equivalent
C
      IF (ISIGN.EQ.-1) PHSOUT = -PHASIN
C
C----  Phase change taken from old CADLCF
C
C      PHASCH=360.*(IHD*rot(1,4,ISYM)+IKD*rot(2,4,ISYM)
C     1 +ILD*Rot(3,4,ISYM))
C
      PHASCH = (JHKL(1)*RSYM(1,4,LSYM)+JHKL(2)*RSYM(2,4,LSYM)+
     +         JHKL(3)*RSYM(3,4,LSYM))*360.0
      PHSOUT = PHSOUT + PHASCH
C
C----  Put phase in range 0 to 360
C
      PHSOUT = MOD(PHSOUT+36000.0,360.0)
      RETURN
      END
C
      SUBROUTINE PATSGP(SPGNAM, PGNAME, PATNAM, LPATSG)
C     =================================================
C
C Determine Patterson spacegroup from true space-group
C
C On entry:
C     SPGNAM    space-group name. Only used to determine lattice centering
C     PGNAME    point-group name
C
C On exit:
C     PATNAM    name of Patterson spacegroup
C     LPATSG    number of Patterson spacegroup
C
      CHARACTER*(*) SPGNAM, PGNAME, PATNAM
      INTEGER LPATSG
C
      CHARACTER NMPG*8
C
C Strip off 'PG' is present
      IF (PGNAME(1:2) .EQ. 'PG') THEN
         NMPG = PGNAME(3:)
      ELSE
         NMPG = PGNAME
      ENDIF
C
C    Patterson space groups
C      P-1     PG1
C      P2/m    PG2
C      C2/m    PG2
C      Pmmm    PG222
C      Cmmm    PG222
C      Fmmm    PG222
C      Immm    PG222
C      P4/m    PG4
C      I4/m    PG4
C      P4/mmm  PG4/mmm
C      I4/mmm  PG4/mmm
C      P-3     PG3
C      R-3     PG3
C      P-31m   PG312
C      P-3m1   PG321
C      R-3m    PG321
C      P6/m    PG622
C      P6/mmm  PG622
C      Pm-3    PG23
C      Fm-3    PG23
C      Im-3    PG23
C      Pm-3m   PG432
C      Fm-3m   PG432
C      Im-3m   PG432
C
      IF (NMPG.EQ.'1'        .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 2
         PATNAM = 'P-1'
      ELSEIF (NMPG.EQ.'2'    .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 10
         PATNAM = 'P2/m'
      ELSEIF (NMPG.EQ.'2'    .AND. SPGNAM(1:1).EQ.'C') THEN
         LPATSG = 12
         PATNAM = 'C2/m'
      ELSEIF (NMPG.EQ.'222'  .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 47
         PATNAM = 'Pmmm'
      ELSEIF (NMPG.EQ.'222'  .AND. SPGNAM(1:1).EQ.'C') THEN
         LPATSG = 65
         PATNAM = 'Cmmm'
      ELSEIF (NMPG.EQ.'222'  .AND. SPGNAM(1:1).EQ.'F') THEN
         LPATSG = 69
         PATNAM = 'Fmmm'
      ELSEIF (NMPG.EQ.'222'  .AND. SPGNAM(1:1).EQ.'I') THEN
         LPATSG = 71
         PATNAM = 'Immm'
      ELSEIF (NMPG.EQ.'4'    .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 83
         PATNAM = 'P4/m'
      ELSEIF (NMPG.EQ.'4'    .AND. SPGNAM(1:1).EQ.'I') THEN
         LPATSG = 87
         PATNAM = 'I4/m'
      ELSEIF (NMPG.EQ.'422'  .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 123
         PATNAM = 'P4/mmm'
      ELSEIF (NMPG.EQ.'422'  .AND. SPGNAM(1:1).EQ.'I') THEN
         LPATSG = 139
         PATNAM = 'I4/mmm'
      ELSEIF (NMPG.EQ.'3'    .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 147
         PATNAM = 'P-3'
      ELSEIF (NMPG.EQ.'3'    .AND. SPGNAM(1:1).EQ.'R') THEN
         LPATSG = 148
         PATNAM = 'R-3'
      ELSEIF (NMPG.EQ.'312'  .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 162
         PATNAM = 'P-31m'
      ELSEIF (NMPG.EQ.'321'  .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 164
         PATNAM = 'P-3m1'
      ELSEIF (NMPG.EQ.'321'  .AND. SPGNAM(1:1).EQ.'R') THEN
         LPATSG = 166
         PATNAM = 'R-3m'
      ELSEIF (NMPG.EQ.'6'    .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 175
         PATNAM = 'P6/m'
      ELSEIF (NMPG.EQ.'622'  .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 191
         PATNAM = 'P6/mmm'
      ELSEIF (NMPG.EQ.'23'   .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 200
         PATNAM = 'Pm-3'
      ELSEIF (NMPG.EQ.'23'   .AND. SPGNAM(1:1).EQ.'F') THEN
         LPATSG = 202
         PATNAM = 'Fm-3'
      ELSEIF (NMPG.EQ.'23'   .AND. SPGNAM(1:1).EQ.'I') THEN
         LPATSG = 204
         PATNAM = 'Im-3'
      ELSEIF (NMPG.EQ.'432'  .AND. SPGNAM(1:1).EQ.'P') THEN
         LPATSG = 221
         PATNAM = 'Pm-3m'
      ELSEIF (NMPG.EQ.'432'  .AND. SPGNAM(1:1).EQ.'F') THEN
         LPATSG = 225
         PATNAM = 'Fm-3m'
      ELSEIF (NMPG.EQ.'432'  .AND. SPGNAM(1:1).EQ.'I') THEN
         LPATSG = 229
         PATNAM = 'Im-3m'
      ELSE
         LPATSG = 0
         PATNAM = ' '
      ENDIF
C
      RETURN
      END


C
C     ==========================================================
      SUBROUTINE SETGRD(NLAUE,SAMPLE,NXMIN,NYMIN,NZMIN,NX,NY,NZ)
C     ==========================================================
C
C Set up a suitable sampling grid for FFT
C
C Input:
C     NLAUE         Laue-group for FFT/SF calculation
C     SAMPLE        default fineness of sample, ie if = 1.0 (minimum),
C                   try to get sampling as close to minimum as possible
C                   Typically = 1.5 to get sample at traditional
C                   3 * maximum index
C     NXMIN NYMIN NZMIN minimum sampling (true XYZ)
C
C Output:
C     NX,NY,NZ       sampling intervals along X,Y,Z
C
C  The sampling intervals must satisfying the following conditions:
C
C     1) approximately SAMPLE * minimum sampling
C     2) no prime factor .gt. 19
C     3) special restrictions for particular space-groups
C
C      IMPLICIT NONE
C
      INTEGER NLAUE,NXMIN,NYMIN,NZMIN,NX,NY,NZ
      REAL SAMPLE
      EXTERNAL FNDSMP
C
C  This is ALL the point groups.
C PG1 PG1bar PG2 PGm PG2/m PG222 PGmm2 PGmmm 
C PG4 PG4bar PG4/m PG422 PG4mm PG4bar2m PG4/mmm 
C PG3 PG3bar PG32 PG3m PG3barm 
C PG6 PG6bar PG6/m PG622 PG6mm PG6bar2m  PG6/mmm
C PG23 PGm/3bar PG432 PG4bar3m PGm3bar m
C
C  We use:
C PG1 PG1bar PG2  PG2/m PG222  PGmmm 
C PG4 PG4/m PG422 PG4/mmm 
C PG3 PG3bar PG32 PG3bar/m 
C PG6 PG6/m PG622 PG6/mmm
C PG23 PGm/3bar PG432 PGm3barm
C  For grid restrictions we only need to know the laue number.
C Here is the table:
C   3 pg1     1bar      hkl:l>=0  hk0:h>=0  0k0:k>=0   1,2
C   4 pg2    2/m        hkl:k>=0, l>=0  hk0:h>=0       3/b,4/b....
C   5 pg2(c) 2/m        hkl:k>=0, l>=0  h0l:h>=0       1003,1004
C   6 pg222  mmm        hkl:h>=0, k>=0, l>=0            16 ...
C   7 pg4    4/m        hkl:h>=0, l>=0 with k>=0 if  h=0  and
C   8 pg422 4/mmm       hkl:h>=0, k>=0, l>=0            89..
C   9 pg3     3bar      hkl:h>=0, k>0  00l:l>0         143..
C  10 pg312  3/m        hkl:h>=0, k>=0 with k<=h for all l.
C                           if k = 0  l>=0
C           Space group numbers :   149-151-153
C  11 pg321  3/m        hkl:h>=0, k>=0 with k<=h for all l.
C                           if h = k  l>=0
C           Space group numbers :   150-152-154
C  12 pg6    6/m        hkl:h>=0, k>=0, l>=0 with k=0 if  h=0
C  13 pg622  6/mmm
C  14 pg23   m3
C  15 pg432  m3m
C 
C Tables of restrictions for FFT Laue-groups
C  NRESTR(1,) lauegroup number
C        (2-4,) factors for NX,NY,NZ
      INTEGER MAXLAU
      PARAMETER (MAXLAU=15)
      INTEGER NRESTR(4,MAXLAU), I
      DATA NRESTR/
C          Nsg    NX NY NZ
C     P1 or P1bar - disallowed:
     $     -1,    2, 2, 2,
C     P1 or P1bar - disallowed:
     $     -2,    2, 2, 2,
C     P1 or P1bar :
     $      3,    2, 2, 2,
C     P2 or P2bar :
     $      4,    2, 4, 2,
C     P2/m:
     $      5,    2, 8, 4,
C     P222 or Pmmm:
     $      6,    4, 4, 4,
C     P4   or P4/m:
     $      7,    4, 4, 8,
C     P422 or P4/mmm:
     $      8,    4, 4, 8,
C     P3   or P3bar:
     $      9,    6, 6, 6,
C     P32  or P3/m:
     $     10,    6, 6, 6,
C     P32  or P3/m:
     $     11,    6, 6, 6,
C     P6   or P6/m:
     $     12,    6, 6,12,
C     P6222   or P6/mmmm:
     $     13,    6, 6,12,
C     P23:
     $     14,    4, 4, 4,
C     P432 or Pmmm:
     $     15,    8, 8, 8/
C
      DO 1, I=1,MAXLAU
         IF (NLAUE .EQ. NRESTR(1,I)) GO TO 10
 1    CONTINUE
C
C Unrecognized Laue-group
      NX = -1
      RETURN
C
 10   CALL FNDSMP(NXMIN, NRESTR(2,I), SAMPLE, NX)
      CALL FNDSMP(NYMIN, NRESTR(3,I), SAMPLE, NY)
      CALL FNDSMP(NZMIN, NRESTR(4,I), SAMPLE, NZ)
C
      RETURN
      END
C
C
C
C     ================================
      SUBROUTINE SETLIM(LSPGRP,XYZLIM)
C     ================================
C
C Set appropriate box (asymmetric unit) for spacegroup (true spacegroup)
C     LSPGRP. For high symmetry spacegroups, this will be more than
C     one asymmetric unit
C
C On entry:
C     lspgrp    true spacegroup (not FFT spacegroup)
C
C On exit
C     xyzlim(2,3)  minimum, maximum limits on x,y,z (fractions of cell)
C                  if spacegroup not recognized, returns xzylim(1,1) = -1.0
C                  Note that the minimum limits (xyzlim(1,)) will always
C                   = 0.0
C
C      IMPLICIT NONE
C
      INTEGER LSPGRP
      REAL XYZLIM(2,3)
C
      INTEGER I,J
C
      INTEGER NUMSGP
      PARAMETER (NUMSGP=89)
      REAL ONE,HALF,THRD,TWTD,SIXT,QUAR,EIGH,TWLT,ROUND,ROUND2
      REAL ONEL,HALFL,THRDL,SIXTL,QUARL
      PARAMETER (ROUND=0.00001, ROUND2=2.0*ROUND)
      PARAMETER (ONE=1.0+ROUND,HALF=0.5+ROUND,THRD=1./3.+ROUND,
     $     TWTD=2./3.+ROUND,SIXT=1./6.+ROUND,
     $     QUAR=0.25+ROUND,EIGH=0.125+ROUND,TWLT=1./12.+ROUND)
      PARAMETER (ONEL=ONE-ROUND2,HALFL=HALF-ROUND2,THRDL=THRD-ROUND2,
     $     SIXTL=SIXT-ROUND2,QUARL=QUAR-ROUND2)
C
      INTEGER NSPGRP(NUMSGP)
      REAL ASULIM(3,NUMSGP)
C
C  asulim contains maximum limit on x,y,z: the box is always assumed to
C     start at 0,0,0
C
C  Space group numbers
      DATA NSPGRP/
     $   1,   2,   3,    4,   5,  10,  16,   17,  18,1018,  19,   20,
     $  21,  22,  23,   24,  47,  65,  69,   71,  75,  76,  77,   78,
     $  79,  80,  83,   87,  89,  90,  91,   92,  93,  94,  95,   96,
     $  97,  98, 123,  139, 143, 144, 145,  146, 147, 148, 149,  150,
     $ 151, 152, 153,  154, 155, 162, 164,  166, 168, 169, 170,  171,  
     $ 172, 173, 175,  177, 178, 179, 180,  181, 182, 191, 195,  196,  
     $ 197, 198, 199,  200, 202, 204, 207,  208, 209, 210, 211,  212,  
     $ 213, 214, 221,  225, 229/
C
      DATA ((ASULIM(II,JJ),II=1,3),JJ=1,73)/
C        1:  P1          2:  P-1         3:  P2            4:  P21
     $ ONEL,ONEL,ONEL, ONEL,HALF,ONEL, HALF,ONEL,ONEL, ONEL,HALFL,ONEL,
C        5:  C2         10:  P2/m       16:  P222         17:  P2221
     $ HALF,HALFL,ONEL, HALF,HALF,ONEL,HALF,HALF,ONEL, HALF,HALF,ONEL,
C       18: P21212    1018: P21212      19: P212121       20:C2221
     $ ONEL,QUAR,ONEL, ONEL,QUAR,ONEL, ONEL,ONEL,QUAR, HALF,QUAR,ONEL,
C       21:  C222       22:  F222       23:  I222         24: I212121
     $ HALF,QUAR,ONEL, QUAR,QUAR,ONEL, HALF,QUAR,ONE, HALF,QUAR,ONEL,
C       47:  Pmmm       65:  Cmmm       69:  Fmmm         71:  Immm
     $ HALF,HALF,HALF, HALF,QUAR,HALF, QUAR,QUAR,HALF, HALF,QUAR,HALF,
C       75:  P4         76:  P41        77:  P42          78:  P43
     $ HALF,HALF,ONEL,ONEL,ONEL,QUARL, HALF,ONEL,HALFL,ONEL,ONEL,QUARL,
C       79:  I4         80:  I41        83:  P4/m         87:  I4/m
     $ HALF,HALF,HALF,HALF,ONEL,QUARL, HALF,HALF,HALF, HALF,HALF,QUAR,
C       89: P422        90: P4212       91: P4122         92: P41212
     $ HALF,HALF,HALF, HALF,HALF,HALF, ONEL,ONEL,EIGH, ONEL,ONEL,EIGH,
C       93: P4222       94: P42212      95: P4322         96: P43212
     $ HALF,ONEL,QUAR, HALF,HALF,HALF, ONEL,ONEL,EIGH, ONEL,ONEL,EIGH,
C       97: I422        98: I4122      123: P4/mmm       139: I4/mmm
     $ HALF,HALF,QUAR, HALF,ONEL,EIGH, HALF,HALF,HALF,  HALF,HALF,QUAR,
C      143:  P3        144:  P31       145: P32          146:  R3
     $ TWTD,TWTD,ONEL,ONEL,ONEL,THRDL,ONEL,ONEL,THRDL, TWTD,TWTD,THRDL,
C      147:  P-3       148:  R-3       149: P312         150:  P321
     $ TWTD,TWTD,HALF, TWTD,TWTD,SIXT, TWTD,TWTD,HALF, TWTD,TWTD,HALF,
C      151: P3112      152: P3121      153: P3212        154: P3221
     $ ONEL, ONEL,SIXT, ONEL,ONEL,SIXT, ONEL,ONEL,SIXT, ONEL,ONEL,SIXT,
C      155: R32        162:  P-31m     164: P-3m1
     $ TWTD,TWTD,SIXT, TWTD,HALF,HALF, twtd,thrd, one,
C      166:  R-3m        168:  P6
     $ TWTD,TWTD,SIXT, TWTD,HALF,ONEL,
C      169:  P61       170:  P65       171:  P62         172:  P64
     $ ONEL,ONEL,SIXTL,ONEL,ONEL,SIXTL,ONEL,ONEL,THRDL,ONEL,ONEL,THRDL,
C      173:  P63       175:  P6/m      177: P622         178: P6122
     $ TWTD,TWTD,HALFL, TWTD,TWTD,HALF,TWTD,HALF,HALF, ONEL,ONEL,TWLT,
C      179: P6522      180: P6222      181: P6422        182: P6322
     $ ONEL,ONEL,TWLT, ONEL,ONEL,SIXT, ONEL,ONEL,SIXT, TWTD,TWTD,QUAR,
C      191: P6/mmm     195: P23        196: F23          197: I23
     $ TWTD,THRD,HALF, ONEL,ONEL,HALF, QUAR,QUAR,ONEL, ONEL,ONEL,HALF/
      DATA ((ASULIM(II,JJ),II=1,3),JJ=74,NUMSGP)/
C      198: P213       199: I213       200: Pm-3         202: Fm-3
     $ HALF,HALF,ONEL, HALF,HALF,HALF, HALF,HALF,HALF, HALF,HALF,QUAR,
C      204: Im-3       207: P432       208: P4232        209: F432
     $ HALF,HALF,HALF, ONEL,HALF,HALF, HALF,ONEL,QUAR, HALF,HALF,HALF,
C      210: F4132      211: I432       212: P4332        213: P4132
     $ HALF,ONEL,EIGH, HALF,HALF,QUAR, ONEL,ONEL,EIGH, ONEL,ONEL,EIGH,
C      214: I4132      221: Pm-3m      225: Fm-3m        229: Im-3m
     $ HALF,ONEL,EIGH, HALF,HALF,HALF, HALF,QUAR,QUAR, HALF,HALF,QUAR/
C
      DO 10, J=1,NUMSGP
         IF (LSPGRP .EQ. NSPGRP(J)) GO TO 20
 10   CONTINUE
C
C Spacegroup not found
      XYZLIM(1,1) = -1.0
      RETURN
C
 20   DO 30, I=1,3
         XYZLIM(1,I) = 0.0
         XYZLIM(2,I) = ASULIM(I,J)
 30   CONTINUE
C
      END
C
C     =============================================
      SUBROUTINE FNDSMP(MINSMP, NMUL, SAMPLE, NSAMPL)
C     =============================================
C
C----  Find suitable grid sample, approximately = SAMPLE/2 * maximum index,
C     with required factor, & no prime factor .gt. 19
C
C  On entry:
C     MINSMP     minimum sample, approximately 2 * maximum index
C     NMUL       required factor
C     SAMPLE     desired sample factor, ie if = 1.0 (minimum), try to
C                get sample close to MINSMP
C
C  On exit:
C     nsampl     grid sample
C                if MINSMP<=0, nsampl=nmul
C
CC      implicit none
C
C     .. Scalar Arguments ..
      INTEGER MINSMP,NMUL,NSAMPL
      REAL SAMPLE
C     ..
C     .. Local Scalars ..
      REAL R1MAX,R1MIN,R2MAX,R2MIN
      INTEGER N
C     ..
C     .. External Functions ..
      LOGICAL FACTRZ
      EXTERNAL FACTRZ
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC NINT,REAL
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
C
C---- This routine makes 2 attempts at finding a suitable factor:-
C     1) searching downwards from r1max*MINSMP to r1min*MINSMP
C     2) searching upwards from r2min*MINSMP to r2max*MINSMP
C
      DATA R1MIN,R1MAX/1.0, 1.6/
      DATA R2MIN,R2MAX/1.4, 4.0/
C     ..
C
C----  Check MINSMP <= 0, if so set NSAMPL = NMUL
      IF (MINSMP .LE. 0) THEN
         NSAMPL = NMUL
         RETURN
      ENDIF
C
C---- Set search limits
      IF (SAMPLE .GE. 1.0) THEN
         R1MAX = SAMPLE
         R2MIN = MAX(1.0, SAMPLE*0.95)
      ENDIF
C----  Start with multiple of nmul
C
      N = NINT(REAL(MINSMP)*R1MAX/REAL(NMUL))*NMUL
C
C---- Function factrz returns .true.
C     if number has all prime factors .le. 19
C
   10 IF (FACTRZ(N)) THEN
C
C---- OK suitable sample interval found, accept it
C
        NSAMPL = N
        RETURN
      END IF
C
C---- decrement trial value & continue if still in range
C
      N = N - NMUL
      IF (REAL(N)/REAL(MINSMP).GT.R1MIN) GO TO 10
C
C---- Now try 2nd search if 1st unsuccesfull
C
      N = NINT(REAL(MINSMP)*R2MIN/REAL(NMUL))*NMUL
   20 IF (FACTRZ(N)) THEN
C
C---- OK suitable sample interval found, accept it
C
        NSAMPL = N
        RETURN
      END IF
C
C---- increment trial value & continue if still in range
C
      N = N + NMUL
      IF (REAL(N)/REAL(MINSMP).LT.R2MAX) GO TO 20
C
C---- Failed
C
      NSAMPL = -1
      RETURN
C
      END
C
C
C
C     ==========================
      LOGICAL FUNCTION FACTRZ(N)
C     ==========================
C
C---- Returns true if N has all prime factors .le. 19
C
C     .. Parameters ..
      INTEGER NFACT
      PARAMETER (NFACT=8)
C     ..
C     .. Scalar Arguments ..
      INTEGER N
C     ..
C     .. Local Scalars ..
      INTEGER I,NN
C     ..
C     .. Local Arrays ..
      INTEGER IFACT(NFACT)
C     ..
C     .. External Subroutines ..
      EXTERNAL CCPERR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Data statements ..
      DATA IFACT/2,3,5,7,11,13,17,19/
C     ..
C
      NN = N
C
      DO 20 I = 1,NFACT
   10   IF (MOD(NN,IFACT(I)).EQ.0) THEN
C
C---- factor found, divide & continue if required
C
          IF (IFACT(I).EQ.0) THEN
C
C                *******************************************
            CALL CCPERR(1,' Error divide by Zero in FACTRZ')
C                *******************************************
C
          END IF
C
          NN = NN/IFACT(I)
          IF (NN.EQ.1) THEN
C
C----  success
C
            FACTRZ = .TRUE.
            RETURN
          END IF
          GO TO 10
        END IF
   20 CONTINUE
C
C---- Failure
C
      FACTRZ = .FALSE.
      END

C ******
C     =========================================================
       SUBROUTINE CALC_ORIG_PS(NAMSPG_CIF,NSYM,RSYM,NORIG,ORIG,
     +                         LPAXISX,LPAXISY,LPAXISZ)
C     =========================================================
C
C -P- CALC_ORIG_PS - creates list of equivalent origins for the named spacegroup.
C
C                  ARGUMENTS
C                  ---------
C                    (I) NAMSPG_CIF - spacegroup name (character)
C                    (I) NSYM - number of symmetry operations
C                    (I) RSYM(4,4,NSYM) - symmetry ops stored as 4x4 matrices
C                    (O) NORIG - number of origins.
C                    (O) ORIG(3,i) - vector of alternate origin 
C                               (for example : 0.5,0.0,0.5)
C                               only positive components.
C                               include vector: (0,0,0)
C                    (O) LPAXISX - logical; set true if s/grp is polar along x axis
C                    (O) LPAXISY - logical; set true if s/grp is polar along y axis
C                    (O) LPAXISZ - logical; set true if s/grp is polar along z axis
C
C                        
C    Taken from Alexei Vagin
C -------------------------------------------------------------
       CHARACTER*(*) NAMSPG_CIF
       CHARACTER LINE*80
       REAL RSYM(4,4,*)

       REAL      ORIG(3,*),RSYMD(3,3)
C ******
      INTEGER   IS(3),ID(6)
      LOGICAL   LPAXISX,LPAXISY,LPAXISZ
C   These 6 fractions can represent an origin shift
      DATA      ID/0,6,4,8,3,9/
C                   1/2 1/3 2/3 1/4 3/4
C -------------------------------------------------------------
C    Initialise logicals
      LPAXISX=.TRUE.
      LPAXISY=.TRUE.
      LPAXISZ=.TRUE.
C    Find which axes are "polar" and cannot have alternate origins.
      IFX=0
      IFY=0
      IFZ=0
C   0.13, 0.17, 0.19 cannot be special positions in any spacegroup.
      X=0.13
      Y=0.17
      Z=0.19
      DO I=2,NSYM

        XX=     RSYM(1,1,I)*X + RSYM(1,2,I)*Y + RSYM(1,3,I)*Z
        YY=     RSYM(2,1,I)*X + RSYM(2,2,I)*Y + RSYM(2,3,I)*Z
        ZZ=     RSYM(3,1,I)*X + RSYM(3,2,I)*Y + RSYM(3,3,I)*Z

        IF((ABS(X-XX)).GT.0.01) LPAXISX=.FALSE.
        IF((ABS(Y-YY)).GT.0.01) LPAXISY=.FALSE.
        IF((ABS(Z-ZZ)).GT.0.01) LPAXISZ=.FALSE. 
      ENDDO

C  First origin is 0,0,0
      NORIG=1
      ORIG(1,1)=0.0
      ORIG(2,1)=0.0
      ORIG(3,1)=0.0

C  Check which points can be an alternate origin.
C                   1/2 1/3 2/3 1/4 3/4
C--- Only six possibilities which are 0,  1/2 1/3 2/3 1/4 3/4

      DO K1=1,6
       DO K2=1,6
        DO K3=1,6
         IF(K1.EQ.1.AND.K2.EQ.1.AND.K3.EQ.1) GO TO 200
          IS(1)=ID(K1)
          IS(2)=ID(K2)
          IS(3)=ID(K3)

          IF( LPAXISX.AND.IS(1).NE.0) GO TO 200
          IF( LPAXISY.AND.IS(2).NE.0) GO TO 200
          IF( LPAXISZ.AND.IS(3).NE.0) GO TO 200

C   Let [Si] =[RSYMi] be (3x4) symmetry operator.
C   Need to Check if the symmetry operator shifts of each alternate origin 
C   [ORx,ORy,ORz)  are preserved for each symmetry operator.
C   Origin (0,0,0) shifts to        Ti(1),     Ti(2)      Ti(3) 
C                             == RSYMi(1,4),RSYMi(2,4),RSYMi(3,4) 

C  [RSYMi] [OR]  =  [OR] + [Ti] + n[I]  = [1 0 0 RSYMi(1,4)] [OR1] +  n[I]
C                                         [0 1 0 RSYMi(2,4)] [OR2]
C                                         [0 0 1 RSYMi(3,4)] [OR3]

C  Hence  [RSYMi(1,1) -1   RSYMi(1,2)      RSYMi(1,3)      0] [OR1]   = n[I]
C         [RSYMi(2,1)      RSYMi(2,2) -1   RSYMi(2,3)      0] [OR2] 
C         [RSYMi(3,1)      RSYMi(3,2) -1   RSYMi(3,3) -1   0] [OR3] 
C         [   0                0               0           1] [1  ]

C   Use RSYM(..1) to respresent indentity.. Enough to use 3x3 matrix..
c          DO I=1,NSYM-1
c          DO J=I+1,NSYM
c            DO K=1,3
c            DO L=1,3
c              M(K,L)=ISYM(K,L,I)-ISYM(K,L,J)
c            ENDDO
c            ENDDO
c            DO K=1,3
c              JS=M(1,K)*IS(1)+M(2,K)*IS(2)+M(3,K)*IS(3)
c              IF(MOD(JS,12).NE.0) GO TO 200
c            ENDDO
c          ENDDO
c          ENDDO

          DO J=2,NSYM
            DO K=1,3
             DO L=1,3
              RSYMD(K,L)=RSYM(K,L,J)-RSYM(K,L,1)
             ENDDO
            ENDDO
            DO K=1,3
              CHK= RSYMD(K,1)*IS(1)+RSYMD(K,2)*IS(2)+RSYMD(K,3)*IS(3) 
     +           + 12000
              IF(ABS(MOD(CHK,12.0)).GT.0.05) GO TO 200
            ENDDO
          ENDDO
          NORIG=NORIG+1
          ORIG(1,NORIG)=IS(1)/12.0
          ORIG(2,NORIG)=IS(2)/12.0
          ORIG(3,NORIG)=IS(3)/12.0
 200      CONTINUE
        ENDDO
       ENDDO
      ENDDO
 100  CONTINUE
       WRITE(6,'(///,3A,I4)') 
     +  ' Number of Alternate origins for Spacegroup:  ',NAMSPG_CIF,
     +  ' is:',NORIG
      LINE = ' '

      IF( LPAXISX)    LINE=
     + ' This is a Polar spacegroup: Origin is not fixed along A axis'
      IF( LPAXISY)    LINE=
     + ' This is a Polar spacegroup: Origin is not fixed along B axis'
      IF( LPAXISZ)    LINE=
     + ' This is a Polar spacegroup: Origin is not fixed along C axis'
      IF( LPAXISX .AND. LPAXISY)    LINE=
     + ' This is a Polar+ spacegroup: Origin anywhere in A B plane'
      IF( LPAXISX .AND. LPAXISZ)    LINE=
     + ' This is a Polar+ spacegroup: Origin anywhere in A C plane'
      IF( LPAXISY .AND. LPAXISZ)    LINE=
     + ' This is a Polar+ spacegroup: Origin anywhere in B C plane'

       IF(LINE.NE.' ') WRITE(6,'(/,A)')LINE

      LINE = ' Norigin     Ox      Oy      Oz'
      IF( LPAXISX)    LINE =
     + ' Norigin     ??      Oy      Oz'
      IF( LPAXISY)    LINE =
     + ' Norigin     Ox      ??      Oz'
      IF( LPAXISZ)    LINE =
     + ' Norigin     Ox      Oy      ??'
      IF( LPAXISX .AND. LPAXISY)    LINE=
     + ' Norigin     ??      ??      Oz'
      IF( LPAXISX .AND. LPAXISZ)    LINE=
     + ' Norigin     ??      Oy      ??'
      IF( LPAXISY .AND. LPAXISZ)    LINE=
     + ' Norigin     Ox      ??      ??'
       WRITE(6,'(//,A)')LINE
       DO I=1,NORIG
         IF(.NOT. (LPAXISX.OR.LPAXISY.OR.LPAXISZ)) 
     +     WRITE(6,'(i8,3F8.4)') I,ORIG(1,I),ORIG(2,I),ORIG(3,I)
         IF(LPAXISX .AND. .NOT. (LPAXISY.OR.LPAXISZ))  
     +     WRITE(6,'(i8,A8,2F8.4)') I,'     ?? ',ORIG(2,I),ORIG(3,I)
         IF(LPAXISY .AND. .NOT. (LPAXISX.OR.LPAXISZ))  
     +     WRITE(6,'(i8,F8.4,A8,F8.4)') I,ORIG(1,I),'     ?? ',ORIG(3,I)
         IF(LPAXISZ .AND. .NOT. (LPAXISX.OR.LPAXISY))  
     +     WRITE(6,'(i8,2F8.4,A8)') I,ORIG(1,I),ORIG(2,I),'     ?? '
         IF(LPAXISX .AND. LPAXISY)  
     +     WRITE(6,'(i8,2A8,F8.4)') I,'     ?? ','     ?? ',ORIG(3,I)
         IF(LPAXISX .AND. LPAXISZ)  
     +     WRITE(6,'(i8,A8,F8.4,A8)') I,'     ?? ',ORIG(2,I),'     ?? '
         IF(LPAXISY .AND. LPAXISZ)  
     +     WRITE(6,'(i8,F8.4,2A8)') I,ORIG(1,I),'     ?? ','     ?? '
       ENDDO

      RETURN
      END






