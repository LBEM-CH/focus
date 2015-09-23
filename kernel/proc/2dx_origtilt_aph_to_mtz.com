#
#############################################################################
#                                                                           #
# This is not an executable script.                                         #
#                                                                           #
# This should be sourced from the calling script.                           #
#                                                                           #
# (C) 2dx.org, GNU Plublic License.                                         #
#                                                                           #
# Created..........: 01/03/2007                                             #
# Last Modification: 01/03/2007                                             #
# Author...........: 2dx.org                                                #
#                                                                           #
#############################################################################
#
if ( ${merge_modus} == "3D" ) then
    source ${proc_2dx}/2dx_plttilt.com
endif
#
#############################################################################
${proc_2dx}/linblock "2dx_avrgamphs - to calculate statistics"
#############################################################################
#
set zmaxlocal = `echo ${ALAT} | awk '{ s = ( 1 / ( 2 * $1 ) ) } END { print s }'`
set zminmaxlocal = `echo -${zmaxlocal},${zmaxlocal}`
# avramphs only works for 2D projection data.
${proc_2dx}/linblock "Symmetry statistics here are only good in 2D."
${proc_2dx}/linblock "Using therefore zminmax=${zminmaxlocal}."
#
\rm -f fort.1
\cp -f APH/merge.aph fort.1
echo "# IMAGE-IMPORTANT: APH/merge.aph <merge.aph [H,K,Z,A,P,#,IQ,W,Bk,CTF]>" >> LOGS/${scriptname}.results
\rm -f fort.2
\rm -f fort.3
\rm -f fort.4
\rm -f TMP444888.tmp
\rm -f TMP444789.tmp
\rm -f LOGS/avramphs.table.txt
#
echo "<<@progress: 24>>"
#
${bin_2dx}/2dx_avrgamphs.exe << eot > LOGS/2dx_avrgamphs2D.log
T
1001,${zminmaxlocal}
${MergeIQMAX}
${avrgamphsNUMBER}
${avrgamphsRESlocal}
${realcell} ${realang}
${max_amp_correction}
eot
#
echo "################################################"
echo "################################################"
echo "output in file LOGS/2dx_avrgamphs.log"
echo "################################################"
echo "################################################"
#
\rm -f fort.1
\mv -f fort.2 APH/avrg2D.hkl
# This is not used:
#\mv -f fort.3 APH/avrg2D.hk
\rm -f fort.3
#\mv -f fort.4 APH/avrg2D.nolimit.aph
\rm -f fort.4
echo "# IMAGE: LOGS/2dx_avrgamphs2D.log <LOG: 2dx_avrgamphs output for 2D run>" >> LOGS/${scriptname}.results
if ( ${tempkeep} == "y" ) then
  echo "# IMAGE: APH/avrg2D.hkl <APH: averaged amp&phs for 2D [H,K,F,P,IQ,FOM]>" >> LOGS/${scriptname}.results
endif
\mv -f TMP444789.tmp LOGS/avramphs.table.txt
\rm -f TMP444888.tmp
if ( ${merge_modus} == "2D" ) then
  cat LOGS/avramphs.table.txt
  cat LOGS/avramphs.table.txt | sed 's/:://g' > LOGS/phase-residuals.txt
  echo "# IMAGE: LOGS/phase-residuals.txt <TXT: Phase residual table>" >> LOGS/${scriptname}.results
  if ( -e 2dx_avrgamphs.phares.txt ) then
    set overall_phase_residual_2D = `cat 2dx_avrgamphs.phares.txt | head -n 1`
    set JREFL_2D = `cat 2dx_avrgamphs.phares.txt | head -n 2 | tail -n 1`
    echo "set overall_phase_residual_2D = ${overall_phase_residual_2D}" >> LOGS/${scriptname}.results
    echo "set JREFL_2D = ${JREFL_2D}" >> LOGS/${scriptname}.results
    \mv -f 2dx_avrgamphs.phares.txt LOGS
    # echo "# IMAGE: LOGS/2dx_avrgamphs.phares.txt <TXT: Phase residual statistics>" >> LOGS/${scriptname}.results
  endif
  echo ":: "
  echo ":: The last column shows the average phase residual in that resolution range,"
  echo ":: which is the mean weighted squared distance of the phase values from the averaged value."
  echo ":: (90 deg = random)"
  echo ":: "
endif
\rm -f LOGS/avramphs.table.txt
#
echo "<<@progress: 26>>"
#
if ( ${merge_modus} == "2D" ) then
  #
  if ( ${TWOFOLD} == "T" ) then
    #############################################################################
    ${proc_2dx}/linblock "fomstats - to calculate FOM statistics"
    #############################################################################  
    #
    setenv IN  APH/avrg2D.hkl
    setenv OUT APH/avrg2D_fomstat.hkl
    setenv OUT2 SCRATCH/fomstats_statistics.dat
    #
    \rm -f APH/avrg2D_fomstat.hkl
    \rm -f SCRATCH/fomstats_statistics.dat
    #
    ${bin_2dx}/fomstats.exe << eot
100
F               !CUTOFFS
90 90           !IMIN,IMAX
${realcell} ${realang}        !A,B,GAMMA
${TWOFOLD} F F           !TWOFOLD,IHSCR,IKSCR
${avrgamphsRESlocal} ${avrgamphsNUMBER}             !RESOL, IBAND (# OF BINS)
eot
    echo ":: "
    echo ":: FOMSTATS output:"
    echo ":: "
    cat SCRATCH/fomstats_statistics.dat | sed 's/^/::/g'
    echo ":: "
    echo ":: The last column shows the average distance of the averaged phase values"
    echo ":: from the symmetry-constrained target values of 0 or 180 degrees."
    echo ":: (45 deg = random)"
    echo ":: "
    echo "# IMAGE-IMPORTANT: APH/avrg2D_fomstat.hkl <APH: FOMSTATS HKL output>" >> LOGS/${scriptname}.results
    echo "# IMAGE-IMPORTANT: SCRATCH/fomstats_statistics.dat <TXT: FOMSTATS statistics>" >> LOGS/${scriptname}.results
  else
    #############################################################################
    ${proc_2dx}/linblock "fomstats is not used, since the symmetry ${SYM} does not require ist."
    #############################################################################  
    #
  endif
  #
endif
#
#############################################################################
${proc_2dx}/linblock "2dx_centric2 - to correct phases to 0 or 180 for 2D run"
#############################################################################  
#
\rm -f APH/centric2D.hkl
\rm -f APH/centric2D.hk
#
${bin_2dx}/2dx_centric2.exe << eot
APH/avrg2D.hkl
APH/centric2D.hkl
APH/centric2D.hk
${realcell},${realang}
${RESMIN},${RESMAX}
${SYM}
eot
#
echo "<<@progress: 30>>"
#
if ( ! -e APH/centric2D.hkl ) then
  ${proc_2dx}/protest "ERROR occured."
endif
#
# This is not used:
\rm -f APH/centric2d.hk
#
if ( ${tempkeep} == "y" ) then
  echo "# IMAGE: APH/centric2D.hkl <APH after CENTRIC for 2D [H,K,L,F,P,FOM]>" >> LOGS/${scriptname}.results
endif
#
echo "<<@progress: 35>>"
#
#############################################################################
${proc_2dx}/linblock "2dx_hklsym4 - to apply symmetry to APH file for 2D run"
#############################################################################  
#
\rm -f APH/sym2D.hkl
\rm -f APH/sym_nosort2D.hkl
\rm -f APH/sym_sort2D.hkl
\rm -f APH/sym_noheader2D.hkl
#
# Set isig to 3, for NO SIGF BUT SET SIGF to 1.0
set isig = 3
#
${bin_2dx}/2dx_hklsym4.exe << eot
APH/centric2D.hkl
APH/sym_nosort2D.hkl
APH/sym2D.hkl
${spcgrp}
1
${isig}
0     ! write out full p1 plane
0     ! do not write out negative L values
eot
#
# LABOUT H K L F PHI FOM SIGF
# CTYPOUT H H H F P W Q
#
echo "# IMAGE: APH/sym2D.hkl <APH after hklsym4 [H,K,L,F,P,FOM,1.0]>" >> LOGS/${scriptname}.results
#
if ( ! -e APH/sym2D.hkl ) then
  ${proc_2dx}/protest "ERROR occured."
endif
#
#############################################################################
${proc_2dx}/linblock "2dx_plotreska - to plot the powerspectrum with resolution circles"
${proc_2dx}/lin "Using plotreska, contributed by Anchi Cheng."
#############################################################################  
#
\rm -f PLOTRES.PS
#
# plot ellipses in canonical HK space
set plotres_ellipse = "1"
#
# Plot as non-tilted section in 3D Fourier space
# 
${bin_2dx}/2dx_plotreska.exe << eot
0.0,0.0
3 	! Show as (here: non-)tilted projections, based on real-space lattice
${realcell},${realang},${lattice}
APH/sym2D.hkl
3	! calculate IQ Value label and expect L column
${plotres_ellipse}
${RESMAX}
${plotres_rings}
eot
#
if ( ! -e PLOTRES.PS ) then
  ${proc_2dx}/protest "ERROR: Problem in 2dx_plotreska."
endif
\mv -f PLOTRES.PS PS/2dx_plotreska_canonical.ps
echo "# IMAGE-IMPORTANT: PS/2dx_plotreska_canonical.ps <PS: Resolution Circle Plot of non-tilted data>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 40>>"
#
set savedir = $PWD
set date = `date`
#
# ATTENTION: f2mtz omits data in the 2D projection data if SIGF is defined??????
#
set LABOUTval = "H K L F PHI FOM SIGF"
set CTYPOUTval = "H H H F P W Q"
#
#############################################################################
${proc_2dx}/linblock "f2mtz - to transform APH file into MTZ file for 2D run"
#############################################################################  
#
set infile = APH/sym2D.hkl
\rm -f merge2D_MRClefthanded.mtz
#
# SYMMETRY ${CCP4_SYM}
#
${bin_ccp4}/f2mtz hklin ${infile} hklout merge2D_MRClefthanded.mtz << eof
TITLE  Map, Symmetry=${CCP4_SYM}, ${savedir}, ${date}
CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}
SYMMETRY 1
LABOUT ${LABOUTval}
CTYPOUT ${CTYPOUTval}
FILE ${infile}
SKIP 0
END
eof
#
cp -f merge2D_MRClefthanded.mtz merge2Dref_MRClefthanded.mtz
#
echo "# IMAGE-IMPORTANT: merge2D_MRClefthanded.mtz <MTZ: Merged full reciproc. space 2D data>" >> LOGS/${scriptname}.results
echo "# IMAGE-IMPORTANT: merge2Dref_MRClefthanded.mtz <MTZ: Merged full reciproc. space 2D data (copy for ref)>" >> LOGS/${scriptname}.results
#
#############################################################################
#
