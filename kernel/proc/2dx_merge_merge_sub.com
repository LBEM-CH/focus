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
set dirfile = "2dx_merge_dirfile.dat"
set scriptAfile = "SCRATCH/2dx_merge_scriptA.com"
set scriptPLTfile = "SCRATCH/2dx_merge_scriptPLT.com"
#
set genref = "0"
set shftin = "0"
#
if ( ${spcgrp} != "1" ) then
  if ( ${avrgamphsRESOL} == '0' || ${avrgamphsRESOL} == '0.0' ) then
    set avrgamphsRESlocal = `echo ${RESMAX} | awk '{ s = int ( $1 * 9.5 / 10.0 ) } END { print s }'`
  else
    set avrgamphsRESlocal = ${avrgamphsRESOL}
  endif
else
  set avrgamphsRESlocal = ${avrgamphsRESOL}
endif
#
#############################################################################
${proc_2dx}/linblock "Compile merging script"
#############################################################################
#
if ( ! -e ${dirfile} ) then
  ${proc_2dx}/protest "ERROR: 2dx_merge did not provide the directory list."
endif
#
if ( -z ${dirfile} ) then
  ${proc_2dx}/linblock "ERROR: Directory list is empty."
  ${proc_2dx}/protest "ERROR: Did you select directories?"
endif
#
\rm -f ${scriptAfile}
#
set number = 1
if ( ${ILIST} == "n" ) then
  set IVERBOSE = 1
else
  set IVERBOSE = 9
endif
set RFACAMP = 1.0
#
${bin_2dx}/2dx_merge_compileA.exe << eot
LOGS/${scriptname}.results
${proc_2dx}
${bin_2dx}
${dirfile}
${scriptAfile}
${genref}
${shftin}
${spcgrp}
${realcell}
${realang}
${ALAT}
${MergeIQMAX}
${MergeHKMAX}
${IVERBOSE}
${RFACAMP}
${merge_res_limit}
${RESMIN}
${RESMAX}
${merge_ML_data}
${ILIST_VAL}
eot
#
echo "# IMAGE: ${scriptAfile} <CSH: merging script>" >> LOGS/${scriptname}.results
echo "<<@progress: 10>"
#
#############################################################################
${proc_2dx}/linblock "Launch merging script"
#############################################################################
#
chmod +x ${scriptAfile}
#
${scriptAfile} > LOGS/2dx_merge_scriptA.log
#
echo "# IMAGE: LOGS/2dx_merge_scriptA.log <LOG: origtilt A output>" >> LOGS/${scriptname}.results
# cat LOGS/2dx_merge_scriptA.log
echo "################################################"
echo "################################################"
echo "output in file LOGS/2dx_merge_scriptA.log"
echo "################################################"
echo "################################################"
#
cat 2dx_origtiltk-console.log
# \rm -f 2dx_origtiltk-console.log
#
echo "<<@progress: 15>"
#
if ( -e fort.3 && -e SUMMARY ) then
  if ( -e TMP987123.tmp ) then
    \mv -f TMP987123.tmp merge.phr
  endif
  \mv -f fort.3 APH/merge.aph
  \mv -f SUMMARY LOGS/mrcmerge.summary.log
else
  ${proc_2dx}/protest "ERROR: Problem in ${scriptAfile}"
endif
#
if ( ${IVERBOSE} > 5 ) then
  \mv -f 2dx_origtiltk-reflections.log LOGS
  echo "# IMAGE: LOGS/2dx_origtiltk-reflections.log <LOG: reflections after origtiltk>" >> LOGS/${scriptname}.results
endif
#
if ( ${scriptname} == "2dx_refine_cyclic" ) then
  if ( ${itogo} > 1 ) then
    exit
  endif
endif
#
if ( ${merge_modus} == "3D" ) then
  #
  #############################################################################
  ${proc_2dx}/linblock "Compile script to plot tilt geometry distribution"
  #############################################################################    
  #
  \rm -f ${scriptPLTfile} 
  #
  set number = 1
  set IVERBOSE = 1
  set RFACAMP = 1.0
  #
  set TANGLST = 15.0
  set TANGLMAX = 90.001
  set IMQLABEL = 1
  set RMAX = ${RESMAX}
  set IQMAX = 5
  set RGOOD = 0.5
  #
  ${bin_2dx}/2dx_merge_compilePLT.exe << eot
LOGS/${scriptname}.results
${proc_2dx}
${bin_2dx}
${dirfile}
${scriptPLTfile}
${genref}
${shftin}
${spcgrp}
${realcell}
${realang}
${ALAT}
${MergeIQMAX}
${MergeHKMAX}
${IVERBOSE}
${RFACAMP}
${TANGLST}
${TANGLMAX}
${IMQLABEL}
${RMAX}
${IQMAX}
${RGOOD}
eot
  #
  echo "# IMAGE: ${scriptPLTfile} <CSH: script to generate 3D plot>" >> LOGS/${scriptname}.results
  echo "<<@progress: 20>"
  #
  #############################################################################
  ${proc_2dx}/linblock "Launch script to plot tilt geometry distribution"
  #############################################################################
  #
  chmod +x ${scriptPLTfile}
  #
  echo "# IMAGE: LOGS/2dx_merge_scriptPLT.log <LOG: PLTILTK output>" >> LOGS/${scriptname}.results
  #
  \rm -f PS/2dx_tltplotk.ps
  #
  ${scriptPLTfile} > LOGS/2dx_merge_scriptPLT.log
  #
  # cat LOGS/2dx_merge_scriptPLT.log
  echo "################################################"
  echo "################################################"
  echo "output in file LOGS/2dx_merge_scriptPLT.log"
  echo "################################################"
  echo "################################################"
  #
  \mv -f TLTPLOT.PS PS/2dx_tltplotk.ps
  echo "# IMAGE-IMPORTANT: PS/2dx_tltplotk.ps <PS: TLTPLOT file>" >> LOGS/${scriptname}.results
  #
  echo "<<@progress: 22>"
  #
endif
#
#############################################################################
${proc_2dx}/linblock "2dx_avrgamphs - to calculate statistics"
#############################################################################
#
echo "current zminmax = ${zminmax}"
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
8
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
echo "# IMAGE: APH/avrg2D.hkl <APH: averaged amp&phs for 2D [H,K,A,P,IQ,FOM]>" >> LOGS/${scriptname}.results
\mv -f TMP444789.tmp LOGS/avramphs.table.txt
\rm -f TMP444888.tmp
if ( ${merge_modus} == "2D" ) then
  cat LOGS/avramphs.table.txt
  cat LOGS/avramphs.table.txt | sed 's/:://g' > LOGS/phase-residuals.txt
  echo "# IMAGE: LOGS/phase-residuals.txt <TXT: Phase residual table>" >> LOGS/${scriptname}.results
endif
\rm -f LOGS/avramphs.table.txt
#
echo "<<@progress: 26>>"
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
echo "# IMAGE: APH/centric2D.hkl <APH after CENTRIC for 2D [H,K,L,A,P,FOM]>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 35>>"
#
#############################################################################
${proc_2dx}/linblock "2dx_hklsym - to apply symmetry to APH file for 2D run"
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
${bin_2dx}/2dx_hklsym.exe << eot
APH/centric2D.hkl
APH/sym_nosort2D.hkl
APH/sym_noheader2D.hkl
${spcgrp}
1
${isig}
0     ! only write out asymmetric unit
eot
#
# This is used within 2dx_hklclean:
\rm -f APH/sym_nosort2D.hkl
\rm -f APH/syn_nosort2D-plot.hkl
#
echo "# IMAGE: APH/sym_noheader2D.hkl <APH after hklsym for 2D [H,K,L,A,P,FOM,1.0]>" >> LOGS/${scriptname}.results
#
sort < APH/sym_noheader2D.hkl > APH/sym_sort2D.hkl
echo "# IMAGE: APH/sym_sort2D.hkl <APH after sort [H,K,L,A,P,FOM,1.0]>" >> LOGS/${scriptname}.results
set withsigf = 0
#
${bin_2dx}/2dx_hklclean.exe << eot
APH/sym_sort2D.hkl
APH/sym2Dref.hkl
0
1
eot
#
# This is used within 2dx_hklclean:
\rm -f APH/sym_nosort2D.hkl
\rm -f APH/syn_nosort2D-plot.hkl
#
${bin_2dx}/2dx_hklclean.exe << eot
APH/sym_sort2D.hkl
APH/sym2D.hkl
0
${withsigf}
eot
#
echo "<<@progress: 40>>"
#
if ( ! -e APH/sym2D.hkl ) then
  ${proc_2dx}/protest "ERROR occured."
endif
#
echo "# IMAGE: APH/sym2D.hkl <APH after 2dx_clean for 2D [H,K,L,A,P,FOM]>" >> LOGS/${scriptname}.results
echo "# IMAGE: APH/sym2Dref.hkl <APH after 2dx_clean for 2D ref [H,K,L,A,P,FOM,1.0]>" >> LOGS/${scriptname}.results
echo "# IMAGE: APH/syn_nosort2D-plot.hkl <APH after 2dx_clean for merge plot [H,K,A,P,IQ,0,FOM]>" >> LOGS/${scriptname}.results
#
#############################################################################
${proc_2dx}/linblock "2dx_plotreska - to plot the powerspectrum with resolution circles"
${proc_2dx}/linblock "Using plotreska, contributed by Anchi Cheng."
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
APH/syn_nosort2D-plot.hkl
1	! Include IQ Value label
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
#
#
#
set savedir = $PWD
set date = `date`
#
# ATTENTION: f2mtz omits data in the 2D projection data if SIGF is defined??????
#
if ( ${withsigf} == 1 ) then
  set LABOUTval = "H K L F PHI FOM SIGF"
  set CTYPOUTval = "H H H F P W Q"
else
  set LABOUTval = "H K L F PHI FOM"
  set CTYPOUTval = "H H H F P W"
endif
#
\rm -f merge2D.mtz
#
if ( ${rotate_to_Z} == "yes" ) then
  # Here comes a messy thing: CCP4 wants to symmetrize in the Y direction,
  # but we want to symmetrize around the Z direction.
  # The data are therefore permutated from XYZ to ZXY, then symmetrized, and finally again
  # permutated to be projected in Y direction. 
  # This is only needed for P2, P2221b, and some others.
  #
  set cellx = `echo ${realcell} | sed 's/,/ /g' | awk '{ s = $1 } END { print s }'`
  set celly = `echo ${realcell} | sed 's/,/ /g' | awk '{ s = $2 } END { print s }'`
  echo "cellx, celly = ${cellx}, ${celly}"
  #
  #############################################################################
  ${proc_2dx}/linblock "2dx_permutate - to transform into K,L,H"
  #############################################################################
  #
  set infile = APH/sym2D.hkl
  ${bin_2dx}/2dx_permutate.exe < ${infile} > SCRATCH/TMP-permutated.hkl
  echo "# IMAGE: SCRATCH/TMP-permutated.hkl <TXT: APH file after permutation>" >> LOGS/${scriptname}.results
  #
  set infile = APH/sym2Dref.hkl
  ${bin_2dx}/2dx_permutateref.exe < ${infile} > SCRATCH/TMP-permutatedref.hkl
  #
  #############################################################################
  ${proc_2dx}/linblock "f2mtz - to transform APH file into MTZ file for 2D run"
  #############################################################################  
  #
  ${bin_ccp4}/f2mtz hklin SCRATCH/TMP-permutated.hkl hklout merge2D.mtz << eof
TITLE  Map, Symmetry=${CCP4_SYM}, ${savedir}, ${date}
CELL ${celly} ${ALAT} ${cellx} 90.0 90.0 ${realang}
SYMMETRY ${CCP4_SYM}
LABOUT ${LABOUTval}
CTYPOUT ${CTYPOUTval}
SKIP 0
END
eof
  #
  #############################################################################
  ${proc_2dx}/linblock "f2mtz - to transform APH file into MTZ file for 2D reference"
  #############################################################################  
  #
  ${bin_ccp4}/f2mtz hklin SCRATCH/TMP-permutatedref.hkl hklout merge2Dref.mtz << eof
TITLE  Map, Symmetry=${CCP4_SYM}, ${savedir}, ${date}
CELL ${celly} ${ALAT} ${cellx} 90.0 90.0 ${realang}
SYMMETRY ${CCP4_SYM}
LABOUT H K L F PHI FOM SIGF
CTYPOUT H H H F P W Q
SKIP 0
END
eof
  #
  # \rm -f SCRATCH/TMP-permutated.hkl
  #
else
  #
  #############################################################################
  ${proc_2dx}/linblock "f2mtz - to transform APH file into MTZ file for 2D run"
  #############################################################################  
  #
  set infile = APH/sym2D.hkl
  ${bin_ccp4}/f2mtz hklin ${infile} hklout merge2D.mtz << eof
TITLE  Map, Symmetry=${CCP4_SYM}, ${savedir}, ${date}
CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}
SYMMETRY ${CCP4_SYM}
LABOUT ${LABOUTval}
CTYPOUT ${CTYPOUTval}
FILE ${infile}
SKIP 0
END
eof
  #
  #############################################################################
  ${proc_2dx}/linblock "f2mtz - to transform APH file into MTZ file for 2D reference"
  #############################################################################  
  #
  set infile = APH/sym2Dref.hkl
  ${bin_ccp4}/f2mtz hklin ${infile} hklout merge2Dref.mtz << eof
TITLE  Map, Symmetry=${CCP4_SYM}, ${savedir}, ${date}
CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}
SYMMETRY ${CCP4_SYM}
LABOUT H K L F PHI FOM SIGF
CTYPOUT H H H F P W Q
FILE ${infile}
SKIP 0
END
eof
  #
endif
#
echo "# IMAGE-IMPORTANT: merge2Dref.mtz <MTZ: Merged full reciproc. space 2D data for reference>" >> LOGS/${scriptname}.results
echo "# IMAGE-IMPORTANT: merge2D.mtz <MTZ: Merged full reciproc. space 2D data>" >> LOGS/${scriptname}.results
#
#############################################################################
#
