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
    set avrgamphsRESlocal = ${RESMAX}
  else
    set avrgamphsRESlocal = ${avrgamphsRESOL}
  endif
else
  set avrgamphsRESlocal = ${avrgamphsRESOL}
endif
#
#############################################################################
${proc_2dx}/linblock "Setting APH file types to use"
#############################################################################
#
if ( ${scriptname} == "2dx_merge" ) then
 foreach imagefile ( ${dirlist} ) 
  cd ..
  cd ${imagefile}
  if ( ! -e 2dx_image.cfg ) then
    ${proc_2dx}/protest "ERROR: 2dx_image.cfg not found in ${imagefile}"
  endif
  set imagename_local = `cat 2dx_image.cfg | grep 'set imagename =' | cut -d\" -f2`
  cd APH
  set APH_file = ${imagename_local}_fou_ctf.aph
  # Already set:
  # if ( ${merge_data_type} == '0' ) then
  #   set APH_file = ${imagename_local}_fou_ctf.aph
  # endif
  if ( ${merge_data_type} == '1' ) then
    if ( ! -e ${imagename_local}_ctf.aph ) then
      set APH_file = ${imagename_local}_syn_ctf.aph
    else
      echo ":${imagename_local}_syn_ctf.aph not found."
    endif
  endif
  if ( ${merge_data_type} == '2' ) then
    if ( -e ${imagename_local}_movie_fou_ctf.aph ) then
      set APH_file = ${imagename_local}_movie_fou_ctf.aph
    else
      echo ":${imagename_local}_movie_fou_ctf.aph not found."
    endif
  endif
  if ( ${merge_data_type} == '3' ) then
    if ( -e ${imagename_local}_movie_syn_ctf.aph ) then
      set APH_file = ${imagename_local}_movie_syn_ctf.aph
    else
      echo ":${imagename_local}_movie_syn_ctf.aph not found."
    endif
  endif
  if ( ${merge_data_type} == '4' ) then 
    if ( -e ${imagename_local}_movieB_fou_ctf.aph ) then
      set APH_file = ${imagename_local}_movieB_fou_ctf.aph
    else
      echo ":${imagename_local}_movieB_fou_ctf.aph not found."
    endif
  endif
  if ( ${merge_data_type} == '5' ) then 
    if ( -e ${imagename_local}_movieB_syn_ctf.aph ) then
      set APH_file = ${imagename_local}_movieB_syn_ctf.aph
    else
      echo ":${imagename_local}_movieB_syn_ctf.aph not found."
    endif
  endif
  if ( ${merge_data_type} == '6' ) then
    if ( -e ML_result.aph ) then
      set APH_file = ML_result.aph
    else
      echo ":ML_result.aph not found."
    endif
  endif
  if ( ${merge_data_type} == '7' || ${merge_data_type} == '9' ) then
    set QVAL2_local =  `cat ../2dx_image.cfg | grep 'set QVAL2 =' | cut -d\" -f2`
    set QVALS_local =  `cat ../2dx_image.cfg | grep 'set QVALS =' | cut -d\" -f2`
    if ( ${QVALS_local} == '.' ) then
      set QVALS_local = 0
    endif 
    set QVALMA_loctmp =  `cat ../2dx_image.cfg | grep 'set QVALMA =' | cut -d\" -f2`
    if ( ${QVALMA_loctmp} == '.' ) then
      set QVALMA_loctmp = 0
    endif 
    set QVALAS_loctmp =  `cat ../2dx_image.cfg | grep 'set QVALAS =' | cut -d\" -f2`
    if ( ${QVALAS_loctmp} == '.' ) then
      set QVALAS_loctmp = 0
    endif 
    set QVALMB_loctmp =  `cat ../2dx_image.cfg | grep 'set QVALMB =' | cut -d\" -f2`
    if ( ${QVALMB_loctmp} == '.' ) then
      set QVALMB_loctmp = 0
    endif 
    set QVALBS_loctmp =  `cat ../2dx_image.cfg | grep 'set QVALBS =' | cut -d\" -f2`
    if ( ${QVALBS_loctmp} == '.' ) then
      set QVALBS_loctmp = 0
    endif 
    set QVALMA_local  = `echo ${QVALMA_loctmp} 1.1 | awk '{ s = $1 * $2 } END { print s }'`
    set QVALAS_local  = `echo ${QVALAS_loctmp} 1.1 | awk '{ s = $1 * $2 } END { print s }'`
    set QVALMB_local  = `echo ${QVALMB_loctmp} 1.1 | awk '{ s = $1 * $2 } END { print s }'`
    set QVALBS_local  = `echo ${QVALBS_loctmp} 1.1 | awk '{ s = $1 * $2 } END { print s }'`
    echo ${QVAL2_local} > awk.dat
    echo ${QVALS_local} >> awk.dat
    echo ${QVALMA_local} >> awk.dat
    echo ${QVALAS_local} >> awk.dat
    echo ${QVALMB_local} >> awk.dat
    echo ${QVALBS_local} >> awk.dat
    #
    # awk script to find max in CR-separated list:
    #  {if(min=="")min=max=$1}; if($1>max) {max=$1}; if($1< min) {min=$1}; total+=$1; count+=1} END {print total/count, min, max}
    #
    set QVAL_max = `awk '{if(max==""){max=$1;best=1;count=1}; if($1>max) {max=$1;best=count}; count += 1} END {print max}' awk.dat`
    set QVAL_best = `awk '{if(max==""){max=$1;best=1;count=1}; if($1>max) {max=$1;best=count}; count += 1} END {print best}' awk.dat`
    \rm -f awk.dat
    if ( ${QVAL_best} == '1' ) then
      set APH_file = ${imagename_local}_fou_ctf.aph
    endif
    if ( ${QVAL_best} == '2' ) then
      set APH_file = ${imagename_local}_syn_ctf.aph
    endif
    if ( ${QVAL_best} == '3' ) then
      set APH_file = ${imagename_local}_movie_fou_ctf.aph
    endif
    if ( ${QVAL_best} == '4' ) then
      set APH_file = ${imagename_local}_movie_syn_ctf.aph
    endif
    if ( ${QVAL_best} == '5' ) then
      set APH_file = ${imagename_local}_movieB_fou_ctf.aph
    endif
    if ( ${QVAL_best} == '6' ) then
      set APH_file = ${imagename_local}_movieB_syn_ctf.aph
    endif
    if ( ! -e ${APH_file} ) then
      set APH_file = ${imagename_local}_fou_ctf.aph
      echo "::${APH_file} should be best, but was not found."
    endif
  endif
  echo ":In directory ${imagename_local}: Using APH file ${APH_file}. "
  \rm -f ${imagename_local}_ctf.aph
  \ln -s ${APH_file} ${imagename_local}_ctf.aph
  cd ${olddir}
 end
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
#
set merge_data_type_local = ${merge_data_type}
#
if ( ${merge_data_type} == '9' ) then
  if ( ${scriptname} == "2dx_finalmerge" ) then
    echo ":: "
    ${proc_2dx}/linblock "Using all APH files simultaneously (this will take some time)."
    echo ":: "
    set merge_data_type_local = ${merge_data_type}
  else
    set merge_data_type_local = 3
    ${proc_2dx}/linblock "Using the APH file with the best QVAL."
  endif
endif
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
${merge_data_type_local}
${Thread_Number}
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
if ( -e 2dx_origtiltk-reflections.log ) then
  \mv -f 2dx_origtiltk-reflections.log LOGS
  echo "# IMAGE: LOGS/2dx_origtiltk-reflections.log <LOG: reflections after origtiltk [H,K,Z,A,P,#,IQ,WT,BK,CTF,--------IQ AMP PHS]>" >> LOGS/${scriptname}.results
endif
#
if ( -e SCRATCH/2dx_origtiltk_jrefl.txt ) then
  set JREFL = `cat SCRATCH/2dx_origtiltk_jrefl.txt`
  echo ":: Total number of reflections processed is ${JREFL}"
  echo "set JREFL = ${JREFL}" >> LOGS/${scriptname}.results
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
  #
  set TANGLST = 15.0
  #
  # The following paramter should be slightly larger than your highest tilt angle,
  # but smaller than 90.0 degrees.
  set TANGLMAX = 70.001
  #
  set IMQLABEL = 1
  set RMAX = ${RESMAX}
  set IQMAX = 8
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
  if ( -e TLTASM ) then
      mv -f TLTASM LOGS/2dx_tltplotk.txt
      echo "# IMAGE: LOGS/2dx_tltplotk.txt <LOG: PLTILTK summary>" >> LOGS/${scriptname}.results
  endif
  #
  # if ( -e LOGS/2dx_merge_scriptPLT.log ) then
  #   cat LOGS/2dx_merge_scriptPLT.log  
  # endif
  echo "################################################"
  echo "################################################"
  echo "::Check output in file LOGS/2dx_merge_scriptPLT.log"
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
