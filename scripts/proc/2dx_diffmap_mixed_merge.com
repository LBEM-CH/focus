#
#############################################################################
#                                                                           #
# This is not an executable script.                                         #
#                                                                           #
# This should be sourced from the calling script.                           #
#                                                                           #
# (C) 2dx.org, GNU Plublic License.                                         #
#                                                                           #
# Created..........: 01/03/2013                                             #
# Last Modification: 01/03/2013                                             #
# Author...........: 2dx.org                                                #
#                                                                           #
#############################################################################
#
set selection1 =  "diffmap_mixed_selection1.dat"
set selection2 =  "diffmap_mixed_selection2.dat"
set scriptAfile = "SCRATCH/2dx_merge_scriptA.com"
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
${proc_2dx}/linblock "Merging selection 1"
#############################################################################
#
if ( ! -e ${selection1} ) then
  ${proc_2dx}/protest "ERROR: 2dx_merge did not provide the selection list 1."
endif
#
if ( -z ${selection1} ) then
  ${proc_2dx}/linblock "ERROR: Selection list 1 is empty."
endif
#
\rm -f ${scriptAfile}
#
if ( ${ILIST} == "n" ) then
  set IVERBOSE = 1
  set ILIST_VAL = 0
else
  set IVERBOSE = 9
  set ILIST_VAL = 1
endif
#
${bin_2dx}/2dx_merge_compileA.exe << eot
LOGS/${scriptname}.results
${proc_2dx}
${bin_2dx}
${selection1}
${scriptAfile}
${genref}
${shftin}
${spcgrp}
${realcell}
${realang}
${ALAT}
${MergeIQMAX}
${MergeHKMAX}
${ctfrev}
${IVERBOSE}
${RFACAMP}
${merge_res_limit}
${RESMIN}
${RESMAX}
${merge_data_type}
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
${scriptAfile} > LOGS/2dx_merge_scriptA_selection1.log
#
echo "# IMAGE: LOGS/2dx_merge_scriptA_selection1.log <LOG: merging selection 1 output>" >> LOGS/${scriptname}.results
#
cat 2dx_origtiltk-console.log
# \rm -f 2dx_origtiltk-console.log
#
#
if ( -e fort.3 && -e SUMMARY ) then
  if ( -e TMP987123.tmp ) then
    \mv -f TMP987123.tmp merge1.phr
  endif
  \mv -f fort.3 diffmap/mixed_merge1.aph
  echo "# IMAGE: diffmap/mixed_merge1.aph <mixed_merge1.aph [H,K,Z,A,P,#,IQ,W,Bk,CTF]>" >> LOGS/${scriptname}.results
  \mv -f SUMMARY LOGS/mrcmerge1.summary.log
else
  ${proc_2dx}/protest "ERROR: Problem in ${scriptAfile}"
endif
#
if ( -e 2dx_origtiltk-reflections.log ) then
  \mv -f 2dx_origtiltk-reflections.log diffmap/2dx_origtiltk-reflections1.log
endif
#
#if ( -e SCRATCH/2dx_origtiltk_jrefl.txt ) then
#  set JREFL = `cat SCRATCH/2dx_origtiltk_jrefl.txt`
#  echo ":: Total number of reflections processed is ${JREFL}"
#  echo "set JREFL = ${JREFL}" >> LOGS/${scriptname}.results
#endif
#
#############################################################################
${proc_2dx}/linblock "Merging selection 2"
#############################################################################
#
if ( ! -e ${selection2} ) then
  ${proc_2dx}/protest "ERROR: 2dx_merge did not provide the selection list 2."
endif
#
if ( -z ${selection2} ) then
  ${proc_2dx}/linblock "ERROR: Selection list 2 is empty."
endif
#
#
\rm -f ${scriptAfile}
#
${bin_2dx}/2dx_merge_compileA.exe << eot
LOGS/${scriptname}.results
${proc_2dx}
${bin_2dx}
${selection2}
${scriptAfile}
${genref}
${shftin}
${spcgrp}
${realcell}
${realang}
${ALAT}
${MergeIQMAX}
${MergeHKMAX}
${ctfrev}
${IVERBOSE}
${RFACAMP}
${merge_res_limit}
${RESMIN}
${RESMAX}
${merge_data_type}
${Thread_Number}
${ILIST_VAL}
eot
#
echo "# IMAGE: ${scriptAfile} <CSH: merging script>" >> LOGS/${scriptname}.results
#
#############################################################################
${proc_2dx}/linblock "Launch merging script"
#############################################################################
#
chmod +x ${scriptAfile}
#
${scriptAfile} > LOGS/2dx_merge_scriptA_selection2.log
#
echo "# IMAGE: LOGS/2dx_merge_scriptA_selection2.log <LOG: merging selection 2 output>" >> LOGS/${scriptname}.results
#
cat 2dx_origtiltk-console.log
# \rm -f 2dx_origtiltk-console.log
#
#
if ( -e fort.3 && -e SUMMARY ) then
  if ( -e TMP987123.tmp ) then
    \mv -f TMP987123.tmp merge2.phr
  endif
  \mv -f fort.3 diffmap/mixed_merge2.aph
  echo "# IMAGE: diffmap/mixed_merge2.aph <mixed_merge2.aph [H,K,Z,A,P,#,IQ,W,Bk,CTF]>" >> LOGS/${scriptname}.results
  \mv -f SUMMARY diffmap/mrcmerge2.summary.log
else
  ${proc_2dx}/protest "ERROR: Problem in ${scriptAfile}"
endif
#
if ( -e 2dx_origtiltk-reflections.log ) then
  \mv -f 2dx_origtiltk-reflections.log diffmap/2dx_origtiltk-reflections2.log
endif
#
#if ( -e SCRATCH/2dx_origtiltk_jrefl.txt ) then
#  set JREFL = `cat SCRATCH/2dx_origtiltk_jrefl.txt`
#  echo ":: Total number of reflections processed is ${JREFL}"
#  echo "set JREFL = ${JREFL}" >> LOGS/${scriptname}.results
#endif
#

