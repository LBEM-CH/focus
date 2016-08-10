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
set scriptAfile = "SCRATCH/2dx_merge_selection_script.com"
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
${proc_2dx}/linblock "Merging selection"
#############################################################################
#
if ( ! -e ${diffmap_split_selection} ) then
  ${proc_2dx}/protest "ERROR: 2dx_merge did not provide the selection list."
endif
#
if ( -z ${diffmap_split_selection} ) then
  ${proc_2dx}/protest "ERROR: Selection list is empty."
endif
#
if ( -z ${diffmap_merge_output_dir} ) then
    echo ":: WARNING: diffmap_merge_output_dir is not set.
    set diffmap_merge_output_dir = "diffmap/selections"
    echo ":: setting diffmap_merge_output_dir to ${diffmap_mere_output_dir}.
endif
if ( ! -d ${diffmap_merge_output_dir} ) then
    mkdir -p ${diffmap_merge_output_dir}
endif
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
${diffmap_split_selection}
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
${scriptAfile} > LOGS/2dx_merge_script_selection.log
#
echo "# IMAGE: LOGS/2dx_merge_script_selection.log <LOG: merging selection output>" >> LOGS/${scriptname}.results
#
cat 2dx_origtiltk-console.log
# \rm -f 2dx_origtiltk-console.log
#
#
set output_filepath = "${diffmap_merge_output_dir}/${diffmap_merge_output}"
if ( -e fort.3 && -e SUMMARY ) then
  if ( -e TMP987123.tmp ) then
    \mv -f TMP987123.tmp ${output_filepath}.phr
  endif
  \mv -f fort.3 ${output_filepath}.aph
  echo "# IMAGE: ${output_filepath}.aph <${diffmap_merge_output}.aph [H,K,Z,A,P,#,IQ,W,Bk,CTF]>" >> LOGS/${scriptname}.results
  \mv -f SUMMARY LOGS/${diffmap_merge_output}.summary.log
else
  ${proc_2dx}/protest "ERROR: Problem in ${scriptAfile}"
endif
#
if ( -e 2dx_origtiltk-reflections.log ) then
  \mv -f 2dx_origtiltk-reflections.log LOGS/2dx_origtiltk-${diffmap_merge_output}.log
endif
#
#if ( -e SCRATCH/2dx_origtiltk_jrefl.txt ) then
#  set JREFL = `cat SCRATCH/2dx_origtiltk_jrefl.txt`
#  echo ":: Total number of reflections processed is ${JREFL}"
#  echo "set JREFL = ${JREFL}" >> LOGS/${scriptname}.results
#endif
#

