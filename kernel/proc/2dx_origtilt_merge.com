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
${proc_2dx}/linblock "Compile merging script"
#############################################################################
#
if ( ! -e ${dirfile} ) then
    ${proc_2dx}/protest "ERROR: 2dx_merge did not provide the directory list."
endif
#
set num_selected = `cat ${dirfile} | wc -l`
if ( ${num_selected} == '0' ) then
    ${proc_2dx}/linblock "ERROR: Directory list is empty. No image directories selected"
    ${proc_2dx}/protest "ERROR: Did you check the image directories to be merged?"
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
if ( ${merge_data_type} == '5' ) then
    if ( ${scriptname} == "2dx_finalmerge" ) then
        echo ":: "
        ${proc_2dx}/linblock "Using all APH files simultaneously (this will take some time)."
        echo ":: "
        set merge_data_type_local = ${merge_data_type}
    else
        set merge_data_type_local = 4
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

