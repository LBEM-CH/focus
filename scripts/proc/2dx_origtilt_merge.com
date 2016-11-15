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
set scriptAfile = "2dx_merge_scriptA.com"
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
  ${proc_2dx}/linblock "Using all APH files simultaneously."
endif
#
set dirnum = `cat ${dirfile} | wc -l`
set maxthread = `echo ${Thread_Number} ${dirnum} | awk '{if ($1<$2/2) { s = $1 } else { s = int($2 / 2) }} END { print s }'`
if ( ${maxthread} < "4" ) then
  set maxthread = 1
endif
if ( ${maxthread} == "0" ) then
  set maxthread = 1
endif
#
echo dummy > SCRATCH/job_01_${scriptname}.results
echo dummy > SCRATCH/job_01_${scriptname}-tmp.reflections
echo dummy > SCRATCH/job_01_${scriptname}-tmp.console
echo dummy > SCRATCH/job_01_results.aph
\rm -f SCRATCH/job_*_${scriptname}.results
\rm -f SCRATCH/job_*_${scriptname}-tmp.reflections
\rm -f SCRATCH/job_*_${scriptname}-tmp.console
\rm -f SCRATCH/job_*_results.aph
#
${bin_2dx}/2dx_merge_compileA_threaded.exe << eot
${scriptname}.results
${scriptname}-tmp.reflections
${scriptname}-tmp.console
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
${ctfrev}
${IVERBOSE}
${RFACAMP}
${merge_res_limit}
${RESMIN}
${RESMAX}
${merge_data_type_local}
${maxthread}
${ILIST_VAL}
${zstarwin}
eot
#
echo "<<@progress: 10>"
#
#############################################################################
${proc_2dx}/linblock "Launch merging scripts"
#############################################################################
#
if ( ${genref} == "1" ) then
  echo dummy > APH/REF1.hkl
  \rm -f APH/REF*.hkl
endif
#
if ( ${shftin} == "1" ) then
  echo dummy > PRJ/HKLAPH1.prj
  \rm -f PRJ/HKLAPH*.prj
endif
#
set maxthread_gt_9 = `echo ${maxthread} | awk '{ if ( $1 > 9 ) { s = 1 } else { s = 0 } } END { print s }'`
if ( ${maxthread_gt_9} == '1' ) then
  set maxthread_with_zero = ${maxthread}
else
  set maxthread_with_zero = "0"${maxthread}
endif
#
if ( ${maxthread} == "1" ) then
  echo "# IMAGE: SCRATCH/job_01_${scriptAfile} <CSH: Merging script>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/job_01_2dx_merge_scriptA.com.log <LOG: Origtilt A output>" >> LOGS/${scriptname}.results
else
  echo "# IMAGE: SCRATCH/job_01_${scriptAfile} <CSH: First (01) merging script>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/job_01_2dx_merge_scriptA.com.log <LOG: First (01) origtilt A output>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/job_${maxthread_with_zero}_${scriptAfile} <CSH: Last (${maxthread_with_zero}) merging script>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/job_${maxthread_with_zero}_2dx_merge_scriptA.com.log <LOG: Last (${maxthread_with_zero}) origtilt A output>" >> LOGS/${scriptname}.results
endif
#
foreach scriptA ( SCRATCH/job_*_${scriptAfile} )
  \chmod +x ${scriptA}
  if ( ${scriptA} != SCRATCH/job_${maxthread_with_zero}_${scriptAfile} ) then
    echo Background nohup ${scriptA} \> ${scriptA}.log \&
    nohup ${scriptA} > ${scriptA}.log &
  else
    echo Forground ${scriptA} \> ${scriptA}.log
    ${scriptA} > ${scriptA}.log
  endif
end
#
wait
#
echo "################################################"
echo "################################################"
echo "output in file SCRATCH/job_XX_2dx_merge_scriptA.com.log"
echo "################################################"
echo "################################################"
#
echo "Refinement jobs produced the following output files:"
touch SCRATCH/job_01_${scriptname}-tmp.console
\ls -l SCRATCH/job_*_${scriptname}-tmp.console
#
sleep 1
#
\rm -f SCRATCH/${scriptname}.console
echo "# IMAGE: SCRATCH/${scriptname}.console <LOG: Console output from merging>" >> LOGS/${scriptname}.results
foreach scriptAconsole ( SCRATCH/job_*_${scriptname}-tmp.console ) 
  cat ${scriptAconsole} >> SCRATCH/${scriptname}.console
  \rm -f ${scriptAconsole}
end
#
#
echo "<<@progress: 15>"
#
\rm -f APH/merge_tmp.aph
foreach aphfile ( SCRATCH/job_*_results.aph )
  cat ${aphfile} >> APH/merge_tmp.aph
  \rm -f ${aphfile}
end
#
echo "# IMAGE: APH/merge.aph <APH: merge.aph>" >> LOGS/${scriptname}.results
echo "0000001001" > APH/merge.aph
sort APH/merge_tmp.aph >> APH/merge.aph
#
if ( -e SUMMARY ) then
    if ( -e TMP987123.tmp ) then
        \mv -f TMP987123.tmp merge.phr
    endif
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

