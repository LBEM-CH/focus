#
#############################################################################
#                                                                           #
# This is not an executable script.                                         #
#                                                                           #
# This should be sourced from the calling script.                           #
#                                                                           #
# (C) 2dx.org, GNU Plublic License.                                         #
#                                                                           #
# Created..........: 20.05.2015                                             #
# Last Modification: 20.05.2015                                             #
# Author...........: 2dx.org                                                #
#                                                                           #
#############################################################################
#
#############################################################################
${proc_2dx}/linblock "Compile refinement script"
#############################################################################
#
set scriptBfile = "2dx_merge_scriptB.com"
set postprocessingfile = "2dx_merge_postprocessing.com"
#
set genref = "0"
#
if ( ${refbeamtilt} == 'y' ) then
  if ( ${merge_modus} == '3D' ) then
    set NBM = T
    ${proc_2dx}/linblock "NBM=T, doing beam tilt refinement."
  else
    ${proc_2dx}/linblock "ERROR: Beamtilt Refinement only in 3D modus possible."
    set NBM = F
    ${proc_2dx}/linblock "NBM=F, no beam tilt refinement."
  endif
else
  set NBM = F
  ${proc_2dx}/linblock "NBM=F, no beam tilt refinement."
endif
#
if ( ${reftiltgeo} == 'y' ) then
  if ( ${merge_modus} == '3D' ) then
    set NTL = T
    ${proc_2dx}/linblock "NTL=T, doing crystal tiltangle and tiltaxis refinement."
  else
    ${proc_2dx}/linblock "ERROR: Tilt Geometry Refinement only in 3D modus possible."
    set NTL = F
    ${proc_2dx}/linblock "NTL=F, no crystal tiltangle or tiltaxis refinement."
  endif
else
  set NTL = F
  ${proc_2dx}/linblock "NTL=F, no crystal tiltangle or tiltaxis refinement."
endif
#
echo "dummy" > SCRATCH/job_00_${scriptBfile}
\rm -f SCRATCH/job_*_${scriptBfile}
#
echo "dummy" > SCRATCH/job_00_${postprocessingfile}
\rm -f SCRATCH/job_*_${postprocessingfile}
#
echo "dummy" > SCRATCH/job_00_${scriptname}-tmp.py
\rm -f SCRATCH/job_*_${scriptname}-tmp.py
#
echo "dummy" > SCRATCH/job_00_${scriptname}-tmp.reflections
\rm -f SCRATCH/job_*_${scriptname}-tmp.reflections
#
echo "dummy" > SCRATCH/job_00_${scriptname}-tmp.console
\rm -f SCRATCH/job_*_${scriptname}-tmp.console
#
echo "dummy" > SCRATCH/job_00_${scriptBfile}.log
\rm -f SCRATCH/job_*_${scriptBfile}.log
#
${bin_2dx}/2dx_merge_compileB.exe << eot
${scriptname}-tmp.py
${scriptname}-tmp.reflections
${scriptname}-tmp.console
${proc_2dx}
${bin_2dx}
${dirfile}
${scriptBfile}
${postprocessingfile}
${genref}
${spcgrp}
${realcell}
${realang}
${zstarwin}
${ALAT}
${IVERBOSE}
${MergeStepSize}
${RFACAMP}
${IBOXPHS}
${NPRG}
${merge_reference}
${NBM}
${NTL}
${ITAXASTEP}
${RTAXASIZE}
${ITANGLSTEP}
${RTANGLSIZE}
${Merge_Reference_IQMAX}
${Merge_Reference_HKMAX}
${merge_res_limit}
${RESMIN}
${MergeResolution}
${merge_data_type}
${maxthread}
${ILIST_VAL}
0
eot
#
echo "<<@progress: +5>>"
#
#############################################################################
${proc_2dx}/linblock "Launching refinement script"
#############################################################################
#
set maxthread_gt_9 = `echo ${maxthread} | awk '{ if ( $1 > 9 ) { s = 1 } else { s = 0 } } END { print s }'`
if ( ${maxthread_gt_9} == '1' ) then
  set maxthread_with_zero = ${maxthread}
else
  set maxthread_with_zero = "0"${maxthread}
endif
if ( ${maxthread} == "1" ) then
  echo "# IMAGE: SCRATCH/job_01_${scriptBfile} <CSH: Refinement script>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/job_01_2dx_merge_scriptB.com.log <LOG: Origtilt B output>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/job_01_${postprocessingfile} <CSH: Refinement postprocessing script>" >> LOGS/${scriptname}.results
else
  echo "# IMAGE: SCRATCH/job_01_${scriptBfile} <CSH: First (01) refinement script>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/job_01_2dx_merge_scriptB.com.log <LOG: First (01) origtilt B output>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/job_01_${postprocessingfile} <CSH: First (01) refinement postprocessing script>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/job_${maxthread_with_zero}_${scriptBfile} <CSH: Last (${maxthread_with_zero}) refinement script>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/job_${maxthread_with_zero}_2dx_merge_scriptB.com.log <LOG: Last (${maxthread_with_zero}) origtilt B output>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/job_${maxthread_with_zero}_${postprocessingfile} <CSH: Last (${maxthread_with_zero}) refinement postprocessing script>" >> LOGS/${scriptname}.results
endif

foreach scriptB ( SCRATCH/job_*_${scriptBfile} )
  if ( ${scriptB} != SCRATCH/job_${maxthread_with_zero}_${scriptBfile} ) then
    echo Background nohup ${scriptB} \> ${scriptB}.log \&
    nohup ${scriptB} > ${scriptB}.log &
  else
    echo Forground ${scriptB} \> ${scriptB}.log
    ${scriptB} > ${scriptB}.log
  endif
end
#
#
echo "################################################"
echo "################################################"
echo "output in file SCRATCH/job_XX_2dx_merge_scriptB.com.log"
echo "################################################"
echo "################################################"
#
echo "Refinement jobs produced the following output files:"
touch SCRATCH/job_01_${scriptname}-tmp.console
\ls -l SCRATCH/job_*_${scriptname}-tmp.console
touch SCRATCH/job_01_${scriptname}-tmp.py
\ls -l SCRATCH/job_*_${scriptname}-tmp.py
#
sleep 1
#
\rm -f SCRATCH/${scriptname}.console
echo "# IMAGE: SCRATCH/${scriptname}.console <LOG: Console output from merging>" >> LOGS/${scriptname}.results
foreach scriptBconsole ( SCRATCH/job_*_${scriptname}-tmp.console ) 
  cat ${scriptBconsole} >> SCRATCH/${scriptname}.console
  \rm -f ${scriptBconsole}
end
#
\rm -f SCRATCH/${scriptname}.py
echo "# IMAGE: SCRATCH/${scriptname}.py <PY: Refinement results update script>" >> LOGS/${scriptname}.results
foreach scriptBresults ( SCRATCH/job_*_${scriptname}-tmp.py ) 
  cat ${scriptBresults} >> SCRATCH/${scriptname}.py
  \rm -f ${scriptBresults}
end
#
python SCRATCH/${scriptname}.py
#
echo "# IMAGE: SCRATCH/2dx_merge_scriptB_postprocessing.log <LOG: origtilt B postprocessing output>" >> LOGS/${scriptname}.results
#
#
#############################################################################
#
