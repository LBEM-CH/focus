###############################################################################
#   !!! NOT STANDALONE !!!!
# SCRIPT: 2dx_plttilt.com
# BRIEF: Plots the tilt series
#
# USAGE:
#	2dx_plttilt.com
#
# AUTHOR: Nikhil Biyani
#         Henning Stahlberg
#
###############################################################################
#
#
#----------------------------------------------------------------------------
${proc_2dx}/linblock "Compile script to plot tilt geometry distribution"
#----------------------------------------------------------------------------    
#
\rm -f ${scriptPLTfile} 
#
set number = 1
set IVERBOSE = 1
#
set TANGLST = 15.0
#
# The following parameter should be slightly larger than your highest tilt angle,
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
${ctfrev}
${zstarwin}
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
#