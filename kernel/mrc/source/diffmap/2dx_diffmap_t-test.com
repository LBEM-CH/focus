#
#############################################################################
#                                                                           #
# This is not an executable script.                                         #
#                                                                           #
# This should be sourced from the calling script.                           #
#                                                                           #
# (C) 2dx.org, GNU Plublic License.                                         #
#                                                                           #
# Created..........: 19/07/2013                                             #
# Last Modification: 19/07/2013                                             #
# Author...........: 2dx.org                                                #
#                                                                           #
#############################################################################
#
#set olddir = $PWD
#cd ..
#set basedir = $PWD
#cd ${olddir}
#
set zwin = `echo ${ALAT} | awk '{ s = 1.0 / ( 2.0 * $1 ) } END { print s }'`
echo zwin = $zwin
#
set make_reference = "n"
set create_PS = "n"
set RESULTSPS = "n"
set RESULTSMRC = "y"
rm -rf RESULTS-MRC
rm -rf RESULTS-TIFF
mkdir RESULTS-MRC
mkdir RESULTS-TIFF
echo 1000001 > RESULTS-TIFF/filenum.tmp
#
#############################################################################
${proc_2dx}/linblock "Compile script to generate maps"
#############################################################################
#
set dirfile = ${diffmap_t_test_selection}
set scriptMfile = "SCRATCH/2dx_diffmap_genmaps_scriptM.com"
rm -f ${scriptMfile}
#
${bin_2dx}/2dx_merge_compileM.exe << eot
${basedir}
${dirfile}
${scriptMfile}
${realcell}
${realang}
${ALAT}
${merge_ML_data}
${gridsize}
eot
#
echo "<<@progress: 50>>"
#
#############################################################################
${proc_2dx}/linblock "Launch script"
#############################################################################
#
echo "# IMAGE-IMPORTANT: ${scriptMfile} <csh: scriptM to generate maps>" >> LOGS/${scriptname}.results
#
source ${scriptMfile}
#
mv RESULTS-MRC ${diffmap_t_test_maps_dir}
#
