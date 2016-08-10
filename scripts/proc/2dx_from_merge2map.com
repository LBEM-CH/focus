#!/bin/csh -ef
#
#############################################################################
#                                                                           #
# This is not an executable script.                                         #
#                                                                           #
# This should be sourced from the calling script.                           #
#                                                                           #
# (C) 2dx.org, GNU Plublic License.                                         #
#                                                                           #
# Created..........: 04/03/2013                                             #
# Last Modification: 04/03/2013                                             #
# Author...........: 2dx.org                                                #
#                                                                           #
#############################################################################
#
# merge_aph is the input file needed
#
set debug = 0
if ( ! ($?merge2map_output_dir) ) then
    set merge2map_output_dir = "."
    ${proc_2dx}/protest "merge2map_output_dir not set."
endif

if ( ! -d ${merge2map_output_dir} ) then
    mkdir -p ${merge2map_output_dir}
    mkdir -p ${merge2map_output_dir}/APH
    mkdir -p ${merge2map_output_dir}/LOGS
    mkdir -p ${merge2map_output_dir}/SCRATCH
else
    if ( ! -d ${merge2map_output_dir}/APH ) then
        mkdir -p ${merge2map_output_dir}/APH
    endif
    if ( ! -d ${merge2map_output_dir}/LOGS ) then
        mkdir -p ${merge2map_output_dir}/LOGS
    endif
    if ( ! -d ${merge2map_output_dir}/SCRATCH ) then
        mkdir -p ${merge2map_output_dir}/SCRATCH
    endif
endif 
#############################################################################
${proc_2dx}/linblock "2dx_avrgamphs - only to calculate FOM"
#############################################################################
#
echo "current zminmax = ${zminmax}"
set zmaxlocal = `echo ${ALAT} | awk '{ s = ( 1 / ( 2 * $1 ) ) } END { print s }'`
set zminmaxlocal = `echo -${zmaxlocal},${zmaxlocal}`
# avramphs only works for 2D projection data.
${proc_2dx}/linblock "Symmetry statistics here are only good in 2D."
${proc_2dx}/linblock "Using therefore zminmax=${zminmaxlocal}."
#
# map1
#
\rm -f fort.1
\cp -f ${merge_aph} fort.1
if( ${debug} ) then
    echo "# IMAGE-IMPORTANT: ${merge_aph} <${merge_aph} [H,K,Z,A,P,#,IQ,W,Bk,CTF]>" >> LOGS/${scriptname}.results
endif
\rm -f fort.2
\rm -f ${merge2map_output_dir}/APH/avrg2D_${postfix}.hkl
\rm -f fort.3
\rm -f fort.4
\rm -f TMP444888.tmp
\rm -f TMP444789.tmp
\rm -f ${merge2map_output_dir}/LOGS/avramphs_${postfix}.table.txt
#
${bin_2dx}/2dx_avrgamphs.exe << eot > ${merge2map_output_dir}/LOGS/2dx_avrgamphs2D_${postfix}.log
T
1001,${zminmaxlocal}
8
${avrgamphsNUMBER}
${avrgamphsRESOL}
${realcell} ${realang}
${max_amp_correction}
eot
#
\rm -f fort.1
\mv -f fort.2 ${merge2map_output_dir}/APH/avrg2D_${postfix}.hkl
\rm -f fort.3
\rm -f fort.4
if ( ${debug} ) then
    echo "# IMAGE: ${merge2map_output_dir}/APH/avrg2D_${postfix}.hkl <averaged amp&phs avrg2D_${postfix}.hkl [H,K,F,P,IQ,FOM]>" >> LOGS/${scriptname}.results
endif
\rm -f TMP444789.tmp 
\rm -f TMP444888.tmp
#
#############################################################################
${proc_2dx}/linblock "2dx_centric2 - to correct phases to 0 or 180 for 2D run"
#############################################################################  
#
\rm -f ${merge2map_output_dir}/APH/centric2D_${postfix}.hkl
\rm -f ${merge2map_output_dir}/APH/centric2D_${postfix}.hk
#
${bin_2dx}/2dx_centric2.exe << eot
${merge2map_output_dir}/APH/avrg2D_${postfix}.hkl
${merge2map_output_dir}/APH/centric2D_${postfix}.hkl
${merge2map_output_dir}/APH/centric2D_${postfix}.hk
${realcell},${realang}
${RESMIN},${RESMAX}
${SYM}
eot
#
if ( ! -e ${merge2map_output_dir}/APH/centric2D_${postfix}.hkl ) then
  ${proc_2dx}/protest "ERROR occured."
endif

if ( ${debug} ) then
  echo "# IMAGE: ${merge2map_output_dir}/APH/centric2D_${postfix}.hkl < centric2D_${postfix}.hkl >" >> LOGS/${scriptname}.results
endif
#
#############################################################################
${proc_2dx}/linblock "2dx_hklsym4 - to apply symmetry to APH file for 2D run"
#############################################################################  
#
\rm -f ${merge2map_output_dir}/APH/sym2D_${postfix}.hkl
\rm -f ${merge2map_output_dir}/APH/sym_nosort2D_${postfix}.hkl
\rm -f ${merge2map_output_dir}/APH/sym_sort2D_${postfix}.hkl
\rm -f ${merge2map_output_dir}/APH/sym_noheader2D_${postfix}.hkl
#
# Set isig to 3, for NO SIGF BUT SET SIGF to 1.0
set isig = 3
#
${bin_2dx}/2dx_hklsym4.exe << eot
${merge2map_output_dir}/APH/centric2D_${postfix}.hkl
${merge2map_output_dir}/APH/sym_nosort2D_${postfix}.hkl
${merge2map_output_dir}/APH/sym2D_${postfix}.hkl
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
if ( ${debug} ) then
    echo "# IMAGE: ${merge2map_output_dir}/APH/sym2D_${postfix}.hkl <sym2D_${postfix}.hkl after symmetrization [H,K,L,F,P,FOM,1.0]>" >> LOGS/${scriptname}.results
endif
#
if ( ! -e ${merge2map_output_dir}/APH/sym2D_${postfix}.hkl ) then
  ${proc_2dx}/protest "ERROR occured."
endif
#
#############################################################################
${proc_2dx}/linblock "f2mtz - to transform APH file into MTZ file for 2D run"
#############################################################################  
#
set LABOUTval = "H K L F PHI FOM"
set CTYPOUTval = "H H H F P W"
#
set infile = ${merge2map_output_dir}/APH/sym2D_${postfix}.hkl
set outfile = ${merge2map_output_dir}/SCRATCH/map_${postfix}_MRClefthanded.mtz
\rm -f ${merge2map_output_dir}/SCRATCH/${postfix}_MRClefthanded.mtz
#
${bin_ccp4}/f2mtz hklin ${infile} hklout ${outfile} << eof
TITLE  Map, Symmetry=${CCP4_SYM}, ${postfix} , ${date}
CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}
SYMMETRY 1 
LABOUT ${LABOUTval}
CTYPOUT ${CTYPOUTval}
FILE ${infile}
SKIP 0
END
eof
#
\rm -f ${merge2map_output_dir}/map_${postfix}_MRClefthanded.mtz 
${bin_ccp4}/sftools << eot
read ${outfile} 
merge
expand
write ${merge2map_output_dir}/map_${postfix}_MRClefthanded.mtz
end
eot
#
if( ${debug} ) then
    echo "# IMAGE-IMPORTANT: ${merge2map_output_dir}/map_${postfix}_MRClefthanded.mtz <MTZ: map_${postfix}_MRClefthanded.mtz>" >> LOGS/${scriptname}.results
endif
#
