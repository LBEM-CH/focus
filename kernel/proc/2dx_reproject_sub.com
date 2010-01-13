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
#
${proc_2dx}/2dx_makedirs 
#
if ( ${make_reference} == "n" ) then
  exit
endif
#
set CCP4_SYM = 1
#
if ( ${ALAT} == "0" || ${ALAT} == "0.0" ) then
  ${proc_2dx}/protest "ALAT is not defined."
endif
set ALATnew = `echo ${ALAT} | awk '{ if ( $1 < 0 ) { s = -$1 } else { s = $1 }} END { print s }'`
if ( ${ALAT} != ${ALATnew} ) then
  set ALAT = ${ALATnew}
endif
#
set aphfile = APH/REFAPH${imagenumber}.hkl
if ( ! -e ${aphfile} ) then
  echo ":: pwd = ${PWD}"
  ${proc_2dx}/linblock "ERROR: ${aphfile} not found."
else
  #
  #
  #############################################################################
  ${proc_2dx}/linblock "f2mtz - to translate ${aphfile} into ${imagename}.mtz"
  #############################################################################
  #
  echo "Calling now:"
  echo "${bin_ccp4}/f2mtz hklin ${aphfile} hklout reference.mtz"
  echo "TITLE Reference for ${imagename}, ${date}"
  echo "CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}"
  echo "SYMMETRY 1"
  echo "LABOUT H K L F PHI FOM"
  echo "CTYPOUT H H H F P W"
  echo "FILE ${infile}"
  echo "SKIP 1"
  echo "END"
  #
${bin_ccp4}/f2mtz hklin ${aphfile} hklout reference.mtz << eof
TITLE Reference for ${imagename}, ${date}
CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}
SYMMETRY ${CCP4_SYM}
LABOUT H K L F PHI FOM
CTYPOUT H H H F P W
FILE ${aphfile}
SKIP 1
END
eof
#
#############################################################################
${proc_2dx}/linblock "fft - to transform reference.mtz into SCRATCH/reference1.map"
#############################################################################
#
set AXIS = "X,Y,Z"
#
# contrast for grey plot
set scale = 1
echo "scale = ${scale}"
#
\rm -f SCRATCH/reference1.map
${bin_ccp4}/fft hklin reference.mtz mapout SCRATCH/reference1.map  << eot
LABIN F1=F PHI=PHI ##
PROJECTION
AXIS ${AXIS}
SCALE F1 ${scale} ${tempfac}
RESOLUTION ${RESMIN} ${RESMAX}
TITLE Reference for ${imagename}, ${date}, res=${RESMAX}, T=${tempfac}
GRID 200 200 20
XYZLIM 0 199 0 199 0 0
RHOLIM 250.0
HKLMAX 50 50 50
END
eot
#
#############################################################################
${proc_2dx}/linblock "extend - to extend SCRATCH/reference1.map into SCRATCH/reference2.map"
#############################################################################
#
\rm -f SCRATCH/reference2.map
${bin_ccp4}/extends mapin SCRATCH/reference1.map mapout SCRATCH/reference2.map << eof
XYZLIM 0 399 0 399 0 0
KEEP
END
eof
#
#############################################################################
${proc_2dx}/linblock "LABEL - to create a clean MRC file format instead of CCP4"
#############################################################################
#
\rm -f reference.mrc
#
${bin_2dx}/labelh.exe << eot
SCRATCH/reference2.map 
2
reference.mrc
1,0
0
eot
#
echo "# IMAGE: reference.mrc <MRC: Reprojected reference map>" >> ${mergedir}/LOGS/${scriptname}.results
#
endif
#
#############################################################################
#                                                                           #
#${proc_2dx}/linhash "Done."
#                                                                           #
#############################################################################
#

