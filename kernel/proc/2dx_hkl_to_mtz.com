#
#
# 2dx_hkl_to_mtz.com
# Converts a hkl file to CCP4 mtz file.
# 
# Usage:
#	2dx_hkl_to_mtz.com <input_hkl> <realcell> <ALAT> <realang> <RESMIN> <RESMAX> <output_mtz>
#
#
#
#
########################################################################
# Read in arguments
########################################################################
#
set input_hkl = ${1}
set realcell = ${2}
set ALAT = ${3}
set realang = ${4}
set RESMIN = ${5}
set RESMAX = ${6}
set output_mtz = ${7}
#
########################################################################
# Convert back projected hkl to mtz
########################################################################
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "F2MTZ : to convert back projected hkl to mtz.."
#------------------------------------------------------------------------
#
set f2mtz_mtz = "SCRATCH/f2mtz_MRClefthanded.mtz"
#
\rm -f ${f2mtz_mtz}
#
${bin_ccp4}/f2mtz hklin ${input_hkl} hklout ${f2mtz_mtz} << eof
TITLE  P1 map, ${date}
CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}
SYMMETRY 1
LABOUT H K L F PHI FOM
CTYPOUT H H H F P W
FILE ${input_hkl}
SKIP 0
END
eof
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "CAD : to finalize back projected mtz file.."
#------------------------------------------------------------------------
#
set cad_mtz = "SCRATCH/cad_MRClefthanded.mtz"
#
\rm -f ${cad_mtz}
#
#
${bin_ccp4}/cad hklin1 ${f2mtz_mtz} hklout ${cad_mtz} << eof
sort h k l
resolution overall ${RESMAX} ${RESMIN}  
outlim spacegroup 1
labin file 1 all
valm NaN NOOUTPUT
end
eof
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "sftools - to create MTZ file for reference with SIGF column"
#------------------------------------------------------------------------
#
\rm -f ${output_mtz}
#
${bin_ccp4}/sftools << eof
read ${cad_mtz}
calc COL SIGF = 1.0
write ${output_mtz}
quit
eof
#
#
\rm -f ${f2mtz_mtz}
\rm -f ${cad_mtz}
#
