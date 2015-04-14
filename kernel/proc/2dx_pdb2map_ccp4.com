#
#
# 2dx_pdb2map_ccp4.com
#
#   -----------------------------------------------------------------------------------
#   ... This is not an independent script. It should only be called from other scripts.
#   -----------------------------------------------------------------------------------
#
########################################################################
# Prepare input and parameters
########################################################################
#
set input_pdb = "${1}"
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "Using ${input_pdb}.."
#------------------------------------------------------------------------
#
set input_name = "`echo ${input_pdb} | cut -d'.' -f1`"
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "Converting ${input_pdb}.."
#------------------------------------------------------------------------
#
set input_map = "${input_name}_ccp4.map"
#
\rm -f ${input_map}
#
${bin_ccp4}/sfall XYZIN  ${input_pdb} MAPOUT ${input_map}  << END-sfall
TITL Initial map of ${input_pdb}
GRID ${ALAT} ${ALAT} ${ALAT}
MODE ATMMAP
RESO ${RESMIN} ${RESMAX}
SYMM 1
BINS  60
VDWR 2.5
END 
END-sfall
#
