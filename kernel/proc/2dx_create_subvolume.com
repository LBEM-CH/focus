#
# 2dx_create_subvolume.com
# A script to create a subvolume from a ccp4 map (extended map)
# Usage:
#	2dx_create_subvolume.com <subvolume_type> <input_extended_map> <real_cell> <ALAT> <output_map>
#
# Example Usage:
#	2dx_create_subvolume.com 1 input_extended.map 131,131 200 input_sub.map
#
#-----------------------------------------------------------------------
# Prepare input and parameters
#-----------------------------------------------------------------------
#
set calculate_subvolume = "${1}"
set input_map = "${2}"
set realcell = "${3}"
set ALAT = "${4}"
set output_map = "${5}"
#
set split = ($realcell:as/,/ /)
set cellx = $split[1]
set celly = $split[2]
#
set cellxm1 = `echo ${cellx} | awk '{ s = $1 - 1 } END {print s}'`
set cellym1 = `echo ${celly} | awk '{ s = $1 - 1 } END {print s}'`
set ALATm1 = `echo ${ALAT} | awk '{ s = $1 - 1 } END {print s}'`
#
#
set input_name = "`echo ${input_map} | cut -d'.' -f1`"
#
#
#
#############################################################################
# Set the Rotational and Translational properties for various subvolume types
#############################################################################
#
#
#
if ( ${calculate_subvolume}x == "1x" ) then
  #------------------------------------------------------------------------
  ${proc_2dx}/linblock "Creating subvolume with type MLOK1 for map ${input_map}."
  #------------------------------------------------------------------------
  set maprot_rota = "0.0 0.0 45.0"
  set maprot_trans = "0.0 -20.0 0.0"
  #
  set middlex = "0.45"
  set middley = "0.45"
  set diam = "0.82"
  #
  set limxmin = `echo ${middlex} ${diam} | awk '{ s = $1 - ( $2 / 2.0 ) } END { print s }'`
  set limxmax = `echo ${middlex} ${diam} | awk '{ s = $1 + ( $2 / 2.0 ) } END { print s }'`
  set limymin = `echo ${middley} ${diam} | awk '{ s = $1 - ( $2 / 2.0 ) } END { print s }'`
  set limymax = `echo ${middley} ${diam} | awk '{ s = $1 + ( $2 / 2.0 ) } END { print s }'`
  set limzmin = "0.0"
  set limzmax = "1.0"
  #
endif
#
#
#
if ( ${calculate_subvolume}x == "2x" ) then
  #------------------------------------------------------------------------
  ${proc_2dx}/linblock "Creating subvolume with type CITS for map ${input_map}."
  #------------------------------------------------------------------------
  set maprot_rota = "0.0 0.0 22.5"
  set maprot_trans = "18.0 0.0 0.0"
  set limxmin = "0.03"
  set limxmax = "0.97"
  set limymin = "0.1"
  set limymax = "0.625"
  set limzmin = "0.0"
  set limzmax = "1.0"
  #
endif
#
#
#
if ( ${calculate_subvolume}x == "3x" ) then
  #------------------------------------------------------------------------
  ${proc_2dx}/linblock "Creating subvolume with type AQP for map ${input_map}."
  #------------------------------------------------------------------------
  set maprot_rota = "0.0 0.0 45.0"
  set maprot_trans = "0.0 0.0 0.0"
  set limxmin = "0.4"
  set limxmax = "0.99"
  set limymin = "0.4"
  set limymax = "0.99"
  set limzmin = "0.0"
  set limzmax = "1.0"
  #
endif
#
#
#
if ( ${calculate_subvolume}x == "4x" ) then
  #------------------------------------------------------------------------
  ${proc_2dx}/linblock "Creating subvolume with type CLC for map ${input_map}."
  #------------------------------------------------------------------------
  set maprot_rota = "0.0 0.0 52.5"
  set maprot_trans = "70.0 -10.0 0.0"
  set limxmin = "0.17"
  set limxmax = "0.71"
  set limymin = "0.05"
  set limymax = "0.92"
  set limzmin = "0.0"
  set limzmax = "1.0"
  #
endif
#
#
#
if ( ${calculate_subvolume}x == "5x" ) then
  #------------------------------------------------------------------------
  ${proc_2dx}/linblock "Creating subvolume with type FHUA for map ${input_map}."
  #------------------------------------------------------------------------
  set maprot_rota = "0.0 0.0 0.0"
  set maprot_trans = "0.0 0.0 0.0"
  set limxmin = "-0.25"
  set limxmax = "1.0"
  set limymin = "-1.0"
  set limymax = "0.25"
  set limzmin = "-0.25"
  set limzmax = "0.07"
  #
endif
#
#
#
#
echo ":Rotating with: ${maprot_rota}"
echo ":Translating with: ${maprot_trans}"
echo ":Limits are: ${limxmin} to ${limxmax}, ${limymin} to ${limymax}, ${limzmin} to ${limzmax}"
#
#
#
#
############################################################################
# PREPARING VOLUMES
############################################################################
set full_cell_map = "${input_name}_full_cell.map"
#
\rm -f ${full_cell_map}
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "MAPMASK - to produce map which covers full unit cell"
#------------------------------------------------------------------------
${bin_ccp4}/mapmask mapin ${input_map} mapout ${full_cell_map} << eof
XYZLIM CELL
END
eof
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "MAPROT - to rotate volume for sub-volume preparation"
#------------------------------------------------------------------------
#
set rot_map = "${input_name}_rotated.map"
#
echo ${rot_map}
#
\rm -f ${rot_map}
#
set ALAT2 = `echo ${ALAT} | awk '{ s = $1 / 2.0 } END { print s }'`
#
${bin_ccp4}/maprot mapin ${full_cell_map} wrkout ${rot_map} << eot
MODE FROM
CELL WORK ${realcell} ${ALAT} 90.0 90.0 ${realang}
GRID WORK ${cellx} ${celly} ${ALAT}
XYZLIM 0 ${cellxm1} 0 ${cellym1} 0 ${ALATm1}
SYMM WORK 1
AVER
ROTA POLAR ${maprot_rota}
TRANS  ${maprot_trans}
eot
#
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "MAPAMSK - to cut sub-volume"
#------------------------------------------------------------------------
#
set processed_sub_map = "${input_name}_sub.map"
#
\rm -f ${processed_sub_map}
#
${bin_ccp4}/mapmask mapin ${rot_map} mapout ${processed_sub_map} << eof
AXIS X,Y,Z
scale factor 1
xyzlim ${limxmin} ${limxmax} ${limymin} ${limymax} ${limzmin} ${limzmax}
pad -100
SYMM 1
END
eof
#
#
\rm -f ${full_cell_map}
\rm -f ${rot_map}
\mv -f ${processed_sub_map} ${output_map}
#