#
# 2dx_create_subvolume.com
# A script to create a subvolume from a ccp4 map (extended map)
# Usage:
#	2dx_create_subvolume.com <input_extended_map> <real_cell> <ALAT> <output_map>
#
# Example Usage:
#	2dx_create_subvolume.com input_extended.map 131,131 200 input_sub.map
#
########################################################################
# Prepare input and parameters
########################################################################
#
set input_map = "${1}"
set realcell = "${2}"
set ALAT = "${3}"
set output_map = "${4}"
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
if ( ${calculate_subvolume}x != "0x" ) then  
  #------------------------------------------------------------------------
  ${proc_2dx}/linblock "Creating subvolume of ccp4 map ${input_map}."
  #------------------------------------------------------------------------
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
ROTA POLAR 0.0 0.0 45.0
TRANS  0.0 -20.0 0.0
eot
  #
  #
  #------------------------------------------------------------------------
  ${proc_2dx}/linblock "MAPAMSK - to cut sub-volume"
  #------------------------------------------------------------------------
  #   
  # 0.7071 = 1/sqrt(2)
  set middlex = "0.45"
  set middley = "0.45"
  set diam = "0.82"
  #
  set limxmin = `echo ${middlex} ${diam} | awk '{ s = $1 - ( $2 / 2.0 ) } END { print s }'`
  set limxmax = `echo ${middlex} ${diam} | awk '{ s = $1 + ( $2 / 2.0 ) } END { print s }'`
  set limymin = `echo ${middley} ${diam} | awk '{ s = $1 - ( $2 / 2.0 ) } END { print s }'`
  set limymax = `echo ${middley} ${diam} | awk '{ s = $1 + ( $2 / 2.0 ) } END { print s }'`
  #
  echo ":Limits are ${limxmin} to ${limxmax}, ${limymin} to ${limymax}"
  #
  set processed_sub_map = "${input_name}_sub.map"
  #
  \rm -f ${processed_sub_map}
  #
  #
  ${bin_ccp4}/mapmask mapin ${rot_map} mapout ${processed_sub_map} << eof
AXIS X,Y,Z
scale factor 1
xyzlim ${limxmin} ${limxmax} ${limymin} ${limymax} 0.0 1.0
pad -100
SYMM 1
END
eof
  #
  #
  #
  \rm -f ${full_cell_map}
  \rm -f ${rot_map}
  \mv -f ${processed_sub_map} ${output_map}
  #
endif

#############################################################################

