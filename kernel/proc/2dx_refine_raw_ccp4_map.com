#
#
# 2dx_refine_raw_ccp4_maps.com
#
#   -----------------------------------------------------------------------------------
#   ... This is not an independent script. It should only be called from other scripts.
#   -----------------------------------------------------------------------------------
#
#
# This script takes in a map file as input and does the following operations on it:
# MAPROT: to invert to correct handedness and correct size.
# MAPMASK: to bring protein to center of volume.
# MAPMASK: to extend the map.
# MAPROT - to rotate volume for sub-volume preparation.
# MAPAMSK - to cut sub-volume if asked for!
#
# The output maps are suffixed by the name of the functions.
# Example:
# Input map: <processed.map> will generate following maps in same directory:
# processed_correct_hand.map
# processed_centered.map
# processed_extended.map
# processed_sub.map (only if asked for!)
#
########################################################################
# Prepare input and parameters
########################################################################
#
set input_map = "${1}"
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "Refining ccp4 map ${input_map}.."
#------------------------------------------------------------------------
#
set input_name = "`echo ${input_map} | cut -d'.' -f1`"
#
########################################################################
# STEP 1: Invert the handedness
########################################################################
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "MAPROT: to invert to correct handedness and correct size.."
#------------------------------------------------------------------------
#
set processed_correct_hand_map = "${input_name}_correct_hand.map"
#
\rm -f ${processed_correct_hand_map}
#
${bin_ccp4}/maprot mapin ${input_map} wrkout ${processed_correct_hand_map} << eot
MODE FROM
CELL WORK ${realcell} ${ALAT} 90.0 90.0 ${realang}
GRID WORK ${cellx} ${celly} ${ALAT}
XYZLIM 0 ${cellxm1} 0 ${cellym1} 0 ${ALATm1}
SYMM WORK 1
AVER
ROTA MATRIX   1.000 0.000 0.000      0.000 1.000 0.000    0.000 0.000 -1.000
TRANS  0.000 0.000 0.000
eot
#
#
########################################################################
# STEP 2: Bring the protein to center
########################################################################
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "MAPMASK: to bring protein to center of volume.."
#------------------------------------------------------------------------
#
set processed_centered_map = "${input_name}_centered.map"
#
\rm -f ${processed_centered_map}
#
${bin_ccp4}/mapmask mapin ${processed_correct_hand_map} mapout ${processed_centered_map} << eof
AXIS X,Y,Z
scale factor 1
xyzlim -0.5 0.5 -0.5 0.5 -0.5 0.5
END
eof
#
#
#
########################################################################
# STEP 3: Extend the map
########################################################################
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "MAPMASK: to extend the map.."
#------------------------------------------------------------------------
#
set processed_extended_map = "${input_name}_extended.map"
#
rm -f ${processed_extended_map}
#
${bin_ccp4}/mapmask mapin ${processed_centered_map} mapout ${processed_extended_map} << eof
AXIS X,Y,Z
scale factor 1
xyzlim -1.0 0.999 -1.0 0.999 -0.5 0.5
END
eof
#
#
########################################################################
# STEP 4: Create sub-volume if required
########################################################################
#
if ( ${calculate_subvolume}x != "0x" ) then  
  #------------------------------------------------------------------------
  ${proc_2dx}/linblock "MAPROT - to rotate volume for sub-volume preparation"
  #------------------------------------------------------------------------
  #
  set processed_rot_map = "${input_name}_rotated.map"
  #
  \rm -f ${processed_rot_map}
  #
  set ALAT2 = `echo ${ALAT} | awk '{ s = $1 / 2.0 } END { print s }'`
  #
  ${bin_ccp4}/maprot mapin ${processed_correct_hand_map} wrkout ${processed_rot_map} << eot
MODE FROM
CELL WORK ${realcell} ${ALAT} 90.0 90.0 ${realang}
GRID WORK ${cellx} ${celly} ${ALAT}
XYZLIM 0 ${cellxm1} 0 ${cellym1} 0 ${ALATm1}
SYMM WORK 1
AVER
ROTA POLAR 0.0 0.0 45.0
TRANS  0.0 -20.0 ${ALAT2}
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
  ${bin_ccp4}/mapmask mapin ${processed_rot_map} mapout ${processed_sub_map} << eof
AXIS X,Y,Z
scale factor 1
xyzlim ${limxmin} ${limxmax} ${limymin} ${limymax} 0.0 1.0
pad -100
SYMM 1
END
eof
endif
#############################################################################
#