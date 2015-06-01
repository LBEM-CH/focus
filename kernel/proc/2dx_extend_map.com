#
set input_map = "${1}"
set output_map = "${2}"
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "MAPMASK: to extend the map."
#------------------------------------------------------------------------
#
rm -f ${output_map}
#
${bin_ccp4}/mapmask mapin ${input_map} mapout ${output_map} << eof
AXIS X,Y,Z
scale factor 1
xyzlim -1.0 0.999 -1.0 0.999 0.0 0.999
END
eof
#
#

