#
#
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
set local_inputstack = $1
set local_samplepixel = $2
set local_SERIALEM_FACTOR = $3
#
echo ":Input stack for dose calculation is ${local_inputstack}"
echo ":Pixel size is ${local_samplepixel} Angstroems"
echo ":SERIALEM_FACTOR is ${local_SERIALEM_FACTOR}"
#
set COUNTS_PER_PIX = ` clip info ${local_inputstack} | grep mean | cut -d\= -f2 `
echo "::Mean count is: ${COUNTS_PER_PIX} electrons/pixel" 
#
set FRAMENUM = ` clip info ${local_inputstack} | grep size | cut -d\, -f3 | cut -d\) -f1`
echo "::Frame number is ${FRAMENUM}"
#
set frame_dose = ` echo "scale=3; ${COUNTS_PER_PIX} / ${local_samplepixel} / ${local_samplepixel} / ${local_SERIALEM_FACTOR}" | bc `
echo "::Calculated frame dose is: ${frame_dose} electrons/A2/frame" 
set total_dose = ` echo "scale=3; ${frame_dose} * ${movie_imagenumber_total} " | bc `
echo "::Calculated total dose is: ${total_dose} electrons/A2" 
#

