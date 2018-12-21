#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
set sub_export_anything_doit = "${1}"
set sub_doit = "${2}"
set sub_basedir = "${3}"
set sub_targetdir = "${4}"
set sub_filename = "${5}"
set sub_targetname = "${6}"
#
echo sub_export_anything_doit = "${1}"
echo sub_doit = "${2}"
echo sub_basedir = "${3}"
echo sub_targetdir = "${4}"
echo sub_filename = "${5}"
echo sub_targetname = "${6}"
#
if ( ${sub_doit} == "y" ) then
  if ( ${sub_filename}x != "x" && ${sub_targetname}x != "x" ) then
    if ( -e ${sub_filename} ) then
      if ( ! -d ${sub_basedir}/${sub_targetdir} ) then
        echo "::   mkdir ${sub_basedir}/${sub_targetdir}"
        \mkdir ${sub_basedir}/${sub_targetdir}
      endif

        set imsize_x = `grep "Stack size" LOGS/motioncor2.out | head -n 1 | cut -d' ' -f3`
        set imsize_y = `grep "Stack size" LOGS/motioncor2.out | head -n 1 | cut -d' ' -f5`

        cat > ${sub_basedir}/${sub_targetdir}/${sub_targetname:r}.star << eot


data_general

_rlnImageSizeX                                     ${imsize_x}
_rlnImageSizeY                                     ${imsize_y}
_rlnImageSizeZ                                     ${movie_imagenumber_total}
_rlnMicrographMovieName                            ${export_rawstack_subdir}/${import_rawstack}
_rlnMicrographGainName                             ${export_gainref_subdir}/${gainref_name}
_rlnMicrographBinning                              ${bin_factor}
_rlnMicrographOriginalPixelSize                    ${pixelsize}
_rlnMicrographDoseRate                             ${frame_dose}
_rlnMicrographPreExposure                          0.000000
_rlnVoltage                                        ${KV}
_rlnMicrographStartFrame                           1
_rlnMotionModelVersion                             1
 

data_global_shift

loop_ 
_rlnMicrographFrameNumber #1 
_rlnMicrographShiftX #2 
_rlnMicrographShiftY #3
eot

awk '{ print NR, $1, $2 }' motioncor2_shifts.txt >> ${sub_basedir}/${sub_targetdir}/${sub_targetname:r}.star
# end




      #
    else
      echo "::WARNING: When working on "\"${sub_targetdir}\"", ${sub_filename} not found."
    endif
  else
    if ( ${sub_export_anything_doit} == "1" ) then
      echo "::WARNING: When working on "\"${sub_targetdir}\"", name of file to rsync not specified."
    else
      echo "::WARNING: When working on "\"${sub_targetdir}\"", name of file to move not specified."
    endif
  endif
endif
#

