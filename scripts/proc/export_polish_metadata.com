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

        if ( ! -e ${sub_basedir}/${export_gainref_subdir}/${gainref_name:r}.mrc ) then

          ${dir_imod}/bin/dm2mrc ${sub_basedir}/${export_gainref_subdir}/${gainref_name} ${sub_basedir}/${export_gainref_subdir}/${gainref_name:r}.mrc

        endif 

        if ( -e LOGS/motioncor2.out ) then

          set imsize_x = `grep "Stack size" LOGS/motioncor2.out | head -n 1 | cut -d' ' -f3`
          set imsize_y = `grep "Stack size" LOGS/motioncor2.out | head -n 1 | cut -d' ' -f5`
          set imsize_z = `grep "Stack size" LOGS/motioncor2.out | head -n 1 | cut -d' ' -f7`

          cat > ${sub_basedir}/${sub_targetdir}/${sub_targetname:r}.star << eot

data_general

_rlnImageSizeX                                     ${imsize_x}
_rlnImageSizeY                                     ${imsize_y}
_rlnImageSizeZ                                     ${imsize_z}
_rlnMicrographMovieName                            ${export_rawstack_subdir}/${import_rawstack}
_rlnMicrographGainName                             ${export_gainref_subdir}/${gainref_name:r}.mrc
_rlnMicrographDefectFile                           ${export_gainref_subdir}/${defects_name}
_rlnMicrographBinning                              ${bin_factor}
_rlnMicrographOriginalPixelSize                    ${pixelsize}
_rlnMicrographDoseRate                             ${frame_dose}
_rlnMicrographPreExposure                          0.000000
_rlnVoltage                                        ${KV}
_rlnMicrographStartFrame                           1
_rlnMotionModelVersion                             0
 

data_global_shift

loop_ 
_rlnMicrographFrameNumber #1 
_rlnMicrographShiftX #2 
_rlnMicrographShiftY #3
eot

          awk '{ print "         ", NR, "   ", $1, "   ", $2 }' motioncor2_shifts.txt >> ${sub_basedir}/${sub_targetdir}/${sub_targetname:r}.star

      else

        if ( -e motioncor2_shifts.star ) then
          cat motioncor2_shifts.star > ${sub_basedir}/${sub_targetdir}/${sub_targetname:r}.star

          # The _rlnMicrographGainName line will be added below together with the _rlnMicrographMovieName line, so we remove it here if present:
          sed -i '/_rlnMicrographGainName/d' ${sub_basedir}/${sub_targetdir}/${sub_targetname:r}.star

          # Here we add the correct _rlnMicrographMovieName and _rlnMicrographGainName lines, to ensure that both are always present
          sed -i "/_rlnMicrographMovieName/c\_rlnMicrographMovieName                            ${export_rawstack_subdir}/${import_rawstack}\n_rlnMicrographGainName                             ${export_gainref_subdir}/${gainref_name:r}.mrc"  ${sub_basedir}/${sub_targetdir}/${sub_targetname:r}.star

        else

          echo "::WARNING: When working on "\"${sub_targetdir}\"", no drift-correction information was found."

        endif

      endif

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

