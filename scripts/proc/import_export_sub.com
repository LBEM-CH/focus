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
      if ( ${sub_export_anything_doit} == "1" ) then
        echo ":   rsync -auvP ${sub_filename}   ${sub_basedir}/${sub_targetdir}/${sub_targetname}"
        \rsync -auvP ${sub_filename}   ${sub_basedir}/${sub_targetdir}/${sub_targetname}
      else
        echo ":   mv ${sub_filename} ${sub_basedir}/${sub_targetdir}/${sub_targetname}"
        \mv ${sub_filename} ${sub_basedir}/${sub_targetdir}/${sub_targetname}
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

