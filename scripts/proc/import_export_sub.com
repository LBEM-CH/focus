#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
set sub_doit = "${1}"
set sub_basedir = "${2}"
set sub_targetdir = "${3}"
set sub_filename = "${4}"
set sub_targetname = "${5}"
#
if ( ${sub_doit} == "y" ) then
  if ( -e ${sub_filename} ) then
    if ( ! -d ${sub_basedir}/${sub_targetdir} ) then
      echo "::   mkdir ${sub_basedir}/${sub_targetdir}"
      \mkdir ${sub_basedir}/${sub_targetdir}
    endif
    echo "::   rsync -auvP ${sub_filename}   ${sub_basedir}/${sub_targetdir}/${sub_targetname}"
    \rsync -auvP ${sub_filename}   ${sub_basedir}/${sub_targetdir}/${sub_targetname}
    #
    echo "#IMAGE-IMPORTANT: ${sub_basedir}/${sub_targetdir} <${sub_targetdir}>" >> LOGS/${scriptname}.results
    #
    set ending = ` echo ${sub_targetname} | rev | cut -c1-4 | rev `
    if ( ${ending} == "star" ) then
      set firstpart = ` echo ${sub_targetname} | sed 's/_micrographs_all_gctf.star//g' `
      echo "${firstpart}_aligned.mrc ${firstpart}_CTFDiag.mrc:mrc" `tail -n 2 ${sub_filename} | head -n 1 | cut -d\  -f3-` >> ${sub_basedir}/${sub_targetdir}/micrographs_all_gctf.star
    endif
    #
  else
    echo "::WARNING: ${sub_filename} not found."
  endif
endif
#
echo "<<@progress: +7>>" 
#
