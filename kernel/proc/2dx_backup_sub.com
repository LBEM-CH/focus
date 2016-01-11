#
set sub_from_dir = "${1}"
set sub_to_dir = "${2}"
set sub_overwrite = "${3}"
set sub_filename = "${4}"
#
if ( -d ${sub_from_dir} ) then
  if ( -e ${sub_from_dir}/${sub_filename} ) then
    if ( ! -d ${sub_to_dir} ) then
      \mkdir ${sub_to_dir}
    endif
    #
    if ( ${sub_overwrite} == "y" ) then
      echo ":New: ${sub_to_dir}/${sub_filename}"
      \cp -f ${sub_from_dir}/${sub_filename} ${sub_to_dir}
    else
      if ( -e ${sub_to_dir}/${sub_filename} ) then
        echo ":Old: ${sub_to_dir}/${sub_filename}"
      else
        echo ":New: ${sub_to_dir}/${sub_filename}"
        \cp -f ${sub_from_dir}/${sub_filename} ${sub_to_dir}
      endif
    endif
  endif
  #
endif
#
