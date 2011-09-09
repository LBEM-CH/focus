#
#
# this is not an independent script. It should only be called from other scripts.
#
# This sub-script is to be called, if the ${imagename}.mrc file or the ${nonmaskimagename}.mrc file are missing.
# This will then use the TIFF file and transform it into an MRC file, while checking correct size.
#
if ( ! -e ${imagename}.mrc ) then
  ${proc_2dx}/linblock "Image ${imagename}.mrc does not exist."
  #
  if ( ! ( -e ${imagename}.tif || -e ${imagename}.tiff ) ) then
    #############################################################################
    ${proc_2dx}/linblock "Image ${imagename}.tif does not exist."
    #############################################################################
    #
    #############################################################################
    ### Testing for presence of *.mrc
    #############################################################################
    #
    echo "dummy" > zzzzz27765.mrc
    set filename = `ls -1 *.mrc | sort | head -n 1`
    if ( ${filename} != "zzzzz27765.mrc" ) then
      set imagename = `echo ${filename} | cut -d\. -f1`
      if ( ! -e ${imagename}.mrc ) then
        ${proc_2dx}/linblock "Image ${imagename}.mrc not existing."
        ${proc_2dx}/linblock "You probably use more than one dot in the image name, which is not recommended."
        echo "#WARNING: Image ${imagename}.mrc not existing."  >> LOGS/${scriptname}.results
        echo "#WARNING: You probably use more than one dot in the image name, which is not recommended."  >> LOGS/${scriptname}.results
      endif
    else
      #
      #############################################################################
      ### Testing for presence of *.tif
      #############################################################################
      #
      echo "dummy" > zzzzz27765.tif
      set filename = `ls -1 *.tif | sort | head -n 1`
      if ( ${filename} != "zzzzz27765.tif" ) then
        set imagename = `echo ${filename} | cut -d\. -f1`
        if ( ! -e ${imagename}.tif ) then
          ${proc_2dx}/linblock "Image ${imagename}.tif not existing."
          ${proc_2dx}/linblock "You probably use more than one dot in the image name, which is not recommended."
          echo "#WARNING: Image ${imagename}.tif not existing."  >> LOGS/${scriptname}.results
          echo "#WARNING: You probably use more than one dot in the image name, which is not recommended."  >> LOGS/${scriptname}.results
        else
          ${proc_2dx}/linblock "Image ${imagename}.tif found...converting (2)."
          #
          ${bin_2dx}/tif2mrc.exe << eot
${imagename}.tif
${imagename}.mrc
y
eot
          #
          set new_mrc_created = 'y'
          #
        endif
      endif
      \rm -f zzzzz27765.tif
      #
      #
      #############################################################################
      ### Testing for presence of *.tiff
      #############################################################################
      #
      echo "dummy" > zzzzz27765.tiff
      set filename = `ls -1 *.tiff | sort | head -n 1`
      if ( ${filename} != "zzzzz27765.tiff" ) then
        set imagename = `echo ${filename} | cut -d\. -f1`
        if ( ! -e ${imagename}.tiff ) then
          ${proc_2dx}/linblock "Image ${imagename}.tiff not existing."
          ${proc_2dx}/linblock "You probably use more than one dot in the image name, which is not recommended."
          echo "#WARNING: Image ${imagename}.tiff not existing."  >> LOGS/${scriptname}.results
          echo "#WARNING: You probably use more than one dot in the image name, which is not recommended."  >> LOGS/${scriptname}.results
        else
          ${proc_2dx}/linblock "Image ${imagename}.tiff found...converting (3)."
          #
          ${bin_2dx}/tif2mrc.exe << eot
${imagename}.tiff
${imagename}.mrc
y
eot
          #
          set new_mrc_created = 'y'
          #
        endif
      endif
      \rm -f zzzzz27765.tiff
    endif
    \rm -f zzzzz27765.mrc
    #
    echo "set imagename = ${imagename}"  >> LOGS/${scriptname}.results
    set nonmaskimagename = ${imagename}
    echo "set nonmaskimagename = ${nonmaskimagename}"  >> LOGS/${scriptname}.results
    #
  else
    if ( -e ${imagename}.tiff ) then
      ${proc_2dx}/linblock "Renaming ${imagename}.tiff into ${imagename}.tif"
      ${proc_2dx}/linblock "Renaming ${imagename}.tiff into ${imagename}.tif" >> History.dat
      \mv -f ${imagename}.tiff ${imagename}.tif
    endif
    ${proc_2dx}/linblock "Image ${imagename}.tif found...converting (6)."
    #
    ${bin_2dx}/tif2mrc.exe << eot
${imagename}.tif
${imagename}.mrc
y
eot
    #
    set new_mrc_created = 'y'
    #
    echo "# IMAGE-IMPORTANT: "${imagename}.mrc "<Image>" >> LOGS/${scriptname}.results    
  endif
else
  echo "# IMAGE-IMPORTANT: "${imagename}.mrc "<Image>" >> LOGS/${scriptname}.results
endif
#
#
if ( ! -e ${imagename}.mrc ) then
  echo ":: "
  echo ":: "
  echo "::     ERROR: ${imagename}.mrc not existing."
  echo ":: "
  echo "::     Use filename like PROT0012345600.tif or PROT0012345600.mrc"
  echo ":: "
  echo ":: "
  ${proc_2dx}/protest "ABORTING."
endif
#
#
if ( ! -e ${nonmaskimagename}.mrc ) then
  ${proc_2dx}/linblock "Image ${nonmaskimagename}.mrc does not exist."
  #
  if ( ! ( -e ${nonmaskimagename}.tif || -e ${nonmaskimagename}.tiff ) ) then
    ${proc_2dx}/protest "Image ${nonmaskimagename}.tif does not exist."
  else
    if ( -e ${nonmaskimagename}.tiff ) then
      ${proc_2dx}/linblock "Renaming ${nonmaskimagename}.tiff into ${nonmaskimagename}.tif"
      ${proc_2dx}/linblock "Renaming ${nonmaskimagename}.tiff into ${nonmaskimagename}.tif" >> History.dat
      \mv -f ${nonmaskimagename}.tiff ${nonmaskimagename}.tif
    endif
    ${proc_2dx}/linblock "Image ${nonmaskimagename}.tif found...converting (7)."
    #
    ${bin_2dx}/tif2mrc.exe << eot
${nonmaskimagename}.tif
${nonmaskimagename}.mrc
y
eot
    #
    set new_mrc_created = 'y'
    #
    echo "# IMAGE-IMPORTANT: "${nonmaskimagename}.mrc "<Non-masked Image>" >> LOGS/${scriptname}.results    
  endif
else
  echo "# IMAGE-IMPORTANT: "${nonmaskimagename}.mrc "<Non-masked Image>" >> LOGS/${scriptname}.results
endif

