#
#
# this is not an independent script. It should only be called from other scripts.
#
# This sub-script is to be called, if the ${imagename}.mrc file or the ${nonmaskimagename}.mrc file are missing.
# This will then use the TIFF file and transform it into an MRC file, while checking correct size.
#
if ( ! -e ${nonmaskimagename}.mrc ) then
  if ( ${nonmaskimagename} != "ScriptWillPutNameHere" ) then
    ${proc_2dx}/linblock "Image ${nonmaskimagename}.mrc does not exist."
  endif
  #
  if ( ! ( -e ${nonmaskimagename}.raw.mrc || -e ${nonmaskimagename}.tif || -e ${nonmaskimagename}.tiff ) ) then
    #############################################################################
    if ( ${nonmaskimagename} != "ScriptWillPutNameHere" ) then
      ${proc_2dx}/linblock "Neither image ${nonmaskimagename}.tif nor ${nonmaskimagename}.raw.mrc do exist."
    endif
    #############################################################################
    #
    #############################################################################
    ### Testing for presence of *.raw.mrc
    #############################################################################
    \rm -f final_map.tif
    echo "dummy" > zzzzz27765.raw.mrc
    set filename = `ls -1 *.raw.mrc | sort | head -n 1`
    \rm -f zzzzz27765.raw.mrc
    if ( ${filename} != "zzzzz27765.raw.mrc" ) then
      ${proc_2dx}/linblock "Found file ${filename}."
      set nonmaskimagename = `echo ${filename} | cut -d\. -f1`
      if ( ! -e ${nonmaskimagename}.raw.mrc ) then
        ${proc_2dx}/linblock "Image ${nonmaskimagename}.raw.mrc not existing."
        ${proc_2dx}/linblock "You probably use more than one or two dots in the image name, which is not recommended."
        echo "#WARNING: Image ${nonmaskimagename}.raw.mrc not existing."  >> LOGS/${scriptname}.results
        echo "#WARNING: You probably use more than one or two dots in the image name, which is not recommended."  >> LOGS/${scriptname}.results
      endif
      cp -f ${nonmaskimagename}.raw.mrc ${nonmaskimagename}.mrc
      set new_mrc_created = 'y'
      #
    else  
      #
      #############################################################################
      ### Testing for presence of *.mrc
      #############################################################################
      #
      echo "dummy" > zzzzz27765.mrc
      set filename = `ls -1 *.mrc | sort | head -n 1`
      if ( ${filename} != "zzzzz27765.mrc" ) then
        ${proc_2dx}/linblock "Found file ${filename}."
        set nonmaskimagename = `echo ${filename} | cut -d\. -f1`
        if ( ! -e ${nonmaskimagename}.mrc ) then
          ${proc_2dx}/linblock "Image ${nonmaskimagename}.mrc not existing."
          ${proc_2dx}/linblock "You probably use more than one dot in the image name, which is not recommended."
          echo "#WARNING: Image ${nonmaskimagename}.mrc not existing."  >> LOGS/${scriptname}.results
          echo "#WARNING: You probably use more than one dot in the image name, which is not recommended."  >> LOGS/${scriptname}.results
        else
          \cp -f ${nonmaskimagename}.mrc ${nonmaskimagename}.raw.mrc
          set new_mrc_created = 'y'
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
          set nonmaskimagename = `echo ${filename} | cut -d\. -f1`
          if ( ! -e ${nonmaskimagename}.tif ) then
            ${proc_2dx}/linblock "Image ${nonmaskimagename}.tif not existing."
            ${proc_2dx}/linblock "You probably use more than one dot in the image name, which is not recommended."
            echo "#WARNING: Image ${nonmaskimagename}.tif not existing."  >> LOGS/${scriptname}.results
            echo "#WARNING: You probably use more than one dot in the image name, which is not recommended."  >> LOGS/${scriptname}.results
          else
            ${proc_2dx}/linblock "Image ${nonmaskimagename}.tif found...converting (2)."
            #
            ${bin_2dx}/tif2mrc.exe << eot
${nonmaskimagename}.tif
${nonmaskimagename}.mrc
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
          set nonmaskimagename = `echo ${filename} | cut -d\. -f1`
          if ( ! -e ${nonmaskimagename}.tiff ) then
            ${proc_2dx}/linblock "Image ${nonmaskimagename}.tiff not existing."
            ${proc_2dx}/linblock "You probably use more than one dot in the image name, which is not recommended."
            echo "#WARNING: Image ${nonmaskimagename}.tiff not existing."  >> LOGS/${scriptname}.results
            echo "#WARNING: You probably use more than one dot in the image name, which is not recommended."  >> LOGS/${scriptname}.results
          else
            ${proc_2dx}/linblock "Image ${nonmaskimagename}.tiff found...converting (3)."
            #
            ${bin_2dx}/tif2mrc.exe << eot
${nonmaskimagename}.tiff
${nonmaskimagename}.mrc
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
    endif
  else
    #############################################################################
    ### Testing for presence of ${nonmaskimagename}.raw.mrc or ${nonmaskimagename}.tif
    #############################################################################
    if ( -e ${nonmaskimagename}.raw.mrc ) then
      \cp -f ${nonmaskimagename}.raw.mrc ${nonmaskimagename}.mrc
      ${proc_2dx}/linblock "Copying ${nonmaskimagename}.raw.mrc to ${nonmaskimagename}.mrc"
      set new_mrc_created = 'y'
    else
      if ( -e ${nonmaskimagename}.tiff ) then
        ${proc_2dx}/linblock "Renaming ${nonmaskimagename}.tiff into ${nonmaskimagename}.tif"
        ${proc_2dx}/linblock "Renaming ${nonmaskimagename}.tiff into ${nonmaskimagename}.tif" >> History.dat
        \mv -f ${nonmaskimagename}.tiff ${nonmaskimagename}.tif
      endif
      if ( -e ${nonmaskimagename}.tif ) then
        ${proc_2dx}/linblock "Image ${nonmaskimagename}.tif found...converting (6)."
        #
        ${bin_2dx}/tif2mrc.exe << eot
${nonmaskimagename}.tif
${nonmaskimagename}.mrc
y
eot
        #
        set new_mrc_created = 'y'
        #
      endif
    endif
  endif
endif
#
if ( ${new_mrc_created} == "y" ) then
  echo "set nonmaskimagename = ${nonmaskimagename}"  >> LOGS/${scriptname}.results
  set imagenumber = `echo ${imagename} | ${bin_2dx}/2dx_getnumber.exe`
  echo "set imagenumber = ${imagenumber}"  >> LOGS/${scriptname}.results
endif
#
echo "# IMAGE-IMPORTANT: "${nonmaskimagename}.mrc "<Non-Masked Image>" >> LOGS/${scriptname}.results
#
if ( ! -e ${nonmaskimagename}.mrc ) then
  echo ":: "
  echo ":: "
  echo "::     ERROR: ${nonmaskimagename}.mrc not existing."
  echo ":: "
  echo "::     Use a filename like PROT0012345600.tif or PROT0012345600.raw.mrc"
  echo ":: "
  echo ":: "
  ${proc_2dx}/protest "ABORTING."
endif
#
if ( ! -e ${imagename}.mrc ) then
  set imagename = ${nonmaskimagename}
  echo "set imagename = ${imagename}"  >> LOGS/${scriptname}.results
endif
echo "# IMAGE-IMPORTANT: "${imagename}.mrc "<Image>" >> LOGS/${scriptname}.results    

