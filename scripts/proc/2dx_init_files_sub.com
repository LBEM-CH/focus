#
#  2dx_init_files_sub.com
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
#
if ( ${scriptname} == '2dx_fftrans' || ${scriptname} == '2dx_initialize_files' ) then
  set lincommand = "linblock"
else
 set lincommand = "lin"
endif
#
if ( ! -e image_2dx.mrc ) then
  if ( -e movie_aligned.mrc ) then
    #################################################################################
    ${proc_2dx}/linblock "Setting imagename to image_2dx.mrc"
    #################################################################################
    #
    set imagename = image_2dx
    echo "set imagename = ${imagename}" >> LOGS/${scriptname}.results
    set nonmaskimagename = ${imagename}
    echo "set nonmaskimagename = ${nonmaskimagename}" >> LOGS/${scriptname}.results
    #
    \rm -f ${nonmaskimagename}_raw.mrc 
    \ln -s ${movie_stackname}.mrc ${nonmaskimagename}_raw.mrc
    #
  endif
endif
#
#################################################################################
${proc_2dx}/${lincommand} "Testing if ${nonmaskimagename}.mrc exists"
#################################################################################
#
set nonmaskimage_missing = "n"
set new_mrc_created = "n"
set correct = 0
#
if ( ! -e ${nonmaskimagename}.mrc && ! -e ${nonmaskimagename}_raw.mrc) then
  set nonmaskimage_missing = "y"
endif
#
if ( -e ${nonmaskimagename}.raw.mrc ) then
  \mv -f ${nonmaskimagename}.raw.mrc ${nonmaskimagename}_raw.mrc
  set nonmaskimage_missing = "n"
endif
#
if ( ${nonmaskimagename} == "ScriptWillPutNameHere" ) then
  set nonmaskimage_missing = "y"
  set correct = 1
endif
if ( ${imagesidelength} == "ScriptWillPutLengthHere" ) then
  set correct = 1
  set new_mrc_created = "y"
endif
#
if ( -e ${nonmaskimagename}.mrc ) then
  if ( ! -e ${nonmaskimagename}_raw.mrc ) then
    \cp -f ${nonmaskimagename}.mrc ${nonmaskimagename}_raw.mrc
    rm -f m${nonmaskimagename}.mrc
    set nonmaskimage_missing = 'n'
    set new_mrc_created = "y"
    set correct = 1
  endif
  set nonmaskimage_missing = 'n'
  set new_mrc_created = "n"
  set correct = 0
else
  if ( -e ${nonmaskimagename}_raw.mrc ) then
    ${proc_2dx}/${lincommand} "Copying ${nonmaskimagename}_raw.mrc onto ${nonmaskimagename}.mrc"
    \cp -f ${nonmaskimagename}_raw.mrc ${nonmaskimagename}.mrc
    set nonmaskimage_missing = 'n'
    set new_mrc_created = "y"
    set correct = 1
  else
    set nonmaskimage_missing = 'y'
  endif
endif
#
if ( ${nonmaskimage_missing} == "n" ) then
  ${proc_2dx}/${lincommand} "${nonmaskimagename}.mrc exists (1)"
else
  #################################################################################
  ### Find TIFF file and transform ${nonmaskimagename}.tif into ${nonmaskimagename}.mrc 
  ${proc_2dx}/linblock "Creating MRC format file."
  source ${proc_2dx}/2dx_initialize_tiff_to_mrc_sub.com
  #################################################################################
endif
#
#################################################################################
### Test (and Correct) Endianess of ${nonmaskimagename}.mrc #####################
${proc_2dx}/linblock "Testing Endianess."
source ${proc_2dx}/2dx_initialize_test_endianess_of_mrc_sub.com
#################################################################################
#
#################################################################################
### Make input image square and correct size ####################################
${proc_2dx}/linblock "Testing correct size of input image."
set loc_imagename = ${nonmaskimagename}
source ${proc_2dx}/2dx_initialize_make_image_square_sub.com
#################################################################################
#
if ( ${new_mrc_created} == "y" || ${movie_inmovie}x == "yx" ) then
  #################################################################################
  ### Treat histogram width, if needed ############################################
  ${proc_2dx}/linblock "Testing histogram width."
  set loc_imagename = ${nonmaskimagename}
  source ${proc_2dx}/2dx_initialize_crop_histogram_sub.com
  #################################################################################  
endif
#
#################################################################################
### Make sure, pixel size in header is correct ####################################
${proc_2dx}/linblock "Testing correct pixel size in header."
source ${proc_2dx}/2dx_correctHeaderCell_sub.com ${nonmaskimagename}.mrc ${sample_pixel}
#################################################################################
#
#
if ( ${use_masked_image} == "y" ) then
  if ( -e ${nonmaskimagename}_manualmask.mrc ) then
    set maskfile = ${nonmaskimagename}_manualmask.mrc
  else
    if ( -e ${nonmaskimagename}_automask.mrc ) then
      set maskfile = ${nonmaskimagename}_automask.mrc
    else
      if ( -e ${nonmaskimagename}_mask.mrc ) then
        \mv ${nonmaskimagename}_mask.mrc ${nonmaskimagename}_automask.mrc
        set maskfile = ${nonmaskimagename}_automask.mrc
      else
        ${proc_2dx}/linblock "No Masking Info file found.  Not masking image."
        set use_masked_image = "n"
        echo "set use_masked_image = ${use_masked_image}"  >> LOGS/${scriptname}.results
      endif 
    endif 
  endif 
endif
if ( ${use_masked_image} == "y" ) then
  if ( ${imagename} != m${nonmaskimagename} ) then
    set imagename = `echo m${nonmaskimagename}`
    echo "set imagename = ${imagename}" >> LOGS/${scriptname}.results
    ${proc_2dx}/linblock "correcting imagename to ${imagename}"
    \rm -f ${imagename}.mrc
  endif
  if ( ! -e ${imagename}.mrc ) then
    ###########################################################################
    ${proc_2dx}/linblock "Masking image with masking info"
    ###########################################################################
    echo "# IMAGE: ${maskfile} <Masking Information Mask>" >> LOGS/${scriptname}.results   
    echo "# IMAGE: ${nonmaskimagename}.mrc <Non-masked image>" >> LOGS/${scriptname}.results   
    echo "# IMAGE: ${imagename}.mrc <Masked image>" >> LOGS/${scriptname}.results   
    ${app_python} ${proc_2dx}/movie/mask.py ${nonmaskimagename}.mrc ${imagename}.mrc ${maskfile}
  endif
else
  if ( ${imagename} == m${nonmaskimagename} ) then
    \cp -f ${nonmaskimagename}.mrc ${imagename}.mrc 
    set imagename = `echo ${nonmaskimagename}`
    echo "set imagename = ${imagename}" >> LOGS/${scriptname}.results
    ${proc_2dx}/linblock "correcting imagename to ${imagename}"
  endif
  set domask = "y"
  echo "set domask = ${domask}" >> LOGS/${scriptname}.results
  ${proc_2dx}/linblock "correcting domask to ${domask}"
endif
#
if ( -l ${nonmaskimagename}_raw.mrc ) then
  \rm -f ${nonmaskimagename}_raw.mrc
endif
#
#
