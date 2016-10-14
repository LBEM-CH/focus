#
#
# this is not an independent script. It should only be called from other scripts.
#
# This sub-script will crop the histogram, if needed.
#
if ( ${new_mrc_created} == "y" || ${movie_inmovie}x == "yx" ) then
  #############################################################################
  ${proc_2dx}/lin "labelh - to calculate image statistics"
  #############################################################################
  #
  set inimage = ${loc_imagename}
  # 
  ${bin_2dx}/labelh.exe << eot
${inimage}.mrc
17               ! Get statistics
${crop_histogram_percent}
${crop_histogram_stdev}
eot
  #
  echo " "
  cat < labelh.tmp
  echo " "
  #
  set goodmin = `head -6 labelh.tmp | tail -1`
  set goodmax = `head -7 labelh.tmp | tail -1`
  \rm -f labelh.tmp
  #
  if ( ${crop_histogram} == 'y' ) then
    #############################################################################
    ${proc_2dx}/lin "labelh - to cutoff over and underflows to ${goodmin} to ${goodmax}"
    #############################################################################
    #
    #
    \mv -f ${inimage}.mrc SCRATCH/${nonmaskimagename}.tmp.mrc
    # 
    ${bin_2dx}/labelh.exe << eot
SCRATCH/${nonmaskimagename}.tmp.mrc
99               ! Cut off over and underflows
3
${inimage}.mrc
${goodmin},${goodmax}
eot
    #
    \rm -f SCRATCH/${nonmaskimagename}.tmp.mrc
    #
    # echo "# IMAGE: "SCRATCH/${nonmaskimagename}.tmp.mrc "<raw image before histogram correction>" >> LOGS/${scriptname}.results
    #
  else
    ${proc_2dx}/lin "Not cropping histogram (Advanced parameter)."
  endif
  #
  #############################################################################
  ${proc_2dx}/lin "LABEL - to produce MODE=2 and autoscaling."
  #############################################################################  
  #
  \mv -f ${inimage}.mrc SCRATCH/${nonmaskimagename}.tmp.mrc
  #
  ${bin_2dx}/labelh.exe << eot
SCRATCH/${nonmaskimagename}.tmp.mrc
39               ! switch to REAL (MODE 2) with autoscaling STDEV=100
${inimage}.mrc
eot
  #
endif
#
