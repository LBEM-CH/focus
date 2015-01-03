#
#
# this is not an independent script. It should only be called from other scripts.
#
# This sub-script will crop the histogram, if needed.
#
if ( ${new_mrc_created} == "y" ) then
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
  if ( ${imageorigin} == '4' ) then
    #############################################################################
    ${proc_2dx}/lin "LABEL - to reduce pixel amplitude by a factor of 4."
    #############################################################################  
    #
    \mv -f ${inimage}.mrc SCRATCH/${nonmaskimagename}.tmp.mrc
    #
    ${bin_2dx}/labelh.exe << eot
SCRATCH/${nonmaskimagename}.tmp.mrc
2               ! switch to REAL (floating point)
${inimage}.mrc
0.25,0
1
eot
    #
    \rm -f SCRATCH/${nonmaskimagename}.tmp.mrc
    #
  endif
  #
  if ( ${imageorigin} == '5' ) then
    #############################################################################
    ${proc_2dx}/lin "LABEL - to produce MODE=1 INTEGER*2 image with autoscaling 0...16k."
    #############################################################################  
    #
    \mv -f ${inimage}.mrc SCRATCH/${nonmaskimagename}.tmp.mrc
    #
    ${bin_2dx}/labelh.exe << eot
SCRATCH/${nonmaskimagename}.tmp.mrc
16               ! switch to INTEGER*2 (MODE 1) with autoscaling 0…16000
${inimage}.mrc
eot
    #
    \rm -f SCRATCH/${nonmaskimagename}.tmp.mrc
    #
  endif
  #
  if ( ${imageorigin} == '6' ) then
    #############################################################################
    ${proc_2dx}/lin "LABEL - to produce MODE=1 with unsigned/signed swap and autoscaling 0...16k."
    #############################################################################  
    #
    \mv -f ${inimage}.mrc SCRATCH/${nonmaskimagename}.tmp.mrc
    #
    ${bin_2dx}/labelh.exe << eot
SCRATCH/${nonmaskimagename}.tmp.mrc
18               ! switch to INTEGER*2 (MODE 1) with unsigned/signed swap and autoscaling 0…16000
${inimage}.mrc
eot
    #
    \rm -f SCRATCH/${nonmaskimagename}.tmp.mrc
    #
  endif
  #
  if ( ${imageorigin} == '7' ) then
    #############################################################################
    ${proc_2dx}/lin "LABEL - to produce MODE=2 and autoscaling, and rotating 90deg."
    #############################################################################  
    #
    \mv -f ${inimage}.mrc SCRATCH/${nonmaskimagename}.tmp.mrc
    #
    ${bin_2dx}/labelh.exe << eot
SCRATCH/${nonmaskimagename}.tmp.mrc
39               ! switch to REAL (MODE 2) with autoscaling -1000,1000
SCRATCH/${nonmaskimagename}.tmp.2.mrc
eot
    #
    ${bin_2dx}/labelh.exe << eot
SCRATCH/${nonmaskimagename}.tmp.2.mrc
99               ! further modes
1		 ! various roations
${inimage}.mrc
1		 ! Z90 rotation
eot
    #
    \rm -f SCRATCH/${nonmaskimagename}.tmp.2.mrc
  endif
  #
  if ( ${imageorigin} == '8' ) then
    #############################################################################
    ${proc_2dx}/lin "LABEL - to produce MODE=2 and autoscaling."
    #############################################################################  
    #
    \mv -f ${inimage}.mrc SCRATCH/${nonmaskimagename}.tmp.mrc
    #
    ${bin_2dx}/labelh.exe << eot
SCRATCH/${nonmaskimagename}.tmp.mrc
39               ! switch to REAL (MODE 2) with autoscaling -1000,1000
${inimage}.mrc
eot
    #
  endif
  #
endif
#
