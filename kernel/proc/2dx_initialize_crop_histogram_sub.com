#
#
# this is not an independent script. It should only be called from other scripts.
#
# This sub-script will crop the histogram, if needed.
#
  #############################################################################
  ${proc_2dx}/linblock "labelh - to calculate image statistics"
  #############################################################################
  #
  set inimage = ${imagename}.mrc
  # 
  ${bin_2dx}/labelh.exe << eot
${inimage}
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
  #
  if ( ${crop_histogram} == 'y' ) then
    #############################################################################
    ${proc_2dx}/linblock "labelh - to cutoff over and underflows to ${goodmin} to ${goodmax}"
    #############################################################################
    #
    #
    \mv -f ${imagename}.mrc SCRATCH/${imagename}.raw.mrc
    # 
    ${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}.raw.mrc
99               ! Cut off over and underflows
3
${imagename}.mrc
${goodmin},${goodmax}
eot
    #
    echo "# IMAGE: "SCRATCH/${imagename}.raw.mrc "<raw image before histogram correction>" >> LOGS/${scriptname}.results
    #
  else
    ${proc_2dx}/lin "Not cropping histogram (Advanced parameter)."
  endif
  #
  if ( ${imageorigin} == '4' ) then
    #############################################################################
    ${proc_2dx}/linblock "LABEL - to reduce pixel amplitude by a factor of 4."
    ${proc_2dx}/linblock "LABEL - to reduce pixel amplitude by a factor of 4." >> History.dat
    #############################################################################  
    #
    \mv -f ${imagename}.mrc SCRATCH/${imagename}.tmp.mrc
    #
    ${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}.tmp.mrc
2               ! switch to REAL (floating point)
${imagename}.mrc
0.25,0
1
eot
    #
  endif
  #
  if ( ${imageorigin} == '5' ) then
    #############################################################################
    ${proc_2dx}/linblock "LABEL - to produce MODE=1 INTEGER*2 image with autoscaling 0...32k."
    ${proc_2dx}/linblock "LABEL - to produce MODE=1 INTEGER*2 image with autoscaling 0...32k." >> History.dat
    #############################################################################  
    #
    \mv -f ${imagename}.mrc SCRATCH/${imagename}.tmp.mrc
    #
    ${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}.tmp.mrc
16               ! switch to INTEGER*2 (MODE 1) with autoscaling 0…32000
${imagename}.mrc
eot
    #
  endif

