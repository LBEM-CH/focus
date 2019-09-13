#################################################################################
${proc_2dx}/linblock "Calculating Histogram of ${movie_stackname}"
#################################################################################
#
\rm -f FASTDISK/tmp1.png
  #
  set inimage = $1
  # 
  ${bin_2dx}/labelh.exe << eot
${inimage}.mrc
17               ! Get statistics
0.0
1.0
eot
  #
  echo " "
  cat < labelh.tmp
  echo " "
  #
  set min = `head -2 labelh.tmp | tail -1`
  set max = `head -3 labelh.tmp | tail -1`
  \rm -f labelh.tmp

# echo "# IMAGE-IMPORTANT: ${inimage}.mrc <Input Image (MRC)>"  >> LOGS/${scriptname}.results
# echo "# IMAGE-IMPORTANT: FASTDISK/tmp1.png <Input Image (PNG)>"  >> LOGS/${scriptname}.results
${app_2dx_mrc_converter} --size 1024 ${inimage}.mrc FASTDISK/tmp1.png  
#
\rm -f Histogram.png
# echo "# IMAGE-IMPORTANT: Histogram.png <Histogram (PNG)>"  >> LOGS/${scriptname}.results
echo ":: Calling: "${app_python} ${proc_2dx}/histogram.py FASTDISK/tmp1.png Histogram.png ${min} ${max}
${app_python} ${proc_2dx}/histogram.py FASTDISK/tmp1.png Histogram.png ${min} ${max}
#

