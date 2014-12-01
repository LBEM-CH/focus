#
#  2dx_refine_unbend2_sub.com
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
#######################################################################################
#######################################################################################
#######################################################################################
#
if ( ${refine_now} == "maskb01" ) then
  #
  \rm -f ${imagename}-maskb01.tabl
  #
  set old_maskb01 = ${maskb01}
  set loc_maskb01_start = `echo ${refine_maskb01_val} | cut -d\, -f1`
  set loc_maskb01_end   = `echo ${refine_maskb01_val} | cut -d\, -f2`
  set loc_maskb01_step  = `echo ${refine_maskb01_val} | cut -d\, -f3`
  set loc_maskb01_factor = `echo ${refine_maskb01_val} | cut -d\, -f4`
  #
  ${proc_2dx}/linblock "Refining maskb01, from ${loc_maskb01_start} to ${loc_maskb01_end} in steps ${loc_maskb01_step}, fact. ${loc_maskb01_factor}"
  #
  set loc_maskb01 = ${loc_maskb01_start}
  #
  while ( ${loc_maskb01} <= ${loc_maskb01_end} )
     #
     \cp -f FFTIR/cor${imagename}_unbend1_fft.mrc FFTIR/cor${imagename}_fft.mrc
     set maskb01 = ${loc_maskb01}
     source ${proc_2dx}/2dx_unbend2_sub.com
     #
     echo ${loc_maskb01} ${QVAL_local} >> ${imagename}-maskb01.tabl
     # ${proc_2dx}/linblock "unbend2: maskb01 ${loc_maskb01} gives a QVAL of ${QVAL_local}"
     set loc_maskb01 = `echo ${loc_maskb01} ${loc_maskb01_step} ${loc_maskb01_factor} | awk '{s = int( ($1 + $2 ) * $3) } END { print s }'`
  end
  #
  \rm -f TMP442211.tmp
  set best_maskb01 = `cat ${imagename}-maskb01.tabl | ${bin_2dx}/getmax.exe`
  set best_QVal = `cat TMP442211.tmp`
  \rm -f TMP442211.tmp
  #
  ${proc_2dx}/linblock "Best value was found at ${best_maskb01}, with QVal ${best_QVal}. Previously was ${old_maskb01}"
  #
  set maskb01 = ${best_maskb01}
  echo set maskb01 = ${maskb01} >> LOGS/${scriptname}.results
  #
endif
#
#######################################################################################
#######################################################################################
#######################################################################################
#
if ( ${refine_now} == "maskb02" ) then
  #
  \rm -f ${imagename}-maskb02.tabl
  #
  set old_maskb02 = ${maskb02}
  set loc_maskb02_start = `echo ${refine_maskb02_val} | cut -d\, -f1`
  set loc_maskb02_end   = `echo ${refine_maskb02_val} | cut -d\, -f2`
  set loc_maskb02_step  = `echo ${refine_maskb02_val} | cut -d\, -f3`
  set loc_maskb02_factor = `echo ${refine_maskb02_val} | cut -d\, -f4`
  #
  ${proc_2dx}/linblock "Refining maskb02, from ${loc_maskb02_start} to ${loc_maskb02_end} in steps ${loc_maskb02_step}, fact. ${loc_maskb02_factor}"
  #
  set loc_maskb02 = ${loc_maskb02_start}
  #
  while ( ${loc_maskb02} <= ${loc_maskb02_end} )
     #
     \cp -f FFTIR/cor${imagename}_unbend1_fft.mrc FFTIR/cor${imagename}_fft.mrc
     set maskb02 = ${loc_maskb02}
     source ${proc_2dx}/2dx_unbend2_sub.com
     #
     echo ${loc_maskb02} ${QVAL_local} >> ${imagename}-maskb02.tabl
     # ${proc_2dx}/linblock "unbend2: maskb02 ${loc_maskb02} gives a QVAL of ${QVAL_local}"
     set loc_maskb02 = `echo ${loc_maskb02} ${loc_maskb02_step} ${loc_maskb02_factor} | awk '{s = int( ($1 + $2 ) * $3) } END { print s }'`
  end
  #
  \rm -f TMP442211.tmp
  set best_maskb02 = `cat ${imagename}-maskb02.tabl | ${bin_2dx}/getmax.exe`
  set best_QVal = `cat TMP442211.tmp`
  \rm -f TMP442211.tmp
  #
  ${proc_2dx}/linblock "Best value was found at ${best_maskb02}, with QVal ${best_QVal}. Previously was ${old_maskb02}"
  #
  set maskb02 = ${best_maskb02}
  echo set maskb02 = ${maskb02} >> LOGS/${scriptname}.results
  #
endif
#
#######################################################################################
#######################################################################################
#######################################################################################
#
if ( ${refine_now} == "maskb03" ) then
  #
  \rm -f ${imagename}-maskb03.tabl
  #
  set old_maskb03 = ${maskb03}
  set loc_maskb03_start = `echo ${refine_maskb03_val} | cut -d\, -f1`
  set loc_maskb03_end   = `echo ${refine_maskb03_val} | cut -d\, -f2`
  set loc_maskb03_step  = `echo ${refine_maskb03_val} | cut -d\, -f3`
  set loc_maskb03_factor = `echo ${refine_maskb03_val} | cut -d\, -f4`
  #
  ${proc_2dx}/linblock "Refining maskb03, from ${loc_maskb03_start} to ${loc_maskb03_end} in steps ${loc_maskb03_step}, fact. ${loc_maskb03_factor}"
  #
  set loc_maskb03 = ${loc_maskb03_start}
  #
  while ( ${loc_maskb03} <= ${loc_maskb03_end} )
     #
     \cp -f FFTIR/cor${imagename}_unbend1_fft.mrc FFTIR/cor${imagename}_fft.mrc
     set maskb03 = ${loc_maskb03}
     source ${proc_2dx}/2dx_unbend2_sub.com
     #
     echo ${loc_maskb03} ${QVAL_local} >> ${imagename}-maskb03.tabl
     # ${proc_2dx}/linblock "unbend2: maskb03 ${loc_maskb03} gives a QVAL of ${QVAL_local}"
     set loc_maskb03 = `echo ${loc_maskb03} ${loc_maskb03_step} ${loc_maskb03_factor} | awk '{s = int( ($1 + $2 ) * $3) } END { print s }'`
  end
  #
  \rm -f TMP442211.tmp
  set best_maskb03 = `cat ${imagename}-maskb03.tabl | ${bin_2dx}/getmax.exe`
  set best_QVal = `cat TMP442211.tmp`
  \rm -f TMP442211.tmp
  #
  ${proc_2dx}/linblock "Best value was found at ${best_maskb03}, with QVal ${best_QVal}. Previously was ${old_maskb03}"
  #
  set maskb03 = ${best_maskb03}
  echo set maskb03 = ${maskb03} >> LOGS/${scriptname}.results
  #
endif
#
#######################################################################################
#######################################################################################
#######################################################################################
#
if ( ${refine_now} == "maskb04" ) then
  #
  \rm -f ${imagename}-maskb04.tabl
  #
  set old_maskb04 = ${maskb04}
  set loc_maskb04_start = `echo ${refine_maskb04_val} | cut -d\, -f1`
  set loc_maskb04_end   = `echo ${refine_maskb04_val} | cut -d\, -f2`
  set loc_maskb04_step  = `echo ${refine_maskb04_val} | cut -d\, -f3`
  set loc_maskb04_factor = `echo ${refine_maskb04_val} | cut -d\, -f4`
  #
  ${proc_2dx}/linblock "Refining maskb04, from ${loc_maskb04_start} to ${loc_maskb04_end} in steps ${loc_maskb04_step}, fact. ${loc_maskb04_factor}"
  #
  set loc_maskb04 = ${loc_maskb04_start}
  #
  while ( ${loc_maskb04} <= ${loc_maskb04_end} )
     #
     \cp -f FFTIR/cor${imagename}_unbend1_fft.mrc FFTIR/cor${imagename}_fft.mrc
     set maskb04 = ${loc_maskb04}
     source ${proc_2dx}/2dx_unbend2_sub.com
     #
     echo ${loc_maskb04} ${QVAL_local} >> ${imagename}-maskb04.tabl
     # ${proc_2dx}/linblock "unbend2: maskb04 ${loc_maskb04} gives a QVAL of ${QVAL_local}"
     set loc_maskb04 = `echo ${loc_maskb04} ${loc_maskb04_step} ${loc_maskb04_factor} | awk '{s = int( ($1 + $2 ) * $3) } END { print s }'`
  end
  #
  \rm -f TMP442211.tmp
  set best_maskb04 = `cat ${imagename}-maskb04.tabl | ${bin_2dx}/getmax.exe`
  set best_QVal = `cat TMP442211.tmp`
  \rm -f TMP442211.tmp
  #
  ${proc_2dx}/linblock "Best value was found at ${best_maskb04}, with QVal ${best_QVal}. Previously was ${old_maskb04}"
  #
  set maskb04 = ${best_maskb04}
  echo set maskb04 = ${maskb04} >> LOGS/${scriptname}.results
  #
endif
#
#######################################################################################
#######################################################################################
#######################################################################################
#
if ( ${refine_now} == "maskb05" ) then
  #
  \rm -f ${imagename}-maskb05.tabl
  #
  set old_maskb05 = ${maskb05}
  set loc_maskb05_start = `echo ${refine_maskb05_val} | cut -d\, -f1`
  set loc_maskb05_end   = `echo ${refine_maskb05_val} | cut -d\, -f2`
  set loc_maskb05_step  = `echo ${refine_maskb05_val} | cut -d\, -f3`
  set loc_maskb05_factor = `echo ${refine_maskb05_val} | cut -d\, -f4`
  #
  ${proc_2dx}/linblock "Refining maskb05, from ${loc_maskb05_start} to ${loc_maskb05_end} in steps ${loc_maskb05_step}, fact. ${loc_maskb05_factor}"
  #
  set loc_maskb05 = ${loc_maskb05_start}
  #
  while ( ${loc_maskb05} <= ${loc_maskb05_end} )
     #
     \cp -f FFTIR/cor${imagename}_unbend1_fft.mrc FFTIR/cor${imagename}_fft.mrc
     set maskb05 = ${loc_maskb05}
     source ${proc_2dx}/2dx_unbend2_sub.com
     #
     echo ${loc_maskb05} ${QVAL_local} >> ${imagename}-maskb05.tabl
     # ${proc_2dx}/linblock "unbend2: maskb05 ${loc_maskb05} gives a QVAL of ${QVAL_local}"
     set loc_maskb05 = `echo ${loc_maskb05} ${loc_maskb05_step} ${loc_maskb05_factor} | awk '{s = int( ($1 + $2 ) * $3) } END { print s }'`
  end
  #
  \rm -f TMP442211.tmp
  set best_maskb05 = `cat ${imagename}-maskb05.tabl | ${bin_2dx}/getmax.exe`
  set best_QVal = `cat TMP442211.tmp`
  \rm -f TMP442211.tmp
  #
  ${proc_2dx}/linblock "Best value was found at ${best_maskb05}, with QVal ${best_QVal}. Previously was ${old_maskb05}"
  #
  set maskb05 = ${best_maskb05}
  echo set maskb05 = ${maskb05} >> LOGS/${scriptname}.results
  #
endif
#

#######################################################################################
#######################################################################################
#######################################################################################
#
if ( ${refine_now} == "boxb1" ) then
  #
  \rm -f ${imagename}-boxb1.tabl
  #
  set old_boxb1 = ${boxb1}
  set loc_boxb1_start = `echo ${refine_boxb1_val} | cut -d\, -f1`
  set loc_boxb1_end   = `echo ${refine_boxb1_val} | cut -d\, -f2`
  set loc_boxb1_step  = `echo ${refine_boxb1_val} | cut -d\, -f3`
  set loc_boxb1_factor = `echo ${refine_boxb1_val} | cut -d\, -f4`
  #
  ${proc_2dx}/linblock "Refining boxb1, from ${loc_boxb1_start} to ${loc_boxb1_end} in steps ${loc_boxb1_step}, fact. ${loc_boxb1_factor}"
  #
  set loc_boxb1 = ${loc_boxb1_start}
  #
  while ( ${loc_boxb1} <= ${loc_boxb1_end} )
     #
     \cp -f FFTIR/cor${imagename}_unbend1_fft.mrc FFTIR/cor${imagename}_fft.mrc
     set boxb1 = ${loc_boxb1}
     source ${proc_2dx}/2dx_unbend2_sub.com
     #
     echo ${loc_boxb1} ${QVAL_local} >> ${imagename}-boxb1.tabl
     # ${proc_2dx}/linblock "unbend2: boxb1 ${loc_boxb1} gives a QVAL of ${QVAL_local}"
     set loc_boxb1 = `echo ${loc_boxb1} ${loc_boxb1_step} ${loc_boxb1_factor} | awk '{s = int( ($1 + $2 ) * $3) } END { print s }'`
  end
  #
  \rm -f TMP442211.tmp
  set best_boxb1 = `cat ${imagename}-boxb1.tabl | ${bin_2dx}/getmax.exe`
  set best_QVal = `cat TMP442211.tmp`
  \rm -f TMP442211.tmp
  #
  ${proc_2dx}/linblock "Best value was found at ${best_boxb1}, with QVal ${best_QVal}. Previously was ${old_boxb1}"
  #
  set boxb1 = ${best_boxb1}
  #
  echo set boxb1 = ${boxb1} >> LOGS/${scriptname}.results
  #
endif
#
#######################################################################################
#######################################################################################
#######################################################################################
#
if ( ${refine_now} == "boxb2" ) then
  #
  \rm -f ${imagename}-boxb2.tabl
  #
  set old_boxb2 = ${boxb2}
  set loc_boxb2_start = `echo ${refine_boxb2_val} | cut -d\, -f1`
  set loc_boxb2_end   = `echo ${refine_boxb2_val} | cut -d\, -f2`
  set loc_boxb2_step  = `echo ${refine_boxb2_val} | cut -d\, -f3`
  set loc_boxb2_factor = `echo ${refine_boxb2_val} | cut -d\, -f4`
  #
  ${proc_2dx}/linblock "Refining boxb2, from ${loc_boxb2_start} to ${loc_boxb2_end} in steps ${loc_boxb2_step}, fact. ${loc_boxb2_factor}"
  #
  set loc_boxb2 = ${loc_boxb2_start}
  #
  while ( ${loc_boxb2} <= ${loc_boxb2_end} )
     #
     \cp -f FFTIR/cor${imagename}_unbend1_fft.mrc FFTIR/cor${imagename}_fft.mrc
     set boxb2 = ${loc_boxb2}
     source ${proc_2dx}/2dx_unbend2_sub.com
     #
     echo ${loc_boxb2} ${QVAL_local} >> ${imagename}-boxb2.tabl
     # ${proc_2dx}/linblock "unbend2: boxb2 ${loc_boxb2} gives a QVAL of ${QVAL_local}"
     set loc_boxb2 = `echo ${loc_boxb2} ${loc_boxb2_step} ${loc_boxb2_factor} | awk '{s = int( ($1 + $2 ) * $3) } END { print s }'`
  end
  #
  \rm -f TMP442211.tmp
  set best_boxb2 = `cat ${imagename}-boxb2.tabl | ${bin_2dx}/getmax.exe`
  set best_QVal = `cat TMP442211.tmp`
  \rm -f TMP442211.tmp
  #
  ${proc_2dx}/linblock "Best value was found at ${best_boxb2}, with QVal ${best_QVal}. Previously was ${old_boxb2}"
  #
  set boxb2 = ${best_boxb2}
  #
  echo set boxb2 = ${boxb2} >> LOGS/${scriptname}.results
  #
endif
#
#######################################################################################
#######################################################################################
#######################################################################################
#
#
if ( ${refine_now} == "maskb01_boxb1" ) then
  #
  \rm -f ${imagename}-maskb01.tabl
  \rm -f ${imagename}-boxb1.tabl
  \rm -f SCRATCH/${imagename}-maskb01_boxb1.tabl
  #
  # you only want run unbend II for one round with maskb01 and boxb01 and not with a varying maskb02
  set locround = 1
  #
  set old_maskb01 = ${maskb01}
  set loc_maskb01_start = `echo ${refine_maskb01_val} | cut -d\, -f1`
  set loc_maskb01_end   = `echo ${refine_maskb01_val} | cut -d\, -f2`
  set loc_maskb01_step  = `echo ${refine_maskb01_val} | cut -d\, -f3`
  set loc_maskb01_factor = `echo ${refine_maskb01_val} | cut -d\, -f4`
  #
  set old_boxb1 = ${boxb1}
  set loc_boxb1_start = `echo ${refine_boxb1_val} | cut -d\, -f1`
  set loc_boxb1_end   = `echo ${refine_boxb1_val} | cut -d\, -f2`
  set loc_boxb1_step  = `echo ${refine_boxb1_val} | cut -d\, -f3`
  set loc_boxb1_factor = `echo ${refine_boxb1_val} | cut -d\, -f4`
  #
  ${proc_2dx}/linblock "Refining maskb01 and boxb1 simultaneously."
  ${proc_2dx}/linblock "Refining maskb01, from ${loc_maskb01_start} to ${loc_maskb01_end} in steps ${loc_maskb01_step}, fact. ${loc_maskb01_factor}"
  ${proc_2dx}/linblock "Refining boxb1, from ${loc_boxb1_start} to ${loc_boxb1_end} in steps ${loc_boxb1_step}, fact. ${loc_boxb1_factor}"
  #
  set loc_maskb01 = ${loc_maskb01_start}
  #
  while ( ${loc_maskb01} <= ${loc_maskb01_end} )
     #
    set maskb01 = ${loc_maskb01}
    set loc_boxb1 = ${loc_boxb1_start}
    set table_row = ""
    #
    while ( ${loc_boxb1} <= ${loc_boxb1_end} )
       #
      \cp -f FFTIR/cor${imagename}_unbend1_fft.mrc FFTIR/cor${imagename}_fft.mrc
       set boxb1 = ${loc_boxb1}
       source ${proc_2dx}/2dx_unbend2_sub.com
       #
       echo ${loc_maskb01} ${QVAL_local} >> ${imagename}-maskb01.tabl
       echo ${loc_boxb1} ${QVAL_local} >> ${imagename}-boxb1.tabl
       set table_row = `echo -n "${table_row} ${QVAL_local}"`
       #${proc_2dx}/linblock "unbend2: maskb01 ${loc_maskb01} and boxb1 ${loc_boxb1} gives a QVAL of ${QVAL_local}"
       set loc_boxb1 = `echo ${loc_boxb1} ${loc_boxb1_step} ${loc_boxb1_factor} | awk '{s = int( ($1 + $2 ) * $3) } END { print s }'`
    end
    echo ${table_row} >> SCRATCH/${imagename}-maskb01_boxb1.tabl
    set loc_maskb01 = `echo ${loc_maskb01} ${loc_maskb01_step} ${loc_maskb01_factor} | awk '{s = int( ($1 + $2 ) * $3) } END { print s }'`
  end
  #
  \mv -f SCRATCH/${imagename}-maskb01_boxb1.tabl SCRATCH/${imagename}-maskb01_boxb1.txt
  echo "# IMAGE: SCRATCH/${imagename}-maskb01_boxb1.txt  <TXT: QVal Table>" >> LOGS/${scriptname}.results
  #  
  \rm -f TMP442211.tmp
  set best_maskb01 = `cat ${imagename}-maskb01.tabl | ${bin_2dx}/getmax.exe`
  \rm -f TMP442211.tmp
  set best_boxb1 = `cat ${imagename}-boxb1.tabl | ${bin_2dx}/getmax.exe`
  set best_QVal = `cat TMP442211.tmp`
  \rm -f TMP442211.tmp
  #
  ${proc_2dx}/linblock "Best value was found at ${best_boxb1}, with QVal ${best_QVal}. Previously was ${old_boxb1}"
  #
  set maskb01 = ${best_maskb01}
  set boxb1 = ${best_boxb1}
  echo set maskb01 = ${maskb01} >> LOGS/${scriptname}.results
  echo set boxb1 = ${boxb1} >> LOGS/${scriptname}.results
  #
endif
#

