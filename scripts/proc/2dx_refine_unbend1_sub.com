#
#  2dx_refine_unbend1_sub.com
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
#
#
if ( ${refine_now} == "maska" ) then
  #
  \rm -f ${imagename}-maska.tabl
  #
  set old_maska = ${maska}
  set loc_maska_start = `echo ${refine_maska_val} | cut -d\, -f1`
  set loc_maska_end   = `echo ${refine_maska_val} | cut -d\, -f2`
  set loc_maska_step  = `echo ${refine_maska_val} | cut -d\, -f3`
  set loc_maska_factor = `echo ${refine_maska_val} | cut -d\, -f4`
  #
  #################################################################################
  ${proc_2dx}/linblock "Refining maska, from ${loc_maska_start} to ${loc_maska_end} in steps ${loc_maska_step}, fact. ${loc_maska_factor}"
  #################################################################################
  #
  set loc_maska = ${loc_maska_start}
  #
  while ( ${loc_maska} <= ${loc_maska_end} )
     #
     set maska = ${loc_maska}
     source ${proc_2dx}/2dx_unbend1_sub.com
     #
     echo ${loc_maska} ${QVAL_local} >> ${imagename}-maska.tabl
     # ${proc_2dx}/linblock "unbend1: maska ${loc_maska} gives a QVAL of ${QVAL_local}"
     set loc_maska = `echo ${loc_maska} ${loc_maska_step} ${loc_maska_factor} | awk '{s = int( ($1 + $2 ) * $3) } END { print s }'`
  end
  #
  \rm -f TMP442211.tmp
  set best_maska = `cat ${imagename}-maska.tabl | ${bin_2dx}/getmax.exe`
  set best_QVal = `cat TMP442211.tmp`
  \rm -f TMP442211.tmp
  #
  ${proc_2dx}/linblock "Best value was found at ${best_maska}, with QVal ${best_QVal}. Previously was ${old_maska}"
  #
  set maska = ${best_maska}
  echo set maska = ${maska} >> LOGS/${scriptname}.results
  #
endif
#
#################################################################################
#################################################################################
#################################################################################
#
if ( ${refine_now} == "boxa1" ) then
  #
  \rm -f ${imagename}-boxa1.tabl
  #
  set old_boxa1 = ${boxa1}
  set loc_boxa1_start = `echo ${refine_boxa1_val} | cut -d\, -f1`
  set loc_boxa1_end   = `echo ${refine_boxa1_val} | cut -d\, -f2`
  set loc_boxa1_step  = `echo ${refine_boxa1_val} | cut -d\, -f3`
  set loc_boxa1_factor = `echo ${refine_boxa1_val} | cut -d\, -f4`
  #
  #################################################################################
  ${proc_2dx}/linblock "Refining boxa1, from ${loc_boxa1_start} to ${loc_boxa1_end} in steps ${loc_boxa1_step}, fact. ${loc_boxa1_factor}"
  #################################################################################
  #
  set loc_boxa1 = ${loc_boxa1_start}
  #
  while ( ${loc_boxa1} <= ${loc_boxa1_end} )
     #
     set boxa1 = ${loc_boxa1}
     source ${proc_2dx}/2dx_unbend1_sub.com
     #
     echo ${loc_boxa1} ${QVAL_local} >> ${imagename}-boxa1.tabl
     # ${proc_2dx}/linblock "unbend1: boxa1 ${loc_boxa1} gives a QVAL of ${QVAL_local}"
     set loc_boxa1 = `echo ${loc_boxa1} ${loc_boxa1_step} ${loc_boxa1_factor} | awk '{s = int( ($1 + $2 ) * $3) } END { print s }'`
  end
  #
  \rm -f TMP442211.tmp
  set best_boxa1 = `cat ${imagename}-boxa1.tabl | ${bin_2dx}/getmax.exe`
  set best_QVal = `cat TMP442211.tmp`
  \rm -f TMP442211.tmp
  #
  ${proc_2dx}/linblock "Best value was found at ${best_boxa1}, with QVal ${best_QVal}. Previously was ${old_boxa1}"
  #
  set boxa1 = ${best_boxa1}
  #
  echo set boxa1 = ${boxa1} >> LOGS/${scriptname}.results
  #
endif
#
#################################################################################
#################################################################################
#################################################################################
#
if ( ${refine_now} == "maska_boxa1" ) then
  #
  \rm -f ${imagename}-maska.tabl
  \rm -f ${imagename}-boxa1.tabl
  \rm -f SCRATCH/${imagename}-maska_boxa1.txt
  #
  set old_maska = ${maska}
  set loc_maska_start = `echo ${refine_maska_val} | cut -d\, -f1`
  set loc_maska_end   = `echo ${refine_maska_val} | cut -d\, -f2`
  set loc_maska_step  = `echo ${refine_maska_val} | cut -d\, -f3`
  set loc_maska_factor = `echo ${refine_maska_val} | cut -d\, -f4`
  #
  set old_boxa1 = ${boxa1}
  set loc_boxa1_start = `echo ${refine_boxa1_val} | cut -d\, -f1`
  set loc_boxa1_end   = `echo ${refine_boxa1_val} | cut -d\, -f2`
  set loc_boxa1_step  = `echo ${refine_boxa1_val} | cut -d\, -f3`
  set loc_boxa1_factor = `echo ${refine_boxa1_val} | cut -d\, -f4`
  #
  #################################################################################
  ${proc_2dx}/linblock "Refining maska and boxa1 in an exhaustive grid search."
  ${proc_2dx}/linblock " maska from ${loc_maska_start} to ${loc_maska_end} in steps ${loc_maska_step}, fact. ${loc_maska_factor}"
  echo  "maska from ${loc_maska_start} to ${loc_maska_end} in steps ${loc_maska_step}, fact. ${loc_maska_factor}" >> SCRATCH/${imagename}-maska_boxa1.txt 
  ${proc_2dx}/linblock " boxa1 from ${loc_boxa1_start} to ${loc_boxa1_end} in steps ${loc_boxa1_step}, fact. ${loc_boxa1_factor}"
  echo  "boxa1 from ${loc_boxa1_start} to ${loc_boxa1_end} in steps ${loc_boxa1_step}, fact. ${loc_boxa1_factor}" >> SCRATCH/${imagename}-maska_boxa1.txt 
  #################################################################################
  #
  set loc_maska = ${loc_maska_start}
  #
  while ( ${loc_maska} <= ${loc_maska_end} )
    #
    set maska = ${loc_maska}
    set loc_boxa1 = ${loc_boxa1_start}
    set table_row = ""
    #
    while ( ${loc_boxa1} <= ${loc_boxa1_end} )
       #
       set boxa1 = ${loc_boxa1}
       source ${proc_2dx}/2dx_unbend1_sub.com
       #
       echo ${loc_maska} ${QVAL_local} >> ${imagename}-maska.tabl
       echo ${loc_boxa1} ${QVAL_local} >> ${imagename}-boxa1.tabl
       set table_row = `echo -n "${table_row} ${QVAL_local}"`
       # ${proc_2dx}/linblock "unbend1: maska ${loc_maska} boxa1 ${loc_boxa1} gives a QVAL of ${QVAL_local}"
       set loc_boxa1 = `echo ${loc_boxa1} ${loc_boxa1_step} ${loc_boxa1_factor} | awk '{s = int( ($1 + $2 ) * $3) } END { print s }'`
    end
    echo ${table_row} >> SCRATCH/${imagename}-maska_boxa1.txt
    set loc_maska = `echo ${loc_maska} ${loc_maska_step} ${loc_maska_factor} | awk '{s = int( ($1 + $2 ) * $3) } END { print s }'`
  end
  #
  echo "# IMAGE: SCRATCH/${imagename}-maska_boxa1.txt  <TXT: QVal Table>" >> LOGS/${scriptname}.results
  #
  \rm -f TMP442211.tmp
  set best_maska = `cat ${imagename}-maska.tabl | ${bin_2dx}/getmax.exe`
  \rm -f TMP442211.tmp
  set best_boxa1 = `cat ${imagename}-boxa1.tabl | ${bin_2dx}/getmax.exe`
  set best_QVal = `cat TMP442211.tmp`
  \rm -f TMP442211.tmp
  #
  ${proc_2dx}/linblock "Old maska and boxa1 values were ${old_maska}, ${old_boxa1}."
  ${proc_2dx}/linblock "New best values are ${best_maska} and ${best_boxa1}, with QVal ${best_QVal}."
  #
  set maska = ${best_maska}
  set boxa1 = ${best_boxa1}
  #
  echo set maska = ${maska} >> LOGS/${scriptname}.results
  echo set boxa1 = ${boxa1} >> LOGS/${scriptname}.results
  #
endif
#
#############################################################################
