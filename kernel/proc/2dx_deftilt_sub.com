#
#   2dx_deftilt.2.com
#
echo New coordinates = ${newX},${newY} 
echo Reference coord = ${refX},${refY}
echo dfdist = ${dfdist}
echo inoast = ${inoast}
set second = 0
echo second = ${second}
#
if ( ${second} == '3' ) then
  set inimage = CUT/${image}.red.center.mrc
  set outimage = CUT/${image}.red.center.ps.mrc
  set outlabel = "CUT-PS_Center"
else
  set inimage = CUT/${image}.${newX}.${newY}.mrc
  set outimage = CUT/${image}.${newX}.${newY}.ps.mrc
  set outlabel = "CUT-PS_${newX},${newY}"
endif
#
###############################################
#
if ( ${refX} == '0' || ${refY} == '0' ) then
  set dfmid = `echo ${defocus} | sed 's/,/ /g' | awk '{s = ($1 + $2 ) / 2 } END {print s}'`
else
  set ypos = `echo ${refY} | awk '{ s = 159 + $1 } END { print s } '`
  set xanf = `echo ${refX} | awk '{ s = 6 + ( $1 - 1 ) * 11 } END { print s } '`
  set xend = `echo ${xanf} | awk '{ s = $1 + 9 } END { print s } '`
  ###############################################
  ###############################################
  ###############################################
  ###############################################
  # set dfmid = `getlinepos DATAFILE.dat ${ypos} ${xanf} ${xend}`
  set dfmid = `echo ${defocus} | sed 's/,/ /g' | awk '{s = ($1 + $2 ) / 2 } END {print s}'`
  ###############################################
  ###############################################
  ###############################################
  ###############################################
endif
echo dfmid = ${dfmid}
#
set dfmin = 1000.0
echo dfmin = ${dfmin}
#
set dftmp   = `echo ${dfmid} ${dfdist} | awk '{s = $1 - $2 } END {print s}'`
set dfstart = `echo ${dftmp} ${dfmin} | awk '{ if ($1 < $2) { s = $2 } else { s = $1 }} END {print s}'`
set dfend   = `echo ${dfmid} ${dfdist} | awk '{s = $1 + $2 } END {print s}'`
echo dfstart,dfend = ${dfstart} , ${dfend}
#
if ( ${inoast} == 0 ) then
  set dfstep = 100.0
else
  set dfstep = 10.0
endif
echo dfstep = ${dfstep}
#
set resohi = ${RESMIN}
echo resohi = ${resohi}
#
set resolo = ${RESMAX}
echo resolo = $resolo
set resolim = `echo ${resolo} | awk '{if ($1 > 5.0) { s = 5.0 } else { s = $1 }} END { print s }'`
echo resolim = $resolim
#
echo phacon = ${phacon}
set ampcon = `echo ${phacon} | awk '{s=1.0-$1} END {print s}'`
echo ampcon = ${ampcon}
#
#############################################################################
#                                                                           #
${proc_2dx}/lin "CTFFIND2 - search the defocus"
#                                                                           #
#############################################################################
#
\rm -f SCRATCH/2dx_ctffind3.result.tmp
#
\rm -f ${outimage}
#
${bin_2dx}/2dx_ctffind3.exe << eof
${inimage}
${outimage}
${CS},${KV},${ampcon},${magnification},${stepdigitizer}
128,${resohi},${resolim},${dfstart},${dfend},${dfstep}
${inoast},${defocus}
eof
#
echo "# IMAGE: "${outimage}" <"${outlabel}">" >> LOGS/${scriptname}.results
#
#######################################################
#PARAMETER: for ctffind3.exe
# card1: Input file name for image
# card2: Output file name to check result
# card3:  CS[mm], HT[kV], AmpCnst, XMAG, DStep[um]
# card4:  JXYZ(1),RESMIN,RESMAX,DFMIN,DFMAX,FSTEP
# card5:  inoast,dfx,dfy,dfast
#######################################################
#
  if ( ! -e SCRATCH/2dx_ctffind3.result.tmp ) then
    ${proc_2dx}/protest "2dx_deftilt_sub.com: ERROR: SCRATCH/2dx_ctffind3.result.tmp does not exist."
  endif
  set newdef = `cat SCRATCH/2dx_ctffind3.result.tmp`
  set def1 = `echo $newdef | awk '{s=$1} END {print s}'`
  set def2 = `echo $newdef | awk '{s=$2} END {print s}'`
  set ang  = `echo $newdef | awk '{s=$3} END {print s}'`
  \rm SCRATCH/2dx_ctffind3.result.tmp
  if ( ${newX} == '4' && ${newY} == '4' ) then
    set newdef = `echo ${def1},${def2},${ang}`
    ${proc_2dx}/linblock "new central defocus ${newdef} (not yet used)"
    echo "Central Defocus = "${newdef} > TMP.txt
    tail -n 7 ${defocus_pos_file} >> TMP.txt
    #
  else
    set defave = `echo ${def1} ${def2} | awk '{ s = ( $1 + $2 ) / 2.0 } END { print s }'`
    set defave = `echo ${defave} ${dfstart} | awk '{ if ( $1 <= $2 ) { s = 0 } else { s = $1 }} END { print s }'` 
    set defave = `echo ${defave} ${dfend}   | awk '{ if ( $1 >= $2 ) { s = 0 } else { s = $1 }} END { print s }'` 
    #
    \rm -f TMP.txt
    #
    ${bin_2dx}/2dx_maintain_defocus_table.exe << eot
${defocus_pos_file}
TMP.txt
${newX},${newY}
${defave}
eot
    #
    ${proc_2dx}/linblock "average defocus = ${defave} inserted in position ${newX},${newY}"
  endif
  #
  \mv -f TMP.txt ${defocus_pos_file}
  #
#
