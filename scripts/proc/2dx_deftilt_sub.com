#
###########################################################
#
#   2dx_deftilt_sub.com
#
# This is not an independent script. It is only called form other scripts.
#
###########################################################
#
echo New coordinates = ${newX},${newY} 
echo Reference coord = ${refX},${refY}
echo dfdist = ${dfdist}
echo inoast = ${inoast}
set second = 0
echo second = ${second}
if ( ${?sub_tilesize} == 0 ) then
  set sub_tilesize = 256
endif
echo ${sub_tilesize}
#
if ( ${second} == '3' ) then
  set inimage = CUT/${image}_red_center.mrc
  set outimage = CUT/${image}_red_center_ps.mrc
  set outlabel = "CUT-PS_Center"
else
  set inimage = CUT/${image}_${newX}_${newY}.mrc
  set outimage = CUT/${image}_${newX}_${newY}_ps.mrc
  set outlabel = "CUT-PS_${newX},${newY}"
endif
#
###############################################
#
if ( ${refX} == '0' || ${refY} == '0' ) then
  #
  set dfmid = `echo ${defocus} | sed 's/,/ /g' | awk '{s = ($1 + $2 ) / 2 } END {print s}'`
  set dfref = `echo ${defocus}`
  set dfstart = ${df_start}
  set dfend = ${df_end}
  set dfstep = ${df_step}
  set dfdast  = ${df_dast}
  #
else
  #
  ###########################################################
  # Get reference defocus value from table:
  ###########################################################
  #
  \rm -f TMP.txt
  #
  ${bin_2dx}/2dx_maintain_defocus_table.exe << eot
${defocus_pos_file}
${refX},${refY}
TMP.txt
1
eot
  #
  set dfmid = `cat TMP.txt`
  \rm -f TMP.txt
  #
  set dfmin = 500.0
  echo dfmin = ${dfmin}
  echo defocus in image center = ${defocus}
  echo defocus mid value at reference location = ${dfmid}
  #
  set astigval = `echo ${defocus} | sed 's/,/ /g' | awk '{ s = ( $2 - $1 ) / 2.0 } END { print s }'`
  set astigang = `echo ${defocus} | sed 's/,/ /g' | awk '{ s = $3 } END { print s }'`
  set dfref1 = `echo ${dfmid} ${astigval} | awk '{ s = $1 - $2 } END { print s }'`
  set dfref2 = `echo ${dfmid} ${astigval} | awk '{ s = $1 + $2 } END { print s }'`
  set dfref = `echo ${dfref1},${dfref2},${astigang}`
  echo "Reference defocus is ${dfref}"
  #
  ###########################################################
  ${proc_2dx}/lin "Reference ${refX},${refY} has defocus of ${dfref} with mid ${dfmid}"
  ###########################################################
  #
  set dftmp   = `echo ${dfmid} ${dfdist} | awk '{s = $1 - $2 } END {print s}'`
  set dfstart = `echo ${dftmp} ${dfmin} | awk '{ if ($1 < $2) { s = $2 } else { s = $1 }} END {print s}'`
  set dfend   = `echo ${dfmid} ${dfdist} | awk '{s = $1 + $2 } END {print s}'`
  set dfstep = 10.0
  set dfdast = 100.0
  #
endif
#
echo ":Search range is ${dfstart}, ${dfend}, ${dfstep}, ${dfdast}"
#
#############################################################################
#                                                                           #
${proc_2dx}/lin "2dx_ctffind3 - search the defocus"
#                                                                           #
#############################################################################
#
\rm -f SCRATCH/2dx_ctffind3_result.tmp
#
\rm -f ${outimage}
#
if ( ${downsample} == '1' ) then
  # The image tiles are 2-times downsampled:
  set locstepdigitizer = `echo ${stepdigitizer} | awk '{ s = 2 * $1 } END { print s }'`
else
  set locstepdigitizer = ${stepdigitizer}
endif
#
if ( ${refX} == '0' || ${refY} == '0' ) then
  set inoast = 0
else
  set inoast = 1
endif
if ( ${use_paralellized} == "y" ) then
  set progname = 2dx_ctffind3.exe
else
  set progname = 2dx_ctffind3_noOMP.exe
endif
#
echo " "
echo "Calling:"  
echo "${bin_2dx}/${progname}"
echo "${inimage}"
echo "${outimage}"
echo "${CS},${KV},${ampcon},${magnification},${locstepdigitizer}"
echo "${sub_tilesize},${resoma},${resolim},${dfstart},${dfend},${dfstep},${df_dast}"
echo "${inoast},${dfref},${drms1}"
echo " "
#
${bin_2dx}/${progname} << eof
${inimage}
${outimage}
${CS},${KV},${ampcon},${magnification},${locstepdigitizer}
${sub_tilesize},${resoma},${resolim},${dfstart},${dfend},${dfstep},${df_dast}
${inoast},${dfref},${drms1}
eof
#
#
\rm -f tmp.map
${bin_2dx}/2dx_processor.exe --mrcin ${outimage} --mrcout tmp.map
\rm -f ${outimage}
#
${bin_2dx}/labelh.exe << eot
tmp.map
40
${outimage}
eot
\rm -f tmp.map
#
if ( ${debugmode} == "y" ) then
  echo "# IMAGE: "${outimage}" <"${outlabel}">" >> LOGS/${scriptname}.results
endif
#
#######################################################
#PARAMETER: for ctffind3.exe
# card1: Input file name for image
# card2: Output file name to check result
# card3:  CS[mm], HT[kV], AmpCnst, XMAG, DStep[um]
# card4:  JXYZ(1),RESMIN,RESMAX,DFMIN,DFMAX,FSTEP,DAST
#
#######################################################
#
  if ( ! -e SCRATCH/2dx_ctffind3_result.tmp ) then
    ${proc_2dx}/linblock "WARNING: 2dx_deftilt_sub.com: ERROR: SCRATCH/2dx_ctffind3_result.tmp does not exist."
    if ( ${newX} == ${centerX} && ${newY} == ${centerY} ) then
       ${proc_2dx}/protest "ABORTING."
    endif
    set newdef = ${dfmid}
    # set drms1 = 
    set def1 = ${dfref1}
    set def2 = ${dfref2}
    set ang  = ${astigang}
  else
    set newdef = `cat SCRATCH/2dx_ctffind3_result.tmp | head -1`
    set drms1 = `cat SCRATCH/2dx_ctffind3_result.tmp | tail -1`
    set def1 = `echo $newdef | awk '{ s = $1 } END { print s }'`
    set def2 = `echo $newdef | awk '{ s = $2 } END { print s }'`
    set ang  = `echo $newdef | awk '{ s = $3 } END { print s }'`
    \rm SCRATCH/2dx_ctffind3_result.tmp
  endif
  #
  if ( ${newX} == ${centerX} && ${newY} == ${centerY}) then
    set defocus = `echo ${def1},${def2},${ang}`
    ${proc_2dx}/linblock "new central defocus ${defocus}"
    echo ": Central Defocus = "${defocus} > TMP.txt
    echo `tail -n 6 ${defocus_pos_file}`     
    #tail -n ${defocus_tiles_count} ${defocus_pos_file} >> TMP.txt
    tail -n 7 ${defocus_pos_file} >> TMP.txt
    \mv -f TMP.txt ${defocus_pos_file}
    echo ": Central Defocus = "${defocus} > TMP.txt
    tail -n 7 ${defocus_pos_select_file} >> TMP.txt
    \mv -f TMP.txt ${defocus_pos_select_file}
    #
  endif
  #
  #############################################################################
  # Write defocus value into defocus table:
  #############################################################################
  #
  set defave = `echo ${def1} ${def2} | awk '{ s = ( $1 + $2 ) / 2.0 } END { print s }'`
  # set defave = `echo ${defave} ${dfstart} | awk '{ if ( $1 <= $2 ) { s = 0 } else { s = $1 }} END { print s }'` 
  # set defave = `echo ${defave} ${dfend}   | awk '{ if ( $1 >= $2 ) { s = 0 } else { s = $1 }} END { print s }'` 
  #
  \rm -f TMP.txt
  #
echo ": ${defocus_pos_file}"
echo ": ${newX},${newY}"
echo ": TMP.txt"
echo ": 0"
echo ": ${defave}"
  ${bin_2dx}/2dx_maintain_defocus_table.exe << eot
${defocus_pos_file}
${newX},${newY}
TMP.txt
0
${defave}
eot
  #
  #############################################################################
  ${proc_2dx}/lin "average defocus = ${defave} inserted in position ${newX},${newY}"
  #############################################################################
  #
  \mv -f TMP.txt ${defocus_pos_file}
  #
#
