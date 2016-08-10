#
#############################################################################
#                                                                           #
# This is not an executable script.                                         #
#                                                                           #
# This should be sourced from the calling script.                           #
#                                                                           #
# (C) 2dx.org, GNU Plublic License.                                         #
#                                                                           #
# Created..........: 01/03/2007                                             #
# Last Modification: 01/03/2007                                             #
# Author...........: 2dx.org                                                #
#                                                                           #
#############################################################################
#
echo bin_2dx = ${bin_2dx}
echo proc_2dx = ${proc_2dx}
echo ccp4 = ${ccp4}
echo bin_ccp4 = ${bin_ccp4}
#
  #
  if ( ${scalimamp3d_rref} != "0" ) then
    #
    #############################################################################
    ${proc_2dx}/linblock "SCALIMAMP3D - to scale image amplitudes to selected reference data"
    #############################################################################
    #
    if ( ${scalimamp3d_rref} == "1" ) then 
      set scalimamp3d_refdata = FF
    endif
    if ( ${scalimamp3d_rref} == "2" ) then 
      set scalimamp3d_refdata = BT
    endif
    if ( ${scalimamp3d_rref} == "3" ) then 
      set scalimamp3d_refdata = LZ
      echo ":: "
      ${proc_2dx}/linblock "WARNING: SCALIMAMP3D option LZ is not yet working"
      echo ":: "
    endif
    if ( ${scalimamp3d_rref} == "4" ) then 
      set scalimamp3d_refdata = BR
    endif
    set bextra = 0.0
    #
    \rm -f SCALIMAMP3D.DAT
    set scalimamp_out = APH/merge_after_scalimamp3d.aph
    setenv OUT ${scalimamp_out}
    \rm -f ${scalimamp_out}
    #
    set RESMAX_local = ${RESMAX}
    echo ": "
    echo ": calling 2dx_scalimamp3d.exe with the following parameters:"
    echo ": ${scalimamp3d_refdata}"
    echo ": 0,F,${scalimamp3d_BXYMINMAX},${scalimamp3d_BZMINMAX} ! NPROG,TWOFOLD,BXYMIN,BXYMAX,BZMIN,BZMAX"
    echo ": ${RESMAX_local}, ${zstarrange_real}, ${scalimamp3d_BEXTRA} ! RESLIMXY, RESLIMZ, BEXTRA"
    echo ": APH/merge.aph"
    echo ": ${realcell}, ${realang}, ${RESMAX} !  A,B,GAMMA,RESOL"
    echo ":  "
    #
    ${bin_2dx}/2dx_scalimamp3d.exe << eot 
${scalimamp3d_refdata}
0,F,${scalimamp3d_BXYMINMAX},${scalimamp3d_BZMINMAX} ! NPROG,TWOFOLD,BXYMIN,BXYMAX,BZMIN,BZMAX
${RESMAX_local}, ${zstarrange_real}, ${scalimamp3d_BEXTRA} ! RESLIMXY, RESLIMZ, BEXTRA
APH/merge.aph
${realcell}, ${realang}, ${RESMAX_local} !  A,B,GAMMA,RESOL
eot
    #
    if ( ! -e ${scalimamp_out} ) then
      ${proc_2dx}/protest "ERROR: Problem in scalimamp3d.exe."
    else    
      \mv -f APH/merge.aph APH/merge_before_scalimamp3d.aph
      \mv -f ${scalimamp_out} APH/merge.aph
      echo "# IMAGE: APH/merge.aph <APH: merge.aph after scalimamp3d>" >> LOGS/${scriptname}.results
    endif
    #
    echo " "
    ${proc_2dx}/lin "-"
    echo " "
    #
  endif
  #
  #############################################################################
  ${proc_2dx}/linblock "LATLINPRESCAL - to apply CTF correction and weight calculation"
  #############################################################################
  #
  \rm -f fort.1
  \rm -f fort.3
  #
  \ln -s APH/merge.aph fort.1
  #
  echo "Calling: " > LOGS/latlinprescal.log
  echo "======== " >> LOGS/latlinprescal.log
  echo " " >> LOGS/latlinprescal.log
  #
  echo "  ${bin_2dx}/2dx_latlinprescal.exe << eot >> LOGS/latlinprescal.log " >> LOGS/latlinprescal.log
  echo "1001,${zminmax} ! NSER,ZMIN,ZMAX " >> LOGS/latlinprescal.log
  echo "${MergeIQMAX}               ! IQMAX " >> LOGS/latlinprescal.log
  echo "${max_amp_correction}       ! Max_Amp_Correction" >> LOGS/latlinprescal.log
  echo "${AMP_Scale_Factor}         ! AMP_Background_Scale Factor" >> LOGS/latlinprescal.log
  echo "eot " >> LOGS/latlinprescal.log
  echo " " >> LOGS/latlinprescal.log
  echo "Running: " >> LOGS/latlinprescal.log
  echo "======== " >> LOGS/latlinprescal.log
  #
  ${bin_2dx}/2dx_latlinprescal.exe << eot >> LOGS/latlinprescal.log
1001,${zminmax} ! NSER,ZMIN,ZMAX
${MergeIQMAX}               ! IQMAX
${max_amp_correction}       ! Max_Amp_Correction
${AMP_Scale_Factor}         ! AMP_Background_Scale Factor
eot
  #
  \rm -f fort.1
  echo "################################################"
  echo "################################################"
  echo "output in file LOGS/latlinprescal.log"
  echo "################################################"
  echo "################################################"
  #
  if ( -e fort.3 ) then
    \mv -f fort.3 APH/latlines.dat
    echo "# IMAGE: LOGS/latlinprescal.log <LOG: latlinprescal output>" >> LOGS/${scriptname}.results
    echo "# IMAGE: APH/latlines.dat <Latline after prescal [H,K,Z,A,P,SigA,SigP,IQ]>" >> LOGS/${scriptname}.results
  else
    ${proc_2dx}/protest "ERROR: latlines.dat does not exist."
  endif
  #
  echo " "
  ${proc_2dx}/lin "-"
  echo " "
  #
#
set Reflections_Unique = `wc -l APH/latlines.dat | cut -f1`
echo "set Reflections_Unique = ${Reflections_Unique}" >> LOGS/${scriptname}.results
echo "::Unique Reflections = ${Reflections_Unique}"
#
echo "<<@progress: +5>>"  
#
#############################################################################
${proc_2dx}/linblock "2dx Processor : creating FOM-weighted averaged HKL file"
#############################################################################  
#
\rm -f APH/latfitted.hkl
#
set split = ($realcell:as/,/ /)
set cellx = $split[1]
set celly = $split[2]
#
echo "cellx = ${cellx}"
echo "celly = ${celly}"
echo "cellz = ${ALAT}" 
#
echo ":Launching ${bin_2dx}/2dx_processor.exe --hkzin APH/latlines.dat -s ${SYM_NAME} -X ${cellx} -Y ${celly} -Z ${ALAT} --gamma ${realang} --res ${RESMAX} --hklout APH/latfitted.hkl --threshold 0 --normalize-grey"
${bin_2dx}/2dx_processor.exe --hkzin APH/latlines.dat -s ${SYM_NAME} -X ${cellx} -Y ${celly} -Z ${ALAT} --gamma ${realang} --res ${RESMAX} --hklout APH/latfitted.hkl --threshold 0 --normalize-grey
#
echo "# IMAGE: APH/latfitted.hkl <HKL: Generated HKL [H,K,L,A,PHI,FOM]>" >> LOGS/${scriptname}.results
#
#
#
echo "<<@progress: +5>>"
#
#############################################################################
${proc_2dx}/linblock "Sourcing 2dx_hkl_to_mtz.com to create reference mtz file"
#############################################################################  
#
set outfile_ref = merge3Dref_MRClefthanded.mtz
\rm -f ${outfile_ref}
#
# echo "::Calling:" source ${proc_2dx}/2dx_hkl_to_mtz.com APH/latfitted.hkl ${realcell} ${ALAT} ${realang} ${RESMIN} ${MergeResolution} ${outfile_ref}
source ${proc_2dx}/2dx_hkl_to_mtz.com APH/latfitted.hkl ${realcell} ${ALAT} ${realang} ${RESMIN} ${MergeResolution} ${outfile_ref}
#
echo "# IMAGE-IMPORTANT: ${outfile_ref} <MTZ: Reference 3D MTZ file (MRC lefthanded) [H,K,L,F,P,FOM,SIGF] >" >> LOGS/${scriptname}.results
#

