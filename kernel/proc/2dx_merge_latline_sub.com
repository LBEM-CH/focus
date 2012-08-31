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
# this is for a later option:
set mode = 0
#
if ( ${mode} == 0 ) then
  #
  if ( ${scalimamp3d_rref} != "0" ) then
    #
    #############################################################################
    #                                                                           #
    ${proc_2dx}/linblock "SCALIMAMP3D - to scale image amplitudes to selected reference data"
    #                                                                           #
    #############################################################################
    #
    if ( ${scalimamp3d_rref} == "1" ) then 
      set scalimamp3d_refdata = BT
    endif
    if ( ${scalimamp3d_rref} == "2" ) then 
      set scalimamp3d_refdata = LZ
      echo ":: "
      ${proc_2dx}/linblock "WARNING: SCALIMAMP3D option LZ is not yet working"
      echo ":: "
    endif
    if ( ${scalimamp3d_rref} == "3" ) then 
      set scalimamp3d_refdata = BR
    endif
    set bextra = 0.0
    #
    \rm -f SCALIMAMP3D.DAT
    \rm -f OUT
    #
    echo ": "
    echo ": calling 2dx_scalimamp3d.exe with the following parameters:"
    echo ": ${scalimamp3d_refdata}"
    echo ": 0,F,${scalimamp3d_BXYMINMAX},${scalimamp3d_BZMINMAX} ! NPROG,TWOFOLD,BXYMIN,BXYMAX,BZMIN,BZMAX"
    echo ": ${RESMAX}, ${zstarrange_real}, ${scalimamp3d_BEXTRA} ! RESLIMXY, RESLIMZ, BEXTRA"
    echo ": APH/merge.aph"
    echo ": ${realcell}, ${realang}, ${RESMAX} !  A,B,GAMMA,RESOL"
    echo ":  "
    #
    ${bin_2dx}/2dx_scalimamp3d.exe << eot 
${scalimamp3d_refdata}
0,F,${scalimamp3d_BXYMINMAX},${scalimamp3d_BZMINMAX} ! NPROG,TWOFOLD,BXYMIN,BXYMAX,BZMIN,BZMAX
${RESMAX}, ${zstarrange_real}, ${scalimamp3d_BEXTRA} ! RESLIMXY, RESLIMZ, BEXTRA
APH/merge.aph
${realcell}, ${realang}, ${RESMAX} !  A,B,GAMMA,RESOL
eot
    #
    if ( ! -e OUT ) then
      ${proc_2dx}/protest "ERROR: Problem in scalimamp3d.exe."
    else    
      \mv -f APH/merge.aph APH/merge_before_scalimamp3d.aph
      \mv -f OUT APH/merge.aph
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
  #                                                                           #
  ${proc_2dx}/linblock "LATLINPRESCAL - to apply CTF correction and weight calculation"
  #                                                                           #
  #                                          merge_3ds.aph  =>  latlines.dat  #
  #                                                                           #
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
  echo "eot " >> LOGS/latlinprescal.log
  echo " " >> LOGS/latlinprescal.log
  echo "Running: " >> LOGS/latlinprescal.log
  echo "======== " >> LOGS/latlinprescal.log
  #
  #
  ${bin_2dx}/2dx_latlinprescal.exe << eot >> LOGS/latlinprescal.log
1001,${zminmax} ! NSER,ZMIN,ZMAX
${MergeIQMAX}               ! IQMAX
${max_amp_correction}       ! Max_Amp_Correction
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
    \mv -f fort.3 SCRATCH/latlines.dat
    echo "# IMAGE: LOGS/latlinprescal.log <LOG: latlinprescal output>" >> LOGS/${scriptname}.results
    echo "# IMAGE: SCRATCH/latlines.dat <Latline after prescal [H,K,Z,A,P,SAMP,SANG,IQ]>" >> LOGS/${scriptname}.results
  else
    ${proc_2dx}/protest "ERROR: latlines.dat does not exist."
  endif
  #
  echo " "
  ${proc_2dx}/lin "-"
  echo " "
  #
endif
#
set Reflections_Unique = `wc -l SCRATCH/latlines.dat | cut -f1`
echo "set Reflections_Unique = ${Reflections_Unique}" >> LOGS/${scriptname}.results
echo "::Unique Reflections = ${Reflections_Unique}"
#
#############################################################################
#                                                                           #
${proc_2dx}/linblock "LATLINEK - to fit lattice lines to merged data (D.Agard's program)"
#                                                                           #
#                latlines.dat  =>  latfitteds.dat + LATLINE.PLT + guess.dat #
#                                                                           #
#############################################################################
#
echo "<<@progress: +5>>"
#
\rm -f PLOT.PS
\rm -f PS/latline.ps
\rm -f SCRATCH/latfitteds.dat
\rm -f SCRATCH/guess.dat
\rm -f latline.statistics
#
set iplterr = 0
set idoh = 0
set idok = 0
set IAQP2 = 0
set iplterr = 1         
set imaxIQplot = ${MergeIQMAX}
set MergeIGUESS = 1
#
if ( ${tempkeep} == 'y' ) then
  set iverbose = 1
else
  set iverbose = 3
endif
#
setenv  OBS   SCRATCH/latlines.dat
setenv  OUT   SCRATCH/latfitteds.dat
setenv  GUESS SCRATCH/guess.dat
#
echo " "
echo " Parameters for latline are:"
echo " "
echo "2dx_merge_latline_sub.com, ${date}"
echo "${spcgrp}                            # IPG (Plane group number 1-17)"
echo "${MergeIPAT}                             # IPAT, 0=F & Phase, 1=Intensity data"
echo "${MergeAK},${MergeIWF_VAL},${MergeIWP_VAL}                      # AK,IWF,IWP - relative weights, + individual sigmas"
echo "${ALAT},${zminmax},${MergeDELPLT}        # ALAT,ZMIN,ZMAX,DELPLT "
echo "${MergeDELPRO},${MergeRminRmax},${MergeRCUT},${MergePFACT}        # DELPRO,RMIN,RMAX,RCUT,PFACT"
echo "${MergeIGUESS},${MergeBINSIZ}                       # IGUESS,BINSIZ"
echo "${MergeNCYCLS},${MergeMPRINT}                          # NCYCLS,MPRINT"
echo "${idoh},${idok}		                          # H,K indices to plot. 0 0 = all."
echo "${IAQP2},${iplterr},${imaxIQplot}                         # IAQP2: 1=y,0=n, iplterr=1:errbar in PHS, maxIQ for PSplot"
echo " "
#
${bin_2dx}/2dx_latlinek.exe << eot > LOGS/2dx_latlinek.log
2dx_merge_latline_sub.com, ${date}
${spcgrp}                                                    ! IPG (Plane group number 1-17)
${MergeIPAT}                                                            ! IPAT, 0=F & Phase, 1=Intensity data
${MergeAK},${MergeIWF_VAL},${MergeIWP_VAL}                   ! AK,IWF,IWP - relative weights, + individual sigmas
${ALAT},${zminmax},${MergeDELPLT}                            ! ALAT,ZMIN,ZMAX,DELPLT
${MergeDELPRO},${MergeRminRmax},${MergeRCUT},${MergePFACT}   ! DELPRO,RMIN,RMAX,RCUT,PFACT
${MergeIGUESS},${MergeBINSIZ}                                ! IGUESS,BINSIZ
${MergeNCYCLS},${MergeMPRINT}                                ! NCYCLS,MPRINT
${idoh},${idok}                                              ! H,K indices to plot. 0 0 = all.
${IAQP2},${iplterr},${imaxIQplot}                            ! IAQP2: 1=y,0=n, iplterr=1:errbar in PHS, maxIQ for PSplot
eot
#
echo "################################################"
echo "################################################"
echo "output in file LOGS/2dx_latlinek.log"
echo "################################################"
echo "################################################"
#
echo "# IMAGE: LOGS/2dx_latlinek.log <LOG: 2dx_latlinek output>" >> LOGS/${scriptname}.results
echo "# IMAGE: SCRATCH/latline_stat.dat <Lattice line statistics>" >> LOGS/${scriptname}.results
echo "# IMAGE: SCRATCH/latfitteds.dat <Lattice line fit data [H,K,Z,A,PHI,SIGF,SIGP,FOM]>" >> LOGS/${scriptname}.results
if ( -e SCRATCH/guess.dat ) then
  echo "# IMAGE: SCRATCH/guess.dat <Lattice line guess data>" >> LOGS/${scriptname}.results
endif
if ( ! -e latline.statistics ) then
  ${proc_2dx}/linblock "#"
  ${proc_2dx}/linhash "3D modus, but do you have 3D (i.e. tilted) data?"
  ${proc_2dx}/protest "ERROR in latlinek. Check logfile."
endif
\mv -f latline.statistics SCRATCH/latline_stat.dat
set num_amplitudes_observed = `cat SCRATCH/latline_stat.dat | grep "Number of amplitudes observed" | cut -c55-`
set num_phases_observed = `cat SCRATCH/latline_stat.dat | grep "Number of phases observed" | cut -c55-`
set num_reflections_fitted = `cat SCRATCH/latfitteds.dat | wc -l`
set overall_R_factor =  `cat SCRATCH/latline_stat.dat | grep "Overall R-factor" | cut -c55-`
set overall_phase_residual =  `cat SCRATCH/latline_stat.dat | grep "Overall phase residual" | cut -c55-`
set overall_weighted_R_factor =  `cat SCRATCH/latline_stat.dat | grep "Overall weighted R-factor" | cut -c55-`
set overall_weighted_phase_residual =  `cat SCRATCH/latline_stat.dat | grep "Overall weighted phase residual" | cut -c55-`
echo "set num_amplitudes_observed = ${num_amplitudes_observed}" >> LOGS/${scriptname}.results
echo "set num_phases_observed = ${num_phases_observed}" >> LOGS/${scriptname}.results
echo "set num_reflections_fitted = ${num_reflections_fitted}" >> LOGS/${scriptname}.results 
echo "set overall_R_factor = ${overall_R_factor}" >> LOGS/${scriptname}.results
echo "set overall_phase_residual = ${overall_phase_residual}" >> LOGS/${scriptname}.results
echo "set overall_weighted_R_factor = ${overall_weighted_R_factor}" >> LOGS/${scriptname}.results
echo "set overall_weighted_phase_residual = ${overall_weighted_phase_residual}" >> LOGS/${scriptname}.results
#
if ( ! -e PLOT.PS ) then
  ${proc_2dx}/protest "2dx_latlinek: ERROR occured."
endif
#
\mv -f PLOT.PS PS/latline.ps 
echo "# IMAGE-IMPORTANT: PS/latline.ps <PS: Lattice lines>" >> LOGS/${scriptname}.results
#
echo " "
${proc_2dx}/lin "-"
echo " "
#
echo "<<@progress: +5>>"
#
#############################################################################
#                                                                           #
${proc_2dx}/linblock "PREPMKMTZ - Program to convert fitted data to CCP4 format"
#                                                                           #
#############################################################################
#
\rm -f APH/latfitted_nosym.hkl
\rm -f 2dx_prepmklcf.statistics
#
setenv IN SCRATCH/latfitteds.dat
setenv OUT APH/latfitted_nosym.hkl
setenv REFHKL APH/latfittedref_nosym.hkl
#
${bin_2dx}/2dx_prepmklcf.exe << eot > LOGS/prepmklcf.log
${RESMAX},1.5                          ! RESOLUTION,REDUCAC (1.5 = 60deg phase error)
${realcell},${realang},${ALAT}         ! a,b,gamma,c
0.0                                    ! SCALE (automatic scaling to max(AMP)=32000.0)
eot
#
echo "################################################"
echo "################################################"
echo "output in file LOGS/prepmklcf.log"
echo "################################################"
echo "################################################"
#
if ( ! -e 2dx_prepmklcf.statistics ) then
  ${proc_2dx}/linblock "ERROR: 2dx_prepmklcf.statistics file is missing."
else
  set num_reflections_FOM1 = `cat 2dx_prepmklcf.statistics | sed 's/ /_/g' | grep 'Number_of_phases_with_FOM_over_1' | sed s'/_/ /g' | cut -d= -f2`
  set num_reflections_FOM50 = `cat 2dx_prepmklcf.statistics | sed 's/ /_/g' | grep 'Number_of_phases_with_FOM_over_50' | sed s'/_/ /g' | cut -d= -f2`
  echo "::Number of phases with FOM>1% is ${num_reflections_FOM1}"
  echo "::Number of phases with FOM>50% is ${num_reflections_FOM50}"
  echo "set num_reflections_FOM1 = ${num_reflections_FOM1}" >> LOGS/${scriptname}.results
  echo "set num_reflections_FOM50 = ${num_reflections_FOM50}" >> LOGS/${scriptname}.results
  \mv -f 2dx_prepmklcf.statistics SCRATCH
endif
echo "# IMAGE: LOGS/prepmklcf.log <LOG: prepmklcf output>" >> LOGS/${scriptname}.results
if ( ${tempkeep} == "y" ) then
  echo "# IMAGE: APH/latfitted_nosym.hkl <APH: Latline for vol after prepmklcf [H,K,L,F,P,FOM]>" >> LOGS/${scriptname}.results
  echo "# IMAGE: APH/latfittedref_nosym.hkl <APH: Latline for ref after prepmklcf [H,K,L,F,P,FOM,SIGF]>" >> LOGS/${scriptname}.results
endif
#
echo "<<@progress: +5>>"
#
#############################################################################
${proc_2dx}/linblock "f2mtz - Program to convert hkl data into MTZ format, for volume"
#############################################################################
#
set infile = APH/latfitted_nosym.hkl
\rm -f merge3D.mtz
#
${bin_ccp4}/f2mtz hklin ${infile} hklout merge3D.mtz << eof
TITLE  P1 map, ${date}
CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}
SYMMETRY ${CCP4_SYM}
LABOUT H K L F PHI FOM
CTYPOUT H H H F P W
FILE ${infile}
SKIP 0
END
eof
#
echo "# IMAGE-IMPORTANT: merge3D.mtz <MTZ: Latline data for volume>" >> LOGS/${scriptname}.results
echo "<<@progress: +5>>"
#
#
#############################################################################
${proc_2dx}/linblock "f2mtz - Program to convert hkl data into MTZ format, for reference"
#############################################################################
#
set infile = APH/latfittedref_nosym.hkl
\rm -f SCRATCH/merge3Dref.mtz
#
${bin_ccp4}/f2mtz hklin ${infile} hklout SCRATCH/merge3Dref.mtz << eof
TITLE  P1 map, ${date}
CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}
SYMMETRY ${CCP4_SYM}
LABOUT H K L F PHI FOM SIGF
CTYPOUT H H H F P W W
FILE ${infile}
SKIP 0
END
eof
#
#
#############################################################################
${proc_2dx}/linblock "sftools - to extend to P1 symmetry"
#############################################################################  
#
\rm -f SCRATCH/merge3Dref-clean.mtz
#
${bin_ccp4}/sftools << eof
read SCRATCH/merge3Dref.mtz
sort h k l 
set spacegroup
${CCP4_SYM}
select phaerr
select invert
purge
y
write SCRATCH/merge3Dref-clean.mtz
quit
eof
#
\rm -f SCRATCH/merge3Dref-clean-p1.mtz
#
${bin_ccp4}/cad hklin1 SCRATCH/merge3Dref-clean.mtz hklout SCRATCH/merge3Dref-clean-p1.mtz << eof
reso overall 10000 1
outlim spacegroup 1
labin file 1 all
end
eof
#
\rm -f merge3Dref.mtz
#
${bin_ccp4}/sftools << eof
read SCRATCH/merge3Dref-clean-p1.mtz
sort h k l 
set spacegroup
1
reduce matrix 1 0 0 0 1 0 0 0 1
write merge3Dref.mtz
quit
eof
#
# \rm -f SCRATCH/merge3Dref.mtz
# \rm -f SCRATCH/merge3Dref-clean.mtz
# \rm -f SCRATCH/merge3Dref-clean-p1.mtz
# 
# \cp -f SCRATCH/merge3Dref.mtz merge3Dref.mtz
#
# echo "# IMAGE: SCRATCH/merge3Dref.mtz <MTZ: SCRATCH/merge3Dref.mtz>" >> LOGS/${scriptname}.results
# echo "# IMAGE: SCRATCH/merge3Dref-clean.mtz <MTZ: SCRATCH/merge3Dref-clean.mtz>" >> LOGS/${scriptname}.results
# echo "# IMAGE: SCRATCH/merge3Dref-clean-p1.mtz <MTZ: SCRATCH/merge3Dref-clean-p1.mtz>" >> LOGS/${scriptname}.results
#
echo "# IMAGE-IMPORTANT: merge3Dref.mtz <MTZ: Latline data for reference>" >> LOGS/${scriptname}.results
#
echo "<<@progress: +5>>"
#
#

