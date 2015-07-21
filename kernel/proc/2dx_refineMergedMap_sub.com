#
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
#
#
#
echo "# IMAGE-IMPORTANT: APH/latlines.dat <APH: latlines.dat input file from last merging [H,K,Z,A,P,SigA,SigP,IQ]>" >> LOGS/${scriptname}.results
#
#
#--------------------------------------------------------------------------
${proc_2dx}/linblock "Preparing appropriate files for back-projected map"
#--------------------------------------------------------------------------
set back_projected_hkl = "back_projected_LeftHanded.hkl"
\rm -f ${back_projected_hkl}
#
set back_projected_map = "back_projected.map"
\rm -f ${back_projected_map}
#
echo ":Launching ${bin_2dx}/volume_processor.exe --hkzin APH/latlines.dat -s ${SYM} -X ${cellx} -Y ${celly} -Z ${ALAT} --gamma ${realang} --res ${RESMAX} --hklout ${back_projected_hkl} --mrcout ${back_projected_map}"
#-----------------------------------------------------------------------------------
${bin_2dx}/volume_processor.exe --hkzin APH/latlines.dat -s ${SYM} -X ${cellx} -Y ${celly} -Z ${ALAT} --gamma ${realang} --res ${RESMAX} --hklout ${back_projected_hkl} --mrcout ${back_projected_map}
#
echo "# IMAGE: ${back_projected_hkl} <Back-Projected HKL (MRC lefthanded) [H K L AMP PHASE FOM]>" >> LOGS/${scriptname}.results
echo "# IMAGE: ${back_projected_map} <Back-Projected map>" >> LOGS/${scriptname}.results
#
set back_projected_mtz = "back_projected_LeftHanded.mtz"
rm -f ${back_projected_mtz}
source ${proc_2dx}/2dx_hkl_to_mtz.com ${back_projected_hkl} ${realcell} ${ALAT} ${realang} ${RESMIN} ${RESMAX} ${back_projected_mtz}
echo "# IMAGE-IMPORTANT: ${back_projected_mtz} <MTZ: Back-Projected Reference 3D MTZ file (MRC lefthanded) [H,K,L,F,P,FOM,SIGF] >" >> LOGS/${scriptname}.results
#
set back_projected_extended_map = "back_projected_extended.map"
set back_projected_sub_map = "back_projected_sub.map"
#
\rm -f ${back_projected_extended_map}
#
source ${proc_2dx}/2dx_extend_map.com ${back_projected_map} ${back_projected_extended_map}
#
echo "# IMAGE-IMPORTANT: ${back_projected_extended_map} <Back-Projected extended map 2X2X1 unit cells>" >> LOGS/${scriptname}.results
#
\rm -f ${back_projected_sub_map}
#
if ( ${calculate_subvolume}x != "0x" ) then 
    source ${proc_2dx}/2dx_create_subvolume.com ${back_projected_extended_map} ${realcell} ${ALAT} ${back_projected_sub_map}
    #
    echo "# IMAGE-IMPORTANT: ${back_projected_sub_map} <Back-Projected sub map>" >> LOGS/${scriptname}.results
    #
endif
#
echo "<<@progress: +10>"
#
###########################################################################
${proc_2dx}/linblock "Refinement - To extrapolate data in Fourier space"
###########################################################################
#
set refined_hkl = "refined_LeftHanded.hkl"
set refined_map = "refined.map"
\rm -f ${refined_hkl}
\rm -f ${refined_map}
#
rm -f SCRATCH/starting_random_volume.map
#
touch SCRATCH/refinement_dummy
\rm -f SCRATCH/refinement_*
#
#------------------------------------------------------------------------------
echo ":Launching ${bin_2dx}/refine_volume.exe --mrcin ${back_projected_map} --temp SCRATCH/ -s ${SYM} --res ${RESMAX} --iterations ${number_refinement_iterations} --slab ${membrane_height} --hklout ${refined_hkl} --mrcout ${refined_map}"
#------------------------------------------------------------------------------
${bin_2dx}/refine_volume.exe --mrcin ${back_projected_map} --temp SCRATCH/ -s ${SYM} --res ${RESMAX} --iterations ${number_refinement_iterations} --slab ${membrane_height} --hklout ${refined_hkl} --mrcout ${refined_map}
#
echo "# IMAGE: SCRATCH/starting_random_volume.map <MAP: Starting random volume map>" >> LOGS/${scriptname}.results
set num = 1
while ( ${num} <= ${number_refinement_iterations} )
  echo "# IMAGE: SCRATCH/refinement_initial_volume_${num}.map <MAP: Iteration initial volume map, iteration ${num}>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/refinement_final_volume_${num}.map <MAP: Iteration refined volume map, iteration ${num}>" >> LOGS/${scriptname}.results
  @ num += 1
end
#
#
echo "# IMAGE: ${refined_hkl} <Refined HKL (MRC lefthanded) [H K L AMP PHASE FOM]>" >> LOGS/${scriptname}.results
echo "# IMAGE: ${refined_map} <Refined map>" >> LOGS/${scriptname}.results
#
set refined_mtz = "merge3Dref_Refined_MRClefthanded.mtz"
\rm -f ${refined_mtz}
source ${proc_2dx}/2dx_hkl_to_mtz.com ${refined_hkl} ${realcell} ${ALAT} ${realang} ${RESMIN} ${RESMAX} ${refined_mtz}
echo "# IMAGE-IMPORTANT: ${refined_mtz} <MTZ: Refined Reference 3D MTZ file (MRC lefthanded) [H,K,L,F,P,FOM,SIGF] >" >> LOGS/${scriptname}.results
#
set refined_extended_map = "processed_extended.map"
set refined_sub_map = "processed_sub.map"
#
\rm -f ${refined_extended_map}
#
source ${proc_2dx}/2dx_extend_map.com ${refined_map} ${refined_extended_map}
#
echo "# IMAGE-IMPORTANT: ${refined_extended_map} <Refined extended map 2X2X1 unit cells>" >> LOGS/${scriptname}.results
#
\rm -f ${refined_sub_map}
#
if ( ${calculate_subvolume}x != "0x" ) then 
    source ${proc_2dx}/2dx_create_subvolume.com ${refined_extended_map} ${realcell} ${ALAT} ${refined_sub_map}
    #
    echo "# IMAGE-IMPORTANT: ${refined_sub_map} <Refined sub map>" >> LOGS/${scriptname}.results
endif
#
echo "<<@progress: +20>"
#
###########################################################################
${proc_2dx}/linblock "Refinement - To apply shrinkwrap"
###########################################################################
#
set sw_hkl = "sw_LeftHanded.hkl"
set sw_map = "sw.map"
\rm -f ${sw_hkl}
\rm -f ${sw_map}
#
rm -f SCRATCH/mask_binary_shrinkwrap.map
#
touch SCRATCH/shrinkwrap_dummy
\rm -f SCRATCH/shrinkwrap_*
#
#------------------------------------------------------------------------------
echo ":Launching ${bin_2dx}/apply_shrinkwrap.exe --mrcin ${back_projected_map} --temp SCRATCH/ -s ${SYM} --res ${RESMAX} --threshold ${density_threshold_refinement} --mask-res ${refinement_mask_resolution} --iterations ${number_refinement_iterations} --hklout ${sw_hkl} --mrcout ${sw_map}"
#------------------------------------------------------------------------------
${bin_2dx}/apply_shrinkwrap.exe --mrcin ${back_projected_map} --temp SCRATCH/ -s ${SYM} --res ${RESMAX} --threshold ${density_threshold_refinement} --mask-res ${refinement_mask_resolution} --iterations ${number_refinement_iterations} --hklout ${sw_hkl} --mrcout ${sw_map}
#
echo "# IMAGE: SCRATCH/mask_binary_shrinkwrap.map <MAP: Binary mask for shrinkwrap>" >> LOGS/${scriptname}.results
set num = 1
while ( ${num} <= ${number_refinement_iterations} )
  echo "# IMAGE: SCRATCH/shrinkwrap_initial_volume_${num}.map <MAP: Iteration initial volume map, iteration ${num}>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/shrinkwrap_final_volume_${num}.map <MAP: Iteration refined volume map, iteration ${num}>" >> LOGS/${scriptname}.results
  @ num += 1
end
#
#
echo "# IMAGE: ${sw_hkl} <Shrinkwraped HKL (MRC lefthanded) [H K L AMP PHASE FOM]>" >> LOGS/${scriptname}.results
echo "# IMAGE: ${sw_map} <Shrinkwraped map>" >> LOGS/${scriptname}.results
#
set sw_mtz = "merge3Dref_Shrinkwraped_MRClefthanded.mtz"
\rm -f ${sw_mtz}
source ${proc_2dx}/2dx_hkl_to_mtz.com ${sw_hkl} ${realcell} ${ALAT} ${realang} ${RESMIN} ${RESMAX} ${sw_mtz}
echo "# IMAGE-IMPORTANT: ${sw_mtz} <MTZ: Shrinkwraped Reference 3D MTZ file (MRC lefthanded) [H,K,L,F,P,FOM,SIGF] >" >> LOGS/${scriptname}.results
#
set sw_extended_map = "sw_extended.map"
set sw_sub_map = "sw_sub.map"
#
\rm -f ${sw_extended_map}
#
source ${proc_2dx}/2dx_extend_map.com ${sw_map} ${sw_extended_map}
#
echo "# IMAGE-IMPORTANT: ${sw_extended_map} <Shrinkwraped extended map 2X2X1 unit cells>" >> LOGS/${scriptname}.results
#
\rm -f ${sw_sub_map}
#
if ( ${calculate_subvolume}x != "0x" ) then 
    source ${proc_2dx}/2dx_create_subvolume.com ${sw_extended_map} ${realcell} ${ALAT} ${sw_sub_map}
    #
    echo "# IMAGE-IMPORTANT: ${sw_sub_map} <Shrinkwraped sub map>" >> LOGS/${scriptname}.results
endif
#
echo "<<@progress: +20>"
#
#######################################################################################