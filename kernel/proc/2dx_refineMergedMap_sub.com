#
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
#
#--------------------------------------------------------------------------
${proc_2dx}/linblock "Calling refine_merged_volume.exe APH/latlines.dat ${cellx} ${celly} ${ALAT} ${SYM} ${realang} ${maximum_amplitude_refinement} ${RESMAX} ${number_refinement_iterations} ${density_threshold_refinement} ${number_of_beads} ${density_threshold_bead} ${maximum_resolution_bead} ${membrane_height}"
#--------------------------------------------------------------------------
${bin_2dx}/2dx_volume_processing/refine_merged_volume.exe APH/latlines.dat ${cellx} ${celly} ${ALAT} ${SYM} ${realang} ${maximum_amplitude_refinement} ${RESMAX} ${number_refinement_iterations} ${density_threshold_refinement} ${number_of_beads} ${density_threshold_bead} ${maximum_resolution_bead} ${membrane_height}  
#
echo "<<@progress: 70>"
#
#--------------------------------------------------------------------------
${proc_2dx}/linblock "Preparing appropriate files for back-projected map"
#--------------------------------------------------------------------------
set back_projected_hkl = "back_projected_LeftHanded.hkl"
\rm -f ${back_projected_hkl}
\mv -f back_projected.hkl ${back_projected_hkl}
echo "# IMAGE: ${back_projected_hkl} <Back-Projected HKL (MRC lefthanded) [H K L AMP PHASE FOM]>" >> LOGS/${scriptname}.results

set back_projected_mtz = "back_projected_LeftHanded.mtz"
rm -f ${back_projected_mtz}
source ${proc_2dx}/2dx_hkl_to_mtz.com ${back_projected_hkl} ${realcell} ${ALAT} ${realang} ${RESMIN} ${RESMAX} ${back_projected_mtz}
echo "# IMAGE-IMPORTANT: ${back_projected_mtz} <MTZ: Back-Projected Reference 3D MTZ file (MRC lefthanded) [H,K,L,F,P,FOM,SIGF] >" >> LOGS/${scriptname}.results

set back_projected_map = "back_projected.map"
set back_projected_extended_map = "back_projected_extended.map"
set back_projected_sub_map = "back_projected_sub.map"

echo "# IMAGE: ${back_projected_map} <Back-Projected map>" >> LOGS/${scriptname}.results
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
#--------------------------------------------------------------------------
${proc_2dx}/linblock "Preparing appropriate files for bead-model map"
#--------------------------------------------------------------------------
set bead_model_hkl = "bead_model.hkl"
set bead_model_map = "bead_model.map"
set bead_model_extended_map = "bead_model_extended.map"
set bead_model_sub_map = "bead_model_sub.map"

echo "# IMAGE: ${bead_model_map} <Bead model map>" >> LOGS/${scriptname}.results
#
\rm -f ${bead_model_extended_map}
#
source ${proc_2dx}/2dx_extend_map.com ${bead_model_map} ${bead_model_extended_map}
#
echo "# IMAGE-IMPORTANT: ${bead_model_extended_map} <Bead model extended map 2X2X1 unit cells>" >> LOGS/${scriptname}.results
#
\rm -f ${bead_model_sub_map}
#
if ( ${calculate_subvolume}x != "0x" ) then 
    source ${proc_2dx}/2dx_create_subvolume.com ${bead_model_extended_map} ${realcell} ${ALAT} ${bead_model_sub_map}
    #
    echo "# IMAGE-IMPORTANT: ${bead_model_sub_map} <Bead model sub map>" >> LOGS/${scriptname}.results
endif
#
#--------------------------------------------------------------------------
${proc_2dx}/linblock "Preparing appropriate files for refined map"
#--------------------------------------------------------------------------
set refined_hkl = "processed_LeftHanded.hkl"
\rm -f ${refined_hkl}
\mv -f refined_final.hkl ${refined_hkl}
echo "# IMAGE: ${refined_hkl} <Refined HKL (MRC lefthanded) [H K L AMP PHASE FOM]>" >> LOGS/${scriptname}.results
#
set refined_mtz = "merge3Dref_Refined_MRClefthanded.mtz"
rm -f ${refined_mtz}
source ${proc_2dx}/2dx_hkl_to_mtz.com ${refined_hkl} ${realcell} ${ALAT} ${realang} ${RESMIN} ${RESMAX} ${refined_mtz}
echo "# IMAGE-IMPORTANT: ${refined_mtz} <MTZ: Refined Reference 3D MTZ file (MRC lefthanded) [H,K,L,F,P,FOM,SIGF] >" >> LOGS/${scriptname}.results
#
set refined_map = "processed.map"
set refined_extended_map = "processed_extended.map"
set refined_sub_map = "processed_sub.map"
#
\mv  refined_final.map ${refined_map}
echo "# IMAGE: ${refined_map} <Refined map>" >> LOGS/${scriptname}.results
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
\mv -f refined_* SCRATCH/
#
#############################################################################

