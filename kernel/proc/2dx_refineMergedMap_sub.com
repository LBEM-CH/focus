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
    source ${proc_2dx}/2dx_create_subvolume.com ${calculate_subvolume} ${back_projected_extended_map} ${realcell} ${ALAT} ${back_projected_sub_map}
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
if ( -d SHRINKWRAP ) then
    rm -rf SHRINKWRAP
endif
mkdir SHRINKWRAP
#
#------------------------------------------------------------------------------
echo ":Launching ${bin_2dx}/apply_shrinkwrap.exe --mrcin ${back_projected_map} --temp SHRINKWRAP/ -s ${SYM} --res ${RESMAX} --threshold_higher ${density_threshold_refinement_higher} --threshold_lower ${density_threshold_refinement_lower} --mask-res ${refinement_mask_resolution} --iterations ${number_refinement_iterations} --slab ${membrane_height} --cone ${refinement_cone_angle} --amp_cutoff ${refinement_amplitude_cutoff} --hklout ${refined_hkl} --mrcout ${refined_map}"
#------------------------------------------------------------------------------
${bin_2dx}/apply_shrinkwrap.exe --mrcin ${back_projected_map} --temp SHRINKWRAP/ -s ${SYM} --res ${RESMAX} --threshold_higher ${density_threshold_refinement_higher} --threshold_lower ${density_threshold_refinement_lower} --mask-res ${refinement_mask_resolution} --iterations ${number_refinement_iterations} --slab ${membrane_height} --cone ${refinement_cone_angle} --amp_cutoff ${refinement_amplitude_cutoff} --hklout ${refined_hkl} --mrcout ${refined_map}
#
echo "# IMAGE: SHRINKWRAP" >> LOGS/${scriptname}.results
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
    source ${proc_2dx}/2dx_create_subvolume.com ${calculate_subvolume} ${refined_extended_map} ${realcell} ${ALAT} ${refined_sub_map}
    #
    echo "# IMAGE-IMPORTANT: ${refined_sub_map} <Refined sub map>" >> LOGS/${scriptname}.results
endif
#
echo "<<@progress: +20>"
#
#######################################################################################