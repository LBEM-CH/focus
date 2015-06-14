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
#-----------------------------------------------------------------------------------
echo ":Launching ${bin_2dx}/2dx_volume_processing/volume_processor.exe --hkzin APH/latlines.dat -s ${SYM} -X ${cellx} -Y ${celly} -Z ${ALAT} --gamma ${realang} --res ${RESMAX} --spread-fourier --normalize-grey --threshold 0 --hklout ${back_projected_hkl} --mrcout ${back_projected_map}"
#-----------------------------------------------------------------------------------
${bin_2dx}/2dx_volume_processing/volume_processor.exe --hkzin APH/latlines.dat -s ${SYM} -X ${cellx} -Y ${celly} -Z ${ALAT} --gamma ${realang} --res ${RESMAX} --spread-fourier --normalize-grey --threshold 0 --hklout ${back_projected_hkl} --mrcout ${back_projected_map}
#
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
${proc_2dx}/linblock "Preparing appropriate files for bead-model map"
###########################################################################
#
set bead_model_map = "bead_model.map"
#
\rm -f ${bead_model_map}
#---------------------------------------------------------------------------
echo ":Launching ${bin_2dx}/2dx_volume_processing/create_bead_model.exe --mrcin ${back_projected_map} --mrcout ${bead_model_map} -b ${number_of_beads} --threshold ${density_threshold_bead} --res ${maximum_resolution_bead}"
#---------------------------------------------------------------------------
${bin_2dx}/2dx_volume_processing/create_bead_model.exe --mrcin ${back_projected_map} --mrcout ${bead_model_map} -b ${number_of_beads} --threshold ${density_threshold_bead} --res ${maximum_resolution_bead}
#
echo "# IMAGE: ${bead_model_map} <Bead model map>" >> LOGS/${scriptname}.results
#
set bead_model_extended_map = "bead_model_extended.map"
set bead_model_sub_map = "bead_model_sub.map"
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
echo "<<@progress: +10>"
#
###########################################################################
${proc_2dx}/linblock "Preparing appropriate files for refined map"
###########################################################################
#
set refined_hkl = "processed_LeftHanded.hkl"
set refined_map = "processed.map"
\rm -f ${refined_hkl}
\rm -f ${refined_map}
#
touch SCRATCH/refined_dummy
\rm -f SCRATCH/refined_*
#
set num = 1
while ( ${num} <= ${number_refinement_iterations} ) 
  echo "# IMAGE: SCRATCH/refined_${num}.map <MAP: Refinement scratch map, iteration ${num}>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/mask_iteration_${num}.map <MAP: Refinement mask map, iteration ${num}>" >> LOGS/${scriptname}.results
  @ num += 1
end  
#
#------------------------------------------------------------------------------
echo ":Launching ${bin_2dx}/2dx_volume_processing/refine_volume.exe --mrcin ${back_projected_map} --refin ${bead_model_map} --temp SCRATCH/ -s ${SYM} --res ${RESMAX} --threshold ${density_threshold_refinement} --iterations ${number_refinement_iterations} --slab ${membrane_height} --hklout ${refined_hkl} --mrcout ${refined_map}"
#------------------------------------------------------------------------------
${bin_2dx}/2dx_volume_processing/refine_volume.exe --mrcin ${back_projected_map} --refin ${bead_model_map} --temp SCRATCH/ -s ${SYM} --res ${RESMAX} --threshold ${density_threshold_refinement} --iterations ${number_refinement_iterations} --slab ${membrane_height} --hklout ${refined_hkl} --mrcout ${refined_map}
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
echo "<<@progress: +40>"
#
#############################################################################

