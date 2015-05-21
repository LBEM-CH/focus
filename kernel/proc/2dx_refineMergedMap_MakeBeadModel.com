#
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
#

#------------------------------------------------------------------------
${proc_2dx}/linblock "Genrating bead model pdb .. "
#------------------------------------------------------------------------
#
set bead_model = "SCRATCH/bead_model"
set bead_model_pdb = "${bead_model}.pdb"
set back_projected_map = "SCRATCH/back_projected.map"
#
\rm -f ${bead_model_pdb}
#
${bin_2dx}/2dx_volume_processing/create_bead_model_pdb.exe ${back_projected_map} ${bead_model_pdb} ${number_of_beads} ${density_threshold_bead} ${noise_level_bead}
#
#############################################################################
${proc_2dx}/linblock "sourcing 2dx_pdb2map_ccp4.com"
#############################################################################
#
set bead_model_ccp4 = "${bead_model}_ccp4"
#
source ${proc_2dx}/2dx_pdb2map_ccp4.com ${bead_model_pdb}
#
source ${proc_2dx}/2dx_refine_raw_ccp4_map.com  ${bead_model_ccp4}.map
#
set bead_model_extended_map = "bead_model_extended.map"
#
\rm -f ${bead_model_extended_map}
#
\mv -f ${bead_model_ccp4}_extended.map ${bead_model_extended_map}
#
echo "# IMAGE: ${bead_model_extended_map} <MAP: Bead model extended map >" >> LOGS/${scriptname}.results
#
if ( ${calculate_subvolume}x != "0x" ) then  
        #
        set bead_model_sub_map = "bead_model_sub.map"
        #
        \rm -f ${bead_model_sub_map}
        #
        \mv -f ${bead_model_ccp4}_sub.map ${bead_model_sub_map}
        #
        echo "# IMAGE: ${bead_model_sub_map} <MAP: Bead model sub map >" >> LOGS/${scriptname}.results
        #
endif
#
echo "<<@progress: +5>>"
#
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "E2PDB2MRC.PY: Converting bead model pdb to mrc"
#------------------------------------------------------------------------
#
set bead_model_e2pdb2mrc = "SCRATCH/bead_model_e2pdb2mrc.mrc"
#
\rm -f ${bead_model_e2pdb2mrc}
#
e2pdb2mrc.py -R ${RESMAX} -A 1.0 -B ${ALAT} ${bead_model_pdb} ${bead_model_e2pdb2mrc}
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "E2PROC3D.PY: Correcting the size of bead model mrc"
#------------------------------------------------------------------------
#
set bead_model_mrc = "SCRATCH/bead_model.mrc"
#
\rm -f ${bead_model_mrc}
#
set size_x = `printf %.0f ${cellx}`
set size_y = `printf %.0f ${celly}`
set size_z = `printf %.0f ${ALAT}`
set input_size = "${size_x},${size_y},${size_z}"
echo "Chopping to ${input_size}"
#
e2proc3d.py --clip ${input_size} ${bead_model_e2pdb2mrc} ${bead_model_mrc}
#
echo "<<@progress: +5>>"

