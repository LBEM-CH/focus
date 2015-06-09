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
#
set bead_model_mrc = "SCRATCH/bead_model.mrc"
#
\rm -f ${bead_model_mrc}
#
#
${bin_2dx}/2dx_volume_processing/create_bead_model.exe --mrcin ${back_projected_map} --mrcout ${bead_model_mrc} -b ${number_of_beads} --thresh ${density_threshold_bead} --res 2.0
#
echo "<<@progress: +5>>"

