#
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
#

#------------------------------------------------------------------------
${proc_2dx}/linblock "Processing the SF-Hist Iterations"
#------------------------------------------------------------------------
#
set bead_model_sf = "SCRATCH/sf_bead_model.dat"
set bead_model_mrc = "SCRATCH/bead_model.mrc"
set back_projected_map = "SCRATCH/back_projected.map"
#
\rm -f ${bead_model_sf}
#
e2proc3d.py ${bead_model_mrc} SCRATCH/junk.mrc --calcsf=${bead_model_sf} --apix=1.0
#
\rm -f SCRATCH/junk.mrc
#
set output_prefix = "SCRATCH/backproj"
#
\rm -f ${output_prefix}.mrc
#
\cp -f ${back_projected_map} ${output_prefix}.mrc
#
set i = 1
while ($i <= ${number_sf_hist_iterations})
    #------------------------------------------------------------------------
    ${proc_2dx}/linblock "Setting the structure factors for iteration ${i}"
    #------------------------------------------------------------------------
    #
    \rm -f ${output_prefix}_sf.mrc
    #
    e2proc3d.py ${output_prefix}.mrc ${output_prefix}_sf.mrc --setsf=${bead_model_sf} --apix=1.0
        #
    #------------------------------------------------------------------------
    ${proc_2dx}/linblock "Matching real space histogram for iteration ${i}"
    #------------------------------------------------------------------------
    #
    \rm -f ${output_prefix}_sf_hist.mrc
    #
    ${bin_2dx}/2dx_volume_processing/match_density_histogram.exe ${output_prefix}_sf.mrc ${bead_model_mrc} ${output_prefix}_sf_hist.mrc
    #
    set output_prefix = "${output_prefix}_sf_hist"
    @ i = $i + 1
    #
end
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "Setting the structure factors for one last time"
#------------------------------------------------------------------------
#
\rm -f ${output_prefix}_sf.mrc
#
e2proc3d.py ${output_prefix}.mrc ${output_prefix}_sf.mrc --setsf=SCRATCH/sf_bead_model.dat
#
#------------------------------------------------------------------------
#
set processed_mrc = "SCRATCH/processed_${number_sf_hist_iterations}_iterations.mrc"
#
\rm -f ${processed_mrc}
#
\mv -f ${output_prefix}_sf.mrc ${processed_mrc}
#
echo "<<@progress: +10>>"
#
########################################################################
# STEP 6: Generate processed hkl
########################################################################
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "Converting the processed mrc to hkl"
#------------------------------------------------------------------------
#
set processed_hkl = "SCRATCH/processed.hkl"
#
\rm -f ${processed_hkl}
#
${bin_2dx}/2dx_volume_processing/mrc_to_hkl.exe ${processed_mrc} ${processed_hkl}
#
echo "<<@progress: +5>>"
#
########################################################################
# STEP 7: Generate processed mtz
########################################################################
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "F2MTZ: to convert processed hkl to mtz.."
#------------------------------------------------------------------------
#
set processed_f2mtz_mtz = "SCRATCH/processed_f2mtz_MRClefthanded.mtz"
#
\rm -f ${processed_f2mtz_mtz}
#
${bin_ccp4}/f2mtz hklin ${processed_hkl} hklout ${processed_f2mtz_mtz} << eof
TITLE  P1 map, ${date}
CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}
SYMMETRY 1
LABOUT H K L F PHI FOM
CTYPOUT H H H F P W
FILE ${processed_hkl}
SKIP 0
END
eof
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "CAD: to finalize processed mtz file.."
#------------------------------------------------------------------------
#
set processed_cad_mtz = "SCRATCH/processed_cad_MRClefthanded.mtz"
#
\rm -f ${processed_cad_mtz}
#
${bin_ccp4}/cad hklin1 ${processed_f2mtz_mtz} hklout ${processed_cad_mtz} << eof
sort h k l
resolution overall ${RESMAX} ${RESMIN}
outlim spacegroup 1
labin file 1 all
valm NaN NOOUTPUT
end
eof
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "Writing the processed mtz file"
#------------------------------------------------------------------------
#
set processed_mtz = "APH/processed_MRClefthanded.mtz"
#
\rm -f ${processed_mtz}
#
mv ${processed_cad_mtz} ${processed_mtz}
#
echo "# IMAGE: ${processed_mtz} <MTZ: Processed SF-HIST MTZ (MRC lefthanded) >" >> LOGS/${scriptname}.results
#
echo "<<@progress: +5>>"
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "sftools - to create MTZ file for reference with SIGF column"
#------------------------------------------------------------------------
#
set outfile = merge3Dref_Refined_MRClefthanded.mtz
#
\rm -f ${outfile}
#
${bin_ccp4}/sftools << eof
read ${processed_mtz}
calc COL SIGF = 1.0
calc COL F = COL F 1000.0 *
write ${outfile}
quit
eof
#
echo "# IMAGE-IMPORTANT: ${outfile} <MTZ: Reference 3D MTZ file (MRC lefthanded) [H,K,L,F,P,FOM,SIGF] >" >> LOGS/${scriptname}.results
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "reindex - to flip hand of MTZ file for further work with CCP4"
#------------------------------------------------------------------------
#
set processed_mtz_righthanded = "APH/processed.mtz"
\rm -f ${processed_mtz_righthanded}
#
${bin_ccp4}/reindex hklin ${processed_mtz} hklout ${processed_mtz_righthanded} << eof
reindex HKL k,h,l
lefthand
end
eof
#
echo "# IMAGE-IMPORTANT: ${processed_mtz_righthanded} <MTZ: Processed SF-HIST CCP4 MTZ file [H,K,L,F,P,FOM]>" >> LOGS/${scriptname}.results
#
#
########################################################################
# STEP 8: Convert processed mtz to map
########################################################################
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "FFT: to convert processed mtz to map.."
#------------------------------------------------------------------------
#
set processed_fft = "SCRATCH/processed_fft"
set processed_fft_map = "${processed_fft}.map"
#
\rm -f ${processed_fft_map}
#
${bin_ccp4}/fft hklin ${processed_mtz} mapout ${processed_fft_map}  << eot
LABIN F1=F  PHI=PHI W=FOM ##
AXIS X,Y,Z
SCALE F1 1 0
SYMMETRY 1
RESOLUTION ${RESMIN} ${RESMAX}
TITLE Sym=${SYM}, res=${RESMAX}, T=0
GRID ${voldim} ${voldim} ${voldim} 
XYZLIM 0 ${voldim_m1}  0 ${voldim_m1} 0 ${voldim_m1}
RHOLIM ${voldim_t142}
HKLMAX 50 50 70
END
eot
#
echo "<<@progress: +5>>"
#
#############################################################################
${proc_2dx}/linblock "sourcing 2dx_refine_raw_ccp4_map.com"
#############################################################################
#
source ${proc_2dx}/2dx_refine_raw_ccp4_map.com  ${processed_fft_map}
#
set processed_extended_map = "processed_extended.map"
#
\rm -f ${processed_extended_map}
#
\mv -f ${processed_fft}_extended.map ${processed_extended_map}
#
echo "# IMAGE-IMPORTANT: ${processed_extended_map} <MAP: Processed extended map >" >> LOGS/${scriptname}.results
#
if ( ${calculate_subvolume}x != "0x" ) then  
        #
        set processed_sub_map = "processed_sub.map"
        #
        \rm -f ${processed_sub_map}
        #
        \mv -f ${processed_fft}_sub.map ${processed_sub_map}
        #
        echo "# IMAGE-IMPORTANT: ${processed_sub_map} <MAP: Processed sub map >" >> LOGS/${scriptname}.results
        #
endif
#
echo "<<@progress: +5>>"

