#
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
#
set hkzFile = "APH/latlines.dat"
#
if ( ! -e ${hkzFile} ) then
   ${proc_2dx}/protest "ERROR: ${hkzFile} not found." 
endif
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "Back projecting reflections from hkz file ${hkzFile}"
#------------------------------------------------------------------------
#
set back_projected_hkl = "SCRATCH/back_projected.hkl"
#
\rm -f ${back_projected_hkl}
#
${bin_2dx}/volume_processor.exe --hkzin ${hkzFile} -s ${SYM} -X ${cellx} -Y ${celly} -Z ${ALAT} --gamma ${realang} --res ${RESMAX} --hklout ${back_projected_hkl}
#
#
echo "<<@progress: +10>>"
#
#
########################################################################
# STEP 2: Convert back projected hkl to mtz
########################################################################
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "F2MTZ : to convert back projected hkl to mtz.."
#------------------------------------------------------------------------
#
set back_projected_f2mtz_mtz = "SCRATCH/back_projected_f2mtz_MRClefthanded.mtz"
#
\rm -f ${back_projected_f2mtz_mtz}
#
${bin_ccp4}/f2mtz hklin ${back_projected_hkl} hklout ${back_projected_f2mtz_mtz} << eof
TITLE  P1 map, ${date}
CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}
SYMMETRY 1
LABOUT H K L F PHI FOM
CTYPOUT H H H F P W
FILE ${back_projected_hkl}
SKIP 0
END
eof
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "CAD : to finalize back projected mtz file.."
#------------------------------------------------------------------------
#
set back_projected_cad_mtz = "SCRATCH/back_projected_cad_MRClefthanded.mtz"
#
\rm -f ${back_projected_cad_mtz}
#
#
${bin_ccp4}/cad hklin1 ${back_projected_f2mtz_mtz} hklout ${back_projected_cad_mtz} << eof
sort h k l
resolution overall ${RESMAX} ${RESMIN}  
outlim spacegroup 1
labin file 1 all
valm NaN NOOUTPUT
end
eof
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "Writing the back projected mtz file"
#------------------------------------------------------------------------
#
set back_projected_mtz = "APH/back_projected_MRClefthanded.mtz"
#
\rm -f ${back_projected_mtz}
#
mv ${back_projected_cad_mtz} ${back_projected_mtz}
#
echo "# IMAGE: ${back_projected_mtz} <MTZ: Back projected MTZ (MRC lefthanded)>" >> LOGS/${scriptname}.results
#
echo "<<@progress: +5>>"
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "reindex - to flip hand of MTZ file for further work with CCP4"
#------------------------------------------------------------------------
#
set back_projected_mtz_righthanded = "APH/back_projected.mtz"
\rm -f ${back_projected_mtz_righthanded}
#
${bin_ccp4}/reindex hklin ${back_projected_mtz} hklout ${back_projected_mtz_righthanded} << eof
reindex HKL k,h,l
lefthand
end
eof
#
echo "# IMAGE-IMPORTANT: ${back_projected_mtz_righthanded} <MTZ: Back projected CCP4 MTZ file (righthanded) [H,K,L,F,P,FOM]>" >> LOGS/${scriptname}.results
#
#
########################################################################
# STEP 3: Convert back projected mtz to map
########################################################################
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "FFT : to convert mtz to map.."
#------------------------------------------------------------------------
#
set back_projected_fft = "SCRATCH/back_projected_fft"
set back_projected_fft_map = "${back_projected_fft}.map"
#
\rm -f ${back_projected_fft_map}
#
${bin_ccp4}/fft hklin ${back_projected_mtz} mapout ${back_projected_fft_map}  << eot
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
# Generate a readable map
#
#############################################################################
${proc_2dx}/linblock "sourcing 2dx_refine_raw_ccp4_map.com"
#############################################################################
#
source ${proc_2dx}/2dx_refine_raw_ccp4_map.com  ${back_projected_fft_map}
#
set back_projected_extended_map = "back_projected_extended.map"
#
\rm -f ${back_projected_extended_map}
#
\mv -f ${back_projected_fft}_extended.map ${back_projected_extended_map}
#
echo "# IMAGE: ${back_projected_extended_map} <MAP: Back projected extended map >" >> LOGS/${scriptname}.results
#
if ( ${calculate_subvolume}x != "0x" ) then  
        #
        set back_projected_sub_map = "back_projected_sub.map"
        #
        \rm -f ${back_projected_sub_map}
        #
        \mv -f ${back_projected_fft}_sub.map ${back_projected_sub_map}
        #
        echo "# IMAGE: ${back_projected_sub_map} <MAP: Back projected sub map >" >> LOGS/${scriptname}.results
        #
endif
#
echo "<<@progress: +5>>"
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "MAPROT : to correct the size of map"
#------------------------------------------------------------------------
#
set back_projected_maprot = "SCRATCH/back_projected_maprot.map"
#
\rm -f ${back_projected_maprot}
#
${bin_ccp4}/maprot mapin ${back_projected_fft_map} wrkout ${back_projected_maprot} << eot
MODE FROM
CELL WORK ${realcell} ${ALAT} 90.0 90.0 ${realang}
GRID WORK ${cellx} ${celly} ${ALAT}
XYZLIM 0 ${cellxm1} 0 ${cellym1} 0 ${ALATm1}
SYMM WORK 1
AVER
ROTA MATRIX   1.000 0.000 0.000      0.000 1.000 0.000    0.000 0.000 -1.000
TRANS  0.000 0.000 0.000
eot
#
#------------------------------------------------------------------------
${proc_2dx}/linblock "MAPMASK : to correct the axis of map"
#------------------------------------------------------------------------
#
set back_projected_map = "SCRATCH/back_projected.map"
#
\rm -f ${back_projected_map}
#
${bin_ccp4}/mapmask mapin ${back_projected_maprot} mapout ${back_projected_map} << eof
AXIS X,Y,Z
END
eof
#
echo "<<@progress: +5>>"


