#
#
#
#
#
# This is not an independent script.
# This should only be called from another script.
#
#
#
#
#
#
#
#
#
set frames_dir = SCRATCH/MA
#
if ( ! -e ${frames_dir} ) then
  ${proc_2dx}/protest "ERROR: First run Unbend Movie A1. Aborting."
endif
#
if ( ! -d MA ) then
  \mkdir MA
endif
#
if ( ${show_frame_PROFDATA} == "y" || ${show_frame_CCmap_marked} == "y" || ${show_frame_CCmap_unbent} == "y") then
  set tempkeep = "y"
endif
#
# echo  "# IMAGE-IMPORTANT: ${movie_stackname} <Raw image stack>" >> LOGS/${scriptname}.results
#
if ( ${ctfcor_imode}x == 9x ) then
  set iname = image_ctfcor_multiplied
else
  set iname = image_ctfcor
endif
#
#
echo "<<@progress: 5>>"
#
if ( ${movie_enable} == "n" ) then
  ${proc_2dx}/linblock "Skipping movie mode unbending."
  exit
endif
#
if ( ${movie_stackname} == "ScriptWillPutNameHere" ) then
  ${proc_2dx}/linblock "Movie-file not found. Skipping script."
  set movie_enable = "n"
  echo "set movie_enable = ${movie_enable}" >> LOGS/${scriptname}.results
  exit
endif
#
${proc_2dx}/linblock "Movie Mode."
if ( -e ${movie_stackname}.mrc ) then
  set movie_stackname = ${movie_stackname}.mrc
  echo "set movie_stackname = ${movie_stackname}"  >> LOGS/${scriptname}.results
endif
#
if ( ${MASKING_done} == "y" ) then
  if ( ${movie_masking_mode} == "0" ) then
    ${proc_2dx}/linblock "WARNING: Correcting Frame Masking Mode to 1=Masking based on UnbendII"
    set movie_masking_mode = 1
    echo "set movie_masking_mode = ${movie_masking_mode}" >> LOGS/${scriptname}.results
  endif
endif
# 
if ( ! -e ${movie_stackname}) then
  ${proc_2dx}/protest "ERROR: ${movie_stackname} missing. Aborting."
else
  # Get the number of frames
  e2iminfo.py -H ${movie_stackname} > tmp_stack_header.txt
  set movie_imagenumber = `\grep "MRC.nz:" tmp_stack_header.txt | cut -d' ' -f 2`
  echo "set movie_imagenumber_total = ${movie_imagenumber}"  >> LOGS/${scriptname}.results
  set movie_imagenumber_touse = `echo ${movie_imagenumber_total} ${movie_imagenumber_toskip} | awk '{ s = $1 - $2 } END { print s }'`
  echo "set movie_imagenumber_touse = ${movie_imagenumber_touse}"  >> LOGS/${scriptname}.results
  \rm tmp_stack_header.txt
endif
#
if ( ${movie_filter_type} == "0" ) then
  #
  # automatic calculation of A and B for exponential filter:  
  # filter type:   res = a * exp(b * N)  ; with N being the frame number, up to Nmax.
  # res should be between 0.0 and 0.5 (Nyquist)
  #
  # For first half of frames, resolution should be the full 0.5.
  # 0.50 = A * exp(B * Nmax/2)
  # At last frame, limit resolution to 0.25
  # 0.25 = A * exp(B * Nmax)
  # 
  # Second equation equals:  A = 0.25 / exp(B * Nmax)
  # Into first equation gives: 
  # 0.50 = 0.25 / exp(B * Nmax) * exp(B * Nmax/2)
  # or: 0.50 = 0.25 / exp(B * Nmax/2)
  # or: 0.5 = exp(B * Nmax/2) 
  # or: ln(0.5) = B * Nmax/2 
  # or: B = 2 * ln(0.5) / Nmax
  # or: B = -1.38629 / Nmax
  #
  # Insertion into first equation gives: 
  # A = 0.5 / exp(2*ln(0.5)/Nmax * Nmax/2)
  # A = 0.5 / exp(2*ln(0.5)/2)
  # A = 0.5 / 0.5 = 1.0
  #
  set filt_a = 1.0
  set filt_b = `echo ${movie_imagenumber_touse} | awk '{ s =  -1.38629 / $1 } END { print s }'`
  echo ":Automatic filters: filt_a = ${filt_a},  filt_b = ${filt_b}"
  echo "set filt_a = ${filt_a}"  >> LOGS/${scriptname}.results
  echo "set filt_b = ${filt_b}"  >> LOGS/${scriptname}.results
else
  if ( ${movie_filter_type} == "1" ) then
    set filt_a = `echo ${movie_filter_param} | sed 's/,/ /g' | awk '{ s = $1 } END { print s }'`
    set filt_b = `echo ${movie_filter_param} | sed 's/,/ /g' | awk '{ s = $2 } END { print s }'`
  else
    set filt_a = 1.0
    set filt_b = 0.0
  endif
endif
#
set num_dia = 100
#
if ( 1 == 1 ) then
  set movie_smoothing = 8
  echo "set movie_smoothing = ${movie_smoothing}"  >> LOGS/${scriptname}.results
endif
if ( ${movie_smoothing}x == "x" ) then
  set movie_smoothing = 8
  echo "set movie_smoothing = ${movie_smoothing}"  >> LOGS/${scriptname}.results
endif
#
#
setenv NCPUS ${Thread_Number}
#
set maskfile = tmp
if ( ${use_masked_image} == "y" ) then
  if ( -e ${nonmaskimagename}_manualmask.mrc ) then
    set maskfile = ${nonmaskimagename}_manualmask
  else
    if ( -e ${nonmaskimagename}_automask.mrc ) then
      set maskfile = ${nonmaskimagename}_automask
    else
      if ( -e ${nonmaskimagename}_mask.mrc ) then
        \mv ${nonmaskimagename}_mask.mrc ${nonmaskimagename}_automask.mrc
        set maskfile = ${nonmaskimagename}_automask
      else
        ${proc_2dx}/linblock "No Masking Info file found.  Not masking image."
        set use_masked_image = "n"
        echo "set use_masked_image = ${use_masked_image}"  >> LOGS/${scriptname}.results
      endif 
    endif 
  endif 
endif
#
if ( ${movie_masking_mode} == '1' && ${use_masked_image} == "y" ) then 
  if ( ! -e ${maskfile}.mrc ) then
    ${proc_2dx}/protest "ERROR: ${maskfile}.mrc not found. First run UNBEND-II with masking option."
  endif 
  echo  "# IMAGE: ${maskfile}.mrc <Crystal Masking Pattern>" >> LOGS/${scriptname}.results
endif
#
if ( ! -e ${iname}.mrc ) then
  ${proc_2dx}/protest "ERROR:  ${iname}.mrc missing. First run script Correct CTF on tiles"
endif

set PROFDATA = ${nonmaskimagename}_profile.dat
if ( ! -e ${PROFDATA} ) then
  ${proc_2dx}/protest "ERROR: First run Unbend II."
endif
#
set cormap = ${iname}_CCmap_unbend2.mrc
#
if ( ! -e ${cormap} ) then
  ${proc_2dx}/protest "ERROR: First run Unbend-I and Unbend-II scripts. File missing: ${cormap}"
endif
# 
set unbent_fil = unbent.mrc
#
set maskb = ${maskb01}
set rmax = 11000
#
echo "<<@progress: 10>>"
#
###########################################################################
${proc_2dx}/linblock "Refining dose-dependent drift of lattice positions" 
###########################################################################
#
setenv NCPUS ${Thread_Number}
#
${bin_2dx}/2dx_movie_refine.exe << eot
${frames_dir}/CCmap
${movie_imagenumber_touse}
${PROFDATA}
eot
#
\mv -f 2dx_movie_refine.dat MA/2dx_movie_refine.dat
echo "<<@progress: 15>>"
#


###########################################################################
${proc_2dx}/linblock "drift_selector.py - Selecting and smoothing drift profiles" 
###########################################################################
#
touch ${frames_dir}/PROFDATA_1.dat
\rm ${frames_dir}/PROFDATA_*.dat
\rm -f ${maskfile}_mask.mrc
set lat_string = `echo ${lattice} | sed 's/,/ /g'`
if ( ${movie_masking_mode} == '1' && ${use_masked_image} == "y" ) then 
  ${app_python} ${proc_2dx}/movie/drift_selector.py MA/2dx_movie_refine.dat MA/2dx_movie_refine_selected.dat ${movie_drift_threshold} ${PROFDATA} ${movie_smoothing} ${movie_imagenumber_touse} ${imagesidelength} ${maskfile}.mrc ${maskfile}_mask.mrc ${lat_string} ${frames_dir}
else
  ${app_python} ${proc_2dx}/movie/drift_selector.py MA/2dx_movie_refine.dat MA/2dx_movie_refine_selected.dat ${movie_drift_threshold} ${PROFDATA} ${movie_smoothing} ${movie_imagenumber_touse} ${imagesidelength} ${maskfile}_mask.mrc ${lat_string} ${frames_dir}
endif
#
echo ":: Done. Now saving ${maskfile}_mask.mrc"
echo  "# IMAGE-IMPORTANT: ${maskfile}.mrc <Original crystal mask>" >> LOGS/${scriptname}.results
echo  "# IMAGE-IMPORTANT: ${maskfile}_mask.mrc <Masked crystal mask>" >> LOGS/${scriptname}.results
#
#
echo  "# IMAGE: ${PROFDATA} <Unbending profile for average image (TXT)>" >> LOGS/${scriptname}.results
if ( ${show_frame_PROFDATA} == "y" ) then
  set i = 1
  while ($i <= ${movie_imagenumber_touse})
    echo  "# IMAGE: ${frames_dir}/PROFDATA_${i}.dat <Unbending profile for frame ${i} (TXT)>" >> LOGS/${scriptname}.results
    @ i += 1
  end
endif
#
echo  "# IMAGE: MA/2dx_movie_refine.dat <Movie drift profiles (TXT)>" >> LOGS/${scriptname}.results
echo  "# IMAGE: MA/2dx_movie_refine_selected.dat <Movie drift profiles, selected (TXT)>" >> LOGS/${scriptname}.results
echo "<<@progress: 20>>"



###########################################################################
${proc_2dx}/linblock "Unbending all frames" 
###########################################################################
#
echo "# IMAGE: weight.mrc <Weight function for adding frames in Fourier space>" >> LOGS/${scriptname}.results
#
set i = 1
while ($i <= ${movie_imagenumber_touse})

       if ( ${show_frame_CCmap_marked} == "y" ) then
         \rm -f ${frames_dir}/CCmap_${i}_marked.mrc
         ${bin_2dx}/2dx_mark_spots.exe << eot
${frames_dir}/CCmap_${i}.mrc
${frames_dir}/CCmap_${i}_marked.mrc
${frames_dir}/PROFDATA_${i}.dat
2
eot
         echo "# IMAGE: SCRATCH/${iname}_CCmap21_marked.mrc <CCmap, marked>" >> LOGS/${scriptname}.results 
         #
       endif


       ###########################################################################
       ${proc_2dx}/linblock "CCUNBEND - Unbend CCmap for frame ${i}"
       ###########################################################################
       setenv CCORDATA ${frames_dir}/PROFDATA_${i}.dat
       setenv TABLEOUT ${frames_dir}/CCUNBEND_Table_${i}.dat
       \rm -f ${frames_dir}/CCUNBEND_Table_${i}.dat
       \rm -f CCPLOT.PS
       \rm -f ${frames_dir}/CCmap_${i}_unbent.mrc

       if ( ${show_frame_CCmap_unbent} == "y" ) then
         ${bin_2dx}/2dx_ccunbendk.exe << eot
${frames_dir}/CCmap_${i}.mrc
0,1,${IMAXCOR},${ISTEP},F,40,T       !ITYPE,IOUT,IMAXCOR,ISTEP,LTAPER,RTAPER,LTABOUT
30,52,0.001,${facthresha},${TLTAXIS},${RMAG},${LCOLOR}     !IKX,IKY,EPS,FACTOR,TLTAXIS,RMAG,LCOLOR
${imagename}, Movie-Mode UNBEND, ${date}
${frames_dir}/CCmap_${i}_unbent.mrc
CCUNBEND, f${i}.mrc, ${date}
eot
         echo  "# IMAGE: ${frames_dir}/CCmap_${i}_unbent.mrc <CCmap Frame ${i} unbent>" >> LOGS/${scriptname}.results
       endif

       ###########################################################################
       ${proc_2dx}/lin "CCUNBEND - Unbend frame ${i}"
       ###########################################################################
       \rm -f ${frames_dir}/CCUNBEND_Table_${i}.dat
       \rm -f CCPLOT.PS
       \rm -f ${frames_dir}/CCUNBEND_f${i}_notap.mrc
       if (${ctfcor_imode}x == "0x" || ${ctfcor_imode}x == "4x" || ${ctfcor_imode}x == "5x" || ${ctfcor_imode}x == "6x" ) then
	   set infile = ${frames_dir}/f${i}.mrc
       else
         set infile = ${frames_dir}/f${i}_ctfcor.mrc
       endif

       ${bin_2dx}/2dx_ccunbendk.exe << eot
${infile}
0,1,${IMAXCOR},${ISTEP},F,40,T       !ITYPE,IOUT,IMAXCOR,ISTEP,LTAPER,RTAPER,LTABOUT
30,52,0.001,${facthresha},${TLTAXIS},${RMAG},${LCOLOR}     !IKX,IKY,EPS,FACTOR,TLTAXIS,RMAG,LCOLOR
${imagename}, Movie-Mode UNBEND, ${date}
${frames_dir}/CCUNBEND_f${i}_notap.mrc
CCUNBEND, f${i}.mrc, ${date}
eot

       ###########################################################################
       echo "Storing distortion-vector-field for visual inspection"
       ###########################################################################
       if ( ! -e CCPLOT.PS ) then
          ${proc_2dx}/protest "ERROR: CCPLOT.PS missing"
       endif
       #
       \cp -f CCPLOT.PS ${frames_dir}/PS/CCUNBEND_${i}.ps

       if ( ${movie_ghostscript_installed} == "y" ) then

         if ( ${i} == '1' ) then
          if ( ${ps2pdf} == "pstopdf" ) then
             ${ps2pdf} CCPLOT.PS  
             \mv -f CCPLOT.pdf frame_unbending.pdf
          else
             ${ps2pdf} CCPLOT.PS frame_unbending.pdf 
          endif
          pdftk A=frame_unbending.pdf cat A1 output out.pdf 
          \mv -f out.pdf frame_unbending.pdf
         else
          if ( ${ps2pdf} == "pstopdf" ) then
             ${ps2pdf} CCPLOT.PS 
          else
             ${ps2pdf} CCPLOT.PS CCPLOT.pdf 
          endif
          pdftk A=frame_unbending.pdf B=CCPLOT.pdf cat A1-end B1 output out.pdf 
          \mv -f out.pdf frame_unbending.pdf
         endif
       endif

       ###############################################################
       ${proc_2dx}/lin "apply_filter.py - Resolution limiting frame ${i}"
       ###############################################################  
       #
       set filtervalue = `${app_python} ${proc_2dx}/movie/getFilter.py ${i} ${movie_filter_type} ${filt_a} ${filt_b}`
       echo ":     Filter unbent frame ${i} with radius ${filtervalue}"
       echo "${app_python} ${proc_2dx}/movie/getFilter.py ${i} ${movie_filter_type} ${filt_a} ${filt_b}"
       #
       if ( ${show_frame_CCmap_unbent} == "y" ) then
         ${app_python} ${proc_2dx}/movie/apply_filter.py ${frames_dir}/CCmap_${i}_unbent.mrc         ${filtervalue} ${i} ${imagesidelength} ${frames_dir}/weight.mrc
       endif
       ${app_python} ${proc_2dx}/movie/apply_filter.py ${frames_dir}/CCUNBEND_f${i}_notap.mrc ${filtervalue} ${i} ${imagesidelength} ${frames_dir}/weight.mrc
       #
       ## This is to make sure the script shows up in the GUI:
       if ( 1 == 2 ) then
         ${app_python} ${proc_2dx}/movie/getFilter.py ${i} ${movie_filter_type} ${filt_a} ${filt_b}
       endif
       #
 
       set prog_num = `echo ${i} ${movie_imagenumber_touse} | awk '{ s = 20 + int( 50 * $1 / $2 ) } END { print s }'` 
       echo "<<@progress: ${prog_num}>>"
         
       @ i += 1
end
#



echo "<<@progress: 70>>"

if ( ${show_frame_CCmap_unbent} == "y" ) then
  ###########################################################################
  ${proc_2dx}/linblock "Averaging unbent CCmaps from frames 1 to ${movie_imagenumber_touse}" 
  ###########################################################################
  \rm -f ${frames_dir}/CCmap_driftCorrected.mrc
  ${app_python} ${proc_2dx}/movie/direct_sum2.py ${movie_imagenumber_touse} ${frames_dir}
  echo "# IMAGE-IMPORTANT: ${frames_dir}/CCmap_driftCorrected.mrc <CCmap drift-corrected>" >> LOGS/${scriptname}.results  


  \rm -f SCRATCH/${iname}_CCmap21_marked.mrc
  ${bin_2dx}/2dx_mark_spots.exe << eot
${frames_dir}/CCmap_driftCorrected.mrc
${frames_dir}/CCmap_driftCorrected_marked.mrc
${nonmaskimagename}_profile.dat
2
eot
  echo "# IMAGE: ${frames_dir}/CCmap_driftCorrected_marked.mrc <CCmap drift-corrected, marked>" >> LOGS/${scriptname}.results 
endif


###########################################################################
${proc_2dx}/linblock "Averaging unbent images from frames 1 to ${movie_imagenumber_touse}" 
###########################################################################
\rm -f ${frames_dir}/direct_sum.mrc
${app_python} ${proc_2dx}/movie/direct_sum.py ${movie_imagenumber_touse} ${nonmaskimagename} ${frames_dir}
echo "# IMAGE: ${frames_dir}/direct_sum.mrc <Sum unbent images>" >> LOGS/${scriptname}.results 


###########################################################################
${proc_2dx}/linblock "Filtering by resolution weights"
###########################################################################
if ( ${show_frame_CCmap_unbent} == "y" ) then
  ${app_python} ${proc_2dx}/movie/apply_filter_sum.py ${frames_dir}/CCmap_driftCorrected.mrc ${frames_dir}/CCmap_driftCorrected_filt.mrc 0.001 ${frames_dir}/weight.mrc
  echo "# IMAGE: ${frames_dir}/CCmap_driftCorrected_filt.mrc <CCmap drift-corrected, filtered>" >> LOGS/${scriptname}.results 
endif

${app_python} ${proc_2dx}/movie/apply_filter_sum.py ${frames_dir}/direct_sum.mrc ${frames_dir}/direct_sum_filt.mrc 0.001 ${frames_dir}/weight.mrc
echo "# IMAGE: ${frames_dir}/direct_sum_filt.mrc <Sum unbent images, filtered>" >> LOGS/${scriptname}.results 



if ( ${ctfcor_imode} == "1" || ${ctfcor_imode} == "2" ) then
  ###########################################################################
  ${proc_2dx}/linblock "Filtering by CTF profile"
  ###########################################################################
  \rm -f ${frames_dir}/direct_sum_filt_ctf.mrc
  ${app_python} ${proc_2dx}/movie/apply_filter_fourier.py ${frames_dir}/direct_sum_filt.mrc SCRATCH/2dx_ctfcor_ctffile.mrc ${frames_dir}/direct_sum_filt_ctf.mrc ${ctfcor_noise}

  if ( ${ctfcor_imode} == "2" ) then
    echo "# IMAGE: SCRATCH/2dx_ctfcor_ctffile.mrc <Summed-CTF**2 (Noise=${ctfcor_noise})>" >> LOGS/${scriptname}.results 
  else
    echo "# IMAGE: SCRATCH/2dx_ctfcor_ctffile.mrc <Summed-CTF (Noise=${ctfcor_noise})>" >> LOGS/${scriptname}.results 
  endif
  echo "# IMAGE: ${frames_dir}/direct_sum_filt_ctf.mrc <Sum unbent images, filtered, CTF-corrected>" >> LOGS/${scriptname}.results 
else
  \rm -f ${frames_dir}/direct_sum_filt_ctf.mrc
  \cp -f ${frames_dir}/direct_sum_filt.mrc ${frames_dir}/direct_sum_filt_ctf.mrc
endif





if ( ${movie_masking_mode} != '0' ) then
  if ( ${show_frame_CCmap_unbent} == "y" ) then
    ###########################################################################
    ${proc_2dx}/linblock "Masking drift-corrected CCmap with masking info"
    ###########################################################################
    ${app_python} ${proc_2dx}/movie/mask.py ${frames_dir}/CCmap_driftCorrected_filt.mrc ${frames_dir}/CCmap_driftCorrected_filt_masked.mrc ${maskfile}_mask.mrc
    echo "# IMAGE: ${frames_dir}/CCmap_driftCorrected_filt_masked.mrc <CCmap drift-corrected, filtered, masked>" >> LOGS/${scriptname}.results 
  endif

  ###########################################################################
  ${proc_2dx}/linblock "Masking average with masking info"
  ###########################################################################
  ${app_python} ${proc_2dx}/movie/mask.py ${frames_dir}/direct_sum_filt.mrc ${frames_dir}/direct_sum_filt_masked.mrc ${maskfile}_mask.mrc
  echo "# IMAGE: ${frames_dir}/direct_sum_filt_masked.mrc <Sum unbent images, filtered, masked>" >> LOGS/${scriptname}.results 

  ${app_python} ${proc_2dx}/movie/mask.py ${frames_dir}/direct_sum_filt_ctf.mrc ${frames_dir}/direct_sum_filt_ctf_masked.mrc ${maskfile}_mask.mrc
  echo "# IMAGE: ${frames_dir}/direct_sum_filt_ctf_masked.mrc <Sum unbent images, filtered, CTFcor, masked>" >> LOGS/${scriptname}.results 
else
  ######################################################
  ${proc_2dx}/lin "Not masking frame"
  ######################################################
endif


if ( ${movie_ghostscript_installed} == "y" ) then
  ###########################################################################
  ${proc_2dx}/linblock "drift_plotter.py - Producing plots of drift"
  ###########################################################################
  echo  "# IMAGE-IMPORTANT: MA/2dx_movie_refine.dat <2dx_movie_refine.dat>" >> LOGS/${scriptname}.results  
  #
  if ( ${show_frame_CCmap_unbent} == "y" ) then
    ${app_python} ${proc_2dx}/movie/drift_plotter.py MA/2dx_movie_refine.dat MA/2dx_movie_refine.pdf ${frames_dir}/CCmap_driftCorrected_filt.mrc
  else
    ${app_python} ${proc_2dx}/movie/drift_plotter.py MA/2dx_movie_refine.dat MA/2dx_movie_refine.pdf ${cormap}
  endif
  echo  "# IMAGE-IMPORTANT: MA/2dx_movie_refine.pdf <PDF: 2dx_movie_refine.pdf>" >> LOGS/${scriptname}.results
  #
  if ( ${movie_masking_mode} != '0' ) then
    if ( ${show_frame_CCmap_unbent} == "y" ) then
      ${app_python} ${proc_2dx}/movie/drift_plotter.py MA/2dx_movie_refine_selected.dat MA/2dx_movie_refine_selected.pdf ${frames_dir}/CCmap_driftCorrected_filt_masked.mrc
    else
      ${app_python} ${proc_2dx}/movie/drift_plotter.py MA/2dx_movie_refine_selected.dat MA/2dx_movie_refine_selected.pdf ${cormap}
    endif
    echo  "# IMAGE-IMPORTANT: MA/2dx_movie_refine_selected.pdf <PDF: MA/2dx_movie_refine_selected.pdf>" >> LOGS/${scriptname}.results
  endif
endif
\rm -f tmp.png
echo "<<@progress: 10>>"





###########################################################################
${proc_2dx}/linblock "TAPEREDGE - Tapering edge of summed frames"
###########################################################################

if ( ${movie_masking_mode} != '0' ) then
  set infile = ${frames_dir}/direct_sum_filt_masked.mrc
else
  set infile = ${frames_dir}/direct_sum_filt.mrc
endif

\rm -f MA/direct_sum_filt_upscale.mrc
${bin_2dx}/labelh.exe << eot
${infile}
39
MA/direct_sum_filt_upscale.mrc
eot

setenv IN  MA/direct_sum_filt_upscale.mrc
setenv OUT ${frames_dir}/direct_sum_fixed.mrc
\rm -f     ${frames_dir}/direct_sum_fixed.mrc
${bin_2dx}/2dx_taperedgek.exe << eot
30,30,100,30       ! IAVER,ISMOOTH,ITAPER
eot
echo "# IMAGE: ${frames_dir}/direct_sum_fixed.mrc <Sum unbent images, edge-tapered>" >> LOGS/${scriptname}.results 

if ( ${movie_masking_mode} != '0' ) then
  set infile = ${frames_dir}/direct_sum_filt_ctf_masked.mrc
else
  set infile = ${frames_dir}/direct_sum_filt_ctf.mrc
endif

\rm -f MA/direct_sum_filt_ctf_upscale.mrc
${bin_2dx}/labelh.exe << eot
${infile}
39
MA/direct_sum_filt_ctf_upscale.mrc
eot

setenv IN  MA/direct_sum_filt_ctf_upscale.mrc
setenv OUT ${frames_dir}/direct_sum_ctf_fixed.mrc
\rm -f     ${frames_dir}/direct_sum_ctf_fixed.mrc
${bin_2dx}/2dx_taperedgek.exe << eot
30,30,100,30       ! IAVER,ISMOOTH,ITAPER
eot
echo "# IMAGE: ${frames_dir}/direct_sum_ctf_fixed.mrc <Sum unbent images, CTFcor, edge-tapered>" >> LOGS/${scriptname}.results 



###########################################################################
${proc_2dx}/linblock "FFTRANS - Producing final FFT"
###########################################################################

set outfile = APH/${iname}_movie_fou.aph
if ( ${ctfcor_imode}x == 9x ) then
  echo ${ctfcor_imode} > APH/image_ctfcor_movie_fou.aph_ctfcor_imode
else
  echo ${ctfcor_imode} > APH/${iname}_movie_fou.aph_ctfcor_imode
endif
set outfft = MA/direct_sum_ctf_fft.mrc
set outfft_noctf = MA/direct_sum_fft.mrc


setenv IN ${frames_dir}/direct_sum_fixed.mrc
setenv OUT ${outfft_noctf}
\rm -f     ${outfft_noctf}
${bin_2dx}/2dx_fftrans.exe
echo "# IMAGE: MA/direct_sum_fft.mrc <Final FFT>" >> LOGS/${scriptname}.results

setenv IN ${frames_dir}/direct_sum_ctf_fixed.mrc
setenv OUT ${outfft}
\rm -f     ${outfft}
${bin_2dx}/2dx_fftrans.exe
echo "# IMAGE: ${outfft} <Final FFT (CTFcor)>" >> LOGS/${scriptname}.results


###########################################################################
${proc_2dx}/linblock "MMBOX - Evaluating APH values (no CTFcor)"
###########################################################################

set imagecenterx = `echo ${imagesidelength} | awk '{ s = int( $1 / 2 ) } END { print s }'`
set imagecentery = ${imagecenterx}
#
\rm -f dummy.aph
#
${bin_2dx}/2dx_mmboxa.exe << eot
${outfft_noctf}
${imagenumber} ${nonmaskimagename}, MA, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
dummy.aph
SCRATCH/TMP9873.dat
UMA
${imagecenterx},${imagecentery}           ! XORIG,YORIG
200.0,1.5,1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot

\rm -f dummy.aph

source SCRATCH/TMP9873.dat

echo "<<@progress: 80>>"


###########################################################################
${proc_2dx}/linblock "Generate IQ-stat output"
###########################################################################

echo "set QVAL = ${QVAL_local}" >> LOGS/${scriptname}.results

echo "set UMA_IQ1 = ${UMA_IQ1}" >> LOGS/${scriptname}.results
echo "set UMA_IQ2 = ${UMA_IQ2}" >> LOGS/${scriptname}.results
echo "set UMA_IQ3 = ${UMA_IQ3}" >> LOGS/${scriptname}.results
echo "set UMA_IQ4 = ${UMA_IQ4}" >> LOGS/${scriptname}.results
echo "set UMA_IQ5 = ${UMA_IQ5}" >> LOGS/${scriptname}.results
echo "set UMA_IQ6 = ${UMA_IQ6}" >> LOGS/${scriptname}.results
echo "set UMA_IQ7 = ${UMA_IQ7}" >> LOGS/${scriptname}.results
echo "set UMA_IQ8 = ${UMA_IQ8}" >> LOGS/${scriptname}.results
echo "set UMA_IQ9 = ${UMA_IQ9}" >> LOGS/${scriptname}.results
echo "set QVALMA = ${QVAL_local}" >> LOGS/${scriptname}.results

set RP_6 = ${PSMAX}
echo "set RP_1 = ${RP_1}" >> LOGS/${scriptname}.results
echo "set RP_2 = ${RP_2}" >> LOGS/${scriptname}.results
echo "set RP_3 = ${RP_3}" >> LOGS/${scriptname}.results
echo "set RP_4 = ${RP_4}" >> LOGS/${scriptname}.results
echo "set RP_5 = ${RP_5}" >> LOGS/${scriptname}.results
echo "set RP_6 = ${RP_6}" >> LOGS/${scriptname}.results

echo "<<@evaluate>>"

echo "<<@progress: 90>>"

if ( ${movie_ghostscript_installed} == "y" ) then
  \rm -f frame_unbending.ps
  ${pdf2ps} frame_unbending.pdf frame_unbending.ps
  echo "# IMAGE-IMPORTANT: frame_unbending.ps <PS: Profiles CCUNBEND>" >> LOGS/${scriptname}.results
endif

set IQS = `echo ${UMA_IQ1} ${UMA_IQ2} ${UMA_IQ3} ${UMA_IQ4} ${UMA_IQ5} ${UMA_IQ6} ${UMA_IQ7} ${UMA_IQ8} ${UMA_IQ9}`
echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
echo "::maskb=${maskb}, movie_refboxa=${movie_refboxa}: QValMA= ${QVAL_local} ... IQ stat = ${IQS}"
echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

echo " " >> History.dat
echo ":Date: ${date}" >> History.dat
echo "::Unbend MA: maskb=${maskb}, movie_refboxa=${movie_refboxa}: QVal= ${QVAL_local} ... IQ stat = ${IQS}" >> History.dat
#



###########################################################################
${proc_2dx}/linblock "MMBOX - Evaluating APH values (after CTF cor)"
###########################################################################
#
\rm -f SCRATCH/TMP9873.dat
\rm -f ${outfile}
#
${bin_2dx}/2dx_mmboxa.exe << eot
${outfft}
${imagenumber} ${nonmaskimagename}, MA, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
${outfile}
SCRATCH/TMP9873.dat
UMA
${imagecenterx},${imagecentery}           ! XORIG,YORIG
200.0,1.5,1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot

echo "# IMAGE-IMPORTANT: ${outfile} <APH file after MA unbending>" >> LOGS/${scriptname}.results
  



if ( ${tempkeep} != "y" ) then
  ###########################################################################
  ${proc_2dx}/linblock "Deleting frame averages temporay files"
  ###########################################################################
  set olddir = $PWD
  cd ${frames_dir}
  \rm -rf frame*
  cd olddir
endif


