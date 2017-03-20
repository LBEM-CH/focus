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
set frames_dir = SCRATCH/MA
#
if ( ! -e ${frames_dir} ) then
  \mkdir ${frames_dir}
endif
#
if ( ${show_frames} == "y" || ${show_frame_FFT} == "y" || ${show_frame_CCmap} == "y") then
  set tempkeep = "y"
endif
#
if ( ${ctfcor_imode}x == 9x ) then
  set iname = image_ctfcor_multiplied
else
  set iname = image_ctfcor
endif
#
# echo  "# IMAGE-IMPORTANT: ${movie_stackname} <Raw image stack>" >> LOGS/${scriptname}.results
#
if ( ${movie_enable} == "n" ) then
  ${proc_2dx}/linblock "Skipping movie mode unbending."
  exit
endif
#
set ctfcor_noise_toosmall = `echo ${ctfcor_noise} | awk '{ if ( s < 0.8 ) { s = 1 } else { s = 0 } } END { print s } '`
if ( ${ctfcor_noise_toosmall} == "1" ) then
  echo ":: Correcting CTF-correction Noise value to 0.8."
  set ctfcor_noise = "0.8"
  echo "set ctfcor_noise = ${ctfcor_noise}" >> LOGS/${scriptname}.results
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
  #
  echo  "# IMAGE: ${movie_stackname} <Input Movie>" >> LOGS/${scriptname}.results  
  #
  # Get the number of frames
  e2iminfo.py -H ${movie_stackname} > tmp_stack_header.txt
  set nx = `\grep "MRC.mx:" tmp_stack_header.txt | cut -d' ' -f 2`
  set ny = `\grep "MRC.my:" tmp_stack_header.txt | cut -d' ' -f 2`
  set nz = `\grep "MRC.mz:" tmp_stack_header.txt | cut -d' ' -f 2`
  set movie_imagenumber = ${nz}
  ${proc_2dx}/linblock "Stack contains ${movie_imagenumber} frames"
  set movie_imagenumber_total = ${movie_imagenumber}
  echo "set movie_imagenumber_total = ${movie_imagenumber_total}"  >> LOGS/${scriptname}.results
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
setenv NCPUS ${Thread_Number}
#
if ( ${movie_masking_mode} == '1' && ${use_masked_image} == "y" ) then 
  if ( ! -e ${maskfile}.mrc ) then
    ${proc_2dx}/protest "ERROR: ${maskfile}.mrc not found. First run UNBEND-II with masking option."
    # ${proc_2dx}/linblock "WARNING: Continuing withoug masking."
    # set movie_masking_mode = 0
    # echo "set movie_masking_mode = ${movie_masking_mode}" >> LOGS/${scriptname}.results
  endif 
  echo  "# IMAGE: ${maskfile}.mrc <Crystal Masking Pattern>" >> LOGS/${scriptname}.results
endif
#
if ( ! -e ${iname}.mrc ) then
  ${proc_2dx}/protest "ERROR:  ${iname}.mrc missing. First run script Correct CTF on stripes"
endif
#
#
#
#
#
#
# Generate subfolder for frame images
\rm -rf ${frames_dir}
if ( ! -d ${frames_dir} ) then
  \mkdir ${frames_dir}
  set olddir = $PWD
  cd ${frames_dir}
  if ( ! -d PS ) then
    \mkdir PS
  endif
  cd ${olddir}
endif
#
#
#
#
##########################################################################
##########################################################################
# Now preparing reference:
##########################################################################
##########################################################################
#
echo "<<@progress: 5>>"
#
set PROFDATA = ${nonmaskimagename}_profile.dat
if ( ! -e ${PROFDATA} ) then
  if ( -e image_ctfcor_profile.dat ) then
    \mv -f image_ctfcor_profile.dat ${PROFDATA}
  endif
endif
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
###############################################################
###############################################################
###############################################################
${proc_2dx}/linblock "Preparing reference from Fourier-filtered UNBEND-II result"
###############################################################
###############################################################
###############################################################
#
###############################################################
${proc_2dx}/linblock "FFTRANS - Calculate FFT of unbent image"
###############################################################
#
set unbent_fil = unbent.mrc  
#
if ( ! -e ${unbent_fil} ) then
  ${proc_2dx}/protest "ERROR: File missing: ${unbent_fil}"
endif
# 
\rm -f SCRATCH/reference_flt_upscale.mrc
${bin_2dx}/labelh.exe << eot
${unbent_fil}
39
${frames_dir}/reference_flt_upscale.mrc
eot
#
setenv IN  ${frames_dir}/reference_flt_upscale.mrc
setenv OUT ${frames_dir}/reference_flt_upscale_fft.mrc
\rm -f     ${frames_dir}/reference_flt_upscale_fft.mrc
${bin_2dx}/2dx_fftrans.exe  
#
echo  "# IMAGE: ${frames_dir}/reference_flt_upscale_fft.mrc <Unbent image (FFT)>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 15>>"
#
#
#########################################################################
${proc_2dx}/linblock "MASKTRAN - Lattice-mask FFT of unbent image, small holes"
#########################################################################
set maskb = ${maskb01}
set rmax = 11000  
#
setenv IN  ${frames_dir}/reference_flt_upscale_fft.mrc
setenv OUT ${frames_dir}/reference_flt_upscale_fft_mask.mrc
\rm -f     ${frames_dir}/reference_flt_upscale_fft_mask.mrc
setenv SPOTS ${nonmaskimagename}.spt
${bin_2dx}/2dx_masktrana.exe << eot
1 F T F ! ISHAPE=1(CIRC),2(GAUSCIR),3(RECT)HOLE,IAMPLIMIT(T or F),ISPOT,IFIL
1 ! RADIUS OF HOLE IF CIRCULAR, X,Y HALF-EDGE-LENGTHS IF RECT.
${lattice},-50,50,-50,50,${rmax},1 ! A/BX/Y,IH/IKMN/MX,RMAX,ITYPE
eot
echo  "# IMAGE: ${frames_dir}/reference_flt_upscale_fft_mask.mrc <Unbent image, Fourier-filtered (1px) (FFT)>" >> LOGS/${scriptname}.results 
#
if ( ${tempkeep} != "y" ) then
  \rm -f SCRATCH/reference_flt_upscale_fft.mrc
endif
#
#
###############################################################
${proc_2dx}/linblock "FFTRANS - Back to real space"
###############################################################
#
setenv IN  ${frames_dir}/reference_flt_upscale_fft_mask.mrc
setenv OUT ${frames_dir}/reference_flt_upscale_fft_mask_fft.mrc
\rm -f     ${frames_dir}/reference_flt_upscale_fft_mask_fft.mrc
${bin_2dx}/2dx_fftrans.exe  
#
echo  "# IMAGE: ${frames_dir}/reference_flt_upscale_fft_mask_fft.mrc <Unbent image, Fourier-filtered>" >> LOGS/${scriptname}.results  
#
if ( ${tempkeep} != "y" ) then
  \rm -f ${frames_dir}/reference_flt_upscale_fft_mask.mrc
endif
#
echo "<<@progress: 17>>"
#
#
###############################################################
${proc_2dx}/linblock "BOXIMAGE - Boxing reference: ${movie_refboxa}"
###############################################################
#
${app_python} ${proc_2dx}/movie/box_reference.py ${frames_dir}/reference_flt_upscale_fft_mask_fft.mrc ${frames_dir}/reference_flt_upscale_fft_mask_fft_box.mrc ${movie_refboxa}
#
if ( ${tempkeep} != "y" ) then
  \rm -f ${frames_dir}/reference_flt_upscale_fft_mask_fft.mrc
endif
#
\rm -f ${frames_dir}/reference_flt_upscale_fft_mask_fft_box_upscale.mrc
${bin_2dx}/labelh.exe << eot
${frames_dir}/reference_flt_upscale_fft_mask_fft_box.mrc
39
${frames_dir}/reference_flt_upscale_fft_mask_fft_box_upscale.mrc
eot
echo  "# IMAGE-IMPORTANT: ${frames_dir}/reference_flt_upscale_fft_mask_fft_box_upscale.mrc <Reference (${movie_refboxa}px)>" >> LOGS/${scriptname}.results
#
#
###############################################################
${proc_2dx}/linblock "FFTRANS - Producing reference in Fourier space"
###############################################################
#
setenv IN  ${frames_dir}/reference_flt_upscale_fft_mask_fft_box_upscale.mrc
setenv OUT ${frames_dir}/reference_flt_upscale_fft_mask_fft_box_fft.mrc
\rm -f     ${frames_dir}/reference_flt_upscale_fft_mask_fft_box_fft.mrc
${bin_2dx}/2dx_fftrans.exe 
#
echo  "# IMAGE: ${frames_dir}/reference_flt_upscale_fft_mask_fft_box_fft.mrc <Reference (FFT)>" >> LOGS/${scriptname}.results
#
#
#########################################################################
${proc_2dx}/linblock "MASKTRAN - Masked FFT of boxed reference"
#########################################################################
set maskb = ${maskb01}
set rmax = 11000
#
setenv IN  ${frames_dir}/reference_flt_upscale_fft_mask_fft_box_fft.mrc
setenv OUT ${frames_dir}/reference_fft.mrc
\rm -f     ${frames_dir}/reference_fft.mrc
setenv SPOTS ${nonmaskimagename}.spt
${bin_2dx}/2dx_masktrana.exe << eot
1 F T F ! ISHAPE=1(CIRC),2(GAUSCIR),3(RECT)HOLE,IAMPLIMIT(T or F),ISPOT,IFIL
${maskb} ! RADIUS OF HOLE IF CIRCULAR, X,Y HALF-EDGE-LENGTHS IF RECT.
${lattice},-50,50,-50,50,${rmax},1 ! A/BX/Y,IH/IKMN/MX,RMAX,ITYPE
eot
#
echo  "# IMAGE: ${frames_dir}/reference_fft.mrc <Reference, Fourier-filtered (${maskb}px) (FFT)>" >> LOGS/${scriptname}.results
#
cat ${nonmaskimagename}.spt
#
if ( ${tempkeep} != "y" ) then
  \rm -f SCRATCH/reference_flt_upscale_fft_mask_fft_box_fft.mrc
endif
#
#
#
#
#
echo "<<@progress: 20>>"
#
#
#
#
#
##########################################################################
# Now preparing frames:
##########################################################################
#
#
###############################################################
${proc_2dx}/linblock "Splitting Stack into ${movie_imagenumber_touse} frames, skipping ${movie_imagenumber_toskip}"
############################################################### 
#
${app_python} ${proc_2dx}/movie/movie_mode_split3.py ${movie_stackname} ${nonmaskimagename} ${frames_dir} ${movie_imagenumber_toskip} ${nx} ${ny} ${movie_imagenumber_touse}
#
#
#
#
###############################################################
${proc_2dx}/linblock "Pre-processing all frames"
###############################################################
#
set prog_num = 25
echo "<<@progress: ${prog_num}>>"       
#
# i counts the super-frames to process:
set i = 1
while ($i <= ${movie_imagenumber_touse})
  #
  # The following line was for testing purposes:
  # cp ${nonmaskimagename}_raw.mrc ${frames_dir}/f${i}.mrc
  #
  ${proc_2dx}/lin "Adapting size and limiting resolution for frame ${i}"
  set new_mrc_created = y
  set loc_imagename = ${frames_dir}/f${i}
  source ${proc_2dx}/2dx_initialize_make_image_square_sub.com
  source ${proc_2dx}/2dx_initialize_crop_histogram_sub.com
  #
  if ( ${show_frames} == "y" ) then
    echo "# IMAGE: ${frames_dir}/f${i}.mrc <Frame ${i}>" >> LOGS/${scriptname}.results
  endif
  #
  if (${ctfcor_imode}x == "0x" || ${ctfcor_imode}x == "4x" || ${ctfcor_imode}x == "5x" || ${ctfcor_imode}x == "6x" ) then
    ${proc_2dx}/linblock "Not applying any CTF correction before unbending."
    set olddir = $PWD
    cd ${frames_dir}
    \ln -s f${i}.mrc f${i}_ctfcor.mrc
    cd olddir
  else
    #  
    set TLTAXIS_local = ${DEFOCUS_TLTAXIS}
    set TLTANG_local = ${DEFOCUS_TLTANG}
    echo ":Using DEFOCUS TLTAXIS of ${TLTAXIS_local}"
    echo ":Using DEFOCUS TLTANG  of ${TLTANG_local}"    
        
    #################################################################################
    ${proc_2dx}/linblock "2dx_ctfcor - CTF correcting frame ${i}"
    #################################################################################  
    #
    \rm -f ${frames_dir}/f${i}_ctfcor.mrc
    #
    ${bin_2dx}/2dx_ctfcor_stripes.exe << eot
${frames_dir}/f${i}.mrc
${frames_dir}/f${i}_ctfcor.mrc
#
${TLTAXIS},${TLTANG}
${CS},${KV},${phacon},${magnification},${stepdigitizer}
${defocus}
${RESMAX}
${ctfcor_noise}
${ctfcor_imode}
${ctfcor_debug}
eot
    #
  endif
  #
  ###############################################################
  ${proc_2dx}/linblock "cross_correlate.py - Cross-correlate reference with frame ${i}"
  ############################################################### 
  #
  ${app_python} ${proc_2dx}/movie/cross_correlate.py ${frames_dir}/f${i}_ctfcor.mrc ${frames_dir}/reference_fft.mrc ${frames_dir}/CCmap_${i}.mrc
  if ( ${show_frame_CCmap} == "y" ) then
    echo  "# IMAGE: ${frames_dir}/CCmap_${i}.mrc <Frame ${i}, CCmap>" >> LOGS/${scriptname}.results
  endif
  #
  set prog_num = `echo ${i} ${movie_imagenumber_touse} | awk '{ s = 25 + int( 75 * $1 / $2 ) } END { print s }'` 
  echo "<<@progress: ${prog_num}>>"       
  #
  #
  @ i += 1
end
#
