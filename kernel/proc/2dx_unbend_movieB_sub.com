#
#
#
#  This is not an independent script. 
#  This should be called from another script.
#
#
#
#
#
#
if ( ${show_frames} == "y" || ${show_frame_FFT} == "y" || ${show_frame_CCmap} == "y" || ${show_frame_CCmap_marked} == "y") then
  set tempkeep = "y"
endif
#
\rm -f MovieB_unbending.pdf
\touch MovieB_unbending.pdf
#
set iname = image_ctfcor
#
set imagecenterx = `echo ${imagesidelength} | awk '{ s = int( $1 / 2 ) } END { print s }'`
set imagecentery = ${imagecenterx}
#
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
echo  "# IMAGE-IMPORTANT: ${movie_stackname} <Raw image stack>" >> LOGS/${scriptname}.results
#
if ( ${movie_enable} == "n" ) then
  ${proc_2dx}/linblock "Skipping movie mode unbending."
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
#
if ( ! -e ${movie_stackname}) then
  ${proc_2dx}/protest "ERROR: ${movie_stackname} missing. Aborting."
else
  # Get the number of frames
  e2iminfo.py -H ${movie_stackname} > tmp_stack_header.txt
  set movie_imagenumber_total = `\grep "MRC.nz:" tmp_stack_header.txt | cut -d' ' -f 2`
  ${proc_2dx}/linblock "Stack contains ${movie_imagenumber_total} frames"
  echo "set movie_imagenumber_total = ${movie_imagenumber_total}"  >> LOGS/${scriptname}.results
  set movie_imagenumber_touse = `echo ${movie_imagenumber_total} ${movie_imagenumber_toskip} | awk '{s = int($1-$2) } END { print s }'`
  echo "set movie_imagenumber_touse = ${movie_imagenumber_touse}"  >> LOGS/${scriptname}.results
  set movie_imagenumber_superframes = `echo ${movie_imagenumber_touse} ${movie_imagenumber_toave} | awk '{s = int($1/$2) } END { print s }'`
  echo "set movie_imagenumber_superframes = ${movie_imagenumber_superframes}"  >> LOGS/${scriptname}.results
  \rm tmp_stack_header.txt
endif
#
#${app_python} ${proc_2dx}/movie/extractAMP.py `ls ${frame_folder}/aph_*`
#${app_python} ${proc_2dx}/movie/plotAMP.py ${frame_folder}/AMPs.txt ${frame_folder}/AMPs.pdf ${movie_imagenumber_toave}
#exit 
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
endif
if ( ${movie_filter_type} == "1" ) then
  set filt_a = `echo ${movie_filter_param} | sed 's/,/ /g' | awk '{ s = $1 } END { print s }'`
  set filt_b = `echo ${movie_filter_param} | sed 's/,/ /g' | awk '{ s = $2 } END { print s }'`
endif
if ( ${movie_filter_type} == "2" ) then
  # This is also taken care of in getFilter.py
  set filt_a = 1.0
  set filt_b = 0.0
endif
#
set num_dia = 100
#
if ( 1 == 2 ) then
  ${proc_2dx}/linblock "Plotting local drift"
  ${app_python} ${proc_2dx}/movie/plotLocalDrift.py ${frame_folder} PS/MovieB_drifts.pdf ${num_dia}
  exit
endif
#
if ( 1 == 2 ) then
  set i = 0
  while ($i <= ${movie_imagenumber_superframes})
    echo "# IMAGE: ${frame_folder}/CC-CE-frame_${i}.mrc <Frame ${i} CC CE>" >> LOGS/${scriptname}.results
    @ i += 1
  end
  set i = 0
  while ($i <= ${movie_imagenumber_superframes})
    echo "# IMAGE: ${frame_folder}/CC-TL-frame_${i}.mrc <Frame ${i} CC TL>" >> LOGS/${scriptname}.results
    @ i += 1
  end
  set i = 0
  while ($i <= ${movie_imagenumber_superframes})
    echo "# IMAGE: ${frame_folder}/CC-TR-frame_${i}.mrc <Frame ${i} CC TR>" >> LOGS/${scriptname}.results
    @ i += 1
  end
  set i = 0
  while ($i <= ${movie_imagenumber_superframes})
    echo "# IMAGE: ${frame_folder}/CC-BR-frame_${i}.mrc <Frame ${i} CC BR>" >> LOGS/${scriptname}.results
    @ i += 1
  end
  set i = 0
  while ($i <= ${movie_imagenumber_superframes})
    echo "# IMAGE: ${frame_folder}/CC-BL-frame_${i}.mrc <Frame ${i} CC BL>" >> LOGS/${scriptname}.results
    @ i += 1
  end
endif
#
set i = 0
while ($i <= ${movie_imagenumber_superframes})
  if ( ${show_frames} == "y" ) then
    echo "# IMAGE: ${frame_folder}/frame_${i}/${nonmaskimagename}_${i}_raw.mrc <Frame ${i} raw>" >> LOGS/${scriptname}.results
    echo "# IMAGE: ${frame_folder}/frame_${i}/${nonmaskimagename}_${i}.mrc <Frame ${i}>" >> LOGS/${scriptname}.results
    echo "# IMAGE: ${frame_folder}/frame_${i}/${iname}_mask.mrc <Frame ${i}, masked>" >> LOGS/${scriptname}.results
  endif
  if ( ${show_frame_FFT} == "y" ) then
    echo "# IMAGE: ${frame_folder}/CCUNBEND_frame_${i}_fft.mrc <Frame ${i}, unbent (fft)>" >> LOGS/${scriptname}.results
    echo "# IMAGE-IMPORTANT: ${frame_folder}/frame_${i}/${iname}_mask_fft.mrc <Frame ${i}, masked (FFT)>" >> LOGS/${scriptname}.results
  endif
  if ( ${show_frame_CCmap} == "y" ) then
    echo "# IMAGE: ${frame_folder}/frame_${i}/SCRATCH/${nonmaskimagename}_CCmapMB.mrc <Frame ${i} CCMapMB>" >> LOGS/${scriptname}.results
    echo "# IMAGE: ${frame_folder}/frame_${i}/SCRATCH/${nonmaskimagename}_CCmapMBb.mrc <Frame ${i} CCMapMBb>" >> LOGS/${scriptname}.results
    echo "# IMAGE: ${frame_folder}/frame_${i}_CCmapMBb_unbent.mrc <Frame ${i} CCMapMB unbent>" >> LOGS/${scriptname}.results
  endif
  echo "# IMAGE: ${frame_folder}/frame_${i}/SCRATCH/TMP-quadserch3-autocor.mrc <Frame ${i} averaged central area>" >> LOGS/${scriptname}.results
  echo "# IMAGE-IMPORTANT: ${frame_folder}/SCRATCH/MovieB/frame_${i}/PS/${nonmaskimagename}_quadserch.ps <Frame ${i} PS QUADSERCH Plot>" >> LOGS/${scriptname}.results 
  echo "# IMAGE-IMPORTANT: ${frame_folder}/SCRATCH/MovieB/frame_${i}/PS/${nonmaskimagename}_ccunbend.ps <Frame ${i} PS CCUNBEND Plot>" >> LOGS/${scriptname}.results
  echo "# IMAGE: ${frame_folder}/CCUNBEND_frame_${i}_notap.mrc <Frame ${i} Unbent image>" >> LOGS/${scriptname}.results 
  #
  @ i += 1
end
#
echo "<<@evaluate>>"
#
# Generate subfolder for frame images
\rm -rf ${frame_folder}
if ( ! -d ${frame_folder} ) then
  \mkdir ${frame_folder}
  #
  echo "<<@progress: 10>>"
  #
  ###############################################################
  ${proc_2dx}/linblock "Splitting Stack into ${movie_imagenumber_superframes} super-frames, merging ${movie_imagenumber_toave} movie sub-frames into each"
  ############################################################### 
  #
  ${app_python} ${proc_2dx}/movie/movie_mode_split2.py ${movie_stackname} ${nonmaskimagename} ${movie_imagenumber_toskip} ${movie_imagenumber_toave} ${frame_folder}
  #
  foreach f (`ls ${frame_folder}`)
    \cp 2dx_image.cfg ${frame_folder}/$f/
    ${app_python} ${proc_2dx}/movie/disable_movie.py  ${frame_folder}/$f/
  end
endif
#
echo "<<@progress: 15>>"
#
setenv PROFILE  SCRATCH/${iname}_fou_unbend2_fft_msk_fft_cro_aut_cro.mrc
setenv PROFDATA ${nonmaskimagename}_profile.dat
setenv ERRORS   SCRATCH/errout${iname}.dat
setenv ERROUT   SCRATCH/errout2${iname}.dat
#
set quadradbx = `echo ${quadradb} | sed 's/,/ /g' | awk '{ s = int( $1 ) } END { print s }'`
set quadradby = `echo ${quadradb} | sed 's/,/ /g' | awk '{ s = int( $2 ) } END { print s }'`
set movie_quadradax = `echo ${movie_quadrada} | sed 's/,/ /g' | awk '{ s = int( $1 ) } END { print s }'`
set movie_quadraday = `echo ${movie_quadrada} | sed 's/,/ /g' | awk '{ s = int( $2 ) } END { print s }'`
set movie_quadradbx = `echo ${movie_quadradb} | sed 's/,/ /g' | awk '{ s = int( $1 ) } END { print s }'`
set movie_quadradby = `echo ${movie_quadradb} | sed 's/,/ /g' | awk '{ s = int( $2 ) } END { print s }'`
set valspotscan = '0'
set createmask = '0'
#
\rm -f SCRATCH/errout2${iname}.dat
\rm -f SPIDERCOORD.spi
#
# 
###############################################################
${proc_2dx}/linblock "QUADSERCH - Updating Initial Error Field" 
###############################################################
#
echo "<<@progress: 20>>"
#
echo "movie_imagenumber_superframes = ${movie_imagenumber_superframes}"
#
#
${bin_2dx}/2dx_quadserchk-2.exe << eot
3,${quadpredb}                     ! IPASS,NRANGE
${cormap}
${imagesidelength},${imagesidelength}     ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F                       ! Lattice vectors
-200,200,-200,200               ! NUMBER UNIT CELLS TO SEARCH
${quadradbx},${quadradby}           ! RADIUS OF CORR SEARCH
${imagecenterx},${imagecentery}           ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N                               ! YES/NO FOR DETAILED PRINTOUT
${radlim}                       ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}          ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
${createmask}                   ! dont create manual Masking information
0                               ! Mask the image directly
eot
  #
endif
#
###############################################################
${proc_2dx}/linblock "apply_filter.py - Applying pre-processing filters"
###############################################################
#
echo "<<@progress: 22>>"
#
set image_dir = `pwd`
# i counts the super-frames to process:
set i = 0
# n indicates the original frame position in the raw input movie stack: 
set n = 1
while ($i <= ${movie_imagenumber_superframes})
  #########################################################################
  ${proc_2dx}/lin "Working on frame ${i}"
  #########################################################################

  if ( ! -e ${frame_folder}/2dx_master.cfg ) then
    set olddir = $PWD
    cd ${frame_folder}
    \ln -s ../2dx_image.cfg 2dx_master.cfg  
    cd ${olddir}
  endif
  echo "set imagename = ${nonmaskimagename}_${i}" >> ${frame_folder}/frame_${i}/2dx_image.cfg
  echo "set nonmaskimagename = ${nonmaskimagename}_${i}" >> ${frame_folder}/frame_${i}/2dx_image.cfg

  if ( -d ${image_dir}/${frame_folder} ) then
    ${app_2dx_image} ${image_dir}/${frame_folder}/frame_${i} "2dx_initialize"
    ${app_2dx_image} ${image_dir}/${frame_folder}/frame_${i} "2dx_initialize_files"
  else
    ${app_2dx_image} ${frame_folder}/frame_${i} "2dx_initialize"
    ${app_2dx_image} ${frame_folder}/frame_${i} "2dx_initialize_files"
  endif
  \rm -f ${frame_folder}/2dx_master.cfg

  set filtervalue = `${app_python} ${proc_2dx}/movie/getFilter.py ${n} ${movie_filter_type} ${filt_a} ${filt_b}`
  echo ":  Filter frame average #${i} with radius ${filtervalue}"
  # echo "LP on: ${frame_folder}/frame_${i}/${nonmaskimagename}_${i}.mrc with filter radius ${filtervalue}"
  # echo "${app_python} ${proc_2dx}/movie/getFilter.py ${n} ${movie_filter_type} ${filt_a} ${filt_b}"
  #
  # echo "${app_python} ${proc_2dx}/movie/apply_filter.py ${frame_folder}/frame_${i}/${nonmaskimagename}_${i}.mrc ${filtervalue} ${i} ${imagesidelength} ${frame_folder}/weight.mrc"
  ${app_python} ${proc_2dx}/movie/apply_filter.py ${frame_folder}/frame_${i}/${nonmaskimagename}_${i}.mrc ${filtervalue} ${i} ${imagesidelength} ${frame_folder}/weight.mrc
  #

  set ctfcor_tilefile = "${frame_folder}/frame_${i}/2dx_ctfcor_tile.mrc"
  set ctfcor_psfile = "${frame_folder}/frame_${i}/2dx_ctfcor_psfile.mrc"
  set ctfcor_ctffile = "${frame_folder}/frame_${i}/2dx_ctfcor_ctffile.mrc"
  \rm -f ${frame_folder}/frame_${i}/image_ctfcor.mrc
  \rm -f ${ctfcor_tilefile}
  \rm -f ${ctfcor_psfile}  
  \rm -f ${ctfcor_ctffile}  
  #
  #
  setenv NCPUS ${Thread_Number}
  #
  if ( ${ctfcor_imode}x == "0x" || ${ctfcor_imode}x == "4x" || ${ctfcor_imode}x == "5x" || ${ctfcor_imode}x == "6x" ) then
    ${proc_2dx}/lin "Not applying any CTF correction before unbending."
    set olddir = $PWD
    cd ${frame_folder}/frame_${i}
    \rm -f image_ctfcor.mrc
    \ln -s ${nonmaskimagename}_${i}.mrc image_ctfcor.mrc
    cd ${olddir}
  else
    if ( ${calculate_tiles} == "0" ) then
      ${bin_2dx}/2dx_ctfcor_stripes.exe << eot
${frame_folder}/frame_${i}/${nonmaskimagename}_${i}.mrc
${frame_folder}/frame_${i}/image_ctfcor.mrc
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
    else
      if ( ${calculate_tiles} == "1" ) then
        ${bin_2dx}/2dx_ctfcor_tiles.exe << eot
${frame_folder}/frame_${i}/${nonmaskimagename}_${i}.mrc
${frame_folder}/frame_${i}/image_ctfcor.mrc
#
#
#
${TLTAXIS},${TLTANG}
${CS},${KV},${phacon},${magnification},${stepdigitizer}
${defocus}
${ctfcor_noise}
${ctfcor_inner_tile}
${ctfcor_outer_tile}
${ctfcor_taper}
${ctfcor_imode}
${ctfcor_debug}
${ctfcor_maxamp_factor}
eot
        #
      else
        ${bin_2dx}/2dx_ctfcor_tiles.exe << eot
${frame_folder}/frame_${i}/${nonmaskimagename}_${i}.mrc
${frame_folder}/frame_${i}/image_ctfcor.mrc
${ctfcor_tilefile}
${ctfcor_psfile}
${ctfcor_ctffile}
${TLTAXIS},${TLTANG}
${CS},${KV},${phacon},${magnification},${stepdigitizer}
${defocus}
${ctfcor_noise}
${ctfcor_inner_tile}
${ctfcor_outer_tile}
${ctfcor_taper}
${ctfcor_imode}
${ctfcor_debug}
${ctfcor_maxamp_factor}
eot
        #
        echo "# IMAGE: ${ctfcor_tilefile} <Images of tiles>" >> LOGS/${scriptname}.results
        echo "# IMAGE: ${ctfcor_psfile} <PowerSpectra of tiles>" >> LOGS/${scriptname}.results
        #
        if ( ${ctfcor_imode} == "2" ) then
          echo "# IMAGE: ${ctfcor_ctffile} <Summed CTF**2 file>" >> LOGS/${scriptname}.results
        else
          echo "# IMAGE: ${ctfcor_ctffile} <Summed CTF file>" >> LOGS/${scriptname}.results
        endif
      endif
    endif
  endif
  echo "# IMAGE-IMPORTANT: ${frame_folder}/frame_${i}/image_ctfcor.mrc <Output Image Frame ${i} CTF corrected>" >> LOGS/${scriptname}.results

  #########################################################################
  ${proc_2dx}/lin "FFT of raw frame average, frame ${i}"
  #########################################################################
  setenv IN ${frame_folder}/frame_${i}/${iname}.mrc
  setenv OUT ${frame_folder}/frame_${i}/${iname}_fft.mrc
  ${bin_2dx}/2dx_fftrans.exe      

  #
  if ( ${i} > 0 ) then
    @ n += ${movie_imagenumber_toave}
  endif
  @ i += 1
end
#
echo "<<@progress: 25>>"
#
touch ${frame_folder}/peaks_dummy
\rm -f ${frame_folder}/peaks*
#
echo "<<@evaluate>>"
#



#
#
##########################################################################
##########################################################################
# Now preparing reference:
##########################################################################
##########################################################################
#
set maskb = ${maskb01}
#
if ( ${SYN_Unbending} == "0" ) then
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
SCRATCH/reference_flt_upscale.mrc
eot
  #
  setenv IN  SCRATCH/reference_flt_upscale.mrc
  setenv OUT SCRATCH/reference_flt_upscale_fft.mrc
  \rm -f     SCRATCH/reference_flt_upscale_fft.mrc
  ${bin_2dx}/2dx_fftrans.exe 
  #
  echo  "# IMAGE: SCRATCH/reference_flt_upscale_fft.mrc <Unbent image (FFT)>" >> LOGS/${scriptname}.results  
  #
  echo "<<@progress: 15>>"
  #
  # 
  #########################################################################
  ${proc_2dx}/linblock "MASKTRAN - Lattice-mask FFT of unbent image, small holes"
  #########################################################################
  set rmax = 11000
  #
  setenv IN  SCRATCH/reference_flt_upscale_fft.mrc
  setenv OUT SCRATCH/reference_flt_upscale_fft_mask.mrc
  \rm -f     SCRATCH/reference_flt_upscale_fft_mask.mrc
  setenv SPOTS ${nonmaskimagename}.spt
  ${bin_2dx}/2dx_masktrana.exe << eot
1 F T F ! ISHAPE=1(CIRC),2(GAUSCIR),3(RECT)HOLE,IAMPLIMIT(T or F),ISPOT,IFIL
1 ! RADIUS OF HOLE IF CIRCULAR, X,Y HALF-EDGE-LENGTHS IF RECT.
${lattice},-50,50,-50,50,${rmax},1 ! A/BX/Y,IH/IKMN/MX,RMAX,ITYPE
eot
  echo  "# IMAGE: SCRATCH/reference_flt_upscale_fft_mask.mrc <Unbent image, Fourier-filtered (1px) (FFT)>" >> LOGS/${scriptname}.results 
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
  setenv IN  SCRATCH/reference_flt_upscale_fft_mask.mrc
  setenv OUT SCRATCH/reference_flt_upscale_fft_mask_fft.mrc
  \rm -f     SCRATCH/reference_flt_upscale_fft_mask_fft.mrc
  ${bin_2dx}/2dx_fftrans.exe
  #
  echo  "# IMAGE: SCRATCH/reference_flt_upscale_fft_mask_fft.mrc <Unbent image, Fourier-filtered>" >> LOGS/${scriptname}.results
  #
  if ( ${tempkeep} != "y" ) then
    \rm -f SCRATCH/reference_flt_upscale_fft_mask.mrc
  endif
  #
  echo "<<@progress: 17>>" 
  #
  #
  ###############################################################
  ${proc_2dx}/linblock "BOXIMAGE - Boxing reference: ${movie_refboxa}"
  ###############################################################
  #
  ${app_python} ${proc_2dx}/movie/box_reference.py SCRATCH/reference_flt_upscale_fft_mask_fft.mrc SCRATCH/reference_flt_upscale_fft_mask_fft_box.mrc ${movie_refboxa} 
  #
  if ( ${tempkeep} != "y" ) then
    \rm -f SCRATCH/reference_flt_upscale_fft_mask_fft.mrc
  endif
  #
  \rm -f SCRATCH/reference_flt_upscale_fft_mask_fft_box_upscale.mrc
  ${bin_2dx}/labelh.exe << eot
SCRATCH/reference_flt_upscale_fft_mask_fft_box.mrc
39
SCRATCH/reference_flt_upscale_fft_mask_fft_box_upscale.mrc
eot
  echo  "# IMAGE-IMPORTANT: SCRATCH/reference_flt_upscale_fft_mask_fft_box_upscale.mrc <Reference (${movie_refboxa}px)>" >> LOGS/${scriptname}.results
  #
  #
  ###############################################################
  ${proc_2dx}/linblock "FFTRANS - Producing reference in Fourier space"
  ###############################################################
  #
  setenv IN  SCRATCH/reference_flt_upscale_fft_mask_fft_box_upscale.mrc
  setenv OUT SCRATCH/reference_flt_upscale_fft_mask_fft_box_fft.mrc
  \rm -f     SCRATCH/reference_flt_upscale_fft_mask_fft_box_fft.mrc
  ${bin_2dx}/2dx_fftrans.exe 
  #
  # echo  "# IMAGE: SCRATCH/reference_flt_upscale_fft_mask_fft_box_fft.mrc <Reference (FFT)>" >> LOGS/${scriptname}.results  
  #
  #
  #########################################################################
  ${proc_2dx}/linblock "MASKTRAN - Masked FFT of boxed reference"
  #########################################################################
  set rmax = 11000  
  #  
  setenv IN  SCRATCH/reference_flt_upscale_fft_mask_fft_box_fft.mrc
  setenv OUT SCRATCH/reference_fft.mrc
  \rm -f     SCRATCH/reference_fft.mrc
  setenv SPOTS ${nonmaskimagename}.spt
  ${bin_2dx}/2dx_masktrana.exe << eot
1 F T F ! ISHAPE=1(CIRC),2(GAUSCIR),3(RECT)HOLE,IAMPLIMIT(T or F),ISPOT,IFIL
${maskb} ! RADIUS OF HOLE IF CIRCULAR, X,Y HALF-EDGE-LENGTHS IF RECT.
${lattice},-50,50,-50,50,${rmax},1 ! A/BX/Y,IH/IKMN/MX,RMAX,ITYPE
eot
  echo  "# IMAGE: SCRATCH/reference.mrc <Reference, Fourier-filtered (${maskb}px) (FFT)>" >> LOGS/${scriptname}.results  
  #
  if ( ${tempkeep} != "y" ) then
    \rm -f SCRATCH/reference_flt_upscale_fft_mask_fft_box_fft.mrc
  endif
  #
  #
  #
  #
  #
else
  #
  ###############################################################
  ###############################################################
  ###############################################################
  ${proc_2dx}/linblock "Preparing reference from merged MTZ file"
  ###############################################################
  ###############################################################
  ###############################################################
  #
  set tmp1 = `echo ${SYN_maska} | awk '{s = int( $1 ) } END { print s }'`
  if ( ${tmp1} == ${SYN_maska} ) then
    echo SYN_maska = ${SYN_maska}
  else
    set SYN_maska = ${tmp1}
    echo SYN_maska = ${SYN_maska}
    echo "set SYN_maska = ${SYN_maska}" >> LOGS/${scriptname}.results
    ${proc_2dx}/linblock "Warning: SYN_maska needs to be an integer number. Now corrected." >> LOGS/${scriptname}.results
    echo "#WARNING: Warning: SYN_maska needs to be an integer number. Now corrected." >> LOGS/${scriptname}.results
  endif
  #
  set loc_SYN_mask = ${SYN_maska}
  set loc_SYN_Bfact = ${SYN_Bfact1}
  set locfactor = '1.0'
  #
  source ${proc_2dx}/2dx_make_SynRef_sub.com
  #
  echo "# IMAGE: SCRATCH/reference_fft.mrc <Synthetic Reference (FFT)>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/reference.mrc <Synthetic Reference>" >> LOGS/${scriptname}.results
  #
  #
  #
endif
#
echo "<<@progress: 20>>"
#
#
# the variable ${prog_num} controls the progress bar in the subscript 2dx_unbend_movieB_sub_sub.com:
set prog_num = 30
echo "<<@progress: ${prog_num}>>"
#
# First calculate unbending pattern for high-contrast frame_0 (0.0 to 0.25, middle is 0.125):
# i counts the super-frames to process:
set irunner = 0
set iforward = 1
set i = 0
#============================================
source ${proc_2dx}/2dx_unbend_movieB_sub_sub.com
#============================================
#
\cp -f SCRATCH/errout2${iname}.dat SCRATCH/errout_keyframe${iname}.dat
set irunner = 1

# Run from key frame downwards to 1:
set iforward = 0
set i = `echo ${movie_imagenumber_superframes} | awk '{ s = int( $1 * 0.125 ) } END { print s }'`
while ($i >= 1)
        #============================================
        source ${proc_2dx}/2dx_unbend_movieB_sub_sub.com
        #============================================
        @ i -= 1
	@ irunner += 1
end

\cp -f SCRATCH/errout_keyframe${iname}.dat SCRATCH/errout2${iname}.dat

# Run from key frame upwards to end:
set iforward = 1
set i = `echo ${movie_imagenumber_superframes} | awk '{ s = int( $1 * 0.125 ) + 1 } END { print s }'`
while ($i <= ${movie_imagenumber_superframes})
        #============================================
        source ${proc_2dx}/2dx_unbend_movieB_sub_sub.com
        #============================================
        @ i += 1
	@ irunner += 1
end

echo "<<@progress: 80>>"




###########################################################################
${proc_2dx}/linblock "Averaging unbent images from frames 1 to ${movie_imagenumber_superframes}" 
###########################################################################
${app_python} ${proc_2dx}/movie/direct_sum.py ${movie_imagenumber_superframes} ${nonmaskimagename} ${frame_folder}
echo "# IMAGE: ${frame_folder}/direct_sum.mrc <Sum unbent images>" >> LOGS/${scriptname}.results 


###########################################################################
${proc_2dx}/linblock "Filtering by resolution weights"
###########################################################################
${app_python} ${proc_2dx}/movie/apply_filter_sum.py ${frame_folder}/direct_sum.mrc ${frame_folder}/direct_sum_filt.mrc 0.033 ${frame_folder}/weight.mrc
echo "# IMAGE: ${frame_folder}/direct_sum_filt.mrc <Sum unbent images, filtered>" >> LOGS/${scriptname}.results 
echo "# IMAGE: weight.mrc <Weight function for adding frames in Fourier space>" >> LOGS/${scriptname}.results

if ( ${ctfcor_imode} == "1" || ${ctfcor_imode} == "2" ) then
  \rm -f ${frame_folder}/direct_sum_filt_ctf.mrc
  ${app_python} ${proc_2dx}/movie/apply_filter_fourier.py ${frame_folder}/direct_sum_filt.mrc SCRATCH/2dx_ctfcor_ctffile.mrc ${frame_folder}/direct_sum_filt_ctf.mrc ${ctfcor_noise}

  if ( ${ctfcor_imode} == "2" ) then
    echo "# IMAGE: SCRATCH/2dx_ctfcor_ctffile.mrc <Summed-CTF**2 (Noise=${ctfcor_noise})>" >> LOGS/${scriptname}.results 
  else
    echo "# IMAGE: SCRATCH/2dx_ctfcor_ctffile.mrc <Summed-CTF (Noise=${ctfcor_noise})>" >> LOGS/${scriptname}.results 
  endif

  echo "# IMAGE: ${frame_folder}/direct_sum_filt_ctf.mrc <Sum unbent images, filtered, CTF-corrected>" >> LOGS/${scriptname}.results 
else
  \rm -f ${frame_folder}/direct_sum_filt_ctf.mrc
  \cp -f ${frame_folder}/direct_sum_filt.mrc ${frame_folder}/direct_sum_filt_ctf.mrc
endif



###########################################################################
${proc_2dx}/linblock "LABELH - Normalizing image to AVG=0, STDEV=100"
###########################################################################

\rm -f MovieB/direct_sum_filt_upscale.mrc
${bin_2dx}/labelh.exe << eot
${frame_folder}/direct_sum_filt.mrc
39
MovieB/direct_sum_filt_upscale.mrc
eot
# echo  "# IMAGE: ${frame_folder}/direct_sum_filt.mrc <Sum unbent images, filtered>" >> LOGS/${scriptname}.results
# echo  "# IMAGE: MovieB/direct_sum_filt_upscale.mrc <Sum unbent images, filtered, upscaled>" >> LOGS/${scriptname}.results

\rm -f MovieB/direct_sum_filt_ctf_upscale.mrc
${bin_2dx}/labelh.exe << eot
${frame_folder}/direct_sum_filt_ctf.mrc
39
MovieB/direct_sum_filt_ctf_upscale.mrc
eot
# echo  "# IMAGE: ${frame_folder}/direct_sum_filt_ctf.mrc <Sum unbent images, filtered, CTFcor>" >> LOGS/${scriptname}.results
# echo  "# IMAGE: MovieB/direct_sum_filt_ctf_upscale.mrc <Sum unbent images, filtered, CTFcor, upscaled>" >> LOGS/${scriptname}.results

###########################################################################
${proc_2dx}/linblock "TAPEREDGE - Tapering edge of summed frames"
###########################################################################

setenv IN  MovieB/direct_sum_filt_upscale.mrc
setenv OUT ${frame_folder}/direct_sum_filt_taper.mrc
\rm -f     ${frame_folder}/direct_sum_filt_taper.mrc
${bin_2dx}/2dx_taperedgek.exe << eot
30,30,100,30       ! IAVER,ISMOOTH,ITAPER
eot
echo "# IMAGE: ${frame_folder}/direct_sum_filt_taper.mrc <Sum unbent images, edge-tapered>" >> LOGS/${scriptname}.results 

setenv IN  MovieB/direct_sum_filt_ctf_upscale.mrc
setenv OUT ${frame_folder}/direct_sum_filt_ctf_taper.mrc
\rm -f     ${frame_folder}/direct_sum_filt_ctf_taper.mrc
${bin_2dx}/2dx_taperedgek.exe << eot
30,30,100,30       ! IAVER,ISMOOTH,ITAPER
eot
echo "# IMAGE: ${frame_folder}/direct_sum_filt_ctf_taper.mrc <Sum unbent images, CTFcor, edge-tapered>" >> LOGS/${scriptname}.results 

###########################################################################
${proc_2dx}/linblock "FFTRANS - Producing final FFT"
###########################################################################

if ( ${SYN_Unbending}x == "0x" ) then
  set outfile = APH/${iname}_movieB_fou.aph
  echo ${ctfcor_imode} > APH/${iname}_movieB_fou.aph_ctfcor_imode
  set outfft = MovieB/direct_sum_ctf_fft.mrc
  set outfft_noctf = MovieB/direct_sum_fft.mrc
else
  set outfile = APH/${iname}_movieB_syn.aph
  echo ${ctfcor_imode} > APH/${iname}_movieB_syn.aph_ctfcor_imode
  set outfft = MovieB/direct_sum_syn_ctf_fft.mrc
  set outfft_noctf = MovieB/direct_sum_syn_fft.mrc
endif


setenv IN ${frame_folder}/direct_sum_filt_taper.mrc
setenv OUT ${outfft_noctf}
\rm -f     ${outfft_noctf}
${bin_2dx}/2dx_fftrans.exe
echo "# IMAGE: MovieB/direct_sum_fft.mrc <Final FFT>" >> LOGS/${scriptname}.results

setenv IN ${frame_folder}/direct_sum_filt_ctf_taper.mrc
setenv OUT ${outfft}
\rm -f     ${outfft}
${bin_2dx}/2dx_fftrans.exe
echo "# IMAGE: ${frame_folder}/direct_sum_ctf_fft.mrc <Final FFT (CTF cor)>" >> LOGS/${scriptname}.results


###########################################################################
${proc_2dx}/linblock "MMBOX - Evaluating APH values"
###########################################################################

\rm -f SCRATCH/TMP9873.dat
\rm -f dummy.aph

${bin_2dx}/2dx_mmboxa.exe << eot
${outfft_noctf}
${imagenumber} ${nonmaskimagename}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
dummy.aph
SCRATCH/TMP9873.dat
UMB
${imagecenterx},${imagecentery}           ! XORIG,YORIG
200.0,1.5,1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot

\rm -f dummy.aph

source SCRATCH/TMP9873.dat

###########################################################################
${proc_2dx}/linblock "Generate IQ-stat output"
###########################################################################

echo "set QVAL = ${QVAL_local}" >> LOGS/${scriptname}.results

if ( ${SYN_Unbending}x == "0x" ) then
  echo "set UMB_IQ1 = ${UMB_IQ1}" >> LOGS/${scriptname}.results
  echo "set UMB_IQ2 = ${UMB_IQ2}" >> LOGS/${scriptname}.results
  echo "set UMB_IQ3 = ${UMB_IQ3}" >> LOGS/${scriptname}.results
  echo "set UMB_IQ4 = ${UMB_IQ4}" >> LOGS/${scriptname}.results
  echo "set UMB_IQ5 = ${UMB_IQ5}" >> LOGS/${scriptname}.results
  echo "set UMB_IQ6 = ${UMB_IQ6}" >> LOGS/${scriptname}.results
  echo "set UMB_IQ7 = ${UMB_IQ7}" >> LOGS/${scriptname}.results
  echo "set UMB_IQ8 = ${UMB_IQ8}" >> LOGS/${scriptname}.results
  echo "set UMB_IQ9 = ${UMB_IQ9}" >> LOGS/${scriptname}.results
  echo "set QVALMB = ${QVAL_local}" >> LOGS/${scriptname}.results
else
  echo "set UBS_IQ1 = ${UMB_IQ1}" >> LOGS/${scriptname}.results
  echo "set UBS_IQ2 = ${UMB_IQ2}" >> LOGS/${scriptname}.results
  echo "set UBS_IQ3 = ${UMB_IQ3}" >> LOGS/${scriptname}.results
  echo "set UBS_IQ4 = ${UMB_IQ4}" >> LOGS/${scriptname}.results
  echo "set UBS_IQ5 = ${UMB_IQ5}" >> LOGS/${scriptname}.results
  echo "set UBS_IQ6 = ${UMB_IQ6}" >> LOGS/${scriptname}.results
  echo "set UBS_IQ7 = ${UMB_IQ7}" >> LOGS/${scriptname}.results
  echo "set UBS_IQ8 = ${UMB_IQ8}" >> LOGS/${scriptname}.results
  echo "set UBS_IQ9 = ${UMB_IQ9}" >> LOGS/${scriptname}.results
  echo "set QVALBS = ${QVAL_local}" >> LOGS/${scriptname}.results
endif

set RP_6 = ${PSMAX}
echo "set RP_1 = ${RP_1}" >> LOGS/${scriptname}.results
echo "set RP_2 = ${RP_2}" >> LOGS/${scriptname}.results
echo "set RP_3 = ${RP_3}" >> LOGS/${scriptname}.results
echo "set RP_4 = ${RP_4}" >> LOGS/${scriptname}.results
echo "set RP_5 = ${RP_5}" >> LOGS/${scriptname}.results
echo "set RP_6 = ${RP_6}" >> LOGS/${scriptname}.results

echo "<<@evaluate>>"

set IQS = `echo ${UMB_IQ1} ${UMB_IQ2} ${UMB_IQ3} ${UMB_IQ4} ${UMB_IQ5} ${UMB_IQ6} ${UMB_IQ7} ${UMB_IQ8} ${UMB_IQ9}`
echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
if ( ${SYN_Unbending}x == "0x" ) then
  echo "::maskb=${maskb}, movie_refboxa=${movie_refboxa}: QValMB= ${QVAL_local} ... IQ stat = ${IQS}"
else
  echo "::maskb=${maskb}, movie_refboxa=${movie_refboxa}: QValBS= ${QVAL_local} ... IQ stat = ${IQS}"
endif
echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

echo " " >> History.dat
echo ":Date: ${date}" >> History.dat
if ( ${SYN_Unbending}x == "0x" ) then
  echo "::Unbend MovieB: maskb=${maskb}, movie_refboxa=${movie_refboxa}: QVal= ${QVAL_local} ... IQ stat = ${IQS}" >> History.dat
else
  echo "::Unbend MB-Syn: maskb=${maskb}, movie_refboxa=${movie_refboxa}: QVal= ${QVAL_local} ... IQ stat = ${IQS}" >> History.dat
endif
#


###########################################################################
${proc_2dx}/linblock "MMBOX - Evaluating APH values (CTF cor)"
###########################################################################

\rm -f SCRATCH/TMP9873.dat
\rm -f ${outfile}

${bin_2dx}/2dx_mmboxa.exe << eot
${outfft}
${imagenumber} ${nonmaskimagename}, MovieB, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
${outfile}
SCRATCH/TMP9873.dat
UMB
${imagecenterx},${imagecentery}           ! XORIG,YORIG
200.0,1.5,1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot

\rm -f SCRATCH/TMP9873.dat

# Variation plot generation (no longer used)
#${app_python} ${proc_2dx}/movie/calculate_distance.py 1 ${movie_imagenumber_superframes} ${nonmaskimagename}
#pdf2ps ${frame_folder}/sd.pdf ${frame_folder}/sd.ps
#convert ${frame_folder}/sd.ps ${frame_folder}/sd.jpg
#echo "# IMAGE: ${frame_folder}/sd.ps <Variations>" >> LOGS/${scriptname}.results

echo "<<@progress: 90>>"
if ( ${movie_filter_type} == '2' ) then
        ${proc_2dx}/linblock "Plotting AMP-Decay"
        ${app_python} ${proc_2dx}/movie/extractAMP.py ${frame_folder}/AMPs.txt `ls ${frame_folder}/aph_*`
        ${app_python} ${proc_2dx}/movie/plotAMP.py ${frame_folder}/AMPs.txt ${frame_folder}/AMPs.pdf ${movie_imagenumber_toave}
        ${pdf2ps} ${frame_folder}/AMPs.pdf ${frame_folder}/AMPs.ps 
        echo  "# IMAGE: ${frame_folder}/AMPs.ps <PS: AMP Decay>" >> LOGS/${scriptname}.results
endif

if ( ${tempkeep} != "y" ) then
  ###########################################################################
  ${proc_2dx}/linblock "Deleting frame averages temporay files"
  ###########################################################################
  set olddir = $PWD
  cd ${frame_folder}
  \rm -rf frame*
  cd ${olddir}
endif

# Prevent the "${frame_folder}" directory from showing up in 2dx_merge:
\rm -f ${frame_folder}/2dx_master.cfg

${proc_2dx}/linblock "Plotting local drift"
${app_python} ${proc_2dx}/movie/plotLocalDrift.py ${frame_folder} PS/MovieB_drifts.pdf ${num_dia}

${proc_2dx}/linblock "Finalizing output"
${pdf2ps} PS/MovieB_drifts.pdf PS/MovieB_drifts.ps
echo  "# IMAGE: PS/MovieB_drifts.ps <PS: Local drifts>" >> LOGS/${scriptname}.results

\rm -f PS/MovieB_quadserch.ps
${pdf2ps} PS/MovieB_quadserch.pdf PS/MovieB_quadserch.ps
echo "# IMAGE-IMPORTANT: PS/MovieB_quadserch.ps <PS: Profiles QUADSERCH>" >> LOGS/${scriptname}.results

\rm -f PS/MovieB_unbending.ps
${pdf2ps} PS/MovieB_unbending.pdf PS/MovieB_unbending.ps
echo "# IMAGE-IMPORTANT: PS/MovieB_unbending.ps <PS: Profiles CCUNBEND>" >> LOGS/${scriptname}.results


echo "# IMAGE-IMPORTANT: ${outfile} <APH: APH file after movie-mode B unbending>" >> LOGS/${scriptname}.results


