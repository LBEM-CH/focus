#
#
# This is not an independent script.  This should rather be called from some other script.
#
#
set local_ctfcor_imode = ${ctfcor_imode}
if ( -e ${CTF_infile}_ctfcor_imode ) then
  set local_ctfcor_imode = `cat ${CTF_infile}_ctfcor_imode`
endif
#
echo ":: "
echo ":: 2dx_applyCTF_sub.com : Working on ${algo} "
echo ":: -------------------------------------"
if ( ${local_ctfcor_imode} == "0" || ${local_ctfcor_imode} == "1" || ${local_ctfcor_imode} == "2" || ${local_ctfcor_imode} == "3" || ${local_ctfcor_imode} == "9" ) then
  if ( -e ${CTF_infile} ) then
    echo ":: Input file          = ${CTF_infile}"
  else
    echo ":: Input file          = ${CTF_infile} (not found)"
    exit
  endif
endif
echo ":: Output file         = ${CTF_outfile}"
echo ":: Unbent image        = ${unbent_image}"
echo ":: Unbent image (FFT)  = ${unbent_FFT}"
echo ":: CTF correction mode = ${local_ctfcor_imode}"
echo ":: "

set imagecenterx = `echo ${imagesidelength} | awk '{ s = int( $1 / 2 ) } END { print s }'`
set imagecentery = ${imagecenterx}
#


if ( ${local_ctfcor_imode} == "0" || ${local_ctfcor_imode} == "1" || ${local_ctfcor_imode} == "2" || ${local_ctfcor_imode} == "3" || ${local_ctfcor_imode} == "9" ) then

  ${proc_2dx}/linblock "2dx_ctfapplyk - Applying CTF correction to data"
  setenv IN  ${CTF_infile}
  setenv OUT ${CTF_outfile}
  \rm -f ${CTF_outfile}
  \rm -f CTFPLOT.PS
  ${bin_2dx}/2dx_ctfapplyk.exe << eot
${lattice},${imagesidelength},${stepdigitizer},${magnification} ! AX,AY,BX,BY,ISIZE,DSTEP,XMAG
${defocus},${CS},${KV},${RESPLOTMAX} ! DFMID1,DFMID2,ANGAST,CS,KV,RESMAX
${imagenumber} ${imagename}, CTFcor_Mode=${local_ctfcor_imode}, ${date}
${phacon}
${RESMIN},1.0
${local_ctfcor_imode}  ! Define modus of CTF correction
eot

  ${proc_2dx}/linblock "2dx_ctfapplyk - Applying CTF correction for PS plot"
  setenv IN  ${CTF_infile}
  setenv OUT dummy.tmp
  \rm -f dummy.tmp
  \rm -f CTFPLOT.PS
  ${bin_2dx}/2dx_ctfapplyk.exe << eot
${lattice},${imagesidelength},${stepdigitizer},${magnification} ! AX,AY,BX,BY,ISIZE,DSTEP,XMAG
${defocus},${CS},${KV},${RESPLOTMAX} ! DFMID1,DFMID2,ANGAST,CS,KV,RESMAX
${imagenumber} ${imagename}, CTFcor_Mode=${local_ctfcor_imode}, ${date}
${phacon}
${RESMIN},${RESMAX}
${local_ctfcor_imode}  ! Define modus of CTF correction
eot
  \rm -f dummy.tmp


endif



if ( ${local_ctfcor_imode} == "4" || ${local_ctfcor_imode} == "5" || ${local_ctfcor_imode} == "6" || ${local_ctfcor_imode} == "7" || ${local_ctfcor_imode} == "8" ) then
  #
  if ( ${local_ctfcor_imode} == "4" || ${local_ctfcor_imode} == "8" ) then
    if ( ! -e ${unbent_FFT} ) then
      ${proc_2dx}/linblock "WARNING: File not found: ${unbent_FFT}"
      exit
    else
      #
      \rm -f ${CTF_outfile}
      \rm -f SCRATCH/TMP9873.dat
      #
      ${proc_2dx}/linblock "TTBOXA - to read out AMPs and PHASES with TTF-correction"
      ${bin_2dx}/2dx_ttboxk.exe << eot
${unbent_FFT}
${imagenumber} CTFcor_Mode=${local_ctfcor_imode} (TTFcor), ${date}
Y                        ! generate grid from lattice
N                        ! generate points from lattice
N                        ! list points as calculated
Y                        ! plot output
${imagesidelength},${imagesidelength},${stepdigitizer},${magnification},${CS},${KV} ! ISIZEX,ISIZEY,DSTEP,MAG,CS,KV
${defocus},${TLTAXIS},${TLTANG} ! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
2,0,50,50,19,19          ! IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
${CTF_outfile}
SCRATCH/TMP9873.dat
${algo}
${RESMIN},1.0,${imagecenterx},${imagecentery},90.0 !RSMN,RSMX,XORIG,YORIG,SEGMNT
${lattice}                  ! reciprocal lattice vectors in pixels
eot
      #
      echo "# IMAGE: ${CTF_outfile} <APH File after TTF correction [H,K,A,P,IQ,Back,0])>" >> LOGS/${scriptname}.results   
      #
      \mv -f TTPLOT.PS PS/${algo}_${imagename}_ttplot_unbend2.ps
      echo "# IMAGE-IMPORTANT: PS/${algo}_${imagename}_ttplot_unbend2.ps <PS: IQ Plot ${algo} after TTF correction>" >> LOGS/${scriptname}.results 
    endif
    #
  else
    #
    if ( ! -e ${unbent_image}.mrc ) then
      ${proc_2dx}/linblock "WARNING: File not found: ${unbent_image}.mrc"
      exit
    else
      #
      echo "# IMAGE: ${unbent_image}.mrc <Unbent Image>" >> LOGS/${scriptname}.results   
      # 
      set ctfcor_ctffile = "SCRATCH/2dx_ctfcor_ctffile.mrc"
      \rm -f ${ctfcor_ctffile}  
      set ctfcor_outfile = "SCRATCH/image_ctf.mrc"
      \rm -f ${ctfcor_outfile}
      #
      # ${bin_2dx}/2dx_ctfcor.exe << eot
      ${bin_2dx}/2dx_ctfcor_stripes.exe << eot
${unbent_image}.mrc
${ctfcor_outfile}
${ctfcor_ctffile}
${TLTAXIS},${TLTANG}
${CS},${KV},${phacon},${magnification},${stepdigitizer}
${defocus}
${RESMAX}
${ctfcor_noise}
${local_ctfcor_imode}
${ctfcor_debug}
${ctfcor_maxamp_factor}
eot
      #
      echo "# IMAGE: ${ctfcor_outfile} <Output Image CTF corrected>" >> LOGS/${scriptname}.results
      if ( ${local_ctfcor_imode} == "2" ) then
        echo "# IMAGE: ${ctfcor_ctffile} <Summed CTF**2 file>" >> LOGS/${scriptname}.results
      else
        echo "# IMAGE: ${ctfcor_ctffile} <Summed CTF file>" >> LOGS/${scriptname}.results
      endif
      #
      ###########################################################################
      ${proc_2dx}/linblock "LABELH - Normalizing image to AVG=0, STDEV=100"
      ###########################################################################

      \rm -f SCRATCH/image_ctf_upscale.mrc
      ${bin_2dx}/labelh.exe << eot
${ctfcor_outfile}
39
SCRATCH/image_ctf_upscale.mrc
eot

      ###########################################################################
      ${proc_2dx}/linblock "TAPEREDGE - Tapering edge of summed frames"
      ###########################################################################

      setenv IN  SCRATCH/image_ctf_upscale.mrc
      setenv OUT SCRATCH/image_ctf_upscale_taper.mrc
      \rm -f     SCRATCH/image_ctf_upscale_taper.mrc
      ${bin_2dx}/2dx_taperedgek.exe << eot
30,30,100,30       ! IAVER,ISMOOTH,ITAPER
eot
      echo "# IMAGE: SCRATCH/image_ctf_upscale_taper.mrc <Unbent image, CTFcor, edge-tapered>" >> LOGS/${scriptname}.results 

      ###########################################################################
      ${proc_2dx}/linblock "FFTRANS - Producing final FFT"
      ###########################################################################

      setenv IN SCRATCH/image_ctf_upscale_taper.mrc
      setenv OUT SCRATCH/image_ctf_upscale_taper_fft.mrc
      \rm -f     SCRATCH/image_ctf_upscale_taper_fft.mrc
      ${bin_2dx}/2dx_fftrans.exe
      echo "# IMAGE: SCRATCH/image_ctf_upscale_taper_fft.mrc <Final FFT (CTF cor)>" >> LOGS/${scriptname}.results

      ###########################################################################
      ${proc_2dx}/linblock "MMBOX - Evaluating APH values"
      ###########################################################################

      \rm -f SCRATCH/TMP9873.dat
      \rm -f ${CTF_outfile}

      ${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/image_ctf_upscale_taper_fft.mrc
${imagenumber} ${imagename}, CTFcor_Mode=${local_ctfcor_imode}, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
${CTF_outfile}
SCRATCH/TMP9873.dat
${algo}
${imagecenterx},${imagecentery}           ! XORIG,YORIG
200.0,1.5,1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot

    endif
  endif
  #
  source SCRATCH/TMP9873.dat
  #
  if ( ${algo} == "U2" ) then
    echo "set QVAL = ${QVAL_local}" >> LOGS/${scriptname}.results
    echo "set QVAL2 = ${QVAL_local}" >> LOGS/${scriptname}.results
    echo "set U2_IQ1 = ${U2_IQ1}" >> LOGS/${scriptname}.results
    echo "set U2_IQ2 = ${U2_IQ2}" >> LOGS/${scriptname}.results
    echo "set U2_IQ3 = ${U2_IQ3}" >> LOGS/${scriptname}.results
    echo "set U2_IQ4 = ${U2_IQ4}" >> LOGS/${scriptname}.results
    echo "set U2_IQ5 = ${U2_IQ5}" >> LOGS/${scriptname}.results
    echo "set U2_IQ6 = ${U2_IQ6}" >> LOGS/${scriptname}.results
    echo "set U2_IQ7 = ${U2_IQ7}" >> LOGS/${scriptname}.results
    echo "set U2_IQ8 = ${U2_IQ8}" >> LOGS/${scriptname}.results
    echo "set U2_IQ9 = ${U2_IQ9}" >> LOGS/${scriptname}.results

    set IQ2 = `echo ${U2_IQ1} ${U2_IQ2} ${U2_IQ3} ${U2_IQ4} ${U2_IQ5} ${U2_IQ6} ${U2_IQ7} ${U2_IQ8} ${U2_IQ9}`
    echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    echo "::QVAL2= ${QVAL_local} ... IQ stat = ${IQ2}"
    echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    #
    echo " " >> History.dat
    echo ":Date: ${date}" >> History.dat
    echo "::Unbend U2: QVAL= ${QVAL_local} ... IQ stat = ${IQ2}" >> History.dat
  endif
  
  if ( ${algo} == "UMA" ) then
    source SCRATCH/TMP9873.dat

    ###########################################################################
    ${proc_2dx}/linblock "Generate IQ-stat output"
    ###########################################################################

    echo "set QVAL = ${QVAL_local}" >> LOGS/${scriptname}.results
    echo "set QVALMA = ${QVAL_local}" >> LOGS/${scriptname}.results

    echo "set UMA_IQ1 = ${UMA_IQ1}" >> LOGS/${scriptname}.results
    echo "set UMA_IQ2 = ${UMA_IQ2}" >> LOGS/${scriptname}.results
    echo "set UMA_IQ3 = ${UMA_IQ3}" >> LOGS/${scriptname}.results
    echo "set UMA_IQ4 = ${UMA_IQ4}" >> LOGS/${scriptname}.results
    echo "set UMA_IQ5 = ${UMA_IQ5}" >> LOGS/${scriptname}.results
    echo "set UMA_IQ6 = ${UMA_IQ6}" >> LOGS/${scriptname}.results
    echo "set UMA_IQ7 = ${UMA_IQ7}" >> LOGS/${scriptname}.results
    echo "set UMA_IQ8 = ${UMA_IQ8}" >> LOGS/${scriptname}.results
    echo "set UMA_IQ9 = ${UMA_IQ9}" >> LOGS/${scriptname}.results

    set IQMA = `echo ${UMA_IQ1} ${UMA_IQ2} ${UMA_IQ3} ${UMA_IQ4} ${UMA_IQ5} ${UMA_IQ6} ${UMA_IQ7} ${UMA_IQ8} ${UMA_IQ9}`
    echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    echo "::QVALMA= ${QVAL_local} ... IQ stat = ${IQMA}"
    echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

    echo " " >> History.dat
    echo ":Date: ${date}" >> History.dat
    echo "::Unbend MA: QVAL= ${QVAL_local} ... IQ stat = ${IQMA}" >> History.dat
    #
  endif
  
  if ( ${algo} == "UMB" ) then
    source SCRATCH/TMP9873.dat

    ###########################################################################
    ${proc_2dx}/linblock "Generate IQ-stat output"
    ###########################################################################

    echo "set QVAL = ${QVAL_local}" >> LOGS/${scriptname}.results
    echo "set QVALMB = ${QVAL_local}" >> LOGS/${scriptname}.results

    echo "set UMB_IQ1 = ${UMB_IQ1}" >> LOGS/${scriptname}.results
    echo "set UMB_IQ2 = ${UMB_IQ2}" >> LOGS/${scriptname}.results
    echo "set UMB_IQ3 = ${UMB_IQ3}" >> LOGS/${scriptname}.results
    echo "set UMB_IQ4 = ${UMB_IQ4}" >> LOGS/${scriptname}.results
    echo "set UMB_IQ5 = ${UMB_IQ5}" >> LOGS/${scriptname}.results
    echo "set UMB_IQ6 = ${UMB_IQ6}" >> LOGS/${scriptname}.results
    echo "set UMB_IQ7 = ${UMB_IQ7}" >> LOGS/${scriptname}.results
    echo "set UMB_IQ8 = ${UMB_IQ8}" >> LOGS/${scriptname}.results
    echo "set UMB_IQ9 = ${UMB_IQ9}" >> LOGS/${scriptname}.results

    set IQMB = `echo ${UMB_IQ1} ${UMB_IQ2} ${UMB_IQ3} ${UMB_IQ4} ${UMB_IQ5} ${UMB_IQ6} ${UMB_IQ7} ${UMB_IQ8} ${UMB_IQ9}`
    echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    echo "::QVALMB= ${QVAL_local} ... IQ stat = ${IQMB}"
    echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

    echo " " >> History.dat
    echo ":Date: ${date}" >> History.dat
    echo "::Unbend MB: QVAL= ${QVAL_local} ... IQ stat = ${IQMB}" >> History.dat
    #    
  endif
  
  #
  \rm -f SCRATCH/TMP9873.dat  
endif

#############################################################################
${proc_2dx}/linblock "2dx_plotreska - to plot the powerspectrum with resolution circles"
${proc_2dx}/linblock "Using plotreska, contributed by Anchi Cheng."
#############################################################################  
#
\rm -f PLOTRES.PS
#
# plot ellipses in canonical HK space
set plotres_ellipse = "1"
#
# Plot as (tilted) section in 3D Fourier space
# 
${bin_2dx}/2dx_plotreska.exe << eot
${TAXA}, ${TANGL}
1 	! Show as tilted projections, based on real-space lattice
${realcell},${realang},${lattice}
${CTF_outfile}
1	! Include IQ Value label
${plotres_ellipse}
${RESMAX}
${plotres_rings}
eot
#
if ( ! -e PLOTRES.PS ) then
  ${proc_2dx}/protest "ERROR: Problem in 2dx_plotreska."
endif
\mv -f PLOTRES.PS PS/2dx_plotreska_canonical.ps
echo "# IMAGE: PS/2dx_plotreska_canonical.ps <PS: Resolution Circle Plot from canonical lattice>" >> LOGS/${scriptname}.results
#
# Plot as non-tilted projection 
# 
${bin_2dx}/2dx_plotreska.exe << eot
${TAXA}, ${TANGL}
2 	! Show as non-tilted projections, based on reciprocal lattice
${lattice},${imagesidelength},${stepdigitizer},${magnification}
${CTF_outfile}
1	! Include IQ Value label
${plotres_ellipse}
${RESMAX}
${plotres_rings}
eot
#
if ( ! -e PLOTRES.PS ) then
  ${proc_2dx}/protest "ERROR: Problem in 2dx_plotreska."
endif
\mv -f PLOTRES.PS PS/2dx_plotreska_measured.ps
echo "# IMAGE: PS/2dx_plotreska_measured.ps <PS: Resolution Circle Plot from measured lattice>" >> LOGS/${scriptname}.results
#
#############################################################################
${proc_2dx}/linblock "2dx_powerhisto - to prepare a histogram of the PS spot intensity"
#############################################################################
#
\rm -f SCRATCH/${algo}_POWERHISTO.txt
#
${bin_2dx}/2dx_powerhisto.exe << eot
${CTF_outfile}
SCRATCH/${algo}_POWERHISTO.txt
${lattice}
0.05
eot
#
#############################################################################
${proc_2dx}/linblock "Producing output links"
#############################################################################
#
echo "# IMAGE: SCRATCH/${algo}_POWERHISTO.txt <${nametag} Histogram of Power in PS>" >> LOGS/${scriptname}.results
#
echo "# IMAGE: ${CTF_infile} <${nametag} APH File before CTF correction [H,K,A,P,IQ,Back]>" >> LOGS/${scriptname}.results
echo "# IMAGE: ${CTF_outfile} <${nametag} APH File after CTF correction [H,K,A,P(CTF phase flipped),IQ,Back,CTF]>" >> LOGS/${scriptname}.results

if ( ${local_ctfcor_imode} == "0" || ${local_ctfcor_imode} == "1" || ${local_ctfcor_imode} == "2" || ${local_ctfcor_imode} == "3" ) then
  \mv -f CTFPLOT.PS PS/${algo}_${imagename}_unbend2_ctf.ps
  echo "# IMAGE-IMPORTANT: PS/${algo}_${imagename}_unbend2_ctf.ps <PS: ${nametag} IQ Plot after CTF correction>" >> LOGS/${scriptname}.results
endif

\mv -f PS/2dx_plotreska_canonical.ps PS/${algo}_2dx_plotreska_canonical.ps
echo "# IMAGE: PS/${algo}_2dx_plotreska_canonical.ps <PS: ${nametag} Resolution Circle Plot from canonical lattice>" >> LOGS/${scriptname}.results

\mv -f PS/2dx_plotreska_measured.ps PS/${algo}_2dx_plotreska_measured.ps
echo "# IMAGE-IMPORTANT: PS/${algo}_2dx_plotreska_measured.ps <PS: ${nametag} Resolution Circle Plot from measured lattice>" >> LOGS/${scriptname}.results



