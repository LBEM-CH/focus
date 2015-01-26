#
#
# This is not an independent script.  This should rather be called from some other script.
#
#
#
echo ":: "
echo ":: Input file          = ${CTF_infile}"
echo ":: Output file         = ${CTF_outfile}"
echo ":: Unbent image        = ${unbent_image}"
echo ":: Unbent image (FFT)  = ${unbent_FFT}"
echo ":: CTF correction mode = ${ctfcor_imode}"
echo ":: "

if ( ${ctfcor_imode} == "0" || ${ctfcor_imode} == "1" || ${ctfcor_imode} == "2" || ${ctfcor_imode} == "3" ) then
  ${proc_2dx}/linblock "2dx_ctfapplyk - Applying CTF correction"
  setenv IN  ${CTF_infile}
  setenv OUT ${CTF_outfile}
  \rm -f ${CTF_outfile}
  \rm -f CTFPLOT.PS
  ${bin_2dx}/2dx_ctfapplyk.exe << eot
${lattice},${imagesidelength},${stepdigitizer},${magnification} ! AX,AY,BX,BY,ISIZE,DSTEP,XMAG
${defocus},${CS},${KV},${RESPLOTMAX} ! DFMID1,DFMID2,ANGAST,CS,KV,RESMAX
${imagenumber} ${iname}, ${date}
${phacon}
${RESMIN},1.0
${ctfcor_imode}  ! Define modus of CTF correction
eot
endif



if ( ${ctfcor_imode} == "4" || ${ctfcor_imode} == "5" || ${ctfcor_imode} == "6" || ${ctfcor_imode} == "7" ) then
  #
  if ( ${ctfcor_imode} == "4" ) then
    if ( ! -e ${unbent_FFT} ) then
      ${proc_2dx}/linblock "WARNING: File not found: ${unbent_FFT}"
    else
      #
      \rm -f ${CTF_outfile}
      \rm -f SCRATCH/TMP9873.dat
      #
      ${proc_2dx}/linblock "TTBOXA - to read out AMPs and PHASES with TTF-correction"
      ${bin_2dx}/2dx_ttboxk.exe << eot
${unbent_FFT}
${imagenumber} CTFcor_Mode=${ctfcor_imode} (TTFcor), ${date}
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
200.0,3.0,${refposix},${refposiy},90.0 !RSMN,RSMX,XORIG,YORIG,SEGMNT
${lattice}                  ! reciprocal lattice vectors in pixels
eot
      #
      echo "# IMAGE-IMPORTANT: ${CTF_outfile} <APH File after TTF correction [H,K,A,P,IQ,Back,0])>" >> LOGS/${scriptname}.results   
      #
      \mv -f TTPLOT.PS PS/${algo}_${iname}_ttplot_unbend2.ps
      echo "# IMAGE-IMPORTANT: PS/${algo}_${iname}_ttplot_unbend2.ps <PS: IQ Plot ${algo} after TTF correction>" >> LOGS/${scriptname}.results 
    endif
    #
  else
    #
    if ( ! -e ${unbent_image}.mrc ) then
      ${proc_2dx}/linblock "WARNING: File not found: ${unbent_image}.mrc"
    else
      #
      echo "# IMAGE-IMPORTANT: ${unbent_image}.mrc <Unbent Image>" >> LOGS/${scriptname}.results   
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
${ctfcor_imode}
${ctfcor_debug}
${ctfcor_maxamp_factor}
eot
      #
      echo "# IMAGE-IMPORTANT: ${ctfcor_outfile} <Output Image CTF corrected>" >> LOGS/${scriptname}.results
      if ( ${ctfcor_imode} == "2" ) then
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
${imagenumber} ${nonmaskimagename}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
${CTF_outfile}
SCRATCH/TMP9873.dat
${algo}
${refposix},${refposiy}           ! XORIG,YORIG
200.0,1.5,1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot

    endif
  endif
  #
  source SCRATCH/TMP9873.dat
  #
  if ( ${algo} == "U2" ) then
    set IQ2 = `echo ${U2_IQ1} ${U2_IQ2} ${U2_IQ3} ${U2_IQ4} ${U2_IQ5} ${U2_IQ6} ${U2_IQ7} ${U2_IQ8} ${U2_IQ9}`
    echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    echo "::QVal2= ${QVAL_local} ... IQ stat = ${IQ2}"
    echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    #
    echo " " >> History.dat
    echo ":Date: ${date}" >> History.dat
    echo "::Unbend 2: QVal= ${QVAL_local} ... IQ stat = ${IQ2}" >> History.dat
    #
    echo "set QVAL = ${QVAL_local}" >> LOGS/${scriptname}.results
    echo "set QVal2 = ${QVAL_local}" >> LOGS/${scriptname}.results
    echo "set U2_QVAL = ${QVAL_local}" >> LOGS/${scriptname}.results
    echo "set U2_IQs = ${IQ2}" >> LOGS/${scriptname}.results
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
    echo "::QValMA= ${QVAL_local} ... IQ stat = ${IQMA}"
    echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

    echo " " >> History.dat
    echo ":Date: ${date}" >> History.dat
    echo "::Unbend MovieA: QVal= ${QVAL_local} ... IQ stat = ${IQMA}" >> History.dat
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
    echo "::QValMB= ${QVAL_local} ... IQ stat = ${IQMB}"
    echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

    echo " " >> History.dat
    echo ":Date: ${date}" >> History.dat
    echo "::Unbend MovieB: QVal= ${QVAL_local} ... IQ stat = ${IQMB}" >> History.dat
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
echo "# IMAGE-IMPORTANT: PS/2dx_plotreska_canonical.ps <PS: Resolution Circle Plot from canonical lattice>" >> LOGS/${scriptname}.results
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
echo "# IMAGE-IMPORTANT: PS/2dx_plotreska_measured.ps <PS: Resolution Circle Plot from measured lattice>" >> LOGS/${scriptname}.results
#
#############################################################################
#                                                                           #
${proc_2dx}/linblock "2dx_powerhisto - to prepare a histogram of the PS spot intensity"
#                                                                           #
#############################################################################
#
\rm -f SCRATCH/POWERHISTO.txt
#
${bin_2dx}/2dx_powerhisto.exe << eot
${CTF_outfile}
SCRATCH/POWERHISTO.txt
${lattice}
0.05
eot
#
echo "# IMAGE: SCRATCH/POWERHISTO.txt <Histogram of Power in PS>" >> LOGS/${scriptname}.results
#
#

