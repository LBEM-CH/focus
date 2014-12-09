#
#
# This is not an independent script.  This should rather be called from some other script.
#
#
#
  echo ":: "
  echo ":: Input file ${CTF_infile}"
  echo ":: Output file ${CTF_outfile}"
  echo ":: "

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
#
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

