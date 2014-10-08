#
#  2dx_fftrans_sub.com
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
#
if ( ${scriptname} == '2dx_fftrans' ) then
  set lincommand = "linblock"
  set iname = ${imagename}
else
  if ( ${ctfcor_imode}x == "x" || ${ctfcor_imode}x == "0x" ) then
    set iname = ${imagename}
  else
    set iname = ${imagename}_ctfcor
  endif
endif
#
if ( ${iname} == ${imagename} ) then
  #################################################################################
  ${proc_2dx}/linblock "Calculate FFT from the original (possibly masked) image ${iname}"
  #################################################################################
else
  #################################################################################
  ${proc_2dx}/linblock "Calculate FFT from the CTF corrected image ${iname}"
  #################################################################################
endif
#
if ( ! -e ${iname}.mrc ) then
  ${proc_2dx}/protest "ERROR: File ${iname}.mrc missing."
endif
#
\rm -f FFTIR/${iname}.fft.mrc
\rm -f FFTIR/${iname}.int.mrc
\rm -f FFTIR/${iname}.red.mrc
\rm -f FFTIR/${iname}.pg.mrc
#
#
#############################################################################
#                                                                           #
${proc_2dx}/linblock "HISTO - to produce a histogram of the grey-values in the image"
#                                                                           #
#############################################################################
#
setenv IN ${iname}.mrc
#
${bin_2dx}/histok.exe << eot
0,${imagesidelength},0,${imagesidelength}
0
eot
#
\mv -f HISTO.PS PS/${iname}-histo.ps
#
echo "# IMAGE: "PS/${iname}-histo.ps "<Histogram>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 20>"
#
if ( ${taperBeforeFFT} == "y" ) then
  #############################################################################
  ${proc_2dx}/linblock "TAPEREDGE: To taper the edges to prevent stripes in the FFT"
  #############################################################################
  #
  \rm -f     SCRATCH/${iname}.taper.mrc
  setenv IN  ${iname}.mrc
  setenv OUT SCRATCH/${iname}.taper.mrc
  ${bin_2dx}/2dx_taperedgek.exe << eot
10,10,100,10       ! IAVER,ISMOOTH,ITAPER
eot
  #
  echo "# IMAGE: "SCRATCH/${iname}.taper.mrc "<Edge-Tapered Image>" >> LOGS/${scriptname}.results
  #
endif
echo "<<@progress: 25>>"
echo "<<@evaluate>>"
#
#############################################################################
#                                                                           #
${proc_2dx}/linblock "FFTRANS - to calculate Fourier transform of image"
#                                                                           #
#############################################################################
#
\rm -f FFTIR/${iname}.fft.mrc
if ( ${taperBeforeFFT} == "y" ) then
  setenv IN SCRATCH/${iname}.taper.mrc
else
  setenv IN ${iname}.mrc
endif 
setenv OUT FFTIR/${iname}.fft.mrc
${bin_2dx}/2dx_fftrans.exe
#
echo "# IMAGE-IMPORTANT: "FFTIR/${iname}.fft.mrc "<FFT of Image>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 30>>"
#
#
#############################################################################
#                                                                           #
${proc_2dx}/linblock "LABEL - to 2x down-interpolate original image, three times."
#                                                                           #
#############################################################################
#
if ( ${taperBeforeFFT} == "y" ) then
  #
  \rm -f FFTIR/${iname}.red.mrc
  #
  ${bin_2dx}/labelh.exe << eot
SCRATCH/${iname}.taper.mrc
4               ! average adjacent pixels
FFTIR/${iname}.red.mrc
2,2
eot
else
  #
  \rm -f FFTIR/${iname}.red.mrc
  #
  ${bin_2dx}/labelh.exe << eot
${iname}.mrc
4               ! average adjacent pixels
FFTIR/${iname}.red.mrc
2,2
eot
endif
#
\rm -f SCRATCH/${nonmaskimagename}.red2.mrc
#
${bin_2dx}/labelh.exe << eot
${nonmaskimagename}.mrc
4               ! average adjacent pixels
SCRATCH/${nonmaskimagename}.red2.mrc
2,2
eot
#
\rm -f SCRATCH/${nonmaskimagename}.red4.mrc
#
${bin_2dx}/labelh.exe << eot
SCRATCH/${nonmaskimagename}.red2.mrc
4               ! average adjacent pixels
SCRATCH/${nonmaskimagename}.red4.mrc
2,2
eot
#
\rm -f SCRATCH/${nonmaskimagename}.red8.mrc
#
${bin_2dx}/labelh.exe << eot
SCRATCH/${nonmaskimagename}.red4.mrc
4               ! average adjacent pixels
SCRATCH/${nonmaskimagename}.red8.mrc
2,2
eot
#
echo "# IMAGE: "FFTIR/${iname}.red.mrc "<Downsampled Image>" >> LOGS/${scriptname}.results
echo "# IMAGE: "SCRATCH/${nonmaskimagename}.red2.mrc "<2x2 binned nonmasked Image>" >> LOGS/${scriptname}.results
echo "# IMAGE: "SCRATCH/${nonmaskimagename}.red4.mrc "<4x4 binned nonmasked Image>" >> LOGS/${scriptname}.results
echo "# IMAGE: "SCRATCH/${nonmaskimagename}.red8.mrc "<8x8 binned nonmasked Image>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 30>>"
echo "<<@evaluate>>"
#
#
#############################################################################
#                                                                           #
${proc_2dx}/linblock "FFTRANS - to calculate reduced Fourier transform"
#                                                                           #
#############################################################################
#
\rm -f FFTIR/${iname}.red.fft.mrc
setenv IN  FFTIR/${iname}.red.mrc
setenv OUT FFTIR/${iname}.red.fft.mrc
${bin_2dx}/2dx_fftrans.exe
#
echo "# IMAGE-IMPORTANT: "FFTIR/${iname}.red.fft.mrc "<FFT of Downsampled Image>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 50>>"
#
set bigimage = `echo ${imagesidelength} | awk '{ if ( $1 > 5001 ) { s = 1 } else { s = 0 }} END { print s }'`
#
if ( ${bigimage} == '1' ) then
  #############################################################################
  #                                                                           #
  ${proc_2dx}/linblock "LABEL - to cut out an image 2048x2048 in center of original image"
  #                                                                           #
  #############################################################################
  #
  set smallsize = 2048  
  #
  set rtemp1 = ${imagesidelength}
  @ rtemp1 /= 4
  set rtemps = ${smallsize}
  @ rtemps /= 2
  @ rtemp1 -= ${rtemps}
  set rtemp2 = ${rtemp1}
  @ rtemp2 += ${smallsize}
  @ rtemp1 += 1
  set patlabel = ${rtemp1},${rtemp2},${rtemp1},${rtemp2}
  echo "Center       : Xmin,Xmax,Ymin,Ymax = ${patlabel}"
  #
  \rm -f SCRATCH/${imagename}.tmp.mrc 
  #
  ${bin_2dx}/labelh.exe << eot
${nonmaskimagename}.mrc
1
SCRATCH/${imagename}.tmp.mrc
${patlabel}
eot
  #
  echo "<<@progress: 60>>"
  echo "<<@evaluate>>"
  #
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/linblock "TAPEREDGE - is needed for spotscan images to eliminate transform stripes"
  #                                                                           #
  #############################################################################
  #
  \rm -f     SCRATCH/${imagename}.center.mrc
  setenv IN  SCRATCH/${imagename}.tmp.mrc
  setenv OUT SCRATCH/${imagename}.center.mrc
  echo "Starting taperedge"
  ${bin_2dx}/2dx_taperedgek.exe << eot
10,10,100,10       ! IAVER,ISMOOTH,ITAPER,IDIST
eot
  echo "Finished taperedge"
  #
  \rm -f SCRATCH/${imagename}.tmp.mrc
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/linblock "LABEL - to down-interpolate image 2 times."
  #                                                                           #
  #############################################################################
  #
  \rm -f CUT/${imagename}.center.red.mrc
  ${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}.center.mrc
4               ! average adjacent pixels
CUT/${imagename}.center.red.mrc
2,2
eot
  #
  echo "# IMAGE: "CUT/${imagename}.center.red.mrc "<Center of Downsampled Image>"  >> LOGS/${scriptname}.results
  #
  echo "<<@progress: 70>>"
  echo "<<@evaluate>>"
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/linblock "FFTRANS - to calculate reduced central Fourier transform"
  #                                                                           #
  #############################################################################
  #
  \rm -f     CUT/${imagename}.center.red.fft.mrc
  #
  setenv IN  CUT/${imagename}.center.red.mrc
  setenv OUT CUT/${imagename}.center.red.fft.mrc
  ${bin_2dx}/2dx_fftrans.exe
  #
  echo "# IMAGE: "CUT/${imagename}.center.red.fft.mrc "<FFT of Center of Downsampled Image>" >> LOGS/${scriptname}.results
  #
  echo "<<@progress: 80>>"
  #
else
  #############################################################################
  #                                                                           #
  ${proc_2dx}/linblock "small image, not producing center image."
  #                                                                           #
  #############################################################################
endif
#
#
if ( ${generatePeriodogram} == "y" ) then
  #############################################################################
  #                                                                           #
  ${proc_2dx}/linblock "2dx_periodogram - to calculate the periodogram of the image"
  #                                                                           #
  #############################################################################
  #
  if ( -e SCRATCH/${iname}.taper.mrc ) then
    ${bin_2dx}/2dx_periodogram.exe SCRATCH/${iname}.taper.mrc ${periodogramWindowsize} ${periodogramNumsubsteps}
  else
    ${bin_2dx}/2dx_periodogram.exe ${iname}.mrc ${periodogramWindowsize} ${periodogramNumsubsteps}
  endif
  #
  \mv -f periodogram.mrc FFTIR/${iname}.pg.mrc
  echo "IMAGE: FFTIR/${iname}.pg.mrc" "<Periodogram>" >> LOGS/${scriptname}.results
  echo "<<@progress: +5>>"
  #
endif
#
#
