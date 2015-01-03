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
else
 set lincommand = "lin"
endif
#
#################################################################################
${proc_2dx}/${lincommand} "Calculate FFT from ${inimage}"
#################################################################################
#
if ( ! -e ${inimage}.mrc ) then
  ${proc_2dx}/protest "ERROR: File ${inimage}.mrc missing."
endif
#
\rm -f FFTIR/${inimage}_fft.mrc
\rm -f FFTIR/${inimage}_int.mrc
\rm -f FFTIR/${inimage}_red.mrc
\rm -f FFTIR/${inimage}_pg.mrc
#
#
#############################################################################
#                                                                           #
${proc_2dx}/${lincommand} "HISTO - to produce a histogram of the grey-values in the image"
#                                                                           #
#############################################################################
#
setenv IN ${inimage}.mrc
#
${bin_2dx}/histok.exe << eot
0,${imagesidelength},0,${imagesidelength}
0
eot
#
\mv -f HISTO.PS PS/${inimage}_histo.ps
#
echo "# IMAGE: "PS/${inimage}_histo.ps "<Histogram>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 20>"
#
if ( ${taperBeforeFFT} == "y" ) then
  #############################################################################
  ${proc_2dx}/${lincommand} "TAPEREDGE: To taper the edges to prevent stripes in the FFT"
  #############################################################################
  #
  \rm -f     SCRATCH/${inimage}_taper.mrc
  setenv IN  ${inimage}.mrc
  setenv OUT SCRATCH/${inimage}_taper.mrc
  ${bin_2dx}/2dx_taperedgek.exe << eot
10,10,100,10       ! IAVER,ISMOOTH,ITAPER
eot
  #
  echo "# IMAGE: "SCRATCH/${inimage}_taper.mrc "<Edge-Tapered Image>" >> LOGS/${scriptname}.results
  #
endif
echo "<<@progress: 25>>"
echo "<<@evaluate>>"
#
#############################################################################
#                                                                           #
${proc_2dx}/${lincommand} "FFTRANS - to calculate Fourier transform of image"
#                                                                           #
#############################################################################
#
if ( ${taperBeforeFFT} == "y" ) then
  set infile = SCRATCH/${inimage}_taper.mrc
else
  set infile = ${inimage}.mrc
endif 
\rm -f SCRATCH/upscaled.mrc
${bin_2dx}/labelh.exe << eot
${infile}
39
SCRATCH/upscaled.mrc
eot
#
\rm -f FFTIR/${inimage}_fft.mrc
setenv IN SCRATCH/upscaled.mrc
setenv OUT FFTIR/${inimage}_fft.mrc
${bin_2dx}/2dx_fftrans.exe
#
echo "# IMAGE-IMPORTANT: "FFTIR/${inimage}_fft.mrc "<FFT of Image>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 30>>"
#
#
#############################################################################
#                                                                           #
${proc_2dx}/${lincommand} "LABEL - to 2x down-interpolate original image, three times."
#                                                                           #
#############################################################################
#
#
\rm -f FFTIR/${inimage}_red.mrc
#
${bin_2dx}/labelh.exe << eot
SCRATCH/upscaled.mrc
4               ! average adjacent pixels
FFTIR/${inimage}_red.mrc
2,2
eot
#
\rm -f SCRATCH/${nonmaskimagename}_red2.mrc
#
${bin_2dx}/labelh.exe << eot
${nonmaskimagename}.mrc
4               ! average adjacent pixels
SCRATCH/${nonmaskimagename}_red2.mrc
2,2
eot
#
\rm -f SCRATCH/${nonmaskimagename}_red4.mrc
#
${bin_2dx}/labelh.exe << eot
SCRATCH/${nonmaskimagename}_red2.mrc
4               ! average adjacent pixels
SCRATCH/${nonmaskimagename}_red4.mrc
2,2
eot
#
\rm -f SCRATCH/${nonmaskimagename}_red8.mrc
#
${bin_2dx}/labelh.exe << eot
SCRATCH/${nonmaskimagename}_red4.mrc
4               ! average adjacent pixels
SCRATCH/${nonmaskimagename}_red8.mrc
2,2
eot
#
echo "# IMAGE: "FFTIR/${inimage}_red.mrc "<Downsampled Image>" >> LOGS/${scriptname}.results
echo "# IMAGE: "SCRATCH/${nonmaskimagename}_red2.mrc "<2x2 binned nonmasked Image>" >> LOGS/${scriptname}.results
echo "# IMAGE: "SCRATCH/${nonmaskimagename}_red4.mrc "<4x4 binned nonmasked Image>" >> LOGS/${scriptname}.results
echo "# IMAGE: "SCRATCH/${nonmaskimagename}_red8.mrc "<8x8 binned nonmasked Image>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 30>>"
echo "<<@evaluate>>"
#
#
#############################################################################
#                                                                           #
${proc_2dx}/${lincommand} "FFTRANS - to calculate reduced Fourier transform"
#                                                                           #
#############################################################################
#
\rm -f FFTIR/${inimage}_red_fft.mrc
setenv IN  FFTIR/${inimage}_red.mrc
setenv OUT FFTIR/${inimage}_red_fft.mrc
${bin_2dx}/2dx_fftrans.exe
#
echo "# IMAGE-IMPORTANT: "FFTIR/${inimage}_red_fft.mrc "<FFT of Downsampled Image>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 50>>"
#
set bigimage = `echo ${imagesidelength} | awk '{ if ( $1 > 5001 ) { s = 1 } else { s = 0 }} END { print s }'`
#
if ( ${bigimage} == '1' ) then
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "LABEL - to cut out an image 2048x2048 in center of original image"
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
  \rm -f SCRATCH/${imagename}_tmp.mrc 
  #
  ${bin_2dx}/labelh.exe << eot
${nonmaskimagename}.mrc
1
SCRATCH/${imagename}_tmp.mrc
${patlabel}
eot
  #
  echo "<<@progress: 60>>"
  echo "<<@evaluate>>"
  #
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "TAPEREDGE - is needed for spotscan images to eliminate transform stripes"
  #                                                                           #
  #############################################################################
  #
  \rm -f     SCRATCH/${imagename}_center.mrc
  setenv IN  SCRATCH/${imagename}_tmp.mrc
  setenv OUT SCRATCH/${imagename}_center.mrc
  echo "Starting taperedge"
  ${bin_2dx}/2dx_taperedgek.exe << eot
10,10,100,10       ! IAVER,ISMOOTH,ITAPER,IDIST
eot
  echo "Finished taperedge"
  #
  \rm -f SCRATCH/${imagename}_tmp.mrc
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "LABEL - to down-interpolate image 2 times."
  #                                                                           #
  #############################################################################
  #
  \rm -f CUT/${imagename}_center_red.mrc
  ${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}_center.mrc
4               ! average adjacent pixels
CUT/${imagename}_center_red.mrc
2,2
eot
  #
  echo "# IMAGE: "CUT/${imagename}_center_red.mrc "<Center of Downsampled Image>"  >> LOGS/${scriptname}.results
  #
  echo "<<@progress: 70>>"
  echo "<<@evaluate>>"
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "FFTRANS - to calculate reduced central Fourier transform"
  #                                                                           #
  #############################################################################
  #
  \rm -f     CUT/${imagename}_center_red_fft.mrc
  #
  setenv IN  CUT/${imagename}_center_red.mrc
  setenv OUT CUT/${imagename}_center_red_fft.mrc
  ${bin_2dx}/2dx_fftrans.exe
  #
  echo "# IMAGE: "CUT/${imagename}_center_red_fft.mrc "<FFT of Center of Downsampled Image>" >> LOGS/${scriptname}.results
  #
  echo "<<@progress: 80>>"
  #
else
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "small image, not producing center image."
  #                                                                           #
  #############################################################################
endif
#
#
if ( ${generatePeriodogram} == "y" ) then
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "2dx_periodogram - to calculate the periodogram of the image"
  #                                                                           #
  #############################################################################
  #
  if ( -e SCRATCH/${inimage}_taper.mrc ) then
    ${bin_2dx}/2dx_periodogram.exe SCRATCH/${inimage}_taper.mrc ${periodogramWindowsize} ${periodogramNumsubsteps}
  else
    ${bin_2dx}/2dx_periodogram.exe ${inimage}.mrc ${periodogramWindowsize} ${periodogramNumsubsteps}
  endif
  #
  \mv -f periodogram.mrc FFTIR/${inimage}_pg.mrc
  echo "IMAGE: FFTIR/${inimage}_pg.mrc" "<Periodogram>" >> LOGS/${scriptname}.results
  echo "<<@progress: +5>>"
  #
endif
#
#
