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
##########################################################################
${proc_2dx}/linblock "TAPEREDGE - Tapering edge of summed frames"
###########################################################################

set infile = ${frames_dir}/CCUNBEND_f${i}_notap.mrc

\rm -f MA/movieA_f${i}_upscale.mrc
${bin_2dx}/labelh.exe << eot
${infile}
39
MA/movieA_f${i}_upscale.mrc
eot

setenv IN  MA/movieA_f${i}_upscale.mrc
setenv OUT ${frames_dir}/movieA_f${i}_upscale_fixed.mrc
\rm -f     ${frames_dir}/movieA_f${i}_upscale_fixed.mrc
${bin_2dx}/2dx_taperedgek.exe << eot
30,30,100,30       ! IAVER,ISMOOTH,ITAPER
eot

# echo "# IMAGE: ${frames_dir}/movieA_f${i}_upscale_fixed.mrc <Unbent frame ${i}, edge-tapered>" >> LOGS/${scriptname}.results 


###########################################################################
${proc_2dx}/linblock "FFTRANS - Producing final FFT"
###########################################################################

set outfile = MA/movieA_f${i}.aph
set outfft = MA/movieA_f${i}_fft.mrc

setenv IN ${frames_dir}/movieA_f${i}_upscale_fixed.mrc
setenv OUT ${outfft}
\rm -f     ${outfft}
${bin_2dx}/2dx_fftrans.exe

# echo "# IMAGE: MA/movieA_f${i}_fft.mrc <Unbent frame ${i}, FFT>" >> LOGS/${scriptname}.results


###########################################################################
${proc_2dx}/linblock "MMBOX - Evaluating APH values (no CTFcor)"
###########################################################################

set imagecenterx = `echo ${imagesidelength} | awk '{ s = int( $1 / 2 ) } END { print s }'`
set imagecentery = ${imagecenterx}
#
\rm -f dummy.aph
#
${bin_2dx}/2dx_mmboxa.exe << eot
${outfft}
${imagenumber} Frame ${i}, ${nonmaskimagename}, MA, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
dummy.aph
SCRATCH/TMP9873.dat
MOA
${imagecenterx},${imagecentery}           ! XORIG,YORIG
200.0,1.5,1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot

\rm -f dummy.aph

source SCRATCH/TMP9873.dat

echo ":: Frame ${i}: IQ Stats:"
echo ":: ${MOA_IQ1}, ${MOA_IQ1}, ${MOA_IQ2}, ${MOA_IQ3}, ${MOA_IQ4}, ${MOA_IQ5}, ${MOA_IQ6}, ${MOA_IQ7}, ${MOA_IQ8}, ${MOA_IQ9}, ${QVAL_local}"
echo ":: Frame ${i}: Resolution Bins:"
echo ":: ${RP_1}, ${RP_2}, ${RP_3}, ${RP_4}, ${RP_5}, ${RP_6}"

echo "Frame ${i} IQs: ${MOA_IQ1}, ${MOA_IQ2}, ${MOA_IQ3}, ${MOA_IQ4}, ${MOA_IQ5}, ${MOA_IQ6}, ${MOA_IQ7}, ${MOA_IQ8}, ${MOA_IQ9}, ${QVAL_local}" >> LOGS/MovieA_stats_IQs.txt
#


  #############################################################################
  ${proc_2dx}/linblock "Measuring signal in resolution bins"
  #############################################################################
  #
  \rm -f SCRATCH/TMP9882.dat
  \rm -f SCRATCH/dummy.aph
  set rlocmin = ${RESMIN}
  set rlocmax = ${RB_1}
  #
  ${bin_2dx}/2dx_mmboxa.exe << eot
${outfft}
${imagenumber} ${iname}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,100,100,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
SCRATCH/dummy.aph
SCRATCH/TMP9882.dat
U2
${imagecenterx},${imagecentery}         ! XORIG,YORIG
${rlocmin},${rlocmax},1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                              ! Lattice vectors
eot
  #
  cat SCRATCH/TMP9882.dat | grep PSMAX > SCRATCH/TMP9881.dat
  source SCRATCH/TMP9881.dat
  set RP_1 = ${PSMAX}
  #
  \rm -f SCRATCH/TMP9882.dat
  \rm -f SCRATCH/dummy.aph
  set rlocmin = ${rlocmax}
  set rlocmax = ${RB_2}
  #
  ${bin_2dx}/2dx_mmboxa.exe << eot
${outfft}
${imagenumber} ${iname}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,100,100,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
SCRATCH/dummy.aph
SCRATCH/TMP9882.dat
U2
${imagecenterx},${imagecentery}         ! XORIG,YORIG
${rlocmin},${rlocmax},1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                              ! Lattice vectors
eot
  #
  cat SCRATCH/TMP9882.dat | grep PSMAX > SCRATCH/TMP9881.dat
  source SCRATCH/TMP9881.dat
  set RP_2 = ${PSMAX}
  #
  \rm -f SCRATCH/TMP9882.dat
  \rm -f SCRATCH/dummy.aph
  set rlocmin = ${rlocmax}
  set rlocmax = ${RB_3}
  #
  ${bin_2dx}/2dx_mmboxa.exe << eot
${outfft}
${imagenumber} ${iname}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,100,100,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
SCRATCH/dummy.aph
SCRATCH/TMP9882.dat
U2
${imagecenterx},${imagecentery}         ! XORIG,YORIG
${rlocmin},${rlocmax},1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                              ! Lattice vectors
eot
  #
  cat SCRATCH/TMP9882.dat | grep PSMAX > SCRATCH/TMP9881.dat
  source SCRATCH/TMP9881.dat
  set RP_3 = ${PSMAX}
  #
  \rm -f SCRATCH/TMP9882.dat
  \rm -f SCRATCH/dummy.aph
  set rlocmin = ${rlocmax}
  set rlocmax = ${RB_4}
  #
  ${bin_2dx}/2dx_mmboxa.exe << eot
${outfft}
${imagenumber} ${iname}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,100,100,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
SCRATCH/dummy.aph
SCRATCH/TMP9882.dat
U2
${imagecenterx},${imagecentery}         ! XORIG,YORIG
${rlocmin},${rlocmax},1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                              ! Lattice vectors
eot
  #
  cat SCRATCH/TMP9882.dat | grep PSMAX > SCRATCH/TMP9881.dat
  source SCRATCH/TMP9881.dat
  set RP_4 = ${PSMAX}
  #
  \rm -f SCRATCH/TMP9882.dat
  \rm -f SCRATCH/dummy.aph
  set rlocmin = ${rlocmax}
  set rlocmax = ${RB_5}
  #
  ${bin_2dx}/2dx_mmboxa.exe << eot
${outfft}
${imagenumber} ${iname}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,100,100,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
SCRATCH/dummy.aph
SCRATCH/TMP9882.dat
U2
${imagecenterx},${imagecentery}         ! XORIG,YORIG
${rlocmin},${rlocmax},1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                              ! Lattice vectors
eot
  #
  cat SCRATCH/TMP9882.dat | grep PSMAX > SCRATCH/TMP9881.dat
  source SCRATCH/TMP9881.dat
  set RP_5 = ${PSMAX}
  #
  \rm -f SCRATCH/TMP9882.dat
  \rm -f SCRATCH/dummy.aph
  set rlocmin = ${rlocmax}
  set rlocmax = ${RB_6}
  #
  ${bin_2dx}/2dx_mmboxa.exe << eot
${outfft}
${imagenumber} ${iname}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,100,100,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
SCRATCH/dummy.aph
SCRATCH/TMP9882.dat
U2
${imagecenterx},${imagecentery}         ! XORIG,YORIG
${rlocmin},${rlocmax},1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                              ! Lattice vectors
eot
  #
  cat SCRATCH/TMP9882.dat | grep PSMAX > SCRATCH/TMP9881.dat
  source SCRATCH/TMP9881.dat
  set RP_6 = ${PSMAX}
  #
  echo "::  ResBin     Power "
  echo ":: ${RB_1}      ${RP_1}"
  echo ":: ${RB_2}      ${RP_2}"
  echo ":: ${RB_3}      ${RP_3}"
  echo ":: ${RB_4}      ${RP_4}"
  echo ":: ${RB_5}      ${RP_5}"
  echo ":: ${RB_6}      ${RP_6}"
  #
echo "Frame ${i} RBins: ${RP_1}, ${RP_2}, ${RP_3}, ${RP_4}, ${RP_5}, ${RP_6}" >> LOGS/MovieA_stats_ResBins.txt
#


