#
#  2dx_unbendSyn_sub.com
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
if ( ${scriptname} == '2dx_unbendSyn' ) then
  set lincommand = "linblock"
  #################################################################################
  ${proc_2dx}/linblock "Synthetic Unbend: ${imagename}"
  #################################################################################
  #
else
  set lincommand = "lin"
endif
#
${proc_2dx}/${lincommand} "2dx_unbendSyn_sub.com: Starting."
#
echo do3quadserch = $do3quadserch
#
if ( $do3quadserch == 'y' )then
  ${proc_2dx}/lin "="
  ${proc_2dx}/lin "Doing also third quadserch with IPASS=3 option."
  ${proc_2dx}/lin "="
  echo SYN_maskb = ${SYN_maskb}
  echo "SYN_Bfact2 = ${SYN_Bfact2}"
endif
#
echo stepdigitizer = ${stepdigitizer}
echo defocus = ${defocus}
echo realcell = $realcell
echo lattice = ${lattice}
echo ALAT = $ALAT
echo imagesidelength = ${imagesidelength}
#
set rmax = 11000
echo rmax = ${rmax}
#
echo tempkeep = ${tempkeep}
#
echo TLTAXIS = ${TLTAXIS}
echo TLTANG  = ${TLTANG}
echo TLTAXA  = ${TLTAXA}
echo TANGL   = ${TANGL}
echo TAXA    = ${TAXA}
#
echo SYN_quadrada = ${SYN_quadrada}
echo SYN_quadradb = ${SYN_quadradb}
echo SYN_quadpreda = ${SYN_quadpreda}
echo SYN_quadpredb = ${SYN_quadpredb}
echo radlim = ${radlim}
echo CS = ${CS}
echo KV = ${KV}
echo magnification = ${magnification}
echo phacon = ${phacon}
echo RESMIN = ${RESMIN}
echo RESMAX = ${RESMAX}
#
${proc_2dx}/linblock "This can be done better: SYN_ref_RESMIN/RESMAX definition."
set SYN_RefRESMIN = ${RESMIN}
set SYN_RefRESMAX = ${RESMAX}
#
echo SYN_RefRESMIN = ${SYN_RefRESMIN}
echo SYN_RefRESMAX = ${SYN_RefRESMAX}
#
echo ":: "
# Test if reference is existing:
set mergedat = "../merge/merge3Dref.mtz"
if ( ! -e ${mergedat} ) then
  # set mergedat = "../../merge/merge3Dref_p1_maketran.mtz"
  set mergedat = "../../merge/merge3Dref.mtz"
  if ( ! -e ${mergedat} ) then
    set mergedat = "../merge/merge2Dref.mtz"
    if ( ! -e ${mergedat} ) then
      set mergedat = "../../merge/merge2Dref.mtz"
      if ( ! -e ${mergedat} ) then
        ${proc_2dx}/linblock "ERROR. No merged reference dataset found."
        ${proc_2dx}/linblock "ERROR. Neither ../merge/merge3Dref.mtz nor ../../merge/merge3Dref.mtz exist."
        ${proc_2dx}/protest "ERROR. Neither ../merge/merge2Dref.mtz nor ../../merge/merge2Dref.mtz exist."
      endif
    endif
  endif
endif
#
echo "::        Using reference datafile ${mergedat}"
echo ":: "
#
echo SYM = ${SYM}
echo realang = $realang
#
set reciangle = `echo ${realang} | awk '{s = 180.0 - $1 } END { print s } '`
echo reciangle = ${reciangle}
#
source ${proc_2dx}/2dx_sym2spcgrp_sub.com
echo spcgrp = ${spcgrp}
set spcgrp_maketran = ${spcgrp}
# set spcgrp_maketran = 1
echo spcgrp_maketran = ${spcgrp_maketran}
echo " "
#
# oxoy should be the negative of the phase-origin as determined for ORIGTILT :
#
echo phaoriFouFilter = ${phaoriFouFilter}
set phaoriFouFilterX = `echo ${phaoriFouFilter} | cut -d\, -f1`
set phaoriFouFilterY = `echo ${phaoriFouFilter} | cut -d\, -f2`
set tox = `echo ${phaoriFouFilterX} | awk '{ s = - $1 } END { print s }'`
set toy = `echo ${phaoriFouFilterY} | awk '{ s = - $1 } END { print s }'`
set oxoy = `echo $tox $toy`
echo oxoy = ${oxoy}
#
echo revhk = ${revhk}
echo rot180 = ${rot180}
echo rot90 = ${rot90}
echo beamtilt = ${beamtilt}
#
set rtempx1 = ${imagecenterx}
set rtempy1 = ${imagecentery}
set rtempx2 = ${imagecenterx}
set rtempy2 = ${imagecentery}
@ rtempx1 -= 400
@ rtempx2 += 399
@ rtempy1 -= 400
@ rtempy2 += 399
# this gives a box at the reference locations with a diameter of 800 pixels.
set boxlabel = ${rtempx1},${rtempx2},${rtempy1},${rtempy2}
echo boxlabel = ${boxlabel}
#
set rtempx1 = ${imagecenterx}
set rtempy1 = ${imagecentery}
set rtempx2 = ${imagecenterx}
set rtempy2 = ${imagecentery}
@ rtempx1 -= 13
@ rtempx2 += 12
@ rtempy1 -= 13
@ rtempy2 += 12
# this gives a box at the reference location with a diameter of 26 pixels.
set patlabel = ${rtempx1},${rtempx2},${rtempy1},${rtempy2}
echo patlabel = ${patlabel}
#
#
#         THRESH -------- THRESHOLD OF CROSS-CORRELATION PEAK HEIGHT,
#                          CALCULATED AS;
#                    DENMAX (READ FROM CCORDATA) * FACTOR (READ FROM UNIT 5),
#                        , BELOW WHICH THE PEAK IS NOT USED.
#
echo SYN_facthresha = ${SYN_facthresha}
#
source ${proc_2dx}/2dx_makedirs
#
/bin/rm -f ${imagename}_int.mrc
#
#############################################################################
${proc_2dx}/linblock "MAKETRAN - create synthetical transform from MTZ data"
#    OX and OY should be negative of values in normal JOBB to make central  #
#    deviation zero.                                                        #
#############################################################################
#
\rm -f SCRATCH/${imagename}_syn_1_fft.mrc
\rm -f SCRATCH/${imagename}_syn_2_fft.mrc
setenv HKLIN $mergedat
echo "HKLIN =" ${mergedat}
setenv SPOTSOUT make${imagename}.spt
#
echo " "
${proc_2dx}/lin "Protein is dark in the images before CTF correction."
#
set locdef = ${defocus}
set locfactor = '1.0'
if ( ${istilt} == "y" ) then
  #
  set locdef = '0.0,0.0,0.0'
  #
  ${proc_2dx}/lin "="
  echo "maketran has to use a defocus of 0, it will then produce white"
  echo "structures (black for ctfrev=y). Therefore the factor has to be -1 to have again dark proteins (white for ctfrev=y)."
  #
  echo "However: ttmask will invert the contrast during the TTF correction, resulting in"
  echo "wrongly white protein."
  echo "Since we still want to correlate the reference with the ttmask filtered image,"
  echo "we also need a white protein reference."
  echo "Therefore, the factor has to be +1.0 again."
  #  
  set locfactor = `echo ${locfactor} | awk '{ s = - $1 } END { print s }'`
  echo locfactor = $locfactor
  ${proc_2dx}/lin "-"
  #
endif
#
set locfactor = `echo ${locfactor} | awk '{ s = 0.00001 * $1 } END { print s }'`
#
echo " "
echo locdef = ${locdef}
echo locfactor = ${locfactor}
echo " "
#
echo " "
${proc_2dx}/lin "-"
${proc_2dx}/lin "parameter for maketran"
${proc_2dx}/lin "-"
echo " "
echo "0 2 F $SYN_maska                                  ! NPROG,ISHAPE (1circ,2gauss,3square),IAMPLIM,RAD"
echo "$imagesidelength $imagesidelength $stepdigitizer $magnification                      ! NX NY DSTEP XMAG"
echo "$lattice $revhkval ${sgnxchval} $rot180val $rot90val ${revhndval}  ${ctfrevval} ${phacon} ! AX,AY,BX,BY,REVHK,SGNXCH,ROT180,ROT90,REVHND,CTFREV,PHACON"
echo "${oxoy} ${beamtilt} $TAXA $TANGL $realcell ${realang}   ! OX OY TX TY TAXA TANGL A B GAMMA"
echo "$SYN_RefRESMIN $SYN_RefRESMAX                                ! RESMIN RESMAX, resolution limits (Angstroms)"
echo "${locdef} ${CS} ${KV}           ! DFMID1 DFMID2 ANGAST CS KV"
echo "SCRATCH/${imagename}_syn_1_fft.mrc"
echo "${mergedat}                        ! projection used for reference"
echo "${spcgrp_maketran} T ${locfactor} $SYN_Bfact1                              ! spacegroup,LFPZERO,scale+temp_factor"
echo "LABIN AMP=F PHASE=PHI FOM=FOM "
echo " "
${proc_2dx}/lin "-"
echo " "
#
${proc_2dx}/lin "-"
${proc_2dx}/lin "maketrana.exe"
${proc_2dx}/lin "-"
echo " "
#
${bin_2dx}/2dx_maketrana.exe << eot
0 2 F $SYN_maska     ! NPROG,ISHAPE (1circ,2gauss,3square),IAMPLIM,RAD
$imagesidelength $imagesidelength $stepdigitizer $magnification    ! NX NY DSTEP XMAG
$lattice $revhkval ${sgnxchval} $rot180val $rot90val ${revhndval} ${ctfrevval} ${phacon}  ! AX,AY,BX,BY,REVHK,SGNXCH,ROT180,ROT90,REVHND,CTFREV,PHACON
${oxoy} ${beamtilt} $TAXA $TANGL $realcell ${realang}                ! OX OY TX TY TAXA TANGL A B GAMMA
$SYN_RefRESMIN $SYN_RefRESMAX                                      ! RESMIN RESMAX, resolution limits (Angstroms)
${locdef} ${CS} ${KV}		                                   ! DFMID1 DFMID2 ANGAST CS KV
SCRATCH/${imagename}_syn_1_fft.mrc
${mergedat}
${spcgrp_maketran} T ${locfactor} $SYN_Bfact1    
LABIN AMP=F PHASE=PHI FOM=FOM 
eot
#
if ( ! -e SCRATCH/${imagename}_syn_1_fft.mrc ) then
  ${proc_2dx}/linblock "#"
  ${proc_2dx}/linblock "ERROR: SCRATCH/${imagename}_syn_1_fft.mrc not created."
  ${proc_2dx}/linblock "Problem in 2dx_maketrana.exe"
  ${proc_2dx}/linblock "#"
  ${proc_2dx}/linblock "Are the parameter, e.g. real-space lattice, the same in the merged dataset as here?"
  ${proc_2dx}/protest "Aborting."
endif
#
echo "# IMAGE: SCRATCH/${imagename}_syn_1_fft.mrc <FFT: First synthetic reference>" >> LOGS/${scriptname}.results
#
if ( $do3quadserch == 'y' )then
  ${proc_2dx}/linblock "MAKETRAN - another time for second reference"
  #
  ${bin_2dx}/2dx_maketrana.exe << eot
0 2 F $SYN_maskb     ! NPROG,ISHAPE (1circ,2gauss,3square),IAMPLIM,RAD
$imagesidelength $imagesidelength $stepdigitizer $magnification    ! NX NY DSTEP XMAG
$lattice $revhkval ${sgnxchval} $rot180val $rot90val ${revhndval} ${ctfrevval} ${phacon}  ! AX,AY,BX,BY,REVHK,SGNXCH,ROT180,ROT90,REVHND,CTFREV,PHACON
$oxoy ${beamtilt} $TAXA $TANGL $realcell ${realang}                ! OX OY TX TY TAXA TANGL A B GAMMA
$SYN_RefRESMIN $SYN_RefRESMAX                                      ! RESMIN RESMAX, resolution limits (Angstroms)
${locdef} ${CS} ${KV}		                                   ! DFMID1 DFMID2 ANGAST CS KV
SCRATCH/${imagename}_syn_2_fft.mrc
${mergedat}
${spcgrp_maketran} T ${locfactor} $SYN_Bfact2    
LABIN AMP=F PHASE=PHI FOM=FOM 
eot
  #
  if ( ! -e SCRATCH/${imagename}_syn_2_fft.mrc ) then
    ${proc_2dx}/linblock "#"
    ${proc_2dx}/linblock "ERROR: SCRATCH/${imagename}_syn_2_fft.mrc not created."
    ${proc_2dx}/linblock "Problem in 2dx_maketrana.exe"
    ${proc_2dx}/linblock "#"
    ${proc_2dx}/linblock "Are the parameter, e.g. real-space lattice, the same in the merged dataset as here?"
    ${proc_2dx}/protest "Aborting."
  endif
  #
  echo "# IMAGE: SCRATCH/${imagename}_syn_2_fft.mrc <FFT: Second synthetic reference>" >> LOGS/${scriptname}.results
  #
endif
#
echo "<<@progress: 15>>"
#
#############################################################################
${proc_2dx}/linblock "FFTRANS - Fourier-Transform the created fft"
#############################################################################
#
\rm -f SCRATCH/${imagename}_syn_1_flt.mrc
#
setenv IN  SCRATCH/${imagename}_syn_1_fft.mrc
setenv OUT SCRATCH/${imagename}_syn_1_flt.mrc
${bin_2dx}/2dx_fftrans.exe
#
echo "# IMAGE: SCRATCH/${imagename}_syn_1_flt.mrc <Image of first synthetic reference>" >> LOGS/${scriptname}.results
#
if ( $do3quadserch == 'y' ) then
  \rm -f SCRATCH/${imagename}_syn_2_flt.mrc
  setenv IN  SCRATCH/${imagename}_syn_2_fft.mrc
  setenv OUT SCRATCH/${imagename}_syn_2_flt.mrc
  ${bin_2dx}/2dx_fftrans.exe
  #
  echo "# IMAGE: SCRATCH/${imagename}_syn_2_flt.mrc <Image of second synthetic reference>" >> LOGS/${scriptname}.results
  #
endif
#
echo "<<@progress: 20>>"
#
#############################################################################
${proc_2dx}/linblock "LABEL - to cut out a larger area from the centre of created reference"
#  just for debugging.                                                      #                                                                           #
#############################################################################
#
echo boxlabel = ${boxlabel}
#
\rm -f SCRATCH/${imagename}_1_ref.mrc
#
${proc_2dx}/lin "labelh.exe"
${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}_syn_1_flt.mrc
1
SCRATCH/${imagename}_1_ref.mrc
${boxlabel}
eot
#
echo "# IMAGE-IMPORTANT: SCRATCH/${imagename}_1_ref.mrc <Center of first reference image>" >> LOGS/${scriptname}.results
#
if ( $do3quadserch == 'y' ) then
  \rm -f SCRATCH/${imagename}_2_ref.mrc
  #
  ${proc_2dx}/lin "labelh.exe"
  ${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}_syn_2_flt.mrc
1
SCRATCH/${imagename}_2_ref.mrc
${boxlabel}
eot
  #
  echo "# IMAGE-IMPORTANT: SCRATCH/${imagename}_2_ref.mrc <Center of second reference image>" >> LOGS/${scriptname}.results
  #
endif
#
echo "<<@progress: 25>>"
#
#############################################################################
${proc_2dx}/linblock "LABEL - to cut out a small box in the centre of the created reference"
#############################################################################
#
echo patlabel = ${patlabel}
#
\rm -f SCRATCH/${imagename}_1_flt_box.mrc
#
${proc_2dx}/lin "labelh.exe"
${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}_syn_1_flt.mrc
1
SCRATCH/${imagename}_1_flt_box.mrc
${patlabel}
eot
#
if ( ${tempkeep} == 'n' ) then
  \rm -f SCRATCH/${imagename}_syn_1_flt.mrc
endif
#
if ( $do3quadserch == 'y' ) then
  #
  \rm -f SCRATCH/${imagename}_2_flt_box.mrc
  #
  ${proc_2dx}/lin "labelh.exe"
  ${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}_syn_2_flt.mrc
1
SCRATCH/${imagename}_2_flt_box.mrc
${patlabel}
eot
  #
  if ( ${tempkeep} == 'n' ) then
    \rm -f SCRATCH/${imagename}_syn_2_flt.mrc
  endif
  #
endif
#
echo "<<@progress: 30>>"
#
#############################################################################
${proc_2dx}/linblock "AUTOCORRL - to calculate the autocorrelation of the boxed reference."
#               This 25x25 box is 20 times magnification., => 500x500              #
#############################################################################
#
\rm -f SCRATCH/${imagename}_1_flt_box_auto.mrc
#
${proc_2dx}/lin "autocorrl.exe"
setenv IN  SCRATCH/${imagename}_1_flt_box.mrc
setenv OUT SCRATCH/${imagename}_1_flt_box_auto.mrc
${bin_2dx}/autocorrl.exe << eot
20
eot
#
if ( $do3quadserch == 'y' ) then
  #
  \rm -f SCRATCH/${imagename}_2_flt_box_auto.mrc
  #
  ${proc_2dx}/lin "autocorrl.exe"
  setenv IN  SCRATCH/${imagename}_2_flt_box.mrc
  setenv OUT SCRATCH/${imagename}_2_flt_box_auto.mrc
  ${bin_2dx}/autocorrl.exe << eot
20
eot
  #
endif
#
echo "<<@progress: 35>>"
#
#############################################################################
${proc_2dx}/linblock "LABEL - to cut out a central region from the autocorrelation map"
#           500x500 => 100x100                                              #
#############################################################################
#
\rm -f SCRATCH/${imagename}_1_flt_box_auto_box.mrc
#
${proc_2dx}/lin "labelh.exe"
${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}_1_flt_box_auto.mrc
1
SCRATCH/${imagename}_1_flt_box_auto_box.mrc
210,310,210,310
eot
#
if ( $do3quadserch == 'y' ) then
  #
  \rm -f SCRATCH/${imagename}_2_flt_box_auto_box.mrc
  #
  ${proc_2dx}/lin "labelh.exe"
  ${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}_2_flt_box_auto.mrc
1
SCRATCH/${imagename}_2_flt_box_auto_box.mrc
210,310,210,310
eot
  #
endif
#
echo "<<@progress: 40>>"
#
#############################################################################
${proc_2dx}/linblock "MASKTRAN - a for the filtered TEM image."
#############################################################################
#
if ( ${istilt} == "n" ) then
  #
  if ( ! -e FFTIR/${imagename}_fft.mrc ) then
    ${proc_2dx}/protest "First calculate FFTs"
  endif
  setenv IN  FFTIR/${imagename}_fft.mrc
  if ( -e GOODSPOT.spt ) then
    setenv SPOTS GOODSPOT.spt
  else
    setenv SPOTS ${nonmaskimagename}.spt
  endif
  setenv OUT SCRATCH/${imagename}_fft_msk.mrc
  #
  \rm -f SCRATCH/${imagename}_fft_msk.mrc
  #
  ${proc_2dx}/lin "2dx_masktrana.exe"
  ${bin_2dx}/2dx_masktrana.exe << eot
1 T T F	                    ! ISHAPE=1(CIRC),2(GAUSCIR),3(RECT)HOLE,IAMPLIMIT(T or F),ISPOT,IFIL
${SYN_maska}                ! RADIUS OF HOLE IF CIRCULAR, X,Y HALF-EDGE-LENGTHS IF RECT.
${lattice},-30,30,-30,30,${rmax},1 !A/BX/Y,IH/IKMN/MX,RMAX,ITYPE
eot
  #
else
  #
  \rm -f TMP234439.dat
  #
  cp FFTIR/${imagename}_fft.mrc SCRATCH/${imagename}_fft_msk.mrc
  #
  setenv INOUT SCRATCH/${imagename}_fft_msk.mrc
  #
  ${proc_2dx}/lin "="
  ${proc_2dx}/lin "parameter for ttmask.exe"
  ${proc_2dx}/lin "="
  #
  echo "${imagesidelength} ${imagesidelength} ${stepdigitizer} ${magnification} ${CS} ${KV}"
  echo "${defocus} ${TLTAXIS} ${TLTANG}       ! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL"
  echo "1                                     ! ISHAPE= 1(circ),2(gauss circ),3(rect)"
  echo "${SYN_maska} ${SYN_maska}             ! radius hole if circular, X,Y half-edge-len if rect"
  echo "${lattice} -30 30 -30 30 ${rmax} 1 0 ! A/BX/Y,IH/IKMN/MX,RMAX,ITYPE,NUMSPOT"
  echo "TMP234439.dat"
  #
  if ( -e GOODSPOT.spt ) then
    ${proc_2dx}/lin "ttmask.exe"
    ${bin_2dx}/2dx_ttmask.exe << eot-ttmask
${imagesidelength} ${imagesidelength} ${stepdigitizer} ${magnification} ${CS} ${KV}
${defocus} ${TLTAXIS} ${TLTANG}       ! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
1                                     ! ISHAPE= 1(circ),2(gauss circ),3(rect)
${SYN_maska} ${SYN_maska}             ! radius hole if circular, X,Y half-edge-len if rect
${lattice} -30 30 -30 30 ${rmax} 1 0 ! A/BX/Y,IH/IKMN/MX,RMAX,ITYPE,NUMSPOT
TMP234439.dat
`cat GOODSPOT.spt`
eot-ttmask
  else
    ${proc_2dx}/lin "ttmask.exe"
    ${bin_2dx}/2dx_ttmask.exe << eot-ttmask
${imagesidelength} ${imagesidelength} ${stepdigitizer} ${magnification} ${CS} ${KV}
${defocus} ${TLTAXIS} ${TLTANG}       ! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
1                                     ! ISHAPE= 1(circ),2(gauss circ),3(rect)
${SYN_maska} ${SYN_maska} 	      ! radius hole if circular, X,Y half-edge-len if rect
${lattice} -30 30 -30 30 ${rmax} 0 0 ! A/BX/Y,IH/IKMN/MX,RMAX,ITYPE,NUMSPOT
TMP234439.dat
`cat ${nonmaskimagename}.spt`
eot-ttmask
    #
  endif
  #
  if ( ! -e TMP234439.dat ) then
    ${proc_2dx}/protest "allref:unbend:ttmask: ERROR occured."
  endif
  #
endif
#
# echo "# IMAGE: SCRATCH/${imagename}_fft_msk.mrc <Fourier-filtered image before twofile>" >> LOGS/${scriptname}.results
echo "<<@progress: 45>>"
#
#############################################################################
${proc_2dx}/linblock "FFTTRANS - to calculate filtered image from masked FFT"
#              just for debugging.                                          #
#############################################################################
#
if ( $tempkeep == 'y' ) then
  \rm -f SCRATCH/${imagename}_fft_msk_fft.mrc
  ${proc_2dx}/lin "fftrans.exe"
  setenv IN  SCRATCH/${imagename}_fft_msk.mrc
  setenv OUT SCRATCH/${imagename}_fft_msk_fft.mrc
  ${bin_2dx}/2dx_fftrans.exe
  #
  echo "# IMAGE: SCRATCH/${imagename}_fft_msk_fft.mrc <Fourier-filtered image before unbending>" >> LOGS/${scriptname}.results
endif
#
echo "<<@progress: 50>>"
#
#############################################################################
${proc_2dx}/linblock "LABEL - to cut out a larger area from the centre of the masked image"
#   just for debugging.                                                     #
#############################################################################
#
if ( $tempkeep == 'y' ) then
  #
  echo boxlabel = ${boxlabel}
  \rm -f SCRATCH/${imagename}.masked.mrc
  #
  ${proc_2dx}/lin "labelh.exe"
  #
  ${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}_fft_msk_fft.mrc
1
SCRATCH/${imagename}.masked.mrc
${boxlabel}
eot
  #
  echo "# IMAGE: SCRATCH/${imagename}.masked.mrc <Center of Fourier-filtered image before unbending>" >> LOGS/${scriptname}.results
endif
#
echo "<<@progress: 53>>"
#
#############################################################################
${proc_2dx}/linblock "TWOFILE - to calculate cross-correlation"
#############################################################################
#
#  Multiply two files together    :    FILE1 * Complex Conjugate of FILE2
#
\rm -f FFTIR/${imagename}_CCmap1_fft.mrc
#
setenv IN1 SCRATCH/${imagename}_fft_msk.mrc
setenv IN2 SCRATCH/${imagename}_syn_1_fft.mrc
setenv OUT FFTIR/${imagename}_CCmap1_fft.mrc
${proc_2dx}/lin "twofile.exe"
${bin_2dx}/twofile.exe << eot
2				! ICOMB = 2
2 0 0 ${imagecenterx} ${imagecentery} ! IORIGIN,OXA,OYA,OXB,OYB  Origin shifts to FFT's.
eot
#
if ( $do3quadserch == 'y' )then
  \rm -f FFTIR/${imagename}_CCmap2_fft.mrc
  #
  setenv IN1 SCRATCH/${imagename}_fft_msk.mrc
  setenv IN2 SCRATCH/${imagename}_syn_2_fft.mrc
  setenv OUT FFTIR/${imagename}_CCmap2_fft.mrc
  ${proc_2dx}/lin "twofile.exe"
  ${bin_2dx}/twofile.exe << eot
2				! ICOMB = 2
2 0 0 ${imagecenterx} ${imagecentery} ! IORIGIN,OXA,OYA,OXB,OYB  Origin shifts to FFT's.
eot
  #
endif
#
if ( ${tempkeep} == 'y' ) then
  \mv -f SCRATCH/${imagename}_fft_msk.mrc SCRATCH/${imagename}.msk.unbend.mrc
  echo "# IMAGE: SCRATCH/${imagename}.msk.unbend.mrc <FFT: Spot-Masked FFT of image>" >> LOGS/${scriptname}.results
else
  \rm -f SCRATCH/${imagename}_fft_msk.mrc
endif
#
echo "<<@progress: 57>>"
#
#############################################################################
${proc_2dx}/linblock "FFTTRANS - to calculate cross-correlation map"
#############################################################################
#
\rm -f SCRATCH/${imagename}_CCmap1.mrc
${proc_2dx}/lin "fftrans.exe"
setenv IN  FFTIR/${imagename}_CCmap1_fft.mrc
setenv OUT SCRATCH/${imagename}_CCmap1.mrc
${bin_2dx}/2dx_fftrans.exe
#
/bin/rm -f FFTIR/${imagename}_CCmap1_fft.mrc
#
if ( $do3quadserch == 'y' )then
  #
  \rm -f SCRATCH/${imagename}_CCmap2.mrc
  ${proc_2dx}/lin "fftrans.exe"
  setenv IN  FFTIR/${imagename}_CCmap2_fft.mrc
  setenv OUT SCRATCH/${imagename}_CCmap2.mrc
  ${bin_2dx}/2dx_fftrans.exe
  #
  \rm -f FFTIR/${imagename}_CCmap2_fft.mrc
endif
#
echo "<<@progress: 60>>"
#
#############################################################################
${proc_2dx}/linblock "QUADSERCH - to search cross-correlation map for peaks, IPASS=1"
#############################################################################
#
setenv PROFILE  SCRATCH/${imagename}_1_flt_box_auto_box.mrc
setenv PROFDATA SCRATCH/prof${imagename}.dat
setenv ERRORS   SCRATCH/errors${imagename}.dat
setenv ERROUT   SCRATCH/errout${imagename}.dat
#
\rm -f CCPLOT.PS
\rm -f SPIDERCOORD.spi
#
${proc_2dx}/lin "quadserchh.exe, IPASS=1"
${bin_2dx}/2dx_quadserchk-2.exe << eot
1,${SYN_quadpreda}                    ! IPASS,NRANGE
SCRATCH/${imagename}_CCmap1.mrc
$imagesidelength,$imagesidelength     ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F                          ! Lattice vectors
-200,200,-200,200                     ! NUMBER UNIT CELLS TO SEARCH
${SYN_quadrada},${SYN_quadrada}       ! RADIUS OF CORR SEARCH
${refposix} ${refposiy}               ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N                                     ! YES/NO FOR DETAILED PRINTOUT
${radlim}                             ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}                             ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
0                                     ! dont create manual Masking information
0                                     ! Dont mask the image directly
eot
#
\mv -f CCPLOT.PS PS/${imagename}-quadserchSa.ps
echo "# IMAGE: PS/${imagename}-quadserchSa.ps <PS: Vector Plot of Distortion, Reference 1, Pass 1>"  >> LOGS/${scriptname}.results 
#
\rm -f CCPLOT.PS
\rm -f SPIDERCOORD.spi
#
#
#############################################################################
${proc_2dx}/linblock "QUADSERCH - to search cross-correlation map for peaks, IPASS=3"
#############################################################################
#
setenv ERRORS   SCRATCH/errors${imagename}.dat
setenv ERROUT   SCRATCH/errout${imagename}.dat
\rm -f SCRATCH/errout${imagename}.dat
\rm -f SCRATCH/prof${imagename}.dat
#
${proc_2dx}/lin "quadserchh.exe, IPASS=3"
#
${bin_2dx}/2dx_quadserchk-2.exe << eot
3,${SYN_quadpreda}			! IPASS,NRANGE
SCRATCH/${imagename}_CCmap1.mrc
${imagesidelength},${imagesidelength}                     ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F			      ! Lattice vectors
-200,200,-200,200	      	      ! NUMBER UNIT CELLS TO SEARCH
${SYN_quadrada},${SYN_quadrada}       ! RADIUS OF CORR SEARCH
${refposix} ${refposiy} 	      ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N				      ! YES/NO FOR DETAILED PRINTOUT
${radlim}         		      ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}                   ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
0                                     ! dont create manual Masking information
0                                     ! Dont mask the image directly
eot
# 
if ( ${tempkeep} == 'y' ) then
  \mv -f SCRATCH/${imagename}_CCmap1.mrc SCRATCH/${imagename}_syn_CCmap1.mrc
  echo "# IMAGE: SCRATCH/${imagename}_syn_CCmap1.mrc <CC-Map between Reference 1 and Image>" >> LOGS/${scriptname}.results
else
  \rm -f SCRATCH/${imagename}_CCmap1.mrc
endif
#
\mv -f CCPLOT.PS PS/${imagename}-quadserchSb.ps
echo "# IMAGE: PS/${imagename}-quadserchSb.ps <PS: Vector Plot of Distortion, Reference 1, Pass 2>" >> LOGS/${scriptname}.results 
#
\mv -f SPIDERCOORD.spi ${imagename}-unitcells-spider.doc
echo "# IMAGE: ${imagename}-unitcells-spider.doc <SPIDER document with unit cell locations>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 65>>"
#
if ( ${do3quadserch} == 'y' ) then
  #
  #############################################################################
  ${proc_2dx}/linblock "QUADSERCH - to search cross-correlation map for peaks, IPASS=2"
  ${proc_2dx}/lin "with IPASS=2 to read in better ERROUT field and find cc-peaks this should find lattice within SpotScan spots."
  #############################################################################
  #
  \rm -f SCRATCH/prof${imagename}.dat
  #
  setenv PROFILE  SCRATCH/${imagename}_2_flt_box_auto_box.mrc
  setenv PROFDATA SCRATCH/prof${imagename}.dat
  setenv ERRORS   SCRATCH/errout${imagename}.dat
  #
  \rm -f CCPLOT.PS
  \rm -f SPIDERCOORD.spi
  #
  ${proc_2dx}/lin "quadserchh.exe, IPASS=2"
  ${bin_2dx}/2dx_quadserchk-2.exe << eot
2,${SYN_quadpredb}                     ! IPASS,NRANGE
SCRATCH/${imagename}_CCmap2.mrc
${imagesidelength},${imagesidelength}     ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F                       ! Lattice vectors
-200,200,-200,200               ! NUMBER UNIT CELLS TO SEARCH
${SYN_quadradb},${SYN_quadradb}           ! RADIUS OF CORR SEARCH
${refposix} ${refposiy}           ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N                               ! YES/NO FOR DETAILED PRINTOUT
${radlim}                       ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}             ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
0                               ! dont create manual Masking information
0                               ! Dont mask the image directly
eot
  #
  \mv -f CCPLOT.PS PS/${imagename}-quadserchSc.ps
  echo "# IMAGE: PS/${imagename}-quadserchSc.ps <PS: Vector Plot of Distortion, Reference 2, Pass 2>" >> LOGS/${scriptname}.results 
  #
  \mv -f SPIDERCOORD.spi ${imagename}-unitcells-spider.doc
  echo "# IMAGE: ${imagename}-unitcells-spider.doc <SPIDER document with unit cell locations>" >> LOGS/${scriptname}.results
  #
  if ( ${tempkeep} == 'y' ) then
    \mv -f SCRATCH/${imagename}_CCmap2.mrc SCRATCH/${imagename}_syn_CCmap2.mrc
    echo "# IMAGE: SCRATCH/${imagename}_syn_CCmap2.mrc <CC-Map between Reference 2 and Image>" >> LOGS/${scriptname}.results
  else
    \rm -f SCRATCH/${imagename}_CCmap2.mrc
  endif
  #
endif
#
#############################################################################
${proc_2dx}/linblock "CCUNBEND - to unbend the original image"
#############################################################################
#
\rm -f SCRATCH/${imagename}_syn_unbent.mrc
setenv CCORDATA SCRATCH/prof${imagename}.dat
\rm -f SCRATCH/ccunbend-table-${imagename}.dat
setenv TABLEOUT SCRATCH/ccunbend-table-${imagename}.dat
\rm -f fort.17
#
# Hopefully, ITYPE=1 does not crash in NAGLIB ???
# set ITYPE = 1
set ITYPE = 0
#
\rm -f CCPLOT.PS
#
${proc_2dx}/lin "ccunbendk.exe"
${bin_2dx}/2dx_ccunbendk.exe << eot
${imagename}.mrc
${ITYPE},1,${IMAXCOR},${ISTEP},F,40,T 	 !ITYPE,IOUT,IMAXCOR,ISTEP,LTAPER,RTAPER 
30,52,0.001,${SYN_facthresha},${TLTAXIS},${RMAG},${LCOLOR} !IKX,IKY,EPS,FACTOR,TLTAXIS,RMAG,LCOLOR
${imagename}, synthetical unbend, ${date}
SCRATCH/${imagename}_syn_unbent.mrc
UNBENT with synthetical reference, ${date}
eot
#
\mv -f fort.17 SCRATCH/ccunbend-histo-${imagename}.dat
#
\mv -f CCPLOT.PS PS/${imagename}-ccunbend.ps
echo "# IMAGE-IMPORTANT: PS/${imagename}-ccunbend.ps <PS: Vector Plot for Unbending>" >> LOGS/${scriptname}.results 
#
echo "# IMAGE: SCRATCH/${imagename}_syn_unbent.mrc <Unbent image>" >> LOGS/${scriptname}.results
#
#############################################################################
${proc_2dx}/${lincommand} "gzip - to compact profile to save space on the harddrive"
#############################################################################
#
\cp -f SCRATCH/prof${imagename}.dat ${imagename}-profile.dat
\rm -f ${imagename}-profile.dat.gz
#
gzip ${imagename}-profile.dat
#
echo "<<@progress: 70>>"
#
#############################################################################
${proc_2dx}/linblock "taperedge - to smoothen edges before FFT."
#############################################################################
#
\rm -f SCRATCH/${imagename}_syn_unbent_taper.mrc
setenv IN  SCRATCH/${imagename}_syn_unbent.mrc
setenv OUT SCRATCH/${imagename}_syn_unbent_taper.mrc
${proc_2dx}/lin "2dx_taperedgek.exe"
${bin_2dx}/2dx_taperedgek.exe  << 'eot'
30,30,100,30       ! IAVER,ISMOOTH,ITAPER
'eot'
#
#
echo "<<@progress: 72>>"
#
#############################################################################
${proc_2dx}/linblock "FFTRANS - to calculate FFT from image after unbending"
#############################################################################
#
\rm -f SCRATCH/${imagename}_syn_unbent_taper_fft.mrc
${proc_2dx}/lin "fftrans.exe"
setenv IN  SCRATCH/${imagename}_syn_unbent_taper.mrc
setenv OUT SCRATCH/${imagename}_syn_unbent_taper_fft.mrc
${bin_2dx}/2dx_fftrans.exe
#
echo "# IMAGE-IMPORTANT: SCRATCH/${imagename}_syn_unbent_taper_fft.mrc <FFT of Unbent and Edge-Tapered Image>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 74>>"
#
#############################################################################
#      MMBOX - resolution limitation for diagnostics                        #
#  TTBOX - to read out amplitudes and phases after TTcorrection             #
#############################################################################
#
\rm -f SCRATCH/TMP9871.dat 
\rm -f SCRATCH/TMP98711.dat 
\rm -f SCRATCH/TMP98712.dat 
\rm -f SCRATCH/TMP98713.dat 
\rm -f SCRATCH/TMP98714.dat 
\rm -f SCRATCH/TMP98715.dat 
\rm -f SCRATCH/TMP98716.dat
#
# Produce a set of resolution ranges and measure stuff in those ranges:
#
set res1 = 200.0
set res2 = 9.7
set res3 = 6.9
set res4 = 5.6
set res5 = 4.9
set res6 = 4.3
set res7 = 2.0
#
if ( ${istilt} == "n" ) then
  #
  \rm -f APH/${imagename}_tmp.aph
  #
  ${proc_2dx}/linblock "MMBOX - resolution limitation for diagnostics"
  ${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/${imagename}_syn_unbent_taper_fft.mrc
  ${imagenumber}     ${imagename} ${date}, PASS 1
Y				! Use grid units?
Y				! Generate grid from lattice?
N				! Generate points from lattice?
2 2 0 50 50 19 19		! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_tmp.aph
SCRATCH/TMP98711.dat
US
${refposix},${refposiy}           ! XORIG,YORIG
${res1},${res2},1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}		 		! Lattice vectors
eot
  #
  \rm -f APH/${imagename}_tmp.aph
  #
  ${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/${imagename}_syn_unbent_taper_fft.mrc
  ${imagenumber}     ${imagename} ${date}, PASS 1
Y				! Use grid units?
Y				! Generate grid from lattice?
N				! Generate points from lattice?
2 2 0 50 50 19 19		! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_tmp.aph
SCRATCH/TMP98712.dat
US
${refposix},${refposiy}           ! XORIG,YORIG
${res2},${res3},1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}		 		! Lattice vectors
eot
  #
  \rm -f APH/${imagename}_tmp.aph
  #
  ${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/${imagename}_syn_unbent_taper_fft.mrc
  ${imagenumber}     ${imagename} ${date}, PASS 1
Y				! Use grid units?
Y				! Generate grid from lattice?
N				! Generate points from lattice?
2 2 0 50 50 19 19		! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_tmp.aph
SCRATCH/TMP98713.dat
US
${refposix},${refposiy}           ! XORIG,YORIG
${res3} ${res4} 1 ${realcell} ${ALAT} ${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}		 		! Lattice vectors
eot
  #
  \rm -f APH/${imagename}_tmp.aph
  #
  ${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/${imagename}_syn_unbent_taper_fft.mrc
  ${imagenumber}     ${imagename} ${date}, PASS 1
Y				! Use grid units?
Y				! Generate grid from lattice?
N				! Generate points from lattice?
2 2 0 50 50 19 19		! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_tmp.aph
SCRATCH/TMP98714.dat
US
${refposix},${refposiy}           ! XORIG,YORIG
${res4} ${res5} 1 ${realcell} ${ALAT} ${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}		 		! Lattice vectors
eot
  #
  \rm -f APH/${imagename}_tmp.aph
  #
  ${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/${imagename}_syn_unbent_taper_fft.mrc
  ${imagenumber}     ${imagename} ${date}, PASS 1
Y				! Use grid units?
Y				! Generate grid from lattice?
N				! Generate points from lattice?
2 2 0 50 50 19 19		! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_tmp.aph
SCRATCH/TMP98715.dat
US
${refposix},${refposiy}           ! XORIG,YORIG
${res5} ${res6} 1 ${realcell} ${ALAT} ${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}		 		! Lattice vectors
eot
  #
  \rm -f APH/${imagename}_tmp.aph
  #
  ${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/${imagename}_syn_unbent_taper_fft.mrc
  ${imagenumber}     ${imagename} ${date}, PASS 1
Y				! Use grid units?
Y				! Generate grid from lattice?
N				! Generate points from lattice?
2 2 0 50 50 19 19		! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_tmp.aph
SCRATCH/TMP98716.dat
US
${refposix},${refposiy}           ! XORIG,YORIG
${res6} ${res7} 1 ${realcell} ${ALAT} ${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}		 		! Lattice vectors
eot
  #
  #####################################################################
else
  #####################################################################
  #
  \rm -f APH/${imagename}_syn_ttf_tmp.aph
  #
  ${proc_2dx}/linblock "TTBOX - to read out amplitudes and phases after TTcorrection"
  ${bin_2dx}/2dx_ttboxk.exe << eot
SCRATCH/${imagename}_syn_unbent_taper_fft.mrc
   ${imagenumber}     ${imagename} ${date}, CCUNBEND PASS 1
Y                        ! generate grid from lattice
N                        ! generate points from lattice
N                        ! list points as calculated
Y                        ! plot output
${imagesidelength} ${imagesidelength} ${stepdigitizer} ${magnification} ${CS} ${KV} ! ISIZEX,ISIZEY,DSTEP,MAG,CS,KV
${defocus} ${TLTAXIS} ${TLTANG} ! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
2 0 50 50 19 19          ! IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_syn_ttf_tmp.aph
SCRATCH/TMP98711.dat
US
${res1} ${res2} ${refposix} ${refposiy} ${reciangle} !RSMN,RSMX,XORIG,YORIG,SEGMNT
${lattice}                  ! reciprocal lattice vectors in pixels
eot
  \rm -f APH/${imagename}_syn_ttf_tmp.aph
  #
  ${proc_2dx}/lin "2dx_ttboxk.exe"
  ${bin_2dx}/2dx_ttboxk.exe << eot
SCRATCH/${imagename}_syn_unbent_taper_fft.mrc
   ${imagenumber}     ${imagename} ${date}, CCUNBEND PASS 1
Y                        ! generate grid from lattice
N                        ! generate points from lattice
N                        ! list points as calculated
Y                        ! plot output
${imagesidelength} ${imagesidelength} ${stepdigitizer} ${magnification} ${CS} ${KV} ! ISIZEX,ISIZEY,DSTEP,MAG,CS,KV
${defocus} ${TLTAXIS} ${TLTANG} ! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
2 0 50 50 19 19          ! IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_syn_ttf_tmp.aph
SCRATCH/TMP98712.dat
US
${res2} ${res3} ${refposix} ${refposiy} ${reciangle} !RSMN,RSMX,XORIG,YORIG,SEGMNT
${lattice}                  ! reciprocal lattice vectors in pixels
eot
  \rm -f APH/${imagename}_syn_ttf_tmp.aph
  #
  ${proc_2dx}/lin "2dx_ttboxk.exe"
  ${bin_2dx}/2dx_ttboxk.exe << eot
SCRATCH/${imagename}_syn_unbent_taper_fft.mrc
   ${imagenumber}     ${imagename} ${date}, CCUNBEND PASS 1
Y                        ! generate grid from lattice
N                        ! generate points from lattice
N                        ! list points as calculated
Y                        ! plot output
${imagesidelength} ${imagesidelength} ${stepdigitizer} ${magnification} ${CS} ${KV} ! ISIZEX,ISIZEY,DSTEP,MAG,CS,KV
${defocus} ${TLTAXIS} ${TLTANG} ! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
2 0 50 50 19 19          ! IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_syn_ttf_tmp.aph
SCRATCH/TMP98713.dat
US
${res3} ${res4} ${refposix} ${refposiy} ${reciangle} !RSMN,RSMX,XORIG,YORIG,SEGMNT
${lattice}                  ! reciprocal lattice vectors in pixels
eot
  \rm -f APH/${imagename}_syn_ttf_tmp.aph
  #
  ${proc_2dx}/lin "2dx_ttboxk.exe"
  ${bin_2dx}/2dx_ttboxk.exe << eot
SCRATCH/${imagename}_syn_unbent_taper_fft.mrc
   ${imagenumber}     ${imagename} ${date}, CCUNBEND PASS 1
Y                        ! generate grid from lattice
N                        ! generate points from lattice
N                        ! list points as calculated
Y                        ! plot output
${imagesidelength} ${imagesidelength} ${stepdigitizer} ${magnification} ${CS} ${KV} ! ISIZEX,ISIZEY,DSTEP,MAG,CS,KV
${defocus} ${TLTAXIS} ${TLTANG} ! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
2 0 50 50 19 19          ! IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_syn_ttf_tmp.aph
SCRATCH/TMP98714.dat
US
${res4} ${res5} ${refposix} ${refposiy} ${reciangle} !RSMN,RSMX,XORIG,YORIG,SEGMNT
${lattice}                  ! reciprocal lattice vectors in pixels
eot
  \rm -f APH/${imagename}_syn_ttf_tmp.aph
  #
  ${proc_2dx}/lin "2dx_ttboxk.exe"
  ${bin_2dx}/2dx_ttboxk.exe << eot
SCRATCH/${imagename}_syn_unbent_taper_fft.mrc
   ${imagenumber}     ${imagename} ${date}, CCUNBEND PASS 1
Y                        ! generate grid from lattice
N                        ! generate points from lattice
N                        ! list points as calculated
Y                        ! plot output
${imagesidelength} ${imagesidelength} ${stepdigitizer} ${magnification} ${CS} ${KV} ! ISIZEX,ISIZEY,DSTEP,MAG,CS,KV
${defocus} ${TLTAXIS} ${TLTANG} ! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
2 0 50 50 19 19          ! IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_syn_ttf_tmp.aph
SCRATCH/TMP98715.dat
US
${res5} ${res6} ${refposix} ${refposiy} ${reciangle} !RSMN,RSMX,XORIG,YORIG,SEGMNT
${lattice}                  ! reciprocal lattice vectors in pixels
eot
  \rm -f APH/${imagename}_syn_ttf_tmp.aph
  #
  ${proc_2dx}/lin "2dx_ttboxk.exe"
  ${bin_2dx}/2dx_ttboxk.exe << eot
SCRATCH/${imagename}_syn_unbent_taper_fft.mrc
   ${imagenumber}     ${imagename} ${date}, CCUNBEND PASS 1
Y                        ! generate grid from lattice
N                        ! generate points from lattice
N                        ! list points as calculated
Y                        ! plot output
${imagesidelength} ${imagesidelength} ${stepdigitizer} ${magnification} ${CS} ${KV} ! ISIZEX,ISIZEY,DSTEP,MAG,CS,KV
${defocus} ${TLTAXIS} ${TLTANG} ! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
2 0 50 50 19 19          ! IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_syn_ttf_tmp.aph
SCRATCH/TMP98716.dat
US
${res6} ${res7} ${refposix} ${refposiy} ${reciangle} !RSMN,RSMX,XORIG,YORIG,SEGMNT
${lattice}                  ! reciprocal lattice vectors in pixels
eot
  \rm -f APH/${imagename}.syn.ttf.limit.tmp.aph
  #
  \rm -f TTPLOT.PS
  #
  #####################################################################
endif
#
echo "<<@progress: 76>>"
#
#############################################################################
#      MMBOX - no resolution limitation                                     #
#  TTBOX - to read out amplitudes and phases after TTcorrection             #
#############################################################################
#
\rm -f SCRATCH/TMP9871.dat
#
if ( ${istilt} == "n" ) then
  #
  \rm -f APH/${imagename}_syn_unbend_tmp.aph
  #
  ${proc_2dx}/linblock "MMBOX - no resolution limitation"
  ${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/${imagename}_syn_unbent_taper_fft.mrc
  ${imagenumber}     ${imagename} ${date}, PASS 1
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2 2 0 50 50 19 19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_syn_unbend_tmp.aph
SCRATCH/TMP9871.dat
US
${refposix},${refposiy}           ! XORIG,YORIG
${RESMIN} 1.5 1 ${realcell} ${ALAT} ${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot
  #
  \mv -f APH/${imagename}_syn_unbend_tmp.aph APH/${imagename}_syn_unbend.aph
  #
else
  #
  \rm -f APH/${imagename}_syn_ttf_unbend_tmp.aph
  #
  \rm -f TTPLOT.PS
  #
  ${proc_2dx}/linblock "TTBOX - to read out amplitudes and phases after TTcorrection"
  ${bin_2dx}/2dx_ttboxk.exe << eot
SCRATCH/${imagename}_syn_unbent_taper_fft.mrc
   ${imagenumber}     ${imagename} ${date}, CCUNBEND PASS 1
Y                        ! generate grid from lattice
N                        ! generate points from lattice
N                        ! list points as calculated
Y                        ! plot output
${imagesidelength} ${imagesidelength} ${stepdigitizer} ${magnification} ${CS} ${KV} ! ISIZEX,ISIZEY,DSTEP,MAG,CS,KV
${defocus} ${TLTAXIS} ${TLTANG} ! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
2 0 50 50 19 19          ! IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_syn_ttf_unbend_tmp.aph
SCRATCH/TMP9871.dat
US
${RESMIN} 1.5 ${refposix} ${refposiy} ${reciangle} !RSMN,RSMX,XORIG,YORIG,SEGMNT
${lattice}                  ! reciprocal lattice vectors in pixels
eot
  #
  \rm -f APH/${imagename}_syn_ttf_unbend.aph
  \mv -f APH/${imagename}_syn_ttf_unbend_tmp.aph APH/${imagename}_syn_ttf_unbend.aph
  echo "# IMAGE: APH/${imagename}_syn_ttf_unbend.aph <APH: Final Syn TTF APH file>" >> LOGS/${scriptname}.results 
  #
  \mv -f TTPLOT.PS PS/${imagename}_ttplot_nolimit_unbendS.ps
  echo "# IMAGE: PS/${imagename}_ttplot_nolimit_unbendS.ps <PS: IQ Plot after TTF correction, no res. limit>" >> LOGS/${scriptname}.results 
  #
endif
#
echo "# IQSTAT-RESLIM:"
cat SCRATCH/TMP9871.dat >> LOGS/${scriptname}.results 
source SCRATCH/TMP9871.dat
#
set IQS = `echo ${US_IQ1} ${US_IQ2} ${US_IQ3} ${US_IQ4} ${US_IQ5} ${US_IQ6} ${US_IQ7} ${US_IQ8} ${US_IQ9}`
#":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
${proc_2dx}/linblock "Synthetic Unbend with maska=${SYN_maska} maskb=${SYN_maskb} gives QVal= ${QVAL_local}"
${proc_2dx}/linblock "IQ stat = ${IQS}"
#":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
#
echo " " >> History.dat
echo ":Date: ${date}" >> History.dat
echo "::Unbend S: maska=${SYN_maska} maskb=${SYN_maskb}: QVal= ${QVAL_local}      IQ stat = ${IQS}" >> History.dat
#
echo "set QVAL = ${QVAL_local}" >> LOGS/${scriptname}.results
echo "set QVALS = ${QVAL_local}" >> LOGS/${scriptname}.results
echo "set US_QVAL = ${QVAL_local}" >> LOGS/${scriptname}.results
echo "set US_IQs = ${IQS}" >> LOGS/${scriptname}.results
#
echo ": "
echo ":Resolution range ${RESMIN} to ${RESMAX}:" `head -1 SCRATCH/TMP9871.dat` > SCRATCH/SYN_unbend.dat 
echo ":Resolution range ${res1} to ${res2}: " `head -1 SCRATCH/TMP98711.dat` >> SCRATCH/SYN_unbend.dat
echo ":Resolution range ${res2} to ${res3}: " `head -1 SCRATCH/TMP98712.dat` >> SCRATCH/SYN_unbend.dat
echo ":Resolution range ${res3} to ${res4}: " `head -1 SCRATCH/TMP98713.dat` >> SCRATCH/SYN_unbend.dat
echo ":Resolution range ${res4} to ${res5}: " `head -1 SCRATCH/TMP98714.dat` >> SCRATCH/SYN_unbend.dat
echo ":Resolution range ${res5} to ${res6}: " `head -1 SCRATCH/TMP98715.dat` >> SCRATCH/SYN_unbend.dat
echo ":Resolution range ${res6} to ${res7}: " `head -1 SCRATCH/TMP98716.dat` >> SCRATCH/SYN_unbend.dat
echo ": "
\rm -f SCRATCH/TMP987*.dat  
#
${proc_2dx}/lin "Unbending results"
cat SCRATCH/SYN_unbend.dat
#
echo "# IMAGE: SCRATCH/SYN_unbend.dat <TXT: Q-Values in Resolution Ranges>" >> LOGS/${scriptname}.results 
#
echo "<<@progress: 100>>"
#
#############################################################################
${proc_2dx}/linblock "${scriptname} finished correctly."
#############################################################################
#






