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
set iname = image_ctfcor
#
${proc_2dx}/${lincommand} "2dx_unbendSyn_sub.com: Starting."
#

if ( ! -e FFTIR/${iname}_fft.mrc ) then
  ${proc_2dx}/protest "First calculate FFTs"
endif

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
  set imagecenter = ${imagesidelength}
  @ imagecenter /= 2
  set imagecenterx = ${imagecenter}
  set imagecentery = ${imagecenter}

#############################################################################
# create synthetical transform from MTZ data
#    OX and OY should be negative of values in normal JOBB to make central  #
#    deviation zero.                                                        #
#############################################################################
#
set tmp1 = `echo ${SYN_maska} | awk '{s = int( $1 ) } END { print s }'`
if ( ${tmp1} != ${SYN_maska} ) then
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
\mv -f SCRATCH/reference_fft.mrc SCRATCH/${iname}_syn_1_fft.mrc
echo "# IMAGE: SCRATCH/${iname}_syn_1_fft.mrc <Synthetic Reference 1 (FFT)>" >> LOGS/${scriptname}.results
#
\mv -f SCRATCH/reference.mrc SCRATCH/${iname}_syn_1_flt.mrc
echo  "# IMAGE: SCRATCH/${iname}_syn_1_flt.mrc <Synthetic Reference 1>" >> LOGS/${scriptname}.results
#
if ( $do3quadserch == 'y' )then
  #
  set tmp1 = `echo ${SYN_maskb} | awk '{s = int( $1 ) } END { print s }'`
  if ( ${tmp1} != ${SYN_maskb} ) then
    set SYN_maskb = ${tmp1}
    echo SYN_maskb = ${SYN_maskb}
    echo "set SYN_maska = ${SYN_maska}" >> LOGS/${scriptname}.results
    ${proc_2dx}/linblock "Warning: SYN_maskb needs to be an integer number. Now corrected." >> LOGS/${scriptname}.results
    echo "#WARNING: Warning: SYN_maskb needs to be an integer number. Now corrected." >> LOGS/${scriptname}.results
  endif
  #
  set loc_SYN_mask = ${SYN_maskb}
  set loc_SYN_Bfact = ${SYN_Bfact2}
  set locfactor = '1.0'
  #
  source ${proc_2dx}/2dx_make_SynRef_sub.com
  #
  \mv -f SCRATCH/reference_fft.mrc SCRATCH/${iname}_syn_2_fft.mrc
  echo "# IMAGE: SCRATCH/${iname}_syn_2_fft.mrc <Synthetic Reference 2 (FFT)>" >> LOGS/${scriptname}.results
  #
  \mv -f SCRATCH/reference.mrc SCRATCH/${iname}_syn_2_flt.mrc
  echo  "# IMAGE: SCRATCH/${iname}_syn_2_flt.mrc <Synthetic Reference 2>" >> LOGS/${scriptname}.results
  #
endif
#
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
\rm -f SCRATCH/${iname}_syn_1_ref.mrc
#
${proc_2dx}/lin "labelh.exe"
${bin_2dx}/labelh.exe << eot
SCRATCH/${iname}_syn_1_flt.mrc
1
SCRATCH/${iname}_syn_1_ref.mrc
${boxlabel}
eot
#
echo "# IMAGE-IMPORTANT: SCRATCH/${iname}_syn_1_ref.mrc <Synthetic Reference 1, center>" >> LOGS/${scriptname}.results
#
if ( $do3quadserch == 'y' ) then
  \rm -f SCRATCH/${iname}_syn_2_ref.mrc
  #
  ${proc_2dx}/lin "labelh.exe"
  ${bin_2dx}/labelh.exe << eot
SCRATCH/${iname}_syn_2_flt.mrc
1
SCRATCH/${iname}_syn_2_ref.mrc
${boxlabel}
eot
  #
  echo "# IMAGE-IMPORTANT: SCRATCH/${iname}_syn_2_ref.mrc <Synthetic Reference 2, center>" >> LOGS/${scriptname}.results
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
\rm -f SCRATCH/${iname}_syn_1_flt_box.mrc
#
${proc_2dx}/lin "labelh.exe"
${bin_2dx}/labelh.exe << eot
SCRATCH/${iname}_syn_1_flt.mrc
1
SCRATCH/${iname}_syn_1_flt_box.mrc
${patlabel}
eot
#
if ( ${tempkeep} == 'n' ) then
  \rm -f SCRATCH/${iname}_syn_1_flt.mrc
endif
#
if ( $do3quadserch == 'y' ) then
  #
  \rm -f SCRATCH/${iname}_syn_2_flt_box.mrc
  #
  ${proc_2dx}/lin "labelh.exe"
  ${bin_2dx}/labelh.exe << eot
SCRATCH/${iname}_syn_2_flt.mrc
1
SCRATCH/${iname}_syn_2_flt_box.mrc
${patlabel}
eot
  #
  if ( ${tempkeep} == 'n' ) then
    \rm -f SCRATCH/${iname}_syn_2_flt.mrc
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
\rm -f SCRATCH/${iname}_syn_1_flt_box_auto.mrc
#
${proc_2dx}/lin "autocorrl.exe"
setenv IN  SCRATCH/${iname}_syn_1_flt_box.mrc
setenv OUT SCRATCH/${iname}_syn_1_flt_box_auto.mrc
${bin_2dx}/autocorrl.exe << eot
20
eot
#
if ( $do3quadserch == 'y' ) then
  #
  \rm -f SCRATCH/${iname}_syn_2_flt_box_auto.mrc
  #
  ${proc_2dx}/lin "autocorrl.exe"
  setenv IN  SCRATCH/${iname}_syn_2_flt_box.mrc
  setenv OUT SCRATCH/${iname}_syn_2_flt_box_auto.mrc
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
\rm -f SCRATCH/${iname}_syn_1_flt_box_auto_box.mrc
#
${proc_2dx}/lin "labelh.exe"
${bin_2dx}/labelh.exe << eot
SCRATCH/${iname}_syn_1_flt_box_auto.mrc
1
SCRATCH/${iname}_syn_1_flt_box_auto_box.mrc
210,310,210,310
eot
#
if ( $do3quadserch == 'y' ) then
  #
  \rm -f SCRATCH/${iname}_syn_2_flt_box_auto_box.mrc
  #
  ${proc_2dx}/lin "labelh.exe"
  ${bin_2dx}/labelh.exe << eot
SCRATCH/${iname}_syn_2_flt_box_auto.mrc
1
SCRATCH/${iname}_syn_2_flt_box_auto_box.mrc
210,310,210,310
eot
  #
endif
#
echo "<<@progress: 40>>"






#############################################################################
#############################################################################
#############################################################################
# Now for the input image:
#############################################################################
#############################################################################
#############################################################################






if ( 1 == 2never ) then
  #
  #############################################################################
  ${proc_2dx}/linblock "MASKTRAN - To mask the input image with the spotlist"
  #############################################################################
  #
  #
  setenv IN  FFTIR/${iname}_fft.mrc
  if ( -e GOODSPOT.spt ) then
    setenv SPOTS GOODSPOT.spt
  else
    setenv SPOTS ${nonmaskimagename}.spt
  endif
  setenv OUT SCRATCH/${iname}_fft_msk.mrc
  \rm -f     SCRATCH/${iname}_fft_msk.mrc
  #
  ${proc_2dx}/lin "2dx_masktrana.exe"
${bin_2dx}/2dx_masktrana.exe << eot
1 T T F	                    ! ISHAPE=1(CIRC),2(GAUSCIR),3(RECT)HOLE,IAMPLIMIT(T or F),ISPOT,IFIL
${SYN_maska}                ! RADIUS OF HOLE IF CIRCULAR, X,Y HALF-EDGE-LENGTHS IF RECT.
${lattice},-30,30,-30,30,${rmax},1 !A/BX/Y,IH/IKMN/MX,RMAX,ITYPE
eot
  #
  echo "# IMAGE: SCRATCH/${iname}_fft_msk.mrc <Image, Spot-filtered (FFT)>" >> LOGS/${scriptname}.results
  echo "<<@progress: 45>>"  
  #
  if ( $tempkeep == 'y' ) then
    #############################################################################
    ${proc_2dx}/linblock "FFTTRANS - to calculate filtered image from masked FFT"
    #############################################################################
    #
    \rm -f SCRATCH/${iname}_fft_msk_fft.mrc
    ${proc_2dx}/lin "fftrans.exe"
    setenv IN  SCRATCH/${iname}_fft_msk.mrc
    setenv OUT SCRATCH/${iname}_fft_msk_fft.mrc
    ${bin_2dx}/2dx_fftrans.exe
    #
    echo "# IMAGE: SCRATCH/${iname}_fft_msk_fft.mrc <Image, Spot-filtered>" >> LOGS/${scriptname}.results
    #
    #############################################################################
    ${proc_2dx}/linblock "LABEL - to cut out a larger area from the centre of the masked image"
    #############################################################################
    #
    echo boxlabel = ${boxlabel}
    \rm -f SCRATCH/${iname}_masked.mrc
    #
    ${proc_2dx}/lin "labelh.exe"
    #
    ${bin_2dx}/labelh.exe << eot
SCRATCH/${iname}_fft_msk_fft.mrc
1
SCRATCH/${iname}_masked.mrc
${boxlabel}
eot
    #
    echo "# IMAGE: SCRATCH/${iname}_masked.mrc <Image, Spot-filtered, center>" >> LOGS/${scriptname}.results
    #
  endif
else
  # Using the full FFT of the input image, not spot-list masked:
  \cp -f FFTIR/${iname}_fft.mrc SCRATCH/${iname}_fft_msk.mrc
endif
#
echo "<<@progress: 50>>"
#
#############################################################################
${proc_2dx}/linblock "TWOFILE - to calculate cross-correlation"
#############################################################################
#
#  Multiply two files together    :    FILE1 * Complex Conjugate of FILE2
#
\rm -f FFTIR/${iname}_CCmap1_fft.mrc
#
setenv IN1 SCRATCH/${iname}_fft_msk.mrc
setenv IN2 SCRATCH/${iname}_syn_1_fft.mrc
setenv OUT FFTIR/${iname}_syn_CCmap1_fft.mrc
${proc_2dx}/lin "twofile.exe"
${bin_2dx}/twofile.exe << eot
2				! ICOMB = 2
2 0 0 ${imagecenterx} ${imagecentery} ! IORIGIN,OXA,OYA,OXB,OYB  Origin shifts to FFT's.
eot
#
if ( $do3quadserch == 'y' )then
  \rm -f FFTIR/${iname}_CCmap2_fft.mrc
  #
  setenv IN1 SCRATCH/${iname}_fft_msk.mrc
  setenv IN2 SCRATCH/${iname}_syn_2_fft.mrc
  setenv OUT FFTIR/${iname}_syn_CCmap2_fft.mrc
  ${proc_2dx}/lin "twofile.exe"
  ${bin_2dx}/twofile.exe << eot
2				! ICOMB = 2
2 0 0 ${imagecenterx} ${imagecentery} ! IORIGIN,OXA,OYA,OXB,OYB  Origin shifts to FFT's.
eot
  #
endif
#
if ( ${tempkeep} == 'y' ) then
  \mv -f SCRATCH/${iname}_fft_msk.mrc SCRATCH/${iname}_msk_unbend.mrc
  echo "# IMAGE: SCRATCH/${iname}_msk_unbend.mrc <Image, Spot-filtered (FFT)>" >> LOGS/${scriptname}.results
else
  \rm -f SCRATCH/${iname}_fft_msk.mrc
endif
#
echo "<<@progress: 57>>"
#
#############################################################################
${proc_2dx}/linblock "FFTTRANS - to calculate cross-correlation map"
#############################################################################
#
\rm -f SCRATCH/${iname}_syn_CCmap1.mrc
${proc_2dx}/lin "fftrans.exe"
setenv IN  FFTIR/${iname}_syn_CCmap1_fft.mrc
setenv OUT SCRATCH/${iname}_syn_CCmap1.mrc
${bin_2dx}/2dx_fftrans.exe
#
\rm -f FFTIR/${iname}_syn_CCmap1_fft.mrc
echo "# IMAGE: SCRATCH/${iname}_syn_CCmap1.mrc <CCMap1>" >> LOGS/${scriptname}.results
#
if ( $do3quadserch == 'y' )then
  #
  \rm -f SCRATCH/${iname}_syn_CCmap2.mrc
  ${proc_2dx}/lin "fftrans.exe"
  setenv IN  FFTIR/${iname}_syn_CCmap2_fft.mrc
  setenv OUT SCRATCH/${iname}_syn_CCmap2.mrc
  ${bin_2dx}/2dx_fftrans.exe
  #
  \rm -f FFTIR/${iname}_syn_CCmap2_fft.mrc
  echo "# IMAGE: SCRATCH/${iname}_syn_CCmap2.mrc <CCMap2>" >> LOGS/${scriptname}.results
endif
#
echo "<<@progress: 60>>"
#
#############################################################################
${proc_2dx}/linblock "QUADSERCH - to search cross-correlation map for peaks, IPASS=1"
#############################################################################
#
setenv PROFILE  SCRATCH/${iname}_syn_1_flt_box_auto_box.mrc
setenv PROFDATA SCRATCH/prof${iname}.dat
setenv ERRORS   SCRATCH/errors${iname}.dat
setenv ERROUT   SCRATCH/errout${iname}.dat
#
\rm -f CCPLOT.PS
\rm -f SPIDERCOORD.spi
#
${proc_2dx}/lin "quadserchh.exe, IPASS=1"
${bin_2dx}/2dx_quadserchk-2.exe << eot
1,${SYN_quadpreda}                    ! IPASS,NRANGE
SCRATCH/${iname}_syn_CCmap1.mrc
$imagesidelength,$imagesidelength     ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F                          ! Lattice vectors
-200,200,-200,200                     ! NUMBER UNIT CELLS TO SEARCH
${SYN_quadrada},${SYN_quadrada}       ! RADIUS OF CORR SEARCH
${imagecenterx} ${imagecentery}       ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N                                     ! YES/NO FOR DETAILED PRINTOUT
${radlim}                             ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}                             ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
0                                     ! dont create manual Masking information
0                                     ! Dont mask the image directly
eot
#
\mv -f CCPLOT.PS PS/${iname}-quadserchSa.ps
echo "# IMAGE: PS/${iname}-quadserchSa.ps <PS: Vector Plot of Distortion, Reference 1, Pass 1>"  >> LOGS/${scriptname}.results 
#
\rm -f CCPLOT.PS
\rm -f SPIDERCOORD.spi
#
#
#############################################################################
${proc_2dx}/linblock "QUADSERCH - to search cross-correlation map for peaks, IPASS=3"
#############################################################################
#
setenv ERRORS   SCRATCH/errors${iname}.dat
setenv ERROUT   SCRATCH/errout${iname}.dat
\rm -f SCRATCH/errout${iname}.dat
\rm -f SCRATCH/prof${iname}.dat
#
${proc_2dx}/lin "quadserchh.exe, IPASS=3"
#
${bin_2dx}/2dx_quadserchk-2.exe << eot
3,${SYN_quadpreda}			! IPASS,NRANGE
SCRATCH/${iname}_syn_CCmap1.mrc
${imagesidelength},${imagesidelength}                     ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F			      ! Lattice vectors
-200,200,-200,200	      	      ! NUMBER UNIT CELLS TO SEARCH
${SYN_quadrada},${SYN_quadrada}       ! RADIUS OF CORR SEARCH
${imagecenterx} ${imagecenterx} 	      ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N				      ! YES/NO FOR DETAILED PRINTOUT
${radlim}         		      ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}                   ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
0                                     ! dont create manual Masking information
0                                     ! Dont mask the image directly
eot
# 
\mv -f CCPLOT.PS PS/${iname}-quadserchSb.ps
echo "# IMAGE: PS/${iname}-quadserchSb.ps <PS: Vector Plot of Distortion, Reference 1, Pass 2>" >> LOGS/${scriptname}.results 
#
\mv -f SPIDERCOORD.spi ${iname}-unitcells-spider.doc
echo "# IMAGE: ${iname}-unitcells-spider.doc <SPIDER document with unit cell locations>" >> LOGS/${scriptname}.results
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
  \rm -f SCRATCH/prof${iname}.dat
  #
  setenv PROFILE  SCRATCH/${iname}_syn_2_flt_box_auto_box.mrc
  setenv PROFDATA SCRATCH/prof${iname}.dat
  setenv ERRORS   SCRATCH/errout${iname}.dat
  #
  \rm -f CCPLOT.PS
  \rm -f SPIDERCOORD.spi
  #
  ${proc_2dx}/lin "quadserchh.exe, IPASS=2"
  ${bin_2dx}/2dx_quadserchk-2.exe << eot
2,${SYN_quadpredb}                     ! IPASS,NRANGE
SCRATCH/${iname}_syn_CCmap2.mrc
${imagesidelength},${imagesidelength}     ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F                       ! Lattice vectors
-200,200,-200,200               ! NUMBER UNIT CELLS TO SEARCH
${SYN_quadradb},${SYN_quadradb}           ! RADIUS OF CORR SEARCH
${imagecenterx} ${imagecentery}           ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N                               ! YES/NO FOR DETAILED PRINTOUT
${radlim}                       ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}             ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
0                               ! dont create manual Masking information
0                               ! Dont mask the image directly
eot
  #
  \mv -f CCPLOT.PS PS/${iname}-quadserchSc.ps
  echo "# IMAGE: PS/${iname}-quadserchSc.ps <PS: Vector Plot of Distortion, Reference 2, Pass 2>" >> LOGS/${scriptname}.results 
  #
  \mv -f SPIDERCOORD.spi ${iname}-unitcells-spider.doc
  echo "# IMAGE: ${iname}-unitcells-spider.doc <SPIDER document with unit cell locations>" >> LOGS/${scriptname}.results
  #
  echo "# IMAGE: SCRATCH/${iname}_syn_CCmap2.mrc <CCMap2>" >> LOGS/${scriptname}.results
  #
endif
#
#############################################################################
${proc_2dx}/linblock "CCUNBEND - to unbend the CCmap1"
#############################################################################
#
\rm -f SCRATCH/${iname}_CCmap1_unbent.mrc
setenv CCORDATA SCRATCH/prof${iname}.dat
\rm -f SCRATCH/ccunbend-table-${iname}.dat
setenv TABLEOUT SCRATCH/ccunbend-table-${iname}.dat
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
SCRATCH/${iname}_syn_CCmap1.mrc
${ITYPE},1,${IMAXCOR},${ISTEP},F,40,T 	 !ITYPE,IOUT,IMAXCOR,ISTEP,LTAPER,RTAPER 
30,52,0.001,${SYN_facthresha},${TLTAXIS},${RMAG},${LCOLOR} !IKX,IKY,EPS,FACTOR,TLTAXIS,RMAG,LCOLOR
${imagename}, synthetical unbend, ${date}
SCRATCH/${iname}_CCmap1_unbent.mrc
UNBENT with synthetical reference, ${date}
eot
#
\rm -f fort.17 
\rm -f CCPLOT.PS
echo "# IMAGE: SCRATCH/${iname}_CCmap1_unbent.mrc <CCmap1, unbent>" >> LOGS/${scriptname}.results
#
#############################################################################
${proc_2dx}/linblock "CCUNBEND - to unbend the original image"
#############################################################################
#
\rm -f SCRATCH/${iname}_syn_unbent.mrc
setenv CCORDATA SCRATCH/prof${iname}.dat
\rm -f SCRATCH/ccunbend-table-${iname}.dat
setenv TABLEOUT SCRATCH/ccunbend-table-${iname}.dat
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
${iname}.mrc
${ITYPE},1,${IMAXCOR},${ISTEP},F,40,T 	 !ITYPE,IOUT,IMAXCOR,ISTEP,LTAPER,RTAPER 
30,52,0.001,${SYN_facthresha},${TLTAXIS},${RMAG},${LCOLOR} !IKX,IKY,EPS,FACTOR,TLTAXIS,RMAG,LCOLOR
${imagename}, synthetical unbend, ${date}
SCRATCH/${iname}_syn_unbent.mrc
UNBENT with synthetical reference, ${date}
eot
#
\mv -f fort.17 SCRATCH/ccunbend-histo-${iname}.dat
#
\mv -f CCPLOT.PS PS/${iname}-ccunbend.ps
echo "# IMAGE-IMPORTANT: PS/${iname}-ccunbend.ps <PS: Vector Plot for Unbending>" >> LOGS/${scriptname}.results 
#
echo "# IMAGE: SCRATCH/${iname}_syn_unbent.mrc <Image, unbent>" >> LOGS/${scriptname}.results
#
#############################################################################
${proc_2dx}/${lincommand} "gzip - to compact profile to save space on the harddrive"
#############################################################################
#
\cp -f SCRATCH/prof${iname}.dat ${nonmaskimagename}_syn_profile.dat
#
echo "<<@progress: 70>>"
#
#############################################################################
${proc_2dx}/linblock "taperedge - to smoothen edges before FFT."
#############################################################################
#
\rm -f SCRATCH/${iname}_syn_unbent_taper.mrc
setenv IN  SCRATCH/${iname}_syn_unbent.mrc
setenv OUT SCRATCH/${iname}_syn_unbent_taper.mrc
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
\rm -f SCRATCH/${iname}_syn_unbent_taper_fft.mrc
${proc_2dx}/lin "fftrans.exe"
setenv IN  SCRATCH/${iname}_syn_unbent_taper.mrc
setenv OUT SCRATCH/${iname}_syn_unbent_taper_fft.mrc
${bin_2dx}/2dx_fftrans.exe
#
echo "# IMAGE-IMPORTANT: SCRATCH/${iname}_syn_unbent_taper_fft.mrc <Image, unbent, edge-tapered (FFT)>" >> LOGS/${scriptname}.results
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
  #
  \rm -f APH/${iname}_tmp.aph
  #
  ${proc_2dx}/linblock "MMBOX - resolution limitation for diagnostics"
  ${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/${iname}_syn_unbent_taper_fft.mrc
  ${imagenumber}     ${imagename} ${date}, PASS 1
Y				! Use grid units?
Y				! Generate grid from lattice?
N				! Generate points from lattice?
2 2 0 50 50 19 19		! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${iname}_tmp.aph
SCRATCH/TMP98711.dat
US
${imagecenterx},${imagecentery}           ! XORIG,YORIG
${res1},${res2},1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}		 		! Lattice vectors
eot
  #
  \rm -f APH/${iname}_tmp.aph
  #
  ${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/${iname}_syn_unbent_taper_fft.mrc
  ${imagenumber}     ${imagename} ${date}, PASS 1
Y				! Use grid units?
Y				! Generate grid from lattice?
N				! Generate points from lattice?
2 2 0 50 50 19 19		! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${iname}_tmp.aph
SCRATCH/TMP98712.dat
US
${imagecenterx},${imagecentery}           ! XORIG,YORIG
${res2},${res3},1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}		 		! Lattice vectors
eot
  #
  \rm -f APH/${iname}_tmp.aph
  #
  ${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/${iname}_syn_unbent_taper_fft.mrc
  ${imagenumber}     ${imagename} ${date}, PASS 1
Y				! Use grid units?
Y				! Generate grid from lattice?
N				! Generate points from lattice?
2 2 0 50 50 19 19		! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${iname}_tmp.aph
SCRATCH/TMP98713.dat
US
${imagecenterx},${imagecentery}           ! XORIG,YORIG
${res3} ${res4} 1 ${realcell} ${ALAT} ${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}		 		! Lattice vectors
eot
  #
  \rm -f APH/${iname}_tmp.aph
  #
  ${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/${iname}_syn_unbent_taper_fft.mrc
  ${imagenumber}     ${imagename} ${date}, PASS 1
Y				! Use grid units?
Y				! Generate grid from lattice?
N				! Generate points from lattice?
2 2 0 50 50 19 19		! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${iname}_tmp.aph
SCRATCH/TMP98714.dat
US
${imagecenterx},${imagecentery}           ! XORIG,YORIG
${res4} ${res5} 1 ${realcell} ${ALAT} ${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}		 		! Lattice vectors
eot
  #
  \rm -f APH/${iname}_tmp.aph
  #
  ${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/${iname}_syn_unbent_taper_fft.mrc
  ${imagenumber}     ${imagename} ${date}, PASS 1
Y				! Use grid units?
Y				! Generate grid from lattice?
N				! Generate points from lattice?
2 2 0 50 50 19 19		! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${iname}_tmp.aph
SCRATCH/TMP98715.dat
US
${imagecenterx},${imagecentery}           ! XORIG,YORIG
${res5} ${res6} 1 ${realcell} ${ALAT} ${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}		 		! Lattice vectors
eot
  #
  \rm -f APH/${iname}_tmp.aph
  #
  ${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/${iname}_syn_unbent_taper_fft.mrc
  ${imagenumber}     ${imagename} ${date}, PASS 1
Y				! Use grid units?
Y				! Generate grid from lattice?
N				! Generate points from lattice?
2 2 0 50 50 19 19		! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${iname}_tmp.aph
SCRATCH/TMP98716.dat
US
${imagecenterx},${imagecentery}           ! XORIG,YORIG
${res6} ${res7} 1 ${realcell} ${ALAT} ${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}		 		! Lattice vectors
eot
  #
  \rm -f APH/${iname}_tmp.aph
  #
  #####################################################################
#
echo "<<@progress: 76>>"
#
#
#############################################################################
${proc_2dx}/linblock "MMBOX - to read out amplitudes and phases"
#############################################################################
#
\rm -f SCRATCH/TMP9871.dat
\rm -f APH/${iname}_syn_unbend_tmp.aph
#
${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/${iname}_syn_unbent_taper_fft.mrc
  ${imagenumber}     ${imagename} ${date}, PASS 1
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2 2 0 50 50 19 19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${iname}_syn_unbend_tmp.aph
SCRATCH/TMP9871.dat
US
${imagecenterx},${imagecentery}           ! XORIG,YORIG
${RESMIN} 1.5 1 ${realcell} ${ALAT} ${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot
#
\mv -f APH/${iname}_syn_unbend_tmp.aph APH/${iname}_syn_unbend.aph
echo ${ctfcor_imode} > APH/${iname}_syn_unbend.aph_ctfcor_imode
#
echo "# IQSTAT-RESLIM:"
cat SCRATCH/TMP9871.dat >> LOGS/${scriptname}.results 
source SCRATCH/TMP9871.dat
#
set IQS = `echo ${US_IQ1} ${US_IQ2} ${US_IQ3} ${US_IQ4} ${US_IQ5} ${US_IQ6} ${US_IQ7} ${US_IQ8} ${US_IQ9}`
#":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
${proc_2dx}/linblock "Synthetic Unbend with maska=${SYN_maska} maskb=${SYN_maskb} gives QVAL= ${QVAL_local}"
${proc_2dx}/linblock "IQ stat = ${IQS}"
#":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
#
echo " " >> History.dat
echo ":Date: ${date}" >> History.dat
echo "::Unbend S: maska=${SYN_maska} maskb=${SYN_maskb}: QVAL= ${QVAL_local}      IQ stat = ${IQS}" >> History.dat
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






