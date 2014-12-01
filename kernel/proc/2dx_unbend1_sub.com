#
#  2dx_unbend1_sub.com
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
# set maska = $1
# set boxa1 = $2
# set scriptname = $3
#
if ( ${scriptname} == '2dx_unbend1' ) then
  set lincommand = "linblock"
  #################################################################################
  ${proc_2dx}/linblock "Unbend 1: ${imagename}, maska=${maska}, boxa1=${boxa1}"
  #################################################################################
  #
else
  set lincommand = "lin"
endif
#
\rm -f ${imagename}_int.mrc
#
source ${proc_2dx}/2dx_makedirs
#
#
#############################################################################
${proc_2dx}/${lincommand} "MASKTRAN - to mask the image according to the spotlist"
#############################################################################
#
\rm -f SCRATCH/${imagename}_fft_msk1.mrc
#
setenv IN  FFTIR/${imagename}_fft.mrc
setenv OUT SCRATCH/${imagename}_fft_msk1.mrc
setenv SPOTS ${imagename}.spt
#
${bin_2dx}/2dx_masktrana.exe << eot
1 T T F ! ISHAPE=1(CIRC),2(GAUSCIR),3(RECT)HOLE,IAMPLIMIT(T or F),ISPOT,IFIL
${maska}  ! RADIUS OF HOLE IF CIRCULAR, X,Y HALF-EDGE-LENGTHS IF RECT.
${lattice} -9 9 -9 9 ${rmax} 1 !A/BX/Y,IH/IKMN/MX,RMAX,ITYPE
eot
#
echo "<<@progress: 7>"
#
if ( ${tempkeep} == 'y' ) then
  #############################################################################
  ${proc_2dx}/${lincommand} "FFTTRANS - to calculate Fourier filtered image"
  #############################################################################
  #
  \rm -f SCRATCH/${imagename}_fft_msk1_fft.mrc
  setenv IN  SCRATCH/${imagename}_fft_msk1.mrc
  setenv OUT SCRATCH/${imagename}_fft_msk1_fft.mrc
  ${bin_2dx}/2dx_fftrans.exe  
  #
  echo "# IMAGE: SCRATCH/${imagename}_fft_msk1.mrc <Image, Fourier filtered (${maska}px) (FFT)>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/${imagename}_fft_msk1_fft.mrc <Image, Fourier filtered (${maska}px)>" >> LOGS/${scriptname}.results
endif
#
#
#
#############################################################################
${proc_2dx}/${lincommand} "MASKTRAN - to mask a reference according to the spotlist"
#############################################################################
#
setenv IN  FFTIR/${imagename}_fft.mrc
setenv OUT SCRATCH/${imagename}_fft_msk2.mrc
setenv SPOTS ${imagename}.spt
set holea = 1
#
\rm -f SCRATCH/${imagename}_fft_msk2.mrc
#
${bin_2dx}/2dx_masktrana.exe << eot
1 T T F	! ISHAPE=1(CIRC),2(GAUSCIR),3(RECT)HOLE,IAMPLIMIT(T or F),ISPOT,IFIL
${holea}       ! RADIUS OF HOLE IF CIRCULAR, X,Y HALF-EDGE-LENGTHS IF RECT.
${lattice} -50 50 -50 50 ${rmax} 1 !A/B X/Y,IH/IK MN/MX,RMAX,ITYPE
eot
#
echo "<<@progress: 10>"
#
#
#############################################################################
${proc_2dx}/${lincommand} "FFTTRANS - to calculate tightly filtered image as reference"
#############################################################################
#
\rm -f SCRATCH/${imagename}_fft_msk2_fft.mrc
setenv IN  SCRATCH/${imagename}_fft_msk2.mrc
setenv OUT SCRATCH/${imagename}_fft_msk2_fft.mrc
${bin_2dx}/2dx_fftrans.exe
#
if ( ${tempkeep} == 'y' ) then
  echo "# IMAGE: SCRATCH/${imagename}_fft_msk2.mrc <Image, Fourier filtered (${holea}px) (FFT)>" >> LOGS/${scriptname}.results
  echo "# IMAGE: SCRATCH/${imagename}_fft_msk2_fft.mrc <Image, Fourier filtered (${holea}px)>" >> LOGS/${scriptname}.results
else
  \rm -f SCRATCH/${imagename}_fft_msk2.mrc 
endif
#
#
#############################################################################
${proc_2dx}/${lincommand} "LABEL - to cut centre of reference for debugging"
#############################################################################
#
echo boxlabel = ${boxlabel}
#
\rm -f SCRATCH/${imagename}_ref1.mrc
#
${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}_fft_msk2_fft.mrc
1
SCRATCH/${imagename}_ref1.mrc
${boxlabel}
eot
#
echo "<<@progress: 13>"
echo "# IMAGE: SCRATCH/${imagename}_ref1.mrc <Reference Unbend1>" >> LOGS/${scriptname}.results
#
#
#############################################################################
${proc_2dx}/${lincommand} "LABEL - to cut centre of the filtered image"
#############################################################################
#
echo patlabel = ${patlabel}
#
\rm -f SCRATCH/${imagename}_fft_msk2_fft_box1.mrc
${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}_fft_msk2_fft.mrc
1
SCRATCH/${imagename}_fft_msk2_fft_box1.mrc
${patlabel}
eot
#
echo "<<@progress: 16>"
echo "<<@evaluate>>"
#
#############################################################################
${proc_2dx}/${lincommand} "AUTOCORRL - to calculate the autocorrelation of the boxed reference"
#               this 25x25 box is 20 times magnif., => 500x500              #
#############################################################################
#
\rm -f SCRATCH/${imagename}_fft_msk2_fft_box_aut.mrc 
setenv IN  SCRATCH/${imagename}_fft_msk2_fft_box1.mrc
setenv OUT SCRATCH/${imagename}_fft_msk2_fft_box_aut.mrc
${bin_2dx}/autocorrl.exe << eot
20
eot
#
echo "<<@progress: 20>"
#
#
#############################################################################
${proc_2dx}/${lincommand} "LABEL - to cut central region from the autocorrelation map"
#           500x500 => 100x100                                              #
#############################################################################
#
\rm -f SCRATCH/${imagename}_fft_msk2_fft_box_aut_cro.mrc
${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}_fft_msk2_fft_box_aut.mrc
1
SCRATCH/${imagename}_fft_msk2_fft_box_aut_cro.mrc
210,310,210,310
eot
#
\rm -f SCRATCH/${imagename}_fft_msk2_fft_box_aut.mrc
#
echo "# IMAGE: SCRATCH/${imagename}_fft_msk2_fft_box_aut_cro.mrc <Autocorrelation Function of Reference>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 23>"
#
#############################################################################
${proc_2dx}/${lincommand} "BOXIMAGE - to box out reference for cross-correlation"
#############################################################################
#
\rm -f SCRATCH/${imagename}_fft_msk2_fft_box1.mrc
#
setenv IN  SCRATCH/${imagename}_fft_msk2_fft.mrc
setenv OUT SCRATCH/${imagename}_fft_msk2_fft_box1.mrc
echo refposi = ${refposix} ${refposiy}
echo boxa1 = ${boxa1}
${bin_2dx}/2dx_boximage.exe << eot
-1 0 		!  NOVERT, VERTEX COORDS GIVEN IN GRID UNITS RELATIVE TO (0,0) ORIGIN.
0 0		! ORIGIN FOR LATER USE (E.G. IN FOURIER TRANSFORM)
${refposix} ${refposiy}  ! VERTEX COORDINATES IN GRID STEPS WRT CORNER (0,0)
${boxa1}
eot
#
echo "<<@progress: 26>"
if ( ${treatspotscan} == 'y' ) then
  \rm -f SCRATCH/${imagename}_fft_msk2_fft_box2.mrc
  #
  setenv IN  SCRATCH/${imagename}_fft_msk2_fft.mrc
  setenv OUT SCRATCH/${imagename}_fft_msk2_fft_box2.mrc
  ${proc_2dx}/${lincommand} "BOXIMAGE - second time for the SpotScan Reference"
  ${bin_2dx}/2dx_boximage.exe << eot
-1 0             !  NOVERT, VERTEX COORDS GIVEN IN GRID UNITS RELATIVE TO (0,0) ORIGIN.
0 0             ! ORIGIN FOR LATER USE (E.G. IN FOURIER TRANSFORM)
${refposix} ${refposiy}  ! VERTEX COORDINATES IN GRID STEPS WRT CORNER (0,0)
${boxa2}
eot
  #
endif
#
if ( ${tempkeep} == 'y' ) then
  # \mv -f SCRATCH/${imagename}_fft_msk2_fft.mrc SCRATCH/${imagename}_ref_flt_unbend1.mrc
else
  \rm -f SCRATCH/${imagename}_fft_msk2_fft.mrc
endif
#
#
echo "<<@progress: 30>"
#
#############################################################################
${proc_2dx}/${lincommand} "FFTRANS - to calculate FFT from reference patch"
#############################################################################
#
\rm -f SCRATCH/${imagename}_fft_msk2_fft_box1_fft.mrc
setenv IN  SCRATCH/${imagename}_fft_msk2_fft_box1.mrc
setenv OUT SCRATCH/${imagename}_fft_msk2_fft_box1_fft.mrc
${bin_2dx}/2dx_fftrans.exe
#
if ( ${tempkeep} == 'y' ) then
  # \mv -f SCRATCH/${imagename}_fft_msk2_fft_box1.mrc SCRATCH/ref1${imagename}_flt_unbend1.mrc
  echo "# IMAGE: SCRATCH/${imagename}_fft_msk2_fft_box1.mrc <Reference 1>" >> LOGS/${scriptname}.results
else
  \rm -f SCRATCH/${imagename}_fft_msk2_fft_box1.mrc
endif
#
echo "# IMAGE: SCRATCH/${imagename}_fft_msk2_fft_box1_fft.mrc <Reference 1 (FFT)>" >> LOGS/${scriptname}.results
#
if ( ${treatspotscan} == 'y' ) then
  #
  \rm -f SCRATCH/${imagename}_fft_msk2_fft_box2_fft.mrc
  setenv IN  SCRATCH/${imagename}_fft_msk2_fft_box2.mrc
  setenv OUT SCRATCH/${imagename}_fft_msk2_fft_box2_fft.mrc
  ${proc_2dx}/${lincommand} "FFTRANS - a second time for SpotScan reference"
  ${bin_2dx}/2dx_fftrans.exe
  #
  if ( ${tempkeep} == 'y' ) then
    # \mv -f SCRATCH/${imagename}_fft_msk2_fft_box2.mrc SCRATCH/ref2${imagename}_flt_unbend1.mrc
    echo "# IMAGE: SCRATCH/${imagename}_fft_msk2_fft_box2.mrc <Reference 2>" >> LOGS/${scriptname}.results
  else
    \rm -f SCRATCH/${imagename}_fft_msk2_fft_box2.mrc 
  endif
  #
  echo "# IMAGE: SCRATCH/${imagename}_fft_msk2_fft_box2_fft.mrc <Reference 2 (FFT)>" >> LOGS/${scriptname}.results
  #
endif
#
echo "<<@progress: 35>"
echo "<<@evaluate>>"
#
#############################################################################
${proc_2dx}/${lincommand} "TWOFILE - to calculate cross-correlation"
#############################################################################
#
#  Multiply two files together    :    FILE1 * Complex Conjugate of FILE2
#
\rm -f SCRATCH/${imagename}_CCmap1_fft.mrc
setenv IN1 SCRATCH/${imagename}_fft_msk1.mrc
setenv IN2 SCRATCH/${imagename}_fft_msk2_fft_box1_fft.mrc
setenv OUT SCRATCH/${imagename}_CCmap1_fft.mrc
${bin_2dx}/twofile.exe << eot
2		! ICOMB = 2
2 0 0 ${refposix} ${refposiy} ! IORIGIN,OXA,OYA,OXB,OYB  Origin shifts to FFT's.
eot
#
echo "<<@progress: 40>"
#
if ( ${treatspotscan} == 'y' ) then
  #
  \rm -f SCRATCH/${imagename}_CCmap2_fft.mrc
  setenv IN1 SCRATCH/${imagename}_fft_msk1.mrc
  setenv IN2 SCRATCH/${imagename}_fft_msk2_fft_box2_fft.mrc
  setenv OUT SCRATCH/${imagename}_CCmap2_fft.mrc
  ${proc_2dx}/${lincommand} "TWOFILE - a second time for SpotScan"
  ${bin_2dx}/twofile.exe << eot
2               ! ICOMB = 2
2 0 0 ${refposix} ${refposiy} ! IORIGIN,OXA,OYA,OXB,OYB  Origin shifts to FFT's.
eot
  #
endif
#
echo "<<@progress: 45>"
#
#############################################################################
${proc_2dx}/${lincommand} "FFTTRANS - to calculate cross-correlation map"
#############################################################################
#
\rm -f SCRATCH/${imagename}_fft_msk2_fft_box1_fft.mrc
#
\rm -f SCRATCH/${imagename}_CCmap1.mrc
setenv IN  SCRATCH/${imagename}_CCmap1_fft.mrc
setenv OUT SCRATCH/${imagename}_CCmap1.mrc
${bin_2dx}/2dx_fftrans.exe
#
echo "# IMAGE: SCRATCH/${imagename}_CCmap1.mrc <CCmap with Reference 1>" >> LOGS/${scriptname}.results
echo "<<@progress: 50>"
#
\rm -f SCRATCH/${imagename}_CCmap1_fft.mrc
#
if ( ${treatspotscan} == 'y' ) then
  #
  \rm -f SCRATCH/${imagename}_fft_msk2_fft_box2_fft.mrc
  #
  \rm -f SCRATCH/${imagename}_CCmap2.mrc
  setenv IN  SCRATCH/${imagename}_CCmap2_fft.mrc
  setenv OUT SCRATCH/${imagename}_CCmap2.mrc
  ${proc_2dx}/${lincommand} "FFTTRANS - a second time for SpotScan"
  ${bin_2dx}/2dx_fftrans.exe
  echo "# IMAGE: SCRATCH/${imagename}_CCmap2.mrc <CCmap with Reference 2>" >> LOGS/${scriptname}.results
  #
  \rm -f SCRATCH/${imagename}_CCmap2_fft.mrc
  #
endif
#
#
echo "<<@progress: 55>"
#
#############################################################################
${proc_2dx}/${lincommand} "QUADSERCH - to search cross-correlation map for peaks"
${proc_2dx}/${lincommand} "with IPASS=1 to find first ERROR field"
#############################################################################
#
\rm -f SCRATCH/prof${imagename}.dat
setenv PROFILE  SCRATCH/${imagename}_fft_msk2_fft_box_aut_cro.mrc
setenv PROFDATA SCRATCH/prof${imagename}.dat
setenv ERRORS   SCRATCH/errors${imagename}.dat
if ( ${treatspotscan} == 'n' ) then
  set cormap = SCRATCH/${imagename}_CCmap1.mrc
  set valspotscan = '0'
else
  set cormap = SCRATCH/${imagename}_CCmap2.mrc
  set valspotscan = '1'
endif
#
\rm -f CCPLOT.PS
\rm -f SPIDERCOORD.spi
\rm -f TMP_quadserch_1.mrc
\rm -f TMP_quadserch_2.mrc
\rm -f TMP_quadserch_3.mrc
\rm -f TMP_quadserch_4.mrc
\rm -f TMP_quadserch_5.mrc
\rm -f TMP_quadserch_6.mrc
\rm -f TMP_quadserch_7.mrc
#
echo "Starting now:"
echo "${bin_2dx}/2dx_quadserchk-2.exe with:" 
echo "1,${quadpreda}"
echo "${cormap}"
echo "${imagesidelength},${imagesidelength}"
echo "${lattice},F"
echo "-200,200,-200,200"
echo "${quadradax},${quadraday}"
echo "${refposix},${refposiy}"
echo "N		"
echo "${radlim} "
echo "${valspotscan},${RMAG},${LCOLOR}"
echo "0	"
echo "0	"
echo " "
echo "starting it now...."
echo " "
#
${bin_2dx}/2dx_quadserchk-2.exe << eot
1,${quadpreda}			! IPASS,NRANGE
${cormap}
${imagesidelength},${imagesidelength}         ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F			! Lattice vectors
-200,200,-200,200		! NUMBER UNIT CELLS TO SEARCH
${quadradax},${quadraday}	! RADIUS OF CORR SEARCH
${refposix},${refposiy} 	! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N				! YES/NO FOR DETAILED PRINTOUT
${radlim}         		! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}	! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
0		                ! dont create manual Masking information
0                               ! Dont mask the image directly
eot
# 
if ( ! -e CCPLOT.PS ) then
  ${proc_2dx}/protest "ERROR in quadsearch: CCPLOT.PS not generated."
else
  \mv -f CCPLOT.PS PS/${imagename}_quadserch1a.ps
  echo "# IMAGE: PS/${imagename}_quadserch1a.ps <PS: Vector Plot for Distortions, Pass 1>" >> LOGS/${scriptname}.results
endif
#
#
echo "<<@progress: 60>"
#
#############################################################################
${proc_2dx}/${lincommand} "QUADSERCH - to search cross-correlation map for peaks"
${proc_2dx}/${lincommand} "with IPASS=3 to read in first ERROR field and generate better ERROUT"
#  This should find positions of SpotScan spots.                            #
#############################################################################
#
\rm -f SCRATCH/prof${imagename}.dat
setenv PROFILE  SCRATCH/${imagename}_fft_msk2_fft_box_aut_cro.mrc
setenv PROFDATA SCRATCH/prof${imagename}.dat
setenv ERRORS   SCRATCH/errors${imagename}.dat
\rm -f SCRATCH/errout${imagename}.dat
setenv ERROUT   SCRATCH/errout${imagename}.dat
\rm -f SPIDERCOORD.spi
\rm -f TMP_quadserch_1.mrc
\rm -f TMP_quadserch_2.mrc
\rm -f TMP_quadserch_3.mrc
\rm -f TMP_quadserch_4.mrc
\rm -f TMP_quadserch_5.mrc
\rm -f TMP_quadserch_6.mrc
\rm -f TMP_quadserch_7.mrc
#
if ( ${createmaskinfo}x == 'yx' ) then
      set createmask = '1'
      \rm -f ManualMasking-CCmap.mrc
      \rm -f ManualMasking-UnbendPlot.mrc
      ${proc_2dx}/linblock "Creating info for manual masking"
else
      set createmask = '0'
endif
#
${bin_2dx}/2dx_quadserchk-2.exe << eot
3,${quadpreda}                     ! IPASS,NRANGE
${cormap} 
${imagesidelength},${imagesidelength}         ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F                    ! Lattice vectors
-200,200,-200,200               ! NUMBER UNIT CELLS TO SEARCH
${quadradax},${quadraday}         ! RADIUS OF CORR SEARCH
${refposix} ${refposiy}           ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N                               ! YES/NO FOR DETAILED PRINTOUT
${radlim}                       ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}	! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
${createmask}                               ! do/dont create manual Masking information
0                               ! Dont mask the image directly
eot
#
\mv -f CCPLOT.PS PS/${imagename}_quadserch1b.ps
echo "# IMAGE: PS/${imagename}_quadserch1b.ps <PS: Vector Plot for Distortions, Pass 2>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 65>"
echo "<<@evaluate>>"
#
if ( ${treatspotscan} == 'y' ) then
  #
  #############################################################################
  #  QUADSERCH - to search cross-correlation map for peaks                    #
  #  with IPASS=2 to read in better ERROUT field and find cc-peaks            #
  #  this should find lattice within SpotScan spots.                          #
  #############################################################################
  #
  \rm -f SCRATCH/prof${imagename}.dat
  \rm -f SPIDERCOORD.spi
  \rm -f TMP_quadserch_1.mrc
  \rm -f TMP_quadserch_2.mrc
  \rm -f TMP_quadserch_3.mrc
  \rm -f TMP_quadserch_4.mrc
  \rm -f TMP_quadserch_5.mrc
  \rm -f TMP_quadserch_6.mrc
  \rm -f TMP_quadserch_7.mrc
  #
  if ( ${createmaskinfo}x == 'yx' ) then
      set createmask = '1'
      \rm -f ManualMasking-CCmap.mrc
      \rm -f ManualMasking-UnbendPlot.mrc
      ${proc_2dx}/linblock "Creating info for manual masking"
  else
      set createmask = '0'
  endif
  #
  setenv PROFILE  SCRATCH/${imagename}_fft_msk2_fft_box_aut_cro.mrc
  setenv PROFDATA SCRATCH/prof${imagename}.dat
  setenv ERRORS   SCRATCH/errout${imagename}.dat
  ${proc_2dx}/${lincommand} "QUADSERCH - a third time for SpotScan images"
  ${bin_2dx}/2dx_quadserchk-2.exe << eot
2,${quadpreda}                  ! IPASS,NRANGE
SCRATCH/${imagename}_CCmap1.mrc
${imagesidelength},${imagesidelength}         ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F                    ! Lattice vectors
-200,200,-200,200               ! NUMBER UNIT CELLS TO SEARCH
${quadradax},${quadraday}         ! RADIUS OF CORR SEARCH
${refposix} ${refposiy}           ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N                               ! YES/NO FOR DETAILED PRINTOUT
${radlim}                       ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}		! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
${createmask}                               ! do/dont create manual Masking information
0                               ! Dont mask the image directly
eot
  #
  \rm -f SCRATCH/${imagename}_CCmap2.mrc 
  \mv -f CCPLOT.PS PS/${imagename}_quadserch1c.ps
  echo "# IMAGE-IMPORTANT: PS/${imagename}_quadserch1c.ps <PS: Vector Plot for Distortions, Pass 3>" >> LOGS/${scriptname}.results
  #
endif
\rm -f SCRATCH/${imagename}_CCmap1.mrc
#
echo "<<@progress: 68>"
#
#
#############################################################################
${proc_2dx}/${lincommand} "CCUNBEND - to unbend the original image"
#############################################################################
#
# Hopefully, ITYPE=1 does not crash in NAGLIB ???
# set ITYPE = 1
set ITYPE = 0
\rm -f SCRATCH/${imagename}_unbend1_notap.mrc
#
echo IMAXCOR = ${IMAXCOR}
${proc_2dx}/lin "Using IMAXCOR = ${IMAXCOR}"
#
echo " "
echo "Starting now:"
#
if ( ${ccunbend_program} == "1" ) then
  set ROFFSET = 50.0
  set NNUM = 6
  ${proc_2dx}/${lincommand} "   Using 2dx_ccunbendh"
  #
  \rm -f SCRATCH/${imagename}_unbend1_notap.mrc
  setenv CCORDATA SCRATCH/prof${imagename}.dat
  ${bin_2dx}/2dx_ccunbendh.exe << eot
${imagename}.mrc
${IMAXCOR},${ISTEP_h},${NNUM},${ROFFSET}	 !IMAXCOR,ISTEP,NNUM,ROFFSET
0.001,${facthresha},${RMAG} !EPS,FACTOR,RMAG
SCRATCH/${imagename}_unbend1_notap.mrc
UNBEND1,${date}
eot
  #
else
  #
  ${proc_2dx}/${lincommand} "   Using 2dx_ccunbendk"
  #
  \rm -f SCRATCH/${imagename}_unbend1_notap.mrc
  setenv CCORDATA SCRATCH/prof${imagename}.dat
  ${bin_2dx}/2dx_ccunbendk.exe << eot
${imagename}.mrc
${ITYPE},1,${IMAXCOR},${ISTEP},F,40,F	 !ITYPE,IOUT,IMAXCOR,ISTEP,LTAPER,RTAPER,LTABOUT
30,52,0.001,${facthresha},46,${RMAG},${LCOLOR} !IKX,IKY,EPS,FACTOR,TLTAXIS,RMAG,LCOLOR
${imagename},UNBEND1,${date},${imagename} PASS 1
SCRATCH/${imagename}_unbend1_notap.mrc
UNBEND1,${date}
eot
  #
endif
#
cp -f SCRATCH/prof${imagename}.dat SCRATCH/prof${imagename}_unbend1.dat
echo "# IMAGE: SCRATCH/prof${imagename}_unbend1.dat <TXT: Distortion Profile Unbend1>" >> LOGS/${scriptname}.results
#
\rm -f fort.17
\mv -f CCPLOT.PS PS/${imagename}_ccunbend1.ps
echo "# IMAGE-IMPORTANT: PS/${imagename}_ccunbend1.ps <PS: Vector Plot for Unbending>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 73>"
#
#
#############################################################################
${proc_2dx}/${lincommand} "TAPEREDGE - to eliminate transform stripes"
#############################################################################
#
\rm -f     SCRATCH/${imagename}_unbend1.mrc
setenv IN  SCRATCH/${imagename}_unbend1_notap.mrc
setenv OUT SCRATCH/${imagename}_unbend1.mrc
${bin_2dx}/2dx_taperedgek.exe << eot
30,30,100,30       ! IAVER,ISMOOTH,ITAPER
eot
#
\rm -f SCRATCH/${imagename}_unbend1_notap.mrc
# 
echo "# IMAGE: SCRATCH/${imagename}_unbend1.mrc <Image, unbent>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 76>"
#
#
#############################################################################
${proc_2dx}/${lincommand} "FFTRANS - to calculate FFT from image after unbending"
#############################################################################
#
\rm -f FFTIR/${imagename}_unbend1_fft.mrc
setenv IN  SCRATCH/${imagename}_unbend1.mrc
setenv OUT FFTIR/${imagename}_unbend1_fft.mrc
${bin_2dx}/2dx_fftrans.exe
#
echo "# IMAGE-IMPORTANT: FFTIR/${imagename}_unbend1_fft.mrc <Image, unbent (FFT)>" >> LOGS/${scriptname}.results
#
echo "<<@progress: 79>"
echo "<<@evaluate>>"
#
#############################################################################
${proc_2dx}/${lincommand} "MMBOX - no resolution limitation for data extraction"
#############################################################################
#
\rm -f SCRATCH/TMP9871.dat APH/${imagename}_nolimit.aph
#
${bin_2dx}/2dx_mmboxa.exe << eot
FFTIR/${imagename}_unbend1_fft.mrc
  ${imagenumber}     ${imagename} ${date}, PASS 1
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2 2 0 50 50 19 19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_nolimit.aph
SCRATCH/TMP9871.dat
U1
${refposix} ${refposiy}           ! XORIG,YORIG
${RESMIN} 1.5 1 ${realcell} ${ALAT} ${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot
#
\rm -f SCRATCH/TMP9871.dat
#
echo "<<@progress: 82>"
#
#############################################################################
${proc_2dx}/${lincommand} "MMBOX - resolution limitation for diagnostics"
#############################################################################
#
\rm -f SCRATCH/TMP9872.dat APH/${imagename}_limit.aph
#
${bin_2dx}/2dx_mmboxa.exe << eot
FFTIR/${imagename}_unbend1_fft.mrc
  ${imagenumber}     ${imagename} ${date}, PASS 1
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2 2 0 50 50 19 19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_limit.aph
SCRATCH/TMP9872.dat
U1
${refposix} ${refposiy}           ! XORIG,YORIG
${RESMIN} ${RESMAX} 1 ${realcell} ${ALAT} ${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot
#
echo "# IQSTAT-RESLIM:"
cat SCRATCH/TMP9872.dat  
if ( ${final_round} == "y" ) then
  cat SCRATCH/TMP9872.dat >> LOGS/${scriptname}.results 
endif
source SCRATCH/TMP9872.dat
#
set IQS = `echo ${U1_IQ1} ${U1_IQ2} ${U1_IQ3} ${U1_IQ4} ${U1_IQ5} ${U1_IQ6} ${U1_IQ7} ${U1_IQ8} ${U1_IQ9}`
echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
echo "::maska=${maska}, boxa1=${boxa1}: QVal1= ${QVAL_local} ... IQ stat = ${IQS}"
echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
#
echo " " >> History.dat
echo "::_________________________________________________________________________________" >> History.dat
echo ":Date: ${date}" >> History.dat
echo "::Unbend 1: maska=${maska}, boxa1=${boxa1}: QVal= ${QVAL_local} ... IQ stat = ${IQS}" >> History.dat
#
echo "set QVal1 = ${QVAL_local}" >> LOGS/${scriptname}.results
echo "set U1_QVAL = ${QVAL_local}" >> LOGS/${scriptname}.results
echo "set U1_IQs = ${IQS}" >> LOGS/${scriptname}.results
#
\rm -f SCRATCH/TMP9872.dat
#
echo "<<@progress: 85>"
echo "<<@evaluate>>"
#
#
