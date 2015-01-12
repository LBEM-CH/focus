#
#  2dx_unbend2_sub.com
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
if ( ${scriptname} == '2dx_unbend2' ) then
  set lincommand = "linblock"
  echo ":: "
  #################################################################################
  ${proc_2dx}/linblock "Unbend 2"
  #################################################################################
  echo ":: "
  #
else
  set lincommand = "lin"
endif
#
${proc_2dx}/${lincommand} "2dx_unbend2_sub.com: Starting round ${locround}..."  
#
#############################################################################
#   MASKTRANA - to mask the FFT in order to create the reference map.       #
#   TTMASK    - same, but with TTF-convolution.                             #
#############################################################################  
  #
  if ( ${ctf_ttf} == 'CTF' ) then
    #
    \rm -f SCRATCH/${iname}_unbend2_fft_msk.mrc
    #
    setenv IN  FFTIR/${iname}_unbend2_fft.mrc
    setenv OUT SCRATCH/${iname}_unbend2_fft_msk.mrc
    setenv SPOTS ${nonmaskimagename}.spt
    ${proc_2dx}/${lincommand} "MASKTRANA - to mask the FFT in order to create the reference map"
    #
    echo 2 T T F	
    echo 1	
    echo ${lattice},-50,50,-50,50,${rmax},1 
    #
    ${bin_2dx}/2dx_masktrana.exe << eot
2 T T F				! ISHAPE= 1(CIRC),2(GAUSS CIRC),OR 3(RECT) HOLE, IAMPLIMIT(T or F)
${holeb}			! RADIUS OF HOLE IF CIRCULAR, X,Y HALF-EDGE-LENGTHS IF RECT.
${lattice},-50,50,-50,50,${rmax},1 !A/BX/Y,IH/IKMN/MX,RMAX,ITYPE
eot
    #
    echo "<<@progress: 10>>"
    echo "2dx_masktrana.exe finished."
    #
    if ( ${locround} == '1' ) then
      echo "# IMAGE-IMPORTANT: FFTIR/${iname}_unbend2_fft.mrc <Unbent image (FFT)>" >> LOGS/${scriptname}.results
    endif
    #
  else
    #
    ${proc_2dx}/${lincommand} "using holeb of ${holeb}"
    #
    \cp -f FFTIR/${iname}_unbend2_fft.mrc SCRATCH/${iname}_unbend2_fft_msk.mrc
    #
    \rm -f TMP234439.dat
    #
    setenv INOUT  SCRATCH/${iname}_unbend2_fft_msk.mrc
    #
    ${proc_2dx}/${lincommand} "TTMASK - to mask the FFT with TTFcor for reference"
    #
    echo "Using parameters:"
    echo ${imagesidelength},${imagesidelength},${stepdigitizer},${magnification},${CS},${KV}
    echo ${defocus},${TLTAXIS},${TLTANG}
    echo 1                          
    echo ${holeb},${holeb}       
    echo ${lattice},-50,50,-50,50,${rmax},1,8 
    echo TMP234439.dat
    #
    ${bin_2dx}/2dx_ttmask.exe << eot
${imagesidelength},${imagesidelength},${stepdigitizer},${magnification},${CS},${KV}
${defocus},${TLTAXIS},${TLTANG}   	! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
1                          		! ISHAPE= 1(circ),2(gauss circ),3(rect)
${holeb},${holeb}       		! radius hole if circular, X,Y half-edge-len if rect
${lattice},-50,50,-50,50,${rmax},1,8   	! A/BX/Y,IH/IKMN/MX,RMAX,ITYPE,NUMSPOT
TMP234439.dat
`cat ${nonmaskimagename}.spt`
eot
    #
    echo "<<@progress: 10>>"
  endif
  #
  #############################################################################
  ${proc_2dx}/${lincommand} "FFTTRANS - to calculate real-space image of masked FFT"     
  ############################################################################# 
  #
  \rm -f SCRATCH/${iname}_unbend2_fft_msk_fft.mrc
  setenv IN  SCRATCH/${iname}_unbend2_fft_msk.mrc
  setenv OUT SCRATCH/${iname}_unbend2_fft_msk_fft.mrc
  ${bin_2dx}/2dx_fftrans.exe 
  #
  if ( ${locround} == '1' ) then
    echo "# IMAGE: SCRATCH/${iname}_unbend2_fft_msk_fft.mrc <Unbent image, Fourier filtered>" >> LOGS/${scriptname}.results
  endif
  if ( ${tempkeep} != 'y' ) then
    \rm -f SCRATCH/${iname}_unbend2_fft_msk.mrc
  endif
  #
  echo "<<@progress: 15>>"
  #
  #
  #############################################################################
  ${proc_2dx}/${lincommand} "LABEL - to cut out small box in center of reference image (1)"
  #############################################################################  
  #
  \rm -f SCRATCH/${iname}_unbend2_fft_msk_fft_cro.mrc
  echo "patlabel = ${patlabel}"
  ${bin_2dx}/labelh.exe << eot
SCRATCH/${iname}_unbend2_fft_msk_fft.mrc
1
SCRATCH/${iname}_unbend2_fft_msk_fft_cro.mrc
${patlabel}
eot
  echo "<<@progress: 25>>"
  #
  #############################################################################
  ${proc_2dx}/${lincommand} "AUTOCORRL - to calculate autocorrelation of reference center (1)"
  #############################################################################  
  #
  \rm -f SCRATCH/${iname}_unbend2_fft_msk_fft_cro_aut.mrc
  setenv IN  SCRATCH/${iname}_unbend2_fft_msk_fft_cro.mrc
  setenv OUT SCRATCH/${iname}_unbend2_fft_msk_fft_cro_aut.mrc
  ${bin_2dx}/autocorrl.exe << eot
20
eot
  #
  if ( ${locround} == '1' ) then
    echo "# IMAGE: SCRATCH/${iname}_unbend2_fft_msk_fft_cro_aut.mrc <Autocorrelation Function of Reference>" >> LOGS/${scriptname}.results 
  endif
  #
  if ( ${tempkeep} != 'y' ) then
    \rm -f SCRATCH/${iname}_unbend2_fft_msk_fft_cro.mrc
  endif
  #
  #
  #############################################################################
  ${proc_2dx}/${lincommand} "LABEL - to extract autocorrelation peak"
  #############################################################################
  #
  \rm -f SCRATCH/${iname}_unbend2_fft_msk_fft_cro_aut_cro.mrc
  ${bin_2dx}/labelh.exe << eot
SCRATCH/${iname}_unbend2_fft_msk_fft_cro_aut.mrc
1
SCRATCH/${iname}_unbend2_fft_msk_fft_cro_aut_cro.mrc
210,310,210,310
eot
  #
  echo "<<@progress: 30>>"
  if ( ${locround} == '1' ) then
    echo "# IMAGE: SCRATCH/${iname}_unbend2_fft_msk_fft_cro_aut.mrc <Autocorrelation of Reference>" >> LOGS/${scriptname}.results
    echo "# IMAGE: SCRATCH/${iname}_unbend2_fft_msk_fft_cro_aut_cro.mrc <Autocorrelation of Reference, center>" >> LOGS/${scriptname}.results 
  endif
  #
  #############################################################################
  ${proc_2dx}/${lincommand} "BOXIMAGE - to box out reference for cross-correlation"
  #############################################################################
  #
  \rm -f SCRATCH/${iname}_unbend2_fft_msk_fft_box1.mrc
  setenv IN  SCRATCH/${iname}_unbend2_fft_msk_fft.mrc
  setenv OUT SCRATCH/${iname}_unbend2_fft_msk_fft_box1.mrc
  ${bin_2dx}/2dx_boximage.exe << eot
-1 0		!  NOVERT, VERTEX COORDS GIVEN IN GRID UNITS RELATIVE TO (0,0) ORIGIN.
0 0		! ORIGIN FOR LATER USE (E.G. IN FOURIER TRANSFORM)
${refposix} ${refposiy}     ! VERTEX COORDINATES IN GRID STEPS WRT CORNER (0,0)
${boxb1}
eot
  #
  echo "<<@progress: 35>>"
  set centergravity = `cat boximage.tmp`
  ${proc_2dx}/${lincommand} "Center of gravity of the reference is ${centergravity}"
  echo "This can be used to check where the current reference was taken"
  echo "==============================================================="
  \rm boximage.tmp
  #
  if ( ${locround} == '1' ) then
    echo "# IMAGE: SCRATCH/${iname}_unbend2_fft_msk_fft_box1.mrc <Reference 1>" >> LOGS/${scriptname}.results 
  endif
  #
  if ( ${treatspotscan} == 'y' ) then
    #
    \rm -f SCRATCH/${iname}_unbend2_fft_msk_fft_box2.mrc
    setenv IN  SCRATCH/${iname}_unbend2_fft_msk_fft.mrc
    setenv OUT SCRATCH/${iname}_unbend2_fft_msk_fft_box2.mrc
    ${proc_2dx}/${lincommand} "BOXIMAGE - again, here for SpotScan treatment"
    ${bin_2dx}/2dx_boximage.exe << eot
-1 0             !  NOVERT, VERTEX COORDS GIVEN IN GRID UNITS RELATIVE TO (0,0) ORIGIN.
0 0             ! ORIGIN FOR LATER USE (E.G. IN FOURIER TRANSFORM)
${refposix} ${refposiy}     ! VERTEX COORDINATES IN GRID STEPS WRT CORNER (0,0)
${boxb2}
eot
    #
    set centergravity = `cat boximage.tmp`
    ${proc_2dx}/lin "Center of gravity is ${centergravity}"
    ${proc_2dx}/lin "This can be used to check where the current reference was taken"
    \rm boximage.tmp
    #
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/${iname}_unbend2_fft_msk_fft_box2.mrc <Reference 2>" >> LOGS/${scriptname}.results 
    endif
    #
  endif
  echo "<<@progress: 40>>"
  #
  if ( ${tempkeep} != 'y' ) then
    \rm -f SCRATCH/${iname}_unbend2_fft_msk_fft.mrc
  endif
  #
  #############################################################################
  ${proc_2dx}/${lincommand} "FFTRANS - to calculate FFT from reference patch"
  #############################################################################  
  #
  \rm -f SCRATCH/${iname}_unbend2_fft_msk_fft_box1_fft.mrc
  setenv IN  SCRATCH/${iname}_unbend2_fft_msk_fft_box1.mrc
  setenv OUT SCRATCH/${iname}_unbend2_fft_msk_fft_box1_fft.mrc
  ${bin_2dx}/2dx_fftrans.exe
  #
  if ( ${treatspotscan} == 'y' ) then
    #
    \rm -f SCRATCH/${iname}_unbend2_fft_msk_fft_box2_fft.mrc
    setenv IN  SCRATCH/${iname}_unbend2_fft_msk_fft_box2.mrc
    setenv OUT SCRATCH/${iname}_unbend2_fft_msk_fft_box2_fft.mrc
    ${bin_2dx}/2dx_fftrans.exe
    #
  endif
  #
  echo "<<@progress: 45>>"
  #
  #############################################################################
  # MASKTRANA - to mask the FFT of the image                                  #
  # TTMASK    - to mask the FFT of the image with TTF-correction              #
  #############################################################################
  #
  if ( ${locround} == '1' ) then
    set maskb = ${maskb01}
  else if ( ${locround} == '2' ) then
    set maskb = ${maskb02}
  else if ( ${locround} == '3' ) then
    set maskb = ${maskb03}
  else if ( ${locround} == '4' ) then
    set maskb = ${maskb04}
  else if ( ${locround} == '5' ) then
    set maskb = ${maskb05}
  endif
  #
  ${proc_2dx}/${lincommand} "using maskb of ${maskb} in round ${locround}"
  #
  if ( ${ctf_ttf} == 'CTF' ) then
    \rm -f FFTIR/${iname}_fft_msk.mrc
    \rm -f SCRATCH/${iname}_fft_msk.mrc
    setenv IN  FFTIR/${iname}_fft.mrc
    setenv OUT SCRATCH/${iname}_fft_msk.mrc
    setenv SPOTS ${nonmaskimagename}.spt
    #
    ${proc_2dx}/${lincommand} "MASKTRANA - to mask the FFT of the image"
    ${bin_2dx}/2dx_masktrana.exe << eot
1 T T F	! ISHAPE=1(CIRC),2(GAUSCIR),3(RECT)HOLE,IAMPLIMIT(T or F),ISPOT,IFIL
${maskb}       ! RADIUS OF HOLE IF CIRCULAR, X,Y HALF-EDGE-LENGTHS IF RECT.
${lattice},-50,50,-50,50,${rmax},1 !A/BX/Y,IH/IKMN/MX,RMAX,ITYPE
eot
    #
  else
    #
    \cp -f FFTIR/${iname}_fft.mrc SCRATCH/${iname}_fft_msk.mrc
    \rm -f TMP234440.dat 
    setenv INOUT  SCRATCH/${iname}_fft_msk.mrc
    #
    if ( ${ttfcorfirst} == 'n' ) then
      ${proc_2dx}/${lincommand} "TTMASK - to mask the FFT of the image with TTF-correction"
      ${proc_2dx}/${lincommand} "-------- only the spots of the SpotList are treated"
      ${bin_2dx}/2dx_ttmask.exe << eot
${imagesidelength},${imagesidelength},${stepdigitizer},${magnification},${CS},${KV}
${defocus},${TLTAXIS},${TLTANG}   	! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
1       				! ISHAPE= 1(circ),2(gauss circ),3(rect),4(circ,maxrad)
${maskb},${maskb} 			! radius hole if circular, X,Y half-edge-len if rect
${lattice},-50,50,-50,50,${rmax},1,8 	! A/BX/Y,IH/IKMN/MX,RMAX,ITYPE,NUMSPOT
TMP234440.dat
`cat ${nonmaskimagename}.spt`
eot
      #
    else
      #
      ${proc_2dx}/${lincommand} "TTMASK - to mask the FFT of the image with TTF-correction"
      ${proc_2dx}/${lincommand} "-------- The entire image has to be treated in order not to loose data"
      #
      # ISHAPE = 4 is a new option, which uses the hard-edge circular mask, and automatically
      # calculates the maximally possible radius for the masking spots, so that almost no 
      # spot masking occurs. This is used to TTF correct the entire image, before the unbending.
      #
      ${bin_2dx}/2dx_ttmask.exe << eot
${imagesidelength},${imagesidelength},${stepdigitizer},${magnification},${CS},${KV}
${defocus},${TLTAXIS},${TLTANG}         ! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
4                                       ! ISHAPE= 1(circ),2(gauss circ),3(rect),4(circ,maxrad)
${maskb},${maskb}                       ! radius hole if circular, X,Y half-edge-len if rect
${lattice},-50,50,-50,50,${rmax},0,8       ! A/BX/Y,IH/IKMN/MX,RMAX,ITYPE,NUMSPOT
TMP234440.dat
eot
      #
    endif
    #
  endif
  #
  if ( ${locround} == '1' ) then
    echo "# IMAGE: SCRATCH/${iname}_fft_msk.mrc <maskb-Masked FFT of Image>" >> LOGS/${scriptname}.results 
  endif
  echo "<<@progress: 55>>"
  #
  #############################################################################
  #  FFTRANS - to calculate image after TTF correction                        #
  #############################################################################
  #
  if ( ${ttfcorfirst} == 'y' ) then
    ${proc_2dx}/${lincommand} "FFTRANS - to calculate image after TTF correction"
    \rm -f     SCRATCH/${iname}_fft_msk_fft.mrc
    setenv IN  SCRATCH/${iname}_fft_msk.mrc
    setenv OUT SCRATCH/${iname}_fft_msk_fft.mrc
    ${bin_2dx}/2dx_fftrans.exe
    #
    ${proc_2dx}/${lincommand} "LABEL - to produce BYTE format of TTF corrected image"
    \rm -f     SCRATCH/${iname}_ttf.mrc
    ${bin_2dx}/labelh.exe << eot
SCRATCH/${iname}_fft_msk_fft.mrc
-3
SCRATCH/${iname}_ttf.mrc
eot
    \rm -f SCRATCH/${iname}_fft_msk_fft.mrc
    # 
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/${iname}_ttf.mrc <Image after TTF correction>" >> LOGS/${scriptname}.results 
    endif
    #
  endif
  #
  echo "<<@progress: 60>>"
  #
  #############################################################################
  ${proc_2dx}/${lincommand} "TWOFILE - to calculate cross-correlation between reference and image"
  #############################################################################
  #
  #  Multiply two files together    :    FILE1 * Complex Conjugate of FILE2
  #
  \rm -f SCRATCH/${iname}_CCmap21_fft.mrc
  setenv IN1 SCRATCH/${iname}_fft_msk.mrc
  setenv IN2 SCRATCH/${iname}_unbend2_fft_msk_fft_box1_fft.mrc
  setenv OUT SCRATCH/${iname}_CCmap21_fft.mrc
  ${bin_2dx}/twofile.exe << eot
2		! ICOMB = 2
2 0 0 ${refposix} ${refposiy} ! IORIGIN,OXA,OYA,OXB,OYB  Origin shifts to FFTs
eot
  #
  if ( ${treatspotscan} == 'y' ) then
    #
    \rm -f SCRATCH/${iname}_CCmap22_fft.mrc
    setenv IN1 SCRATCH/${iname}_fft_msk.mrc
    setenv IN2 SCRATCH/${iname}_unbend2_fft_msk_fft_box2_fft.mrc
    setenv OUT SCRATCH/${iname}_CCmap22_fft.mrc
    ${proc_2dx}/${lincommand} "TWOFILE - again, second for SpotScan treatment"
    ${bin_2dx}/twofile.exe << eot
2               ! ICOMB = 2
2 0 0 ${refposix} ${refposiy} ! IORIGIN,OXA,OYA,OXB,OYB  Origin shifts to FFT's.
eot
    #
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/${iname}_CCmap22_fft.mrc <CCmap with Reference 2 (FFT)>" >> LOGS/${scriptname}.results 
    endif
    #
  endif
  #
  if ( ! -e SCRATCH/${iname}_CCmap21_fft.mrc ) then
    ${proc_2dx}/protest "ERROR: SCRATCH/${iname}_CCmap21_fft.mrc does not exist."
  endif
  #
  \rm -f SCRATCH/${iname}_fft_msk.mrc
  \rm SCRATCH/${iname}_unbend2_fft_msk_fft_box1_fft.mrc
  \rm -f SCRATCH/${iname}_unbend2_fft_msk_fft_box2_fft.mrc
  echo "<<@progress: 65>>"
  #
  #############################################################################
  ${proc_2dx}/${lincommand} "FFTTRANS - to calculate cross-correlation map"
  #############################################################################
  #
  /bin/rm -f SCRATCH/${iname}_CCmap21.mrc
  #
  setenv IN  SCRATCH/${iname}_CCmap21_fft.mrc
  setenv OUT SCRATCH/${iname}_CCmap21.mrc
  ${bin_2dx}/2dx_fftrans.exe
  #
  if ( ${locround} == '1' ) then
    echo "# IMAGE: SCRATCH/${iname}_CCmap21.mrc <CCmap with Reference 1>" >> LOGS/${scriptname}.results 
  endif
  if ( ${tempkeep} == 'n' ) then
    \rm -f SCRATCH/${iname}_CCmap21_fft.mrc
  endif
  #
  if ( ${treatspotscan} == 'y' ) then
    #
    \rm -f SCRATCH/${iname}_CCmap22.mrc
    #
    setenv IN  SCRATCH/${iname}_CCmap22_fft.mrc
    setenv OUT SCRATCH/${iname}_CCmap22.mrc
    ${proc_2dx}/${lincommand} "FFTTRANS - again, for SpotScan treatment"
    ${bin_2dx}/2dx_fftrans.exe
    #
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/${iname}_CCmap22.mrc <CCmap with Reference 2>" >> LOGS/${scriptname}.results 
    endif
    \rm -f SCRATCH/${iname}_CCmap22_fft.mrc
    #
  endif
  echo "<<@progress: 70>>"
  #
  #############################################################################
  ${proc_2dx}/${lincommand} "QUADSERCH - to search cross-correlation map for peaks"
  ${proc_2dx}/${lincommand} "with IPASS=1 to find first ERROR field"
  #############################################################################
  #
  \rm -f SCRATCH/errors${iname}.dat
  \rm -f ${iname}_profile.dat
  setenv PROFILE  SCRATCH/${iname}_unbend2_fft_msk_fft_cro_aut_cro.mrc
  setenv PROFDATA ${iname}_profile.dat
  setenv ERRORS   SCRATCH/errors${iname}.dat
  #
  if ( ${treatspotscan} == 'n' ) then
    set cormap = SCRATCH/${iname}_CCmap21.mrc
  else
    set cormap = SCRATCH/${iname}_CCmap22.mrc
  endif
  #
  \rm -f TMP_quadserch_1.mrc
  \rm -f TMP_quadserch_2.mrc
  \rm -f TMP_quadserch_3.mrc
  \rm -f TMP_quadserch_4.mrc
  \rm -f TMP_quadserch_5.mrc
  \rm -f TMP_quadserch_6.mrc
  \rm -f TMP_quadserch_7.mrc
  #
  echo quadpredb=${quadpredb}
  echo cormap=${cormap}
  echo imagesidelength=${imagesidelength}
  echo lattice=${lattice}
  echo quadradbx=${quadradbx}
  echo quadradby=${quadradby}
  echo refposi=${refposix},${refposiy}
  echo radlim=${radlim}
  echo treatspotscan=${treatspotscan}
  #
  \rm -f CCPLOT.PS
  \rm -f SPIDERCOORD.spi
  #
  ${bin_2dx}/2dx_quadserchk-2.exe << eot
1,${quadpredb}			! IPASS,NRANGE
${cormap}
${imagesidelength},${imagesidelength}     ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F			! Lattice vectors
-200,200,-200,200               ! NUMBER UNIT CELLS TO SEARCH
${quadradbx},${quadradby}	! RADIUS OF CORR SEARCH
${refposix} ${refposiy}		! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N				! YES/NO FOR DETAILED PRINTOUT
${radlim}                       ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}          ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
0                               ! dont create manual Masking information
0                               ! Dont mask the image directly
eot
  #
  \mv -f CCPLOT.PS PS/${iname}_quadserch2a.ps
  if ( ${locround} == '1' ) then
    echo "# IMAGE-IMPORTANT: PS/${iname}_quadserch2a.ps <PS: Vector Plot of Distortion, Pass 1>" >> LOGS/${scriptname}.results 
  endif
  echo "<<@progress: 75>>"
  echo "<<@evaluate>>"
  #
  #############################################################################
  ${proc_2dx}/${lincommand} "QUADSERCH - to search cross-correlation map for peaks"
  ${proc_2dx}/${lincommand} "with IPASS=3 to transform ERROR field into refined ERROUT field."
  #############################################################################
  #
  \rm -f SCRATCH/errout${iname}.dat
  \rm -f ${iname}_profile.dat
  setenv PROFILE  SCRATCH/${iname}_unbend2_fft_msk_fft_cro_aut_cro.mrc
  setenv PROFDATA ${iname}_profile.dat
  setenv ERRORS   SCRATCH/errors${iname}.dat
  setenv ERROUT   SCRATCH/errout${iname}.dat
  #
  if ( ${domask} == 'n' || ${treatspotscan} == 'y' ) then
    #
    if ( ${treatspotscan} == 'y' ) then
      ${proc_2dx}/lin "This should find positions of SpotScan spots."
      ${proc_2dx}/lin "="
    endif
    #
    if ( ${createmaskinfo} == 'y' ) then
      set createmask = '1'
      \rm -f ManualMasking_CCmap.mrc
      \rm -f ManualMasking_UnbendPlot.mrc
      ${proc_2dx}/linblock "Creating info for manual masking"
    else
      set createmask = '0'
    endif
    #
    \rm -f SPIDERCOORD.spi
    #
    ${bin_2dx}/2dx_quadserchk-2.exe << eot
3,${quadpredb}                     ! IPASS,NRANGE
${cormap}
${imagesidelength},${imagesidelength}     		! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F                       ! Lattice vectors
-200,200,-200,200               ! NUMBER UNIT CELLS TO SEARCH
${quadradbx},${quadradby}           ! RADIUS OF CORR SEARCH
${refposix},${refposiy}           ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N                               ! YES/NO FOR DETAILED PRINTOUT
${radlim}                       ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}          ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
${createmask}                   ! create manual Masking information (0=n,1=y)
0                               ! Dont mask the image directly
eot
    #
    if ( ${createmaskinfo} == 'y' ) then
      echo "# IMAGE-IMPORTANT: ManualMasking_CCmap.mrc <CCmap for Manual Masking>" >> LOGS/${scriptname}.results
      echo "# IMAGE-IMPORTANT: ManualMasking_UnbendPlot.mrc <Distortion Plot for Manual Masking>" >> LOGS/${scriptname}.results
    endif
    #
  else
    #
    ${proc_2dx}/${lincommand} "attention, masking image (1)"
    #
    \rm -f m${iname}.mrc
    \rm -f TMP_quadserch_1.mrc
    \rm -f TMP_quadserch_2.mrc
    \rm -f TMP_quadserch_3.mrc
    \rm -f TMP_quadserch_4.mrc
    \rm -f TMP_quadserch_5.mrc
    \rm -f TMP_quadserch_6.mrc
    \rm -f TMP_quadserch_7.mrc
    #
    \rm -f CCPLOT.PS
    if ( ${createmaskinfo} == 'y' ) then
      set createmask = '1'
      \rm -f ManualMasking_CCmap.mrc
      \rm -f ManualMasking_UnbendPlot.mrc
      ${proc_2dx}/linblock "Creating info for manual masking"
    else
      set createmask = '0'
    endif
    #
    \rm -f m${nonmaskimagename}.mrc
    \rm -f SPIDERCOORD.spi
    #
    ${bin_2dx}/2dx_quadserchk-2.exe << eot
3,${quadpredb}                     ! IPASS,NRANGE
${cormap}
${imagesidelength},${imagesidelength}     ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F                       ! Lattice vectors
-200,200,-200,200               ! NUMBER UNIT CELLS TO SEARCH
${quadradbx},${quadradby}           ! RADIUS OF CORR SEARCH
${refposix},${refposiy}           ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N                               ! YES/NO FOR DETAILED PRINTOUT
${radlim}                       ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}          ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
${createmask}			! dont create manual Masking information
1                               ! Mask the image directly
${nonmaskimagename}.mrc
m${nonmaskimagename}.mrc
256       
0
eot
    #
    if ( -e TMP_quadserch_6.mrc ) then
      \mv -f TMP_quadserch_7.mrc ${nonmaskimagename}_automask.mrc
      if ( ${tempkeep} == 'y' ) then
        echo "# IMAGE-IMPORTANT: TMP_quadserch_1.mrc <Masking file 1>" >> LOGS/${scriptname}.results 
        echo "# IMAGE-IMPORTANT: TMP_quadserch_2.mrc <Masking file 2>" >> LOGS/${scriptname}.results 
        echo "# IMAGE-IMPORTANT: TMP_quadserch_3.mrc <Masking file 3>" >> LOGS/${scriptname}.results 
        echo "# IMAGE-IMPORTANT: TMP_quadserch_4.mrc <Masking file 4>" >> LOGS/${scriptname}.results 
        echo "# IMAGE-IMPORTANT: TMP_quadserch_5.mrc <Masking file 5>" >> LOGS/${scriptname}.results 
        echo "# IMAGE-IMPORTANT: TMP_quadserch_6.mrc <Masking file 6>" >> LOGS/${scriptname}.results 
      else
        \rm -f TMP_quadserch_1.mrc
        \rm -f TMP_quadserch_2.mrc
        \rm -f TMP_quadserch_3.mrc
        \rm -f TMP_quadserch_4.mrc
        \rm -f TMP_quadserch_5.mrc
      endif
      echo "# IMAGE-IMPORTANT: ${nonmaskimagename}_automask.mrc <Masking Filter from Automatic Masking>" >> LOGS/${scriptname}.results 
    else
      ${proc_2dx}/protest "unbend2: ERROR: TMP_quadserch_6.mrc does not exist."
    endif
  endif
  \mv -f CCPLOT.PS PS/${iname}_quadserch2b.ps
  if ( ${locround} == '1' ) then
    echo "# IMAGE-IMPORTANT: PS/${iname}_quadserch2b.ps <PS: Vector Plot of Distortion, Pass 2>" >> LOGS/${scriptname}.results 
  endif
  #
  echo "<<@progress: 80>>"
  if ( ${treatspotscan} == 'y' ) then
    #
    #############################################################################
    ${proc_2dx}/${lincommand} "QUADSERCH - to search cross-correlation map for peaks"
    ${proc_2dx}/${lincommand} "with IPASS=2 to read in better ERROUT field and find cc-peaks"
    ${proc_2dx}/lin "this should find lattice within SpotScan spots."
    #############################################################################
    #
    #
    \rm -f ${iname}_profile.dat
    setenv PROFILE  SCRATCH/${iname}_unbend2_fft_msk_fft_cro_aut_cro.mrc
    setenv PROFDATA ${iname}_profile.dat
    setenv ERRORS   SCRATCH/errout${iname}.dat
    #
    \rm -f CCPLOT.PS
    #
    if ( ${createmaskinfo} == 'y' ) then
      set createmask = '1'
      \rm -f ManualMasking_CCmap.mrc
      \rm -f ManualMasking_UnbendPlot.mrc
      ${proc_2dx}/linblock "Creating info for manual masking"
    else
      set createmask = '0'
    endif
    #
    if ( ${domask} == 'n' ) then
      #
      \rm -f SPIDERCOORD.spi
      #
      ${bin_2dx}/2dx_quadserchk-2.exe << eot
2,${quadpredb}                     ! IPASS,NRANGE
SCRATCH/${iname}_CCmap21.mrc
${imagesidelength},${imagesidelength}     ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F                       ! Lattice vectors
-200,200,-200,200               ! NUMBER UNIT CELLS TO SEARCH
${quadradbx},${quadradby}           ! RADIUS OF CORR SEARCH
${refposix} ${refposiy}           ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N                               ! YES/NO FOR DETAILED PRINTOUT
${radlim}                       ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}          ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
${createmask}                   ! create manual Masking information (0=n,1=y)
0                               ! Dont mask the image directly
eot
      #
    else
      #
      ${proc_2dx}/${lincommand} "attention, masking image (2)"
      #
      \rm -f m${iname}.mrc  
      \rm -f TMP_quadserch_1.mrc
      \rm -f TMP_quadserch_2.mrc
      \rm -f TMP_quadserch_3.mrc
      \rm -f TMP_quadserch_4.mrc
      \rm -f TMP_quadserch_5.mrc
      \rm -f TMP_quadserch_6.mrc
      \rm -f TMP_quadserch_7.mrc
      \rm -f SPIDERCOORD.spi
      \rm -f m${nonmaskimagename}.mrc
      #
      ${bin_2dx}/2dx_quadserchk-2.exe << eot
2,${quadpredb}                     ! IPASS,NRANGE
SCRATCH/${iname}_CCmap21.mrc
${imagesidelength},${imagesidelength}     ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F                       ! Lattice vectors
-200,200,-200,200               ! NUMBER UNIT CELLS TO SEARCH
${quadradbx},${quadradby}           ! RADIUS OF CORR SEARCH
${refposix} ${refposiy}           ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N                               ! YES/NO FOR DETAILED PRINTOUT
${radlim}                       ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}                   ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
${createmask}                   ! create manual Masking information (0=n,1=y)
1                               ! Do mask the image directly
${nonmaskimagename}.mrc
m${nonmaskimagename}.mrc
-1
0
eot
      #
      if ( -e TMP_quadserch_6.mrc ) then
        \mv -f TMP_quadserch_7.mrc ${nonmaskimagename}_automask.mrc
        if ( ${locround} == '1' ) then
          echo "# IMAGE-IMPORTANT: ${nonmaskimagename}_automask.mrc <Masking Filter from Automatic Masking>" >> LOGS/${scriptname}.results 
        endif
      else
        ${proc_2dx}/protest "unbend2: ERROR: TMP_quadserch_6.mrc does not exist."
      endif
      #
    endif
    #
    if ( ${createmaskinfo} == 'y' ) then
      echo "# IMAGE-IMPORTANT: ManualMasking_CCmap.mrc <CCmap for Manual Masking>" >> LOGS/${scriptname}.results
      echo "# IMAGE-IMPORTANT: ManualMasking_UnbendPlot.mrc <Distortion Plot for Manual Masking>" >> LOGS/${scriptname}.results
    endif
    #
    \mv -f CCPLOT.PS PS/${iname}_quadserch2c.ps
    if ( ${locround} == '1' ) then
      echo "# IMAGE-IMPORTANT: PS/${iname}_quadserch2c.ps <PS: Vector Plot for Distortions, Pass 2>" >> LOGS/${scriptname}.results 
    endif
    #
    if ( ${tempkeep} != 'y' ) then
      \rm -f SCRATCH/${iname}_CCmap22.mrc
    endif
  endif
  #
  \cp -f SCRATCH/${iname}_CCmap21.mrc ${iname}_CCmap_unbend2.mrc
  #
  echo "<<@progress: 85>>"
  #
  #############################################################################
  ${proc_2dx}/${lincommand} "SPIDERCOORD.spi - Unit cell locations in SPIDER format."
  #############################################################################
  #
  \mv -f SPIDERCOORD.spi ${nonmaskimagename}-unitcells-spider.doc
  echo "# IMAGE: ${nonmaskimagename}-unitcells-spider.doc <SPIDER document with unit cell locations>" >> LOGS/${scriptname}.results
  #
  echo "# IMAGE: ${iname}_profile.dat <PROFILE with unit cell locations>" >> LOGS/${scriptname}.results
  #
  #
  if ( ${domask} == 'y' ) then
    echo "# IMAGE: ${nonmaskimagename}.mrc" >> LOGS/${scriptname}.results
    echo "# IMAGE: m${nonmaskimagename}.mrc" >> LOGS/${scriptname}.results
    set imagename = m${nonmaskimagename}
    echo "set imagename = ${imagename}" >> LOGS/${scriptname}.results
    #
    set locround = 1
    set domask = n
    echo "set domask = n" >> LOGS/${scriptname}.results
    set use_masked_image = y
    echo "set use_masked_image = ${use_masked_image}" >> LOGS/${scriptname}.results
    #
    echo "set MASKING_done = y" >> LOGS/${scriptname}.results
    #
    # echo "set FFT_done = n" >> LOGS/${scriptname}.results
    echo "set UNBENDING_done = n" >> LOGS/${scriptname}.results
    #  
    # Redo Masking, since QUADSERCH doesn't do it righ:
    echo "# IMAGE: ${nonmaskimagename}.mrc <Nonmasked image>" >> LOGS/${scriptname}.results
    echo "# IMAGE: ${imagename}.mrc <Image>" >> LOGS/${scriptname}.results
    echo "# IMAGE: ${nonmaskimagename}_automask.mrc <Masking info file>" >> LOGS/${scriptname}.results
    python ${proc_2dx}/movie/mask.py ${nonmaskimagename}.mrc ${imagename}.mrc ${nonmaskimagename}_automask.mrc
    #
    if (${ctfcor_imode}x == "0x" ) then
      ${proc_2dx}/linblock "Not applying any CTF correction to ${imagename}.mrc."
      \cp -f ${imagename}.mrc image_ctfcor.mrc
      echo "# IMAGE-IMPORTANT: image_ctfcor.mrc <Output Image CTF corrected>" >> LOGS/${scriptname}.results
    else
      #  
      #
      #################################################################################
      ${proc_2dx}/linblock "2dx_ctfcor - to apply CTF correction to the image ${imagename}.mrc"
      #################################################################################  
      #
      \rm -f image_ctfcor.mrc
      #
      setenv NCPUS ${Thread_Number}
      #
      ${bin_2dx}/2dx_ctfcor_stripes.exe << eot
${imagename}.mrc
image_ctfcor.mrc
#
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
      echo "# IMAGE-IMPORTANT: image_ctfcor.mrc <Output Image CTF corrected>" >> LOGS/${scriptname}.results
    endif
    #
    #
    #################################################################################
    ${proc_2dx}/linblock "Sourcing 2dx_fftrans_sub.com"
    #################################################################################
    #
    set generatePeriodogram = "n"
    set inimage = image_ctfcor
    source ${proc_2dx}/2dx_fftrans_sub.com
    #
    ${proc_2dx}/linblock "#"
    ${proc_2dx}/linblock "#"
    ${proc_2dx}/linblock "Image now masked, name was changed to m${nonmaskimagename}"
    ${proc_2dx}/linblock "The masked image was CTF corrected, and the FFTs calculated again."
    ${proc_2dx}/linblock "#"
    ${proc_2dx}/linblock "#"
    #
    #
  endif
  #
  #############################################################################
  # CCUNBEND - to unbend the image                                            #
  #############################################################################
  #
  \rm -f SCRATCH/${iname}_unbend2_notap.mrc
  setenv CCORDATA ${iname}_profile.dat
  \rm -f SCRATCH/ccunbend-table-${iname}.dat
  setenv TABLEOUT SCRATCH/ccunbend-table-${iname}.dat
  #
  echo facthresha=${facthresha}
  echo TLTAXIS=${TLTAXIS}
  echo iname=${iname}
  echo date=${date}
  #
  \rm -f CCPLOT.PS
  #
  # Hopefully, ITYPE=1 does not crash in NAGLIB ???
  # set ITYPE = 1
  set ITYPE = 0
  #
  set ROFFSET = 50.0
  set NNUM = 6
  #
  echo IMAXCOR = ${IMAXCOR}
  ${proc_2dx}/lin "Using IMAXCOR = ${IMAXCOR}"
  #
  \rm -f SCRATCH/${iname}_CCmap21_unbend2.mrc
  if ( ${ccunbend_program} == "1" ) then
    ${proc_2dx}/${lincommand} "2dx_ccunbendh - to unbend the CCmap (for verification only)"
    ${bin_2dx}/2dx_ccunbendh.exe << eot
SCRATCH/${iname}_CCmap21.mrc
${IMAXCOR},${ISTEP_h},${NNUM},${ROFFSET}	 !IMAXCOR,ISTEP,NNUM,ROFFSET
0.001,${facthresha},${RMAG} !EPS,FACTOR,RMAG
SCRATCH/${iname}_CCmap21_unbend2.mrc
UNBENT,PASS,2,${date}
eot
    #
  else
    #
    ${proc_2dx}/${lincommand} "2dx_ccunbendk - to unbend the CCmap (for verification only)"
    ${bin_2dx}/2dx_ccunbendk.exe << eot
SCRATCH/${iname}_CCmap21.mrc
${ITYPE},1,${IMAXCOR},${ISTEP},F,40,T                !ITYPE,IOUT,IMAXCOR,ISTEP,LTAPER,RTAPER,LTABOUT
50,72,0.001,${facthresha},${TLTAXIS},${RMAG},${LCOLOR}        !IKX,IKY,EPS,FACTOR,TLTAXIS,RMAG,LCOLOR
${iname}, UNBEND2, ${date}
SCRATCH/${iname}_CCmap21_unbend2.mrc
UNBENT,PASS,2,${date}
eot
    #
  endif
  echo "# IMAGE: SCRATCH/${iname}_CCmap21_unbend2.mrc <CCmap with Reference 1, unbent>" >> LOGS/${scriptname}.results 
  #
  \rm -f fort.17
  #
  if ( ${ttfcorfirst} == 'n' ) then
    #
    if ( ${ccunbend_program} == "1" ) then
      ${proc_2dx}/${lincommand} "2dx_ccunbendh - to unbend the original image"
      ${bin_2dx}/2dx_ccunbendh.exe << eot
${iname}.mrc
${IMAXCOR},${ISTEP_h},${NNUM},${ROFFSET}	 !IMAXCOR,ISTEP,NNUM,ROFFSET
0.001,${facthresha},${RMAG} !EPS,FACTOR,RMAG
SCRATCH/${iname}_unbend2_notap.mrc
UNBENT,PASS,2,${date}
eot
      #
    else
      #
      ${proc_2dx}/${lincommand} "2dx_ccunbendk - to unbend the original image"
      ${bin_2dx}/2dx_ccunbendk.exe << eot
${iname}.mrc
${ITYPE},1,${IMAXCOR},${ISTEP},F,40,T       !ITYPE,IOUT,IMAXCOR,ISTEP,LTAPER,RTAPER,LTABOUT
50,72,0.001,${facthresha},${TLTAXIS},${RMAG},${LCOLOR}     !IKX,IKY,EPS,FACTOR,TLTAXIS,RMAG,LCOLOR
${iname}, UNBEND2, ${date}
SCRATCH/${iname}_unbend2_notap.mrc
UNBENT,PASS,2,${date}
eot
      #
      \rm -f SCRATCH/${iname}_CCmap21_marked.mrc
      ${bin_2dx}/2dx_mark_spots.exe << eot
SCRATCH/${iname}_CCmap21.mrc
SCRATCH/${iname}_CCmap21_marked.mrc
${iname}_profile.dat
2
eot
      echo "# IMAGE: SCRATCH/${iname}_CCmap21_marked.mrc <CCmap, marked>" >> LOGS/${scriptname}.results 
      #
      \rm -f SCRATCH/${iname}_CCmap21_unbent.mrc
      ${bin_2dx}/2dx_ccunbendk.exe << eot
SCRATCH/${iname}_CCmap21.mrc
${ITYPE},1,${IMAXCOR},${ISTEP},F,40,T       !ITYPE,IOUT,IMAXCOR,ISTEP,LTAPER,RTAPER,LTABOUT
50,72,0.001,${facthresha},${TLTAXIS},${RMAG},${LCOLOR}     !IKX,IKY,EPS,FACTOR,TLTAXIS,RMAG,LCOLOR
${iname}, UNBEND2, ${date}
SCRATCH/${iname}_CCmap21_unbent.mrc
CCmap unbent
eot
      #
      echo "# IMAGE-IMPORTANT: SCRATCH/${iname}_CCmap21_unbent.mrc <CCmap, unbent>" >> LOGS/${scriptname}.results 
      #
    endif
    #
    \rm -f fort.17
    #
  else
    #
    if ( ${ccunbend_program} == "1" ) then
      ${proc_2dx}/${lincommand} "2dx_ccunbendh - to unbend the TTF-corrected image"
      ${bin_2dx}/2dx_ccunbendh.exe << eot
SCRATCH/${iname}_ttf.mrc
${IMAXCOR},${ISTEP_h},${NNUM},${ROFFSET}	 !IMAXCOR,ISTEP,NNUM,ROFFSET
0.001,${facthresha},${RMAG} !EPS,FACTOR,RMAG
SCRATCH/${iname}_unbend2_notap.mrc
UNBENT,PASS,2,${date}
eot
      #
    else
      #
      ${bin_2dx}/2dx_ccunbendk.exe << eot
SCRATCH/${iname}_ttf.mrc
${ITYPE},1,${IMAXCOR},${ISTEP},F,40,T                !ITYPE,IOUT,IMAXCOR,ISTEP,LTAPER,RTAPER,LTABOUT
50,72,0.001,${facthresha},${TLTAXIS},${RMAG},${LCOLOR}        !IKX,IKY,EPS,FACTOR,TLTAXIS,RMAG,LCOLOR
${iname}, UNBEND2, ${date}
SCRATCH/${iname}_unbend2_notap.mrc
UNBENT,PASS,2,${date}
eot
      #
    endif
    #
    \rm -f fort.17
    #
    if ( ${tempkeep} == 'n' ) then
      \rm SCRATCH/${iname}_ttf.mrc
    endif
    #
  endif
  #
  \mv -f CCPLOT.PS PS/${iname}_ccunbend2.ps
  if ( ${locround} == '1' ) then
    echo "# IMAGE-IMPORTANT: PS/${iname}_ccunbend2.ps <PS: Vector Plot for Unbending>"  >> LOGS/${scriptname}.results 
  endif
  #
  echo "<<@progress: 90>>"
  #
  #
  if ( ( ${ctfcor_imode} == "1" || ${ctfcor_imode} == "2" ) &&  ${final_round} == "y" ) then
    #############################################################################
    ${proc_2dx}/${lincommand} "apply_filter_Fourier.py - to perform Weiner filtration on unbent image"
    #############################################################################
    #
    python ${proc_2dx}/movie/apply_filter_Fourier.py SCRATCH/${iname}_unbend2_notap.mrc SCRATCH/2dx_ctfcor_ctffile.mrc SCRATCH/${iname}_unbend2_notap_ctf.mrc ${ctfcor_noise}
    if ( ${ctfcor_imode} == "2" ) then
      echo "# IMAGE: SCRATCH/2dx_ctfcor_ctffile.mrc <Summed CTF**2 file (for correction)>" >> LOGS/${scriptname}.results 
    else
      echo "# IMAGE: SCRATCH/2dx_ctfcor_ctffile.mrc <Summed CTF file (for correction)>" >> LOGS/${scriptname}.results 
    endif
    echo "# IMAGE: SCRATCH/${iname}_unbend2_notap_ctf.mrc <Unbent image, CTF-corrected>" >> LOGS/${scriptname}.results 
    #
  endif
  #
  #############################################################################
  ${proc_2dx}/${lincommand} "TAPEREDGE - to eliminate transform stripes"
  #############################################################################
  #
  \rm -f unbent.mrc
  #
  if ( ${ctfcor_imode} == "2"  &&  ${final_round} == "y" ) then
    setenv IN  SCRATCH/${iname}_unbend2_notap_ctf.mrc
  else
    setenv IN  SCRATCH/${iname}_unbend2_notap.mrc
  endif
  setenv OUT unbent.mrc
  ${bin_2dx}/2dx_taperedgek.exe << eot
30,30,100,30       ! IAVER,ISMOOTH,ITAPER
eot
  #
  if ( ${tempkeep} == 'n' ) then
    \rm -f SCRATCH/${iname}_unbend2_notap.mrc
  endif
  #
  #
  #############################################################################
  ${proc_2dx}/${lincommand} "FFTRANS - to calculate FFT from image after unbending"
  #############################################################################
  #
  \rm -f FFTIR/${iname}_unbend2_fft.mrc
  setenv IN  unbent.mrc
  setenv OUT FFTIR/${iname}_unbend2_fft.mrc
  ${bin_2dx}/2dx_fftrans.exe
  #
  if ( ${locround} == '1' ) then
    echo "# IMAGE: unbent.mrc <Unbent Image>" >> LOGS/${scriptname}.results 
  endif
  #
  if ( ${locround} == '1' ) then
    echo "# IMAGE-IMPORTANT: FFTIR/${iname}_unbend2_fft.mrc <FFT of Unbent and Edge-Tapered Image>" >> LOGS/${scriptname}.results 
  endif
  #
  echo "<<@progress: 93>>"
  #
  #############################################################################
  #  MMBOX - evaluate data for  merging                             #
  #  TTBOX (T-JOBB) - to read out amplitudes and phases from 2nd unbent image #
  #############################################################################
  #
  \rm -f SCRATCH/TMP9873.dat
  #
  if ( ${ctf_ttf} == 'CTF' && ${ttfcorfirst} == 'n' ) then
    #
    set inimage = FFTIR/${iname}_unbend2_fft.mrc
    #
    \rm -f APH/${imagename}_fou.aph
    #
    ${proc_2dx}/${lincommand} "MMBOXA - to read out AMPs and PHASES"
    ${bin_2dx}/2dx_mmboxa.exe << eot
${inimage}
${imagenumber} ${iname}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_fou.aph
SCRATCH/TMP9873.dat
U2
${refposix},${refposiy}           ! XORIG,YORIG
200.0,1.5,1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot
    #
    echo "# IMAGE-IMPORTANT: APH/${imagename}_fou.aph <APH File [H,K,A,P,IQ,Back,0]>" >> LOGS/${scriptname}.results
    #
  else
    #############################################################################
    #
    if ( ${ttfcorfirst} == 'n' ) then
      #
      \rm -f APH/${imagename}_fou_ttf.aph
      #
      ${proc_2dx}/${lincommand} "TTBOXA - to read out AMPs and PHASES with TTF-correction"
      ${bin_2dx}/2dx_ttboxk.exe << eot
FFTIR/${iname}_unbend2_fft.mrc
${imagenumber} ${iname}, Unbend2, ${date}
Y                        ! generate grid from lattice
N                        ! generate points from lattice
N                        ! list points as calculated
Y                        ! plot output
${imagesidelength},${imagesidelength},${stepdigitizer},${magnification},${CS},${KV} ! ISIZEX,ISIZEY,DSTEP,MAG,CS,KV
${defocus},${TLTAXIS},${TLTANG} ! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
2,0,50,50,19,19          ! IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_fou_ttf.aph
SCRATCH/TMP9873.dat
U2
200.0,3.0,${refposix},${refposiy},90.0 !RSMN,RSMX,XORIG,YORIG,SEGMNT
${lattice}                  ! reciprocal lattice vectors in pixels
eot
      #
      echo "# IMAGE-IMPORTANT: APH/${imagename}_fou_ttf.aph <APH File after TTF correction [H,K,A,P,IQ,Back,0])>" >> LOGS/${scriptname}.results
      #
      cd APH
      \rm -f ${imagename}_fou_ctf.aph
      \ln -s ${imagename}_fou_ttf.aph ${imagename}_fou_ctf.aph
      cd ..
      #
      \mv -f TTPLOT.PS PS/${iname}.ttplot_unbend2.ps
      if ( ${locround} == '1' ) then
        echo "# IMAGE-IMPORTANT: PS/${iname}.ttplot_unbend2.ps <PS: IQ Plot after TTF correction>" >> LOGS/${scriptname}.results 
      endif
      #
    else
      #
      #############################################################################
      #
      \rm -f APH/${imagename}_fou_ttf.aph
      #
      ${proc_2dx}/${lincommand} "MMBOXA - to read out AMPs and PHASES without TTF-correction"
      ${proc_2dx}/${lincommand} "         (because image is already TTF corrected)"
      ${bin_2dx}/2dx_mmboxa.exe << eot
FFTIR/${iname}_unbend2_fft.mrc
${imagenumber} ${iname}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_fou_ttf.aph
SCRATCH/TMP9873.dat
U2
${refposix},${refposiy}           ! XORIG,YORIG
200.0,1.5,1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot
      #
      echo "# IMAGE-IMPORTANT: APH/${imagename}_fou_ttf.aph <APH File after TTF correction [H,K,A,P,IQ,Back,0]>" >> LOGS/${scriptname}.results
      #
    endif
    #
    cd APH
    \rm -f ${imagename}_fou_ctf.aph
    \ln -s ${imagename}_fou_ttf.aph ${imagename}_fou_ctf.aph
    \rm -f ${imagename}_ttf.aph
    \ln -s ${imagename}_fou_ttf.aph ${imagename}_ttf.aph
    cd ..
    #
    echo "# IMAGE-IMPORTANT: APH/${imagename}_ctf.aph <Link to Final APH File [H,K,A,P,IQ,Back,0]>" >> LOGS/${scriptname}.results
    # 
  endif
  #
  echo "# MASKB: "${maskb}
  echo "# BOXB: "${boxb1}
  echo "# IQSTAT:"
  cat SCRATCH/TMP9873.dat
  #
  if ( ${final_round} == "y" ) then
    cat SCRATCH/TMP9873.dat >> LOGS/${scriptname}.results 
  endif
  source SCRATCH/TMP9873.dat
  #
  set IQS = `echo ${U2_IQ1} ${U2_IQ2} ${U2_IQ3} ${U2_IQ4} ${U2_IQ5} ${U2_IQ6} ${U2_IQ7} ${U2_IQ8} ${U2_IQ9}`
  echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  echo "::maskb=${maskb}, boxb1=${boxb1}: QVal2= ${QVAL_local} ... IQ stat = ${IQS}"
  echo ":++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  #
  echo " " >> History.dat
  echo ":Date: ${date}" >> History.dat
  echo "::Unbend 2: maskb=${maskb}, boxb1=${boxb1}: QVal= ${QVAL_local} ... IQ stat = ${IQS}" >> History.dat
  #
  echo "set QVAL = ${QVAL_local}" >> LOGS/${scriptname}.results
  echo "set QVal2 = ${QVAL_local}" >> LOGS/${scriptname}.results
  echo "set U2_QVAL = ${QVAL_local}" >> LOGS/${scriptname}.results
  echo "set U2_IQs = ${IQS}" >> LOGS/${scriptname}.results
  #
  \rm -f SCRATCH/TMP9873.dat  
  #
  echo "<<@progress: 96>>"  
  echo "<<@evaluate>>"
  #
  \rm -f SPIDERCOORD.spi
  #
  #

