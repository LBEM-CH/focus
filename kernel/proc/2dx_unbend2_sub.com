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
  ${proc_2dx}/linblock "Unbend 2: ${iname}"
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
#                                                                           #
#   MASKTRANA - to mask the FFT in order to create the reference map.       #
#   TTMASK    - same, but with TTF-convolution.                             #
#                                                                           #
#  ${iname}_spt + cor${iname}_fft.mrc => ref${iname}_fft_msk.mrc#
#                                                                           #
#############################################################################  
  #
  if ( ${ctf_ttf} == 'CTF' ) then
    #
    \rm -f SCRATCH/ref${iname}_fft_msk.mrc
    #
    setenv IN  FFTIR/cor${iname}_fft.mrc
    setenv OUT SCRATCH/ref${iname}_fft_msk.mrc
    setenv SPOTS ${imagename}.spt
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
    if ( ${tempkeep} == 'y' ) then
      \mv -f FFTIR/cor${iname}_fft.mrc SCRATCH/cor${iname}_fft_unbend2.mrc
      if ( ${locround} == '1' ) then
        echo "# IMAGE-IMPORTANT: FFTIR/cor${iname}_fft_unbend2.mrc <FFT of Unbent Image>" >> LOGS/${scriptname}.results
      endif
    else
      \rm -f FFTIR/cor${iname}_fft.mrc
    endif
    #
  else
    #
    ${proc_2dx}/${lincommand} "using holeb of ${holeb}"
    #
    \cp -f FFTIR/cor${iname}_fft.mrc SCRATCH/ref${iname}_fft_msk.mrc
    #
    \rm -f TMP234439.dat
    #
    setenv INOUT  SCRATCH/ref${iname}_fft_msk.mrc
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
`cat ${imagename}.spt`
eot
    #
    echo "<<@progress: 10>>"
  endif
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "FFTTRANS - to calculate real-space image of masked FFT"     
  #                                                                           #
  #                 ref${iname}_fft_msk.mrc  =>  ref${iname}_flt.mrc  #
  #                                                                           #
  ############################################################################# 
  #
  \rm -f SCRATCH/ref${iname}_flt.mrc
  setenv IN  SCRATCH/ref${iname}_fft_msk.mrc
  setenv OUT SCRATCH/ref${iname}_flt.mrc
  ${bin_2dx}/2dx_fftrans.exe 
  #
  if ( ${tempkeep} == 'y' ) then
    \mv -f SCRATCH/ref${iname}_fft_msk.mrc SCRATCH/ref${iname}_fft_msk_unbend2.mrc
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/ref${iname}_fft_msk_unbend2.mrc <FFT of Unbent Reference>" >> LOGS/${scriptname}.results
    endif
  else
    \rm -f SCRATCH/ref${iname}_fft_msk.mrc
  endif
  #
  echo "<<@progress: 15>>"
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "LABEL - to cut out a larger area from the centre of created reference"
  #           just for debugging.                                             #
  #                                                                           #
  #                ref${iname}_flt.mrc  =>  ${iname}_reference2.mrc   #
  #                                                                           #
  #############################################################################  
  # 
  echo boxlabel = ${boxlabel} 
  #
  if ( -e SCRATCH/${iname}_reference2.mrc ) then
    \rm -f SCRATCH/${iname}_reference2.mrc
  endif
  ${bin_2dx}/labelh.exe << eot
SCRATCH/ref${iname}_flt.mrc
1
SCRATCH/${iname}_reference2.mrc
${boxlabel}
eot
  #
  echo "<<@progress: 20>>"
echo "<<@evaluate>>"
  if ( ${locround} == '1' ) then
    echo "# IMAGE: SCRATCH/${iname}_reference2.mrc <Reference Patch>" >> LOGS/${scriptname}.results  
  endif
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "LABEL - to cut out small box in center of reference image"
  #                                                                           #
  #                     ref${iname}_flt.mrc  =>  box${iname}_flt.mrc  #
  #                                                                           #
  #############################################################################  
  #
  ${bin_2dx}/labelh.exe << eot
SCRATCH/ref${iname}_flt.mrc
1
SCRATCH/box${iname}_flt.mrc
${patlabel}
eot
  echo "<<@progress: 25>>"
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "AUTOCORRL - to calculate autocorrelation of reference center"
  #                                                                           #
  #                             box${iname}_flt  =>  auto${iname}_cor #
  #                                                                           #
  #############################################################################  
  #
  \rm -f SCRATCH/auto${iname}_cor.mrc
  setenv IN  SCRATCH/box${iname}_flt.mrc
  setenv OUT SCRATCH/auto${iname}_cor.mrc
  ${bin_2dx}/autocorrl.exe << eot
20
eot
  #
  if ( ${locround} == '1' ) then
    echo "# IMAGE: SCRATCH/auto${iname}_cor.mrc <Autocorrelation Function of Reference Patch>" >> LOGS/${scriptname}.results 
  endif
  #
  if ( ${tempkeep} == 'y' ) then
    \mv -f SCRATCH/box${iname}_flt.mrc SCRATCH/box${iname}_flt2.mrc
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/box${iname}_flt2.mrc <Center of Reference Patch>" >> LOGS/${scriptname}.results
    endif
  else
    \rm -f SCRATCH/box${iname}_flt.mrc
  endif
  #
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "LABEL - to extract autocorrelation peak"
  #                                                                           #
  #                            auto${iname}_cor  =>  auto${iname}_map #
  #                                                                           #
  #############################################################################
  #
  \rm -f SCRATCH/auto${iname}_map.mrc
  ${bin_2dx}/labelh.exe << eot
SCRATCH/auto${iname}_cor.mrc
1
SCRATCH/auto${iname}_map.mrc
210,310,210,310
eot
  #
  echo "<<@progress: 30>>"
  echo "<<@evaluate>>"
  \mv -f SCRATCH/auto${iname}_cor.mrc SCRATCH/auto${iname}_cor_unbend2.mrc
  if ( ${locround} == '1' ) then
    echo "# IMAGE: SCRATCH/auto${iname}_cor_unbend2.mrc <Autocorrelation Function of Reference Patch>" >> LOGS/${scriptname}.results
    echo "# IMAGE: SCRATCH/auto${iname}_map.mrc <Central Peak of Autocorrelation Function of Reference Patch>" >> LOGS/${scriptname}.results 
  endif
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "BOXIMAGE - to box out reference for cross-correlation"
  #                                                                           #
  #                             ref${iname}_flt  =>  refb${iname}_flt #
  #                                                                           #
  #############################################################################
  #
  \rm -f SCRATCH/refb1${iname}_flt.mrc
  setenv IN  SCRATCH/ref${iname}_flt.mrc
  setenv OUT SCRATCH/refb1${iname}_flt.mrc
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
    echo "# IMAGE: SCRATCH/refb1${iname}_flt.mrc <Reference 1>" >> LOGS/${scriptname}.results 
  endif
  #
  if ( ${treatspotscan} == 'y' ) then
    #
    \rm -f SCRATCH/refb2${iname}_flt.mrc
    setenv IN  SCRATCH/ref${iname}_flt.mrc
    setenv OUT SCRATCH/refb2${iname}_flt.mrc
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
      echo "# IMAGE: SCRATCH/refb2${iname}_flt.mrc <Reference 2>" >> LOGS/${scriptname}.results 
    endif
    #
  endif
  echo "<<@progress: 40>>"
  echo "<<@evaluate>>"
  #
  if ( ${tempkeep} == 'y' ) then
    \mv -f SCRATCH/ref${iname}_flt.mrc SCRATCH/ref${iname}_flt_unbend2.mrc
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/ref${iname}_flt_unbend2.mrc <Filtered Image for Reference generation>" >> LOGS/${scriptname}.results 
    endif
  else
    \rm -f SCRATCH/ref${iname}_flt.mrc
  endif
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "FFTRANS - to calculate FFT from reference patch"
  #                                                                           #
  #                             refb${iname}_flt  =>  ref${iname}_fft #
  #                                                                           #
  #############################################################################  
  #
  \rm -f SCRATCH/ref1${iname}_fft.mrc
  setenv IN  SCRATCH/refb1${iname}_flt.mrc
  setenv OUT SCRATCH/ref1${iname}_fft.mrc
  ${bin_2dx}/2dx_fftrans.exe
  #
  if ( ${treatspotscan} == 'y' ) then
    #
    \rm -f SCRATCH/ref2${iname}_fft.mrc
    setenv IN  SCRATCH/refb2${iname}_flt.mrc
    setenv OUT SCRATCH/ref2${iname}_fft.mrc
    ${bin_2dx}/2dx_fftrans.exe
    #
  endif
  #
  if ( ${tempkeep} == 'y' ) then
    \mv -f SCRATCH/refb1${iname}_flt.mrc SCRATCH/refb1${iname}_flt_unbend2.mrc
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/refb1${iname}_flt_unbend2.mrc <Reference 1>" >> LOGS/${scriptname}.results 
    endif
    if ( ${treatspotscan} == 'y' ) then
      \mv -f SCRATCH/refb2${iname}_flt.mrc SCRATCH/refb2${iname}_flt_unbend2.mrc
      if ( ${locround} == '1' ) then
        echo "# IMAGE: SCRATCH/refb2${iname}_flt_unbend2.mrc <Reference 2>" >> LOGS/${scriptname}.results 
      endif
    endif
  else
    \rm SCRATCH/refb1${iname}_flt.mrc
    if ( ${treatspotscan} == 'y' ) then
      \rm SCRATCH/refb2${iname}_flt.mrc
    endif
  endif
  echo "<<@progress: 45>>"
  #
  #############################################################################
  #                                                                           #
  # MASKTRANA - to mask the FFT of the image                                  #
  # TTMASK    - to mask the FFT of the image with TTF-correction              #
  #                                                                           #
  #          ${imagename}.spt  +  ${iname}_fft  =>  ${iname}_msk      #
  #                                                                           #
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
    setenv SPOTS ${imagename}.spt
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
`cat ${imagename}.spt`
eot
      #
      # There is a strange bug in ttmask on OSX, where a mask of "20" causes one funny pixel in the masked FFT to be extremely strong.
      # The stuff below was an attempt to trace this bug, but without any success.
      # All other masking diameters, like 18,19,21,22, work well....
      #
      if ( ${locround} == '-1' ) then
        echo "::" ${imagesidelength},${imagesidelength},${stepdigitizer},${magnification},${CS},${KV}
        echo "::" ${defocus},${TLTAXIS},${TLTANG} 
        echo "::" 1
        echo "::" ${maskb},${maskb} 	
        echo "::" ${lattice},-30,30,-30,30,${rmax},1,8 	
        echo "::" TMP234440.dat
        echo "::" cat ${imagename}.spt
        cp -f FFTIR/${iname}_fft.mrc test1-input.mrc
        echo "# IMAGE: test1-input.mrc <TEST1: FFT of Image>" >> LOGS/${scriptname}.results 
        cp -f SCRATCH/mgf08034b_fft_msk.mrc test1.mrc
        echo "# IMAGE: test1.mrc <TEST1: maskb-Masked FFT of Image>" >> LOGS/${scriptname}.results 
        ${proc_2dx}/protest "stop"
      endif
      #
    else
      #
      ${proc_2dx}/${lincommand} "TTMASK - to mask the FFT of the image with TTF-correction"
      ${proc_2dx}/${lincommand} "-------- The hole image has to be treated in order not to loose data"
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
  echo "<<@evaluate>>"
  #
  #############################################################################
  #                                                                           #
  #  FFTRANS - to calculate image after TTF correction                        #
  #                                                                           #
  #                             refb${iname}.flt  =>  ref${iname}_fft #
  #                                                                           #
  #############################################################################
  #
  if ( ${ttfcorfirst} == 'y' ) then
    ${proc_2dx}/${lincommand} "FFTRANS - to calculate image after TTF correction"
    \rm -f     SCRATCH/${iname}_tmp.mrc
    setenv IN  SCRATCH/${iname}_fft_msk.mrc
    setenv OUT SCRATCH/${iname}_tmp.mrc
    ${bin_2dx}/2dx_fftrans.exe
    #
    ${proc_2dx}/${lincommand} "LABEL - to produce BYTE format of TTF corrected image"
    \rm -f     SCRATCH/${iname}_ttf.mrc
    ${bin_2dx}/labelh.exe << eot
SCRATCH/${iname}_tmp.mrc
-3
SCRATCH/${iname}_ttf.mrc
eot
    \rm -f SCRATCH/${iname}_tmp.mrc
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
  #                                                                           #
  ${proc_2dx}/${lincommand} "TWOFILE - to calculate cross-correlation between reference and image"
  #                                                                           #
  #    ${iname}_msk  +  ref${iname}_fft  =>  corel${iname}_fft    #
  #                                                                           #
  #############################################################################
  #
  #  Multiply two files together    :    FILE1 * Complex Conjugate of FILE2
  #
  \rm -f SCRATCH/corel1${iname}_fft.mrc
  setenv IN1 SCRATCH/${iname}_fft_msk.mrc
  setenv IN2 SCRATCH/ref1${iname}_fft.mrc
  setenv OUT SCRATCH/corel1${iname}_fft.mrc
  ${bin_2dx}/twofile.exe << eot
2		! ICOMB = 2
2 0 0 ${refposix} ${refposiy} ! IORIGIN,OXA,OYA,OXB,OYB  Origin shifts to FFTs
eot
  #
  if ( ${treatspotscan} == 'y' ) then
    #
    \rm -f SCRATCH/corel2${iname}_fft.mrc
    setenv IN1 SCRATCH/${iname}_fft_msk.mrc
    setenv IN2 SCRATCH/ref2${iname}_fft.mrc
    setenv OUT SCRATCH/corel2${iname}_fft.mrc
    ${proc_2dx}/${lincommand} "TWOFILE - again, second for SpotScan treatment"
    ${bin_2dx}/twofile.exe << eot
2               ! ICOMB = 2
2 0 0 ${refposix} ${refposiy} ! IORIGIN,OXA,OYA,OXB,OYB  Origin shifts to FFT's.
eot
    #
    # if ( ${locround} == '1' ) then
    #   echo "# IMAGE: SCRATCH/corel2${iname}_fft.mrc <XCF between Reference 2 and Image>" >> LOGS/${scriptname}.results 
    # endif
    #
  endif
  #
  if ( ! -e SCRATCH/corel1${iname}_fft.mrc ) then
    ${proc_2dx}/protest "ERROR: SCRATCH/corel${iname}_fft.mrc does not exist."
  endif
  #
  if ( ${tempkeep} == 'y' ) then
    \mv -f SCRATCH/${iname}_fft_msk.mrc SCRATCH/${iname}_fft_msk_unbend2.mrc
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/${iname}_fft_msk_unbend2.mrc <maskb-Masked FFT of Image (2)>" >> LOGS/${scriptname}.results 
    endif
    \mv -f SCRATCH/ref1${iname}_fft.mrc SCRATCH/ref1${iname}_fft_unbend2.mrc
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/ref1${iname}_fft_unbend2.mrc <FFT of Reference 1>" >> LOGS/${scriptname}.results 
    endif
    if ( ${treatspotscan} == 'y' ) then
      \mv -f SCRATCH/ref2${iname}_fft.mrc SCRATCH/ref2${iname}_fft_unbend2.mrc
      if ( ${locround} == '1' ) then
        echo "# IMAGE: SCRATCH/ref2${iname}_fft_unbend2.mrc <FFT of Reference 2>" >> LOGS/${scriptname}.results 
      endif
    endif
  else
    \rm -f SCRATCH/${iname}_fft_msk.mrc
    \rm SCRATCH/ref1${iname}_fft.mrc
    if ( ${treatspotscan} == 'y' ) then
      \rm -f SCRATCH/ref2${iname}_fft.mrc
    endif
  endif
  echo "<<@progress: 65>>"
  echo "<<@evaluate>>"
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "FFTTRANS - to calculate cross-correlation map"
  #                                                                           #
  #                            corel${iname}_fft  =>  cor${iname}_cor #
  #                                                                           #
  #############################################################################
  #
  /bin/rm -f SCRATCH/cor1${iname}_cor.mrc
  #
  setenv IN  SCRATCH/corel1${iname}_fft.mrc
  setenv OUT SCRATCH/cor1${iname}_cor.mrc
  ${bin_2dx}/2dx_fftrans.exe
  #
  if ( ${tempkeep} == 'n' ) then
    \rm -f SCRATCH/corel1${iname}_fft.mrc
  else
    # if ( ${locround} == '1' ) then
    #   echo "# IMAGE: SCRATCH/corel1${iname}_fft.mrc <XCF between Reference 1 and Image>" >> LOGS/${scriptname}.results 
    # endif
  endif
  #
  if ( ${treatspotscan} == 'y' ) then
    #
    /bin/rm -f SCRATCH/cor2${iname}_cor.mrc
    #
    setenv IN  SCRATCH/corel2${iname}_fft.mrc
    setenv OUT SCRATCH/cor2${iname}_cor.mrc
    ${proc_2dx}/${lincommand} "FFTTRANS - again, for SpotScan treatment"
    ${bin_2dx}/2dx_fftrans.exe
    #
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/cor2${iname}_cor.mrc <XCF between Reference 2 and Image>" >> LOGS/${scriptname}.results 
    endif
    \rm -f SCRATCH/corel2${iname}_fft.mrc
    #
  endif
  echo "<<@progress: 70>>"
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "QUADSERCH - to search cross-correlation map for peaks"
  ${proc_2dx}/${lincommand} "with IPASS=1 to find first ERROR field"
  #                                                                           #
  #     cor${iname}_cor =>  auto${iname}_map  +  prof${iname}.dat #
  #                                                                           #
  #############################################################################
  #
  \rm -f SCRATCH/errors${iname}.dat
  \rm -f SCRATCH/prof${iname}.dat
  setenv PROFILE  SCRATCH/auto${iname}_map.mrc
  setenv PROFDATA SCRATCH/prof${iname}.dat
  setenv ERRORS   SCRATCH/errors${iname}.dat
  #
  if ( ${treatspotscan} == 'n' ) then
    set cormap = SCRATCH/cor1${iname}_cor.mrc
  else
    set cormap = SCRATCH/cor2${iname}_cor.mrc
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
${quadradbx},${quadradby}		! RADIUS OF CORR SEARCH
${refposix} ${refposiy}		! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N				! YES/NO FOR DETAILED PRINTOUT
${radlim}                       ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}          ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
0                               ! dont create manual Masking information
0                               ! Dont mask the image directly
eot
  #
  \mv -f CCPLOT.PS PS/${iname}-quadserch2a.ps
  if ( ${locround} == '1' ) then
    echo "# IMAGE-IMPORTANT: PS/${iname}-quadserch2a.ps <PS: Vector Plot of Distortion, Pass 1>" >> LOGS/${scriptname}.results 
  endif
  echo "<<@progress: 75>>"
  echo "<<@evaluate>>"
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "QUADSERCH - to search cross-correlation map for peaks"
  ${proc_2dx}/${lincommand} "with IPASS=3 to transform ERROR field into refined ERROUT field."
  #                                                                           #
  #     cor${iname}_cor =>  auto${iname}_map  +  prof${iname}.dat #
  #                                                                           #
  #############################################################################
  #
  \rm -f SCRATCH/errout${iname}.dat
  \rm -f SCRATCH/prof${iname}.dat
  setenv PROFILE  SCRATCH/auto${iname}_map.mrc
  setenv PROFDATA SCRATCH/prof${iname}.dat
  setenv ERRORS   SCRATCH/errors${iname}.dat
  setenv ERROUT   SCRATCH/errout${iname}.dat
  # echo ":: "`ls -l SCRATCH/errors${iname}.dat`
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
      echo "# IMAGE-IMPORTANT: ManualMasking_CCmap.mrc <XCF Map for Manual Masking>" >> LOGS/${scriptname}.results
      echo "# IMAGE-IMPORTANT: ManualMasking_UnbendPlot.mrc <Distortion Plot for Manual Masking>" >> LOGS/${scriptname}.results
    endif
    #
  else
    #
    ${proc_2dx}/${lincommand} "attention, masking image"
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
${iname}.mrc
m${iname}.mrc
256       
0
eot
    #
    if ( -e TMP_quadserch_6.mrc ) then
      \mv -f TMP_quadserch_6.mrc CUT/${nonmaskimagename}_masking.mrc
      \mv -f TMP_quadserch_7.mrc CUT/${nonmaskimagename}_masking_final.mrc
      \cp -f CUT/${nonmaskimagename}_masking_final.mrc ${nonmaskimagename}_masking_final.mrc
      if ( ${tempkeep} == 'y' ) then
        echo "# IMAGE-IMPORTANT: TMP_quadserch_1.mrc <Masking file 1>" >> LOGS/${scriptname}.results 
        echo "# IMAGE-IMPORTANT: TMP_quadserch_2.mrc <Masking file 2>" >> LOGS/${scriptname}.results 
        echo "# IMAGE-IMPORTANT: TMP_quadserch_3.mrc <Masking file 3>" >> LOGS/${scriptname}.results 
        echo "# IMAGE-IMPORTANT: TMP_quadserch_4.mrc <Masking file 4>" >> LOGS/${scriptname}.results 
        echo "# IMAGE-IMPORTANT: TMP_quadserch_5.mrc <Masking file 5>" >> LOGS/${scriptname}.results 
      else
        \rm -f TMP_quadserch_1.mrc
        \rm -f TMP_quadserch_2.mrc
        \rm -f TMP_quadserch_3.mrc
        \rm -f TMP_quadserch_4.mrc
        \rm -f TMP_quadserch_5.mrc
      endif
      echo "# IMAGE-IMPORTANT: CUT/${nonmaskimagename}_masking.mrc <Masking Area from Automatic Masking>" >> LOGS/${scriptname}.results 
      echo "# IMAGE-IMPORTANT: CUT/${nonmaskimagename}_masking_final.mrc <Masking Filter from Automatic Masking>" >> LOGS/${scriptname}.results 
    else
      ${proc_2dx}/protest "unbend2: ERROR: TMP_quadserch_6.mrc does not exist."
    endif
  endif
  \mv -f CCPLOT.PS PS/${iname}-quadserch2b.ps
  if ( ${locround} == '1' ) then
    echo "# IMAGE-IMPORTANT: PS/${iname}-quadserch2b.ps <PS: Vector Plot of Distortion, Pass 2>" >> LOGS/${scriptname}.results 
  endif
  #
  echo "<<@progress: 80>>"
  if ( ${treatspotscan} == 'y' ) then
    #
    #############################################################################
    #                                                                           #
    ${proc_2dx}/${lincommand} "QUADSERCH - to search cross-correlation map for peaks"
    ${proc_2dx}/${lincommand} "with IPASS=2 to read in better ERROUT field and find cc-peaks"
    ${proc_2dx}/lin "this should find lattice within SpotScan spots."
    #                                                                           #
    #                 cor${iname}_cor =>  auto${iname}_map  +  prof${iname}.dat #
    #                                                                           #
    #############################################################################
    #
    #
    \rm -f SCRATCH/prof${iname}.dat
    setenv PROFILE  SCRATCH/auto${iname}_map.mrc
    setenv PROFDATA SCRATCH/prof${iname}.dat
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
SCRATCH/cor1${iname}_cor.mrc
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
      ${proc_2dx}/${lincommand} "attention, masking image"
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
      #
      ${bin_2dx}/2dx_quadserchk-2.exe << eot
2,${quadpredb}                     ! IPASS,NRANGE
SCRATCH/cor1${iname}_cor.mrc
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
${iname}.mrc
m${iname}.mrc
-1
0
eot
      #
      if ( -e TMP_quadserch_6.mrc ) then
        \mv -f TMP_quadserch_6.mrc CUT/${nonmaskimagename}_masking.mrc
        \mv -f TMP_quadserch_7.mrc CUT/${nonmaskimagename}_masking_final.mrc
        \cp -f CUT/${nonmaskimagename}_masking_final.mrc ${nonmaskimagename}_masking_final.mrc
        if ( ${locround} == '1' ) then
          echo "# IMAGE-IMPORTANT: CUT/${nonmaskimagename}_masking.mrc <Masking Area from Automatic Masking>" >> LOGS/${scriptname}.results 
          echo "# IMAGE-IMPORTANT: CUT/${nonmaskimagename}_masking_final.mrc <Masking Filter from Automatic Masking>" >> LOGS/${scriptname}.results 
        endif
      else
        ${proc_2dx}/protest "unbend2: ERROR: TMP_quadserch_6.mrc does not exist."
      endif
      #
    endif
    #
    if ( ${createmaskinfo} == 'y' ) then
      echo "# IMAGE-IMPORTANT: ManualMasking_CCmap.mrc <XCF Map for Manual Masking>" >> LOGS/${scriptname}.results
      echo "# IMAGE-IMPORTANT: ManualMasking_UnbendPlot.mrc <Distortion Plot for Manual Masking>" >> LOGS/${scriptname}.results
    endif
    #
    \mv -f CCPLOT.PS PS/${iname}-quadserch2c.ps
    if ( ${locround} == '1' ) then
      echo "# IMAGE-IMPORTANT: PS/${iname}-quadserch2c.ps <PS: Vector Plot for Distortions, Pass 2>" >> LOGS/${scriptname}.results 
    endif
    #
    if ( ${tempkeep} == 'y' ) then
      \mv -f SCRATCH/cor2${iname}_cor.mrc SCRATCH/cor2${iname}_cor_unbend2.mrc
      if ( ${locround} == '1' ) then
        echo "# IMAGE: SCRATCH/cor2${iname}_cor_unbend2.mrc <XCF Map between Reference 2 and Image>" >> LOGS/${scriptname}.results 
      endif
    else
      \rm -f SCRATCH/cor2${iname}_cor.mrc
    endif
  endif
  #
  \mv -f SCRATCH/cor1${iname}_cor.mrc SCRATCH/cor1${iname}_cor_unbend2.mrc
  if ( ${locround} == '1' ) then
    echo "# IMAGE: SCRATCH/cor1${iname}_cor_unbend2.mrc <XCF Map between Reference 1 and Image>" >> LOGS/${scriptname}.results 
  endif
  echo "<<@progress: 85>>"
  echo "<<@evaluate>>"
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "SPIDERCOORD.spi - Unit cell locations in SPIDER format."
  #                                                                           #
  #############################################################################
  #
  \mv -f SPIDERCOORD.spi ${nonmaskimagename}-unitcells-spider.doc
  echo "# IMAGE: ${nonmaskimagename}-unitcells-spider.doc <SPIDER document with unit cell locations>" >> LOGS/${scriptname}.results
  #
  \cp -f SCRATCH/prof${iname}.dat ${iname}-profile.dat
  echo "# IMAGE: ${iname}-profile.dat <PROFILE with unit cell locations>" >> LOGS/${scriptname}.results
  #
  #
  #############################################################################
  #                                                                           #
  # Exit if automatic masking was done.                                       #
  #                                                                           #
  #############################################################################
  #
  if ( ${domask} == 'y' ) then
    \rm -f m${nonmaskimagename}.spt
    \ln -s ${nonmaskimagename}.spt m${nonmaskimagename}.spt
    echo "# IMAGE: ${nonmaskimagename}.mrc" >> LOGS/${scriptname}.results
    echo "# IMAGE: m${nonmaskimagename}.mrc" >> LOGS/${scriptname}.results
    echo "set imagename = m${nonmaskimagename}" >> LOGS/${scriptname}.results
    ${proc_2dx}/linblock "="
    ${proc_2dx}/linblock "#"
    ${proc_2dx}/linblock "#"
    ${proc_2dx}/linblock "Image now masked, name was changed to m${nonmaskimagename}"
    ${proc_2dx}/linblock "SpotList link created: m${nonmaskimagename}.spt -> ${nonmaskimagename}.spt"
    ${proc_2dx}/linblock "#"
    ${proc_2dx}/linblock "Please relaunch the entire processing."
    ${proc_2dx}/linblock "#"
    ${proc_2dx}/linblock "Only the Defocus and Tilt Geometry determination is not needed again."
    ${proc_2dx}/linblock "#"
    ${proc_2dx}/linblock "#"
    #
    echo "set domask = n" >> LOGS/${scriptname}.results
    #
    echo "set MASKING_done = y" >> LOGS/${scriptname}.results
    #
    echo "set FFT_done = n" >> LOGS/${scriptname}.results
    echo "set UNBENDING_done = n" >> LOGS/${scriptname}.results
    #
    echo "<<@progress: 100>>"
    exit -1
  endif
  #
  #############################################################################
  #                                                                           #
  # CCUNBEND - to unbend the image                                            #
  #                                                                           #
  # prof${iname}.dat  +  ${iname}.mrc      =>  cor${iname}.mrc    #
  #                                                                           #
  #############################################################################
  #
  \rm -f SCRATCH/cor${iname}_notap.mrc
  setenv CCORDATA SCRATCH/prof${iname}.dat
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
  if ( ${ttfcorfirst} == 'n' ) then
    #
    if ( ${ccunbend_program} == "1" ) then
      ${proc_2dx}/${lincommand} "2dx_ccunbendh - to unbend the original image"
      ${bin_2dx}/2dx_ccunbendh.exe << eot
${iname}.mrc
${IMAXCOR},${ISTEP_h},${NNUM},${ROFFSET}	 !IMAXCOR,ISTEP,NNUM,ROFFSET
0.001,${facthresha},${RMAG} !EPS,FACTOR,RMAG
SCRATCH/cor${iname}_notap.mrc
UNBENT,PASS,2,${date}
eot
      #
    else
      #
      ${proc_2dx}/${lincommand} "2dx_ccunbendk - to unbend the original image"
      ${bin_2dx}/2dx_ccunbendk.exe << eot
${iname}.mrc
${ITYPE},1,${IMAXCOR},${ISTEP},F,40,T       !ITYPE,IOUT,IMAXCOR,ISTEP,LTAPER,RTAPER,LTABOUT
30,52,0.001,${facthresha},${TLTAXIS},${RMAG},${LCOLOR}     !IKX,IKY,EPS,FACTOR,TLTAXIS,RMAG,LCOLOR
${iname}, UNBEND2, ${date}
SCRATCH/cor${iname}_notap.mrc
UNBENT,PASS,2,${date}
eot
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
SCRATCH/cor${iname}_notap.mrc
UNBENT,PASS,2,${date}
eot
      #
    else
      #
      ${bin_2dx}/2dx_ccunbendk.exe << eot
SCRATCH/${iname}_ttf.mrc
${ITYPE},1,${IMAXCOR},${ISTEP},F,40,T                !ITYPE,IOUT,IMAXCOR,ISTEP,LTAPER,RTAPER,LTABOUT
30,52,0.001,${facthresha},${TLTAXIS},${RMAG},${LCOLOR}        !IKX,IKY,EPS,FACTOR,TLTAXIS,RMAG,LCOLOR
${iname}, UNBEND2, ${date}
SCRATCH/cor${iname}_notap.mrc
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
  echo "<<@evaluate>>"
  #
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "TAPEREDGE - to eliminate transform stripes"
  #                                                                           #
  #                          cor${iname}.mrc  =>  cor${iname}.tap.mrc #
  #                                                                           #
  #############################################################################
  #
  \rm -f unbent.mrc
  #
  # UNBENDING
  setenv IN  SCRATCH/cor${iname}_notap.mrc
  setenv OUT unbent.mrc
  ${bin_2dx}/2dx_taperedgek.exe << eot
30,30,100,30       ! IAVER,ISMOOTH,ITAPER
eot
  #
  if ( ${tempkeep} == 'n' ) then
    \rm -f SCRATCH/cor${iname}_notap.mrc
  else
    \mv -f SCRATCH/cor${iname}_notap.mrc SCRATCH/cor${iname}_notap_unbend2.mrc
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/cor${iname}_notap_unbend2.mrc <Raw Unbent Image>" >> LOGS/${scriptname}.results 
    endif
  endif
  #
  #
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "FFTRANS - to calculate FFT from image after unbending"
  #                                                                           #
  #                          cor${iname}.tap.mrc  =>  cor${iname}_fft #
  #                                                                           #
  #############################################################################
  #
  \rm -f FFTIR/cor${iname}_fft.mrc
  setenv IN  unbent.mrc
  setenv OUT FFTIR/cor${iname}_fft.mrc
  ${bin_2dx}/2dx_fftrans.exe
  #
  if ( ${locround} == '1' ) then
    echo "# IMAGE: unbent.mrc <Unbent and Edge-Tapered Image>" >> LOGS/${scriptname}.results 
  endif
  #
  if ( ${locround} == '1' ) then
    echo "# IMAGE-IMPORTANT: FFTIR/cor${iname}_fft.mrc <FFT of Unbent and Edge-Tapered Image>" >> LOGS/${scriptname}.results 
  endif
  #
  echo "<<@progress: 93>>"
  #
  #############################################################################
  #                                                                           #
  #      MMBOX - no resolution limitation for merging                         #
  #  TTBOX (T-JOBB) - to read out amplitudes and phases from 2nd unbent image #
  #                                                                           #
  #############################################################################
  #
  \rm -f SCRATCH/TMP9873.dat
  #
  if ( ${ctf_ttf} == 'CTF' && ${ttfcorfirst} == 'n' ) then
    #
    \rm -f APH/${imagename}_nolimit.aph
    #
    ${proc_2dx}/${lincommand} "MMBOXA - to read out AMPs and PHASES, no resolution limit"
    ${bin_2dx}/2dx_mmboxa.exe << eot
FFTIR/cor${iname}_fft.mrc
${imagenumber} ${iname}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_fou_nolimit.aph
SCRATCH/TMP9873.dat
U2
${refposix},${refposiy}           ! XORIG,YORIG
200.0,1.5,1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot
    #
    echo "# IMAGE-IMPORTANT: APH/${imagename}_fou_nolimit.aph <APH File (No Res_limit) [H,K,A,P,IQ,Back,0]>" >> LOGS/${scriptname}.results
    #
  else
    #############################################################################
    #
    if ( ${ttfcorfirst} == 'n' ) then
      #
      \rm -f APH/${imagename}_fou_ttf_nolimit.aph
      #
      ${proc_2dx}/${lincommand} "TTBOXA - to read out AMPs and PHASES with TTF-correction"
      ${bin_2dx}/2dx_ttboxk.exe << eot
FFTIR/cor${iname}_fft.mrc
${imagenumber} ${iname}, Unbend2, ${date}
Y                        ! generate grid from lattice
N                        ! generate points from lattice
N                        ! list points as calculated
Y                        ! plot output
${imagesidelength},${imagesidelength},${stepdigitizer},${magnification},${CS},${KV} ! ISIZEX,ISIZEY,DSTEP,MAG,CS,KV
${defocus},${TLTAXIS},${TLTANG} ! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
2,0,30,30,19,19          ! IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_fou_ttf_nolimit.aph
SCRATCH/TMP9873.dat
U2
200.0,3.0,${refposix},${refposiy},90.0 !RSMN,RSMX,XORIG,YORIG,SEGMNT
${lattice}                  ! reciprocal lattice vectors in pixels
eot
      #
      echo "# IMAGE-IMPORTANT: APH/${imagename}_fou_ttf_nolimit.aph <APH File after TTF correction (No Res_limit) [H,K,A,P,IQ,Back,0])>" >> LOGS/${scriptname}.results
      #
      cd APH
      \rm -f ${imagename}_fou_cor.aph
      \ln -s ${imagename}_fou_ttf_nolimit.aph ${imagename}_fou_cor.aph
      cd ..
      #
      \mv -f TTPLOT.PS PS/${iname}.ttplot_nolimit_unbend2.ps
      if ( ${locround} == '1' ) then
        echo "# IMAGE-IMPORTANT: PS/${iname}.ttplot_nolimit_unbend2.ps <PS: IQ Plot after TTF correction (No Resolution Limit)>" >> LOGS/${scriptname}.results 
      endif
      #
    else
      #
      #############################################################################
      #
      \rm -f APH/${imagename}_fou_ttf_nolimit.aph
      #
      ${proc_2dx}/${lincommand} "MMBOXA - to read out AMPs and PHASES without TTF-correction"
      ${proc_2dx}/${lincommand} "         (because image is already TTF corrected)"
      ${bin_2dx}/2dx_mmboxa.exe << eot
FFTIR/cor${iname}_fft.mrc
${imagenumber} ${iname}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_fou_ttf_nolimit.aph
SCRATCH/TMP9873.dat
U2
${refposix},${refposiy}           ! XORIG,YORIG
200.0,1.5,1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot
      #
      echo "# IMAGE-IMPORTANT: APH/${imagename}_fou_ttf_nolimit.aph <APH File after TTF correction (No Res_limit) [H,K,A,P,IQ,Back,0]>" >> LOGS/${scriptname}.results
      #
    endif
    #
    cd APH
    \rm -f ${imagename}_fou_cor.aph
    \ln -s ${imagename}_fou_ttf_nolimit.aph ${imagename}_fou_cor.aph
    \rm -f ${imagename}_ttf_nolimit.aph
    \ln -s ${imagename}_fou_ttf_nolimit.aph ${imagename}_ttf_nolimit.aph
    cd ..
    #
    echo "# IMAGE-IMPORTANT: APH/${imagename}_cor.aph <Link to Final APH File [H,K,A,P,IQ,Back,0]>" >> LOGS/${scriptname}.results
    # 
  endif
  #
  echo "# MASKB: "${maskb}
  echo "# BOXB: "${boxb1}
  echo "# IQSTAT-NORESLIM:"
  cat SCRATCH/TMP9873.dat
  #
  \rm -f SCRATCH/TMP9873.dat
  #
  #
  #############################################################################
  #                                                                           #
  #      MMBOX - resolution limitation for output                             #
  #  TTBOX (T-JOBB) - to read out amplitudes and phases from 2nd unbent image #
  #                                                                           #
  #                       cor${iname}_fft  =>  ${iname}_cor_limit.aph #
  #                                                                           #
  #############################################################################  
  #
  \rm -f SCRATCH/TMP9882.dat
  if ( ${ctf_ttf} == 'CTF' && ${ttfcorfirst} == 'n' ) then
    #
    \rm -f APH/${imagename}_fou_limit.aph
    #
    ${proc_2dx}/${lincommand} "MMBOXA - to read out AMPs and PHASES, with resolution limit"
    ${bin_2dx}/2dx_mmboxa.exe << eot
FFTIR/cor${iname}_fft.mrc
${imagenumber} ${iname}, Unbend2, ${date}
Y				! Use grid units?
Y				! Generate grid from lattice?
N				! Generate points from lattice?
2,2,0,50,50,19,19		! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_fou_limit.aph
SCRATCH/TMP9882.dat
U2
${refposix},${refposiy}		! XORIG,YORIG
${RESMIN},${RESMAX},1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}				! Lattice vectors
eot
    #
    echo "# IMAGE-IMPORTANT: APH/${imagename}_fou_limit.aph <APH File (Res_limit) [H,K,A,P,IQ,Back,0]>" >> LOGS/${scriptname}.results
    #
  else
    #
    if ( ${ttfcorfirst} == 'n' ) then
      #
      \rm -f APH/${imagename}_fou_ttf_limit.aph
      #
      ${proc_2dx}/${lincommand} "TTBOXA - to read out AMPs and PHASES with TTF-correction"
      #
      echo       ${bin_2dx}/2dx_ttboxk.exe
      echo  FFTIR/cor${iname}_fft.mrc
      echo  ${imagenumber} ${iname}, Unbend2, ${date}
      echo  Y                        
      echo  N                        
      echo  N                        
      echo  Y                        
      echo  ${imagesidelength} ${imagesidelength} ${stepdigitizer} ${magnification} ${CS},${KV}
      echo  ${defocus} ${TLTAXIS} ${TLTANG}
      echo  2 0 30 30 19 19          
      echo  APH/${imagename}_fou_ttf_limit.aph
      echo  SCRATCH/TMP9882.dat
      echo  U2
      echo  ${RESMIN} ${RESMAX} ${refposix} ${refposiy} 90.0 
      echo  ${lattice}                  
      echo  eot
      echo "Starting now:"
      #
      ${bin_2dx}/2dx_ttboxk.exe << eot
FFTIR/cor${iname}_fft.mrc
${imagenumber} ${iname}, Unbend2, ${date}
Y                        ! generate grid from lattice
N                        ! generate points from lattice
N                        ! list points as calculated
Y                        ! plot output
${imagesidelength} ${imagesidelength} ${stepdigitizer} ${magnification} ${CS} ${KV}
${defocus} ${TLTAXIS} ${TLTANG}
2 0 30 30 19 19          ! IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_fou_ttf_limit.aph
SCRATCH/TMP9882.dat
U2
${RESMIN} ${RESMAX} ${refposix} ${refposiy} 90.0 !RSMN,RSMX,XORIG,YORIG,SEGMNT
${lattice}                  ! reciprocal lattice vectors in pixels
eot
      #
      echo "# IMAGE-IMPORTANT: APH/${imagename}_fou_ttf_limit.aph <APH File after TTF correction (Res_limit) [H,K,A,P,IQ,Back,0]>" >> LOGS/${scriptname}.results
      #
      \mv -f TTPLOT.PS PS/${iname}.ttplot_limit_unbend2.ps
      if ( ${locround} == '1' ) then
        echo "# IMAGE-IMPORTANT: PS/${iname}.ttplot_limit_unbend2.ps <PS: IQ Plot after TTF correction (Resolution Limitation)>" >> LOGS/${scriptname}.results 
      endif
      #
    else
      #
      \rm -f APH/${imagename}_fou_ttf_limit.aph
      #
      ${proc_2dx}/${lincommand} "MMBOXA - to read out AMPs and PHASES without TTF-correction"
      ${proc_2dx}/${lincommand} "         (because image is already TTF corrected)"
      ${bin_2dx}/2dx_mmboxa.exe << eot
FFTIR/cor${iname}_fft.mrc
${imagenumber} ${iname}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}_fou_ttf_limit.aph
SCRATCH/TMP9882.dat
U2
${refposix},${refposiy}           ! XORIG,YORIG
${RESMIN},${RESMAX},1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot
      #
      echo "# IMAGE-IMPORTANT: APH/${imagename}_fou_ttf_limit.aph <APH File after TTF correction (Res_limit) [H,K,A,P,IQ,Back,0]>" >> LOGS/${scriptname}.results
      #
    endif
    #
  endif  
  #
  echo "# IQSTAT-RESLIM:"
  cat SCRATCH/TMP9882.dat
  if ( ${final_round} == "y" ) then
    cat SCRATCH/TMP9882.dat >> LOGS/${scriptname}.results 
  endif
  source SCRATCH/TMP9882.dat
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
  \rm -f SCRATCH/TMP9882.dat  
  #
  echo "<<@progress: 96>>"  
  echo "<<@evaluate>>"
  #
  \rm -f SPIDERCOORD.spi
  #
  #

