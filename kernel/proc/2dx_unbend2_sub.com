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
  ${proc_2dx}/linblock "Unbend 2: ${imagename}"
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
#  ${imagename}.spt + cor${imagename}.fft.mrc => ref${imagename}.fft.msk.mrc#
#                                                                           #
#############################################################################  
  #
  if ( ${ctf_ttf} == 'CTF' ) then
    #
    \rm -f SCRATCH/ref${imagename}.fft.msk.mrc
    #
    setenv IN  FFTIR/cor${imagename}.fft.mrc
    setenv OUT SCRATCH/ref${imagename}.fft.msk.mrc
    setenv SPOTS ${imagename}.spt
    ${proc_2dx}/${lincommand} "MASKTRANA - to mask the FFT in order to create the reference map"
    #
    echo 2 T T F	
    echo 1	
    echo ${lattice},-30,30,-30,30,${rmax},1 
    #
    ${bin_2dx}/2dx_masktrana.exe << eot
2 T T F				! ISHAPE= 1(CIRC),2(GAUSS CIRC),OR 3(RECT) HOLE, IAMPLIMIT(T or F)
${holeb}			! RADIUS OF HOLE IF CIRCULAR, X,Y HALF-EDGE-LENGTHS IF RECT.
${lattice},-30,30,-30,30,${rmax},1 !A/BX/Y,IH/IKMN/MX,RMAX,ITYPE
eot
    #
    echo "<<@progress: 10>>"
    echo "2dx_masktrana.exe finished."
    #
    if ( ${tempkeep} == 'y' ) then
      \mv -f FFTIR/cor${imagename}.fft.mrc SCRATCH/cor${imagename}.fft.unbend2.mrc
      if ( ${locround} == '1' ) then
        echo "# IMAGE-IMPORTANT: FFTIR/cor${imagename}.fft.unbend2.mrc <FFT of Unbent Image>" >> LOGS/${scriptname}.results
      endif
    else
      \rm -f FFTIR/cor${imagename}.fft.mrc
    endif
    #
  else
    #
    ${proc_2dx}/${lincommand} "using holeb of ${holeb}"
    #
    \cp -f FFTIR/cor${imagename}.fft.mrc SCRATCH/ref${imagename}.fft.msk.mrc
    #
    \rm -f TMP234439.dat
    #
    setenv INOUT  SCRATCH/ref${imagename}.fft.msk.mrc
    #
    ${proc_2dx}/${lincommand} "TTMASK - to mask the FFT with TTFcor for reference"
    #
    echo "Using parameters:"
    echo ${imagesidelength},${imagesidelength},${stepdigitizer},${magnification},${CS},${KV}
    echo ${defocus},${TLTAXIS},${TLTANG}
    echo 1                          
    echo ${holeb},${holeb}       
    echo ${lattice},-30,30,-30,30,${rmax},1,8 
    echo TMP234439.dat
    #
    ${bin_2dx}/2dx_ttmask.exe << eot
${imagesidelength},${imagesidelength},${stepdigitizer},${magnification},${CS},${KV}
${defocus},${TLTAXIS},${TLTANG}   	! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
1                          		! ISHAPE= 1(circ),2(gauss circ),3(rect)
${holeb},${holeb}       		! radius hole if circular, X,Y half-edge-len if rect
${lattice},-30,30,-30,30,${rmax},1,8   	! A/BX/Y,IH/IKMN/MX,RMAX,ITYPE,NUMSPOT
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
  #                 ref${imagename}.fft.msk.mrc  =>  ref${imagename}.flt.mrc  #
  #                                                                           #
  ############################################################################# 
  #
  \rm -f SCRATCH/ref${imagename}.flt.mrc
  setenv IN  SCRATCH/ref${imagename}.fft.msk.mrc
  setenv OUT SCRATCH/ref${imagename}.flt.mrc
  ${bin_2dx}/2dx_fftrans.exe 
  #
  if ( ${tempkeep} == 'y' ) then
    \mv -f SCRATCH/ref${imagename}.fft.msk.mrc SCRATCH/ref${imagename}.fft.msk.unbend2.mrc
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/ref${imagename}.fft.msk.unbend2.mrc <FFT of Unbent Reference>" >> LOGS/${scriptname}.results
    endif
  else
    \rm -f SCRATCH/ref${imagename}.fft.msk.mrc
  endif
  #
  echo "<<@progress: 15>>"
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "LABEL - to cut out a larger area from the centre of created reference"
  #           just for debugging.                                             #
  #                                                                           #
  #                ref${imagename}.flt.mrc  =>  ${imagename}.reference2.mrc   #
  #                                                                           #
  #############################################################################  
  # 
  echo boxlabel = ${boxlabel} 
  #
  if ( -e SCRATCH/${imagename}.reference2.mrc ) then
    \rm -f SCRATCH/${imagename}.reference2.mrc
  endif
  ${bin_2dx}/labelh.exe << eot
SCRATCH/ref${imagename}.flt.mrc
1
SCRATCH/${imagename}.reference2.mrc
${boxlabel}
eot
  #
  echo "<<@progress: 20>>"
echo "<<@evaluate>>"
  if ( ${locround} == '1' ) then
    echo "# IMAGE: SCRATCH/${imagename}.reference2.mrc <Reference Patch>" >> LOGS/${scriptname}.results  
  endif
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "LABEL - to cut out small box in center of reference image"
  #                                                                           #
  #                     ref${imagename}.flt.mrc  =>  box${imagename}.flt.mrc  #
  #                                                                           #
  #############################################################################  
  #
  ${bin_2dx}/labelh.exe << eot
SCRATCH/ref${imagename}.flt.mrc
1
SCRATCH/box${imagename}.flt.mrc
${patlabel}
eot
  echo "<<@progress: 25>>"
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "AUTOCORRL - to calculate autocorrelation of reference center"
  #                                                                           #
  #                             box${imagename}.flt  =>  auto${imagename}.cor #
  #                                                                           #
  #############################################################################  
  #
  \rm -f SCRATCH/auto${imagename}.cor.mrc
  setenv IN  SCRATCH/box${imagename}.flt.mrc
  setenv OUT SCRATCH/auto${imagename}.cor.mrc
  ${bin_2dx}/autocorrl.exe << eot
20
eot
  #
  if ( ${locround} == '1' ) then
    echo "# IMAGE: SCRATCH/auto${imagename}.cor.mrc <Autocorrelation Function of Reference Patch>" >> LOGS/${scriptname}.results 
  endif
  #
  if ( ${tempkeep} == 'y' ) then
    \mv -f SCRATCH/box${imagename}.flt.mrc SCRATCH/box${imagename}.flt2.mrc
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/box${imagename}.flt2.mrc <Center of Reference Patch>" >> LOGS/${scriptname}.results
    endif
  else
    \rm -f SCRATCH/box${imagename}.flt.mrc
  endif
  #
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "LABEL - to extract autocorrelation peak"
  #                                                                           #
  #                            auto${imagename}.cor  =>  auto${imagename}.map #
  #                                                                           #
  #############################################################################
  #
  \rm -f SCRATCH/auto${imagename}.map.mrc
  ${bin_2dx}/labelh.exe << eot
SCRATCH/auto${imagename}.cor.mrc
1
SCRATCH/auto${imagename}.map.mrc
210,310,210,310
eot
  #
  echo "<<@progress: 30>>"
  echo "<<@evaluate>>"
  \mv -f SCRATCH/auto${imagename}.cor.mrc SCRATCH/auto${imagename}.cor.unbend2.mrc
  if ( ${locround} == '1' ) then
    echo "# IMAGE: SCRATCH/auto${imagename}.cor.unbend2.mrc <Autocorrelation Function of Reference Patch>" >> LOGS/${scriptname}.results
    echo "# IMAGE: SCRATCH/auto${imagename}.map.mrc <Central Peak of Autocorrelation Function of Reference Patch>" >> LOGS/${scriptname}.results 
  endif
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "BOXIMAGE - to box out reference for cross-correlation"
  #                                                                           #
  #                             ref${imagename}.flt  =>  refb${imagename}.flt #
  #                                                                           #
  #############################################################################
  #
  \rm -f SCRATCH/refb1${imagename}.flt.mrc
  setenv IN  SCRATCH/ref${imagename}.flt.mrc
  setenv OUT SCRATCH/refb1${imagename}.flt.mrc
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
    echo "# IMAGE: SCRATCH/refb1${imagename}.flt.mrc <Reference 1>" >> LOGS/${scriptname}.results 
  endif
  #
  if ( ${treatspotscan} == 'y' ) then
    #
    \rm -f SCRATCH/refb2${imagename}.flt.mrc
    setenv IN  SCRATCH/ref${imagename}.flt.mrc
    setenv OUT SCRATCH/refb2${imagename}.flt.mrc
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
      echo "# IMAGE: SCRATCH/refb2${imagename}.flt.mrc <Reference 2>" >> LOGS/${scriptname}.results 
    endif
    #
  endif
  echo "<<@progress: 40>>"
  echo "<<@evaluate>>"
  #
  if ( ${tempkeep} == 'y' ) then
    \mv -f SCRATCH/ref${imagename}.flt.mrc SCRATCH/ref${imagename}.flt.unbend2.mrc
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/ref${imagename}.flt.unbend2.mrc <Filtered Image for Reference generation>" >> LOGS/${scriptname}.results 
    endif
  else
    \rm -f SCRATCH/ref${imagename}.flt.mrc
  endif
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "FFTRANS - to calculate FFT from reference patch"
  #                                                                           #
  #                             refb${imagename}.flt  =>  ref${imagename}.fft #
  #                                                                           #
  #############################################################################  
  #
  \rm -f SCRATCH/ref1${imagename}.fft.mrc
  setenv IN  SCRATCH/refb1${imagename}.flt.mrc
  setenv OUT SCRATCH/ref1${imagename}.fft.mrc
  ${bin_2dx}/2dx_fftrans.exe
  #
  if ( ${treatspotscan} == 'y' ) then
    #
    \rm -f SCRATCH/ref2${imagename}.fft.mrc
    setenv IN  SCRATCH/refb2${imagename}.flt.mrc
    setenv OUT SCRATCH/ref2${imagename}.fft.mrc
    ${bin_2dx}/2dx_fftrans.exe
    #
  endif
  #
  if ( ${tempkeep} == 'y' ) then
    \mv -f SCRATCH/refb1${imagename}.flt.mrc SCRATCH/refb1${imagename}.flt.unbend2.mrc
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/refb1${imagename}.flt.unbend2.mrc <Reference 1>" >> LOGS/${scriptname}.results 
    endif
    if ( ${treatspotscan} == 'y' ) then
      \mv -f SCRATCH/refb2${imagename}.flt.mrc SCRATCH/refb2${imagename}.flt.unbend2.mrc
      if ( ${locround} == '1' ) then
        echo "# IMAGE: SCRATCH/refb2${imagename}.flt.unbend2.mrc <Reference 2>" >> LOGS/${scriptname}.results 
      endif
    endif
  else
    \rm SCRATCH/refb1${imagename}.flt.mrc
    if ( ${treatspotscan} == 'y' ) then
      \rm SCRATCH/refb2${imagename}.flt.mrc
    endif
  endif
  echo "<<@progress: 45>>"
  #
  #############################################################################
  #                                                                           #
  # MASKTRANA - to mask the FFT of the image                                  #
  # TTMASK    - to mask the FFT of the image with TTF-correction              #
  #                                                                           #
  #          ${imagename}.spt  +  ${imagename}.fft  =>  ${imagename}.msk      #
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
    \rm -f FFTIR/${imagename}.fft.msk.mrc
    \rm -f SCRATCH/${imagename}.fft.msk.mrc
    setenv IN  FFTIR/${imagename}.fft.mrc
    setenv OUT SCRATCH/${imagename}.fft.msk.mrc
    setenv SPOTS ${imagename}.spt
    #
    ${proc_2dx}/${lincommand} "MASKTRANA - to mask the FFT of the image"
    ${bin_2dx}/2dx_masktrana.exe << eot
1 T T F	! ISHAPE=1(CIRC),2(GAUSCIR),3(RECT)HOLE,IAMPLIMIT(T or F),ISPOT,IFIL
${maskb}       ! RADIUS OF HOLE IF CIRCULAR, X,Y HALF-EDGE-LENGTHS IF RECT.
${lattice},-30,30,-30,30,${rmax},1 !A/BX/Y,IH/IKMN/MX,RMAX,ITYPE
eot
    #
  else
    #
    \cp -f FFTIR/${imagename}.fft.mrc SCRATCH/${imagename}.fft.msk.mrc
    \rm -f TMP234440.dat 
    setenv INOUT  SCRATCH/${imagename}.fft.msk.mrc
    #
    if ( ${ttfcorfirst} == 'n' ) then
      ${proc_2dx}/${lincommand} "TTMASK - to mask the FFT of the image with TTF-correction"
      ${proc_2dx}/${lincommand} "-------- only the spots of the SpotList are treated"
      ${bin_2dx}/2dx_ttmask.exe << eot
${imagesidelength},${imagesidelength},${stepdigitizer},${magnification},${CS},${KV}
${defocus},${TLTAXIS},${TLTANG}   	! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
1       				! ISHAPE= 1(circ),2(gauss circ),3(rect),4(circ,maxrad)
${maskb},${maskb} 			! radius hole if circular, X,Y half-edge-len if rect
${lattice},-30,30,-30,30,${rmax},1,8 	! A/BX/Y,IH/IKMN/MX,RMAX,ITYPE,NUMSPOT
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
        cp -f FFTIR/${imagename}.fft.mrc test1-input.mrc
        echo "# IMAGE: test1-input.mrc <TEST1: FFT of Image>" >> LOGS/${scriptname}.results 
        cp -f SCRATCH/mgf08034b.fft.msk.mrc test1.mrc
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
${lattice},-30,30,-30,30,${rmax},0,8       ! A/BX/Y,IH/IKMN/MX,RMAX,ITYPE,NUMSPOT
TMP234440.dat
eot
      #
    endif
    #
  endif
  #
  if ( ${locround} == '1' ) then
    echo "# IMAGE: SCRATCH/${imagename}.fft.msk.mrc <maskb-Masked FFT of Image>" >> LOGS/${scriptname}.results 
  endif
  echo "<<@progress: 55>>"
  echo "<<@evaluate>>"
  #
  #############################################################################
  #                                                                           #
  #  FFTRANS - to calculate image after TTF correction                        #
  #                                                                           #
  #                             refb${imagename}.flt  =>  ref${imagename}.fft #
  #                                                                           #
  #############################################################################
  #
  if ( ${ttfcorfirst} == 'y' ) then
    ${proc_2dx}/${lincommand} "FFTRANS - to calculate image after TTF correction"
    \rm -f     SCRATCH/${imagename}.tmp.mrc
    setenv IN  SCRATCH/${imagename}.fft.msk.mrc
    setenv OUT SCRATCH/${imagename}.tmp.mrc
    ${bin_2dx}/2dx_fftrans.exe
    #
    ${proc_2dx}/${lincommand} "LABEL - to produce BYTE format of TTF corrected image"
    \rm -f     SCRATCH/${imagename}.ttf.mrc
    ${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}.tmp.mrc
-3
SCRATCH/${imagename}.ttf.mrc
eot
    \rm -f SCRATCH/${imagename}.tmp.mrc
    # 
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/${imagename}.ttf.mrc <Image after TTF correction>" >> LOGS/${scriptname}.results 
    endif
    #
  endif
  #
  echo "<<@progress: 60>>"
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "TWOFILE - to calculate cross-correlation"
  #                                                                           #
  #    ${imagename}.msk  +  ref${imagename}.fft  =>  corel${imagename}.fft    #
  #                                                                           #
  #############################################################################
  #
  #  Multiply two files together    :    FILE1 * Complex Conjugate of FILE2
  #
  \rm -f SCRATCH/corel1${imagename}.fft.mrc
  setenv IN1 SCRATCH/${imagename}.fft.msk.mrc
  setenv IN2 SCRATCH/ref1${imagename}.fft.mrc
  setenv OUT SCRATCH/corel1${imagename}.fft.mrc
  ${bin_2dx}/twofile.exe << eot
2		! ICOMB = 2
2 0 0 ${refposix} ${refposiy} ! IORIGIN,OXA,OYA,OXB,OYB  Origin shifts to FFTs
eot
  #
  if ( ${treatspotscan} == 'y' ) then
    #
    \rm -f SCRATCH/corel2${imagename}.fft.mrc
    setenv IN1 SCRATCH/${imagename}.fft.msk.mrc
    setenv IN2 SCRATCH/ref2${imagename}.fft.mrc
    setenv OUT SCRATCH/corel2${imagename}.fft.mrc
    ${proc_2dx}/${lincommand} "TWOFILE - again, second for SpotScan treatment"
    ${bin_2dx}/twofile.exe << eot
2               ! ICOMB = 2
2 0 0 ${refposix} ${refposiy} ! IORIGIN,OXA,OYA,OXB,OYB  Origin shifts to FFT's.
eot
    #
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/corel2${imagename}.fft.mrc <XCF between Reference 2 and Image>" >> LOGS/${scriptname}.results 
    endif
    #
  endif
  #
  if ( ! -e SCRATCH/corel1${imagename}.fft.mrc ) then
    ${proc_2dx}/protest "ERROR: SCRATCH/corel${imagename}.fft.mrc does not exist."
  endif
  #
  if ( ${tempkeep} == 'y' ) then
    \mv -f SCRATCH/${imagename}.fft.msk.mrc SCRATCH/${imagename}.fft.msk.unbend2.mrc
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/${imagename}.fft.msk.unbend2.mrc <maskb-Masked FFT of Image (2)>" >> LOGS/${scriptname}.results 
    endif
    \mv -f SCRATCH/ref1${imagename}.fft.mrc SCRATCH/ref1${imagename}.fft.unbend2.mrc
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/ref1${imagename}.fft.unbend2.mrc <FFT of Reference 1>" >> LOGS/${scriptname}.results 
    endif
    if ( ${treatspotscan} == 'y' ) then
      \mv -f SCRATCH/ref2${imagename}.fft.mrc SCRATCH/ref2${imagename}.fft.unbend2.mrc
      if ( ${locround} == '1' ) then
        echo "# IMAGE: SCRATCH/ref2${imagename}.fft.unbend2.mrc <FFT of Reference 2>" >> LOGS/${scriptname}.results 
      endif
    endif
  else
    \rm -f SCRATCH/${imagename}.fft.msk.mrc
    \rm SCRATCH/ref1${imagename}.fft.mrc
    if ( ${treatspotscan} == 'y' ) then
      \rm -f SCRATCH/ref2${imagename}.fft.mrc
    endif
  endif
  echo "<<@progress: 65>>"
  echo "<<@evaluate>>"
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "FFTTRANS - to calculate cross-correlation map"
  #                                                                           #
  #                            corel${imagename}.fft  =>  cor${imagename}.cor #
  #                                                                           #
  #############################################################################
  #
  /bin/rm -f SCRATCH/cor1${imagename}.cor.mrc
  #
  setenv IN  SCRATCH/corel1${imagename}.fft.mrc
  setenv OUT SCRATCH/cor1${imagename}.cor.mrc
  ${bin_2dx}/2dx_fftrans.exe
  #
  if ( ${tempkeep} == 'n' ) then
    \rm -f SCRATCH/corel1${imagename}.fft.mrc
  else
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/corel1${imagename}.fft.mrc <XCF between Reference 1 and Image>" >> LOGS/${scriptname}.results 
    endif
  endif
  #
  if ( ${treatspotscan} == 'y' ) then
    #
    /bin/rm -f SCRATCH/cor2${imagename}.cor.mrc
    #
    setenv IN  SCRATCH/corel2${imagename}.fft.mrc
    setenv OUT SCRATCH/cor2${imagename}.cor.mrc
    ${proc_2dx}/${lincommand} "FFTTRANS - again, for SpotScan treatment"
    ${bin_2dx}/2dx_fftrans.exe
    #
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/cor2${imagename}.cor.mrc <XCF between Reference 2 and Image>" >> LOGS/${scriptname}.results 
    endif
    \rm -f SCRATCH/corel2${imagename}.fft.mrc
    #
  endif
  echo "<<@progress: 70>>"
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "QUADSERCH - to search cross-correlation map for peaks"
  ${proc_2dx}/${lincommand} "with IPASS=1 to find first ERROR field"
  #                                                                           #
  #     cor${imagename}.cor =>  auto${imagename}.map  +  prof${imagename}.dat #
  #                                                                           #
  #############################################################################
  #
  \rm -f SCRATCH/errors${imagename}.dat
  \rm -f SCRATCH/prof${imagename}.dat
  setenv PROFILE  SCRATCH/auto${imagename}.map.mrc
  setenv PROFDATA SCRATCH/prof${imagename}.dat
  setenv ERRORS   SCRATCH/errors${imagename}.dat
  #
  if ( ${treatspotscan} == 'n' ) then
    set cormap = SCRATCH/cor1${imagename}.cor.mrc
  else
    set cormap = SCRATCH/cor2${imagename}.cor.mrc
  endif
  #
  \rm -f TMP-quadserch-1.mrc
  \rm -f TMP-quadserch-2.mrc
  \rm -f TMP-quadserch-3.mrc
  \rm -f TMP-quadserch-4.mrc
  \rm -f TMP-quadserch-5.mrc
  \rm -f TMP-quadserch-6.mrc
  \rm -f TMP-quadserch-7.mrc
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
  \mv -f CCPLOT.PS PS/${imagename}-quadserch2a.ps
  if ( ${locround} == '1' ) then
    echo "# IMAGE-IMPORTANT: PS/${imagename}-quadserch2a.ps <PS: Vector Plot of Distortion, Pass 1>" >> LOGS/${scriptname}.results 
  endif
  echo "<<@progress: 75>>"
  echo "<<@evaluate>>"
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "QUADSERCH - to search cross-correlation map for peaks"
  ${proc_2dx}/${lincommand} "with IPASS=3 to transform ERROR field into refined ERROUT field."
  #                                                                           #
  #     cor${imagename}.cor =>  auto${imagename}.map  +  prof${imagename}.dat #
  #                                                                           #
  #############################################################################
  #
  \rm -f SCRATCH/errout${imagename}.dat
  \rm -f SCRATCH/prof${imagename}.dat
  setenv PROFILE  SCRATCH/auto${imagename}.map.mrc
  setenv PROFDATA SCRATCH/prof${imagename}.dat
  setenv ERRORS   SCRATCH/errors${imagename}.dat
  setenv ERROUT   SCRATCH/errout${imagename}.dat
  # echo ":: "`ls -l SCRATCH/errors${imagename}.dat`
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
      \rm -f ManualMasking-CCmap.mrc
      \rm -f ManualMasking-UnbendPlot.mrc
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
      echo "# IMAGE-IMPORTANT: ManualMasking-CCmap.mrc <XCF Map for Manual Masking>" >> LOGS/${scriptname}.results
      echo "# IMAGE-IMPORTANT: ManualMasking-UnbendPlot.mrc <Distortion Plot for Manual Masking>" >> LOGS/${scriptname}.results
    endif
    #
  else
    #
    ${proc_2dx}/${lincommand} "attention, masking image"
    #
    \rm -f m${imagename}.mrc
    \rm -f TMP-quadserch-1.mrc
    \rm -f TMP-quadserch-2.mrc
    \rm -f TMP-quadserch-3.mrc
    \rm -f TMP-quadserch-4.mrc
    \rm -f TMP-quadserch-5.mrc
    \rm -f TMP-quadserch-6.mrc
    \rm -f TMP-quadserch-7.mrc
    #
    \rm -f CCPLOT.PS
    if ( ${createmaskinfo} == 'y' ) then
      set createmask = '1'
      \rm -f ManualMasking-CCmap.mrc
      \rm -f ManualMasking-UnbendPlot.mrc
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
${imagename}.mrc
m${imagename}.mrc
256       
0
eot
    #
    if ( -e TMP-quadserch-6.mrc ) then
      \mv -f TMP-quadserch-6.mrc CUT/${imagename}-masking.mrc
      \mv -f TMP-quadserch-7.mrc CUT/${imagename}-masking-final.mrc
      \cp -f CUT/${imagename}-masking-final.mrc ${imagename}-masking-final.mrc
      if ( ${tempkeep} == 'y' ) then
        echo "# IMAGE-IMPORTANT: TMP-quadserch-1.mrc <Masking file 1>" >> LOGS/${scriptname}.results 
        echo "# IMAGE-IMPORTANT: TMP-quadserch-2.mrc <Masking file 2>" >> LOGS/${scriptname}.results 
        echo "# IMAGE-IMPORTANT: TMP-quadserch-3.mrc <Masking file 3>" >> LOGS/${scriptname}.results 
        echo "# IMAGE-IMPORTANT: TMP-quadserch-4.mrc <Masking file 4>" >> LOGS/${scriptname}.results 
        echo "# IMAGE-IMPORTANT: TMP-quadserch-5.mrc <Masking file 5>" >> LOGS/${scriptname}.results 
      else
        \rm -f TMP-quadsearch-1.mrc
        \rm -f TMP-quadsearch-2.mrc
        \rm -f TMP-quadsearch-3.mrc
        \rm -f TMP-quadsearch-4.mrc
        \rm -f TMP-quadsearch-5.mrc
      endif
      echo "# IMAGE-IMPORTANT: CUT/${imagename}-masking.mrc <Masking Area from Automatic Masking>" >> LOGS/${scriptname}.results 
      echo "# IMAGE-IMPORTANT: CUT/${imagename}-masking-final.mrc <Masking Filter from Automatic Masking>" >> LOGS/${scriptname}.results 
    else
      ${proc_2dx}/protest "unbend2: ERROR: TMP-quadserch-6.mrc does not exist."
    endif
  endif
  \mv -f CCPLOT.PS PS/${imagename}-quadserch2b.ps
  if ( ${locround} == '1' ) then
    echo "# IMAGE-IMPORTANT: PS/${imagename}-quadserch2b.ps <PS: Vector Plot of Distortion, Pass 2>" >> LOGS/${scriptname}.results 
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
    #                 cor${imagename}.cor =>  auto${imagename}.map  +  prof${imagename}.dat #
    #                                                                           #
    #############################################################################
    #
    #
    \rm -f SCRATCH/prof${imagename}.dat
    setenv PROFILE  SCRATCH/auto${imagename}.map.mrc
    setenv PROFDATA SCRATCH/prof${imagename}.dat
    setenv ERRORS   SCRATCH/errout${imagename}.dat
    #
    \rm -f CCPLOT.PS
    #
    if ( ${createmaskinfo} == 'y' ) then
      set createmask = '1'
      \rm -f ManualMasking-CCmap.mrc
      \rm -f ManualMasking-UnbendPlot.mrc
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
SCRATCH/cor1${imagename}.cor.mrc
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
      \rm -f m${imagename}.mrc  
      \rm -f TMP-quadserch-1.mrc
      \rm -f TMP-quadserch-2.mrc
      \rm -f TMP-quadserch-3.mrc
      \rm -f TMP-quadserch-4.mrc
      \rm -f TMP-quadserch-5.mrc
      \rm -f TMP-quadserch-6.mrc
      \rm -f TMP-quadserch-7.mrc
      \rm -f SPIDERCOORD.spi
      #
      ${bin_2dx}/2dx_quadserchk-2.exe << eot
2,${quadpredb}                     ! IPASS,NRANGE
SCRATCH/cor1${imagename}.cor.mrc
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
${imagename}.mrc
m${imagename}.mrc
-1
0
eot
      #
      if ( -e TMP-quadserch-6.mrc ) then
        \mv -f TMP-quadserch-6.mrc CUT/${imagename}-masking.mrc
        \mv -f TMP-quadserch-7.mrc CUT/${imagename}-masking-final.mrc
        \cp -f CUT/${imagename}-masking-final.mrc ${imagename}-masking-final.mrc
        if ( ${locround} == '1' ) then
          echo "# IMAGE-IMPORTANT: CUT/${imagename}-masking.mrc <Masking Area from Automatic Masking>" >> LOGS/${scriptname}.results 
          echo "# IMAGE-IMPORTANT: CUT/${imagename}-masking-final.mrc <Masking Filter from Automatic Masking>" >> LOGS/${scriptname}.results 
        endif
      else
        ${proc_2dx}/protest "unbend2: ERROR: TMP-quadserch-6.mrc does not exist."
      endif
      #
    endif
    #
    if ( ${createmaskinfo} == 'y' ) then
      echo "# IMAGE-IMPORTANT: ManualMasking-CCmap.mrc <XCF Map for Manual Masking>" >> LOGS/${scriptname}.results
      echo "# IMAGE-IMPORTANT: ManualMasking-UnbendPlot.mrc <Distortion Plot for Manual Masking>" >> LOGS/${scriptname}.results
    endif
    #
    \mv -f CCPLOT.PS PS/${imagename}-quadserch2c.ps
    if ( ${locround} == '1' ) then
      echo "# IMAGE-IMPORTANT: PS/${imagename}-quadserch2c.ps <PS: Vector Plot for Distortions, Pass 2>" >> LOGS/${scriptname}.results 
    endif
    #
    if ( ${tempkeep} == 'y' ) then
      \mv -f SCRATCH/cor2${imagename}.cor.mrc SCRATCH/cor2${imagename}.cor.unbend2.mrc
      if ( ${locround} == '1' ) then
        echo "# IMAGE: SCRATCH/cor2${imagename}.cor.unbend2.mrc <XCF Map between Reference 2 and Image>" >> LOGS/${scriptname}.results 
      endif
    else
      \rm -f SCRATCH/cor2${imagename}.cor.mrc
    endif
  endif
  #
  \mv -f SCRATCH/cor1${imagename}.cor.mrc SCRATCH/cor1${imagename}.cor.unbend2.mrc
  if ( ${locround} == '1' ) then
    echo "# IMAGE: SCRATCH/cor1${imagename}.cor.unbend2.mrc <XCF Map between Reference 1 and Image>" >> LOGS/${scriptname}.results 
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
  \mv -f SPIDERCOORD.spi ${imagename}-unitcells-spider.doc
  echo "# IMAGE: ${imagename}-unitcells-spider.doc <SPIDER document with unit cell locations>" >> LOGS/${scriptname}.results
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "gzip - to compact profile to save space on the harddrive"
  #                                                                           #
  #############################################################################
  #
  \cp -f SCRATCH/prof${imagename}.dat ${imagename}-profile.dat
  #
  \rm -rf ${imagename}-profile.dat.gz
  #
  gzip ${imagename}-profile.dat
  #
  #############################################################################
  #                                                                           #
  # Exit if automatic masking was done.                                       #
  #                                                                           #
  #############################################################################
  #
  if ( ${domask} == 'y' ) then
    \rm -f m${imagename}.spt
    \ln -s ${imagename}.spt m${imagename}.spt
    echo "# IMAGE: ${imagename}.mrc" >> LOGS/${scriptname}.results
    echo "# IMAGE: m${imagename}.mrc" >> LOGS/${scriptname}.results
    echo "set imagename = m${nonmaskimagename}" >> LOGS/${scriptname}.results
    ${proc_2dx}/linblock "="
    ${proc_2dx}/linblock "#"
    ${proc_2dx}/linblock "#"
    ${proc_2dx}/linblock "Image now masked, name was changed to m${imagename}"
    ${proc_2dx}/linblock "SpotList link created: m${imagename}.spt -> ${imagename}.spt"
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
  # prof${imagename}.dat  +  ${imagename}.mrc      =>  cor${imagename}.mrc    #
  #                                                                           #
  #############################################################################
  #
  \rm -f SCRATCH/cor${imagename}.notap.mrc
  setenv CCORDATA SCRATCH/prof${imagename}.dat
  \rm -f SCRATCH/ccunbend-table-${imagename}.dat
  setenv TABLEOUT SCRATCH/ccunbend-table-${imagename}.dat
  #
  echo facthresha=${facthresha}
  echo TLTAXIS=${TLTAXIS}
  echo imagename=${imagename}
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
${imagename}.mrc
${IMAXCOR},${ISTEP_h},${NNUM},${ROFFSET}	 !IMAXCOR,ISTEP,NNUM,ROFFSET
0.001,${facthresha},${RMAG} !EPS,FACTOR,RMAG
SCRATCH/cor${imagename}.notap.mrc
UNBENT,PASS,2,${date}
eot
      #
    else
      #
      ${proc_2dx}/${lincommand} "2dx_ccunbendk - to unbend the original image"
      ${bin_2dx}/2dx_ccunbendk.exe << eot
${imagename}.mrc
${ITYPE},1,${IMAXCOR},${ISTEP},F,40,T       !ITYPE,IOUT,IMAXCOR,ISTEP,LTAPER,RTAPER,LTABOUT
30,52,0.001,${facthresha},${TLTAXIS},${RMAG},${LCOLOR}     !IKX,IKY,EPS,FACTOR,TLTAXIS,RMAG,LCOLOR
${imagename}, UNBEND2, ${date}
SCRATCH/cor${imagename}.notap.mrc
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
SCRATCH/${imagename}.ttf.mrc
${IMAXCOR},${ISTEP_h},${NNUM},${ROFFSET}	 !IMAXCOR,ISTEP,NNUM,ROFFSET
0.001,${facthresha},${RMAG} !EPS,FACTOR,RMAG
SCRATCH/cor${imagename}.notap.mrc
UNBENT,PASS,2,${date}
eot
      #
    else
      #
      ${bin_2dx}/2dx_ccunbendk.exe << eot
SCRATCH/${imagename}.ttf.mrc
${ITYPE},1,${IMAXCOR},${ISTEP},F,40,T                !ITYPE,IOUT,IMAXCOR,ISTEP,LTAPER,RTAPER,LTABOUT
30,52,0.001,${facthresha},${TLTAXIS},${RMAG},${LCOLOR}        !IKX,IKY,EPS,FACTOR,TLTAXIS,RMAG,LCOLOR
${imagename}, UNBEND2, ${date}
SCRATCH/cor${imagename}.notap.mrc
UNBENT,PASS,2,${date}
eot
      #
    endif
    #
    \rm -f fort.17
    #
    if ( ${tempkeep} == 'n' ) then
      \rm SCRATCH/${imagename}.ttf.mrc
    endif
    #
  endif
  #
  \mv -f CCPLOT.PS PS/${imagename}-ccunbend2.ps
  if ( ${locround} == '1' ) then
    echo "# IMAGE-IMPORTANT: PS/${imagename}-ccunbend2.ps <PS: Vector Plot for Unbending>"  >> LOGS/${scriptname}.results 
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
  #                          cor${imagename}.mrc  =>  cor${imagename}.tap.mrc #
  #                                                                           #
  #############################################################################
  #
  \rm -f     SCRATCH/cor${imagename}.mrc
  #
  # UNBENDING
  setenv IN  SCRATCH/cor${imagename}.notap.mrc
  #setenv IN  ${imagename}.mrc
  setenv OUT SCRATCH/cor${imagename}.mrc
  ${bin_2dx}/2dx_taperedgek.exe << eot
30,30,100,30       ! IAVER,ISMOOTH,ITAPER
eot
  #
  if ( ${tempkeep} == 'n' ) then
    \rm -f SCRATCH/cor${imagename}.notap.mrc
  else
    \mv -f SCRATCH/cor${imagename}.notap.mrc SCRATCH/cor${imagename}.notap.unbend2.mrc
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/cor${imagename}.notap.unbend2.mrc <Raw Unbent Image>" >> LOGS/${scriptname}.results 
    endif
  endif
  #
  #
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/${lincommand} "FFTRANS - to calculate FFT from image after unbending"
  #                                                                           #
  #                          cor${imagename}.tap.mrc  =>  cor${imagename}.fft #
  #                                                                           #
  #############################################################################
  #
  \rm -f FFTIR/cor${imagename}.fft.mrc
  setenv IN  SCRATCH/cor${imagename}.mrc
  setenv OUT FFTIR/cor${imagename}.fft.mrc
  ${bin_2dx}/2dx_fftrans.exe
  #
  if ( ${tempkeep} == 'n' ) then
    \rm -f SCRATCH/cor${imagename}.mrc
  else
    if ( ${locround} == '1' ) then
      echo "# IMAGE: SCRATCH/cor${imagename}.mrc <Unbent and Edge-Tapered Image>" >> LOGS/${scriptname}.results 
    endif
  endif
  #
  if ( ${locround} == '1' ) then
    echo "# IMAGE-IMPORTANT: FFTIR/cor${imagename}.fft.mrc <FFT of Unbent and Edge-Tapered Image>" >> LOGS/${scriptname}.results 
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
    \rm -f APH/${imagename}.nolimit.aph
    #
    ${proc_2dx}/${lincommand} "MMBOXA - to read out AMPs and PHASES, no resolution limit"
    ${bin_2dx}/2dx_mmboxa.exe << eot
FFTIR/cor${imagename}.fft.mrc
${imagenumber} ${imagename}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}.fou.nolimit.aph
SCRATCH/TMP9873.dat
U2
${refposix},${refposiy}           ! XORIG,YORIG
200.0,1.5,1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot
    #
    echo "# IMAGE-IMPORTANT: APH/${imagename}.fou.nolimit.aph <APH File (No Res.Limit) [H,K,A,P,IQ,Back,0]>" >> LOGS/${scriptname}.results
    #
  else
    #############################################################################
    #
    if ( ${ttfcorfirst} == 'n' ) then
      #
      \rm -f APH/${imagename}.fou.ttf.nolimit.aph
      #
      ${proc_2dx}/${lincommand} "TTBOXA - to read out AMPs and PHASES with TTF-correction"
      ${bin_2dx}/2dx_ttboxk.exe << eot
FFTIR/cor${imagename}.fft.mrc
${imagenumber} ${imagename}, Unbend2, ${date}
Y                        ! generate grid from lattice
N                        ! generate points from lattice
N                        ! list points as calculated
Y                        ! plot output
${imagesidelength},${imagesidelength},${stepdigitizer},${magnification},${CS},${KV} ! ISIZEX,ISIZEY,DSTEP,MAG,CS,KV
${defocus},${TLTAXIS},${TLTANG} ! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
2,0,30,30,19,19          ! IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}.fou.ttf.nolimit.aph
SCRATCH/TMP9873.dat
U2
200.0,3.0,${refposix},${refposiy},90.0 !RSMN,RSMX,XORIG,YORIG,SEGMNT
${lattice}                  ! reciprocal lattice vectors in pixels
eot
      #
      echo "# IMAGE-IMPORTANT: APH/${imagename}.fou.ttf.nolimit.aph <APH File after TTF correction (No Res.Limit) [H,K,A,P,IQ,Back,0])>" >> LOGS/${scriptname}.results
      #
      cd APH
      \rm -f ${imagename}.fou.cor.aph
      \ln -s ${imagename}.fou.ttf.nolimit.aph ${imagename}.fou.cor.aph
      cd ..
      #
      \mv -f TTPLOT.PS PS/${imagename}.ttplot.nolimit.unbend2.ps
      if ( ${locround} == '1' ) then
        echo "# IMAGE-IMPORTANT: PS/${imagename}.ttplot.nolimit.unbend2.ps <PS: IQ Plot after TTF correction (No Resolution Limit)>" >> LOGS/${scriptname}.results 
      endif
      #
    else
      #
      #############################################################################
      #
      \rm -f APH/${imagename}.fou.ttf.nolimit.aph
      #
      ${proc_2dx}/${lincommand} "MMBOXA - to read out AMPs and PHASES without TTF-correction"
      ${proc_2dx}/${lincommand} "         (because image is already TTF corrected)"
      ${bin_2dx}/2dx_mmboxa.exe << eot
FFTIR/cor${imagename}.fft.mrc
${imagenumber} ${imagename}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}.fou.ttf.nolimit.aph
SCRATCH/TMP9873.dat
U2
${refposix},${refposiy}           ! XORIG,YORIG
200.0,1.5,1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot
      #
      echo "# IMAGE-IMPORTANT: APH/${imagename}.fou.ttf.nolimit.aph <APH File after TTF correction (No Res.Limit) [H,K,A,P,IQ,Back,0]>" >> LOGS/${scriptname}.results
      #
    endif
    #
    cd APH
    \rm -f ${imagename}.fou.cor.aph
    \ln -s ${imagename}.fou.ttf.nolimit.aph ${imagename}.fou.cor.aph
    \rm -f ${imagename}.ttf.nolimit.aph
    \ln -s ${imagename}.fou.ttf.nolimit.aph ${imagename}.ttf.nolimit.aph
    cd ..
    #
    echo "# IMAGE-IMPORTANT: APH/${imagename}.cor.aph <Link to Final APH File [H,K,A,P,IQ,Back,0]>" >> LOGS/${scriptname}.results
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
  #                       cor${imagename}.fft  =>  ${imagename}.cor.limit.aph #
  #                                                                           #
  #############################################################################  
  #
  \rm -f SCRATCH/TMP9882.dat
  if ( ${ctf_ttf} == 'CTF' && ${ttfcorfirst} == 'n' ) then
    #
    \rm -f APH/${imagename}.fou.limit.aph
    #
    ${proc_2dx}/${lincommand} "MMBOXA - to read out AMPs and PHASES, with resolution limit"
    ${bin_2dx}/2dx_mmboxa.exe << eot
FFTIR/cor${imagename}.fft.mrc
${imagenumber} ${imagename}, Unbend2, ${date}
Y				! Use grid units?
Y				! Generate grid from lattice?
N				! Generate points from lattice?
2,2,0,50,50,19,19		! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}.fou.limit.aph
SCRATCH/TMP9882.dat
U2
${refposix},${refposiy}		! XORIG,YORIG
${RESMIN},${RESMAX},1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}				! Lattice vectors
eot
    #
    echo "# IMAGE-IMPORTANT: APH/${imagename}.fou.limit.aph <APH File (Res.Limit) [H,K,A,P,IQ,Back,0]>" >> LOGS/${scriptname}.results
    #
  else
    #
    if ( ${ttfcorfirst} == 'n' ) then
      #
      \rm -f APH/${imagename}.fou.ttf.limit.aph
      #
      ${proc_2dx}/${lincommand} "TTBOXA - to read out AMPs and PHASES with TTF-correction"
      #
      echo       ${bin_2dx}/2dx_ttboxk.exe
      echo  FFTIR/cor${imagename}.fft.mrc
      echo  ${imagenumber} ${imagename}, Unbend2, ${date}
      echo  Y                        
      echo  N                        
      echo  N                        
      echo  Y                        
      echo  ${imagesidelength} ${imagesidelength} ${stepdigitizer} ${magnification} ${CS},${KV}
      echo  ${defocus} ${TLTAXIS} ${TLTANG}
      echo  2 0 30 30 19 19          
      echo  APH/${imagename}.fou.ttf.limit.aph
      echo  SCRATCH/TMP9882.dat
      echo  U2
      echo  ${RESMIN} ${RESMAX} ${refposix} ${refposiy} 90.0 
      echo  ${lattice}                  
      echo  eot
      echo "Starting now:"
      #
      ${bin_2dx}/2dx_ttboxk.exe << eot
FFTIR/cor${imagename}.fft.mrc
${imagenumber} ${imagename}, Unbend2, ${date}
Y                        ! generate grid from lattice
N                        ! generate points from lattice
N                        ! list points as calculated
Y                        ! plot output
${imagesidelength} ${imagesidelength} ${stepdigitizer} ${magnification} ${CS} ${KV}
${defocus} ${TLTAXIS} ${TLTANG}
2 0 30 30 19 19          ! IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}.fou.ttf.limit.aph
SCRATCH/TMP9882.dat
U2
${RESMIN} ${RESMAX} ${refposix} ${refposiy} 90.0 !RSMN,RSMX,XORIG,YORIG,SEGMNT
${lattice}                  ! reciprocal lattice vectors in pixels
eot
      #
      echo "# IMAGE-IMPORTANT: APH/${imagename}.fou.ttf.limit.aph <APH File after TTF correction (Res.Limit) [H,K,A,P,IQ,Back,0]>" >> LOGS/${scriptname}.results
      #
      \mv -f TTPLOT.PS PS/${imagename}.ttplot.limit.unbend2.ps
      if ( ${locround} == '1' ) then
        echo "# IMAGE-IMPORTANT: PS/${imagename}.ttplot.limit.unbend2.ps <PS: IQ Plot after TTF correction (Resolution Limitation)>" >> LOGS/${scriptname}.results 
      endif
      #
    else
      #
      \rm -f APH/${imagename}.fou.ttf.limit.aph
      #
      ${proc_2dx}/${lincommand} "MMBOXA - to read out AMPs and PHASES without TTF-correction"
      ${proc_2dx}/${lincommand} "         (because image is already TTF corrected)"
      ${bin_2dx}/2dx_mmboxa.exe << eot
FFTIR/cor${imagename}.fft.mrc
${imagenumber} ${imagename}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
APH/${imagename}.fou.ttf.limit.aph
SCRATCH/TMP9882.dat
U2
${refposix},${refposiy}           ! XORIG,YORIG
${RESMIN},${RESMAX},1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot
      #
      echo "# IMAGE-IMPORTANT: APH/${imagename}.fou.ttf.limit.aph <APH File after TTF correction (Res.Limit) [H,K,A,P,IQ,Back,0]>" >> LOGS/${scriptname}.results
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

