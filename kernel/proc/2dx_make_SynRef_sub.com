#
#
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
#
#
#
  echo ":: "
  echo "::        Making synthetic reference: Mask ${loc_SYN_mask} and Bfactor ${loc_SYN_Bfact}"
  #  
  set imagecenterx = `echo ${imagesidelength} | awk '{ s = int( $1 / 2 ) } END { print s }'`
  set imagecentery = ${imagecenterx}
  #
  # set RESPLOTMAX = 0.3
  set RESPLOTMAX = `echo ${ctfplotresmax} | awk '{ if ( $1 > 0.1 ) { s = 1.0 / $1 } else { s = 0.3 } } END { print s }'`
  # 0.3 corresponds to 3.33 Angstroem for the border of the plot.  
  #
  set u1 = `echo ${lattice} | cut -d\, -f1`
  set u2 = `echo ${lattice} | cut -d\, -f2`
  set v1 = `echo ${lattice} | cut -d\, -f3`
  set v2 = `echo ${lattice} | cut -d\, -f4`
  set ulen = `echo ${u1} ${u2} | awk '{ s = sqrt ( $1 * $1 + $2 * $2 ) } END { print s }'`
  set vlen = `echo ${v1} ${v2} | awk '{ s = sqrt ( $1 * $1 + $2 * $2 ) } END { print s }'`
  #
  set latlenok = `echo ${ulen} ${vlen} | awk '{ if ( $1 + $2 < 0.1 ) { s = 0 } else { s = 1 }} END { print s }'`
  if ( ${latlenok} == '0' ) then
    ${proc_2dx}/linblock "ERROR: Lattice is ${lattice}"
    ${proc_2dx}/protest "Determine lattice first. Aborting."
  endif
  #
  set istilt = `echo $TLTANG | awk '{if ( -25.0 < $1 && $1 < 25.0 ) {s = 0} else {s = 1}} END {print s}'`
  echo istilt = $istilt  
  #
  if ( ${istilt} == '0' ) then
    set istilt = 'n'
  else
    set istilt = 'y'
  endif
  #
  echo stepdigitizer = ${stepdigitizer}
  echo defocus = ${defocus}
  echo realcell = $realcell
  echo lattice = ${lattice}
  echo ALAT = $ALAT
  echo imagesidelength = ${imagesidelength}
  #
  echo tempkeep = ${tempkeep}
  #
  echo TLTAXIS = ${TLTAXIS}
  echo TLTANG  = ${TLTANG}
  echo TANGL   = ${TANGL}
  echo TAXA    = ${TAXA}
  #
  echo radlim = ${radlim}
  echo CS = ${CS}
  echo KV = ${KV}
  echo magnification = ${magnification}
  echo phacon = ${phacon}
  echo RESMIN = ${RESMIN}
  echo RESMAX = ${RESMAX}
  #
  set SYN_RefRESMIN = ${RESMIN}
  set SYN_RefRESMAX = ${RESMAX}
  #
  echo SYN_RefRESMIN = ${SYN_RefRESMIN}
  echo SYN_RefRESMAX = ${SYN_RefRESMAX}
  #
  # Test if reference is existing:
  set mergedat = "../merge/merge3Dref_MRClefthanded.mtz"
  if ( ! -e ${mergedat} ) then
    # set mergedat = "../../merge/merge3Dref_p1_maketran_MRClefthanded.mtz"
    set mergedat = "../../merge/merge3Dref_MRClefthanded.mtz"
    if ( ! -e ${mergedat} ) then
      set mergedat = "../merge/merge2Dref_MRClefthanded.mtz"
      if ( ! -e ${mergedat} ) then
        set mergedat = "../../merge/merge2Dref_MRClefthanded.mtz"
        if ( ! -e ${mergedat} ) then
          ${proc_2dx}/linblock "ERROR. No merged reference dataset found."
          ${proc_2dx}/linblock "ERROR. Neither ../merge/merge3Dref_MRClefthanded.mtz nor ../../merge/merge3Dref_MRClefthanded.mtz exist."
          ${proc_2dx}/protest "ERROR. Neither ../merge/merge2Dref_MRClefthanded.mtz nor ../../merge/merge2Dref_MRClefthanded.mtz exist."
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
  echo ": oxoy = "${oxoy}
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
  if ( ${rot180} == 'y' ) then
    set rot180val = '1'
  else
    set rot180val = '0'
  endif
  #
  if ( ${rot90} == 'y' ) then
    set rot90val = '1'
  else
    set rot90val = '0'
  endif
  #
  if ( ${revhk} == 'y' ) then
    set revhkval = '1'
  else
    set revhkval = '0'
  endif
  #
  if ( ${revhnd} == 'y' ) then
    set revhndval = '1'
  else
    set revhndval = '0'
  endif 
  #
  if ( ${ctfrev} == 'y' ) then
    set ctfrevval = '1'
  else
    set ctfrevval = '0'
  endif
  #
  echo sgnxch = ${sgnxch}
  if ( ${sgnxch} == "y" ) then
    set sgnxchval = 1
    set phaoriFouFilterx = `echo ${phaoriFouFilter} | cut -d\, -f1 `
    set phaoriFouFiltery = `echo ${phaoriFouFilter} | cut -d\, -f2 | awk '{ s = -$1 } END { print s }'`
    set phaoriFouFilter = `echo ${phaoriFouFilterx},${phaoriFouFiltery}`
  else
    set sgnxchval = 0
  endif
  #
  echo revxsgn = ${revxsgn}
  if ( ${revxsgn} == "y" ) then
    set revxsgnval = 1
    set phaoriFouFilterx = `echo ${phaoriFouFilter} | cut -d\, -f1 | awk '{ s = -$1 } END { print s }'`
    set phaoriFouFiltery = `echo ${phaoriFouFilter} | cut -d\, -f2 `
    set phaoriFouFilter = `echo ${phaoriFouFilterx},${phaoriFouFiltery}`
  else
    set revxsgnval = 0
  endif
  #
  source ${proc_2dx}/2dx_makedirs
  #
  \rm -f ${imagename}_int.mrc
  #
  #############################################################################
  ${proc_2dx}/linblock "MAKETRAN - create synthetical transform from MTZ data"
  #    OX and OY should be negative of values in normal JOBB to make central  #
  #    deviation zero.                                                        #
  ############################################################################# 
  #
  \rm -f SCRATCH/reference_fft.mrc
  setenv HKLIN $mergedat
  echo "HKLIN =" ${mergedat}
  setenv SPOTSOUT make${imagename}.spt
  #
  echo " "
  ${proc_2dx}/lin "Protein is dark in the images before CTF correction."  
  #
  set locdef = ${defocus}
  if ( ${ctfcor_imode}x == '1x' || ${ctfcor_imode}x == '2x' || ${ctfcor_imode}x == '3x' ) then
    #
    set locdef = '0.0,0.0,0.0'
    #
    echo "If maketran is used with a defocus of 0, it will produce a FT with inverted contrast."
    echo "The locfactor therefore has to be -1."
    #  
    set locfactor = `echo ${locfactor} | awk '{ s = - $1 } END { print s }'`
    echo locfactor = $locfactor
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
  echo "0 2 F ${loc_SYN_mask}                                  ! NPROG,ISHAPE (1circ,2gauss,3square),IAMPLIM,RAD"
  echo "$imagesidelength $imagesidelength $stepdigitizer $magnification                      ! NX NY DSTEP XMAG"
  echo "$lattice $revhkval ${sgnxchval} $rot180val $rot90val ${revhndval}  ${ctfrevval} ${phacon} ! AX,AY,BX,BY,REVHK,SGNXCH,ROT180,ROT90,REVHND,CTFREV,PHACON"
  echo "${oxoy} ${beamtilt} $TAXA $TANGL $realcell ${realang}   ! OX OY TX TY TAXA TANGL A B GAMMA"
  echo "$SYN_RefRESMIN $SYN_RefRESMAX                                ! RESMIN RESMAX, resolution limits (Angstroms)"
  echo "${locdef} ${CS} ${KV}           ! DFMID1 DFMID2 ANGAST CS KV"
  echo "SCRATCH/reference_fft.mrc"
  echo "${mergedat}                        ! projection used for reference"
  echo "${spcgrp_maketran} T ${locfactor} ${loc_SYN_Bfact}                              ! spacegroup,LFPZERO,scale+temp_factor"
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
0 2 F ${loc_SYN_mask}     ! NPROG,ISHAPE (1circ,2gauss,3square),IAMPLIM,RAD
$imagesidelength $imagesidelength $stepdigitizer $magnification    ! NX NY DSTEP XMAG
$lattice $revhkval ${sgnxchval} $rot180val $rot90val ${revhndval} ${ctfrevval} ${phacon}  ! AX,AY,BX,BY,REVHK,SGNXCH,ROT180,ROT90,REVHND,CTFREV,PHACON
${oxoy} ${beamtilt} $TAXA $TANGL $realcell ${realang}                ! OX OY TX TY TAXA TANGL A B GAMMA
$SYN_RefRESMIN $SYN_RefRESMAX                                      ! RESMIN RESMAX, resolution limits (Angstroms)
${locdef} ${CS} ${KV}                                              ! DFMID1 DFMID2 ANGAST CS KV
SCRATCH/reference_fft.mrc
${mergedat}
${spcgrp_maketran} T ${locfactor} ${loc_SYN_Bfact}    
LABIN AMP=F PHASE=PHI FOM=FOM 
eot
  #
  if ( ! -e SCRATCH/reference_fft.mrc ) then
    ${proc_2dx}/linblock "#"
    ${proc_2dx}/linblock "ERROR: SCRATCH/reference_fft.mrc not created."
    ${proc_2dx}/linblock "Problem in 2dx_maketrana.exe"
    ${proc_2dx}/linblock "#"
    ${proc_2dx}/linblock "Are the parameter, e.g. real-space lattice, the same in the merged dataset as here?"
    ${proc_2dx}/protest "Aborting."
  endif
  #
  #
  ###############################################################
  ${proc_2dx}/linblock "FFTRANS - Producing reference in real space, for debugging"
  ###############################################################
  #
  setenv IN  SCRATCH/reference_fft.mrc
  setenv OUT SCRATCH/reference.mrc
  \rm -f     SCRATCH/reference.mrc
  ${bin_2dx}/2dx_fftrans.exe 
  #
  #
  #

