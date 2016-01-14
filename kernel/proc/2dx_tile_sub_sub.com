#
#  This is not an independent script.  It should be called by another script.
#
#
#
#
  \rm -f ${ctfcor_ctffile}
  \rm -f ${ctfcor_outfile}
  #
  # ${bin_2dx}/2dx_ctfcor.exe << eot
  ${bin_2dx}/2dx_ctfcor_stripes.exe << eot
${inimage}
${ctfcor_outfile}
${ctfcor_ctffile}
${TLTAXIS},${TLTANG}
${CS},${KV},${phacon},${magnification},${stepdigitizer}
${defocus}
${RESMAX}
${ctfcor_noise}
${imode}
${ctfcor_debug}
${ctfcor_maxamp_factor}
eot
  #
  echo "# IMAGE: ${olddir}/../${from_dir}/${ctfcor_outfile} <Output Image CTF corrected>" >>  ${olddir}/LOGS/${scriptname}.results
  if ( ${imode} == "2" ) then
    echo "# IMAGE: ${olddir}/../${from_dir}/${ctfcor_ctffile} <Summed CTF**2 file>" >>  ${olddir}/LOGS/${scriptname}.results
  else
    echo "# IMAGE: ${olddir}/../${from_dir}/${ctfcor_ctffile} <Summed CTF file>" >>  ${olddir}/LOGS/${scriptname}.results
  endif
  #
  ###########################################################################
  ${proc_2dx}/lin "LABELH - Normalizing image to AVG=0, STDEV=100"
  ###########################################################################
  #
  \rm -f SCRATCH/image_ctf_upscale.mrc
  ${bin_2dx}/labelh.exe << eot
${ctfcor_outfile}
39
SCRATCH/image_ctf_upscale.mrc
eot
  #
  foreach num_x ( ${tileseries} )
    foreach num_y ( ${tileseries} )
      #
      set locimagenumber = `echo ${locimagenumber} ${num_x} ${num_y} ${image_type} | awk '{ s = $1 * 10 + 100 * $2 + 10 * $3 + $4 } END { print s }'`
      set newimagenumber = `echo ${locimagenumber} | ${bin_2dx}/2dx_getnumber.exe`
      set testval = `echo ${newimagenumber} | wc -c`
      if ( `echo ${testval} | awk '{ if ( $1 < 11 ) { s = 1 } else { s = 0 }} END { print s }'` == 1 ) then
        set oldval = ${newimagenumber}
        set newimagenumber = `echo 0000000000 | cut -c${testval}-`${newimagenumber}
      endif  
      #
      cd ${target_base}
      #
      set newdir = TILE_${newimagenumber}
      if ( ! -d ${newdir} ) then
        \mkdir ${newdir}
        cp ${olddir}/../${from_dir}/2dx_image.cfg ${newdir}
      endif
      #
      cd ${newdir}
      echo "set imagenumber = ${newimagenumber}" >> 2dx_image.cfg
      echo "set lattice = ${newlattice}" >> 2dx_image.cfg
      echo "set imagesidelength = ${newimagesidelength}" >> 2dx_image.cfg
      #
      set offset = `echo ${imagesidelength} ${newimagesidelength} ${tilenumberm1} | awk ' { s = int ( ( $1 - $2 ) / $3 ) } END { print s } '`
      set xmin = `echo ${offset} ${newimagesidelength} ${num_x} | awk '{ s = 1 + ( $3 - 1 ) * $1 } END { print s }'`
      set xmax = `echo ${xmin} ${newimagesidelength} | awk '{ s = $1 + $2 - 1 } END { print s }'`
      set ymin = `echo ${offset} ${newimagesidelength} ${num_y} | awk '{ s = 1 + ( $3 - 1 ) * $1 } END { print s }'`
      set ymax = `echo ${ymin} ${newimagesidelength} | awk '{ s = $1 + $2 - 1 } END { print s }'`
      echo "xmin,xmax,ymin,ymax = ${xmin},${xmax},${ymin},${ymax}"
      #
      # Calculate the difference vector of this tile from the image center:
      set xcen = `echo ${xmax} ${xmin} | awk '{ s = int ( ( $1 + $2 ) / 2 ) } END { print s }'`
      set ycen = `echo ${ymax} ${ymin} | awk '{ s = int ( ( $1 + $2 ) / 2 ) } END { print s }'`
      set ocen = `echo ${imagesidelength} | awk '{ s = int ( $1 / 2 ) } END { print s }'`
      set xdif = `echo ${ocen} ${xcen} | awk '{ s = int ( $2 - $1 ) } END { print s }'`
      set ydif = `echo ${ocen} ${ycen} | awk '{ s = int ( $2 - $1 ) } END { print s }'`
      echo "tile difference vector is ${xdif} ${ydif}"
      # 
      # Calculate the real lattice and its matrix and its determinant:
      set ux   = `echo ${lattice} | sed 's/,/ /g' | awk '{ s = $1 } END { print s }'`
      set uy   = `echo ${lattice} | sed 's/,/ /g' | awk '{ s = $2 } END { print s }'`
      set vx   = `echo ${lattice} | sed 's/,/ /g' | awk '{ s = $3 } END { print s }'`
      set vy   = `echo ${lattice} | sed 's/,/ /g' | awk '{ s = $4 } END { print s }'`
      echo "reciprocal lattice is ${ux} ${uy} ${vx} ${vy}"
      set detB = `echo ${ux} ${uy} ${vx} ${vy} | awk '{ s = $1 * $4 - $2 * $3 } END { print s }'`
      echo "reciprocal lattice determinant is ${detB}"
      set rfac = `echo ${imagesidelength} ${detB} | awk '{ s = abs ( $1 / $2 ) } END { print s }'`
      set ax   = `echo ${ux} ${uy} ${vx} ${vy} ${rfac} | awk '{ s =  $4 * $5 } END { print s }'`
      set ay   = `echo ${ux} ${uy} ${vx} ${vy} ${rfac} | awk '{ s = -$3 * $5 } END { print s }'`
      set bx   = `echo ${ux} ${uy} ${vx} ${vy} ${rfac} | awk '{ s = -$2 * $5 } END { print s }'`
      set by   = `echo ${ux} ${uy} ${vx} ${vy} ${rfac} | awk '{ s =  $1 * $5 } END { print s }'`
      set detR = `echo ${ax} ${ay} ${bx} ${by} | awk '{ s = $1 * $4 - $2 * $3 } END { print s }'`
      echo "real lattice determinant is ${detR}"
      #
      # Calculate the inverse matrix to the lattice:
      set nax  = `echo ${ax} ${ay} ${bx} ${by} ${detR} | awk '{ s =  $4 / $5 } END { print s }'`
      set nay  = `echo ${ax} ${ay} ${bx} ${by} ${detR} | awk '{ s = -$2 / $5 } END { print s }'`
      set nbx  = `echo ${ax} ${ay} ${bx} ${by} ${detR} | awk '{ s = -$3 / $5 } END { print s }'`
      set nby  = `echo ${ax} ${ay} ${bx} ${by} ${detR} | awk '{ s =  $1 / $5 } END { print s }'`
      echo "real    lattice matrix is ${ax} ${ay} ${bx} ${by}"
      echo "inverse lattice matrix is ${nax} ${nay} ${nbx} ${nby}"
      #
      # Transform the difference vector for this tile into the lattice vector space (in fractions of 1):
      set xdiB = `echo ${xdif} ${ydif} ${nax} ${nay} ${nbx} ${nby} | awk '{ s = $1 * $3 + $2 * $5 } END { print s }'`
      set ydiB = `echo ${xdif} ${ydif} ${nax} ${nay} ${nbx} ${nby} | awk '{ s = $1 * $4 + $2 * $6 } END { print s }'`
      echo "tile center in lattice vector space is ${xdiB} ${ydiB}"
      #
      # Transform this difference vector into phase origin space:
      set xdip = `echo ${xdiB} | awk '{ s = ( $1 * 360.0 ) % 360.0 } END { print s }'`
      set ydip = `echo ${ydiB} | awk '{ s = ( $1 * 360.0 ) % 360.0 } END { print s }'`
      echo "tile center in unit cell space is ${xdip} ${ydip} degrees"
      #
      # Calculate new phase origin for this tile:
      set xphaori  = `echo ${phaori} | sed 's/,/ /g' | awk '{ s = $1 } END { print s }'`
      set yphaori  = `echo ${phaori} | sed 's/,/ /g' | awk '{ s = $2 } END { print s }'`
      set nxphaor  = `echo ${xphaori} ${xdip} | awk '{ s = ( $1 + $2 ) % 360.0 } END { print s }'`
      set nyphaor  = `echo ${yphaori} ${ydip} | awk '{ s = ( $1 + $2 ) % 360.0 } END { print s }'`
      set nxphaori = `echo ${nxphaor} | awk '{ if ( $1 < 0 ) { s = $1 + 360.0 } else { s = $1 }} END { print s }'`
      set nyphaori = `echo ${nyphaor} | awk '{ if ( $1 < 0 ) { s = $1 + 360.0 } else { s = $1 }} END { print s }'`
      set nphaori = `echo ${nxphaori},${nyphaori}`
      echo "set phaori = ${nphaori}" >> 2dx_image.cfg
      echo ":old phaori is ${phaori}"
      echo ":new phaori is ${nphaori}"
      #
      # Update the 2dx_image.cfg file:
      set fullpath = `pwd`
      ${app_2dx_image} ${fullpath} "2dx_initialize"
      #
      \rm -f SCRATCH/tile_ctf.mrc
      #
      ${bin_2dx}/labelh.exe << eot
${olddir}/../${from_dir}/SCRATCH/image_ctf_upscale.mrc
1
SCRATCH/tile_ctf.mrc
${xmin},${xmax},${ymin},${ymax},1,1
eot
      #
      ###########################################################################
      ${proc_2dx}/lin "TAPEREDGE - Tapering edge of summed frames"
      ###########################################################################

      setenv IN  SCRATCH/tile_ctf.mrc
      setenv OUT tile_ctf_taper.mrc
      \rm -f     tile_ctf_taper.mrc
      ${bin_2dx}/2dx_taperedgek.exe << eot
30,30,100,30       ! IAVER,ISMOOTH,ITAPER
eot
      ###########################################################################
      ${proc_2dx}/lin "FFTRANS - Producing final FFT"
      ###########################################################################

      setenv IN tile_ctf_taper.mrc
      setenv OUT SCRATCH/tile_ctf_taper_fft.mrc
      \rm -f     SCRATCH/tile_ctf_taper_fft.mrc
      ${bin_2dx}/2dx_fftrans.exe
      echo "# IMAGE: tile_ctf_taper.mrc <TILE ${imagenumber}>" >>  ${olddir}/LOGS/${scriptname}.results
      echo "# IMAGE: SCRATCH/tile_ctf_taper_fft.mrc <TILE ${imagenumber} FFT>" >>  ${olddir}/LOGS/${scriptname}.results
      \rm -f SCRATCH/tile_ctf.mrc

      ###########################################################################
      ${proc_2dx}/lin "MMBOX - Evaluating APH values"
      ###########################################################################

      \rm -f SCRATCH/TMP9873.dat
      set CTF_outfile = APH/${CTF_outfile_nA}
      \rm -f ${CTF_outfile}
      set algo = U2
      set imagecenterx = `echo ${newimagesidelength} | awk '{ s = int( $1 / 2 ) } END { print s }'`
      set imagecentery = ${imagecenterx}
 
      ${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/tile_ctf_taper_fft.mrc
${imagenumber} ${imagename}, CTFcor_Mode=${imode}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
${CTF_outfile}
SCRATCH/TMP9873.dat
${algo}
${imagecenterx},${imagecentery}           ! XORIG,YORIG
200.0,1.5,1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${newlattice}                         ! Lattice vectors
eot
      #
      cd APH
      \rm -f image_ctfcor_ctf.aph
      \ln -s ${CTF_outfile_nA} image_ctfcor_ctf.aph
      cd ..
      #
      cd ${olddir}
      cd ..
      cd ${from_dir} 
    end
  end
  #
