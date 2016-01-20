#
#  This is not an independent script.  It should be called by another script.
#
#
#
if ( ${imode} == "7" ) then
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
else
  \cp -f ${inimage} SCRATCH/image_ctf_upscale.mrc
endif
#
foreach num_row ( ${tileseries} )
   foreach num_column ( ${tileseries} )
      #
      set tmpimagenumber = `echo ${locimagenumber} ${num_row} ${num_column} ${image_type} | awk '{ s = $1 * 10 + 100 * $2 + 10 * $3 + $4 } END { print s }'`
      set newimagenumber = `echo ${tmpimagenumber} | ${bin_2dx}/2dx_getnumber.exe`
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
      # Calculate the offset between tiles:
      set offset = `echo ${imagesidelength} ${newimagesidelength} ${tilenumberm1} | awk ' { s = int ( ( $1 - $2 ) / $3 ) } END { print s } '`
      #
      # Calculate the coordinates of the new tile, with (0,0) being lower left corner or the old image:
      set xmin = `echo ${offset} ${newimagesidelength} ${num_row} | awk '{ s = 0 + ( $3 - 1 ) * $1 } END { print s }'`
      set xmax = `echo ${xmin} ${newimagesidelength} | awk '{ s = $1 + $2 - 1 } END { print s }'`
      set ymin = `echo ${offset} ${newimagesidelength} ${num_column} | awk '{ s = 0 + ( $3 - 1 ) * $1 } END { print s }'`
      set ymax = `echo ${ymin} ${newimagesidelength} | awk '{ s = $1 + $2 - 1 } END { print s }'`
      echo "xmin,xmax,ymin,ymax = ${xmin},${xmax},${ymin},${ymax} pixels"
      #
      # Calculate the difference vector of this tile from the image center, in pixels:
      set xcen = `echo ${xmax} ${xmin} | awk '{ s = int ( ( $1 + $2 ) / 2 ) } END { print s }'`
      set ycen = `echo ${ymax} ${ymin} | awk '{ s = int ( ( $1 + $2 ) / 2 ) } END { print s }'`
      set ocen = `echo ${imagesidelength} | awk '{ s = int ( $1 / 2 ) } END { print s }'`
      set xdif = `echo ${ocen} ${xcen} | awk '{ s = int ( $2 - $1 ) } END { print s }'`
      set ydif = `echo ${ocen} ${ycen} | awk '{ s = int ( $2 - $1 ) } END { print s }'`
      echo "tile difference vector is ${xdif} ${ydif}  pixels"
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
      echo "tile center in lattice vector space is ${xdiB} ${ydiB}  unit cells"
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
      # Transform the difference vector for this tile from pixels into Angstroems:
      set xdifA = `echo ${xdif} ${sample_pixel} | awk '{ s = $1 * $2 } END { print s }'`
      set ydifA = `echo ${ydif} ${sample_pixel} | awk '{ s = $1 * $2 } END { print s }'`
      echo "tile center in Angstroms is ${xdifA} ${ydifA}"
      #
      # Calculate the height difference for this tile center, in Angstroems:
      #
      # Calculate the distance between image center and tile center:   rdist1 = sqrt((xdifA)**2 + (ydifA)**2)
      set rdist1 = `echo ${xdifA} ${ydifA} | awk '{ s = sqrt( $1 * $1 + $2 * $2 ) } END { print s }'`
      echo "rdist1 = ${rdist1} = distance of tile center in Angstroms"
      #
      # Calculate the angle "beta" between X-axis and line from image-center to tile-center:   rbeta = atan2(ydifA,xdifA))*180.0/PI
      set rbeta = `echo ${xdifA} ${ydifA} | awk '{ s = atan2( $2 , $1 ) * 180.0 / 3.141592654 } END { print s }'`
      echo "rbeta = ${rbeta}"
      #
      # Calculate the angle between tilt axis and line from image-center to tile-center:   rgamma = beta - TLTAXIS
      set rgamma = `echo ${rbeta} ${TLTAXIS} | awk '{ s = $1 - $2 } END { print s }'`
      echo "rgamma = ${rgamma}"
      #
      # Calculate the distance between tile-center and closest point on tilt axis:  rdist2 = sin(rgamma*PI/180)*rdist1
      set rdist2 = `echo ${rgamma} ${rdist1} | awk '{ s = sin( $1 * 3.141592654 / 180.0) * $2 } END { print s }'`
      echo "rdist2 = ${rdist2} = distance between tile and tilt axis in Angstroms"
      #
      echo ":TLTAXIS = ${TLTAXIS}, TLTANG = ${TLTANG}"
      # Calculate the defocus offset for this tile:  rdist3 * tan(TLTANG)
      set height_offset = `echo ${rdist2} ${TLTANG} | awk '{ s = $1 * sin( $2 * 3.141592654 / 180.0 ) / cos( $2 * 3.141592654 / 180.0 ) } END { print s }'`
      echo ":Defocus height offset for this tile ${num_row} ${num_column} is ${height_offset} Angstroms"
      #
      # Calculate the new defocus for this tile:
      set newdefx = `echo ${defocus} ${height_offset} | sed 's/,/ /g' | awk '{ s = $1 + $4 } END { print s }'`
      set newdefy = `echo ${defocus} ${height_offset} | sed 's/,/ /g' | awk '{ s = $2 + $4 } END { print s }'`
      set newdefa = `echo ${defocus} ${height_offset} | sed 's/,/ /g' | awk '{ s = $3      } END { print s }'`
      set newdefocus = `echo ${newdefx},${newdefy},${newdefa}`
      echo ":old defocus: ${defocus}.   new defocus: ${newdefocus}"
      echo "set defocus = ${newdefocus}" >> 2dx_image.cfg
      #
      # Update the 2dx_image.cfg file:
      set fullpath = `pwd`
      ${app_2dx_image} ${fullpath} "2dx_initialize"
      #
      ###########################################################################
      ${proc_2dx}/lin "LABELH - to crop tile from unbent and masked image"
      ###########################################################################
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
      ${proc_2dx}/lin "LABELH - to crop tile from original image"
      ###########################################################################
      #
      \rm -f SCRATCH/nonmasktile.mrc
      #
      ${bin_2dx}/labelh.exe << eot
${olddir}/../${from_dir}/${nonmaskimagename}.mrc
1
SCRATCH/nonmasktile.mrc
${xmin},${xmax},${ymin},${ymax},1,1
eot
      #
      ###########################################################################
      ${proc_2dx}/lin "TAPEREDGE - Tapering edge of summed frames"
      ###########################################################################
      #
      setenv IN  SCRATCH/tile_ctf.mrc
      setenv OUT tile_ctf_taper.mrc
      \rm -f     tile_ctf_taper.mrc
      ${bin_2dx}/2dx_taperedgek.exe << eot
30,30,100,30       ! IAVER,ISMOOTH,ITAPER
eot
      #
      ###########################################################################
      ${proc_2dx}/lin "FFTRANS - Producing FFT of unbent crystal image tile"
      ###########################################################################
      #
      setenv IN tile_ctf_taper.mrc
      setenv OUT SCRATCH/tile_ctf_taper_fft.mrc
      \rm -f     SCRATCH/tile_ctf_taper_fft.mrc
      ${bin_2dx}/2dx_fftrans.exe
      echo "# IMAGE: tile_ctf_taper.mrc <TILE ${imagenumber}>" >>  ${olddir}/LOGS/${scriptname}.results
      echo "# IMAGE: SCRATCH/tile_ctf_taper_fft.mrc <TILE ${imagenumber} FFT>" >>  ${olddir}/LOGS/${scriptname}.results
      \rm -f SCRATCH/tile_ctf.mrc
      #
      ###########################################################################
      ${proc_2dx}/lin "FFTRANS - Producing FFT of oroginal image tile"
      ###########################################################################
      #
      setenv IN SCRATCH/nonmasktile.mrc
      setenv OUT SCRATCH/nonmasktile_fft.mrc
      \rm -f     SCRATCH/nonmasktile_fft.mrc
      ${bin_2dx}/2dx_fftrans.exe
      #
      #
      #
      \rm -f SCRATCH/TMP9873.dat
      set CTF_outfile = APH/${CTF_outfile_nA}
      \rm -f ${CTF_outfile}
      set algo = U2
      set imagecenterx = `echo ${newimagesidelength} | awk '{ s = int( $1 / 2 ) } END { print s }'`
      set imagecentery = ${imagecenterx}
   
      if ( ${imode} == "7" ) then
        #
        ###########################################################################
        ${proc_2dx}/lin "MMBOX - Evaluating APH values"
        ###########################################################################

        ${bin_2dx}/2dx_mmboxa.exe << eot
SCRATCH/tile_ctf_taper_fft.mrc
${imagenumber} ${imagename}, CTFcor_Mode=${imode}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,13,13               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
${CTF_outfile}
SCRATCH/TMP9873.dat
${algo}
${imagecenterx},${imagecentery}           ! XORIG,YORIG
200.0,1.5,1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${newlattice}                         ! Lattice vectors
eot
        #
      else
        #
        ###########################################################################
        ${proc_2dx}/lin "CTFFIND3 - To verify defocus"
        ###########################################################################
        #
        if ( ${use_paralellized} == "y" ) then
          set progname = 2dx_ctffind3.exe
        else
          set progname = 2dx_ctffind3_noOMP.exe
        endif
        #
        set inimage  = SCRATCH/nonmasktile.mrc
        set outimage = SCRATCH/nonmasktile_ctffind3.mrc
        \rm -f ${outimage}
        set outlabel = "TILE-PS"
        set dfmid    = `echo ${newdefocus} | sed 's/,/ /g' | awk '{s = ($1 + $2 ) / 2 } END {print s}'`
        set dfstart  = `echo ${dfmid} | awk '{ s = $1 - 1000.0 } END { print s }'`
        set dfend    = `echo ${dfmid} | awk '{ s = $1 + 1000.0 } END { print s }'`
        set dfstep   = 100.0
        set df_dast  = 300.0
        # Do not search for astigmatism here, only one-dimensional defocus check:
        set inoast   = 1
        set dfref    = `echo ${newdefocus}`
        set drms1    = 0.0
        set sub_tilesize = 128
        set resolim  = ${defocus_res_max}
        set resolim  = 150.0
        echo resolim = ${resolim}
        set resoma   = ${defocus_res_min}
        set resoma   = 4.0
        echo resoma  = ${resoma}
  

        echo " "
        echo "Calling:"  
        echo "${bin_2dx}/${progname}"
        echo "${inimage}"
        echo "${outimage}"
        echo "${CS},${KV},${ampcon},${magnification},${stepdigitizer}"
        echo "${sub_tilesize},${resoma},${resolim},${dfstart},${dfend},${dfstep},${df_dast}"
        echo "${inoast},${dfref},${drms1}"
        echo " "
        #
        ${bin_2dx}/${progname} << eof
${inimage}
${outimage}
${CS},${KV},${ampcon},${magnification},${stepdigitizer}
${sub_tilesize},${resoma},${resolim},${dfstart},${dfend},${dfstep},${df_dast}
${inoast},${dfref},${drms1}
eof
        #
        set corrected_defocus_tmp = `cat SCRATCH/2dx_ctffind3_result.tmp | head -n 1`
        set corrected_defocus = `echo ${corrected_defocus_tmp} | sed 's/ /,/g'`
        set DRMS1 = `cat SCRATCH/2dx_ctffind3_result.tmp | head -n 2 | tail -n 1`
        echo "::Old Defocus = ${defocus},     predicted defocus = ${newdefocus},     corrected defocus = ${corrected_defocus},    DRMS1 = ${DRMS1}"
        set newdefocus = `echo ${corrected_defocus}`
        echo "set defocus = ${newdefocus}" >> 2dx_image.cfg
        #
        # Update the 2dx_image.cfg file:
        set fullpath = `pwd`
        ${app_2dx_image} ${fullpath} "2dx_initialize"
        #
        ###########################################################################
        ${proc_2dx}/lin "TTBOX - To evaluate APH values"
        ###########################################################################
        #
        ${proc_2dx}/linblock "TTBOXA - to read out AMPs and PHASES with TTF-correction"
        ${bin_2dx}/2dx_ttboxk.exe << eot
SCRATCH/tile_ctf_taper_fft.mrc
${imagenumber} 
Y                        ! generate grid from lattice
N                        ! generate points from lattice
N                        ! list points as calculated
N                        ! plot output
${newimagesidelength},${newimagesidelength},${stepdigitizer},${magnification},${CS},${KV} ! ISIZEX,ISIZEY,DSTEP,MAG,CS,KV
${newdefocus},${TLTAXIS},${TLTANG} ! DFMID1,DFMID2,ANGAST,TLTAXIS,TLTANGL
2,0,50,50,13,13          ! IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
${CTF_outfile}
SCRATCH/TMP9873.dat
${algo}
${RESMIN},1.0,${imagecenterx},${imagecentery},90.0 !RSMN,RSMX,XORIG,YORIG,SEGMNT
${newlattice}                  ! reciprocal lattice vectors in pixels
eot
        #
        echo "# IMAGE: ${CTF_outfile} <APH File after TTF correction [H,K,A,P,IQ,Back,0])>" >> LOGS/${scriptname}.results   
        \rm -f TTPLOT.PS
      endif
      #
      cat SCRATCH/TMP9873.dat | grep PSMAX >> 2dx_image.cfg
      # Update the 2dx_image.cfg file:
      set fullpath = `pwd`
      ${app_2dx_image} ${fullpath} "2dx_initialize"
      #
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
