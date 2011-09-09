#
#
# this is not an independent script. It should only be called from other scripts.
#
# This sub-script will make sure the ${imagename}.mrc is square, by either padding or cropping it.
# It will also make sure the image size is an allowed product of small prime factors.
#
#
#
    setenv IN ${imagename}.mrc
    set dimens = `${bin_2dx}/header.exe | awk "/Number\ of\ columns/{print $1}" | cut -c51-`
    set sizeX = `echo ${dimens} | cut -d\  -f1` 
    set sizeY = `echo ${dimens} | cut -d\  -f2` 
    if ( ${sizeX} != ${sizeY} ) then
      ${proc_2dx}/linblock "ERROR: only square images are supported (${sizeX},${sizeY})."
      echo "#WARNING: ERROR: only square images are supported."  >> LOGS/${scriptname}.results
      #
      if( ${crop} == "0" ) then
        #############################################################################
        ${proc_2dx}/linblock "Crop image into smaller square array"
        ${proc_2dx}/linblock "Crop image into smaller square array" >> History.dat
        #############################################################################
        #
        set newsize = `echo ${sizeX} ${sizeY} | awk '{ if ( $1 < $2 ) { s = $1 } else { s = $2 }} END { print s }'`
      else
        #############################################################################
        ${proc_2dx}/linblock "Pad image into larger square array"
        ${proc_2dx}/linblock "Pad image into larger square array" >> History.dat
        #############################################################################
        #
        set newsize = `echo ${sizeX} ${sizeY} | awk '{ if ( $1 > $2 ) { s = $1 } else { s = $2 }} END { print s }'`
      endif
      ${proc_2dx}/linblock "New image size will be ${newsize}"
      ${proc_2dx}/linblock "New image size will be ${newsize}" >> History.dat
      \rm -f SCRATCH/TMPnewsize.mrc
      #
      ${bin_2dx}/labelh.exe << eot
${imagename}.mrc
30
SCRATCH/TMPnewsize.mrc
${newsize}
eot
      #
        if ( ! -e ${imagename}-original.mrc ) then
        \mv -f ${imagename}.mrc ${imagename}-original.mrc
      else
        ${proc_2dx}/linblock "${imagename}-original.mrc already existing."
        ${proc_2dx}/protest "ERROR: Renaming not possible."
      endif
      \mv -f SCRATCH/TMPnewsize.mrc ${imagename}.mrc
      #
      echo "# IMAGE-IMPORTANT: ${imagename}-orignal.mrc <Original Image>"  >> LOGS/${scriptname}.results
      #
      set sizeX = ${newsize}
      #
      #############################################################################
      ${proc_2dx}/linblock "Testing new image size of ${sizeX} for prime factors"
      #############################################################################
      #
      set primesfile = SCRATCH/2dx_primes.dat  
      #
      \rm -f ${primesfile}}
      #
      ${bin_2dx}/2dx_primes.exe << eot
${sizeX}
${primesfile}
eot
      #
    else
      #############################################################################
      ${proc_2dx}/lin "Testing new image size of ${sizeX} for prime factors"
      #############################################################################
      #
      set primesfile = SCRATCH/2dx_primes.dat  
      #
      \rm -f ${primesfile}}
      #
      ${bin_2dx}/2dx_primes.exe << eot > SCRATCH/TMPprimesout.dat
${sizeX}
${primesfile}
eot
      #
      if ( -e ${primesfile} ) then
        cat SCRATCH/TMPprimesout.dat
      else
        cat SCRATCH/TMPprimesout.dat | sed 's/::/:/g'
      endif
      \rm -f SCRATCH/TMPprimesout.dat
      #
    endif
    #
    if ( -e ${primesfile} ) then
      set bettersize = `cat ${primesfile} | head -n 1`
      set cropdimensions = `cat ${primesfile} | head -n 2 | tail -n 1`
      #
      #############################################################################
      ${proc_2dx}/linblock "cutting image down into better smaller size of ${bettersize}"
      ${proc_2dx}/linblock "cutting image down into better smaller size of ${bettersize}" >> History.dat
      #############################################################################  
      #
      \rm -f SCRATCH/TMPnewsize1.mrc
      #
      ${bin_2dx}/labelh.exe << eot
${imagename}.mrc
1
SCRATCH/TMPnewsize1.mrc
${cropdimensions}
eot
      #
      ${proc_2dx}/linblock "Cropping done."
      #
      #############################################################################
      ${proc_2dx}/linblock "Pad into image with same dimensions to get the header right"
      #############################################################################
      #
      \rm -f SCRATCH/TMPnewsize2.mrc
      #
      ${bin_2dx}/labelh.exe << eot
SCRATCH/TMPnewsize1.mrc
30
SCRATCH/TMPnewsize2.mrc
${bettersize}
eot
      echo "<<@progress: 60>>"
      #
      ${proc_2dx}/lin "Padding done."
      #
     \mv -f SCRATCH/TMPnewsize2.mrc ${imagename}.mrc
      #
      set sizeX = ${bettersize}
      #
      \rm SCRATCH/TMPnewsize1.mrc
      \rm ${primesfile}
    endif
    #
    set newsize = `echo ${sizeX} | awk '{ if ( $1 < 1024 ) { s = 2 * $1 } else { s = $1 }} END { print s }'`
    #
    if ( ${newsize} != ${sizeX} ) then
      #
      #############################################################################
      echo ":: "
      echo "::WARNING: Interpolating image up to ${newsize}"
      echo ":: "
      echo "#WARNING: WARNING: Image was up-interpolated two times, to ${newsize} pixels" >> LOGS/${scriptname}.results
      #############################################################################  
      #
      \rm -f SCRATCH/TMPnewsize1.mrc
      #
      ${bin_2dx}/labelh.exe << eot
${imagename}.mrc
29
SCRATCH/TMPnewsize1.mrc
eot
      #
      ${proc_2dx}/linblock "Interpolation done."
      #
      echo "<<@progress: 62>>"
      #
      if ( ! -e ORIGINAL.mrc ) then
        \mv -f ${imagename}.mrc ORIGINAL.mrc
        echo "#IMAGE-IMPORTANT: ORIGINAL.mrc <Original image>" >> LOGS/${scriptname}.results
      endif
      #
      \mv -f SCRATCH/TMPnewsize1.mrc ${imagename}.mrc
      #
      set sizeX = ${newsize}
      #
    endif
    #
    if ( ${sizeX} != ${imagesidelength} ) then
      set oldval = ${imagesidelength}
      set imagesidelength = ${sizeX}
      ${proc_2dx}/linblock "WARNING: correcting imagesidelength from ${oldval} to ${imagesidelength}"
      echo "set imagesidelength = ${imagesidelength}"  >> LOGS/${scriptname}.results
    endif
    #


