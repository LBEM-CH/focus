#
#
# this is not an independent script. It should only be called from other scripts.
#
# This sub-script will test if the current ${imagename}.mrc has the correct endianess, and if neccessary correct that.
#
#
#
#
    ${bin_2dx}/2dx_endianTest.exe ${imagename}.mrc | head -n 2
    #
    set correctend = `${bin_2dx}/2dx_endianTest.exe ${imagename}.mrc | tail -n 1`
    #
    if ( ${correctend} == 'n' ) then
      #############################################################################
      ${proc_2dx}/linblock "WARNING: wrong endedness. Tying 2dx_byteSwap.exe to correct endedness."
      ${proc_2dx}/linblock "WARNING: wrong endedness. Tying 2dx_byteSwap.exe to correct endedness." >> History.dat
      #############################################################################
      \cp ${imagename}.mrc SCRATCH/TMPconverted.mrc
      ${bin_2dx}/2dx_byteSwap.exe SCRATCH/TMPconverted.mrc
      set correctend = `${bin_2dx}/2dx_endianTest.exe SCRATCH/TMPconverted.mrc | tail -n 1`
      if ( ${correctend} == 'y' ) then
        \mv SCRATCH/TMPconverted.mrc ${imagename}.mrc
      else
        \rm SCRATCH/TMPconverted.mrc
      endif
    endif
    #
    if ( ${correctend} == 'n' ) then
      #############################################################################
      ${proc_2dx}/linblock "WARNING: Still wrong endedness. Tying image_convert.exe to correct endedness."
      ${proc_2dx}/linblock "WARNING: Still wrong endedness. Tying image_convert.exe to correct endedness." >> History.dat
      #############################################################################
      #
      ${bin_2dx}/image_convert.exe << eot
${imagename}.mrc
SCRATCH/TMPconverted.mrc
eot
      echo "<<@progress: 30>>"
      #
      if ( ! -e SCRATCH/TMPconverted.mrc ) then
        #############################################################################
        ${proc_2dx}/linblock "image_convert.exe FAILED."
        ${proc_2dx}/linblock "Attempting to convert with byte_swap_map.exe."
        ${proc_2dx}/linblock "Attempting to convert with byte_swap_map.exe." >> History.dat
        #############################################################################
        #
        ${bin_2dx}/byte_swap_map.exe << eot
${imagename}.mrc
eot
        #
        setenv IN="${imagename}.mrc"
        \rm -f SCRATCH/TMP001.tmp
        ${bin_2dx}/header.exe | grep old\ style\ 20th > SCRATCH/TMP001.tmp 
        \ls -l SCRATCH/TMP001.tmp 
        if ( -s SCRATCH/TMP001.tmp ) then
          #############################################################################
          echo ":: byte_swap_map.exe successfully corrected the endianness, "
          echo ":: but ${imagename}.mrc is old style 20th century map."
          echo ":: Trying again image_convert.exe to update."
          #############################################################################
          #
          ${bin_2dx}/image_convert.exe << eot
${imagename}.mrc
SCRATCH/TMPconverted.mrc
eot
          #
          if ( -e SCRATCH/TMPconverted.mrc ) then
            \mv -f SCRATCH/TMPconverted.mrc ${imagename}.mrc
            echo ":: That worked. Image should be good now."
            echo "#WARNING: File format and endedness of the input file corrected."  >> LOGS/${scriptname}.results
          else
            echo ":: image_convert.exe failed again."
            echo "#WARNING: WARNING: All attempts to correct endianness failed. Some problem with the file."  >> LOGS/${scriptname}.results
          endif
        else
          echo ":: byte_swap_map failed."
          echo "#WARNING: WARNING: All attempts to correct endianness failed. Some problem with the file."  >> LOGS/${scriptname}.results
        endif
        ${proc_2dx}/linblock "Finished Endian Test"
      else
        \mv -f ${imagename}.mrc ${imagename}-wrong-endedness.mrc
        \mv -f SCRATCH/TMPconverted.mrc ${imagename}.mrc
        ${proc_2dx}/linblock "WARNING: Endedness of the input file was corrected."
        ${proc_2dx}/linblock "WARNING: Endedness of the input file was corrected." >> History.dat
        echo "# WARNING: Warning: Endedness of the input file was corrected."  >> LOGS/${scriptname}.results
      endif
    endif


