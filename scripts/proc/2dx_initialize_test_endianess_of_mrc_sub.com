#
#
# this is not an independent script. It should only be called from other scripts.
#
# This sub-script will test if the current ${imagename}.mrc has the correct endianess, and if neccessary correct that.
#
#
#
#
    ${bin_2dx}/2dx_endianTest.exe ${nonmaskimagename}.mrc | head -n 2
    #
    set correctend = `${bin_2dx}/2dx_endianTest.exe ${nonmaskimagename}.mrc | tail -n 1`
    #
    if ( ${correctend} == 'n' ) then
      #############################################################################
      ${proc_2dx}/linblock "WARNING: wrong endedness. Tying 2dx_byteSwap.exe to correct endedness."
      ${proc_2dx}/linblock "WARNING: wrong endedness. Tying 2dx_byteSwap.exe to correct endedness." >> History.dat
      #############################################################################
      \cp ${nonmaskimagename}.mrc SCRATCH/TMPconverted.mrc
      ${bin_2dx}/2dx_byteSwap.exe SCRATCH/TMPconverted.mrc
      set correctend = `${bin_2dx}/2dx_endianTest.exe SCRATCH/TMPconverted.mrc | tail -n 1`
      if ( ${correctend} == 'y' ) then
        \mv SCRATCH/TMPconverted.mrc ${nonmaskimagename}.mrc
      else
        \rm SCRATCH/TMPconverted.mrc
      endif
    else
      ${proc_2dx}/linblock "Endedness is correct."
    endif
    #
    if ( ${correctend} == 'n' ) then
      #############################################################################
      ${proc_2dx}/linblock "WARNING: Still wrong endedness. Tying image_convert.exe to correct endedness."
      ${proc_2dx}/linblock "WARNING: Still wrong endedness. Tying image_convert.exe to correct endedness." >> History.dat
      #############################################################################
      #
      ${bin_2dx}/image_convert.exe << eot
${nonmaskimagename}.mrc
SCRATCH/TMPconverted.mrc
eot
      #
      if ( ! -e SCRATCH/TMPconverted.mrc ) then
        #############################################################################
        ${proc_2dx}/linblock "image_convert.exe FAILED."
        ${proc_2dx}/linblock "Attempting to convert with byte_swap_map.exe."
        ${proc_2dx}/linblock "Attempting to convert with byte_swap_map.exe." >> History.dat
        #############################################################################
        #
        ${bin_2dx}/byte_swap_map.exe << eot
${nonmaskimagename}.mrc
eot
        #
        setenv IN="${nonmaskimagename}.mrc"
        \rm -f SCRATCH/TMP001.tmp
        ${bin_2dx}/header.exe | grep old\ style\ 20th > SCRATCH/TMP001.tmp 
        \ls -l SCRATCH/TMP001.tmp 
        if ( -s SCRATCH/TMP001.tmp ) then
          #############################################################################
          echo ":: byte_swap_map.exe successfully corrected the endianness, "
          echo ":: but ${nonmaskimagename}.mrc is old style 20th century map."
          echo ":: Trying again image_convert.exe to update."
          #############################################################################
          #
          ${bin_2dx}/image_convert.exe << eot
${nonmaskimagename}.mrc
SCRATCH/TMPconverted.mrc
eot
          #
          if ( -e SCRATCH/TMPconverted.mrc ) then
            \mv -f SCRATCH/TMPconverted.mrc ${nonmaskimagename}.mrc
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
        \mv -f ${nonmaskimagename}.mrc ${nonmaskimagename}-wrong-endedness.mrc
        \mv -f SCRATCH/TMPconverted.mrc ${nonmaskimagename}.mrc
        ${proc_2dx}/linblock "WARNING: Endedness of the input file was corrected."
        ${proc_2dx}/linblock "WARNING: Endedness of the input file was corrected." >> History.dat
        echo "# WARNING: Warning: Endedness of the input file was corrected."  >> LOGS/${scriptname}.results
      endif
    endif


