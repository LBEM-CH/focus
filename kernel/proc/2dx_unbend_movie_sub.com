#
#  2dx_unbend_movie_sub.com
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh script.
#

        set valspotscan = '1'

        ###############################################################
        ${proc_2dx}/linblock "Processing frame average #${i}"
        ###############################################################
                
        set image_dir = `pwd`
 
        if ( ! -e frames/2dx_master.cfg ) then
          cd frames
          \ln -s ../2dx_image.cfg 2dx_master.cfg
          cd ..
        endif
        ${app_2dx_image} ${image_dir}/frames/frame_${i} "2dx_initialize"
        ${app_2dx_image} ${image_dir}/frames/frame_${i} "2dx_initialize_files"
        \rm -f frames/2dx_master.cfg


        set prog_num = `echo ${irunner} ${movie_imagenumber_touse} | awk '{ s = 30 + int( 50 * $1 / $2 ) } END { print s }'` 
        echo "<<@progress: ${prog_num}>>"
        
        echo frames/frame_${i}/FFTIR/${nonmaskimagename}_${i}.fft.mrc
        
        #########################################################################
        ${proc_2dx}/lin "FFT of raw frame average"
        #########################################################################
        setenv IN frames/frame_${i}/${nonmaskimagename}_${i}.mrc
        setenv OUT frames/frame_${i}/FFTIR/${nonmaskimagename}_${i}.fft.mrc
        ${bin_2dx}/2dx_fftrans.exe
        
        
        #########################################################################
        ${proc_2dx}/lin "TWOFILE - Cross-correlate frame and reference in Fourier-space, with ref A"
        #########################################################################
        \rm -f frames/frame_${i}/SCRATCH/corel_a_${nonmaskimagename}.fft.mrc
        setenv IN1 frames/frame_${i}/FFTIR/${nonmaskimagename}_${i}.fft.mrc
        setenv IN2 SCRATCH/refam1${imagename}.fft.msk.mrc
        setenv OUT frames/frame_${i}/SCRATCH/corel_a_${nonmaskimagename}.fft.mrc
        ${bin_2dx}/twofile.exe << eot
        2 ! ICOMB = 2
        2 0 0 ${refposix} ${refposiy} ! IORIGIN,OXA,OYA,OXB,OYB  Origin shifts to FFTs
eot

        #########################################################################
        ${proc_2dx}/lin "FFTRANS - Inverse FFT to obtain cross-correlation profile (raw frame with reference A)"
        #########################################################################
        /bin/rm -f frames/frame_${i}/SCRATCH/corel_a_${nonmaskimagename}.cor.mrc
        setenv IN frames/frame_${i}/SCRATCH/corel_a_${nonmaskimagename}.fft.mrc
        setenv OUT frames/frame_${i}/SCRATCH/corel_a_${nonmaskimagename}.cor.mrc
        ${bin_2dx}/2dx_fftrans.exe
        

        \rm -f frames/frame_${i}/SCRATCH/prof${nonmaskimagename}.dat
        \rm -f SPIDERCOORD.spi
        \rm -f CCPLOT.PS

        setenv PROFILE  SCRATCH/auto${imagename}.map.mrc
        setenv PROFDATA frames/frame_${i}/SCRATCH/prof${nonmaskimagename}.dat
        setenv ERRORS   SCRATCH/errout2${imagename}.dat

        set createmask = '0'
        
        if ( ${movie_masking_mode} != '0' && ${i} != "0" ) then
                
           if ( ${movie_masking_mode} == '1' ) then
                #
                #########################################################################
                ${proc_2dx}/lin "QUADSERCH - masking with external mask-info (IPASS=2)"
                #########################################################################
                #
                if ( ! -e ${nonmaskimagename}-masking-final.mrc ) then
                  ${proc_2dx}/protest "ERROR: ${nonmaskimagename}-masking-final.mrc not found. First run UNBEND-II with masking option."
                endif 
                \cp -f ${nonmaskimagename}-masking-final.mrc TMP-quadserch-7.mrc
                #
                ${bin_2dx}/2dx_quadserchk-2.exe << eot
2,${quadpredb}                     ! IPASS,NRANGE
frames/frame_${i}/SCRATCH/corel_a_${nonmaskimagename}.cor.mrc
${imagesidelength},${imagesidelength}     ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F                       ! Lattice vectors
-200,200,-200,200               ! NUMBER UNIT CELLS TO SEARCH
${movie_quadradax},${movie_quadraday}         ! RADIUS OF CORR SEARCH, search offset in pixels
${refposix},${refposiy}           ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N                               ! YES/NO FOR DETAILED PRINTOUT
${radlim}                       ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}          ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
${createmask}                   ! dont create manual Masking information
1                               ! Do mask the image directly
frames/frame_${i}/${nonmaskimagename}_${i}.mrc
frames/frame_${i}/m${nonmaskimagename}_${i}.mrc
-1                              ! create output images (0=no, <0 = one, >0 =size)
1                               ! use external masking template (0=no, 1=yes)
TMP-quadserch-7.mrc
eot
                #
           else
                #
                #########################################################################
                ${proc_2dx}/lin "QUADSERCH - masking frame individually (IPASS=2)"
                #########################################################################
                #
                ${bin_2dx}/2dx_quadserchk-2.exe << eot
2,${quadpredb}                     ! IPASS,NRANGE
frames/frame_${i}/SCRATCH/corel_a_${nonmaskimagename}.cor.mrc
${imagesidelength},${imagesidelength}     ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F                       ! Lattice vectors
-200,200,-200,200               ! NUMBER UNIT CELLS TO SEARCH
${movie_quadradax},${movie_quadraday}           ! RADIUS OF CORR SEARCH, search offset in pixels
${refposix} ${refposiy}           ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N                               ! YES/NO FOR DETAILED PRINTOUT
${radlim}                       ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}          ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
${createmask}                   ! dont create manual Masking information
1                               ! Do mask the image directly
frames/frame_${i}/${nonmaskimagename}_${i}.mrc
frames/frame_${i}/m${nonmaskimagename}_${i}.mrc
-1                              ! create output images (0=no, <0 = one, >0 =size)
0                               ! use external masking template (0=no, 1=yes)
eot
                #
           endif

           ######################################################
           ${proc_2dx}/lin "FFT of masked frame average"
           ######################################################
           setenv IN frames/frame_${i}/m${nonmaskimagename}_${i}.mrc
           setenv OUT frames/frame_${i}/FFTIR/m${nonmaskimagename}_${i}.fft.mrc
           ${bin_2dx}/2dx_fftrans.exe

        else
           ######################################################
           ${proc_2dx}/lin "Not masking frame"
           ######################################################
           cd frames/frame_${i}
           \rm -f m${nonmaskimagename}_${i}.mrc
           \ln -s ${nonmaskimagename}_${i}.mrc m${nonmaskimagename}_${i}.mrc
           cd FFTIR
           \rm -f m${nonmaskimagename}_${i}.fft.mrc
           \ln -s ${nonmaskimagename}_${i}.fft.mrc m${nonmaskimagename}_${i}.fft.mrc
           cd ..
           cd ../.. 
        endif

        \rm -f frames/frame_${i}/SCRATCH/corel_a_${nonmaskimagename}.cor.mrc
        \rm -f frames/frame_${i}/SCRATCH/corel_a_${nonmaskimagename}.fft.mrc
        \rm -f frames/frame_${i}/SCRATCH/${nonmaskimagename}.tmp.mrc


        ###########################################################################
        ${proc_2dx}/lin "TWOFILE - Cross-correlation with references in Fourier-space"
        ###########################################################################
        set refposix = `echo ${refori} | sed 's/,/ /g' | awk '{ s = int ( $1 ) } END { print s }'`
        set refposiy = `echo ${refori} | sed 's/,/ /g' | awk '{ s = int ( $2 ) } END { print s }'`
        \rm -f frames/frame_${i}/SCRATCH/corel_a_m${nonmaskimagename}.fft.mrc
        setenv IN1 frames/frame_${i}/FFTIR/m${nonmaskimagename}_${i}.fft.mrc
        setenv IN2 SCRATCH/refam1${imagename}.fft.msk.mrc
        setenv OUT frames/frame_${i}/SCRATCH/corel_a_m${nonmaskimagename}.fft.mrc
        ${bin_2dx}/twofile.exe << eot
        2 ! ICOMB = 2
        2 0 0 ${refposix} ${refposiy} ! IORIGIN,OXA,OYA,OXB,OYB  Origin shifts to FFTs
eot

        if ( ${movie_refboxb} != "0" ) then
       
          \rm -f frames/frame_${i}/SCRATCH/corel_b_m${nonmaskimagename}.fft.mrc
          setenv IN1 frames/frame_${i}/FFTIR/m${nonmaskimagename}_${i}.fft.mrc
          setenv IN2 SCRATCH/refbm${imagename}.fft.msk.mrc
          setenv OUT frames/frame_${i}/SCRATCH/corel_b_m${nonmaskimagename}.fft.mrc
          ${bin_2dx}/twofile.exe << eot
          2 ! ICOMB = 2
          2 0 0 ${refposix} ${refposiy} ! IORIGIN,OXA,OYA,OXB,OYB  Origin shifts to FFTs
eot
        endif 

        ###########################################################################
        ${proc_2dx}/lin "FFTRANS - IFFT to obtain cross correlation profiles"
        ###########################################################################
        /bin/rm -f frames/frame_${i}/SCRATCH/corel_a_m${nonmaskimagename}.cor.mrc
        setenv IN frames/frame_${i}/SCRATCH/corel_a_m${nonmaskimagename}.fft.mrc
        setenv OUT frames/frame_${i}/SCRATCH/corel_a_m${nonmaskimagename}.cor.mrc
        ${bin_2dx}/2dx_fftrans.exe
        
        if ( ${movie_refboxb} != "0" ) then
          /bin/rm -f frames/frame_${i}/SCRATCH/corel_b_m${nonmaskimagename}.cor.mrc
          setenv IN frames/frame_${i}/SCRATCH/corel_b_m${nonmaskimagename}.fft.mrc
          setenv OUT frames/frame_${i}/SCRATCH/corel_b_m${nonmaskimagename}.cor.mrc
          ${bin_2dx}/2dx_fftrans.exe
        endif
   


set rtempx1 = 2048
set rtempy1 = 2048
set rtempx2 = 2048
set rtempy2 = 2048
@ rtempx1 -= 200
@ rtempx2 += 199
@ rtempy1 -= 200
@ rtempy2 += 199
set boxlabel = ${rtempx1},${rtempx2},${rtempy1},${rtempy2}
echo boxlabel = ${boxlabel}
\rm -f frames/CC-CE-frame_${i}.mrc
${bin_2dx}/labelh.exe << eot
frames/frame_${i}/SCRATCH/corel_a_m${nonmaskimagename}.cor.mrc
1
frames/CC-CE-frame_${i}.mrc
${boxlabel}
eot
#
${bin_2dx}/mrc2tif.exe << eot
frames/CC-CE-frame_${i}.mrc
frames/CC-CE-frame_${i}.tif
eot
#
set rtempx1 = 500
set rtempy1 = 500
set rtempx2 = 500
set rtempy2 = 500
@ rtempx1 -= 200
@ rtempx2 += 199
@ rtempy1 -= 200
@ rtempy2 += 199
set boxlabel = ${rtempx1},${rtempx2},${rtempy1},${rtempy2}
echo boxlabel = ${boxlabel}
\rm -f frames/CC-BL-frame_${i}.mrc
${bin_2dx}/labelh.exe << eot
frames/frame_${i}/SCRATCH/corel_a_m${nonmaskimagename}.cor.mrc
1
frames/CC-BL-frame_${i}.mrc
${boxlabel}
eot
#
${bin_2dx}/mrc2tif.exe << eot
frames/CC-BL-frame_${i}.mrc
frames/CC-BL-frame_${i}.tif
eot
#
set rtempx1 = 500
set rtempy1 = 3000
set rtempx2 = 500
set rtempy2 = 3000
@ rtempx1 -= 200
@ rtempx2 += 199
@ rtempy1 -= 200
@ rtempy2 += 199
set boxlabel = ${rtempx1},${rtempx2},${rtempy1},${rtempy2}
echo boxlabel = ${boxlabel}
\rm -f frames/CC-TL-frame_${i}.mrc
${bin_2dx}/labelh.exe << eot
frames/frame_${i}/SCRATCH/corel_a_m${nonmaskimagename}.cor.mrc
1
frames/CC-TL-frame_${i}.mrc
${boxlabel}
eot
#
${bin_2dx}/mrc2tif.exe << eot
frames/CC-TL-frame_${i}.mrc
frames/CC-TL-frame_${i}.tif
eot
#
set rtempx1 = 3000
set rtempy1 = 3000
set rtempx2 = 3000
set rtempy2 = 3000
@ rtempx1 -= 200
@ rtempx2 += 199
@ rtempy1 -= 200
@ rtempy2 += 199
set boxlabel = ${rtempx1},${rtempx2},${rtempy1},${rtempy2}
echo boxlabel = ${boxlabel}
\rm -f frames/CC-TR-frame_${i}.mrc
${bin_2dx}/labelh.exe << eot
frames/frame_${i}/SCRATCH/corel_a_m${nonmaskimagename}.cor.mrc
1
frames/CC-TR-frame_${i}.mrc
${boxlabel}
eot
#
${bin_2dx}/mrc2tif.exe << eot
frames/CC-TR-frame_${i}.mrc
frames/CC-TR-frame_${i}.tif
eot
#
set rtempx1 = 3000
set rtempy1 =  500
set rtempx2 = 3000
set rtempy2 =  500
@ rtempx1 -= 200
@ rtempx2 += 199
@ rtempy1 -= 200
@ rtempy2 += 199
\rm -f frames/CC-BR-frame_${i}.mrc
set boxlabel = ${rtempx1},${rtempx2},${rtempy1},${rtempy2}
echo boxlabel = ${boxlabel}
${bin_2dx}/labelh.exe << eot
frames/frame_${i}/SCRATCH/corel_a_m${nonmaskimagename}.cor.mrc
1
frames/CC-BR-frame_${i}.mrc
${boxlabel}
eot
#
${bin_2dx}/mrc2tif.exe << eot
frames/CC-BR-frame_${i}.mrc
frames/CC-BR-frame_${i}.tif
eot
#




        ###########################################################################
        ${proc_2dx}/lin "QUADSERCH - Search unbending profile with first reference (1nd round, IPASS=3)"
        ###########################################################################
        \rm -f frames/frame_${i}/SCRATCH/profm${nonmaskimagename}.dat
        \rm -f SPIDERCOORD.spi
        \rm -f SCRATCH/errout3${imagename}.dat
        \rm -f CCPLOT.PS
        \rm -f frames/frame_${i}/m${nonmaskimagename}_${i}_aligned.mrc

        # \cp -f frames/frame_${i}/m${nonmaskimagename}_${i}.mrc frames/frame_${i}/m${nonmaskimagename}_${i}_ori.mrc

        setenv PROFILE  SCRATCH/auto${imagename}.map.mrc
        setenv PROFDATA frames/frame_${i}/SCRATCH/profm${nonmaskimagename}.dat
        setenv ERRORS   SCRATCH/errout2${imagename}.dat
        setenv ERROUT   SCRATCH/errout3${imagename}.dat

        set valspotscan = '0'
        set createmask = '0'

        if ( 1 == 2 ) then
          # IPASS = 13   ! same as IPASS=3, but align images also
          set IPASS = 13

          ${bin_2dx}/2dx_quadserchk-2.exe << eot
${IPASS},${quadpredb}                     ! IPASS,NRANGE
frames/frame_${i}/SCRATCH/corel_a_m${nonmaskimagename}.cor.mrc
frames/frame_${i}/m${nonmaskimagename}_${i}.mrc
frames/frame_${i}/m${nonmaskimagename}_${i}_aligned.mrc
${imagesidelength},${imagesidelength}     ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F                       ! Lattice vectors
-200,200,-200,200               ! NUMBER UNIT CELLS TO SEARCH
${movie_quadradax},${movie_quadraday},500,500           ! RADIUS OF CORR SEARCH, search offset in pixels
${refposix},${refposiy}           ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N                               ! YES/NO FOR DETAILED PRINTOUT
${radlim}                       ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}          ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
${createmask}                   ! create manual Masking information (0=n,1=y)
0                               ! Do mask the image directly
eot
          #
          if ( ! -e frames/frame_${i}/m${nonmaskimagename}_${i}_aligned.mrc ) then
            ${proc_2dx}/protest "ERROR in 2dx_quadserchk-2" 
          else
            \mv -f frames/frame_${i}/m${nonmaskimagename}_${i}_aligned.mrc frames/frame_${i}/m${nonmaskimagename}_${i}.mrc
          endif
          #
        else
          #
          set IPASS = 3
          ${bin_2dx}/2dx_quadserchk-2.exe << eot
${IPASS},${quadpredb}                     ! IPASS,NRANGE
frames/frame_${i}/SCRATCH/corel_a_m${nonmaskimagename}.cor.mrc
${imagesidelength},${imagesidelength}     ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F                       ! Lattice vectors
-200,200,-200,200               ! NUMBER UNIT CELLS TO SEARCH
${movie_quadradax},${movie_quadraday}          ! RADIUS OF CORR SEARCH, search offset in pixels
${refposix},${refposiy}           ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N                               ! YES/NO FOR DETAILED PRINTOUT
${radlim}                       ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}          ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
${createmask}                   ! create manual Masking information (0=n,1=y)
0                               ! Do mask the image directly
eot
          #
        endif
        #
        
        if ( ${movie_refboxb} != "0" ) then

          ###########################################################################
          ${proc_2dx}/lin "QUADSERCH - Refine unbending profile with second reference (2nd round, IPASS=3)"
          ###########################################################################

          \mv -f SCRATCH/errout3${imagename}.dat SCRATCH/errout2${imagename}.dat

          \rm -f frames/frame_${i}/SCRATCH/profm${nonmaskimagename}.dat
          \rm -f SPIDERCOORD.spi
          \rm -f CCPLOT.PS

          setenv PROFILE  SCRATCH/auto${imagename}.map.mrc
          setenv PROFDATA frames/frame_${i}/SCRATCH/profm${nonmaskimagename}.dat
          setenv ERRORS   SCRATCH/errout2${imagename}.dat
          setenv ERROUT   SCRATCH/errout3${imagename}.dat

          set createmask = '0'
          set IPASS = 3

          ${bin_2dx}/2dx_quadserchk-2.exe << eot
${IPASS},${quadpredb}                     ! IPASS,NRANGE
frames/frame_${i}/SCRATCH/corel_b_m${nonmaskimagename}.cor.mrc
${imagesidelength},${imagesidelength}     ! SIZE OF TRANSFORM (ISIZEX, ISIZEY)
${lattice},F                       ! Lattice vectors
-200,200,-200,200               ! NUMBER UNIT CELLS TO SEARCH
${movie_quadradbx},${movie_quadradby}           ! RADIUS OF CORR SEARCH, search offset in pixels
${refposix},${refposiy}           ! POSN OF START SEARCH ORIGIN  0,0 IS ORIGIN
N                               ! YES/NO FOR DETAILED PRINTOUT
${radlim}                       ! RADLIM IN PROFILE GRID UNITS
${valspotscan},${RMAG},${LCOLOR}          ! prohibit fractures in crystal (1=y,0=n),RMAG,LCOLOR
${createmask}                   ! create manual Masking information (0=n,1=y)
0                               ! Do mask the image directly
eot
        endif

        if ( -e TMP-quadserch3-autocor.mrc ) then
          \mv -f TMP-quadserch3-autocor.mrc frames/frame_${i}/SCRATCH/TMP-quadserch3-autocor.mrc
        endif 

        if ( ! -e SCRATCH/errout3${imagename}.dat ) then
          ${proc_2dx}/protest "ERROR: Problem in 2dx_quadserchk-2: SCRATCH/errout3${imagename}.dat does not exist."
        endif
 
        # The other frames are subsequently used as refinement reference:
        \mv -f SCRATCH/errout3${imagename}.dat SCRATCH/errout2${imagename}.dat


        ###########################################################################
        ${proc_2dx}/lin "Store distortion-vector-field for visual inspection"
        ###########################################################################
        if ( ! -e CCPLOT.PS ) then
          ${proc_2dx}/protest "ERROR: CCPLOT.PS missing"
        endif
        #
        \cp -f CCPLOT.PS frames/frame_${i}/PS/${nonmaskimagename}-quadserch.ps

        # The high-contrast frame-0 is only used as refinement reference  
        if ( ${i} != "0" ) then

            if ( ${irunner} == '1' ) then
                if ( ${ps2pdf} == "pstopdf" ) then
                  ${ps2pdf} CCPLOT.PS 
                  \mv -f CCPLOT.pdf frame_quadserch.pdf
                else
                   ${ps2pdf} CCPLOT.PS frame_quadserch.pdf 
                endif
                pdftk A=frame_quadserch.pdf cat A1 output out.pdf
                \mv -f out.pdf frame_quadserch.pdf
            else
                if ( ${ps2pdf} == "pstopdf" ) then
                  ${ps2pdf} CCPLOT.PS  
                else
                  ${ps2pdf} CCPLOT.PS CCPLOT.pdf 
                endif
                if ( ${iforward} == '1' ) then
                  pdftk A=frame_quadserch.pdf B=CCPLOT.pdf cat A1-end B1 output out.pdf 
                else
                  pdftk A=CCPLOT.pdf B=frame_quadserch.pdf cat A1 B1-end output out.pdf 
                endif
                \mv -f out.pdf frame_quadserch.pdf
            endif

            ###########################################################################
            ${proc_2dx}/lin "CCUNBEND - Unbend the frame average"
            ###########################################################################
            setenv CCORDATA frames/frame_${i}/SCRATCH/profm${nonmaskimagename}.dat
            setenv TABLEOUT frames/frame_${i}/SCRATCH/ccunbend-table-m${nonmaskimagename}.dat
            set ITYPE = 0
            set ROFFSET = 50.0
            set NNUM = 6
            \rm -f CCPLOT.PS

            ${bin_2dx}/2dx_ccunbendk.exe << eot
frames/frame_${i}/m${nonmaskimagename}_${i}.mrc
${ITYPE},1,${IMAXCOR},${ISTEP},F,40,T       !ITYPE,IOUT,IMAXCOR,ISTEP,LTAPER,RTAPER,LTABOUT
30,52,0.001,${movie_facthreshb},${TLTAXIS},${RMAG},${LCOLOR}     !IKX,IKY,EPS,FACTOR,TLTAXIS,RMAG,LCOLOR
${imagename}, Movie-Mode UNBEND, ${date}
frames/frame_${i}/SCRATCH/corm${nonmaskimagename}.notap.mrc
CCUNBEND, frame_${i}/m${nonmaskimagename}_${i}.mrc, ${date}
eot

            ###########################################################################
            ${proc_2dx}/lin "Extract trajectory for region drift plotting"
            ###########################################################################
            python ${proc_2dx}/movie/deleteZeros.py frames/frame_${i}/SCRATCH/profm${nonmaskimagename}.dat frames/frame_${i}/SCRATCH/profm${nonmaskimagename}_nz.dat
                  
            set x_1_4 = `echo ${imagesidelength} | awk '{ s = int( $1 / 4 ) } END { print s }'`
            set x_3_4 = `echo ${imagesidelength} | awk '{ s = int( 3 * $1 / 4 ) } END { print s }'`
          
            python ${proc_2dx}/movie/getClosestPeaks.py frames/frame_${i}/SCRATCH/profm${nonmaskimagename}_nz.dat frames/frame_${i}/SCRATCH/profm${nonmaskimagename}_closest_I.dat   ${x_1_4} ${x_1_4} ${num_dia} frames/peaks_I.dat
            python ${proc_2dx}/movie/getClosestPeaks.py frames/frame_${i}/SCRATCH/profm${nonmaskimagename}_nz.dat frames/frame_${i}/SCRATCH/profm${nonmaskimagename}_closest_II.dat  ${x_3_4} ${x_1_4} ${num_dia} frames/peaks_II.dat
            python ${proc_2dx}/movie/getClosestPeaks.py frames/frame_${i}/SCRATCH/profm${nonmaskimagename}_nz.dat frames/frame_${i}/SCRATCH/profm${nonmaskimagename}_closest_III.dat ${x_1_4} ${x_3_4} ${num_dia} frames/peaks_III.dat
            python ${proc_2dx}/movie/getClosestPeaks.py frames/frame_${i}/SCRATCH/profm${nonmaskimagename}_nz.dat frames/frame_${i}/SCRATCH/profm${nonmaskimagename}_closest_IV.dat  ${x_3_4} ${x_3_4} ${num_dia} frames/peaks_IV.dat
    
            ###########################################################################
            ${proc_2dx}/lin "Store distortion-vector-field for visual inspection"
            ###########################################################################
            if ( ! -e CCPLOT.PS ) then
              ${proc_2dx}/protest "ERROR: CCPLOT.PS missing"
            endif
            #
            \cp -f CCPLOT.PS frames/frame_${i}/PS/${nonmaskimagename}-ccunbend.ps

            if ( ${irunner} == '1' ) then
                if ( ${ps2pdf} == "pstopdf" ) then
                  ${ps2pdf} CCPLOT.PS  
                  \mv -f CCPLOT.pdf frame_unbending.pdf
                else
                   ${ps2pdf} CCPLOT.PS frame_unbending.pdf 
                endif
                pdftk A=frame_unbending.pdf cat A1 output out.pdf 
                \mv -f out.pdf frame_unbending.pdf
            else
                if ( ${ps2pdf} == "pstopdf" ) then
                  ${ps2pdf} CCPLOT.PS 
                else
                  ${ps2pdf} CCPLOT.PS CCPLOT.pdf 
                endif
                if ( ${iforward} == 1 ) then
                  pdftk A=frame_unbending.pdf B=CCPLOT.pdf cat A1-end B1 output out.pdf 
                else
                  pdftk A=CCPLOT.pdf B=frame_unbending.pdf cat A1 B1-end output out.pdf 
                endif
                \mv -f out.pdf frame_unbending.pdf
            endif
        
            ###########################################################################
            ${proc_2dx}/lin "TAPEREDGE - correct unbent frame average for taper edges"
            ###########################################################################
            \rm -f frames/frame_${i}/SCRATCH/corm${nonmaskimagename}.mrc
            setenv IN  frames/frame_${i}/SCRATCH/corm${nonmaskimagename}.notap.mrc
            setenv OUT frames/frame_${i}/SCRATCH/corm${nonmaskimagename}.mrc
            ${bin_2dx}/2dx_taperedgek.exe << eot
            30,30,100,30       ! IAVER,ISMOOTH,ITAPER
eot

            ###########################################################################
            ${proc_2dx}/lin "FFTRANS - Obtain Fourier-transform of unbent and masked frame average"
            ###########################################################################
            \rm -f frames/frame_${i}/SCRATCH/corm${nonmaskimagename}.fft.mrc
            setenv IN  frames/frame_${i}/SCRATCH/corm${nonmaskimagename}.mrc
            setenv OUT frames/frame_${i}/SCRATCH/corm${nonmaskimagename}.fft.mrc
            ${bin_2dx}/2dx_fftrans.exe

            ###########################################################################
            ${proc_2dx}/lin "MMBOX - evaluate AMPlitudes and PHAses from unbent movie"
            ###########################################################################
            ${bin_2dx}/2dx_mmboxa.exe << eot
frames/frame_${i}/SCRATCH/corm${nonmaskimagename}.fft.mrc
${imagenumber} ${nonmaskimagename}, Unbend2, ${date}
Y                               ! Use grid units?
Y                               ! Generate grid from lattice?
N                               ! Generate points from lattice?
2,2,0,50,50,19,19               ! IPIXEL,IOUT,NUMSPOT,NOH,NOK,NHOR,NVERT
frames/aph_${i}.fou.nolimit.aph
SCRATCH/TMP9873.dat
U2
${refposix},${refposiy}           ! XORIG,YORIG
200.0,1.5,1,${realcell},${ALAT},${realang} ! RINNER,ROUTER,IRAD,A,B,W,ABANG
${lattice}                         ! Lattice vectors
eot
            #
            #
            #
        endif
        #

