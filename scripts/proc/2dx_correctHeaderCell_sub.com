#
#
# this is not an independent script. It should only be called from other scripts.
#
# This sub-script will correct the pixel size in the header, if needed.
#
  set loc_inimage = $1
  set loc_sample_pixel = $2
  set loc_outimage = SCRATCH/dummy.mrc
#
setenv IN ${loc_inimage}
${bin_2dx}/2dx_header.exe
set samplx = `cat 2dx_header.out | head -n 4 | tail -n 1 | cut -c43-52`
set cellx  = `cat 2dx_header.out | head -n 5 | tail -n 1 | cut -c43-55`
set ratio = `echo ${samplx} ${cellx} | awk '{ s = $1 / $2 } END { print s }'`
set isok = `echo ${ratio} | awk '{ if ( $1 < 1.001 && $1 > 0.999 ) { s = 1 } else { s = 0 }} END { print s }'`
#
if ( ${isok} == "1" ) then
  ${proc_2dx}/linblock "WARNING: Correcting pixelsize in header of ${loc_inimage} to ${loc_sample_pixel} Angstroems"
  #
  \rm -f ${loc_outimage}
  #
  ${bin_2dx}/labelh.exe << eot
${loc_inimage}
41
${loc_outimage}
${loc_sample_pixel}
eot
  #
  \mv -f ${loc_outimage} ${loc_inimage}
endif
#
