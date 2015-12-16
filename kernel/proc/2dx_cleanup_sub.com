#
#
#
# ... This is not an independent script.
#
#     This should only be called from a superior calling script.
#
#
#
#
set dir = ${PWD}
############################################################################# 
${proc_2dx}/lin "cleaning up ...  ${dir}"
#############################################################################
#
echo dummy > dummy.tmp
echo dummy > fort.1
echo dummy > ${imagename}.plt
echo dummy > ${imagename}-p5-scaled.tif
echo dummy > ${imagename}-p5-scaled-from_Spider.mrc
echo dummy > TMP.dummy
echo dummy > TMP-dummy
echo dummy > TMP9871.dat
echo dummy > 2dx_calcpositions.dummy
echo dummy > REFdummy.hkl
echo dummy > results.spi.0
echo dummy > TMP_1.txt
echo dummy > dummy.plt
echo dummy > dummy_phase_zero_MRClefthanded.mtz
echo dummy > dummy_phase_zero-p1_MRClefthanded.mtz
echo dummy > dummy_phase_zero.mtz
echo dummy > dummy_phase_zero-p1.mtz
echo dummy > dummy_phase_zero-p1.mrc
echo dummy > dummy-profile.dat
echo dummy > dummy-permutated.hkl
if ( ! -d LOGS ) then
  \mkdir LOGS
endif
echo dummy > LOGS/dummy.log
echo dummy > LOGS/dummy.results
echo dummy > ${imagename}-tmp.tabl
\rm LOGS/*.results
\rm LOGS/*.log
#
if ( "${level}" == "radical" ) then
  #
  ${proc_2dx}/lin "Radical Cleanup"
  #
  echo dummy > avrg.p7.hkl
  echo dummy > dummy.profile
  echo dummy > TMP001.spi
  \rm -rf FFTIR
  \rm -rf PS
  \rm -rf PRJ
  #
  if ( -d proc ) then
    cd proc
    \rm -f 2dx_initialize.com
    \rm -f 2dx_allspace.com
    \rm -f 2dx_applyCTF.com
    \rm -f 2dx_determineCTF.com
    \rm -f 2dx_fftrans.com
    \rm -f 2dx_findlat.com
    \rm -f 2dx_generateMAP.com
    \rm -f 2dx_generateSymMap.com
    \rm -f 2dx_getspots.com
    \rm -f 2dx_inventory.com
    \rm -f 2dx_refine_unbend1.com
    \rm -f 2dx_refine_unbend2.com
    \rm -f 2dx_refinespots.com
    \rm -f 2dx_unbend1.com
    \rm -f 2dx_unbend2.com
    \rm -f 2dx_evaluateLattice.com
    \rm -f 2dx_getDefocusTilt.com
    \rm -f 2dx_getLattice.com
    \rm -f 2dx_refinetilt.com
    \rm -f 2dx_maskCrystal.com
    \rm -f 2dx_max_likelihood.com
    \rm -f 2dx_unbendSyn.com
    \rm -f 2dx_phaseorigin.com
    \rm -f 2dx_refineParams.com
    \rm -f 2dx_refineLattice.com
    \rm -f 2dx_getspots1.com
    \rm -f 2dx_getSampleTilt.com
    \rm -f 2dx_getDefTilt.com
    \rm -f 2dx_openDir.com
    \rm -f 2dx_refinedefocus.com
    \rm -f 2dx_unbend_movie.com
    \rm -f 2dx_initialize_files.com
    \rm -f 2dx_ctfcor.com
    \rm -f 2dx_unbend_movie1a.com
    \rm -f 2dx_unbend_movie1b.com
    \rm -f 2dx_unbend_movie2.com
    cd ..
  endif
  #
  \rm -f ManualMasking-CCmap.mrc
  \rm -f ManualMasking-UnbendPlot.mrc
  \rm -f centric.hk
  \rm -f centric.hkl
  \rm -f movie_centric.hk
  \rm -f movie_centric.hkl
  \rm -f movieB_centric.hk
  \rm -f movieB_centric.hkl
  \rm -f avrg-1.hk
  \rm -f avrg-1.hkl
  \rm -f movie_avrg.hk
  \rm -f movie_avrg.hkl
  \rm -f movieB_avrg.hk
  \rm -f movieB_avrg.hkl
  \rm -f movie_centric_phase_zero.hkl
  \rm -f movieB_centric_phase_zero.hkl
  \rm -f avrg.*.hkl
  \rm -f *.profile
  \rm -f ${imagename}-p*-scaled.tif
  \rm -f ${imagename}-p*-scaled-from_Spider.mrc
  \rm -f image_ctfcor_CCmap_unbend2.mrc
  \rm -f image_ctfcor.mrc
  \rm -f unbent.mrc
  \rm -f ${imagename}_mask_mask.mrc
  \rm -f ${nonmaskimagename}_mask_mask.mrc
  #
  if ( -e ${imagename}_raw.mrc ) then
    \rm -f ${imagename}.mrc
    \rm -f ${imagename}-original.mrc
    \rm -f ${imagename}-original-big.mrc
  endif
  #
  if ( -e ${nonmaskimagename}_raw.mrc ) then
    \rm -f ${nonmaskimagename}.mrc
    \rm -f ${nonmaskimagename}-original.mrc
    \rm -f ${nonmaskimagename}-original-big.mrc
  else
    if ( -e ${nonmaskimagename}.raw.mrc ) then
      \mv -f ${nonmaskimagename}.raw.mrc ${nonmaskimagename}_raw.mrc
    endif
  endif
  #
  if ( -e ${imagename}.tif ) then
    \rm -f ${imagename}.mrc
    \rm -f ${imagename}_raw.mrc
    \rm -f ${imagename}.raw.mrc
    \rm -f ${imagename}-original.mrc
    \rm -f ${imagename}-original-big.mrc
  endif
  #
  if ( -e ${nonmaskimagename}.tif ) then
    \rm -f ${nonmaskimagename}.mrc
    \rm -f ${nonmaskimagename}_raw.mrc
    \rm -f ${nonmaskimagename}-original.mrc
    \rm -f ${nonmaskimagename}-original-big.mrc
  endif
  #
  \rm -rf ML
  \rm -f stack_ctf.binary
  \rm -f stack_whit.binary
  \rm -f whit.binary
  \rm -f reference.pgm
  \rm -f reference_1.pgm
  \rm -f reference_2.pgm
  \rm -f ML_avrg-1.hk
  \rm -f ML_avrg-1.hkl
  \rm -f ML_avrg.hk
  \rm -f ML_avrg.hkl
  \rm -f ML_centric.hk
  \rm -f ML_centric.hkl
  \rm -f ML_interpolated.mrc
  \rm -f TMP???.spi
  \rm -f CCPLOT.pdf
  \rm -f weight.mrc
  \rm -f frame_quadserch.pdf
  \rm -f frame_unbending.pdf
  \rm -f 2dx_origtiltk-reflections.log
  \rm -f frame_quadserch.ps
  \rm -f frame_unbending.ps
  \rm -f 2dx_peaksearch-amp_before_LHpass.mrc
  \rm -f u2_map.mrc
  \rm -f u2_map.ps
  \rm -f tmp_mask.mrc
  \rm -f framesB_quadserch.pdf
  \rm -f framesB_quadserch.ps
  \rm -f framesB_unbending.pdf
  \rm -f framesB_unbending.ps
  \rm -f image_ctfcor.spt
  #
else
  #
  ${proc_2dx}/linblock "Slight Cleanup"
  #
endif
#
#
if ( -d frames ) then
  \rm -rf frames
endif
#
if ( -d framesB ) then
  \rm -rf framesB
endif
#
if ( -l SCRATCH ) then
  set target = ` ls -l SCRATCH | awk '{ print $11}'`
  echo "Removing ${target}"
  if ( -e ${target} ) then
    \rm -rf ${target}
  endif
endif
#
\rm -rf SCRATCH
#
\rm -f SUMMARY
\rm -f 2dx_calcposition-positions.dat
\rm -f 2dx_calcposition-runfile.com
\rm -f CTFPLOT.PS
\rm -f HKLOUT
\rm -f mask.dat
\rm -f masked_image.dat
\rm -f peaks_image_final.dat
\rm -f peaks_image.dat
\rm -f peaks_xy_final.dat
\rm -f peaks_xy.dat
\rm -f TMP-quadserch-1.mrc
\rm -f phase.dat
\rm -f average.dat
\rm -f amp.dat
\rm -f mtzdump.txt
\rm -f latticeRefinementList.dat
\rm -f amp_LHpass.dat
\rm -f 2dx_findlat.out
\rm -f 2dx_lattilt.1.out
\rm -f 2dx_tiltgeom.out
\rm -f 2dx_tiltgeom2.out
\rm -f HISTO.PS
\rm -f CCPLOT.PS
\rm -f avrg.hk
\rm -f avrg.hkl
\rm -f *-profile.dat
\rm -rf THUMB
\rm -rf CUT
\rm -rf 2dx_origtiltk-console.log
#
\rm -f *.tmp
echo dummy > dummy.TMP
\rm -f tag_data
\rm -f fort.*
\rm -f ${imagename}*.plt
\rm -f TMP.*
\rm -f TMP-*
\rm -r TMP*.dat
\rm -f 2dx_calcpositions.*
\rm -f REF*.hkl
\rm -f *.TMP
\rm -f results.spi.*
\rm -f LOG.spi
\rm -f *.plt
\rm -f APH/dummy.aph
\rm -f *-permutated.hkl
\rm -f mrcmergec.origtmp.com
\rm -f mrcmergec.runjob.com
\rm -f merge.phr
\rm -f ${imagename}.spt.bak
\rm -f ${imagename}-*.tabl
\rm -f avrgwork.mtz
\rm -f avrg.hnegkl
\rm -f centric_phase_zero.hkl
\rm -f *_phase_zero_MRClefthanded.mtz
\rm -f *_phase_zero-p1_MRClefthanded.mtz
\rm -f *_phase_zero.mtz
\rm -f *_phase_zero-p1.mtz
\rm -f *_phase_zero-p1.mrc
\rm -f TMP_*
\rm -f fort.3
\rm -f dummy.results
#
\rm -f movieB_syn_avrg.hkl
\rm -f movieB_syn_centric.hk
\rm -f movieB_syn_centric.hkl
\rm -f movieB_syn_centric_phase_zero.hkl
\rm -f movie_syn_avrg.hk
\rm -f movie_syn_avrg.hkl
\rm -f movie_syn_centric.hk
\rm -f movie_syn_centric.hkl
\rm -f 2dx_movie_refine_selected.dat
\rm -f MovieB_peaks_I.dat
\rm -f MovieB_peaks_II.dat
\rm -f MovieB_peaks_III.dat
\rm -f MovieB_peaks_IV.dat
\rm -f SPIDERCOORD.spi
#
if ( -e ${imagename}.mrc ) then
  if ( ${imagename} == m${nonmaskimagename} ) then
    \rm -f ${imagename}.mrc
    echo "set imagename = ${nonmaskimagename}" >> LOGS/${scriptname}.results
  endif
endif
#
if ( -e ${nonmaskimagename}-masking-final.mrc ) then
  \mv -f ${nonmaskimagename}-masking-final.mrc ${nonmaskimagename}_mask.mrc 
  echo "(Renaming ${nonmaskimagename}-masking-final.mrc to ${nonmaskimagename}_mask.mrc)"
endif
#
if ( -d MovieA ) then
  \rm -f MovieA/direct_sum_ctf_fft.mrc
  \rm -f MovieA/direct_sum_filt_ctf_upscale.mrc
  \rm -f MovieA/direct_sum_syn_ctf_fft.mrc
endif
#
if ( -d MovieB ) then
  \rm -f MovieB/direct_sum_ctf_fft.mrc
  \rm -f MovieB/direct_sum_filt_ctf_upscale.mrc
  \rm -f MovieB/direct_sum_syn_ctf_fft.mrc
  \rm -f MovieB/MovieB_peaks_I.dat
  \rm -f MovieB/MovieB_peaks_II.dat
  \rm -f MovieB/MovieB_peaks_III.dat
  \rm -f MovieB/MovieB_peaks_IV.dat
endif
#
