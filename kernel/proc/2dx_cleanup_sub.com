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
echo dummy > dummy.results
echo dummy > results.spi.0
echo dummy > dummy.plt
echo dummy > dummy-profile.dat
echo dummy > dummy-permutated.hkl
if ( ! -d LOGS ) then
  \mkdir LOGS
endif
echo dummy > LOGS/dummy.log
echo dummy > ${imagename}-tmp.tabl
#
if ( "${level}" == "radical" ) then
  #
  ${proc_2dx}/linblock "Radical Cleanup"
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
    cd ..
  endif
  #
  \rm -f ManualMasking-CCmap.mrc
  \rm -f ManualMasking-UnbendPlot.mrc
  \rm -f selectionList.dat
  \rm -f centric.hk
  \rm -f centric.hkl
  \rm -f avrg-1.hk
  \rm -f avrg-1.hkl
  \rm -f avrg.*.hkl
  \rm -f *.profile
  \rm -f ${imagename}-p*-scaled.tif
  \rm -f ${imagename}-p*-scaled-from_Spider.mrc
  #
  if ( -e ${imagename}.tif ) then
    \rm -f ${imagename}.mrc
    \rm -f ${imagename}-original.mrc
    \rm -f ${imagename}-original-big.mrc
  endif
  #
  if ( -e ${nonmaskimagename}.tif ) then
    \rm -f ${nonmaskimagename}.mrc
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
  #
else
  #
  ${proc_2dx}/linblock "Slight Cleanup"
  #
endif
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
\rm -rf SCRATCH
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
#
\rm *.results
\rm -rf LOGS/*.log



