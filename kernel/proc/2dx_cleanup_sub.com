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
#
if ( "${level}" == "radical" ) then
  #
  ${proc_2dx}/linblock "Radical Cleanup"
  #
  echo dummy > avrg.p7.hkl
  echo dummy > dummy.profile
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
else
  #
  ${proc_2dx}/linblock "Slight Cleanup"
  #
endif
#
\rm -rf LOGS
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
\rm *.results
\rm -f results.spi.*
\rm -f LOG.spi
\rm -f *.plt
\rm -f APH/dummy.aph
#
