#
#
# This is not an independent script.
#
# This should only be called from another script.
#
#
if ( ${ML_do_whiten} == 'y' ) then
  set ML_do_whiten_val = "1"
else
  set ML_do_whiten_val = "0"
endif
#
if ( ${ML_correct_CTF} == 'y' ) then
  set ML_correct_CTF_val = "1"
else
  set ML_correct_CTF_val = "0"
endif
#
if ( ${ctfrev} == "y" ) then
  set contrast_val = "-1"
else
  set contrast_val = "1"
endif
#
if ( ${ML_rotational_symmetry} == "0" ) then
  set ML_rotational_symmetry_val = 1
else if ( ${ML_rotational_symmetry} == "1" ) then
  set ML_rotational_symmetry_val = 2
else if( ${ML_rotational_symmetry} == "2" ) then
  set ML_rotational_symmetry_val = 3
else if( ${ML_rotational_symmetry} == "3" ) then
  set ML_rotational_symmetry_val = 4
else if( ${ML_rotational_symmetry} == "4" ) then
  set ML_rotational_symmetry_val = 6
endif
#
pwd
set date = `date`
echo date = ${date}
#
if ( -e ${imagename}-profile.dat.gz ) then
  #############################################################################
  ${proc_2dx}/linblock "gunzip - to uncompress the profile"
  ############################################################################# 
  #
  \cp -f ${imagename}-profile.dat.gz ${imagename}.tmp.gz
  \rm -f ${imagename}.tmp
  #
  gunzip ${imagename}.tmp.gz
  #
  \cp -f ${imagename}.tmp ${imagename}-profile.dat
  \rm -f ${imagename}.tmp
  #
  echo "# IMAGE: ${imagename}-profile.dat <TXT: UnitCell Particle Profile>" >> LOGS/${scriptname}.results
  #
else
  ${proc_2dx}/linblock "${imagename}-profile.dat.gz not existing."
  ${proc_2dx}/protest "First run UNBEND II to create the PROFILE."
endif
#
echo "# IMAGE: APH/${imagename}.cor.aph <APH: Unbending Amp&Phase File>" >> LOGS/${scriptname}.results
echo "# IMAGE: PS/${imagename}MAP-p1.ps <PS: Unbending MAP in p1>" >> LOGS/${scriptname}.results
echo "# IMAGE: ${imagename}-p1.mrc <Unbending MAP in p1>" >> LOGS/${scriptname}.results
#
if ( ! -e ${imagename}.mrc ) then
  ${proc_2dx}/protest "${imagename}.mrc not existing. No input image file ???"
endif
#
echo dummy > ML/ML_reference_1.mrc
\rm -f ML_result_noEnvelope.mrc
\rm -f ML_result_withEnvelope.mrc
\rm -f ML_result_ref_even.mrc
\rm -f ML_result_ref_odd.mrc
\rm -f ML/ML_reference_*.mrc
#
echo "<<@progress: 10>>"
#
#############################################################################
${proc_2dx}/linblock "2dx_ML - to run maximum likelihood processing"
#############################################################################   
#
${bin_2dx}/2dx_ML.exe << eot_ML1
${ML_doMLorCC}
LOGS/${scriptname}.results
eot_ML1
#
echo ${imagenumber} "           Maximum Likelihood 2D AMPS and PHS of the unit cell" > SCRATCH/ML_tmp.txt
cat APH/ML_result.aph >> SCRATCH/ML_tmp.txt
\mv -f SCRATCH/ML_tmp.txt APH/ML_result.aph
#
#############################################################################
#

