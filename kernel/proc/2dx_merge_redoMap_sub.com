#
#############################################################################
#                                                                           #
# This is not an executable script.                                         #
#                                                                           #
# This should be sourced from the calling script.                           #
#                                                                           #
# (C) 2dx.org, GNU Plublic License.                                         #
#                                                                           #
# Created..........: 01/03/2007                                             #
# Last Modification: 01/03/2007                                             #
# Author...........: 2dx.org                                                #
#                                                                           #
#############################################################################
#
#
source ${proc_2dx}/2dx_makedirs 
#
set symmetry = 'p1'
set spcgrp = 1
set CCP4_SYM = 1
set ABANG = `echo $realang | awk '{s=180-$1} END {print s}'`
echo "ABANG = ${ABANG}"
set IAQP2 = 0
set IVERBOSE = 1
set LOGOUTPUT = F
set LPROTFOUFIL = F
set phastepnum = 1
set phastep = 0.1
#
if ( ${ALAT} == "0" || ${ALAT} == "0.0" ) then
  ${proc_2dx}/protest "ALAT is not defined."
endif
set ALATnew = `echo ${ALAT} | awk '{ if ( $1 < 0 ) { s = -$1 } else { s = $1 }} END { print s }'`
if ( ${ALAT} != ${ALATnew} ) then
  set ALAT = ${ALATnew}
endif
if ( ! ($?gridsize) ) then
  set local_gridsize = "200 200 20"
  set xyzlim = "0 199 0 199 0 0"
  set extends = "y"
else
  set local_gridsize = "${gridsize}"
  set xyzlim = "ASU"
  set extends = "n"
endif
#
#############################################################################
${proc_2dx}/lin "ORIGTILT - to transform the APH file into merge.amp"
#############################################################################
#
\rm -f SCRATCH/2dx_origtilt-LOG1.dat
#
if ( ${rot180} == 'y' ) then
  set rot180val = '1'
else
  set rot180val = '0'
endif
#
if ( ${rot90} == 'y' ) then
  set rot90val = '1'
else
  set rot90val = '0'
endif
#
if ( ${revhk} == 'y' ) then
  set revhkval = '1'
else
  set revhkval = '0'
endif
#
if ( ${revhnd} == 'y' ) then
  set revhndval = '1'
else
  set revhndval = '0'
endif
#
if ( ${ctfrev} == 'y' ) then
  set ctfrevval = '1'
else
  set ctfrevval = '0'
endif
#
echo sgnxch = ${sgnxch}
if ( ${sgnxch} == "y" ) then
  set sgnxchval = 1
  set phaorix = `echo ${phaori} | cut -d\, -f1 `
  set phaoriy = `echo ${phaori} | cut -d\, -f2 | awk '{ s = -$1 } END { print s }'`
  set phaori = `echo ${phaorix},${phaoriy}`
else
  set sgnxchval = 0
endif
#
echo revxsgn = ${revxsgn}
if ( ${revxsgn} == "y" ) then
  set revxsgnval = 1
  set phaorix = `echo ${phaori} | cut -d\, -f1 | awk '{ s = -$1 } END { print s }'`
  set phaoriy = `echo ${phaori} | cut -d\, -f2 `
  set phaori = `echo ${phaorix},${phaoriy}`
else
  set revxsgnval = 0
endif
#
set SCL = 1
echo "SCL = ${SCL}"
#
#
set aphdummy = ${proc_2dx}/dummy.aph
if ( ! -e ${aphdummy} ) then
  ${proc_2dx}/protest "ERROR: dummy.aph not found."
endif
#
\rm -f SUMMARY
#
if ( ${LUSEML} == "T" ) then
  set aphfile = APH/ML_result.aph
else
  set aphfile = APH/image_ctfcor_fou_unbent_ctf.aph
endif
#
${proc_2dx}/lin "Using APH files ${aphfile}"
#
if ( ! -e ${aphfile} ) then
  ${proc_2dx}/linblock "ERROR: ${aphfile} not found."
  ${proc_2dx}/linblock "ERROR: You might first need to run previous scripts."
else
  #
  \rm -f 2dx_origtiltk-console.log
  #
  ${bin_2dx}/2dx_origtiltk.exe << eot
SCRATCH/2dx_origtilt-LOG1.dat
SCRATCH/TMP.tmp.reflections
SCRATCH/TMP.tmp.console
${spcgrp},0,F,F,0,${realcell},${ALAT},${realang},0,15,${IAQP2},${IVERBOSE},${LOGOUTPUT} !ISPG,NPRG,NTL,NBM,ILST,A,B,W,ANG,IPL,MNRF,IAQP2,IVERBOSE,LOGOUTPUT
10,0.7,10,0.5                                                              ! itaxastep,rtaxasize,itanglstep,rtanglsize
${imagenumber},0,30,${MergeIQMAX},${phastepnum},F,F,${RFACAMP}          !IRUN,LHMN,LHMX,IQMX,IBXPHS,NREFOUT,NSHFTIN,RFACAMP
100,DUMMY
${aphdummy}
${imagenumber},${imagename},${date}
${aphfile}
F
${TAXA},${TANGL},0                                                  ! TAXA,TANGL,IORIGT
${lattice}							! reciprocal lattice
${phaori},${phastep},${zwin},${sgnxchval},${SCL},${rot180val},${revhkval},${ctfrevval},${rot90val},${revhndval},${revxsgnval},${LPROTFOUFIL},${LUSEML} ! OH,OK,STEP,WIN,SGNXCH,SCL,ROT180,REVHK,CTFREV,ROT90,REVHND,REVXSGN,LPROTFOUFIL,LUSEML
${CS},${KV},${beamtilt}                                                ! cs,kv,tx,ty
${RESMIN},${RESMAX}                                                 ! resolution limits
-1
eot
  #
  \rm -f SUMMARY
  \rm -f SCRATCH/TMP.tmp.reflections
  \rm -f SCRATCH/TMP.tmp.console
  \cp -f fort.3 APH/${imagename}.cor.origtiltd.aph
  #
#############################################################################
${proc_2dx}/lin "AVRAMPHS - to transform merge.aph into avrg.hkl"
#############################################################################
#
set zminmax = "-1.0,1.0"
# avramphs only works for 2D projection data.  
# But here, we exclude the limitation since we only want the projection map.
${proc_2dx}/lin "WARNING: Using zminmax=${zminmax}, but statistics only good for 2D."
#
\cp -f APH/${imagename}.cor.origtiltd.aph fort.1
\rm -f fort.2
\rm -f fort.3
\rm -f fort.4
\rm -f TMP444888.tmp
#
${bin_2dx}/avrgamphs.exe << eot
T
${imagenumber},${zminmax}
8
${realcell} ${realang}
eot
#
\rm -f fort.1
\mv -f fort.2 avrg.hkl
\mv -f fort.3 avrg.hk
\mv -f fort.4 APH/ctfmerge.aph
#\mv -f TMP444888.tmp SCRATCH/avrg.phares
\rm -f TMP444888.tmp
#
#############################################################################
#############################################################################
#############################################################################
############### Now work on reference dataset ###############################
#############################################################################
#############################################################################
#############################################################################
#
set refnamecore = "REF${imagenumber}"
set refhklfile = "APH/${refnamecore}.hkl"
set refmtzfile = "APH/${refnamecore}_MRClefthanded.mtz"
set refmap = "${nonmaskimagename}_reference.mrc"
#
\rm -f ${refmtzfile}
\rm -f ${refmap}
#
if ( -e ${refhklfile} ) then
  set filehere = `wc -l ${refhklfile} | awk '{ if ( $1 > 1 ) { s = 1 } else { s = 0 }} END { print s }'`
else
  set filehere = 0
endif
if (( ${filehere} == '1' ) && ( ${make_reference} == "y" )) then
  #
  ${proc_2dx}/lin "Creating reference projection map ${refhklfile}"
  #
  set linenum = `wc -l ${refhklfile} | awk '{ s = $1 - 1 } END { print s }'`
  head -n 1 ${refhklfile} > TMP.tmp
  tail -n ${linenum} ${refhklfile} | sort >> TMP.tmp
  #
  #############################################################################
  ${proc_2dx}/lin "2dx_hklclean - to eliminated duplicates from APH file, for volume"
  #############################################################################      
  #
  \rm -f ${refhklfile}
  \rm -f APH/syn_nosort2D-plot.hkl
  #
  ${bin_2dx}/2dx_hklclean.exe << eot
TMP.tmp
${refhklfile}
1     ! header line
0     ! no sigma column
eot
  #
  \rm -f TMP.tmp
  #
  #############################################################################
  ${proc_2dx}/lin "f2mtz - to translate ${refhklfile} into ${refmtzfile}"
  ############################################################################# 
  #
  echo "Calling now:"
  echo "${bin_ccp4}/f2mtz hklin ${refhklfile} hklout ${refmtzfile}"
  echo "TITLE  Reference map for ${imagename}, ${date}"
  echo "CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}"
  echo "SYMMETRY ${CCP4_SYM}"
  echo "LABOUT H K L F PHI FOM"
  echo "CTYPOUT H H H F P W"
  echo "SKIP 1"
  echo "FILE ${refhklfile}"
  echo "END"
  #
  ${bin_ccp4}/f2mtz hklin ${refhklfile} hklout ${refmtzfile} << eof
TITLE  Reference map for ${imagename}, ${date}
CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}
SYMMETRY ${CCP4_SYM}
LABOUT H K L F PHI FOM
CTYPOUT H H H F P W
SKIP 1
END
eof
  #
  #############################################################################
  ${proc_2dx}/lin "volume_processor.exe to create map file"
  ############################################################################# 
  #
  echo "Calling now:"
  echo "${bin_2dx}/volume_processor.exe --hklin ${refhklfile} --mrcout ${refmap} --nx 200 --ny 200 --nz 1 --gamma ${realang} --extended 2"
  ${bin_2dx}/volume_processor.exe --hklin ${refhklfile} --mrcout ${refmap} --nx 200 --ny 200 --nz 1 --gamma ${realang} --extended 2
  #
  echo "Calling now:"
  echo "${bin_2dx}/mrc_header_modifier.exe --mrcin ${refmap} --mrcout ${refmap} --cellx ${cellx} --celly ${celly}"
  ${bin_2dx}/mrc_header_modifier.exe --mrcin ${refmap} --mrcout ${refmap} --cellx ${cellx} --celly ${celly}
  #
  cd ${mergedir}/RESULTS-MRC
  \rm -f ${imagename}-${imagenumber}_reference.mrc
  \cp ${rootdir}/${refmap} ${imagename}-${imagenumber}_reference.mrc
  cd ${rootdir}
  #
else
  if ( ${make_reference} == "y" ) then
    ${proc_2dx}/lin "WARNING: ERROR in reading reference APH file ${refhklfile}. No reference map created."
  endif
endif
#
#############################################################################
#############################################################################
#############################################################################
############### Now work on experimental dataset ############################
#############################################################################
#############################################################################
#############################################################################
#
#############################################################################
${proc_2dx}/lin "f2mtz - to translate avrg.hkl into ${imagename}_MRClefthanded.mtz"
#############################################################################
#
set infile = avrg.hkl
echo "Calling now:"
echo "${bin_ccp4}/f2mtz hklin ${infile} hklout ${imagename}_MRClefthanded.mtz"
echo "TITLE  P1 map ${imagename}, ${date}"
echo "CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}"
echo "SYMMETRY ${CCP4_SYM}"
echo "LABOUT H K L F PHI FOM"
echo "CTYPOUT H H H F P W"
echo "FILE ${infile}"
echo "END"
#
${bin_ccp4}/f2mtz hklin ${infile} hklout ${imagename}_MRClefthanded.mtz << eof
TITLE  P1 map ${imagename}, ${date}
CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}
SYMMETRY ${CCP4_SYM}
LABOUT H K L F PHI FOM
CTYPOUT H H H F P W
FILE ${infile}
Skip 1
END
eof
#
echo "Running: ${bin_2dx}/volume_processor.exe --hklin ${infile} --mrcout SCRATCH/${imagename}-${symmetry}.map --nx 200 --ny 200 --nz 1 --gamma ${realang}"
#
\rm -f SCRATCH/${imagename}-${symmetry}.map
#
${bin_2dx}/volume_processor.exe --hklin ${infile} --mrcout SCRATCH/${imagename}-${symmetry}.map --nx 200 --ny 200 --nz 1 --gamma ${realang}
#
  echo "Calling now:"
  echo "${bin_2dx}/mrc_header_modifier.exe --mrcin SCRATCH/${imagename}-${symmetry}.map --mrcout SCRATCH/${imagename}-${symmetry}.map --cellx ${cellx} --celly ${celly}"
  ${bin_2dx}/mrc_header_modifier.exe --mrcin SCRATCH/${imagename}-${symmetry}.map --mrcout SCRATCH/${imagename}-${symmetry}.map --cellx ${cellx} --celly ${celly}
  #
if( ${extends} == "y" ) then
    echo "Running: ${bin_2dx}/volume_processor.exe --mrcin SCRATCH/${imagename}-${symmetry}.map --mrcout SCRATCH/${imagename}-${symmetry}.map --extended 2"
    #
    ${bin_2dx}/volume_processor.exe --mrcin SCRATCH/${imagename}-${symmetry}.map --mrcout SCRATCH/${imagename}-${symmetry}.map --extended 2
endif

if ( ${create_PS} == "y" ) then
  #
  #############################################################################
  ${proc_2dx}/lin "npo - to create a line plot ${imagename}-${symmetry}.plt"
  #############################################################################  
  #
  \rm -f ${imagename}-${symmetry}.plt
  #
  ${bin_ccp4}/npo  MAPIN  SCRATCH/${imagename}-${symmetry}.map PLOT  ${imagename}-${symmetry}.plt  << eof
TITLE P1 NOSYMMETRY ${imagename}
MAP SCALE 0.4
${npo_line1}
${npo_line2}
SECTS 0 0
PLOT
END
eof
  #
  #############################################################################
  ${proc_2dx}/lin "laserplot - to create PS/${imagename}MAP-${symmetry}.ps"
  #############################################################################
  #
  \rm -f PS/${imagename}MAP-${symmetry}.ps
  ${bin_2dx}/laserplot.exe -outputfile=PS/${imagename}MAP-${symmetry}.ps ${imagename}-${symmetry}.plt
  #
  \rm -f ${imagename}-${symmetry}.plt
  #
  cd ${mergedir}/RESULTS-PS
  \rm -f ${imagename}-${imagenumber}.ps
  \ln -s ${rootdir}/PS/${imagename}MAP-${symmetry}.ps ${imagename}-${imagenumber}.ps
  cd ${rootdir}
  #
  if ( ${RESULTSPS} != "y" ) then
    echo "# IMAGE: RESULTS-PS" >> ${mergedir}/LOGS/${scriptname}.results
    set RESULTSPS = "y"
  endif
  #
endif
#
#############################################################################
${proc_2dx}/lin "LABEL - to create a clean MRC file format instead of CCP4"
#############################################################################
#
\rm -f ${imagename}-${symmetry}.mrc
#
${bin_2dx}/labelh.exe << eot
SCRATCH/${imagename}-${symmetry}.map 
2
${imagename}-${symmetry}.mrc
1,0
0
eot
#
\rm -f final_map.mrc
\ln -s ${imagename}-${symmetry}.mrc final_map.mrc
#
if ( ${RESULTSMRC} != "y" ) then
  echo "# IMAGE: RESULTS-MRC" >> ${mergedir}/LOGS/${scriptname}.results
  set RESULTSMRC = "y"
endif
#
#
if (( ${filehere} == '1' ) && ( ${make_reference} == "y" )) then
  #############################################################################
  ${proc_2dx}/lin "LABEL - to merge two output files into one"
  #############################################################################
  #
  \rm -f ${imagename}-${symmetry}_ref2.mrc
  #
  ${bin_2dx}/labelh.exe << eot
${imagename}-${symmetry}.mrc
21
${imagename}-${symmetry}_ref2.mrc
${refmap}
eot
  #
  \rm -f half_half.mrc
  \ln -s ${imagename}-${symmetry}_ref2.mrc half_half.mrc
  #
endif
#
#############################################################################
${proc_2dx}/lin "MRC2TIF - to create TIFF file of final map"
#############################################################################
#
#
\rm -f ${imagename}-${symmetry}.tif
#
${bin_2dx}/mrc2tif.exe << eot
${imagename}-${symmetry}.mrc
${imagename}-${symmetry}.tif
eot
#
\rm -f final_map.tif
\ln -s ${imagename}-${symmetry}.tif final_map.tif
#
cd ${mergedir}/RESULTS-MRC
\rm -f ${imagename}-${imagenumber}.mrc
\ln -s ${rootdir}/final_map.mrc ${imagename}-${imagenumber}.mrc
cd ${mergedir}/RESULTS-TIFF
set filenum = `cat filenum.tmp`
\cp ${rootdir}/final_map.tif TIFF${filenum}.tif
@ filenum += 1
echo ${filenum} > filenum.tmp
cd ..
#
#
endif
#
#############################################################################
#                                                                           #
#${proc_2dx}/linhash "Done."
#                                                                           #
#############################################################################
#
