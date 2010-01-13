#
#
#
#   ... This is not an independent script. It should only be called from other scripts.
#
#
#
#
if ( ${ALAT} == "0" || ${ALAT} == "0.0" ) then
  ${proc_2dx}/protest "ALAT is not defined."
endif
set ALATnew = `echo ${ALAT} | awk '{ if ( $1 < 0 ) { s = -$1 } else { s = $1 }} END { print s }'`
if ( ${ALAT} != ${ALATnew} ) then
  set ALAT = ${ALATnew}
  echo "set ALAT = ${ALAT}" >> LOGS/${scriptname}.results
endif
#
set zmin = `echo ${zstarrange} | awk '{s = -$1} END {print s}'`
set zminmax = `echo ${zmin},${zstarrange}`
#
echo zminmax = ${zminmax}
#
set testval = `echo ${imagenumber} | wc -c`
if ( `echo ${testval} | awk '{ if ( $1 < 11 ) { s = 1 } else { s = 0 }} END { print s }'` == 1 ) then
  set oldval = ${imagenumber}
  set imagenumber = `echo 0000000000 | cut -c${testval}-`${imagenumber}
  ${proc_2dx}/linblock "ERROR: correcting imagenumber from ${oldval} to ${imagenumber}"
  echo "set imagenumber = ${imagenumber}" >> LOGS/${scriptname}.results
endif
#
if ( `echo ${testval} | awk '{ if ( $1 > 11 ) { s = 1 } else { s = 0 }} END { print s }'` == 1 ) then
  set oldval = ${imagenumber}
  set startnum = `echo ${testval} | awk '{ s = $1 - 10 } END { print s }'`
  set endnum   = `echo ${testval} | awk '{ s = $1 - 1 } END { print s }'`
  set imagenumber = `echo ${imagenumber} | cut -c${startnum}-${endnum}`
  ${proc_2dx}/linblock "ERROR: correcting imagenumber from ${oldval} to ${imagenumber}"
  echo "set imagenumber = ${imagenumber}" >> LOGS/${scriptname}.results
endif
#
if ( ${spcgrp} != "1" ) then
  if ( ${avrgamphsRESOL} == '0' || ${avrgamphsRESOL} == '0.0' ) then
    set avrgamphsRESlocal = `echo ${RESMAX} | awk '{ s = int ( $1 * 9.5 / 10.0 ) } END { print s }'`
  else
    set avrgamphsRESlocal = ${avrgamphsRESOL}
  endif
endif
#
#???? Check this ???
set SCL = 1
echo "SCL = ${SCL}"
#
# contrast for grey plot
set scale = 1
echo "scale = ${scale}"
#
source ${proc_2dx}/2dx_checklattice_sub.com 
#
#############################################################################
${proc_2dx}/linblock "ORIGTILT - A first time to produce phase residual table"
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
#
if ( ${sgnxch} == "y" ) then
  set sgnxchval = 1
  set phaorix = `echo ${phaori_local} | cut -d\, -f1 `
  set phaoriy = `echo ${phaori_local} | cut -d\, -f2 | awk '{ s = -$1 } END { print s }'`
  set phaori_local = `echo ${phaorix},${phaoriy}`
else
  set sgnxchval = 0
endif
#
echo revxsgn = ${revxsgn}
if ( ${revxsgn} == "y" ) then
  set revxsgnval = 1
  set phaorix = `echo ${phaori_local} | cut -d\, -f1 | awk '{ s = -$1 } END { print s }'`
  set phaoriy = `echo ${phaori_local} | cut -d\, -f2 `
  set phaori_local = `echo ${phaorix},${phaoriy}`
else
  set revxsgnval = 0
endif
#
set SCL = 1
echo "SCL = ${SCL}"
#
set ampweight = 0.5
#

##################################################### <+> Bryant (Mitsuoka Lab) ########################################################

set aphdummy = APH/mergeReference.hkl


if ( ! -e ${aphdummy} ) then
  ${proc_2dx}/linblock "Warning: No merge reference found so Phase Residual will be meaningless."
  set aphdummy = ${proc_2dx}/dummy.aph
  if ( ! -e ${aphdummy} ) then
    ${proc_2dx}/protest "ERROR: dummy.aph not found."
  endif
endif

##################################################### \<+> Bryant (Mitsuoka Lab) #######################################################

#
if ( ! -e ${aphfile} ) then
  ${proc_2dx}/linblock "#"
  ${proc_2dx}/linhash "ERROR: ${aphfile} not found."
  ${proc_2dx}/linhash "ERROR: You might first need to run previous scripts."
  ${proc_2dx}/linblock "#"
else
#
###########################################################################
if ( ${spcgrp} != "1" ) then
  ###########################################################################

##################################################### <+> Bryant (Mitsuoka Lab) #######################################################
  
  #
  \rm -f SUMMARY
  echo dummy > fort.1
  \rm -f fort.?
  #

set origtiltkParameters ="${bin_2dx},${spcgrp},${realcell},${ALAT},${realang},${IAQP2},${IVERBOSE},${LOGOUTPUT},	\\
${imagenumber},${phastepnum},${ampweight},						\\
${aphdummy},										\\
${imagenumber},${imagename},${date},							\\
${aphfile},${TAXA},${TANGL},${lattice},							\\
${phaori_local},${phastep},${zwin},${sgnxchval},${SCL},					\\
${rot180val},${revhkval},								\\
${ctfrevval},${rot90val},${revhndval},${revxsgnval},${LPROTFOUFIL},			\\
${CS},${KV},${beamtilt},${RESMIN},${RESMAX}"

${proc_2dx}/2dx_functions.com origtiltk ${origtiltkParameters}
  
  #
  \rm -f SUMMARY
  \cp -f fort.3 APH/${prefix}${imagename}.cor.origtiltd.aph
  #
##################################################### \<+> Bryant (Mitsuoka Lab) ######################################################  
  if ( ${scriptname} == "2dx_generateMAP" ) then
    echo "<<@progress: 10>>"
  else
    if ( ${SYM_sub} == "p1" ) then
      echo "<<@progress: 5>>"
    else
      echo "<<@progress: 55>>"
    endif
  endif
  #
  #############################################################################
  ${proc_2dx}/linblock "2dx_avrgamphs - to calculate statistics and produce phase residual table"
  #############################################################################
  #
  echo "current zminmax = ${zminmax}"
  set zmaxlocal = `echo ${ALAT} | awk '{ s = ( 1 / ( 2 * $1 ) ) } END { print s }'`
  set zminmaxlocal = `echo -${zmaxlocal},${zmaxlocal}`
  # avramphs only works for 2D projection data.
  ${proc_2dx}/linblock "Symmetry statistics here are only good in 2D."
  ${proc_2dx}/linblock "Using therefore zminmax=${zminmaxlocal}."
  #
  \cp -f APH/${prefix}${imagename}.cor.origtiltd.aph fort.1
  \rm -f fort.2
  \rm -f fort.3
  \rm -f fort.4
  \rm -f TMP444888.tmp
  \rm -f TMP444789.tmp
  \rm -f LOGS/${prefix}avramphs.table.txt
  #
  if ( ${scriptname} == "2dx_generateMAP" ) then
    echo "<<@progress: 15>>"
  else
    if ( ${SYM_sub} == "p1" ) then
      echo "<<@progress: 8>>"
    else
      echo "<<@progress: 58>>"
    endif
  endif
  #
  ${bin_2dx}/2dx_avrgamphs.exe << eot
T
${imagenumber},${zminmaxlocal}
8
${avrgamphsNUMBER}
${avrgamphsRESlocal}
${realcell} ${realang}
eot
  #
  \rm -f fort.1
  \mv -f fort.2 ${prefix}avrg-1.hkl
  \mv -f fort.3 ${prefix}avrg-1.hk
  \mv -f fort.4 APH/${prefix}ctfmerge.nolimit-1.aph
  \mv -f TMP444789.tmp LOGS/${prefix}avramphs.table.txt
  \rm -f TMP444888.tmp
  #
  cat LOGS/${prefix}avramphs.table.txt
  cat LOGS/${prefix}avramphs.table.txt | sed 's/:://g' > LOGS/${prefix}phase-residuals.txt
  echo "# IMAGE: LOGS/${prefix}phase-residuals.txt <TXT: ${prename}Phase Residuals in Resolution Ranges>" >> LOGS/${scriptname}.results
  #
  ###########################################################################
endif
###########################################################################
#
\rm -f LOGS/${prefix}avramphs.table.txt
#
#############################################################################
${proc_2dx}/linblock "ORIGTILT - in p1 symmetry to transform the APH file into APH/${prefix}${imagename}.cor.origtiltd.aph"
#############################################################################
#
\rm -f SUMMARY
echo dummy > fort.1
\rm -f fort.?
#
\rm -f 2dx_origtiltk-console.log
#

#
# Parameter glob which must be delimited by "|" for use in the function. A bit cumbersome perhaps, but hopefully more flexible.
#
set origtiltkParameters = "${bin_2dx}|${spcgrp}|${realcell}|${ALAT}|${realang}|${IAQP2}|${IVERBOSE}|${LOGOUTPUT}|\
${imagenumber}|${phastepnum}|${ampweight}|\
${aphdummy}|\
${imagename}|${date}|							\
${aphfile}|${TAXA}|${TANGL}|${lattice}|							\
${phaori_local}|${phastep}|${zwin}|${sgnxchval}|${SCL}|					\
${ctfrevval}|${rot90val}|${revhndval}|${revxsgnval}|${LPROTFOUFIL}|			\
${CS}|${KV}|${beamtilt}|${RESMIN}|${RESMAX}"


${proc_2dx}/2dx_functions.com origtiltk ${origtiltkParameters}"|0|0"
${proc_2dx}/2dx_functions.com origtiltk ${origtiltkParameters}"|0|1"
${proc_2dx}/2dx_functions.com origtiltk ${origtiltkParameters}"|1|0"
${proc_2dx}/2dx_functions.com origtiltk ${origtiltkParameters}"|1|1"


#
cp 2dx_origtiltk-console.log LOGS/2dx_origtiltk-console.txt
echo "# IMAGE: LOGS/2dx_origtiltk-console.txt <TXT: Origtiltk Console Log>" >> LOGS/${scriptname}.results
\rm -f SUMMARY
\cp -f fort.3 APH/${prefix}${imagename}.cor.origtiltd.aph
#
echo "# IMAGE: APH/${prefix}${imagename}.cor.origtiltd.aph <APH: ${prename}Amp&Phs after ORIGTILT>" >> LOGS/${scriptname}.results
#
if ( ${scriptname} == "2dx_generateMAP" ) then
  echo "<<@progress: 20>>"
else
  if ( ${SYM_sub} == "p1" ) then
    echo "<<@progress: 10>>"
  else
    echo "<<@progress: 60>>"
  endif
endif
#
#############################################################################
${proc_2dx}/linblock "avrgamphs - to transform merge.aph into avrg.hkl"
#############################################################################
#
\cp -f APH/${prefix}${imagename}.cor.origtiltd.aph fort.1
\rm -f fort.2
\rm -f fort.3
\rm -f fort.4
\rm -f TMP444888.tmp
#
if ( ${scriptname} == "2dx_generateMAP" ) then
  echo "<<@progress: 30>>"
else
  if ( ${SYM_sub} == "p1" ) then
    echo "<<@progress: 15>>"
  else
    echo "<<@progress: 65>>"
  endif
endif
#
${bin_2dx}/avrgamphs.exe << eot
T
${imagenumber},${zminmax}
8
${realcell} ${realang}
eot
#
\rm -f fort.1
\mv -f fort.2 ${prefix}avrg.hkl
\mv -f fort.3 ${prefix}avrg.hk
\mv -f fort.4 APH/${prefix}ctfmerge.nolimit.aph
\rm -f TMP444888.tmp
#
echo "# IMAGE: ${prefix}avrg.hkl <TXT: ${prename}APH file after AVRGAMPHS>" >> LOGS/${scriptname}.results
#
if ( ${scriptname} == "2dx_generateMAP" ) then
  echo "<<@progress: 38>>"
else
  if ( ${SYM_sub} == "p1" ) then
    echo "<<@progress: 20>>"
  else
    echo "<<@progress: 72>>"
  endif
endif
#
#############################################################################
${proc_2dx}/linblock "2dx_centric2 - to correct phases to 0 or 180"
#############################################################################  
#
\rm -f ${prefix}centric.hkl
\rm -f ${prefix}centric.hk
#
${bin_2dx}/2dx_centric2.exe << eot
${prefix}avrg.hkl
${prefix}centric.hkl
${prefix}centric.hk
${realcell},${realang}
${RESMIN},${RESMAX}
${SYM_sub}
eot
#
if ( ${scriptname} == "2dx_generateMAP" ) then
  echo "<<@progress: 41>>"
else
  if ( ${SYM_sub} == "p1" ) then
    echo "<<@progress: 22>>"
  else
    echo "<<@progress: 74>>"
  endif
endif
#
if ( ! -e ${prefix}centric.hkl ) then
  ${proc_2dx}/protest "ERROR occured."
endif
#
echo "# IMAGE: ${prefix}centric.hkl <TXT: ${prename}APH file after CENTRIC>" >> LOGS/${scriptname}.results
#
#############################################################################
${proc_2dx}/linblock "f2mtz - to translate avrg.hkl into ${imagename}.mtz"
#############################################################################
#
set infile = ${prefix}centric.hkl
#
echo "Calling now:"
echo "${bin_ccp4}/f2mtz hklin ${infile} hklout ${prefix}${imagename}.mtz"
echo "TITLE  P1 map ${imagename}, ${date}"
echo "CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}"
echo "SYMMETRY ${CCP4_SYM}"
echo "LABOUT H K L F PHI FOM"
echo "CTYPOUT H H H F P W"
echo "FILE ${infile}"
echo "SKIP 1"
echo "END"
#
if ( ${rotate_to_Z} == "yes" ) then
  # Here comes a messy thing: CCP4 wants to symmetrize in the Y direction,
  # but we want to symmetrize around the Z direction.
  # The data are therefore permutated from XYZ to ZXY, then symmetrized, and finally again
  # permutated to be projected in Y direction. 
  # This is only needed for P2, P2221b, and some others.
  #
  set cellx = `echo ${realcell} | sed 's/,/ /g' | awk '{ s = $1 } END { print s }'`
  set celly = `echo ${realcell} | sed 's/,/ /g' | awk '{ s = $2 } END { print s }'`
  echo "cellx, celly = ${cellx}, ${celly}"
  #
  ${proc_2dx}/linblock "2dx_permutate - to transform into K,L,H"
  #
  ${bin_2dx}/2dx_permutate.exe < ${infile} > ${prefix}${imagename}-permutated.hkl
  echo "# IMAGE: ${prefix}${imagename}-permutated.hkl <TXT: APH file after permutation>" >> LOGS/${scriptname}.results
  #
  ${bin_ccp4}/f2mtz hklin ${prefix}${imagename}-permutated.hkl hklout ${prefix}${imagename}.mtz << eof
TITLE  Map, Symmetry=${CCP4_SYM}, ${prename}${imagename}, ${date}
CELL ${celly} ${ALAT} ${cellx} 90.0 90.0 90.0
SYMMETRY ${CCP4_SYM}
LABOUT H K L F PHI FOM
CTYPOUT H H H F P W
SKIP 1
END
eof
  #
  # \rm -f ${prefix}${imagename}-permutated.hkl
  #
else
  ${bin_ccp4}/f2mtz hklin ${infile} hklout ${prefix}${imagename}.mtz << eof
TITLE  Map, Symmetry=${CCP4_SYM}, ${prename}${imagename}, ${date}
CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}
SYMMETRY ${CCP4_SYM}
LABOUT H K L F PHI FOM
CTYPOUT H H H F P W
FILE ${infile}
SKIP 1
END
eof
  #
endif
#
if ( ${scriptname} == "2dx_generateMAP" ) then
  echo "<<@progress: 45>>"
else
  if ( ${SYM_sub} == "p1" ) then
    echo "<<@progress: 25>>"
  else
    echo "<<@progress: 77>>"
  endif
endif
#
#############################################################################
${proc_2dx}/linblock "mtzdump - to look at ${prefix}${imagename}.mtz"
#############################################################################
#
${bin_ccp4}/mtzdump hklin ${prefix}${imagename}.mtz << eot
NREF 100
END
eot
#
#############################################################################
${proc_2dx}/linblock "fft - to transform ${prefix}${imagename}.mtz into SCRATCH/scratch1.map"
#############################################################################
#
# contrast for grey plot
set scale = 1
echo "scale = ${scale}"
#
\rm -f SCRATCH/scratch1.map
#
if ( ${rotate_to_Z} == "yes" ) then
  #
  set AXIS = "Z,X,Y"
  #
  ${bin_ccp4}/fft hklin ${prefix}${imagename}.mtz mapout SCRATCH/scratch1.map  << eot
LABIN F1=F PHI=PHI ##
PROJECTION
AXIS ${AXIS}
SCALE F1 ${scale} ${tempfac}
RESOLUTION ${RESMIN} ${RESMAX}
TITLE Sym=${SYM_sub}, ${imagename}, ${date}, res=${RESMAX}, T=${tempfac}
GRID 200 2 200
XYZLIM 0 199 0 0 0 199
RHOLIM 250.0
HKLMAX 50 50 50
END
eot
  #
else
  #
  set AXIS = "X,Y,Z"
  #
  ${bin_ccp4}/fft hklin ${prefix}${imagename}.mtz mapout SCRATCH/scratch1.map  << eot
LABIN F1=F PHI=PHI ##
PROJECTION
AXIS ${AXIS}
SCALE F1 ${scale} ${tempfac}
RESOLUTION ${RESMIN} ${RESMAX}
TITLE Sym=${SYM_sub}, ${imagename}, ${date}, res=${RESMAX}, T=${tempfac}
GRID 200 200 20
XYZLIM 0 199 0 199 0 0
RHOLIM 250.0
HKLMAX 50 50 50
END
eot
  #
endif
#
if ( ${scriptname} == "2dx_generateMAP" ) then
  echo "<<@progress: 50>>"
else
  if ( ${SYM_sub} == "p1" ) then
    echo "<<@progress: 27>>"
  else
    echo "<<@progress: 79>>"
  endif
endif
#
#############################################################################
${proc_2dx}/linblock "extend - to extend SCRATCH/scratch1.map into ${prefix}${imagename}-${SYM_sub}.rec.mrc"
#############################################################################
#
\rm -f SCRATCH/${prefix}${imagename}-${SYM_sub}.map
#
if ( ${rotate_to_Z} == "yes" ) then
  #
  \rm -f SCRATCH/TMP001.map
  #
  ${bin_ccp4}/extends mapin SCRATCH/scratch1.map mapout SCRATCH/TMP001.map << eof
XYZLIM 0 399 0 0 0 399
KEEP
END
eof
  #
  \rm -f SCRATCH/TMP001.mrc
  #
  ${bin_2dx}/labelh.exe << eot
SCRATCH/TMP001.map
13
SCRATCH/${prefix}${imagename}-${SYM_sub}.map
eot
  #
else
  #
  ${bin_ccp4}/extends mapin SCRATCH/scratch1.map mapout SCRATCH/${prefix}${imagename}-${SYM_sub}.map << eof
XYZLIM 0 399 0 399 0 0
KEEP
END
eof
  #
endif
#
if ( ${scriptname} == "2dx_generateMAP" ) then
  echo "<<@progress: 55>>"
else
  if ( ${SYM_sub} == "p1" ) then
    echo "<<@progress: 29>>"
  else
    echo "<<@progress: 81>>"
  endif
endif
#
#############################################################################
${proc_2dx}/linblock "LABEL - to create a clean MRC file format instead of CCP4"
#############################################################################
#
\rm -f ${prefix}${imagename}-${SYM_sub}.mrc
#
${bin_2dx}/labelh.exe << eot
SCRATCH/${prefix}${imagename}-${SYM_sub}.map 
2
${prefix}${imagename}-${SYM_sub}.mrc
1,0
0
eot
#
if ( ${SYM_sub} == 'p1' ) then
  \rm -f final_map.mrc
  \ln -s ${prefix}${imagename}-p1.mrc final_map.mrc
endif
#
echo "# IMAGE-IMPORTANT: ${prefix}${imagename}-p1.mrc <${prename}Non-symmetrized Map>"  >> LOGS/${scriptname}.results
echo "# IMAGE-IMPORTANT: ${prefix}${imagename}-${SYM_sub}.mrc <${prename}${SYM_sub}-symmetrized Map>" >> LOGS/${scriptname}.results
#
#############################################################################
${proc_2dx}/linblock "npo - to create a line plot ${imagename}-${SYM_sub}.plt"
#############################################################################
#
\rm -f ${prefix}${imagename}-${SYM_sub}.plt
#
${bin_ccp4}/npo  MAPIN  SCRATCH/${prefix}${imagename}-${SYM_sub}.map  PLOT  ${prefix}${imagename}-${SYM_sub}.plt  << eof
TITLE P1 NOSYMMETRY ${prename} ${imagename}
MAP SCALE 0.4
CONTRS -500 TO 500 BY 15
MODE BELOW -40 DASHED 1 0.15 0
SECTS 0 0
PLOT
END
eof
#
if ( ${scriptname} == "2dx_generateMAP" ) then
  echo "<<@progress: 70>>"
else
  if ( ${SYM_sub} == "p1" ) then
    echo "<<@progress: 35>>"
  else
    echo "<<@progress: 85>>"
  endif
endif
#
#############################################################################
${proc_2dx}/linblock "laserplot - to create PS/${prefix}${imagename}MAP-${SYM_sub}.ps"
#############################################################################
#
\rm -f PS/${prefix}${imagename}MAP-${SYM_sub}.ps
${bin_2dx}/laserplot.exe -outputfile=PS/${prefix}${imagename}MAP-${SYM_sub}.ps ${prefix}${imagename}-${SYM_sub}.plt
#
\rm -f ${imagename}-${SYM_sub}.plt
#
echo "# IMAGE-IMPORTANT: PS/${prefix}${imagename}MAP-p1.ps <PS: ${prename}Non-symmetrized Map>"  >> LOGS/${scriptname}.results
echo "# IMAGE-IMPORTANT: PS/${prefix}${imagename}MAP-${SYM_sub}.ps <PS: ${prename}${SYM_sub}-symmetrized Map>" >> LOGS/${scriptname}.results
#
if ( ${scriptname} == "2dx_generateMAP" ) then
  echo "<<@progress: 90>>"
else
  if ( ${SYM_sub} == "p1" ) then
    echo "<<@progress: 40>>"
  else
    echo "<<@progress: 90>>"
  endif
endif
#
if ( ${produceSpiderMap} == 'n' ) then
  if ( ${scriptname} == "2dx_generateMAP" ) then
    echo "<<@progress: 95>>"
  else
    if ( ${SYM_sub} == "p1" ) then
      echo "<<@progress: 45>>"
    else
      echo "<<@progress: 95>>"
    endif
  endif
  #
  #
  exit
  #
  #
  #
else
  #
  #
  #
  #############################################################################
  ${proc_2dx}/linblock "Launching SPIDER script - to create a scaled version of the final map"
  #############################################################################
  #
  set realu = `echo ${realcell} | sed 's/,/ /g' | awk '{ s = $1 } END { print s }'`
  set realv = `echo ${realcell} | sed 's/,/ /g' | awk '{ s = $2 } END { print s }'`
  #
  ${bin_2dx}/labelh.exe << eot
${prefix}${imagename}-${SYM_sub}.mrc
-3
SCRATCH/${prefix}${imagename}-${SYM_sub}.mrc
eot
  #
  ${bin_2dx}/mrc2tif.exe << eot
SCRATCH/${prefix}${imagename}-${SYM_sub}.mrc
SCRATCH/${prefix}${imagename}-${SYM_sub}.tif
Y
eot
  #
  # echo "# IMAGE: "SCRATCH/${imagename}-${SYM_sub}.mrc  >> LOGS/${scriptname}.results
  #
  ### THIS NEEDS TO BE A VALID PATH TO SPIDER: For example:
  #/usr/local/spider/bin/spider.exe
  #
  /usr/local/spider/bin/spider.exe << eot
spi
;
; X70 is the size of the unit cell in the MRC output file from above.
X70=200.0
;
; This attempts to get the CCP4 or MRC file into spider. There is an issue with MRC2000, which now 
; can deal with X,Y and Z dimensions. Spider may not yet be able to read that. 
; You can either go through TIFF, by skipping a certain header size:
; The second last parameter is the size of the tiff header. That might be platform dependent. 
; On OSX Intel it is 198 bytes.
; The file SCRATCH/${imagename}-${SYM}.tif is 400x400 pixels wide, which should be 16,000 pixels.
; If that file has a file size of 16,198, then those 198 byte are the header. If you skip the header,
; you then can read the rest of the TIFF file as RAW. If your TIFF file has a slightly different size, 
; for example 17,024, then adjust that number from "198" to "1024":
;
;cp from raw
;SCRATCH/${prefix}${imagename}-${SYM_sub}.tif
;8
;400,400,1
;198
;TMP001
;
; or you can go through CCP4 directly, if that works better:
;
cp from ccp4
SCRATCH/${prefix}${imagename}-${SYM_sub}.map
TMP001
n
;
WI
TMP001
TMP002
X70,X70,1
1,1,1
;
X41=${unitcellnumber} * X70
BL
TMP003
X41,X41,1
N
0.0
;
X42=1.0
X43=1.0
DO LB5 X51=1,${unitcellnumber}
  DO LB4 X52=1,${unitcellnumber}
    PA
TMP002
TMP003
(X42,X43,1)
    X42=X42+X70
  LB4
  X42=1.0
  X43=X43+X70
LB5
;
X31=-$realang
X32=180.0-X31
X36=1.0/${mapscale}
; X36 is how many pixels are in one Angstroem
X34=${unitcellnumber}*X36*${realu}/cos(-${realang}-90.0)
X34=${unitcellnumber}*X36*${realu}
X35=${unitcellnumber}*X36*${realv}
;
;
IP
TMP003
TMP004
X34,X35
;
if(X32.lt.90.0)then
SZ
TMP004
X32
TMP005
else
cp
TMP004
TMP005
endif
;
AR SCA
TMP005
TMP006
0,255
;
; the usual origin top-left / bottom-left issue:
MR
TMP006
TMP007
X
;
fs
TMP007
;
cp to tiff
TMP007
${prefix}${imagename}-${SYM_sub}-scaled.tif
;
cp to mrc
TMP006
${prefix}${imagename}-${SYM_sub}-scaled-from_Spider.mrc
-9999
;
cp to CCP4
TMP006
${prefix}${imagename}-${SYM_sub}-scaled-preCCP4.mrc
32
${mapscale}

;
; de a
; TMP001
;
; if you want to keep the SPIDER results logfile, finish with "en". 
; if you don't want to keep the SPIDER results logfile, finish with "en d". 
;
en d
eot
  #
  \rm -f LOG.spi
  \rm -f fort.1
  #
  #############################################################################
  ${proc_2dx}/linblock "LABEL - to create a clean MRC file format instead of CCP4"
  ############################################################################# 
  #
  \rm -f ${prefix}${imagename}-${SYM_sub}-scaled-CCP4.mrc
  #
  ${bin_2dx}/labelh.exe << eot
${prefix}${imagename}-${SYM_sub}-scaled-preCCP4.mrc
2
${prefix}${imagename}-${SYM_sub}-scaled-CCP4.mrc
1,0
0
eot
  #
  #############################################################################
  ${proc_2dx}/linblock "tif2mrc - to create a MRC file format from the TIFF file"
  ############################################################################# 
  #
  #
  ${bin_2dx}/tif2mrc.exe << eot
${prefix}${imagename}-${SYM_sub}-scaled.tif
${prefix}${imagename}-${SYM_sub}-scaled.mrc
eot
  #
  echo "# IMAGE-IMPORTANT: ${prefix}${imagename}-${SYM_sub}-scaled-from_Spider.mrc <${prename}${SYM_sub}-symmetrized MultiUnitCell Map from Spider>" >> LOGS/${scriptname}.results
  echo "# IMAGE-IMPORTANT: ${prefix}${imagename}-${SYM_sub}-scaled-CCP4.mrc <${prename}${SYM_sub}-symmetrized MultiUnitCell Map in CCP4-format>" >> LOGS/${scriptname}.results
  echo "# IMAGE-IMPORTANT: ${prefix}${imagename}-${SYM_sub}-scaled.mrc <${prename}${SYM_sub}-symmetrized MultiUnitCell Map>" >> LOGS/${scriptname}.results
  # echo "# IMAGE: ${prefix}${imagename}-${SYM_sub}-scaled.tif <TIFF: ${prename}${SYM_sub}-symmetrized MultiUnitCell Map>" >> LOGS/${scriptname}.results
  ${proc_2dx}/linblock "TIFF file created: ${prefix}${imagename}-${SYM_sub}-scaled.tif"
  #
endif
#
endif
#############################################################################
#
