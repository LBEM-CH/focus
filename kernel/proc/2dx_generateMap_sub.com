#
#
#
#   ... This is not an independent script. It should only be called from other scripts.
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
  ${proc_2dx}/linblock "ERROR: Correcting ALAT to ${ALAT}."
endif
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
    set avrgamphsRESlocal = ${RESMAX}
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
set aphdummy = ${proc_2dx}/dummy.aph
if ( ! -e ${aphdummy} ) then
  ${proc_2dx}/protest "ERROR: dummy.aph not found."
endif
#
if ( ! -e ${aphfile} ) then
  ${proc_2dx}/linblock "#"
  ${proc_2dx}/linhash "ERROR: ${aphfile} not found."
  ${proc_2dx}/protest "ERROR: You might first need to run previous scripts."
endif
#
# For the map generation, there should not be any data cutoff:
set zwinlocal = 0.5
#
###########################################################################
if ( ${spcgrp} != "1" ) then
  ###########################################################################
  #
  \rm -f SUMMARY
  echo dummy > fort.1
  \rm -f fort.?
  #
  echo "Parameter for 2dx_origtiltk.exe are:"
  echo "================================================================================="
  echo SCRATCH/2dx_origtilt-LOG1.dat
  echo ${spcgrp},0,F,F,1,${realcell},${ALAT},${realang},0,15,${IAQP2},${IVERBOSE},${LOGOUTPUT}
  echo 10,0.7,10,0.5 
  echo ${imagenumber},0,30,5,${phastepnum},T,F,${ampweight} 
  echo 100,DUMMY
  echo ${aphdummy}
  echo ${imagenumber},${imagename},${date}
  echo ${aphfile}
  echo F
  echo ${TAXA},${TANGL},0 
  echo ${lattice}
  echo ${phaori_local},${phastep},${zwinlocal},${sgnxchval},${SCL},${rot180val},${revhkval},${ctfrevval},${rot90val},${revhndval},${revxsgnval},${LPROTFOUFIL}
  echo ${CS},${KV},${beamtilt}                                                
  echo ${RESMIN},${RESMAX}                                                
  echo -1
  echo "================================================================================="
  echo " "
  #
  \rm -f 2dx_origtiltk-console.log
  #
  ${bin_2dx}/2dx_origtiltk.exe << eot
SCRATCH/2dx_origtilt-LOG1.dat
${spcgrp},0,F,F,1,${realcell},${ALAT},${realang},0,15,${IAQP2},${IVERBOSE},${LOGOUTPUT} !ISPG,NPRG,NTL,NBM,ILST,A,B,W,ANG,IPL,MNRF,IAQP2,IVERBOSE,LOGOUTPUT,LPROTFOUFIL
10,0.7,10,0.5                                                   ! itaxastep,rtaxasize,itanglstep,rtanglsize
${imagenumber},0,30,${MergeIQMAX},${phastepnum},F,F,${ampweight}          	!IRUN,LHMN,LHMX,IQMX,IBXPHS,NREFOUT,NSHFTIN,RFACAMP
0000000100 DUMMY
${aphdummy}
${imagenumber},${imagename},${date}
${aphfile}
F
${TAXA},${TANGL},0                                              ! TAXA,TANGL,IORIGT
${lattice} 							! Reciprocal lattice
${phaori_local},${phastep},${zwinlocal},${sgnxchval},${SCL},${rot180val},${revhkval},${ctfrevval},${rot90val},${revhndval},${revxsgnval},${LPROTFOUFIL} ! OH,OK,STEP,WIN,SGNXCH,SCL,ROT,REV,CTFREV,ROT90,REVHND,REVXSGN,LPROTFOUFIL
${CS},${KV},${beamtilt}                                         ! cs,kv,tx,ty
${RESMIN},${RESMAX}                                             ! resolution limits
-1
eot
  #
  \rm -f SUMMARY
  \cp -f fort.3 APH/${prefix}${imagename}.cor.origtiltd.aph
  #
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
  # set zmaxlocal = `echo ${ALAT} | awk '{ s = ( 1 / ( 2 * $1 ) ) } END { print s }'`
  # set zminmaxlocal = `echo -${zmaxlocal},${zmaxlocal}`
  # avramphs only works for 2D projection data.
  # ${proc_2dx}/linblock "Symmetry statistics here are only good in 2D."
  #
  set zminmaxlocal = "-0.5,0.5"
  #
  ${proc_2dx}/linblock "Using zminmax=${zminmaxlocal}."
  set istilt = `echo ${TANGL} | awk ' { if ( abs ( $1 ) > 0.1 ) { s = 1 } else { s = 0 }} END { print s }'`
  if ( ${istilt} == "1" ) then
    echo ":: "
    echo "::   WARNING: Your sample has a tilt angle of ${TANGL} degrees."
    echo "::            Symmetry is only partially valid. The table below is likely not relevant, "
    echo "::            i.e., your data might be better than shown here."
    echo ":: "
  endif
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
  if ( ${SYM_sub} == "p2" ) then
    echo ":: "
    echo "::Symmetry is applied by AVRGAMPHS, which does not work with p2 symmetry,"
    echo "::unfortunately. The following table is therefore empty (sorry)."
    echo "::Somebody needs to sit down and extend 2dx_avrgamphs.for for Friedel symmetry consideration."
    echo "::Does anybody volunteer for that?"
    echo ":: "
  endif
  #
  ${bin_2dx}/2dx_avrgamphs.exe << eot
T
${imagenumber},${zminmaxlocal}
${MergeIQMAX}
${avrgamphsNUMBER}
${avrgamphsRESlocal}
${realcell} ${realang}
5
eot
  #
  \rm -f fort.1
  \mv -f fort.2 ${prefix}avrg-1.hkl
  \mv -f fort.3 ${prefix}avrg-1.hk
  \mv -f fort.4 APH/${prefix}ctfmerge.nolimit-1.aph
  \mv -f TMP444789.tmp LOGS/${prefix}avramphs.table.txt
  \rm -f TMP444888.tmp
  if ( -e 2dx_avrgamphs.phares.txt ) then
    set PHARES_SYM = `cat 2dx_avrgamphs.phares.txt | head -n 1 | tail -n 1`
    set PHARES_NUM_SPOTS = `cat 2dx_avrgamphs.phares.txt | head -n 2 | tail -n 1`
    echo "set PHARES_SYM = ${PHARES_SYM}" >> LOGS/${scriptname}.results
    echo "set PHARES_NUM_SPOTS = ${PHARES_NUM_SPOTS}" >> LOGS/${scriptname}.results
    echo ":PHARES_SYM set to ${PHARES_SYM}"
  else
    echo "::PROBLEM IN AVRGAMPHS"
  endif
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
${bin_2dx}/2dx_origtiltk.exe << eot
SCRATCH/2dx_origtilt-LOG1.dat
${spcgrp_first},0,F,F,1,${realcell},${ALAT},${realang},0,15,${IAQP2},${IVERBOSE},${LOGOUTPUT} !ISPG,NPRG,NTL,NBM,ILST,A,B,W,ANG,IPL,MNRF,IAQP2,IVERBOSE,LOGOUTPUT
10,0.7,10,0.5                                                              ! itaxastep,rtaxasize,itanglstep,rtanglsize
${imagenumber},0,30,${MergeIQMAX},${phastepnum},F,F,${ampweight}          !IRUN,LHMN,LHMX,IQMX,IBXPHS,NREFOUT,NSHFTIN,RFACAMP
0000000100 DUMMY
${aphdummy}
${imagenumber},${imagename},${date}
${aphfile}
F
${TAXA},${TANGL},0                                                  ! TAXA,TANGL,IORIGT
${lattice} 					! Reciprocal lattice
${phaori_local},${phastep},${zwinlocal},${sgnxchval},${SCL},${rot180val},${revhkval},${ctfrevval},${rot90val},${revhndval},${revxsgnval},${LPROTFOUFIL} ! OH,OK,STEP,WIN,SGNXCH,SCL,ROT,REV,CTFREV,ROT90,REVHND,REVXSGN,LPROTFOUFIL
${CS},${KV},${beamtilt}                                                ! cs,kv,tx,ty
${RESMIN},${RESMAX}                                                 ! resolution limits
-1
eot
#
\rm -f SUMMARY
\cp -f fort.3 APH/${prefix}${imagename}.cor.origtiltd.aph
# \cp -f ../../merge/APH/merge.aph APH/${prefix}${imagename}.cor.origtiltd.aph
#
echo "# IMAGE: APH/${prefix}${imagename}.cor.origtiltd.aph <APH: ${prename}Amp&Phs after ORIGTILT [H,K,Z,A,P(CTF Phase flipped, PhaOri),Num,IQ,WGHT,Back,CTF]>" >> LOGS/${scriptname}.results
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
set zminmax = "-0.5,0.5"
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
echo "# IMAGE: ${prefix}avrg.hkl <TXT: ${prename}APH file after AVRGAMPHS [H,K,L,A,P,FOM]>" >> LOGS/${scriptname}.results
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
\rm -f ${prefix}centric_phase_zero.hkl
#
${bin_2dx}/2dx_centric3.exe << eot
${prefix}avrg.hkl
${prefix}centric.hkl
${prefix}centric.hk
${prefix}centric_phase_zero.hkl
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
  ${proc_2dx}/protest "ERROR occured. ${prefix}centric.hkl missing."
endif
#
echo "# IMAGE: ${prefix}centric.hkl <TXT: ${prename}APH file after CENTRIC [H,K,L,A,P,FOM]>" >> LOGS/${scriptname}.results
#
#############################################################################
${proc_2dx}/linblock "f2mtz - to translate avrg.hkl into ${imagename}.mtz"
#############################################################################
#
set infile = ${prefix}centric.hkl
#
echo "# IMAGE: ${infile} <TXT: HKL file>" >> LOGS/${scriptname}.results
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
  set outfile = ${prefix}${imagename}-permutated.hkl
  \rm -f ${outfile}
  #
  ${bin_2dx}/2dx_permutate_file.exe << eot
${infile}
${outfile}
1
eot
  #
  if ( ! -s ${outfile} ) then
    ${proc_2dx}/protest "ERROR in 2dx_permutate_file.exe"
  endif
  #
  echo "# IMAGE: ${outfile} <TXT: APH file after permutation>" >> LOGS/${scriptname}.results
  #
  ${proc_2dx}/linblock "f2mtz - to transform into MTZ file"
  #
  ${bin_ccp4}/f2mtz hklin ${prefix}${imagename}-permutated.hkl hklout ${prefix}${imagename}.mtz << eof
TITLE  Map, Symmetry=${CCP4_SYM}, ${prename}${imagename}, ${date}
CELL ${celly} ${ALAT} ${cellx} 90.0  ${realang} 90.0 
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
#
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
echo "# IMAGE: ${prefix}${imagename}.mtz <MTZ file of Amp&Phs>" >> LOGS/${scriptname}.results
#
set infile = ${prefix}centric_phase_zero.hkl
echo "# IMAGE: ${infile} <HKL file of phase zero>" >> LOGS/${scriptname}.results
#
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
  ${proc_2dx}/linblock "2dx_permutate - to transform into K,L,H for PSF file"
  #
  set outfile = ${prefix}${imagename}_phase_zero-permutated.hkl
  \rm -f ${outfile}
  #
  ${bin_2dx}/2dx_permutate_file.exe << eot
${infile}
${outfile}
1
eot
  #
  if ( ! -s ${outfile} ) then
    ${proc_2dx}/protest "ERROR in 2dx_permutate_file.exe"
  endif
  #
  echo "# IMAGE: ${prefix}${imagename}_phase_zero-permutated.hkl <permutated HKL file>" >> LOGS/${scriptname}.results
  #
  ${proc_2dx}/linblock "f2mtz - to transform into MTZ file of phase zero"
  #
  ${bin_ccp4}/f2mtz hklin ${prefix}${imagename}_phase_zero-permutated.hkl hklout ${prefix}${imagename}_phase_zero.mtz << eof
TITLE  Map, Symmetry=${CCP4_SYM}, ${prename}${imagename}_phase_zero, ${date}
CELL ${celly} ${ALAT} ${cellx} 90.0 ${realang} 90.0 
SYMMETRY ${CCP4_SYM}
LABOUT H K L F PHI FOM
CTYPOUT H H H F P W
SKIP 1
END
eof
  #
  # \rm -f ${prefix}${imagename}_phase_zero-permutated.hkl
  #
else
#
  ${bin_ccp4}/f2mtz hklin ${infile} hklout ${prefix}${imagename}_phase_zero.mtz << eof
TITLE  Map, Symmetry=${CCP4_SYM}, ${prename}${imagename}_phase_zero, ${date}
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
${bin_ccp4}/mtzdump hklin ${prefix}${imagename}.mtz << eot > mtzdump.txt
NREF 100
END
eot
# echo "# IMAGE: mtzdump.txt <TXT form of MTZ file of Amp&Phs>" >> LOGS/${scriptname}.results
#
#############################################################################
${proc_2dx}/linblock "fft - to transform ${prefix}${imagename}.mtz into SCRATCH/scratch1.map"
#############################################################################
#
# contrast for grey plot
set scale = 1
echo "scale = ${scale}"
#
\rm -f SCRATCH/scratch1_phase_zero.map
#
if ( ${rotate_to_Z} == "yes" ) then
  #
  set AXIS = "Z,X,Y"
  #
  ${bin_ccp4}/fft hklin ${prefix}${imagename}_phase_zero.mtz mapout SCRATCH/scratch1_phase_zero.map  << eot
LABIN F1=F PHI=PHI ##
PROJECTION
AXIS ${AXIS}
SCALE F1 ${scale} ${tempfac}
RESOLUTION ${RESMIN} ${RESMAX}
TITLE Sym=${SYM_sub}, ${imagename}, ${date}, res=${RESMAX}, T=${tempfac}
GRID 200 20 200
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
  ${bin_ccp4}/fft hklin ${prefix}${imagename}_phase_zero.mtz mapout SCRATCH/scratch1_phase_zero.map  << eot
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
GRID 200 20 200
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
${proc_2dx}/linblock "mapmask - to extend SCRATCH/scratch1.map into ${prefix}${imagename}-${SYM_sub}.rec.mrc"
#############################################################################
#
\rm -f SCRATCH/${prefix}${imagename}_phase_zero-${SYM_sub}.map
#
if ( ${rotate_to_Z} == "yes" ) then
  #
  \rm -f SCRATCH/TMP001_phase_zero.map
  #
  echo "Checkpoint 1"
  #
  ${bin_ccp4}/mapmask mapin SCRATCH/scratch1_phase_zero.map mapout SCRATCH/TMP001_phase_zero.map << eof
MODE mapin
XYZLIM -0.3 0.3 -0.3 0.3 -0.0 0.0
END
eof
  #
  \rm -f SCRATCH/TMP001.mrc
  #
  echo "Checkpoint 2"
  #
  ${bin_2dx}/labelh.exe << eot
SCRATCH/TMP001_phase_zero.map
13
SCRATCH/${prefix}${imagename}_phase_zero-${SYM_sub}.map
eot
  #
else
  #
  ${bin_ccp4}/mapmask mapin SCRATCH/scratch1_phase_zero.map mapout SCRATCH/${prefix}${imagename}_phase_zero-${SYM_sub}.map << eof
MODE mapin
XYZLIM -0.3 0.3 -0.3 0.3 -0.0 0.0
END
eof
  #
endif
#
\rm -f SCRATCH/${prefix}${imagename}-${SYM_sub}.map
#
if ( ${rotate_to_Z} == "yes" ) then
  #
  \rm -f SCRATCH/TMP001.map
  #
  ${bin_ccp4}/mapmask mapin SCRATCH/scratch1.map mapout SCRATCH/${prefix}${imagename}-${SYM_sub}.map << eof
XYZLIM 0 399 0 0 0 399
AXIS Z X Y
END
eof
  #
else
  #
  ${bin_ccp4}/mapmask mapin SCRATCH/scratch1.map mapout SCRATCH/${prefix}${imagename}-${SYM_sub}.map << eof
XYZLIM 0 399 0 399 0 0
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
# ${proc_2dx}/linblock "LABEL - to create a clean MRC file format instead of CCP4"
#############################################################################
#
\rm -f ${prefix}${imagename}_phase_zero-${SYM_sub}.mrc
#
${bin_2dx}/labelh.exe << eot
SCRATCH/${prefix}${imagename}_phase_zero-${SYM_sub}.map 
2
${prefix}${imagename}_phase_zero-${SYM_sub}.mrc
1,0
0
eot
#
# cp -f SCRATCH/${prefix}${imagename}_phase_zero-${SYM_sub}.map ${prefix}${imagename}_phase_zero-${SYM_sub}.mrc
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
# cp -f SCRATCH/${prefix}${imagename}-${SYM_sub}.map ${prefix}${imagename}-${SYM_sub}.mrc
#
if ( ${SYM_sub} == 'p1' ) then
  \rm -f final_map.mrc
  \ln -s ${prefix}${imagename}-p1.mrc final_map.mrc
endif

if ( ${movie_enable}x == "nx" ) then
  \cp -f ${prefix}${imagename}-p1.mrc u2_map.mrc
endif

#
if ( ${SYN_Unbending} == "0" ) then
  \cp -f final_map.mrc final_map_FouFilter.mrc
else
  \cp -f final_map.mrc final_map_SynRef.mrc
endif
#
echo "# IMAGE: ${prefix}${imagename}_phase_zero-p1.mrc <PSF as map>"  >> LOGS/${scriptname}.results
echo "# IMAGE: ${prefix}${imagename}_phase_zero-${SYM_sub}.mrc <PSF symmetrized as map>" >> LOGS/${scriptname}.results
# echo "# IMAGE: SCRATCH/${prefix}${imagename}-p1.mrc <SCRATCH/${prename}Non-symmetrized Map>"  >> LOGS/${scriptname}.results
echo "# IMAGE-IMPORTANT: ${prefix}${imagename}-p1.mrc <${prename}Non-symmetrized Map>"  >> LOGS/${scriptname}.results
# echo "# IMAGE: SCRATCH/${prefix}${imagename}-${SYM_sub}.map <SCRATCH/${prename}${SYM_sub}-symmetrized Map>" >> LOGS/${scriptname}.results
echo "# IMAGE-IMPORTANT: ${prefix}${imagename}-${SYM_sub}.mrc <${prename}${SYM_sub}-symmetrized Map>" >> LOGS/${scriptname}.results
#
echo "# IMAGE: final_map_FouFilter.mrc <${prename}Non-symmetrized Map Fourier Filter>"  >> LOGS/${scriptname}.results
echo "# IMAGE: final_map_SynRef.mrc <${prename}Non-symmetrized Map Synthetic Reference>"  >> LOGS/${scriptname}.results
echo "# IMAGE: ${imagename}_ref.mrc <Reference 3D Map Projection>"  >> LOGS/${scriptname}.results
#
#############################################################################
${proc_2dx}/linblock "npo - to create a line plot ${imagename}-${SYM_sub}.plt"
#############################################################################
#
if ( ${rotate_to_Z} == "yes" ) then
  set title_phase_zero = "TITLE PSF. Symmetry: ${SYM_sub}. Grid: 20A. Line 50% of PSF. Axis: right=X, down=Y."
  set title_map = "TITLE Symmetry: ${SYM_sub}. Filename: ${prename} ${imagename}. Axis: right=X, down=Y."
else
  set title_phase_zero = "TITLE PSF. Symmetry: ${SYM_sub}. Grid: 20A. Line 50% of PSF."
  set title_map = "TITLE Symmetry: ${SYM_sub}. Filename: ${prename} ${imagename}"
endif
\rm -f ${prefix}${imagename}_phase_zero-${SYM_sub}.plt
#
${bin_ccp4}/npo  MAPIN  SCRATCH/${prefix}${imagename}_phase_zero-${SYM_sub}.map  PLOT  ${prefix}${imagename}_phase_zero-${SYM_sub}.plt  << eof
${title_phase_zero}
MAP SCALE 2.0
CONTRS 0 TO 250 BY 125
MODE BELOW 125 DASHED 1 0.15 0
SECTS 0 0
GRID 20 20
PLOT
END
eof
#
\rm -f ${prefix}${imagename}-${SYM_sub}.plt
#
${bin_ccp4}/npo  MAPIN  SCRATCH/${prefix}${imagename}-${SYM_sub}.map  PLOT  ${prefix}${imagename}-${SYM_sub}.plt  << eof
${title_map}
MAP SCALE 0.4
CONTRS SIG -3.0 TO 3.0 BY ${npo_cntrs_step} 
MODE GREEN BELOW ${npo_cntrs_below} DASHED 1 0.15 0
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
\rm -f PS/${prefix}${imagename}PSF-${SYM_sub}.ps
${bin_2dx}/laserplot.exe -outputfile=PS/${prefix}${imagename}PSF-${SYM_sub}.ps ${prefix}${imagename}_phase_zero-${SYM_sub}.plt
#
\rm -f ${imagename}_phase_zero-${SYM_sub}.plt
#
\rm -f PS/${prefix}${imagename}MAP-${SYM_sub}.ps
${bin_2dx}/laserplot.exe -outputfile=PS/${prefix}${imagename}MAP-${SYM_sub}.ps ${prefix}${imagename}-${SYM_sub}.plt
#
\rm -f ${imagename}-${SYM_sub}.plt
#
echo "# IMAGE-IMPORTANT: PS/${prefix}${imagename}PSF-p1.ps <PS: PSF>"  >> LOGS/${scriptname}.results
echo "# IMAGE-IMPORTANT: PS/${prefix}${imagename}PSF-${SYM_sub}.ps <PS: PSF symmetrized>" >> LOGS/${scriptname}.results
echo "# IMAGE-IMPORTANT: PS/${prefix}${imagename}MAP-p1.ps <PS: ${prename}Non-symmetrized Map>"  >> LOGS/${scriptname}.results
echo "# IMAGE-IMPORTANT: PS/${prefix}${imagename}MAP-${SYM_sub}.ps <PS: ${prename}${SYM_sub}-symmetrized Map>" >> LOGS/${scriptname}.results
#
if ( ${movie_enable}x == "nx" ) then
  \cp -f PS/${prefix}${imagename}MAP-p1.ps u2_map.ps
endif
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
if ( ${produceSpiderMapLocal} == 'n' ) then
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
  exit
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
X31=${realang}
X32=X31-90.0
X33=180.0-X31
X36=1.0/${mapscale}
; X36 is how many pixels are in one Angstroem
X34=${unitcellnumber}*X36*${realu}/cos(X32)
X35=${unitcellnumber}*X36*${realv}
;
;
IP
TMP003
TMP004
X34,X35
;
; NOW SHEARING:
;
vm
echo "::X33 = {*****X33}"
;
if(X33.lt.90.0)then
SZ
TMP004
X33
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
cp to mrc
TMP006
${prefix}${imagename}-${SYM_sub}-scaled.mrc
-9999
;
goto LB77
; the usual origin top-left / bottom-left issue:
MR
TMP006
TMP007
X
;
fs
TMP007
;
fi
TMP007
;
cp to tiff
TMP007
${prefix}${imagename}-${SYM_sub}-scaled.tif
;
cp to mrc
TMP007
${prefix}${imagename}-${SYM_sub}-scaled.mrc
-9999
;
LB77
;
de a
TMP001
;
; if you want to keep the SPIDER results logfile, finish with "en". 
; if you don't want to keep the SPIDER results logfile, finish with "en d". 
;
en d
eot
  #
  \rm -f LOG.spi
  \rm -f fort.1
  ${proc_2dx}/linblock "TIFF file created: ${prefix}${imagename}-${SYM_sub}-scaled.tif"
  echo "# IMAGE-IMPORTANT: ${prefix}${imagename}-${SYM_sub}-scaled.tif <${prename}${SYM_sub} Spider Map (TIFF)>" >> LOGS/${scriptname}.results
  echo "# IMAGE-IMPORTANT: ${prefix}${imagename}-${SYM_sub}-scaled.mrc <${prename}${SYM_sub} Spider Map (MRC)>" >> LOGS/${scriptname}.results
  #
#  #############################################################################
#  ${proc_2dx}/linblock "tif2mrc - to create a MRC file format from the TIFF file"
#  ############################################################################# 
#  #
#   \rm -f ${prefix}${imagename}-${SYM_sub}-scaled.mrc
#   ${bin_2dx}/tif2mrc.exe << eot
# ${prefix}${imagename}-${SYM_sub}-scaled.tif
# ${prefix}${imagename}-${SYM_sub}-scaled.mrc
# eot
  #
  #
endif
#
