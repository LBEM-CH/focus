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
echo bin_2dx = ${bin_2dx}
echo proc_2dx = ${proc_2dx}
echo ccp4 = ${ccp4}
echo bin_ccp4 = ${bin_ccp4}
#
# this is for a later option:
set mode = 0
#
if ( ${mode} == 0 ) then
  #
  #############################################################################
  #                                                                           #
  ${proc_2dx}/linblock "LATLINPRESCAL - to apply CTF correction and weight calculation"
  #                                                                           #
  #                                          merge_3ds.aph  =>  latlines.dat  #
  #                                                                           #
  #############################################################################
  #
  \rm -f fort.1
  \rm -f fort.3
  #
  \ln -s APH/merge.aph fort.1
  #
  ${bin_2dx}/2dx_latlinprescal.exe << eot > LOGS/latlinprescal.log
1001,${zminmax} ! NSER,ZMIN,ZMAX
${MergeIQMAX}               ! IQMAX
${max_amp_correction}
eot
  #
  \rm -f fort.1
  echo "################################################"
  echo "################################################"
  echo "output in file LOGS/latlinprescal.log"
  echo "################################################"
  echo "################################################"
  #
  if ( -e fort.3 ) then
    \mv -f fort.3 SCRATCH/latlines.dat
    echo "# IMAGE: SCRATCH/latlines.dat <Lattice line data after pre-scaling [H,K,Z,A,P,FOM,SIGAMP,SIGANG,IQ]>" >> LOGS/${scriptname}.results
    echo "# IMAGE: LOGS/latlinprescal.log <LOG: latlinprescal output>" >> LOGS/${scriptname}.results
  else
    ${proc_2dx}/protest "ERROR: latlines.dat does not exist."
  endif
  #
  echo " "
  ${proc_2dx}/lin "-"
  echo " "
  #
endif
#
#############################################################################
#                                                                           #
${proc_2dx}/linblock "LATLINEK - to fit lattice lines to merged data (D.Agard's program)"
#                                                                           #
#                latlines.dat  =>  latfitteds.dat + LATLINE.PLT + guess.dat #
#                                                                           #
#############################################################################
#
echo "<<@progress: +5>>"
#
\rm -f PLOT.PS
\rm -f PS/latline.ps
\rm -f SCRATCH/latfitteds.dat
\rm -f SCRATCH/guess.dat
\rm -f latline.statistics
#
set iplterr = 0
set idoh = 0
set idok = 0
set IAQP2 = 0
set iplterr = 1         
set imaxIQplot = 4
set MergeIGUESS = 1
#
if ( ${tempkeep} == 'y' ) then
  set iverbose = 1
else
  set iverbose = 1
endif
#
setenv  OBS   SCRATCH/latlines.dat
setenv  OUT   SCRATCH/latfitteds.dat
setenv  GUESS SCRATCH/guess.dat
#
echo " "
echo " Parameters for latline are:"
echo " "
echo "2dx_merge_latline_sub.com, ${date}"
echo "${spcgrp}                            # IPG (Plane group number 1-17)"
echo "0                             # IPAT, 0=F & Phase, 1=Intensity data"
echo "${MergeAK},${MergeIWF_VAL},${MergeIWP_VAL}                      # AK,IWF,IWP - relative weights, + individual sigmas"
echo "${ALAT},${zminmax},${MergeDELPLT}        # ALAT,ZMIN,ZMAX,DELPLT "
echo "${MergeDELPRO},${MergeRminRmax},${MergeRCUT},${MergePFACT}        # DELPRO,RMIN,RMAX,RCUT,PFACT"
echo "${MergeIGUESS},${MergeBINSIZ}                       # IGUESS,BINSIZ"
echo "${MergeNCYCLS},${MergeMPRINT}                          # NCYCLS,MPRINT"
echo "${idoh},${idok}		                          # H,K indices to plot. 0 0 = all."
echo "${IAQP2},${iplterr},${imaxIQplot}                         # IAQP2: 1=y,0=n, iplterr=1:errbar in PHS, maxIQ for PSplot"
echo " "
#
${bin_2dx}/2dx_latlinek.exe << eot > LOGS/2dx_latlinek.log
2dx_merge_latline_sub.com, ${date}
${spcgrp}                                                    ! IPG (Plane group number 1-17)
0                                                            ! IPAT, 0=F & Phase, 1=Intensity data
${MergeAK},${MergeIWF_VAL},${MergeIWP_VAL}                   ! AK,IWF,IWP - relative weights, + individual sigmas
${ALAT},${zminmax},${MergeDELPLT}                            ! ALAT,ZMIN,ZMAX,DELPLT
${MergeDELPRO},${MergeRminRmax},${MergeRCUT},${MergePFACT}   ! DELPRO,RMIN,RMAX,RCUT,PFACT
${MergeIGUESS},${MergeBINSIZ}                                ! IGUESS,BINSIZ
${MergeNCYCLS},${MergeMPRINT}                                ! NCYCLS,MPRINT
${idoh},${idok}                                              ! H,K indices to plot. 0 0 = all.
${IAQP2},${iplterr},${imaxIQplot}                            ! IAQP2: 1=y,0=n, iplterr=1:errbar in PHS, maxIQ for PSplot
eot
#
echo "################################################"
echo "################################################"
echo "output in file LOGS/2dx_latlinek.log"
echo "################################################"
echo "################################################"
#
echo "# IMAGE: LOGS/2dx_latlinek.log <LOG: 2dx_latlinek output>" >> LOGS/${scriptname}.results
echo "# IMAGE: SCRATCH/latline_stat.dat <Lattice line statistics>" >> LOGS/${scriptname}.results
echo "# IMAGE: SCRATCH/latfitteds.dat <Lattice line fit data [H,K,Z,A,PHI,SIGF,SIGP,FOM]>" >> LOGS/${scriptname}.results
if ( -e SCRATCH/guess.dat ) then
  echo "# IMAGE: SCRATCH/guess.dat <Lattice line guess data>" >> LOGS/${scriptname}.results
endif
if ( ! -e latline.statistics ) then
  ${proc_2dx}/linblock "#"
  ${proc_2dx}/linhash "3D modus, but do you have 3D (i.e. tilted) data?"
  ${proc_2dx}/protest "ERROR in latlinek. Check logfile."
endif
\mv -f latline.statistics SCRATCH/latline_stat.dat
#
if ( ! -e PLOT.PS ) then
  ${proc_2dx}/protest "2dx_latlinek: ERROR occured."
endif
#
\mv -f PLOT.PS PS/latline.ps 
echo "# IMAGE-IMPORTANT: PS/latline.ps <PS: Lattice lines>" >> LOGS/${scriptname}.results
#
echo " "
${proc_2dx}/lin "-"
echo " "
#
echo "<<@progress: +5>>"
#
# if ( ${mode} != 0 ) then
#   exit
# endif
#
#############################################################################
#                                                                           #
${proc_2dx}/linblock "PREPMKMTZ - Program to convert fitted data to CCP4 format"
#                                                                           #
#############################################################################
#
\rm -f APH/latfitted_nosym.hkl
\rm -f APH/latfittedref.hkl
#
setenv IN SCRATCH/latfitteds.dat
setenv OUT APH/latfitted_nosym.hkl
setenv REFHKL APH/latfittedref_nosym.hkl
#
${bin_2dx}/prepmklcf.exe << eot > LOGS/prepmklcf.log
${RESMAX},1.5                          ! RESOLUTION,REDUCAC (1.5 = 60deg phase error)
${realcell},${realang},${ALAT}         ! a,b,gamma,c
1.0                                    ! SCALE
eot
#
echo "################################################"
echo "################################################"
echo "output in file LOGS/prepmklcf.log"
echo "################################################"
echo "################################################"
#
echo "# IMAGE: LOGS/prepmklcf.log <LOG: prepmklcf output>" >> LOGS/${scriptname}.results
echo "# IMAGE: APH/latfitted_nosym.hkl <Lattice lines for volume fitted after prepmklcf [H,K,L,A,P,FOM]>" >> LOGS/${scriptname}.results
echo "# IMAGE: APH/latfittedref_nosym.hkl <Lattice lines for reference fitted after prepmklcf [H,K,L,A,P,FOM,SIGA]>" >> LOGS/${scriptname}.results
#
echo "<<@progress: +5>>"
#
#############################################################################
${proc_2dx}/linblock "2dx_hklsym - to apply symmetry to latfitted APH file, for volume"
#############################################################################  
#
${bin_2dx}/2dx_hklsym.exe << eot
APH/latfitted_nosym.hkl
APH/latfitted_sym_nosort.hkl
APH/latfitted_sym_noheader.hkl
${spcgrp}
0     ! no header line
0     ! no sigma column
eot
#
\rm -f APH/latfitted_sym_noheader.hkl
echo "# IMAGE: APH/latfitted_sym_nosort.hkl <Lattice lines for volume after 2dx_hklsym [H,K,L,A,P,FOM]>" >> LOGS/${scriptname}.results
#
sort < APH/latfitted_sym_nosort.hkl > APH/latfitted_sym_sort.hkl
echo "# IMAGE: APH/latfitted_sym_sort.hkl <Lattice lines for volume after 2dx_hklsym, sorted [H,K,L,A,P,FOM]>" >> LOGS/${scriptname}.results
#
#############################################################################
${proc_2dx}/linblock "2dx_hklsym - to apply symmetry to latfitted APH file, for reference"
#############################################################################  
#
${bin_2dx}/2dx_hklsym.exe << eot
APH/latfittedref_nosym.hkl
APH/latfittedref_sym_nosort.hkl
APH/latfittedref_sym_noheader.hkl
${spcgrp}
0     ! no header line
1     ! with sigma column
eot
#
\rm -f APH/latfittedref_sym_noheader.hkl
echo "# IMAGE: APH/latfittedref_sym_nosort.hkl <Lattice lines for reference after 2dx_hklsym [H,K,L,A,P,FOM,SIGA]>" >> LOGS/${scriptname}.results
#
sort < APH/latfittedref_sym_nosort.hkl > APH/latfittedref_sym_sort.hkl
echo "# IMAGE: APH/latfittedref_sym_sort.hkl <Lattice lines for reference after 2dx_hklsym, sorted [H,K,L,A,P,FOM,SIGA]>" >> LOGS/${scriptname}.results
#
#############################################################################
${proc_2dx}/linblock "2dx_hklclean - to eliminated duplicates from APH file, for volume"
#############################################################################  
#
${bin_2dx}/2dx_hklclean.exe << eot
APH/latfitted_sym_sort.hkl
APH/latfitted.hkl
0     ! no header line
0     ! no sigma column
eot
#
echo "<<@progress: +5>>"
#
if ( ! -e APH/latfitted.hkl ) then
  ${proc_2dx}/protest "ERROR occured."
endif
#
echo "# IMAGE: APH/latfitted.hkl <Lattice lines for volume after 2dx_hklclean [H,K,L,A,P,FOM]>" >> LOGS/${scriptname}.results
#
#############################################################################
${proc_2dx}/linblock "2dx_hklclean - to eliminated duplicates from APH file, for reference"
#############################################################################  
#
${bin_2dx}/2dx_hklclean.exe << eot
APH/latfittedref_sym_sort.hkl
APH/latfittedref.hkl
0     ! no header line
1     ! with sigma column
eot
#
echo "<<@progress: +5>>"
#
if ( ! -e APH/latfitted.hkl ) then
  ${proc_2dx}/protest "ERROR occured."
endif
#
echo "# IMAGE: APH/latfittedref.hkl <Lattice lines for reference after 2dx_hklclean [H,K,L,A,P,FOM,SIGA]>" >> LOGS/${scriptname}.results
#
#############################################################################
${proc_2dx}/linblock "f2mtz - Program to convert hkl data into MTZ format, for volume"
#############################################################################
#
set infile = APH/latfitted.hkl
\rm -f merge3D.mtz
#
${bin_ccp4}/f2mtz hklin ${infile} hklout merge3D.mtz << eof
TITLE  P1 map, ${date}
CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}
SYMMETRY ${CCP4_SYM}
LABOUT H K L F PHI FOM
CTYPOUT H H H F P W
FILE ${infile}
SKIP 0
END
eof
#
echo "# IMAGE-IMPORTANT: merge3D.mtz <MTZ: Full fitted lattice line data for volume>" >> LOGS/${scriptname}.results
echo "<<@progress: +5>>"
#
#
#############################################################################
${proc_2dx}/linblock "f2mtz - Program to convert hkl data into MTZ format, for reference"
#############################################################################
#
set infile = APH/latfittedref.hkl
\rm -f merge3Dref.mtz
#
${bin_ccp4}/f2mtz hklin ${infile} hklout merge3Dref.mtz << eof
TITLE  P1 map, ${date}
CELL ${realcell} ${ALAT} 90.0 90.0 ${realang}
SYMMETRY ${CCP4_SYM}
LABOUT H K L F PHI FOM SIGA
CTYPOUT H H H F P W W
FILE ${infile}
SKIP 0
END
eof
#
echo "# IMAGE-IMPORTANT: merge3Dref.mtz <MTZ: Full fitted lattice line data for reference>" >> LOGS/${scriptname}.results
#
####################################################################################
#                                                                                  #
${proc_2dx}/linblock "sftools to expand mtz and to eliminate lattice lines with illegal phases"
#                                                                                  #
####################################################################################
#
\rm -f SCRATCH/merge3Dref_nophaerr_tmp.mtz
#
${bin_ccp4}/sftools << eof
read merge3Dref.mtz
sort h k l 
set spacegroup
${CCP4_SYM}
select phaerr
select invert
purge
y
set spacegroup
1
expand
write SCRATCH/merge3Dref_nophaerr_tmp.mtz
y
quit
eof
#
# this should garantee that the file is never missing:
\mv -f SCRATCH/merge3Dref_nophaerr_tmp.mtz merge3Dref_nophaerr.mtz
#
echo "# IMAGE-IMPORTANT: merge3Dref_nophaerr.mtz <MTZ: Lattice lines without phase-error for synref>" >> LOGS/${scriptname}.results
echo "<<@progress: +5>>"
#
#

