#!/bin/csh -e
####
#
if ( $#argv == 0 ) then
  echo "mrcdeftilt: determines tilt geometry from defoci at various locations on image."
  echo "    usage: mrcdeftilt [mp|sp] [2|3]"
  echo "     with: mp=MultiProcessors"
  echo "           sp=SingleProcessor"
  echo "           2 =only 2nd middle part"
  echo "           3 =only central defocus"
  echo "  example: mrcdeftilt mp"
  echo "       or: mrcdeftilt sp"
  echo "       or: mrcdeftilt sp 2"
  echo "       or: mrcdeftilt sp 3"
  echo "       or: mrcdeftilt 2"
  echo "       or: mrcdeftilt 3"
else
  #
  tredat 6 DATAFILE.dat 
  #
  set image = `getline DATAFILE.dat 12`
  # use unmasked image
  set image =  `echo $image | sed 's/^m//'`
  #
  mrcmakedirs
  #
  \rm -f LOGS/mrcdeftilt.log
  #
  set mode = '_mp'
  if ( $#argv >= 1 ) then
    if ( $1 == 'sp' || $1 == '_sp' ) then
      set mode = ''
    endif
  endif
  #
  if ( ${mode} == '_mp' ) then
    lin "mrcdeftilt (mp)"
  else
    lin "mrcdeftilt (sp)"
  endif
  #
  set second = 0
  if ( $#argv == 1 ) then
    if ( $1 == '2' ) then
      set second = 1
    endif
    if ( $1 == '3' ) then
      set second = 3
    endif
  endif
  if ( $#argv == 2 ) then
    if ( $2 == '2' ) then
      set second = 1
    endif
    if ( $2 == '3' ) then
      set second = 3
    endif
  endif
  #
  if ( $second == '0' || $second == '3' ) then
    if ( !( -e FFTIR/${image}.red.fft.mrc ) || !( -e FFTIR/${image}.fft.mrc )) then
      fftir_unmasked ${mode}
    else
      mrcprepare
      lin "fftir is not neccessary"
    endif
  endif
  #
  if ( ${second} == '0' ) then
    lin "mrcdeftilt: determining defoci in ${image}"
  endif
  if ( ${second} == '2' ) then
    lin "mrcdeftilt: determining geometry from defoci in ${image}"
  endif
  if ( ${second} == '3' ) then
    lin "mrcdeftilt: determining central defocus in ${image}"
  endif
  #
  set range1 = 15000.0
  set range2 = 4000.0
  #
  nice -5 mrcdeftilt.1.com ${image} ${range1} ${range2} ${second} ${mode} >> LOGS/mrcdeftilt.log
  #
  if ( $second == '3' ) then
    echo "Central defocus is:"
    getline DATAFILE.dat 32
    exit
  endif
  #
  echo "Defoci in different corners are:"
  getline DATAFILE.dat 160
  getline DATAFILE.dat 161
  getline DATAFILE.dat 162
  getline DATAFILE.dat 163
  getline DATAFILE.dat 164
  getline DATAFILE.dat 165
  getline DATAFILE.dat 166
  #
  lin "mrcdeftilt: determining tiltgeometry in ${image}"
  nice -5 mrcdeftilt.3.com >> LOGS/mrcdeftilt.log
  #
  tail -2 TMP.mrcdeftilt.3.out
  set line = `tail -1 TMP.mrcdeftilt.3.out`
  set TLTAXIS = `echo ${line} | cut -d\  -f6`
  set TLTANG  = `echo ${line} | cut -d\  -f3`
  set cell = `getline DATAFILE.dat 32`
  #
  if ( ${cell} != '0.0,0.0,0.0,0.0' ) then
    # lin "mrcdeftilt: determining other tiltgeometry angles"
    nice -5 mrcdeftilt.4.com ${TLTAXIS} ${TLTANG} ${cell} >> LOGS/mrcdeftilt.log
    #
    set line = `tail -3 TMP.mrcdeftilt.4.out | head -1`
    set TLTAXA = `echo ${line} | cut -d\  -f3`
    set line = `tail -2 TMP.mrcdeftilt.4.out | head -1`
    set TAXA   = `echo ${line} | cut -d\  -f3`
    set line = `tail -1 TMP.mrcdeftilt.4.out | head -1`
    set TANGL  = `echo ${line} | cut -d\  -f3`
  else
    set TLTAXA = '0.0'
    set TAXA   = '0.0'
    set TANGL  = '0.0'
  endif
  #
  tredat 4 DATAFILE.dat 17 ${TLTAXIS}
  tredat 4 DATAFILE.dat 18 ${TLTANG}
  tredat 4 DATAFILE.dat 19 ${TLTAXA}
  tredat 4 DATAFILE.dat 20 ${TAXA}
  tredat 4 DATAFILE.dat 24 ${TANGL}
  #
  tredat 5 DATAFILE.dat 152 41 ${TLTAXIS}
  tredat 5 DATAFILE.dat 153 41 ${TLTANG}
  tredat 5 DATAFILE.dat 154 41 ${TLTAXA}
  tredat 5 DATAFILE.dat 155 41 ${TAXA}
  tredat 5 DATAFILE.dat 156 41 ${TANGL}
  tredat 5 DATAFILE.dat 151 11 "mrclattilt"
  tredat 5 DATAFILE.dat 151 26 "mrctiltaxcalc"
  tredat 5 DATAFILE.dat 151 41 "MRCDEFTILT"
  tredat 5 DATAFILE.dat 151 56 "ttrefine"
  tredat 5 DATAFILE.dat 151 71 "origtiltd"
  #
  getline DATAFILE.dat 151
  getline DATAFILE.dat 152
  getline DATAFILE.dat 153
  getline DATAFILE.dat 154
  getline DATAFILE.dat 155
  getline DATAFILE.dat 156
  lin "-"
  #
  echo dummy > TMP.mrcdeftilt.1
  echo dummy > CUT/${image}.1.1.mrc
  # \rm -f TMP.mrcdeftilt.* TMP-runfile.com TMP-positions.com
  \rm -f CUT/${image}.?.?.mrc 
  #
  echo " "
  echo " Try     mrcdeftilt.disp"
  echo " "
  #
endif
#
