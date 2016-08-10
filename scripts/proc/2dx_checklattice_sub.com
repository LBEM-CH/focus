#
#  2dx_checklattice-sub.com
#
# Attention:
# This is not an independent csh file.
# This file has to be sourced from another csh.
#
# Input is in ${lattice} ${realcell} ${realang} ${magnification} ${stepdigitizer} ${imagesidelength} ${TLTANG} ${TLTAXIS} ${TLTAXA}
# Output is in ${latticeok}
#
#
#############################################################################
${proc_2dx}/linblock "2dx_lencalc: To calculate some values for the tilt geometry determination."
#############################################################################
#
set docfile = "2dx_lencalc-doc.tmp"
#
\rm -f ${docfile}
#
${bin_2dx}/2dx_lencalc.exe << eot
${lattice}
${realcell}
${realang}
${imagesidelength}
${magnification}
${stepdigitizer}
${docfile}
eot
#
cat ${docfile}
#
set hand     = `head -n 2 ${docfile} | tail -n 1`
set untilted = `head -n 4 ${docfile} | tail -n 1`
set alpha    = `head -n 6 ${docfile} | tail -n 1`
set tilted   = `head -n 8 ${docfile} | tail -n 1`
set realtlt  = `head -n 10 ${docfile} | tail -n 1`
#
\rm -f ${docfile}
#
set reallatu = `echo ${realtlt} | cut -d\  -f1`
set reallatv = `echo ${realtlt} | cut -d\  -f2`
set locrealcell = `echo ${reallatu},${reallatv}`
set reallatang = `echo ${realtlt} | cut -d\  -f3`
#
set realu = `echo ${realcell} | sed 's/,/ /g' | awk '{ s = $1 } END { print s }'`
set realv = `echo ${realcell} | sed 's/,/ /g' | awk '{ s = $2 } END { print s }'`
#
set istilt = `echo $TLTANG | awk '{if ( -6.0 < $1 && $1 < 6.0 ) {s = 0} else {s = 1}} END {print s}'`
if ( ${istilt} == '0' ) then
  echo ": This is a non-tilted image (istilt = ${istilt})"
  set latdiffu = `echo ${reallatu} ${realu} ${realv} | awk '{ s = ( 200 * sqrt (( $1 - $2 ) * ( $1 - $2 ))) / ( $2 + $3 ) } END { print s }'`
  set latdiffv = `echo ${reallatv} ${realu} ${realv} | awk '{ s = ( 200 * sqrt (( $1 - $3 ) * ( $1 - $3 ))) / ( $2 + $3 ) } END { print s }'`
  set latdiff3 = `echo ${realang} ${reallatang} | awk '{ s = sqrt (( $1 - $2 ) * ( $1 - $2 )) } END { print s }'`
  echo ":relative vector u difference = ${latdiffu}%"
  echo ":relative vector v difference = ${latdiffv}%"
  echo ":angle difference = ${latdiff3}"
  set badreallat = `echo ${latdiffu} ${latdiffv} ${latdiff3} | awk '{ if ( $1 > 20  || $2 > 20 || $3 > 10 ) { s = 1 } else { s = 0 }} END { print s }'`
  if ( ${badreallat} == '1' ) then
    set latticeok = 'n'
    echo "::"
    echo "::                              LATTICE CONFLICT DETECTED:"
    echo "::"
    echo "::         given real lattice      =  ${realu} A, ${realv} A, ${realang} deg"
    echo "::         calculated real lattice =  ${reallatu} A, ${reallatv} A, ${reallatang} deg"
    echo "::"
    echo "#WARNING: Warning: lattice conflict detected. Is your real-space lattice correct? (Run 'Evaluate Lattice'.)" >> LOGS/${scriptname}.results
  else
    set latticeok = 'y'
    echo ": given real lattice      =  ${realu} A, ${realv} A, ${realang} deg"
    echo ": calculated real lattice =  ${reallatu} A, ${reallatv} A, ${reallatang} deg"
  endif
else
  set latticeok = 'u'
endif
#
echo "latticeok = ${latticeok}"
#
