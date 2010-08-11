#!/bin/csh -ef
#
#
if ( $#argv != 4 ) then
  echo 2dx_split.com : split one dirfile into two sub-files, 
  echo containing one special line in one, and the rest in the other.
  echo \\t usage: 2dx_split.com \<inputfile\> \<longerfile\> \<onelinefile\> \<linenum\>
  echo \\t example: 2dx_split.com 2dx_merge_dirfile.dat 2dx_merge_mergefile.dat 2dx_merge_oneline.dat 3
else
  set inputfile = $1
  set longerfile = $2
  set onelinefile = $3
  set linenum = $4
  set endnum    = `cat ${inputfile} | wc -l`
  set beforenum = `echo ${linenum} | awk '{ s = $1 - 1 } END { print s }'`
  set afternum  = `echo ${linenum} ${endnum} | awk '{ s = $1 - $2 } END { print s }'`
  cat ${inputfile} | head -n ${linenum} | tail -n 1 > ${onelinefile}
  if ( ${beforenum} == '0' ) then
    \rm -f ${longerfile}
  else
    cat ${inputfile} | head -n ${beforenum} > ${longerfile}
  endif
  if ( ${afternum} != '0' ) then
    cat ${inputfile} | tail -n ${afternum} >> ${longerfile}
  endif
endif
#

