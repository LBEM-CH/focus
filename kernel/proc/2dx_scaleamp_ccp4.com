#!/bin/csh
#2dx_scaleamp_cpp4.sh
#
# To scale the amplitudes of a MTZ file to 100
#
if ( $#argv < 2 ) then
	echo "$#argv"
	echo "Usage: `basename $0` <mtz-map> <output>" 
	exit 1
endif
set map=$1
set output=$2
set filename = `echo $1 | awk -F. '{ print $1 }'`
#source /Applications/ccp4-6.1.13/bin/ccp4.setup-csh   
sftools << eot > map.hkl
read  ${map}
checkhkl
eot
set average = `grep  "   1    " map.hkl | awk '{print $6}'`
echo $average

sftools << eot
read  ${map}
calc col F = col 1 ${average} / 100.0 *
set type col 4 
F
write ${output}
checkhkl
end
eot

