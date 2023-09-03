

set trackfile = "../../.GPU_in_use.tmp"
if ( -e ${trackfile} ) then
  set current_GPU = `cat ../../.GPU_in_use.tmp`
  set next_GPU = `echo ${current_GPU} | awk '{ s = $1 + 1 } END { print s }'`
  set next_GPU = `echo ${next_GPU} ${GPU_how_many} | awk '{ if ( ( $1 + 1 ) > $2 ) { s = 0 } else { s = $1 } } END { print s }'`
else
  set next_GPU = "0"
endif
#
echo ${next_GPU} > ${trackfile}
#
echo "This job is for GPU "${next_GPU}"."


