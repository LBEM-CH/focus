

\rm -f tmp.tmp

set counter = ${GPU_how_many}
@ counter -= 1

while ( ${counter} >= 0 ) 
  set GPUload = `nvidia-smi -i ${counter} | head -n 9 | tail -n 1 | cut -c 58-66 | cut -d\% -f1 `
  set GLUloadplus1000 = `echo ${GPUload} | awk '{ s = $1 + 1000 } END { print s }'`
  echo ${GLUloadplus1000} ${counter} >> tmp.tmp
  echo "GPU "${counter}" has load of "${GPUload}
  @ counter -= 1
end

sort tmp.tmp
set next_GPU = `sort tmp.tmp | head -n 1 | cut -d\   -f2`

echo "This job is for GPU "${next_GPU}"."

\rm -f tmp.tmp

