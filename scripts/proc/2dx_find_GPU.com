
set GPU0 = `nvidia-smi  -i 0 | head -n 9 | tail -n 1 | cut -c 58-66 | cut -d\% -f1 `
set GPU1 = `nvidia-smi  -i 1 | head -n 9 | tail -n 1 | cut -c 58-66 | cut -d\% -f1 `

set next_GPU = `echo ${GPU0} ${GPU1} | awk '{ if ( $1 < $2 } { s = 0 } else { s = 1 } END { print s }'`

echo "GPU0 load is ${GPU0}%, GPU1 load is %{GPU1}%, choosing for next job GPU${next_GPU}."

