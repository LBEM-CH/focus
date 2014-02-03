echo 'converting images from mrc to spider'

FILES=spider_particles/particle*

for f in $FILES
do
	echo "Processing $f file..."
	im_mrc=${f:0:31}
	im_spi="${f:0:17}spi_${f:17:14}"
	echo $im_mrc
	echo $im_spi
	
spider dat <<EOT
cp from mrc
$im_mrc
$im_spi
en
EOT
		
done

rm spider_particles/particle*



