

spider dat <<EOT
bp 3g
spider_particles/spi_particle_*****
1-$1
spider_particles/info
*
vol_spi
en
EOT


spider dat <<EOT
cp to mrc
vol_spi
vol_mrc
-9999
en
EOT

mv vol_mrc.dat vol.mrc