#!/bin/csh -fe
####
#
#
#  This is not an independent script.  It should be called by another script.
#
#
set bin_2dx = $1
set proc_2dx = $2
set SCRATCH_DISK = $3
set scriptname = $4
set newimagesidelength = $5
set tilenumber = $6
set olddir = $7
set from_dir = $8
#
cd ${olddir}
cd ..
cd TILES
set target_base = $PWD
cd ${olddir}
#
set tilenumberm1 = `echo ${tilenumber} | awk '{ s = $1 - 1 } END { print s }'`
if ( ${tilenumber} == "1" ) then
  set tileseries = "1"
endif
if ( ${tilenumber} == "3" ) then
  set tileseries = "1 2 3"
endif
if ( ${tilenumber} == "5" ) then
  set tileseries = "1 2 3 4 5"
endif
if ( ${tilenumber} == "7" ) then
  set tileseries = "1 2 3 4 5 6 7"
endif
if ( ${tilenumber} == "9" ) then
  set tileseries = "1 2 3 4 5 6 7 8 9"
endif
#
cd ${olddir}
#
echo ":The current working directory is" ${olddir}
echo ": "
#
  #
  cd ..
  cd ${from_dir}
  #
  if ( ! -w SCRATCH ) then
    \rm -rf SCRATCH
  endif

  source ${proc_2dx}/2dx_merge_makedirs 

  set imagename            = `cat 2dx_image.cfg | grep "set imagename ="            | cut -d\" -f2`
  set nonmaskimagename     = `cat 2dx_image.cfg | grep "set nonmaskimagename ="     | cut -d\" -f2`
  set imagenumber          = `cat 2dx_image.cfg | grep "set imagenumber ="          | cut -d\" -f2`
  set TLTAXIS              = `cat 2dx_image.cfg | grep "set TLTAXIS ="              | cut -d\" -f2`
  set TLTANG               = `cat 2dx_image.cfg | grep "set TLTANG ="               | cut -d\" -f2`
  set CS                   = `cat 2dx_image.cfg | grep "set CS ="                   | cut -d\" -f2`
  set KV                   = `cat 2dx_image.cfg | grep "set KV ="                   | cut -d\" -f2`
  set phacon               = `cat 2dx_image.cfg | grep "set phacon ="               | cut -d\" -f2`
  set magnification        = `cat 2dx_image.cfg | grep "set magnification ="        | cut -d\" -f2`
  set stepdigitizer        = `cat 2dx_image.cfg | grep "set stepdigitizer ="        | cut -d\" -f2`
  set defocus              = `cat 2dx_image.cfg | grep "set defocus ="              | cut -d\" -f2`
  set RESMIN               = `cat 2dx_image.cfg | grep "set RESMIN ="               | cut -d\" -f2`
  set RESMAX               = `cat 2dx_image.cfg | grep "set RESMAX ="               | cut -d\" -f2`
  set ctfcor_noise         = `cat 2dx_image.cfg | grep "set ctfcor_noise ="         | cut -d\" -f2`
  set ctfcor_debug         = `cat 2dx_image.cfg | grep "set ctfcor_debug ="         | cut -d\" -f2`
  set ctfcor_maxamp_factor = `cat 2dx_image.cfg | grep "set ctfcor_maxamp_factor =" | cut -d\" -f2`
  set realcell             = `cat 2dx_image.cfg | grep "set realcell ="             | cut -d\" -f2`
  set ALAT                 = `cat 2dx_image.cfg | grep "set ALAT ="                 | cut -d\" -f2`
  set realang              = `cat 2dx_image.cfg | grep "set realang ="              | cut -d\" -f2`
  set lattice              = `cat 2dx_image.cfg | grep "set lattice ="              | cut -d\" -f2`
  set phaori               = `cat 2dx_image.cfg | grep "set phaori ="               | cut -d\" -f2`
  set imagesidelength      = `cat 2dx_image.cfg | grep "set imagesidelength ="      | cut -d\" -f2`
  set sample_pixel         = `cat 2dx_image.cfg | grep "set sample_pixel ="         | cut -d\" -f2`
  set phacon               = `cat 2dx_image.cfg | grep "set phacon ="               | cut -d\" -f2`
  set defocus_res_min      = `cat 2dx_image.cfg | grep "set defocus_res_min ="      | cut -d\" -f2`
  set defocus_res_max      = `cat 2dx_image.cfg | grep "set defocus_res_max ="      | cut -d\" -f2`
  set use_paralellized     = `cat 2dx_image.cfg | grep "set use_paralellized ="     | cut -d\" -f2`

  set ampcon = `echo ${phacon} | awk '{s=sqrt(1.0 - ( $1 * $1 ))} END {print s}'`
  #
  # set imode                = 7
  # set imode                = 1
  # set imode                = 6
  set imode                = 1
  #
  set locimagenumber = ${imagenumber}
  #
  echo ": "
  echo ":      Working on TILE_${imagenumber}: ${from_dir}"
  #
  set unbent_image = unbent
  echo ":PWD = $PWD"
  ls -l ${unbent_image}.mrc
  #
  set realu      = `echo ${realcell} | sed 's/,/ /g' | awk '{ s = $1 } END { print s }'`
  set realv      = `echo ${realcell} | sed 's/,/ /g' | awk '{ s = $2 } END { print s }'`
  set lattice_u1 = `echo ${lattice} ${imagesidelength} ${newimagesidelength} | sed 's/,/ /g' | awk '{ s = $1 / $5 * $6 } END { print s }'`
  set lattice_u2 = `echo ${lattice} ${imagesidelength} ${newimagesidelength} | sed 's/,/ /g' | awk '{ s = $2 / $5 * $6 } END { print s }'`
  set lattice_v1 = `echo ${lattice} ${imagesidelength} ${newimagesidelength} | sed 's/,/ /g' | awk '{ s = $3 / $5 * $6 } END { print s }'`
  set lattice_v2 = `echo ${lattice} ${imagesidelength} ${newimagesidelength} | sed 's/,/ /g' | awk '{ s = $4 / $5 * $6 } END { print s }'`
  set newlattice = `echo ${lattice_u1},${lattice_u2},${lattice_v1},${lattice_v2}`
  echo ": "
  echo ":Old lattice = ${lattice}"
  echo ":New lattice = ${newlattice}"
  echo ": "
  #
  set inimage          = "unbent.mrc"
  if ( -e ${inimage} ) then
    set ctfcor_ctffile = "SCRATCH/2dx_ctfcor_ctffile.mrc"
    set ctfcor_outfile = "SCRATCH/image_ctf.mrc"
    set outimage       = "tile_ctf_taper.mrc"
    set outfft         = "SCRATCH/tile_ctf_taper_fft.mrc"
    set CTF_outfile_nA = image_ctfcor_fou_unbent_ctf.aph
    set image_type     = 1
    echo "# IMAGE: ${olddir}/../${from_dir}/${inimage} <Unbent U2 Image>" >>  ${olddir}/LOGS/${scriptname}.results
    source ${proc_2dx}/2dx_tile_sub_sub.com
  endif
  #
  set inimage          = "MA/direct_sum_filt_upscale.mrc"
  if ( -e ${inimage} ) then
    set ctfcor_ctffile = "SCRATCH/2dx_ctfcor_ctffile.mrc"
    set ctfcor_outfile = "SCRATCH/image_ctf.mrc"
    set outimage       = "tile_ctf_taper.mrc"
    set outfft         = "SCRATCH/tile_ctf_taper_fft.mrc"
    set CTF_outfile_nA = image_ctfcor_fou_unbent_ctf.aph
    set image_type     = 2
    echo "# IMAGE: ${olddir}/../${from_dir}/${inimage} <Unbent MA Image>" >>  ${olddir}/LOGS/${scriptname}.results
    source ${proc_2dx}/2dx_tile_sub_sub.com
  endif
  #
  set inimage          = "MB/direct_sum_filt_upscale.mrc"
  if ( -e ${inimage} ) then
    set ctfcor_ctffile = "SCRATCH/2dx_ctfcor_ctffile.mrc"
    set ctfcor_outfile = "SCRATCH/image_ctf.mrc"
    set outimage       = "tile_ctf_taper.mrc"
    set outfft         = "SCRATCH/tile_ctf_taper_fft.mrc"
    set CTF_outfile_nA = image_ctfcor_fou_unbent_ctf.aph
    set image_type     = 3
    echo "# IMAGE: ${olddir}/../${from_dir}/${inimage} <Unbent MB Image>" >>  ${olddir}/LOGS/${scriptname}.results
    source ${proc_2dx}/2dx_tile_sub_sub.com
  endif
  #
${proc_2dx}/lin "Done with ${from_dir}"
#
#
