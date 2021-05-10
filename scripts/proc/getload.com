#!/bin/tcsh -ef
#
#
ps -axf | sed -n '/2dx_tile_sub.com/p' | wc -l > SCRATCH/getload.dat
#
