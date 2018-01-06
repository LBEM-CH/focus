#!/usr/bin/env python
#############################################################################
#                                                                           #
# Title: Tile MRCS															#
#                                                                           #
# (C) 2dx.org, GNU Plublic License.                                         #
#                                                                           #
# Created..........: 23/10/2017                                             #
# Last Modification: 23/10/2017                                             #
# Author...........: Ricardo Righetto                                       #
#                                                                           #
#############################################################################
# This script will tile an .mrcs stack to generate a single MRC image
from mrcz import ioMRC
import sys
import copy
import numpy as np
# import matplotlib.pyplot as plt
# import matplotlib.cm as cmap

mrcs_in = sys.argv[1]
mrc_out = sys.argv[2]
tile_x = int( sys.argv[3] )
tile_y = int( sys.argv[4] )


mrcs = ioMRC.readMRC( mrcs_in )[0]

if tile_x * tile_y < mrcs.shape[0]:

	print( 'Tiling size must be at least equal to the number of particles in the stack! You specified: tile_x = %d, tile_y = %d' % ( tile_x, tile_y ) )
	sys.exit(1)

elif tile_x * tile_y >= mrcs.shape[0]:

	outmrc = np.zeros( ( tile_y*mrcs.shape[2], tile_x*mrcs.shape[1] ) ).astype( mrcs.dtype )
	k = 0
	for i in np.arange( tile_y ):
		for j in np.arange( tile_x ):

			if k < mrcs.shape[0]:

				outmrc[i*mrcs.shape[2]:(i+1)*mrcs.shape[2],j*mrcs.shape[1]:(j+1)*mrcs.shape[1]] = mrcs[k,:,:]
				k += 1

# print mrcs.shape
# print outmrc.shape

ioMRC.writeMRC( outmrc[::-1], mrc_out, dtype='float32' )
