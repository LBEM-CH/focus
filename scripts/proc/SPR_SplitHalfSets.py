# !/usr/bin/env python
#############################################################################
#                                                                           #
# Title: Split particles in tow half-sets based on the 2D crystal they		#
# belong to.																#								
#                                                                           #
# (C) 2dx.org, GNU Plublic License.                                         #
#                                                                           #
# Created..........:24/02/2017                                            #
# Last Modification:24/02/2017                                              #
# Author...........:Ricardo Righetto                                       #
#                                                                           #
#############################################################################

import numpy as np
from mrcz import ioMRC
import sys
import os
import focus_utilities as util
# print ioMRC.__file__

def main():

	# BATCHSIZE=2048

	parfile = sys.argv[1]
	newparfile = sys.argv[2]
	mrcfile = sys.argv[3]
	newmrcfile = sys.argv[4]

	par = np.loadtxt(parfile, comments='C')

	U = np.unique( par[:,7] )

	oddU = U[0::2] # .par file convention starts at 1!
	evenU = U[1::2]


	oddpar = par[np.in1d( par[:,7], oddU )]
	evenpar = par[np.in1d( par[:,7], evenU )]
	
	# Sets may be unbalanced because we don't have control of how many particles each crystal has:
	Nmin = min( oddpar.shape[0], evenpar.shape[0] )
	Nmax = max( oddpar.shape[0], evenpar.shape[0] )
	diff = Nmax - Nmin # This gives how many particles we have to exclude in order to have balanced sets

	# So to avoid removing all "extra" particles from the same crystal, we remove 'diff' particles at random:
	idx = np.arange( Nmax )
	np.random.seed( seed=123 ) # Fix random seed to get reproducible results
	np.random.shuffle( idx )
	keep = sorted( idx[diff:] ) # These are the indices of the particles we want to keep

	if oddpar.shape[0] == Nmax:

		oddpar = oddpar[keep,:]

	else:

		evenpar = evenpar[keep,:]

	N = Nmin*2
	both = np.empty( ( N, par.shape[1] ), dtype=par.dtype )
	both[0::2,:] = oddpar
	both[1::2,:] = evenpar

	print 'Reordered %s into new dataset with %d particles evenly split.' % (parfile, N)
	print '%d particles needed to be excluded from the original dataset.' % diff
	print 'Now writing new MRC stack with reordered particles...'

	order = ( both[:,0] - 1 ).astype( 'int' )
	# sys.stdout = open(os.devnull, "w") # Suppress output

	j = 0
	for i in order:
		mrc = ioMRC.readMRC( mrcfile, idx = i )[0]
		ioMRC.writeMRC( mrc, newmrcfile, dtype='float32', idx = j )
		# print 'Wrote particle %d/%d...           \r' % (j+1, N),
		j += 1

	sys.stdout = sys.__stdout__

	print 'Done writing new MRC stack, now correcting indices in new .par file...'

	both[:,0] = np.reshape( np.arange( 1, N + 1 ), ( 1, N ) )

	np.savetxt( newparfile, both, fmt='    %d    %.2f    %.2f    %.2f    %.2f    %.2f    %d    %d    %.2f    %.2f    %.2f    %.2f    %d    %.4f    %.2f    %.2f' )
	# Let's also save a text file containing the indices of the excluded particles:
	np.savetxt( parfile+'.excluded.idx', idx[:diff], fmt='%d' )

	print 'Done!'

if __name__ == '__main__':
	main()