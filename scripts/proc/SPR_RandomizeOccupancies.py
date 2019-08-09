#!/usr/bin/env python

import sys
import numpy as np

def main():

	if len(sys.argv) <= 2:

		parfile = sys.argv[1]

		print( 'WARNING: %s will be overwritten with randomized occupancy values and all header information will be lost!' % parfile )

		par = np.loadtxt(parfile, comments='C')

		par[:,11] = np.random.random((par.shape[0])) * 100.0 # Generate a random class assignment for each particle in each class

		np.savetxt(parfile, par, fmt='    %d    %.2f    %.2f    %.2f    %.2f    %.2f    %d    %d    %.2f    %.2f    %.2f    %.2f    %d    %.4f    %.2f    %.2f')

		print( 'Done!' )

	else:

		parfiles = sorted(sys.argv[1:])

		par = np.loadtxt(parfiles[0], comments='C')

		K = len(parfiles)
		N = par.shape[0]

		occ = np.random.random((N,K)) # Generate a random class assignment for each particle in each class
		row_sums = occ.sum(axis=1)
		occ = occ / row_sums[:, np.newaxis]
		occ *= 100.0

		# For the first file:
		par[:,11] = occ[:,0]

		print( 'WARNING: %s will be overwritten with randomized occupancy values and all header information will be lost!' % parfiles[0] )
		np.savetxt(parfiles[0], par, fmt='    %d    %.2f    %.2f    %.2f    %.2f    %.2f    %d    %d    %.2f    %.2f    %.2f    %.2f    %d    %.4f    %.2f    %.2f')

		i = 1
		for p in parfiles[1:]:

			par = np.loadtxt(p, comments='C')
			par[:,11] = occ[:,i]
			print 'WARNING: %s will be overwritten with randomized occupancy values and all header information will be lost!' % p
			np.savetxt(p, par, fmt='    %d    %.2f    %.2f    %.2f    %.2f    %.2f    %d    %d    %.2f    %.2f    %.2f    %.2f    %d    %.4f    %.2f    %.2f')
			i += 1

if __name__ == '__main__':
	main()