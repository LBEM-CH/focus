#!/usr/bin/env python

import sparx as spx
import numpy as np
import sys
# import os

def main():

	par_in = sys.argv[1]
	par_out = sys.argv[2]
	sig_euler = sys.argv[3]
	sig_shift = sys.argv[4]
	pmaskstr = sys.argv[5]
	apix = sys.argv[6]

	sig_shift = sig_shift * apix # FREALIGN uses shifts in Angstroems but this is more intuitive to the user in pixels

	pmask = np.ones((5), dtype=int)

	# if os.path.exists('mparameters'):

	# 	f = open('mparameters','r')
	# 	for line in f.readlines():

	# 		if line.startswith('parameter_mask'):

	# 			pmaskstr = line.split('"')[1].split() # Get the refinement parameters mask from the mparameters file
	# 			break

	# 	f.close()

		for i in np.arange(5):

			pmask[i] = int(pmaskstr[i])

	par = np.loadtxt(par_in, comments='C')

	X = np.unique(par[:,7]) # List of unique crystals in the dataset

	for x in X: # Loop around the crystals

		sel = par[:,7] == x

		# print sum(sel)
		# If there's only representative for the crystal we skip it:
		if sum(sel) > 1:

			for i in np.arange(5):

				if pmask[i]:

					if i <= 2:

						rad = np.radians(par[sel,i+1])
						sinmat = np.sin(rad)
						cosmat = np.cos(rad)

						euler_mean = np.arctan2(np.mean(sinmat), np.mean(cosmat))

						if sig_euler <= 0.0:

							euler_std = np.arctan2(np.std(sinmat), np.std(cosmat))

						else:

							euler_std = sig_euler

						sqdiff = np.arctan2(np.sin(rad - euler_mean), np.cos(rad - euler_mean)) ** 2

						# print sqdiff.shape


						# Downweight the occupancy of the particles in the crystal according to the crystal statistics:

						par[sel,11] *= np.exp(-sqdiff / (2.0 * euler_std ** 2))

					else:

						shift_mean = np.mean(par[sel,i+1])

						if sig_shift <= 0.0:

							shift_std = np.std(par[sel,i+1])

						else:

							shift_std = sig_shift

						sqdiff = (par[sel,i+1] - shift_mean) ** 2
						par[sel,11] *= np.exp(-sqdiff / (2.0 * shift_std ** 2))

	np.savetxt(par_out, par, fmt=['%d', '%.2f', '%.2f', '%.2f', '%.2f', '%.2f', '%d', '%d', '%.2f', '%.2f', '%.2f', '%.2f', '%d', '%.4f', '%.2f', '%.2f'], delimiter='    ')

if __name__ == '__main__':
	main()