#!/usr/bin/env python

import numpy as np
import sys
import os
import matplotlib.pyplot as plt
from optparse import OptionParser


def main():

	progname = os.path.basename(sys.argv[0])
	usage = progname + """ [options] <par file1> <par file2> ... <par fileN>

	Given one or more FREALIGN .par files, generates a new .par file for each of them, with modified occupancies according to some alignment statistics.
	For 2D crystals it is expected that all unit cells from a given crystal must be similarly aligned.
	Therefore, particles that deviate from the crystal statistics can be downweighted.

	Output:

		-Parameter file(s) with modified occupancies
		-Histogram plots in PNG format

	 """

	parser = OptionParser(usage)

	parser.add_option("--sigma_euler", metavar=1.0, type="float", help="The sigma (in degrees) for a Gaussian function to be used to weight the particle according to the Euler angles. If not specified, will estimate automatically for each 2D crystal.")
	
	parser.add_option("--sigma_shift", metavar=1.0, type="float", help="The sigma (in Angstroems or pixels) for a Gaussian function to be used to weight the particle according to the Euler angles. If not specified, will estimate automatically for each 2D crystal. If you specify this sigma in pixels, be sure to specify the option --angpix.")
	
	parser.add_option("--pmask", metavar='"1 1 1 1 1"', default="1 1 1 1 1", type="string", help="Five flags of 0 or 1 (e.g. 1 1 1 1 1). Determines which parameters are analyzed (PSI, THETA, PHI, SHX, SHY).")
	
	parser.add_option("--out", metavar="xweighted", type="string", default="xweighted", help="Output string to be appended to the .par file name.")

	parser.add_option("--plot_bins", metavar=120, type="int", help="Plot histograms for each parameter analyzed with this number of bins.")

	parser.add_option("--angpix", metavar=1.0, type="float", default=1.0, help="Pixel size in Angstroems")

	(options, args) = parser.parse_args()

	command = ' '.join(sys.argv)

	if options.out == None:

		options.out = 'xweighted'

	if options.sigma_euler != None and options.sigma_euler <= 0.0:

		print 'Sigma must be greater than zero!'
		sys.exit(1)

	elif options.sigma_euler != None:

		euler_std = np.radians(float(options.sigma_euler))

	if options.sigma_shift != None and options.sigma_shift <= 0.0:

		print 'Sigma must be greater than zero!'
		sys.exit(1)

	elif options.sigma_shift != None:

		shift_std = float(options.angpix * options.sigma_shift)

	if options.plot_bins != None and options.plot_bins <= 0.0:

		print 'Number of bins must be greater than zero!'
		sys.exit(1)


	parfiles = sorted(args)

	

	# if os.path.exists('mparameters'):

	# 	f = open('mparameters','r')
	# 	for line in f.readlines():

	# 		if line.startswith('parameter_mask'):

	# 			pmaskstr = line.split('"')[1].split() # Get the refinement parameters mask from the mparameters file
	# 			break

	# 	f.close()

	if options.pmask != None:

		pmaskstr = options.pmask.split()

		pmask = np.ones((5), dtype=int)
		for i in np.arange(5):

			pmask[i] = int(pmaskstr[i])

	pmaskname = ['PSI', 'THETA', 'PHI', 'SHX', 'SHY']


	# print pmask
	for p in parfiles:

		par = np.loadtxt(p, comments='C')

		X = np.unique(par[:,7]) # List of unique crystals in the dataset

		for x in X: # Loop around the crystals

			sel = par[:,7] == x

			N = sum(sel)
			# points = 1000
			# print sum(sel)
			# If there's only one representative for the crystal we skip it:
			if N > 1:

				for i in np.arange(5):

					if pmask[i]:

						if i <= 2:

							rad = np.radians(par[sel,i+1])
							sinmat = np.sin(rad)
							cosmat = np.cos(rad)

							ang = np.arctan2(sinmat, cosmat) # Normalize the angular range

							euler_mean = np.mean(ang)
							
							if options.sigma_euler == None:
								
								euler_std = np.std(ang)

							# Downweight the occupancy of the particles in the crystal according to the crystal statistics:

							sqdiff = (ang - euler_mean)**2

							par[sel,11] *= np.exp(-sqdiff / (2.0 * euler_std ** 2))

							if options.plot_bins != None:

								plt.figure()
								weights = np.ones_like(ang)/float(N)
								n, bins, patches = plt.hist(np.degrees(ang), options.plot_bins, weights=weights)
								# xpoints = np.linspace(min(np.degrees(ang)), max(np.degrees(ang)), points)
								y = np.exp(-(bins - np.degrees(euler_mean))**2 / (2.0 * np.degrees(euler_std) ** 2))
								# y = mlab.normpdf( bins, euler_mean * 180.0/np.pi, euler_std * 180.0/np.pi)
								plt.plot(bins, y/sum(y), 'r-')
								plt.title(pmaskname[i]+': N = %d, mean = %.2f, std = %.2f' % (N, np.degrees(euler_mean), np.degrees(euler_std)))
								plt.savefig(p[:-4]+'-'+options.out+'_crystal_%.3d_' % x + pmaskname[i] + '.png', dpi=300 )
								plt.close()

						else:

							shift_mean = np.mean(par[sel,i+1])

							if options.sigma_shift == None:

								shift_std = np.std(par[sel,i+1])


							sqdiff = (par[sel,i+1] - shift_mean) ** 2

							# print shift_mean, shift_std

							par[sel,11] *= np.exp(-sqdiff / (2.0 * shift_std ** 2))

							if options.plot_bins != None:

								plt.figure()
								weights = np.ones_like(ang)/float(N)
								n, bins, patches = plt.hist(par[sel,i+1], options.plot_bins, weights=weights)
								# xpoints = np.linspace(min(par[sel,i+1]), max(par[sel,i+1]), points)
								y = np.exp(-(bins - shift_mean)**2 / (2.0 * shift_std ** 2))
								# y = mlab.normpdf( bins, shift_mean, shift_std)
								plt.plot(bins, y/sum(y), 'r-')
								plt.title(pmaskname[i]+': N = %d, mean = %.2f, std = %.2f' % (N, shift_mean, shift_std))
								plt.savefig(p[:-4]+'-'+options.out+'_crystal_%.3d_' % x + pmaskname[i] + '.png', dpi=300 )
								plt.close()

		np.savetxt(p[:-4]+'-'+options.out+'.par', par, fmt=['%d', '%.2f', '%.2f', '%.2f', '%.2f', '%.2f', '%d', '%d', '%.2f', '%.2f', '%.2f', '%.2f', '%d', '%.4f', '%.2f', '%.2f'], delimiter='    ')

if __name__ == "__main__":
	main()