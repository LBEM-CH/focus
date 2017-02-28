#!/usr/bin/env python

#############################################################################
#                                                                           #
# FSC calculation and map postprocessing tools                 				#
#                                                                           #
# (C) 2dx.org, GNU Public License. 	                                        #
#                                                                           #
# Created..........: 09/09/2016                                             #
# Last Modification: 21/09/2016                                             #
# Author...........: Ricardo Righetto                                       #
# Author FCC...........: Robb McLeod                                        #
# 																			#
# This Python script uses the EMAN2 libraries:								#
# http://blake.bcm.edu/emanwiki/EMAN2 										#
# Reference: Tang et al., JSB 2007											#
#																			#
# It has also been inspired on the relion_postprocess program:				#
# http://www2.mrc-lmb.cam.ac.uk/relion 										#
# Reference: Scheres, JMB 2012												#
#																			#
# Single-Particle Wiener filter application is based on FREALIGN:			#
# http://grigoriefflab.janelia.org/frealign 								#
# References: 																#
# Grigorieff, Meth. Enzymol. 2016											#
# Sindelar & Grigorieff, JSB 2012											#
# 																			#
# High-Resolution Noise Substitution:										#
# Chen et al., Ultramicroscopy 2013											#
# 																			#
# Additional reference for FSC weighting and map sharpening: 				#
# Rosenthal & Henderson, JMB 2003											#
# 																			#
#############################################################################

import sys
import os.path
import numpy as np
import matplotlib.pyplot as plt
import EMAN2 as e2
import focus_utilities as util
# import ioMRC
from optparse import OptionParser


def main():

	progname = os.path.basename(sys.argv[0])
	usage = progname + """ <half-map1> <half-map2> [options] 

	Given two unmasked and unfiltered reconstructions from random halves of your dataset, calculates the FSC between them and applies additional postprocessing filters.

	Output:

			-FSC plot(s) in PNG format
			-Text file containing the curves' points
			-Masked and unmasked postprocessed map 

	"""

	parser = OptionParser(usage)

	parser.add_option("--out", metavar="postprocess", type="string", default='postprocess', help="Output rootname.")

	parser.add_option("--angpix", metavar=1.0, type="float", help="Pixel size in Angstroems")

	parser.add_option("--fsc_threshold", metavar=0.143, default=0.143, type="float", help="Display the resolution at which the FSC curve crosses this value.")

	parser.add_option("--lowpass", metavar='"auto"', default='auto', help="Resolution (in Angstroems) at which to low-pass filter the final map. A negative value will skip low-pass filtering. Default is to low-pass at resolution determined from FSC threshold.")

	parser.add_option("--mask", metavar="MyMask.mrc", type="string", help="A file containing the mask to be applied to the half-maps before calculating the FSC.")
	
	parser.add_option("--force_mask", action="store_true", help="Force using this mask even if it has strange properties such as values outside the range [0,1].", default=False)

	parser.add_option("--mw", metavar=1000.0, type="float", help="Molecular mass in kDa of particle or helical segment comprised within the mask. Needed to calculate volume-normalized Single-Particle Wiener filter (Sindelar & Grigorieff, JSB 2012). If not specified, will do conventional FSC weighting on the final map (Rosenthal & Henderson, JMB 2003).")

	parser.add_option("--mw_ignore", metavar=0.0, type="float", default=0.0, help="EXPERIMENTAL OPTION: Molecular mass in kDa present within the mask that needs to be ignored. Needed to calculate an adaptation of the volume-normalized Single-Particle Wiener filter (Sindelar & Grigorieff, JSB 2012). Only used if --mw is also specified. May be useful if your particles are extracted from 2D crystals.")

	parser.add_option("--skip_fsc_weighting", action="store_true", help="Do NOT apply FSC weighting (Rosenthal & Henderson, JMB 2003) to the final map, nor the Single-Particle Wiener filter (Sindelar & Grigorieff, JSB 2012).", default=False)

	parser.add_option("--tophat", action="store_true", help="Apply a top-hat low-pass filter instead of the default Gaussian low-pass filter to the final map.", default=False)

	parser.add_option("--mtf", type="string", help="File containing the detector MTF for sharpening of the final map.")

	parser.add_option("--randomize_below_fsc", metavar=0.8, type="float", help="If provided, will randomize phases for all Fourier shells beyond where the FSC drops below this value, to assess correlations introduced by the mask by High-Resolution Noise Substitution (Chen et al, Ultramicroscopy 2013). Be aware that this procedure may introduce a 'dip' in the FSC curves at the corresponding resolution value.")

	# parser.add_option("--evaluate_spw_random", action="store_true", default=False, help="If both --mw and --randomize_below_fsc are provided, will evalute the performance of the Single-Particle Wiener filter (Sindelar & Grigorieff, JSB 2012) on the phase-randomized maps. Useful to assess how much artifacts (e.g. ringing) are being amplified by this filter.")

	parser.add_option("--cone_aperture", metavar=90.0, type="float", help="Instead of FSC, calculate the Fourier Conical Correlation, within a cone with this aperture, in degrees.")

	parser.add_option("--refine_res_lim", metavar=10.0, type="float", help="Resolution limit in Angstroems used during the refinement, to be displayed on the FSC plots.")

	# parser.add_option("--three_sigma", action="store_true", help="Show the 3-Sigma criterion curve on the FSC plots (van Heel, Ultramicroscopy 1987).", default=False)

	parser.add_option("--dpi", metavar=300, type="int", default=300, help="Resolution of the PNG files to be saved with the FSC curves (dots per inch).")

	(options, args) = parser.parse_args()

	command = ' '.join(sys.argv)

	# Do some sanity checks:
	if len(sys.argv) < 3:

		print 'You must specify at least two map files to compute an FSC:'
		print usage
		sys.exit(1)

	if options.lowpass != 'auto':

		options.lowpass = float(options.lowpass)

	if options.mw != None and options.mw < 0.0:

		print 'Molecular mass cannot be negative!'
		sys.exit(1)

	if options.mw_ignore != None and options.mw_ignore < 0.0:

		print 'Molecular mass to be ignored cannot be negative!'
		sys.exit(1)

	if options.angpix == None:

		print '\nWARNING: Pixel size was not specified. Using 1.0 A/pixel.'
		options.angpix = 1.0 

	elif options.angpix <= 0.0:

		print 'Pixel size must be greater than zero!'
		sys.exit(1)

	if options.refine_res_lim != None and options.refine_res_lim <= 0.0:

		print 'Refinement resolution limit must be greater than zero!'
		sys.exit(1)

	if options.randomize_below_fsc != None and (options.randomize_below_fsc < -1.0 or options.randomize_below_fsc > 1.0):

		print 'FSC values for phase-randomization must be in the range [-1,1]!'
		sys.exit(1)

	if options.cone_aperture != None:

		options.cone_aperture = float(options.cone_aperture)/2
	

	# Read in the two maps:
	map1 = e2.EMData(args[0])
	map2 = e2.EMData(args[1])
	# map1 = ioMRC.readMRC( args[0] )[0]
	# map2 = ioMRC.readMRC( args[1] )[0]

	NSAM = map1.get_xsize()
	# NSAM = np.sqrt( np.sum( np.power( map1.shape, 2 ) ) ) / 2.0 / np.sqrt( 2 ) # For cubic volumes this is just half the box size.

	if NSAM != map2.get_xsize():
	# if np.any( map1.shape != map2.shape ):

		print 'Input maps must be the same size!'
		sys.exit(1)

	print '\nCalculating unmasked FSC...'

	if options.cone_aperture == None:

		f = map1.calc_fourier_shell_correlation(map2)
		l = len(f)/3
		fscmat = np.reshape(f, (3, l)).T

		freq = np.reshape(fscmat[:,0], (l, 1))
		fsc = np.reshape(fscmat[:,1], (l, 1))

	else:

		map1np = e2.EMNumPy.em2numpy( map1 )
		map2np = e2.EMNumPy.em2numpy( map2 )

		l = NSAM/2 + 1
		if np.mod(NSAM, 2):

			freq = np.arange( float(l) ) / (NSAM - 1)

		else:

			freq = np.arange( float(l) ) / NSAM


		freq = np.reshape(freq, (l, 1))

		fsc = util.FCC( map1np , map2np , [ options.cone_aperture ] )
		fsc = fsc[:l]
		fsc = np.reshape(fsc, (l, 1))

	if options.angpix != 1.0:

		freq = freq / options.angpix # Adjust resolution shells according to the voxel size

	# if options.three_sigma:

	# 	three_sigma_curve = 3.0 / np.sqrt( np.reshape(fscmat[:,2], (l, 1)) )

	dat = np.append(1/freq[1:], freq[1:],  axis=1) # Start creating the matrix that will be written to an output file
	head = 'Res\t1/Res\t' # Header of the output file describing the data columns

	res = ResolutionAtThreshold(freq[1:], fsc[1:], options.fsc_threshold)
	print 'FSC >= %.3f up to %.3f A (unmasked)' % (options.fsc_threshold, res)

	# Plot
	plt.figure()
	# if options.three_sigma:

	# 	plt.plot(freq[1:], fsc[1:], freq[1:], three_sigma_curve[1:])

	# else:

	# 	plt.plot(freq[1:], fsc[1:])
	plt.plot(freq[1:], fsc[1:])
	plt.title('Fourier Shell Correlation - unmasked')
	plt.ylabel('FSC')
	plt.xlabel('Spatial frequency (1/A)')
	plt.minorticks_on()
	ax = plt.gca()
	ax.set_yticks([options.fsc_threshold], minor=True)
	ax.set_yticklabels([str(options.fsc_threshold)], minor=True)
	if options.refine_res_lim != None:
		ax.axvline(1/options.refine_res_lim, linestyle='dashed', linewidth=0.75, color='m')
	plt.grid(b=True, which='both')
	plt.savefig(options.out+'_fsc-unmasked.png', dpi=options.dpi)
	plt.close()

	dat = np.append(dat, fsc[1:], axis=1) # Append the unmasked FSC
	head += 'FSC-unmasked\t'

	# Now we go to the mask-related operations which are activated if a mask or MW are specified. If only
	if options.mask != None or options.mw != None:

		if options.mask != None:

			mask = e2.EMData(options.mask)

		else: # If MW is specified but no mask, we create the largest possible sphere to be used as mask:

			print '\nYou specified MW but no mask. Using a big soft-edged sphere as mask. This may produce inaccurate results.'
			dummy = e2.EMData(NSAM,NSAM,NSAM) # Create an empty box
			dummy += 1.0 # Make it a full box

			mask = dummy.process('mask.soft',{'outer_radius' : -1}) # Create a soft-edged sphere
			# mask.write_image('mask.mrc')
		
			del dummy

		if options.force_mask == False:

			if (mask['minimum'] < 0.0 or mask['maximum'] > 1.0):

				print '\nMask values not in range [0,1]!'
				sys.exit(1)

		else:

			print '\nWARNING: You are forcing a mask that may have strange properties. Use at your own risk!!!'

		map1masked = map1 * mask
		map2masked = map2 * mask

		print '\nCalculating masked FSC...'

		if options.cone_aperture == None:

			fmasked = map1masked.calc_fourier_shell_correlation(map2masked)

			fscmatmasked = np.reshape(fmasked,(3,len(fmasked)/3)).T
			fsc_mask = np.reshape(fscmatmasked[:,1], (l, 1))

		else:

			map1maskednp = e2.EMNumPy.em2numpy( map1masked )
			map2maskednp = e2.EMNumPy.em2numpy( map2masked )

			fsc_mask = util.FCC( map1maskednp , map2maskednp , [ options.cone_aperture ] )
			fsc_mask = fsc_mask[:l]
			fsc_mask = np.reshape(fsc_mask, (l, 1))

		res_mask = ResolutionAtThreshold(freq[1:], fsc_mask[1:], options.fsc_threshold)
		print 'FSC >= %.3f up to %.3f A (masked)' % (options.fsc_threshold, res_mask)

		dat = np.append(dat, fsc_mask[1:], axis=1) # Append the masked FSC
		head += 'FSC-masked\t'

		if options.randomize_below_fsc == None:

			# Plot
			plt.figure()
			plt.plot(freq[1:], fsc_mask[1:])
			plt.title('Fourier Shell Correlation - masked')
			plt.ylabel('FSC')
			plt.xlabel('Spatial frequency (1/A)')
			plt.minorticks_on()
			ax = plt.gca()
			ax.set_yticks([options.fsc_threshold], minor=True)
			ax.set_yticklabels([str(options.fsc_threshold)], minor=True)
			if options.refine_res_lim != None:
				ax.axvline(1/options.refine_res_lim, linestyle='dashed', linewidth=0.75, color='m')
			plt.grid(b=True, which='both')
			plt.savefig(options.out+'_fsc-masked.png', dpi=options.dpi)
			plt.close()

		else:

			rand_res = ResolutionAtThreshold(freq[1:], fsc[1:], options.randomize_below_fsc)
			print '\nRandomizing phases beyond %.2f A...\n' % rand_res

			rand_freq = 1/float(rand_res)

			map1randphase = map1.process('filter.lowpass.randomphase', {'apix' : options.angpix, 'cutoff_freq' : rand_freq})
			map2randphase = map2.process('filter.lowpass.randomphase', {'apix' : options.angpix, 'cutoff_freq' : rand_freq})

			# We mask the phase-randomized maps:
			map1randphasemasked = map1randphase * mask
			map2randphasemasked = map2randphase * mask

			print '\nCalculating masked FSC for phase-randomized maps...'

			if options.cone_aperture == None:

				fmasked_rnd = map1randphasemasked.calc_fourier_shell_correlation(map2randphasemasked)

				fscmatmaskedrnd = np.reshape(fmasked_rnd, (3, l)).T
				fsc_mask_rnd = np.reshape(fscmatmaskedrnd[:,1], (l, 1))

			else:

				map1randphasemaskednp = e2.EMNumPy.em2numpy( map1randphasemasked )
				map2randphasemaskednp = e2.EMNumPy.em2numpy( map2randphasemasked )

				fsc_mask_rnd = util.FCC( map1randphasemaskednp , map2randphasemaskednp , [ options.cone_aperture ] )
				fsc_mask_rnd = fsc_mask_rnd[:l]
				fsc_mask_rnd = np.reshape(fsc_mask_rnd, (l, 1))

			# We compute FSCtrue following (Chen et al, Ultramicroscopy 2013). For masked maps this will correct the FSC for eventual refinement overfitting, including from the mask:
			fsc_mask_true = np.zeros(fsc.shape) # Create new array
			fsc_mask_true[:] = fsc_mask

			fsc_mask_true[freq >= rand_freq] = (fsc_mask[freq >= rand_freq] - fsc_mask_rnd[freq >= rand_freq]) / (1 - fsc_mask_rnd[freq >= rand_freq])

			res_mask_true = ResolutionAtThreshold(freq[1:], fsc_mask_true[1:], options.fsc_threshold)
			print 'FSC >= %.3f up to %.3f A (masked - true)' % (options.fsc_threshold, res_mask_true)

			dat = np.append(dat, fsc_mask_true[1:], axis=1) # Append the true masked FSC
			head += 'FSC-masked_true\t'

			# Plot
			plt.figure()
			plt.plot(freq[1:], fsc_mask[1:], freq[1:], fsc_mask_rnd[1:], freq[1:], fsc_mask_true[1:])
			plt.title('Fourier Shell Correlation - masked')
			plt.ylabel('FSC')
			plt.xlabel('Spatial frequency (1/A)')
			plt.legend(['FSC', 'FSC - phase randomized', 'FSC - true'])
			plt.minorticks_on()
			ax = plt.gca()
			ax.set_yticks([options.fsc_threshold], minor=True)
			ax.set_yticklabels([str(options.fsc_threshold)], minor=True)
			if options.refine_res_lim != None:
				ax.axvline(1/options.refine_res_lim, linestyle='dashed', linewidth=0.75, color='m')
			plt.grid(b=True, which='both')
			plt.savefig(options.out+'_fsc-masked_true.png', dpi=options.dpi)
			plt.close()

			res_mask = res_mask_true

		if options.mw == None and options.randomize_below_fsc == None:

			# Plot
			plt.figure()
			plt.plot(freq[1:], fsc[1:], freq[1:], fsc_mask[1:])
			plt.title('Fourier Shell Correlation')
			plt.ylabel('FSC')
			plt.xlabel('Spatial frequency (1/A)')
			plt.legend(['unmasked', 'masked'])
			plt.minorticks_on()
			ax = plt.gca()
			ax.set_yticks([options.fsc_threshold], minor=True)
			ax.set_yticklabels([str(options.fsc_threshold)], minor=True)
			if options.refine_res_lim != None:
				ax.axvline(1/options.refine_res_lim, linestyle='dashed', linewidth=0.75, color='m')
			plt.grid(b=True, which='both')
			plt.savefig(options.out+'_fsc.png', dpi=options.dpi)
			plt.close()

		elif options.mw == None and options.randomize_below_fsc != None:

			# Plot
			plt.figure()
			plt.plot(freq[1:], fsc[1:], freq[1:], fsc_mask_true[1:])
			plt.title('Fourier Shell Correlation')
			plt.ylabel('FSC')
			plt.xlabel('Spatial frequency (1/A)')
			plt.legend(['unmasked', 'masked - true'])
			plt.minorticks_on()
			ax = plt.gca()
			ax.set_yticks([options.fsc_threshold], minor=True)
			ax.set_yticklabels([str(options.fsc_threshold)], minor=True)
			if options.refine_res_lim != None:
				ax.axvline(1/options.refine_res_lim, linestyle='dashed', linewidth=0.75, color='m')
			plt.grid(b=True, which='both')
			plt.savefig(options.out+'_fsc.png', dpi=options.dpi)
			plt.close()


		if options.mw != None:

			DALT = 0.81 # Da/A^3

			# Estimate fraction of volume occupied by the molecule:
			fpart = 1000.0 * options.mw / DALT / (options.angpix * NSAM)**3

			fignore = 1000.0 * options.mw_ignore / DALT / (options.angpix * NSAM)**3

			# Fraction of the volume occupied by the mask:
			maskvoxsum = np.sum(mask.get_data_as_vector())
			fmask = maskvoxsum / NSAM**3

			print '\nCalculating Single-Particle Wiener filter...'
			print '\nFraction of particle within the volume (Fpart): %.6f' % fpart
			print 'Fraction of mask within the volume (Fmask): %.6f' % fmask
			if options.mw_ignore > 0.0:

				print 'Fraction of densities to be ignored within the volume (Fignore): %.6f' % fignore
				print 'Fpart/(Fmask-Fignore) ratio: %.6f' % (fpart/(fmask-fignore))

				if (fpart/(fmask-fignore)) >= 1.0:

					print '\nWARNING: Your particle occupies a volume bigger than the mask. Mask is probably too tight or even too small!'

			else:

				print 'Fpart/Fmask ratio: %.6f' % (fpart/fmask)

				if (fpart/fmask) >= 1.0:

					print '\nWARNING: Your particle occupies a volume bigger than the mask. Mask is probably too tight or even too small!'

			# We now compute the volume-normalized FSC or Single-Particle filter (see Sindelar & Grigorieff, JSB 2012):

			# 	# # With overfitting correction:
			# 	# fsc_spw_true = fsc_mask_true / (fsc_mask_true + (fpart / (fmask - fignore)) * (1.0 - fsc_mask_true))

			# 	# Without overfitting correction:
			# 	fsc_spw_mask = fsc_mask / (fsc_mask + (fpart / (fmask - fignore)) * (1.0 - fsc_mask))

			# 	# SPW filter on the phase-randomized masked FSC:
			# 	fsc_spw_rnd = fsc_mask_rnd / (fsc_mask_rnd + (fpart / (fmask - fignore)) * (1.0 - fsc_mask_rnd))

			# 	# Applying FSCtrue to account for effects of masking and the SPW filter itself:
			# 	fsc_spw_true_mask = np.zeros(fsc.shape) # Create new array
			# 	fsc_spw_true_mask[:] = fsc_spw_mask
			# 	fsc_spw_true_mask[freq >= rand_freq] = (fsc_spw_mask[freq >= rand_freq] - fsc_spw_rnd[freq >= rand_freq]) / (1 - fsc_spw_rnd[freq >= rand_freq])

			# 	fsc_spw_true = np.zeros(fsc.shape) # Create new array
			# 	fsc_spw_true[:] = fsc_spw_mask
			# 	fsc_spw_true[freq >= rand_freq] = (fsc_spw_mask[freq >= rand_freq] - fsc_rnd_spw[freq >= rand_freq]) / (1 - fsc_rnd_spw[freq >= rand_freq])

			# 	# res_spw_true = ResolutionAtThreshold(freq[1:], fsc_spw_true[1:], options.fsc_threshold)
			# 	# print '\nFSC >= %.3f up to %.3f A (volume-normalized)' % (options.fsc_threshold, res_spw_true)

			# 	# dat = np.append(dat, fsc_spw_mask[1:], axis=1) # Append the FSC-SPW
			# 	# head += 'FSC-SPW\t'
			# 	# dat = np.append(dat, fsc_spw_true[1:], axis=1) # Append the true FSC-SPW
			# 	# head += 'FSC-SPW_true\t'

			# 	# Plot
			# 	plt.figure()
			# 	plt.plot(freq[1:], fsc_spw_mask[1:], freq[1:], fsc_rnd_spw[1:], freq[1:], fsc_spw_true[1:])
			# 	plt.title('Fourier Shell Correlation - Single-Particle Wiener filter')
			# 	plt.ylabel('FSC')
			# 	plt.xlabel('Spatial frequency (1/A)')
			# 	plt.legend(['FSC', 'FSC - phase randomized', 'FSC - true'])
			# 	plt.minorticks_on()
			# 	ax = plt.gca()
			# 	ax.set_yticks([options.fsc_threshold], minor=True)
			# 	ax.set_yticklabels([str(options.fsc_threshold)], minor=True)
			# 	if options.refine_res_lim != None:
			# 		ax.axvline(1/options.refine_res_lim, linestyle='dashed', linewidth=0.75, color='m')
			# 	plt.grid(b=True, which='both')
			# 	plt.savefig(options.out+'_fsc-spw_true.png', dpi=options.dpi)
			# 	plt.close()

			# 	# Plot
			# 	plt.figure()
			# 	plt.plot(freq[1:], fsc_spw_mask[1:], freq[1:], fsc_spw_rnd[1:], freq[1:], fsc_spw_true_mask[1:])
			# 	plt.title('Fourier Shell Correlation - Single-Particle Wiener filter')
			# 	plt.ylabel('FSC')
			# 	plt.xlabel('Spatial frequency (1/A)')
			# 	plt.legend(['FSC', 'FSC - phase randomized, mask', 'FSC - true, mask'])
			# 	plt.minorticks_on()
			# 	ax = plt.gca()
			# 	ax.set_yticks([options.fsc_threshold], minor=True)
			# 	ax.set_yticklabels([str(options.fsc_threshold)], minor=True)
			# 	if options.refine_res_lim != None:
			# 		ax.axvline(1/options.refine_res_lim, linestyle='dashed', linewidth=0.75, color='m')
			# 	plt.grid(b=True, which='both')
			# 	plt.savefig(options.out+'_fsc-spw_true-mask.png', dpi=options.dpi)
			# 	plt.close()

			# # 	# Plot
			# # 	plt.figure()
			# # 	plt.plot(freq[1:], fsc[1:], freq[1:], fsc_mask_true[1:], freq[1:], fsc_spw_true[1:])
			# # 	plt.title('Fourier Shell Correlation')
			# # 	plt.ylabel('FSC')
			# # 	plt.xlabel('Spatial frequency (1/A)')
			# # 	plt.legend(['unmasked', 'masked', 'SPW'])
			# # 	plt.minorticks_on()
			# # 	ax = plt.gca()
			# # 	ax.set_yticks([options.fsc_threshold], minor=True)
			# # 	ax.set_yticklabels([str(options.fsc_threshold)], minor=True)
			# # 	if options.refine_res_lim != None:
			# # 		ax.axvline(1/options.refine_res_lim, linestyle='dashed', linewidth=0.75, color='m')
			# # 	plt.grid(b=True, which='both')
			# # 	plt.savefig(options.out+'_fsc.png', dpi=options.dpi)
			# # 	plt.close()

			# # 	res_spw = res_spw_true

			# # else:

			# if options.evaluate_spw_random and options.mw != None and options.randomize_below_fsc != None:

			# 	# Let's evaluate the performance of the SPW filter on random noise:
			# 	f_rnd = map1randphase.calc_fourier_shell_correlation(map2randphase)
			# 	fscmatrnd = np.reshape(f_rnd, (3, l)).T
			# 	fsc_rnd = np.reshape(fscmatrnd[:,1], (l, 1))

			# 	fsc_rnd_spw = fsc_rnd / (fsc_rnd + (fpart / (fmask - fignore)) * (1.0 - fsc_rnd))

			# 	# Plot
			# 	plt.figure()
			# 	plt.plot(freq[1:], fsc_rnd[1:], freq[1:], fsc_rnd_spw[1:])
			# 	plt.title('Fourier Shell Correlation - Single-Particle Wiener filter')
			# 	plt.ylabel('FSC')
			# 	plt.xlabel('Spatial frequency (1/A)')
			# 	plt.legend(['FSC - phase randomized', 'SPW - phase randomized'])
			# 	plt.minorticks_on()
			# 	ax = plt.gca()
			# 	ax.set_yticks([options.fsc_threshold], minor=True)
			# 	ax.set_yticklabels([str(options.fsc_threshold)], minor=True)
			# 	if options.refine_res_lim != None:
			# 		ax.axvline(1/options.refine_res_lim, linestyle='dashed', linewidth=0.75, color='m')
			# 	plt.grid(b=True, which='both')
			# 	plt.savefig(options.out+'_fsc-spw-random.png', dpi=options.dpi)
			# 	plt.close()


			# Let's do Single-Particle Wiener filtering following (Sindelar & Grigorieff, 2012):
			fsc_spw = fsc_mask / (fsc_mask + (fpart / (fmask - fignore)) * (1.0 - fsc_mask))

			res_spw = ResolutionAtThreshold(freq[1:], fsc_spw[1:], options.fsc_threshold)
			print '\nFSC >= %.3f up to %.3f A (volume-normalized)' % (options.fsc_threshold, res_spw)

			dat = np.append(dat, fsc_spw[1:], axis=1) # Append the FSC-SPW
			head += 'FSC-SPW\t'

			# Plot
			plt.figure()
			plt.plot(freq[1:], fsc_spw[1:])
			plt.title('Fourier Shell Correlation - Single-Particle Wiener filter')
			plt.ylabel('FSC')
			plt.xlabel('Spatial frequency (1/A)')
			plt.minorticks_on()
			ax = plt.gca()
			ax.set_yticks([options.fsc_threshold], minor=True)
			ax.set_yticklabels([str(options.fsc_threshold)], minor=True)
			if options.refine_res_lim != None:
				ax.axvline(1/options.refine_res_lim, linestyle='dashed', linewidth=0.75, color='m')
			plt.grid(b=True, which='both')
			plt.savefig(options.out+'_fsc-spw.png', dpi=options.dpi)
			plt.close()

			if options.randomize_below_fsc != None:

				# Plot
				plt.figure()
				plt.plot(freq[1:], fsc[1:], freq[1:], fsc_mask[1:], freq[1:], fsc_spw[1:], freq[1:], fsc_mask_true[1:])
				plt.title('Fourier Shell Correlation')
				plt.ylabel('FSC')
				plt.xlabel('Spatial frequency (1/A)')
				plt.legend(['unmasked', 'masked', 'masked - SPW', 'masked - true'])
				plt.minorticks_on()
				ax = plt.gca()
				ax.set_yticks([options.fsc_threshold], minor=True)
				ax.set_yticklabels([str(options.fsc_threshold)], minor=True)
				if options.refine_res_lim != None:
					ax.axvline(1/options.refine_res_lim, linestyle='dashed', linewidth=0.75, color='m')
				plt.grid(b=True, which='both')
				plt.savefig(options.out+'_fsc.png', dpi=options.dpi)
				plt.close()

			else:

				# Plot
				plt.figure()
				plt.plot(freq[1:], fsc[1:], freq[1:], fsc_mask[1:], freq[1:], fsc_spw[1:])
				plt.title('Fourier Shell Correlation')
				plt.ylabel('FSC')
				plt.xlabel('Spatial frequency (1/A)')
				plt.legend(['unmasked', 'masked', 'masked - SPW'])
				plt.minorticks_on()
				ax = plt.gca()
				ax.set_yticks([options.fsc_threshold], minor=True)
				ax.set_yticklabels([str(options.fsc_threshold)], minor=True)
				if options.refine_res_lim != None:
					ax.axvline(1/options.refine_res_lim, linestyle='dashed', linewidth=0.75, color='m')
				plt.grid(b=True, which='both')
				plt.savefig(options.out+'_fsc.png', dpi=options.dpi)
				plt.close()

#### MAP FILTERING STEPS:

	# 1. Sum the two half-reconstructions:
	print '\nAveraging the two half-maps...'
	fullmap = map1 + map2

	# 2. Sharpen map by recovering amplitudes from detector's MTF:
	if options.mtf != None:

		print 'Dividing map by the detector MTF...'

		try:

			mtf = np.loadtxt(options.mtf)

			ignore_mtf = False

		except ValueError:

			if options.mtf[-5:] == '.star':

				mtf = np.loadtxt(options.mtf, skiprows=4)

				ignore_mtf = False

			else:

				print 'Could not read MTF file! Ignoring MTF...'
				ignore_mtf = True

		if ignore_mtf == False:

			# We need to know the MTF values at the Fourier bins of our map. So we interpolate from the MTF description available:
			interp_mtf = np.interp(fscmat[:,0], mtf[:,0], mtf[:,1])

			# Divide Fourier components by the detector MTF:
			inv_mtf = 1/interp_mtf
			# np.savetxt('interp_mtf.txt', [fscmat[:,0], interp_mtf, inv_mtf], fmt='%.6f')
			fullmap = fullmap.process('filter.radialtable', {'table' : inv_mtf.tolist()})

	# 3. Apply FSC weighting or SPW filter to the final map, accordingly:
	if options.skip_fsc_weighting == False:

		print 'Applying FSC weighting to the map...'
		if options.mask == None and options.mw == None:
			
			# Derive weights from unmasked FSC
			fsc_weights = np.sqrt(2 * np.abs(fsc) / (1 + np.abs(fsc)))

		elif options.mw == None:

			# Derive weights from masked FSC
			if options.randomize_below_fsc != None:

				fsc_weights = np.sqrt(2 * np.abs(fsc_mask_true) / (1 + np.abs(fsc_mask_true)))

			else:

				fsc_weights = np.sqrt(2 * np.abs(fsc_mask) / (1 + np.abs(fsc_mask)))

		else:

			fsc_weights = np.sqrt(2 * np.abs(fsc_spw) / (1 + np.abs(fsc_spw)))
			

		fullmap = fullmap.process('filter.radialtable', {'table' : fsc_weights.ravel().tolist()})

		dat = np.append(dat, fsc_weights[1:], axis=1) # Append the FSC weighting
		head += 'FourierWeights\t'

	# 4. Impose a Gaussian low-pass filter with cutoff at given resolution, or resolution determined from FSC threshold:
	if options.lowpass == 'auto':
		print 'Low-pass filtering the map at resolution cutoff...'
		if options.mw != None:

			res_cutoff = 1/res_spw

		elif options.mask != None:

			res_cutoff = 1/res_mask

		else:

			res_cutoff = 1/res
	
		if options.tophat == False: 

			fullmap = fullmap.process('filter.lowpass.gauss', {'apix' : options.angpix, 'cutoff_freq' : res_cutoff[0]})

		else:

			fullmap = fullmap.process('filter.lowpass.tophat', {'apix' : options.angpix, 'cutoff_freq' : res_cutoff[0]})

	elif options.lowpass >= 0.0:
		print 'Low-pass filtering the map at resolution cutoff...'
		res_cutoff = 1/options.lowpass

		if options.tophat == False: 

			fullmap = fullmap.process('filter.lowpass.gauss', {'apix' : options.angpix, 'cutoff_freq' : res_cutoff})

		else:

			fullmap = fullmap.process('filter.lowpass.tophat', {'apix' : options.angpix, 'cutoff_freq' : res_cutoff})

	# 5. Apply mask, if provided:
	if options.mask != None or options.mw != None:
		print 'Masking the map...'
		masked = fullmap * mask
		masked.write_image(options.out+'-masked.mrc')

	# Write filtered, unmasked map
	fullmap.write_image(options.out+'-unmasked.mrc')

	# Save output file with all relevant FSC data
	np.savetxt(options.out+'_data.fsc', np.matrix(dat), header=command+'\n'+head, delimiter='\t', fmt='%.6f')

	print '\nDone!'


def ResolutionAtThreshold(freq, fsc, thr):
# Do a simple linear interpolation to get resolution value at the specified FSC threshold
# (DEPRECATED, RETURN THE RESOLUTION AT WHICH FSC IS STILL HIGHER THAN THRESHOLD)

	i = 0
	for f in fsc:

		if f <= thr:

			break

		i += 1

	if i < len(fsc):

		y1 = fsc[i]
		y0 = fsc[i-1]
		x1 = freq[i]
		x0 = freq[i-1]

		delta = (y1-y0)/(x1-x0)

		# res_freq = x0 + (thr - y0) / delta
		
		# Just return the highest resolution bin at which FSC is still higher than threshold:
		res_freq = x0

	else:

		print '\nFSC NEVER DROPS BELOW %.3f THRESHOLD. THERE IS SOMETHING WRONG!!!\n' % thr
		res_freq = freq[-1]


	return 1/res_freq


if __name__ == "__main__":
	main()