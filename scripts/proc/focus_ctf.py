# Python utilities for CTF correction
# Author: Ricardo Righetto
# E-mail: ricardo.righetto@unibas.ch

import numpy as np
import focus_utilities
import copy

def CTF( imsize = [100, 100], DF1 = 1000.0, DF2 = None, AST = 0.0, WGH = 0.10, Cs = 2.7, kV = 300.0, apix = 1.0, B = 0.0 ):
# Generates 2D CTF function
# Underfocus is positive following conventions of FREALIGN and most of the packages out there (in Angstroms).
# B is B-factor

	Cs *= 1e7 # Convert Cs to Angstroms

	if DF2 == None or np.isscalar( imsize ):

		DF2 = DF1

	# else:

	# 	# NOTATION FOR DEFOCUS1, DEFOCUS2, ASTIGMASTISM BELOW IS INVERTED DUE TO NUMPY CONVENTION:
	# 	DF1, DF2 = DF2, DF1

	AST *= -np.pi / 180.0

	WL = ElectronWavelength( kV )

	w1 = np.sqrt( 1 - WGH*WGH )
	w2 = WGH

	import warnings
	with warnings.catch_warnings():
		warnings.filterwarnings( "ignore", category=RuntimeWarning )

		if np.isscalar( imsize ):

			rmesh = np.fft.rfftfreq( imsize )
			amesh = 0.0

		else:

			# rmesh,amesh = focus_utilities.RadialIndices( imsize, RFFT=True )
			xmesh = np.fft.fftfreq( imsize[0] )
			ymesh = np.fft.rfftfreq( imsize[1] )

			xmeshtile = np.tile( xmesh, [len( ymesh ), 1] ).T
			ymeshtile = np.tile( ymesh, [len( xmesh ), 1] )

			rmesh = np.sqrt( xmeshtile*xmeshtile + ymeshtile*ymeshtile ) / apix

			amesh = np.nan_to_num( np.arctan2( ymeshtile, xmeshtile ) )

		# From Mindell & Grigorieff, JSB 2003:
		DF = 0.5 * (DF1 + DF2 + (DF1 - DF2) * np.cos( 2.0 * (amesh - AST) ) )

		Xr = np.nan_to_num( np.pi * WL * rmesh2 * ( DF - 1.0 / (2.0 * WL*WL * rmesh2 * Cs) ) )

	sinXr = np.sin( Xr )
	cosXr = np.cos( Xr )
	# CTFreal = w1 * sinXr - w2 * cosXr
	# CTFimag = -w1 * cosXr - w2 * sinXr

	# CTFim = CTFreal + CTFimag*1j
	CTFim = -w1 * sinXr - w2 * cosXr

	if B != 0.0: # Apply B-factor only if necessary:

		CTFim = CTFim * np.exp( -B * ( rmesh2 ) / 4 )

	return CTFim

def ElectronWavelength( kV = 300.0 ):
# Returns electorn wavelength in Angstroms
	# kV *= 1e3 # ensure Kilovolts for below formula
	# return 12.2639 / np.sqrt( kV + 0.97845 * kV*kV / ( 1e6 ) )
	return 12.2639 / np.sqrt( kV * 1e3 + 0.97845 * kV*kV )

def CorrectCTF( img, DF1 = 1000.0, DF2 = None, AST = 0.0, WGH = 0.10, invert_contrast = False, Cs = 2.7, kV = 300.0, apix = 1.0, B = 0.0, ctftype = 0, C = 1.0, return_ctf = False ):
# Applies CTF correction to image
# Type can be one of the following:
# 0 - Phase-flipping only
# 1 - CTF multiplication
# 2 - Wiener filtering with Wiener constant C
# By default will return image with same contrast as input, otherwise set invert_contrast=True.


	# Direct CTF correction would invert the image contrast. By default we don't do that, hence the negative sign:
	CTFim = -CTF( img.shape, DF1, DF2, AST, WGH, Cs, kV, apix, B )

	if invert_contrast:

		CTFim *= -1.0

	FT = np.fft.rfftn( img )

	if ctftype == 0: # Phase-flipping

		CTFcor = np.fft.irfftn( FT * np.sign( CTFim ) )

	elif ctftype == 1: # CTF multiplication

		CTFcor = np.fft.irfftn( FT * CTFim )

	elif ctftype == 2: # Wiener filtering

		if C <= 0.0:

			raise ValueError( "Error: Wiener filter constant cannot be less than or equal to zero! C = %f " % C )

		CTFcor = np.fft.irfftn( FT * CTFim / ( CTFim*CTFim + C ) )

	else:

		raise ValueError( "Error: Type of CTF correction must be 0 (phase-flipping), 1 (CTF multiplication) or 2 (Wiener filtering). ctftype = %d " % ctftype )

	# if return_half:

	# 	# AmpHalf = np.zeros( img.shape )
	# 	CTFnorm = CTFim**2
	# 	AmpHalf = np.abs( FT )**2
	# 	AmpHalf = AmpHalf - AmpHalf.mean() + CTFnorm.mean()
	# 	AmpHalf = AmpHalf*CTFnorm.std()/AmpHalf.std()
	# 	# CTFnorm -= CTFnorm.mean()
	# 	# CTFnorm /= CTFnorm.std()
	# 	AmpHalf[:,img.shape[1]/2:] = CTFnorm[:,img.shape[1]/2:]
	# 	# AmpHalf = AmpHalf**2

	# if return_ctf and return_half:

	# 	return CTFcor.real, CTFim, AmpHalf

	# elif return_ctf:

	# 	return CTFcor.real, CTFim

	# elif return_half:

	# 	return CTFcor.real, AmpHalf

	if return_ctf:

		return CTFcor, CTFim

	else:

		return CTFcor



