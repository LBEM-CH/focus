# Python utilities for CTF correction
# Author: Ricardo Righetto
# E-mail: ricardo.righetto@unibas.ch

import numpy as np
import focus_utilities
try:
	# SciPy FFT pack is faster than NumPy's:
	import scipy.fftpack as fft

except ImportError:

	import numpy.fft as fft

def CTF( imsize = [100, 100], DF1 = 1000.0, DF2 = None, AST = 0.0, WGH = 0.10, Cs = 2.7, kV = 300.0, apix = 1.0, B = 0.0 ):
# Generates 2D CTF function
# Underfocus is positive following conventions of FREALIGN and most of the packages out there (in Angstroms).
# B is B-factor

	if DF2 == None:

		DF2 = DF1

	# NOTATION BELOW IS INVERTED DUE TO NUMPY CONVENTION:
	df1 = DF2
	df2 = DF1
	ast = AST * np.pi / 180.0

	WL = ElectronWavelength( kV )

	w1 = np.sqrt( 1 - WGH*WGH )
	w2 = WGH

	rmesh,amesh = focus_utilities.RadialIndices( imsize, rounding=True )

	rmesh = rmesh / ( np.min( imsize ) * apix )

	ast = np.radians( ast )

	df = 0.5 * (df1 + df2 + (df1 - df2) * np.cos( 2 * (amesh - ast) ) )

	Xr = np.pi * WL * rmesh*rmesh * ( df - 1 / (2 * WL*WL * rmesh*rmesh * Cs) )
	Xr = np.nan_to_num( Xr )
	CTFim = -w1 * np.sin( Xr ) - w2 * np.cos( Xr )
	CTFim = CTFim * np.exp( -B * ( rmesh*rmesh ) / 4 )

	return CTFim

def ElectronWavelength( kV = 300.0 ):
# Returns electorn wavelength in Angstroms
	kV *= 1000.0 # ensure Kilovolts for below formula
	return 12.26 / np.sqrt( kV + 0.9785 * kV*kV / ( 10.0**6.0 ) )

def CorrectCTF( img, DF1 = 1000.0, DF2 = None, AST = 0.0, WGH = 0.10, invert = False, Cs = 2.7, kV = 300.0, apix = 1.0, B = 0.0, ctftype = 0, C = 1.0, return_ctf = False ):
# Applies CTF correction to image
# Type can be one of the following:
# 0 - Phase-flipping only
# 1 - CTF multiplication
# 2 - Wiener filtering with Wiener constant C
# By default image should have dark proteins and bright background, otherwise set invert=True.

	if invert:

		img *= -1.0

	imsize = img.shape

	CTFim = CTF( imsize, DF1, DF2, AST, WGH, Cs, kV, apix, B )

	FT = fft.fftshift( fft.fftn( img ) )

	if ctftype == 0: # Phase-flipping

		CTFcor = fft.ifftn( fft.ifftshift( FT * np.sign( CTFim ) ) )

	elif ctftype == 1: # CTF multiplication

		CTFcor = fft.ifftn( fft.ifftshift( FT * CTFim ) )

	elif ctftype == 2: # Wiener filtering

		if C <= 0.0:

			raise ValueError( "Error: Wiener filter constant cannot be less than or equal to zero! C = %f " % C )

		CTFcor = fft.ifftn( fft.ifftshift( FT * CTFim / ( CTFim*CTFim + C ) ) )

	else:

		raise ValueError( "Error: Type of CTF correction must be 0 (phase-flipping), 1 (CTF multiplication) or 2 (Wiener filtering). ctftype = %d " % ctftype )

	if return_ctf:

		return CTFcor, CTFim

	else:

		return CTFcor





