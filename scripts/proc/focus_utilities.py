# Python utilities for Focus
# Author: Ricardo Righetto
# E-mail: ricardo.righetto@unibas.ch

import numpy as np

def RadialIndices( imsize = [100, 100], rounding=True, normalize=False ):
# Returns radius and angles for each pixel (or voxel) in a 2D image or 3D volume of shape = imsize
# For 2D returns the angle with the horizontal x- axis
# For 3D returns the angle with the horizontal x,y plane
# If imsize is a scalar, will default to 2D.
# Rounding is to ensure "perfect" radial symmetry, desirable in some applications.
# Normalize=True will normalize the radius to values between 0.0 and 1.0.
# Note: This function is compliant with SciPy/NumPy fftfreq(), however it is not optimized for RFFT operations. 

	if np.isscalar(imsize):

		imsize = [imsize, imsize]

	if len( imsize ) > 3:

		raise ValueError( "Object should not have dimensions larger than 3: len(imsize) = %d " % len(imsize))

	import warnings
	with warnings.catch_warnings():
		warnings.filterwarnings( "ignore", category=RuntimeWarning )

		m = np.mod(imsize, 2) # Check if dimensions are odd or even

		if len(imsize) == 2:

			# [xmesh, ymesh] = np.mgrid[-imsize[0]/2:imsize[0]/2, -imsize[1]/2:imsize[1]/2]
			# The definition below is consistent with scipy/numpy np.fft.fftfreq: (?)
			[xmesh, ymesh] = np.mgrid[-imsize[0]//2+m[0]:(imsize[0]-1)//2+1, -imsize[1]//2+m[1]:(imsize[1]-1)//2+1]

			rmesh = np.sqrt( xmesh*xmesh + ymesh*ymesh )
			
			amesh = np.arctan2( ymesh, xmesh )

		else:

			# [xmesh, ymesh, zmesh] = np.mgrid[-imsize[0]/2:imsize[0]/2, -imsize[1]/2:imsize[1]/2, -imsize[2]/2:imsize[2]/2]
			# The definition below is consistent with scipy/numpy np.fft.fftfreq:
			[xmesh, ymesh, zmesh] = np.mgrid[-imsize[0]//2+m[0]:(imsize[0]-1)//2+1, -imsize[1]//2+m[1]:(imsize[1]-1)//2+1, -imsize[2]//2+m[2]:(imsize[2]-1)//2+1]

			rmesh = np.sqrt( xmesh*xmesh + ymesh*ymesh + zmesh*zmesh )

			amesh = np.arccos( zmesh / rmesh )

	if rounding:

		rmesh = np.round( rmesh ).astype('int')

	if normalize:

		rmesh = rmesh / np.sqrt( np.sum( np.power( imsize, 2 ) ) ) / 2.0 / np.sqrt(2)

	return rmesh, np.nan_to_num( amesh )

def RotationalAverage( img ):
# Compute the rotational average of a 2D image or 3D volume

	rmesh = RadialIndices( img.shape, rounding=True )[0]

	rotavg = np.zeros( img.shape )

	for r in np.unique( rmesh ):

		idx = rmesh == r
		rotavg[idx] = img[idx].mean()

	return rotavg

def SoftMask( imsize = [100, 100], radius = 0.5, width = 6.0 ):
# Generates a circular or spherical mask with a soft cosine edge

	if np.isscalar(imsize):

		imsize = [imsize, imsize]

	if len(imsize) > 3:

		raise ValueError ( "Object should not have dimensions larger than 3: len(imsize) = %d " % len(imsize))

	if width < 0.0:

		width = 0.0

	if width > 0.0 and width <= 1.0:

		width = 0.5 * width * np.min( imsize )

	if radius < 0.0 or np.any( imsize < radius):

		radius = np.min( imsize ) - float(width)/2

	if radius > 0.0 and radius <= 1.0:

		radius = radius * np.min( imsize )

	radius *= 0.5

	rii = radius + width/2
	rih = radius - width/2

	rmesh = RadialIndices( imsize, rounding=True )[0]

	mask = np.zeros( imsize )

	fill_idx = rmesh < rih
	mask[fill_idx] = 1.0

	rih_idx = rmesh >= rih
	rii_idx = rmesh <= rii
	edge_idx = rih_idx * rii_idx

	mask[edge_idx] = ( 1.0 + np.cos( np.pi * ( rmesh[edge_idx] - rih ) / width ) ) / 2.0

	return mask

def FilterGauss( img, apix=1.0, lp=-1, hp=-1, return_filter=False ):
# Gaussian band-pass filtering of images.

	rmesh = RadialIndices( img.shape, normalize=True )[0] / apix
	rmesh2 = rmesh*rmesh

	if lp <= 0.0:

		lowpass = 1.0

	else:

		lowpass = np.exp( - lp ** 2 * rmesh2 / 2 )

	if hp <= 0.0:

		highpass = 1.0

	else:

		highpass = 1.0 - np.exp( - hp ** 2 * rmesh2 / 2 )

	bandpass = lowpass * highpass

	ft = np.fft.fftshift( np.fft.fftn( img ) )

	filtered = np.fft.ifftn( np.fft.ifftshift( ft * bandpass ) )

	if return_filter:

		return filtered.real, bandpass

	else:

		return filtered.real

def FilterBfactor( img, apix=1.0, B=0.0, return_filter=False ):
# Applies a B-factor to images. B can be positive or negative.

	rmesh = RadialIndices( img.shape, normalize=True )[0] / apix
	rmesh2 = rmesh*rmesh

	bfac = np.exp( - (B * rmesh2  ) /  4  )

	ft = np.fft.fftshift( np.fft.fftn( img ) )

	filtered = np.fft.ifftn( np.fft.ifftshift( ft * bfac ) )

	if return_filter:

		return filtered.real, bfac

	else:

		return filtered.real

def FilterCosine( img, apix=1.0, lp=-1, hp=-1, width=6.0, return_filter=False ):
# Band-pass filtering of images with a cosine edge. Good to approximate a top-hat filter while still reducing edge artifacts.

	if width < 0.0:

		width = 0.0

	if width > 0.0 and width <= 1.0:

		width = 0.5 * width * np.min( img.shape )

	if lp <= 0.0:

		lowpass = 1.0

	else:

		lowpass = SoftMask( img.shape, radius=np.min( img.shape ) * apix/lp, width=width )

	if hp <= 0.0:

		highpass = 1.0

	else:

		highpass = 1.0 - SoftMask( img.shape, radius=np.min( img.shape ) * apix/hp, width=width )

	bandpass = lowpass * highpass

	ft = np.fft.fftshift( np.fft.fftn( img ) )

	filtered = np.fft.ifftn( np.fft.ifftshift( ft * bandpass ) )

	if return_filter:

		return filtered.real, bandpass

	else:

		return filtered.real

def Resample( img, newsize=None, apix=1.0, newapix=None ):
# Resizes a real image or volume by cropping/padding its Fourier Transform, i.e. resampling.

	if newsize == None and newapix == None:

		newsize = img.shape

	elif newapix != None:

		newsize = np.round( np.array( img.shape ) * apix / newapix ).astype( 'int' )

	return np.fft.irfftn( np.fft.rfftn( img ), s = newsize ).real

def NormalizeImg( img, mean=0.0, std=1.0 ):
# Normalizes an image to specified mean and standard deviation:

	return (img - img.mean() + mean) * std / img.std()

def FCC( volume1, volume2, phiArray = [0.0] ):
	"""
	Fourier conic correlation

	Created on Fri Dec  4 16:35:42 2015
	@author: Robert A. McLeod

	Modified by: Ricardo Righetto
	Date of modification: 23.02.2017 
	Change: now also supports (conical) FRC

	Returns FCC_normed, which has len(phiArray) Fourier conic correlations
	"""

	import warnings
	with warnings.catch_warnings():
		warnings.filterwarnings( "ignore", category=RuntimeWarning )

		if volume1.ndim == 3:

			[M,N,P] = volume1.shape
			[zmesh, ymesh, xmesh] = np.mgrid[ -M/2:M/2, -N/2:N/2, -P/2:P/2  ]
			rhomax = np.int( np.ceil( np.sqrt( M*M/4.0 + N*N/4.0 + P*P/4.0) ) + 1 )
			rhomesh = np.sqrt( xmesh*xmesh + ymesh*ymesh + zmesh*zmesh )
			phimesh = np.arccos( zmesh / rhomesh )
			phimesh[M/2,N/2,P/2] = 0.0
			phimesh = np.ravel( phimesh )

		elif volume1.ndim == 2:

			[M,N] = volume1.shape
			[ymesh, xmesh] = np.mgrid[ -M/2:M/2, -N/2:N/2  ]
			rhomax = np.int( np.ceil( np.sqrt( M*M/4.0 + N*N/4.0 ) ) + 1 )
			rhomesh = np.sqrt( xmesh*xmesh + ymesh*ymesh )
			phimesh = np.arctan2( ymesh, xmesh )
			phimesh[M/2,N/2] = 0.0
			phimesh = np.ravel( phimesh )

		else:

			raise RuntimeError("Error: FCC only supports 2D and 3D objects.")

	phiArray = np.deg2rad( phiArray )

	rhoround = np.round( rhomesh.ravel() ).astype( 'int' ) # Indices for bincount
	# rhomax = np.int( np.ceil( np.sqrt( M*M/4.0 + N*N/4.0 + P*P/4.0) ) + 1 )

	fft1 = np.ravel( np.fft.fftshift( np.fft.fftn( volume1 ) )  )
	conj_fft2 = np.ravel( np.fft.fftshift( np.fft.fftn( volume2 ) ).conj()  )

	FCC_normed = np.zeros( [rhomax, len(phiArray)] )
	for J, phiAngle in enumerate( phiArray ):

		if phiAngle == 0.0:
			fft1_conic = fft1
			conj_fft2_conic = conj_fft2
			rhoround_conic = rhoround
		else:
			conic = np.ravel( (phimesh <= phiAngle ) + ( (np.abs(phimesh - np.pi)) <= phiAngle )  )
			rhoround_conic = rhoround[conic]
			fft1_conic = fft1[conic]
			conj_fft2_conic = conj_fft2[conic]

		FCC = np.bincount( rhoround_conic, np.real(fft1_conic * conj_fft2_conic) )
		Norm1 = np.bincount( rhoround_conic, np.abs(fft1_conic)*np.abs(fft1_conic) )
		Norm2 = np.bincount( rhoround_conic, np.abs(conj_fft2_conic)*np.abs(conj_fft2_conic) )

		goodIndices = np.argwhere( (Norm1 * Norm2) > 0.0 )
		FCC_normed[goodIndices,J] = FCC[goodIndices] / np.sqrt( Norm1[goodIndices] * Norm2[goodIndices] )

	return FCC_normed

def FSC( volume1, volume2, phiArray = [0.0] ):
# FSC is just a wrapper to FCC

	return FCC( volume1, volume2, phiArray = [0.0] )

def FRC( image1, image2, phiArray = [0.0] ):
# FSC is just a wrapper to FRC

	return FCC( image1, image2, phiArray = [0.0] )

# def Resize( img, newsize=None, padval=None ):
# # Resizes a real image or volume by cropping/padding. I.e. sampling is not changed.

##### UNDER CONSTRUCTION #####

#     if newsize == None:

#         return img

#     else:

#         imgshape = np.array( img.shape )
#         newsize = np.array( newsize )

#         maxdim = np.maximum( imgshape, newsize )

#         imgones = np.ones( imgshape ).astype('int')
#         newones = np.ones( newsize ).astype('int')
#         padwidthimg = maxdim - imgshape
#         padwidthnew = maxdim - newsize

#         imgpad = np.pad( img, padwidthimg, mode='constant', constant_values=0 )
#         imgonespad = np.pad( imgones, padwidthimg, mode='constant', constant_values=2 )
#         imgnewonespad = np.pad( newones, padwidthnew, mode='constant', constant_values=0 )
        
#         imgnewidx = imgonespad * imgnewonespad

#         imgonespad[imgonespad]
#         newimg = np.zeros( maxdim, dtype=img.dtype )

#         if padval == None:

#             padval = np.mean( img )

#         newimg[imgnewidx == 2] = padval
#         newimg[imgnewidx == 1] = imgpad[imgnewidx == 1]