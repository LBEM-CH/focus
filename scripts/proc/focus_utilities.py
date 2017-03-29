# Python utilities for Focus
# Author: Ricardo Righetto
# E-mail: ricardo.righetto@unibas.ch

import numpy as np

def RadialIndices( imsize = [100, 100], rounding=True, normalize=False, rfft=False, xyz=[0,0,0] ):
# Returns radius and angles for each pixel (or voxel) in a 2D image or 3D volume of shape = imsize
# For 2D returns the angle with the horizontal x- axis
# For 3D returns the angle with the horizontal x,y plane
# If imsize is a scalar, will default to 2D.
# Rounding is to ensure "perfect" radial symmetry, desirable in some applications.
# Normalize=True will normalize the radius to values between 0.0 and 1.0.
# rfft=True will return only half of the radial indices in a way that is compliant with the FFT of real inputs.
# Note: This function is compliant with NumPy fftfreq() and rfftfreq()

	if np.isscalar(imsize):

		imsize = [imsize, imsize]

	if len( imsize ) > 3:

		raise ValueError( "Object should not have dimensions larger than 3: len(imsize) = %d " % len(imsize))

	xyz = np.flipud( xyz )

	import warnings
	with warnings.catch_warnings():
		warnings.filterwarnings( "ignore", category=RuntimeWarning )

		m = np.mod(imsize, 2) # Check if dimensions are odd or even

		if len(imsize) == 2:

			# [xmesh, ymesh] = np.mgrid[-imsize[0]/2:imsize[0]/2, -imsize[1]/2:imsize[1]/2]
			# The definition below is consistent with numpy np.fft.fftfreq and np.fft.rfftfreq:

			if not rfft:

				[xmesh, ymesh] = np.mgrid[-imsize[0]//2+m[0]-xyz[0]:(imsize[0]-1)//2+1-xyz[0], -imsize[1]//2+m[1]-xyz[1]:(imsize[1]-1)//2+1-xyz[1]]

			else:

				[xmesh, ymesh] = np.mgrid[-imsize[0]//2+m[0]-xyz[0]:(imsize[0]-1)//2+1-xyz[0], 0-xyz[1]:imsize[1]//2+1-xyz[1]]
				xmesh = np.fft.ifftshift( xmesh )

			rmesh = np.sqrt( xmesh*xmesh + ymesh*ymesh )
			
			amesh = np.arctan2( ymesh, xmesh )

			n = 2 # Normalization factor

		else:

			# [xmesh, ymesh, zmesh] = np.mgrid[-imsize[0]/2:imsize[0]/2, -imsize[1]/2:imsize[1]/2, -imsize[2]/2:imsize[2]/2]
			# The definition below is consistent with numpy np.fft.fftfreq and np.fft.rfftfreq:

			if not rfft:

				[xmesh, ymesh, zmesh] = np.mgrid[-imsize[0]//2+m[0]-xyz[0]:(imsize[0]-1)//2+1-xyz[0], -imsize[1]//2+m[1]-xyz[1]:(imsize[1]-1)//2+1-xyz[1], -imsize[2]//2+m[2]-xyz[2]:(imsize[2]-1)//2+1-xyz[2]]

			else:

				[xmesh, ymesh, zmesh] = np.mgrid[-imsize[0]//2+m[0]-xyz[0]:(imsize[0]-1)//2+1-xyz[0], -imsize[1]//2+m[1]-xyz[1]:(imsize[1]-1)//2+1-xyz[1], 0-xyz[2]:imsize[2]//2+1-xyz[2]]
				xmesh = np.fft.ifftshift( xmesh )
				ymesh = np.fft.ifftshift( ymesh )

			rmesh = np.sqrt( xmesh*xmesh + ymesh*ymesh + zmesh*zmesh )

			amesh = np.arccos( zmesh / rmesh )

			n = 3 # Normalization factor

	if rounding:

		rmesh = np.round( rmesh ).astype('int')

	if normalize:

		rmesh = rmesh / ( np.sqrt( np.sum( np.power( imsize, 2 ) ) ) / np.sqrt(n) )

	return rmesh, np.nan_to_num( amesh )

def RotationalAverage( img ):
# Compute the rotational average of a 2D image or 3D volume

	rmesh = RadialIndices( img.shape, rounding=True )[0]

	rotavg = np.zeros( img.shape )

	for r in np.unique( rmesh ):

		idx = rmesh == r
		rotavg[idx] = img[idx].mean()

	return rotavg

def RadialFilter( img, filt, return_filter = False ):
# Given a list of factors 'filt', radially multiplies the Fourier Transform of 'img' by the corresponding term in 'filt'

	rmesh = RadialIndices( img.shape, rounding=True, rfft=True )[0]

	ft = np.fft.rfftn( img )
	# print len(np.unique( rmesh )),len(filt)
	j = 0
	for r in np.unique( rmesh ):

		idx = rmesh == r
		ft[idx] *= filt[j]
		j += 1

	if return_filter:

		filter2d = np.zeros(rmesh.shape)
		j = 0
		for r in np.unique( rmesh ):

			idx = rmesh == r
			filter2d[idx] = filt[j]
			j += 1

	if not return_filter:

		return np.fft.irfftn( ft )

	else:

		return np.fft.irfftn( ft ), filter2d

def SoftMask( imsize = [100, 100], radius = 0.5, width = 6.0, rounding=False, xyz=[0,0,0] ):
# Generates a circular or spherical mask with a soft cosine edge

	if np.isscalar(imsize):

		imsize = [imsize, imsize]

	if len(imsize) > 3:

		raise ValueError ( "Object should not have dimensions larger than 3: len(imsize) = %d " % len(imsize))

	if width < 0.0:

		width = 0.0

	if width > 0.0 and width < 1.0:

		width = 0.5 * width * np.min( imsize )

	if radius > 0.0 and radius <= 1.0:

		radius = radius * np.min( imsize )

		radius *= 0.5

	if ( radius < 0.0 ) or ( np.min( imsize ) < radius*2 ):

		radius = 0.5 * ( np.min( imsize ) - float(width)/2 )

	rii = radius + width/2
	rih = radius - width/2

	rmesh = RadialIndices( imsize, rounding=rounding, xyz=xyz )[0]

	mask = np.zeros( imsize )

	fill_idx = rmesh <= rih
	mask[fill_idx] = 1.0

	rih_idx = rmesh > rih
	rii_idx = rmesh <= rii
	edge_idx = rih_idx * rii_idx

	mask[edge_idx] = ( 1.0 + np.cos( np.pi * ( rmesh[edge_idx] - rih ) / width ) ) / 2.0

	return mask

def FilterGauss( img, apix=1.0, lp=-1, hp=-1, return_filter=False ):
# Gaussian band-pass filtering of images.

	rmesh = RadialIndices( img.shape, rounding=False, normalize=True, rfft=True )[0] / apix
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

	# ft = np.fft.fftshift( np.fft.fftn( img ) )

	# filtered = np.fft.ifftn( np.fft.ifftshift( ft * bandpass ) )

	ft = np.fft.rfftn( img )

	filtered = np.fft.irfftn( ft * bandpass )

	if return_filter:

		return filtered, bandpass

	else:

		return filtered

def FilterBfactor( img, apix=1.0, B=0.0, return_filter=False ):
# Applies a B-factor to images. B can be positive or negative.

	rmesh = RadialIndices( img.shape, rounding=False, normalize=True, rfft=True )[0] / apix
	rmesh2 = rmesh*rmesh

	bfac = np.exp( - (B * rmesh2  ) /  4  )

	# ft = np.fft.fftshift( np.fft.fftn( img ) )

	# filtered = np.fft.ifftn( np.fft.ifftshift( ft * bfac ) )

	ft = np.fft.rfftn( img )

	filtered = np.fft.irfftn( ft * bfac )

	if return_filter:

		return filtered, bfac

	else:

		return filtered

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

def FilterTophat( img, apix=1.0, lp=-1, hp=-1, return_filter=False ):
# Just a wrapper to the cosine filter with a hard edge:

	return FilterCosine( img, apix=apix, lp=lp, hp=hp, width=0.0, return_filter=False )

def HighResolutionNoiseSubstitution( img, lp = -1, apix = 1.0 ):
# Randomizes the phases of a map beyond resolution 'lp'

	# Get resolution shells:
	rmesh = RadialIndices( img.shape, rounding=False, normalize=True, rfft=True )[0] / apix

	lp = 1.0/lp

	ft = np.fft.rfftn( img )

	# Decompose Fourier transform into amplitudes and phases:
	amps = np.absolute( ft )
	phases = np.angle( ft )

	idx = rmesh >= lp # Select only terms beyond desired resolution (inclusive)

	if lp > 0.0:

		# numpy.random.seed( seed=123 ) # We have to enforce the random seed otherwise different runs would not be comparable
		phasesrnd = np.random.random( phases.shape ) * 2.0 * np.pi # Generate random phases in radians

		phases[idx] = phasesrnd[idx]

	ftnew = amps * ( np.cos( phases ) + 1j*np.sin( phases ) )

	return np.fft.irfftn( ftnew )

def Resample( img, newsize=None, apix=1.0, newapix=None ):
# Resizes a real image or volume by cropping/padding its Fourier Transform, i.e. resampling.

	if newsize == None and newapix == None:

		newsize = img.shape

	elif newapix != None:

		newsize = np.round( np.array( img.shape ) * apix / newapix ).astype( 'int' )

	return np.fft.irfftn( np.fft.rfftn( img ), s = newsize )

def NormalizeImg( img, mean=0.0, std=1.0, radius = -1 ):
# Normalizes an image to specified mean and standard deviation:
# If 'radius' is specified, will use only the area outside the circle with this radius to calculate 'mean' and 'std'
# which is the way RELION expects images to be normalized

	if radius > 0.0:

		mask = SoftMask( img.shape, radius = radius, width = 0, rounding=False ).astype('int')
		mask = 1 - mask # Get only the area outside the disk
		m = img[mask].mean()
		s = img[mask].std()

	else:

		m = img.mean()
		s = img.std()

	return (img - m + mean) * std / s

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

	return FCC( volume1, volume2, phiArray = phiArray )

def FRC( image1, image2, phiArray = [0.0] ):
# FSC is just a wrapper to FRC

	return FCC( image1, image2, phiArray = phiArray )

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