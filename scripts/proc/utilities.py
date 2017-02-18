# Python utilities for Focus
# Author: Ricardo Righetto
# E-mail: ricardo.righetto@unibas.ch

import numpy as np

def RadialIndices( imsize = [100, 100], rounding=True ):
# Returns radius and angles for each pixel (or voxel) in a 2D image or 3D volume of shape = imsize
# For 2D returns the angle with the horizontal x- axis
# For 3D returns the angle with the horizontal x,y plane
# If imsize is a scalar, will default to 2D.
# Rounding is to ensure "perfect" radial symmetry, desirable in most applications.

	if np.isscalar(imsize):

		imsize = [imsize, imsize]

	if len(imsize) > 3:

		raise ValueError ( "Object should not have dimensions larger than 3: len(imsize) = %d " % len(imsize))

	if len(imsize) == 2:

		[xmesh, ymesh] = np.mgrid[-imsize[0]/2:imsize[0]/2, -imsize[1]/2:imsize[1]/2]

		rmesh = np.sqrt( xmesh*xmesh + ymesh*ymesh )
		amesh = np.arctan( ymesh / xmesh )

		return rmesh, amesh

	else:

		[xmesh, ymesh, zmesh] = np.mgrid[-imsize[0]/2:imsize[0]/2, -imsize[1]/2:imsize[1]/2, -imsize[2]/2:imsize[2]/2]

		rmesh = np.sqrt( xmesh*xmesh + ymesh*ymesh + zmesh*zmesh )
		amesh = np.arccos( zmesh / rmesh )
		amesh[imsize[0]/2, imsize[1]/2, imsize[2]/2] = 0.0

	if rounding:

		return np.round(rmesh), amesh

	else:

		return rmesh,amesh

def RotationalAverage( img ):
# Compute the rotational average of a 2D image or 3D volume

	rmesh = RadialIndices( img.shape )[0]

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

	width *= 0.5
	radius *= 0.5

	rii = radius + width/2
	rih = radius - width/2

	rmesh = RadialIndices( imsize )[0]

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

	rmesh = RadialIndices( img.shape )[0] / ( np.min( img.shape ) * apix )

	if lp <= 0.0:

		lowpass = 1.0

	else:

		lowpass = np.exp( - lp ** 2 * rmesh ** 2 / 2 )

	if hp <= 0.0:

		highpass = 1.0

	else:

		highpass = 1.0 - np.exp( - hp ** 2 * rmesh ** 2 / 2 )

	bandpass = lowpass * highpass

	fft = np.fft.fftshift( np.fft.fftn( img ) )

	filtered = np.fft.ifftn( np.fft.ifftshift( fft * bandpass ) )

	if return_filter:

		return filtered.real, bandpass

	else:

		return filtered.real

def FilterBfactor( img, apix=1.0, B=0.0, return_filter=False ):
# Applies a B-factor to images. B can be positive or negative.

	rmesh = RadialIndices( img.shape )[0] / ( np.sqrt( np.prod( img.shape )) * apix )

	bfac = np.exp( - (B * rmesh ** 2  ) /  4  )

	fft = np.fft.fftshift( np.fft.fftn( img ) )

	filtered = np.fft.ifftn( np.fft.ifftshift( fft * bfac ) )

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

	fft = np.fft.fftshift( np.fft.fftn( img ) )

	filtered = np.fft.ifftn( np.fft.ifftshift( fft * bandpass ) )

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

def Resize( img, newsize=None, padval=None ):
# Resizes a real image or volume by cropping/padding. I.e. sampling is not changed.

    if newsize == None:

        return img

    else:

        imgshape = np.array( img.shape )
        newsize = np.array( newsize )

        maxdim = np.maximum( imgshape, newsize )

        imgones = np.ones( imgshape ).astype('int')
        newones = np.ones( newsize ).astype('int')
        padwidthimg = maxdim - imgshape
        padwidthnew = maxdim - newsize

        imgpad = np.pad( img, padwidthimg, mode='constant', constant_values=0 )
        imgonespad = np.pad( imgones, padwidthimg, mode='constant', constant_values=2 )
        imgnewonespad = np.pad( newones, padwidthnew, mode='constant', constant_values=0 )
        
        imgnewidx = imgonespad * imgnewonespad

        imgonespad[imgonespad]
        newimg = np.zeros( maxdim, dtype=img.dtype )

        if padval == None:

            padval = np.mean( img )

        newimg[imgnewidx == 2] = padval
        newimg[imgnewidx == 1] = imgpad[imgnewidx == 1]